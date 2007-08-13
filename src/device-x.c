/* Device functions for X windows.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Original authors: Jamie Zawinski and the FSF */
/* Rewritten by Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* Numerous places access the fields of
				   a core widget directly.  We could
				   use XtVaGetValues(), but ... */
#include "xgccache.h"
#include <X11/Shell.h>
#include "xmu.h"
#include "glyphs-x.h"
#include "objects-x.h"

#include "buffer.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "sysfile.h"
#include "systime.h"

Lisp_Object Vdefault_x_device;

/* Qdisplay in general.c */
Lisp_Object Qx_error; 
Lisp_Object Qinit_pre_x_win, Qinit_post_x_win;

/* The application class of Emacs. */
Lisp_Object Vx_emacs_application_class;

Lisp_Object Vx_initial_argv_list; /* #### ugh! */

static XrmOptionDescRec emacs_options[] =
{
  {(String)"-geometry", (String)".geometry", XrmoptionSepArg, NULL},
  {(String)"-iconic", (String)".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {(String)"-internal-border-width", (String)"*EmacsFrame.internalBorderWidth",
   XrmoptionSepArg, NULL},
  {(String)"-ib", (String)"*EmacsFrame.internalBorderWidth", XrmoptionSepArg,
   NULL},
  {(String)"-scrollbar-width", (String)"*EmacsFrame.scrollBarWidth",
   XrmoptionSepArg, NULL},
  {(String)"-scrollbar-height", (String)"*EmacsFrame.scrollBarHeight",
   XrmoptionSepArg, NULL},

  /* #### Beware!  If the type of the shell changes, update this. */
  {(String)"-T", (String)"*TopLevelEmacsShell.title", XrmoptionSepArg, NULL},
  {(String)"-wn", (String)"*TopLevelEmacsShell.title", XrmoptionSepArg, NULL},
  {(String)"-title", (String)"*TopLevelEmacsShell.title", XrmoptionSepArg,
   NULL},
  {(String)"-iconname", (String)"*TopLevelEmacsShell.iconName",
   XrmoptionSepArg, NULL},
  {(String)"-in", (String)"*TopLevelEmacsShell.iconName", XrmoptionSepArg,
   NULL},
  {(String)"-mc", (String)"*pointerColor", XrmoptionSepArg, NULL},
  {(String)"-cr", (String)"*cursorColor", XrmoptionSepArg, NULL},
  {(String)"-fontset", (String)"*FontSet", XrmoptionSepArg, NULL},
};

static void validify_resource_string (char *str);

/* Functions to synchronize mirroring resources and specifiers */
int in_resource_setting;
int in_specifier_change_function;


/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

struct device *
get_device_from_display (Display *dpy)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      if (DEVICE_X_P (d) && DEVICE_X_DISPLAY (d) == dpy)
	return d;
    }

  /* Only devices we are actually managing should ever be used as an
     argument to this function. */
  abort ();

  return 0; /* suppress compiler warning */
}

struct device *
decode_x_device (Lisp_Object device)
{
  XSETDEVICE (device, decode_device (device));
  CHECK_X_DEVICE (device);
  return XDEVICE (device);
}

Display *
get_x_display (Lisp_Object device)
{
  return DEVICE_X_DISPLAY (decode_x_device (device));
}


/************************************************************************/
/*		      initializing an X connection			*/
/************************************************************************/

static void
allocate_x_device_struct (struct device *d)
{
  d->device_data = (struct x_device *) xmalloc (sizeof (struct x_device));

  /* zero out all slots. */
  memset (d->device_data, 0, sizeof (struct x_device));
}

static void
Xatoms_of_device_x (struct device *d)
{
  Display *display = DEVICE_X_DISPLAY (d);
#define ATOM(x) XInternAtom (display, (x), False)

  DEVICE_XATOM_WM_PROTOCOLS (d) = ATOM ("WM_PROTOCOLS");
  DEVICE_XATOM_WM_DELETE_WINDOW (d) = ATOM ("WM_DELETE_WINDOW");
  DEVICE_XATOM_WM_SAVE_YOURSELF (d) = ATOM ("WM_SAVE_YOURSELF");
  DEVICE_XATOM_WM_TAKE_FOCUS (d) = ATOM ("WM_TAKE_FOCUS");
  DEVICE_XATOM_WM_STATE (d) = ATOM ("WM_STATE");
}

static void
sanity_check_geometry_resource (Display *dpy)
{
  char *app_name, *app_class, *s;
  char buf1 [255], buf2 [255];
  char *type;
  XrmValue value;
  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  strcpy (buf1, app_name);
  strcpy (buf2, app_class);
  for (s = buf1; *s; s++) if (*s == '.') *s = '_';
  strcat (buf1, "._no_._such_._resource_.geometry");
  strcat (buf2, "._no_._such_._resource_.Geometry");
  if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
    {
      warn_when_safe (Qgeometry, Qerror,
		      "\n"
"Apparently \"%s*geometry: %s\" or \"%s*geometry: %s\" was\n"
"specified in the resource database.  Specifying \"*geometry\" will make\n"
"XEmacs (and most other X programs) malfunction in obscure ways. (i.e.\n"
"the Xt or Xm libraries will probably crash, which is a very bad thing.)\n"
"You should always use \".geometry\" or \"*EmacsFrame.geometry\" instead.\n",
		  app_name, (char *) value.addr,
		  app_class, (char *) value.addr);
      suppress_early_backtrace = 1;
      error ("Invalid geometry resource");
    }
}

static void
x_init_device_class (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);
  if (DisplayCells (dpy, DefaultScreen (dpy)) > 2)
    {
      switch (DefaultVisualOfScreen (DefaultScreenOfDisplay (dpy))->class)
	{
	case StaticGray:
	case GrayScale:
	  DEVICE_CLASS (d) = Qgrayscale;
	  break;
	default:
	  DEVICE_CLASS (d) = Qcolor;
	}
    }
  else
    DEVICE_CLASS (d) = Qmono;
}

static void
x_init_device (struct device *d, Lisp_Object props)
{
  Lisp_Object display;
  Lisp_Object device;
  Display *dpy;
  int argc;
  char **argv;
  CONST char *app_class;
  CONST char *disp_name;

  XSETDEVICE (device, d);
  display = DEVICE_CONNECTION (d);

  allocate_x_device_struct (d);

  make_argc_argv (Vx_initial_argv_list, &argc, &argv);

  if (STRINGP (Vx_emacs_application_class) &&
      string_length (XSTRING (Vx_emacs_application_class)) > 0)
    GET_C_STRING_CTEXT_DATA_ALLOCA (Vx_emacs_application_class, app_class);
  else
    app_class = "Emacs";

  GET_C_STRING_CTEXT_DATA_ALLOCA (display, disp_name);

  slow_down_interrupts ();
  /* The Xt code can't deal with signals here.  Yuck. */
  dpy = DEVICE_X_DISPLAY (d) =
    XtOpenDisplay (Xt_app_con, disp_name, NULL, app_class, emacs_options,
                   XtNumber (emacs_options), &argc, argv);
  speed_up_interrupts ();

  if (dpy == 0)
    {
      suppress_early_backtrace = 1;
      signal_simple_error ("X server not responding\n", display);
    }

  if (NILP (Vdefault_x_device))
    Vdefault_x_device = device;

  if (NILP (DEVICE_NAME (d)))
    DEVICE_NAME (d) = display;

  /* We're going to modify the string in-place, so be a nice XEmacs */
  DEVICE_NAME (d) = Fcopy_sequence (DEVICE_NAME (d));
  /* colons and periods can't appear in individual elements of resource
     strings */
  validify_resource_string ((char *) string_data (XSTRING (DEVICE_NAME (d))));
  DEVICE_XT_APP_SHELL (d) = XtAppCreateShell (NULL, app_class,
					      applicationShellWidgetClass,
					      dpy, NULL, 0);


  Vx_initial_argv_list = make_arg_list (argc, argv);
  free_argc_argv (argv);

  DEVICE_X_WM_COMMAND_FRAME (d) = Qnil;

  sanity_check_geometry_resource (dpy);

  /* In event-Xt.c */
  x_init_modifier_mapping (d);

  DEVICE_INFD (d) = DEVICE_OUTFD (d) = ConnectionNumber (dpy);
  init_baud_rate (d);
  init_one_device (d);

  DEVICE_X_GC_CACHE (d) =
    make_gc_cache (dpy, RootWindow (dpy, DefaultScreen (dpy)));
  DEVICE_X_GRAY_PIXMAP (d) = None;
  Xatoms_of_device_x (d);
  Xatoms_of_xselect (d);
  Xatoms_of_objects_x (d);
  x_init_device_class (d);

  /* Run the elisp side of the X device initialization. */
  call0 (Qinit_pre_x_win);
}

static void
x_finish_init_device (struct device *d, Lisp_Object props)
{
  call0 (Qinit_post_x_win);
}

static void
x_mark_device (struct device *d, void (*markobj) (Lisp_Object))
{
  ((markobj) (DEVICE_X_DATA (d)->WM_COMMAND_frame));
}


/************************************************************************/
/*                       closing an X connection	                */
/************************************************************************/

static void
free_x_device_struct (struct device *d)
{
  xfree (d->device_data);
}

static void
x_delete_device (struct device *d)
{
  Lisp_Object device;
  Display *display;
#ifdef FREE_CHECKING
  extern void (*__free_hook)();
  int checking_free;
#endif

  XSETDEVICE (device, d);
  display = DEVICE_X_DISPLAY (d);

  if (display)
    {
#ifdef FREE_CHECKING
      checking_free = (__free_hook != 0);
      
      /* Disable strict free checking, to avoid bug in X library */
      if (checking_free)
	disable_strict_free_check ();
#endif

      free_gc_cache (DEVICE_X_GC_CACHE (d));
      if (DEVICE_X_DATA (d)->x_modifier_keymap)
	XFreeModifiermap (DEVICE_X_DATA (d)->x_modifier_keymap);
      if (DEVICE_X_DATA (d)->x_keysym_map)
	XFree ((char *) DEVICE_X_DATA (d)->x_keysym_map);

      XtCloseDisplay (display);
      DEVICE_X_DISPLAY (d) = 0;
#ifdef FREE_CHECKING
      if (checking_free)
	enable_strict_free_check ();
#endif
    }
  
  if (EQ (device, Vdefault_x_device))
    {
      Lisp_Object devcons, concons;
      /* #### handle deleting last X device */
      Vdefault_x_device = Qnil;
      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  if (DEVICE_X_P (XDEVICE (XCAR (devcons))))
	    {
	      Vdefault_x_device = XCAR (devcons);
	      goto double_break;
	    }
	}
    }
 double_break:
  free_x_device_struct (d);
}


/************************************************************************/
/*				handle X errors				*/
/************************************************************************/

static CONST char *events[] =
{
   "0: ERROR!",
   "1: REPLY",
   "KeyPress",
   "KeyRelease",
   "ButtonPress",
   "ButtonRelease",
   "MotionNotify",
   "EnterNotify",
   "LeaveNotify",
   "FocusIn",
   "FocusOut",
   "KeymapNotify",
   "Expose",
   "GraphicsExpose",
   "NoExpose",
   "VisibilityNotify",
   "CreateNotify",
   "DestroyNotify",
   "UnmapNotify",
   "MapNotify",
   "MapRequest",
   "ReparentNotify",
   "ConfigureNotify",
   "ConfigureRequest",
   "GravityNotify",
   "ResizeRequest",
   "CirculateNotify",
   "CirculateRequest",
   "PropertyNotify",
   "SelectionClear",
   "SelectionRequest",
   "SelectionNotify",
   "ColormapNotify",
   "ClientMessage",
   "MappingNotify",
   "LASTEvent"
};

CONST char *
x_event_name (int event_type)
{
  if (event_type < 0) return 0;
  if (event_type >= (sizeof (events) / sizeof (char *))) return 0;
  return events [event_type];
}

/* Handling errors.

   If an X error occurs which we are not expecting, we have no alternative
   but to print it to stderr.  It would be nice to stuff it into a pop-up
   buffer, or to print it in the minibuffer, but that's not possible, because
   one is not allowed to do any I/O on the display connection from an error
   handler. The guts of Xlib expect these functions to either return or exit.

   However, there are occasions when we might expect an error to reasonably
   occur.  The interface to this is as follows:

   Before calling some X routine which may error, call
	expect_x_error (dpy);

   Just after calling the X routine, call either:

	x_error_occurred_p (dpy);

   to ask whether an error happened (and was ignored), or:

	signal_if_x_error (dpy, resumable_p);

   which will call Fsignal() with args appropriate to the X error, if there
   was one.  (Resumable_p is whether the debugger should be allowed to
   continue from the call to signal.)

   You must call one of these two routines immediately after calling the X
   routine; think of them as bookends like BLOCK_INPUT and UNBLOCK_INPUT.
 */

static int error_expected;
static int error_occurred;
static XErrorEvent last_error;

/* OVERKILL! */

#ifdef EXTERNAL_WIDGET
static Lisp_Object
x_error_handler_do_enqueue (Lisp_Object frame)
{
  enqueue_magic_eval_event (io_error_delete_frame, frame);
  return Qt;
}

static Lisp_Object
x_error_handler_error (Lisp_Object data, Lisp_Object dummy)
{
  return Qnil;
}
#endif /* EXTERNAL_WIDGET */

int
x_error_handler (Display *disp, XErrorEvent *event)
{
  if (error_expected)
    {
      error_expected = 0;
      error_occurred = 1;
      last_error = *event;
    }
  else
    {
#ifdef EXTERNAL_WIDGET
      struct frame *f;
      struct device *d = get_device_from_display (disp);

      if ((event->error_code == BadWindow ||
	   event->error_code == BadDrawable)
	  && ((f = x_any_window_to_frame (d, event->resourceid)) != 0))
	{
	  Lisp_Object frame;

	/* one of the windows comprising one of our frames has died.
	   This occurs particularly with ExternalShell frames when the
	   client that owns the ExternalShell's window dies.

	   We cannot do any I/O on the display connection so we need
	   to enqueue an eval event so that the deletion happens
	   later.

	   Furthermore, we need to trap any errors (out-of-memory) that
	   may occur when Fenqueue_eval_event is called.
	 */

	if (f->being_deleted)
	  return 0;
	XSETFRAME (frame, f);
	if (!NILP (condition_case_1 (Qerror, x_error_handler_do_enqueue,
				     frame, x_error_handler_error, Qnil)))
	  {
	    f->being_deleted = 1;
	    f->visible = 0;
	  }
	return 0;
      }
#endif /* EXTERNAL_WIDGET */

      stderr_out ("\n%s: ",
		  (STRINGP (Vinvocation_name)
		   ? (char *) string_data (XSTRING (Vinvocation_name))
		   : "xemacs"));
      XmuPrintDefaultErrorMessage (disp, event, stderr);
    }
  return 0;
}

void
expect_x_error (Display *dpy)
{
  assert (!error_expected);
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  error_expected = 1;
  error_occurred = 0;
}

int
x_error_occurred_p (Display *dpy)
{
  int val;
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  val = error_occurred;
  error_expected = 0;
  error_occurred = 0;
  return val;
}

int
signal_if_x_error (Display *dpy, int resumable_p)
{
  char buf[1024];
  Lisp_Object data;
  if (! x_error_occurred_p (dpy))
    return 0;
  data = Qnil;
  sprintf (buf, "0x%X", (unsigned int) last_error.resourceid);
  data = Fcons (build_string (buf), data);
  {
    char num [32];
    sprintf (num, "%d", last_error.request_code);
    XGetErrorDatabaseText (last_error.display, "XRequest", num, "",
			   buf, sizeof (buf));
    if (! *buf)
      sprintf (buf, "Request-%d", last_error.request_code);
    data = Fcons (build_string (buf), data);
  }
  XGetErrorText (last_error.display, last_error.error_code, buf, sizeof (buf));
  data = Fcons (build_string (buf), data);
 again:
  Fsignal (Qx_error, data);
  if (! resumable_p) goto again;
  return 1;
}

int
x_IO_error_handler (Display *disp)
{
  /* This function can GC */
  Lisp_Object dev;
  struct device *d = get_device_from_display (disp);
  XSETDEVICE (dev, d);

  if (NILP (find_nonminibuffer_frame_not_on_device (dev)))
    {
      /* We're going down. */
      stderr_out
	("\n%s: Fatal I/O Error %d (%s) on display connection \"%s\"\n",
         (STRINGP (Vinvocation_name) ?
	  (char *) string_data (XSTRING (Vinvocation_name)) : "xemacs"),
	 errno, strerror (errno), DisplayString (disp));
      stderr_out
        ("  after %lu requests (%lu known processed) with %d events remaining.\n",
         NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
         QLength (disp));
      /* assert (!_Xdebug); */
    }
  else
    {
      warn_when_safe
	(Qx, Qcritical,
	 "I/O Error %d (%s) on display connection \"%s\"\n"
	 "  after %lu requests (%lu known processed) with "
	 "%d events remaining.\n",
	 errno, strerror (errno), DisplayString (disp),
         NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
         QLength (disp));
    }

  enqueue_magic_eval_event (io_error_delete_device, dev);

  return 0;
}

DEFUN ("x-debug-mode", Fx_debug_mode, Sx_debug_mode, 1, 2, 0 /*
With a true arg, make the connection to the X server synchronous.
With false, make it asynchronous.  Synchronous connections are much slower,
but are useful for debugging. (If you get X errors, make the connection
synchronous, and use a debugger to set a breakpoint on `x_error_handler'.
Your backtrace of the C stack will now be useful.  In asynchronous mode,
the stack above `x_error_handler' isn't helpful because of buffering.)
If DEVICE is not specified, the selected device is assumed.

Calling this function is the same as calling the C function `XSynchronize',
or starting the program with the `-sync' command line argument.
*/ )
    (arg, device)
    Lisp_Object arg, device;
{
  struct device *d = decode_x_device (device);

  XSynchronize (DEVICE_X_DISPLAY (d), !NILP (arg));

  if (!NILP (arg))
    message ("X connection is synchronous");
  else
    message ("X connection is asynchronous");

  return arg;
}


/************************************************************************/
/*                             X resources                              */
/************************************************************************/

#if 0 /* bah humbug.  The whole "widget == resource" stuff is such
	 a crock of shit that I'm just going to ignore it all. */

/* If widget is NULL, we are retrieving device or global face data. */

static void
construct_name_list (Display *display, Widget widget, char *fake_name,
		     char *fake_class, char *name, char *class)
{
  char *stack [100][2];
  Widget this;
  int count = 0;
  char *name_tail, *class_tail;

  if (widget)
    {
      for (this = widget; this; this = XtParent (this))
	{
	  stack [count][0] = this->core.name;
	  stack [count][1] = XtClass (this)->core_class.class_name;
	  count++;
	}
      count--;
    }
  else if (fake_name && fake_class)
    {
      stack [count][0] = fake_name;
      stack [count][1] = fake_class;
      count++;
    }

  /* The root widget is an application shell; resource lookups use the
     specified application name and application class in preference to
     the name/class of that widget (which is argv[0] / "ApplicationShell").
     Generally the app name and class will be argv[0] / "Emacs" but
     the former can be set via the -name command-line option, and the
     latter can be set by changing `x-emacs-application-class' in
     lisp/term/x-win.el.
   */
  XtGetApplicationNameAndClass (display,
				&stack [count][0],
				&stack [count][1]);

  name [0] = 0;
  class [0] = 0;

  name_tail  = name;
  class_tail = class;
  for (; count >= 0; count--)
    {
      strcat (name_tail,  stack [count][0]);
      for (; *name_tail; name_tail++)
	if (*name_tail == '.') *name_tail = '_';
      strcat (name_tail, ".");
      name_tail++;

      strcat (class_tail, stack [count][1]);
      for (; *class_tail; class_tail++)
	if (*class_tail == '.') *class_tail = '_';
      strcat (class_tail, ".");
      class_tail++;
    }
}

#endif

/* Only the characters [-_A-Za-z0-9] are allowed in the individual
   sections of a resource.  Convert invalid characters to -. */

static void
validify_resource_string (char *str)
{
  while (*str)
    {
      if (!strchr ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		   "abcdefghijklmnopqrstuvwxyz"
		   "0123456789-_", *str))
	*str = '-';
      str++;
    }
}

/* Given a locale and device specification from x-get-resource or
x-get-resource-prefix, return the resource prefix and display to
fetch the resource on. */

static void
x_get_resource_prefix (Lisp_Object locale, Lisp_Object device,
		       Display **display_out, char *name_out,
		       char *class_out)
{
  char *appname, *appclass;

  if (NILP (locale))
    locale = Qglobal;
  if (NILP (Fvalid_specifier_locale_p (locale)))
    signal_simple_error ("Invalid locale", locale);
  if (WINDOWP (locale))
    /* #### I can't come up with any coherent way of naming windows.
       By relative position?  That seems tricky because windows
       can change position, be split, etc.  By order of creation?
       That seems less than useful. */
    signal_simple_error ("Windows currently can't be resourced", locale);

  if (!NILP (device) && !DEVICEP (device))
    CHECK_DEVICE (device);
  if (DEVICEP (device) && !DEVICE_X_P (XDEVICE (device)))
    device = Qnil;
  if (NILP (device))
    {
      device = DFW_DEVICE (locale);
      if (DEVICEP (device) && !DEVICE_X_P (XDEVICE (device)))
	device = Qnil;
      if (NILP (device))
	device = Vdefault_x_device;
      if (NILP (device))
	{
	  *display_out = 0;
	  return;
	}
    }

  *display_out = DEVICE_X_DISPLAY (XDEVICE (device));

  XtGetApplicationNameAndClass (*display_out, &appname, &appclass);
  strcpy (name_out, appname);
  strcpy (class_out, appclass);
  validify_resource_string (name_out);
  validify_resource_string (class_out);

  if (EQ (locale, Qglobal))
    return;
  if (BUFFERP (locale))
    {
      strcat (name_out, ".buffer.");
      /* we know buffer is live; otherwise we got an error above. */
      strcat (name_out,
	      (CONST char *) string_data (XSTRING (Fbuffer_name (locale))));
      strcat (class_out, ".EmacsLocaleType.EmacsBuffer");
    }
  else if (FRAMEP (locale))
    {
      strcat (name_out, ".frame.");
      /* we know frame is live; otherwise we got an error above. */
      strcat (name_out,
	      (CONST char *) string_data (XSTRING (Fframe_name (locale))));
      strcat (class_out, ".EmacsLocaleType.EmacsFrame");
    }
  else
    {
      assert (DEVICEP (locale));
      strcat (name_out, ".device.");
      /* we know device is live; otherwise we got an error above. */
      strcat (name_out,
	      (CONST char *) string_data (XSTRING (Fdevice_name (locale))));
      strcat (class_out, ".EmacsLocaleType.EmacsDevice");
    }
  return;
}

DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 3, 6, 0 /*
Retrieve an X resource from the resource manager.

The first arg is the name of the resource to retrieve, such as \"font\".
The second arg is the class of the resource to retrieve, like \"Font\".
The third arg should be one of the symbols 'string, 'integer, 'natnum, or
  'boolean, specifying the type of object that the database is searched for.
The fourth arg is the locale to search for the resources on, and can
  currently be a a buffer, a frame, a device, or 'global.  If omitted, it
  defaults to 'global.
The fifth arg is the device to search for the resources on. (The resource
  database for a particular device is constructed by combining non-device-
  specific resources such any command-line resources specified and any
  app-defaults files found [or the fallback resources supplied by XEmacs,
  if no app-defaults file is found] with device-specific resources such as
  those supplied using xrdb.) If omitted, it defaults to the device of
  LOCALE, if a device can be derived (i.e. if LOCALE is a frame or device),
  and otherwise defaults to the value of `default-x-device'.
The sixth arg NOERROR, if non-nil, means do not signal an error if a
  bogus resource specification was retrieved (e.g. if a non-integer was
  given when an integer was requested).  In this case, a warning is issued
  instead.

The resource names passed to this function are looked up relative to the
locale.

If you want to search for a subresource, you just need to specify the
resource levels in NAME and CLASS.  For example, NAME could be
\"modeline.attributeFont\", and CLASS \"Face.AttributeFont\".

Specifically,

1) If LOCALE is a buffer, a call

    (x-get-resource \"foreground\" \"Foreground\" 'string SOME-BUFFER)

is an interface to a C call something like

    XrmGetResource (db, \"xemacs.buffer.BUFFER-NAME.foreground\",
			\"Emacs.EmacsLocaleType.EmacsBuffer.Foreground\",
			\"String\");

2) If LOCALE is a frame, a call

    (x-get-resource \"foreground\" \"Foreground\" 'string SOME-FRAME)

is an interface to a C call something like

    XrmGetResource (db, \"xemacs.frame.FRAME-NAME.foreground\",
			\"Emacs.EmacsLocaleType.EmacsFrame.Foreground\",
			\"String\");

3) If LOCALE is a device, a call

    (x-get-resource \"foreground\" \"Foreground\" 'string SOME-DEVICE)

is an interface to a C call something like

    XrmGetResource (db, \"xemacs.device.DEVICE-NAME.foreground\",
			\"Emacs.EmacsLocaleType.EmacsDevice.Foreground\",
			\"String\");

4) If LOCALE is 'global, a call

    (x-get-resource \"foreground\" \"Foreground\" 'string 'global)

is an interface to a C call something like

    XrmGetResource (db, \"xemacs.foreground\",
			\"Emacs.Foreground\",
			\"String\");

Note that for 'global, no prefix is added other than that of the
application itself; thus, you can use this locale to retrieve
arbitrary application resources, if you really want to.

The returned value of this function is nil if the queried resource is not
found.  If the third arg is `string', a string is returned, and if it is
`integer', an integer is returned.  If the third arg is `boolean', then the
returned value is the list (t) for true, (nil) for false, and is nil to
mean ``unspecified.''
*/ )
     (name, class, type, locale, device, no_error)
     Lisp_Object name, class, type, locale, device, no_error;
{
  /* #### fixed limit, could be overflowed */
  char name_string[2048], class_string[2048];
  char *raw_result;
  XrmDatabase db;
  Display *display;
  Error_behavior errb = decode_error_behavior_flag (no_error);

  CHECK_STRING (name);
  CHECK_STRING (class);
  CHECK_SYMBOL (type);

  if (!EQ (type, Qstring) && !EQ (type, Qboolean) &&
      !EQ (type, Qinteger) && !EQ (type, Qnatnum))
    return maybe_signal_continuable_error
      (Qwrong_type_argument,
       list2 (build_translated_string
	      ("should be string, integer, natnum or boolean"),
	      type),
       Qresource, errb);

  x_get_resource_prefix (locale, device, &display, name_string,
			 class_string);
  if (!display)
    return Qnil;

  db = XtDatabase (display);

  strcat (name_string, ".");
  strcat (name_string, (CONST char *) string_data (XSTRING (name)));
  strcat (class_string, ".");
  strcat (class_string, (CONST char *) string_data (XSTRING (class)));

  {
    XrmValue xrm_value;
    XrmName namelist[100];
    XrmClass classlist[100];
    XrmName *namerest = namelist;
    XrmClass *classrest = classlist;
    XrmRepresentation xrm_type;
    XrmRepresentation string_quark;
    int result;
    XrmStringToNameList (name_string, namelist);
    XrmStringToClassList (class_string, classlist);
    string_quark = XrmStringToQuark ("String");

    /* ensure that they have the same length */
    while (namerest[0] && classrest[0])
      namerest++, classrest++;
    if (namerest[0] || classrest[0])
      signal_simple_error_2
	("class list and name list must be the same length", name, class);
    result = XrmQGetResource (db, namelist, classlist, &xrm_type, &xrm_value);

    if (result != True || xrm_type != string_quark)
      return Qnil;
    raw_result = (char *) xrm_value.addr;
  }

  if (EQ (type, Qstring))
    return build_string (raw_result);
  else if (EQ (type, Qboolean))
    {
      if (!strcasecmp (raw_result, "off") ||
	  !strcasecmp (raw_result, "false") ||
	  !strcasecmp (raw_result,"no"))
	return Fcons (Qnil, Qnil);
      else if (!strcasecmp (raw_result, "on") ||
	       !strcasecmp (raw_result, "true") ||
	       !strcasecmp (raw_result, "yes"))
	return Fcons (Qt, Qnil);
      else
	return maybe_continuable_error (Qresource, errb,
					"can't convert %s: %s to a Boolean",
					name_string, raw_result);
    }
  else if (EQ (type, Qinteger) || EQ (type, Qnatnum))
    {
      int i;
      char c;
      if (1 != sscanf (raw_result, "%d%c", &i, &c))
	return maybe_continuable_error
	  (Qresource, errb,
	   "can't convert %s: %s to an integer",
	   name_string, raw_result);
      else if (EQ (type, Qnatnum) && i < 0)
	return maybe_continuable_error
	  (Qresource, errb,
	   "invalid numerical value %d for resource %s",
	   i, name_string);
      else
	return make_int (i);
    }
  else
    abort ();

  /* Can't get here. */
  return Qnil;	/* shut up compiler */
}

DEFUN ("x-get-resource-prefix", Fx_get_resource_prefix,
       Sx_get_resource_prefix, 1, 2, 0 /*
Return the resource prefix for LOCALE on DEVICE.
The resource prefix is the strings used to prefix resources if
the LOCALE and DEVICE arguments were passed to `x-get-resource'.
The returned value is a cons of a name prefix and a class prefix.
For example, if LOCALE is a frame, the returned value might be
\(\"xemacs.frame.FRAME-NAME\" . \"Emacs.EmacsLocaleType.EmacsFrame\").
If no valid X device for resourcing can be obtained, this function
returns nil. (In such a case, `x-get-resource' would always return nil.)
*/ )
  (locale, device)
  Lisp_Object locale, device;
{
  /* #### fixed limit, could be overflowed */
  char name[1024], class[1024];
  Display *display;

  x_get_resource_prefix (locale, device, &display, name, class);
  if (!display)
    return Qnil;
  return Fcons (build_string (name), build_string (class));
}

DEFUN ("x-put-resource", Fx_put_resource, Sx_put_resource, 1, 2, 0 /*
Add a resource to the resource database for DEVICE.
RESOURCE-LINE specifies the resource to add and should be a
standard resource specification.
*/ )
     (resource_line, device)
     Lisp_Object resource_line, device;
{
  struct device *d = decode_device (device);
  char *str, *colon_pos;

  CHECK_STRING (resource_line);
  str = (char *) string_data (XSTRING (resource_line));
  if (!(colon_pos = strchr (str, ':')) || strchr (str, '\n'))
  invalid:
    signal_simple_error ("Invalid resource line", resource_line);
  if (strspn (str,
	      /* Only the following chars are allowed before the colon */
	      " \t.*?abcdefghijklmnopqrstuvwxyz"
	      "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-") != colon_pos - str)
    goto invalid;

  if (DEVICE_X_P (d))
    {
      XrmDatabase db = XtDatabase (DEVICE_X_DISPLAY (d));
      XrmPutLineResource (&db, str);
    }

  return Qnil;
}


/************************************************************************/
/*                   display information functions                      */
/************************************************************************/

DEFUN ("default-x-device", Fdefault_x_device, Sdefault_x_device, 0, 0, 0 /*
Return the default X device for resourcing.
This is the first-created X device that still exists.
*/ )
     ()
{
  return Vdefault_x_device;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0 /*
Return the visual class of the X display `device' is on.
The returned value will be one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
*/ )
     (device)
     Lisp_Object device;
{
  switch (DefaultVisualOfScreen
	  (DefaultScreenOfDisplay (get_x_display (device)))->class)
    {
    case StaticGray:  return (intern ("static-gray"));
    case GrayScale:   return (intern ("gray-scale"));
    case StaticColor: return (intern ("static-color"));
    case PseudoColor: return (intern ("pseudo-color"));
    case TrueColor:   return (intern ("true-color"));
    case DirectColor: return (intern ("direct-color"));
    default:
      error ("display has an unknown visual class");
    }

  return Qnil;	/* suppress compiler warning */
}

static int
x_device_pixel_width (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayWidth (dpy, DefaultScreen (dpy));
}

static int
x_device_pixel_height (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayHeight (dpy, DefaultScreen (dpy));
}

static int
x_device_mm_width (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayWidthMM (dpy, DefaultScreen (dpy));
}

static int
x_device_mm_height (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayHeightMM (dpy, DefaultScreen (dpy));
}

static int
x_device_bitplanes (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayPlanes (dpy, DefaultScreen (dpy));
}

static int
x_device_color_cells (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  return DisplayCells (dpy, DefaultScreen (dpy));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0 /*
Return the vendor ID string of the X server `device' on.
*/ )
     (device)
  Lisp_Object device;
{
  Display *dpy = get_x_display (device);
  char *vendor = ServerVendor (dpy);

  if (vendor)
    return (build_string (vendor));
  else
    return (build_string (""));
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0 /*
Return the version numbers of the X server `device' is on.
The returned value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the vendor-specific release
number.  See also `x-server-vendor'.
*/ )
     (device)
  Lisp_Object device;
{
  Display *dpy = get_x_display (device);

  return list3 (make_int (ProtocolVersion (dpy)),
		make_int (ProtocolRevision (dpy)),
		make_int (VendorRelease (dpy)));
}

DEFUN ("x-valid-keysym-name-p", Fx_valid_keysym_name_p, Sx_valid_keysym_name_p,
       1, 1, 0 /*
Return true if KEYSYM names a keysym that the X library knows about.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
*/ )
     (keysym)
     Lisp_Object keysym;
{
  CONST char *keysym_ext;
  
  CHECK_STRING (keysym);
  GET_C_STRING_CTEXT_DATA_ALLOCA (keysym, keysym_ext);
  if (XStringToKeysym (keysym_ext))
    return Qt;
  return Qnil;
}

DEFUN ("x-keysym-on-keyboard-p", Fx_keysym_on_keyboard_p, Sx_keysym_on_keyboard_p,
       1, 2, 0 /*
Return true if KEYSYM names a key on the keyboard of DEVICE.
More precisely, return true if pressing a physical key
on the keyboard of DEVICE without any modifier keys generates KEYSYM.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
*/ )
     (keysym, device)
     Lisp_Object keysym, device;
{
  struct device *d = decode_device(device);
  CONST char *keysym_string;
  KeySym  keysym_KeySym;
  KeySym *keysym_ptr, *keysym_last;
  int min_code, max_code, keysyms_per_code;
  
  if (!DEVICE_X_P (d))
    signal_simple_error ("Not an X device", device);
  CHECK_STRING (keysym);
  GET_C_STRING_CTEXT_DATA_ALLOCA (keysym, keysym_string);
  keysym_KeySym = XStringToKeysym (keysym_string);
  if (!keysym_KeySym)           /* Invalid keysym */
    return Qnil;
  
  XDisplayKeycodes (DEVICE_X_DISPLAY (d), &min_code, &max_code);
  keysyms_per_code = DEVICE_X_DATA (d)->x_keysym_map_keysyms_per_code;
  keysym_ptr       = DEVICE_X_DATA (d)->x_keysym_map;
  keysym_last      = keysym_ptr + (max_code - min_code) * keysyms_per_code;
  for ( ; keysym_ptr <= keysym_last; keysym_ptr += keysyms_per_code)
    {
      if (keysym_KeySym == *keysym_ptr)
        return Qt;
    }
  
  return Qnil;
}


/************************************************************************/
/*                          grabs and ungrabs                           */
/************************************************************************/

DEFUN ("x-grab-pointer", Fx_grab_pointer, Sx_grab_pointer, 0, 3, 0 /*
Grab the pointer and restrict it to its current window.
If optional DEVICE argument is nil, the default device will be used.
If optional CURSOR argument is non-nil, change the pointer shape to that
 until `x-ungrab-pointer' is called (it should be an object returned by the
 `make-cursor-glyph' function).
If the second optional argument IGNORE-KEYBOARD is non-nil, ignore all
  keyboard events during the grab.
Returns t if the grab is successful, nil otherwise.
*/ )
  (device, cursor, ignore_keyboard)
     Lisp_Object device, cursor, ignore_keyboard;
{
  Window w;
  int pointer_mode, result;
  struct device *d = decode_x_device (device);

  if (!NILP (cursor))
    {
      CHECK_POINTER_GLYPH (cursor);
      cursor = glyph_image_instance (cursor, device, ERROR_ME, 0);
    }

  if (!NILP (ignore_keyboard))
    pointer_mode = GrabModeSync;
  else
    pointer_mode = GrabModeAsync;

  w = XtWindow (FRAME_X_TEXT_WIDGET (device_selected_frame (d)));

  /* #### Possibly this needs to gcpro the cursor somehow, but it doesn't
     seem to cause a problem if XFreeCursor is called on a cursor in use
     in a grab; I suppose the X server counts the grab as a reference
     and doesn't free it until it exits? */
  result = XGrabPointer (DEVICE_X_DISPLAY (d), w,
			 False,
			 ButtonMotionMask | ButtonPressMask
			 | ButtonReleaseMask | PointerMotionHintMask,
			 GrabModeAsync,	      /* Keep pointer events flowing */
			 pointer_mode,	      /* Stall keyboard events */
			 w,		      /* Stay in this window */
			 (NILP (cursor) ? 0
			  : XIMAGE_INSTANCE_X_CURSOR (cursor)),
			 CurrentTime);
  return ((result == GrabSuccess) ? Qt : Qnil);
}

DEFUN ("x-ungrab-pointer", Fx_ungrab_pointer, Sx_ungrab_pointer, 0, 1, 0 /*
Release a pointer grab made with `x-grab-pointer'.
If optional first arg DEVICE is nil the default device is used.
If it is t the pointer will be released on all X devices.
*/ )
     (device)
     Lisp_Object device;
{
  if (!EQ (device, Qt))
    {
      Display *dpy = get_x_display (device);
      XUngrabPointer (dpy, CurrentTime);
    }
  else
    {
      Lisp_Object devcons, concons;

      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  struct device *d = XDEVICE (XCAR (devcons));

	  if (DEVICE_X_P (d))
	    XUngrabPointer (DEVICE_X_DISPLAY (d), CurrentTime);
	}
    }

  return Qnil;
}

DEFUN ("x-grab-keyboard", Fx_grab_keyboard, Sx_grab_keyboard, 0, 1, 0 /*
Grab the keyboard on the given device (defaulting to the selected one).
So long as the keyboard is grabbed, all keyboard events will be delivered
to emacs -- it is not possible for other X clients to eavesdrop on them.
Ungrab the keyboard with `x-ungrab-keyboard' (use an unwind-protect).
Returns t if the grab was successful; nil otherwise.
*/ )
     (device)
     Lisp_Object device;
{
  struct device *d = decode_x_device (device);
  Window w = XtWindow (FRAME_X_TEXT_WIDGET (device_selected_frame (d)));
  Display *dpy = DEVICE_X_DISPLAY (d);
  Status status;
  XSync (dpy, False);
  status = XGrabKeyboard (dpy, w, True,
			  /* I don't really understand sync-vs-async
			     grabs, but this is what xterm does. */
			  GrabModeAsync, GrabModeAsync,
			  /* Use the timestamp of the last user action
			     read by emacs proper; xterm uses CurrentTime
			     but there's a comment that says "wrong"...
			     (Despite the name this is the time of the
			     last key or mouse event.) */
			  DEVICE_X_MOUSE_TIMESTAMP (d));
  if (status == GrabSuccess)
    {
      /* The XUngrabKeyboard should generate a FocusIn back to this
         window but it doesn't unless we explicitly set focus to the
         window first (which should already have it.  The net result
         is that without this call when x-ungrab-keyboard is called
         the selected frame ends up not having focus. */
      XSetInputFocus (dpy, w, RevertToParent, DEVICE_X_MOUSE_TIMESTAMP (d));
      return Qt;
    }
  else
    return Qnil;
}

DEFUN ("x-ungrab-keyboard", Fx_ungrab_keyboard, Sx_ungrab_keyboard, 0, 1, 0 /*
Release a keyboard grab made with `x-grab-keyboard'.
*/ )
     (device)
     Lisp_Object device;
{
  Display *dpy = get_x_display (device);
  XUngrabKeyboard (dpy, CurrentTime);
  return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_x (void)
{
  defsubr (&Sx_debug_mode);
  defsubr (&Sx_get_resource);
  defsubr (&Sx_get_resource_prefix);
  defsubr (&Sx_put_resource);

  defsubr (&Sdefault_x_device);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_valid_keysym_name_p);
  defsubr (&Sx_keysym_on_keyboard_p);

  defsubr (&Sx_grab_pointer);
  defsubr (&Sx_ungrab_pointer);
  defsubr (&Sx_grab_keyboard);
  defsubr (&Sx_ungrab_keyboard);

  defsymbol (&Qx_error, "x-error");
  defsymbol (&Qinit_pre_x_win, "init-pre-x-win");
  defsymbol (&Qinit_post_x_win, "init-post-x-win");
}

void
console_type_create_device_x (void)
{
  CONSOLE_HAS_METHOD (x, init_device);
  CONSOLE_HAS_METHOD (x, finish_init_device);
  CONSOLE_HAS_METHOD (x, mark_device);
  CONSOLE_HAS_METHOD (x, delete_device);
  CONSOLE_HAS_METHOD (x, device_pixel_width);
  CONSOLE_HAS_METHOD (x, device_pixel_height);
  CONSOLE_HAS_METHOD (x, device_mm_width);
  CONSOLE_HAS_METHOD (x, device_mm_height);
  CONSOLE_HAS_METHOD (x, device_bitplanes);
  CONSOLE_HAS_METHOD (x, device_color_cells);
}

void
vars_of_device_x (void)
{
  DEFVAR_LISP ("x-emacs-application-class", &Vx_emacs_application_class /*
The X application class of the XEmacs process.
This controls, among other things, the name of the `app-defaults' file
that XEmacs will use.  For changes to this variable to take effect, they
must be made before the connection to the X server is initialized, that is,
this variable may only be changed before emacs is dumped, or by setting it
in the file lisp/term/x-win.el.
*/ );
  Vx_emacs_application_class = Fpurecopy (build_string ("Emacs"));

  DEFVAR_LISP ("x-initial-argv-list", &Vx_initial_argv_list /*
You don't want to know.
This is used during startup to communicate the remaining arguments in
`command-line-args-left' to the C code, which passes the args to
the X initialization code, which removes some args, and then the
args are placed back into `x-initial-arg-list' and thence into
`command-line-args-left'.  Perhaps `command-line-args-left' should
just reside in C.
*/ );
  Vx_initial_argv_list = Qnil;

  Fprovide (Qx);

  staticpro (&Vdefault_x_device);
  Vdefault_x_device = Qnil;

  error_expected = 0;
  error_occurred = 0;

  in_resource_setting = 0;
  in_specifier_change_function = 0;
}
