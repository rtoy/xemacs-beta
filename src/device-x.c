/* Device functions for X windows.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002, 2004, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

/* 7-8-00 !!#### This file needs definite Mule review. */

/* Original authors: Jamie Zawinski and the FSF */
/* Rewritten by Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "elhash.h"
#include "events.h"
#include "faces.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "process.h"		/* for egetenv */
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "console-x-impl.h"
#include "glyphs-x.h"
#include "fontcolor-x.h"

#include "sysfile.h"
#include "systime.h"

#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* Numerous places access the fields of
				   a core widget directly.  We could
				   use XtGetValues(), but ... */
#include "gccache-x.h"
#include <X11/Shell.h>
#include <X11/Xmu/Error.h>

#if defined(HAVE_SHLIB) && defined(LWLIB_USES_ATHENA) && !defined(HAVE_ATHENA_3D)
#include "sysdll.h"
#endif /* HAVE_SHLIB and LWLIB_USES_ATHENA and not HAVE_ATHENA_3D */

Lisp_Object Vx_app_defaults_directory;
#ifdef MULE
Lisp_Object Qget_coding_system_from_locale;
#endif

/* Qdisplay in general.c */
Lisp_Object Qx_error;
Lisp_Object Qmake_device_early_x_entry_point, Qmake_device_late_x_entry_point;

/* The application class of Emacs. */
Lisp_Object Vx_emacs_application_class;

Lisp_Object Vx_initial_argv_list; /* #### ugh! */

/* Shut up G++ 4.3. */
#define Xrm_ODR(option,resource,type,default) \
  { (String) option, (String) resource, type, default }

static XrmOptionDescRec emacs_options[] =
{
  Xrm_ODR ("-geometry", ".geometry", XrmoptionSepArg, NULL),
  Xrm_ODR ("-iconic", ".iconic", XrmoptionNoArg, (String) "yes"),

  Xrm_ODR ("-internal-border-width", "*EmacsFrame.internalBorderWidth", XrmoptionSepArg, NULL),
  Xrm_ODR ("-ib",                    "*EmacsFrame.internalBorderWidth", XrmoptionSepArg, NULL),
  Xrm_ODR ("-scrollbar-width",       "*EmacsFrame.scrollBarWidth",      XrmoptionSepArg, NULL),
  Xrm_ODR ("-scrollbar-height",      "*EmacsFrame.scrollBarHeight",     XrmoptionSepArg, NULL),

  Xrm_ODR ("-privatecolormap", ".privateColormap", XrmoptionNoArg,  (String) "yes"),
  Xrm_ODR ("-visual",   ".EmacsVisual",	    XrmoptionSepArg, NULL),

  /* #### Beware!  If the type of the shell changes, update this. */
  Xrm_ODR ("-T",        "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL),
  Xrm_ODR ("-wn",       "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL),
  Xrm_ODR ("-title",    "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL),

  Xrm_ODR ("-iconname", "*TopLevelEmacsShell.iconName", XrmoptionSepArg, NULL),
  Xrm_ODR ("-in",       "*TopLevelEmacsShell.iconName", XrmoptionSepArg, NULL),
  Xrm_ODR ("-mc",       "*pointerColor",                XrmoptionSepArg, NULL),
  Xrm_ODR ("-cr",       "*cursorColor",                 XrmoptionSepArg, NULL),
  Xrm_ODR ("-fontset",  "*FontSet",                     XrmoptionSepArg, NULL),
};

static const struct memory_description x_device_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct x_device, x_keysym_map_hash_table) },
  { XD_LISP_OBJECT, offsetof (struct x_device, WM_COMMAND_frame) },
  { XD_END }
};

#ifdef NEW_GC
DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("x-device", x_device,
				      0, x_device_data_description_1,
				      Lisp_X_Device);
#else /* not NEW_GC */
extern const struct sized_memory_description x_device_data_description;

const struct sized_memory_description x_device_data_description = {
  sizeof (struct x_device), x_device_data_description_1
};
#endif /* not NEW_GC */

/* Functions to synchronize mirroring resources and specifiers */
int in_resource_setting;

/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* JH 97/11/25 removed the static declaration because I need it during setup in event-Xt... */
struct device * get_device_from_display_1 (Display *dpy);
struct device *
get_device_from_display_1 (Display *dpy)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      if (DEVICE_X_P (d) && DEVICE_X_DISPLAY (d) == dpy)
	return d;
    }

  return 0;
}

struct device *
get_device_from_display (Display *dpy)
{
#define FALLBACK_RESOURCE_NAME "xemacs"
  struct device *d = get_device_from_display_1 (dpy);

  if (!d)
    {
      /* This isn't one of our displays.  Let's crash? */
      stderr_out
	("\n%s: Fatal X Condition.  Asked about display we don't own: \"%s\"\n",
	 (STRINGP (Vinvocation_name) ?
	  (char *) XSTRING_DATA (Vinvocation_name) : FALLBACK_RESOURCE_NAME),
	 DisplayString (dpy) ? DisplayString (dpy) : "???");
      ABORT();
    }

#undef FALLBACK_RESOURCE_NAME

  return d;
}

struct device *
decode_x_device (Lisp_Object device)
{
  device = wrap_device (decode_device (device));
  CHECK_X_DEVICE (device);
  return XDEVICE (device);
}

static Display *
get_x_display (Lisp_Object device)
{
  return DEVICE_X_DISPLAY (decode_x_device (device));
}

static Lisp_Object
coding_system_of_xrm_database (XrmDatabase USED_IF_MULE (db))
{
#ifdef MULE
  const Extbyte *locale;
  Lisp_Object localestr;
  static XrmDatabase last_xrm_db; 

  /* This will always be zero, nil or an actual coding system object, so no
     need to worry about GCPROing it--it'll be protected from garbage
     collection by means of Vcoding_system_hash_table in file-coding.c. */
  static Lisp_Object last_coding_system; 

  if (db == last_xrm_db)
    {
      return last_coding_system; 
    }

  last_xrm_db = db;

  locale = XrmLocaleOfDatabase (db);
  localestr = build_extstring (locale, Qbinary);
  last_coding_system = call1 (Qget_coding_system_from_locale, localestr);

  return last_coding_system;
#else
  return Qbinary;
#endif
}


/************************************************************************/
/*		      initializing an X connection			*/
/************************************************************************/

static struct device *device_being_initialized = NULL;

static void
allocate_x_device_struct (struct device *d)
{
#ifdef NEW_GC
  d->device_data = XX_DEVICE (ALLOC_NORMAL_LISP_OBJECT (x_device));
#else /* not NEW_GC */
  d->device_data = xnew_and_zero (struct x_device);
#endif /* not NEW_GC */
}

static void
Xatoms_of_device_x (struct device *d)
{
  Display *D = DEVICE_X_DISPLAY (d);

  DEVICE_XATOM_WM_PROTOCOLS    (d) = XInternAtom (D, "WM_PROTOCOLS",    False);
  DEVICE_XATOM_WM_DELETE_WINDOW(d) = XInternAtom (D, "WM_DELETE_WINDOW",False);
  DEVICE_XATOM_WM_SAVE_YOURSELF(d) = XInternAtom (D, "WM_SAVE_YOURSELF",False);
  DEVICE_XATOM_WM_TAKE_FOCUS   (d) = XInternAtom (D, "WM_TAKE_FOCUS",   False);
  DEVICE_XATOM_WM_STATE        (d) = XInternAtom (D, "WM_STATE",        False);
}

static void
sanity_check_geometry_resource (Display *dpy)
{
  Extbyte *app_name, *app_class, *s;
  Extbyte buf1 [255], buf2 [255];
  Extbyte *type;
  XrmValue value;
  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  strcpy (buf1, app_name);
  strcpy (buf2, app_class);
  for (s = buf1; *s; s++) if (*s == '.') *s = '_';
  strcat (buf1, "._no_._such_._resource_.geometry");
  strcat (buf2, "._no_._such_._resource_.Geometry");
  if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
    {
      Ibyte *app_name_int, *app_class_int, *value_addr_int;
      Lisp_Object codesys = coding_system_of_xrm_database (XtDatabase (dpy));
      app_name_int = EXTERNAL_TO_ITEXT (app_name, codesys);
      app_class_int = EXTERNAL_TO_ITEXT (app_class, codesys);
      value_addr_int = EXTERNAL_TO_ITEXT (value.addr, codesys);

      warn_when_safe (Qgeometry, Qerror,
		      "\n"
"Apparently \"%s*geometry: %s\" or \"%s*geometry: %s\" was\n"
"specified in the resource database.  Specifying \"*geometry\" will make\n"
"XEmacs (and most other X programs) malfunction in obscure ways. (i.e.\n"
"the Xt or Xm libraries will probably crash, which is a very bad thing.)\n"
"You should always use \".geometry\" or \"*EmacsFrame.geometry\" instead.\n",
		      app_name_int, value_addr_int,
		      app_class_int, value_addr_int);
      suppress_early_error_handler_backtrace = 1;
      syntax_error ("Invalid geometry resource", Qunbound);
    }
}

static void
x_init_device_class (struct device *d)
{
  if (DEVICE_X_DEPTH(d) > 2)
    {
      switch (DEVICE_X_VISUAL(d)->X_CLASSFIELD)
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

/*
 * Figure out what application name to use for xemacs
 *
 * Since we have decomposed XtOpenDisplay into XOpenDisplay and
 * XtDisplayInitialize, we no longer get this for free.
 *
 * If there is a `-name' argument in argv, use that.
 * Otherwise use the last component of argv[0].
 *
 * I have removed the gratuitous use of getenv("RESOURCE_NAME")
 * which was in X11R5, but left the matching of any prefix of `-name'.
 * Finally, if all else fails, return `xemacs', as it is more
 * appropriate (X11R5 returns `main').
 */
static Extbyte *
compute_x_app_name (int argc, Extbyte **argv)
{
  int i;
  Extbyte *ptr;

  for (i = 1; i < argc - 1; i++)
    if (!strncmp(argv[i], "-name", max (2, strlen (argv[1]))))
      return argv[i+1];

  if (argc > 0 && argv[0] && *argv[0])
    return (ptr = strrchr (argv[0], '/')) ? ++ptr : argv[0];

  return (Extbyte *) "xemacs";	/* shut up g++ 4.3 */
}

/*
 * This function figures out whether the user has any resources of the
 * form "XEmacs.foo" or "XEmacs*foo".
 *
 * Currently we only consult the display's global resources; to look
 * for screen specific resources, we would need to also consult:
 * xdefs = XScreenResourceString(ScreenOfDisplay(dpy, scrno));
 */
static int
have_xemacs_resources_in_xrdb (Display *dpy)
{
  const char *xdefs, *key;
  int len;

  key = "XEmacs";
  len = strlen (key);

  if (!dpy)
    return 0;

  xdefs = XResourceManagerString (dpy);      /* don't free - owned by X */
  while (xdefs && *xdefs)
    {
      if (strncmp (xdefs, key, len) == 0  &&
          (xdefs[len] == '*' || xdefs[len] == '.'))
        return 1;

      while (*xdefs && *xdefs++ != '\n')     /* find start of next entry.. */
        ;
    }

  return 0;
}

/* Only the characters [-_A-Za-z0-9] are allowed in the individual
   components of a resource.  Convert invalid characters to `-' */

static char valid_resource_char_p[256];

static void
validify_resource_component (Extbyte *str, Bytecount len)
{
  for (; len; len--, str++)
    if (!valid_resource_char_p[(unsigned char) (*str)])
      *str = '-';
}

static void
Dynarr_add_validified_lisp_string (Extbyte_dynarr *cda, Lisp_Object str)
{
  Bytecount len;
  Extbyte *data;

  LISP_STRING_TO_SIZED_EXTERNAL (str, data, len, Qbinary); 
  if (len)
    {
      Dynarr_add_many (cda, data, len);
      validify_resource_component (Dynarr_atp (cda, Dynarr_length (cda) - len),
				   len);
    }
}

#if 0
/* compare visual info for qsorting */
static int
x_comp_visual_info (const void *elem1, const void *elem2)
{
  XVisualInfo *left, *right;

  left = (XVisualInfo *)elem1;
  right = (XVisualInfo *)elem2;

  if ( left == NULL )
    return -1;
  if ( right == NULL )
    return 1;

  if ( left->depth > right->depth )
    return 1;
  else if ( left->depth == right->depth )
    {
      if ( left->colormap_size > right->colormap_size )
	return 1;
      if ( left->X_CLASSFIELD > right->X_CLASSFIELD )
	return 1;
      else if ( left->X_CLASSFIELD < right->X_CLASSFIELD )
	return -1;
      else
	return 0;
    }
  else
    return -1;
}
#endif /* if 0 */

#define XXX_IMAGE_LIBRARY_IS_SOMEWHAT_BROKEN
static Visual *
x_try_best_visual_class (Screen *screen, int scrnum, int visual_class)
{
  Display *dpy = DisplayOfScreen (screen);
  XVisualInfo vi_in;
  XVisualInfo *vi_out = NULL;
  int out_count;

  vi_in.X_CLASSFIELD = visual_class;
  vi_in.screen = scrnum;
  vi_out = XGetVisualInfo (dpy, (VisualClassMask | VisualScreenMask),
			   &vi_in, &out_count);
  if ( vi_out )
    {
      int i, best;
      Visual *visual;
      for (i = 0, best = 0; i < out_count; i++)
	/* It's better if it's deeper, or if it's the same depth with
	   more cells (does that ever happen?  Well, it could...)
	   NOTE: don't allow pseudo color to get larger than 8! */
	if (((vi_out [i].depth > vi_out [best].depth) ||
	     ((vi_out [i].depth == vi_out [best].depth) &&
	      (vi_out [i].colormap_size > vi_out [best].colormap_size)))
#ifdef XXX_IMAGE_LIBRARY_IS_SOMEWHAT_BROKEN
	    /* For now, the image library doesn't like PseudoColor visuals
	       of depths other than 1 or 8.  Depths greater than 8 only occur
	       on machines which have TrueColor anyway, so probably we'll end
	       up using that (it is the one that `Best' would pick) but if a
	       PseudoColor visual is explicitly specified, pick the 8 bit one.
	    */
	    && (visual_class != PseudoColor ||
		vi_out [i].depth == 1 ||
		vi_out [i].depth == 8)
#endif

	    /* SGI has 30-bit deep visuals.  Ignore them.
                (We only have 24-bit data anyway.)
              */
	    && (vi_out [i].depth <= 24)
	    )
	  best = i;
      visual = vi_out[best].visual;
      XFree ((char *) vi_out);
      return visual;
    }
  else
    return 0;
}

static int
x_get_visual_depth (Display *dpy, Visual *visual)
{
  XVisualInfo vi_in;
  XVisualInfo *vi_out;
  int out_count, d;

  vi_in.visualid = XVisualIDFromVisual (visual);
  vi_out = XGetVisualInfo (dpy, /*VisualScreenMask|*/VisualIDMask,
			   &vi_in, &out_count);
  assert (vi_out);
  d = vi_out [0].depth;
  XFree ((char *) vi_out);
  return d;
}

static Visual *
x_try_best_visual (Display *dpy, int scrnum)
{
  Visual *visual = NULL;
  Screen *screen = ScreenOfDisplay (dpy, scrnum);
  if ((visual = x_try_best_visual_class (screen, scrnum, TrueColor))
      && x_get_visual_depth (dpy, visual) >= 16 )
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, PseudoColor)))
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, TrueColor)))
    return visual;
#ifdef DIRECTCOLOR_WORKS
  if ((visual = x_try_best_visual_class (screen, scrnum, DirectColor)))
    return visual;
#endif

  visual = DefaultVisualOfScreen (screen);
  if ( x_get_visual_depth (dpy, visual) >= 8 )
    return visual;

  if ((visual = x_try_best_visual_class (screen, scrnum, StaticGray)))
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, GrayScale)))
    return visual;
  return DefaultVisualOfScreen (screen);
}


static void
x_init_device (struct device *d, Lisp_Object UNUSED (props))
{
  /* !!#### */
  Lisp_Object display;
  Display *dpy;
  Widget app_shell;
  int argc;
  Extbyte **argv;
  const char *app_class;
  const char *app_name;
  const char *disp_name;
  Visual *visual = NULL;
  int depth = 8;		/* shut up the compiler */
  Colormap cmap;
  int screen;
  /* */
  int best_visual_found = 0;

  /* Run the elisp side of the X device initialization, allowing it to set
     x-emacs-application-class and x-app-defaults-directory.  */
  call0 (Qmake_device_early_x_entry_point);

#if defined(HAVE_SHLIB) && defined(LWLIB_USES_ATHENA) && !defined(HAVE_ATHENA_3D)
  /*
   * In order to avoid the lossage with flat Athena widgets dynamically
   * linking to one of the ThreeD variants, using the dynamic symbol helpers
   * to look for symbols that shouldn't be there and refusing to run if they
   * are seems a less toxic idea than having XEmacs crash when we try and
   * use a subclass of a widget that has changed size.
   *
   * It's ugly, I know, and not going to work everywhere. It seems better to
   * do our damnedest to try and tell the user what to expect rather than
   * simply blow up though.
   *
   * All the ThreeD variants I have access to define the following function
   * symbols in the shared library. The flat Xaw library does not define them:
   *
   * Xaw3dComputeBottomShadowRGB
   * Xaw3dComputeTopShadowRGB
   *
   * So far only Linux has shown this problem. This seems to be portable to
   * all the distributions (certainly all the ones I checked - Debian and
   * Redhat)
   *
   * This will only work, sadly, with dlopen() -- the other dynamic linkers
   * are simply not capable of doing what is needed. :/
   */

  {
    /* Get a dll handle to the main process. */
    dll_handle xaw_dll_handle = dll_open (Qnil);

    /* Did that fail?  If so, continue without error.
     * We could die here but, well, that's unfriendly and all -- plus I feel
     * better about some crashing somewhere rather than preventing a perfectly
     * good configuration working just because dll_open failed.
     */
    if (xaw_dll_handle != NULL)
      {
	/* Look for the Xaw3d function */
	dll_func xaw_function_handle =
	  dll_function (xaw_dll_handle,
			(const Ibyte *) "Xaw3dComputeTopShadowRGB");

	/* If we found it, warn the user in big, nasty, unfriendly letters */
	if (xaw_function_handle != NULL)
	  {
	    warn_when_safe (Qdevice, Qcritical, "\n"
"It seems that XEmacs is built dynamically linked to the flat Athena widget\n"
"library but it finds a 3D Athena variant with the same name at runtime.\n"
"\n"
"This WILL cause your XEmacs process to dump core at some point.\n"
"You should not continue to use this binary without resolving this issue.\n"
"\n"
"This can be solved with the xaw-wrappers package under Debian\n"
"(register XEmacs as incompatible with all 3d widget sets, see\n"
"update-xaw-wrappers(8) and .../doc/xaw-wrappers/README.packagers).  It\n"
"can be verified by checking the runtime path in /etc/ld.so.conf and by\n"
"using `ldd /path/to/xemacs' under other Linux distributions.  One\n"
"solution is to use LD_PRELOAD or LD_LIBRARY_PATH to force ld.so to\n"
"load the flat Athena widget library instead of the aliased 3D widget\n"
"library (see ld.so(8) for use of these environment variables).\n\n"
			    );

	  }

	/* Otherwise release the handle to the library
	 * No error catch here; I can't think of a way to recover anyhow.
	 */
	dll_close (xaw_dll_handle);
      }
  }
#endif /* HAVE_SHLIB and LWLIB_USES_ATHENA and not HAVE_ATHENA_3D */

  display = DEVICE_CONNECTION (d);

  allocate_x_device_struct (d);

  make_argc_argv (Vx_initial_argv_list, &argc, &argv);

  disp_name = LISP_STRING_TO_EXTERNAL (display, Qctext);

  /*
   * Break apart the old XtOpenDisplay call into XOpenDisplay and
   * XtDisplayInitialize so we can figure out whether there
   * are any XEmacs resources in the resource database before
   * we initialize Xt.  This is so we can automagically support
   * both `Emacs' and `XEmacs' application classes.
   */
  slow_down_interrupts ();
  /* May not be needed but XtOpenDisplay could not deal with signals here. */
  device_being_initialized = d;
  dpy = DEVICE_X_DISPLAY (d) = XOpenDisplay (disp_name);
  device_being_initialized = NULL;
  speed_up_interrupts ();

  if (dpy == 0)
    {
      suppress_early_error_handler_backtrace = 1;
      gui_error ("X server not responding\n", display);
    }

  if (STRINGP (Vx_emacs_application_class) &&
      XSTRING_LENGTH (Vx_emacs_application_class) > 0)
    app_class = LISP_STRING_TO_EXTERNAL (Vx_emacs_application_class, Qctext);
  else
    {
      if (egetenv ("USE_EMACS_AS_DEFAULT_APPLICATION_CLASS"))
	{
	  app_class = (NILP (Vx_emacs_application_class)  &&
		       have_xemacs_resources_in_xrdb (dpy))
	    ? "XEmacs"
	    : "Emacs";
	}
      else 
	{
	  app_class = "XEmacs";
	}

      /* need to update Vx_emacs_application_class: */
      Vx_emacs_application_class = build_cistring (app_class);
    }

  slow_down_interrupts ();
  /* May not be needed but XtOpenDisplay could not deal with signals here.
     Yuck. */
  XtDisplayInitialize (Xt_app_con, dpy, compute_x_app_name (argc, argv),
                       app_class, emacs_options,
                       XtNumber (emacs_options), &argc, (char **) argv);
  speed_up_interrupts ();

  screen = DefaultScreen (dpy);

#ifdef MULE
  {
    /* Read in locale-specific resources from
       data-directory/app-defaults/$LANG/Emacs.
       This is in addition to the standard app-defaults files, and
       does not override resources defined elsewhere */
    const Extbyte *data_dir;
    Extbyte *path;
    const Extbyte *format;
    XrmDatabase db = XtDatabase (dpy); /* #### XtScreenDatabase(dpy) ? */
    Extbyte *locale = xstrdup (XrmLocaleOfDatabase (db));
    Extbyte *locale_end;

    if (STRINGP (Vx_app_defaults_directory) &&
	XSTRING_LENGTH (Vx_app_defaults_directory) > 0)
      {
	LISP_PATHNAME_CONVERT_OUT (Vx_app_defaults_directory, data_dir);
	path = alloca_extbytes (strlen (data_dir) + strlen (locale) + 7);
	format = "%s%s/Emacs";
      }
    else if (STRINGP (Vdata_directory) && XSTRING_LENGTH (Vdata_directory) > 0)
      {
	LISP_PATHNAME_CONVERT_OUT (Vdata_directory, data_dir);
	path = alloca_extbytes (strlen (data_dir) + 13 + strlen (locale) + 7);
	format = "%sapp-defaults/%s/Emacs";
      }
    else
      {
	goto no_data_directory;
      }

    /*
     * The general form for $LANG is <language>_<country>.<encoding>.  Try
     * that form, <language>_<country> and <language> and load for first
     * app-defaults file found.
     */

    sprintf (path, format, data_dir, locale);
    if (!access (path, R_OK))
      XrmCombineFileDatabase (path, &db, False);

    if ((locale_end = strchr (locale, '.')))
      {
	*locale_end = '\0';
	sprintf (path, format, data_dir, locale);

	if (!access (path, R_OK))
	  XrmCombineFileDatabase (path, &db, False);
      }

    if ((locale_end = strchr (locale, '_')))
      {
	*locale_end = '\0';
	sprintf (path, format, data_dir, locale);

	if (!access (path, R_OK))
	  XrmCombineFileDatabase (path, &db, False);
      }

  no_data_directory:
    xfree (locale);
 }
#endif /* MULE */

  if (NILP (DEVICE_NAME (d)))
    DEVICE_NAME (d) = display;

  /* We're going to modify the string in-place, so be a nice XEmacs */
  DEVICE_NAME (d) = Fcopy_sequence (DEVICE_NAME (d));
  /* colons and periods can't appear in individual elements of resource
     strings */

  XtGetApplicationNameAndClass (dpy, (char **) &app_name, (char **) &app_class);
  /* search for a matching visual if requested by the user, or setup the display default */
  {
    int resource_name_length = max (sizeof (".emacsVisual"),
				    sizeof (".privateColormap"));
    char *buf1 = alloca_array (char, strlen (app_name)  + resource_name_length);
    char *buf2 = alloca_array (char, strlen (app_class) + resource_name_length);
    char *type;
    XrmValue value;

    sprintf (buf1, "%s.emacsVisual", app_name);
    sprintf (buf2, "%s.EmacsVisual", app_class);
    if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
      {
	int cnt = 0;
	int vis_class = PseudoColor;
	XVisualInfo vinfo;
	char *str = (char*) value.addr;

#define CHECK_VIS_CLASS(visual_class)					\
 else if (memcmp (str, #visual_class, sizeof (#visual_class) - 1) == 0)	\
	cnt = sizeof (#visual_class) - 1, vis_class = visual_class

	if (1)
	  ;
	CHECK_VIS_CLASS (StaticGray);
	CHECK_VIS_CLASS (StaticColor);
	CHECK_VIS_CLASS (TrueColor);
	CHECK_VIS_CLASS (GrayScale);
	CHECK_VIS_CLASS (PseudoColor);
	CHECK_VIS_CLASS (DirectColor);

	if (cnt)
	  {
	    depth = atoi (str + cnt);
	    if (depth == 0)
	      {
		stderr_out ("Invalid Depth specification in %s... "
			    "ignoring...\n", str);
	      }
	    else
	      {
		if (XMatchVisualInfo (dpy, screen, depth, vis_class, &vinfo))
		  {
		    visual = vinfo.visual;
		  }
		else
		  {
		    stderr_out ("Can't match the requested visual %s... "
				"using defaults\n", str);
		  }
	      }
	  }
	else
	  {
	    stderr_out ("Invalid Visual specification in %s... "
			"ignoring.\n", str);
	  }
      }
    if (visual == NULL)
      {
	/*
	  visual = DefaultVisual(dpy, screen);
	  depth = DefaultDepth(dpy, screen);
	*/
	visual = x_try_best_visual (dpy, screen);
	depth = x_get_visual_depth (dpy, visual);
	best_visual_found = (visual != DefaultVisual (dpy, screen));
      }

    /* If we've got the same visual as the default and it's PseudoColor,
       check to see if the user specified that we need a private colormap */
    if (visual == DefaultVisual (dpy, screen))
      {
	sprintf (buf1, "%s.privateColormap", app_name);
	sprintf (buf2, "%s.PrivateColormap", app_class);
	if ((visual->X_CLASSFIELD == PseudoColor) &&
	    (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value)
	     == True))
	  cmap = XCopyColormapAndFree (dpy, DefaultColormap (dpy, screen));
	else
	  cmap = DefaultColormap (dpy, screen);
      }
    else
      {
	if ( best_visual_found )
	  cmap = XCreateColormap (dpy,  RootWindow (dpy, screen), visual,
				  AllocNone);
	else
	  {
	    /* We have to create a matching colormap anyway...  ####
	       think about using standard colormaps (need the Xmu
	       libs?) */
	    cmap = XCreateColormap (dpy, RootWindow (dpy, screen), visual,
				    AllocNone);
	    XInstallColormap (dpy, cmap);
	  }
      }
  }

  DEVICE_X_VISUAL   (d) = visual;
  DEVICE_X_COLORMAP (d) = cmap;
  DEVICE_X_DEPTH    (d) = depth;
  validify_resource_component ((char *) XSTRING_DATA (DEVICE_NAME (d)),
			       XSTRING_LENGTH (DEVICE_NAME (d)));

  /* #### If we're going to implement X session management, this would
     be the place.  Make sure it doesn't conflict with GNOME. */
  {
    Arg al[3];
    Xt_SET_ARG (al[0], XtNvisual,   visual);
    Xt_SET_ARG (al[1], XtNdepth,    depth);
    Xt_SET_ARG (al[2], XtNcolormap, cmap);

    app_shell = XtAppCreateShell (NULL, app_class,
				  applicationShellWidgetClass,
				  dpy, al, countof (al));
  }

  DEVICE_XT_APP_SHELL (d) = app_shell;

#ifdef HAVE_XIM
  XIM_init_device(d);
#endif /* HAVE_XIM */

  /* Realize the app_shell so that its window exists for GC creation purposes,
     and set it to the size of the root window for child placement purposes */
  {
    Arg al[5];
    Xt_SET_ARG (al[0], XtNmappedWhenManaged, False);
    Xt_SET_ARG (al[1], XtNx, 0);
    Xt_SET_ARG (al[2], XtNy, 0);
    Xt_SET_ARG (al[3], XtNwidth,
		WidthOfScreen  (ScreenOfDisplay (dpy, screen)));
    Xt_SET_ARG (al[4], XtNheight,
		HeightOfScreen (ScreenOfDisplay (dpy, screen)));
    XtSetValues (app_shell, al, countof (al));
    XtRealizeWidget (app_shell);
  }

#ifdef HAVE_WMCOMMAND
  {
    int new_argc;
    Extbyte **new_argv;
    make_argc_argv (Vcommand_line_args, &new_argc, &new_argv);
    XSetCommand (XtDisplay (app_shell), XtWindow (app_shell),
		 (char **) new_argv, new_argc);
    free_argc_argv (new_argv);
  }
#endif /* HAVE_WMCOMMAND */

  Vx_initial_argv_list = make_arg_list (argc, argv);
  free_argc_argv (argv);

  DEVICE_X_WM_COMMAND_FRAME (d) = Qnil;

  sanity_check_geometry_resource (dpy);

  /* In event-Xt.c */
  x_init_modifier_mapping (d);

  DEVICE_INFD (d) = DEVICE_OUTFD (d) = ConnectionNumber (dpy);
  init_baud_rate (d);
  init_one_device (d);

  DEVICE_X_GC_CACHE (d) = make_gc_cache (dpy, XtWindow (app_shell));
  DEVICE_X_GRAY_PIXMAP (d) = None;
  Xatoms_of_device_x (d);
  Xatoms_of_select_x (d);
  Xatoms_of_fontcolor_x (d);
  x_init_device_class (d);
}

static void
x_finish_init_device (struct device *d, Lisp_Object UNUSED (props))
{
  call1 (Qmake_device_late_x_entry_point, wrap_device (d));
}

static void
x_mark_device (struct device *d)
{
  mark_object (DEVICE_X_WM_COMMAND_FRAME (d));
  mark_object (DEVICE_X_DATA (d)->x_keysym_map_hash_table);
}


/************************************************************************/
/*                       closing an X connection	                */
/************************************************************************/

#ifndef NEW_GC
static void
free_x_device_struct (struct device *d)
{
  xfree (d->device_data);
}
#endif /* not NEW_GC */

static void
x_delete_device (struct device *d)
{
  Display *display;
#ifdef FREE_CHECKING
  extern void (*__free_hook) (void *);
  int checking_free;
#endif

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

      if (DEVICE_XT_APP_SHELL (d))
	{
	  XtDestroyWidget (DEVICE_XT_APP_SHELL (d));
	  DEVICE_XT_APP_SHELL (d) = NULL;
	}

      XtCloseDisplay (display);
      DEVICE_X_DISPLAY (d) = 0;
#ifdef FREE_CHECKING
      if (checking_free)
	enable_strict_free_check ();
#endif
    }

#ifndef NEW_GC
  free_x_device_struct (d);
#endif /* not NEW_GC */
}


/************************************************************************/
/*				handle X errors				*/
/************************************************************************/

const Ascbyte *
x_event_name (int event_type)
{
  static const Ascbyte *events[] =
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

  if (event_type < 0 || event_type >= countof (events))
    return NULL;
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
x_error_handler_error (Lisp_Object UNUSED (data), Lisp_Object UNUSED (dummy))
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
      int depth;

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
	frame = wrap_frame (f);
	if (!NILP (condition_case_1 (Qerror, x_error_handler_do_enqueue,
				     frame, x_error_handler_error, Qnil)))
	  {
	    f->being_deleted = 1;
	    FRAME_VISIBLE_P (f) = 0;
	  }
	return 0;
      }
#endif /* EXTERNAL_WIDGET */

      /* #### this should issue a warning instead of outputting to stderr */
      depth = begin_dont_check_for_quit ();
#if 0
      /* This ends up calling X, which isn't allowed in an X error handler
       */
      stderr_out ("\n%s: ",
		  (STRINGP (Vinvocation_name)
		   ? (char *) XSTRING_DATA (Vinvocation_name)
		   : "xemacs"));
#endif
      XmuPrintDefaultErrorMessage (disp, event, stderr);
      unbind_to (depth);
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
  Extbyte buf[1024];
  Ibyte num[100];
  Lisp_Object data;
  if (! x_error_occurred_p (dpy))
    return 0;
  data = Qnil;
  qxesprintf (num, "0x%X", (unsigned int) last_error.resourceid);
  data = Fcons (build_istring (num), data);
  qxesprintf (num, "%d", last_error.request_code);
  XGetErrorDatabaseText (last_error.display, "XRequest", (char *) num, "",
			 buf, sizeof (buf));
  if (*buf)
    data = Fcons (build_extstring (buf, Qx_error_message_encoding), data);
  else
    {
      qxesprintf (num, "Request-%d", last_error.request_code);
      data = Fcons (build_istring (num), data);
    }
  XGetErrorText (last_error.display, last_error.error_code, buf, sizeof (buf));
  data = Fcons (build_extstring (buf, Qx_error_message_encoding), data);
 again:
  Fsignal (Qx_error, data);
  if (! resumable_p) goto again;
  return 1;
}

int
x_IO_error_handler (Display *disp)
{
  /* This function can GC */
  /* I'd like to just check for errno == 0 here and return, but that's
     too risky.  Returning from an X error handler gives X the chance to
     abort you.
     A "goto back_to_toplevel" could be used, but on second thought I
     decided it was a good idea to get the warning for now. */
  Lisp_Object dev;
  struct device *d = get_device_from_display_1 (disp);

  if (!d)
    d = device_being_initialized;

  assert (d != NULL);
  dev = wrap_device (d);

  /* The test against 0 is a hack for Mac OS X 10.9 and 10.10, which
     invoke this handler on successful deletion of a window. */
  if (errno != 0 && NILP (find_nonminibuffer_frame_not_on_device (dev)))
    {
      int depth = begin_dont_check_for_quit ();
      /* We're going down. */
      Ibyte *errmess;
      GET_STRERROR (errmess, errno);
      stderr_out ("\n%s: Fatal I/O Error %d (%s) on display "
		  "connection \"%s\"\n",
		  (STRINGP (Vinvocation_name) ?
		   (char *) XSTRING_DATA (Vinvocation_name) : "xemacs"),
		  errno, errmess, DisplayString (disp));
      stderr_out ("  after %lu requests (%lu known processed) with %d "
		  "events remaining.\n",
		  NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
		  QLength (disp));
      /* assert (!_Xdebug); */
      unbind_to (depth);
    }
  else
    {
      Ibyte *errmess;
      GET_STRERROR (errmess, errno);
      warn_when_safe
	(Qx, Qcritical,
	 "I/O Error %d (%s) on display connection\n"
	 "  \"%s\" after %lu requests (%lu known processed)\n"
	 "  with %d events remaining.\n"
	 "  Throwing to top level.\n",
	 errno, errmess, DisplayString (disp),
         NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
         QLength (disp));
    }

  /* According to X specs, we should not return from this function, or
     Xlib might just decide to exit().  So we mark the offending
     console for deletion and throw to top level.
     The test against 0 is a hack for Mac OS X 10.9 and 10.10, which
     invoke this handler on successful deletion of a window. */
  if (errno != 0 && d)
    {
      enqueue_magic_eval_event (io_error_delete_device, dev);
      DEVICE_X_BEING_DELETED (d) = 1;
    }

  redisplay_cancel_ritual_suicide();
  throw_or_bomb_out_unsafe (Qtop_level, Qnil, 0, Qnil, Qnil);

  RETURN_NOT_REACHED (0);
}

DEFUN ("x-debug-mode", Fx_debug_mode, 1, 2, 0, /*
With a true arg, make the connection to the X server synchronous.
With false, make it asynchronous.  Synchronous connections are much slower,
but are useful for debugging. (If you get X errors, make the connection
synchronous, and use a debugger to set a breakpoint on `x_error_handler'.
Your backtrace of the C stack will now be useful.  In asynchronous mode,
the stack above `x_error_handler' isn't helpful because of buffering.)
If DEVICE is not specified, the selected device is assumed.

Calling this function is the same as calling the C function `XSynchronize',
or starting the program with the `-sync' command line argument.
*/
       (arg, device))
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
		     char *fake_class, char *name, char *class_)
{
  char *stack [100][2];
  Widget this_widget;
  int count = 0;
  char *name_tail, *class_tail;

  if (widget)
    {
      for (this_widget = widget; this_widget;
	   this_widget = XtParent (this_widget))
	{
	  stack [count][0] = this_widget->core.name;
	  stack [count][1] = XtClass (this_widget)->core_class.class_name;
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
  class_ [0] = 0;

  name_tail  = name;
  class_tail = class_;
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

#endif /* 0 */

static Extbyte_dynarr *name_Extbyte_dynarr;
static Extbyte_dynarr *class_Extbyte_dynarr;

/* Given a locale and device specification from x-get-resource or
x-get-resource-prefix, return the resource prefix and display to
fetch the resource on. */

static void
x_get_resource_prefix (Lisp_Object locale, Lisp_Object device,
		       Display **display_out, Extbyte_dynarr *name,
		       Extbyte_dynarr *class_)
{
  if (NILP (locale))
    locale = Qglobal;
  if (NILP (Fvalid_specifier_locale_p (locale)))
    invalid_argument ("Invalid locale", locale);
  if (WINDOWP (locale))
    /* #### I can't come up with any coherent way of naming windows.
       By relative position?  That seems tricky because windows
       can change position, be split, etc.  By order of creation?
       That seems less than useful. */
    signal_error (Qunimplemented,
		  "Windows currently can't be resourced", locale);

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
	device = get_default_device (Qx);
      if (NILP (device))
	{
	  *display_out = 0;
	  return;
	}
    }

  *display_out = DEVICE_X_DISPLAY (XDEVICE (device));

  {
    Extbyte *appname, *appclass;
    int name_len, class_len;
    XtGetApplicationNameAndClass (*display_out, &appname, &appclass);
    name_len  = strlen (appname);
    class_len = strlen (appclass);
    Dynarr_add_many (name,  appname,  name_len);
    Dynarr_add_many (class_, appclass, class_len);
    validify_resource_component (Dynarr_begin (name), name_len);
    validify_resource_component (Dynarr_begin (class_), class_len);
  }

  if (EQ (locale, Qglobal))
    return;
  if (BUFFERP (locale))
    {
      Dynarr_add_literal_string (name, ".buffer.");
      /* we know buffer is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fbuffer_name (locale));
      Dynarr_add_literal_string (class_, ".EmacsLocaleType.EmacsBuffer");
    }
  else if (FRAMEP (locale))
    {
      Dynarr_add_literal_string (name, ".frame.");
      /* we know frame is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fframe_name (locale));
      Dynarr_add_literal_string (class_, ".EmacsLocaleType.EmacsFrame");
    }
  else
    {
      assert (DEVICEP (locale));
      Dynarr_add_literal_string (name, ".device.");
      /* we know device is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fdevice_name (locale));
      Dynarr_add_literal_string (class_, ".EmacsLocaleType.EmacsDevice");
    }
  return;
}

DEFUN ("x-get-resource", Fx_get_resource, 3, 6, 0, /*
Retrieve an X resource from the resource manager.

The first arg is the name of the resource to retrieve, such as "font".
The second arg is the class of the resource to retrieve, such as "Font".
The third arg must be one of the symbols `string', `integer', `natnum', or
  `boolean', specifying the type of object that the database is searched for.
The fourth arg is the locale to search for the resources on, and can
  currently be a buffer, a frame, a device, or `global'.  If omitted, it
  defaults to `global'.
The fifth arg is the device to search for the resources on. (The resource
  database for a particular device is constructed by combining non-device-
  specific resources such as any command-line resources specified and any
  app-defaults files found [or the fallback resources supplied by XEmacs,
  if no app-defaults file is found] with device-specific resources such as
  those supplied using xrdb.) If omitted, it defaults to the device of
  LOCALE, if a device can be derived (i.e. if LOCALE is a frame or device),
  and otherwise defaults to the value of `default-x-device'.
The sixth arg NOERROR, if non-nil, means do not signal an error if a
  bogus resource specification was retrieved (e.g. if a non-integer was
  given when an integer was requested).  In this case, a warning is issued
  instead, unless NOERROR is t, in which case no warning is issued.

The resource names passed to this function are looked up relative to the
locale.

If you want to search for a subresource, you just need to specify the
resource levels in NAME and CLASS.  For example, NAME could be
"modeline.attributeFont", and CLASS "Face.AttributeFont".

Specifically,

1) If LOCALE is a buffer, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-BUFFER)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.buffer.BUFFER-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsBuffer.Foreground",
			"String");

2) If LOCALE is a frame, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-FRAME)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.frame.FRAME-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsFrame.Foreground",
			"String");

3) If LOCALE is a device, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-DEVICE)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.device.DEVICE-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsDevice.Foreground",
			"String");

4) If LOCALE is `global', a call

    (x-get-resource "foreground" "Foreground" 'string 'global)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.foreground",
			"Emacs.Foreground",
			"String");

Note that for `global', no prefix is added other than that of the
application itself; thus, you can use this locale to retrieve
arbitrary application resources, if you really want to.

The returned value of this function is nil if the queried resource is not
found.  If the third arg is `string', a string is returned, and if it is
`integer', an integer is returned.  If the third arg is `boolean', then the
returned value is the list (t) for true, (nil) for false, and is nil to
mean ``unspecified''.
*/
       (name, class_, type, locale, device, noerror))
{
  Extbyte *name_string, *class_string;
  Extbyte *raw_result;
  XrmDatabase db;
  Display *display;
  Error_Behavior errb = decode_error_behavior_flag (noerror);
  Lisp_Object codesys;

  CHECK_STRING (name);
  CHECK_STRING (class_);
  CHECK_SYMBOL (type);

  Dynarr_reset (name_Extbyte_dynarr);
  Dynarr_reset (class_Extbyte_dynarr);

  x_get_resource_prefix (locale, device, &display,
			 name_Extbyte_dynarr, class_Extbyte_dynarr);
  if (!display)
    return Qnil;

  db = XtDatabase (display);
  codesys = coding_system_of_xrm_database (db);
  Dynarr_add (name_Extbyte_dynarr, '.');
  Dynarr_add_ext_lisp_string (name_Extbyte_dynarr, name, Qbinary);
  Dynarr_add (class_Extbyte_dynarr, '.');
  Dynarr_add_ext_lisp_string (class_Extbyte_dynarr, class_, Qbinary);
  Dynarr_add (name_Extbyte_dynarr,  '\0');
  Dynarr_add (class_Extbyte_dynarr, '\0');

  name_string  = Dynarr_begin (name_Extbyte_dynarr);
  class_string = Dynarr_begin (class_Extbyte_dynarr);

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
      {
	maybe_signal_error_2
	  (Qstructure_formation_error,
	   "class list and name list must be the same length", name, class_,
	   Qresource, errb);
	return Qnil;
      }
    result = XrmQGetResource (db, namelist, classlist, &xrm_type, &xrm_value);

    if (result != True || xrm_type != string_quark)
      return Qnil;
    raw_result = (Extbyte *) xrm_value.addr;
  }

  if (EQ (type, Qstring))
    return build_extstring (raw_result, codesys);
  else if (EQ (type, Qboolean))
    {
      if (!strcasecmp (raw_result, "off")   ||
	  !strcasecmp (raw_result, "false") ||
	  !strcasecmp (raw_result, "no"))
	return Fcons (Qnil, Qnil);
      if (!strcasecmp (raw_result, "on")   ||
	  !strcasecmp (raw_result, "true") ||
	  !strcasecmp (raw_result, "yes"))
	return Fcons (Qt, Qnil);
      return maybe_signal_continuable_error_2
	(Qinvalid_operation, "Can't convert to a Boolean",
	 build_extstring (name_string, Qbinary),
	 build_extstring (raw_result, codesys), Qresource,
	 errb);
    }
  else if (EQ (type, Qinteger) || EQ (type, Qnatnum))
    {
      int i;
      char c;
      if (1 != sscanf (raw_result, "%d%c", &i, &c))
      return maybe_signal_continuable_error_2
	(Qinvalid_operation, "Can't convert to an integer",
	 build_extstring (name_string, Qbinary),
	 build_extstring (raw_result, codesys), Qresource,
	 errb);
      else if (EQ (type, Qnatnum) && i < 0)
	return maybe_signal_continuable_error_2
	  (Qinvalid_argument, "Invalid numerical value for resource",
	   make_fixnum (i), build_extstring (name_string, Qbinary),
	   Qresource, errb);
      else
	return make_fixnum (i);
    }
  else
    {
      return maybe_signal_continuable_error
	(Qwrong_type_argument, "Should be string, integer, natnum or boolean",
	 type, Qresource, errb);
    }
}

DEFUN ("x-get-resource-prefix", Fx_get_resource_prefix, 1, 2, 0, /*
Return the resource prefix for LOCALE on DEVICE.
The resource prefix is the strings used to prefix resources if
the LOCALE and DEVICE arguments were passed to `x-get-resource'.
The returned value is a cons of a name prefix and a class prefix.
For example, if LOCALE is a frame, the returned value might be
\("xemacs.frame.FRAME-NAME" . "Emacs.EmacsLocaleType.EmacsFrame").
If no valid X device for resourcing can be obtained, this function
returns nil. (In such a case, `x-get-resource' would always return nil.)
*/
       (locale, device))
{
  Display *display;

  Dynarr_reset (name_Extbyte_dynarr );
  Dynarr_reset (class_Extbyte_dynarr);

  x_get_resource_prefix (locale, device, &display,
			 name_Extbyte_dynarr, class_Extbyte_dynarr);
  if (!display)
    return Qnil;

  return Fcons (make_string ((Ibyte *) Dynarr_begin (name_Extbyte_dynarr),
			     Dynarr_length (name_Extbyte_dynarr)),
		make_string ((Ibyte *) Dynarr_begin (class_Extbyte_dynarr),
			     Dynarr_length (class_Extbyte_dynarr)));
}

DEFUN ("x-put-resource", Fx_put_resource, 1, 2, 0, /*
Add a resource to the resource database for DEVICE.
RESOURCE-LINE specifies the resource to add and should be a
standard resource specification.
*/
       (resource_line, device))
{
  struct device *d = decode_device (device);

  if (DEVICE_X_P (d))
    {
      XrmDatabase db = XtDatabase (DEVICE_X_DISPLAY (d));
      Extbyte *str, *colon_pos;

      CHECK_STRING (resource_line);
      str = LISP_STRING_TO_EXTERNAL (resource_line,
				     coding_system_of_xrm_database (db));
      if (!(colon_pos = strchr (str, ':')) || strchr (str, '\n'))
      invalid:
	syntax_error ("Invalid resource line", resource_line);
      if ((int)
	  strspn (str,
		  /* Only the following chars are allowed before the colon */
		  " \t.*?abcdefghijklmnopqrstuvwxyz"
		  "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")
	  != colon_pos - str)
	goto invalid;

      XrmPutLineResource (&db, str);
    }

  return Qnil;
}


/************************************************************************/
/*                   display information functions                      */
/************************************************************************/

DEFUN ("default-x-device", Fdefault_x_device, 0, 0, 0, /*
Return the default X device for resourcing.
This is the first-created X device that still exists.
See also `default-device'.
*/
       ())
{
  return get_default_device (Qx);
}

DEFUN ("x-display-visual-class", Fx_display_visual_class, 0, 1, 0, /*
Return the visual class of the X display DEVICE is using.
This can be altered from the default at startup using the XResource "EmacsVisual".
The returned value will be one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
*/
       (device))
{
  Visual *vis = DEVICE_X_VISUAL (decode_x_device (device));
  switch (vis->X_CLASSFIELD)
    {
    case StaticGray:  return intern ("static-gray");
    case GrayScale:   return intern ("gray-scale");
    case StaticColor: return intern ("static-color");
    case PseudoColor: return intern ("pseudo-color");
    case TrueColor:   return intern ("true-color");
    case DirectColor: return intern ("direct-color");
    default:
      invalid_state ("display has an unknown visual class", Qunbound);
      return Qnil;	/* suppress compiler warning */
    }
}

DEFUN ("x-display-visual-depth", Fx_display_visual_depth, 0, 1, 0, /*
Return the bitplane depth of the visual the X display DEVICE is using.
*/
       (device))
{
   return make_fixnum (DEVICE_X_DEPTH (decode_x_device (device)));
}

static Lisp_Object
x_device_system_metrics (struct device *d,
			 enum device_metrics m)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  switch (m)
    {
    case DM_size_device:
      return Fcons (make_fixnum (DisplayWidth (dpy, DefaultScreen (dpy))),
		    make_fixnum (DisplayHeight (dpy, DefaultScreen (dpy))));
    case DM_size_device_mm:
      return Fcons (make_fixnum (DisplayWidthMM (dpy, DefaultScreen (dpy))),
		    make_fixnum (DisplayHeightMM (dpy, DefaultScreen (dpy))));
    case DM_num_bit_planes:
      return make_fixnum (DisplayPlanes (dpy, DefaultScreen (dpy)));
    case DM_num_color_cells:
      return make_fixnum (DisplayCells (dpy, DefaultScreen (dpy)));
    case DM_num_screens:
      return make_fixnum (ScreenCount (dpy));
    case DM_backing_store:
      switch (DoesBackingStore (DefaultScreenOfDisplay (dpy)))
	{
	case Always:
	  return intern ("always");
	case WhenMapped:
	  return intern ("when-mapped");
	default:
	  return intern ("not-useful");
	}
    case DM_save_under:
      return (DoesSaveUnders (DefaultScreenOfDisplay (dpy)) == True)
	? Qt : Qnil;
    default: /* No such device metric property for X devices  */
      return Qunbound;
    }
}

DEFUN ("x-server-vendor", Fx_server_vendor, 0, 1, 0, /*
Return the vendor ID string of the X server DEVICE is on.
Return the empty string if the vendor ID string cannot be determined.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  Extbyte *vendor = ServerVendor (dpy);

  return build_extstring (vendor ? vendor : "", Qx_hpc_encoding);
}

DEFUN ("x-server-version", Fx_server_version, 0, 1, 0, /*
Return the version numbers of the X server DEVICE is on.
The returned value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the vendor-specific release
number.  See also `x-server-vendor'.
*/
       (device))
{
  Display *dpy = get_x_display (device);

  return list3 (make_fixnum (ProtocolVersion  (dpy)),
		make_fixnum (ProtocolRevision (dpy)),
		make_fixnum (VendorRelease    (dpy)));
}

DEFUN ("x-valid-keysym-name-p", Fx_valid_keysym_name_p, 1, 1, 0, /*
Return true if KEYSYM names a keysym that the X library knows about.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
*/
       (keysym))
{
  const Extbyte *keysym_ext;

  CHECK_STRING (keysym);
  keysym_ext = LISP_STRING_TO_EXTERNAL (keysym, Qctext);

  return XStringToKeysym (keysym_ext) ? Qt : Qnil;
}

DEFUN ("x-keysym-hash-table", Fx_keysym_hash_table, 0, 1, 0, /*
Return a hash table containing a key for all keysyms on DEVICE.
DEVICE must be an X11 display device.  See `x-keysym-on-keyboard-p'.
*/
       (device))
{
  struct device *d = decode_device (device);
  if (!DEVICE_X_P (d))
    gui_error ("Not an X device", device);

  return DEVICE_X_DATA (d)->x_keysym_map_hash_table;
}

/************************************************************************/
/*                          grabs and ungrabs                           */
/************************************************************************/

DEFUN ("x-grab-pointer", Fx_grab_pointer, 0, 3, 0, /*
Grab the pointer and restrict it to its current window.
If optional DEVICE argument is nil, the default device will be used.
If optional CURSOR argument is non-nil, change the pointer shape to that
 until `x-ungrab-pointer' is called (it should be an object returned by the
 `make-cursor-glyph' function).
If the second optional argument IGNORE-KEYBOARD is non-nil, ignore all
  keyboard events during the grab.
Returns t if the grab is successful, nil otherwise.
*/
       (device, cursor, ignore_keyboard))
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
			 ButtonMotionMask  |
			 ButtonPressMask   |
			 ButtonReleaseMask |
			 PointerMotionHintMask,
			 GrabModeAsync,	      /* Keep pointer events flowing */
			 pointer_mode,	      /* Stall keyboard events */
			 w,		      /* Stay in this window */
			 (NILP (cursor) ? 0
			  : XIMAGE_INSTANCE_X_CURSOR (cursor)),
			 CurrentTime);
  return (result == GrabSuccess) ? Qt : Qnil;
}

DEFUN ("x-ungrab-pointer", Fx_ungrab_pointer, 0, 1, 0, /*
Release a pointer grab made with `x-grab-pointer'.
If optional first arg DEVICE is nil the default device is used.
If it is t the pointer will be released on all X devices.
*/
       (device))
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

DEFUN ("x-grab-keyboard", Fx_grab_keyboard, 0, 1, 0, /*
Grab the keyboard on the given device (defaulting to the selected one).
So long as the keyboard is grabbed, all keyboard events will be delivered
to emacs -- it is not possible for other X clients to eavesdrop on them.
Ungrab the keyboard with `x-ungrab-keyboard' (use an unwind-protect).
Returns t if the grab is successful, nil otherwise.
*/
       (device))
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

DEFUN ("x-ungrab-keyboard", Fx_ungrab_keyboard, 0, 1, 0, /*
Release a keyboard grab made with `x-grab-keyboard'.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  XUngrabKeyboard (dpy, CurrentTime);
  return Qnil;
}

DEFUN ("x-get-font-path", Fx_get_font_path, 0, 1, 0, /*
Get the X Server's font path.

See also `x-set-font-path'.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  int ndirs_return;
  const Extbyte **directories =
    (const Extbyte **) XGetFontPath (dpy, &ndirs_return);
  Lisp_Object font_path = Qnil;

  if (!directories)
    gui_error ("Can't get X font path", device);

  while (ndirs_return--)
      font_path = Fcons (build_extstring (directories[ndirs_return],
                                           Qfile_name),
			 font_path);

  XFreeFontPath ((char **)directories);

  return font_path;
}

DEFUN ("x-set-font-path", Fx_set_font_path, 1, 2, 0, /*
Set the X Server's font path to FONT-PATH.

There is only one font path per server, not one per client.  Use this
sparingly.  It uncaches all of the X server's font information.

Font directories should end in the path separator and should contain
a file called fonts.dir usually created with the program mkfontdir.

Setting the FONT-PATH to nil tells the X server to use the default
font path.

See also `x-get-font-path'.
*/
       (font_path, device))
{
  Display *dpy = get_x_display (device);
  Extbyte **directories;
  int i=0,ndirs=0;

  {
    EXTERNAL_LIST_LOOP_2 (path_entry, font_path)
      {
	CHECK_STRING (path_entry);
	ndirs++;
      }
  }

  directories = alloca_array (Extbyte *, ndirs);

  {
    EXTERNAL_LIST_LOOP_2 (path_entry, font_path)
      LISP_PATHNAME_CONVERT_OUT (path_entry, directories[i++]);
  }

  expect_x_error (dpy);
  XSetFontPath (dpy, directories, ndirs);
  signal_if_x_error (dpy, 1/*resumable_p*/);

  return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_x (void)
{
#ifdef NEW_GC
  INIT_LISP_OBJECT (x_device);
#endif /* NEW_GC */

  DEFSUBR (Fx_debug_mode);
  DEFSUBR (Fx_get_resource);
  DEFSUBR (Fx_get_resource_prefix);
  DEFSUBR (Fx_put_resource);

  DEFSUBR (Fdefault_x_device);
  DEFSUBR (Fx_display_visual_class);
  DEFSUBR (Fx_display_visual_depth);
  DEFSUBR (Fx_server_vendor);
  DEFSUBR (Fx_server_version);
  DEFSUBR (Fx_valid_keysym_name_p);
  DEFSUBR (Fx_keysym_hash_table);

  DEFSUBR (Fx_grab_pointer);
  DEFSUBR (Fx_ungrab_pointer);
  DEFSUBR (Fx_grab_keyboard);
  DEFSUBR (Fx_ungrab_keyboard);

  DEFSUBR (Fx_get_font_path);
  DEFSUBR (Fx_set_font_path);

  DEFSYMBOL (Qx_error);
  DEFSYMBOL (Qmake_device_early_x_entry_point);
  DEFSYMBOL (Qmake_device_late_x_entry_point);

#ifdef MULE
  DEFSYMBOL (Qget_coding_system_from_locale);
#endif
}

void
reinit_console_type_create_device_x (void)
{
  /* Initialize variables to speed up X resource interactions */
  const Ascbyte *valid_resource_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
  while (*valid_resource_chars)
    valid_resource_char_p[(unsigned int) (*valid_resource_chars++)] = 1;

  name_Extbyte_dynarr  = Dynarr_new (Extbyte);
  class_Extbyte_dynarr = Dynarr_new (Extbyte);
}

void
console_type_create_device_x (void)
{
  reinit_console_type_create_device_x ();
  CONSOLE_HAS_METHOD (x, init_device);
  CONSOLE_HAS_METHOD (x, finish_init_device);
  CONSOLE_HAS_METHOD (x, mark_device);
  CONSOLE_HAS_METHOD (x, delete_device);
  CONSOLE_HAS_METHOD (x, device_system_metrics);
}

void
reinit_vars_of_device_x (void)
{
  error_expected = 0;
  error_occurred = 0;

  in_resource_setting = 0;
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

If this variable is nil on startup, the application uses `XEmacs'.  Versions
previous to 21.5.21 examined the resource database and used `XEmacs' if any
resources beginning with that string existed, and `Emacs' otherwise, for
greater backward compatibility. However, this has always tended to conflict
with GNU Emacs, so this behavior is deprecated--in the short term, you can
restore it in a post-21.5.21 XEmacs by setting the
USE_EMACS_AS_DEFAULT_APPLICATION_CLASS environment variable to some value,
but in the medium and long term, you should migrate your X resources.
*/ );
  Vx_emacs_application_class = Qnil;

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

  DEFVAR_LISP ("x-app-defaults-directory", &Vx_app_defaults_directory /*
Used by the Lisp code to communicate to the low level X initialization
where the localized init files are.
*/ );
  Vx_app_defaults_directory = Qnil;

  Fprovide (Qx);
}
