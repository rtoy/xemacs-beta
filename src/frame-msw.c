/* Functions for the mswindows window system.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* Authorship:

   Ultimately based on FSF.
   Substantially rewritten for XEmacs by Ben Wing.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-msw.h"
#include "glyphs-msw.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "redisplay.h"
#include "window.h"

#define MSWINDOWS_FRAME_STYLE (WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_OVERLAPPEDWINDOW)
#define MSWINDOWS_POPUP_STYLE (WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_POPUP \
			       | WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_MINIMIZEBOX)

#define MSWINDOWS_FRAME_EXSTYLE WS_EX_OVERLAPPEDWINDOW
#define MSWINDOWS_POPUP_EXSTYLE WS_EX_PALETTEWINDOW

/* Default popup left top corner offset from the same
   corner of the parent frame, in pixel */
#define POPUP_OFFSET 30

/* Default popup size, in characters */
#define POPUP_WIDTH 30
#define POPUP_HEIGHT 10

#ifdef HAVE_MENUBARS
#define ADJR_MENUFLAG TRUE
#else
#define ADJR_MENUFLAG FALSE
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_mswindows_frame_plist;

/* Lisp_Object Qname, Qheight, Qwidth, Qinitially_unmapped, Qpopup, Qtop, Qleft; */
Lisp_Object Qinitially_unmapped, Qpopup;

/* This does not need to be GC protected, as it holds a
   frame Lisp_Object already protected by Fmake_frame */
Lisp_Object Vmswindows_frame_being_created;

/* Geometry, in characters, as specified by proplist during frame
   creation. Memebers are set to -1 for unspecified */
XEMACS_RECT_WH mswindows_frame_target_rect;

static void
mswindows_init_frame_1 (struct frame *f, Lisp_Object props)
{
  Lisp_Object initially_unmapped;
  Lisp_Object name, height, width, popup, top, left;
  Lisp_Object frame_obj = Qnil;
  RECT rect;
  XEMACS_RECT_WH rect_default;
  DWORD style, exstyle;
  HWND hwnd, hwnd_parent;

  /* Pick up relevant properties */
  initially_unmapped = Fplist_get (props, Qinitially_unmapped, Qnil);
  name = Fplist_get (props, Qname, Qnil);
  
  popup = Fplist_get (props, Qpopup, Qnil);
  if (EQ (popup, Qt))
    popup = Fselected_frame (Qnil);

  left = Fplist_get (props, Qleft, Qnil);
  if (!NILP (left))
    CHECK_INT (left);

  top = Fplist_get (props, Qtop, Qnil);
  if (!NILP (top))
    CHECK_INT (top);

  width = Fplist_get (props, Qwidth, Qnil);
  if (!NILP (width))
    CHECK_INT (width);

  height = Fplist_get (props, Qheight, Qnil);
  if (!NILP (height))
    CHECK_INT (height);

  mswindows_frame_target_rect.left = NILP (left) ? -1 : abs (XINT (left));
  mswindows_frame_target_rect.top = NILP (top) ? -1 : abs (XINT (top));
  mswindows_frame_target_rect.width = NILP (width) ? -1 : abs (XINT (width));
  mswindows_frame_target_rect.height = NILP (height) ? -1 : abs (XINT (height));

  f->frame_data = xnew_and_zero (struct mswindows_frame);

  /* Misc frame stuff */
  FRAME_MSWINDOWS_DATA(f)->button2_need_lbutton = 0;
  FRAME_MSWINDOWS_DATA(f)->button2_need_rbutton = 0;
  FRAME_MSWINDOWS_DATA(f)->button2_is_down = 0;
  FRAME_MSWINDOWS_DATA(f)->ignore_next_lbutton_up = 0;
  FRAME_MSWINDOWS_DATA(f)->ignore_next_rbutton_up = 0;
  FRAME_MSWINDOWS_DATA(f)->sizing = 0;
  FRAME_MSWINDOWS_MENU_HASHTABLE(f) = Qnil;
#ifdef HAVE_TOOLBARS
  FRAME_MSWINDOWS_TOOLBAR_HASHTABLE(f) = Fmake_hashtable (make_int (50), 
							  Qequal);
#endif

  /* Will initialize these in WM_SIZE handler. We cannot do it now,
     because we do not know what is CW_USEDEFAULT height and width */
  FRAME_WIDTH (f) = 0;
  FRAME_HEIGHT (f) = 0;
  FRAME_PIXWIDTH (f) = 0;
  FRAME_PIXHEIGHT (f) = 0;

  if (NILP (popup))
    {
      style = MSWINDOWS_FRAME_STYLE;
      exstyle = MSWINDOWS_FRAME_EXSTYLE;
      hwnd_parent = NULL;

      /* We always create am overlapped frame with default size,
	 and later adjust only requested geometry parameters. */
      rect_default.left = rect_default.top = CW_USEDEFAULT;
      rect_default.width = rect_default.height = CW_USEDEFAULT;
    }
  else
    {
      style = MSWINDOWS_POPUP_STYLE;
      exstyle = MSWINDOWS_POPUP_EXSTYLE;

      CHECK_MSWINDOWS_FRAME (popup);
      hwnd_parent = FRAME_MSWINDOWS_HANDLE (XFRAME (popup));
      assert (IsWindow (hwnd_parent));

      /* We cannot use CW_USEDEFAULT when creating a popup window.
	 So by default, we offset the new popup 30 pixels right
	 and down from its parent, and give it size of 30x10 characters.
	 These dimensions look adequate on both high and low res monitors */
      GetWindowRect (hwnd_parent, &rect);
      rect_default.left = rect.left + POPUP_OFFSET;
      rect_default.top = rect.top + POPUP_OFFSET;
      char_to_real_pixel_size (f, POPUP_WIDTH, POPUP_HEIGHT,
			       &rect_default.width, &rect_default.height);
    }

  AdjustWindowRectEx(&rect, style, ADJR_MENUFLAG, exstyle);

  XSETFRAME (frame_obj, f);

  Vmswindows_frame_being_created = frame_obj;

  hwnd = CreateWindowEx (exstyle,
			 XEMACS_CLASS,
			 STRINGP(f->name) ? XSTRING_DATA(f->name) :
			 (STRINGP(name) ? XSTRING_DATA(name) : XEMACS_CLASS),
			 style,
			 rect_default.left, rect_default.top,
			 rect_default.width, rect_default.height,
			 hwnd_parent, NULL, NULL, NULL);

  Vmswindows_frame_being_created = Qnil;

  if (hwnd == NULL)
    error ("System call to create frame failed");
			   
  FRAME_MSWINDOWS_HANDLE(f) = hwnd;

  SetWindowLong (hwnd, XWL_FRAMEOBJ, (LONG)LISP_TO_VOID(frame_obj));
  FRAME_MSWINDOWS_DC(f) = GetDC (hwnd);
  FRAME_MSWINDOWS_CDC(f) = CreateCompatibleDC (FRAME_MSWINDOWS_CDC(f));
  SetTextAlign (FRAME_MSWINDOWS_DC(f), TA_BASELINE | TA_LEFT | TA_NOUPDATECP);
}

#if 0 /* #### unused */
static void
mswindows_init_frame_2 (struct frame *f, Lisp_Object props)
{
}
#endif

/* Called after frame's properties are set */
static void
mswindows_init_frame_3 (struct frame *f)
{
  /* Don't do this earlier or we get a WM_PAINT before the frame is ready */
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOWNORMAL);
  SetForegroundWindow (FRAME_MSWINDOWS_HANDLE(f));
  DragAcceptFiles (FRAME_MSWINDOWS_HANDLE(f), TRUE);
}

static void
mswindows_after_init_frame (struct frame *f, int first_on_device,
		            int first_on_console)
{
  /* Windows, unlike X, is very synchronous. After the initial
     frame is created, it will never be displayed, except for 
     hollow border, unless we start pumping messages. Load progress
     messages show in the bottom of the hollow frame, which is ugly.
     We redipsplay the initial frame here, so modeline and root window
     backgorund show.
  */
  if (first_on_console)
    redisplay ();
}

static void
mswindows_mark_frame (struct frame *f, void (*markobj) (Lisp_Object))
{
  ((markobj) (FRAME_MSWINDOWS_MENU_HASHTABLE (f)));
#ifdef HAVE_TOOLBARS
  ((markobj) (FRAME_MSWINDOWS_TOOLBAR_HASHTABLE (f)));
#endif
}

static void
mswindows_focus_on_frame (struct frame *f)
{
  SetForegroundWindow (FRAME_MSWINDOWS_HANDLE(f));
}

static void
mswindows_delete_frame (struct frame *f)
{
  if (f->frame_data)
    {
      DeleteDC(FRAME_MSWINDOWS_CDC(f));
      ReleaseDC(FRAME_MSWINDOWS_HANDLE(f), FRAME_MSWINDOWS_DC(f));
      DestroyWindow(FRAME_MSWINDOWS_HANDLE(f));
      xfree (f->frame_data);
    }
  f->frame_data = 0;
}

static void
mswindows_set_frame_size (struct frame *f, int cols, int rows)
{
  RECT rect;
  rect.left = rect.top = 0;
  rect.right = cols;
  rect.bottom = rows;

  AdjustWindowRectEx (&rect,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
		      GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));

  if (IsIconic (FRAME_MSWINDOWS_HANDLE(f)) || IsZoomed (FRAME_MSWINDOWS_HANDLE(f)))
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);

  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL, 
		0, 0, rect.right-rect.left, rect.bottom-rect.top,
		SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING | SWP_NOMOVE);
}

static void
mswindows_set_frame_position (struct frame *f, int xoff, int yoff)
{
  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL, 
		xoff, yoff, 0, 0,
		SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING | SWP_NOSIZE);
}

static void
mswindows_make_frame_visible (struct frame *f) 
{
  if (f->iconified)
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);
  else
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOWNORMAL);
  f->visible = 1;
  f->iconified = 0;
}

static void
mswindows_make_frame_invisible (struct frame *f) 
{
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_HIDE);
  f->visible = -1;
}

static int
mswindows_frame_totally_visible_p (struct frame *f)
{
  RECT rc_me, rc_other, rc_temp;
  HWND hwnd = FRAME_MSWINDOWS_HANDLE(f);

  /* We test against not a whole window rectangle, only agaist its
     client part. So, if non-client are is covered and client area is
     not, we return true. */
  GetClientRect (hwnd, &rc_me);
  MapWindowPoints (hwnd, HWND_DESKTOP, (LPPOINT)&rc_me, 2);

  /* First see if we're off the desktop */
  GetWindowRect (GetDesktopWindow(), &rc_other);
  UnionRect(&rc_temp, &rc_me, &rc_other);
  if (!EqualRect (&rc_temp, &rc_other))
    return 0;
  
  /* Then see if any window above us obscures us */
  while ((hwnd = GetWindow (hwnd, GW_HWNDPREV)) != NULL)
    if (IsWindowVisible (hwnd))
      {
	GetWindowRect (hwnd, &rc_other);
	if (IntersectRect(&rc_temp, &rc_me, &rc_other))
	  return 0;
      }

  return 1;
}

static int
mswindows_frame_visible_p (struct frame *f)
{
  return IsWindowVisible (FRAME_MSWINDOWS_HANDLE(f))
    && !IsIconic (FRAME_MSWINDOWS_HANDLE(f));
}


static void
mswindows_iconify_frame (struct frame *f)
{
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_MINIMIZE);
  f->visible = 0;
  f->iconified = 1;
}

static int
mswindows_frame_iconified_p (struct frame *f)
{
  return IsIconic (FRAME_MSWINDOWS_HANDLE(f));
}

static void
mswindows_set_frame_icon (struct frame *f)
{
  if (IMAGE_INSTANCEP (f->icon)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (f->icon)))
    {
      if (!XIMAGE_INSTANCE_MSWINDOWS_ICON (f->icon))
	{
	  mswindows_initialize_image_instance_icon (XIMAGE_INSTANCE (f->icon), 
						    FALSE);
	}
      
      SetClassLong (FRAME_MSWINDOWS_HANDLE (f), GCL_HICON, 
		    (LONG) XIMAGE_INSTANCE_MSWINDOWS_ICON (f->icon));
    }
}

static void
mswindows_set_frame_pointer (struct frame *f)
{
  if (IMAGE_INSTANCEP (f->pointer)
      && IMAGE_INSTANCE_TYPE (XIMAGE_INSTANCE (f->pointer)) == IMAGE_POINTER)
    {
      SetClassLong (FRAME_MSWINDOWS_HANDLE (f), GCL_HCURSOR,
		    (LONG) XIMAGE_INSTANCE_MSWINDOWS_ICON (f->pointer));
    }
}

static void
mswindows_set_mouse_position (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  POINT pt;

  pt.x = w->pixel_left + x;
  pt.y = w->pixel_top  + y;
  ClientToScreen (FRAME_MSWINDOWS_HANDLE(f), &pt);
  SetCursorPos (pt.x, pt.y);
}

static int
mswindows_get_mouse_position (struct device *d, Lisp_Object *frame, int *x, int *y)
{
  POINT pt;
  HWND hwnd;

  GetCursorPos (&pt);

  /* What's under cursor? */
  hwnd = WindowFromPoint (pt);
  if (hwnd == NULL)
    return 0;

  /* Get grandest parent of the window */
  {
    HWND hwnd_parent;
    while ((hwnd_parent = GetParent (hwnd)) != NULL)
      hwnd = hwnd_parent;
  }

  /* Make sure it belongs to us */
  if (GetWindowThreadProcessId (hwnd, NULL) != GetCurrentThreadId ())
    return 0;

  /* And that the window is an XEmacs frame */
  {
    char class_name [sizeof(XEMACS_CLASS) + 1];
    if (!GetClassName (hwnd, class_name, sizeof(XEMACS_CLASS))
	|| strcmp (class_name, XEMACS_CLASS) != 0)
      return 0;
  }

  /* Yippie! */
  ScreenToClient (hwnd, &pt);
  VOID_TO_LISP (*frame, GetWindowLong (hwnd, XWL_FRAMEOBJ));
  *x = pt.x;
  *y = pt.y;
  return 1;
}

static void
mswindows_raise_frame (struct frame *f)
{
  BringWindowToTop (FRAME_MSWINDOWS_HANDLE(f));
  /* XXX Should we do SetWindowForeground too ? */
}

static void
mswindows_lower_frame (struct frame *f)
{
  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), HWND_BOTTOM, 0, 0, 0, 0,
		SWP_NOSIZE | SWP_NOMOVE | SWP_NOSENDCHANGING);
}

static void
mswindows_set_title_from_bufbyte (struct frame *f, Bufbyte *title) 
{
  unsigned int new_checksum = hash_string (title, strlen (title));
  if (new_checksum != FRAME_MSWINDOWS_TITLE_CHECKSUM(f))
    {
      FRAME_MSWINDOWS_TITLE_CHECKSUM(f) = new_checksum;
      SetWindowText (FRAME_MSWINDOWS_HANDLE(f), title);
    }
}

static Lisp_Object
mswindows_frame_property (struct frame *f, Lisp_Object property)
{
  if (EQ (Qleft, property) || EQ (Qtop, property))
    {
      RECT rc;
      GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rc);
      return make_int (EQ (Qtop,  property) ? rc.top : rc.left);
    }
  return Qunbound;
}

static int
mswindows_internal_frame_property_p (struct frame *f, Lisp_Object property)
{
  return EQ (property, Qleft)
    || EQ (property, Qtop);
  /* #### frame-x.c has also this. Why?
    || STRINGP (property);
  */
}

static Lisp_Object
mswindows_frame_properties (struct frame *f)
{
  Lisp_Object props = Qnil;
  RECT rc;
  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rc);

  props = cons3 (Qtop,  make_int (rc.top), props);
  props = cons3 (Qleft, make_int (rc.left), props);

  return props;
}

static void
mswindows_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  int x=0, y=0;
  int width = 0, height = 0;
  BOOL width_specified_p = FALSE;
  BOOL height_specified_p = FALSE;
  BOOL x_specified_p = FALSE;
  BOOL y_specified_p = FALSE;
  Lisp_Object tail;

  /* Extract the properties from plist */
  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      Lisp_Object prop = Fcar (tail);
      Lisp_Object val = Fcar (Fcdr (tail));

      if (SYMBOLP (prop))
	{
	  /* Kludge to handle the font property. */
	  if (EQ (prop, Qfont))
	    {
	      /* If the value is not a string we silently ignore it. */
	      if (STRINGP (val))
		{
		  Lisp_Object frm, font_spec;
		  
		  XSETFRAME (frm, f);
		  font_spec = Fget (Fget_face (Qdefault), Qfont, Qnil);

		  Fadd_spec_to_specifier (font_spec, val, frm, Qnil, Qnil);
		  update_frame_face_values (f);
		}
	    }
	  else if (EQ (prop, Qwidth))
	    {
	      CHECK_INT (val);
	      width = XINT (val);
	      width_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qheight))
	    {
	      CHECK_INT (val);
	      height = XINT (val);
	      height_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qleft))
	    {
	      CHECK_INT (val);
	      x = XINT (val);
	      x_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qtop))
	    {
	      CHECK_INT (val);
	      y = XINT (val);
	      y_specified_p = TRUE;
	    }
	}
    }

  /* Now we've extracted the properties, apply them.
     Do not apply geometric properties during frame creation. This
     is excessive anyways, and this loses becuase WM_SIZE has not
     been sent yet, so frame width and height fields are not initialized
  */ 
  if (f->init_finished
      && (width_specified_p || height_specified_p || x_specified_p || y_specified_p))
    {
      Lisp_Object frame = Qnil;
      RECT rect;
      int pixel_width, pixel_height;
      XSETFRAME (frame, f);

      char_to_real_pixel_size (f, width, height, &pixel_width, &pixel_height);
      if (!width_specified_p)
	pixel_width = FRAME_PIXWIDTH (f);
      if (!height_specified_p)
	pixel_height = FRAME_PIXHEIGHT (f);

      GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect);
      if (!x_specified_p)
	x = rect.left;
      if (!y_specified_p)
	y = rect.top;

      rect.left = rect.top = 0;
      rect.right = pixel_width;
      rect.bottom = pixel_height;
      AdjustWindowRectEx (&rect,
			  GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
			  GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
			  GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));
      

      if (IsIconic (FRAME_MSWINDOWS_HANDLE(f)) || IsZoomed (FRAME_MSWINDOWS_HANDLE(f)))
	ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);

      SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL, 
		    x, y, rect.right - rect.left, rect.bottom - rect.top,
		    SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING
		    | ((width_specified_p || height_specified_p) ? 0 : SWP_NOSIZE)
		    | ((x_specified_p || y_specified_p) ? 0 : SWP_NOMOVE));
    }
}

static Lisp_Object
mswindows_get_frame_parent (struct frame *f)
{
  HWND hwnd = FRAME_MSWINDOWS_HANDLE(f);
  hwnd = GetParent (hwnd);
  if (hwnd)
    {
      Lisp_Object parent;
      VOID_TO_LISP (parent, GetWindowLong (hwnd, XWL_FRAMEOBJ));
      assert (FRAME_MSWINDOWS_P (XFRAME (parent)));
      return parent;
    }
  else
    return Qnil;
}

static void
mswindows_update_frame_external_traits (struct frame* frm, Lisp_Object name)
{
}

static int
mswindows_frame_size_fixed_p (struct frame *f)
{
  /* Frame size cannot change if the frame is maximized */
  return IsZoomed (FRAME_MSWINDOWS_HANDLE (f));
}

void
console_type_create_frame_mswindows (void)
{
  /* frame methods */
  CONSOLE_HAS_METHOD (mswindows, init_frame_1);
/*  CONSOLE_HAS_METHOD (mswindows, init_frame_2); */
  CONSOLE_HAS_METHOD (mswindows, init_frame_3);
  CONSOLE_HAS_METHOD (mswindows, after_init_frame);
  CONSOLE_HAS_METHOD (mswindows, mark_frame);
  CONSOLE_HAS_METHOD (mswindows, focus_on_frame);
  CONSOLE_HAS_METHOD (mswindows, delete_frame);
  CONSOLE_HAS_METHOD (mswindows, get_mouse_position);
  CONSOLE_HAS_METHOD (mswindows, set_mouse_position);
  CONSOLE_HAS_METHOD (mswindows, raise_frame);
  CONSOLE_HAS_METHOD (mswindows, lower_frame);
  CONSOLE_HAS_METHOD (mswindows, make_frame_visible);
  CONSOLE_HAS_METHOD (mswindows, make_frame_invisible);
  CONSOLE_HAS_METHOD (mswindows, iconify_frame);
  CONSOLE_HAS_METHOD (mswindows, set_frame_size);
  CONSOLE_HAS_METHOD (mswindows, set_frame_position);
  CONSOLE_HAS_METHOD (mswindows, frame_property);
  CONSOLE_HAS_METHOD (mswindows, internal_frame_property_p);
  CONSOLE_HAS_METHOD (mswindows, frame_properties);
  CONSOLE_HAS_METHOD (mswindows, set_frame_properties);
  CONSOLE_HAS_METHOD (mswindows, set_title_from_bufbyte);
/*  CONSOLE_HAS_METHOD (mswindows, set_icon_name_from_bufbyte); */
  CONSOLE_HAS_METHOD (mswindows, frame_visible_p);
  CONSOLE_HAS_METHOD (mswindows, frame_totally_visible_p);
  CONSOLE_HAS_METHOD (mswindows, frame_iconified_p);
  CONSOLE_HAS_METHOD (mswindows, set_frame_pointer); 
  CONSOLE_HAS_METHOD (mswindows, set_frame_icon); 
  CONSOLE_HAS_METHOD (mswindows, get_frame_parent);
  CONSOLE_HAS_METHOD (mswindows, update_frame_external_traits);
  CONSOLE_HAS_METHOD (mswindows, frame_size_fixed_p);
}

void
syms_of_frame_mswindows (void)
{
}

void
vars_of_frame_mswindows (void)
{
  /* Needn't staticpro -- see comment above.  */
  Vmswindows_frame_being_created = Qnil;

  DEFVAR_LISP ("default-mswindows-frame-plist", &Vdefault_mswindows_frame_plist /*
Plist of default frame-creation properties for mswindows frames.
These override what is specified in `default-frame-plist', but are
overridden by the arguments to the particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

  initially-unmapped		If non-nil, the frame will not be visible
				when it is created.  In this case, you
				need to call `make-frame-visible' to make
				the frame appear.
  popup				If non-nil, it should be a frame, and this
				frame will be created as a "popup" frame
				whose parent is the given frame.  This
				will make the window manager treat the
				frame as a dialog box, which may entail
				doing different things (e.g. not asking
				for positioning, and not iconifying
				separate from its parent).
  top				Y position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  left				X position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).

See also `default-frame-plist', which specifies properties which apply
to all frames, not just mswindows frames.
*/ );
  Vdefault_mswindows_frame_plist = Qnil;

  mswindows_console_methods->device_specific_frame_props =
    &Vdefault_mswindows_frame_plist;
}
