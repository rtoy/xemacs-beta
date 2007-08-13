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
   Rewritten for mswindows by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "event-msw.h"

#include "buffer.h"
#include "frame.h"
#include "events.h"

#define MSWINDOWS_FRAME_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_OVERLAPPEDWINDOW
#define MSWINDOWS_POPUP_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_CAPTION|WS_POPUP

#define MSWINDOWS_FRAME_EXSTYLE WS_EX_OVERLAPPEDWINDOW
#define MSWINDOWS_POPUP_EXSTYLE WS_EX_OVERLAPPEDWINDOW

#ifdef HAVE_MENUBARS
#define ADJR_MENUFLAG TRUE
#else
#define ADJR_MENUFLAG FALSE
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_mswindows_frame_plist;
/* Lisp_Object Qname, Qheight, Qwidth, Qinitially_unmapped, Qpopup, Qtop, Qleft; */
Lisp_Object Qinitially_unmapped, Qpopup;

static void
mswindows_init_frame_1 (struct frame *f, Lisp_Object props)
{
  Lisp_Object device = FRAME_DEVICE (f);
  struct device *d = XDEVICE (device);
  Lisp_Object lisp_window_id, initially_unmapped;
  Lisp_Object name, height, width, popup, top, left;
  Lisp_Object frame_obj;
  int pixel_width, pixel_height;
  RECT rect;
  DWORD style, exstyle;
  HWND hwnd;

  initially_unmapped = Fplist_get (props, Qinitially_unmapped, Qnil);
  name = Fplist_get (props, Qname, Qnil);
  height = Fplist_get (props, Qheight, Qnil);
  width = Fplist_get (props, Qwidth, Qnil);
  popup = Fplist_get (props, Qpopup, Qnil);
  top = Fplist_get (props, Qtop, Qnil);
  left = Fplist_get (props, Qleft, Qnil);

  /* These shouldn't be here, but the window is created too early.
     The initialization of scrollbar resources is done between
     init_frame_1 and init_frame_2 in make_frame.  jsparkes */
  f->scrollbar_width  = make_int (15);
  f->scrollbar_height = make_int (15);

  f->frame_data = xnew_and_zero (struct mswindows_frame);
  FRAME_WIDTH (f) = INTP(width) ? XINT(width) : 80;
  FRAME_HEIGHT (f) = INTP(height) ? XINT(height) : 30;
  char_to_pixel_size (f, FRAME_WIDTH(f), FRAME_HEIGHT (f),
		      &FRAME_PIXWIDTH (f), &FRAME_PIXHEIGHT (f));

  style = (NILP(popup)) ? MSWINDOWS_FRAME_STYLE : MSWINDOWS_POPUP_STYLE;
  exstyle = (NILP(popup)) ? MSWINDOWS_FRAME_EXSTYLE : MSWINDOWS_POPUP_EXSTYLE;
  rect.left = rect.top = 0;
  rect.right = FRAME_PIXWIDTH (f);
  rect.bottom = FRAME_PIXHEIGHT (f);

  FRAME_MSWINDOWS_DATA(f)->button2_need_lbutton = 0;
  FRAME_MSWINDOWS_DATA(f)->button2_need_rbutton = 0;
  FRAME_MSWINDOWS_DATA(f)->button2_is_down = 0;
  FRAME_MSWINDOWS_DATA(f)->ignore_next_lbutton_up = 0;
  FRAME_MSWINDOWS_DATA(f)->ignore_next_rbutton_up = 0;
  FRAME_MSWINDOWS_DATA(f)->sizing = 0;

  AdjustWindowRectEx(&rect, style, ADJR_MENUFLAG, exstyle);

  FRAME_MSWINDOWS_HANDLE(f) =
    CreateWindowEx (exstyle,
		    XEMACS_CLASS,
		    STRINGP(f->name) ? XSTRING_DATA(f->name) :
		    	(STRINGP(name) ? XSTRING_DATA(name) : XEMACS_CLASS),
		    style,
		    INTP(left) ? XINT(left) : CW_USEDEFAULT,
		    INTP(top) ? XINT(top) : CW_USEDEFAULT,
		    rect.right-rect.left, rect.bottom-rect.top,
		    NULL, NULL, NULL, NULL);
  XSETFRAME (frame_obj, f);
  SetWindowLong (FRAME_MSWINDOWS_HANDLE(f), XWL_FRAMEOBJ, (LONG)frame_obj);
  FRAME_MSWINDOWS_DC(f) = GetDC(FRAME_MSWINDOWS_HANDLE(f));
  SetTextAlign(FRAME_MSWINDOWS_DC(f), TA_BASELINE|TA_LEFT|TA_NOUPDATECP);
}

/* Called just before frame's properties are set, size is 10x10 or something */
static void
mswindows_init_frame_2 (struct frame *f, Lisp_Object props)
{
}

/* Called after frame's properties are set */
static void
mswindows_init_frame_3 (struct frame *f)
{
  /* Don't do this earlier or we get a WM_PAINT before the frame is ready*/
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOWNORMAL);
  SetForegroundWindow (FRAME_MSWINDOWS_HANDLE(f));
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
      ReleaseDC(FRAME_MSWINDOWS_HANDLE(f), FRAME_MSWINDOWS_DC(f));
      DestroyWindow(FRAME_MSWINDOWS_HANDLE(f));
    }
}

static void
mswindows_set_frame_size (struct frame *f, int cols, int rows)
{
  RECT rect1, rect2;
  
  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect1);
  rect2.left = rect2.top = 0;
  char_to_pixel_size (f, cols, rows, &rect2.right, &rect2.bottom);

  AdjustWindowRectEx (&rect2,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
		      GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));
		    
  MoveWindow (FRAME_MSWINDOWS_HANDLE(f), rect1.left, rect1.top,
 	      rect2.right-rect2.left, rect2.bottom-rect2.top, TRUE);
}

static void
mswindows_set_frame_position (struct frame *f, int xoff, int yoff)
{
  RECT rect;

  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect);
  MoveWindow (FRAME_MSWINDOWS_HANDLE(f), xoff, yoff,
	      rect.right-rect.left, rect.bottom-rect.top, TRUE);
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
mswindows_raise_frame (struct frame *f)
{
  BringWindowToTop (FRAME_MSWINDOWS_HANDLE(f));
  /* XXX Should we do SetWindowForeground too ? */
}

static void
mswindows_lower_frame (struct frame *f)
{
  RECT rect;
  
  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect);
  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), HWND_BOTTOM, rect.top, rect.left,
		rect.right-rect.left, rect.bottom-rect.top, 0);
}

static void
mswindows_set_title_from_bufbyte (struct frame *f, Bufbyte *title) 
{
  SetWindowText (FRAME_MSWINDOWS_HANDLE(f), title);
}

static void
mswindows_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  int x, y;
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

  /* Now we've extracted the properties, apply them */
  if (width_specified_p || height_specified_p || x_specified_p || y_specified_p)
    {
      Lisp_Object frame;
      RECT rect;
      int pixel_width, pixel_height;
      XSETFRAME (frame, f);

      if (!width_specified_p)
	width = FRAME_WIDTH (f);
      if (!height_specified_p)
	height = FRAME_HEIGHT (f);
      char_to_pixel_size (f, width, height, &pixel_width, &pixel_height);

      GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect);
      if (!x_specified_p)
	x = rect.left;
      if (!y_specified_p)
	y = rect.top;

      AdjustWindowRectEx (&rect,
			  GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
			  GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
			  GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));

      MoveWindow (FRAME_MSWINDOWS_HANDLE(f), x, y, pixel_width, pixel_height,
		  (width_specified_p || height_specified_p));
    }
}


void
console_type_create_frame_mswindows (void)
{
  /* frame methods */
  CONSOLE_HAS_METHOD (mswindows, init_frame_1);
  CONSOLE_HAS_METHOD (mswindows, init_frame_2);
  CONSOLE_HAS_METHOD (mswindows, init_frame_3);
/*  CONSOLE_HAS_METHOD (mswindows, mark_frame); */
  CONSOLE_HAS_METHOD (mswindows, focus_on_frame);
  CONSOLE_HAS_METHOD (mswindows, delete_frame);
/*  CONSOLE_HAS_METHOD (mswindows, get_mouse_position); */
/*  CONSOLE_HAS_METHOD (mswindows, set_mouse_position); */
  CONSOLE_HAS_METHOD (mswindows, raise_frame);
  CONSOLE_HAS_METHOD (mswindows, lower_frame);
  CONSOLE_HAS_METHOD (mswindows, make_frame_visible);
  CONSOLE_HAS_METHOD (mswindows, make_frame_invisible);
  CONSOLE_HAS_METHOD (mswindows, iconify_frame);
  CONSOLE_HAS_METHOD (mswindows, set_frame_size);
  CONSOLE_HAS_METHOD (mswindows, set_frame_position);
/*  CONSOLE_HAS_METHOD (mswindows, frame_property); */
/*  CONSOLE_HAS_METHOD (mswindows, internal_frame_property_p); */
/*  CONSOLE_HAS_METHOD (mswindows, frame_properties); */
  CONSOLE_HAS_METHOD (mswindows, set_frame_properties);
  CONSOLE_HAS_METHOD (mswindows, set_title_from_bufbyte);
/*  CONSOLE_HAS_METHOD (mswindows, set_icon_name_from_bufbyte); */
  CONSOLE_HAS_METHOD (mswindows, frame_visible_p);
/*  CONSOLE_HAS_METHOD (mswindows, frame_totally_visible_p); */
  CONSOLE_HAS_METHOD (mswindows, frame_iconified_p);
/*  CONSOLE_HAS_METHOD (mswindows, set_frame_pointer); */
/*  CONSOLE_HAS_METHOD (mswindows, set_frame_icon); */
/*  CONSOLE_HAS_METHOD (mswindows, get_frame_parent); */
}

void
syms_of_frame_mswindows (void)
{
#if 0	/* XXX these are in general.c */
  defsymbol (&Qname, "name");
  defsymbol (&Qheight, "height");
  defsymbol (&Qwidth, "width");
  defsymbol (&Qtop, "top");
  defsymbol (&Qleft, "left");
#endif
  defsymbol (&Qinitially_unmapped, "initially-unmapped");
  defsymbol (&Qpopup, "popup");
}

void
vars_of_frame_mswindows (void)
{
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
