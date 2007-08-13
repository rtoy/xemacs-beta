/* Functions for the win32 window system.
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
   Rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "console-w32.h"
#include "event-w32.h"

#include "buffer.h"
#include "frame.h"
#include "events.h"

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_w32_frame_plist;
/* Lisp_Object Qname, Qheight, Qwidth, Qinitially_unmapped, Qpopup, Qtop, Qleft; */
Lisp_Object Qinitially_unmapped, Qpopup;

static void
w32_init_frame_1 (struct frame *f, Lisp_Object props)
{
  w32_request_type request = { f, &props };
  Lisp_Object device = FRAME_DEVICE (f);
  struct device *d = XDEVICE (device);
  Lisp_Object lisp_window_id, initially_unmapped;
  initially_unmapped = Fplist_get (props, Qinitially_unmapped, Qnil);

#if 0
  if (NILP (DEVICE_SELECTED_FRAME (d)) &&	/* first frame on the device */
      NILP (initially_unmapped))
    f->visible = 1;
#endif

  f->frame_data = xnew_and_zero (struct w32_frame);
  FRAME_W32_HANDLE(f) = (HWND)w32_make_request(WM_XEMACS_CREATEWINDOW,
					       0, &request);
  FRAME_W32_DC(f) = GetDC(FRAME_W32_HANDLE(f));
  SetTextAlign(FRAME_W32_DC(f), TA_BASELINE|TA_LEFT|TA_NOUPDATECP);

  /* XXX FIXME: This function should be made to do something */
  update_frame_face_values (f);
}

/* Called just before frame's properties are set */
static void
w32_init_frame_2 (struct frame *f, Lisp_Object props)
{
}

/* Called after frame's properties are set */
static void
w32_init_frame_3 (struct frame *f)
{
  /* Don't do this earlier or we get a WM_PAINT before the frame is ready*/
  ShowWindow(FRAME_W32_HANDLE(f), SW_SHOWNORMAL);
}

static void
w32_delete_frame (struct frame *f)
{
  if (f->frame_data)
    {
      ReleaseDC(FRAME_W32_HANDLE(f), FRAME_W32_DC(f));
      DestroyWindow(FRAME_W32_HANDLE(f));
    }
}

static void
w32_set_frame_size (struct frame *f, int cols, int rows)
{
}


static void
w32_set_frame_position (struct frame *f, int xoff, int yoff)
{
}

static void
w32_set_frame_properties (struct frame *f, Lisp_Object plist)
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

      GetWindowRect (FRAME_W32_HANDLE(f), &rect);
      if (!x_specified_p)
	x = rect.left;
      if (!y_specified_p)
	y = rect.top;
      /* XXX FIXME: Should do AdjustWindowRect here like in w32_handle_request */
      MoveWindow (FRAME_W32_HANDLE(f), x, y, pixel_width, pixel_height,
		  (width_specified_p || height_specified_p));
    }
}


void
console_type_create_frame_w32 (void)
{
  /* frame methods */
  CONSOLE_HAS_METHOD (w32, init_frame_1);
  CONSOLE_HAS_METHOD (w32, init_frame_2);
  CONSOLE_HAS_METHOD (w32, init_frame_3);
/*  CONSOLE_HAS_METHOD (w32, mark_frame); */
/*  CONSOLE_HAS_METHOD (w32, focus_on_frame); */
  CONSOLE_HAS_METHOD (w32, delete_frame);
/*  CONSOLE_HAS_METHOD (w32, get_mouse_position); */
/*  CONSOLE_HAS_METHOD (w32, set_mouse_position); */
/*  CONSOLE_HAS_METHOD (w32, raise_frame); */
/*  CONSOLE_HAS_METHOD (w32, lower_frame); */
/*  CONSOLE_HAS_METHOD (w32, make_frame_visible); */
/*  CONSOLE_HAS_METHOD (w32, make_frame_invisible); */
/*  CONSOLE_HAS_METHOD (w32, iconify_frame); */
  CONSOLE_HAS_METHOD (w32, set_frame_size);
  CONSOLE_HAS_METHOD (w32, set_frame_position);
/*  CONSOLE_HAS_METHOD (w32, frame_property); */
/*  CONSOLE_HAS_METHOD (w32, internal_frame_property_p); */
/*  CONSOLE_HAS_METHOD (w32, frame_properties); */
  CONSOLE_HAS_METHOD (w32, set_frame_properties);
/*  CONSOLE_HAS_METHOD (w32, set_title_from_bufbyte); */
/*  CONSOLE_HAS_METHOD (w32, set_icon_name_from_bufbyte); */
/*  CONSOLE_HAS_METHOD (w32, frame_visible_p); */
/*  CONSOLE_HAS_METHOD (w32, frame_totally_visible_p); */
/*  CONSOLE_HAS_METHOD (w32, frame_iconified_p); */
/*  CONSOLE_HAS_METHOD (w32, set_frame_pointer); */
/*  CONSOLE_HAS_METHOD (w32, set_frame_icon); */
/*  CONSOLE_HAS_METHOD (w32, get_frame_parent); */
}

void
syms_of_frame_w32 (void)
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
vars_of_frame_w32 (void)
{
  DEFVAR_LISP ("default-w32-frame-plist", &Vdefault_w32_frame_plist /*
Plist of default frame-creation properties for w32 frames.
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
to all frames, not just w32 frames.
*/ );
  Vdefault_w32_frame_plist = Qnil;

  w32_console_methods->device_specific_frame_props =
    &Vdefault_w32_frame_plist;
}
