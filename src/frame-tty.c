/* TTY frame functions.
   Copyright (C) 1995  Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1997  Free Software Foundation, Inc.

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

/* Written by Ben Wing.
   Multi-frame support added by Hrvoje Niksic. */

#include <config.h>
#include "lisp.h"

#include "console-tty.h"
#include "frame.h"


/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_tty_frame_plist;

/* The count of frame number. */
static int tty_frame_count;

static void tty_make_frame_visible (struct frame *);
static void tty_make_frame_invisible (struct frame *);


static void
tty_init_frame_1 (struct frame *f, Lisp_Object props)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  struct console *c = XCONSOLE (DEVICE_CONSOLE (d));

  ++CONSOLE_TTY_DATA (c)->frame_count;
  f->order_count = CONSOLE_TTY_DATA (c)->frame_count;
  f->height = CONSOLE_TTY_DATA (c)->height;
  f->width = CONSOLE_TTY_DATA (c)->width;
#ifdef HAVE_SCROLLBARS
  f->scrollbar_on_left = 1;
  f->scrollbar_on_top = 0;
#endif
}

static void
tty_init_frame_3 (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  Lisp_Object tail = DEVICE_FRAME_LIST (d);

  while (CONSP (tail))
    {
      tty_make_frame_invisible (decode_frame (XCAR (tail)));
      tail = XCDR (tail);
    }
  select_frame_2 (make_frame (f));
  SET_FRAME_CLEAR (f);
  tty_make_frame_visible (f);
}

static void
tty_after_init_frame (struct frame *f, int first_on_device,
		      int first_on_console)
{
  if (first_on_console)
    call1 (Qinit_post_tty_win, FRAME_CONSOLE (f));
}

/* Change from withdrawn state to mapped state. */
static void
tty_make_frame_visible (struct frame *f)
{
  if (!FRAME_VISIBLE_P(f))
    {
      SET_FRAME_CLEAR(f);
      f->visible = 1;
    }
}

/* Change from mapped state to withdrawn state. */
static void
tty_make_frame_invisible (struct frame *f)
{
  f->visible = 0;
}

static int
tty_frame_visible_p (struct frame *f)
{
  return FRAME_VISIBLE_P (f);
}

/* Raise the frame.  This means that it becomes visible, and all the
   others become invisible.  */
static void
tty_raise_frame (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  Lisp_Object frame_list = DEVICE_FRAME_LIST (d);
  Lisp_Object tail = frame_list;

  while (CONSP (tail))
    {
      if (decode_frame (XCAR (tail)) != f)
	tty_make_frame_invisible (XFRAME (XCAR (tail)));
      tail = XCDR (tail);
    }
  select_frame_2 (make_frame (f));
  tty_make_frame_visible (f);
}

/* Lower the frame.  This means that it becomes invisible, while the
   one after it in the frame list becomes visible.  */
static void
tty_lower_frame (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  Lisp_Object frame_list = DEVICE_FRAME_LIST (d);
  Lisp_Object tail;
  Lisp_Object new;

  if (!FRAME_VISIBLE_P (f))
    return;

  tail = frame_list;
  while (CONSP (tail))
    {
      if (decode_frame (XCAR (tail)) == f)
	break;
      tail = XCDR (tail);
    }
  if (!CONSP (tail))
    {
      error ("Cannot find frame to lower");
    }

  tty_make_frame_invisible (f);
  if (CONSP (XCDR (tail)))
    new = XCAR (XCDR (tail));
  else
    new = XCAR (frame_list);
  tty_make_frame_visible (XFRAME (new));
  select_frame_2 (new);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_frame_tty (void)
{
  CONSOLE_HAS_METHOD (tty, init_frame_1);
  CONSOLE_HAS_METHOD (tty, init_frame_3);
  CONSOLE_HAS_METHOD (tty, after_init_frame);
  CONSOLE_HAS_METHOD (tty, make_frame_visible);
  CONSOLE_HAS_METHOD (tty, make_frame_invisible);
  CONSOLE_HAS_METHOD (tty, frame_visible_p);
  CONSOLE_HAS_METHOD (tty, raise_frame);
  CONSOLE_HAS_METHOD (tty, lower_frame);
}

void
vars_of_frame_tty (void)
{
  DEFVAR_LISP ("default-tty-frame-plist", &Vdefault_tty_frame_plist /*
Plist of default frame-creation properties for tty frames.
These are in addition to and override what is specified in
`default-frame-plist', but are overridden by the arguments to the
particular call to `make-frame'.
*/ );
  Vdefault_tty_frame_plist = Qnil;

  tty_console_methods->device_specific_frame_props =
    &Vdefault_tty_frame_plist;
}
