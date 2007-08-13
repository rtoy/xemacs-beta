/* TTY frame functions.
   Copyright (C) 1995 Free Software Foundation, Inc.
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

/* Synched up with: Not in FSF. */

/* Written by Ben Wing. */

/* #### This file is just a stub.  It should be possible to have more
   than one frame on a tty, with only one frame being "active" (displayed)
   at a time. */

#include <config.h>
#include "lisp.h"

#include "console-tty.h"
#include "frame.h"

Lisp_Object Vdefault_tty_frame_plist;

static void
tty_init_frame_1 (struct frame *f, Lisp_Object props)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  struct console *c = XCONSOLE (DEVICE_CONSOLE (d));
  if (!NILP (DEVICE_FRAME_LIST (d)))
    error ("Only one frame allowed on TTY devices");

  f->name = build_string ("emacs");
  f->height = CONSOLE_TTY_DATA (c)->height;
  f->width = CONSOLE_TTY_DATA (c)->width;
  f->visible = 1;
#ifdef HAVE_SCROLLBARS
  f->scrollbar_on_left = 1;
  f->scrollbar_on_top = 0;
#endif
  SET_FRAME_CLEAR (f);
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
  return FRAME_VISIBLE_P(f);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_frame_tty (void)
{
  CONSOLE_HAS_METHOD (tty, init_frame_1);
  CONSOLE_HAS_METHOD (tty, after_init_frame);
  CONSOLE_HAS_METHOD (tty, make_frame_visible);
  CONSOLE_HAS_METHOD (tty, make_frame_invisible);
  CONSOLE_HAS_METHOD (tty, frame_visible_p);
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
