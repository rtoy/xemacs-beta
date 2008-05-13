/* TTY device functions.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

/* Authors: Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "device-impl.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "lstream.h"
#include "redisplay.h"
#include "sysdep.h"

#include "console-tty-impl.h"
#include "console-stream.h"

#include "sysfile.h"
#include "syssignal.h" /* for SIGWINCH */

Lisp_Object Qinit_pre_tty_win, Qinit_post_tty_win;


#ifdef NEW_GC
static const struct memory_description tty_device_data_description_1 [] = {
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("tty-device", tty_device,
			       1, /*dumpable-flag*/
                               0, 0, 0, 0, 0,
			       tty_device_data_description_1,
			       Lisp_Tty_Device);
#endif /* NEW_GC */

static void
allocate_tty_device_struct (struct device *d)
{
#ifdef NEW_GC
  d->device_data = alloc_lrecord_type (struct tty_device, &lrecord_tty_device);
#else /* not NEW_GC */
  d->device_data = xnew_and_zero (struct tty_device);
#endif /* not NEW_GC */
}

static void
tty_init_device (struct device *d, Lisp_Object UNUSED (props))
{
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
  Lisp_Object terminal_type = CONSOLE_TTY_DATA (con)->terminal_type;

  DEVICE_INFD (d) = CONSOLE_TTY_DATA (con)->infd;
  DEVICE_OUTFD (d) = CONSOLE_TTY_DATA (con)->outfd;

  allocate_tty_device_struct (d);
  init_baud_rate (d);

  switch (init_tty_for_redisplay (d, (char *) XSTRING_DATA (terminal_type)))
    {
#if 0
    case TTY_UNABLE_OPEN_DATABASE:
      suppress_early_error_handler_backtrace = 1;
      signal_error (Qio_error, "Can't access terminal information database", Qunbound);
      break;
#endif
    case TTY_TYPE_UNDEFINED:
      suppress_early_error_handler_backtrace = 1;
      signal_error (Qio_error, "Terminal type undefined (or can't access database?)",
	     terminal_type);
      break;
    case TTY_TYPE_INSUFFICIENT:
      suppress_early_error_handler_backtrace = 1;
      signal_error (Qio_error, "Terminal type not powerful enough to run Emacs",
	     terminal_type);
      break;
    case TTY_SIZE_UNSPECIFIED:
      suppress_early_error_handler_backtrace = 1;
      signal_error (Qio_error, "Can't determine window size of terminal", Qunbound);
      break;
    case TTY_INIT_SUCCESS:
      break;
    default:
      ABORT ();
    }

  init_one_device (d);

  /* Run part of the elisp side of the TTY device initialization.
     The post-init is run in the tty_after_init_frame() method. */
  call0 (Qinit_pre_tty_win);
}

#ifndef NEW_GC
static void
free_tty_device_struct (struct device *d)
{
  if (d->device_data)
    xfree (d->device_data, void *);
}

static void
tty_delete_device (struct device *d)
{
  free_tty_device_struct (d);
}
#endif /* not NEW_GC */

#ifdef SIGWINCH

static SIGTYPE
tty_device_size_change_signal (int UNUSED (signo))
{
  int old_errno = errno;
  asynch_device_change_pending++;
#ifdef HAVE_UNIXOID_EVENT_LOOP
  signal_fake_event ();
#endif
  EMACS_REESTABLISH_SIGNAL (SIGWINCH, tty_device_size_change_signal);
  errno = old_errno;
  SIGRETURN;
}

/* frame_change_signal does nothing but set a flag that it was called.
   When redisplay is called, it will notice that the flag is set and
   call handle_pending_device_size_change to do the actual work. */
static void
tty_asynch_device_change (void)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      int width, height;
      Lisp_Object tail;
      struct device *d = XDEVICE (XCAR (devcons));
      struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

      if (!DEVICE_TTY_P (d))
	continue;

      get_tty_device_size (d, &width, &height);
      if (width > 0 && height > 0
	  && (CONSOLE_TTY_DATA (con)->width != width
	      || CONSOLE_TTY_DATA (con)->height != height))
	{
	  CONSOLE_TTY_DATA (con)->width = width;
	  CONSOLE_TTY_DATA (con)->height = height;

	  for (tail = DEVICE_FRAME_LIST (d);
	       !NILP (tail);
	       tail = XCDR (tail))
	    {
	      struct frame *f = XFRAME (XCAR (tail));

	      /* We know the frame is tty because we made sure that the
		 device is tty. */
	      change_frame_size (f, height, width, 1);
	    }
	}
    }
}

#endif /* SIGWINCH */

static Lisp_Object
tty_device_system_metrics (struct device *d,
			   enum device_metrics m)
{
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
  switch (m)
    {
    case DM_size_device:
      return Fcons (make_int (CONSOLE_TTY_DATA (con)->width),
		    make_int (CONSOLE_TTY_DATA (con)->height));
    default: /* No such device metric property for TTY devices */
      return Qunbound;
    }
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_tty (void)
{
#ifdef NEW_GC
  INIT_LRECORD_IMPLEMENTATION (tty_device);
#endif /* NEW_GC */

  DEFSYMBOL (Qinit_pre_tty_win);
  DEFSYMBOL (Qinit_post_tty_win);
}

void
console_type_create_device_tty (void)
{
  /* device methods */
  CONSOLE_HAS_METHOD (tty, init_device);
#ifndef NEW_GC
  CONSOLE_HAS_METHOD (tty, delete_device);
#endif /* not NEW_GC */
#ifdef SIGWINCH
  CONSOLE_HAS_METHOD (tty, asynch_device_change);
#endif /* SIGWINCH */
  CONSOLE_HAS_METHOD (tty, device_system_metrics);
}

void
init_device_tty (void)
{
#ifdef SIGWINCH
  if (initialized && !noninteractive)
    EMACS_SIGNAL (SIGWINCH, tty_device_size_change_signal);
#endif /* SIGWINCH */
}
