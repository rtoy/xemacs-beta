/* TTY device functions.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2010 Ben Wing.

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

Lisp_Object Qmake_device_early_tty_entry_point;


#ifdef NEW_GC
static const struct memory_description tty_device_data_description_1 [] = {
  { XD_END }
};

DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("tty-device", tty_device,
				      0, tty_device_data_description_1,
				      Lisp_Tty_Device);
#endif /* NEW_GC */

static void
allocate_tty_device_struct (struct device *d)
{
#ifdef NEW_GC
  d->device_data = XTTY_DEVICE (ALLOC_NORMAL_LISP_OBJECT (tty_device));
#else /* not NEW_GC */
  d->device_data = xnew_and_zero (struct tty_device);
#endif /* not NEW_GC */
}

static void
tty_init_device (struct device *d, Lisp_Object UNUSED (props))
{
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
  Lisp_Object terminal_type = CONSOLE_TTY_DATA (con)->terminal_type;

  /* Run part of the elisp side of the TTY device initialization.
     The post-init is run in the tty_finish_init_device() method. */
  call0 (Qmake_device_early_tty_entry_point);

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
}

#ifndef NEW_GC
static void
free_tty_device_struct (struct device *d)
{
  if (d->device_data)
    {
      xfree (d->device_data);
      d->device_data = 0;
    }
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
	      change_frame_size (f, width, height, 1);
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
      return Fcons (make_fixnum (CONSOLE_TTY_DATA (con)->width),
		    make_fixnum (CONSOLE_TTY_DATA (con)->height));
    case DM_num_bit_planes:
      {
        EMACS_INT l2 = (EMACS_INT)  (log (CONSOLE_TTY_DATA (con)->colors)
                                     / log (2));
        return make_fixnum (l2);
      }
    case DM_num_color_cells:
      return make_fixnum (CONSOLE_TTY_DATA (con)->colors);
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
  INIT_LISP_OBJECT (tty_device);
#endif /* NEW_GC */

  DEFSYMBOL (Qmake_device_early_tty_entry_point);
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
