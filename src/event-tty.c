/* The event_stream interface for tty's.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 2002, 2003 Ben Wing.

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

#include <config.h>
#include "lisp.h"

#include "device.h"
#include "console-tty-impl.h"
#include "events.h"
#include "frame.h"
#include "process.h"

#include "sysproc.h"
#include "syswait.h"
#include "systime.h"

/* Mask of bits indicating the descriptors that we wait for input on */
extern SELECT_TYPE input_wait_mask, non_fake_input_wait_mask;
extern SELECT_TYPE process_only_mask, tty_only_mask;

static struct event_stream *tty_event_stream;

#ifdef WIN32_ANY
extern int mswindows_is_blocking;
#endif


/************************************************************************/
/*				timeout events				*/
/************************************************************************/

/* The pending timers are stored in an ordered list, where the first timer
   on the list is the first one to fire.  Times recorded here are
   absolute. */
static struct low_level_timeout *tty_timer_queue;

static int
emacs_tty_add_timeout (EMACS_TIME thyme)
{
  return add_low_level_timeout (&tty_timer_queue, thyme);
}

static void
emacs_tty_remove_timeout (int id)
{
  remove_low_level_timeout (&tty_timer_queue, id);
}

static void
tty_timeout_to_emacs_event (Lisp_Event *emacs_event)
{
  /* timeout events have nil as channel */
  SET_EVENT_TYPE (emacs_event, timeout_event);
  SET_EVENT_TIMESTAMP_ZERO (emacs_event); /* #### */
  SET_EVENT_TIMEOUT_INTERVAL_ID (emacs_event,
				 pop_low_level_timeout (&tty_timer_queue, 0));
  SET_EVENT_TIMEOUT_FUNCTION (emacs_event, Qnil);
  SET_EVENT_TIMEOUT_OBJECT (emacs_event, Qnil);
}



static int
emacs_tty_event_pending_p (int how_many)
{
  if (!how_many)
    {
      EMACS_TIME sometime;
      /* see if there's a pending timeout. */
      EMACS_GET_TIME (sometime);
      if (tty_timer_queue &&
	  EMACS_TIME_EQUAL_OR_GREATER (sometime, tty_timer_queue->time))
	return 1;

      return poll_fds_for_input (non_fake_input_wait_mask);
    }

  /* #### Not right!  We need to *count* the number of pending events, which
     means we need to have a dispatch queue and drain the pending events,
     using drain_tty_devices(). */
  return poll_fds_for_input (tty_only_mask);
}

static void
emacs_tty_next_event (Lisp_Event *emacs_event)
{
  while (1)
    {
      int ndesc;
      int i;
      SELECT_TYPE temp_mask = input_wait_mask;
      EMACS_TIME time_to_block;
      EMACS_SELECT_TIME select_time_to_block, *pointer_to_this;

      if (!get_low_level_timeout_interval (tty_timer_queue, &time_to_block))
	/* no timer events; block indefinitely */
 	pointer_to_this = 0;
      else
	{
	  EMACS_TIME_TO_SELECT_TIME (time_to_block, select_time_to_block);
	  pointer_to_this = &select_time_to_block;
	}

#ifdef WIN32_ANY
      mswindows_is_blocking = 1;
#endif
      ndesc = select (MAXDESC, &temp_mask, 0, 0, pointer_to_this);
#ifdef WIN32_ANY
      mswindows_is_blocking = 0;
#endif
      if (ndesc > 0)
	{
	  /* Look for a TTY event */
	  for (i = 0; i < MAXDESC; i++)
	    {
	      /* To avoid race conditions (among other things, an infinite
		 loop when called from Fdiscard_input()), we must return
		 user events ahead of process events. */
	      if (FD_ISSET (i, &temp_mask) && FD_ISSET (i, &tty_only_mask))
		{
		  struct console *c = find_tty_or_stream_console_from_fd (i);

		  assert (c);
		  if (read_event_from_tty_or_stream_desc (emacs_event, c))
		    return;
		}
	    }

	  /* Look for a process event */
	  for (i = 0; i < MAXDESC; i++)
	    {
	      if (FD_ISSET (i, &temp_mask) && FD_ISSET (i, &process_only_mask))
		{
		  Lisp_Object process;
		  Lisp_Process *p = get_process_from_usid (FD_TO_USID(i));

		  assert (p);
		  process = wrap_process (p);
		  set_event_type (emacs_event, process_event);
		  /* process events have nil as channel */
		  SET_EVENT_TIMESTAMP_ZERO (emacs_event); /* #### */
		  SET_EVENT_PROCESS_PROCESS (emacs_event, process);
		  return;
		}
	    }

	  /* We might get here when a fake event came through a signal. */
	  /* Return a dummy event, so that a cycle of the command loop will
	     occur. */
	  drain_signal_event_pipe ();
	  set_event_type (emacs_event, eval_event);
	  /* eval events have nil as channel */
	  SET_EVENT_EVAL_FUNCTION (emacs_event, Qidentity);
	  SET_EVENT_EVAL_OBJECT (emacs_event, Qnil);
	  return;
	}
      else if (ndesc == 0) /* timeout fired */
	{
	  tty_timeout_to_emacs_event (emacs_event);
	  return;
	}
    }
}

static void
emacs_tty_format_magic_event (Lisp_Event *UNUSED (emacs_event),
			      Lisp_Object UNUSED (pstream))
{
  /* Nothing to do currently */
}

static int
emacs_tty_compare_magic_event (Lisp_Event *UNUSED (e1),
			       Lisp_Event *UNUSED (e2))
{
  return 1;
}

static Hashcode
emacs_tty_hash_magic_event (Lisp_Event *UNUSED (e))
{
  return 0;
}

static void
emacs_tty_handle_magic_event (Lisp_Event *UNUSED (emacs_event))
{
  /* Nothing to do currently */
}


static void
emacs_tty_select_process (Lisp_Process *process, int doin, int doerr)
{
  int infd, errfd;

  event_stream_unixoid_select_process (process, doin, doerr, &infd, &errfd);
}

static void
emacs_tty_unselect_process (Lisp_Process *process, int doin, int doerr)
{
  int infd, errfd;

  event_stream_unixoid_unselect_process (process, doin, doerr, &infd, &errfd);
}

static void
emacs_tty_select_console (struct console *con)
{
  event_stream_unixoid_select_console (con);
}

static void
emacs_tty_unselect_console (struct console *con)
{
  event_stream_unixoid_unselect_console (con);
}

static void
emacs_tty_drain_queue (void)
{
  drain_tty_devices ();
}

static void
emacs_tty_create_io_streams (void* inhandle, void* outhandle,
			     void *errhandle, Lisp_Object* instream,
			     Lisp_Object* outstream,
			     Lisp_Object* errstream,
			     USID* in_usid,
			     USID* err_usid,
			     int flags)
{
  event_stream_unixoid_create_io_streams
    (inhandle, outhandle, errhandle, instream, outstream,
     errstream, in_usid, err_usid, flags);
}

static void
emacs_tty_delete_io_streams (Lisp_Object instream,
			     Lisp_Object outstream,
			     Lisp_Object errstream,
			     USID* in_usid,
			     USID* err_usid)
{
  event_stream_unixoid_delete_io_streams
    (instream, outstream, errstream, in_usid, err_usid);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
reinit_vars_of_event_tty (void)
{
  tty_event_stream = xnew_and_zero (struct event_stream);

  tty_event_stream->event_pending_p 	= emacs_tty_event_pending_p;
  tty_event_stream->next_event_cb	= emacs_tty_next_event;
  tty_event_stream->handle_magic_event_cb = emacs_tty_handle_magic_event;
  tty_event_stream->format_magic_event_cb = emacs_tty_format_magic_event;
  tty_event_stream->compare_magic_event_cb= emacs_tty_compare_magic_event;
  tty_event_stream->hash_magic_event_cb   = emacs_tty_hash_magic_event;
  tty_event_stream->add_timeout_cb 	= emacs_tty_add_timeout;
  tty_event_stream->remove_timeout_cb 	= emacs_tty_remove_timeout;
  tty_event_stream->select_console_cb 	= emacs_tty_select_console;
  tty_event_stream->unselect_console_cb = emacs_tty_unselect_console;
  tty_event_stream->select_process_cb 	= emacs_tty_select_process;
  tty_event_stream->unselect_process_cb = emacs_tty_unselect_process;
  tty_event_stream->drain_queue_cb	= emacs_tty_drain_queue;
  tty_event_stream->create_io_streams_cb = emacs_tty_create_io_streams;
  tty_event_stream->delete_io_streams_cb = emacs_tty_delete_io_streams;
}

void
vars_of_event_tty (void)
{
  reinit_vars_of_event_tty ();
}

void
init_event_tty_late (void)
{
  event_stream = tty_event_stream;
}
