/* The  mswindows event_stream interface.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996 Ben Wing.
   Copyright (C) 1997 Jonathan Harris.

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

/* Authorship:

   Ultimately based on FSF.
   Rewritten by Ben Wing.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "device.h"
#include "console-msw.h"
#include "events.h"
#include "frame.h"
#include "process.h"

#include "sysproc.h"
#include "syswait.h"
#include "systime.h"

#include "event-msw.h"

static struct event_stream *mswindows_event_stream;
static Lisp_Object mswindows_dispatch_event_queue, mswindows_dispatch_event_queue_tail;
CRITICAL_SECTION mswindows_dispatch_crit;

/*
 * List of mswindows waitable handles.
 * Apart from the dispatch queue semaphore, all of these handles may be waited
 * on multiple times in emacs_mswindows_next_event before being processed and so
 * must be manual-reset events.
 */
static HANDLE mswindows_waitable[MAX_WAITABLE];

/* random emacs info associated with each of the wait handles */
static mswindows_waitable_info_type mswindows_waitable_info[MAX_WAITABLE];

/* Number of wait handles */
static mswindows_waitable_count=0;

/*
 * Add an emacs event to the dispatch queue and increment the semaphore
 */
void
mswindows_enqueue_dispatch_event (Lisp_Object event)
{
  enqueue_event (event, &mswindows_dispatch_event_queue,
		 &mswindows_dispatch_event_queue_tail);
  ReleaseSemaphore(mswindows_waitable[0], 1, NULL);
}

/*
 * Remove and return the first emacs event on the dispatch queue. Don't
 * decrement the queue's semaphore because it will be decremented by being
 * waited on.
 */
static Lisp_Object
mswindows_dequeue_dispatch_event (void)
{
  Lisp_Object event;
  event = dequeue_event (&mswindows_dispatch_event_queue,
			 &mswindows_dispatch_event_queue_tail);
  return event;
}

/*
 * Remove and return the first emacs event on the dispatch queue that matches
 * the supplied event and decrement the queue's semaphore.
 * Only supports timeout events.
 */
Lisp_Object
mswindows_cancel_dispatch_event (Lisp_Object match_event)
{
  Lisp_Object event;
  Lisp_Object previous_event=Qnil;
  struct Lisp_Event *match = XEVENT(match_event);

  assert (match->event_type == timeout_event);

  EVENT_CHAIN_LOOP (event, mswindows_dispatch_event_queue)
    if (XEVENT_TYPE (event) == match->event_type)
      {
	/* We only handle timeouts */
	if (XEVENT(event)->event.timeout.interval_id ==
	    match->event.timeout.interval_id)
	  {
	    if (NILP (previous_event))
	      dequeue_event (&mswindows_dispatch_event_queue,
			     &mswindows_dispatch_event_queue_tail);
	    else
	      {
		XSET_EVENT_NEXT (previous_event, XEVENT_NEXT (event));
		if (EQ (mswindows_dispatch_event_queue_tail, event))
		  mswindows_dispatch_event_queue_tail = previous_event;
	      }
 
	    /* Decrement the dispatch queue counter */
	    WaitForSingleObject(mswindows_waitable[0], INFINITE);
	    return event;
	  }
      }
    else
      previous_event = event;

  return Qnil;
}

/*
 * Find a free waitable slot
 */
static int
mswindows_find_free_waitable(void)
{
  int i;
  for (i=0; i<mswindows_waitable_count; i++)
    if (mswindows_waitable_info[i].type == mswindows_waitable_type_none)
      return i;
  assert (mswindows_waitable_count < MAX_WAITABLE);
  return mswindows_waitable_count++;
}

/*
 * Create a new waitable using the type and data passed in by the info structure
 * Returns a pointer to the info associated with the assigned waitable object
 */
mswindows_waitable_info_type *
mswindows_add_waitable(mswindows_waitable_info_type *info)
{
  int waitable;

  switch (info->type)
  {
  case mswindows_waitable_type_dispatch:
    /* Can only have one waitable for the dispatch queue, and it's the first one */
    assert (mswindows_waitable_count++ == 0);
    waitable=0;
    InitializeCriticalSection(&mswindows_dispatch_crit);
    assert (mswindows_waitable[0] = CreateSemaphore (NULL, 0, 0x7fffffff, NULL));
    return mswindows_waitable_info+0;

#if 0	/* Windows95 doesn't support WaitableTimers */
  case mswindows_waitable_type_timeout:
    {
      LARGE_INTEGER due;
      due.QuadPart = 10000 * (LONGLONG) info->data.timeout.milliseconds;
      waitable = mswindows_find_free_waitable();
      mswindows_waitable[waitable] = CreateWaitableTimer(NULL, TRUE, NULL);
      SetWaitableTimer(mswindows_waitable[waitable], &due, 0, NULL, NULL, FALSE);
      mswindows_waitable_info[waitable].data.timeout.id = waitable;
    }
    break;
#endif

  default:
    assert(0);
  }
  mswindows_waitable_info[waitable].type = info->type;
  return mswindows_waitable_info+waitable;
}

/*
 * Remove a waitable using the type and data passed in by the info structure.
 */
void
mswindows_remove_waitable(mswindows_waitable_info_type *info)
{
  int waitable;

  switch (info->type)
  {
#if 0
  case mswindows_waitable_type_timeout:
    waitable = info->data.timeout.id;
    CancelWaitableTimeout(mswindows_waitable[waitable]);
    break;
#endif

  default:
    assert(0);
  }

  CloseHandle(mswindows_waitable[waitable]);
  mswindows_waitable[waitable] = 0;
  mswindows_waitable_info[waitable].type = mswindows_waitable_type_none;
  if (waitable == mswindows_waitable_count-1)
    --mswindows_waitable_count;
}


/************************************************************************/
/*                            methods                                   */
/************************************************************************/

static int
emacs_mswindows_add_timeout (EMACS_TIME thyme)
{
  EMACS_TIME current_time;
  int milliseconds;
  int id;
  mswindows_request_type request;

  EMACS_GET_TIME (current_time);
  EMACS_SUB_TIME (thyme, thyme, current_time);
  milliseconds = EMACS_SECS (thyme) * 1000 + EMACS_USECS (thyme) / 1000;
  if (milliseconds < 1)
    milliseconds = 1;
  request.thing1 = (void *) milliseconds;
  id = mswindows_make_request(WM_XEMACS_SETTIMER, 0, &request);
  assert(id);	/* XXX */
  return id;
}

static void
emacs_mswindows_remove_timeout (int id)
{
  mswindows_request_type request = { (void *) id };
  mswindows_make_request(WM_XEMACS_KILLTIMER, 0, &request);
}

/* If `user_p' is false, then return whether there are any win32, timeout,
 * or subprocess events pending (that is, whether
 * emacs_mswindows_next_event() would return immediately without blocking).
 *
 * if `user_p' is true, then return whether there are any *user generated*
 * events available (that is, whether there are keyboard or mouse-click
 * events ready to be read).  This also implies that
 * emacs_mswindows_next_event() would not block.
 */
static int
emacs_mswindows_event_pending_p (int user_p)
{
  if (user_p)
    {
      /* Iterate over the dispatch queue looking for user-events */
      int found = 0;
      Lisp_Object event;

      EnterCriticalSection (&mswindows_dispatch_crit);
      EVENT_CHAIN_LOOP (event, mswindows_dispatch_event_queue)
	if (command_event_p (event))
	  found = 1;
      LeaveCriticalSection (&mswindows_dispatch_crit);
      return found;
    }
  else
    {
      /* Check for any kind of input, including the dispatch queue */
#if 0
      /* Want do do the following, but it's not clear whether this would
       * cause the waitables to become unsignalled */
      return (WaitForMultipleObjects (mswindows_waitable_count,
				      mswindows_waitable, FALSE, 0)
	      != WAIT_TIMEOUT);
#else
      return !NILP (mswindows_dispatch_event_queue);
#endif
    }
}

static struct console *
find_console_from_fd (int fd)
{
  return 0;
}

/*
 * Return the next event
 * We return windows events off the dispatch event queue in preference to other events
 */
static void
emacs_mswindows_next_event (struct Lisp_Event *emacs_event)
{
  DWORD active;
  active = WaitForMultipleObjects (mswindows_waitable_count, mswindows_waitable,
				   FALSE, INFINITE);
  assert(active >= WAIT_OBJECT_0 && active <= WAIT_OBJECT_0 + mswindows_waitable_count - 1);
  
  /* Windows events on the dispatch event queue */
  if (active == WAIT_OBJECT_0)
  {
    /* XXX Copied from event-Xt.c */
    Lisp_Object event, event2;

    EnterCriticalSection (&mswindows_dispatch_crit);
    XSETEVENT (event2, emacs_event);
    event = mswindows_dequeue_dispatch_event ();
    Fcopy_event (event, event2);
    Fdeallocate_event (event);
    LeaveCriticalSection (&mswindows_dispatch_crit);
  }
  else
  {
    /* XXX FIXME: We should do some kind of round-robin scheme to ensure fairness */
    int waitable = active - WAIT_OBJECT_0;
    mswindows_waitable_info_type *info  = mswindows_waitable_info + waitable;

    switch (info->type)
    {
    case mswindows_waitable_type_timeout:
      emacs_event->channel = Qnil;
      emacs_event->event_type = timeout_event;
      emacs_event->event.timeout.interval_id = info->data.timeout.id;
      mswindows_remove_waitable(info);
      break;

    default:
      assert(0);
    }
  }
}

/*
 * Handle a magic event off the dispatch queue.
 * XXX split into seperate functions for clarity.
 */
static void
emacs_mswindows_handle_magic_event (struct Lisp_Event *emacs_event)
{
  RECT *rect = &EVENT_MSWINDOWS_MAGIC_DATA(emacs_event);
  struct frame *f = XFRAME (EVENT_CHANNEL (emacs_event));
  Lisp_Object frame = Qnil;
  XSETFRAME (frame, f);
#if 0  
  stderr_out("magic %x, (%d,%d), (%d,%d)\n",
	     EVENT_MSWINDOWS_MAGIC_TYPE(emacs_event),
	     rect->left, rect->top, rect->right, rect->bottom);
#endif
  switch (EVENT_MSWINDOWS_MAGIC_TYPE(emacs_event))
  {
  case WM_SETFOCUS:
  case WM_KILLFOCUS:
    {
      int in_p = (EVENT_MSWINDOWS_MAGIC_TYPE(emacs_event) == WM_SETFOCUS);
      Lisp_Object conser;
      /* struct gcpro gcpro1; */

      /* Clear sticky modifiers here (if we had any) */

      conser = Fcons (frame, Fcons (FRAME_DEVICE (f), in_p ? Qt : Qnil));
      /* GCPRO1 (conser); XXX Not necessary? */
      emacs_handle_focus_change_preliminary (conser);
      /* Under X the stuff up to here is done in the X event handler.
	 I Don't know why */
      emacs_handle_focus_change_final (conser);
      /* UNGCPRO; */
    }
    break;

    /* XXX What about Enter & Leave */
#if 0
      va_run_hook_with_args (in_p ? Qmouse_enter_frame_hook :
			     Qmouse_leave_frame_hook, 1, frame);
    break;
#endif

  case WM_SIZE:
    if ((rect->left & rect->top & rect->right & rect->bottom) == -1)
      {
	/* Iconified */
        FRAME_VISIBLE_P (f) = 0;
        va_run_hook_with_args (Qunmap_frame_hook, 1, frame);
	Fframe_iconified_p (frame);
      }
    else
      {
	/* If we're uniconified, our size may or may not have changed */
        int columns, rows;
	int was_visible = FRAME_VISIBLE_P (f);

	FRAME_VISIBLE_P (f) = 1;
        FRAME_PIXWIDTH(f) = rect->right;
	FRAME_PIXHEIGHT(f) = rect->bottom;

        pixel_to_char_size (f, rect->right, rect->bottom, &columns, &rows);
	change_frame_size (f, rows, columns, 0);
/*	      MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (f); /* XXX Too extreme? */

	if (!was_visible)
          va_run_hook_with_args (Qmap_frame_hook, 1, frame);

      }
      break;

  case WM_PAINT:
    mswindows_redraw_exposed_area(f, rect->left, rect->top,
				  rect->right, rect->bottom);
    break;

  case WM_CLOSE:
    enqueue_misc_user_event (frame, Qeval, list3 (Qdelete_frame, frame, Qt));
    break;

  default:
    assert(0);
  }
}

static void
emacs_mswindows_select_process (struct Lisp_Process *process)
{
}

static void
emacs_mswindows_unselect_process (struct Lisp_Process *process)
{
}

static void
emacs_mswindows_select_console (struct console *con)
{
}

static void
emacs_mswindows_unselect_console (struct console *con)
{
}

static void
emacs_mswindows_quit_p (void)
{
}

/* This is called from GC when a process object is about to be freed.
   If we've still got pointers to it in this file, we're gonna lose hard.
 */
void
debug_process_finalization (struct Lisp_Process *p)
{
#if 0 /* #### */
  int i;
  int infd, outfd;
  get_process_file_descriptors (p, &infd, &outfd);
  /* if it still has fds, then it hasn't been killed yet. */
  assert (infd < 0);
  assert (outfd < 0);
  /* Better not still be in the "with input" table; we know it's got no fds. */
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process = filedesc_fds_with_input [i];
      assert (!PROCESSP (process) || XPROCESS (process) != p);
    }
#endif
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/
 
void
vars_of_event_mswindows (void)
{
  mswindows_dispatch_event_queue = Qnil;
  staticpro (&mswindows_dispatch_event_queue);
  mswindows_dispatch_event_queue_tail = Qnil;

  mswindows_event_stream = xnew (struct event_stream);

  mswindows_event_stream->event_pending_p 	= emacs_mswindows_event_pending_p;
  mswindows_event_stream->next_event_cb	= emacs_mswindows_next_event;
  mswindows_event_stream->handle_magic_event_cb = emacs_mswindows_handle_magic_event;
  mswindows_event_stream->add_timeout_cb 	= emacs_mswindows_add_timeout;
  mswindows_event_stream->remove_timeout_cb 	= emacs_mswindows_remove_timeout;
  mswindows_event_stream->select_console_cb 	= emacs_mswindows_select_console;
  mswindows_event_stream->unselect_console_cb = emacs_mswindows_unselect_console;
  mswindows_event_stream->select_process_cb 	= emacs_mswindows_select_process;
  mswindows_event_stream->unselect_process_cb = emacs_mswindows_unselect_process;
  mswindows_event_stream->quit_p_cb		= emacs_mswindows_quit_p;
}

void
syms_of_event_mswindows (void)
{
}

void
init_event_mswindows_late (void)
{
  event_stream = mswindows_event_stream;
}
