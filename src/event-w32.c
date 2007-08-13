/* The event_stream interface win32.
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
   Rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "device.h"
#include "console-w32.h"
#include "events.h"
#include "frame.h"
#include "process.h"

#include "sysproc.h"
#include "syswait.h"
#include "systime.h"

#include "event-w32.h"

static struct event_stream *w32_event_stream;
static Lisp_Object w32_dispatch_event_queue, w32_dispatch_event_queue_tail;
static w32_waitable_count=0;
CRITICAL_SECTION w32_dispatch_crit;

static Lisp_Object w32_dequeue_dispatch_event (void);

/*
 * List of win32 waitable handles.
 * Apart from the dispatch queue semaphore, all of these handles may be waited
 * on multiple times in emacs_w32_next_event before being processed and so must
 * be manual-reset events.
 */
static HANDLE w32_waitable[MAX_WAITABLE];

/* random emacs info associated with each of the wait handles */
static w32_waitable_info_type w32_waitable_info[MAX_WAITABLE];

void
w32_enqueue_dispatch_event (Lisp_Object event)
{
  assert(w32_waitable_count);
//  EnterCriticalSection (&w32_dispatch_crit);
  enqueue_event (event, &w32_dispatch_event_queue, &w32_dispatch_event_queue_tail);
  ReleaseSemaphore(w32_waitable[0], 1, NULL);
//  LeaveCriticalSection (&w32_dispatch_crit);
}

static Lisp_Object
w32_dequeue_dispatch_event (void)
{
  Lisp_Object event;
  assert(w32_waitable_count);
//  EnterCriticalSection (&w32_dispatch_crit);
  event = dequeue_event (&w32_dispatch_event_queue, &w32_dispatch_event_queue_tail);
//  LeaveCriticalSection (&w32_dispatch_crit);
  return event;
}

/*
 * Find a free waitable slot
 */
static int
w32_find_free_waitable(void)
{
  int i;
  for (i=0; i<w32_waitable_count; i++)
    if (w32_waitable_info[i].type == w32_waitable_type_none)
      return i;
  assert (w32_waitable_count < MAX_WAITABLE);
  return w32_waitable_count++;
}

/*
 * Create a new waitable using the type and data passed in by the info structure
 * Returns a pointer to the info associated with the assigned waitable object
 */
w32_waitable_info_type *
w32_add_waitable(w32_waitable_info_type *info)
{
  int waitable;

  switch (info->type)
  {
  case w32_waitable_type_dispatch:
    /* Can only have one waitable for the dispatch queue, and it's the first one */
    assert (w32_waitable_count++ == 0);
    waitable=0;
    InitializeCriticalSection(&w32_dispatch_crit);
    assert (w32_waitable[0] = CreateSemaphore (NULL, 0, 0x7fffffff, NULL));
    return w32_waitable_info+0;

#if 0	/* Windows95 doesn't support WaitableTimers */
  case w32_waitable_type_timeout:
    {
      LARGE_INTEGER due;
      due.QuadPart = 10000 * (LONGLONG) info->data.timeout.milliseconds;
      waitable = w32_find_free_waitable();
      w32_waitable[waitable] = CreateWaitableTimer(NULL, TRUE, NULL);
      SetWaitableTimer(w32_waitable[waitable], &due, 0, NULL, NULL, FALSE);
      w32_waitable_info[waitable].data.timeout.id = waitable;
    }
    break;
#endif

  default:
    assert(0);
  }
  w32_waitable_info[waitable].type = info->type;
  return w32_waitable_info+waitable;
}

/*
 * Remove a waitable using the type and data passed in by the info structure.
 */
void
w32_remove_waitable(w32_waitable_info_type *info)
{
  int waitable;

  switch (info->type)
  {
#if 0
  case w32_waitable_type_timeout:
    waitable = info->data.timeout.id;
    CancelWaitableTimeout(w32_waitable[waitable]);
    break;
#endif

  default:
    assert(0);
  }

  CloseHandle(w32_waitable[waitable]);
  w32_waitable[waitable] = 0;
  w32_waitable_info[waitable].type = w32_waitable_type_none;
  if (waitable == w32_waitable_count-1)
    --w32_waitable_count;
}


/************************************************************************/
/*                            methods                                   */
/************************************************************************/

static int
emacs_w32_add_timeout (EMACS_TIME thyme)
{
  EMACS_TIME current_time;
  int milliseconds;
  int id;
  w32_request_type request;

  EMACS_GET_TIME (current_time);
  EMACS_SUB_TIME (thyme, thyme, current_time);
  milliseconds = EMACS_SECS (thyme) * 1000 + EMACS_USECS (thyme) / 1000;
  if (milliseconds < 1)
    milliseconds = 1;
  request.thing1 = (void *) milliseconds;
  id = w32_make_request(WM_XEMACS_SETTIMER, 0, &request);
  assert(id);	/* XXX */
  return id;
}

static void
emacs_w32_remove_timeout (int id)
{
  w32_request_type request = { (void *) id };
  w32_make_request(WM_XEMACS_KILLTIMER, 0, &request);
}

static int
emacs_w32_event_pending_p (int user_p)
{
  return 0;
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
emacs_w32_next_event (struct Lisp_Event *emacs_event)
{
  DWORD active;
  active = WaitForMultipleObjects (w32_waitable_count, w32_waitable,
				   FALSE, INFINITE);
  assert(active >= WAIT_OBJECT_0 && active <= WAIT_OBJECT_0 + w32_waitable_count - 1);
  
  /* Windows events on the dispatch event queue */
  if (active == WAIT_OBJECT_0)
  {
    /* XXX Copied from event-Xt.c */
    Lisp_Object event, event2;

    EnterCriticalSection (&w32_dispatch_crit);
    XSETEVENT (event2, emacs_event);
    event = w32_dequeue_dispatch_event ();
    Fcopy_event (event, event2);
    Fdeallocate_event (event);
    LeaveCriticalSection (&w32_dispatch_crit);
  }
  else
  {
    /* XXX FIXME: We should do some kind of round-robin scheme to ensure fairness */
    int waitable = active - WAIT_OBJECT_0;
    w32_waitable_info_type *info  = w32_waitable_info + waitable;

    switch (info->type)
    {
    case w32_waitable_type_timeout:
      emacs_event->channel = Qnil;
      emacs_event->event_type = timeout_event;
      emacs_event->event.timeout.interval_id = info->data.timeout.id;
      w32_remove_waitable(info);
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
emacs_w32_handle_magic_event (struct Lisp_Event *emacs_event)
{
  RECT *rect = &EVENT_W32_MAGIC_DATA(emacs_event);
  struct frame *f = XFRAME (EVENT_CHANNEL (emacs_event));
  Lisp_Object frame = Qnil;
  XSETFRAME (frame, f);
#if 0  
  stderr_out("magic %x, (%d,%d), (%d,%d)\n",
	     EVENT_W32_MAGIC_TYPE(emacs_event),
	     rect->left, rect->top, rect->right, rect->bottom);
#endif
  switch (EVENT_W32_MAGIC_TYPE(emacs_event))
  {
  case WM_SETFOCUS:
  case WM_KILLFOCUS:
    {
      int in_p = (EVENT_W32_MAGIC_TYPE(emacs_event) == WM_SETFOCUS);
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
        pixel_to_char_size (f, rect->right, rect->bottom, &columns, &rows);

	FRAME_VISIBLE_P (f) = 1;
	if (f->height!=rows || f->width!=columns || f->size_change_pending)
	  {
	    /* Size changed */
	    f->pixwidth = rect->right;
	    f->pixheight = rect->bottom;
	    change_frame_size (f, rows, columns, 0);
/*	      MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (f); /* XXX Too extreme? */
	  }

	if (!was_visible)
          va_run_hook_with_args (Qmap_frame_hook, 1, frame);

      }
      break;

  case WM_PAINT:
    w32_redraw_exposed_area(f, rect->left, rect->top,
			    rect->right, rect->bottom);
    break;

  default:
    assert(0);
  }
}

static void
emacs_w32_select_process (struct Lisp_Process *process)
{
}

static void
emacs_w32_unselect_process (struct Lisp_Process *process)
{
}

static void
emacs_w32_select_console (struct console *con)
{
}

static void
emacs_w32_unselect_console (struct console *con)
{
}

static void
emacs_w32_quit_p (void)
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
vars_of_event_w32 (void)
{
  w32_dispatch_event_queue = Qnil;
  staticpro (&w32_dispatch_event_queue);
  w32_dispatch_event_queue_tail = Qnil;

  w32_event_stream = xnew (struct event_stream);

  w32_event_stream->event_pending_p 	= emacs_w32_event_pending_p;
  w32_event_stream->next_event_cb	= emacs_w32_next_event;
  w32_event_stream->handle_magic_event_cb = emacs_w32_handle_magic_event;
  w32_event_stream->add_timeout_cb 	= emacs_w32_add_timeout;
  w32_event_stream->remove_timeout_cb 	= emacs_w32_remove_timeout;
  w32_event_stream->select_console_cb 	= emacs_w32_select_console;
  w32_event_stream->unselect_console_cb = emacs_w32_unselect_console;
  w32_event_stream->select_process_cb 	= emacs_w32_select_process;
  w32_event_stream->unselect_process_cb = emacs_w32_unselect_process;
  w32_event_stream->quit_p_cb		= emacs_w32_quit_p;
}

void
syms_of_event_w32 (void)
{
}

void
init_event_w32_late (void)
{
  event_stream = w32_event_stream;
}
