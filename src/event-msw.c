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

/*
 * Two separate queues, for efficiency, one (_u_) for user events, and
 * another (_s_) for non-user ones. We always return events out of the
 * first one until it is empty and only then proceed with the second
 * one.
 */
static Lisp_Object mswindows_u_dispatch_event_queue, mswindows_u_dispatch_event_queue_tail;
static Lisp_Object mswindows_s_dispatch_event_queue, mswindows_s_dispatch_event_queue_tail;

/*
 * List of mswindows waitable handles.
 * Apart from the dispatch queue semaphore, all of these handles may be waited
 * on multiple times in emacs_mswindows_next_event before being processed and so
 * must be manual-reset events.
 */
static HANDLE mswindows_waitable[MAX_WAITABLE];

/* random emacs info associated with each of the wait handles */
static mswindows_waitable_info_type mswindows_waitable_info[MAX_WAITABLE];

/* Count of quit chars currently in the queue */
/* Incremented in WM_CHAR handler in msw-proc.c
   Decremented in mswindows_dequeue_dispatch_event() */
int mswindows_quit_chars_count = 0;

/* These are Lisp integers; see DEFVARS in this file for description. */
int mswindows_dynamic_frame_resize;
int mswindows_num_mouse_buttons;
int mswindows_button2_max_skew_x;
int mswindows_button2_max_skew_y;
int mswindows_button2_chord_time;

/* Number of wait handles */
static mswindows_waitable_count=0;

static int
mswindows_user_event_p (struct Lisp_Event* sevt)
{
  return (sevt->event_type == key_press_event
	  || sevt->event_type == button_press_event
	  || sevt->event_type == button_release_event
	  || sevt->event_type == pointer_motion_event);
}

/*
 * Add an emacs event to the proper dispatch queue
 */
void
mswindows_enqueue_dispatch_event (Lisp_Object event)
{
  int user_p = mswindows_user_event_p (XEVENT(event));
  enqueue_event (event,
		 user_p ? &mswindows_u_dispatch_event_queue : 
		 	&mswindows_s_dispatch_event_queue,
		 user_p ? &mswindows_u_dispatch_event_queue_tail :
		 	&mswindows_s_dispatch_event_queue_tail);

  /* This one does not go to window procedure, hence does not
     generate XM_BUMPQUEUE magic event! */
  PostMessage (NULL, XM_BUMPQUEUE, 0, 0);
}

/*
 * Remove and return the first emacs event on the dispatch queue.
 * Give a preference to user events over non-user ones.
 */
static Lisp_Object
mswindows_dequeue_dispatch_event ()
{
  Lisp_Object event;
  struct Lisp_Event* sevt;

  assert (!NILP(mswindows_u_dispatch_event_queue) ||
	  !NILP(mswindows_s_dispatch_event_queue));

  event = dequeue_event (
		 NILP(mswindows_u_dispatch_event_queue) ? 
			 &mswindows_s_dispatch_event_queue : 
			 &mswindows_u_dispatch_event_queue,
		 NILP(mswindows_u_dispatch_event_queue) ? 
			 &mswindows_s_dispatch_event_queue_tail :
			 &mswindows_u_dispatch_event_queue_tail);

  sevt = XEVENT(event);
  if (sevt->event_type == key_press_event
      && (sevt->event.key.modifiers & FAKE_MOD_QUIT))
    {
      sevt->event.key.modifiers &= ~FAKE_MOD_QUIT;
      --mswindows_quit_chars_count;
    }

  return event;
}

/*
 * Remove and return the first emacs event on the dispatch queue that matches
 * the supplied event
 * Timeout event matches if interval_id equals to that of the given event.
 * Keypress event matches if logical AND between modifiers bitmask of the
 * event in the queue and that of the given event is non-zero
 * For all other event types, this function asserts.
 */

Lisp_Object
mswindows_cancel_dispatch_event (struct Lisp_Event* match)
{
  Lisp_Object event;
  Lisp_Object previous_event=Qnil;
  int user_p = mswindows_user_event_p (match);
  Lisp_Object* head = user_p ? &mswindows_u_dispatch_event_queue : 
    			       &mswindows_s_dispatch_event_queue;
  Lisp_Object* tail = user_p ? &mswindows_u_dispatch_event_queue_tail : 
    			       &mswindows_s_dispatch_event_queue_tail;

  assert (match->event_type == timeout_event
	  || match->event_type == key_press_event);

  EVENT_CHAIN_LOOP (event, *head)
    {
      int found = 1;
      if (XEVENT_TYPE (event) != match->event_type)
	found = 0;
      if (found && match->event_type == timeout_event
	  && (XEVENT(event)->event.timeout.interval_id !=
	      match->event.timeout.interval_id))
	found = 0;
      if (found && match->event_type == key_press_event
	  && ((XEVENT(event)->event.key.modifiers &
	      match->event.key.modifiers) == 0))
	found = 0;

      if (found)
	{
	  if (NILP (previous_event))
	    dequeue_event (head, tail);
	  else
	    {
	      XSET_EVENT_NEXT (previous_event, XEVENT_NEXT (event));
	      if (EQ (*tail, event))
		*tail = previous_event;
	    }
	  
	  return event;
	}
      previous_event = event;
    }
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
    assert (0); /* kkm - should not get here */
    /* Can only have one waitable for the dispatch queue, and it's the first one */
    assert (mswindows_waitable_count++ == 0);
    waitable=0;
//    InitializeCriticalSection(&mswindows_dispatch_crit);
    assert (mswindows_waitable[0] = CreateSemaphore (NULL, 0, 0x7fffffff, NULL));
    return mswindows_waitable_info+0;

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

  default:
    assert(0);
  }

  CloseHandle(mswindows_waitable[waitable]);
  mswindows_waitable[waitable] = 0;
  mswindows_waitable_info[waitable].type = mswindows_waitable_type_none;
  if (waitable == mswindows_waitable_count-1)
    --mswindows_waitable_count;
}

/* 
 * Callback procedure for synchronous timer messages
 */
static void CALLBACK
mswindows_wm_timer_callback (HWND hwnd, UINT umsg, UINT id_timer, DWORD dwtime)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  struct Lisp_Event *event = XEVENT (emacs_event);

  KillTimer (NULL, id_timer);

  event->channel = Qnil;
  event->timestamp = dwtime;
  event->event_type = timeout_event;
  event->event.timeout.interval_id = id_timer;

  mswindows_enqueue_dispatch_event (emacs_event);
}

static void 
mswindows_drain_windows_queue ()
{
  MSG msg;
  while (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
    DispatchMessage (&msg);
}

/*
 * This drains the event queue and fills up two internal queues until
 * an event of a type specified by USER_P is retrieved.
 *
 * If user_p, then the function drains until the first user event, or
 * the first non-user event if there no user events. Otherwise, If
 * not user_p, it does not give preference to user events.
 *
 * If badly_p, then the function does not return until an event is
 * available.
 *
 * The code does not rely on MsgWaitForMultipleObjects preference for
 * messages over waitable handles.
 *
 * Used by emacs_mswindows_event_pending_p and emacs_mswindows_next_event
 */
static void
mswindows_need_event (int user_p, int badly_p)
{
  int active;

  /* Have to drain Windows message queue first, otherwise, we may miss
     quit char when called from quit_p */
  mswindows_drain_windows_queue ();

  while (NILP (mswindows_u_dispatch_event_queue) &&
	 (user_p || NILP (mswindows_s_dispatch_event_queue)))
  {
    /* If we already have an event, we've got someting to return - no wait! */
    if (!NILP (mswindows_u_dispatch_event_queue)
	|| !NILP (mswindows_s_dispatch_event_queue))
      badly_p = 0;
    
    /* Now try getting a message */
    active = MsgWaitForMultipleObjects (mswindows_waitable_count,
					mswindows_waitable,
					FALSE, badly_p ? INFINITE : 0,
					QS_ALLINPUT);

    /* This will assert if handle being waited for becomes abandoned.
       Not the case currently tho */
    assert ((!badly_p && active == WAIT_TIMEOUT) ||
	    (active >= WAIT_OBJECT_0 &&
	     active <= WAIT_OBJECT_0 + mswindows_waitable_count));
    
    if (active == WAIT_TIMEOUT)
      {
	/* No luck trying - just return what we've already got */
	return;
      }
    else if (active == WAIT_OBJECT_0 + mswindows_waitable_count)
      {
	/* Got your message, thanks */
	mswindows_drain_windows_queue ();
      }
    else
      {
	/* XXX FIXME: We should do some kind of round-robin scheme to ensure fairness */
	int waitable = active - WAIT_OBJECT_0;
	mswindows_waitable_info_type *info  = mswindows_waitable_info + waitable;

	switch (info->type)
	  {
	    /* XXX FIXME: Should enque subprocess event here so that it is not lost */
	  default:
	    assert(0);
	  }
      }
  } /* while */

  return;
}


/************************************************************************/
/*                            methods                                   */
/************************************************************************/

static int
emacs_mswindows_add_timeout (EMACS_TIME thyme)
{
  int milliseconds;
  EMACS_TIME current_time;
  EMACS_GET_TIME (current_time);
  EMACS_SUB_TIME (thyme, thyme, current_time);
  milliseconds = EMACS_SECS (thyme) * 1000 +
    (EMACS_USECS (thyme) + 500) / 1000;
  if (milliseconds < 1)
    milliseconds = 1;
  return SetTimer (NULL, 0, milliseconds, mswindows_wm_timer_callback);
}

static void
emacs_mswindows_remove_timeout (int id)
{
  struct Lisp_Event match_against;
  Lisp_Object emacs_event;

  KillTimer (NULL, id);

  /* If there is a dispatch event generated by this
     timeout in the queue, we have to remove it too. */
  match_against.event_type = timeout_event;
  match_against.event.timeout.interval_id = id;
  emacs_event = mswindows_cancel_dispatch_event (&match_against);
  if (!NILP (emacs_event))
    Fdeallocate_event(emacs_event);
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
  mswindows_need_event (user_p, 0);

  return (!NILP (mswindows_u_dispatch_event_queue)
	  || (!user_p && !NILP (mswindows_s_dispatch_event_queue)));
}

/*
 * Return the next event
 */
static void
emacs_mswindows_next_event (struct Lisp_Event *emacs_event)
{
  Lisp_Object event, event2;

  /* Give strong preference to user events */
  mswindows_need_event (1, 1);

  /* XXX Copied from event-Xt.c */
  event = mswindows_dequeue_dispatch_event (!NILP(mswindows_u_dispatch_event_queue));
  XSETEVENT (event2, emacs_event);
  Fcopy_event (event, event2);
  Fdeallocate_event (event);
}

/*
 * Handle a magic event off the dispatch queue.
 */
static void
emacs_mswindows_handle_magic_event (struct Lisp_Event *emacs_event)
{
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
      Lisp_Object frame = EVENT_CHANNEL (emacs_event);
      struct frame *f = XFRAME (frame);
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

  case XM_BUMPQUEUE:
    /* This is a nice event, when we're in need to queue *something* */
    break;

  case XM_MAPFRAME:
  case XM_UNMAPFRAME:
    {
      Lisp_Object frame = EVENT_CHANNEL (emacs_event);
      va_run_hook_with_args (EVENT_MSWINDOWS_MAGIC_TYPE(emacs_event) 
			      == XM_MAPFRAME ?
			      Qmap_frame_hook : Qunmap_frame_hook, 
			      1, frame);
    }
    break;
			    
      /* XXX What about Enter & Leave */
#if 0
      va_run_hook_with_args (in_p ? Qmouse_enter_frame_hook :
			     Qmouse_leave_frame_hook, 1, frame);
#endif

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
  mswindows_need_event (1, 0);

  if (mswindows_quit_chars_count > 0)
    {
      /* Yes there's a hidden one... Throw it away */
      struct Lisp_Event match_against;
      Lisp_Object emacs_event;

      match_against.event_type = key_press_event;
      match_against.event.key.modifiers = FAKE_MOD_QUIT;

      emacs_event = mswindows_cancel_dispatch_event (&match_against);
      assert (!NILP (emacs_event));

      Vquit_flag = (XEVENT(emacs_event)->event.key.modifiers & MOD_SHIFT
		    ? Qcritical : Qt);

      Fdeallocate_event(emacs_event);
      --mswindows_quit_chars_count;
    }
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
  mswindows_u_dispatch_event_queue = Qnil;
  staticpro (&mswindows_u_dispatch_event_queue);
  mswindows_u_dispatch_event_queue_tail = Qnil;

  mswindows_s_dispatch_event_queue = Qnil;
  staticpro (&mswindows_s_dispatch_event_queue);
  mswindows_s_dispatch_event_queue_tail = Qnil;

  mswindows_event_stream = xnew (struct event_stream);

  mswindows_event_stream->event_pending_p 	= emacs_mswindows_event_pending_p;
  mswindows_event_stream->next_event_cb		= emacs_mswindows_next_event;
  mswindows_event_stream->handle_magic_event_cb = emacs_mswindows_handle_magic_event;
  mswindows_event_stream->add_timeout_cb 	= emacs_mswindows_add_timeout;
  mswindows_event_stream->remove_timeout_cb 	= emacs_mswindows_remove_timeout;
  mswindows_event_stream->select_console_cb 	= emacs_mswindows_select_console;
  mswindows_event_stream->unselect_console_cb	= emacs_mswindows_unselect_console;
  mswindows_event_stream->select_process_cb 	= emacs_mswindows_select_process;
  mswindows_event_stream->unselect_process_cb	= emacs_mswindows_unselect_process;
  mswindows_event_stream->quit_p_cb		= emacs_mswindows_quit_p;

  DEFVAR_BOOL ("w32-dynamic-frame-resize", &mswindows_dynamic_frame_resize /*
*Controls redrawing frame contents during mouse-drag or keyboard resize
operation. When non-nil, the frame is redrawn while being resized. When
nil, frame is not redrawn, and exposed areas are filled with default
MDI application background color. Note that this option only has effect
if "Show window contents while dragging" is on in system Display/Plus!
settings.
Default is t on fast machines, nil on slow.
*/ );

/* The description copied verbatim from nt-emacs. (C) Geoff Voelker */
  DEFVAR_INT ("w32-mouse-button-tolerance", &mswindows_button2_chord_time /*
*Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left/right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.
If negative or zero, currently set system default is used instead.
*/ );

/* The description copied verbatim from nt-emacs. (C) Geoff Voelker */
  DEFVAR_INT ("w32-num-mouse-buttons", &mswindows_num_mouse_buttons /*
Number of physical mouse buttons.
*/ );

  DEFVAR_INT ("w32-mouse-button-max-skew-x", &mswindows_button2_max_skew_x /*
*Maximum horizontal distance in pixels between points in which left and
right button clicks occured for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable w32-mouse-button-tolerance.
If negative or zero, currently set system default is used instead.
*/ );

  DEFVAR_INT ("w32-mouse-button-max-skew-y", &mswindows_button2_max_skew_y /*
*Maximum vertical distance in pixels between points in which left and
right button clicks occured for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable w32-mouse-button-tolerance.
If negative or zero, currently set system default is used instead.
*/ );

  mswindows_button2_max_skew_x = 0;
  mswindows_button2_max_skew_y = 0;
  mswindows_button2_chord_time = 0;
}

void
syms_of_event_mswindows (void)
{
}

void
init_event_mswindows_late (void)
{
  event_stream = mswindows_event_stream;

  mswindows_dynamic_frame_resize = !GetSystemMetrics (SM_SLOWMACHINE);
  mswindows_num_mouse_buttons = GetSystemMetrics (SM_CMOUSEBUTTONS);
}
