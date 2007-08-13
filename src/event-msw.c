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
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
 */

#include <config.h>
#include "lisp.h"

#include "console-msw.h"

#ifdef HAVE_SCROLLBARS
# include "scrollbar-msw.h"
#endif

#ifdef HAVE_MENUBARS
# include "menubar-msw.h"
#endif

#include "device.h"
#include "events.h"
#include "frame.h"
#include "process.h"
#include "redisplay.h"
#include "sysproc.h"
#include "syswait.h"
#include "systime.h"

#include "events-mod.h"

#ifdef BROKEN_CYGWIN
int WINAPI      DdeCmpStringHandles (HSZ, HSZ);
HDDEDATA WINAPI DdeCreateDataHandle (DWORD, LPBYTE, DWORD, DWORD, HSZ,
				     UINT, UINT);
#endif

#ifdef HAVE_MENUBARS
#define ADJR_MENUFLAG TRUE
#else
#define ADJR_MENUFLAG FALSE
#endif

/* Fake key modifier which is attached to a quit char event.
   Removed upon dequeueing an event */
#define FAKE_MOD_QUIT	0x80

/* Timer ID used for button2 emulation */
#define BUTTON_2_TIMER_ID 1

/* Drag and drop event data types (subset of types in offix-types.h) */
#define DndFile		2
#define	DndFiles	3
#define	DndText		4


static Lisp_Object mswindows_find_frame (HWND hwnd);
static Lisp_Object mswindows_find_console (HWND hwnd);
static Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key, int mods);
static int mswindows_modifier_state (BYTE* keymap, int has_AltGr);
static void mswindows_set_chord_timer (HWND hwnd);
static int mswindows_button2_near_enough (POINTS p1, POINTS p2);
static int mswindows_current_layout_has_AltGr (void);

static struct event_stream *mswindows_event_stream;

/*
 * Two separate queues, for efficiency, one (_u_) for user events, and
 * another (_s_) for non-user ones. We always return events out of the
 * first one until it is empty and only then proceed with the second
 * one.
 */
static Lisp_Object mswindows_u_dispatch_event_queue, mswindows_u_dispatch_event_queue_tail;
static Lisp_Object mswindows_s_dispatch_event_queue, mswindows_s_dispatch_event_queue_tail;

/* The number of things we can wait on */
#define MAX_WAITABLE (MAXIMUM_WAIT_OBJECTS - 1)

/* List of mswindows waitable handles. */
static HANDLE mswindows_waitable[MAX_WAITABLE];

/* Count of quit chars currently in the queue */
/* Incremented in WM_[SYS]KEYDOWN handler in the mswindows_wnd_proc()
   Decremented in mswindows_dequeue_dispatch_event() */
int mswindows_quit_chars_count = 0;

/* These are Lisp integers; see DEFVARS in this file for description. */
int mswindows_dynamic_frame_resize;
int mswindows_num_mouse_buttons;
int mswindows_mouse_button_max_skew_x;
int mswindows_mouse_button_max_skew_y;
int mswindows_mouse_button_tolerance;

/* Number of wait handles */
static int mswindows_waitable_count=0;

/* This is the event signaled by the event pump.
   See mswindows_pump_outstanding_events for comments */
static Lisp_Object mswindows_error_caught_in_modal_loop;
static int mswindows_in_modal_loop;

/* Count of wound timers */
static int mswindows_pending_timers_count;

static int
mswindows_user_event_p (struct Lisp_Event* sevt)
{
  return (sevt->event_type == key_press_event
	  || sevt->event_type == button_press_event
	  || sevt->event_type == button_release_event
	  || sevt->event_type == dnd_drop_event);
}

/************************************************************************/
/*                     Dispatch queue management                        */
/************************************************************************/

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

void
mswindows_enqueue_magic_event (HWND hwnd, UINT message)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  struct Lisp_Event* event = XEVENT (emacs_event);

  event->channel = mswindows_find_frame (hwnd);
  event->timestamp = GetMessageTime();
  event->event_type = magic_event;
  EVENT_MSWINDOWS_MAGIC_TYPE (event) = message;

  mswindows_enqueue_dispatch_event (emacs_event);
}

static void
mswindows_enqueue_mouse_button_event (HWND hwnd, UINT message, POINTS where, DWORD when)
{

  /* We always use last message time, because mouse button
     events may get delayed, and XEmacs double click
     recognition will fail */

  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  struct Lisp_Event* event = XEVENT(emacs_event);

  event->channel = mswindows_find_frame(hwnd);
  event->timestamp = when;
  event->event.button.button =
    (message==WM_LBUTTONDOWN || message==WM_LBUTTONUP) ? 1 :
    ((message==WM_RBUTTONDOWN || message==WM_RBUTTONUP) ? 3 : 2);
  event->event.button.x = where.x;
  event->event.button.y = where.y;
  event->event.button.modifiers = mswindows_modifier_state (NULL, 0);
      
  if (message==WM_LBUTTONDOWN || message==WM_MBUTTONDOWN ||
      message==WM_RBUTTONDOWN)
    {
      event->event_type = button_press_event;
      SetCapture (hwnd);
    }
  else
    {
      event->event_type = button_release_event;
      ReleaseCapture ();
    }
  
  mswindows_enqueue_dispatch_event (emacs_event);
}

static void
mswindows_enqueue_keypress_event (HWND hwnd, Lisp_Object keysym, int mods)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  struct Lisp_Event* event = XEVENT(emacs_event);

  event->channel = mswindows_find_console(hwnd);
  event->timestamp = GetMessageTime();
  event->event_type = key_press_event;
  event->event.key.keysym = keysym;
  event->event.key.modifiers = mods;
  mswindows_enqueue_dispatch_event (emacs_event);
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
  return Qnil;
}


/************************************************************************/
/*                             Event pump                               */
/************************************************************************/

static Lisp_Object
mswindows_modal_loop_error_handler (Lisp_Object cons_sig_data,
				    Lisp_Object u_n_u_s_e_d)
{
  mswindows_error_caught_in_modal_loop = cons_sig_data;
  return Qunbound;
}

Lisp_Object
mswindows_protect_modal_loop (Lisp_Object (*bfun) (Lisp_Object barg),
			      Lisp_Object barg)
{
  Lisp_Object tmp;

  ++mswindows_in_modal_loop; 
  tmp = condition_case_1 (Qt,
			  bfun, barg,
			  mswindows_modal_loop_error_handler, Qnil);
  --mswindows_in_modal_loop;

  return tmp;
}

void
mswindows_unmodalize_signal_maybe (void)
{
  if (!NILP (mswindows_error_caught_in_modal_loop))
    {
      /* Got an error while messages were pumped while
	 in window procedure - have to resignal */
      Lisp_Object sym = XCAR (mswindows_error_caught_in_modal_loop);
      Lisp_Object data = XCDR (mswindows_error_caught_in_modal_loop);
      mswindows_error_caught_in_modal_loop = Qnil;
      Fsignal (sym, data);
    }
}

/*
 * This is an unsafe part of event pump, guarded by 
 * condition_case. See mswindows_pump_outstanding_events
 */
static Lisp_Object
mswindows_unsafe_pump_events (Lisp_Object u_n_u_s_e_d)
{
  /* This function can call lisp */
  Lisp_Object event = Fmake_event (Qnil, Qnil);
  struct gcpro gcpro1;
  int do_redisplay = 0;
  GCPRO1 (event);

  while (detect_input_pending ())
    {
      Fnext_event (event, Qnil);
      Fdispatch_event (event);
      do_redisplay = 1;
    }

  if (do_redisplay)
    redisplay ();

  Fdeallocate_event (event);
  UNGCPRO;
  
  /* Qt becomes return value of mswindows_pump_outstanding_events
     once we get here */
  return Qt;
}

/*
 * This function pumps emacs events, while available, by using
 * next_message/dispatch_message loop. Errors are trapped around
 * the loop so the function always returns.
 *
 * Windows message queue is not looked into during the call,
 * neither are waitable handles checked. The function pumps
 * thus only dispatch events already queued, as well as those
 * resulted in dispatching thereof. This is done by setting
 * module local variable mswidows_in_modal_loop to nonzero.
 *
 * Return value is Qt if no errors was trapped, or Qunbound if
 * there was an error.
 *
 * In case of error, a cons representing the error, in the
 * form (SIGNAL . DATA), is stored in the module local variable
 * mswindows_error_caught_in_modal_loop. This error is signaled
 * again when DispatchMessage returns. Thus, Windows internal
 * modal loops are protected against throws, which are proven
 * to corrupt internal Windows structures.
 *
 * In case of success, mswindows_error_caught_in_modal_loop is
 * assigned Qnil.
 *
 * If the value of mswindows_error_caught_in_modal_loop is not
 * nil already upon entry, the function just returns non-nil.
 * This situation means that a new event has been queued while
 * cancleng mode. The event will be dequeued on the next regular
 * call of next-event; the pump is off since error is caught.
 * The caller must *unconditionally* cancel modal loop if the
 * value returned by this function is nil. Otherwise, everything
 * will become frozen until the modal loop exits under normal
 * condition (scrollbar drag is released, menu closed etc.)
 */
Lisp_Object
mswindows_pump_outstanding_events (void)
{
  /* This function can call lisp */

  Lisp_Object result = Qt;
  struct gcpro gcpro1;
  GCPRO1 (result);
  
  if (NILP(mswindows_error_caught_in_modal_loop))
      result = mswindows_protect_modal_loop (mswindows_unsafe_pump_events, Qnil);
  UNGCPRO;
  return result;
}



static void 
mswindows_drain_windows_queue ()
{
  MSG msg;
  while (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
    {
      DispatchMessage (&msg);
      mswindows_unmodalize_signal_maybe ();
    }
}

/* 
 * This is a special flavour of the mswindows_need_event function,
 * used while in event pump. Actually, there is only kind of events
 * allowed while in event pump: a timer.  An attempt to fetch any
 * other event leads to a dealock, as there's no source of user input
 * ('cause event pump mirrors windows modal loop, which is a sole
 * owner of thread message queue).
 *
 * To detect this, we use a counter of active timers, and allow
 * fetching WM_TIMER messages. Instead of trying to fetch a WM_TIMER
 * which will never come when there are no pending timers, which leads
 * to deadlock, we simply signal an error.
 *
 * The implementation does not honor user_p by design.
 */
static void
mswindows_need_event_in_modal_loop (int user_p, int badly_p)
{
  MSG msg;

  /* Check if already have one */
  if (!NILP (mswindows_u_dispatch_event_queue)
      || !NILP (mswindows_s_dispatch_event_queue))
    return;

  /* No event is ok */
  if (!badly_p)
    return;

  /* We do not check the _u_ queue, because timers go to _s_ */
  while (NILP (mswindows_s_dispatch_event_queue))
    {
      /* We'll deadlock if go waiting */
      if (mswindows_pending_timers_count == 0)
	error ("Deadlock due to an attempt to call next-event in a wrong context");
      
      /* Fetch and dispatch any pending timers */
      GetMessage (&msg, NULL, WM_TIMER, WM_TIMER);
      DispatchMessage (&msg);
    }
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

  if (mswindows_in_modal_loop)
    {
      mswindows_need_event_in_modal_loop (user_p, badly_p);
      return;
    }

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
	assert(0);	/* #### */
      }
  } /* while */

  return;
}

/************************************************************************/
/*                           Event generators                           */
/************************************************************************/

/* 
 * Callback procedure for synchronous timer messages
 */
static void CALLBACK
mswindows_wm_timer_callback (HWND hwnd, UINT umsg, UINT id_timer, DWORD dwtime)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  struct Lisp_Event *event = XEVENT (emacs_event);

  if (KillTimer (NULL, id_timer))
    --mswindows_pending_timers_count;

  event->channel = Qnil;
  event->timestamp = dwtime;
  event->event_type = timeout_event;
  event->event.timeout.interval_id = id_timer;

  mswindows_enqueue_dispatch_event (emacs_event);
}

/* 
 * Callback procedure for dde messages
 */
HDDEDATA CALLBACK
mswindows_dde_callback (UINT uType, UINT uFmt, HCONV hconv,
			HSZ hszTopic, HSZ hszItem, HDDEDATA hdata,
			DWORD dwData1, DWORD dwData2)
{ 
  switch (uType)
    { 
    case XTYP_CONNECT:
      if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system))
	return (HDDEDATA)TRUE;
      return (HDDEDATA)FALSE;

    case XTYP_WILDCONNECT:
      {
	/* We only support one {service,topic} pair */
	HSZPAIR pairs[2] = {
	  { mswindows_dde_service, mswindows_dde_topic_system }, { 0, 0 } };

	if (!(hszItem  || DdeCmpStringHandles (hszItem, mswindows_dde_service)) &&
	    !(hszTopic || DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system)));
	  return (DdeCreateDataHandle (mswindows_dde_mlid, (LPBYTE)pairs,
				       sizeof (pairs), 0L, 0, uFmt, 0));
      }
      return (HDDEDATA)NULL; 

    case XTYP_EXECUTE:
      if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system))
	{
	  DWORD len = DdeGetData (hdata, NULL, 0, 0);
	  char *cmd = alloca (len+1);
#ifdef __CYGWIN32__
	  char *cmd_1;
#endif
	  char *end;
          Lisp_Object l_dndlist;
	  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
	  struct Lisp_Event *event = XEVENT (emacs_event);

	  DdeGetData (hdata, cmd, len, 0);
	  cmd[len] = '\0';
	  DdeFreeDataHandle (hdata);

	  /* Check syntax & that it's an [Open("foo")] command */
	  /* #### Ought to be generalised and accept some other commands */
	  if (*cmd == '[')
	    cmd++;
	  if (strnicmp (cmd, MSWINDOWS_DDE_ITEM_OPEN,
			strlen (MSWINDOWS_DDE_ITEM_OPEN)))
	    return DDE_FNOTPROCESSED;
	  cmd += strlen (MSWINDOWS_DDE_ITEM_OPEN);
	  while (*cmd==' ')
	    cmd++;
	  if (*cmd!='(' || *(cmd+1)!='\"')
	    return DDE_FNOTPROCESSED;
	  end = (cmd+=2);
	  while (*end && *end!='\"')
	    end++;
	  if (!*end)
	    return DDE_FNOTPROCESSED;
	  *end = '\0';
	  if (*(++end)!=')')
	    return DDE_FNOTPROCESSED;
	  if (*(++end)==']')
	    end++;
	  if (*end)
	    return DDE_FNOTPROCESSED;
#ifdef __CYGWIN32__
	  CYGWIN_CONV_PATH(cmd,cmd_1);
	  cmd = cmd_1;
#endif
	  l_dndlist = make_ext_string (cmd, strlen(cmd), FORMAT_FILENAME);

	  event->channel = Qnil;
	  event->timestamp = GetTickCount();
	  event->event_type = dnd_drop_event;
	  event->event.dnd_drop.button = 0;
	  event->event.dnd_drop.modifiers = 0;
	  event->event.dnd_drop.x = -1;
	  event->event.dnd_drop.y = -1;
	  event->event.dnd_drop.data = Fcons (make_int (DndFile),
					      Fcons (l_dndlist, Qnil));
	  mswindows_enqueue_dispatch_event (emacs_event);

	  return (HDDEDATA) DDE_FACK;
	}
      DdeFreeDataHandle (hdata); 
      return (HDDEDATA) DDE_FNOTPROCESSED;
    default: 
      return (HDDEDATA) NULL; 
    } 

}

/*
 * The windows procedure for the window class XEMACS_CLASS
 */
LRESULT WINAPI
mswindows_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  /* Note: Remember to initialise emacs_event and event before use.
     This code calls code that can GC. You must GCPRO before calling such code. */
  Lisp_Object emacs_event = Qnil;
  Lisp_Object fobj = Qnil;

  struct Lisp_Event *event;
  struct frame *frame;
  struct mswindows_frame* msframe;

  switch (message)
  {
  case WM_ERASEBKGND:
    /* Erase background only during non-dynamic sizing */
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    if (msframe->sizing && !mswindows_dynamic_frame_resize)
      goto defproc;
    return 1;

  case WM_CLOSE:
    fobj = mswindows_find_frame (hwnd);
    enqueue_misc_user_event (fobj, Qeval, list3 (Qdelete_frame, fobj, Qt));
    mswindows_enqueue_magic_event (hwnd, XM_BUMPQUEUE);
    break;

  case WM_KEYDOWN:
  case WM_SYSKEYDOWN:
    {
      BYTE keymap[256];
      int has_AltGr = mswindows_current_layout_has_AltGr ();
      int mods;
      Lisp_Object keysym;

      GetKeyboardState (keymap);
      mods = mswindows_modifier_state (keymap, has_AltGr);

      /* Handle those keys that TranslateMessage won't generate a WM_CHAR for */
      if (!NILP (keysym = mswindows_key_to_emacs_keysym(wParam, mods)))
	mswindows_enqueue_keypress_event (hwnd, keysym, mods);
      else
	{
	  int quit_ch = CONSOLE_QUIT_CHAR (XCONSOLE (mswindows_find_console (hwnd)));
	  BYTE keymap_orig[256];
	  MSG msg = { hwnd, message, wParam, lParam, GetMessageTime(), (GetMessagePos()) };
	  memcpy (keymap_orig, keymap, 256);

	  /* Clear control and alt modifiers out of the keymap */
	  keymap [VK_RCONTROL] = 0;
	  keymap [VK_LMENU] = 0;
	  if (!has_AltGr || !(keymap [VK_LCONTROL] & 0x80) || !(keymap [VK_RMENU] & 0x80))
	    {
	      keymap [VK_LCONTROL] = 0;
	      keymap [VK_CONTROL] = 0;
	      keymap [VK_RMENU] = 0;
	      keymap [VK_MENU] = 0;
	    }
	  SetKeyboardState (keymap);

	  /* Have some WM_[SYS]CHARS in the queue */
	  TranslateMessage (&msg);

	  while (PeekMessage (&msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE)
		 ||PeekMessage (&msg, hwnd, WM_SYSCHAR, WM_SYSCHAR, PM_REMOVE))
	    {
	      int ch = msg.wParam;
	      /* CH is a character code for the key: 
		 'C' for Shift+C and Ctrl+Shift+C
		 'c' for c and Ctrl+c */

	      /* #### If locale is not C, US or other latin-1,
		 isalpha() maybe not what do we mean */
	      
	      /* XEmacs doesn't seem to like Shift on non-alpha keys */
	      if (!isalpha(ch))
		mods &= ~MOD_SHIFT;

	      /* Un-capitalise alpha control keys */
	      if ((mods & MOD_CONTROL) && isalpha(ch))
		ch |= ('A' ^ 'a');

	      /* If a quit char with no modifiers other than control and
		 shift, then mark it with a fake modifier, which is removed
		 upon dequeueing the event */
	      /* #### This might also not withstand localization, if
		 quit character is not a latin-1 symbol */
	      if (((quit_ch < ' ' && (mods & MOD_CONTROL) && quit_ch + 'a' - 1 == ch)
		   || (quit_ch >= ' ' && !(mods & MOD_CONTROL) && quit_ch == ch))
		  && ((mods  & ~(MOD_CONTROL | MOD_SHIFT)) == 0))
		{
		  mods |= FAKE_MOD_QUIT;
		  ++mswindows_quit_chars_count;
		}

	      mswindows_enqueue_keypress_event (hwnd, make_char(ch), mods);
	    } /* while */
	  SetKeyboardState (keymap_orig);
	} /* else */
    }
    goto defproc;

  case WM_MBUTTONDOWN:
  case WM_MBUTTONUP:
    /* Real middle mouse button has nothing to do with emulated one:
       if one wants to exercise fingers playing chords on the mouse,
       he is allowed to do that! */
    mswindows_enqueue_mouse_button_event (hwnd, message,
					  MAKEPOINTS (lParam), GetMessageTime());
    break;
    
  case WM_LBUTTONUP:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    msframe->last_click_time =  GetMessageTime();

    KillTimer (hwnd, BUTTON_2_TIMER_ID);
    msframe->button2_need_lbutton = 0;
    if (msframe->ignore_next_lbutton_up)
      {
	msframe->ignore_next_lbutton_up = 0;
      }
    else if (msframe->button2_is_down)
      {
	msframe->button2_is_down = 0;
	msframe->ignore_next_rbutton_up = 1;
	mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONUP,
					      MAKEPOINTS (lParam), GetMessageTime());
      }
    else
      {
	if (msframe->button2_need_rbutton)
	  {
	    msframe->button2_need_rbutton = 0;
	    mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	  }
	mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONUP,
					      MAKEPOINTS (lParam), GetMessageTime());
      }
    break;

  case WM_RBUTTONUP:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    msframe->last_click_time =  GetMessageTime();

    KillTimer (hwnd, BUTTON_2_TIMER_ID);
    msframe->button2_need_rbutton = 0;
    if (msframe->ignore_next_rbutton_up)
      {
	msframe->ignore_next_rbutton_up = 0;
      }
    else if (msframe->button2_is_down)
      {
	msframe->button2_is_down = 0;
	msframe->ignore_next_lbutton_up = 1;
	mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONUP,
					      MAKEPOINTS (lParam), GetMessageTime());
      }
    else
      {
	if (msframe->button2_need_lbutton)
	  {
	    msframe->button2_need_lbutton = 0;
	    mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	  }
	mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONUP,
					      MAKEPOINTS (lParam), GetMessageTime());
      }
    break;

  case WM_LBUTTONDOWN:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));

    if (msframe->button2_need_lbutton)
      {
	KillTimer (hwnd, BUTTON_2_TIMER_ID);
	msframe->button2_need_lbutton = 0;
	msframe->button2_need_rbutton = 0;
	if (mswindows_button2_near_enough (msframe->last_click_point, MAKEPOINTS (lParam)))
	  {
	    mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	    msframe->button2_is_down = 1;
	  }
	else
	  {
	    mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
			msframe->last_click_point, msframe->last_click_time);
	    mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	  }
      }
    else
      {
	mswindows_set_chord_timer (hwnd);
	msframe->button2_need_rbutton = 1;
	msframe->last_click_point = MAKEPOINTS (lParam);
      }
    msframe->last_click_time =  GetMessageTime();
    break;

  case WM_RBUTTONDOWN:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));

    if (msframe->button2_need_rbutton)
      {
	KillTimer (hwnd, BUTTON_2_TIMER_ID);
	msframe->button2_need_lbutton = 0;
	msframe->button2_need_rbutton = 0;
	if (mswindows_button2_near_enough (msframe->last_click_point, MAKEPOINTS (lParam)))
	  {
	    mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	    msframe->button2_is_down = 1;
	  }
	else
	  {
	    mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
				msframe->last_click_point, msframe->last_click_time);
	    mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						  MAKEPOINTS (lParam), GetMessageTime());
	  }
      }
    else
      {
	mswindows_set_chord_timer (hwnd);
	msframe->button2_need_lbutton = 1;
	msframe->last_click_point = MAKEPOINTS (lParam);
      }
    msframe->last_click_time =  GetMessageTime();
    break;
	
  case WM_TIMER:
    if (wParam == BUTTON_2_TIMER_ID)
      {
	msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
	KillTimer (hwnd, BUTTON_2_TIMER_ID);

	if (msframe->button2_need_lbutton)
	  {
	    msframe->button2_need_lbutton = 0;
	    mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
				msframe->last_click_point, msframe->last_click_time);
	  }
	else if (msframe->button2_need_rbutton)
	  {
	    msframe->button2_need_rbutton = 0;
	    mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
				msframe->last_click_point, msframe->last_click_time);
	  }
      }
    else
      assert ("Spurious timer fired" == 0);
    break;

  case WM_MOUSEMOVE:
    /* Optimization: don't report mouse movement while size is changind */
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    if (!msframe->sizing)
    {
      /* When waiting for the second mouse button to finish
	 button2 emulation, and have moved too far, just pretend
	 as if timer has expired. This impoves drag-select feedback */
      if ((msframe->button2_need_lbutton || msframe->button2_need_rbutton)
	  && !mswindows_button2_near_enough (msframe->last_click_point,
					     MAKEPOINTS (lParam)))
	{
	  KillTimer (hwnd, BUTTON_2_TIMER_ID);
	  SendMessage (hwnd, WM_TIMER, BUTTON_2_TIMER_ID, 0);
	}

      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = GetMessageTime();
      event->event_type = pointer_motion_event;
      event->event.motion.x = MAKEPOINTS(lParam).x;
      event->event.motion.y = MAKEPOINTS(lParam).y;
      event->event.motion.modifiers = mswindows_modifier_state (NULL, 0);
      
      mswindows_enqueue_dispatch_event (emacs_event);
    }
    break;

  case WM_PAINT:
    {
      PAINTSTRUCT paintStruct;
      
      frame = XFRAME (mswindows_find_frame (hwnd));

      BeginPaint (hwnd, &paintStruct);
      mswindows_redraw_exposed_area (frame,
			paintStruct.rcPaint.left, paintStruct.rcPaint.top,
			paintStruct.rcPaint.right, paintStruct.rcPaint.bottom);
      EndPaint (hwnd, &paintStruct);
    }
    break;

  case WM_SIZE:
    /* We only care about this message if our size has really changed */
    if (wParam==SIZE_RESTORED || wParam==SIZE_MAXIMIZED || wParam==SIZE_MINIMIZED)
    {
      RECT rect;
      int columns, rows;

      fobj = mswindows_find_frame (hwnd);
      frame = XFRAME (fobj);
      msframe  = FRAME_MSWINDOWS_DATA (frame);

      /* We cannot handle frame map and unmap hooks right in
	 this routine, because these may throw. We queue
	 magic events to run these hooks instead - kkm */

      if (wParam==SIZE_MINIMIZED)
	{
	  /* Iconified */
          FRAME_VISIBLE_P (frame) = 0;
	  mswindows_enqueue_magic_event (hwnd, XM_UNMAPFRAME);
	}
      else
	{
	  GetClientRect(hwnd, &rect);
	  FRAME_PIXWIDTH(frame) = rect.right;
	  FRAME_PIXHEIGHT(frame) = rect.bottom;

	  pixel_to_real_char_size (frame, rect.right, rect.bottom,
				   &MSWINDOWS_FRAME_CHARWIDTH (frame),
				   &MSWINDOWS_FRAME_CHARHEIGHT (frame));

	  pixel_to_char_size (frame, rect.right, rect.bottom, &columns, &rows);
	  change_frame_size (frame, rows, columns, 1);

	  /* If we are inside frame creation, we have to apply geometric
	     properties now. */
	  if (mswindows_frame_target_rect.left >= 0
	      || mswindows_frame_target_rect.top >= 0
	      || mswindows_frame_target_rect.width >= 0
	      || mswindows_frame_target_rect.height >= 0)
	    {
	      /* Yes, we have to size again */
	      XEMACS_RECT_WH geom;
	      
	      geom.left = mswindows_frame_target_rect.left;
	      geom.top = mswindows_frame_target_rect.top;
	      char_to_real_pixel_size (frame,
				       mswindows_frame_target_rect.width,
				       mswindows_frame_target_rect.height,
				       &geom.width, &geom.height);
	      if (mswindows_frame_target_rect.width < 0)
		geom.width = -1;
	      if (mswindows_frame_target_rect.height < 0)
		geom.height = -1;

	      /* Reset to we do not get here again */
	      mswindows_frame_target_rect.left = -1;
	      mswindows_frame_target_rect.top = -1;
	      mswindows_frame_target_rect.width = -1;
	      mswindows_frame_target_rect.height = -1;

	      /* Size the rectangle to the actual size */
	      GetWindowRect (hwnd, &rect);
	      SetWindowPos
		(hwnd, NULL,
		 geom.left >= 0 ? geom.left : rect.left,
		 geom.top >= 0 ? geom.top : rect.top,
		 geom.width >= 0 ? geom.width : rect.right - rect.left,
		 geom.height >= 0 ? geom.height : rect.bottom - rect.top,
		 SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING
		 | ((geom.left >= 0 || geom.top >= 0) ? 0 : SWP_NOMOVE)
		 | ((geom.width >= 0 || geom.height >= 0) ? 0 : SWP_NOSIZE));
	    }
	  else
	    {
	      if (!msframe->sizing && !FRAME_VISIBLE_P (frame))
		mswindows_enqueue_magic_event (hwnd, XM_MAPFRAME);
	      FRAME_VISIBLE_P (frame) = 1;
	      
	      if (!msframe->sizing || mswindows_dynamic_frame_resize)
		redisplay ();
	    }
	}
    }
    break;

  /* Misc magic events which only require that the frame be identified */
  case WM_SETFOCUS:
  case WM_KILLFOCUS:
    mswindows_enqueue_magic_event (hwnd, message);
    break;

  case WM_WINDOWPOSCHANGING:
    {
      WINDOWPOS *wp = (LPWINDOWPOS) lParam;
      WINDOWPLACEMENT wpl = { sizeof(WINDOWPLACEMENT) };
      GetWindowPlacement(hwnd, &wpl);

      /* Only interested if size is changing and we're not being iconified */
      if (wpl.showCmd != SW_SHOWMINIMIZED
	  && wpl.showCmd != SW_SHOWMAXIMIZED
	  && !(wp->flags & SWP_NOSIZE))
      {
	RECT ncsize = { 0, 0, 0, 0 };
	int pixwidth, pixheight;
 	AdjustWindowRectEx (&ncsize, GetWindowLong (hwnd, GWL_STYLE),
 			    GetMenu(hwnd) != NULL,
			    GetWindowLong (hwnd, GWL_EXSTYLE));

	round_size_to_real_char (XFRAME (mswindows_find_frame (hwnd)),
				 wp->cx - (ncsize.right - ncsize.left),
				 wp->cy - (ncsize.bottom - ncsize.top),
				 &pixwidth, &pixheight);

	/* Convert client sizes to window sizes */
	pixwidth += (ncsize.right - ncsize.left);
	pixheight += (ncsize.bottom - ncsize.top);

	if (wpl.showCmd != SW_SHOWMAXIMIZED)
	  {
	    /* Adjust so that the bottom or right doesn't move if it's
	     * the top or left that's being changed */
	    RECT rect;
	    GetWindowRect (hwnd, &rect);

	    if (rect.left != wp->x)
	      wp->x += wp->cx - pixwidth;
	    if (rect.top != wp->y)
	      wp->y += wp->cy - pixheight;
	  }

	wp->cx = pixwidth;
	wp->cy = pixheight;
      }
      /* DefWindowProc sends useful WM_GETMINMAXINFO message, and adjusts
	 window position if the user tries to track window too small */
    }
    goto defproc;

  case WM_ENTERSIZEMOVE:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    msframe->sizing = 1;
    return 0;

  case WM_EXITSIZEMOVE:
    msframe  = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
    msframe->sizing = 0;
    /* Queue noop event */
    mswindows_enqueue_magic_event (hwnd, XM_BUMPQUEUE);
    return 0;

#ifdef HAVE_SCROLLBARS
  case WM_VSCROLL:
  case WM_HSCROLL:
    {
      /* Direction of scroll is determined by scrollbar instance. */
      int code = (int) LOWORD(wParam);
      int pos = (short int) HIWORD(wParam);
      HWND hwndScrollBar = (HWND) lParam;
      struct gcpro gcpro1, gcpro2;

      mswindows_handle_scrollbar_event (hwndScrollBar, code,  pos);
      GCPRO2 (emacs_event, fobj);
      if (UNBOUNDP(mswindows_pump_outstanding_events()))	/* Can GC */
	{
	  /* Error during event pumping - cancel scroll */
	  SendMessage (hwndScrollBar, WM_CANCELMODE, 0, 0);
	}
      UNGCPRO;
      break;     
    }
#endif

#ifdef HAVE_MENUBARS
  case WM_INITMENU:
    if (UNBOUNDP (mswindows_handle_wm_initmenu (
			(HMENU) wParam,
			XFRAME (mswindows_find_frame (hwnd)))))
      SendMessage (hwnd, WM_CANCELMODE, 0, 0);
    break;

  case WM_INITMENUPOPUP:
    if (!HIWORD(lParam))
      {
	if (UNBOUNDP (mswindows_handle_wm_initmenupopup (
			(HMENU) wParam,
			 XFRAME (mswindows_find_frame (hwnd)))))
	  SendMessage (hwnd, WM_CANCELMODE, 0, 0);
      }
    break;

  case WM_EXITMENULOOP:
    if (UNBOUNDP (mswindows_handle_wm_exitmenuloop (
			XFRAME (mswindows_find_frame (hwnd)))))
      SendMessage (hwnd, WM_CANCELMODE, 0, 0);
    break;

#endif /* HAVE_MENUBARS */

  case WM_COMMAND:
    {
      WORD id = LOWORD (wParam);
      frame = XFRAME (mswindows_find_frame (hwnd));

#ifdef HAVE_MENUBARS
      if (!NILP (mswindows_handle_wm_command (frame, id)))
	break;
#endif

#ifdef HAVE_TOOLBARS
      /* O Toolbar Implementor, this place may have something for you!;*/
#endif

      /* Bite me - a spurious command. This cannot happen. */
      error ("XEMACS BUG: Cannot decode command message");
    }
  break;

  case WM_DROPFILES:	/* implementation ripped-off from event-Xt.c */
    {
      UINT filecount, i, len;
      POINT point;
      char filename[MAX_PATH];
#ifdef __CYGWIN32__
      char* fname;
#endif
      Lisp_Object l_type, l_dndlist = Qnil, l_item;

      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      if (!DragQueryPoint ((HANDLE) wParam, &point))
	point.x = point.y = -1;		/* outside client area */

      filecount = DragQueryFile ((HANDLE) wParam, -1, NULL, 0);
      if (filecount == 1)
	{
      	  l_type = make_int (DndFile);
	  len = DragQueryFile ((HANDLE) wParam, 0, filename, MAX_PATH);
#ifdef __CYGWIN32__
	  CYGWIN_CONV_PATH(filename, fname);
	  len=strlen(fname);
	  l_dndlist = make_ext_string (fname, len, FORMAT_FILENAME);
#else
	  l_dndlist = make_ext_string (filename, len, FORMAT_FILENAME);
#endif
	}
      else
	{
	  l_type = make_int (DndFiles);	  
	  for (i=0; i<filecount; i++)
	    {
  	      len = DragQueryFile ((HANDLE) wParam, i, filename, MAX_PATH);
#ifdef __CYGWIN32__
	      CYGWIN_CONV_PATH(filename, fname);
	      len=strlen(fname);
	      l_item = make_ext_string (fname, len, FORMAT_FILENAME);
#else
	      l_item = make_ext_string (filename, len, FORMAT_FILENAME);
#endif
	      l_dndlist = Fcons (l_item, l_dndlist);	/* reverse order */
	    }
	}
      DragFinish ((HANDLE) wParam);
      
      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = GetMessageTime();
      event->event_type = dnd_drop_event;
      event->event.dnd_drop.button = 1;		/* #### Should try harder */
      event->event.dnd_drop.modifiers = mswindows_modifier_state (NULL, 0);
      event->event.dnd_drop.x = point.x;
      event->event.dnd_drop.y = point.y;
      event->event.dnd_drop.data = Fcons (l_type, Fcons (l_dndlist, Qnil));

      mswindows_enqueue_dispatch_event (emacs_event);
    }
  break;

  defproc:
  default:
    return DefWindowProc (hwnd, message, wParam, lParam);
  }
  return (0);
}


/************************************************************************/
/*      keyboard, mouse & other helpers for the windows procedure       */
/************************************************************************/
static void
mswindows_set_chord_timer (HWND hwnd)
{
  int interval;

  /* We get one third half system double click threshold */
  if (mswindows_mouse_button_tolerance <= 0)
    interval = GetDoubleClickTime () / 3;
  else
    interval = mswindows_mouse_button_tolerance;

  SetTimer (hwnd, BUTTON_2_TIMER_ID, interval, 0);
}

static int
mswindows_button2_near_enough (POINTS p1, POINTS p2)
{
  int dx, dy;
  if (mswindows_mouse_button_max_skew_x <= 0)
    dx = GetSystemMetrics (SM_CXDOUBLECLK) / 2;
  else
    dx = mswindows_mouse_button_max_skew_x;

  if (mswindows_mouse_button_max_skew_y <= 0)
    dy = GetSystemMetrics (SM_CYDOUBLECLK) / 2;
  else
    dy = mswindows_mouse_button_max_skew_y;

  return abs (p1.x - p2.x) < dx && abs (p1.y- p2.y)< dy;
}

static int
mswindows_current_layout_has_AltGr (void)
{
  /* This simple caching mechanism saves 10% of CPU
     time when a key typed at autorepeat rate of 30 cps! */
  static HKL last_hkl = 0;
  static int last_hkl_has_AltGr;

  HKL current_hkl = GetKeyboardLayout (0);
  if (current_hkl != last_hkl)
    {
      TCHAR c;
      last_hkl_has_AltGr = 0;
      /* In this loop, we query whether a character requires
	 AltGr to be down to generate it. If at least such one
	 found, this means that the layout does regard AltGr */
      for (c = ' '; c <= 0xFFU && c != 0 && !last_hkl_has_AltGr; ++c)
	if (HIBYTE (VkKeyScan (c)) == 6)
	  last_hkl_has_AltGr = 1;
      last_hkl = current_hkl;
    }
  return last_hkl_has_AltGr;
}


/* Returns the state of the modifier keys in the format expected by the
 * Lisp_Event key_data, button_data and motion_data modifiers member */
int mswindows_modifier_state (BYTE* keymap, int has_AltGr)
{
  int mods = 0;

  if (keymap == NULL)
    {
      keymap = (BYTE*) alloca(256);
      GetKeyboardState (keymap);
      has_AltGr = mswindows_current_layout_has_AltGr ();
    }

  if (has_AltGr && (keymap [VK_LCONTROL] & 0x80) && (keymap [VK_RMENU] & 0x80))
    {
      mods |= (keymap [VK_LMENU] & 0x80) ? MOD_META : 0;
      mods |= (keymap [VK_RCONTROL] & 0x80) ? MOD_CONTROL : 0;
    }
  else
    {
      mods |= (keymap [VK_MENU] & 0x80) ? MOD_META : 0;
      mods |= (keymap [VK_CONTROL] & 0x80) ? MOD_CONTROL : 0;
    }

  mods |= (keymap [VK_SHIFT] & 0x80) ? MOD_SHIFT : 0;

  return mods;
}

/*
 * Translate a mswindows virtual key to a keysym.
 * Only returns non-Qnil for keys that don't generate WM_CHAR messages
 * or whose ASCII codes (like space) xemacs doesn't like.
 * Virtual key values are defined in winresrc.h
 * XXX I'm not sure that KEYSYM("name") is the best thing to use here.
 */
Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key, int mods)
{
  switch (mswindows_key)
  {
  /* First the predefined ones */
  case VK_BACK:		return QKbackspace;
  case VK_TAB:		return QKtab;
  case '\n':		return QKlinefeed;  /* No VK_LINEFEED in winresrc.h */
  case VK_RETURN:	return QKreturn;
  case VK_ESCAPE:	return QKescape;
  case VK_SPACE:	return QKspace;
  case VK_DELETE:	return QKdelete;

  /* The rest */
  case VK_CLEAR:	return KEYSYM ("clear");  /* Should do ^L ? */
  case VK_PRIOR:	return KEYSYM ("prior");
  case VK_NEXT:		return KEYSYM ("next");
  case VK_END:		return KEYSYM ("end");
  case VK_HOME:		return KEYSYM ("home");
  case VK_LEFT:		return KEYSYM ("left");
  case VK_UP:		return KEYSYM ("up");
  case VK_RIGHT:	return KEYSYM ("right");
  case VK_DOWN:		return KEYSYM ("down");
  case VK_SELECT:	return KEYSYM ("select");
  case VK_PRINT:	return KEYSYM ("print");
  case VK_EXECUTE:	return KEYSYM ("execute");
  case VK_SNAPSHOT:	return KEYSYM ("print");
  case VK_INSERT:	return KEYSYM ("insert");
  case VK_HELP:		return KEYSYM ("help");
#if 0	/* XXX What are these supposed to do? */
  case VK_LWIN		return KEYSYM ("");
  case VK_RWIN		return KEYSYM ("");
#endif
  case VK_APPS:		return KEYSYM ("menu");
  case VK_F1:		return KEYSYM ("f1");
  case VK_F2:		return KEYSYM ("f2");
  case VK_F3:		return KEYSYM ("f3");
  case VK_F4:		return KEYSYM ("f4");
  case VK_F5:		return KEYSYM ("f5");
  case VK_F6:		return KEYSYM ("f6");
  case VK_F7:		return KEYSYM ("f7");
  case VK_F8:		return KEYSYM ("f8");
  case VK_F9:		return KEYSYM ("f9");
  case VK_F10:		return KEYSYM ("f10");
  case VK_F11:		return KEYSYM ("f11");
  case VK_F12:		return KEYSYM ("f12");
  case VK_F13:		return KEYSYM ("f13");
  case VK_F14:		return KEYSYM ("f14");
  case VK_F15:		return KEYSYM ("f15");
  case VK_F16:		return KEYSYM ("f16");
  case VK_F17:		return KEYSYM ("f17");
  case VK_F18:		return KEYSYM ("f18");
  case VK_F19:		return KEYSYM ("f19");
  case VK_F20:		return KEYSYM ("f20");
  case VK_F21:		return KEYSYM ("f21");
  case VK_F22:		return KEYSYM ("f22");
  case VK_F23:		return KEYSYM ("f23");
  case VK_F24:		return KEYSYM ("f24");
  }
  return Qnil;
}

/*
 * Find the console that matches the supplied mswindows window handle
 */
Lisp_Object
mswindows_find_console (HWND hwnd)
{
  Lisp_Object concons;

  CONSOLE_LOOP (concons)
    {
      Lisp_Object console = XCAR (concons);
      /* We only support one console so this must be it */
      return console;
    }

  return Qnil;
}

/*
 * Find the frame that matches the supplied mswindows window handle
 */
static Lisp_Object
mswindows_find_frame (HWND hwnd)
{
  LONG l = GetWindowLong (hwnd, XWL_FRAMEOBJ);
  Lisp_Object f;
  if (l == 0)
    {
      /* We are in progress of frame creation. Return the frame
	 being created, as it still not remembered in the window
	 extra storage. */
      assert (!NILP (mswindows_frame_being_created));
      return mswindows_frame_being_created;
    }
  VOID_TO_LISP (f, l);
  return f;
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
  ++mswindows_pending_timers_count;
  return SetTimer (NULL, 0, milliseconds,
		   (TIMERPROC) mswindows_wm_timer_callback);
}

static void
emacs_mswindows_remove_timeout (int id)
{
  struct Lisp_Event match_against;
  Lisp_Object emacs_event;

  if (KillTimer (NULL, id))
    --mswindows_pending_timers_count;

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
			    
      /* #### What about Enter & Leave */
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

#ifndef HAVE_X_WINDOWS
/* This is called from GC when a process object is about to be freed.
   If we've still got pointers to it in this file, we're gonna lose hard.
 */
void
debug_process_finalization (struct Lisp_Process *p)
{
#if 0 /* #### */
  Lisp_Object instr, outstr;

  get_process_streams (p, &instr, &outstr);
  /* if it still has fds, then it hasn't been killed yet. */
  assert (NILP(instr));
  assert (NILP(outstr));

  /* #### More checks here */
#endif
}
#endif

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

  mswindows_error_caught_in_modal_loop = Qnil;
  staticpro (&mswindows_error_caught_in_modal_loop);
  mswindows_in_modal_loop = 0;
  mswindows_pending_timers_count = 0;

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

  DEFVAR_BOOL ("mswindows-dynamic-frame-resize", &mswindows_dynamic_frame_resize /*
*Controls redrawing frame contents during mouse-drag or keyboard resize
operation. When non-nil, the frame is redrawn while being resized. When
nil, frame is not redrawn, and exposed areas are filled with default
MDI application background color. Note that this option only has effect
if "Show window contents while dragging" is on in system Display/Plus!
settings.
Default is t on fast machines, nil on slow.
*/ );

/* The description copied verbatim from nt-emacs. (C) Geoff Voelker */
  DEFVAR_INT ("mswindows-mouse-button-tolerance", &mswindows_mouse_button_tolerance /*
*Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left/right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.
If negative or zero, currently set system default is used instead.
*/ );

/* The description copied verbatim from nt-emacs. (C) Geoff Voelker */
  DEFVAR_INT ("mswindows-num-mouse-buttons", &mswindows_num_mouse_buttons /*
Number of physical mouse buttons.
*/ );

  DEFVAR_INT ("mswindows-mouse-button-max-skew-x", &mswindows_mouse_button_max_skew_x /*
*Maximum horizontal distance in pixels between points in which left and
right button clicks occured for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable `mswindows-mouse-button-tolerance'.
If negative or zero, currently set system default is used instead.
*/ );

  DEFVAR_INT ("mswindows-mouse-button-max-skew-y", &mswindows_mouse_button_max_skew_y /*
*Maximum vertical distance in pixels between points in which left and
right button clicks occured for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable `mswindows-mouse-button-tolerance'.
If negative or zero, currently set system default is used instead.
*/ );

  mswindows_mouse_button_max_skew_x = 0;
  mswindows_mouse_button_max_skew_y = 0;
  mswindows_mouse_button_tolerance = 0;
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
