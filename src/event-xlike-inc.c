/* Shared event code between X and GTK -- include file.
   Copyright (C) 1991-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996, 2001, 2002, 2003 Ben Wing.

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

/* For some code it's reasonable to have only one copy and conditionalize
   at run-time.  For other code it isn't. #### Perhaps all code should be
   included here, not in event-xlike.c.  However, event-xlike.c is always
   X-specific, whereas the following code isn't, in the GTK case. */

static int
#ifdef THIS_IS_GTK
emacs_gtk_event_pending_p (int how_many)
#else
emacs_Xt_event_pending_p (int how_many)
#endif
{
  Lisp_Object event;
  int tick_count_val;

  /* If `how_many' is 0, then this function returns whether there are any
     X, timeout, or fd events pending (that is, whether
     emacs_Xt_next_event() would return immediately without blocking).

     If `how_many' is > 0, then this function returns whether there are
     that many *user generated* events available (keyboard, mouse click,
     etc.).  This also implies that emacs_Xt_next_event() would not block.
   */

  /* This function used to simply check whether there were any X events (or
     if user_p was 1, it iterated over all the pending X events using
     XCheckIfEvent(), looking for keystrokes and button events).  That
     worked in the old cheesoid event loop, which didn't go through
     XtAppDispatchEvent(), but it doesn't work any more -- X events may not
     result in anything.  For example, a button press in a blank part of
     the menubar appears as an X event but will not result in any Emacs
     events (a button press that activates the menubar results in an Emacs
     event through the stop_next_event mechanism).

     The only accurate way of determining whether these X events translate
     into Emacs events is to go ahead and dispatch them until there's
     something on the dispatch queue. */

  if (!how_many)
    {
      /* We're being asked for *ALL* events, not just user events. */

      /* (1) Any pending events in the dispatch queue? */
      if (!NILP (dispatch_event_queue))
	return 1;

      /* (2) Any TTY or process input available?

	 Note that formerly we just checked the value of XtAppPending() to
	 determine if there was file-desc input.  This doesn't work any
	 more with the signal_event_pipe; XtAppPending() will says "yes" in
	 this case but there isn't really any input.  So instead we keep
	 track of the file descriptors, and call select() ourselves.
	 Another way of fixing this problem is for the signal_event_pipe to
	 generate actual input in the form of an identity eval event or
	 something. (#### maybe this actually happens?) */

      if (poll_fds_for_input (non_fake_input_wait_mask))
	return 1;

#ifndef THIS_IS_GTK
      /* (3) Any timeout input available? */
      if (XtAppPending (Xt_app_con) & XtIMTimer)
	return 1;
#else
      /* #### Is there any way to do this in Gtk?  I don't think there
              is a 'peek' for events */
#endif
    }
  else
    {
      /* HOW_MANY > 0 */
      EVENT_CHAIN_LOOP (event, dispatch_event_queue)
	{
	  if (command_event_p (event))
	    {
	      how_many--;
	      if (how_many <= 0)
		return 1;
	    }
	}
    }

  /* XtAppPending() can be super-slow, esp. over a network connection.
     Quantify results have indicated that in some cases the call to
     detect_input_pending() completely dominates the running time of
     redisplay().  Fortunately, in a SIGIO world we can more quickly
     determine whether there are any X events: if an event has happened
     since the last time we checked, then a SIGIO will have happened.  On a
     machine with broken SIGIO, we'll still be in an OK state --
     quit_check_signal_tick_count will get ticked at least every 1/4
     second, so we'll be no more than that much behind reality. (In general
     it's OK if we erroneously report no input pending when input is
     actually pending() -- preemption is just a bit less efficient, that's
     all.  It's bad bad bad if you err the other way -- you've promised
     that `next-event' won't block but it actually will, and some action
     might get delayed until the next time you hit a key.)
     */

  if (!in_modal_loop)
    {
      /* quit_check_signal_tick_count is volatile so try to avoid race
	 conditions by using a temporary variable */
      tick_count_val = quit_check_signal_tick_count;
      if (last_quit_check_signal_tick_count != tick_count_val
#if !defined (THIS_IS_GTK) && (!defined (SIGIO) || defined (CYGWIN))
	  || (XtIMXEvent & XtAppPending (Xt_app_con))
#endif 
	  )
	{
	  last_quit_check_signal_tick_count = tick_count_val;

	  /* We need to drain the entire queue now -- if we only drain part of
	     it, we may later on end up with events actually pending but
	     detect_input_pending() returning false because there wasn't
	     another SIGIO. */
	  event_stream_drain_queue ();

	  if (!how_many)
	    return !NILP (dispatch_event_queue);

	  EVENT_CHAIN_LOOP (event, dispatch_event_queue)
	    {
	      if (command_event_p (event))
		{
		  how_many--;
		  if (how_many <= 0)
		    return 1;
		}
	    }

	  return 0;
	}
    }

  return 0;
}
