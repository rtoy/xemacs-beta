/* Handling asynchronous signals.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2001, 2002 Ben Wing.

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

/* Synched up with: Not synched with FSF.  Split out of keyboard.c. */

#include <config.h>
#include "lisp.h"

#include "console.h"
#include "device-impl.h"
#include "events.h" /* for signal_fake_event() */
#include "frame-impl.h"
#include "process.h"

#include "sysdep.h"
#include "sysfile.h"
#include "syssignal.h"
#include "systime.h"

/* Set to 1 when a quit-check signal (either a SIGIO interrupt or
   the asynch. timeout for poll-for-quit) occurs.  The QUITP
   macro may look at this. */
volatile int quit_check_signal_happened;

/* Count of the number of times a quit-check signal has occurred.
   Some stuff in event-Xt.c looks at this. */
volatile int quit_check_signal_tick_count;

/* Set to 1 when a SIGINT (or SIGQUIT) interrupt is processed.
   maybe_read_quit_event() looks at this. */
volatile int sigint_happened;

/* Set to 1 when an asynch. timeout signal occurs. */
static volatile int async_timeout_happened;

/* Set to 1 when a multiple of SLOWED_DOWN_INTERRUPTS_SECS elapses,
   after slow_down_interrupts() is called. */
static volatile int slowed_interrupt_timeout_happened;

/* This is used to synchronize setting the waiting_for_user_input_p
   flag. */
static volatile int async_timeout_happened_while_emacs_was_blocking;

/* See check_quit() for when this is set. */
int dont_check_for_quit;

static int poll_for_quit_id;
static int poll_for_sigchld_id;

/* This variable is used to communicate to a lisp
   process-filter/sentinel/asynchronous callback (via the function
   Fwaiting_for_user_input_p below) whether XEmacs was waiting for
   user-input when that process-filter was called. */
static int waiting_for_user_input_p;

static int interrupts_slowed_down;

#define SLOWED_DOWN_INTERRUPTS_SECS 15
#define NORMAL_QUIT_CHECK_TIMEOUT_MSECS 250
#define NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS 250

/* Used so that signals can break out of system calls that aren't
   naturally interruptible. */

JMP_BUF break_system_call_jump;
volatile int can_break_system_calls;

static SIGTYPE alarm_signal (int signo);



/**********************************************************************/
/*                  Asynchronous timeout functions                    */
/**********************************************************************/

/* See the comment in event-stream.c, under major heading "Timeouts",
   for the difference between low-level (one-shot) and high-level
   (periodic/resignaling) timeouts. */

/* The pending timers are stored in an ordered list, where the first timer
   on the list is the first one to fire.  Times recorded here are
   absolute. */
static struct low_level_timeout *async_timer_queue;

/* Nonzero means async timers are temporarily suppressed.  */
static int async_timer_suppress_count;

static void
set_one_shot_timer (EMACS_TIME interval)
{
#ifdef HAVE_SETITIMER
  struct itimerval it;
  it.it_value = interval;
  EMACS_SET_SECS_USECS (it.it_interval, 0, 0);
  qxe_setitimer (ITIMER_REAL, &it, 0);
#else
  int secs;
  EMACS_TIME_TO_INT (interval, secs);
  alarm (secs);
#endif
}

static void
reset_interval_timer (void)
{
  EMACS_TIME interval;

  /* Get the interval to set.  If an interval is available,
     make sure it's not zero (this is a valid return, but it will
     cause the timer to get disabled, so convert it to a very short
     time). */
  if (get_low_level_timeout_interval (async_timer_queue, &interval))
    {
      if (EMACS_SECS (interval) == 0 && EMACS_USECS (interval) == 0)
	EMACS_SET_USECS (interval, 1);
    }
  else
    /* A time of 0 means "disable". */
    EMACS_SET_SECS_USECS (interval, 0, 0);

  set_one_shot_timer (interval);
}


static void
init_async_timeouts (void)
{
  set_timeout_signal (SIGALRM, alarm_signal);
  async_timer_suppress_count = 0;
}

/* Turn off async timeouts.  */

static void
stop_async_timeouts (void)
{
  if (async_timer_suppress_count == 0)
    {
      /* If timer was on, turn it off. */
      EMACS_TIME thyme;
      EMACS_SET_SECS_USECS (thyme, 0, 0);
      set_one_shot_timer (thyme);
    }
  async_timer_suppress_count++;
}

/* Turn on async timeouts again. */

static void
start_async_timeouts (void)
{
  assert (async_timer_suppress_count > 0);
  async_timer_suppress_count--;
  if (async_timer_suppress_count == 0)
    {
      /* Some callers turn off async timeouts and then use the alarm
	 for their own purposes; so reinitialize everything. */
      set_timeout_signal (SIGALRM, alarm_signal);
      reset_interval_timer ();
    }
}

static void
handle_async_timeout_signal (void)
{
  int interval_id;
  int wakeup_id;
  Lisp_Object fun, arg;
  /* Avoid any possibility of GC during QUIT */
  int specco = begin_gc_forbidden ();

  /* No checks for Vinhibit_quit here or anywhere else in this file!!!
     Otherwise critical quit will not work right.
     The only check for Vinhibit_quit is in QUIT itself.

     (#### ???? I don't quite understand this comment.) */
  interval_id = pop_low_level_timeout (&async_timer_queue, 0);

  reset_interval_timer ();
  if (async_timeout_happened_while_emacs_was_blocking)
    {
      async_timeout_happened_while_emacs_was_blocking = 0;
      waiting_for_user_input_p = 1;
    }

  wakeup_id = event_stream_resignal_wakeup (interval_id, 1, &fun, &arg);

  if (wakeup_id == poll_for_quit_id)
    {
      quit_check_signal_happened = 1;
      quit_check_signal_tick_count++;
    }
  else if (wakeup_id == poll_for_sigchld_id)
    {
      kick_status_notify ();
    }
  else
    /* call1 GC-protects its arguments */
    call1_trapping_problems ("Error in asynchronous timeout callback",
			     fun, arg, INHIBIT_GC);

  waiting_for_user_input_p = 0;

  unbind_to (specco);
}

/* The following two functions are the external interface onto
   creating/deleting asynchronous interval timeouts, and are
   called by event-stream.c.  We call back to event-stream.c using
   event_stream_resignal_wakeup(), when an interval goes off. */

int
signal_add_async_interval_timeout (EMACS_TIME thyme)
{
  int id = add_low_level_timeout (&async_timer_queue, thyme);

  /* If this timeout is at the head of the queue, then we need to
     set the timer right now for this timeout.  Otherwise, things
     are fine as-is; after the timers ahead of us are signalled,
     the timer will be set for us. */

  if (async_timer_queue->id == id)
    reset_interval_timer ();

  return id;
}

void
signal_remove_async_interval_timeout (int id)
{
  int first = (async_timer_queue && async_timer_queue->id == id);
  remove_low_level_timeout (&async_timer_queue, id);

  /* If we removed the timeout from the head of the queue, then
     we need to reset the interval timer right now. */
  if (first)
    reset_interval_timer ();
}

/* If alarm() gets called when polling isn't disabled, it will mess up
   the asynchronous timeouts, and then C-g checking won't work again.
   Some libraries call alarm() directly, so we override the standard
   library's alarm() and abort() if the caller of the library function
   didn't wrap in stop_interrupts()/start_interrupts().

   NOTE: We could potentially avoid the need to wrap by adding a
   one-shot timeout to simulate the alarm(), smashing our signal
   handler back into place, and calling the library function when the
   alarm goes off.  But do we want to?  We're not going to gain the
   ability to C-g out of library functions this way (unless we forcibly
   longjmp() out of a signal handler, which is likely to lead to a
   crash). --ben */

#ifdef HAVE_SETITIMER

unsigned int
alarm (unsigned int howlong)
{
  struct itimerval old_it, new_it;

  assert (async_timer_suppress_count > 0);

  new_it.it_value.tv_sec = howlong;
  new_it.it_value.tv_usec = 0;
  new_it.it_interval.tv_sec = 0;
  new_it.it_interval.tv_usec = 0;
  qxe_setitimer (ITIMER_REAL, &new_it, &old_it);

  /* Never return zero if there was a timer outstanding. */
  return old_it.it_value.tv_sec + (old_it.it_value.tv_usec > 0 ? 1 : 0);
}

int
qxe_setitimer (int kind, const struct itimerval *itnew,
	       struct itimerval *itold)
{
#ifdef WIN32_ANY
  /* setitimer() does not exist on native MS Windows, and appears broken
     on Cygwin.  See win32.c.
     
     We are emulating the Unix98 setitimer() function, as found in its
     incarnations on modern versions of Unix.  Note however that in
     the win32.c version, ITNEW and ITOLD must be equal if both are
     non-zero, due to limitations in the underlying multimedia-timer
     API. */
  return mswindows_setitimer (kind, itnew, itold);
#else
  /* YUCK!  glibc defines setitimer's first argument as
     enum __itimer_which, not int, which causes compile errors if
     we call setitimer() in the obvious way. */
  switch (kind)
    {
    case ITIMER_REAL: return setitimer (ITIMER_REAL, itnew, itold);
    case ITIMER_VIRTUAL: return setitimer (ITIMER_VIRTUAL, itnew, itold);
    case ITIMER_PROF: return setitimer (ITIMER_PROF, itnew, itold);
    default: abort (); return 0;
    }
#endif
}

#endif /* HAVE_SETITIMER */

signal_handler_t
set_timeout_signal (int signal_number, signal_handler_t action)
{
#ifdef CYGWIN_BROKEN_SIGNALS
  return mswindows_sigset (signal_number, action);
#else
  return EMACS_SIGNAL (signal_number, action);
#endif
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p, 0, 0, 0, /*
Return non-nil if XEmacs is waiting for input from the user.
This is intended for use by asynchronous timeout callbacks and by
asynchronous process output filters and sentinels (not yet implemented
in XEmacs).  It will always be nil if XEmacs is not inside of
an asynchronous timeout or process callback.
*/
       ())
{
  return waiting_for_user_input_p ? Qt : Qnil;
}


/**********************************************************************/
/*                     Enabling/disabling signals                     */
/**********************************************************************/

static int interrupts_initted;

void
stop_interrupts (void)
{
  if (!interrupts_initted)
    return;
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
  unrequest_sigio ();
#endif
  stop_async_timeouts ();
}

void
start_interrupts (void)
{
  if (!interrupts_initted)
    return;
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
  request_sigio ();
#endif
  start_async_timeouts ();
}


static void
establish_slow_interrupt_timer (void)
{
  EMACS_TIME thyme;

  EMACS_SET_SECS_USECS (thyme, SLOWED_DOWN_INTERRUPTS_SECS, 0);
  set_one_shot_timer (thyme);
}

/* Some functions don't like being interrupted with SIGALRM or SIGIO.
   Previously we were calling stop_interrupts() / start_interrupts(),
   but then if the program hangs in one of those functions, e.g.
   waiting for a connect(), we're really screwed.  So instead we
   just "slow them down".  We do this by disabling all interrupts
   and then installing a timer of length fairly large, like 5 or
   10 secs.  That way, any "legitimate" connections (which should
   take a fairly short amount of time) go through OK, but we can
   interrupt bogus ones. */

void
slow_down_interrupts (void)
{
  /* We have to set the flag *before* setting the slowed-down timer,
     to avoid a race condition -- if the signal occurs between the
     call to set_one_shot_timer() and the setting of this flag,
     async_timeout_happened will get set, which will be a Bad Thing if
     there were no timeouts on the queue. */
  interrupts_slowed_down++;
  if (interrupts_slowed_down == 1)
    {
      stop_interrupts ();
      establish_slow_interrupt_timer ();
    }
}

void
speed_up_interrupts (void)
{
  if (interrupts_slowed_down > 0)
    {
      start_interrupts ();
      /* Change this flag AFTER fiddling with interrupts, for the same
	 race-condition reasons as above. */
      interrupts_slowed_down--;
    }
}


/**********************************************************************/
/*                 The mechanism that drives it all                   */
/**********************************************************************/

/* called from QUIT when something_happened gets set (as a result of
   a signal) */

void
check_what_happened (void)
{
  /* No GC can happen anywhere here.  handle_async_timeout_signal()
     prevents GC (from asynch timeout handler), so does check_quit()
     (from processing a message such as WM_INITMENU as a result of
     draining the message queue).  establish_slow_interrupt_timer() is
     too low-level to do anything that might invoke QUIT or call Lisp
     code. */

#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
  /* When in a critical section, don't reset something_happened, so that
     every single QUIT will verify proper wrapping. (something_happened
     was set by enter_redisplay_critical_section() and will be reset
     upon exit.) */
  if (!in_display)
#endif
    something_happened = 0;

  if (async_timeout_happened)
    {
      async_timeout_happened = 0;
      handle_async_timeout_signal ();
    }
  if (slowed_interrupt_timeout_happened)
    {
      slowed_interrupt_timeout_happened = 0;
      establish_slow_interrupt_timer ();
    }

  check_quit ();
}

#ifdef SIGIO

/* Signal handler for SIGIO. */

static void
input_available_signal (int SIG_ARG_MAYBE_UNUSED (signo))
{
  something_happened = 1; /* tell QUIT to wake up */
  quit_check_signal_happened = 1;
  quit_check_signal_tick_count++;
  EMACS_REESTABLISH_SIGNAL (signo, input_available_signal);
  SIGRETURN;
}

#endif /* SIGIO */

/* Actual signal handler for SIGALRM.  Called when:

   -- asynchronous timeouts (added with `add-async-timeout') go off

   -- when the poll-for-quit timer (used for C-g handling; more or
      less when SIGIO is unavailable or BROKEN_SIGIO is defined) or
      poll-for-sigchld timer (used when BROKEN_SIGCHLD is defined) go
      off.  The latter two timers, if set, normally go off every 1/4
      of a second -- see NORMAL_QUIT_CHECK_TIMEOUT_MSECS and
      NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS. (Both of these timers are
      treated like other asynchronous timeouts, but special-cased
      in handle_async_timeout_signal().)

   -- we called slow_down_interrupts() and SLOWED_DOWN_INTERRUPTS_SECS
      (or a multiple of it) has elapsed.

   Note that under Windows, we have no working setitimer(), so we
   simulate it using the multimedia timeout functions,
   e.g. timeSetEvent().  See setitimer() in nt.c.

   Note also that we don't actually *do* anything here (except in the
   case of can_break_system_calls).  Instead, we just set various
   flags; next time QUIT is called, the flags will cause
   check_what_happened() to be called, at which point we do everything
   indicated by the flags.
*/

static SIGTYPE
alarm_signal (int signo)
{
  something_happened = 1; /* tell QUIT to wake up and call
			     check_what_happened() */

  if (interrupts_slowed_down)
    {
      /* we are in "slowed-down interrupts" mode; the only alarm
	 happening here is the slowed-down quit-check alarm, so
	 we set this flag.

	 Do NOT set async_timeout_happened, because we don't want
	 anyone looking at the timeout queue -- async timeouts
	 are disabled. */
      quit_check_signal_happened = 1;
      quit_check_signal_tick_count++;
      /* make sure we establish the slow timer again. */
      slowed_interrupt_timeout_happened = 1;

      /* can_break_system_calls is set when we want to break out of
	 non-interruptible system calls. */
      if (can_break_system_calls)
	{
	  /* reset the flag for safety and such.  Do this *before*
	     unblocking or reestablishing the signal to avoid potential
	     race conditions. */
	  can_break_system_calls = 0;
#ifndef WIN32_NATIVE
	  /* #### I didn't add this WIN32_NATIVE check.  I'm not sure
	     why it's here.  But then again, someone needs to review
	     this can_break_system_calls stuff and see if it still
	     makes sense. --ben */
	  EMACS_UNBLOCK_SIGNAL (signo);
	  EMACS_REESTABLISH_SIGNAL (signo, alarm_signal);
	  LONGJMP (break_system_call_jump, 0);
#endif
	}
    }
  else
    {
      async_timeout_happened = 1;
      if (emacs_is_blocking)
	async_timeout_happened_while_emacs_was_blocking = 1;
      /* #### This is for QUITP.  When it is run, it may not be the
	 place to do arbitrary stuff like run asynch. handlers, but
	 it needs to know whether the poll-for-quit asynch. timeout
	 went off.  Rather than put the code in to compute this
	 specially, we just set this flag.  Should fix this. */
      quit_check_signal_happened = 1;

#ifdef HAVE_UNIXOID_EVENT_LOOP
      signal_fake_event ();
#endif
    }

  EMACS_REESTABLISH_SIGNAL (signo, alarm_signal);
  SIGRETURN;
}

/* Set this for debugging, to have a way to get out */
int stop_character; /* #### not currently implemented */

/* Signal handler for SIGINT and SIGQUIT.  On TTY's, one of these two
   signals will get generated in response to C-g.  (When running under
   X, C-g is handled using the SIGIO handler, which sets a flag
   telling the QUIT macro to scan the unread events for a ^G.)
   */

static SIGTYPE
interrupt_signal (int sig)
{
  /* This function can call lisp */
  /* #### we should NOT be calling lisp from a signal handler, boys
     and girls */
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;

  EMACS_REESTABLISH_SIGNAL (sig, interrupt_signal);

  if (sigint_happened && CONSOLEP (Vcontrolling_terminal) &&
      CONSOLE_LIVE_P (XCONSOLE (Vcontrolling_terminal)) &&
      !emacs_is_blocking)
    {
      /* #### this is inherited from GNU Emacs.  Do we really want this?
	 --ben */
      char c;
      fflush (stdout);
      reset_initial_console ();
      EMACS_UNBLOCK_SIGNAL (sig);
#ifdef SIGTSTP			/* Support possible in later USG versions */
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
      sys_suspend ();
#else
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      stdout_out ("No support for stopping a process on this operating system;\n");
      stdout_out ("you can continue or abort.\n");
#endif /* not SIGTSTP */
      stdout_out ("Auto-save? (y or n) ");
      if (((c = getc (stdin)) & ~040) == 'Y')
	Fdo_auto_save (Qnil, Qnil);
      while (c != '\n')
        c = getc (stdin);
      stdout_out ("Abort (and dump core)? (y or n) ");
      if (((c = getc (stdin)) & ~040) == 'Y')
	abort ();
      while (c != '\n')
        c = getc (stdin);
      stdout_out ("Continuing...\n");
      reinit_initial_console ();
      MARK_FRAME_CHANGED (XFRAME (DEVICE_SELECTED_FRAME
				  (XDEVICE (CONSOLE_SELECTED_DEVICE
					    (XCONSOLE
					     (Vcontrolling_terminal))))));
    }
  else
    {
      /* Else request quit when it's safe */
      Vquit_flag = Qt;
      sigint_happened = 1;
#ifdef HAVE_UNIXOID_EVENT_LOOP
      signal_fake_event ();
#endif
    }
  errno = old_errno;
  SIGRETURN;
}


/**********************************************************************/
/*                        Control-G checking                          */
/**********************************************************************/

/*

Info on Control-G checking:

  (Info-goto-node "(internals)Control-G (Quit) Checking")
*/

/* Defer all checking or processing of C-g.  You can do this, for example,
   if you want to read C-g's as events. (In that case, you should set
   Vquit_flag to Qnil just before you unbind, because it typically gets set
   as a result of reading C-g.) */

int
begin_dont_check_for_quit (void)
{
  int depth = specpdl_depth ();
  /* As an optimization in QUIT_FLAG_SAYS_SHOULD_QUIT, we bind inhibit-quit
     to t -- it has to be checked anyway, and by doing this, we only need
     to check dont_check_for_quit when quit-flag == `critical', which is
     rare. */
  specbind (Qinhibit_quit, Qt);
  internal_bind_int (&dont_check_for_quit, 1);

  return depth;
}

/* If we're inside of a begin_dont_check_for_quit() section, but want
   to temporarily enable quit-checking, call this.  This is used in
   particular when processing menu filters -- some menu filters do
   antisocial things like load large amounts of Lisp code (custom in
   particular), and we obviously want a way of breaking out of any
   problems.  If you do use this, you should really be trapping the
   throw() that comes from the quitting (as does the code that handles
   menus popping up). */

int
begin_do_check_for_quit (void)
{
  int depth = specpdl_depth ();
  specbind (Qinhibit_quit, Qnil);
  internal_bind_int (&dont_check_for_quit, 0);
  /* #### should we set Vquit_flag to Qnil? */
  return depth;
 }

/* The effect of this function is to set Vquit_flag appropriately if the
   user pressed C-g or Sh-C-g.  After this function finishes, Vquit_flag
   will be Qt for C-g, Qcritical for Sh-C-g, and unchanged otherwise.
   The C-g or Sh-C-g is discarded, so it won't be noticed again. 
*/

void
check_quit (void)
{
  /* dont_check_for_quit is set in three circumstances:

     (1) when we are in the process of changing the window
     configuration.  The frame might be in an inconsistent state,
     which will cause assertion failures if we check for QUIT.

     (2) when we are reading events, and want to read the C-g
     as an event.  The normal check for quit will discard the C-g,
     which would be bad.

     (3) when we're going down with a fatal error.  we're most likely
     in an inconsistent state, and we definitely don't want to be
     interrupted. */

  /* We should *not* conditionalize on Vinhibit_quit, or
     critical-quit (Control-Shift-G) won't work right. */

  /* WARNING: Even calling check_quit(), without actually dispatching
     a quit signal, can result in arbitrary Lisp code getting executed
     -- at least under Windows. (Not to mention obvious Lisp
     invocations like asynchronous timer callbacks.) Here's a sample
     stack trace to demonstrate:

 NTDLL! DbgBreakPoint@0 address 0x77f9eea9
assert_failed(const char * 0x012d036c, int 4596, const char * 0x012d0354) line 3478
re_match_2_internal(re_pattern_buffer * 0x012d6780, const unsigned char * 0x00000000, int 0, const unsigned char * 0x022f9328, int 34, int 0, re_registers * 0x012d53d0 search_regs, int 34) line 4596 + 41 bytes
re_search_2(re_pattern_buffer * 0x012d6780, const char * 0x00000000, int 0, const char * 0x022f9328, int 34, int 0, int 34, re_registers * 0x012d53d0 search_regs, int 34) line 4269 + 37 bytes
re_search(re_pattern_buffer * 0x012d6780, const char * 0x022f9328, int 34, int 0, int 34, re_registers * 0x012d53d0 search_regs) line 4031 + 37 bytes
string_match_1(long 31222628, long 30282164, long 28377092, buffer * 0x022fde00, int 0) line 413 + 69 bytes
Fstring_match(long 31222628, long 30282164, long 28377092, long 28377092) line 436 + 34 bytes
Ffuncall(int 3, long * 0x008297f8) line 3488 + 168 bytes
execute_optimized_program(const unsigned char * 0x020ddc50, int 6, long * 0x020ddf50) line 744 + 16 bytes
funcall_compiled_function(long 34407748, int 1, long * 0x00829aec) line 516 + 53 bytes
Ffuncall(int 2, long * 0x00829ae8) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x020ddc90, int 4, long * 0x020ddf90) line 744 + 16 bytes
funcall_compiled_function(long 34407720, int 1, long * 0x00829e28) line 516 + 53 bytes
Ffuncall(int 2, long * 0x00829e24) line 3523 + 17 bytes
mapcar1(long 15, long * 0x00829e48, long 34447820, long 34187868) line 2929 + 11 bytes
Fmapcar(long 34447820, long 34187868) line 3035 + 21 bytes
Ffuncall(int 3, long * 0x00829f20) line 3488 + 93 bytes
execute_optimized_program(const unsigned char * 0x020c2b70, int 7, long * 0x020dd010) line 744 + 16 bytes
funcall_compiled_function(long 34407580, int 2, long * 0x0082a210) line 516 + 53 bytes
Ffuncall(int 3, long * 0x0082a20c) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x020cf810, int 6, long * 0x020cfb10) line 744 + 16 bytes
funcall_compiled_function(long 34407524, int 0, long * 0x0082a580) line 516 + 53 bytes
Ffuncall(int 1, long * 0x0082a57c) line 3523 + 17 bytes
run_hook_with_args_in_buffer(buffer * 0x022fde00, int 1, long * 0x0082a57c, int 0) line 3980 + 13 bytes
run_hook_with_args(int 1, long * 0x0082a57c, int 0) line 3993 + 23 bytes
Frun_hooks(int 1, long * 0x0082a57c) line 3847 + 19 bytes
run_hook(long 34447484) line 4094 + 11 bytes
unsafe_handle_wm_initmenu_1(frame * 0x01dbb000) line 736 + 11 bytes
unsafe_handle_wm_initmenu(long 28377092) line 807 + 11 bytes
condition_case_1(long 28377116, long (long)* 0x0101c827 unsafe_handle_wm_initmenu(long), long 28377092, long (long, long)* 0x01005fa4 mswindows_modal_loop_error_handler(long, long), long 28377092) line 1692 + 7 bytes
mswindows_protect_modal_loop(long (long)* 0x0101c827 unsafe_handle_wm_initmenu(long), long 28377092) line 1194 + 32 bytes
mswindows_handle_wm_initmenu(HMENU__ * 0x00010199, frame * 0x01dbb000) line 826 + 17 bytes
mswindows_wnd_proc(HWND__ * 0x000501da, unsigned int 278, unsigned int 65945, long 0) line 3089 + 31 bytes
USER32! UserCallWinProc@20 + 24 bytes
USER32! DispatchClientMessage@20 + 47 bytes
USER32! __fnDWORD@4 + 34 bytes
NTDLL! KiUserCallbackDispatcher@12 + 19 bytes
USER32! DispatchClientMessage@20 address 0x77e163cc
USER32! DefWindowProcW@16 + 34 bytes
qxeDefWindowProc(HWND__ * 0x000501da, unsigned int 274, unsigned int 61696, long 98) line 1188 + 22 bytes
mswindows_wnd_proc(HWND__ * 0x000501da, unsigned int 274, unsigned int 61696, long 98) line 3362 + 21 bytes
USER32! UserCallWinProc@20 + 24 bytes
USER32! DispatchClientMessage@20 + 47 bytes
USER32! __fnDWORD@4 + 34 bytes
NTDLL! KiUserCallbackDispatcher@12 + 19 bytes
USER32! DispatchClientMessage@20 address 0x77e163cc
USER32! DefWindowProcW@16 + 34 bytes
qxeDefWindowProc(HWND__ * 0x000501da, unsigned int 262, unsigned int 98, long 540016641) line 1188 + 22 bytes
mswindows_wnd_proc(HWND__ * 0x000501da, unsigned int 262, unsigned int 98, long 540016641) line 3362 + 21 bytes
USER32! UserCallWinProc@20 + 24 bytes
USER32! DispatchMessageWorker@8 + 244 bytes
USER32! DispatchMessageW@4 + 11 bytes
qxeDispatchMessage(const tagMSG * 0x0082c684 {msg=0x00000106 wp=0x00000062 lp=0x20300001}) line 989 + 10 bytes
mswindows_drain_windows_queue() line 1345 + 9 bytes
emacs_mswindows_quit_p() line 3947
event_stream_quit_p() line 666
check_quit() line 686
check_what_happened() line 437
re_match_2_internal(re_pattern_buffer * 0x012d5a18, const unsigned char * 0x00000000, int 0, const unsigned char * 0x02235000, int 23486, int 14645, re_registers * 0x012d53d0 search_regs, int 23486) line 4717 + 14 bytes
re_search_2(re_pattern_buffer * 0x012d5a18, const char * 0x02235000, int 23486, const char * 0x0223b38e, int 0, int 14645, int 8841, re_registers * 0x012d53d0 search_regs, int 23486) line 4269 + 37 bytes
search_buffer(buffer * 0x022fde00, long 29077572, long 13789, long 23487, long 1, int 1, long 28377092, long 28377092, int 0) line 1224 + 89 bytes
search_command(long 29077572, long 46975, long 28377116, long 28377092, long 28377092, int 1, int 1, int 0) line 1054 + 151 bytes
Fre_search_forward(long 29077572, long 46975, long 28377116, long 28377092, long 28377092) line 2147 + 31 bytes
Ffuncall(int 4, long * 0x0082ceb0) line 3488 + 216 bytes
execute_optimized_program(const unsigned char * 0x02047810, int 13, long * 0x02080c10) line 744 + 16 bytes
funcall_compiled_function(long 34187208, int 3, long * 0x0082d1b8) line 516 + 53 bytes
Ffuncall(int 4, long * 0x0082d1b4) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x01e96a10, int 6, long * 0x020ae510) line 744 + 16 bytes
funcall_compiled_function(long 34186676, int 3, long * 0x0082d4a0) line 516 + 53 bytes
Ffuncall(int 4, long * 0x0082d49c) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x02156b50, int 4, long * 0x020c2db0) line 744 + 16 bytes
funcall_compiled_function(long 34186564, int 2, long * 0x0082d780) line 516 + 53 bytes
Ffuncall(int 3, long * 0x0082d77c) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x0082d964, int 3, long * 0x020c2d70) line 744 + 16 bytes
Fbyte_code(long 29405156, long 34352480, long 7) line 2392 + 38 bytes
Feval(long 34354440) line 3290 + 187 bytes
condition_case_1(long 34354572, long (long)* 0x01087232 Feval(long), long 34354440, long (long, long)* 0x01084764 run_condition_case_handlers(long, long), long 28377092) line 1692 + 7 bytes
condition_case_3(long 34354440, long 28377092, long 34354572) line 1779 + 27 bytes
execute_rare_opcode(long * 0x0082dc7c, const unsigned char * 0x01b090af, int 143) line 1269 + 19 bytes
execute_optimized_program(const unsigned char * 0x01b09090, int 6, long * 0x020ae590) line 654 + 17 bytes
funcall_compiled_function(long 34186620, int 0, long * 0x0082df68) line 516 + 53 bytes
Ffuncall(int 1, long * 0x0082df64) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x02195470, int 1, long * 0x020c2df0) line 744 + 16 bytes
funcall_compiled_function(long 34186508, int 0, long * 0x0082e23c) line 516 + 53 bytes
Ffuncall(int 1, long * 0x0082e238) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x01e5d410, int 6, long * 0x0207d410) line 744 + 16 bytes
funcall_compiled_function(long 34186312, int 1, long * 0x0082e524) line 516 + 53 bytes
Ffuncall(int 2, long * 0x0082e520) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x02108fb0, int 2, long * 0x020c2e30) line 744 + 16 bytes
funcall_compiled_function(long 34186340, int 0, long * 0x0082e7fc) line 516 + 53 bytes
Ffuncall(int 1, long * 0x0082e7f8) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x020fe150, int 2, long * 0x01e6f510) line 744 + 16 bytes
funcall_compiled_function(long 31008124, int 0, long * 0x0082ebd8) line 516 + 53 bytes
Ffuncall(int 1, long * 0x0082ebd4) line 3523 + 17 bytes
run_hook_with_args_in_buffer(buffer * 0x022fde00, int 1, long * 0x0082ebd4, int 0) line 3980 + 13 bytes
run_hook_with_args(int 1, long * 0x0082ebd4, int 0) line 3993 + 23 bytes
Frun_hooks(int 1, long * 0x0082ebd4) line 3847 + 19 bytes
Ffuncall(int 2, long * 0x0082ebd0) line 3509 + 14 bytes
execute_optimized_program(const unsigned char * 0x01ef2210, int 5, long * 0x01da8e10) line 744 + 16 bytes
funcall_compiled_function(long 31020440, int 2, long * 0x0082eeb8) line 516 + 53 bytes
Ffuncall(int 3, long * 0x0082eeb4) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x0082f09c, int 3, long * 0x01d89390) line 744 + 16 bytes
Fbyte_code(long 31102388, long 30970752, long 7) line 2392 + 38 bytes
Feval(long 31087568) line 3290 + 187 bytes
condition_case_1(long 30961240, long (long)* 0x01087232 Feval(long), long 31087568, long (long, long)* 0x01084764 run_condition_case_handlers(long, long), long 28510180) line 1692 + 7 bytes
condition_case_3(long 31087568, long 28510180, long 30961240) line 1779 + 27 bytes
execute_rare_opcode(long * 0x0082f450, const unsigned char * 0x01ef23ec, int 143) line 1269 + 19 bytes
execute_optimized_program(const unsigned char * 0x01ef2310, int 6, long * 0x01da8f10) line 654 + 17 bytes
funcall_compiled_function(long 31020412, int 1, long * 0x0082f740) line 516 + 53 bytes
Ffuncall(int 2, long * 0x0082f73c) line 3523 + 17 bytes
execute_optimized_program(const unsigned char * 0x020fe650, int 3, long * 0x01d8c490) line 744 + 16 bytes
funcall_compiled_function(long 31020020, int 2, long * 0x0082fa14) line 516 + 53 bytes
Ffuncall(int 3, long * 0x0082fa10) line 3523 + 17 bytes
Fcall_interactively(long 29685180, long 28377092, long 28377092) line 1008 + 22 bytes
Fcommand_execute(long 29685180, long 28377092, long 28377092) line 2929 + 17 bytes
execute_command_event(command_builder * 0x01be1900, long 36626492) line 4048 + 25 bytes
Fdispatch_event(long 36626492) line 4341 + 70 bytes
Fcommand_loop_1() line 582 + 9 bytes
command_loop_1(long 28377092) line 495
condition_case_1(long 28377188, long (long)* 0x01064fb9 command_loop_1(long), long 28377092, long (long, long)* 0x010649d0 cmd_error(long, long), long 28377092) line 1692 + 7 bytes
command_loop_3() line 256 + 35 bytes
command_loop_2(long 28377092) line 269
internal_catch(long 28457612, long (long)* 0x01064b20 command_loop_2(long), long 28377092, int * volatile 0x00000000) line 1317 + 7 bytes
initial_command_loop(long 28377092) line 305 + 25 bytes
STACK_TRACE_EYE_CATCHER(int 1, char * * 0x01b63ff0, char * * 0x01ca5300, int 0) line 2501
main(int 1, char * * 0x01b63ff0, char * * 0x01ca5300) line 2938
XEMACS! mainCRTStartup + 180 bytes
_start() line 171
KERNEL32! BaseProcessStart@4 + 115547 bytes

*/
  int specdepth;
  
  if (dont_check_for_quit)
    return;

  if (quit_check_signal_happened)
    {
#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
      /* Since the code below can call Lisp, make sure that proper wrapping is
	 in place during redisplay. */
      assert_with_message
	(proper_redisplay_wrapping_in_place (),
	 "QUIT called from within redisplay without being properly wrapped");
#endif

      /* Since arbitrary Lisp code may be executed (e.g. through a menu
         filter, see backtrace directly above), GC might happen,
         which would majorly fuck a lot of things, e.g. re_match()
         [string gets relocated] and lots of other code that's not
         prepared to handle GC in QUIT. */
      specdepth = begin_gc_forbidden ();
      quit_check_signal_happened = 0;
      event_stream_quit_p ();
      unbind_to (specdepth);
    }
}



void
init_poll_for_quit (void)
{
#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
  /* Check for C-g every 1/4 of a second.

     #### This is just a guess.  Some investigation will have to be
     done to see what the best value is.  The best value is the
     smallest possible value that doesn't cause a significant amount
     of running time to be spent in C-g checking. */
  if (!poll_for_quit_id)
    poll_for_quit_id =
      event_stream_generate_wakeup (NORMAL_QUIT_CHECK_TIMEOUT_MSECS,
				    NORMAL_QUIT_CHECK_TIMEOUT_MSECS,
				    Qnil, Qnil, 1);
#endif /* not SIGIO and not DONT_POLL_FOR_QUIT */
}

#if 0 /* not used anywhere */

void
reset_poll_for_quit (void)
{
#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
  if (poll_for_quit_id)
    {
      event_stream_disable_wakeup (poll_for_quit_id, 1);
      poll_for_quit_id = 0;
    }
#endif /* not SIGIO and not DONT_POLL_FOR_QUIT */
}

#endif /* 0 */

#if defined (HAVE_UNIX_PROCESSES) && !defined (SIGCHLD)

static void
init_poll_for_sigchld (void)
{
  /* Check for terminated processes every 1/4 of a second.

     #### This is just a guess.  Some investigation will have to be
     done to see what the best value is.  The best value is the
     smallest possible value that doesn't cause a significant amount
     of running time to be spent in process-termination checking.
     */
  poll_for_sigchld_id =
    event_stream_generate_wakeup (NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS,
				  NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS,
				  Qnil, Qnil, 1);
}

#endif /* not SIGCHLD */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

/* If we've been nohup'ed, keep it that way.
   This allows `nohup xemacs &' to work.
   More generally, if a normally fatal signal has been redirected
   to SIG_IGN by our invocation environment, trust the environment.
   This keeps xemacs from being killed by a SIGQUIT intended for a
   different process after having been backgrounded under a
   non-job-control shell! */
static void
handle_signal_if_fatal (int signo)
{
  if (EMACS_SIGNAL (signo,  fatal_error_signal) == SIG_IGN)
    EMACS_SIGNAL (signo, SIG_IGN);
}

void
init_signals_very_early (void)
{
  /* Catch all signals that would kill us.
     Don't catch these signals in batch mode if not initialized.
     On some machines, this sets static data that would make
     signal fail to work right when the dumped Emacs is run.  */
  if (noninteractive && !initialized)
    return;

  handle_signal_if_fatal (SIGILL);  /* ANSI */
  handle_signal_if_fatal (SIGABRT); /* ANSI */
  handle_signal_if_fatal (SIGFPE);  /* ANSI */
  handle_signal_if_fatal (SIGSEGV); /* ANSI */
  handle_signal_if_fatal (SIGTERM); /* ANSI */


#ifdef SIGHUP
  handle_signal_if_fatal (SIGHUP);  /* POSIX */
#endif
#ifdef SIGQUIT
  handle_signal_if_fatal (SIGQUIT); /* POSIX */
#endif
#ifdef SIGTRAP
  handle_signal_if_fatal (SIGTRAP); /* POSIX */
#endif
#ifdef SIGUSR1
  handle_signal_if_fatal (SIGUSR1); /* POSIX */
#endif
#ifdef SIGUSR2
  handle_signal_if_fatal (SIGUSR2); /* POSIX */
#endif
#ifdef SIGPIPE
  handle_signal_if_fatal (SIGPIPE); /* POSIX */
#endif
#ifdef SIGALRM
  /* This will get reset later, once we're
     capable of handling it properly. */
  handle_signal_if_fatal (SIGALRM); /* POSIX */
#endif


#ifdef SIGBUS
  handle_signal_if_fatal (SIGBUS);  /* XPG5 */
#endif
#ifdef SIGSYS
  handle_signal_if_fatal (SIGSYS);  /* XPG5 */
#endif
#ifdef SIGXCPU
  handle_signal_if_fatal (SIGXCPU); /* XPG5 */
#endif
#ifdef SIGXFSZ
  handle_signal_if_fatal (SIGXFSZ); /* XPG5 */
#endif
#ifdef SIGVTALRM
  handle_signal_if_fatal (SIGVTALRM); /* XPG5 */
#endif
#ifdef SIGPROF
  /* Messes up the REAL profiler */
  /* handle_signal_if_fatal (SIGPROF); */ /* XPG5 */
#endif


#ifdef SIGHWE
  handle_signal_if_fatal (SIGHWE);
#endif
#ifdef SIGPRE
  handle_signal_if_fatal (SIGPRE);
#endif
#ifdef SIGORE
  handle_signal_if_fatal (SIGORE);
#endif
#ifdef SIGUME
  handle_signal_if_fatal (SIGUME);
#endif
#ifdef SIGDLK
  handle_signal_if_fatal (SIGDLK);
#endif
#ifdef SIGCPULIM
  handle_signal_if_fatal (SIGCPULIM);
#endif
#ifdef SIGIOT
  handle_signal_if_fatal (SIGIOT);
#endif
#ifdef SIGEMT
  handle_signal_if_fatal (SIGEMT);
#endif
#ifdef SIGLOST
  handle_signal_if_fatal (SIGLOST);
#endif
#ifdef SIGSTKFLT /* coprocessor stack fault under Linux */
  handle_signal_if_fatal (SIGSTKFLT);
#endif
#ifdef SIGUNUSED /* exists under Linux, and will kill process! */
  handle_signal_if_fatal (SIGUNUSED);
#endif

#ifdef AIX
/* 20 is SIGCHLD, 21 is SIGTTIN, 22 is SIGTTOU.  */
#ifndef _I386
  handle_signal_if_fatal (SIGIOINT);
#endif
  handle_signal_if_fatal (SIGGRANT);
  handle_signal_if_fatal (SIGRETRACT);
  handle_signal_if_fatal (SIGSOUND);
  handle_signal_if_fatal (SIGMSG);
#endif /* AIX */

#ifdef SIGDANGER
  /* This just means available memory is getting low.  */
  EMACS_SIGNAL (SIGDANGER, memory_warning_signal);
#endif
}

void
syms_of_signal (void)
{
  DEFSUBR (Fwaiting_for_user_input_p);
}

void
init_interrupts_late (void)
{
  if (!noninteractive)
    {
      EMACS_SIGNAL (SIGINT, interrupt_signal);
#ifdef HAVE_TERMIO
      /* On  systems with TERMIO, C-g is set up for both SIGINT and SIGQUIT
	 and we can't tell which one it will give us.  */
      EMACS_SIGNAL (SIGQUIT, interrupt_signal);
#endif /* HAVE_TERMIO */
      init_async_timeouts ();
#ifdef SIGIO
      EMACS_SIGNAL (SIGIO, input_available_signal);
# ifdef SIGPOLL /* XPG5 */
      /* Some systems (e.g. Motorola SVR4) losingly have different
	 values for SIGIO and SIGPOLL, and send SIGPOLL instead of
	 SIGIO.  On those same systems, an uncaught SIGPOLL kills the
	 process. */
      EMACS_SIGNAL (SIGPOLL, input_available_signal);
# endif
#elif !defined (DONT_POLL_FOR_QUIT)
      init_poll_for_quit ();
#endif
    }

#if defined (HAVE_UNIX_PROCESSES) && !defined (SIGCHLD)
  init_poll_for_sigchld ();
#endif

  EMACS_UNBLOCK_ALL_SIGNALS ();

  interrupts_initted = 1;
}

