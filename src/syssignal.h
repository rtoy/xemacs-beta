/* syssignal.h - System-dependent definitions for signals.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
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

/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_syssignal_h_
#define INCLUDED_syssignal_h_

/* In the old world, one could not #include <signal.h> here.  The party line
   was that that header should always be #included before <config.h>, because
   some configuration files (like s/hpux.h) indicate that SIGIO doesn't work
   by #undef-ing SIGIO, and if this file #includes <signal.h>, then that will
   re-#define SIGIO and confuse things.

   This was, however, a completely fucked up state of affairs, because on some
   systems it's necessary for the s/m files to #define things in order to get
   <signal.h> to provide the right typedefs, etc.  And it's generally a broken
   concept for <config.h> to not be the very very first file included.

   So instead of #undef'ing SIGIO in the various s/m files, I've changed them
   to define BROKEN_SIGIO instead, then we (syssignal.h) do an #undef SIGIO
   at the end, after including signal.h.  Therefore, it's important that
   <signal.h> not be included after "syssignal.h", but that's the normal state:
   nothing should be directly including <signal.h> these days.
							-- jwz, 29-nov-93
 */

#include <signal.h>
#include <errno.h>

/* SIGPOLL is the SVR4 signal.  Those systems generally define
   SIGIO as an alias for SIGPOLL, but just in case ... */

#if defined (BROKEN_SIGIO)
#  if defined (SIGIO) && defined (SIGPOLL)
#    if SIGIO == SIGPOLL
#      undef SIGIO
#      undef SIGPOLL
#    else
#      undef SIGIO
#    endif
#  endif
#else /* Not BROKEN_SIGIO */
#  if !defined (SIGIO) && defined (SIGPOLL)
#    define SIGIO SIGPOLL
#  endif
#endif

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */
#if defined (SIGCLD) && !defined (SIGCHLD)
# define SIGCHLD SIGCLD
#endif /* SIGCHLD */

#ifdef BROKEN_SIGCHLD
#undef SIGCHLD
#endif

#ifdef SIGCHLD
#define EMACS_BLOCK_SIGCHLD EMACS_BLOCK_SIGNAL (SIGCHLD)
#define EMACS_UNBLOCK_SIGCHLD EMACS_UNBLOCK_SIGNAL (SIGCHLD)
#else
#define EMACS_BLOCK_SIGCHLD
#define EMACS_UNBLOCK_SIGCHLD
#endif

/* According to W.R. Stevens __Advanced Programming in the Unix
   Environment__, there are four different paradigms for handling
   signals.  We use autoconf to tell us which one applies.

   Note that on some systems, more than one paradigm is implemented
   (typically, the POSIX sigaction/sigprocmask and either the older
   SYSV or BSD way).  In such a case, we prefer the POSIX way.

   We used to say this:

   [[ NOTE: We use EMACS_* macros for most signal operations, but
   just signal() for the standard signal-setting operation.
   Perhaps we should change this to EMACS_SIGNAL(), but that runs
   the risk of someone forgetting this convention and calling
   signal() directly. ]]

   But current policy is to avoid playing with macros as much as
   possible, since in the long run it really just ends up creating
   unmaintainable code -- someone newly reading the code is never
   going to realize exactly which calls are redirected, and on
   which systems, and where the redirection occurs.

   Possibly we should use the new "qxe" convention.
*/

#ifndef NeXT
typedef RETSIGTYPE (XCDECL * signal_handler_t) (int);
#endif

#if defined (HAVE_SIGPROCMASK)

/* The POSIX way (sigaction, sigprocmask, sigpending, sigsuspend) */

signal_handler_t qxe_reliable_signal (int signal_number,
				      signal_handler_t action);

#define EMACS_SIGNAL qxe_reliable_signal

#define EMACS_BLOCK_SIGNAL(sig) do		\
{						\
  sigset_t ES_mask;				\
  sigemptyset (&ES_mask);			\
  sigaddset (&ES_mask, sig);			\
  sigprocmask (SIG_BLOCK, &ES_mask, NULL);	\
} while (0)
#define EMACS_UNBLOCK_SIGNAL(sig) do		\
{						\
  sigset_t ES_mask;				\
  sigemptyset (&ES_mask);			\
  sigaddset (&ES_mask, sig);			\
  sigprocmask (SIG_UNBLOCK, &ES_mask, NULL);	\
} while (0)
#define EMACS_UNBLOCK_ALL_SIGNALS() do		\
{						\
  sigset_t ES_mask;				\
  sigemptyset (&ES_mask);			\
  sigprocmask (SIG_SETMASK, &ES_mask, NULL);	\
} while (0)
#define EMACS_WAIT_FOR_SIGNAL(sig) do		\
{						\
  sigset_t ES_mask;				\
  sigprocmask (0, NULL, &ES_mask);		\
  sigdelset (&ES_mask, sig);			\
  sigsuspend (&ES_mask);			\
} while (0)
#define EMACS_REESTABLISH_SIGNAL(sig, handler)
#define SIG_ARG_MAYBE_UNUSED(decl) UNUSED (decl)

#elif defined (HAVE_SIGBLOCK)

/* The older BSD way (signal/sigvec, sigblock, sigsetmask, sigpause) */

/* It's OK to use signal() here directly.  No unreliable signal
   problems.  However, we use sigvec() because it allows us to
   request interruptible I/O. */

#define EMACS_SIGNAL qxe_reliable_signal

/* Is it necessary to define sigmask like this? */
#ifndef sigmask
# define sigmask(no) (1L << ((no) - 1))
#endif

#define EMACS_BLOCK_SIGNAL(sig) sigblock (sigmask (sig))
#define EMACS_UNBLOCK_SIGNAL(sig) sigsetmask (sigblock (0) & ~sigmask (sig))
#define EMACS_UNBLOCK_ALL_SIGNALS() sigsetmask (0)
#define EMACS_WAIT_FOR_SIGNAL(sig) do		\
{						\
  int ES_mask = sigblock (0);			\
  sigpause (ES_mask & ~sigmask (sig));		\
} while (0)
#define EMACS_REESTABLISH_SIGNAL(sig, handler)
#define SIG_ARG_MAYBE_UNUSED(decl) UNUSED (decl)

#elif defined (HAVE_SIGHOLD)

/* The older SYSV way (signal/sigset, sighold, sigrelse, sigignore,
   sigpause) */

#define EMACS_SIGNAL sigset
#define EMACS_BLOCK_SIGNAL(sig) sighold (sig)
#define EMACS_UNBLOCK_SIGNAL(sig) sigrelse (sig)
/* #### There's not really any simple way to implement this.
   Since EMACS_UNBLOCK_ALL_SIGNALS() is only called once (at startup),
   it's probably OK to just ignore it. */
#define EMACS_UNBLOCK_ALL_SIGNALS() 0
#define EMACS_WAIT_FOR_SIGNAL(sig) sigpause (sig)
#define EMACS_REESTABLISH_SIGNAL(sig, handler)
#define SIG_ARG_MAYBE_UNUSED(decl) UNUSED (decl)

#elif defined (WIN32_NATIVE)

/* MS Windows signal emulation (in turns emulates the sigset/sighold
   paradigm) */

#define EMACS_SIGNAL mswindows_sigset
#define EMACS_BLOCK_SIGNAL(sig) mswindows_sighold (sig)
#define EMACS_UNBLOCK_SIGNAL(sig) mswindows_sigrelse (sig)
/* #### There's not really any simple way to implement this.
   Since EMACS_UNBLOCK_ALL_SIGNALS() is only called once (at startup),
   it's probably OK to just ignore it. */
#define EMACS_UNBLOCK_ALL_SIGNALS() 0
#define EMACS_WAIT_FOR_SIGNAL(sig) mswindows_sigpause (sig)
#define EMACS_REESTABLISH_SIGNAL(sig, handler)
#define SIG_ARG_MAYBE_UNUSED(decl) UNUSED (decl)

/* Defines that we need that aren't in the standard signal.h  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGKILL 9               /* Die, die die */
#define SIGALRM 14              /* Alarm */
#define SIGPROF 29		/* Profiling timer exp */

#else

/* The oldest SYSV way (signal only; unreliable signals) */

/* Old USG systems don't really have signal blocking.
   We indicate this by not defining EMACS_BLOCK_SIGNAL or
   EMACS_WAIT_FOR_SIGNAL. */
#define EMACS_SIGNAL signal
#define EMACS_UNBLOCK_SIGNAL(sig) 0
#define EMACS_UNBLOCK_ALL_SIGNALS() 0
#define EMACS_REESTABLISH_SIGNAL(sig, handler) do	\
{							\
  int old_errno = errno;				\
  signal (sig, handler);				\
  errno = old_errno;					\
} while (0)
#define SIG_ARG_MAYBE_UNUSED(decl) decl

/* Under SYSV, setting a signal handler for SIGCLD causes
   SIGCLD to immediately be sent if there any unwaited processes
   out there.  This means that the SIGCLD handler *must* call
   wait() to reap the status of all processes -- it cannot
   simply set a flag and then reestablish the handler, because
   it will get called again, infinitely.  We only need to
   worry about this on systems where signals need to be
   reestablished (SYSV Release 2 and earlier). */
#define OBNOXIOUS_SYSV_SIGCLD_BEHAVIOR

#endif /* different signalling methods */

/* On bsd, [man says] kill does not accept a negative number to kill a pgrp.
   Must do that using the killpg call.  */
#ifdef HAVE_KILLPG
#define EMACS_KILLPG(pid, signo) killpg (pid, signo)
#elif defined (WIN32_NATIVE)
#define EMACS_KILLPG(pid, signo) should never be called
#else
#define EMACS_KILLPG(pid, signo) kill (-(pid), signo)
#endif

#ifndef NSIG
# ifdef USG5_4
/* Some SVr4s don't define NSIG in sys/signal.h for ANSI environments;
 * instead, there's a system variable _sys_nsig.  Unfortunately, we need the
 * constant to dimension an array.  So wire in the appropriate value here.
 */
#  define NSIG 32
# else
#  define NSIG (SIGUSR2+1) /* guess how many elements are in sys_siglist... */
# endif
#endif

/* HAVE_DECL_SYS_SIGLIST is determined by configure.  On Linux, it seems,
   configure incorrectly fails to find it, so s/linux.h defines
   HAVE_SYS_SIGLIST. */
#if !defined (HAVE_DECL_SYS_SIGLIST) && !defined (HAVE_SYS_SIGLIST)
extern const char *sys_siglist[];
#endif

#ifdef SIGDANGER
SIGTYPE memory_warning_signal (int sig);
#endif

#if defined (WIN32_NATIVE) || defined (CYGWIN_BROKEN_SIGNALS)
typedef void (__cdecl *mswindows_sighandler) (int);

/* Prototypes for signal functions, see win32.c */
int mswindows_sighold (int nsig);
int mswindows_sigrelse (int nsig);
int mswindows_sigpause (int nsig);
int mswindows_raise (int nsig);
mswindows_sighandler mswindows_sigset (int sig, mswindows_sighandler handler);

#endif /* defined (WIN32_NATIVE) || defined (CYGWIN_BROKEN_SIGNALS) */

signal_handler_t set_timeout_signal (int signal_number,
				     signal_handler_t action);


#endif /* INCLUDED_syssignal_h_ */
