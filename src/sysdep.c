/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985-1988, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2010 Ben Wing.

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


/* Synched up with: FSF 19.30 except for some Windows-NT crap. */

/* Authorship:

   Current primary author: Various

   Originally from FSF.  Major changes at various times.
   Substantially cleaned up by Ben Wing, Dec. 1994 / Jan. 1995.
   SIGIO stuff ripped apart and redone by Ben Wing. (during 19.14 devel?)
   Signal stuff totally redone by Ben Wing. (during 19.14 devel?  that would
     be Dec 1995 - Apr 1996.)
   Controlling terminal stuff redone by Ben Wing for 19.13.
   System call encapsulation stuff written by Ben Wing for 19.12. (1995)
   Ripped up and redone avoiding preprocessor tricks Aug - Sep 2001 during
   Mule-on-Windows development.
   */

#include <config.h>
#include "lisp.h"

/* ------------------------------- */
/*          basic includes         */
/* ------------------------------- */


#include "buffer.h"
#include "device-impl.h"
#include "events.h"
#include "frame.h"
#include "process.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#ifdef HAVE_TTY
#include "console-tty-impl.h"
#else
#endif /* HAVE_TTY */

#include "console-stream-impl.h"
#ifdef WIN32_NATIVE
#include "syswindows.h"
#endif

#include "sysdir.h"
#include "sysfile.h"
#include "sysproc.h"
#include "syspwd.h"
#include "syssignal.h"
#include "systime.h"
#include "systty.h"
#include "syswait.h"

#include <setjmp.h>


/* ------------------------------- */
/*         TTY definitions         */
/* ------------------------------- */

#ifdef USG
#include <sys/utsname.h>
#endif /* USG */

/* LPASS8 is new in 4.3, and makes cbreak mode provide all 8 bits.  */
#ifndef LPASS8
#define LPASS8 0
#endif

#ifndef HAVE_H_ERRNO
int h_errno;
#endif

#ifdef HAVE_TTY

static int baud_convert[] =
#ifdef BAUD_CONVERT
  BAUD_CONVERT;
#else
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };
#endif

#endif


/************************************************************************/
/*                         subprocess control                           */
/************************************************************************/

#ifdef NEED_SYNC_PROCESS_CODE

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
volatile int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
const char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;

#endif /* NEED_SYNC_PROCESS_CODE */

#ifdef HAVE_TTY

#ifdef SIGTSTP

/* Arrange for character C to be read as the next input from
   the terminal.  */
void
stuff_char (struct console *con,
#ifdef TIOCSTI
	    int c
#else
	    int UNUSED (c)
#endif
	    )
{
  int input_fd;

  assert (CONSOLE_TTY_P (con));
  input_fd = CONSOLE_TTY_DATA (con)->infd;
/* Should perhaps error if in batch mode */
#ifdef TIOCSTI
  ioctl (input_fd, TIOCSTI, &c);
#else /* no TIOCSTI */
  invalid_operation ("Cannot stuff terminal input characters in this version of Unix.", Qunbound);
#endif /* no TIOCSTI */
}

#endif /* SIGTSTP */

#endif /* HAVE_TTY */

void
set_exclusive_use (
#ifdef FIOCLEX
		   int fd
#else
		   int UNUSED (fd)
#endif
		   )
{
#ifdef FIOCLEX
  ioctl (fd, FIOCLEX, 0);
#endif
  /* Ok to do nothing if this feature does not exist */
}

void
set_descriptor_non_blocking (
#if defined (STRIDE) || (defined (pfa) && defined (HAVE_PTYS)) || defined (AIX) || defined (F_SETFL)
			     int fd
#else
			     int UNUSED (fd)
#endif
			     )
{
/* Stride people say it's a mystery why this is needed
   as well as the O_NDELAY, but that it fails without this.  */
  /* For AIX: Apparently need this for non-blocking reads on sockets.
     It seems that O_NONBLOCK applies only to FIFOs?  From
     lowry@watson.ibm.com (Andy Lowry). */
  /* #### Should this be conditionalized on FIONBIO? */
#if defined (STRIDE) || (defined (pfa) && defined (HAVE_PTYS)) || defined (AIX)
  {
    int one = 1;
    ioctl (fd, FIONBIO, &one);
  }
#endif

#ifdef F_SETFL
  fcntl (fd, F_SETFL, O_NONBLOCK);
#endif
}

#ifdef NEED_SYNC_PROCESS_CODE /* #### Used only on super-ancient systems */

static void
wait_for_termination (int pid)
{
  /* #### With the new improved SIGCHLD handling stuff, there is much
     less danger of race conditions and some of the comments below
     don't apply.  This should be updated. */

#if defined (HAVE_WAITPID)
  /* Note that, whenever any subprocess terminates (asynch. or synch.),
     the SIGCHLD handler will be called and it will call wait().  Thus
     we cannot just call wait() ourselves, and we can't block SIGCHLD
     and then call wait(), because then if an asynch.  process dies
     while we're waiting for our synch. process, Emacs will never
     notice that the asynch. process died.

     So, the general approach we take is to repeatedly block until a
     signal arrives, and then check if our process died using kill
     (pid, 0).  (We could also check the value of `synch_process_alive',
     since the SIGCHLD handler will reset that and we know that we're
     only being called on synchronous processes, but this approach is
     safer.  I don't trust the proper delivery of SIGCHLD.

     Note also that we cannot use any form of waitpid().  A loop with
     WNOHANG will chew up CPU time; better to use sleep().  A loop
     without WNOWAIT will screw up the SIGCHLD handler (actually this
     is not true, if you duplicate the exit-status-reaping code; see
     below).  A loop with WNOWAIT will result in a race condition if
     the process terminates between the process-status check and the
     call to waitpid(). */

  /* Formerly, immediate_quit was set around this function call, but
     that could lead to problems if the QUIT happened when SIGCHLD was
     blocked -- it would remain blocked.  Yet another reason why
     immediate_quit is a bad idea.  In any case, there is no reason to
     resort to this because either the SIGIO or the SIGALRM will stop
     the block in EMACS_WAIT_FOR_SIGNAL(). */

  /* Apparently there are bugs on some systems with the second method
     used below (the EMACS_BLOCK_SIGNAL method), whereby zombie
     processes get left around.  It appears in those cases that the
     SIGCHLD handler is never getting invoked.  It's not clear whether
     this is an Emacs bug or a kernel bug or both: on HPUX this
     problem is observed only with XEmacs, but under Solaris 2.4 all
     sorts of different programs have problems with zombies.  The
     method we use here does not require a working SIGCHLD (but will
     not break if it is working), and should be safe. */
  /*
     We use waitpid(), contrary to the remarks above.  There is no
     race condition, because the three situations when sigchld_handler
     is invoked should be handled OK:

     - handler invoked before waitpid(): In this case, subprocess
       status will be set by sigchld_handler.  waitpid() here will
       return -1 with errno set to ECHILD, which is a valid exit
       condition.

     - handler invoked during waitpid(): as above, except that errno
       here will be set to EINTR.  This will cause waitpid() to be
       called again, and this time it will exit with ECHILD.

     - handler invoked after waitpid(): The following code will reap
       the subprocess. In the handler, wait() will return -1 because
       there is no child to reap, and the handler will exit without
       modifying child subprocess status.  */
  int ret, status;

  /* Because the SIGCHLD handler can potentially reap the synchronous
     subprocess, we should take care of that.  */

  /* Will stay in the do loop as long as:
     1. Process is alive
     2. Ctrl-G is not pressed */
  do
    {
      QUIT;
      ret = waitpid (pid, &status, 0);
      /* waitpid returns 0 if the process is still alive. */
    }
  while (ret == 0 || (ret == -1 && errno == EINTR));

  if (ret == pid) /* Success */
    /* Set synch process globals.  This is can also happen
       in sigchld_handler, and that code is duplicated. */
    {
      synch_process_alive = 0;
      if (WIFEXITED (status))
	synch_process_retcode = WEXITSTATUS (status);
      else if (WIFSIGNALED (status))
	synch_process_death = signal_name (WTERMSIG (status));
    }
  /* On exiting the loop, ret will be -1, with errno set to ECHILD if
     the child has already been reaped, e.g. in the signal handler.  */

  /* Otherwise, we've had some error condition here.
     Per POSIX, the only other possibilities are:
     - EFAULT (bus error accessing arg 2) or
     - EINVAL (incorrect arguments),
     which are both program bugs.

     Since implementations may add their own error indicators on top,
     we ignore it by default.  */
#elif defined (EMACS_BLOCK_SIGNAL) && !defined (BROKEN_WAIT_FOR_SIGNAL) && defined (SIGCHLD)
  while (1)
    {
      static int wait_debugging = 0; /* Set nonzero to make following
                           function work under dbx (at least for bsd).  */
      QUIT;
      if (wait_debugging)
	return;

      EMACS_BLOCK_SIGNAL (SIGCHLD);
      /* Block SIGCHLD from happening during this check,
	 to avoid race conditions. */
      if (kill (pid, 0) < 0)
	{
	  EMACS_UNBLOCK_SIGNAL (SIGCHLD);
	  return;
	}
      else
	/* WARNING: Whatever this macro does *must* not allow SIGCHLD
	   to happen between the time that it's reenabled and when we
	   begin to block.  Otherwise we may end up blocking for a
	   signal that has already arrived and isn't coming again.
	   Can you say "race condition"?

	   I assume that the system calls sigpause() or sigsuspend()
	   to provide this atomicness.  If you're getting hangs in
	   sigpause()/sigsuspend(), then your OS doesn't implement
	   this properly (this applies under hpux9, for example).
	   Try defining BROKEN_WAIT_FOR_SIGNAL. */
	EMACS_WAIT_FOR_SIGNAL (SIGCHLD);
    }
#else /* not HAVE_WAITPID and (not EMACS_BLOCK_SIGNAL or BROKEN_WAIT_FOR_SIGNAL) */
  /* This approach is kind of cheesy but is guaranteed(?!) to work
     for all systems. */
  while (1)
    {
      QUIT;
      if (kill (pid, 0) < 0)
	return;
      stop_interrupts ();
      sleep (1);
      start_interrupts ();
    }
#endif /* OS features */
}

#endif /* NEED_SYNC_PROCESS_CODE */

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */

void
flush_pending_output (
#if !defined (HAVE_TERMIOS) && (defined (TCFLSH) || defined (TIOCFLUSH))
		      int channel
#else
		      int UNUSED (channel)
#endif
		      )
{
#ifdef HAVE_TERMIOS
  /* If we try this, we get hit with SIGTTIN, because
     the child's tty belongs to the child's pgrp. */
#elif defined (TCFLSH)
  ioctl (channel, TCFLSH, 1);
#elif defined (TIOCFLUSH)
  int zero = 0;
  /* 3rd arg should be ignored
     but some 4.2 kernels actually want the address of an int
     and nonzero means something different.  */
  ioctl (channel, TIOCFLUSH, &zero);
#endif
}

#ifndef WIN32_NATIVE
/*  Set up the terminal at the other end of a pseudo-terminal that
    we will be controlling an inferior through.
    It should not echo or do line-editing, since that is done
    in Emacs.  No padding needed for insertion into an Emacs buffer.  */

void
child_setup_tty (int out)
{
  struct emacs_tty s;
  emacs_get_tty (out, &s);

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  assert (isatty(out));
  s.main.c_oflag |= OPOST;	/* Enable output postprocessing */
  s.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */

  {
    /* Disable all output delays. */
    tcflag_t delay_mask = 0;
#ifdef NLDLY
    delay_mask |= NLDLY;
#endif
#ifdef CRDLY
    delay_mask |= CRDLY;
#endif
#ifdef TABDLY
    delay_mask |= TABDLY;	/* Also disables tab expansion (Posix). */
#endif
#ifdef BSDLY
    delay_mask |= BSDLY;
#endif
#ifdef VTDLY
    delay_mask |= VTDLY;
#endif
#ifdef FFDLY
    delay_mask |= FFDLY;
#endif
    s.main.c_oflag &= ~delay_mask;
  }

#ifdef OXTABS
  /* Posix defines the TAB3 value for TABDLY to mean: expand tabs to spaces.
     On those systems tab expansion would be disabled by the above code.
     BSD systems use an independent flag, OXTABS. */
  s.main.c_oflag &= ~OXTABS;	/* Disable tab expansion */
#endif

  s.main.c_lflag &= ~ECHO;	/* Disable echo */
  s.main.c_lflag |= ISIG;	/* Enable signals */
#ifdef IUCLC
  s.main.c_iflag &= ~IUCLC;     /* Disable downcasing on input.  */
#endif
#ifdef OLCUC
  s.main.c_oflag &= ~OLCUC;	/* Disable upcasing on output.  */
#endif

#if defined (CSIZE) && defined (CS8)
  s.main.c_cflag = (s.main.c_cflag & ~CSIZE) | CS8; /* Don't strip 8th bit */
#endif
#ifdef ISTRIP
  s.main.c_iflag &= ~ISTRIP;    /* Don't strip 8th bit on input */
#endif
#if 0
  /* Unnecessary as long as ICANON is set */
  s.main.c_cc[VMIN]  = 1;	/* minimum number of characters to accept  */
  s.main.c_cc[VTIME] = 0;	/* wait forever for at least 1 character  */
#endif /* 0 */

  s.main.c_lflag |= ICANON;	/* Enable erase/kill and eof processing */
  s.main.c_cc[VEOF] = 04;	/* ensure that EOF is Control-D */
  s.main.c_cc[VERASE] = _POSIX_VDISABLE; /* disable erase processing */
  s.main.c_cc[VKILL]  = _POSIX_VDISABLE; /* disable kill processing */

#ifdef HPUX
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* HPUX */

#ifdef AIX
#ifndef IBMR2AIX
  /* AIX enhanced edit loses NULs, so disable it. */
  s.main.c_line = 0;
  s.main.c_iflag &= ~ASCEDIT;
#endif /* IBMR2AIX */
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.
     This really serves a purpose only if running in an XTERM window
     or via TELNET or the like, but does no harm elsewhere.  */
  s.main.c_iflag &= ~IGNBRK;
  s.main.c_iflag &= ~BRKINT;
#endif /* AIX */
#ifdef SIGNALS_VIA_CHARACTERS
  /* TTY `special characters' are used in process_send_signal
     so set them here to something useful.  */
  s.main.c_cc[VQUIT] = '\\'&037; /* Control-\ */
  s.main.c_cc[VINTR] = 'C' &037; /* Control-C */
  s.main.c_cc[VSUSP] = 'Z' &037; /* Control-Z */
#else /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  /* TTY `special characters' work better as signals, so disable
     character forms */
  s.main.c_cc[VQUIT] = _POSIX_VDISABLE;
  s.main.c_cc[VINTR] = _POSIX_VDISABLE;
  s.main.c_cc[VSUSP] = _POSIX_VDISABLE;
  s.main.c_lflag &= ~ISIG;
#endif /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  s.main.c_cc[VEOL] = _POSIX_VDISABLE;
#if defined (CBAUD)
  /* <mdiers> #### This is not portable. ###
     POSIX does not specify CBAUD, and 4.4BSD does not have it.
     Instead, POSIX suggests to use cfset{i,o}speed().
     [cf. D. Lewine, POSIX Programmer's Guide, Chapter 8: Terminal
     I/O, O'Reilly 1991] */
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#else
  /* <mdiers> What to do upon failure? Just ignoring rc is probably
     not acceptable, is it? */
  if (cfsetispeed (&s.main, B9600) == -1) /* ignore */;
  if (cfsetospeed (&s.main, B9600) == -1) /* ignore */;
#endif /* defined (CBAUD) */

#else /* not HAVE_TERMIO */

  s.main.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE
		       | CBREAK | TANDEM);
  s.main.sg_flags |= LPASS8;
  s.main.sg_erase = 0377;
  s.main.sg_kill  = 0377;
  s.lmode = LLITOUT | s.lmode;        /* Don't strip 8th bit */

#endif /* not HAVE_TERMIO */
  emacs_set_tty (out, &s, 0);
}
#endif /* WIN32_NATIVE */


#if !defined (SIGTSTP)

#define SIG_PARAM_TYPE int

/* Record a signal code and the handler for it.  */
struct save_signal
{
  int code;
  RETSIGTYPE (XCDECL * handler) (SIG_PARAM_TYPE);
};

static void
save_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      saved_handlers->handler
	= (RETSIGTYPE (XCDECL *) (SIG_PARAM_TYPE)) EMACS_SIGNAL (saved_handlers->code, SIG_IGN);
      saved_handlers++;
    }
}

static void
restore_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      EMACS_SIGNAL (saved_handlers->code, saved_handlers->handler);
      saved_handlers++;
    }
}

/* Fork a subshell.  */
static void
sys_subshell (void)
{
  Lisp_Object dir;
  Ibyte *str = 0;
  Bytecount len;
  struct gcpro gcpro1;
  Ibyte *sh = 0;
  Extbyte *shext;

  /* Use our buffer's default directory for the subshell.  */

  /* Note: These calls are spread out to insure that the return values
     of the calls (which may be newly-created strings) are properly
     GC-protected. */

  GCPRO1 (dir);

  dir = current_buffer->directory;
  /* If the current dir has no terminating slash, we'll get undesirable
     results, so put the slash back. */
  dir = Ffile_name_as_directory (dir);
  dir = Funhandled_file_name_directory (dir);
  dir = expand_and_dir_to_file (dir, Qnil);

  str = alloca_ibytes (XSTRING_LENGTH (dir) + 2);
  len = XSTRING_LENGTH (dir);
  memcpy (str, XSTRING_DATA (dir), len);
  if (!IS_ANY_SEP (str[len - 1]))
    str[len++] = DIRECTORY_SEP;
  str[len] = 0;

  if (sh == 0)
    sh = egetenv ("SHELL");
  if (sh == 0)
    sh = (Ibyte *) "sh";

  PATHNAME_CONVERT_OUT (sh, shext);

  UNGCPRO;

#ifdef WIN32_NATIVE

  if (str)
    qxe_chdir (str);

  /* Waits for process completion */
  if (XEUNICODE_P ?
      _wspawnlp (_P_WAIT, (const wchar_t *) shext,
		 (const wchar_t *) shext, NULL) != 0 :
      _spawnlp (_P_WAIT, shext, shext, NULL) != 0)
    report_process_error ("Can't spawn subshell", Qunbound);
  else
    return; /* we're done, no need to wait for termination */

#else /* not WIN32_NATIVE */

  {
    int pid;
    struct save_signal saved_handlers[5];

    saved_handlers[0].code = SIGINT;
    saved_handlers[1].code = SIGQUIT;
    saved_handlers[2].code = SIGTERM;
#ifdef SIGIO
    saved_handlers[3].code = SIGIO;
    saved_handlers[4].code = 0;
#else
    saved_handlers[3].code = 0;
#endif

    pid = fork ();

    if (pid == -1)
      report_process_error ("Can't spawn subshell", Qunbound);
    if (pid == 0)
      {
	if (str)
	  qxe_chdir (str);

#if !defined (NO_SUBPROCESSES)
	close_process_descs (); /* Close Emacs's pipes/ptys */
#endif

#ifdef SET_EMACS_PRIORITY
	if (emacs_priority != 0)
	  nice (-emacs_priority); /* Give the new shell the default priority */
#endif

	execlp (shext, shext, 0);
	retry_write (1, "Can't execute subshell", 22);
	_exit (1);
      }

    save_signal_handlers (saved_handlers);
    synch_process_alive = 1;
    wait_for_termination (pid);
    restore_signal_handlers (saved_handlers);
  }

#endif /* not WIN32_NATIVE */
}

#endif /* !defined (SIGTSTP) */



/* Suspend the Emacs process; give terminal to its superior.  */
void
sys_suspend (void)
{
#if defined (SIGTSTP)
  {
    int pgrp = EMACS_GET_PROCESS_GROUP ();
    EMACS_KILLPG (pgrp, SIGTSTP);
  }

#else /* No SIGTSTP */

  /* On a system where suspending is not implemented,
     instead fork a subshell and let it talk directly to the terminal
     while we wait.  */
  sys_subshell ();

#endif
}

/* Suspend a process if possible; give terminal to its superior.  */
void
sys_suspend_process (
#ifdef SIGTSTP
		     int process
#else
		     int UNUSED (process)
#endif
		     )
{
    /* I don't doubt that it is possible to suspend processes on
     * VMS machines, but I don't know how to do it, so...
     */
#if defined (SIGTSTP)
    kill(process, SIGTSTP);
#endif
}


/* Given FD, obtain pty buffer size. When no luck, a good guess is made,
   so that the function works even when fd is not a pty. */

int
get_pty_max_bytes (
#if defined (HAVE_FPATHCONF) && defined (_PC_MAX_CANON)
		   int fd
#else
		   int UNUSED (fd)
#endif
		   )
{
  /* DEC OSF 4.0 fpathconf returns 255, but xemacs hangs on long shell
     input lines if we return 253.  252 is OK!.  So let's leave a bit
     of slack for the newline that xemacs will insert, and for those
     inevitable vendor off-by-one-or-two-or-three bugs. */
#define MAX_CANON_SLACK 10
#define SAFE_MAX_CANON (127 - MAX_CANON_SLACK)
#if defined (HAVE_FPATHCONF) && defined (_PC_MAX_CANON)
  {
    int max_canon = fpathconf (fd, _PC_MAX_CANON);
#ifdef __hpux__
    /* HP-UX 10.20 fpathconf returns 768, but this results in
       truncated input lines, while 255 works. */
    if (max_canon > 255) max_canon = 255;
#endif
    return (max_canon < 0 ? SAFE_MAX_CANON :
	    max_canon > SAFE_MAX_CANON ? max_canon - MAX_CANON_SLACK :
	    max_canon);
  }
#elif defined (_POSIX_MAX_CANON)
  return (_POSIX_MAX_CANON > SAFE_MAX_CANON ?
	  _POSIX_MAX_CANON - MAX_CANON_SLACK :
	  _POSIX_MAX_CANON);
#else
  return SAFE_MAX_CANON;
#endif
}

/* Figure out the eof character for the FD. */

Ibyte
get_eof_char (int fd)
{
  const Ibyte ctrl_d = (Ibyte) '\004';

  if (!isatty (fd))
    return ctrl_d;
#ifdef HAVE_TERMIOS
  {
    struct termios t;
    tcgetattr (fd, &t);
#if 0
    /* What is the following line designed to do??? -mrb */
    if ((int) strlen ((const char *) t.c_cc) < (VEOF + 1))
      return ctrl_d;
    else
      return (Ibyte) t.c_cc[VEOF];
#endif
    return t.c_cc[VEOF] == _POSIX_VDISABLE ? ctrl_d : (Ibyte) t.c_cc[VEOF];
  }
#else /* ! HAVE_TERMIOS */
  /* On Berkeley descendants, the following IOCTL's retrieve the
    current control characters.  */
#if defined (TIOCGETC)
  {
    struct tchars c;
    ioctl (fd, TIOCGETC, &c);
    return (Ibyte) c.t_eofc;
  }
#else /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
  /* On SYSV descendants, the TCGETA ioctl retrieves the current control
     characters.  */
#ifdef TCGETA
  {
    struct termio t;
    ioctl (fd, TCGETA, &t);
    if ((int) strlen ((const char *) t.c_cc) < (VINTR + 1))
      return ctrl_d;
    else
      return (Ibyte) t.c_cc[VINTR];
  }
#else /* ! defined (TCGETA) */
  /* Rather than complain, we'll just guess ^D, which is what
   * earlier emacsen always used. */
  return ctrl_d;
#endif /* ! defined (TCGETA) */
#endif /* ! defined (TIOCGETC) */
#endif /* ! defined (HAVE_TERMIOS) */
}

/* Set the logical window size associated with descriptor FD
   to HEIGHT and WIDTH.  This is used mainly with ptys.  */

int
set_window_size (
#if defined (TIOCSWINSZ) || defined (TIOCSSIZE)
		 int fd, int height, int width
#else
		 int UNUSED (fd), int UNUSED (height), int UNUSED (width)
#endif
		 )
{
#ifdef TIOCSWINSZ

  /* BSD-style.  */
  struct winsize size;
  size.ws_row = height;
  size.ws_col = width;

  if (ioctl (fd, TIOCSWINSZ, &size) == -1)
    return 0; /* error */
  else
    return 1;

#elif defined (TIOCSSIZE)

  /* SunOS - style.  */
  struct ttysize size;
  size.ts_lines = height;
  size.ts_cols = width;

  if (ioctl (fd, TIOCGSIZE, &size) == -1)
    return 0;
  else
    return 1;
#else
  return -1;
#endif
}

/* Set up the proper status flags for use of a pty.  */

void
setup_pty (
#ifdef TIOCPKT
	   int fd
#else
	   int UNUSED (fd)
#endif
	   )
{
#ifdef TIOCPKT
  /* In some systems (Linux through 2.0.0, at least), packet mode doesn't
     get cleared when a pty is closed, so we need to clear it here.
     Linux pre2.0.13 contained an attempted fix for this (from Ted Ts'o,
     tytso@mit.edu), but apparently it messed up rlogind and telnetd, so he
     removed the fix in pre2.0.14.     - dkindred@cs.cmu.edu
   */
  {
    int off = 0;
    ioctl (fd, TIOCPKT, (char *)&off);
  }
#endif /* TIOCPKT */
}


/************************************************************************/
/*                            TTY control                               */
/************************************************************************/

/* ------------------------------------------------------ */
/*                     get baud rate                      */
/* ------------------------------------------------------ */

/* It really makes more sense for the baud-rate to be console-specific
   and not device-specific, but it's (at least potentially) used for output
   decisions. */

void
init_baud_rate (struct device *d)
{
  if (DEVICE_WIN_P (d) || DEVICE_STREAM_P (d))
    {
      DEVICE_BAUD_RATE (d) = 38400;
      return;
    }

#ifdef HAVE_TTY
  assert (DEVICE_TTY_P (d));
  {
    struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
    int input_fd = CONSOLE_TTY_DATA (con)->infd;
#ifdef HAVE_TERMIOS
    struct termios sg;

    sg.c_cflag = B9600;
    tcgetattr (input_fd, &sg);
    DEVICE_TTY_DATA (d)->ospeed = cfgetospeed (&sg);
#elif defined (HAVE_TERMIO)
    struct termio sg;

    sg.c_cflag = B9600;
# ifdef HAVE_TCATTR
    tcgetattr (input_fd, &sg);
# else
    ioctl (input_fd, TCGETA, &sg);
# endif
    DEVICE_TTY_DATA (d)->ospeed = sg.c_cflag & CBAUD;
#else /* neither TERMIOS nor TERMIO */
    struct sgttyb sg;

    sg.sg_ospeed = B9600;
    if (ioctl (input_fd, TIOCGETP, &sg) < 0)
      ABORT ();
    DEVICE_TTY_DATA (d)->ospeed = sg.sg_ospeed;
#endif
  }

  DEVICE_BAUD_RATE (d) =
    (DEVICE_TTY_DATA (d)->ospeed < countof (baud_convert)
     ? baud_convert[DEVICE_TTY_DATA (d)->ospeed]
     : 9600);

  if (DEVICE_BAUD_RATE (d) == 0)
    DEVICE_BAUD_RATE (d) = 1200;
#endif /* HAVE_TTY */
}


/* ------------------------------------------------------ */
/*                       SIGIO control                    */
/* ------------------------------------------------------ */

#if defined (SIGIO) && !defined (BROKEN_SIGIO)

static void
init_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

#if defined (FIOSSAIOOWN)
  { /* HPUX stuff */
    int owner = getpid ();
    int ioctl_status;
    if (DEVICE_TTY_P (d))
	{
	  ioctl_status = ioctl (filedesc, FIOGSAIOOWN,
				&DEVICE_OLD_FCNTL_OWNER (d));
	  ioctl_status = ioctl (filedesc, FIOSSAIOOWN, &owner);
	}
#ifdef HAVE_WINDOW_SYSTEM
    else if (!DEVICE_STREAM_P (d))
      {
	ioctl_status = ioctl (filedesc, SIOCGPGRP,
			      &DEVICE_OLD_FCNTL_OWNER (d));
	ioctl_status = ioctl (filedesc, SIOCSPGRP, &owner);
      }
#endif
  }
#elif defined (F_SETOWN) && !defined (F_SETOWN_BUG)
  DEVICE_OLD_FCNTL_OWNER (d) = fcntl (filedesc, F_GETOWN, 0);
  fcntl (filedesc, F_SETOWN, getpid ());
#endif
}

static void
reset_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

#if defined (FIOSSAIOOWN)
  { /* HPUX stuff */
    int ioctl_status;
    if (DEVICE_TTY_P (d))
      {
	ioctl_status = ioctl (filedesc, FIOSSAIOOWN,
			      &DEVICE_OLD_FCNTL_OWNER (d));
      }
#ifdef HAVE_WINDOW_SYSTEM
    else if (!DEVICE_STREAM_P (d))
      {
	ioctl_status = ioctl (filedesc, SIOCSPGRP,
			      &DEVICE_OLD_FCNTL_OWNER (d));
      }
#endif
  }
#elif defined (F_SETOWN) && !defined (F_SETOWN_BUG)
  fcntl (filedesc, F_SETOWN, DEVICE_OLD_FCNTL_OWNER (d));
#endif
}

static void
request_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

  /* NOTE: It appears that Linux has its own mechanism for requesting
     SIGIO, using the F_GETSIG and F_SETSIG commands to fcntl().
     These let you pick which signal you want sent (not just SIGIO),
     and if you do this, you get additional info which tells you which
     file descriptor has input ready on it.  The man page says:

       Using  these  mechanisms,  a  program  can implement fully
       asynchronous I/O without using select(2) or  poll(2)  most
       of the time.

       The  use of O_ASYNC, F_GETOWN, F_SETOWN is specific to BSD
       and Linux.   F_GETSIG  and  F_SETSIG  are  Linux-specific.
       POSIX  has asynchronous I/O and the aio_sigevent structure
       to achieve similar things; these  are  also  available  in
       Linux as part of the GNU C Library (Glibc).

     But it appears that Linux also supports O_ASYNC, so I see no
     particular need to switch. --ben
  */

#if defined (I_SETSIG) && !defined (HPUX11) && !defined (LINUX)
  {
    int events = 0;
    ioctl (filedesc, I_GETSIG, &events);
    ioctl (filedesc, I_SETSIG, events | S_INPUT);
  }
#elif defined (O_ASYNC)
  /* Generally FASYNC and O_ASYNC are both defined, and both equal;
     but let's not depend on that.  O_ASYNC appears to be more
     standard (at least the Linux include files think so), so
     check it first. */
  fcntl (filedesc, F_SETFL, fcntl (filedesc, F_GETFL, 0) | O_ASYNC);
#elif defined (FASYNC)
  fcntl (filedesc, F_SETFL, fcntl (filedesc, F_GETFL, 0) | FASYNC);
#elif defined (FIOSSAIOSTAT)
  {
      /* DG: Changed for HP-UX. HP-UX uses different IOCTLs for
	 sockets and other devices for some bizarre reason. We guess
	 that an X device is a socket, and tty devices aren't. We then
	 use the following crud to do the appropriate thing. */
    int on = 1;
    int ioctl_status;		/* ####DG: check if IOCTL succeeds here. */

    if (DEVICE_TTY_P (d))
      {
	ioctl_status = ioctl (filedesc, FIOSSAIOSTAT, &on);
      }
#ifdef HAVE_WINDOW_SYSTEM
    else if (!DEVICE_STREAM_P (d))
      {
	ioctl_status = ioctl (filedesc, FIOASYNC, &on);
      }
#endif
  }
#elif defined (FIOASYNC)
  {
    int on = 1;
    ioctl (filedesc, FIOASYNC, &on);
  }
#endif
}

static void
unrequest_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

#if defined (I_SETSIG) && !defined (HPUX11) && !defined (LINUX)
  {
    int events = 0;
    ioctl (filedesc, I_GETSIG, &events);
    ioctl (filedesc, I_SETSIG, events & ~S_INPUT);
  }
#elif defined (O_ASYNC)
  fcntl (filedesc, F_SETFL, fcntl (filedesc, F_GETFL, 0) & ~O_ASYNC);
#elif defined (FASYNC)
  fcntl (filedesc, F_SETFL, fcntl (filedesc, F_GETFL, 0) & ~FASYNC);
#elif defined (FIOSSAIOSTAT)
  {
      /* DG: Changed for HP-UX. HP-UX uses different IOCTLs for
	 sockets and other devices for some bizarre reason. We guess
	 that an X device is a socket, and tty devices aren't. We then
	 use the following crud to do the appropriate thing. */

    int off = 0;
    int ioctl_status;

    /* See comment for request_sigio_on_device */

    if (DEVICE_TTY_P (d))
      {
	ioctl_status = ioctl (filedesc, FIOSSAIOSTAT, &off);
      }
    else
      {
	ioctl_status = ioctl (filedesc, FIOASYNC, &off);
      }
  }
#elif defined (FIOASYNC)
  {
    int off = 0;
    ioctl (filedesc, FIOASYNC, &off);
  }
#endif
}

void
request_sigio (void)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d;

      d = XDEVICE (XCAR (devcons));

      if (!DEVICE_STREAM_P (d))
	request_sigio_on_device (d);
    }
}

void
unrequest_sigio (void)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d;

      d = XDEVICE (XCAR (devcons));

      if (!DEVICE_STREAM_P (d))
	unrequest_sigio_on_device (d);
    }
}

#endif /* SIGIO */

/* ------------------------------------------------------ */
/*             Changing Emacs's process group             */
/* ------------------------------------------------------ */

/* Saving and restoring the process group of Emacs's terminal.  */

/* On some systems, apparently (?!) Emacs must be in its own process
   group in order to receive SIGIO correctly.  On other systems
   (e.g. Solaris), it's not required and doing it makes things
   get fucked up.  So, we only do it when
   SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP is defined.  Basically,
   this is only required for BSD 4.2 systems. (Actually, I bet
   we don't have to do this at all -- those systems also
   required interrupt input, which we don't support.)

   If Emacs was in its own process group (i.e. inherited_pgroup ==
   getpid ()), then we know we're running under a shell with job
   control (Emacs would never be run as part of a pipeline).
   Everything is fine.

   If Emacs was not in its own process group, then we know we're
   running under a shell (or a caller) that doesn't know how to
   separate itself from Emacs (like sh).  Emacs must be in its own
   process group in order to receive SIGIO correctly.  In this
   situation, we put ourselves in our own pgroup, forcibly set the
   tty's pgroup to our pgroup, and make sure to restore and reinstate
   the tty's pgroup just like any other terminal setting.  If
   inherited_group was not the tty's pgroup, then we'll get a
   SIGTTmumble when we try to change the tty's pgroup, and a CONT if
   it goes foreground in the future, which is what should happen.  */

#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP

static pid_t inherited_pgroup;
static pid_t inherited_tty_pgroup;

#endif

void
munge_tty_process_group (void)
{
#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
  if (noninteractive)
    return;

  /* Only do this munging if we have a device on the controlling
     terminal.  See the large comment below. */

  if (CONSOLEP (Vcontrolling_terminal) &&
      CONSOLE_LIVE_P (XCONSOLE (Vcontrolling_terminal)))
    {
      int fd = open ("/dev/tty", O_RDWR, 0);
      pid_t me = getpid ();
      EMACS_BLOCK_SIGNAL (SIGTTOU);
      EMACS_SET_TTY_PROCESS_GROUP (fd, &me);
      EMACS_UNBLOCK_SIGNAL (SIGTTOU);
      retry_close (fd);
    }
#endif
}

/* Split off the foreground process group to Emacs alone.
   When we are in the foreground, but not started in our own process
   group, redirect the TTY to point to our own process group.  We need
   to be in our own process group to receive SIGIO properly.  */
static void
munge_process_groups (void)
{
#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
  if (noninteractive)
    return;

  EMACS_SEPARATE_PROCESS_GROUP ();

  munge_tty_process_group ();
#endif
}

void
unmunge_tty_process_group (void)
{
#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
  {
    int fd = open ("/dev/tty", O_RDWR, 0);
    EMACS_BLOCK_SIGNAL (SIGTTOU);
    EMACS_SET_TTY_PROCESS_GROUP (fd, &inherited_tty_pgroup);
    EMACS_UNBLOCK_SIGNAL (SIGTTOU);
    retry_close (fd);
  }
#endif
}

/* Set the tty to our original foreground group.
   Also restore the original process group (put us back into sh's
   process group), so that ^Z will suspend both us and sh. */
static void
unmunge_process_groups (void)
{
#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
  if (noninteractive)
    return;

  unmunge_tty_process_group ();

  EMACS_SET_PROCESS_GROUP (inherited_pgroup);
#endif
}

/* According to some old wisdom, we need to be in a separate process
   group for SIGIO to work correctly (at least on some systems ...).
   So go ahead and put ourselves into our own process group.  This
   will fail if we're already in our own process group, but who cares.
   Also record whether we were in our own process group. (In general,
   we will already be in our own process group if we were started from
   a job-control shell like csh, but not if we were started from sh).

   If we succeeded in changing our process group, then we will no
   longer be in the foreground process group of our controlling
   terminal.  Therefore, if we have a console open onto this terminal,
   we have to change the controlling terminal's foreground process
   group (otherwise we will get stopped with a SIGTTIN signal when
   attempting to read from the terminal).  It's important,
   however, that we do this *only* when we have a console open onto
   the terminal.  It's a decidedly bad idea to do so otherwise,
   especially if XEmacs was started from the background. */

void
init_process_group (void)
{
#ifdef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
  if (! noninteractive)
    {
      int fd = open ("/dev/tty", O_RDWR, 0);
      inherited_pgroup = EMACS_GET_PROCESS_GROUP ();
      EMACS_GET_TTY_PROCESS_GROUP (fd, &inherited_tty_pgroup);
      retry_close (fd);
      EMACS_SEPARATE_PROCESS_GROUP ();
    }
#endif
}

void
disconnect_controlling_terminal (void)
{
#  ifdef HAVE_SETSID
  /* Controlling terminals are attached to a session.
     Create a new session for us; it will have no controlling
     terminal.  This also, of course, puts us in our own
     process group. */
  setsid ();
#  else
  /* Put us in our own process group. */
  EMACS_SEPARATE_PROCESS_GROUP ();
#    if defined (TIOCNOTTY)
  /* This is the older way of disconnecting the controlling
     terminal, on 4.3 BSD.  We must open /dev/tty; using
     filedesc 0 is not sufficient because it could be
     something else (e.g. our stdin was redirected to
     another terminal).
     */
  {
    int j = open ("/dev/tty", O_RDWR, 0);
    ioctl (j, TIOCNOTTY, 0);
    retry_close (j);
  }
#    endif /* TIOCNOTTY */
  /*
     On systems without TIOCNOTTY and without
     setsid(), we don't need to do anything more to
     disconnect our controlling terminal.  Here is
     what the man page for termio(7) from a SYSV 3.2
     system says:

     "The first terminal file opened by the process group leader
     of a terminal file not already associated with a process
     group becomes the control terminal for that process group.
     The control terminal plays a special role in handling quit
     and interrupt signals, as discussed below.  The control
     terminal is inherited by a child process during a fork(2).
     A process can break this association by changing its process
     group using setpgrp(2)."

     */
#  endif /* not HAVE_SETSID */
}


/* ------------------------------------------------------ */
/*        Getting and setting emacs_tty structures        */
/* ------------------------------------------------------ */

/* It's wrong to encase these into #ifdef HAVE_TTY because we need
   them for child TTY processes.  */
/* However, this does break NT support while we don't do child TTY processes */
#ifndef WIN32_NATIVE

/* Set *TC to the parameters associated with the terminal FD.
   Return zero if all's well, or -1 if we ran into an error we
   couldn't deal with.  */
int
emacs_get_tty (int fd, struct emacs_tty *settings)
{
  /* Retrieve the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  /* We have those nifty POSIX tcmumbleattr functions.  */
  if (tcgetattr (fd, &settings->main) < 0)
    return -1;

#elif defined HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, TCGETA, &settings->main) < 0)
    return -1;

#else
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, TIOCGETP, &settings->main) < 0)
    return -1;
#endif /* HAVE_TCATTR */

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCGLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCGETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLGET, &settings->lmode) < 0)
    return -1;
#endif

  /* We have survived the tempest.  */
  return 0;
}

/* Set the parameters of the tty on FD according to the contents of
   *SETTINGS.  If FLUSHP is non-zero, we discard input.
   Return 0 if all went well, and -1 if anything failed.
   #### All current callers use FLUSHP == 0. */

int
emacs_set_tty (int fd, struct emacs_tty *settings, int flushp)
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */
#ifdef HAVE_TCATTR
  int i;
  /* We have those nifty POSIX tcmumbleattr functions.
     William J. Smith <wjs@wiis.wang.com> writes:
     "POSIX 1003.1 defines tcsetattr() to return success if it was
     able to perform any of the requested actions, even if some
     of the requested actions could not be performed.
     We must read settings back to ensure tty setup properly.
     AIX requires this to keep tty from hanging occasionally."  */
  /* This makes sure that we don't loop indefinitely in here.  */
  for (i = 0 ; i < 10 ; i++)
    if (tcsetattr (fd, flushp ? TCSAFLUSH : TCSADRAIN, &settings->main) < 0)
      {
	if (errno == EINTR)
	  continue;
	else
	  return -1;
      }
    else
      {
	struct termios new_;

	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new_);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 * FIXME: Now that aix386 is gone, can we memcmp the whole structure?
	 */
	if (   new_.c_iflag == settings->main.c_iflag
	    && new_.c_oflag == settings->main.c_oflag
	    && new_.c_cflag == settings->main.c_cflag
	    && new_.c_lflag == settings->main.c_lflag
	    && memcmp(new_.c_cc, settings->main.c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }
#elif defined HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, flushp ? TCSETAF : TCSETAW, &settings->main) < 0)
    return -1;

#else
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, (flushp) ? TIOCSETP : TIOCSETN, &settings->main) < 0)
    return -1;
#endif /* HAVE_TCATTR */

  /* Suivant - Do we have to get struct ltchars data?  */
#ifdef HAVE_LTCHARS
  if (ioctl (fd, TIOCSLTC, &settings->ltchars) < 0)
    return -1;
#endif

  /* How about a struct tchars and a wordful of lmode bits?  */
#ifdef HAVE_TCHARS
  if (ioctl (fd, TIOCSETC, &settings->tchars) < 0
      || ioctl (fd, TIOCLSET, &settings->lmode) < 0)
    return -1;
#endif

  /* We have survived the tempest.  */
  return 0;
}

#endif /* WIN32_NATIVE */

/* ------------------------------------------------------ */
/*                 Initializing a device                  */
/* ------------------------------------------------------ */

#ifdef HAVE_TTY

#if defined (TIOCGLTC) && defined (HAVE_LTCHARS) /* HAVE_LTCHARS */
static struct ltchars new_ltchars = {-1,-1,-1,-1,-1,-1};
#endif
#ifdef TIOCGETC /* HAVE_TCHARS */
#ifdef HAVE_TCHARS
static struct tchars new_tchars = {-1,-1,-1,-1,-1,-1};
#endif
#endif

static void
tty_init_sys_modes_on_device (struct device *d)
{
  struct emacs_tty tty;
  int input_fd;
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

  input_fd = CONSOLE_TTY_DATA (con)->infd;

  emacs_get_tty (input_fd, &CONSOLE_TTY_DATA (con)->old_tty);
  tty = CONSOLE_TTY_DATA (con)->old_tty;

  con->tty_erase_char = Qnil;

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  /* after all those years... */
  con->tty_erase_char = make_char (tty.main.c_cc[VERASE]);
  tty.main.c_iflag |= (IGNBRK);	/* Ignore break condition */
  tty.main.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef ISTRIP
  tty.main.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
  tty.main.c_lflag &= ~ECHO;	/* Disable echo */
  tty.main.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
#ifdef IEXTEN
  tty.main.c_lflag &= ~IEXTEN;	/* Disable other editing characters.  */
#endif
  tty.main.c_lflag |= ISIG;	/* Enable signals */
  if (TTY_FLAGS (con).flow_control)
    {
      tty.main.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
      tty.main.c_iflag &= ~IXANY;
#endif /* IXANY */
    }
  else
    tty.main.c_iflag &= ~IXON;	/* Disable start/stop output control */
  tty.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL
				   on output */

#if 0
  /* We used to disable tab expansion here, but this is the user's decision. */
#if defined (TABDLY) && defined (TAB3)
  if ((tty.main.c_oflag & TABDLY) == TAB3)
    tty.main.c_oflag &= ~TABDLY; /* Disable tab expansion (Posix). */
#elif defined (OXTABS)
  tty.main.c_oflag &= ~OXTABS;	 /* Disable tab expansion (BSD). */
#endif
#endif /* 0 */

#ifdef CS8
  if (TTY_FLAGS (con).meta_key)
    {
      tty.main.c_cflag |= CS8;	/* allow 8th bit on input */
      tty.main.c_cflag &= ~PARENB;/* Don't check parity */
    }
#endif
  if (CONSOLE_TTY_DATA (con)->controlling_terminal)
    {
      tty.main.c_cc[VINTR] = /* C-g (usually) gives SIGINT */
	event_to_character (CONSOLE_QUIT_EVENT (con), 0, 1);
      /* Set up C-g for both SIGQUIT and SIGINT.
	 We don't know which we will get, but we handle both alike
	 so which one it really gives us does not matter.  */
      tty.main.c_cc[VQUIT] = tty.main.c_cc[VINTR];
    }
  else
    {
      tty.main.c_cc[VINTR] = _POSIX_VDISABLE;
      tty.main.c_cc[VQUIT] = _POSIX_VDISABLE;
    }
  tty.main.c_cc[VMIN] = 1;	/* Input should wait for at
				   least 1 char */
  tty.main.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
  tty.main.c_cc[VSWTCH] = _POSIX_VDISABLE; /* Turn off shell layering use
					      of C-z */
#endif /* VSWTCH */
  /* There was some conditionalizing here on (mips or TCATTR), but
     I think that's wrong.  There was one report of C-y (DSUSP) not being
     disabled on HP9000s700 systems, and this might fix it. */
#ifdef VSUSP
  tty.main.c_cc[VSUSP] = _POSIX_VDISABLE; /* Turn off mips handling of C-z. */
#endif /* VSUSP */
#ifdef V_DSUSP
  tty.main.c_cc[V_DSUSP] = _POSIX_VDISABLE; /* Turn off mips handling of C-y. */
#endif /* V_DSUSP */
#ifdef VDSUSP /* Some systems have VDSUSP, some have V_DSUSP.  */
  tty.main.c_cc[VDSUSP] = _POSIX_VDISABLE;
#endif /* VDSUSP */
#ifdef VLNEXT
  tty.main.c_cc[VLNEXT] = _POSIX_VDISABLE;
#endif /* VLNEXT */
#ifdef VREPRINT
  tty.main.c_cc[VREPRINT] = _POSIX_VDISABLE;
#endif /* VREPRINT */
#ifdef VWERASE
  tty.main.c_cc[VWERASE] = _POSIX_VDISABLE;
#endif /* VWERASE */
#ifdef VDISCARD
  tty.main.c_cc[VDISCARD] = _POSIX_VDISABLE;
#endif /* VDISCARD */
#ifdef VSTART
  tty.main.c_cc[VSTART] = _POSIX_VDISABLE;
#endif /* VSTART */
#ifdef VSTRT
  tty.main.c_cc[VSTRT] = _POSIX_VDISABLE; /* called VSTRT on some systems */
#endif /* VSTART */
#ifdef VSTOP
  tty.main.c_cc[VSTOP] = _POSIX_VDISABLE;
#endif /* VSTOP */

#ifdef AIX
#ifndef IBMR2AIX
  /* AIX enhanced edit loses NULs, so disable it. */
  tty.main.c_line = 0;
  tty.main.c_iflag &= ~ASCEDIT;
#else
  tty.main.c_cc[VSTRT] = 255;
  tty.main.c_cc[VSTOP] = 255;
  tty.main.c_cc[VSUSP] = 255;
  tty.main.c_cc[VDSUSP] = 255;
#endif /* IBMR2AIX */
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.
     This really serves a purpose only if running in an XTERM window
     or via TELNET or the like, but does no harm elsewhere.  */
  tty.main.c_iflag &= ~IGNBRK;
  tty.main.c_iflag &= ~BRKINT;
#endif /* AIX */
#else /* if not HAVE_TERMIO */
  con->tty_erase_char = make_char (tty.main.sg_erase);
  tty.main.sg_flags &= ~(ECHO | CRMOD | XTABS);
  if (TTY_FLAGS (con).meta_key)
    tty.main.sg_flags |= ANYP;
  /* #### should we be using RAW mode here? */
  tty.main.sg_flags |= /* interrupt_input ? RAW : */ CBREAK;
#endif /* not HAVE_TERMIO */

  /* If going to use CBREAK mode, we must request C-g to interrupt
     and turn off start and stop chars, etc.  If not going to use
     CBREAK mode, do this anyway so as to turn off local flow
     control for user coming over network on 4.2; in this case,
     only t_stopc and t_startc really matter.  */
#ifndef HAVE_TERMIO
#ifdef HAVE_TCHARS
  /* Note: if not using CBREAK mode, it makes no difference how we
     set this */
  tty.tchars = new_tchars;
  tty.tchars.t_intrc = event_to_character (CONSOLE_QUIT_EVENT (con), 0, 1);
  if (TTY_FLAGS (con).flow_control)
    {
      tty.tchars.t_startc = '\021';
      tty.tchars.t_stopc = '\023';
    }

  tty.lmode = LDECCTQ | LLITOUT | LPASS8 | LNOFLSH |
    CONSOLE_TTY_DATA (con)->old_tty.lmode;
#endif /* HAVE_TCHARS */
#endif /* not HAVE_TERMIO */

#ifdef HAVE_LTCHARS
  tty.ltchars = new_ltchars;
#endif /* HAVE_LTCHARS */

  emacs_set_tty (input_fd, &tty, 0);

  /* This code added to insure that, if flow-control is not to be used,
     we have an unlocked terminal at the start. */

#ifdef TCXONC
  if (!TTY_FLAGS (con).flow_control) ioctl (input_fd, TCXONC, 1);
#endif
#ifdef TIOCSTART
  if (!TTY_FLAGS (con).flow_control) ioctl (input_fd, TIOCSTART, 0);
#endif

#if defined (HAVE_TERMIOS)
#ifdef TCOON
  if (!TTY_FLAGS (con).flow_control) tcflow (input_fd, TCOON);
#endif
#endif

  set_tty_modes (con);
}

#endif /* HAVE_TTY */

void
init_one_device (
#if defined(HAVE_TTY) || (defined(SIGIO) && !defined(BROKEN_SIGIO))
		 struct device *d
#else
		 struct device *UNUSED (d)
#endif
		 )
{
#ifdef HAVE_TTY
  if (DEVICE_TTY_P (d))
    tty_init_sys_modes_on_device (d);
#endif
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
  if (!DEVICE_STREAM_P (d))
    {
      init_sigio_on_device (d);
      request_sigio_on_device (d);
    }
#endif
}

void
init_one_console (struct console *con)
{
  Lisp_Object devcons;

  CONSOLE_DEVICE_LOOP (devcons, con)
    {
      struct device *d = XDEVICE (XCAR (devcons));

      init_one_device (d);
    }
}

void
reinit_initial_console (void)
{
  munge_process_groups ();
  if (CONSOLEP (Vcontrolling_terminal) &&
      CONSOLE_LIVE_P (XCONSOLE (Vcontrolling_terminal)))
    init_one_console (XCONSOLE (Vcontrolling_terminal));
}


/* ------------------------------------------------------ */
/*                   Other TTY functions                  */
/* ------------------------------------------------------ */

#ifdef HAVE_TTY

#if 0 /* not currently used */

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */

int
tabs_safe_p (struct device *USED_IF_TTY (d))
{
#ifdef HAVE_TTY
  if (DEVICE_TTY_P (d))
    {
      struct emacs_tty tty;

      emacs_get_tty (DEVICE_INFD (d), &tty);
      return EMACS_TTY_TABS_OK (&tty);
    }
#endif
  return 1;
}

#endif /* 0 */

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

void
get_tty_device_size (struct device *d, int *widthp, int *heightp)
{
  int input_fd = DEVICE_INFD (d);

  assert (DEVICE_TTY_P (d));

#ifdef TIOCGWINSZ
  {
    /* BSD-style.  */
    struct winsize size;

    if (ioctl (input_fd, TIOCGWINSZ, &size) == -1)
      *widthp = *heightp = 0;
    else
      {
	*widthp = size.ws_col;
	*heightp = size.ws_row;
      }
  }
#elif defined TIOCGSIZE
  {
    /* SunOS - style.  */
    struct ttysize size;

    if (ioctl (input_fd, TIOCGSIZE, &size) == -1)
      *widthp = *heightp = 0;
    else
      {
	*widthp = size.ts_cols;
	*heightp = size.ts_lines;
      }
  }
#else /* system doesn't know size */

  *widthp = 0;
  *heightp = 0;

#endif /* not !TIOCGWINSZ */
}

#endif /* HAVE_TTY */


/* ------------------------------------------------------ */
/*                   Is device 8 bit ?			  */
/* ------------------------------------------------------ */

#ifdef HAVE_TTY

int
eight_bit_tty (struct device *d)
{
  struct emacs_tty s;
  int input_fd;
  int eight_bit = 0;

  assert (DEVICE_TTY_P (d));
  input_fd = DEVICE_INFD (d);

  emacs_get_tty (input_fd, &s);

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  eight_bit = (s.main.c_cflag & CSIZE) == CS8;
#else
  eight_bit = 0;	/* I don't know how to do it */
#endif
  return eight_bit;
}

#endif /* HAVE_TTY */


/* ------------------------------------------------------ */
/*                   Resetting a device                   */
/* ------------------------------------------------------ */

#ifdef HAVE_TTY

/* Prepare the terminal for exiting Emacs; move the cursor to the
   bottom of the frame, turn off interrupt-driven I/O, etc.  */
static void
tty_reset_sys_modes_on_device (struct device *d)
{
#if defined (BSD)
  int output_fd;
#endif
  int input_fd;
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

  input_fd = CONSOLE_TTY_DATA (con)->infd;
#if defined (BSD)
  output_fd = CONSOLE_TTY_DATA (con)->outfd;
#endif

  tty_redisplay_shutdown (con);
  /* reset_tty_modes() flushes the connection at its end. */
  reset_tty_modes (con);

#if defined (BSD)
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (output_fd);
#endif

  while (emacs_set_tty (input_fd, &CONSOLE_TTY_DATA (con)->old_tty, 0)
	 < 0 && errno == EINTR)
    ;
}

#endif /* HAVE_TTY */

void
reset_one_device (struct device *d)
{
#ifdef HAVE_TTY
  if (DEVICE_TTY_P (d))
    tty_reset_sys_modes_on_device (d);
  else
#endif
  if (DEVICE_STREAM_P (d))
    fflush (CONSOLE_STREAM_DATA (XCONSOLE (DEVICE_CONSOLE (d)))->out);
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
  if (!DEVICE_STREAM_P (d))
    {
      unrequest_sigio_on_device (d);
      reset_sigio_on_device (d);
    }
#endif
}

void
reset_one_console (struct console *con)
{
  /* Note: this can be called during GC. */
  Lisp_Object devcons;

  CONSOLE_DEVICE_LOOP (devcons, con)
    {
      struct device *d = XDEVICE (XCAR (devcons));

      reset_one_device (d);
    }
}

void
reset_all_consoles (void)
{
  /* Note: this can be called during GC. */
  Lisp_Object concons;

  CONSOLE_LOOP (concons)
    {
      struct console *con = XCONSOLE (XCAR (concons));

      reset_one_console (con);
    }

  unmunge_process_groups ();
}

void
reset_initial_console (void)
{
  if (CONSOLEP (Vcontrolling_terminal) &&
      CONSOLE_LIVE_P (XCONSOLE (Vcontrolling_terminal)))
    reset_one_console (XCONSOLE (Vcontrolling_terminal));
  unmunge_process_groups ();
}


/************************************************************************/
/*                    limits of text/data segments                      */
/************************************************************************/

/* Need start_of_data() as much as possible now, for total_data_usage();
   but with PDUMP and WIN32_NATIVE, can't currently do it. */
#if ! (defined (PDUMP) && defined (WIN32_NATIVE) && defined (SYSTEM_MALLOC))
#define NEED_STARTS
#endif

#ifdef NEED_STARTS
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the start of the text segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See crt0.c for further explanation and _start.
 *
 */

#if !defined (HAVE_TEXT_START) && !defined (PDUMP)

EXTERN_C int _start (void);

char *
start_of_text (void)
{
#ifdef TEXT_START
  return (char *) TEXT_START;
#else
  return (char *) _start;
#endif /* TEXT_START */
}
#endif /* !defined(HAVE_TEXT_START) && !defined(PDUMP) */

/*
 *	Return the address of the start of the data segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See ecrt0.c for further information and definition of data_start.
 *
 *	Apparently, on BSD systems this is etext at startup.  On
 *	USG systems (swapping) this is highly mmu dependent and
 *	is also dependent on whether or not the program is running
 *	with shared text.  Generally there is a (possibly large)
 *	gap between end of text and start of data with shared text.
 *
 *	On Uniplus+ systems with shared text, data starts at a
 *	fixed address.  Each port (from a given oem) is generally
 *	different, and the specific value of the start of data can
 *	be obtained via the UniPlus+ specific "uvar" system call,
 *	however the method outlined in crt0.c seems to be more portable.
 *
 *	Probably what will have to happen when a USG unexec is available,
 *	at least on UniPlus, is temacs will have to be made unshared so
 *	that text and data are contiguous.  Then once loadup is complete,
 *	unexec will produce a shared executable where the data can be
 *	at the normal shared text boundary and the startofdata variable
 *	will be patched by unexec to the correct value.
 *
 */

#if defined (ORDINARY_LINK) && !defined (MINGW)
extern char **environ;
#endif

void *
start_of_data (void)
{
#ifdef DATA_START
  return ((char *) DATA_START);
#else
#if defined (ORDINARY_LINK) || defined(PDUMP)
  /*
   * This is a hack.  Since we're not linking crt0.c or pre_crt0.c,
   * data_start isn't defined.  We take the address of environ, which
   * is known to live at or near the start of the system crt0.c, and
   * we don't sweat the handful of bytes that might lose.
   */
#if defined (HEAP_IN_DATA) && !defined(PDUMP)
  extern char* static_heap_base;
  if (!initialized)
    return static_heap_base;
#endif
  return ((char *) &environ);
#else
  extern int data_start;
  return ((char *) &data_start);
#endif /* ORDINARY_LINK */
#endif /* DATA_START */
}
#endif /* NEED_STARTS aka !(PDUMP && WIN32_NATIVE && SYSTEM_MALLOC) */

extern void *minimum_address_seen; /* from xmalloc() */
extern void *maximum_address_seen; /* from xmalloc() */

Bytecount
total_data_usage (void)
{
#ifdef NEED_STARTS
  void *data_start = start_of_data ();
#else
  void *data_start = minimum_address_seen;
#endif

#ifndef WIN32_ANY
  void *data_end = sbrk (0);
#else
  void *data_end = maximum_address_seen;
#endif

  /* Sanity checking -- the min determined by malloc() should always be
     greater than data start determined by other means.  We could do the
     same check on the max, except that things like rel-alloc might
     invalidate it. */
  if (minimum_address_seen &&
      (char *) minimum_address_seen < (char *) data_start)
    data_start = minimum_address_seen;

  if (data_end < data_start) /* Huh?????????? */
    data_end = maximum_address_seen;

  /* #### Doesn't seem to give good results on Windows; values are much
     higher than actual memory usage.  How to fix??? */
  return (char *) data_end - (char *) data_start;
}


/************************************************************************/
/*                          get the system name                         */
/************************************************************************/

/* init_system_name sets up the string for the Lisp function
   system-name to return. */

extern Lisp_Object Vsystem_name;

void
init_system_name (void)
{
#if defined (WIN32_NATIVE)
  Extbyte hostname[MAX_XETCHAR_SIZE * (MAX_COMPUTERNAME_LENGTH + 1)];
  DWORD size = sizeof (hostname) / XETCHAR_SIZE;
  qxeGetComputerName (hostname, &size);
  Vsystem_name = build_tstr_string (hostname);
#elif !defined (HAVE_GETHOSTNAME)
  struct utsname uts;
  uname (&uts);
  Vsystem_name = build_extstring (uts.nodename, Qunix_host_name_encoding);
#else /* HAVE_GETHOSTNAME */
  int hostname_size = 256;
  Extbyte *hostname = alloca_extbytes (hostname_size);

  /* Try to get the host name; if the buffer is too short, try
     again.  Apparently, the only indication gethostname gives of
     whether the buffer was large enough is the presence or absence
     of a '\0' in the string.  Eech.  */
  for (;;)
    {
      gethostname (hostname, hostname_size - 1);
      hostname[hostname_size - 1] = '\0';

      /* Was the buffer large enough for the '\0'?  */
      if ((int) strlen (hostname) < (hostname_size - 1))
	break;

      hostname_size <<= 1;
      hostname = alloca_extbytes (hostname_size);
    }
# if defined (HAVE_SOCKETS)
  /* Turn the hostname into the official, fully-qualified hostname.
     Don't do this if we're going to dump; this can confuse system
     libraries on some machines and make the dumped emacs core dump. */
  if (initialized)
    /* !!#### Could fail if we have a 7-bit external encoding */
    if (!strchr (hostname, '.'))
      {
#  if !(defined (HAVE_GETADDRINFO) && defined (HAVE_GETNAMEINFO))
	struct hostent *hp = NULL;
	int count;
#   ifdef TRY_AGAIN
	for (count = 0; count < 10; count++)
	  {
	    h_errno = 0;
#   endif
	    /* Some systems can't handle SIGALARM/SIGIO in gethostbyname(). */
	    stop_interrupts ();
	    hp = gethostbyname (hostname);
	    start_interrupts ();
#   ifdef TRY_AGAIN
	    if (! (hp == 0 && h_errno == TRY_AGAIN))
	      break;
	    Fsleep_for (Qone);
	  }
#   endif
	if (hp)
	  {
	    const Extbyte *fqdn = (const Extbyte *) hp->h_name;

	    /* !!#### Could fail if we have a 7-bit external encoding */
	    if (!strchr (fqdn, '.'))
	      {
		/* We still don't have a fully qualified domain name.
		   Try to find one in the list of alternate names */
		Extbyte **alias = hp->h_aliases;
		while (*alias && !strchr (*alias, '.'))
		  alias++;
		if (*alias)
		  fqdn = *alias;
	      }
	    hostname = alloca_extbytes (strlen (fqdn) + 1);
	    strcpy (hostname, fqdn);
	  }
#  else /* !(HAVE_GETADDRINFO && HAVE_GETNAMEINFO) */
	struct addrinfo hints, *res;

	xzero (hints);
	hints.ai_flags = AI_CANONNAME;
#ifdef IPV6_CANONICALIZE
	hints.ai_family = AF_UNSPEC;
#else
	hints.ai_family = PF_INET;
#endif
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = 0;
	if (!getaddrinfo (hostname, NULL, &hints, &res))
	  {
	    hostname = alloca_extbytes (strlen (res->ai_canonname) + 1);
	    strcpy (hostname, res->ai_canonname);

	    freeaddrinfo (res);
	  }
#  endif  /* !(HAVE_GETADDRINFO && HAVE_GETNAMEINFO) */
      }
# endif /* HAVE_SOCKETS */
  Vsystem_name = build_extstring (hostname, Qunix_host_name_encoding);
#endif /* HAVE_GETHOSTNAME  */
  {
    Ibyte *p;
    Bytecount i;

    for (i = 0, p = XSTRING_DATA (Vsystem_name);
	 i < XSTRING_LENGTH (Vsystem_name);
	 i++, p++)
      {
	if (*p == ' ' || *p == '\t')
	  *p = '-';
      }
  }
}


/************************************************************************/
/*                        Emulation of select()                         */
/************************************************************************/

#ifndef HAVE_SELECT

ERROR: XEmacs requires a working select().

#endif /* not HAVE_SELECT */


/************************************************************************/
/*                      Emulation of signal stuff                       */
/************************************************************************/

/* BSD 4.1 crap deleted.  4.2 was released in 1983, for God's sake!  I
   can't imagine that anyone is actually running that OS any more.
   You can't use X under it (I think) because there's no select().
   Anyway, the signal stuff has all been changed.  If someone wants to
   get this stuff working again, look in the FSF Emacs sources. */

/* POSIX signals support - DJB */

#ifdef HAVE_SIGPROCMASK

signal_handler_t
qxe_reliable_signal (int signal_number, signal_handler_t action)
{
  static struct sigaction new_action, old_action;
 
  /* XEmacs works better if system calls are not restarted.
     This allows C-g to interrupt reads and writes, on most systems.

     #### Another possibility is to just longjmp() out of the signal
     handler.  According to W.R. Stevens, this should be OK on all
     systems.  However, I don't want to deal with the potential
     evil ramifications of this at this point. */
  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
#if defined (SA_INTERRUPT) /* don't restart system calls, under SunOS */
  new_action.sa_flags = SA_INTERRUPT;
#else
  new_action.sa_flags = 0;
#endif
  sigaction (signal_number, &new_action, &old_action);
  return (signal_handler_t) (old_action.sa_handler);
}

#elif defined (HAVE_SIGBLOCK)

/* We use sigvec() rather than signal() if we have it, because
   it lets us specify interruptible system calls. */
signal_handler_t
qxe_reliable_signal (int signal_number, signal_handler_t action)
{
  struct sigvec vec, ovec;

  vec.sv_handler = action;
  vec.sv_mask = 0;
#ifdef SV_INTERRUPT /* don't restart system calls */
  vec.sv_flags = SV_INTERRUPT;
#else
  vec.sv_flags = 0;
#endif

  sigvec (signal_number, &vec, &ovec);

  return (ovec.sv_handler);
}

#endif /* HAVE_SIGBLOCK (HAVE_SIGPROCMASK) */


/************************************************************************/
/*           Emulation of strerror() and errno support                  */
/************************************************************************/

#ifndef HAVE_STRERROR

#if !defined(__alpha) && !defined(MACH) && !defined(LINUX) && !defined(IRIX6_5) && !defined(__NetBSD__)
/* Linux added here by Raymond L. Toy <toy@alydar.crd.ge.com> for XEmacs. */
/* Irix added here by gparker@sni-usa.com for XEmacs. */
/* NetBSD added here by James R Grinter <jrg@doc.ic.ac.uk> for XEmacs */
extern const char *sys_errlist[];
extern int sys_nerr;
#endif

#ifdef __NetBSD__
extern char *sys_errlist[];
extern int sys_nerr;
#endif


const char *
strerror (int errnum)
{
  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return ((const char *) GETTEXT ("Unknown error"));
}

#endif /* ! HAVE_STRERROR */


/************************************************************************/
/*                    Encapsulations of system calls                    */
/************************************************************************/

/***************** low-level calls ****************/

/*
 *	On USG systems the system calls are INTERRUPTIBLE by signals
 *	that the user program has elected to catch.  Thus the system call
 *	must be retried in these cases.  To handle this without massive
 *	changes in the source code, we remap the standard system call names
 *	to names for our own functions in sysdep.c that do the system call
 *	with retries.  Actually, for portability reasons, it is good
 *	programming practice, as this example shows, to limit all actual
 *	system calls to a single occurrence in the source.  Sure, this
 *	adds an extra level of function call overhead but it is almost
 *	always negligible.   Fred Fish, Unisoft Systems Inc.
 */

/* Ben sez: read Dick Gabriel's essay about the Worse Is Better
   approach to programming and its connection to the silly
   interruptible-system-call business.  To find it, look on
   Jamie's home page (http://www.jwz.org/doc/worse-is-better.html). */

#ifdef WIN32_NATIVE

static int
underlying_open_1 (const Extbyte *path, int oflag, int mode)
{
  if (XEUNICODE_P)
    return _wopen ((const wchar_t *) path, oflag, mode);
  else
    return _open (path, oflag, mode);
}

#endif /* WIN32_NATIVE */

/* Just call open() with normal open() semantics, with some fixups for
   problems under Windows. */

static int
underlying_open (const Extbyte *path, int oflag, int mode)
{
#ifdef WIN32_NATIVE
  {
    /* Try to open file without _O_CREAT, to be able to write to hidden
       and system files. Force all file handles to be
       non-inheritable. */
    int res = underlying_open_1 (path, (oflag & ~_O_CREAT) | _O_NOINHERIT,
				 mode);
    if (res >= 0)
      return res;
    return underlying_open_1 (path, oflag | _O_NOINHERIT, mode);
  }
#else
  return open (path, oflag, mode);
#endif /* WIN32_NATIVE */
}

static int
retry_open_1 (const Extbyte *path, int oflag, int mode)
{
  int rtnval;
  while ((rtnval = underlying_open (path, oflag, mode)) == -1
	 && (errno == EINTR))
    DO_NOTHING;
  return rtnval;
}

/* A version of open() that retries when interrupted.  Operates on
   externally-encoded filenames. */

int XCDECL
retry_open (const Extbyte *path, int oflag, ...)
{
  int mode;
  va_list ap;

  va_start (ap, oflag);
  mode = va_arg (ap, int);
  va_end (ap);

  return retry_open_1 (path, oflag, mode);
}

#if defined (WIN32_NATIVE) && defined (WEXTTEXT_IS_WIDE)

/* Like retry_open() but operate on Wexttext filenames. */

int XCDECL
wext_retry_open (const Wexttext *path, int oflag, ...)
{
  int mode;
  va_list ap;

  va_start (ap, oflag);
  mode = va_arg (ap, int);
  va_end (ap);

  if (!XEUNICODE_P)
    return retry_open_1 (WEXTTEXT_TO_MULTIBYTE (path), oflag, mode);
  else
    return retry_open_1 ((Extbyte *) path, oflag, mode);
}

#endif

/* The basic external entry point to open().  Handles conversion to
   external encoding, interruptions, etc. */

int XCDECL
qxe_open (const Ibyte *path, int oflag, ...)
{
  Extbyte *pathout;
  int mode;
  va_list ap;

  va_start (ap, oflag);
  mode = va_arg (ap, int);
  va_end (ap);

  PATHNAME_CONVERT_OUT (path, pathout);
  return retry_open (pathout, oflag, mode);
}

/* Like qxe_open, only when open() is interrupted by EINTR, check for
   QUIT.  This allows the callers of this function to be interrupted
   with C-g when, say, reading from named pipes.  However, this should
   be used with caution, as it can run random Lisp code (although it
   cannot GC).

   This function will not function as expected on systems where open()
   is not interrupted by C-g.  However, the worst that can happen is
   the fallback to simple open().  */
int
qxe_interruptible_open (const Ibyte *path, int oflag, int mode)
{
  /* This function can GC */
  Extbyte *pathout;

  PATHNAME_CONVERT_OUT (path, pathout);

#ifdef WIN32_NATIVE
  /* Make all handles non-inheritable */
  oflag |= _O_NOINHERIT;
#endif

  for (;;)
    {
      int rtnval = underlying_open (pathout, oflag, mode);
      if (!(rtnval == -1 && errno == EINTR))
	return rtnval;
      /* open() was interrupted.  Was QUIT responsible?  */
      QUIT;
    }
}

int
retry_close (int filedes)
{
  int did_retry = 0;
  REGISTER int rtnval;

  while ((rtnval = close (filedes)) == -1
	 && (errno == EINTR))
    did_retry = 1;

  /* If close is interrupted SunOS 4.1 may or may not have closed the
     file descriptor.  If it did the second close will fail with
     errno = EBADF.  That means we have succeeded.  */
  if (rtnval == -1 && did_retry && errno == EBADF)
    return 0;

  return rtnval;
}

static ssize_t
retry_read_1 (int fildes, void *buf, size_t nbyte, int allow_quit)
{
  ssize_t rtnval;

  while ((rtnval = read (fildes, buf, nbyte)) == -1
	 && (errno == EINTR))
    {
      if (allow_quit)
	QUIT;
    }
  return rtnval;
}

ssize_t
retry_read (int fildes, void *buf, size_t nbyte)
{
  return retry_read_1 (fildes, buf, nbyte, 0);
}

static ssize_t
retry_write_1 (int fildes, const void *buf, size_t nbyte, int allow_quit)
{
  ssize_t bytes_written = 0;
  const char *b = (const char *) buf;

  while (nbyte > 0)
    {
      ssize_t rtnval = write (fildes, b, nbyte);

      if (allow_quit)
	QUIT;

      if (rtnval == -1)
	{
	  if (errno == EINTR)
	    continue;
	  else
            return bytes_written ? bytes_written : -1;
	}
      b += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }
  return bytes_written;
}

ssize_t
retry_write (int fildes, const void *buf, size_t nbyte)
{
  return retry_write_1 (fildes, buf, nbyte, 0);
}

/* Versions of read() and write() that allow quitting out of the actual
   I/O.  We don't use immediate_quit (i.e. direct longjmp() out of the
   signal handler) because that's way too losing.

   (#### Actually, longjmp()ing out of the signal handler may not be
   as losing as I thought.  See qxe_reliable_signal() in sysdep.c.) */

Bytecount
read_allowing_quit (int fildes, void *buf, Bytecount size)
{
  QUIT;
  return retry_read_1 (fildes, buf, size, 1);
}

Bytecount
write_allowing_quit (int fildes, const void *buf, Bytecount size)
{
  QUIT;
  return retry_write_1 (fildes, buf, size, 1);
}


/**************** stdio calls ****************/

/* There is at least some evidence that the stdio calls are interruptible
   just like the normal system calls, at least on some systems.  In any
   case, it doesn't hurt to encapsulate them. */

/* #### Should also encapsulate fflush().
   #### Should conceivably encapsulate getchar() etc.  What a pain! */

FILE *
retry_fopen (const Extbyte *path, const Ascbyte *mode)
{
#ifdef WIN32_NATIVE
  int fd;
  int oflag;
  const Ascbyte *mode_save = mode;

  /* Force all file handles to be non-inheritable.  This is necessary to
     ensure child processes don't unwittingly inherit handles that might
     prevent future file access. */

  if (mode[0] == 'r')
    oflag = O_RDONLY;
  else if (mode[0] == 'w' || mode[0] == 'a')
    oflag = O_WRONLY | O_CREAT | O_TRUNC;
  else
    return NULL;

  /* Only do simplistic option parsing. */
  while (*++mode)
    if (mode[0] == '+')
      {
	oflag &= ~(O_RDONLY | O_WRONLY);
	oflag |= O_RDWR;
      }
    else if (mode[0] == 'b')
      {
	oflag &= ~O_TEXT;
	oflag |= O_BINARY;
      }
    else if (mode[0] == 't')
      {
	oflag &= ~O_BINARY;
	oflag |= O_TEXT;
      }
    else break;

  fd = underlying_open (path, oflag, 0644);
  if (fd < 0)
    return NULL;

  return _fdopen (fd, mode_save);
#else
  FILE *rtnval;
  while (!(rtnval = fopen (path, mode)) && (errno == EINTR))
    DO_NOTHING;
  return rtnval;
#endif /* (not) WIN32_NATIVE */
}

FILE *
qxe_fopen (const Ibyte *path, const Ascbyte *mode)
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return retry_fopen (pathout, mode);
}

int
retry_fclose (FILE *stream)
{
  int rtnval;

  while ((rtnval = fclose (stream)) == EOF
	 && (errno == EINTR))
    ;
  return rtnval;
}

size_t
retry_fread (void *ptr, size_t size, size_t nitem, FILE *stream)
{
  size_t rtnval;
  size_t items_read = 0;
  char *b = (char *) ptr;

  while (nitem > 0)
    {
      rtnval = fread (b, size, nitem, stream);
      if (rtnval == 0)
	{
	  if (ferror (stream) && errno == EINTR)
	    continue;
	  else
            return items_read;
	}
      b += size*rtnval;
      nitem -= rtnval;
      items_read += rtnval;
    }
  return (items_read);
}

size_t
retry_fwrite (const void *ptr, size_t size, size_t nitem, FILE *stream)
{
  size_t rtnval;
  size_t items_written = 0;
  const char *b = (const char *) ptr;

  while (nitem > 0)
    {
      rtnval = fwrite (b, size, nitem, stream);
      if (rtnval == 0)
	{
	  if (ferror (stream) && errno == EINTR)
	    continue;
	  else
            return items_written;
	}
      b += size*rtnval;
      nitem -= rtnval;
      items_written += rtnval;
    }
  return (items_written);
}

/********************* directory calls *******************/

int
qxe_chdir (const Ibyte *path)
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
#ifdef WIN32_NATIVE
  if (XEUNICODE_P)
    return _wchdir ((const wchar_t *) pathout);
  else
    return _chdir (pathout);
#else
  return chdir (pathout);
#endif
}

int
qxe_mkdir (const Ibyte *path,
#ifdef WIN32_NATIVE
	   mode_t UNUSED (mode)
#else
	   mode_t mode
#endif
	   )
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
#ifdef WIN32_NATIVE
  if (XEUNICODE_P)
    return _wmkdir ((const wchar_t *) pathout);
  else
    return _mkdir (pathout);
#else
  return mkdir (pathout, mode);
#endif
}

DIR *
qxe_opendir (const Ibyte *filename)
{
#ifdef WIN32_NATIVE
  return mswindows_opendir (filename);
#else
  DIR *rtnval;
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (filename, pathout);

  while (!(rtnval = opendir (pathout))
	 && (errno == EINTR))
    ;
  return rtnval;
#endif /* WIN32_NATIVE */
}

DIRENTRY *
qxe_readdir (DIR *dirp)
{
#ifdef WIN32_NATIVE
  return mswindows_readdir (dirp);
#else /* not WIN32_NATIVE */
  DIRENTRY *rtnval;

  /* Apparently setting errno is necessary on some systems?
     Maybe readdir() doesn't always set errno ?! */
  while (!(errno = 0, rtnval = readdir (dirp))
	 && (errno == EINTR))
    ;
#ifndef MULE
  return rtnval;
#else /* MULE */
  if (rtnval == NULL)           /* End of directory */
    return NULL;
  {
    const Extbyte * const external_name = (const Extbyte *) rtnval->d_name;
    Bytecount external_len = strlen (rtnval->d_name);
    const Ibyte *internal_name;
    Bytecount internal_len;

    TO_INTERNAL_FORMAT (DATA, (external_name, external_len),
			ALLOCA, (internal_name, internal_len),
			Qfile_name);

    /* check for common case of ASCII filename */
    if (internal_len == external_len &&
	!memcmp (external_name, internal_name, internal_len))
      return rtnval;

    { /* Non-ASCII filename */
      static Ibyte_dynarr *internal_DIRENTRY;
      if (!internal_DIRENTRY)
        internal_DIRENTRY = Dynarr_new (Ibyte);
      else
        Dynarr_reset (internal_DIRENTRY);

      Dynarr_add_many (internal_DIRENTRY, (Ibyte *) rtnval,
                       offsetof (DIRENTRY, d_name));


      Dynarr_add_many (internal_DIRENTRY, internal_name, internal_len);
      Dynarr_add (internal_DIRENTRY, '\0'); /* NUL-terminate */
      return (DIRENTRY *) Dynarr_begin (internal_DIRENTRY);
    }
  }
#endif /* MULE */
#endif /* WIN32_NATIVE */
}

int
qxe_closedir (DIR *dirp)
{
#ifdef WIN32_NATIVE
  return mswindows_closedir (dirp);
#else /* not WIN32_NATIVE */
  int rtnval;

  while ((rtnval = closedir (dirp)) == -1
	 && (errno == EINTR))
    ;
  return rtnval;
#endif /* WIN32_NATIVE */
}

int
qxe_rmdir (const Ibyte *path)
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
#ifdef WIN32_NATIVE
  if (XEUNICODE_P)
    return _wrmdir ((const wchar_t *) pathout);
  else
    return _rmdir (pathout);
#else
  return rmdir (pathout);
#endif
}

Ibyte *
qxe_allocating_getcwd (void)
{
#ifdef HAVE_GETCWD
  Bytecount cwdsize = 1024;
  Extbyte *cwd = xnew_array (Extbyte, cwdsize);

  /* Many getcwd()'s can take a NULL argument and malloc() the right amount
     of data, but this is non-standard. */
  while (1)
    {
#ifdef WIN32_NATIVE
      Extbyte *ret;

      if (XEUNICODE_P)
	ret = (Extbyte *) _wgetcwd ((wchar_t *) cwd,
				    cwdsize / sizeof (wchar_t));
      else
	ret = _getcwd (cwd, cwdsize);

      if (ret)
	{
	  Ibyte *retin;
	  retin = TSTR_TO_ITEXT_MALLOC (ret);
	  xfree (cwd);
	  return retin;
	}
#else
      Extbyte *ret = getcwd (cwd, cwdsize);
      if (ret)
	{
	  Ibyte *retin;
	  retin = EXTERNAL_TO_ITEXT_MALLOC (ret, Qfile_name);
	  xfree (cwd);
	  return retin;
	}
#endif /* WIN32_NATIVE */

      if (errno == ERANGE)
	{
	  cwdsize *= 2;
	  XREALLOC_ARRAY (cwd, Extbyte, cwdsize);
	}
      else
	{
	  xfree (cwd);
	  return NULL;
	}
    }
#else
  Extbyte chingame_limitos_arbitrarios[PATH_MAX_TCHAR];
  Ibyte *ret2;

  if (!getwd (chingame_limitos_arbitrarios))
    return 0;
  ret2 = EXTERNAL_TO_ITEXT_MALLOC (chingame_limitos_arbitrarios, Qfile_name);
  return ret2;
#endif /* HAVE_GETCWD */
}

/***************** file-information calls ******************/

int
qxe_access (const Ibyte *path, int mode)
{
#ifdef WIN32_NATIVE
  return mswindows_access (path, mode);
#else /* not WIN32_NATIVE */
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return access (pathout, mode);
#endif /* WIN32_NATIVE */
}

#if defined (HAVE_EACCESS)
int
qxe_eaccess (const Ibyte *path, int mode)
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return eaccess (pathout, mode);
}
#endif /* defined (HAVE_EACCESS) */

int
qxe_lstat (const Ibyte *path, struct stat *buf)
{
  /* if system does not have symbolic links, it does not have lstat.
     In that case, use ordinary stat instead.  */
#ifndef S_IFLNK
  return qxe_stat (path, buf);
#elif defined (WIN32_NATIVE)
  if (mswindows_shortcuts_are_symlinks)
    {
      /* We want to resolve the directory component and leave the rest
	 alone. */
      Ibyte *dirend = find_end_of_directory_component (path, qxestrlen (path));
      Bytecount len;
      
      if (dirend != path)
	{
	  Ibyte *resdir;
	  Ichar lastch;
	  DECLARE_EISTRING (resname);
	  DECLARE_EISTRING (dir);
	  
	  eicpy_raw (dir, path, dirend - path);
	  PATHNAME_RESOLVE_LINKS (eidata (dir), resdir);
	  eicpy_rawz (resname, resdir);
	  lastch = eigetch_char (resname, eicharlen (resname) - 1);
	  if (!IS_DIRECTORY_SEP (lastch))
	    eicat_ch (resname, '\\');
	  eicat_rawz (resname, dirend);
	  path = eidata (resname);
	}

      /* However, if what we are trying to stat is a link, we need to add
	 the .LNK so that the actual file is statted. */
      len = qxestrlen (path);
      if (len > 4 && qxestrcasecmp_ascii (path + len - 4, ".LNK"))
	{
	  DECLARE_EISTRING (name2);
	  Ibyte *resolved;
	
	  eicpy_rawz (name2, path);
	  eicat_ascii (name2, ".LNK");
	  resolved = mswindows_read_link (eidata (name2));
	  if (resolved)
	    {
	      xfree (resolved);
	      return mswindows_stat (eidata (name2), buf);
	    }
	}
    }

  return mswindows_stat (path, buf);
#else
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return lstat (pathout, buf);
#endif
}

#if defined (HAVE_READLINK)
int
qxe_readlink (const Ibyte *path, Ibyte *buf, size_t bufsiz)
{
  int retval;
  Extbyte *pathout;

  PATHNAME_CONVERT_OUT (path, pathout);
  retval = readlink (pathout, (char *) buf, bufsiz);
  if (retval < 0)
    return retval;
  {
    Ibyte *intbuf;
    Bytecount tamanho;

    TO_INTERNAL_FORMAT (DATA, (buf, retval),
			ALLOCA, (intbuf, tamanho), Qfile_name);
    /* the man page says this function does not null-terminate */
    if (tamanho >= (Bytecount) bufsiz)
      tamanho = bufsiz;
    memcpy (buf, intbuf, tamanho);
    return tamanho;
  }
}
#endif /* defined (HAVE_READLINK) */

int
qxe_fstat (int fd, struct stat *buf)
{
#ifdef WIN32_NATIVE
  return mswindows_fstat (fd, buf);
#else
  return fstat (fd, buf);
#endif /* WIN32_NATIVE */
}

int
qxe_stat (const Ibyte *path, struct stat *buf)
{
#ifdef WIN32_NATIVE
  Ibyte *resolved;
  PATHNAME_RESOLVE_LINKS (path, resolved);
  return mswindows_stat (resolved, buf);
#else /* not WIN32_NATIVE */
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return stat (pathout, buf);
#endif /* WIN32_NATIVE */
}


/****************** file-manipulation calls *****************/

int
qxe_chmod (const Ibyte *path, mode_t mode)
{
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
#ifdef WIN32_NATIVE
  if (XEUNICODE_P)
    return _wchmod ((const wchar_t *) pathout, mode);
  else
    return _chmod (pathout, mode);
#else
  return chmod (pathout, mode);
#endif
}

#if defined (HAVE_LINK)
int
qxe_link (const Ibyte *existing, const Ibyte *new_)
{
#ifdef WIN32_NATIVE
  return mswindows_link (existing, new_);
#else /* not WIN32_NATIVE */
  Extbyte *existingout, *newout;
  PATHNAME_CONVERT_OUT (existing, existingout);
  PATHNAME_CONVERT_OUT (new_, newout);
  return link (existingout, newout);
#endif /* WIN32_NATIVE */
}
#endif /* defined (HAVE_LINK) */

int
qxe_rename (const Ibyte *old, const Ibyte *new_)
{
#ifdef WIN32_NATIVE
  return mswindows_rename (old, new_);
#else /* not WIN32_NATIVE */
  Extbyte *oldout, *newout;
  PATHNAME_CONVERT_OUT (old, oldout);
  PATHNAME_CONVERT_OUT (new_, newout);
  return rename (oldout, newout);
#endif /* WIN32_NATIVE */
}

#if defined (HAVE_SYMLINK)
int
qxe_symlink (const Ibyte *name1, const Ibyte *name2)
{
  Extbyte *name1out, *name2out;
  PATHNAME_CONVERT_OUT (name1, name1out);
  PATHNAME_CONVERT_OUT (name2, name2out);
  return symlink (name1out, name2out);
}
#endif /* defined (HAVE_SYMLINK) */

int
qxe_unlink (const Ibyte *path)
{
#ifdef WIN32_NATIVE
  return mswindows_unlink (path);
#else /* not WIN32_NATIVE */
  Extbyte *pathout;
  PATHNAME_CONVERT_OUT (path, pathout);
  return unlink (pathout);
#endif /* WIN32_NATIVE */
}


/****************** process calls *****************/

int
qxe_execve (const Ibyte *filename, Ibyte * const argv[],
	    Ibyte * const envp[])
{
  int i, argc, envc;
  Extbyte *pathext;
  Extbyte **new_argv;
  Extbyte **new_envp;

  PATHNAME_CONVERT_OUT (filename, pathext);

  for (argc = 0; argv[argc]; argc++)
    ;
  new_argv = alloca_array (Extbyte *, argc + 1);
  for (i = 0; i < argc; i++)
    new_argv[i] = ITEXT_TO_EXTERNAL (argv[i], Qcommand_argument_encoding);
  new_argv[argc] = NULL;

  for (envc = 0; envp[envc]; envc++)
    ;
  new_envp = alloca_array (Extbyte *, envc + 1);
  for (i = 0; i < envc; i++)
    new_envp[i] = ITEXT_TO_EXTERNAL (envp[i], Qenvironment_variable_encoding);
  new_envp[envc] = NULL;

#if defined (WIN32_NATIVE)
  if (XEUNICODE_P)
    return _wexecve ((const wchar_t *) pathext,
		     (const wchar_t * const *) new_argv,
		     (const wchar_t * const *) new_envp);
#endif
  return execve (pathext, new_argv, new_envp);
}

pid_t
qxe_getpid (void)
{
#ifdef WIN32_NATIVE
  return abs (getpid ());
#else
  return getpid ();
#endif
}


/****************** passwd calls *****************/

struct passwd cached_pwd;

static struct passwd *
copy_in_passwd (struct passwd *pwd)
{
  if (!pwd)
    return NULL;

  if (cached_pwd.pw_name)
    xfree (cached_pwd.pw_name);
  if (cached_pwd.pw_passwd)
    xfree (cached_pwd.pw_passwd);
  if (cached_pwd.pw_gecos)
    xfree (cached_pwd.pw_gecos);
  if (cached_pwd.pw_dir)
    xfree (cached_pwd.pw_dir);
  if (cached_pwd.pw_shell)
    xfree (cached_pwd.pw_shell);

  cached_pwd = *pwd;

#define FROB(field, encoding)					\
do								\
{								\
  if (cached_pwd.field)						\
    cached_pwd.field = (CIbyte *)				\
      EXTERNAL_TO_ITEXT_MALLOC (cached_pwd.field, encoding);	\
} while (0)

  FROB (pw_name, Quser_name_encoding);
  FROB (pw_passwd, Quser_name_encoding);
  FROB (pw_gecos, Quser_name_encoding);
  FROB (pw_dir, Qfile_name);
  FROB (pw_shell, Qfile_name);
#undef FROB
  return &cached_pwd;
}

struct passwd *
qxe_getpwnam (const Ibyte *name)
{
#ifdef WIN32_NATIVE
  /* Synthetic versions are defined in nt.c and already do conversion. */
  return getpwnam (name);
#else
  Extbyte *nameext = ITEXT_TO_EXTERNAL (name, Quser_name_encoding);

  return copy_in_passwd (getpwnam (nameext));
#endif /* WIN32_NATIVE */
}

struct passwd *
qxe_getpwuid (uid_t uid)
{
#ifdef WIN32_NATIVE
  /* Synthetic versions are defined in nt.c and already do conversion. */
  return getpwuid (uid);
#else
  return copy_in_passwd (getpwuid (uid));
#endif /* WIN32_NATIVE */
}

#ifndef WIN32_NATIVE

struct passwd *
qxe_getpwent (void)
{
  /* No WIN32_NATIVE version of this. */
  return copy_in_passwd (getpwent ());
}

#endif /* not WIN32_NATIVE */

/****************** time calls *****************/

static Ibyte *ctime_static;

Ibyte *
qxe_ctime (const time_t *t)
{
  Extbyte *str = (Extbyte *) ctime (t);
  if (!str) /* can happen on MS Windows */
    return (Ibyte *) "Sun Jan 01 00:00:00 1970";
  if (ctime_static)
    xfree (ctime_static);
  ctime_static = EXTERNAL_TO_ITEXT_MALLOC (str, Qtime_function_encoding);
  return ctime_static;
}


/************************************************************************/
/*                  Emulation of missing functions from wchar.h         */
/************************************************************************/

#ifndef HAVE_WCHAR_H
size_t
wcslen (const wchar_t *s)
{
  const wchar_t *p = s;
  if (s == NULL) return NULL;

  while (*p++)
    ;
  
  return p - s;
}
#endif

/************************************************************************/
/*                  Emulation of missing functions from string.h        */
/************************************************************************/

#ifndef HAVE_STRLWR
char *
strlwr (char *s)
{
  REGISTER char *c;
  if (s == NULL) return NULL;

  for (c = s; *c; c++)
    {
      *c = tolower (*c);
    }
  return s;
}
#endif

#ifndef HAVE_STRUPR
char *
strupr (char *s)
{
  REGISTER char *c;

  for (c = s; *c; c++)
    {
      *c = toupper (*c);
    }
  return s;
}
#endif


/************************************************************************/
/*                  Emulations of missing system calls                  */
/************************************************************************/

/***** (these are primarily required for USG, it seems) *****/

/*
 *	Emulate rename using unlink/link.  Note that this is
 *	only partially correct.  Also, doesn't enforce restriction
 *	that files be of same type (regular->regular, dir->dir, etc).
 */

#ifndef HAVE_RENAME
int
rename (const Extbyte *from, const Extbyte *to)
{
  if (access (from, 0) == 0)
    {
      unlink (to);
      if (link (from, to) == 0)
	if (unlink (from) == 0)
	  return (0);
    }
  return (-1);
}
#endif /* HAVE_RENAME */

#ifdef HPUX
#ifndef HAVE_PERROR

/* HPUX curses library references perror, but as far as we know
   it won't be called.  Anyway this definition will do for now.  */

perror (void)
{
}

#endif /* not HAVE_PERROR */
#endif /* HPUX */

#ifndef HAVE_DUP2

/*
 *	Emulate BSD dup2.  First close newd if it already exists.
 *	Then, attempt to dup oldd.  If not successful, call dup2 recursively
 *	until we are, then close the unsuccessful ones.
 */

int
dup2 (int oldd, int newd)
{
  int fd, ret;

  retry_close (newd);

#ifdef F_DUPFD
  fd = fcntl (oldd, F_DUPFD, newd);
  if (fd != newd)
    signal_ferror_with_frob (Qfile_error, lisp_strerror (errno),
			     "can't dup2 (%i, %i)", oldd, newd);
#else
  fd = dup (oldd);
  if (fd == -1)
    return -1;
  if (fd == newd)
    return newd;
  ret = dup2 (oldd, newd);
  retry_close (fd);
  return ret;
#endif /*  F_DUPFD */
}

#endif /* not HAVE_DUP2 */

/*
 *	Gettimeofday.  Simulate as much as possible.  Only accurate
 *	to nearest second.  Emacs doesn't use tzp so ignore it for now.
 */

#if !defined (HAVE_GETTIMEOFDAY)

int
gettimeofday (struct timeval *tp, struct timezone *tzp)
{
  extern long time ();

  tp->tv_sec = time ((long *)0);
  tp->tv_usec = 0;
  if (tzp != 0)
    tzp->tz_minuteswest = -1;
  return (0);
}

#endif /* !HAVE_GETTIMEOFDAY */

/* No need to encapsulate utime and utimes explicitly because all
   access to those functions goes through the following. */

int
set_file_times (
#if defined (WIN32_NATIVE) || defined (HAVE_UTIME) || defined (HAVE_UTIMES)
		Lisp_Object path, EMACS_TIME atime, EMACS_TIME mtime
#else
		Lisp_Object UNUSED (path), EMACS_TIME UNUSED (atime),
		EMACS_TIME UNUSED (mtime)
#endif
		)
{
#if defined (WIN32_NATIVE)
  struct utimbuf utb;
  utb.actime = EMACS_SECS (atime);
  utb.modtime = EMACS_SECS (mtime);
  return mswindows_utime (path, &utb);
#elif defined (HAVE_UTIME)
  struct utimbuf utb;
  Extbyte *filename;
  utb.actime = EMACS_SECS (atime);
  utb.modtime = EMACS_SECS (mtime);
  LISP_PATHNAME_CONVERT_OUT (path, filename);
  return utime (filename, &utb);
#elif defined (HAVE_UTIMES)
  struct timeval tv[2];
  Extbyte *filename;
  tv[0] = atime;
  tv[1] = mtime;
  LISP_PATHNAME_CONVERT_OUT (path, filename);
  return utimes (filename, tv);
#else
  /* No file times setting function available. */
  return -1;
#endif
}

/* */

static long ticks_per_second;
static long orig_user_ticks, orig_system_ticks;
EMACS_TIME orig_real_time;

static int process_times_available;

/* Return the relative user and system tick count.  We try to
   maintain calculations in terms of integers as long as possible
   for increased accuracy. */

static int
get_process_times_1 (
#if defined (CLOCKS_PER_SEC) || defined (_SC_CLK_TCK) || defined (CLK_TCK) && !defined(WIN32_NATIVE)
		     long *user_ticks, long *system_ticks
#else
		     long *UNUSED (user_ticks), long *UNUSED (system_ticks)
#endif
		     )
{
#if defined (_SC_CLK_TCK) || defined (CLK_TCK) && !defined(WIN32_NATIVE)
  /* We have the POSIX times() function available. */
  /* #### Perhaps we should just use a configure test for times()? */
  struct tms tttt;
  times (&tttt);
  *user_ticks = (long) tttt.tms_utime;
  *system_ticks = (long) tttt.tms_stime;
  return 1;
#elif defined (CLOCKS_PER_SEC)
  *user_ticks = (long) clock ();
  *system_ticks = 0;
  return 1;
#else
  return 0;
#endif
}

void
init_process_times_very_early (void)
{
#if defined (_SC_CLK_TCK)
  ticks_per_second = sysconf (_SC_CLK_TCK);
#elif defined (CLK_TCK)
  ticks_per_second = CLK_TCK;
#elif defined (CLOCKS_PER_SEC)
  ticks_per_second = CLOCKS_PER_SEC;
#endif

  process_times_available = get_process_times_1 (&orig_user_ticks,
						 &orig_system_ticks);
  EMACS_GET_TIME (orig_real_time);
}

/* Return the user and system times used up by this process so far. */
void
get_process_times (double *user_time, double *system_time, double *real_time)
{
  EMACS_TIME curr_real_time;
  EMACS_TIME elapsed_time;
  long curr_user_ticks, curr_system_ticks;

  EMACS_GET_TIME (curr_real_time);
  EMACS_SUB_TIME (elapsed_time, curr_real_time, orig_real_time);
  *real_time = (EMACS_SECS (elapsed_time)
		+ ((double) EMACS_USECS (elapsed_time)) / 1000000);
  if (get_process_times_1 (&curr_user_ticks, &curr_system_ticks))
    {
      *user_time = (((double) (curr_user_ticks - orig_user_ticks))
		    / ticks_per_second);
      *system_time = (((double) (curr_system_ticks - orig_system_ticks))
		      / ticks_per_second);
    }
  else
    {
      /* A lame OS */
      *user_time = *real_time;
      *system_time = 0;
    }
}

#ifndef HAVE_RANDOM
#ifdef random
#define HAVE_RANDOM
#endif
#endif

/* Figure out how many bits the system's random number generator uses.
   `random' and `lrand48' are assumed to return 31 usable bits.
   BSD `rand' returns a 31 bit value but the low order bits are unusable;
   so we'll shift it and treat it like the 15-bit USG `rand'.  */

#ifndef RAND_BITS
# ifdef HAVE_RANDOM
#  define RAND_BITS 31
# else /* !HAVE_RANDOM */
#  ifdef HAVE_LRAND48
#   define RAND_BITS 31
#   define random lrand48
#  else /* !HAVE_LRAND48 */
#   define RAND_BITS 15
#   if RAND_MAX == 32767
#    define random rand
#   else /* RAND_MAX != 32767 */
#    if RAND_MAX == 2147483647
#     define random() (rand () >> 16)
#    else /* RAND_MAX != 2147483647 */
#     ifdef USG
#      define random rand
#     else
#      define random() (rand () >> 16)
#     endif /* !BSD */
#    endif /* RAND_MAX != 2147483647 */
#   endif /* RAND_MAX != 32767 */
#  endif /* !HAVE_LRAND48 */
# endif /* !HAVE_RANDOM */
#endif /* !RAND_BITS */

void
seed_random (long arg)
{
#ifdef HAVE_RANDOM
  srandom ((unsigned int)arg);
#else
# ifdef HAVE_LRAND48
  srand48 (arg);
# else
  srand ((unsigned int)arg);
# endif
#endif
#ifdef HAVE_BIGNUM
  bignum_random_seed ((unsigned long) arg);
#endif
}

/*
 * Build a full Emacs-sized word out of whatever we've got.
 * This suffices even for a 64-bit architecture with a 15-bit rand.
 */
long
get_random (void)
{
  long val = random ();
#if INT_VALBITS > RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if INT_VALBITS > 2*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if INT_VALBITS > 3*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if INT_VALBITS > 4*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#endif /* need at least 5 */
#endif /* need at least 4 */
#endif /* need at least 3 */
#endif /* need at least 2 */
  return val & (EMACS_INT) ((1UL << INT_VALBITS) - 1);
}


/************************************************************************/
/*               Strings corresponding to defined signals               */
/************************************************************************/

#if (!defined(HAVE_DECL_SYS_SIGLIST) || !HAVE_DECL_SYS_SIGLIST ) && !defined (HAVE_SYS_SIGLIST)

#if defined(WIN32_NATIVE) || defined(CYGWIN)
const char *sys_siglist[] =
  {
    /* @@@begin-snarf@@@ */
    "bum signal!!",
    "hangup",
    "interrupt",
    "quit",
    "illegal instruction",
    "trace trap",
    "iot instruction",
    "emt instruction",
    "floating point exception",
    "kill",
    "bus error",
    "segmentation violation",
    "bad argument to system call",
    "write on a pipe with no one to read it",
    "alarm clock",
    "software termination signal from kill",
    "status signal",
    "sendable stop signal not from tty",
    "stop signal from tty",
    "continue a stopped process",
    "child status has changed",
    "background read attempted from control tty",
    "background write attempted from control tty",
    "input record available at control tty",
    "exceeded CPU time limit",
    "exceeded file size limit",
    /* @@@end-snarf@@@ */
    };
#endif

#ifdef USG
#ifdef AIX
const char *sys_siglist[NSIG + 1] =
  {
    /* AIX has changed the signals a bit */
    /* @@@begin-snarf@@@ */
    "bogus signal",				/* 0 */
    "hangup",					/* 1  SIGHUP */
    "interrupt",				/* 2  SIGINT */
    "quit",					/* 3  SIGQUIT */
    "illegal instruction",			/* 4  SIGILL */
    "trace trap",				/* 5  SIGTRAP */
    "IOT instruction",				/* 6  SIGIOT */
    "crash likely",				/* 7  SIGDANGER */
    "floating point exception",			/* 8  SIGFPE */
    "kill",					/* 9  SIGKILL */
    "bus error",				/* 10 SIGBUS */
    "segmentation violation",			/* 11 SIGSEGV */
    "bad argument to system call",		/* 12 SIGSYS */
    "write on a pipe with no one to read it",	/* 13 SIGPIPE */
    "alarm clock",				/* 14 SIGALRM */
    "software termination signal",		/* 15 SIGTERM */
    "user defined signal 1",			/* 16 SIGUSR1 */
    "user defined signal 2",			/* 17 SIGUSR2 */
    "death of a child",				/* 18 SIGCLD */
    "power-fail restart",			/* 19 SIGPWR */
    "bogus signal",				/* 20 */
    "bogus signal",				/* 21 */
    "bogus signal",				/* 22 */
    "bogus signal",				/* 23 */
    "bogus signal",				/* 24 */
    "LAN I/O interrupt",			/* 25 SIGAIO */
    "PTY I/O interrupt",			/* 26 SIGPTY */
    "I/O intervention required",		/* 27 SIGIOINT */
    /* @@@end-snarf@@@ */
    0
  };
#else /* USG, not AIX */
const char *sys_siglist[NSIG + 1] =
  {
    /* @@@begin-snarf@@@ */
    "bogus signal",				/* 0 */
    "hangup",					/* 1  SIGHUP */
    "interrupt",				/* 2  SIGINT */
    "quit",					/* 3  SIGQUIT */
    "illegal instruction",			/* 4  SIGILL */
    "trace trap",				/* 5  SIGTRAP */
    "IOT instruction",				/* 6  SIGIOT */
    "EMT instruction",				/* 7  SIGEMT */
    "floating point exception",			/* 8  SIGFPE */
    "kill",					/* 9  SIGKILL */
    "bus error",				/* 10 SIGBUS */
    "segmentation violation",			/* 11 SIGSEGV */
    "bad argument to system call",		/* 12 SIGSYS */
    "write on a pipe with no one to read it",	/* 13 SIGPIPE */
    "alarm clock",				/* 14 SIGALRM */
    "software termination signal",		/* 15 SIGTERM */
    "user defined signal 1",			/* 16 SIGUSR1 */
    "user defined signal 2",			/* 17 SIGUSR2 */
    "death of a child",				/* 18 SIGCLD */
    "power-fail restart",			/* 19 SIGPWR */
#ifdef sun					
    "window size changed",			/* 20 SIGWINCH */
    "urgent socket condition",			/* 21 SIGURG */
    "pollable event occurred",			/* 22 SIGPOLL */
    "stop (cannot be caught or ignored)",	/*  23 SIGSTOP */
    "user stop requested from tty",		/* 24 SIGTSTP */
    "stopped process has been continued",	/* 25 SIGCONT */
    "background tty read attempted",		/* 26 SIGTTIN */
    "background tty write attempted",		/* 27 SIGTTOU */
    "virtual timer expired",			/* 28 SIGVTALRM */
    "profiling timer expired",			/* 29 SIGPROF */
    "exceeded cpu limit",			/* 30 SIGXCPU */
    "exceeded file size limit",			/* 31 SIGXFSZ */
    "process's lwps are blocked",		/* 32 SIGWAITING */
    "special signal used by thread library",	/* 33 SIGLWP */
#ifdef SIGFREEZE
    "special signal used by CPR",		/* 34 SIGFREEZE */
#endif
#ifdef SIGTHAW
    "special signal used by CPR",		/* 35 SIGTHAW */
#endif
#endif /* sun */
    /* @@@end-snarf@@@ */
    0
  };
#endif /* not AIX */
#endif /* USG */

#endif /* (!defined(HAVE_DECL_SYS_SIGLIST) || !HAVE_DECL_SYS_SIGLIST ) && !defined (HAVE_SYS_SIGLIST) */


/************************************************************************/
/*         Directory routines for systems that don't have them          */
/************************************************************************/

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>

#if !defined(HAVE_CLOSEDIR)
int
closedir (DIR *dirp)  /* stream from opendir */
{
  int rtnval;

  rtnval = retry_close (dirp->dd_fd);

  /* Some systems (like Solaris) allocate the buffer and the DIR all
     in one block.  Why in the world are we freeing this ourselves
     anyway?  */
#if ! (defined (sun) && defined (USG5_4))
  xfree (dirp->dd_buf); /* directory block defined in <dirent.h> */
#endif
  xfree (dirp);
  return (rtnval);
}
#endif /* not HAVE_CLOSEDIR */
#endif /* SYSV_SYSTEM_DIR */

#ifdef NONSYSTEM_DIR_LIBRARY

DIR *
opendir (const char *filename)	/* name of directory */
{
  DIR *dirp;		/* -> malloc'ed storage */
  int fd;		/* file descriptor for read */
  struct stat sbuf;		/* result of fstat */

  fd = open (filename, O_RDONLY);
  if (fd < 0)
    return 0;

  if (fstat (fd, &sbuf) < 0
      || (sbuf.st_mode & S_IFMT) != S_IFDIR
      || (dirp = (DIR *) malloc (sizeof (DIR))) == 0)
    {
      retry_close (fd);
      return 0;		/* bad luck today */
    }

  dirp->dd_fd = fd;
  dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

  return dirp;
}

void
closedir (DIR *dirp)		/* stream from opendir */
{
  retry_close (dirp->dd_fd);
  xfree (dirp);
}


#define DIRSIZ	14
struct olddir
  {
    ino_t od_ino; 		/* inode */
    char od_name[DIRSIZ];	/* filename */
  };

static struct direct dir_static; /* simulated directory contents */

/* ARGUSED */
struct direct *
readdir (DIR *dirp)	/* stream from opendir */
{
  struct olddir *dp;	/* -> directory data */

  for (; ;)
    {
      if (dirp->dd_loc >= dirp->dd_size)
	dirp->dd_loc = dirp->dd_size = 0;

      if (dirp->dd_size == 0 	/* refill buffer */
	  && (dirp->dd_size =
	      retry_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
	return 0;

      dp = (struct olddir *) &dirp->dd_buf[dirp->dd_loc];
      dirp->dd_loc += sizeof (struct olddir);

      if (dp->od_ino != 0)	/* not deleted entry */
	{
	  dir_static.d_ino = dp->od_ino;
	  strncpy (dir_static.d_name, dp->od_name, DIRSIZ);
	  dir_static.d_name[DIRSIZ] = '\0';
	  dir_static.d_namlen = strlen (dir_static.d_name);
	  dir_static.d_reclen = sizeof (struct direct)
	    - MAXNAMLEN + 3
	      + dir_static.d_namlen - dir_static.d_namlen % 4;
	  return &dir_static;	/* -> simulated structure */
	}
    }
}


#endif /* NONSYSTEM_DIR_LIBRARY */


/* mkdir and rmdir functions, for systems which don't have them.  */

#ifndef HAVE_MKDIR
/*
 * Written by Robert Rother, Mariah Corporation, August 1985.
 *
 * If you want it, it's yours.  All I ask in return is that if you
 * figure out how to do this in a Bourne Shell script you send me
 * a copy.
 *					sdcsvax!rmr or rmr@uscd
 *
 * Severely hacked over by John Gilmore to make a 4.2BSD compatible
 * subroutine.	11Mar86; hoptoad!gnu
 *
 * Modified by rmtodd@uokmax 6-28-87 -- when making an already existing dir,
 * subroutine didn't return EEXIST.  It does now.
 */

/*
 * Make a directory.
 */
#ifdef MKDIR_PROTOTYPE
MKDIR_PROTOTYPE
#else
int
mkdir (const char *dpath, int dmode)
#endif
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) == 0) /* we do want stat() here */
    {
      errno = EEXIST;		/* Stat worked, so it already exists */
      return -1;
    }

  /* If stat fails for a reason other than non-existence, return error */
  if (errno != ENOENT)
    return -1;

  synch_process_alive = 1;
  switch (cpid = fork ())
    {

    case -1:			/* Error in fork() */
      return -1;		/* Errno is set already */

    case 0:			/* Child process */
    {
      /*
       * Cheap hack to set mode of new directory.  Since this
       * child process is going away anyway, we zap its umask.
       * ####, this won't suffice to set SUID, SGID, etc. on this
       * directory.  Does anybody care?
       */
      status = umask (0);	/* Get current umask */
      status = umask (status | (0777 & ~dmode));	/* Set for mkdir */
      fd = open ("/dev/null", O_RDWR);
      if (fd >= 0)
        {
	  if (fd != STDIN_FILENO)  dup2 (fd, STDIN_FILENO);
	  if (fd != STDOUT_FILENO) dup2 (fd, STDOUT_FILENO);
	  if (fd != STDERR_FILENO) dup2 (fd, STDERR_FILENO);
        }
      execl ("/bin/mkdir", "mkdir", dpath, (char *) 0);
      _exit (-1);		/* Can't exec /bin/mkdir */
    }

    default:			/* Parent process */
      wait_for_termination (cpid);
    }

  if (synch_process_death != 0 || synch_process_retcode != 0)
    {
      errno = EIO;		/* We don't know why, but */
      return -1;		/* /bin/mkdir failed */
    }

  return 0;
}
#endif /* not HAVE_MKDIR */

#ifndef HAVE_RMDIR
int
rmdir (const char *dpath)
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) != 0) /* we do want stat() here */
    {
      /* Stat just set errno.  We don't have to */
      return -1;
    }

  synch_process_alive = 1;
  switch (cpid = fork ())
    {

    case -1:			/* Error in fork() */
      return (-1);		/* Errno is set already */

    case 0:			/* Child process */
      fd = open ("/dev/null", O_RDWR);
      if (fd >= 0)
        {
	  if (fd != STDIN_FILENO)  dup2 (fd, STDIN_FILENO);
	  if (fd != STDOUT_FILENO) dup2 (fd, STDOUT_FILENO);
	  if (fd != STDERR_FILENO) dup2 (fd, STDERR_FILENO);
        }
      execl ("/bin/rmdir", "rmdir", dpath, (char *) 0);
      _exit (-1);		/* Can't exec /bin/mkdir */

    default:			/* Parent process */
      wait_for_termination (cpid);
    }

  if (synch_process_death   != 0 ||
      synch_process_retcode != 0)
    {
      errno = EIO;		/* We don't know why, but */
      return -1;		/* /bin/rmdir failed */
    }

  return 0;
}
#endif /* !HAVE_RMDIR */


/************************************************************************/
/*                            Misc. SunOS crap                          */
/************************************************************************/

#ifdef USE_DL_STUBS

/* These are included on Sunos 4.1 when we do not use shared libraries.
   X11 libraries may refer to these functions but (we hope) do not
   actually call them.  */

void *
dlopen (void)
{
  return 0;
}

void *
dlsym (void)
{
  return 0;
}

int
dlclose (void)
{
  return -1;
}

#endif /* USE_DL_STUBS */
