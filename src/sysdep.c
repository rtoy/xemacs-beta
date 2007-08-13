/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985-1988, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems.

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

/* Synched up with: FSF 19.30 except for some Windows-NT crap. */

/* Substantially cleaned up by Ben Wing, Dec. 1994 / Jan. 1995. */

/* In this file, open, read and write refer to the system calls,
   not our sugared interfaces sys_open, sys_read and sys_write.
 */

#define DONT_ENCAPSULATE

#include <config.h>
#include "lisp.h"

#include <stddef.h>

/* ------------------------------- */
/*          basic includes         */
/* ------------------------------- */

#ifdef HAVE_TTY
#include "console-tty.h"
#else
#include "syssignal.h"
#include "systty.h"
#endif /* HAVE_TTY */

#include "console-stream.h"

#include "buffer.h"
#include "events.h"
#include "frame.h"
#include "redisplay.h"
#include "process.h"
#include "sysdep.h"
#include "window.h"

#include <setjmp.h>
#ifdef HAVE_LIBGEN_H            /* Must come before sysfile.h */
#include <libgen.h>
#endif
#include "sysfile.h"
#include "syswait.h"
#include "sysdir.h"
#include "systime.h"
#if defined(WINDOWSNT)
#include "syssignal.h"
#else
#include <sys/times.h>
#endif

#ifdef WINDOWSNT
#include <direct.h>
/* In process.h which conflicts with the local copy.  */
#define _P_WAIT 0
int _CRTAPI1 _spawnlp (int, const char *, const char *, ...);
int _CRTAPI1 _getpid (void);
#endif

/* ------------------------------- */
/*           VMS includes          */
/* ------------------------------- */

#ifdef VMS
#include <ttdef.h>
#include <tt2def.h>
#include <iodef.h>
#include <ssdef.h>
#include <descrip.h>
#include <fibdef.h>
#include <atrdef.h>
#undef F_SETFL
#ifndef RAB/*$C_BID   -- suppress compiler warnings */
#include <rab.h>
#endif
#define	MAXIOSIZE (32 * PAGESIZE)	/* Don't I/O more than 32 blocks at a time */
#endif /* VMS */

/* ------------------------------- */
/*         TTY definitions         */
/* ------------------------------- */

#ifdef USG
#include <sys/utsname.h>
#if defined (TIOCGWINSZ) || defined (ISC4_0)
#ifdef NEED_SIOCTL
#include <sys/sioctl.h>
#endif
#ifdef NEED_PTEM_H
#include <sys/stream.h>
#include <sys/ptem.h>
#endif
#endif /* TIOCGWINSZ or ISC4_0 */
#endif /* USG */

#ifdef HAVE_SYS_STROPTS_H
#include <sys/stropts.h>
#endif /* HAVE_SYS_STROPTS_H */

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

#ifdef AIXHFT
static void hft_init (struct console *c);
static void hft_reset (struct console *c);
#endif

/* ------------------------------- */
/*          miscellaneous          */
/* ------------------------------- */

#ifndef HAVE_UTIMES
#ifndef HAVE_STRUCT_UTIMBUF
/* We want to use utime rather than utimes, but we couldn't find the
   structure declaration.  We'll use the traditional one.  */
struct utimbuf
{
  long actime;
  long modtime;
};
#endif
#endif


/************************************************************************/
/*                         subprocess control                           */
/************************************************************************/

#ifdef HAVE_TTY

#ifdef SIGTSTP

/* Arrange for character C to be read as the next input from
   the terminal.  */
void
stuff_char (struct console *con, int c)
{
  int input_fd;

  assert (CONSOLE_TTY_P (con));
  input_fd = CONSOLE_TTY_DATA (con)->infd;
/* Should perhaps error if in batch mode */
#ifdef TIOCSTI
  ioctl (input_fd, TIOCSTI, &c);
#else /* no TIOCSTI */
  error ("Cannot stuff terminal input characters in this version of Unix.");
#endif /* no TIOCSTI */
}

#endif /* SIGTSTP */

#endif /* HAVE_TTY */

void
set_exclusive_use (int fd)
{
#ifdef FIOCLEX
  ioctl (fd, FIOCLEX, 0);
#endif
  /* Ok to do nothing if this feature does not exist */
}

void
set_descriptor_non_blocking (int fd)
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

#ifdef O_NONBLOCK /* The POSIX way */
  fcntl (fd, F_SETFL, O_NONBLOCK);
#elif defined (O_NDELAY)
  fcntl (fd, F_SETFL, O_NDELAY);
#endif /* O_NONBLOCK */
}

#if defined (NO_SUBPROCESSES)

#ifdef BSD
void
wait_without_blocking (void)
{
  wait3 (0, WNOHANG | WUNTRACED, 0);
  synch_process_alive = 0;
}
#endif /* BSD */

#endif /* NO_SUBPROCESSES */


void
wait_for_termination (int pid)
{
  /* #### With the new improved SIGCHLD handling stuff, there is much
     less danger of race conditions and some of the comments below
     don't apply.  This should be updated. */

#if defined (NO_SUBPROCESSES)
  while (1)
    {
      /* No need to be tricky like below; we can just call wait(). */
      /* #### should figure out how to write a wait_allowing_quit().
	 Since hardly any systems don't have subprocess support,
	 however, there doesn't seem to be much point. */
      if (wait (0) == pid)
	return;
    }
#elif defined (VMS)
  int status = SYS$FORCEX (&pid, 0, 0);
  return;

#elif defined (HAVE_WAITPID)
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
      emacs_sleep (1);
    }
#endif /* OS features */
}


#if !defined (NO_SUBPROCESSES)

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */

void
flush_pending_output (int channel)
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

#ifndef VMS
#ifndef MSDOS
#ifndef WINDOWSNT
/*  Set up the terminal at the other end of a pseudo-terminal that
    we will be controlling an inferior through.
    It should not echo or do line-editing, since that is done
    in Emacs.  No padding needed for insertion into an Emacs buffer.  */

void
child_setup_tty (int out)
{
  struct emacs_tty s;
  EMACS_GET_TTY (out, &s);

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  assert (isatty(out));
  s.main.c_oflag |= OPOST;	/* Enable output postprocessing */
  s.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
#ifdef NLDLY
  s.main.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
  				/* No output delays */
#endif
  s.main.c_lflag &= ~ECHO;	/* Disable echo */
  s.main.c_lflag |= ISIG;	/* Enable signals */
#ifdef IUCLC
  s.main.c_iflag &= ~IUCLC;     /* Disable downcasing on input.  */
#endif
#ifdef OLCUC
  s.main.c_oflag &= ~OLCUC;	/* Disable upcasing on output.  */
#endif
  s.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
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
  s.main.c_cc[VERASE] = CDISABLE; /* disable erase processing */
  s.main.c_cc[VKILL]  = CDISABLE; /* disable kill processing */

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
  /* the QUIT and INTR character are used in process_send_signal
     so set them here to something useful.  */
  s.main.c_cc[VQUIT] = '\\'&037; /* Control-\ */
  s.main.c_cc[VINTR] = 'C' &037; /* Control-C */
#else /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  /* QUIT and INTR work better as signals, so disable character forms */
  s.main.c_cc[VQUIT] = CDISABLE;
  s.main.c_cc[VINTR] = CDISABLE;
  s.main.c_lflag &= ~ISIG;
#endif /* no TIOCGPGRP or no TIOCGLTC or no TIOCGETC */
  s.main.c_cc[VEOL] = CDISABLE;
#if defined (CBAUD)
  /* <mdiers> ### This is not portable. ###
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
  EMACS_SET_TTY (out, &s, 0);

#ifdef RTU
  {
    int zero = 0;
    ioctl (out, FIOASYNC, &zero);
  }
#endif /* RTU */
}
#endif /* WINDOWSNT */
#endif /* not MSDOS */
#endif /* not VMS */

#endif /* not NO_SUBPROCESSES */


#if !defined (VMS) && !defined (SIGTSTP) && !defined (USG_JOBCTRL)

/* Record a signal code and the handler for it.  */
struct save_signal
{
  int code;
  SIGTYPE (*handler) ();
};

static void
save_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      saved_handlers->handler
	= (SIGTYPE (*) ()) signal (saved_handlers->code, SIG_IGN);
      saved_handlers++;
    }
}

static void
restore_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      signal (saved_handlers->code, saved_handlers->handler);
      saved_handlers++;
    }
}

/* Fork a subshell.  */
static void
sys_subshell (void)
{
#ifdef MSDOS
  int st;
  char oldwd[MAXPATHLEN+1]; /* Fixed length is safe on MSDOS.  */
#endif /* MSDOS */
  int pid;
  struct save_signal saved_handlers[5];
  Lisp_Object dir;
  unsigned char *str = 0;
  int len;
  struct gcpro gcpro1;

  saved_handlers[0].code = SIGINT;
  saved_handlers[1].code = SIGQUIT;
  saved_handlers[2].code = SIGTERM;
#ifdef SIGIO
  saved_handlers[3].code = SIGIO;
  saved_handlers[4].code = 0;
#else
  saved_handlers[3].code = 0;
#endif

  /* Mentioning current_buffer->buffer would mean including buffer.h,
     which somehow wedges the hp compiler.  So instead...  */

  if (NILP (Fboundp (Qdefault_directory)))
    goto xyzzy;
  dir = Fsymbol_value (Qdefault_directory);
  if (!STRINGP (dir))
    goto xyzzy;

  GCPRO1 (dir);
  dir = Funhandled_file_name_directory (dir);
  dir = expand_and_dir_to_file (dir, Qnil);
  UNGCPRO;
  str = (unsigned char *) alloca (XSTRING_LENGTH (dir) + 2);
  len = XSTRING_LENGTH (dir);
  memcpy (str, XSTRING_DATA (dir), len);
  /* #### Unix specific */
  if (str[len - 1] != '/') str[len++] = '/';
  str[len] = 0;
 xyzzy:

#ifdef WINDOWSNT
  pid = -1;
#else /* not WINDOWSNT */

  pid = vfork ();

  if (pid == -1)
    error ("Can't spawn subshell");
  if (pid == 0)

#endif /* not WINDOWSNT */
  {
      char *sh = 0;

#ifdef MSDOS    /* MW, Aug 1993 */
      getwd (oldwd);
      if (sh == 0)
	sh = (char *) egetenv ("SUSPEND");	/* KFS, 1994-12-14 */
#endif
      if (sh == 0)
	sh = (char *) egetenv ("SHELL");
      if (sh == 0)
	sh = "sh";

    /* Use our buffer's default directory for the subshell.  */
    if (str)
      sys_chdir (str);

#if !defined (NO_SUBPROCESSES)
    close_process_descs ();	/* Close Emacs's pipes/ptys */
#endif

#ifdef SET_EMACS_PRIORITY
    if (emacs_priority != 0)
      nice (-emacs_priority);   /* Give the new shell the default priority */
#endif

#ifdef MSDOS
    st = system (sh);
    sys_chdir (oldwd);
#if 0	/* This is also reported if last command executed in subshell failed, KFS */
      if (st)
	report_file_error ("Can't execute subshell",
			   Fcons (build_string (sh), Qnil));
#endif
#else /* not MSDOS */
#ifdef WINDOWSNT
      /* Waits for process completion */
      pid = _spawnlp (_P_WAIT, sh, sh, NULL);
      if (pid == -1)
        write (1, "Can't execute subshell", 22);

#if 0
/* This relates to the GNU Emacs console port, not required under X ? */
      take_console ();
#endif
#else   /* not WINDOWSNT */
    execlp (sh, sh, 0);
    write (1, "Can't execute subshell", 22);
    _exit (1);
#endif /* not WINDOWSNT */
#endif /* not MSDOS */
  }

  save_signal_handlers (saved_handlers);
  synch_process_alive = 1;
#ifndef MSDOS
  wait_for_termination (pid);
#endif
  restore_signal_handlers (saved_handlers);
}

#endif /* !defined (VMS) && !defined (SIGTSTP) && !defined (USG_JOBCTRL) */



/* Suspend the Emacs process; give terminal to its superior.  */
void
sys_suspend (void)
{
#ifdef VMS
  /* "Foster" parentage allows emacs to return to a subprocess that attached
     to the current emacs as a cheaper than starting a whole new process.  This
     is set up by KEPTEDITOR.COM.  */
  unsigned long parent_id, foster_parent_id;
  char *fpid_string;

  fpid_string = getenv ("EMACS_PARENT_PID");
  if (fpid_string != NULL)
    {
      sscanf (fpid_string, "%x", &foster_parent_id);
      if (foster_parent_id != 0)
	parent_id = foster_parent_id;
      else
	parent_id = getppid ();
    }
  else
    parent_id = getppid ();

  xfree (fpid_string);		/* On VMS, this was malloc'd */

  if (parent_id && parent_id != 0xffffffff)
    {
      SIGTYPE (*oldsig)() = (int) signal (SIGINT, SIG_IGN);
      int status = LIB$ATTACH (&parent_id) & 1;
      signal (SIGINT, oldsig);
      return status;
    }
  else
    {
      struct {
	int	l;
	char	*a;
      } d_prompt;
      d_prompt.l = sizeof ("Emacs: ");		/* Our special prompt */
      d_prompt.a = "Emacs: ";			/* Just a reminder */
      LIB$SPAWN (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &d_prompt, 0);
      return 1;
    }
  return -1;
#elif defined (SIGTSTP) && !defined (MSDOS)
  {
    int pgrp = EMACS_GET_PROCESS_GROUP ();
    EMACS_KILLPG (pgrp, SIGTSTP);
  }

#elif defined (USG_JOBCTRL)
  /* If you don't know what this is don't mess with it */
  ptrace (0, 0, 0, 0);		/* set for ptrace - caught by csh */
  kill (getpid (), SIGQUIT);

#else /* No SIGTSTP or USG_JOBCTRL */

  /* On a system where suspending is not implemented,
     instead fork a subshell and let it talk directly to the terminal
     while we wait.  */
  sys_subshell ();

#endif
}

/* Suspend a process if possible; give terminal to its superior.  */
void
sys_suspend_process (int process)
{
    /* I don't doubt that it is possible to suspend processes on
     * VMS machines or thost that use USG_JOBCTRL,
     * but I don't know how to do it, so...
     */
#if defined (SIGTSTP) && !defined (MSDOS)
    kill(process, SIGTSTP);
#endif
}

/* Set the logical window size associated with descriptor FD
   to HEIGHT and WIDTH.  This is used mainly with ptys.  */

int
set_window_size (int fd, int height, int width)
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

#ifdef HAVE_PTYS

/* Set up the proper status flags for use of a pty.  */

void
setup_pty (int fd)
{
  /* I'm told that TOICREMOTE does not mean control chars
     "can't be sent" but rather that they don't have
     input-editing or signaling effects.
     That should be good, because we have other ways
     to do those things in Emacs.
     However, telnet mode seems not to work on 4.2.
     So TIOCREMOTE is turned off now. */

  /* Under hp-ux, if TIOCREMOTE is turned on, some calls
     will hang.  In particular, the "timeout" feature (which
     causes a read to return if there is no data available)
     does this.  Also it is known that telnet mode will hang
     in such a way that Emacs must be stopped (perhaps this
     is the same problem).

     If TIOCREMOTE is turned off, then there is a bug in
     hp-ux which sometimes loses data.  Apparently the
     code which blocks the master process when the internal
     buffer fills up does not work.  Other than this,
     though, everything else seems to work fine.

     Since the latter lossage is more benign, we may as well
     lose that way.  -- cph */
#if defined (FIONBIO) && defined (SYSV_PTYS)
  {
    int on = 1;
    ioctl (fd, FIONBIO, &on);
  }
#endif
#ifdef IBMRTAIX
  /* On AIX, the parent gets SIGHUP when a pty attached child dies.  So, we */
  /* ignore SIGHUP once we've started a child on a pty.  Note that this may */
  /* cause EMACS not to die when it should, i.e., when its own controlling  */
  /* tty goes away.  I've complained to the AIX developers, and they may    */
  /* change this behavior, but I'm not going to hold my breath.             */
  signal (SIGHUP, SIG_IGN);
#endif
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
#endif
}
#endif /* HAVE_PTYS */


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
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));
  if (DEVICE_WIN_P (d) || DEVICE_STREAM_P (d))
    {
      DEVICE_BAUD_RATE (d) = 38400;
      return;
    }

#ifdef HAVE_TTY
  assert (DEVICE_TTY_P (d));
  {
    int input_fd = CONSOLE_TTY_DATA (con)->infd;
#ifdef MSDOS
    DEVICE_TTY_DATA (d)->ospeed = 15;
#elif defined (VMS)
    struct vms_sensemode sg;

    SYS$QIOW (0, input_fd, IO$_SENSEMODE, &sg, 0, 0,
	      &sg.class, 12, 0, 0, 0, 0 );
    DEVICE_TTY_DATA (d)->ospeed = sg.xmit_baud;
#elif defined (HAVE_TERMIOS)
    struct termios sg;

    sg.c_cflag = B9600;
    tcgetattr (input_fd, &sg);
    DEVICE_TTY_DATA (d)->ospeed = cfgetospeed (&sg);
# if defined (USE_GETOBAUD) && defined (getobaud)
    /* m88k-motorola-sysv3 needs this (ghazi@noc.rutgers.edu) 9/1/94. */
    if (DEVICE_TTY_DATA (d)->ospeed == 0)
      DEVICE_TTY_DATA (d)->ospeed = getobaud (sg.c_cflag);
# endif
#elif defined (HAVE_TERMIO)
    struct termio sg;

    sg.c_cflag = B9600;
# ifdef HAVE_TCATTR
    tcgetattr (input_fd, &sg);
# else
    ioctl (input_fd, TCGETA, &sg);
# endif
    DEVICE_TTY_DATA (d)->ospeed = sg.c_cflag & CBAUD;
#else /* neither VMS nor TERMIOS nor TERMIO */
    struct sgttyb sg;

    sg.sg_ospeed = B9600;
    if (ioctl (input_fd, TIOCGETP, &sg) < 0)
      abort ();
    DEVICE_TTY_DATA (d)->ospeed = sg.sg_ospeed;
#endif
  }

  DEVICE_BAUD_RATE (d) =
    (DEVICE_TTY_DATA (d)->ospeed < sizeof baud_convert / sizeof baud_convert[0]
     ? baud_convert[DEVICE_TTY_DATA (d)->ospeed]
     : 9600);

  if (DEVICE_BAUD_RATE (d) == 0)
    DEVICE_BAUD_RATE (d) = 1200;
#endif /* HAVE_TTY */
}


/* ------------------------------------------------------ */
/*                       SIGIO control                    */
/* ------------------------------------------------------ */

#ifdef SIGIO

static void
init_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

#if defined (I_SETSIG) && !defined(HPUX10)
  ioctl (filedesc, I_GETSIG, &DEVICE_OLD_SIGIO_FLAG (d));
  DEVICE_OLD_SIGIO_FLAG (d) &= ~S_INPUT;
#elif defined (FASYNC)
  DEVICE_OLD_SIGIO_FLAG (d) =
    fcntl (filedesc, F_GETFL, 0) & ~FASYNC;
#endif

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
# ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (filedesc, F_SETOWN, -getpid ());
# else
  fcntl (filedesc, F_SETOWN, getpid ());
# endif
#endif
}

static void
reset_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

#if defined (FIOSSAIOOWN)
  { /* HPUX stuff */
    int owner = getpid ();
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

  /* prevent redundant ioctl()s, which may cause syslog messages
     (e.g. on Solaris) */
  if (d->sigio_enabled)
    return;

#if defined (I_SETSIG) && !defined(HPUX10)
  ioctl (filedesc, I_SETSIG, DEVICE_OLD_SIGIO_FLAG (d) | S_INPUT);
#elif defined (FASYNC)
  fcntl (filedesc, F_SETFL, DEVICE_OLD_SIGIO_FLAG (d) | FASYNC);
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

#if defined (_CX_UX) /* #### Is this crap necessary? */
  EMACS_UNBLOCK_SIGNAL (SIGIO);
#endif

  d->sigio_enabled = 1;
}

static void
unrequest_sigio_on_device (struct device *d)
{
  int filedesc = DEVICE_INFD (d);

  /* prevent redundant ioctl()s, which may cause syslog messages
     (e.g. on Solaris) */
  if (!d->sigio_enabled)
    return;

#if defined (I_SETSIG) && !defined(HPUX10)
  ioctl (filedesc, I_SETSIG, DEVICE_OLD_SIGIO_FLAG (d));
#elif defined (FASYNC)
  fcntl (filedesc, F_SETFL, DEVICE_OLD_SIGIO_FLAG (d));
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

  d->sigio_enabled = 0;
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

static int inherited_pgroup;
static int inherited_tty_pgroup;

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
      int me = getpid ();
      EMACS_BLOCK_SIGNAL (SIGTTOU);
      EMACS_SET_TTY_PROCESS_GROUP (fd, &me);
      EMACS_UNBLOCK_SIGNAL (SIGTTOU);
      close (fd);
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
    close (fd);
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
      close (fd);
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
    close (j);
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
#ifndef WINDOWSNT

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

#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, TCGETA, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SENSEMODE, settings, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
#ifndef MSDOS
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, TIOCGETP, &settings->main) < 0)
    return -1;
#endif /* not MSDOS */
#endif /* not VMS */
#endif /* HAVE_TERMIO */
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
   Return 0 if all went well, and -1 if anything failed.  */

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
	struct termios new;

	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 */
	if (   new.c_iflag == settings->main.c_iflag
	    && new.c_oflag == settings->main.c_oflag
	    && new.c_cflag == settings->main.c_cflag
	    && new.c_lflag == settings->main.c_lflag
	    && memcmp(new.c_cc, settings->main.c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }
#else
#ifdef HAVE_TERMIO
  /* The SYSV-style interface?  */
  if (ioctl (fd, flushp ? TCSETAF : TCSETAW, &settings->main) < 0)
    return -1;

#else
#ifdef VMS
  /* Vehemently Monstrous System?  :-)  */
  if (! (SYS$QIOW (0, fd, IO$_SETMODE, &input_iosb, 0, 0,
		   &settings->main.class, 12, 0, 0, 0, 0)
	 & 1))
    return -1;

#else
#ifndef MSDOS
  /* I give up - I hope you have the BSD ioctls.  */
  if (ioctl (fd, (flushp) ? TIOCSETP : TIOCSETN, &settings->main) < 0)
    return -1;
#endif /* not MSDOS */
#endif /* VMS */
#endif /* HAVE_TERMIO */
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

#endif /* WINDOWSNT */

/* ------------------------------------------------------ */
/*                 Initializing a device                  */
/* ------------------------------------------------------ */

#ifdef HAVE_TTY

/* This may also be defined in stdio,
   but if so, this does no harm,
   and using the same name avoids wasting the other one's space.  */

#if ((defined(USG) || defined(DGUX)) && !defined(__STDC__))
char _sobuf[BUFSIZ+8];
#elif (defined(USG) && !defined(LINUX) && !defined(_SCO_DS)) || defined(IRIX5)
extern unsigned char _sobuf[BUFSIZ+8];
#else
char _sobuf[BUFSIZ];
#endif

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
  int input_fd, output_fd;
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

  input_fd = CONSOLE_TTY_DATA (con)->infd;
  output_fd = CONSOLE_TTY_DATA (con)->outfd;

  EMACS_GET_TTY (input_fd, &CONSOLE_TTY_DATA (con)->old_tty);
  tty = CONSOLE_TTY_DATA (con)->old_tty;

  con->tty_erase_char = Qnil;

#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
  /* after all those years... */
  con->tty_erase_char = make_char (tty.main.c_cc[VERASE]);
#ifdef DGUX
  /* This allows meta to be sent on 8th bit.  */
  tty.main.c_iflag &= ~INPCK;	/* don't check input for parity */
#endif
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
  tty.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
#ifdef CS8
  if (TTY_FLAGS (con).meta_key)
    {
      tty.main.c_cflag |= CS8;	/* allow 8th bit on input */
      tty.main.c_cflag &= ~PARENB;/* Don't check parity */
    }
#endif
  if (CONSOLE_TTY_DATA (con)->controlling_terminal)
    {
      tty.main.c_cc[VINTR] =
	CONSOLE_QUIT_CHAR (con); /* C-g (usually) gives SIGINT */
      /* Set up C-g for both SIGQUIT and SIGINT.
	 We don't know which we will get, but we handle both alike
	 so which one it really gives us does not matter.  */
      tty.main.c_cc[VQUIT] = CONSOLE_QUIT_CHAR (con);
    }
  else
    {
      tty.main.c_cc[VINTR] = CDISABLE;
      tty.main.c_cc[VQUIT] = CDISABLE;
    }
  tty.main.c_cc[VMIN] = 1;	/* Input should wait for at
				   least 1 char */
  tty.main.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
  tty.main.c_cc[VSWTCH] = CDISABLE;	/* Turn off shell layering use
					   of C-z */
#endif /* VSWTCH */
  /* There was some conditionalizing here on (mips or TCATTR), but
     I think that's wrong.  There was one report of C-y (DSUSP) not being
     disabled on HP9000s700 systems, and this might fix it. */
#ifdef VSUSP
  tty.main.c_cc[VSUSP] = CDISABLE;/* Turn off mips handling of C-z.  */
#endif /* VSUSP */
#ifdef V_DSUSP
  tty.main.c_cc[V_DSUSP] = CDISABLE; /* Turn off mips handling of C-y.  */
#endif /* V_DSUSP */
#ifdef VDSUSP /* Some systems have VDSUSP, some have V_DSUSP.  */
  tty.main.c_cc[VDSUSP] = CDISABLE;
#endif /* VDSUSP */
#ifdef VLNEXT
  tty.main.c_cc[VLNEXT] = CDISABLE;
#endif /* VLNEXT */
#ifdef VREPRINT
  tty.main.c_cc[VREPRINT] = CDISABLE;
#endif /* VREPRINT */
#ifdef VWERASE
  tty.main.c_cc[VWERASE] = CDISABLE;
#endif /* VWERASE */
#ifdef VDISCARD
  tty.main.c_cc[VDISCARD] = CDISABLE;
#endif /* VDISCARD */
#ifdef VSTART
  tty.main.c_cc[VSTART] = CDISABLE;
#endif /* VSTART */
#ifdef VSTRT
  tty.main.c_cc[VSTRT] = CDISABLE; /* called VSTRT on some systems */
#endif /* VSTART */
#ifdef VSTOP
  tty.main.c_cc[VSTOP] = CDISABLE;
#endif /* VSTOP */
#ifdef SET_LINE_DISCIPLINE
  /* Need to explicitely request TERMIODISC line discipline or
     Ultrix's termios does not work correctly.  */
  tty.main.c_line = SET_LINE_DISCIPLINE;
#endif

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
#ifndef MSDOS
  con->tty_erase_char = make_char (tty.main.sg_erase);
  tty.main.sg_flags &= ~(ECHO | CRMOD | XTABS);
  if (TTY_FLAGS (con).meta_key)
    tty.main.sg_flags |= ANYP;
  /* #### should we be using RAW mode here? */
  tty.main.sg_flags |= /* interrupt_input ? RAW : */ CBREAK;
#endif /* not MSDOS */
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
  tty.tchars.t_intrc = CONSOLE_QUIT_CHAR (con);
  if (TTY_FLAGS (con).flow_control)
    {
      tty.tchars.t_startc = '\021';
      tty.tchars.t_stopc = '\023';
    }

  tty.lmode = LDECCTQ | LLITOUT | LPASS8 | LNOFLSH |
    CONSOLE_TTY_DATA (con)->old_tty.lmode;

#if defined (ultrix) || defined (__bsdi__)
  /* Under Ultrix 4.2a, leaving this out doesn't seem to hurt
     anything, and leaving it in breaks the meta key.  Go figure.  */
  /* Turning off ONLCR is enough under BSD/386.  Leave the general
     output post-processing flag alone since for some reason it
     doesn't get reset after XEmacs goes away. */
  tty.lmode &= ~LLITOUT;
#endif

#endif /* HAVE_TCHARS */
#endif /* not HAVE_TERMIO */

#ifdef HAVE_LTCHARS
  tty.ltchars = new_ltchars;
#endif /* HAVE_LTCHARS */
#ifdef MSDOS
  internal_terminal_init ();
  dos_ttraw ();
#endif

  EMACS_SET_TTY (input_fd, &tty, 0);

  /* This code added to insure that, if flow-control is not to be used,
     we have an unlocked terminal at the start. */

#ifdef TCXONC
  if (!TTY_FLAGS (con).flow_control) ioctl (input_fd, TCXONC, 1);
#endif
#ifndef APOLLO
#ifdef TIOCSTART
  if (!TTY_FLAGS (con).flow_control) ioctl (input_fd, TIOCSTART, 0);
#endif
#endif

#if defined (HAVE_TERMIOS) || defined (HPUX9)
#ifdef TCOON
  if (!TTY_FLAGS (con).flow_control) tcflow (input_fd, TCOON);
#endif
#endif
#ifdef AIXHFT
  hft_init (con);
#ifdef IBMR2AIX
  {
    /* IBM's HFT device usually thinks a ^J should be LF/CR.
       We need it to be only LF.  This is the way that is
       done. */
    struct termio tty;

    if (ioctl (output_fd, HFTGETID, &tty) != -1)
      write (output_fd, "\033[20l", 5);
  }
#endif
#endif

#ifdef VMS
  /*  Appears to do nothing when in PASTHRU mode.
      SYS$QIOW (0, input_fd, IO$_SETMODE|IO$M_OUTBAND, 0, 0, 0,
      interrupt_signal, oob_chars, 0, 0, 0, 0);
      */
  queue_kbd_input (0);
#endif /* VMS */

#if 0 /* We do our own buffering with lstreams. */
#ifdef _IOFBF
  /* This symbol is defined on recent USG systems.
     Someone says without this call USG won't really buffer the file
     even with a call to setbuf. */
  setvbuf (CONSOLE_TTY_DATA (con)->outfd, (char *) _sobuf, _IOFBF, sizeof _sobuf);
#else
  setbuf (CONSOLE_TTY_DATA (con)->outfd, (char *) _sobuf);
#endif
#endif
  set_tty_modes (con);
}

#endif /* HAVE_TTY */

void
init_one_device (struct device *d)
{
#ifdef HAVE_TTY
  if (DEVICE_TTY_P (d))
    tty_init_sys_modes_on_device (d);
#endif
#ifdef SIGIO
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
tabs_safe_p (struct device *d)
{
#ifdef HAVE_TTY
  if (DEVICE_TTY_P (d))
    {
      struct emacs_tty tty;

      EMACS_GET_TTY (DEVICE_INFD (d), &tty);
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
#else
#ifdef TIOCGSIZE
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
#else
#ifdef VMS
  {
    struct vms_sensemode tty;

    SYS$QIOW (0, input_fd, IO$_SENSEMODE, &tty, 0, 0,
	      &tty.class, 12, 0, 0, 0, 0);
    *widthp = tty.scr_wid;
    *heightp = tty.scr_len;
  }
#else
#ifdef MSDOS

  *widthp = FrameCols ();
  *heightp = FrameRows ();

#else /* system doesn't know size */

  *widthp = 0;
  *heightp = 0;

#endif /* not MSDOS */
#endif /* not VMS */
#endif /* not SunOS-style */
#endif /* not BSD-style */
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

  EMACS_GET_TTY (input_fd, &s);

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
  int input_fd, output_fd;
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

  input_fd = CONSOLE_TTY_DATA (con)->infd;
  output_fd = CONSOLE_TTY_DATA (con)->outfd;

#if defined (IBMR2AIX) && defined (AIXHFT)
  {
    /* HFT consoles normally use ^J as a LF/CR.  We forced it to
       do the LF only.  Now, we need to reset it. */
    struct termio tty;

    if (ioctl (output_fd, HFTGETID, &tty) != -1)
      write (output_fd, "\033[20h", 5);
  }
#endif

  tty_redisplay_shutdown (con);
  /* reset_tty_modes() flushes the connection at its end. */
  reset_tty_modes (con);

#if defined (BSD)
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (output_fd);
#endif

  while (EMACS_SET_TTY (input_fd, &CONSOLE_TTY_DATA (con)->old_tty, 0)
	 < 0 && errno == EINTR)
    ;

#ifdef MSDOS
  dos_ttcooked ();
#endif

#ifdef SET_LINE_DISCIPLINE
  /* Ultrix's termios *ignores* any line discipline except TERMIODISC.
     A different old line discipline is therefore not restored, yet.
     Restore the old line discipline by hand.  */
  ioctl (input_fd, TIOCSETD, &old_tty.main.c_line);
#endif

#ifdef AIXHFT
  hft_reset (con);
#endif

#ifdef VMS
  stop_vms_input (con);
#endif
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
    fflush (CONSOLE_STREAM_DATA (XCONSOLE (DEVICE_CONSOLE (d)))->outfd);
#ifdef SIGIO
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


/* ------------------------------------------------------ */
/*                 extra TTY stuff under AIX              */
/* ------------------------------------------------------ */

#ifdef AIXHFT

/* Called from init_sys_modes.  */
static void
hft_init (struct console *con)
{
  int junk;
  int input_fd;

  assert (CONSOLE_TTY_P (con));
  input_fd = CONSOLE_TTY_DATA (con)->infd;

  /* If we're not on an HFT we shouldn't do any of this.  We determine
     if we are on an HFT by trying to get an HFT error code.  If this
     call fails, we're not on an HFT. */
#ifdef IBMR2AIX
  if (ioctl (input_fd, HFQERROR, &junk) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (input_fd, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  /* On AIX the default hft keyboard mapping uses backspace rather than delete
     as the rubout key's ASCII code.  Here this is changed.  The bug is that
     there's no way to determine the old mapping, so in reset_one_console
     we need to assume that the normal map had been present.  Of course, this
     code also doesn't help if on a terminal emulator which doesn't understand
     HFT VTD's. */
  {
    struct hfbuf buf;
    struct hfkeymap keymap;

    buf.hf_bufp = (char *)&keymap;
    buf.hf_buflen = sizeof (keymap);
    keymap.hf_nkeys = 2;
    keymap.hfkey[0].hf_kpos = 15;
    keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
    keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[0].hf_char = 127;
    keymap.hfkey[1].hf_kpos = 15;
    keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
    keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[1].hf_char = 127;
    hftctl (input_fd, HFSKBD, &buf);
  }
  /* #### Should probably set a console TTY flag here. */
#if 0
  /* The HFT system on AIX doesn't optimize for scrolling, so it's really ugly
     at times. */
  line_ins_del_ok = char_ins_del_ok = 0;
#endif /* 0 */
}

/* Reset the rubout key to backspace. */

static void
hft_reset (struct console *con)
{
  struct hfbuf buf;
  struct hfkeymap keymap;
  int junk;
  int input_fd;

  assert (CONSOLE_TTY_P (con));
  input_fd = CONSOLE_TTY_DATA (con)->infd;

#ifdef IBMR2AIX
  if (ioctl (input_fd, HFQERROR, &junk) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (input_fd, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  buf.hf_bufp = (char *)&keymap;
  buf.hf_buflen = sizeof (keymap);
  keymap.hf_nkeys = 2;
  keymap.hfkey[0].hf_kpos = 15;
  keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
  keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[0].hf_char = 8;
  keymap.hfkey[1].hf_kpos = 15;
  keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
  keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[1].hf_char = 8;
  hftctl (input_fd, HFSKBD, &buf);
}

#endif /* AIXHFT */


/* ------------------------------------------------------ */
/*                   TTY stuff under VMS                  */
/* ------------------------------------------------------ */

/***** #### this is all broken ****/

#ifdef VMS

/* Assigning an input channel is done at the start of Emacs execution.
   This is called each time Emacs is resumed, also, but does nothing
   because input_chain is no longer zero.  */

void
init_vms_input (void)
{
  /* #### broken. */
  int status;

  if (input_fd == 0)
    {
      status = SYS$ASSIGN (&vms_input_dsc, &input_fd, 0, 0);
      if (! (status & 1))
	LIB$STOP (status);
    }
}

/* Deassigning the input channel is done before exiting.  */

static void
stop_vms_input (struct console *con)
{
  int input_fd = CONSOLE_TTY_DATA (con)->infd;
  return SYS$DASSGN (input_fd);
}

static short vms_input_buffer;

/* Request reading one character into the keyboard buffer.
   This is done as soon as the buffer becomes empty.  */

static void
queue_vms_kbd_input (struct console *con)
{
  int input_fd = CONSOLE_TTY_DATA (con)->infd;
  int status;
  vms_waiting_for_ast = 0;
  vms_stop_input = 0;
  status = SYS$QIO (0, input_fd, IO$_READVBLK,
		    &vms_input_iosb, vms_kbd_input_ast, 1,
		    &vms_input_buffer, 1, 0, vms_terminator_mask, 0, 0);
}

static int vms_input_count;

/* Ast routine that is called when keyboard input comes in
   in accord with the SYS$QIO above.  */

static void
vms_kbd_input_ast (struct console *con)
{
  int c = -1;
  int old_errno = errno;
  extern EMACS_TIME *input_available_clear_time;

  if (vms_waiting_for_ast)
    SYS$SETEF (vms_input_ef);
  vms_waiting_for_ast = 0;
  vms_input_count++;
#ifdef ASTDEBUG
  if (vms_input_count == 25)
    exit (1);
  printf ("Ast # %d,", vms_input_count);
  printf (" iosb = %x, %x, %x, %x",
	  vms_input_iosb.offset, vms_input_iosb.status,
          vms_input_iosb.termlen, vms_input_iosb.term);
#endif
  if (vms_input_iosb.offset)
    {
      c = vms_input_buffer;
#ifdef ASTDEBUG
      printf (", char = 0%o", c);
#endif
    }
#ifdef ASTDEBUG
  printf ("\n");
  fflush (stdout);
  emacs_sleep (1);
#endif
  if (! vms_stop_input)
    queue_vms_kbd_input (con);
  if (c >= 0)
    kbd_buffer_store_char (c);

  if (input_available_clear_time)
    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
  errno = old_errno;
}

#if 0 /* Unused */
/* Wait until there is something in kbd_buffer.  */

void
vms_wait_for_kbd_input (void)
{
  /* This function can GC */
  extern int have_process_input, process_exited;

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending (0))
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending (0))
    {
      /* No timing error: wait for flag to be set.  */
      set_waiting_for_input (0);
      SYS$WFLOR (vms_input_ef, vms_input_eflist);
      clear_waiting_for_input (0);
      if (!detect_input_pending (0))
	/* Check for subprocess input availability */
	{
	  int dsp = have_process_input || process_exited;

	  SYS$CLREF (vms_process_ef);
	  if (have_process_input)
	    process_command_input ();
	  if (process_exited)
	    process_exit ();
	  if (dsp)
	    {
	      MARK_MODELINE_CHANGED;
	      redisplay ();
	    }
	}
    }
  vms_waiting_for_ast = 0;
}
#endif

/* Get rid of any pending QIO, when we are about to suspend
   or when we want to throw away pending input.
   We wait for a positive sign that the AST routine has run
   and therefore there is no I/O request queued when we return.
   SYS$SETAST is used to avoid a timing error.  */

static void
vms_end_kbd_input (struct console *con)
{
  int input_fd;

  assert (CONSOLE_TTY_P (con));
  input_fd = CONSOLE_TTY_DATA (con)->infd;
#ifdef ASTDEBUG
  printf ("At end_kbd_input.\n");
  fflush (stdout);
  emacs_sleep (1);
#endif
  if (LIB$AST_IN_PROG ())  /* Don't wait if suspending from kbd_buffer_store_char! */
    {
      SYS$CANCEL (input_fd);
      return;
    }

  SYS$SETAST (0);
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  vms_stop_input = 1;
  SYS$CANCEL (input_fd);
  SYS$SETAST (1);
  SYS$WAITFR (vms_input_ef);
  vms_waiting_for_ast = 0;
}

#if 0 /* Unused */
/* Wait for either input available or time interval expiry.  */

void
vms_input_wait_timeout (int timeval) /* Time to wait, in seconds */
{
  int time [2];
  static int zero = 0;
  static int large = -10000000;

  LIB$EMUL (&timeval, &large, &zero, time); 	  /* Convert to VMS format */

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending (0))
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (vms_input_ef);
  vms_waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending (0))
    {
      /* No timing error: wait for flag to be set.  */
      SYS$CANTIM (1, 0);
      if (SYS$SETIMR (vms_timer_ef, time, 0, 1) & 1) /* Set timer */
	SYS$WFLOR (vms_timer_ef, vms_timer_eflist);  /* Wait for timer expiry or input */
    }
  vms_waiting_for_ast = 0;
}
#endif /* 0 */

#endif /* VMS */


/************************************************************************/
/*                    limits of text/data segments                      */
/************************************************************************/

/* Note that VMS compiler won't accept defined (CANNOT_DUMP).  */
#ifndef CANNOT_DUMP
#define NEED_STARTS
#endif

#ifndef SYSTEM_MALLOC
#ifndef NEED_STARTS
#define NEED_STARTS
#endif
#endif

#ifdef NEED_STARTS
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the start of the text segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See crt0.c for further explanation and _start.
 *
 */

#ifdef __cplusplus
  extern "C" int _start ();
#else
  extern int _start ();
#endif

#ifndef HAVE_TEXT_START
char *
start_of_text (void)
{
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
#ifdef GOULD
  extern csrt ();
  return ((char *) csrt);
#else /* not GOULD */
  return ((char *) _start);
#endif /* GOULD */
#endif /* TEXT_START */
}
#endif /* not HAVE_TEXT_START */

/*
 *	Return the address of the start of the data segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 *	See crt0.c for further information and definition of data_start.
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
 *	at the normal shared text boundry and the startofdata variable
 *	will be patched by unexec to the correct value.
 *
 */

void *
start_of_data (void)
{
#ifdef DATA_START
  return ((char *) DATA_START);
#else
#ifdef ORDINARY_LINK
  /*
   * This is a hack.  Since we're not linking crt0.c or pre_crt0.c,
   * data_start isn't defined.  We take the address of environ, which
   * is known to live at or near the start of the system crt0.c, and
   * we don't sweat the handful of bytes that might lose.
   */
  extern char **environ;

  return((char *) &environ);
#else
  extern int data_start;
  return ((char *) &data_start);
#endif /* ORDINARY_LINK */
#endif /* DATA_START */
}
#endif /* NEED_STARTS (not CANNOT_DUMP or not SYSTEM_MALLOC) */

#ifndef CANNOT_DUMP
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the end of the text segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 */

char *
end_of_text (void)
{
#ifdef TEXT_END
  return ((char *) TEXT_END);
#else
  extern int etext;
  return ((char *) &etext);
#endif
}

/*
 *	Return the address of the end of the data segment prior to
 *	doing an unexec.  After unexec the return value is undefined.
 */

char *
end_of_data (void)
{
#ifdef DATA_END
  return ((char *) DATA_END);
#else
  extern int edata;
  return ((char *) &edata);
#endif
}

#endif /* not CANNOT_DUMP */


/************************************************************************/
/*                          get the system name                         */
/************************************************************************/

/* init_system_name sets up the string for the Lisp function
   system-name to return. */

extern Lisp_Object Vsystem_name;

#if defined (HAVE_SOCKETS) && !defined (VMS)
# include <sys/socket.h>
# include <netdb.h>
#endif /* HAVE_SOCKETS and not VMS */

void
init_system_name (void)
{
#if defined (VMS)
  char *sp, *end;
  if ((sp = egetenv ("SYS$NODE")) == 0)
    Vsystem_name = build_string ("vax-vms");
  else if ((end = strchr (sp, ':')) == 0)
    Vsystem_name = build_string (sp);
  else
    Vsystem_name = make_string ((Bufbyte *) sp, end - sp);
#elif !defined (HAVE_GETHOSTNAME)
  struct utsname uts;
  uname (&uts);
  Vsystem_name = build_string (uts.nodename);
#else /* HAVE_GETHOSTNAME */
  unsigned int hostname_size = 256;
  char *hostname = (char *) alloca (hostname_size);

  /* Try to get the host name; if the buffer is too short, try
     again.  Apparently, the only indication gethostname gives of
     whether the buffer was large enough is the presence or absence
     of a '\0' in the string.  Eech.  */
  for (;;)
    {
      gethostname (hostname, hostname_size - 1);
      hostname[hostname_size - 1] = '\0';

      /* Was the buffer large enough for the '\0'?  */
      if (strlen (hostname) < (size_t) (hostname_size - 1))
	break;

      hostname_size <<= 1;
      hostname = (char *) alloca (hostname_size);
    }
# ifdef HAVE_SOCKETS
  /* Turn the hostname into the official, fully-qualified hostname.
     Don't do this if we're going to dump; this can confuse system
     libraries on some machines and make the dumped emacs core dump. */
#  ifndef CANNOT_DUMP
  if (initialized)
#  endif /* not CANNOT_DUMP */
    {
      struct hostent *hp;
      int count;
#  ifdef TRY_AGAIN
      for (count = 0; count < 10; count++)
	{
	  h_errno = 0;
#  endif
	  /* Some systems can't handle SIGALARM/SIGIO in gethostbyname(). */
	  stop_interrupts ();
	  hp = gethostbyname (hostname);
	  start_interrupts ();
#  ifdef TRY_AGAIN
	  if (! (hp == 0 && h_errno == TRY_AGAIN))
	    break;
	  Fsleep_for (make_int (1));
	}
#  endif
      if (hp)
	{
	  CONST char *fqdn = (CONST char *) hp->h_name;

	  if (!strchr (fqdn, '.'))
	    {
	      /* We still don't have a fully qualified domain name.
		 Try to find one in the list of alternate names */
	      char **alias = hp->h_aliases;
	      while (*alias && !strchr (*alias, '.'))
		alias++;
	      if (*alias)
		fqdn = *alias;
	    }
	  hostname = (char *) alloca (strlen (fqdn) + 1);
	  strcpy (hostname, fqdn);
	}
    }
# endif /* HAVE_SOCKETS */
  Vsystem_name = build_string (hostname);
#endif /* HAVE_GETHOSTNAME and not VMS */
  {
    Bufbyte *p;
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

#ifndef VMS
#ifndef HAVE_SELECT

ERROR: XEmacs requires a working select().

#endif /* not HAVE_SELECT */
#endif /* not VMS */


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

/* #### Is there any reason this is static global rather than local? */
static struct sigaction new_action, old_action;

signal_handler_t
sys_do_signal (int signal_number, signal_handler_t action)
{
#if 0

  /* XEmacs works better if system calls are *not* restarted.
     This allows C-g to interrupt reads and writes, on most systems.

     #### Another possibility is to just longjmp() out of the signal
     handler.  According to W.R. Stevens, this should be OK on all
     systems.  However, I don't want to deal with the potential
     evil ramifications of this at this point. */

#ifdef DGUX
  /* This gets us restartable system calls for efficiency.
     The "else" code will work as well. */
  return (berk_signal (signal_number, action));
#else
  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
#if defined (SA_RESTART)
  /* Emacs mostly works better with restartable system services. If this
   * flag exists, we probably want to turn it on here.
   */
  new_action.sa_flags = SA_RESTART;
#else
  new_action.sa_flags = 0;
#endif
  sigaction (signal_number, &new_action, &old_action);
  return (old_action.sa_handler);
#endif /* DGUX */

#else /* not 0 */

  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
#if defined (SA_INTERRUPT) /* don't restart system calls, under SunOS */
  new_action.sa_flags = SA_INTERRUPT;
#else
  new_action.sa_flags = 0;
#endif
  sigaction (signal_number, &new_action, &old_action);
  return (signal_handler_t) (old_action.sa_handler);

#endif /* not 0 */
}

#elif defined (HAVE_SIGBLOCK)

/* We use sigvec() rather than signal() if we have it, because
   it lets us specify interruptible system calls. */
signal_handler_t
sys_do_signal (int signal_number, signal_handler_t action)
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
/*                        Emulation of strerror()                       */
/************************************************************************/

#ifndef HAVE_STRERROR

#if defined (VMS) && defined (LINK_CRTL_SHARE) && defined (SHAREABLE_LIB_BUG)

/* Variables declared noshare and initialized in sharable libraries
   cannot be shared.  The VMS linker incorrectly forces you to use a private
   version which is uninitialized... If not for this "feature", we
   could use the C library definition of sys_nerr and sys_errlist. */
CONST char *sys_errlist[] =
  {
    "error 0",
    "not owner",
    "no such file or directory",
    "no such process",
    "interrupted system call",
    "I/O error",
    "no such device or address",
    "argument list too long",
    "exec format error",
    "bad file number",
    "no child process",
    "no more processes",
    "not enough memory",
    "permission denied",
    "bad address",
    "block device required",
    "mount devices busy",
    "file exists",
    "cross-device link",
    "no such device",
    "not a directory",
    "is a directory",
    "invalid argument",
    "file table overflow",
    "too many open files",
    "not a typewriter",
    "text file busy",
    "file too big",
    "no space left on device",
    "illegal seek",
    "read-only file system",
    "too many links",
    "broken pipe",
    "math argument",
    "result too large",
    "I/O stream empty",
    "vax/vms specific error code nontranslatable error"
  };
int sys_nerr = countof (sys_errlist);

#endif /* VMS & LINK_CRTL_SHARE & SHAREABLE_LIB_BUG */


#if !defined(NeXT) && !defined(__alpha) && !defined(MACH) && !defined(LINUX) && !defined(IRIX) && !defined(__NetBSD__)
/* Linux added here by Raymond L. Toy <toy@alydar.crd.ge.com> for XEmacs. */
/* Irix added here by gparker@sni-usa.com for XEmacs. */
/* NetBSD added here by James R Grinter <jrg@doc.ic.ac.uk> for XEmacs */
extern CONST char *sys_errlist[];
extern int sys_nerr;
#endif

#ifdef __NetBSD__
extern char *sys_errlist[];
extern int sys_nerr;
#endif


CONST char *
strerror (int errnum)
{
  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return ((CONST char *) GETTEXT ("Unknown error"));
}

#endif /* ! HAVE_STRERROR */



/************************************************************************/
/*                    Encapsulations of system calls                    */
/************************************************************************/

#define PATHNAME_CONVERT_OUT(path) \
  GET_C_CHARPTR_EXT_FILENAME_DATA_ALLOCA (path, path)

/***** VMS versions are at the bottom of this file *****/
/***** MSDOS versions are in msdos.c *****/

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
   interruptible-system-call business.  To find it, look at
   Jamie's home page (http://www.netscape.com/people/jwz). */

#ifdef ENCAPSULATE_OPEN
int
sys_open (CONST char *path, int oflag, ...)
{
  int mode;
  va_list ap;

  va_start (ap, oflag);
  mode = va_arg (ap, int);
  va_end (ap);

  PATHNAME_CONVERT_OUT (path);
#ifdef INTERRUPTIBLE_OPEN
  {
    int rtnval;
    while ((rtnval = open (path, oflag, mode)) == -1
	   && (errno == EINTR));
    return rtnval;
  }
#else
    return open (path, oflag, mode);
#endif
}
#endif /* ENCAPSULATE_OPEN */


#ifdef ENCAPSULATE_CLOSE
int
sys_close (int fd)
{
#ifdef INTERRUPTIBLE_CLOSE
  int did_retry = 0;
  register int rtnval;

  while ((rtnval = close (fd)) == -1
	 && (errno == EINTR))
    did_retry = 1;

  /* If close is interrupted SunOS 4.1 may or may not have closed the
     file descriptor.  If it did the second close will fail with
     errno = EBADF.  That means we have succeeded.  */
  if (rtnval == -1 && did_retry && errno == EBADF)
    return 0;

  return rtnval;
#else
  return close (fd);
#endif
}
#endif /* ENCAPSULATE_CLOSE */

int
sys_read_1 (int fildes, void *buf, unsigned int nbyte, int allow_quit)
{
#ifdef VMS
  return vms_read (fildes, buf, nbyte);
#else
  int rtnval;

  /* No harm in looping regardless of the INTERRUPTIBLE_IO setting. */
  while ((rtnval = read (fildes, buf, nbyte)) == -1
	 && (errno == EINTR))
    {
      if (allow_quit)
	REALLY_QUIT;
    }
  return rtnval;
#endif
}

#ifdef ENCAPSULATE_READ
int
sys_read (int fildes, void *buf, unsigned int nbyte)
{
  return sys_read_1 (fildes, buf, nbyte, 0);
}
#endif /* ENCAPSULATE_READ */

int
sys_write_1 (int fildes, CONST void *buf, unsigned int nbyte, int allow_quit)
{
#ifdef VMS
  return vms_write (fildes, buf, nbyte);
#else
  int rtnval;
  int bytes_written = 0;
  CONST char *b = (CONST char *) buf;

  /* No harm in looping regardless of the INTERRUPTIBLE_IO setting. */
  while (nbyte > 0)
    {
      rtnval = write (fildes, b, nbyte);

      if (allow_quit)
	REALLY_QUIT;

      if (rtnval == -1)
	{
	  if (errno == EINTR)
	    continue;
	  else
            return (bytes_written ? bytes_written : -1);
	}
      b += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }
  return (bytes_written);
#endif
}

#ifdef ENCAPSULATE_WRITE
int
sys_write (int fildes, CONST void *buf, unsigned int nbyte)
{
  return sys_write_1 (fildes, buf, nbyte, 0);
}
#endif /* ENCAPSULATE_WRITE */


/**************** stdio calls ****************/

/* There is at least some evidence that the stdio calls are interruptible
   just like the normal system calls, at least on some systems.  In any
   case, it doesn't hurt to encapsulate them. */

/* #### Should also encapsulate fflush().
   #### Should conceivably encapsulate getchar() etc.  What a pain! */

#ifdef ENCAPSULATE_FOPEN
FILE *
sys_fopen (CONST char *path, CONST char *type)
{
  PATHNAME_CONVERT_OUT (path);
#ifdef INTERRUPTIBLE_OPEN
  {
    FILE *rtnval;
    while (!(rtnval = fopen (path, type)) && (errno == EINTR));
    return rtnval;
  }
#else
  return fopen (path, type);
#endif
}
#endif /* ENCAPSULATE_FOPEN */


#ifdef ENCAPSULATE_FCLOSE
int
sys_fclose (FILE *stream)
{
#ifdef INTERRUPTIBLE_CLOSE
  int rtnval;

  while ((rtnval = fclose (stream)) == EOF
	 && (errno == EINTR))
    ;
  return rtnval;
#else
  return fclose (stream);
#endif
}
#endif /* ENCAPSULATE_FCLOSE */


#ifdef ENCAPSULATE_FREAD
size_t
sys_fread (void *ptr, size_t size, size_t nitem, FILE *stream)
{
#ifdef INTERRUPTIBLE_IO
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
#else
  return fread (ptr, size, nitem, stream);
#endif
}
#endif /* ENCAPSULATE_FREAD */


#ifdef ENCAPSULATE_FWRITE
size_t
sys_fwrite (CONST void *ptr, size_t size, size_t nitem, FILE *stream)
{
#ifdef INTERRUPTIBLE_IO
  size_t rtnval;
  size_t items_written = 0;
  CONST char *b = (CONST char *) ptr;

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
#elif defined (VMS)
  return vms_fwrite (ptr, size, nitem, stream);
#else
  return fwrite (ptr, size, nitem, stream);
#endif
}
#endif /* ENCAPSULATE_FWRITE */


/********************* directory calls *******************/

#ifdef ENCAPSULATE_CHDIR
int
sys_chdir (CONST char *path)
{
  PATHNAME_CONVERT_OUT (path);
#ifdef MSDOS
  return dos_chdir (path);
#else
  return chdir (path);
#endif
}
#endif /* ENCAPSULATE_CHDIR */


#ifdef ENCAPSULATE_MKDIR
int
sys_mkdir (CONST char *path, int mode)
{
  PATHNAME_CONVERT_OUT (path);
  return mkdir (path, mode);
}
#endif /* ENCAPSULATE_MKDIR */


#ifdef ENCAPSULATE_OPENDIR
DIR *
sys_opendir (CONST char *filename)
{
  DIR *rtnval;
  PATHNAME_CONVERT_OUT (filename);

  while (!(rtnval = opendir (filename))
	 && (errno == EINTR))
    ;
  return rtnval;
}
#endif /* ENCAPSULATE_OPENDIR */


#ifdef ENCAPSULATE_READDIR
DIRENTRY *
sys_readdir (DIR *dirp)
{
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
    Extcount external_len;
    int ascii_filename_p = 1;
    CONST Extbyte * CONST external_name = (CONST Extbyte *) rtnval->d_name;

    /* Optimize for the common all-ASCII case, computing len en passant */
    for (external_len = 0; external_name[external_len] ; external_len++)
      {
        if (!BYTE_ASCII_P (external_name[external_len]))
          ascii_filename_p = 0;
      }
    if (ascii_filename_p)
      return rtnval;

    { /* Non-ASCII filename */
      static Bufbyte_dynarr *internal_DIRENTRY;
      CONST Bufbyte *internal_name;
      Bytecount internal_len;
      if (!internal_DIRENTRY)
        internal_DIRENTRY = Dynarr_new (Bufbyte);
      else
        Dynarr_reset (internal_DIRENTRY);

      Dynarr_add_many (internal_DIRENTRY, (Bufbyte *) rtnval,
                       offsetof (DIRENTRY, d_name));

      internal_name =
        convert_from_external_format (external_name, external_len,
                                      &internal_len, FORMAT_FILENAME);

      Dynarr_add_many (internal_DIRENTRY, internal_name, internal_len);
      Dynarr_add (internal_DIRENTRY, 0); /* zero-terminate */
      return (DIRENTRY *) Dynarr_atp (internal_DIRENTRY, 0);
    }
  }
#endif /* MULE */
}
#endif /* ENCAPSULATE_READDIR */


#ifdef ENCAPSULATE_CLOSEDIR
int
sys_closedir (DIR *dirp)
{
  int rtnval;

  while ((rtnval = closedir (dirp)) == -1
	 && (errno == EINTR))
    ;
  return rtnval;
}
#endif /* ENCAPSULATE_CLOSEDIR */


#ifdef ENCAPSULATE_RMDIR
int
sys_rmdir (CONST char *path)
{
  PATHNAME_CONVERT_OUT (path);
  return rmdir (path);
}
#endif /* ENCAPSULATE_RMDIR */


/***************** file-information calls ******************/

#ifdef ENCAPSULATE_ACCESS
int
sys_access (CONST char *path, int mode)
{
  PATHNAME_CONVERT_OUT (path);
#ifdef VMS
  return vms_access (path, mode);
#else
  return access (path, mode);
#endif
}
#endif /* ENCAPSULATE_ACCESS */


#ifdef HAVE_EACCESS
#ifdef ENCAPSULATE_EACCESS
int
sys_eaccess (CONST char *path, int mode)
{
  PATHNAME_CONVERT_OUT (path);
  return eaccess (path, mode);
}
#endif /* ENCAPSULATE_EACCESS */
#endif /* HAVE_EACCESS */


#ifdef ENCAPSULATE_LSTAT
int
sys_lstat (CONST char *path, struct stat *buf)
{
  PATHNAME_CONVERT_OUT (path);
  return lstat (path, buf);
}
#endif /* ENCAPSULATE_LSTAT */


#ifdef ENCAPSULATE_READLINK
int
sys_readlink (CONST char *path, char *buf, int bufsiz)
{
  PATHNAME_CONVERT_OUT (path);
  /* #### currently we don't do conversions on the incoming data */
  return readlink (path, buf, bufsiz);
}
#endif /* ENCAPSULATE_READLINK */


#ifdef ENCAPSULATE_STAT
int
sys_stat (CONST char *path, struct stat *buf)
{
  PATHNAME_CONVERT_OUT (path);
  return stat (path, buf);
}
#endif /* ENCAPSULATE_STAT */


/****************** file-manipulation calls *****************/

#ifdef ENCAPSULATE_CHMOD
int
sys_chmod (CONST char *path, int mode)
{
  PATHNAME_CONVERT_OUT (path);
  return chmod (path, mode);
}
#endif /* ENCAPSULATE_CHMOD */


#ifdef ENCAPSULATE_CREAT
int
sys_creat (CONST char *path, int mode)
{
  PATHNAME_CONVERT_OUT (path);
  return creat (path, mode);
}
#endif /* ENCAPSULATE_CREAT */


#ifdef ENCAPSULATE_LINK
int
sys_link (CONST char *existing, CONST char *new)
{
  PATHNAME_CONVERT_OUT (existing);
  PATHNAME_CONVERT_OUT (new);
  return link (existing, new);
}
#endif /* ENCAPSULATE_LINK */


#ifdef ENCAPSULATE_RENAME
int
sys_rename (CONST char *old, CONST char *new)
{
  PATHNAME_CONVERT_OUT (old);
  PATHNAME_CONVERT_OUT (new);
  return rename (old, new);
}
#endif /* ENCAPSULATE_RENAME */


#ifdef ENCAPSULATE_SYMLINK
int
sys_symlink (CONST char *name1, CONST char *name2)
{
  PATHNAME_CONVERT_OUT (name1);
  PATHNAME_CONVERT_OUT (name2);
  return symlink (name1, name2);
}
#endif /* ENCAPSULATE_SYMLINK */


#ifdef ENCAPSULATE_UNLINK
int
sys_unlink (CONST char *path)
{
  PATHNAME_CONVERT_OUT (path);
  return unlink (path);
}
#endif /* ENCAPSULATE_UNLINK */


#ifdef ENCAPSULATE_EXECVP
int
sys_execvp (CONST char *path, char * CONST * argv)
{
  int i, argc;
  CONST char ** new_argv;

  PATHNAME_CONVERT_OUT (path);
  for (argc = 0; argv[argc]; argc++)
    ;
  new_argv = alloca_array (CONST char *, argc + 1);
  for (i = 0; i < argc; i++)
    GET_C_CHARPTR_EXT_FILENAME_DATA_ALLOCA (argv[i], new_argv[i]);
  new_argv[argc] = NULL;
  return execvp (path, (char **) new_argv);
}
#endif /* ENCAPSULATE_EXECVP */


/************************************************************************/
/*                  Emulations of missing system calls                  */
/************************************************************************/

/***** (these are primarily required for USG, it seems) *****/

#ifndef HAVE_GETCWD
char *
getcwd (char *pathname, int size)
{
  return getwd (pathname);
}
#endif /* emulate getcwd */


#if 0 /* mrb */
/*
 *	Warning, this function may not duplicate BSD 4.2 action properly
 *	under error conditions.
 */

#ifndef HAVE_GETWD
char *
getwd (char *pathname)
{
  char *npath, *spath;
#if !__STDC__ && !defined(STDC_HEADERS)
  extern char *getcwd ();
#endif

  spath = npath = getcwd ((char *) 0, MAXPATHLEN);
  if (spath == 0)
    return spath;
  /* On Altos 3068, getcwd can return @hostname/dir, so discard
     up to first slash.  Should be harmless on other systems.  */
  while (*npath && *npath != '/')
    npath++;
  strcpy (pathname, npath);
  xfree (spath);                  /* getcwd uses malloc */
  return pathname;
}
#endif /* HAVE_GETWD */
#endif /* 0 - mrb */

/*
 *	Emulate rename using unlink/link.  Note that this is
 *	only partially correct.  Also, doesn't enforce restriction
 *	that files be of same type (regular->regular, dir->dir, etc).
 */

#ifndef HAVE_RENAME
int
rename (CONST char *from, CONST char *to)
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

  sys_close (newd);

#ifdef F_DUPFD
  fd = fcntl (oldd, F_DUPFD, newd);
  if (fd != newd)
    error ("can't dup2 (%i,%i) : %s", oldd, newd, strerror (errno));
#else
  fd = dup (old);
  if (fd == -1)
    return -1;
  if (fd == new)
    return new;
  ret = dup2 (old, new);
  sys_close (fd);
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
set_file_times (char *filename, EMACS_TIME atime, EMACS_TIME mtime)
{
#ifdef HAVE_UTIMES
  struct timeval tv[2];
  tv[0] = atime;
  tv[1] = mtime;
  return utimes (filename, tv);
#else /* not HAVE_UTIMES */
  struct utimbuf utb;
  utb.actime = EMACS_SECS (atime);
  utb.modtime = EMACS_SECS (mtime);
  return utime (filename, &utb);
#endif /* not HAVE_UTIMES */
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
get_process_times_1 (long *user_ticks, long *system_ticks)
{
#if defined (_SC_CLK_TCK) || defined (CLK_TCK) && !defined(WINDOWSNT)
  /* We have the POSIX times() function available. */
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
      /* MS-DOS or equally lame OS */
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

void seed_random (long arg);
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
}

/*
 * Build a full Emacs-sized word out of whatever we've got.
 * This suffices even for a 64-bit architecture with a 15-bit rand.
 */
long get_random (void);
long
get_random (void)
{
  long val = random ();
#if VALBITS > RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if VALBITS > 2*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if VALBITS > 3*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#if VALBITS > 4*RAND_BITS
  val = (val << RAND_BITS) ^ random ();
#endif /* need at least 5 */
#endif /* need at least 4 */
#endif /* need at least 3 */
#endif /* need at least 2 */
  return val & ((1L << VALBITS) - 1);
}

#ifdef WRONG_NAME_INSQUE

void
insque (caddr_t q, caddr_t p)
{
  _insque (q,p);
}

#endif


/************************************************************************/
/*               Strings corresponding to defined signals               */
/************************************************************************/

#if !defined (SYS_SIGLIST_DECLARED) && !defined (HAVE_SYS_SIGLIST)

#ifdef WINDOWSNT
char *sys_siglist[] =
  {
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
    "exceeded file size limit"
    };
#endif

#ifdef USG
#ifdef AIX
CONST char *sys_siglist[NSIG + 1] =
  {
    /* AIX has changed the signals a bit */
    DEFER_GETTEXT ("bogus signal"),			/* 0 */
    DEFER_GETTEXT ("hangup"),				/* 1  SIGHUP */
    DEFER_GETTEXT ("interrupt"),			/* 2  SIGINT */
    DEFER_GETTEXT ("quit"),				/* 3  SIGQUIT */
    DEFER_GETTEXT ("illegal instruction"),		/* 4  SIGILL */
    DEFER_GETTEXT ("trace trap"),			/* 5  SIGTRAP */
    DEFER_GETTEXT ("IOT instruction"),			/* 6  SIGIOT */
    DEFER_GETTEXT ("crash likely"),			/* 7  SIGDANGER */
    DEFER_GETTEXT ("floating point exception"),		/* 8  SIGFPE */
    DEFER_GETTEXT ("kill"),				/* 9  SIGKILL */
    DEFER_GETTEXT ("bus error"),			/* 10 SIGBUS */
    DEFER_GETTEXT ("segmentation violation"),		/* 11 SIGSEGV */
    DEFER_GETTEXT ("bad argument to system call"),	/* 12 SIGSYS */
    DEFER_GETTEXT ("write on a pipe with no one to read it"), /* 13 SIGPIPE */
    DEFER_GETTEXT ("alarm clock"),			/* 14 SIGALRM */
    DEFER_GETTEXT ("software termination signum"),	/* 15 SIGTERM */
    DEFER_GETTEXT ("user defined signal 1"),		/* 16 SIGUSR1 */
    DEFER_GETTEXT ("user defined signal 2"),		/* 17 SIGUSR2 */
    DEFER_GETTEXT ("death of a child"),			/* 18 SIGCLD */
    DEFER_GETTEXT ("power-fail restart"),		/* 19 SIGPWR */
    DEFER_GETTEXT ("bogus signal"),			/* 20 */
    DEFER_GETTEXT ("bogus signal"),			/* 21 */
    DEFER_GETTEXT ("bogus signal"),			/* 22 */
    DEFER_GETTEXT ("bogus signal"),			/* 23 */
    DEFER_GETTEXT ("bogus signal"),			/* 24 */
    DEFER_GETTEXT ("LAN I/O interrupt"),		/* 25 SIGAIO */
    DEFER_GETTEXT ("PTY I/O interrupt"),		/* 26 SIGPTY */
    DEFER_GETTEXT ("I/O intervention required"),	/* 27 SIGIOINT */
#ifdef AIXHFT
    DEFER_GETTEXT ("HFT grant"),			/* 28 SIGGRANT */
    DEFER_GETTEXT ("HFT retract"),			/* 29 SIGRETRACT */
    DEFER_GETTEXT ("HFT sound done"),			/* 30 SIGSOUND */
    DEFER_GETTEXT ("HFT input ready"),			/* 31 SIGMSG */
#endif
    0
  };
#else /* USG, not AIX */
CONST char *sys_siglist[NSIG + 1] =
  {
    DEFER_GETTEXT ("bogus signal"),			/* 0 */
    DEFER_GETTEXT ("hangup"),				/* 1  SIGHUP */
    DEFER_GETTEXT ("interrupt"),			/* 2  SIGINT */
    DEFER_GETTEXT ("quit"),				/* 3  SIGQUIT */
    DEFER_GETTEXT ("illegal instruction"),		/* 4  SIGILL */
    DEFER_GETTEXT ("trace trap"),			/* 5  SIGTRAP */
    DEFER_GETTEXT ("IOT instruction"),			/* 6  SIGIOT */
    DEFER_GETTEXT ("EMT instruction"),			/* 7  SIGEMT */
    DEFER_GETTEXT ("floating point exception"),		/* 8  SIGFPE */
    DEFER_GETTEXT ("kill"),				/* 9  SIGKILL */
    DEFER_GETTEXT ("bus error"),			/* 10 SIGBUS */
    DEFER_GETTEXT ("segmentation violation"),		/* 11 SIGSEGV */
    DEFER_GETTEXT ("bad argument to system call"),	/* 12 SIGSYS */
    DEFER_GETTEXT ("write on a pipe with no one to read it"), /* 13 SIGPIPE */
    DEFER_GETTEXT ("alarm clock"),			/* 14 SIGALRM */
    DEFER_GETTEXT ("software termination signum"),	/* 15 SIGTERM */
    DEFER_GETTEXT ("user defined signal 1"),		/* 16 SIGUSR1 */
    DEFER_GETTEXT ("user defined signal 2"),		/* 17 SIGUSR2 */
    DEFER_GETTEXT ("death of a child"),			/* 18 SIGCLD */
    DEFER_GETTEXT ("power-fail restart"),		/* 19 SIGPWR */
#ifdef sun
    DEFER_GETTEXT ("window size changed"),		/* 20 SIGWINCH */
    DEFER_GETTEXT ("urgent socket condition"),		/* 21 SIGURG */
    DEFER_GETTEXT ("pollable event occurred"),		/* 22 SIGPOLL */
    DEFER_GETTEXT ("stop (cannot be caught or ignored)"), /*  23 SIGSTOP */
    DEFER_GETTEXT ("user stop requested from tty"),	/* 24 SIGTSTP */
    DEFER_GETTEXT ("stopped process has been continued"), /* 25 SIGCONT */
    DEFER_GETTEXT ("background tty read attempted"),	/* 26 SIGTTIN */
    DEFER_GETTEXT ("background tty write attempted"),	/* 27 SIGTTOU */
    DEFER_GETTEXT ("virtual timer expired"),		/* 28 SIGVTALRM */
    DEFER_GETTEXT ("profiling timer expired"),		/* 29 SIGPROF */
    DEFER_GETTEXT ("exceeded cpu limit"),		/* 30 SIGXCPU */
    DEFER_GETTEXT ("exceeded file size limit"),		/* 31 SIGXFSZ */
    DEFER_GETTEXT ("process's lwps are blocked"),	/* 32 SIGWAITING */
    DEFER_GETTEXT ("special signal used by thread library"), /* 33 SIGLWP */
#ifdef SIGFREEZE
    DEFER_GETTEXT ("special signal used by CPR"),        /* 34 SIGFREEZE */
#endif
#ifdef SIGTHAW
    DEFER_GETTEXT ("special signal used by CPR"),        /* 35 SIGTHAW */
#endif
#endif /* sun */
    0
  };
#endif /* not AIX */
#endif /* USG */
#ifdef DGUX
CONST char *sys_siglist[NSIG + 1] =
  {
    DEFER_GETTEXT ("null signal"),			 /*  0 SIGNULL   */
    DEFER_GETTEXT ("hangup"),				 /*  1 SIGHUP    */
    DEFER_GETTEXT ("interrupt"),	       		 /*  2 SIGINT    */
    DEFER_GETTEXT ("quit"),				 /*  3 SIGQUIT   */
    DEFER_GETTEXT ("illegal instruction"),		 /*  4 SIGILL    */
    DEFER_GETTEXT ("trace trap"),			 /*  5 SIGTRAP   */
    DEFER_GETTEXT ("abort termination"),		 /*  6 SIGABRT   */
    DEFER_GETTEXT ("SIGEMT"),				 /*  7 SIGEMT    */
    DEFER_GETTEXT ("floating point exception"),		 /*  8 SIGFPE    */
    DEFER_GETTEXT ("kill"),				 /*  9 SIGKILL   */
    DEFER_GETTEXT ("bus error"),			 /* 10 SIGBUS    */
    DEFER_GETTEXT ("segmentation violation"),		 /* 11 SIGSEGV   */
    DEFER_GETTEXT ("bad argument to system call"),	 /* 12 SIGSYS    */
    DEFER_GETTEXT ("write on a pipe with no reader"),	 /* 13 SIGPIPE   */
    DEFER_GETTEXT ("alarm clock"),			 /* 14 SIGALRM   */
    DEFER_GETTEXT ("software termination signal"),	 /* 15 SIGTERM   */
    DEFER_GETTEXT ("user defined signal 1"),		 /* 16 SIGUSR1   */
    DEFER_GETTEXT ("user defined signal 2"),		 /* 17 SIGUSR2   */
    DEFER_GETTEXT ("child stopped or terminated"),	 /* 18 SIGCLD    */
    DEFER_GETTEXT ("power-fail restart"),		 /* 19 SIGPWR    */
    DEFER_GETTEXT ("window size changed"),		 /* 20 SIGWINCH  */
    DEFER_GETTEXT ("undefined"),			 /* 21           */
    DEFER_GETTEXT ("pollable event occurred"),		 /* 22 SIGPOLL   */
    DEFER_GETTEXT ("sendable stop signal not from tty"), /* 23 SIGSTOP   */
    DEFER_GETTEXT ("stop signal from tty"),		 /* 24 SIGSTP    */
    DEFER_GETTEXT ("continue a stopped process"),	 /* 25 SIGCONT   */
    DEFER_GETTEXT ("attempted background tty read"),	 /* 26 SIGTTIN   */
    DEFER_GETTEXT ("attempted background tty write"),	 /* 27 SIGTTOU   */
    DEFER_GETTEXT ("undefined"),			 /* 28           */
    DEFER_GETTEXT ("undefined"),			 /* 29           */
    DEFER_GETTEXT ("undefined"),			 /* 30           */
    DEFER_GETTEXT ("undefined"),			 /* 31           */
    DEFER_GETTEXT ("undefined"),			 /* 32           */
    DEFER_GETTEXT ("socket (TCP/IP) urgent data arrival"), /* 33 SIGURG    */
    DEFER_GETTEXT ("I/O is possible"),			 /* 34 SIGIO     */
    DEFER_GETTEXT ("exceeded cpu time limit"),		 /* 35 SIGXCPU   */
    DEFER_GETTEXT ("exceeded file size limit"),		 /* 36 SIGXFSZ   */
    DEFER_GETTEXT ("virtual time alarm"),		 /* 37 SIGVTALRM */
    DEFER_GETTEXT ("profiling time alarm"),		 /* 38 SIGPROF   */
    DEFER_GETTEXT ("undefined"),			 /* 39           */
    DEFER_GETTEXT ("file record locks revoked"),	 /* 40 SIGLOST   */
    DEFER_GETTEXT ("undefined"),			 /* 41           */
    DEFER_GETTEXT ("undefined"),			 /* 42           */
    DEFER_GETTEXT ("undefined"),			 /* 43           */
    DEFER_GETTEXT ("undefined"),			 /* 44           */
    DEFER_GETTEXT ("undefined"),			 /* 45           */
    DEFER_GETTEXT ("undefined"),			 /* 46           */
    DEFER_GETTEXT ("undefined"),			 /* 47           */
    DEFER_GETTEXT ("undefined"),			 /* 48           */
    DEFER_GETTEXT ("undefined"),			 /* 49           */
    DEFER_GETTEXT ("undefined"),			 /* 50           */
    DEFER_GETTEXT ("undefined"),			 /* 51           */
    DEFER_GETTEXT ("undefined"),			 /* 52           */
    DEFER_GETTEXT ("undefined"),			 /* 53           */
    DEFER_GETTEXT ("undefined"),			 /* 54           */
    DEFER_GETTEXT ("undefined"),			 /* 55           */
    DEFER_GETTEXT ("undefined"),			 /* 56           */
    DEFER_GETTEXT ("undefined"),			 /* 57           */
    DEFER_GETTEXT ("undefined"),			 /* 58           */
    DEFER_GETTEXT ("undefined"),			 /* 59           */
    DEFER_GETTEXT ("undefined"),			 /* 60           */
    DEFER_GETTEXT ("undefined"),			 /* 61           */
    DEFER_GETTEXT ("undefined"),			 /* 62           */
    DEFER_GETTEXT ("undefined"),			 /* 63           */
    DEFER_GETTEXT ("notification message in mess. queue"), /* 64 SIGDGNOTIFY */
    0
  };
#endif /* DGUX */

#endif /* ! SYS_SIGLIST_DECLARED && ! HAVE_SYS_SIGLIST */


/************************************************************************/
/*         Directory routines for systems that don't have them          */
/************************************************************************/

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>

#if defined(BROKEN_CLOSEDIR) || !defined(HAVE_CLOSEDIR)
int
closedir (DIR *dirp)  /* stream from opendir */
{
  int rtnval;

  rtnval = sys_close (dirp->dd_fd);

  /* Some systems (like Solaris) allocate the buffer and the DIR all
     in one block.  Why in the world are we freeing this ourselves
     anyway?  */
#if ! (defined (sun) && defined (USG5_4))
  xfree ((char *) dirp->dd_buf); /* directory block defined in <dirent.h> */
#endif
  xfree ((char *) dirp);
  return (rtnval);
}
#endif /* BROKEN_CLOSEDIR or not HAVE_CLOSEDIR */
#endif /* SYSV_SYSTEM_DIR */

#ifdef NONSYSTEM_DIR_LIBRARY

DIR *
opendir (CONST char *filename)	/* name of directory */
{
  DIR *dirp;		/* -> malloc'ed storage */
  int fd;		/* file descriptor for read */
  struct stat sbuf;		/* result of fstat */

  fd = sys_open (filename, 0);
  if (fd < 0)
    return 0;

  if (fstat (fd, &sbuf) < 0
      || (sbuf.st_mode & S_IFMT) != S_IFDIR
      || (dirp = (DIR *) malloc (sizeof (DIR))) == 0)
    {
      sys_close (fd);
      return 0;		/* bad luck today */
    }

  dirp->dd_fd = fd;
  dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

  return dirp;
}

void
closedir (DIR *dirp)		/* stream from opendir */
{
  sys_close (dirp->dd_fd);
  xfree (dirp);
}


#ifndef VMS
#define DIRSIZ	14
struct olddir
  {
    ino_t od_ino; 		/* inode */
    char od_name[DIRSIZ];	/* filename */
  };
#endif /* not VMS */

static struct direct dir_static; /* simulated directory contents */

/* ARGUSED */
struct direct *
readdir (DIR *dirp)	/* stream from opendir */
{
#ifndef VMS
  struct olddir *dp;	/* -> directory data */
#else /* VMS */
  struct dir$_name *dp; /* -> directory data */
  struct dir$_version *dv; /* -> version data */
#endif /* VMS */

  for (; ;)
    {
      if (dirp->dd_loc >= dirp->dd_size)
	dirp->dd_loc = dirp->dd_size = 0;

      if (dirp->dd_size == 0 	/* refill buffer */
	  && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
	return 0;

#ifndef VMS
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
#else /* VMS */
      dp = (struct dir$_name *) dirp->dd_buf;
      if (dirp->dd_loc == 0)
	dirp->dd_loc = (dp->dir$b_namecount&1) ? dp->dir$b_namecount + 1
	  : dp->dir$b_namecount;
      dv = (struct dir$_version *)&dp->dir$t_name[dirp->dd_loc];
      dir_static.d_ino = dv->dir$w_fid_num;
      dir_static.d_namlen = dp->dir$b_namecount;
      dir_static.d_reclen = sizeof (struct direct)
	- MAXNAMLEN + 3
	  + dir_static.d_namlen - dir_static.d_namlen % 4;
      strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
      dir_static.d_name[dir_static.d_namlen] = '\0';
      dirp->dd_loc = dirp->dd_size; /* only one record at a time */
      return &dir_static;
#endif /* VMS */
    }
}

#ifdef VMS
/* readdirver is just like readdir except it returns all versions of a file
   as separate entries.  */

/* ARGUSED */
struct direct *
readdirver (DIR *dirp)	/* stream from opendir */
{
  struct dir$_name *dp; /* -> directory data */
  struct dir$_version *dv; /* -> version data */

  if (dirp->dd_loc >= dirp->dd_size - sizeof (struct dir$_name))
    dirp->dd_loc = dirp->dd_size = 0;

  if (dirp->dd_size == 0 	/* refill buffer */
      && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
    return 0;

  dp = (struct dir$_name *) dirp->dd_buf;
  if (dirp->dd_loc == 0)
    dirp->dd_loc = (dp->dir$b_namecount & 1) ? dp->dir$b_namecount + 1
		   : dp->dir$b_namecount;
  dv = (struct dir$_version *) &dp->dir$t_name[dirp->dd_loc];
  strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
  sprintf (&dir_static.d_name[dp->dir$b_namecount], ";%d", dv->dir$w_version);
  dir_static.d_namlen = strlen (dir_static.d_name);
  dir_static.d_ino = dv->dir$w_fid_num;
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3
			+ dir_static.d_namlen - dir_static.d_namlen % 4;
  dirp->dd_loc = ((char *) (++dv) - dp->dir$t_name);
  return &dir_static;
}

#endif /* VMS */

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
mkdir (CONST char *dpath, int dmode)
#endif
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) == 0)
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
      return (-1);		/* Errno is set already */

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
      fd = sys_open ("/dev/null", 2);
      if (fd >= 0)
        {
	  dup2 (fd, 0);
	  dup2 (fd, 1);
	  dup2 (fd, 2);
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
rmdir (CONST char *dpath)
{
  int cpid, status, fd;
  struct stat statbuf;

  if (stat (dpath, &statbuf) != 0)
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
      fd = sys_open("/dev/null", 2);
      if (fd >= 0)
        {
	  dup2 (fd, 0);
	  dup2 (fd, 1);
	  dup2 (fd, 2);
        }
      execl ("/bin/rmdir", "rmdir", dpath, (char *) 0);
      _exit (-1);		/* Can't exec /bin/mkdir */

    default:			/* Parent process */
      wait_for_termination (cpid);
    }

  if (synch_process_death != 0 || synch_process_retcode != 0)
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


/************************************************************************/
/*                     VMS emulation of system calls                    */
/************************************************************************/

#ifdef VMS
#include "vms-pwd.h"
#include <acldef.h>
#include <chpdef.h>
#include <jpidef.h>

/* Return as a string the VMS error string pertaining to STATUS.
   Reuses the same static buffer each time it is called.  */

char *
vmserrstr (int status)	/* VMS status code */
{
  int bufadr[2];
  short len;
  static char buf[257];

  bufadr[0] = sizeof buf - 1;
  bufadr[1] = (int) buf;
  if (! (SYS$GETMSG (status, &len, bufadr, 0x1, 0) & 1))
    return "untranslatable VMS error status";
  buf[len] = '\0';
  return buf;
}

#ifdef access
#undef access

/* The following is necessary because 'access' emulation by VMS C (2.0) does
 * not work correctly.  (It also doesn't work well in version 2.3.)
 */

#ifdef VMS4_4

#define DESCRIPTOR(name,string) struct dsc$descriptor_s name = \
	{ strlen (string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string }

typedef union {
    struct {
	unsigned short s_buflen;
	unsigned short s_code;
	char *s_bufadr;
	unsigned short *s_retlenadr;
    } s;
    int end;
} item;
#define buflen s.s_buflen
#define code s.s_code
#define bufadr s.s_bufadr
#define retlenadr s.s_retlenadr

#define R_OK 4	/* test for read permission */
#define W_OK 2	/* test for write permission */
#define X_OK 1	/* test for execute (search) permission */
#define F_OK 0	/* test for presence of file */

int
vms_access (CONST char *path, int mode)
{
  static char *user = NULL;
  char dir_fn[512];

  /* translate possible directory spec into .DIR file name, so brain-dead
   * access can treat the directory like a file.  */
  if (directory_file_name (path, dir_fn))
    path = dir_fn;

  if (mode == F_OK)
    return access (path, mode);
  if (user == NULL && (user = (char *) getenv ("USER")) == NULL)
    return -1;
  {
    int stat;
    int flags;
    int acces;
    unsigned short int dummy;
    item itemlst[3];
    static int constant = ACL$C_FILE;
    DESCRIPTOR (path_desc, path);
    DESCRIPTOR (user_desc, user);

    flags = 0;
    acces = 0;
    if ((mode & X_OK) && ((stat = access (path, mode)) < 0 || mode == X_OK))
      return stat;
    if (mode & R_OK)
      acces |= CHP$M_READ;
    if (mode & W_OK)
      acces |= CHP$M_WRITE;
    itemlst[0].buflen = sizeof (int);
    itemlst[0].code = CHP$_FLAGS;
    itemlst[0].bufadr = (char *) &flags;
    itemlst[0].retlenadr = &dummy;
    itemlst[1].buflen = sizeof (int);
    itemlst[1].code = CHP$_ACCESS;
    itemlst[1].bufadr = (char *) &acces;
    itemlst[1].retlenadr = &dummy;
    itemlst[2].end = CHP$_END;
    stat = SYS$CHECK_ACCESS (&constant, &path_desc, &user_desc, itemlst);
    return stat == SS$_NORMAL ? 0 : -1;
  }
}

#else /* not VMS4_4 */

#include <prvdef.h>
#define	ACE$M_WRITE	2
#define	ACE$C_KEYID	1

static unsigned short vms_memid, vms_grpid;
static unsigned int vms_uic;

/* Called from init_sys_modes, so it happens not very often
   but at least each time Emacs is loaded.  */
sys_access_reinit (void)
{
  vms_uic = 0;
}

int
vms_access (CONST char *filename, int type)
{
  struct FAB fab;
  struct XABPRO xab;
  int status, size, i, typecode, acl_controlled;
  unsigned int *aclptr, *aclend, aclbuf[60];
  union prvdef prvmask;

  /* Get UIC and GRP values for protection checking.  */
  if (vms_uic == 0)
    {
      status = LIB$GETJPI (&JPI$_UIC, 0, 0, &vms_uic, 0, 0);
      if (! (status & 1))
	return -1;
      vms_memid = vms_uic & 0xFFFF;
      vms_grpid = vms_uic >> 16;
    }

  if (type != 2)		/* not checking write access */
    return access (filename, type);

  /* Check write protection. */

#define	CHECKPRIV(bit)    (prvmask.bit)
#define	WRITEABLE(field)  (! ((xab.xab$w_pro >> field) & XAB$M_NOWRITE))

  /* Find privilege bits */
  status = SYS$SETPRV (0, 0, 0, prvmask);
  if (! (status & 1))
    error ("Unable to find privileges: %s", vmserrstr (status));
  if (CHECKPRIV (PRV$V_BYPASS))
    return 0;			/* BYPASS enabled */
  fab = cc$rms_fab;
  fab.fab$b_fac = FAB$M_GET;
  fab.fab$l_fna = filename;
  fab.fab$b_fns = strlen (filename);
  fab.fab$l_xab = &xab;
  xab = cc$rms_xabpro;
  xab.xab$l_aclbuf = aclbuf;
  xab.xab$w_aclsiz = sizeof (aclbuf);
  status = SYS$OPEN (&fab, 0, 0);
  if (! (status & 1))
    return -1;
  SYS$CLOSE (&fab, 0, 0);
  /* Check system access */
  if (CHECKPRIV (PRV$V_SYSPRV) && WRITEABLE (XAB$V_SYS))
    return 0;
  /* Check ACL entries, if any */
  acl_controlled = 0;
  if (xab.xab$w_acllen > 0)
    {
      aclptr = aclbuf;
      aclend = &aclbuf[xab.xab$w_acllen / 4];
      while (*aclptr && aclptr < aclend)
	{
	  size = (*aclptr & 0xff) / 4;
	  typecode = (*aclptr >> 8) & 0xff;
	  if (typecode == ACE$C_KEYID)
	    for (i = size - 1; i > 1; i--)
	      if (aclptr[i] == vms_uic)
		{
		  acl_controlled = 1;
		  if (aclptr[1] & ACE$M_WRITE)
		    return 0;	/* Write access through ACL */
		}
	  aclptr = &aclptr[size];
	}
      if (acl_controlled)	/* ACL specified, prohibits write access */
	return -1;
    }
  /* No ACL entries specified, check normal protection */
  if (WRITEABLE (XAB$V_WLD))	/* World writeable */
    return 0;
  if (WRITEABLE (XAB$V_GRP) &&
      (unsigned short) (xab.xab$l_uic >> 16) == vms_grpid)
    return 0;			/* Group writeable */
  if (WRITEABLE (XAB$V_OWN) &&
      (xab.xab$l_uic & 0xFFFF) == vms_memid)
    return 0;			/* Owner writeable */

  return -1;	/* Not writeable */
}
#endif /* not VMS4_4 */
#endif /* access */

static char vtbuf[NAM$C_MAXRSS+1];

/* translate a vms file spec to a unix path */
char *
sys_translate_vms (char *vfile)
{
  char * p;
  char * targ;

  if (!vfile)
    return 0;

  targ = vtbuf;

  /* leading device or logical name is a root directory */
  if (p = strchr (vfile, ':'))
    {
      *targ++ = '/';
      while (vfile < p)
	*targ++ = *vfile++;
      vfile++;
      *targ++ = '/';
    }
  p = vfile;
  if (*p == '[' || *p == '<')
    {
      while (*++vfile != *p + 2)
	switch (*vfile)
	  {
	  case '.':
	    if (vfile[-1] == *p)
	      *targ++ = '.';
	    *targ++ = '/';
	    break;

	  case '-':
	    *targ++ = '.';
	    *targ++ = '.';
	    break;

	  default:
	    *targ++ = *vfile;
	    break;
	  }
      vfile++;
      *targ++ = '/';
    }
  while (*vfile)
    *targ++ = *vfile++;

  return vtbuf;
}

static char utbuf[NAM$C_MAXRSS+1];

/* translate a unix path to a VMS file spec */
char *
sys_translate_unix (char *ufile)
{
  int slash_seen = 0;
  char *p;
  char * targ;

  if (!ufile)
    return 0;

  targ = utbuf;

  if (*ufile == '/')
    {
      ufile++;
    }

  while (*ufile)
    {
      switch (*ufile)
	{
	case '/':
	  if (slash_seen)
	    if (strchr (&ufile[1], '/'))
	      *targ++ = '.';
	    else
	      *targ++ = ']';
	  else
	    {
	      *targ++ = ':';
	      if (strchr (&ufile[1], '/'))
		*targ++ = '[';
	      slash_seen = 1;
	    }
	  break;

	case '.':
	  if (strncmp (ufile, "./", 2) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      ufile++;		/* skip the dot */
	      if (strchr (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else if (strncmp (ufile, "../", 3) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      *targ++ = '-';
	      ufile += 2;	/* skip the dots */
	      if (strchr (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else
	    *targ++ = *ufile;
	  break;

	default:
	  *targ++ = *ufile;
	  break;
	}
      ufile++;
    }
  *targ = '\0';

  return utbuf;
}

char *
getwd (char *pathname)
{
  char *ptr;
  strcpy (pathname, egetenv ("PATH"));

  ptr = pathname;
  while (*ptr)
    {
      /* #### This is evil.  Smashes (shared) result of egetenv */
      *ptr = toupper (* (unsigned char *) ptr);
      ptr++;
    }
  return pathname;
}

int
getppid (void)
{
  long item_code = JPI$_OWNER;
  unsigned long parent_id;
  int status;

  if (((status = LIB$GETJPI (&item_code, 0, 0, &parent_id)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  return parent_id;
}

#undef getuid
unsigned int
sys_getuid (void)
{
  return (getgid () << 16) | getuid ();
}

int
vms_read (int fildes, CONST void *buf, unsigned int nbyte)
{
  return read (fildes, buf, (nbyte < MAXIOSIZE ? nbyte : MAXIOSIZE));
}

#if 0
int
vms_write (int fildes, CONST void *buf, unsigned int nbyte)
{
  int nwrote, rtnval = 0;

  while (nbyte > MAXIOSIZE && (nwrote = write (fildes, buf, MAXIOSIZE)) > 0)
    {
      nbyte -= nwrote;
      buf += nwrote;
      rtnval += nwrote;
    }
  if (nwrote < 0)
    return rtnval ? rtnval : -1;
  if ((nwrote = write (fildes, buf, nbyte)) < 0)
    return rtnval ? rtnval : -1;
  return (rtnval + nwrote);
}
#endif /* 0 */

/*
 *	VAX/VMS VAX C RTL really loses. It insists that records
 *      end with a newline (carriage return) character, and if they
 *	don't it adds one (nice of it isn't it!)
 *
 *	Thus we do this stupidity below.
 */

int
vms_write (int fildes, CONST void *buf, unsigned int nbytes)
{
  char *p;
  char *e;
  int sum = 0;
  struct stat st;

  fstat (fildes, &st);
  p = buf;
  while (nbytes > 0)
    {
      int len, retval;

      /* Handle fixed-length files with carriage control.  */
      if (st.st_fab_rfm == FAB$C_FIX
	  && ((st.st_fab_rat & (FAB$M_FTN | FAB$M_CR)) != 0))
	{
	  len = st.st_fab_mrs;
	  retval = write (fildes, p, min (len, nbytes));
	  if (retval != len)
	    return -1;
	  retval++;	/* This skips the implied carriage control */
	}
      else
	{
	  e =  p + min (MAXIOSIZE, nbytes) - 1;
	  while (*e != '\n' && e > p) e--;
	  if (p == e)		/* Ok.. so here we add a newline... sigh. */
	    e = p + min (MAXIOSIZE, nbytes) - 1;
	  len = e + 1 - p;
	  retval = write (fildes, p, len);
	  if (retval != len)
	    return -1;
	}
      p += retval;
      sum += retval;
      nbytes -= retval;
    }
  return sum;
}

/* Create file NEW copying its attributes from file OLD.  If
   OLD is 0 or does not exist, create based on the value of
   vms_stmlf_recfm. */

/* Protection value the file should ultimately have.
   Set by create_copy_attrs, and use by rename_sansversions.  */
static unsigned short int vms_fab_final_pro;

int
creat_copy_attrs (char *old, char *new)
{
  struct FAB fab = cc$rms_fab;
  struct XABPRO xabpro;
  char aclbuf[256];	/* Choice of size is arbitrary.  See below. */
  extern int vms_stmlf_recfm;

  if (old)
    {
      fab.fab$b_fac = FAB$M_GET;
      fab.fab$l_fna = old;
      fab.fab$b_fns = strlen (old);
      fab.fab$l_xab = (char *) &xabpro;
      xabpro = cc$rms_xabpro;
      xabpro.xab$l_aclbuf = aclbuf;
      xabpro.xab$w_aclsiz = sizeof aclbuf;
      /* Call $OPEN to fill in the fab & xabpro fields. */
      if (SYS$OPEN (&fab, 0, 0) & 1)
	{
	  SYS$CLOSE (&fab, 0, 0);
	  fab.fab$l_alq = 0;	/* zero the allocation quantity */
	  if (xabpro.xab$w_acllen > 0)
	    {
	      if (xabpro.xab$w_acllen > sizeof aclbuf)
		/* If the acl buffer was too short, redo open with longer one.
		   Wouldn't need to do this if there were some system imposed
		   limit on the size of an ACL, but I can't find any such. */
		{
		  xabpro.xab$l_aclbuf = (char *) alloca (xabpro.xab$w_acllen);
		  xabpro.xab$w_aclsiz = xabpro.xab$w_acllen;
		  if (SYS$OPEN (&fab, 0, 0) & 1)
		    SYS$CLOSE (&fab, 0, 0);
		  else
		    old = 0;
		}
	    }
	  else
	    xabpro.xab$l_aclbuf = 0;
	}
      else
	old = 0;
    }
  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);
  if (!old)
    {
      fab.fab$l_xab = 0;
      fab.fab$b_rfm = vms_stmlf_recfm ? FAB$C_STMLF : FAB$C_VAR;
      fab.fab$b_rat = FAB$M_CR;
    }

  /* Set the file protections such that we will be able to manipulate
     this file.  Once we are done writing and renaming it, we will set
     the protections back.  */
  if (old)
    vms_fab_final_pro = xabpro.xab$w_pro;
  else
    SYS$SETDFPROT (0, &vms_fab_final_pro);
  xabpro.xab$w_pro &= 0xff0f; /* set O:rewd for now. This is set back later. */

  /* Create the new file with either default attrs or attrs copied
     from old file. */
  if (!(SYS$CREATE (&fab, 0, 0) & 1))
    return -1;
  SYS$CLOSE (&fab, 0, 0);
  /* As this is a "replacement" for creat, return a file descriptor
     opened for writing. */
  return open (new, O_WRONLY);
}

int
vms_creat (CONST char *path, int mode, ...)
{
  int rfd;			/* related file descriptor */
  int fd;			/* Our new file descriptor */
  int count;
  struct stat st_buf;
  char rfm[12];
  char rat[15];
  char mrs[13];
  char fsz[13];
  extern int vms_stmlf_recfm;

  /* #### there was some weird machine-dependent code to determine how many
     arguments were passed to this function.  This certainly won't work
     under ANSI C. */
  if (count > 2)
    rfd = fix this;
  if (count > 2)
    {
      /* Use information from the related file descriptor to set record
	 format of the newly created file. */
      fstat (rfd, &st_buf);
      switch (st_buf.st_fab_rfm)
	{
	case FAB$C_FIX:
	  strcpy (rfm, "rfm = fix");
	  sprintf (mrs, "mrs = %d", st_buf.st_fab_mrs);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, mrs);

	case FAB$C_VFC:
	  strcpy (rfm, "rfm = vfc");
	  sprintf (fsz, "fsz = %d", st_buf.st_fab_fsz);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, fsz);

	case FAB$C_STM:
	  strcpy (rfm, "rfm = stm");
	  break;

	case FAB$C_STMCR:
	  strcpy (rfm, "rfm = stmcr");
	  break;

	case FAB$C_STMLF:
	  strcpy (rfm, "rfm = stmlf");
	  break;

	case FAB$C_UDF:
	  strcpy (rfm, "rfm = udf");
	  break;

	case FAB$C_VAR:
	  strcpy (rfm, "rfm = var");
	  break;
	}
      strcpy (rat, "rat = ");
      if (st_buf.st_fab_rat & FAB$M_CR)
	strcat (rat, "cr");
      else if (st_buf.st_fab_rat & FAB$M_FTN)
	strcat (rat, "ftn");
      else if (st_buf.st_fab_rat & FAB$M_PRN)
	strcat (rat, "prn");
      if (st_buf.st_fab_rat & FAB$M_BLK)
	if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	  strcat (rat, ", blk");
	else
	  strcat (rat, "blk");
    }
  else
    {
      strcpy (rfm, vms_stmlf_recfm ? "rfm = stmlf" : "rfm=var");
      strcpy (rat, "rat=cr");
    }
  /* Until the VAX C RTL fixes the many bugs with modes, always use
     mode 0 to get the user's default protection. */
  fd = creat (name, 0, rfm, rat);
  if (fd < 0 && errno == EEXIST)
    {
      if (unlink (name) < 0)
	report_file_error ("delete", build_string (name));
      fd = creat (name, 0, rfm, rat);
    }
  return fd;
}

/* fwrite to stdout is S L O W.  Speed it up by using fputc...*/
int
vms_fwrite (CONST void *ptr, int size, int num, FILE *fp)
{
  int tot = num * size;

  while (tot--)
    fputc (* (CONST char *) ptr++, fp);
  return (num);
}

/*
 * The VMS C library routine creat actually creates a new version of an
 * existing file rather than truncating the old version.  There are times
 * when this is not the desired behavior, for instance, when writing an
 * auto save file (you only want one version), or when you don't have
 * write permission in the directory containing the file (but the file
 * itself is writable).  Hence this routine, which is equivalent to
 * "close (creat (fn, 0));" on Unix if fn already exists.
 */
int
vms_truncate (char *fn)
{
  struct FAB xfab = cc$rms_fab;
  struct RAB xrab = cc$rms_rab;
  int status;

  xfab.fab$l_fop = FAB$M_TEF;	/* free allocated but unused blocks on close */
  xfab.fab$b_fac = FAB$M_TRN | FAB$M_GET; /* allow truncate and get access */
  xfab.fab$b_shr = FAB$M_NIL;	/* allow no sharing - file must be locked */
  xfab.fab$l_fna = fn;
  xfab.fab$b_fns = strlen (fn);
  xfab.fab$l_dna = ";0";	/* default to latest version of the file */
  xfab.fab$b_dns = 2;
  xrab.rab$l_fab = &xfab;

  /* This gibberish opens the file, positions to the first record, and
     deletes all records from there until the end of file. */
  if ((SYS$OPEN (&xfab) & 01) == 01)
    {
      if ((SYS$CONNECT (&xrab) & 01) == 01 &&
	  (SYS$FIND (&xrab) & 01) == 01 &&
	  (SYS$TRUNCATE (&xrab) & 01) == 01)
	status = 0;
      else
	status = -1;
    }
  else
    status = -1;
  SYS$CLOSE (&xfab);
  return status;
}

/* Define this symbol to actually read SYSUAF.DAT.  This requires either
   SYSPRV or a readable SYSUAF.DAT. */

#ifdef READ_SYSUAF
/*
 * getuaf.c
 *
 * Routine to read the VMS User Authorization File and return
 * a specific user's record.
 */

static struct UAF vms_retuaf;

static struct UAF *
get_uaf_name (char *uname)
{
  status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;

  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = SYS$OPEN (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uname */
  uaf_rab.rab$l_kbf = uname;
  uaf_rab.rab$b_ksz = strlen (uname);
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&vms_retuaf;
  uaf_rab.rab$w_usz = sizeof vms_retuaf;
  status = SYS$GET (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = SYS$DISCONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CLOSE (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &vms_retuaf;
}

static struct UAF *
get_uaf_uic (unsigned long uic)
{
  status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;

  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = SYS$OPEN (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uic */
  uaf_rab.rab$b_krf = 1;	/* 1st alternate key */
  uaf_rab.rab$l_kbf = (char *) &uic;
  uaf_rab.rab$b_ksz = sizeof uic;
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&vms_retuaf;
  uaf_rab.rab$w_usz = sizeof vms_retuaf;
  status = SYS$GET (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = SYS$DISCONNECT (&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = SYS$CLOSE (&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &vms_retuaf;
}

static struct passwd vms_retpw;

static struct passwd *
cnv_uaf_pw (struct UAF *up)
{
  char * ptr;

  /* copy these out first because if the username is 32 chars, the next
     section will overwrite the first byte of the UIC */
  vms_retpw.pw_uid = up->uaf$w_mem;
  vms_retpw.pw_gid = up->uaf$w_grp;

  /* I suppose this is not the best sytle, to possibly overwrite one
     byte beyond the end of the field, but what the heck... */
  ptr = &up->uaf$t_username[UAF$S_USERNAME];
  while (ptr[-1] == ' ')
    ptr--;
  *ptr = '\0';
  strcpy (vms_retpw.pw_name, up->uaf$t_username);

  /* the rest of these are counted ascii strings */
  strncpy (vms_retpw.pw_gecos, &up->uaf$t_owner[1], up->uaf$t_owner[0]);
  vms_retpw.pw_gecos[up->uaf$t_owner[0]] = '\0';
  strncpy (vms_retpw.pw_dir, &up->uaf$t_defdev[1], up->uaf$t_defdev[0]);
  vms_retpw.pw_dir[up->uaf$t_defdev[0]] = '\0';
  strncat (vms_retpw.pw_dir, &up->uaf$t_defdir[1], up->uaf$t_defdir[0]);
  vms_retpw.pw_dir[up->uaf$t_defdev[0] + up->uaf$t_defdir[0]] = '\0';
  strncpy (vms_retpw.pw_shell, &up->uaf$t_defcli[1], up->uaf$t_defcli[0]);
  vms_retpw.pw_shell[up->uaf$t_defcli[0]] = '\0';

  return &vms_retpw;
}
#else /* not READ_SYSUAF */
static struct passwd vms_retpw;
#endif /* not READ_SYSUAF */

struct passwd *
getpwnam (char *name)
{
#ifdef READ_SYSUAF
  struct UAF *up;
#else
  char * user;
  char * dir;
  unsigned char * full;
#endif /* READ_SYSUAF */
  char *ptr = name;

  while (*ptr)
    {
      *ptr = toupper (* (unsigned char *) ptr);
      ptr++;
    }
#ifdef READ_SYSUAF
  if (!(up = get_uaf_name (name)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (strcmp (name, getenv ("USER")) == 0)
    {
      vms_retpw.pw_uid = getuid ();
      vms_retpw.pw_gid = getgid ();
      strcpy (vms_retpw.pw_name, name);
      if (full = egetenv ("FULLNAME"))
	strcpy (vms_retpw.pw_gecos, full);
      else
	*vms_retpw.pw_gecos = '\0';
      strcpy (vms_retpw.pw_dir, egetenv ("HOME"));
      *vms_retpw.pw_shell = '\0';
      return &vms_retpw;
    }
  else
    return 0;
#endif /* not READ_SYSUAF */
}

struct passwd *
getpwuid (unsigned long uid)
{
#ifdef READ_SYSUAF
  struct UAF * up;

  if (!(up = get_uaf_uic (uid)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (uid == sys_getuid ())
    return getpwnam (egetenv ("USER"));
  else
    return 0;
#endif /* not READ_SYSUAF */
}

/* return total address space available to the current process.  This is
   the sum of the current p0 size, p1 size and free page table entries
   available. */
int
vlimit (void)
{
  int item_code;
  unsigned long free_pages;
  unsigned long frep0va;
  unsigned long frep1va;
  status;

  item_code = JPI$_FREPTECNT;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &free_pages)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  free_pages *= 512;

  item_code = JPI$_FREP0VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep0va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  item_code = JPI$_FREP1VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep1va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return free_pages + frep0va + (0x7fffffff - frep1va);
}

int
define_logical_name (char *varname, char *string)
{
  struct dsc$descriptor_s strdsc =
    {strlen (string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string};
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$SET_LOGICAL (&envdsc, &strdsc, &lnmdsc, 0, 0);
}

int
delete_logical_name (char *varname)
{
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$DELETE_LOGICAL (&envdsc, &lnmdsc);
}

execvp (void)
{
  error ("execvp system call not implemented");
}

int
rename (char *from, char *to)
{
  int status;
  struct FAB from_fab = cc$rms_fab, to_fab = cc$rms_fab;
  struct NAM from_nam = cc$rms_nam, to_nam = cc$rms_nam;
  char from_esn[NAM$C_MAXRSS];
  char to_esn[NAM$C_MAXRSS];

  from_fab.fab$l_fna = from;
  from_fab.fab$b_fns = strlen (from);
  from_fab.fab$l_nam = &from_nam;
  from_fab.fab$l_fop = FAB$M_NAM;

  from_nam.nam$l_esa = from_esn;
  from_nam.nam$b_ess = sizeof from_esn;

  to_fab.fab$l_fna = to;
  to_fab.fab$b_fns = strlen (to);
  to_fab.fab$l_nam = &to_nam;
  to_fab.fab$l_fop = FAB$M_NAM;

  to_nam.nam$l_esa = to_esn;
  to_nam.nam$b_ess = sizeof to_esn;

  status = SYS$RENAME (&from_fab, 0, 0, &to_fab);

  if (status & 1)
    return 0;
  else
    {
      if (status == RMS$_DEV)
	errno = EXDEV;
      else
	errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
}

/* This function renames a file like `rename', but it strips
   the version number from the "to" filename, such that the "to" file is
   will always be a new version.  It also sets the file protection once it is
   finished.  The protection that we will use is stored in vms_fab_final_pro,
   and was set when we did a creat_copy_attrs to create the file that we
   are renaming.

   We could use the chmod function, but Eunichs uses 3 bits per user category
   to describe the protection, and VMS uses 4 (write and delete are separate
   bits).  To maintain portability, the VMS implementation of `chmod' wires
   the W and D bits together.  */


static char vms_file_written[NAM$C_MAXRSS];

int
rename_sans_version (char *from, char *to)
{
  short int chan;
  int stat;
  short int iosb[4];
  int status;
  struct fibdef fib;
  struct FAB to_fab = cc$rms_fab;
  struct NAM to_nam = cc$rms_nam;
  struct dsc$descriptor fib_d ={sizeof (fib),0,0,(char*) &fib};
  struct dsc$descriptor fib_attr[2]
    = {{sizeof (vms_fab_final_pro),ATR$C_FPRO,0,(char*) &vms_fab_final_pro},{0,0,0,0}};
  char to_esn[NAM$C_MAXRSS];

  $DESCRIPTOR (disk,to_esn);

  memset (&fib, 0, sizeof (fib));

  to_fab.fab$l_fna = to;
  to_fab.fab$b_fns = strlen (to);
  to_fab.fab$l_nam = &to_nam;
  to_fab.fab$l_fop = FAB$M_NAM;

  to_nam.nam$l_esa = to_esn;
  to_nam.nam$b_ess = sizeof to_esn;

  status = SYS$PARSE (&to_fab, 0, 0); /* figure out the full file name */

  if (to_nam.nam$l_fnb && NAM$M_EXP_VER)
    *(to_nam.nam$l_ver) = '\0';

  stat = rename (from, to_esn);
  if (stat < 0)
    return stat;

  strcpy (vms_file_written, to_esn);

  to_fab.fab$l_fna = vms_file_written; /* this points to the versionless name */
  to_fab.fab$b_fns = strlen (vms_file_written);

  /* Now set the file protection to the correct value */
  SYS$OPEN (&to_fab, 0, 0);	/* This fills in the nam$w_fid fields */

  /* Copy these fields into the fib */
  fib.fib$r_fid_overlay.fib$w_fid[0] = to_nam.nam$w_fid[0];
  fib.fib$r_fid_overlay.fib$w_fid[1] = to_nam.nam$w_fid[1];
  fib.fib$r_fid_overlay.fib$w_fid[2] = to_nam.nam$w_fid[2];

  SYS$CLOSE (&to_fab, 0, 0);

  stat = SYS$ASSIGN (&disk, &chan, 0, 0); /* open a channel to the disk */
  if (!stat)
    LIB$SIGNAL (stat);
  stat = SYS$QIOW (0, chan, IO$_MODIFY, iosb, 0, 0, &fib_d,
		   0, 0, 0, &fib_attr, 0);
  if (!stat)
    LIB$SIGNAL (stat);
  stat = SYS$DASSGN (chan);
  if (!stat)
    LIB$SIGNAL (stat);
  strcpy (vms_file_written, to_esn); /* We will write this to the terminal*/
  return 0;
}

int
link (char *file, char *new)
{
  status;
  struct FAB fab;
  struct NAM nam;
  unsigned short fid[3];
  char esa[NAM$C_MAXRSS];

  fab = cc$rms_fab;
  fab.fab$l_fop = FAB$M_OFP;
  fab.fab$l_fna = file;
  fab.fab$b_fns = strlen (file);
  fab.fab$l_nam = &nam;

  nam = cc$rms_nam;
  nam.nam$l_esa = esa;
  nam.nam$b_ess = NAM$C_MAXRSS;

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  status = SYS$SEARCH (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  fid[0] = nam.nam$w_fid[0];
  fid[1] = nam.nam$w_fid[1];
  fid[2] = nam.nam$w_fid[2];

  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  nam.nam$w_fid[0] = fid[0];
  nam.nam$w_fid[1] = fid[1];
  nam.nam$w_fid[2] = fid[2];

  nam.nam$l_esa = nam.nam$l_name;
  nam.nam$b_esl = nam.nam$b_name + nam.nam$b_type + nam.nam$b_ver;

  status = SYS$ENTER (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return 0;
}

#ifdef getenv
/* If any place else asks for the TERM variable,
   allow it to be overridden with the EMACS_TERM variable
   before attempting to translate the logical name TERM.  As a last
   resort, ask for VAX C's special idea of the TERM variable.  */
#undef getenv
char *
sys_getenv (char *name)
{
  char *val;
  static char buf[256];
  static struct dsc$descriptor_s equiv
    = {sizeof (buf), DSC$K_DTYPE_T, DSC$K_CLASS_S, buf};
  static struct dsc$descriptor_s d_name
    = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
  short eqlen;

  if (!strcmp (name, "TERM"))
    {
      val = (char *) getenv ("EMACS_TERM");
      if (val)
	return val;
    }

  d_name.dsc$w_length = strlen (name);
  d_name.dsc$a_pointer = name;
  if (LIB$SYS_TRNLOG (&d_name, &eqlen, &equiv) == 1)
    {
      char *str = (char *) xmalloc (eqlen + 1);
      memcpy (str, buf, eqlen);
      str[eqlen] = '\0';
      /* This is a storage leak, but a pain to fix.  With luck,
	 no one will ever notice.  */
      return str;
    }
  return (char *) getenv (name);
}
#endif /* getenv */

#ifdef abort
/* Since VMS doesn't believe in core dumps, the only way to debug this beast is
   to force a call on the debugger from within the image. */
#undef abort
sys_abort (void)
{
  reset_all_consoles ();
  LIB$SIGNAL (SS$_DEBUG);
}
#endif /* abort */

#if 0 /* Apparently unused */
/* The standard `sleep' routine works some other way
   and it stops working if you have ever quit out of it.
   This one continues to work.  */

void
sys_sleep (int timeval)
{
  int time [2];
  static int zero = 0;
  static int large = -10000000;

  LIB$EMUL (&timeval, &large, &zero, time); 	  /* Convert to VMS format */

  SYS$CANTIM (1, 0);
  if (SYS$SETIMR (vms_timer_ef, time, 0, 1) & 1) /* Set timer */
    SYS$WAITFR (vms_timer_ef);	  /* Wait for timer expiry only */
}
#endif /* 0 */

void
bzero (register char *b, register int length)
{
  short zero = 0;
  long max_str = 65535;

  while (length > max_str) {
    (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
    length -= max_str;
    b += max_str;
  }
  max_str = length;
  (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
}

/* Saying `void' requires a declaration, above, where bcopy is used
   and that declaration causes pain for systems where bcopy is a macro.  */
bcopy (register char *b1, register char *b2, register int length)
{
  long max_str = 65535;

  while (length > max_str) {
    (void) LIB$MOVC3 (&max_str, b1, b2);
    length -= max_str;
    b1 += max_str;
    b2 += max_str;
  }
  max_str = length;
  (void) LIB$MOVC3 (&length, b1, b2);
}

int
bcmp (register char *b1, register char *b2, register int length)
/* This could be a macro! */
{
  struct dsc$descriptor_s src1 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b1};
  struct dsc$descriptor_s src2 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b2};

  return STR$COMPARE (&src1, &src2);
}

#endif /* VMS */

#ifndef HAVE_STRCASECMP
/*
 * From BSD
 */
static unsigned char charmap[] = {
        '\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
        '\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
        '\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
        '\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
        '\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
        '\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
        '\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
        '\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
        '\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
        '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
        '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
        '\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
        '\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
        '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
        '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
        '\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
        '\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
        '\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
        '\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
        '\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
        '\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
        '\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
        '\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
        '\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
        '\300', '\301', '\302', '\303', '\304', '\305', '\306', '\307',
        '\310', '\311', '\312', '\313', '\314', '\315', '\316', '\317',
        '\320', '\321', '\322', '\323', '\324', '\325', '\326', '\327',
        '\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
        '\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
        '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
        '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
        '\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

int
strcasecmp (char *s1, char *s2)
{
  unsigned char *cm = charmap;
  unsigned char *us1 = (unsigned char *) s1;
  unsigned char *us2 = (unsigned char *)s2;

  while (cm[*us1] == cm[*us2++])
    if (*us1++ == '\0')
      return (0);

  return (cm[*us1] - cm[*--us2]);
}
#endif /* !HAVE_STRCASECMP */
