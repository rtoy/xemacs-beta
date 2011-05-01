/* Definitions file for XEmacs running on AT&T's System V Release 4
   Copyright (C) 1987, 1990 Free Software Foundation, Inc.

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

/* Synced up with: FSF 23.1.92. */
/* Synced by: Ben Wing, 2-18-10. */

/* This file written by James Van Artsdalen of Dell Computer Corporation.
 * james@bigtex.cactus.org.  Subsequently improved for Dell 2.2 by Eric
 * S. Raymond <esr@snark.thyrsus.com>.
 */

#define USG				/* System III, System V, etc */
#define USG5
#define USG5_4

/* SYSTEM_TYPE should indicate the kind of system you are using.
 * It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "usg-unix-v"

/* Delete HAVE_TERMIO, SYSV_SYSTEM_DIR, KERNEL_FILE, LDAV_SYMBOL,
   sigsetmask, _setjmp, _longjmp, HAVE_INDEX, HAVE_RINDEX, TERMINFO,
   HAVE_SYSV_SIGPAUSE, BSTRING, SIGTYPE -- not used in XEmacs or found by
   configure */

/* Letter to use in finding device name of first pty,
 * if system supports pty's.  'p' means it is /dev/ptyp0  */

#define FIRST_PTY_LETTER 'z'

/* define MAIL_USE_FLOCK if the mailer uses flock
 * to interlock access to /usr/spool/mail/$USER.
 * The alternative is that a lock file named
 * /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Special hacks needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 * that the user program has elected to catch.  Thus the system call
 * must be retried in these cases.  To handle this without massive
 * changes in the source code, we remap the standard system call names
 * to names for our own functions in sysdep.c that do the system call
 * with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Prevent -lg from being used for debugging.  Not implemented?  */

#define LIBS_DEBUG

/* 5.3 apparently makes close() interruptible */

#define INTERRUPTIBLE_CLOSE

/* Apparently -lg is provided in 5.3 */

#undef LIBS_DEBUG

/* Enable support for shared libraries in unexec.  */

#define USG_SHARED_LIBRARIES

#define LIBS_SYSTEM "-lsocket -lnsl -lelf"
#define ORDINARY_LINK
#define LIB_STANDARD

/* there are no -lg libraries on this system, and no libPW */

/* XEmacs deleted LIBS_DEBUG, LIB_STANDARD */

/* No <sioctl.h> */

#define NO_SIOCTL_H

#define UNEXEC "unexelf.o"

/* Get <sys/ttold.h> to get struct
 * tchars. But get <termio.h> first to make sure ttold.h doesn't
 * interfere.
 */

#ifndef NOT_C_CODE
#include <sys/wait.h>
#endif

#ifdef emacs
#include <sys/filio.h>
#include <termio.h>
#include <sys/ttold.h>
#include <sys/stream.h>
#include <sys/termios.h>
#endif

/* This sets the name of the master side of the PTY. */

#define PTY_NAME_SPRINTF qxestrcpy_ascii (pty_name, "/dev/ptmx");

/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after EMACS_UNBLOCK_SIGNAL. */

/* XEmacs change */
#ifndef NOT_C_CODE
# if !__STDC__ && !defined(STDC_HEADERS)
char *ptsname ();
# endif
#endif

#define PTY_TTY_NAME_SPRINTF				\
  {							\
    char *ptyname;					\
							\
    EMACS_BLOCK_SIGCHLD;				\
    if (grantpt (fd) == -1)				\
      { close (fd); return -1; }			\
    EMACS_UNBLOCK_SIGCHLD;				\
    if (unlockpt (fd) == -1)				\
      { close (fd); return -1; }			\
    if (!(ptyname = ptsname (fd)))			\
      { close (fd); return -1; }			\
    qxestrncpy_ascii (pty_name, ptyname,		\
		      sizeof (pty_name));		\
    pty_name[sizeof (pty_name) - 1] = 0;		\
  }

/* Push various streams modules onto a PTY channel. */

#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1)		\
    fatal ("ioctl I_PUSH ptem: errno %d\n", errno);	\
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)		\
    fatal ("ioctl I_PUSH ldterm: errno %d\n", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) 	\
    fatal ("ioctl I_PUSH ttcompat: errno %d\n", errno);

/* Tell x11term.c and keyboard.c we have the system V streams feature.  */
#define SYSV_STREAMS
/* On Some SysV System , w3 freeze. If freeze your xemacs , Add below definition */
/* This definition added by Shogo Fujii(shogo@bsd1.kbnes.nec.co.jp) */
#define PROCESS_IO_BLOCKING
