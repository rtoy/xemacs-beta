/* Definitions file for GNU Emacs running on AT&T's System V Release 4
   Copyright (C) 1987, 1990, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

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

#define ORDINARY_LINK

/* there are no -lg libraries on this system, and no libPW */

/* XEmacs deleted LIBS_DEBUG, LIB_STANDARD */

/* Undump with ELF */

#undef COFF

#define UNEXEC "unexelf.o"

/* Get FIONREAD from <sys/filio.h>.  Get <sys/ttold.h> to get struct
 * tchars. But get <termio.h> first to make sure ttold.h doesn't
 * interfere.  And don't try to use SIGIO yet.
 */

#ifndef NOT_C_CODE
#include <sys/wait.h>
#endif

#ifdef emacs
#include <sys/filio.h>
#include <termio.h>
#include <sys/ttold.h>
/* Delete #include <signal.h> */
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/termios.h>
/* XEmacs -- GNU added this, but we never had it defined and C-g apparently
   worked fine before, so don't define it */
/* #define BROKEN_SIGIO */
#endif

/* Delete NSIG_MINIMUM -- unused in XEmacs */

/* Delete CLASH_DETECTION (config option), HAVE_PTYS, HAVE_TERMIOS,
   wait3, WRETCODE, TIOCSIGSEND -- not used in XEmacs or found by configure */

/* Delete FIRST_PTY_LETTER, PTY_NAME_SPRINTF, PTY_TTY_NAME_SPRINTF --
   duplicative of code already in process-unix.c */

/* Delete SETUP_SLAVE_PTY -- unused in XEmacs */

/* Delete HAVE_SOCKETS -- autodetected */



/* Begin XEmacs additions */

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Enable support for shared libraries in unexec.  */

#define USG_SHARED_LIBRARIES

#define LIBS_SYSTEM "-lsocket -lnsl -lelf"

/* On Some SysV System , w3 freeze. If freeze your xemacs , Add below definition */
/* This definition added by Shogo Fujii(shogo@bsd1.kbnes.nec.co.jp) */
#define PROCESS_IO_BLOCKING
