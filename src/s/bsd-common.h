/* Definitions file for GNU Emacs running on bsd 4.3
   Copyright (C) 1985, 1986, 2001, 2002, 2003, 2004, 2005, 2006,
                 2007, 2008, 2009, 2010  Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

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

/* Synced up with: FSF 23.1.92 */
/* Synced by: Ben Wing, 2-17-10 */

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* We give these symbols the numeric values found in <sys/param.h> to
   avoid warnings about redefined macros.  */
#ifndef BSD4_3
#define BSD4_3 1
#endif /* BSD4_3 */

#ifndef BSD_SYSTEM
#define BSD_SYSTEM 43
#endif /* BSD_SYSTEM */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 *  It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "berkeley-unix"

/* Deleted INTERRUPT_INPUT, FIRST_PTY_LETTER, HAVE_PTYS, HAVE_SOCKETS,
   BSTRING -- unnecessary and/or autoconfigured on XEmacs */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Deleted CLASH_DETECTION, KERNEL_FILE, LDAV_SYMBOL, SIGNALS_VIA_CHARACTERS
   -- unnecessary and/or autoconfigured on XEmacs */

/* arch-tag: 0c367245-bde3-492e-9029-3ff6898beb95
   (do not change this comment) */

/* XEmacs addition: `ifdef BSD' used in many places */

#ifndef BSD
#define BSD 43
#endif /* BSD */
