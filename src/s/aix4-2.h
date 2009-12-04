/* Definitions file for XEmacs running on IBM AIX version 4.2
 *    Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.
 *
 *    This file is part of XEmacs.
 *
 *    XEmacs is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2, or (at your option)
 *    any later version.
 *
 *    XEmacs is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with XEmacs; see the file COPYING.  If not, write to
 *    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG				/* System III, System V, etc */
#define USG5

/*      Specify IBM AIX version of system */

#ifndef AIX
#define AIX
#define AIX4
#define AIX4_1
#endif

/*      This symbol should be defined on AIX Version 3  ??????? */
#ifndef _AIX
#define _AIX
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 *  It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "aix"

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
 *    is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special itemss needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 *  that the user program has elected to catch.  Thus the system call
 *   must be retried in these cases.  To handle this without massive
 *    changes in the source code, we remap the standard system call names
 *     to names for our own functions in sysdep.c that do the system call
 *      with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

#ifndef __GNUC__
#define LINKER "cc"
#endif

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* No need to specify -lc when linking.  */

#define LIB_STANDARD

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* AIX doesn't define this.  */
#define unix 1

#ifndef __GNUC__
/* Otherwise, XEmacs is just too big ... */
#define C_SWITCH_SYSTEM "-mminimal-toc"
#endif

#define HAVE_ALLOCA

/* FIXME: This was needed for AIX 4.1.  Is it for AIX 4.2? */
/* The character-composition stuff is broken in X11R5.
 * Even with XIMStatusNothing aliased to XIMStatusNone,
 * tranle@intellicorp.com (Minh Tran-Le) reports that enabling
 * the internationalization code causes the modifier keys C, M and Shift
 * to beep after a mouse click.  */
#define X11R5_INHIBIT_I18N

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
 *    because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
 *       "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#undef C_OPTIMIZE_SWITCH
#define C_DEBUG_SWITCH
#define C_OPTIMIZE_SWITCH "-O"
#endif

#define MAIL_USE_LOCKF

/* XEmacs: from dkeller@VNET.IBM.COM */
#define BROKEN_SIGIO

#ifndef NOT_C_CODE
#define _XFUNCS_H_ 1

/* #### we don't use either any more.  some AIX user should delete the include
 * and see if anything breaks. --ben */
/* AIX is happier when bzero and strcasecmp are declared */
#include "strings.h"

/* Forward declarations for xlc warning suppressions */
struct ether_addr;
struct sockaddr_dl;

#ifdef __xlC__			/* "eXceLlent C compiler" ?! */
#if __xlC__ >= 1200
/* IBM's C compiler option `-O3' is too aggressive.
 * We recommend instead the combination `-O3 -qstrict', which seems safe.
 *
 * cc -O3 miscompiles at least two functions.  From IBM's docs:
 *
 * IBM> -qstrict turns off the following optimizations:
 *
 * IBM> Performing code motion and scheduling on computations such as loads
 * IBM> and floating-point computations that may trigger an exception.
 *
 * Nevertheless, we try to work with these compiler options. */
#pragma option_override (bytecount_to_charcount, "opt(strict,yes)")
#pragma option_override (Fexpand_file_name, "opt(strict,yes)")
#endif /* recent IBM C compiler */
#endif /* IBM's C compiler */

#endif /* C code */

/* getaddrinfo is broken in AIX 4.3 as per IY04165.
 *    At this time (2/21/2000), there's no PTF available.
 *       -- Mike Sperber <mike@xemacs.org> */

#undef HAVE_GETADDRINFO

#ifdef __GNUC__
#undef _NO_PROTO
#endif

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
 *    because "cc -O" crashes.  Under AIX 3.2.5, "cc -O" is required because
 *       "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu.
 *          4.1 seems to need -g again. -- larry@vaquita.mitra.com.  */
/* David Edelsohn <dje@watson.ibm.com> says that this actually depends
 *    on the version of XLC, which can't be predicted from the system version.
 *       What a mess!  */
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#undef C_OPTIMIZE_SWITCH
#define C_DEBUG_SWITCH "-g"
#endif
