/* Definitions file for XEmacs running on Silicon Graphics Irix system 6.5.

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
  2008, 2009, 2010  Free Software Foundation, Inc.
Copyright (C) 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* Synced up with: FSF 23.1.92. */
/* Synced by: Ben Wing, 2-18-10. */

#define IRIX6_5			/* [[ used in m/iris4d ]] -- not currently */
#include "usg5-4.h"

/* Delete sigsetmask, _longjmp, _setjmp */

/* Delete SETPGRP_RELEASES_CTTY */

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#endif

#ifdef SYSTEM_TYPE
#undef SYSTEM_TYPE
#endif
#define SYSTEM_TYPE "irix"

/* Delete SETUP_SLAVE_PTY -- unused in XEmacs */

/* Delete SIGNALS_VIA_CHARACTERS, PTY_TTY_NAME_SPRINTF, PTY_NAME_SPRINTF,
   PTY_ITERATION, PTY_OPEN -- unnecessary and/or autoconfigured on XEmacs */

/* Delete POSIX_SIGNALS -- autoconfigured in XEmacs */

/* Ulimit(UL_GMEMLIM) is busted...  */
#define ULIMIT_BREAK_VALUE 0x14000000

/* Delete PREFER_VSUSP -- unused in XEmacs */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Delete NARROWPROTO -- autoconfigured in XEmacs */

/* Delete USE_MMAP_FOR_BUFFERS -- unused in XEmacs */

/* arch-tag: ad0660e0-acf8-46ae-b866-4f3df5b1101b
   (do not change this comment) */


#if _MIPS_SZLONG == 64		/* -mabi=64 (gcc) or -64 (MIPSpro) */
#define _LP64			/* lisp.h takes care of the rest */
#endif /* _MIPS_SZLONG */

/* #### Delete C_DEBUG_SWITCH, but should be moved to configure.ac;
   see comments there under with_cflags_debugging */
/* #define C_DEBUG_SWITCH -g3 -O -OPT:Olimit=3500 */

/* Delete undef of SA_RESTART, TIOCSIGSEND -- unused in XEmacs */

/* Delete GC_SETJMP_WORKS, GC_MARK_STACK -- unused in XEmacs */

/* arch-tag: d7ad9ec2-54ad-4b2f-adf2-0070c5c63e83
   (do not change this comment) */



/* XEmacs additions: */

/* jpff@maths.bath.ac.uk reports `struct exception' is not defined
 * on this system, so inhibit use of matherr.  */
#define NO_MATHERR

/* use K&R C */
/* XEmacs change -- use ANSI, not K&R */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM "-xansi"
#endif

/* jackr@engr.sgi.com says that you can't mix different kinds of
 * signal-handling functions under IRIX 5.3.  I'm going to assume
 * that that was the reason this got broken.  Now that the
 * signal routines are fixed up, maybe this will work. --ben */
/* Nope, it doesn't.  I've tried lots of things; it must be
 * genuinely broken. */
/* XEmacs addition: People on IRIX 5.2 and IRIX 5.3 systems have
 * reported that they can't break out of (while t) using C-g or C-G.
 * This does not occur on other systems, so let's assume that SIGIO
 * is broken on these systems. */
#define BROKEN_SIGIO

/* By Tor Arntsen <tor@spacetec.no> for XEmacs.
 * With the following kludge the above LD_SWITCH_SYSTEM will still work just 
 * fine even with USE_GCC, and additional tweaking of config.h or ymakefile 
 * is avoided. */
#ifdef NOT_C_CODE
# ifdef USE_GCC
#  undef LINKER
#  undef LIB_GCC
#  define LINKER "ld"
#  define LIB_GCC "`gcc --print`"
#  endif
#endif
