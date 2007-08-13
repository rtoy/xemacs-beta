/* This file is the configuration file for the GNU/Linux operating system,
   prior to version 1.1.56.
   Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31 (called, ahem ... lignux.h in FSF). */

/* This file was put together by Michael K. Johnson and Rik Faith.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
#define USG
/* #define BSD */
#define LINUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "linux"		/* All the best software is free. */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptyp0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* Both are used in Linux by different mail programs.  I assume that most
   people are using newer mailers that have heard of flock.  Change this
   if you need to. */

/*#define MAIL_USE_FLOCK*/

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* On POSIX systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

/* If you mount the proc file system somewhere other than /proc
   you will have to uncomment the following and make the proper
   changes */

/* #define LINUX_LDAV_FILE "/proc/loadavg" */

/* This is needed for dispnew.c:update_frame */

#ifdef emacs
#include <stdio.h>  /* Get the definition of _IO_STDIO_H.  */
#if defined(_IO_STDIO_H) || defined(_STDIO_USES_IOSTREAM)
/* new C libio names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#else /* !_IO_STDIO_H */
/* old C++ iostream names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_pptr - (FILE)->_pbase)
#endif /* !_IO_STDIO_H */
#endif /* emacs */

/* Ask GCC where to find libgcc.a.  */
#define LIB_GCC `$(CC) $(C_SWITCH_X_SITE) -print-libgcc-file-name`

#ifndef __ELF__
/* Linux has crt0.o in a non-standard place */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#else
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o
#endif

/* Check the version number of Linux--if it is at least 1.1.56,
   it is safe to use SIGIO.  If we can't find version.h (could happen
   if the user did a `make distclean' or something similar on the
   kernel distribution) just assume that SIGIO works, because nearly
   everybody should be running Linux 1.2 or later by now. */
#ifndef NOT_C_CODE
#ifdef emacs
#ifdef HAVE_LINUX_VERSION_H
#include <linux/version.h>

#if LINUX_VERSION_CODE < 0x10138
#define BROKEN_SIGIO
#endif /* LINUX_VERSION_CODE < 0x10138 */
#endif /* HAVE_LINUX_VERSION_H */
#endif /* emacs */
#endif /* NOT_C_CODE */

/* This is needed for sysdep.c */

#define NO_SIOCTL_H           /* don't have sioctl.h */

#define HAVE_SYS_SIGLIST
#define HAVE_WAIT_HEADER

#define POSIX                 /* affects getpagesize.h and systty.h */

/* Best not to include -lg, unless it is last on the command line */
#define LIBS_DEBUG
#define LIBS_TERMCAP -ltermcap -lcurses /* save some space with shared libs*/
#ifndef __ELF__
#define LIB_STANDARD -lc /* avoid -lPW */
#else
#undef LIB_GCC
#define LIB_GCC
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtn.o
#endif

/* Don't use -g in test compiles in configure.
   This is so we will use the same shared libs for that linking
   that are used when linking temacs.  */
#ifdef THIS_IS_CONFIGURE
#define C_DEBUG_SWITCH
#endif

/* Let's try this out, just in case.
   Nah.  Rik Faith <faith@cs.unc.edu> says it doesn't work well.  */
/* #define SIGNALS_VIA_CHARACTERS */

#ifdef TERM
#define LIBS_SYSTEM -lclient
#define C_SWITCH_SYSTEM -D_BSD_SOURCE -I/usr/src/term
#else
/* alane@wozzle.linet.org says that -lipc is not a separate library,
   since libc-4.4.1.  So -lipc was deleted.  */
#define LIBS_SYSTEM

#if 0 /* these options should either be cross-platform or removed - mrb */
/* XFree86 is built with -DFUNCPROTO=11 -DNARROWPROTO so we better build
   XEmacs with these switches too so that X functions get called correctly.
   At least XawScrollbarSetThumb needs this. */ 
#define C_SWITCH_SYSTEM -DFUNCPROTO=11 -DNARROWPROTO -D_BSD_SOURCE
#endif
     /* #define C_SWITCH_SYSTEM -DNARROWPROTO -D_BSD_SOURCE */
#define _BSD_SOURCE 1
#endif


/* XEmacs change: configure doesn't find this because math.h aliases
   rint to __rint so that it's not found. */
#define HAVE_RINT 1

#ifdef __ELF__
#define UNEXEC "unexelf.o"
#define UNEXEC_USE_MAP_PRIVATE
#endif

#ifdef LINUX_QMAGIC

#define HAVE_TEXT_START
#define UNEXEC "unexsunos4.o"
#define N_PAGSIZ(x) PAGE_SIZE

#else /* not LINUX_QMAGIC */

#define A_TEXT_OFFSET(hdr) (N_MAGIC(hdr) == QMAGIC ? sizeof (struct exec) : 0)
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#define ADJUST_EXEC_HEADER \
  unexec_text_start = N_TXTADDR(ohdr) + A_TEXT_OFFSET(ohdr)

#endif /* not LINUX_QMAGIC */

#if 0
/* In 19.23 and 19.24, configure sometimes fails to define these.
   It has to do with the fact that configure uses CFLAGS when linking
   while Makefile.in.in (erroneously) fails to do so when linking temacs.  */
#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif
#ifndef HAVE_MKDIR
#define HAVE_MKDIR
#endif
#ifndef HAVE_RMDIR
#define HAVE_RMDIR
#endif
#ifndef HAVE_XSCREENNUMBEROFSCREEN
#define HAVE_XSCREENNUMBEROFSCREEN
#endif
#endif /* 0 */

/* This is to work around mysterious gcc failures in some system versions.
   It is unlikely that Emacs changes will work around this problem;
   therefore, this should remain permanently.  */
#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

/* XEmacs addition: */
/* Linux defines these in <values.h>, but they can't be used in #if's
   Include values.h now so that we don't get complaints if it's included
   later.  This loses with glibc-2 (libc-6) */

/* # include <features.h> */
#if 0
#if !(defined (__GLIBC__) && (__GLIBC__ >= 2))

#include <values.h>
#undef  SHORTBITS
#undef  INTBITS
#undef  LONGBITS

#endif
#endif
/* The regex.o routines are a part of the GNU C-library used with Linux.  */
/* However, sometimes they disagree with the src/regex.h that comes with Emacs,
   and that can make trouble in etags.c because it gets the regex.h from Emacs
   and the function definitions in libc.  So turn this off.  */
/* XEmacs: in any case, Mule uses different regex routines. */
/* #define REGEXP_IN_LIBC */

/* XEmacs change: the standard linux libc includes regex routines in
   it.  We have to use our own and have to avoid name conflicts. */

#define re_compile_pattern sys_re_compile_pattern
#define re_search sys_re_search
#define re_search_2 sys_re_search_2
#define re_match_2 sys_re_match_2
#define re_max_failures sys_re_max_failures
#define re_set_syntax sys_re_set_syntax
#define re_set_registers sys_re_set_registers
#define re_compile_fastmap sys_re_compile_fastmap
#define re_match sys_re_match
#define regcomp sys_regcomp
#define regexec sys_regexec
#define regerror sys_regerror
#define regfree sys_regfree

/* XEmacs: Damon Lipparelli says that he incorrectly gets this
   defined on his system */
#undef GETTIMEOFDAY_ONE_ARGUMENT

/* Use BSD process groups, but use setpgid() instead of setpgrp() to
   actually set a process group. */

/* Formerly "BSD_PGRPS" */

#if 0 /* XEmacs (ben): I'm not convinced this is necessary and it has
	 lots of possibility of fuckup. */
#define SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
#endif
/* XEmacs: removed setpgrp() definition because we use setpgid() when
   it's available, and autodetect it. */

/* glibc fuckage */
#if defined __GLIBC__ && ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1) || __GLIBC__ > 2)
# define GETPGRP_NEEDS_ARG
#endif

#ifdef __ELF__
/* mrb - Ordinary link is simple and effective */
#define ORDINARY_LINK
#undef LIB_STANDARD
#undef START_FILES
#undef LIB_GCC
#endif /* __ELF__ */
