/* Definitions file for XEmacs running on HPUX release 11.0.
 *    Copyright (C) 1985, 1986 Free Software Foundation, Inc.
 *
 *    This file is part of XEmacs.
 *
 *    XEmacs is free software: you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation, either version 3 of the License, or (at your
 *    option) any later version.
 *    
 *    XEmacs is distributed in the hope that it will be useful, but WITHOUT
 *    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 *    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *    for more details.
 *    
 *    You should have received a copy of the GNU General Public License
 *    along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.31. */

/* Only support for hp9000s800 currently */

#define ORDINARY_LINK

/* XEmacs: */
/* Don't tell the linker to link statically */
#ifdef NOT_C_CODE
#define START_FILES
#define LINKER "$(CC)"
#endif /* THIS IS YMAKEFILE */

/* get call to brk() when rerunning XEmacs */
/* #ifndef USE_GCC */
#define RUN_TIME_REMAP
/* #endif */

#define USG				/* System III, System V, etc */

#define USG5

#define HPUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 *  It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "hpux"

/* `nomultiplejobs' should be defined if your system's shell
 *  does not have "job control" (the ability to stop a program,
 *  run some other program, then continue the first one).
 *
 *  On hpux this depends on the precise kind of machine in use,
 *  so the m- file defines this symbol if appropriate.  */

/* Letter to use in finding device name of first pty,
 * if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

#define FIRST_PTY_LETTER 'p'

/* The symbol in the kernel where the load average is found
 * depends on the cpu type, so we let the m- files define LDAV_SYMBOL.  */

/* Special hacks needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 * that the user program has elected to catch.  Thus the system call
 * must be retried in these cases.  To handle this without massive
 * changes in the source code, we remap the standard system call names
 * to names for our own functions in sysdep.c that do the system call
 * with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO
/* XEmacs change */
#define INTERRUPTIBLE_CLOSE

/* Use the system provided termcap(3) library */
#define TERMINFO

/* The 48-bit versions are more winning for Emacs;
 * the ordinary ones don't give even 32 bits.  */
#define random lrand48
#define srandom srand48

/* Define extra libraries to load.
 * This should have -lBSD, but that library is said to make
 * `signal' fail to work.  */

#ifdef HPUX_NET
#define LIBS_SYSTEM "-ln"
#else
#define LIBS_SYSTEM
#endif

/* Some additional system facilities exist.  */

/* Baud-rate values in tty status have nonstandard meanings.  */

#define BAUD_CONVERT  \
{ 0, 50, 75, 110, 135, 150, 200, 300, 600, 900, 1200,  \
  1800, 2400, 3600, 4800, 7200, 9600, 19200, 38400 }

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
            sprintf (pty_name, "/dev/pty/tty%c%x", c, i);

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF \
	sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);

#ifdef HPUX_USE_SHLIBS
#define LD_SWITCH_SYSTEM
#else
#define LD_SWITCH_SYSTEM "-Xlinker -a -Xlinker archive"
#endif

#ifndef __GNUC__
/* Make room for enough symbols, so dispnew.c does not fail.  */
/* XEmacs: cognot@ensg.u-nancy.fr: C_SWITCH_SYSTEM already defined in hpux8.h,
 *                            -D_BSD makes hp CC choke on process.c
 *                            #define C_SWITCH_SYSTEM "-Wp,-H200000 -D_BSD"
 *                            */
#undef C_SWITCH_SYSTEM
#define C_SWITCH_SYSTEM "-Aa -D_HPUX_SOURCE"
#endif

/* mrb */
#undef LD_SWITCH_SYSTEM

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX11

/* AlainF 20-Jul-1996 -- fixes for 10.10, untested for 10.0x */
/* Fix kernel file name for 10.10 and later */
#define KERNEL_FILE "/stand/vmunix"
