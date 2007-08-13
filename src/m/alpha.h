/* machine description file For the alpha chip.
   Copyright (C) 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Use -opsystem=osf1
NOTE-END

*/

#define DEC_ALPHA		/* Digital Alpha AXP */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* XEmacs addition: is this necessary? */

/* XEmacs: Apparently not Andrew Cohen  8/24/95 */
/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

/* #define SIGN_EXTEND_CHAR(c) ((signed char)(c)) */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

/* __alpha defined automatically */


/* Use type EMACS_INT rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

/* XEmacs change:  something is not quite right with this defined. */
/* I'm sure this is because VALBITS is defined to be 28 instead of 60.
   --ben */
#undef NO_UNION_TYPE

/* Define the type to use.  */
#define EMACS_INT long
#define EMACS_UINT unsigned long
#define SPECIAL_EMACS_INT

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */
/* Load average requires special crocks.  Version 19 has them.
   For now, don't define this.  */

#define LOAD_AVE_TYPE long 

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define HAVE_ALLOCA

/* GNU malloc and the relocating allocator do not work together
   with X.   [Who wrote that?]  */

/* May 1995: reportedly [Rainer Schoepf <schoepf@uni-mainz.de>] both the
   system and the gnu malloc system work with "alpha-dec-osf3.0" and
   "alpha-dec-osf3.2".  */

/* May 1995: it seems to me [Morten Welinder <terra@diku.dk>] that both
   mallocs work with "alpha-dec-osf2.0", but I daren't break anything
   right now.  Feel free to play if you want.  */

/* #define SYSTEM_MALLOC */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/* #define NO_REMAP */

/* Some really obscure 4.2-based systems (like Sequent DYNIX)
 * do not support asynchronous I/O (using SIGIO) on sockets,
 * even though it works fine on tty's.  If you have one of
 * these systems, define the following, and then use it in
 * config.h (or elsewhere) to decide when (not) to use SIGIO.
 *
 * You'd think this would go in an operating-system description file,
 * but since it only occurs on some, but not all, BSD systems, the
 * reasonable place to select for it is in the machine description
 * file.
 */

/* #define NO_SOCK_SIGIO */


#define HAVE_X11R4
#define HAVE_X11R5


/* Describe layout of the address space in an executing process.  */

#define TEXT_START    0x120000000
#define DATA_START    0x140000000

#if 0 /* #### XEmacs: see below */
/* This is necessary for mem-limits.h, so that start_of_data gives
   the correct value */
#define DATA_SEG_BITS 0x140000000
#endif /* 0 */

#ifdef OSF1
#define ORDINARY_LINK
/* Some systems seem to have this, others don't.  */
#ifdef HAVE_LIBDNET
#define LIBS_MACHINE -ldnet
#else
#define LIBS_MACHINE -ldnet_stub
#endif /* HAVE_LIBDNET */
#endif /* OSF1 */

#if 0 /* Rainer Schoepf <schoepf@uni-mainz.de> says this loses with X11R6
	 since it has only shared libraries.  */
#ifndef __GNUC__
/* This apparently is for the system ld as opposed to Gnu ld.  */
#ifdef OSF1
#define LD_SWITCH_MACHINE      -non_shared
#endif /* OSF1 */
#endif /* not __GNUC__ */
#endif /* 0 */

#ifdef OSF1
#define LIBS_DEBUG
#define START_FILES pre-crt0.o 
#endif /* OSF1 */

#ifdef LINUX
/* This controls a conditional in main.  */
#define LINUX_SBRK_BUG
#endif /* LINUX */

/* The program to be used for unexec. */

#define UNEXEC unexalpha.o

#define PNTR_COMPARISON_TYPE unsigned long

#if 0

/* XEmacs -- removed code to define XINT, etc.  This gets
   correctly handled automatically. */

/* Declare malloc and realloc in a way that is clean.
   But not in makefiles!  */

#ifndef NOT_C_CODE
/* We need these because pointers are larger than the default ints.  */
#include <alloca.h>
#endif /* not NOT_C_CODE */

#endif /* 0 */

#ifdef OSF1
#define PTY_ITERATION		for (i = 0; i < 1; i++) /* ick */
#define PTY_NAME_SPRINTF	/* none */
#define PTY_TTY_NAME_SPRINTF	/* none */
#define PTY_OPEN					\
  do							\
    {							\
      int dummy;					\
      int mask = sigsetmask (SIGCHLD);			\
      if (-1 == openpty (&fd, &dummy, pty_name, 0, 0))	\
	fd = -1;					\
      sigsetmask (mask);				\
      close (dummy);					\
    }							\
  while (0)
#endif /* OSF1 */

#ifdef linux
#define COFF
#define TEXT_END ({ extern int _etext; &_etext; })
#define DATA_END ({ extern int _EDATA; &_EDATA; })
#endif /* linux */

/* XEmacs: The Dec CC compiler (but apparently not GCC, which uses different
   macros due to its own compiler bug) breaks spectacularly
   when MACROIZE_ERROR_CHECKING is turned on. */
#define BROKEN_MACROIZE_ERROR_CHECKING

/* XEmacs: from Andrew Cohen based on this comment found in <termio.h>:

   Unfortunately System V and POSIX have different values for some of
   the defines.  So we have to perform the following kluge which
   redefines the V___ values for termio.  This means you cannot do
   ioctls using termios and termio in the same source file.
   */
#define NO_TERMIO 1
