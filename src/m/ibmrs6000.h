/* R2 AIX machine/system dependent defines
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.31. */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="aix3-1"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define IBMR2AIX

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
#ifdef USG5_4
#define CANNOT_DUMP
#endif

#ifndef UNEXEC
#define UNEXEC "unexaix.o"
#endif

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

#ifndef USG5_4
#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0
#endif

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.	*/

#ifndef USG5_4
#define DATA_SEG_BITS 0x20000000
#else
#define DATA_SEG_BITS 0
#endif

#ifdef CANNOT_DUMP
/* Define shared memory segment symbols */

#define PURE_SEG_BITS 0x30000000

/* Use shared memory.  */
/* This is turned off because it does not always work.	See etc/AIX.DUMP.  */
/* #define HAVE_SHM */
#define SHMKEY 5305035		/* used for shared memory code segments */
#endif /* CANNOT_DUMP */

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
/* #define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
*/
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.	 */

/* Note: aix3-2.h defines HAVE_ALLOCA; aix3-1.h doesn't.  */
#ifndef HAVE_ALLOCA
#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */
#endif

/* Specify the font for X to use.
   This used to be Rom14.500; that's nice on the X server shipped with
   the RS/6000, but it's not available on other servers.  */
#define X_DEFAULT_FONT "fixed"

/* Here override various assumptions in ymakefile */

#ifdef AIXHFT
#define OBJECTS_MACHINE "hftctl.o"
#endif

#ifndef USG5_4
/* XEmacs change -- commented out: dkeller@vnet.ibm.com */
/* #define C_SWITCH_MACHINE -D_BSD */
#endif

#ifdef AIX3_2
/* -lpthreads seems to be necessary for Xlib in X11R6, and should be harmless
   on older versions of X where it happens to exist.  */
#ifdef HAVE_LIBPTHREADS
#define LIBS_MACHINE "-lrts -lIM -liconv -lpthreads"
#else
/* IBM's X11R5 use -lIM and -liconv in AIX 3.2.2.  */
#define LIBS_MACHINE "-lrts -lIM -liconv"
#endif
#else
#ifdef USG5_4
#define LIBS_MACHINE
#else
#define LIBS_MACHINE "-lIM"
#endif
#endif

#define START_FILES

/* Don't try to include sioctl.h or ptem.h.  */
#undef NEED_SIOCTL
#undef NEED_PTEM_H

#define ORDINARY_LINK

#if 0 /* mrb */
#ifndef USG5_4
/* XEmacs change -- automatically add the correct path for smt.exp if
   it exists. */
/* marc@sti.com (Marc Pawliger) says ibmrs6000.inp is needed to avoid
   linker error for updated X11R5 libraries, which references pthread library
   which most machines don't have.  We use the name .inp instead of .imp
   because .inp is a better convention to use in make-dist for naming
   random input files.  */
/* Avoid gcc 2.7.x collect2 bug by using /bin/ld instead.  */
#if __GNUC__ == 2 && __GNUC_MINOR__ == 7
#ifdef AIX_SMT_EXP
#define LD_SWITCH_MACHINE "-B/bin/ -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp,AIX_SMT_EXP"
#else
#define LD_SWITCH_MACHINE "-B/bin/ -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp"
#endif
#else /* not gcc 2.7.x */
#ifdef AIX_SMT_EXP
#define LD_SWITCH_MACHINE "-Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp,AIX_SMT_EXP"
#else
#define LD_SWITCH_MACHINE "-Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp"
#endif
#endif /* __GNUC__ == 2 && __GNUC_MINOR__ == 7 */

#endif /* USG5_4 */
#endif /* mrb */
