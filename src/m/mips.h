/* m- file for Mips machines.
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.31. */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Use m-mips4.h for RISCOS version 4; use s-bsd4-3.h with the BSD world.
Note that the proper m- file for the Decstation is m-pmax.h.
NOTE-END  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef mips
#	define mips
#endif

#ifdef ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / 256.0)

#endif /* ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF */

#ifndef linux
/* CDC EP/IX 1.4.3 uses /unix */

#ifdef ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"

#endif /* ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF */

#endif /* ! linux */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#ifndef linux
#define UNEXEC "unexmips.o"
#endif /* !linux */
/* Describe layout of the address space in an executing process.  */

#ifdef linux
#define TEXT_START      0x00400000
#define DATA_START      0x10000000
#else /* !linux */
#define TEXT_START 0x400000
#define DATA_START 0x800000
#endif /* linux */

/* Alter some of the options used when linking.  */

#if !defined(linux)
#ifdef BSD

/* DECstations don't have this library. */
/* #define LIBS_MACHINE "-lmld"  */

#define LD_SWITCH_MACHINE "-D 800000"
#define LIBS_DEBUG

#define LINKER "/bsd43/bin/ld"
  
#else /* not BSD */

#undef LIBS_MACHINE
#define LD_SWITCH_MACHINE "-D 800000 -g3"
#define START_FILES "pre-crt0.o /usr/lib/crt1.o"
#define LIB_STANDARD "-lbsd -lc /usr/lib/crtn.o"
/* LIBS_TERMCAP deleted */

#define C_SWITCH_MACHINE "-I/usr/include/bsd"
/* XEmacs deleted C_DEBUG_SWITCH */

#endif /* not BSD */
#endif /* !linux */

#if !defined(linux)
#ifdef USG

/* Don't try to use SIGIO even though it is defined.  */
#define BROKEN_SIGIO

/* Describe special kernel features.  */

#if defined(emacs) && !defined(INHIBIT_BSD_TIME)
#include <bsd/sys/time.h>
#endif

/* The `select' in the system won't work for pipes, so don't use it.  */
#undef HAVE_SELECT /* override configuration decision */

/* ??? */
#define IRIS

#endif /* USG */

#ifdef BSD
#define COFF
#define TERMINFO
#undef MAIL_USE_FLOCK  /* Someone should check this.  */
#endif /* BSD */
#endif /* !linux */
