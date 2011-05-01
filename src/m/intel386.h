/* Machine description file for intel 386.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
Intel 386 (-machine=intel386 or -machine=is386.h)

  The possibilities for -opsystem are: bsd4-2, usg5-4, usg5-4-2, and linux.

  18.58 should support a wide variety of operating systems.
  Use linux for Linux.
  It isn't clear what to do on an SCO system.

Cubix QBx/386 (-machine=intel386 -opsystem=usg5-3)

  Changes merged in 19.1.  Systems before 2/A/0 may fail to compile etags.c
  due to a compiler bug.
NOTE-END */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define INTEL386

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG, NO_REMAP says this isn't used. */

/* Mly 16-Jan-96 16:38:32: this is part of a prototype -- same bug present in 
   other m*.h files */
#define CRT0_DUMMIES int bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

#ifdef SOLARIS2

#ifdef ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF

/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))

#endif /* ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF */

/* configure thinks solaris X86 has gethostname, but it does not work,
   so undefine it.  */
#undef HAVE_GETHOSTNAME

#else /* not SOLARIS2 */
#ifdef USG5_4 /* Older USG systems do not support the load average.  */
/* Data type of load average, as read out of kmem.  */

#ifdef ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#define FSCALE 256.0

#endif /* ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF */

#endif
#endif /* not SOLARIS2 */

#ifdef USG
#define NO_REMAP 
#define TEXT_START 0
#endif /* USG */

#ifdef linux
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif
