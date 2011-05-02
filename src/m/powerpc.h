/* machine description file for Power PC
   Copyright (C) 1987, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois
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

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="solaris2-5" */

#ifdef NOT_C_CODE
# define POWERPC
#else
# ifndef powerpc
#  define powerpc
# endif
#endif

/* Delete C_OPTIMIZE_SWITCH stuff, moved (currently brokenly) to
   configure.ac */

#ifndef __linux__

#ifdef ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

#endif /* ENABLE_SM_FILE_DECLS_OF_LOADAVG_STUFF */

#else /* mklinux */

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
/* #define A_TEXT_OFFSET(HDR) sizeof(HDR) */
/* #define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
*/
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* Specify the font for X to use.
   This used to be Rom14.500; that's nice on the X server shipped with
   the RS/6000, but it's not available on other servers.  */
#define X_DEFAULT_FONT "fixed"

/* Here override various assumptions in ymakefile */

/* #undef START_FILES */
/* #define HAVE_SYSVIPC */

/* Don't try to include sioctl.h or ptem.h.  */
#undef NEED_SIOCTL
#undef NEED_PTEM_H

#define ORDINARY_LINK
/*#define LD_SWITCH_MACHINE -T ${srcdir}/src/ppc.ldscript*/
#endif
