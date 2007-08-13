/* machine description file for Power PC
   Copyright (C) 1987, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

#ifdef __GNUC__
# define C_OPTIMIZE_SWITCH -O
#else
/* XEmacs change */
# ifdef USE_LCC
#  define C_OPTIMIZE_SWITCH -O4 -Oi
# else
     /* This level of optimization is reported to work.  */
#  define C_OPTIMIZE_SWITCH -O2
# endif
#endif

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)
