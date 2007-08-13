/*
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not really in FSF. */

/* Work around a problem that happens because math.h on hpux 7
   defines two static variables--which, in Emacs, are not really static,
   because `static' is defined as nothing.  The problem is that they are
   defined both in data.c and in floatfns.c.
   These macros prevent the name conflict.

   (Is it still necessary to define static to nothing on hpux7?
   Removing that would be the best fix. -jwz)
  */
# if defined (HPUX) && !defined (HPUX8)
#  define _MAXLDBL THIS_FILENAME ## _maxldbl
#  define _NMAXLDBL THIS_FILENAME ## _nmaxldbl
# endif

#ifdef MSDOS
/* These are redefined (correctly, but differently) in values.h.  */
#undef INTBITS
#undef LONGBITS
#undef SHORTBITS
#endif

#include <math.h>

#ifdef NO_MATHERR
#undef HAVE_MATHERR
#endif

#ifdef HAVE_MATHERR
# ifdef FLOAT_CHECK_ERRNO
#  undef FLOAT_CHECK_ERRNO
# endif
# ifdef FLOAT_CHECK_DOMAIN
#  undef FLOAT_CHECK_DOMAIN
# endif
#endif

#ifndef NO_FLOAT_CHECK_ERRNO
#define FLOAT_CHECK_ERRNO
#endif

#ifdef FLOAT_CHECK_ERRNO
# include <errno.h>
#endif

/* Avoid traps on VMS from sinh and cosh.
   All the other functions set errno instead.  */

#ifdef VMS
#undef cosh
#undef sinh
#define cosh(x) ((exp(x)+exp(-x))*0.5)
#define sinh(x) ((exp(x)-exp(-x))*0.5)
#endif /* VMS */

#ifndef isnan
# define isnan(x) ((x) != (x))
#endif

