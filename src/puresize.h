/* Definition of PURESIZE.
   Copyright (C) 1986, 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Not in FSF. */

#ifndef PURESIZE_H
#define PURESIZE_H

/* If PURESIZE is already defined then the user overrode it at
   configure time. */
#ifndef PURESIZE

/* Basic amount of purespace to use, in the absence of extra
   things configured in. */

#if (LONGBITS == 64)
# define BASE_PURESIZE 944000
#else
# define BASE_PURESIZE 584000
#endif

/* If any particular systems need to change the base puresize, they
   should define SYSTEM_PURESIZE_EXTRA.  Note that this can be
   negative as well as positive.

   Do NOT define PURESIZE or any other values.  This allows the
   other values to shift while still keeping things in sync. */

#ifndef SYSTEM_PURESIZE_EXTRA
# define SYSTEM_PURESIZE_EXTRA 0
#endif

/* Extra amount of purespace needed for menubars. */

#ifdef HAVE_MENUBARS
# if (LONGBITS == 64)
#  define MENUBAR_PURESIZE_EXTRA 43000
# else
#  define MENUBAR_PURESIZE_EXTRA 35000
# endif
#else
# define MENUBAR_PURESIZE_EXTRA 0
#endif

/* Scrollbar purespace needed is only about 2K so there's no sense
   worrying about it separately. */

/* Extra amount of purespace needed for X11, separate from menubars. */

#ifdef HAVE_X_WINDOWS
# if (LONGBITS == 64)
#  define X11_PURESIZE_EXTRA 95000
# else
#  define X11_PURESIZE_EXTRA 63000
# endif
#else
# define X11_PURESIZE_EXTRA 10000
#endif

/* Extra amount of purespace needed for Mule. */

#ifdef MULE
# if (LONGBITS == 64)
#  define MULE_PURESIZE_EXTRA 144000
# else
#  define MULE_PURESIZE_EXTRA 123000
# endif
#else
# define MULE_PURESIZE_EXTRA 0
#endif

/* Extra amount of purespace needed for Tooltalk. */

#ifdef TOOLTALK
# if (LONGBITS == 64)
#  define TOOLTALK_PURESIZE_EXTRA 100000
# else
#  define TOOLTALK_PURESIZE_EXTRA 69000
# endif
#else
# define TOOLTALK_PURESIZE_EXTRA 0
#endif

/* Extra amount of purespace needed for Energize builds. */

#ifdef ENERGIZE
# define ENERGIZE_PURESIZE_EXTRA 290000
#else
# define ENERGIZE_PURESIZE_EXTRA 0
#endif

/* Extra amount of purespace needed for Sunpro builds. */

#ifdef SUNPRO
# define SUNPRO_PURESIZE_EXTRA 135000
#else
# define SUNPRO_PURESIZE_EXTRA 0
#endif

#define PURESIZE ((BASE_PURESIZE) + (MENUBAR_PURESIZE_EXTRA) +		  \
		  (X11_PURESIZE_EXTRA) +				  \
		  (SYSTEM_PURESIZE_EXTRA) + (MULE_PURESIZE_EXTRA) +	  \
		  (TOOLTALK_PURESIZE_EXTRA) + (ENERGIZE_PURESIZE_EXTRA) + \
		  (SUNPRO_PURESIZE_EXTRA))

#endif /* !PURESIZE */

#endif /* PURESIZE_H */
