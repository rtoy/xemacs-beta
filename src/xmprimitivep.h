/* This file just includes the Motif header file PrimitiveP.h, but does
   the necessary magic to do this properly.

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

/* Motif attempts to use old '/ * * /' method of pasting tokens together
   unless __STDC__ > 0.  Bad idea, because the SunPro C compiler defines
   __STDC__ to 0 in "lenient ANSI mode" (which is what you need to
   compile Emacs in).  Unfortunately, some compilers don't let you mess
   around with __STDC__, so ... */

#if defined(__SUNPRO_C) && (__STDC__ == 0)
# undef __STDC__
# define __STDC__ 1
# define __STDC__CHANGED__
#endif

/* PrimitiveP.h doesn't exist in old versions of Motif; the stuff is
   in XmP.h instead */

#include <Xm/Xm.h>	/* to get XmVersion */
#if (XmVersion >= 1002)
# include <Xm/PrimitiveP.h>
#else
# include <Xm/XmP.h>
#endif

#ifdef __STDC__CHANGED__
# undef __STDC__
# define __STDC__ 0
# undef __STDC__CHANGED__
#endif
