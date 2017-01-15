/* s/ file for System V release 4.2.

   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010  Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* Synced up with: FSF 23.1.92. */
/* Synced by: Ben Wing, 2-18-10. */

#include "usg5-4.h"

/* fnf@cygnus.com says these exist.  */
#define HAVE_TCATTR
/* Delete HAVE_GETWD, POSIX_SIGNALS, sigsetmask, HAVE_SYSV_SIGPAUSE --
   unused or autoconfigured in XEmacs */

/* Motif needs -lgen.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"

/* Delete redefinition of PTY_TTY_NAME_SPRINTF, unnecessary since we
   handle signal-calling better -- XEmacs */

/* Delete NEED_LIBW, LIB_MOTIF -- unused in XEmacs */

/* arch-tag: 9bbfcfc1-19be-45a1-9699-af57b87da2c6
   (do not change this comment) */

/* XEmacs addition: */

/* XEmacs change: communicate to m/intel386.h */
#define USG5_4_2
