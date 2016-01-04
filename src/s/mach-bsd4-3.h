/* Definitions file for XEmacs running on Mach (BSD 4.3)
   Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

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

/* I don't care if this doesn't do more than including bsd-common.h;
   Mach is not BSD and the moment you forget it chances are that
   you're in deep shit.  */

#include "bsd-common.h"

/* The rest of this stuff is XEmacs additions. */
 
/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "mach"

/* Don't send signals to subprocesses by "typing" special chars at them. */
#undef SIGNALS_VIA_CHARACTERS

/* Delete defn of _POSIX_PATH_MAX -- we check for being defined before using
   it */

#ifndef NOT_C_CODE
typedef int pid_t;
/* XEmacs change */
typedef unsigned short mode_t;
#endif /* NOT_C_CODE */

#if (defined(i386) || defined(ibmrt))
  /* use drem() instead of fmod() -- this is a BUG in the compiler runtime. */
# define USE_DREM
#endif
