/* Common system description file for windowsnt/mingw32.
   Copyright (C) 1993, 1994, 1995, 1999 Free Software Foundation, Inc.
   Copyright (C) 2001 Ben Wing.

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

#include "win32-common.h"

/* Identify ourselves */
#ifndef WIN32_NATIVE
#define WIN32_NATIVE
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "windows-nt"

/* The null device on Windows NT. */
#define NULL_DEVICE     "NUL:"

/* Setitimer is emulated */
#define HAVE_SETITIMER

/* Define process implementation */
#define HAVE_WIN32_PROCESSES

/* These two are used in etags.c. */
#define popen _popen
#define pclose _pclose
