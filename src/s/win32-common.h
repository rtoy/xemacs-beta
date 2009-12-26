/* Common system description file for cygwin32/windowsnt/mingw32.
   Copyright (C) 1993, 1994, 1995, 1999 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* See win32.c for info about the different Windows files in XEmacs. */

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no load average available. */

#define NO_MATHERR

#define EXEC_SUFFIXES   ".exe:.com:.bat:.cmd:"

/* Define an identifier for all MS Windows systems -- Cygwin, native, MinGW */
#define WIN32_ANY
