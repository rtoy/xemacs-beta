/* system description file for mingw32.
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

/* based on cygwin32.h by Andy Piper <andy@xemacs.org> */

/* NOTE: MinGW is a way of using GCC to target the native Windows
   environment.  This works similarly to building with Cygwin, but the
   resulting executable does not use the Cygwin DLL.  Instead, MINGW
   provides import libraries for the standard C library DLL's (specifically
   CRTDLL -- #### how does this differ from MSVCRT and LIBC.LIB?).  This
   means that code for MinGW will be very similar to code written for
   VC++.  See comment in windowsnt.h for more information on how Cygwin
   and native Windows relate. */

#include "win32-native.h"

#define MINGW

#ifndef ORDINARY_LINK
#define ORDINARY_LINK
#endif

#define C_SWITCH_SYSTEM "-mno-cygwin -fno-caller-saves -DWIN32_NATIVE -DMINGW"
#define LIBS_SYSTEM "-mno-cygwin -mwindows -lwinmm -lwsock32"
#define WIN32_LEAN_AND_MEAN

#define TEXT_START -1
#define HEAP_IN_DATA
#define UNEXEC "unexcw.o"

#define TIME_ONESHOT 0
#define TIME_PERIODIC 1
#define LOCALE_USE_CP_ACP 0x40000000
#define NSIG 23

/* this is necessary to get the TCS_* definitions in <commctrl.h> */
#define _WIN32_IE 0x0400

/* translate NT world unexec stuff to our a.out definitions */

#define strnicmp strncasecmp
/* #ifndef HAVE_SOCKETS */
#define HAVE_SOCKETS
/* #endif */
#define OBJECTS_SYSTEM nt.o ntheap.o ntproc.o dired-msw.o

#undef MAIL_USE_SYSTEM_LOCK
#define HAVE_MSW_C_DIRED

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no load average available. */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#ifndef NOT_C_CODE
#include <stdlib.h>
#include <../mingw/process.h>
#define mkdir __mkdir
#include <dir.h>
#undef mkdir
#ifdef HAVE_CYGWIN_VERSION_H
#include <cygwin/version.h>
#endif
#endif /* NOT_C_CODE */

#define DONT_USE_LITOUT

/* Stuff that gets set wrongly or otherwise */
#define HAVE_GETTIMEOFDAY
#define HAVE_SELECT
/* systime.h includes winsock.h which defines timeval */
#define HAVE_TIMEVAL
#define HAVE_GETPAGESIZE
#define getpagesize() 4096
#ifndef HAVE_H_ERRNO
#define HAVE_H_ERRNO
#endif
#ifndef HAVE_TZNAME
#define HAVE_TZNAME
#endif

#undef GETTIMEOFDAY_ONE_ARGUMENT
#undef HAVE_SYS_WAIT_H
#undef HAVE_TERMIOS
#undef SYSV_SYSTEM_DIR
#undef CLASH_DETECTION
