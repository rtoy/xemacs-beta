/* System description file for Windows 9x and NT.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* Synched up with: FSF 19.31. */

/* See win32.c for info about the different Windows files in XEmacs. */

#include "win32-native.h"

/* In case non-Microsoft compiler is used, we fake _MSC_VER */
#ifndef _MSC_VER
#define _MSC_VER  1
#endif

/* Stuff from old nt/config.h: */

#define NTHEAP_PROBE_BASE 1

#ifdef HAVE_X_WINDOWS

#define HAVE_XREGISTERIMINSTANTIATECALLBACK

#define THIS_IS_X11R6
#define HAVE_XMU
#define HAVE_XLOCALE_H
#define HAVE_X11_LOCALE_H
#define GETTIMEOFDAY_ONE_ARGUMENT

#define LWLIB_USES_ATHENA
#define LWLIB_MENUBARS_LUCID
#define LWLIB_SCROLLBARS_LUCID
#define LWLIB_DIALOGS_ATHENA
#define LWLIB_TABS_LUCID
#define LWLIB_WIDGETS_ATHENA

/* These are what gets defined under Cygwin */
#define _BSD_SOURCE 1
#define _SVID_SOURCE 1
#define X_LOCALE 1
#define NARROWPROTO 1

#endif /* HAVE_X_WINDOWS */

#define HAVE_LOCALE_H
#define STDC_HEADERS

#define HAVE_LONG_FILE_NAMES

#define HAVE_TIMEVAL
#define HAVE_TZNAME
#define HAVE_H_ERRNO

#define HAVE_CLOSEDIR
#define HAVE_DUP2
#define HAVE_EXECVPE
#define HAVE_FMOD
#define HAVE_FREXP
#define HAVE_FTIME
#define HAVE_GETCWD
#define HAVE_GETHOSTNAME
#define HAVE_GETPAGESIZE
#define getpagesize() 4096
#define HAVE_GETTIMEOFDAY
#define HAVE_LINK
#define HAVE_LOGB
#define HAVE_MKDIR
#define HAVE_MKTIME
#define HAVE_RENAME
#define HAVE_RMDIR
#define HAVE_SELECT
#define HAVE_STRERROR
#define HAVE_STRLWR
#define HAVE_STRUPR

#define HAVE_SOCKETS

#ifdef DEBUG_XEMACS
#define USE_ASSERTIONS
#define MEMORY_USAGE_STATS
#endif /* DEBUG_XEMACS */

#define HAVE_DRAGNDROP

#define SIZEOF_SHORT 2
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_LONG_LONG 0
#define SIZEOF_VOID_P 4

typedef int mode_t;
typedef int pid_t;
typedef int uid_t;
typedef int gid_t;
typedef int pid_t;
typedef int ssize_t;

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_POP
#define HAVE_LOCKING
#define MAIL_USE_LOCKING

/* See unexnt.c */
#if (_MSC_VER >= 1100) && !defined(PDUMP)
#define DUMP_SEPARATE_SECTION
#endif
#ifdef DUMP_SEPARATE_SECTION
#pragma data_seg("xdata")
#pragma bss_seg("xdata")
#endif

#ifdef emacs
/* intl-auto-encap-win32.[ch] assumes _WIN32_WINNT>=0x0400
   We don't want this set when building command-line helpers in lib-src */
# ifndef _WIN32_WINNT
#  define _WIN32_WINNT 0x0400
# endif
#endif

/* The VC++ (5.0, at least) headers treat WINVER non-existent as 0x0400 */
#if defined (WINVER) && WINVER < 0x0400
# undef WINVER
# define WINVER 0x0400
#endif

/* Vararg routines, main(), and callback routines for library functions
   (qsort(), signal(), etc.) need to be __cdecl if we use the fastcall
   convention by default (a good idea, since it speeds things up). #### Why
   do they have to complain about this?  Why not just do the right thing
   automatically?

   Prefix with X because plain CDECL is already defined by the VC++ header
   files. */
#define XCDECL __cdecl

/* MSVC 6.0 has a mechanism to declare functions which never return */
#if (_MSC_VER >= 1200)
#define DOESNT_RETURN_TYPE(rettype) __declspec(noreturn) rettype
#define DECLARE_DOESNT_RETURN_TYPE(rettype,decl) \
  __declspec(noreturn) rettype XCDECL decl
#endif /* MSVC 6.0 */

/* MSVC warnings no-no crap.  When adding one to this section,
   1. Think twice.
   2. Insert textual description of the warning.
   3. Think again.  Undo still works. */
#if (_MSC_VER >= 800)

/* unnamed type definition in parentheses
  (Martin added a pedantically correct definition of ALIGNOF, which
  generates temporary anonymous structures, and MSVC complains) */
#pragma warning ( disable : 4116 )

#endif /* compiler understands #pragma warning*/

/* MSVC version >= 2.x without /Za supports __inline */
#if (_MSC_VER < 900) || defined (__STDC__)
# define inline
#else
# define inline __inline
#endif

/* lisp.h defines abort() as a macro.  therefore, we must include all
   files that contain prototypes for abort() before then. */
#include <../include/process.h>
