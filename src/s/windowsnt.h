/* System description file for Windows 9x and NT.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* Synched up with: FSF 19.31. */

/* Capsule summary of different preprocessor flags:

1. Keep in mind that there are two possible OS environments we are dealing
   with -- Cygwin and Native Windows.  MS Windows natively provides
   file-system, process, and window-system services through the Win32 API,
   implemented by various DLL's. (The most important and KERNEL32, USER32,
   and GDI32.  KERNEL32 implements the basic file-system and process
   services.  USER32 implements the fundamental window-system services
   such as creating windows and handling messages.  GDI32 implements
   higher-level drawing capabilities -- fonts, colors, lines, etc.) The C
   library is implemented on top of Win32 using either MSVCRT (dynamically
   linked) or LIBC.LIB (statically linked).  Cygwin provides a POSIX
   emulation layer on top of MS Windows -- in particular, providing the
   file-system, process, tty, and signal semantics that are part of a
   modern, standard Unix operating system.  Cygwin does this using its own
   DLL, cygwin1.dll, which makes calls to the Win32 API services in
   kernel32.dll.  Cygwin also provides its own implementation of the C
   library, called `newlib' (libcygwin.a; libc.a and libm.a are symlinked
   to it), which is implemented on top of the Unix system calls provided
   in cygwin1.dll.  In addition, Cygwin provides static import libraries
   that give you direct access to the Win32 API -- XEmacs uses this to
   provide GUI support under Cygwin.  The two environments also use
   different compilers -- Native Windows uses Visual C++, and Cygwin uses
   GCC.  (MINGW, however, is a way of using GCC to target the Native
   Windows environment.  This works similarly to building with Cygwin, but
   the resulting executable does not use the Cygwin DLL.  Instead, MINGW
   provides import libraries for the standard C library DLL's
   (specifically CRTDLL -- #### how does this differ from MSVCRT and
   LIBC.LIB?).)

2. There are two windowing environments we can target XEmacs for when
   running under MS Windows -- Windows native, and X. (It may seem strange
   to write an X application under Windows, but there are in fact many X
   servers out there running on Windows, and as far as I know there is no
   real (or at least, that works well) networking Window-system extension
   under MS Windows.  Furthermore, if you're porting a Unix application to
   Windows and use Cygwin to assist you, it might seem natural to use an
   X server to avoid having to port all the code to Windows.) For XEmacs,
   there are various reasons people could come up with for why we would
   want to keep maintaining X Windows under MS Windows support.

That gives us four possible build environments.  I (Ben) build
regularly on fully-native-everything, Andy builds on Cygwin + MS
Windows + X Windows for windowing.

The build flags used for these divisions are:

CYGWIN -- for Cygwin-only stuff.
WIN32_NATIVE -- Win32 native OS-level stuff (files, process, etc.).  Applies
                whenever linking against the native C libraries -- i.e.
                all compilations with VC++ and with MINGW, but never Cygwin.
HAVE_X_WINDOWS -- for X Windows (regardless of whether under MS Win)
HAVE_MS_WINDOWS -- MS Windows native windowing system (anything related to
                   the appearance of the graphical screen).  May or may not
                   apply to any of VC++, MINGW, Cygwin.

Finally, there's also the MINGW build environment, which uses GCC
\(similar to Cygwin), but native MS Windows libraries rather than a
POSIX emulation layer (the Cygwin approach).  This environment defines
WIN32_NATIVE, but also defines MINGW, which is used mostly because
uses its own include files (related to Cygwin), which have a few
things messed up.


Formerly, we had a whole host of flags.  Here's the conversion, for porting
code from GNU Emacs and such:


WINDOWSNT -> WIN32_NATIVE
WIN32 -> WIN32_NATIVE
_WIN32 -> WIN32_NATIVE
HAVE_WIN32 -> WIN32_NATIVE
DOS_NT -> WIN32_NATIVE
HAVE_NTGUI -> WIN32_NATIVE, unless it ends up already bracketed by this
HAVE_FACES -> always true
MSDOS -> determine whether this code is really specific to MS-DOS (and not
         Windows -- e.g. DJGPP code); if so, delete the code; otherwise,
         convert to WIN32_NATIVE (we do not support MS-DOS w/DOS Extender
         under XEmacs)

__CYGWIN__ -> CYGWIN
__CYGWIN32__ -> CYGWIN
__MINGW32__ -> MINGW

*/

#include "win32-native.h"

/* In case non-Microsoft compiler is used, we fake _MSC_VER */
#ifndef _MSC_VER
#define _MSC_VER  1
#endif

/* Stuff from old nt/config.h: */

#define NTHEAP_PROBE_BASE 1

#define LISP_FLOAT_TYPE

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

#define HAVE_SOCKETS

#ifdef DEBUG_XEMACS
#define USE_ASSERTIONS
#define MEMORY_USAGE_STATS
#define ERROR_CHECK_EXTENTS
#define ERROR_CHECK_TYPECHECK
#define ERROR_CHECK_CHARBPOS
#define ERROR_CHECK_GC
#define ERROR_CHECK_MALLOC
#define ERROR_CHECK_BYTE_CODE
#define ERROR_CHECK_GLYPHS
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
#if (_MSC_VER >= 1100)
#define DUMP_SEPARATE_SECTION
#endif
#ifdef DUMP_SEPARATE_SECTION
#pragma data_seg("xdata")
#pragma bss_seg("xdata")
#endif

#ifdef HAVE_SCROLLBARS
/* Ensure the NT 4 mouse definitions in winuser.h are available */
# ifndef _WIN32_WINNT
#  define _WIN32_WINNT 0x0400
# endif
#endif

/* Force the various NT 4 structures and constants to be included; we're
   careful not to call (or even link with) functions not in NT 3.51 when
   running on 3.51, but when running on NT 4 or Win9x, we use the later
   functions, and need their headers. */
/* The VC++ (5.0, at least) headers treat WINVER non-existent as 0x0400 */
#if defined (WINVER) && WINVER < 0x0400
# undef WINVER
# define WINVER 0x0400
#endif

/* MSVC 6.0 has a mechanism to declare functions which never return */
#if (_MSC_VER >= 1200)
#define DOESNT_RETURN __declspec(noreturn) void
#define DECLARE_DOESNT_RETURN(decl) __declspec(noreturn) extern void decl
#define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
          __declspec(noreturn) extern void decl PRINTF_ARGS(str,idx)
#endif /* MSVC 6.0 */

/* MSVC warnings no-no crap. When adding one to this section,
   1. Think twice
   2. Insert textual description of the warning.
   3. Think twice. Undo still works  */
#if (_MSC_VER >= 800)

/* 'expression' : signed/unsigned mismatch */
/* #pragma warning ( disable : 4018 ) */
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
