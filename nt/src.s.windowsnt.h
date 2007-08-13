/* System description file for Windows NT.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
/* #define USG */
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
/* #define BSD4_2 */
/* #define BSD4_3 */
/* #define BSD */
/* #define VMS */
#ifndef WINDOWSNT
#define WINDOWSNT
#endif
#ifndef DOS_NT
#define DOS_NT 	/* MSDOS or WINDOWSNT */
#endif

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here */
#define _VARARGS_ __cdecl

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal, or main) its calling
   convention must be whatever standard the libraries expect */
#define _CALLBACK_ __cdecl

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "windows-nt"

#define NO_MATHERR

#define SIZEOF_LONG 4
#define SIZEOF_INT 4
#define SIZEOF_SHORT 4

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

/*
 *      Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *      Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL

/*
 *      Define HAVE_SELECT if the system supports the `select' system call.
 */

/* #define HAVE_SELECT */

/*
 *      Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* NT supports Winsock which is close enough (with some hacks) */

#define HAVE_SOCKETS

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */
#define MAIL_USE_POP
#define MAIL_USE_SYSTEM_LOCK

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
#define SEPCHAR ';'

/* ============================================================ */

/* Here, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

#if 0
/* Define this to be the separator between path elements */
#define DIRECTORY_SEP XINT (Vdirectory_sep_char)
#endif

/* Define this to be the separator between devices and paths */
#define DEVICE_SEP ':'

/* We'll support either convention on NT.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))

/* The null device on Windows NT. */
#define NULL_DEVICE     "NUL:"
#define EXEC_SUFFIXES   ".exe:.com:.bat:.cmd:"

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

#define LISP_FLOAT_TYPE

#define HAVE_SYS_TIMEB_H
#undef HAVE_SYS_TIME_H
#undef HAVE_UNISTD_H
#undef STDC_HEADERS
#undef TIME_WITH_SYS_TIME

#define HAVE_GETTIMEOFDAY
#define HAVE_GETHOSTNAME
#define HAVE_DUP2
#define HAVE_RENAME
#define HAVE_CLOSEDIR

#define HAVE_TZNAME

#define HAVE_LONG_FILE_NAMES

#define HAVE_MKDIR
#define HAVE_RMDIR
#define HAVE_RANDOM
#define HAVE_BCOPY
#define HAVE_BCMP
#define HAVE_LOGB
#define HAVE_FREXP
#define HAVE_FMOD
#define HAVE_FTIME
#define HAVE_MKTIME

#define HAVE_MOUSE
#define HAVE_H_ERRNO

#ifdef HAVE_NTGUI
#define HAVE_WINDOW_SYSTEM
#define HAVE_FACES
#endif

#define MODE_LINE_BINARY_TEXT(_b_) (NILP ((_b_)->buffer_file_type) ? "T" : "B")

/* get some redefinitions in place */

#if 0
/* IO calls that are emulated or shadowed */
#define access  sys_access
#define chdir   sys_chdir
#define chmod   sys_chmod
#define close   sys_close
#define creat   sys_creat
#define ctime sys_ctime
#define dup     sys_dup
#define dup2    sys_dup2
#define fopen   sys_fopen
#define link    sys_link
#define mkdir   sys_mkdir
#define mktemp  sys_mktemp
#define open    sys_open
#define read    sys_read
#define rename  sys_rename
#define rmdir   sys_rmdir
#define select  sys_select
#define unlink  sys_unlink
#define write   sys_write
#endif

#if 0
/* this is hacky, but is necessary to avoid warnings about macro
   redefinitions using the SDK compilers */
#ifndef __STDC__
#define __STDC__ 1
#define MUST_UNDEF__STDC__
#endif
#include <direct.h>
#include <io.h>
#include <stdio.h>
#ifdef MUST_UNDEF__STDC__
#undef __STDC__
#undef MUST_UNDEF__STDC__
#endif
#endif


/* IO calls that are emulated or shadowed */
#define pipe    sys_pipe
#define sleep   sys_sleep

/* subprocess calls that are emulated */
#define spawnve sys_spawnve
#define wait    sys_wait
#define kill    sys_kill
#define signal  sys_signal

/* map to MSVC names */
#define execlp    _execlp
#define execvp    _execvp
#define fcloseall _fcloseall
#define fdopen          _fdopen
#define fgetchar  _fgetchar
#define fileno          _fileno
#define flushall  _flushall
#define fputchar  _fputchar
#define getw    _getw
#define getpid    _getpid
#define isatty    _isatty
#define logb      _logb
#define _longjmp  longjmp
#define lseek     _lseek
#define popen     _popen
#define pclose    _pclose
#define putw    _putw
#define umask   _umask
#define utime   _utime
#define index     strchr
#define rindex    strrchr
#define read	_read
#define write	_write

#ifdef HAVE_NTGUI
#define abort	win32_abort
#endif

/* Defines that we need that aren't in the standard signal.h  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGTRAP 5               /* Trace trap */
#define SIGKILL 9               /* Die, die die */
#define SIGPIPE 13              /* Write on pipe with no readers */
#define SIGALRM 14              /* Alarm */
#define SIGCHLD 18              /* Death of child */

/* For integration with MSDOS support.  */
#define getdisk()               (_getdrive () - 1)
#define getdefdir(_drv, _buf)   _getdcwd (_drv, _buf, MAXPATHLEN)

#define EMACS_CONFIGURATION 	get_emacs_configuration ()
#define EMACS_CONFIG_OPTIONS	"NT"	/* Not very meaningful yet.  */

/* Define this so that winsock.h definitions don't get included when windows.h
   is...  I don't know if they do the right thing for emacs.  For this to
   have proper effect, config.h must always be included before windows.h.  */
#define _WINSOCKAPI_    1

/* Defines size_t and alloca ().  */
#include <malloc.h>

#include <sys/stat.h>

/* Define for those source files that do not include enough NT 
   system files.  */
#ifndef NULL
#ifdef __cplusplus
#define NULL	0
#else
#define NULL	((void *)0)
#endif
#endif

/* For proper declaration of environ.  */
#include <stdlib.h>
#include <string.h>

/* Emacs takes care of ensuring that these are defined.  */
#ifdef max
#undef max
#undef min
#endif

/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 15000

/* ============================================================ */
