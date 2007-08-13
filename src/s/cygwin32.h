/* System description file for cygwin32.
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

/* Building under cygwin
 *
 * The approach I have taken with this port is to use primarily the UNIX 
 * code base adding stuff that is MS-Windows specific. This works quite 
 * well, and is in keeping with my perception of the cygwin philosophy.
 * Note that if you make changes to this file you do NOT want to define 
 * WINDOWSNT, I repeat - do not define this, it will break everything 
 * horribly. What does get defined is HAVE_MS_WINDOWS, but this is 
 * done by configure and only applies to the window system.
 *
 * The important thing about building is that it is done on a binary
 * mounted filesystem. i.e. something mounted like: mount -b c:
 * /binary. If you do not do this then compilation of el files will
 * produce garbage.  Make sure you have installed cygwin32 b18 +
 * patched dll (which can be found at http://www.lexa.ru/sos or on my
 * home page. Alternatively when b19 comes out the patched dll will be
 * unnecessary. Also make sure your HOME path is unix style -
 * i.e. without a drive letter.
 *
 * once you have done this, configure and make. The
 * undump phase will fail but that is to be expected. To run you need
 * to set EMACSLOADPATH, EMACSDOC, EMACSDATA etc appropriately and then do:
 * temacs -batch -l loadup.el run-temacs
 * 
 * What I want to do
 *
 * the fileio stuff merely uses the unix system calls this means that
 * the mount type of your fs will determine how files are edited. This
 * is fine except in the instance that you want to convert one to the
 * other. In this instance I would like to bring the buffer_file_type
 * code into the picture without all the other windows-nt cruft.
 *
 * Also the undumped version should be able to do path guessing, I
 * don't know why it doesn't currently.
 *
 * Ideally a dumped version would be done but I'm not sure I am up to
 * the task.
 *
 * Andy Piper <andyp@parallax.co.uk> 8/1/98 
 * http://parallax.co.uk/~andyp
 */


/* Need the win32 api */
#ifndef NOT_C_CODE
#ifdef CONST
#undef CONST
#endif 

/* Start and end of text and data.  */
extern void* _data_start__;
extern void* _data_end__;

#include <windows.h> 
#endif

#define HAVE_NTGUI
#define HAVE_FACES

#ifndef ORDINARY_LINK
#define ORDINARY_LINK
#endif

#undef MOD_ALT
#undef MOD_CONTROL
#undef MOD_SHIFT

#define SIF_TRACKPOS	0x0010
#define FW_BLACK	FW_HEAVY
#define FW_ULTRABOLD	FW_EXTRABOLD
#define FW_ULTRALIGHT	FW_EXTRALIGHT
#define TMPF_FIXED_PITCH	0x01
#define VK_APPS			0x5D
#define SIGPROF	0
#define NO_LIM_DATA
#define HAVE_TEXT_START

#undef MAIL_USE_SYSTEM_LOCK

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#define WORD_MACHINE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#define CANNOT_DUMP	1
#define	CANNOT_UNEXEC	1
#define UNEXEC /* unexnt.o */

#define DATA_START 	_data_start__
#define DATA_END 	_data_end__

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* Text does precede data space, but this is never a safe assumption.  */
#define VIRT_ADDR_VARIES

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

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

/*
 *      Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

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

/* Define this to be the separator between path elements */
/* #define DIRECTORY_SEP XINT (Vdirectory_sep_char) */

/* Define this to be the separator between devices and paths */
#define DEVICE_SEP ':'

#define SIGWINCH NSIG

/* We'll support either convention on NT.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))

/* The null device on Windows NT. */
#define NULL_DEVICE     "NUL:"
#define EXEC_SUFFIXES   ".exe:.com:.bat:.cmd:"

#define MODE_LINE_BINARY_TEXT(_b_) (NILP ((_b_)->buffer_file_type) ? "T" : "B")

/* For integration with MSDOS support.  
#define getdisk()               (_getdrive () - 1)
#define getdefdir(_drv, _buf)   _getdcwd (_drv, _buf, MAXPATHLEN)
*/

/* Defines size_t and alloca ().  */

/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 15000

/* ============================================================ */
