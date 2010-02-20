/* s/ file for netbsd system.

   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007,
   Copyright (C) 2010 Ben Wing.
                 2008, 2009, 2010  Free Software Foundation, Inc.

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

/* Get most of the stuff from bsd-common */
#include "bsd-common.h"

#if defined (__alpha__) && !defined (__ELF__)
#define NO_SHARED_LIBS
#endif

/* Delete BSD4_2 -- unused in XEmacs */

/* KERNEL_FILE, LDAV_SYMBOL HAVE_GETLOADAVG deleted */

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* netbsd uses OXTABS instead of the expected TAB3.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* HAVE_TERMIOS, NO_TERMIO deleted */

/* XEmacs deleted LIBS_DEBUG */
/* -lutil is not needed for NetBSD >0.9.  */
/* #define LIBS_SYSTEM -lutil */
/* LIBS_TERMCAP deleted */

#define NEED_ERRNO
/* SYSV_SYSTEM_DIR deleted */

/* BSD_PGRPS deleted */

#if !defined (NO_SHARED_LIBS) && defined (__ELF__)
#define UNEXEC "unexelf.o"
#endif

#if 0 /* Following mrb, this stuff is probably unneeded for XEmacs */
#if !defined (NO_SHARED_LIBS) && defined (__ELF__)
#define START_FILES pre-crt0.o /usr/lib/crt0.o START_FILES_1 /usr/lib/crtbegin.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o END_FILES_1
#undef LIB_GCC
#define LIB_GCC
#endif

#ifdef HAVE_CRTIN
#define START_FILES_1 /usr/lib/crti.o
#define END_FILES_1 /usr/lib/crtn.o
#else
#define START_FILES_1
#define END_FILES_1
#endif
#else /* not 0 */
/* XEmacs: */
#define HAVE_TEXT_START		/* No need to define `start_of_text'.  */
#define ORDINARY_LINK
#endif /* 0 */

#define AMPERSAND_FULL_NAME

#if 0  /* Following mrb, this stuff is probably unneeded for XEmacs */
#ifdef __ELF__
/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  We convert it to a -rpath option
   which is what OSF1 uses.  */
#define LD_SWITCH_SYSTEM_tmp `echo LD_SWITCH_X_SITE_AUX | sed -e 's/-R/-Wl,-rpath,/'`
#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp -Wl,-rpath,/usr/pkg/lib -L/usr/pkg/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib

/* The following is needed to make `configure' find Xpm, Xaw3d and
   image include and library files if using /usr/bin/gcc.  That
   compiler seems to be modified to not find headers in
   /usr/local/include or libs in /usr/local/lib by default.  */

#define C_SWITCH_SYSTEM -I/usr/X11R6/include -I/usr/pkg/include -I/usr/local/include -L/usr/pkg/lib -L/usr/local/lib

/* Link temacs with -z nocombreloc so that unexec works right, whether or
   not -z combreloc is the default.  GNU ld ignores unknown -z KEYWORD
   switches, so this also works with older versions that don't implement
   -z combreloc.  */

#define LD_SWITCH_SYSTEM_TEMACS -Wl,-z,nocombreloc
#endif /* __ELF__ */

/* On post 1.3 releases of NetBSD, gcc -nostdlib also clears
   the library search parth, i.e. it won't search /usr/lib
   for libc and friends. Using -nostartfiles instead avoids
   this problem, and will also work on earlier NetBSD releases */

#define LINKER "$(CC) -nostartfiles"

#endif /* 0 */

/* NARROWPROTO deleted */

/* #define DEFAULT_SOUND_DEVICE "/dev/audio" -- unused in XEmacs */

/* #include <signal.h> no need in XEmacs */

/* DONT_REOPEN_PTY deleted -- unused in XEmacs */

/* Delete GC_SETJMP_WORKS, GC_MARK_STACK -- unused in XEmacs */

/* POSIX_SIGNALS deleted */

/* arch-tag: e80f364a-04e9-4faf-93cb-f36a0fe95c81
   (do not change this comment) */
#endif /* 0 */

/* Begin XEmacs additions */
#undef BSD

#ifndef NOT_C_CODE
#include <sys/param.h>
#include <sys/exec.h>
#endif /* C_CODE */

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define NO_MATHERR
