/* src/config.h.  Generated automatically by configure.  */
/* XEmacs site configuration template file.  -*- C -*-
   Copyright (C) 1986, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30 (more or less). */

/* No code in XEmacs #includes config.h twice, but some of the code
   intended to work with other packages as well (like gmalloc.c) 
   think they can include it as many times as they like.  */
#ifndef _CONFIG_H_
#define _CONFIG_H_

/* #### This will be removed in 19.15. */
#define LOSING_BYTECODE


/* These are all defined in the top-level Makefile by configure.
   They're here only for reference.  */

/* Define LISP_FLOAT_TYPE if you want XEmacs to support floating-point
   numbers. */
#define LISP_FLOAT_TYPE 1

/* Define GNU_MALLOC if you want to use the *new* GNU memory allocator. */
#define GNU_MALLOC 1

/* Define USE_SYSTEM_MALLOC if you forcing the use of it. */
/* #undef USE_SYSTEM_MALLOC */

/* Define HAVE_TTY if you want TTY support compiled in. */
#define HAVE_TTY 1

/* Define HAVE_X_WINDOWS if you want to use the X window system.  */
#define HAVE_X_WINDOWS 1

/* Define HAVE_NEXTSTEP if you want to use the NeXTstep window system.  */
/* #undef HAVE_NEXTSTEP */

/* Define HAVE_WINDOW_SYSTEM if any windowing system is available.  */
#if defined (HAVE_X_WINDOWS) || defined (HAVE_NEXTSTEP)
#define HAVE_WINDOW_SYSTEM
#endif

/* Define HAVE_UNIXOID_EVENT_LOOP if we use select() to wait for events.  */
#if defined (HAVE_X_WINDOWS) || defined (HAVE_TTY)
#define HAVE_UNIXOID_EVENT_LOOP
#endif

/* Define this if you're using XFree386.  */
#define HAVE_XFREE386 1

/* #undef THIS_IS_X11R4 */
/* #undef THIS_IS_X11R5 */
#define THIS_IS_X11R6 1

/* Define USER_FULL_NAME to return a string
   that is the user's full name.
   It can assume that the variable `pw'
   points to the password file entry for this user.

   At some sites, the pw_gecos field contains
   the user's full name.  If neither this nor any other
   field contains the right thing, use pw_name,
   giving the user's login name, since that is better than nothing.  */
#define USER_FULL_NAME pw->pw_gecos

/* Define AMPERSAND_FULL_NAME if you use the convention
   that & in the full name stands for the login id.  */
/* #undef AMPERSAND_FULL_NAME */

/* Some things figured out by the configure script, grouped as they are in
   configure.in.  */
/* #undef HAVE_MACH_MACH_H */
/* #undef HAVE_SYS_STROPTS_H */
#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_UTIME_H 1
#define HAVE_SYS_WAIT_H 1
/* #undef HAVE_LIBGEN_H */
#define HAVE_LOCALE_H 1
/* #undef HAVE_X11_LOCALE_H */
#define HAVE_LINUX_VERSION_H 1
#define STDC_HEADERS 1
#define TIME_WITH_SYS_TIME 1
/* #undef WORDS_BIGENDIAN */

#define HAVE_LONG_FILE_NAMES 1

#ifdef HAVE_LONG_FILE_NAMES
#define CLASH_DETECTION
#endif

/* #undef HAVE_LIBDNET */
/* #undef HAVE_LIBRESOLV */

/* Define if `sys_siglist' is declared by <signal.h>. */
#define SYS_SIGLIST_DECLARED 1

/* Define if `struct utimbuf' is declared by <utime.h>.  */
#define HAVE_STRUCT_UTIMBUF 1

/* Define if `struct timeval' is declared by <sys/time.h>.  */
#define HAVE_TIMEVAL 1


/* #undef TM_IN_SYS_TIME */
/* #undef HAVE_TM_ZONE */
#define HAVE_TZNAME 1

/* Define if netdb.h declares h_errno.  */
#define HAVE_H_ERRNO 1

/* Define if localtime caches TZ */
#define LOCALTIME_CACHE 1

/* Define if gettimeofday can't accept two arguments */
/* #undef GETTIMEOFDAY_ONE_ARGUMENT */

/* Define in keyword `inline' exists. */
#define HAVE_INLINE 1

#define HAVE_ALLOCA_H 1
/* #undef HAVE_VFORK_H */
/* #undef vfork */

#define HAVE_MMAP 1
#define HAVE_STRCOLL 1

#define SIZEOF_SHORT 2
#define SIZEOF_INT 4
#define SIZEOF_LONG 4

#define HAVE_ACOSH 1
#define HAVE_ASINH 1
#define HAVE_ATANH 1

#if defined (HAVE_ACOSH) && defined (HAVE_ASINH) && defined (HAVE_ATANH)
#define HAVE_INVERSE_HYPERBOLIC
#endif

#define HAVE_CBRT 1
#define HAVE_CLOSEDIR 1
#define HAVE_DUP2 1
/* #undef HAVE_EACCESS */
#define HAVE_FMOD 1
#define HAVE_FPATHCONF 1
#define HAVE_FREXP 1
#define HAVE_FTIME 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETPAGESIZE 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GETWD 1
/* #undef HAVE_LOGB */
#define HAVE_LRAND48 1
/* #undef HAVE_MATHERR */
#define HAVE_MKDIR 1
#define HAVE_MKTIME 1
#define HAVE_PERROR 1
/* #undef HAVE_POLL */
#define HAVE_RANDOM 1
#define HAVE_REALPATH 1
#define HAVE_RENAME 1
#define HAVE_RES_INIT 1
#define HAVE_RINT 1
#define HAVE_RMDIR 1
#define HAVE_SELECT 1
#define HAVE_SETITIMER 1
#define HAVE_SETPGID 1
#define HAVE_SETSID 1
#define HAVE_SIGBLOCK 1
/* #undef HAVE_SIGHOLD */
#define HAVE_SIGPROCMASK 1
#define HAVE_SIGSETJMP 1
#define HAVE_STRCASECMP 1
#define HAVE_STRERROR 1
#define HAVE_TZSET 1
#define HAVE_UTIMES 1
#define HAVE_WAITPID 1

#define HAVE_SOCKETS 1
/* #undef HAVE_SOCKADDR_SUN_LEN */
/* #undef HAVE_SYSVIPC */

#define SYSV_SYSTEM_DIR 1
/* #undef NONSYSTEM_DIR_LIBRARY */

#define HAVE_TERMIOS 1
/* #undef HAVE_TERMIO */

#define NLIST_STRUCT 1

#define UNEXEC_SRC unexelf.c
/* #undef AIX_SMT_EXP */

/* Define HAVE_SOCKS if you have the `socks' library and want XEmacs to
   use it.  */
/* #undef HAVE_SOCKS */

/* Define HAVE_TERM if you run the `term' program (e.g. under Linux) and
   want XEmacs to use it.  */
/* #undef HAVE_TERM */

/* Define HAVE_XPM if you have the `xpm' library and want XEmacs to use it. */
#define HAVE_XPM 1

/* Define HAVE_XFACE if you have the `compface' library and want to use it.
   This will permit X-face pixmaps in mail and news messages to display
   quickly. */
#define HAVE_XFACE 1

/* Define HAVE_GIF if you want XEmacs to support converting GIF
   (Graphics Interchange Format) images. */
#define HAVE_GIF 1

/* Define HAVE_JPEG if you have the JPEG library and want XEmacs to use it.
   This is for converting JPEG images. */
#define HAVE_JPEG 1

/* Define HAVE_PNG if you have the PNG library and want XEmacs to use it.
   This is for converting PNG images. */
#define HAVE_PNG 1

/* Define HAVE_PNG_GNUZ if you want to use -lgz instead of -lz for PNG. */
/* #undef HAVE_PNG_GNUZ */

/* Define HAVE_TIFF if you have the TIFF library and want XEmacs to use it.
   This is for converting TIFF images. */
/* #undef HAVE_TIFF */

/* Define HAVE_XMU if you have the Xmu library.  This should always be
   the case except on losing HPUX systems. */
#define HAVE_XMU 1

/* Define HAVE_DBM if you want to use the DBM libraries */
#define HAVE_DBM 1

/* Define HAVE_GNU_DBM if you want to use the GNU DBM libraries;
   if you define this, you should also define HAVE_DBM */
/* #undef HAVE_GNU_DBM */

/* Define HAVE_BERKELEY_DB if you want to use the BerkDB libraries */
/* #undef HAVE_BERKELEY_DB */

/* Define HAVE_LIBGDBM if you have -lgdbm (separated from HAVE_DBM
   stuff because FreeBSD has the DBM routines in libc) */
/* #undef HAVE_LIBGDBM */

/* Define HAVE_LIBDBM if you have -ldbm */
/* #undef HAVE_LIBDBM */

/* Define HAVE_LIBDB if you have -ldb */
/* #undef HAVE_LIBDB */

#if defined (HAVE_DBM) || defined (HAVE_BERKELEY_DB)
# define HAVE_DATABASE
#endif

/* Define HAVE_XAUTH if the Xauth library is present.  This will add
   some extra functionality to gnuserv. */
#define HAVE_XAUTH 1

/* Define HAVE_XLOCALE_H if X11/Xlocale.h is present. */
#define HAVE_XLOCALE_H 1

/* Define HAVE_NCURSES if -lncurses is present. */
/* #undef HAVE_NCURSES */

/* Define HAVE_NCURSES_CURSES_H if ncurses/curses.h is present. */
/* #undef HAVE_NCURSES_CURSES_H */

/* Define HAVE_NCURSES_TERM_H if ncurses/term.h is present. */
/* #undef HAVE_NCURSES_TERM_H */

/* Define EPOCH to include extra functionality that was present in Epoch.
   This code has received only limited testing. */
/* #undef EPOCH */

#define LOWTAGS

/* Define USE_ASSERTIONS if you want the abort() to be changed to assert()
   If the assertion fails, assert_failed() will be called.  This is
   recommended for general use because it gives more info about the crash
   than just the abort() message.  Too many people "Can't find the corefile"
   or have limited core dumps out of existence. */
#define USE_ASSERTIONS 1

/* Define one or more of the following if you want lots of extra checks
   (e.g. structure validation) compiled in.  These should be turned
   on during the beta-test cycle. */

/* Check the entire extent structure of a buffer each time an extent
   change is done, and do other extent-related checks. */
/* #undef ERROR_CHECK_EXTENTS */
/* Make sure that all X... macros are dereferencing the correct type,
   and that all XSET... macros (as much as possible) are setting the
   correct type of structure.  Highly recommended for all
   development work. */
/* #undef ERROR_CHECK_TYPECHECK */
/* Make sure valid buffer positions are passed to BUF_* macros. */
/* #undef ERROR_CHECK_BUFPOS */
/* Attempt to catch bugs related to garbage collection (e.g.
   insufficient GCPRO'ing). */
/* #undef ERROR_CHECK_GC */
/* Attempt to catch freeing of a non-malloc()ed block, heap corruption,
   etc. */
/* #undef ERROR_CHECK_MALLOC */

/* Define DEBUG_XEMACS if you want extra debugging code compiled in.
   This is mainly intended for use by developers. */
/* #undef DEBUG_XEMACS */

/* Define MEMORY_USAGE_STATS if you want extra code compiled in to
   determine where XEmacs's memory is going. */
/* #undef MEMORY_USAGE_STATS */

/* Define QUANTIFY if using Quantify from Pure Software.  This adds
   some additional calls to control data collection.  This is only
   intended for use by the developers. */
/* #undef QUANTIFY */

/* Define EXTERNAL_WIDGET to compile support for using the editor as a
   widget in another program. */
/* #undef EXTERNAL_WIDGET */

/* There are some special-case defines for gcc and lcc. */
#define USE_GCC 1
/* #undef USE_LCC */

/* Allow the user to override the default value of PURESIZE at configure
   time.  This must come before we include the sys files in order for
   it to be able to override any changes in them. */
/* #undef PURESIZE */


/* Define this if you want to use the Common Desktop Environment
*/
/* #undef HAVE_CDE */

/* Mocklisp Support. */
/* #undef MOCKLISP_SUPPORT */

/* enable special GNU Make features in the Makefiles. */
/* #undef USE_GNU_MAKE */

/* Undocumented debugging option: Don't automatically rebuild the DOC
   file.  This saves a lot of time when you're repeatedly
   compiling-running-crashing. */
/* #undef NO_DOC_FILE */

  /* To eliminate use of `const' in the XEmacs sources,
     do `#define CONST_IS_LOSING' */
#define CONST_IS_LOSING 1

/* # undef CONST */
# ifdef CONST_IS_LOSING
#  define CONST
# else
#  define CONST const
# endif /* CONST */

/* If not defined, use unions instead of ints.  A few systems (DEC Alpha)
   seem to require this, probably because something with the int
   definitions isn't right with 64-bit systems.

   (It's NO_UNION_TYPE instead of USE_UNION_TYPE for historical reasons.)
*/
#define NO_UNION_TYPE 1

/* The configuration script defines opsysfile to be the name of the
   s/...h file that describes the system type you are using.  The file
   is chosen based on the configuration name you give.

   See the file ../etc/MACHINES for a list of systems and the
   configuration names to use for them.

   See s/template.h for documentation on writing s/...h files.  */
#define config_opsysfile "s/linux.h" 
#include config_opsysfile

/* The configuration script defines machfile to be the name of the
   m/...h file that describes the machine you are using.  The file is
   chosen based on the configuration name you give.

   See the file ../etc/MACHINES for a list of machines and the
   configuration names to use for them.

   See m/template.h for documentation on writing m/...h files.  */
#define config_machfile "m/intel386.h"
#include config_machfile

#if defined (USE_SYSTEM_MALLOC) && !defined (SYSTEM_MALLOC)
#define SYSTEM_MALLOC
#endif

#if 0 /* RMSmacs */
/* These typedefs shouldn't appear when alloca.s or Makefile.in
   includes config.h.  */
#ifndef NOT_C_CODE
#ifndef SPECIAL_EMACS_INT
typedef long EMACS_INT;
typedef unsigned long EMACS_UINT;
#endif
#endif
#endif

/* The configuration name.  This is used as the install directory name
   for the lib-src programs. */
#define EMACS_CONFIGURATION "i486-unknown-linux2.0.0"

/* Load in the conversion definitions if this system
   needs them and the source file being compiled has not
   said to inhibit this.  There should be no need for you
   to alter these lines.  */

#ifdef SHORTNAMES
#ifndef NO_SHORTNAMES
#include "../shortnames/remap.h"
#endif /* not NO_SHORTNAMES */
#endif /* SHORTNAMES */

/* Define REL_ALLOC if you want to use the relocating allocator for
   buffer space. */
#define REL_ALLOC 1

/* Define LD_SWITCH_SITE to contain any special flags your loader may need.  */
#define LD_SWITCH_SITE  -L/usr/i486-linuxaout -L/lib-aout -L/m/xpm-3.4h/lib

/* Define C_SWITCH_SITE to contain any special flags your compiler needs.  */
#define C_SWITCH_SITE  -I/usr/i486-linuxaout/include -I/m/xpm-3.4h/lib

/* Define LD_SWITCH_X_SITE to contain any special flags your loader
   may need to deal with X Windows.  For instance, if you've defined
   HAVE_X_WINDOWS above and your X libraries aren't in a place that
   your loader can find on its own, you might want to add "-L/..." or
   something similar.  */
#define LD_SWITCH_X_SITE -L/usr/X11R6/lib

/* Define LD_SWITCH_X_SITE_AUX with an -R option
   in case it's needed (for Solaris, for example).  */
#define LD_SWITCH_X_SITE_AUX 

/* Define C_SWITCH_X_SITE to contain any special flags your compiler
   may need to deal with X Windows.  For instance, if you've defined
   HAVE_X_WINDOWS above and your X include files aren't in a place
   that your compiler can find on its own, you might want to add
   "-I/..." or something similar.  */
#define C_SWITCH_X_SITE -I/usr/X11R6/include

/* Define the return type of signal handlers if the s-xxx file
   did not already do so.  */
#define RETSIGTYPE void

/* SIGTYPE is the macro we actually use.  */
#ifndef SIGTYPE
#define SIGTYPE RETSIGTYPE
#define SIGRETURN return
#endif

/* Define DYNODUMP if it is necessary to properly dump on this system.
   Currently this is only Solaris. */
/* #undef DYNODUMP */

/* Define NEED_XILDOFF if the -xildoff flag must be passed to cc to
   avoid invoking the incremental linker ild which is incompatible
   with dynodump.  This is needed for recent Sunsoft compilers. */
/* #undef NEED_XILDOFF */

/* Define ENERGIZE to compile with support for the Energize Programming System.
   If you do this, don't forget to define ENERGIZE in lwlib/Imakefile as well.
   You will need to set your C_SWITCH_SITE and LD_SWITCH_SITE to point at the
   Energize connection library (libconn.a) and associated header files.
 */
/* #undef ENERGIZE */
/* #undef ENERGIZE_2 */
/* #undef ENERGIZE_3 */

/* Define SUNPRO to compiled in support for Sun Sparcworks. */
/* #undef SUNPRO */

/* Sun SparcStations, SGI machines, and HP9000s700s have support for playing
   different sound files as beeps.  If you are on a SparcStation but do not 
   have the sound option installed for some reason, then undefine
   HAVE_NATIVE_SOUND.  (It's usually found in /usr/demo/SOUND/ on SunOS 4
   and Solaris systems; on Solaris, you may need to install the "SUNWaudmo"
   package.)
 */
#define HAVE_NATIVE_SOUND 1

/* If you wish to compile with support for the Network Audio System
   system define HAVE_NAS_SOUND.
   NAS_NO_ERROR_JUMP means that the NAS libraries don't inlcude some
   error handling changes.
 */
/* #undef HAVE_NAS_SOUND */
/* #undef NAS_NO_ERROR_JUMP */

/* Compile in support for SunPro usage-tracking code. */
/* #undef USAGE_TRACKING */

/* Define TOOLTALK if your site supports the ToolTalk library. */
/* #undef TOOLTALK */

/* #undef LWLIB_USES_MOTIF */
#define LWLIB_MENUBARS_LUCID 1
/* #undef LWLIB_MENUBARS_MOTIF */
#define LWLIB_SCROLLBARS_LUCID 1
/* #undef LWLIB_SCROLLBARS_MOTIF */
/* #undef LWLIB_SCROLLBARS_ATHENA */
/* #undef LWLIB_DIALOGS_MOTIF */
#define LWLIB_DIALOGS_ATHENA 1

/* Other things that can be disabled by configure. */
#define HAVE_MENUBARS 1
#define HAVE_SCROLLBARS 1
#define HAVE_DIALOGS 1
#define HAVE_TOOLBARS 1


#if defined (HAVE_MENUBARS) || defined (HAVE_DIALOGS)
#define HAVE_POPUPS
#endif

/* If you are using SunOS 4.1.1 and X11r5, then you need this patch.
   There is a stupid bug in the SunOS libc.a: two functions which X11r5
   uses, mbstowcs() and wcstombs(), are unusable when programs are
   statically linked (as XEmacs must be) because the static version of
   libc.a contains the *dynamic* versions of these functions.  These
   functions don't seem to be called when XEmacs is running, so it's 
   enough to define stubs for them.

   This appears to be fixed in SunOS 4.1.2.

   Also, SunOS 4.1.1 contains buggy versions of strcmp and strcpy that
   sometimes reference memory past the end of the string, which can segv.
   I don't know whether this is has been fixed as of 4.1.2 or 4.1.3.
 */
#if defined (sparc) && !defined (USG)
#define OBJECTS_SYSTEM sunOS-fix.o strcmp.o strcpy.o
#endif

/* If you turn this flag on, it forces encapsulation in all
circumstances; this can be used to make sure things compile OK
on various systems. */
#define DEBUG_ENCAPSULATION

/* basic system calls */

#if defined (INTERRUPTIBLE_IO) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_READ
# define ENCAPSULATE_WRITE
#endif
#if defined (INTERRUPTIBLE_OPEN) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_OPEN
#endif
#if defined (INTERRUPTIBLE_CLOSE) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_CLOSE
#endif

/* stdio calls */

#if defined (INTERRUPTIBLE_IO) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_FREAD
# define ENCAPSULATE_FWRITE
#endif
#if defined (INTERRUPTIBLE_OPEN) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_FOPEN
#endif
#if defined (INTERRUPTIBLE_CLOSE) || defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_FCLOSE
#endif

/* directory calls */

#if defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_CHDIR
# define ENCAPSULATE_MKDIR
# define ENCAPSULATE_OPENDIR
# define ENCAPSULATE_READDIR
# define ENCAPSULATE_RMDIR
#endif

/* file-information calls */

#if defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_ACCESS
# define ENCAPSULATE_LSTAT
# define ENCAPSULATE_READLINK
# define ENCAPSULATE_STAT
#endif

/* file-manipulation calls */

#if defined (DEBUG_ENCAPSULATION)
# define ENCAPSULATE_CHMOD
# define ENCAPSULATE_CREAT
# define ENCAPSULATE_LINK
# define ENCAPSULATE_RENAME
# define ENCAPSULATE_SYMLINK
# define ENCAPSULATE_UNLINK
#endif

#if (defined (MSDOS) && defined (FEPCTRL)) || (defined (WIN32) && defined (USE_IME))
#define HAVE_FEP
#endif

#if defined (HAVE_SOCKS) && !defined (DO_NOT_SOCKSIFY)
#define accept Raccept
#define bind Rbind
#define connect Rconnect
#define getsockname Rgetsockname
#define listen Rlisten
#endif /* HAVE_SOCKS && !DO_NOT_SOCKSIFY */

#ifndef SHORTBITS
#define SHORTBITS (8 * SIZEOF_SHORT)
#endif
#ifndef INTBITS
#define INTBITS (8 * SIZEOF_INT)
#endif
#ifndef LONGBITS
#define LONGBITS (8 * SIZEOF_LONG)
#endif

#ifdef HAVE_INLINE
# if defined (__GNUC__)
#  if defined (DONT_EXTERN_INLINE_FUNCTIONS)
#   define INLINE inline
#  else
#   define INLINE extern inline
#  endif
# else
#  define INLINE static inline
# endif
#else
# define INLINE static
#endif

/* We want to avoid saving the signal mask if possible, because
   that necessitates a system call. */
#ifdef HAVE_SIGSETJMP
# define SETJMP(x) sigsetjmp (x, 0)
# define LONGJMP(x, y) siglongjmp (x, y)
# define JMP_BUF sigjmp_buf
#else
# define SETJMP(x) setjmp (x)
# define LONGJMP(x, y) longjmp (x, y)
# define JMP_BUF jmp_buf
#endif

#endif /* _CONFIG_H_ */
