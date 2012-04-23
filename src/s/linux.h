/* This file is the configuration file for the GNU/Linux operating system,
   prior to version 1.1.56.
   Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

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

/* Synched up with: FSF 19.31 (called, ahem ... lignux.h in FSF). */

/* This file was put together by Michael K. Johnson and Rik Faith.  */

#define USG
#define LINUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "linux"		/* All the best software is free. */



/* Deleted GNU_LIBRARY_PENDING_OUTPUT_COUNT -- unused in XEmacs */

/* This is needed for sysdep.c */

#define HAVE_SYS_SIGLIST

/* #define POSIX -- not used in XEmacs */

/* Deleted TERM stuff -- probably hugely obsolete */

#define UNEXEC "unexelf.o"
#define UNEXEC_USE_MAP_PRIVATE

/* mrb - Ordinary link is simple and effective */
/* slb - Not any more ... :-( */
/* jwj - slb's comment does not seem relevant to current Linuxes */
#define ORDINARY_LINK

#define A_TEXT_OFFSET(hdr) (N_MAGIC(hdr) == QMAGIC ? sizeof (struct exec) : 0)
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#define ADJUST_EXEC_HEADER \
  unexec_text_start = N_TXTADDR(ohdr) + A_TEXT_OFFSET(ohdr)

/* HAVE_XRMSETDATABASE deleted */

/* The regex.o routines are a part of the GNU C-library used with Linux.  */
/* However, sometimes they disagree with the src/regex.h that comes with Emacs,
   and that can make trouble in etags.c because it gets the regex.h from Emacs
   and the function definitions in libc.  So turn this off.  */
/* XEmacs: in any case, Mule uses different regex routines. */
/* #define REGEXP_IN_LIBC */

/* XEmacs change: the standard linux libc includes regex routines in
   it.  We have to use our own and have to avoid name conflicts. */

#if 0 /* Probably not necessary any more */
#define re_compile_pattern sys_re_compile_pattern
#define re_search sys_re_search
#define re_search_2 sys_re_search_2
#define re_match_2 sys_re_match_2
#define re_max_failures sys_re_max_failures
#define re_set_syntax sys_re_set_syntax
#define re_set_registers sys_re_set_registers
#define re_compile_fastmap sys_re_compile_fastmap
#define re_match sys_re_match
#define regcomp sys_regcomp
#define regexec sys_regexec
#define regerror sys_regerror
#define regfree sys_regfree
#endif /* 0 */

/* The in-built malloc does not work on PPC64 or Alpha, so use the system
   malloc for now. */
#if defined(__powerpc64__) || defined(__alpha__) || defined(__ia64__)
#define SYSTEM_MALLOC
#endif
