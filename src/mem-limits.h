/* Includes for memory limit warnings.
   Copyright (C) 1990, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.30. */

/* Subsequently cleaned up and reorganised, made to use configure. */ 

#ifndef INCLUDED_mem_limits_h_
#define INCLUDED_mem_limits_h_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_ULIMIT_H
#include <ulimit.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
/* Some systems need either sys/types.h or sys/time.h before <sys/resource.h>.  */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef HAVE_SYS_VLIMIT_H
#include <sys/vlimit.h>
#endif

#ifdef _LIBC

/* Old Linux startup code won't define __data_start.  */
extern int etext, __data_start; weak_symbol (__data_start)
#define start_of_data()	(&__data_start ?: &etext)

#else /* not GNU libc */

#ifdef emacs
typedef void *POINTER;

#ifndef CYGWIN
typedef unsigned long SIZE;
#endif

extern POINTER start_of_data (void);
#define EXCEEDS_LISP_PTR(ptr) 0

#ifdef BSD
extern int etext;
#define start_of_data() &etext
#endif

#else  /* not emacs */
extern char etext;
#define start_of_data() &etext
#endif /* not emacs */

#endif /* not _LIBC */



/* start of data space; can be changed by calling malloc_init */
static POINTER data_space_start;

/* Number of bytes of writable memory we can expect to be able to get */
extern unsigned long lim_data;

/* The implementation of get_lim_data() is very machine dependent. */

#if defined (HEAP_IN_DATA) && !defined(PDUMP)
extern unsigned long static_heap_size;
extern MODULE_API int initialized;

static void
get_lim_data (void)
{
  if (!initialized)
    {
      lim_data = (unsigned long) -1; /* static_heap_size; */
    }
  else
    {
      lim_data = (unsigned long) -1;
    }
}

#elif defined(NO_LIM_DATA)

static void
get_lim_data (void)
{
  lim_data = (unsigned long) -1;
}

#elif defined(HAVE_GETRLIMIT)

static void
get_lim_data (void)
{
  struct rlimit XXrlimit;

  getrlimit (RLIMIT_DATA, &XXrlimit);
#ifdef RLIM_INFINITY
  lim_data = XXrlimit.rlim_cur & RLIM_INFINITY; /* soft limit */
#else
  lim_data = XXrlimit.rlim_cur;	/* soft limit */
#endif
}

#elif defined(HAVE_ULIMIT)

static void
get_lim_data (void)
{
  lim_data = (unsigned long) -1;

  /* Use the ulimit call, if we seem to have it.  */
#if !defined (ULIMIT_BREAK_VALUE)
  lim_data = ulimit (3, 0);
#endif

  /* If that didn't work, just use the macro's value.  */
#ifdef ULIMIT_BREAK_VALUE
  if (lim_data == (unsigned long) -1)
    lim_data = ULIMIT_BREAK_VALUE;
#endif

  lim_data -= (long) data_space_start;
}

#elif defined(WIN32_NATIVE)

static void
get_lim_data (void)
{
  extern unsigned long data_region_size;
  lim_data = data_region_size;
}

#elif defined(HAVE_VLIMIT)

static void
get_lim_data (void)
{
  lim_data = vlimit (LIM_DATA, -1);
}

#else
#error Cannot determine an implementation of get_lim_data().
#endif /* not HAVE_VLIMIT */

#endif /* INCLUDED_mem_limits_h_ */
