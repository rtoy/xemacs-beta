/* Why the hell is XEmacs so fucking slow?
   Copyright (C) 1996, 2002 Ben Wing.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "bytecode.h"
#include "elhash.h"
#include "hash.h"

#include "syssignal.h"
#include "systime.h"

#ifndef HAVE_SETITIMER
#error Sorry charlie.  We need a scalpel and all we have is a lawnmower.
#endif

/* We implement our own profiling scheme so that we can determine
   things like which Lisp functions are occupying the most time.  Any
   standard OS-provided profiling works on C functions, which is
   somewhat useless.

   The basic idea is simple.  We set a profiling timer using setitimer
   (ITIMER_PROF), which generates a SIGPROF every so often.  (This
   runs not in real time but rather when the process is executing or
   the system is running on behalf of the process.) When the signal
   goes off, we see what we're in, and add 1 to the count associated
   with that function.

   It would be nice to use the Lisp allocation mechanism etc. to keep
   track of the profiling information, but we can't because that's not
   safe, and trying to make it safe would be much more work than it's
   worth.

   Jan 1998: In addition to this, I have added code to remember call
   counts of Lisp funcalls.  The profile_increase_call_count()
   function is called from Ffuncall(), and serves to add data to
   Vcall_count_profile_table.  This mechanism is much simpler and
   independent of the SIGPROF-driven one.  It uses the Lisp allocation
   mechanism normally, since it is not called from a handler.  It may
   even be useful to provide a way to turn on only one profiling
   mechanism, but I haven't done so yet.  --hniksic */

static struct hash_table *big_profile_table;
Lisp_Object Vcall_count_profile_table;

Fixnum default_profiling_interval;

int profiling_active;

/* The normal flag in_display is used as a critical-section flag
   and is not set the whole time we're in redisplay. */
int profiling_redisplay_flag;

static Lisp_Object QSin_redisplay;
static Lisp_Object QSin_garbage_collection;
static Lisp_Object QSprocessing_events_at_top_level;
static Lisp_Object QSunknown;

static Lisp_Object Qtiming, Qcall_count;

/* We use inside_profiling to prevent the handler from writing to
   the table while another routine is operating on it.  We also set
   inside_profiling in case the timeout between signal calls is short
   enough to catch us while we're already in there. */
static volatile int inside_profiling;

static void
create_call_count_profile_table (void)
{
  if (NILP (Vcall_count_profile_table))
    Vcall_count_profile_table =
      make_lisp_hash_table (100, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
}

static void
create_timing_profile_table (void)
{
  /* #### The hash code can safely be called from a signal handler
     except when it has to grow the hash table.  In this case, it calls
     realloc(), which is not (in general) re-entrant.  We'll just be
     sleazy and make the table large enough that it (hopefully) won't
     need to be realloc()ed. */
  if (!big_profile_table)
    big_profile_table = make_hash_table (10000);
}

/* Increase the value of OBJ in Vcall_count_profile_table hash table.
   If the hash table is nil, create it first.  */
void
profile_increase_call_count (Lisp_Object obj)
{
  Lisp_Object count;

  create_call_count_profile_table ();

  count = Fgethash (obj, Vcall_count_profile_table, Qzero);
  if (!INTP (count))
    count = Qzero;
  Fputhash (obj, make_int (1 + XINT (count)), Vcall_count_profile_table);
}

static SIGTYPE
sigprof_handler (int signo)
{
  /* Don't do anything if we are shutting down, or are doing a maphash
     or clrhash on the table. */
  if (!inside_profiling && !preparing_for_armageddon)
    {
      Lisp_Object fun;

      /* If something below causes an error to be signaled, we'll
	 not correctly reset this flag.  But we'll be in worse shape
	 than that anyways, since we'll longjmp back to the last
	 condition case. */
      inside_profiling = 1;

      if (profiling_redisplay_flag)
	fun = QSin_redisplay;
      else if (gc_in_progress)
	fun = QSin_garbage_collection;
      else if (backtrace_list)
	{
	  fun = *backtrace_list->function;

	  if (!SYMBOLP (fun)
	      && !COMPILED_FUNCTIONP (fun)
	      && !SUBRP (fun)
	      && !CONSP (fun))
	     fun = QSunknown;
	}
      else
	fun = QSprocessing_events_at_top_level;

      {
	/* #### see comment about memory allocation in start-profiling.
	   Allocating memory in a signal handler is BAD BAD BAD.
	   If you are using the non-mmap rel-alloc code, you might
	   lose because of this.  Even worse, if the memory allocation
	   fails, the `error' generated whacks everything hard. */
	long count;
	const void *vval;

	if (gethash (LISP_TO_VOID (fun), big_profile_table, &vval))
	  count = (long) vval;
	else
	  count = 0;
	count++;
	vval = (const void *) count;
	puthash (LISP_TO_VOID (fun), (void *) vval, big_profile_table);
      }

      inside_profiling = 0;
    }
}

DEFUN ("start-profiling", Fstart_profiling, 0, 1, 0, /*
Start profiling, with profile queries every MICROSECS.
If MICROSECS is nil or omitted, the value of `default-profiling-interval'
is used.

Information on function timings and call counts is currently recorded.
You can retrieve the recorded profiling info using `get-profiling-info'.

Starting and stopping profiling does not clear the currently recorded
info.  Thus you can start and stop as many times as you want and everything
will be properly accumulated.
*/
       (microsecs))
{
  /* This function can GC */
  int msecs;
  struct itimerval foo;

  /* #### The hash code can safely be called from a signal handler
     except when it has to grow the hash table.  In this case, it calls
     realloc(), which is not (in general) re-entrant.  We'll just be
     sleazy and make the table large enough that it (hopefully) won't
     need to be realloc()ed. */
  create_timing_profile_table ();

  if (NILP (microsecs))
    msecs = default_profiling_interval;
  else
    {
      CHECK_NATNUM (microsecs);
      msecs = XINT (microsecs);
    }
  if (msecs <= 0)
    msecs = 1000;

  set_timeout_signal (SIGPROF, sigprof_handler);
  foo.it_value.tv_sec = 0;
  foo.it_value.tv_usec = msecs;
  EMACS_NORMALIZE_TIME (foo.it_value);
  foo.it_interval = foo.it_value;
  profiling_active = 1;
  inside_profiling = 0;
  qxe_setitimer (ITIMER_PROF, &foo, 0);
  return Qnil;
}

DEFUN ("stop-profiling", Fstop_profiling, 0, 0, 0, /*
Stop profiling.
*/
       ())
{
  /* This function does not GC */
  struct itimerval foo;

  foo.it_value.tv_sec = 0;
  foo.it_value.tv_usec = 0;
  foo.it_interval = foo.it_value;
  qxe_setitimer (ITIMER_PROF, &foo, 0);
  profiling_active = 0;
  set_timeout_signal (SIGPROF, fatal_error_signal);
  return Qnil;
}

DEFUN ("clear-profiling-info", Fclear_profiling_info, 0, 0, "", /*
Clear out the recorded profiling info.
This clears both the internal timing information and the call counts in
`call-count-profile-table'.
*/
       ())
{
  /* This function does not GC */
  if (big_profile_table)
    {
      inside_profiling = 1;
      clrhash (big_profile_table);
      inside_profiling = 0;
    }
  if (!NILP (Vcall_count_profile_table))
    Fclrhash (Vcall_count_profile_table);
  return Qnil;
}

struct get_profiling_info_closure
{
  Lisp_Object timing;
};

static int
get_profiling_info_timing_maphash (const void *void_key,
				   void *void_val,
				   void *void_closure)
{
  /* This function does not GC */
  Lisp_Object key;
  struct get_profiling_info_closure *closure
    = (struct get_profiling_info_closure *) void_closure;
  EMACS_INT val;

  key = VOID_TO_LISP (void_key);
  val = (EMACS_INT) void_val;

  Fputhash (key, make_int (val), closure->timing);
  return 0;
}

DEFUN ("get-profiling-info", Fget_profiling_info, 0, 0, 0, /*
Return the currently recorded profiling info.
The format is a plist of symbols describing type of info recorded and
an associated type-specific entry.  Currently, the following info types
are recorded

`timing'
  A hash table of funcallable objects or strings describing internal processing
  operations \(redisplay, garbage collection, etc.), along with associated
  tick counts (the frequency of ticks is controlled by
  `default-profiling-interval' or the argument to `start-profiling').

`call-count'
  A hash table of funcallable objects and associated call counts.
*/
       ())
{
  /* This function does not GC */
  struct get_profiling_info_closure closure;

  closure.timing =
    make_lisp_hash_table (100, HASH_TABLE_NON_WEAK, HASH_TABLE_EQUAL);

  if (big_profile_table)
    {
      int count = internal_bind_int ((int *) &inside_profiling, 1);
      maphash (get_profiling_info_timing_maphash, big_profile_table, &closure);
      unbind_to (count);
    }

  return list4 (Qtiming, closure.timing, Qcall_count,
		!NILP (Vcall_count_profile_table) ?
		Fcopy_hash_table (Vcall_count_profile_table) :
		make_lisp_hash_table (100, HASH_TABLE_NON_WEAK,
				      HASH_TABLE_EQ));
}

struct set_profiling_info_closure
{
  Lisp_Object timing;
};

static int
set_profiling_info_timing_maphash (Lisp_Object key,
				   Lisp_Object val,
				   void *void_closure)
{
  /* This function does not GC */
  if (!INTP (val))
    invalid_argument_2
      ("Function timing count is not an integer in given entry",
       key, val);

  puthash (LISP_TO_VOID (key), (void *) XINT (val), big_profile_table);

  return 0;
}

DEFUN ("set-profiling-info", Fset_profiling_info, 1, 1, 0, /*
Set the currently recorded profiling info.
INFO should be in the same format returned by `get-profiling-info',
as described there.
*/
       (info))
{
  /* This function does not GC */
  Fclear_profiling_info ();

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, info)
      {
	if (EQ (key, Qtiming))
	  {
	    CHECK_HASH_TABLE (value);
	    create_timing_profile_table ();
	    elisp_maphash_unsafe (set_profiling_info_timing_maphash, value,
				  NULL);
	  }
	else if (EQ (key, Qcall_count))
	  {
	    Vcall_count_profile_table = Fcopy_hash_table (value);
	  }
	else
	  invalid_constant ("Unrecognized profiling-info keyword", key);
      }
  }

  return Qnil;
}

static int
mark_profiling_info_maphash (const void *void_key,
			     void *void_val,
			     void *void_closure)
{
  Lisp_Object key;

  key = VOID_TO_LISP (void_key);
  mark_object (key);
  return 0;
}

void
mark_profiling_info (void)
{
  /* This function does not GC */
  if (big_profile_table)
    {
      inside_profiling = 1;
      maphash (mark_profiling_info_maphash, big_profile_table, 0);
      inside_profiling = 0;
    }
}

DEFUN ("profiling-active-p", Fprofiling_active_p, 0, 0, 0, /*
Return non-nil if profiling information is currently being recorded.
*/
       ())
{
  return profiling_active ? Qt : Qnil;
}

void
syms_of_profile (void)
{
  DEFSUBR (Fstart_profiling);
  DEFSUBR (Fstop_profiling);
  DEFSUBR (Fget_profiling_info);
  DEFSUBR (Fset_profiling_info);
  DEFSUBR (Fclear_profiling_info);
  DEFSUBR (Fprofiling_active_p);
}

void
vars_of_profile (void)
{
  DEFVAR_INT ("default-profiling-interval", &default_profiling_interval /*
Default CPU time in microseconds between profiling sampling.
Used when the argument to `start-profiling' is nil or omitted.
Note that the time in question is CPU time (when the program is executing
or the kernel is executing on behalf of the program) and not real time, and
there is usually a machine-dependent limit on how small this value can be.
*/ );
  default_profiling_interval = 1000;

  staticpro (&Vcall_count_profile_table);
  Vcall_count_profile_table = Qnil;

  inside_profiling = 0;

  QSin_redisplay = build_msg_string ("(in redisplay)");
  staticpro (&QSin_redisplay);
  QSin_garbage_collection = build_msg_string ("(in garbage collection)");
  staticpro (&QSin_garbage_collection);
  QSunknown = build_msg_string ("(unknown)");
  staticpro (&QSunknown);
  QSprocessing_events_at_top_level =
    build_msg_string ("(processing events at top level)");
  staticpro (&QSprocessing_events_at_top_level);

  DEFSYMBOL (Qtiming);
  DEFSYMBOL (Qcall_count);
}
