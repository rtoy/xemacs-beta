/* Profiling.
   Copyright (C) 2003 Ben Wing.

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

/* Synched up with: Not in FSF. */

/* Authorship:

   Ben Wing: Feb 2003.
 */

#include "backtrace.h"

void mark_profiling_info (void);
void profile_record_unwind (struct backtrace *);
void profile_record_about_to_call (struct backtrace *);
void profile_record_just_called (struct backtrace *);
void profile_record_consing (EMACS_INT size);
void profile_record_unconsing (EMACS_INT size);
extern int profiling_active;

/* We call about_to_call() and just_called() depending on the current
   *dynamic* value of profiling_active (which could change as a result of
   calling the function) but if we push a backtrace, we must pop it later,
   so we need to remember the status of this. */
#define PROFILE_DECLARE()						 \
int do_backtrace = profiling_active || backtrace_with_internal_sections; \
struct backtrace backtrace

/* As just mentioned, we rely on the dynamic value of profiling_active.
   This ensures correct behavior (e.g. we never modify the profiling info
   when profiling is not active) because we seed and reap all functions
   currently on the stack when starting and stopping.  See
   `start-profiling'. */
#define PROFILE_ENTER_FUNCTION()		\
do						\
{						\
  if (profiling_active)				\
    profile_record_about_to_call (&backtrace);	\
}						\
while (0)

#define PROFILE_EXIT_FUNCTION()			\
do						\
{						\
  if (profiling_active)				\
    profile_record_just_called (&backtrace);	\
}						\
while (0)

/* We are entering a section that we would like to record profile information
   about.  We put this information into the backtrace list, just like
   normal functions do.  That is one easy way to make sure that we always
   record info on the innermost section or function, whether section or
   function. (To do this, we always need some sort of collusion between
   profile and eval; this is one way.) */

#define PROFILE_RECORD_ENTERING_SECTION(var)		\
do							\
{							\
  if (do_backtrace)					\
    {							\
      backtrace.function = &var;			\
      backtrace.args = NULL;				\
      backtrace.nargs = UNEVALLED;			\
      backtrace.evalargs = 0;				\
      backtrace.pdlcount = specpdl_depth ();		\
      backtrace.debug_on_exit = 0;			\
      backtrace.function_being_called = 0;		\
      PUSH_BACKTRACE (backtrace);			\
    }							\
  PROFILE_ENTER_FUNCTION ();				\
} while (0)

#define PROFILE_RECORD_EXITING_SECTION(var)		\
do							\
{							\
  PROFILE_EXIT_FUNCTION ();				\
  if (do_backtrace)					\
    POP_BACKTRACE (backtrace);				\
} while (0)
