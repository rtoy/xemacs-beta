/* Profiling.
   Copyright (C) 2003, 2005 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

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
   `start-profiling'.

   We check do_backtrace to make sure that the backtrace structure is
   initialised. If it isn't, we can enter a function with profiling turned
   off, and exit it with it turned on, with the consequence that an
   unitialised backtrace structure is passed to
   profile_record_just_called. Since do_backtrace is function-local (apart
   from in the garbage collector) this avoids that.  */
#define PROFILE_ENTER_FUNCTION()		\
do						\
{						\
  if (profiling_active && do_backtrace)		\
    profile_record_about_to_call (&backtrace);	\
}						\
while (0)

#define PROFILE_EXIT_FUNCTION()			\
do						\
{						\
  if (profiling_active && do_backtrace)		\
    profile_record_just_called (&backtrace);	\
}						\
while (0)

/* We are entering a section that we would like to record profile information
   about.  We put this information into the backtrace list, just like
   normal functions do.  That is one easy way to make sure that we always
   record info on the innermost section or function, whether section or
   function. (To do this, we always need some sort of collusion between
   profile and eval; this is one way.) */

/* Or, we could call xzero() to zero the whole thing, and avoid four
   of the statements below; or we could create a global backtrace object,
   uninitialized (i.e. it will be initialized to all 0), and do structure
   copy to initialize.  It's not clear it will make much difference here,
   but someone who really cared about counting cycles could implement it. */
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

#define RETURN_EXIT_PROFILING(tag, type, expr)	\
do						\
{						\
  type _ret_exitpr_ = (expr);			\
  PROFILE_RECORD_EXITING_SECTION (tag);		\
  RETURN_SANS_WARNINGS _ret_exitpr_;		\
} while (0)

#define RETURN_LISP_EXIT_PROFILING(tag, expr)		\
  RETURN_EXIT_PROFILING (tag, Lisp_Object, expr)
  
#define RETURN_UNGCPRO_EXIT_PROFILING(tag, expr)	\
do							\
{							\
  Lisp_Object ret_ungc_val = (expr);			\
  UNGCPRO;						\
  PROFILE_RECORD_EXITING_SECTION (tag);			\
  RETURN_SANS_WARNINGS ret_ungc_val;			\
} while (0)

#ifdef DEBUG_XEMACS
extern Lisp_Object QSin_temp_spot_1;
extern Lisp_Object QSin_temp_spot_2;
extern Lisp_Object QSin_temp_spot_3;
extern Lisp_Object QSin_temp_spot_4;
extern Lisp_Object QSin_temp_spot_5;
#endif /* DEBUG_XEMACS */
