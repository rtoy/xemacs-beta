/* Evaluator for XEmacs Lisp interpreter.
   Copyright (C) 1985-1987, 1992-1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

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

/* Synched up with: FSF 19.30 (except for Fsignal), Mule 2.0. */

/* Authorship:

   Based on code from pre-release FSF 19, c. 1991.
   Some work by Richard Mlynarik long ago (c. 1993?) --
     added call-with-condition-handler; synch. up to released FSF 19.7
     for lemacs 19.8.  some signal changes.
   Various work by Ben Wing, 1995-1996:
     added all stuff dealing with trapping errors, suspended-errors, etc.
     added most Fsignal front ends.
     added warning code.
     reworked the Fsignal code and synched the rest up to FSF 19.30.
   Some changes by Martin Buchholz c. 1999?
     e.g. PRIMITIVE_FUNCALL macros.
   New call_trapping_problems code and large comments below
     by Ben Wing, Mar-Apr 2000.
*/

/* This file has been Mule-ized. */

/* What is in this file?

   This file contains the engine for the ELisp interpreter in XEmacs.
   The engine does the actual work of implementing function calls,
   form evaluation, non-local exits (catch, throw, signal,
   condition-case, call-with-condition-handler), unwind-protects,
   dynamic bindings, let constructs, backtraces, etc.  You might say
   that this module is the very heart of XEmacs, and everything else
   in XEmacs is merely an auxiliary module implementing some specific
   functionality that may be called from the heart at an appropriate
   time.

   The only exception is the alloc.c module, which implements the
   framework upon which this module (eval.c) works.  alloc.c works
   with creating the actual Lisp objects themselves and garbage
   collecting them as necessary, presenting a nice, high-level
   interface for object creation, deletion, access, and modification.

   The only other exception that could be cited is the event-handling
   module in event-stream.c.  From its perspective, it is also the
   heart of XEmacs, and controls exactly what gets done at what time.
   From its perspective, eval.c is merely one of the auxiliary modules
   out there that can be invoked by event-stream.c.

   Although the event-stream-centric view is a convenient fiction that
   makes sense particularly from the user's perspective and from the
   perspective of time, the engine-centric view is actually closest to
   the truth, because anywhere within the event-stream module, you are
   still somewhere in a Lisp backtrace, and event-loops are begun by
   functions such as `command-loop-1', a Lisp function.

   As the Lisp engine is doing its thing, it maintains the state of
   the engine primarily in five list-like items, which are:

   -- the backtrace list
   -- the catchtag list
   -- the condition-handler list
   -- the specbind list
   -- the GCPRO list.

   These are described in detail in the next comment.

   --ben
 */

/* Note that there are five separate lists used to maintain state in
   the evaluator.  All of them conceptually are stacks (last-in,
   first-out).  All non-local exits happen ultimately through the
   catch/throw mechanism, which uses one of the five lists (the
   catchtag list) and records the current state of the others in each
   frame of the list (some other information is recorded and restored
   as well, such as the current eval depth), so that all the state of
   the evaluator is restored properly when a non-local exit occurs.
   (Note that the current state of the condition-handler list is not
   recorded in the catchtag list.  Instead, when a condition-case or
   call-with-condition-handler is set up, it installs an
   unwind-protect on the specbind list to restore the appropriate
   setting for the condition-handler list.  During the course of
   handling the non-local exit, all entries on the specbind list that
   are past the location stored in the catch frame are "unwound"
   (i.e. variable bindings are restored and unwind-protects are
   executed), so the condition-handler list gets reset properly.

   The five lists are

   1. The backtrace list, which is chained through `struct backtrace's
      declared in the stack frames of various primitives, and keeps
      track of all Lisp function call entries and exits.
   2. The catchtag list, which is chained through `struct catchtag's
      declared in the stack frames of internal_catch and condition_case_1,
      and keeps track of information needed to reset the internal state
      of the evaluator to the state that was current when the catch or
      condition-case were established, in the event of a non-local exit.
   3. The condition-handler list, which is a simple Lisp list with new
      entries consed onto the front of the list.  It records condition-cases
      and call-with-condition-handlers established either from C or from
      Lisp.  Unlike with the other lists (but similar to everything else
      of a similar nature in the rest of the C and Lisp code), it takes care
      of restoring itself appropriately in the event of a non-local exit
      through the use of the unwind-protect mechanism.
   4. The specbind list, which is a contiguous array of `struct specbinding's,
      expanded as necessary using realloc().  It holds dynamic variable
      bindings (the only kind we currently have in ELisp) and unwind-protects.
   5. The GCPRO list, which is chained through `struct gcpro's declared in
      the stack frames of any functions that need to GC-protect Lisp_Objects
      declared on the stack.  This is one of the most fragile areas of the
      entire scheme -- you must not forget to UNGCPRO at the end of your
      function, you must make sure you GCPRO in many circumstances you don't
      think you have to, etc.  See the internals manual for more information
      about this.

      --ben
*/

#include <config.h>
#include "lisp.h"

#include "commands.h"
#include "backtrace.h"
#include "bytecode.h"
#include "buffer.h"
#include "console-impl.h"
#include "device.h"
#include "frame.h"
#include "lstream.h"
#include "opaque.h"
#include "profile.h"
#include "window.h"

struct backtrace *backtrace_list;

/* Macros for calling subrs with an argument list whose length is only
   known at runtime.  See EXFUN and DEFUN for similar hackery.  */

#define AV_0(av)
#define AV_1(av) av[0]
#define AV_2(av) AV_1(av), av[1]
#define AV_3(av) AV_2(av), av[2]
#define AV_4(av) AV_3(av), av[3]
#define AV_5(av) AV_4(av), av[4]
#define AV_6(av) AV_5(av), av[5]
#define AV_7(av) AV_6(av), av[6]
#define AV_8(av) AV_7(av), av[7]

#define PRIMITIVE_FUNCALL_1(fn, av, ac) \
  (((Lisp_Object (*)(EXFUN_##ac)) (fn)) (AV_##ac (av)))

/* If subrs take more than 8 arguments, more cases need to be added
   to this switch.  (But wait - don't do it - if you really need
   a SUBR with more than 8 arguments, use max_args == MANY.
   Or better, considering using a property list as one of your args.
   See the DEFUN macro in lisp.h)  */
#define PRIMITIVE_FUNCALL(rv, fn, av, ac) do {			\
  void (*PF_fn)(void) = (void (*)(void)) fn;			\
  Lisp_Object *PF_av = (av);					\
  switch (ac)							\
    {								\
    default:rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 0); break;	\
    case 1: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 1); break;	\
    case 2: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 2); break;	\
    case 3: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 3); break;	\
    case 4: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 4); break;	\
    case 5: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 5); break;	\
    case 6: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 6); break;	\
    case 7: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 7); break;	\
    case 8: rv = PRIMITIVE_FUNCALL_1(PF_fn, PF_av, 8); break;	\
    }								\
} while (0)

#define FUNCALL_SUBR(rv, subr, av, ac) \
	PRIMITIVE_FUNCALL (rv, subr_function (subr), av, ac);


/* This is the list of current catches (and also condition-cases).
   This is a stack: the most recent catch is at the head of the list.
   The list is threaded through the stack frames of the C functions
   that set up the catches; this is similar to the way the GCPRO list
   is handled, but different from the condition-handler list (which is
   a simple Lisp list) and the specbind stack, which is a contiguous
   array of `struct specbinding's, grown (using realloc()) as
   necessary. (Note that all four of these lists behave as a stacks.)

   Catches are created by declaring a 'struct catchtag' locally,
   filling the .TAG field in with the tag, and doing a setjmp() on
   .JMP.  Fthrow() will store the value passed to it in .VAL and
   longjmp() back to .JMP, back to the function that established the
   catch.  This will always be either internal_catch() (catches
   established internally or through `catch') or condition_case_1
   (condition-cases established internally or through
   `condition-case').

   The catchtag also records the current position in the
   call stack (stored in BACKTRACE_LIST), the current position
   in the specpdl stack (used for variable bindings and
   unwind-protects), the value of LISP_EVAL_DEPTH, and the
   current position in the GCPRO stack.  All of these are
   restored by Fthrow().
  */

struct catchtag *catchlist;

/* A special tag that can be used internally from C code to catch
   every attempt to throw past this level. */
Lisp_Object Vcatch_everything_tag;

Lisp_Object Qautoload, Qmacro, Qexit;
Lisp_Object Qinteractive, Qcommandp, Qdefun, Qprogn, Qvalues;
Lisp_Object Vquit_flag, Vinhibit_quit;
Lisp_Object Qand_rest, Qand_optional;
Lisp_Object Qdebug_on_error, Qstack_trace_on_error;
Lisp_Object Qdebug_on_signal, Qstack_trace_on_signal;
Lisp_Object Qdebugger;
Lisp_Object Qinhibit_quit;
Lisp_Object Qfinalize_list;
Lisp_Object Qrun_hooks;
Lisp_Object Qsetq;
Lisp_Object Qdisplay_warning;
Lisp_Object Vpending_warnings, Vpending_warnings_tail;
Lisp_Object Qif;

/* Flags specifying which operations are currently inhibited. */
int inhibit_flags;

/* Buffers, frames, windows, devices, and consoles created since most
   recent active
   call_trapping_problems (INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION).
*/
Lisp_Object Vdeletable_permanent_display_objects;

/* Buffers created since most recent active
   call_trapping_problems (INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION). */
Lisp_Object Vmodifiable_buffers;

/* Minimum level at which warnings are logged.  Below this, they're ignored
   entirely -- not even generated. */
Lisp_Object Vlog_warning_minimum_level;

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (OFEATURES . nil) for a provide.  */
Lisp_Object Vautoload_queue;

/* Current number of specbindings allocated in specpdl.  */
int specpdl_size;

/* Pointer to beginning of specpdl.  */
struct specbinding *specpdl;

/* Pointer to first unused element in specpdl.  */
struct specbinding *specpdl_ptr;

/* specpdl_ptr - specpdl */
int specpdl_depth_counter;

/* Maximum size allowed for specpdl allocation */
Fixnum max_specpdl_size;

/* Depth in Lisp evaluations and function calls.  */
int lisp_eval_depth;

/* Maximum allowed depth in Lisp evaluations and function calls.  */
Fixnum max_lisp_eval_depth;

/* Nonzero means enter debugger before next function call */
static int debug_on_next_call;

int backtrace_with_internal_sections;

/* List of conditions (non-nil atom means all) which cause a backtrace
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vstack_trace_on_error;

/* List of conditions (non-nil atom means all) which enter the debugger
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vdebug_on_error;

/* List of conditions and regexps specifying error messages which
   do not enter the debugger even if Vdebug_on_error says they should.  */
Lisp_Object Vdebug_ignored_errors;

/* List of conditions (non-nil atom means all) which cause a backtrace
   if any error is signalled.  */
Lisp_Object Vstack_trace_on_signal;

/* List of conditions (non-nil atom means all) which enter the debugger
   if any error is signalled.  */
Lisp_Object Vdebug_on_signal;

/* Nonzero means enter debugger if a quit signal
   is handled by the command loop's error handler.

   From lisp, this is a boolean variable and may have the values 0 and 1.
   But, eval.c temporarily uses the second bit of this variable to indicate
   that a critical_quit is in progress.  The second bit is reset immediately
   after it is processed in signal_call_debugger().  */
int debug_on_quit;

#if 0 /* FSFmacs */
/* entering_debugger is basically equivalent */
/* The value of num_nonmacro_input_chars as of the last time we
   started to enter the debugger.  If we decide to enter the debugger
   again when this is still equal to num_nonmacro_input_chars, then we
   know that the debugger itself has an error, and we should just
   signal the error instead of entering an infinite loop of debugger
   invocations.  */
int when_entered_debugger;
#endif

/* Nonzero means we are trying to enter the debugger.
   This is to prevent recursive attempts.
   Cleared by the debugger calling Fbacktrace */
static int entering_debugger;

/* Function to call to invoke the debugger */
Lisp_Object Vdebugger;

/* List of condition handlers currently in effect.
   The elements of this lists were at one point in the past
   threaded through the stack frames of Fcondition_case and
   related functions, but now are stored separately in a normal
   stack.  When an error is signaled (by calling Fsignal, below),
   this list is searched for an element that applies.

   Each element of this list is one of the following:

   -- A list of a handler function and possibly args to pass to the
   function.  This is a handler established with the Lisp primitive
   `call-with-condition-handler' or related C function
   call_with_condition_handler():

     If the handler function is an opaque ptr object, it is a handler
     that was established in C using call_with_condition_handler(),
     and the contents of the object are a function pointer which takes
     three arguments, the signal name and signal data (same arguments
     passed to `signal') and a third Lisp_Object argument, specified
     in the call to call_with_condition_handler() and stored as the
     second element of the list containing the handler functionl.
  
     If the handler function is a regular Lisp_Object, it is a handler
     that was established using `call-with-condition-handler'.
     Currently there are no more arguments in the list containing the
     handler function, and only one argument is passed to the handler
     function: a cons of the signal name and signal data arguments
     passed to `signal'.
  
   -- A list whose car is Qunbound and whose cdr is Qt.  This is a
   special condition-case handler established by C code with
   condition_case_1().  All errors are trapped; the debugger is not
   invoked even if `debug-on-error' was set.

   -- A list whose car is Qunbound and whose cdr is Qerror.  This is a
   special condition-case handler established by C code with
   condition_case_1().  It is like Qt except that the debugger is
   invoked normally if it is called for.

   -- A list whose car is Qunbound and whose cdr is a list of lists
   (CONDITION-NAME BODY ...) exactly as in `condition-case'.  This is
   a normal `condition-case' handler.

   Note that in all cases *except* the first, there is a corresponding
   catch, whose TAG is the value of Vcondition_handlers just after the
   handler data just described is pushed onto it.  The reason is that
   `condition-case' handlers need to throw back to the place where the
   handler was installed before invoking it, while
   `call-with-condition-handler' handlers are invoked in the
   environment that `signal' was invoked in.  */


static Lisp_Object Vcondition_handlers;

/* I think we should keep this enabled all the time, not just when
   error checking is enabled, because if one of these puppies pops up,
   it will trash the stack if not caught, making it that much harder to
   debug.  It doesn't cause speed loss. */
#define DEFEND_AGAINST_THROW_RECURSION

#ifdef DEFEND_AGAINST_THROW_RECURSION
/* Used for error catching purposes by throw_or_bomb_out */
static int throw_level;
#endif

static int warning_will_be_discarded (Lisp_Object level);


/************************************************************************/
/*			The subr object type				*/
/************************************************************************/

static void
print_subr (Lisp_Object obj, Lisp_Object printcharfun, int UNUSED (escapeflag))
{
  Lisp_Subr *subr = XSUBR (obj);
  const CIbyte *header =
    (subr->max_args == UNEVALLED) ? "#<special-form " : "#<subr ";
  const CIbyte *name = subr_name (subr);
  const CIbyte *trailer = subr->prompt ? " (interactive)>" : ">";

  if (print_readably)
    printing_unreadable_object ("%s%s%s", header, name, trailer);

  write_c_string (printcharfun, header);
  write_c_string (printcharfun, name);
  write_c_string (printcharfun, trailer);
}

static const struct memory_description subr_description[] = {
  { XD_DOC_STRING, offsetof (Lisp_Subr, doc), 0, 0, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("subr", subr,
				     1, /*dumpable-flag*/
				     0, print_subr, 0, 0, 0,
				     subr_description,
				     Lisp_Subr);

/************************************************************************/
/*			 Entering the debugger				*/
/************************************************************************/

static Lisp_Object
current_warning_level (void)
{
  if (inhibit_flags & ISSUE_WARNINGS_AT_DEBUG_LEVEL)
    return Qdebug;
  else
    return Qwarning;
}

/* Actually call the debugger.  ARG is a list of args that will be
   passed to the debugger function, as follows;

If due to frame exit, args are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, args are `error' and a list of the args to `signal'.
If due to `apply' or `funcall' entry, one arg, `lambda'.
If due to `eval' entry, one arg, t.

*/

static Lisp_Object
call_debugger_259 (Lisp_Object arg)
{
  return apply1 (Vdebugger, arg);
}

/* Call the debugger, doing some encapsulation.  We make sure we have
   some room on the eval and specpdl stacks, and bind entering_debugger
   to 1 during this call.  This is used to trap errors that may occur
   when entering the debugger (e.g. the value of `debugger' is invalid),
   so that the debugger will not be recursively entered if debug-on-error
   is set. (Otherwise, XEmacs would infinitely recurse, attempting to
   enter the debugger.) entering_debugger gets reset to 0 as soon
   as a backtrace is displayed, so that further errors can indeed be
   handled normally.

   We also establish a catch for 'debugger.  If the debugger function
   throws to this instead of returning a value, it means that the user
   pressed 'c' (pretend like the debugger was never entered).  The
   function then returns Qunbound. (If the user pressed 'r', for
   return a value, then the debugger function returns normally with
   this value.)

   The difference between 'c' and 'r' is as follows:

   debug-on-call:
     No difference.  The call proceeds as normal.
   debug-on-exit:
     With 'r', the specified value is returned as the function's
     return value.  With 'c', the value that would normally be
     returned is returned.
   signal:
     With 'r', the specified value is returned as the return
     value of `signal'. (This is the only time that `signal'
     can return, instead of making a non-local exit.) With `c',
     `signal' will continue looking for handlers as if the
     debugger was never entered, and will probably end up
     throwing to a handler or to top-level.
*/

static Lisp_Object
call_debugger (Lisp_Object arg)
{
  int threw;
  Lisp_Object val;
  int speccount;

  debug_on_next_call = 0;

  if (inhibit_flags & INHIBIT_ENTERING_DEBUGGER)
    {
      if (!(inhibit_flags & INHIBIT_WARNING_ISSUE))
	warn_when_safe
	  (Qdebugger, current_warning_level (),
	   "Unable to enter debugger within critical section");
      return Qunbound;
    }

  if (lisp_eval_depth + 20 > max_lisp_eval_depth)
    max_lisp_eval_depth = lisp_eval_depth + 20;
  if (specpdl_size + 40 > max_specpdl_size)
    max_specpdl_size = specpdl_size + 40;

  speccount = internal_bind_int (&entering_debugger, 1);
  val = internal_catch (Qdebugger, call_debugger_259, arg, &threw, 0);

  return unbind_to_1 (speccount, ((threw)
				? Qunbound /* Not returning a value */
				: val));
}

/* Called when debug-on-exit behavior is called for.  Enter the debugger
   with the appropriate args for this.  VAL is the exit value that is
   about to be returned. */

static Lisp_Object
do_debug_on_exit (Lisp_Object val)
{
  /* This is falsified by call_debugger */
  Lisp_Object v = call_debugger (list2 (Qexit, val));

  return !UNBOUNDP (v) ? v : val;
}

/* Called when debug-on-call behavior is called for.  Enter the debugger
   with the appropriate args for this.  VAL is either t for a call
   through `eval' or 'lambda for a call through `funcall'.

   #### The differentiation here between EVAL and FUNCALL is bogus.
   FUNCALL can be defined as

   (defmacro func (fun &rest args)
     (cons (eval fun) args))

   and should be treated as such.
 */

static void
do_debug_on_call (Lisp_Object code)
{
  debug_on_next_call = 0;
  backtrace_list->debug_on_exit = 1;
  call_debugger (list1 (code));
}

/* LIST is the value of one of the variables `debug-on-error',
   `debug-on-signal', `stack-trace-on-error', or `stack-trace-on-signal',
   and CONDITIONS is the list of error conditions associated with
   the error being signalled.  This returns non-nil if LIST
   matches CONDITIONS. (A nil value for LIST does not match
   CONDITIONS.  A non-list value for LIST does match CONDITIONS.
   A list matches CONDITIONS when one of the symbols in LIST is the
   same as one of the symbols in CONDITIONS.) */

static int
wants_debugger (Lisp_Object list, Lisp_Object conditions)
{
  if (NILP (list))
    return 0;
  if (! CONSP (list))
    return 1;

  while (CONSP (conditions))
    {
      Lisp_Object this, tail;
      this = XCAR (conditions);
      for (tail = list; CONSP (tail); tail = XCDR (tail))
	if (EQ (XCAR (tail), this))
	  return 1;
      conditions = XCDR (conditions);
    }
  return 0;
}


/* Return 1 if an error with condition-symbols CONDITIONS,
   and described by SIGNAL-DATA, should skip the debugger
   according to debugger-ignore-errors.  */

static int
skip_debugger (Lisp_Object conditions, Lisp_Object data)
{
  /* This function can GC */
  Lisp_Object tail;
  int first_string = 1;
  Lisp_Object error_message = Qnil;

  for (tail = Vdebug_ignored_errors; CONSP (tail); tail = XCDR (tail))
    {
      if (STRINGP (XCAR (tail)))
	{
	  if (first_string)
	    {
	      error_message = Ferror_message_string (data);
	      first_string = 0;
	    }
	  if (fast_lisp_string_match (XCAR (tail), error_message) >= 0)
	    return 1;
	}
      else
	{
	  Lisp_Object contail;

          for (contail = conditions; CONSP (contail); contail = XCDR (contail))
            if (EQ (XCAR (tail), XCAR (contail)))
	      return 1;
	}
    }

  return 0;
}

/* Actually generate a backtrace on STREAM. */

static Lisp_Object
backtrace_259 (Lisp_Object stream)
{
  return Fbacktrace (stream, Qt);
}

#ifdef DEBUG_XEMACS

static void
trace_out_and_die (Lisp_Object err)
{
  Fdisplay_error (err, Qt);
  backtrace_259 (Qnil);
  stderr_out ("XEmacs exiting to debugger.\n");
  Fforce_debugging_signal (Qt);
  /* Unlikely to be reached */
}

#endif

/* An error was signaled.  Maybe call the debugger, if the `debug-on-error'
   etc. variables call for this.  CONDITIONS is the list of conditions
   associated with the error being signalled.  SIG is the actual error
   being signalled, and DATA is the associated data (these are exactly
   the same as the arguments to `signal').  ACTIVE_HANDLERS is the
   list of error handlers that are to be put in place while the debugger
   is called.  This is generally the remaining handlers that are
   outside of the innermost handler trapping this error.  This way,
   if the same error occurs inside of the debugger, you usually don't get
   the debugger entered recursively.

   This function returns Qunbound if it didn't call the debugger or if
   the user asked (through 'c') that XEmacs should pretend like the
   debugger was never entered.  Otherwise, it returns the value
   that the user specified with `r'. (Note that much of the time,
   the user will abort with C-], and we will never have a chance to
   return anything at all.)

   SIGNAL_VARS_ONLY means we should only look at debug-on-signal
   and stack-trace-on-signal to control whether we do anything.
   This is so that debug-on-error doesn't make handled errors
   cause the debugger to get invoked.

   STACK_TRACE_DISPLAYED and DEBUGGER_ENTERED are used so that
   those functions aren't done more than once in a single `signal'
   session. */

static Lisp_Object
signal_call_debugger (Lisp_Object conditions,
                      Lisp_Object sig, Lisp_Object data,
                      Lisp_Object active_handlers,
		      int signal_vars_only,
		      int *stack_trace_displayed,
		      int *debugger_entered)
{
#ifdef PIGS_FLY_AND_ALL_C_CODE_CAN_HANDLE_GC_OCCURRING_ALMOST_ANYWHERE
  /* This function can GC */
#else /* reality check */
  /* This function cannot GC because it inhibits GC during its operation */
#endif

  Lisp_Object val = Qunbound;
  Lisp_Object all_handlers = Vcondition_handlers;
  Lisp_Object temp_data = Qnil;
  int outer_speccount = specpdl_depth();
  int speccount;

#ifdef PIGS_FLY_AND_ALL_C_CODE_CAN_HANDLE_GC_OCCURRING_ALMOST_ANYWHERE
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (all_handlers, temp_data);
#else
  begin_gc_forbidden ();
#endif

  speccount = specpdl_depth();

  Vcondition_handlers = active_handlers;

  temp_data = Fcons (sig, data); /* needed for skip_debugger */

  if (!entering_debugger && !*stack_trace_displayed && !signal_vars_only
      && wants_debugger (Vstack_trace_on_error, conditions)
      && !skip_debugger (conditions, temp_data))
    {
      specbind (Qdebug_on_error,	Qnil);
      specbind (Qstack_trace_on_error,	Qnil);
      specbind (Qdebug_on_signal,	Qnil);
      specbind (Qstack_trace_on_signal, Qnil);

      if (!noninteractive)
	internal_with_output_to_temp_buffer (build_string ("*Backtrace*"),
					     backtrace_259,
					     Qnil,
					     Qnil);
      else /* in batch mode, we want this going to stderr. */
	backtrace_259 (Qnil);
      unbind_to (speccount);
      *stack_trace_displayed = 1;
    }

  if (!entering_debugger && !*debugger_entered && !signal_vars_only
      && (EQ (sig, Qquit)
	  ? debug_on_quit
	  : wants_debugger (Vdebug_on_error, conditions))
      && !skip_debugger (conditions, temp_data))
    {
      debug_on_quit &= ~2;	/* reset critical bit */

      specbind (Qdebug_on_error,	Qnil);
      specbind (Qstack_trace_on_error,	Qnil);
      specbind (Qdebug_on_signal,	Qnil);
      specbind (Qstack_trace_on_signal, Qnil);

#ifdef DEBUG_XEMACS
      if (noninteractive)
	trace_out_and_die (Fcons (sig, data));
#endif

      val = call_debugger (list2 (Qerror, (Fcons (sig, data))));
      unbind_to (speccount);
      *debugger_entered = 1;
    }

  if (!entering_debugger && !*stack_trace_displayed
      && wants_debugger (Vstack_trace_on_signal, conditions))
    {
      specbind (Qdebug_on_error,	Qnil);
      specbind (Qstack_trace_on_error,	Qnil);
      specbind (Qdebug_on_signal,	Qnil);
      specbind (Qstack_trace_on_signal, Qnil);

      if (!noninteractive)
	internal_with_output_to_temp_buffer (build_string ("*Backtrace*"),
					     backtrace_259,
					     Qnil,
					     Qnil);
      else /* in batch mode, we want this going to stderr. */
	backtrace_259 (Qnil);
      unbind_to (speccount);
      *stack_trace_displayed = 1;
    }

  if (!entering_debugger && !*debugger_entered
      && (EQ (sig, Qquit)
	  ? debug_on_quit
	  : wants_debugger (Vdebug_on_signal, conditions)))
    {
      debug_on_quit &= ~2;	/* reset critical bit */

      specbind (Qdebug_on_error,	Qnil);
      specbind (Qstack_trace_on_error,	Qnil);
      specbind (Qdebug_on_signal,	Qnil);
      specbind (Qstack_trace_on_signal, Qnil);

#ifdef DEBUG_XEMACS
      if (noninteractive)
	trace_out_and_die (Fcons (sig, data));
#endif

      val = call_debugger (list2 (Qerror, (Fcons (sig, data))));
      *debugger_entered = 1;
    }

#ifdef PIGS_FLY_AND_ALL_C_CODE_CAN_HANDLE_GC_OCCURRING_ALMOST_ANYWHERE
  UNGCPRO;
#endif
  Vcondition_handlers = all_handlers;
  return unbind_to_1 (outer_speccount, val);
}


/************************************************************************/
/*		       The basic special forms				*/
/************************************************************************/

/* Except for Fprogn(), the basic special forms below are only called
   from interpreted code.  The byte compiler turns them into bytecodes. */

DEFUN ("or", For, 0, UNEVALLED, 0, /*
Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.
*/
       (args))
{
  /* This function can GC */
  REGISTER Lisp_Object val;

  LIST_LOOP_2 (arg, args)
    {
      if (!NILP (val = Feval (arg)))
	return val;
    }

  return Qnil;
}

DEFUN ("and", Fand, 0, UNEVALLED, 0, /*
Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.
*/
       (args))
{
  /* This function can GC */
  REGISTER Lisp_Object val = Qt;

  LIST_LOOP_2 (arg, args)
    {
      if (NILP (val = Feval (arg)))
	return val;
    }

  return val;
}

DEFUN ("if", Fif, 2, UNEVALLED, 0, /*
\(if COND THEN ELSE...): if COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object condition  = XCAR (args);
  Lisp_Object then_form  = XCAR (XCDR (args));
  Lisp_Object else_forms = XCDR (XCDR (args));

  if (!NILP (Feval (condition)))
    return Feval (then_form);
  else
    return Fprogn (else_forms);
}

/* Macros `when' and `unless' are trivially defined in Lisp,
   but it helps for bootstrapping to have them ALWAYS defined. */

DEFUN ("when", Fwhen, 1, MANY, 0, /*
\(when COND BODY...): if COND yields non-nil, do BODY, else return nil.
BODY can be zero or more expressions.  If BODY is nil, return nil.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object cond = args[0];
  Lisp_Object body;
 
  switch (nargs)
    {
    case 1:  body = Qnil; break;
    case 2:  body = args[1]; break;
    default: body = Fcons (Qprogn, Flist (nargs-1, args+1)); break;
    }

  return list3 (Qif, cond, body);
}

DEFUN ("unless", Funless, 1, MANY, 0, /*
\(unless COND BODY...): if COND yields nil, do BODY, else return nil.
BODY can be zero or more expressions.  If BODY is nil, return nil.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object cond = args[0];
  Lisp_Object body = Flist (nargs-1, args+1);
  return Fcons (Qif, Fcons (cond, Fcons (Qnil, body)));
}

DEFUN ("cond", Fcond, 0, UNEVALLED, 0, /*
\(cond CLAUSES...): try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If no clause succeeds, cond returns nil.
If a clause has one element, as in (CONDITION),
CONDITION's value if non-nil is returned from the cond-form.
*/
       (args))
{
  /* This function can GC */
  REGISTER Lisp_Object val;

  LIST_LOOP_2 (clause, args)
    {
      CHECK_CONS (clause);
      if (!NILP (val = Feval (XCAR (clause))))
	{
	  if (!NILP (clause = XCDR (clause)))
	    {
	      CHECK_TRUE_LIST (clause);
	      val = Fprogn (clause);
	    }
	  return val;
	}
    }

  return Qnil;
}

DEFUN ("progn", Fprogn, 0, UNEVALLED, 0, /*
\(progn BODY...): eval BODY forms sequentially and return value of last one.
*/
       (args))
{
  /* This function can GC */
  /* Caller must provide a true list in ARGS */
  REGISTER Lisp_Object val = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (args);

  {
    LIST_LOOP_2 (form, args)
      val = Feval (form);
  }

  UNGCPRO;
  return val;
}

/* Fprog1() is the canonical example of a function that must GCPRO a
   Lisp_Object across calls to Feval(). */

DEFUN ("prog1", Fprog1, 1, UNEVALLED, 0, /*
Similar to `progn', but the value of the first form is returned.
\(prog1 FIRST BODY...): All the arguments are evaluated sequentially.
The value of FIRST is saved during evaluation of the remaining args,
whose values are discarded.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object val;
  struct gcpro gcpro1;

  val = Feval (XCAR (args));

  GCPRO1 (val);

  {
    LIST_LOOP_2 (form, XCDR (args))
      Feval (form);
  }

  UNGCPRO;
  return val;
}

DEFUN ("prog2", Fprog2, 2, UNEVALLED, 0, /*
Similar to `progn', but the value of the second form is returned.
\(prog2 FIRST SECOND BODY...): All the arguments are evaluated sequentially.
The value of SECOND is saved during evaluation of the remaining args,
whose values are discarded.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object val;
  struct gcpro gcpro1;

  Feval (XCAR (args));
  args = XCDR (args);
  val = Feval (XCAR (args));
  args = XCDR (args);

  GCPRO1 (val);

  {
    LIST_LOOP_2 (form, args)
      Feval (form);
  }

  UNGCPRO;
  return val;
}

DEFUN ("let*", FletX, 1, UNEVALLED, 0, /*
\(let* VARLIST BODY...): bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object varlist = XCAR (args);
  Lisp_Object body    = XCDR (args);
  int speccount = specpdl_depth();

  EXTERNAL_LIST_LOOP_3 (var, varlist, tail)
    {
      Lisp_Object symbol, value, tem;
      if (SYMBOLP (var))
	symbol = var, value = Qnil;
      else
	{
	  CHECK_CONS (var);
	  symbol = XCAR (var);
	  tem    = XCDR (var);
	  if (NILP (tem))
	    value = Qnil;
	  else
	    {
	      CHECK_CONS (tem);
	      value = Feval (XCAR (tem));
	      if (!NILP (XCDR (tem)))
		sferror
		  ("`let' bindings can have only one value-form", var);
	    }
	}
      specbind (symbol, value);
    }
  return unbind_to_1 (speccount, Fprogn (body));
}

DEFUN ("let", Flet, 1, UNEVALLED, 0, /*
\(let VARLIST BODY...): bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object varlist = XCAR (args);
  Lisp_Object body    = XCDR (args);
  int speccount = specpdl_depth();
  Lisp_Object *temps;
  int idx;
  struct gcpro gcpro1;

  /* Make space to hold the values to give the bound variables. */
  {
    int varcount;
    GET_EXTERNAL_LIST_LENGTH (varlist, varcount);
    temps = alloca_array (Lisp_Object, varcount);
  }

  /* Compute the values and store them in `temps' */
  GCPRO1 (*temps);
  gcpro1.nvars = 0;

  idx = 0;
  {
    LIST_LOOP_2 (var, varlist)
      {
	Lisp_Object *value = &temps[idx++];
	if (SYMBOLP (var))
	  *value = Qnil;
	else
	  {
	    Lisp_Object tem;
	    CHECK_CONS (var);
	    tem = XCDR (var);
	    if (NILP (tem))
	      *value = Qnil;
	    else
	      {
		CHECK_CONS (tem);
		*value = Feval (XCAR (tem));
		gcpro1.nvars = idx;

		if (!NILP (XCDR (tem)))
		  sferror
		    ("`let' bindings can have only one value-form", var);
	      }
	  }
      }
  }

  idx = 0;
  {
    LIST_LOOP_2 (var, varlist)
      {
	specbind (SYMBOLP (var) ? var : XCAR (var), temps[idx++]);
      }
  }

  UNGCPRO;

  return unbind_to_1 (speccount, Fprogn (body));
}

DEFUN ("while", Fwhile, 1, UNEVALLED, 0, /*
\(while TEST BODY...): if TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object test = XCAR (args);
  Lisp_Object body = XCDR (args);

  while (!NILP (Feval (test)))
    {
      QUIT;
      Fprogn (body);
    }

  return Qnil;
}

DEFUN ("setq", Fsetq, 0, UNEVALLED, 0, /*
\(setq SYM VAL SYM VAL ...): set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the `setq'.
The return value of the `setq' form is the value of the last VAL.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object symbol, tail, val = Qnil;
  int nargs;
  struct gcpro gcpro1;

  GET_LIST_LENGTH (args, nargs);

  if (nargs & 1)		/* Odd number of arguments? */
    Fsignal (Qwrong_number_of_arguments, list2 (Qsetq, make_int (nargs)));

  GCPRO1 (val);

  PROPERTY_LIST_LOOP (tail, symbol, val, args)
    {
      val = Feval (val);
      Fset (symbol, val);
    }

  UNGCPRO;
  return val;
}

DEFUN ("quote", Fquote, 1, UNEVALLED, 0, /*
Return the argument, without evaluating it.  `(quote x)' yields `x'.
*/
       (args))
{
  return XCAR (args);
}

DEFUN ("function", Ffunction, 1, UNEVALLED, 0, /*
Like `quote', but preferred for objects which are functions.
In byte compilation, `function' causes its argument to be compiled.
`quote' cannot do that.
*/
       (args))
{
  return XCAR (args);
}


/************************************************************************/
/*			Defining functions/variables			*/
/************************************************************************/
static Lisp_Object
define_function (Lisp_Object name, Lisp_Object defn)
{
  Ffset (name, defn);
  LOADHIST_ATTACH (name);
  return name;
}

DEFUN ("defun", Fdefun, 2, UNEVALLED, 0, /*
\(defun NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
*/
       (args))
{
  /* This function can GC */
  return define_function (XCAR (args),
			  Fcons (Qlambda, XCDR (args)));
}

DEFUN ("defmacro", Fdefmacro, 2, UNEVALLED, 0, /*
\(defmacro NAME ARGLIST [DOCSTRING] BODY...): define NAME as a macro.
The definition is (macro lambda ARGLIST [DOCSTRING] BODY...).
When the macro is called, as in (NAME ARGS...),
the function (lambda ARGLIST BODY...) is applied to
the list ARGS... as it appears in the expression,
and the result should be a form to be evaluated instead of the original.
*/
       (args))
{
  /* This function can GC */
  return define_function (XCAR (args),
			  Fcons (Qmacro, Fcons (Qlambda, XCDR (args))));
}

DEFUN ("defvar", Fdefvar, 1, UNEVALLED, 0, /*
\(defvar SYMBOL INITVALUE DOCSTRING): define SYMBOL as a variable.
You are not required to define a variable in order to use it,
 but the definition can supply documentation and an initial value
 in a way that tags can recognize.

INITVALUE is evaluated, and used to set SYMBOL, only if SYMBOL's value is
 void. (However, when you evaluate a defvar interactively, it acts like a
 defconst: SYMBOL's value is always set regardless of whether it's currently
 void.)
If SYMBOL is buffer-local, its default value is what is set;
 buffer-local values are not affected.
INITVALUE and DOCSTRING are optional.
If DOCSTRING starts with *, this variable is identified as a user option.
 This means that M-x set-variable recognizes it.
If INITVALUE is missing, SYMBOL's value is not set.

In lisp-interaction-mode defvar is treated as defconst.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object sym = XCAR (args);

  if (!NILP (args = XCDR (args)))
    {
      Lisp_Object val = XCAR (args);

      if (NILP (Fdefault_boundp (sym)))
	{
	  struct gcpro gcpro1;
	  GCPRO1 (val);
	  val = Feval (val);
	  Fset_default (sym, val);
	  UNGCPRO;
	}

      if (!NILP (args = XCDR (args)))
	{
	  Lisp_Object doc = XCAR (args);
	  Fput (sym, Qvariable_documentation, doc);
	  if (!NILP (args = XCDR (args)))
	    signal_error (Qwrong_number_of_arguments, "too many arguments", Qunbound);
	}
    }

#ifdef I18N3
  if (!NILP (Vfile_domain))
    Fput (sym, Qvariable_domain, Vfile_domain);
#endif

  LOADHIST_ATTACH (sym);
  return sym;
}

DEFUN ("defconst", Fdefconst, 2, UNEVALLED, 0, /*
\(defconst SYMBOL INITVALUE DOCSTRING): define SYMBOL as a constant
variable.
The intent is that programs do not change this value, but users may.
Always sets the value of SYMBOL to the result of evalling INITVALUE.
If SYMBOL is buffer-local, its default value is what is set;
 buffer-local values are not affected.
DOCSTRING is optional.
If DOCSTRING starts with *, this variable is identified as a user option.
 This means that M-x set-variable recognizes it.

Note: do not use `defconst' for user options in libraries that are not
 normally loaded, since it is useful for users to be able to specify
 their own values for such variables before loading the library.
Since `defconst' unconditionally assigns the variable,
 it would override the user's choice.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object sym = XCAR (args);
  Lisp_Object val = Feval (XCAR (args = XCDR (args)));
  struct gcpro gcpro1;

  GCPRO1 (val);

  Fset_default (sym, val);

  UNGCPRO;

  if (!NILP (args = XCDR (args)))
    {
      Lisp_Object doc = XCAR (args);
      Fput (sym, Qvariable_documentation, doc);
      if (!NILP (args = XCDR (args)))
	signal_error (Qwrong_number_of_arguments, "too many arguments", Qunbound);
    }

#ifdef I18N3
  if (!NILP (Vfile_domain))
    Fput (sym, Qvariable_domain, Vfile_domain);
#endif

  LOADHIST_ATTACH (sym);
  return sym;
}

DEFUN ("user-variable-p", Fuser_variable_p, 1, 1, 0, /*
Return t if VARIABLE is intended to be set and modified by users.
\(The alternative is a variable used internally in a Lisp program.)
Determined by whether the first character of the documentation
for the variable is `*'.
*/
       (variable))
{
  Lisp_Object documentation = Fget (variable, Qvariable_documentation, Qnil);

  return
    ((INTP (documentation) && XINT (documentation) < 0) ||

     (STRINGP (documentation) &&
      (string_byte (documentation, 0) == '*')) ||

     /* If (STRING . INTEGER), a negative integer means a user variable. */
     (CONSP (documentation)
      && STRINGP (XCAR (documentation))
      && INTP (XCDR (documentation))
      && XINT (XCDR (documentation)) < 0)) ?
    Qt : Qnil;
}

DEFUN ("macroexpand-internal", Fmacroexpand_internal, 1, 2, 0, /*
Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.
*/
       (form, environment))
{
  /* This function can GC */
  /* With cleanups from Hallvard Furuseth.  */
  REGISTER Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (!CONSP (form))
	break;
      /* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
      def = sym = XCAR (form);
      tem = Qnil;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (SYMBOLP (def))
	{
	  QUIT;
	  sym = def;
	  tem = Fassq (sym, environment);
	  if (NILP (tem))
	    {
	      def = XSYMBOL (sym)->function;
	      if (!UNBOUNDP (def))
		continue;
	    }
	  break;
	}
      /* Right now TEM is the result from SYM in ENVIRONMENT,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NILP (tem))
	{
	  /* SYM is not mentioned in ENVIRONMENT.
	     Look at its function definition.  */
	  if (UNBOUNDP (def)
	      || !CONSP (def))
	    /* Not defined or definition not suitable */
	    break;
	  if (EQ (XCAR (def), Qautoload))
	    {
	      /* Autoloading function: will it be a macro when loaded?  */
	      tem = Felt (def, make_int (4));
	      if (EQ (tem, Qt) || EQ (tem, Qmacro))
		{
		  /* Yes, load it and try again.  */
		  /* do_autoload GCPROs both arguments */
		  do_autoload (def, sym);
		  continue;
		}
	      else
		break;
	    }
	  else if (!EQ (XCAR (def), Qmacro))
	    break;
	  else expander = XCDR (def);
	}
      else
	{
	  expander = XCDR (tem);
	  if (NILP (expander))
	    break;
	}
      form = apply1 (expander, XCDR (form));
    }
  return form;
}


/************************************************************************/
/*			    Non-local exits				*/
/************************************************************************/

#ifdef ERROR_CHECK_TRAPPING_PROBLEMS

int
proper_redisplay_wrapping_in_place (void)
{
  return !in_display
    || ((get_inhibit_flags () & INTERNAL_INHIBIT_ERRORS)
	&& (get_inhibit_flags () & INTERNAL_INHIBIT_THROWS));
}

static void
check_proper_critical_section_nonlocal_exit_protection (void)
{
  assert_with_message
    (proper_redisplay_wrapping_in_place (),
     "Attempted non-local exit from within redisplay without being properly wrapped");
}

static void
check_proper_critical_section_lisp_protection (void)
{
  assert_with_message
    (proper_redisplay_wrapping_in_place (),
     "Attempt to call Lisp code from within redisplay without being properly wrapped");
}

#endif /* ERROR_CHECK_TRAPPING_PROBLEMS */

DEFUN ("catch", Fcatch, 1, UNEVALLED, 0, /*
\(catch TAG BODY...): eval BODY allowing nonlocal exits using `throw'.
TAG is evalled to get the tag to use.  Then the BODY is executed.
Within BODY, (throw TAG) with same (`eq') tag exits BODY and this `catch'.
If no throw happens, `catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from `catch'.
*/
       (args))
{
  /* This function can GC */
  Lisp_Object tag  = Feval (XCAR (args));
  Lisp_Object body = XCDR (args);
  return internal_catch (tag, Fprogn, body, 0, 0);
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code. */

Lisp_Object
internal_catch (Lisp_Object tag,
                Lisp_Object (*func) (Lisp_Object arg),
                Lisp_Object arg,
                int * volatile threw,
		Lisp_Object * volatile thrown_tag)
{
  /* This structure is made part of the chain `catchlist'.  */
  struct catchtag c;

  /* Fill in the components of c, and put it on the list.  */
  c.next = catchlist;
  c.tag = tag;
  c.actual_tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
#if 0 /* FSFmacs */
  /* #### */
  c.handlerlist = handlerlist;
#endif
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = specpdl_depth();
#if 0 /* FSFmacs */
  c.poll_suppress_count = async_timer_suppress_count;
#endif
  c.gcpro = gcprolist;
  catchlist = &c;

  /* Call FUNC.  */
  if (SETJMP (c.jmp))
    {
      /* Throw works by a longjmp that comes right here.  */
      if (threw) *threw = 1;
      if (thrown_tag) *thrown_tag = c.actual_tag;
      return c.val;
    }
  c.val = (*func) (arg);
  if (threw) *threw = 0;
  if (thrown_tag) *thrown_tag = Qnil;
  catchlist = c.next;
  check_catchlist_sanity ();
  return c.val;
}


/* Unwind the specbind, catch, and handler stacks back to CATCH, and
   jump to that CATCH, returning VALUE as the value of that catch.

   This is the guts of Fthrow and Fsignal; they differ only in the
   way they choose the catch tag to throw to.  A catch tag for a
   condition-case form has a TAG of Qnil.

   Before each catch is discarded, unbind all special bindings and
   execute all unwind-protect clauses made above that catch.  Unwind
   the handler stack as we go, so that the proper handlers are in
   effect for each unwind-protect clause we run.  At the end, restore
   some static info saved in CATCH, and longjmp to the location
   specified in the

   This is used for correct unwinding in Fthrow and Fsignal.  */

static DECLARE_DOESNT_RETURN (unwind_to_catch (struct catchtag *, Lisp_Object,
					       Lisp_Object));

static DOESNT_RETURN
unwind_to_catch (struct catchtag *c, Lisp_Object val, Lisp_Object tag)
{
  REGISTER int last_time;

  /* Unwind the specbind, catch, and handler stacks back to CATCH
     Before each catch is discarded, unbind all special bindings
     and execute all unwind-protect clauses made above that catch.
     At the end, restore some static info saved in CATCH,
     and longjmp to the location specified.
     */

  /* Save the value somewhere it will be GC'ed.
     (Can't overwrite tag slot because an unwind-protect may
     want to throw to this same tag, which isn't yet invalid.) */
  c->val = val;
  c->actual_tag = tag;

#if 0 /* FSFmacs */
  /* Restore the polling-suppression count.  */
  set_poll_suppress_count (catch->poll_suppress_count);
#endif

#if 1
  do
    {
      last_time = catchlist == c;

      /* Unwind the specpdl stack, and then restore the proper set of
         handlers.  */
      unbind_to (catchlist->pdlcount);
      catchlist = catchlist->next;
      check_catchlist_sanity ();
    }
  while (! last_time);
#else
  /* Former XEmacs code.  This is definitely not as correct because
     there may be a number of catches we're unwinding, and a number
     of unwind-protects in the process.  By not undoing the catches till
     the end, there may be invalid catches still current. (This would
     be a particular problem with code like this:

     (catch 'foo
       (call-some-code-which-does...
        (catch 'bar
          (unwind-protect
              (call-some-code-which-does...
               (catch 'bar
                 (call-some-code-which-does...
                  (throw 'foo nil))))
            (throw 'bar nil)))))

     This would try to throw to the inner (catch 'bar)!

     --ben
   */
  /* Unwind the specpdl stack */
  unbind_to (c->pdlcount);
  catchlist = c->next;
  check_catchlist_sanity ();
#endif /* Former code */

  UNWIND_GCPRO_TO (c->gcpro);
  if (profiling_active)
    {
      while (backtrace_list != c->backlist)
	{
          profile_record_unwind (backtrace_list);
	  backtrace_list = backtrace_list->next;
	}
    }
  else
    backtrace_list = c->backlist;
  lisp_eval_depth = c->lisp_eval_depth;

#ifdef DEFEND_AGAINST_THROW_RECURSION
  throw_level = 0;
#endif
  LONGJMP (c->jmp, 1);
}

static DECLARE_DOESNT_RETURN (throw_or_bomb_out (Lisp_Object, Lisp_Object, int,
						 Lisp_Object, Lisp_Object));

static DOESNT_RETURN
throw_or_bomb_out (Lisp_Object tag, Lisp_Object val, int bomb_out_p,
		   Lisp_Object sig, Lisp_Object data)
{
#ifdef DEFEND_AGAINST_THROW_RECURSION
  /* die if we recurse more than is reasonable */
  if (++throw_level > 20)
    abort ();
#endif

#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
  check_proper_critical_section_nonlocal_exit_protection ();
#endif

  /* If bomb_out_p is t, this is being called from Fsignal as a
     "last resort" when there is no handler for this error and
      the debugger couldn't be invoked, so we are throwing to
     'top-level.  If this tag doesn't exist (happens during the
     initialization stages) we would get in an infinite recursive
     Fsignal/Fthrow loop, so instead we bomb out to the
     really-early-error-handler.

     Note that in fact the only time that the "last resort"
     occurs is when there's no catch for 'top-level -- the
     'top-level catch and the catch-all error handler are
     established at the same time, in initial_command_loop/
     top_level_1.

     [[#### Fix this horrifitude!]]

     I don't think this is horrifitude, just defensive programming. --ben
     */

  while (1)
    {
      REGISTER struct catchtag *c;

#if 0 /* FSFmacs */
      if (!NILP (tag)) /* #### */
#endif
      for (c = catchlist; c; c = c->next)
	{
	  if (EQ (c->tag, tag) || EQ (c->tag, Vcatch_everything_tag))
	    unwind_to_catch (c, val, tag);
	}
      if (!bomb_out_p)
        tag = Fsignal (Qno_catch, list2 (tag, val));
      else
        call1 (Qreally_early_error_handler, Fcons (sig, data));
    }
}

/* See above, where CATCHLIST is defined, for a description of how
   Fthrow() works.

   Fthrow() is also called by Fsignal(), to do a non-local jump
   back to the appropriate condition-case handler after (maybe)
   the debugger is entered.  In that case, TAG is the value
   of Vcondition_handlers that was in place just after the
   condition-case handler was set up.  The car of this will be
   some data referring to the handler: Its car will be Qunbound
   (thus, this tag can never be generated by Lisp code), and
   its CDR will be the HANDLERS argument to condition_case_1()
   (either Qerror, Qt, or a list of handlers as in `condition-case').
   This works fine because Fthrow() does not care what TAG was
   passed to it: it just looks up the catch list for something
   that is EQ() to TAG.  When it finds it, it will longjmp()
   back to the place that established the catch (in this case,
   condition_case_1).  See below for more info.
*/

DEFUN_NORETURN ("throw", Fthrow, 2, 2, 0, /*
Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.  Tags are the same iff they are `eq'.
*/
       (tag, value))
{
  throw_or_bomb_out (tag, value, 0, Qnil, Qnil); /* Doesn't return */
  RETURN_NOT_REACHED (Qnil);
}

DEFUN ("unwind-protect", Funwind_protect, 1, UNEVALLED, 0, /*
Do BODYFORM, protecting with UNWINDFORMS.
Usage looks like (unwind-protect BODYFORM UNWINDFORMS...).
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
*/
       (args))
{
  /* This function can GC */
  int speccount = specpdl_depth();

  record_unwind_protect (Fprogn, XCDR (args));
  return unbind_to_1 (speccount, Feval (XCAR (args)));
}


/************************************************************************/
/*                           Trapping errors                            */
/************************************************************************/

static Lisp_Object
condition_bind_unwind (Lisp_Object loser)
{
  /* There is no problem freeing stuff here like there is in
     condition_case_unwind(), because there are no outside pointers
     (like the tag below in the catchlist) pointing to the objects. */
  
  /* ((handler-fun . handler-args) ... other handlers) */
  Lisp_Object tem = XCAR (loser);
  int first = 1;

  while (CONSP (tem))
    {
      Lisp_Object victim = tem;
      if (first && OPAQUE_PTRP (XCAR (victim)))
	free_opaque_ptr (XCAR (victim));
      first = 0;
      tem = XCDR (victim);
      free_cons (victim);
    }

  if (EQ (loser, Vcondition_handlers)) /* may have been rebound to some tail */
    Vcondition_handlers = XCDR (loser);

  free_cons (loser);
  return Qnil;
}

static Lisp_Object
condition_case_unwind (Lisp_Object loser)
{
  /* ((<unbound> . clauses) ... other handlers */
  /* NO! Doing this now leaves the tag deleted in a still-active
     catch.  With the recent changes to unwind_to_catch(), the
     evil situation might not happen any more; it certainly could
     happen before because it did.  But it's very precarious to rely
     on something like this.  #### Instead we should rewrite, adopting
     the FSF's mechanism with a struct handler instead of
     Vcondition_handlers; then we have NO Lisp-object structures used
     to hold all of the values, and there's no possibility either of
     crashes from freeing objects too quickly, or objects not getting
     freed and hanging around till the next GC.

     In practice, the extra consing here should not matter because
     it only happens when we throw past the condition-case, which almost
     always is the result of an error.  Most of the time, there will be
     no error, and we will free the objects below in the main function.

     --ben

     DO NOT DO: free_cons (XCAR (loser));
     */

  if (EQ (loser, Vcondition_handlers)) /* may have been rebound to some tail */
    Vcondition_handlers = XCDR (loser);

  /* DO NOT DO: free_cons (loser); */
  return Qnil;
}

/* Split out from condition_case_3 so that primitive C callers
   don't have to cons up a lisp handler form to be evaluated. */

/* Call a function BFUN of one argument BARG, trapping errors as
   specified by HANDLERS.  If no error occurs that is indicated by
   HANDLERS as something to be caught, the return value of this
   function is the return value from BFUN.  If such an error does
   occur, HFUN is called, and its return value becomes the
   return value of condition_case_1().  The second argument passed
   to HFUN will always be HARG.  The first argument depends on
   HANDLERS:

   If HANDLERS is Qt, all errors (this includes QUIT, but not
   non-local exits with `throw') cause HFUN to be invoked, and VAL
   (the first argument to HFUN) is a cons (SIG . DATA) of the
   arguments passed to `signal'.  The debugger is not invoked even if
   `debug-on-error' was set.

   A HANDLERS value of Qerror is the same as Qt except that the
   debugger is invoked if `debug-on-error' was set.

   Otherwise, HANDLERS should be a list of lists (CONDITION-NAME BODY ...)
   exactly as in `condition-case', and errors will be trapped
   as indicated in HANDLERS.  VAL (the first argument to HFUN) will
   be a cons whose car is the cons (SIG . DATA) and whose CDR is the
   list (BODY ...) from the appropriate slot in HANDLERS.

   This function pushes HANDLERS onto the front of Vcondition_handlers
   (actually with a Qunbound marker as well -- see Fthrow() above
   for why), establishes a catch whose tag is this new value of
   Vcondition_handlers, and calls BFUN.  When Fsignal() is called,
   it calls Fthrow(), setting TAG to this same new value of
   Vcondition_handlers and setting VAL to the same thing that will
   be passed to HFUN, as above.  Fthrow() longjmp()s back to the
   jump point we just established, and we in turn just call the
   HFUN and return its value.

   For a real condition-case, HFUN will always be
   run_condition_case_handlers() and HARG is the argument VAR
   to condition-case.  That function just binds VAR to the cons
   (SIG . DATA) that is the CAR of VAL, and calls the handler
   (BODY ...) that is the CDR of VAL.  Note that before calling
   Fthrow(), Fsignal() restored Vcondition_handlers to the value
   it had *before* condition_case_1() was called.  This maintains
   consistency (so that the state of things at exit of
   condition_case_1() is the same as at entry), and implies
   that the handler can signal the same error again (possibly
   after processing of its own), without getting in an infinite
   loop. */

Lisp_Object
condition_case_1 (Lisp_Object handlers,
                  Lisp_Object (*bfun) (Lisp_Object barg),
                  Lisp_Object barg,
                  Lisp_Object (*hfun) (Lisp_Object val, Lisp_Object harg),
                  Lisp_Object harg)
{
  int speccount = specpdl_depth();
  struct catchtag c;
  struct gcpro gcpro1, gcpro2, gcpro3;

#if 0 /* FSFmacs */
  c.tag = Qnil;
#else
  /* Do consing now so out-of-memory error happens up front */
  /* (unbound . stuff) is a special condition-case kludge marker
     which is known specially by Fsignal.
     [[ This is an abomination, but to fix it would require either
     making condition_case cons (a union of the conditions of the clauses)
     or changing the byte-compiler output (no thanks).]]

     The above comment is clearly wrong.  FSF does not do it this way
     and did not change the byte-compiler output.  Instead they use a
     `struct handler' to hold the various values (in place of our
     Vcondition_handlers) and chain them together, with pointers from
     the `struct catchtag' to the `struct handler'.  We should perhaps
     consider moving to something similar, but not before I merge my
     stderr-proc workspace, which contains changes to these
     functions. --ben */
  c.tag = noseeum_cons (noseeum_cons (Qunbound, handlers),
			Vcondition_handlers);
#endif
  c.val = Qnil;
  c.actual_tag = Qnil;
  c.backlist = backtrace_list;
#if 0 /* FSFmacs */
  /* #### */
  c.handlerlist = handlerlist;
#endif
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = specpdl_depth();
#if 0 /* FSFmacs */
  c.poll_suppress_count = async_timer_suppress_count;
#endif
  c.gcpro = gcprolist;
  /* #### FSFmacs does the following statement *after* the setjmp(). */
  c.next = catchlist;

  if (SETJMP (c.jmp))
    {
      /* throw does ungcpro, etc */
      return (*hfun) (c.val, harg);
    }

  record_unwind_protect (condition_case_unwind, c.tag);

  catchlist = &c;
#if 0 /* FSFmacs */
  h.handler = handlers;
  h.var = Qnil;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;
#else
  Vcondition_handlers = c.tag;
#endif
  GCPRO1 (harg);                /* Somebody has to gc-protect */
  c.val = ((*bfun) (barg));
  UNGCPRO;

  /* Once we change `catchlist' below, the stuff in c will not be GCPRO'd. */
  GCPRO3 (harg, c.val, c.tag);

  catchlist = c.next;
  check_catchlist_sanity ();
  /* Note: The unbind also resets Vcondition_handlers.  Maybe we should
     delete this here. */
  Vcondition_handlers = XCDR (c.tag);
  unbind_to (speccount);

  UNGCPRO;
  /* free the conses *after* the unbind, because the unbind will run
     condition_case_unwind above. */
  free_cons (XCAR (c.tag));
  free_cons (c.tag);
  return c.val;
}

static Lisp_Object
run_condition_case_handlers (Lisp_Object val, Lisp_Object var)
{
  /* This function can GC */
#if 0 /* FSFmacs */
  if (!NILP (h.var))
    specbind (h.var, c.val);
  val = Fprogn (Fcdr (h.chosen_clause));

  /* Note that this just undoes the binding of h.var; whoever
     longjmp()ed to us unwound the stack to c.pdlcount before
     throwing. */
  unbind_to (c.pdlcount);
  return val;
#else
  int speccount;

  CHECK_TRUE_LIST (val);
  if (NILP (var))
    return Fprogn (Fcdr (val)); /* tail call */

  speccount = specpdl_depth();
  specbind (var, Fcar (val));
  val = Fprogn (Fcdr (val));
  return unbind_to_1 (speccount, val);
#endif
}

/* Here for bytecode to call non-consfully.  This is exactly like
   condition-case except that it takes three arguments rather
   than a single list of arguments. */
Lisp_Object
condition_case_3 (Lisp_Object bodyform, Lisp_Object var, Lisp_Object handlers)
{
  /* This function can GC */
  EXTERNAL_LIST_LOOP_2 (handler, handlers)
    {
      if (NILP (handler))
	;
      else if (CONSP (handler))
	{
	  Lisp_Object conditions = XCAR (handler);
	  /* CONDITIONS must a condition name or a list of condition names */
	  if (SYMBOLP (conditions))
	    ;
	  else
	    {
	      EXTERNAL_LIST_LOOP_2 (condition, conditions)
		if (!SYMBOLP (condition))
		  goto invalid_condition_handler;
	    }
	}
      else
	{
	invalid_condition_handler:
	  sferror ("Invalid condition handler", handler);
	}
    }

  CHECK_SYMBOL (var);

  return condition_case_1 (handlers,
			   Feval, bodyform,
			   run_condition_case_handlers,
			   var);
}

DEFUN ("condition-case", Fcondition_case, 2, UNEVALLED, 0, /*
Regain control when an error is signalled.
Usage looks like (condition-case VAR BODYFORM HANDLERS...).
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
where the BODY is made of Lisp expressions.

A typical usage of `condition-case' looks like this:

(condition-case nil
    ;; you need a progn here if you want more than one statement ...
    (progn
      (do-something)
      (do-something-else))
  (error
   (issue-warning-or)
   ;; but strangely, you don't need one here.
   (return-a-value-etc)
   ))

A handler is applicable to an error if CONDITION-NAME is one of the
error's condition names.  If an error happens, the first applicable
handler is run.  As a special case, a CONDITION-NAME of t matches
all errors, even those without the `error' condition name on them
\(e.g. `quit').

The car of a handler may be a list of condition names
instead of a single condition name.

When a handler handles an error,
control returns to the condition-case and the handler BODY... is executed
with VAR bound to (SIGNALED-CONDITIONS . SIGNAL-DATA).
VAR may be nil; then you do not get access to the signal information.

The value of the last BODY form is returned from the condition-case.
See also the function `signal' for more info.

Note that at the time the condition handler is invoked, the Lisp stack
and the current catches, condition-cases, and bindings have all been
popped back to the state they were in just before the call to
`condition-case'.  This means that resignalling the error from
within the handler will not result in an infinite loop.

If you want to establish an error handler that is called with the
Lisp stack, bindings, etc. as they were when `signal' was called,
rather than when the handler was set, use `call-with-condition-handler'.
*/
     (args))
{
  /* This function can GC */
  Lisp_Object var = XCAR (args);
  Lisp_Object bodyform = XCAR (XCDR (args));
  Lisp_Object handlers = XCDR (XCDR (args));
  return condition_case_3 (bodyform, var, handlers);
}

DEFUN ("call-with-condition-handler", Fcall_with_condition_handler, 2, MANY, 0, /*
Regain control when an error is signalled, without popping the stack.
Usage looks like (call-with-condition-handler HANDLER FUNCTION &rest ARGS).
This function is similar to `condition-case', but the handler is invoked
with the same environment (Lisp stack, bindings, catches, condition-cases)
that was current when `signal' was called, rather than when the handler
was established.

HANDLER should be a function of one argument, which is a cons of the args
\(SIG . DATA) that were passed to `signal'.  It is invoked whenever
`signal' is called (this differs from `condition-case', which allows
you to specify which errors are trapped).  If the handler function
returns, `signal' continues as if the handler were never invoked.
\(It continues to look for handlers established earlier than this one,
and invokes the standard error-handler if none is found.)
*/
       (int nargs, Lisp_Object *args)) /* Note!  Args side-effected! */
{
  /* This function can GC */
  int speccount = specpdl_depth();
  Lisp_Object tem;

  tem = Ffunction_max_args (args[0]);
  if (! (XINT (Ffunction_min_args (args[0])) <= 1
	 && (NILP (tem) || 1 <= XINT (tem))))
    invalid_argument ("Must be function of one argument", args[0]);

  /* (handler-fun . handler-args)  but currently there are no handler-args */
  tem = noseeum_cons (list1 (args[0]), Vcondition_handlers);
  record_unwind_protect (condition_bind_unwind, tem);
  Vcondition_handlers = tem;

  /* Caller should have GC-protected args */
  return unbind_to_1 (speccount, Ffuncall (nargs - 1, args + 1));
}

/* This is the C version of the above function.  It calls FUN, passing it
   ARG, first setting up HANDLER to catch signals in the environment in
   which they were signalled. (HANDLER is only invoked if there was no
   handler (either from condition-case or call-with-condition-handler) set
   later on that handled the signal; therefore, this is a real error.

   HANDLER is invoked with three arguments: the ERROR-SYMBOL and DATA as
   passed to `signal', and HANDLER_ARG.  Originally I made HANDLER_ARG and
   ARG be void * to facilitate passing structures, but I changed to
   Lisp_Objects because all the other C interfaces to catch/condition-case/etc.
   take Lisp_Objects, and it is easy enough to use make_opaque_ptr() et al.
   to convert between Lisp_Objects and structure pointers. */

Lisp_Object
call_with_condition_handler (Lisp_Object (*handler) (Lisp_Object, Lisp_Object,
						     Lisp_Object),
			     Lisp_Object handler_arg,
			     Lisp_Object (*fun) (Lisp_Object),
			     Lisp_Object arg)
{
  /* This function can GC */
  int speccount = specpdl_depth ();
  Lisp_Object tem;

  /* ((handler-fun . (handler-arg . nil)) ... ) */
  tem = noseeum_cons (noseeum_cons (make_opaque_ptr ((void *) handler),
				    noseeum_cons (handler_arg, Qnil)),
		      Vcondition_handlers);
  record_unwind_protect (condition_bind_unwind, tem);
  Vcondition_handlers = tem;
  
  return unbind_to_1 (speccount, (*fun) (arg));
}

static int
condition_type_p (Lisp_Object type, Lisp_Object conditions)
{
  if (EQ (type, Qt))
    /* (condition-case c # (t c)) catches -all- signals
     *   Use with caution! */
    return 1;

  if (SYMBOLP (type))
    return !NILP (Fmemq (type, conditions));

  for (; CONSP (type); type = XCDR (type))
    if (!NILP (Fmemq (XCAR (type), conditions)))
      return 1;

  return 0;
}

static Lisp_Object
return_from_signal (Lisp_Object value)
{
#if 1
  /* Most callers are not prepared to handle gc if this
     returns.  So, since this feature is not very useful,
     take it out.  */
  /* Have called debugger; return value to signaller  */
  return value;
#else  /* But the reality is that that stinks, because: */
  /* GACK!!! Really want some way for debug-on-quit errors
     to be continuable!! */
  signal_error (Qunimplemented,
		"Returning a value from an error is no longer supported",
		Qunbound);
#endif
}


/************************************************************************/
/*		 the workhorse error-signaling function			*/
/************************************************************************/

/* This exists only for debugging purposes, as a place to put a breakpoint
   that won't get signalled for errors occurring when
   call_with_suspended_errors() was invoked. */

/* Don't make static or it might be compiled away */
void signal_1 (void);

void
signal_1 (void)
{
}

/* #### This function has not been synched with FSF.  It diverges
   significantly. */

/* The simplest external error function: it would be called
   signal_continuable_error() in the terminology below, but it's
   Lisp-callable. */

DEFUN ("signal", Fsignal, 2, 2, 0, /*
Signal a continuable error.  Args are ERROR-SYMBOL, and associated DATA.
An error symbol is a symbol defined using `define-error'.
DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See also the function `signal-error', and the functions to handle errors:
`condition-case' and `call-with-condition-handler'.

Note that this function can return, if the debugger is invoked and the
user invokes the "return from signal" option.
*/
       (error_symbol, data))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object conditions = Qnil;
  Lisp_Object handlers = Qnil;
  /* signal_call_debugger() could get called more than once
     (once when a call-with-condition-handler is about to
     be dealt with, and another when a condition-case handler
     is about to be invoked).  So make sure the debugger and/or
     stack trace aren't done more than once. */
  int stack_trace_displayed = 0;
  int debugger_entered = 0;

  /* Fsignal() is one of these functions that's called all the time
     with newly-created Lisp objects.  We allow this; but we must GC-
     protect the objects because all sorts of weird stuff could
     happen. */

  GCPRO4 (conditions, handlers, error_symbol, data);

  if (!(inhibit_flags & CALL_WITH_SUSPENDED_ERRORS))
    signal_1 ();

  if (!initialized)
    {
      /* who knows how much has been initialized?  Safest bet is
         just to bomb out immediately. */
      stderr_out ("Error before initialization is complete!\n");
      abort ();
    }

  assert (!gc_in_progress);

  /* We abort if in_display and we are not protected, as garbage
     collections and non-local exits will invariably be fatal, but in
     messy, difficult-to-debug ways.  See enter_redisplay_critical_section().
  */

#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
  check_proper_critical_section_nonlocal_exit_protection ();
#endif

  conditions = Fget (error_symbol, Qerror_conditions, Qnil);

  for (handlers = Vcondition_handlers;
       CONSP (handlers);
       handlers = XCDR (handlers))
    {
      Lisp_Object handler_fun = XCAR (XCAR (handlers));
      Lisp_Object handler_data = XCDR (XCAR (handlers));
      Lisp_Object outer_handlers = XCDR (handlers);

      if (!UNBOUNDP (handler_fun))
        {
          /* call-with-condition-handler */
          Lisp_Object tem;
          Lisp_Object all_handlers = Vcondition_handlers;
          struct gcpro ngcpro1;
          NGCPRO1 (all_handlers);
          Vcondition_handlers = outer_handlers;

          tem = signal_call_debugger (conditions, error_symbol, data,
				      outer_handlers, 1,
				      &stack_trace_displayed,
				      &debugger_entered);
          if (!UNBOUNDP (tem))
	    RETURN_NUNGCPRO (return_from_signal (tem));

	  if (OPAQUE_PTRP (handler_fun))
	    {
	      if (NILP (handler_data))
		{
		  Lisp_Object (*hfun) (Lisp_Object, Lisp_Object) =
		    (Lisp_Object (*) (Lisp_Object, Lisp_Object))
		    (get_opaque_ptr (handler_fun));

		  tem = (*hfun) (error_symbol, data);
		}
	      else
		{
		  Lisp_Object (*hfun) (Lisp_Object, Lisp_Object, Lisp_Object) =
		    (Lisp_Object (*) (Lisp_Object, Lisp_Object, Lisp_Object))
		    (get_opaque_ptr (handler_fun));

		  assert (NILP (XCDR (handler_data)));
		  tem = (*hfun) (error_symbol, data, XCAR (handler_data));
		}
	    }
	  else
	    {
	      tem = Fcons (error_symbol, data);
	      if (NILP (handler_data))
		tem = call1 (handler_fun, tem);
	      else
		{
		  /* (This code won't be used (for now?).) */
		  struct gcpro nngcpro1;
		  Lisp_Object args[3];
		  NNGCPRO1 (args[0]);
		  nngcpro1.nvars = 3;
		  args[0] = handler_fun;
		  args[1] = tem;
		  args[2] = handler_data;
		  nngcpro1.var = args;
		  tem = Fapply (3, args);
		  NNUNGCPRO;
		}
	    }
          NUNGCPRO;
#if 0
          if (!EQ (tem, Qsignal))
            return return_from_signal (tem);
#endif
          /* If handler didn't throw, try another handler */
          Vcondition_handlers = all_handlers;
        }

      /* It's a condition-case handler */

      /* t is used by handlers for all conditions, set up by C code.
       *  debugger is not called even if debug_on_error */
      else if (EQ (handler_data, Qt))
	{
          UNGCPRO;
          return Fthrow (handlers, Fcons (error_symbol, data));
	}
      /* `error' is used similarly to the way `t' is used, but in
         addition it invokes the debugger if debug_on_error.
	 This is normally used for the outer command-loop error
	 handler. */
      else if (EQ (handler_data, Qerror))
        {
          Lisp_Object tem = signal_call_debugger (conditions, error_symbol,
						  data,
                                                  outer_handlers, 0,
						  &stack_trace_displayed,
						  &debugger_entered);

          UNGCPRO;
          if (!UNBOUNDP (tem))
            return return_from_signal (tem);

          tem = Fcons (error_symbol, data);
          return Fthrow (handlers, tem);
        }
      else
	{
          /* handler established by real (Lisp) condition-case */
          Lisp_Object h;

	  for (h = handler_data; CONSP (h); h = Fcdr (h))
	    {
	      Lisp_Object clause = Fcar (h);
	      Lisp_Object tem = Fcar (clause);

	      if (condition_type_p (tem, conditions))
		{
		  tem = signal_call_debugger (conditions, error_symbol, data,
                                              outer_handlers, 1,
					      &stack_trace_displayed,
					      &debugger_entered);
                  UNGCPRO;
		  if (!UNBOUNDP (tem))
                    return return_from_signal (tem);

                  /* Doesn't return */
                  tem = Fcons (Fcons (error_symbol, data), Fcdr (clause));
                  return Fthrow (handlers, tem);
                }
	    }
	}
    }

  /* If no handler is present now, try to run the debugger,
     and if that fails, throw to top level.

     #### The only time that no handler is present is during
     temacs or perhaps very early in XEmacs.  In both cases,
     there is no 'top-level catch. (That's why the
     "bomb-out" hack was added.)

     [[#### Fix this horrifitude!]]

     I don't think this is horrifitude, but just defensive coding. --ben */

  signal_call_debugger (conditions, error_symbol, data, Qnil, 0,
			&stack_trace_displayed,
			&debugger_entered);
  UNGCPRO;
  throw_or_bomb_out (Qtop_level, Qt, 1, error_symbol,
		     data); /* Doesn't return */
  RETURN_NOT_REACHED (Qnil);
}

/****************** Error functions class 1 ******************/

/* Class 1: General functions that signal an error.
   These functions take an error type and a list of associated error
   data. */

/* No signal_continuable_error_1(); it's called Fsignal(). */

/* Signal a non-continuable error. */

DOESNT_RETURN
signal_error_1 (Lisp_Object sig, Lisp_Object data)
{
  for (;;)
    Fsignal (sig, data);
}

#ifdef ERROR_CHECK_CATCH

void
check_catchlist_sanity (void)
{
#if 0
  /* vou me tomar no cu!  i just masked andy's missing-unbind
     bug! */
  struct catchtag *c;
  int found_error_tag = 0;

  for (c = catchlist; c; c = c->next)
    {
      if (EQ (c->tag, Qunbound_suspended_errors_tag))
	{
	  found_error_tag = 1;
	  break;
	}
    }

  assert (found_error_tag || NILP (Vcurrent_error_state));
#endif /* vou me tomar no cul */
}

void
check_specbind_stack_sanity (void)
{
}

#endif /* ERROR_CHECK_CATCH */

/* Signal a non-continuable error or display a warning or do nothing,
   according to ERRB.  CLASS is the class of warning and should
   refer to what sort of operation is being done (e.g. Qtoolbar,
   Qresource, etc.). */

void
maybe_signal_error_1 (Lisp_Object sig, Lisp_Object data, Lisp_Object class_,
		      Error_Behavior errb)
{
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return;
  else if (ERRB_EQ (errb, ERROR_ME_DEBUG_WARN))
    warn_when_safe_lispobj (class_, Qdebug, Fcons (sig, data));
  else if (ERRB_EQ (errb, ERROR_ME_WARN))
    warn_when_safe_lispobj (class_, Qwarning, Fcons (sig, data));
  else
    for (;;)
      Fsignal (sig, data);
}

/* Signal a continuable error or display a warning or do nothing,
   according to ERRB. */

Lisp_Object
maybe_signal_continuable_error_1 (Lisp_Object sig, Lisp_Object data,
				  Lisp_Object class_, Error_Behavior errb)
{
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return Qnil;
  else if (ERRB_EQ (errb, ERROR_ME_DEBUG_WARN))
    {
      warn_when_safe_lispobj (class_, Qdebug, Fcons (sig, data));
      return Qnil;
    }
  else if (ERRB_EQ (errb, ERROR_ME_WARN))
    {
      warn_when_safe_lispobj (class_, Qwarning, Fcons (sig, data));
      return Qnil;
    }
  else
    return Fsignal (sig, data);
}


/****************** Error functions class 2 ******************/

/* Class 2: Signal an error with a string and an associated object.
   Normally these functions are used to attach one associated object,
   but to attach no objects, specify Qunbound for FROB, and for more
   than one object, make a list of the objects with Qunbound as the
   first element. (If you have specifically two objects to attach,
   consider using the function in class 3 below.) These functions
   signal an error of a specified type, whose data is one or more
   objects (usually two), a string the related Lisp object(s)
   specified as FROB. */

/* Out of REASON and FROB, return a list of elements suitable for passing
   to signal_error_1(). */

Lisp_Object
build_error_data (const CIbyte *reason, Lisp_Object frob)
{
  if (EQ (frob, Qunbound))
    frob = Qnil;
  else if (CONSP (frob) && EQ (XCAR (frob), Qunbound))
    frob = XCDR (frob);
  else
    frob = list1 (frob);
  if (!reason)
    return frob;
  else
    return Fcons (build_msg_string (reason), frob);
}

DOESNT_RETURN
signal_error (Lisp_Object type, const CIbyte *reason, Lisp_Object frob)
{
  signal_error_1 (type, build_error_data (reason, frob));
}

void
maybe_signal_error (Lisp_Object type, const CIbyte *reason,
		    Lisp_Object frob, Lisp_Object class_,
		    Error_Behavior errb)
{
  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return;
  maybe_signal_error_1 (type, build_error_data (reason, frob), class_, errb);
}

Lisp_Object
signal_continuable_error (Lisp_Object type, const CIbyte *reason,
			  Lisp_Object frob)
{
  return Fsignal (type, build_error_data (reason, frob));
}

Lisp_Object
maybe_signal_continuable_error (Lisp_Object type, const CIbyte *reason,
				Lisp_Object frob, Lisp_Object class_,
				Error_Behavior errb)
{
  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return Qnil;
  return maybe_signal_continuable_error_1 (type,
					   build_error_data (reason, frob),
					   class_, errb);
}


/****************** Error functions class 3 ******************/

/* Class 3: Signal an error with a string and two associated objects.
   These functions signal an error of a specified type, whose data
   is three objects, a string and two related Lisp objects.
   (The equivalent could be accomplished using the class 2 functions,
   but these are more convenient in this particular case.) */

DOESNT_RETURN
signal_error_2 (Lisp_Object type, const CIbyte *reason,
		Lisp_Object frob0, Lisp_Object frob1)
{
  signal_error_1 (type, list3 (build_msg_string (reason), frob0,
			       frob1));
}

void
maybe_signal_error_2 (Lisp_Object type, const CIbyte *reason,
		      Lisp_Object frob0, Lisp_Object frob1,
		      Lisp_Object class_, Error_Behavior errb)
{
  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return;
  maybe_signal_error_1 (type, list3 (build_msg_string (reason), frob0,
				     frob1), class_, errb);
}

Lisp_Object
signal_continuable_error_2 (Lisp_Object type, const CIbyte *reason,
			    Lisp_Object frob0, Lisp_Object frob1)
{
  return Fsignal (type, list3 (build_msg_string (reason), frob0,
			       frob1));
}

Lisp_Object
maybe_signal_continuable_error_2 (Lisp_Object type, const CIbyte *reason,
				  Lisp_Object frob0, Lisp_Object frob1,
				  Lisp_Object class_, Error_Behavior errb)
{
  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return Qnil;
  return maybe_signal_continuable_error_1
    (type, list3 (build_msg_string (reason), frob0, frob1),
     class_, errb);
}


/****************** Error functions class 4 ******************/

/* Class 4: Printf-like functions that signal an error.
   These functions signal an error of a specified type, whose data
   is a single string, created using the arguments. */

DOESNT_RETURN
signal_ferror (Lisp_Object type, const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  signal_error (type, 0, obj);
}

void
maybe_signal_ferror (Lisp_Object type, Lisp_Object class_, Error_Behavior errb,
		     const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  maybe_signal_error (type, 0, obj, class_, errb);
}

Lisp_Object
signal_continuable_ferror (Lisp_Object type, const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  return Fsignal (type, list1 (obj));
}

Lisp_Object
maybe_signal_continuable_ferror (Lisp_Object type, Lisp_Object class_,
				 Error_Behavior errb, const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return Qnil;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  return maybe_signal_continuable_error (type, 0, obj, class_, errb);
}


/****************** Error functions class 5 ******************/

/* Class 5: Printf-like functions that signal an error.
   These functions signal an error of a specified type, whose data
   is a one or more objects, a string (created using the arguments)
   and additional Lisp objects specified in FROB. (The syntax of FROB
   is the same as for class 2.)

   There is no need for a class 6 because you can always attach 2
   objects using class 5 (for FROB, specify a list with three
   elements, the first of which is Qunbound), and these functions are
   not commonly used.   
*/

DOESNT_RETURN
signal_ferror_with_frob (Lisp_Object type, Lisp_Object frob, const CIbyte *fmt,
			 ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  signal_error_1 (type, Fcons (obj, build_error_data (0, frob)));
}

void
maybe_signal_ferror_with_frob (Lisp_Object type, Lisp_Object frob,
			       Lisp_Object class_, Error_Behavior errb,
			       const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  maybe_signal_error_1 (type, Fcons (obj, build_error_data (0, frob)), class_,
			errb);
}

Lisp_Object
signal_continuable_ferror_with_frob (Lisp_Object type, Lisp_Object frob,
				     const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  return Fsignal (type, Fcons (obj, build_error_data (0, frob)));
}

Lisp_Object
maybe_signal_continuable_ferror_with_frob (Lisp_Object type, Lisp_Object frob,
					   Lisp_Object class_,
					   Error_Behavior errb,
					   const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  /* Optimization: */
  if (ERRB_EQ (errb, ERROR_ME_NOT))
    return Qnil;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  return maybe_signal_continuable_error_1 (type,
					   Fcons (obj,
						  build_error_data (0, frob)),
					   class_, errb);
}


/* This is what the QUIT macro calls to signal a quit */
void
signal_quit (void)
{
  /* This function cannot GC.  GC is prohibited because most callers do
     not expect GC occurring in QUIT.  Remove this if/when that gets fixed.
     --ben */

  int count;

  if (EQ (Vquit_flag, Qcritical))
    debug_on_quit |= 2;		/* set critical bit. */
  Vquit_flag = Qnil;
  count = begin_gc_forbidden ();
  /* note that this is continuable. */
  Fsignal (Qquit, Qnil);
  unbind_to (count);
}


/************************ convenience error functions ***********************/

Lisp_Object
signal_void_function_error (Lisp_Object function)
{
  return Fsignal (Qvoid_function, list1 (function));
}

Lisp_Object
signal_invalid_function_error (Lisp_Object function)
{
  return Fsignal (Qinvalid_function, list1 (function));
}

Lisp_Object
signal_wrong_number_of_arguments_error (Lisp_Object function, int nargs)
{
  return Fsignal (Qwrong_number_of_arguments,
		  list2 (function, make_int (nargs)));
}

/* Used in list traversal macros for efficiency. */
DOESNT_RETURN
signal_malformed_list_error (Lisp_Object list)
{
  signal_error (Qmalformed_list, 0, list);
}

DOESNT_RETURN
signal_malformed_property_list_error (Lisp_Object list)
{
  signal_error (Qmalformed_property_list, 0, list);
}

DOESNT_RETURN
signal_circular_list_error (Lisp_Object list)
{
  signal_error (Qcircular_list, 0, list);
}

DOESNT_RETURN
signal_circular_property_list_error (Lisp_Object list)
{
  signal_error (Qcircular_property_list, 0, list);
}

/* Called from within emacs_doprnt_1, so REASON is not formatted. */
DOESNT_RETURN
syntax_error (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qsyntax_error, reason, frob);
}

DOESNT_RETURN
syntax_error_2 (const CIbyte *reason, Lisp_Object frob1, Lisp_Object frob2)
{
  signal_error_2 (Qsyntax_error, reason, frob1, frob2);
}

void
maybe_syntax_error (const CIbyte *reason, Lisp_Object frob,
		    Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qsyntax_error, reason, frob, class_, errb);
}

DOESNT_RETURN
sferror (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qstructure_formation_error, reason, frob);
}

DOESNT_RETURN
sferror_2 (const CIbyte *reason, Lisp_Object frob1, Lisp_Object frob2)
{
  signal_error_2 (Qstructure_formation_error, reason, frob1, frob2);
}

void
maybe_sferror (const CIbyte *reason, Lisp_Object frob,
	       Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qstructure_formation_error, reason, frob, class_, errb);
}

DOESNT_RETURN
invalid_argument (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_argument, reason, frob);
}

DOESNT_RETURN
invalid_argument_2 (const CIbyte *reason, Lisp_Object frob1,
		    Lisp_Object frob2)
{
  signal_error_2 (Qinvalid_argument, reason, frob1, frob2);
}

void
maybe_invalid_argument (const CIbyte *reason, Lisp_Object frob,
			Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qinvalid_argument, reason, frob, class_, errb);
}

DOESNT_RETURN
invalid_constant (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_constant, reason, frob);
}

DOESNT_RETURN
invalid_constant_2 (const CIbyte *reason, Lisp_Object frob1,
		    Lisp_Object frob2)
{
  signal_error_2 (Qinvalid_constant, reason, frob1, frob2);
}

void
maybe_invalid_constant (const CIbyte *reason, Lisp_Object frob,
			Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qinvalid_constant, reason, frob, class_, errb);
}

DOESNT_RETURN
invalid_operation (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_operation, reason, frob);
}

DOESNT_RETURN
invalid_operation_2 (const CIbyte *reason, Lisp_Object frob1,
		     Lisp_Object frob2)
{
  signal_error_2 (Qinvalid_operation, reason, frob1, frob2);
}

void
maybe_invalid_operation (const CIbyte *reason, Lisp_Object frob,
			 Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qinvalid_operation, reason, frob, class_, errb);
}

DOESNT_RETURN
invalid_change (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_change, reason, frob);
}

DOESNT_RETURN
invalid_change_2 (const CIbyte *reason, Lisp_Object frob1, Lisp_Object frob2)
{
  signal_error_2 (Qinvalid_change, reason, frob1, frob2);
}

void
maybe_invalid_change (const CIbyte *reason, Lisp_Object frob,
		      Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qinvalid_change, reason, frob, class_, errb);
}

DOESNT_RETURN
invalid_state (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_state, reason, frob);
}

DOESNT_RETURN
invalid_state_2 (const CIbyte *reason, Lisp_Object frob1, Lisp_Object frob2)
{
  signal_error_2 (Qinvalid_state, reason, frob1, frob2);
}

void
maybe_invalid_state (const CIbyte *reason, Lisp_Object frob,
		     Lisp_Object class_, Error_Behavior errb)
{
  maybe_signal_error (Qinvalid_state, reason, frob, class_, errb);
}

DOESNT_RETURN
wtaerror (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qwrong_type_argument, reason, frob);
}

DOESNT_RETURN
stack_overflow (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qstack_overflow, reason, frob);
}

DOESNT_RETURN
out_of_memory (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qout_of_memory, reason, frob);
}

DOESNT_RETURN
printing_unreadable_object (const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  signal_error (Qprinting_unreadable_object, 0, obj);
}


/************************************************************************/
/*			      User commands				*/
/************************************************************************/

DEFUN ("commandp", Fcommandp, 1, 1, 0, /*
Return t if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include

-- strings and vectors (treated as keyboard macros)
-- lambda-expressions that contain a top-level call to `interactive'
-- autoload definitions made by `autoload' with non-nil fourth argument
   (i.e. the interactive flag)
-- compiled-function objects with a non-nil `compiled-function-interactive'
   value
-- subrs (built-in functions) that are interactively callable

Also, a symbol satisfies `commandp' if its function definition does so.
*/
       (function))
{
  Lisp_Object fun = indirect_function (function, 0);

  if (COMPILED_FUNCTIONP (fun))
    return XCOMPILED_FUNCTION (fun)->flags.interactivep ? Qt : Qnil;

  /* Lists may represent commands.  */
  if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qlambda))
	return Fassq (Qinteractive, Fcdr (Fcdr (fun)));
      if (EQ (funcar, Qautoload))
	return Fcar (Fcdr (Fcdr (Fcdr (fun))));
      else
	return Qnil;
    }

  /* Emacs primitives are interactive if their DEFUN specifies an
     interactive spec.  */
  if (SUBRP (fun))
    return XSUBR (fun)->prompt ? Qt : Qnil;

  /* Strings and vectors are keyboard macros.  */
  if (VECTORP (fun) || STRINGP (fun))
    return Qt;

  /* Everything else (including Qunbound) is not a command.  */
  return Qnil;
}

DEFUN ("command-execute", Fcommand_execute, 1, 3, 0, /*
Execute CMD as an editor command.
CMD must be an object that satisfies the `commandp' predicate.
Optional second arg RECORD-FLAG is as in `call-interactively'.
The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments.
*/
       (cmd, record_flag, keys))
{
  /* This function can GC */
  Lisp_Object prefixarg;
  Lisp_Object final = cmd;
  struct backtrace backtrace;
  struct console *con = XCONSOLE (Vselected_console);

  prefixarg = con->prefix_arg;
  con->prefix_arg = Qnil;
  Vcurrent_prefix_arg = prefixarg;
  debug_on_next_call = 0; /* #### from FSFmacs; correct? */

  if (SYMBOLP (cmd) && !NILP (Fget (cmd, Qdisabled, Qnil)))
    return run_hook (Qdisabled_command_hook);

  for (;;)
    {
      final = indirect_function (cmd, 1);
      if (CONSP (final) && EQ (Fcar (final), Qautoload))
	{
	  /* do_autoload GCPROs both arguments */
	  do_autoload (final, cmd);
	}
      else
	break;
    }

  if (CONSP (final) || SUBRP (final) || COMPILED_FUNCTIONP (final))
    {
      backtrace.function = &Qcall_interactively;
      backtrace.args = &cmd;
      backtrace.nargs = 1;
      backtrace.evalargs = 0;
      backtrace.pdlcount = specpdl_depth ();
      backtrace.debug_on_exit = 0;
      backtrace.function_being_called = 0;
      PUSH_BACKTRACE (backtrace);

      PROFILE_ENTER_FUNCTION ();
      final = Fcall_interactively (cmd, record_flag, keys);
      PROFILE_EXIT_FUNCTION ();

      POP_BACKTRACE (backtrace);
      return final;
    }
  else if (STRINGP (final) || VECTORP (final))
    {
      return Fexecute_kbd_macro (final, prefixarg);
    }
  else
    {
      Fsignal (Qwrong_type_argument,
	       Fcons (Qcommandp,
		      (EQ (cmd, final)
                       ? list1 (cmd)
                       : list2 (cmd, final))));
      return Qnil;
    }
}

DEFUN ("interactive-p", Finteractive_p, 0, 0, 0, /*
Return t if function in which this appears was called interactively.
This means that the function was called with call-interactively (which
includes being called as the binding of a key)
and input is currently coming from the keyboard (not in keyboard macro).
*/
       ())
{
  REGISTER struct backtrace *btp;
  REGISTER Lisp_Object fun;

  if (!INTERACTIVE)
    return Qnil;

  /*  Unless the object was compiled, skip the frame of interactive-p itself
      (if interpreted) or the frame of byte-code (if called from a compiled
      function).  Note that *btp->function may be a symbol pointing at a
      compiled function. */
  btp = backtrace_list;

#if 0 /* FSFmacs */

  /* #### FSFmacs does the following instead.  I can't figure
     out which one is more correct. */
  /* If this isn't a byte-compiled function, there may be a frame at
     the top for Finteractive_p itself.  If so, skip it.  */
  fun = Findirect_function (*btp->function);
  if (SUBRP (fun) && XSUBR (fun) == &Sinteractive_p)
    btp = btp->next;

  /* If we're running an Emacs 18-style byte-compiled function, there
     may be a frame for Fbyte_code.  Now, given the strictest
     definition, this function isn't really being called
     interactively, but because that's the way Emacs 18 always builds
     byte-compiled functions, we'll accept it for now.  */
  if (EQ (*btp->function, Qbyte_code))
    btp = btp->next;

  /* If this isn't a byte-compiled function, then we may now be
     looking at several frames for special forms.  Skip past them.  */
  while (btp &&
	 btp->nargs == UNEVALLED)
    btp = btp->next;

#else

  if (! (COMPILED_FUNCTIONP (Findirect_function (*btp->function))))
    btp = btp->next;
  for (;
       btp && (btp->nargs == UNEVALLED
	       || EQ (*btp->function, Qbyte_code));
       btp = btp->next)
    {}
  /* btp now points at the frame of the innermost function
     that DOES eval its args.
     If it is a built-in function (such as load or eval-region)
     return nil.  */
  /* Beats me why this is necessary, but it is */
  if (btp && EQ (*btp->function, Qcall_interactively))
    return Qt;

#endif

  fun = Findirect_function (*btp->function);
  if (SUBRP (fun))
    return Qnil;
  /* btp points to the frame of a Lisp function that called interactive-p.
     Return t if that function was called interactively.  */
  if (btp && btp->next && EQ (*btp->next->function, Qcall_interactively))
    return Qt;
  return Qnil;
}


/************************************************************************/
/*			      Autoloading				*/
/************************************************************************/

DEFUN ("autoload", Fautoload, 2, 5, 0, /*
Define FUNCTION to autoload from FILENAME.
FUNCTION is a symbol; FILENAME is a file name string to pass to `load'.
The remaining optional arguments provide additional info about the
real definition.
DOCSTRING is documentation for FUNCTION.
INTERACTIVE, if non-nil, says FUNCTION can be called interactively.
TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   `keymap' says FUNCTION is really a keymap, and
   `macro' or t says FUNCTION is really a macro.
If FUNCTION already has a non-void function definition that is not an
autoload object, this function does nothing and returns nil.
*/
       (function, filename, docstring, interactive, type))
{
  /* This function can GC */
  CHECK_SYMBOL (function);
  CHECK_STRING (filename);

  /* If function is defined and not as an autoload, don't override */
  {
    Lisp_Object f = XSYMBOL (function)->function;
    if (!UNBOUNDP (f) && !(CONSP (f) && EQ (XCAR (f), Qautoload)))
      return Qnil;
  }

  if (purify_flag)
    {
      /* Attempt to avoid consing identical (string=) pure strings. */
      filename = Fsymbol_name (Fintern (filename, Qnil));
    }

  return Ffset (function, Fcons (Qautoload, list4 (filename,
						   docstring,
						   interactive,
						   type)));
}

Lisp_Object
un_autoload (Lisp_Object oldqueue)
{
  /* This function can GC */
  REGISTER Lisp_Object queue, first, second;

  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      first = XCAR (queue);
      second = Fcdr (first);
      first = Fcar (first);
      if (NILP (second))
	Vfeatures = first;
      else
	Ffset (first, second);
      queue = Fcdr (queue);
    }
  return Qnil;
}

/* do_autoload GCPROs both arguments */
void
do_autoload (Lisp_Object fundef,
             Lisp_Object funname)
{
  /* This function can GC */
  int speccount = specpdl_depth();
  Lisp_Object fun = funname;
  struct gcpro gcpro1, gcpro2, gcpro3;

  CHECK_SYMBOL (funname);
  GCPRO3 (fundef, funname, fun);

  /* Value saved here is to be restored into Vautoload_queue */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  call4 (Qload, Fcar (Fcdr (fundef)), Qnil, noninteractive ? Qt : Qnil, Qnil);

  {
    Lisp_Object queue;

    /* Save the old autoloads, in case we ever do an unload. */
    for (queue = Vautoload_queue; CONSP (queue); queue = XCDR (queue))
      {
	Lisp_Object first  = XCAR (queue);
	Lisp_Object second = Fcdr (first);

	first = Fcar (first);

	/* Note: This test is subtle.  The cdr of an autoload-queue entry
	   may be an atom if the autoload entry was generated by a defalias
	   or fset. */
	if (CONSP (second))
	  Fput (first, Qautoload, (XCDR (second)));
      }
  }

  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (speccount);

  fun = indirect_function (fun, 0);

#if 0 /* FSFmacs */
  if (!NILP (Fequal (fun, fundef)))
#else
  if (UNBOUNDP (fun)
      || (CONSP (fun)
          && EQ (XCAR (fun), Qautoload)))
#endif
    invalid_state ("Autoloading failed to define function", funname);
  UNGCPRO;
}


/************************************************************************/
/*			   eval, funcall, apply				*/
/************************************************************************/

/* NOTE: If you are hearing the endless complaint that function calls in
   elisp are extremely slow, it just isn't true any more!  The stuff below
   -- in particular, the calling of subrs and compiled functions, the most
   common cases -- has been highly optimized.  There isn't a whole lot left
   to do to squeeze more speed out except by switching to lexical
   variables, which would eliminate the specbind loop. (But the real gain
   from lexical variables would come from better optimization -- with
   dynamic binding, you have the constant problem that any function call
   that you haven't explicitly proven to be side-effect-free might
   potentially side effect your local variables, which makes optimization
   extremely difficult when there are function calls anywhere in a chunk of
   code to be optimized.  Even worse, you don't know that *your* local
   variables aren't side-effecting an outer function's local variables, so
   it's impossible to optimize away almost *any* variable assignment.) */

static Lisp_Object funcall_lambda (Lisp_Object fun,
				   int nargs, Lisp_Object args[]);
static int in_warnings;


void handle_compiled_function_with_and_rest (Lisp_Compiled_Function *f,
					     int nargs,
					     Lisp_Object args[]);

/* The theory behind making this a separate function is to shrink
   funcall_compiled_function() so as to increase the likelihood of a cache
   hit in the L1 cache -- &rest processing is not going to be fast anyway.
   The idea is the same as with execute_rare_opcode() in bytecode.c.  We
   make this non-static to ensure the compiler doesn't inline it. */

void
handle_compiled_function_with_and_rest (Lisp_Compiled_Function *f, int nargs,
					Lisp_Object args[])
{
  REGISTER int i = 0;
  int max_non_rest_args = f->args_in_array - 1;
  int bindargs = min (nargs, max_non_rest_args);

  for (i = 0; i < bindargs; i++)
    SPECBIND_FAST_UNSAFE (f->args[i], args[i]);
  for (i = bindargs; i < max_non_rest_args; i++)
    SPECBIND_FAST_UNSAFE (f->args[i], Qnil);
  SPECBIND_FAST_UNSAFE
    (f->args[max_non_rest_args],
     nargs > max_non_rest_args ?
     Flist (nargs - max_non_rest_args, &args[max_non_rest_args]) :
     Qnil);
}

/* Apply compiled-function object FUN to the NARGS evaluated arguments
   in ARGS, and return the result of evaluation. */
inline static Lisp_Object
funcall_compiled_function (Lisp_Object fun, int nargs, Lisp_Object args[])
{
  /* This function can GC */
  int speccount = specpdl_depth();
  REGISTER int i = 0;
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);

  if (!OPAQUEP (f->instructions))
    /* Lazily munge the instructions into a more efficient form */
    optimize_compiled_function (fun);

  /* optimize_compiled_function() guaranteed that f->specpdl_depth is
     the required space on the specbinding stack for binding the args
     and local variables of fun.   So just reserve it once. */
  SPECPDL_RESERVE (f->specpdl_depth);

  if (nargs == f->max_args) /* Optimize for the common case -- no unspecified
			       optional arguments. */
    {
#if 1
      for (i = 0; i < nargs; i++)
	SPECBIND_FAST_UNSAFE (f->args[i], args[i]);
#else
      /* Here's an alternate way to write the loop that tries to further
         optimize funcalls for functions with few arguments by partially
         unrolling the loop.  It's not clear whether this is a win since it
         increases the size of the function and the possibility of L1 cache
         misses. (Microsoft VC++ 6 with /O2 /G5 generates 0x90 == 144 bytes
         per SPECBIND_FAST_UNSAFE().) Tests under VC++ 6, running the byte
         compiler repeatedly and looking at the total time, show very
         little difference between the simple loop above, the unrolled code
         below, and a "partly unrolled" solution with only cases 0-2 below
         instead of 0-4.  Therefore, I'm keeping it at the simple loop
         because it's smaller. */
      switch (nargs)
	{
	default:
	  for (i = nargs - 1; i >= 4; i--)
	    SPECBIND_FAST_UNSAFE (f->args[i], args[i]);
	case 4: SPECBIND_FAST_UNSAFE (f->args[3], args[3]);
	case 3: SPECBIND_FAST_UNSAFE (f->args[2], args[2]);
	case 2: SPECBIND_FAST_UNSAFE (f->args[1], args[1]);
	case 1: SPECBIND_FAST_UNSAFE (f->args[0], args[0]);
	case 0: break;
	}
#endif
    }
  else if (nargs < f->min_args)
    goto wrong_number_of_arguments;
  else if (nargs < f->max_args)
    {
      for (i = 0; i < nargs; i++)
	SPECBIND_FAST_UNSAFE (f->args[i], args[i]);
      for (i = nargs; i < f->max_args; i++)
	SPECBIND_FAST_UNSAFE (f->args[i], Qnil);
    }
  else if (f->max_args == MANY)
    handle_compiled_function_with_and_rest (f, nargs, args);
  else
    {
    wrong_number_of_arguments:
      /* The actual printed compiled_function object is incomprehensible.
	 Check the backtrace to see if we can get a more meaningful symbol. */
      if (EQ (fun, indirect_function (*backtrace_list->function, 0)))
	fun = *backtrace_list->function;
      return Fsignal (Qwrong_number_of_arguments,
		      list2 (fun, make_int (nargs)));
    }

  {
    Lisp_Object value =
      execute_optimized_program ((Opbyte *) XOPAQUE_DATA (f->instructions),
				 f->stack_depth,
				 XVECTOR_DATA (f->constants));

    /* The attempt to optimize this by only unbinding variables failed
       because using buffer-local variables as function parameters
       leads to specpdl_ptr->func != 0 */
    /* UNBIND_TO_GCPRO_VARIABLES_ONLY (speccount, value); */
    UNBIND_TO_GCPRO (speccount, value);
    return value;
  }
}

DEFUN ("eval", Feval, 1, 1, 0, /*
Evaluate FORM and return its value.
*/
       (form))
{
  /* This function can GC */
  Lisp_Object fun, val, original_fun, original_args;
  int nargs;
  struct backtrace backtrace;

#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
  check_proper_critical_section_lisp_protection ();
#endif

  /* I think this is a pretty safe place to call Lisp code, don't you? */
  while (!in_warnings && !NILP (Vpending_warnings)
	 /* well, perhaps not so safe after all! */
	 && !(inhibit_flags & INHIBIT_ANY_CHANGE_AFFECTING_REDISPLAY))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
      Lisp_Object this_warning_cons, this_warning, class_, level, messij;
      int speccount = internal_bind_int (&in_warnings, 1);

      this_warning_cons = Vpending_warnings;
      this_warning = XCAR (this_warning_cons);
      /* in case an error occurs in the warn function, at least
	 it won't happen infinitely */
      Vpending_warnings = XCDR (Vpending_warnings);
      free_cons (this_warning_cons);
      class_ = XCAR (this_warning);
      level = XCAR (XCDR (this_warning));
      messij = XCAR (XCDR (XCDR (this_warning)));
      free_list (this_warning);

      if (NILP (Vpending_warnings))
	Vpending_warnings_tail = Qnil; /* perhaps not strictly necessary,
					  but safer */

      GCPRO4 (form, class_, level, messij);
      if (!STRINGP (messij))
	messij = Fprin1_to_string (messij, Qnil);
      call3 (Qdisplay_warning, class_, messij, level);
      UNGCPRO;
      unbind_to (speccount);
    }

  if (!CONSP (form))
    {
      if (SYMBOLP (form))
	return Fsymbol_value (form);
      else
	return form;
    }

  QUIT;
  if (need_to_garbage_collect)
    {
      struct gcpro gcpro1;
      GCPRO1 (form);
      garbage_collect_1 ();
      UNGCPRO;
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	stack_overflow ("Lisp nesting exceeds `max-lisp-eval-depth'",
			Qunbound);
    }

  /* We guaranteed CONSP (form) above */
  original_fun  = XCAR (form);
  original_args = XCDR (form);

  GET_EXTERNAL_LIST_LENGTH (original_args, nargs);

  backtrace.pdlcount = specpdl_depth();
  backtrace.function = &original_fun; /* This also protects them from gc */
  backtrace.args = &original_args;
  backtrace.nargs = UNEVALLED;
  backtrace.evalargs = 1;
  backtrace.debug_on_exit = 0;
  backtrace.function_being_called = 0;
  PUSH_BACKTRACE (backtrace);

  if (debug_on_next_call)
    do_debug_on_call (Qt);

  /* At this point, only original_fun and original_args
     have values that will be used below. */
 retry:
  fun = indirect_function (original_fun, 1);

  if (SUBRP (fun))
    {
      Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;

      if (nargs < subr->min_args)
	goto wrong_number_of_arguments;

      if (max_args == UNEVALLED) /* Optimize for the common case */
	{
	  backtrace.evalargs = 0;
	  PROFILE_ENTER_FUNCTION ();
	  val = (((Lisp_Object (*) (Lisp_Object)) subr_function (subr))
		 (original_args));
	  PROFILE_EXIT_FUNCTION ();
	}
      else if (nargs <= max_args)
        {
          struct gcpro gcpro1;
	  Lisp_Object args[SUBR_MAX_ARGS];
	  REGISTER Lisp_Object *p = args;

	  GCPRO1 (args[0]);
	  gcpro1.nvars = 0;

	  {
	    LIST_LOOP_2 (arg, original_args)
	      {
		*p++ = Feval (arg);
		gcpro1.nvars++;
	      }
	  }

	  /* &optional args default to nil. */
	  while (p - args < max_args)
	    *p++ = Qnil;

          backtrace.args  = args;
          backtrace.nargs = nargs;

	  PROFILE_ENTER_FUNCTION ();
	  FUNCALL_SUBR (val, subr, args, max_args);
	  PROFILE_EXIT_FUNCTION ();

	  UNGCPRO;
        }
      else if (max_args == MANY)
	{
	  /* Pass a vector of evaluated arguments */
          struct gcpro gcpro1;
	  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
	  REGISTER Lisp_Object *p = args;

	  GCPRO1 (args[0]);
	  gcpro1.nvars = 0;

	  {
	    LIST_LOOP_2 (arg, original_args)
	      {
		*p++ = Feval (arg);
		gcpro1.nvars++;
	      }
	  }

	  backtrace.args  = args;
	  backtrace.nargs = nargs;

	  PROFILE_ENTER_FUNCTION ();
	  val = (((Lisp_Object (*) (int, Lisp_Object *)) subr_function (subr))
		 (nargs, args));
	  PROFILE_EXIT_FUNCTION ();

	  UNGCPRO;
	}
      else
	{
	wrong_number_of_arguments:
	  val = signal_wrong_number_of_arguments_error (original_fun, nargs);
	}
    }
  else if (COMPILED_FUNCTIONP (fun))
    {
      struct gcpro gcpro1;
      Lisp_Object *args = alloca_array (Lisp_Object, nargs);
      REGISTER Lisp_Object *p = args;

      GCPRO1 (args[0]);
      gcpro1.nvars = 0;

      {
	LIST_LOOP_2 (arg, original_args)
	  {
	    *p++ = Feval (arg);
	    gcpro1.nvars++;
	  }
      }

      backtrace.args     = args;
      backtrace.nargs    = nargs;
      backtrace.evalargs = 0;

      PROFILE_ENTER_FUNCTION ();
      val = funcall_compiled_function (fun, nargs, args);
      PROFILE_EXIT_FUNCTION ();

      /* Do the debug-on-exit now, while args is still GCPROed.  */
      if (backtrace.debug_on_exit)
	val = do_debug_on_exit (val);
      /* Don't do it again when we return to eval.  */
      backtrace.debug_on_exit = 0;

      UNGCPRO;
    }
  else if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);

      if (EQ (funcar, Qautoload))
	{
	  /* do_autoload GCPROs both arguments */
	  do_autoload (fun, original_fun);
	  goto retry;
	}
      else if (EQ (funcar, Qmacro))
	{
	  PROFILE_ENTER_FUNCTION ();
	  val = Feval (apply1 (XCDR (fun), original_args));
	  PROFILE_EXIT_FUNCTION ();
	}
      else if (EQ (funcar, Qlambda))
	{
	  struct gcpro gcpro1;
	  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
	  REGISTER Lisp_Object *p = args;

	  GCPRO1 (args[0]);
	  gcpro1.nvars = 0;

	  {
	    LIST_LOOP_2 (arg, original_args)
	      {
		*p++ = Feval (arg);
		gcpro1.nvars++;
	      }
	  }

	  UNGCPRO;

	  backtrace.args     = args; /* this also GCPROs `args' */
	  backtrace.nargs    = nargs;
	  backtrace.evalargs = 0;

	  PROFILE_ENTER_FUNCTION ();
	  val = funcall_lambda (fun, nargs, args);
	  PROFILE_EXIT_FUNCTION ();

	  /* Do the debug-on-exit now, while args is still GCPROed.  */
	  if (backtrace.debug_on_exit)
	    val = do_debug_on_exit (val);
	  /* Don't do it again when we return to eval.  */
	  backtrace.debug_on_exit = 0;
	}
      else
	{
	  goto invalid_function;
	}
    }
  else /* ! (SUBRP (fun) || COMPILED_FUNCTIONP (fun) || CONSP (fun)) */
    {
    invalid_function:
      val = signal_invalid_function_error (fun);
    }

  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = do_debug_on_exit (val);
  POP_BACKTRACE (backtrace);
  return val;
}



static void
run_post_gc_hook (void)
{
  Lisp_Object args[2];

  args[0] = Qpost_gc_hook;
  args[1] = Fcons (Fcons (Qfinalize_list, zap_finalize_list ()), Qnil);
  
  run_hook_with_args_trapping_problems
    (Qgarbage_collecting, 2, args, RUN_HOOKS_TO_COMPLETION,
     INHIBIT_QUIT | NO_INHIBIT_ERRORS);
}

DEFUN ("funcall", Ffuncall, 1, MANY, 0, /*
Call first argument as a function, passing the remaining arguments to it.
Thus, (funcall 'cons 'x 'y) returns (x . y).
*/
       (int nargs, Lisp_Object *args))
{
  /* This function can GC */
  Lisp_Object fun;
  Lisp_Object val;
  struct backtrace backtrace;
  int fun_nargs = nargs - 1;
  Lisp_Object *fun_args = args + 1;

  /* QUIT will check for proper redisplay wrapping */

  QUIT;

  if (funcall_allocation_flag)
    {
      if (need_to_garbage_collect)
	/* Callers should gcpro lexpr args */
	garbage_collect_1 ();
      if (need_to_check_c_alloca)
	{
	  if (++funcall_alloca_count >= MAX_FUNCALLS_BETWEEN_ALLOCA_CLEANUP)
	    {
	      xemacs_c_alloca (0);
	      funcall_alloca_count = 0;
	    }
	}
      if (need_to_signal_post_gc)
	{
	  need_to_signal_post_gc = 0;
	  recompute_funcall_allocation_flag ();
	  run_post_gc_hook ();
	}
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	stack_overflow ("Lisp nesting exceeds `max-lisp-eval-depth'",
			Qunbound);
    }

  backtrace.pdlcount = specpdl_depth ();
  backtrace.function = &args[0];
  backtrace.args  = fun_args;
  backtrace.nargs = fun_nargs;
  backtrace.evalargs = 0;
  backtrace.debug_on_exit = 0;
  backtrace.function_being_called = 0;
  PUSH_BACKTRACE (backtrace);

  if (debug_on_next_call)
    do_debug_on_call (Qlambda);

 retry:

  fun = args[0];

  /* We could call indirect_function directly, but profiling shows
     this is worth optimizing by partially unrolling the loop.  */
  if (SYMBOLP (fun))
    {
      fun = XSYMBOL (fun)->function;
      if (SYMBOLP (fun))
	{
	  fun = XSYMBOL (fun)->function;
	  if (SYMBOLP (fun))
	    fun = indirect_function (fun, 1);
	}
    }

  if (SUBRP (fun))
    {
      Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;
      Lisp_Object spacious_args[SUBR_MAX_ARGS];

      if (fun_nargs == max_args) /* Optimize for the common case */
	{
	funcall_subr:
	  PROFILE_ENTER_FUNCTION ();
	  FUNCALL_SUBR (val, subr, fun_args, max_args);
	  PROFILE_EXIT_FUNCTION ();
	}
      else if (fun_nargs < subr->min_args)
	{
	  goto wrong_number_of_arguments;
	}
      else if (fun_nargs < max_args)
	{
	  Lisp_Object *p = spacious_args;

          /* Default optionals to nil */
	  while (fun_nargs--)
	    *p++ = *fun_args++;
	  while (p - spacious_args < max_args)
	    *p++ = Qnil;

	  fun_args = spacious_args;
	  goto funcall_subr;
	}
      else if (max_args == MANY)
	{
	  PROFILE_ENTER_FUNCTION ();
	  val = SUBR_FUNCTION (subr, MANY) (fun_nargs, fun_args);
	  PROFILE_EXIT_FUNCTION ();
	}
      else if (max_args == UNEVALLED) /* Can't funcall a special form */
	{
	  goto invalid_function;
	}
      else
	{
	wrong_number_of_arguments:
	  val = signal_wrong_number_of_arguments_error (fun, fun_nargs);
	}
    }
  else if (COMPILED_FUNCTIONP (fun))
    {
      PROFILE_ENTER_FUNCTION ();
      val = funcall_compiled_function (fun, fun_nargs, fun_args);
      PROFILE_EXIT_FUNCTION ();
    }
  else if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);

      if (EQ (funcar, Qlambda))
	{
	  PROFILE_ENTER_FUNCTION ();
	  val = funcall_lambda (fun, fun_nargs, fun_args);
	  PROFILE_EXIT_FUNCTION ();
	}
      else if (EQ (funcar, Qautoload))
	{
	  /* do_autoload GCPROs both arguments */
	  do_autoload (fun, args[0]);
	  goto retry;
	}
      else /* Can't funcall a macro */
	{
	  goto invalid_function;
	}
    }
  else if (UNBOUNDP (fun))
    {
      val = signal_void_function_error (args[0]);
    }
  else
    {
    invalid_function:
      val = signal_invalid_function_error (fun);
    }

  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = do_debug_on_exit (val);
  POP_BACKTRACE (backtrace);
  return val;
}

DEFUN ("functionp", Ffunctionp, 1, 1, 0, /*
Return t if OBJECT can be called as a function, else nil.
A function is an object that can be applied to arguments,
using for example `funcall' or `apply'.
*/
       (object))
{
  if (SYMBOLP (object))
    object = indirect_function (object, 0);

  if (COMPILED_FUNCTIONP (object) || SUBRP (object))
    return Qt;
  if (CONSP (object))
    {
      Lisp_Object car = XCAR (object);
      if (EQ (car, Qlambda))
	return Qt;
      if (EQ (car, Qautoload)
	  && NILP (Fcar_safe (Fcdr_safe (Fcdr_safe (Fcdr_safe (XCDR (object)))))))
	return Qt;
    }
  return Qnil;
}

static Lisp_Object
function_argcount (Lisp_Object function, int function_min_args_p)
{
  Lisp_Object orig_function = function;
  Lisp_Object arglist;

 retry:

  if (SYMBOLP (function))
    function = indirect_function (function, 1);

  if (SUBRP (function))
    {
      /* Using return with the ?: operator tickles a DEC CC compiler bug. */
      if (function_min_args_p)
	return Fsubr_min_args (function);
      else
	return Fsubr_max_args (function);
   }
  else if (COMPILED_FUNCTIONP (function))
    {
      Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (function);
      
      if (!OPAQUEP (f->instructions))
	      /* Lazily munge the instructions into a more efficient form */
	      /* Needed to set max_args */
	      optimize_compiled_function (function);

      if (function_min_args_p)
	return make_int (f->min_args);
      else if (f->max_args == MANY)
	return Qnil;
      else
	return make_int (f->max_args);
    }
  else if (CONSP (function))
    {
      Lisp_Object funcar = XCAR (function);

      if (EQ (funcar, Qmacro))
	{
	  function = XCDR (function);
	  goto retry;
	}
      else if (EQ (funcar, Qautoload))
	{
	  /* do_autoload GCPROs both arguments */
	  do_autoload (function, orig_function);
	  function = orig_function;
	  goto retry;
	}
      else if (EQ (funcar, Qlambda))
	{
	  arglist = Fcar (XCDR (function));
	}
      else
	{
	  goto invalid_function;
	}
    }
  else
    {
    invalid_function:
      return signal_invalid_function_error (orig_function);
    }

  {
    int argcount = 0;

    EXTERNAL_LIST_LOOP_2 (arg, arglist)
      {
	if (EQ (arg, Qand_optional))
	  {
	    if (function_min_args_p)
	      break;
	  }
	else if (EQ (arg, Qand_rest))
	  {
	    if (function_min_args_p)
	      break;
	    else
	      return Qnil;
	  }
	else
	  {
	    argcount++;
	  }
      }

    return make_int (argcount);
  }
}

DEFUN ("function-min-args", Ffunction_min_args, 1, 1, 0, /*
Return the minimum number of arguments a function may be called with.
The function may be any form that can be passed to `funcall',
any special form, or any macro.

To check if a function can be called with a specified number of
arguments, use `function-allows-args'.
*/
       (function))
{
  return function_argcount (function, 1);
}

DEFUN ("function-max-args", Ffunction_max_args, 1, 1, 0, /*
Return the maximum number of arguments a function may be called with.
The function may be any form that can be passed to `funcall',
any special form, or any macro.
If the function takes an arbitrary number of arguments or is
a built-in special form, nil is returned.

To check if a function can be called with a specified number of
arguments, use `function-allows-args'.
*/
       (function))
{
  return function_argcount (function, 0);
}


DEFUN ("apply", Fapply, 2, MANY, 0, /*
Call FUNCTION with the remaining args, using the last arg as a list of args.
Thus, (apply '+ 1 2 '(3 4)) returns 10.
*/
       (int nargs, Lisp_Object *args))
{
  /* This function can GC */
  Lisp_Object fun = args[0];
  Lisp_Object spread_arg = args [nargs - 1];
  int numargs;
  int funcall_nargs;

  GET_EXTERNAL_LIST_LENGTH (spread_arg, numargs);

  if (numargs == 0)
    /* (apply foo 0 1 '()) */
    return Ffuncall (nargs - 1, args);
  else if (numargs == 1)
    {
      /* (apply foo 0 1 '(2)) */
      args [nargs - 1] = XCAR (spread_arg);
      return Ffuncall (nargs, args);
    }

  /* -1 for function, -1 for spread arg */
  numargs = nargs - 2 + numargs;
  /* +1 for function */
  funcall_nargs = 1 + numargs;

  if (SYMBOLP (fun))
    fun = indirect_function (fun, 0);

  if (SUBRP (fun))
    {
      Lisp_Subr *subr = XSUBR (fun);
      int max_args = subr->max_args;

      if (numargs < subr->min_args
	  || (max_args >= 0 && max_args < numargs))
        {
          /* Let funcall get the error */
        }
      else if (max_args > numargs)
	{
	  /* Avoid having funcall cons up yet another new vector of arguments
	     by explicitly supplying nil's for optional values */
          funcall_nargs += (max_args - numargs);
        }
    }
  else if (UNBOUNDP (fun))
    {
      /* Let funcall get the error */
      fun = args[0];
    }

  {
    REGISTER int i;
    Lisp_Object *funcall_args = alloca_array (Lisp_Object, funcall_nargs);
    struct gcpro gcpro1;

    GCPRO1 (*funcall_args);
    gcpro1.nvars = funcall_nargs;

    /* Copy in the unspread args */
    memcpy (funcall_args, args, (nargs - 1) * sizeof (Lisp_Object));
    /* Spread the last arg we got.  Its first element goes in
       the slot that it used to occupy, hence this value of I.  */
    for (i = nargs - 1;
         !NILP (spread_arg);    /* i < 1 + numargs */
         i++, spread_arg = XCDR (spread_arg))
      {
	funcall_args [i] = XCAR (spread_arg);
      }
    /* Supply nil for optional args (to subrs) */
    for (; i < funcall_nargs; i++)
      funcall_args[i] = Qnil;


    RETURN_UNGCPRO (Ffuncall (funcall_nargs, funcall_args));
  }
}


/* Apply lambda list FUN to the NARGS evaluated arguments in ARGS and
   return the result of evaluation. */

static Lisp_Object
funcall_lambda (Lisp_Object fun, int nargs, Lisp_Object args[])
{
  /* This function can GC */
  Lisp_Object arglist, body, tail;
  int speccount = specpdl_depth();
  REGISTER int i = 0;

  tail = XCDR (fun);

  if (!CONSP (tail))
    goto invalid_function;

  arglist = XCAR (tail);
  body    = XCDR (tail);

  {
    int optional = 0, rest = 0;

    EXTERNAL_LIST_LOOP_2 (symbol, arglist)
      {
	if (!SYMBOLP (symbol))
	  goto invalid_function;
	if (EQ (symbol, Qand_rest))
	  rest = 1;
	else if (EQ (symbol, Qand_optional))
	  optional = 1;
	else if (rest)
	  {
	    specbind (symbol, Flist (nargs - i, &args[i]));
	    i = nargs;
	  }
	else if (i < nargs)
	  specbind (symbol, args[i++]);
	else if (!optional)
	  goto wrong_number_of_arguments;
	else
	  specbind (symbol, Qnil);
      }
  }

  if (i < nargs)
    goto wrong_number_of_arguments;

  return unbind_to_1 (speccount, Fprogn (body));

 wrong_number_of_arguments:
  return signal_wrong_number_of_arguments_error (fun, nargs);

 invalid_function:
  return signal_invalid_function_error (fun);
}


/************************************************************************/
/*		     Run hook variables in various ways.		*/
/************************************************************************/

DEFUN ("run-hooks", Frun_hooks, 1, MANY, 0, /*
Run each hook in HOOKS.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
       (int nargs, Lisp_Object *args))
{
  REGISTER int i;

  for (i = 0; i < nargs; i++)
    run_hook_with_args (1, args + i, RUN_HOOKS_TO_COMPLETION);

  return Qnil;
}

DEFUN ("run-hook-with-args", Frun_hook_with_args, 1, MANY, 0, /*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value returned by `run-hook-with-args',
as that may change.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
       (int nargs, Lisp_Object *args))
{
  return run_hook_with_args (nargs, args, RUN_HOOKS_TO_COMPLETION);
}

DEFUN ("run-hook-with-args-until-success", Frun_hook_with_args_until_success, 1, MANY, 0, /*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns a non-nil value.  Then we return that value.
If all the functions return nil, we return nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
       (int nargs, Lisp_Object *args))
{
  return run_hook_with_args (nargs, args, RUN_HOOKS_UNTIL_SUCCESS);
}

DEFUN ("run-hook-with-args-until-failure", Frun_hook_with_args_until_failure, 1, MANY, 0, /*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns nil.  Then we return nil.
If all the functions return non-nil, we return non-nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
       (int nargs, Lisp_Object *args))
{
  return run_hook_with_args (nargs, args, RUN_HOOKS_UNTIL_FAILURE);
}

/* ARGS[0] should be a hook symbol.
   Call each of the functions in the hook value, passing each of them
   as arguments all the rest of ARGS (all NARGS - 1 elements).
   COND specifies a condition to test after each call
   to decide whether to stop.
   The caller (or its caller, etc) must gcpro all of ARGS,
   except that it isn't necessary to gcpro ARGS[0].  */

Lisp_Object
run_hook_with_args_in_buffer (struct buffer *buf, int nargs, Lisp_Object *args,
			      enum run_hooks_condition cond)
{
  Lisp_Object sym, val, ret;

  if (!initialized || preparing_for_armageddon)
    /* We need to bail out of here pronto. */
    return Qnil;

  /* Whenever gc_in_progress is true, preparing_for_armageddon
     will also be true unless something is really hosed. */
  assert (!gc_in_progress);

  sym = args[0];
  val = symbol_value_in_buffer (sym, wrap_buffer (buf));
  ret = (cond == RUN_HOOKS_UNTIL_FAILURE ? Qt : Qnil);

  if (UNBOUNDP (val) || NILP (val))
    return ret;
  else if (!CONSP (val) || EQ (XCAR (val), Qlambda))
    {
      args[0] = val;
      return Ffuncall (nargs, args);
    }
  else
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      Lisp_Object globals = Qnil;
      GCPRO3 (sym, val, globals);

      for (;
	   CONSP (val) && ((cond == RUN_HOOKS_TO_COMPLETION)
			   || (cond == RUN_HOOKS_UNTIL_SUCCESS ? NILP (ret)
			       : !NILP (ret)));
	   val = XCDR (val))
	{
	  if (EQ (XCAR (val), Qt))
	    {
	      /* t indicates this hook has a local binding;
		 it means to run the global binding too.  */
	      globals = Fdefault_value (sym);

	      if ((! CONSP (globals) || EQ (XCAR (globals), Qlambda)) &&
		  ! NILP (globals))
		{
		  args[0] = globals;
		  ret = Ffuncall (nargs, args);
		}
	      else
		{
		  for (;
		       CONSP (globals) && ((cond == RUN_HOOKS_TO_COMPLETION)
					   || (cond == RUN_HOOKS_UNTIL_SUCCESS
					       ? NILP (ret)
					       : !NILP (ret)));
		       globals = XCDR (globals))
		    {
		      args[0] = XCAR (globals);
		      /* In a global value, t should not occur.  If it does, we
			 must ignore it to avoid an endless loop.  */
		      if (!EQ (args[0], Qt))
			ret = Ffuncall (nargs, args);
		    }
		}
	    }
	  else
	    {
	      args[0] = XCAR (val);
	      ret = Ffuncall (nargs, args);
	    }
	}

      UNGCPRO;
      return ret;
    }
}

Lisp_Object
run_hook_with_args (int nargs, Lisp_Object *args,
		    enum run_hooks_condition cond)
{
  return run_hook_with_args_in_buffer (current_buffer, nargs, args, cond);
}

#if 0

/* From FSF 19.30, not currently used; seems like a big kludge. */

/* Run a hook symbol ARGS[0], but use FUNLIST instead of the actual
   present value of that symbol.
   Call each element of FUNLIST,
   passing each of them the rest of ARGS.
   The caller (or its caller, etc) must gcpro all of ARGS,
   except that it isn't necessary to gcpro ARGS[0].  */

Lisp_Object
run_hook_list_with_args (Lisp_Object funlist, int nargs, Lisp_Object *args)
{
  omitted;
}

#endif /* 0 */

void
va_run_hook_with_args (Lisp_Object hook_var, int nargs, ...)
{
  /* This function can GC */
  struct gcpro gcpro1;
  int i;
  va_list vargs;
  Lisp_Object *funcall_args = alloca_array (Lisp_Object, 1 + nargs);

  va_start (vargs, nargs);
  funcall_args[0] = hook_var;
  for (i = 0; i < nargs; i++)
    funcall_args[i + 1] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  GCPRO1 (*funcall_args);
  gcpro1.nvars = nargs + 1;
  run_hook_with_args (nargs + 1, funcall_args, RUN_HOOKS_TO_COMPLETION);
  UNGCPRO;
}

void
va_run_hook_with_args_in_buffer (struct buffer *buf, Lisp_Object hook_var,
				 int nargs, ...)
{
  /* This function can GC */
  struct gcpro gcpro1;
  int i;
  va_list vargs;
  Lisp_Object *funcall_args = alloca_array (Lisp_Object, 1 + nargs);

  va_start (vargs, nargs);
  funcall_args[0] = hook_var;
  for (i = 0; i < nargs; i++)
    funcall_args[i + 1] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  GCPRO1 (*funcall_args);
  gcpro1.nvars = nargs + 1;
  run_hook_with_args_in_buffer (buf, nargs + 1, funcall_args,
				RUN_HOOKS_TO_COMPLETION);
  UNGCPRO;
}

Lisp_Object
run_hook (Lisp_Object hook)
{
  return run_hook_with_args (1, &hook, RUN_HOOKS_TO_COMPLETION);
}


/************************************************************************/
/*		    Front-ends to eval, funcall, apply			*/
/************************************************************************/

/* Apply fn to arg */
Lisp_Object
apply1 (Lisp_Object fn, Lisp_Object arg)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[2];

  if (NILP (arg))
    return Ffuncall (1, &fn);
  GCPRO1 (args[0]);
  gcpro1.nvars = 2;
  args[0] = fn;
  args[1] = arg;
  RETURN_UNGCPRO (Fapply (2, args));
}

/* Call function fn on no arguments */
Lisp_Object
call0 (Lisp_Object fn)
{
  /* This function can GC */
  struct gcpro gcpro1;

  GCPRO1 (fn);
  RETURN_UNGCPRO (Ffuncall (1, &fn));
}

/* Call function fn with argument arg0 */
Lisp_Object
call1 (Lisp_Object fn,
       Lisp_Object arg0)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[2];
  args[0] = fn;
  args[1] = arg0;
  GCPRO1 (args[0]);
  gcpro1.nvars = 2;
  RETURN_UNGCPRO (Ffuncall (2, args));
}

/* Call function fn with arguments arg0, arg1 */
Lisp_Object
call2 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[3];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  GCPRO1 (args[0]);
  gcpro1.nvars = 3;
  RETURN_UNGCPRO (Ffuncall (3, args));
}

/* Call function fn with arguments arg0, arg1, arg2 */
Lisp_Object
call3 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[4];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  GCPRO1 (args[0]);
  gcpro1.nvars = 4;
  RETURN_UNGCPRO (Ffuncall (4, args));
}

/* Call function fn with arguments arg0, arg1, arg2, arg3 */
Lisp_Object
call4 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
       Lisp_Object arg3)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[5];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  GCPRO1 (args[0]);
  gcpro1.nvars = 5;
  RETURN_UNGCPRO (Ffuncall (5, args));
}

/* Call function fn with arguments arg0, arg1, arg2, arg3, arg4 */
Lisp_Object
call5 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
       Lisp_Object arg3, Lisp_Object arg4)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[6];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  args[5] = arg4;
  GCPRO1 (args[0]);
  gcpro1.nvars = 6;
  RETURN_UNGCPRO (Ffuncall (6, args));
}

Lisp_Object
call6 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
       Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[7];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  args[5] = arg4;
  args[6] = arg5;
  GCPRO1 (args[0]);
  gcpro1.nvars = 7;
  RETURN_UNGCPRO (Ffuncall (7, args));
}

Lisp_Object
call7 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
       Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5,
       Lisp_Object arg6)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[8];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  args[5] = arg4;
  args[6] = arg5;
  args[7] = arg6;
  GCPRO1 (args[0]);
  gcpro1.nvars = 8;
  RETURN_UNGCPRO (Ffuncall (8, args));
}

Lisp_Object
call8 (Lisp_Object fn,
       Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
       Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5,
       Lisp_Object arg6, Lisp_Object arg7)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object args[9];
  args[0] = fn;
  args[1] = arg0;
  args[2] = arg1;
  args[3] = arg2;
  args[4] = arg3;
  args[5] = arg4;
  args[6] = arg5;
  args[7] = arg6;
  args[8] = arg7;
  GCPRO1 (args[0]);
  gcpro1.nvars = 9;
  RETURN_UNGCPRO (Ffuncall (9, args));
}

Lisp_Object
call0_in_buffer (struct buffer *buf, Lisp_Object fn)
{
  if (current_buffer == buf)
    return call0 (fn);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = call0 (fn);
      unbind_to (speccount);
      return val;
    }
}

Lisp_Object
call1_in_buffer (struct buffer *buf, Lisp_Object fn,
		 Lisp_Object arg0)
{
  if (current_buffer == buf)
    return call1 (fn, arg0);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = call1 (fn, arg0);
      unbind_to (speccount);
      return val;
    }
}

Lisp_Object
call2_in_buffer (struct buffer *buf, Lisp_Object fn,
		 Lisp_Object arg0, Lisp_Object arg1)
{
  if (current_buffer == buf)
    return call2 (fn, arg0, arg1);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = call2 (fn, arg0, arg1);
      unbind_to (speccount);
      return val;
    }
}

Lisp_Object
call3_in_buffer (struct buffer *buf, Lisp_Object fn,
		 Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2)
{
  if (current_buffer == buf)
    return call3 (fn, arg0, arg1, arg2);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = call3 (fn, arg0, arg1, arg2);
      unbind_to (speccount);
      return val;
    }
}

Lisp_Object
call4_in_buffer (struct buffer *buf, Lisp_Object fn,
		 Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
		 Lisp_Object arg3)
{
  if (current_buffer == buf)
    return call4 (fn, arg0, arg1, arg2, arg3);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = call4 (fn, arg0, arg1, arg2, arg3);
      unbind_to (speccount);
      return val;
    }
}

Lisp_Object
eval_in_buffer (struct buffer *buf, Lisp_Object form)
{
  if (current_buffer == buf)
    return Feval (form);
  else
    {
      Lisp_Object val;
      int speccount = specpdl_depth();
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      set_buffer_internal (buf);
      val = Feval (form);
      unbind_to (speccount);
      return val;
    }
}


/************************************************************************/
/*	   Error-catching front-ends to eval, funcall, apply		*/
/************************************************************************/

int
get_inhibit_flags (void)
{
  return inhibit_flags;
}

void
check_allowed_operation (int what, Lisp_Object obj, Lisp_Object UNUSED (prop))
{
  if (inhibit_flags & INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION)
    {
      if (what == OPERATION_MODIFY_BUFFER_TEXT && BUFFERP (obj)
	  && NILP (memq_no_quit (obj, Vmodifiable_buffers)))
	invalid_change
	  ("Modification of this buffer not currently permitted", obj);
    }
  if (inhibit_flags & INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION)
    {
      if (what == OPERATION_DELETE_OBJECT
	  && (BUFFERP (obj) || WINDOWP (obj) || FRAMEP (obj) || DEVICEP (obj)
	      || CONSOLEP (obj))
	  && NILP (memq_no_quit (obj, Vdeletable_permanent_display_objects)))
	invalid_change
	  ("Deletion of this object not currently permitted", obj);
    }
}

void
note_object_created (Lisp_Object obj)
{
  if (inhibit_flags & INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION)
    {
      if (BUFFERP (obj))
	Vmodifiable_buffers = Fcons (obj, Vmodifiable_buffers);
    }
  if (inhibit_flags & INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION)
    {
      if (BUFFERP (obj) || WINDOWP (obj) || FRAMEP (obj) || DEVICEP (obj)
	  || CONSOLEP (obj))
	Vdeletable_permanent_display_objects =
	  Fcons (obj, Vdeletable_permanent_display_objects);
    }
}

void
note_object_deleted (Lisp_Object obj)
{
  if (inhibit_flags & INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION)
    {
      if (BUFFERP (obj))
	Vmodifiable_buffers = delq_no_quit (obj, Vmodifiable_buffers);
    }
  if (inhibit_flags & INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION)
    {
      if (BUFFERP (obj) || WINDOWP (obj) || FRAMEP (obj) || DEVICEP (obj)
	  || CONSOLEP (obj))
	Vdeletable_permanent_display_objects =
	  delq_no_quit (obj, Vdeletable_permanent_display_objects);
    }
}

struct call_trapping_problems
{
  Lisp_Object catchtag;
  Lisp_Object error_conditions;
  Lisp_Object data;
  Lisp_Object backtrace;
  Lisp_Object warning_class;

  const CIbyte *warning_string;
  Lisp_Object (*fun) (void *);
  void *arg;
};

static DECLARE_DOESNT_RETURN_TYPE
  (Lisp_Object, flagged_a_squirmer (Lisp_Object, Lisp_Object, Lisp_Object));

static DOESNT_RETURN_TYPE (Lisp_Object)
flagged_a_squirmer (Lisp_Object error_conditions, Lisp_Object data,
		    Lisp_Object opaque)
{
  struct call_trapping_problems *p =
    (struct call_trapping_problems *) get_opaque_ptr (opaque);

  if (!(inhibit_flags & INHIBIT_WARNING_ISSUE)
      && !warning_will_be_discarded (current_warning_level ()))
    {
      struct gcpro gcpro1;
      Lisp_Object lstream = Qnil;
      int speccount = specpdl_depth ();

      /* We're no longer protected against errors or quit here, so at
	 least let's temporarily inhibit quit.  We definitely do not
	 want to inhibit quit during the calling of the function
	 itself!!!!!!!!!!! */
     
      specbind (Qinhibit_quit, Qt);

      GCPRO1 (lstream);
      lstream = make_resizing_buffer_output_stream ();
      Fbacktrace (lstream, Qt);
      Lstream_flush (XLSTREAM (lstream));
      p->backtrace = resizing_buffer_to_lisp_string (XLSTREAM (lstream));
      Lstream_delete (XLSTREAM (lstream));
      UNGCPRO;

      unbind_to (speccount);
    }
  else
    p->backtrace = Qnil;

  p->error_conditions = error_conditions;
  p->data = data;

  Fthrow (p->catchtag, Qnil);
  RETURN_NOT_REACHED (Qnil);
}

static Lisp_Object
call_trapping_problems_2 (Lisp_Object opaque)
{
  struct call_trapping_problems *p =
    (struct call_trapping_problems *) get_opaque_ptr (opaque);

  return (p->fun) (p->arg);
}

static Lisp_Object
call_trapping_problems_1 (Lisp_Object opaque)
{
  return call_with_condition_handler (flagged_a_squirmer, opaque,
				      call_trapping_problems_2, opaque);
}

static void
issue_call_trapping_problems_warning (Lisp_Object warning_class,
				      const CIbyte *warning_string,
				      struct call_trapping_problems_result *p)
{
  if (!warning_will_be_discarded (current_warning_level ()))
    {
      int depth = specpdl_depth ();

      /* We're no longer protected against errors or quit here, so at
	 least let's temporarily inhibit quit. */
      specbind (Qinhibit_quit, Qt);

      if (p->caught_throw)
	{
	  Lisp_Object errstr =
	    emacs_sprintf_string_lisp
	    ("%s: Attempt to throw outside of function "
	     "to catch `%s' with value `%s'",
	     Qnil, 3,
	     build_msg_string (warning_string ? warning_string : "error"),
	     p->thrown_tag, p->thrown_value);
	  warn_when_safe_lispobj (Qerror, current_warning_level (), errstr);
	}
      else if (p->caught_error)
	{
	  Lisp_Object errstr;
	  /* #### This should call
	     (with-output-to-string (display-error (cons error_conditions
	                                                 data))
	     but that stuff is all in Lisp currently. */
	  errstr =
	    emacs_sprintf_string_lisp
	    ("%s: (%s %s)\n\nBacktrace follows:\n\n%s",
	     Qnil, 4,
	     build_msg_string (warning_string ? warning_string : "error"),
	     p->error_conditions, p->data, p->backtrace);

	  warn_when_safe_lispobj (warning_class, current_warning_level (),
				  errstr);
	}

      unbind_to (depth);
    }
}

/* Turn on the trapping flags in FLAGS -- see call_trapping_problems().
   This cannot handle INTERNAL_INHIBIT_THROWS() or INTERNAL_INHIBIT_ERRORS
   (because they ultimately boil down to a setjmp()!) -- you must directly
   use call_trapping_problems() for that.  Turn the flags off with
   unbind_to().  Returns the "canonicalized" flags (particularly in the
   case of INHIBIT_ANY_CHANGE_AFFECTING_REDISPLAY, which is shorthand for
   various other flags). */

int
set_trapping_problems_flags (int flags)
{
  int new_inhibit_flags;

  if (flags & INHIBIT_ANY_CHANGE_AFFECTING_REDISPLAY)
    flags |= INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION
      | INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION
      | INHIBIT_ENTERING_DEBUGGER
      | INHIBIT_WARNING_ISSUE
      | INHIBIT_GC;

  new_inhibit_flags = inhibit_flags | flags;
  if (new_inhibit_flags != inhibit_flags)
    internal_bind_int (&inhibit_flags, new_inhibit_flags);

  if (flags & INHIBIT_QUIT)
    specbind (Qinhibit_quit, Qt);

  if (flags & UNINHIBIT_QUIT)
    begin_do_check_for_quit ();

  if (flags & INHIBIT_GC)
    begin_gc_forbidden ();

  /* #### If we have nested calls to call_trapping_problems(), and the
     inner one creates some buffers/etc., should the outer one be able
     to delete them?  I think so, but it means we need to combine rather
     than just reset the value. */
  if (flags & INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION)
    internal_bind_lisp_object (&Vdeletable_permanent_display_objects, Qnil);

  if (flags & INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION)
    internal_bind_lisp_object (&Vmodifiable_buffers, Qnil);

  return flags;
}

/* This is equivalent to (*fun) (arg), except that various conditions
   can be trapped or inhibited, according to FLAGS.

   If FLAGS does not contain NO_INHIBIT_ERRORS, when an error occurs,
   the error is caught and a warning is issued, specifying the
   specific error that occurred and a backtrace.  In that case,
   WARNING_STRING should be given, and will be printed at the
   beginning of the error to indicate where the error occurred.

   If FLAGS does not contain NO_INHIBIT_THROWS, all attempts to
   `throw' out of the function being called are trapped, and a warning
   issued. (Again, WARNING_STRING should be given.)

   (If FLAGS contains INHIBIT_WARNING_ISSUE, no warnings are issued;
   this applies to recursive invocations of call_trapping_problems, too.

   If FLAGS contains POSTPONE_WARNING_ISSUE, no warnings are issued;
   but values useful for generating a warning are still computed (in
   particular, the backtrace), so that the calling function can issue
   a warning.

   If FLAGS contains ISSUE_WARNINGS_AT_DEBUG_LEVEL, warnings will be
   issued, but at level `debug', which normally is below the minimum
   specified by `log-warning-minimum-level', meaning such warnings will
   be ignored entirely.  The user can change this variable, however,
   to see the warnings.)

   Note: If neither of NO_INHIBIT_THROWS or NO_INHIBIT_ERRORS is
   given, you are *guaranteed* that there will be no non-local exits
   out of this function.

   If FLAGS contains INHIBIT_QUIT, QUIT using C-g is inhibited.  (This
   is *rarely* a good idea.  Unless you use NO_INHIBIT_ERRORS, QUIT is
   automatically caught as well, and treated as an error; you can
   check for this using EQ (problems->error_conditions, Qquit).

   If FLAGS contains UNINHIBIT_QUIT, QUIT checking will be explicitly
   turned on. (It will abort the code being called, but will still be
   trapped and reported as an error, unless NO_INHIBIT_ERRORS is
   given.) This is useful when QUIT checking has been turned off by a
   higher-level caller.

   If FLAGS contains INHIBIT_GC, garbage collection is inhibited.
   This is useful for Lisp called within redisplay, for example.

   If FLAGS contains INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION,
   Lisp code is not allowed to delete any window, buffers, frames, devices,
   or consoles that were already in existence at the time this function
   was called. (However, it's perfectly legal for code to create a new
   buffer and then delete it.)

   #### It might be useful to have a flag that inhibits deletion of a
   specific permanent display object and everything it's attached to
   (e.g. a window, and the buffer, frame, device, and console it's
   attached to.

   If FLAGS contains INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION, Lisp
   code is not allowed to modify the text of any buffers that were
   already in existence at the time this function was called.
   (However, it's perfectly legal for code to create a new buffer and
   then modify its text.)

       [These last two flags are implemented using global variables
       Vdeletable_permanent_display_objects and Vmodifiable_buffers,
       which keep track of a list of all buffers or permanent display
       objects created since the last time one of these flags was set.
       The code that deletes buffers, etc. and modifies buffers checks
    
       (1) if the corresponding flag is set (through the global variable
       inhibit_flags or its accessor function get_inhibit_flags()), and
    
       (2) if the object to be modified or deleted is not in the
       appropriate list.
    
       If so, it signals an error.
    
       Recursive calls to call_trapping_problems() are allowed.  In
       the case of the two flags mentioned above, the current values
       of the global variables are stored in an unwind-protect, and
       they're reset to nil.]
    
   If FLAGS contains INHIBIT_ENTERING_DEBUGGER, the debugger will not
   be entered if an error occurs inside the Lisp code being called,
   even when the user has requested an error.  In such case, a warning
   is issued stating that access to the debugger is denied, unless
   INHIBIT_WARNING_ISSUE has also been supplied.  This is useful when
   calling Lisp code inside redisplay, in menu callbacks, etc. because
   in such cases either the display is in an inconsistent state or
   doing window operations is explicitly forbidden by the OS, and the
   debugger would causes visual changes on the screen and might create
   another frame.

   If FLAGS contains INHIBIT_ANY_CHANGE_AFFECTING_REDISPLAY, no
   changes of any sort to extents, faces, glyphs, buffer text,
   specifiers relating to display, other variables relating to
   display, splitting, deleting, or resizing windows or frames,
   deleting buffers, windows, frames, devices, or consoles, etc. is
   allowed.  This is for things called absolutely in the middle of
   redisplay, which expects things to be *exactly* the same after the
   call as before.  This isn't completely implemented and needs to be
   thought out some more to determine exactly what its semantics are.
   For the moment, turning on this flag also turns on

        INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION
        INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION
        INHIBIT_ENTERING_DEBUGGER
        INHIBIT_WARNING_ISSUE
        INHIBIT_GC

   #### The following five flags are defined, but unimplemented:

   #define INHIBIT_EXISTING_CODING_SYSTEM_DELETION (1<<6)
   #define INHIBIT_EXISTING_CHARSET_DELETION (1<<7)
   #define INHIBIT_PERMANENT_DISPLAY_OBJECT_CREATION (1<<8)
   #define INHIBIT_CODING_SYSTEM_CREATION (1<<9)
   #define INHIBIT_CHARSET_CREATION (1<<10)

   FLAGS containing CALL_WITH_SUSPENDED_ERRORS is a sign that
   call_with_suspended_errors() was invoked.  This exists only for
   debugging purposes -- often we want to break when a signal happens,
   but ignore signals from call_with_suspended_errors(), because they
   occur often and for legitimate reasons.

   If PROBLEM is non-zero, it should be a pointer to a structure into
   which exact information about any occurring problems (either an
   error or an attempted throw past this boundary).

   If a problem occurred and aborted operation (error, quit, or
   invalid throw), Qunbound is returned.  Otherwise the return value
   from the call to (*fun) (arg) is returned.  */

Lisp_Object
call_trapping_problems (Lisp_Object warning_class,
			const CIbyte *warning_string,
			int flags,
			struct call_trapping_problems_result *problem,
			Lisp_Object (*fun) (void *),
			void *arg)
{
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  struct call_trapping_problems package;
  struct call_trapping_problems_result real_problem;
  Lisp_Object opaque, thrown_tag, tem;
  int thrown = 0;

  assert (SYMBOLP (warning_class)); /* sanity-check */
  assert (!NILP (warning_class));

  flags ^= INTERNAL_INHIBIT_ERRORS | INTERNAL_INHIBIT_THROWS;

  package.warning_class = warning_class;
  package.warning_string = warning_string;
  package.fun = fun;
  package.arg = arg;
  package.catchtag =
    flags & INTERNAL_INHIBIT_THROWS ? Vcatch_everything_tag :
    flags & INTERNAL_INHIBIT_ERRORS ? make_opaque_ptr (0) :
    Qnil;
  package.error_conditions = Qnil;
  package.data = Qnil;
  package.backtrace = Qnil;

  flags = set_trapping_problems_flags (flags);

  if (flags & (INTERNAL_INHIBIT_THROWS | INTERNAL_INHIBIT_ERRORS))
    opaque = make_opaque_ptr (&package);
  else
    opaque = Qnil;

  GCPRO5 (package.catchtag, package.error_conditions, package.data,
	  package.backtrace, opaque);

  if (flags & INTERNAL_INHIBIT_ERRORS)
    /* We need a catch so that our condition-handler can throw back here
       after printing the warning. (We print the warning in the stack
       context of the error, so we can get a backtrace.) */
    tem = internal_catch (package.catchtag, call_trapping_problems_1, opaque,
			  &thrown, &thrown_tag);
  else if (flags & INTERNAL_INHIBIT_THROWS)
    /* We skip over the first wrapper, which traps errors. */
    tem = internal_catch (package.catchtag, call_trapping_problems_2, opaque,
			  &thrown, &thrown_tag);
  else
    /* Nothing special. */
    tem = (fun) (arg);

  if (!problem)
    problem = &real_problem;

  if (!thrown)
    {
      problem->caught_error = 0;
      problem->caught_throw = 0;
      problem->error_conditions = Qnil;
      problem->data = Qnil;
      problem->backtrace = Qnil;
      problem->thrown_tag = Qnil;
      problem->thrown_value = Qnil;
    }
  else if (EQ (thrown_tag, package.catchtag))
    {
      problem->caught_error = 1;
      problem->caught_throw = 0;
      problem->error_conditions = package.error_conditions;
      problem->data = package.data;
      problem->backtrace = package.backtrace;
      problem->thrown_tag = Qnil;
      problem->thrown_value = Qnil;
    }
  else
    {
      problem->caught_error = 0;
      problem->caught_throw = 1;
      problem->error_conditions = Qnil;
      problem->data = Qnil;
      problem->backtrace = Qnil;
      problem->thrown_tag = thrown_tag;
      problem->thrown_value = tem;
    }

  if (!(flags & INHIBIT_WARNING_ISSUE) && !(flags & POSTPONE_WARNING_ISSUE))
    issue_call_trapping_problems_warning (warning_class, warning_string,
					  problem);

  if (!NILP (package.catchtag) &&
      !EQ (package.catchtag, Vcatch_everything_tag))
    free_opaque_ptr (package.catchtag);

  if (!NILP (opaque))
    free_opaque_ptr (opaque);

  unbind_to (speccount);
  RETURN_UNGCPRO (thrown ? Qunbound : tem);
}

struct va_call_trapping_problems
{
  lisp_fn_t fun;
  int nargs;
  Lisp_Object *args;
};

static Lisp_Object
va_call_trapping_problems_1 (void *ai_mi_madre)
{
  struct va_call_trapping_problems *ai_no_corrida =
    (struct va_call_trapping_problems *) ai_mi_madre;
  Lisp_Object pegar_no_bumbum;

  PRIMITIVE_FUNCALL (pegar_no_bumbum, ai_no_corrida->fun,
		     ai_no_corrida->args, ai_no_corrida->nargs);
  return pegar_no_bumbum;
}

/* #### document me. */

Lisp_Object
va_call_trapping_problems (Lisp_Object warning_class,
			   const CIbyte *warning_string,
			   int flags,
			   struct call_trapping_problems_result *problem,
			   lisp_fn_t fun, int nargs, ...)
{
  va_list vargs;
  Lisp_Object args[20];
  int i;
  struct va_call_trapping_problems fazer_invocacao_atrapalhando_problemas;
  struct gcpro gcpro1;

  assert (nargs >= 0 && nargs < 20);

  va_start (vargs, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  fazer_invocacao_atrapalhando_problemas.fun = fun;
  fazer_invocacao_atrapalhando_problemas.nargs = nargs;
  fazer_invocacao_atrapalhando_problemas.args = args;

  GCPRO1_ARRAY (args, nargs);
  RETURN_UNGCPRO
    (call_trapping_problems
     (warning_class, warning_string, flags, problem,
      va_call_trapping_problems_1, &fazer_invocacao_atrapalhando_problemas));
}

/* this is an older interface, barely different from
   va_call_trapping_problems.

   #### eliminate this or at least merge the ERROR_BEHAVIOR stuff into
   va_call_trapping_problems(). */

Lisp_Object
call_with_suspended_errors (lisp_fn_t fun, Lisp_Object retval,
			    Lisp_Object class_, Error_Behavior errb,
			    int nargs, ...)
{
  va_list vargs;
  Lisp_Object args[20];
  int i;
  struct va_call_trapping_problems fazer_invocacao_atrapalhando_problemas;
  int flags;
  struct gcpro gcpro1;

  assert (SYMBOLP (class_)); /* sanity-check */
  assert (!NILP (class_));
  assert (nargs >= 0 && nargs < 20);

  va_start (vargs, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (vargs, Lisp_Object);
  va_end (vargs);

  /* If error-checking is not disabled, just call the function. */

  if (ERRB_EQ (errb, ERROR_ME))
    {
      Lisp_Object val;
      PRIMITIVE_FUNCALL (val, fun, args, nargs);
      return val;
    }

  if (ERRB_EQ (errb, ERROR_ME_NOT)) /* person wants no warnings */
    flags = INHIBIT_WARNING_ISSUE | INHIBIT_ENTERING_DEBUGGER;
  else if (ERRB_EQ (errb, ERROR_ME_DEBUG_WARN))
    flags = ISSUE_WARNINGS_AT_DEBUG_LEVEL | INHIBIT_ENTERING_DEBUGGER;
  else
    {
      assert (ERRB_EQ (errb, ERROR_ME_WARN));
      flags = INHIBIT_ENTERING_DEBUGGER;
    }

  flags |= CALL_WITH_SUSPENDED_ERRORS;

  fazer_invocacao_atrapalhando_problemas.fun = fun;
  fazer_invocacao_atrapalhando_problemas.nargs = nargs;
  fazer_invocacao_atrapalhando_problemas.args = args;

  GCPRO1_ARRAY (args, nargs);
  {
    Lisp_Object its_way_too_goddamn_late =
      call_trapping_problems
       (class_, 0, flags, 0, va_call_trapping_problems_1,
	&fazer_invocacao_atrapalhando_problemas);
    UNGCPRO;
    if (UNBOUNDP (its_way_too_goddamn_late))
      return retval;
    else
      return its_way_too_goddamn_late;
  }
}

struct calln_trapping_problems
{
  int nargs;
  Lisp_Object *args;
};

static Lisp_Object
calln_trapping_problems_1 (void *puta)
{
  struct calln_trapping_problems *p = (struct calln_trapping_problems *) puta;

  return Ffuncall (p->nargs, p->args);
}

static Lisp_Object
calln_trapping_problems (Lisp_Object warning_class,
			 const CIbyte *warning_string, int flags,
			 struct call_trapping_problems_result *problem,
			 int nargs, Lisp_Object *args)
{
  struct calln_trapping_problems foo;
  struct gcpro gcpro1;

  if (SYMBOLP (args[0]))
    {
      Lisp_Object tem = XSYMBOL (args[0])->function;
      if (NILP (tem) || UNBOUNDP (tem))
	{
	  if (problem)
	    {
	      problem->caught_error = 0;
	      problem->caught_throw = 0;
	      problem->error_conditions = Qnil;
	      problem->data = Qnil;
	      problem->backtrace = Qnil;
	      problem->thrown_tag = Qnil;
	      problem->thrown_value = Qnil;
	    }
	  return Qnil;
	}
    }

  foo.nargs = nargs;
  foo.args = args;

  GCPRO1_ARRAY (args, nargs);
  RETURN_UNGCPRO (call_trapping_problems (warning_class, warning_string,
					  flags, problem,
					  calln_trapping_problems_1,
					  &foo));
}

/* #### fix these functions to follow the calling convention of
   call_trapping_problems! */

Lisp_Object
call0_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 int flags)
{
  return calln_trapping_problems (Qerror, warning_string, flags, 0, 1,
				  &function);
}

Lisp_Object
call1_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 Lisp_Object object, int flags)
{
  Lisp_Object args[2];

  args[0] = function;
  args[1] = object;

  return calln_trapping_problems (Qerror, warning_string, flags, 0, 2,
				  args);
}

Lisp_Object
call2_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 Lisp_Object object1, Lisp_Object object2,
			 int flags)
{
  Lisp_Object args[3];

  args[0] = function;
  args[1] = object1;
  args[2] = object2;

  return calln_trapping_problems (Qerror, warning_string, flags, 0, 3,
				  args);
}

Lisp_Object
call3_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 Lisp_Object object1, Lisp_Object object2,
			 Lisp_Object object3, int flags)
{
  Lisp_Object args[4];

  args[0] = function;
  args[1] = object1;
  args[2] = object2;
  args[3] = object3;

  return calln_trapping_problems (Qerror, warning_string, flags, 0, 4,
				  args);
}

Lisp_Object
call4_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 Lisp_Object object1, Lisp_Object object2,
			 Lisp_Object object3, Lisp_Object object4,
			 int flags)
{
  Lisp_Object args[5];

  args[0] = function;
  args[1] = object1;
  args[2] = object2;
  args[3] = object3;
  args[4] = object4;

  return calln_trapping_problems (Qerror, warning_string, flags, 0, 5,
				  args);
}

Lisp_Object
call5_trapping_problems (const CIbyte *warning_string, Lisp_Object function,
			 Lisp_Object object1, Lisp_Object object2,
			 Lisp_Object object3, Lisp_Object object4,
			 Lisp_Object object5, int flags)
{
  Lisp_Object args[6];

  args[0] = function;
  args[1] = object1;
  args[2] = object2;
  args[3] = object3;
  args[4] = object4;
  args[5] = object5;

  return calln_trapping_problems (Qerror, warning_string, flags, 0, 6,
				  args);
}

struct eval_in_buffer_trapping_problems
{
  struct buffer *buf;
  Lisp_Object form;
};

static Lisp_Object
eval_in_buffer_trapping_problems_1 (void *arg)
{
  struct eval_in_buffer_trapping_problems *p =
    (struct eval_in_buffer_trapping_problems *) arg;

  return eval_in_buffer (p->buf, p->form);
}

/* #### fix these functions to follow the calling convention of
   call_trapping_problems! */

Lisp_Object
eval_in_buffer_trapping_problems (const CIbyte *warning_string,
				  struct buffer *buf, Lisp_Object form,
				  int flags)
{
  struct eval_in_buffer_trapping_problems p;
  Lisp_Object buffer = wrap_buffer (buf);
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (buffer, form);
  p.buf = buf;
  p.form = form;
  RETURN_UNGCPRO (call_trapping_problems (Qerror, warning_string, flags, 0,
					  eval_in_buffer_trapping_problems_1,
					  &p));
}

Lisp_Object
run_hook_trapping_problems (Lisp_Object warning_class,
			    Lisp_Object hook_symbol,
			    int flags)
{
  return run_hook_with_args_trapping_problems (warning_class, 1, &hook_symbol,
					       RUN_HOOKS_TO_COMPLETION,
					       flags);
}

static Lisp_Object
safe_run_hook_trapping_problems_1 (void *puta)
{
  Lisp_Object hook = VOID_TO_LISP (puta);

  run_hook (hook);
  return Qnil;
}

/* Same as run_hook_trapping_problems() but also set the hook to nil
   if an error occurs (but not a quit). */

Lisp_Object
safe_run_hook_trapping_problems (Lisp_Object warning_class,
				 Lisp_Object hook_symbol, int flags)
{
  Lisp_Object tem;
  struct gcpro gcpro1, gcpro2;
  struct call_trapping_problems_result prob;

  if (!initialized || preparing_for_armageddon)
    return Qnil;
  tem = find_symbol_value (hook_symbol);
  if (NILP (tem) || UNBOUNDP (tem))
    return Qnil;

  GCPRO2 (hook_symbol, tem);
  tem = call_trapping_problems (Qerror, NULL,
				flags | POSTPONE_WARNING_ISSUE,
				&prob,
				safe_run_hook_trapping_problems_1,
				LISP_TO_VOID (hook_symbol));
  {
    Lisp_Object hook_name = XSYMBOL_NAME (hook_symbol);
    Ibyte *hook_str = XSTRING_DATA (hook_name);
    Ibyte *err = alloca_ibytes (XSTRING_LENGTH (hook_name) + 100);

    if (prob.caught_throw || (prob.caught_error && !EQ (prob.error_conditions,
							Qquit)))
      {
	Fset (hook_symbol, Qnil);
	qxesprintf (err, "Error in `%s' (resetting to nil)", hook_str);
      }
    else
      qxesprintf (err, "Quit in `%s'", hook_str);

  
    issue_call_trapping_problems_warning (warning_class, (CIbyte *) err,
					  &prob);
  }

  UNGCPRO;
  return tem;
}

struct run_hook_with_args_in_buffer_trapping_problems
{
  struct buffer *buf;
  int nargs;
  Lisp_Object *args;
  enum run_hooks_condition cond;
};

static Lisp_Object
run_hook_with_args_in_buffer_trapping_problems_1 (void *puta)
{
  struct run_hook_with_args_in_buffer_trapping_problems *porra =
    (struct run_hook_with_args_in_buffer_trapping_problems *) puta;

  return run_hook_with_args_in_buffer (porra->buf, porra->nargs, porra->args,
				       porra->cond);
}

/* #### fix these functions to follow the calling convention of
   call_trapping_problems! */

Lisp_Object
run_hook_with_args_in_buffer_trapping_problems (Lisp_Object warning_class,
						struct buffer *buf, int nargs,
						Lisp_Object *args,
						enum run_hooks_condition cond,
						int flags)
{
  Lisp_Object sym, val, ret;
  struct run_hook_with_args_in_buffer_trapping_problems diversity_and_distrust;
  struct gcpro gcpro1;
  Lisp_Object hook_name;
  Ibyte *hook_str;
  Ibyte *err;

  if (!initialized || preparing_for_armageddon)
    /* We need to bail out of here pronto. */
    return Qnil;

  GCPRO1_ARRAY (args, nargs);

  sym = args[0];
  val = symbol_value_in_buffer (sym, wrap_buffer (buf));
  ret = (cond == RUN_HOOKS_UNTIL_FAILURE ? Qt : Qnil);

  if (UNBOUNDP (val) || NILP (val))
    RETURN_UNGCPRO (ret);

  diversity_and_distrust.buf = buf;
  diversity_and_distrust.nargs = nargs;
  diversity_and_distrust.args = args;
  diversity_and_distrust.cond = cond;

  hook_name = XSYMBOL_NAME (args[0]);
  hook_str = XSTRING_DATA (hook_name);
  err = alloca_ibytes (XSTRING_LENGTH (hook_name) + 100);
  qxesprintf (err, "Error in `%s'", hook_str);
  RETURN_UNGCPRO
    (call_trapping_problems
     (warning_class, (CIbyte *) err, flags, 0,
      run_hook_with_args_in_buffer_trapping_problems_1,
      &diversity_and_distrust));
}

Lisp_Object
run_hook_with_args_trapping_problems (Lisp_Object warning_class,
				      int nargs,
				      Lisp_Object *args,
				      enum run_hooks_condition cond,
				      int flags)
{
  return run_hook_with_args_in_buffer_trapping_problems
    (warning_class, current_buffer, nargs, args, cond, flags);
}

Lisp_Object
va_run_hook_with_args_trapping_problems (Lisp_Object warning_class,
					 Lisp_Object hook_var,
					 int nargs, ...)
{
  /* This function can GC */
  struct gcpro gcpro1;
  int i;
  va_list vargs;
  Lisp_Object *funcall_args = alloca_array (Lisp_Object, 1 + nargs);
  int flags;

  va_start (vargs, nargs);
  funcall_args[0] = hook_var;
  for (i = 0; i < nargs; i++)
    funcall_args[i + 1] = va_arg (vargs, Lisp_Object);
  flags = va_arg (vargs, int);
  va_end (vargs);

  GCPRO1_ARRAY (funcall_args, nargs + 1);
  RETURN_UNGCPRO (run_hook_with_args_in_buffer_trapping_problems
		  (warning_class, current_buffer, nargs + 1, funcall_args,
		   RUN_HOOKS_TO_COMPLETION, flags));
}

Lisp_Object
va_run_hook_with_args_in_buffer_trapping_problems (Lisp_Object warning_class,
						   struct buffer *buf,
						   Lisp_Object hook_var,
						   int nargs, ...)
{
  /* This function can GC */
  struct gcpro gcpro1;
  int i;
  va_list vargs;
  Lisp_Object *funcall_args = alloca_array (Lisp_Object, 1 + nargs);
  int flags;

  va_start (vargs, nargs);
  funcall_args[0] = hook_var;
  for (i = 0; i < nargs; i++)
    funcall_args[i + 1] = va_arg (vargs, Lisp_Object);
  flags = va_arg (vargs, int);
  va_end (vargs);

  GCPRO1_ARRAY (funcall_args, nargs + 1);
  RETURN_UNGCPRO (run_hook_with_args_in_buffer_trapping_problems
		  (warning_class, buf, nargs + 1, funcall_args,
		   RUN_HOOKS_TO_COMPLETION, flags));
}


/************************************************************************/
/*		       The special binding stack			*/
/* Most C code should simply use specbind() and unbind_to_1().		*/
/* When performance is critical, use the macros in backtrace.h.		*/
/************************************************************************/

#define min_max_specpdl_size 400

void
grow_specpdl (EMACS_INT reserved)
{
  EMACS_INT size_needed = specpdl_depth() + reserved;
  if (size_needed >= max_specpdl_size)
    {
      if (max_specpdl_size < min_max_specpdl_size)
	max_specpdl_size = min_max_specpdl_size;
      if (size_needed >= max_specpdl_size)
	{
	  /* Leave room for some specpdl in the debugger. */
	  max_specpdl_size = size_needed + 100;
	  if (max_specpdl_size > specpdl_size)
	    {
	      specpdl_size = max_specpdl_size;
	      XREALLOC_ARRAY (specpdl, struct specbinding, specpdl_size);
	      specpdl_ptr = specpdl + specpdl_depth();
	    }
	  signal_continuable_error
	    (Qstack_overflow,
	     "Variable binding depth exceeds max-specpdl-size", Qunbound);
	}
    }
  while (specpdl_size < size_needed)
    {
      specpdl_size *= 2;
      if (specpdl_size > max_specpdl_size)
	specpdl_size = max_specpdl_size;
    }
  XREALLOC_ARRAY (specpdl, struct specbinding, specpdl_size);
  specpdl_ptr = specpdl + specpdl_depth();
  check_specbind_stack_sanity ();
}


/* Handle unbinding buffer-local variables */
static Lisp_Object
specbind_unwind_local (Lisp_Object ovalue)
{
  Lisp_Object current = Fcurrent_buffer ();
  Lisp_Object symbol = specpdl_ptr->symbol;
  Lisp_Object victim = ovalue;
  Lisp_Object buf = get_buffer (XCAR (victim), 0);
  ovalue = XCDR (victim);

  free_cons (victim);

  if (NILP (buf))
    {
      /* Deleted buffer -- do nothing */
    }
  else if (symbol_value_buffer_local_info (symbol, XBUFFER (buf)) == 0)
    {
      /* Was buffer-local when binding was made, now no longer is.
       *  (kill-local-variable can do this.)
       * Do nothing in this case.
       */
    }
  else if (EQ (buf, current))
    Fset (symbol, ovalue);
  else
  {
    /* Urk! Somebody switched buffers */
    struct gcpro gcpro1;
    GCPRO1 (current);
    Fset_buffer (buf);
    Fset (symbol, ovalue);
    Fset_buffer (current);
    UNGCPRO;
  }
  return symbol;
}

static Lisp_Object
specbind_unwind_wasnt_local (Lisp_Object buffer)
{
  Lisp_Object current = Fcurrent_buffer ();
  Lisp_Object symbol = specpdl_ptr->symbol;

  buffer = get_buffer (buffer, 0);
  if (NILP (buffer))
    {
      /* Deleted buffer -- do nothing */
    }
  else if (symbol_value_buffer_local_info (symbol, XBUFFER (buffer)) == 0)
    {
      /* Was buffer-local when binding was made, now no longer is.
       *  (kill-local-variable can do this.)
       * Do nothing in this case.
       */
    }
  else if (EQ (buffer, current))
    Fkill_local_variable (symbol);
  else
    {
      /* Urk! Somebody switched buffers */
      struct gcpro gcpro1;
      GCPRO1 (current);
      Fset_buffer (buffer);
      Fkill_local_variable (symbol);
      Fset_buffer (current);
      UNGCPRO;
    }
  return symbol;
}


void
specbind (Lisp_Object symbol, Lisp_Object value)
{
  SPECBIND (symbol, value);

  check_specbind_stack_sanity ();
}

void
specbind_magic (Lisp_Object symbol, Lisp_Object value)
{
  int buffer_local =
    symbol_value_buffer_local_info (symbol, current_buffer);

  if (buffer_local == 0)
    {
      specpdl_ptr->old_value = find_symbol_value (symbol);
      specpdl_ptr->func = 0;      /* Handled specially by unbind_to_1 */
    }
  else if (buffer_local > 0)
    {
      /* Already buffer-local */
      specpdl_ptr->old_value = noseeum_cons (Fcurrent_buffer (),
					     find_symbol_value (symbol));
      specpdl_ptr->func = specbind_unwind_local;
    }
  else
    {
      /* About to become buffer-local */
      specpdl_ptr->old_value = Fcurrent_buffer ();
      specpdl_ptr->func = specbind_unwind_wasnt_local;
    }

  specpdl_ptr->symbol = symbol;
  specpdl_ptr++;
  specpdl_depth_counter++;

  Fset (symbol, value);

  check_specbind_stack_sanity ();
}

/* Record an unwind-protect -- FUNCTION will be called with ARG no matter
   whether a normal or non-local exit occurs. (You need to call unbind_to_1()
   before your function returns normally, passing in the integer returned
   by this function.) Note: As long as the unwind-protect exists, ARG is
   automatically GCPRO'd.  The return value from FUNCTION is completely
   ignored. #### We should eliminate it entirely. */

int
record_unwind_protect (Lisp_Object (*function) (Lisp_Object arg),
                       Lisp_Object arg)
{
  SPECPDL_RESERVE (1);
  specpdl_ptr->func = function;
  specpdl_ptr->symbol = Qnil;
  specpdl_ptr->old_value = arg;
  specpdl_ptr++;
  specpdl_depth_counter++;
  check_specbind_stack_sanity ();
  return specpdl_depth_counter - 1;
}

static Lisp_Object
restore_lisp_object (Lisp_Object cons)
{
  Lisp_Object opaque = XCAR (cons);
  Lisp_Object *addr = (Lisp_Object *) get_opaque_ptr (opaque);
  *addr = XCDR (cons);
  free_opaque_ptr (opaque);
  free_cons (cons);
  return Qnil;
}

/* Establish an unwind-protect which will restore the Lisp_Object pointed to
   by ADDR with the value VAL. */
static int
record_unwind_protect_restoring_lisp_object (Lisp_Object *addr,
					     Lisp_Object val)
{
  Lisp_Object opaque = make_opaque_ptr (addr);
  return record_unwind_protect (restore_lisp_object,
				noseeum_cons (opaque, val));
}

/* Similar to specbind() but for any C variable whose value is a
   Lisp_Object.  Sets up an unwind-protect to restore the variable
   pointed to by ADDR to its existing value, and then changes its
   value to NEWVAL.  Returns the previous value of specpdl_depth();
   pass this to unbind_to() after you are done. */
int
internal_bind_lisp_object (Lisp_Object *addr, Lisp_Object newval)
{
  int count = specpdl_depth ();
  record_unwind_protect_restoring_lisp_object (addr, *addr);
  *addr = newval;
  return count;
}

static Lisp_Object
restore_int (Lisp_Object cons)
{
  Lisp_Object opaque = XCAR (cons);
  Lisp_Object lval = XCDR (cons);
  int *addr = (int *) get_opaque_ptr (opaque);
  int val;

  if (INTP (lval))
    val = XINT (lval);
  else
    {
      val = (int) get_opaque_ptr (lval);
      free_opaque_ptr (lval);
    }

  *addr = val;
  free_opaque_ptr (opaque);
  free_cons (cons);
  return Qnil;
}

/* Establish an unwind-protect which will restore the int pointed to
   by ADDR with the value VAL.  This function works correctly with
   all ints, even those that don't fit into a Lisp integer. */
int
record_unwind_protect_restoring_int (int *addr, int val)
{
  Lisp_Object opaque = make_opaque_ptr (addr);
  Lisp_Object lval;

  if (NUMBER_FITS_IN_AN_EMACS_INT (val))
    lval = make_int (val);
  else
    lval = make_opaque_ptr ((void *) val);
  return record_unwind_protect (restore_int, noseeum_cons (opaque, lval));
}

/* Similar to specbind() but for any C variable whose value is an int.
   Sets up an unwind-protect to restore the variable pointed to by
   ADDR to its existing value, and then changes its value to NEWVAL.
   Returns the previous value of specpdl_depth(); pass this to
   unbind_to() after you are done.  This function works correctly with
   all ints, even those that don't fit into a Lisp integer. */
int
internal_bind_int (int *addr, int newval)
{
  int count = specpdl_depth ();
  record_unwind_protect_restoring_int (addr, *addr);
  *addr = newval;
  return count;
}

static Lisp_Object
free_pointer (Lisp_Object opaque)
{
  xfree (get_opaque_ptr (opaque), void *);
  free_opaque_ptr (opaque);
  return Qnil;
}

/* Establish an unwind-protect which will free the specified block.
 */
int
record_unwind_protect_freeing (void *ptr)
{
  Lisp_Object opaque = make_opaque_ptr (ptr);
  return record_unwind_protect (free_pointer, opaque);
}

static Lisp_Object
free_dynarr (Lisp_Object opaque)
{
  Dynarr_free (get_opaque_ptr (opaque));
  free_opaque_ptr (opaque);
  return Qnil;
}

int
record_unwind_protect_freeing_dynarr (void *ptr)
{
  Lisp_Object opaque = make_opaque_ptr (ptr);
  return record_unwind_protect (free_dynarr, opaque);
}

/* Unwind the stack till specpdl_depth() == COUNT.
   VALUE is not used, except that, purely as a convenience to the
   caller, it is protected from garbage-protection and returned. */
Lisp_Object
unbind_to_1 (int count, Lisp_Object value)
{
  UNBIND_TO_GCPRO (count, value);
  check_specbind_stack_sanity ();
  return value;
}

/* Don't call this directly.
   Only for use by UNBIND_TO* macros in backtrace.h */
void
unbind_to_hairy (int count)
{
  ++specpdl_ptr;
  ++specpdl_depth_counter;

  while (specpdl_depth_counter != count)
    {
      Lisp_Object oquit = Qunbound;

      /* Do this check BEFORE decrementing the values below, because once
	 they're decremented, GC protection is lost on
	 specpdl_ptr->old_value. */
      if (specpdl_ptr[-1].func == Fprogn)
	{
	  /* Allow QUIT within unwind-protect routines, but defer any
	     existing QUIT until afterwards.  Only do this, however, for
	     unwind-protects established by Lisp code, not by C code
	     (e.g. free_opaque_ptr() or something), because the act of
	     checking for QUIT can cause all sorts of weird things to
	     happen, since it churns the event loop -- redisplay, running
	     Lisp, etc.  Code should not have to worry about this just
	     because of establishing an unwind-protect. */
	  check_quit (); /* make Vquit_flag accurate */
	  oquit = Vquit_flag;
	  Vquit_flag = Qnil;
	}

      --specpdl_ptr;
      --specpdl_depth_counter;

      /* #### At this point, there is no GC protection on old_value.  This
	 could be a real problem, depending on what unwind-protect function
	 is called.  It looks like it just so happens that the ones
	 actually called don't have a problem with this, e.g. Fprogn.  But
	 we should look into fixing this. (Many unwind-protect functions
	 free values.  Is it a problem if freed values are
	 GC-protected?) */
      if (specpdl_ptr->func != 0)
	{
	  /* An unwind-protect */
	  (*specpdl_ptr->func) (specpdl_ptr->old_value);
	}
	  
      else
	{
	  /* We checked symbol for validity when we specbound it,
	     so only need to call Fset if symbol has magic value.  */
	  Lisp_Symbol *sym = XSYMBOL (specpdl_ptr->symbol);
	  if (!SYMBOL_VALUE_MAGIC_P (sym->value))
	    sym->value = specpdl_ptr->old_value;
	  else
	    Fset (specpdl_ptr->symbol, specpdl_ptr->old_value);
	}

#if 0 /* martin */
#ifndef EXCEEDINGLY_QUESTIONABLE_CODE
      /* There should never be anything here for us to remove.
	 If so, it indicates a logic error in Emacs.  Catches
	 should get removed when a throw or signal occurs, or
	 when a catch or condition-case exits normally.  But
	 it's too dangerous to just remove this code. --ben */

      /* Furthermore, this code is not in FSFmacs!!!
	 Braino on mly's part? */
      /* If we're unwound past the pdlcount of a catch frame,
         that catch can't possibly still be valid. */
      while (catchlist && catchlist->pdlcount > specpdl_depth_counter)
        {
          catchlist = catchlist->next;
          /* Don't mess with gcprolist, backtrace_list here */
        }
#endif
#endif

      if (!UNBOUNDP (oquit))
	Vquit_flag = oquit;
    }
  check_specbind_stack_sanity ();
}



/* Get the value of symbol's global binding, even if that binding is
   not now dynamically visible.  May return Qunbound or magic values. */

Lisp_Object
top_level_value (Lisp_Object symbol)
{
  REGISTER struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	return ptr->old_value;
    }
  return XSYMBOL (symbol)->value;
}

#if 0

Lisp_Object
top_level_set (Lisp_Object symbol, Lisp_Object newval)
{
  REGISTER struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	{
	  ptr->old_value = newval;
	  return newval;
	}
    }
  return Fset (symbol, newval);
}

#endif /* 0 */


/************************************************************************/
/*			      Backtraces				*/
/************************************************************************/

DEFUN ("backtrace-debug", Fbacktrace_debug, 2, 2, 0, /*
Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
The debugger is entered when that frame exits, if the flag is non-nil.
*/
       (level, flag))
{
  REGISTER struct backtrace *backlist = backtrace_list;
  REGISTER int i;

  CHECK_INT (level);

  for (i = 0; backlist && i < XINT (level); i++)
    {
      backlist = backlist->next;
    }

  if (backlist)
    backlist->debug_on_exit = !NILP (flag);

  return flag;
}

static void
backtrace_specials (int speccount, int speclimit, Lisp_Object stream)
{
  int printing_bindings = 0;

  for (; speccount > speclimit; speccount--)
    {
      if (specpdl[speccount - 1].func == 0
          || specpdl[speccount - 1].func == specbind_unwind_local
          || specpdl[speccount - 1].func == specbind_unwind_wasnt_local)
	{
	  write_c_string (stream, !printing_bindings ? "  # bind (" : " ");
	  Fprin1 (specpdl[speccount - 1].symbol, stream);
	  printing_bindings = 1;
	}
      else
	{
	  if (printing_bindings) write_c_string (stream, ")\n");
	  write_c_string (stream, "  # (unwind-protect ...)\n");
	  printing_bindings = 0;
	}
    }
  if (printing_bindings) write_c_string (stream, ")\n");
}

static Lisp_Object
backtrace_unevalled_args (Lisp_Object *args)
{
  if (args)
    return *args;
  else
    return list1 (build_string ("[internal]"));
}

DEFUN ("backtrace", Fbacktrace, 0, 2, "", /*
Print a trace of Lisp function calls currently active.
Optional arg STREAM specifies the output stream to send the backtrace to,
and defaults to the value of `standard-output'.
Optional second arg DETAILED non-nil means show places where currently
active variable bindings, catches, condition-cases, and
unwind-protects, as well as function calls, were made.
*/
       (stream, detailed))
{
  /* This function can GC */
  struct backtrace *backlist = backtrace_list;
  struct catchtag *catches = catchlist;
  int speccount = specpdl_depth();

  int old_nl = print_escape_newlines;
  int old_pr = print_readably;
  Lisp_Object old_level = Vprint_level;
  Lisp_Object oiq = Vinhibit_quit;
  struct gcpro gcpro1, gcpro2;

  /* We can't allow quits in here because that could cause the values
     of print_readably and print_escape_newlines to get screwed up.
     Normally we would use a record_unwind_protect but that would
     screw up the functioning of this function. */
  Vinhibit_quit = Qt;

  entering_debugger = 0;

  if (!NILP (detailed))
    Vprint_level = make_int (50);
  else
    Vprint_level = make_int (3);
  print_readably = 0;
  print_escape_newlines = 1;

  GCPRO2 (stream, old_level);

  stream = canonicalize_printcharfun (stream);

  for (;;)
    {
      if (!NILP (detailed) && catches && catches->backlist == backlist)
	{
          int catchpdl = catches->pdlcount;
          if (speccount > catchpdl
	      && specpdl[catchpdl].func == condition_case_unwind)
            /* This is a condition-case catchpoint */
            catchpdl = catchpdl + 1;

          backtrace_specials (speccount, catchpdl, stream);

          speccount = catches->pdlcount;
          if (catchpdl == speccount)
	    {
	      write_c_string (stream, "  # (catch ");
	      Fprin1 (catches->tag, stream);
	      write_c_string (stream, " ...)\n");
	    }
          else
            {
              write_c_string (stream, "  # (condition-case ... . ");
              Fprin1 (Fcdr (Fcar (catches->tag)), stream);
              write_c_string (stream, ")\n");
            }
          catches = catches->next;
	}
      else if (!backlist)
	break;
      else
	{
	  if (!NILP (detailed) && backlist->pdlcount < speccount)
	    {
	      backtrace_specials (speccount, backlist->pdlcount, stream);
	      speccount = backlist->pdlcount;
	    }
	  write_c_string (stream, backlist->debug_on_exit ? "* " : "  ");
	  if (backlist->nargs == UNEVALLED)
	    {
	      Fprin1 (Fcons (*backlist->function,
			     backtrace_unevalled_args (backlist->args)),
		      stream);
	      write_c_string (stream, "\n"); /* from FSFmacs 19.30 */
	    }
	  else
	    {
	      Lisp_Object tem = *backlist->function;
	      Fprin1 (tem, stream); /* This can QUIT */
	      write_c_string (stream, "(");
	      if (backlist->nargs == MANY)
		{
		  int i;
		  Lisp_Object tail = Qnil;
		  struct gcpro ngcpro1;

		  NGCPRO1 (tail);
		  for (tail = *backlist->args, i = 0;
		       !NILP (tail);
		       tail = Fcdr (tail), i++)
		    {
		      if (i != 0) write_c_string (stream, " ");
		      Fprin1 (Fcar (tail), stream);
		    }
		  NUNGCPRO;
		}
	      else
		{
		  int i;
		  for (i = 0; i < backlist->nargs; i++)
		    {
		      if (!i && EQ (tem, Qbyte_code))
			{
			  write_c_string (stream, "\"...\"");
			  continue;
			}
		      if (i != 0) write_c_string (stream, " ");
		      Fprin1 (backlist->args[i], stream);
		    }
		}
	      write_c_string (stream, ")\n");
	    }
	  backlist = backlist->next;
	}
    }
  Vprint_level = old_level;
  print_readably = old_pr;
  print_escape_newlines = old_nl;
  UNGCPRO;
  Vinhibit_quit = oiq;
  return Qnil;
}


DEFUN ("backtrace-frame", Fbacktrace_frame, 1, 1, 0, /*
Return the function and arguments NFRAMES up from current execution point.
If that frame has not evaluated the arguments yet (or is a special form),
the value is (nil FUNCTION ARG-FORMS...).
If that frame has evaluated its arguments and called its function already,
the value is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.
If NFRAMES is more than the number of frames, the value is nil.
*/
       (nframes))
{
  REGISTER struct backtrace *backlist = backtrace_list;
  REGISTER int i;
  Lisp_Object tem;

  CHECK_NATNUM (nframes);

  /* Find the frame requested.  */
  for (i = XINT (nframes); backlist && (i-- > 0);)
    backlist = backlist->next;

  if (!backlist)
    return Qnil;
  if (backlist->nargs == UNEVALLED)
    return Fcons (Qnil, Fcons (*backlist->function,
			       backtrace_unevalled_args (backlist->args)));
  else
    {
      if (backlist->nargs == MANY)
	tem = *backlist->args;
      else
	tem = Flist (backlist->nargs, backlist->args);

      return Fcons (Qt, Fcons (*backlist->function, tem));
    }
}


/************************************************************************/
/*			      Warnings					*/
/************************************************************************/

static int
warning_will_be_discarded (Lisp_Object level)
{
  /* Don't even generate debug warnings if they're going to be discarded,
     to avoid excessive consing. */
  return (EQ (level, Qdebug) && !NILP (Vlog_warning_minimum_level) &&
	  !EQ (Vlog_warning_minimum_level, Qdebug));
}

void
warn_when_safe_lispobj (Lisp_Object class_, Lisp_Object level,
			Lisp_Object obj)
{
  if (warning_will_be_discarded (level))
    return;

  obj = list1 (list3 (class_, level, obj));
  if (NILP (Vpending_warnings))
    Vpending_warnings = Vpending_warnings_tail = obj;
  else
    {
      Fsetcdr (Vpending_warnings_tail, obj);
      Vpending_warnings_tail = obj;
    }
}

/* #### This should probably accept Lisp objects; but then we have
   to make sure that Feval() isn't called, since it might not be safe.

   An alternative approach is to just pass some non-string type of
   Lisp_Object to warn_when_safe_lispobj(); `prin1-to-string' will
   automatically be called when it is safe to do so. */

void
warn_when_safe (Lisp_Object class_, Lisp_Object level, const CIbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  if (warning_will_be_discarded (level))
    return;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (CGETTEXT (fmt), args);
  va_end (args);

  warn_when_safe_lispobj (class_, level, obj);
}




/************************************************************************/
/*			    Initialization				*/
/************************************************************************/

void
syms_of_eval (void)
{
  INIT_LRECORD_IMPLEMENTATION (subr);

  DEFSYMBOL (Qinhibit_quit);
  DEFSYMBOL (Qautoload);
  DEFSYMBOL (Qdebug_on_error);
  DEFSYMBOL (Qstack_trace_on_error);
  DEFSYMBOL (Qdebug_on_signal);
  DEFSYMBOL (Qstack_trace_on_signal);
  DEFSYMBOL (Qdebugger);
  DEFSYMBOL (Qmacro);
  defsymbol (&Qand_rest, "&rest");
  defsymbol (&Qand_optional, "&optional");
  /* Note that the process code also uses Qexit */
  DEFSYMBOL (Qexit);
  DEFSYMBOL (Qsetq);
  DEFSYMBOL (Qinteractive);
  DEFSYMBOL (Qcommandp);
  DEFSYMBOL (Qdefun);
  DEFSYMBOL (Qprogn);
  DEFSYMBOL (Qvalues);
  DEFSYMBOL (Qdisplay_warning);
  DEFSYMBOL (Qrun_hooks);
  DEFSYMBOL (Qfinalize_list);
  DEFSYMBOL (Qif);

  DEFSUBR (For);
  DEFSUBR (Fand);
  DEFSUBR (Fif);
  DEFSUBR_MACRO (Fwhen);
  DEFSUBR_MACRO (Funless);
  DEFSUBR (Fcond);
  DEFSUBR (Fprogn);
  DEFSUBR (Fprog1);
  DEFSUBR (Fprog2);
  DEFSUBR (Fsetq);
  DEFSUBR (Fquote);
  DEFSUBR (Ffunction);
  DEFSUBR (Fdefun);
  DEFSUBR (Fdefmacro);
  DEFSUBR (Fdefvar);
  DEFSUBR (Fdefconst);
  DEFSUBR (Fuser_variable_p);
  DEFSUBR (Flet);
  DEFSUBR (FletX);
  DEFSUBR (Fwhile);
  DEFSUBR (Fmacroexpand_internal);
  DEFSUBR (Fcatch);
  DEFSUBR (Fthrow);
  DEFSUBR (Funwind_protect);
  DEFSUBR (Fcondition_case);
  DEFSUBR (Fcall_with_condition_handler);
  DEFSUBR (Fsignal);
  DEFSUBR (Finteractive_p);
  DEFSUBR (Fcommandp);
  DEFSUBR (Fcommand_execute);
  DEFSUBR (Fautoload);
  DEFSUBR (Feval);
  DEFSUBR (Fapply);
  DEFSUBR (Ffuncall);
  DEFSUBR (Ffunctionp);
  DEFSUBR (Ffunction_min_args);
  DEFSUBR (Ffunction_max_args);
  DEFSUBR (Frun_hooks);
  DEFSUBR (Frun_hook_with_args);
  DEFSUBR (Frun_hook_with_args_until_success);
  DEFSUBR (Frun_hook_with_args_until_failure);
  DEFSUBR (Fbacktrace_debug);
  DEFSUBR (Fbacktrace);
  DEFSUBR (Fbacktrace_frame);
}

void
init_eval_semi_early (void)
{
  specpdl_ptr = specpdl;
  specpdl_depth_counter = 0;
  catchlist = 0;
  Vcondition_handlers = Qnil;
  backtrace_list = 0;
  Vquit_flag = Qnil;
  debug_on_next_call = 0;
  lisp_eval_depth = 0;
  entering_debugger = 0;
}

void
reinit_vars_of_eval (void)
{
  preparing_for_armageddon = 0;
  in_warnings = 0;
  specpdl_size = 50;
  specpdl = xnew_array (struct specbinding, specpdl_size);
  /* XEmacs change: increase these values. */
  max_specpdl_size = 3000;
  max_lisp_eval_depth = 1000;
#ifdef DEFEND_AGAINST_THROW_RECURSION
  throw_level = 0;
#endif
}

void
vars_of_eval (void)
{
  reinit_vars_of_eval ();

  DEFVAR_INT ("max-specpdl-size", &max_specpdl_size /*
Limit on number of Lisp variable bindings & unwind-protects before error.
*/ );

  DEFVAR_INT ("max-lisp-eval-depth", &max_lisp_eval_depth /*
Limit on depth in `eval', `apply' and `funcall' before error.
This limit is to catch infinite recursions for you before they cause
actual stack overflow in C, which would be fatal for Emacs.
You can safely make it considerably larger than its default value,
if that proves inconveniently small.
*/ );

  DEFVAR_LISP ("quit-flag", &Vquit_flag /*
t causes running Lisp code to abort, unless `inhibit-quit' is non-nil.
`critical' causes running Lisp code to abort regardless of `inhibit-quit'.
Normally, you do not need to set this value yourself.  It is set to
t each time a Control-G is detected, and to `critical' each time a
Shift-Control-G is detected.  The XEmacs core C code is littered with
calls to the QUIT; macro, which check the values of `quit-flag' and
`inhibit-quit' and abort (or more accurately, call (signal 'quit)) if
it's correct to do so.
*/ );
  Vquit_flag = Qnil;

  DEFVAR_LISP ("inhibit-quit", &Vinhibit_quit /*
Non-nil inhibits C-g quitting from happening immediately.
Note that `quit-flag' will still be set by typing C-g,
so a quit will be signalled as soon as `inhibit-quit' is nil.
To prevent this happening, set `quit-flag' to nil
before making `inhibit-quit' nil.

The value of `inhibit-quit' is ignored if a critical quit is
requested by typing control-shift-G in a window-system frame;
this is explained in more detail in `quit-flag'.
*/ );
  Vinhibit_quit = Qnil;

  DEFVAR_LISP ("stack-trace-on-error", &Vstack_trace_on_error /*
*Non-nil means automatically display a backtrace buffer
after any error that is not handled by a `condition-case'.
If the value is a list, an error only means to display a backtrace
if one of its condition symbols appears in the list.
See also variable `stack-trace-on-signal'.
*/ );
  Vstack_trace_on_error = Qnil;

  DEFVAR_LISP ("stack-trace-on-signal", &Vstack_trace_on_signal /*
*Non-nil means automatically display a backtrace buffer
after any error that is signalled, whether or not it is handled by
a `condition-case'.
If the value is a list, an error only means to display a backtrace
if one of its condition symbols appears in the list.
See also variable `stack-trace-on-error'.
*/ );
  Vstack_trace_on_signal = Qnil;

  DEFVAR_LISP ("debug-ignored-errors", &Vdebug_ignored_errors /*
*List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.
*/ );
  Vdebug_ignored_errors = Qnil;

  DEFVAR_LISP ("debug-on-error", &Vdebug_on_error /*
*Non-nil means enter debugger if an unhandled error is signalled.
The debugger will not be entered if the error is handled by
a `condition-case'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
This variable is overridden by `debug-ignored-errors'.
See also variables `debug-on-quit' and `debug-on-signal'.

If this variable is set while XEmacs is running noninteractively (using
`-batch'), and XEmacs was configured with `--debug' (#define XEMACS_DEBUG
in the C code), instead of trying to invoke the Lisp debugger (which
obviously won't work), XEmacs will break out to a C debugger using
\(force-debugging-signal t).  This is useful because debugging
noninteractive runs of XEmacs is often very difficult, since they typically
happen as part of sometimes large and complex make suites (e.g. rebuilding
the XEmacs packages).  NOTE: This runs abort()!!! (As well as and after
executing INT 3 under MS Windows, which should invoke a debugger if it's
active.) This is guaranteed to kill XEmacs! (But in this situation, XEmacs
is about to die anyway, and if no debugger is present, this will usefully
dump core.) The most useful way to set this flag when debugging
noninteractive runs, especially in makefiles, is using the environment
variable XEMACSDEBUG, like this:

\(using csh)      setenv XEMACSDEBUG '(setq debug-on-error t)'
\(using bash)     export XEMACSDEBUG='(setq debug-on-error t)'
*/ );
  Vdebug_on_error = Qnil;

  DEFVAR_LISP ("debug-on-signal", &Vdebug_on_signal /*
*Non-nil means enter debugger if an error is signalled.
The debugger will be entered whether or not the error is handled by
a `condition-case'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
See also variable `debug-on-quit'.

This will attempt to enter a C debugger when XEmacs is run noninteractively
and under the same conditions as described in `debug-on-error'.
*/ );
  Vdebug_on_signal = Qnil;

  DEFVAR_BOOL ("debug-on-quit", &debug_on_quit /*
*Non-nil means enter debugger if quit is signalled (C-G, for example).
Does not apply if quit is handled by a `condition-case'.  Entering the
debugger can also be achieved at any time (for X11 console) by typing
control-shift-G to signal a critical quit.
*/ );
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", &debug_on_next_call /*
Non-nil means enter debugger before next `eval', `apply' or `funcall'.
*/ );

  DEFVAR_BOOL ("backtrace-with-interal-sections",
	       &backtrace_with_internal_sections /*
Non-nil means backtraces will contain additional information indicating
when particular sections of the C code have been entered, e.g. redisplay(),
byte-char conversion, internal-external conversion, etc.  This can be
particularly useful when XEmacs crashes, in helping to pinpoint the problem.
*/ );
#ifdef ERROR_CHECK_STRUCTURES
  backtrace_with_internal_sections = 1;
#else
  backtrace_with_internal_sections = 0;
#endif

  DEFVAR_LISP ("debugger", &Vdebugger /*
Function to call to invoke debugger.
If due to frame exit, args are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, args are `error' and a list of the args to `signal'.
If due to `apply' or `funcall' entry, one arg, `lambda'.
If due to `eval' entry, one arg, t.
*/ );
  Vdebugger = Qnil;

  staticpro (&Vcatch_everything_tag);
  Vcatch_everything_tag = make_opaque (OPAQUE_CLEAR, 0);

  staticpro (&Vpending_warnings);
  Vpending_warnings = Qnil;
  dump_add_root_lisp_object (&Vpending_warnings_tail);
  Vpending_warnings_tail = Qnil;

  DEFVAR_LISP ("log-warning-minimum-level", &Vlog_warning_minimum_level);
  Vlog_warning_minimum_level = Qinfo;

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;

  staticpro (&Vcondition_handlers);

  staticpro (&Vdeletable_permanent_display_objects);
  Vdeletable_permanent_display_objects = Qnil;

  staticpro (&Vmodifiable_buffers);
  Vmodifiable_buffers = Qnil;

  inhibit_flags = 0;
}
