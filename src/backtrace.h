/* The lisp stack.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 2002, 2003 Ben Wing.

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

/* Synched up with: FSF 19.30.  Contained redundantly in various C files
   in FSFmacs. */

/* Authorship:

   FSF: Original version; a long time ago.
   XEmacs: split out of some C files. (For some obscure reason, a header
           file couldn't be used in FSF Emacs, but XEmacs doesn't have
	   that problem.)
   Mly (probably) or JWZ: Some changes.
 */

#ifndef INCLUDED_backtrace_h_
#define INCLUDED_backtrace_h_

#include <setjmp.h>

#ifdef ERROR_CHECK_CATCH
/* you can use this if you are trying to debug corruption in the
   catchlist */
void check_catchlist_sanity (void);

/* you can use this if you are trying to debug corruption in the specbind
   stack */
void check_specbind_stack_sanity (void);
#else
#define check_catchlist_sanity()
#define check_specbind_stack_sanity()
#endif

/* These definitions are used in eval.c and alloc.c */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;		/* Points to vector of args. */
    int nargs;			/* Length of vector.
				   If nargs is UNEVALLED, args points to
				   slot holding list of unevalled args */
    int pdlcount;               /* specpdl_depth () when invoked */
    char evalargs;
    /* Nonzero means call value of debugger when done with this operation. */
    char debug_on_exit;

    /* All the rest is information for the use of the profiler.  The only
       thing that eval.c does is set the first value to 0 so that it can
       be relied upon. */

    /* ----------------------------------------------------------------- */

    /* 0 = profiling not turned on when function called.
           Since profiling can be turned on and off dynamically, we can't
           always count on having info recorded when a function was called
           and need to take evasive action if necessary.
       1 = profiling turned on but function not yet actually called.  Lots of
           stuff can happen between when a function is pushed onto the
           backtrace list and when it's actually called (e.g. evalling its
           arguments, autoloading, etc.).  For greater accuracy we don't
           treat the preamble stuff as part of the function itself.
       2 = profiling turned on, function called.
    */
    char function_being_called;
    /* The trick here is handling recursive functions and dealing with the
       dynamicity of in-profile/not-in-profile.  I used to just use a bunch
       of hash tables for all info but that fails in the presence of
       recursive functions because they can modify values out from under
       you.  The algorithm here is that we record the total_ticks and
       total_consing, as well as the current values of `total-timing' and
       `total-gc-usage' for the OBJ -- that's because recursive functions,
       which get called later and exit early, will go ahead and modify the
       `total-timing' and `total-gc-usage' for the fun, even though it's
       not "correct" because the outer function is still running.  However,
       if we ask for profiling info at this point, at least we're getting
       SOME info.

       So ... On entry, we record these four values.  On exit, we compute
       an offset from the recorded value to the current value and then
       store it into the appropriate hash table entry, using the recorded
       value in the entry rather than the actual one. (Inner recursive
       functions may have added their own values to the total-counts, and
       we want to subsume them, not add to them.)

       #### Also we need to go through the backtrace list during
       stop-profiling and record values, just like for unwind_to. */
    EMACS_INT  current_total_timing_val;
    EMACS_INT  current_total_gc_usage_val;
    EMACS_UINT total_ticks_at_start;
    EMACS_UINT total_consing_at_start;
  };

/* This structure helps implement the `catch' and `throw' control
   structure.  A struct catchtag contains all the information needed
   to restore the state of the interpreter after a non-local jump.
   (No information is stored concerning how to restore the state of
   the condition-handler list; this is handled implicitly through
   an unwind-protect.  unwind-protects are on the specbind stack,
   which is reset to its proper value by `throw'.  In the process of
   that, any intervening bindings are reset and unwind-protects called,
   which fixes up the condition-handler list.

   catchtag structures are chained together in the C calling stack;
   the `next' member points to the next outer catchtag.

   A call like (throw TAG VAL) searches for a catchtag whose `tag'
   member is TAG, and then unbinds to it.  A value of Vcatch_everything_tag
   for the `tag' member of a catchtag is special and means "catch all throws,
   regardless of the tag".  This is used internally by the C code.  The `val'
   member is used to hold VAL while the stack is unwound; `val' is returned
   as the value of the catch form.  The `actual_tag' member holds the value
   of TAG as passed to throw, so that it can be retrieved when catches with
   Vcatch_everything_tag are set up.

   All the other members are concerned with restoring the interpreter
   state.  */

struct catchtag
  {
    Lisp_Object tag;
    /* Stores the actual tag used in `throw'; the same as TAG, unless
       TAG is Vcatch_everything_tag. */
    Lisp_Object actual_tag;
    Lisp_Object val;
    struct catchtag *next;
    struct gcpro *gcpro;
    JMP_BUF jmp;
    struct backtrace *backlist;
#if 0 /* FSFmacs */
    /* FSF uses a separate handler stack to hold condition-cases,
       where we use Vcondition_handlers.  We should switch to their
       system becaue it avoids the need to mess around with consing
       up stuff and then dangerously freeing it.  See comment in
       condition_case_unwind(). */
    struct handler *handlerlist;
#endif
    int lisp_eval_depth;
    int pdlcount;
#if 0 /* FSFmacs */
    /* This is the equivalent of async_timer_suppress_count.
       We probably don't have to bother with this. */
    int poll_suppress_count;
#endif
  };

/* Dynamic-binding-o-rama */

/* Structure for recording Lisp call stack for backtrace purposes.  */

/* The special binding stack holds the outer values of variables while
   they are bound by a function application or a let form, stores the
   code to be executed for Lisp unwind-protect forms, and stores the C
   functions to be called for record_unwind_protect.

   If func is non-zero, undoing this binding applies func to old_value;
      This implements record_unwind_protect.
   If func is zero and symbol is nil, undoing this binding evaluates
      the list of forms in old_value; this implements Lisp's unwind-protect
      form.
   Otherwise, undoing this binding stores old_value as symbol's value; this
      undoes the bindings made by a let form or function call.  */

struct specbinding
  {
    Lisp_Object symbol;
    Lisp_Object old_value;
    Lisp_Object (*func) (Lisp_Object); /* for unwind-protect */
  };

#if 0 /* FSFmacs */
/* #### */
/* Everything needed to describe an active condition case.  */
struct handler
  {
    /* The handler clauses and variable from the condition-case form.  */
    Lisp_Object handler;
    Lisp_Object var;
    /* Fsignal stores here the condition-case clause that applies,
       and Fcondition_case thus knows which clause to run.  */
    Lisp_Object chosen_clause;

    /* Used to effect the longjmp() out to the handler.  */
    struct catchtag *tag;

    /* The next enclosing handler.  */
    struct handler *next;
  };

extern struct handler *handlerlist;

#endif

/* These are extern because GC needs to mark them */
extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

/* Most callers should simply use specbind() and unbind_to_1(), but if
   speed is REALLY IMPORTANT, you can use the faster macros below */
void specbind_magic (Lisp_Object, Lisp_Object);
void grow_specpdl (EMACS_INT reserved);
void unbind_to_hairy (int);
extern int specpdl_size;

/* Inline version of specbind().
   Use this instead of specbind() if speed is sufficiently important
   to save the overhead of even a single function call. */
#define SPECBIND(symbol_object, value_object) do {			\
  Lisp_Object SB_symbol = (symbol_object);				\
  Lisp_Object SB_newval = (value_object);				\
  Lisp_Object SB_oldval;						\
  Lisp_Symbol *SB_sym;							\
									\
  SPECPDL_RESERVE (1);							\
									\
  CHECK_SYMBOL (SB_symbol);						\
  SB_sym = XSYMBOL (SB_symbol);						\
  SB_oldval = SB_sym->value;						\
									\
  if (!SYMBOL_VALUE_MAGIC_P (SB_oldval) || UNBOUNDP (SB_oldval))	\
    {									\
      /* #### the following test will go away when we have a constant	\
         symbol magic object */						\
      if (EQ (SB_symbol, Qnil) ||					\
	  EQ (SB_symbol, Qt)   ||					\
	  SYMBOL_IS_KEYWORD (SB_symbol))				\
	reject_constant_symbols (SB_symbol, SB_newval, 0,		\
				 UNBOUNDP (SB_newval) ?			\
				 Qmakunbound : Qset);			\
									\
      specpdl_ptr->symbol    = SB_symbol;				\
      specpdl_ptr->old_value = SB_oldval;				\
      specpdl_ptr->func      = 0;					\
      specpdl_ptr++;							\
      specpdl_depth_counter++;						\
									\
      SB_sym->value = (SB_newval);					\
    }									\
  else									\
    specbind_magic (SB_symbol, SB_newval);				\
  check_specbind_stack_sanity ();					\
} while (0)

/* An even faster, but less safe inline version of specbind().
   Caller guarantees that:
   - SYMBOL is a non-constant symbol (i.e. not Qnil, Qt, or keyword).
   - specpdl_depth_counter >= specpdl_size.
   Else we crash.  */
#define SPECBIND_FAST_UNSAFE(symbol_object, value_object) do {		\
  Lisp_Object SFU_symbol = (symbol_object);				\
  Lisp_Object SFU_newval = (value_object);				\
  Lisp_Symbol *SFU_sym   = XSYMBOL (SFU_symbol);			\
  Lisp_Object SFU_oldval = SFU_sym->value;				\
  /* Most of the time, will be previously unbound.  #### With a bit of	\
   rearranging, this could be reduced to only one check. */		\
  if (UNBOUNDP (SFU_oldval) || !SYMBOL_VALUE_MAGIC_P (SFU_oldval))	\
    {									\
      specpdl_ptr->symbol    = SFU_symbol;				\
      specpdl_ptr->old_value = SFU_oldval;				\
      specpdl_ptr->func      = 0;					\
      specpdl_ptr++;							\
      specpdl_depth_counter++;						\
									\
      SFU_sym->value = (SFU_newval);					\
    }									\
  else									\
    specbind_magic (SFU_symbol, SFU_newval);				\
  check_specbind_stack_sanity ();					\
} while (0)
/* Request enough room for SIZE future entries on special binding stack */
#define SPECPDL_RESERVE(size) do {			\
  EMACS_INT SR_size = (size);				\
  if (specpdl_depth() + SR_size >= specpdl_size)	\
    grow_specpdl (SR_size);				\
} while (0)

/* Inline version of unbind_to_1().
   [[Use this instead of unbind_to_1() if speed is sufficiently important
   to save the overhead of even a single function call.]]
   This is bogus pseudo-optimization. --ben

   Most of the time, unbind_to_1() is called only on ordinary
   variables, so optimize for that.  */
#define UNBIND_TO_GCPRO(count, value) do {		\
  int UNBIND_TO_count = (count);			\
  while (specpdl_depth_counter != UNBIND_TO_count)	\
    {							\
      Lisp_Symbol *sym;					\
      --specpdl_ptr;					\
      --specpdl_depth_counter;				\
							\
      if (specpdl_ptr->func != 0 ||			\
	  ((sym = XSYMBOL (specpdl_ptr->symbol)),	\
	   SYMBOL_VALUE_MAGIC_P (sym->value)))		\
	{						\
	  struct gcpro gcpro1;				\
	  GCPRO1 (value);				\
	  unbind_to_hairy (UNBIND_TO_count);		\
	  UNGCPRO;					\
	  break;					\
	}						\
							\
      sym->value = specpdl_ptr->old_value;		\
    }							\
  check_specbind_stack_sanity ();			\
} while (0)

/* A slightly faster inline version of unbind_to_1,
   that doesn't offer GCPROing services. */
#define UNBIND_TO(count) do {				\
  int UNBIND_TO_count = (count);			\
  while (specpdl_depth_counter != UNBIND_TO_count)	\
    {							\
      Lisp_Symbol *sym;					\
      --specpdl_ptr;					\
      --specpdl_depth_counter;				\
							\
      if (specpdl_ptr->func != 0 ||			\
	  ((sym = XSYMBOL (specpdl_ptr->symbol)),	\
	   SYMBOL_VALUE_MAGIC_P (sym->value)))		\
	{						\
	  unbind_to_hairy (UNBIND_TO_count);		\
	  break;					\
	}						\
							\
      sym->value = specpdl_ptr->old_value;		\
    }							\
  check_specbind_stack_sanity ();			\
} while (0)

#if 0
/* Unused.  It's too hard to guarantee that the current bindings
   contain only variables.  */
/* Another inline version of unbind_to_1().  VALUE is GC-protected.
   Caller guarantees that:
   - all of the elements on the binding stack are variable bindings.
   Else we crash.  */
#define UNBIND_TO_GCPRO_VARIABLES_ONLY(count, value) do {	\
  int UNBIND_TO_count = (count);				\
  while (specpdl_depth_counter != UNBIND_TO_count)		\
    {								\
      Lisp_Symbol *sym;						\
      --specpdl_ptr;						\
      --specpdl_depth_counter;					\
								\
      sym = XSYMBOL (specpdl_ptr->symbol);			\
      if (!SYMBOL_VALUE_MAGIC_P (sym->value))			\
	sym->value = specpdl_ptr->old_value;			\
      else							\
	{							\
	  struct gcpro gcpro1;					\
	  GCPRO1 (value);					\
	  unbind_to_hairy (UNBIND_TO_count);			\
	  UNGCPRO;						\
	  break;						\
	}							\
    }								\
} while (0)
#endif /* unused */

/* A faster, but less safe inline version of Fset().
   Caller guarantees that:
   - SYMBOL is a non-constant symbol (i.e. not Qnil, Qt, or keyword).
   Else we crash.  */
#define FSET_FAST_UNSAFE(sym, newval) do {				\
  Lisp_Object FFU_sym = (sym);						\
  Lisp_Object FFU_newval = (newval);					\
  Lisp_Symbol *FFU_symbol = XSYMBOL (FFU_sym);				\
  Lisp_Object FFU_oldval = FFU_symbol->value;				\
  if (!SYMBOL_VALUE_MAGIC_P (FFU_oldval) || UNBOUNDP (FFU_oldval))	\
    FFU_symbol->value = FFU_newval;					\
  else									\
    Fset (FFU_sym, FFU_newval);						\
} while (0)

/* Note: you must always fill in all of the fields in a backtrace structure
   before pushing them on the backtrace_list.  The profiling code depends
   on this. */

#define PUSH_BACKTRACE(bt) do {		\
  (bt).next = backtrace_list;		\
  backtrace_list = &(bt);		\
} while (0)

#define POP_BACKTRACE(bt) do {		\
  backtrace_list = (bt).next;		\
} while (0)

#endif /* INCLUDED_backtrace_h_ */
