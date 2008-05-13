/* New incremental garbage collector for XEmacs.
   Copyright (C) 2005 Marcus Crestani.

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

#ifndef INCLUDED_gc_h_
#define INCLUDED_gc_h_

BEGIN_C_DECLS


#ifdef NEW_GC
/************************************************************************/
/*		         Incremental Statistics      			*/
/************************************************************************/
#ifdef ERROR_CHECK_GC
void gc_stat_print_stats (void);
void gc_stat_finalized (void);
void gc_stat_freed (void);
# define GC_STAT_FINALIZED gc_stat_finalized ()
# define GC_STAT_FREED gc_stat_freed ()
#else /* not ERROR_CHECK_GC */
# define GC_STAT_FINALIZED
# define GC_STAT_FREED
#endif /* not ERROR_CHECK_GC */
#endif /* not NEW_GC */


/************************************************************************/
/*		             Global Variables     			*/
/************************************************************************/
/* Number of bytes of consing done since the last GC. */
extern EMACS_INT consing_since_gc;

/* Number of bytes of consing done since startup. */
extern EMACS_UINT total_consing;

/* Number of bytes of current allocated heap objects. */
extern EMACS_INT total_gc_usage;

/* If the above is set. */
extern int total_gc_usage_set;

/* Number of bytes of consing since gc before another gc should be done. */
extern EMACS_INT gc_cons_threshold;

/* Percentage of consing of total data size before another GC. */
extern EMACS_INT gc_cons_percentage;

#ifdef NEW_GC
/* Number of bytes of consing since gc before another cycle of the gc
   should be done in incremental mode. */
extern EMACS_INT gc_cons_incremental_threshold;

/* Nonzero during gc */
extern int gc_in_progress;

/* Nonzero during write barrier */
extern int write_barrier_enabled;

/* Enable/disable incremental garbage collection during runtime. */
extern int allow_incremental_gc;
#endif /* NEW_GC */


/************************************************************************/
/*		               Prototypes         			*/
/************************************************************************/

#ifndef MALLOC_OVERHEAD
#ifdef GNU_MALLOC
#define MALLOC_OVERHEAD 0
#elif defined (rcheck)
#define MALLOC_OVERHEAD 20
#else
#define MALLOC_OVERHEAD 8
#endif
#endif /* MALLOC_OVERHEAD */

#ifdef ERROR_CHECK_GC
#define GC_CHECK_LHEADER_INVARIANTS(lheader) do {		\
  struct lrecord_header * GCLI_lh = (lheader);			\
  assert (GCLI_lh != 0);					\
  assert (GCLI_lh->type < (unsigned int) lrecord_type_count);	\
} while (0)
#else
#define GC_CHECK_LHEADER_INVARIANTS(lheader)
#endif

void recompute_need_to_garbage_collect (void);


/* KKCC mark algorithm. */
#ifdef DEBUG_XEMACS
void kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj, int level, int pos);
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj, level, pos)
void kkcc_gc_stack_repush_dirty_object_1 (Lisp_Object obj, int level, int pos);
#define kkcc_gc_stack_repush_dirty_object(obj) \
  kkcc_gc_stack_repush_dirty_object_1 (obj, 0, -2)
void kkcc_backtrace (void);
#else
void kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj);
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj)
void kkcc_gc_stack_repush_dirty_object_1 (Lisp_Object obj);
#define kkcc_gc_stack_repush_dirty_object(obj) \
  kkcc_gc_stack_repush_dirty_object_1 (obj)
#define kkcc_backtrace()
#endif

#ifdef NEW_GC

/* Repush objects that are caught by the write barrier. */
#define gc_write_barrier(obj) kkcc_gc_stack_repush_dirty_object (obj);


/* GC functions: */

/* Perform a full garbage collection without interruption. If an
   incremental garbage collection is already running it is completed
   without further interruption. This function calls gc() with a
   negative or zero argument. */
void gc_full (void);

/* This function starts an incremental garbage collection. If an
   incremental garbage collection is already running, the next cycle
   of traversal work is done, or the garbage collection is completed
   when no more traversal work has to be done. This function calls gc
   with a positive argument, indicating how many objects can be
   traversed in this cycle. */
void gc_incremental (void);
#endif /* NEW_GC */

/* Initializers */
void init_gc_early (void);
void reinit_gc_early (void);
void init_gc_once_early (void);

void syms_of_gc (void);
void vars_of_gc (void);
void complex_vars_of_gc (void);

#ifndef NEW_GC
/* Needed prototypes due to the garbage collector code move from
   alloc.c to gc.c. */
void gc_sweep_1 (void);

extern void *breathing_space;
#endif /* not NEW_GC */

#ifdef NEW_GC
void add_finalizable_obj (Lisp_Object obj);
void register_for_finalization (void);
void run_finalizers (void);
#endif /* NEW_GC */

END_C_DECLS

#endif /* INCLUDED_gc_h_ */
