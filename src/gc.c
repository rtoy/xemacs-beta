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

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "chartab.h"
#include "console-stream.h"
#include "device.h"
#include "elhash.h"
#include "events.h"
#include "extents-impl.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "gc.h"
#include "glyphs.h"
#include "opaque.h"
#include "lrecord.h"
#include "lstream.h"
#include "process.h"
#include "profile.h"
#include "redisplay.h"
#include "specifier.h"
#include "sysfile.h"
#include "sysdep.h"
#include "window.h"
#include "vdb.h"


#define GC_CONS_THRESHOLD                  2000000
#define GC_CONS_INCREMENTAL_THRESHOLD       200000
#define GC_INCREMENTAL_TRAVERSAL_THRESHOLD  100000

/* Number of bytes of consing done since the last GC. */
EMACS_INT consing_since_gc;

/* Number of bytes of consing done since startup. */
EMACS_UINT total_consing;

/* Number of bytes of current allocated heap objects. */
EMACS_INT total_gc_usage;

/* If the above is set. */
int total_gc_usage_set;

/* Number of bytes of consing since gc before another gc should be done. */
EMACS_INT gc_cons_threshold;

/* Nonzero during gc */
int gc_in_progress;

/* Percentage of consing of total data size before another GC. */
EMACS_INT gc_cons_percentage;

#ifdef NEW_GC
/* Number of bytes of consing since gc before another cycle of the gc
   should be done in incremental mode. */
EMACS_INT gc_cons_incremental_threshold;

/* Number of elements marked in one cycle of incremental GC. */
EMACS_INT gc_incremental_traversal_threshold;

/* Nonzero during write barrier */
int write_barrier_enabled;
#endif /* NEW_GC */



#ifdef NEW_GC
/************************************************************************/
/*		   Incremental State and Statistics   			*/
/************************************************************************/

enum gc_phase
{
  NONE,
  INIT_GC,
  PUSH_ROOT_SET,
  MARK,
  REPUSH_ROOT_SET,
  FINISH_MARK,
  FINALIZE,
  SWEEP,
  FINISH_GC
};

#ifndef ERROR_CHECK_GC
struct
{
  enum gc_phase phase;
} gc_state;
#else /* ERROR_CHECK_GC */
enum gc_stat_id
{
  GC_STAT_TOTAL,
  GC_STAT_IN_LAST_GC,
  GC_STAT_IN_THIS_GC,
  GC_STAT_IN_LAST_CYCLE,
  GC_STAT_IN_THIS_CYCLE,
  GC_STAT_COUNT /* has to be last */
};

struct
{
  enum gc_phase phase;
  double n_gc[GC_STAT_COUNT];
  double n_cycles[GC_STAT_COUNT];
  double enqueued[GC_STAT_COUNT];
  double dequeued[GC_STAT_COUNT];
  double repushed[GC_STAT_COUNT];
  double enqueued2[GC_STAT_COUNT];
  double dequeued2[GC_STAT_COUNT];
  double finalized[GC_STAT_COUNT];
  double freed[GC_STAT_COUNT];
} gc_state;
#endif /* ERROR_CHECK_GC */

#define GC_PHASE gc_state.phase
#define GC_SET_PHASE(p) GC_PHASE = p

#ifdef ERROR_CHECK_GC
# define GC_STAT_START_NEW_GC gc_stat_start_new_gc ()
# define GC_STAT_RESUME_GC gc_stat_resume_gc ()

#define GC_STAT_TICK(STAT)			\
  gc_state.STAT[GC_STAT_TOTAL]++;		\
  gc_state.STAT[GC_STAT_IN_THIS_GC]++;	\
  gc_state.STAT[GC_STAT_IN_THIS_CYCLE]++

# define GC_STAT_ENQUEUED			\
  if (GC_PHASE == REPUSH_ROOT_SET)		\
    {						\
      GC_STAT_TICK (enqueued2);			\
    }						\
  else						\
    {						\
      GC_STAT_TICK (enqueued);			\
    } 

# define GC_STAT_DEQUEUED			\
  if (gc_state.phase == REPUSH_ROOT_SET)	\
    {						\
      GC_STAT_TICK (dequeued2);			\
    }						\
  else						\
    {						\
      GC_STAT_TICK (dequeued);			\
    } 
# define GC_STAT_REPUSHED GC_STAT_TICK (repushed)

#define GC_STAT_RESUME(stat)			\
  gc_state.stat[GC_STAT_IN_LAST_CYCLE] =	\
    gc_state.stat[GC_STAT_IN_THIS_CYCLE];	\
  gc_state.stat[GC_STAT_IN_THIS_CYCLE] = 0

#define GC_STAT_RESTART(stat)			\
  gc_state.stat[GC_STAT_IN_LAST_GC] =		\
    gc_state.stat[GC_STAT_IN_THIS_GC];	\
  gc_state.stat[GC_STAT_IN_THIS_GC] = 0;	\
  GC_STAT_RESUME (stat)

void
gc_stat_start_new_gc (void)
{
  gc_state.n_gc[GC_STAT_TOTAL]++;
  gc_state.n_cycles[GC_STAT_TOTAL]++;
  gc_state.n_cycles[GC_STAT_IN_LAST_GC] = gc_state.n_cycles[GC_STAT_IN_THIS_GC];
  gc_state.n_cycles[GC_STAT_IN_THIS_GC] = 1;
  
  GC_STAT_RESTART (enqueued);
  GC_STAT_RESTART (dequeued);
  GC_STAT_RESTART (repushed);
  GC_STAT_RESTART (finalized);
  GC_STAT_RESTART (enqueued2);
  GC_STAT_RESTART (dequeued2);
  GC_STAT_RESTART (freed);
} 

void
gc_stat_resume_gc (void)
{
  gc_state.n_cycles[GC_STAT_TOTAL]++;
  gc_state.n_cycles[GC_STAT_IN_THIS_GC]++;
  GC_STAT_RESUME (enqueued);
  GC_STAT_RESUME (dequeued);
  GC_STAT_RESUME (repushed);
  GC_STAT_RESUME (finalized);
  GC_STAT_RESUME (enqueued2);
  GC_STAT_RESUME (dequeued2);
  GC_STAT_RESUME (freed);
}

void
gc_stat_finalized (void)
{
  GC_STAT_TICK (finalized);
}

void
gc_stat_freed (void)
{
  GC_STAT_TICK (freed);
}

DEFUN("gc-stats", Fgc_stats, 0, 0 ,"", /*
Return statistics about garbage collection cycles in a property list.
*/
      ())
{
  Lisp_Object pl = Qnil;
#define PL(name,value) \
  pl = cons3 (intern (name), make_float (gc_state.value), pl)

  PL ("freed-in-this-cycle", freed[GC_STAT_IN_THIS_CYCLE]);
  PL ("freed-in-this-gc", freed[GC_STAT_IN_THIS_GC]);
  PL ("freed-in-last-cycle", freed[GC_STAT_IN_LAST_CYCLE]);
  PL ("freed-in-last-gc", freed[GC_STAT_IN_LAST_GC]);
  PL ("freed-total", freed[GC_STAT_TOTAL]);
  PL ("finalized-in-this-cycle", finalized[GC_STAT_IN_THIS_CYCLE]);
  PL ("finalized-in-this-gc", finalized[GC_STAT_IN_THIS_GC]);
  PL ("finalized-in-last-cycle", finalized[GC_STAT_IN_LAST_CYCLE]);
  PL ("finalized-in-last-gc", finalized[GC_STAT_IN_LAST_GC]);
  PL ("finalized-total", finalized[GC_STAT_TOTAL]);
  PL ("repushed-in-this-cycle", repushed[GC_STAT_IN_THIS_CYCLE]);
  PL ("repushed-in-this-gc", repushed[GC_STAT_IN_THIS_GC]);
  PL ("repushed-in-last-cycle", repushed[GC_STAT_IN_LAST_CYCLE]);
  PL ("repushed-in-last-gc", repushed[GC_STAT_IN_LAST_GC]);
  PL ("repushed-total", repushed[GC_STAT_TOTAL]);
  PL ("dequeued2-in-this-cycle", dequeued2[GC_STAT_IN_THIS_CYCLE]);
  PL ("dequeued2-in-this-gc", dequeued2[GC_STAT_IN_THIS_GC]);
  PL ("dequeued2-in-last-cycle", dequeued2[GC_STAT_IN_LAST_CYCLE]);
  PL ("dequeued2-in-last-gc", dequeued2[GC_STAT_IN_LAST_GC]);
  PL ("dequeued2-total", dequeued2[GC_STAT_TOTAL]);
  PL ("enqueued2-in-this-cycle", enqueued2[GC_STAT_IN_THIS_CYCLE]);
  PL ("enqueued2-in-this-gc", enqueued2[GC_STAT_IN_THIS_GC]);
  PL ("enqueued2-in-last-cycle", enqueued2[GC_STAT_IN_LAST_CYCLE]);
  PL ("enqueued2-in-last-gc", enqueued2[GC_STAT_IN_LAST_GC]);
  PL ("enqueued2-total", enqueued2[GC_STAT_TOTAL]);
  PL ("dequeued-in-this-cycle", dequeued[GC_STAT_IN_THIS_CYCLE]);
  PL ("dequeued-in-this-gc", dequeued[GC_STAT_IN_THIS_GC]);
  PL ("dequeued-in-last-cycle", dequeued[GC_STAT_IN_LAST_CYCLE]);
  PL ("dequeued-in-last-gc", dequeued[GC_STAT_IN_LAST_GC]);
  PL ("dequeued-total", dequeued[GC_STAT_TOTAL]);
  PL ("enqueued-in-this-cycle", enqueued[GC_STAT_IN_THIS_CYCLE]);
  PL ("enqueued-in-this-gc", enqueued[GC_STAT_IN_THIS_GC]);
  PL ("enqueued-in-last-cycle", enqueued[GC_STAT_IN_LAST_CYCLE]);
  PL ("enqueued-in-last-gc", enqueued[GC_STAT_IN_LAST_GC]);
  PL ("enqueued-total", enqueued[GC_STAT_TOTAL]);
  PL ("n-cycles-in-this-gc", n_cycles[GC_STAT_IN_THIS_GC]);
  PL ("n-cycles-in-last-gc", n_cycles[GC_STAT_IN_LAST_GC]);
  PL ("n-cycles-total", n_cycles[GC_STAT_TOTAL]);
  PL ("n-gc-total", n_gc[GC_STAT_TOTAL]);
  PL ("phase", phase);
  return pl;
}
#else /* not ERROR_CHECK_GC */
# define GC_STAT_START_NEW_GC
# define GC_STAT_RESUME_GC
# define GC_STAT_ENQUEUED
# define GC_STAT_DEQUEUED
# define GC_STAT_REPUSHED
# define GC_STAT_REMOVED
#endif /* not ERROR_CHECK_GC */
#endif /* NEW_GC */


/************************************************************************/
/*		Recompute need to garbage collect			*/
/************************************************************************/

int need_to_garbage_collect;

#ifdef ERROR_CHECK_GC
int always_gc = 0;    		/* Debugging hack; equivalent to
				   (setq gc-cons-thresold -1) */
#else
#define always_gc 0
#endif

/* True if it's time to garbage collect now. */
void
recompute_need_to_garbage_collect (void)
{
  if (always_gc)
    need_to_garbage_collect = 1;
  else
    need_to_garbage_collect = 
#ifdef NEW_GC
      write_barrier_enabled ? 
      (consing_since_gc > gc_cons_incremental_threshold) :
#endif /* NEW_GC */
      (consing_since_gc > gc_cons_threshold
       &&
#if 0 /* #### implement this better */
       ((double)consing_since_gc) / total_data_usage()) >=
      ((double)gc_cons_percentage / 100)
#else
       (!total_gc_usage_set ||
	((double)consing_since_gc / total_gc_usage) >=
	((double)gc_cons_percentage / 100))
#endif
       );
  recompute_funcall_allocation_flag ();
}



/************************************************************************/
/*			      Mark Phase       				*/
/************************************************************************/

static const struct memory_description lisp_object_description_1[] = {
  { XD_LISP_OBJECT, 0 },
  { XD_END }
};

const struct sized_memory_description lisp_object_description = {
  sizeof (Lisp_Object),
  lisp_object_description_1
};

#if defined (USE_KKCC) || defined (PDUMP)

/* This function extracts the value of a count variable described somewhere 
   else in the description. It is converted corresponding to the type */ 
EMACS_INT
lispdesc_indirect_count_1 (EMACS_INT code,
			   const struct memory_description *idesc,
			   const void *idata)
{
  EMACS_INT count;
  const void *irdata;

  int line = XD_INDIRECT_VAL (code);
  int delta = XD_INDIRECT_DELTA (code);

  irdata = ((char *) idata) +
    lispdesc_indirect_count (idesc[line].offset, idesc, idata);
  switch (idesc[line].type)
    {
    case XD_BYTECOUNT:
      count = * (Bytecount *) irdata;
      break;
    case XD_ELEMCOUNT:
      count = * (Elemcount *) irdata;
      break;
    case XD_HASHCODE:
      count = * (Hashcode *) irdata;
      break;
    case XD_INT:
      count = * (int *) irdata;
      break;
    case XD_LONG:
      count = * (long *) irdata;
      break;
    default:
      stderr_out ("Unsupported count type : %d (line = %d, code = %ld)\n",
		  idesc[line].type, line, (long) code);
#if defined(USE_KKCC) && defined(DEBUG_XEMACS)
      if (gc_in_progress)
	kkcc_backtrace ();
#endif
#ifdef PDUMP
      if (in_pdump)
	pdump_backtrace ();
#endif
      count = 0; /* warning suppression */
      ABORT ();
    }
  count += delta;
  return count;
}

/* SDESC is a "description map" (basically, a list of offsets used for
   successive indirections) and OBJ is the first object to indirect off of.
   Return the description ultimately found. */

const struct sized_memory_description *
lispdesc_indirect_description_1 (const void *obj,
				 const struct sized_memory_description *sdesc)
{
  int pos;

  for (pos = 0; sdesc[pos].size >= 0; pos++)
    obj = * (const void **) ((const char *) obj + sdesc[pos].size);

  return (const struct sized_memory_description *) obj;
}

/* Compute the size of the data at RDATA, described by a single entry
   DESC1 in a description array.  OBJ and DESC are used for
   XD_INDIRECT references. */

static Bytecount
lispdesc_one_description_line_size (void *rdata,
				    const struct memory_description *desc1,
				    const void *obj,
				    const struct memory_description *desc)
{
 union_switcheroo:
  switch (desc1->type)
    {
    case XD_LISP_OBJECT_ARRAY:
      {
	EMACS_INT val = lispdesc_indirect_count (desc1->data1, desc, obj);
	return (val * sizeof (Lisp_Object));
      }
    case XD_LISP_OBJECT:
    case XD_LO_LINK:
      return sizeof (Lisp_Object);
    case XD_OPAQUE_PTR:
      return sizeof (void *);
#ifdef NEW_GC
    case XD_LISP_OBJECT_BLOCK_PTR:
#endif /* NEW_GC */
    case XD_BLOCK_PTR:
      {
	EMACS_INT val = lispdesc_indirect_count (desc1->data1, desc, obj);
	return val * sizeof (void *);
      }
    case XD_BLOCK_ARRAY:
      {
	EMACS_INT val = lispdesc_indirect_count (desc1->data1, desc, obj);
	    
	return (val *
		lispdesc_block_size
		(rdata,
		 lispdesc_indirect_description (obj, desc1->data2.descr)));
      }
    case XD_OPAQUE_DATA_PTR:
      return sizeof (void *);
    case XD_UNION_DYNAMIC_SIZE:
      {
	/* If an explicit size was given in the first-level structure
	   description, use it; else compute size based on current union
	   constant. */
	const struct sized_memory_description *sdesc =
	  lispdesc_indirect_description (obj, desc1->data2.descr);
	if (sdesc->size)
	  return sdesc->size;
	else
	  {
	    desc1 = lispdesc_process_xd_union (desc1, desc, obj);
	    if (desc1)
	      goto union_switcheroo;
	    break;
	  }
      }
    case XD_UNION:
      {
	/* If an explicit size was given in the first-level structure
	   description, use it; else compute size based on maximum of all
	   possible structures. */
	const struct sized_memory_description *sdesc =
	  lispdesc_indirect_description (obj, desc1->data2.descr);
	if (sdesc->size)
	  return sdesc->size;
	else
	  {
	    int count;
	    Bytecount max_size = -1, size;

	    desc1 = sdesc->description;

	    for (count = 0; desc1[count].type != XD_END; count++)
	      {
		size = lispdesc_one_description_line_size (rdata,
							   &desc1[count],
							   obj, desc);
		if (size > max_size)
		  max_size = size;
	      }
	    return max_size;
	  }
      }
    case XD_ASCII_STRING:
      return sizeof (void *);
    case XD_DOC_STRING:
      return sizeof (void *);
    case XD_INT_RESET:
      return sizeof (int);
    case XD_BYTECOUNT:
      return sizeof (Bytecount);
    case XD_ELEMCOUNT:
      return sizeof (Elemcount);
    case XD_HASHCODE:
      return sizeof (Hashcode);
    case XD_INT:
      return sizeof (int);
    case XD_LONG:
      return sizeof (long);
    default:
      stderr_out ("Unsupported dump type : %d\n", desc1->type);
      ABORT ();
    }

  return 0;
}


/* Return the size of the memory block (NOT necessarily a structure!) 
   described by SDESC and pointed to by OBJ.  If SDESC records an
   explicit size (i.e. non-zero), it is simply returned; otherwise,
   the size is calculated by the maximum offset and the size of the
   object at that offset, rounded up to the maximum alignment.  In
   this case, we may need the object, for example when retrieving an
   "indirect count" of an inlined array (the count is not constant,
   but is specified by one of the elements of the memory block). (It
   is generally not a problem if we return an overly large size -- we
   will simply end up reserving more space than necessary; but if the
   size is too small we could be in serious trouble, in particular
   with nested inlined structures, where there may be alignment
   padding in the middle of a block. #### In fact there is an (at
   least theoretical) problem with an overly large size -- we may
   trigger a protection fault when reading from invalid memory.  We
   need to handle this -- perhaps in a stupid but dependable way,
   i.e. by trapping SIGSEGV and SIGBUS.) */

Bytecount
lispdesc_block_size_1 (const void *obj, Bytecount size,
		       const struct memory_description *desc)
{
  EMACS_INT max_offset = -1;
  int max_offset_pos = -1;
  int pos;

  if (size)
    return size;

  for (pos = 0; desc[pos].type != XD_END; pos++)
    {
      EMACS_INT offset = lispdesc_indirect_count (desc[pos].offset, desc, obj);
      if (offset == max_offset)
	{
	  stderr_out ("Two relocatable elements at same offset?\n");
	  ABORT ();
	}
      else if (offset > max_offset)
	{
	  max_offset = offset;
	  max_offset_pos = pos;
	}
    }

  if (max_offset_pos < 0)
    return 0;

  {
    Bytecount size_at_max;
    size_at_max =
      lispdesc_one_description_line_size ((char *) obj + max_offset,
					  &desc[max_offset_pos], obj, desc);

    /* We have no way of knowing the required alignment for this structure,
       so just make it maximally aligned. */
    return MAX_ALIGN_SIZE (max_offset + size_at_max);
  }
}
#endif /* defined (USE_KKCC) || defined (PDUMP) */

#ifdef NEW_GC
#define GC_CHECK_NOT_FREE(lheader)			\
      gc_checking_assert (! LRECORD_FREE_P (lheader));
#else /* not NEW_GC */
#define GC_CHECK_NOT_FREE(lheader)					\
      gc_checking_assert (! LRECORD_FREE_P (lheader));			\
      gc_checking_assert (LHEADER_IMPLEMENTATION (lheader)->basic_p ||	\
			  ! ((struct old_lcrecord_header *) lheader)->free)
#endif /* not NEW_GC */

#ifdef USE_KKCC
/* The following functions implement the new mark algorithm. 
   They mark objects according to their descriptions.  They 
   are modeled on the corresponding pdumper procedures. */

#if 0
# define KKCC_STACK_AS_QUEUE 1
#endif

#ifdef DEBUG_XEMACS
/* The backtrace for the KKCC mark functions. */
#define KKCC_INIT_BT_STACK_SIZE 4096

typedef struct
{
  void *obj;
  const struct memory_description *desc;
  int pos;
} kkcc_bt_stack_entry;

static kkcc_bt_stack_entry *kkcc_bt;
static int kkcc_bt_stack_size;
static int kkcc_bt_depth = 0;

static void
kkcc_bt_init (void)
{
  kkcc_bt_depth = 0;
  kkcc_bt_stack_size = KKCC_INIT_BT_STACK_SIZE;
  kkcc_bt = (kkcc_bt_stack_entry *)
    xmalloc_and_zero (kkcc_bt_stack_size * sizeof (kkcc_bt_stack_entry));
  if (!kkcc_bt)
    {
      stderr_out ("KKCC backtrace stack init failed for size %d\n",
		  kkcc_bt_stack_size);
      ABORT ();
    }
}

void
kkcc_backtrace (void)
{
  int i;
  stderr_out ("KKCC mark stack backtrace :\n");
  for (i = kkcc_bt_depth - 1; i >= 0; i--)
    {
      Lisp_Object obj = wrap_pointer_1 (kkcc_bt[i].obj);
      stderr_out (" [%d]", i);
      if ((XRECORD_LHEADER (obj)->type >= lrecord_type_last_built_in_type)
	  || (!LRECORDP (obj))
	  || (!XRECORD_LHEADER_IMPLEMENTATION (obj)))
	{
	  stderr_out (" non Lisp Object");
	}
      else
	{
	  stderr_out (" %s",
		      XRECORD_LHEADER_IMPLEMENTATION (obj)->name);
	}
      stderr_out (" (addr: %p, desc: %p, ",
		  (void *) kkcc_bt[i].obj,
		  (void *) kkcc_bt[i].desc);
      if (kkcc_bt[i].pos >= 0)
	stderr_out ("pos: %d)\n", kkcc_bt[i].pos);
      else
	if (kkcc_bt[i].pos == -1)
	  stderr_out ("root set)\n");
	else if (kkcc_bt[i].pos == -2)
	  stderr_out ("dirty object)\n");
    }
}

static void
kkcc_bt_stack_realloc (void)
{
  kkcc_bt_stack_size *= 2;
  kkcc_bt = (kkcc_bt_stack_entry *)
    xrealloc (kkcc_bt, kkcc_bt_stack_size * sizeof (kkcc_bt_stack_entry));
  if (!kkcc_bt)
    {
      stderr_out ("KKCC backtrace stack realloc failed for size %d\n", 
		  kkcc_bt_stack_size);
      ABORT ();
    }
}

static void
kkcc_bt_free (void)
{
  xfree_1 (kkcc_bt);
  kkcc_bt = 0;
  kkcc_bt_stack_size = 0;
}

static void
kkcc_bt_push (void *obj, const struct memory_description *desc, 
	      int level, int pos)
{
  kkcc_bt_depth = level;
  kkcc_bt[kkcc_bt_depth].obj = obj;
  kkcc_bt[kkcc_bt_depth].desc = desc;
  kkcc_bt[kkcc_bt_depth].pos = pos;
  kkcc_bt_depth++;
  if (kkcc_bt_depth >= kkcc_bt_stack_size)
    kkcc_bt_stack_realloc ();
}

#else /* not DEBUG_XEMACS */
#define kkcc_bt_init()
#define kkcc_bt_push(obj, desc, level, pos)
#endif /* not DEBUG_XEMACS */

/* Object memory descriptions are in the lrecord_implementation structure.
   But copying them to a parallel array is much more cache-friendly. */
const struct memory_description *lrecord_memory_descriptions[countof (lrecord_implementations_table)];

/* the initial stack size in kkcc_gc_stack_entries */
#define KKCC_INIT_GC_STACK_SIZE 16384

typedef struct
{
  void *data;
  const struct memory_description *desc;
#ifdef DEBUG_XEMACS
  int level;
  int pos;
#endif
} kkcc_gc_stack_entry;


static kkcc_gc_stack_entry *kkcc_gc_stack_ptr;
static int kkcc_gc_stack_front;
static int kkcc_gc_stack_rear;
static int kkcc_gc_stack_size;

#define KKCC_INC(i) ((i + 1) % kkcc_gc_stack_size)
#define KKCC_INC2(i) ((i + 2) % kkcc_gc_stack_size)

#define KKCC_GC_STACK_FULL (KKCC_INC2 (kkcc_gc_stack_rear) == kkcc_gc_stack_front)
#define KKCC_GC_STACK_EMPTY (KKCC_INC (kkcc_gc_stack_rear) == kkcc_gc_stack_front)

static void
kkcc_gc_stack_init (void)
{
  kkcc_gc_stack_size = KKCC_INIT_GC_STACK_SIZE;
  kkcc_gc_stack_ptr = (kkcc_gc_stack_entry *)
    xmalloc_and_zero (kkcc_gc_stack_size * sizeof (kkcc_gc_stack_entry));
  if (!kkcc_gc_stack_ptr) 
    {
      stderr_out ("stack init failed for size %d\n", kkcc_gc_stack_size);
      ABORT ();
    }
  kkcc_gc_stack_front = 0;
  kkcc_gc_stack_rear = kkcc_gc_stack_size - 1;
}

static void
kkcc_gc_stack_free (void)
{
  xfree_1 (kkcc_gc_stack_ptr);
  kkcc_gc_stack_ptr = 0;
  kkcc_gc_stack_front = 0;
  kkcc_gc_stack_rear = 0;
  kkcc_gc_stack_size = 0;
}

static void
kkcc_gc_stack_realloc (void)
{
  kkcc_gc_stack_entry *old_ptr = kkcc_gc_stack_ptr;
  int old_size = kkcc_gc_stack_size;
  kkcc_gc_stack_size *= 2;
  kkcc_gc_stack_ptr = (kkcc_gc_stack_entry *)
    xmalloc_and_zero (kkcc_gc_stack_size * sizeof (kkcc_gc_stack_entry));
  if (!kkcc_gc_stack_ptr)
    {
      stderr_out ("stack realloc failed for size %d\n", kkcc_gc_stack_size);
      ABORT ();
    }
  if (kkcc_gc_stack_rear >= kkcc_gc_stack_front)
    {
      int number_elements = kkcc_gc_stack_rear - kkcc_gc_stack_front + 1;
      memcpy (kkcc_gc_stack_ptr, &old_ptr[kkcc_gc_stack_front], 
	      number_elements * sizeof (kkcc_gc_stack_entry));
      kkcc_gc_stack_front = 0;
      kkcc_gc_stack_rear = number_elements - 1;
    }
  else
    {
      int number_elements = old_size - kkcc_gc_stack_front;
      memcpy (kkcc_gc_stack_ptr, &old_ptr[kkcc_gc_stack_front],
	      number_elements * sizeof (kkcc_gc_stack_entry));
      memcpy (&kkcc_gc_stack_ptr[number_elements], &old_ptr[0],
	      (kkcc_gc_stack_rear + 1) * sizeof (kkcc_gc_stack_entry));
      kkcc_gc_stack_front = 0;
      kkcc_gc_stack_rear = kkcc_gc_stack_rear + number_elements;
    }
  xfree_1 (old_ptr);
}

static void
#ifdef DEBUG_XEMACS
kkcc_gc_stack_push_1 (void *data, const struct memory_description *desc,
		    int level, int pos)
#else
kkcc_gc_stack_push_1 (void *data, const struct memory_description *desc)
#endif
{
#ifdef NEW_GC
  GC_STAT_ENQUEUED;
#endif /* NEW_GC */
  if (KKCC_GC_STACK_FULL)
      kkcc_gc_stack_realloc();
  kkcc_gc_stack_rear = KKCC_INC (kkcc_gc_stack_rear);
  kkcc_gc_stack_ptr[kkcc_gc_stack_rear].data = data;
  kkcc_gc_stack_ptr[kkcc_gc_stack_rear].desc = desc;
#ifdef DEBUG_XEMACS
  kkcc_gc_stack_ptr[kkcc_gc_stack_rear].level = level;
  kkcc_gc_stack_ptr[kkcc_gc_stack_rear].pos = pos;
#endif
}

#ifdef DEBUG_XEMACS
#define kkcc_gc_stack_push(data, desc, level, pos)	\
  kkcc_gc_stack_push_1 (data, desc, level, pos)
#else
#define kkcc_gc_stack_push(data, desc, level, pos)	\
  kkcc_gc_stack_push_1 (data, desc)
#endif

static kkcc_gc_stack_entry *
kkcc_gc_stack_pop (void)
{
  if (KKCC_GC_STACK_EMPTY)
    return 0;
#ifdef NEW_GC
  GC_STAT_DEQUEUED;
#endif /* NEW_GC */
#ifndef KKCC_STACK_AS_QUEUE
  /* stack behaviour */
  return &kkcc_gc_stack_ptr[kkcc_gc_stack_rear--];
#else
  /* queue behaviour */
  {
    int old_front = kkcc_gc_stack_front;
    kkcc_gc_stack_front = KKCC_INC (kkcc_gc_stack_front);
    return &kkcc_gc_stack_ptr[old_front];
  }
#endif
}

void
#ifdef DEBUG_XEMACS
kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj, int level, int pos)
#else
kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj)
#endif
{
  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);
      const struct memory_description *desc;
      GC_CHECK_LHEADER_INVARIANTS (lheader);
      desc = RECORD_DESCRIPTION (lheader);
      if (! MARKED_RECORD_HEADER_P (lheader)) 
	{
#ifdef NEW_GC
	  MARK_GREY (lheader);
#else /* not NEW_GC */
	  MARK_RECORD_HEADER (lheader);
#endif /* not NEW_GC */
	  kkcc_gc_stack_push ((void *) lheader, desc, level, pos);
	}
    }
}

#ifdef NEW_GC
#ifdef DEBUG_XEMACS
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj, level, pos)
#else
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj)
#endif

void
#ifdef DEBUG_XEMACS
kkcc_gc_stack_repush_dirty_object_1 (Lisp_Object obj, int level, int pos)
#else
kkcc_gc_stack_repush_dirty_object_1 (Lisp_Object obj)
#endif
{
  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);
      const struct memory_description *desc;
      GC_STAT_REPUSHED;
      GC_CHECK_LHEADER_INVARIANTS (lheader);
      desc = RECORD_DESCRIPTION (lheader);
      MARK_GREY (lheader);
      kkcc_gc_stack_push ((void*) lheader, desc, level, pos);
    }
}
#endif /* NEW_GC */

#ifdef ERROR_CHECK_GC
#define KKCC_DO_CHECK_FREE(obj, allow_free)			\
do								\
{								\
  if (!allow_free && XTYPE (obj) == Lisp_Type_Record)		\
    {								\
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);	\
      GC_CHECK_NOT_FREE (lheader);				\
    }								\
} while (0)
#else
#define KKCC_DO_CHECK_FREE(obj, allow_free)
#endif

#ifdef ERROR_CHECK_GC
#ifdef DEBUG_XEMACS
static void
mark_object_maybe_checking_free_1 (Lisp_Object obj, int allow_free,
				 int level, int pos)
#else
static void
mark_object_maybe_checking_free_1 (Lisp_Object obj, int allow_free)
#endif
{
  KKCC_DO_CHECK_FREE (obj, allow_free);
  kkcc_gc_stack_push_lisp_object (obj, level, pos);
}

#ifdef DEBUG_XEMACS
#define mark_object_maybe_checking_free(obj, allow_free, level, pos) \
  mark_object_maybe_checking_free_1 (obj, allow_free, level, pos)
#else
#define mark_object_maybe_checking_free(obj, allow_free, level, pos) \
  mark_object_maybe_checking_free_1 (obj, allow_free)
#endif
#else /* not ERROR_CHECK_GC */
#define mark_object_maybe_checking_free(obj, allow_free, level, pos) 	\
  kkcc_gc_stack_push_lisp_object (obj, level, pos)
#endif /* not ERROR_CHECK_GC */


/* This function loops all elements of a struct pointer and calls 
   mark_with_description with each element. */
static void
#ifdef DEBUG_XEMACS
mark_struct_contents_1 (const void *data,
		      const struct sized_memory_description *sdesc,
		      int count, int level, int pos)
#else
mark_struct_contents_1 (const void *data,
		      const struct sized_memory_description *sdesc,
		      int count)
#endif
{
  int i;
  Bytecount elsize;
  elsize = lispdesc_block_size (data, sdesc);

  for (i = 0; i < count; i++)
    {
      kkcc_gc_stack_push (((char *) data) + elsize * i, sdesc->description,
			  level, pos);
    }
}

#ifdef DEBUG_XEMACS
#define mark_struct_contents(data, sdesc, count, level, pos) \
  mark_struct_contents_1 (data, sdesc, count, level, pos)
#else
#define mark_struct_contents(data, sdesc, count, level, pos) \
  mark_struct_contents_1 (data, sdesc, count)
#endif


#ifdef NEW_GC
/* This function loops all elements of a struct pointer and calls 
   mark_with_description with each element. */
static void
#ifdef DEBUG_XEMACS
mark_lisp_object_block_contents_1 (const void *data,
		      const struct sized_memory_description *sdesc,
		      int count, int level, int pos)
#else
mark_lisp_object_block_contents_1 (const void *data,
		      const struct sized_memory_description *sdesc,
		      int count)
#endif
{
  int i;
  Bytecount elsize;
  elsize = lispdesc_block_size (data, sdesc);

  for (i = 0; i < count; i++)
    {
      const Lisp_Object obj = wrap_pointer_1 (((char *) data) + elsize * i);
      if (XTYPE (obj) == Lisp_Type_Record)
	{
	  struct lrecord_header *lheader = XRECORD_LHEADER (obj);
	  const struct memory_description *desc;
	  GC_CHECK_LHEADER_INVARIANTS (lheader);
	  desc = sdesc->description;
	  if (! MARKED_RECORD_HEADER_P (lheader)) 
	    {
	      MARK_GREY (lheader);
	      kkcc_gc_stack_push ((void *) lheader, desc, level, pos);
	    }
	}
    }
}

#ifdef DEBUG_XEMACS
#define mark_lisp_object_block_contents(data, sdesc, count, level, pos) \
  mark_lisp_object_block_contents_1 (data, sdesc, count, level, pos)
#else
#define mark_lisp_object_block_contents(data, sdesc, count, level, pos) \
  mark_lisp_object_block_contents_1 (data, sdesc, count)
#endif
#endif /* not NEW_GC */

/* This function implements the KKCC mark algorithm.
   Instead of calling mark_object, all the alive Lisp_Objects are pushed
   on the kkcc_gc_stack. This function processes all elements on the stack
   according to their descriptions. */
static void
kkcc_marking (
#ifdef NEW_GC
	      int cnt
#else /* not NEW_GC */
	      int UNUSED(cnt)
#endif /* not NEW_GC */ 
	      )
{
  kkcc_gc_stack_entry *stack_entry = 0;
  void *data = 0;
  const struct memory_description *desc = 0;
  int pos;
#ifdef NEW_GC
  int count = cnt;
#endif /* NEW_GC */
#ifdef DEBUG_XEMACS
  int level = 0;
#endif
  
  while ((stack_entry = kkcc_gc_stack_pop ()) != 0)
    {
      data = stack_entry->data;
      desc = stack_entry->desc;
#ifdef DEBUG_XEMACS
      level = stack_entry->level + 1;
#endif
      kkcc_bt_push (data, desc, stack_entry->level, stack_entry->pos);

#ifdef NEW_GC
      /* Mark black if object is currently grey.  This first checks,
	 if the object is really allocated on the mc-heap.  If it is,
	 it can be marked black; if it is not, it cannot be marked. */
      maybe_mark_black (data);
#endif /* NEW_GC */

      if (!data) continue;

      gc_checking_assert (data);
      gc_checking_assert (desc);

      for (pos = 0; desc[pos].type != XD_END; pos++)
	{
	  const struct memory_description *desc1 = &desc[pos];
	  const void *rdata =
	    (const char *) data + lispdesc_indirect_count (desc1->offset,
							   desc, data);
	union_switcheroo:
	  
	  /* If the flag says don't mark, then don't mark. */
	  if ((desc1->flags) & XD_FLAG_NO_KKCC)
	    continue;

	  switch (desc1->type)
	    {
	    case XD_BYTECOUNT:
	    case XD_ELEMCOUNT:
	    case XD_HASHCODE:
	    case XD_INT:
	    case XD_LONG:
	    case XD_INT_RESET:
	    case XD_LO_LINK:
	    case XD_OPAQUE_PTR:
	    case XD_OPAQUE_DATA_PTR:
	    case XD_ASCII_STRING:
	    case XD_DOC_STRING:
	      break;
	    case XD_LISP_OBJECT: 
	      {
		const Lisp_Object *stored_obj = (const Lisp_Object *) rdata;

		/* Because of the way that tagged objects work (pointers and
		   Lisp_Objects have the same representation), XD_LISP_OBJECT
		   can be used for untagged pointers.  They might be NULL,
		   though. */
		if (EQ (*stored_obj, Qnull_pointer))
		  break;
#ifdef NEW_GC
		mark_object_maybe_checking_free (*stored_obj, 0, level, pos);
#else /* not NEW_GC */
		mark_object_maybe_checking_free
		  (*stored_obj, (desc1->flags) & XD_FLAG_FREE_LISP_OBJECT,
		   level, pos);
#endif /* not NEW_GC */
		break;
	      }
	    case XD_LISP_OBJECT_ARRAY:
	      {
		int i;
		EMACS_INT count =
		  lispdesc_indirect_count (desc1->data1, desc, data);
	
		for (i = 0; i < count; i++)
		  {
		    const Lisp_Object *stored_obj =
		      (const Lisp_Object *) rdata + i;

		    if (EQ (*stored_obj, Qnull_pointer))
		      break;
#ifdef NEW_GC
		    mark_object_maybe_checking_free 
		      (*stored_obj, 0, level, pos);
#else /* not NEW_GC */
		    mark_object_maybe_checking_free
		      (*stored_obj, (desc1->flags) & XD_FLAG_FREE_LISP_OBJECT,
		       level, pos);
#endif /* not NEW_GC */
		  }
		break;
	      }
#ifdef NEW_GC
	    case XD_LISP_OBJECT_BLOCK_PTR:
	      {
		EMACS_INT count = lispdesc_indirect_count (desc1->data1, desc,
							   data);
		const struct sized_memory_description *sdesc =
		  lispdesc_indirect_description (data, desc1->data2.descr);
		const char *dobj = * (const char **) rdata;
		if (dobj)
		  mark_lisp_object_block_contents 
		    (dobj, sdesc, count, level, pos);
		break;
	      }
#endif /* NEW_GC */
	    case XD_BLOCK_PTR:
	      {
		EMACS_INT count = lispdesc_indirect_count (desc1->data1, desc,
							   data);
		const struct sized_memory_description *sdesc =
		  lispdesc_indirect_description (data, desc1->data2.descr);
		const char *dobj = * (const char **) rdata;
		if (dobj)
		  mark_struct_contents (dobj, sdesc, count, level, pos);
		break;
	      }
	    case XD_BLOCK_ARRAY:
	      {
		EMACS_INT count = lispdesc_indirect_count (desc1->data1, desc,
							   data);
		const struct sized_memory_description *sdesc =
		  lispdesc_indirect_description (data, desc1->data2.descr);
		      
		mark_struct_contents (rdata, sdesc, count, level, pos);
		break;
	      }
	    case XD_UNION:
	    case XD_UNION_DYNAMIC_SIZE:
	      desc1 = lispdesc_process_xd_union (desc1, desc, data);
	      if (desc1)
		goto union_switcheroo;
	      break;
		    
	    default:
	      stderr_out ("Unsupported description type : %d\n", desc1->type);
	      kkcc_backtrace ();
	      ABORT ();
	    }
	}

#ifdef NEW_GC
      if (cnt) 
	if (!--count)
	  break;
#endif /* NEW_GC */
    }
}
#endif /* USE_KKCC */

/* I hate duplicating all this crap! */
int
marked_p (Lisp_Object obj)
{
  /* Checks we used to perform. */
  /* if (EQ (obj, Qnull_pointer)) return 1; */
  /* if (!POINTER_TYPE_P (XGCTYPE (obj))) return 1; */
  /* if (PURIFIED (XPNTR (obj))) return 1; */

  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);

      GC_CHECK_LHEADER_INVARIANTS (lheader);

      return MARKED_RECORD_HEADER_P (lheader);
    }
  return 1;
}


/* Mark reference to a Lisp_Object.  If the object referred to has not been
   seen yet, recursively mark all the references contained in it. */
void
mark_object (
#ifdef USE_KKCC
	     Lisp_Object UNUSED (obj)
#else
	     Lisp_Object obj
#endif
	     )
{
#ifdef USE_KKCC
  /* this code should never be reached when configured for KKCC */
  stderr_out ("KKCC: Invalid mark_object call.\n");
  stderr_out ("Replace mark_object with kkcc_gc_stack_push_lisp_object.\n");
  ABORT ();
#else /* not USE_KKCC */

 tail_recurse:

  /* Checks we used to perform */
  /* if (EQ (obj, Qnull_pointer)) return; */
  /* if (!POINTER_TYPE_P (XGCTYPE (obj))) return; */
  /* if (PURIFIED (XPNTR (obj))) return; */

  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);

      GC_CHECK_LHEADER_INVARIANTS (lheader);

      /* We handle this separately, above, so we can mark free objects */
      GC_CHECK_NOT_FREE (lheader);

      /* All c_readonly objects have their mark bit set,
	 so that we only need to check the mark bit here. */
      if (! MARKED_RECORD_HEADER_P (lheader))
	{
	  MARK_RECORD_HEADER (lheader);

	  if (RECORD_MARKER (lheader))
	    {
	      obj = RECORD_MARKER (lheader) (obj);
	      if (!NILP (obj)) goto tail_recurse;
	    }
	}
    }
#endif /* not KKCC */
}


/************************************************************************/
/*			       Hooks         				*/
/************************************************************************/

/* Nonzero when calling certain hooks or doing other things where a GC
   would be bad. It prevents infinite recursive calls to gc. */
int gc_currently_forbidden;

int
begin_gc_forbidden (void)
{
  return internal_bind_int (&gc_currently_forbidden, 1);
}

void
end_gc_forbidden (int count)
{
  unbind_to (count);
}

/* Hooks. */
Lisp_Object Vpre_gc_hook, Qpre_gc_hook;
Lisp_Object Vpost_gc_hook, Qpost_gc_hook;

/* Maybe we want to use this when doing a "panic" gc after memory_full()? */
static int gc_hooks_inhibited;

struct post_gc_action
{
  void (*fun) (void *);
  void *arg;
};

typedef struct post_gc_action post_gc_action;

typedef struct
{
  Dynarr_declare (post_gc_action);
} post_gc_action_dynarr;

static post_gc_action_dynarr *post_gc_actions;

/* Register an action to be called at the end of GC.
   gc_in_progress is 0 when this is called.
   This is used when it is discovered that an action needs to be taken,
   but it's during GC, so it's not safe. (e.g. in a finalize method.)

   As a general rule, do not use Lisp objects here.
   And NEVER signal an error.
*/

void
register_post_gc_action (void (*fun) (void *), void *arg)
{
  post_gc_action action;

  if (!post_gc_actions)
    post_gc_actions = Dynarr_new (post_gc_action);

  action.fun = fun;
  action.arg = arg;

  Dynarr_add (post_gc_actions, action);
}

static void
run_post_gc_actions (void)
{
  int i;

  if (post_gc_actions)
    {
      for (i = 0; i < Dynarr_length (post_gc_actions); i++)
	{
	  post_gc_action action = Dynarr_at (post_gc_actions, i);
	  (action.fun) (action.arg);
	}

      Dynarr_reset (post_gc_actions);
    }
}

#ifdef NEW_GC
/* Asynchronous finalization. */
typedef struct finalize_elem
{
  Lisp_Object obj;
  struct finalize_elem *next;
} finalize_elem;

finalize_elem *Vall_finalizable_objs;
Lisp_Object Vfinalizers_to_run;

void
add_finalizable_obj (Lisp_Object obj)
{
  finalize_elem *next = Vall_finalizable_objs;
  Vall_finalizable_objs =
    (finalize_elem *) xmalloc_and_zero (sizeof (finalize_elem));
  Vall_finalizable_objs->obj = obj;
  Vall_finalizable_objs->next = next;
}

void
register_for_finalization (void)
{
  finalize_elem *rest = Vall_finalizable_objs;

  if (!rest) 
    return;

  while (!marked_p (rest->obj))
    {
      finalize_elem *temp = rest;
      Vfinalizers_to_run = Fcons (rest->obj, Vfinalizers_to_run);
      Vall_finalizable_objs = rest->next;
      xfree (temp, finalize_elem *);
      rest = Vall_finalizable_objs;
    }

  while (rest->next)
    {
      if (LRECORDP (rest->next->obj)
	  && !marked_p (rest->next->obj))
	{
	  finalize_elem *temp = rest->next;
	  Vfinalizers_to_run = Fcons (rest->next->obj, Vfinalizers_to_run);
	  rest->next = rest->next->next;
	  xfree (temp, finalize_elem *);
	}
      else
	{
	  rest = rest->next;
	}
    }
  /* Keep objects alive that need to be finalized by marking
     Vfinalizers_to_run transitively. */
  kkcc_gc_stack_push_lisp_object (Vfinalizers_to_run, 0, -1);
  kkcc_marking (0);
}

void
run_finalizers (void)
{
  Lisp_Object rest;
  for (rest = Vfinalizers_to_run; !NILP (rest); rest = XCDR (rest))
    {
      MC_ALLOC_CALL_FINALIZER (XPNTR (XCAR (rest)));
    }
  Vfinalizers_to_run = Qnil;
}
#endif /* not NEW_GC */


/************************************************************************/
/*			    Garbage Collection				*/
/************************************************************************/

/* Enable/disable incremental garbage collection during runtime. */
int allow_incremental_gc;

/* For profiling. */
static Lisp_Object QSin_garbage_collection;

/* Nonzero means display messages at beginning and end of GC.  */
int garbage_collection_messages;

/* "Garbage collecting" */
Lisp_Object Vgc_message;
Lisp_Object Vgc_pointer_glyph;
static const Ascbyte gc_default_message[] = "Garbage collecting";
Lisp_Object Qgarbage_collecting;

/* "Locals" during GC. */
struct frame *f;
int speccount;
int cursor_changed;
Lisp_Object pre_gc_cursor;

/* PROFILE_DECLARE */
int do_backtrace;
struct backtrace backtrace;

/* Maximum amount of C stack to save when a GC happens.  */
#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 0 /* 16000 */
#endif

void
show_gc_cursor_and_message (void) 
{
  /* Now show the GC cursor/message. */
  pre_gc_cursor = Qnil;
  cursor_changed = 0;

  /* We used to call selected_frame() here.

     The following functions cannot be called inside GC
     so we move to after the above tests. */
  {
    Lisp_Object frame;
    Lisp_Object device = Fselected_device (Qnil);
    if (NILP (device)) /* Could happen during startup, eg. if always_gc */
      return;
    frame = Fselected_frame (device);
    if (NILP (frame))
      invalid_state ("No frames exist on device", device);
    f = XFRAME (frame);
  }

  if (!noninteractive)
    {
      if (FRAME_WIN_P (f))
	{
	  Lisp_Object frame = wrap_frame (f);
	  Lisp_Object cursor = glyph_image_instance (Vgc_pointer_glyph,
						     FRAME_SELECTED_WINDOW (f),
						     ERROR_ME_NOT, 1);
	  pre_gc_cursor = f->pointer;
	  if (POINTER_IMAGE_INSTANCEP (cursor)
	      /* don't change if we don't know how to change back. */
	      && POINTER_IMAGE_INSTANCEP (pre_gc_cursor))
	    {
	      cursor_changed = 1;
	      Fset_frame_pointer (frame, cursor);
	    }
	}

      /* Don't print messages to the stream device. */
      if (!cursor_changed && !FRAME_STREAM_P (f))
	{
	  if (garbage_collection_messages)
	    {
	      Lisp_Object args[2], whole_msg;
	      args[0] = (STRINGP (Vgc_message) ? Vgc_message :
			 build_msg_string (gc_default_message));
	      args[1] = build_string ("...");
	      whole_msg = Fconcat (2, args);
	      echo_area_message (f, (Ibyte *) 0, whole_msg, 0, -1,
				 Qgarbage_collecting);
	    }
	}
    }
}

void
remove_gc_cursor_and_message (void)
{
  /* Now remove the GC cursor/message */
  if (!noninteractive)
    {
      if (cursor_changed)
	Fset_frame_pointer (wrap_frame (f), pre_gc_cursor);
      else if (!FRAME_STREAM_P (f))
	{
	  /* Show "...done" only if the echo area would otherwise be empty. */
	  if (NILP (clear_echo_area (selected_frame (),
				     Qgarbage_collecting, 0)))
	    {
	      if (garbage_collection_messages)
		{
		  Lisp_Object args[2], whole_msg;
		  args[0] = (STRINGP (Vgc_message) ? Vgc_message :
			     build_msg_string (gc_default_message));
		  args[1] = build_msg_string ("... done");
		  whole_msg = Fconcat (2, args);
		  echo_area_message (selected_frame (), (Ibyte *) 0,
				     whole_msg, 0, -1,
				     Qgarbage_collecting);
		}
	    }
	}
    }
}

void
gc_prepare (void)
{
#if MAX_SAVE_STACK > 0
  char stack_top_variable;
  extern char *stack_bottom;
#endif

#ifdef NEW_GC
  GC_STAT_START_NEW_GC;
  GC_SET_PHASE (INIT_GC);
#endif /* NEW_GC */

  do_backtrace = profiling_active || backtrace_with_internal_sections;

  assert (!gc_in_progress);
  assert (!in_display || gc_currently_forbidden);

  PROFILE_RECORD_ENTERING_SECTION (QSin_garbage_collection);

  need_to_signal_post_gc = 0;
  recompute_funcall_allocation_flag ();

  if (!gc_hooks_inhibited)
    run_hook_trapping_problems
      (Qgarbage_collecting, Qpre_gc_hook,
       INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION);

  /***** Now we actually start the garbage collection. */

  gc_in_progress = 1;
#ifndef NEW_GC
  inhibit_non_essential_conversion_operations = 1;
#endif /* not NEW_GC */

#if MAX_SAVE_STACK > 0

  /* Save a copy of the contents of the stack, for debugging.  */
  if (!purify_flag)
    {
      /* Static buffer in which we save a copy of the C stack at each GC.  */
      static char *stack_copy;
      static Bytecount stack_copy_size;

      ptrdiff_t stack_diff = &stack_top_variable - stack_bottom;
      Bytecount stack_size = (stack_diff > 0 ? stack_diff : -stack_diff);
      if (stack_size < MAX_SAVE_STACK)
	{
	  if (stack_copy_size < stack_size)
	    {
	      stack_copy = (char *) xrealloc (stack_copy, stack_size);
	      stack_copy_size = stack_size;
	    }

	  memcpy (stack_copy,
		  stack_diff > 0 ? stack_bottom : &stack_top_variable,
		  stack_size);
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  /* Do some totally ad-hoc resource clearing. */
  /* #### generalize this? */
  clear_event_resource ();
  cleanup_specifiers ();
  cleanup_buffer_undo_lists ();
}

void
gc_mark_root_set (
#ifdef NEW_GC
		  enum gc_phase phase
#else /* not NEW_GC */
		  void
#endif /* not NEW_GC */
		  )
{
#ifdef NEW_GC
  GC_SET_PHASE (phase);
#endif /* NEW_GC */

  /* Mark all the special slots that serve as the roots of accessibility. */

#ifdef USE_KKCC
# define mark_object(obj) kkcc_gc_stack_push_lisp_object (obj, 0, -1)
#endif /* USE_KKCC */

  { /* staticpro() */
    Lisp_Object **p = Dynarr_begin (staticpros);
    Elemcount count;
    for (count = Dynarr_length (staticpros); count; count--, p++)
      /* Need to check if the pointer in the staticpro array is not
	 NULL. A gc can occur after variable is added to the staticpro
	 array and _before_ it is correctly initialized. In this case
	 its value is NULL, which we have to catch here. */
      if (*p)
	mark_object (**p);
  }

  { /* staticpro_nodump() */
    Lisp_Object **p = Dynarr_begin (staticpros_nodump);
    Elemcount count;
    for (count = Dynarr_length (staticpros_nodump); count; count--, p++)
      /* Need to check if the pointer in the staticpro array is not
	 NULL. A gc can occur after variable is added to the staticpro
	 array and _before_ it is correctly initialized. In this case
	 its value is NULL, which we have to catch here. */
      if (*p)
	mark_object (**p);
  }

#ifdef NEW_GC
  { /* mcpro () */
    Lisp_Object *p = Dynarr_begin (mcpros);
    Elemcount count;
    for (count = Dynarr_length (mcpros); count; count--)
      mark_object (*p++);
  }
#endif /* NEW_GC */

  { /* GCPRO() */
    struct gcpro *tail;
    int i;
    for (tail = gcprolist; tail; tail = tail->next)
      for (i = 0; i < tail->nvars; i++)
	mark_object (tail->var[i]);
  }

  { /* specbind() */
    struct specbinding *bind;
    for (bind = specpdl; bind != specpdl_ptr; bind++)
      {
	mark_object (bind->symbol);
	mark_object (bind->old_value);
      }
  }

  {
    struct catchtag *c;
    for (c = catchlist; c; c = c->next)
      {
	mark_object (c->tag);
	mark_object (c->val);
	mark_object (c->actual_tag);
	mark_object (c->backtrace);
      }
  }

  {
    struct backtrace *backlist;
    for (backlist = backtrace_list; backlist; backlist = backlist->next)
      {
	int nargs = backlist->nargs;
	int i;

	mark_object (*backlist->function);
	if (nargs < 0 /* nargs == UNEVALLED || nargs == MANY */
	    /* might be fake (internal profiling entry) */
	    && backlist->args)
	  mark_object (backlist->args[0]);
	else
	  for (i = 0; i < nargs; i++)
	    mark_object (backlist->args[i]);
      }
  }

  mark_profiling_info ();
#ifdef USE_KKCC
# undef mark_object
#endif
}

void
gc_finish_mark (void)
{
#ifdef NEW_GC
  GC_SET_PHASE (FINISH_MARK);
#endif /* NEW_GC */
  init_marking_ephemerons ();

  while (finish_marking_weak_hash_tables () > 0 ||
	 finish_marking_weak_lists       () > 0 ||
	 continue_marking_ephemerons     () > 0)
#ifdef USE_KKCC
    {
      kkcc_marking (0);
    }
#else /* not USE_KKCC */
  ;
#endif /* not USE_KKCC */

  /* At this point, we know which objects need to be finalized: we
     still need to resurrect them */

  while (finish_marking_ephemerons       () > 0 ||
	 finish_marking_weak_lists       () > 0 ||
	 finish_marking_weak_hash_tables () > 0)
#ifdef USE_KKCC
    {
      kkcc_marking (0);
    }
#else /* not USE_KKCC */
  ;
#endif /* not USE_KKCC */

  /* And prune (this needs to be called after everything else has been
     marked and before we do any sweeping). */
  /* #### this is somewhat ad-hoc and should probably be an object
     method */
  prune_weak_hash_tables ();
  prune_weak_lists ();
  prune_specifiers ();
  prune_syntax_tables ();

  prune_ephemerons ();
  prune_weak_boxes ();
}

#ifdef NEW_GC
void
gc_finalize (void)
{
  GC_SET_PHASE (FINALIZE);
  register_for_finalization ();
}

void
gc_sweep (void)
{
  GC_SET_PHASE (SWEEP);
  mc_sweep ();
}
#endif /* NEW_GC */


void
gc_finish (void)
{
#ifdef NEW_GC
  GC_SET_PHASE (FINISH_GC);
#endif /* NEW_GC */
  consing_since_gc = 0;
#ifndef DEBUG_XEMACS
  /* Allow you to set it really fucking low if you really want ... */
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;
#endif
  recompute_need_to_garbage_collect ();

#ifndef NEW_GC
  inhibit_non_essential_conversion_operations = 0;
#endif /* not NEW_GC */
  gc_in_progress = 0;

  run_post_gc_actions ();

  /******* End of garbage collection ********/

#ifndef NEW_GC
  if (!breathing_space)
    {
      breathing_space = malloc (4096 - MALLOC_OVERHEAD);
    }
#endif /* not NEW_GC */

  need_to_signal_post_gc = 1;
  funcall_allocation_flag = 1;

  PROFILE_RECORD_EXITING_SECTION (QSin_garbage_collection);

#ifdef NEW_GC
  GC_SET_PHASE (NONE);
#endif /* NEW_GC */
}

#ifdef NEW_GC
void
gc_suspend_mark_phase (void)
{
  PROFILE_RECORD_EXITING_SECTION (QSin_garbage_collection);
  write_barrier_enabled = 1;
  consing_since_gc = 0;
  vdb_start_dirty_bits_recording ();
}

int
gc_resume_mark_phase (void)
{
  PROFILE_RECORD_ENTERING_SECTION (QSin_garbage_collection);
  assert (write_barrier_enabled);
  vdb_stop_dirty_bits_recording ();
  write_barrier_enabled = 0;
  return vdb_read_dirty_bits ();
}

int
gc_mark (int incremental)
{
  GC_SET_PHASE (MARK);
  if (!incremental)
    {
      kkcc_marking (0);
    }
  else 
    {
      kkcc_marking (gc_incremental_traversal_threshold);
      if (!KKCC_GC_STACK_EMPTY)
	{
	  gc_suspend_mark_phase ();
	  return 0;
	}
    }
  return 1;
}

int
gc_resume_mark (int incremental)
{
  if (!incremental)
    {
      if (!KKCC_GC_STACK_EMPTY)
	{
	  GC_STAT_RESUME_GC;
	  /* An incremental garbage collection is already running ---
	     now wrap it up and resume it atomically. */
	  gc_resume_mark_phase ();
	  gc_mark_root_set (REPUSH_ROOT_SET);
	  kkcc_marking (0);
	}
    }
  else
    {
      int repushed_objects;
      int mark_work;
      GC_STAT_RESUME_GC;
      repushed_objects = gc_resume_mark_phase ();
      mark_work = (gc_incremental_traversal_threshold > repushed_objects) ?
	gc_incremental_traversal_threshold : repushed_objects;
      kkcc_marking (mark_work);
      if (KKCC_GC_STACK_EMPTY)
	{
	  /* Mark root set again and finish up marking. */
	  gc_mark_root_set (REPUSH_ROOT_SET);
	  kkcc_marking (0);
	}
      else
	{
	  gc_suspend_mark_phase ();
	  return 0;
	}
    }
  return 1;
}


void
gc_1 (int incremental)
{
  switch (GC_PHASE)
    {
    case NONE:
      gc_prepare ();
      kkcc_gc_stack_init();
#ifdef DEBUG_XEMACS
      kkcc_bt_init ();
#endif
    case INIT_GC:
      gc_mark_root_set (PUSH_ROOT_SET);
    case PUSH_ROOT_SET:
      if (!gc_mark (incremental))
	return; /* suspend gc */
    case MARK:
      if (!KKCC_GC_STACK_EMPTY)
	if (!gc_resume_mark (incremental))
	  return; /* suspend gc */
      gc_finish_mark ();
    case FINISH_MARK:
      gc_finalize ();
      kkcc_gc_stack_free ();
#ifdef DEBUG_XEMACS
      kkcc_bt_free ();
#endif
    case FINALIZE:
      gc_sweep ();
    case SWEEP:
      gc_finish ();
    case FINISH_GC:
      break;
    }
}

void gc (int incremental)
{
  if (gc_currently_forbidden
      || in_display
      || preparing_for_armageddon)
    return;

  /* Very important to prevent GC during any of the following
     stuff that might run Lisp code; otherwise, we'll likely
     have infinite GC recursion. */
  speccount = begin_gc_forbidden ();

  show_gc_cursor_and_message ();

  gc_1 (incremental);

  remove_gc_cursor_and_message ();

  /* now stop inhibiting GC */
  unbind_to (speccount);
}

void 
gc_full (void)
{
  gc (0);
}

DEFUN ("gc-full", Fgc_full, 0, 0, "", /*
This function performs a full garbage collection. If an incremental
garbage collection is already running, it completes without any
further interruption.  This function guarantees that unused objects
are freed when it returns. Garbage collection happens automatically if
the client allocates more than `gc-cons-threshold' bytes of Lisp data
since the previous garbage collection.
*/
       ())
{
  gc_full ();
  return Qt;
}

void 
gc_incremental (void)
{
  gc (allow_incremental_gc);
}

DEFUN ("gc-incremental", Fgc_incremental, 0, 0, "", /*
This function starts an incremental garbage collection. If an
incremental garbage collection is already running, the next cycle
starts. Note that this function has not necessarily freed any memory
when it returns. This function only guarantees, that the traversal of
the heap makes progress.  The next cycle of incremental garbage
collection happens automatically if the client allocates more than
`gc-incremental-cons-threshold' bytes of Lisp data since previous
garbage collection.
*/
       ())
{
  gc_incremental ();
  return Qt;
}
#else /* not NEW_GC */
void garbage_collect_1 (void)
{
  if (gc_in_progress
      || gc_currently_forbidden
      || in_display
      || preparing_for_armageddon)
    return;

  /* Very important to prevent GC during any of the following
     stuff that might run Lisp code; otherwise, we'll likely
     have infinite GC recursion. */
  speccount = begin_gc_forbidden ();

  show_gc_cursor_and_message ();

  gc_prepare ();
#ifdef USE_KKCC
  kkcc_gc_stack_init();
#ifdef DEBUG_XEMACS
  kkcc_bt_init ();
#endif
#endif /* USE_KKCC */
  gc_mark_root_set ();
#ifdef USE_KKCC
  kkcc_marking (0);
#endif /* USE_KKCC */
  gc_finish_mark ();
#ifdef USE_KKCC
  kkcc_gc_stack_free ();
#ifdef DEBUG_XEMACS
  kkcc_bt_free ();
#endif
#endif /* USE_KKCC */
  gc_sweep_1 ();
  gc_finish ();

  remove_gc_cursor_and_message ();

  /* now stop inhibiting GC */
  unbind_to (speccount);
}
#endif /* not NEW_GC */


/************************************************************************/
/*			     Initializations				*/
/************************************************************************/

/* Initialization */
static void
common_init_gc_early (void)
{
  Vgc_message = Qzero;

  gc_currently_forbidden = 0;
  gc_hooks_inhibited = 0;

  need_to_garbage_collect = always_gc;

  gc_cons_threshold = GC_CONS_THRESHOLD;
  gc_cons_percentage = 40; /* #### what is optimal? */
  total_gc_usage_set = 0;
#ifdef NEW_GC
  gc_cons_incremental_threshold = GC_CONS_INCREMENTAL_THRESHOLD;
  gc_incremental_traversal_threshold = GC_INCREMENTAL_TRAVERSAL_THRESHOLD;
#endif /* NEW_GC */
}

void
init_gc_early (void)
{
#ifdef NEW_GC
  /* Reset the finalizers_to_run list after pdump_load. */
  Vfinalizers_to_run = Qnil;
#endif /* NEW_GC */
}

void
reinit_gc_early (void)
{
  common_init_gc_early ();
}

void
init_gc_once_early (void)
{
  common_init_gc_early ();
}

void
syms_of_gc (void)
{
  DEFSYMBOL (Qpre_gc_hook);
  DEFSYMBOL (Qpost_gc_hook);
#ifdef NEW_GC
  DEFSUBR (Fgc_full);
  DEFSUBR (Fgc_incremental);
#ifdef ERROR_CHECK_GC
  DEFSUBR (Fgc_stats);
#endif /* not ERROR_CHECK_GC */
#endif /* NEW_GC */
}

void
vars_of_gc (void)
{
  staticpro_nodump (&pre_gc_cursor);

  QSin_garbage_collection = build_msg_string ("(in garbage collection)");
  staticpro (&QSin_garbage_collection);

  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold /*
*Number of bytes of consing between full garbage collections.
\"Consing\" is a misnomer in that this actually counts allocation
of all different kinds of objects, not just conses.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically when `eval' or `funcall' are
called.  (Note that `funcall' is called implicitly as part of evaluation.)
By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.

Normally, you cannot set this value less than 10,000 (if you do, it is
automatically reset during the next garbage collection).  However, if
XEmacs was compiled with DEBUG_XEMACS, this does not happen, allowing
you to set this value very low to track down problems with insufficient
GCPRO'ing.  If you set this to a negative number, garbage collection will
happen at *EVERY* call to `eval' or `funcall'.  This is an extremely
effective way to check GCPRO problems, but be warned that your XEmacs
will be unusable!  You almost certainly won't have the patience to wait
long enough to be able to set it back.
 
See also `consing-since-gc' and `gc-cons-percentage'.
*/ );

  DEFVAR_INT ("gc-cons-percentage", &gc_cons_percentage /*
*Percentage of memory allocated between garbage collections.

Garbage collection will happen if this percentage of the total amount of
memory used for data (see `lisp-object-memory-usage') has been allocated
since the last garbage collection.  However, it will not happen if less
than `gc-cons-threshold' bytes have been allocated -- this sets an absolute
minimum in case very little data has been allocated or the percentage is
set very low.  Set this to 0 to have garbage collection always happen after
`gc-cons-threshold' bytes have been allocated, regardless of current memory
usage.

See also `consing-since-gc' and `gc-cons-threshold'.
*/ );

#ifdef NEW_GC
  DEFVAR_INT ("gc-cons-incremental-threshold", 
	      &gc_cons_incremental_threshold /*
*Number of bytes of consing between cycles of incremental garbage
collections.  \"Consing\" is a misnomer in that this actually counts
allocation of all different kinds of objects, not just conses.  The
next garbage collection cycle can happen automatically once this many
bytes have been allocated since the last garbage collection cycle.
All data types count.

See also `gc-cons-threshold'.
*/ );

  DEFVAR_INT ("gc-incremental-traversal-threshold", 
	      &gc_incremental_traversal_threshold /*
*Number of elements processed in one cycle of incremental travesal.
*/ );
#endif /* NEW_GC */

  DEFVAR_BOOL ("purify-flag", &purify_flag /*
Non-nil means loading Lisp code in order to dump an executable.
This means that certain objects should be allocated in readonly space.
*/ );

  DEFVAR_BOOL ("garbage-collection-messages", &garbage_collection_messages /*
 Non-nil means display messages at start and end of garbage collection.
*/ );
  garbage_collection_messages = 0;

  DEFVAR_LISP ("pre-gc-hook", &Vpre_gc_hook /*
Function or functions to be run just before each garbage collection.
Interrupts, garbage collection, and errors are inhibited while this hook
runs, so be extremely careful in what you add here.  In particular, avoid
consing, and do not interact with the user.
*/ );
  Vpre_gc_hook = Qnil;

  DEFVAR_LISP ("post-gc-hook", &Vpost_gc_hook /*
Function or functions to be run just after each garbage collection.
Interrupts, garbage collection, and errors are inhibited while this hook
runs.  Each hook is called with one argument which is an alist with
finalization data.
*/ );
  Vpost_gc_hook = Qnil;

  DEFVAR_LISP ("gc-message", &Vgc_message /*
String to print to indicate that a garbage collection is in progress.
This is printed in the echo area.  If the selected frame is on a
window system and `gc-pointer-glyph' specifies a value (i.e. a pointer
image instance) in the domain of the selected frame, the mouse pointer
will change instead of this message being printed.
*/ );
  Vgc_message = build_string (gc_default_message);

  DEFVAR_LISP ("gc-pointer-glyph", &Vgc_pointer_glyph /*
Pointer glyph used to indicate that a garbage collection is in progress.
If the selected window is on a window system and this glyph specifies a
value (i.e. a pointer image instance) in the domain of the selected
window, the pointer will be changed as specified during garbage collection.
Otherwise, a message will be printed in the echo area, as controlled
by `gc-message'.
*/ );

#ifdef NEW_GC
  DEFVAR_BOOL ("allow-incremental-gc", &allow_incremental_gc /*
*Non-nil means to allow incremental garbage collection. Nil prevents
*incremental garbage collection, the garbage collector then only does
*full collects (even if (gc-incremental) is called).
*/ );

  Vfinalizers_to_run = Qnil;
  staticpro_nodump (&Vfinalizers_to_run);
#endif /* NEW_GC */
}

void
complex_vars_of_gc (void)
{
  Vgc_pointer_glyph = Fmake_glyph_internal (Qpointer);
}
