/* Storage allocation and gc for XEmacs Lisp interpreter.
   Copyright (C) 1985-1998 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2001, 2002, 2003 Ben Wing.

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

/* Synched up with: FSF 19.28, Mule 2.0.  Substantially different from
   FSF. */

/* Authorship:

   FSF: Original version; a long time ago.
   Mly: Significantly rewritten to use new 3-bit tags and
        nicely abstracted object definitions, for 19.8.
   JWZ: Improved code to keep track of purespace usage and
        issue nice purespace and GC stats.
   Ben Wing: Cleaned up frob-block lrecord code, added error-checking
        and various changes for Mule, for 19.12.
        Added bit vectors for 19.13.
	Added lcrecord lists for 19.14.
   slb: Lots of work on the purification and dump time code.
        Synched Doug Lea malloc support from Emacs 20.2.
   og:  Killed the purespace.  Portable dumper (moved to dumper.c)
*/

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "chartab.h"
#include "device.h"
#include "elhash.h"
#include "events.h"
#include "extents-impl.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "glyphs.h"
#include "opaque.h"
#include "lstream.h"
#include "process.h"
#include "profile.h"
#include "redisplay.h"
#include "specifier.h"
#include "sysfile.h"
#include "sysdep.h"
#include "window.h"
#include "console-stream.h"

#ifdef DOUG_LEA_MALLOC
#include <malloc.h>
#endif

EXFUN (Fgarbage_collect, 0);

static void recompute_need_to_garbage_collect (void);

#if 0 /* this is _way_ too slow to be part of the standard debug options */
#if defined(DEBUG_XEMACS) && defined(MULE)
#define VERIFY_STRING_CHARS_INTEGRITY
#endif
#endif

/* Define this to use malloc/free with no freelist for all datatypes,
   the hope being that some debugging tools may help detect
   freed memory references */
#ifdef USE_DEBUG_MALLOC	/* Taking the above comment at face value -slb */
#include <dmalloc.h>
#define ALLOC_NO_POOLS
#endif

#ifdef DEBUG_XEMACS
static Fixnum debug_allocation;
static Fixnum debug_allocation_backtrace_length;
#endif

/* Number of bytes of consing done since the last gc */
static EMACS_INT consing_since_gc;
EMACS_UINT total_consing;

int need_to_garbage_collect;
int need_to_check_c_alloca;
int need_to_signal_post_gc;
int funcall_allocation_flag;
Bytecount __temp_alloca_size__;
Bytecount funcall_alloca_count;

/* Determine now whether we need to garbage collect or not, to make
   Ffuncall() faster */
#define INCREMENT_CONS_COUNTER_1(size)		\
do						\
{						\
  consing_since_gc += (size);			\
  total_consing += (size);			\
  if (profiling_active)				\
    profile_record_consing (size);		\
  recompute_need_to_garbage_collect ();		\
} while (0)

#define debug_allocation_backtrace()				\
do {								\
  if (debug_allocation_backtrace_length > 0)			\
    debug_short_backtrace (debug_allocation_backtrace_length);	\
} while (0)

#ifdef DEBUG_XEMACS
#define INCREMENT_CONS_COUNTER(foosize, type)		\
  do {							\
    if (debug_allocation)				\
      {							\
	stderr_out ("allocating %s (size %ld)\n", type,	\
		    (long) foosize);			\
	debug_allocation_backtrace ();			\
      }							\
    INCREMENT_CONS_COUNTER_1 (foosize);			\
  } while (0)
#define NOSEEUM_INCREMENT_CONS_COUNTER(foosize, type)		\
  do {								\
    if (debug_allocation > 1)					\
      {								\
	stderr_out ("allocating noseeum %s (size %ld)\n", type,	\
		    (long) foosize);				\
	debug_allocation_backtrace ();				\
      }								\
    INCREMENT_CONS_COUNTER_1 (foosize);				\
  } while (0)
#else
#define INCREMENT_CONS_COUNTER(size, type) INCREMENT_CONS_COUNTER_1 (size)
#define NOSEEUM_INCREMENT_CONS_COUNTER(size, type) \
  INCREMENT_CONS_COUNTER_1 (size)
#endif

#define DECREMENT_CONS_COUNTER(size) do {	\
  consing_since_gc -= (size);			\
  total_consing -= (size);			\
  if (profiling_active)				\
    profile_record_unconsing (size);		\
  if (consing_since_gc < 0)			\
    consing_since_gc = 0;			\
  recompute_need_to_garbage_collect ();		\
} while (0)

/* Number of bytes of consing since gc before another gc should be done. */
static EMACS_INT gc_cons_threshold;

/* Percentage of consing of total data size before another GC. */
static EMACS_INT gc_cons_percentage;

#ifdef ERROR_CHECK_GC
int always_gc;			/* Debugging hack; equivalent to
				   (setq gc-cons-thresold -1) */
#else
#define always_gc 0
#endif

/* Nonzero during gc */
int gc_in_progress;

/* Nonzero means display messages at beginning and end of GC.  */

int garbage_collection_messages;

/* Number of times GC has happened at this level or below.
 * Level 0 is most volatile, contrary to usual convention.
 *  (Of course, there's only one level at present) */
EMACS_INT gc_generation_number[1];

/* This is just for use by the printer, to allow things to print uniquely */
static int lrecord_uid_counter;

/* Nonzero when calling certain hooks or doing other things where
   a GC would be bad */
static int gc_currently_forbidden;

/* Hooks. */
Lisp_Object Vpre_gc_hook, Qpre_gc_hook;
Lisp_Object Vpost_gc_hook, Qpost_gc_hook;

/* "Garbage collecting" */
Lisp_Object Vgc_message;
Lisp_Object Vgc_pointer_glyph;
static const Char_ASCII gc_default_message[] = "Garbage collecting";
Lisp_Object Qgarbage_collecting;

static Lisp_Object QSin_garbage_collection;

/* Non-zero means we're in the process of doing the dump */
int purify_flag;

/* Non-zero means we're pdumping out or in */
#ifdef PDUMP
int in_pdump;
#endif

#ifdef ERROR_CHECK_TYPES

Error_Behavior ERROR_ME, ERROR_ME_NOT, ERROR_ME_WARN, ERROR_ME_DEBUG_WARN;

#endif

/* Very cheesy ways of figuring out how much memory is being used for
   data. #### Need better (system-dependent) ways. */
void *minimum_address_seen;
void *maximum_address_seen;

int
c_readonly (Lisp_Object obj)
{
  return POINTER_TYPE_P (XTYPE (obj)) && C_READONLY (obj);
}

int
lisp_readonly (Lisp_Object obj)
{
  return POINTER_TYPE_P (XTYPE (obj)) && LISP_READONLY (obj);
}


/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 0 /* 16000 */
#endif

/* Non-zero means ignore malloc warnings.  Set during initialization.  */
int ignore_malloc_warnings;


static void *breathing_space;

void
release_breathing_space (void)
{
  if (breathing_space)
    {
      void *tmp = breathing_space;
      breathing_space = 0;
      xfree (tmp);
    }
}

/* malloc calls this if it finds we are near exhausting storage */
void
malloc_warning (const char *str)
{
  if (ignore_malloc_warnings)
    return;

  warn_when_safe
    (Qmemory, Qemergency,
     "%s\n"
     "Killing some buffers may delay running out of memory.\n"
     "However, certainly by the time you receive the 95%% warning,\n"
     "you should clean up, kill this Emacs, and start a new one.",
     str);
}

/* Called if malloc returns zero */
DOESNT_RETURN
memory_full (void)
{
  /* Force a GC next time eval is called.
     It's better to loop garbage-collecting (we might reclaim enough
     to win) than to loop beeping and barfing "Memory exhausted"
   */
  consing_since_gc = gc_cons_threshold + 1;
  recompute_need_to_garbage_collect ();
  release_breathing_space ();

  /* Flush some histories which might conceivably contain garbalogical
     inhibitors.  */
  if (!NILP (Fboundp (Qvalues)))
    Fset (Qvalues, Qnil);
  Vcommand_history = Qnil;

  out_of_memory ("Memory exhausted", Qunbound);
}

static void
set_alloc_mins_and_maxes (void *val, Bytecount size)
{
  if (!val)
    return;
  if ((char *) val + size > (char *) maximum_address_seen)
    maximum_address_seen = (char *) val + size;
  if (!minimum_address_seen)
    minimum_address_seen =
#if SIZEOF_VOID_P == 8
      (void *) 0xFFFFFFFFFFFFFFFF;
#else
      (void *) 0xFFFFFFFF;
#endif
  if ((char *) val < (char *) minimum_address_seen)
    minimum_address_seen = (char *) val;
}

/* like malloc and realloc but check for no memory left. */

#ifdef ERROR_CHECK_MALLOC
static int in_malloc;
extern int regex_malloc_disallowed;
#endif

#undef xmalloc
void *
xmalloc (Bytecount size)
{
  void *val;
#ifdef ERROR_CHECK_MALLOC
  assert (!in_malloc);
  assert (!regex_malloc_disallowed);
  in_malloc = 1;
#endif
  val = malloc (size);
#ifdef ERROR_CHECK_MALLOC
  in_malloc = 0;
#endif
  if (!val && (size != 0)) memory_full ();
  set_alloc_mins_and_maxes (val, size);
  return val;
}

#undef xcalloc
static void *
xcalloc (Elemcount nelem, Bytecount elsize)
{
  void *val;
#ifdef ERROR_CHECK_MALLOC
  assert (!in_malloc);
  assert (!regex_malloc_disallowed);
  in_malloc = 1;
#endif
  val= calloc (nelem, elsize);
#ifdef ERROR_CHECK_MALLOC
  in_malloc = 0;
#endif

  if (!val && (nelem != 0)) memory_full ();
  set_alloc_mins_and_maxes (val, nelem * elsize);
  return val;
}

void *
xmalloc_and_zero (Bytecount size)
{
  return xcalloc (size, sizeof (char));
}

#undef xrealloc
void *
xrealloc (void *block, Bytecount size)
{
#ifdef ERROR_CHECK_MALLOC
  assert (!in_malloc);
  assert (!regex_malloc_disallowed);
  in_malloc = 1;
#endif
  block = realloc (block, size);
#ifdef ERROR_CHECK_MALLOC
  in_malloc = 0;
#endif

  if (!block && (size != 0)) memory_full ();
  set_alloc_mins_and_maxes (block, size);
  return block;
}

void
#ifdef ERROR_CHECK_MALLOC
xfree_1 (void *block)
#else
xfree (void *block)
#endif
{
#ifdef ERROR_CHECK_MALLOC
  /* Unbelievably, calling free() on 0xDEADBEEF doesn't cause an
     error until much later on for many system mallocs, such as
     the one that comes with Solaris 2.3.  FMH!! */
  assert (block != (void *) 0xDEADBEEF);
  assert (block);
  assert (!in_malloc);
  assert (!regex_malloc_disallowed);
  in_malloc = 1;
#endif /* ERROR_CHECK_MALLOC */
  free (block);
#ifdef ERROR_CHECK_MALLOC
  in_malloc = 0;
#endif
}

#ifdef ERROR_CHECK_GC

static void
deadbeef_memory (void *ptr, Bytecount size)
{
  UINT_32_BIT *ptr4 = (UINT_32_BIT *) ptr;
  Bytecount beefs = size >> 2;

  /* In practice, size will always be a multiple of four.  */
  while (beefs--)
    (*ptr4++) = 0xDEADBEEF; /* -559038737 base 10 */
}

#else /* !ERROR_CHECK_GC */


#define deadbeef_memory(ptr, size)

#endif /* !ERROR_CHECK_GC */

#undef xstrdup
char *
xstrdup (const char *str)
{
  int len = strlen (str) + 1;   /* for stupid terminating 0 */
  void *val = xmalloc (len);

  if (val == 0) return 0;
  return (char *) memcpy (val, str, len);
}

#ifdef NEED_STRDUP
char *
strdup (const char *s)
{
  return xstrdup (s);
}
#endif /* NEED_STRDUP */


static void *
allocate_lisp_storage (Bytecount size)
{
  void *val = xmalloc (size);
  /* We don't increment the cons counter anymore.  Calling functions do
     that now because we have two different kinds of cons counters -- one
     for normal objects, and one for no-see-um conses (and possibly others
     similar) where the conses are used totally internally, never escape,
     and are created and then freed and shouldn't logically increment the
     cons counting. #### (Or perhaps, we should decrement it when an object
     get freed?)  */

  /* But we do now (as of 3-27-02) go and zero out the memory.  This is a
     good thing, as it will guarantee we won't get any intermittent bugs
     coming from an uninitiated field.  The speed loss is unnoticeable,
     esp. as the objects are not large -- large stuff like buffer text and
     redisplay structures are allocated separately. */
  memset (val, 0, size);

  if (need_to_check_c_alloca)
    xemacs_c_alloca (0);

  return val;
}


/* lcrecords are chained together through their "next" field.
   After doing the mark phase, GC will walk this linked list
   and free any lcrecord which hasn't been marked. */
static struct lcrecord_header *all_lcrecords;

/* The most basic of the lcrecord allocation functions.  Not usually called
   directly.  Allocates an lrecord not managed by any lcrecord-list, of a
   specified size.  See lrecord.h. */

void *
basic_alloc_lcrecord (Bytecount size,
		      const struct lrecord_implementation *implementation)
{
  struct lcrecord_header *lcheader;

  type_checking_assert
    ((implementation->static_size == 0 ?
      implementation->size_in_bytes_method != NULL :
      implementation->static_size == size)
     &&
     (! implementation->basic_p)
     &&
     (! (implementation->hash == NULL && implementation->equal != NULL)));

  lcheader = (struct lcrecord_header *) allocate_lisp_storage (size);
  set_lheader_implementation (&lcheader->lheader, implementation);
  lcheader->next = all_lcrecords;
#if 1                           /* mly prefers to see small ID numbers */
  lcheader->uid = lrecord_uid_counter++;
#else				/* jwz prefers to see real addrs */
  lcheader->uid = (int) &lcheader;
#endif
  lcheader->free = 0;
  all_lcrecords = lcheader;
  INCREMENT_CONS_COUNTER (size, implementation->name);
  return lcheader;
}

#if 0 /* Presently unused */
/* Very, very poor man's EGC?
 * This may be slow and thrash pages all over the place.
 *  Only call it if you really feel you must (and if the
 *  lrecord was fairly recently allocated).
 * Otherwise, just let the GC do its job -- that's what it's there for
 */
void
very_old_free_lcrecord (struct lcrecord_header *lcrecord)
{
  if (all_lcrecords == lcrecord)
    {
      all_lcrecords = lcrecord->next;
    }
  else
    {
      struct lrecord_header *header = all_lcrecords;
      for (;;)
	{
	  struct lrecord_header *next = header->next;
	  if (next == lcrecord)
	    {
	      header->next = lrecord->next;
	      break;
	    }
	  else if (next == 0)
	    abort ();
	  else
	    header = next;
	}
    }
  if (lrecord->implementation->finalizer)
    lrecord->implementation->finalizer (lrecord, 0);
  xfree (lrecord);
  return;
}
#endif /* Unused */


static void
disksave_object_finalization_1 (void)
{
  struct lcrecord_header *header;

  for (header = all_lcrecords; header; header = header->next)
    {
      if (LHEADER_IMPLEMENTATION (&header->lheader)->finalizer &&
	  !header->free)
	LHEADER_IMPLEMENTATION (&header->lheader)->finalizer (header, 1);
    }
}

/* Bitwise copy all parts of a Lisp object other than the header */

void
copy_lisp_object (Lisp_Object dst, Lisp_Object src)
{
  const struct lrecord_implementation *imp =
    XRECORD_LHEADER_IMPLEMENTATION (src);
  Bytecount size = lisp_object_size (src);

  assert (imp == XRECORD_LHEADER_IMPLEMENTATION (dst));
  assert (size == lisp_object_size (dst));

  if (imp->basic_p)
    memcpy ((char *) XRECORD_LHEADER (dst) + sizeof (struct lrecord_header),
	    (char *) XRECORD_LHEADER (src) + sizeof (struct lrecord_header),
	    size - sizeof (struct lrecord_header));
  else
    memcpy ((char *) XRECORD_LHEADER (dst) + sizeof (struct lcrecord_header),
	    (char *) XRECORD_LHEADER (src) + sizeof (struct lcrecord_header),
	    size - sizeof (struct lcrecord_header));
}


/************************************************************************/
/*			  Debugger support				*/
/************************************************************************/
/* Give gdb/dbx enough information to decode Lisp Objects.  We make
   sure certain symbols are always defined, so gdb doesn't complain
   about expressions in src/.gdbinit.  See src/.gdbinit or src/.dbxrc
   to see how this is used.  */

EMACS_UINT dbg_valmask = ((1UL << VALBITS) - 1) << GCBITS;
EMACS_UINT dbg_typemask = (1UL << GCTYPEBITS) - 1;

#ifdef USE_UNION_TYPE
unsigned char dbg_USE_UNION_TYPE = 1;
#else
unsigned char dbg_USE_UNION_TYPE = 0;
#endif

unsigned char dbg_valbits = VALBITS;
unsigned char dbg_gctypebits = GCTYPEBITS;

/* On some systems, the above definitions will be optimized away by
   the compiler or linker unless they are referenced in some function. */
long dbg_inhibit_dbg_symbol_deletion (void);
long
dbg_inhibit_dbg_symbol_deletion (void)
{
  return
    (dbg_valmask +
     dbg_typemask +
     dbg_USE_UNION_TYPE +
     dbg_valbits +
     dbg_gctypebits);
}

/* Macros turned into functions for ease of debugging.
   Debuggers don't know about macros! */
int dbg_eq (Lisp_Object obj1, Lisp_Object obj2);
int
dbg_eq (Lisp_Object obj1, Lisp_Object obj2)
{
  return EQ (obj1, obj2);
}


/************************************************************************/
/*			  Fixed-size type macros			*/
/************************************************************************/

/* For fixed-size types that are commonly used, we malloc() large blocks
   of memory at a time and subdivide them into chunks of the correct
   size for an object of that type.  This is more efficient than
   malloc()ing each object separately because we save on malloc() time
   and overhead due to the fewer number of malloc()ed blocks, and
   also because we don't need any extra pointers within each object
   to keep them threaded together for GC purposes.  For less common
   (and frequently large-size) types, we use lcrecords, which are
   malloc()ed individually and chained together through a pointer
   in the lcrecord header.  lcrecords do not need to be fixed-size
   (i.e. two objects of the same type need not have the same size;
   however, the size of a particular object cannot vary dynamically).
   It is also much easier to create a new lcrecord type because no
   additional code needs to be added to alloc.c.  Finally, lcrecords
   may be more efficient when there are only a small number of them.

   The types that are stored in these large blocks (or "frob blocks")
   are cons, float, compiled-function, symbol, marker, extent, event,
   and string.

   Note that strings are special in that they are actually stored in
   two parts: a structure containing information about the string, and
   the actual data associated with the string.  The former structure
   (a struct Lisp_String) is a fixed-size structure and is managed the
   same way as all the other such types.  This structure contains a
   pointer to the actual string data, which is stored in structures of
   type struct string_chars_block.  Each string_chars_block consists
   of a pointer to a struct Lisp_String, followed by the data for that
   string, followed by another pointer to a Lisp_String, followed by
   the data for that string, etc.  At GC time, the data in these
   blocks is compacted by searching sequentially through all the
   blocks and compressing out any holes created by unmarked strings.
   Strings that are more than a certain size (bigger than the size of
   a string_chars_block, although something like half as big might
   make more sense) are malloc()ed separately and not stored in
   string_chars_blocks.  Furthermore, no one string stretches across
   two string_chars_blocks.

   Vectors are each malloc()ed separately as lcrecords.

   In the following discussion, we use conses, but it applies equally
   well to the other fixed-size types.

   We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc() whenever necessary.  Cons cells reclaimed
   by GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.  Each cons_block is
   just under 2^n - MALLOC_OVERHEAD bytes long, since malloc (at least
   the versions in malloc.c and gmalloc.c) really allocates in units
   of powers of two and uses 4 bytes for its own overhead.

   What GC actually does is to search through all the cons_blocks,
   from the most recently allocated to the oldest, and put all
   cons cells that are not marked (whether or not they're already
   free) on a cons_free_list.  The cons_free_list is a stack, and
   so the cons cells in the oldest-allocated cons_block end up
   at the head of the stack and are the first to be reallocated.
   If any cons_block is entirely free, it is freed with free()
   and its cons cells removed from the cons_free_list.  Because
   the cons_free_list ends up basically in memory order, we have
   a high locality of reference (assuming a reasonable turnover
   of allocating and freeing) and have a reasonable probability
   of entirely freeing up cons_blocks that have been more recently
   allocated.  This stage is called the "sweep stage" of GC, and
   is executed after the "mark stage", which involves starting
   from all places that are known to point to in-use Lisp objects
   (e.g. the obarray, where are all symbols are stored; the
   current catches and condition-cases; the backtrace list of
   currently executing functions; the gcpro list; etc.) and
   recursively marking all objects that are accessible.

   At the beginning of the sweep stage, the conses in the cons blocks
   are in one of three states: in use and marked, in use but not
   marked, and not in use (already freed).  Any conses that are marked
   have been marked in the mark stage just executed, because as part
   of the sweep stage we unmark any marked objects.  The way we tell
   whether or not a cons cell is in use is through the LRECORD_FREE_P
   macro.  This uses a special lrecord type `lrecord_type_free',
   which is never associated with any valid object.

   Conses on the free_cons_list are threaded through a pointer stored
   in the conses themselves.  Because the cons is still in a
   cons_block and needs to remain marked as not in use for the next
   time that GC happens, we need room to store both the "free"
   indicator and the chaining pointer.  So this pointer is stored
   after the lrecord header (actually where C places a pointer after
   the lrecord header; they are not necessarily contiguous).  This
   implies that all fixed-size types must be big enough to contain at
   least one pointer.  This is true for all current fixed-size types,
   with the possible exception of Lisp_Floats, for which we define the
   meat of the struct using a union of a pointer and a double to
   ensure adequate space for the free list chain pointer.

   Some types of objects need additional "finalization" done
   when an object is converted from in use to not in use;
   this is the purpose of the ADDITIONAL_FREE_type macro.
   For example, markers need to be removed from the chain
   of markers that is kept in each buffer.  This is because
   markers in a buffer automatically disappear if the marker
   is no longer referenced anywhere (the same does not
   apply to extents, however).

   WARNING: Things are in an extremely bizarre state when
   the ADDITIONAL_FREE_type macros are called, so beware!

   When ERROR_CHECK_GC is defined, we do things differently so as to
   maximize our chances of catching places where there is insufficient
   GCPROing.  The thing we want to avoid is having an object that
   we're using but didn't GCPRO get freed by GC and then reallocated
   while we're in the process of using it -- this will result in
   something seemingly unrelated getting trashed, and is extremely
   difficult to track down.  If the object gets freed but not
   reallocated, we can usually catch this because we set most of the
   bytes of a freed object to 0xDEADBEEF. (The lisp object type is set
   to the invalid type `lrecord_type_free', however, and a pointer
   used to chain freed objects together is stored after the lrecord
   header; we play some tricks with this pointer to make it more
   bogus, so crashes are more likely to occur right away.)

   We want freed objects to stay free as long as possible,
   so instead of doing what we do above, we maintain the
   free objects in a first-in first-out queue.  We also
   don't recompute the free list each GC, unlike above;
   this ensures that the queue ordering is preserved.
   [This means that we are likely to have worse locality
   of reference, and that we can never free a frob block
   once it's allocated. (Even if we know that all cells
   in it are free, there's no easy way to remove all those
   cells from the free list because the objects on the
   free list are unlikely to be in memory order.)]
   Furthermore, we never take objects off the free list
   unless there's a large number (usually 1000, but
   varies depending on type) of them already on the list.
   This way, we ensure that an object that gets freed will
   remain free for the next 1000 (or whatever) times that
   an object of that type is allocated.  */

#ifndef MALLOC_OVERHEAD
#ifdef GNU_MALLOC
#define MALLOC_OVERHEAD 0
#elif defined (rcheck)
#define MALLOC_OVERHEAD 20
#else
#define MALLOC_OVERHEAD 8
#endif
#endif /* MALLOC_OVERHEAD */

#if !defined(HAVE_MMAP) || defined(DOUG_LEA_MALLOC)
/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c.  */
void refill_memory_reserve (void);
void
refill_memory_reserve (void)
{
  if (breathing_space == 0)
    breathing_space = (char *) malloc (4096 - MALLOC_OVERHEAD);
}
#endif

#ifdef ALLOC_NO_POOLS
# define TYPE_ALLOC_SIZE(type, structtype) 1
#else
# define TYPE_ALLOC_SIZE(type, structtype)			\
    ((2048 - MALLOC_OVERHEAD - sizeof (struct type##_block *))	\
     / sizeof (structtype))
#endif /* ALLOC_NO_POOLS */

#define DECLARE_FIXED_TYPE_ALLOC(type, structtype)	\
							\
struct type##_block					\
{							\
  struct type##_block *prev;				\
  structtype block[TYPE_ALLOC_SIZE (type, structtype)];	\
};							\
							\
static struct type##_block *current_##type##_block;	\
static int current_##type##_block_index;		\
							\
static Lisp_Free *type##_free_list;			\
static Lisp_Free *type##_free_list_tail;		\
							\
static void						\
init_##type##_alloc (void)				\
{							\
  current_##type##_block = 0;				\
  current_##type##_block_index =			\
    countof (current_##type##_block->block);		\
  type##_free_list = 0;					\
  type##_free_list_tail = 0;				\
}							\
							\
static int gc_count_num_##type##_in_use;		\
static int gc_count_num_##type##_freelist

#define ALLOCATE_FIXED_TYPE_FROM_BLOCK(type, result) do {		\
  if (current_##type##_block_index					\
      == countof (current_##type##_block->block))			\
    {									\
      struct type##_block *AFTFB_new = (struct type##_block *)		\
	allocate_lisp_storage (sizeof (struct type##_block));		\
      AFTFB_new->prev = current_##type##_block;				\
      current_##type##_block = AFTFB_new;				\
      current_##type##_block_index = 0;					\
    }									\
  (result) =								\
    &(current_##type##_block->block[current_##type##_block_index++]);	\
} while (0)

/* Allocate an instance of a type that is stored in blocks.
   TYPE is the "name" of the type, STRUCTTYPE is the corresponding
   structure type. */

#ifdef ERROR_CHECK_GC

/* Note: if you get crashes in this function, suspect incorrect calls
   to free_cons() and friends.  This happened once because the cons
   cell was not GC-protected and was getting collected before
   free_cons() was called. */

#define ALLOCATE_FIXED_TYPE_1(type, structtype, result) do {	\
  if (gc_count_num_##type##_freelist >				\
      MINIMUM_ALLOWED_FIXED_TYPE_CELLS_##type)			\
    {								\
      result = (structtype *) type##_free_list;			\
      assert (LRECORD_FREE_P (result));				\
      /* Before actually using the chain pointer, we complement	\
	 all its bits; see PUT_FIXED_TYPE_ON_FREE_LIST(). */	\
      type##_free_list = (Lisp_Free *)				\
	(~ (EMACS_UINT) (type##_free_list->chain));		\
      gc_count_num_##type##_freelist--;				\
    }								\
  else								\
    ALLOCATE_FIXED_TYPE_FROM_BLOCK (type, result);		\
  MARK_LRECORD_AS_NOT_FREE (result);				\
} while (0)

#else /* !ERROR_CHECK_GC */

#define ALLOCATE_FIXED_TYPE_1(type, structtype, result) do {	\
  if (type##_free_list)						\
    {								\
      result = (structtype *) type##_free_list;			\
      type##_free_list = type##_free_list->chain;		\
    }								\
  else								\
    ALLOCATE_FIXED_TYPE_FROM_BLOCK (type, result);		\
  MARK_LRECORD_AS_NOT_FREE (result);				\
} while (0)

#endif /* !ERROR_CHECK_GC */


#define ALLOCATE_FIXED_TYPE(type, structtype, result)	\
do							\
{							\
  ALLOCATE_FIXED_TYPE_1 (type, structtype, result);	\
  INCREMENT_CONS_COUNTER (sizeof (structtype), #type);	\
} while (0)

#define NOSEEUM_ALLOCATE_FIXED_TYPE(type, structtype, result)	\
do								\
{								\
  ALLOCATE_FIXED_TYPE_1 (type, structtype, result);		\
  NOSEEUM_INCREMENT_CONS_COUNTER (sizeof (structtype), #type);	\
} while (0)


/* Lisp_Free is the type to represent a free list member inside a frob
   block of any lisp object type.  */
typedef struct Lisp_Free
{
  struct lrecord_header lheader;
  struct Lisp_Free *chain;
} Lisp_Free;

#define LRECORD_FREE_P(ptr) \
(((struct lrecord_header *) ptr)->type == lrecord_type_free)

#define MARK_LRECORD_AS_FREE(ptr) \
((void) (((struct lrecord_header *) ptr)->type = lrecord_type_free))

#ifdef ERROR_CHECK_GC
#define MARK_LRECORD_AS_NOT_FREE(ptr) \
((void) (((struct lrecord_header *) ptr)->type = lrecord_type_undefined))
#else
#define MARK_LRECORD_AS_NOT_FREE(ptr) DO_NOTHING
#endif

#ifdef ERROR_CHECK_GC

#define PUT_FIXED_TYPE_ON_FREE_LIST(type, structtype, ptr) do {	\
  if (type##_free_list_tail)					\
    {								\
      /* When we store the chain pointer, we complement all	\
	 its bits; this should significantly increase its	\
	 bogosity in case someone tries to use the value, and	\
	 should make us crash faster if someone overwrites the	\
	 pointer because when it gets un-complemented in	\
	 ALLOCATED_FIXED_TYPE(), the resulting pointer will be	\
	 extremely bogus. */					\
      type##_free_list_tail->chain =				\
	(Lisp_Free *) ~ (EMACS_UINT) (ptr);			\
    }								\
  else								\
    type##_free_list = (Lisp_Free *) (ptr);			\
  type##_free_list_tail = (Lisp_Free *) (ptr);			\
} while (0)

#else /* !ERROR_CHECK_GC */

#define PUT_FIXED_TYPE_ON_FREE_LIST(type, structtype, ptr) do {	\
  ((Lisp_Free *) (ptr))->chain = type##_free_list;		\
  type##_free_list = (Lisp_Free *) (ptr);			\
} while (0)							\

#endif /* !ERROR_CHECK_GC */

/* TYPE and STRUCTTYPE are the same as in ALLOCATE_FIXED_TYPE(). */

#define FREE_FIXED_TYPE(type, structtype, ptr) do {		\
  structtype *FFT_ptr = (ptr);					\
  gc_checking_assert (!LRECORD_FREE_P (FFT_ptr));		\
  ADDITIONAL_FREE_##type (FFT_ptr);				\
  deadbeef_memory (FFT_ptr, sizeof (structtype));		\
  PUT_FIXED_TYPE_ON_FREE_LIST (type, structtype, FFT_ptr);	\
  MARK_LRECORD_AS_FREE (FFT_ptr);				\
} while (0)

/* Like FREE_FIXED_TYPE() but used when we are explicitly
   freeing a structure through free_cons(), free_marker(), etc.
   rather than through the normal process of sweeping.
   We attempt to undo the changes made to the allocation counters
   as a result of this structure being allocated.  This is not
   completely necessary but helps keep things saner: e.g. this way,
   repeatedly allocating and freeing a cons will not result in
   the consing-since-gc counter advancing, which would cause a GC
   and somewhat defeat the purpose of explicitly freeing.

   We also disable this mechanism entirely when ALLOC_NO_POOLS is
   set, which is used for Purify and the like. */

#ifndef ALLOC_NO_POOLS
#define FREE_FIXED_TYPE_WHEN_NOT_IN_GC(type, structtype, ptr)	\
do { FREE_FIXED_TYPE (type, structtype, ptr);			\
     DECREMENT_CONS_COUNTER (sizeof (structtype));		\
     gc_count_num_##type##_freelist++;				\
   } while (0)
#else
#define FREE_FIXED_TYPE_WHEN_NOT_IN_GC(type, structtype, ptr)
#endif



/************************************************************************/
/*			   Cons allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (cons, Lisp_Cons);
/* conses are used and freed so often that we set this really high */
/* #define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_cons 20000 */
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_cons 2000

static Lisp_Object
mark_cons (Lisp_Object obj)
{
  if (NILP (XCDR (obj)))
    return XCAR (obj);

  mark_object (XCAR (obj));
  return XCDR (obj);
}

static int
cons_equal (Lisp_Object ob1, Lisp_Object ob2, int depth)
{
  depth++;
  while (internal_equal (XCAR (ob1), XCAR (ob2), depth))
    {
      ob1 = XCDR (ob1);
      ob2 = XCDR (ob2);
      if (! CONSP (ob1) || ! CONSP (ob2))
	return internal_equal (ob1, ob2, depth);
    }
  return 0;
}

static const struct memory_description cons_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Cons, car_) },
  { XD_LISP_OBJECT, offsetof (Lisp_Cons, cdr_) },
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("cons", cons,
				     1, /*dumpable-flag*/
				     mark_cons, print_cons, 0,
				     cons_equal,
				     /*
				      * No `hash' method needed.
				      * internal_hash knows how to
				      * handle conses.
				      */
				     0,
				     cons_description,
				     Lisp_Cons);

DEFUN ("cons", Fcons, 2, 2, 0, /*
Create a new cons, give it CAR and CDR as components, and return it.
*/
       (car, cdr))
{
  /* This cannot GC. */
  Lisp_Object val;
  Lisp_Cons *c;

  ALLOCATE_FIXED_TYPE (cons, Lisp_Cons, c);
  set_lheader_implementation (&c->lheader, &lrecord_cons);
  val = wrap_cons (c);
  XSETCAR (val, car);
  XSETCDR (val, cdr);
  return val;
}

/* This is identical to Fcons() but it used for conses that we're
   going to free later, and is useful when trying to track down
   "real" consing. */
Lisp_Object
noseeum_cons (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object val;
  Lisp_Cons *c;

  NOSEEUM_ALLOCATE_FIXED_TYPE (cons, Lisp_Cons, c);
  set_lheader_implementation (&c->lheader, &lrecord_cons);
  val = wrap_cons (c);
  XCAR (val) = car;
  XCDR (val) = cdr;
  return val;
}

DEFUN ("list", Flist, 0, MANY, 0, /*
Return a newly created list with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object val = Qnil;
  Lisp_Object *argp = args + nargs;

  while (argp > args)
    val = Fcons (*--argp, val);
  return val;
}

Lisp_Object
list1 (Lisp_Object obj0)
{
  /* This cannot GC. */
  return Fcons (obj0, Qnil);
}

Lisp_Object
list2 (Lisp_Object obj0, Lisp_Object obj1)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Qnil));
}

Lisp_Object
list3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Qnil)));
}

Lisp_Object
cons3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, obj2));
}

Lisp_Object
acons (Lisp_Object key, Lisp_Object value, Lisp_Object alist)
{
  return Fcons (Fcons (key, value), alist);
}

Lisp_Object
list4 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Qnil))));
}

Lisp_Object
list5 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3,
       Lisp_Object obj4)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Fcons (obj4, Qnil)))));
}

Lisp_Object
list6 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3,
       Lisp_Object obj4, Lisp_Object obj5)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Fcons (obj4, Fcons (obj5, Qnil))))));
}

DEFUN ("make-list", Fmake_list, 2, 2, 0, /*
Return a new list of length LENGTH, with each element being OBJECT.
*/
       (length, object))
{
  CHECK_NATNUM (length);

  {
    Lisp_Object val = Qnil;
    EMACS_INT size = XINT (length);

    while (size--)
      val = Fcons (object, val);
    return val;
  }
}


/************************************************************************/
/*			  Float allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (float, Lisp_Float);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_float 1000

Lisp_Object
make_float (double float_value)
{
  Lisp_Float *f;

  ALLOCATE_FIXED_TYPE (float, Lisp_Float, f);

  /* Avoid dump-time `uninitialized memory read' purify warnings. */
  if (sizeof (struct lrecord_header) + sizeof (double) != sizeof (*f))
    xzero (*f);

  set_lheader_implementation (&f->lheader, &lrecord_float);
  float_data (f) = float_value;
  return wrap_float (f);
}


/************************************************************************/
/*			   Vector allocation				*/
/************************************************************************/

static Lisp_Object
mark_vector (Lisp_Object obj)
{
  Lisp_Vector *ptr = XVECTOR (obj);
  int len = vector_length (ptr);
  int i;

  for (i = 0; i < len - 1; i++)
    mark_object (ptr->contents[i]);
  return (len > 0) ? ptr->contents[len - 1] : Qnil;
}

static Bytecount
size_vector (const void *lheader)
{
  return FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Vector, Lisp_Object, contents,
				       ((Lisp_Vector *) lheader)->size);
}

static int
vector_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  int len = XVECTOR_LENGTH (obj1);
  if (len != XVECTOR_LENGTH (obj2))
    return 0;

  {
    Lisp_Object *ptr1 = XVECTOR_DATA (obj1);
    Lisp_Object *ptr2 = XVECTOR_DATA (obj2);
    while (len--)
      if (!internal_equal (*ptr1++, *ptr2++, depth + 1))
	return 0;
  }
  return 1;
}

static Hashcode
vector_hash (Lisp_Object obj, int depth)
{
  return HASH2 (XVECTOR_LENGTH (obj),
		internal_array_hash (XVECTOR_DATA (obj),
				     XVECTOR_LENGTH (obj),
				     depth + 1));
}

static const struct memory_description vector_description[] = {
  { XD_LONG,              offsetof (Lisp_Vector, size) },
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Vector, contents), XD_INDIRECT(0, 0) },
  { XD_END }
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("vector", vector,
					1, /*dumpable-flag*/
					mark_vector, print_vector, 0,
					vector_equal,
					vector_hash,
					vector_description,
					size_vector, Lisp_Vector);
/* #### should allocate `small' vectors from a frob-block */
static Lisp_Vector *
make_vector_internal (Elemcount sizei)
{
  /* no `next' field; we use lcrecords */
  Bytecount sizem = FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Vector, Lisp_Object,
						  contents, sizei);
  Lisp_Vector *p =
    (Lisp_Vector *) basic_alloc_lcrecord (sizem, &lrecord_vector);

  p->size = sizei;
  return p;
}

Lisp_Object
make_vector (Elemcount length, Lisp_Object object)
{
  Lisp_Vector *vecp = make_vector_internal (length);
  Lisp_Object *p = vector_data (vecp);

  while (length--)
    *p++ = object;

  return wrap_vector (vecp);
}

DEFUN ("make-vector", Fmake_vector, 2, 2, 0, /*
Return a new vector of length LENGTH, with each element being OBJECT.
See also the function `vector'.
*/
       (length, object))
{
  CONCHECK_NATNUM (length);
  return make_vector (XINT (length), object);
}

DEFUN ("vector", Fvector, 0, MANY, 0, /*
Return a newly created vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Vector *vecp = make_vector_internal (nargs);
  Lisp_Object *p = vector_data (vecp);

  while (nargs--)
    *p++ = *args++;

  return wrap_vector (vecp);
}

Lisp_Object
vector1 (Lisp_Object obj0)
{
  return Fvector (1, &obj0);
}

Lisp_Object
vector2 (Lisp_Object obj0, Lisp_Object obj1)
{
  Lisp_Object args[2];
  args[0] = obj0;
  args[1] = obj1;
  return Fvector (2, args);
}

Lisp_Object
vector3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  Lisp_Object args[3];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  return Fvector (3, args);
}

#if 0 /* currently unused */

Lisp_Object
vector4 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3)
{
  Lisp_Object args[4];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  return Fvector (4, args);
}

Lisp_Object
vector5 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4)
{
  Lisp_Object args[5];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  return Fvector (5, args);
}

Lisp_Object
vector6 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5)
{
  Lisp_Object args[6];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  return Fvector (6, args);
}

Lisp_Object
vector7 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5,
	 Lisp_Object obj6)
{
  Lisp_Object args[7];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  args[6] = obj6;
  return Fvector (7, args);
}

Lisp_Object
vector8 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5,
	 Lisp_Object obj6, Lisp_Object obj7)
{
  Lisp_Object args[8];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  args[6] = obj6;
  args[7] = obj7;
  return Fvector (8, args);
}
#endif /* unused */

/************************************************************************/
/*			 Bit Vector allocation				*/
/************************************************************************/

/* #### should allocate `small' bit vectors from a frob-block */
static Lisp_Bit_Vector *
make_bit_vector_internal (Elemcount sizei)
{
  /* no `next' field; we use lcrecords */
  Elemcount num_longs = BIT_VECTOR_LONG_STORAGE (sizei);
  Bytecount sizem = FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Bit_Vector,
						  unsigned long,
						  bits, num_longs);
  Lisp_Bit_Vector *p = (Lisp_Bit_Vector *)
    basic_alloc_lcrecord (sizem, &lrecord_bit_vector);

  bit_vector_length (p) = sizei;
  return p;
}

Lisp_Object
make_bit_vector (Elemcount length, Lisp_Object bit)
{
  Lisp_Bit_Vector *p = make_bit_vector_internal (length);
  Elemcount num_longs = BIT_VECTOR_LONG_STORAGE (length);

  CHECK_BIT (bit);

  if (ZEROP (bit))
    memset (p->bits, 0, num_longs * sizeof (long));
  else
    {
      Elemcount bits_in_last = length & (LONGBITS_POWER_OF_2 - 1);
      memset (p->bits, ~0, num_longs * sizeof (long));
      /* But we have to make sure that the unused bits in the
	 last long are 0, so that equal/hash is easy. */
      if (bits_in_last)
	p->bits[num_longs - 1] &= (1 << bits_in_last) - 1;
    }

  return wrap_bit_vector (p);
}

Lisp_Object
make_bit_vector_from_byte_vector (unsigned char *bytevec, Elemcount length)
{
  Elemcount i;
  Lisp_Bit_Vector *p = make_bit_vector_internal (length);

  for (i = 0; i < length; i++)
    set_bit_vector_bit (p, i, bytevec[i]);

  return wrap_bit_vector (p);
}

DEFUN ("make-bit-vector", Fmake_bit_vector, 2, 2, 0, /*
Return a new bit vector of length LENGTH. with each bit set to BIT.
BIT must be one of the integers 0 or 1.  See also the function `bit-vector'.
*/
       (length, bit))
{
  CONCHECK_NATNUM (length);

  return make_bit_vector (XINT (length), bit);
}

DEFUN ("bit-vector", Fbit_vector, 0, MANY, 0, /*
Return a newly created bit vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
Each argument must be one of the integers 0 or 1.
*/
       (int nargs, Lisp_Object *args))
{
  int i;
  Lisp_Bit_Vector *p = make_bit_vector_internal (nargs);

  for (i = 0; i < nargs; i++)
    {
      CHECK_BIT (args[i]);
      set_bit_vector_bit (p, i, !ZEROP (args[i]));
    }

  return wrap_bit_vector (p);
}


/************************************************************************/
/*		     Compiled-function allocation			*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (compiled_function, Lisp_Compiled_Function);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_compiled_function 1000

static Lisp_Object
make_compiled_function (void)
{
  Lisp_Compiled_Function *f;

  ALLOCATE_FIXED_TYPE (compiled_function, Lisp_Compiled_Function, f);
  set_lheader_implementation (&f->lheader, &lrecord_compiled_function);

  f->stack_depth = 0;
  f->specpdl_depth = 0;
  f->flags.documentationp = 0;
  f->flags.interactivep = 0;
  f->flags.domainp = 0; /* I18N3 */
  f->instructions = Qzero;
  f->constants = Qzero;
  f->arglist = Qnil;
  f->doc_and_interactive = Qnil;
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  f->annotated = Qnil;
#endif
  return wrap_compiled_function (f);
}

DEFUN ("make-byte-code", Fmake_byte_code, 4, MANY, 0, /*
Return a new compiled-function object.
Usage: (arglist instructions constants stack-depth
	&optional doc-string interactive)
Note that, unlike all other emacs-lisp functions, calling this with five
arguments is NOT the same as calling it with six arguments, the last of
which is nil.  If the INTERACTIVE arg is specified as nil, then that means
that this function was defined with `(interactive)'.  If the arg is not
specified, then that means the function is not interactive.
This is terrible behavior which is retained for compatibility with old
`.elc' files which expect these semantics.
*/
       (int nargs, Lisp_Object *args))
{
/* In a non-insane world this function would have this arglist...
   (arglist instructions constants stack_depth &optional doc_string interactive)
 */
  Lisp_Object fun = make_compiled_function ();
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);

  Lisp_Object arglist      = args[0];
  Lisp_Object instructions = args[1];
  Lisp_Object constants    = args[2];
  Lisp_Object stack_depth  = args[3];
  Lisp_Object doc_string   = (nargs > 4) ? args[4] : Qnil;
  Lisp_Object interactive  = (nargs > 5) ? args[5] : Qunbound;

  if (nargs < 4 || nargs > 6)
    return Fsignal (Qwrong_number_of_arguments,
		    list2 (intern ("make-byte-code"), make_int (nargs)));

  /* Check for valid formal parameter list now, to allow us to use
     SPECBIND_FAST_UNSAFE() later in funcall_compiled_function(). */
  {
    EXTERNAL_LIST_LOOP_2 (symbol, arglist)
      {
	CHECK_SYMBOL (symbol);
	if (EQ (symbol, Qt)   ||
	    EQ (symbol, Qnil) ||
	    SYMBOL_IS_KEYWORD (symbol))
	  invalid_constant_2
	    ("Invalid constant symbol in formal parameter list",
	     symbol, arglist);
      }
  }
  f->arglist = arglist;

  {
    int minargs = 0, maxargs = 0, totalargs = 0;
    int optional_p = 0, rest_p = 0, i = 0;
    {
      LIST_LOOP_2 (arg, arglist)
	{
	  if (EQ (arg, Qand_optional))
	    optional_p = 1;
	  else if (EQ (arg, Qand_rest))
	    rest_p = 1;
	  else
	    {
	      if (rest_p)
		{
		  maxargs = MANY;
		  totalargs++;
		  break;
		}
	      if (!optional_p)
		minargs++;
	      maxargs++;
	      totalargs++;
	    }
	}
    }
  
    if (totalargs)
      f->args = xnew_array (Lisp_Object, totalargs);

    {
      LIST_LOOP_2 (arg, arglist)
	{
	  if (!EQ (arg, Qand_optional) && !EQ (arg, Qand_rest))
	    f->args[i++] = arg;
	}
    }

    f->max_args = maxargs;
    f->min_args = minargs;
    f->args_in_array = totalargs;
  }
  
  /* `instructions' is a string or a cons (string . int) for a
     lazy-loaded function. */
  if (CONSP (instructions))
    {
      CHECK_STRING (XCAR (instructions));
      CHECK_INT (XCDR (instructions));
    }
  else
    {
      CHECK_STRING (instructions);
    }
  f->instructions = instructions;

  if (!NILP (constants))
    CHECK_VECTOR (constants);
  f->constants = constants;

  CHECK_NATNUM (stack_depth);
  f->stack_depth = (unsigned short) XINT (stack_depth);

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  if (!NILP (Vcurrent_compiled_function_annotation))
    f->annotated = Fcopy (Vcurrent_compiled_function_annotation);
  else if (!NILP (Vload_file_name_internal_the_purecopy))
    f->annotated = Vload_file_name_internal_the_purecopy;
  else if (!NILP (Vload_file_name_internal))
    {
      struct gcpro gcpro1;
      GCPRO1 (fun);		/* don't let fun get reaped */
      Vload_file_name_internal_the_purecopy =
	Ffile_name_nondirectory (Vload_file_name_internal);
      f->annotated = Vload_file_name_internal_the_purecopy;
      UNGCPRO;
    }
#endif /* COMPILED_FUNCTION_ANNOTATION_HACK */

  /* doc_string may be nil, string, int, or a cons (string . int).
     interactive may be list or string (or unbound). */
  f->doc_and_interactive = Qunbound;
#ifdef I18N3
  if ((f->flags.domainp = !NILP (Vfile_domain)) != 0)
    f->doc_and_interactive = Vfile_domain;
#endif
  if ((f->flags.interactivep = !UNBOUNDP (interactive)) != 0)
    {
      f->doc_and_interactive
	= (UNBOUNDP (f->doc_and_interactive) ? interactive :
	   Fcons (interactive, f->doc_and_interactive));
    }
  if ((f->flags.documentationp = !NILP (doc_string)) != 0)
    {
      f->doc_and_interactive
	= (UNBOUNDP (f->doc_and_interactive) ? doc_string :
	   Fcons (doc_string, f->doc_and_interactive));
    }
  if (UNBOUNDP (f->doc_and_interactive))
    f->doc_and_interactive = Qnil;

  return fun;
}


/************************************************************************/
/*			    Symbol allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (symbol, Lisp_Symbol);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_symbol 1000

DEFUN ("make-symbol", Fmake_symbol, 1, 1, 0, /*
Return a newly allocated uninterned symbol whose name is NAME.
Its value and function definition are void, and its property list is nil.
*/
       (name))
{
  Lisp_Symbol *p;

  CHECK_STRING (name);

  ALLOCATE_FIXED_TYPE (symbol, Lisp_Symbol, p);
  set_lheader_implementation (&p->lheader, &lrecord_symbol);
  p->name     = name;
  p->plist    = Qnil;
  p->value    = Qunbound;
  p->function = Qunbound;
  symbol_next (p) = 0;
  return wrap_symbol (p);
}


/************************************************************************/
/*			   Extent allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (extent, struct extent);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_extent 1000

struct extent *
allocate_extent (void)
{
  struct extent *e;

  ALLOCATE_FIXED_TYPE (extent, struct extent, e);
  set_lheader_implementation (&e->lheader, &lrecord_extent);
  extent_object (e) = Qnil;
  set_extent_start (e, -1);
  set_extent_end (e, -1);
  e->plist = Qnil;

  xzero (e->flags);

  extent_face (e) = Qnil;
  e->flags.end_open = 1;  /* default is for endpoints to behave like markers */
  e->flags.detachable = 1;

  return e;
}


/************************************************************************/
/*			   Event allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (event, Lisp_Event);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_event 1000

Lisp_Object
allocate_event (void)
{
  Lisp_Event *e;

  ALLOCATE_FIXED_TYPE (event, Lisp_Event, e);
  set_lheader_implementation (&e->lheader, &lrecord_event);

  return wrap_event (e);
}

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_FIXED_TYPE_ALLOC (key_data, Lisp_Key_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_key_data 1000

Lisp_Object
make_key_data (void)
{
  Lisp_Key_Data *d;

  ALLOCATE_FIXED_TYPE (key_data, Lisp_Key_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_key_data);
  d->keysym = Qnil;

  return wrap_key_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (button_data, Lisp_Button_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_button_data 1000

Lisp_Object
make_button_data (void)
{
  Lisp_Button_Data *d;

  ALLOCATE_FIXED_TYPE (button_data, Lisp_Button_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_button_data);

  return wrap_button_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (motion_data, Lisp_Motion_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_motion_data 1000

Lisp_Object
make_motion_data (void)
{
  Lisp_Motion_Data *d;

  ALLOCATE_FIXED_TYPE (motion_data, Lisp_Motion_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_motion_data);

  return wrap_motion_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (process_data, Lisp_Process_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_process_data 1000

Lisp_Object
make_process_data (void)
{
  Lisp_Process_Data *d;

  ALLOCATE_FIXED_TYPE (process_data, Lisp_Process_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_process_data);
  d->process = Qnil;

  return wrap_process_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (timeout_data, Lisp_Timeout_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_timeout_data 1000

Lisp_Object
make_timeout_data (void)
{
  Lisp_Timeout_Data *d;

  ALLOCATE_FIXED_TYPE (timeout_data, Lisp_Timeout_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_timeout_data);
  d->function = Qnil;
  d->object = Qnil;

  return wrap_timeout_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (magic_data, Lisp_Magic_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_magic_data 1000

Lisp_Object
make_magic_data (void)
{
  Lisp_Magic_Data *d;

  ALLOCATE_FIXED_TYPE (magic_data, Lisp_Magic_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_magic_data);

  return wrap_magic_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (magic_eval_data, Lisp_Magic_Eval_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_magic_eval_data 1000

Lisp_Object
make_magic_eval_data (void)
{
  Lisp_Magic_Eval_Data *d;

  ALLOCATE_FIXED_TYPE (magic_eval_data, Lisp_Magic_Eval_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_magic_eval_data);
  d->object = Qnil;

  return wrap_magic_eval_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (eval_data, Lisp_Eval_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_eval_data 1000

Lisp_Object
make_eval_data (void)
{
  Lisp_Eval_Data *d;

  ALLOCATE_FIXED_TYPE (eval_data, Lisp_Eval_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_eval_data);
  d->function = Qnil;
  d->object = Qnil;

  return wrap_eval_data (d);
}

DECLARE_FIXED_TYPE_ALLOC (misc_user_data, Lisp_Misc_User_Data);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_misc_user_data 1000

Lisp_Object
make_misc_user_data (void)
{
  Lisp_Misc_User_Data *d;

  ALLOCATE_FIXED_TYPE (misc_user_data, Lisp_Misc_User_Data, d);
  xzero (*d);
  set_lheader_implementation (&d->lheader, &lrecord_misc_user_data);
  d->function = Qnil;
  d->object = Qnil;

  return wrap_misc_user_data (d);
}

#endif /* EVENT_DATA_AS_OBJECTS */

/************************************************************************/
/*			 Marker allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (marker, Lisp_Marker);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_marker 1000

DEFUN ("make-marker", Fmake_marker, 0, 0, 0, /*
Return a new marker which does not point at any place.
*/
       ())
{
  Lisp_Marker *p;

  ALLOCATE_FIXED_TYPE (marker, Lisp_Marker, p);
  set_lheader_implementation (&p->lheader, &lrecord_marker);
  p->buffer = 0;
  p->membpos = 0;
  marker_next (p) = 0;
  marker_prev (p) = 0;
  p->insertion_type = 0;
  return wrap_marker (p);
}

Lisp_Object
noseeum_make_marker (void)
{
  Lisp_Marker *p;

  NOSEEUM_ALLOCATE_FIXED_TYPE (marker, Lisp_Marker, p);
  set_lheader_implementation (&p->lheader, &lrecord_marker);
  p->buffer = 0;
  p->membpos = 0;
  marker_next (p) = 0;
  marker_prev (p) = 0;
  p->insertion_type = 0;
  return wrap_marker (p);
}


/************************************************************************/
/*			  String allocation				*/
/************************************************************************/

/* The data for "short" strings generally resides inside of structs of type
   string_chars_block. The Lisp_String structure is allocated just like any
   other basic lrecord, and these are freelisted when they get garbage
   collected. The data for short strings get compacted, but the data for
   large strings do not.

   Previously Lisp_String structures were relocated, but this caused a lot
   of bus-errors because the C code didn't include enough GCPRO's for
   strings (since EVERY REFERENCE to a short string needed to be GCPRO'd so
   that the reference would get relocated).

   This new method makes things somewhat bigger, but it is MUCH safer.  */

DECLARE_FIXED_TYPE_ALLOC (string, Lisp_String);
/* strings are used and freed quite often */
/* #define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_string 10000 */
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_string 1000

static Lisp_Object
mark_string (Lisp_Object obj)
{
  if (CONSP (XSTRING_PLIST (obj)) && EXTENT_INFOP (XCAR (XSTRING_PLIST (obj))))
    flush_cached_extent_info (XCAR (XSTRING_PLIST (obj)));
  return XSTRING_PLIST (obj);
}

static int
string_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Bytecount len;
  return (((len = XSTRING_LENGTH (obj1)) == XSTRING_LENGTH (obj2)) &&
	  !memcmp (XSTRING_DATA (obj1), XSTRING_DATA (obj2), len));
}

static const struct memory_description string_description[] = {
  { XD_BYTECOUNT,       offsetof (Lisp_String, size_) },
  { XD_OPAQUE_DATA_PTR, offsetof (Lisp_String, data_), XD_INDIRECT(0, 1) },
  { XD_LISP_OBJECT,     offsetof (Lisp_String, plist) },
  { XD_END }
};

/* We store the string's extent info as the first element of the string's
   property list; and the string's MODIFF as the first or second element
   of the string's property list (depending on whether the extent info
   is present), but only if the string has been modified.  This is ugly
   but it reduces the memory allocated for the string in the vast
   majority of cases, where the string is never modified and has no
   extent info.

   #### This means you can't use an int as a key in a string's plist. */

static Lisp_Object *
string_plist_ptr (Lisp_Object string)
{
  Lisp_Object *ptr = &XSTRING_PLIST (string);

  if (CONSP (*ptr) && EXTENT_INFOP (XCAR (*ptr)))
    ptr = &XCDR (*ptr);
  if (CONSP (*ptr) && INTP (XCAR (*ptr)))
    ptr = &XCDR (*ptr);
  return ptr;
}

static Lisp_Object
string_getprop (Lisp_Object string, Lisp_Object property)
{
  return external_plist_get (string_plist_ptr (string), property, 0, ERROR_ME);
}

static int
string_putprop (Lisp_Object string, Lisp_Object property, Lisp_Object value)
{
  external_plist_put (string_plist_ptr (string), property, value, 0, ERROR_ME);
  return 1;
}

static int
string_remprop (Lisp_Object string, Lisp_Object property)
{
  return external_remprop (string_plist_ptr (string), property, 0, ERROR_ME);
}

static Lisp_Object
string_plist (Lisp_Object string)
{
  return *string_plist_ptr (string);
}

/* No `finalize', or `hash' methods.
   internal_hash() already knows how to hash strings and finalization
   is done with the ADDITIONAL_FREE_string macro, which is the
   standard way to do finalization when using
   SWEEP_FIXED_TYPE_BLOCK(). */
DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS ("string", string,
						1, /*dumpable-flag*/
						mark_string, print_string,
						0, string_equal, 0,
						string_description,
						string_getprop,
						string_putprop,
						string_remprop,
						string_plist,
						Lisp_String);
/* String blocks contain this many useful bytes. */
#define STRING_CHARS_BLOCK_SIZE					\
  ((Bytecount) (8192 - MALLOC_OVERHEAD -			\
	        ((2 * sizeof (struct string_chars_block *))	\
	         + sizeof (EMACS_INT))))
/* Block header for small strings. */
struct string_chars_block
{
  EMACS_INT pos;
  struct string_chars_block *next;
  struct string_chars_block *prev;
  /* Contents of string_chars_block->string_chars are interleaved
     string_chars structures (see below) and the actual string data */
  unsigned char string_chars[STRING_CHARS_BLOCK_SIZE];
};

static struct string_chars_block *first_string_chars_block;
static struct string_chars_block *current_string_chars_block;

/* If SIZE is the length of a string, this returns how many bytes
 *  the string occupies in string_chars_block->string_chars
 *  (including alignment padding).
 */
#define STRING_FULLSIZE(size) \
   ALIGN_FOR_TYPE (((size) + 1 + sizeof (Lisp_String *)), Lisp_String *)

#define BIG_STRING_FULLSIZE_P(fullsize) ((fullsize) >= STRING_CHARS_BLOCK_SIZE)
#define BIG_STRING_SIZE_P(size) (BIG_STRING_FULLSIZE_P (STRING_FULLSIZE(size)))

#define STRING_CHARS_FREE_P(ptr) ((ptr)->string == NULL)
#define MARK_STRING_CHARS_AS_FREE(ptr) ((void) ((ptr)->string = NULL))

struct string_chars
{
  Lisp_String *string;
  unsigned char chars[1];
};

struct unused_string_chars
{
  Lisp_String *string;
  EMACS_INT fullsize;
};

static void
init_string_chars_alloc (void)
{
  first_string_chars_block = xnew (struct string_chars_block);
  first_string_chars_block->prev = 0;
  first_string_chars_block->next = 0;
  first_string_chars_block->pos = 0;
  current_string_chars_block = first_string_chars_block;
}

static Ibyte *
allocate_big_string_chars (Bytecount length)
{
  Ibyte *p = xnew_array (Ibyte, length);
  INCREMENT_CONS_COUNTER (length, "string chars");
  return p;
}

static struct string_chars *
allocate_string_chars_struct (Lisp_Object string_it_goes_with,
			      Bytecount fullsize)
{
  struct string_chars *s_chars;

  if (fullsize <=
      (countof (current_string_chars_block->string_chars)
       - current_string_chars_block->pos))
    {
      /* This string can fit in the current string chars block */
      s_chars = (struct string_chars *)
	(current_string_chars_block->string_chars
	 + current_string_chars_block->pos);
      current_string_chars_block->pos += fullsize;
    }
  else
    {
      /* Make a new current string chars block */
      struct string_chars_block *new_scb = xnew (struct string_chars_block);

      current_string_chars_block->next = new_scb;
      new_scb->prev = current_string_chars_block;
      new_scb->next = 0;
      current_string_chars_block = new_scb;
      new_scb->pos = fullsize;
      s_chars = (struct string_chars *)
	current_string_chars_block->string_chars;
    }

  s_chars->string = XSTRING (string_it_goes_with);

  INCREMENT_CONS_COUNTER (fullsize, "string chars");

  return s_chars;
}

#ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
void
sledgehammer_check_ascii_begin (Lisp_Object str)
{
  Bytecount i;

  for (i = 0; i < XSTRING_LENGTH (str); i++)
    {
      if (!byte_ascii_p (string_byte (str, i)))
	break;
    }

  assert (i == (Bytecount) XSTRING_ASCII_BEGIN (str) ||
	  (i > MAX_STRING_ASCII_BEGIN &&
	   (Bytecount) XSTRING_ASCII_BEGIN (str) ==
	   (Bytecount) MAX_STRING_ASCII_BEGIN));
}
#endif

/* You do NOT want to be calling this! (And if you do, you must call
   XSET_STRING_ASCII_BEGIN() after modifying the string.) Use ALLOCA ()
   instead and then call make_string() like the rest of the world. */

Lisp_Object
make_uninit_string (Bytecount length)
{
  Lisp_String *s;
  Bytecount fullsize = STRING_FULLSIZE (length);

  assert (length >= 0 && fullsize > 0);

  /* Allocate the string header */
  ALLOCATE_FIXED_TYPE (string, Lisp_String, s);
  xzero (*s);
  set_lheader_implementation (&s->u.lheader, &lrecord_string);
  
  set_lispstringp_data (s, BIG_STRING_FULLSIZE_P (fullsize)
		   ? allocate_big_string_chars (length + 1)
		   : allocate_string_chars_struct (wrap_string (s),
						   fullsize)->chars);

  set_lispstringp_length (s, length);
  s->plist = Qnil;
  set_string_byte (wrap_string (s), length, 0);

  return wrap_string (s);
}

#ifdef VERIFY_STRING_CHARS_INTEGRITY
static void verify_string_chars_integrity (void);
#endif

/* Resize the string S so that DELTA bytes can be inserted starting
   at POS.  If DELTA < 0, it means deletion starting at POS.  If
   POS < 0, resize the string but don't copy any characters.  Use
   this if you're planning on completely overwriting the string.
*/

void
resize_string (Lisp_Object s, Bytecount pos, Bytecount delta)
{
  Bytecount oldfullsize, newfullsize;
#ifdef VERIFY_STRING_CHARS_INTEGRITY
  verify_string_chars_integrity ();
#endif

#ifdef ERROR_CHECK_TEXT
  if (pos >= 0)
    {
      assert (pos <= XSTRING_LENGTH (s));
      if (delta < 0)
	assert (pos + (-delta) <= XSTRING_LENGTH (s));
    }
  else
    {
      if (delta < 0)
	assert ((-delta) <= XSTRING_LENGTH (s));
    }
#endif /* ERROR_CHECK_TEXT */

  if (delta == 0)
    /* simplest case: no size change. */
    return;

  if (pos >= 0 && delta < 0)
    /* If DELTA < 0, the functions below will delete the characters
       before POS.  We want to delete characters *after* POS, however,
       so convert this to the appropriate form. */
    pos += -delta;

  oldfullsize = STRING_FULLSIZE (XSTRING_LENGTH (s));
  newfullsize = STRING_FULLSIZE (XSTRING_LENGTH (s) + delta);

  if (BIG_STRING_FULLSIZE_P (oldfullsize))
    {
      if (BIG_STRING_FULLSIZE_P (newfullsize))
	{
	  /* Both strings are big.  We can just realloc().
	     But careful!  If the string is shrinking, we have to
	     memmove() _before_ realloc(), and if growing, we have to
	     memmove() _after_ realloc() - otherwise the access is
	     illegal, and we might crash. */
	  Bytecount len = XSTRING_LENGTH (s) + 1 - pos;

	  if (delta < 0 && pos >= 0)
	    memmove (XSTRING_DATA (s) + pos + delta,
		     XSTRING_DATA (s) + pos, len);
	  XSET_STRING_DATA
	    (s, (Ibyte *) xrealloc (XSTRING_DATA (s),
				      XSTRING_LENGTH (s) + delta + 1));
	  if (delta > 0 && pos >= 0)
	    memmove (XSTRING_DATA (s) + pos + delta, XSTRING_DATA (s) + pos,
		     len);
	  /* Bump the cons counter.
	     Conservative; Martin let the increment be delta. */
	  INCREMENT_CONS_COUNTER (newfullsize, "string chars");
	}
      else /* String has been demoted from BIG_STRING. */
	{
	  Ibyte *new_data =
	    allocate_string_chars_struct (s, newfullsize)->chars;
	  Ibyte *old_data = XSTRING_DATA (s);

	  if (pos >= 0)
	    {
	      memcpy (new_data, old_data, pos);
	      memcpy (new_data + pos + delta, old_data + pos,
		      XSTRING_LENGTH (s) + 1 - pos);
	    }
	  XSET_STRING_DATA (s, new_data);
	  xfree (old_data);
	}
    }
  else /* old string is small */
    {
      if (oldfullsize == newfullsize)
	{
	  /* special case; size change but the necessary
	     allocation size won't change (up or down; code
	     somewhere depends on there not being any unused
	     allocation space, modulo any alignment
	     constraints). */
	  if (pos >= 0)
	    {
	      Ibyte *addroff = pos + XSTRING_DATA (s);

	      memmove (addroff + delta, addroff,
		       /* +1 due to zero-termination. */
		       XSTRING_LENGTH (s) + 1 - pos);
	    }
	}
      else
	{
	  Ibyte *old_data = XSTRING_DATA (s);
	  Ibyte *new_data =
	    BIG_STRING_FULLSIZE_P (newfullsize)
	    ? allocate_big_string_chars (XSTRING_LENGTH (s) + delta + 1)
	    : allocate_string_chars_struct (s, newfullsize)->chars;

	  if (pos >= 0)
	    {
	      memcpy (new_data, old_data, pos);
	      memcpy (new_data + pos + delta, old_data + pos,
		      XSTRING_LENGTH (s) + 1 - pos);
	    }
	  XSET_STRING_DATA (s, new_data);

	  {
	    /* We need to mark this chunk of the string_chars_block
	       as unused so that compact_string_chars() doesn't
	       freak. */
	    struct string_chars *old_s_chars = (struct string_chars *)
	      ((char *) old_data - offsetof (struct string_chars, chars));
	    /* Sanity check to make sure we aren't hosed by strange
	       alignment/padding. */
	    assert (old_s_chars->string == XSTRING (s));
	    MARK_STRING_CHARS_AS_FREE (old_s_chars);
	    ((struct unused_string_chars *) old_s_chars)->fullsize =
	      oldfullsize;
	  }
	}
    }

  XSET_STRING_LENGTH (s, XSTRING_LENGTH (s) + delta);
  /* If pos < 0, the string won't be zero-terminated.
     Terminate now just to make sure. */
  XSTRING_DATA (s)[XSTRING_LENGTH (s)] = '\0';

  if (pos >= 0)
    /* We also have to adjust all of the extent indices after the
       place we did the change.  We say "pos - 1" because
       adjust_extents() is exclusive of the starting position
       passed to it. */
    adjust_extents (s, pos - 1, XSTRING_LENGTH (s), delta);

#ifdef VERIFY_STRING_CHARS_INTEGRITY
  verify_string_chars_integrity ();
#endif
}

#ifdef MULE

/* WARNING: If you modify an existing string, you must call
   CHECK_LISP_WRITEABLE() before and bump_string_modiff() afterwards. */
void
set_string_char (Lisp_Object s, Charcount i, Ichar c)
{
  Ibyte newstr[MAX_ICHAR_LEN];
  Bytecount bytoff = string_index_char_to_byte (s, i);
  Bytecount oldlen = itext_ichar_len (XSTRING_DATA (s) + bytoff);
  Bytecount newlen = set_itext_ichar (newstr, c);

  sledgehammer_check_ascii_begin (s);
  if (oldlen != newlen)
    resize_string (s, bytoff, newlen - oldlen);
  /* Remember, XSTRING_DATA (s) might have changed so we can't cache it. */
  memcpy (XSTRING_DATA (s) + bytoff, newstr, newlen);
  if (oldlen != newlen) 
    {
      if (newlen > 1 && i <= (Charcount) XSTRING_ASCII_BEGIN (s))
      /* Everything starting with the new char is no longer part of
	 ascii_begin */
	XSET_STRING_ASCII_BEGIN (s, i);
      else if (newlen == 1 && i == (Charcount) XSTRING_ASCII_BEGIN (s))
	/* We've extended ascii_begin, and we have to figure out how much by */
	{
	  Bytecount j;
	  for (j = (Bytecount) i + 1; j < XSTRING_LENGTH (s); j++)
	    {
	      if (!byte_ascii_p (XSTRING_DATA (s)[j]))
		break;
	    }
	  XSET_STRING_ASCII_BEGIN (s, min (j, (Bytecount) MAX_STRING_ASCII_BEGIN));
	}
    }
  sledgehammer_check_ascii_begin (s);
}

#endif /* MULE */

DEFUN ("make-string", Fmake_string, 2, 2, 0, /*
Return a new string consisting of LENGTH copies of CHARACTER.
LENGTH must be a non-negative integer.
*/
       (length, character))
{
  CHECK_NATNUM (length);
  CHECK_CHAR_COERCE_INT (character);
  {
    Ibyte init_str[MAX_ICHAR_LEN];
    int len = set_itext_ichar (init_str, XCHAR (character));
    Lisp_Object val = make_uninit_string (len * XINT (length));

    if (len == 1)
      {
	/* Optimize the single-byte case */
	memset (XSTRING_DATA (val), XCHAR (character), XSTRING_LENGTH (val));
	XSET_STRING_ASCII_BEGIN (val, min (MAX_STRING_ASCII_BEGIN,
					   len * XINT (length)));
      }
    else
      {
	EMACS_INT i;
	Ibyte *ptr = XSTRING_DATA (val);

	for (i = XINT (length); i; i--)
	  {
	    Ibyte *init_ptr = init_str;
	    switch (len)
	      {
	      case 4: *ptr++ = *init_ptr++;
	      case 3: *ptr++ = *init_ptr++;
	      case 2: *ptr++ = *init_ptr++;
	      case 1: *ptr++ = *init_ptr++;
	      }
	  }
      }
    sledgehammer_check_ascii_begin (val);
    return val;
  }
}

DEFUN ("string", Fstring, 0, MANY, 0, /*
Concatenate all the argument characters and make the result a string.
*/
       (int nargs, Lisp_Object *args))
{
  Ibyte *storage = alloca_array (Ibyte, nargs * MAX_ICHAR_LEN);
  Ibyte *p = storage;

  for (; nargs; nargs--, args++)
    {
      Lisp_Object lisp_char = *args;
      CHECK_CHAR_COERCE_INT (lisp_char);
      p += set_itext_ichar (p, XCHAR (lisp_char));
    }
  return make_string (storage, p - storage);
}

/* Initialize the ascii_begin member of a string to the correct value. */

void
init_string_ascii_begin (Lisp_Object string)
{
#ifdef MULE
  int i;
  Bytecount length = XSTRING_LENGTH (string);
  Ibyte *contents = XSTRING_DATA (string);

  for (i = 0; i < length; i++)
    {
      if (!byte_ascii_p (contents[i]))
	break;
    }
  XSET_STRING_ASCII_BEGIN (string, min (i, MAX_STRING_ASCII_BEGIN));
#else
  XSET_STRING_ASCII_BEGIN (string, min (XSTRING_LENGTH (string),
					MAX_STRING_ASCII_BEGIN));
#endif
  sledgehammer_check_ascii_begin (string);
}

/* Take some raw memory, which MUST already be in internal format,
   and package it up into a Lisp string. */
Lisp_Object
make_string (const Ibyte *contents, Bytecount length)
{
  Lisp_Object val;

  /* Make sure we find out about bad make_string's when they happen */
#if defined (ERROR_CHECK_TEXT) && defined (MULE)
  bytecount_to_charcount (contents, length); /* Just for the assertions */
#endif

  val = make_uninit_string (length);
  memcpy (XSTRING_DATA (val), contents, length);
  init_string_ascii_begin (val);
  sledgehammer_check_ascii_begin (val);  
  return val;
}

/* Take some raw memory, encoded in some external data format,
   and convert it into a Lisp string. */
Lisp_Object
make_ext_string (const Extbyte *contents, EMACS_INT length,
		 Lisp_Object coding_system)
{
  Lisp_Object string;
  TO_INTERNAL_FORMAT (DATA, (contents, length),
		      LISP_STRING, string,
		      coding_system);
  return string;
}

Lisp_Object
build_intstring (const Ibyte *str)
{
  /* Some strlen's crash and burn if passed null. */
  return make_string (str, (str ? qxestrlen (str) : (Bytecount) 0));
}

Lisp_Object
build_string (const CIbyte *str)
{
  /* Some strlen's crash and burn if passed null. */
  return make_string ((const Ibyte *) str, (str ? strlen (str) : 0));
}

Lisp_Object
build_ext_string (const Extbyte *str, Lisp_Object coding_system)
{
  /* Some strlen's crash and burn if passed null. */
  return make_ext_string ((const Extbyte *) str, (str ? strlen(str) : 0),
			  coding_system);
}

Lisp_Object
build_msg_intstring (const Ibyte *str)
{
  return build_intstring (GETTEXT (str));
}

Lisp_Object
build_msg_string (const CIbyte *str)
{
  return build_string (CGETTEXT (str));
}

Lisp_Object
make_string_nocopy (const Ibyte *contents, Bytecount length)
{
  Lisp_String *s;
  Lisp_Object val;

  /* Make sure we find out about bad make_string_nocopy's when they happen */
#if defined (ERROR_CHECK_TEXT) && defined (MULE)
  bytecount_to_charcount (contents, length); /* Just for the assertions */
#endif

  /* Allocate the string header */
  ALLOCATE_FIXED_TYPE (string, Lisp_String, s);
  set_lheader_implementation (&s->u.lheader, &lrecord_string);
  SET_C_READONLY_RECORD_HEADER (&s->u.lheader);
  s->plist = Qnil;
  set_lispstringp_data (s, (Ibyte *) contents);
  set_lispstringp_length (s, length);
  val = wrap_string (s);
  init_string_ascii_begin (val);
  sledgehammer_check_ascii_begin (val);

  return val;
}


/************************************************************************/
/*                           lcrecord lists                             */
/************************************************************************/

/* Lcrecord lists are used to manage the allocation of particular
   sorts of lcrecords, to avoid calling basic_alloc_lcrecord() (and thus
   malloc() and garbage-collection junk) as much as possible.
   It is similar to the Blocktype class.

   See detailed comment in lcrecord.h.
*/

const struct memory_description free_description[] = {
  { XD_LISP_OBJECT, offsetof (struct free_lcrecord_header, chain), 0, 0,
    XD_FLAG_FREE_LISP_OBJECT },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("free", free,
			       0, /*dumpable-flag*/
			       0, internal_object_printer,
			       0, 0, 0, free_description,
			       struct free_lcrecord_header);

const struct memory_description lcrecord_list_description[] = {
  { XD_LISP_OBJECT, offsetof (struct lcrecord_list, free), 0, 0,
    XD_FLAG_FREE_LISP_OBJECT },
  { XD_END }
};

static Lisp_Object
mark_lcrecord_list (Lisp_Object obj)
{
  struct lcrecord_list *list = XLCRECORD_LIST (obj);
  Lisp_Object chain = list->free;

  while (!NILP (chain))
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (chain);
      struct free_lcrecord_header *free_header =
	(struct free_lcrecord_header *) lheader;

      gc_checking_assert
	(/* There should be no other pointers to the free list. */
	 ! MARKED_RECORD_HEADER_P (lheader)
	 &&
	 /* Only lcrecords should be here. */
	 ! list->implementation->basic_p
	 &&
	 /* Only free lcrecords should be here. */
	 free_header->lcheader.free
	 &&
	 /* The type of the lcrecord must be right. */
	 lheader->type == lrecord_type_free
	 &&
	 /* So must the size. */
	 (list->implementation->static_size == 0 ||
	  list->implementation->static_size == list->size)
	 );

      MARK_RECORD_HEADER (lheader);
      chain = free_header->chain;
    }

  return Qnil;
}

DEFINE_LRECORD_IMPLEMENTATION ("lcrecord-list", lcrecord_list,
			       0, /*dumpable-flag*/
			       mark_lcrecord_list, internal_object_printer,
			       0, 0, 0, lcrecord_list_description,
			       struct lcrecord_list);

Lisp_Object
make_lcrecord_list (Elemcount size,
		    const struct lrecord_implementation *implementation)
{
  /* Don't use alloc_lcrecord_type() avoid infinite recursion
     allocating this, */
  struct lcrecord_list *p = (struct lcrecord_list *)
    basic_alloc_lcrecord (sizeof (struct lcrecord_list),
			  &lrecord_lcrecord_list);

  p->implementation = implementation;
  p->size = size;
  p->free = Qnil;
  return wrap_lcrecord_list (p);
}

Lisp_Object
alloc_managed_lcrecord (Lisp_Object lcrecord_list)
{
  struct lcrecord_list *list = XLCRECORD_LIST (lcrecord_list);
  if (!NILP (list->free))
    {
      Lisp_Object val = list->free;
      struct free_lcrecord_header *free_header =
	(struct free_lcrecord_header *) XPNTR (val);
      struct lrecord_header *lheader = &free_header->lcheader.lheader;

#ifdef ERROR_CHECK_GC
      /* Major overkill here. */
      /* There should be no other pointers to the free list. */
      assert (! MARKED_RECORD_HEADER_P (lheader));
      /* Only free lcrecords should be here. */
      assert (free_header->lcheader.free);
      assert (lheader->type == lrecord_type_free);
      /* Only lcrecords should be here. */
      assert (! (list->implementation->basic_p));
#if 0 /* Not used anymore, now that we set the type of the header to
	 lrecord_type_free. */
      /* The type of the lcrecord must be right. */
      assert (LHEADER_IMPLEMENTATION (lheader) == list->implementation);
#endif /* 0 */
      /* So must the size. */
      assert (list->implementation->static_size == 0 ||
	      list->implementation->static_size == list->size);
#endif /* ERROR_CHECK_GC */

      list->free = free_header->chain;
      free_header->lcheader.free = 0;
      /* Put back the correct type, as we set it to lrecord_type_free. */
      lheader->type = list->implementation->lrecord_type_index;
      zero_sized_lcrecord (free_header, list->size);
      return val;
    }
  else
    return wrap_pointer_1 (basic_alloc_lcrecord (list->size,
						 list->implementation));
}

/* "Free" a Lisp object LCRECORD by placing it on its associated free list
   LCRECORD_LIST; next time alloc_managed_lcrecord() is called with the
   same LCRECORD_LIST as its parameter, it will return an object from the
   free list, which may be this one.  Be VERY VERY SURE there are no
   pointers to this object hanging around anywhere where they might be
   used!

   The first thing this does before making any global state change is to
   call the finalize method of the object, if it exists. */

void
free_managed_lcrecord (Lisp_Object lcrecord_list, Lisp_Object lcrecord)
{
  struct lcrecord_list *list = XLCRECORD_LIST (lcrecord_list);
  struct free_lcrecord_header *free_header =
    (struct free_lcrecord_header *) XPNTR (lcrecord);
  struct lrecord_header *lheader = &free_header->lcheader.lheader;
  const struct lrecord_implementation *implementation
    = LHEADER_IMPLEMENTATION (lheader);

  /* Finalizer methods may try to free objects within them, which typically
     won't be marked and thus are scheduled for demolition.  Putting them
     on the free list would be very bad, as we'd have xfree()d memory in
     the list.  Even if for some reason the objects are still live
     (generally a logic error!), we still will have problems putting such
     an object on the free list right now (e.g. we'd have to avoid calling
     the finalizer twice, etc.).  So basically, those finalizers should not
     be freeing any objects if during GC.  Abort now to catch those
     problems. */
  gc_checking_assert (!gc_in_progress);
  
  /* Make sure the size is correct.  This will catch, for example,
     putting a window configuration on the wrong free list. */
  gc_checking_assert (detagged_lisp_object_size (lheader) == list->size);
  /* Make sure the object isn't already freed. */
  gc_checking_assert (!free_header->lcheader.free);
  
  if (implementation->finalizer)
    implementation->finalizer (lheader, 0);
  /* Yes, there are two ways to indicate freeness -- the type is
     lrecord_type_free or the ->free flag is set.  We used to do only the
     latter; now we do the former as well for KKCC purposes.  Probably
     safer in any case, as we will lose quicker this way than keeping
     around an lrecord of apparently correct type but bogus junk in it. */
  MARK_LRECORD_AS_FREE (lheader);
  free_header->chain = list->free;
  free_header->lcheader.free = 1;
  list->free = lcrecord;
}

static Lisp_Object all_lcrecord_lists[countof (lrecord_implementations_table)];

void *
alloc_automanaged_lcrecord (Bytecount size,
			    const struct lrecord_implementation *imp)
{
  if (EQ (all_lcrecord_lists[imp->lrecord_type_index], Qzero))
    all_lcrecord_lists[imp->lrecord_type_index] =
      make_lcrecord_list (size, imp);

  return XPNTR (alloc_managed_lcrecord
		(all_lcrecord_lists[imp->lrecord_type_index]));
}

void
free_lcrecord (Lisp_Object rec)
{
  int type = XRECORD_LHEADER (rec)->type;

  assert (!EQ (all_lcrecord_lists[type], Qzero));

  free_managed_lcrecord (all_lcrecord_lists[type], rec);
}


DEFUN ("purecopy", Fpurecopy, 1, 1, 0, /*
Kept for compatibility, returns its argument.
Old:
Make a copy of OBJECT in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.
*/
       (object))
{
  return object;
}


/************************************************************************/
/*			   Garbage Collection				*/
/************************************************************************/

/* All the built-in lisp object types are enumerated in `enum lrecord_type'.
   Additional ones may be defined by a module (none yet).  We leave some
   room in `lrecord_implementations_table' for such new lisp object types. */
const struct lrecord_implementation *lrecord_implementations_table[(int)lrecord_type_last_built_in_type + MODULE_DEFINABLE_TYPE_COUNT];
int lrecord_type_count = lrecord_type_last_built_in_type;
#ifndef USE_KKCC
/* Object marker functions are in the lrecord_implementation structure.
   But copying them to a parallel array is much more cache-friendly.
   This hack speeds up (garbage-collect) by about 5%. */
Lisp_Object (*lrecord_markers[countof (lrecord_implementations_table)]) (Lisp_Object);
#endif /* not USE_KKCC */

struct gcpro *gcprolist;

/* We want the staticpro list relocated, but not the pointers found
   therein, because they refer to locations in the global data segment, not
   in the heap; we only dump heap objects.  Hence we use a trivial
   description, as for pointerless objects. (Note that the data segment
   objects, which are global variables like Qfoo or Vbar, themselves are
   pointers to heap objects.  Each needs to be described to pdump as a
   "root pointer"; this happens in the call to staticpro(). */
static const struct memory_description staticpro_description_1[] = {
  { XD_END }
};

static const struct sized_memory_description staticpro_description = {
  sizeof (Lisp_Object *),
  staticpro_description_1
};

static const struct memory_description staticpros_description_1[] = {
  XD_DYNARR_DESC (Lisp_Object_ptr_dynarr, &staticpro_description),
  { XD_END }
};

static const struct sized_memory_description staticpros_description = {
  sizeof (Lisp_Object_ptr_dynarr),
  staticpros_description_1
};

#ifdef DEBUG_XEMACS

static const struct memory_description staticpro_one_name_description_1[] = {
  { XD_C_STRING, 0 },
  { XD_END }
};

static const struct sized_memory_description staticpro_one_name_description = {
  sizeof (char *),
  staticpro_one_name_description_1
};

static const struct memory_description staticpro_names_description_1[] = {
  XD_DYNARR_DESC (char_ptr_dynarr, &staticpro_one_name_description),
  { XD_END }
};


extern const struct sized_memory_description staticpro_names_description;

const struct sized_memory_description staticpro_names_description = {
  sizeof (char_ptr_dynarr),
  staticpro_names_description_1
};

/* Help debug crashes gc-marking a staticpro'ed object. */

Lisp_Object_ptr_dynarr *staticpros;
char_ptr_dynarr *staticpro_names;

/* Mark the Lisp_Object at non-heap VARADDRESS as a root object for
   garbage collection, and for dumping. */
void
staticpro_1 (Lisp_Object *varaddress, char *varname)
{
  Dynarr_add (staticpros, varaddress);
  Dynarr_add (staticpro_names, varname);
  dump_add_root_lisp_object (varaddress);
}


Lisp_Object_ptr_dynarr *staticpros_nodump;
char_ptr_dynarr *staticpro_nodump_names;

/* Mark the Lisp_Object at heap VARADDRESS as a root object for
   garbage collection, but not for dumping. (See below.) */
void
staticpro_nodump_1 (Lisp_Object *varaddress, char *varname)
{
  Dynarr_add (staticpros_nodump, varaddress);
  Dynarr_add (staticpro_nodump_names, varname);
}

#ifdef HAVE_SHLIB
/* Stop treating the Lisp_Object at non-heap VARADDRESS as a root object
   for garbage collection, but not for dumping. */
void
unstaticpro_nodump_1 (Lisp_Object *varaddress, char *varname)
{
  Dynarr_delete_object (staticpros, varaddress);
  Dynarr_delete_object (staticpro_names, varname);
}
#endif

#else /* not DEBUG_XEMACS */

Lisp_Object_ptr_dynarr *staticpros;

/* Mark the Lisp_Object at non-heap VARADDRESS as a root object for
   garbage collection, and for dumping. */
void
staticpro (Lisp_Object *varaddress)
{
  Dynarr_add (staticpros, varaddress);
  dump_add_root_lisp_object (varaddress);
}


Lisp_Object_ptr_dynarr *staticpros_nodump;

/* Mark the Lisp_Object at heap VARADDRESS as a root object for garbage
   collection, but not for dumping.  This is used for objects where the
   only sure pointer is in the heap (rather than in the global data
   segment, as must be the case for pdump root pointers), but not inside of
   another Lisp object (where it will be marked as a result of that Lisp
   object's mark method).  The call to staticpro_nodump() must occur *BOTH*
   at initialization time and at "reinitialization" time (startup, after
   pdump load.) (For example, this is the case with the predicate symbols
   for specifier and coding system types.  The pointer to this symbol is
   inside of a methods structure, which is allocated on the heap.  The
   methods structure will be written out to the pdump data file, and may be
   reloaded at a different address.)

   #### The necessity for reinitialization is a bug in pdump.  Pdump should
   automatically regenerate the staticpro()s for these symbols when it
   loads the data in. */

void
staticpro_nodump (Lisp_Object *varaddress)
{
  Dynarr_add (staticpros_nodump, varaddress);
}

#ifdef HAVE_SHLIB
/* Unmark the Lisp_Object at non-heap VARADDRESS as a root object for
   garbage collection, but not for dumping. */
void
unstaticpro_nodump (Lisp_Object *varaddress)
{
  Dynarr_delete_object (staticpros, varaddress);
}
#endif

#endif /* not DEBUG_XEMACS */

#ifdef ERROR_CHECK_GC
#define GC_CHECK_LHEADER_INVARIANTS(lheader) do {		\
  struct lrecord_header * GCLI_lh = (lheader);			\
  assert (GCLI_lh != 0);					\
  assert (GCLI_lh->type < (unsigned int) lrecord_type_count);	\
  assert (! C_READONLY_RECORD_HEADER_P (GCLI_lh) ||		\
	  (MARKED_RECORD_HEADER_P (GCLI_lh) &&			\
	   LISP_READONLY_RECORD_HEADER_P (GCLI_lh)));		\
} while (0)
#else
#define GC_CHECK_LHEADER_INVARIANTS(lheader)
#endif


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
#ifdef PDUMP
      if (in_pdump)
	pdump_backtrace ();
#endif
      count = 0; /* warning suppression */
      abort ();
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
    case XD_STRUCT_PTR:
      {
	EMACS_INT val = lispdesc_indirect_count (desc1->data1, desc, obj);
	return val * sizeof (void *);
      }
    case XD_STRUCT_ARRAY:
      {
	EMACS_INT val = lispdesc_indirect_count (desc1->data1, desc, obj);
	    
	return (val *
		lispdesc_structure_size
		(rdata, lispdesc_indirect_description (obj, desc1->data2)));
      }
    case XD_OPAQUE_DATA_PTR:
      return sizeof (void *);
    case XD_UNION_DYNAMIC_SIZE:
      {
	/* If an explicit size was given in the first-level structure
	   description, use it; else compute size based on current union
	   constant. */
	const struct sized_memory_description *sdesc =
	  lispdesc_indirect_description (obj, desc1->data2);
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
	  lispdesc_indirect_description (obj, desc1->data2);
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
    case XD_C_STRING:
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
      abort ();
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
lispdesc_structure_size (const void *obj,
			 const struct sized_memory_description *sdesc)
{
  EMACS_INT max_offset = -1;
  int max_offset_pos = -1;
  int pos;
  const struct memory_description *desc;

  if (sdesc->size)
    return sdesc->size;

  desc = sdesc->description;

  for (pos = 0; desc[pos].type != XD_END; pos++)
    {
      EMACS_INT offset = lispdesc_indirect_count (desc[pos].offset, desc, obj);
      if (offset == max_offset)
	{
	  stderr_out ("Two relocatable elements at same offset?\n");
	  abort ();
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

#define GC_CHECK_NOT_FREE(lheader)					\
      gc_checking_assert (LHEADER_IMPLEMENTATION (lheader)->basic_p ||	\
			  ! ((struct lcrecord_header *) lheader)->free)

#ifdef USE_KKCC
/* The following functions implement the new mark algorithm. 
   They mark objects according to their descriptions.  They 
   are modeled on the corresponding pdumper procedures. */

/* Object memory descriptions are in the lrecord_implementation structure.
   But copying them to a parallel array is much more cache-friendly. */
const struct memory_description *lrecord_memory_descriptions[countof (lrecord_implementations_table)];

/* the initial stack size in kkcc_gc_stack_entries */
#define KKCC_INIT_GC_STACK_SIZE 16384

typedef struct
{
  void *data;
  const struct memory_description *desc;
} kkcc_gc_stack_entry;

static kkcc_gc_stack_entry *kkcc_gc_stack_ptr;
static kkcc_gc_stack_entry *kkcc_gc_stack_top;
static kkcc_gc_stack_entry *kkcc_gc_stack_last_entry;
static int kkcc_gc_stack_size;

static void
kkcc_gc_stack_init (void)
{
  kkcc_gc_stack_size = KKCC_INIT_GC_STACK_SIZE;
  kkcc_gc_stack_ptr = (kkcc_gc_stack_entry *)
    malloc (kkcc_gc_stack_size * sizeof (kkcc_gc_stack_entry));
  if (!kkcc_gc_stack_ptr) 
    {
      stderr_out ("stack init failed for size %d\n", kkcc_gc_stack_size);
      exit(23);
    }
  kkcc_gc_stack_top = kkcc_gc_stack_ptr - 1;
  kkcc_gc_stack_last_entry = kkcc_gc_stack_ptr + kkcc_gc_stack_size - 1;
}

static void
kkcc_gc_stack_free (void)
{
  free (kkcc_gc_stack_ptr);
  kkcc_gc_stack_ptr = 0;
  kkcc_gc_stack_top = 0;
  kkcc_gc_stack_size = 0;
}

static void
kkcc_gc_stack_realloc (void)
{
  int current_offset = (int)(kkcc_gc_stack_top - kkcc_gc_stack_ptr);
  kkcc_gc_stack_size *= 2;
  kkcc_gc_stack_ptr = (kkcc_gc_stack_entry *)
    realloc (kkcc_gc_stack_ptr, 
	     kkcc_gc_stack_size * sizeof (kkcc_gc_stack_entry));
  if (!kkcc_gc_stack_ptr) 
    {
      stderr_out ("stack realloc failed for size %d\n", kkcc_gc_stack_size);
      exit(23);
    }
  kkcc_gc_stack_top = kkcc_gc_stack_ptr + current_offset;
  kkcc_gc_stack_last_entry = kkcc_gc_stack_ptr + kkcc_gc_stack_size - 1;
}

#define KKCC_GC_STACK_FULL (kkcc_gc_stack_top >= kkcc_gc_stack_last_entry)
#define KKCC_GC_STACK_EMPTY (kkcc_gc_stack_top < kkcc_gc_stack_ptr)

static void
kkcc_gc_stack_push (void *data, const struct memory_description *desc)
{
  if (KKCC_GC_STACK_FULL)
      kkcc_gc_stack_realloc();
  kkcc_gc_stack_top++;
  kkcc_gc_stack_top->data = data;
  kkcc_gc_stack_top->desc = desc;
}

static kkcc_gc_stack_entry *
kkcc_gc_stack_pop (void)
{
  if (KKCC_GC_STACK_EMPTY)
    return 0;
  kkcc_gc_stack_top--;
  return kkcc_gc_stack_top + 1;
}

void
kkcc_gc_stack_push_lisp_object (Lisp_Object obj)
{
  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);
      const struct memory_description *desc;
      GC_CHECK_LHEADER_INVARIANTS (lheader);
      desc = RECORD_DESCRIPTION (lheader);
      if (! MARKED_RECORD_HEADER_P (lheader)) 
	{
	  MARK_RECORD_HEADER (lheader);
	  kkcc_gc_stack_push((void*) lheader, desc);
	}
    }
}

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
static void
mark_object_maybe_checking_free (Lisp_Object obj, int allow_free)
{
  KKCC_DO_CHECK_FREE (obj, allow_free);
  kkcc_gc_stack_push_lisp_object (obj);
}
#else
#define mark_object_maybe_checking_free(obj, allow_free) 	\
      kkcc_gc_stack_push_lisp_object (obj)
#endif /* ERROR_CHECK_GC */


/* This function loops all elements of a struct pointer and calls 
   mark_with_description with each element. */
static void
mark_struct_contents (const void *data,
		      const struct sized_memory_description *sdesc,
		      int count)
{
  int i;
  Bytecount elsize;
  elsize = lispdesc_structure_size (data, sdesc);

  for (i = 0; i < count; i++)
    {
      kkcc_gc_stack_push (((char *) data) + elsize * i, sdesc->description);
    }
}


/* This function implements the KKCC mark algorithm.
   Instead of calling mark_object, all the alive Lisp_Objects are pushed
   on the kkcc_gc_stack. This function processes all elements on the stack
   according to their descriptions. */
static void
kkcc_marking (void) 
{
  kkcc_gc_stack_entry *stack_entry = 0;
  void *data = 0;
  const struct memory_description *desc = 0;
  int pos;
  
  while ((stack_entry = kkcc_gc_stack_pop ()) != 0)
    {
      data = stack_entry->data;
      desc = stack_entry->desc;

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
	    case XD_C_STRING:
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
		mark_object_maybe_checking_free
		  (*stored_obj, (desc1->flags) & XD_FLAG_FREE_LISP_OBJECT);
	    
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

		    mark_object_maybe_checking_free
		      (*stored_obj, (desc1->flags) & XD_FLAG_FREE_LISP_OBJECT);
		  }
		break;
	      }
	    case XD_STRUCT_PTR:
	      {
		EMACS_INT count = lispdesc_indirect_count (desc1->data1, desc,
							   data);
		const struct sized_memory_description *sdesc =
		  lispdesc_indirect_description (data, desc1->data2);
		const char *dobj = * (const char **) rdata;
		if (dobj)
		  mark_struct_contents (dobj, sdesc, count);
		break;
	      }
	    case XD_STRUCT_ARRAY:
	      {
		EMACS_INT count = lispdesc_indirect_count (desc1->data1, desc,
							   data);
		const struct sized_memory_description *sdesc =
		  lispdesc_indirect_description (data, desc1->data2);
		      
		mark_struct_contents (rdata, sdesc, count);
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
	      abort ();
	    }
	}
    }
}
#endif /* USE_KKCC */  

/* Mark reference to a Lisp_Object.  If the object referred to has not been
   seen yet, recursively mark all the references contained in it. */

void
mark_object (Lisp_Object obj)
{
#ifdef USE_KKCC
  /* this code should never be reached when configured for KKCC */
  stderr_out ("KKCC: Invalid mark_object call.\n");
  stderr_out ("Replace mark_object with kkcc_gc_stack_push_lisp_object.\n");
  abort ();
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


static int gc_count_num_short_string_in_use;
static Bytecount gc_count_string_total_size;
static Bytecount gc_count_short_string_total_size;

/* static int gc_count_total_records_used, gc_count_records_total_size; */


/* stats on lcrecords in use - kinda kludgy */

static struct
{
  int instances_in_use;
  int bytes_in_use;
  int instances_freed;
  int bytes_freed;
  int instances_on_free_list;
} lcrecord_stats [countof (lrecord_implementations_table)
		  + MODULE_DEFINABLE_TYPE_COUNT];

static void
tick_lcrecord_stats (const struct lrecord_header *h, int free_p)
{
  int type_index = h->type;

  if (((struct lcrecord_header *) h)->free)
    {
      gc_checking_assert (!free_p);
      lcrecord_stats[type_index].instances_on_free_list++;
    }
  else
    {
      Bytecount sz = detagged_lisp_object_size (h);

      if (free_p)
	{
	  lcrecord_stats[type_index].instances_freed++;
	  lcrecord_stats[type_index].bytes_freed += sz;
	}
      else
	{
	  lcrecord_stats[type_index].instances_in_use++;
	  lcrecord_stats[type_index].bytes_in_use += sz;
	}
    }
}


/* Free all unmarked records */
static void
sweep_lcrecords_1 (struct lcrecord_header **prev, int *used)
{
  struct lcrecord_header *header;
  int num_used = 0;
  /* int total_size = 0; */

  xzero (lcrecord_stats); /* Reset all statistics to 0. */

  /* First go through and call all the finalize methods.
     Then go through and free the objects.  There used to
     be only one loop here, with the call to the finalizer
     occurring directly before the xfree() below.  That
     is marginally faster but much less safe -- if the
     finalize method for an object needs to reference any
     other objects contained within it (and many do),
     we could easily be screwed by having already freed that
     other object. */

  for (header = *prev; header; header = header->next)
    {
      struct lrecord_header *h = &(header->lheader);

      GC_CHECK_LHEADER_INVARIANTS (h);

      if (! MARKED_RECORD_HEADER_P (h) && ! header->free)
	{
	  if (LHEADER_IMPLEMENTATION (h)->finalizer)
	    LHEADER_IMPLEMENTATION (h)->finalizer (h, 0);
	}
    }

  for (header = *prev; header; )
    {
      struct lrecord_header *h = &(header->lheader);
      if (MARKED_RECORD_HEADER_P (h))
	{
	  if (! C_READONLY_RECORD_HEADER_P (h))
	    UNMARK_RECORD_HEADER (h);
	  num_used++;
	  /* total_size += n->implementation->size_in_bytes (h);*/
	  /* #### May modify header->next on a C_READONLY lcrecord */
	  prev = &(header->next);
	  header = *prev;
	  tick_lcrecord_stats (h, 0);
	}
      else
	{
	  struct lcrecord_header *next = header->next;
          *prev = next;
	  tick_lcrecord_stats (h, 1);
	  /* used to call finalizer right here. */
	  xfree (header);
	  header = next;
	}
    }
  *used = num_used;
  /* *total = total_size; */
}

/* And the Lord said: Thou shalt use the `c-backslash-region' command
   to make macros prettier. */

#ifdef ERROR_CHECK_GC

#define SWEEP_FIXED_TYPE_BLOCK_1(typename, obj_type, lheader)		\
do {									\
  struct typename##_block *SFTB_current;				\
  int SFTB_limit;							\
  int num_free = 0, num_used = 0;					\
									\
  for (SFTB_current = current_##typename##_block,			\
       SFTB_limit = current_##typename##_block_index;			\
       SFTB_current;							\
       )								\
    {									\
      int SFTB_iii;							\
									\
      for (SFTB_iii = 0; SFTB_iii < SFTB_limit; SFTB_iii++)		\
	{								\
	  obj_type *SFTB_victim = &(SFTB_current->block[SFTB_iii]);	\
									\
	  if (LRECORD_FREE_P (SFTB_victim))				\
	    {								\
	      num_free++;						\
	    }								\
	  else if (C_READONLY_RECORD_HEADER_P (&SFTB_victim->lheader))	\
	    {								\
	      num_used++;						\
	    }								\
	  else if (! MARKED_RECORD_HEADER_P (&SFTB_victim->lheader))	\
	    {								\
	      num_free++;						\
	      FREE_FIXED_TYPE (typename, obj_type, SFTB_victim);	\
	    }								\
	  else								\
	    {								\
	      num_used++;						\
	      UNMARK_##typename (SFTB_victim);				\
	    }								\
	}								\
      SFTB_current = SFTB_current->prev;				\
      SFTB_limit = countof (current_##typename##_block->block);		\
    }									\
									\
  gc_count_num_##typename##_in_use = num_used;				\
  gc_count_num_##typename##_freelist = num_free;			\
} while (0)

#else /* !ERROR_CHECK_GC */

#define SWEEP_FIXED_TYPE_BLOCK_1(typename, obj_type, lheader)		     \
do {									     \
  struct typename##_block *SFTB_current;				     \
  struct typename##_block **SFTB_prev;					     \
  int SFTB_limit;							     \
  int num_free = 0, num_used = 0;					     \
									     \
  typename##_free_list = 0;						     \
									     \
  for (SFTB_prev = &current_##typename##_block,				     \
       SFTB_current = current_##typename##_block,			     \
       SFTB_limit = current_##typename##_block_index;			     \
       SFTB_current;							     \
       )								     \
    {									     \
      int SFTB_iii;							     \
      int SFTB_empty = 1;						     \
      Lisp_Free *SFTB_old_free_list = typename##_free_list;		     \
									     \
      for (SFTB_iii = 0; SFTB_iii < SFTB_limit; SFTB_iii++)		     \
	{								     \
	  obj_type *SFTB_victim = &(SFTB_current->block[SFTB_iii]);	     \
									     \
	  if (LRECORD_FREE_P (SFTB_victim))				     \
	    {								     \
	      num_free++;						     \
	      PUT_FIXED_TYPE_ON_FREE_LIST (typename, obj_type, SFTB_victim); \
	    }								     \
	  else if (C_READONLY_RECORD_HEADER_P (&SFTB_victim->lheader))	     \
	    {								     \
	      SFTB_empty = 0;						     \
	      num_used++;						     \
	    }								     \
	  else if (! MARKED_RECORD_HEADER_P (&SFTB_victim->lheader))	     \
	    {								     \
	      num_free++;						     \
	      FREE_FIXED_TYPE (typename, obj_type, SFTB_victim);	     \
	    }								     \
	  else								     \
	    {								     \
	      SFTB_empty = 0;						     \
	      num_used++;						     \
	      UNMARK_##typename (SFTB_victim);				     \
	    }								     \
	}								     \
      if (!SFTB_empty)							     \
	{								     \
	  SFTB_prev = &(SFTB_current->prev);				     \
	  SFTB_current = SFTB_current->prev;				     \
	}								     \
      else if (SFTB_current == current_##typename##_block		     \
	       && !SFTB_current->prev)					     \
	{								     \
	  /* No real point in freeing sole allocation block */		     \
	  break;							     \
	}								     \
      else								     \
	{								     \
	  struct typename##_block *SFTB_victim_block = SFTB_current;	     \
	  if (SFTB_victim_block == current_##typename##_block)		     \
	    current_##typename##_block_index				     \
	      = countof (current_##typename##_block->block);		     \
	  SFTB_current = SFTB_current->prev;				     \
	  {								     \
	    *SFTB_prev = SFTB_current;					     \
	    xfree (SFTB_victim_block);					     \
	    /* Restore free list to what it was before victim was swept */   \
	    typename##_free_list = SFTB_old_free_list;			     \
	    num_free -= SFTB_limit;					     \
	  }								     \
	}								     \
      SFTB_limit = countof (current_##typename##_block->block);		     \
    }									     \
									     \
  gc_count_num_##typename##_in_use = num_used;				     \
  gc_count_num_##typename##_freelist = num_free;			     \
} while (0)

#endif /* !ERROR_CHECK_GC */

#define SWEEP_FIXED_TYPE_BLOCK(typename, obj_type) \
  SWEEP_FIXED_TYPE_BLOCK_1 (typename, obj_type, lheader)




static void
sweep_conses (void)
{
#define UNMARK_cons(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_cons(ptr)

  SWEEP_FIXED_TYPE_BLOCK (cons, Lisp_Cons);
}

/* Explicitly free a cons cell.  */
void
free_cons (Lisp_Object cons)
{
  Lisp_Cons *ptr = XCONS (cons);

#ifdef ERROR_CHECK_GC
  /* If the CAR is not an int, then it will be a pointer, which will
     always be four-byte aligned.  If this cons cell has already been
     placed on the free list, however, its car will probably contain
     a chain pointer to the next cons on the list, which has cleverly
     had all its 0's and 1's inverted.  This allows for a quick
     check to make sure we're not freeing something already freed.

     NOTE: This check may not be necessary.  Freeing an object sets its
     type to lrecord_type_free, which will trip up the XCONS() above -- as
     well as a check in FREE_FIXED_TYPE(). */
  if (POINTER_TYPE_P (XTYPE (cons_car (ptr))))
    ASSERT_VALID_POINTER (XPNTR (cons_car (ptr)));
#endif /* ERROR_CHECK_GC */

  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (cons, Lisp_Cons, ptr);
}

/* explicitly free a list.  You **must make sure** that you have
   created all the cons cells that make up this list and that there
   are no pointers to any of these cons cells anywhere else.  If there
   are, you will lose. */

void
free_list (Lisp_Object list)
{
  Lisp_Object rest, next;

  for (rest = list; !NILP (rest); rest = next)
    {
      next = XCDR (rest);
      free_cons (rest);
    }
}

/* explicitly free an alist.  You **must make sure** that you have
   created all the cons cells that make up this alist and that there
   are no pointers to any of these cons cells anywhere else.  If there
   are, you will lose. */

void
free_alist (Lisp_Object alist)
{
  Lisp_Object rest, next;

  for (rest = alist; !NILP (rest); rest = next)
    {
      next = XCDR (rest);
      free_cons (XCAR (rest));
      free_cons (rest);
    }
}

static void
sweep_compiled_functions (void)
{
#define UNMARK_compiled_function(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_compiled_function(ptr) \
  if (ptr->args_in_array) xfree (ptr->args)

  SWEEP_FIXED_TYPE_BLOCK (compiled_function, Lisp_Compiled_Function);
}

static void
sweep_floats (void)
{
#define UNMARK_float(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_float(ptr)

  SWEEP_FIXED_TYPE_BLOCK (float, Lisp_Float);
}

static void
sweep_symbols (void)
{
#define UNMARK_symbol(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_symbol(ptr)

  SWEEP_FIXED_TYPE_BLOCK (symbol, Lisp_Symbol);
}

static void
sweep_extents (void)
{
#define UNMARK_extent(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_extent(ptr)

  SWEEP_FIXED_TYPE_BLOCK (extent, struct extent);
}

static void
sweep_events (void)
{
#define UNMARK_event(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_event(ptr)

  SWEEP_FIXED_TYPE_BLOCK (event, Lisp_Event);
}

#ifdef EVENT_DATA_AS_OBJECTS

static void
sweep_key_data (void)
{
#define UNMARK_key_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_key_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (key_data, Lisp_Key_Data);
}

void
free_key_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (key_data, Lisp_Key_Data, XKEY_DATA (ptr));
}

static void
sweep_button_data (void)
{
#define UNMARK_button_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_button_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (button_data, Lisp_Button_Data);
}

void
free_button_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (button_data, Lisp_Button_Data, XBUTTON_DATA (ptr));
}

static void
sweep_motion_data (void)
{
#define UNMARK_motion_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_motion_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (motion_data, Lisp_Motion_Data);
}

void
free_motion_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (motion_data, Lisp_Motion_Data, XMOTION_DATA (ptr));
}

static void
sweep_process_data (void)
{
#define UNMARK_process_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_process_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (process_data, Lisp_Process_Data);
}

void
free_process_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (process_data, Lisp_Process_Data, XPROCESS_DATA (ptr));
}

static void
sweep_timeout_data (void)
{
#define UNMARK_timeout_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_timeout_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (timeout_data, Lisp_Timeout_Data);
}

void
free_timeout_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (timeout_data, Lisp_Timeout_Data, XTIMEOUT_DATA (ptr));
}

static void
sweep_magic_data (void)
{
#define UNMARK_magic_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_magic_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (magic_data, Lisp_Magic_Data);
}

void
free_magic_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (magic_data, Lisp_Magic_Data, XMAGIC_DATA (ptr));
}

static void
sweep_magic_eval_data (void)
{
#define UNMARK_magic_eval_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_magic_eval_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (magic_eval_data, Lisp_Magic_Eval_Data);
}

void
free_magic_eval_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (magic_eval_data, Lisp_Magic_Eval_Data, XMAGIC_EVAL_DATA (ptr));
}

static void
sweep_eval_data (void)
{
#define UNMARK_eval_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_eval_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (eval_data, Lisp_Eval_Data);
}

void
free_eval_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (eval_data, Lisp_Eval_Data, XEVAL_DATA (ptr));
}

static void
sweep_misc_user_data (void)
{
#define UNMARK_misc_user_data(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_misc_user_data(ptr)

  SWEEP_FIXED_TYPE_BLOCK (misc_user_data, Lisp_Misc_User_Data);
}

void
free_misc_user_data (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (misc_user_data, Lisp_Misc_User_Data, XMISC_USER_DATA (ptr));
}

#endif /* EVENT_DATA_AS_OBJECTS */

static void
sweep_markers (void)
{
#define UNMARK_marker(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_marker(ptr)					\
  do { Lisp_Object tem;							\
       tem = wrap_marker (ptr);						\
       unchain_marker (tem);						\
     } while (0)

  SWEEP_FIXED_TYPE_BLOCK (marker, Lisp_Marker);
}

/* Explicitly free a marker.  */
void
free_marker (Lisp_Object ptr)
{
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (marker, Lisp_Marker, XMARKER (ptr));
}


#if defined (MULE) && defined (VERIFY_STRING_CHARS_INTEGRITY)

static void
verify_string_chars_integrity (void)
{
  struct string_chars_block *sb;

  /* Scan each existing string block sequentially, string by string.  */
  for (sb = first_string_chars_block; sb; sb = sb->next)
    {
      int pos = 0;
      /* POS is the index of the next string in the block.  */
      while (pos < sb->pos)
        {
          struct string_chars *s_chars =
            (struct string_chars *) &(sb->string_chars[pos]);
          Lisp_String *string;
	  int size;
	  int fullsize;

	  /* If the string_chars struct is marked as free (i.e. the
	     STRING pointer is NULL) then this is an unused chunk of
	     string storage. (See below.) */

	  if (STRING_CHARS_FREE_P (s_chars))
	    {
	      fullsize = ((struct unused_string_chars *) s_chars)->fullsize;
	      pos += fullsize;
	      continue;
            }

          string = s_chars->string;
	  /* Must be 32-bit aligned. */
	  assert ((((int) string) & 3) == 0);

          size = string->size_;
          fullsize = STRING_FULLSIZE (size);

          assert (!BIG_STRING_FULLSIZE_P (fullsize));
	  assert (string->data_ == s_chars->chars);
	  pos += fullsize;
        }
      assert (pos == sb->pos);
    }
}

#endif /* defined (MULE) && defined (VERIFY_STRING_CHARS_INTEGRITY) */

/* Compactify string chars, relocating the reference to each --
   free any empty string_chars_block we see. */
static void
compact_string_chars (void)
{
  struct string_chars_block *to_sb = first_string_chars_block;
  int to_pos = 0;
  struct string_chars_block *from_sb;

  /* Scan each existing string block sequentially, string by string.  */
  for (from_sb = first_string_chars_block; from_sb; from_sb = from_sb->next)
    {
      int from_pos = 0;
      /* FROM_POS is the index of the next string in the block.  */
      while (from_pos < from_sb->pos)
        {
          struct string_chars *from_s_chars =
            (struct string_chars *) &(from_sb->string_chars[from_pos]);
          struct string_chars *to_s_chars;
          Lisp_String *string;
	  int size;
	  int fullsize;

	  /* If the string_chars struct is marked as free (i.e. the
	     STRING pointer is NULL) then this is an unused chunk of
	     string storage.  This happens under Mule when a string's
	     size changes in such a way that its fullsize changes.
	     (Strings can change size because a different-length
	     character can be substituted for another character.)
	     In this case, after the bogus string pointer is the
	     "fullsize" of this entry, i.e. how many bytes to skip. */

	  if (STRING_CHARS_FREE_P (from_s_chars))
	    {
	      fullsize = ((struct unused_string_chars *) from_s_chars)->fullsize;
	      from_pos += fullsize;
	      continue;
            }

          string = from_s_chars->string;
	  gc_checking_assert (!(LRECORD_FREE_P (string)));

          size = string->size_;
          fullsize = STRING_FULLSIZE (size);

          gc_checking_assert (! BIG_STRING_FULLSIZE_P (fullsize));

          /* Just skip it if it isn't marked.  */
	  if (! MARKED_RECORD_HEADER_P (&(string->u.lheader)))
            {
              from_pos += fullsize;
              continue;
            }

          /* If it won't fit in what's left of TO_SB, close TO_SB out
             and go on to the next string_chars_block.  We know that TO_SB
             cannot advance past FROM_SB here since FROM_SB is large enough
             to currently contain this string. */
          if ((to_pos + fullsize) > countof (to_sb->string_chars))
            {
              to_sb->pos = to_pos;
              to_sb = to_sb->next;
              to_pos = 0;
            }

          /* Compute new address of this string
             and update TO_POS for the space being used.  */
          to_s_chars = (struct string_chars *) &(to_sb->string_chars[to_pos]);

          /* Copy the string_chars to the new place.  */
          if (from_s_chars != to_s_chars)
            memmove (to_s_chars, from_s_chars, fullsize);

          /* Relocate FROM_S_CHARS's reference */
          set_lispstringp_data (string, &(to_s_chars->chars[0]));

          from_pos += fullsize;
          to_pos += fullsize;
        }
    }

  /* Set current to the last string chars block still used and
     free any that follow. */
  {
    struct string_chars_block *victim;

    for (victim = to_sb->next; victim; )
      {
	struct string_chars_block *next = victim->next;
	xfree (victim);
	victim = next;
      }

    current_string_chars_block = to_sb;
    current_string_chars_block->pos = to_pos;
    current_string_chars_block->next = 0;
  }
}

#if 1 /* Hack to debug missing purecopy's */
static int debug_string_purity;

static void
debug_string_purity_print (Lisp_Object p)
{
  Charcount i;
  Charcount s = string_char_length (p);
  stderr_out ("\"");
  for (i = 0; i < s; i++)
  {
    Ichar ch = string_ichar (p, i);
    if (ch < 32 || ch >= 126)
      stderr_out ("\\%03o", ch);
    else if (ch == '\\' || ch == '\"')
      stderr_out ("\\%c", ch);
    else
      stderr_out ("%c", ch);
  }
  stderr_out ("\"\n");
}
#endif /* 1 */


static void
sweep_strings (void)
{
  int num_small_used = 0;
  Bytecount num_small_bytes = 0, num_bytes = 0;
  int debug = debug_string_purity;

#define UNMARK_string(ptr) do {				\
    Lisp_String *p = (ptr);				\
    Bytecount size = p->size_;				\
    UNMARK_RECORD_HEADER (&(p->u.lheader));		\
    num_bytes += size;					\
    if (!BIG_STRING_SIZE_P (size))			\
      {							\
	num_small_bytes += size;			\
        num_small_used++;				\
      }							\
    if (debug)						\
      debug_string_purity_print (wrap_string (p));	\
  } while (0)
#define ADDITIONAL_FREE_string(ptr) do {	\
    Bytecount size = ptr->size_;		\
    if (BIG_STRING_SIZE_P (size))		\
      xfree (ptr->data_);			\
  } while (0)

  SWEEP_FIXED_TYPE_BLOCK_1 (string, Lisp_String, u.lheader);

  gc_count_num_short_string_in_use = num_small_used;
  gc_count_string_total_size = num_bytes;
  gc_count_short_string_total_size = num_small_bytes;
}


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

static void
gc_sweep (void)
{
  /* Free all unmarked records.  Do this at the very beginning,
     before anything else, so that the finalize methods can safely
     examine items in the objects.  sweep_lcrecords_1() makes
     sure to call all the finalize methods *before* freeing anything,
     to complete the safety. */
  {
    int ignored;
    sweep_lcrecords_1 (&all_lcrecords, &ignored);
  }

  compact_string_chars ();

  /* Finalize methods below (called through the ADDITIONAL_FREE_foo
     macros) must be *extremely* careful to make sure they're not
     referencing freed objects.  The only two existing finalize
     methods (for strings and markers) pass muster -- the string
     finalizer doesn't look at anything but its own specially-
     created block, and the marker finalizer only looks at live
     buffers (which will never be freed) and at the markers before
     and after it in the chain (which, by induction, will never be
     freed because if so, they would have already removed themselves
     from the chain). */

  /* Put all unmarked strings on free list, free'ing the string chars
     of large unmarked strings */
  sweep_strings ();

  /* Put all unmarked conses on free list */
  sweep_conses ();

  /* Free all unmarked compiled-function objects */
  sweep_compiled_functions ();

  /* Put all unmarked floats on free list */
  sweep_floats ();

  /* Put all unmarked symbols on free list */
  sweep_symbols ();

  /* Put all unmarked extents on free list */
  sweep_extents ();

  /* Put all unmarked markers on free list.
     Dechain each one first from the buffer into which it points. */
  sweep_markers ();

  sweep_events ();

#ifdef EVENT_DATA_AS_OBJECTS
  sweep_key_data ();
  sweep_button_data ();
  sweep_motion_data ();
  sweep_process_data ();
  sweep_timeout_data ();
  sweep_magic_data ();
  sweep_magic_eval_data ();
  sweep_eval_data ();
  sweep_misc_user_data ();
#endif /* EVENT_DATA_AS_OBJECTS */

#ifdef PDUMP
  pdump_objects_unmark ();
#endif
}

/* Clearing for disksave. */

void
disksave_object_finalization (void)
{
  /* It's important that certain information from the environment not get
     dumped with the executable (pathnames, environment variables, etc.).
     To make it easier to tell when this has happened with strings(1) we
     clear some known-to-be-garbage blocks of memory, so that leftover
     results of old evaluation don't look like potential problems.
     But first we set some notable variables to nil and do one more GC,
     to turn those strings into garbage.
  */

  /* Yeah, this list is pretty ad-hoc... */
  Vprocess_environment = Qnil;
  env_initted = 0;
  Vexec_directory = Qnil;
  Vdata_directory = Qnil;
  Vsite_directory = Qnil;
  Vdoc_directory = Qnil;
  Vexec_path = Qnil;
  Vload_path = Qnil;
  /* Vdump_load_path = Qnil; */
  /* Release hash tables for locate_file */
  Flocate_file_clear_hashing (Qt);
  uncache_home_directory ();
  zero_out_command_line_status_vars ();
  clear_default_devices ();

#if defined(LOADHIST) && !(defined(LOADHIST_DUMPED) || \
			   defined(LOADHIST_BUILTIN))
  Vload_history = Qnil;
#endif
  Vshell_file_name = Qnil;

  garbage_collect_1 ();

  /* Run the disksave finalization methods of all live objects. */
  disksave_object_finalization_1 ();

  /* Zero out the uninitialized (really, unused) part of the containers
     for the live strings. */
  {
    struct string_chars_block *scb;
    for (scb = first_string_chars_block; scb; scb = scb->next)
      {
	int count = sizeof (scb->string_chars) - scb->pos;

	assert (count >= 0 && count < STRING_CHARS_BLOCK_SIZE);
	if (count != 0)
	  {
	    /* from the block's fill ptr to the end */
	    memset ((scb->string_chars + scb->pos), 0, count);
	  }
      }
  }

  /* There, that ought to be enough... */

}


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


void
garbage_collect_1 (void)
{
#if MAX_SAVE_STACK > 0
  char stack_top_variable;
  extern char *stack_bottom;
#endif
  struct frame *f;
  int speccount;
  int cursor_changed;
  Lisp_Object pre_gc_cursor;
  struct gcpro gcpro1;
  PROFILE_DECLARE ();

  assert (!in_display || gc_currently_forbidden);

  if (gc_in_progress
      || gc_currently_forbidden
      || in_display
      || preparing_for_armageddon)
    return;

  PROFILE_RECORD_ENTERING_SECTION (QSin_garbage_collection);

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

  pre_gc_cursor = Qnil;
  cursor_changed = 0;

  GCPRO1 (pre_gc_cursor);

  /* Very important to prevent GC during any of the following
     stuff that might run Lisp code; otherwise, we'll likely
     have infinite GC recursion. */
  speccount = begin_gc_forbidden ();

  need_to_signal_post_gc = 0;
  recompute_funcall_allocation_flag ();

  if (!gc_hooks_inhibited)
    run_hook_trapping_problems
      (Qgarbage_collecting, Qpre_gc_hook,
       INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION);

  /* Now show the GC cursor/message. */
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

  /***** Now we actually start the garbage collection. */

  gc_in_progress = 1;
  inhibit_non_essential_printing_operations = 1;

  gc_generation_number[0]++;

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

  /* Mark all the special slots that serve as the roots of accessibility. */

#ifdef USE_KKCC
  /* initialize kkcc stack */
  kkcc_gc_stack_init();
#define mark_object kkcc_gc_stack_push_lisp_object
#endif /* USE_KKCC */

  { /* staticpro() */
    Lisp_Object **p = Dynarr_begin (staticpros);
    Elemcount count;
    for (count = Dynarr_length (staticpros); count; count--)
      mark_object (**p++);
  }

  { /* staticpro_nodump() */
    Lisp_Object **p = Dynarr_begin (staticpros_nodump);
    Elemcount count;
    for (count = Dynarr_length (staticpros_nodump); count; count--)
      mark_object (**p++);
  }

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
    struct catchtag *catch;
    for (catch = catchlist; catch; catch = catch->next)
      {
	mark_object (catch->tag);
	mark_object (catch->val);
	mark_object (catch->actual_tag);
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

  /* OK, now do the after-mark stuff.  This is for things that
     are only marked when something else is marked (e.g. weak hash tables).
     There may be complex dependencies between such objects -- e.g.
     a weak hash table might be unmarked, but after processing a later
     weak hash table, the former one might get marked.  So we have to
     iterate until nothing more gets marked. */
#ifdef USE_KKCC
  kkcc_marking ();
#endif /* USE_KKCC */
  init_marking_ephemerons ();
  while (finish_marking_weak_hash_tables () > 0 ||
	 finish_marking_weak_lists       () > 0 ||
	 continue_marking_ephemerons     () > 0)
    ;

#ifdef USE_KKCC
  kkcc_marking ();
#endif /* USE_KKCC */

  /* At this point, we know which objects need to be finalized: we
     still need to resurrect them */

  while (finish_marking_ephemerons       () > 0 ||
	 finish_marking_weak_lists       () > 0 ||
	 finish_marking_weak_hash_tables () > 0)
    ;

#ifdef USE_KKCC
  kkcc_marking ();
  kkcc_gc_stack_free ();
#undef mark_object
#endif /* USE_KKCC */

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

  gc_sweep ();

  consing_since_gc = 0;
#ifndef DEBUG_XEMACS
  /* Allow you to set it really fucking low if you really want ... */
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;
#endif
  recompute_need_to_garbage_collect ();

  inhibit_non_essential_printing_operations = 0;
  gc_in_progress = 0;

  run_post_gc_actions ();

  /******* End of garbage collection ********/

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

  /* now stop inhibiting GC */
  unbind_to (speccount);

  if (!breathing_space)
    {
      breathing_space = malloc (4096 - MALLOC_OVERHEAD);
    }

  UNGCPRO;

  need_to_signal_post_gc = 1;
  funcall_allocation_flag = 1;

  PROFILE_RECORD_EXITING_SECTION (QSin_garbage_collection);

  return;
}

/* Debugging aids.  */

static Lisp_Object
gc_plist_hack (const Char_ASCII *name, int value, Lisp_Object tail)
{
  /* C doesn't have local functions (or closures, or GC, or readable syntax,
     or portable numeric datatypes, or bit-vectors, or characters, or
     arrays, or exceptions, or ...) */
  return cons3 (intern (name), make_int (value), tail);
}

#define HACK_O_MATIC(type, name, pl) do {				\
  int s = 0;								\
  struct type##_block *x = current_##type##_block;			\
  while (x) { s += sizeof (*x) + MALLOC_OVERHEAD; x = x->prev; }	\
  (pl) = gc_plist_hack ((name), s, (pl));				\
} while (0)

DEFUN ("garbage-collect", Fgarbage_collect, 0, 0, "", /*
Reclaim storage for Lisp objects no longer needed.
Return info on amount of space in use:
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS
  PLIST)
  where `PLIST' is a list of alternating keyword/value pairs providing
  more detailed information.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
*/
       ())
{
  Lisp_Object pl = Qnil;
  int i;
  int gc_count_vector_total_size = 0;
  garbage_collect_1 ();

  for (i = 0; i < lrecord_type_count; i++)
    {
      if (lcrecord_stats[i].bytes_in_use != 0
          || lcrecord_stats[i].bytes_freed != 0
	  || lcrecord_stats[i].instances_on_free_list != 0)
        {
          char buf [255];
          const char *name = lrecord_implementations_table[i]->name;
	  int len = strlen (name);
	  /* save this for the FSFmacs-compatible part of the summary */
	  if (i == lrecord_type_vector)
	    gc_count_vector_total_size =
	      lcrecord_stats[i].bytes_in_use + lcrecord_stats[i].bytes_freed;

          sprintf (buf, "%s-storage", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].bytes_in_use, pl);
	  /* Okay, simple pluralization check for `symbol-value-varalias' */
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-freed", name);
	  else
	    sprintf (buf, "%ss-freed", name);
          if (lcrecord_stats[i].instances_freed != 0)
            pl = gc_plist_hack (buf, lcrecord_stats[i].instances_freed, pl);
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-on-free-list", name);
	  else
	    sprintf (buf, "%ss-on-free-list", name);
          if (lcrecord_stats[i].instances_on_free_list != 0)
            pl = gc_plist_hack (buf, lcrecord_stats[i].instances_on_free_list,
				pl);
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-used", name);
	  else
	    sprintf (buf, "%ss-used", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].instances_in_use, pl);
        }
    }

  HACK_O_MATIC (extent, "extent-storage", pl);
  pl = gc_plist_hack ("extents-free", gc_count_num_extent_freelist, pl);
  pl = gc_plist_hack ("extents-used", gc_count_num_extent_in_use, pl);
  HACK_O_MATIC (event, "event-storage", pl);
  pl = gc_plist_hack ("events-free", gc_count_num_event_freelist, pl);
  pl = gc_plist_hack ("events-used", gc_count_num_event_in_use, pl);
  HACK_O_MATIC (marker, "marker-storage", pl);
  pl = gc_plist_hack ("markers-free", gc_count_num_marker_freelist, pl);
  pl = gc_plist_hack ("markers-used", gc_count_num_marker_in_use, pl);
  HACK_O_MATIC (float, "float-storage", pl);
  pl = gc_plist_hack ("floats-free", gc_count_num_float_freelist, pl);
  pl = gc_plist_hack ("floats-used", gc_count_num_float_in_use, pl);
  HACK_O_MATIC (string, "string-header-storage", pl);
  pl = gc_plist_hack ("long-strings-total-length",
                      gc_count_string_total_size
		      - gc_count_short_string_total_size, pl);
  HACK_O_MATIC (string_chars, "short-string-storage", pl);
  pl = gc_plist_hack ("short-strings-total-length",
                      gc_count_short_string_total_size, pl);
  pl = gc_plist_hack ("strings-free", gc_count_num_string_freelist, pl);
  pl = gc_plist_hack ("long-strings-used",
                      gc_count_num_string_in_use
		      - gc_count_num_short_string_in_use, pl);
  pl = gc_plist_hack ("short-strings-used",
                      gc_count_num_short_string_in_use, pl);

  HACK_O_MATIC (compiled_function, "compiled-function-storage", pl);
  pl = gc_plist_hack ("compiled-functions-free",
		      gc_count_num_compiled_function_freelist, pl);
  pl = gc_plist_hack ("compiled-functions-used",
		      gc_count_num_compiled_function_in_use, pl);

  HACK_O_MATIC (symbol, "symbol-storage", pl);
  pl = gc_plist_hack ("symbols-free", gc_count_num_symbol_freelist, pl);
  pl = gc_plist_hack ("symbols-used", gc_count_num_symbol_in_use, pl);

  HACK_O_MATIC (cons, "cons-storage", pl);
  pl = gc_plist_hack ("conses-free", gc_count_num_cons_freelist, pl);
  pl = gc_plist_hack ("conses-used", gc_count_num_cons_in_use, pl);

  /* The things we do for backwards-compatibility */
  return
    list6 (Fcons (make_int (gc_count_num_cons_in_use),
		  make_int (gc_count_num_cons_freelist)),
	   Fcons (make_int (gc_count_num_symbol_in_use),
		  make_int (gc_count_num_symbol_freelist)),
	   Fcons (make_int (gc_count_num_marker_in_use),
		  make_int (gc_count_num_marker_freelist)),
	   make_int (gc_count_string_total_size),
	   make_int (gc_count_vector_total_size),
	   pl);
}
#undef HACK_O_MATIC

DEFUN ("consing-since-gc", Fconsing_since_gc, 0, 0, "", /*
Return the number of bytes consed since the last garbage collection.
\"Consed\" is a misnomer in that this actually counts allocation
of all different kinds of objects, not just conses.

If this value exceeds `gc-cons-threshold', a garbage collection happens.
*/
       ())
{
  return make_int (consing_since_gc);
}

#if 0
DEFUN ("memory-limit", Fmemory_limit, 0, 0, 0, /*
Return the address of the last byte XEmacs has allocated, divided by 1024.
This may be helpful in debugging XEmacs's memory usage.
The value is divided by 1024 to make sure it will fit in a lisp integer.
*/
       ())
{
  return make_int ((EMACS_INT) sbrk (0) / 1024);
}
#endif

DEFUN ("memory-usage", Fmemory_usage, 0, 0, 0, /*
Return the total number of bytes used by the data segment in XEmacs.
This may be helpful in debugging XEmacs's memory usage.
*/
       ())
{
  return make_int (total_data_usage ());
}

void
recompute_funcall_allocation_flag (void)
{
  funcall_allocation_flag =
    need_to_garbage_collect ||
    need_to_check_c_alloca ||
    need_to_signal_post_gc;
}

/* True if it's time to garbage collect now. */
static void
recompute_need_to_garbage_collect (void)
{
  if (always_gc)
    need_to_garbage_collect = 1;
  else
    need_to_garbage_collect =
      (consing_since_gc > gc_cons_threshold
#if 0 /* #### implement this better */
       &&
       (100 * consing_since_gc) / total_data_usage () >=
       gc_cons_percentage
#endif /* 0 */
       );
  recompute_funcall_allocation_flag ();
}


int
object_dead_p (Lisp_Object obj)
{
  return ((BUFFERP  (obj) && !BUFFER_LIVE_P  (XBUFFER  (obj))) ||
	  (FRAMEP   (obj) && !FRAME_LIVE_P   (XFRAME   (obj))) ||
	  (WINDOWP  (obj) && !WINDOW_LIVE_P  (XWINDOW  (obj))) ||
	  (DEVICEP  (obj) && !DEVICE_LIVE_P  (XDEVICE  (obj))) ||
	  (CONSOLEP (obj) && !CONSOLE_LIVE_P (XCONSOLE (obj))) ||
	  (EVENTP   (obj) && !EVENT_LIVE_P   (XEVENT   (obj))) ||
	  (EXTENTP  (obj) && !EXTENT_LIVE_P  (XEXTENT  (obj))));
}

#ifdef MEMORY_USAGE_STATS

/* Attempt to determine the actual amount of space that is used for
   the block allocated starting at PTR, supposedly of size "CLAIMED_SIZE".

   It seems that the following holds:

   1. When using the old allocator (malloc.c):

      -- blocks are always allocated in chunks of powers of two.  For
	 each block, there is an overhead of 8 bytes if rcheck is not
	 defined, 20 bytes if it is defined.  In other words, a
	 one-byte allocation needs 8 bytes of overhead for a total of
	 9 bytes, and needs to have 16 bytes of memory chunked out for
	 it.

   2. When using the new allocator (gmalloc.c):

      -- blocks are always allocated in chunks of powers of two up
         to 4096 bytes.  Larger blocks are allocated in chunks of
	 an integral multiple of 4096 bytes.  The minimum block
         size is 2*sizeof (void *), or 16 bytes if SUNOS_LOCALTIME_BUG
	 is defined.  There is no per-block overhead, but there
	 is an overhead of 3*sizeof (size_t) for each 4096 bytes
	 allocated.

    3. When using the system malloc, anything goes, but they are
       generally slower and more space-efficient than the GNU
       allocators.  One possibly reasonable assumption to make
       for want of better data is that sizeof (void *), or maybe
       2 * sizeof (void *), is required as overhead and that
       blocks are allocated in the minimum required size except
       that some minimum block size is imposed (e.g. 16 bytes). */

Bytecount
malloced_storage_size (void *ptr, Bytecount claimed_size,
		       struct overhead_stats *stats)
{
  Bytecount orig_claimed_size = claimed_size;

#ifdef GNU_MALLOC
  if (claimed_size < (Bytecount) (2 * sizeof (void *)))
    claimed_size = 2 * sizeof (void *);
# ifdef SUNOS_LOCALTIME_BUG
  if (claimed_size < 16)
    claimed_size = 16;
# endif
  if (claimed_size < 4096)
    {
      int log = 1;

      /* compute the log base two, more or less, then use it to compute
	 the block size needed. */
      claimed_size--;
      /* It's big, it's heavy, it's wood! */
      while ((claimed_size /= 2) != 0)
	++log;
      claimed_size = 1;
      /* It's better than bad, it's good! */
      while (log > 0)
        {
	  claimed_size *= 2;
          log--;
        }
      /* We have to come up with some average about the amount of
	 blocks used. */
      if ((Bytecount) (rand () & 4095) < claimed_size)
	claimed_size += 3 * sizeof (void *);
    }
  else
    {
      claimed_size += 4095;
      claimed_size &= ~4095;
      claimed_size += (claimed_size / 4096) * 3 * sizeof (size_t);
    }

#elif defined (SYSTEM_MALLOC)

  if (claimed_size < 16)
    claimed_size = 16;
  claimed_size += 2 * sizeof (void *);

#else /* old GNU allocator */

# ifdef rcheck /* #### may not be defined here */
  claimed_size += 20;
# else
  claimed_size += 8;
# endif
  {
    int log = 1;

    /* compute the log base two, more or less, then use it to compute
       the block size needed. */
    claimed_size--;
    /* It's big, it's heavy, it's wood! */
    while ((claimed_size /= 2) != 0)
      ++log;
    claimed_size = 1;
    /* It's better than bad, it's good! */
    while (log > 0)
      {
	claimed_size *= 2;
        log--;
      }
  }

#endif /* old GNU allocator */

  if (stats)
    {
      stats->was_requested += orig_claimed_size;
      stats->malloc_overhead += claimed_size - orig_claimed_size;
    }
  return claimed_size;
}

Bytecount
fixed_type_block_overhead (Bytecount size)
{
  Bytecount per_block = TYPE_ALLOC_SIZE (cons, unsigned char);
  Bytecount overhead = 0;
  Bytecount storage_size = malloced_storage_size (0, per_block, 0);
  while (size >= per_block)
    {
      size -= per_block;
      overhead += sizeof (void *) + per_block - storage_size;
    }
  if (rand () % per_block < size)
    overhead += sizeof (void *) + per_block - storage_size;
  return overhead;
}

#endif /* MEMORY_USAGE_STATS */


/* Initialization */
static void
common_init_alloc_early (void)
{
#ifndef Qzero
  Qzero = make_int (0);	/* Only used if Lisp_Object is a union type */
#endif

#ifndef Qnull_pointer
  /* C guarantees that Qnull_pointer will be initialized to all 0 bits,
     so the following is actually a no-op.  */
  Qnull_pointer = wrap_pointer_1 (0);
#endif

  gc_generation_number[0] = 0;
  breathing_space = 0;
  Vgc_message = Qzero;
  all_lcrecords = 0;
  ignore_malloc_warnings = 1;
#ifdef DOUG_LEA_MALLOC
  mallopt (M_TRIM_THRESHOLD, 128*1024); /* trim threshold */
  mallopt (M_MMAP_THRESHOLD, 64*1024); /* mmap threshold */
#if 0 /* Moved to emacs.c */
  mallopt (M_MMAP_MAX, 64); /* max. number of mmap'ed areas */
#endif
#endif
  init_string_alloc ();
  init_string_chars_alloc ();
  init_cons_alloc ();
  init_symbol_alloc ();
  init_compiled_function_alloc ();
  init_float_alloc ();
  init_marker_alloc ();
  init_extent_alloc ();
  init_event_alloc ();
#ifdef EVENT_DATA_AS_OBJECTS
  init_key_data_alloc ();
  init_button_data_alloc ();
  init_motion_data_alloc ();
  init_process_data_alloc ();
  init_timeout_data_alloc ();
  init_magic_data_alloc ();
  init_magic_eval_data_alloc ();
  init_eval_data_alloc ();
  init_misc_user_data_alloc ();
#endif /* EVENT_DATA_AS_OBJECTS */

  ignore_malloc_warnings = 0;

  if (staticpros_nodump)
    Dynarr_free (staticpros_nodump);
  staticpros_nodump = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_resize (staticpros_nodump, 100); /* merely a small optimization */
#ifdef DEBUG_XEMACS
  if (staticpro_nodump_names)
    Dynarr_free (staticpro_nodump_names);
  staticpro_nodump_names = Dynarr_new2 (char_ptr_dynarr, char *);
  Dynarr_resize (staticpro_nodump_names, 100); /* ditto */
#endif

  consing_since_gc = 0;
  need_to_garbage_collect = always_gc;
  need_to_check_c_alloca = 0;
  funcall_allocation_flag = 0;
  funcall_alloca_count = 0;

#if 1
  gc_cons_threshold = 500000; /* XEmacs change */
#else
  gc_cons_threshold = 15000; /* debugging */
#endif
  gc_cons_percentage = 0; /* #### 20; Don't have an accurate measure of
			     memory usage on Windows; not verified on other
			     systems */
  lrecord_uid_counter = 259;
  debug_string_purity = 0;

  gc_currently_forbidden = 0;
  gc_hooks_inhibited = 0;

#ifdef ERROR_CHECK_TYPES
  ERROR_ME.really_unlikely_name_to_have_accidentally_in_a_non_errb_structure =
    666;
  ERROR_ME_NOT.
    really_unlikely_name_to_have_accidentally_in_a_non_errb_structure = 42;
  ERROR_ME_WARN.
    really_unlikely_name_to_have_accidentally_in_a_non_errb_structure =
      3333632;
  ERROR_ME_DEBUG_WARN.
    really_unlikely_name_to_have_accidentally_in_a_non_errb_structure =
      8675309;
#endif /* ERROR_CHECK_TYPES */
}

static void
init_lcrecord_lists (void)
{
  int i;

  for (i = 0; i < countof (lrecord_implementations_table); i++)
    {
      all_lcrecord_lists[i] = Qzero; /* Qnil not yet set */
      staticpro_nodump (&all_lcrecord_lists[i]);
    }
}

void
init_alloc_early (void)
{
#if defined (__cplusplus) && defined (ERROR_CHECK_GC)
  static struct gcpro initial_gcpro;

  initial_gcpro.next = 0;
  initial_gcpro.var = &Qnil;
  initial_gcpro.nvars = 1;
  gcprolist = &initial_gcpro;
#else
  gcprolist = 0;
#endif /* defined (__cplusplus) && defined (ERROR_CHECK_GC) */
}

void
reinit_alloc_early (void)
{
  common_init_alloc_early ();
  init_lcrecord_lists ();
}

void
init_alloc_once_early (void)
{
  common_init_alloc_early ();

  {
    int i;
    for (i = 0; i < countof (lrecord_implementations_table); i++)
      lrecord_implementations_table[i] = 0;
  }

  INIT_LRECORD_IMPLEMENTATION (cons);
  INIT_LRECORD_IMPLEMENTATION (vector);
  INIT_LRECORD_IMPLEMENTATION (string);
  INIT_LRECORD_IMPLEMENTATION (lcrecord_list);
  INIT_LRECORD_IMPLEMENTATION (free);

  staticpros = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_resize (staticpros, 1410); /* merely a small optimization */
  dump_add_root_struct_ptr (&staticpros, &staticpros_description);
#ifdef DEBUG_XEMACS
  staticpro_names = Dynarr_new2 (char_ptr_dynarr, char *);
  Dynarr_resize (staticpro_names, 1410); /* merely a small optimization */
  dump_add_root_struct_ptr (&staticpro_names, &staticpro_names_description);
#endif

  init_lcrecord_lists ();
}

void
syms_of_alloc (void)
{
  DEFSYMBOL (Qpre_gc_hook);
  DEFSYMBOL (Qpost_gc_hook);
  DEFSYMBOL (Qgarbage_collecting);

  DEFSUBR (Fcons);
  DEFSUBR (Flist);
  DEFSUBR (Fvector);
  DEFSUBR (Fbit_vector);
  DEFSUBR (Fmake_byte_code);
  DEFSUBR (Fmake_list);
  DEFSUBR (Fmake_vector);
  DEFSUBR (Fmake_bit_vector);
  DEFSUBR (Fmake_string);
  DEFSUBR (Fstring);
  DEFSUBR (Fmake_symbol);
  DEFSUBR (Fmake_marker);
  DEFSUBR (Fpurecopy);
  DEFSUBR (Fgarbage_collect);
#if 0
  DEFSUBR (Fmemory_limit);
#endif
  DEFSUBR (Fmemory_usage);
  DEFSUBR (Fconsing_since_gc);
}

void
vars_of_alloc (void)
{
  QSin_garbage_collection = build_msg_string ("(in garbage collection)");
  staticpro (&QSin_garbage_collection);

  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold /*
*Number of bytes of consing between garbage collections.
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
 
See also `consing-since-gc'.
*/ );

  DEFVAR_INT ("gc-cons-percentage", &gc_cons_percentage /*
*Percentage of memory allocated between garbage collections.

Garbage collection will happen if this percentage of the total amount of
memory used for data has been allocated since the last garbage collection.
However, it will not happen if less than `gc-cons-threshold' bytes have
been allocated -- this sets an absolute minimum in case very little data
has been allocated or the percentage is set very low.  Set this to 0 to
have garbage collection always happen after `gc-cons-threshold' bytes have
been allocated, regardless of current memory usage.

Garbage collection happens automatically when `eval' or `funcall' are
called.  (Note that `funcall' is called implicitly as part of evaluation.)
By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.

See also `consing-since-gc'.
*/ );

#ifdef DEBUG_XEMACS
  DEFVAR_INT ("debug-allocation", &debug_allocation /*
If non-zero, print out information to stderr about all objects allocated.
See also `debug-allocation-backtrace-length'.
*/ );
  debug_allocation = 0;

  DEFVAR_INT ("debug-allocation-backtrace-length",
	      &debug_allocation_backtrace_length /*
Length (in stack frames) of short backtrace printed out by `debug-allocation'.
*/ );
  debug_allocation_backtrace_length = 2;
#endif

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
}

void
complex_vars_of_alloc (void)
{
  Vgc_pointer_glyph = Fmake_glyph_internal (Qpointer);
}
