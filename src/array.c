/* Support for dynarrs and other types of dynamic arrays.
   Copyright (c) 1994, 1995 Free Software Foundation, Inc.
   Copyright (c) 1993, 1995 Sun Microsystems, Inc.
   Copyright (c) 1995, 1996, 2000, 2002, 2003, 2004, 2005, 2010 Ben Wing.

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

/* Synched up with:  Not in FSF. */

/* Written by Ben Wing, December 1993. */

#include <config.h>
#include "lisp.h"

#include "insdel.h"


/*****************************************************************************/
/*                       "dynarr" a.k.a. dynamic array                       */
/*****************************************************************************/

/*
A "dynamic array" or "dynarr" is a contiguous array of fixed-size elements
where there is no upper limit (except available memory) on the number of
elements in the array.  Because the elements are maintained contiguously,
space is used efficiently (no per-element pointers necessary) and random
access to a particular element is in constant time.  At any one point, the
block of memory that holds the array has an upper limit; if this limit is
exceeded, the memory is realloc()ed into a new array that is twice as big.
Assuming that the time to grow the array is on the order of the new size of
the array block, this scheme has a provably constant amortized time
\(i.e. average time over all additions).

When you add elements or retrieve elements, pointers are used.  Note that
the element itself (of whatever size it is), and not the pointer to it,
is stored in the array; thus you do not have to allocate any heap memory
on your own.  Also, returned pointers are only guaranteed to be valid
until the next operation that changes the length of the array.

This is a container object.  Declare a dynamic array of a specific type
as follows:

  typedef struct
  {
    Dynarr_declare (mytype);
  } mytype_dynarr;

Use the following functions/macros:


  ************* Dynarr creation *************

   void *Dynarr_new(type)
      [MACRO] Create a new dynamic-array object, with each element of the
      specified type.  The return value is cast to (type##_dynarr).
      This requires following the convention that types are declared in
      such a way that this type concatenation works.  In particular, TYPE
      must be a symbol, not an arbitrary C type.  To make dynarrs of
      complex types, a typedef must be declared, e.g.

      typedef unsigned char *unsigned_char_ptr;

      and then you can say

      unsigned_char_ptr_dynarr *dyn = Dynarr_new (unsigned_char_ptr);

   void *Dynarr_new2(dynarr_type, type)
      [MACRO] Create a new dynamic-array object, with each element of the
      specified type.  The array itself is of type DYNARR_TYPE.  This makes
      it possible to create dynarrs over complex types without the need
      to create typedefs, as described above.  Use is as follows:

      ucharptr_dynarr *dyn = Dynarr_new2 (ucharptr_dynarr *, unsigned char *);

   Dynarr_free(d)
      Destroy a dynamic array and the memory allocated to it.

  ************* Dynarr access *************

   type Dynarr_at(d, i)
      [MACRO] Return the element at the specified index.  The index must be
      between 0 and Dynarr_largest(d), inclusive.  With error-checking
      enabled, bounds checking on the index is in the form of asserts() --
      an out-of-bounds index causes an abort.  The element itself is
      returned, not a pointer to it.

   type *Dynarr_atp(d, i)
      [MACRO] Return a pointer to the element at the specified index.
      Restrictions and bounds checking on the index is as for Dynarr_at.
      The pointer may not be valid after an element is added to or
      (conceivably) removed from the array, because this may trigger a
      realloc() performed on the underlying dynarr storage, which may
      involve moving the entire underlying storage to a new location in
      memory.

   type *Dynarr_begin(d)
      [MACRO] Return a pointer to the first element in the dynarr.  See
      Dynarr_atp() for warnings about when the pointer might become invalid.

   type *Dynarr_lastp(d)
      [MACRO] Return a pointer to the last element in the dynarr.  See
      Dynarr_atp() for warnings about when the pointer might become invalid.

   type *Dynarr_past_lastp(d)
      [MACRO] Return a pointer to the beginning of the element just past the
      last one.  WARNING: This may not point to valid memory; however, the
      byte directly before will be pointer will be valid memory.  This macro
      might be useful for various reasons, e.g. as a stopping point in a loop
      (although Dynarr_lastp() could be used just as well) or as a place to
      start writing elements if Dynarr_length() < Dynarr_largest().

  ************* Dynarr length/size retrieval and setting *************

   int Dynarr_length(d)
      [MACRO] Return the number of elements currently in a dynamic array.

   int Dynarr_largest(d)
      [MACRO] Return the maximum value that Dynarr_length(d) would
      ever have returned.  This is used esp. in the redisplay code,
      which reuses dynarrs for performance reasons.

   int Dynarr_max(d)
      [MACRO] Return the maximum number of elements that can fit in the
      dynarr before it needs to be resized.

      Note that Dynarr_length(d) <= Dynarr_largest(d) <= Dynarr_max(d).
   
   Bytecount Dynarr_sizeof(d)
      [MACRO] Return the total size of the elements currently in dynarr
      D.  This 

   Dynarr_set_lengthr(d, len)
      [MACRO] Set the length of D to LEN, which must be between 0 and
      Dynarr_largest(d), inclusive.  With error-checking enabled, an
      assertion failure will result from trying to set the length
      to less than zero or greater than Dynarr_largest(d).  The
      restriction to Dynarr_largest() is to ensure that

   Dynarr_set_length(d, len)
      [MACRO] Set the length of D to LEN, resizing the dynarr as
      necessary to make sure enough space is available.  there are no
      restrictions on LEN other than available memory and that it must
      be at least 0.  Note that

   Dynarr_set_length_and_zero(d, len)
      [MACRO] Like Dynarr_set_length(d, len) but also, if increasing
      the length, zero out the memory between the old and new lengths,
      i.e. starting just past the previous last element and up through
      the new last element.

   Dynarr_incrementr(d)
      [MACRO] Increments the length of D by 1.  Equivalent to
      Dynarr_set_lengthr(d, Dynarr_length(d) + 1).

   Dynarr_increment(d)
      [MACRO] Increments the length of D by 1.  Equivalent to
      Dynarr_set_length(d, Dynarr_length(d) + 1).

   Dynarr_reset(d)
      [MACRO] Reset the length of a dynamic array to 0.

   Dynarr_resize(d, maxval)
      Resize the internal dynarr storage to so that it can hold at least
      MAXVAL elements.  Resizing is done using a geometric series
      (repeatedly multiply the old maximum by a constant, normally 1.5,
      till a large enough size is reached), so this will be efficient
      even if resizing larger by one element at a time.  This is mostly
      an internal function.



  ************* Adding/deleting elements to/from a dynarr *************

   Dynarr_add(d, el)
      [MACRO] Add an element to the end of a dynamic array.  EL is a pointer
      to the element; the element itself is stored in the array, however.
      No function call is performed unless the array needs to be resized.

   Dynarr_add_many(d, base, len)
      [MACRO] Add LEN elements to the end of the dynamic array.  The elements
      should be contiguous in memory, starting at BASE.  If BASE if NULL,
      just make space for the elements; don't actually add them.

   Dynarr_prepend_many(d, base, len)
      [MACRO] Prepend LEN elements to the beginning of the dynamic array.
      The elements should be contiguous in memory, starting at BASE.
      If BASE if NULL, just make space for the elements; don't actually
      add them.

   Dynarr_insert_many(d, base, len, pos)
      Insert LEN elements to the dynamic array starting at position
      POS.  The elements should be contiguous in memory, starting at BASE.
      If BASE if NULL, just make space for the elements; don't actually
      add them.

   type Dynarr_pop(d)
      [MACRO] Pop the last element off the dynarr and return it.

   Dynarr_delete(d, i)
      [MACRO] Delete an element from the dynamic array at position I.

   Dynarr_delete_many(d, pos, len)
      Delete LEN elements from the dynamic array starting at position
      POS.

   Dynarr_zero_many(d, pos, len)
      Zero out LEN elements in the dynarr D starting at position POS.

   Dynarr_delete_by_pointer(d, p)
      [MACRO] Delete an element from the dynamic array at pointer P,
      which must point within the block of memory that stores the data.
      P should be obtained using Dynarr_atp().

  ************* Dynarr locking *************

   Dynarr_lock(d)
      Lock the dynarr against further locking or writing.  With error-checking
      enabled, any attempts to write into a locked dynarr or re-lock an
      already locked one will cause an assertion failure and abort.

   Dynarr_unlock(d)
      Unlock a locked dynarr, allowing writing into it.

  ************* Dynarr global variables *************

   Dynarr_min_size
      Minimum allowable size for a dynamic array when it is resized.

*/

static const struct memory_description const_Ascbyte_ptr_description_1[] = {
  { XD_ASCII_STRING, 0 },
  { XD_END }
};

const struct sized_memory_description const_Ascbyte_ptr_description = {
  sizeof (const Ascbyte *),
  const_Ascbyte_ptr_description_1
};

static const struct memory_description const_Ascbyte_ptr_dynarr_description_1[] = {
  XD_DYNARR_DESC (const_Ascbyte_ptr_dynarr, &const_Ascbyte_ptr_description),
  { XD_END }
};

const struct sized_memory_description const_Ascbyte_ptr_dynarr_description = {
  sizeof (const_Ascbyte_ptr_dynarr),
  const_Ascbyte_ptr_dynarr_description_1
};


static Elemcount Dynarr_min_size = 8;

static void
Dynarr_realloc (Dynarr *dy, Elemcount new_size)
{
  if (DUMPEDP (dy->base))
    {
      void *new_base = malloc (new_size * Dynarr_elsize (dy));
      memcpy (new_base, dy->base, 
	      (Dynarr_max (dy) < new_size ? Dynarr_max (dy) : new_size) *
	      Dynarr_elsize (dy));
      dy->base = new_base;
    }
  else
    dy->base = xrealloc (dy->base, new_size * Dynarr_elsize (dy));
}

void *
Dynarr_newf (Bytecount elsize)
{
  Dynarr *d = xnew_and_zero (Dynarr);
  d->elsize_ = elsize;

  return d;
}

#ifdef NEW_GC
DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("dynarr", dynarr,
				      0, 0,
				      Dynarr);

static void
Dynarr_lisp_realloc (Dynarr *dy, Elemcount new_size)
{
  void *new_base =
    XPNTR (alloc_sized_lrecord_array (Dynarr_elsize (dy), new_size,
				      dy->lisp_imp));
  if (dy->base)
    memcpy (new_base, dy->base, 
	    (Dynarr_max (dy) < new_size ? Dynarr_max (dy) : new_size) *
	    Dynarr_elsize (dy));
  dy->base = new_base;
}

void *
Dynarr_lisp_newf (Bytecount elsize, 
		  const struct lrecord_implementation *dynarr_imp, 
		  const struct lrecord_implementation *imp)
{
  Dynarr *d = (Dynarr *) XPNTR (alloc_sized_lrecord (sizeof (Dynarr),
                                                     dynarr_imp));
  d->elsize_ = elsize;
  d->lisp_imp = imp;

  return d;
}
#endif /* not NEW_GC */

void
Dynarr_resize (void *d, Elemcount size)
{
  Elemcount newsize;
  double multiplier;
  Dynarr *dy = (Dynarr *) Dynarr_verify (d);

  if (Dynarr_max (dy) <= 8)
    multiplier = 2;
  else
    multiplier = 1.5;

  for (newsize = Dynarr_max (dy); newsize < size;)
    newsize = max (Dynarr_min_size, (Elemcount) (multiplier * newsize));

  /* Don't do anything if the array is already big enough. */
  if (newsize > Dynarr_max (dy))
    {
#ifdef NEW_GC
      if (dy->lisp_imp)
	Dynarr_lisp_realloc (dy, newsize);
      else
	Dynarr_realloc (dy, newsize);
#else /* not NEW_GC */
      Dynarr_realloc (dy, newsize);
#endif /* not NEW_GC */
      dy->max_ = newsize;
    }
}

/* Add a number of contiguous elements to the array starting at POS. */

void
Dynarr_insert_many (void *d, const void *base, Elemcount len, Elemcount pos)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  Elemcount old_len = Dynarr_length (dy);

  /* #### This could conceivably be wrong, if code wants to access stuff
     between len and largest. */
  dynarr_checking_assert (pos >= 0 && pos <= old_len);
  dynarr_checking_assert (len >= 0);
  Dynarr_increase_length (dy, old_len + len);

  if (pos != old_len)
    {
      memmove ((Rawbyte *) dy->base + (pos + len)*Dynarr_elsize (dy),
	       (Rawbyte *) dy->base + pos*Dynarr_elsize (dy),
	       (old_len - pos)*Dynarr_elsize (dy));
    }
  /* Some functions call us with a value of 0 to mean "reserve space but
     don't write into it" */
  if (base)
    memcpy ((Rawbyte *) dy->base + pos*Dynarr_elsize (dy), base,
	    len*Dynarr_elsize (dy));
}

void
Dynarr_delete_many (void *d, Elemcount pos, Elemcount len)
{
  Dynarr *dy = Dynarr_verify_mod (d);

  dynarr_checking_assert (pos >= 0 && len >= 0 &&
			  pos + len <= Dynarr_length (dy));

  memmove ((Rawbyte *) dy->base + pos*Dynarr_elsize (dy),
	   (Rawbyte *) dy->base + (pos + len)*Dynarr_elsize (dy),
	   (Dynarr_length (dy) - pos - len)*Dynarr_elsize (dy));

  Dynarr_set_length_1 (dy, Dynarr_length (dy) - len);
}

void
Dynarr_free (void *d)
{
  Dynarr *dy = (Dynarr *) d;

#ifdef NEW_GC
  if (dy->base && !DUMPEDP (dy->base))
    {
      if (!dy->lisp_imp)
	{
	  xfree (dy->base);
	  dy->base = 0;
	}
    }
  if (!DUMPEDP (dy))
    {
      if (!dy->lisp_imp)
	xfree (dy);
    }
#else /* not NEW_GC */
  if (dy->base && !DUMPEDP (dy->base))
    {
      xfree (dy->base);
      dy->base = 0;
    }
  if(!DUMPEDP (dy))
    xfree (dy);
#endif /* not NEW_GC */
}

#ifdef MEMORY_USAGE_STATS

/* Return memory usage for dynarr D.  The returned value is the total
   amount of bytes actually being used for the dynarr, including all
   overhead.  The extra amount of space in the dynarr that is
   allocated beyond what was requested is returned in DYNARR_OVERHEAD
   in STATS.  The extra amount of space that malloc() allocates beyond
   what was requested of it is returned in MALLOC_OVERHEAD in STATS.
   See the comment above the definition of this structure. */

Bytecount
Dynarr_memory_usage (void *d, struct usage_stats *stats)
{
  Bytecount total = 0;
  Dynarr *dy = (Dynarr *) d;

  /* We have to be a bit tricky here because not all of the
     memory that malloc() will claim as "requested" was actually
     requested. */

  if (dy->base)
    {
      Bytecount malloc_used =
	malloced_storage_size (dy->base, Dynarr_elsize (dy) * Dynarr_max (dy),
			       0);
      /* #### This may or may not be correct.  Some dynarrs would
	 prefer that we use dy->len instead of dy->largest here. */
      Bytecount was_requested = Dynarr_elsize (dy) * Dynarr_largest (dy);
      Bytecount dynarr_overhead =
	Dynarr_elsize (dy) * (Dynarr_max (dy) - Dynarr_largest (dy));

      total += malloc_used;
      stats->was_requested += was_requested;
      stats->dynarr_overhead += dynarr_overhead;
      /* And the remainder must be malloc overhead. */
      stats->malloc_overhead +=
	malloc_used - was_requested - dynarr_overhead;
    }

  total += malloced_storage_size (d, sizeof (*dy), stats);

  return total;
}

#endif /* MEMORY_USAGE_STATS */


/*****************************************************************************/
/*                           stack-like allocation                           */
/*****************************************************************************/

/* Version of malloc() that will be extremely efficient when allocation
   nearly always occurs in LIFO (stack) order.

   #### Perhaps shouldn't be in this file, but where else? */

typedef struct
{
  Dynarr_declare (char_dynarr *);
} char_dynarr_dynarr;

char_dynarr_dynarr *stack_like_free_list;
char_dynarr_dynarr *stack_like_in_use_list;

void *
stack_like_malloc (Bytecount size)
{
  char_dynarr *this_one;
  if (!stack_like_free_list)
    {
      stack_like_free_list = Dynarr_new2 (char_dynarr_dynarr,
					  char_dynarr *);
      stack_like_in_use_list = Dynarr_new2 (char_dynarr_dynarr,
					    char_dynarr *);
    }

  if (Dynarr_length (stack_like_free_list) > 0)
    this_one = Dynarr_pop (stack_like_free_list);
  else
    this_one = Dynarr_new (char);
  Dynarr_add (stack_like_in_use_list, this_one);
  Dynarr_reset (this_one);
  Dynarr_add_many (this_one, 0, size);
  return Dynarr_begin (this_one);
}

void
stack_like_free (void *val)
{
  Elemcount len = Dynarr_length (stack_like_in_use_list);
  assert (len > 0);
  /* The vast majority of times, we will be called in a last-in first-out
     order, and the item at the end of the list will be the one we're
     looking for, so just check for this first and avoid any function
     calls. */
  if (Dynarr_begin (Dynarr_at (stack_like_in_use_list, len - 1)) == val)
    {
      char_dynarr *this_one = Dynarr_pop (stack_like_in_use_list);
      Dynarr_add (stack_like_free_list, this_one);
    }
  else
    {
      /* Find the item and delete it. */
      int i;
      assert (len >= 2);
      for (i = len - 2; i >= 0; i--)
	if (Dynarr_begin (Dynarr_at (stack_like_in_use_list, i)) ==
	    val)
	  {
	    char_dynarr *this_one = Dynarr_at (stack_like_in_use_list, i);
	    Dynarr_add (stack_like_free_list, this_one);
	    Dynarr_delete (stack_like_in_use_list, i);
	    return;
	  }

      ABORT ();
    }
}


/*****************************************************************************/
/*                           Generalized gap array                           */
/*****************************************************************************/

/* A "gap array" is an array that has a "gap" somewhere in the middle of it,
   so that insertions and deletions near the gap -- or in general, highly
   localized insertions and deletions -- are very fast.  Inserting or
   deleting works by first moving the gap to the insertion or deletion
   position and then shortening or lengthening the gap as necessary.  The
   idea comes from the gap used in storing text in a buffer.

   The gap array interface differs in a number of ways from dynarrs (####
   and should be changed so that it works the same as dynarrs):

   (1) There aren't separate type-specific gap array types.  As a result,
       operations like gap_array_at() require that the type be specified as
       one of the arguments.  It is often more convenient to use a macro
       wrapper around this operation.

   (2) The gap array type is itself a stretchy array rather than using a
       separate block of memory to store the array.  This means that certain
       operations (especially insertions) may relocate the the gap array,
       and as a result return a pointer to the (possibly) moved gap array,
       which must be stored back into the location where the gap array
       pointer resides.  This also means that the caller must worry about
       cloning the gap array in the case where it has been dumped, or you
       will get an ABORT() inside of xrealloc().

   (3) Fewer operations are available than for dynarrs, and may have
       different names and/or different calling conventions.

   (4) The mechanism for creating "Lisp-object gap arrays" isn't completely
       developed.  Currently it's only possible to create a gap-array Lisp
       object that wraps Lisp_Object pointers (not Lisp object structures
       directly), and only under NEW_GC.

   (5) Gap arrays have a concept of a "gap array marker" that properly
       tracks insertions and deletions; no such thing exists in dynarrs.
       It exists in gap arrays because it's necessary for their use in
       implementing extent lists.
 */

extern const struct sized_memory_description gap_array_marker_description;

static const struct memory_description gap_array_marker_description_1[] = { 
#ifdef NEW_GC
  { XD_LISP_OBJECT, offsetof (Gap_Array_Marker, next) },
#else /* not NEW_GC */
  { XD_BLOCK_PTR, offsetof (Gap_Array_Marker, next), 1,
    { &gap_array_marker_description } },
#endif /* not NEW_GC */
  { XD_END }
};

#ifdef NEW_GC
DEFINE_NODUMP_INTERNAL_LISP_OBJECT ("gap-array-marker", gap_array_marker,
				    0, gap_array_marker_description_1,
				    struct gap_array_marker);
#else /* not NEW_GC */
const struct sized_memory_description gap_array_marker_description = {
  sizeof (Gap_Array_Marker),
  gap_array_marker_description_1
};
#endif /* not NEW_GC */

static const struct memory_description lispobj_gap_array_description_1[] = {
  XD_GAP_ARRAY_DESC (&lisp_object_description),
  { XD_END }
};

#ifdef NEW_GC

static Bytecount
size_gap_array (Lisp_Object obj)
{
  Gap_Array *ga = XGAP_ARRAY (obj);
  return gap_array_byte_size (ga);
}

DEFINE_DUMPABLE_SIZABLE_INTERNAL_LISP_OBJECT ("gap-array", gap_array,
					      0,
					      lispobj_gap_array_description_1,
					      size_gap_array,
					      struct gap_array);
#else /* not NEW_GC */
const struct sized_memory_description lispobj_gap_array_description = {
  0, lispobj_gap_array_description_1
};
#endif /* (not) NEW_GC */

#ifndef NEW_GC
static Gap_Array_Marker *gap_array_marker_freelist;
#endif /* not NEW_GC */

/* This generalizes the "array with a gap" model used to store buffer
   characters.  This is based on the stuff in insdel.c and should
   probably be merged with it.  This is not extent-specific and should
   perhaps be moved into a separate file. */

/* ------------------------------- */
/*        internal functions       */
/* ------------------------------- */

/* Adjust the gap array markers in the range (FROM, TO].  Parallel to
   adjust_markers() in insdel.c. */

static void
gap_array_adjust_markers (Gap_Array *ga, Memxpos from,
			  Memxpos to, Elemcount amount)
{
  Gap_Array_Marker *m;

  for (m = ga->markers; m; m = m->next)
    m->pos = do_marker_adjustment (m->pos, from, to, amount);
}

static void
gap_array_recompute_derived_values (Gap_Array *ga)
{
  ga->offset_past_gap = ga->elsize * (ga->gap + ga->gapsize);
  ga->els_past_gap = ga->numels - ga->gap;
}

/* Move the gap to array position POS.  Parallel to move_gap() in
   insdel.c but somewhat simplified. */

static void
gap_array_move_gap (Gap_Array *ga, Elemcount pos)
{
  Elemcount gap = ga->gap;
  Elemcount gapsize = ga->gapsize;

  if (pos < gap)
    {
      memmove (GAP_ARRAY_MEMEL_ADDR (ga, pos + gapsize),
	       GAP_ARRAY_MEMEL_ADDR (ga, pos),
	       (gap - pos)*ga->elsize);
      gap_array_adjust_markers (ga, (Memxpos) pos, (Memxpos) gap,
				gapsize);
    }
  else if (pos > gap)
    {
      memmove (GAP_ARRAY_MEMEL_ADDR (ga, gap),
	       GAP_ARRAY_MEMEL_ADDR (ga, gap + gapsize),
	       (pos - gap)*ga->elsize);
      gap_array_adjust_markers (ga, (Memxpos) (gap + gapsize),
				(Memxpos) (pos + gapsize), - gapsize);
    }
  ga->gap = pos;

  gap_array_recompute_derived_values (ga);
}

/* Make the gap INCREMENT characters longer.  Parallel to make_gap() in
   insdel.c.  The gap array may be moved, so assign the return value back
   to the array pointer. */

static Gap_Array *
gap_array_make_gap (Gap_Array *ga, Elemcount increment)
{
  Elemcount real_gap_loc;
  Elemcount old_gap_size;

  /* If we have to get more space, get enough to last a while.  We use
     a geometric progression that saves on realloc space. */
  increment += 100 + ga->numels / 8;

#ifdef NEW_GC
  if (ga->is_lisp)
    ga = (Gap_Array *) mc_realloc (ga,
				   offsetof (Gap_Array, array) +
				   (ga->numels + ga->gapsize + increment) *
				   ga->elsize);
  else
#endif /* not NEW_GC */
    ga = (Gap_Array *) xrealloc (ga,
				 offsetof (Gap_Array, array) +
				 (ga->numels + ga->gapsize + increment) *
				 ga->elsize);
  if (ga == 0)
    memory_full ();

  real_gap_loc = ga->gap;
  old_gap_size = ga->gapsize;

  /* Call the newly allocated space a gap at the end of the whole space.  */
  ga->gap = ga->numels + ga->gapsize;
  ga->gapsize = increment;

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_array_move_gap (ga, real_gap_loc + old_gap_size);

  /* Now combine the two into one large gap.  */
  ga->gapsize += old_gap_size;
  ga->gap = real_gap_loc;

  gap_array_recompute_derived_values (ga);

  return ga;
}

/* ------------------------------- */
/*        external functions       */
/* ------------------------------- */

Bytecount
gap_array_byte_size (Gap_Array *ga)
{
  return offsetof (Gap_Array, array) + (ga->numels + ga->gapsize) * ga->elsize;
}

/* Insert NUMELS elements (pointed to by ELPTR) into the specified
   gap array at POS.  The gap array may be moved, so assign the
   return value back to the array pointer. */

Gap_Array *
gap_array_insert_els (Gap_Array *ga, Elemcount pos, void *elptr,
		      Elemcount numels)
{
  assert (pos >= 0 && pos <= ga->numels);
  if (ga->gapsize < numels)
    ga = gap_array_make_gap (ga, numels - ga->gapsize);
  if (pos != ga->gap)
    gap_array_move_gap (ga, pos);

  memcpy (GAP_ARRAY_MEMEL_ADDR (ga, ga->gap), (char *) elptr,
	  numels*ga->elsize);
  ga->gapsize -= numels;
  ga->gap += numels;
  ga->numels += numels;
  gap_array_recompute_derived_values (ga);
  /* This is the equivalent of insert-before-markers.

     #### Should only happen if marker is "moves forward at insert" type.
     */

  gap_array_adjust_markers (ga, pos - 1, pos, numels);
  return ga;
}

/* Delete NUMELS elements from the specified gap array, starting at FROM. */

void
gap_array_delete_els (Gap_Array *ga, Elemcount from, Elemcount numdel)
{
  Elemcount to = from + numdel;
  Elemcount gapsize = ga->gapsize;

  assert (from >= 0);
  assert (numdel >= 0);
  assert (to <= ga->numels);

  /* Make sure the gap is somewhere in or next to what we are deleting.  */
  if (to < ga->gap)
    gap_array_move_gap (ga, to);
  if (from > ga->gap)
    gap_array_move_gap (ga, from);

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  gap_array_adjust_markers (ga, to + gapsize, to + gapsize,
			    - numdel - gapsize);

  ga->gapsize += numdel;
  ga->numels -= numdel;
  ga->gap = from;
  gap_array_recompute_derived_values (ga);
}

Gap_Array_Marker *
gap_array_make_marker (Gap_Array *ga, Elemcount pos)
{
  Gap_Array_Marker *m;

  assert (pos >= 0 && pos <= ga->numels);
#ifdef NEW_GC
    m = XGAP_ARRAY_MARKER (ALLOC_NORMAL_LISP_OBJECT (gap_array_marker));
#else /* not NEW_GC */
  if (gap_array_marker_freelist)
    {
      m = gap_array_marker_freelist;
      gap_array_marker_freelist = gap_array_marker_freelist->next;
    }
  else
    m = xnew (Gap_Array_Marker);
#endif /* not NEW_GC */

  m->pos = GAP_ARRAY_ARRAY_TO_MEMORY_POS (ga, pos);
  m->next = ga->markers;
  ga->markers = m;
  return m;
}

void
gap_array_delete_marker (Gap_Array *ga, Gap_Array_Marker *m)
{
  Gap_Array_Marker *p, *prev;

  for (prev = 0, p = ga->markers; p && p != m; prev = p, p = p->next)
    ;
  assert (p);
  if (prev)
    prev->next = p->next;
  else
    ga->markers = p->next;
#ifndef NEW_GC
  m->next = gap_array_marker_freelist;
  m->pos = 0xDEADBEEF; /* -559038737 base 10 */
  gap_array_marker_freelist = m;
#endif /* not NEW_GC */
}

#ifndef NEW_GC
void
gap_array_delete_all_markers (Gap_Array *ga)
{
  Gap_Array_Marker *p, *next;

  for (p = ga->markers; p; p = next)
    {
      next = p->next;
      p->next = gap_array_marker_freelist;
      p->pos = 0xDEADBEEF; /* -559038737 as an int */
      gap_array_marker_freelist = p;
    }
}
#endif /* not NEW_GC */

void
gap_array_move_marker (Gap_Array *ga, Gap_Array_Marker *m, Elemcount pos)
{
  assert (pos >= 0 && pos <= ga->numels);
  m->pos = GAP_ARRAY_ARRAY_TO_MEMORY_POS (ga, pos);
}

Gap_Array *
make_gap_array (Elemcount elsize, int USED_IF_NEW_GC (do_lisp))
{
  Gap_Array *ga;
#ifdef NEW_GC
  /* #### I don't quite understand why it's necessary to make all these
     internal objects into Lisp objects under NEW_GC.  It's a pain in the
     ass to code around this.  I'm proceeding on the assumption that it's
     not really necessary to do it after all, and so we only make a Lisp-
     object gap array when the object being held is a Lisp_Object, i.e. a
     pointer to a Lisp object.  In the case where instead we hold a `struct
     range_table_entry', just blow it off.  Otherwise we either need to do
     a bunch of painful and/or boring rewriting. --ben */
  if (do_lisp)
    {
      ga = XGAP_ARRAY (ALLOC_SIZED_LISP_OBJECT (sizeof (Gap_Array),
						gap_array));
      ga->is_lisp = 1;
    }
  else
#endif /* not NEW_GC */
    ga = xnew_and_zero (Gap_Array);
  ga->elsize = elsize;
  return ga;
}

Gap_Array *
gap_array_clone (Gap_Array *ga)
{
  Bytecount size = gap_array_byte_size (ga);
  Gap_Array *ga2;
  Gap_Array_Marker *m;

#ifdef NEW_GC
  if (ga->is_lisp)
    {
      ga2 = XGAP_ARRAY (ALLOC_SIZED_LISP_OBJECT (size, gap_array));
      copy_lisp_object (wrap_gap_array (ga2), wrap_gap_array (ga));
    }
  else
#endif
    {
      ga2 = (Gap_Array *) xmalloc (size);
      memcpy (ga2, ga, size);
    }
  ga2->markers = NULL;
  for (m = ga->markers; m; m = m->next)
    gap_array_make_marker (ga2, m->pos);
  return ga2;
}

#ifndef NEW_GC
void
free_gap_array (Gap_Array *ga)
{
  gap_array_delete_all_markers (ga);
  xfree (ga);
}
#endif /* not NEW_GC */


/*****************************************************************************/
/*                              Initialization                               */
/*****************************************************************************/

void
syms_of_array (void)
{
#ifdef NEW_GC
  INIT_LISP_OBJECT (gap_array_marker);
  INIT_LISP_OBJECT (gap_array);
#endif /* NEW_GC */
}

