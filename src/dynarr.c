/* Support for dynamic arrays.
   Copyright (C) 1993 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003, 2004, 2005, 2010 Ben Wing.

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

#include <config.h>
#include "lisp.h"

/* ------------------------ dynamic arrays ------------------- */

static const struct memory_description int_dynarr_description_1[] = {
  XD_DYNARR_DESC (int_dynarr, &int_description),
  { XD_END }
};

const struct sized_memory_description int_dynarr_description = {
  sizeof (int_dynarr),
  int_dynarr_description_1
};

static const struct memory_description unsigned_char_dynarr_description_1[] = {
  XD_DYNARR_DESC (unsigned_char_dynarr, &unsigned_char_description),
  { XD_END }
};

const struct sized_memory_description unsigned_char_dynarr_description = {
  sizeof (unsigned_char_dynarr),
  unsigned_char_dynarr_description_1
};

static const struct memory_description Lisp_Object_dynarr_description_1[] = {
  XD_DYNARR_DESC (Lisp_Object_dynarr, &lisp_object_description),
  { XD_END }
};

/* Not static; used in mule-coding.c */
const struct sized_memory_description Lisp_Object_dynarr_description = {
  sizeof (Lisp_Object_dynarr),
  Lisp_Object_dynarr_description_1
};

static const struct memory_description Lisp_Object_pair_dynarr_description_1[] = {
  XD_DYNARR_DESC (Lisp_Object_pair_dynarr, &Lisp_Object_pair_description),
  { XD_END }
};

const struct sized_memory_description Lisp_Object_pair_dynarr_description = {
  sizeof (Lisp_Object_pair_dynarr),
  Lisp_Object_pair_dynarr_description_1
};

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
	xfree (dy->base);
    }
  if(!DUMPEDP (dy))
    {
      if (!dy->lisp_imp)
	xfree (dy);
    }
#else /* not NEW_GC */
  if (dy->base && !DUMPEDP (dy->base))
    xfree (dy->base);
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
Dynarr_memory_usage (void *d, struct overhead_stats *stats)
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

void
mark_Lisp_Object_dynarr (Lisp_Object_dynarr *dyn)
{
  int i;
  for (i = 0; i < Dynarr_length (dyn); i++)
    mark_object (Dynarr_at (dyn, i));
}

/* --------------------------- static dynarrs ------------------------- */

/* Add a number of contiguous elements to the array starting at START. */
void
Stynarr_insert_many_1 (void *d, const void *els, int len, int start,
		       int num_static, int elsize, int staticoff)
{
  Stynarr *dy = (Stynarr *) d;
  type_checking_assert (start >= 0 && start <= dy->nels);
  /* If we'll need Dynarr space, make sure the Dynarr is there */
  if (len + dy->nels > num_static && !dy->els)
    VOIDP_CAST (dy->els) = Dynarr_newf (elsize);
  /* Entirely within Dynarr? */
  if (start >= num_static)
    Dynarr_insert_many (dy->els, els, len, start - num_static);
  /* Entirely within static part? */
  else if (len + dy->nels <= num_static)
    {
      if (start != dy->nels)
	{
	  memmove ((char *) dy + staticoff + (start + len)*elsize,
		   (char *) dy + staticoff + start*elsize,
		   (dy->nels - start)*elsize);
	}
      if (els)
	memcpy ((char *) dy + staticoff + start*elsize, els, len*elsize);
    }
  /* Else, partly within static, partly within Dynarr */
  else
    {
      /* #### Finish me */
      ABORT ();
    }
}

/* ---------------------- stack-like malloc ----------------------- */

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
