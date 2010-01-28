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

A "dynamic array" is a contiguous array of fixed-size elements where there
is no upper limit (except available memory) on the number of elements in the
array.  Because the elements are maintained contiguously, space is used
efficiently (no per-element pointers necessary) and random access to a
particular element is in constant time.  At any one point, the block of memory
that holds the array has an upper limit; if this limit is exceeded, the
memory is realloc()ed into a new array that is twice as big.  Assuming that
the time to grow the array is on the order of the new size of the array
block, this scheme has a provably constant amortized time (i.e. average
time over all additions).

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

   void *Dynarr_new(type)
      [MACRO] Create a new dynamic-array object, with each element of the
      specified type.  The return value is cast to (type##_dynarr).
      This requires following the convention that types are declared in
      such a way that this type concatenation works.  In particular, TYPE
      must be a symbol, not an arbitrary C type.

   Dynarr_add(d, el)
      [MACRO] Add an element to the end of a dynamic array.  EL is a pointer
      to the element; the element itself is stored in the array, however.
      No function call is performed unless the array needs to be resized.

   Dynarr_add_many(d, base, len)
      [MACRO] Add LEN elements to the end of the dynamic array.  The elements
      should be contiguous in memory, starting at BASE.  If BASE if NULL,
      just make space for the elements; don't actually add them.

   Dynarr_insert_many_at_start(d, base, len)
      [MACRO] Append LEN elements to the beginning of the dynamic array.
      The elements should be contiguous in memory, starting at BASE.
      If BASE if NULL, just make space for the elements; don't actually
      add them.

   Dynarr_insert_many(d, base, len, start)
      Insert LEN elements to the dynamic array starting at position
      START.  The elements should be contiguous in memory, starting at BASE.
      If BASE if NULL, just make space for the elements; don't actually
      add them.

   Dynarr_delete(d, i)
      [MACRO] Delete an element from the dynamic array at position I.

   Dynarr_delete_many(d, start, len)
      Delete LEN elements from the dynamic array starting at position
      START.

   Dynarr_delete_by_pointer(d, p)
      [MACRO] Delete an element from the dynamic array at pointer P,
      which must point within the block of memory that stores the data.
      P should be obtained using Dynarr_atp().

   int Dynarr_length(d)
      [MACRO] Return the number of elements currently in a dynamic array.

   int Dynarr_largest(d)
      [MACRO] Return the maximum value that Dynarr_length(d) would
      ever have returned.  This is used esp. in the redisplay code,
      which reuses dynarrs for performance reasons.

   type Dynarr_at(d, i)
      [MACRO] Return the element at the specified index (no bounds checking
      done on the index).  The element itself is returned, not a pointer
      to it.

   type *Dynarr_atp(d, i)
      [MACRO] Return a pointer to the element at the specified index (no
      bounds checking done on the index).  The pointer may not be valid
      after an element is added to or removed from the array.

   Dynarr_reset(d)
      [MACRO] Reset the length of a dynamic array to 0.

   Dynarr_free(d)
      Destroy a dynamic array and the memory allocated to it.

Use the following global variable:

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


static int Dynarr_min_size = 8;

static void
Dynarr_realloc (Dynarr *dy, int new_size)
{
  if (DUMPEDP (dy->base))
    {
      void *new_base = malloc (new_size * dy->elsize);
      memcpy (new_base, dy->base, 
	      (dy->max < new_size ? dy->max : new_size) * dy->elsize);
      dy->base = new_base;
    }
  else
    dy->base = xrealloc (dy->base, new_size * dy->elsize);
}

void *
Dynarr_newf (int elsize)
{
  Dynarr *d = xnew_and_zero (Dynarr);
  d->elsize = elsize;

  return d;
}

#ifdef NEW_GC
DEFINE_LRECORD_IMPLEMENTATION ("dynarr", dynarr,
			       1, /*dumpable-flag*/
                               0, 0, 0, 0, 0,
			       0,
			       Dynarr);

static void
Dynarr_lisp_realloc (Dynarr *dy, int new_size)
{
  void *new_base = alloc_lrecord_array (dy->elsize, new_size, dy->lisp_imp);
  if (dy->base)
    memcpy (new_base, dy->base, 
	    (dy->max < new_size ? dy->max : new_size) * dy->elsize);
  dy->base = new_base;
}

void *
Dynarr_lisp_newf (int elsize, 
		  const struct lrecord_implementation *dynarr_imp, 
		  const struct lrecord_implementation *imp)
{
  Dynarr *d = (Dynarr *) alloc_lrecord (sizeof (Dynarr), dynarr_imp);
  d->elsize = elsize;
  d->lisp_imp = imp;

  return d;
}
#endif /* not NEW_GC */

void
Dynarr_resize (void *d, Elemcount size)
{
  int newsize;
  double multiplier;
  Dynarr *dy = (Dynarr *) Dynarr_verify (d);

  if (dy->max <= 8)
    multiplier = 2;
  else
    multiplier = 1.5;

  for (newsize = dy->max; newsize < size;)
    newsize = max (Dynarr_min_size, (int) (multiplier * newsize));

  /* Don't do anything if the array is already big enough. */
  if (newsize > dy->max)
    {
#ifdef NEW_GC
      if (dy->lisp_imp)
	Dynarr_lisp_realloc (dy, newsize);
      else
	Dynarr_realloc (dy, newsize);
#else /* not NEW_GC */
      Dynarr_realloc (dy, newsize);
#endif /* not NEW_GC */
      dy->max = newsize;
    }
}

/* Add a number of contiguous elements to the array starting at START. */
void
Dynarr_insert_many (void *d, const void *el, int len, int start)
{
  Dynarr *dy = (Dynarr *) Dynarr_verify (d);
  
  if (dy->len + len > dy->max)
    Dynarr_resize (dy, dy->len + len);
#if 0
  /* WTF? We should be catching these problems. */
  /* Silently adjust start to be valid. */
  if (start > dy->len)
    start = dy->len;
  else if (start < 0)
    start = 0;
#else
  /* #### This could conceivably be wrong, if code wants to access stuff
     between len and largest. */
  type_checking_assert (start >= 0 && start <= dy->len);
#endif

  if (start != dy->len)
    {
      memmove ((char *) dy->base + (start + len)*dy->elsize,
	       (char *) dy->base + start*dy->elsize,
	       (dy->len - start)*dy->elsize);
    }
  if (el)
    memcpy ((char *) dy->base + start*dy->elsize, el, len*dy->elsize);
  dy->len += len;

  if (dy->len > dy->largest)
    dy->largest = dy->len;
}

void
Dynarr_delete_many (void *d, int start, int len)
{
  Dynarr *dy = (Dynarr *) Dynarr_verify (d);

  type_checking_assert (start >= 0 && len >= 0 && start + len <= dy->len);
  memmove ((char *) dy->base + start*dy->elsize,
	   (char *) dy->base + (start + len)*dy->elsize,
	   (dy->len - start - len)*dy->elsize);
  dy->len -= len;
}

void
Dynarr_free (void *d)
{
  Dynarr *dy = (Dynarr *) d;

#ifdef NEW_GC
  if (dy->base && !DUMPEDP (dy->base))
    {
      if (!dy->lisp_imp)
	xfree (dy->base, void *);
    }
  if(!DUMPEDP (dy))
    {
      if (!dy->lisp_imp)
	xfree (dy, Dynarr *);
    }
#else /* not NEW_GC */
  if (dy->base && !DUMPEDP (dy->base))
    xfree (dy->base, void *);
  if(!DUMPEDP (dy))
    xfree (dy, Dynarr *);
#endif /* not NEW_GC */
}

#ifdef MEMORY_USAGE_STATS

/* Return memory usage for Dynarr D.  The returned value is the total
   amount of bytes actually being used for the Dynarr, including all
   overhead.  The extra amount of space in the Dynarr that is
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
      Bytecount malloc_used = malloced_storage_size (dy->base,
						     dy->elsize * dy->max, 0);
      /* #### This may or may not be correct.  Some Dynarrs would
	 prefer that we use dy->len instead of dy->largest here. */
      Bytecount was_requested = dy->elsize * dy->largest;
      Bytecount dynarr_overhead = dy->elsize * (dy->max - dy->largest);

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
  return Dynarr_atp (this_one, 0);
}

void
stack_like_free (void *val)
{
  int len = Dynarr_length (stack_like_in_use_list);
  assert (len > 0);
  /* The vast majority of times, we will be called in a last-in first-out
     order, and the item at the end of the list will be the one we're
     looking for, so just check for this first and avoid any function
     calls. */
  if (Dynarr_atp (Dynarr_at (stack_like_in_use_list, len - 1), 0) == val)
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
	if (Dynarr_atp (Dynarr_at (stack_like_in_use_list, i), 0) ==
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
