/* Header for arrays (dynarrs, gap arrays, etc.).
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002, 2004, 2005, 2009, 2010 Ben Wing.

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

/* This file has been Mule-ized, Ben Wing, 10-13-04. */

#ifndef INCLUDED_array_h_
#define INCLUDED_array_h_

/************************************************************************/
/**               Definition of dynamic arrays (dynarrs)               **/
/************************************************************************/

BEGIN_C_DECLS

/************* Dynarr declaration *************/

#ifdef NEW_GC
#define DECLARE_DYNARR_LISP_IMP()			\
  const struct lrecord_implementation *lisp_imp;
#else
#define DECLARE_DYNARR_LISP_IMP()
#endif

#ifdef ERROR_CHECK_DYNARR
#define DECLARE_DYNARR_LOCKED()				\
  int locked;
#else
#define DECLARE_DYNARR_LOCKED()
#endif

#define Dynarr_declare(type)			\
  struct lrecord_header header;			\
  type *base;					\
  DECLARE_DYNARR_LISP_IMP ()			\
  DECLARE_DYNARR_LOCKED ()			\
  int elsize_;					\
  int len_;					\
  int largest_;					\
  int max_

typedef struct dynarr
{
  Dynarr_declare (void);
} Dynarr;

#define XD_DYNARR_DESC(base_type, sub_desc)				\
  { XD_BLOCK_PTR, offsetof (base_type, base),				\
    XD_INDIRECT(1, 0), {sub_desc} },					\
  { XD_INT,        offsetof (base_type, len_) },			\
  { XD_INT_RESET,  offsetof (base_type, largest_), XD_INDIRECT(1, 0) },	\
  { XD_INT_RESET,  offsetof (base_type, max_), XD_INDIRECT(1, 0) }

#ifdef NEW_GC
#define XD_LISP_DYNARR_DESC(base_type, sub_desc)			\
  { XD_INLINE_LISP_OBJECT_BLOCK_PTR, offsetof (base_type, base),		\
    XD_INDIRECT(1, 0), {sub_desc} },					\
  { XD_INT,        offsetof (base_type, len_) },			\
  { XD_INT_RESET,  offsetof (base_type, largest_), XD_INDIRECT(1, 0) },	\
  { XD_INT_RESET,  offsetof (base_type, max_), XD_INDIRECT(1, 0) }
#endif /* NEW_GC */

/************* Dynarr verification *************/

/* Dynarr locking and verification.

   [I] VERIFICATION

   Verification routines simply return their basic argument, possibly
   casted, but in the process perform some verification on it, aborting if
   the verification fails.  The verification routines take FILE and LINE
   parameters, and use them to output the file and line of the caller
   when an abort occurs, rather than the file and line of the inline
   function, which is less than useful.

   There are three basic types of verification routines:

   (1) Verify the dynarr itself.  This verifies the basic invariant
   involving the length/size values:

   0 <= Dynarr_length(d) <= Dynarr_largest(d) <= Dynarr_max(d)

   (2) Verify the dynarr itself prior to modifying it.  This performs
   the same verification as previously, but also checks that the
   dynarr is not locked (see below).

   (3) Verify a dynarr position.  Unfortunately we have to have
   different verification routines depending on which kind of operation
   is being performed:

   (a) For Dynarr_at(), we check that the POS is bounded by Dynarr_largest(),
       i.e. 0 <= POS < Dynarr_largest().
   (b) For Dynarr_atp_allow_end(), we also have to allow
       POS == Dynarr_largest().
   (c) For Dynarr_atp(), we behave largely like Dynarr_at() but make a
       special exception when POS == 0 and Dynarr_largest() == 0 -- see
       comment below.
   (d) Some other routines contain the POS verification within their code,
       and make the check 0 <= POS < Dynarr_length() or
       0 <= POS <= Dynarr_length().

   #### It is not well worked-out whether and in what circumstances it's
   allowed to use a position that is between Dynarr_length() and
   Dynarr_largest().  The ideal solution is to never allow this, and require
   instead that code first change the length before accessing higher
   positions.  That would require looking through all the code that accesses
   dynarrs and fixing it appropriately (especially redisplay code, and
   especially redisplay code in the vicinity of a reference to
   Dynarr_largest(), since such code usually checks explicitly to see whether
   there is extra stuff between Dynarr_length() and Dynarr_largest().)

   [II] LOCKING

   The idea behind dynarr locking is simple: Locking a dynarr prevents
   any modification from occurring, or rather, leads to an abort upon
   any attempt to modify a dynarr.

   Dynarr locking was originally added to catch some sporadic and hard-to-
   debug crashes in the redisplay code where dynarrs appeared to be getting
   corrupted in an unexpected fashion.  The solution was to lock the
   dynarrs that were getting corrupted (in this case, the display-line
   dynarrs) around calls to routines that weren't supposed to be changing
   these dynarrs but might somehow be calling code that modified them.
   This eventually revealed that there was a reentrancy problem with
   redisplay that involved the QUIT mechanism and the processing done in
   order to determine whether C-g had been pressed -- this processing
   involves retrieving, processing and queueing pending events to see
   whether any of them result in a C-g keypress.  However, at least under
   MS Windows this can result in redisplay being called reentrantly.
   For more info:--
   
  (Info-goto-node "(internals)Critical Redisplay Sections")

*/

#ifdef ERROR_CHECK_DYNARR
DECLARE_INLINE_HEADER (
int
Dynarr_verify_pos_at (void *d, Elemcount pos, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  /* We use `largest', not `len', because the redisplay code often
     accesses stuff between len and largest. */
  assert_at_line (pos >= 0 && pos < dy->largest_, file, line);
  return pos;
}

DECLARE_INLINE_HEADER (
int
Dynarr_verify_pos_atp (void *d, Elemcount pos, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  /* We use `largest', not `len', because the redisplay code often
     accesses stuff between len and largest. */
  /* [[ Code will often do something like ...

     val = make_bit_vector_from_byte_vector (Dynarr_atp (dyn, 0),
	                                     Dynarr_length (dyn));

     which works fine when the Dynarr_length is non-zero, but when zero,
     the result of Dynarr_atp() not only points past the end of the
     allocated array, but the array may not have ever been allocated and
     hence the return value is NULL.  But the length of 0 causes the
     pointer to never get checked.  These can occur throughout the code
     so we put in a special check. --ben ]]

     Update: The common idiom `Dynarr_atp (dyn, 0)' has been changed to
     `Dynarr_begin (dyn)'.  Possibly this special check at POS 0 can be
     done only for Dynarr_begin() not for general Dynarr_atp(). --ben */
  if (pos == 0 && dy->len_ == 0)
    return pos;
  /* #### It's vaguely possible that some code could legitimately want to
     retrieve a pointer to the position just past the end of dynarr memory.
     This could happen with Dynarr_atp() but not Dynarr_at().  If so, it
     will trigger this assert().  In such cases, it should be obvious that
     the code wants to do this; rather than relaxing the assert, we should
     probably create a new macro Dynarr_atp_allow_end() which is like
     Dynarr_atp() but which allows for pointing at invalid addresses -- we
     really want to check for cases of accessing just past the end of
     memory, which is a likely off-by-one problem to occur and will usually
     not trigger a protection fault (instead, you'll just get random
     behavior, possibly overwriting other memory, which is bad). --ben */
  assert_at_line (pos >= 0 && pos < dy->largest_, file, line);
  return pos;
}

DECLARE_INLINE_HEADER (
int
Dynarr_verify_pos_atp_allow_end (void *d, Elemcount pos, const Ascbyte *file,
				 int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  /* We use `largest', not `len', because the redisplay code often
     accesses stuff between len and largest.
     We also allow referencing the very end, past the end of allocated
     legitimately space.  See comments in Dynarr_verify_pos_atp.()*/
  assert_at_line (pos >= 0 && pos <= dy->largest_, file, line);
  return pos;
}

#else
#define Dynarr_verify_pos_at(d, pos, file, line) (pos)
#define Dynarr_verify_pos_atp(d, pos, file, line) (pos)
#define Dynarr_verify_pos_atp_allow_end(d, pos, file, line) (pos)
#endif /* ERROR_CHECK_DYNARR */

#ifdef ERROR_CHECK_DYNARR
DECLARE_INLINE_HEADER (
Dynarr *
Dynarr_verify_1 (void *d, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  assert_at_line (dy->len_ >= 0 && dy->len_ <= dy->largest_ &&
		  dy->largest_ <= dy->max_, file, line);
  return dy;
}

DECLARE_INLINE_HEADER (
Dynarr *
Dynarr_verify_mod_1 (void *d, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  assert_at_line (!dy->locked, file, line);
  return Dynarr_verify_1 (d, file, line);
}

#define Dynarr_verify(d) Dynarr_verify_1 (d, __FILE__, __LINE__)
#define Dynarr_verify_mod(d) Dynarr_verify_mod_1 (d, __FILE__, __LINE__)

DECLARE_INLINE_HEADER (
void
Dynarr_lock (void *d)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  dy->locked = 1;
}

DECLARE_INLINE_HEADER (
void
Dynarr_unlock (void *d)
)
{
  Dynarr *dy = Dynarr_verify (d);
  assert (dy->locked);
  dy->locked = 0;
}

#else /* not ERROR_CHECK_DYNARR */

#define Dynarr_verify(d) ((Dynarr *) d)
#define Dynarr_verify_mod(d) ((Dynarr *) d)
#define Dynarr_lock(d) DO_NOTHING
#define Dynarr_unlock(d) DO_NOTHING

#endif /* ERROR_CHECK_DYNARR */

/************* Dynarr creation *************/

MODULE_API void *Dynarr_newf (Bytecount elsize);
MODULE_API void Dynarr_free (void *d);

#ifdef NEW_GC
MODULE_API void *Dynarr_lisp_newf (Bytecount elsize,
				   const struct lrecord_implementation 
				   *dynarr_imp,
				   const struct lrecord_implementation *imp);

#define Dynarr_lisp_new(type, dynarr_imp, imp)			\
  ((type##_dynarr *) Dynarr_lisp_newf (sizeof (type), dynarr_imp, imp))
#define Dynarr_lisp_new2(dynarr_type, type, dynarr_imp, imp)	\
  ((dynarr_type *) Dynarr_lisp_newf (sizeof (type)), dynarr_imp, imp)
#endif /* NEW_GC */
#define Dynarr_new(type) ((type##_dynarr *) Dynarr_newf (sizeof (type)))
#define Dynarr_new2(dynarr_type, type) \
  ((dynarr_type *) Dynarr_newf (sizeof (type)))

/************* Dynarr access *************/

#ifdef ERROR_CHECK_DYNARR
#define Dynarr_at(d, pos) \
  ((d)->base[Dynarr_verify_pos_at (d, pos, __FILE__, __LINE__)])
#define Dynarr_atp_allow_end(d, pos) \
  (&((d)->base[Dynarr_verify_pos_atp_allow_end (d, pos, __FILE__, __LINE__)]))
#define Dynarr_atp(d, pos) \
  (&((d)->base[Dynarr_verify_pos_atp (d, pos, __FILE__, __LINE__)]))
#else
#define Dynarr_at(d, pos) ((d)->base[pos])
#define Dynarr_atp(d, pos) (&Dynarr_at (d, pos))
#define Dynarr_atp_allow_end(d, pos) Dynarr_atp (d, pos)
#endif

/* Old #define Dynarr_atp(d, pos) (&Dynarr_at (d, pos)) */
#define Dynarr_begin(d) Dynarr_atp (d, 0)
#define Dynarr_lastp(d) Dynarr_atp (d, Dynarr_length (d) - 1)
#define Dynarr_past_lastp(d) Dynarr_atp_allow_end (d, Dynarr_length (d))


/************* Dynarr length/size retrieval and setting *************/

/* Retrieve the length of a dynarr.  The `+ 0' is to ensure that this cannot
   be used as an lvalue. */
#define Dynarr_length(d) (Dynarr_verify (d)->len_ + 0)
/* Retrieve the largest ever length seen of a dynarr.  The `+ 0' is to
   ensure that this cannot be used as an lvalue. */
#define Dynarr_largest(d) (Dynarr_verify (d)->largest_ + 0)
/* Retrieve the number of elements that fit in the currently allocated
   space.  The `+ 0' is to ensure that this cannot be used as an lvalue. */
#define Dynarr_max(d) (Dynarr_verify (d)->max_ + 0)
/* Return the size in bytes of an element in a dynarr. */
#define Dynarr_elsize(d) (Dynarr_verify (d)->elsize_ + 0)
/* Retrieve the advertised memory usage of a dynarr, i.e. the number of
   bytes occupied by the elements in the dynarr, not counting any overhead. */
#define Dynarr_sizeof(d) (Dynarr_length (d) * Dynarr_elsize (d))

/* Actually set the length of a dynarr.  This is a low-level routine that
   should not be directly used; use Dynarr_set_length() or
   Dynarr_set_lengthr() instead. */
DECLARE_INLINE_HEADER (
void
Dynarr_set_length_1 (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  dynarr_checking_assert (len >= 0 && len <= Dynarr_max (dy));
  /* Use the raw field references here otherwise we get a crash because
     we've set the length but not yet fixed up the largest value. */
  dy->len_ = len;
  if (dy->len_ > dy->largest_)
    dy->largest_ = dy->len_;
  (void) Dynarr_verify_mod (d);
}

/* "Restricted set-length": Set the length of dynarr D to LEN,
    which must be in the range [0, Dynarr_largest(d)]. */

DECLARE_INLINE_HEADER (
void
Dynarr_set_lengthr (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  dynarr_checking_assert (len >= 0 && len <= Dynarr_largest (dy));
  Dynarr_set_length_1 (dy, len);
}

/* "Restricted increment": Increment the length of dynarr D by 1; the resulting
    length must be in the range [0, Dynarr_largest(d)]. */

#define Dynarr_incrementr(d) Dynarr_set_lengthr (d, Dynarr_length (d) + 1)


MODULE_API void Dynarr_resize (void *d, Elemcount size);

DECLARE_INLINE_HEADER (
void
Dynarr_resize_to_fit (void *d, Elemcount size)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  if (size > Dynarr_max (dy))
    Dynarr_resize (dy, size);
}

#define Dynarr_resize_to_add(d, numels)			\
  Dynarr_resize_to_fit (d, Dynarr_length (d) + numels)

/* This is an optimization.  This is like Dynarr_set_length() but the length
   is guaranteed to be at least as big as the existing length. */

DECLARE_INLINE_HEADER (
void
Dynarr_increase_length (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  dynarr_checking_assert (len >= Dynarr_length (dy));
  Dynarr_resize_to_fit (dy, len);
  Dynarr_set_length_1 (dy, len);
}

/* Set the length of dynarr D to LEN.  If the length increases, resize as
   necessary to fit. (NOTE: This will leave uninitialized memory.  If you
   aren't planning on immediately overwriting the memory, use
   Dynarr_set_length_and_zero() to zero out all the memory that would
   otherwise be uninitialized.) */

DECLARE_INLINE_HEADER (
void
Dynarr_set_length (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  Elemcount old_len = Dynarr_length (dy);
  if (old_len >= len)
    Dynarr_set_lengthr (dy, len);
  else
    Dynarr_increase_length (d, len);
}

#define Dynarr_increment(d) Dynarr_increase_length (d, Dynarr_length (d) + 1)

/* Zero LEN contiguous elements starting at POS. */

DECLARE_INLINE_HEADER (
void
Dynarr_zero_many (void *d, Elemcount pos, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  memset ((Rawbyte *) dy->base + pos*Dynarr_elsize (dy), 0,
	  len*Dynarr_elsize (dy));
}

/* This is an optimization.  This is like Dynarr_set_length_and_zero() but
   the length is guaranteed to be at least as big as the existing
   length. */

DECLARE_INLINE_HEADER (
void
Dynarr_increase_length_and_zero (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  Elemcount old_len = Dynarr_length (dy);
  Dynarr_increase_length (dy, len);
  Dynarr_zero_many (dy, old_len, len - old_len);
}

/* Set the length of dynarr D to LEN.  If the length increases, resize as
   necessary to fit and zero out all the elements between the old and new
   lengths. */

DECLARE_INLINE_HEADER (
void
Dynarr_set_length_and_zero (void *d, Elemcount len)
)
{
  Dynarr *dy = Dynarr_verify_mod (d);
  Elemcount old_len = Dynarr_length (dy);
  if (old_len >= len)
    Dynarr_set_lengthr (dy, len);
  else
    Dynarr_increase_length_and_zero (d, len);
}

/* Reset the dynarr's length to 0. */
#define Dynarr_reset(d) Dynarr_set_lengthr (d, 0)

#ifdef MEMORY_USAGE_STATS
struct usage_stats;
Bytecount Dynarr_memory_usage (void *d, struct usage_stats *stats);
#endif

/************* Adding/deleting elements to/from a dynarr *************/

/* Set the Lisp implementation of the element at POS in dynarr D.  Only
   does this if the dynarr holds Lisp objects of a particular type (the
   objects themselves, not pointers to them), and only under NEW_GC. */

#ifdef NEW_GC
#define DYNARR_SET_LISP_IMP(d, pos)					\
do {									\
  if ((d)->lisp_imp)							\
    set_lheader_implementation						\
      ((struct lrecord_header *)&(((d)->base)[pos]), (d)->lisp_imp);	\
} while (0)  
#else
#define DYNARR_SET_LISP_IMP(d, pos) DO_NOTHING
#endif /* (not) NEW_GC */

/* Add Element EL to the end of dynarr D. */

#define Dynarr_add(d, el)			\
do {						\
  Elemcount _da_pos = Dynarr_length (d);	\
  (void) Dynarr_verify_mod (d);			\
  Dynarr_increment (d);				\
  ((d)->base)[_da_pos] = (el);			\
  DYNARR_SET_LISP_IMP (d, _da_pos);		\
} while (0)

/* Set EL as the element at position POS in dynarr D.
   Expand the dynarr as necessary so that its length is enough to include
   position POS within it, and zero out any new elements created as a
   result of expansion, other than the one at POS. */

#define Dynarr_set(d, pos, el)				\
do {							\
  Elemcount _ds_pos = (pos);				\
  (void) Dynarr_verify_mod (d);				\
  if (Dynarr_length (d) < _ds_pos + 1)			\
    Dynarr_increase_length_and_zero (d, _ds_pos + 1);	\
  ((d)->base)[_ds_pos] = (el);				\
  DYNARR_SET_LISP_IMP (d, _ds_pos);			\
} while (0)

/* Add LEN contiguous elements, stored at BASE, to dynarr D.  If BASE is
   NULL, reserve space but don't store anything. */

DECLARE_INLINE_HEADER (
void
Dynarr_add_many (void *d, const void *base, Elemcount len)
)
{
  /* This duplicates Dynarr_insert_many to some extent; but since it is
     called so often, it seemed useful to remove the unnecessary stuff
     from that function and to make it inline */
  Dynarr *dy = Dynarr_verify_mod (d);
  Elemcount pos = Dynarr_length (dy);
  Dynarr_increase_length (dy, Dynarr_length (dy) + len);
  if (base)
    memcpy ((Rawbyte *) dy->base + pos*Dynarr_elsize (dy), base,
	    len*Dynarr_elsize (dy));
}

/* Insert LEN elements, currently pointed to by BASE, into dynarr D
   starting at position POS. */

MODULE_API void Dynarr_insert_many (void *d, const void *base, Elemcount len,
				    Elemcount pos);

/* Prepend LEN elements, currently pointed to by BASE, to the beginning. */

#define Dynarr_prepend_many(d, base, len) Dynarr_insert_many (d, base, len, 0)

/* Add literal string S to dynarr D, which should hold chars or unsigned
   chars.  The final zero byte is not stored. */

#define Dynarr_add_literal_string(d, s) Dynarr_add_many (d, s, sizeof (s) - 1)

/* Convert Lisp string S to an external encoding according to CODESYS and
   add to dynarr D, which should hold chars or unsigned chars.  No final
   zero byte is appended. */

/* #### This should be an inline function but LISP_STRING_TO_SIZED_EXTERNAL
   isn't declared yet. */

#define Dynarr_add_ext_lisp_string(d, s, codesys)		\
do {								\
  Lisp_Object dyna_ls_s = (s);					\
  Lisp_Object dyna_ls_cs = (codesys);				\
  Extbyte *dyna_ls_eb;						\
  Bytecount dyna_ls_bc;						\
								\
  LISP_STRING_TO_SIZED_EXTERNAL (dyna_ls_s, dyna_ls_eb,		\
				 dyna_ls_bc, dyna_ls_cs);	\
  Dynarr_add_many (d, dyna_ls_eb, dyna_ls_bc);			\
} while (0)

/* Delete LEN elements starting at position POS. */

MODULE_API void Dynarr_delete_many (void *d, Elemcount pos, Elemcount len);

/* Pop off (i.e. delete) the last element from the dynarr and return it */

#define Dynarr_pop(d)					\
  (dynarr_checking_assert (Dynarr_length (d) > 0),	\
   Dynarr_verify_mod (d)->len_--,			\
   Dynarr_at (d, Dynarr_length (d)))

/* Delete the item at POS */

#define Dynarr_delete(d, pos) Dynarr_delete_many (d, pos, 1)

/* Delete the item located at memory address P, which must be a `type *'
   pointer, where `type' is the type of the elements of the dynarr. */
#define Dynarr_delete_by_pointer(d, p) \
  Dynarr_delete_many (d, (p) - ((d)->base), 1)

/* Delete all elements that are numerically equal to EL. */

#define Dynarr_delete_object(d, el)		\
do						\
{						\
  REGISTER int i;				\
  for (i = Dynarr_length (d) - 1; i >= 0; i--)	\
    {						\
      if (el == Dynarr_at (d, i))		\
	Dynarr_delete_many (d, i, 1);		\
    }						\
} while (0)


/************************************************************************/
/**                       Stack-like malloc/free                       **/
/************************************************************************/

#ifdef WIN32_ANY
void *stack_like_malloc (Bytecount size);
void stack_like_free (void *val);
#endif


/************************************************************************/
/**                             Gap array                              **/
/************************************************************************/

/* Holds a marker that moves as elements in the array are inserted and
   deleted, similar to standard markers. */

typedef struct gap_array_marker
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */
  int pos;
  struct gap_array_marker *next;
} Gap_Array_Marker;


/* Holds a "gap array", which is an array of elements with a gap located
   in it.  Insertions and deletions with a high degree of locality
   are very fast, essentially in constant time.  Array positions as
   used and returned in the gap array functions are independent of
   the gap. */

/* Layout of gap array:

   <------ gap ------><---- gapsize ----><----- numels - gap ---->
   <---------------------- numels + gapsize --------------------->

   For marking purposes, we use two extra variables computed from
   the others -- the offset to the data past the gap, plus the number
   of elements in that data:

   offset_past_gap = elsize * (gap + gapsize)
   els_past_gap = numels - gap
*/


typedef struct gap_array
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
  int is_lisp;
#endif /* NEW_GC */
  Elemcount gap;
  Elemcount gapsize;
  Elemcount numels;
  Bytecount elsize;
  /* Redundant numbers computed from the others, for marking purposes */
  Bytecount offset_past_gap;
  Elemcount els_past_gap;
  Gap_Array_Marker *markers;
  /* this is a stretchy array */
  char array[1];
} Gap_Array;

#ifdef NEW_GC
struct gap_array_marker;

DECLARE_LISP_OBJECT (gap_array_marker, struct gap_array_marker);
#define XGAP_ARRAY_MARKER(x) \
  XRECORD (x, gap_array_marker, struct gap_array_marker)
#define wrap_gap_array_marker(p) wrap_record (p, gap_array_marker)
#define GAP_ARRAY_MARKERP(x) RECORDP (x, gap_array_marker)
#define CHECK_GAP_ARRAY_MARKER(x) CHECK_RECORD (x, gap_array_marker)
#define CONCHECK_GAP_ARRAY_MARKER(x) CONCHECK_RECORD (x, gap_array_marker)

struct gap_array;

DECLARE_LISP_OBJECT (gap_array, struct gap_array);
#define XGAP_ARRAY(x) XRECORD (x, gap_array, struct gap_array)
#define wrap_gap_array(p) wrap_record (p, gap_array)
#define GAP_ARRAYP(x) RECORDP (x, gap_array)
#define CHECK_GAP_ARRAY(x) CHECK_RECORD (x, gap_array)
#define CONCHECK_GAP_ARRAY(x) CONCHECK_RECORD (x, gap_array)
#endif /* NEW_GC */

#ifdef NEW_GC
#define XD_GAP_ARRAY_MARKER_DESC					\
  { XD_LISP_OBJECT, offsetof (Gap_Array, markers) }
#else /* not NEW_GC */
#define XD_GAP_ARRAY_MARKER_DESC					\
  { XD_BLOCK_PTR, offsetof (Gap_Array, markers), 1,			\
    { &gap_array_marker_description }, XD_FLAG_NO_KKCC }
#endif /* not NEW_GC */

#define XD_GAP_ARRAY_DESC(sub_desc)					\
  { XD_ELEMCOUNT, offsetof (Gap_Array, gap) },				\
  { XD_BYTECOUNT, offsetof (Gap_Array, offset_past_gap) },		\
  { XD_ELEMCOUNT, offsetof (Gap_Array, els_past_gap) },			\
  XD_GAP_ARRAY_MARKER_DESC,						\
  { XD_BLOCK_ARRAY, offsetof (Gap_Array, array), XD_INDIRECT (0, 0),	\
    { sub_desc } },							\
  { XD_BLOCK_ARRAY, XD_INDIRECT (1, offsetof (Gap_Array, array)),	\
    XD_INDIRECT (2, 0), { sub_desc } }

/* Convert a "memory position" (i.e. taking the gap into account) into
   the address of the element at (i.e. after) that position.  "Memory
   positions" are only used internally and are of type Memxpos.
   "Array positions" are used externally and are of type Elemcount. */
#define GAP_ARRAY_MEMEL_ADDR(ga, memel) ((ga)->array + (ga)->elsize*(memel))

/* Number of elements currently in a gap array */
#define gap_array_length(ga) ((ga)->numels)

#define gap_array_gappos(ga) ((ga)->gap)
#define gap_array_gapsize(ga) ((ga)->gapsize)

#define GAP_ARRAY_ARRAY_TO_MEMORY_POS_1(pos, gappos, gapsize) \
  ((pos) < gappos ? (pos) : (pos) + gapsize)

#define GAP_ARRAY_ARRAY_TO_MEMORY_POS(ga, pos) \
  GAP_ARRAY_ARRAY_TO_MEMORY_POS_1 (pos, (ga)->gap, (ga)->gapsize)

#define GAP_ARRAY_MEMORY_TO_ARRAY_POS(ga, pos) \
  ((pos) <= (ga)->gap ? (pos) : (pos) - (ga)->gapsize)

/* Return a pointer to the element at a given position. */
#define gap_array_atp(ga, pos, type) \
  ((type *) GAP_ARRAY_MEMEL_ADDR (ga, GAP_ARRAY_ARRAY_TO_MEMORY_POS (ga, pos)))

/* Return the element at a given position. */
#define gap_array_at(ga, pos, type) (*gap_array_atp (ga, pos, type))

/* Return a pointer to the beginning of memory storage for the gap array.
   Note this is NOT the same as gap_array_atp(ga, 0, type) because that
   will skip forward past the gap if the gap is at position 0. */
#define gap_array_begin(ga, type) ((type *) ((ga)->array))

#ifndef NEW_GC
extern const struct sized_memory_description lispobj_gap_array_description;
extern const struct sized_memory_description gap_array_marker_description;
#endif

Bytecount gap_array_byte_size (Gap_Array *ga);
Gap_Array *gap_array_insert_els (Gap_Array *ga, Elemcount pos, void *elptr,
				 Elemcount numels);
void gap_array_delete_els (Gap_Array *ga, Elemcount from, Elemcount numdel);
#define gap_array_delete_all_els(ga) \
  gap_array_delete_els (ga, 0, gap_array_length (ga))
Gap_Array_Marker *gap_array_make_marker (Gap_Array *ga, Elemcount pos);
void gap_array_delete_marker (Gap_Array *ga, Gap_Array_Marker *m);
void gap_array_delete_all_markers (Gap_Array *ga);
void gap_array_move_marker (Gap_Array *ga, Gap_Array_Marker *m, Elemcount pos);
#define gap_array_marker_pos(ga, m) \
  GAP_ARRAY_MEMORY_TO_ARRAY_POS (ga, (m)->pos)
Gap_Array *make_gap_array (Elemcount elsize, int USED_IF_NEW_GC (do_lisp));
Gap_Array *gap_array_clone (Gap_Array *ga);
void free_gap_array (Gap_Array *ga);
Bytecount gap_array_memory_usage (Gap_Array *ga, struct usage_stats *stats,
				  Bytecount *marker_ancillary);

#endif /* INCLUDED_array_h_ */
