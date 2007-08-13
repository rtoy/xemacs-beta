/* Lisp interface to hash tables.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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
#include "hash.h"
#include "elhash.h"
#include "bytecode.h"

Lisp_Object Qhashtablep;

#define LISP_OBJECTS_PER_HENTRY (sizeof (hentry) / sizeof (Lisp_Object))/* 2 */

struct hashtable
{
  struct lcrecord_header header;
  unsigned int fullness;
  unsigned long (*hash_function) (CONST void *);
  int		(*test_function) (CONST void *, CONST void *);
  Lisp_Object zero_entry;
  Lisp_Object harray;
  enum hashtable_type type; /* whether and how this hashtable is weak */
  Lisp_Object next_weak;    /* Used to chain together all of the weak
			       hashtables.  Don't mark through this. */
};

static Lisp_Object Vall_weak_hashtables;

static Lisp_Object mark_hashtable (Lisp_Object, void (*) (Lisp_Object));
static void print_hashtable (Lisp_Object, Lisp_Object, int);
DEFINE_LRECORD_IMPLEMENTATION ("hashtable", hashtable,
                               mark_hashtable, print_hashtable, 0, 0, 0,
			       struct hashtable);

static Lisp_Object
mark_hashtable (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct hashtable *table = XHASHTABLE (obj);

  if (table->type != HASHTABLE_NONWEAK)
    {
      /* If the table is weak, we don't want to mark the keys and values
	 (we scan over them after everything else has been marked,
	 and mark or remove them as necessary).	 Note that we will mark
	 the table->harray itself at the same time; it's hard to mark
	 that here without also marking its contents. */
      return Qnil;
    }
  ((markobj) (table->zero_entry));
  return table->harray;
}

static void
print_hashtable (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct hashtable *table = XHASHTABLE (obj);
  char buf[200];
  if (print_readably)
    error ("printing unreadable object #<hashtable 0x%x>",
	   table->header.uid);
  sprintf (buf, GETTEXT ("#<%shashtable %d/%ld 0x%x>"),
	   (table->type == HASHTABLE_WEAK ? "weak " :
	    table->type == HASHTABLE_KEY_WEAK ? "key-weak " :
	    table->type == HASHTABLE_VALUE_WEAK ? "value-weak " :
	    table->type == HASHTABLE_KEY_CAR_WEAK ? "key-car-weak " :
	    table->type == HASHTABLE_VALUE_CAR_WEAK ? "value-car-weak " :
	    ""),
           table->fullness,
           XVECTOR_LENGTH (table->harray) / LISP_OBJECTS_PER_HENTRY,
           table->header.uid);
  write_c_string (buf, printcharfun);
}

static void
ht_copy_to_c (struct hashtable *ht, c_hashtable c_table)
{
  int len = XVECTOR_LENGTH (ht->harray);

  c_table->harray = (hentry *) XVECTOR_DATA (ht->harray);
  c_table->zero_set = (!GC_UNBOUNDP (ht->zero_entry));
  c_table->zero_entry = LISP_TO_VOID (ht->zero_entry);
  if (len < 0)
    {
      /* #### if alloc.c mark_object() changes, this must change too. */
      /* barf gag retch.  When a vector is marked, its len is
	 made less than 0.  In the prune_weak_hashtables() stage,
	 we are called on vectors that are like this, and we must
	 be able to deal. */
      assert (gc_in_progress);
      len = -1 - len;
    }
  c_table->size          = len/LISP_OBJECTS_PER_HENTRY;
  c_table->fullness      = ht->fullness;
  c_table->hash_function = ht->hash_function;
  c_table->test_function = ht->test_function;
  XSETHASHTABLE (c_table->elisp_table, ht);
}

static void
ht_copy_from_c (c_hashtable c_table, struct hashtable *ht)
{
  struct Lisp_Vector dummy;
  /* C is truly hateful */
  void *vec_addr
    = ((char *) c_table->harray
       - ((char *) &(dummy.contents[0]) - (char *) &dummy));

  XSETVECTOR (ht->harray, vec_addr);
  if (c_table->zero_set)
    VOID_TO_LISP (ht->zero_entry, c_table->zero_entry);
  else
    ht->zero_entry = Qunbound;
  ht->fullness = c_table->fullness;
}


static struct hashtable *
allocate_hashtable (void)
{
  struct hashtable *table =
    alloc_lcrecord_type (struct hashtable, lrecord_hashtable);
  table->harray        = Qnil;
  table->zero_entry    = Qunbound;
  table->fullness      = 0;
  table->hash_function = 0;
  table->test_function = 0;
  return table;
}

void *
elisp_hvector_malloc (unsigned int bytes, Lisp_Object table)
{
  Lisp_Object new_vector;
  struct hashtable *ht = XHASHTABLE (table);

  assert (bytes > XVECTOR_LENGTH (ht->harray) * sizeof (Lisp_Object));
  new_vector = make_vector ((bytes / sizeof (Lisp_Object)), Qzero);
  return (void *) XVECTOR_DATA (new_vector);
}

void
elisp_hvector_free (void *ptr, Lisp_Object table)
{
  struct hashtable *ht = XHASHTABLE (table);
#if defined (USE_ASSERTIONS) || defined (DEBUG_XEMACS)
  Lisp_Object current_vector = ht->harray;
#endif

  assert (((void *) XVECTOR_DATA (current_vector)) == ptr);
  ht->harray = Qnil;            /* Let GC do its job */
}


DEFUN ("hashtablep", Fhashtablep, 1, 1, 0, /*
Return t if OBJ is a hashtable, else nil.
*/
       (obj))
{
  return HASHTABLEP (obj) ? Qt : Qnil;
}




#if 0 /* I don't think these are needed any more.
	 If using the general lisp_object_equal_*() functions
	 causes efficiency problems, these can be resurrected. --ben */
/* equality and hash functions for Lisp strings */
int
lisp_string_equal (CONST void *x1, CONST void *x2)
{
  Lisp_Object str1, str2;
  CVOID_TO_LISP (str1, x1);
  CVOID_TO_LISP (str2, x2);
  return !strcmp ((char *) XSTRING_DATA (str1), (char *) XSTRING_DATA (str2));
}

unsigned long
lisp_string_hash (CONST void *x)
{
  Lisp_Object str;
  CVOID_TO_LISP (str, x);
  return hash_string (XSTRING_DATA (str), XSTRING_LENGTH (str));
}

#endif /* 0 */

static int
lisp_object_eql_equal (CONST void *x1, CONST void *x2)
{
  Lisp_Object obj1, obj2;
  CVOID_TO_LISP (obj1, x1);
  CVOID_TO_LISP (obj2, x2);
  return FLOATP (obj1) ? internal_equal (obj1, obj2, 0) : EQ (obj1, obj2);
}

static unsigned long
lisp_object_eql_hash (CONST void *x)
{
  Lisp_Object obj;
  CVOID_TO_LISP (obj, x);
  if (FLOATP (obj))
    return internal_hash (obj, 0);
  else
    return LISP_HASH (obj);
}

static int
lisp_object_equal_equal (CONST void *x1, CONST void *x2)
{
  Lisp_Object obj1, obj2;
  CVOID_TO_LISP (obj1, x1);
  CVOID_TO_LISP (obj2, x2);
  return internal_equal (obj1, obj2, 0);
}

static unsigned long
lisp_object_equal_hash (CONST void *x)
{
  Lisp_Object obj;
  CVOID_TO_LISP (obj, x);
  return internal_hash (obj, 0);
}

Lisp_Object
make_lisp_hashtable (int size,
		     enum hashtable_type type,
		     enum hashtable_test_fun test)
{
  Lisp_Object result;
  struct hashtable *table = allocate_hashtable ();

  table->harray = make_vector ((compute_harray_size (size)
				* LISP_OBJECTS_PER_HENTRY),
                               Qzero);
  switch (test)
    {
    case HASHTABLE_EQ:
      table->test_function = NULL;
      table->hash_function = NULL;
      break;

    case HASHTABLE_EQL:
      table->test_function = lisp_object_eql_equal;
      table->hash_function = lisp_object_eql_hash;
      break;

    case HASHTABLE_EQUAL:
      table->test_function = lisp_object_equal_equal;
      table->hash_function = lisp_object_equal_hash;
      break;

    default:
      abort ();
    }

  table->type = type;
  XSETHASHTABLE (result, table);

  if (table->type != HASHTABLE_NONWEAK)
    {
      table->next_weak = Vall_weak_hashtables;
      Vall_weak_hashtables = result;
    }
  else
    table->next_weak = Qunbound;

  return result;
}

static enum hashtable_test_fun
decode_hashtable_test_fun (Lisp_Object sym)
{
  if (NILP (sym))       return HASHTABLE_EQL;
  if (EQ (sym, Qeq))    return HASHTABLE_EQ;
  if (EQ (sym, Qequal)) return HASHTABLE_EQUAL;
  if (EQ (sym, Qeql))   return HASHTABLE_EQL;

  signal_simple_error ("Invalid hashtable test fun", sym);
  return HASHTABLE_EQ; /* not reached */
}

DEFUN ("make-hashtable", Fmake_hashtable, 1, 2, 0, /*
Make a hashtable of initial size SIZE.
Comparison between keys is done with TEST-FUN, which must be one of
`eq', `eql', or `equal'.  The default is `eql'; i.e. two keys must
be the same object (or have the same floating-point value, for floats)
to be considered equivalent.

See also `make-weak-hashtable', `make-key-weak-hashtable', and
`make-value-weak-hashtable'.
*/
       (size, test_fun))
{
  CHECK_NATNUM (size);
  return make_lisp_hashtable (XINT (size), HASHTABLE_NONWEAK,
			      decode_hashtable_test_fun (test_fun));
}

DEFUN ("copy-hashtable", Fcopy_hashtable, 1, 1, 0, /*
Make a new hashtable which contains the same keys and values
as the given table.  The keys and values will not themselves be copied.
*/
       (old_table))
{
  struct _C_hashtable old_htbl;
  struct _C_hashtable new_htbl;
  struct hashtable *old_ht;
  struct hashtable *new_ht;
  Lisp_Object result;

  CHECK_HASHTABLE (old_table);
  old_ht = XHASHTABLE (old_table);
  ht_copy_to_c (old_ht, &old_htbl);

  /* we can't just call Fmake_hashtable() here because that will make a
     table that is slightly larger than the one we're trying to copy,
     which will make copy_hash() blow up. */
  new_ht = allocate_hashtable ();
  new_ht->fullness = 0;
  new_ht->zero_entry = Qunbound;
  new_ht->hash_function = old_ht->hash_function;
  new_ht->test_function = old_ht->test_function;
  new_ht->harray = Fmake_vector (Flength (old_ht->harray), Qzero);
  ht_copy_to_c (new_ht, &new_htbl);
  copy_hash (&new_htbl, &old_htbl);
  ht_copy_from_c (&new_htbl, new_ht);
  new_ht->type = old_ht->type;
  XSETHASHTABLE (result, new_ht);

  if (UNBOUNDP (old_ht->next_weak))
    new_ht->next_weak = Qunbound;
  else
    {
      new_ht->next_weak = Vall_weak_hashtables;
      Vall_weak_hashtables = result;
    }

  return result;
}


DEFUN ("gethash", Fgethash, 2, 3, 0, /*
Find hash value for KEY in HASHTABLE.
If there is no corresponding value, return DEFAULT (defaults to nil).
*/
       (key, hashtable, default_))
{
  CONST void *vval;
  struct _C_hashtable htbl;
  if (!gc_in_progress)
    CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  if (gethash (LISP_TO_VOID (key), &htbl, &vval))
    {
      Lisp_Object val;
      CVOID_TO_LISP (val, vval);
      return val;
    }
  else
    return default_;
}


DEFUN ("remhash", Fremhash, 2, 2, 0, /*
Remove hash value for KEY in HASHTABLE.
*/
       (key, hashtable))
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (hashtable);

  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  remhash (LISP_TO_VOID (key), &htbl);
  ht_copy_from_c (&htbl, XHASHTABLE (hashtable));
  return Qnil;
}


DEFUN ("puthash", Fputhash, 3, 3, 0, /*
Hash KEY to VAL in HASHTABLE.
*/
       (key, val, hashtable))
{
  struct hashtable *ht;
  void *vkey = LISP_TO_VOID (key);

  CHECK_HASHTABLE (hashtable);
  ht = XHASHTABLE (hashtable);
  if (!vkey)
    ht->zero_entry = val;
  else
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      struct _C_hashtable htbl;

      ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
      GCPRO3 (key, val, hashtable);
      puthash (vkey, LISP_TO_VOID (val), &htbl);
      ht_copy_from_c (&htbl, XHASHTABLE (hashtable));
      UNGCPRO;
    }
  return val;
}

DEFUN ("clrhash", Fclrhash, 1, 1, 0, /*
Remove all entries from HASHTABLE.
*/
       (hashtable))
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  clrhash (&htbl);
  ht_copy_from_c (&htbl, XHASHTABLE (hashtable));
  return Qnil;
}

DEFUN ("hashtable-fullness", Fhashtable_fullness, 1, 1, 0, /*
Return number of entries in HASHTABLE.
*/
       (hashtable))
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  return make_int (htbl.fullness);
}


static void
verify_function (Lisp_Object function, CONST char *description)
{
  if (SYMBOLP (function))
  {
    if (NILP (function))
      return;
    else
      function = indirect_function (function, 1);
  }
  if (SUBRP (function) || COMPILED_FUNCTIONP (function))
    return;
  else if (CONSP (function))
  {
    Lisp_Object funcar = Fcar (function);
    if ((SYMBOLP (funcar)) && (EQ (funcar, Qlambda) ||
			       EQ (funcar, Qautoload)))
      return;
  }
  signal_error (Qinvalid_function, list1 (function));
}

static void
lisp_maphash_function (CONST void *void_key,
		       void *void_val,
		       void *void_fn)
{
  /* This function can GC */
  Lisp_Object key, val, fn;
  CVOID_TO_LISP (key, void_key);
  VOID_TO_LISP (val, void_val);
  VOID_TO_LISP (fn, void_fn);
  call2 (fn, key, val);
}


DEFUN ("maphash", Fmaphash, 2, 2, 0, /*
Map FUNCTION over entries in HASHTABLE, calling it with two args,
each key and value in the table.
*/
       (function, hashtable))
{
  struct _C_hashtable htbl;
  struct gcpro gcpro1, gcpro2;

  verify_function (function, GETTEXT ("hashtable mapping function"));
  CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  GCPRO2 (hashtable, function);
  maphash (lisp_maphash_function, &htbl, LISP_TO_VOID (function));
  UNGCPRO;
  return Qnil;
}


/* This function is for mapping a *C* function over the elements of a
   lisp hashtable.
 */
void
elisp_maphash (maphash_function function, Lisp_Object hashtable, void *closure)
{
  struct _C_hashtable htbl;

  if (!gc_in_progress) CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  maphash (function, &htbl, closure);
}

void
elisp_map_remhash (remhash_predicate function, Lisp_Object hashtable,
		   void *closure)
{
  struct _C_hashtable htbl;

  if (!gc_in_progress) CHECK_HASHTABLE (hashtable);
  ht_copy_to_c (XHASHTABLE (hashtable), &htbl);
  map_remhash (function, &htbl, closure);
  ht_copy_from_c (&htbl, XHASHTABLE (hashtable));
}

#if 0
void
elisp_table_op (Lisp_Object table, generic_hashtable_op op, void *arg1,
		void *arg2, void *arg3)
{
  struct _C_hashtable htbl;
  CHECK_HASHTABLE (table);
  ht_copy_to_c (XHASHTABLE (table), &htbl);
  (*op) (&htbl, arg1, arg2, arg3);
  ht_copy_from_c (&htbl, XHASHTABLE (table));
}
#endif /* 0 */



DEFUN ("make-weak-hashtable", Fmake_weak_hashtable, 1, 2, 0, /*
Make a fully weak hashtable of initial size SIZE.
A weak hashtable is one whose pointers do not count as GC referents:
for any key-value pair in the hashtable, if the only remaining pointer
to either the key or the value is in a weak hash table, then the pair
will be removed from the table, and the key and value collected.  A
non-weak hash table (or any other pointer) would prevent the object
from being collected.

You can also create semi-weak hashtables; see `make-key-weak-hashtable'
and `make-value-weak-hashtable'.
*/
       (size, test_fun))
{
  CHECK_NATNUM (size);
  return make_lisp_hashtable (XINT (size), HASHTABLE_WEAK,
			      decode_hashtable_test_fun (test_fun));
}

DEFUN ("make-key-weak-hashtable", Fmake_key_weak_hashtable, 1, 2, 0, /*
Make a key-weak hashtable of initial size SIZE.
A key-weak hashtable is similar to a fully-weak hashtable (see
`make-weak-hashtable') except that a key-value pair will be removed
only if the key remains unmarked outside of weak hashtables.  The pair
will remain in the hashtable if the key is pointed to by something other
than a weak hashtable, even if the value is not.
*/
       (size, test_fun))
{
  CHECK_NATNUM (size);
  return make_lisp_hashtable (XINT (size), HASHTABLE_KEY_WEAK,
			      decode_hashtable_test_fun (test_fun));
}

DEFUN ("make-value-weak-hashtable", Fmake_value_weak_hashtable, 1, 2, 0, /*
Make a value-weak hashtable of initial size SIZE.
A value-weak hashtable is similar to a fully-weak hashtable (see
`make-weak-hashtable') except that a key-value pair will be removed only
if the value remains unmarked outside of weak hashtables.  The pair will
remain in the hashtable if the value is pointed to by something other
than a weak hashtable, even if the key is not.
*/
       (size, test_fun))
{
  CHECK_NATNUM (size);
  return make_lisp_hashtable (XINT (size), HASHTABLE_VALUE_WEAK,
			      decode_hashtable_test_fun (test_fun));
}

struct marking_closure
{
  int (*obj_marked_p) (Lisp_Object);
  void (*markobj) (Lisp_Object);
  enum hashtable_type type;
  int did_mark;
};

static void
marking_mapper (CONST void *key, void *contents, void *closure)
{
  Lisp_Object keytem, valuetem;
  struct marking_closure *fmh =
    (struct marking_closure *) closure;

  /* This function is called over each pair in the hashtable.
     We complete the marking for semi-weak hashtables. */
  CVOID_TO_LISP (keytem, key);
  CVOID_TO_LISP (valuetem, contents);

  switch (fmh->type)
    {
    case HASHTABLE_KEY_WEAK:
      if ((fmh->obj_marked_p) (keytem) &&
	  !(fmh->obj_marked_p) (valuetem))
	{
	  (fmh->markobj) (valuetem);
	  fmh->did_mark = 1;
	}
      break;

    case HASHTABLE_VALUE_WEAK:
      if ((fmh->obj_marked_p) (valuetem) &&
	  !(fmh->obj_marked_p) (keytem))
	{
	  (fmh->markobj) (keytem);
	  fmh->did_mark = 1;
	}
      break;

    case HASHTABLE_KEY_CAR_WEAK:
      if (!CONSP (keytem) || (fmh->obj_marked_p) (XCAR (keytem)))
	{
	  if (!(fmh->obj_marked_p) (keytem))
	    {
	      (fmh->markobj) (keytem);
	      fmh->did_mark = 1;
	    }
	  if (!(fmh->obj_marked_p) (valuetem))
	    {
	      (fmh->markobj) (valuetem);
	      fmh->did_mark = 1;
	    }
	}
      break;

    case HASHTABLE_VALUE_CAR_WEAK:
      if (!CONSP (valuetem) || (fmh->obj_marked_p) (XCAR (valuetem)))
	{
	  if (!(fmh->obj_marked_p) (keytem))
	    {
	      (fmh->markobj) (keytem);
	      fmh->did_mark = 1;
	    }
	  if (!(fmh->obj_marked_p) (valuetem))
	    {
	      (fmh->markobj) (valuetem);
	      fmh->did_mark = 1;
	    }
	}
      break;

    default:
      abort (); /* Huh? */
    }

  return;
}

int
finish_marking_weak_hashtables (int (*obj_marked_p) (Lisp_Object),
				void (*markobj) (Lisp_Object))
{
  Lisp_Object rest;
  int did_mark = 0;

  for (rest = Vall_weak_hashtables;
       !GC_NILP (rest);
       rest = XHASHTABLE (rest)->next_weak)
    {
      enum hashtable_type type;

      if (! ((*obj_marked_p) (rest)))
	/* The hashtable is probably garbage.  Ignore it. */
	continue;
      type = XHASHTABLE (rest)->type;
      if (type == HASHTABLE_KEY_WEAK     ||
	  type == HASHTABLE_VALUE_WEAK   ||
	  type == HASHTABLE_KEY_CAR_WEAK ||
	  type == HASHTABLE_VALUE_CAR_WEAK)
	{
          struct marking_closure fmh;

          fmh.obj_marked_p = obj_marked_p;
	  fmh.markobj = markobj;
	  fmh.type = type;
	  fmh.did_mark = 0;
	  /* Now, scan over all the pairs.  For all pairs that are
	     half-marked, we may need to mark the other half if we're
	     keeping this pair. */
	  elisp_maphash (marking_mapper, rest, &fmh);
	  if (fmh.did_mark)
	    did_mark = 1;
	}

      /* #### If alloc.c mark_object changes, this must change also... */
      {
	/* Now mark the vector itself.  (We don't need to call markobj
	   here because we know that everything *in* it is already marked,
	   we just need to prevent the vector itself from disappearing.)
	   (The remhash above has taken care of zero_entry.)
	   */
	struct Lisp_Vector *ptr = XVECTOR (XHASHTABLE (rest)->harray);
	int len = vector_length (ptr);
	if (len >= 0)
	  {
	    ptr->size = -1 - len;
	    did_mark = 1;
	  }
	/* else it's already marked (remember, this function is iterated
	   until marking stops) */
      }
    }

  return did_mark;
}

struct pruning_closure
{
  int (*obj_marked_p) (Lisp_Object);
};

static int
pruning_mapper (CONST void *key, CONST void *contents, void *closure)
{
  Lisp_Object keytem, valuetem;
  struct pruning_closure *fmh = (struct pruning_closure *) closure;

  /* This function is called over each pair in the hashtable.
     We remove the pairs that aren't completely marked (everything
     that is going to stay ought to have been marked already
     by the finish_marking stage). */
  CVOID_TO_LISP (keytem, key);
  CVOID_TO_LISP (valuetem, contents);

  return ! ((*fmh->obj_marked_p) (keytem) &&
	    (*fmh->obj_marked_p) (valuetem));
}

void
prune_weak_hashtables (int (*obj_marked_p) (Lisp_Object))
{
  Lisp_Object rest, prev = Qnil;
  for (rest = Vall_weak_hashtables;
       !GC_NILP (rest);
       rest = XHASHTABLE (rest)->next_weak)
    {
      if (! ((*obj_marked_p) (rest)))
	{
	  /* This table itself is garbage.  Remove it from the list. */
	  if (GC_NILP (prev))
	    Vall_weak_hashtables = XHASHTABLE (rest)->next_weak;
	  else
	    XHASHTABLE (prev)->next_weak = XHASHTABLE (rest)->next_weak;
	}
      else
	{
          struct pruning_closure fmh;
          fmh.obj_marked_p = obj_marked_p;
	  /* Now, scan over all the pairs.  Remove all of the pairs
	     in which the key or value, or both, is unmarked
	     (depending on the type of weak hashtable). */
	  elisp_map_remhash (pruning_mapper, rest, &fmh);
	  prev = rest;
	}
    }
}

/* Return a hash value for an array of Lisp_Objects of size SIZE. */

unsigned long
internal_array_hash (Lisp_Object *arr, int size, int depth)
{
  int i;
  unsigned long hash = 0;

  if (size <= 5)
    {
      for (i = 0; i < size; i++)
	hash = HASH2 (hash, internal_hash (arr[i], depth + 1));
      return hash;
    }

  /* just pick five elements scattered throughout the array.
     A slightly better approach would be to offset by some
     noise factor from the points chosen below. */
  for (i = 0; i < 5; i++)
    hash = HASH2 (hash, internal_hash (arr[i*size/5], depth + 1));

  return hash;
}

/* Return a hash value for a Lisp_Object.  This is for use when hashing
   objects with the comparison being `equal' (for `eq', you can just
   use the Lisp_Object itself as the hash value).  You need to make a
   tradeoff between the speed of the hash function and how good the
   hashing is.  In particular, the hash function needs to be FAST,
   so you can't just traipse down the whole tree hashing everything
   together.  Most of the time, objects will differ in the first
   few elements you hash.  Thus, we only go to a short depth (5)
   and only hash at most 5 elements out of a vector.  Theoretically
   we could still take 5^5 time (a big big number) to compute a
   hash, but practically this won't ever happen. */

unsigned long
internal_hash (Lisp_Object obj, int depth)
{
  if (depth > 5)
    return 0;
  if (CONSP (obj))
    {
      /* no point in worrying about tail recursion, since we're not
	 going very deep */
      return HASH2 (internal_hash (XCAR (obj), depth + 1),
		    internal_hash (XCDR (obj), depth + 1));
    }
  else if (STRINGP (obj))
    return hash_string (XSTRING_DATA (obj), XSTRING_LENGTH (obj));
#ifndef LRECORD_VECTOR
  else if (VECTORP (obj))
    {
      struct Lisp_Vector *v = XVECTOR (obj);
      return HASH2 (vector_length (v),
		    internal_array_hash (v->contents, vector_length (v),
					 depth + 1));
    }
#endif /* !LRECORD_VECTOR */
  else if (LRECORDP (obj))
    {
      CONST struct lrecord_implementation
	*imp = XRECORD_LHEADER (obj)->implementation;
      if (imp->hash)
	return (imp->hash) (obj, depth);
    }

  return LISP_HASH (obj);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_elhash (void)
{
  DEFSUBR (Fmake_hashtable);
  DEFSUBR (Fcopy_hashtable);
  DEFSUBR (Fhashtablep);
  DEFSUBR (Fgethash);
  DEFSUBR (Fputhash);
  DEFSUBR (Fremhash);
  DEFSUBR (Fclrhash);
  DEFSUBR (Fmaphash);
  DEFSUBR (Fhashtable_fullness);
  DEFSUBR (Fmake_weak_hashtable);
  DEFSUBR (Fmake_key_weak_hashtable);
  DEFSUBR (Fmake_value_weak_hashtable);
  defsymbol (&Qhashtablep, "hashtablep");
}

void
vars_of_elhash (void)
{
  /* This must NOT be staticpro'd */
  Vall_weak_hashtables = Qnil;
}
