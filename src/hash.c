/* Hash tables.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 2003, 2004 Ben Wing.

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

/* Author: Lost in the mists of history.  At least back to Lucid 19.3,
   circa Sep 1992. */

#include <config.h>
#include "lisp.h"
#include "hash.h"

#define NULL_ENTRY ((void *) 0xdeadbeef) /* -559038737 base 10 */

#define COMFORTABLE_SIZE(size) (21 * (size) / 16)

#define KEYS_DIFFER_P(old, new_, testfun) \
  (((old) != (new_)) && (!(testfun) || !(testfun) ((old),(new_))))

static void rehash (hentry *harray, struct hash_table *ht, Elemcount size);

Hashcode
memory_hash (const void *xv, Bytecount size)
{
  Hashcode h = 0;
  unsigned const char *x = (unsigned const char *) xv;

  if (!x) return 0;

  while (size--)
    {
      Hashcode g;
      h = (h << 4) + *x++;
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }

  return h;
}

static int
string_equal (const void *st1, const void *st2)
{
  if (!st1)
    return st2 ? 0 : 1;
  else if (!st2)
    return 0;
  else
    return !strcmp ((const char *) st1, (const char *) st2);
}

static Hashcode
string_hash (const void *xv)
{
  Hashcode h = 0;
  unsigned const char *x = (unsigned const char *) xv;

  if (!x) return 0;

  while (*x)
    {
      Hashcode g;
      h = (h << 4) + *x++;
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }

  return h;
}

/* Return a suitable size for a hash table, with at least SIZE slots. */
static Elemcount
hash_table_size (Elemcount requested_size)
{
  /* Return some prime near, but greater than or equal to, SIZE.
     Decades from the time of writing, someone will have a system large
     enough that the list below will be too short... */
  static const Elemcount primes [] =
  {
    19, 29, 41, 59, 79, 107, 149, 197, 263, 347, 457, 599, 787, 1031,
    1361, 1777, 2333, 3037, 3967, 5167, 6719, 8737, 11369, 14783,
    19219, 24989, 32491, 42257, 54941, 71429, 92861, 120721, 156941,
    204047, 265271, 344857, 448321, 582821, 757693, 985003, 1280519,
    1664681, 2164111, 2813353, 3657361, 4754591, 6180989, 8035301,
    10445899, 13579681, 17653589, 22949669, 29834603, 38784989,
    50420551, 65546729, 85210757, 110774011, 144006217, 187208107,
    243370577, 316381771, 411296309, 534685237, 695090819, 903618083,
    1174703521, 1527114613, 1985248999 /* , 2580823717UL, 3355070839UL */
  };
  /* We've heard of binary search. */
  int low, high;
  for (low = 0, high = countof (primes) - 1; high - low > 1;)
    {
      /* Loop Invariant: size < primes [high] */
      int mid = (low + high) / 2;
      if (primes [mid] < requested_size)
	low = mid;
      else
	high = mid;
    }
  return primes [high];
}

const void *
gethash (const void *key, struct hash_table *hash_table, const void **ret_value)
{
  if (!key)
    {
      *ret_value = hash_table->zero_entry;
      return (void *) hash_table->zero_set;
    }
  else
    {
      hentry *harray = hash_table->harray;
      hash_table_test_function test_function = hash_table->test_function;
      Elemcount size = hash_table->size;
      Hashcode hcode_initial =
	hash_table->hash_function ?
	hash_table->hash_function (key) :
	(Hashcode) key;
      Elemcount hcode = (Elemcount) (hcode_initial % size);
      hentry *e = &harray [hcode];
      const void *e_key = e->key;

      if (e_key ?
	  KEYS_DIFFER_P (e_key, key, test_function) :
	  e->contents == NULL_ENTRY)
	{
	  Elemcount h2 = size - 2;
	  Elemcount incr = (Elemcount) (1 + (hcode_initial % h2));
	  do
	    {
	      hcode += incr; if (hcode >= size) hcode -= size;
	      e = &harray [hcode];
	      e_key = e->key;
	    }
	  while (e_key ?
		 KEYS_DIFFER_P (e_key, key, test_function) :
		 e->contents == NULL_ENTRY);
	}

      *ret_value = e->contents;
      return e->key;
    }
}

void
clrhash (struct hash_table *hash_table)
{
  memset (hash_table->harray, 0, sizeof (hentry) * hash_table->size);
  hash_table->zero_entry = 0;
  hash_table->zero_set   = 0;
  hash_table->fullness   = 0;
}

void
free_hash_table (struct hash_table *hash_table)
{
  xfree (hash_table->harray, hentry *);
  xfree (hash_table, struct hash_table *);
}

struct hash_table *
make_hash_table (Elemcount size)
{
  struct hash_table *hash_table = xnew_and_zero (struct hash_table);
  hash_table->size = hash_table_size (COMFORTABLE_SIZE (size));
  hash_table->harray = xnew_array (hentry, hash_table->size);
  clrhash (hash_table);
  return hash_table;
}

struct hash_table *
make_string_hash_table (Elemcount size)
{
  return make_general_hash_table (size, string_hash, string_equal);
}

struct hash_table *
make_general_hash_table (Elemcount size,
			hash_table_hash_function hash_function,
			hash_table_test_function test_function)
{
  struct hash_table* hash_table = make_hash_table (size);
  hash_table->hash_function = hash_function;
  hash_table->test_function = test_function;
  return hash_table;
}

static void
grow_hash_table (struct hash_table *hash_table, Elemcount new_size)
{
  Elemcount old_size   = hash_table->size;
  hentry     *old_harray = hash_table->harray;

  hash_table->size   = hash_table_size (new_size);
  hash_table->harray = xnew_array (hentry, hash_table->size);

  /* do the rehash on the "grown" table */
  {
    long old_zero_set    = hash_table->zero_set;
    void *old_zero_entry = hash_table->zero_entry;
    clrhash (hash_table);
    hash_table->zero_set   = old_zero_set;
    hash_table->zero_entry = old_zero_entry;
    rehash (old_harray, hash_table, old_size);
  }

  xfree (old_harray, hentry *);
}

void
pregrow_hash_table_if_necessary (struct hash_table *hash_table,
				 Elemcount breathing_room)
{
  Elemcount comfortable_size = COMFORTABLE_SIZE (hash_table->fullness);
  if (hash_table->size < comfortable_size - breathing_room)
    grow_hash_table (hash_table, comfortable_size + 1);
}

void
puthash (const void *key, void *contents, struct hash_table *hash_table)
{
  if (!key)
    {
      hash_table->zero_entry = contents;
      hash_table->zero_set = 1;
    }
  else
    {
      hash_table_test_function test_function = hash_table->test_function;
      Elemcount size = hash_table->size;
      hentry *harray   = hash_table->harray;
      Hashcode hcode_initial =
	hash_table->hash_function ?
	hash_table->hash_function (key) :
	(Hashcode) key;
      Elemcount hcode = (Elemcount) (hcode_initial % size);
      Elemcount h2 = size - 2;
      Elemcount incr = (Elemcount) (1 + (hcode_initial % h2));
      const void *e_key = harray [hcode].key;
      const void *oldcontents;

      if (e_key && KEYS_DIFFER_P (e_key, key, test_function))
	{
	  do
	    {
	      hcode += incr; if (hcode >= size) hcode -= size;
	      e_key = harray [hcode].key;
	    }
	  while (e_key && KEYS_DIFFER_P (e_key, key, test_function));
	}
      oldcontents = harray [hcode].contents;
      harray [hcode].key = key;
      harray [hcode].contents = contents;
      /* If the entry that we used was a deleted entry,
	 check for a non deleted entry of the same key,
	 then delete it. */
      if (!e_key && oldcontents == NULL_ENTRY)
	{
	  hentry *e;

	  do
	    {
	      hcode += incr; if (hcode >= size) hcode -= size;
	      e = &harray [hcode];
	      e_key = e->key;
	    }
	  while (e_key ?
		 KEYS_DIFFER_P (e_key, key, test_function):
		 e->contents == NULL_ENTRY);

	  if (e_key)
	    {
	      e->key = 0;
	      e->contents = NULL_ENTRY;
	    }
	}

      /* only increment the fullness when we used up a new hentry */
      if (!e_key || KEYS_DIFFER_P (e_key, key, test_function))
	{
	  Elemcount comfortable_size = COMFORTABLE_SIZE (++(hash_table->fullness));
	  if (hash_table->size < comfortable_size)
	    grow_hash_table (hash_table, comfortable_size + 1);
	}
    }
}

static void
rehash (hentry *harray, struct hash_table *hash_table, Elemcount size)
{
  hentry *limit = harray + size;
  hentry *e;
  for (e = harray; e < limit; e++)
    {
      if (e->key)
	puthash (e->key, e->contents, hash_table);
    }
}

void
remhash (const void *key, struct hash_table *hash_table)
{
  if (!key)
    {
      hash_table->zero_entry = 0;
      hash_table->zero_set = 0;
    }
  else
    {
      hentry *harray = hash_table->harray;
      hash_table_test_function test_function = hash_table->test_function;
      Elemcount size = hash_table->size;
      Hashcode hcode_initial =
	(hash_table->hash_function) ?
	(hash_table->hash_function (key)) :
	((Hashcode) key);
      Elemcount hcode = (Elemcount) (hcode_initial % size);
      hentry *e = &harray [hcode];
      const void *e_key = e->key;

      if (e_key ?
	  KEYS_DIFFER_P (e_key, key, test_function) :
	  e->contents == NULL_ENTRY)
	{
	  Elemcount h2 = size - 2;
	  Elemcount incr = (Elemcount) (1 + (hcode_initial % h2));
	  do
	    {
	      hcode += incr; if (hcode >= size) hcode -= size;
	      e = &harray [hcode];
	      e_key = e->key;
	    }
	  while (e_key?
		 KEYS_DIFFER_P (e_key, key, test_function):
		 e->contents == NULL_ENTRY);
	}
      if (e_key)
	{
	  e->key = 0;
	  e->contents = NULL_ENTRY;
	  /* Note: you can't do fullness-- here, it breaks the world. */
	}
    }
}

void
maphash (maphash_function mf, struct hash_table *hash_table, void *arg)
{
  hentry *e;
  hentry *limit;

  if (hash_table->zero_set)
    {
      if (mf (0, hash_table->zero_entry, arg))
	return;
    }

  for (e = hash_table->harray, limit = e + hash_table->size; e < limit; e++)
    {
      if (e->key && mf (e->key, e->contents, arg))
	return;
    }
}

void
map_remhash (remhash_predicate predicate, struct hash_table *hash_table, void *arg)
{
  hentry *e;
  hentry *limit;

  if (hash_table->zero_set && predicate (0, hash_table->zero_entry, arg))
    {
      hash_table->zero_set = 0;
      hash_table->zero_entry = 0;
    }

  for (e = hash_table->harray, limit = e + hash_table->size; e < limit; e++)
    if (predicate (e->key, e->contents, arg))
      {
        e->key = 0;
        e->contents = NULL_ENTRY;
      }
}
