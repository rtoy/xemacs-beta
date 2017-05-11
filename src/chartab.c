/* XEmacs routines to deal with char tables.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002, 2003, 2005, 2010 Ben Wing.
   Copyright (C) 1995, 1997, 1999 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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

/* Synched up with: Mule 2.3.  Not synched with FSF.

   This file was written independently of the FSF implementation,
   and is not compatible. */

/* Authorship:

   Ben Wing: wrote, for 19.13 (Mule).  Some category table stuff
             loosely based on the original Mule.
   Jareth Hein: fixed a couple of bugs in the implementation, and
   	     added regex support for categories with check_category_at
   Ben Wing: Drastic rewrite, October 2005, for Unicode-internal support.
             The old implementation used an ugly system indexed by charset
             ID, with `char-table-entry' objects.  The implementation of
             map_char_table() was long and nasty.  The new system uses page
             tables, as in unicode.c.
   Ben Wing: Redo category tables for improved memory usage, March 2010.
             Change from a system that used 96-bit bit vectors listing the
             category membership for each character, stored in a generic
             char table, to a special `category-table' object with 12
             subtables, each directly storing an 8-bit bit array in place
             of the Lisp_Object pointer.
   Ben Wing: Redo char tables and category tables again, March 2010.
             Now category tables have one char table per bit, so we have
             95 of them per category table.  At level 1, instead of a
             category-subtable, we have a 256-element bit vector.  Char tables
             also now can efficiently handle ranges.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "chartab.h"
#include "syntax.h"


Lisp_Object Qchar_tablep, Qchar_table;

Lisp_Object Vall_syntax_tables;

#ifdef MULE
Lisp_Object Qcategory_tablep;
Lisp_Object Qcategory_designator_p;

Lisp_Object Vstandard_category_table;

/* Variables to determine word boundary.  */
Lisp_Object Vword_combining_categories, Vword_separating_categories;
#endif /* MULE */

#ifdef MEMORY_USAGE_STATS
Lisp_Object Qpage_tables;
#endif

static int check_valid_char_table_value (Lisp_Object value,
					 enum char_table_type type,
			                 Error_Behavior errb);

/* For a single entry at the specified level, how many characters does it
   span?  Or more correctly, if the span is described as a closed interval
   [0, X], what is X? */
static Ichar chartab_span_top[5];


/* A char table maps from characters to values.

   We used to use a complicated structure that allowed certain types of
   ranges (all characters in a charset or all characters in a particular
   row of a charset, where a "row" means all characters with the same first
   octet) to be directly assigned values.  With the change to Unicode as an
   internal representation, it no longer makes sense to special-case for
   charsets, and so the implementation was changed to use page tables,
   similarly to how Unicode conversion maps are handled.

   Implementation is as follows:

   There are up to four levels, corresponding to the four bytes in a 32-bit
   character.  Each level is implemented by a char-subtable, which wraps a
   size-256 array of Lisp_Object pointers.  The value of each object in the
   array can either be another char-subtable, corresponding to the next
   level down, or some other value, indicating that all characters spanned
   by this entry have the same value.  A char-table contains a pointer to
   the level-4 char-subtable, corresponding to the most-significant byte
   (the highest 8 bits in the character).

   The actual number of levels used by the char table may be less than 4.
   For example, if no characters above 0xFFFFFF are given values, there
   will be at most 3 levels.  If no characters above 0xFFFF are given
   values, there will be at most 2 levels, etc.

   If MAXIMIZE_CHAR_TABLE_DEPTH is set, char tables always use the maximum
   number of levels.  This is currently the case in non-Mule, where there
   is only one level anyway.

   Category tables use a special type of char table, called a "category
   char table".  A category table is its own type of object, containing up
   to 95 category char tables, one per category.  At level 1 of a category
   char table, instead of there being a char-subtable corresponding to the
   lowest 8 bits of a character, there is a 256-element bit vector, with
   the bits specifying whether the 256 characters in the range spanned by
   the bit vector belong or don't belong to the category.

   The possible values assignable to a character vary depending on the type
   of table.  At one extreme `generic' char tables allow any type of object
   to be assigned (except Qunbound and `char-subtable' objects, which are
   internal objects that shouldn't escape to the user level anyway).  At
   the other extreme, category tables only allow two possible values to be
   assigned, indicating membership or nonmembership in the category.  

   Another implementation possibility would be to use range tables.  I
   think GNU Emacs allows char tables of both kinds (page tables and range
   tables), or something similar, although I don't know how it chooses one
   or the other.

   Char tables are used to implement the following types of tables, among
   others:

   1) category tables
   2) syntax tables
   3) display tables
   4) case tables
   */

static int
print_preprocess_mapper (Lisp_Object UNUSED (table), Ichar UNUSED (from),
                         Ichar UNUSED (to), Lisp_Object val, void *extra_arg)
{
  print_preprocess (val, ((preprocess_info_t *) extra_arg)->table,
                    ((preprocess_info_t *) extra_arg)->count);
  return 0;
}

static void
char_table_print_preprocess (Lisp_Object object, Lisp_Object print_number_table,
                             Elemcount *seen_object_count)
{
  struct chartab_range ctr = { CHARTAB_RANGE_ALL, 0, 0, Qnil, 0 };
  preprocess_info_t preprocess_info = { print_number_table, seen_object_count };
  map_char_table (object, &ctr, print_preprocess_mapper, &preprocess_info);
}

static int
nsubst_structures_mapper (Lisp_Object table, Ichar from, Ichar  to,
                          Lisp_Object value, void *extra_arg)
{
  Lisp_Object number_table
    = ((nsubst_structures_info_t *) extra_arg)->number_table;
  Lisp_Object new_ = ((nsubst_structures_info_t *) extra_arg)->new_;
  Lisp_Object old = ((nsubst_structures_info_t *) extra_arg)->old;
  Boolint test_not_unboundp
    = ((nsubst_structures_info_t *) extra_arg)->test_not_unboundp;

  if (EQ (old, value) == test_not_unboundp)
    {
      put_char_table (table, from, to, new_);
    }
  else if (LRECORDP (value) &&
           HAS_OBJECT_METH_P (value, nsubst_structures_descend))
    {
      nsubst_structures_descend (new_, old, value, number_table,
                                 test_not_unboundp);
    }

  return 0;
}

static void
char_table_nsubst_structures_descend (Lisp_Object new_, Lisp_Object old,
                                      Lisp_Object object,
                                      Lisp_Object number_table,
                                      Boolint test_not_unboundp)
{
  struct chartab_range ctr = { CHARTAB_RANGE_ALL, 0, 0, Qnil, 0 };
  nsubst_structures_info_t nsubst_structures_info
    = { number_table, new_, old, object, test_not_unboundp };

  map_char_table (object, &ctr, nsubst_structures_mapper,
                  &nsubst_structures_info);
}

/************************************************************************/
/*                         Char Table subtables                         */
/************************************************************************/

/* We use the same code from unicode.c.
   Code duplication is generally a bad thing, but there isn't that much total
   code and there are a lot of differences.  I originally tried abstracting
   using preprocessing, but it got real ugly real fast.  This is even more
   the case now that char tables can use Lisp objects for their subtables. */

static const struct memory_description char_subtable_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Char_Subtable, ptr), 256 }, 
  { XD_END }
};

static Lisp_Object
mark_char_subtable (Lisp_Object obj)
{
  int i;

  for (i = 1; i < 256; i++)
    mark_object (XCHAR_SUBTABLE (obj)->ptr[i]);

  return XCHAR_SUBTABLE (obj)->ptr[0];
}

DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("char-subtable", char_subtable,
				      mark_char_subtable,
				      char_subtable_description,
				      Lisp_Char_Subtable);


/************************************************************************/
/*                      Char table implementation                       */
/************************************************************************/

#ifdef ERROR_CHECK_STRUCTURES

static void
check_chartab_invariants (Lisp_Object table, int level, int catp)
{
  assert (level >= 0 && level <= 4);
  /* No subtables at the lowest level */
  if (level == 0)
    assert (!CHAR_SUBTABLEP (table));
  if (catp)
    {
      /* Category tables have a bit vector instead of a char subtable
	 at level 1, and the only possible non-table values are Qone
	 or Qunbound */
      if (level == 1 && BIT_VECTORP (table))
	return;
      if (level > 1 && CHAR_SUBTABLEP (table))
	return;
      assert (ONEP (table) || UNBOUNDP (table));
    }
}

#else

#define check_chartab_invariants(table, level, catp) DO_NOTHING

#endif

static Lisp_Object
clone_chartab_table (Lisp_Object table, int level, int catp)
{
  Lisp_Object newtab;
  Bytecount size;

  check_chartab_invariants (table, level, catp);

  if (!CHAR_SUBTABLEP (table))
    {
      if (catp && BIT_VECTORP (table))
	return clone_bit_vector (table);
      return table;
    }

  size = sizeof (Lisp_Object);
  newtab = ALLOCATE_LEVEL_N_SUBTAB ();
  memcpy (SUBTAB_ARRAY_FROM_SUBTAB (newtab), SUBTAB_ARRAY_FROM_SUBTAB (table),
	  256 * size);

  {
    int i;
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (newtab);
    for (i = 0; i < 256; i++)
      tab[i] = clone_chartab_table (tab[i], level - 1, catp);
  }

  return newtab;
}

static Lisp_Object
create_new_chartab_table (int level, int catp, Lisp_Object set)
{
  Lisp_Object newtab;

  if (catp && level == 1)
    return ALLOCATE_LEVEL_1_CATEGORY_SUBTAB ();

  newtab = ALLOCATE_LEVEL_N_SUBTAB ();
  {
    int i;
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (newtab);
    for (i = 0; i < 256; i++)
      tab[i] = set;
  }

  return newtab;
}

static void
free_chartab_table (Lisp_Object table)
{
  if (!CHAR_SUBTABLEP (table))
    return;

  {
    int i;
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (table);

    for (i = 0; i < 256; i++)
      free_chartab_table (tab[i]);
  }

  FREE_ONE_SUBTAB (table);
}

#ifdef MEMORY_USAGE_STATS

/* #### Define better */

struct char_table_stats
{
  struct usage_stats u;
  Bytecount page_tables;
};

static Bytecount
compute_chartab_table_size_1 (Lisp_Object table, int catp)
{
  Bytecount size = 0;

  if (catp && BIT_VECTORP (table))
    return lisp_object_memory_usage (table);
  if (!CHAR_SUBTABLEP (table))
    return 0;
  
  {
    int i;
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
    for (i = 0; i < 256; i++)
      size += compute_chartab_table_size_1 (tab[i], catp);
  }

  size += lisp_object_memory_usage (table);
  return size;
}

static Bytecount
compute_chartab_table_size (Lisp_Object chartab, int catp)
{
  return (compute_chartab_table_size_1 (XCHAR_TABLE_TABLE (chartab), catp));
}

static void
compute_char_table_usage (Lisp_Object chartab, struct char_table_stats *stats)
{
  stats->page_tables +=
    compute_chartab_table_size (chartab, XCHAR_TABLE_CATEGORY_P (chartab));
}


static void
char_table_memory_usage (Lisp_Object char_table,
			 struct generic_usage_stats *gustats)
{
  struct char_table_stats *stats = (struct char_table_stats *) gustats;

  compute_char_table_usage (char_table, stats);
}

#endif /* MEMORY_USAGE_STATS */


/* Set all characters in the range [START, END] to VAL.  *TABLE is the
   table value at this level -- either an array of 256 elements, a bit
   vector of 256 elements (for category char tables, at level 1), or some
   other value, specifying that all characters spanned by this entry have
   that value.  Qunbound as a value means that the characters spanned by
   this entry all have no defined value.  LEVEL is the depth (1 - 4).
   OFFSET is the character offset corresponding to this table (essentially,
   the bits for all levels above this one -- e.g. if level == 3, offset
   will have bits 0-23 cleared and bits 24-31 set to the index of this
   table in the level-4 table, indicating the span of this entry). */

static void
put_chartab_table (Lisp_Object *table, int level, int catp,
		   int offset, Ichar start, Ichar end, Lisp_Object val)
{
  int i;
  int startind, endind;
  Lisp_Object *tab;

  check_chartab_invariants (*table, level, catp);

  /* Are we setting the entire spanned range?  If so, we don't need a
     table at this level.  If there was one previously, free it. */
  if (start <= offset && offset + chartab_span_top[level] <= end)
    {
      if (CHAR_SUBTABLEP (*table))
	free_chartab_table (*table);
      *table = val;
      return;
    }

  /* Else, we need a table. */

  /* Compute the start and end indices into the table */
  startind = max (0, (start - offset) >> ((level - 1) * 8));
  endind = min (255, (end - offset) >> ((level - 1) * 8));
  structure_checking_assert (startind <= 255);
  structure_checking_assert (endind >= 0);
  structure_checking_assert (startind <= endind);

  if (!SUBTAB_TABLE_P (*table, catp))
    *table = create_new_chartab_table (level, catp, *table);

  /* As usual, have to special-case for category tables.  If we see
     a bit vector, then necessarily we're at level 1. */
  if (BIT_VECTORP (*table))
    {
      int bitval = ONEP (val);
      structure_checking_assert (level == 1);
      for (i = startind; i <= endind; i++)
	set_bit_vector_bit (XBIT_VECTOR (*table), i, bitval);
      return;
    }

  tab = SUBTAB_ARRAY_FROM_SUBTAB (*table);

  /* Optimize the level 1 case.  We could leave this out and go through
     put_chartab_table() as normal, but we may as well write the simpler
     and faster code. */
  if (level == 1)
    {
      for (i = startind; i <= endind; i++)
	tab[i] = val;
      return;
    }

  for (i = startind; i <= endind; i++)
    {
      put_chartab_table (&tab[i], level - 1, catp,
			 offset + (i << ((level - 1) * 8)), start, end, val);
    }
}

void
put_char_table (Lisp_Object chartab, Ichar start, Ichar end, Lisp_Object val)
{
  /* 

  [[

  Another possibility for handling the setting of large ranges to a
  particular value: modifying the code that loops over a range to create
  shared subtables, similar to the current blank tables. (Then, we would
  need to implement reference-counting over the tables, to know when to
  free them, and copy-on-write semantics if the reference count is greater
  than one.  This would also obviate the need for special blank tables.
  Either we need to keep the reference count with the subtables themselves,
  which is convenient but potentially a bad idea since it makes the tables
  slightly larger than a power of 2 and hence difficult for the memory
  manager to handle efficiently, or we need to use a separate hash table to
  track the references.  This scheme is harder to implement and may make
  table updating slower compared to the Lisp-object scheme, but has the
  advantage that lookup is faster -- we don't need to do a bunch of if-then
  checks for each lookup.  The Lisp-object scheme also suffers from the
  same slightly-over-a-power-of-2 problem.)

  Shared tables we check for could potentially be specific to the particular
  char table; we'd keep track of the shared tables in the char-table object,
  and check to see if they are shared with the generic blank tables.)

  Maybe we should also allow for two different types of char tables, one
  that allows for semi-efficient handling of large ranges and one that doesn't
  (but is faster).  In such a case it might make sense for there to be a
  get_char_table() method pointer to avoid an if-check every time for the
  type.  Similarly if we allow the `always-maximize-table-size' option to
  be given.

  ]]

  -- This comment is out-of-date now.  It was written when the char table
  implementation was more similar to the Unicode page-table implementation.
  Currently we don't have blank tables any more and we do handle ranges
  efficiently.  But I'll leave the comment for the moment since it may have
  some useful stuff in it. --ben */

  int levels;
#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  int code_levels;
  int catp = XCHAR_TABLE_CATEGORY_P (chartab);
#endif

  /* DO NOT check to see whether START and END are valid Ichars.  They
     might not be (e.g. if we pass `t' to `put-char-table' so as to set
     all characters, this function gets called with ICHAR_MAX as the value
     of END.  Under old-Mule, that isn't valid.  We go ahead and ignore the
     fact that we may be setting values for invalid characters.  It won't
     be a problem when retrieving values either, since when retrieving we
     will always be passed valid Ichars.  Only when mapping do we have
     to worry, and then we find the nearest valid Ichar up or down. */


  levels = CHARTAB_LEVELS (XCHAR_TABLE_LEVELS (chartab));
  text_checking_assert (levels >= 1 && levels <= 4);

#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  GET_CHAR_LEVELS (end, code_levels);

  /* Make sure the chartab's tables have at least as many levels as
     the code point has. */
  if (levels < code_levels)
    {
      /* If nothing is currently set at all, or we're going to
	 replace everything that's been set, no point in expanding
	 out the tables because they'll just get replaced */
      if (!UNBOUNDP (XCHAR_TABLE_TABLE (chartab)) &&
	  !(start == 0 && end >= chartab_span_top[levels]))
	{
	  int i;

	  for (i = levels + 1; i <= code_levels; i++)
	    {
	      Lisp_Object old_table = XCHAR_TABLE_TABLE (chartab);
	      Lisp_Object table = create_new_chartab_table (i, catp, Qunbound);
	      XCHAR_TABLE_TABLE (chartab) = table;
	      SUBTAB_ARRAY_FROM_SUBTAB (table)[0] = old_table;
	    }
	}

      levels = code_levels;
      XCHAR_TABLE_LEVELS (chartab) = code_levels;
    }
#endif /* not MAXIMIZE_CHAR_TABLE_DEPTH */

  put_chartab_table (&XCHAR_TABLE_TABLE (chartab), levels,
		     XCHAR_TABLE_CATEGORY_P (chartab), 0, start, end, val);
}

struct map_chartab_range
{
  Ichar start, end;
  Lisp_Object val;
};

static inline int
really_call_map_chartab_fun (int (*fn) (Lisp_Object chartab, Ichar from,
					Ichar to, Lisp_Object val, void *arg),
			     Lisp_Object chartab, Ichar from, Ichar to,
			     Lisp_Object val, void *arg)
{
  from = round_up_to_valid_ichar (from);
  to = round_down_to_valid_ichar (to);
  if (from <= to && from >= 0)
    {
      int retval =
	(fn) (chartab, from, to, val, arg);
      if (retval)
	return retval;
    }
  return 0;
}

static inline int
call_map_chartab_fun (int (*fn) (Lisp_Object chartab, Ichar from, Ichar to,
				 Lisp_Object val, void *arg),
		      Lisp_Object chartab, Ichar from, Ichar to,
		      Lisp_Object val, void *arg,
		      struct map_chartab_range *range)
{
  if (range->start == -1)
    {
      range->start = from;
      range->end = to;
      range->val = val;
    }
  else if (range->end == from - 1 && EQ (range->val, val))
    range->end = to;
  else
    {
      int retval =
	really_call_map_chartab_fun (fn, chartab, range->start, range->end,
				     range->val, arg);
      if (retval)
	return retval;
      range->start = from;
      range->end = to;
      range->val = val;
    }
  return 0;
}

/* Map over all characters in the range [START, END].  TABLE is the table
   value at this level -- either an array of 256 elements, a bit vector of
   256 elements (for category char tables, at level 1), or some other
   value, specifying that all characters spanned by this entry have that
   value.  Qunbound as a value means that the characters spanned by this
   entry all have no defined value.  LEVEL is the depth (1 - 4).  OFFSET is
   the character offset corresponding to this table.  CHARTAB is the
   char-table object being mapped over.  RANGE is used to group together
   adjacent ranges into a single larger range.  The FN will be called with
   CHARTAB, the code of the character in question, its value, and the value
   of ARG.  Stops mapping the first time that FN returns non-zero, and
   returns that value.  Returns zero if mapping got all the way to the
   end. */

static int
map_chartab_table (Lisp_Object table, int level, int offset, Ichar start,
		   Ichar end, Lisp_Object chartab,
		   int (*fn) (Lisp_Object chartab, Ichar from, Ichar to,
			      Lisp_Object val, void *arg),
		   void *arg, struct map_chartab_range *range)
{
  int i;
  int startind, endind;
  int catp = XCHAR_TABLE_CATEGORY_P (chartab);

  check_chartab_invariants (table, level, catp);

  if (UNBOUNDP (table))
    return 0;

  if (!SUBTAB_TABLE_P (table, catp))
    {
      Ichar from, to;

      if (level == 0)
	{
	  structure_checking_assert (start <= offset);
	  structure_checking_assert (offset <= end);
	}
      from = max (start, offset);
      to = min (end, offset + chartab_span_top[level]);
      return call_map_chartab_fun (fn, chartab, from, to, table, arg, range);
    }


  startind = max (0, (start - offset) >> ((level - 1) * 8));
  endind = min (255, (end - offset) >> ((level - 1) * 8));
  structure_checking_assert (startind <= 255);
  structure_checking_assert (endind >= 0);
  structure_checking_assert (startind <= endind);
  
  if (BIT_VECTORP (table))
    {
      for (i = startind; i <= endind; i++)
	{
	  if (bit_vector_bit (XBIT_VECTOR (table), i) &&
	      valid_ichar_p (offset + i))
	    {
	      int retval = call_map_chartab_fun (fn, chartab, offset + i,
						 offset + i, Qone, arg, range);
	      if (retval)
		return retval;
	    }
	}
      return 0;
    }

  {
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
    for (i = startind; i <= endind; i++)
      {
	int retval =
	  map_chartab_table (tab[i], level - 1,
			     offset + (i << ((level - 1) * 8)),
			     start, end, chartab, fn, arg, range);
	if (retval)
	  return retval;
      }
  }

  return 0;
}

/* Call map_chartab_table().  Then, if there's a range still needing
   to have FN called on, do it. */

static int
map_chartab_table_0 (Lisp_Object table, int level, int offset, Ichar start,
		     Ichar end, Lisp_Object chartab,
		     int (*fn) (Lisp_Object chartab, Ichar from, Ichar to,
				Lisp_Object val, void *arg),
		     void *arg, struct map_chartab_range *range)
{
  int retval = map_chartab_table (table, level, offset, start, end,
				  chartab, fn, arg, range);
  if (retval)
    return retval;
  if (range->start != -1)
    return really_call_map_chartab_fun (fn, chartab, range->start, range->end,
					range->val, arg);
  return 0;
}


/* Check whether the given table is entirely blank.  TABLE is LEVEL levels
   deep.  Start checking at START (this will normally be 1, since we don't
   want to check the 0th level, which indexes the possibly non-blank
   lower levels.
   */

static int
check_if_blank (Lisp_Object table, int level, int start, int depth, int catp)
{
  int i;

  check_chartab_invariants (table, level, catp);
  if (UNBOUNDP (table))
    return 1;

  if (catp && BIT_VECTORP (table))
    {
      for (i = start; i < 256; i++)
	if (bit_vector_bit (XBIT_VECTOR (table), i))
	  return 0;
      return 1;
    }

  if (!CHAR_SUBTABLEP (table))
    return 0;

  {
    Lisp_Object *tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
    for (i = start; i < 256; i++)
      if (!check_if_blank (tab[i], level - 1, 0, depth, catp))
	return 0;
    return 1;
  }
}

static int
chartab_tables_equal (Lisp_Object table1, Lisp_Object table2, int level,
		      int depth, int foldcase, int catp)
{
  int i;

  check_chartab_invariants (table1, level, catp);
  check_chartab_invariants (table2, level, catp);

  /* Things are made tricky by the fact that one of the values may be a
     non-table value that applies to all characters spanned, and the other
     may be a table all of whose members are EQ to that same value (or some
     of whose members are sub-tables whose members in turn are EQ to that
     same value, etc.), in which case the tables are equal. */

  if (catp && level == 1)
    {
      int table2_val;

      /* The level-1 values will be either integers or bit vectors.  We
	 have to handle each possible combination in turn. */
      if (!BIT_VECTORP (table1) && !BIT_VECTORP (table2))
	return EQ (table1, table2);
      if (BIT_VECTORP (table1) && BIT_VECTORP (table2))
	return internal_equal (table1, table2, depth + 1);
      /* If one is a bit vector and the other one isn't, make sure
	 the bit vector comes first. */
      if (!BIT_VECTORP (table1))
	{
	  Lisp_Object tmp = table1;
	  table1 = table2;
	  table2 = tmp;
	}
      table2_val = ONEP (table2);
      for (i = 0; i < 256; i++)
	if (bit_vector_bit (XBIT_VECTOR (table1), i) != table2_val)
	  return 0;
      return 1;
    }

  /* In other circumstances we are dealing with char subtables or
     non-subtable values (which can potentially be anything). */

  if (!CHAR_SUBTABLEP (table1) && !CHAR_SUBTABLEP (table2))
    return internal_equal_0 (table1, table2, depth + 1, foldcase);
  if (CHAR_SUBTABLEP (table1) && CHAR_SUBTABLEP (table2))
    {
      Lisp_Object *tab1 = SUBTAB_ARRAY_FROM_SUBTAB (table1);
      Lisp_Object *tab2 = SUBTAB_ARRAY_FROM_SUBTAB (table2);
      for (i = 0; i < 256; i++)
	if (!chartab_tables_equal (tab1[i], tab2[i], level - 1, depth,
				   foldcase, catp))
	  return 0;
      return 1;
    }

  /* If one is a table and the other one isn't, make sure the table comes
     first. */

  if (!CHAR_SUBTABLEP (table1))
    {
      Lisp_Object tmp = table1;
      table1 = table2;
      table2 = tmp;
    }
  {
    Lisp_Object *tab1 = SUBTAB_ARRAY_FROM_SUBTAB (table1);
    for (i = 0; i < 256; i++)
      if (!chartab_tables_equal (tab1[i], table2, level - 1, depth,
				 foldcase, catp))
      return 0;
    return 1;
  }
}

static int
char_table_equal (Lisp_Object obj1, Lisp_Object obj2, int depth, int foldcase)
{
  /* NOTE:

     This code was formerly written so that it checks to see whether all
     entries are actually equal, whether or not they have the same default,
     by using the actual value of the char table entry, after the default
     had been supplied if necessary.  What we do now, which is considerably
     simpler, is just check the underlying entries, *before* applying the
     default.  Some things that are `equal' under the other scheme aren't
     `equal' under this scheme.  I think this makes more sense because
     objects that are `equal' should be identical in their behavior and
     have the same print representation; neither of these may be true in the
     former case.

     To speed things up, we should also keep track of the # of items
     currently set. */

  Lisp_Object table;
  int catp = XCHAR_TABLE_CATEGORY_P (obj1);

  if (XCHAR_TABLE_TYPE (obj1) != XCHAR_TABLE_TYPE (obj2))
    return 0;

  if (!internal_equal_0 (XCHAR_TABLE_DEFAULT (obj1),
			 XCHAR_TABLE_DEFAULT (obj2),
			 depth + 1, foldcase) ||
      !internal_equal_0 (XCHAR_TABLE_PARENT (obj1),
			 XCHAR_TABLE_PARENT (obj2),
			 depth + 1, foldcase))
    return 0;

  /* Switch if necessary so that obj1 always has >= # of levels of obj2 */
  if (XCHAR_TABLE_LEVELS (obj2) > XCHAR_TABLE_LEVELS (obj1))
    {
      Lisp_Object tmp = obj1;
      obj1 = obj2;
      obj2 = tmp;
    }

  table = XCHAR_TABLE_TABLE (obj1);
  /* If one table has more levels than the other, make sure the extra
     levels are all blank.  Successively drill down the tables,
     checking that all subtables except #0 are completely blank (including
     recursively checking any sub-subtables of them). */
  if (XCHAR_TABLE_LEVELS (obj1) > XCHAR_TABLE_LEVELS (obj2))
    {
      int i;
      for (i = XCHAR_TABLE_LEVELS (obj1); i > XCHAR_TABLE_LEVELS (obj2); i--)
	{
	  if (!check_if_blank (table, i, 1, depth, catp))
	    return 0;
	  if (!CHAR_SUBTABLEP (table))
	    {
	      assert (UNBOUNDP (table));
	      break;
	    }
	  table = SUBTAB_ARRAY_FROM_SUBTAB (table)[0];
	}
    }

  /* If we got this far, TABLE points to the appropriate (sub)table with the
     same number of levels as that of OBJ2. */
  return chartab_tables_equal (table,
			       XCHAR_TABLE_TABLE (obj2),
			       XCHAR_TABLE_LEVELS (obj2),
			       depth, foldcase, catp);
}

/* Characters likely to have case pairs or special syntax -- e.g. comment
   characters -- or unusual mappings in e.g. JIS-ROMAN, or in certain
   national character sets that map higher ASCII punctuation chars into
   extra letters (cf. the need for digraphs and trigraphs in C/C++) plus
   some random ones to boot; probably, ASCII chars are more likely to show
   up in char tables than others. */
static const Ascbyte *likely_test = "\t\n\r\f\016\025\0330128!@#$%^&*`'_+=-,.<>?;:/~()[]{}\\\"acehijlnortuxyzADEGIKMOQSVY";

static inline Hashcode
hash_raw_chartab_val (Ichar ch, Lisp_Object chartab, int depth, Boolint equalp)
{
  Lisp_Object val = get_char_table_raw (ch, chartab);
  return internal_hash (val, depth + 1, equalp);
}

static Hashcode
char_table_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  Hashcode hashval = HASH2 (XCHAR_TABLE_TYPE (obj),
			    internal_hash (XCHAR_TABLE_DEFAULT (obj),
					   depth + 1, equalp));
  const Ascbyte *p;
  Ichar ch;

  /* Hash those most likely to have values */
  for (p = likely_test; *p; p++)
    hashval = HASH2 (hashval, hash_raw_chartab_val ((Ichar) *p, obj, depth,
						    equalp));
  /* Hash some random Latin characters */
  for (ch = 130; ch <= 255; ch += 5)
    hashval = HASH2 (hashval, hash_raw_chartab_val (ch, obj, depth, equalp));
  /* Don't bother trying to hash higher stuff if there is none. */
  if (XCHAR_TABLE_LEVELS (obj) > 1)
    {
#ifdef UNICODE_INTERNAL
      /* #### We should really hash less in some of the higher realms but try
	 to get at least one value from each of the defined Unicode ranges.
	 Note that we cannot do charset lookups like we do below for old-Mule
	 because computed hash values for a particular object need to be the
	 same throughout the lifetime of the program, whereas they would
	 change if the Unicode-to-charset tables are changed. */
      /* Hash some random extended Latin characters */
      for (ch = 260; ch <= 500; ch += 10)
	hashval = HASH2 (hashval, hash_raw_chartab_val (ch, obj, depth,
							equalp));
      /* Hash some higher characters */
      for (ch = 500; ch <= 4000; ch += 50)
	hashval = HASH2 (hashval, hash_raw_chartab_val (ch, obj, depth,
							equalp));
      /* Hash some random CJK characters */
      for (ch = 0x4E00; ch <= 0x9FFF; ch += 791)
	hashval = HASH2 (hashval, hash_raw_chartab_val (ch, obj, depth,
							equalp));
      /* Hash some random Hangul characters */
      for (ch = 0xAC00; ch <= 0xD7AF; ch += 791)
	hashval = HASH2 (hashval, hash_raw_chartab_val (ch, obj, depth,
							equalp));
#elif defined (MULE)
/* 0xA1 is usually the first alphabetic character and differs across
   charsets, whereas 0xA0 is no-break-space across many of them.
   charset_codepoint_to_ichar_raw() can't fail because we are in non-
   Unicode-internal. */
#define FROB1(cs)							\
  hashval = HASH2 (hashval,						\
		   hash_raw_chartab_val (charset_codepoint_to_ichar_raw	\
                                         (cs, 0, 0xA1), obj, depth, equalp))
/* 0x3021 is the first CJK character in a number of different CJK charsets
   and differs across them. */
#define FROB2(cs)							\
  hashval = HASH2 (hashval,						\
		   hash_raw_chartab_val (charset_codepoint_to_ichar_raw	\
				         (cs, 0x30, 0x21), obj, depth, equalp))
      FROB1 (Vcharset_latin_iso8859_2);
      FROB1 (Vcharset_latin_iso8859_3);
      FROB1 (Vcharset_latin_iso8859_4);
      FROB1 (Vcharset_thai_tis620);
      FROB1 (Vcharset_arabic_iso8859_6);
      FROB1 (Vcharset_greek_iso8859_7);
      FROB1 (Vcharset_hebrew_iso8859_8);
      FROB1 (Vcharset_katakana_jisx0201);
      FROB1 (Vcharset_latin_jisx0201);
      FROB1 (Vcharset_cyrillic_iso8859_5);
      FROB1 (Vcharset_latin_iso8859_9);
      FROB1 (Vcharset_latin_iso8859_15);
      FROB1 (Vcharset_chinese_sisheng);
      FROB2 (Vcharset_japanese_jisx0208_1978);
      FROB2 (Vcharset_chinese_gb2312);
      FROB2 (Vcharset_japanese_jisx0208);
      FROB2 (Vcharset_korean_ksc5601);
      FROB2 (Vcharset_japanese_jisx0212);
      FROB2 (Vcharset_chinese_cns11643_1);
      FROB2 (Vcharset_chinese_cns11643_2);
      FROB2 (Vcharset_chinese_big5_1);
      FROB2 (Vcharset_chinese_big5_2);
#undef FROB1
#undef FROB2
#endif /* MULE */
    }
  return hashval;
}

static Lisp_Object
mark_char_table (Lisp_Object obj)
{
  mark_object (XCHAR_TABLE_PARENT (obj));
  mark_object (XCHAR_TABLE_DEFAULT (obj));
#ifdef MIRROR_TABLE
  mark_object (XCHAR_TABLE_MIRROR_TABLE (obj));
#endif /* MIRROR_TABLE */
  return XCHAR_TABLE_TABLE (obj);
}

static void
init_chartab_tables (Lisp_Object chartab)
{
  /* CHARTAB_LEVELS (foo) will evaluates to 4 when MAXIMIZE_CHAR_TABLE_DEPTH
     and MULE, to 1 if MAXIMIZE_CHAR_TABLE_DEPTH and not MULE, and to
     foo otherwise. */
  XCHAR_TABLE_LEVELS (chartab) = CHARTAB_LEVELS (1);
  XCHAR_TABLE_TABLE (chartab) = Qunbound;
}

static void
free_chartab_tables (Lisp_Object chartab)
{
  if (!UNBOUNDP (XCHAR_TABLE_TABLE (chartab)))
    {
      free_chartab_table (XCHAR_TABLE_TABLE (chartab));
      XCHAR_TABLE_TABLE (chartab) = Qunbound;
    }
}

static Lisp_Object
char_table_type_to_symbol (enum char_table_type type)
{
  switch (type)
  {
  default: ABORT();
  case CHAR_TABLE_TYPE_GENERIC:  return Qgeneric;
  case CHAR_TABLE_TYPE_SYNTAX:   return Qsyntax;
  case CHAR_TABLE_TYPE_DISPLAY:  return Qdisplay;
  case CHAR_TABLE_TYPE_CHAR:     return Qchar;
#ifdef MULE
  case CHAR_TABLE_TYPE_CATEGORY: return Qcategory;
#endif
  }
}

static enum char_table_type
symbol_to_char_table_type (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);

  if (EQ (symbol, Qgeneric))  return CHAR_TABLE_TYPE_GENERIC;
  if (EQ (symbol, Qsyntax))   return CHAR_TABLE_TYPE_SYNTAX;
  if (EQ (symbol, Qdisplay))  return CHAR_TABLE_TYPE_DISPLAY;
  if (EQ (symbol, Qchar))     return CHAR_TABLE_TYPE_CHAR;
#ifdef MULE
  if (EQ (symbol, Qcategory)) return CHAR_TABLE_TYPE_CATEGORY;
#endif

  invalid_constant ("Unrecognized char table type", symbol);
  RETURN_NOT_REACHED (CHAR_TABLE_TYPE_GENERIC);
}

static Lisp_Object
char_table_default_for_type (enum char_table_type type)
{
  switch (type)
    {
    case CHAR_TABLE_TYPE_CHAR:
      return make_char (0);
      break;
    case CHAR_TABLE_TYPE_DISPLAY:
    case CHAR_TABLE_TYPE_GENERIC:
#ifdef MULE
    case CHAR_TABLE_TYPE_CATEGORY:
#endif /* MULE */
      return Qnil;
      break;

    case CHAR_TABLE_TYPE_SYNTAX:
      return make_fixnum (Sinherit);
      break;
    }
  ABORT();
  return Qzero;
}

struct ptemap
{
  Lisp_Object printcharfun;
  int first;
  int num_printed;
  int max;
};

static int
print_table_entry (Lisp_Object UNUSED (table), Ichar from, Ichar to,
		   Lisp_Object val, void *arg)
{
  struct ptemap *a = (struct ptemap *) arg;
  QUIT;
  if (!a->first)
    write_ascstring (a->printcharfun, " ");
  a->first = 0;
  if (!print_readably && a->num_printed > a->max)
    {
      write_ascstring (a->printcharfun, "...");
      return 1;
    }
  if (from == to)
    write_fmt_string_lisp (a->printcharfun, "%s %S", make_char (from), val);
  else if (print_readably)
    write_fmt_string_lisp (a->printcharfun, "(%s %s) %S",
			   make_char (from), make_char (to), val);
  else
    write_fmt_string_lisp (a->printcharfun, "%s-%s %S",
			   make_char (from), make_char (to), val);
  a->num_printed++;
  return 0;
}

static void
print_char_table (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_Char_Table *ct = XCHAR_TABLE (obj);
  Lisp_Object typname = XSYMBOL_NAME (char_table_type_to_symbol (ct->type));
  struct chartab_range range;
  struct ptemap arg;

  range.type = CHARTAB_RANGE_ALL;
  arg.printcharfun = printcharfun;
  arg.first = 1;

  write_ascstring (printcharfun, "#s(char-table :type ");
  /* write_lisp_string() is fine, we know it's not an uninterned symbol. */
  write_lisp_string (printcharfun, typname, 0, XSTRING_LENGTH (typname));
			 
  if (!(EQ (ct->default_, char_table_default_for_type (ct->type))))
    {
      write_ascstring (printcharfun, " :default ");
      print_internal (ct->default_, printcharfun, 1);
    }

  write_ascstring (printcharfun, " :data (");
  map_char_table (obj, &range, print_table_entry, &arg);
  write_ascstring (printcharfun, "))");
}

static const struct memory_description char_table_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, table) },
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, parent) },
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, default_) },
  { XD_LO_LINK,     offsetof (Lisp_Char_Table, next_table) },
#ifdef MIRROR_TABLE
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, mirror_table) },
#endif /* MIRROR_TABLE */
  { XD_END }
};

DEFINE_DUMPABLE_LISP_OBJECT ("char-table", char_table,
			     mark_char_table, print_char_table, 0,
			     char_table_equal, char_table_hash,
			     char_table_description,
			     Lisp_Char_Table);

/* WARNING: All functions of this nature need to be written extremely
   carefully to avoid crashes during GC.  Cf. prune_specifiers()
   and prune_weak_hash_tables(). */

void
prune_syntax_tables (void)
{
  Lisp_Object rest, prev = Qnil;

  for (rest = Vall_syntax_tables;
       !NILP (rest);
       rest = XCHAR_TABLE (rest)->next_table)
    {
      if (! marked_p (rest))
	{
	  /* This table is garbage.  Remove it from the list. */
	  if (NILP (prev))
	    Vall_syntax_tables = XCHAR_TABLE (rest)->next_table;
	  else
	    XCHAR_TABLE (prev)->next_table =
	      XCHAR_TABLE (rest)->next_table;
	}
    }
}

static void
decode_char_table_range (Lisp_Object range, struct chartab_range *outrange)
{
  xzero (*outrange);
  if (EQ (range, Qt))
    outrange->type = CHARTAB_RANGE_ALL;
  else if (CHAR_OR_CHAR_INTP (range))
    {
      outrange->type = CHARTAB_RANGE_CHAR;
      outrange->ch = XCHAR_OR_CHAR_INT (range);
    }
  else if (CONSP (range))
    {
      CHECK_CHAR_COERCE_INT (XCAR (range));
      CHECK_CHAR_COERCE_INT (XCDR (range));
      outrange->type = CHARTAB_RANGE_RANGE;
      outrange->ch = XCHAR_OR_CHAR_INT (XCAR (range));
      outrange->chtop = XCHAR_OR_CHAR_INT (XCDR (range));
    }
#ifndef MULE
  else
    sferror ("Range must be t, character or cons of char range", range);
#else /* MULE */
  else if (VECTORP (range))
    {
      Lisp_Vector *vec = XVECTOR (range);
      Lisp_Object *elts = vector_data (vec);
      if (vector_length (vec) != 2)
	sferror ("Length of charset row vector must be 2",
			     range);
      outrange->type = CHARTAB_RANGE_ROW;
      outrange->charset = Fget_charset (elts[0]);
      CHECK_FIXNUM (elts[1]);
      outrange->row = XFIXNUM (elts[1]);
      if (XCHARSET_DIMENSION (outrange->charset) == 1)
	sferror ("Charset in row vector must be multi-byte",
		 outrange->charset);
      else
	{
	  check_integer_range
            (elts[1], make_fixnum (XCHARSET_OFFSET (outrange->charset, 0)),
             make_fixnum (XCHARSET_OFFSET (outrange->charset, 0) +
                       XCHARSET_CHARS (outrange->charset, 0) - 1));
	}
    }
  else
    {
      if (!CHARSETP (range) && !SYMBOLP (range))
	sferror
	  ("Char table range must be t, char, charset, cons or vector", range);
      outrange->type = CHARTAB_RANGE_CHARSET;
      outrange->charset = Fget_charset (range);
    }
#endif /* MULE */
}

static void
check_non_category_char_table (Lisp_Object chartab)
{
  CHECK_CHAR_TABLE (chartab);
  if (XCHAR_TABLE_CATEGORY_P (chartab))
    invalid_operation ("Can't perform this operation on a category char-table",
		       chartab);
}

DEFUN ("char-table-p", Fchar_table_p, 1, 1, 0, /*
Return non-nil if OBJECT is a char table.
*/
       (object))
{
  return CHAR_TABLEP (object) ? Qt : Qnil;
}

DEFUN ("char-table-type-list", Fchar_table_type_list, 0, 0, 0, /*
Return a list of the recognized char table types.
See `make-char-table'.
*/
       ())
{
#ifdef MULE
  return list5 (Qchar, Qcategory, Qdisplay, Qgeneric, Qsyntax);
#else
  return list4 (Qchar, Qdisplay, Qgeneric, Qsyntax);
#endif
}

DEFUN ("valid-char-table-type-p", Fvalid_char_table_type_p, 1, 1, 0, /*
Return t if TYPE if a recognized char table type.
See `make-char-table'.
*/
       (type))
{
  return (EQ (type, Qchar)     ||
#ifdef MULE
	  EQ (type, Qcategory) ||
#endif
	  EQ (type, Qdisplay)  ||
	  EQ (type, Qgeneric)  ||
	  EQ (type, Qsyntax)) ? Qt : Qnil;
}

DEFUN ("char-table-type", Fchar_table_type, 1, 1, 0, /*
Return the type of CHAR-TABLE.
See `make-char-table'.
*/
       (char_table))
{
  CHECK_CHAR_TABLE (char_table);
  return char_table_type_to_symbol (XCHAR_TABLE (char_table)->type);
}

static void
set_char_table_dirty (Lisp_Object USED_IF_MIRROR_TABLE (table))
{
#ifdef MIRROR_TABLE
  assert (!XCHAR_TABLE (table)->mirror_table_p);
  XCHAR_TABLE (XCHAR_TABLE (table)->mirror_table)->dirty = 1;
#endif /* MIRROR_TABLE */
}

void
set_char_table_default (Lisp_Object table, Lisp_Object value)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
  ct->default_ = value;
  if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
    set_char_table_dirty (table);
}

DEFUN ("reset-char-table", Freset_char_table, 1, 1, 0, /*
Reset CHAR-TABLE to its default state.
*/
       (char_table))
{
  Lisp_Char_Table *ct;

  CHECK_CHAR_TABLE (char_table);
  ct = XCHAR_TABLE (char_table);

  /* Avoid doubly updating the syntax table by setting the default ourselves,
     since set_char_table_default() also updates. */
  ct->default_ = char_table_default_for_type (ct->type);
  free_chartab_tables (char_table);
  init_chartab_tables (char_table);

  return Qnil;
}

static Lisp_Object
make_char_table (Lisp_Object type, int internal_p)
{
  Lisp_Object obj;
  Lisp_Char_Table *ct;
  enum char_table_type ty;

  if (EQ (type, Qcategory) && !internal_p)
    invalid_operation ("Can't directly create category-type char tables",
		       Qunbound);

  obj = ALLOC_NORMAL_LISP_OBJECT (char_table);
  ct = XCHAR_TABLE (obj);
  ty = symbol_to_char_table_type (type);

  ct->type = ty;
  obj = wrap_char_table (ct);
  ct->table = Qunbound;
#ifdef MIRROR_TABLE
  if (ty == CHAR_TABLE_TYPE_SYNTAX)
    {
      /* Qgeneric not Qsyntax because a syntax table has a mirror table
	 and we don't want infinite recursion */
      ct->mirror_table = Fmake_char_table (Qgeneric);
      set_char_table_default (ct->mirror_table, make_fixnum (Sword));
      XCHAR_TABLE (ct->mirror_table)->mirror_table_p = 1;
      XCHAR_TABLE (ct->mirror_table)->mirror_table = obj;
    }
  else
    ct->mirror_table = Qnil;
#endif /* MIRROR_TABLE */
  ct->next_table = Qnil;
  ct->parent = Qnil;
  ct->default_ = Qnil;
  if (ty == CHAR_TABLE_TYPE_SYNTAX)
    {
      ct->next_table = Vall_syntax_tables;
      Vall_syntax_tables = obj;
    }
  Freset_char_table (obj);
  return obj;
}

DEFUN ("make-char-table", Fmake_char_table, 1, 1, 0, /*
Return a new, empty char table of type TYPE.

A char table is a table that maps characters (or ranges of characters)
to values.  Char tables are specialized for characters, only allowing
particular sorts of ranges to be assigned values.  Although this
loses in generality, it makes for extremely fast (constant-time)
lookups, and thus is feasible for applications that do an extremely
large number of lookups (e.g. scanning a buffer for a character in
a particular syntax, where a lookup in the syntax table must occur
once per character).

When Mule support exists, the types of ranges that can be assigned
values are

-- all characters (represented by t)
-- an entire charset
-- a single row in a two-octet charset (represented by a vector of two
   elements: a two-octet charset and a row number; the row must be an
   integer, not a character)
-- a single character

When Mule support is not present, the types of ranges that can be
assigned values are

-- all characters (represented by t)
-- a single character

To create a char table, use `make-char-table'.
To modify a char table, use `put-char-table' or `remove-char-table'.
To retrieve the value for a particular character, use `get-char-table'.
See also `map-char-table', `reset-char-table', `copy-char-table',
`char-table-p', `valid-char-table-type-p', `char-table-type-list',
`valid-char-table-value-p', and `check-char-table-value'.

Each char table type is used for a different purpose and allows different
sorts of values.  The different char table types are

`char'
	A generalized char table, for mapping from one character to another.
	Used for case tables, syntax matching tables,
	`keyboard-translate-table', etc.  The valid values are characters,
	and the default result given by `get-char-table' if a value hasn't
	been set for a given character or for a range that includes it, is
	?\x00.
`generic'
        An even more generalized char table, for mapping from a character to
	anything. The default result given by `get-char-table' is nil.
`syntax'
	Used for syntax tables, which specify the syntax of a particular
	character.  Higher-level Lisp functions are provided for
	working with syntax tables.  The valid values are integers (intended
        to be syntax codes as generated by `syntax-string-to-code'), and the
	default result given by `get-char-table' is the syntax code for
	`word'. (Note: In 21.4 and prior, it was the code for `inherit'.)
`category'
	Used internally for category tables.  These are a special type of
        char tables, which specify the regexp categories that a
	character is in.  It is not possible to create such a table from
        Lisp.  Instead, use the functions that are provided for working
        with category tables (see `make-category-table').  Currently categories
	and category tables only exist when Mule support is present.
`display'
	Used for display tables, which specify how a particular character is
	to appear when displayed.  #### Not yet implemented; currently, the
	display table code uses generic char tables, and it's not clear that
	implementing this char table type would be useful.
*/
       (type))
{
  return make_char_table (type, 0);
}

DEFUN ("copy-char-table", Fcopy_char_table, 1, 1, 0, /*
Return a new char table which is a copy of CHAR-TABLE.
It will contain the same values for the same characters and ranges
as CHAR-TABLE.  The values will not themselves be copied.
*/
       (char_table))
{
  Lisp_Char_Table *ct, *ctnew;
  Lisp_Object obj;

  CHECK_CHAR_TABLE (char_table);
  ct = XCHAR_TABLE (char_table);
#ifdef MIRROR_TABLE
  assert (!ct->mirror_table_p);
#endif
  obj = ALLOC_NORMAL_LISP_OBJECT (char_table);
  ctnew = XCHAR_TABLE (obj);
  ctnew->type = ct->type;
  ctnew->parent = ct->parent;
  ctnew->default_ = ct->default_;
  ctnew->levels = ct->levels;
  ctnew->table = clone_chartab_table (ct->table, ct->levels,
				      CHAR_TABLE_CATEGORY_P (ct));
  obj = wrap_char_table (ctnew);

#ifdef MIRROR_TABLE
  ctnew->mirror_table_p = 0;
  if (!EQ (ct->mirror_table, Qnil))
    {
      ctnew->mirror_table = Fmake_char_table (Qgeneric);
      set_char_table_default (ctnew->mirror_table, make_fixnum (Sword));
      XCHAR_TABLE (ctnew->mirror_table)->mirror_table = obj;
      XCHAR_TABLE (ctnew->mirror_table)->mirror_table_p = 1;
      XCHAR_TABLE (ctnew->mirror_table)->dirty = 1;
    }
  else
    ctnew->mirror_table = Qnil;
#endif /* MIRROR_TABLE */

  ctnew->next_table = Qnil;
  if (ctnew->type == CHAR_TABLE_TYPE_SYNTAX)
    {
      ctnew->next_table = Vall_syntax_tables;
      Vall_syntax_tables = obj;
    }
  return obj;
}

DEFUN ("char-table-default", Fchar_table_default, 1, 1, 0, /*
Return the default value for CHAR-TABLE.  When an entry for a character
does not exist, the default is returned.
*/
       (char_table))
{
  check_non_category_char_table (char_table);
  return XCHAR_TABLE (char_table)->default_;
}

DEFUN ("set-char-table-default", Fset_char_table_default, 2, 2, 0, /*
Set the default value for CHAR-TABLE to DEFAULT.
Currently, the default value for syntax tables cannot be changed.
(This policy might change in the future.)
*/
       (char_table, default_))
{
  check_non_category_char_table (char_table);
  if (XCHAR_TABLE_TYPE (char_table) == CHAR_TABLE_TYPE_SYNTAX)
    invalid_change ("Can't change default for syntax tables", char_table);
  check_valid_char_table_value (default_, XCHAR_TABLE_TYPE (char_table),
				ERROR_ME);
  set_char_table_default (char_table, default_);
  return Qnil;
}

DEFUN ("get-char-table", Fget_char_table, 2, 2, 0, /*
Find value for CHARACTER in CHAR-TABLE.
*/
       (character, char_table))
{
  check_non_category_char_table (char_table);
  CHECK_CHAR_COERCE_INT (character);

  return get_char_table (XCHAR (character), char_table);
}
    
static int
check_valid_char_table_value (Lisp_Object value, enum char_table_type type,
			      Error_Behavior errb)
{
  switch (type)
    {
    case CHAR_TABLE_TYPE_SYNTAX:
      if (!ERRB_EQ (errb, ERROR_ME))
	return FIXNUMP (value) || (CONSP (value) && FIXNUMP (XCAR (value))
				&& CHAR_OR_CHAR_INTP (XCDR (value)));
      if (CONSP (value))
        {
	  Lisp_Object cdr = XCDR (value);
          CHECK_FIXNUM (XCAR (value));
	  CHECK_CHAR_COERCE_INT (cdr);
         }
      else
        CHECK_FIXNUM (value);
      break;

#ifdef MULE
    case CHAR_TABLE_TYPE_CATEGORY:
      maybe_signal_error (Qinvalid_operation,
			  "Can't set category char tables in this fashion",
			  value, Qchar_table, errb);
      return 0;
#endif /* MULE */

    case CHAR_TABLE_TYPE_GENERIC:
      return 1;

    case CHAR_TABLE_TYPE_DISPLAY:
      /* #### fix this */
      maybe_signal_error (Qunimplemented,
			  "Display char tables not yet implemented",
			  value, Qchar_table, errb);
      return 0;

    case CHAR_TABLE_TYPE_CHAR:
      if (!ERRB_EQ (errb, ERROR_ME))
	return CHAR_OR_CHAR_INTP (value);
      CHECK_CHAR_COERCE_INT (value);
      break;

    default:
      ABORT ();
    }

  return 0; /* not (usually) reached */
}

static Lisp_Object
canonicalize_char_table_value (Lisp_Object value, enum char_table_type type)
{
  switch (type)
    {
    case CHAR_TABLE_TYPE_SYNTAX:
      if (CONSP (value))
	{
	  Lisp_Object car = XCAR (value);
	  Lisp_Object cdr = XCDR (value);
	  CHECK_CHAR_COERCE_INT (cdr);
	  return Fcons (car, cdr);
	}
      break;
    case CHAR_TABLE_TYPE_CHAR:
      CHECK_CHAR_COERCE_INT (value);
      break;
    default:
      break;
    }
  return value;
}

DEFUN ("valid-char-table-value-p", Fvalid_char_table_value_p, 2, 2, 0, /*
Return non-nil if VALUE is a valid value for CHAR-TABLE-TYPE.
*/
       (value, char_table_type))
{
  enum char_table_type type = symbol_to_char_table_type (char_table_type);

  return check_valid_char_table_value (value, type, ERROR_ME_NOT) ? Qt : Qnil;
}

DEFUN ("check-valid-char-table-value", Fcheck_valid_char_table_value, 2, 2, 0, /*
Signal an error if VALUE is not a valid value for CHAR-TABLE-TYPE.
*/
       (value, char_table_type))
{
  enum char_table_type type = symbol_to_char_table_type (char_table_type);

  check_valid_char_table_value (value, type, ERROR_ME);
  return Qnil;
}

/* Assign VAL to all characters in RANGE in char table TABLE. */

static void
put_char_table_range (Lisp_Object table, struct chartab_range *range,
		      Lisp_Object val)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
#ifdef MULE
  int l1, l2, h1, h2;
#endif

  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      put_char_table (table, 0, CHAR_CODE_LIMIT - 1, val);
      break;

    case CHARTAB_RANGE_RANGE:
      put_char_table (table, range->ch, range->chtop, val);
      break;

    case CHARTAB_RANGE_CHAR:
      put_char_table (table, range->ch, range->ch, val);
      break;


#ifdef MULE
    case CHARTAB_RANGE_ROW:
      {
	get_charset_limits (range->charset, &l1, &l2, &h1, &h2);
	l1 = h1 = range->row;
	goto iterate_charset;
      }

    case CHARTAB_RANGE_CHARSET:
      get_charset_limits (range->charset, &l1, &l2, &h1, &h2);
    iterate_charset:
#ifdef UNICODE_INTERNAL
      {
	int i, j;

	/* Under Unicode-internal, the characters in a charset row or in a
	   charset are non-contiguous and may not even exist */
	for (i = l1; i <= h1; i++)
	  for (j = l2; j <= h2; j++)
	    {
	      Ichar ch = charset_codepoint_to_ichar_raw (range->charset, i, j);
	      if (ch >= 0)
		put_char_table (table, ch, ch, val);
	    }
	break;
      }
#else /* not UNICODE_INTERNAL */
      {
	/* Under old-Mule, the characters in a charset row all exist and
	   are contiguous.  The characters in a charset are contiguous
	   but with gaps in them; however, that's not an issue here. */
	Ichar from = charset_codepoint_to_ichar_raw (range->charset, l1, l2);
	Ichar to = charset_codepoint_to_ichar_raw (range->charset, h1, h2);
	text_checking_assert (from >= 0);
	text_checking_assert (to >= 0);
	put_char_table (table, from, to, val);
      }
#endif /* (not) UNICODE_INTERNAL */
#endif /* MULE */
    }

  if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
    set_char_table_dirty (wrap_char_table (ct));
}

DEFUN ("put-char-table", Fput_char_table, 3, 3, 0, /*
Set the value for CHAR to be VALUE in CHAR-TABLE.

CHAR specifies one or more characters to be affected and should be
one of the following:

-- A charset (only allowed when Mule support is present; all characters
   in the charset are set)
-- A vector of two elements: a two-octet charset and a row number; the row
   must be an integer, not a character (only allowed when Mule support is
   present)
-- A cons of two characters (a range, inclusive on both ends)
-- A single character

VALUE must be a value appropriate for the type of CHAR-TABLE.
See `make-char-table'.
*/
       (range, value, char_table))
{
  Lisp_Char_Table *ct;
  struct chartab_range rainj;

  check_non_category_char_table (char_table);
  ct = XCHAR_TABLE (char_table);
  check_valid_char_table_value (value, ct->type, ERROR_ME);
  decode_char_table_range (range, &rainj);
  value = canonicalize_char_table_value (value, ct->type);
  put_char_table_range (char_table, &rainj, value);
  return Qnil;
}

DEFUN ("remove-char-table", Fremove_char_table, 2, 2, 0, /*
Remove any value from chars in RANGE in CHAR-TABLE.

RANGE specifies one or more characters to be affected and should be
one of the following:

-- t (all characters are affected)
-- A cons of two characters (a range, inclusive on both ends)
-- A charset (only allowed when Mule support is present)
-- A vector of two elements: a two-octet charset and a row number
   (only allowed when Mule support is present)
-- A single character

With all values removed, the default value will be returned by 
`get-char-table' and `get-range-char-table'.
*/
       (range, char_table))
{
  struct chartab_range rainj;

  check_non_category_char_table (char_table);
  decode_char_table_range (range, &rainj);
  if (rainj.type == CHARTAB_RANGE_ALL)
    {
      free_chartab_tables (char_table);
      init_chartab_tables (char_table);
    }
  else
    put_char_table_range (char_table, &rainj, Qunbound);
  return Qnil;
}

/* Map FN (with client data ARG) over range RANGE in char table CT.
   Function is called on a range [START, END] (specifying a range over
   which teh ta, poss Mapping stops the first time FN returns non-zero, and
   that value becomes the return value of map_char_table().
 */

int
map_char_table (Lisp_Object table,
		struct chartab_range *range,
		int (*fn) (Lisp_Object table, Ichar from, Ichar to,
			   Lisp_Object val, void *arg),
		void *arg)
{
#ifdef MULE
  int l1, h1, l2, h2;
#endif
  int levels = XCHAR_TABLE_LEVELS (table);
  /* Compute maximum allowed value for this table, which may be less than
     the range we have been requested to map over. */
  int maxval = chartab_span_top[levels];
  struct map_chartab_range rainj;

  rainj.start = -1;
  rainj.end = -1;
  rainj.val = Qunbound;

  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      return map_chartab_table_0 (XCHAR_TABLE_TABLE (table),
				  XCHAR_TABLE_LEVELS (table),
				  0, 0, maxval, table, fn, arg, &rainj);

    case CHARTAB_RANGE_RANGE:
      return map_chartab_table_0 (XCHAR_TABLE_TABLE (table),
				  XCHAR_TABLE_LEVELS (table),
				  0, min (range->ch, maxval),
				  min (range->chtop, maxval),
				  table, fn, arg, &rainj);


    case CHARTAB_RANGE_CHAR:
      {
	Lisp_Object val = get_char_table_raw (range->ch, table);

	if (!UNBOUNDP (val))
	  return really_call_map_chartab_fun (fn, table, range->ch, range->ch,
					      val, arg);
	else
	  return 0;
      }
#ifdef MULE
    case CHARTAB_RANGE_ROW:
      {
	get_charset_limits (range->charset, &l1, &l2, &h1, &h2);
	l1 = h1 = range->row;
	goto iterate_charset;
      }

    case CHARTAB_RANGE_CHARSET:
      get_charset_limits (range->charset, &l1, &l2, &h1, &h2);
    iterate_charset:
#ifdef UNICODE_INTERNAL
      {
	int i, j;

	/* Under Unicode-internal, the characters in a charset row or in a
	   charset are non-contiguous and may not even exist */
	for (i = l1; i <= h1; i++)
	  for (j = l2; j <= h2; j++)
	    {
	      Ichar ch = charset_codepoint_to_ichar_raw (range->charset, i, j);
	      if (ch >= 0)
		{
		  Lisp_Object val = get_char_table_raw (ch, table);
		  if (!UNBOUNDP (val))
		    {
		      int retval =
			really_call_map_chartab_fun (fn, table, ch, ch, val,
						     arg);
		      if (retval)
			return retval;
		    }
		}
	    }
	break;
      }
#else /* not UNICODE_INTERNAL */
      {
	/* Under old-Mule, the characters in a charset row all exist and
	   are contiguous.  The characters in a charset are contiguous
	   but with gaps in them; however, that's not an issue here. */
	Ichar from = charset_codepoint_to_ichar_raw (range->charset, l1, l2);
	Ichar to = charset_codepoint_to_ichar_raw (range->charset, h1, h2);
	text_checking_assert (from >= 0);
	text_checking_assert (to >= 0);
	return map_chartab_table_0 (XCHAR_TABLE_TABLE (table),
				    XCHAR_TABLE_LEVELS (table),
				    0, min (from, maxval), min (to, maxval),
				    table, fn, arg, &rainj);
      }
#endif /* (not) UNICODE_INTERNAL */
#endif /* MULE */

    default:
      ABORT ();
    }

  return 0;
}

struct slow_map_char_table_arg
{
  Lisp_Object function;
  Lisp_Object retval;
};

static int
slow_map_char_table_fun (Lisp_Object UNUSED (table),
			 Ichar from, Ichar to, Lisp_Object val, void *arg)
{
  struct slow_map_char_table_arg *closure =
    (struct slow_map_char_table_arg *) arg;

  closure->retval =
    call2 (closure->function,
	   from == to ? make_char (from) :
	   Fcons (make_char (from), make_char (to)),
	   val);
  return !NILP (closure->retval);
}

DEFUN ("map-char-table", Fmap_char_table, 2, 3, 0, /*
Map FUNCTION over CHAR-TABLE until it returns non-nil; return that value.
FUNCTION is called with two arguments, a character or range (FROM . TO) and
the associated value in the table.  FUNCTION will only be called for
characters whose value has been set.

RANGE specifies a subrange to map over.  If omitted or t, it defaults to
the entire table.  Other possible values are the same as can be passed to
`put-char-table': an individual character, a cons specifying a character
range, a charset or a vector giving a charset and a row in that charset.
*/
       (function, char_table, range))
{
  struct slow_map_char_table_arg slarg;
  struct gcpro gcpro1, gcpro2;
  struct chartab_range rainj;

  check_non_category_char_table (char_table);
  if (NILP (range))
    range = Qt;
  decode_char_table_range (range, &rainj);
  slarg.function = function;
  slarg.retval = Qnil;
  GCPRO2 (slarg.function, slarg.retval);
  map_char_table (char_table, &rainj, slow_map_char_table_fun, &slarg);
  UNGCPRO;

  return slarg.retval;
}



/************************************************************************/
/*                         Char table read syntax                       */
/************************************************************************/

static int
chartab_type_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
		       Error_Behavior UNUSED (errb))
{
  /* #### should deal with ERRB */
  symbol_to_char_table_type (value);
  return 1;
}

/* #### Document the print/read format; esp. what's this cons element? */

static int
chartab_data_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
		       Error_Behavior UNUSED (errb))
{
  /* #### should deal with ERRB */
  EXTERNAL_PROPERTY_LIST_LOOP_3 (range, data, value)
    {
      if (CONSP (range))
	{
	  if (CHARP (XCAR (range)) &&
	      CONSP (XCDR (range)) &&
	      CHARP (XCAR (XCDR (range))) &&
	      NILP (XCDR (XCDR (range))))
	    continue;
	}
      else if (CHARP (range))
	continue;
      sferror ("Invalid range format", range);
    }

  return 1;
}

static int
chartab_default_validate (Lisp_Object UNUSED (keyword),
			  Lisp_Object UNUSED (value),
			  Error_Behavior UNUSED (errb))
{
  /* We can't yet validate this, since we don't know what the type of the
     char table is. We do the validation below in chartab_instantiate(). */
  return 1;
}

static Lisp_Object
chartab_instantiate (Lisp_Object plist)
{
  Lisp_Object chartab;
  Lisp_Object type = Qgeneric;
  Lisp_Object dataval = Qnil, default_ = Qunbound;

  if (KEYWORDP (Fcar (plist)))
    {
      PROPERTY_LIST_LOOP_3 (key, value, plist)
	{
	  if (EQ (key, Q_data))
	    {
	      dataval = value;
	    }
	  else if (EQ (key, Q_type))
	    {
	      type = value;
	    }
	  else if (EQ (key, Q_default_))
	    {
	      default_ = value;
	    }
	  else if (!KEYWORDP (key))
	    {
	      signal_error
		(Qinvalid_read_syntax, 
		 "can't mix keyword and non-keyword structure syntax",
		 key);
	    }
	  else 
	    ABORT ();
	}
    }
#ifdef NEED_TO_HANDLE_21_4_CODE
  else
    {
      PROPERTY_LIST_LOOP_3 (key, value, plist)
	{
	  if (EQ (key, Qdata))
	    {
	      dataval = value;
	    }
	  else if (EQ (key, Qtype))
	    {
	      type = value;
	    }
	  else if (KEYWORDP (key))
            signal_error
	      (Qinvalid_read_syntax, 
	       "can't mix keyword and non-keyword structure syntax",
	       key);
	  else 
	    ABORT ();
	}
    }
#endif /* NEED_TO_HANDLE_21_4_CODE */

  chartab = Fmake_char_table (type);
  if (!UNBOUNDP (default_))
    {
      check_valid_char_table_value (default_, XCHAR_TABLE_TYPE (chartab),
				    ERROR_ME);
      set_char_table_default (chartab, default_);
#ifdef MIRROR_TABLE
      if (!NILP (XCHAR_TABLE (chartab)->mirror_table))
        {
          set_char_table_default (XCHAR_TABLE (chartab)->mirror_table,
                                  default_);
        }
#endif
    }

  while (!NILP (dataval))
    {
      Lisp_Object range = Fcar (dataval);
      Lisp_Object val = Fcar (Fcdr (dataval));

      dataval = Fcdr (Fcdr (dataval));
      if (CONSP (range))
        {
	  if (CHAR_OR_CHAR_INTP (XCAR (range)))
	    {
	      Ichar first = XCHAR_OR_CHAR_INT (Fcar (range));
	      Ichar last = XCHAR_OR_CHAR_INT (Fcar (Fcdr (range)));
	      Ichar i;

	      for (i = first; i <= last; i++)
		 Fput_char_table (make_char (i), val, chartab);
	    }
	  else
	    ABORT ();
	}
      else
	Fput_char_table (range, val, chartab);
    }

  return chartab;
}

#ifdef MULE


/************************************************************************/
/*                           Category Tables                            */
/************************************************************************/

static int
category_table_equal (Lisp_Object obj1, Lisp_Object obj2, int depth,
		      int foldcase)
{
  int i;

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    if (!internal_equal_0 (XCATEGORY_TABLE_TABLES (obj1)[i],
			   XCATEGORY_TABLE_TABLES (obj2)[i],
			   depth + 1, foldcase))
      return 0;
  return 1;
}

static Hashcode
category_table_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  int i;
  Hashcode hashval = 0;

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    hashval = HASH2 (hashval,
		     internal_hash (XCATEGORY_TABLE_TABLES (obj)[i],
				    depth + 1, equalp));

  return hashval;
}

static Lisp_Object
mark_category_table (Lisp_Object obj)
{
  int i;

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    mark_object (XCATEGORY_TABLE_TABLES (obj)[i]);
  return Qnil;
}

static void
print_category_table (Lisp_Object obj, Lisp_Object printcharfun,
		      int UNUSED (escapeflag))
{
  int i;

  /* #### Eventually need to print properly and readably */
  write_ascstring (printcharfun, "#<category-table");
  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    write_fmt_string_lisp (printcharfun, " %s",
			   XCATEGORY_TABLE_TABLES (obj)[i]);
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static const struct memory_description category_table_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Category_Table, tables),
    CHAR_TABLES_PER_CATEGORY_TABLE },
  { XD_END }
};

DEFINE_DUMPABLE_LISP_OBJECT ("category-table", category_table,
			     mark_category_table, print_category_table, 0,
			     category_table_equal, category_table_hash,
			     category_table_description,
			     Lisp_Category_Table);

DEFUN ("make-category-table", Fmake_category_table, 0, 0, 0, /*
Construct a new and empty category table and return it.
A category table is a table used for keeping track of categories.
Categories are used for classifying characters for use in regexps -- you
can refer to a category rather than having to use a complicated []
expression (and category lookups are significantly faster).

There are 95 different categories available, one for each printable
character (including space) in the ASCII charset.  Each category
is designated by one such character, called a "category designator".
They are specified in a regexp using the syntax "\\cX", where X is
a category designator.

A category table specifies, for each character, the categories that
the character is in.  Note that a character can be in more than one
category.
*/
       ())
{
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (category_table);
  int i;

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    XCATEGORY_TABLE_TABLES (obj)[i] = make_char_table (Qcategory, 1);
  return obj;
}

DEFUN ("category-table-p", Fcategory_table_p, 1, 1, 0, /*
Return t if OBJECT is a category table.
See `make-category-table' for more information.
*/
       (object))
{
  return (CATEGORY_TABLEP (object) ? Qt : Qnil);
}

static Lisp_Object
check_category_table (Lisp_Object object, Lisp_Object default_)
{
  if (NILP (object))
    object = default_;
  CHECK_CATEGORY_TABLE (object);
  return object;
}

/* Check whether character CH is in the category specified by DESIGNATOR
   in category table TABLE.  If NOT_P is non-zero, reverse the sense of the
   check, i.e. return non-zero if character CH is *not* in the category. */

int
check_char_in_category (Ichar ch, Lisp_Object table, int designator, int not_p)
{
  Lisp_Object chartab, val;

  chartab =
    XCATEGORY_TABLE_TABLES (table)[DESIGNATOR_TO_CHAR_TABLE (designator)];
  val = get_char_table_raw (ch, chartab);
  return ONEP (val) ? !not_p : not_p;
}

DEFUN ("check-category-at", Fcheck_category_at, 2, 4, 0, /*
Return t if category of the character at POSITION includes DESIGNATOR.
Optional third arg BUFFER specifies which buffer to use, and defaults
to the current buffer.
Optional fourth arg CATEGORY-TABLE specifies the category table to
use, and defaults to BUFFER's category table.
*/
       (position, designator, buffer, category_table))
{
  Lisp_Object ctbl;
  Ichar ch;
  int des;
  struct buffer *buf = decode_buffer (buffer, 0);

  CHECK_FIXNUM (position);
  CHECK_CATEGORY_DESIGNATOR (designator);
  des = XCHAR (designator);
  ctbl = check_category_table (category_table, buf->category_table);
  ch = BUF_FETCH_CHAR (buf, XFIXNUM (position));
  return check_char_in_category (ch, ctbl, des, 0) ? Qt : Qnil;
}

DEFUN ("char-in-category-p", Fchar_in_category_p, 2, 3, 0, /*
Return non-nil if category of CHARACTER includes DESIGNATOR.
Optional third arg CATEGORY-TABLE specifies the category table to use,
and defaults to the current buffer's category table.
*/
       (character, designator, category_table))
{
  Lisp_Object ctbl;
  Ichar ch;
  int des;

  CHECK_CATEGORY_DESIGNATOR (designator);
  des = XCHAR (designator);
  CHECK_CHAR (character);
  ch = XCHAR (character);
  ctbl = check_category_table (category_table, current_buffer->category_table);
  return check_char_in_category (ch, ctbl, des, 0) ? Qt : Qnil;
}

DEFUN ("category-table", Fcategory_table, 0, 1, 0, /*
Return BUFFER's current category table.
BUFFER defaults to the current buffer.
*/
       (buffer))
{
  return decode_buffer (buffer, 0)->category_table;
}

DEFUN ("standard-category-table", Fstandard_category_table, 0, 0, 0, /*
Return the standard category table.
This is the one used for new buffers.
*/
       ())
{
  return Vstandard_category_table;
}

DEFUN ("copy-category-table", Fcopy_category_table, 0, 1, 0, /*
Return a new category table which is a copy of CATEGORY-TABLE.
CATEGORY-TABLE defaults to the standard category table.
*/
       (category_table))
{
  Lisp_Object obj;
  int i;

  category_table =
    check_category_table (category_table, Vstandard_category_table);

  obj = ALLOC_NORMAL_LISP_OBJECT (category_table);

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    XCATEGORY_TABLE_TABLES (obj)[i] =
      Fcopy_char_table (XCATEGORY_TABLE_TABLES (category_table)[i]);
  return obj;
}

DEFUN ("set-category-table", Fset_category_table, 1, 2, 0, /*
Select CATEGORY-TABLE as the new category table for BUFFER.
BUFFER defaults to the current buffer if omitted.
*/
       (category_table, buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  category_table = check_category_table (category_table, Qnil);
  buf->category_table = category_table;
  /* Indicate that this buffer now has a specified category table.  */
  buf->local_var_flags |= XFIXNUM (buffer_local_flags.category_table);
  return category_table;
}

DEFUN ("modify-category-entry-internal", Fmodify_category_entry_internal,
       2, 4, 0, /*
  "Add a category to the categories associated with CHAR-RANGE.
CHAR-RANGE is a single character or a range of characters,
 as per `put-char-table'.
The category is given by a designator character.
The changes are made in CATEGORY-TABLE, which defaults to the current
 buffer's category table.
If optional fourth argument RESET is non-nil, previous categories associated
 with CHAR-RANGE are removed before adding the specified category.

NOTE: This function is identical to `modify-category-entry' except that it
doesn't check to make sure that DESIGNATOR passes `defined-category-p', i.e.
that it is a category previously created with `define-category'.
*/
       (char_range, designator, category_table, reset))
{
  struct chartab_range rainj;

  decode_char_table_range (char_range, &rainj);
  category_table = check_category_table (category_table,
					 Fcategory_table (Qnil));
  CHECK_CATEGORY_DESIGNATOR (designator);
  if (!NILP (reset))
    {
      int i;

      for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
	put_char_table_range (XCATEGORY_TABLE_TABLES (category_table)[i],
			      &rainj, Qunbound);
    }
  put_char_table_range
    (XCATEGORY_TABLE_TABLES (category_table)
     [DESIGNATOR_TO_CHAR_TABLE (XCHAR (designator))], &rainj, Qone);
  return Qnil;
}

DEFUN ("category-designator-p", Fcategory_designator_p, 1, 1, 0, /*
Return t if OBJECT is a category designator (a char in the range ' ' to '~').
*/
       (object))
{
  return CATEGORY_DESIGNATORP (object) ? Qt : Qnil;
}

struct slow_map_category_table_arg
{
  Lisp_Object function;
  Lisp_Object retval;
  int tablenum;
};

static int
slow_map_category_table_fun (Lisp_Object UNUSED (table),
			     Ichar from, Ichar to, Lisp_Object val, void *arg)
{
  struct slow_map_category_table_arg *closure =
    (struct slow_map_category_table_arg *) arg;
  structure_checking_assert (ONEP (val));

  closure->retval = call2 (closure->function,
			   from == to ? make_char (from) :
			   Fcons (make_char (from), make_char (to)),
			   make_char
			   (CHAR_TABLE_TO_DESIGNATOR (closure->tablenum)));
  if (!NILP (closure->retval))
    return 1;
  return 0;
}

DEFUN ("map-category-table", Fmap_category_table, 2, 3, 0, /*
Map FUNCTION over CATEGORY-TABLE until it returns non-nil; return that value.
FUNCTION is called with two arguments, a character and a category designator.
FUNCTION will only be called for pairs (CHARACTER, DESIGNATOR) that have
been previously set.

RANGE specifies a subrange to map over.  If omitted or t, it defaults to
the entire table.  Other possible values are the same as can be passed to
`put-char-table': an individual character, a cons specifying a character
range, a charset or a vector giving a charset and a row in that charset.
*/
       (function, category_table, range))
{
  struct slow_map_category_table_arg slarg;
  struct gcpro gcpro1, gcpro2;
  struct chartab_range rainj;
  int i;

  CHECK_CATEGORY_TABLE (category_table);
  if (NILP (range))
    range = Qt;
  decode_char_table_range (range, &rainj);
  slarg.function = function;
  slarg.retval = Qnil;
  GCPRO2 (slarg.function, slarg.retval);

  for (i = 0; i < CHAR_TABLES_PER_CATEGORY_TABLE; i++)
    {
      slarg.tablenum = i;
      map_char_table (XCATEGORY_TABLE_TABLES (category_table)[i],
		      &rainj, slow_map_category_table_fun, &slarg);
    }
  UNGCPRO;

  return slarg.retval;
}

/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   Use the macro WORD_BOUNDARY_P instead of calling this function
   directly.  */

int
word_boundary_p (struct buffer *buf, Ichar c1, Ichar c2)
{
  Lisp_Object tail;
  int default_result;
  Lisp_Object table = buf->category_table;

#ifdef ENABLE_COMPOSITE_CHARS
  if (COMPOSITE_CHAR_P (c1))
    c1 = cmpchar_component (c1, 0, 1);
  if (COMPOSITE_CHAR_P (c2))
    c2 = cmpchar_component (c2, 0, 1);
#endif

  /* @@#### fix me */
  if (EQ (buffer_ichar_charset_obsolete_me_baby (buf, c1),
	  buffer_ichar_charset_obsolete_me_baby (buf, c2)))
    {
      tail = Vword_separating_categories;
      default_result = 0;
    }
  else
    {
      tail = Vword_combining_categories;
      default_result = 1;
    }

  for (; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt = XCAR (tail);

      if (CONSP (elt)
	  && CATEGORY_DESIGNATORP (XCAR (elt))
	  && CATEGORY_DESIGNATORP (XCDR (elt))
	  && check_char_in_category (c1, table, XCHAR (XCAR (elt)), 0)
	  && check_char_in_category (c2, table, XCHAR (XCDR (elt)), 0))
	return !default_result;
    }
  return default_result;
}

#endif /* MULE */


void
chartab_objects_create (void)
{
  OBJECT_HAS_METHOD (char_table, print_preprocess);
  OBJECT_HAS_METHOD (char_table, nsubst_structures_descend);
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_METHOD (char_table, memory_usage);
#endif
}

void
syms_of_chartab (void)
{
  INIT_LISP_OBJECT (char_table);
  INIT_LISP_OBJECT (char_subtable);

#ifdef MULE
  INIT_LISP_OBJECT (category_table);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qcategory_tablep);
  DEFSYMBOL (Qcategory_designator_p);
#endif /* MULE */

  DEFSYMBOL (Qchar_table);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qchar_tablep);

#ifdef MEMORY_USAGE_STATS
  DEFSYMBOL (Qpage_tables);
#endif

  DEFSUBR (Fchar_table_p);
  DEFSUBR (Fchar_table_type_list);
  DEFSUBR (Fvalid_char_table_type_p);
  DEFSUBR (Fchar_table_type);
  DEFSUBR (Fchar_table_default);
  DEFSUBR (Fset_char_table_default);
  DEFSUBR (Freset_char_table);
  DEFSUBR (Fmake_char_table);
  DEFSUBR (Fcopy_char_table);
  DEFSUBR (Fget_char_table);
  DEFSUBR (Fvalid_char_table_value_p);
  DEFSUBR (Fcheck_valid_char_table_value);
  DEFSUBR (Fput_char_table);
  DEFSUBR (Fremove_char_table);
  DEFSUBR (Fmap_char_table);

#ifdef MULE
  DEFSUBR (Fmake_category_table);
  DEFSUBR (Fcategory_table_p);
  DEFSUBR (Fcategory_table);
  DEFSUBR (Fstandard_category_table);
  DEFSUBR (Fcopy_category_table);
  DEFSUBR (Fset_category_table);
  DEFSUBR (Fcheck_category_at);
  DEFSUBR (Fchar_in_category_p);
  DEFSUBR (Fcategory_designator_p);
  DEFSUBR (Fmodify_category_entry_internal);
  DEFSUBR (Fmap_category_table);
#endif /* MULE */

}

void
vars_of_chartab (void)
{
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_PROPERTY
    (char_table, memusage_stats_list, list2 (Qt, Qpage_tables));
#endif /* MEMORY_USAGE_STATS */

  /* DO NOT staticpro this.  It works just like Vweak_hash_tables. */
  Vall_syntax_tables = Qnil;
  dump_add_weak_object_chain (&Vall_syntax_tables);

  /* The value at level 4 is not 2^32 - 1.  With 32-bit EMACS_INTs, it's
     2^30 - 1 because characters are only 30 bits wide. */
  chartab_span_top[0] = 0;
  chartab_span_top[1] = (1 << 8) - 1;
  chartab_span_top[2] = (1 << 16) - 1;
  chartab_span_top[3] = (1 << 24) - 1;
  chartab_span_top[4] = CHAR_CODE_LIMIT - 1;
  dump_add_opaque (chartab_span_top, sizeof (chartab_span_top));
}

void
structure_type_create_chartab (void)
{
  struct structure_type *st;

  st = define_structure_type (Qchar_table, 0, chartab_instantiate);

#ifdef NEED_TO_HANDLE_21_4_CODE
  define_structure_type_keyword (st, Qtype, chartab_type_validate);
  define_structure_type_keyword (st, Qdata, chartab_data_validate);
#endif /* NEED_TO_HANDLE_21_4_CODE */

  define_structure_type_keyword (st, Q_type, chartab_type_validate);
  define_structure_type_keyword (st, Q_data, chartab_data_validate);
  define_structure_type_keyword (st, Q_default_, chartab_default_validate);
}

void
complex_vars_of_chartab (void)
{
#ifdef MULE
  /* Set this now, so first buffer creation can refer to it. */
  Vstandard_category_table = Fmake_category_table ();
  staticpro (&Vstandard_category_table);

  DEFVAR_LISP ("word-combining-categories", &Vword_combining_categories /*
List of pair (cons) of categories to determine word boundary.

Emacs treats a sequence of word constituent characters as a single
word (i.e. finds no word boundary between them) iff they belongs to
the same charset.  But, exceptions are allowed in the following cases.

\(1) The case that characters are in different charsets is controlled
by the variable `word-combining-categories'.

Emacs finds no word boundary between characters of different charsets
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's no word boundary between C1 and C2.

For instance, to tell that ASCII characters and Latin-1 characters can
form a single word, the element `(?l . ?l)' should be in this list
because both characters have the category `l' (Latin characters).

\(2) The case that character are in the same charset is controlled by
the variable `word-separating-categories'.

Emacs find a word boundary between characters of the same charset
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's a word boundary between C1 and C2.

For instance, to tell that there's a word boundary between Japanese
Hiragana and Japanese Kanji (both are in the same charset), the
element `(?H . ?C) should be in this list.
*/ );

  Vword_combining_categories = Qnil;

  DEFVAR_LISP ("word-separating-categories", &Vword_separating_categories /*
List of pair (cons) of categories to determine word boundary.
See the documentation of the variable `word-combining-categories'.
*/ );

  Vword_separating_categories = Qnil;
#endif /* MULE */
}
