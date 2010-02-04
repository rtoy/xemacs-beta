/* XEmacs routines to deal with char tables.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002, 2003, 2005, 2010 Ben Wing.
   Copyright (C) 1995, 1997, 1999 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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

/* Synched up with: Mule 2.3.  Not synched with FSF.

   This file was written independently of the FSF implementation,
   and is not compatible. */

/* Authorship:

   Ben Wing: wrote, for 19.13 (Mule).  Some category table stuff
             loosely based on the original Mule.
   Jareth Hein: fixed a couple of bugs in the implementation, and
   	     added regex support for categories with check_category_at
   Ben Wing: Drastic rewrite, October 2005, for Unicode-internal support.
   The old implementation used an ugly system indexed by charset ID,
   with `char-table-entry' objects.  The implementation of map_char_table()
   was long and nasty.  The new system uses page tables, as in unicode.c.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "chartab.h"
#include "syntax.h"


Lisp_Object Qchar_tablep, Qchar_table;

Lisp_Object Vall_syntax_tables;

#ifdef MULE
Lisp_Object Qcategory_table_p;
Lisp_Object Qcategory_designator_p;
Lisp_Object Qcategory_table_value_p;

Lisp_Object Vstandard_category_table;

/* Variables to determine word boundary.  */
Lisp_Object Vword_combining_categories, Vword_separating_categories;
#endif /* MULE */

static int check_valid_char_table_value (Lisp_Object value,
					 enum char_table_type type,
			                 Error_Behavior errb);


/* A char table maps from ranges of characters to values.

   Implementing a general data structure that maps from arbitrary
   ranges of numbers to values is tricky to do efficiently.  As it
   happens, it should suffice (and is usually more convenient, anyway)
   when dealing with characters to restrict the sorts of ranges that
   can be assigned values, as follows:

   1) All characters.
   2) All characters in a charset.
   3) All characters in a particular row of a charset, where a "row"
      means all characters with the same first byte.
   4) A particular character in a charset.

   We use char tables to generalize the 256-element vectors now
   littering the Emacs code.

   Possible uses (all should be converted at some point):

   1) category tables
   2) syntax tables
   3) display tables
   4) case tables
   5) keyboard-translate-table?

   We provide an
   abstract type to generalize the Emacs vectors and Mule
   vectors-of-vectors goo.
   */

/************************************************************************/
/*                         Char Table tables                            */
/************************************************************************/

/* We use the same code from unicode.c.

   Code duplication is generally a bad thing, but there isn't that much total
   code and there are a lot of differences.  I originally tried abstracting
   using preprocessing, but it got real ugly real fast.  This is even more
   the case now that char tables can use Lisp objects for their subtables. */

static SUBTAB_TYPE chartab_blank[5];

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

DEFINE_LRECORD_IMPLEMENTATION ("char-subtable", char_subtable,
			       1, /*dumpable-flag*/
                               mark_char_subtable, internal_object_printer,
			       0, 0, 0, char_subtable_description,
			       Lisp_Char_Subtable);


/************************************************************************/
/*                      Char table implementation                       */
/************************************************************************/

static void
init_blank_chartab_tables (void)
{
  int i;

  chartab_blank[1] = ALLOCATE_LEVEL_1_SUB_TABLE ();
  chartab_blank[2] = ALLOCATE_LEVEL_N_SUB_TABLE ();
  chartab_blank[3] = ALLOCATE_LEVEL_N_SUB_TABLE ();
  chartab_blank[4] = ALLOCATE_LEVEL_N_SUB_TABLE ();
  for (i = 0; i < 256; i++)
    {
      LISPOBJ_ARRAY_FROM_SUBTAB (chartab_blank[1])[i] = Qunbound;
      SUBTAB_ARRAY_FROM_SUBTAB (chartab_blank[2])[i] = chartab_blank[1];
      SUBTAB_ARRAY_FROM_SUBTAB (chartab_blank[3])[i] = chartab_blank[2];
      SUBTAB_ARRAY_FROM_SUBTAB (chartab_blank[4])[i] = chartab_blank[3];
    }
}

static SUBTAB_TYPE
copy_chartab_table (SUBTAB_TYPE table, int level)
{
  SUBTAB_TYPE newtab;
  Bytecount size;

  text_checking_assert (level >= 1 && level <= 4);
  /* WARNING: sizeof (Lisp_Object) maybe != sizeof (SUBTAB_TYPE). */
  if (level == 1)
    {
      size = sizeof (Lisp_Object);
      newtab = ALLOCATE_LEVEL_1_SUB_TABLE ();
      memcpy (LISPOBJ_ARRAY_FROM_SUBTAB (newtab),
	      LISPOBJ_ARRAY_FROM_SUBTAB (table),
	      256 * size);
    }
  else
    {
      size = sizeof (SUBTAB_TYPE);
      newtab = ALLOCATE_LEVEL_N_SUB_TABLE ();
      memcpy (SUBTAB_ARRAY_FROM_SUBTAB (newtab),
	      SUBTAB_ARRAY_FROM_SUBTAB (table),
	      256 * size);
    }

  if (level >= 2)
    {
      int i;
      SUBTAB_ARRAY_TYPE tab = SUBTAB_ARRAY_FROM_SUBTAB (newtab);
      for (i = 0; i < 256; i++)
	{
	  if (!SUBTAB_EQ (tab[i], chartab_blank[level - 1]))
	    tab[i] = copy_chartab_table (tab[i], level - 1);
	}
    }

  return newtab;
}

static SUBTAB_TYPE
create_new_chartab_table (int level)
{
  return copy_chartab_table (chartab_blank[level], level);
}

static void
free_chartab_table (SUBTAB_TYPE table, int level)
{
  if (level >= 2)
    {
      int i;
      SUBTAB_ARRAY_TYPE tab = SUBTAB_ARRAY_FROM_SUBTAB (table);

      for (i = 0; i < 256; i++)
	{
	  if (!SUBTAB_EQ (tab[i], chartab_blank[level - 1]))
	    free_chartab_table (tab[i], level - 1);
	}
    }

  FREE_ONE_SUBTABLE (table);
}

#ifdef MEMORY_USAGE_STATS

static Bytecount
compute_chartab_table_size_1 (SUBTAB_TYPE table, int level,
			      struct overhead_stats *stats)
{
  Bytecount size = 0;

  if (level >= 2)
    {
      int i;
      SUBTAB_ARRAY_TYPE tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
      for (i = 0; i < 256; i++)
	{
	  if (!SUBTAB_EQ (tab[i], chartab_blank[level - 1]))
	    size += compute_chartab_table_size_1 (tab[i], level - 1, stats);
	}
    }

  size += SUBTAB_STORAGE_SIZE (table, level, stats);
  return size;
}

Bytecount
compute_chartab_table_size (Lisp_Object chartab,
			    struct overhead_stats *stats)
{
  return (compute_chartab_table_size_1
	  (XCHAR_TABLE_TABLE (chartab),
	   XCHAR_TABLE_LEVELS (chartab),
	   stats));
}

#endif

void
put_char_table_1 (Lisp_Object chartab, Ichar ch, Lisp_Object val)
{
  /* #### NOTE NOTE NOTE!

  If it turns out that people are often setting large ranges to a
  particular value (and particularly so if we have to implement the FSF
  characteristic of allowing `t' to signify *all* characters), then we
  should consider either (a) modifying things so that the subtables are
  actual Lisp objects and at any level there can either be a subtable or
  some other Lisp object, which signifies the value everywhere at and below
  that level (then we also don't need blank tables; instead we just use
  Qunbound); (b) modifying the code that loops over a range to create
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

  shared tables we check for could potentially be specific to the particular
  char table; we'd keep track of the shared tables in the char-table object,
  and check to see if they are shared with the generic blank tables.)

  If we don't do this, we should make sure to put in a call to QUIT
  periodically when setting a range so if someone does something stupid
  like set a range of (0,2000000000), they can break out.  We also need a
  big warning about this in the docs to `put-char-table' and such.
  Maybe we should also allow for two different types of char tables, one
  that allows for semi-efficient handling of large ranges and one that doesn't
  (but is faster).  In such a case it might make sense for there to be a
  get_char_table() method pointer to avoid an if-check every time for the
  type.  Similarly if we allow the `always-maximize-table-size' option to
  be given. */

  int levels;
  int u4, u3, u2, u1;
#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  int code_levels;
#endif

  text_checking_assert (valid_ichar_p (ch));
  CHARTAB_BREAKUP_CHAR_CODE ((int) ch, u4, u3, u2, u1, code_levels);

  levels = CHARTAB_LEVELS (XCHAR_TABLE_LEVELS (chartab));
  text_checking_assert (levels >= 1 && levels <= 4);

#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  /* Make sure the chartab's tables have at least as many levels as
     the code point has: Note that the table is guaranteed to have
     at least one level, because it was created that way */
  if (levels < code_levels)
    {
      int i;

      for (i = 2; i <= code_levels; i++)
	{
	  if (levels < i)
	    {
	      SUBTAB_TYPE old_table = XCHAR_TABLE_TABLE (chartab);
	      SUBTAB_TYPE table = create_new_chartab_table (i);
	      XCHAR_TABLE_TABLE (chartab) = table;
	      SUBTAB_ARRAY_FROM_SUBTAB (table)[0] = old_table;
	    }
	}

      levels = code_levels;
      XCHAR_TABLE_LEVELS (chartab) = code_levels;
    }
#endif /* not MAXIMIZE_CHAR_TABLE_DEPTH */

  /* Now, make sure there is a non-default table at each level */
  {
    int i;
    SUBTAB_TYPE table = XCHAR_TABLE_TABLE (chartab);

    for (i = levels; i >= 2; i--)
      {
	int ind;

	switch (i)
	  {
	  case 4: ind = u4; break;
	  case 3: ind = u3; break;
	  case 2: ind = u2; break;
	  default: ABORT (); ind = 0;
	  }

	if (SUBTAB_EQ (SUBTAB_ARRAY_FROM_SUBTAB (table)[ind],
		       chartab_blank[i - 1]))
	  SUBTAB_ARRAY_FROM_SUBTAB (table)[ind] =
	    create_new_chartab_table (i - 1);
	table = SUBTAB_ARRAY_FROM_SUBTAB (table)[ind];
      }
  }

  /* Finally, set the character */
	  
  {
    register SUBTAB_TYPE table = XCHAR_TABLE_TABLE (chartab);
    /* We are really helping the compiler here.  CHARTAB_LEVELS() will
       evaluate to a constant when MAXIMIZE_CHAR_TABLE_DEPTH is true,
       so any reasonable optimizing compiler should eliminate the
       switch entirely. */
    switch (CHARTAB_LEVELS (levels))
      {
#if 1 /* The new way */
	/* fall through */
      case 4: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u4];
      case 3: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u3];
      case 2: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u2];
      case 1: LISPOBJ_ARRAY_FROM_SUBTAB (table)[u1] = val;
#else /* The old way */
      case 1: ((Lisp_Object *) table)[u1] = val; break;
      case 2: ((Lisp_Object **) table)[u2][u1] = val; break;
      case 3: ((Lisp_Object ***) table)[u3][u2][u1] = val; break;
      case 4: ((Lisp_Object ****) table)[u4][u3][u2][u1] = val; break;
#endif
      }
  }
}

/* Map over all characters in the range [START, END].  TABLE is an array
   of 256 elements, LEVEL is the depth (1 - 4).  OFFSET is the character
   offset corresponding to this table.  CHARTAB is the char-table object
   being mapped over.  The FN will be called with CHARTAB, the code of the
   character in question, its value, and the value of ARG.  Stops mapping
   the first time that FN returns non-zero, and returns that value.
   Returns zero if mapping got all the way to the end. */

static int
map_chartab_table (SUBTAB_TYPE table, int level, int offset, int start,
		   int end, Lisp_Object chartab,
		   int (*fn) (Lisp_Object chartab, Ichar code, Lisp_Object val,
			      void *arg),
		   void *arg)
{
  int i;
  int startind = max (0, (start - offset) >> ((level - 1) * 8));
  int endind = min (255, (end - offset) >> ((level - 1) * 8));

  structure_checking_assert (startind <= 255);
  structure_checking_assert (endind >= 0);
  structure_checking_assert (startind <= endind);

  switch (level)
    {
    case 1:
      {
	Lisp_Object *tab = LISPOBJ_ARRAY_FROM_SUBTAB (table);
	for (i = startind; i <= endind; i++)
	  {
	    if (!UNBOUNDP (tab[i]))
	      {
		int retval = (fn) (chartab, offset + i, tab[i], arg);
		if (retval)
		  return retval;
	      }
	  }
	break;
      }
    case 2:
    case 3:
    case 4:
      {
	SUBTAB_ARRAY_TYPE tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
	for (i = startind; i <= endind; i++)
	  {
	    if (!SUBTAB_EQ (tab[i], chartab_blank[level - 1]))
	      {
		int retval =
		  map_chartab_table (tab[i], level - 1,
				     offset + (i << ((level - 1) * 8)),
				     start, end, chartab, fn, arg);
		if (retval)
		  return retval;
	      }
	  }
	break;
      }
    default:
      ABORT ();
    }

  return 0;
}

/* Check whether the given table is entirely blank.  TABLE is LEVEL levels
   deep.  Start checking at START (this will normally be 1, since we don't
   want to check the 0th level, which indexes the possibly non-blank
   lower levels.
   */

static int
check_if_blank (SUBTAB_TYPE table, int level, int start, int depth)
{
  int i;

  switch (level)
    {
    case 1:
      {
	Lisp_Object *tab = LISPOBJ_ARRAY_FROM_SUBTAB (table);
	for (i = start; i < 256; i++)
	  {
	    if (!UNBOUNDP (tab[i]))
	      return 0;
	  }
	break;
      }
    case 2:
    case 3:
    case 4:
      {
	SUBTAB_ARRAY_TYPE tab = SUBTAB_ARRAY_FROM_SUBTAB (table);
	for (i = start; i < 256; i++)
	  {
	    if (!SUBTAB_EQ (tab[i], chartab_blank[level - 1]) &&
		!check_if_blank (tab[i], level - 1, 0, depth))
	      return 0;
	  }
	break;
      }
    default:
      ABORT ();
    }

  return 1;
}

static int
chartab_tables_equal (SUBTAB_TYPE table1, SUBTAB_TYPE table2, int level,
		      int depth, int foldcase)
{
  int i;

  switch (level)
    {
    case 1:
      {
	Lisp_Object *tab1 = LISPOBJ_ARRAY_FROM_SUBTAB (table1);
	Lisp_Object *tab2 = LISPOBJ_ARRAY_FROM_SUBTAB (table2);
	for (i = 0; i < 256; i++)
	  {
	    if (!internal_equal_0 (tab1[i], tab2[i], depth + 1, foldcase))
	      return 0;
	  }
	break;
      }
    case 2:
    case 3:
    case 4:
      {
	SUBTAB_ARRAY_TYPE tab1 = SUBTAB_ARRAY_FROM_SUBTAB (table1);
	SUBTAB_ARRAY_TYPE tab2 = SUBTAB_ARRAY_FROM_SUBTAB (table2);
	for (i = 0; i < 256; i++)
	  {
	    if (SUBTAB_EQ (tab1[i], chartab_blank[level - 1]) &&
		SUBTAB_EQ (tab2[i], chartab_blank[level - 1]))
	      ;
	    else if (SUBTAB_EQ (tab1[i], chartab_blank[level - 1]))
	      {
		if (!check_if_blank (tab2[i], level - 1, 0, depth))
		  return 0;
	      }
	    else if (SUBTAB_EQ (tab2[i], chartab_blank[level - 1]))
	      {
		if (!check_if_blank (tab1[i], level - 1, 0, depth))
		  return 0;
	      }
	    else
	      {
		if (!chartab_tables_equal (tab1[1], tab2[1], level - 1,
					   depth, foldcase))
		  return 0;
	      }
	  }
	break;
      }
    default:
      ABORT ();
    }

  return 1;
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

  SUBTAB_TYPE table;

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
	  if (!check_if_blank (table, i, 1, depth))
	    return 0;
	  table = SUBTAB_ARRAY_FROM_SUBTAB (table)[0];
	}
    }

  /* If we got this far, TABLE points to the appropriate (sub)table with the
     same number of levels as that of OBJ2. */
  return chartab_tables_equal (table,
			       XCHAR_TABLE_TABLE (obj2),
			       XCHAR_TABLE_LEVELS (obj2),
			       depth, foldcase);
}

/* Characters likely to have case pairs or special syntax -- e.g. comment
   characters -- or unusual mappings in e.g. JIS-ROMAN, or in certain
   national character sets that map higher ASCII punctuation chars into
   extra letters (cf. the need for digraphs and trigraphs in C/C++) plus
   some random ones to boot; probably, ASCII chars are more likely to show
   up in char tables than others. */
static const Ascbyte *likely_test = "\t\n\r\f\016\025\0330128!@#$%^&*`'_+=-,.<>?;:/~()[]{}\\\"acehijlnortuxyzADEGIKMOQSVY";

static Hashcode
char_table_hash (Lisp_Object obj, int depth)
{
  Hashcode hashval = HASH2 (XCHAR_TABLE_TYPE (obj),
			    internal_hash (XCHAR_TABLE_DEFAULT (obj),
					   depth + 1));
  const Ascbyte *p;
  Ichar ch;

  /* Hash those most likely to have values */
  for (p = likely_test; *p; p++)
    hashval = HASH2 (hashval, internal_hash
		     (get_char_table_raw ((Ichar) *p, obj), depth + 1));
  /* Hash some random Latin characters */
  for (ch = 130; ch <= 255; ch += 5)
    hashval = HASH2 (hashval, internal_hash
		     (get_char_table_raw (ch, obj), depth + 1));
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
	hashval = HASH2 (hashval, internal_hash (get_char_table_raw (ch, obj),
						 depth + 1));
      /* Hash some higher characters */
      for (ch = 500; ch <= 4000; ch += 50)
	hashval = HASH2 (hashval, internal_hash (get_char_table_raw (ch, obj),
						 depth + 1));
      /* Hash some random CJK characters */
      for (ch = 0x4E00; ch <= 0x9FFF; ch += 791)
	hashval = HASH2 (hashval, internal_hash (get_char_table_raw (ch, obj),
						 depth + 1));
      /* Hash some random Hangul characters */
      for (ch = 0xAC00; ch <= 0xD7AF; ch += 791)
	hashval = HASH2 (hashval, internal_hash (get_char_table_raw (ch, obj),
						 depth + 1));
#elif defined (MULE)
/* 0xA1 is usually the first alphabetic character and differs across
   charsets, whereas 0xA0 is no-break-space across many of them.
   charset_codepoint_to_ichar_raw() can't fail because we are in non-
   Unicode-internal. */
#define FROB1(cs)							\
  hashval = HASH2 (hashval,						\
		   internal_hash (get_char_table_raw			\
				  (charset_codepoint_to_ichar_raw	\
				   (cs, 0, 0xA1),			\
				   obj), depth + 1))
/* 0x3021 is the first CJK character in a number of different CJK charsets
   and differs across them. */
#define FROB2(cs)							\
  hashval = HASH2 (hashval,						\
		   internal_hash (get_char_table_raw			\
				  (charset_codepoint_to_ichar_raw	\
				   (cs, 0x30, 0x21),			\
				   obj), depth + 1))
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

/* Allocate and blank the tables. */
static void
init_chartab_tables (Lisp_Object chartab)
{
  /* CHARTAB_LEVELS (foo) will evaluates to 4 when MAXIMIZE_CHAR_TABLE_DEPTH
     and MULE, to 1 if MAXIMIZE_CHAR_TABLE_DEPTH and not MULE, and to
     foo otherwise. */
  XCHAR_TABLE_LEVELS (chartab) = CHARTAB_LEVELS (1);
  XCHAR_TABLE_TABLE (chartab) =
    create_new_chartab_table (XCHAR_TABLE_LEVELS (chartab));
}

static void
free_chartab_tables (Lisp_Object chartab)
{
  if (!UNBOUNDP (XCHAR_TABLE_TABLE (chartab)))
    {
      free_chartab_table (XCHAR_TABLE_TABLE (chartab),
			  XCHAR_TABLE_LEVELS (chartab));
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

struct ptemap
{
  Lisp_Object printcharfun;
  int first;
};

static int
print_table_entry (Lisp_Object UNUSED (table), Ichar ch,
		   Lisp_Object val, void *arg)
{
  struct ptemap *a = (struct ptemap *) arg;
  if (!a->first)
    write_ascstring (a->printcharfun, " ");
  a->first = 0;
  write_fmt_string_lisp (a->printcharfun, "%s %S", 2, make_char (ch), val);
  return 0;
}

static void
print_char_table (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_Char_Table *ct = XCHAR_TABLE (obj);
  struct chartab_range range;
  struct ptemap arg;

  range.type = CHARTAB_RANGE_ALL;
  arg.printcharfun = printcharfun;
  arg.first = 1;

  write_fmt_string_lisp (printcharfun, "#s(char-table type %s data (",
			 1, char_table_type_to_symbol (ct->type));
  map_char_table (obj, &range, print_table_entry, &arg);
  write_ascstring (printcharfun, "))");

  /* #### need to print and read the default; but that will allow the
     default to be modified, which we don't (yet) support -- but FSF does */
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

DEFINE_LRECORD_IMPLEMENTATION ("char-table", char_table,
			       1, /*dumpable-flag*/
                               mark_char_table, print_char_table,
			       0,
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
      CHECK_INT (elts[1]);
      outrange->row = XINT (elts[1]);
      if (XCHARSET_DIMENSION (outrange->charset) == 1)
	sferror ("Charset in row vector must be multi-byte",
		 outrange->charset);
      else
	{
	  check_int_range (outrange->row,
			   XCHARSET_OFFSET (outrange->charset, 0),
			   XCHARSET_OFFSET (outrange->charset, 0) +
			   XCHARSET_CHARS (outrange->charset, 0) - 1);
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
  Lisp_Object def;

  CHECK_CHAR_TABLE (char_table);
  ct = XCHAR_TABLE (char_table);

  switch (ct->type)
    {
    case CHAR_TABLE_TYPE_CHAR:
      def = make_char (0);
      break;
    case CHAR_TABLE_TYPE_DISPLAY:
    case CHAR_TABLE_TYPE_GENERIC:
#ifdef MULE
    case CHAR_TABLE_TYPE_CATEGORY:
#endif /* MULE */
      def = Qnil;
      break;

    case CHAR_TABLE_TYPE_SYNTAX:
      def = make_int (Sinherit);
      break;

    default:
      ABORT ();
      def = Qnil;
      break;
    }

  /* Avoid doubly updating the syntax table by setting the default ourselves,
     since set_char_table_default() also updates. */
  XCHAR_TABLE_DEFAULT (char_table) = def;
  free_chartab_tables (char_table);
  init_chartab_tables (char_table);

  return Qnil;
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

`category'
	Used for category tables, which specify the regexp categories that a
	character is in.  The valid values are nil or a bit vector of 95
	elements, and values default to nil.  Higher-level Lisp functions
	are provided for working with category tables.  Currently categories
	and category tables only exist when Mule support is present.
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
`display'
	Used for display tables, which specify how a particular character is
	to appear when displayed.  #### Not yet implemented; currently, the
	display table code uses generic char tables, and it's not clear that
	implementing this char table type would be useful.
`syntax'
	Used for syntax tables, which specify the syntax of a particular
	character.  Higher-level Lisp functions are provided for
	working with syntax tables.  The valid values are integers (intended
        to be syntax codes as generated by `syntax-string-to-code'), and the
	default result given by `get-char-table' is the syntax code for
	`word'. (Note: In 21.4 and prior, it was the code for `inherit'.)
*/
       (type))
{
  Lisp_Char_Table *ct;
  Lisp_Object obj;
  enum char_table_type ty = symbol_to_char_table_type (type);

  ct = ALLOC_LCRECORD_TYPE (Lisp_Char_Table, &lrecord_char_table);
  ct->type = ty;
  obj = wrap_char_table (ct);
  ct->table = Qunbound;
#ifdef MIRROR_TABLE
  if (ty == CHAR_TABLE_TYPE_SYNTAX)
    {
      /* Qgeneric not Qsyntax because a syntax table has a mirror table
	 and we don't want infinite recursion */
      ct->mirror_table = Fmake_char_table (Qgeneric);
      set_char_table_default (ct->mirror_table, make_int (Sword));
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
  ctnew = ALLOC_LCRECORD_TYPE (Lisp_Char_Table, &lrecord_char_table);
  ctnew->type = ct->type;
  ctnew->parent = ct->parent;
  ctnew->default_ = ct->default_;
  ctnew->levels = ct->levels;
  ctnew->table = copy_chartab_table (ct->table, ct->levels);
  obj = wrap_char_table (ctnew);

#ifdef MIRROR_TABLE
  ctnew->mirror_table_p = 0;
  if (!EQ (ct->mirror_table, Qnil))
    {
      ctnew->mirror_table = Fmake_char_table (Qgeneric);
      set_char_table_default (ctnew->mirror_table, make_int (Sword));
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
  CHECK_CHAR_TABLE (char_table);
  return XCHAR_TABLE (char_table)->default_;
}

DEFUN ("set-char-table-default", Fset_char_table_default, 2, 2, 0, /*
Set the default value for CHAR-TABLE to DEFAULT.
Currently, the default value for syntax tables cannot be changed.
(This policy might change in the future.)
*/
       (char_table, default_))
{
  CHECK_CHAR_TABLE (char_table);
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
  CHECK_CHAR_TABLE (char_table);
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
	return INTP (value) || (CONSP (value) && INTP (XCAR (value))
				&& CHAR_OR_CHAR_INTP (XCDR (value)));
      if (CONSP (value))
        {
	  Lisp_Object cdr = XCDR (value);
          CHECK_INT (XCAR (value));
	  CHECK_CHAR_COERCE_INT (cdr);
         }
      else
        CHECK_INT (value);
      break;

#ifdef MULE
    case CHAR_TABLE_TYPE_CATEGORY:
      if (!ERRB_EQ (errb, ERROR_ME))
	return CATEGORY_TABLE_VALUEP (value);
      CHECK_CATEGORY_TABLE_VALUE (value);
      break;
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

void
put_char_table (Lisp_Object table, struct chartab_range *range,
		Lisp_Object val)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
#ifdef MULE
  int l1, h1, l2, h2;
#endif

  switch (range->type)
    {
#ifdef MULE
    case CHARTAB_RANGE_ROW:
      {
	get_charset_limits (range->charset, &l1, &h1, &l2, &h2);
	l1 = h1 = range->row;
	goto iterate_charset;
      }

    case CHARTAB_RANGE_CHARSET:
      {
	int i, j;
	get_charset_limits (range->charset, &l1, &h1, &l2, &h2);
      iterate_charset:
	for (i = l1; i <= h1; i++)
	  for (j = l2; j <= h2; j++)
	    {
	      Ichar ch = charset_codepoint_to_ichar_raw (range->charset, i, j);
	      if (ch >= 0)
		put_char_table_1 (table, ch, val);
	    }
      }
      break;
#endif /* MULE */

#define CHAR_INTERVAL_FOR_QUIT 1000
    case CHARTAB_RANGE_RANGE:
      {
	Ichar i;
	for (i = range->ch; i <= range->chtop; i += CHAR_INTERVAL_FOR_QUIT)
	  {
	    Ichar stop = min (i + CHAR_INTERVAL_FOR_QUIT - 1, range->chtop);
	    Ichar j;

	    /* QUIT every CHAR_INTERVAL_FOR_QUIT characters */
	    for (j = i; j <= stop; j++)
	      put_char_table_1 (table, j, val);
	    QUIT;
	  }
      }

      break;

    case CHARTAB_RANGE_CHAR:
      put_char_table_1 (table, range->ch, val);
      break;
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

  CHECK_CHAR_TABLE (char_table);
  ct = XCHAR_TABLE (char_table);
  check_valid_char_table_value (value, ct->type, ERROR_ME);
  decode_char_table_range (range, &rainj);
  if (rainj.type == CHARTAB_RANGE_ALL)
    invalid_argument ("Can't currently set all characters in a char table",
                      range);
  value = canonicalize_char_table_value (value, ct->type);
  put_char_table (char_table, &rainj, value);
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

  CHECK_CHAR_TABLE (char_table);
  decode_char_table_range (range, &rainj);
  if (rainj.type == CHARTAB_RANGE_ALL)
    {
      free_chartab_tables (char_table);
      init_chartab_tables (char_table);
    }
  else
    put_char_table (char_table, &rainj, Qunbound);
  return Qnil;
}

/* Map FN (with client data ARG) over range RANGE in char table CT.
   Mapping stops the first time FN returns non-zero, and that value
   becomes the return value of map_char_table().
 */

int
map_char_table (Lisp_Object table,
		struct chartab_range *range,
		int (*fn) (Lisp_Object table, Ichar code, Lisp_Object val,
			   void *arg),
		void *arg)
{
#ifdef MULE
  int l1, h1, l2, h2;
#endif
  int levels = XCHAR_TABLE_LEVELS (table);
  /* Compute maximum allowed value for this table, which may be less than
     the range we have been requested to map over. */
  int maxval = /* Value is 2^31-1 for 4, but 2^24-1 for 3,
		  2^16-1 for 2, 2^8-1 for 1. */
    levels == 4 ? INT_32_BIT_MAX : (1 << (levels * 8)) - 1;
  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      return map_chartab_table (XCHAR_TABLE_TABLE (table),
				XCHAR_TABLE_LEVELS (table),
				0, 0, maxval,
                                table, fn, arg);

    case CHARTAB_RANGE_RANGE:
      return map_chartab_table (XCHAR_TABLE_TABLE (table),
				XCHAR_TABLE_LEVELS (table),
				0, min (range->ch, maxval),
				min (range->chtop, maxval),
				table, fn, arg);

#ifdef MULE
    case CHARTAB_RANGE_ROW:
      {
	get_charset_limits (range->charset, &l1, &h1, &l2, &h2);
	l1 = h1 = range->row;
	goto iterate_charset;
      }

    case CHARTAB_RANGE_CHARSET:
      {
	int i, j;
	get_charset_limits (range->charset, &l1, &h1, &l2, &h2);
      iterate_charset:
	for (i = l1; i <= h1; i++)
	  for (j = l2; j <= h2; j++)
	    {
	      Ichar ch = charset_codepoint_to_ichar_raw (range->charset, i, j);
	      if (ch >= 0)
		{
		  Lisp_Object val = get_char_table (ch, table);
		  if (!UNBOUNDP (val))
		    {
		      int retval = (fn) (table, ch, val, arg);
		      if (retval)
			return retval;
		    }
		}
	    }
      }
      break;

#endif /* MULE */

    case CHARTAB_RANGE_CHAR:
      {
	Lisp_Object val = get_char_table (range->ch, table);

	if (!UNBOUNDP (val))
	  return (fn) (table, range->ch, val, arg);
	else
	  return 0;
      }

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
			 Ichar ch, Lisp_Object val, void *arg)
{
  struct slow_map_char_table_arg *closure =
    (struct slow_map_char_table_arg *) arg;

  closure->retval = call2 (closure->function, make_char (ch), val);
  return !NILP (closure->retval);
}

DEFUN ("map-char-table", Fmap_char_table, 2, 3, 0, /*
Map FUNCTION over CHAR-TABLE until it returns non-nil; return that value.
FUNCTION is called with two arguments, a character and the value for that
character in the table.  FUNCTION will only be called for characters whose
value has been set.

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

  CHECK_CHAR_TABLE (char_table);
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
      struct chartab_range dummy;

      if (CONSP (range))
	{
	  if (!CONSP (XCDR (range))
	      || !NILP (XCDR (XCDR (range))))
	    sferror ("Invalid range format", range);
	  decode_char_table_range (XCAR (range), &dummy);
	  decode_char_table_range (XCAR (XCDR (range)), &dummy);
	}
      else
	decode_char_table_range (range, &dummy);
    }

  return 1;
}

static Lisp_Object
chartab_instantiate (Lisp_Object data)
{
  Lisp_Object chartab;
  Lisp_Object type = Qgeneric;
  Lisp_Object dataval = Qnil;

  while (!NILP (data))
    {
      Lisp_Object keyw = Fcar (data);
      Lisp_Object valw;

      data = Fcdr (data);
      valw = Fcar (data);
      data = Fcdr (data);
      if (EQ (keyw, Qtype))
	type = valw;
      else if (EQ (keyw, Qdata))
	dataval = valw;
    }

  chartab = Fmake_char_table (type);

  data = dataval;
  while (!NILP (data))
    {
      Lisp_Object range = Fcar (data);
      Lisp_Object val = Fcar (Fcdr (data));

      data = Fcdr (Fcdr (data));
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
/*                     Category Tables, specifically                    */
/************************************************************************/

DEFUN ("category-table-p", Fcategory_table_p, 1, 1, 0, /*
Return t if OBJECT is a category table.
A category table is a type of char table used for keeping track of
categories.  Categories are used for classifying characters for use
in regexps -- you can refer to a category rather than having to use
a complicated [] expression (and category lookups are significantly
faster).

There are 95 different categories available, one for each printable
character (including space) in the ASCII charset.  Each category
is designated by one such character, called a "category designator".
They are specified in a regexp using the syntax "\\cX", where X is
a category designator.

A category table specifies, for each character, the categories that
the character is in.  Note that a character can be in more than one
category.  More specifically, a category table maps from a character
to either the value nil (meaning the character is in no categories)
or a 95-element bit vector, specifying for each of the 95 categories
whether the character is in that category.

Special Lisp functions are provided that abstract this, so you do not
have to directly manipulate bit vectors.
*/
       (object))
{
  return (CHAR_TABLEP (object) &&
	  XCHAR_TABLE_TYPE (object) == CHAR_TABLE_TYPE_CATEGORY) ?
    Qt : Qnil;
}

static Lisp_Object
check_category_table (Lisp_Object object, Lisp_Object default_)
{
  if (NILP (object))
    object = default_;
  while (NILP (Fcategory_table_p (object)))
    object = wrong_type_argument (Qcategory_table_p, object);
  return object;
}

int
check_category_char (Ichar ch, Lisp_Object table,
		     int designator, int not_p)
{
  REGISTER Lisp_Object temp;
  if (NILP (Fcategory_table_p (table)))
    wtaerror ("Expected category table", table);
  temp = get_char_table (ch, table);
  if (NILP (temp))
    return not_p;

  designator -= ' ';
  return bit_vector_bit (XBIT_VECTOR (temp), designator) ? !not_p : not_p;
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

  CHECK_INT (position);
  CHECK_CATEGORY_DESIGNATOR (designator);
  des = XCHAR (designator);
  ctbl = check_category_table (category_table, buf->category_table);
  ch = BUF_FETCH_CHAR (buf, XINT (position));
  return check_category_char (ch, ctbl, des, 0) ? Qt : Qnil;
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
  return check_category_char (ch, ctbl, des, 0) ? Qt : Qnil;
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
  if (NILP (Vstandard_category_table))
    return Fmake_char_table (Qcategory);

  category_table =
    check_category_table (category_table, Vstandard_category_table);
  return Fcopy_char_table (category_table);
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
  buf->local_var_flags |= XINT (buffer_local_flags.category_table);
  return category_table;
}

DEFUN ("category-designator-p", Fcategory_designator_p, 1, 1, 0, /*
Return t if OBJECT is a category designator (a char in the range ' ' to '~').
*/
       (object))
{
  return CATEGORY_DESIGNATORP (object) ? Qt : Qnil;
}

DEFUN ("category-table-value-p", Fcategory_table_value_p, 1, 1, 0, /*
Return t if OBJECT is a category table value.
Valid values are nil or a bit vector of size 95.
*/
       (object))
{
  return CATEGORY_TABLE_VALUEP (object) ? Qt : Qnil;
}


#define CATEGORYP(x) \
  (CHARP (x) && XCHAR (x) >= 0x20 && XCHAR (x) <= 0x7E)

#define CATEGORY_SET(c)	get_char_table (c, current_buffer->category_table)

/* Return 1 if CATEGORY_SET contains CATEGORY, else return 0.
   The faster version of `!NILP (Faref (category_set, category))'.  */
#define CATEGORY_MEMBER(category, category_set)		 	\
  (bit_vector_bit(XBIT_VECTOR (category_set), category - 32))

/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   Use the macro WORD_BOUNDARY_P instead of calling this function
   directly.  */

int
word_boundary_p (struct buffer *buf, Ichar c1, Ichar c2)
{
  Lisp_Object category_set1, category_set2;
  Lisp_Object tail;
  int default_result;

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

  category_set1 = CATEGORY_SET (c1);
  if (NILP (category_set1))
    return default_result;
  category_set2 = CATEGORY_SET (c2);
  if (NILP (category_set2))
    return default_result;

  for (; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt = XCAR (tail);

      if (CONSP (elt)
	  && CATEGORYP (XCAR (elt))
	  && CATEGORYP (XCDR (elt))
	  && CATEGORY_MEMBER (XCHAR (XCAR (elt)), category_set1)
	  && CATEGORY_MEMBER (XCHAR (XCDR (elt)), category_set2))
	return !default_result;
    }
  return default_result;
}
#endif /* MULE */


void
syms_of_chartab (void)
{
  INIT_LRECORD_IMPLEMENTATION (char_table);
  INIT_LRECORD_IMPLEMENTATION (char_subtable);

#ifdef MULE
  DEFSYMBOL (Qcategory_table_p);
  DEFSYMBOL (Qcategory_designator_p);
  DEFSYMBOL (Qcategory_table_value_p);
#endif /* MULE */

  DEFSYMBOL (Qchar_table);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qchar_tablep);

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
  DEFSUBR (Fcategory_table_p);
  DEFSUBR (Fcategory_table);
  DEFSUBR (Fstandard_category_table);
  DEFSUBR (Fcopy_category_table);
  DEFSUBR (Fset_category_table);
  DEFSUBR (Fcheck_category_at);
  DEFSUBR (Fchar_in_category_p);
  DEFSUBR (Fcategory_designator_p);
  DEFSUBR (Fcategory_table_value_p);
#endif /* MULE */

}

void
vars_of_chartab (void)
{
  /* DO NOT staticpro this.  It works just like Vweak_hash_tables. */
  Vall_syntax_tables = Qnil;
  dump_add_weak_object_chain (&Vall_syntax_tables);

  init_blank_chartab_tables ();

  staticpro (&chartab_blank[1]);
  staticpro (&chartab_blank[2]);
  staticpro (&chartab_blank[3]);
  staticpro (&chartab_blank[4]);
}

void
structure_type_create_chartab (void)
{
  struct structure_type *st;

  st = define_structure_type (Qchar_table, 0, chartab_instantiate);

  define_structure_type_keyword (st, Qtype, chartab_type_validate);
  define_structure_type_keyword (st, Qdata, chartab_data_validate);
}

void
complex_vars_of_chartab (void)
{
#ifdef MULE
  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-category-table
     so that copy-category-table will know not to try to copy from garbage */
  Vstandard_category_table = Qnil;
  Vstandard_category_table = Fcopy_category_table (Qnil);
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
