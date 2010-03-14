/* Code to handle Unicode conversion.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2009, 2010 Ben Wing.

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

/* Synched up with: FSF 20.3.  Not in FSF. */

/* Authorship:

   Current primary author: Ben Wing <ben@xemacs.org>

   Written by Ben Wing <ben@xemacs.org>, June, 2001.
   Separated out into this file, August, 2001.
   Includes Unicode coding systems, some parts of which have been written
   by someone else.  #### Morioka and Hayashi, I think.

   As of September 2001, the detection code is here and abstraction of the
   detection system is finished.  The unicode detectors have been rewritten
   to include multiple levels of likelihood.
   */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "elhash.h"
#include "extents.h"
#include "file-coding.h"
#include "opaque.h"
#include "rangetab.h"
#include "unicode.h"

#include "sysfile.h"

/* For more info about how Unicode works under Windows, see intl-win32.c. */

/* Info about Unicode translation tables [ben]:

   FORMAT:
   -------

   We currently use the following format for tables:

   If dimension == 1, to_unicode_table is a CHARSET_MAX_SIZE-element array
   of ints (Unicode code points); else, it's a CHARSET_MAX_SIZE-element
   array of int * pointers, each of which points to a
   CHARSET_MAX_SIZE-element array of ints.  If no elements in a row have
   been filled in, the pointer will point to a default empty table; that
   way, memory usage is more reasonable but lookup still fast.

   -- If from_unicode_levels == 1, from_unicode_table is a 256-element
   array of UINT_16_BITs (octet 1 in high byte, octet 2 in low byte; we don't
   store Ichars directly to save space).

   -- If from_unicode_levels == 2, from_unicode_table is a 256-element
   array of UINT_16_BIT * pointers, each of which points to a 256-element array
   of UINT_16_BITs.

   -- If from_unicode_levels == 3, from_unicode_table is a 256-element
   array of UINT_16_BIT ** pointers, each of which points to a 256-element
   array of UINT_16_BIT * pointers, each of which points to a 256-element
   array of UINT_16_BITs.

   -- If from_unicode_levels == 4, same thing but one level deeper.

   Just as for to_unicode_table, we use default tables to fill in all
   entries with no values in them.

   #### An obvious space-saving optimization is to use variable-sized
   tables, where each table instead of just being a 256-element array, is a
   structure with a start value, an end value, and a variable number of
   entries (END - START + 1).  Only 8 bits are needed for END and START,
   and could be stored at the end to avoid alignment problems.  However,
   before charging off and implementing this, we need to consider whether
   it's worth it:

   (1) Most tables will be highly localized in which code points are
   defined, heavily reducing the possible memory waste.  Before doing any
   rewriting, write some code to see how much memory is actually being
   wasted (i.e. ratio of empty entries to total # of entries) and only
   start rewriting if it's unacceptably high.  You have to check over all
   charsets.

   (2) Since entries are usually added one at a time, you have to be very
   careful when creating the tables to avoid realloc()/free() thrashing in
   the common case when you are in an area of high localization and are
   going to end up using most entries in the table.  You'd certainly want
   to allow only certain sizes, not arbitrary ones (probably powers of 2,
   where you want the entire block including the START/END values to fit
   into a power of 2, minus any malloc overhead if there is any -- there's
   none under gmalloc.c, and probably most system malloc() functions are
   quite smart nowadays and also have no overhead).  You could optimize
   somewhat during the in-C initializations, because you can compute the
   actual usage of various tables by scanning the entries you're going to
   add in a separate pass before adding them. (You could actually do the
   same thing when entries are added on the Lisp level by making the
   assumption that all the entries will come in one after another before
   any use is made of the data.  So as they're coming in, you just store
   them in a big long list, and the first time you need to retrieve an
   entry, you compute the whole table at once.) You'd still have to deal
   with the possibility of later entries coming in, though.

   (3) You do lose some speed using START/END values, since you need a
   couple of comparisons at each level.  This could easily make each single
   lookup become 3-4 times slower.  The Unicode book considers this a big
   issue, and recommends against variable-sized tables for this reason;
   however, they almost certainly have in mind applications that primarily
   involve conversion of large amounts of data.  Most Unicode strings that
   are translated in XEmacs are fairly small.  The only place where this
   might matter is in loading large files -- e.g. a 3-megabyte
   Unicode-encoded file.  So think about this, and maybe do a trial
   implementation where you don't worry too much about the intricacies of
   (2) and just implement some basic "multiply by 1.5" trick or something
   to do the resizing.  There is a very good FAQ on Unicode called
   something like the Linux-Unicode How-To (it should be part of the Linux
   How-To's, I think), that lists the url of a guy with a whole bunch of
   unicode files you can use to stress-test your implementations, and he's
   highly likely to have a good multi-megabyte Unicode-encoded file (with
   normal text in it -- if you created your own just by creating repeated
   strings of letters and numbers, you probably wouldn't get accurate
   results).

   INITIALIZATION:
   ---------------

   There are advantages and disadvantages to loading the tables at
   run-time.

   Advantages:

   They're big, and it's very fast to recreate them (a fraction of a second
   on modern processors).

   Disadvantages:

   (1) User-defined charsets: It would be inconvenient to require all
   dumped user-defined charsets to be reloaded at init time.

   NB With run-time loading, we load in init-mule-at-startup, in
   mule-cmds.el.  This is called from startup.el, which is quite late in
   the initialization process -- but data-directory isn't set until then.
   With dump-time loading, you still can't dump in a Japanese directory
   (again, until we move to Unicode internally), but this is not such an
   imposition.

   
*/

/* #### WARNING!  The current sledgehammer routines have a fundamental
   problem in that they can't handle two characters mapping to a
   single Unicode codepoint or vice-versa in a single charset table.
   It's not clear there is any way to handle this and still make the
   sledgehammer routines useful.

   Inquiring Minds Want To Know Dept: does the above WARNING mean that
   _if_ it happens, then it will signal error, or then it will do
   something evil and unpredictable?  Signaling an error is OK: for
   all national standards, the national to Unicode map is an inclusion
   (1-to-1).  Any character set that does not behave that way is
   broken according to the Unicode standard.

   Answer: You will get an ABORT(), since the purpose of the sledgehammer
   routines is self-checking.  The above problem with non-1-to-1 mapping
   occurs in the Big5 tables, as provided by the Unicode Consortium. */

/* #define SLEDGEHAMMER_CHECK_UNICODE */

/* When MULE is not defined, we may still need some Unicode support --
   in particular, some Windows API's always want Unicode, and the way
   we've set up the Unicode encapsulation, we may as well go ahead and
   always use the Unicode versions of split API's. (It would be
   trickier to not use them, and pointless -- under NT, the ANSI API's
   call the Unicode ones anyway, so in the case of structures, we'd be
   converting from Unicode to ANSI structures, only to have the OS
   convert them back.) */

Lisp_Object Qunicode;
Lisp_Object Qutf_16, Qutf_8, Qucs_4, Qutf_7, Qutf_32;
Lisp_Object Qneed_bom;

Lisp_Object Qutf_16_little_endian, Qutf_16_bom;
Lisp_Object Qutf_16_little_endian_bom;

Lisp_Object Qutf_8_bom;

extern int firstbyte_mask[];

#ifdef MULE
/* Default Unicode precedence list, set by
   set-default-unicode-precedence-list */
static Lisp_Object Vdefault_unicode_precedence_list;
/* Cached version of Vdefault_unicode_precedence_list, as a precedence array */
Lisp_Object Vdefault_unicode_precedence_array;
/* Cache mapping raw precedence lists to precedence arrays */
static Lisp_Object Vprecedence_list_to_array;
/* Cache mapping conses of pairs of precedence arrays to combined
   precedence arrays */
static Lisp_Object Vprecedence_array_cons_to_array;

static Lisp_Object Qcharset_tag_to_charset_list;

/* Used internally in the generation of precedence arrays, to keep
   track of charsets already seen */
Lisp_Object Vprecedence_array_charsets_seen_hash;

Lisp_Object Qignore_first_column;

#ifndef UNICODE_INTERNAL
Lisp_Object Vcharset_jit_ucs_charset_0;
Lisp_Object Vcurrent_jit_charset;
int last_allocated_jit_c1, last_allocated_jit_c2;
int number_of_jit_charsets;
Lisp_Object Vcharset_descr;
#endif
#endif /* MULE */

#ifdef MULE 

/* There is no single badval that will work for all cases with the from tables,
   because we allow arbitrary 256x256 charsets. #### This is a real problem;
   need a better fix.  One possibility is to compute a bad value that is
   outside the range of a particular charset, and have separate blank tables
   for each charset.  This still chokes on 256x256, but not anywhere else.
   The value of 0x0100 will not be valid in any dimension-1 charset (they
   always are of the form 0x0001), not valid in a ku-ten style charset, and
   not valid in any ISO-2022-like charset, or Shift-JIS, Big5, JOHAB, etc.;
   or any related charset, all of which try to avoid using the control
   character ranges.  Of course it *is* valid in Unicode, if someone tried
   to create a national unicode charset; but if we chose a value that is
   invalid in Unicode, it's likely to be valid for many other charsets; no
   win. */
#define BADVAL_FROM_TABLE ((UINT_16_BIT) 0x0100)
/* For the to tables we are safe, because -1 is never a valid Unicode
   codepoint. */
#define BADVAL_TO_TABLE (-1)

/* We use int for to_unicode; Unicode codepoints always fit into a signed
   32-bit value.

   We use UINT_16_BIT to store a charset codepoint, up to 256x256,
   unsigned to avoid problems.
*/
static int *to_unicode_blank_1;
static int **to_unicode_blank_2;

/* The lowest table is always null.  We do it this way so that the table index
   corresponds to the number of levels of the table, i.e. how many indices
   before you get an actual value rather than a pointer. */
static void *from_unicode_blank[5];

static const struct memory_description to_unicode_level_0_desc_1[] = {
  { XD_END }
};

static const struct sized_memory_description to_unicode_level_0_desc = {
  sizeof (int), to_unicode_level_0_desc_1
};

static const struct memory_description to_unicode_level_1_desc_1[] = {
  { XD_BLOCK_PTR, 0, CHARSET_MAX_SIZE, { &to_unicode_level_0_desc } },
  { XD_END }
};

static const struct sized_memory_description to_unicode_level_1_desc = {
  sizeof (void *), to_unicode_level_1_desc_1
};

static const struct memory_description to_unicode_description_1[] = {
  { XD_BLOCK_PTR, 1, CHARSET_MAX_SIZE, { &to_unicode_level_0_desc } },
  { XD_BLOCK_PTR, 2, CHARSET_MAX_SIZE, { &to_unicode_level_1_desc } },
  { XD_END }
};

/* Not static because each charset has a set of to and from tables and
   needs to describe them to pdump. */
const struct sized_memory_description to_unicode_description = {
  sizeof (void *), to_unicode_description_1
};

/* Used only for to_unicode_blank_2 */
static const struct memory_description to_unicode_level_2_desc_1[] = {
  { XD_BLOCK_PTR, 0, CHARSET_MAX_SIZE, { &to_unicode_level_1_desc } },
  { XD_END }
};

static const struct memory_description from_unicode_level_0_desc_1[] = {
  { XD_END }
};

static const struct sized_memory_description from_unicode_level_0_desc = {
   sizeof (UINT_16_BIT), from_unicode_level_0_desc_1
};

static const struct memory_description from_unicode_level_1_desc_1[] = {
  { XD_BLOCK_PTR, 0, 256, { &from_unicode_level_0_desc } },
  { XD_END }
};

static const struct sized_memory_description from_unicode_level_1_desc = {
  sizeof (void *), from_unicode_level_1_desc_1
};

static const struct memory_description from_unicode_level_2_desc_1[] = {
  { XD_BLOCK_PTR, 0, 256, { &from_unicode_level_1_desc } },
  { XD_END }
};

static const struct sized_memory_description from_unicode_level_2_desc = {
  sizeof (void *), from_unicode_level_2_desc_1
};

static const struct memory_description from_unicode_level_3_desc_1[] = {
  { XD_BLOCK_PTR, 0, 256, { &from_unicode_level_2_desc } },
  { XD_END }
};

static const struct sized_memory_description from_unicode_level_3_desc = {
  sizeof (void *), from_unicode_level_3_desc_1
};

static const struct memory_description from_unicode_description_1[] = {
  { XD_BLOCK_PTR, 1, 256, { &from_unicode_level_0_desc } },
  { XD_BLOCK_PTR, 2, 256, { &from_unicode_level_1_desc } },
  { XD_BLOCK_PTR, 3, 256, { &from_unicode_level_2_desc } },
  { XD_BLOCK_PTR, 4, 256, { &from_unicode_level_3_desc } },
  { XD_END }
};

/* Not static because each charset has a set of to and from tables and
   needs to describe them to pdump. */
const struct sized_memory_description from_unicode_description = {
  sizeof (void *), from_unicode_description_1
};

/* Used only for from_unicode_blank[4] */
static const struct memory_description from_unicode_level_4_desc_1[] = {
  { XD_BLOCK_PTR, 0, 256, { &from_unicode_level_3_desc } },
  { XD_END }
};

/* Break up a 32-bit character code into 8-bit parts. */

#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
#define TO_TABLE_SIZE_FROM_CHARSET(charset) 2
#define UNICODE_BREAKUP_CHAR_CODE(val, u1, u2, u3, u4, levels)	\
do {								\
  int buc_val = (val);						\
								\
  (u1) = buc_val >> 24;						\
  (u2) = (buc_val >> 16) & 255;					\
  (u3) = (buc_val >> 8) & 255;					\
  (u4) = buc_val & 255;						\
} while (0)
#else /* not MAXIMIZE_UNICODE_TABLE_DEPTH */
#define TO_TABLE_SIZE_FROM_CHARSET(charset) XCHARSET_DIMENSION (charset)
#define UNICODE_BREAKUP_CHAR_CODE(val, u1, u2, u3, u4, levels)	\
do {								\
  int buc_val = (val);						\
								\
  (u1) = buc_val >> 24;						\
  (u2) = (buc_val >> 16) & 255;					\
  (u3) = (buc_val >> 8) & 255;					\
  (u4) = buc_val & 255;						\
  (levels) = (buc_val <= 0xFF ? 1 :				\
	      buc_val <= 0xFFFF ? 2 :				\
	      buc_val <= 0xFFFFFF ? 3 :				\
	      4);						\
} while (0)
#endif /* (not) MAXIMIZE_UNICODE_TABLE_DEPTH */

#endif /* MULE */

#define UNICODE_DECODE_ERROR_OCTET(octet, dst, data, ignore_bom)	\
  decode_unicode_to_dynarr_0 ((octet) + UNICODE_ERROR_OCTET_RANGE_START, \
                              dst, data, ignore_bom)


/************************************************************************/
/*                        Unicode implementation                        */
/************************************************************************/

/* Given a Lisp_Object that is supposed to represent a Unicode codepoint,
   make sure it does, and return it. */

int
decode_unicode (Lisp_Object unicode, enum unicode_allow allow)
{
  EMACS_INT val;
  CHECK_INT (unicode);
  val = XINT (unicode);
  if (!valid_unicode_codepoint_p (val, allow))
    {
      EMACS_INT maxval = allow == UNICODE_ALLOW_PRIVATE ?
	EMACS_INT_UNICODE_PRIVATE_MAX : UNICODE_OFFICIAL_MAX;
      args_out_of_range_3 (unicode, Qzero, make_int (maxval));
    }
  return (int) val;
}

#ifdef MULE

static void
init_blank_unicode_tables (void)
{
  int i;

  from_unicode_blank[0] = NULL;
  from_unicode_blank[1] = xnew_array (UINT_16_BIT, 256);
  from_unicode_blank[2] = xnew_array (UINT_16_BIT *, 256);
  from_unicode_blank[3] = xnew_array (UINT_16_BIT **, 256);
  from_unicode_blank[4] = xnew_array (UINT_16_BIT ***, 256);
  for (i = 0; i < 256; i++)
    {
      /* See comment above on BADVAL_FROM_TABLE */
      ((UINT_16_BIT *) from_unicode_blank[1])[i] = BADVAL_FROM_TABLE;
      ((void **) from_unicode_blank[2])[i] = from_unicode_blank[1];
      ((void **) from_unicode_blank[3])[i] = from_unicode_blank[2];
      ((void **) from_unicode_blank[4])[i] = from_unicode_blank[3];
    }

  to_unicode_blank_1 = xnew_array (int, CHARSET_MAX_SIZE);
  to_unicode_blank_2 = xnew_array (int *, CHARSET_MAX_SIZE);
  for (i = 0; i < CHARSET_MAX_SIZE; i++)
    {
      /* Likewise for BADVAL_TO_TABLE */
      to_unicode_blank_1[i] = BADVAL_TO_TABLE;
      to_unicode_blank_2[i] = to_unicode_blank_1;
    }
}

static void *
create_new_from_unicode_table (int level)
{
  /* WARNING: sizeof (UINT_16_BIT) != sizeof (UINT_16_BIT *). */
  Bytecount size = level == 1 ? sizeof (UINT_16_BIT) : sizeof (void *);
  void *newtab;

  text_checking_assert (level >= 1 && level <= 4);
  newtab = xmalloc (256 * size);
  memcpy (newtab, from_unicode_blank[level], 256 * size);
  return newtab;
}

#if 0

/* If we ever implement the idea of finding a value for BADVAL_FROM_TABLE
   that is specific to a particular charset, the following function may
   prove useful.  It's not clear whether there's much of a point doing
   this, because we still run into the problem of dealing with a charset
   that is 256x256, where there is no safe value of BADVAL_FROM_TABLE.
   Furthermore, if we implement this, we have to change the handling of the
   blank from-tables: The zero-level blank from-table has BADVAL_FROM_TABLE
   stored in it, so we'd need to have charset-specific blank from-tables
   with the charset-specific value of BADVAL_FROM_TABLE stored in them. */

static int
find_badval_for_charset (Lisp_Object charset)
{
  int val1, val2;
  int low1, low2, high1, high2;

  get_charset_limits (charset, &low1, &low2, &high1, &high2);
  if (low1 > 0)
    val1 = 0;
  else if (high1 < 255)
    val1 = 255;
  else
    val1 = 1;
  if (low2 > 0)
    val2 = 0;
  else if (high2 < 255)
    val2 = 255;
  else
    val2 = 1;
  return (val1 << 8) + val2;
}

#endif /* 0 */

/* Allocate and blank the tables.
   Loading them up is done by load-unicode-mapping-table. */
void
init_charset_unicode_tables (Lisp_Object charset)
{
  if (TO_TABLE_SIZE_FROM_CHARSET (charset) == 1)
    {
      int *to_table = xnew_array (int, CHARSET_MAX_SIZE);
      memcpy (to_table, to_unicode_blank_1, CHARSET_MAX_SIZE * sizeof (int));
      XCHARSET_TO_UNICODE_TABLE (charset) = to_table;
    }
  else
    {
      int **to_table = xnew_array (int *, CHARSET_MAX_SIZE);
      memcpy (to_table, to_unicode_blank_2, CHARSET_MAX_SIZE * sizeof (int *));
      XCHARSET_TO_UNICODE_TABLE (charset) = to_table;
    }

#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
  XCHARSET_FROM_UNICODE_TABLE (charset) = create_new_from_unicode_table (4);
  XCHARSET_FROM_UNICODE_LEVELS (charset) = 4;
#else
  XCHARSET_FROM_UNICODE_TABLE (charset) = create_new_from_unicode_table (1);
  XCHARSET_FROM_UNICODE_LEVELS (charset) = 1;
#endif /* MAXIMIZE_UNICODE_TABLE_DEPTH */

  XCHARSET_BADVAL_UNICODE_CODE (charset) = -1;
}

static void
free_from_unicode_table (void *table, int level)
{
  if (level >= 2)
    {
      void **tab = (void **) table;
      int i;

      for (i = 0; i < 256; i++)
	{
	  if (tab[i] != from_unicode_blank[level - 1])
	    free_from_unicode_table (tab[i], level - 1);
	}
    }

  xfree (table);
}

static void
free_to_unicode_table (void *table, int level)
{
  if (level == 2)
    {
      int i;
      int **tab = (int **) table;

      for (i = 0; i < CHARSET_MAX_SIZE; i++)
	{
	  if (tab[i] != to_unicode_blank_1)
	    free_to_unicode_table (tab[i], 1);
	}
    }

  xfree (table);
}

void
free_charset_unicode_tables (Lisp_Object charset)
{
  free_to_unicode_table (XCHARSET_TO_UNICODE_TABLE (charset),
			 TO_TABLE_SIZE_FROM_CHARSET (charset));
  free_from_unicode_table (XCHARSET_FROM_UNICODE_TABLE (charset),
			   XCHARSET_FROM_UNICODE_LEVELS (charset));
}

#ifdef MEMORY_USAGE_STATS

static Bytecount
compute_from_unicode_table_size_1 (void *table, int level,
				   struct overhead_stats *stats)
{
  Bytecount size = 0;

  if (level >= 2)
    {
      int i;
      void **tab = (void **) table;
      for (i = 0; i < 256; i++)
	{
	  if (tab[i] != from_unicode_blank[level - 1])
	    size += compute_from_unicode_table_size_1 (tab[i], level - 1,
						       stats);
	}
    }

  size += malloced_storage_size (table,
				 256 * (level == 1 ? sizeof (UINT_16_BIT) :
					sizeof (void *)),
				 stats);
  return size;
}

static Bytecount
compute_to_unicode_table_size_1 (void *table, int level,
				 struct overhead_stats *stats)
{
  Bytecount size = 0;

  if (level == 2)
    {
      int i;
      int **tab = (int **) table;

      for (i = 0; i < CHARSET_MAX_SIZE; i++)
	{
	  if (tab[i] != to_unicode_blank_1)
	    size += compute_to_unicode_table_size_1 (tab[i], 1, stats);
	}
    }

  size += malloced_storage_size (table,
				 CHARSET_MAX_SIZE *
				 (level == 1 ? sizeof (int) :
				  sizeof (void *)),
				 stats);
  return size;
}

Bytecount
compute_from_unicode_table_size (Lisp_Object charset,
				 struct overhead_stats *stats)
{
  return (compute_from_unicode_table_size_1
	  (XCHARSET_FROM_UNICODE_TABLE (charset),
	   XCHARSET_FROM_UNICODE_LEVELS (charset),
	   stats));
}

Bytecount
compute_to_unicode_table_size (Lisp_Object charset,
			       struct overhead_stats *stats)
{
  return (compute_to_unicode_table_size_1
	  (XCHARSET_TO_UNICODE_TABLE (charset),
	   TO_TABLE_SIZE_FROM_CHARSET (charset),
	   stats));
}

#endif

#ifdef SLEDGEHAMMER_CHECK_UNICODE

/* "Sledgehammer checks" are checks that verify the self-consistency
   of an entire structure every time a change is about to be made or
   has been made to the structure.  Not fast but a pretty much
   sure-fire way of flushing out any incorrectnesses in the algorithms
   that create the structure.

   Checking only after a change has been made will speed things up by
   a factor of 2, but it doesn't absolutely prove that the code just
   checked caused the problem; perhaps it happened elsewhere, either
   in some code you forgot to sledgehammer check or as a result of
   data corruption. */

static void
assert_not_any_blank_table (void *tab)
{
  assert (tab != from_unicode_blank[1]);
  assert (tab != from_unicode_blank[2]);
  assert (tab != from_unicode_blank[3]);
  assert (tab != from_unicode_blank[4]);
  assert (tab != to_unicode_blank_1);
  assert (tab != to_unicode_blank_2);
  assert (tab);
}

static void
sledgehammer_check_from_table (Lisp_Object charset, void *table, int level,
			       int codetop)
{
  int i;

  switch (level)
    {
    case 1:
      {
	UINT_16_BIT *tab = (UINT_16_BIT *) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != BADVAL_FROM_TABLE)
	      {
		int c1, c2;

		c1 = tab[i] >> 8;
		c2 = tab[i] & 0xFF;
		assert_codepoint_in_range (charset, c1, c2);
		if (TO_TABLE_SIZE_FROM_CHARSET (charset) == 1)
		  {
		    int *to_table =
		      (int *) XCHARSET_TO_UNICODE_TABLE (charset);
		    assert_not_any_blank_table (to_table);
		    assert (to_table[c2 - CHARSET_MIN_OFFSET] ==
			    (codetop << 8) + i);
		  }
		else
		  {
		    int **to_table =
		      (int **) XCHARSET_TO_UNICODE_TABLE (charset);
		    assert_not_any_blank_table (to_table);
		    assert_not_any_blank_table
		      (to_table[c1 - CHARSET_MIN_OFFSET]);
		    assert (to_table[c1 - CHARSET_MIN_OFFSET]
			    [c2 - CHARSET_MIN_OFFSET] == (codetop << 8) + i);
		  }
	      }
	  }
	break;
      }
    case 2:
    case 3:
    case 4:
      {
	void **tab = (void **) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank[level - 1])
	      sledgehammer_check_from_table (charset, tab[i], level - 1,
					     (codetop << 8) + i);
	  }
	break;
      }
    default:
      ABORT ();
    }
}

static void
sledgehammer_check_to_table (Lisp_Object charset, void *table, int level,
			     int codetop)
{
  int i;
  int low1, low2, high1, high2;

  get_charset_limits (charset, &low1, &low2, &high1, &high2);

  switch (level)
    {
    case 1:
      {
	int *tab = (int *) table;

	if (TO_TABLE_SIZE_FROM_CHARSET (charset) == 2)
	  /* This means we're traversing a nested table */
	  low1 = low2, high1 = high2;
	for (i = 0; i < CHARSET_MAX_SIZE; i++)
	  {
	    /* Make sure no out-of-bounds characters were set */
	    if (i + CHARSET_MIN_OFFSET < low1 ||
		i + CHARSET_MIN_OFFSET > high1)
	      assert (tab[i] == BADVAL_TO_TABLE);
	    if (tab[i] != BADVAL_TO_TABLE)
	      {
		int u4, u3, u2, u1, levels;
		UINT_16_BIT val;
		void *frtab = XCHARSET_FROM_UNICODE_TABLE (charset);

		assert (tab[i] >= 0);
		UNICODE_BREAKUP_CHAR_CODE (tab[i], u4, u3, u2, u1, levels);
#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
		levels = 4;
#endif /* MAXIMIZE_UNICODE_TABLE_DEPTH */
		assert (levels <= XCHARSET_FROM_UNICODE_LEVELS (charset));

		switch (XCHARSET_FROM_UNICODE_LEVELS (charset))
		  {
		  case 1: val = ((UINT_16_BIT *) frtab)[u1]; break;
		  case 2: val = ((UINT_16_BIT **) frtab)[u2][u1]; break;
		  case 3: val = ((UINT_16_BIT ***) frtab)[u3][u2][u1]; break;
		  case 4: val = ((UINT_16_BIT ****) frtab)[u4][u3][u2][u1];
		    break;
		  default: ABORT ();
		  }

		if (TO_TABLE_SIZE_FROM_CHARSET (charset) == 1)
		  {
		    assert (i + CHARSET_MIN_OFFSET == (val >> 8));
		    assert (0 == (val & 0xFF));
		  }
		else
		  {
		    assert (codetop + CHARSET_MIN_OFFSET == (val >> 8));
		    assert (i + CHARSET_MIN_OFFSET == (val & 0xFF));
		  }

		switch (XCHARSET_FROM_UNICODE_LEVELS (charset))
		  {
		  case 4:
		    assert_not_any_blank_table (frtab);
		    frtab = ((UINT_16_BIT ****) frtab)[u4];
		    /* fall through */
		  case 3:
		    assert_not_any_blank_table (frtab);
		    frtab = ((UINT_16_BIT ***) frtab)[u3];
		    /* fall through */
		  case 2:
		    assert_not_any_blank_table (frtab);
		    frtab = ((UINT_16_BIT **) frtab)[u2];
		    /* fall through */
		  case 1:
		    assert_not_any_blank_table (frtab);
		    break;
		  default: ABORT ();
		  }
	      }
	  }
	break;
      }
    case 2:
      {
	int **tab = (int **) table;

	for (i = 0; i < CHARSET_MAX_SIZE; i++)
	  {
	    /* Make sure no out-of-bounds characters were set */
	    if (i + CHARSET_MIN_OFFSET < low1 ||
		i + CHARSET_MIN_OFFSET > high1)
	      assert (tab[i] == to_unicode_blank_1);
	    if (tab[i] != to_unicode_blank_1)
	      sledgehammer_check_to_table (charset, tab[i], 1, i);
	  }
	break;
      }
    default:
      ABORT ();
    }
}

static void
sledgehammer_check_unicode_tables (Lisp_Object charset)
{
  /* verify that the blank tables have not been modified */
  int i;
  int from_level = XCHARSET_FROM_UNICODE_LEVELS (charset);
  int to_level = XCHARSET_FROM_UNICODE_LEVELS (charset);

  for (i = 0; i < 256; i++)
    {
      assert (((UINT_16_BIT *) from_unicode_blank[1])[i] ==
	      BADVAL_FROM_TABLE);
      assert (((void **) from_unicode_blank[2])[i] == from_unicode_blank[1]);
      assert (((void **) from_unicode_blank[3])[i] == from_unicode_blank[2]);
      assert (((void **) from_unicode_blank[4])[i] == from_unicode_blank[3]);
    }

  for (i = 0; i < CHARSET_MAX_SIZE; i++)
    {
      assert (to_unicode_blank_1[i] == BADVAL_TO_TABLE);
      assert (to_unicode_blank_2[i] == to_unicode_blank_1);
    }

  assert (from_level >= 1 && from_level <= 4);

  sledgehammer_check_from_table (charset,
				 XCHARSET_FROM_UNICODE_TABLE (charset),
				 from_level, 0);

  sledgehammer_check_to_table (charset,
			       XCHARSET_TO_UNICODE_TABLE (charset),
			       TO_TABLE_SIZE_FROM_CHARSET (charset), 0);
}

#endif /* SLEDGEHAMMER_CHECK_UNICODE */

static void
set_unicode_conversion_char_to_unicode (int code, Lisp_Object charset,
					int c1, int c2)
{
  /* First, the char -> unicode translation */

  if (TO_TABLE_SIZE_FROM_CHARSET (charset) == 1)
    {
      int *to_table = (int *) XCHARSET_TO_UNICODE_TABLE (charset);
      to_table[c2 - CHARSET_MIN_OFFSET] = code;
    }
  else
    {
      int **to_table_2 = (int **) XCHARSET_TO_UNICODE_TABLE (charset);
      int *to_table_1;

      to_table_1 = to_table_2[c1 - CHARSET_MIN_OFFSET];
      if (to_table_1 == to_unicode_blank_1)
	{
	  to_table_1 = xnew_array (int, CHARSET_MAX_SIZE);
	  memcpy (to_table_1, to_unicode_blank_1,
		  CHARSET_MAX_SIZE * sizeof (int));
	  to_table_2[c1 - CHARSET_MIN_OFFSET] = to_table_1;
	}
      to_table_1[c2 - CHARSET_MIN_OFFSET] = code;
    }
}

static void
set_unicode_conversion_unicode_to_char (int code, Lisp_Object charset,
					int combined_code)
{
  /* Then, unicode -> char: much harder */

  {
    int levels;
    int u4, u3, u2, u1;
#ifndef MAXIMIZE_UNICODE_TABLE_DEPTH
    int code_levels;
#endif /* not MAXIMIZE_UNICODE_TABLE_DEPTH */
    UNICODE_BREAKUP_CHAR_CODE (code, u4, u3, u2, u1, code_levels);

    levels = XCHARSET_FROM_UNICODE_LEVELS (charset);
    text_checking_assert (levels >= 1 && levels <= 4);

#ifndef MAXIMIZE_UNICODE_TABLE_DEPTH
    text_checking_assert (code_levels <= 4);
    /* Make sure the charset's tables have at least as many levels as
       the code point has: Note that the charset is guaranteed to have
       at least one level, because it was created that way */
    if (levels < code_levels)
      {
	int i;

	for (i = 2; i <= code_levels; i++)
	  {
	    if (levels < i)
	      {
		void *old_table = XCHARSET_FROM_UNICODE_TABLE (charset);
		void *table = create_new_from_unicode_table (i);
		XCHARSET_FROM_UNICODE_TABLE (charset) = table;
		((void **) table)[0] = old_table;
	      }
	  }

	levels = code_levels;
	XCHARSET_FROM_UNICODE_LEVELS (charset) = code_levels;
      }
#endif /* not MAXIMIZE_UNICODE_TABLE_DEPTH */

    /* Now, make sure there is a non-default table at each level */
    {
      int i;
      void *table = XCHARSET_FROM_UNICODE_TABLE (charset);

      for (i = levels; i >= 2; i--)
	{
	  int ind;

	  switch (i)
	    {
	    case 4: ind = u4; break;
	    case 3: ind = u3; break;
	    case 2: ind = u2; break;
	    default: ind = 0; ABORT ();
	    }

	  if (((void **) table)[ind] == from_unicode_blank[i - 1])
	    ((void **) table)[ind] =
	      ((void *) create_new_from_unicode_table (i - 1));
	  table = ((void **) table)[ind];
	}
    }

    /* Finally, set the character */
	  
    {
      void *table = XCHARSET_FROM_UNICODE_TABLE (charset);
#ifndef MAXIMIZE_UNICODE_TABLE_DEPTH
      switch (levels)
	{
	case 4: ((UINT_16_BIT ****) table)[u4][u3][u2][u1] = combined_code; break;
	case 3: ((UINT_16_BIT ***) table)[u3][u2][u1] = combined_code; break;
	case 2: ((UINT_16_BIT **) table)[u2][u1] = combined_code; break;
	case 1: ((UINT_16_BIT *) table)[u1] = combined_code; break;
	default:  ABORT ();
	}
#else /* MAXIMIZE_UNICODE_TABLE_DEPTH */
      ((UINT_16_BIT ****) table)[u4][u3][u2][u1] = combined_code;
#endif /* not MAXIMIZE_UNICODE_TABLE_DEPTH */
    }
  }
}

/* Actual function to store a conversion between charset codepoint (C1, C2)
   in CHARSET and Unicode codepoint CODE.  If CODE is -1, remove any
   conversion for the charset codepoint. */

static void
set_unicode_conversion (int code, Lisp_Object charset, int c1, int c2)
{
  int old_code = charset_codepoint_to_unicode (charset, c1, c2, CONVERR_FAIL);
  int combined_code = (c1 << 8) + c2;

  ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
  if (code != -1)
    /* @@#### Is UNICODE_ALLOW_PRIVATE correct here?  If so, replace with
       ASSERT_VALID_UNICODE_CODEPOINT(). */
    text_checking_assert (valid_unicode_codepoint_p (code,
						     UNICODE_ALLOW_PRIVATE));

  /* I tried an assert on code > 255 || chr == code, but that fails because
     Mule gives many Latin characters separate code points for different
     ISO 8859 coded character sets.  Obvious in hindsight.... */
  text_checking_assert (!EQ (charset, Vcharset_ascii) || code == c2);
  text_checking_assert (!EQ (charset, Vcharset_control_1) || code == c2);
  text_checking_assert (!EQ (charset, Vcharset_latin_iso8859_1) ||
			code == c2);

  /* This assert is needed because it is simply unimplemented. */
  text_checking_assert (!EQ (charset, Vcharset_composite));

  if (old_code >= 0 && old_code <= 127)
    {
      assert (XCHARSET (charset)->number_of_ascii_mappings > 0);
      XCHARSET (charset)->number_of_ascii_mappings--;
    }
  if (code >= 0 && code <= 127)
    XCHARSET (charset)->number_of_ascii_mappings++;

#ifdef SLEDGEHAMMER_CHECK_UNICODE
  sledgehammer_check_unicode_tables (charset);
#endif

  set_unicode_conversion_char_to_unicode (code, charset, c1, c2);
  if (code != -1)
    set_unicode_conversion_unicode_to_char (code, charset, combined_code);

  /* If there was a previous mapping, we have to erase the mapping in the
     unicode->char direction; else it will persist and we will get incorrect
     results when doing a conversion from the previous Unicode value to
     a charset codepoint. */
  if (old_code >= 0 && old_code != code)
    set_unicode_conversion_unicode_to_char (old_code, charset,
					    BADVAL_FROM_TABLE);
  if (combined_code == BADVAL_FROM_TABLE)
    XCHARSET_BADVAL_UNICODE_CODE (charset) = code;
  else if (XCHARSET_BADVAL_UNICODE_CODE (charset) == old_code)
    XCHARSET_BADVAL_UNICODE_CODE (charset) = -1;

#ifdef SLEDGEHAMMER_CHECK_UNICODE
  sledgehammer_check_unicode_tables (charset);
#endif
}

#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
#define USED_IF_NOT_MUTD(arg) UNUSED (arg)
#else
#define USED_IF_NOT_MUTD(arg) arg
#endif

/* Actual implementation of lookup of a conversion mapping for Unicode
   codepoint CODE in CHARSET.  Requires extra arguments passed in that are
   the result of calling UNICODE_BREAKUP_CHAR_CODE() on code.
   Returns non-zero if mapping found. */

inline static int
get_unicode_conversion_1 (int code, int u1, int u2, int u3, int u4,
			  int USED_IF_NOT_MUTD (code_levels),
			  Lisp_Object charset, int *c1, int *c2)
{
  void *table;

  table = XCHARSET_FROM_UNICODE_TABLE (charset);
#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
  if (!table)
    {
      int algo_low = XCHARSET_ALGO_LOW (charset);
      text_checking_assert (algo_low >= 0);
      if (code >= algo_low &&
	  code < algo_low +
	  XCHARSET_CHARS (charset, 0) * XCHARSET_CHARS (charset, 1))
	{
	  code -= algo_low;
	  *c1 = code / XCHARSET_CHARS (charset, 1);
	  *c2 = code % XCHARSET_CHARS (charset, 1);
	  *c1 += XCHARSET_OFFSET (charset, 0);
	  *c2 += XCHARSET_OFFSET (charset, 1);
	  return 1;
	}
      return 0;
    }
#endif /* ALLOW_ALGORITHMIC_CONVERSION_TABLES */
  {
    UINT_16_BIT retval;
#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
    retval = ((UINT_16_BIT ****) table)[u4][u3][u2][u1];
#else
    int levels = XCHARSET_FROM_UNICODE_LEVELS (charset);
    if (levels >= code_levels)
      {
	switch (levels)
	  {
	  case 1: retval = ((UINT_16_BIT *) table)[u1]; break;
	  case 2: retval = ((UINT_16_BIT **) table)[u2][u1]; break;
	  case 3: retval = ((UINT_16_BIT ***) table)[u3][u2][u1]; break;
	  case 4: retval = ((UINT_16_BIT ****) table)[u4][u3][u2][u1]; break;
	  default: ABORT (); retval = 0;
	  }
      }
#endif /* MAXIMIZE_UNICODE_TABLE_DEPTH */
    if (retval != BADVAL_FROM_TABLE)
      {
      found_value:
	*c1 = retval >> 8;
	*c2 = retval & 0xFF;
	return 1;
      }
    if (code == XCHARSET_BADVAL_UNICODE_CODE (charset))
      {
	retval = BADVAL_FROM_TABLE;
	goto found_value;
      }
  }
  return 0;
}

/* Convert a Unicode codepoint to a charset codepoint of a specified
   charset CHARSET.  Return non-zero if codepoint found. */

int
unicode_to_one_charset_codepoint (int code, Lisp_Object charset,
				  int *c1, int *c2)
{
  int u1, u2, u3, u4;
  int code_levels = 0;

  UNICODE_BREAKUP_CHAR_CODE (code, u4, u3, u2, u1, code_levels);
  return get_unicode_conversion_1 (code, u1, u2, u3, u4, code_levels, charset,
				   c1, c2);
}

#ifndef UNICODE_INTERNAL

static void
allocate_jit_ucs_charset (void)
{
  Ascbyte setname[100];

  sprintf (setname, "jit-ucs-charset-%d", number_of_jit_charsets);

  Vcurrent_jit_charset = Fmake_charset 
    (intern (setname), Vcharset_descr, 
     nconc2 (list6 (Qcolumns, make_int (1), Qchars,
		    make_int (96),
		    Qdimension, make_int (2)),
	     list4 (Qregistries, Qunicode_registries,
		    /* not allowed to set non-nil tags when not yet
		       initialized, for bootstrapping reasons; these
		       get set in mule-charset.el */
		    Qtags, initialized ? list2 (intern ("jit"), Qinternal)
		    : Qnil)));
  XCHARSET (Vcurrent_jit_charset)->jit_charset_p = 1;
  last_allocated_jit_c1 = last_allocated_jit_c2 = 32;

  number_of_jit_charsets++;
}

/* Return a free JIT codepoint.  Return 1 on success, 0 on failure.
   (Currently never returns 0.  Presumably if we ever run out of JIT charsets,
   we will signal an error in Fmake_charset().) */

static int
get_free_jit_codepoint (Lisp_Object *charset, int *c1, int *c2)
{
  if (!NILP (Vcurrent_jit_charset) &&
      !(last_allocated_jit_c1 == 127 && last_allocated_jit_c2 == 127))
    {
      if (127 == last_allocated_jit_c2)
	{
	  ++last_allocated_jit_c1;
	  last_allocated_jit_c2 = 0x20;
	}
      else
	{
	  ++last_allocated_jit_c2;
	}
    }
  else
    allocate_jit_ucs_charset ();
  *charset = Vcurrent_jit_charset;
  *c1 = last_allocated_jit_c1;
  *c2 = last_allocated_jit_c2;
  ASSERT_VALID_CHARSET_CODEPOINT (*charset, *c1, *c2);
  
  return 1;
}

#endif /* not UNICODE_INTERNAL */

/* Convert a Unicode codepoint to a charset codepoint.  PRECEDENCE_LIST is
   a list of charsets.  The charsets will be consulted in order for
   characters that match the Unicode codepoint.  If PREDICATE is non-NULL,
   only charsets that pass the predicate will be considered.

   Under old-Mule, if a charset codepoint cannot be found and
   jit-ucs-charset-0 is in the precedence list, create a "just-in-time"
   (JIT) character in one of the JIT charsets (named `jit-ucs-charset-*')
   and return it.  If necessary, create a new JIT charset to hold the
   character.  This is done so that unfamiliar Unicode codepoints in
   external files can be correctly displayed in a buffer and written out
   again to a file, rather than displaying a '~' or the like and
   corrupting the external file by writing out a replacement character.

   The JIT creation mechanism happens only when jit-ucs-charset-0 is
   present so that conversion involving user-supplied precedence lists
   correctly fails.  The presence of jit-ucs-charset-0 is a signal that a
   character can be created in this or another JIT charset, and it is
   normally only present in global or buffer-local precedence lists.  We
   create jit-ucs-charset-0 during initialization to ensure that it is
   always present even before the first JIT character has been created.
 */

void
unicode_to_charset_codepoint_raw (int code,
				  Lisp_Object precarray,
				  int (*predicate) (Lisp_Object),
				  Lisp_Object *charset,
				  int *c1, int *c2)
{
  int u1, u2, u3, u4;
  int code_levels = 0;
  int i;
  Lisp_Object_dynarr *precdyn = XPRECEDENCE_ARRAY_DYNARR (precarray);
  int n = Dynarr_length (precdyn);

  ASSERT_VALID_UNICODE_CODEPOINT (code);

  UNICODE_BREAKUP_CHAR_CODE (code, u4, u3, u2, u1, code_levels);

  for (i = 0; i < n; i++)
    {
      *charset = Dynarr_at (precdyn, i);
      if (predicate && !(*predicate) (*charset))
	continue;
      if (get_unicode_conversion_1 (code, u1, u2, u3, u4, code_levels,
				    *charset, c1, c2))
	goto done;
    }

  /* Unable to convert */

#ifndef UNICODE_INTERNAL
  /* Non-Unicode-internal: Maybe do just-in-time assignment */

  for (i = 0; i < n; i++)
    {
      if (EQ (Dynarr_at (precdyn, i), Vcharset_jit_ucs_charset_0))
	{
	  get_free_jit_codepoint (charset, c1, c2);
	  set_unicode_conversion (code, *charset, *c1, *c2);
	  goto done;
	}
    }
#endif /* not UNICODE_INTERNAL */

  /* Unable to convert; try the private codepoint range -- i.e. a private
     Unicode codepoint generated to maintain round-trip conversion with
     unknown charset codepoints. */
  private_unicode_to_charset_codepoint (code, charset, c1, c2);
  return;

done:
  ASSERT_VALID_CHARSET_CODEPOINT (*charset, *c1, *c2);
  return;
}

/*
  Convert a charset codepoint to a private Unicode codepoint, for round-trip
  conversion involving charset codepoints with no Unicode mapping.  Used
  when handling external files with charset codepoints.

 @@####
 there has to be a way for Lisp callers to *always* request the private
 codepoint if they want it. (#### EXPLAIN???)

 Possibly, we want always-private characters to behave somewhat like
 their normal equivalents, when such exists; e.g. when retrieving
 a property from a char table using such a char, try to retrieve a
 normal Unicode value and use its properties instead.  When setting,
 do a similar switcheroo.  Maybe at redisplay time also, and anywhere
 else we access properties of a char, do similar switching.
 This might be necessary to get recompilation to really work right,
 I'm not sure.  Hold off on this until necessary.
*/

int
charset_codepoint_to_private_unicode (Lisp_Object charset, int c1, int c2)
{
  /* If the charset is ISO2022-compatible, try to use a representation
     that is portable, in case we are writing out to an external
     Unicode representation.  Otherwise, just do something.

     What we use is this:

     If ISO2022-compatible:

       23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
        1 <---> <------------------> <------------------> <------------------>
            1            2                    3                    4

     Field 1 is the type (94, 96, 94x94, 96x96).
     Field 2 is the final byte.
     Field 3 is the first octet.
     Field 4 is the second octet.
     Bit 23 is set so that we are above all possible UTF-16 chars.
     Bits 24 and up are 0 to ensure that we don't conflict with
       non-ISO2022-compatible private characters (see below).

 <-... 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
 <-..------------------------> <---------------------> <--------------------->
              1                           2                       3

     If non-ISO2022-compatible:

     Field 1 (extends up to 31 bits) is the charset ID + 256, to place it
     above all ISO2022-compatible private characters. (It would seem that
     we can rely on charset ID >= 256 anyway, since non-encodable charset
     ID's are always >= 256.  But in fact, not all ISO2022-compatible
     charsets are encodable, or vice-versa; see
     get_charset_iso2022_type().)
    
     Fields 2 and 3 are the octet values.
  */
  int type = get_charset_iso2022_type (charset);

  /* NOTE NOTE NOTE: It's important that our private codepoints here not
     conflict with the private codepoints used for encoding error octets,
     as defined by UNICODE_ERROR_OCTET_RANGE_START. */
  text_checking_assert (UNICODE_ERROR_OCTET_RANGE_END < 0x800000);

  if (type >= 0)
    {
      /* The types are defined between 0 and 3 in charset.h; make sure
	 someone doesn't change them */
      text_checking_assert (type <= 3);
      c1 &= 127;
      c2 &= 127;
      return 0x800000 + (type << 21) + (XCHARSET_FINAL (charset) << 14)
	+ (c1 << 7) + c2;
    }
  else
    {
      int retval = ((256 + XCHARSET_ID (charset)) << 16) + (c1 << 8) + c2;
      /* Check for overflow */
      if (!valid_unicode_codepoint_p (retval, UNICODE_ALLOW_PRIVATE))
	retval = CANT_CONVERT_CHAR_WHEN_ENCODING_UNICODE;
      return retval;
    }
}

void
private_unicode_to_charset_codepoint (int priv, Lisp_Object *charset,
				      int *c1, int *c2)
{
  if (priv >= 0x1000000)
    {
      *charset = charset_by_id ((priv >> 16) - 256);
      *c1 = (priv >> 8) & 0xFF;
      *c2 = priv & 0xFF;
    }
  else if (priv >= 0x800000)
    {
      int type;
      int final;
      
      priv -= 0x800000;
      type = (priv >> 21) & 3;
      final = (priv >> 14) & 0x7F;
      *c1 = (priv >> 7) & 0x7F;
      *c2 = priv & 0x7F;
      *charset = charset_by_attributes (type, final, CHARSET_LEFT_TO_RIGHT);
      if (NILP (*charset))
	*charset = charset_by_attributes (type, final, CHARSET_RIGHT_TO_LEFT);
      if (!NILP (*charset))
	{
	  if (XCHARSET_OFFSET (*charset, 0) >= 128)
	    *c1 += 128;
	  if (XCHARSET_OFFSET (*charset, 1) >= 128)
	    *c2 += 128;
	}
    }
  else
    {
      /* @@#### Better error recovery? */
      *charset = Qnil;
      *c1 = 0;
      *c2 = 0;
    }
  if (!NILP (*charset) && !valid_charset_codepoint_p (*charset, *c1, *c2))
    /* @@#### Better error recovery? */
    {
      /* @@#### Better error recovery? */
      *charset = Qnil;
      *c1 = 0;
      *c2 = 0;
    }
}

#endif /* MULE */


/***************************************************************************/
/*                   Unicode precedence lists and arrays                   */
/***************************************************************************/


#ifndef MULE

Lisp_Object
decode_buffer_or_precedence_list (Lisp_Object preclist)
{
  if (NILP (preclist))
    return wrap_buffer (current_buffer);
  else if (BUFFERP (preclist))
    return preclist;
  else
    {
      EXTERNAL_LIST_LOOP_2 (elt, preclist)
	{
	  if (!EQ (elt, Vcharset_ascii))
	    invalid_argument ("In non-Mule, charset must be `ascii'", elt);
	}
      return wrap_buffer (current_buffer);
    }
}


#else /* MULE */

/******************** Precedence-array object *******************/

/************

  NOTE: Unicode precedence lists are used when converting Unicode
  codepoints to charset codepoints.  There may be more than one charset
  containing a character matching a given Unicode codepoint; to determine
  which charset to use, we use a precedence list.  Externally, precedence
  lists are just lists, but internally we use an object that encapsulates
  a Lisp_Object_dynarr.

************/

static const struct memory_description precedence_array_description [] = {
  { XD_BLOCK_PTR, offsetof (struct precedence_array, precdyn),
    1, { &Lisp_Object_dynarr_description} },
  { XD_END }
};

static Lisp_Object
mark_precedence_array (Lisp_Object obj)
{
  struct precedence_array *data =
    (struct precedence_array *) XPRECEDENCE_ARRAY (obj);
  mark_Lisp_Object_dynarr (data->precdyn);

  return Qnil;
}

static void
finalize_precedence_array (Lisp_Object obj)
{
  struct precedence_array *data = XPRECEDENCE_ARRAY (obj);

  if (data->precdyn)
    {
      Dynarr_free (data->precdyn);
      data->precdyn = 0;
    }
}

static void
print_precedence_array (Lisp_Object obj, Lisp_Object printcharfun,
			int UNUSED (escapeflag))
{
  struct precedence_array *data =
    (struct precedence_array *) XPRECEDENCE_ARRAY (obj);
  int i;

  if (print_readably)
    printing_unreadable_object_fmt ("precedence array");

  write_ascstring (printcharfun,
		   "#<INTERNAL OBJECT (XEmacs bug?) (precedence-array)");
  write_fmt_string (printcharfun, " length=%d", Dynarr_length (data->precdyn));
  for (i = 0; i < Dynarr_length (data->precdyn); i++)
    {
      Lisp_Object charset = Dynarr_at (data->precdyn, i);
      write_fmt_string_lisp (printcharfun, " #%d: %s", 2, make_int (i + 1),
			     XCHARSET_NAME (charset));
    }
  write_fmt_string (printcharfun, " 0x%lx>", (unsigned long) XPNTR (obj));
}


/* NOTE: This is non-dumpable currently, even though it would be more
   convenient if it were dumpable, due to annoying limitations in New GC.
   Basically, dumpable objects cannot have finalizers -- any objects that
   would be freed by finalizers, such as Dynarrs, have to be converted to
   Lisp objects.  There's a special mechanism in New-GC for making
   Lisp-object Dynarrs, and I tried to use it, but (a) it's painful, and
   (b) it requires that the object contained in the Dynarr be a particular
   Lisp object -- not a pointer to Lisp object.  So I tried creating a
   nasty internal "lisp-object-wrapper" object but I realized it would just
   get too gooey.  That extra object would have an lrecord header and mark
   method and you'd have to dereference it to fetch the Lisp object inside
   and it would make the code even messier.  It seemed less of a hassle
   just to ensure that precedence-array objects don't get dumped, by
   clearing out any places where they otherwise would occur and recreating
   them when starting up again. */

DEFINE_NODUMP_LISP_OBJECT ("precedence-array", precedence_array,
			   mark_precedence_array, print_precedence_array,
			   finalize_precedence_array, 0, 0, 
			   precedence_array_description,
			   struct precedence_array);

/******************** Basic precedence-array functions *******************/

Lisp_Object
allocate_precedence_array (void)
{
  Lisp_Object precedence_array = ALLOC_NORMAL_LISP_OBJECT (precedence_array);
  struct precedence_array *data = XPRECEDENCE_ARRAY (precedence_array);
  data->precdyn = Dynarr_new (Lisp_Object);
  return precedence_array;
}

void
reset_precedence_array (Lisp_Object precarray)
{
  Lisp_Object_dynarr *precdyn = XPRECEDENCE_ARRAY_DYNARR (precarray);
  Dynarr_reset (precdyn);
}

void
begin_precedence_array_generation (void)
{
  Fclrhash (Vprecedence_array_charsets_seen_hash);
}

/* Add a single charset to a precedence list.  Charsets already present
   are not added.  To keep track of charsets already seen, this makes use
   of a hash table.  At the beginning of generating the list, you must
   call begin_precedence_array_generation(). */

void
add_charset_to_precedence_array (Lisp_Object charset, Lisp_Object precarray)
{
  struct precedence_array *precarr = XPRECEDENCE_ARRAY (precarray);
  Lisp_Object_dynarr *precdyn = XPRECEDENCE_ARRAY_DYNARR (precarray);
  if (NILP (Fgethash (charset, Vprecedence_array_charsets_seen_hash, Qnil)))
    {
      Dynarr_add (precdyn, charset);
      Fputhash (charset, Qt, Vprecedence_array_charsets_seen_hash);
      /* Update the flags used to determine whether we can short-circuit
	 ASCII conversion */
      if (EQ (charset, Vcharset_ascii))
	precarr->seen_ascii = 1;
      else if (XCHARSET (charset)->number_of_ascii_mappings > 0 &&
	       !precarr->seen_ascii)
	precarr->has_overriding_ascii = 1;
    }
}

/* Add a list of charsets to a precedence list.  LIST must be a list of
   charsets or charset names.  Charsets already present are not added.  To
   keep track of charsets already seen, this makes use of a hash table.  At
   the beginning of generating the list, you must call
   begin_precedence_array_generation(). */

void
add_charsets_to_precedence_array (Lisp_Object list, Lisp_Object precarray)
{
  {
    EXTERNAL_LIST_LOOP_2 (elt, list)
      {
	Lisp_Object charset = Fget_charset (elt);
	add_charset_to_precedence_array (charset, precarray);
      }
  }
}

/* Go through ORIG_PRECARRAY and add all charsets to NEW_PRECARRAY that pass
   the predicate, if not already added.  To keep track of charsets already
   seen, this makes use of a hash table.  At the beginning of generating
   the list, you must call begin_precedence_array_generation().  PREDICATE
   is passed a charset and should return non-zero if the charset is to be
   added.  If PREDICATE is NULL, always add the charset. */
static void
filter_precedence_array (Lisp_Object orig_precarray,
			Lisp_Object new_precarray,
			int (*predicate) (Lisp_Object))
{
  int i;
  Lisp_Object_dynarr *orig_precdyn = XPRECEDENCE_ARRAY_DYNARR (orig_precarray);
  for (i = 0; i < Dynarr_length (orig_precdyn); i++)
    {
      Lisp_Object charset = Dynarr_at (orig_precdyn, i);
      if (!predicate || (*predicate) (charset))
	add_charset_to_precedence_array (charset, new_precarray);
    }
}

void
free_precedence_array (Lisp_Object precarray)
{
#ifdef ERROR_CHECK_TEXT
  /* We shouldn't be trying to free any precarray that's attached to a
     buffer */
  {
    ALIST_LOOP_3 (name, buf, Vbuffer_alist)
      assert (!EQ (precarray, XBUFFER (buf)->unicode_precedence_array));
  }
  assert (!EQ (precarray, Vdefault_unicode_precedence_array));
#endif /* ERROR_CHECK_TEXT */
  if (!gc_in_progress)
    /* Will abort if you try to free a Lisp object during GC */
    free_normal_lisp_object (precarray);
}

/******************** External precedence-list functions *******************/

/* Called for each pair of (symbol, charset) in the hash table tracking
   charsets. */
static int
cplta_mapper (Lisp_Object UNUSED (key), Lisp_Object value, void *closure)
{
  add_charset_to_precedence_array (value, GET_LISP_FROM_VOID (closure));
  return 0;
}

#define CPLTA_NORMALIZE_P (1 << 0)
#define CPLTA_MAKE_FULL_P (1 << 1)
#define CPLTA_EARLY_ERROR_HANDLING (1 << 2)

/* Process an external precedence list and write the charsets into the
   given precedence array object.  Same as convert_precedence_list_to_array()
   except that the precedence array must be passed in, and you must call
   begin_precedence_array_generation() yourself. */

static void
convert_precedence_list_to_array_1 (Lisp_Object precarray,
				    Lisp_Object preclist, int flags)
{
  EXTERNAL_LIST_LOOP_2 (elt, preclist)
    {
      if (flags & CPLTA_NORMALIZE_P)
	{
	  Lisp_Object charset = Qnil;
	  /* Optimization to avoid calling Lisp in case a charset is listed
	     directly */
	  if (CHARSETP (elt))
	    charset = elt;
	  else if (SYMBOLP (elt))
	    charset = Ffind_charset (elt);
	  if (!NILP (charset))
	    add_charset_to_precedence_array (charset, precarray);
	  else
	    {
	      if (flags & CPLTA_EARLY_ERROR_HANDLING)
		elt = call_critical_lisp_code
		  (NULL, Qcharset_tag_to_charset_list, elt);
	      else
		elt = call1 (Qcharset_tag_to_charset_list, elt);
	      add_charsets_to_precedence_array (elt, precarray);
	    }
	}
      else
	add_charset_to_precedence_array (elt, precarray);
    }
  if (flags & CPLTA_MAKE_FULL_P)
    /* Now add all remaining charsets */
    elisp_maphash (cplta_mapper, Vcharset_hash_table,
                   STORE_LISP_IN_VOID (precarray));
}

/* Convert an external precedence list to a precedence array object.  If
   CPLTA_NORMALIZE_P, elements can be charsets or tags.  Otherwise, elements
   can only be charsets.  If CPLTA_MAKE_FULL_P, also add all remaining
   unmentioned charsets to the array. If CPLTA_EARLY_ERROR_HANDLING, wrap
   calls to Lisp in call_critical_lisp_code(), which inhibits GC and will
   bomb out if an error occurs.

   WARNING: This can call Lisp when CPLTA_NORMALIZE_P.   */

static Lisp_Object
convert_precedence_list_to_array (Lisp_Object preclist, int flags)
{
  Lisp_Object precarray;
  struct gcpro gcpro1;

  precarray = allocate_precedence_array ();
  GCPRO1 (precarray);
  begin_precedence_array_generation ();
  convert_precedence_list_to_array_1 (precarray, preclist, flags);
  UNGCPRO;
  return precarray;
}

/* Validate and convert the given list of charsets into a precedence array
   object for use with unicode_to_charset_codepoint().  Don't do any
   caching, don't normalize or call Lisp, don't make full, etc. */

Lisp_Object
simple_convert_predence_list_to_array (Lisp_Object preclist)
{
  return convert_precedence_list_to_array (preclist, 0);
}

/* Validate and convert an external precedence list consisting of charsets
   and/or charset tags into a precedence array object for use with
   unicode_to_charset_codepoint().  Caches the results for faster lookup.

   WARNING: This calls Lisp. */

static Lisp_Object
external_convert_precedence_list_to_array (Lisp_Object preclist)
{
  Lisp_Object precarray = Fgethash (preclist, Vprecedence_list_to_array, Qnil);
  if (NILP (precarray))
    {
      precarray = convert_precedence_list_to_array (preclist,
						    CPLTA_NORMALIZE_P);
      Fputhash (preclist, precarray, Vprecedence_list_to_array);
    }
  return precarray;
}


Lisp_Object
decode_buffer_or_precedence_list (Lisp_Object preclist)
{
  if (NILP (preclist))
    return wrap_buffer (current_buffer);
  else if (BUFFERP (preclist))
    return preclist;
  else
    return external_convert_precedence_list_to_array (preclist);
}

/**************** The global and buffer-local precedence lists ***************/

#define RUP_NUKE_BUFFER_SLOTS (1 << 0)
#define RUP_EARLY_ERROR_HANDLING (1 << 1)
#define RUP_CLEAR_HASH_TABLE (1 << 2)
#define RUP_MAKE_FULL_P (1 << 3)

/* Rebuild one buffer-local Unicode precedence array slot.  Return a
   precedence array.  Compute the precedence array from PRECLIST1.  If
   PRECLIST2 is non-nil, also add the charsets from it, after the ones
   from PRECLIST1.  FLAGS is as in recalculate_unicode_precedence().
*/

static Lisp_Object
recalculate_unicode_precedence_1 (Lisp_Object preclist, int flags)
{
  if (flags & RUP_NUKE_BUFFER_SLOTS)
    return Qnil;
  else
    {
      Lisp_Object value = Fgethash (preclist, Vprecedence_list_to_array, Qnil);
      if (!NILP (value))
	return value;
      else
	{
	  Lisp_Object precarr;
	  int cplta_flags = CPLTA_NORMALIZE_P |
	    (flags & RUP_MAKE_FULL_P ? CPLTA_MAKE_FULL_P : 0) |
	    (flags & RUP_EARLY_ERROR_HANDLING ? CPLTA_EARLY_ERROR_HANDLING : 0)
	    ;

	  /* No GCPRO because we don't have pointers to any created objects
	     till after the following function runs, and it's the only one
	     that calls Lisp */
	  precarr = convert_precedence_list_to_array (preclist, cplta_flags);
	  Fputhash (preclist, precarr, Vprecedence_list_to_array);
	  return precarr;
	}
    }
}

/* Rebuild the default and buffer-local Unicode precedence arrays.  If
   RUP_CLEAR_HASH_TABLE, clear the hash table first; otherwise, we get use
   the hash table to avoid expensive calls to Lisp.  If
   RUP_NUKE_BUFFER_SLOTS, set the array values to nil instead of
   recalculating them.  If RUP_EARLY_ERROR_HANDLING, wrap calls to Lisp in
   call_critical_lisp_code(), which inhibits GC and will bomb out if an
   error occurs. */

static void
recalculate_unicode_precedence (int flags)
{
  if (flags & RUP_CLEAR_HASH_TABLE)
    {
      Fclrhash (Vprecedence_list_to_array);
      Fclrhash (Vprecedence_array_cons_to_array);
    }
  Vdefault_unicode_precedence_array =
    recalculate_unicode_precedence_1 (Vdefault_unicode_precedence_list,
				      flags | RUP_MAKE_FULL_P);
  {
    ALIST_LOOP_3 (name, buffer, Vbuffer_alist)
      {
	struct buffer *buf = XBUFFER (buffer);
	buf->unicode_precedence_array =
	  recalculate_unicode_precedence_1 (buf->unicode_precedence_list,
					    flags);
      }
  }
}

void
charset_created_recalculate_unicode_precedence (void)
{
  recalculate_unicode_precedence (RUP_CLEAR_HASH_TABLE);
}

void
disksave_clear_unicode_precedence (void)
{
  recalculate_unicode_precedence (RUP_NUKE_BUFFER_SLOTS |
				  RUP_CLEAR_HASH_TABLE);
}

int
unicode_precedence_list_changed (Lisp_Object UNUSED (sym),
				 Lisp_Object *UNUSED (val),
				 Lisp_Object UNUSED (in_object),
				 int UNUSED (flags))
{
  recalculate_unicode_precedence (0);
  return 0;
}

DEFUN ("set-default-unicode-precedence-list",
       Fset_default_unicode_precedence_list,
       1, 1, 0, /*
Set the default precedence list used for Unicode decoding.

This is a list of charsets or charset tags, used to convert Unicode
codepoints to charset codepoints.  These are searched in order for a
translation matching a given Unicode character.  Charset tags are tags that
can match multiple charsets and generally correspond to classes of
charsets. (See `define-charset-tag'.)

See `make-char', `unicode-to-char' and `make-charset' for more information
about characters, charsets, charset codepoints, Unicode codepoints, and
Unicode precedence lists.

The actual charset ordering used for converting Unicode codepoints to
charset codepoints is determined by concatenating the buffer-specific
Unicode precedence list (see `set-buffer-unicode-precedence-list'), the
default precedence list, and the list of all charsets, converting tags to
their corresponding charsets using `charset-tag-to-charset-list', and
removing any duplicates.
*/
       (list))
{
  /* Convert and validate first before changing
     Vdefault_unicode_precedence_list */
  Vdefault_unicode_precedence_array =
    external_convert_precedence_list_to_array (list);
  Vdefault_unicode_precedence_list = list;
  return Qnil;
}

DEFUN ("default-unicode-precedence-list",
       Fdefault_unicode_precedence_list,
       0, 0, 0, /*
Return the default precedence list used for Unicode decoding.
See `set-default-unicode-precedence-list' for more information.
*/
       ())
{
  return Vdefault_unicode_precedence_list;
}

DEFUN ("set-buffer-unicode-precedence-list",
       Fset_buffer_unicode_precedence_list,
       2, 2, 0, /*
Set the BUFFER's local precedence list used for Unicode decoding.

This is a list of charsets or charset tags, used to convert Unicode
codepoints to charset codepoints.  These are searched in order for a
translation matching a given Unicode character.  Charset tags are tags that
can match multiple charsets and generally correspond to classes of
charsets. (See `define-charset-tag'.)

See `make-char', `unicode-to-char' and `make-charset' for more information
about characters, charsets, charset codepoints, Unicode codepoints, and
Unicode precedence lists.

The actual charset ordering used for converting Unicode codepoints to
charset codepoints is determined by concatenating the buffer-specific
Unicode precedence list, the default precedence list (see
`set-default-unicode-precedence-list'), and the list of all charsets,
converting tags to their corresponding charsets using
`charset-tag-to-charset-list', and removing any duplicates.
*/
       (buffer, list))
{
  /* Convert and validate first before changing buffer's
     unicode_precedence_list */
  struct buffer *buf = decode_buffer (buffer, 0);
  buf->unicode_precedence_array =
    external_convert_precedence_list_to_array (list);
  buf->unicode_precedence_list = list;
  return Qnil;
}

DEFUN ("buffer-unicode-precedence-list",
       Fbuffer_unicode_precedence_list,
       0, 1, 0, /*
Return the BUFFER's local precedence list used for Unicode decoding.
BUFFER defaults to the current buffer if nil or omitted.
See `set-buffer-unicode-precedence-list' for more information.
*/
       (buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  return buf->unicode_precedence_list;
}

static Lisp_Object
precedence_array_to_list (Lisp_Object precarray)
{
  Lisp_Object list = Qnil;
  Lisp_Object_dynarr *precdyn = XPRECEDENCE_ARRAY_DYNARR (precarray);
  int i;

  for (i = Dynarr_length (precdyn) - 1; i >= 0; i--)
    list = Fcons (XCHARSET_NAME (Dynarr_at (precdyn, i)), list);
  return list;
}

DEFUN ("normalized-unicode-precedence-list",
       Fnormalized_unicode_precedence_list,
       0, 1, 0, /*
Return the precedence order among charsets used for Unicode decoding.

Return value is the actual list of charsets that would be consulted if
BUFFER-OR-PRECEDENCE-LIST were specified as the corresponding argument
to any function that accepts a buffer or precedence list and uses it
to convert Unicode codepoints to charset codepoints.  This value differs
from the value returned by `buffer-unicode-precedence-list' or
`default-unicode-precedence-list' in that charset tags are expanded and,
in the case that `nil' or a buffer is given, any unspecified charsets
are added at the end in an indeterminate order.

See `unicode-to-char' for more information on precedence lists, and
`unicode-to-charset-codepoint' as an example of a function that takes a
BUFFER-OR-PRECEDENCE-LIST argument.

A Value of nil for BUFFER-OR-PRECEDENCE-LIST is the same as specifying
the current buffer.
*/
       (buffer_or_precedence_list))
{
  Lisp_Object bopa =
    decode_buffer_or_precedence_list (buffer_or_precedence_list);
  Lisp_Object precarray;

  if (BUFFERP (bopa))
    {
      precarray = allocate_precedence_array ();
      begin_precedence_array_generation ();
      filter_precedence_array (XBUFFER (bopa)->unicode_precedence_array,
			       precarray, NULL);
      filter_precedence_array (Vdefault_unicode_precedence_array,
			       precarray, NULL);
    }
  else
    precarray = bopa;

  return precedence_array_to_list (precarray);
}

/***************************************************************************/
/*                set up Unicode <-> charset codepoint conversion          */
/***************************************************************************/

DEFUN ("set-unicode-conversion", Fset_unicode_conversion,
       3, 4, 0, /*
Add conversion information between Unicode and charset codepoints.
A single Unicode codepoint may have multiple corresponding charset
codepoints, but each codepoint in a charset corresponds to only one
Unicode codepoint.  Further calls to this function with the same
values for (CHARSET, C1 [, C2]) and a different value for UNICODE
will overwrite the previous value.

If UNICODE is nil, remove any Unicode conversion for the given charset
codepoint.

Note that the Unicode codepoints corresponding to the ASCII, Control-1,
and Latin-1 charsets are hard-wired.  Attempts to set these values
will raise an error.

C2 either must or may not be specified, depending on the dimension of
CHARSET (see `make-char').
*/
       (unicode, charset, c1, c2))
{
  int a1, a2;
  int ucp;

  /* Private codepoints should not get put into conversion tables. */
  if (!NILP (unicode))
    ucp = decode_unicode (unicode, UNICODE_OFFICIAL_ONLY);
  else
    ucp = -1;
  charset = get_external_charset_codepoint (charset, c1, c2, &a1, &a2, 0);
  
  /* It would not be a good idea to change these.  We definitely have
     hardcoded assumptions about ASCII mapping to Unicode 0 - 127 in
     various places.  It's not clear whether we have such assumptions
     about Control-1 or Latin-1 but it's clearly not a good idea to
     change them. */
  if ((EQ (charset, Vcharset_ascii) ||
       EQ (charset, Vcharset_latin_iso8859_1) ||
       EQ (charset, Vcharset_control_1)))
    {
      if (ucp != a2)
	invalid_argument
	  ("Can't change Unicode translation for ASCII, Control-1 or Latin-1 character",
           unicode);
      return Qnil;
    }
  
  /* #### Composite characters are not properly implemented yet. */
  if (EQ (charset, Vcharset_composite))
    invalid_argument ("Can't set Unicode translation for Composite char",
		      unicode);

#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
  if (!XCHARSET_FROM_UNICODE_TABLE (charset))
    {
      text_checking_assert (XCHARSET_ALGO_LOW (charset) >= 0);
      invalid_argument
	("Can't set Unicode translation of charset with automatic translation",
	 charset);
    }
#endif

  set_unicode_conversion (ucp, charset, a1, a2);
  return Qnil;
}

/* "cerrar el fulano" = close the so-and-so */
static Lisp_Object
cerrar_el_fulano (Lisp_Object fulano)
{
  FILE *file = (FILE *) get_opaque_ptr (fulano);
  retry_fclose (file);
  return Qnil;
}

DEFUN ("load-unicode-mapping-table", Fload_unicode_mapping_table,
       2, 6, 0, /*
Load Unicode tables with the Unicode mapping data in FILENAME for CHARSET.
Data is text, in the form of one translation per line.  Lines are of the
form

CHARSETCODE UNICODECODE

for a single codepoint translation, or

CHARSETCODE1-CHARSETCODE2 UNICODE

for a range of charset codepoints mapping to a range of Unicode codepoints
beginning at UNICODE.  Numbers are decimal or hex (preceded by 0x).
Comments are marked with a #.  Charset codepoints for two-dimensional
charsets have the first octet stored in the high 8 bits of the hex number
and the second in the low 8 bits.

If START and END are given, only charset codepoints within the given
range will be processed.  (START and END apply to the codepoints in the
file, before OFFSET is applied.)

If OFFSET is given, that value will be added to all charset codepoints in
the file to obtain the internal charset codepoint.  (For example, normal
size-94 charsets have octets in the range 33 to 126.  If you have a table
in ku-ten form, with octets in the range 1 to 94, you will have to use an
offset of #x2020.)

FLAGS, if specified, control further how the tables are interpreted
and are used to special-case certain known format deviations in the
Unicode tables or in the charset:

`ignore-first-column'
  The JIS X 0208 tables have 3 columns of data instead of 2.  The first
  column contains the Shift-JIS codepoint, which we ignore.
`big5'
  The charset codepoints are Big Five codepoints; convert it to the
  hacked-up Mule codepoint in `chinese-big5-1' or `chinese-big5-2'.
  Not when (featurep 'unicode-internal).
*/
     (filename, charset, start, end, offset, flags))
{
  int st = 0, en = INT_MAX, of = 0;
  FILE *file;
  struct gcpro gcpro1;
  char line[1025];
  int fondo = specpdl_depth (); /* "fondo" = depth */
  int ignore_first_column = 0;
#ifndef UNICODE_INTERNAL
  int big5 = 0;
#endif /* not UNICODE_INTERNAL */

  CHECK_STRING (filename);
  charset = Fget_charset (charset);
  if (!NILP (start))
    {
      CHECK_INT (start);
      st = XINT (start);
    }
  if (!NILP (end))
    {
      CHECK_INT (end);
      en = XINT (end);
    }
  if (!NILP (offset))
    {
      CHECK_INT (offset);
      of = XINT (offset);
    }

  if (!LISTP (flags))
    flags = list1 (flags);

  {
    EXTERNAL_LIST_LOOP_2 (elt, flags)
      {
	if (EQ (elt, Qignore_first_column))
	  ignore_first_column = 1;
#ifndef UNICODE_INTERNAL
	else if (EQ (elt, Qbig5))
	  {
	    big5 = 1;
	    /* At this point the charsets haven't been initialzied
	       yet, so at least set the values for big5-1 and big5-2
	       so we can use big5_char_to_fake_codepoint(). */
	    Vcharset_chinese_big5_1 = Fget_charset (Qchinese_big5_1);
	    Vcharset_chinese_big5_2 = Fget_charset (Qchinese_big5_2);
	  }
#endif /* not UNICODE_INTERNAL */
	else
	  invalid_constant
	    ("Unrecognized `load-unicode-mapping-table' flag", elt);
      }
  }

  GCPRO1 (filename);
  filename = Fexpand_file_name (filename, Qnil);
  file = qxe_fopen (XSTRING_DATA (filename), READ_TEXT);
  if (!file)
    report_file_error ("Cannot open", filename);
  record_unwind_protect (cerrar_el_fulano, make_opaque_ptr (file));
  while (fgets (line, sizeof (line), file))
    {
      char *p = line;
      int cp1from, cp1to, cp1, cp2, endcount;
      int cp1high, cp1low;
      int dummy;
      int scanf_count, garbage_after_scanf;

      /* #### Perhaps we should rewrite this using regular expressions */
      while (*p) /* erase all comments out of the line */
	{
	  if (*p == '#')
	    *p = '\0';
	  else
	    p++;
	}
      /* see if line is nothing but whitespace and skip if so;
         count ^Z among this because it appears at the end of some
         Microsoft translation tables. */
      p = line + strspn (line, " \t\n\r\f\032");
      if (!*p)
	continue;
      /* NOTE: It appears that MS Windows and Newlib sscanf() have
	 different interpretations for whitespace (== "skip all whitespace
	 at processing point"): Newlib requires at least one corresponding
	 whitespace character in the input, but MS allows none.  The
	 following would be easier to write if we could count on the MS
	 interpretation.

	 Also, the return value does NOT include %n storage. */

      /* First check for a range. */
      scanf_count =
	(!ignore_first_column ?
	 sscanf (p, "%i-%i %i%n", &cp1from, &cp1to, &cp2, &endcount) :
	 sscanf (p, "%i-%i %i %i%n", &dummy, &cp1from, &cp1to, &cp2,
		 &endcount) - 1);
      /* If we didn't find one, try a single codepoint translation. */
      if (scanf_count < 3)
	{
	  scanf_count =
	    (!ignore_first_column ?
	     sscanf (p, "%i %i%n", &cp1from, &cp2, &endcount) :
	     sscanf (p, "%i %i %i%n", &dummy, &cp1from, &cp2, &endcount) - 1);
	  cp1to = cp1from;
	}
      else
	scanf_count--;
      /* #### Temporary code!  Cygwin newlib fucked up scanf() handling
	 of numbers beginning 0x0... starting in 04/2004, in an attempt
	 to fix another bug.  A partial fix for this was put in in
	 06/2004, but as of 10/2004 the value of ENDCOUNT returned in
	 such case is still wrong.  If this gets fixed soon, remove
	 this code. --ben */
      if (endcount > (int) strlen (p))
	/* We know we have a broken sscanf in this case!!! */
	garbage_after_scanf = 0;
      else
	{
#ifndef CYGWIN_SCANF_BUG
	  garbage_after_scanf =
	    *(p + endcount + strspn (p + endcount, " \t\n\r\f\032"));
#else
	  garbage_after_scanf = 0;
#endif
	}

      /* #### Hack.  A number of the CP###.TXT files from Microsoft contain
	 lines with a charset codepoint and no corresponding Unicode
	 codepoint, representing undefined values in the code page.

	 Skip them so we don't get a raft of warnings. */
      if (scanf_count == 1 && !garbage_after_scanf)
	continue;
      if (scanf_count < 2 || garbage_after_scanf)
	{
	  warn_when_safe (Qunicode, Qwarning,
			  "Unrecognized line in translation file %s:\n%s",
			  XSTRING_DATA (filename), line);
	  continue;
	}
      for (cp1 = cp1from; cp1 <= cp1to; cp1++, cp2++)
	{
	  if (cp1 >= st && cp1 <= en)
	    {
	      cp1 += of;
	      if (cp1 < 0 || cp1 >= 65536)
		{
		out_of_range:
		  warn_when_safe (Qunicode, Qwarning,
				  "Out of range first codepoint 0x%x in "
				  "translation file %s:\n%s",
				  cp1, XSTRING_DATA (filename), line);
		  continue;
		}

	      cp1high = cp1 >> 8;
	      cp1low = cp1 & 255;

#ifndef UNICODE_INTERNAL
	      if (big5)
		{
		  Lisp_Object fake_charset;
		  int c1, c2;
		  big5_char_to_fake_codepoint (cp1high, cp1low, &fake_charset,
					       &c1, &c2);
		  if (NILP (fake_charset))
		    warn_when_safe (Qunicode, Qwarning,
				    "Out of range Big5 codepoint 0x%x in "
				    "translation file %s:\n%s",
				    cp1, XSTRING_DATA (filename), line);
		  else
		    set_unicode_conversion (cp2, fake_charset, c1, c2);
		}
	      else
#endif /* not UNICODE_INTERNAL */
		{
		  int l1, l2, h1, h2;
		  int c1 = cp1high, c2 = cp1low;

		  get_charset_limits (charset, &l1, &l2, &h1, &h2);
		  
		  if (c1 < l1 || c1 > h1 || c2 < l2 || c2 > h2)
		    goto out_of_range;
		  
		  set_unicode_conversion (cp2, charset, c1, c2);
		}
	    }
	}
    }

  if (ferror (file))
    report_file_error ("IO error when reading", filename);

  unbind_to (fondo); /* close file */
  UNGCPRO;
  return Qnil;
}

void
init_charset_unicode_map (Lisp_Object charset, Lisp_Object map)
{
  Lisp_Object savemap = map;

  CHECK_TRUE_LIST (map);
  if (STRINGP (XCAR (map)))
    {
      Lisp_Object filename = XCAR (map);
      Lisp_Object start = Qnil, end = Qnil, offset = Qnil, flags = Qnil;
      map = XCDR (map);
      if (!NILP (map))
	{
	  start = XCAR (map);
	  map = XCDR (map);
	}
      if (!NILP (map))
	{
	  end = XCAR (map);
	  map = XCDR (map);
	}
      if (!NILP (map))
	{
	  offset = XCAR (map);
	  map = XCDR (map);
	}
      if (!NILP (map))
	{
	  flags = XCAR (map);
	  map = XCDR (map);
	}
      if (!NILP (map))
	invalid_argument ("Unicode map can have at most 5 arguments",
			  savemap);
      Fload_unicode_mapping_table (filename, charset, start, end,
				   offset, flags);
    }
  else
    {
      EXTERNAL_LIST_LOOP_2 (entry, map)
	{
	  int len;
	  CHECK_TRUE_LIST (entry);
	  len = XINT (Flength (entry));
	  if (XCHARSET_DIMENSION (charset) == 1)
	    {
	      if (len != 2)
		invalid_argument ("Unicode map entry must have length 2 for dimension-1 charset", entry);
	      Fset_unicode_conversion (XCADR (entry), charset, XCAR (entry),
				       Qnil);
	    }
	  else
	    {
	      if (len != 3)
		invalid_argument ("Unicode map entry must have length 3 for dimension-1 charset", entry);
	      Fset_unicode_conversion (XCADDR (entry), charset, XCAR (entry),
				       XCADR (entry));
	    }
	}
    }

  /* Only set map after we have gone through everything and gotten
     no errors */
  XCHARSET_UNICODE_MAP (charset) = savemap;
}

#endif /* MULE */


/************************************************************************/
/*                      Properties of Unicode chars                     */
/************************************************************************/

#ifdef MULE

int
unicode_char_columns (int code)
{
#if defined (HAVE_WCWIDTH) && defined (__STDC_ISO_10646__)
  return wcwidth ((wchar_t) code);
#else
  /* #### We need to do a much better job here.  Although maybe wcwidth()
     is available everywhere we care.  @@#### Copy the source for wcwidth().
     Also check under Windows for an equivalent. */
  /* #### Use a range table for this! */
  if (
      /* Tibetan */
      (code >= 0x0F00 && code <= 0x0FFF) ||
      /* Ethiopic, Ethiopic Supplement */
      (code >= 0x1200 && code <= 0x139F) ||
      /* Unified Canadian Aboriginal Syllabic */
      (code >= 0x1400 && code <= 0x167F) ||
      /* Ethiopic Extended */
      (code >= 0x2D80 && code <= 0x2DDF) ||
      /* Do not combine the previous range with this one, as
	 0x2E00 .. 0x2E7F is Supplemental Punctuation (Ancient Greek, etc.) */
      /* CJK Radicals Supplement ... Hangul Syllables */
      (code >= 0x2E80 && code <= 0xD7AF) ||
      /* CJK Compatibility Ideographs */
      (code >= 0xF900 && code <= 0xFAFF) ||
      /* CJK Compatibility Forms */
      (code >= 0xFE30 && code <= 0xFE4F) ||
      /* CJK Unified Ideographs Extension B, CJK Compatibility Ideographs
	 Supplement, any other crap in this region */
      (code >= 0x20000 && code <= 0x2FFFF))
    return 2;
  return 1;
#endif /* defined (HAVE_WCWIDTH) && defined (__STDC_ISO_10646__) */
}

#endif /* MULE */


/************************************************************************/
/*                         Unicode coding system                        */
/************************************************************************/

struct unicode_coding_system
{
  enum unicode_encoding_type type;
  unsigned int little_endian :1;
  unsigned int need_bom :1;
};

#define CODING_SYSTEM_UNICODE_TYPE(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, unicode)->type)
#define XCODING_SYSTEM_UNICODE_TYPE(codesys) \
  CODING_SYSTEM_UNICODE_TYPE (XCODING_SYSTEM (codesys))
#define CODING_SYSTEM_UNICODE_LITTLE_ENDIAN(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, unicode)->little_endian)
#define XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN(codesys) \
  CODING_SYSTEM_UNICODE_LITTLE_ENDIAN (XCODING_SYSTEM (codesys))
#define CODING_SYSTEM_UNICODE_NEED_BOM(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, unicode)->need_bom)
#define XCODING_SYSTEM_UNICODE_NEED_BOM(codesys) \
  CODING_SYSTEM_UNICODE_NEED_BOM (XCODING_SYSTEM (codesys))

static const struct memory_description unicode_coding_system_description[] = {
  { XD_END }
};

static const struct memory_description unicode_coding_stream_description[] = {
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (unicode);

/* Convert Unicode codepoint UCS into an Ichar and add its textual
   representation to the dynarr DST. */
void
decode_unicode_to_dynarr (int ucs, unsigned_char_dynarr *dst)
{
  /* @@####
     FIXME: This following comment may not be correct, given the
     just-in-time Unicode character handling 
     [[
     #### If the lookup fails, we will currently use a replacement char
     (e.g. <GETA MARK> (U+3013) of JIS X 0208).
     #### Danger, Will Robinson!  Data loss. Should we signal user?
     ]]
  */
  /* @@#### What about errors? */
  /* @@#### current_buffer dependency */
  Ichar chr = buffer_unicode_to_ichar (ucs, current_buffer, CONVERR_SUCCEED);
  Dynarr_add_ichar (dst, chr);
}

/* Convert Unicode codepoint UCS into an Ichar and add its textual
   representation to the dynarr DST.  However, if UCS is a byte-order mark
   (0xFEFF) and no characters have been seen (according to a flag in DATA)
   and IGNORE_BOM is given, ignore it.  Also note in DATA that a character
   has been seen.  This function is meant to be called from within the
   Unicode conversion routines. */

/* Decode a UCS-2 or UCS-4 character (or -1 for error) into a buffer. */
inline static void
decode_unicode_to_dynarr_0 (int ucs, unsigned_char_dynarr *dst,
			    struct unicode_coding_stream *data,
			    int ignore_bom)
{
  ASSERT_VALID_UNICODE_CODEPOINT (ucs);
  if (ucs == 0xFEFF && ignore_bom && !data->seen_char)
    ;
  else
    {
      decode_unicode_to_dynarr (ucs, dst);
    }

  data->seen_char = 1;
}


void
decode_utf_8 (struct unicode_coding_stream *data, unsigned_char_dynarr *dst,
	      UExtbyte c, int ignore_bom, int allow_private)
{
  if (0 == data->counter)
    {
      if (0 == (c & 0x80))
	{
	  /* ASCII. */
	  decode_unicode_to_dynarr_0 (c, dst, data, ignore_bom);
	}
      else if (0 == (c & 0x40))
	{
	  /* Highest bit set, second highest not--there's
	     something wrong. */
	  UNICODE_DECODE_ERROR_OCTET (c, dst, data, ignore_bom);
	}
      else if (0 == (c & 0x20))
	{
	  data->ch = c & 0x1f; 
	  data->counter = 1;
	  data->indicated_length = 2;
	}
      else if (0 == (c & 0x10))
	{
	  data->ch = c & 0x0f;
	  data->counter = 2;
	  data->indicated_length = 3;
	}
      else if (0 == (c & 0x08))
	{
	  data->ch = c & 0x07;
	  data->counter = 3;
	  data->indicated_length = 4;
	}
      else if (allow_private && 0 == (c & 0x04))
	{
	  data->ch = c & 0x03;
	  data->counter = 4;
	  data->indicated_length = 5;
	}
      else if (allow_private && 0 == (c & 0x02))
	{
	  data->ch = c & 0x01;
	  data->counter = 5;
	  data->indicated_length = 6;
	}

      else
	{
	  /* #xFE, #xFF are not valid leading bytes in any form of UTF-8.
	     We don't supports lengths longer than 4 in external-format
	     data, unless we are in decoding escape-quoted (signalled by
	     ALLOW_PRIVATE). */
	  UNICODE_DECODE_ERROR_OCTET (c, dst, data, ignore_bom);

	}
    }
  else
    {
      /* data->counter != 0 */
      if ((0 == (c & 0x80)) || (0 != (c & 0x40)))
	{
	  indicate_invalid_utf_8 (data->indicated_length, 
				  data->counter, 
				  data->ch, dst, data, ignore_bom);
	  if (c & 0x80)
	    {
	      UNICODE_DECODE_ERROR_OCTET (c, dst, data, ignore_bom);
	    }
	  else
	    {
	      /* The character just read is ASCII. Treat it as
		 such.  */
	      decode_unicode_to_dynarr_0 (c, dst, data, ignore_bom);
	    }
	  data->ch = 0;
	  data->counter = 0;
	}
      else 
	{
	  data->ch = (data->ch << 6) | (c & 0x3f);
	  data->counter--;
	  /* Just processed the final byte. Emit the character. */
	  if (!data->counter)
	    {
	      /* Don't accept over-long sequences, surrogates,
		 or maybe codes above #x10FFFF. */
	      int invalid = 0;
	      switch (data->indicated_length)
		{
		case 0:
		case 1: ABORT ();
		case 2: invalid = data->ch < 0x80; break;
		case 3: invalid = data->ch < 0x800; break;
		case 4: invalid = data->ch < 0x10000; break;
		case 5: invalid = data->ch < 0x200000; break;
		case 6: invalid = data->ch < 0x4000000; break;
		}
	      if (invalid || valid_utf_16_surrogate (data->ch) ||
		  /* We accept values above #x10FFFF in
		     escape-quoted, though not in UTF-8. */
		  (!allow_private && data->ch > UNICODE_OFFICIAL_MAX))
		{
		  indicate_invalid_utf_8 (data->indicated_length, 
					  data->counter, 
					  data->ch, dst, data,
					  ignore_bom);
		}
	      else
		{
		  decode_unicode_to_dynarr_0 (data->ch, dst, data, ignore_bom);
		}
	      data->ch = 0;
	    }
	}
    }
}

inline static void
add_16_bit_char (int code, unsigned_char_dynarr *dst, int little_endian)
{
  if (little_endian)
    {
      Dynarr_add (dst, (unsigned char) (code & 255));
      Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
    }
  else
    {
      Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
      Dynarr_add (dst, (unsigned char) (code & 255));
    }
}

/* Encode Unicode codepoint CODE into UTF-8, UCS-2 or the like (according
   to TYPE and LITTLE_ENDIAN), and write to dynarr DST.  Also used in
   mule-coding.c for UTF-8 handling in ISO 2022-oriented encodings.  If
   PRESERVE_ERROR_CHARACTERS is non-zero, write out error octets using
   their literal representation as Unicode codepoints, rather than
   converting them to their corresponding ASCII or Latin-1 byte and writing
   that.  If an error occurs, store the appropriate values for "read_good"
   and "written_good" into STR, based on SRC (and assuming that one
   erroneous character was read in order to produce the bad Unicode
   codepoint).  CODE can be -1, indicating that the error values should be
   set in STR and the Unicode replacement character (0xFFFD) written out.
   (Note that if the replacement character is simply passed in as CODE,
   no error will be signalled.)

   Return value is -1 if an error occurred, 0 otherwise.
*/
int
encode_unicode_to_dynarr (int code, struct coding_stream *str,
			  const UExtbyte *src,
			  unsigned_char_dynarr *dst,
			  enum unicode_encoding_type type,
			  int little_endian,
			  int preserve_error_characters)
{
  int err = 0;
  if (code == -1)
    {
      handle_encoding_error_before_output (str, src, dst, 1,
					   CODING_UNENCODABLE);
      err = -1;
      code = CANT_CONVERT_CHAR_WHEN_ENCODING_UNICODE;
    }

  ASSERT_VALID_UNICODE_CODEPOINT (code);

  if (unicode_error_octet_code_p (code))
    {
      handle_encoding_error_before_output (str, src, dst, 1,
					   CODING_INVALID_SEQUENCE);
      err = -1;
      if (!preserve_error_characters)
	{
	  Dynarr_add (dst, unicode_error_octet_code_to_octet (code));
	  return err;
	}
    }
  
  switch (type)
    {
    case UNICODE_UTF_16:
      /* Handle surrogates */
      if (code < 0x10000)
	add_16_bit_char (code, dst, little_endian);
      else if (code <= UNICODE_OFFICIAL_MAX)
	{
	  int first, second;
	  
	  CODE_TO_UTF_16_SURROGATES (code, first, second);

	  add_16_bit_char (first, dst, little_endian);
	  add_16_bit_char (second, dst, little_endian);
	}
      else
	{
	  /* Not valid Unicode. Pass the replacement char (U+FFFD). */
	  handle_encoding_error_before_output (str, src, dst, 1,
					       CODING_UNENCODABLE);
	  err = -1;
	  add_16_bit_char (CANT_CONVERT_CHAR_WHEN_ENCODING_UNICODE,
			   dst, little_endian);
	}
      break;

    case UNICODE_UCS_4:
    case UNICODE_UTF_32:
      /* We generate and accept incorrect sequences here, which is okay,
	 in the interest of preservation of the user's data.  */
      if (little_endian)
	{
	  Dynarr_add (dst, (unsigned char) (code & 255));
	  Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
	  Dynarr_add (dst, (unsigned char) ((code >> 16) & 255));
	  Dynarr_add (dst, (unsigned char) (code >> 24));
	}
      else
	{
	  Dynarr_add (dst, (unsigned char) (code >> 24));
	  Dynarr_add (dst, (unsigned char) ((code >> 16) & 255));
	  Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
	  Dynarr_add (dst, (unsigned char) (code & 255));
	}
      break;

    case UNICODE_UTF_8:
      {
	/* #### This code is duplicated in non_ascii_set_itext_ichar() in
	   text.c.  There should be a better way. */
	if (code <= 0x7f)
	  Dynarr_add (dst, (unsigned char) code);
	else
	  {
	    register int bytes;
	    register unsigned char *dstp;

	    if (code <= 0x7ff) bytes = 2;
	    else if (code <= 0xffff) bytes = 3;
	    else if (code <= 0x1fffff) bytes = 4;
	    else if (code <= 0x3ffffff) bytes = 5;
	    else bytes = 6;

	    Dynarr_add_many (dst, 0, bytes);
	    dstp = Dynarr_past_lastp (dst);
	    switch (bytes)
	      {
	      case 6:*--dstp = (code | 0x80) & 0xBF; code >>= 6;
	      case 5:*--dstp = (code | 0x80) & 0xBF; code >>= 6;
	      case 4:*--dstp = (code | 0x80) & 0xBF; code >>= 6;
	      case 3:*--dstp = (code | 0x80) & 0xBF; code >>= 6;
	      case 2:*--dstp = (code | 0x80) & 0xBF; code >>= 6;
	      case 1:*--dstp = code | firstbyte_mask[bytes];
	      }
	  }
  
	break;
      }

    case UNICODE_UTF_7: ABORT ();

    default: ABORT ();
    }

  return err;
}

/* If we're in the middle of processing a UTF-8-encoded character and
   encounter an error, this function spits out the bytes seen so far as
   literal error-octet characters.  CH is the character as built up so far,
   INDICATED_LENGTH is the total length of the UTF-8 sequence as indicated
   by the first byte, COUNTER is the number ofbytes remaining to be seen in
   the sequence (so that INDICATED_LENGTH - COUNTER indicates how many
   bytes have been seen so far), DST is where to write the error-octet
   characters, and DATA and IGNORE_BOM are used in deciding whether to
   ignore byte-order marks (see decode_unicode_to_dynarr_0()). */

void
indicate_invalid_utf_8 (int indicated_length, int counter,
                        int ch, unsigned_char_dynarr *dst,
                        struct unicode_coding_stream *data,
                        int ignore_bom)
{
  int stored = indicated_length - counter; 
  int mask = firstbyte_mask[indicated_length];

  while (stored > 0)
    {
      UNICODE_DECODE_ERROR_OCTET (((ch >> (6 * (stored - 1))) & 0x3f) | mask,
				  dst, data, ignore_bom);
      mask = 0x80, stored--;
    }
}

static Bytecount
unicode_convert (struct coding_stream *str, const UExtbyte *src,
		 Bytecount n, unsigned_char_dynarr *dst)
{
  struct unicode_coding_stream *data = CODING_STREAM_TYPE_DATA (str, unicode);
  enum unicode_encoding_type type =
    XCODING_SYSTEM_UNICODE_TYPE (str->codesys);
  int little_endian =
    XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (str->codesys);
  int ignore_bom = XCODING_SYSTEM_UNICODE_NEED_BOM (str->codesys);
  Bytecount orign = n;

  if (str->direction == CODING_DECODE)
    {
      int counter = data->counter;
      int ch = data->ch;
      int indicated_length = data->indicated_length;

      switch (type)
	{
	case UNICODE_UTF_8:
	  while (n--)
	    {
	      UExtbyte c = *src++;
	      decode_utf_8 (data, dst, c, ignore_bom, 0);
	    }
	  counter = data->counter;
	  ch = data->ch;
	  indicated_length = data->indicated_length;
	  break;

	case UNICODE_UTF_16:
	  while (n--)
	    {
	      UExtbyte c = *src++;
	      if (little_endian)
		ch = (c << counter) | ch;
	      else
		ch = (ch << 8) | c;

	      counter += 8;

	      if (16 == counter)
                {
		  int tempch = ch;

                  if (valid_utf_16_first_surrogate (ch))
		    continue;
		  ch = 0;
		  counter = 0;
		  decode_unicode_to_dynarr_0 (tempch, dst, data, ignore_bom);
		}
	      else if (32 == counter)
		{
		  int tempch;

                  if (little_endian)
                    {
                      if (!valid_utf_16_last_surrogate (ch >> 16))
                        {
                          UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
						      ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst,
						      data, ignore_bom);
                        }
                      else
                        {
                          tempch = utf_16_surrogates_to_code ((ch & 0xffff),
							      (ch >> 16));
                          decode_unicode_to_dynarr_0 (tempch, dst,
						      data, ignore_bom);
			}
                    }
                  else
                    {
                      if (!valid_utf_16_last_surrogate (ch & 0xFFFF))
                        {
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst,
						      data, ignore_bom);
                        }
                      else 
                        {
                          tempch = utf_16_surrogates_to_code ((ch >> 16), 
							      (ch & 0xffff));
                          decode_unicode_to_dynarr_0 (tempch, dst,
						      data, ignore_bom);
			}
                    }

		  ch = 0;
		  counter = 0;
                }
              else
                assert (8 == counter || 24 == counter);
	    }
	  break;
	  
	case UNICODE_UCS_4:
	case UNICODE_UTF_32:
	  while (n--)
	    {
	      UExtbyte c = *src++;
	      if (little_endian)
		ch = (c << counter) | ch;
	      else
		ch = (ch << 8) | c;
	      counter += 8;
	      if (counter == 32)
		{
		  if (ch > UNICODE_OFFICIAL_MAX)
		    {
                      /* ch is not a legal Unicode character. We're fine
                         with that in UCS-4, though not in UTF-32. */
                      if (UNICODE_UCS_4 == type &&
			  (unsigned long) ch < 0x80000000L)
                        {
                          decode_unicode_to_dynarr_0 (ch, dst,
						      data, ignore_bom);
			}
                      else if (little_endian)
                        {
                          UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data, 
						      ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst,
						      data, ignore_bom);
                        }
                      else
                        {
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst,
						      data, ignore_bom);
                          UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst,
						      data, ignore_bom);
                        }
		    }
                  else
                    {
                      decode_unicode_to_dynarr_0 (ch, dst, data, ignore_bom);
                    }
		  ch = 0;
		  counter = 0;
		}
	    }
	  break;

	case UNICODE_UTF_7:
	  ABORT ();
	  break;

	default: ABORT ();
	}

      if (str->eof && counter)
        {
          switch (type)
            {
	    case UNICODE_UTF_8:
              indicate_invalid_utf_8 (indicated_length, 
				      counter, ch, dst, data, 
				      ignore_bom);
              break;

            case UNICODE_UTF_16:
            case UNICODE_UCS_4:
            case UNICODE_UTF_32:
              if (8 == counter)
                {
                  UNICODE_DECODE_ERROR_OCTET (ch, dst, data, ignore_bom);
                }
              else if (16 == counter)
                {
                  if (little_endian)
                    {
                      UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
						  ignore_bom); 
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
						  ignore_bom); 
                    }
                  else
                    {
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
						  ignore_bom); 
                      UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
						  ignore_bom); 
                    }
                }
              else if (24 == counter)
                {
                  if (little_endian)
                    {
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
						  ignore_bom);
                      UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
						  ignore_bom); 
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
						  ignore_bom); 
                    }
                  else
                    {
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
						  ignore_bom);
                      UNICODE_DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
						  ignore_bom); 
                      UNICODE_DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
						  ignore_bom); 
                    }
                }
              else assert (0);
              break;
            }
          ch = 0;
          counter = 0;
        }

      data->ch = ch;
      data->counter = counter;
      data->indicated_length = indicated_length;
    }
  else
    {
#ifdef ENABLE_COMPOSITE_CHARS
      /* flags for handling composite chars.  We do a little switcheroo
	 on the source while we're outputting the composite char. */
      Bytecount saved_n = 0;
      const Ibyte *saved_src = NULL;
      int in_composite = 0;

    back_to_square_n:
#endif /* ENABLE_COMPOSITE_CHARS */

      if (XCODING_SYSTEM_UNICODE_NEED_BOM (str->codesys) && !data->wrote_bom)
	{
	  assert (encode_unicode_to_dynarr (0xFEFF, str, src, dst, type,
					    little_endian, 0) >= 0);
	  data->wrote_bom = 1;
	}

      while (n--)
	{
	  Ibyte c = *src++;

#ifdef MULE
	  if (byte_ascii_p (c))
#endif /* MULE */
	    assert (encode_unicode_to_dynarr (c, str, src, dst, type,
					      little_endian, 0) >= 0);
#ifdef MULE
	  else
	    {
	      COPY_PARTIAL_CHAR_BYTE (c, str);
	      if (!str->pind_remaining)
		{
#ifdef UNICODE_INTERNAL
		  if (encode_unicode_to_dynarr (non_ascii_itext_ichar
						(str->partial),
						str, src, dst, type,
						little_endian, 0) < 0)
		    {
		      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
		    }
#else
		  Lisp_Object charset;
		  int c1, c2;
		  itext_to_charset_codepoint_raw (str->partial, Qnil, NULL,
						  &charset, &c1, &c2);
#ifdef ENABLE_COMPOSITE_CHARS
		  if (EQ (charset, Vcharset_composite))
		    {
		      if (in_composite)
			{
			  /* #### Bother! We don't know how to
			     handle this yet. */
			  encode_unicode_to_dynarr (-1, str, src, dst,
						    type, little_endian, 0);
			  ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
			}
		      else
			{
			  Ichar emch =
			    charset_codepoint_to_ichar
			    /* @@#### CONVERR_SUCCEED is wrong, can't handle
			       errors this way */
			    (Vcharset_composite, c1, c2, CONVERR_SUCCEED);
			  Lisp_Object lstr = composite_char_string (emch);
			  saved_n = n;
			  saved_src = src;
			  in_composite = 1;
			  src = XSTRING_DATA   (lstr);
			  n   = XSTRING_LENGTH (lstr);
			}
		    }
		  else
#endif /* ENABLE_COMPOSITE_CHARS */
		    {
		      int code =
			charset_codepoint_to_unicode
			(charset, c1, c2, CONVERR_FAIL);
		      if (encode_unicode_to_dynarr (code, str, src, dst, type,
						    little_endian, 0) < 0)
			{
			  ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
			}
		    }
#endif /* UNICODE_INTERNAL */
		}
	    }
#endif /* MULE */
	}

#ifdef ENABLE_COMPOSITE_CHARS
      if (in_composite)
	{
	  n = saved_n;
	  src = saved_src;
	  in_composite = 0;
	  goto back_to_square_n; /* Wheeeeeeeee ..... */
	}
#endif /* ENABLE_COMPOSITE_CHARS */

      /* La palabra se hizo carne! */
      /* O verbo fez-se carne! */
      /* La parole devint chair! */
      /* Das Wort ward Fleisch! */
      /* Whatever. */
    }

  return orign;
}

/* DEFINE_DETECTOR (utf_7); */
DEFINE_DETECTOR (utf_8);
DEFINE_DETECTOR_CATEGORY (utf_8, utf_8);
DEFINE_DETECTOR_CATEGORY (utf_8, utf_8_bom);
DEFINE_DETECTOR (ucs_4);
DEFINE_DETECTOR_CATEGORY (ucs_4, ucs_4);
DEFINE_DETECTOR (utf_16);
DEFINE_DETECTOR_CATEGORY (utf_16, utf_16);
DEFINE_DETECTOR_CATEGORY (utf_16, utf_16_little_endian);
DEFINE_DETECTOR_CATEGORY (utf_16, utf_16_bom);
DEFINE_DETECTOR_CATEGORY (utf_16, utf_16_little_endian_bom);

struct ucs_4_detector
{
  int in_ucs_4_byte;
};

static void
ucs_4_detect (struct detection_state *st, const UExtbyte *src,
	      Bytecount n)
{
  struct ucs_4_detector *data = DETECTION_STATE_DATA (st, ucs_4);

  while (n--)
    {
      UExtbyte c = *src++;
      switch (data->in_ucs_4_byte)
	{
	case 0:
	  if (c >= 128)
	    {
	      DET_RESULT (st, ucs_4) = DET_NEARLY_IMPOSSIBLE;
	      return;
	    }
	  else
	    data->in_ucs_4_byte++;
	  break;
	case 3:
	  data->in_ucs_4_byte = 0;
	  break;
	default:
	  data->in_ucs_4_byte++;
	}
    }

  /* !!#### write this for real */
  DET_RESULT (st, ucs_4) = DET_AS_LIKELY_AS_UNLIKELY;
}

struct utf_16_detector
{
  unsigned int seen_ffff:1;
  unsigned int seen_forward_bom:1;
  unsigned int seen_rev_bom:1;
  int byteno;
  int prev_char;
  int text, rev_text;
  int sep, rev_sep;
  int num_ascii;
};

static void
utf_16_detect (struct detection_state *st, const UExtbyte *src,
	       Bytecount n)
{
  struct utf_16_detector *data = DETECTION_STATE_DATA (st, utf_16);
  
  while (n--)
    {
      UExtbyte c = *src++;
      int prevc = data->prev_char;
      if (data->byteno == 1 && c == 0xFF && prevc == 0xFE)
	data->seen_forward_bom = 1;
      else if (data->byteno == 1 && c == 0xFE && prevc == 0xFF)
	data->seen_rev_bom = 1;

      if (data->byteno & 1)
	{
	  if (c == 0xFF && prevc == 0xFF)
	    data->seen_ffff = 1;
	  if (prevc == 0
	      && (c == '\r' || c == '\n'
		  || (c >= 0x20 && c <= 0x7E)))
	    data->text++;
	  if (c == 0
	      && (prevc == '\r' || prevc == '\n'
		  || (prevc >= 0x20 && prevc <= 0x7E)))
	    data->rev_text++;
	  /* #### 0x2028 is LINE SEPARATOR and 0x2029 is PARAGRAPH SEPARATOR.
	     I used to count these in text and rev_text but that is very bad,
	     as 0x2028 is also space + left-paren in ASCII, which is extremely
	     common.  So, what do we do with these? */
	  if (prevc == 0x20 && (c == 0x28 || c == 0x29))
	    data->sep++;
	  if (c == 0x20 && (prevc == 0x28 || prevc == 0x29))
	    data->rev_sep++;
	}

      if ((c >= ' ' && c <= '~') || c == '\n' || c == '\r' || c == '\t' ||
	  c == '\f' || c == '\v')
	data->num_ascii++;
      data->byteno++;
      data->prev_char = c;
    }

  {
    int variance_indicates_big_endian =
      (data->text >= 10
       && (data->rev_text == 0
	   || data->text / data->rev_text >= 10));
    int variance_indicates_little_endian =
      (data->rev_text >= 10
       && (data->text == 0
	   || data->rev_text / data->text >= 10));

    if (data->seen_ffff)
      SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
    else if (data->seen_forward_bom)
      {
	SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
	if (variance_indicates_big_endian)
	  DET_RESULT (st, utf_16_bom) = DET_NEAR_CERTAINTY;
	else if (variance_indicates_little_endian)
	  DET_RESULT (st, utf_16_bom) = DET_SOMEWHAT_LIKELY;
	else
	  DET_RESULT (st, utf_16_bom) = DET_QUITE_PROBABLE;
      }
    else if (data->seen_forward_bom)
      {
	SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
	if (variance_indicates_big_endian)
	  DET_RESULT (st, utf_16_bom) = DET_NEAR_CERTAINTY;
	else if (variance_indicates_little_endian)
	  /* #### may need to rethink */
	  DET_RESULT (st, utf_16_bom) = DET_SOMEWHAT_LIKELY;
	else
	  /* #### may need to rethink */
	  DET_RESULT (st, utf_16_bom) = DET_QUITE_PROBABLE;
      }
    else if (data->seen_rev_bom)
      {
	SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
	if (variance_indicates_little_endian)
	  DET_RESULT (st, utf_16_little_endian_bom) = DET_NEAR_CERTAINTY;
	else if (variance_indicates_big_endian)
	  /* #### may need to rethink */
	  DET_RESULT (st, utf_16_little_endian_bom) = DET_SOMEWHAT_LIKELY;
	else
	  /* #### may need to rethink */
	  DET_RESULT (st, utf_16_little_endian_bom) = DET_QUITE_PROBABLE;
      }
    else if (variance_indicates_big_endian)
      {
	SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
	DET_RESULT (st, utf_16) = DET_SOMEWHAT_LIKELY;
	DET_RESULT (st, utf_16_little_endian) = DET_SOMEWHAT_UNLIKELY;
      }
    else if (variance_indicates_little_endian)
      {
	SET_DET_RESULTS (st, utf_16, DET_NEARLY_IMPOSSIBLE);
	DET_RESULT (st, utf_16) = DET_SOMEWHAT_UNLIKELY;
	DET_RESULT (st, utf_16_little_endian) = DET_SOMEWHAT_LIKELY;
      }
    else
      {
	/* #### FUCKME!  There should really be an ASCII detector.  This
	   would rule out the need to have this built-in here as
	   well. --ben */
	int pct_ascii = data->byteno ? (100 * data->num_ascii) / data->byteno
		        : 100;

	if (pct_ascii > 90)
	  SET_DET_RESULTS (st, utf_16, DET_QUITE_IMPROBABLE);
	else if (pct_ascii > 75)
	  SET_DET_RESULTS (st, utf_16, DET_SOMEWHAT_UNLIKELY);
	else
	  SET_DET_RESULTS (st, utf_16, DET_AS_LIKELY_AS_UNLIKELY);
      }
  }
}

struct utf_8_detector
{
  int byteno;
  int first_byte;
  int second_byte;
  int prev_byte;
  int in_utf_8_byte;
  int recent_utf_8_sequence;
  int seen_bogus_utf8;
  int seen_really_bogus_utf8;
  int seen_2byte_sequence;
  int seen_longer_sequence;
  int seen_iso2022_esc;
  int seen_iso_shift;
  unsigned int seen_utf_bom:1;
};

static void
utf_8_detect (struct detection_state *st, const UExtbyte *src,
 	      Bytecount n)
{
  struct utf_8_detector *data = DETECTION_STATE_DATA (st, utf_8);

  while (n--)
    {
      UExtbyte c = *src++;
      switch (data->byteno)
	{
	case 0:
	  data->first_byte = c;
	  break;
	case 1:
	  data->second_byte = c;
	  break;
	case 2:
	  if (data->first_byte == 0xef &&
	      data->second_byte == 0xbb &&
	      c == 0xbf)
	    data->seen_utf_bom = 1;
	  break;
	}

      switch (data->in_utf_8_byte)
	{
	case 0:
	  if (data->prev_byte == ISO_CODE_ESC && c >= 0x28 && c <= 0x2F)
	    data->seen_iso2022_esc++;
	  else if (c == ISO_CODE_SI || c == ISO_CODE_SO)
	    data->seen_iso_shift++;
	  else if (c >= 0xfc)
	    data->in_utf_8_byte = 5;
	  else if (c >= 0xf8)
	    data->in_utf_8_byte = 4;
	  else if (c >= 0xf0)
	    data->in_utf_8_byte = 3;
	  else if (c >= 0xe0)
	    data->in_utf_8_byte = 2;
	  else if (c >= 0xc0)
	    data->in_utf_8_byte = 1;
	  else if (c >= 0x80)
	    data->seen_bogus_utf8++;
	  if (data->in_utf_8_byte > 0)
	    data->recent_utf_8_sequence = data->in_utf_8_byte;
	  break;
	default:
	  if ((c & 0xc0) != 0x80)
	    data->seen_really_bogus_utf8++;
	  else
	    {
	      data->in_utf_8_byte--;
	      if (data->in_utf_8_byte == 0)
		{
		  if (data->recent_utf_8_sequence == 1)
		    data->seen_2byte_sequence++;
		  else
		    {
		      assert (data->recent_utf_8_sequence >= 2);
		      data->seen_longer_sequence++;
		    }
		}
	    }
	}

      data->byteno++;
      data->prev_byte = c;
    }

  /* either BOM or no BOM, but not both */
  SET_DET_RESULTS (st, utf_8, DET_NEARLY_IMPOSSIBLE);


  if (data->seen_utf_bom)
    DET_RESULT (st, utf_8_bom) = DET_NEAR_CERTAINTY;
  else
    {
      if (data->seen_really_bogus_utf8 ||
	  data->seen_bogus_utf8 >= 2)
	; /* bogus */
      else if (data->seen_bogus_utf8)
	DET_RESULT (st, utf_8) = DET_SOMEWHAT_UNLIKELY;
      else if ((data->seen_longer_sequence >= 5 ||
		data->seen_2byte_sequence >= 10) &&
	       (!(data->seen_iso2022_esc + data->seen_iso_shift) ||
		(data->seen_longer_sequence * 2 + data->seen_2byte_sequence) /
		(data->seen_iso2022_esc + data->seen_iso_shift) >= 10))
	/* heuristics, heuristics, we love heuristics */
	DET_RESULT (st, utf_8) = DET_QUITE_PROBABLE;
      else if (data->seen_iso2022_esc ||
	       data->seen_iso_shift >= 3)
	DET_RESULT (st, utf_8) = DET_SOMEWHAT_UNLIKELY;
      else if (data->seen_longer_sequence ||
	       data->seen_2byte_sequence)
	DET_RESULT (st, utf_8) = DET_SOMEWHAT_LIKELY;
      else if (data->seen_iso_shift)
	DET_RESULT (st, utf_8) = DET_SOMEWHAT_UNLIKELY;
      else
	DET_RESULT (st, utf_8) = DET_AS_LIKELY_AS_UNLIKELY;
    }
}

static void
unicode_init_coding_stream (struct coding_stream *str)
{
  struct unicode_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, unicode);
  xzero (*data);
}

static void
unicode_rewind_coding_stream (struct coding_stream *str)
{
  unicode_init_coding_stream (str);
}

static int
unicode_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  if (EQ (key, Qunicode_type))
    {
      enum unicode_encoding_type type;

      if (EQ (value, Qutf_8))
	type = UNICODE_UTF_8;
      else if (EQ (value, Qutf_16))
	type = UNICODE_UTF_16;
      else if (EQ (value, Qutf_7))
	type = UNICODE_UTF_7;
      else if (EQ (value, Qucs_4))
	type = UNICODE_UCS_4;
      else if (EQ (value, Qutf_32))
	type = UNICODE_UTF_32;
      else
	invalid_constant ("Invalid Unicode type", key);
      
      XCODING_SYSTEM_UNICODE_TYPE (codesys) = type;
    }
  else if (EQ (key, Qlittle_endian))
    XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (codesys) = !NILP (value);
  else if (EQ (key, Qneed_bom))
    XCODING_SYSTEM_UNICODE_NEED_BOM (codesys) = !NILP (value);
  else
    return 0;
  return 1;
}

static Lisp_Object
unicode_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  if (EQ (prop, Qunicode_type))
    {
      switch (XCODING_SYSTEM_UNICODE_TYPE (coding_system))
	{
	case UNICODE_UTF_16: return Qutf_16;
	case UNICODE_UTF_8: return Qutf_8;
	case UNICODE_UTF_7: return Qutf_7;
	case UNICODE_UCS_4: return Qucs_4;
	case UNICODE_UTF_32: return Qutf_32;
	default: ABORT ();
	}
    }
  else if (EQ (prop, Qlittle_endian))
    return XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (coding_system) ? Qt : Qnil;
  else if (EQ (prop, Qneed_bom))
    return XCODING_SYSTEM_UNICODE_NEED_BOM (coding_system) ? Qt : Qnil;
  return Qunbound;
}

static void
unicode_print (Lisp_Object cs, Lisp_Object printcharfun,
	       int UNUSED (escapeflag))
{
  write_fmt_string_lisp (printcharfun, "(%s", 1,
                         unicode_getprop (cs, Qunicode_type));
  if (XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (cs))
    write_ascstring (printcharfun, ", little-endian");
  if (XCODING_SYSTEM_UNICODE_NEED_BOM (cs))
    write_ascstring (printcharfun, ", need-bom");
  write_ascstring (printcharfun, ")");
}

int
dfc_coding_system_is_unicode (
#ifdef WIN32_ANY
			      Lisp_Object codesys
#else
			      Lisp_Object UNUSED (codesys)
#endif
			      )
{
#ifdef WIN32_ANY
  codesys = Fget_coding_system (codesys);
  return (EQ (XCODING_SYSTEM_TYPE (codesys), Qunicode) &&
	  XCODING_SYSTEM_UNICODE_TYPE (codesys) == UNICODE_UTF_16 &&
	  XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (codesys));
	      
#else
  return 0;
#endif
}


/************************************************************************/
/*                             Initialization                           */
/************************************************************************/

#ifdef MULE

void
initialize_ascii_control_1_latin_1_unicode_translation (void)
{
  int i;

  for (i = 0; i < 128; i++)
    set_unicode_conversion (i, Vcharset_ascii, 0, i);
  for (i = 128; i < 160; i++)
    set_unicode_conversion (i, Vcharset_control_1, 0, i);
  for (i = 160; i < 256; i++)
    set_unicode_conversion (i, Vcharset_latin_iso8859_1, 0, i);
}

#endif

void
syms_of_unicode (void)
{
#ifdef MULE
  INIT_LISP_OBJECT (precedence_array);

  DEFSUBR (Fset_default_unicode_precedence_list);
  DEFSUBR (Fdefault_unicode_precedence_list);
  DEFSUBR (Fset_buffer_unicode_precedence_list);
  DEFSUBR (Fbuffer_unicode_precedence_list);
  DEFSUBR (Fnormalized_unicode_precedence_list);
  DEFSUBR (Fset_unicode_conversion);

  DEFSUBR (Fload_unicode_mapping_table);

  DEFSYMBOL (Qignore_first_column);
  DEFSYMBOL (Qunicode_registries);

  DEFSYMBOL (Qcharset_tag_to_charset_list);
#endif /* MULE */

  DEFSYMBOL (Qunicode);
  DEFSYMBOL (Qucs_4);
  DEFSYMBOL (Qutf_16);
  DEFSYMBOL (Qutf_32);
  DEFSYMBOL (Qutf_8);
  DEFSYMBOL (Qutf_7);

  DEFSYMBOL (Qneed_bom);

  DEFSYMBOL (Qutf_16);
  DEFSYMBOL (Qutf_16_little_endian);
  DEFSYMBOL (Qutf_16_bom);
  DEFSYMBOL (Qutf_16_little_endian_bom);

  DEFSYMBOL (Qutf_8);
  DEFSYMBOL (Qutf_8_bom);
}

void
coding_system_type_create_unicode (void)
{
  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (unicode, "unicode-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (unicode, print);
  CODING_SYSTEM_HAS_METHOD (unicode, convert);
  CODING_SYSTEM_HAS_METHOD (unicode, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (unicode, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (unicode, putprop);
  CODING_SYSTEM_HAS_METHOD (unicode, getprop);

  INITIALIZE_DETECTOR (utf_8);
  DETECTOR_HAS_METHOD (utf_8, detect);
  INITIALIZE_DETECTOR_CATEGORY (utf_8, utf_8);
  INITIALIZE_DETECTOR_CATEGORY (utf_8, utf_8_bom);

  INITIALIZE_DETECTOR (ucs_4);
  DETECTOR_HAS_METHOD (ucs_4, detect);
  INITIALIZE_DETECTOR_CATEGORY (ucs_4, ucs_4);

  INITIALIZE_DETECTOR (utf_16);
  DETECTOR_HAS_METHOD (utf_16, detect);
  INITIALIZE_DETECTOR_CATEGORY (utf_16, utf_16);
  INITIALIZE_DETECTOR_CATEGORY (utf_16, utf_16_little_endian);
  INITIALIZE_DETECTOR_CATEGORY (utf_16, utf_16_bom);
  INITIALIZE_DETECTOR_CATEGORY (utf_16, utf_16_little_endian_bom);
}

void
reinit_coding_system_type_create_unicode (void)
{
  REINITIALIZE_CODING_SYSTEM_TYPE (unicode);
}

void
vars_of_unicode (void)
{
  Fprovide (intern ("unicode"));

#ifdef MULE
#ifndef UNICODE_INTERNAL
  dump_add_opaque_int (&number_of_jit_charsets);
  dump_add_opaque_int (&last_allocated_jit_c1);
  dump_add_opaque_int (&last_allocated_jit_c2);
  staticpro (&Vcurrent_jit_charset);
  Vcurrent_jit_charset = Qnil;
  staticpro (&Vcharset_descr);
  Vcharset_descr
    = build_defer_string ("Mule charset for otherwise unknown Unicode code points.");
#endif /* not UNICODE_INTERNAL */

  staticpro (&Vdefault_unicode_precedence_list);
  /* Gets reset in complex_vars_of_unicode() */
  Vdefault_unicode_precedence_list = Qnil;
  staticpro (&Vdefault_unicode_precedence_array);
  /* Gets reset in complex_vars_of_unicode() */
  Vdefault_unicode_precedence_array = Qnil;

  staticpro (&Vprecedence_array_charsets_seen_hash);
  Vprecedence_array_charsets_seen_hash =
    make_lisp_hash_table (20, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

  staticpro (&Vprecedence_array_charsets_seen_hash);
  Vprecedence_array_charsets_seen_hash =
    make_lisp_hash_table (20, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

  staticpro (&Vprecedence_list_to_array);
  Vprecedence_list_to_array =
    /* The entries are lists, but for speed in lookup, we only compare
       using `eq'. */
    make_lisp_hash_table (20, HASH_TABLE_KEY_WEAK, HASH_TABLE_EQ);
  staticpro (&Vprecedence_array_cons_to_array);
  Vprecedence_array_cons_to_array =
    /* We must compare with `equal' since we have conses.  We make the table
       key-weak but then we depend on the cons being stored somewhere when
       it's in use.  So we store it in a slot in the buffer. */
    make_lisp_hash_table (20, HASH_TABLE_KEY_WEAK, HASH_TABLE_EQUAL);

  init_blank_unicode_tables ();

  /* Note that the "block" we are describing is a single pointer, and hence
     we could potentially use dump_add_root_block_ptr().  However, given
     the way the descriptions are written, we couldn't use them, and would
     have to write new descriptions for each of the pointers below, since
     we would have to make use of a description with an XD_BLOCK_ARRAY
     in it. */

  dump_add_root_block (&to_unicode_blank_1, sizeof (void *),
		       to_unicode_level_1_desc_1);
  dump_add_root_block (&to_unicode_blank_2, sizeof (void *),
		       to_unicode_level_2_desc_1);

  dump_add_root_block (&from_unicode_blank[1], sizeof (void *),
		       from_unicode_level_1_desc_1);
  dump_add_root_block (&from_unicode_blank[2], sizeof (void *),
		       from_unicode_level_2_desc_1);
  dump_add_root_block (&from_unicode_blank[3], sizeof (void *),
		       from_unicode_level_3_desc_1);
  dump_add_root_block (&from_unicode_blank[4], sizeof (void *),
		       from_unicode_level_4_desc_1);

  DEFVAR_LISP ("unicode-registries", &Qunicode_registries /*
Vector describing the X11 registries searched when using fallback fonts.

"Fallback fonts" here includes by default those fonts used by redisplay
when displaying JIT charsets (used in non-Unicode-internal for holding
Unicode codepoints that can't be otherwise represented), and those used
when no font matching the charset's registries property has been found
(that is, they're probably Mule-specific charsets like Ethiopic or IPA).
*/ );
  Qunicode_registries = vector1 (build_ascstring ("iso10646-1"));
#endif /* MULE */
}

void
complex_vars_of_unicode (void)
{
  /* We used to define this in unicode.el.  But we need it early for
     Cygwin 1.7 -- used in LOCAL_FILE_FORMAT_TO_TSTR() et al. */
  Fmake_coding_system
    (Qutf_8, Qunicode,
     build_defer_string ("UTF-8"),
     nconc2 (list4 (Qdocumentation,
		    build_defer_string (
"UTF-8 Unicode encoding -- ASCII-compatible 8-bit variable-width encoding\n"
"sharing the following principles with the Mule-internal encoding:\n"
"\n"
"  -- All ASCII characters (codepoints 0 through 127) are represented\n"
"     by themselves (i.e. using one byte, with the same value as the\n"
"     ASCII codepoint), and these bytes are disjoint from bytes\n"
"     representing non-ASCII characters.\n"
"\n"
"     This means that any 8-bit clean application can safely process\n"
"     UTF-8-encoded text as it were ASCII, with no corruption (e.g. a\n"
"     '/' byte is always a slash character, never the second byte of\n"
"     some other character, as with Big5, so a pathname encoded in\n"
"     UTF-8 can safely be split up into components and reassembled\n"
"     again using standard ASCII processes).\n"
"\n"
"  -- Leading bytes and non-leading bytes in the encoding of a\n"
"     character are disjoint, so moving backwards is easy.\n"
"\n"
"  -- Given only the leading byte, you know how many following bytes\n"
"     are present.\n"
),
		    Qmnemonic, build_ascstring ("UTF8")),
	     list2 (Qunicode_type, Qutf_8)));

#ifdef MULE
#ifndef UNICODE_INTERNAL
  /* Allocate the first JIT charset so that it appears in Unicode
     precedence lists.  This is important because JIT characters will only
     be generated in jit-ucs-charset-0 is seen in the precedence list.
  */
  allocate_jit_ucs_charset ();
  Vcharset_jit_ucs_charset_0 = Vcurrent_jit_charset;
  staticpro (&Vcharset_jit_ucs_charset_0);
#endif /* not UNICODE_INTERNAL */

  /* Set up a default-unicode-precedence-list for the moment so that
     e.g. unicode.el won't run into real problems doing the stuff it
     does */
  Vdefault_unicode_precedence_list =
#ifndef UNICODE_INTERNAL
    list4 (Vcharset_ascii, Vcharset_control_1, Vcharset_latin_iso8859_1,
	   Vcharset_jit_ucs_charset_0)
#else /* UNICODE_INTERNAL */
    list3 (Vcharset_ascii, Vcharset_control_1, Vcharset_latin_iso8859_1)
#endif /* UNICODE_INTERNAL */
    ;
  Vdefault_unicode_precedence_array =
    simple_convert_predence_list_to_array (Vdefault_unicode_precedence_list);
#endif /* MULE */
}

void
init_unicode (void)
{
#ifdef MULE
  /* We had to clear all references to precedence arrays before dumping,
     because precedence arrays aren't dumpable.  So put them back now.
     WARNING: This calls Lisp, very early in the init process!
     So we make sure that the Lisp code is wrapped in
     `very-early-error-handler'. */
  if (initialized)
    recalculate_unicode_precedence (RUP_CLEAR_HASH_TABLE |
				    RUP_EARLY_ERROR_HANDLING);
#endif /* MULE */
}
