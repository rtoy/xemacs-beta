/* Code to handle Unicode conversion.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2010 Ben Wing.

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

#include "charset.h"
#include "file-coding.h"
#include "opaque.h"

#include "buffer.h"
#include "rangetab.h"
#include "extents.h"

#include "sysfile.h"

/* For more info about how Unicode works under Windows, see intl-win32.c. */

/* Info about Unicode translation tables [ben]:

   FORMAT:
   -------

   We currently use the following format for tables:

   If dimension == 1, to_unicode_table is a 96-element array of ints
   (Unicode code points); else, it's a 96-element array of int * pointers,
   each of which points to a 96-element array of ints.  If no elements in a
   row have been filled in, the pointer will point to a default empty
   table; that way, memory usage is more reasonable but lookup still fast.

   -- If from_unicode_levels == 1, from_unicode_table is a 256-element
   array of shorts (octet 1 in high byte, octet 2 in low byte; we don't
   store Ichars directly to save space).

   -- If from_unicode_levels == 2, from_unicode_table is a 256-element
   array of short * pointers, each of which points to a 256-element array
   of shorts.

   -- If from_unicode_levels == 3, from_unicode_table is a 256-element
   array of short ** pointers, each of which points to a 256-element array
   of short * pointers, each of which points to a 256-element array of
   shorts.

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

#ifdef MULE
/* These range tables are not directly accessible from Lisp: */
static Lisp_Object Vunicode_invalid_and_query_skip_chars;
static Lisp_Object Vutf_8_invalid_and_query_skip_chars;
static Lisp_Object Vunicode_query_skip_chars;

static Lisp_Object Vunicode_query_string, Vunicode_invalid_string,
  Vutf_8_invalid_string;
#endif /* MULE */

/* See the Unicode FAQ, http://www.unicode.org/faq/utf_bom.html#35 for this
   algorithm. 
 
   (They also give another, really verbose one, as part of their explanation
   of the various planes of the encoding, but we won't use that.) */
 
#define UTF_16_LEAD_OFFSET (0xD800 - (0x10000 >> 10))
#define UTF_16_SURROGATE_OFFSET (0x10000 - (0xD800 << 10) - 0xDC00)

#define utf_16_surrogates_to_code(lead, trail) \
  (((lead) << 10) + (trail) + UTF_16_SURROGATE_OFFSET)

#define CODE_TO_UTF_16_SURROGATES(codepoint, lead, trail) do {	\
    int __ctu16s_code = (codepoint);				\
    lead = UTF_16_LEAD_OFFSET + (__ctu16s_code >> 10);		\
    trail = 0xDC00 + (__ctu16s_code & 0x3FF);			\
} while (0)

#ifdef MULE 

/* Using ints for to_unicode is OK (as long as they are >= 32 bits).
   In from_unicode, we're converting from Mule characters, which means
   that the values being converted to are only 96x96, and we can save
   space by using shorts (signedness doesn't matter). */
static int *to_unicode_blank_1;
static int **to_unicode_blank_2;

static short *from_unicode_blank_1;
static short **from_unicode_blank_2;
static short ***from_unicode_blank_3;
static short ****from_unicode_blank_4;

static const struct memory_description to_unicode_level_0_desc_1[] = {
  { XD_END }
};

static const struct sized_memory_description to_unicode_level_0_desc = {
  sizeof (int), to_unicode_level_0_desc_1
};

static const struct memory_description to_unicode_level_1_desc_1[] = {
  { XD_BLOCK_PTR, 0, 96, { &to_unicode_level_0_desc } },
  { XD_END }
};

static const struct sized_memory_description to_unicode_level_1_desc = {
  sizeof (void *), to_unicode_level_1_desc_1
};

static const struct memory_description to_unicode_description_1[] = {
  { XD_BLOCK_PTR, 1, 96, { &to_unicode_level_0_desc } },
  { XD_BLOCK_PTR, 2, 96, { &to_unicode_level_1_desc } },
  { XD_END }
};

/* Not static because each charset has a set of to and from tables and
   needs to describe them to pdump. */
const struct sized_memory_description to_unicode_description = {
  sizeof (void *), to_unicode_description_1
};

/* Used only for to_unicode_blank_2 */
static const struct memory_description to_unicode_level_2_desc_1[] = {
  { XD_BLOCK_PTR, 0, 96, { &to_unicode_level_1_desc } },
  { XD_END }
};

static const struct memory_description from_unicode_level_0_desc_1[] = {
  { XD_END }
};

static const struct sized_memory_description from_unicode_level_0_desc = {
   sizeof (short), from_unicode_level_0_desc_1
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

/* Used only for from_unicode_blank_4 */
static const struct memory_description from_unicode_level_4_desc_1[] = {
  { XD_BLOCK_PTR, 0, 256, { &from_unicode_level_3_desc } },
  { XD_END }
};

static Lisp_Object_dynarr *unicode_precedence_dynarr;

static const struct memory_description lod_description_1[] = {
  XD_DYNARR_DESC (Lisp_Object_dynarr, &lisp_object_description),
  { XD_END }
};

static const struct sized_memory_description lisp_object_dynarr_description = {
  sizeof (Lisp_Object_dynarr),
  lod_description_1
};

Lisp_Object Vlanguage_unicode_precedence_list;
Lisp_Object Vdefault_unicode_precedence_list;

Lisp_Object Qignore_first_column;

Lisp_Object Vcurrent_jit_charset;
Lisp_Object Qlast_allocated_character;
Lisp_Object Qccl_encode_to_ucs_2;

Lisp_Object Vnumber_of_jit_charsets;
Lisp_Object Vlast_jit_charset_final;
Lisp_Object Vcharset_descr;



/************************************************************************/
/*                        Unicode implementation                        */
/************************************************************************/

#define BREAKUP_UNICODE_CODE(val, u1, u2, u3, u4, levels)	\
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

static void
init_blank_unicode_tables (void)
{
  int i;

  from_unicode_blank_1 = xnew_array (short, 256);
  from_unicode_blank_2 = xnew_array (short *, 256);
  from_unicode_blank_3 = xnew_array (short **, 256);
  from_unicode_blank_4 = xnew_array (short ***, 256);
  for (i = 0; i < 256; i++)
    {
      /* #### IMWTK: Why does using -1 here work? Simply because there are
         no existing 96x96 charsets?

         Answer: I don't understand the concern.  -1 indicates there is no
         entry for this particular codepoint, which is always the case for
	 blank tables. */
      from_unicode_blank_1[i] = (short) -1;
      from_unicode_blank_2[i] = from_unicode_blank_1;
      from_unicode_blank_3[i] = from_unicode_blank_2;
      from_unicode_blank_4[i] = from_unicode_blank_3;
    }

  to_unicode_blank_1 = xnew_array (int, 96);
  to_unicode_blank_2 = xnew_array (int *, 96);
  for (i = 0; i < 96; i++)
    {
      /* Here -1 is guaranteed OK. */
      to_unicode_blank_1[i] = -1;
      to_unicode_blank_2[i] = to_unicode_blank_1;
    }
}

static void *
create_new_from_unicode_table (int level)
{
  switch (level)
    {
      /* WARNING: If you are thinking of compressing these, keep in
	 mind that sizeof (short) does not equal sizeof (short *). */
    case 1:
      {
	short *newtab = xnew_array (short, 256);
	memcpy (newtab, from_unicode_blank_1, 256 * sizeof (short));
	return newtab;
      }
    case 2:
      {
	short **newtab = xnew_array (short *, 256);
	memcpy (newtab, from_unicode_blank_2, 256 * sizeof (short *));
	return newtab;
      }
    case 3:
      {
	short ***newtab = xnew_array (short **, 256);
	memcpy (newtab, from_unicode_blank_3, 256 * sizeof (short **));
	return newtab;
      }
    case 4:
      {
	short ****newtab = xnew_array (short ***, 256);
	memcpy (newtab, from_unicode_blank_4, 256 * sizeof (short ***));
	return newtab;
      }
    default:
      ABORT ();
      return 0;
    }
}

/* Allocate and blank the tables.
   Loading them up is done by load-unicode-mapping-table. */
void
init_charset_unicode_tables (Lisp_Object charset)
{
  if (XCHARSET_DIMENSION (charset) == 1)
    {
      int *to_table = xnew_array (int, 96);
      memcpy (to_table, to_unicode_blank_1, 96 * sizeof (int));
      XCHARSET_TO_UNICODE_TABLE (charset) = to_table;
    }
  else
    {
      int **to_table = xnew_array (int *, 96);
      memcpy (to_table, to_unicode_blank_2, 96 * sizeof (int *));
      XCHARSET_TO_UNICODE_TABLE (charset) = to_table;
    }

  {
    XCHARSET_FROM_UNICODE_TABLE (charset) =
      create_new_from_unicode_table (1);
    XCHARSET_FROM_UNICODE_LEVELS (charset) = 1;
  }
}

static void
free_from_unicode_table (void *table, int level)
{
  int i;

  switch (level)
    {
    case 2:
      {
	short **tab = (short **) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_1)
	      free_from_unicode_table (tab[i], 1);
	  }
	break;
      }
    case 3:
      {
	short ***tab = (short ***) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_2)
	      free_from_unicode_table (tab[i], 2);
	  }
	break;
      }
    case 4:
      {
	short ****tab = (short ****) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_3)
	      free_from_unicode_table (tab[i], 3);
	  }
	break;
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

      for (i = 0; i < 96; i++)
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
			 XCHARSET_DIMENSION (charset));
  free_from_unicode_table (XCHARSET_FROM_UNICODE_TABLE (charset),
			   XCHARSET_FROM_UNICODE_LEVELS (charset));
}

#ifdef MEMORY_USAGE_STATS

static Bytecount
compute_from_unicode_table_size_1 (void *table, int level,
				   struct usage_stats *stats)
{
  int i;
  Bytecount size = 0;

  switch (level)
    {
    case 2:
      {
	short **tab = (short **) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_1)
	      size += compute_from_unicode_table_size_1 (tab[i], 1, stats);
	  }
	break;
      }
    case 3:
      {
	short ***tab = (short ***) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_2)
	      size += compute_from_unicode_table_size_1 (tab[i], 2, stats);
	  }
	break;
      }
    case 4:
      {
	short ****tab = (short ****) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_3)
	      size += compute_from_unicode_table_size_1 (tab[i], 3, stats);
	  }
	break;
      }
    }

  size += malloced_storage_size (table,
				 256 * (level == 1 ? sizeof (short) :
					sizeof (void *)),
				 stats);
  return size;
}

static Bytecount
compute_to_unicode_table_size_1 (void *table, int level,
				 struct usage_stats *stats)
{
  Bytecount size = 0;

  if (level == 2)
    {
      int i;
      int **tab = (int **) table;

      for (i = 0; i < 96; i++)
	{
	  if (tab[i] != to_unicode_blank_1)
	    size += compute_to_unicode_table_size_1 (tab[i], 1, stats);
	}
    }

  size += malloced_storage_size (table,
				 96 * (level == 1 ? sizeof (int) :
				       sizeof (void *)),
				 stats);
  return size;
}

Bytecount
compute_from_unicode_table_size (Lisp_Object charset,
				 struct usage_stats *stats)
{
  return (compute_from_unicode_table_size_1
	  (XCHARSET_FROM_UNICODE_TABLE (charset),
	   XCHARSET_FROM_UNICODE_LEVELS (charset),
	   stats));
}

Bytecount
compute_to_unicode_table_size (Lisp_Object charset,
			       struct usage_stats *stats)
{
  return (compute_to_unicode_table_size_1
	  (XCHARSET_TO_UNICODE_TABLE (charset),
	   XCHARSET_DIMENSION (charset),
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
  assert (tab != from_unicode_blank_1);
  assert (tab != from_unicode_blank_2);
  assert (tab != from_unicode_blank_3);
  assert (tab != from_unicode_blank_4);
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
	short *tab = (short *) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != -1)
	      {
		Lisp_Object char_charset;
		int c1, c2;

		assert (valid_ichar_p (tab[i]));
		BREAKUP_ICHAR (tab[i], char_charset, c1, c2);
		assert (EQ (charset, char_charset));
		if (XCHARSET_DIMENSION (charset) == 1)
		  {
		    int *to_table =
		      (int *) XCHARSET_TO_UNICODE_TABLE (charset);
		    assert_not_any_blank_table (to_table);
		    assert (to_table[c1 - 32] == (codetop << 8) + i);
		  }
		else
		  {
		    int **to_table =
		      (int **) XCHARSET_TO_UNICODE_TABLE (charset);
		    assert_not_any_blank_table (to_table);
		    assert_not_any_blank_table (to_table[c1 - 32]);
		    assert (to_table[c1 - 32][c2 - 32] == (codetop << 8) + i);
		  }
	      }
	  }
	break;
      }
    case 2:
      {
	short **tab = (short **) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_1)
	      sledgehammer_check_from_table (charset, tab[i], 1,
					     (codetop << 8) + i);
	  }
	break;
      }
    case 3:
      {
	short ***tab = (short ***) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_2)
	      sledgehammer_check_from_table (charset, tab[i], 2,
					     (codetop << 8) + i);
	  }
	break;
      }
    case 4:
      {
	short ****tab = (short ****) table;
	for (i = 0; i < 256; i++)
	  {
	    if (tab[i] != from_unicode_blank_3)
	      sledgehammer_check_from_table (charset, tab[i], 3,
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

  switch (level)
    {
    case 1:
      {
	int *tab = (int *) table;

	if (XCHARSET_CHARS (charset) == 94)
	  {
	    assert (tab[0] == -1);
	    assert (tab[95] == -1);
	  }

	for (i = 0; i < 96; i++)
	  {
	    if (tab[i] != -1)
	      {
		int u4, u3, u2, u1, levels;
		Ichar ch;
		Ichar this_ch;
		short val;
		void *frtab = XCHARSET_FROM_UNICODE_TABLE (charset);

		if (XCHARSET_DIMENSION (charset) == 1)
		  this_ch = make_ichar (charset, i + 32, 0);
		else
		  this_ch = make_ichar (charset, codetop + 32, i + 32);

		assert (tab[i] >= 0);
		BREAKUP_UNICODE_CODE (tab[i], u4, u3, u2, u1, levels);
		assert (levels <= XCHARSET_FROM_UNICODE_LEVELS (charset));

		switch (XCHARSET_FROM_UNICODE_LEVELS (charset))
		  {
		  case 1: val = ((short *) frtab)[u1]; break;
		  case 2: val = ((short **) frtab)[u2][u1]; break;
		  case 3: val = ((short ***) frtab)[u3][u2][u1]; break;
		  case 4: val = ((short ****) frtab)[u4][u3][u2][u1]; break;
		  default: ABORT ();
		  }

		ch = make_ichar (charset, val >> 8, val & 0xFF);
		assert (ch == this_ch);

		switch (XCHARSET_FROM_UNICODE_LEVELS (charset))
		  {
		  case 4:
		    assert_not_any_blank_table (frtab);
		    frtab = ((short ****) frtab)[u4];
		    /* fall through */
		  case 3:
		    assert_not_any_blank_table (frtab);
		    frtab = ((short ***) frtab)[u3];
		    /* fall through */
		  case 2:
		    assert_not_any_blank_table (frtab);
		    frtab = ((short **) frtab)[u2];
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

	if (XCHARSET_CHARS (charset) == 94)
	  {
	    assert (tab[0] == to_unicode_blank_1);
	    assert (tab[95] == to_unicode_blank_1);
	  }

	for (i = 0; i < 96; i++)
	  {
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
      assert (from_unicode_blank_1[i] == (short) -1);
      assert (from_unicode_blank_2[i] == from_unicode_blank_1);
      assert (from_unicode_blank_3[i] == from_unicode_blank_2);
      assert (from_unicode_blank_4[i] == from_unicode_blank_3);
    }

  for (i = 0; i < 96; i++)
    {
      assert (to_unicode_blank_1[i] == -1);
      assert (to_unicode_blank_2[i] == to_unicode_blank_1);
    }

  assert (from_level >= 1 && from_level <= 4);

  sledgehammer_check_from_table (charset,
				 XCHARSET_FROM_UNICODE_TABLE (charset),
				 from_level, 0);

  sledgehammer_check_to_table (charset,
			       XCHARSET_TO_UNICODE_TABLE (charset),
			       XCHARSET_DIMENSION (charset), 0);
}

#endif /* SLEDGEHAMMER_CHECK_UNICODE */

static void
set_unicode_conversion (Ichar chr, int code)
{
  Lisp_Object charset;
  int c1, c2;

  BREAKUP_ICHAR (chr, charset, c1, c2);

  /* I tried an assert on code > 255 || chr == code, but that fails because
     Mule gives many Latin characters separate code points for different
     ISO 8859 coded character sets.  Obvious in hindsight.... */
  assert (!EQ (charset, Vcharset_ascii) || chr == code);
  assert (!EQ (charset, Vcharset_latin_iso8859_1) || chr == code);
  assert (!EQ (charset, Vcharset_control_1) || chr == code);

  /* This assert is needed because it is simply unimplemented. */
  assert (!EQ (charset, Vcharset_composite));

#ifdef SLEDGEHAMMER_CHECK_UNICODE
  sledgehammer_check_unicode_tables (charset);
#endif

  if (EQ(charset, Vcharset_ascii) || EQ(charset, Vcharset_control_1))
    return;

  /* First, the char -> unicode translation */

  if (XCHARSET_DIMENSION (charset) == 1)
    {
      int *to_table = (int *) XCHARSET_TO_UNICODE_TABLE (charset);
      to_table[c1 - 32] = code;
    }
  else
    {
      int **to_table_2 = (int **) XCHARSET_TO_UNICODE_TABLE (charset);
      int *to_table_1;

      assert (XCHARSET_DIMENSION (charset) == 2);
      to_table_1 = to_table_2[c1 - 32];
      if (to_table_1 == to_unicode_blank_1)
	{
	  to_table_1 = xnew_array (int, 96);
	  memcpy (to_table_1, to_unicode_blank_1, 96 * sizeof (int));
	  to_table_2[c1 - 32] = to_table_1;
	}
      to_table_1[c2 - 32] = code;
    }

  /* Then, unicode -> char: much harder */

  {
    int charset_levels;
    int u4, u3, u2, u1;
    int code_levels;
    BREAKUP_UNICODE_CODE (code, u4, u3, u2, u1, code_levels);

    charset_levels = XCHARSET_FROM_UNICODE_LEVELS (charset);

    /* Make sure the charset's tables have at least as many levels as
       the code point has: Note that the charset is guaranteed to have
       at least one level, because it was created that way */
    if (charset_levels < code_levels)
      {
	int i;

	assert (charset_levels > 0);
	for (i = 2; i <= code_levels; i++)
	  {
	    if (charset_levels < i)
	      {
		void *old_table = XCHARSET_FROM_UNICODE_TABLE (charset);
		void *table = create_new_from_unicode_table (i);
		XCHARSET_FROM_UNICODE_TABLE (charset) = table;
		  
		switch (i)
		  {
		  case 2:
		    ((short **) table)[0] = (short *) old_table;
		    break;
		  case 3:
		    ((short ***) table)[0] = (short **) old_table;
		    break;
		  case 4:
		    ((short ****) table)[0] = (short ***) old_table;
		    break;
		  default: ABORT ();
		  }
	      }
	  }

	charset_levels = code_levels;
	XCHARSET_FROM_UNICODE_LEVELS (charset) = code_levels;
      }

    /* Now, make sure there is a non-default table at each level */
    {
      int i;
      void *table = XCHARSET_FROM_UNICODE_TABLE (charset);

      for (i = charset_levels; i >= 2; i--)
	{
	  switch (i)
	    {
	    case 4:
	      if (((short ****) table)[u4] == from_unicode_blank_3)
		((short ****) table)[u4] =
		  ((short ***) create_new_from_unicode_table (3));
	      table = ((short ****) table)[u4];
	      break;
	    case 3:
	      if (((short ***) table)[u3] == from_unicode_blank_2)
		((short ***) table)[u3] =
		  ((short **) create_new_from_unicode_table (2));
	      table = ((short ***) table)[u3];
	      break;
	    case 2:
	      if (((short **) table)[u2] == from_unicode_blank_1)
		((short **) table)[u2] =
		  ((short *) create_new_from_unicode_table (1));
	      table = ((short **) table)[u2];
	      break;
	    default: ABORT ();
	    }
	}
    }

    /* Finally, set the character */
	  
    {
      void *table = XCHARSET_FROM_UNICODE_TABLE (charset);
      switch (charset_levels)
	{
	case 1: ((short *) table)[u1] = (c1 << 8) + c2; break;
	case 2: ((short **) table)[u2][u1] = (c1 << 8) + c2; break;
	case 3: ((short ***) table)[u3][u2][u1] = (c1 << 8) + c2; break;
	case 4: ((short ****) table)[u4][u3][u2][u1] = (c1 << 8) + c2; break;
	default:  ABORT ();
	}
    }
  }

#ifdef SLEDGEHAMMER_CHECK_UNICODE
  sledgehammer_check_unicode_tables (charset);
#endif
}

int
ichar_to_unicode (Ichar chr)
{
  Lisp_Object charset;
  int c1, c2;

  type_checking_assert (valid_ichar_p (chr));
  /* This shortcut depends on the representation of an Ichar, see text.c. */
  if (chr < 256)
    return (int) chr;

  BREAKUP_ICHAR (chr, charset, c1, c2);
  if (EQ (charset, Vcharset_composite))
    return -1; /* #### don't know how to handle */
  else if (XCHARSET_DIMENSION (charset) == 1)
    return ((int *) XCHARSET_TO_UNICODE_TABLE (charset))[c1 - 32];
  else
    return ((int **) XCHARSET_TO_UNICODE_TABLE (charset))[c1 - 32][c2 - 32];
}

static Ichar
get_free_codepoint(Lisp_Object charset)
{
  Lisp_Object name = Fcharset_name(charset);
  Lisp_Object zeichen = Fget(name, Qlast_allocated_character, Qnil);
  Ichar res;

  /* Only allow this with the 96x96 character sets we are using for
     temporary Unicode support. */
  assert(2 == XCHARSET_DIMENSION(charset) && 96 == XCHARSET_CHARS(charset));

  if (!NILP(zeichen))
    {
      int c1, c2;

      BREAKUP_ICHAR(XCHAR(zeichen), charset, c1, c2);

      if (127 == c1 && 127 == c2)
	{
	  /* We've already used the hightest-numbered character in this
	     set--tell our caller to create another. */
	  return -1;
	}

      if (127 == c2)
	{
	  ++c1;
	  c2 = 0x20;
	}
      else
	{
	  ++c2;
	}

      res = make_ichar(charset, c1, c2);
      Fput(name, Qlast_allocated_character, make_char(res));
    }
  else
    {
      res = make_ichar(charset, 32, 32);
      Fput(name, Qlast_allocated_character, make_char(res));
    }
  return res;
}

/* The just-in-time creation of XEmacs characters that correspond to unknown
   Unicode code points happens when: 

   1. The lookup would otherwise fail. 

   2. The charsets array is the nil or the default. 

   If there are no free code points in the just-in-time Unicode character
   set, and the charsets array is the default unicode precedence list,
   create a new just-in-time Unicode character set, add it at the end of the
   unicode precedence list, create the XEmacs character in that character
   set, and return it. */

static Ichar
unicode_to_ichar (int code, Lisp_Object_dynarr *charsets)
{
  int u1, u2, u3, u4;
  int code_levels;
  int i;
  int n = Dynarr_length (charsets);

  type_checking_assert (code >= 0);
  /* This shortcut depends on the representation of an Ichar, see text.c.
     Note that it may _not_ be extended to U+00A0 to U+00FF (many ISO 8859
     coded character sets have points that map into that region, so this
     function is many-valued). */
  if (code < 0xA0)
    return (Ichar) code;

  BREAKUP_UNICODE_CODE (code, u4, u3, u2, u1, code_levels);

  for (i = 0; i < n; i++)
    {
      Lisp_Object charset = Dynarr_at (charsets, i);
      int charset_levels = XCHARSET_FROM_UNICODE_LEVELS (charset);
      if (charset_levels >= code_levels)
	{
	  void *table = XCHARSET_FROM_UNICODE_TABLE (charset);
	  short retval;

	  switch (charset_levels)
	    {
	    case 1: retval = ((short *) table)[u1]; break;
	    case 2: retval = ((short **) table)[u2][u1]; break;
	    case 3: retval = ((short ***) table)[u3][u2][u1]; break;
	    case 4: retval = ((short ****) table)[u4][u3][u2][u1]; break;
	    default: ABORT (); retval = 0;
	    }

	  if (retval != -1)
	    return make_ichar (charset, retval >> 8, retval & 0xFF);
	}
    }
  
  /* Only do the magic just-in-time assignment if we're using the default
     list. */ 
  if (unicode_precedence_dynarr == charsets) 
    {
      if (NILP (Vcurrent_jit_charset) || 
	  (-1 == (i = get_free_codepoint(Vcurrent_jit_charset))))
	{
	  Ibyte setname[32]; 
	  int number_of_jit_charsets = XFIXNUM (Vnumber_of_jit_charsets);
	  Ascbyte last_jit_charset_final = XCHAR (Vlast_jit_charset_final);

	  /* This final byte shit is, umm, not that cool. */
	  assert (last_jit_charset_final >= 0x30);

	  /* Assertion added partly because our Win32 layer doesn't
	     support snprintf; with this, we're sure it won't overflow
	     the buffer.  */
	  assert(100 > number_of_jit_charsets);

	  qxesprintf(setname, "jit-ucs-charset-%d", number_of_jit_charsets);

	  Vcurrent_jit_charset = Fmake_charset 
	    (intern ((const CIbyte *) setname), Vcharset_descr, 
	     /* Set encode-as-utf-8 to t, to have this character set written
		using UTF-8 escapes in escape-quoted and ctext. This
		sidesteps the fact that our internal character -> Unicode
		mapping is not stable from one invocation to the next.  */
	     listu (Qencode_as_utf_8, Qt, Qcolumns, make_fixnum (1),
		    Qchars, make_fixnum (96), Qdimension, make_fixnum (2),
		    Qregistries, Qunicode_registries,
		    Qfinal, make_char (last_jit_charset_final),
		    /* This CCL program is initialised in
		       unicode.el. */
		    Qccl_program, Qccl_encode_to_ucs_2, Qunbound));

	  /* Record for the Unicode infrastructure that we've created
	     this character set.  */
	  Vnumber_of_jit_charsets = make_fixnum (number_of_jit_charsets + 1);
	  Vlast_jit_charset_final = make_char (last_jit_charset_final + 1);

	  i = get_free_codepoint(Vcurrent_jit_charset);
	} 

      if (-1 != i)
	{
	  set_unicode_conversion((Ichar)i, code);
	  /* No need to add the charset to the end of the list; it's done
	     automatically. */
	}
    }
  return (Ichar) i;
}

/* Add charsets to precedence list.
   LIST must be a list of charsets.  Charsets which are in the list more
   than once are given the precedence implied by their earliest appearance.
   Later appearances are ignored. */
static void
add_charsets_to_precedence_list (Lisp_Object list, int *lbs,
				 Lisp_Object_dynarr *dynarr)
{
  {
    EXTERNAL_LIST_LOOP_2 (elt, list)
      {
	Lisp_Object charset = Fget_charset (elt);
	int lb = XCHARSET_LEADING_BYTE (charset);
	if (lbs[lb - MIN_LEADING_BYTE] == 0)
	  {
	    Dynarr_add (dynarr, charset);
	    lbs[lb - MIN_LEADING_BYTE] = 1;
	  }
      }
  }
}

/* Rebuild the charset precedence array.
   The "charsets preferred for the current language" get highest precedence,
   followed by the "charsets preferred by default", ordered as in
   Vlanguage_unicode_precedence_list and Vdefault_unicode_precedence_list,
   respectively.  All remaining charsets follow in an arbitrary order. */
void
recalculate_unicode_precedence (void)
{
  int lbs[NUM_LEADING_BYTES];
  int i;

  for (i = 0; i < NUM_LEADING_BYTES; i++)
    lbs[i] = 0;

  Dynarr_reset (unicode_precedence_dynarr);

  add_charsets_to_precedence_list (Vlanguage_unicode_precedence_list,
				   lbs, unicode_precedence_dynarr);
  add_charsets_to_precedence_list (Vdefault_unicode_precedence_list,
				   lbs, unicode_precedence_dynarr);

  for (i = 0; i < NUM_LEADING_BYTES; i++)
    {
      if (lbs[i] == 0)
	{
	  Lisp_Object charset = charset_by_leading_byte (i + MIN_LEADING_BYTE);
	  if (!NILP (charset))
	    Dynarr_add (unicode_precedence_dynarr, charset);
	}
    }
}

DEFUN ("unicode-precedence-list", 
       Funicode_precedence_list,
       0, 0, 0, /*
Return the precedence order among charsets used for Unicode decoding.

Value is a list of charsets, which are searched in order for a translation
matching a given Unicode character.

The highest precedence is given to the language-specific precedence list of
charsets, defined by `set-language-unicode-precedence-list'.  These are
followed by charsets in the default precedence list, defined by
`set-default-unicode-precedence-list'.  Charsets occurring multiple times are
given precedence according to their first occurrence in either list.  These
are followed by the remaining charsets, in some arbitrary order.

The language-specific precedence list is meant to be set as part of the
language environment initialization; the default precedence list is meant
to be set by the user.

#### NOTE: This interface may be changed.
*/
       ())
{
  int i;
  Lisp_Object list = Qnil;

  for (i = Dynarr_length (unicode_precedence_dynarr) - 1; i >= 0; i--)
    list = Fcons (Dynarr_at (unicode_precedence_dynarr, i), list);
  return list;
}


/* #### This interface is wrong.  Cyrillic users and Chinese users are going
   to have varying opinions about whether ISO Cyrillic, KOI8-R, or Windows
   1251 should take precedence, and whether Big Five or CNS should take
   precedence, respectively.  This means that users are sometimes going to
   want to set Vlanguage_unicode_precedence_list.
   Furthermore, this should be language-local (buffer-local would be a
   reasonable approximation).

   Answer: You are right, this needs rethinking. */
DEFUN ("set-language-unicode-precedence-list",
       Fset_language_unicode_precedence_list,
       1, 1, 0, /*
Set the language-specific precedence of charsets in Unicode decoding.
LIST is a list of charsets.
See `unicode-precedence-list' for more information.

#### NOTE: This interface may be changed.
*/
       (list))
{
  {
    EXTERNAL_LIST_LOOP_2 (elt, list)
      Fget_charset (elt);
  }

  Vlanguage_unicode_precedence_list = list;
  recalculate_unicode_precedence ();
  return Qnil;
}

DEFUN ("language-unicode-precedence-list",
       Flanguage_unicode_precedence_list,
       0, 0, 0, /*
Return the language-specific precedence list used for Unicode decoding.
See `unicode-precedence-list' for more information.

#### NOTE: This interface may be changed.
*/
       ())
{
  return Vlanguage_unicode_precedence_list;
}

DEFUN ("set-default-unicode-precedence-list",
       Fset_default_unicode_precedence_list,
       1, 1, 0, /*
Set the default precedence list used for Unicode decoding.
This is intended to be set by the user.  See
`unicode-precedence-list' for more information.

#### NOTE: This interface may be changed.
*/
       (list))
{
  {
    EXTERNAL_LIST_LOOP_2 (elt, list)
      Fget_charset (elt);
  }

  Vdefault_unicode_precedence_list = list;
  recalculate_unicode_precedence ();
  return Qnil;
}

DEFUN ("default-unicode-precedence-list",
       Fdefault_unicode_precedence_list,
       0, 0, 0, /*
Return the default precedence list used for Unicode decoding.
See `unicode-precedence-list' for more information.

#### NOTE: This interface may be changed.
*/
       ())
{
  return Vdefault_unicode_precedence_list;
}

DEFUN ("set-unicode-conversion", Fset_unicode_conversion,
       2, 2, 0, /*
Add conversion information between Unicode codepoints and characters.
Conversions for U+0000 to U+00FF are hardwired to ASCII, Control-1, and
Latin-1.  Attempts to set these values will raise an error.

CHARACTER is one of the following:

-- A character (in which case CODE must be a non-negative integer; values
   above 2^20 - 1 are allowed for the purpose of specifying private
   characters, but are illegal in standard Unicode---they will cause errors
   when converted to utf-16)
-- A vector of characters (in which case CODE must be a vector of integers
   of the same length)
*/
       (character, code))
{
  Lisp_Object charset;
  int ichar, unicode;

  CHECK_CHAR (character);

  check_integer_range (code, Qzero, make_fixnum (MOST_POSITIVE_FIXNUM));

  unicode = XFIXNUM (code);
  ichar = XCHAR (character);
  charset = ichar_charset (ichar);

  /* The translations of ASCII, Control-1, and Latin-1 code points are
     hard-coded in ichar_to_unicode and unicode_to_ichar.

     Checking unicode < 256 && ichar != unicode is wrong because Mule gives
     many Latin characters code points in a few different character sets. */
  if ((EQ (charset, Vcharset_ascii) ||
       EQ (charset, Vcharset_control_1) ||
       EQ (charset, Vcharset_latin_iso8859_1))
      && unicode != ichar)
    signal_error (Qinvalid_argument, "Can't change Unicode translation for ASCII, Control-1 or Latin-1 character",
		  character);

  /* #### Composite characters are not properly implemented yet. */
  if (EQ (charset, Vcharset_composite))
    signal_error (Qinvalid_argument, "Can't set Unicode translation for Composite char",
		  character);

  set_unicode_conversion (ichar, unicode);
  return Qnil;
}

#endif /* MULE */

DEFUN ("char-to-unicode", Fchar_to_unicode, 1, 1, 0, /*
Convert character to Unicode codepoint.
When there is no international support (i.e. the `mule' feature is not
present), this function simply does `char-to-int'.
*/
       (character))
{
  CHECK_CHAR (character);
#ifdef MULE
  return make_fixnum (ichar_to_unicode (XCHAR (character)));
#else
  return Fchar_to_int (character);
#endif /* MULE */
}

DEFUN ("unicode-to-char", Funicode_to_char, 1, 2, 0, /*
Convert Unicode codepoint to character.
CODE should be a non-negative integer.
If CHARSETS is given, it should be a list of charsets, and only those
charsets will be consulted, in the given order, for a translation.
Otherwise, the default ordering of all charsets will be given (see
`set-unicode-charset-precedence').

When there is no international support (i.e. the `mule' feature is not
present), this function simply does `int-to-char' and ignores the CHARSETS
argument.

If the CODE would not otherwise be converted to an XEmacs character, and the
list of character sets to be consulted is nil or the default, a new XEmacs
character will be created for it in one of the `jit-ucs-charset' Mule
character sets, and that character will be returned.  

This is limited to around 400,000 characters per XEmacs session, though, so
while normal usage will not be problematic, things like:

\(dotimes (i #x110000) (decode-char 'ucs i))

will eventually error.  The long-term solution to this is Unicode as an
internal encoding. 
*/
       (code, USED_IF_MULE (charsets)))
{
#ifdef MULE
  Lisp_Object_dynarr *dyn;
  int lbs[NUM_LEADING_BYTES];
  int c;

  check_integer_range (code, Qzero, make_fixnum (MOST_POSITIVE_FIXNUM));
  c = XFIXNUM (code);
  {
    EXTERNAL_LIST_LOOP_2 (elt, charsets)
      Fget_charset (elt);
  }

  if (NILP (charsets))
    {
      Ichar ret = unicode_to_ichar (c, unicode_precedence_dynarr);
      if (ret == -1)
	return Qnil;
      return make_char (ret);
    }

  dyn = Dynarr_new (Lisp_Object);
  memset (lbs, 0, NUM_LEADING_BYTES * sizeof (int));
  add_charsets_to_precedence_list (charsets, lbs, dyn);
  {
    Ichar ret = unicode_to_ichar (c, dyn);
    Dynarr_free (dyn);
    if (ret == -1)
      return Qnil;
    return make_char (ret);
  }
#else
  check_integer_range (code, Qzero, make_fixnum (MOST_POSITIVE_FIXNUM));
  return Fint_to_char (code);
#endif /* MULE */
}

#ifdef MULE

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
Data is text, in the form of one translation per line -- charset
codepoint followed by Unicode codepoint.  Numbers are decimal or hex
\(preceded by 0x).  Comments are marked with a #.  Charset codepoints
for two-dimensional charsets have the first octet stored in the
high 8 bits of the hex number and the second in the low 8 bits.

If START and END are given, only charset codepoints within the given
range will be processed.  (START and END apply to the codepoints in the
file, before OFFSET is applied.)

If OFFSET is given, that value will be added to all charset codepoints
in the file to obtain the internal charset codepoint.  \(We assume
that octets in the table are in the range 33 to 126 or 32 to 127.  If
you have a table in ku-ten form, with octets in the range 1 to 94, you
will have to use an offset of 5140, i.e. 0x2020.)

FLAGS, if specified, control further how the tables are interpreted
and are used to special-case certain known format deviations in the
Unicode tables or in the charset:

`ignore-first-column'
  The JIS X 0208 tables have 3 columns of data instead of 2.  The first
  column contains the Shift-JIS codepoint, which we ignore.
`big5'
  The charset codepoints are Big Five codepoints; convert it to the
  hacked-up Mule codepoint in `chinese-big5-1' or `chinese-big5-2'.
*/
     (filename, charset, start, end, offset, flags))
{
  int st = 0, en = INT_MAX, of = 0;
  FILE *file;
  struct gcpro gcpro1;
  char line[1025];
  int fondo = specpdl_depth ();
  int ignore_first_column = 0;
  int big5 = 0;

  CHECK_STRING (filename);
  charset = Fget_charset (charset);
  if (!NILP (start))
    {
      CHECK_FIXNUM (start);
      st = XFIXNUM (start);
    }
  if (!NILP (end))
    {
      CHECK_FIXNUM (end);
      en = XFIXNUM (end);
    }
  if (!NILP (offset))
    {
      CHECK_FIXNUM (offset);
      of = XFIXNUM (offset);
    }

  if (!LISTP (flags))
    flags = list1 (flags);

  {
    EXTERNAL_LIST_LOOP_2 (elt, flags)
      {
	if (EQ (elt, Qignore_first_column))
	  ignore_first_column = 1;
	else if (EQ (elt, Qbig5))
	  big5 = 1;
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
      int cp1, cp2, endcount;
      int cp1high, cp1low;
      int dummy;

      while (*p) /* erase all comments out of the line */
	{
	  if (*p == '#')
	    *p = '\0';
	  else
	    p++;
	}
      /* see if line is nothing but whitespace and skip if so */
      p = line + strspn (line, " \t\n\r\f");
      if (!*p)
	continue;
      /* NOTE: It appears that MS Windows and Newlib sscanf() have
	 different interpretations for whitespace (== "skip all whitespace
	 at processing point"): Newlib requires at least one corresponding
	 whitespace character in the input, but MS allows none.  The
	 following would be easier to write if we could count on the MS
	 interpretation.

	 Also, the return value does NOT include %n storage. */
      if ((!ignore_first_column ?
	   sscanf (p, "%i %i%n", &cp1, &cp2, &endcount) < 2 :
	   sscanf (p, "%i %i %i%n", &dummy, &cp1, &cp2, &endcount) < 3)
	  /* #### Temporary code!  Cygwin newlib fucked up scanf() handling
	     of numbers beginning 0x0... starting in 04/2004, in an attempt
	     to fix another bug.  A partial fix for this was put in in
	     06/2004, but as of 10/2004 the value of ENDCOUNT returned in
	     such case is still wrong.  If this gets fixed soon, remove
	     this code. --ben */
#ifndef CYGWIN_SCANF_BUG
	  || *(p + endcount + strspn (p + endcount, " \t\n\r\f"))
#endif
	  )
	{
	  warn_when_safe (Qunicode, Qwarning,
			  "Unrecognized line in translation file %s:\n%s",
			  XSTRING_DATA (filename), line);
	  continue;
	}
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

	  if (big5)
	    {
	      Ichar ch = decode_big5_char (cp1high, cp1low);
	      if (ch == -1)

		warn_when_safe (Qunicode, Qwarning,
				"Out of range Big5 codepoint 0x%x in "
				"translation file %s:\n%s",
				cp1, XSTRING_DATA (filename), line);
	      else
		set_unicode_conversion (ch, cp2);
	    }
	  else
	    {
	      int l1, h1, l2, h2;
	      Ichar emch;

	      switch (XCHARSET_TYPE (charset))
		{
		case CHARSET_TYPE_94: l1 = 33; h1 = 126; l2 = 0; h2 = 0; break;
		case CHARSET_TYPE_96: l1 = 32; h1 = 127; l2 = 0; h2 = 0; break;
		case CHARSET_TYPE_94X94: l1 = 33; h1 = 126; l2 = 33; h2 = 126;
		  break;
		case CHARSET_TYPE_96X96: l1 = 32; h1 = 127; l2 = 32; h2 = 127;
		  break;
		default: ABORT (); l1 = 0; h1 = 0; l2 = 0; h2 = 0;
		}

	      if (cp1high < l2 || cp1high > h2 || cp1low < l1 || cp1low > h1)
		goto out_of_range;

	      emch = (cp1high == 0 ? make_ichar (charset, cp1low, 0) :
		      make_ichar (charset, cp1high, cp1low));
	      set_unicode_conversion (emch, cp2);
	    }
	}
    }

  if (ferror (file))
    report_file_error ("IO error when reading", filename);

  unbind_to (fondo); /* close file */
  UNGCPRO;
  return Qnil;
}

#endif /* MULE */


/************************************************************************/
/*                         Unicode coding system                        */
/************************************************************************/

struct unicode_coding_system
{
  enum unicode_type type;
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

struct unicode_coding_stream
{
  /* decode */
  unsigned char counter;
  unsigned char indicated_length;
  int seen_char;
  Charcount characters_seen;
  /* encode */
  Lisp_Object current_charset;
  int current_char_boundary;
  int wrote_bom;
};

static const struct memory_description unicode_coding_system_description[] = {
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (unicode);

static void
decode_unicode_char (int ch, unsigned_char_dynarr *dst,
		     struct unicode_coding_stream *data,
		     unsigned int ignore_bom)
{
  if (ch == 0xFEFF && !data->seen_char && ignore_bom)
    ;
  else
    {
#ifdef MULE
      Ichar chr = unicode_to_ichar (ch, unicode_precedence_dynarr);

      if (chr != -1)
	{
	  Ibyte work[MAX_ICHAR_LEN];
	  int len;

	  len = set_itext_ichar (work, chr);
	  Dynarr_add_many (dst, work, len);
	}
      else
	{
	  Dynarr_add (dst, LEADING_BYTE_JAPANESE_JISX0208);
	  Dynarr_add (dst, 34 + 128);
	  Dynarr_add (dst, 46 + 128);
	}
#else
      Dynarr_add (dst, (Ibyte) ch);
#endif /* MULE */
    }

  data->seen_char = 1;
}

#define DECODE_ERROR_OCTET(octet, dst, data, ignore_bom) \
  decode_unicode_char ((octet) + UNICODE_ERROR_OCTET_RANGE_START, \
                       dst, data, ignore_bom)

static inline void
indicate_invalid_utf_8 (unsigned char indicated_length,
                        unsigned char counter,
                        int ch, unsigned_char_dynarr *dst,
                        struct unicode_coding_stream *data,
                        unsigned int ignore_bom)
{
  Binbyte stored = indicated_length - counter; 
  Binbyte mask = "\x00\x00\xC0\xE0\xF0\xF8\xFC"[indicated_length];

  while (stored > 0)
    {
      DECODE_ERROR_OCTET (((ch >> (6 * (stored - 1))) & 0x3f) | mask,
                        dst, data, ignore_bom);
      mask = 0x80, stored--;
    }
}

static void
encode_unicode_char_1 (int code, unsigned_char_dynarr *dst,
		       enum unicode_type type, unsigned int little_endian,
                       int write_error_characters_as_such)
{
  switch (type)
    {
    case UNICODE_UTF_16:
      if (little_endian)
	{
	  if (code < 0x10000) {
	    Dynarr_add (dst, (unsigned char) (code & 255));
	    Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
	  } else if (write_error_characters_as_such && 
                     code >= UNICODE_ERROR_OCTET_RANGE_START &&
                     code < (UNICODE_ERROR_OCTET_RANGE_START + 0x100))
            {
              Dynarr_add (dst, (unsigned char) ((code & 0xFF)));
            }
          else if (code < 0x110000)
            {
              /* Little endian; least significant byte first. */
              int first, second;

              CODE_TO_UTF_16_SURROGATES(code, first, second);

              Dynarr_add (dst, (unsigned char) (first & 255));
              Dynarr_add (dst, (unsigned char) ((first >> 8) & 255));

              Dynarr_add (dst, (unsigned char) (second & 255));
              Dynarr_add (dst, (unsigned char) ((second >> 8) & 255));
            }
          else
            {
              /* Not valid Unicode. Pass U+FFFD, least significant byte
                 first. */
              Dynarr_add (dst, (unsigned char) 0xFD);
              Dynarr_add (dst, (unsigned char) 0xFF);
            }
	}
      else
	{
	  if (code < 0x10000) {
	    Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
	    Dynarr_add (dst, (unsigned char) (code & 255));
	  } else if (write_error_characters_as_such && 
                     code >= UNICODE_ERROR_OCTET_RANGE_START &&
                     code < (UNICODE_ERROR_OCTET_RANGE_START + 0x100))
            {
              Dynarr_add (dst, (unsigned char) ((code & 0xFF)));
            }
          else if (code < 0x110000)
            {
              /* Big endian; most significant byte first. */
              int first, second;

              CODE_TO_UTF_16_SURROGATES(code, first, second);

              Dynarr_add (dst, (unsigned char) ((first >> 8) & 255));
              Dynarr_add (dst, (unsigned char) (first & 255));

              Dynarr_add (dst, (unsigned char) ((second >> 8) & 255));
              Dynarr_add (dst, (unsigned char) (second & 255));
            }
          else
            {
              /* Not valid Unicode. Pass U+FFFD, most significant byte
                 first. */
              Dynarr_add (dst, (unsigned char) 0xFF);
              Dynarr_add (dst, (unsigned char) 0xFD);
            }
	}
      break;

    case UNICODE_UCS_4:
    case UNICODE_UTF_32:
      if (little_endian)
	{
          if (write_error_characters_as_such && 
              code >= UNICODE_ERROR_OCTET_RANGE_START &&
              code < (UNICODE_ERROR_OCTET_RANGE_START + 0x100))
            {
              Dynarr_add (dst, (unsigned char) ((code & 0xFF)));
            }
          else
            {
              /* We generate and accept incorrect sequences here, which is
                 okay, in the interest of preservation of the user's
                 data.  */
              Dynarr_add (dst, (unsigned char) (code & 255));
              Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
              Dynarr_add (dst, (unsigned char) ((code >> 16) & 255));
              Dynarr_add (dst, (unsigned char) (code >> 24));
            }
	}
      else
	{
          if (write_error_characters_as_such && 
              code >= UNICODE_ERROR_OCTET_RANGE_START &&
              code < (UNICODE_ERROR_OCTET_RANGE_START + 0x100))
            {
              Dynarr_add (dst, (unsigned char) ((code & 0xFF)));
            }
          else
            {
              /* We generate and accept incorrect sequences here, which is okay,
                 in the interest of preservation of the user's data.  */
              Dynarr_add (dst, (unsigned char) (code >> 24));
              Dynarr_add (dst, (unsigned char) ((code >> 16) & 255));
              Dynarr_add (dst, (unsigned char) ((code >> 8) & 255));
              Dynarr_add (dst, (unsigned char) (code & 255));
            }
	}
      break;

    case UNICODE_UTF_8:
      if (code <= 0x7f)
	{
	  Dynarr_add (dst, (unsigned char) code);
	}
      else if (code <= 0x7ff)
	{
	  Dynarr_add (dst, (unsigned char) ((code >> 6) | 0xc0));
	  Dynarr_add (dst, (unsigned char) ((code & 0x3f) | 0x80));
	}
      else if (code <= 0xffff)
	{
	  Dynarr_add (dst, (unsigned char) ((code >> 12) | 0xe0));
	  Dynarr_add (dst, (unsigned char) (((code >>  6) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) ((code        & 0x3f) | 0x80));
	}
      else if (code <= 0x1fffff)
	{
	  Dynarr_add (dst, (unsigned char) ((code >> 18) | 0xf0));
	  Dynarr_add (dst, (unsigned char) (((code >> 12) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) (((code >>  6) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) ((code        & 0x3f) | 0x80));
	}
      else if (code <= 0x3ffffff)
	{

#if !(UNICODE_ERROR_OCTET_RANGE_START > 0x1fffff \
          && UNICODE_ERROR_OCTET_RANGE_START < 0x3ffffff)
#error "This code needs to be rewritten. " 
#endif
          if (write_error_characters_as_such && 
              code >= UNICODE_ERROR_OCTET_RANGE_START &&
              code < (UNICODE_ERROR_OCTET_RANGE_START + 0x100))
            {
              Dynarr_add (dst, (unsigned char) ((code & 0xFF)));
            }
          else 
            {
              Dynarr_add (dst, (unsigned char) ((code >> 24) | 0xf8));
              Dynarr_add (dst, (unsigned char) (((code >> 18) & 0x3f) | 0x80));
              Dynarr_add (dst, (unsigned char) (((code >> 12) & 0x3f) | 0x80));
              Dynarr_add (dst, (unsigned char) (((code >>  6) & 0x3f) | 0x80));
              Dynarr_add (dst, (unsigned char) ((code        & 0x3f) | 0x80));
            }
	}
      else
	{
	  Dynarr_add (dst, (unsigned char) ((code >> 30) | 0xfc));
	  Dynarr_add (dst, (unsigned char) (((code >> 24) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) (((code >> 18) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) (((code >> 12) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) (((code >>  6) & 0x3f) | 0x80));
	  Dynarr_add (dst, (unsigned char) ((code        & 0x3f) | 0x80));
	}
      break;

    case UNICODE_UTF_7: ABORT ();

    default: ABORT ();
    }
}

/* Also used in mule-coding.c for UTF-8 handling in ISO 2022-oriented
   encodings. */
void
encode_unicode_char (Lisp_Object USED_IF_MULE (charset), int h,
		     int USED_IF_MULE (l), unsigned_char_dynarr *dst,
		     enum unicode_type type, unsigned int little_endian,
                     int write_error_characters_as_such)
{
#ifdef MULE
  int code = ichar_to_unicode (make_ichar (charset, h & 127, l & 127));

  if (code == -1)
    {
      code = 0xFFFD;
    }
#else
  int code = h;
#endif /* MULE */

  encode_unicode_char_1 (code, dst, type, little_endian, 
                         write_error_characters_as_such);
}

static Charcount
unicode_character_tell (struct coding_stream *str)
{
  if (CODING_STREAM_TYPE_DATA (str, unicode)->counter == 0)
    {
      return CODING_STREAM_TYPE_DATA (str, unicode)->characters_seen;
    }

  return -1;
}

static Bytecount
unicode_convert (struct coding_stream *str, const UExtbyte *src,
		 unsigned_char_dynarr *dst, Bytecount n)
{
  unsigned int ch    = str->ch;
  struct unicode_coding_stream *data = CODING_STREAM_TYPE_DATA (str, unicode);
  enum unicode_type type =
    XCODING_SYSTEM_UNICODE_TYPE (str->codesys);
  unsigned int little_endian =
    XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (str->codesys);
  unsigned int ignore_bom = XCODING_SYSTEM_UNICODE_NEED_BOM (str->codesys);
  Bytecount orign = n;

  if (str->direction == CODING_DECODE)
    {
      unsigned char counter = data->counter;
      unsigned char indicated_length
        = data->indicated_length;
      Charcount characters_seen = data->characters_seen;

      while (n--)
	{
	  UExtbyte c = *src++;

	  switch (type)
	    {
	    case UNICODE_UTF_8:
              if (0 == counter)
                {
                  if (0 == (c & 0x80))
                    {
                      /* ASCII. */
                      decode_unicode_char (c, dst, data, ignore_bom);
                      characters_seen++;
                    }
                  else if (0 == (c & 0x40))
                    {
                      /* Highest bit set, second highest not--there's
                         something wrong. */
                      DECODE_ERROR_OCTET (c, dst, data, ignore_bom);
                      /* This is a character in the buffer. */
                      characters_seen++;
                    }
                  else if (0 == (c & 0x20))
                    {
                      ch = c & 0x1f; 
                      counter = 1;
                      indicated_length = 2;
                    }
                  else if (0 == (c & 0x10))
                    {
                      ch = c & 0x0f;
                      counter = 2;
                      indicated_length = 3;
                    }
                  else if (0 == (c & 0x08))
                    {
                      ch = c & 0x0f;
                      counter = 3;
                      indicated_length = 4;
                    }
                  else
                    {
                      /* We don't supports lengths longer than 4 in
                         external-format data. */
                      DECODE_ERROR_OCTET (c, dst, data, ignore_bom);
                      characters_seen++;
                    }
                }
              else
                {
                  /* counter != 0 */
                  if ((0 == (c & 0x80)) || (0 != (c & 0x40)))
                    {
                      indicate_invalid_utf_8(indicated_length, 
                                             counter, 
                                             ch, dst, data, ignore_bom);
                      /* These are characters our receiver will see, not
                         actual characters we've seen in the input. */
                      characters_seen += (indicated_length - counter);
                      if (c & 0x80)
                        {
                          DECODE_ERROR_OCTET (c, dst, data, ignore_bom);
                          characters_seen++;
                        }
                      else
                        {
                          /* The character just read is ASCII. Treat it as
                             such.  */
                          decode_unicode_char (c, dst, data, ignore_bom);
                          characters_seen++;
                        }
                      ch = 0;
                      counter = 0;
                    }
                  else 
                    {
                      ch = (ch << 6) | (c & 0x3f);
                      counter--;
                      /* Just processed the final byte. Emit the character. */
                      if (!counter)
                        {
			  /* Don't accept over-long sequences, surrogates,
                             or codes above #x10FFFF. */
                          if ((ch < 0x80) ||
                              ((ch < 0x800) && indicated_length > 2) || 
                              ((ch < 0x10000) && indicated_length > 3) || 
                              valid_utf_16_surrogate(ch) || (ch > 0x110000))
                            {
                              indicate_invalid_utf_8(indicated_length, 
                                                     counter, 
                                                     ch, dst, data,
                                                     ignore_bom);
                              characters_seen += (indicated_length - counter);
                            }
                          else
                            {
                              decode_unicode_char (ch, dst, data, ignore_bom);
                              characters_seen++;
                            }
                          ch = 0;
                        }
                    }
		}
	      break;

	    case UNICODE_UTF_16:

	      if (little_endian)
		ch = (c << counter) | ch;
	      else
		ch = (ch << 8) | c;

	      counter += 8;

	      if (16 == counter)
                {
		  int tempch = ch;

                  if (valid_utf_16_first_surrogate(ch))
                    {
                      break;
                    }
		  ch = 0;
		  counter = 0;
		  decode_unicode_char (tempch, dst, data, ignore_bom);
		}
	      else if (32 == counter)
		{
		  int tempch;

                  if (little_endian)
                    {
                      if (!valid_utf_16_last_surrogate(ch >> 16))
                        {
                          DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst, data,
                                              ignore_bom);
                        }
                      else
                        {
                          tempch = utf_16_surrogates_to_code((ch & 0xffff),
                                                             (ch >> 16));
                          decode_unicode_char(tempch, dst, data, ignore_bom); 
                        }
                    }
                  else
                    {
                      if (!valid_utf_16_last_surrogate(ch & 0xFFFF))
                        {
                          DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                              ignore_bom);
                          DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
                                              ignore_bom);
                        }
                      else 
                        {
                          tempch = utf_16_surrogates_to_code((ch >> 16), 
                                                             (ch & 0xffff));
                          decode_unicode_char(tempch, dst, data, ignore_bom); 
                        }
                    }

		  ch = 0;
		  counter = 0;
                }
              else
                assert(8 == counter || 24 == counter);
	      break;

	    case UNICODE_UCS_4:
            case UNICODE_UTF_32:
	      if (little_endian)
		ch = (c << counter) | ch;
	      else
		ch = (ch << 8) | c;
	      counter += 8;
	      if (counter == 32)
		{
		  if (ch > 0x10ffff)
		    {
                      /* ch is not a legal Unicode character. We're fine
                         with that in UCS-4, though not in UTF-32. */
                      if (UNICODE_UCS_4 == type && ch < 0x80000000)
                        {
                          decode_unicode_char (ch, dst, data, ignore_bom);
                        }
                      else if (little_endian)
                        {
                          DECODE_ERROR_OCTET (ch & 0xFF, dst, data, 
                                            ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                            ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                            ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst, data,
                                            ignore_bom);
                        }
                      else
                        {
                          DECODE_ERROR_OCTET ((ch >> 24) & 0xFF, dst, data,
                                            ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                            ignore_bom);
                          DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                            ignore_bom);
                          DECODE_ERROR_OCTET (ch & 0xFF, dst, data, 
                                            ignore_bom);
                        }
		    }
                  else
                    {
                      decode_unicode_char (ch, dst, data, ignore_bom);
                    }
		  ch = 0;
		  counter = 0;
		}
	      break;

	    case UNICODE_UTF_7:
	      ABORT ();
	      break;

	    default: ABORT ();
	    }

	}

      if (str->eof && counter)
        {
          switch (type)
            {
	    case UNICODE_UTF_8:
              indicate_invalid_utf_8(indicated_length, 
                                     counter, ch, dst, data, 
                                     ignore_bom);
              characters_seen += (indicated_length - counter);
              break;

            case UNICODE_UTF_16:
            case UNICODE_UCS_4:
            case UNICODE_UTF_32:
              if (8 == counter)
                {
                  DECODE_ERROR_OCTET (ch, dst, data, ignore_bom);
                }
              else if (16 == counter)
                {
                  if (little_endian)
                    {
                      DECODE_ERROR_OCTET (ch & 0xFF, dst, data, ignore_bom); 
                      DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                          ignore_bom); 
                    }
                  else
                    {
                      DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                          ignore_bom); 
                      DECODE_ERROR_OCTET (ch & 0xFF, dst, data, ignore_bom); 
                    }
                }
              else if (24 == counter)
                {
                  if (little_endian)
                    {
                      DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                          ignore_bom);
                      DECODE_ERROR_OCTET (ch & 0xFF, dst, data, ignore_bom); 
                      DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                          ignore_bom); 
                    }
                  else
                    {
                      DECODE_ERROR_OCTET ((ch >> 16) & 0xFF, dst, data,
                                          ignore_bom);
                      DECODE_ERROR_OCTET ((ch >> 8) & 0xFF, dst, data,
                                          ignore_bom); 
                      DECODE_ERROR_OCTET (ch & 0xFF, dst, data,
                                          ignore_bom); 
                    }
                }
              else assert(0);
              break;
            }
          ch = 0;
          counter = 0;
        }

      data->counter = counter;
      data->indicated_length = indicated_length;
      data->characters_seen = characters_seen;
    }
  else
    {
      unsigned char char_boundary = data->current_char_boundary;
      Lisp_Object charset = data->current_charset;

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
	  encode_unicode_char_1 (0xFEFF, dst, type, little_endian, 1);
	  data->wrote_bom = 1;
	}

      while (n--)
	{
	  Ibyte c = *src++;

#ifdef MULE
	  if (byte_ascii_p (c))
#endif /* MULE */
	    {			/* Processing ASCII character */
	      ch = 0;
	      encode_unicode_char (Vcharset_ascii, c, 0, dst, type,
				   little_endian, 1);

	      char_boundary = 1;
	    }
#ifdef MULE
	  else if (ibyte_leading_byte_p (c) || ibyte_leading_byte_p (ch))
	    {			/* Processing Leading Byte */
	      ch = 0;
	      charset = charset_by_leading_byte (c);
	      if (leading_byte_prefix_p(c))
		ch = c;
	      char_boundary = 0;
	    }
	  else
	    {			/* Processing Non-ASCII character */
	      char_boundary = 1;
	      if (EQ (charset, Vcharset_control_1))
		/* See:

		   (Info-goto-node "(internals)Internal String Encoding")

		   for the rationale behind subtracting #xa0 from the
		   character's code. */
		encode_unicode_char (Vcharset_control_1, c - 0xa0, 0, dst,
				     type, little_endian, 1);
	      else
		{
		  switch (XCHARSET_REP_BYTES (charset))
		    {
		    case 2:
		      encode_unicode_char (charset, c, 0, dst, type,
					   little_endian, 1);
		      break;
		    case 3:
		      if (XCHARSET_PRIVATE_P (charset))
			{
			  encode_unicode_char (charset, c, 0, dst, type,
					       little_endian, 1);
			  ch = 0;
			}
		      else if (ch)
			{
#ifdef ENABLE_COMPOSITE_CHARS
			  if (EQ (charset, Vcharset_composite))
			    {
			      if (in_composite)
				{
				  /* #### Bother! We don't know how to
				     handle this yet. */
				  encode_unicode_char (Vcharset_ascii, '~', 0,
						       dst, type,
						       little_endian, 1);
				}
			      else
				{
				  Ichar emch = make_ichar (Vcharset_composite,
							   ch & 0x7F,
							   c & 0x7F);
				  Lisp_Object lstr =
				    composite_char_string (emch);
				  saved_n = n;
				  saved_src = src;
				  in_composite = 1;
				  src = XSTRING_DATA   (lstr);
				  n   = XSTRING_LENGTH (lstr);
				}
			    }
			  else
#endif /* ENABLE_COMPOSITE_CHARS */
			    encode_unicode_char (charset, ch, c, dst, type,
						 little_endian, 1);
			  ch = 0;
			}
		      else
			{
			  ch = c;
			  char_boundary = 0;
			}
		      break;
		    case 4:
		      if (ch)
			{
			  encode_unicode_char (charset, ch, c, dst, type,
					       little_endian, 1);
			  ch = 0;
			}
		      else
			{
			  ch = c;
			  char_boundary = 0;
			}
		      break;
		    default:
		      ABORT ();
		    }
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

      data->current_char_boundary = char_boundary;
      data->current_charset = charset;

      /* La palabra se hizo carne! */
      /* A palavra fez-se carne! */
      /* Whatever. */
    }

  str->ch    = ch;
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
  data->current_charset = Qnil;
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
      enum unicode_type type;

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
  write_fmt_string_lisp (printcharfun, "(%s",
                         unicode_getprop (cs, Qunicode_type));
  if (XCODING_SYSTEM_UNICODE_LITTLE_ENDIAN (cs))
    write_ascstring (printcharfun, ", little-endian");
  if (XCODING_SYSTEM_UNICODE_NEED_BOM (cs))
    write_ascstring (printcharfun, ", need-bom");
  write_ascstring (printcharfun, ")");
}

#ifdef MULE
DEFUN ("set-unicode-query-skip-chars-args", Fset_unicode_query_skip_chars_args,
       3, 3, 0, /*
Specify strings as matching characters known to Unicode coding systems.

QUERY-STRING is a string matching characters that can unequivocally be
encoded by the Unicode coding systems.

INVALID-STRING is a string to match XEmacs characters that represent known
octets on disk, but that are invalid sequences according to Unicode. 

UTF-8-INVALID-STRING is a more restrictive string to match XEmacs characters
that are invalid UTF-8 octets.

All three strings are in the format accepted by `skip-chars-forward'. 
*/
       (query_string, invalid_string, utf_8_invalid_string))
{
  CHECK_STRING (query_string);
  CHECK_STRING (invalid_string);
  CHECK_STRING (utf_8_invalid_string);

  Vunicode_query_string = query_string;
  Vunicode_invalid_string = invalid_string;
  Vutf_8_invalid_string = utf_8_invalid_string;

  return Qnil;
}

static void
add_lisp_string_to_skip_chars_range (Lisp_Object string, Lisp_Object rtab,
                                     Lisp_Object value)
{
  Ibyte *p, *pend;
  Ichar c;

  p = XSTRING_DATA (string);
  pend = p + XSTRING_LENGTH (string);

  while (p != pend)
    {
      c = itext_ichar (p);

      INC_IBYTEPTR (p);

      if (c == '\\')
        {
          if (p == pend) break;
          c = itext_ichar (p);
          INC_IBYTEPTR (p);
        }

      if (p != pend && *p == '-')
        {
          Ichar cend;

          /* Skip over the dash.  */
          p++;
          if (p == pend) break;
          cend = itext_ichar (p);

          Fput_range_table (make_fixnum (c), make_fixnum (cend), value,
                            rtab);

          INC_IBYTEPTR (p);
        }
      else
        {
          Fput_range_table (make_fixnum (c), make_fixnum (c), value, rtab);
        }
    }
}

/* This function wouldn't be necessary if initialised range tables were
   dumped properly; see
   http://mid.gmane.org/18179.49815.622843.336527@parhasard.net . */
static void
initialize_unicode_query_range_tables_from_strings (void)
{
  CHECK_STRING (Vunicode_query_string);
  CHECK_STRING (Vunicode_invalid_string);
  CHECK_STRING (Vutf_8_invalid_string);

  Vunicode_query_skip_chars = Fmake_range_table (Qstart_closed_end_closed);

  add_lisp_string_to_skip_chars_range (Vunicode_query_string,
                                       Vunicode_query_skip_chars,
                                       Qsucceeded);

  Vunicode_invalid_and_query_skip_chars
    = Fcopy_range_table (Vunicode_query_skip_chars);

  add_lisp_string_to_skip_chars_range (Vunicode_invalid_string,
                                       Vunicode_invalid_and_query_skip_chars,
                                       Qinvalid_sequence);

  Vutf_8_invalid_and_query_skip_chars
    = Fcopy_range_table (Vunicode_query_skip_chars);

  add_lisp_string_to_skip_chars_range (Vutf_8_invalid_string,
                                       Vutf_8_invalid_and_query_skip_chars, 
                                       Qinvalid_sequence);
}

static Lisp_Object
unicode_query (Lisp_Object codesys, struct buffer *buf, Charbpos end,
               int flags)
{
  Charbpos pos = BUF_PT (buf), fail_range_start, fail_range_end;
  Charbpos pos_byte = BYTE_BUF_PT (buf);
  Lisp_Object skip_chars_range_table, result = Qnil;
  enum query_coding_failure_reasons failed_reason,
    previous_failed_reason = query_coding_succeeded;
  int checked_unicode,
    invalid_lower_limit = UNICODE_ERROR_OCTET_RANGE_START,
    invalid_upper_limit = -1,
    unicode_type = XCODING_SYSTEM_UNICODE_TYPE (codesys);

  if (flags & QUERY_METHOD_HIGHLIGHT && 
      /* If we're being called really early, live without highlights getting
         cleared properly: */
      !(UNBOUNDP (XSYMBOL (Qquery_coding_clear_highlights)->function)))
    {
      /* It's okay to call Lisp here, the only non-stack object we may have
         allocated up to this point is skip_chars_range_table, and that's
         reachable from its entry in Vfixed_width_query_ranges_cache. */
      call3 (Qquery_coding_clear_highlights, make_fixnum (pos), make_fixnum (end),
             wrap_buffer (buf));
    }

  if (NILP (Vunicode_query_skip_chars))
    {
      initialize_unicode_query_range_tables_from_strings();
    }

  if (flags & QUERY_METHOD_IGNORE_INVALID_SEQUENCES)
    {
      switch (unicode_type)
        {
        case UNICODE_UTF_8:
          skip_chars_range_table = Vutf_8_invalid_and_query_skip_chars;
          break;
        case UNICODE_UTF_7:
          /* #### See above. */
          return Qunbound;
          break;
        default:
          skip_chars_range_table = Vunicode_invalid_and_query_skip_chars;
          break;
        }
    }
  else
    {
      switch (unicode_type)
        {
        case UNICODE_UTF_8:
          invalid_lower_limit = UNICODE_ERROR_OCTET_RANGE_START + 0x80;
          invalid_upper_limit = UNICODE_ERROR_OCTET_RANGE_START + 0xFF;
          break;
        case UNICODE_UTF_7:
          /* #### Work out what to do here in reality, read the spec and decide
             which octets are invalid. */
          return Qunbound;
          break;
        default:
          invalid_lower_limit = UNICODE_ERROR_OCTET_RANGE_START;
          invalid_upper_limit = UNICODE_ERROR_OCTET_RANGE_START + 0xFF;
          break;
        }

      skip_chars_range_table = Vunicode_query_skip_chars;
    }

  while (pos < end)
    {
      Ichar ch = BYTE_BUF_FETCH_CHAR (buf, pos_byte);
      if ((ch < 0x100 ? 1 : 
           (!EQ (Qnil, Fget_range_table (make_fixnum (ch), skip_chars_range_table,
                                         Qnil)))))
        {
          pos++;
          INC_BYTEBPOS (buf, pos_byte);
        }
      else
        {
          fail_range_start = pos;
          while ((pos < end) &&  
                 ((checked_unicode = ichar_to_unicode (ch),
                   -1 == checked_unicode
                   && (failed_reason = query_coding_unencodable))
                  || (!(flags & QUERY_METHOD_IGNORE_INVALID_SEQUENCES) &&
                      (invalid_lower_limit <= checked_unicode) &&
                      (checked_unicode <= invalid_upper_limit)
                      && (failed_reason = query_coding_invalid_sequence)))
                 && (previous_failed_reason == query_coding_succeeded
                     || previous_failed_reason == failed_reason))
            {
              pos++;
              INC_BYTEBPOS (buf, pos_byte);
              ch = BYTE_BUF_FETCH_CHAR (buf, pos_byte);
              previous_failed_reason = failed_reason;
            }

          if (fail_range_start == pos)
            {
              /* The character can actually be encoded; move on. */
              pos++;
              INC_BYTEBPOS (buf, pos_byte);
            }
          else
            {
              assert (previous_failed_reason == query_coding_invalid_sequence
                      || previous_failed_reason == query_coding_unencodable);

              if (flags & QUERY_METHOD_ERRORP)
                {
                  signal_error_2
		    (Qtext_conversion_error,
		     "Cannot encode using coding system",
		     make_string_from_buffer (buf, fail_range_start,
					      pos - fail_range_start),
		     XCODING_SYSTEM_NAME (codesys));
                }

              if (NILP (result))
                {
                  result = Fmake_range_table (Qstart_closed_end_open);
                }

              fail_range_end = pos;

              Fput_range_table (make_fixnum (fail_range_start), 
                                make_fixnum (fail_range_end),
                                (previous_failed_reason
                                 == query_coding_unencodable ?
                                 Qunencodable : Qinvalid_sequence), 
                                result);
              previous_failed_reason = query_coding_succeeded;

              if (flags & QUERY_METHOD_HIGHLIGHT) 
                {
                  Lisp_Object extent
                    = Fmake_extent (make_fixnum (fail_range_start),
                                    make_fixnum (fail_range_end), 
                                    wrap_buffer (buf));
                  
                  Fset_extent_priority
                    (extent, make_fixnum (2 + mouse_highlight_priority));
                  Fset_extent_face (extent, Qquery_coding_warning_face);
                }
            }
        }
    }

  return result;
}
#else /* !MULE */
static Lisp_Object
unicode_query (Lisp_Object UNUSED (codesys),
               struct buffer * UNUSED (buf),
               Charbpos UNUSED (end), int UNUSED (flags))
{
  return Qnil;
}
#endif

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

void
syms_of_unicode (void)
{
#ifdef MULE
  DEFSUBR (Funicode_precedence_list);
  DEFSUBR (Fset_language_unicode_precedence_list);
  DEFSUBR (Flanguage_unicode_precedence_list);
  DEFSUBR (Fset_default_unicode_precedence_list);
  DEFSUBR (Fdefault_unicode_precedence_list);
  DEFSUBR (Fset_unicode_conversion);

  DEFSUBR (Fload_unicode_mapping_table);

  DEFSUBR (Fset_unicode_query_skip_chars_args);

  DEFSYMBOL (Qccl_encode_to_ucs_2);
  DEFSYMBOL (Qlast_allocated_character);
  DEFSYMBOL (Qignore_first_column);

  DEFSYMBOL (Qunicode_registries);
#endif /* MULE */

  DEFSUBR (Fchar_to_unicode);
  DEFSUBR (Funicode_to_char);

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
  CODING_SYSTEM_HAS_METHOD (unicode, query);
  CODING_SYSTEM_HAS_METHOD (unicode, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (unicode, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (unicode, putprop);
  CODING_SYSTEM_HAS_METHOD (unicode, getprop);

  CODING_SYSTEM_HAS_METHOD (unicode, character_tell);

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
  staticpro (&Vnumber_of_jit_charsets);
  Vnumber_of_jit_charsets = make_fixnum (0);
  staticpro (&Vlast_jit_charset_final);
  Vlast_jit_charset_final = make_char (0x30);
  staticpro (&Vcharset_descr);
  Vcharset_descr
    = build_defer_string ("Mule charset for otherwise unknown Unicode code points.");

  staticpro (&Vlanguage_unicode_precedence_list);
  Vlanguage_unicode_precedence_list = Qnil;

  staticpro (&Vdefault_unicode_precedence_list);
  Vdefault_unicode_precedence_list = Qnil;

  unicode_precedence_dynarr = Dynarr_new (Lisp_Object);
  dump_add_root_block_ptr (&unicode_precedence_dynarr,
			    &lisp_object_dynarr_description);

  
  
  init_blank_unicode_tables ();

  staticpro (&Vcurrent_jit_charset);
  Vcurrent_jit_charset = Qnil;

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

  dump_add_root_block (&from_unicode_blank_1, sizeof (void *),
		       from_unicode_level_1_desc_1);
  dump_add_root_block (&from_unicode_blank_2, sizeof (void *),
		       from_unicode_level_2_desc_1);
  dump_add_root_block (&from_unicode_blank_3, sizeof (void *),
		       from_unicode_level_3_desc_1);
  dump_add_root_block (&from_unicode_blank_4, sizeof (void *),
		       from_unicode_level_4_desc_1);

  DEFVAR_LISP ("unicode-registries", &Qunicode_registries /*
Vector describing the X11 registries searched when using fallback fonts.

"Fallback fonts" here includes by default those fonts used by redisplay when
displaying charsets for which the `encode-as-utf-8' property is true, and
those used when no font matching the charset's registries property has been
found (that is, they're probably Mule-specific charsets like Ethiopic or
IPA.)
*/ );
  Qunicode_registries = vector1(build_ascstring("iso10646-1"));

  /* Initialised in lisp/mule/general-late.el, by a call to
     #'set-unicode-query-skip-chars-args. Or at least they would be, but we
     can't do this at dump time right now, initialised range tables aren't
     dumped properly. */
  staticpro (&Vunicode_invalid_and_query_skip_chars);
  Vunicode_invalid_and_query_skip_chars = Qnil;
  staticpro (&Vutf_8_invalid_and_query_skip_chars);
  Vutf_8_invalid_and_query_skip_chars = Qnil;
  staticpro (&Vunicode_query_skip_chars);
  Vunicode_query_skip_chars = Qnil;

  /* If we could dump the range table above these wouldn't be necessary: */
  staticpro (&Vunicode_query_string);
  Vunicode_query_string = Qnil;
  staticpro (&Vunicode_invalid_string);
  Vunicode_invalid_string = Qnil;
  staticpro (&Vutf_8_invalid_string);
  Vutf_8_invalid_string = Qnil;
#endif /* MULE */
}

void
complex_vars_of_unicode (void)
{
  /* We used to define this in unicode.el.  But we need it early for
     Cygwin 1.7 -- used in LOCAL_FILE_FORMAT_TO_TSTR() et al. */
  Fmake_coding_system_internal
    (Qutf_8, Qunicode,
     build_defer_string ("UTF-8"),
     listu (Qdocumentation,
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
            Qmnemonic, build_ascstring ("UTF8"),
            Qunicode_type, Qutf_8,
            Qunbound));
}
