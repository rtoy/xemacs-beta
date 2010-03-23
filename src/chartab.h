/* Declarations having to do with Mule char tables.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003, 2005, 2010 Ben Wing.

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

#ifndef INCLUDED_chartab_h_
#define INCLUDED_chartab_h_

#include "charset.h"

/************************************************************************/
/*                            Basic Char Table Format                   */
/************************************************************************/

/* Things are written this way because at one point I designed the
   subtables so they could either be stored as "plain tables" (as direct
   256-element arrays), as unified Lisp objects (where the header and
   following array is a single unit) or as split Lisp object (with a
   wrapper Lisp object around a separately allocated table).  The plain
   tables are the fastest and most memory efficient of the three, but
   can't be used with KKCC. (KKCC doesn't keep track of
   whether it has already traversed non-Lisp-object arrays, and thus
   traverses the shared "blank" subtables numerous times when marking,
   making it become *extremely* slow.) */

struct Lisp_Char_Subtable
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object ptr[256];
};

/* Definition of the non-level-1 subtables, which are always `char subtables'
   whether or not we have a category table or other char table. */

#define ALLOCATE_LEVEL_N_SUBTAB() ALLOC_NORMAL_LISP_OBJECT (char_subtable)
#define SUBTAB_STORAGE_SIZE(table, level, stats)			\
  lisp_object_storage_size (table, stats)
#define FREE_ONE_SUBTAB(table) free_normal_lisp_object (table)

/* If we use split Lisp char subtables, we'd modify the above struct and
   three defines (ALLOCATE_LEVEL_N_SUBTAB, SUBTAB_STORAGE_SIZE,
   FREE_ONE_SUBTAB).  If we use "plain" non-Lisp char subtables, we'd
   modify the three macros above and the various macros below as well, and
   omit the definition of a Lisp subtable object. */

#define SUBTAB_EQ(a, b) EQ (a, b)
#define SUBTAB_TYPE Lisp_Object
#define SUBTAB_ARRAY_TYPE SUBTAB_TYPE *
#define SUBTAB_ARRAY_FROM_SUBTAB(tab) (XCHAR_SUBTABLE (tab)->ptr)

/* Definition of the level-1 subtables, which are either `char subtables'
   or `category subtables'. */

#ifdef MULE
#define ALLOCATE_LEVEL_1_SUBTAB(catp)					\
  ((catp) ? ALLOCATE_CATEGORY_SUBTABLE () : ALLOCATE_LEVEL_N_SUBTAB ())
#define BASE_TYPE_ARRAY_FROM_SUBTAB(tab, catp)			\
  (catp ? (void *) BASE_TYPE_ARRAY_FROM_CATEGORY_SUBTAB (tab) :	\
          (void *) BASE_TYPE_ARRAY_FROM_CHAR_SUBTAB (tab))
#define SUBTAB_BLANK(catp) (catp ? category_chartab_blank : chartab_blank)
#else
#define ALLOCATE_LEVEL_1_SUBTAB(catp)		\
  (assert (!catp), ALLOCATE_LEVEL_N_SUBTAB ())
#define BASE_TYPE_ARRAY_FROM_SUBTAB(tab, catp)	\
  (assert (!catp), BASE_TYPE_ARRAY_FROM_CHAR_SUBTAB (tab))
#define SUBTAB_BLANK(catp) (assert (!catp), chartab_blank)
#endif /* (not) MULE */

/* Specialization of above code to level-1 char subtables. */

#define CHARTAB_BASE_TYPE Lisp_Object
#define BASE_TYPE_ARRAY_FROM_CHAR_SUBTAB(tab) (XCHAR_SUBTABLE (tab)->ptr)

typedef struct Lisp_Char_Subtable Lisp_Char_Subtable;

DECLARE_LISP_OBJECT (char_subtable, Lisp_Char_Subtable);
#define XCHAR_SUBTABLE(x) XRECORD (x, char_subtable, Lisp_Char_Subtable)
#define wrap_char_subtable(p) wrap_record (p, char_subtable)
#define CHAR_SUBTABLEP(x) RECORDP (x, char_subtable)
#define CHECK_CHAR_SUBTABLE(x) CHECK_RECORD (x, char_subtable)
#define CONCHECK_CHAR_SUBTABLE(x) CONCHECK_RECORD (x, char_subtable)

/************************************************************************/
/*                               Char Tables                            */
/************************************************************************/

#ifndef MULE
#define MAXIMIZE_CHAR_TABLE_DEPTH
#endif

/* Break up a 32-bit character code into 8-bit parts. */

#ifdef MAXIMIZE_CHAR_TABLE_DEPTH
# define CHARTAB_BREAKUP_CHAR_CODE(val, u1, u2, u3, u4, levels)	\
do {								\
  int buc_val = (val);						\
								\
  (u1) = buc_val >> 24;						\
  (u2) = (buc_val >> 16) & 255;					\
  (u3) = (buc_val >> 8) & 255;					\
  (u4) = buc_val & 255;						\
} while (0)
/* Define the current chartab levels given an expr indicating the level value.
   This is an optimization designed to cause compiler simplfication of code
   due to constant expression in if, switch, etc. statements. */
# ifdef MULE
#  define CHARTAB_LEVELS(expr) 4
# else
#  define CHARTAB_LEVELS(expr) 1
# endif
#else /* not MAXIMIZE_CHAR_TABLE_DEPTH */
# define CHARTAB_BREAKUP_CHAR_CODE(val, u1, u2, u3, u4, levels)	\
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
# define CHARTAB_LEVELS(expr) (expr)
#endif /* not MAXIMIZE_CHAR_TABLE_DEPTH */

enum char_table_type
{
  CHAR_TABLE_TYPE_GENERIC,
#ifdef MULE
  CHAR_TABLE_TYPE_CATEGORY,
#endif
  CHAR_TABLE_TYPE_SYNTAX,
  CHAR_TABLE_TYPE_DISPLAY,
  CHAR_TABLE_TYPE_CHAR
};

struct Lisp_Char_Table
{
  NORMAL_LISP_OBJECT_HEADER header;

  /* Currently we use the same structure as for the Unicode->charset
     translation tables in unicode.c.  This is extremely fast (constant-
     time lookup) but a potential space hog, especially in the presence of
     sparse, non-localized data.  Alternative representations could use
     hash tables or sorted gap arrays (see extents.c; all the code is
     already there, including the binary-search algorithm to do lookups).
     Possibly, we could/should allow the type to be chosen at creation
     time as a parameter to `make-char-table'. */

  SUBTAB_TYPE table;
  int levels;
  Lisp_Object default_;
  Lisp_Object parent; /* #### not yet implemented */
  
  enum char_table_type type;

  Lisp_Object next_table; /* DO NOT mark through this. */
#ifdef MIRROR_TABLE
  /* stuff used for syntax tables */
  Lisp_Object mirror_table; /* points to mirror table for this table
			       (a cache for quicker access), or a back
			       pointer if MIRROR_TABLE_P. */
  char dirty; /* nonzero if mirror dirty and needs updating. */
  char mirror_table_p; /* nonzero if this is a mirror table. */
#endif /* MIRROR_TABLE */
};
typedef struct Lisp_Char_Table Lisp_Char_Table;

DECLARE_LISP_OBJECT (char_table, Lisp_Char_Table);
#define XCHAR_TABLE(x) XRECORD (x, char_table, Lisp_Char_Table)
#define wrap_char_table(p) wrap_record (p, char_table)
#define CHAR_TABLEP(x) RECORDP (x, char_table)
#define CHECK_CHAR_TABLE(x) CHECK_RECORD (x, char_table)
#define CONCHECK_CHAR_TABLE(x) CONCHECK_RECORD (x, char_table)

/* Note, there is no speed gain whatsoever from dereferencing XCHAR_TABLE()
   once into a temporary variable and then using it, as compared with just
   repeatedly using with XCHAR_TABLE_FOO macros, at least in a production
   build (no-error checking, optimization).  Without error-checking,
   XCHAR_TABLE() is merely a cast to (foo *), which is a no-op. */

#define XCHAR_TABLE_TABLE(ct) (XCHAR_TABLE (ct)->table)
#define XCHAR_TABLE_LEVELS(ct) (XCHAR_TABLE (ct)->levels)
#define XCHAR_TABLE_DEFAULT(ct) (XCHAR_TABLE (ct)->default_)
#define XCHAR_TABLE_PARENT(ct) (XCHAR_TABLE (ct)->parent)

#define CHAR_TABLE_TYPE(ct) ((ct)->type)
#define XCHAR_TABLE_TYPE(ct) (XCHAR_TABLE (ct)->type)

#ifdef MULE
#define CHAR_TABLE_CATEGORY_P(ct) \
  (CHAR_TABLE_TYPE (ct) == CHAR_TABLE_TYPE_CATEGORY)
#else
#define CHAR_TABLE_CATEGORY_P(ct) ((void) ct, 0)
#endif /* (not) MULE */
#define XCHAR_TABLE_CATEGORY_P(ct) CHAR_TABLE_CATEGORY_P (XCHAR_TABLE (ct))

#define XCHAR_TABLE_NEXT_TABLE(ct) (XCHAR_TABLE (ct)->next_table)
#ifdef MIRROR_TABLE
#define XCHAR_TABLE_MIRROR_TABLE(ct) (XCHAR_TABLE (ct)->mirror_table)
#define XCHAR_TABLE_DIRTY(ct) (XCHAR_TABLE (ct)->dirty)
#define XCHAR_TABLE_MIRROR_TABLE_P(ct) (XCHAR_TABLE (ct)->mirror_table_p)
#endif /* MIRROR_TABLE */

enum chartab_range_type
{
  CHARTAB_RANGE_ALL,
#ifdef MULE
  CHARTAB_RANGE_CHARSET,
  CHARTAB_RANGE_ROW,
#endif
  CHARTAB_RANGE_RANGE,
  CHARTAB_RANGE_CHAR
};

struct chartab_range
{
  enum chartab_range_type type;
  Ichar ch, chtop;
  Lisp_Object charset;
  int row;
};

void set_char_table_default (Lisp_Object table, Lisp_Object value);

enum put_category_operation
{
  PUT_CATEGORY_SET,
  PUT_CATEGORY_UNSET,
  PUT_CATEGORY_RESET,
  PUT_CATEGORY_RESET_AND_SET
};

void put_char_table (Lisp_Object chartab, Ichar ch, Lisp_Object val);
void put_char_table_range (Lisp_Object table, struct chartab_range *range,
			   Lisp_Object val);
int map_char_table (Lisp_Object table,
		    struct chartab_range *range,
		    int (*fn) (Lisp_Object table, Ichar code, void *val,
			       void *arg),
		    void *arg);
void prune_syntax_tables (void);
int word_boundary_p (struct buffer *buf, Ichar c1, Ichar c2);

EXFUN (Fcopy_char_table, 1);
EXFUN (Fmake_char_table, 1);
EXFUN (Fput_char_table, 3);
EXFUN (Fget_char_table, 2);

extern Lisp_Object Vall_syntax_tables;


#ifdef MULE
/************************************************************************/
/*                           Category Tables                            */
/************************************************************************/

/* We used to store category tables using a normal char table, where the
   items in the char table were 96-bit bit vectors, one per category.  This
   was a simple approach and allowed some of the manipulation of category
   tables to occur in Lisp, and worked reasonably well with the old hairy
   char-table format which explicitly encoded charsets and charset ranges
   in it, but with the new page-table format it's horribly inefficient.  In
   a standard build, coming mostly from a single category table, you ended
   up with 125,047 bit vectors taking up 3MB of memory, and another 1MB of
   overhead when using gmalloc.c for allocation.  This is in addition to
   the space used to store the char subtables, which is likely another 800K
   or so.  And this is all for a single char table!  If you ever have to
   make another one, with different categories, the space usage could
   double.  Instead, rather the values of the char table being a 32-bit
   pointer to a bit vectors, we use the 32 bits themselves to store
   category values.  Then instead of one char table, we have three of them,
   stored within a new `category table' object.  In practice, we also
   reduce the size of the values down to 16 bits or 8 bits.  Since it's
   likely to be only one category that includes so many characters, we
   gain a lot of space by having only 8 bits used per most characters
   instead of 16 or 32, or 96 bits plus 96 more for the header plus
   malloc overhead, for the old bit-vector implementation.

   So the current implementation is:

   1. A Lisp `category-table' object, encapsulating 3, 6 or 12 `char-table'
      objects (corresponding to 32 bits, 16 bits and 8 bits, respectively).
   2. A `char-table' object, encapsulating various levels of `char-subtable'
      objects.
   3. `char-subtable' objects encapsulating other `char-subtable' objects
      at various levels, but at the bottom, encapsulating a
      `category-subtable' object.
   4. A `category-subtable' object encapsulating 256 sets of bits, each
      one holding 8, 16 or 32 bits, respectively.

  #### NOTE: It might be even more efficient to hold less than 8 bits per
  category-subtable object.  That would mean packing the bits for more
  than one character into a single integral type, and would require some
  additional work in the set/get routines.

*/

/* Specialization of general level-1 subtable code to category subtables. */

#define BITS_PER_CATEGORY_SUBTABLE 8
#define BITS_PER_CATEGORY_TABLE 96

#if BITS_PER_CATEGORY_SUBTABLE == 8
#define CATEGORY_DIVIDE_SHIFT 3
#define CATEGORY_MOD_AND 0x7
#define CATEGORY_TAB_BASE_TYPE unsigned char
#elif BITS_PER_CATEGORY_SUBTABLE == 16
#define CATEGORY_DIVIDE_SHIFT 4
#define CATEGORY_MOD_AND 0xF
#define CATEGORY_TAB_BASE_TYPE UINT_16_BIT
#elif BITS_PER_CATEGORY_SUBTABLE == 32
#define CATEGORY_DIVIDE_SHIFT 5
#define CATEGORY_MOD_AND 0x1F
#define CATEGORY_TAB_BASE_TYPE UINT_32_BIT
#else
#error "Invalid value for BITS_PER_CATEGORY_SUBTABLE"
#endif

#define CHAR_TABLES_PER_CATEGORY_TABLE \
  (BITS_PER_CATEGORY_TABLE / BITS_PER_CATEGORY_SUBTABLE)

struct Lisp_Category_Subtable
{
  NORMAL_LISP_OBJECT_HEADER header;
  CATEGORY_TAB_BASE_TYPE ptr[256];
};
typedef struct Lisp_Category_Subtable Lisp_Category_Subtable;

#define ALLOCATE_CATEGORY_SUBTABLE() \
  ALLOC_NORMAL_LISP_OBJECT (category_subtable)
#define BASE_TYPE_ARRAY_FROM_CATEGORY_SUBTAB(tab) \
  (XCATEGORY_SUBTABLE (tab)->ptr)

DECLARE_LISP_OBJECT (category_subtable, Lisp_Category_Subtable);
#define XCATEGORY_SUBTABLE(x) XRECORD (x, category_subtable, Lisp_Category_Subtable)
#define wrap_category_subtable(p) wrap_record (p, category_subtable)
#define CATEGORY_SUBTABLEP(x) RECORDP (x, category_subtable)
#define CHECK_CATEGORY_SUBTABLE(x) CHECK_RECORD (x, category_subtable)
#define CONCHECK_CATEGORY_SUBTABLE(x) CONCHECK_RECORD (x, category_subtable)

struct Lisp_Category_Table
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object tables[CHAR_TABLES_PER_CATEGORY_TABLE];
};
typedef struct Lisp_Category_Table Lisp_Category_Table;

DECLARE_LISP_OBJECT (category_table, Lisp_Category_Table);
#define XCATEGORY_TABLE(x) XRECORD (x, category_table, Lisp_Category_Table)
#define wrap_category_table(p) wrap_record (p, category_table)
#define CATEGORY_TABLEP(x) RECORDP (x, category_table)
#define CHECK_CATEGORY_TABLE(x) CHECK_RECORD (x, category_table)
#define CONCHECK_CATEGORY_TABLE(x) CONCHECK_RECORD (x, category_table)

#define CATEGORY_TABLE_TABLES(ct) ((ct)->tables)
#define XCATEGORY_TABLE_TABLES(ct) CATEGORY_TABLE_TABLES (XCATEGORY_TABLE (ct))

int check_char_in_category (Ichar ch, Lisp_Object ctbl, int designator,
			 int not_p);

extern Lisp_Object Vstandard_category_table;

#define CATEGORY_DESIGNATORP(x) \
 (CHARP (x) && XCHAR (x) >= 0x20 && XCHAR (x) <= 0x7E)

#define CHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    dead_wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)

#define CONCHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    x = wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)


/* Return the index of the char table storing the setting for this
   designator */
#define DESIGNATOR_TO_CHAR_TABLE(desig) \
  (((desig) - 0x20) >> CATEGORY_DIVIDE_SHIFT)
/* Return the bit index into the value of type CATEGORY_TAB_BASE_TYPE
   corresponding to the given designator */
#define DESIGNATOR_TO_BIT_INDEX(desig) \
  (((desig) - 0x20) & CATEGORY_MOD_AND)
/* Given a char table number and an index, return the corresponding
   designator */
#define BIT_INDEX_TO_DESIGNATOR(tablenum, ind) \
  ((tablenum) * BITS_PER_CATEGORY_SUBTABLE + (ind) + 0x20)


/* Return the OR (set) mask for this designator in the integral value of
   type CATEGORY_TAB_BASE_TYPE */
#define DESIGNATOR_TO_OR_MASK(desig) \
  BIT_INDEX_TO_SET_MASK (DESIGNATOR_TO_BIT_INDEX (desig))
/* Return the AND (clear) mask for this designator in the integral value of
   type CATEGORY_TAB_BASE_TYPE */
#define DESIGNATOR_TO_AND_MASK(desig) \
  BIT_INDEX_TO_CLEAR_MASK (DESIGNATOR_TO_BIT_INDEX (desig))

#else /* not MULE */

#define CATEGORY_TAB_BASE_TYPE unsigned char
#define BASE_TYPE_ARRAY_FROM_CATEGORY_SUBTAB(tab) \
  ((CATEGORY_TAB_BASE_TYPE *) NULL)

#endif /* MULE */


/************************************************************************/
/*                         get_char_table etc.                          */
/************************************************************************/

/* Get the raw value of CHARTAB for character CH.  If the character's value
   has not been set, return NULL (for a category char table) or the void *
   equivalent of Qunbound (for other char tables). */

DECLARE_INLINE_HEADER (
void *
get_char_table_raw (Ichar ch, Lisp_Object chartab)
)
{
  int levels;
  int u4, u3, u2, u1;
#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  int code_levels;
#endif
  int catp = XCHAR_TABLE_CATEGORY_P (chartab);

  text_checking_assert (valid_ichar_p (ch));
  CHARTAB_BREAKUP_CHAR_CODE ((int) ch, u4, u3, u2, u1, code_levels);

  levels = CHARTAB_LEVELS (XCHAR_TABLE_LEVELS (chartab));
  text_checking_assert (levels >= 1 && levels <= 4);

#if !defined (MULE) && defined (MAXIMIZE_CHAR_TABLE_DEPTH)
  /* This better be the case or something has gone majorly wrong --
     the "maximum" depth can't actually account for the highest possible
     character. */
  text_checking_assert (ch <= 255);
#endif

#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  /* If not that many levels even in the table, then value definitely not
     in the table */
  if (levels < code_levels)
    return catp ? NULL : STORE_LISP_IN_VOID (Qunbound);
#endif /* not MAXIMIZE_CHAR_TABLE_DEPTH */

  {
    register SUBTAB_TYPE table = XCHAR_TABLE_TABLE (chartab);
    /* We are really helping the compiler here.  CHARTAB_LEVELS() will
       evaluate to a constant when MAXIMIZE_CHAR_TABLE_DEPTH is true,
       so any reasonable optimizing compiler should eliminate the
       switch entirely. */
    switch (CHARTAB_LEVELS (levels))
      {
	/* Fall through */
      case 4: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u4];
      case 3: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u3];
      case 2: table = SUBTAB_ARRAY_FROM_SUBTAB (table)[u2];
      case 1:
	if (catp)
	  return (void *) (EMACS_INT) \
            BASE_TYPE_ARRAY_FROM_CATEGORY_SUBTAB (table)[u1];
	else
	  return
	    STORE_LISP_IN_VOID (BASE_TYPE_ARRAY_FROM_CHAR_SUBTAB (table)[u1]);
      }
  }

  ABORT (); /* Should never happen */
  return NULL;
}

/* Same as get_char_table_raw() but return the default value for
   non-category char tables if the value is not set. */

DECLARE_INLINE_HEADER (
void *
get_char_table (Ichar ch, Lisp_Object chartab)
)
{
  void *retval = get_char_table_raw (ch, chartab);
  if (XCHAR_TABLE_CATEGORY_P (chartab))
    return retval;
  else
    {
      if (!EQ (GET_LISP_FROM_VOID (retval), Qunbound))
	return retval;
      else
	return STORE_LISP_IN_VOID (XCHAR_TABLE_DEFAULT (chartab));
    }
}

/* Given a non-category char table, return the raw value of CHARTAB for
   character CH.  This will be a Lisp object, since we don't have to worry
   about category char tables. */
DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_lisp_raw (Ichar ch, Lisp_Object chartab)
)
{
  type_checking_assert (!XCHAR_TABLE_CATEGORY_P (chartab));
  return GET_LISP_FROM_VOID (get_char_table_raw (ch, chartab));
}

/* Same as get_char_table_lisp but don't trip an assert that we aren't
   retrieving the value for a mirror table. (Normally we have this assert
   in place to make sure that mirror tables don't escape to where they
   shouldn't be.  But some code really does need to access the mirror value
   itself -- otherwise, of course, we wouldn't have any need for mirror
   tables. */
DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_lisp_1 (Ichar ch, Lisp_Object chartab)
)
{
  Lisp_Object retval;
  retval = get_char_table_lisp_raw (ch, chartab);
  if (!EQ (retval, Qunbound))
    return retval;
  else
    return XCHAR_TABLE_DEFAULT (chartab);
}

/* Get the value of CHARTAB for character CH.  TABLE must not be a
   category-table char table.  If the character's value has not been set,
   this returns the default value for the char table. */

DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_lisp (Ichar ch, Lisp_Object table)
)
{
#ifdef MIRROR_TABLE
  type_checking_assert (!XCHAR_TABLE (table)->mirror_table_p);
#endif /* MIRROR_TABLE */
  return get_char_table_lisp_1 (ch, table);
}

#endif /* INCLUDED_chartab_h_ */
