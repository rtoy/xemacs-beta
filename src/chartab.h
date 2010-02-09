/* Declarations having to do with Mule char tables.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003, 2005 Ben Wing.

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
   can't be used with either KKCC or NEWGC. (KKCC doesn't keep track of
   whether it has already traversed non-Lisp-object arrays, and thus
   traverses the shared "blank" subtables numerous times when marking,
   making it become *extremely* slow.  NEWGC requires that all Lisp objects
   occur inside of other Lisp objects, never inside of The split
   Lisp object is slightly more efficient than the */

struct Lisp_Char_Subtable
{
  struct LCRECORD_HEADER lheader;
  Lisp_Object ptr[256];
};

#define ALLOCATE_LEVEL_N_SUB_TABLE()					\
  wrap_char_subtable (ALLOC_LCRECORD_TYPE				\
		      (Lisp_Char_Subtable, &lrecord_char_subtable))

#define SUBTAB_STORAGE_SIZE(table, level, stats)			\
  LISPOBJ_STORAGE_SIZE (XCHAR_SUBTABLE (table),				\
			sizeof (struct Lisp_Char_Subtable), stats)

#define FREE_ONE_SUBTABLE(table) FREE_LCRECORD (table)

/* If we use split Lisp char subtables, we'd modify the above struct and
   three defines.  If we use "plain" non-Lisp char subtables, we'd modify
   the three macros above and the macros below as well, and omit the
   definition of a Lisp subtable object. */

#define SUBTAB_TYPE Lisp_Object
#define SUBTAB_ARRAY_TYPE SUBTAB_TYPE *
#define SUBTAB_ARRAY_FROM_SUBTAB(tab) (XCHAR_SUBTABLE (tab)->ptr)
#define LISPOBJ_ARRAY_FROM_SUBTAB(tab) (XCHAR_SUBTABLE (tab)->ptr)
#define SUBTAB_EQ(a, b) EQ (a, b)

#define ALLOCATE_LEVEL_1_SUB_TABLE() ALLOCATE_LEVEL_N_SUB_TABLE ()

typedef struct Lisp_Char_Subtable Lisp_Char_Subtable;

DECLARE_LRECORD (char_subtable, Lisp_Char_Subtable);
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
  struct LCRECORD_HEADER header;

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

DECLARE_LRECORD (char_table, Lisp_Char_Table);
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

#define XCHAR_TABLE_NEXT_TABLE(ct) (XCHAR_TABLE (ct)->next_table)
#ifdef MIRROR_TABLE
#define XCHAR_TABLE_MIRROR_TABLE(ct) (XCHAR_TABLE (ct)->mirror_table)
#define XCHAR_TABLE_DIRTY(ct) (XCHAR_TABLE (ct)->dirty)
#define XCHAR_TABLE_MIRROR_TABLE_P(ct) (XCHAR_TABLE (ct)->mirror_table_p)
#endif /* MIRROR_TABLE */

/* Get the raw value of CHARTAB for character CH.  This returns Qunbound
   if the character's value has not been set. */

DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_raw (Ichar ch, Lisp_Object chartab)
)
{
  int levels;
  int u4, u3, u2, u1;
#ifndef MAXIMIZE_CHAR_TABLE_DEPTH
  int code_levels;
#endif

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
    return Qunbound;
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
      case 1: return LISPOBJ_ARRAY_FROM_SUBTAB (table)[u1];
      }
  }

  ABORT (); /* Should never happen */
  return Qunbound;
}

/* Same as get_char_table but don't trip an assert that we aren't retrieving
   the value for a mirror table. (Normally we have this assert in place
   to make sure that mirror tables don't escape to where they shouldn't be.
   But some code really does need to access the mirror value itself --
   otherwise, of course, we wouldn't have any need for mirror tables. */
DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_1 (Ichar ch, Lisp_Object chartab)
)
{
  Lisp_Object retval = get_char_table_raw (ch, chartab);
  if (!UNBOUNDP (retval))
    return retval;
  else
    return XCHAR_TABLE_DEFAULT (chartab);
}

/* Get the value of CHARTAB for character CH.  If the character's value has
   not been set, this returns the default value for the char table. */

#ifdef ERROR_CHECK_TYPES
DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table (Ichar ch, Lisp_Object table)
)
{
#ifdef MIRROR_TABLE
  assert (!XCHAR_TABLE (table)->mirror_table_p);
#endif /* MIRROR_TABLE */
  return get_char_table_1 (ch, table);
}
#else
#define get_char_table(ch, table) get_char_table_1 (ch, table)
#endif

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
void put_char_table_1 (Lisp_Object chartab, Ichar ch, Lisp_Object val);
void put_char_table (Lisp_Object table, struct chartab_range *range,
		     Lisp_Object val);
int map_char_table (Lisp_Object table,
		    struct chartab_range *range,
		    int (*fn) (Lisp_Object table, Ichar code, Lisp_Object val,
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
int check_category_char (Ichar ch, Lisp_Object ctbl, int designator,
			 int not_p);

extern Lisp_Object Vstandard_category_table;

#define CATEGORY_DESIGNATORP(x) \
 (CHARP (x) && XCHAR (x) >= 32 && XCHAR (x) <= 126)

#define CHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    dead_wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)

#define CONCHECK_CATEGORY_DESIGNATOR(x) do {			\
  if (!CATEGORY_DESIGNATORP (x))				\
    x = wrong_type_argument (Qcategory_designator_p, x);	\
} while (0)

#define CATEGORY_TABLE_VALUEP(x) \
 (NILP (x) || (BIT_VECTORP (x) && (bit_vector_length (XBIT_VECTOR (x)) == 95)))

#define CHECK_CATEGORY_TABLE_VALUE(x) do {			\
  if (!CATEGORY_TABLE_VALUEP (x))				\
    dead_wrong_type_argument (Qcategory_table_value_p, x);	\
} while (0)

#define CONCHECK_CATEGORY_TABLE_VALUE(x) do {			\
  if (!CATEGORY_TABLE_VALUEP (x))				\
    x = wrong_type_argument (Qcategory_table_value_p, x);	\
} while (0)

#endif /* MULE */

#endif /* INCLUDED_chartab_h_ */
