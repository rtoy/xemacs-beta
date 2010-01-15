/* Declarations having to do with Mule char tables.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003 Ben Wing.

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
/*                               Char Tables                            */
/************************************************************************/

/* Under Mule, we use a complex representation (see below).
   When not under Mule, there are only 256 possible characters
   so we just represent them directly. */

#ifdef MULE

struct Lisp_Char_Table_Entry
{
  struct LCRECORD_HEADER header;

  /* In the interests of simplicity, we just use a fixed 96-entry
     table.  If we felt like being smarter, we could make this
     variable-size and add an offset value into this structure. */
  Lisp_Object level2[96];
};
typedef struct Lisp_Char_Table_Entry Lisp_Char_Table_Entry;

DECLARE_LRECORD (char_table_entry, Lisp_Char_Table_Entry);
#define XCHAR_TABLE_ENTRY(x) \
  XRECORD (x, char_table_entry, Lisp_Char_Table_Entry)
#define wrap_char_table_entry(p) wrap_record (p, char_table_entry)
#define CHAR_TABLE_ENTRYP(x) RECORDP (x, char_table_entry)
/* #define CHECK_CHAR_TABLE_ENTRY(x) CHECK_RECORD (x, char_table_entry)
   char table entries should never escape to Lisp */

#endif /* MULE */

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

#ifdef MULE
#define NUM_ASCII_CHARS 160
#else
#define NUM_ASCII_CHARS 256
#endif

struct Lisp_Char_Table
{
  struct LCRECORD_HEADER header;

  Lisp_Object ascii[NUM_ASCII_CHARS];
  Lisp_Object default_;
  Lisp_Object parent; /* #### not yet implemented */
  
#ifdef MULE
  /* We basically duplicate the Mule vectors-of-vectors implementation.
     We can do this because we know a great deal about the sorts of
     things we are going to be indexing.

     The current implementation is as follows:

     ascii[0-159] is used for ASCII and Control-1 characters.

     level1[0 .. (NUM_LEADING_BYTES-1)] indexes charsets by leading
     byte (subtract MIN_LEADING_BYTE from the leading byte).  If the
     value of this is not an opaque, then it specifies a value for all
     characters in the charset.  Otherwise, it will be a
     96-Lisp-Object opaque that we created, specifying a value for
     each row.  If the value of this is not an opaque, then it
     specifies a value for all characters in the row.  Otherwise, it
     will be a 96-Lisp-Object opaque that we created, specifying a
     value for each character.

     NOTE: 1) This will fail if some C routine passes an opaque to
              Fput_char_table().  Currently this is not a problem
	      since all char tables that are created are Lisp-visible
	      and thus no one should ever be putting an opaque in
	      a char table.  Another possibility is to consider
	      adding a type to */

  Lisp_Object level1[NUM_LEADING_BYTES];

#endif /* MULE */

  enum char_table_type type;

  /* stuff used for syntax tables */
  Lisp_Object mirror_table; /* points to mirror table for this table
			       (a cache for quicker access), or a back
			       pointer if MIRROR_TABLE_P. */
  Lisp_Object next_table; /* DO NOT mark through this. */
  char dirty; /* nonzero if mirror dirty and needs updating. */
  char mirror_table_p; /* nonzero if this is a mirror table. */
};
typedef struct Lisp_Char_Table Lisp_Char_Table;

DECLARE_LRECORD (char_table, Lisp_Char_Table);
#define XCHAR_TABLE(x) XRECORD (x, char_table, Lisp_Char_Table)
#define wrap_char_table(p) wrap_record (p, char_table)
#define CHAR_TABLEP(x) RECORDP (x, char_table)
#define CHECK_CHAR_TABLE(x) CHECK_RECORD (x, char_table)
#define CONCHECK_CHAR_TABLE(x) CONCHECK_RECORD (x, char_table)

#define CHAR_TABLE_TYPE(ct) ((ct)->type)
#define XCHAR_TABLE_TYPE(ct) CHAR_TABLE_TYPE (XCHAR_TABLE (ct))

Lisp_Object get_non_ascii_char_table_value (Lisp_Char_Table *ct,
					    int leading_byte,
					    Ichar c);

DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table_1 (Ichar ch, Lisp_Object table)
)
{
  Lisp_Object retval;
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
#ifdef MULE
  if (ch < NUM_ASCII_CHARS)
    retval = ct->ascii[ch];
  else
    {
      unsigned char lb = ichar_leading_byte (ch);
      if (!CHAR_TABLE_ENTRYP (ct->level1[lb - MIN_LEADING_BYTE]))
	retval = ct->level1[lb - MIN_LEADING_BYTE];
      else
	retval = get_non_ascii_char_table_value (ct, lb, ch);
    }
#else /* not MULE */
  retval = ct->ascii[(unsigned char) ch];
#endif /* not MULE */
  if (!UNBOUNDP (retval))
    return retval;
  else
    return ct->default_;
}

#ifdef ERROR_CHECK_TYPES
DECLARE_INLINE_HEADER (
Lisp_Object
get_char_table (Ichar ch, Lisp_Object table)
)
{
  assert (!XCHAR_TABLE (table)->mirror_table_p);
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
  CHARTAB_RANGE_CHAR
};

struct chartab_range
{
  enum chartab_range_type type;
  Ichar ch;
  Lisp_Object charset;
  int row;
};

void set_char_table_default (Lisp_Object table, Lisp_Object value);
void put_char_table (Lisp_Object table, struct chartab_range *range,
		     Lisp_Object val);
int map_char_table (Lisp_Object table,
		    struct chartab_range *range,
		    int (*fn) (struct chartab_range *range,
			       Lisp_Object table,
			       Lisp_Object val, void *arg),
		    void *arg);
void prune_syntax_tables (void);
Lisp_Object get_range_char_table (struct chartab_range *range,
				  Lisp_Object table, Lisp_Object multi);
#ifdef ERROR_CHECK_TYPES
Lisp_Object updating_mirror_get_range_char_table (struct chartab_range *range,
						  Lisp_Object table,
						  Lisp_Object multi);
#else
#define updating_mirror_get_range_char_table get_range_char_table
#endif
void copy_char_table_range (Lisp_Object from, Lisp_Object to,
			    struct chartab_range *range);
int word_boundary_p (Ichar c1, Ichar c2);

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
