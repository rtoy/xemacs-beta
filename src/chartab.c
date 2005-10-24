/* XEmacs routines to deal with char tables.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002, 2003 Ben Wing.
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
/*                         Char Table object                            */
/************************************************************************/

#ifdef MULE

static Lisp_Object
mark_char_table_entry (Lisp_Object obj)
{
  Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY (obj);
  int i;

  for (i = 0; i < 96; i++)
    {
      mark_object (cte->level2[i]);
    }
  return Qnil;
}

static int
char_table_entry_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Char_Table_Entry *cte1 = XCHAR_TABLE_ENTRY (obj1);
  Lisp_Char_Table_Entry *cte2 = XCHAR_TABLE_ENTRY (obj2);
  int i;

  for (i = 0; i < 96; i++)
    if (!internal_equal (cte1->level2[i], cte2->level2[i], depth + 1))
      return 0;

  return 1;
}

static Hashcode
char_table_entry_hash (Lisp_Object obj, int depth)
{
  Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY (obj);

  return internal_array_hash (cte->level2, 96, depth + 1);
}

static const struct memory_description char_table_entry_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Char_Table_Entry, level2), 96 },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("char-table-entry", char_table_entry,
			       1, /* dumpable flag */
                               mark_char_table_entry, internal_object_printer,
			       0, char_table_entry_equal,
			       char_table_entry_hash,
			       char_table_entry_description,
			       Lisp_Char_Table_Entry);

#endif /* MULE */

static Lisp_Object
mark_char_table (Lisp_Object obj)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (obj);
  int i;

  for (i = 0; i < NUM_ASCII_CHARS; i++)
    mark_object (ct->ascii[i]);
#ifdef MULE
  for (i = 0; i < NUM_LEADING_BYTES; i++)
    mark_object (ct->level1[i]);
#endif
  mark_object (ct->parent);
  mark_object (ct->default_);
  return ct->mirror_table;
}

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

static void
decode_char_table_range (Lisp_Object range, struct chartab_range *outrange)
{
  if (EQ (range, Qt))
    outrange->type = CHARTAB_RANGE_ALL;
  else if (CHAR_OR_CHAR_INTP (range))
    {
      outrange->type = CHARTAB_RANGE_CHAR;
      outrange->ch = XCHAR_OR_CHAR_INT (range);
    }
#ifndef MULE
  else
    sferror ("Range must be t or a character", range);
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
      switch (XCHARSET_TYPE (outrange->charset))
	{
	case CHARSET_TYPE_94:
	case CHARSET_TYPE_96:
	  sferror ("Charset in row vector must be multi-byte",
			       outrange->charset);
	case CHARSET_TYPE_94X94:
	  check_int_range (outrange->row, 33, 126);
	  break;
	case CHARSET_TYPE_96X96:
	  check_int_range (outrange->row, 32, 127);
	  break;
	default:
	  ABORT ();
	}
    }
  else
    {
      if (!CHARSETP (range) && !SYMBOLP (range))
	sferror
	  ("Char table range must be t, charset, char, or vector", range);
      outrange->type = CHARTAB_RANGE_CHARSET;
      outrange->charset = Fget_charset (range);
    }
#endif /* MULE */
}

static Lisp_Object
encode_char_table_range (struct chartab_range *range)
{
  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      return Qt;
      
#ifdef MULE
    case CHARTAB_RANGE_CHARSET:
      return XCHARSET_NAME (Fget_charset (range->charset));

    case CHARTAB_RANGE_ROW:
      return vector2 (XCHARSET_NAME (Fget_charset (range->charset)),
		      make_int (range->row));
#endif
    case CHARTAB_RANGE_CHAR:
      return make_char (range->ch);
    default:
      ABORT ();
    }
  return Qnil; /* not reached */
}

struct ptemap
{
  Lisp_Object printcharfun;
  int first;
};

static int
print_table_entry (struct chartab_range *range, Lisp_Object UNUSED (table),
		   Lisp_Object val, void *arg)
{
  struct ptemap *a = (struct ptemap *) arg;
  struct gcpro gcpro1;
  Lisp_Object lisprange;
  if (!a->first)
    write_c_string (a->printcharfun, " ");
  a->first = 0;
  lisprange = encode_char_table_range (range);
  GCPRO1 (lisprange);
  write_fmt_string_lisp (a->printcharfun, "%s %s", 2, lisprange, val);
  UNGCPRO;
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
  write_c_string (printcharfun, "))");

  /* #### need to print and read the default; but that will allow the
     default to be modified, which we don't (yet) support -- but FSF does */
}

static int
char_table_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Char_Table *ct1 = XCHAR_TABLE (obj1);
  Lisp_Char_Table *ct2 = XCHAR_TABLE (obj2);
  int i;

  if (CHAR_TABLE_TYPE (ct1) != CHAR_TABLE_TYPE (ct2))
    return 0;

  for (i = 0; i < NUM_ASCII_CHARS; i++)
    if (!internal_equal (ct1->ascii[i], ct2->ascii[i], depth + 1))
      return 0;

#ifdef MULE
  for (i = 0; i < NUM_LEADING_BYTES; i++)
    if (!internal_equal (ct1->level1[i], ct2->level1[i], depth + 1))
      return 0;
#endif /* MULE */

  return internal_equal (ct1->default_, ct2->default_, depth + 1);
}

static Hashcode
char_table_hash (Lisp_Object obj, int depth)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (obj);
  Hashcode hashval = internal_array_hash (ct->ascii, NUM_ASCII_CHARS,
					   depth + 1);
#ifdef MULE
  hashval = HASH2 (hashval,
		   internal_array_hash (ct->level1, NUM_LEADING_BYTES,
					depth + 1));
#endif /* MULE */
  return HASH2 (hashval, internal_hash (ct->default_, depth + 1));
}

static const struct memory_description char_table_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Char_Table, ascii), NUM_ASCII_CHARS },
#ifdef MULE
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Char_Table, level1), NUM_LEADING_BYTES },
#endif
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, parent) },
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, default_) },
  { XD_LISP_OBJECT, offsetof (Lisp_Char_Table, mirror_table) },
  { XD_LO_LINK,     offsetof (Lisp_Char_Table, next_table) },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("char-table", char_table,
			       1, /*dumpable-flag*/
                               mark_char_table, print_char_table, 0,
			       char_table_equal, char_table_hash,
			       char_table_description,
			       Lisp_Char_Table);

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
set_char_table_dirty (Lisp_Object table)
{
  assert (!XCHAR_TABLE (table)->mirror_table_p);
  XCHAR_TABLE (XCHAR_TABLE (table)->mirror_table)->dirty = 1;
}

void
set_char_table_default (Lisp_Object table, Lisp_Object value)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
  ct->default_ = value;
  if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
    set_char_table_dirty (table);
}

static void
fill_char_table (Lisp_Char_Table *ct, Lisp_Object value)
{
  int i;

  for (i = 0; i < NUM_ASCII_CHARS; i++)
    ct->ascii[i] = value;
#ifdef MULE
  for (i = 0; i < NUM_LEADING_BYTES; i++)
    {
      /* Don't get stymied when initting the table, or when trying to
	 free a pdump object. */
      if (!EQ (ct->level1[i], Qnull_pointer) &&
	  CHAR_TABLE_ENTRYP (ct->level1[i]) &&
	  !OBJECT_DUMPED_P (ct->level1[1]))
	FREE_LCRECORD (ct->level1[i]);
      ct->level1[i] = value;
    }
#endif /* MULE */

  if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
    set_char_table_dirty (wrap_char_table (ct));
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
  ct->default_ = def;
  fill_char_table (ct, Qunbound);

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
	Used for category tables, which specify the regexp categories
	that a character is in.  The valid values are nil or a
	bit vector of 95 elements.  Higher-level Lisp functions are
	provided for working with category tables.  Currently categories
	and category tables only exist when Mule support is present.
`char'
	A generalized char table, for mapping from one character to
	another.  Used for case tables, syntax matching tables,
	`keyboard-translate-table', etc.  The valid values are characters.
`generic'
        An even more generalized char table, for mapping from a
	character to anything.
`display'
	Used for display tables, which specify how a particular character
	is to appear when displayed.  #### Not yet implemented.
`syntax'
	Used for syntax tables, which specify the syntax of a particular
	character.  Higher-level Lisp functions are provided for
	working with syntax tables.  The valid values are integers.
*/
       (type))
{
  Lisp_Char_Table *ct;
  Lisp_Object obj;
  enum char_table_type ty = symbol_to_char_table_type (type);

  ct = ALLOC_LCRECORD_TYPE (Lisp_Char_Table, &lrecord_char_table);
  ct->type = ty;
  obj = wrap_char_table (ct);
  if (ty == CHAR_TABLE_TYPE_SYNTAX)
    {
      /* Qgeneric not Qsyntax because a syntax table has a mirror table
	 and we don't want infinite recursion */
      ct->mirror_table = Fmake_char_table (Qgeneric);
      set_char_table_default (ct->mirror_table, make_int (Spunct));
      XCHAR_TABLE (ct->mirror_table)->mirror_table_p = 1;
      XCHAR_TABLE (ct->mirror_table)->mirror_table = obj;
    }
  else
    ct->mirror_table = Qnil;
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

#ifdef MULE

static Lisp_Object
make_char_table_entry (Lisp_Object initval)
{
  int i;
  Lisp_Char_Table_Entry *cte =
    ALLOC_LCRECORD_TYPE (Lisp_Char_Table_Entry, &lrecord_char_table_entry);

  for (i = 0; i < 96; i++)
    cte->level2[i] = initval;

  return wrap_char_table_entry (cte);
}

static Lisp_Object
copy_char_table_entry (Lisp_Object entry)
{
  Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY (entry);
  int i;
  Lisp_Char_Table_Entry *ctenew =
    ALLOC_LCRECORD_TYPE (Lisp_Char_Table_Entry, &lrecord_char_table_entry);

  for (i = 0; i < 96; i++)
    {
      Lisp_Object new = cte->level2[i];
      if (CHAR_TABLE_ENTRYP (new))
	ctenew->level2[i] = copy_char_table_entry (new);
      else
	ctenew->level2[i] = new;
    }

  return wrap_char_table_entry (ctenew);
}

#endif /* MULE */

DEFUN ("copy-char-table", Fcopy_char_table, 1, 1, 0, /*
Return a new char table which is a copy of CHAR-TABLE.
It will contain the same values for the same characters and ranges
as CHAR-TABLE.  The values will not themselves be copied.
*/
       (char_table))
{
  Lisp_Char_Table *ct, *ctnew;
  Lisp_Object obj;
  int i;

  CHECK_CHAR_TABLE (char_table);
  ct = XCHAR_TABLE (char_table);
  ctnew = ALLOC_LCRECORD_TYPE (Lisp_Char_Table, &lrecord_char_table);
  ctnew->type = ct->type;
  ctnew->parent = ct->parent;
  ctnew->default_ = ct->default_;
  ctnew->mirror_table_p = ct->mirror_table_p;
  obj = wrap_char_table (ctnew);

  for (i = 0; i < NUM_ASCII_CHARS; i++)
    {
      Lisp_Object new = ct->ascii[i];
#ifdef MULE
      assert (! (CHAR_TABLE_ENTRYP (new)));
#endif /* MULE */
      ctnew->ascii[i] = new;
    }

#ifdef MULE

  for (i = 0; i < NUM_LEADING_BYTES; i++)
    {
      Lisp_Object new = ct->level1[i];
      if (CHAR_TABLE_ENTRYP (new))
	ctnew->level1[i] = copy_char_table_entry (new);
      else
	ctnew->level1[i] = new;
    }

#endif /* MULE */

  if (!ct->mirror_table_p && CHAR_TABLEP (ct->mirror_table))
    {
      ctnew->mirror_table = Fcopy_char_table (ct->mirror_table);
      XCHAR_TABLE (ctnew->mirror_table)->mirror_table = obj;
    }
  else
    ctnew->mirror_table = ct->mirror_table;
  ctnew->next_table = Qnil;
  if (ctnew->type == CHAR_TABLE_TYPE_SYNTAX)
    {
      ctnew->next_table = Vall_syntax_tables;
      Vall_syntax_tables = obj;
    }
  return obj;
}

#ifdef MULE

/* called from get_char_table(). */
Lisp_Object
get_non_ascii_char_table_value (Lisp_Char_Table *ct, int leading_byte,
				Ichar c)
{
  Lisp_Object val;
  Lisp_Object charset = charset_by_leading_byte (leading_byte);
  int byte1, byte2;

  BREAKUP_ICHAR_1_UNSAFE (c, charset, byte1, byte2);
  val = ct->level1[leading_byte - MIN_LEADING_BYTE];
  if (CHAR_TABLE_ENTRYP (val))
    {
      Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY (val);
      val = cte->level2[byte1 - 32];
      if (CHAR_TABLE_ENTRYP (val))
	{
	  cte = XCHAR_TABLE_ENTRY (val);
	  assert (byte2 >= 32);
	  val = cte->level2[byte2 - 32];
	  assert (!CHAR_TABLE_ENTRYP (val));
	}
    }

  return val;
}

#endif /* MULE */

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
copy_mapper (struct chartab_range *range, Lisp_Object UNUSED (table),
	     Lisp_Object val, void *arg)
{
  put_char_table (VOID_TO_LISP (arg), range, val);
  return 0;
}

void
copy_char_table_range (Lisp_Object from, Lisp_Object to,
		       struct chartab_range *range)
{
  map_char_table (from, range, copy_mapper, LISP_TO_VOID (to));
}

static Lisp_Object
get_range_char_table_1 (struct chartab_range *range, Lisp_Object table,
			Lisp_Object multi)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
  Lisp_Object retval = Qnil;

  switch (range->type)
    {
    case CHARTAB_RANGE_CHAR:
      return get_char_table (range->ch, table);

    case CHARTAB_RANGE_ALL:
      {
	int i;
	retval = ct->ascii[0];

	for (i = 1; i < NUM_ASCII_CHARS; i++)
	  if (!EQ (retval, ct->ascii[i]))
	    return multi;

#ifdef MULE
	for (i = MIN_LEADING_BYTE; i < MIN_LEADING_BYTE + NUM_LEADING_BYTES;
	     i++)
	  {
	    if (!CHARSETP (charset_by_leading_byte (i))
		|| i == LEADING_BYTE_ASCII
		|| i == LEADING_BYTE_CONTROL_1)
	      continue;
	    if (!EQ (retval, ct->level1[i - MIN_LEADING_BYTE]))
	      return multi;
	  }
#endif /* MULE */

	break;
      }

#ifdef MULE
    case CHARTAB_RANGE_CHARSET:
      if (EQ (range->charset, Vcharset_ascii))
	{
	  int i;
	  retval = ct->ascii[0];

	  for (i = 1; i < 128; i++)
	    if (!EQ (retval, ct->ascii[i]))
	      return multi;
	  break;
	}

      if (EQ (range->charset, Vcharset_control_1))
	{
	  int i;
	  retval = ct->ascii[128];

	  for (i = 129; i < 160; i++)
	    if (!EQ (retval, ct->ascii[i]))
	      return multi;
	  break;
	}

      {
	retval = ct->level1[XCHARSET_LEADING_BYTE (range->charset) -
			    MIN_LEADING_BYTE];
	if (CHAR_TABLE_ENTRYP (retval))
	  return multi;
	break;
      }

    case CHARTAB_RANGE_ROW:
      {
	retval = ct->level1[XCHARSET_LEADING_BYTE (range->charset) -
			    MIN_LEADING_BYTE];
	if (!CHAR_TABLE_ENTRYP (retval))
	  break;
	retval = XCHAR_TABLE_ENTRY (retval)->level2[range->row - 32];
	if (CHAR_TABLE_ENTRYP (retval))
	  return multi;
	break;
      }
#endif /* not MULE */

    default:
      ABORT ();
    }

  if (UNBOUNDP (retval))
    return ct->default_;
  return retval;
}

Lisp_Object
get_range_char_table (struct chartab_range *range, Lisp_Object table,
		      Lisp_Object multi)
{
  if (range->type == CHARTAB_RANGE_CHAR)
    return get_char_table (range->ch, table);
  else
    return get_range_char_table_1 (range, table, multi);
}

#ifdef ERROR_CHECK_TYPES

/* Only exists so as not to trip an assert in get_char_table(). */
Lisp_Object
updating_mirror_get_range_char_table (struct chartab_range *range,
				      Lisp_Object table,
				      Lisp_Object multi)
{
  if (range->type == CHARTAB_RANGE_CHAR)
    return get_char_table_1 (range->ch, table);
  else
    return get_range_char_table_1 (range, table, multi);
}

#endif /* ERROR_CHECK_TYPES */

DEFUN ("get-range-char-table", Fget_range_char_table, 2, 3, 0, /*
Find value for RANGE in CHAR-TABLE.
If there is more than one value, return MULTI (defaults to nil).

Valid values for RANGE are single characters, charsets, a row in a
two-octet charset, and all characters.  See `put-char-table'.
*/
       (range, char_table, multi))
{
  struct chartab_range rainj;

  if (CHAR_OR_CHAR_INTP (range))
    return Fget_char_table (range, char_table);
  CHECK_CHAR_TABLE (char_table);

  decode_char_table_range (range, &rainj);
  return get_range_char_table (&rainj, char_table, multi);
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

  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      fill_char_table (ct, val);
      return; /* fill_char_table() recorded the table as dirty. */

#ifdef MULE
    case CHARTAB_RANGE_CHARSET:
      if (EQ (range->charset, Vcharset_ascii))
	{
	  int i;
	  for (i = 0; i < 128; i++)
	    ct->ascii[i] = val;
	}
      else if (EQ (range->charset, Vcharset_control_1))
	{
	  int i;
	  for (i = 128; i < 160; i++)
	    ct->ascii[i] = val;
	}
      else
	{
	  int lb = XCHARSET_LEADING_BYTE (range->charset) - MIN_LEADING_BYTE;
	  if (CHAR_TABLE_ENTRYP (ct->level1[lb]) &&
	      !OBJECT_DUMPED_P (ct->level1[lb]))
	    FREE_LCRECORD (ct->level1[lb]);
	  ct->level1[lb] = val;
	}
      break;

    case CHARTAB_RANGE_ROW:
      {
	Lisp_Char_Table_Entry *cte;
	int lb = XCHARSET_LEADING_BYTE (range->charset) - MIN_LEADING_BYTE;
	/* make sure that there is a separate entry for the row. */
	if (!CHAR_TABLE_ENTRYP (ct->level1[lb]))
	  ct->level1[lb] = make_char_table_entry (ct->level1[lb]);
	cte = XCHAR_TABLE_ENTRY (ct->level1[lb]);
	cte->level2[range->row - 32] = val;
      }
      break;
#endif /* MULE */

    case CHARTAB_RANGE_CHAR:
#ifdef MULE
      {
	Lisp_Object charset;
	int byte1, byte2;

	BREAKUP_ICHAR (range->ch, charset, byte1, byte2);
	if (EQ (charset, Vcharset_ascii))
	  ct->ascii[byte1] = val;
	else if (EQ (charset, Vcharset_control_1))
	  ct->ascii[byte1 + 128] = val;
	else
	  {
	    Lisp_Char_Table_Entry *cte;
	    int lb = XCHARSET_LEADING_BYTE (charset) - MIN_LEADING_BYTE;
	    /* make sure that there is a separate entry for the row. */
	    if (!CHAR_TABLE_ENTRYP (ct->level1[lb]))
	      ct->level1[lb] = make_char_table_entry (ct->level1[lb]);
	    cte = XCHAR_TABLE_ENTRY (ct->level1[lb]);
	    /* now CTE is a char table entry for the charset;
	       each entry is for a single row (or character of
	       a one-octet charset). */
	    if (XCHARSET_DIMENSION (charset) == 1)
	      cte->level2[byte1 - 32] = val;
	    else
	      {
		/* assigning to one character in a two-octet charset. */
		/* make sure that the charset row contains a separate
		   entry for each character. */
		if (!CHAR_TABLE_ENTRYP (cte->level2[byte1 - 32]))
		  cte->level2[byte1 - 32] =
		    make_char_table_entry (cte->level2[byte1 - 32]);
		cte = XCHAR_TABLE_ENTRY (cte->level2[byte1 - 32]);
		cte->level2[byte2 - 32] = val;
	      }
	  }
      }
#else /* not MULE */
      ct->ascii[(unsigned char) (range->ch)] = val;
      break;
#endif /* not MULE */
    }

  if (ct->type == CHAR_TABLE_TYPE_SYNTAX)
    set_char_table_dirty (wrap_char_table (ct));
}

DEFUN ("put-char-table", Fput_char_table, 3, 3, 0, /*
Set the value for chars in RANGE to be VALUE in CHAR-TABLE.

RANGE specifies one or more characters to be affected and should be
one of the following:

-- t (all characters are affected)
-- A charset (only allowed when Mule support is present)
-- A vector of two elements: a two-octet charset and a row number; the row
   must be an integer, not a character (only allowed when Mule support is
   present)
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
  value = canonicalize_char_table_value (value, ct->type);
  put_char_table (char_table, &rainj, value);
  return Qnil;
}

DEFUN ("remove-char-table", Fremove_char_table, 2, 2, 0, /*
Remove any value from chars in RANGE in CHAR-TABLE.

RANGE specifies one or more characters to be affected and should be
one of the following:

-- t (all characters are affected)
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
  put_char_table (char_table, &rainj, Qunbound);
  return Qnil;
}

/* Map FN over the ASCII chars in CT. */

static int
map_over_charset_ascii_1 (Lisp_Char_Table *ct,
			  int start, int stop,
			  int (*fn) (struct chartab_range *range,
				     Lisp_Object table, Lisp_Object val,
				     void *arg),
			  void *arg)
{
  struct chartab_range rainj;
  int i, retval;

  rainj.type = CHARTAB_RANGE_CHAR;

  for (i = start, retval = 0; i <= stop && retval == 0; i++)
    {
      rainj.ch = (Ichar) i;
      if (!UNBOUNDP (ct->ascii[i]))
	retval = (fn) (&rainj, wrap_char_table (ct), ct->ascii[i], arg);
    }

  return retval;
}


/* Map FN over the ASCII chars in CT. */

static int
map_over_charset_ascii (Lisp_Char_Table *ct,
			int (*fn) (struct chartab_range *range,
				   Lisp_Object table, Lisp_Object val,
				   void *arg),
			void *arg)
{
  return map_over_charset_ascii_1 (ct, 0,
#ifdef MULE
				   127,
#else
				   255,
#endif
				   fn, arg);
}

#ifdef MULE

/* Map FN over the Control-1 chars in CT. */

static int
map_over_charset_control_1 (Lisp_Char_Table *ct,
			    int (*fn) (struct chartab_range *range,
				       Lisp_Object table, Lisp_Object val,
				       void *arg),
			    void *arg)
{
  return map_over_charset_ascii_1 (ct, 128, 159, fn, arg);
}

/* Map FN over the row ROW of two-byte charset CHARSET.
   There must be a separate value for that row in the char table.
   CTE specifies the char table entry for CHARSET. */

static int
map_over_charset_row (Lisp_Char_Table *ct,
		      Lisp_Char_Table_Entry *cte,
		      Lisp_Object charset, int row,
		      int (*fn) (struct chartab_range *range,
				 Lisp_Object table, Lisp_Object val,
				 void *arg),
		      void *arg)
{
  Lisp_Object val = cte->level2[row - 32];

  if (UNBOUNDP (val))
    return 0;
  else if (!CHAR_TABLE_ENTRYP (val))
    {
      struct chartab_range rainj;
      
      rainj.type = CHARTAB_RANGE_ROW;
      rainj.charset = charset;
      rainj.row = row;
      return (fn) (&rainj, wrap_char_table (ct), val, arg);
    }
  else
    {
      struct chartab_range rainj;
      int i, retval;
      int start, stop;
	  
      get_charset_limits (charset, &start, &stop);

      cte = XCHAR_TABLE_ENTRY (val);

      rainj.type = CHARTAB_RANGE_CHAR;

      for (i = start, retval = 0; i <= stop && retval == 0; i++)
	{
	  rainj.ch = make_ichar (charset, row, i);
	  if (!UNBOUNDP (cte->level2[i - 32]))
	    retval = (fn) (&rainj, wrap_char_table (ct), cte->level2[i - 32],
			   arg);
	}
      return retval;
    }
}


static int
map_over_other_charset (Lisp_Char_Table *ct, int lb,
			int (*fn) (struct chartab_range *range,
				   Lisp_Object table, Lisp_Object val,
				   void *arg),
			void *arg)
{
  Lisp_Object val = ct->level1[lb - MIN_LEADING_BYTE];
  Lisp_Object charset = charset_by_leading_byte (lb);

  if (!CHARSETP (charset)
      || lb == LEADING_BYTE_ASCII
      || lb == LEADING_BYTE_CONTROL_1)
    return 0;

  if (UNBOUNDP (val))
    return 0;
  if (!CHAR_TABLE_ENTRYP (val))
    {
      struct chartab_range rainj;

      rainj.type = CHARTAB_RANGE_CHARSET;
      rainj.charset = charset;
      return (fn) (&rainj, wrap_char_table (ct), val, arg);
    }
  {
    Lisp_Char_Table_Entry *cte = XCHAR_TABLE_ENTRY (val);
    int start, stop;
    int i, retval;

    get_charset_limits (charset, &start, &stop);
    if (XCHARSET_DIMENSION (charset) == 1)
      {
	struct chartab_range rainj;
	rainj.type = CHARTAB_RANGE_CHAR;

	for (i = start, retval = 0; i <= stop && retval == 0; i++)
	  {
	    rainj.ch = make_ichar (charset, i, 0);
	    if (!UNBOUNDP (cte->level2[i - 32]))
	      retval = (fn) (&rainj, wrap_char_table (ct), cte->level2[i - 32],
			     arg);
	  }
      }
    else
      {
	for (i = start, retval = 0; i <= stop && retval == 0; i++)
	  retval = map_over_charset_row (ct, cte, charset, i, fn, arg);
      }

    return retval;
  }
}

#endif /* MULE */

/* Map FN (with client data ARG) over range RANGE in char table CT.
   Mapping stops the first time FN returns non-zero, and that value
   becomes the return value of map_char_table().

   #### This mapping code is way ugly.  The FSF version, in contrast,
   is short and sweet, and much more recursive.  There should be some way
   of cleaning this up. */

int
map_char_table (Lisp_Object table,
		struct chartab_range *range,
		int (*fn) (struct chartab_range *range,
			   Lisp_Object table, Lisp_Object val, void *arg),
		void *arg)
{
  Lisp_Char_Table *ct = XCHAR_TABLE (table);
  switch (range->type)
    {
    case CHARTAB_RANGE_ALL:
      {
	int retval;

	retval = map_over_charset_ascii (ct, fn, arg);
	if (retval)
	  return retval;
#ifdef MULE
	retval = map_over_charset_control_1 (ct, fn, arg);
	if (retval)
	  return retval;
	{
	  int i;
	  int start = MIN_LEADING_BYTE;
	  int stop  = start + NUM_LEADING_BYTES;

	  for (i = start, retval = 0; i < stop && retval == 0; i++)
	    {
	      if (i != LEADING_BYTE_ASCII && i != LEADING_BYTE_CONTROL_1)
		retval = map_over_other_charset (ct, i, fn, arg);
	    }
	}
#endif /* MULE */
	return retval;
      }

#ifdef MULE
    case CHARTAB_RANGE_CHARSET:
      return map_over_other_charset (ct,
				     XCHARSET_LEADING_BYTE (range->charset),
				     fn, arg);

    case CHARTAB_RANGE_ROW:
      {
	Lisp_Object val = ct->level1[XCHARSET_LEADING_BYTE (range->charset) -
				     MIN_LEADING_BYTE];

	if (CHAR_TABLE_ENTRYP (val))
	  return map_over_charset_row (ct, XCHAR_TABLE_ENTRY (val),
				       range->charset, range->row, fn, arg);
	else if (!UNBOUNDP (val))
	  {
	    struct chartab_range rainj;

	    rainj.type = CHARTAB_RANGE_ROW;
	    rainj.charset = range->charset;
	    rainj.row = range->row;
	    return (fn) (&rainj, table, val, arg);
	  }
	else
	  return 0;
      }
#endif /* MULE */

    case CHARTAB_RANGE_CHAR:
      {
	Ichar ch = range->ch;
	Lisp_Object val = get_char_table (ch, table);
	struct chartab_range rainj;

	if (!UNBOUNDP (val))
	  {
	    rainj.type = CHARTAB_RANGE_CHAR;
	    rainj.ch = ch;
	    return (fn) (&rainj, table, val, arg);
	  }
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
slow_map_char_table_fun (struct chartab_range *range,
			 Lisp_Object UNUSED (table), Lisp_Object val,
			 void *arg)
{
  struct slow_map_char_table_arg *closure =
    (struct slow_map_char_table_arg *) arg;

  closure->retval = call2 (closure->function, encode_char_table_range (range),
			   val);
  return !NILP (closure->retval);
}

DEFUN ("map-char-table", Fmap_char_table, 2, 3, 0, /*
Map FUNCTION over CHAR-TABLE until it returns non-nil; return that value.
FUNCTION is called with two arguments, each key and entry in the table.

RANGE specifies a subrange to map over.  If omitted or t, it defaults to
the entire table.

Both RANGE and the keys passed to FUNCTION are in the same format as the
RANGE argument to `put-char-table'.  N.B. This function does NOT map over
all characters in RANGE, but over the subranges that have been assigned to.
Thus this function is most suitable for searching a char-table, or for
populating one char-table based on the contents of another.  The current
implementation does not coalesce ranges all of whose values are the same.
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
word_boundary_p (Ichar c1, Ichar c2)
{
  Lisp_Object category_set1, category_set2;
  Lisp_Object tail;
  int default_result;

#if 0
  if (COMPOSITE_CHAR_P (c1))
    c1 = cmpchar_component (c1, 0, 1);
  if (COMPOSITE_CHAR_P (c2))
    c2 = cmpchar_component (c2, 0, 1);
#endif

  if (EQ (ichar_charset (c1), ichar_charset (c2)))
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

#ifdef MULE
  INIT_LRECORD_IMPLEMENTATION (char_table_entry);

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
  DEFSUBR (Fget_range_char_table);
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
