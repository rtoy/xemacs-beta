/* XEmacs routines to deal with case tables.
   Copyright (C) 1987, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002 Ben Wing.

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

/* Synched up with: FSF 19.28.  Between FSF 19.28 and 19.30, casetab.c
   was rewritten to use junky FSF char tables.  Meanwhile I rewrote it
   to use more logical char tables. --ben */

/* Written by Howard Gayle. */

/* Modified for Mule by Ben Wing. */

/* The four tables in a case table are downcase, upcase, canon, and eqv.
   Each is a char-table.  Their workings are rather non-obvious.

   (1) `downcase' is the only obvious table: Map a character to its
   lowercase equivalent.

   (2) `upcase' does *NOT* map a character to its uppercase equivalent,
   despite its name.  Rather, it maps lowercase characters to their
   uppercase equivalent, and uppercase characters to *ANYTHING BUT* their
   uppercase equivalent (currently, their lowercase equivalent), and
   characters without case to themselves.  It is used to determine if a
   character "has no case" (no uppercase or lowercase mapping). #### This
   is way bogus.  Just use the obvious implementation of uppercase mapping
   and of NOCASE_P.

   (3) `canon' maps each character to a "canonical" lowercase, such that if
   two different uppercase characters map to the same lowercase character,
   or vice versa, both characters will have the same entry in the canon
   table.

   (4) `equiv' lists the "equivalence classes" defined by `canon'.  Imagine
   that all characters are divided into groups having the same `canon'
   entry; these groups are called "equivalence classes" and `equiv' lists
   them by linking the characters in each equivalence class together in a
   circular list.

   `canon' is used when doing case-insensitive comparisons.  `equiv' is
   used in the Boyer-Moore search code.
   */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "opaque.h"
#include "chartab.h"
#include "casetab.h"

Lisp_Object Qcase_tablep, Qdowncase, Qupcase;
Lisp_Object Vstandard_case_table;

Lisp_Object case_table_char (Lisp_Object ch, Lisp_Object table);

#define STRING256_P(obj) ((STRINGP (obj) && string_char_length (obj) == 256))

static Lisp_Object
mark_case_table (Lisp_Object obj)
{
  Lisp_Case_Table *ct = XCASE_TABLE (obj);

  mark_object (CASE_TABLE_DOWNCASE (ct));
  mark_object (CASE_TABLE_UPCASE (ct));
  mark_object (CASE_TABLE_CANON (ct));
  mark_object (CASE_TABLE_EQV (ct));
  return Qnil;
}

static void
print_case_table (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_Case_Table *ct = XCASE_TABLE (obj);
  if (print_readably)
    printing_unreadable_object ("#<case-table 0x%x>", ct->header.uid);
  write_fmt_string_lisp
    (printcharfun, "#<case-table downcase=%s upcase=%s canon=%s eqv=%s ", 4,
     CASE_TABLE_DOWNCASE (ct), CASE_TABLE_UPCASE (ct),
     CASE_TABLE_CANON (ct), CASE_TABLE_EQV (ct));
  write_fmt_string (printcharfun, "0x%x>", ct->header.uid);
}

static const struct memory_description case_table_description [] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Case_Table, downcase_table) },
  { XD_LISP_OBJECT, offsetof (Lisp_Case_Table, upcase_table) },
  { XD_LISP_OBJECT, offsetof (Lisp_Case_Table, case_canon_table) },
  { XD_LISP_OBJECT, offsetof (Lisp_Case_Table, case_eqv_table) },
  { XD_END }
};


DEFINE_LRECORD_IMPLEMENTATION("case-table", case_table,
			      1, /*dumpable-flag*/
			      mark_case_table, print_case_table, 0,
			      0, 0, case_table_description, Lisp_Case_Table);

static Lisp_Object
allocate_case_table (int init_tables)
{
  Lisp_Case_Table *ct =
    alloc_lcrecord_type (Lisp_Case_Table, &lrecord_case_table);

  if (init_tables)
    {
      SET_CASE_TABLE_DOWNCASE (ct, MAKE_TRT_TABLE ());
      SET_CASE_TABLE_UPCASE (ct, MAKE_TRT_TABLE ());
      SET_CASE_TABLE_CANON (ct, MAKE_TRT_TABLE ());
      SET_CASE_TABLE_EQV (ct, MAKE_TRT_TABLE ());
    }
  else
    {
      SET_CASE_TABLE_DOWNCASE (ct, Qnil);
      SET_CASE_TABLE_UPCASE (ct, Qnil);
      SET_CASE_TABLE_CANON (ct, Qnil);
      SET_CASE_TABLE_EQV (ct, Qnil);
    }
  return wrap_case_table (ct);
}

DEFUN ("make-case-table", Fmake_case_table, 0, 0, 0, /*
Create a new, empty case table.
*/
       ())
{
  return allocate_case_table (1);
}

DEFUN ("case-table-p", Fcase_table_p, 1, 1, 0, /*
Return t if OBJECT is a case table.
See `set-case-table' for more information on these data structures.
*/
       (object))
{
  if (CASE_TABLEP (object))
    return Qt;
  else
    {
      Lisp_Object down, up, canon, eqv;
      if (!CONSP (object))
	return Qnil;
      down = XCAR (object); object = XCDR (object);
      if (!CONSP (object))
	return Qnil;
      up = XCAR (object); object = XCDR (object);
      if (!CONSP (object))
	return Qnil;
      canon = XCAR (object); object = XCDR (object);
      if (!CONSP (object))
	return Qnil;
      eqv = XCAR (object);

      return ((STRING256_P (down)
	       && (NILP (up) || STRING256_P (up))
	       && ((NILP (canon) && NILP (eqv))
		   || STRING256_P (canon))
	       && (NILP (eqv) || STRING256_P (eqv)))
	      ? Qt : Qnil);

    }
}

static Lisp_Object
check_case_table (Lisp_Object object)
{
  /* This function can GC */
  while (NILP (Fcase_table_p (object)))
    object = wrong_type_argument (Qcase_tablep, object);
  return object;
}

Lisp_Object
case_table_char (Lisp_Object ch, Lisp_Object table)
{
  Lisp_Object ct_char;
  ct_char = get_char_table (XCHAR (ch), table);
  if (NILP (ct_char))
    return ch;
  else
    return ct_char;
}

DEFUN ("get-case-table", Fget_case_table, 3, 3, 0, /*
Return CHAR-CASE version of CHARACTER in CASE-TABLE.

CHAR-CASE is either `downcase' or `upcase'.
*/
       (char_case, character, case_table))
{
  CHECK_CHAR (character);
  CHECK_CASE_TABLE (case_table);
  if (EQ (char_case, Qdowncase))
    return case_table_char (character, XCASE_TABLE_DOWNCASE (case_table));
  else if (EQ (char_case, Qupcase))
    return case_table_char (character, XCASE_TABLE_UPCASE (case_table));
  else
    invalid_constant ("Char case must be downcase or upcase", char_case);

  return Qnil; /* Not reached. */
}

DEFUN ("put-case-table", Fput_case_table, 4, 4, 0, /*
Set CHAR-CASE version of CHARACTER to be VALUE in CASE-TABLE.

CHAR-CASE is either `downcase' or `upcase'.
See also `put-case-table-pair'.
*/
       (char_case, character, value, case_table))
{
  CHECK_CHAR (character);
  CHECK_CHAR (value);

  if (EQ (char_case, Qdowncase))
    {
      Fput_char_table (character, value, XCASE_TABLE_DOWNCASE (case_table));
      /* This one is not at all intuitive.  See comment at top of file. */
      Fput_char_table (character, value, XCASE_TABLE_UPCASE (case_table));
    }
  else if (EQ (char_case, Qupcase))
    {
      Fput_char_table (character, value, XCASE_TABLE_UPCASE (case_table));
      Fput_char_table (character, character,
		       XCASE_TABLE_DOWNCASE (case_table));
    }
  else
    invalid_constant ("CHAR-CASE must be downcase or upcase", char_case);

  XCASE_TABLE (case_table)->dirty = 1;
  return Qnil;
}

DEFUN ("put-case-table-pair", Fput_case_table_pair, 3, 3, 0, /*
Make UC and LC a pair of inter-case-converting letters in CASE-TABLE.
UC is an uppercase character and LC is a downcase character.
*/
       (uc, lc, case_table))
{
  CHECK_CHAR (uc);
  CHECK_CHAR (lc);
  CHECK_CASE_TABLE (case_table);

  Fput_char_table (lc, lc, XCASE_TABLE_DOWNCASE (case_table));
  Fput_char_table (uc, lc, XCASE_TABLE_UPCASE (case_table));
  Fput_char_table (uc, lc, XCASE_TABLE_DOWNCASE (case_table));
  Fput_char_table (lc, uc, XCASE_TABLE_UPCASE (case_table));

  XCASE_TABLE (case_table)->dirty = 1;
  return Qnil;
}

DEFUN ("copy-case-table", Fcopy_case_table, 1, 1, 0, /*
Return a new case table which is a copy of CASE-TABLE
*/
       (case_table))
{
  Lisp_Object new_obj;
  CHECK_CASE_TABLE (case_table);

  new_obj = allocate_case_table (0);
  XSET_CASE_TABLE_DOWNCASE
    (new_obj, Fcopy_char_table (XCASE_TABLE_DOWNCASE (case_table)));
  XSET_CASE_TABLE_UPCASE
    (new_obj, Fcopy_char_table (XCASE_TABLE_UPCASE (case_table)));
  XSET_CASE_TABLE_CANON
    (new_obj, Fcopy_char_table (XCASE_TABLE_CANON (case_table)));
  XSET_CASE_TABLE_EQV
    (new_obj, Fcopy_char_table (XCASE_TABLE_EQV (case_table)));
  return new_obj;
}

static int
compute_canon_mapper (struct chartab_range *range,
		      Lisp_Object UNUSED (table), Lisp_Object val, void *arg)
{
  Lisp_Object casetab = VOID_TO_LISP (arg);
  if (range->type == CHARTAB_RANGE_CHAR)
    SET_TRT_TABLE_OF (XCASE_TABLE_CANON (casetab), range->ch,
		      TRT_TABLE_OF (XCASE_TABLE_DOWNCASE (casetab),
				    TRT_TABLE_OF (XCASE_TABLE_UPCASE (casetab),
						  XCHAR (val))));

  return 0;
}

static int
initialize_identity_mapper (struct chartab_range *range,
			    Lisp_Object UNUSED (table),
			    Lisp_Object UNUSED (val), void *arg)
{
  Lisp_Object trt = VOID_TO_LISP (arg);
  if (range->type == CHARTAB_RANGE_CHAR)
    SET_TRT_TABLE_OF (trt, range->ch, range->ch);
  
  return 0;
}

static int
compute_up_or_eqv_mapper (struct chartab_range *range,
			  Lisp_Object UNUSED (table),
			  Lisp_Object val, void *arg)
{
  Lisp_Object inverse = VOID_TO_LISP (arg);
  Ichar toch = XCHAR (val);

  if (range->type == CHARTAB_RANGE_CHAR && range->ch != toch)
    {
      Ichar c = TRT_TABLE_OF (inverse, toch);
      SET_TRT_TABLE_OF (inverse, toch, range->ch);
      SET_TRT_TABLE_OF (inverse, range->ch, c);
    }
  
  return 0;
}

/* Recomputing the canonical and equivalency tables from scratch is a
   lengthy process, and doing them incrementally is extremely difficult or
   perhaps impossible -- and certainly not worth it.  To avoid lots of
   excessive recomputation when lots of stuff is incrementally added, we
   just store a dirty flag and then recompute when a value from the canon
   or eqv tables is actually needed. */

void
recompute_case_table (Lisp_Object casetab)
{
  struct chartab_range range;

  range.type = CHARTAB_RANGE_ALL;
  /* Turn off dirty flag first so we don't get infinite recursion when
     retrieving the values below! */
  XCASE_TABLE (casetab)->dirty = 0;
  map_char_table (XCASE_TABLE_DOWNCASE (casetab), &range,
		  compute_canon_mapper, LISP_TO_VOID (casetab));
  map_char_table (XCASE_TABLE_CANON (casetab), &range,
		  initialize_identity_mapper,
		  LISP_TO_VOID (XCASE_TABLE_EQV (casetab)));
  map_char_table (XCASE_TABLE_CANON (casetab), &range,
		  compute_up_or_eqv_mapper,
		  LISP_TO_VOID (XCASE_TABLE_EQV (casetab)));
}  

DEFUN ("current-case-table", Fcurrent_case_table, 0, 1, 0, /*
Return the case table of BUFFER, which defaults to the current buffer.
*/
       (buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);

  return buf->case_table;
}

DEFUN ("standard-case-table", Fstandard_case_table, 0, 0, 0, /*
Return the standard case table.
This is the one used for new buffers.
*/
       ())
{
  return Vstandard_case_table;
}

static void
convert_old_style_syntax_string (Lisp_Object table, Lisp_Object string)
{
  Ichar i;
  
  for (i = 0; i < 256; i++)
    SET_TRT_TABLE_OF (table, i, string_ichar (string, i));
}

static Lisp_Object
set_case_table (Lisp_Object table, int standard)
{
  /* This function can GC */
  struct buffer *buf =
    standard ? XBUFFER (Vbuffer_defaults) : current_buffer;

  check_case_table (table);

  if (CASE_TABLEP (table))
    {
      if (standard)
	Vstandard_case_table = table;

      buf->case_table = table;
    }
  else
    {
      /* For backward compatibility. */
      Lisp_Object down, up, canon, eqv, tail = table;
      Lisp_Object casetab =
	standard ? Vstandard_case_table :  buf->case_table;
      struct chartab_range range;

      range.type = CHARTAB_RANGE_ALL;

      Freset_char_table (XCASE_TABLE_DOWNCASE (casetab));
      Freset_char_table (XCASE_TABLE_UPCASE (casetab));
      Freset_char_table (XCASE_TABLE_CANON (casetab));
      Freset_char_table (XCASE_TABLE_EQV (casetab));

      down = XCAR (tail); tail = XCDR (tail);
      up = XCAR (tail); tail = XCDR (tail);
      canon = XCAR (tail); tail = XCDR (tail);
      eqv = XCAR (tail);

      convert_old_style_syntax_string (XCASE_TABLE_DOWNCASE (casetab), down);
      
      if (NILP (up))
	{
	  map_char_table (XCASE_TABLE_DOWNCASE (casetab), &range,
			  initialize_identity_mapper,
			  LISP_TO_VOID (XCASE_TABLE_UPCASE (casetab)));
	  map_char_table (XCASE_TABLE_DOWNCASE (casetab), &range,
			  compute_up_or_eqv_mapper,
			  LISP_TO_VOID (XCASE_TABLE_UPCASE (casetab)));
	}
      else
	convert_old_style_syntax_string (XCASE_TABLE_UPCASE (casetab), up);

      if (NILP (canon))
	map_char_table (XCASE_TABLE_DOWNCASE (casetab), &range,
			compute_canon_mapper, LISP_TO_VOID (casetab));
      else
	convert_old_style_syntax_string (XCASE_TABLE_CANON (casetab), canon);

      if (NILP (eqv))
	{
	  map_char_table (XCASE_TABLE_CANON (casetab), &range,
			  initialize_identity_mapper,
			  LISP_TO_VOID (XCASE_TABLE_EQV (casetab)));
	  map_char_table (XCASE_TABLE_CANON (casetab), &range,
			  compute_up_or_eqv_mapper,
			  LISP_TO_VOID (XCASE_TABLE_EQV (casetab)));
	}
      else
	convert_old_style_syntax_string (XCASE_TABLE_CANON (casetab), eqv);
    }

  return buf->case_table;
}

DEFUN ("set-case-table", Fset_case_table, 1, 1, 0, /*
Select CASE-TABLE as the new case table for the current buffer.
A case table is a case-table object or list
 (DOWNCASE UPCASE CANONICALIZE EQUIVALENCES)
 where each element is either nil or a string of length 256.
The latter is provided for backward-compatibility.
DOWNCASE maps each character to its lower-case equivalent.
UPCASE maps each character to its upper-case equivalent;
 if lower and upper case characters are in 1-1 correspondence,
 you may use nil and the upcase table will be deduced from DOWNCASE.
CANONICALIZE maps each character to a canonical equivalent;
 any two characters that are related by case-conversion have the same
 canonical equivalent character; it may be nil, in which case it is
 deduced from DOWNCASE and UPCASE.
EQUIVALENCES is a map that cyclicly permutes each equivalence class
 (of characters with the same canonical equivalent); it may be nil,
 in which case it is deduced from CANONICALIZE.

See also `get-case-table', `put-case-table' and `put-case-table-pair'.
*/
       (case_table))
{
  /* This function can GC */
  return set_case_table (case_table, 0);
}

DEFUN ("set-standard-case-table", Fset_standard_case_table, 1, 1, 0, /*
Select CASE-TABLE as the new standard case table for new buffers.
See `set-case-table' for more info on case tables.
*/
       (case_table))
{
  /* This function can GC */
  return set_case_table (case_table, 1);
}


void
syms_of_casetab (void)
{
  INIT_LRECORD_IMPLEMENTATION (case_table);

  DEFSYMBOL_MULTIWORD_PREDICATE (Qcase_tablep);
  DEFSYMBOL (Qdowncase);
  DEFSYMBOL (Qupcase);

  DEFSUBR (Fmake_case_table);
  DEFSUBR (Fcase_table_p);
  DEFSUBR (Fget_case_table);
  DEFSUBR (Fput_case_table);
  DEFSUBR (Fput_case_table_pair);
  DEFSUBR (Fcurrent_case_table);
  DEFSUBR (Fstandard_case_table);
  DEFSUBR (Fcopy_case_table);
  DEFSUBR (Fset_case_table);
  DEFSUBR (Fset_standard_case_table);
}

void
complex_vars_of_casetab (void)
{
  REGISTER Ichar i;

  staticpro (&Vstandard_case_table);

  Vstandard_case_table = allocate_case_table (1);

  for (i = 0; i < 256; i++)
    {
      unsigned char lowered = tolower (i);

      SET_TRT_TABLE_OF (XCASE_TABLE_DOWNCASE (Vstandard_case_table), i,
		        lowered);
    }

  for (i = 0; i < 256; i++)
    {
      unsigned char flipped = (isupper (i) ? tolower (i)
			       : (islower (i) ? toupper (i) : i));

      SET_TRT_TABLE_OF (XCASE_TABLE_UPCASE (Vstandard_case_table), i,
		        flipped);
    }

  recompute_case_table (Vstandard_case_table);
}
