/* XEmacs routines to deal with case tables.
   Copyright (C) 1987, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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
   was rewritten to use junky RMS char tables.  Meanwhile I rewrote it
   to use more logical char tables.  RMS also discards the "list of four
   tables" format and instead stuffs the other tables as "extra slots"
   in the downcase table.  I've kept the four-lists format for now. */

/* Written by Howard Gayle.  See some mythical and not-in-the-Emacs-
   distribution file chartab.c for details. */

/* Modified for Mule by Ben Wing. */

/* #### We do not currently deal properly with translating non-ASCII
   (including Latin-1!) characters under Mule.  Getting this right is
   *hard*, way fucking hard.  So we at least preserve consistency by
   sanitizing all the case tables to remove translations that would
   get us into trouble and possibly result in inconsistent internal
   text, which would likely lead to crashes. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "opaque.h"

Lisp_Object Qcase_table_p;
Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
Lisp_Object Vascii_canon_table, Vascii_eqv_table;
Lisp_Object Qtranslate_table;

static void compute_trt_inverse (Lisp_Object trt, Lisp_Object inverse);

#define STRING256_P(obj) \
  (STRINGP (obj) && string_char_length (XSTRING (obj)) == 256)

DEFUN ("case-table-p", Fcase_table_p, Scase_table_p, 1, 1, 0 /*
Return t iff ARG is a case table.
See `set-case-table' for more information on these data structures.
*/ )
  (table)
     Lisp_Object table;
{
  Lisp_Object down, up, canon, eqv;
  down = Fcar_safe (table);
  up = Fcar_safe (Fcdr_safe (table));
  canon = Fcar_safe (Fcdr_safe (Fcdr_safe (table)));
  eqv = Fcar_safe (Fcdr_safe (Fcdr_safe (Fcdr_safe (table))));

  return (STRING256_P (down)
	  && (NILP (up) || STRING256_P (up))
	  && ((NILP (canon) && NILP (eqv))
	      || (STRING256_P (canon)
                  && (NILP (eqv) || STRING256_P (eqv))))
	  ? Qt : Qnil);
}

static Lisp_Object
check_case_table (Lisp_Object obj)
{
  REGISTER Lisp_Object tem;

  while (tem = Fcase_table_p (obj), NILP (tem))
    obj = wrong_type_argument (Qcase_table_p, obj);
  return (obj);
}   

DEFUN ("current-case-table", Fcurrent_case_table, Scurrent_case_table,
       0, 1, 0 /*
Return the case table of BUFFER, which defaults to the current buffer.
*/ )
  (buffer)
     Lisp_Object buffer;
{
  Lisp_Object down, up, canon, eqv;
  struct buffer *buf = decode_buffer (buffer, 0);
  
  down = buf->downcase_table;
  up = buf->upcase_table;
  canon = buf->case_canon_table;
  eqv = buf->case_eqv_table;

  return Fcons (down, Fcons (up, Fcons (canon, Fcons (eqv, Qnil))));
}

DEFUN ("standard-case-table", Fstandard_case_table,
  Sstandard_case_table, 0, 0, 0 /*
Return the standard case table.
This is the one used for new buffers.
*/ )
  ()
{
  return Fcons (Vascii_downcase_table,
		Fcons (Vascii_upcase_table,
		       Fcons (Vascii_canon_table,
			      Fcons (Vascii_eqv_table,
				     Qnil))));
}

static Lisp_Object set_case_table (Lisp_Object table, int standard);


DEFUN ("set-case-table", Fset_case_table, Sset_case_table, 1, 1, 0 /*
Select a new case table for the current buffer.
A case table is a list (DOWNCASE UPCASE CANONICALIZE EQUIVALENCES)
 where each element is either nil or a string of length 256.
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
*/ )
  (table)
     Lisp_Object table;
{
  return set_case_table (table, 0);
}

DEFUN ("set-standard-case-table",
       Fset_standard_case_table, Sset_standard_case_table, 1, 1, 0 /*
Select a new standard case table for new buffers.
See `set-case-table' for more info on case tables.
*/ )
  (table)
     Lisp_Object table;
{
  return set_case_table (table, 1);
}

static Lisp_Object
set_case_table (Lisp_Object table, int standard)
{
  Lisp_Object down, up, canon, eqv;
  struct buffer *buf = current_buffer;

  check_case_table (table);

  down = Fcar_safe (table);
  up = Fcar_safe (Fcdr_safe (table));
  canon = Fcar_safe (Fcdr_safe (Fcdr_safe (table)));
  eqv = Fcar_safe (Fcdr_safe (Fcdr_safe (Fcdr_safe (table))));

  if (NILP (up))
    {
      up = MAKE_TRT_TABLE ();
      compute_trt_inverse (down, up);
    }

  if (NILP (canon))
    {
      REGISTER Charcount i;

      canon = MAKE_TRT_TABLE ();

      /* Set up the CANON vector; for each character,
	 this sequence of upcasing and downcasing ought to
	 get the "preferred" lowercase equivalent.  */
      for (i = 0; i < 256; i++)
	SET_TRT_TABLE_CHAR_1 (canon, i,
			      TRT_TABLE_CHAR_1
			      (down,
			       TRT_TABLE_CHAR_1
			       (up,
				TRT_TABLE_CHAR_1 (down, i))));
    }

  if (NILP (eqv))
    {
      eqv = MAKE_TRT_TABLE ();

      compute_trt_inverse (canon, eqv);
    }

  if (standard)
    {
      Vascii_downcase_table = down;
      Vascii_upcase_table = up;
      Vascii_canon_table = canon;
      Vascii_eqv_table = eqv;
    }
  else
    {
      buf->downcase_table = down;
      buf->upcase_table = up;
      buf->case_canon_table = canon;
      buf->case_eqv_table = eqv;
    }
  return table;
}

/* Given a translate table TRT, store the inverse mapping into INVERSE.
   Since TRT is not one-to-one, INVERSE is not a simple mapping.
   Instead, it divides the space of characters into equivalence classes.
   All characters in a given class form one circular list, chained through
   the elements of INVERSE.  */

static void
compute_trt_inverse (Lisp_Object trt, Lisp_Object inverse)
{
  Charcount i = 0400;
  Emchar c, q;

  while (--i)
    SET_TRT_TABLE_CHAR_1 (inverse, i, (Emchar) i);
  i = 0400;
  while (--i)
    {
      if ((q = TRT_TABLE_CHAR_1 (trt, i)) != (Emchar) i)
	{
	  c = TRT_TABLE_CHAR_1 (inverse, q);
	  SET_TRT_TABLE_CHAR_1 (inverse, q, (Emchar) i);
	  SET_TRT_TABLE_CHAR_1 (inverse, i, c);
	}
    }
}


void
syms_of_casetab (void)
{
  defsymbol (&Qcase_table_p, "case-table-p");
  defsymbol (&Qtranslate_table, "translate-table");

  defsubr (&Scase_table_p);
  defsubr (&Scurrent_case_table);
  defsubr (&Sstandard_case_table);
  defsubr (&Sset_case_table);
  defsubr (&Sset_standard_case_table);
}

void
complex_vars_of_casetab (void)
{
  REGISTER Emchar i;
  Lisp_Object tem;

  staticpro (&Vascii_downcase_table);
  staticpro (&Vascii_upcase_table);
  staticpro (&Vascii_canon_table);
  staticpro (&Vascii_eqv_table);

  tem = MAKE_TRT_TABLE ();
  Vascii_downcase_table = tem;
  Vascii_canon_table = tem;

  /* Under Mule, can't do set_string_char() until Vcharset_control_1
     and Vcharset_ascii are initialized. */
  for (i = 0; i < 256; i++)
    {
      unsigned char lowered = tolower (i);

      SET_TRT_TABLE_CHAR_1 (tem, i, lowered);
    }

  tem = MAKE_TRT_TABLE ();
  Vascii_upcase_table = tem;
  Vascii_eqv_table = tem;
  
  for (i = 0; i < 256; i++)
    {
      unsigned char flipped = (isupper (i) ? tolower (i)
			       : (islower (i) ? toupper (i) : i));

      SET_TRT_TABLE_CHAR_1 (tem, i, flipped);
    }

}
