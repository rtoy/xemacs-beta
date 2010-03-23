/* XEmacs routines to deal with case tables.
   Copyright (C) 2000 Yoshiki Hayashi.
   Copyright (C) 2002, 2005, 2010 Ben Wing.
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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_casetab_h_
#define INCLUDED_casetab_h_

/* These are needed for LOWERCASEP, NOCASEP, UPCASE, DOWNCASE below */
#include "buffer.h"
#include "chartab.h"

struct Lisp_Case_Table
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object downcase_table;
  Lisp_Object upcase_table;
  Lisp_Object case_canon_table;
  Lisp_Object case_eqv_table;
  int dirty;
};
typedef struct Lisp_Case_Table Lisp_Case_Table;
  
DECLARE_LISP_OBJECT (case_table, Lisp_Case_Table);
#define XCASE_TABLE(x) XRECORD (x, case_table, Lisp_Case_Table)
#define wrap_case_table(p) wrap_record (p, case_table)
#define CASE_TABLEP(x) RECORDP (x, case_table)
#define CHECK_CASE_TABLE(x) CHECK_RECORD (x, case_table)
#define CONCHECK_CASE_TABLE(x) CONCHECK_RECORD (x, case_table)

void recompute_case_table (Lisp_Object casetab);

DECLARE_INLINE_HEADER (
Lisp_Case_Table *
XCASE_TABLE_UPDATE (Lisp_Object table)
)
{
  Lisp_Case_Table *ct = XCASE_TABLE (table);
  /* If the table is dirty (changes have been made without ancillary
     structures updated), recompute first. */
  if (ct->dirty)
    recompute_case_table (table);
  return ct;
}

#define CASE_TABLE_DOWNCASE(ct) ((ct)->downcase_table)
#define CASE_TABLE_UPCASE(ct) ((ct)->upcase_table)
#define CASE_TABLE_CANON(ct) ((ct)->case_canon_table)
#define CASE_TABLE_EQV(ct) ((ct)->case_eqv_table)
#define XCASE_TABLE_DOWNCASE(ct) (XCASE_TABLE (ct)->downcase_table)
#define XCASE_TABLE_UPCASE(ct) (XCASE_TABLE (ct)->upcase_table)
/* Only do automatic updating for canon and eqv, which are the two that are
   automatically computed and that are not up to date.  These are not
   normally used by the simple case routines.  canon is used by
   compare-buffer-substrings when case-insensitive and by the regex
   routines, and eqv is used only by the Boyer-Moore search routines. */
#define XCASE_TABLE_CANON(ct) (XCASE_TABLE_UPDATE (ct)->case_canon_table)
#define XCASE_TABLE_EQV(ct) (XCASE_TABLE_UPDATE (ct)->case_eqv_table)

#define SET_CASE_TABLE_DOWNCASE(ct, p) ((ct)->downcase_table = p)
#define SET_CASE_TABLE_UPCASE(ct, p) ((ct)->upcase_table = p)
#define SET_CASE_TABLE_CANON(ct, p) ((ct)->case_canon_table = p)
#define SET_CASE_TABLE_EQV(ct, p) ((ct)->case_eqv_table = p)
#define XSET_CASE_TABLE_DOWNCASE(ct, p)	\
  SET_CASE_TABLE_DOWNCASE (XCASE_TABLE (ct), p)
#define XSET_CASE_TABLE_UPCASE(ct, p)	\
  SET_CASE_TABLE_UPCASE (XCASE_TABLE (ct), p)
#define XSET_CASE_TABLE_CANON(ct, p)	\
  SET_CASE_TABLE_CANON (XCASE_TABLE (ct),  p)
#define XSET_CASE_TABLE_EQV(ct, p)	\
  SET_CASE_TABLE_EQV (XCASE_TABLE (ct),  p)

extern Lisp_Object Vstandard_case_table;


/************************************************************************/
/*                         Case conversion                              */
/************************************************************************/

/* A "trt" table is a mapping from characters to other characters,
   typically used to convert between uppercase and lowercase.
   */

/* The _1 macros are named as such because they assume that you have
   already guaranteed that the character values are all in the range
   0 - 255.  Bad lossage will happen otherwise. */

#define MAKE_TRT_TABLE() Fmake_char_table (Qgeneric)
DECLARE_INLINE_HEADER (
Ichar
TRT_TABLE_OF (Lisp_Object table, Ichar ch)
)
{
  Lisp_Object TRT_char;
  TRT_char = get_char_table_lisp (ch, table);
  if (NILP (TRT_char))
    return ch;
  else
    return XCHAR (TRT_char);
}
#define SET_TRT_TABLE_OF(table, ch1, ch2)	\
  Fput_char_table (make_char (ch1), make_char (ch2), table)

DECLARE_INLINE_HEADER (
Lisp_Object
BUFFER_CASE_TABLE (struct buffer *buf)
)
{
  return buf ? buf->case_table : current_buffer->case_table;
  /* When buf=0, was Vstandard_case_table, but this sucks.  If I set a
     different case table in this buffer, operations that use a case table
     by default should use the current one. */
}

/* Macros used below. */
#define DOWNCASE_TABLE_OF(buf, c)	\
  TRT_TABLE_OF (XCASE_TABLE_DOWNCASE (BUFFER_CASE_TABLE (buf)), c)
#define UPCASE_TABLE_OF(buf, c)		\
  TRT_TABLE_OF (XCASE_TABLE_UPCASE (BUFFER_CASE_TABLE (buf)), c)
#define CANON_TABLE_OF(buf, c)	\
  TRT_TABLE_OF (XCASE_TABLE_CANON (BUFFER_CASE_TABLE (buf)), c)

/* 1 if CH is upper case.  */

DECLARE_INLINE_HEADER (
int
UPPERCASEP (struct buffer *buf, Ichar ch)
)
{
  return DOWNCASE_TABLE_OF (buf, ch) != ch;
}

/* 1 if CH is lower case.  */

DECLARE_INLINE_HEADER (
int
LOWERCASEP (struct buffer *buf, Ichar ch)
)
{
  return (UPCASE_TABLE_OF   (buf, ch) != ch &&
	  DOWNCASE_TABLE_OF (buf, ch) == ch);
}

/* 1 if CH is neither upper nor lower case.  */

DECLARE_INLINE_HEADER (
int
NOCASEP (struct buffer *buf, Ichar ch)
)
{
  return UPCASE_TABLE_OF (buf, ch) == ch;
}

/* Upcase a character, or make no change if that cannot be done.  */

DECLARE_INLINE_HEADER (
Ichar
UPCASE (struct buffer *buf, Ichar ch)
)
{
  return (DOWNCASE_TABLE_OF (buf, ch) == ch) ? UPCASE_TABLE_OF (buf, ch) : ch;
}

/* Upcase a character known to be not upper case.  Unused. */

#define UPCASE1(buf, ch) UPCASE_TABLE_OF (buf, ch)

/* Downcase a character, or make no change if that cannot be done. */

#define DOWNCASE(buf, ch) DOWNCASE_TABLE_OF (buf, ch)

/* Convert a character to a canonical representation, so that case-independent
   comparisons will work. */

#define CANONCASE(buf, ch) CANON_TABLE_OF (buf, ch)

#endif /* INCLUDED_casetab_h_ */
