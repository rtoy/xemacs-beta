/* XEmacs routines to deal with case tables.
   Copyright (C) 2000 Yoshiki Hayashi.
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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_casetab_h_
#define INCLUDED_casetab_h_

struct Lisp_Case_Table
{
  struct lcrecord_header header;
  Lisp_Object downcase_table;
  Lisp_Object upcase_table;
  Lisp_Object case_canon_table;
  Lisp_Object case_eqv_table;
  int dirty;
};
typedef struct Lisp_Case_Table Lisp_Case_Table;
  
DECLARE_LRECORD (case_table, Lisp_Case_Table);
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

#endif /* INCLUDED_casetab_h_ */
