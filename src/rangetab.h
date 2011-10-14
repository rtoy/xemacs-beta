/* XEmacs routines to deal with range tables.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 2004, 2010 Ben Wing.

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

/* Extracted from rangetab.c by O. Galibert, 1998. */

#ifndef INCLUDED_rangetab_h_
#define INCLUDED_rangetab_h_

typedef struct range_table_entry range_table_entry;
struct range_table_entry
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */
  EMACS_INT first;
  EMACS_INT last;
  Lisp_Object val;
};

typedef struct
{
  Dynarr_declare (range_table_entry);
} range_table_entry_dynarr;

enum range_table_type
{
  RANGE_START_CLOSED_END_OPEN,
  RANGE_START_CLOSED_END_CLOSED,
  RANGE_START_OPEN_END_CLOSED,
  RANGE_START_OPEN_END_OPEN
};

struct Lisp_Range_Table
{
  NORMAL_LISP_OBJECT_HEADER header;
  Gap_Array *entries;
  enum range_table_type type;
};
typedef struct Lisp_Range_Table Lisp_Range_Table;

DECLARE_LISP_OBJECT (range_table, Lisp_Range_Table);
#define XRANGE_TABLE(x) XRECORD (x, range_table, Lisp_Range_Table)
#define wrap_range_table(p) wrap_record (p, range_table)
#define RANGE_TABLEP(x) RECORDP (x, range_table)
#define CHECK_RANGE_TABLE(x) CHECK_RECORD (x, range_table)

#define rangetab_gap_array_at(ga, pos) \
  gap_array_at (ga, pos, struct range_table_entry)
#define rangetab_gap_array_atp(ga, pos) \
  gap_array_atp (ga, pos, struct range_table_entry)
#endif /* INCLUDED_rangetab_h_ */
