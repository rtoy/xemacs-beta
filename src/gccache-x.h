/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.


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

/* Synched up with: Not in FSF. */

/* Written by jwz, 14 jun 93 */

#ifndef INCLUDED_gccache_x_h_
#define INCLUDED_gccache_x_h_

#include <X11/Xlib.h>

#define GC_CACHE_SIZE 100

struct gcv_and_mask {
  XGCValues gcv;
  unsigned long mask;
};

struct gc_cache_cell {
  struct gcv_and_mask gcvm;
  GC gc;
  INT_16_BIT prev_index, next_index;
};

struct x_gc_cache {
  /* This is marked in x_mark_device(). */
  Lisp_Object table;

  struct gc_cache_cell *head;
  struct gc_cache_cell *tail;

  Display *dpy;
  Window window;

  UINT_16_BIT count;

#ifdef DEBUG_XEMACS
  UINT_16_BIT create_count;
  UINT_16_BIT delete_count;
#endif

  struct gc_cache_cell cells[GC_CACHE_SIZE];
};

void init_x_gc_cache (struct device *);
void free_x_gc_cache_entries (struct device *);
GC x_gc_cache_lookup (struct device *, XGCValues *, unsigned long mask);

#define XE_GCONTEXT(cell) (XGContextFromGC(cell->gc))

extern Lisp_Object Vgc_cache_hash_table_test;
extern Lisp_Object define_gc_cache_hash_table_test (void);

#ifdef DEBUG_XEMACS

void describe_gc_cache (struct x_gc_cache *, int flags);

#define DGCCFLAG_DISABLE		0
#define DGCCFLAG_SUMMARY		1 << 0
#define DGCCFLAG_LIST_CELLS		1 << 1
#define DGCCFLAG_CELL_DETAILS		1 << 2
/* A combination of the flags above. */
#define DGCCFLAG_DEFAULT		DGCCFLAG_SUMMARY | DGCCFLAG_LIST_CELLS

#endif /* DEBUG_XEMACS */

#endif /* INCLUDED_gccache_x_h_ */
