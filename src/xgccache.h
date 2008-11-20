/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.


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

/* Written by jwz, 14 jun 93 */

#ifndef INCLUDED_xgccache_h_
#define INCLUDED_xgccache_h_

struct gc_cache;
struct gc_cache *make_gc_cache (Display *, Window);
void free_gc_cache (struct gc_cache *cache);
GC gc_cache_lookup (struct gc_cache *, XGCValues *, unsigned long mask);

#define XE_GCONTEXT(cell) (XGContextFromGC(cell->gc))

#ifdef DEBUG_XEMACS

void describe_gc_cache (struct gc_cache *cache, int flags);

#define DGCCFLAG_DISABLE		0
#define DGCCFLAG_SUMMARY		1 << 0
#define DGCCFLAG_LIST_CELLS		1 << 1
#define DGCCFLAG_CELL_DETAILS		1 << 2
/* A combination of the flags above. */
#define DGCCFLAG_DEFAULT		DGCCFLAG_SUMMARY | DGCCFLAG_LIST_CELLS
#endif

#endif /* INCLUDED_xgccache_h_ */
