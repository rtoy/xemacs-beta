/* Simple 'n' stupid dynamic-array module -- include file.
   Copyright (C) 1993 Sun Microsystems, Inc.

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
Boston, MA 02111-1307, USA. */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing, December 1993. */

#ifndef _XEMACS_DYNARR_H_
#define _XEMACS_DYNARR_H_

#define Dynarr_declare(type)                                         \
  type *base;                                                        \
  int elsize;                                                        \
  int cur;                                                           \
  int largest;							     \
  int max

typedef struct dynarr
{
  Dynarr_declare (void);
} Dynarr;

void *Dynarr_newf (int elsize);
void Dynarr_resize (void *dy, int size);
void Dynarr_insert_many (void *d, CONST void *el, int len, int start);
void Dynarr_delete_many (void *d, int start, int len);
void Dynarr_free (void *d);

#define Dynarr_new(type) Dynarr_newf (sizeof(* (type *) NULL))
#define Dynarr_at(d, pos) ((d)->base[pos])
#define Dynarr_atp(d, pos) (&Dynarr_at (d, pos))
#define Dynarr_length(d) ((d)->cur)
#define Dynarr_largest(d) ((d)->largest)
#define Dynarr_reset(d) ((d)->cur = 0)
#define Dynarr_add_many(d, el, len) Dynarr_insert_many (d, el, len, (d)->cur)
#define Dynarr_insert_many_at_start(d, el, len)				\
  Dynarr_insert_many (d, el, len, 0)

#define Dynarr_add(d, el) (						\
  (d)->cur >= (d)->max ? Dynarr_resize ((d), (d)->cur+1) : (void) 0,	\
  ((d)->base)[(d)->cur++] = (el),					\
  (d)->cur > (d)->largest ? (d)->largest = (d)->cur : (int) 0)

/* The following defines will get you into real trouble if you aren't
   careful.  But they can save a lot of execution time when used wisely. */
#define Dynarr_increment(d) ((d)->cur++)
#define Dynarr_set_size(d, n) ((d)->cur = n)

/* Minimum size in elements for dynamic array when resized; default is 32 */
extern int Dynarr_min_size;

#ifdef MEMORY_USAGE_STATS

struct overhead_stats;

int Dynarr_memory_usage (void *d, struct overhead_stats *stats);

#endif

#endif /* _XEMACS_DYNARR_H_ */
