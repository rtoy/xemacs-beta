/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2010 Ben Wing.

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

/* Emacs uses a lot of different display attributes; for example, assume
   that only four fonts are in use (normal, bold, italic, and bold-italic).
   Then assume that one stipple or background is used for text selections,
   and another is used for highlighting mousable regions.  That makes 16
   GCs already.  Add in the fact that another GC may be needed to display
   the text cursor in any of those regions, and you've got 32.  Add in
   more fonts, and it keeps increasing exponentially.

   We used to keep these GCs in a cache of merged (fully qualified) faces.
   However, a lot of other code in xterm.c used XChangeGC of existing GCs,
   which is kind of slow and kind of random.  Also, managing the face cache
   was tricky because it was hard to know when a face was no longer visible
   on the frame -- we had to mark all frames as garbaged whenever a face
   was changed, which caused an unpleasant amount of flicker (since faces are
   created/destroyed (= changed) whenever a frame is created/destroyed.

   So this code maintains a cache at the GC level instead of at the face
   level.  There is an upper limit on the size of the cache, after which we
   will stop creating GCs and start reusing them (reusing the least-recently-
   used ones first).  So if faces get changed, their GCs will eventually be
   recycled.  Also more sharing of GCs is possible.

   This code uses hash tables.  It could be that, if the cache size is small
   enough, a linear search might be faster; but I doubt it, since we need
   `equal' comparisons, not `eq', and I expect that the optimal cache size
   will be ~100.

   Written by jwz, 14 jun 93
 */

#include <config.h>
#include "lisp.h"
#include "hash.h"

#include "gccache-x.h"
#include "device-impl.h"
#include "console-x-impl.h"
#include "elhash.h"

static int
gc_cache_eql (const Hash_Table_Test * UNUSED (http),
              Lisp_Object arg1, Lisp_Object arg2)
{
  return !memcmp (GET_VOID_FROM_LISP (arg1), GET_VOID_FROM_LISP (arg2),
                  sizeof (struct gcv_and_mask));
}

static Hashcode
gc_cache_hash (const Hash_Table_Test * UNUSED (http), Lisp_Object arg)
{
  const struct gcv_and_mask *gcvm
    = (const struct gcv_and_mask *) GET_VOID_FROM_LISP (arg);
  EMACS_UINT *longs = (EMACS_UINT *) &gcvm->gcv;
  Hashcode hash = gcvm->mask;
  unsigned i;

  /* Starting from the end of the XGCValues and moving to the beginning has
     eliminated collisions on my machine as of 20170417, since the foreground
     and background pixels are closest to the beginning, and they vary so much
     more than the dash_offset, the dashes, the clip_x_origin and so on.

     There was an old comment here, from revision zero, about possibly looking
     at the mask and only hashing based on the used fields. That doesn't make
     sense in today's world, where branches are relatively expensive. The
     below (technically a use of Duff's device, but note the initial loop
     counter is a compile-time constant, and so the usual criticisms don't
     apply) translates into compile-time branchless inline code as of April
     2017 with -Ofast on GCC. I don't see any faster, or, really, smaller
     alternative. Aidan Kehoe, 20170417. */
  switch ((i = (unsigned) (sizeof (gcvm->gcv) / SIZEOF_EMACS_INT))
          % SIZEOF_EMACS_INT)
    {
      do
        {
        case 0:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
#if SIZEOF_EMACS_INT > 16
#error "unimplemented, look at the below code and copy it"
#endif
#if SIZEOF_EMACS_INT > 8
        case 15:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 14:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 13:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 12:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 11:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 10:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 9:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 8:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
#endif
#if SIZEOF_EMACS_INT > 4
        case 7:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 6:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 5:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 4:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
#endif
        case 3:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 2:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        case 1:
          hash = (hash << 1) ^ longs[--i];
          /* FALLTHROUGH */
        } while (i);
    }

  return hash;
}

Lisp_Object
define_gc_cache_hash_table_test ()
{
  return define_hash_table_test (Qunbound, gc_cache_eql, gc_cache_hash,
                                 Qnil, Qnil);
}

void
init_x_gc_cache (struct device *d)
{
  struct x_gc_cache *cache = DEVICE_X_GC_CACHE (d);
  xzero (*cache);
  cache->table = make_general_lisp_hash_table (Vgc_cache_hash_table_test,
                                               GC_CACHE_SIZE, -1.0, -1.0, 
                                               /* Don't mark the keys and
                                                  values, don't process them
                                                  for GC. Note that this
                                                  object is marked in
                                                  x_mark_device(). */
                                               HASH_TABLE_WEAK);
}

void
free_x_gc_cache_entries (struct device *d)
{
  struct x_gc_cache *cache = DEVICE_X_GC_CACHE (d);
  Display *dpy = DEVICE_X_DISPLAY (d);
  unsigned ii = 0;

  while (ii < cache->count)
    {
      XFreeGC (dpy, cache->cells[ii++].gc);
    }

  cache->table = Qnil; /* Let the entries be GC'd, there's no value to freeing
                          them explicitly, they are just fixnums. */
}

/* The hotspots of this function are (in decreasing order of frequency of call):

   a) When the requested GCV and MASK reflect the most-recently-used GC.
   b) When the requested GCV and MASK reflect a recently-used, but not *the*
   most-recently-used GC.
   c) When the requested GCV and MASK require that an old GC be evicted and a
   new GC be created, since the cache is full.

   Case B) could be made faster by using a tick counter and not bothering to
   adjust the LRU list for the most recent GC_CACHE_SIZE / 2 items.  I don't
   seee any evident possible wins for the other two cases. */
GC
x_gc_cache_lookup (struct device *d, XGCValues *gcv, unsigned long mask)
{
  struct x_gc_cache *cache = DEVICE_X_GC_CACHE (d);
  struct gc_cache_cell *cell = NULL;
  struct gcv_and_mask gcvm;
  htentry *e;
  int next_index, prev_index;

  gcvm.mask = mask;
  gcvm.gcv = *gcv;	/* this copies... */

  e = find_htentry (STORE_VOID_IN_LISP (&gcvm), XHASH_TABLE (cache->table));
  if (!HTENTRY_CLEAR_P (e))
    {
      cell = (struct gc_cache_cell *) GET_VOID_FROM_LISP (e->value);

      /* Found a cell. */
#ifdef DEBUG_GC_CACHE
      stderr_out ("Returning %scached GC: %08lx\n",
                 cell == cache->tail ? "most recently used " : "",
                 XE_GCONTEXT(cell));
#endif

      if (cell == cache->tail)
	return cell->gc; /* Case a) above. */

      /* Case b) above. */
      /* Move this cell to the end of the list, so that it will be less likely
	 to be collected than a cell that was accessed less recently. */
      next_index = cell->next_index;
      prev_index = cell->prev_index;
      if (prev_index != -1)
        {
          cache->cells[prev_index].next_index = next_index;
        }
      if (next_index != -1)
        {
          cache->cells[next_index].prev_index = prev_index;
        }
      if (cache->head == cell)
        {
          cache->head = &((cache->cells)[next_index]);
        }

      cell->next_index = -1;
      if (cache->tail)
        {
          assert (cache->tail >= cache->cells);
          assert ((cache->tail - cache->cells) < GC_CACHE_SIZE);
          cell->prev_index = (cache->tail - cache->cells);
        }
      else
        {
          cell->prev_index = -1;
        }

      assert ((cell - cache->cells) < GC_CACHE_SIZE);

      cache->tail->next_index = cell - cache->cells;
      cache->tail = cell;

      assert (cache->head != cell);
      assert (cache->head->prev_index == -1);
      assert (cache->tail->next_index == -1);

      return cell->gc;
    }

  assert ((!!cache->head) == (!!cache->tail));
  assert (!(cache->head && (cache->head->prev_index != -1
                            || cache->tail->next_index != -1)));

#ifdef DEBUG_GC_CACHE
  (void) describe_gc_cache (cache,
                            DGCCFLAG_SUMMARY | DGCCFLAG_LIST_CELLS);
#endif

  /* else, cache miss. */

  if (cache->count == GC_CACHE_SIZE)
    /* Reuse the first cell on the list (least-recently-used).
       Remove it from the list, and unhash it from the table. */
    {
      cell = cache->head;
      cache->head = &((cache->cells)[cell->next_index]);
      cache->head->prev_index = -1;
      if (cache->tail == cell) cache->tail = 0; /* only one */
#ifdef DEBUG_XEMACS
#ifdef DEBUG_GC_CACHE
      stderr_out ("Cache full, freeing GC: %08lx\n  ", XE_GCONTEXT(cell));
#endif
      cache->delete_count++;
#endif
      XFreeGC (DEVICE_X_DISPLAY (d), cell->gc);

      Fremhash (STORE_VOID_IN_LISP (&gcvm), cache->table);
    }
  else
    {
      assert (cache->count < GC_CACHE_SIZE);

      /* Reserve a new cell (don't put it in the list or table yet). */
      cell = &(cache->cells[cache->count++]);
    }

  /* Now we've got a cell (new or reused).  Fill it in. */
  memcpy (&cell->gcvm.gcv, gcv, sizeof (XGCValues));
  cell->gcvm.mask = mask;

  /* Put the cell on the end of the list. */
  cell->next_index = -1;
  cell->prev_index = cache->tail ? (cache->tail - cache->cells) : -1;
  if (cache->tail) cache->tail->next_index = (cell - cache->cells);
  cache->tail = cell;
  if (! (cache->head)) cache->head = cell;

#ifdef DEBUG_XEMACS
  cache->create_count++;
#endif

  /* Hash it in the table. */
  Fputhash (STORE_VOID_IN_LISP (&cell->gcvm), STORE_VOID_IN_LISP (cell),
            cache->table);

  /* Now make and return the GC. */
  cell->gc = XCreateGC (DEVICE_X_DISPLAY (d),
                        XtWindow (DEVICE_XT_APP_SHELL (d)),
                        mask, gcv);

#ifdef DEBUG_XEMACS
  /* debug */
  assert (cell->gc == x_gc_cache_lookup (d, gcv, mask));

#ifdef DEBUG_GC_CACHE
  stderr_out ("Returning new GC: %08lx\n  ", XE_GCONTEXT (cell));
#endif
#endif
  return cell->gc;
}


#ifdef DEBUG_XEMACS

/* FLAGS
   The flags argument is a bitwise or of any of the following:

   DGCCFLAG_SUMMARY		Summary statistics for cache
   DGCCFLAG_LIST_CELLS		If summary is being printed, print cell IDs too.
   DGCCFLAG_CELL_DETAILS	If cell IDs are being printed, additionally
				print the internal fields used and values.

   DGCCFLAG_DEFAULT		A predefined combination giving whatever the
				maintainers are currently interested in seeing.
*/
void
describe_gc_cache (struct x_gc_cache *cache, int flags)
{
  unsigned count = 0;
  struct gc_cache_cell *cell = cache->head;

  if (!(flags & DGCCFLAG_SUMMARY)) return;

  stderr_out ("\nsize:    %u", cache->count);
  stderr_out ("\ncreated: %u", cache->create_count);
  stderr_out ("\ndeleted: %u", cache->delete_count);

  if (flags & DGCCFLAG_LIST_CELLS)
    while (cell)
      {
	struct gc_cache_cell *cell2;
	unsigned i = 0;
 	stderr_out ("\n%u:\t%p  GC: 0x%08lx  hash: 0x%08lx\n",
 		    count, cell, XE_GCONTEXT (cell),
 		    gc_cache_hash (NULL, STORE_VOID_IN_LISP (&cell->gcvm)));

 	for (cell2 = cache->head; cell2;
             (cell2 = ((cell2->next_index == -1) ? NULL :
                       &(cache->cells[cell2->next_index]))), i++)
	  if (count != i &&
 	      gc_cache_hash (NULL, STORE_VOID_IN_LISP (&cell->gcvm))
              == gc_cache_hash (NULL, STORE_VOID_IN_LISP (&cell2->gcvm)))
 	    stderr_out ("\tHASH COLLISION with cell %u\n", i);
	stderr_out ("\tmask:       %8lx\n", cell->gcvm.mask);

	if (flags & DGCCFLAG_CELL_DETAILS)
	  {
#define FROB(field) do {						\
  if ((int)cell->gcvm.gcv.field != (~0))				\
    stderr_out ("\t%-12s%8x\n", #field ":", (int)cell->gcvm.gcv.field);	\
} while (0)
	    FROB (function);
	    FROB (plane_mask);
	    FROB (foreground);
	    FROB (background);
	    FROB (line_width);
	    FROB (line_style);
	    FROB (cap_style);
	    FROB (join_style);
	    FROB (fill_style);
	    FROB (fill_rule);
	    FROB (arc_mode);
	    FROB (tile);
	    FROB (stipple);
	    FROB (ts_x_origin);
	    FROB (ts_y_origin);
	    FROB (font);
	    FROB (subwindow_mode);
	    FROB (graphics_exposures);
	    FROB (clip_x_origin);
	    FROB (clip_y_origin);
	    FROB (clip_mask);
	    FROB (dash_offset);
#undef FROB
	  }

	count++;
	if (cell->next_index != -1 && cell == cache->tail)
	  stderr_out ("\nERROR!  tail is here!\n\n");
	else if (cell->next_index == -1 && cell != cache->tail)
	  stderr_out ("\nERROR!  tail is not at the end\n\n");
	cell = (cell->next_index == -1) ? NULL : 
          &(cache->cells[cell->next_index]);
      }	/* while (cell) */

  if (count != cache->count)
    stderr_out ("\nERROR!  count should be %u\n\n", cache->count);
}

#endif /* DEBUG_XEMACS */
