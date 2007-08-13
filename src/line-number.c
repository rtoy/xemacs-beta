/* Line number cache routines.
   Copyright (C) 1997 Free Software Foundation, Inc.

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

/* To calculate the line numbers, redisplay must count the newlines
   from a known position.  This used to be BUF_BEGV, but this made the
   redisplay extremely slow for large buffers, because Emacs must
   rescan the whole buffer at each redisplay, just to count the
   newlines.

   To make line numbering efficient, we maintain a simple-minded
   cache.  Each buffer contains a small ring of known positions, each
   element of the ring being a Lisp_Object -- either nil or a cons of
   a buffer position and the line number (beginning with 0).

   When calculating the line numbers, this cache is consulted if it
   would otherwise take too much time to count the newlines in the
   buffer (see the comment to window_line_number.)

   Insertion and deletions that contain/delete newlines invalidate the
   cached positions after the insertion point.  This guarantees
   relatively fast line numbers caching (even in buffers where point
   moves a lot), and low memory usage.

   NOTE: line-number cache should not be confused with line-start
   cache.  Line-start cache (a part of redisplay) works with the
   display lines, whereas this works with the buffer lines (literally
   counting the newlines).  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "insdel.h"

#include "line-number.h"


/* #### The following three values could use some tweaking, to get the
   best performance.  */

/* Size of the ring.  The current code expects this to be a small
   number.  If you make it much bigger, you should probably tr yto
   optimize the various routines to keep it sorted. */
#define LINE_NUMBER_RING_SIZE 8

/* How much traversal has to be exceeded for two points to be
   considered "far" from each other.  When two points are far, cache
   will be used.  You can set this to a small value for debugging
   purposes.  */
#define LINE_NUMBER_FAR 16384

/* How large a string has to be to give up searching it for newlines,
   before change. */
#define LINE_NUMBER_LARGE_STRING 256

/* To be used only when you *know* the cache has been allocated!  */
#define LINE_NUMBER_RING(b) (XCAR ((b)->line_number_cache))
#define LINE_NUMBER_BEGV(b) (XCDR ((b)->line_number_cache))


/* Initialize the cache.  Cache is (in pseudo-BNF):

   CACHE		= nil | INITIALIZED-CACHE
   INITITIALIZED-CACHE	= cons (RING, BEGV-LINE)
   RING			= vector (*RING-ELEMENT)
   RING-ELEMENT		= nil | RING-PAIR
   RING-PAIR		= cons (marker, integer)
   BEGV-LINE		= integer

   Line number cache should never, ever, be visible to Lisp (because
   destructively modifying its elements can cause crashes.)  Debug it
   using debug_print (current_buffer->last_number_cache).  */
static void
allocate_line_number_cache (struct buffer *b)
{
  b->line_number_cache = Fcons (make_vector (LINE_NUMBER_RING_SIZE, Qnil),
				Qzero);
  narrow_line_number_cache (b);
}

/* Update line_number_begv.  Do it only if the line number cache is
   already initialized.  */
void
narrow_line_number_cache (struct buffer *b)
{
  EMACS_INT lots = 999999999, shortage;

  if (NILP (b->line_number_cache))
    return;
  /* Optimization: if BUF_BEG == BUF_BEGV (as is the case after Fwiden
     and save_restriction_restore), don't bother calling scan_buffer.  */
  if (BUF_BEG (b) == BUF_BEGV (b))
    {
      LINE_NUMBER_BEGV (b) = Qzero;
      return;
    }
  /* Count the newlines between beginning of buffer and beginning of
     the visible portion of the buffer.  */
  scan_buffer (b, '\n', BUF_BEG (b), BUF_BEGV (b), lots,
	       (int *)&shortage, 0);
  LINE_NUMBER_BEGV (b) = make_int (lots - shortage);
}

/* Invalidate the line number cache positions that lie after POS. */
static void
invalidate_line_number_cache (struct buffer *b, Bufpos pos)
{
  EMACS_INT i, j;
  Lisp_Object *ring = XVECTOR_DATA (LINE_NUMBER_RING (b));
  Lisp_Object lisp_buffer = make_buffer (b);

  for (i = 0; i < LINE_NUMBER_RING_SIZE; i++)
    {
      if (!CONSP (ring[i]))
	break;
      if (marker_position (XCAR (ring[i])) > pos)
	{
	  /* Get the marker out of the way.  */
	  Fset_marker (XCAR (ring[i]), Qnil, lisp_buffer);
	  /* ...and shift the ring elements, up to the first nil.  */
	  for (j = i; !NILP (ring[j]) && j < LINE_NUMBER_RING_SIZE - 1; j++)
	    ring[j] = ring[j + 1];
	  ring[j] = Qnil;
	  /* Must reevaluate the thing at position i. */
	  i--;
	}
    }
}

/* Invalidate the cache positions after POS, if the string to be
   inserted contains a newline.  If the string is too large (larger
   than LINE_NUMBER_LARGE_STRING), invalidate the cache positions
   after POS without prior search.

   This will do nothing, if cache is uninitialized.  */
void
insert_invalidate_line_number_cache (struct buffer *b, Bufpos pos,
				     CONST Bufbyte *nonreloc, Bytecount length)
{
  if (NILP (b->line_number_cache))
    return;

  if (length > LINE_NUMBER_LARGE_STRING
      ||
      /* We could also count how many newlines are in the string, and
         update the cache accordingly, but it would be too much work
         for too little gain. */
      memchr ((void *)nonreloc, '\n', (size_t) length))
    invalidate_line_number_cache (b, pos);
}

/* Invalidate the cache positions after FROM, if the region to be
   deleted contains a newline.  If the region is too large (larger
   than LINE_NUMBER_LARGE_STRING), invalidate the cache positions
   after FROM without prior search.

   This will do nothing, if cache is uninitialized.  */
void
delete_invalidate_line_number_cache (struct buffer *b, Bufpos from, Bufpos to)
{
  if (NILP (b->line_number_cache))
    return;

  if ((to - from) > LINE_NUMBER_LARGE_STRING)
    invalidate_line_number_cache (b, from);
  else
    {
      int shortage;
      scan_buffer (b, '\n', from, to, 1, &shortage, 0);
      if (!shortage)
	/* The same as above applies. */
	invalidate_line_number_cache (b, from);
    }
}


/* Get the nearest known position we know the line number of
   (i.e. BUF_BEGV, and cached positions).  The return position will be
   either closer than BEG, or BEG.

   *LINE should be initialized to the line number of BEG (normally,
   BEG will be BUF_BEGV, and *LINE will be XINT (LINE_NUMBER_BEGV).
   This will initialize the cache, if necessary.  */
static void
get_nearest_line_number (struct buffer *b, Bufpos *beg, Bufpos pos,
			 EMACS_INT *line)
{
  Lisp_Object *ring = XVECTOR_DATA (LINE_NUMBER_RING (b));
  EMACS_INT i;
  Charcount length, howfar;
  Bufpos newpos;

  length = pos - *beg;
  if (length < 0)
    length = -length;

  /* Look for the nearest match. */
  for (i = 0; i < LINE_NUMBER_RING_SIZE; i++)
    {
      if (!CONSP (ring[i]))
	break;
      newpos = marker_position (XCAR (ring[i]));
      howfar = newpos - pos;
      if (howfar < 0)
	howfar = -howfar;
      if (howfar < length)
	{
	  length = howfar;
	  *beg = newpos;
	  *line = XINT (XCDR (ring[i]));
	}
    }
}

/* Add a (pos, line) pair to the ring, and rotate it. */
static void
add_line_number (struct buffer *b, Bufpos pos, int line)
{
  Lisp_Object *ring = XVECTOR_DATA (LINE_NUMBER_RING (b));
  Lisp_Object marker;
  int i;

  for (i = LINE_NUMBER_RING_SIZE - 1; i > 0; i--)
    ring[i] = ring[i - 1];
  marker = Fmake_marker ();
  Fset_marker (marker, make_int (pos), make_buffer (b));
  ring[0] = Fcons (marker, make_int (line));
}

/* Calculate the buffer line number.  If CACHEP is non-zero,
   initialize the line-number cache for future use.  The line number
   of the first line is 0.  If narrowing is in effect, count the lines
   from the beginning of the visible portion of the buffer.

   The cache works as follows: To calculate the line number, we need
   two positions: position of point (POS) and the position from which
   to count newlines (BEG).  We start by setting BEG to BUF_BEGV.  If
   this would require too much searching (i.e. pos - BUF_BEGV >
   LINE_NUMBER_FAR), try to find a closer position in the ring.  If it
   is found, use that position for BEG, and increment the line number
   appropriately.

   If the calculation (with or without the cache lookup) required more
   than LINE_NUMBER_FAR bytes of traversal, update the cache.  */
EMACS_INT
buffer_line_number (struct buffer *b, Bufpos pos, int cachep)
{
  Bufpos beg = BUF_BEGV (b);
  EMACS_INT cached_lines = 0;
  EMACS_INT lots = 999999999;
  EMACS_INT shortage, line;

  if (cachep)
    {
      if (NILP (b->line_number_cache))
	allocate_line_number_cache (b);
      cached_lines = XINT (LINE_NUMBER_BEGV (b));
      get_nearest_line_number (b, &beg, pos, &cached_lines);
    }

  scan_buffer (b, '\n', beg, pos, pos > beg ? lots : -lots,
	       (int *)&shortage, 0);

  line = lots - shortage;
  if (beg > pos)
    line = -line;
  line += cached_lines;

  if (cachep)
    {
      /* If too far, update the cache. */
      if ((pos > beg ? pos - beg : beg - pos) > LINE_NUMBER_FAR)
	add_line_number (b, pos, line);
      /* Account for narrowing.  If CACHEP is nil, this is
	 unnecessary, because we counted from BUF_BEGV anyway.  */
      if (cachep)
	line -= XINT (LINE_NUMBER_BEGV (b));
    }

  return line;
}

