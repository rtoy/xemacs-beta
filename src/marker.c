/* Markers: examining, setting and killing.
   Copyright (C) 1985, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 2002, 2010 Ben Wing.

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

/* Synched up with: FSF 19.30. */

/* This file has been Mule-ized. */

/* Note that markers are currently kept in an unordered list.
   This means that marker operations may be inefficient if
   there are a bunch of markers in the buffer.  This probably
   won't have a significant impact on redisplay (which uses
   markers), but if it does, it wouldn't be too hard to change
   to an ordered gap array. (Just copy the code from extents.c.)
   */

#include <config.h>
#include "lisp.h"

#include "buffer.h"

static Lisp_Object
mark_marker (Lisp_Object obj)
{
  Lisp_Marker *marker = XMARKER (obj);
  Lisp_Object buf;
  /* DO NOT mark through the marker's chain.
     The buffer's markers chain does not preserve markers from gc;
     Instead, markers are removed from the chain when they are freed
     by gc.
   */
  if (!marker->buffer)
    return (Qnil);

  buf = wrap_buffer (marker->buffer);
  return (buf);
}

static void
print_marker (Lisp_Object obj, Lisp_Object printcharfun,
	      int UNUSED (escapeflag))
{
  Lisp_Marker *marker = XMARKER (obj);

  if (print_readably)
    printing_unreadable_object_fmt ("#<marker 0x%x>", LISP_OBJECT_UID (obj));

  write_ascstring (printcharfun, GETTEXT ("#<marker "));
  if (!marker->buffer)
    write_ascstring (printcharfun, GETTEXT ("in no buffer"));
  else
    {
      write_fmt_string (printcharfun, "at %ld in ",
			(long) marker_position (obj));
      print_internal (marker->buffer->name, printcharfun, 0);
    }
  if (marker->insertion_type)
    write_ascstring (printcharfun, " insertion-type=t");
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static int
marker_equal (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth),
	      int UNUSED (foldcase))
{
  Lisp_Marker *marker1 = XMARKER (obj1);
  Lisp_Marker *marker2 = XMARKER (obj2);

  return ((marker1->buffer == marker2->buffer) &&
	  (marker1->membpos == marker2->membpos ||
	  /* All markers pointing nowhere are equal */
	   !marker1->buffer));
}

static Hashcode
marker_hash (Lisp_Object obj, int UNUSED (depth), Boolint UNUSED (equalp))
{
  Hashcode hash = (Hashcode) XMARKER (obj)->buffer;
  if (hash)
    hash = HASH2 (hash, XMARKER (obj)->membpos);
  return hash;
}

static const struct memory_description marker_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Marker, next), 0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof (Lisp_Marker, prev), 0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof (Lisp_Marker, buffer) },
  { XD_END }
};

#ifdef NEW_GC
static void
finalize_marker (Lisp_Object obj)
{
  unchain_marker (obj);
}
#endif /* NEW_GC */

DEFINE_DUMPABLE_FROB_BLOCK_LISP_OBJECT ("marker", marker,
					mark_marker, print_marker,
					IF_NEW_GC (finalize_marker),
					marker_equal, marker_hash,
					marker_description, Lisp_Marker);

/* Operations on markers. */

DEFUN ("marker-buffer", Fmarker_buffer, 1, 1, 0, /*
Return the buffer that MARKER points into, or nil if none.
Return nil if MARKER points into a dead buffer or doesn't point anywhere.
*/
       (marker))
{
  struct buffer *buf;
  CHECK_MARKER (marker);
  /* Return marker's buffer only if it is not dead.  */
  if ((buf = XMARKER (marker)->buffer) && BUFFER_LIVE_P (buf))
    {
      return wrap_buffer (buf);
    }
  return Qnil;
}

DEFUN ("marker-position", Fmarker_position, 1, 1, 0, /*
Return the buffer position to which MARKER points, as a fixnum.

Return `nil' if marker doesn't point anywhere.

Usually there is no need to call this function; if you are using
marker positions in arithmetic or comparing them, markers are
converted automatically as needed. Markers are stored internally as
byte, not character positions, so in the worst case scenario
`marker-position' is O(N) on the (possibly large) position in the
buffer, and much of the time the C code can avoid this entirely given
its knowledge of the relationship between byte and character
positions.

One of the few use cases for this function is when saving a buffer
offset to an external file, when the integer value does need to be
generated.
*/
       (marker))
{
  CHECK_MARKER (marker);
  return XMARKER (marker)->buffer ? make_fixnum (marker_position (marker)) : Qnil;
}

#if 0 /* useful debugging function */

static void
check_marker_circularities (struct buffer *buf)
{
  Lisp_Marker *tortoise, *hare;

  tortoise = BUF_MARKERS (buf);
  hare = tortoise;

  if (!tortoise)
    return;

  while (1)
    {
      assert (hare->buffer == buf);
      hare = hare->next;
      if (!hare)
        return;
      assert (hare->buffer == buf);
      hare = hare->next;
      if (!hare)
        return;
      tortoise = tortoise->next;
      assert (tortoise != hare);
    }
}

#endif

static Bytebpos
fixup_set_marker_args (Lisp_Object marker, Lisp_Object position,
                       Bytebpos byteno, Lisp_Object buffer,
                       Boolint restricted_p, struct buffer **buffer_out,
                       Boolint *pointp_out)
{
  struct buffer *b;
  Boolint point_p;
  Charbpos charno;

  CHECK_MARKER (marker);
  point_p = *pointp_out = POINT_MARKER_P (marker);

  /* If position is nil or a marker that points nowhere, make this marker
     point nowhere. */
  if ((NILP (position) && -1 == byteno) ||
      (MARKERP (position) && !XMARKER (position)->buffer))
    {
      if (point_p)
        {
          invalid_operation ("Can't make point-marker point nowhere",
			     marker);
        }

      /* This is a signal that the marker should not point anyhere, none of
	 the other _out variables need to be set. */
      *buffer_out = NULL;
      return -1;
    }

  if (NILP (buffer))
    {
      b = *buffer_out = current_buffer;
    }
  else
    {
      CHECK_BUFFER (buffer);
      b = *buffer_out = XBUFFER (buffer);

      if (!BUFFER_LIVE_P (XBUFFER (buffer)))
        {
	  if (point_p)
            {
              invalid_operation ("Can't move point-marker into killed buffer",
				 marker);
            }

          *buffer_out = NULL;
          return -1;
        }
    }

  if (point_p && XMARKER (marker)->buffer != b)
    {
      invalid_operation ("Can't change buffer of point-marker", marker);
    }

  if (byteno != -1)
    {
      if (restricted_p)
        {
          if (byteno < BYTE_BUF_BEGV (b)) byteno = BYTE_BUF_BEGV (b);
          if (byteno > BYTE_BUF_ZV (b)) byteno = BYTE_BUF_ZV (b);
        }
      else
        {
          if (byteno < BYTE_BUF_BEG (b)) byteno = BYTE_BUF_BEG (b);
          if (byteno > BYTE_BUF_Z (b)) byteno = BYTE_BUF_Z (b);
        }

      return byteno;
    }

  if (MARKERP (position) && XMARKER (position)->buffer == b)
    {
      text_checking_assert (-1 == byteno);
      byteno = membpos_to_bytebpos (b, XMARKER (position)->membpos);

      if (restricted_p)
        {
          if (byteno < BYTE_BUF_BEGV (b)) byteno = BYTE_BUF_BEGV (b);
          if (byteno > BYTE_BUF_ZV (b)) byteno = BYTE_BUF_ZV (b);
        }

      /* No need to do the usual restriction to valid buffer positions, since
         we know POSITION is valid within BUFFER. */

      return byteno;
    }

  CHECK_FIXNUM_COERCE_MARKER (position);
  charno = XFIXNUM (position);
  if (restricted_p)
    {
      if (charno < BUF_BEGV (b)) 
        {
          return BYTE_BUF_BEGV (b);
        }
      if (charno > BUF_ZV (b))
        {
          return BYTE_BUF_ZV (b);
        }
    }
  else
    {
      if (charno < BUF_BEG (b)) 
        {
          return BYTE_BUF_BEG (b);
        }
      if (charno > BUF_Z (b))
        {
          return BYTE_BUF_Z (b);
        }
    }
  
  return charbpos_to_bytebpos (b, charno);
}

static Lisp_Object
set_marker_internal (Lisp_Object marker, Bytebpos byteno, struct buffer *b,
		     Boolint point_p)
{
  Lisp_Marker *m;

  m = XMARKER (marker);

  if (NULL == b)
    {
      /* Make this marker point nowhere. */
      if (m->buffer)
        {
          unchain_marker (marker);
        }

      return marker;
    }

  if (point_p)
    {
      BYTE_BUF_SET_PT (b, byteno);	/* this will move the marker */
    }
  else
    {
      m->membpos = bytebpos_to_membpos (b, byteno);
    }

  if (m->buffer != b)
    {
      if (m->buffer != 0)
	unchain_marker (marker);
      m->buffer = b;
      marker_next (m) = BUF_MARKERS (b);
      marker_prev (m) = 0;
      if (BUF_MARKERS (b))
        marker_prev (BUF_MARKERS (b)) = m;
      BUF_MARKERS (b) = m;
    }

  return marker;
}
 
/* Set the byte position of MARKER, a marker object, to BYTENO in
   BUFFER. MARKER must be a marker object, BUFFER must be a valid buffer, and
   BYTENO must be a valid byte position within BUFFER. MARKER may not be
   BUFFER's point marker. */
Lisp_Object
set_byte_marker_position (Lisp_Object marker, Bytebpos byteno,
			  Lisp_Object buffer)
{
#ifdef ERROR_CHECK_STRUCTURES
  struct buffer *b;
  Boolint point_p = 0;
  Bytebpos old_byteno = byteno;

  /* For the type, range checking. */
  byteno = fixup_set_marker_args (marker, Qnil, byteno, buffer, 0, &b,
                                  &point_p);

  structure_checking_assert (!point_p);
  structure_checking_assert (byteno == old_byteno);
  structure_checking_assert (b == XBUFFER (buffer));
#endif

  return set_marker_internal (marker, byteno, XBUFFER (buffer), 0);
}

DEFUN ("set-marker", Fset_marker, 2, 3, 0, /*
Move MARKER to position POSITION in BUFFER.
POSITION can be a marker, an integer or nil.  If POSITION is an
integer, make MARKER point before the POSITIONth character in BUFFER.
If POSITION is nil, makes MARKER point nowhere.  Then it no longer
slows down editing in any buffer.  If POSITION is less than 1, move
MARKER to the beginning of BUFFER.  If POSITION is greater than the
size of BUFFER, move MARKER to the end of BUFFER.
BUFFER defaults to the current buffer.
If this marker was returned by (point-marker t), then changing its
position moves point.  You cannot change its buffer or make it point
nowhere.
The return value is MARKER.
*/
       (marker, position, buffer))
{
  struct buffer *b;
  Bytebpos byteno = -1;
  Boolint point_p;

  byteno = fixup_set_marker_args (marker, position, -1, buffer, 0, &b,
                                  &point_p);

  return set_marker_internal (marker, byteno, b, point_p);
}

/* This version of Fset_marker won't let the position
   be outside the visible part.  */
Lisp_Object
set_marker_restricted (Lisp_Object marker, Lisp_Object position,
		       Lisp_Object buffer)
{
  struct buffer *b;
  Bytebpos byteno = -1;
  Boolint point_p;

  byteno = fixup_set_marker_args (marker, position, -1, buffer, 1, &b,
                                  &point_p);
  return set_marker_internal (marker, byteno, b, point_p);
}

/* This is called during garbage collection,
   so we must be careful to ignore and preserve mark bits,
   including those in chain fields of markers.  */

void
unchain_marker (Lisp_Object m)
{
  Lisp_Marker *marker = XMARKER (m);
  struct buffer *b = marker->buffer;

  if (b == 0)
    return;

#ifdef ERROR_CHECK_STRUCTURES
  assert (BUFFER_LIVE_P (b));
#endif

  if (marker_next (marker))
    marker_prev (marker_next (marker)) = marker_prev (marker);
  if (marker_prev (marker))
    marker_next (marker_prev (marker)) = marker_next (marker);
  else
    BUF_MARKERS (b) = marker_next (marker);

#ifdef ERROR_CHECK_STRUCTURES
  assert (marker != XMARKER (b->point_marker));
#endif

  marker->buffer = 0;
}

Bytebpos
byte_marker_position (Lisp_Object marker)
{
  Lisp_Marker *m = XMARKER (marker);
  struct buffer *buf = m->buffer;
  Bytebpos pos;

  if (!buf)
    invalid_argument ("Marker does not point anywhere", Qunbound);

  /* FSF claims that marker indices could end up denormalized, i.e.
     in the gap.  This is way bogus if it ever happens, and means
     something fucked up elsewhere.  Since I've overhauled all this
     shit, I don't think this can happen.  In any case, the following
     macro has an assert() in it that will catch these denormalized
     positions. */
  pos = membpos_to_bytebpos (buf, m->membpos);

  return pos;
}

Charbpos
marker_position (Lisp_Object marker)
{
  struct buffer *buf = XMARKER (marker)->buffer;

  if (!buf)
    invalid_argument ("Marker does not point anywhere", Qunbound);

  return bytebpos_to_charbpos (buf, byte_marker_position (marker));
}

static Lisp_Object
copy_marker_1 (Lisp_Object marker, Lisp_Object type, int noseeum)
{
  REGISTER Lisp_Object new_;

  while (1)
    {
      if (FIXNUMP (marker) || MARKERP (marker))
	{
	  if (noseeum)
	    new_ = noseeum_make_marker ();
	  else
	    new_ = Fmake_marker ();
	  Fset_marker (new_, marker,
		       (MARKERP (marker) ? Fmarker_buffer (marker) : Qnil));
	  XMARKER (new_)->insertion_type = !NILP (type);
	  return new_;
	}
      else
	marker = wrong_type_argument (Qinteger_or_marker_p, marker);
    }

  RETURN_NOT_REACHED (Qnil); /* not reached */
}

DEFUN ("copy-marker", Fcopy_marker, 1, 2, 0, /*
Return a new marker pointing at the same place as MARKER-OR-INTEGER.
If MARKER-OR-INTEGER is an integer, return a new marker pointing
at that position in the current buffer.
Optional argument MARKER-TYPE specifies the insertion type of the new
marker; see `marker-insertion-type'.
*/
       (marker_or_integer, marker_type))
{
  return copy_marker_1 (marker_or_integer, marker_type, 0);
}

Lisp_Object
noseeum_copy_marker (Lisp_Object marker, Lisp_Object marker_type)
{
  return copy_marker_1 (marker, marker_type, 1);
}

DEFUN ("marker-insertion-type", Fmarker_insertion_type, 1, 1, 0, /*
Return insertion type of MARKER: t if it stays after inserted text.
nil means the marker stays before text inserted there.
*/
       (marker))
{
  CHECK_MARKER (marker);
  return XMARKER (marker)->insertion_type ? Qt : Qnil;
}

DEFUN ("set-marker-insertion-type", Fset_marker_insertion_type, 2, 2, 0, /*
Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it.
*/
       (marker, type))
{
  CHECK_MARKER (marker);

  XMARKER (marker)->insertion_type = ! NILP (type);
  return type;
}

/* #### What is the possible use of this?  It looks quite useless to
   me, because there is no way to find *which* markers are positioned
   at POSITION.  Additional bogosity bonus: (buffer-has-markers-at
   (point)) will always return t because of the `point-marker'.  The
   same goes for the position of mark.  Bletch!

   Someone should discuss this with Stallman, but I don't have the
   stomach.  In fact, this function sucks so badly that I'm disabling
   it by default (although I've debugged it).  If you want to use it,
   use extents instead.  --hniksic */
#if 0
DEFUN ("buffer-has-markers-at", Fbuffer_has_markers_at, 1, 1, 0, /*
Return t if there are markers pointing at POSITION in the current buffer.
*/
       (position))
{
  Lisp_Marker *marker;
  Membpos pos;

  /* A small optimization trick: convert POS to membpos now, rather
     than converting every marker's memory index to charbpos.  */
  pos = bytebpos_to_membpos (current_buffer,
			  get_buffer_pos_byte (current_buffer, position,
					       GB_COERCE_RANGE));

  for (marker = BUF_MARKERS (current_buffer);
       marker;
       marker = marker_next (marker))
    {
      /* We use marker->membpos, so we don't have to go through the
         unwieldy operation of creating a Lisp_Object for
         marker_position() every time around.  */
      if (marker->membpos == pos)
	return Qt;
    }

  return Qnil;
}
#endif /* 0 */

#ifdef MEMORY_USAGE_STATS

Bytecount
compute_buffer_marker_usage (struct buffer *b)
{
  Lisp_Marker *m;
  Bytecount total = 0;

  for (m = BUF_MARKERS (b); m; m = m->next)
    total += lisp_object_memory_usage (wrap_marker (m));
  return total;
}

#endif /* MEMORY_USAGE_STATS */


void
syms_of_marker (void)
{
  INIT_LISP_OBJECT (marker);

  DEFSUBR (Fmarker_position);
  DEFSUBR (Fmarker_buffer);
  DEFSUBR (Fset_marker);
  DEFSUBR (Fcopy_marker);
  DEFSUBR (Fmarker_insertion_type);
  DEFSUBR (Fset_marker_insertion_type);
#if 0 /* FSFmacs crock */
  DEFSUBR (Fbuffer_has_markers_at);
#endif
}

void
init_buffer_markers (struct buffer *b)
{
  Lisp_Object buf = wrap_buffer (b);

  b->mark = Fmake_marker ();
  BUF_MARKERS (b) = 0;
  b->point_marker = Fmake_marker ();
  Fset_marker (b->point_marker,
	       /* For indirect buffers, point is already set.  */
	       b->base_buffer ? make_fixnum (BUF_PT (b)) : Qone,
	       buf);
}

void
uninit_buffer_markers (struct buffer *b)
{
  /* Unchain all markers of this buffer
     and leave them pointing nowhere.  */
  REGISTER Lisp_Marker *m, *next;
  for (m = BUF_MARKERS (b); m; m = next)
    {
      m->buffer = 0;
      next = marker_next (m);
      marker_next (m) = 0;
      marker_prev (m) = 0;
    }
  BUF_MARKERS (b) = 0;
}
