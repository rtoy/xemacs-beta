/* Buffer insertion/deletion and gap motion for XEmacs.
   Copyright (C) 1985, 1986, 1991, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30.  Diverges significantly. */

/* This file has been Mule-ized. */

/* Overhauled by Ben Wing, December 1994, for Mule implementation. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "frame.h"
#include "extents.h"
#include "insdel.h"
#include "lstream.h"
#include "redisplay.h"
#include "line-number.h"

/* Various macros modelled along the lines of those in buffer.h.
   Purposefully omitted from buffer.h because files other than this
   one should not be using them. */

/* Address of beginning of buffer.  This is an lvalue because
   BUFFER_ALLOC needs it to be. */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Set the address of beginning of buffer. */
#define SET_BUF_BEG_ADDR(buf, addr) do { (buf)->text->beg = (addr); } while (0)

/* Gap size.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size + 0)
#define BUF_END_GAP_SIZE(buf) ((buf)->text->end_gap_size + 0)
/* Set gap size.  */
#define SET_BUF_GAP_SIZE(buf, value) \
  do { (buf)->text->gap_size = (value); } while (0)
#define SET_BUF_END_GAP_SIZE(buf, value) \
  do { (buf)->text->end_gap_size = (value); } while (0)

/* Gap location.  */
#define BI_BUF_GPT(buf) ((buf)->text->gpt + 0)
#define BUF_GPT_ADDR(buf) (BUF_BEG_ADDR (buf) + BI_BUF_GPT (buf) - 1)

/* Set gap location.  */
#define SET_BI_BUF_GPT(buf, value) do { (buf)->text->gpt = (value); } while (0)

/* Set end of buffer.  */
#define SET_BOTH_BUF_Z(buf, val, bival)		\
do						\
{						\
  (buf)->text->z = (bival);			\
  (buf)->text->bufz = (val);			\
} while (0)

/* Under Mule, we maintain two sentinels in the buffer: one at the
   beginning of the gap, and one at the end of the buffer.  This
   allows us to move forward, examining bytes looking for the
   end of a character, and not worry about running off the end.
   We do not need corresponding sentinels when moving backwards
   because we do not have to look past the beginning of a character
   to find the beginning of the character.

   Every time we change the beginning of the gap, we have to
   call SET_GAP_SENTINEL().

   Every time we change the total size (characters plus gap)
   of the buffer, we have to call SET_END_SENTINEL().
 */


#ifdef MULE
# define GAP_CAN_HOLD_SIZE_P(buf, len) (BUF_GAP_SIZE (buf) >= (len) + 1)
# define SET_GAP_SENTINEL(buf) (*BUF_GPT_ADDR (buf) = 0)
# define BUF_END_SENTINEL_SIZE 1
# define SET_END_SENTINEL(buf) \
  (*(BUF_BEG_ADDR (buf) + BUF_GAP_SIZE (buf) + BI_BUF_Z (buf) - 1) = 0)
#else
# define GAP_CAN_HOLD_SIZE_P(buf, len) (BUF_GAP_SIZE (buf) >= (len))
# define SET_GAP_SENTINEL(buf)
# define BUF_END_SENTINEL_SIZE 0
# define SET_END_SENTINEL(buf)
#endif


/************************************************************************/
/*                     point and marker adjustment                      */
/************************************************************************/

/* just_set_point() is the only place `PT' is an lvalue in all of emacs.
   This function is called from set_buffer_point(), which is the function
   that the SET_PT and BUF_SET_PT macros expand into, and from the
   routines below that insert and delete text. (This is in cases where
   the point marker logically doesn't move but PT (being a byte index)
   needs to get adjusted.) */

/* Set point to a specified value.  This is used only when the value
   of point changes due to an insert or delete; it does not represent
   a conceptual change in point as a marker.  In particular, point is
   not crossing any interval boundaries, so there's no need to use the
   usual SET_PT macro.  In fact it would be incorrect to do so, because
   either the old or the new value of point is out of synch with the
   current set of intervals.  */

/* This gets called more than enough to make the function call
   overhead a significant factor so we've turned it into a macro. */
#define JUST_SET_POINT(buf, charbpos, ind)	\
do						\
{						\
  buf->bufpt = (charbpos);			\
  buf->pt = (ind);				\
} while (0)

/* Set a buffer's point. */

void
set_buffer_point (struct buffer *buf, Charbpos charbpos, Bytebpos bytpos)
{
  assert (bytpos >= BI_BUF_BEGV (buf) && bytpos <= BI_BUF_ZV (buf));
  if (bytpos == BI_BUF_PT (buf))
    return;
  JUST_SET_POINT (buf, charbpos, bytpos);
  MARK_POINT_CHANGED;
  assert (MARKERP (buf->point_marker));
  XMARKER (buf->point_marker)->membpos =
    bytebpos_to_membpos (buf, bytpos);

  /* FSF makes sure that PT is not being set within invisible text.
     However, this is the wrong place for that check.  The check
     should happen only at the next redisplay. */

  /* Some old coder said:

     "If there were to be hooks which were run when point entered/left an
     extent, this would be the place to put them.

     However, it's probably the case that such hooks should be implemented
     using a post-command-hook instead, to avoid running the hooks as a
     result of intermediate motion inside of save-excursions, for example."

     I definitely agree with this.  PT gets moved all over the place
     and it would be a Bad Thing for any hooks to get called, both for
     the reason above and because many callers are not prepared for
     a GC within this function. --ben
   */
}

/* Do the correct marker-like adjustment on MPOS (see below).  FROM, TO,
   and AMOUNT are as in adjust_markers().  If MPOS doesn't need to be
   adjusted, nothing will happen. */
Membpos
do_marker_adjustment (Membpos mpos, Membpos from,
		      Membpos to, Bytecount amount)
{
  if (amount > 0)
    {
      if (mpos > to && mpos < to + amount)
	mpos = to + amount;
    }
  else
    {
      if (mpos > from + amount && mpos <= from)
	mpos = from + amount;
    }
  if (mpos > from && mpos <= to)
    mpos += amount;
  return mpos;
}

/* Do the following:

   (1) Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).

   (2) Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.

   This function is called in two different cases: when a region of
   characters adjacent to the gap is moved, causing the gap to shift
   to the other side of the region (in this case, `from' and `to'
   point to the old position of the region and there should be no
   markers affected by (2) because they would be inside the gap),
   or when a region of characters adjacent to the gap is wiped out,
   causing the gap to increase to include the region (in this case,
   `from' and `to' are the same, both pointing to the boundary
   between the gap and the deleted region, and there are no markers
   affected by (1)).

   The reason for the use of exclusive and inclusive is that markers at
   the gap always sit at the beginning, not at the end.
*/

static void
adjust_markers (struct buffer *buf, Membpos from, Membpos to,
		Bytecount amount)
{
  Lisp_Marker *m;

  for (m = BUF_MARKERS (buf); m; m = marker_next (m))
    m->membpos = do_marker_adjustment (m->membpos, from, to, amount);
}

/* Adjust markers whose insertion-type is t
   for an insertion of AMOUNT characters at POS.  */

static void
adjust_markers_for_insert (struct buffer *buf, Membpos ind, Bytecount amount)
{
  Lisp_Marker *m;

  for (m = BUF_MARKERS (buf); m; m = marker_next (m))
    {
      if (m->insertion_type && m->membpos == ind)
	m->membpos += amount;
    }
}


/************************************************************************/
/*                  Routines for dealing with the gap                   */
/************************************************************************/

/* maximum amount of memory moved in a single chunk.  Increasing this
   value improves gap-motion efficiency but decreases QUIT responsiveness
   time.  Was 32000 but today's processors are faster and files are
   bigger.  --ben */
#define GAP_MOVE_CHUNK 300000

/* Move the gap to POS, which is less than the current GPT. */

static void
gap_left (struct buffer *buf, Bytebpos pos)
{
  Intbyte *to, *from;
  Bytecount i;
  Bytebpos new_s1;
  struct buffer *mbuf;
  Lisp_Object bufcons;

  from = BUF_GPT_ADDR (buf);
  to = from + BUF_GAP_SIZE (buf);
  new_s1 = BI_BUF_GPT (buf);

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most GAP_MOVE_CHUNK chars before checking again for a quit. */
      if (i > GAP_MOVE_CHUNK)
	i = GAP_MOVE_CHUNK;

      if (i >= 128)
	{
	  new_s1 -= i;
	  from   -= i;
	  to     -= i;
	  memmove (to, from, i);
	}
      else
	{
	  new_s1 -= i;
	  while (--i >= 0)
	    *--to = *--from;
	}
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      adjust_markers (mbuf, pos, BI_BUF_GPT (mbuf), BUF_GAP_SIZE (mbuf));
    }
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      adjust_extents (wrap_buffer (mbuf), pos, BI_BUF_GPT (mbuf),
		      BUF_GAP_SIZE (mbuf));
    }
  SET_BI_BUF_GPT (buf, pos);
  SET_GAP_SENTINEL (buf);
#ifdef ERROR_CHECK_EXTENTS
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      sledgehammer_extent_check (wrap_buffer (mbuf));
    }
#endif
  QUIT;
}

static void
gap_right (struct buffer *buf, Bytebpos pos)
{
  Intbyte *to, *from;
  Bytecount i;
  Bytebpos new_s1;
  struct buffer *mbuf;
  Lisp_Object bufcons;

  to = BUF_GPT_ADDR (buf);
  from = to + BUF_GAP_SIZE (buf);
  new_s1 = BI_BUF_GPT (buf);

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most GAP_MOVE_CHUNK chars before checking again for a quit. */
      if (i > GAP_MOVE_CHUNK)
	i = GAP_MOVE_CHUNK;

      if (i >= 128)
	{
	  new_s1 += i;
	  memmove (to, from, i);
	  from += i;
	  to   += i;
	}
      else
	{
	  new_s1 += i;
	  while (--i >= 0)
	    *to++ = *from++;
	}
    }

  {
    int gsize = BUF_GAP_SIZE (buf);
    MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
      {
	adjust_markers (mbuf, BI_BUF_GPT (mbuf) + gsize, pos + gsize, - gsize);
      }
    MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
      {
	adjust_extents (wrap_buffer (mbuf), BI_BUF_GPT (mbuf) + gsize,
			pos + gsize, - gsize);
      }
    SET_BI_BUF_GPT (buf, pos);
    SET_GAP_SENTINEL (buf);
#ifdef ERROR_CHECK_EXTENTS
    MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
      {
	sledgehammer_extent_check (wrap_buffer (mbuf));
      }
#endif
  }
  if (pos == BI_BUF_Z (buf))
    {
      /* merge gap with end gap */

      SET_BUF_GAP_SIZE (buf, BUF_GAP_SIZE (buf) + BUF_END_GAP_SIZE (buf));
      SET_BUF_END_GAP_SIZE (buf, 0);
      SET_END_SENTINEL (buf);
    }

  QUIT;
}

/* Move gap to position `pos'.
   Note that this can quit!  */

static void
move_gap (struct buffer *buf, Bytebpos pos)
{
  if (! BUF_BEG_ADDR (buf))
    abort ();
  if (pos < BI_BUF_GPT (buf))
    gap_left (buf, pos);
  else if (pos > BI_BUF_GPT (buf))
    gap_right (buf, pos);
}

/* Merge the end gap into the gap */

static void
merge_gap_with_end_gap (struct buffer *buf)
{
  Lisp_Object tem;
  Bytebpos real_gap_loc;
  Bytecount old_gap_size;
  Bytecount increment;

  increment = BUF_END_GAP_SIZE (buf);
  SET_BUF_END_GAP_SIZE (buf, 0);

  if (increment > 0)
    {
      /* Prevent quitting in move_gap.  */
      tem = Vinhibit_quit;
      Vinhibit_quit = Qt;

      real_gap_loc = BI_BUF_GPT (buf);
      old_gap_size = BUF_GAP_SIZE (buf);

      /* Pretend the end gap is the gap */
      SET_BI_BUF_GPT (buf, BI_BUF_Z (buf) + BUF_GAP_SIZE (buf));
      SET_BUF_GAP_SIZE (buf, increment);

      /* Move the new gap down to be consecutive with the end of the old one.
	 This adjusts the markers properly too.  */
      gap_left (buf, real_gap_loc + old_gap_size);

      /* Now combine the two into one large gap.  */
      SET_BUF_GAP_SIZE (buf, BUF_GAP_SIZE (buf) + old_gap_size);
      SET_BI_BUF_GPT (buf, real_gap_loc);
      SET_GAP_SENTINEL (buf);

      /* We changed the total size of the buffer (including gap),
	 so we need to fix up the end sentinel. */
      SET_END_SENTINEL (buf);

      Vinhibit_quit = tem;
    }
}

/* Make the gap INCREMENT bytes longer.  */

static void
make_gap (struct buffer *buf, Bytecount increment)
{
  Intbyte *result;
  Lisp_Object tem;
  Bytebpos real_gap_loc;
  Bytecount old_gap_size;

  /* If we have to get more space, get enough to last a while.  We use
     a geometric progression that saves on realloc space. */
  increment += 2000 + ((BI_BUF_Z (buf) - BI_BUF_BEG (buf)) / 8);

  if (increment > BUF_END_GAP_SIZE (buf))
    {
      /* Don't allow a buffer size that won't fit in an int
	 even if it will fit in a Lisp integer.
	 That won't work because so many places use `int'.  */

      if (BUF_Z (buf) - BUF_BEG (buf) + BUF_GAP_SIZE (buf) + increment
	  > EMACS_INT_MAX)
	out_of_memory ("Maximum buffer size exceeded", Qunbound);

      result = BUFFER_REALLOC (buf->text->beg,
			       BI_BUF_Z (buf) - BI_BUF_BEG (buf) +
			       BUF_GAP_SIZE (buf) + increment +
			       BUF_END_SENTINEL_SIZE);
      if (result == 0)
	memory_full ();

      SET_BUF_BEG_ADDR (buf, result);
    }
  else
    increment = BUF_END_GAP_SIZE (buf);

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = BI_BUF_GPT (buf);
  old_gap_size = BUF_GAP_SIZE (buf);

  /* Call the newly allocated space a gap at the end of the whole space.  */
  SET_BI_BUF_GPT (buf, BI_BUF_Z (buf) + BUF_GAP_SIZE (buf));
  SET_BUF_GAP_SIZE (buf, increment);

  SET_BUF_END_GAP_SIZE (buf, 0);

  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (buf, real_gap_loc + old_gap_size);

  /* Now combine the two into one large gap.  */
  SET_BUF_GAP_SIZE (buf, BUF_GAP_SIZE (buf) + old_gap_size);
  SET_BI_BUF_GPT (buf, real_gap_loc);
  SET_GAP_SENTINEL (buf);

  /* We changed the total size of the buffer (including gap),
     so we need to fix up the end sentinel. */
  SET_END_SENTINEL (buf);

  Vinhibit_quit = tem;
}


/************************************************************************/
/*                     Before/after-change processing                   */
/************************************************************************/

/* Those magic changes ... */

static void
buffer_signal_changed_region (struct buffer *buf, Charbpos start,
			      Charbpos end)
{
  /* The changed region is recorded as the number of unchanged
     characters from the beginning and from the end of the
     buffer.  This obviates much of the need of shifting the
     region around to compensate for insertions and deletions.
     */
  if (buf->changes->begin_unchanged < 0 ||
      buf->changes->begin_unchanged > start - BUF_BEG (buf))
    buf->changes->begin_unchanged = start - BUF_BEG (buf);
  if (buf->changes->end_unchanged < 0 ||
      buf->changes->end_unchanged > BUF_Z (buf) - end)
    buf->changes->end_unchanged = BUF_Z (buf) - end;
}

void
buffer_extent_signal_changed_region (struct buffer *buf, Charbpos start,
				     Charbpos end)
{
  if (buf->changes->begin_extent_unchanged < 0 ||
      buf->changes->begin_extent_unchanged > start - BUF_BEG (buf))
    buf->changes->begin_extent_unchanged = start - BUF_BEG (buf);
  if (buf->changes->end_extent_unchanged < 0 ||
      buf->changes->end_extent_unchanged > BUF_Z (buf) - end)
    buf->changes->end_extent_unchanged = BUF_Z (buf) - end;
}

void
buffer_reset_changes (struct buffer *buf)
{
  buf->changes->begin_unchanged = -1;
  buf->changes->end_unchanged = -1;
  buf->changes->begin_extent_unchanged = -1;
  buf->changes->end_extent_unchanged = -1;
  buf->changes->newline_was_deleted = 0;
}

static void
signal_after_change (struct buffer *buf, Charbpos start, Charbpos orig_end,
		     Charbpos new_end);


/* Call the after-change-functions according to the changes made so far
   and treat all further changes as single until the outermost
   multiple change exits.  This is called when the outermost multiple
   change exits and when someone is trying to make a change that violates
   the constraints specified in begin_multiple_change(), typically
   when nested multiple-change sessions occur. (There are smarter ways of
   dealing with nested multiple changes, but these rarely occur so there's
   probably no point in it.) */

/* #### This needs to keep track of what actually changed and only
   call the after-change functions on that region. */

static void
cancel_multiple_change (struct buffer *buf)
{
  /* This function can GC */
  /* Call the after-change-functions except when they've already been
     called or when there were no changes made to the buffer at all. */
  if (buf->text->changes->mc_begin != 0 &&
      buf->text->changes->mc_begin_signaled)
    {
      Charbpos real_mc_begin = buf->text->changes->mc_begin;
      buf->text->changes->mc_begin = 0;

      signal_after_change (buf, real_mc_begin, buf->text->changes->mc_orig_end,
			   buf->text->changes->mc_new_end);
    }
  else
    {
      buf->text->changes->mc_begin = 0;
    }
}

/* this is an unwind_protect, to ensure that the after-change-functions
   get called even in a non-local exit. */

static Lisp_Object
multiple_change_finish_up (Lisp_Object buffer)
{
  struct buffer *buf = XBUFFER (buffer);

  /* #### I don't know whether or not it should even be possible to
     get here with a dead buffer (though given how it is called I can
     see how it might be).  In any case, there isn't time before 19.14
     to find out. */
  if (!BUFFER_LIVE_P (buf))
    return Qnil;

  /* This function can GC */
  buf->text->changes->in_multiple_change = 0; /* do this first so that
						 errors in the after-change
						 functions don't mess things
						 up. */
  cancel_multiple_change (buf);
  return Qnil;
}

/* Call this function when you're about to make a number of buffer changes
   that should be considered a single change. (e.g. `replace-match' calls
   this.) You need to specify the START and END of the region that is
   going to be changed so that the before-change-functions are called
   with the correct arguments.  The after-change region is calculated
   automatically, however, and if changes somehow or other happen outside
   of the specified region, that will also be handled correctly.

   begin_multiple_change() returns a number (actually a specpdl depth)
   that you must pass to end_multiple_change() when you are done.

   FSF Emacs 20 implements a similar feature, accessible from Lisp
   through a `combine-after-change-calls' special form, which is
   essentially equivalent to this function.  We should consider
   whether we want to introduce a similar Lisp form.  */

int
begin_multiple_change (struct buffer *buf, Charbpos start, Charbpos end)
{
  /* This function can GC */
  int count = -1;
  if (buf->text->changes->in_multiple_change)
    {
      if (buf->text->changes->mc_begin != 0 &&
	  (start < buf->text->changes->mc_begin ||
	   end > buf->text->changes->mc_new_end))
	cancel_multiple_change (buf);
    }
  else
    {
      Lisp_Object buffer;

      buf->text->changes->mc_begin = start;
      buf->text->changes->mc_orig_end = buf->text->changes->mc_new_end = end;
      buf->text->changes->mc_begin_signaled = 0;
      count = specpdl_depth ();
      buffer = wrap_buffer (buf);
      record_unwind_protect (multiple_change_finish_up, buffer);
    }
  buf->text->changes->in_multiple_change++;
  /* We don't call before-change-functions until signal_before_change()
     is called, in case there is a read-only or other error. */
  return count;
}

void
end_multiple_change (struct buffer *buf, int count)
{
  assert (buf->text->changes->in_multiple_change > 0);
  buf->text->changes->in_multiple_change--;
  if (!buf->text->changes->in_multiple_change)
    unbind_to (count);
}

static int inside_change_hook;

static Lisp_Object
change_function_restore (Lisp_Object buffer)
{
  /* We should first reset the variable and then change the buffer,
     because Fset_buffer() can throw.  */
  inside_change_hook = 0;
  if (XBUFFER (buffer) != current_buffer)
    Fset_buffer (buffer);
  return Qnil;
}

static int in_first_change;

static Lisp_Object
first_change_hook_restore (Lisp_Object buffer)
{
  in_first_change = 0;
  Fset_buffer (buffer);
  return Qnil;
}

/* Signal an initial modification to the buffer.  */

static void
signal_first_change (struct buffer *buf)
{
  /* This function can GC */
  Lisp_Object buffer = wrap_buffer (current_buffer);


  if (!in_first_change)
    {
      if (!NILP (symbol_value_in_buffer (Qfirst_change_hook, buffer)))
	{
	  int speccount = specpdl_depth ();
	  record_unwind_protect (first_change_hook_restore, buffer);
	  set_buffer_internal (buf);
	  in_first_change = 1;
	  run_hook (Qfirst_change_hook);
	  unbind_to (speccount);
	}
    }
}

/* Signal a change to the buffer immediately before it happens.
   START and END are the bounds of the text to be changed. */

static void
signal_before_change (struct buffer *buf, Charbpos start, Charbpos end)
{
  /* This function can GC */
  struct buffer *mbuf;
  Lisp_Object bufcons;

  if (!inside_change_hook)
    {
      Lisp_Object buffer;
      int speccount;

      /* Are we in a multiple-change session? */
      if (buf->text->changes->in_multiple_change &&
	  buf->text->changes->mc_begin != 0)
	{
	  /* If we're violating the constraints of the session,
	     call the after-change-functions as necessary for the
	     changes already made and treat further changes as
	     single. */
	  if (start < buf->text->changes->mc_begin ||
	      end > buf->text->changes->mc_new_end)
	    cancel_multiple_change (buf);
	  /* Do nothing if this is not the first change in the session. */
	  else if (buf->text->changes->mc_begin_signaled)
	    return;
	  else
	    {
	      /* First time through; call the before-change-functions
		 specifying the entire region to be changed. (Note that
		 we didn't call before-change-functions in
		 begin_multiple_change() because the buffer might be
		 read-only, etc.) */
	      start = buf->text->changes->mc_begin;
	      end = buf->text->changes->mc_new_end;
	    }
	}

      /* If buffer is unmodified, run a special hook for that case.  */
      if (BUF_SAVE_MODIFF (buf) >= BUF_MODIFF (buf))
	{
	  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	    {
	      signal_first_change (mbuf);
	    }
	}

      /* Now in any case run the before-change-functions if any.  */
      speccount = specpdl_depth ();
      record_unwind_protect (change_function_restore, Fcurrent_buffer ());
      inside_change_hook = 1;

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  buffer = wrap_buffer (mbuf);
	  if (!NILP (symbol_value_in_buffer (Qbefore_change_functions, buffer))
	      /* Obsolete, for compatibility */
	      || !NILP (symbol_value_in_buffer (Qbefore_change_function, buffer)))
	    {
	      set_buffer_internal (buf);
	      va_run_hook_with_args (Qbefore_change_functions, 2,
				     make_int (start), make_int (end));
	      /* Obsolete, for compatibility */
	      va_run_hook_with_args (Qbefore_change_function, 2,
				     make_int (start), make_int (end));
	    }
	}

      /* Make sure endpoints remain valid.  before-change-functions
	 might have modified the buffer. */
      if (start < BUF_BEGV (buf)) start = BUF_BEGV (buf);
      if (start > BUF_ZV (buf))   start = BUF_ZV (buf);
      if (end < BUF_BEGV (buf)) end = BUF_BEGV (buf);
      if (end > BUF_ZV (buf))   end = BUF_ZV (buf);

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  buffer = wrap_buffer (mbuf);
	  report_extent_modification (buffer, start, end, 0);
	}
      unbind_to (speccount);

      /* Only now do we indicate that the before-change-functions have
	 been called, in case some function throws out. */
      buf->text->changes->mc_begin_signaled = 1;
    }
}

/* Signal a change immediately after it happens.
   START is the charbpos of the start of the changed text.
   ORIG_END is the charbpos of the end of the before-changed text.
   NEW_END is the charbpos of the end of the after-changed text.
 */

static void
signal_after_change (struct buffer *buf, Charbpos start, Charbpos orig_end,
		     Charbpos new_end)
{
  /* This function can GC */
  struct buffer *mbuf;
  Lisp_Object bufcons;

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      /* always do this. */
      buffer_signal_changed_region (mbuf, start, new_end);
    }
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      /* #### This seems inefficient.  Wouldn't it be better to just
         keep one cache per base buffer?  */
      font_lock_maybe_update_syntactic_caches (mbuf, start, orig_end, new_end);
    }

  if (!inside_change_hook)
    {
      Lisp_Object buffer;
      int speccount;

      if (buf->text->changes->in_multiple_change &&
	  buf->text->changes->mc_begin != 0)
	{
	  assert (start >= buf->text->changes->mc_begin &&
		  start <= buf->text->changes->mc_new_end);
	  assert (orig_end >= buf->text->changes->mc_begin &&
		  orig_end <= buf->text->changes->mc_new_end);
	  buf->text->changes->mc_new_end += new_end - orig_end;
	  return; /* after-change-functions signalled when all changes done */
	}

      speccount = specpdl_depth ();
      record_unwind_protect (change_function_restore, Fcurrent_buffer ());
      inside_change_hook = 1;
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  buffer = wrap_buffer (mbuf);

	  if (!NILP (symbol_value_in_buffer (Qafter_change_functions, buffer))
	      /* Obsolete, for compatibility */
	      || !NILP (symbol_value_in_buffer (Qafter_change_function, buffer)))
	    {
	      set_buffer_internal (buf);
	      /* The actual after-change functions take slightly
		 different arguments than what we were passed. */
	      va_run_hook_with_args (Qafter_change_functions, 3,
				     make_int (start), make_int (new_end),
				     make_int (orig_end - start));
	      /* Obsolete, for compatibility */
	      va_run_hook_with_args (Qafter_change_function, 3,
				     make_int (start), make_int (new_end),
				     make_int (orig_end - start));
	    }
	}

      /* Make sure endpoints remain valid.  after-change-functions
	 might have modified the buffer. */
      if (start < BUF_BEGV (buf)) start = BUF_BEGV (buf);
      if (start > BUF_ZV (buf))   start = BUF_ZV (buf);
      if (new_end < BUF_BEGV (buf)) new_end = BUF_BEGV (buf);
      if (new_end > BUF_ZV (buf))   new_end = BUF_ZV (buf);
      if (orig_end < BUF_BEGV (buf)) orig_end = BUF_BEGV (buf);
      if (orig_end > BUF_ZV (buf))   orig_end = BUF_ZV (buf);

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  buffer = wrap_buffer (mbuf);
	  report_extent_modification (buffer, start, new_end, 1);
	}
      unbind_to (speccount); /* sets inside_change_hook back to 0 */
    }
}

/* Call this if you're about to change the region of BUFFER from START
   to END.  This checks the read-only properties of the region, calls
   the necessary modification hooks, and warns the next redisplay that
   it should pay attention to that area.  */

static void
prepare_to_modify_buffer (struct buffer *buf, Charbpos start, Charbpos end,
			  int lockit)
{
  /* This function can GC */
  /* dmoore - This function can also kill the buffer buf, the current
     buffer, and do anything it pleases.  So if you call it, be
     careful. */
  struct buffer *mbuf;
  Lisp_Object buffer, bufcons;
  struct gcpro gcpro1;

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      barf_if_buffer_read_only (mbuf, start, end);
    }

  /* if this is the first modification, see about locking the buffer's
     file */
  buffer = wrap_buffer (buf);
  GCPRO1 (buffer);
  if (!NILP (buf->filename) && lockit &&
      BUF_SAVE_MODIFF (buf) >= BUF_MODIFF (buf))
    {
#ifdef CLASH_DETECTION
      if (!NILP (buf->file_truename))
	/* Make binding buffer-file-name to nil effective.  */
	lock_file (buf->file_truename);
#else 
      /* At least warn if this file has changed on disk since it was visited.*/
      if (NILP (Fverify_visited_file_modtime (buffer))
	  && !NILP (Ffile_exists_p (buf->filename)))
	call1_in_buffer (buf, intern ("ask-user-about-supersession-threat"),
			 buf->filename);
#endif /* not CLASH_DETECTION */
    }
  UNGCPRO;

  /* #### dmoore - is this reasonable in case of buf being killed above? */
  if (!BUFFER_LIVE_P (buf))
    return;

  signal_before_change (buf, start, end);

#ifdef REGION_CACHE_NEEDS_WORK
  if (buf->newline_cache)
    invalidate_region_cache (buf,
                             buf->newline_cache,
                             start - BUF_BEG (buf), BUF_Z (buf) - end);
  if (buf->width_run_cache)
    invalidate_region_cache (buf,
                             buf->width_run_cache,
                             start - BUF_BEG (buf), BUF_Z (buf) - end);
#endif

#if 0 /* FSFmacs */
  Vdeactivate_mark = Qt;
#endif

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      mbuf->point_before_scroll = Qnil;
    }
}


/************************************************************************/
/*                        Insertion of strings                          */
/************************************************************************/

void
fixup_internal_substring (const Intbyte *nonreloc, Lisp_Object reloc,
			  Bytecount offset, Bytecount *len)
{
  assert ((nonreloc && NILP (reloc)) || (!nonreloc && STRINGP (reloc)));

  if (*len < 0)
    {
      if (nonreloc)
	*len = strlen ((const char *) nonreloc) - offset;
      else
	*len = XSTRING_LENGTH (reloc) - offset;
    }
#ifdef ERROR_CHECK_TEXT
  assert (*len >= 0);
  if (STRINGP (reloc))
    {
      assert (offset >= 0 && offset <= XSTRING_LENGTH (reloc));
      assert (offset + *len <= XSTRING_LENGTH (reloc));
    }
#endif
}

/* Insert a string into BUF at Charbpos POS.  The string data comes
   from one of two sources: constant, non-relocatable data (specified
   in NONRELOC), or a Lisp string object (specified in RELOC), which
   is relocatable and may have extent data that needs to be copied
   into the buffer.  OFFSET and LENGTH specify the substring of the
   data that is actually to be inserted.  As a special case, if POS
   is -1, insert the string at point and move point to the end of the
   string.

   Normally, markers at the insertion point end up before the
   inserted string.  If INSDEL_BEFORE_MARKERS is set in flags, however,
   they end up after the string.

   INSDEL_NO_LOCKING is kludgy and is used when insert-file-contents is
   visiting a new file; it inhibits the locking checks normally done
   before modifying a buffer.  Similar checks were already done
   in the higher-level Lisp functions calling insert-file-contents. */

Charcount
buffer_insert_string_1 (struct buffer *buf, Charbpos pos,
			const Intbyte *nonreloc, Lisp_Object reloc,
			Bytecount offset, Bytecount length,
			int flags)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Bytebpos ind;
  Charcount cclen;
  int move_point = 0;
  struct buffer *mbuf;
  Lisp_Object bufcons;

  /* Defensive steps just in case a buffer gets deleted and a calling
     function doesn't notice it. */
  if (!BUFFER_LIVE_P (buf))
    return 0;

  fixup_internal_substring (nonreloc, reloc, offset, &length);

  if (pos == -1)
    {
      pos = BUF_PT (buf);
      move_point = 1;
    }

#ifdef I18N3
  /* #### See the comment in print_internal().  If this buffer is marked
     as translatable, then Fgettext() should be called on obj if it
     is a string. */
#endif

  /* Make sure that point-max won't exceed the size of an emacs int. */
  if ((length + BUF_Z (buf)) > EMACS_INT_MAX)
    out_of_memory ("Maximum buffer size exceeded", Qunbound);

  /* theoretically not necessary -- caller should GCPRO.
     #### buffer_insert_from_buffer_1() doesn't!  */
  GCPRO1 (reloc);

  prepare_to_modify_buffer (buf, pos, pos, !(flags & INSDEL_NO_LOCKING));

  /* Defensive steps in case the before-change-functions fuck around */
  if (!BUFFER_LIVE_P (buf))
    {
      UNGCPRO;
      /* Bad bad pre-change function. */
      return 0;
    }

  /* Make args be valid again.  prepare_to_modify_buffer() might have
     modified the buffer. */
  if (pos < BUF_BEGV (buf))
    pos = BUF_BEGV (buf);
  if (pos > BUF_ZV (buf))
    pos = BUF_ZV (buf);

  ind = charbpos_to_bytebpos (buf, pos);

  /* string may have been relocated up to this point */
  if (STRINGP (reloc))
    {
      cclen = string_offset_byte_to_char_len (reloc, offset, length);
      nonreloc = XSTRING_DATA (reloc);
    }
  else
    cclen = bytecount_to_charcount (nonreloc + offset, length);

  if (ind != BI_BUF_GPT (buf))
    /* #### if debug-on-quit is invoked and the user changes the
       buffer, bad things can happen.  This is a rampant problem
       in Emacs. */
    move_gap (buf, ind); /* may QUIT */
  if (! GAP_CAN_HOLD_SIZE_P (buf, length))
    {
      if (BUF_END_GAP_SIZE (buf) >= length)
	merge_gap_with_end_gap (buf);
      else
	make_gap (buf, length - BUF_GAP_SIZE (buf));
    }

  insert_invalidate_line_number_cache (buf, pos, nonreloc + offset, length);

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      record_insert (mbuf, pos, cclen);
    }

  BUF_MODIFF (buf)++;
  MARK_BUFFERS_CHANGED;

  /* string may have been relocated up to this point */
  if (STRINGP (reloc))
    nonreloc = XSTRING_DATA (reloc);

  memcpy (BUF_GPT_ADDR (buf), nonreloc + offset, length);

  SET_BUF_GAP_SIZE (buf, BUF_GAP_SIZE (buf) - length);
  SET_BI_BUF_GPT (buf, BI_BUF_GPT (buf) + length);
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      SET_BOTH_BUF_ZV (mbuf, BUF_ZV (mbuf) + cclen, BI_BUF_ZV (mbuf) + length);
    }
  SET_BOTH_BUF_Z (buf, BUF_Z (buf) + cclen, BI_BUF_Z (buf) + length);
  SET_GAP_SENTINEL (buf);
  
  
#ifdef MULE
  buffer_mule_signal_inserted_region (buf, pos, length, cclen);
#endif

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      process_extents_for_insertion (wrap_buffer (mbuf), ind, length);
    }

  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      /* We know the gap is at IND so the cast is OK. */
      adjust_markers_for_insert (mbuf, (Membpos) ind, length);
    }

  /* Point logically doesn't move, but may need to be adjusted because
     it's a byte index.  point-marker doesn't change because it's a
     memory index. */
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      if (BI_BUF_PT (mbuf) > ind)
	JUST_SET_POINT (mbuf, BUF_PT (mbuf) + cclen,
			BI_BUF_PT (mbuf) + length);
    }

  /* Well, point might move. */
  if (move_point)
    BI_BUF_SET_PT (buf, ind + length);

  if (STRINGP (reloc))
    {
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  splice_in_string_extents (reloc, mbuf, ind, length, offset);
	}
    }

  if (flags & INSDEL_BEFORE_MARKERS)
    {
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  /* ind - 1 is correct because the FROM argument is exclusive.
	     I formerly used DEC_BYTEBPOS() but that caused problems at the
	     beginning of the buffer. */
	  adjust_markers (mbuf, ind - 1, ind, length);
	}
    }

  signal_after_change (buf, pos, pos, pos + cclen);

  UNGCPRO;

  return cclen;
}


/* The following functions are interfaces onto the above function,
   for inserting particular sorts of data.  In all the functions,
   BUF and POS specify the buffer and location where the insertion is
   to take place. (If POS is -1, text is inserted at point and point
   moves forward past the text.) FLAGS is as above. */

Charcount
buffer_insert_raw_string_1 (struct buffer *buf, Charbpos pos,
			    const Intbyte *nonreloc, Bytecount length,
			    int flags)
{
  /* This function can GC */
  return buffer_insert_string_1 (buf, pos, nonreloc, Qnil, 0, length,
				 flags);
}

Charcount
buffer_insert_lisp_string_1 (struct buffer *buf, Charbpos pos, Lisp_Object str,
			     int flags)
{
  /* This function can GC */
  return buffer_insert_string_1 (buf, pos, 0, str, 0,
				 XSTRING_LENGTH (str),
				 flags);
}

/* Insert the null-terminated string S (in external format). */

Charcount
buffer_insert_c_string_1 (struct buffer *buf, Charbpos pos, const char *s,
			  int flags)
{
  /* This function can GC */
  const char *translated = GETTEXT (s);
  return buffer_insert_string_1 (buf, pos, (const Intbyte *) translated, Qnil,
				 0, strlen (translated), flags);
}

Charcount
buffer_insert_emacs_char_1 (struct buffer *buf, Charbpos pos, Emchar ch,
			    int flags)
{
  /* This function can GC */
  Intbyte str[MAX_EMCHAR_LEN];
  Bytecount len = set_charptr_emchar (str, ch);
  return buffer_insert_string_1 (buf, pos, str, Qnil, 0, len, flags);
}

Charcount
buffer_insert_c_char_1 (struct buffer *buf, Charbpos pos, char c,
			int flags)
{
  /* This function can GC */
  return buffer_insert_emacs_char_1 (buf, pos, (Emchar) (unsigned char) c,
				     flags);
}

Charcount
buffer_insert_from_buffer_1 (struct buffer *buf, Charbpos pos,
			     struct buffer *buf2, Charbpos pos2,
			     Charcount length, int flags)
{
  /* This function can GC */
  Lisp_Object str = make_string_from_buffer (buf2, pos2, length);
  return buffer_insert_string_1 (buf, pos, 0, str, 0,
				 XSTRING_LENGTH (str), flags);
}


/************************************************************************/
/*                        Deletion of ranges                            */
/************************************************************************/

/* Delete characters in buffer from FROM up to (but not including) TO.  */

void
buffer_delete_range (struct buffer *buf, Charbpos from, Charbpos to, int flags)
{
  /* This function can GC */
  Charcount numdel;
  Bytebpos bi_from, bi_to;
  Bytecount bc_numdel;
  EMACS_INT shortage;
  struct buffer *mbuf;
  Lisp_Object bufcons;

  /* Defensive steps just in case a buffer gets deleted and a calling
     function doesn't notice it. */
  if (!BUFFER_LIVE_P (buf))
    return;

  /* Make args be valid */
  if (from < BUF_BEGV (buf))
    from = BUF_BEGV (buf);
  if (to > BUF_ZV (buf))
    to = BUF_ZV (buf);
  if ((numdel = to - from) <= 0)
    return;

  prepare_to_modify_buffer (buf, from, to, !(flags & INSDEL_NO_LOCKING));

  /* Defensive steps in case the before-change-functions fuck around */
  if (!BUFFER_LIVE_P (buf))
    /* Bad bad pre-change function. */
    return;

  /* Make args be valid again.  prepare_to_modify_buffer() might have
     modified the buffer. */
  if (from < BUF_BEGV (buf))
    from = BUF_BEGV (buf);
  if (to > BUF_ZV (buf))
    to = BUF_ZV (buf);
  if ((numdel = to - from) <= 0)
    return;

  /* Redisplay needs to know if a newline was in the deleted region.
     If we've already marked the changed region as having a deleted
     newline there is no use in performing the check. */
  if (!buf->changes->newline_was_deleted)
    {
      scan_buffer (buf, '\n', from, to, 1, &shortage, 1);
      if (!shortage)
	{
	  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	    {
	      mbuf->changes->newline_was_deleted = 1;
	    }
	}
    }

  bi_from = charbpos_to_bytebpos (buf, from);
  bi_to = charbpos_to_bytebpos (buf, to);
  bc_numdel = bi_to - bi_from;

  delete_invalidate_line_number_cache (buf, from, to);

  if (to == BUF_Z (buf) &&
      bi_from > BI_BUF_GPT (buf))
    {
      /* avoid moving the gap just to delete from the bottom. */

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  record_delete (mbuf, from, numdel);
	}
      BUF_MODIFF (buf)++;
      MARK_BUFFERS_CHANGED;

      /* #### Point used to be modified here, but this causes problems
	 with MULE, as point is used to calculate bytebposs, and if the
	 offset in bc_numdel causes point to move to a non first-byte
	 location, causing some other function to throw an assertion
	 in ASSERT_VALID_BYTEBPOS. I've moved the code to right after
	 the other movements and adjustments, but before the gap is
	 moved.  -- jh 970813 */

      /* Detach any extents that are completely within the range [FROM, TO],
	 if the extents are detachable.

	 This must come AFTER record_delete(), so that the appropriate
	 extents will be present to be recorded, and BEFORE the gap
	 size is increased, as otherwise we will be confused about
	 where the extents end. */
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  process_extents_for_deletion (wrap_buffer (mbuf), bi_from, bi_to, 0);
	}

      /* Relocate all markers pointing into the new, larger gap to
	 point at the end of the text before the gap.  */
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  adjust_markers (mbuf,
			  (bi_to + BUF_GAP_SIZE (mbuf)),
			  (bi_to + BUF_GAP_SIZE (mbuf)),
			  (- bc_numdel));
	}

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  /* Relocate any extent endpoints just like markers. */
	  adjust_extents_for_deletion (wrap_buffer (mbuf), bi_from, bi_to,
				       BUF_GAP_SIZE (mbuf), bc_numdel, 0);
	}

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  /* Relocate point as if it were a marker.  */
	  if (bi_from < BI_BUF_PT (mbuf))
	    {
	      if (BI_BUF_PT (mbuf) < bi_to)
		JUST_SET_POINT (mbuf, from, bi_from);
	      else
		JUST_SET_POINT (mbuf, BUF_PT (mbuf) - numdel,
				BI_BUF_PT (mbuf) - bc_numdel);
	    }
	}

      SET_BUF_END_GAP_SIZE (buf, BUF_END_GAP_SIZE (buf) + bc_numdel);

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  SET_BOTH_BUF_ZV (mbuf, BUF_ZV (mbuf) - numdel,
			   BI_BUF_ZV (mbuf) - bc_numdel);
	}
      SET_BOTH_BUF_Z (buf, BUF_Z (buf) - numdel, BI_BUF_Z (buf) - bc_numdel);
      SET_GAP_SENTINEL (buf);
    }
  else
    {
      /* Make sure the gap is somewhere in or next to what we are deleting.  */
      if (bi_to < BI_BUF_GPT (buf))
	gap_left (buf, bi_to);
      if (bi_from > BI_BUF_GPT (buf))
	gap_right (buf, bi_from);

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  record_delete (mbuf, from, numdel);
	}
      BUF_MODIFF (buf)++;
      MARK_BUFFERS_CHANGED;

      /* #### Point used to be modified here, but this causes problems
	 with MULE, as point is used to calculate bytebposs, and if the
	 offset in bc_numdel causes point to move to a non first-byte
	 location, causing some other function to throw an assertion
	 in ASSERT_VALID_BYTEBPOS. I've moved the code to right after
	 the other movements and adjustments, but before the gap is
	 moved.  -- jh 970813 */

      /* Detach any extents that are completely within the range [FROM, TO],
	 if the extents are detachable.

	 This must come AFTER record_delete(), so that the appropriate extents
	 will be present to be recorded, and BEFORE the gap size is increased,
	 as otherwise we will be confused about where the extents end. */
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  process_extents_for_deletion (wrap_buffer (mbuf), bi_from, bi_to, 0);
	}

      /* Relocate all markers pointing into the new, larger gap to
	 point at the end of the text before the gap.  */
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  adjust_markers (mbuf,
			  (bi_to + BUF_GAP_SIZE (mbuf)),
			  (bi_to + BUF_GAP_SIZE (mbuf)),
			  (- bc_numdel - BUF_GAP_SIZE (mbuf)));
	}

      /* Relocate any extent endpoints just like markers. */
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  adjust_extents_for_deletion (wrap_buffer (mbuf), bi_from, bi_to,
				       BUF_GAP_SIZE (mbuf),
				       bc_numdel, BUF_GAP_SIZE (mbuf));
	}

      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  /* Relocate point as if it were a marker.  */
	  if (bi_from < BI_BUF_PT (mbuf))
	    {
	      if (BI_BUF_PT (mbuf) < bi_to)
		JUST_SET_POINT (mbuf, from, bi_from);
	      else
		JUST_SET_POINT (mbuf, BUF_PT (mbuf) - numdel,
				BI_BUF_PT (mbuf) - bc_numdel);
	    }
	}

      SET_BUF_GAP_SIZE (buf, BUF_GAP_SIZE (buf) + bc_numdel);
      MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	{
	  SET_BOTH_BUF_ZV (mbuf, BUF_ZV (mbuf) - numdel,
			   BI_BUF_ZV (mbuf) - bc_numdel);
	}
      SET_BOTH_BUF_Z (buf, BUF_Z (buf) - numdel, BI_BUF_Z (buf) - bc_numdel);
      SET_BI_BUF_GPT (buf, bi_from);
      SET_GAP_SENTINEL (buf);
    }

#ifdef MULE
  buffer_mule_signal_deleted_region (buf, from, to, bi_from, bi_to);
#endif

#ifdef ERROR_CHECK_EXTENTS
  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
    {
      sledgehammer_extent_check (wrap_buffer (mbuf));
    }
#endif

  signal_after_change (buf, from, to, from);
}


/************************************************************************/
/*                    Replacement of characters                         */
/************************************************************************/

/* Replace the character at POS in buffer B with CH. */

void
buffer_replace_char (struct buffer *buf, Charbpos pos, Emchar ch,
		     int not_real_change, int force_lock_check)
{
  /* This function can GC */
  Intbyte curstr[MAX_EMCHAR_LEN];
  Intbyte newstr[MAX_EMCHAR_LEN];
  Bytecount curlen, newlen;

  /* Defensive steps just in case a buffer gets deleted and a calling
     function doesn't notice it. */
  if (!BUFFER_LIVE_P (buf))
    return;

  curlen = BUF_CHARPTR_COPY_CHAR (buf, pos, curstr);
  newlen = set_charptr_emchar (newstr, ch);

  if (curlen == newlen)
    {
      struct buffer *mbuf;
      Lisp_Object bufcons;

      /* then we can just replace the text. */
      prepare_to_modify_buffer (buf, pos, pos + 1,
				!not_real_change || force_lock_check);
      /* Defensive steps in case the before-change-functions fuck around */
      if (!BUFFER_LIVE_P (buf))
	/* Bad bad pre-change function. */
	return;

      /* Make args be valid again.  prepare_to_modify_buffer() might have
	 modified the buffer. */
      if (pos < BUF_BEGV (buf))
	pos = BUF_BEGV (buf);
      if (pos >= BUF_ZV (buf))
	pos = BUF_ZV (buf) - 1;
      if (pos < BUF_BEGV (buf))
	/* no more characters in buffer! */
	return;

      if (BUF_FETCH_CHAR (buf, pos) == '\n')
	{
	  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	    {
	      mbuf->changes->newline_was_deleted = 1;
	    }
	}
      MARK_BUFFERS_CHANGED;
      if (!not_real_change)
	{
	  MAP_INDIRECT_BUFFERS (buf, mbuf, bufcons)
	    {
	      record_change (mbuf, pos, 1);
	    }
	  BUF_MODIFF (buf)++;
	}
      memcpy (BUF_BYTE_ADDRESS (buf, pos), newstr, newlen);

      signal_after_change (buf, pos, pos + 1, pos + 1);

      /* We do not have to adjust the Mule data; we just replaced a
	 character with another of the same number of bytes. */
    }
  else
    {
      /*
       * Must implement as deletion followed by insertion.
       *
       * Make a note to move point forward later in the one situation
       * where it is needed, a delete/insert one position behind
       * point.  Point will drift backward by one position and stay
       * there otherwise.
       */
      int movepoint = (pos == BUF_PT (buf) - 1);

      buffer_delete_range (buf, pos, pos + 1, 0);
      /* Defensive steps in case the before-change-functions fuck around */
      if (!BUFFER_LIVE_P (buf))
	/* Bad bad pre-change function. */
	return;

      /* Make args be valid again.  prepare_to_modify_buffer() might have
	 modified the buffer. */
      if (pos < BUF_BEGV (buf))
	pos = BUF_BEGV (buf);
      if (pos >= BUF_ZV (buf))
	pos = BUF_ZV (buf) - 1;
      if (pos < BUF_BEGV (buf))
	/* no more characters in buffer! */
	return;
      /*
       * -1 as the pos argument means to move point forward with the
       * insertion, which we must do if the deletion moved point
       * backward so that it now equals the insertion point.
       */
      buffer_insert_string_1 (buf, (movepoint ? -1 : pos),
			      newstr, Qnil, 0, newlen, 0);
    }
}


/************************************************************************/
/*                            Other functions                           */
/************************************************************************/

/* Make a string from a buffer.  This needs to take into account the gap,
   and add any necessary extents from the buffer. */

static Lisp_Object
make_string_from_buffer_1 (struct buffer *buf, Charbpos pos, Charcount length,
			   int no_extents)
{
  /* This function can GC */
  Bytebpos    bi_ind = charbpos_to_bytebpos (buf, pos);
  Bytecount bi_len = charbpos_to_bytebpos (buf, pos + length) - bi_ind;
  Lisp_Object  val = make_uninit_string (bi_len);

  struct gcpro gcpro1;
  GCPRO1 (val);

  if (!no_extents)
    add_string_extents (val, buf, bi_ind, bi_len);

  {
    Bytecount len1 = BI_BUF_GPT (buf) - bi_ind;
    Intbyte *start1 = BI_BUF_BYTE_ADDRESS (buf, bi_ind);
    Intbyte *dest = XSTRING_DATA (val);

    if (len1 < 0)
      {
	/* Completely after gap */
	memcpy (dest, start1, bi_len);
      }
    else if (bi_len <= len1)
      {
	/* Completely before gap */
	memcpy (dest, start1, bi_len);
      }
    else
      {
	/* Spans gap */
	Bytebpos pos2 = bi_ind + len1;
	Intbyte *start2 = BI_BUF_BYTE_ADDRESS (buf, pos2);

	memcpy (dest, start1, len1);
	memcpy (dest + len1, start2, bi_len - len1);
      }
  }

  init_string_ascii_begin (val);
  sledgehammer_check_ascii_begin (val);

  UNGCPRO;
  return val;
}

Lisp_Object
make_string_from_buffer (struct buffer *buf, Charbpos pos, Charcount length)
{
  return make_string_from_buffer_1 (buf, pos, length, 0);
}

Lisp_Object
make_string_from_buffer_no_extents (struct buffer *buf, Charbpos pos,
				    Charcount length)
{
  return make_string_from_buffer_1 (buf, pos, length, 1);
}

void
barf_if_buffer_read_only (struct buffer *buf, Charbpos from, Charbpos to)
{
  Lisp_Object buffer;
  Lisp_Object iro;

  buffer = wrap_buffer (buf);
 back:
  iro = (buf == current_buffer ? Vinhibit_read_only :
	 symbol_value_in_buffer (Qinhibit_read_only, buffer));
  if (!LISTP (iro))
    return;
  if (NILP (iro) && !NILP (buf->read_only))
    {
      Fsignal (Qbuffer_read_only, (list1 (buffer)));
      goto back;
    }
  if (from > 0)
    {
      if (to < 0)
	to = from;
      verify_extent_modification (buffer,
				  charbpos_to_bytebpos (buf, from),
				  charbpos_to_bytebpos (buf, to),
				  iro);
    }
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
reinit_vars_of_insdel (void)
{
  inside_change_hook = 0;
  in_first_change = 0;
}

void
vars_of_insdel (void)
{
  reinit_vars_of_insdel ();
}

void
init_buffer_text (struct buffer *b)
{
  if (!b->base_buffer)
    {
      SET_BUF_GAP_SIZE (b, 20);
      BUFFER_ALLOC (b->text->beg, BUF_GAP_SIZE (b) + BUF_END_SENTINEL_SIZE);
      if (! BUF_BEG_ADDR (b))
	memory_full ();

      SET_BUF_END_GAP_SIZE (b, 0);
      SET_BI_BUF_GPT (b, 1);
      SET_BOTH_BUF_Z (b, 1, 1);
      SET_GAP_SENTINEL (b);
      SET_END_SENTINEL (b);
#ifdef MULE
      {
	int i;

	b->text->mule_bufmin = b->text->mule_bufmax = 1;
	b->text->mule_bytmin = b->text->mule_bytmax = 1;
	b->text->mule_shifter = 0;
	b->text->mule_three_p = 0;
	b->text->entirely_ascii_p = 1;

	for (i = 0; i < 16; i++)
	  {
	    b->text->mule_charbpos_cache[i] = 1;
	    b->text->mule_bytebpos_cache[i] = 1;
	  }
      }
#endif /* MULE */
      b->text->line_number_cache = Qnil;

      BUF_MODIFF (b) = 1;
      BUF_SAVE_MODIFF (b) = 1;

      JUST_SET_POINT (b, 1, 1);
      SET_BOTH_BUF_BEGV (b, 1, 1);
      SET_BOTH_BUF_ZV (b, 1, 1);

      b->text->changes = xnew_and_zero (struct buffer_text_change_data);
    }
  else
    {
      JUST_SET_POINT (b, BUF_PT (b->base_buffer), BI_BUF_PT (b->base_buffer));
      SET_BOTH_BUF_BEGV (b, BUF_BEGV (b->base_buffer),
			 BI_BUF_BEGV (b->base_buffer));
      SET_BOTH_BUF_ZV (b, BUF_ZV (b->base_buffer),
			 BI_BUF_ZV (b->base_buffer));
    }

  b->changes = xnew_and_zero (struct each_buffer_change_data);
  BUF_FACECHANGE (b) = 1;

#ifdef REGION_CACHE_NEEDS_WORK
  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->width_table = Qnil;
#endif
}

void
uninit_buffer_text (struct buffer *b)
{
  if (!b->base_buffer)
    {
      BUFFER_FREE (b->text->beg);
      xfree (b->text->changes);
    }
  xfree (b->changes);

#ifdef REGION_CACHE_NEEDS_WORK
  if (b->newline_cache)
    {
      free_region_cache (b->newline_cache);
      b->newline_cache = 0;
    }
  if (b->width_run_cache)
    {
      free_region_cache (b->width_run_cache);
      b->width_run_cache = 0;
    }
  b->width_table = Qnil;
#endif
}
