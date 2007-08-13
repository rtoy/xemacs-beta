/* Indentation functions.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.

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

/* This file has been Mule-ized. */

/* Synched up with: 19.30.  Diverges significantly from FSF. */


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "extents.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"
#include "insdel.h"
#ifdef REGION_CACHE_NEEDS_WORK
#include "region-cache.h"
#endif
#include "window.h"

/* Indentation can insert tabs if this is non-zero;
   otherwise always uses spaces */
int indent_tabs_mode;

/* Avoid recalculation by remembering things in these variables. */

/* Last value returned by current_column.

   Some things set last_known_column_point to -1
   to mark the memoized value as invalid */
static int last_known_column;

/* Last buffer searched by current_column */
static struct buffer *last_known_column_buffer;

/* Value of point when current_column was called */
static Bufpos last_known_column_point;

/* Value of MODIFF when current_column was called */
static int last_known_column_modified;

static Bufpos
last_visible_position (Bufpos pos, struct buffer *buf)
{
  Lisp_Object buffer;
  Lisp_Object value;

  XSETBUFFER (buffer, buf);
  value = Fprevious_single_property_change (make_int (pos), Qinvisible,
					    buffer, Qnil);
  if (NILP (value))
    return 0; /* no visible position found */
  else
    /* #### bug bug bug!!! This will return the position of the beginning
       of an invisible extent; this extent is very likely to be start-closed,
       and thus the spaces inserted in `indent-to' will go inside the
       invisible extent.

       Not sure what the correct solution is here.  Rethink indent-to? */
    return XINT (value);
}

#ifdef REGION_CACHE_NEEDS_WORK

/* Allocate or free the width run cache, as requested by the current
   state of current_buffer's cache_long_line_scans variable.  */
static void
width_run_cache_on_off (struct buffer *buf)
{
  if (NILP (buf->cache_long_line_scans))
    {
      /* It should be off.  */
      if (buf->width_run_cache)
        {
          free_region_cache (buf->width_run_cache);
          buf->width_run_cache = 0;
          buf->width_table = Qnil;
        }
    }
  else
    {
      /* It should be on.  */
      if (buf->width_run_cache == 0)
        {
          buf->width_run_cache = new_region_cache ();
          recompute_width_table (buf, buffer_display_table ());
        }
    }
}

#endif /* REGION_CACHE_NEEDS_WORK */


/* Cancel any recorded value of the horizontal position.  */
 
void
invalidate_current_column (void)
{
  last_known_column_point = -1;
}

int
column_at_point (struct buffer *buf, Bufpos init_pos, int cur_col)
{
  int col;
  int tab_seen;
  int tab_width = XINT (buf->tab_width);
  int post_tab;
  Bufpos pos = init_pos;

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;
  col = tab_seen = post_tab = 0;

  while (1)
    {
      if (pos <= BUF_BEGV (buf))
	break;

      pos--;
      if (BUF_FETCH_CHAR (buf, pos) == '\t')
	{
	  if (tab_seen)
	    col = ((col + tab_width) / tab_width) * tab_width;

	  post_tab += col;
	  col = 0;
	  tab_seen = 1;
	}
      else if (BUF_FETCH_CHAR (buf, pos) == '\n' ||
	       (EQ (buf->selective_display, Qt) &&
		BUF_FETCH_CHAR (buf, pos) == '\r'))
	break;
      else
	{
	  /* #### This needs updating to handle the new redisplay. */
	  /* #### FSFmacs looks at ctl_arrow, display tables.
	     We need to do similar. */
#if 0
	  displayed_glyphs = glyphs_from_bufpos (sel_frame, buf,
						 XWINDOW (selected_window),
						 pos, dp, 0, col, 0, 0, 0);
	  col += (displayed_glyphs->columns
		  - (displayed_glyphs->begin_columns
		     + displayed_glyphs->end_columns));
#else
	  col++;
#endif
	}
    }

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  if (cur_col)
    {
      last_known_column_buffer = buf;
      last_known_column = col;
      last_known_column_point = BUF_PT (buf);
      last_known_column_modified = BUF_MODIFF (buf);
    }

  return col;
}

int
current_column (struct buffer *buf)
{
  if (buf == last_known_column_buffer
      && BUF_PT (buf) == last_known_column_point
      && BUF_MODIFF (buf) == last_known_column_modified)
    return last_known_column;

  return column_at_point (buf, BUF_PT (buf), 1);
}

DEFUN ("current-column", Fcurrent_column, Scurrent_column, 0, 1, 0 /*
Return the horizontal position of point.  Beginning of line is column 0.
This is calculated by adding together the widths of all the displayed
 representations of the character between the start of the previous line
 and point. (e.g. control characters will have a width of 2 or 4, tabs
 will have a variable width.)
Ignores finite width of frame, which means that this function may return
 values greater than (frame-width).
Whether the line is visible (if `selective-display' is t) has no effect;
 however, ^M is treated as end of line when `selective-display' is t.
If BUFFER is nil, the current buffer is assumed.
*/ )
  (buffer)
     Lisp_Object buffer;
{
  return (make_int (current_column (decode_buffer (buffer, 0))));
}


DEFUN ("indent-to", Findent_to, Sindent_to, 1, 3, "NIndent to column: " /*
Indent from point with tabs and spaces until COLUMN is reached.
Optional second argument MIN says always do at least MIN spaces
 even if that goes past COLUMN; by default, MIN is zero.
If BUFFER is nil, the current buffer is assumed.
*/ )
  (col, minimum, buffer)
     Lisp_Object col, minimum, buffer;
{
  /* This function can GC */
  int mincol;
  int fromcol;
  struct buffer *buf = decode_buffer (buffer, 0);
  int tab_width = XINT (buf->tab_width);
  Bufpos opoint = 0;

  CHECK_INT (col);
  if (NILP (minimum))
    minimum = Qzero;
  else
    CHECK_INT (minimum);

  XSETBUFFER (buffer, buf);
  
  fromcol = current_column (buf);
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (col)) mincol = XINT (col);

  if (fromcol == mincol)
    return make_int (mincol);

  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;
  
  if (!NILP (Fextent_at (make_int (BUF_PT (buf)), buffer, Qinvisible,
			 Qnil, Qnil)))
    {
      Bufpos last_visible = last_visible_position (BUF_PT (buf), buf);

      opoint = BUF_PT (buf);
      if (last_visible >= BUF_BEGV (buf))
	BUF_SET_PT (buf, last_visible);
      else 
        error ("Visible portion of buffer not modifiable");
    }

  if (indent_tabs_mode)
    {
      int n = mincol / tab_width - fromcol / tab_width;
      if (n != 0)
	{
	  Finsert_char (make_char ('\t'), make_int (n), Qnil, buffer);

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  Finsert_char (make_char (' '), make_int (mincol - fromcol), Qnil, buffer);

  last_known_column_buffer = buf;
  last_known_column = mincol;
  last_known_column_point = BUF_PT (buf);
  last_known_column_modified = BUF_MODIFF (buf);

  /* Not in FSF: */
  if (opoint > 0)
    BUF_SET_PT (buf, opoint);

  return (make_int (mincol));
}

int
bi_spaces_at_point (struct buffer *b, Bytind bi_pos)
{
  Bytind bi_end = BI_BUF_ZV (b);
  int col = 0;
  Emchar c;
  int tab_width = XINT (b->tab_width);

  if (tab_width <= 0 || tab_width > 1000)
    tab_width = 8;

  while (bi_pos < bi_end &&
	 (c = BI_BUF_FETCH_CHAR (b, bi_pos),
	  (c == '\t'
	   ? (col += tab_width - col % tab_width)
	   : (c == ' ' ? ++col : 0))))
    INC_BYTIND (b, bi_pos);

  return col;
}


DEFUN ("current-indentation", Fcurrent_indentation, Scurrent_indentation,
  0, 1, 0 /*
Return the indentation of the current line.
This is the horizontal position of the character
following any initial whitespace.
*/ )
  (buffer)
  Lisp_Object buffer;
{
  struct buffer *buf = decode_buffer (buffer, 0);
  Bufpos pos = find_next_newline (buf, BUF_PT (buf), -1);

  XSETBUFFER (buffer, buf);

  if (!NILP (Fextent_at (make_int (pos), buffer, Qinvisible, Qnil, Qnil)))
    return Qzero;

  return make_int (bi_spaces_at_point (buf, bufpos_to_bytind (buf, pos)));
}


DEFUN ("move-to-column", Fmove_to_column, Smove_to_column, 1, 3, 0 /*
Move point to column COLUMN in the current line.
The column of a character is calculated by adding together the widths
as displayed of the previous characters in the line.
This function ignores line-continuation;
there is no upper limit on the column number a character can have
and horizontal scrolling has no effect.

If specified column is within a character, point goes after that character.
If it's past end of line, point goes to end of line.

A non-nil second (optional) argument FORCE means, if the line
is too short to reach column COLUMN then add spaces/tabs to get there,
and if COLUMN is in the middle of a tab character, change it to spaces.
Returns the actual column that it moved to.
*/ )
  (column, force, buffer)
     Lisp_Object column, force, buffer;
{
  /* This function can GC */
  Bufpos pos;
  struct buffer *buf = decode_buffer (buffer, 0);
  int col = current_column (buf);
  int goal;
  Bufpos end;
  int tab_width = XINT (buf->tab_width);

  int prev_col = 0;
  Emchar c = 0;

  XSETBUFFER (buffer, buf);
  if (tab_width <= 0 || tab_width > 1000) tab_width = 8;
  CHECK_NATNUM (column);
  goal = XINT (column);

 retry:
  pos = BUF_PT (buf);
  end = BUF_ZV (buf);

  /* If we're starting past the desired column,
     back up to beginning of line and scan from there.  */
  if (col > goal)
    {
      pos = find_next_newline (buf, pos, -1);
      col = 0;
    }

  while (col < goal && pos < end)
    {
      c = BUF_FETCH_CHAR (buf, pos);
      if (c == '\n')
	break;
      if (c == '\r' && EQ (buf->selective_display, Qt))
	break;
      if (c == '\t')
	{
	  prev_col = col;
	  col += tab_width;
	  col = col / tab_width * tab_width;
	}
      else
	{
	  /* #### oh for the days of the complete new redisplay */
	  /* #### FSFmacs looks at ctl_arrow, display tables.
	     We need to do similar. */
#if 0
	  displayed_glyphs = glyphs_from_bufpos (selected_frame (),
						 buf,
						 XWINDOW (Fselected_window (Qnil)),
						 pos, dp, 0, col, 0, 0, 0);
	  col += (displayed_glyphs->columns
		  - (displayed_glyphs->begin_columns
		     + displayed_glyphs->end_columns));
#else
	  col++;
#endif
	}

      pos++;
    }

  BUF_SET_PT (buf, pos);

  /* If a tab char made us overshoot, change it to spaces
     and scan through it again.  */
  if (!NILP (force) && col > goal && c == '\t' && prev_col < goal)
    {
      buffer_delete_range (buf, BUF_PT (buf) - 1, BUF_PT (buf), 0);
      Findent_to (make_int (col - 1), Qzero, buffer);
      buffer_insert_emacs_char (buf, ' ');
      goto retry;
    }

  /* If line ends prematurely, add space to the end.  */
  if (col < goal && !NILP (force))
    {
      col = goal;
      Findent_to (make_int (col), Qzero, buffer);
    }

  last_known_column_buffer = buf;
  last_known_column = col;
  last_known_column_point = BUF_PT (buf);
  last_known_column_modified = BUF_MODIFF (buf);

  return (make_int (col));
}

#if 0 /* #### OK boys, this function needs to be present, I think.
	 It was there before the 19.12 redisplay rewrite. */

xxDEFUN ("compute-motion", Fcompute_motion, Scompute_motion, 7, 7, 0 /*
  "Scan through the current buffer, calculating screen position.
Scan the current buffer forward from offset FROM,
assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--
to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--
and return the ending buffer position and screen location.

There are three additional arguments:

WIDTH is the number of columns available to display text;
this affects handling of continuation lines.
This is usually the value returned by `window-width', less one (to allow
for the continuation glyph).

OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
HSCROLL is the number of columns not being displayed at the left
margin; this is usually taken from a window's hscroll member.
TAB-OFFSET is the number of columns of the first tab that aren't
being displayed, perhaps because the line was continued within it.
If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.

WINDOW is the window to operate on.  Currently this is used only to
find the display table.  It does not matter what buffer WINDOW displays;
`compute-motion' always operates on the current buffer.

The value is a list of five elements:
  (POS HPOS VPOS PREVHPOS CONTIN)
POS is the buffer position where the scan stopped.
VPOS is the vertical position where the scan stopped.
HPOS is the horizontal position where the scan stopped.

PREVHPOS is the horizontal position one character back from POS.
CONTIN is t if a line was continued after (or within) the previous character.

For example, to find the buffer position of column COL of line LINE
of a certain window, pass the window's starting location as FROM
and the window's upper-left coordinates as FROMPOS.
Pass the buffer's (point-max) as TO, to limit the scan to the end of the
visible section of the buffer, and pass LINE and COL as TOPOS.
*/ )
  (from, frompos, to, topos, width, offsets, window)
     Lisp_Object from, frompos, to, topos;
     Lisp_Object width, offsets, window;
{
  Lisp_Object bufpos, hpos, vpos, prevhpos, contin;
  struct position *pos;
  int hscroll, tab_offset;
  struct window *w = decode_window (window);

  CHECK_INT_COERCE_MARKER (from);
  CHECK_CONS (frompos);
  CHECK_INT (XCAR (frompos));
  CHECK_INT (XCDR (frompos));
  CHECK_INT_COERCE_MARKER (to);
  CHECK_CONS (topos);
  CHECK_INT (XCAR (topos));
  CHECK_INT (XCDR (topos));
  CHECK_INT (width);
  if (!NILP (offsets))
    {
      CHECK_CONS (offsets);
      CHECK_INT (XCAR (offsets));
      CHECK_INT (XCDR (offsets));
      hscroll = XINT (XCAR (offsets));
      tab_offset = XINT (XCDR (offsets));
    }
  else
    hscroll = tab_offset = 0;

  pos = compute_motion (XINT (from), XINT (XCDR (frompos)),
			XINT (XCAR (frompos)),
			XINT (to), XINT (XCDR (topos)),
			XINT (XCAR (topos)),
			XINT (width), hscroll, tab_offset, w);

  XSETINT (bufpos, pos->bufpos);
  XSETINT (hpos, pos->hpos);
  XSETINT (vpos, pos->vpos);
  XSETINT (prevhpos, pos->prevhpos);

  return list5 (bufpos,
		hpos,
		vpos,
		prevhpos,
		pos->contin ? Qt : Qnil);

}

#endif /* 0 */

/*****************************************************************************
 vmotion

 Given a starting position ORIG, move point VTARGET lines in WINDOW.
 Returns the new value for point.  If the arg ret_vpos is not nil, it is
 taken to be a pointer to an int and the number of lines actually moved is
 returned in it.
 ****************************************************************************/
Bufpos
vmotion (struct window *w, Bufpos orig, int vtarget, int *ret_vpos)
{
  struct buffer *b = XBUFFER (w->buffer);
  int elt;

  elt = point_in_line_start_cache (w, orig, (vtarget < 0
					     ? -vtarget
					     : vtarget));

  /* #### This assertion must be true before the if statements are hit
     but may possibly be wrong after the call to
     point_in_line_start_cache if orig is outside of the visible
     region of the buffer.  Handle this. */
  assert (elt >= 0);

  /* Moving downward. */
  if (vtarget > 0)
    {
      int cur_line = Dynarr_length (w->line_start_cache) - 1 - elt;
      Bufpos ret_pt;

      if (cur_line > vtarget)
	cur_line = vtarget;

      /* The traditional FSF behavior is to return the end of buffer
         position if we couldn't move far enough because we hit it.  */
      if (cur_line < vtarget)
	ret_pt = BUF_ZV (b);
      else
	ret_pt = Dynarr_atp (w->line_start_cache, cur_line + elt)->start;

      while (ret_pt > BUF_ZV (b) && cur_line > 0)
	{
	  cur_line--;
	  ret_pt = Dynarr_atp (w->line_start_cache, cur_line + elt)->start;
	}

      if (ret_vpos) *ret_vpos = cur_line;
      return (ret_pt);
    }
  else if (vtarget < 0)
    {
      if (elt < -vtarget)
	{
	  if (ret_vpos) *ret_vpos = -elt;
	  /* #### This should be BUF_BEGV (b), right? */
	  return (Dynarr_atp (w->line_start_cache, 0)->start);
	}
      else
	{
	  if (ret_vpos) *ret_vpos = vtarget;
	  return (Dynarr_atp (w->line_start_cache, elt + vtarget)->start);
	}
    }
  else
    {
      /* No vertical motion requested so we just return the position
         of the beginning of the current line. */
      if (ret_vpos) *ret_vpos = 0;

      return (Dynarr_atp (w->line_start_cache, elt)->start);
    }

  RETURN_NOT_REACHED(0)	/* shut up compiler */
}

DEFUN ("vertical-motion", Fvertical_motion, Svertical_motion, 1, 2, 0 /*
Move to start of frame line LINES lines down.
If LINES is negative, this is moving up.

The optional second argument WINDOW specifies the window to use for
parameters such as width, horizontal scrolling, and so on.
the default is the selected window.
Note that `vertical-motion' sets WINDOW's buffer's point, not
WINDOW's point. (This differs from FSF Emacs, which buggily always
sets current buffer's point, regardless of WINDOW.)

Sets point to position found; this may be start of line
 or just the start of a continuation line.
Returns number of lines moved; may be closer to zero than LINES
 if beginning or end of buffer was reached.
Optional second argument is WINDOW to move in.
*/ )
  (lines, window)
     Lisp_Object lines, window;
{
  if (NILP (window))
    window = Fselected_window (Qnil);
  CHECK_WINDOW (window);
  {
    Bufpos bufpos;
    int vpos;
    struct window *w  = XWINDOW (window);

    CHECK_INT (lines);

    bufpos = vmotion (XWINDOW (window), BUF_PT (XBUFFER (w->buffer)),
		      XINT (lines), &vpos);

    /* Note that the buffer's point is set, not the window's point. */
    BUF_SET_PT (XBUFFER (w->buffer), bufpos);

    return make_int (vpos);
  }
}


void
syms_of_indent (void)
{
  defsubr (&Scurrent_indentation);
  defsubr (&Sindent_to);
  defsubr (&Scurrent_column);
  defsubr (&Smove_to_column);
#if 0 /* #### */
  defsubr (&Scompute_motion);
#endif
  defsubr (&Svertical_motion);
}

void
vars_of_indent (void)
{
  DEFVAR_BOOL ("indent-tabs-mode", &indent_tabs_mode /*
*Indentation can insert tabs if this is non-nil.
Setting this variable automatically makes it local to the current buffer.
*/ );
  indent_tabs_mode = 1;
}
