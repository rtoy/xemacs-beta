/* Gutter implementation.
   Copyright (C) 1999, 2000 Andy Piper.
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

/* written by Andy Piper <andy@xemacs.org> with specifiers partially
   ripped-off from toolbar.c */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "frame-impl.h"
#include "device-impl.h"
#include "faces.h"
#include "glyphs.h"
#include "redisplay.h"
#include "window.h"
#include "gutter.h"

Lisp_Object Vgutter[NUM_EDGES];
Lisp_Object Vgutter_size[NUM_EDGES];
Lisp_Object Vgutter_visible_p[NUM_EDGES];
Lisp_Object Vgutter_border_width[NUM_EDGES];

Lisp_Object Vdefault_gutter, Vdefault_gutter_visible_p;
Lisp_Object Vdefault_gutter_width, Vdefault_gutter_height;
Lisp_Object Vdefault_gutter_border_width;

Lisp_Object Vdefault_gutter_position;

Lisp_Object Qgutter_size;
Lisp_Object Qgutter_visible;
Lisp_Object Qdefault_gutter_position_changed_hook;

static void
update_gutter_geometry (struct frame *f, enum edge_pos pos);

#if 0
static Lisp_Object
frame_topmost_window (struct frame *f)
{
  Lisp_Object w = FRAME_ROOT_WINDOW (f);

  do {
    while (!NILP (XWINDOW (w)->vchild))
      {
	w = XWINDOW (w)->vchild;
      }
  } while (!NILP (XWINDOW (w)->hchild) && !NILP (w = XWINDOW (w)->hchild));

  return w;
}
#endif

static Lisp_Object
frame_bottommost_window (struct frame *f)
{
  Lisp_Object w = FRAME_ROOT_WINDOW (f);

  do {
    while (!NILP (XWINDOW (w)->vchild))
      {
	w = XWINDOW (w)->vchild;
	while (!NILP (XWINDOW (w)->next))
	  {
	    w = XWINDOW (w)->next;
	  }
      }
  } while (!NILP (XWINDOW (w)->hchild) && !NILP (w = XWINDOW (w)->hchild));

  return w;
}

#if 0
static Lisp_Object
frame_leftmost_window (struct frame *f)
{
  Lisp_Object w = FRAME_ROOT_WINDOW (f);

  do {
    while (!NILP (XWINDOW (w)->hchild))
      {
	w = XWINDOW (w)->hchild;
      }
  } while (!NILP (XWINDOW (w)->vchild) && !NILP (w = XWINDOW (w)->vchild));

  return w;
}

static Lisp_Object
frame_rightmost_window (struct frame *f)
{
  Lisp_Object w = FRAME_ROOT_WINDOW (f);

  do {
    while (!NILP (XWINDOW (w)->hchild))
      {
	w = XWINDOW (w)->hchild;
	while (!NILP (XWINDOW (w)->next))
	  {
	    w = XWINDOW (w)->next;
	  }
      }
  } while (!NILP (XWINDOW (w)->vchild) && !NILP (w = XWINDOW (w)->vchild));
  return w;
}
#endif

/* calculate the coordinates of a gutter for the current frame and
   selected window. we have to be careful in calculating this as we
   need to use *two* windows, the currently selected window will give
   us the actual height, width and contents of the gutter, but if we
   use this for calculating the gutter positions we run into trouble
   if it is not the window nearest the gutter. Instead we predetermine
   the nearest window and then use that.*/
static void
get_gutter_coords (struct frame *f, enum edge_pos pos, int *x, int *y,
		   int *width, int *height)
{
  /* We use the bottommost window (not the minibuffer, but the bottommost
     non-minibuffer window) rather than any FRAME_BOTTOM_GUTTER_START
     because the gutter goes *above* the minibuffer -- for this same reason,
     FRAME_BOTTOM_GUTTER_START isn't currently defined. */
  struct window *bot = XWINDOW (frame_bottommost_window (f));
  /* The top and bottom gutters take precedence over the left and
     right. */
  switch (pos)
    {
    case TOP_EDGE:
      *x = FRAME_LEFT_GUTTER_START (f);
      *y = FRAME_TOP_GUTTER_START (f);
      *width = FRAME_RIGHT_GUTTER_END (f) - *x;
      *height = FRAME_TOP_GUTTER_BOUNDS (f);
      break;

    case BOTTOM_EDGE:
      *x = FRAME_LEFT_GUTTER_START (f);
#ifdef BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER
      *y = FRAME_BOTTOM_GUTTER_START (f);
#else
      *y = WINDOW_BOTTOM (bot);
#endif
      *width = FRAME_RIGHT_GUTTER_END (f) - *x;
      *height = FRAME_BOTTOM_GUTTER_BOUNDS (f);
      break;

    case LEFT_EDGE:
      *x = FRAME_LEFT_GUTTER_START (f);
      *y = FRAME_TOP_GUTTER_END (f);
      *width = FRAME_LEFT_GUTTER_BOUNDS (f);
      *height = WINDOW_BOTTOM (bot) - *y;
      break;

    case RIGHT_EDGE:
      *x = FRAME_RIGHT_GUTTER_START (f);
      *y = FRAME_TOP_GUTTER_END (f);
      *width = FRAME_RIGHT_GUTTER_BOUNDS (f);
      *height = WINDOW_BOTTOM (bot) - *y;
      break;

    default:
      ABORT ();
    }
}

/*
 display_boxes_in_gutter_p

 Determine whether the required display_glyph_area is completely
 inside the gutter. -1 means the display_box is not in the gutter. 1
 means the display_box and the display_glyph_area are in the
 window. 0 means the display_box is in the gutter but the
 display_glyph_area is not. */
int display_boxes_in_gutter_p (struct frame *f, struct display_box* db,
			       struct display_glyph_area* dga)
{
  enum edge_pos pos;
  EDGE_POS_LOOP (pos)
    {
      if (FRAME_GUTTER_VISIBLE (f, pos))
	{
	  int x, y, width, height;
	  get_gutter_coords (f, pos, &x, &y, &width, &height);
	  if (db->xpos + dga->xoffset >= x
	      &&
	      db->ypos + dga->yoffset >= y
	      &&
	      db->xpos + dga->xoffset + dga->width <= x + width
	      &&
	      db->ypos + dga->yoffset + dga->height <= y + height)
	    return 1;
	  else if (db->xpos >= x && db->ypos >= y
		   && db->xpos + db->width <= x + width
		   && db->ypos + db->height <= y + height)
	    return 0;
	}
    }
  return -1;
}

/* Convert the gutter specifier into something we can actually
   display. */
static Lisp_Object construct_window_gutter_spec (struct window* w,
						 enum edge_pos pos)
{
  Lisp_Object rest, *args;
  int nargs = 0;
  Lisp_Object gutter = RAW_WINDOW_GUTTER (w, pos);

  if (STRINGP (gutter) || NILP (gutter))
    return gutter;

  GET_LIST_LENGTH (gutter, nargs);
  args = alloca_array (Lisp_Object, nargs >> 1);
  nargs = 0;

  for (rest = gutter; !NILP (rest); rest = XCDR (XCDR (rest)))
    {
      /* We only put things in the real gutter that are declared to be
	 visible. */
      if (!CONSP (WINDOW_GUTTER_VISIBLE (w, pos))
	  ||
	  !NILP (Fmemq (XCAR (rest), WINDOW_GUTTER_VISIBLE (w, pos))))
	{
	  args [nargs++] = XCAR (XCDR (rest));
	}
    }

  return concatenate (nargs, args, Qstring, 0);
}

/* Sizing gutters is a pain so we try and help the user by determining
   what height will accommodate all lines. This is useless on left and
   right gutters as we always have a maximal number of lines. */
static int
calculate_gutter_size_from_display_lines (enum edge_pos pos,
					  display_line_dynarr* ddla)
{
  int size = 0;
  struct display_line *dl;

  /* For top and bottom the calculation is easy. */
  if (pos == TOP_EDGE || pos == BOTTOM_EDGE)
    {
      /* grab coordinates of last line  */
      if (Dynarr_length (ddla))
	{
	  dl = Dynarr_atp (ddla, Dynarr_length (ddla) - 1);
	  size = (dl->ypos + dl->descent - dl->clip)
	    - (Dynarr_begin (ddla)->ypos - Dynarr_begin (ddla)->ascent);
	}
    }
  /* For left and right we have to do some maths. */
  else
    {
      int start_pos = 0, end_pos = 0, line;
      for (line = 0; line < Dynarr_length (ddla); line++)
	{
	  int block;
	  dl = Dynarr_atp (ddla, line);

	  for (block = 0; block < Dynarr_largest (dl->display_blocks); block++)
	    {
	      struct display_block *db = Dynarr_atp (dl->display_blocks, block);

	      if (db->type == TEXT)
		{
		  start_pos = min (db->start_pos, start_pos);
		  end_pos = max (db->end_pos, end_pos);
		}
	    }
	}
      size = end_pos - start_pos;
    }

  return size;
}

static Lisp_Object
calculate_gutter_size (struct window *w, enum edge_pos pos)
{
  struct frame* f = XFRAME (WINDOW_FRAME (w));
  display_line_dynarr *ddla;
  Lisp_Object ret = Qnil;

  /* Callers need to handle this. */
  assert (!in_display);
  /* degenerate case */
  if (NILP (RAW_WINDOW_GUTTER (w, pos))
      ||
      !FRAME_VISIBLE_P (f)
      ||
      NILP (w->buffer))
    return Qnil;

  if (!in_display)
    {
      int count;

      /* We are calling directly into redisplay from the outside, so turn on
	 critical section protection. */
      count = enter_redisplay_critical_section ();

      ddla = Dynarr_new (display_line);
      /* generate some display lines */
      generate_displayable_area (w, WINDOW_GUTTER (w, pos),
				 FRAME_LEFT_GUTTER_START (f),
				 FRAME_TOP_GUTTER_START (f),
				 FRAME_RIGHT_GUTTER_END (f)
				 - FRAME_LEFT_GUTTER_START (f),
#ifdef BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER
				 FRAME_BOTTOM_GUTTER_END (f)
#else
				 /* #### GEOM! This is how it used to read,
				    and this includes both gutter and
				    minibuffer below it.  Not clear whether
				    it was intended that way. --ben */
				 FRAME_BOTTOM_INTERNAL_BORDER_START (f)
#endif
				 - FRAME_TOP_GUTTER_START (f),
				 ddla, 0, DEFAULT_INDEX);

      /* Let GC happen again. */
      exit_redisplay_critical_section (count);

      ret = make_fixnum (calculate_gutter_size_from_display_lines (pos, ddla));
      free_display_lines (ddla);
    }

  return ret;
}

static void
output_gutter (struct frame *f, enum edge_pos pos, int force)
{
  Lisp_Object window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  struct device *d = XDEVICE (f->device);
  struct window* w = XWINDOW (window);
  int x, y, width, height, ypos;
  int line, border_width;
  face_index findex;
  display_line_dynarr* ddla, *cdla;
  struct display_line *dl = 0;
  int cdla_len;

  if (!WINDOW_LIVE_P (w))
    return;

  border_width = FRAME_GUTTER_BORDER_WIDTH (f, pos);
  findex = get_builtin_face_cache_index (w, Vwidget_face);

  if (!f->current_display_lines[pos])
    f->current_display_lines[pos] = Dynarr_new (display_line);
  if (!f->desired_display_lines[pos])
    f->desired_display_lines[pos] = Dynarr_new (display_line);

  ddla = f->desired_display_lines[pos];
  cdla = f->current_display_lines[pos];
  cdla_len = Dynarr_length (cdla);

  get_gutter_coords (f, pos, &x, &y, &width, &height);
  /* generate some display lines */
  generate_displayable_area (w, WINDOW_GUTTER (w, pos),
			     x + border_width, y + border_width,
			     width - 2 * border_width,
			     height - 2 * border_width, ddla, 0, findex);

  /* We only output the gutter if we think something of significance
     has changed.  This is, for example, because redisplay can cause
     new face cache elements to get added causing compare_runes to
     fail because the findex for a particular face has changed.  */
  if (force || f->faces_changed || f->frame_changed ||
      f->gutter_changed || f->glyphs_changed ||
      f->size_changed || f->subwindows_changed ||
      w->windows_changed || f->windows_structure_changed ||
      cdla_len != Dynarr_length (ddla) ||
      (f->extents_changed && w->gutter_extent_modiff[pos]))
    {
#ifdef DEBUG_GUTTERS
      stderr_out ("gutter redisplay [%s %dx%d@%d+%d] triggered by %s,\n",
	      pos == TOP_EDGE ? "TOP" :
	      pos == BOTTOM_EDGE ? "BOTTOM" :
	      pos == LEFT_EDGE ? "LEFT" : "RIGHT",
	      width, height, x, y, force ? "force" :
	      f->faces_changed ? "f->faces_changed" :
	      f->frame_changed ? "f->frame_changed" :
	      f->gutter_changed ? "f->gutter_changed" :
	      f->glyphs_changed ? "f->glyphs_changed" :
	      f->size_changed ? "f->size_changed" :
	      f->subwindows_changed ? "f->subwindows_changed" :
	      w->windows_changed ? "w->windows_changed" :
	      f->windows_structure_changed ? "f->windows_structure_changed" :
	      cdla_len != Dynarr_length (ddla) ? "different display structures" :
	      f->extents_changed && w->gutter_extent_modiff[pos] ?
	      "f->extents_changed && w->gutter_extent_modiff[pos]" : "<null>");
#endif
      /* Output each line. */
      for (line = 0; line < Dynarr_length (ddla); line++)
	{
	  output_display_line (w, cdla, ddla, line, -1, -1);
	}

      /* If the number of display lines has shrunk, adjust. */
      if (cdla_len > Dynarr_length (ddla))
	{
	  Dynarr_set_lengthr (cdla, Dynarr_length (ddla));
	}

      /* grab coordinates of last line and blank after it. */
      if (Dynarr_length (ddla) > 0)
	{
	  dl = Dynarr_lastp (ddla);
	  ypos = dl->ypos + dl->descent - dl->clip;
	}
      else
	ypos = y;

      redisplay_clear_region (window, findex, x + border_width , ypos,
			      width - 2 * border_width, height - (ypos - y) - border_width);
      /* If, for some reason, we have more to display than we have
	 room for, and we are allowed to resize the gutter, then make
	 sure this happens before the next time we try and
	 output. This can happen when face font sizes change. */
      if (dl && EQ (w->gutter_size[pos], Qautodetect)
	  && (dl->clip > 0 ||
	      calculate_gutter_size_from_display_lines (pos, ddla) >
	      WINDOW_GUTTER_SIZE_INTERNAL (w, pos)))
	{
	  /* #### Ideally we would just mark the specifier as dirty
	  and everything else would "just work". Unfortunately we have
	  two problems with this. One is that the specifier cache
	  won't be recalculated unless the specifier code thinks the
	  cached value has actually changed, even though we have
	  marked the specifier as dirty. Additionally, although doing
	  this results in a gutter size change, we never seem to get
	  back into redisplay so that the frame size can be updated. I
	  think this is because we are already in redisplay and later
	  on the frame will be marked as clean. Thus we also have to
	  force a pending recalculation of the frame size.  */
	  w->gutter_size[pos] = Qnil;
	  Fset_specifier_dirty_flag (Vgutter_size[pos]);
	  update_gutter_geometry (f, pos);
	}

      /* bevel the gutter area if so desired */
      if (border_width != 0)
	{
	  MAYBE_DEVMETH (d, bevel_area,
			 (w, findex, x, y, width, height, border_width,
			  EDGE_ALL, EDGE_BEVEL_OUT));
	}
    }
  else
    {
      /* Nothing of significance happened so sync the display line
	 structs. */
      for (line = 0; line < Dynarr_length (ddla); line++)
	{
	  sync_display_line_structs (w, line, 1, cdla, ddla);
	}
    }

  w->gutter_extent_modiff [pos] = 0;
}

static void
clear_gutter (struct frame *f, enum edge_pos pos)
{
  int x, y, width, height;
  Lisp_Object window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  face_index findex = get_builtin_face_cache_index (XWINDOW (window),
						    Vwidget_face);
  get_gutter_coords (f, pos, &x, &y, &width, &height);

  f->gutter_was_visible[pos] = 0;

  redisplay_clear_region (window, findex, x, y, width, height);
}

/* [[#### I don't currently believe that redisplay needs to mark the
   glyphs in its structures since these will always be referenced from
   somewhere else. However, I'm not sure enough to stake my life on it
   at this point, so we do the safe thing.]]

   ALWAYS mark everything. --ben */

/* See the comment in image_instantiate_cache_result as to why marking
   the glyph will also mark the image_instance. */
void
mark_gutters (struct frame *f)
{
  enum edge_pos pos;
  EDGE_POS_LOOP (pos)
    {
      if (f->current_display_lines[pos])
	mark_redisplay_structs (f->current_display_lines[pos]);
      /* [[#### Do we really need to mark the desired lines?]]
	 ALWAYS mark everything. --ben */
      if (f->desired_display_lines[pos])
	mark_redisplay_structs (f->desired_display_lines[pos]);
    }
}

/* This is called by extent_changed_for_redisplay, so that redisplay
   knows exactly what extents have changed. */
void
gutter_extent_signal_changed_region_maybe (Lisp_Object obj,
					   Bytexpos UNUSED (start),
					   Bytexpos UNUSED (end))
{
  /* #### Start and end are currently ignored but could be used by a
     more optimal gutter redisplay. We currently loop over all frames
     here, this could be optimized. */
  Lisp_Object frmcons, devcons, concons;

  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
    {
      struct frame *f = XFRAME (XCAR (frmcons));
      enum edge_pos pos;
      Lisp_Object window = FRAME_LAST_NONMINIBUF_WINDOW (f);
      struct window* w = XWINDOW (window);

      EDGE_POS_LOOP (pos)
	{
	  if (EQ (WINDOW_GUTTER (w, pos), obj))
	    {
	      w->gutter_extent_modiff[pos]++;
	    }
	}
    }
}

/* We have to change the gutter geometry separately to the gutter
   update since it needs to occur outside of redisplay proper. */
static void
update_gutter_geometry (struct frame *f, enum edge_pos pos)
{
  /* If the gutter geometry has changed then re-layout the
     frame. If we are in display there is almost no point in doing
     anything else since the frame size changes will be delayed
     until we are out of redisplay proper. */
  if (FRAME_GUTTER_BOUNDS (f, pos) != f->current_gutter_bounds[pos])
    {
      int width, height;
      pixel_to_frame_unit_size (f, FRAME_PIXWIDTH (f), FRAME_PIXHEIGHT (f),
			  &width, &height);
      change_frame_size (f, width, height, 0);
      MARK_FRAME_LAYOUT_CHANGED (f);
    }

  /* Mark sizes as up-to-date. */
  f->current_gutter_bounds[pos] = FRAME_GUTTER_BOUNDS (f, pos);
}

void
update_frame_gutter_geometry (struct frame *f)
{
  if (f->gutter_changed
      || f->frame_layout_changed
      || f->windows_structure_changed)
    {
      enum edge_pos pos;

      /* If the gutter geometry has changed then re-layout the
	 frame. If we are in display there is almost no point in doing
	 anything else since the frame size changes will be delayed
	 until we are out of redisplay proper. */
      EDGE_POS_LOOP (pos)
	{
	  update_gutter_geometry (f, pos);
	}
    }
}

void
update_frame_gutters (struct frame *f)
{
  if (f->faces_changed || f->frame_changed ||
      f->gutter_changed || f->glyphs_changed ||
      f->size_changed || f->subwindows_changed ||
      f->windows_changed || f->windows_structure_changed ||
      f->extents_changed || f->frame_layout_changed)
    {
      enum edge_pos pos;

      /* We don't actually care about these when outputting the gutter
	 so locally disable them. */
      int local_clip_changed = f->clip_changed;
      int local_buffers_changed = f->buffers_changed;
      f->clip_changed = 0;
      f->buffers_changed = 0;

      /* and output */
      EDGE_POS_LOOP (pos)
	{
	  if (FRAME_GUTTER_VISIBLE (f, pos))
	      output_gutter (f, pos, 0);

	  else if (f->gutter_was_visible[pos])
	      clear_gutter (f, pos);
	}

      f->clip_changed = local_clip_changed;
      f->buffers_changed = local_buffers_changed;
      f->gutter_changed = 0;
    }
}

void
reset_gutter_display_lines (struct frame* f)
{
  enum edge_pos pos;
  EDGE_POS_LOOP (pos)
    {
      if (f->current_display_lines[pos])
	Dynarr_reset (f->current_display_lines[pos]);
    }
}

static void
redraw_exposed_gutter (struct frame *f, enum edge_pos pos, int x, int y,
		       int width, int height)
{
  int g_x, g_y, g_width, g_height;

  get_gutter_coords (f, pos, &g_x, &g_y, &g_width, &g_height);

  if (((y + height) < g_y) || (y > (g_y + g_height)) || !height || !width || !g_height || !g_width)
    return;
  if (((x + width) < g_x) || (x > (g_x + g_width)))
    return;

#ifdef DEBUG_WIDGETS
  stderr_out ("redrawing gutter after expose %d+%d, %dx%d\n",
	  x, y, width, height);
#endif
  /* #### optimize this - redrawing the whole gutter for every expose
     is very expensive. We reset the current display lines because if
     they're being exposed they are no longer current. */
  reset_gutter_display_lines (f);

  /* Even if none of the gutter is in the area, the blank region at
     the very least must be because the first thing we did is verify
     that some portion of the gutter is in the exposed region. */
  output_gutter (f, pos, 1);
}

void
redraw_exposed_gutters (struct frame *f, int x, int y, int width,
			int height)
{
  enum edge_pos pos;

  /* We are already inside the critical section -- our caller did that. */
  EDGE_POS_LOOP (pos)
    {
      if (FRAME_GUTTER_VISIBLE (f, pos))
	redraw_exposed_gutter (f, pos, x, y, width, height);
    }
}

void
free_frame_gutters (struct frame *f)
{
  enum edge_pos pos;
  EDGE_POS_LOOP (pos)
    {
      if (f->current_display_lines[pos])
	{
	  free_display_lines (f->current_display_lines[pos]);
	  f->current_display_lines[pos] = 0;
	}
      if (f->desired_display_lines[pos])
	{
	  free_display_lines (f->desired_display_lines[pos]);
	  f->desired_display_lines[pos] = 0;
	}
    }
}

static enum edge_pos
decode_gutter_position (Lisp_Object position)
{
  if (EQ (position, Qtop))    return TOP_EDGE;
  if (EQ (position, Qbottom)) return BOTTOM_EDGE;
  if (EQ (position, Qleft))   return LEFT_EDGE;
  if (EQ (position, Qright))  return RIGHT_EDGE;
  invalid_constant ("Invalid gutter position", position);

  RETURN_NOT_REACHED (TOP_EDGE);
}

DEFUN ("set-default-gutter-position", Fset_default_gutter_position, 1, 1, 0, /*
Set the position that the `default-gutter' will be displayed at.
Valid positions are `top', `bottom', `left' and `right'.
See `default-gutter-position'.
*/
       (position))
{
  enum edge_pos cur = decode_gutter_position (Vdefault_gutter_position);
  enum edge_pos new_ = decode_gutter_position (position);

  if (cur != new_)
    {
      /* The following calls will automatically cause the dirty
	 flags to be set; we delay frame size changes to avoid
	 lots of frame flickering. */
      /* #### I think this should be GC protected. -sb */
      int depth = begin_hold_frame_size_changes ();

      set_specifier_fallback (Vgutter[cur], list1 (Fcons (Qnil, Qnil)));
      set_specifier_fallback (Vgutter[new_], Vdefault_gutter);
      set_specifier_fallback (Vgutter_size[cur], list1 (Fcons (Qnil, Qzero)));
      set_specifier_fallback (Vgutter_size[new_],
			      new_ == TOP_EDGE || new_ == BOTTOM_EDGE
			      ? Vdefault_gutter_height
			      : Vdefault_gutter_width);
      set_specifier_fallback (Vgutter_border_width[cur],
			      list1 (Fcons (Qnil, Qzero)));
      set_specifier_fallback (Vgutter_border_width[new_],
			      Vdefault_gutter_border_width);
      set_specifier_fallback (Vgutter_visible_p[cur], list1 (Fcons (Qnil, Qt)));
      set_specifier_fallback (Vgutter_visible_p[new_], Vdefault_gutter_visible_p);
      Vdefault_gutter_position = position;

      unbind_to (depth);
    }

  run_hook (Qdefault_gutter_position_changed_hook);

  return position;
}

DEFUN ("default-gutter-position", Fdefault_gutter_position, 0, 0, 0, /*
Return the position that the `default-gutter' will be displayed at.
The `default-gutter' will only be displayed here if the corresponding
position-specific gutter specifier does not provide a value.
*/
       ())
{
  return Vdefault_gutter_position;
}

DEFUN ("gutter-pixel-width", Fgutter_pixel_width, 0, 2, 0, /*
Return the pixel width of the gutter at POS in LOCALE.
POS defaults to the default gutter position. LOCALE defaults to
the current window.
*/
       (pos, locale))
{
  int x, y, width, height;
  enum edge_pos p = TOP_EDGE;
  struct frame *f = decode_frame (FW_FRAME (locale));

  if (NILP (pos))
    pos = Vdefault_gutter_position;
  p = decode_gutter_position (pos);

  get_gutter_coords (f, p, &x, &y, &width, &height);
  width -= (FRAME_GUTTER_BORDER_WIDTH (f, p) * 2);

  return make_fixnum (width);
}

DEFUN ("gutter-pixel-height", Fgutter_pixel_height, 0, 2, 0, /*
Return the pixel height of the gutter at POS in LOCALE.
POS defaults to the default gutter position. LOCALE defaults to
the current window.
*/
       (pos, locale))
{
  int x, y, width, height;
  enum edge_pos p = TOP_EDGE;
  struct frame *f = decode_frame (FW_FRAME (locale));

  if (NILP (pos))
    pos = Vdefault_gutter_position;
  p = decode_gutter_position (pos);

  get_gutter_coords (f, p, &x, &y, &width, &height);
  height -= (FRAME_GUTTER_BORDER_WIDTH (f, p) * 2);

  return make_fixnum (height);
}

DEFINE_SPECIFIER_TYPE (gutter);

static void
gutter_after_change (Lisp_Object UNUSED (specifier),
		     Lisp_Object UNUSED (locale))
{
  MARK_GUTTER_CHANGED;
}

static void
gutter_validate (Lisp_Object instantiator)
{
  if (NILP (instantiator))
    return;

  /* Must be a string or a plist. */
  if (!STRINGP (instantiator) && NILP (Fvalid_plist_p (instantiator)))
      sferror ("Gutter spec must be string, plist or nil", instantiator);

  if (!STRINGP (instantiator))
    {
      Lisp_Object rest;

      for (rest = instantiator; !NILP (rest); rest = XCDR (XCDR (rest)))
	{
	  if (!SYMBOLP (XCAR (rest))
	      || !STRINGP (XCAR (XCDR (rest))))
	    sferror ("Gutter plist spec must contain strings", instantiator);
	}
    }
}

DEFUN ("gutter-specifier-p", Fgutter_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a gutter specifier.

See `make-gutter-specifier' for a description of possible gutter
instantiators.
*/
       (object))
{
  return GUTTER_SPECIFIERP (object) ? Qt : Qnil;
}


/*
  Helper for invalidating the real specifier when default
  specifier caching changes
*/
static void
recompute_overlaying_specifier (Lisp_Object real_one[NUM_EDGES])
{
  enum edge_pos pos = decode_gutter_position (Vdefault_gutter_position);
  Fset_specifier_dirty_flag (real_one[pos]);
}

static void gutter_specs_changed (Lisp_Object specifier, struct window *w,
				  Lisp_Object oldval, enum edge_pos pos);

static void
gutter_specs_changed_1 (Lisp_Object arg)
{
  gutter_specs_changed (X1ST (arg), XWINDOW (X2ND (arg)),
			X3RD (arg), (enum edge_pos) XFIXNUM (X4TH (arg)));
  free_list (arg);
}

static void
gutter_specs_changed (Lisp_Object specifier, struct window *w,
		      Lisp_Object oldval, enum edge_pos pos)
{
  if (in_display)
    register_post_redisplay_action (gutter_specs_changed_1,
				    list4 (specifier, wrap_window (w),
					   oldval, make_fixnum (pos)));
  else
    {
      w->real_gutter[pos] = construct_window_gutter_spec (w, pos);
      w->real_gutter_size[pos] = w->gutter_size[pos];

      if (EQ (w->real_gutter_size[pos], Qautodetect)
	  && !NILP (w->gutter_visible_p[pos]))
	{
	  w->real_gutter_size [pos] = calculate_gutter_size (w, pos);
	}
      MARK_GUTTER_CHANGED;
      MARK_MODELINE_CHANGED;
      MARK_WINDOWS_CHANGED (w);
    }
}

/* We define all of these so we can access which actual gutter changed. */
static void
top_gutter_specs_changed (Lisp_Object specifier, struct window *w,
			  Lisp_Object oldval)
{
  gutter_specs_changed (specifier, w, oldval, TOP_EDGE);
}

static void
bottom_gutter_specs_changed (Lisp_Object specifier, struct window *w,
			     Lisp_Object oldval)
{
  gutter_specs_changed (specifier, w, oldval, BOTTOM_EDGE);
}

static void
left_gutter_specs_changed (Lisp_Object specifier, struct window *w,
			   Lisp_Object oldval)
{
  gutter_specs_changed (specifier, w, oldval, LEFT_EDGE);
}

static void
right_gutter_specs_changed (Lisp_Object specifier, struct window *w,
			    Lisp_Object oldval)
{
  gutter_specs_changed (specifier, w, oldval, RIGHT_EDGE);
}

static void
default_gutter_specs_changed (Lisp_Object UNUSED (specifier),
			      struct window *UNUSED (w),
			      Lisp_Object UNUSED (oldval))
{
  recompute_overlaying_specifier (Vgutter);
}

static void gutter_geometry_changed_in_window (Lisp_Object specifier,
					       struct window *w,
					       Lisp_Object oldval);

static void
gutter_geometry_changed_in_window_1 (Lisp_Object arg)
{
  gutter_geometry_changed_in_window (X1ST (arg), XWINDOW (X2ND (arg)),
				     X3RD (arg));
  free_list (arg);
}

static void
gutter_geometry_changed_in_window (Lisp_Object specifier, struct window *w,
				    Lisp_Object oldval)
{
  if (in_display)
    register_post_redisplay_action (gutter_geometry_changed_in_window_1,
				    list3 (specifier, wrap_window (w),
					   oldval));
  else
    {
      enum edge_pos pos;
      EDGE_POS_LOOP (pos)
	{
	  w->real_gutter_size[pos] = w->gutter_size[pos];
	  if (EQ (w->real_gutter_size[pos], Qautodetect)
	      && !NILP (w->gutter_visible_p[pos]))
	    {
	      w->real_gutter_size [pos] = calculate_gutter_size (w, pos);
	    }
	}

      MARK_GUTTER_CHANGED;
      MARK_MODELINE_CHANGED;
      MARK_WINDOWS_CHANGED (w);
    }
}

static void
default_gutter_size_changed_in_window (Lisp_Object UNUSED (specifier),
				       struct window *UNUSED (w),
				       Lisp_Object UNUSED (oldval))
{
  recompute_overlaying_specifier (Vgutter_size);
}

static void
default_gutter_border_width_changed_in_window (Lisp_Object UNUSED (specifier),
					       struct window *UNUSED (w),
					       Lisp_Object UNUSED (oldval))
{
  recompute_overlaying_specifier (Vgutter_border_width);
}

static void
default_gutter_visible_p_changed_in_window (Lisp_Object UNUSED (specifier),
					    struct window *UNUSED (w),
					    Lisp_Object UNUSED (oldval))
{
  recompute_overlaying_specifier (Vgutter_visible_p);
  /* Need to reconstruct the gutter specifier as it is affected by the
     visibility. */
  recompute_overlaying_specifier (Vgutter);
}


/* No need for the declaration, we don't use the
   error_check_gutter_size_{type,data} functions. */
/* DECLARE_SPECIFIER_TYPE (gutter_size); */
#define GUTTER_SIZE_SPECIFIERP(x) SPECIFIER_TYPEP (x, gutter_size)
DEFINE_SPECIFIER_TYPE (gutter_size);

static void
gutter_size_validate (Lisp_Object instantiator)
{
  if (NILP (instantiator))
    return;

  if (!FIXNUMP (instantiator) && !EQ (instantiator, Qautodetect))
    invalid_argument ("Gutter size must be an integer or `autodetect'", instantiator);
}

DEFUN ("gutter-size-specifier-p", Fgutter_size_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a gutter-size specifier.

See `make-gutter-size-specifier' for a description of possible gutter-size
instantiators.
*/
       (object))
{
  return GUTTER_SIZE_SPECIFIERP (object) ? Qt : Qnil;
}

/* No need for the declaration, we don't use the
   error_check_gutter_visible_{type,data} functions. */
/* DECLARE_SPECIFIER_TYPE (gutter_visible); */
#define GUTTER_VISIBLE_SPECIFIERP(x) SPECIFIER_TYPEP (x, gutter_visible)
DEFINE_SPECIFIER_TYPE (gutter_visible);

static void
gutter_visible_validate (Lisp_Object instantiator)
{
  if (NILP (instantiator))
    return;

  if (!NILP (instantiator) && !EQ (instantiator, Qt) && !CONSP (instantiator))
    invalid_argument ("Gutter visibility must be a boolean or list of symbols",
			 instantiator);

  if (CONSP (instantiator))
    {
      EXTERNAL_LIST_LOOP_2 (elt, instantiator)
	{
	  if (!SYMBOLP (elt))
	      invalid_argument ("Gutter visibility must be a boolean or list of symbols",
				   instantiator);
	}
    }
}

DEFUN ("gutter-visible-specifier-p", Fgutter_visible_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a gutter-visible specifier.

See `make-gutter-visible-specifier' for a description of possible
gutter-visible instantiators.
*/
       (object))
{
  return GUTTER_VISIBLE_SPECIFIERP (object) ? Qt : Qnil;
}

DEFUN ("redisplay-gutter-area", Fredisplay_gutter_area, 0, 0, 0, /*
Ensure that all gutters are correctly showing their gutter specifier.
*/
       ())
{
  Lisp_Object devcons, concons;

  /* Can't reentrantly enter redisplay */
  if (in_display)
    return Qnil;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      Lisp_Object frmcons;

      DEVICE_FRAME_LOOP (frmcons, d)
	{
	  struct frame *f = XFRAME (XCAR (frmcons));

	  MAYBE_DEVMETH (d, frame_output_begin, (f));

	  /* Sequence is quite important here. We not only want to
	   redisplay the gutter area but we also want to flush any
	   frame size changes out so that the gutter redisplay happens
	   in a kosha environment.

	   This is not only so that things look right but so that
	   glyph redisplay optimization kicks in, by default display
	   lines will be completely re-output if
	   f->windows_structure_changed is 1, and this is true if
	   frame size changes haven't been flushed out. Once frame
	   size changes have been flushed out we then need to
	   redisplay the frame in order to flush out pending window
	   size changes. */
	  update_frame_gutter_geometry (f);

	  if (f->windows_structure_changed)
	    redisplay_frame (f, 1);
	  else if (FRAME_REPAINT_P (f))
	    {
	      int depth;

	      /* We have to be "in display" when we output the gutter
		 - make it so. */
	      depth = enter_redisplay_critical_section ();
	      update_frame_gutters (f);
	      exit_redisplay_critical_section (depth);
	    }

	  MAYBE_DEVMETH (d, frame_output_end, (f));
      }

      d->gutter_changed = 0;
    }

  /* This is so that further changes to the gutters will trigger redisplay. */
  gutter_changed_set = 0;
  gutter_changed = 0;

  return Qnil;
}

void
init_frame_gutters (struct frame *f)
{
  enum edge_pos pos;
  struct window* w = XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f));
  /* We are here as far in frame creation so cached specifiers are
     already recomputed, and possibly modified by resource
     initialization. We need to recalculate autodetected gutters. */
  EDGE_POS_LOOP (pos)
    {
      w->real_gutter[pos] = construct_window_gutter_spec (w, pos);
      w->real_gutter_size[pos] = w->gutter_size[pos];
      if (EQ (w->gutter_size[pos], Qautodetect)
	  && !NILP (w->gutter_visible_p[pos]))
	{
	  w->real_gutter_size [pos] = calculate_gutter_size (w, pos);
	  MARK_GUTTER_CHANGED;
	  MARK_WINDOWS_CHANGED (w);
	}
    }

  /* Keep a record of the current sizes of things. */
  EDGE_POS_LOOP (pos)
    {
      f->current_gutter_bounds[pos] = FRAME_GUTTER_BOUNDS (f, pos);
    }
}

void
syms_of_gutter (void)
{
  DEFSUBR (Fgutter_specifier_p);
  DEFSUBR (Fgutter_size_specifier_p);
  DEFSUBR (Fgutter_visible_specifier_p);
  DEFSUBR (Fset_default_gutter_position);
  DEFSUBR (Fdefault_gutter_position);
  DEFSUBR (Fgutter_pixel_height);
  DEFSUBR (Fgutter_pixel_width);
  DEFSUBR (Fredisplay_gutter_area);

  DEFSYMBOL (Qgutter_size);
  DEFSYMBOL (Qgutter_visible);
  DEFSYMBOL (Qdefault_gutter_position_changed_hook);
}

void
vars_of_gutter (void)
{
  staticpro (&Vdefault_gutter_position);
  Vdefault_gutter_position = Qtop;

  Fprovide (Qgutter);
}

void
specifier_type_create_gutter (void)
{
  INITIALIZE_SPECIFIER_TYPE (gutter, "gutter", "gutter-specifier-p");
  SPECIFIER_HAS_METHOD (gutter, validate);
  SPECIFIER_HAS_METHOD (gutter, after_change);

  INITIALIZE_SPECIFIER_TYPE (gutter_size, "gutter-size", "gutter-size-specifier-p");
  SPECIFIER_HAS_METHOD (gutter_size, validate);

  INITIALIZE_SPECIFIER_TYPE (gutter_visible, "gutter-visible", "gutter-visible-specifier-p");
  SPECIFIER_HAS_METHOD (gutter_visible, validate);
}

void
reinit_specifier_type_create_gutter (void)
{
  REINITIALIZE_SPECIFIER_TYPE (gutter);
  REINITIALIZE_SPECIFIER_TYPE (gutter_size);
  REINITIALIZE_SPECIFIER_TYPE (gutter_visible);
}

void
specifier_vars_of_gutter (void)
{
  Lisp_Object fb;

  DEFVAR_SPECIFIER ("default-gutter", &Vdefault_gutter /*
Specifier for a fallback gutter.
Use `set-specifier' to change this.

The position of this gutter is specified in the function
`default-gutter-position'.  If the corresponding position-specific
gutter (e.g. `top-gutter' if `default-gutter-position' is `top')
does not specify a gutter in a particular domain (usually a window),
then the value of `default-gutter' in that domain, if any, will be
used instead.

Note that the gutter at any particular position will not be
displayed unless its visibility flag is true and its thickness
\(width or height, depending on orientation) is non-zero.  The
visibility is controlled by the specifiers `top-gutter-visible-p',
`bottom-gutter-visible-p', `left-gutter-visible-p', and
`right-gutter-visible-p', and the thickness is controlled by the
specifiers `top-gutter-height', `bottom-gutter-height',
`left-gutter-width', and `right-gutter-width'.

Note that one of the four visibility specifiers inherits from
`default-gutter-visibility' and one of the four thickness
specifiers inherits from either `default-gutter-width' or
`default-gutter-height' (depending on orientation), just
like for the gutter description specifiers (e.g. `top-gutter')
mentioned above.

Therefore, if you are setting `default-gutter', you should control
the visibility and thickness using `default-gutter-visible-p',
`default-gutter-width', and `default-gutter-height', rather than
using position-specific specifiers.  That way, you will get sane
behavior if the user changes the default gutter position.

The gutter value should be a string, a property list of strings or
nil. You can attach extents and glyphs to the string and hence display
glyphs and text in other fonts in the gutter area. If the gutter value
is a property list then the strings will be concatenated together
before being displayed.  */ );

  Vdefault_gutter = Fmake_specifier (Qgutter);
  /* #### It would be even nicer if the specifier caching
     automatically knew about specifier fallbacks, so we didn't
     have to do it ourselves. */
  set_specifier_caching (Vdefault_gutter,
			 offsetof (struct window, default_gutter),
			 default_gutter_specs_changed,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("top-gutter",
		    &Vgutter[TOP_EDGE] /*
Specifier for the gutter at the top of the frame.
Use `set-specifier' to change this.
See `default-gutter' for a description of a valid gutter instantiator.
*/ );
  Vgutter[TOP_EDGE] = Fmake_specifier (Qgutter);
  set_specifier_caching (Vgutter[TOP_EDGE],
			 offsetof (struct window, gutter[TOP_EDGE]),
			 top_gutter_specs_changed,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("bottom-gutter",
		    &Vgutter[BOTTOM_EDGE] /*
Specifier for the gutter at the bottom of the frame.
Use `set-specifier' to change this.
See `default-gutter' for a description of a valid gutter instantiator.

Note that, unless the `default-gutter-position' is `bottom', by
default the height of the bottom gutter (controlled by
`bottom-gutter-height') is 0; thus, a bottom gutter will not be
displayed even if you provide a value for `bottom-gutter'.
*/ );
  Vgutter[BOTTOM_EDGE] = Fmake_specifier (Qgutter);
  set_specifier_caching (Vgutter[BOTTOM_EDGE],
			 offsetof (struct window, gutter[BOTTOM_EDGE]),
			 bottom_gutter_specs_changed,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("left-gutter",
		    &Vgutter[LEFT_EDGE] /*
Specifier for the gutter at the left edge of the frame.
Use `set-specifier' to change this.
See `default-gutter' for a description of a valid gutter instantiator.

Note that, unless the `default-gutter-position' is `left', by
default the height of the left gutter (controlled by
`left-gutter-width') is 0; thus, a left gutter will not be
displayed even if you provide a value for `left-gutter'.
*/ );
  Vgutter[LEFT_EDGE] = Fmake_specifier (Qgutter);
  set_specifier_caching (Vgutter[LEFT_EDGE],
			 offsetof (struct window, gutter[LEFT_EDGE]),
			 left_gutter_specs_changed,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("right-gutter",
		    &Vgutter[RIGHT_EDGE] /*
Specifier for the gutter at the right edge of the frame.
Use `set-specifier' to change this.
See `default-gutter' for a description of a valid gutter instantiator.

Note that, unless the `default-gutter-position' is `right', by
default the height of the right gutter (controlled by
`right-gutter-width') is 0; thus, a right gutter will not be
displayed even if you provide a value for `right-gutter'.
*/ );
  Vgutter[RIGHT_EDGE] = Fmake_specifier (Qgutter);
  set_specifier_caching (Vgutter[RIGHT_EDGE],
			 offsetof (struct window, gutter[RIGHT_EDGE]),
			 right_gutter_specs_changed,
			 0, 0, 1);

  /* initially, top inherits from default; this can be
     changed with `set-default-gutter-position'. */
  fb = list1 (Fcons (Qnil, Qnil));
  set_specifier_fallback (Vdefault_gutter, fb);
  set_specifier_fallback (Vgutter[TOP_EDGE], Vdefault_gutter);
  set_specifier_fallback (Vgutter[BOTTOM_EDGE], fb);
  set_specifier_fallback (Vgutter[LEFT_EDGE],   fb);
  set_specifier_fallback (Vgutter[RIGHT_EDGE],  fb);

  DEFVAR_SPECIFIER ("default-gutter-height", &Vdefault_gutter_height /*
*Height of the default gutter, if it's oriented horizontally.
This is a specifier; use `set-specifier' to change it.

The position of the default gutter is specified by the function
`set-default-gutter-position'.  If the corresponding position-specific
gutter thickness specifier (e.g. `top-gutter-height' if
`default-gutter-position' is `top') does not specify a thickness in a
particular domain (a window or a frame), then the value of
`default-gutter-height' or `default-gutter-width' (depending on the
gutter orientation) in that domain, if any, will be used instead.

Note that `default-gutter-height' is only used when
`default-gutter-position' is `top' or `bottom', and `default-gutter-width'
is only used when `default-gutter-position' is `left' or `right'.

Note that all of the position-specific gutter thickness specifiers
have a fallback value of zero when they do not correspond to the
default gutter.  Therefore, you will have to set a non-zero thickness
value if you want a position-specific gutter to be displayed.

If you set the height to `autodetect' the size of the gutter will be
calculated to be large enough to hold the contents of the gutter. This
is the default.
*/ );
  Vdefault_gutter_height = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vdefault_gutter_height,
			 offsetof (struct window, default_gutter_height),
			 default_gutter_size_changed_in_window,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("default-gutter-width", &Vdefault_gutter_width /*
*Width of the default gutter, if it's oriented vertically.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vdefault_gutter_width = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vdefault_gutter_width,
			 offsetof (struct window, default_gutter_width),
			 default_gutter_size_changed_in_window,
			 0, 0, 1);

  DEFVAR_SPECIFIER ("top-gutter-height",
		    &Vgutter_size[TOP_EDGE] /*
*Height of the top gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_size[TOP_EDGE] = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vgutter_size[TOP_EDGE],
			 offsetof (struct window, gutter_size[TOP_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 1);

  DEFVAR_SPECIFIER ("bottom-gutter-height",
		    &Vgutter_size[BOTTOM_EDGE] /*
*Height of the bottom gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_size[BOTTOM_EDGE] = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vgutter_size[BOTTOM_EDGE],
			 offsetof (struct window, gutter_size[BOTTOM_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 1);

  DEFVAR_SPECIFIER ("left-gutter-width",
		    &Vgutter_size[LEFT_EDGE] /*
*Width of left gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_size[LEFT_EDGE] = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vgutter_size[LEFT_EDGE],
			 offsetof (struct window, gutter_size[LEFT_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 1);

  DEFVAR_SPECIFIER ("right-gutter-width",
		    &Vgutter_size[RIGHT_EDGE] /*
*Width of right gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_size[RIGHT_EDGE] = Fmake_specifier (Qgutter_size);
  set_specifier_caching (Vgutter_size[RIGHT_EDGE],
			 offsetof (struct window, gutter_size[RIGHT_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 1);

  fb = Qnil;
#ifdef HAVE_TTY
  fb = Fcons (Fcons (list1 (Qtty), Qautodetect), fb);
#endif
#ifdef HAVE_GTK
  fb = Fcons (Fcons (list1 (Qgtk), Qautodetect), fb);
#endif
#ifdef HAVE_X_WINDOWS
  fb = Fcons (Fcons (list1 (Qx), Qautodetect), fb);
#endif
#ifdef HAVE_MS_WINDOWS
  fb = Fcons (Fcons (list1 (Qmsprinter), Qautodetect), fb);
  fb = Fcons (Fcons (list1 (Qmswindows), Qautodetect), fb);
#endif
  if (!NILP (fb))
    set_specifier_fallback (Vdefault_gutter_height, fb);

  fb = Qnil;
#ifdef HAVE_TTY
  fb = Fcons (Fcons (list1 (Qtty), Qautodetect), fb);
#endif
#ifdef HAVE_X_WINDOWS
  fb = Fcons (Fcons (list1 (Qx), Qautodetect), fb);
#endif
#ifdef HAVE_GTK
  fb = Fcons (Fcons (list1 (Qgtk), Qautodetect), fb);
#endif
#ifdef HAVE_MS_WINDOWS
  fb = Fcons (Fcons (list1 (Qmsprinter), Qautodetect), fb);
  fb = Fcons (Fcons (list1 (Qmswindows), Qautodetect), fb);
#endif
  if (!NILP (fb))
    set_specifier_fallback (Vdefault_gutter_width, fb);

  set_specifier_fallback (Vgutter_size[TOP_EDGE], Vdefault_gutter_height);
  fb = list1 (Fcons (Qnil, Qzero));
  set_specifier_fallback (Vgutter_size[BOTTOM_EDGE], fb);
  set_specifier_fallback (Vgutter_size[LEFT_EDGE],   fb);
  set_specifier_fallback (Vgutter_size[RIGHT_EDGE],  fb);

  DEFVAR_SPECIFIER ("default-gutter-border-width",
		    &Vdefault_gutter_border_width /*
*Width of the border around the default gutter.
This is a specifier; use `set-specifier' to change it.

The position of the default gutter is specified by the function
`set-default-gutter-position'.  If the corresponding position-specific
gutter border width specifier (e.g. `top-gutter-border-width' if
`default-gutter-position' is `top') does not specify a border width in a
particular domain (a window or a frame), then the value of
`default-gutter-border-width' in that domain, if any, will be used
instead.

*/ );
  Vdefault_gutter_border_width = Fmake_specifier (Qnatnum);
  set_specifier_caching (Vdefault_gutter_border_width,
			 offsetof (struct window, default_gutter_border_width),
			 default_gutter_border_width_changed_in_window,
			 0, 0, 0);

  DEFVAR_SPECIFIER ("top-gutter-border-width",
		    &Vgutter_border_width[TOP_EDGE] /*
*Border width of the top gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_border_width[TOP_EDGE] = Fmake_specifier (Qnatnum);
  set_specifier_caching (Vgutter_border_width[TOP_EDGE],
			 offsetof (struct window,
				   gutter_border_width[TOP_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 0);

  DEFVAR_SPECIFIER ("bottom-gutter-border-width",
		    &Vgutter_border_width[BOTTOM_EDGE] /*
*Border width of the bottom gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_border_width[BOTTOM_EDGE] = Fmake_specifier (Qnatnum);
  set_specifier_caching (Vgutter_border_width[BOTTOM_EDGE],
			 offsetof (struct window,
				   gutter_border_width[BOTTOM_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 0);

  DEFVAR_SPECIFIER ("left-gutter-border-width",
		    &Vgutter_border_width[LEFT_EDGE] /*
*Border width of left gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_border_width[LEFT_EDGE] = Fmake_specifier (Qnatnum);
  set_specifier_caching (Vgutter_border_width[LEFT_EDGE],
			 offsetof (struct window,
				   gutter_border_width[LEFT_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 0);

  DEFVAR_SPECIFIER ("right-gutter-border-width",
		    &Vgutter_border_width[RIGHT_EDGE] /*
*Border width of right gutter.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-height' for more information.
*/ );
  Vgutter_border_width[RIGHT_EDGE] = Fmake_specifier (Qnatnum);
  set_specifier_caching (Vgutter_border_width[RIGHT_EDGE],
			 offsetof (struct window,
				   gutter_border_width[RIGHT_EDGE]),
			 gutter_geometry_changed_in_window, 0, 0, 0);

  fb = Qnil;
#ifdef HAVE_TTY
  fb = Fcons (Fcons (list1 (Qtty), Qzero), fb);
#endif
#ifdef HAVE_X_WINDOWS
  fb = Fcons (Fcons (list1 (Qx), make_fixnum (DEFAULT_GUTTER_BORDER_WIDTH)), fb);
#endif
#ifdef HAVE_MS_WINDOWS
  fb = Fcons (Fcons (list1 (Qmsprinter), Qzero), fb);
  fb = Fcons (Fcons (list1 (Qmswindows), make_fixnum (DEFAULT_GUTTER_BORDER_WIDTH)), fb);
#endif
  if (!NILP (fb))
    set_specifier_fallback (Vdefault_gutter_border_width, fb);

  set_specifier_fallback (Vgutter_border_width[TOP_EDGE], Vdefault_gutter_border_width);
  fb = list1 (Fcons (Qnil, Qzero));
  set_specifier_fallback (Vgutter_border_width[BOTTOM_EDGE], fb);
  set_specifier_fallback (Vgutter_border_width[LEFT_EDGE],   fb);
  set_specifier_fallback (Vgutter_border_width[RIGHT_EDGE],  fb);

  DEFVAR_SPECIFIER ("default-gutter-visible-p", &Vdefault_gutter_visible_p /*
*Whether the default gutter is visible.
This is a specifier; use `set-specifier' to change it.

The position of the default gutter is specified by the function
`set-default-gutter-position'.  If the corresponding position-specific
gutter visibility specifier (e.g. `top-gutter-visible-p' if
`default-gutter-position' is `top') does not specify a visible-p value
in a particular domain (a window or a frame), then the value of
`default-gutter-visible-p' in that domain, if any, will be used
instead.

`default-gutter-visible-p' and all of the position-specific gutter
visibility specifiers have a fallback value of true.
*/ );
  Vdefault_gutter_visible_p = Fmake_specifier (Qgutter_visible);
  set_specifier_caching (Vdefault_gutter_visible_p,
			 offsetof (struct window,
				   default_gutter_visible_p),
			 default_gutter_visible_p_changed_in_window,
			 0, 0, 0);

  DEFVAR_SPECIFIER ("top-gutter-visible-p",
		    &Vgutter_visible_p[TOP_EDGE] /*
*Whether the top gutter is visible.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-visible-p' for more information.
*/ );
  Vgutter_visible_p[TOP_EDGE] = Fmake_specifier (Qgutter_visible);
  set_specifier_caching (Vgutter_visible_p[TOP_EDGE],
			 offsetof (struct window,
				   gutter_visible_p[TOP_EDGE]),
			 top_gutter_specs_changed, 0, 0, 0);

  DEFVAR_SPECIFIER ("bottom-gutter-visible-p",
		    &Vgutter_visible_p[BOTTOM_EDGE] /*
*Whether the bottom gutter is visible.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-visible-p' for more information.
*/ );
  Vgutter_visible_p[BOTTOM_EDGE] = Fmake_specifier (Qgutter_visible);
  set_specifier_caching (Vgutter_visible_p[BOTTOM_EDGE],
			 offsetof (struct window,
				   gutter_visible_p[BOTTOM_EDGE]),
			 bottom_gutter_specs_changed, 0, 0, 0);

  DEFVAR_SPECIFIER ("left-gutter-visible-p",
		    &Vgutter_visible_p[LEFT_EDGE] /*
*Whether the left gutter is visible.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-visible-p' for more information.
*/ );
  Vgutter_visible_p[LEFT_EDGE] = Fmake_specifier (Qgutter_visible);
  set_specifier_caching (Vgutter_visible_p[LEFT_EDGE],
			 offsetof (struct window,
				   gutter_visible_p[LEFT_EDGE]),
			 left_gutter_specs_changed, 0, 0, 0);

  DEFVAR_SPECIFIER ("right-gutter-visible-p",
		    &Vgutter_visible_p[RIGHT_EDGE] /*
*Whether the right gutter is visible.
This is a specifier; use `set-specifier' to change it.

See `default-gutter-visible-p' for more information.
*/ );
  Vgutter_visible_p[RIGHT_EDGE] = Fmake_specifier (Qgutter_visible);
  set_specifier_caching (Vgutter_visible_p[RIGHT_EDGE],
			 offsetof (struct window,
				   gutter_visible_p[RIGHT_EDGE]),
			 right_gutter_specs_changed, 0, 0, 0);

  /* initially, top inherits from default; this can be
     changed with `set-default-gutter-position'. */
  fb = list1 (Fcons (Qnil, Qt));
  set_specifier_fallback (Vdefault_gutter_visible_p, fb);
  set_specifier_fallback (Vgutter_visible_p[TOP_EDGE],
			  Vdefault_gutter_visible_p);
  set_specifier_fallback (Vgutter_visible_p[BOTTOM_EDGE], fb);
  set_specifier_fallback (Vgutter_visible_p[LEFT_EDGE],   fb);
  set_specifier_fallback (Vgutter_visible_p[RIGHT_EDGE],  fb);
}
