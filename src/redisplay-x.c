/* X output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003 Ben Wing.

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

/* Synched up with:  Not in FSF. */

/* Author: Chuck Thompson */

/* Lots of work done by Ben Wing for Mule */


/* Number of pixels below each line. */
int x_interline_space; /* #### this needs to be implemented, but per-font */

#define THIS_IS_X
#include "redisplay-xlike-inc.c"

static void x_output_shadows (struct frame *f, int x, int y, int width,
			      int height, GC top_shadow_gc,
			      GC bottom_shadow_gc, GC background_gc,
			      int shadow_thickness, int edges);

/*****************************************************************************
 x_window_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void
XLIKE_window_output_begin (struct window *UNUSED (w))
{
}

/*****************************************************************************
 x_window_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void
XLIKE_window_output_end (struct window *w)
{
  if (!(check_if_pending_expose_event (WINDOW_XDEVICE (w))))
    XFlush (DEVICE_X_DISPLAY (WINDOW_XDEVICE (w)));
}

/*****************************************************************************
 x_bevel_area

 Draw shadows for the given area in the given face.
 ****************************************************************************/
static void
XLIKE_bevel_area (struct window *w, face_index findex,
		  int x, int y, int width, int height,
		  int shadow_thickness, int edges, enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  Pixel top_shadow_pixel, bottom_shadow_pixel, background_pixel;
  Lisp_Object tmp_pixel;
  XColor tmp_color;
  XGCValues gcv;
  GC top_shadow_gc, bottom_shadow_gc, background_gc;

  int use_pixmap = 0;
  int flip_gcs = 0;
  unsigned long mask;

  assert (shadow_thickness >=0);
  memset (&gcv, ~0, sizeof (XGCValues));

  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
  tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));

  /* First, get the GC's. */
  top_shadow_pixel = tmp_color.pixel;
  bottom_shadow_pixel = tmp_color.pixel;
  background_pixel = tmp_color.pixel;

  x_generate_shadow_pixels (f, &top_shadow_pixel, &bottom_shadow_pixel,
			    background_pixel, ef->core.background_pixel);

  tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
  tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
  gcv.background = tmp_color.pixel;
  gcv.graphics_exposures = False;
  mask = GCForeground | GCBackground | GCGraphicsExposures;

  /* If we can't distinguish one of the shadows (the color is the same as the
     background), it's better to use a pixmap to generate a dithered gray. */
  if (top_shadow_pixel == background_pixel ||
      bottom_shadow_pixel == background_pixel)
    use_pixmap = 1;

  if (use_pixmap)
    {
      if (DEVICE_X_GRAY_PIXMAP (d) == None)
	{
	  DEVICE_X_GRAY_PIXMAP (d) =
	    XCreatePixmapFromBitmapData (dpy, x_win, (char *) gray_bits,
					 gray_width, gray_height, 1, 0, 1);
	}

      tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
      tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
      gcv.foreground = tmp_color.pixel;
      /* this is needed because the GC draws with a pixmap here */
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple = DEVICE_X_GRAY_PIXMAP (d);
      top_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv,
				       (mask | GCStipple | GCFillStyle));

      tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
      tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
      bottom_shadow_pixel = tmp_color.pixel;

      flip_gcs = (bottom_shadow_pixel ==
		  WhitePixelOfScreen (DefaultScreenOfDisplay (dpy)));
    }
  else
    {
      gcv.foreground = top_shadow_pixel;
      top_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);
    }

  gcv.foreground = bottom_shadow_pixel;
  bottom_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);

  if (use_pixmap && flip_gcs)
    {
      GC tmp_gc = bottom_shadow_gc;
      bottom_shadow_gc = top_shadow_gc;
      top_shadow_gc = tmp_gc;
    }

  gcv.foreground = background_pixel;
  background_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);

  /* possibly revert the GC's This will give a depressed look to the
     divider */
  if (style == EDGE_ETCHED_IN || style == EDGE_BEVEL_IN)
    {
      GC temp;

      temp = top_shadow_gc;
      top_shadow_gc = bottom_shadow_gc;
      bottom_shadow_gc = temp;
    }

  if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT)
    shadow_thickness /= 2;

  /* Draw the shadows around the divider line */
  x_output_shadows (f, x, y, width, height,
		    top_shadow_gc, bottom_shadow_gc,
		    background_gc, shadow_thickness, edges);

  if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT)
    {
      /* Draw the shadows around the divider line */
      x_output_shadows (f, x + shadow_thickness, y + shadow_thickness,
			width - 2*shadow_thickness, height - 2*shadow_thickness,
			bottom_shadow_gc, top_shadow_gc,
			background_gc, shadow_thickness, edges);
    }
}

/*****************************************************************************
 x_output_shadows

 Draw a shadow around the given area using the given GC's.  It is the
 callers responsibility to set the GC's appropriately.
 ****************************************************************************/
static void
x_output_shadows (struct frame *f, int x, int y, int width, int height,
		  GC top_shadow_gc, GC bottom_shadow_gc,
		  GC UNUSED (background_gc), int shadow_thickness, int edges)
{
  struct device *d = XDEVICE (f->device);

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));

  XSegment top_shadow[20], bottom_shadow[20];
  int elt;

  if (shadow_thickness > 10)
    shadow_thickness = 10;
  else if (shadow_thickness < 0)
    shadow_thickness = 0;
  if (shadow_thickness > (width / 2))
    shadow_thickness = width / 2;
  if (shadow_thickness > (height / 2))
    shadow_thickness = height / 2;

  for (elt = 0; elt < shadow_thickness; elt++)
    {
      int seg1 = elt;
      int seg2 = (edges & EDGE_TOP) ? elt + shadow_thickness : elt;
      int bot_seg2 = (edges & EDGE_BOTTOM) ? elt + shadow_thickness : elt;

      if (edges & EDGE_TOP)
	{
	  top_shadow[seg1].x1 = x + elt;
	  top_shadow[seg1].x2 = x + width - elt - 1;
	  top_shadow[seg1].y1 = top_shadow[seg1].y2 = y + elt;
	}
      if (edges & EDGE_LEFT)
	{
	  top_shadow[seg2].x1 = top_shadow[seg2].x2 = x + elt;
	  top_shadow[seg2].y1 = y + elt;
	  top_shadow[seg2].y2 = y + height - elt - 1;
	}
      if (edges & EDGE_BOTTOM)
	{
	  bottom_shadow[seg1].x1 = x + elt;
	  bottom_shadow[seg1].x2 = x + width - elt - 1;
	  bottom_shadow[seg1].y1 = bottom_shadow[seg1].y2 = y + height - elt - 1;
	}
      if (edges & EDGE_RIGHT)
	{
	  bottom_shadow[bot_seg2].x1 = bottom_shadow[bot_seg2].x2 = x + width - elt - 1;
	  bottom_shadow[bot_seg2].y1 = y + elt;
	  bottom_shadow[bot_seg2].y2 = y + height - elt - 1;
	}
    }

  XDrawSegments (dpy, x_win, top_shadow_gc, top_shadow,
		 ((edges & EDGE_TOP) ? shadow_thickness : 0)
		 + ((edges & EDGE_LEFT) ? shadow_thickness : 0));
  XDrawSegments (dpy, x_win, bottom_shadow_gc, bottom_shadow,
		 ((edges & EDGE_BOTTOM) ? shadow_thickness : 0)
		 + ((edges & EDGE_RIGHT) ? shadow_thickness : 0));
}

/*****************************************************************************
 x_generate_shadow_pixels

 Given three pixels (top shadow, bottom shadow, background) massage
 the top and bottom shadow colors to guarantee that they differ.  The
 background pixels are not allowed to be modified.

 This function modifies its parameters.

 This code is modified from code blatantly stolen from lwlib/xlwmenu.c
 ****************************************************************************/
#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

void
x_generate_shadow_pixels (struct frame *f, unsigned long *top_shadow,
			  unsigned long *bottom_shadow,
			  unsigned long background,
			  unsigned long core_background)
{
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Colormap cmap = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL (d);

  XColor topc, botc;
  int top_frobbed = 0, bottom_frobbed = 0;

  /* If the top shadow is the same color as the background, try to
     adjust it. */
  if (*top_shadow == background)
    {
      topc.pixel = background;
      XQueryColor (dpy, cmap, &topc);
      /* don't overflow/wrap! */
      topc.red   = MINL (65535, (unsigned long) topc.red   * 6 / 5);
      topc.green = MINL (65535, (unsigned long) topc.green * 6 / 5);
      topc.blue  = MINL (65535, (unsigned long) topc.blue  * 6 / 5);
      if (x_allocate_nearest_color (dpy, cmap, visual, &topc))
	{
	  *top_shadow = topc.pixel;
	  top_frobbed = 1;
	}
    }

  /* If the bottom shadow is the same color as the background, try to
     adjust it. */
  if (*bottom_shadow == background)
    {
      botc.pixel = background;
      XQueryColor (dpy, cmap, &botc);
      botc.red   = (unsigned short) ((unsigned long) botc.red   * 3 / 5);
      botc.green = (unsigned short) ((unsigned long) botc.green * 3 / 5);
      botc.blue  = (unsigned short) ((unsigned long) botc.blue  * 3 / 5);
      if (x_allocate_nearest_color (dpy, cmap, visual, &botc))
	{
	  *bottom_shadow = botc.pixel;
	  bottom_frobbed = 1;
	}
    }

  /* If we had to adjust both shadows, then we have to do some
     additional work. */
  if (top_frobbed && bottom_frobbed)
    {
      int top_avg = ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
      int bot_avg = ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
      if (bot_avg > top_avg)
	{
	  Pixel tmp = *top_shadow;

	  *top_shadow = *bottom_shadow;
	  *bottom_shadow = tmp;
	}
      else if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == background)
	    *top_shadow = core_background;
	  else
	    *bottom_shadow = background;
	}
    }
}

/*****************************************************************************
 XLIKE_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
****************************************************************************/
static void
XLIKE_output_vertical_divider (struct window *w, int USED_IF_X (clear))
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  Lisp_Object tmp_pixel;
  XLIKE_GCVALUES gcv;
  XLIKE_GC background_gc;
  enum edge_style style;
  unsigned long mask;
  int x, ytop, ybottom, width, shadow_thickness, spacing, line_width;
  face_index div_face =
    get_builtin_face_cache_index (w, Vvertical_divider_face);

  width = window_divider_width (w);
  shadow_thickness = XFIXNUM (w->vertical_divider_shadow_thickness);
  spacing = XFIXNUM (w->vertical_divider_spacing);
  line_width = XFIXNUM (w->vertical_divider_line_width);
  x = WINDOW_RIGHT (w) - width;
  ytop = WINDOW_TOP (w);
  ybottom = WINDOW_BOTTOM (w);

  memset (&gcv, ~0, sizeof (gcv));

  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, div_face);

  /* First, get the GC's. */
  XLIKE_SET_GC_COLOR (gcv.background, XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  gcv.foreground = gcv.background;
  gcv.graphics_exposures = XLIKE_FALSE;
  mask = XLIKE_GC_FOREGROUND | XLIKE_GC_BACKGROUND | XLIKE_GC_EXPOSURES;

  background_gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, mask);

  /* Clear the divider area first.  This needs to be done when a
     window split occurs. */
  if (clear)
    XClearArea (dpy, x_win, x, ytop, width, ybottom - ytop, False);

  /* Draw the divider line. */
  XLIKE_FILL_RECTANGLE (dpy, x_win, background_gc,
			x + spacing + shadow_thickness, ytop,
			line_width, ybottom - ytop);
  if (shadow_thickness < 0)
    {
      shadow_thickness = -shadow_thickness;
      style = EDGE_BEVEL_IN;
    }
  else
    {
      style = EDGE_BEVEL_OUT;
    }

  /* Draw the shadows around the divider line */
  XLIKE_bevel_area (w, div_face, x + spacing, ytop,
		    width - 2 * spacing, ybottom - ytop,
		    shadow_thickness, EDGE_ALL, style);
}

/* Make audible bell.  */

static void
XLIKE_ring_bell (struct device *d, int volume, int pitch, int duration)
{
  Display *display = DEVICE_X_DISPLAY (d);

  if (volume < 0) volume = 0;
  else if (volume > 100) volume = 100;
  if (pitch < 0 && duration < 0)
    {
      XBell (display, (volume * 2) - 100);
      XFlush (display);
    }
  else
    {
      XKeyboardState state;
      XKeyboardControl ctl;
      XSync (display, 0);
      /* #### grab server? */
      XGetKeyboardControl (display, &state);

      ctl.bell_pitch    = (pitch    >= 0 ? pitch    : (int) state.bell_pitch);
      ctl.bell_duration = (duration >= 0 ? duration : (int) state.bell_duration);
      XChangeKeyboardControl (display, KBBellPitch|KBBellDuration, &ctl);

      XBell (display, (volume * 2) - 100);

      ctl.bell_pitch    = state.bell_pitch;
      ctl.bell_duration = state.bell_duration;
      XChangeKeyboardControl (display, KBBellPitch|KBBellDuration, &ctl);

      /* #### ungrab server? */
      XSync (display, 0);
    }
}

/* The end-of-line cursor is narrower than the normal cursor. */
static void
XLIKE_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
			 face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object window;

  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;
  face_index elt = get_builtin_face_cache_index (w, Vtext_cursor_face);
  struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL (w, elt);

  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));

  int x = xpos;
  int y = XLIKE_DISPLAY_LINE_YPOS (dl);
  int width = EOL_CURSOR_WIDTH;
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int cursor_height, cursor_y;
  int defheight, defascent;

  window = wrap_window (w);
  redisplay_clear_region (window, findex, x, y, width, height);

  if (NILP (w->text_cursor_visible_p))
    return;

  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
		     Qnil, Qnil, Qnil);

  default_face_font_info (window, &defascent, 0, 0, &defheight, 0);

  /* make sure the cursor is entirely contained between y and y+height */
  cursor_height = min (defheight, height);
  cursor_y = max (y, min (y + height - cursor_height,
			  dl->ypos - defascent));

  if (focus)
    {
#ifdef HAVE_XIM
      XIM_SetSpotLocation (f, x - 2 , cursor_y + cursor_height - 2);
#endif /* HAVE_XIM */

      if (NILP (bar_cursor_value))
	{
	  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, cursor_y, width,
				cursor_height);
	}
      else
	{
	  int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			     Qnil, Qnil,
			     make_fixnum (bar_width));
	  XLIKE_DRAW_LINE (dpy, x_win, gc, x + bar_width - 1, cursor_y,
			   x + bar_width - 1, cursor_y + cursor_height - 1);
	}
    }
  else if (NILP (bar_cursor_value))
    {
      XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, x, cursor_y, width - 1,
			    cursor_height - 1);
    }
}
