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

/*
  XLIKE_text_width

  Given a string and a merged face, return the string's length in pixels
  when displayed in the fonts associated with the face.
*/

static int
XLIKE_text_width (struct frame *f, struct face_cachel *cachel,
		  const Ichar *str, Charcount len)
{
  /* !!#### Needs review */
  int width_so_far = 0;
  unsigned char *text_storage = (unsigned char *) ALLOCA (2 * len);
  struct textual_run *runs = alloca_array (struct textual_run, len);
  int nruns;
  int i;

  nruns = separate_textual_runs (text_storage, runs, str, len,
				 cachel);

  for (i = 0; i < nruns; i++)
    width_so_far += XLIKE_text_width_single_run (f, cachel, runs + i);

  return width_so_far;
}

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

/****************************************************************************
 XLIKE_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
****************************************************************************/
static void
XLIKE_clear_region (Lisp_Object UNUSED (locale), struct frame* f,
		    face_index UNUSED (findex),
		    int x, int y, int width, int height,
		    Lisp_Object fcolor, Lisp_Object bcolor,
		    Lisp_Object background_pixmap,
		    Lisp_Object background_placement)
{
  XLIKE_DISPLAY dpy =  GET_XLIKE_DISPLAY (XDEVICE (f->device));
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;

  if (!UNBOUNDP (background_pixmap))
    {
      gc = XLIKE_get_gc (f, Qnil, fcolor, bcolor,
			 background_pixmap, background_placement, Qnil);
      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);
    }
  else
    {
      XLIKE_CLEAR_AREA (dpy, x_win, x, y, width, height);
    }
}

static void
XLIKE_clear_frame (struct frame *f)
{
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (XDEVICE (f->device));
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  int x, y, width, height;
  Lisp_Object frame;

  /* #### GEOM! This clears the internal border and gutter (and scrollbars)
     but not the toolbar.  Correct? */
  x = FRAME_LEFT_INTERNAL_BORDER_START (f);
  width = (FRAME_RIGHT_INTERNAL_BORDER_END (f) - x);
  /* #### This adjustment by 1 should be being done in the macros.
     There is some small differences between when the menubar is on
     and off that we still need to deal with.  The adjustment also occurs in
     redisplay_clear_top_of_window(). */
  y = FRAME_TOP_INTERNAL_BORDER_START (f) - 1;
  height = (FRAME_BOTTOM_INTERNAL_BORDER_END (f) - y);

  XLIKE_CLEAR_AREA (dpy, x_win, x, y, width, height);

  frame = wrap_frame (f);

  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      XLIKE_clear_frame_windows (f->root_window);
    }
  {
    struct device *d = XDEVICE (f->device);
    if (!(check_if_pending_expose_event (d)))
      XFlush (DEVICE_X_DISPLAY (d));
  }
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
 XLIKE_output_horizontal_line

 Output a horizontal line in the foreground of its face.
****************************************************************************/
static void
XLIKE_output_horizontal_line (struct window *w, struct display_line *dl,
			      struct rune *rb)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;
  int x = rb->xpos;
  int width = rb->width;
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int ypos1, ypos2, ypos3, ypos4;

  ypos1 = XLIKE_DISPLAY_LINE_YPOS (dl);
  ypos2 = ypos1 + rb->object.hline.yoffset;
  ypos3 = ypos2 + dl->ascent / 2;
  ypos4 = dl->ypos + dl->descent - dl->clip;

  /* First clear the area not covered by the line. Clear rectangles
     above and below the line. */
  if (height - rb->object.hline.thickness > 0)
    {
      gc = XLIKE_get_gc (f, Qnil,
			 WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
			 Qnil, Qnil, Qnil, Qnil);

      if (ypos2 - ypos1 > 0)
	XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos1, width, ypos2 - ypos1);
      if (ypos4 - ypos3 > 0)
	XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos3, width, ypos4 - ypos3);
    }

  /* Now draw the line. */
  gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		     Qnil, Qnil, Qnil, Qnil);

  if (ypos2 < ypos1)
    ypos2 = ypos1;
  if (ypos3 > ypos4)
    ypos3 = ypos4;

  if (ypos3 - ypos2 > 0)
    {
      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos3, width,
			    rb->object.hline.thickness);
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

/* briefly swap the foreground and background colors.
 */
static int
XLIKE_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;
  XLIKE_GCVALUES gcv;
  XLIKE_PIXEL tmp_fcolor, tmp_bcolor;
  Lisp_Object tmp_pixel, frame;
  struct window *w = XWINDOW (FRAME_ROOT_WINDOW (f));
  int flash_height;

  frame = wrap_frame (f);

  tmp_pixel = FACE_FOREGROUND (Vdefault_face, frame);
  tmp_fcolor = XLIKE_COLOR_TO_PIXEL (XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  tmp_pixel = FACE_BACKGROUND (Vdefault_face, frame);
  tmp_bcolor = XLIKE_COLOR_TO_PIXEL (XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  memset (&gcv, ~0, sizeof (gcv)); /* initialize all slots to ~0 */
  XLIKE_SET_GC_PIXEL (gcv.foreground, tmp_fcolor ^ tmp_bcolor);
  gcv.function = XLIKE_GX_XOR;
  gcv.graphics_exposures = XLIKE_FALSE;
  gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (XDEVICE (f->device)), &gcv,
			XLIKE_GC_FOREGROUND | XLIKE_GC_FUNCTION | XLIKE_GC_EXPOSURES);
  default_face_width_and_height (frame, 0, &flash_height);

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			    w->pixel_width, flash_height);
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left,
			    w->pixel_top + w->pixel_height - flash_height,
			    w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			  w->pixel_width, w->pixel_height);

  XLIKE_FLUSH (dpy);

#ifdef HAVE_SELECT
  {
    int usecs = 100000;
    struct timeval tv;
    tv.tv_sec  = usecs / 1000000L;
    tv.tv_usec = usecs % 1000000L;
    /* I'm sure someone is going to complain about this... */
    select (0, 0, 0, 0, &tv);
  }
#else
#ifdef HAVE_POLL
  poll (0, 0, 100);
#else /* !HAVE_POLL */
#error bite me
#endif /* HAVE_POLL */
#endif /* HAVE_SELECT */

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			    w->pixel_width, flash_height);
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left,
			    w->pixel_top + w->pixel_height - flash_height,
			    w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			  w->pixel_width, w->pixel_height);

  XLIKE_FLUSH (dpy);

  return 1;
}

/*****************************************************************************
 XLIKE_output_blank

 Output a blank by clearing the area it covers in the foreground color
 of its face.
****************************************************************************/
static void
XLIKE_output_blank (struct window *w, struct display_line *dl, struct rune *rb,
		    int start_pixpos, int cursor_start, int cursor_width)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;
  struct face_cachel *cursor_cachel =
    WINDOW_FACE_CACHEL (w,
			get_builtin_face_cache_index
			(w, Vtext_cursor_face));
  Lisp_Object bg_pmap;
  Lisp_Object buffer = WINDOW_BUFFER (w);
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 buffer);

  int x = rb->xpos;
  int y = XLIKE_DISPLAY_LINE_YPOS (dl);
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int width = rb->width;

  /* Unmap all subwindows in the area we are going to blank. */
  redisplay_unmap_subwindows_maybe (f, x, y, width, height);

  if (start_pixpos > x)
    {
      if (start_pixpos >= (x + width))
	return;
      else
	{
	  width -= (start_pixpos - x);
	  x = start_pixpos;
	}
    }

  bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, rb->findex);
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (NILP (bg_pmap))
    gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
                       Qnil, Qnil, Qnil);
  else
    gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       bg_pmap,
		       WINDOW_FACE_CACHEL_BACKGROUND_PLACEMENT (w, rb->findex),
		       Qnil);

  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);

  /* If this rune is marked as having the cursor, then it is actually
     representing a tab. */
  if (!NILP (w->text_cursor_visible_p)
      && (rb->cursor_type == CURSOR_ON
	  || (cursor_width
	      && (cursor_start + cursor_width > x)
	      && cursor_start < (x + width))))
    {
      int cursor_height, cursor_y;
      int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
      Lisp_Font_Instance *fi;

      fi = XFONT_INSTANCE (FACE_CACHEL_FONT
			   (WINDOW_FACE_CACHEL (w, rb->findex),
			    Vcharset_ascii));

      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			 Qnil, Qnil, Qnil);

      cursor_y = dl->ypos - fi->ascent;
      cursor_height = fi->height;
      if (cursor_y + cursor_height > y + height)
	cursor_height = y + height - cursor_y;

      if (focus)
	{
	  if (NILP (bar_cursor_value))
	    {
	      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, cursor_start, cursor_y,
				    fi->width, cursor_height);
	    }
	  else
	    {
	      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background,
				 Qnil, Qnil, Qnil,
				 make_fixnum (bar_width));
	      XLIKE_DRAW_LINE (dpy, x_win, gc, cursor_start + bar_width - 1,
			       cursor_y, cursor_start + bar_width - 1,
			       cursor_y + cursor_height - 1);
	    }
	}
      else if (NILP (bar_cursor_value))
        {
          XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, cursor_start, cursor_y,
                                fi->width - 1, cursor_height - 1);
        }
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

static void
XLIKE_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x,
			   int y, int xoffset, int yoffset,
			   int width, int height,
			   XLIKE_COLOR fg, XLIKE_COLOR bg)
{
  struct device *d = XDEVICE (f->device);
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;
  XLIKE_GCVALUES gcv;
  unsigned long pixmap_mask;

  memset (&gcv, ~0, sizeof (gcv));
  gcv.graphics_exposures = XLIKE_FALSE;
  XLIKE_SET_GC_COLOR (gcv.foreground, fg);
  XLIKE_SET_GC_COLOR (gcv.background, bg);
  pixmap_mask = XLIKE_GC_FOREGROUND | XLIKE_GC_BACKGROUND | XLIKE_GC_EXPOSURES;

  if (IMAGE_INSTANCE_XLIKE_MASK (p))
    {
      gcv.function = XLIKE_GX_COPY;
      gcv.clip_mask = IMAGE_INSTANCE_XLIKE_MASK (p);
      gcv.clip_x_origin = x - xoffset;
      gcv.clip_y_origin = y - yoffset;
      pixmap_mask |= (XLIKE_GC_FUNCTION | XLIKE_GC_CLIP_MASK |
		      XLIKE_GC_CLIP_X_ORIGIN |
		      XLIKE_GC_CLIP_Y_ORIGIN);
      /* Can't set a clip rectangle below because we already have a mask.
	 We could conceivably create a new clipmask by zeroing out
	 everything outside the clip region.  Is it worth it?
	 Is it possible to get an equivalent effect by changing the
	 args to XCopyArea below rather than messing with a clip box?
	 - dkindred@cs.cmu.edu
	 Yes. We don't clip at all now - andy@xemacs.org
      */
    }

  gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, pixmap_mask);

  /* depth of 0 means it's a bitmap, not a pixmap, and we should use
     XCopyPlane (1 = current foreground color, 0 = background) instead
     of XCopyArea, which means that the bits in the pixmap are actual
     pixel values, instead of symbolic of fg/bg. */

  if (IMAGE_INSTANCE_PIXMAP_DEPTH (p) > 0)
    {
      XCopyArea (dpy,
		 IMAGE_INSTANCE_X_PIXMAP_SLICE
		 (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc, xoffset,
		 yoffset, width,
		 height, x, y);
    }
  else
    {
      XCopyPlane (dpy, IMAGE_INSTANCE_X_PIXMAP_SLICE
		  (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc,
		  xoffset, yoffset, width, height, x, y, 1L);
    }
}

