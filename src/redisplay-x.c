/* X output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003 Ben Wing.

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
