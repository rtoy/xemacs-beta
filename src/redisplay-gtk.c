/* GTK output and frame manipulation routines.
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

#include "redisplay-xlike-inc.c"

/*****************************************************************************
 gtk_bevel_modeline

 Draw a 3d border around the modeline on window W.
 ****************************************************************************/
static void
gtk_bevel_modeline (struct window *w, struct display_line *dl)
{
  struct frame *f = XFRAME (w->frame);
  int shadow_thickness = MODELINE_SHADOW_THICKNESS (w);
  int x,y, width, height;

  x = WINDOW_MODELINE_LEFT (w);
  width = WINDOW_MODELINE_RIGHT (w) - x;
  y = dl->ypos - dl->ascent - shadow_thickness;
  height = dl->ascent + dl->descent + 2 * shadow_thickness;

  gtk_output_shadows (f, x, y, width, height, shadow_thickness);
}

/*****************************************************************************
 gtk_output_shadows

 Draw a shadow around the given area using the standard theme engine routines.
 ****************************************************************************/
void
gtk_output_shadows (struct frame *f, int x, int y, int width, int height,
		    int shadow_thickness)
{
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GtkStyle *style = FRAME_GTK_TEXT_WIDGET (f)->style;
  GtkShadowType stype = GTK_SHADOW_OUT;

  if (shadow_thickness < 0)
  {
      stype = GTK_SHADOW_IN;
  }
  else if (shadow_thickness == 0)
  {
      stype = GTK_SHADOW_NONE;
  }

  /* Do we want to have some magic constants to set
     GTK_SHADOW_ETCHED_IN or GTK_SHADOW_ETCHED_OUT? */

  gtk_paint_shadow (style, x_win, GTK_STATE_NORMAL, stype, NULL,
		    FRAME_GTK_TEXT_WIDGET (f), "modeline",
		    x, y, width, height);
}

static void
gtk_bevel_area (struct window *w, face_index UNUSED (findex),
		int x, int y, int width, int height,
		int shadow_thickness, int UNUSED (edges),
		enum edge_style UNUSED (style))
{
  struct frame *f = XFRAME (w->frame);

  gtk_output_shadows (f, x, y, width, height, shadow_thickness);
}



/* Make audible bell.  */
static void
gtk_ring_bell (struct device *UNUSED (d), int volume, int UNUSED (pitch),
	       int UNUSED (duration))
{
  /* Gdk does not allow us to control the duration / pitch / volume */
  if (volume > 0)
    {
      gdk_beep ();
    }
}


/* This makes me feel incredibly dirty... but there is no other way to
   get this done right other than calling clear_area before every
   single $#!%@ing piece of text, which I do NOT want to do. */
#define USE_X_SPECIFIC_DRAW_ROUTINES 1

#include <gdk/gdkx.h>

static
void gdk_draw_text_image (GdkDrawable *drawable,
			  GdkFont     *font,
			  GdkGC       *gc,
			  gint         x,
			  gint         y,
			  const gchar *text,
			  gint         text_length)
{
#if !USE_X_SPECIFIC_DRAW_ROUTINES
  int width = gdk_text_measure (font, text, text_length);
  int height = gdk_text_height (font, text, text_length);

  gdk_draw_rectangle (drawable, gc, TRUE, x, y, width, height);
  gdk_draw_text (drawable, font, gc, x, y, text, text_length);
#else
  GdkWindowPrivate *drawable_private;
  GdkFontPrivate *font_private;
  GdkGCPrivate *gc_private;

  g_return_if_fail (drawable != NULL);
  g_return_if_fail (font != NULL);
  g_return_if_fail (gc != NULL);
  g_return_if_fail (text != NULL);

  drawable_private = (GdkWindowPrivate*) drawable;
  if (drawable_private->destroyed)
    return;
  gc_private = (GdkGCPrivate*) gc;
  font_private = (GdkFontPrivate*) font;

  if (font->type == GDK_FONT_FONT)
    {
      XFontStruct *xfont = (XFontStruct *) font_private->xfont;
      XSetFont(drawable_private->xdisplay, gc_private->xgc, xfont->fid);
      if ((xfont->min_byte1 == 0) && (xfont->max_byte1 == 0))
	{
	  XDrawImageString (drawable_private->xdisplay, drawable_private->xwindow,
			    gc_private->xgc, x, y, text, text_length);
	}
      else
	{
	  XDrawImageString16 (drawable_private->xdisplay, drawable_private->xwindow,
			      gc_private->xgc, x, y, (XChar2b *) text, text_length / 2);
	}
    }
  else if (font->type == GDK_FONT_FONTSET)
    {
      XFontSet fontset = (XFontSet) font_private->xfont;
      XmbDrawImageString (drawable_private->xdisplay, drawable_private->xwindow,
			  fontset, gc_private->xgc, x, y, text, text_length);
    }
  else
    g_error("undefined font type\n");
#endif
}

static void
our_draw_bitmap (GdkDrawable *drawable,
		 GdkGC       *gc,
		 GdkPixmap   *src,
		 gint         xsrc,
		 gint         ysrc,
		 gint         xdest,
		 gint         ydest,
		 gint         width,
		 gint         height)
{
  GdkWindowPrivate *drawable_private;
  GdkWindowPrivate *src_private;
  GdkGCPrivate *gc_private;

  g_return_if_fail (drawable != NULL);
  g_return_if_fail (src != NULL);
  g_return_if_fail (gc != NULL);

  drawable_private = (GdkWindowPrivate*) drawable;
  src_private = (GdkWindowPrivate*) src;
  if (drawable_private->destroyed || src_private->destroyed)
    return;
  gc_private = (GdkGCPrivate*) gc;

  if (width == -1)
    width = src_private->width;
  if (height == -1)
    height = src_private->height;

  XCopyPlane (drawable_private->xdisplay,
	     src_private->xwindow,
	     drawable_private->xwindow,
	     gc_private->xgc,
	     xsrc, ysrc,
	     width, height,
	     xdest, ydest, 1L);
}
