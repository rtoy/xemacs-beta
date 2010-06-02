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

#define THIS_IS_GTK
#include "redisplay-xlike-inc.c"

/*****************************************************************************
 Draw a shadow around the given area using the standard theme engine routines.
 ****************************************************************************/

static void
XLIKE_bevel_area (struct window *w, face_index UNUSED (findex),
		  int x, int y, int width, int height,
		  int shadow_thickness, int UNUSED (edges),
		  enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GtkStyle *gstyle = FRAME_GTK_TEXT_WIDGET (f)->style;
  GtkShadowType stype;

  if (shadow_thickness == 0)
    stype = GTK_SHADOW_NONE;
  else
    switch (style)
      {
      case EDGE_BEVEL_IN: style = GTK_SHADOW_IN; break;
      case EDGE_BEVEL_OUT: style = GTK_SHADOW_OUT; break;
      case EDGE_ETCHED_IN: style = GTK_SHADOW_ETCHED_IN; break;
      case EDGE_ETCHED_OUT: style = GTK_SHADOW_ETCHED_OUT; break;
      default: ABORT (); style = GTK_SHADOW_OUT;
      }

  /* Do we want to have some magic constants to set
     GTK_SHADOW_ETCHED_IN or GTK_SHADOW_ETCHED_OUT? */

  gtk_paint_shadow (gstyle, x_win, GTK_STATE_NORMAL, stype, NULL,
		    FRAME_GTK_TEXT_WIDGET (f), "modeline",
		    x, y, width, height);
}



/* Make audible bell.  */
static void
XLIKE_ring_bell (struct device *UNUSED (d), int volume, int UNUSED (pitch),
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

#include "sysgdkx.h"

/*
 * Only this function can erase the text area because the text area
 * is calculated by pango.   The layout is currently not shared.  There
 * can only be one PangoLayout per line, I think. --jsparkes
 */

static void
gdk_draw_text_image (GdkDrawable *drawable, GdkFont *font, GdkGC *gc,
		     GdkGC *bgc, gint x, gint y, gchar *text, gint len)
{
  int width = -1;
  int height = -1;
#ifdef USE_PANGO
  PangoLayout *layout;
#endif

#ifdef USE_PANGO
  Display *disp = GDK_DRAWABLE_XDISPLAY (drawable);
  int screen = GDK_SCREEN_XNUMBER (gdk_drawable_get_screen (drawable));

  /* Xft render */
  //context = pango_xft_get_context (display, screen);
  //layout = pango_layout_new (context);
  /* Gtk render */
  context = gtk_widget_get_pango_context (widget);
  layout = pango_layout_new (context);

  pango_layout_set_text (layout, text, text_length);
  pango_layout_get_pixel_size (layout, &width, &height);

  if (bgc != 0)
    gdk_draw_rectangle (drawable, bgc, TRUE, x, y, width, height);

  /* xft draw */
  //pango_xft_layout_render (xft_draw, xft_color, layout, x, y);
  /* Gtk draw */
  gdk_draw_layout (drawable, gc, x, y, layout);
  //g_object_unref (layout);
#else
  height = font->ascent + font->descent;
  width  = gdk_text_width (font, text, len);
  if (bgc != 0)
    /* The rectangle and text areas don't quite fit, so I have to the
       te height a little --jsparkes */
    gdk_draw_rectangle (drawable, bgc, TRUE, x, y-height+3, width, height);
  gdk_draw_text (drawable, font, gc, x, y, text, len);
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
  gint src_width, src_height;

  g_return_if_fail (drawable != NULL);
  g_return_if_fail (src != NULL);
  g_return_if_fail (gc != NULL);

  gdk_drawable_get_size (src, &src_width, &src_height);


  if (width == -1)
    width = src_width;
  if (height == -1)
    height = src_height;

  gdk_draw_drawable(drawable, gc, src,  xsrc, ysrc, xdest, ydest,
                    width, height);
}
