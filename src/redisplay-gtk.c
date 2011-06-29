/* GTK output and frame manipulation routines.
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

#define THIS_IS_GTK
#include "redisplay-xlike-inc.c"

void gtk_fill_rectangle (cairo_t *cr, gint x, gint y, gint width, gint height);

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
      case EDGE_BEVEL_IN: stype = GTK_SHADOW_IN; break;
      case EDGE_BEVEL_OUT: stype = GTK_SHADOW_OUT; break;
      case EDGE_ETCHED_IN: stype = GTK_SHADOW_ETCHED_IN; break;
      case EDGE_ETCHED_OUT: stype = GTK_SHADOW_ETCHED_OUT; break;
      default: ABORT (); stype = GTK_SHADOW_OUT;
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


#include "sysgdkx.h"

void gtk_fill_rectangle (cairo_t *cr, gint x, gint y,
                         gint width, gint height)
{
  GdkRectangle area;
  area.x = x; area.y = y;
  area.width = width;
  area.height = height;

  /* Set new clip region and fill it. */
  cairo_save (cr);
  cairo_new_path (cr);
  gdk_cairo_rectangle (cr, &area);
  cairo_fill (cr);
  cairo_new_path (cr);
  gdk_cairo_rectangle (cr, &area);
  cairo_restore (cr);
}

/*
 * Only this function can erase the text area because the text area
 * is calculated by pango.
 */
static void
gdk_draw_text_image (GtkWidget *widget, struct face_cachel *cachel, GdkGC *gc,
		     gint x, gint y, struct textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  gint width = 0;
  gint height = 0;
  
  GdkDrawable *drawable = gtk_widget_get_window (widget);
  cairo_t *cr = gdk_cairo_create (drawable);
  PangoContext *context = gtk_widget_get_pango_context (widget);
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
  PangoFontMetrics *pfm = FONT_INSTANCE_GTK_FONT_METRICS (fi);
  PangoAttrList *attr_list = pango_attr_list_new ();
  gint ascent;

  GdkColor *fg = XCOLOR_INSTANCE_GTK_COLOR (cachel->foreground);
  GdkColor *bg = XCOLOR_INSTANCE_GTK_COLOR (cachel->background);

  pango_attr_list_insert (attr_list,
                          pango_attr_foreground_new (fg->red,
                                                     fg->green,
                                                     fg->blue));
  if (bg)
    pango_attr_list_insert (attr_list,
                            pango_attr_background_new (bg->red,
                                                       bg->green,
                                                       bg->blue));
  if (cachel->strikethru)
    pango_attr_list_insert (attr_list,
                            pango_attr_strikethrough_new (TRUE));
  if (cachel->underline)
    pango_attr_list_insert (attr_list,
                            pango_attr_underline_new (PANGO_UNDERLINE_SINGLE));
  
  pango_layout_set_attributes (layout, attr_list);
  pango_layout_set_font_description (layout, pfd);
  
  assert (run->dimension == 1);  /* UTF-8 only. */
  pango_layout_set_text (layout, (const char *)run->ptr, run->len);
  pango_layout_get_pixel_size (layout, &width, &height);
  ascent = pango_font_metrics_get_ascent (pfm) / PANGO_SCALE;

  /* gdk_cairo_set_source_color (cr, bg); */
  /* gtk_fill_rectangle (cr, x, y - ascent, width, height); */

  /* xft draw */
  /* pango_xft_layout_render (xft_draw, xft_color, layout, x, y); */

  /* Gtk draw */
  gdk_draw_layout (drawable, gc, x, y - ascent, layout);
  cairo_destroy (cr);
  g_object_unref (layout);
  pango_attr_list_unref (attr_list);
}

#ifdef NOTUSED
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

  gdk_draw_drawable(drawable, gc, src, xsrc, ysrc, xdest, ydest,
                    width, height);
}

#endif

/* We always have to layout text to include combining marks etc. */
static int
XLIKE_text_width_single_run (struct frame *f,
			     struct face_cachel *cachel,
			     struct textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  gint width, height;
  PangoContext *context = gtk_widget_get_pango_context (widget);
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
  Lisp_Object font = FACE_CACHEL_FONT (cachel, run->charset);
  PangoAttrList *attr_list = pango_attr_list_new ();

  if (cachel->strikethru)
    pango_attr_list_insert (attr_list,
                            pango_attr_strikethrough_new (TRUE));
  if (cachel->underline)
    pango_attr_list_insert (attr_list,
                            pango_attr_underline_new (PANGO_UNDERLINE_SINGLE));
  
  pango_layout_set_attributes (layout, attr_list);
  pango_layout_set_font_description (layout, pfd);
  pango_layout_set_text (layout, (const char *)run->ptr, run->len);
  pango_layout_get_pixel_size (layout, &width, &height);
  g_object_unref (layout);
  pango_attr_list_unref (attr_list);
  return width;
}
