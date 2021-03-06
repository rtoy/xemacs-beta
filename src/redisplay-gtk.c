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

static void cr_set_foreground (cairo_t *cr, Lisp_Object color);
static void gtk_draw_rectangle (cairo_t *cr, gint x, gint y,
                                gint width, gint height);
static void gtk_fill_rectangle (cairo_t *cr, gint x, gint y,
                                gint width, gint height);

/****************************************************************************
 XLIKE_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
****************************************************************************/
static void
XLIKE_clear_region (Lisp_Object UNUSED (locale), struct frame* f,
		    face_index UNUSED (findex),
		    int x, int y, int width, int height,
		    Lisp_Object UNUSED (fcolor), Lisp_Object UNUSED (bcolor),
		    Lisp_Object background_pixmap,
		    Lisp_Object UNUSED (background_placement))
{
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif

  if (!NILP (background_pixmap) && !UNBOUNDP (background_pixmap))
    {
      /* XXX Implement me! */
      /*
      gc = XLIKE_get_gc (f, Qnil, fcolor, bcolor,
      background_pixmap, background_placement, Qnil);
      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);
      */
#ifdef HAVE_GTK2
      gdk_window_clear_area(gtk_widget_get_window (widget),
			    x, y, width, height);
#endif
#ifdef HAVE_GTK3
      gtk_render_background(gtk_widget_get_style_context (widget),
			    cr, x, y, width, height);
#endif
    }
  else
    {
#ifdef HAVE_GTK2
      gdk_window_clear_area(gtk_widget_get_window (widget),
			    x, y, width, height);
#endif
#ifdef HAVE_GTK3
      gtk_render_background(gtk_widget_get_style_context (widget),
			    cr, x, y, width, height);
#endif
    }
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
}

static void
XLIKE_clear_frame (struct frame *f)
{
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif
  Lisp_Object frame;
#ifdef HAVE_GTK2
  GdkRectangle r;
  GdkWindow *w = gtk_widget_get_window (widget);
  GtkStyle *style = gtk_widget_get_style (widget);
  GdkColor *bg;
#endif
#ifdef HAVE_GTK3
  cairo_rectangle_t r;
  GtkStyleContext *sc = gtk_widget_get_style_context (widget);
#endif

  /* #### GEOM! This clears the internal border and gutter (and scrollbars)
     but not the toolbar.  Correct? */
  r.x = FRAME_LEFT_INTERNAL_BORDER_START (f);
  r.width = (FRAME_RIGHT_INTERNAL_BORDER_END (f) - r.x);
  /* #### This adjustment by 1 should be being done in the macros.
     There is some small differences between when the menubar is on
     and off that we still need to deal with.  The adjustment also occurs in
     redisplay_clear_top_of_window(). */
  r.y = FRAME_TOP_INTERNAL_BORDER_START (f) - 1;
  r.height = (FRAME_BOTTOM_INTERNAL_BORDER_END (f) - r.y);

#ifdef HAVE_GTK2
  /*
  bg = &style->bg[GTK_STATE_NORMAL];
  cairo_set_source_rgb (cr, bg->red, bg->green, bg->blue);
  gtk_style_apply_default_background (style, w, TRUE, GTK_STATE_NORMAL, NULL,
    r.x, r.y, r.width, r.height);
  */
  /* gdk_window_clear (w); */
  gdk_window_clear_area (w, r.x, r.y, r.width, r.height);
#endif

#ifdef HAVE_GTK3
  gtk_render_background (sc, cr, r.x, r.y, r.width, r.height);
#endif

#if 1
  frame = wrap_frame (f);
  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      XLIKE_clear_frame_windows (f->root_window);
    }
#endif
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
}

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
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
#ifdef HAVE_GTK2
  GtkStyle *gstyle = gtk_widget_get_style (widget);
  GdkWindow *x_win = gtk_widget_get_window (FRAME_GTK_TEXT_WIDGET (f));
#endif
#ifdef HAVE_GTK3
  GtkStyleContext *gstyle = gtk_widget_get_style_context (widget);
  GdkWindow *window = gtk_widget_get_window (widget);
#endif
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
#endif
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
#ifdef HAVE_GTK2
  gtk_paint_shadow (gstyle, x_win, GTK_STATE_NORMAL, stype, NULL,
		    FRAME_GTK_TEXT_WIDGET (f), "modeline",
		    x, y, width, height);
#endif
#ifdef HAVE_GTK3
  /* Should the edges be 0.0 and 1.0? Need to get shadows enabled to test. */
  gtk_render_frame (gstyle, cr, x*1.0, y*1.0, width*1.0, height*1.0);
#endif
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif

}

/*****************************************************************************
 XLIKE_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
****************************************************************************/
static void
XLIKE_output_vertical_divider (struct window *w, int UNUSED (clear))
{
  struct frame *f = XFRAME (w->frame);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
#ifdef HAVE_GTK2
  GtkStyle *gstyle = gtk_widget_get_style (widget);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  enum edge_style style;
  int shadow_thickness;
#endif
#ifdef HAVE_GTK3
  GtkStyleContext *gstyle = gtk_widget_get_style_context (widget);
#endif
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
#endif
  int x, ytop, ybottom, width, spacing;
  face_index div_face =
    get_builtin_face_cache_index (w, Vvertical_divider_face);

  width = window_divider_width (w);
  spacing = XFIXNUM (w->vertical_divider_spacing);
  /* line_width = XFIXNUM (w->vertical_divider_line_width); */
  x = WINDOW_RIGHT (w) - width;
  ytop = WINDOW_TOP (w);
  ybottom = WINDOW_BOTTOM (w);

  cr_set_foreground (cr, WINDOW_FACE_CACHEL_BACKGROUND (w, div_face));
  gtk_fill_rectangle (cr, x, ytop, width, ybottom - ytop);

#ifdef HAVE_GTK2
  shadow_thickness = XFIXNUM (w->vertical_divider_shadow_thickness);

  if (shadow_thickness < 0)
    {
      shadow_thickness = -shadow_thickness;
      style = EDGE_BEVEL_IN;
    }
  else
    {
      style = EDGE_BEVEL_OUT;
    }
#endif

  cr_set_foreground (cr, WINDOW_FACE_CACHEL_FOREGROUND (w, div_face));
#ifdef HAVE_GTK2
  /* Draw the shadows around the divider line */
  gtk_bevel_area (w, div_face, x + spacing, ytop,
		  width - 2 * spacing, ybottom - ytop,
		  shadow_thickness, EDGE_ALL, style);
  gtk_paint_vline (gstyle, x_win, GTK_STATE_NORMAL, NULL,
		   FRAME_GTK_TEXT_WIDGET (f), "vline", ytop + spacing,
		   ybottom + spacing , x + width / 2);
#endif
#ifdef HAVE_GTK3
  gtk_render_line (gstyle, cr, x + (width/2), ytop + spacing,
		   x + (width/2), ybottom + spacing);
#endif
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
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
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif

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
      cr_set_foreground (cr, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex));
      gtk_fill_rectangle (cr, x, ypos1, width, ypos4 - ypos1);
      /*
      if (ypos2 - ypos1 > 0)
	gtk_draw_rectangle (cr, x, ypos1, width, ypos2 - ypos1);
      if (ypos4 - ypos3 > 0)
      gtk_draw_rectangle (cr, x, ypos3, width, ypos4 - ypos3);
      */
    }

  cr_set_foreground (cr, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex));

  if (ypos2 < ypos1)
    ypos2 = ypos1;
  if (ypos3 > ypos4)
    ypos3 = ypos4;

  if (ypos3 - ypos2 > 0)
    {
#ifdef HAVE_GTK2
      GtkStyle *style = gtk_widget_get_style (FRAME_GTK_TEXT_WIDGET (f));
      XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
#endif
#ifdef HAVE_GTK3
      GtkStyleContext *gstyle = gtk_widget_get_style_context (widget);
#endif

#ifdef HAVE_GTK2
      gtk_paint_hline (style, x_win, GTK_STATE_NORMAL, NULL,
		       FRAME_GTK_TEXT_WIDGET (f), "hline", x, x + width,
		       ypos3 + rb->object.hline.thickness / 2);
      /* gtk_fill_rectangle (cr, x, ypos3, width, rb->object.hline.thickness); */
#endif
#ifdef HAVE_GTK3
      gtk_render_line (gstyle, cr, x, ypos2 - (rb->object.hline.thickness / 2),
		       x + width, ypos3 + (rb->object.hline.thickness / 2));
#endif
    }
 #if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
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

static int
XLIKE_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif
  struct window *w = XWINDOW (FRAME_ROOT_WINDOW (f));
  Lisp_Object frame = wrap_frame (f);
  int flash_height;

  cr_set_foreground (cr, FACE_FOREGROUND (Vdefault_face, frame));
  cairo_set_operator (cr, CAIRO_OPERATOR_XOR);
  default_face_width_and_height (frame, 0, &flash_height);

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      gtk_fill_rectangle (cr, w->pixel_left, w->pixel_top,
			  w->pixel_width, flash_height);
      gtk_fill_rectangle (cr, w->pixel_left,
			  w->pixel_top + w->pixel_height - flash_height,
			  w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    gtk_fill_rectangle (cr, w->pixel_left, w->pixel_top,
			w->pixel_width, w->pixel_height);

  gdk_flush ();
  g_poll (NULL, 0, 100);

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      gtk_fill_rectangle (cr, w->pixel_left, w->pixel_top,
			  w->pixel_width, flash_height);
      gtk_fill_rectangle (cr, w->pixel_left,
			  w->pixel_top + w->pixel_height - flash_height,
			  w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    gtk_fill_rectangle (cr, w->pixel_left, w->pixel_top,
			w->pixel_width, w->pixel_height);

#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
  return 1;
}

static void
XLIKE_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
		       face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *win = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  /* Can't initialise REGION, CTX and CR until we have called
      redisplay_clear_region(). */
  cairo_region_t *region;
  GdkDrawingContext *ctx;
#endif
  cairo_t *cr;
  struct device *d = XDEVICE (f->device);
  Lisp_Object window;

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

#if GTK_CHECK_VERSION(3, 22, 0)
  region = gdk_window_get_visible_region (win);
  /* We should be able to avoid the next call, but the GTK documentation
     doesn't explain how to get the drawing context. */
  ctx = gdk_window_begin_draw_frame (win, region);
  cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cr = gdk_cairo_create (win);
#endif

  default_face_font_info (window, &defascent, 0, 0, &defheight, 0);

  /* make sure the cursor is entirely contained between y and y+height */
  cursor_height = min (defheight, height);
  cursor_y = max (y, min (y + height - cursor_height,
			  dl->ypos - defascent));
  cr_set_foreground (cr, cursor_cachel->background);

  if (focus)
    {
      if (NILP (bar_cursor_value))
	{
	  gtk_fill_rectangle (cr, x, cursor_y, width, cursor_height);
	}
      else
	{
	  int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;
	  gtk_fill_rectangle (cr, x + bar_width - 1, cursor_y,
			 bar_width, cursor_height);
	}
    }
  else if (NILP (bar_cursor_value))
    {
      gtk_draw_rectangle (cr, x, cursor_y, width - 1, cursor_height - 1);
    }

#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (win, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
}

#include "sysgdkx.h"

static void
cr_set_foreground (cairo_t *cr, Lisp_Object color)
{
#ifdef HAVE_GTK2
  GdkColor *fg = XCOLOR_INSTANCE_GTK_COLOR (color);

  cairo_set_source_rgb (cr,
			(double) fg->red/65535,
			(double) fg->green/65535,
			(double) fg->blue/65535);
#endif
#ifdef HAVE_GTK3
  GdkRGBA *fg = XCOLOR_INSTANCE_GTK_COLOR (color);

  cairo_set_source_rgba (cr, fg->red, fg->green, fg->blue, fg->alpha);
#endif
}

static void
gtk_fill_rectangle (cairo_t *cr, gint x, gint y,
                    gint width, gint height)
{
  cairo_new_path (cr);
  cairo_set_line_width (cr, 0.5);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
}

static void
gtk_draw_rectangle (cairo_t *cr, gint x, gint y,
                    gint width, gint height)
{
  cairo_new_path (cr);
  cairo_set_line_width (cr, 0.5);
  cairo_rectangle (cr, x, y, width, height);
  cairo_stroke_preserve (cr);
}

static PangoAttrList *
gtk_text_attributes (struct face_cachel *cachel)
{
  PangoAttrList *attr_list = pango_attr_list_new ();
#ifdef HAVE_GTK2
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

#endif
#ifdef HAVE_GTK3
  GdkRGBA *fg = XCOLOR_INSTANCE_GTK_COLOR (cachel->foreground);
  GdkRGBA *bg = XCOLOR_INSTANCE_GTK_COLOR (cachel->background);

  pango_attr_list_insert (attr_list,
                          pango_attr_foreground_new (fg->red * 65535,
                                                     fg->green * 65535,
                                                     fg->blue * 65535));
  if (bg)
    pango_attr_list_insert (attr_list,
                            pango_attr_background_new (bg->red * 65535,
                                                       bg->green * 65535,
                                                       bg->blue * 65535));
#endif
  if (cachel->strikethru)
    pango_attr_list_insert (attr_list,
                            pango_attr_strikethrough_new (TRUE));
  if (cachel->underline)
    pango_attr_list_insert (attr_list,
                            pango_attr_underline_new (PANGO_UNDERLINE_SINGLE));

  return attr_list;
}

static void
gdk_draw_text_image (GtkWidget *widget, struct face_cachel *cachel,
                     cairo_t *cr, gint x, gint y,
                     Lisp_Object charset, Extbyte *ptr, Bytecount len)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);

  PangoContext *context = gtk_widget_get_pango_context (widget);
  /* PangoLayout *layout = pango_layout_new (context); */
  PangoLayout *layout = pango_cairo_create_layout (cr);
  PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
  PangoFontMetrics *pfm = FONT_INSTANCE_GTK_FONT_METRICS (fi);
  PangoAttrList *attr_list = gtk_text_attributes (cachel);
  GList *items = NULL, *current = NULL;;

  pango_layout_set_attributes (layout, attr_list);
  pango_layout_set_font_description (layout, pfd);
  /* Pango breaks text into directional sections. */
  items = pango_itemize (context, ptr, 0, len, attr_list, 0);

  current = items;
  while (current)
    {
      PangoItem *item = (PangoItem *) current->data;
      gint ascent = 0;
      gint width = 0;
      gint height = 0;

      pango_layout_set_text (layout, ptr + item->offset, item->length);
      pango_layout_get_pixel_size (layout, &width, &height);
      ascent = pango_font_metrics_get_ascent (pfm) / PANGO_SCALE;

      /* gdk_cairo_set_source_color (cr, bg); */
      /* gtk_fill_rectangle (cr, x, y - ascent, width, height); */

      /* xft draw */
      /* pango_xft_layout_render (xft_draw, xft_color, layout, x, y); */
      cairo_new_path (cr);
      cairo_move_to (cr, x, y - ascent);
      pango_cairo_update_layout (cr, layout);
      pango_cairo_show_layout (cr, layout);
      /* Gtk draw */
      /* gdk_draw_layout (drawable, gc, x, y - ascent, layout); */
      pango_item_free (item);
      current = g_list_next (current);
      x += width;
    }
  g_list_free (items);
  g_object_unref (layout);
  pango_attr_list_unref (attr_list);
}

/* gtk_text_width

   Given a string and a merged face, return the string's length in pixels
   when displayed in the fonts associated with the face. */
static int
gtk_text_width (struct frame *f, struct face_cachel *cachel,
                const Ibyte *str, Bytecount len)
{
  Extbyte *alloca_ext_storage;
  Bytecount extbytes = 0;
  gint width = 0;
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);

  TO_EXTERNAL_FORMAT (DATA, (str, len),
                      ALLOCA, (alloca_ext_storage, extbytes),
                      Qutf_8);

  if (!fi->proportional_p)
    {
      width = fi->width * extbytes;
    }
  else
    {
      GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
      gint height;
      PangoContext *context = gtk_widget_get_pango_context (widget);
      PangoLayout *layout = pango_layout_new (context);
      PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
      PangoAttrList *attr_list = pango_attr_list_new ();

      if (cachel->strikethru)
        {
          pango_attr_list_insert (attr_list, pango_attr_strikethrough_new (TRUE));
        }

      if (cachel->underline)
        {
          pango_attr_list_insert (attr_list,
                                  pango_attr_underline_new
                                  (PANGO_UNDERLINE_SINGLE));
        }

      pango_layout_set_attributes (layout, attr_list);
      pango_layout_set_font_description (layout, pfd);
      pango_layout_set_text (layout, (const char *) alloca_ext_storage, extbytes);
      pango_layout_get_pixel_size (layout, &width, &height);
      g_object_unref (layout);
      pango_attr_list_unref (attr_list);
    }
  return width;
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
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif

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
    {
      cr_set_foreground (cr, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex));
    }
  else
    {
      /* XXX Implement me. */
      /*
      gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       bg_pmap,
		       WINDOW_FACE_CACHEL_BACKGROUND_PLACEMENT (w, rb->findex),
		       Qnil);
      */
      if (IMAGE_INSTANCEP (bg_pmap))
	{
	  Lisp_Image_Instance *p = XIMAGE_INSTANCE (bg_pmap);
	  gdk_cairo_set_source_pixbuf(cr, IMAGE_INSTANCE_GTK_PIXMAP (p),
				      0.0, 0.0);
	  cairo_paint (cr);
	}
      cr_set_foreground (cr, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex));
    }

  gtk_fill_rectangle (cr, x, y, width, height);

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

      cr_set_foreground (cr, cursor_cachel->background);
      cursor_y = dl->ypos - fi->ascent;
      cursor_height = fi->height;
      if (cursor_y + cursor_height > y + height)
	cursor_height = y + height - cursor_y;

      if (focus)
	{
	  if (NILP (bar_cursor_value))
	    {
	      gtk_fill_rectangle (cr, cursor_start, cursor_y,
				  fi->width, cursor_height);
	    }
	  else
	    {
	      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	      gtk_fill_rectangle (cr, cursor_start, cursor_y,
				  bar_width, cursor_height);
	      /*
	      XLIKE_DRAW_LINE (dpy, x_win, gc, cursor_start + bar_width - 1,
			       cursor_y, cursor_start + bar_width - 1,
			       cursor_y + cursor_height - 1);
	      */
	    }
	}
      else if (NILP (bar_cursor_value))
        {
	  gtk_draw_rectangle (cr, cursor_start, cursor_y,
			      fi->width - 1, cursor_height - 1);
        }
    }
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif
}

/*****************************************************************************
 gtk_output_string

 Given a string and a starting position, output that string in the
 given face.  If cursor is true, draw a cursor around the string.
 Correctly handles multiple charsets in the string.

 The meaning of the parameters is something like this:

 W		Window that the text is to be displayed in.
 DL		Display line that this text is on.  The values in the
 		structure are used to determine the vertical position and
		clipping range of the text.
 BUF		Pointer to those Ibytes to be output.
 LEN            Number of those Ibytes to be output
 XPOS		X position in pixels where the text should start being drawn.
 XOFFSET	Number of pixels to be chopped off the left side of the
 		text.  The effect is as if the text were shifted to the
		left this many pixels and clipped at XPOS.
 CLIP_START	Clip everything left of this X position.
 WIDTH		Clip everything right of XPOS + WIDTH.
 FINDEX		Index for the face cache element describing how to display
 		the text.
 CURSOR		#### I don't understand this.  There's something
 		strange and overcomplexified with this variable.
		Chuck, explain please?
 CURSOR_START	Starting X position of cursor.
 CURSOR_WIDTH	Width of cursor in pixels.
 CURSOR_HEIGHT	Height of cursor in pixels.

 Starting Y position of cursor is the top of the text line.
 The cursor is drawn sometimes whether or not CURSOR is set. ???
 ****************************************************************************/

void
gtk_output_string (struct window *w, struct display_line *dl,
                   const Ibyte *buf, Bytecount len,
                   int xpos, int xoffset, int clip_start,
                   int width, face_index findex, int cursor,
                   int cursor_start, int cursor_width, int cursor_height)
{
  /* General variables */
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  int clip_end;
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);

  /* Cursor-related variables */
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  int cursor_clip;
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));
  /* Text-related variables */
  Lisp_Object bg_pmap;
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int ypos = XLIKE_DISPLAY_LINE_YPOS (dl);
  Extbyte *text_storage;
  Bytecount extbytes;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  if (cursor == 1)
    {
      display_checking_assert (itext_ichar_len (buf) == len);
    }

  if (width < 0)
    {
      width = gtk_text_width (f, cachel, buf, len);
    }

  /* Regularize the variables passed in. */
  clip_start = min (clip_start, xpos);
  clip_end = xpos + width;
  if (clip_start >= clip_end)
    /* It's all clipped out. */
    return;

  xpos -= xoffset;

  /* make sure the area we are about to display is subwindow free. */
  redisplay_unmap_subwindows_maybe (f, clip_start, ypos,
				    clip_end - clip_start, height);

  cursor_clip = (cursor_start >= clip_start &&
		 cursor_start < clip_end);

#ifdef HAVE_XIM
  if (cursor && focus && (cursor_start == clip_start) && cursor_height)
    XIM_SetSpotLocation (f, xpos - 2, dl->ypos + dl->descent - 2);
#endif /* HAVE_XIM */

  bg_pmap = cachel->background_pixmap;
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if ((cursor && focus && NILP (bar_cursor_value)
       && !NILP (w->text_cursor_visible_p)) || NILP (bg_pmap))
    {
      /* Nothing. */
    }
  else
    {
      /* Clear the cursor location? */
#ifdef NOTUSED
      bgc = XLIKE_get_gc (f, Qnil, cachel->background, cachel->background,
                          bg_pmap, cachel->background_placement, Qnil);
      XLIKE_FILL_RECTANGLE (dpy, x_win, bgc, clip_start,
                            ypos, clip_end - clip_start,
                            height);
#endif
    }

  TO_EXTERNAL_FORMAT (DATA, (buf, len),
                      ALLOCA, (text_storage, extbytes), Qutf_8);
  do
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
      int need_clipping;
      GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
      cairo_region_t *region = gdk_window_get_visible_region (window);
      /* We should be able to avoid the next call, but the GTK documentation
       * doesn't explain how to get the drawing context. */
      GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							    region);
      cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
      cairo_t *cr = gdk_cairo_create (window);
#endif

      if (!NILP (bg_pmap) && IMAGE_INSTANCEP (bg_pmap))
	{
	  Lisp_Image_Instance *p = XIMAGE_INSTANCE (bg_pmap);
	  gdk_cairo_set_source_pixbuf(cr, IMAGE_INSTANCE_GTK_PIXMAP (p),
				      0.0, 0.0);
	  cairo_paint (cr);
	}

      if (EQ (font, Vthe_null_font_instance))
        {
          break;
        }

      cachel = WINDOW_FACE_CACHEL (w, findex);
      cr_set_foreground (cr, cachel->foreground);

      need_clipping = (dl->clip || clip_start > xpos ||
		       clip_end < xpos + width);

      if (cursor && focus && NILP (bar_cursor_value))
        {
          face_index ix = get_builtin_face_cache_index (w, Vtext_cursor_face);
          cachel = WINDOW_FACE_CACHEL (w, ix);
        }

      if (cachel->dim)
	{
	  /* TODO This should be done with by setting the alpha? -jsparkes */
	}

      if (need_clipping)
        {
          GdkRectangle clip_box;

          clip_box.x = 0;
          clip_box.y = 0;
          clip_box.width = clip_end - clip_start;
          clip_box.height = height;

          gdk_cairo_rectangle (cr, &clip_box);
          cairo_clip (cr);
        }

      gdk_draw_text_image (widget, cachel, cr, xpos, dl->ypos,
                           Vcharset_ascii, text_storage, extbytes);

      /* If we are actually superimposing the cursor then redraw with just
	 the appropriate section highlighted. */
      if (cursor_clip && cursor && focus)
	{
          GdkRectangle clip_box;

          clip_box.x = 0;
          clip_box.y = 0;
          clip_box.width = cursor_width;
          clip_box.height = height;

          gdk_cairo_rectangle (cr, &clip_box);
          cairo_clip (cr);
          gdk_draw_text_image (widget, cachel, cr, xpos, dl->ypos,
                               Vcharset_ascii, text_storage, extbytes);
	}

      xpos += width;
#if GTK_CHECK_VERSION(3, 22, 0)
      gdk_window_end_draw_frame (window, ctx);
      cairo_region_destroy (region);
#else
      cairo_destroy (cr);
#endif
    }
  while (0);

  /* Draw the non-focus box or bar-cursor as needed. */
  /* Can't this logic be simplified? */
  if (((cursor && !focus && NILP (bar_cursor_value))
	  || (cursor_width
	      && (cursor_start + cursor_width >= clip_start)
	      && !NILP (bar_cursor_value))))
    {
      int tmp_height, tmp_y;
      int need_clipping = (cursor_start < clip_start
			   || clip_end < cursor_start + cursor_width);

      /* #### This value is correct (as far as I know) because
	 all of the times we need to draw this cursor, we will
	 be called with exactly one character, so we know we
	 can always use runs[0].

	 This is bogus as all hell, however.  The cursor handling in
	 this function is way bogus and desperately needs to be
	 cleaned up.  (In particular, the drawing of the cursor should
	 really really be separated out of this function.  This may be
	 a bit tricky now because this function itself does way too
	 much stuff, a lot of which needs to be moved into
	 redisplay.c.)  This is the only way to be able to easily add
	 new cursor types or (e.g.) make the bar cursor be able to
	 span two characters instead of overlaying just one. */
      int bogusly_obtained_ascent_value =
	XFONT_INSTANCE (FACE_CACHEL_FONT (cachel, Vcharset_ascii))->ascent;

      face_index ix = get_builtin_face_cache_index (w, Vtext_cursor_face);
      struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL (w, ix);
      GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
      cairo_region_t *region = gdk_window_get_visible_region (window);
      /* We should be able to avoid the next call, but the GTK documentation
       * doesn't explain how to get the drawing context. */
      GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							    region);
      cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
      cairo_t *cr = gdk_cairo_create (window);
#endif

      assert (cursor_cachel);
      cr_set_foreground (cr, cursor_cachel->background);

      tmp_y = dl->ypos - bogusly_obtained_ascent_value;
      tmp_height = cursor_height;

      if (tmp_y + tmp_height > (int) (ypos + height))
	{
	  tmp_y = ypos + height - tmp_height;
          tmp_y = min (tmp_y, ypos);
	  tmp_height = ypos + height - tmp_y;
	}

      if (need_clipping)
	{
          cairo_rectangle (cr, 0, 0, clip_end - clip_start, tmp_height);
          cairo_clip (cr);
	}

      if (!focus && NILP (bar_cursor_value))
	{
          gtk_draw_rectangle (cr, cursor_start, tmp_y,
                              cursor_width - 1, tmp_height - 1);
	}
      else if (focus && !NILP (bar_cursor_value))
	{
          int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

          gtk_draw_rectangle (cr, cursor_start, tmp_y,
                              bar_width - 1, tmp_height - 1);

	}
#if GTK_CHECK_VERSION(3, 22, 0)
      gdk_window_end_draw_frame (window, ctx);
      cairo_region_destroy (region);
#else
      cairo_destroy (cr);
#endif
    }
}

static void
XLIKE_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x,
			   int y, int xoffset, int yoffset,
			   int width, int height,
			   XLIKE_COLOR UNUSED (fg), XLIKE_COLOR UNUSED (bg))
{
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);
  GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
  cairo_region_t *region = gdk_window_get_visible_region (window);
  /* We should be able to avoid the next call, but the GTK documentation
   * doesn't explain how to get the drawing context. */
  GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							region);
  cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
  cairo_t *cr = gdk_cairo_create (window);
#endif
  GdkPixbuf *pb, *scaled = NULL;

  assert (IMAGE_INSTANCE_GTK_PIXMAP (p) != NULL);
  pb = IMAGE_INSTANCE_GTK_PIXMAP (p);

  if (gdk_pixbuf_get_width (pb) == width &&
      gdk_pixbuf_get_height (pb) == height)
    {
      gdk_cairo_set_source_pixbuf (cr, IMAGE_INSTANCE_GTK_PIXMAP (p),
                                   x + xoffset, y + yoffset);
    } else {
      scaled = gdk_pixbuf_scale_simple (pb, width, height,
                                        GDK_INTERP_BILINEAR);
      gdk_cairo_set_source_pixbuf (cr, scaled, x + xoffset, y + yoffset);
  }
  cairo_paint (cr);
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame (window, ctx);
  cairo_region_destroy (region);
#else
  cairo_destroy (cr);
#endif

  if (scaled)
    g_object_unref (scaled);
}
