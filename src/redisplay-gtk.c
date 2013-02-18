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
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

  if (!UNBOUNDP (background_pixmap))
    {
      /* XXX Implement me! */
      /*
      gc = XLIKE_get_gc (f, Qnil, fcolor, bcolor,
      background_pixmap, background_placement, Qnil);
      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);
      */
    }
  else
    {
      cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
      gtk_fill_rectangle (cr, x, y, width, height);
    }
  cairo_destroy (cr);
}

static void
XLIKE_clear_frame (struct frame *f)
{
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GdkWindow *w = gtk_widget_get_window (widget);

  if (w)
    gdk_window_clear (w);

#if 0
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
  /* int x, y, width, height; */
  Lisp_Object frame;

  /* #### GEOM! This clears the internal border and gutter (and scrollbars)
     but not the toolbar.  Correct? */
  /*
    x = FRAME_LEFT_INTERNAL_BORDER_START (f);
    width = (FRAME_RIGHT_INTERNAL_BORDER_END (f) - x);
  */
  /* #### This adjustment by 1 should be being done in the macros.
     There is some small differences between when the menubar is on
     and off that we still need to deal with.  The adjustment also occurs in
     redisplay_clear_top_of_window(). */
  /*
    y = FRAME_TOP_INTERNAL_BORDER_START (f) - 1;
    height = (FRAME_BOTTOM_INTERNAL_BORDER_END (f) - y);
  */

  /* cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR); */
  /* gtk_fill_rectangle (cr, x, y, width, height); */

  frame = wrap_frame (f);

  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      XLIKE_clear_frame_windows (f->root_window);
    }
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

/*****************************************************************************
 XLIKE_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
****************************************************************************/
static void
XLIKE_output_vertical_divider (struct window *w, int UNUSED (clear))
{
  struct frame *f = XFRAME (w->frame);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  GtkStyle *gstyle = FRAME_GTK_TEXT_WIDGET (f)->style;
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
  enum edge_style style;
  int x, ytop, ybottom, width, shadow_thickness, spacing;
  face_index div_face =
    get_builtin_face_cache_index (w, Vvertical_divider_face);

  width = window_divider_width (w);
  shadow_thickness = XFIXNUM (w->vertical_divider_shadow_thickness);
  spacing = XFIXNUM (w->vertical_divider_spacing);
  /* line_width = XFIXNUM (w->vertical_divider_line_width); */
  x = WINDOW_RIGHT (w) - width;
  ytop = WINDOW_TOP (w);
  ybottom = WINDOW_BOTTOM (w);

  cr_set_foreground (cr, WINDOW_FACE_CACHEL_BACKGROUND (w, div_face));
  gtk_fill_rectangle (cr, x, ytop, width, ybottom - ytop);

  if (shadow_thickness < 0)
    {
      shadow_thickness = -shadow_thickness;
      style = EDGE_BEVEL_IN;
    }
  else
    {
      style = EDGE_BEVEL_OUT;
    }

  cr_set_foreground (cr, WINDOW_FACE_CACHEL_FOREGROUND (w, div_face));
  /* Draw the shadows around the divider line */
  gtk_bevel_area (w, div_face, x + spacing, ytop,
		  width - 2 * spacing, ybottom - ytop,
		  shadow_thickness, EDGE_ALL, style);
  gtk_paint_vline (gstyle, x_win, GTK_STATE_NORMAL, NULL,
		   FRAME_GTK_TEXT_WIDGET (f), "vline", ytop + spacing,
		   ybottom + spacing, x + width / 2);
  cairo_destroy (cr);
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
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

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
      GtkStyle *style = FRAME_GTK_TEXT_WIDGET (f)->style;
      XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);

      gtk_paint_hline (style, x_win, GTK_STATE_NORMAL, NULL,
		       FRAME_GTK_TEXT_WIDGET (f), "hline", x, x + width,
		       ypos3 + rb->object.hline.thickness / 2);
      /* gtk_fill_rectangle (cr, x, ypos3, width, rb->object.hline.thickness); */
    }
  cairo_destroy (cr);
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
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
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

  cairo_destroy (cr);
  return 1;
}

static void
XLIKE_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
		       face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
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

  cairo_destroy (cr);
}

#include "sysgdkx.h"

static void
cr_set_foreground (cairo_t *cr, Lisp_Object color)
{
  GdkColor *fg = XCOLOR_INSTANCE_GTK_COLOR (color);

  cairo_set_source_rgb (cr,
			(double) fg->red/65535,
			(double) fg->green/65535,
			(double) fg->blue/65535);
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

  return attr_list;
}

static void
gdk_draw_text_image (GtkWidget *widget, struct face_cachel *cachel, cairo_t *cr,
		     gint x, gint y, struct textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);

  PangoContext *context = gtk_widget_get_pango_context (widget);
  /* PangoLayout *layout = pango_layout_new (context); */
  PangoLayout *layout = pango_cairo_create_layout (cr);
  PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
  PangoFontMetrics *pfm = FONT_INSTANCE_GTK_FONT_METRICS (fi);
  PangoAttrList *attr_list = gtk_text_attributes (cachel);
  GList *items = NULL, *current = NULL;;

  assert (run->dimension == 1);  /* UTF-8 only. */

  pango_layout_set_attributes (layout, attr_list);
  pango_layout_set_font_description (layout, pfd);
  /* Pango breaks text into directional sections. */
  items = pango_itemize (context, (const char *) run->ptr, 0, run->len,
                         attr_list, 0);

  current = items;
  while (current)
    {
      PangoItem *item = (PangoItem *) current->data;
      gint ascent = 0;
      gint width = 0;
      gint height = 0;

      pango_layout_set_text (layout, (const char *) run->ptr + item->offset,
                             item->length);
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
    }
  g_list_free (items);
  g_object_unref (layout);
  pango_attr_list_unref (attr_list);
}

/* XLIKE_text_width

   Given a string and a merged face, return the string's length in pixels
   when displayed in the fonts associated with the face. */

static int
XLIKE_text_width (struct frame *f, struct face_cachel *cachel,
		  const Ichar *str, Charcount len)
{
  Ibyte *int_storage = alloca_ibytes (MAX_ICHAR_LEN * len);
  Ibyte *int_storage_ptr = int_storage;
  Extbyte *alloca_ext_storage;
  Bytecount extbytes = 0;
  gint width = 0;
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  int ii;

  for (ii = 0; ii < len; ii++)
    {
      int_storage_ptr += set_itext_ichar (int_storage_ptr, str[ii]);
    }

  TO_EXTERNAL_FORMAT (DATA, (int_storage, int_storage_ptr - int_storage),
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
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

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
  cairo_destroy (cr);
}

void
XLIKE_output_string (struct window *w, struct display_line *dl,
		     Ichar_dynarr *buf, int xpos, int xoffset, int clip_start,
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
  int len = Dynarr_length (buf);
  unsigned char *text_storage;
  struct textual_run *runs;
  int nruns;
  int i;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  if (cursor == 1)
    assert (Dynarr_length (buf) == 1);

  if (width < 0)
    width = XLIKE_text_width (w, cachel, Dynarr_begin (buf),
			      Dynarr_length (buf));

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

  runs = alloca_new (struct textual_run);
  nruns = 1;
  {
    Elemcount ii, extbytes;
    Ibyte *int_storage = alloca_ibytes (MAX_ICHAR_LEN * len);
    Ibyte *int_storage_ptr = int_storage;

    for (ii = 0; ii < len; ii++)
      {
        int_storage_ptr += set_itext_ichar (int_storage_ptr,
                                            Dynarr_at (buf, ii));
      }

    TO_EXTERNAL_FORMAT (DATA, (int_storage, int_storage_ptr - int_storage),
                        ALLOCA, (text_storage, extbytes),
                        Qutf_8);

    runs->ptr = text_storage;
    runs->len = extbytes;
    runs->dimension = 1;
    runs->charset = Vcharset_ascii;
  }

 for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      int need_clipping;
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

      if (!NILP (bg_pmap) && IMAGE_INSTANCEP (bg_pmap))
	{
	  Lisp_Image_Instance *p = XIMAGE_INSTANCE (bg_pmap);
	  gdk_cairo_set_source_pixbuf(cr, IMAGE_INSTANCE_GTK_PIXMAP (p),
				      0.0, 0.0);
	}

      if (EQ (font, Vthe_null_font_instance))
	continue;

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
	  /* Ensure the gray bitmap exists */
	  if (DEVICE_XLIKE_GRAY_PIXMAP (d) == XLIKE_NONE)
	    DEVICE_XLIKE_GRAY_PIXMAP (d) =
	      /* #### FIXME! Implement me! */
	      XLIKE_NONE;

	  /* Request a GC with the gray stipple pixmap to draw dimmed text */
	  /* gc = XLIKE_get_gc (f, font, cachel->foreground, cachel->background,
             bg_pmap, cachel->background_placement, Qnil); */
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

      gdk_draw_text_image (widget, cachel, cr, xpos, dl->ypos, &runs[i]);

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
          gdk_draw_text_image (widget, cachel, cr,
				     xpos, dl->ypos, &runs[i]);
	}

      xpos += width;
      cairo_destroy (cr);
    }

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
	XFONT_INSTANCE (FACE_CACHEL_FONT (cachel, runs[0].charset))->ascent;

      face_index ix = get_builtin_face_cache_index (w, Vtext_cursor_face);
      struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL (w, ix);
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));

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

      cairo_destroy (cr);
    }
}

static void
XLIKE_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x,
			   int y, int xoffset, int yoffset,
			   int width, int height,
			   XLIKE_COLOR fg, XLIKE_COLOR bg)
{
  struct device *d = XDEVICE (f->device);
  /* XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d); */
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);
  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
  double xpos, ypos;

  /* There is a possibility that callers expect the pixbuf to be
     resized. */

  gdk_cairo_set_source_pixbuf (cr, IMAGE_INSTANCE_GTK_PIXMAP (p),
			       x, y);
  cairo_paint (cr);
  cairo_destroy (cr);
}
