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

static void
gtk_set_source_rgb (cairo_t *cr, GdkColor *fg)
{
  cairo_set_source_rgb (cr,
                        (double) fg->red/65535,
                        (double) fg->green/65535,
                        (double) fg->blue/65535);
}

#ifdef UNUSED
static void
gtk_fill_rectangle (cairo_t *cr, gint x, gint y,
                    gint width, gint height)
{
  cairo_new_path (cr);
  cairo_set_line_width (cr, 0.5);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
}
#endif

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


/* XLIKE_text_width

   Given a string and a merged face, return the string's length in pixels
   when displayed in the fonts associated with the face. */

static int
XLIKE_text_width (struct window *w, struct face_cachel *cachel,
		  const Ichar *str, Charcount len)
{
  Ibyte *int_storage = alloca_ibytes (MAX_ICHAR_LEN * len);
  Ibyte *int_storage_ptr = int_storage;
  Extbyte *alloca_ext_storage;
  Bytecount extbytes = 0;
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (WINDOW_XFRAME (w));
  gint width, height;
  PangoContext *context = gtk_widget_get_pango_context (widget);
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *pfd = FONT_INSTANCE_GTK_FONT_DESC (fi);
  PangoAttrList *attr_list = pango_attr_list_new ();
  int ii;

  for (ii = 0; ii < len; ii++)
    {
      int_storage_ptr += set_itext_ichar (int_storage_ptr, str[ii]);
    }

  TO_EXTERNAL_FORMAT (DATA, (int_storage, int_storage_ptr - int_storage),
                      ALLOCA, (alloca_ext_storage, extbytes),
                      Qutf_8);

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
  return width;
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
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  Lisp_Object window = wrap_window (w);
  int clip_end;
  GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);
  
  /* Cursor-related variables */
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  int cursor_clip;
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));
  /* Text-related variables */
  Lisp_Object bg_pmap;
  XLIKE_GC gc, bgc;
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
    bgc = 0;
  else
    {
      /* Clear the cursor location? */
      bgc = XLIKE_get_gc (f, Qnil, cachel->background, cachel->background,
                          bg_pmap, cachel->background_placement, Qnil);
      XLIKE_FILL_RECTANGLE (dpy, x_win, bgc, clip_start,
                            ypos, clip_end - clip_start,
                            height);
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
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;
      int need_clipping;
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
      GdkColor *fg = XCOLOR_INSTANCE_GTK_COLOR (cachel->foreground);

      if (EQ (font, Vthe_null_font_instance))
	continue;

      cachel = WINDOW_FACE_CACHEL (w, findex);
      gtk_set_source_rgb (cr, fg);

#ifdef THIS_IS_GTK
      this_width = width;
#else
      this_width = XLIKE_text_width_single_run (f, cachel, runs + i);
#endif

      need_clipping = (dl->clip || clip_start > xpos ||
		       clip_end < xpos + this_width);

 #if 0
      /* XDrawImageString only clears the area equal to the height of
	 the given font.  It is possible that a font is being displayed
	 on a line taller than it is, so this would cause us to fail to
	 clear some areas. */
      if ((int) fi->height < (int) (height + dl->clip +
				    XLIKE_DISPLAY_LINE_TOP_CLIP (dl)))
	{
	  int clear_start = max (xpos, clip_start);
	  int clear_end = min (xpos + this_width, clip_end);

	  if (cursor)
	    {
	      int ypos1_line, ypos1_string, ypos2_line, ypos2_string;

	      ypos1_string = dl->ypos - fi->ascent;
	      ypos2_string = dl->ypos + fi->descent;
	      ypos1_line = ypos;
	      ypos2_line = ypos1_line + height;

	      /* Make sure we don't clear below the real bottom of the
		 line. */
              ypos1_string = min (ypos1_string, ypos2_line);
              ypos2_string = min (ypos2_string, ypos2_line);

	      if (ypos1_line < ypos1_string)
		{
		  redisplay_clear_region (window, findex, clear_start, ypos1_line,
                                          clear_end - clear_start,
                                          ypos1_string - ypos1_line);
		}

	      if (ypos2_line > ypos2_string)
		{
		  redisplay_clear_region (window, findex, clear_start, ypos2_string,
                                          clear_end - clear_start,
                                          ypos2_line - ypos2_string);
		}
	    }
	  else
	    {
	      redisplay_clear_region (window, findex, clear_start,
                                      ypos, clear_end - clear_start,
                                      height);
	    }
	}
#endif

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
#ifdef THIS_IS_X
	      XCreateBitmapFromData (dpy, x_win, (char *)gray_bits,
				     gray_width, gray_height)
#else
	      /* #### FIXME! Implement me! */
	      XLIKE_NONE
#endif
	      ;

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

      xpos += this_width;
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
      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;
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
      GdkColor *fg = XCOLOR_INSTANCE_GTK_COLOR (cursor_cachel->background);

      gtk_set_source_rgb (cr, fg);
      assert (cursor_cachel);

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
	  GdkRectangle clip_box;
	  clip_box.x = 0;
	  clip_box.y = 0;
	  clip_box.width = clip_end - clip_start;
	  clip_box.height = tmp_height;

          gdk_cairo_rectangle (cr, &clip_box);
          cairo_clip (cr);
	}

      if (!focus && NILP (bar_cursor_value))
	{
          gtk_draw_rectangle (cr, cursor_start, tmp_y,
                              cursor_width - 1, tmp_height - 1);
	}
      else if (focus && !NILP (bar_cursor_value))
	{
          gtk_draw_rectangle (cr, cursor_start, tmp_y,
                              bar_width - 1, tmp_height - 1);

	}

      cairo_destroy (cr);
    }
}
