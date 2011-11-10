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

  /* Cursor-related variables */
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  int cursor_clip;
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));
  struct face_cachel *cursor_cachel = 0;

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

#ifdef THIS_IS_X
  int use_x_font = 1;		/* #### bogus!!
				   The logic of this function needs review! */
#endif
#ifdef USE_XFT
  Colormap cmap = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL (d);
  static XftColor fg, bg;
  XftDraw *xftDraw;

  /* Lazily initialize frame's xftDraw member. */
  if (!FRAME_X_XFTDRAW (f)) {
    FRAME_X_XFTDRAW (f) = XftDrawCreate (dpy, x_win, visual, cmap);
  }
  xftDraw = FRAME_X_XFTDRAW (f);

  /* #### This will probably cause asserts when passed a Lisp integer for a
     color.  See ca. line 759 this file.
     #### Maybe xft_convert_color should take an XColor, not a pixel. */
#define XFT_FROB_LISP_COLOR(color, dim)					\
  xft_convert_color (dpy, cmap, visual,					\
		     XCOLOR_INSTANCE_X_COLOR (color).pixel, (dim))
#endif /* USE_XFT */

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

  /* This cursor code is really a mess. */
  if (!NILP (w->text_cursor_visible_p)
      && (cursor
	  || cursor_clip
	  || (cursor_width
	      && (cursor_start + cursor_width >= clip_start)
	      && !NILP (bar_cursor_value))))
    {
      /* These have to be in separate statements in order to avoid a
         compiler bug. */
      face_index sucks = get_builtin_face_cache_index (w, Vtext_cursor_face);
      cursor_cachel = WINDOW_FACE_CACHEL (w, sucks);

      /* We have to reset this since any call to WINDOW_FACE_CACHEL
         may cause the cache to resize and any pointers to it to
         become invalid. */
      cachel = WINDOW_FACE_CACHEL (w, findex);
    }

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

#ifndef THIS_IS_GTK
  runs = alloca_array (struct textual_run, len);
  text_storage = (unsigned char *) ALLOCA (2 * len);
  nruns = separate_textual_runs (text_storage, runs, Dynarr_begin (buf),
				 Dynarr_length (buf), cachel);
#else
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
#endif

#ifdef THIS_IS_GTK
  /* XXX Horrible kludge to force display of the only block cursor
     I can get to work correctly!   -- jsparkes */
  if (NILP (bar_cursor_value))
    focus = 0;
#endif
  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;
      int need_clipping;

      if (EQ (font, Vthe_null_font_instance))
	continue;

#ifdef THIS_IS_GTK
      this_width = width;
#else
      this_width = XLIKE_text_width_single_run (f, cachel, runs + i);
#endif

      need_clipping = (dl->clip || clip_start > xpos ||
		       clip_end < xpos + this_width);

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

      if (cursor && cursor_cachel && focus && NILP (bar_cursor_value))
	{
#ifdef USE_XFT
	  fg = XFT_FROB_LISP_COLOR (cursor_cachel->foreground, 0);
	  bg = XFT_FROB_LISP_COLOR (cursor_cachel->background, 0);
#endif
	  gc = XLIKE_get_gc (f, font, cursor_cachel->foreground,
			     cursor_cachel->background, Qnil, Qnil, Qnil);
	}
      else if (cachel->dim)
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
#ifdef USE_XFT
	  fg = XFT_FROB_LISP_COLOR (cachel->foreground, 1);
	  bg = XFT_FROB_LISP_COLOR (cachel->background, 0);
#endif
	  gc = XLIKE_get_gc (f, font, cachel->foreground, cachel->background,
			     bg_pmap, cachel->background_placement, Qnil);
	}
      else
	{
#ifdef USE_XFT
	  fg = XFT_FROB_LISP_COLOR (cachel->foreground, 0);
	  bg = XFT_FROB_LISP_COLOR (cachel->background, 0);
#endif
	  gc = XLIKE_get_gc (f, font, cachel->foreground, cachel->background,
			     Qnil, Qnil, Qnil);
	}
#ifdef USE_XFT
      {
	XftFont *rf = FONT_INSTANCE_X_XFTFONT (fi);

	if (rf)
	  {
	    use_x_font = 0;
	    if (need_clipping)
	      {
		Region clip_reg = XCreateRegion();
		XRectangle clip_box = { clip_start, ypos,
					clip_end - clip_start, height };

		XUnionRectWithRegion (&clip_box, clip_reg, clip_reg); 
		XftDrawSetClip (xftDraw, clip_reg);
		XDestroyRegion (clip_reg);
	      }

	    if (!bgc)
	      {
		/* #### Neither rect_height nor XftTextExtents as computed
		   below handles the vertical space taken up by antialiasing,
		   which for some fonts (eg, Bitstream Vera Sans Mono-16 on
		   my Mac PowerBook G4) leaves behind orphaned dots on
		   insertion or deletion earlier in the line, especially in
		   the case of the underscore character.
		   Interestingly, insertion or deletion of a single character
		   immediately after a refresh does not leave any droppings,
		   but any further insertions or deletions do.
		   While adding a pixel to rect_height (mostly) takes care of
		   this, it trashes aggressively laid-out elements like the
		   modeline (overwriting part of the bevel).
		   OK, unconditionally redraw the bevel, and increment
		   rect_height by 1.  See x_output_display_block. -- sjt */
		struct textual_run *run = &runs[i];
		int rect_width = x_text_width_single_run (f, cachel, run);
#ifndef USE_XFTTEXTENTS_TO_AVOID_FONT_DROPPINGS
		int rect_height = FONT_INSTANCE_ASCENT (fi)
                  + FONT_INSTANCE_DESCENT (fi) + 1;
#else
		int rect_height = FONT_INSTANCE_ASCENT (fi)
                  + FONT_INSTANCE_DESCENT (fi);
		XGlyphInfo gi;
		if (run->dimension == 2) {
		  XftTextExtents16 (dpy,
				    FONT_INSTANCE_X_XFTFONT (fi),
				    (XftChar16 *) run->ptr, run->len, &gi);
		} else {
		  XftTextExtents8 (dpy,
				   FONT_INSTANCE_X_XFTFONT (fi),
				   run->ptr, run->len, &gi);
		}
		rect_height = rect_height > gi.height
                  ? rect_height : gi.height;
#endif

		XftDrawRect (xftDraw, &bg,
			     xpos, ypos, rect_width, rect_height);
	      }
	
	    if (runs[i].dimension == 1)
	      XftDrawString8 (xftDraw, &fg, rf, xpos, dl->ypos,
			      runs[i].ptr, runs[i].len);
	    else
	      XftDrawString16 (xftDraw, &fg, rf, xpos, dl->ypos,
			       (XftChar16 *) runs[i].ptr, runs[i].len);
	  }
      }
#endif /* USE_XFT */

#ifdef THIS_IS_X
      if (use_x_font)
#endif
	{
	  if (need_clipping)
	    {
	      XLIKE_RECTANGLE clip_box;

	      clip_box.x = 0;
	      clip_box.y = 0;
	      clip_box.width = clip_end - clip_start;
	      clip_box.height = height;

	      XLIKE_SET_CLIP_RECTANGLE (dpy, gc, clip_start, ypos, &clip_box);
	    }

#ifdef THIS_IS_X
	  if (runs[i].dimension == 1)
	    (bgc ? XDrawString : XDrawImageString)
	      (dpy, x_win, gc, xpos, dl->ypos,
	       (char *) runs[i].ptr, runs[i].len);
	  else
	    (bgc ? XDrawString16 : XDrawImageString16)
	      (dpy, x_win, gc, xpos, dl->ypos,
	       (XChar2b *) runs[i].ptr, runs[i].len);
#else /* THIS_IS_GTK */

          {
	    GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);

            gdk_draw_text_image (widget, cachel, gc,
				 xpos, dl->ypos, &runs[i]);
          }
#endif 
	}

#ifdef THIS_IS_X
      /* We draw underlines in the same color as the text. */
      if (cachel->underline)
	{
	  int upos, uthick;
	  unsigned long upos_ext, uthick_ext;
	  XFontStruct *fs =
	    use_x_font ? FONT_INSTANCE_X_FONT (XFONT_INSTANCE (font)) : 0;
	  /* #### the logic of the next two may be suboptimal: we may want
	     to use the POSITION and/or THICKNESS information with Xft */
	  if (fs && XGetFontProperty (fs, XA_UNDERLINE_POSITION, &upos_ext))
	    upos = (int) upos_ext;
	  else
	    upos = dl->descent / 2;
	  if (fs && XGetFontProperty (fs, XA_UNDERLINE_THICKNESS, &uthick_ext))
	    uthick = (int) uthick_ext;
	  else
	    uthick = 1;

	  if (dl->ypos + upos < dl->ypos + dl->descent - dl->clip)
	    {
	      if (dl->ypos + upos + uthick > dl->ypos + dl->descent - dl->clip)
		uthick = dl->descent - dl->clip - upos;

	      if (uthick == 1)
		{
		  XLIKE_DRAW_LINE (dpy, x_win, gc, xpos, dl->ypos + upos,
				   xpos + this_width, dl->ypos + upos);
		}
	      else if (uthick > 1)
		{
		  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, xpos,
					dl->ypos + upos, this_width, uthick);
		}
	    }
	}

      if (cachel->strikethru)
	{
	  int ascent, descent, upos, uthick;
	  unsigned long ascent_ext, descent_ext, uthick_ext;
	  XFontStruct *fs = FONT_INSTANCE_X_FONT (fi);

	  if (!use_x_font)
	    {
	      ascent = dl->ascent;
	      descent = dl->descent;
	      uthick = 1;
	    }
	  else
	    {
	      if (!XGetFontProperty (fs, XA_STRIKEOUT_ASCENT, &ascent_ext))
		ascent = fs->ascent;
	      else
		ascent = (int) ascent_ext;
	      if (!XGetFontProperty (fs, XA_STRIKEOUT_DESCENT, &descent_ext))
		descent = fs->descent;
	      else
		descent = (int) descent_ext;
	      if (!XGetFontProperty (fs, XA_UNDERLINE_THICKNESS, &uthick_ext))
		uthick = 1;
	      else
		uthick = (int) uthick_ext;
	    }
	  upos = ascent - ((ascent + descent) / 2) + 1;

	  /* Generally, upos will be positive (above the baseline),so
             subtract */
	  if (dl->ypos - upos < dl->ypos + dl->descent - dl->clip)
	    {
	      if (dl->ypos - upos + uthick > dl->ypos + dl->descent - dl->clip)
		uthick = dl->descent - dl->clip + upos;

	      if (uthick == 1)
		XLIKE_DRAW_LINE (dpy, x_win, gc, xpos, dl->ypos - upos,
				 xpos + this_width, dl->ypos - upos);
	      else if (uthick > 1)
                XLIKE_FILL_RECTANGLE (dpy, x_win, gc, xpos, dl->ypos + upos,
                                      this_width, uthick);
	    }
	}
#endif
      /* Restore the GC */
      if (need_clipping)
	{
#ifdef USE_XFT
	  if (!use_x_font)
	    {
	      XftDrawSetClip (xftDraw, 0);
	    }
	  else
#endif
	    XLIKE_CLEAR_CLIP_MASK (dpy, gc);
	}

      /* If we are actually superimposing the cursor then redraw with just
	 the appropriate section highlighted. */
      if (cursor_clip && !cursor && focus && cursor_cachel)
	{
#ifdef USE_XFT
	  if (!use_x_font)	/* Xft */
	    {
	      XftFont *rf = FONT_INSTANCE_X_XFTFONT (fi);
	  
	      { /* set up clipping */
		Region clip_reg = XCreateRegion();
		XRectangle clip_box = { cursor_start, ypos,
					cursor_width, height };
	    
		XUnionRectWithRegion (&clip_box, clip_reg, clip_reg); 
		XftDrawSetClip (xftDraw, clip_reg);
		XDestroyRegion (clip_reg);
	      }
	      { /* draw background rectangle & draw text */
		int rect_height = FONT_INSTANCE_ASCENT (fi)
                  + FONT_INSTANCE_DESCENT (fi);
		int rect_width = x_text_width_single_run (f, cachel, &runs[i]);
		XftColor xft_color;

		xft_color = XFT_FROB_LISP_COLOR (cursor_cachel->background, 0);
		XftDrawRect (xftDraw, &xft_color,
			     xpos, ypos, rect_width, rect_height);

		xft_color = XFT_FROB_LISP_COLOR (cursor_cachel->foreground, 0);
		if (runs[i].dimension == 1)
		  XftDrawString8 (xftDraw, &xft_color, rf, xpos, dl->ypos,
				  runs[i].ptr, runs[i].len);
		else
		  XftDrawString16 (xftDraw, &xft_color, rf, xpos, dl->ypos,
				   (XftChar16 *) runs[i].ptr, runs[i].len);
	      }

	      XftDrawSetClip (xftDraw, 0);
	    }
	  else			/* core font, not Xft */
#endif /* USE_XFT */
	    {
	      XLIKE_RECTANGLE clip_box;
	      XLIKE_GC cgc;
	      cgc = XLIKE_get_gc (f, font, cursor_cachel->foreground,
				  cursor_cachel->background, Qnil, Qnil, Qnil);

	      clip_box.x = 0;
	      clip_box.y = 0;
	      clip_box.width = cursor_width;
	      clip_box.height = height;

              XLIKE_SET_CLIP_RECTANGLE (dpy, cgc, cursor_start, ypos,
                                        &clip_box);
#ifdef THIS_IS_X
	      if (runs[i].dimension == 1)
		XDrawImageString (dpy, x_win, cgc, xpos, dl->ypos,
				  (char *) runs[i].ptr, runs[i].len);
	      else
		XDrawImageString16 (dpy, x_win, cgc, xpos, dl->ypos,
				    (XChar2b *) runs[i].ptr, runs[i].len);
#else
              {
		GtkWidget *widget = FRAME_GTK_TEXT_WIDGET(f);
                
                gdk_draw_text_image (widget, cursor_cachel, cgc,
				     xpos, dl->ypos, &runs[i]);
              }
#endif /* (not) THIS_IS_X */

	      XLIKE_CLEAR_CLIP_MASK (dpy, cgc);
	    }
	}

      xpos += this_width;
    }

  /* Draw the non-focus box or bar-cursor as needed. */
  /* Can't this logic be simplified? */
  if (cursor_cachel
      && ((cursor && !focus && NILP (bar_cursor_value))
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

      if (!NILP (bar_cursor_value))
	{
	  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			     Qnil, Qnil,
			     make_fixnum (bar_width));
	}
      else
	{
	  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background,
			     Qnil, Qnil, Qnil, Qnil);
	}
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
	  XLIKE_RECTANGLE clip_box;
	  clip_box.x = 0;
	  clip_box.y = 0;
	  clip_box.width = clip_end - clip_start;
	  clip_box.height = tmp_height;
	  XLIKE_SET_CLIP_RECTANGLE (dpy, gc, clip_start, tmp_y, &clip_box);
	}

      if (!focus && NILP (bar_cursor_value))
	{
	  XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, cursor_start, tmp_y, 
				cursor_width - 1, tmp_height - 1);
	}
      else if (focus && !NILP (bar_cursor_value))
	{
	  XLIKE_DRAW_LINE (dpy, x_win, gc, cursor_start + bar_width - 1, tmp_y,
			   cursor_start + bar_width - 1,
			   tmp_y + tmp_height - 1);
	}

      /* Restore the GC */
      if (need_clipping)
	{
	  XLIKE_CLEAR_CLIP_MASK (dpy, gc);
	}
    }

#ifdef USE_XFT
#undef XFT_FROB_LISP_COLOR
#endif
}
