/* mswindows output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2003, 2010 Ben Wing.

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

/* I think this file is essentially Mule-ized, but I'm not sure!
   Could stand a good once-over.  Unicode support is trash, of course. */

/* Authorship:

   Chuck Thompson
   Lots of work done by Ben Wing for Mule

   Partially rewritten for mswindows by Jonathan Harris, November 1997
   for 21.0.  */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "debug.h"
#include "device-impl.h"
#include "events.h"
#include "faces.h"
#include "frame-impl.h"
#include "gutter.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window-impl.h"

#include "console-msw-impl.h"
#include "glyphs-msw.h"
#include "fontcolor-msw-impl.h"

#define MSWINDOWS_EOL_CURSOR_WIDTH	5

/*
 * Random forward declarations
 */
static void mswindows_update_dc (HDC hdc, Lisp_Object fg, Lisp_Object bg,
				 Lisp_Object bg_pmap);
static void mswindows_set_dc_font (HDC hdc, Lisp_Object font,
				   int under, int strike);
static void mswindows_output_vertical_divider (struct window *w, int clear);
static void mswindows_output_dibitmap (struct frame *f, 
				       Lisp_Image_Instance *p,
				       struct display_box *db,
				       struct display_glyph_area *dga);

typedef struct textual_run
{
  Lisp_Object charset; /* charset of this run */
  WCHAR *ptr; /* pointer to Unicode chars in this run */
  Elemcount nwchars; /* number of Unicode chars in this run */
} textual_run;

/* Separate out the text in STR into a series of textual runs of a particular
   charset.  Returns the number of runs actually used.  Returns the textual
   runs in RUN_STORAGE, which is presumed to contain enough space for the
   worst case, which is LEN runs. */

static Elemcount
separate_textual_runs (WCHAR *text_storage, struct textual_run *run_storage,
                       const Ibyte *str, Bytecount len)
{
  Lisp_Object prev_charset = Qunbound;
  const Ibyte *end = str + len; 
  Elemcount runs_so_far = 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].nwchars = 0;

  while (str < end)
    {
      Ichar ch = itext_ichar (str);
      Lisp_Object charset = ichar_charset (ch);

      if (!EQ (charset, prev_charset))
	{
	  if (runs_so_far)
            {
              run_storage[runs_so_far-1].nwchars
                = text_storage - run_storage[runs_so_far-1].ptr;
            }

	  run_storage[runs_so_far].ptr = text_storage;
	  run_storage[runs_so_far].charset = charset;
	  prev_charset = charset;
	  runs_so_far++;
	}

      if (valid_utf_16_first_surrogate (*text_storage))
        {
          text_storage++;
        }

      text_storage++;
      INC_IBYTEPTR (str);
    }

  if (runs_so_far)
    {
      run_storage[runs_so_far-1].nwchars
        = text_storage - run_storage[runs_so_far-1].ptr;
    }

  return runs_so_far;
}

static int
mswindows_text_width_single_run (HDC hdc, struct face_cachel *cachel,
				 textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  SIZE size;

  /* The X11 code doesn't have to do this explicitly, because there we trust
     the font instance to know whether it's actually proportional or not,
     and we use the zero width that is stored in the monospace null font
     instance.  */
  if (EQ (Vthe_null_font_instance, font_inst))
    {
      return 0;
    }

#if 0 /* #### not the way of ikeyama's ws */
  if (!fi->proportional_p || !hdc)
    {
      if (XCHARSET_DIMENSION (run->charset) == 2)
	/* Don't trust FONT_INSTANCE_WIDTH. Asian fonts have both of
	   one and two column characters. */
	goto the_hard_way;
      else
	return fi->width * run->nchars;
    }
  else
    {
    the_hard_way:
#endif
      mswindows_set_dc_font (hdc, font_inst,
			     cachel->underline, cachel->strikethru);
      GetTextExtentPoint32W (hdc, run->ptr, run->nwchars, &size);
      return size.cx;
#if 0 /* #### not the way of ikeyama's ws */
    }
#endif
}

/*
 * Given F, retrieve device context. F can be a display frame, or
 * a print job. For a print job, page is also started when printer's
 * device context is first time requested. 
 */
static HDC
get_frame_dc (struct frame *f, int start_page_p)
{
  if (FRAME_MSWINDOWS_P (f))
    return FRAME_MSWINDOWS_DC (f);
  else
    {
      if (start_page_p && !FRAME_MSPRINTER_PAGE_STARTED (f))
	msprinter_start_page (f);
      return DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f)));
    }
}

/*
 * Given F, retrieve compatible device context. F can be a display
 * frame, or a print job.
 */
static HDC
get_frame_compdc (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  if (DEVICE_MSWINDOWS_P (d))
    return DEVICE_MSWINDOWS_HCDC (d);
  else
    return DEVICE_MSPRINTER_HCDC (d);
}

/*****************************************************************************
 mswindows_update_dc

 Given a number of parameters munge the DC so it has those properties.
 ****************************************************************************/
static void
mswindows_update_dc (HDC hdc, Lisp_Object fg, Lisp_Object bg,
		     Lisp_Object UNUSED (bg_pmap))
{
  if (!NILP (fg))
    {
      SetTextColor (hdc, COLOR_INSTANCE_MSWINDOWS_COLOR 
		    (XCOLOR_INSTANCE (fg)));
    }

  if (!NILP (bg))
    { 
      SetBkMode (hdc, OPAQUE);
      SetBkColor (hdc, COLOR_INSTANCE_MSWINDOWS_COLOR (XCOLOR_INSTANCE (bg)));
    }
  else 
    {
      SetBkMode (hdc, TRANSPARENT);
    }
}

static void
mswindows_set_dc_font (HDC hdc, Lisp_Object font, int under, int strike)
{
  SelectObject (hdc, mswindows_get_hfont (XFONT_INSTANCE (font),
					  under, strike));
}

/*****************************************************************************
 mswindows_output_hline

 Output a horizontal line in the foreground of its face.
 ****************************************************************************/
static void
mswindows_output_hline (struct window *UNUSED (w),
			struct display_line *UNUSED (dl),
			struct rune *UNUSED (rb))
{ /* #### Implement me */
}


/*****************************************************************************
 mswindows_output_blank

 Output a blank by clearing the area it covers in the background color
 of its face.
 ****************************************************************************/
static void
mswindows_output_blank (struct window *w, struct display_line *dl, 
			struct rune *rb, int start_pixpos)
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);
  RECT rect = { rb->xpos, DISPLAY_LINE_YPOS (dl),
		rb->xpos+rb->width, 
		DISPLAY_LINE_YEND (dl) };
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, rb->findex);

  Lisp_Object bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, rb->findex);

  /* Unmap all subwindows in the area we are going to blank. */
  redisplay_unmap_subwindows_maybe (f, rb->xpos, DISPLAY_LINE_YPOS (dl),
				    rb->width, DISPLAY_LINE_HEIGHT (dl));

  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (!NILP(bg_pmap))
    {
      struct display_box db;
      struct display_glyph_area dga;
      redisplay_calculate_display_boxes (dl, rb->xpos, 
					 /*rb->object.dglyph.xoffset*/ 0,
                                         /*rb->object.dglyph.yoffset*/ 0,
					 start_pixpos, rb->width,
					 &db, &dga);
      /* blank the background in the appropriate color */
      mswindows_update_dc (hdc, cachel->foreback,
			   cachel->background, Qnil);
      redisplay_output_pixmap (w, bg_pmap, &db, &dga, rb->findex,
			       0, 0, 0, TRUE);
    }
  else 
    {
      mswindows_update_dc (hdc, Qnil, cachel->background, Qnil);
      ExtTextOutW (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }
}


/*****************************************************************************
 mswindows_output_cursor

 Draw a normal or end-of-line cursor. The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
mswindows_output_cursor (struct window *w, struct display_line *dl, int xpos,
			 int width, face_index findex, Ichar ch, int image_p)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object font = Qnil;
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  HDC hdc = get_frame_dc (f, 1);
  int local_face_index = 0;
  RECT rect = { xpos,
		DISPLAY_LINE_YPOS (dl),
		xpos + width,
		DISPLAY_LINE_YEND (dl) };
  Lisp_Object bar = symbol_value_in_buffer (Qbar_cursor,
					    WINDOW_BUFFER (w));
  int bar_p = image_p || !NILP (bar);
  int cursor_p = !NILP (w->text_cursor_visible_p);
  int real_char_p = ch != 0;
  WCHAR *ptr = NULL;
  Elemcount nwchars = 0;

  /* Unmap all subwindows in the area we are going to blank. */
  redisplay_unmap_subwindows_maybe (f, xpos, DISPLAY_LINE_YPOS (dl),
				    width, DISPLAY_LINE_HEIGHT (dl));

  if (real_char_p)
    {
      /* Use the font from the underlying character */
      struct face_cachel *font_cachel = WINDOW_FACE_CACHEL (w, findex);
      Ibyte chbuf[MAX_ICHAR_LEN];

      font = FACE_CACHEL_FONT (font_cachel, ichar_charset (ch));
      mswindows_set_dc_font (hdc, font,
			     font_cachel->underline, font_cachel->strikethru);

      TO_EXTERNAL_FORMAT (DATA, (chbuf, set_itext_ichar (chbuf, ch)),
                          ALLOCA, (ptr, nwchars), Qmswindows_unicode);

      nwchars /= 2; /* TO_EXTERNAL_FORMAT gave us a bytecount, we want the
                       number of WCHARS, which is always half that. */
    }

  if (!image_p)
    {
      struct face_cachel *color_cachel;

      /* Use cursor fg/bg for block cursor, or character fg/bg for the bar
	 or when we need to erase the cursor. Output nothing at eol if bar
	 cursor */
      local_face_index = get_builtin_face_cache_index (w, Vtext_cursor_face);
      color_cachel = WINDOW_FACE_CACHEL (w, ((!cursor_p || bar_p) ?
					     findex : local_face_index));
      mswindows_update_dc (hdc, color_cachel->foreground,
			   color_cachel->background, Qnil);
      ExtTextOutW (hdc, xpos, dl->ypos, ETO_OPAQUE|ETO_CLIPPED, &rect,
	           ptr, nwchars, NULL);
    }

  if (!cursor_p)
    return;

  if (focus && bar_p)
    {
      struct face_cachel *cursor_cachel;
      rect.right = rect.left + (EQ (bar, Qt) ? 1 : min (2, width));
      local_face_index = get_builtin_face_cache_index (w, Vtext_cursor_face);
      cursor_cachel = WINDOW_FACE_CACHEL (w, local_face_index);
      mswindows_update_dc (hdc, Qnil, cursor_cachel->background, Qnil);
      ExtTextOutW (hdc, xpos, dl->ypos, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }
  else if (!focus)
    {
      struct face_cachel *cursor_cachel;

      /* Now have real character drawn in its own color. We deflate
	 the rectangle so character cell will be bounded by the
	 previously drawn cursor shape */
      InflateRect (&rect, -1, -1);
      local_face_index = get_builtin_face_cache_index (w, Vdefault_face);
      cursor_cachel = 
	WINDOW_FACE_CACHEL (w, (real_char_p ? findex : local_face_index));
      mswindows_update_dc (hdc, 
			   cursor_cachel->foreground, 
			   cursor_cachel->background, Qnil);
      ExtTextOutW (hdc, xpos, dl->ypos, ETO_OPAQUE | ETO_CLIPPED,
		   &rect, ptr, nwchars, NULL);
    }

#ifdef MULE
  if (DEVICE_MSWINDOWS_P (d) &&
      (FRAME_MSWINDOWS_CURSOR_X (f) != xpos
       || FRAME_MSWINDOWS_CURSOR_Y (f) != DISPLAY_LINE_YPOS (dl)
       || FRAME_MSWINDOWS_CURSOR_FINDEX (f) != findex))
    {
      HWND hwnd = FRAME_MSWINDOWS_HANDLE(f);
      HIMC himc = ImmGetContext (hwnd);

      FRAME_MSWINDOWS_CURSOR_X (f) = xpos;
      FRAME_MSWINDOWS_CURSOR_Y (f) = DISPLAY_LINE_YPOS (dl);
      FRAME_MSWINDOWS_CURSOR_FINDEX (f) = findex;

    /* If the composition window is active, reset position of the
       composition window. */
      if (qxeImmGetCompositionString (himc, GCS_COMPSTR, NULL, 0))
	mswindows_start_ime_composition (f);

      ImmReleaseContext (hwnd, himc);
    }
#endif /* MULE */
}


/*****************************************************************************
 mswindows_output_string

 Given a string and a starting position, output that string in the
 given face.
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
 ****************************************************************************/
static void
mswindows_output_string (struct window *w, struct display_line *dl,
			 const Ibyte *buf, Bytecount len,
                         int xpos, int xoffset, int clip_start,
                         int width, face_index findex,
			 int UNUSED (cursor), int UNUSED (cursor_start),
			 int UNUSED (cursor_width), int UNUSED (cursor_height))
{
  struct frame *f = XFRAME (w->frame);
  /* struct device *d = XDEVICE (f->device);*/
  Lisp_Object window;
  HDC hdc = get_frame_dc (f, 1);
  int clip_end;
  Lisp_Object bg_pmap;
  Extbyte *text_storage;
  textual_run *runs;
  Elemcount nruns;
  Bytecount text_storage_len;
  int i, height;
  RECT rect;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  window = wrap_window (w);

#if 0	/* #### FIXME? */
  /* We can't work out the width before we've set the font in the DC */
  if (width < 0)
    width = mswindows_text_width (f, cachel, Dynarr_begin (buf),
				  Dynarr_length (buf));
#else
  assert (width >= 0);
#endif

  /* Regularize the variables passed in. */
  if (clip_start < xpos)
    clip_start = xpos;
  clip_end = xpos + width;
  if (clip_start >= clip_end)
    /* It's all clipped out. */
    return;

  xpos -= xoffset;

  /* sort out the destination rectangle */
  height = DISPLAY_LINE_HEIGHT (dl);
  rect.left = clip_start;
  rect.top  = DISPLAY_LINE_YPOS (dl);
  rect.right = clip_end;
  rect.bottom = rect.top + height;

  /* make sure the area we are about to display is subwindow free. */
  redisplay_unmap_subwindows_maybe (f, clip_start, DISPLAY_LINE_YPOS (dl),
				    clip_end - clip_start, DISPLAY_LINE_HEIGHT (dl));

  /* output the background pixmap if there is one */
  bg_pmap = cachel->background_pixmap;
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (!NILP(bg_pmap))
    {
      struct display_box db;
      struct display_glyph_area dga;
      redisplay_calculate_display_boxes (dl, xpos + xoffset, 0, 0,
					 clip_start, width, &db, &dga);
      /* blank the background in the appropriate color */
      mswindows_update_dc (hdc, cachel->foreback, cachel->background, Qnil);
      redisplay_output_pixmap (w, bg_pmap, &db, &dga, findex,
			       0, 0, 0, TRUE);
      /* output pixmap calls this so we have to recall to get correct
         references */
      cachel = WINDOW_FACE_CACHEL (w, findex);
    }

  TO_EXTERNAL_FORMAT (DATA, (buf, len),
                      ALLOCA, (text_storage, text_storage_len),
                      Qmswindows_unicode);

  /* TEXT_STORAGE_LEN / 2 will be smaller than LEN, while still being an
     inclusive upper bound on the number of possible textual runs (the maximum
     value possible is the number of Ichars at BUF). */
  runs = alloca_array (textual_run, text_storage_len >> 1);

  nruns = separate_textual_runs ((WCHAR *) text_storage, runs, buf, len);

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      mswindows_update_dc (hdc, cachel->foreground,
			   NILP(bg_pmap) ? cachel->background : Qnil, Qnil);
      mswindows_set_dc_font (hdc, font, cachel->underline, cachel->strikethru);

      this_width = mswindows_text_width_single_run (hdc, cachel, runs + i);
      
      /* cope with fonts taller than lines */
      if ((int) fi->height < (int) (height + dl->clip + dl->top_clip))
	{
	  int clear_start = max (xpos, clip_start);
	  int clear_end = min (xpos + this_width, clip_end);
	  
	  {
	    redisplay_clear_region (window, findex, clear_start,
				    DISPLAY_LINE_YPOS (dl), 
				    clear_end - clear_start,
				    height);
	    /* output pixmap calls this so we have to recall to get correct
	       references */
	    cachel = WINDOW_FACE_CACHEL (w, findex);
	  }
	}

      ExtTextOutW (hdc, xpos, dl->ypos,
		   NILP(bg_pmap) ? ETO_CLIPPED | ETO_OPAQUE : ETO_CLIPPED,
		   &rect, runs[i].ptr, runs[i].nwchars, NULL);

      xpos += this_width;
    }
}

static void
mswindows_output_dibitmap (struct frame *f, Lisp_Image_Instance *p,
			   struct display_box *db,
			   struct display_glyph_area *dga)
{
  HDC hdc = get_frame_dc (f, 1);
  HDC hcompdc = get_frame_compdc (f);
  HGDIOBJ old=NULL;
  const int real_x = IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_WIDTH (p);
  const int real_y = IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_HEIGHT (p);
  const int surface_x = IMAGE_INSTANCE_PIXMAP_WIDTH (p);
  const int surface_y = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);

  /* first blit the mask */
  if (IMAGE_INSTANCE_MSWINDOWS_MASK (p))
    {
      RGBQUAD bg;
      COLORREF bgcolor;

      old = SelectObject (hcompdc, IMAGE_INSTANCE_MSWINDOWS_MASK (p));
      
      if (IMAGE_INSTANCE_TYPE (p) == IMAGE_MONO_PIXMAP)
       {
         COLORREF fgcolor;
         RGBQUAD fg;

         fgcolor = GetTextColor (hdc);
         fg.rgbBlue = GetBValue (fgcolor);
         fg.rgbRed = GetRValue (fgcolor);
         fg.rgbGreen = GetGValue (fgcolor);
         fg.rgbReserved = 0;
         SetDIBColorTable (hcompdc, 0, 1, &fg);
       }

      bgcolor = GetBkColor (hdc);
      bg.rgbBlue = GetBValue (bgcolor);
      bg.rgbRed = GetRValue (bgcolor);
      bg.rgbGreen = GetGValue (bgcolor);
      bg.rgbReserved = 0;
      SetDIBColorTable (hcompdc, 1, 1, &bg);

      StretchBlt (hdc, 
		  db->xpos, db->ypos,
		  dga->width, dga->height, 
		  hcompdc,
		  MulDiv (dga->xoffset, real_x, surface_x),
		  MulDiv (dga->yoffset, real_y, surface_y),
		  MulDiv (dga->width, real_x, surface_x),
		  MulDiv (dga->height, real_y, surface_y),
		  SRCCOPY);                  

      SelectObject (hcompdc, old);
    }
  
  /* Now blit the bitmap itself, or one of its slices. */
  old = SelectObject (hcompdc,
		      IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICE 
		      (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)));

  StretchBlt (hdc, 
	      db->xpos, db->ypos,
	      dga->width, dga->height,
	      hcompdc,
	      MulDiv (dga->xoffset, real_x, surface_x),
	      MulDiv (dga->yoffset, real_y, surface_y),
	      MulDiv (dga->width, real_x, surface_x),
	      MulDiv (dga->height, real_y, surface_y),
	      IMAGE_INSTANCE_MSWINDOWS_MASK (p) ? SRCINVERT : SRCCOPY);

  SelectObject (hcompdc, old);
}

/* Return x MOD y, but the result is guaranteed positive */

static int
posmod (int x, int y)
{
  int retval = x % y;
  if (retval < 0)
    retval += y;
  return retval;
}

/* X gc's have this nice property that setting the bg pixmap will
 * output it offset relative to the window. Windows doesn't have this
 * feature so we have to emulate this by outputting multiple pixmaps.
 * This is only used for background pixmaps. Normal pixmaps are
 * outputted once and are scrollable */
static void
mswindows_output_dibitmap_region (struct frame *f, 
				  Lisp_Image_Instance *p,
				  struct display_box *db,
				  struct display_glyph_area *dga,
				  int absolute)
{
  struct display_box xdb = { db->xpos, db->ypos, db->width, db->height };
  struct display_glyph_area xdga
    = { 0, 0, IMAGE_INSTANCE_PIXMAP_WIDTH (p),
	IMAGE_INSTANCE_PIXMAP_HEIGHT (p) };
  int pxoffset = 0, pyoffset = 0;
  int absolute_pxoffset = 0, absolute_pyoffset = 0;

  if (dga)
    {	
      xdga.width = dga->width;
      xdga.height = dga->height;
    }
  else if (!redisplay_normalize_glyph_area (&xdb, &xdga))
    return;

  if (absolute)
    {
      POINT point;
      point.x = 0;
      point.y = 0;
      if (ScreenToClient (FRAME_MSWINDOWS_HANDLE (f), &point))
	{
	  absolute_pxoffset = point.x;
	  absolute_pyoffset = point.y;
	}
    }

  /* when doing a bg pixmap do a partial pixmap first so that we
     blt whole pixmaps thereafter */
  xdga.height = min (xdga.height, IMAGE_INSTANCE_PIXMAP_HEIGHT (p) -
		     posmod (db->ypos - absolute_pyoffset,
			     IMAGE_INSTANCE_PIXMAP_HEIGHT (p)));

  while (xdga.height > 0)
    {
      xdga.width = min (min (db->width, IMAGE_INSTANCE_PIXMAP_WIDTH (p)),
			IMAGE_INSTANCE_PIXMAP_WIDTH (p) -
			posmod (db->xpos - absolute_pxoffset,
				IMAGE_INSTANCE_PIXMAP_WIDTH (p)));
      pxoffset = 0;
      while (xdga.width > 0)
	{
	  xdb.xpos = db->xpos + pxoffset;
	  xdb.ypos = db->ypos + pyoffset;
	    /* do we need to offset the pixmap vertically? this is necessary
	       for background pixmaps. */
	  xdga.xoffset = posmod (xdb.xpos - absolute_pxoffset,
				 IMAGE_INSTANCE_PIXMAP_WIDTH (p));
	  xdga.yoffset = posmod (xdb.ypos - absolute_pyoffset,
				 IMAGE_INSTANCE_PIXMAP_HEIGHT (p));
	  /* [[ the width is handled by mswindows_output_pixmap_region ]]
	     #### -- What is the correct meaning of this comment?  There is
	     no mswindows_output_pixmap_region(). --ben*/
	  mswindows_output_dibitmap (f, p, &xdb, &xdga);
	  pxoffset += xdga.width;
	  xdga.width = min ((db->width - pxoffset),
			    IMAGE_INSTANCE_PIXMAP_WIDTH (p));
	}
      pyoffset += xdga.height;
      xdga.height = min ((db->height - pyoffset), 
			 IMAGE_INSTANCE_PIXMAP_HEIGHT (p));
    }
}

/* Output a pixmap at the desired location. 
   DB		normalized display_box.
   DGA		normalized display_glyph_area. */
static void
mswindows_output_pixmap (struct window *w, Lisp_Object image_instance,
			 struct display_box *db,
			 struct display_glyph_area *dga, face_index findex,
			 int UNUSED (cursor_start), int UNUSED (cursor_width),
			 int UNUSED (cursor_height), int bg_pixmap)
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);

  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);

  /* Output the pixmap. Have to do this as many times as is required
   to fill the given area */
  mswindows_update_dc (hdc,
		       WINDOW_FACE_CACHEL_FOREGROUND (w, findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, findex), Qnil);

  if (bg_pixmap)
    mswindows_output_dibitmap_region
      (f, p, db, dga,
       EQ (WINDOW_FACE_CACHEL_BACKGROUND_PLACEMENT (w, findex), Qabsolute));
  else
    mswindows_output_dibitmap (f, p, db, dga);
}

#ifdef HAVE_SCROLLBARS
/*
 * This function paints window's deadbox, a rectangle between window
 * borders and two short edges of both scrollbars.
 *
 * Function checks whether deadbox intersects with the rectangle pointed
 * to by PRC, and paints only the intersection
 */
static void
mswindows_redisplay_deadbox (struct window *w, int x, int y, int width,
			     int height)
{
  RECT rc = { x, y, x + width, y + height };
  int sbh = window_scrollbar_height (w);
  int sbw = window_scrollbar_width (w);
  RECT rect_dead, rect_paint;
  if (sbh == 0 || sbw == 0)
    return;

  if (!NILP (w->scrollbar_on_left_p))
    rect_dead.left = WINDOW_LEFT (w);
  else
    rect_dead.left = WINDOW_TEXT_RIGHT (w);
  rect_dead.right = rect_dead.left + sbw;

  if (!NILP (w->scrollbar_on_top_p))
    rect_dead.top = WINDOW_TOP (w);
  else
    rect_dead.top = WINDOW_TEXT_BOTTOM (w);
  rect_dead.bottom = rect_dead.top + sbh;
      
  if (IntersectRect (&rect_paint, &rect_dead, &rc))
    {
      struct frame *f = XFRAME (WINDOW_FRAME (w));
      FillRect (get_frame_dc (f, 1), &rect_paint,
		(HBRUSH) (COLOR_BTNFACE+1));
    }
}

#endif /* HAVE_SCROLLBARS */

/*****************************************************************************
 mswindows_bevel_area

 Draw a 3d border around the specified area on window W.
 ****************************************************************************/
static void
mswindows_bevel_area (struct window *w, face_index findex, int x, int y, 
		      int width, int height, int thickness,
		      int edges, enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  UINT edge;
  UINT border = 0;

  if (style == EDGE_ETCHED_IN)
    edge = EDGE_ETCHED;
  else if (style == EDGE_ETCHED_OUT)
    edge = EDGE_BUMP;
  else if (style == EDGE_BEVEL_IN)
    {
      if (thickness == 1)
	edge = BDR_SUNKENINNER;
      else
	edge = EDGE_SUNKEN;
    }
  else				/* EDGE_BEVEL_OUT */
    {
      if (thickness == 1)
	edge = BDR_RAISEDINNER;
      else
	edge = EDGE_RAISED;
    }

  if (edges & EDGE_TOP)
    border |= BF_TOP;
  if (edges & EDGE_LEFT)
    border |= BF_LEFT;
  if (edges & EDGE_BOTTOM)
    border |= BF_BOTTOM;
  if (edges & EDGE_RIGHT)
    border |= BF_RIGHT;

  {
    RECT rect = { x, y, x + width, y + height };
    Lisp_Object color = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
    HDC hdc = get_frame_dc (f, 1);

    mswindows_update_dc (hdc, Qnil, color, Qnil);
    DrawEdge (hdc, &rect, edge, border);
  }
}


/*****************************************************************************
 Display methods
*****************************************************************************/

/*****************************************************************************
 mswindows_divider_height

 Return the height of the horizontal divider.
 ****************************************************************************/
static int
mswindows_divider_height (void)
{
  return 1;   /* XXX Copied from redisplay-X.c. What is this? */
}

/*****************************************************************************
 mswindows_eol_cursor_width

 Return the width of the end-of-line cursor.
 ****************************************************************************/
static int
mswindows_eol_cursor_width (void)
{
  return MSWINDOWS_EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 mswindows_frame_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void
mswindows_frame_output_begin (struct frame *UNUSED (f))
{
}

/*****************************************************************************
 mswindows_frame_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void
mswindows_frame_output_end (struct frame *
#ifdef DEFER_WINDOW_POS
			    f
#else
			    UNUSED (f)
#endif
			    )
{
#ifdef DEFER_WINDOW_POS
  HDWP hdwp = FRAME_MSWINDOWS_DATA (f)->hdwp;

  if (hdwp != 0)
    {
      EndDeferWindowPos (hdwp);
      FRAME_MSWINDOWS_DATA (f)->hdwp = 0;
    }
#endif
  GdiFlush();
}

/* Printer version is more lightweight. */
static void
msprinter_frame_output_end (struct frame *UNUSED (f))
{
  GdiFlush();
}

static int
mswindows_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  HDC hdc = get_frame_dc (f, 1);
  RECT rc;

  GetClientRect (FRAME_MSWINDOWS_HANDLE (f), &rc);
  InvertRect (hdc, &rc);
  GdiFlush ();
  Sleep (25);
  InvertRect (hdc, &rc);

  return 1;
}

static void
mswindows_ring_bell (struct device *UNUSED (d), int UNUSED (volume),
		     int UNUSED (pitch), int UNUSED (duration))
{
  /* Beep does not work at all, anyways! -kkm */
  MessageBeep (MB_OK);
}

/*****************************************************************************
 mswindows_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 Ripped off with minimal thought from the corresponding X routine.
 ****************************************************************************/
static void
mswindows_output_display_block (struct window *w, struct display_line *dl,
				int block, int start, int end,
				int start_pixpos, int cursor_start,
				int cursor_width, int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  Ibyte *buffer, *bufp;
  Lisp_Object window;

  struct display_block *db = Dynarr_atp (dl->display_blocks, block);
  rune_dynarr *rba = db->runes;
  struct rune *rb;

  int elt = start;
  face_index findex;
  int xpos, width;
  Lisp_Object charset = Qunbound; /* Qnil is a valid charset when
				     MULE is not defined */
  window = wrap_window (w);
  rb = Dynarr_atp (rba, start);

  if (!rb)
    {
      /* Nothing to do so don't do anything. */
      return;
    }

  findex = rb->findex;
  xpos = rb->xpos;
  width = 0;
  if (rb->type == RUNE_CHAR)
    /* @@#### fix me */
    charset = buffer_ichar_charset_obsolete_me_baby (WINDOW_XBUFFER (w),
						     rb->object.chr.ch);

  if (end < 0)
    end = Dynarr_length (rba);

  buffer = bufp = alloca_ibytes (end * MAX_ICHAR_LEN);

  while (elt < end)
    {
      rb = Dynarr_atp (rba, elt);

      if (rb->findex == findex && rb->type == RUNE_CHAR
	  && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
	  /* @@#### fix me */
	  && EQ (charset,
		 buffer_ichar_charset_obsolete_me_baby (WINDOW_XBUFFER (w),
							rb->object.chr.ch)))
	{
          bufp += set_itext_ichar (bufp, rb->object.chr.ch);
	  width += rb->width;
	  elt++;
	}
      else
	{
	  if (bufp - buffer)
	    {
	      mswindows_output_string (w, dl, buffer, bufp - buffer, xpos, 0,
                                       start_pixpos, width, findex,
                                       0, 0, 0, 0);
	      xpos = rb->xpos;
	      width = 0;
              bufp = buffer;
	    }
	  width = 0;

	  if (rb->type == RUNE_CHAR)
	    {
	      findex = rb->findex;
	      xpos = rb->xpos;
	      /* @@#### fix me */
	      charset =
		buffer_ichar_charset_obsolete_me_baby (WINDOW_XBUFFER (w),
						       rb->object.chr.ch);

	      if (rb->cursor_type == CURSOR_ON)
		{
		  if (rb->object.chr.ch == '\n')
		    {
		      mswindows_output_cursor (w, dl, xpos, cursor_width,
					       findex, 0, 0);
		    }
		  else
		    {
		      mswindows_output_cursor (w, dl, xpos, cursor_width,
					       findex, rb->object.chr.ch, 0);
		    }

		  xpos += rb->width;
		  elt++;
		}
	      else if (rb->object.chr.ch == '\n')
		{
		  /* Clear in case a cursor was formerly here. */
		  redisplay_clear_region (window, findex, xpos, 
					  DISPLAY_LINE_YPOS (dl),
					  rb->width, DISPLAY_LINE_HEIGHT (dl));
		  elt++;
		}
	    }
	  else if (rb->type == RUNE_BLANK || rb->type == RUNE_HLINE)
	    {
	      if (rb->type == RUNE_BLANK)
		mswindows_output_blank (w, dl, rb, start_pixpos);
	      else
		{
		  /* #### Our flagging of when we need to redraw the
                     modeline shadows sucks.  Since RUNE_HLINE is only used
                     by the modeline at the moment it is a good bet
                     that if it gets redrawn then we should also
                     redraw the shadows.  This won't be true forever.
                     We borrow the shadow_thickness_changed flag for
                     now. */
		  w->shadow_thickness_changed = 1;
		  mswindows_output_hline (w, dl, rb);
		}

	      if (rb->cursor_type == CURSOR_ON)
                {
                  mswindows_output_cursor (w, dl, xpos, cursor_width,
                                           rb->findex, 0, 0);
                }

	      elt++;
	      if (elt < end)
		{
		  rb = Dynarr_atp (rba, elt);

		  findex = rb->findex;
		  xpos = rb->xpos;
		}
	    }
	  else if (rb->type == RUNE_DGLYPH)
	    {
	      Lisp_Object instance;
	      struct display_box dbox;
	      struct display_glyph_area dga;

	      redisplay_calculate_display_boxes (dl, rb->xpos,
                                                 rb->object.dglyph.xoffset,
						 rb->object.dglyph.yoffset,
                                                 start_pixpos, rb->width,
                                                 &dbox, &dga);

	      window = wrap_window (w);
	      instance = glyph_image_instance (rb->object.dglyph.glyph,
					       window, ERROR_ME_DEBUG_WARN, 1);
	      findex = rb->findex;

	      if (IMAGE_INSTANCEP (instance))
		{
		  switch (XIMAGE_INSTANCE_TYPE (instance))
		    {
		    case IMAGE_MONO_PIXMAP:
		    case IMAGE_COLOR_PIXMAP:
		      redisplay_output_pixmap (w, instance, &dbox, &dga, findex,
					       cursor_start, cursor_width,
					       cursor_height, 0);
		      if (rb->cursor_type == CURSOR_ON)
			mswindows_output_cursor (w, dl, xpos, cursor_width,
						 findex, 0, 1);
		      break;
		      
		    case IMAGE_WIDGET:
		      if (EQ (XIMAGE_INSTANCE_WIDGET_TYPE (instance),
			      Qlayout))
			{
			  redisplay_output_layout (window, instance, &dbox, &dga, findex,
						   cursor_start, cursor_width,
						   cursor_height);
			  if (rb->cursor_type == CURSOR_ON)
			    mswindows_output_cursor (w, dl, xpos, cursor_width,
						     findex, 0, 1);
			  break;
			}
		    case IMAGE_SUBWINDOW:
		      redisplay_output_subwindow (w, instance, &dbox, &dga, findex,
						  cursor_start, cursor_width,
						  cursor_height);
		      if (rb->cursor_type == CURSOR_ON)
			mswindows_output_cursor (w, dl, xpos, cursor_width,
						 findex, 0, 1);
		      break;
		      
		    case IMAGE_NOTHING:
		      /* nothing is as nothing does */
		      break;

		    case IMAGE_TEXT:
		    case IMAGE_POINTER:
		    default:
		      ABORT ();
		    }
		  IMAGE_INSTANCE_OPTIMIZE_OUTPUT 
		    (XIMAGE_INSTANCE (instance)) = 0;
		}
	      xpos += rb->width;
	      elt++;
	    }
	  else
	    ABORT ();
	}
    }

  if (bufp - buffer)
    mswindows_output_string (w, dl, buffer, bufp - buffer, xpos, 0,
                             start_pixpos, width, findex, 0, 0, 0, 0);

  if (dl->modeline
      && !EQ (Qzero, w->modeline_shadow_thickness)
      && (f->clear
	  || f->windows_structure_changed
	  || w->shadow_thickness_changed))
    bevel_modeline (w, dl);
}


/*****************************************************************************
 mswindows_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void
mswindows_output_vertical_divider (struct window *w, int UNUSED (clear_unused))
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);
  RECT rect;
  int spacing = XFIXNUM (w->vertical_divider_spacing);
  int shadow = XFIXNUM (w->vertical_divider_shadow_thickness);
  int abs_shadow = abs (shadow);
  int line_width = XFIXNUM (w->vertical_divider_line_width);
  int div_left = WINDOW_RIGHT (w) - window_divider_width (w);
  int ytop = WINDOW_TOP (w);
  int ybot = WINDOW_BOTTOM (w);

  /* Clear left and right spacing areas */
  if (spacing)
    {
      rect.top = ytop;
      rect.bottom = ybot;
      mswindows_update_dc (hdc, Qnil,
		   WINDOW_FACE_CACHEL_BACKGROUND (w, DEFAULT_INDEX), Qnil);
      rect.right = WINDOW_RIGHT (w);
      rect.left = rect.right - spacing;
      ExtTextOutW (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
      rect.left = div_left;
      rect.right = div_left + spacing;
      ExtTextOutW (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }
  
  /* Clear divider face */
  rect.top = ytop + abs_shadow;
  rect.bottom = ybot - abs_shadow;
  rect.left = div_left + spacing + abs_shadow;
  rect.right = rect.left + line_width;
  if (rect.left < rect.right)
    {
      face_index div_face
	= get_builtin_face_cache_index (w, Vvertical_divider_face);
      mswindows_update_dc (hdc, Qnil,
		   WINDOW_FACE_CACHEL_BACKGROUND (w, div_face), Qnil);
      ExtTextOutW (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }

  /* Draw a shadow around the divider */
  if (shadow != 0)
    {
      /* #### This will be fixed to support arbitrary thickness */
      InflateRect (&rect, abs_shadow, abs_shadow);
      DrawEdge (hdc, &rect,
		shadow > 0 ? EDGE_RAISED : EDGE_SUNKEN, BF_RECT);
    }
}

/****************************************************************************
 mswindows_text_width

 Given a string and a face, return the string's length in pixels when
 displayed in the font associated with the face.
 ****************************************************************************/
static int
mswindows_text_width (struct frame *f, struct face_cachel *cachel,
		      const Ibyte *str, Bytecount len)
{
  HDC hdc = get_frame_dc (f, 0);
  int width_so_far = 0;
  textual_run *runs;
  Extbyte *text_storage;
  Elemcount nruns;
  Bytecount text_storage_len;
  int i;

  TO_EXTERNAL_FORMAT (DATA, (str, len),
                      ALLOCA, (text_storage, text_storage_len),
                      Qmswindows_unicode);

  runs = alloca_array (textual_run, text_storage_len >> 1);

  nruns = separate_textual_runs ((WCHAR *) text_storage, runs, str, len);

  for (i = 0; i < nruns; i++)
    width_so_far += mswindows_text_width_single_run (hdc, cachel, runs + i);

  return width_so_far;
}


/****************************************************************************
 mswindows_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
mswindows_clear_region (Lisp_Object USED_IF_SCROLLBARS (locale),
			struct frame *f, 
			face_index UNUSED (findex), int x, int y,
			int width, int height, Lisp_Object fcolor,
			Lisp_Object bcolor,
			Lisp_Object background_pixmap,
			Lisp_Object background_placement)
{
  RECT rect = { x, y, x+width, y+height };
  HDC hdc = get_frame_dc (f, 1);

  if (!NILP (background_pixmap))
    {
      struct display_box db = { x, y, width, height };
      mswindows_update_dc (hdc,
			   fcolor, bcolor, background_pixmap);
      mswindows_output_dibitmap_region 
	(f, XIMAGE_INSTANCE (background_pixmap), &db, 0,
	 EQ (background_placement, Qabsolute));
    }
  else
    {
      mswindows_update_dc (hdc, Qnil, fcolor, Qnil);
      ExtTextOutW (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }

#ifdef HAVE_SCROLLBARS
  if (WINDOWP (locale))
    mswindows_redisplay_deadbox (XWINDOW (locale), x, y, width, height);
#endif
}

/* #### Implement me! */
static void
mswindows_clear_frame (struct frame *UNUSED (f))
{
  GdiFlush ();
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_redisplay_mswindows (void)
{
  /* redisplay methods - display*/
  CONSOLE_HAS_METHOD (mswindows, text_width);
  CONSOLE_HAS_METHOD (mswindows, output_display_block);
  CONSOLE_HAS_METHOD (mswindows, divider_height);
  CONSOLE_HAS_METHOD (mswindows, eol_cursor_width);
  CONSOLE_HAS_METHOD (mswindows, output_vertical_divider);
  CONSOLE_HAS_METHOD (mswindows, clear_region);
  CONSOLE_HAS_METHOD (mswindows, clear_frame);
  CONSOLE_HAS_METHOD (mswindows, frame_output_begin);
  CONSOLE_HAS_METHOD (mswindows, frame_output_end);
  CONSOLE_HAS_METHOD (mswindows, flash);
  CONSOLE_HAS_METHOD (mswindows, ring_bell);
  CONSOLE_HAS_METHOD (mswindows, bevel_area);
  CONSOLE_HAS_METHOD (mswindows, output_string);
  CONSOLE_HAS_METHOD (mswindows, output_pixmap);
#ifdef HAVE_SCROLLBARS
  CONSOLE_HAS_METHOD (mswindows, redisplay_deadbox);
#endif

  /* redisplay methods - printer */
  CONSOLE_HAS_METHOD (msprinter, frame_output_end);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, text_width);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_display_block);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, divider_height);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, eol_cursor_width);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_vertical_divider);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, clear_region);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, clear_frame);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, frame_output_begin);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, bevel_area);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_string);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_pixmap);
}
