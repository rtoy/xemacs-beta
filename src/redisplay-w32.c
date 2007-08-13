/* win32 output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Authorship:

   Chuck Thompson
   Lots of work done by Ben Wing for Mule
   Partially rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "console-w32.h"
#include "objects-w32.h"

#include "buffer.h"
#include "debug.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"	/* XXX FIXME: Should be glyphs-w32 when we make one */
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "windows.h"

/* W32_DIVIDER_LINE_WIDTH is the width of the line drawn in the gutter.
   W32_DIVIDER_SPACING is the amount of blank space on each side of the line.
   W32_DIVIDER_WIDTH = W32_DIVIDER_LINE_WIDTH + 2*W32_DIVIDER_SPACING
*/
#define W32_DIVIDER_LINE_WIDTH	5
#define W32_DIVIDER_SPACING	0
#define W32_DIVIDER_WIDTH	(W32_DIVIDER_LINE_WIDTH + 2 * W32_DIVIDER_SPACING)

#define W32_EOL_CURSOR_WIDTH	5

/*
 * Random forward delarations
 */
static void w32_clear_region (Lisp_Object locale, face_index findex,
			      int x, int y, int width, int height);
static void w32_output_vertical_divider (struct window *w, int clear);
static void w32_redraw_exposed_windows (Lisp_Object window, int x,
					int y, int width, int height);



typedef struct textual_run
{
  Lisp_Object charset;
  unsigned char *ptr;
  int len;
  int dimension;
} textual_run;

/* Separate out the text in DYN into a series of textual runs of a
   particular charset.  Also convert the characters as necessary into
   the format needed by XDrawImageString(), XDrawImageString16(), et
   al.  (This means converting to one or two byte format, possibly
   tweaking the high bits, and possibly running a CCL program.) You
   must pre-allocate the space used and pass it in. (This is done so
   you can alloca() the space.)  You need to allocate (2 * len) bytes
   of TEXT_STORAGE and (len * sizeof (textual_run)) bytes of
   RUN_STORAGE, where LEN is the length of the dynarr.

   Returns the number of runs actually used. */

static int
separate_textual_runs (unsigned char *text_storage,
		       textual_run *run_storage,
		       CONST Emchar *str, Charcount len)
{
  Lisp_Object prev_charset = Qunbound; /* not Qnil because that is a
					  possible valid charset when
					  MULE is not defined */
  int runs_so_far = 0;
  int i;
#ifdef MULE
  struct ccl_program char_converter;
  int need_ccl_conversion = 0;
#endif

  for (i = 0; i < len; i++)
    {
      Emchar ch = str[i];
      Lisp_Object charset;
      int byte1, byte2;
      int dimension;
      int graphic;

      BREAKUP_CHAR (ch, charset, byte1, byte2);
      dimension = XCHARSET_DIMENSION (charset);
      graphic   = XCHARSET_GRAPHIC   (charset);

      if (!EQ (charset, prev_charset))
	{
	  run_storage[runs_so_far].ptr       = text_storage;
	  run_storage[runs_so_far].charset   = charset;
	  run_storage[runs_so_far].dimension = dimension;

	  if (runs_so_far)
	    {
	      run_storage[runs_so_far - 1].len =
		text_storage - run_storage[runs_so_far - 1].ptr;
	      if (run_storage[runs_so_far - 1].dimension == 2)
		run_storage[runs_so_far - 1].len >>= 1;
	    }
	  runs_so_far++;
	  prev_charset = charset;
#ifdef MULE
	  {
	    Lisp_Object ccl_prog = XCHARSET_CCL_PROGRAM (charset);
	    need_ccl_conversion = !NILP (ccl_prog);
	    if (need_ccl_conversion)
	      set_ccl_program (&char_converter, ccl_prog, 0, 0, 0);
	  }
#endif
	}

      if (graphic == 0)
	{
	  byte1 &= 0x7F;
	  byte2 &= 0x7F;
	}
      else if (graphic == 1)
	{
	  byte1 |= 0x80;
	  byte2 |= 0x80;
	}
#ifdef MULE
      if (need_ccl_conversion)
	{
	  char_converter.reg[0] = byte1;
	  char_converter.reg[1] = byte2;
	  char_converter.ic = 0; /* start at beginning each time */
	  ccl_driver (&char_converter, 0, 0, 0, 0);
	  byte1 = char_converter.reg[0];
	  byte2 = char_converter.reg[1];
	}
#endif
      *text_storage++ = (unsigned char) byte1;
      if (dimension == 2)
	*text_storage++ = (unsigned char) byte2;
    }

  if (runs_so_far)
    {
      run_storage[runs_so_far - 1].len =
	text_storage - run_storage[runs_so_far - 1].ptr;
      if (run_storage[runs_so_far - 1].dimension == 2)
	run_storage[runs_so_far - 1].len >>= 1;
    }

  return runs_so_far;
}


static int
w32_text_width_single_run (HDC hdc, struct face_cachel *cachel,
			   textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  struct Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  SIZE size;

#if 0	/* XXX HACK: w32_text_width is broken and will pass in a NULL hdc */
  if (!fi->proportional_p)
#else
  if (!fi->proportional_p || !hdc)
#endif
    return (fi->width * run->len);
  else
    {
      assert(run->dimension == 1);	/* XXX FIXME! */
      GetTextExtentPoint32(hdc, run->ptr, run->len, &size);
      return(size.cx);
    }
}


/*****************************************************************************
 w32_update_gc

 Given a number of parameters munge the GC so it has those properties.
 ****************************************************************************/
static void
w32_update_gc (HDC hdc, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
	       Lisp_Object bg_pmap, Lisp_Object lwidth)
{
  if (!NILP (font))
    SelectObject(hdc, (XFONT_INSTANCE (font))->data);

  /* evil kludge! - XXX do we need this? */
  if (!NILP (fg) && !COLOR_INSTANCEP (fg))
    {
      fprintf (stderr, "Help! w32_update_gc got a bogus fg value! fg = ");
      debug_print (fg);
      fg = Qnil;
    }

  if (!NILP (fg))
    SetTextColor (hdc, COLOR_INSTANCE_W32_COLOR (XCOLOR_INSTANCE (fg)));

  if (!NILP (bg))
    SetBkColor (hdc, COLOR_INSTANCE_W32_COLOR (XCOLOR_INSTANCE (bg)));

#if 0	/* XXX Implement me */
  /* I expect that the Lisp_Image_Instance's data will point to a brush */
  if (IMAGE_INSTANCEP (bg_pmap)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    {
      if (XIMAGE_INSTANCE_PIXMAP_DEPTH (bg_pmap) == 0)
	{
	  gcv.fill_style = FillOpaqueStippled;
	  gcv.stipple = XIMAGE_INSTANCE_X_PIXMAP (bg_pmap);
	  mask |= (GCStipple | GCFillStyle);
	}
      else
	{
	  gcv.fill_style = FillTiled;
	  gcv.tile = XIMAGE_INSTANCE_X_PIXMAP (bg_pmap);
	  mask |= (GCTile | GCFillStyle);
	}
    }
#endif

#if 0	/* XXX FIXME */
  if (!NILP (lwidth))
    {
      gcv.line_width = XINT (lwidth);
      mask |= GCLineWidth;
    }
#endif
}


/*****************************************************************************
 w32_output_hline

 Output a horizontal line in the foreground of its face.
 ****************************************************************************/
static void
w32_output_hline (struct window *w, struct display_line *dl, struct rune *rb)
{ /* XXX Implement me */
}


/*****************************************************************************
 w32_output_blank

 Output a blank by clearing the area it covers in the background color
 of its face.
 ****************************************************************************/
static void
w32_output_blank (struct window *w, struct display_line *dl, struct rune *rb)
{
  struct frame *f = XFRAME (w->frame);
  RECT rect = { rb->xpos, dl->ypos-dl->ascent,
		rb->xpos+rb->width, dl->ypos+dl->descent-dl->clip };
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, rb->findex);

  Lisp_Object bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, rb->findex);
  
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  FillRect (FRAME_W32_DC (f), &rect,
	    COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (cachel->background)));
}


/*****************************************************************************
 w32_output_cursor

 Draw a normal or end-of-line cursor. The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
w32_output_cursor (struct window *w, struct display_line *dl, int xpos,
		   int width, struct rune *rb)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  struct face_cachel *cachel;
  Lisp_Object font;
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  HBRUSH brush;
  HDC hdc = FRAME_W32_DC (f);
  int real_char_p = (rb->type == RUNE_CHAR && rb->object.chr.ch != '\n');
  RECT rect = { xpos,
		dl->ypos - dl->ascent,
		xpos + width,
		dl->ypos + dl->descent - dl->clip};

#if 0	/* XXX FIXME: Whar about the bar_cursor? */
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));
#endif

  if (real_char_p)
    {
      /* Use the font from the underlying character */
      cachel = WINDOW_FACE_CACHEL (w, rb->findex);

      /* XXX MULE: Need to know the charset! */
      font = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
    }

  /* Clear the area */
  if (focus)
    cachel = WINDOW_FACE_CACHEL (w,
		get_builtin_face_cache_index (w, Vtext_cursor_face));
  else if (!real_char_p)
    cachel = WINDOW_FACE_CACHEL (w, rb->findex);

  brush = COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (cachel->background));
  FillRect (hdc, &rect, brush);

  if (real_char_p)
    {
      /* XXX FIXME: Need to clip if dl->clip!=0. How rare is this case? */
      /* Output the underlying character */
      w32_update_gc (hdc, font, cachel->foreground,
		     cachel->background, Qnil, Qnil);
      TextOut(hdc, xpos, dl->ypos, (char*) &rb->object.chr.ch, 1);
    }

  if (!focus)
    {
      /* Draw hollow rectangle in cursor's background color */
      cachel = WINDOW_FACE_CACHEL (w,
		get_builtin_face_cache_index (w, Vtext_cursor_face));
      brush = COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (cachel->background));
      FrameRect (hdc, &rect, brush);
    }
}


/*****************************************************************************
 w32_output_string

 Given a string and a starting position, output that string in the
 given face.
 Correctly handles multiple charsets in the string.

 The meaning of the parameters is something like this:

 W		Window that the text is to be displayed in.
 DL		Display line that this text is on.  The values in the
 		structure are used to determine the vertical position and
		clipping range of the text.
 BUF		Dynamic array of Emchars specifying what is actually to be
		drawn.
 XPOS		X position in pixels where the text should start being drawn.
 XOFFSET	Number of pixels to be chopped off the left side of the
 		text.  The effect is as if the text were shifted to the
		left this many pixels and clipped at XPOS.
 CLIP_START	Clip everything left of this X position.
 WIDTH		Clip everything right of XPOS + WIDTH.
 FINDEX		Index for the face cache element describing how to display
 		the text.
 ****************************************************************************/
void
w32_output_string (struct window *w, struct display_line *dl,
		   Emchar_dynarr *buf, int xpos, int xoffset, int clip_start,
		   int width, face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object window = Qnil;
  HDC hdc;
  int clip_end;
  Lisp_Object bg_pmap;
  int len = Dynarr_length (buf);
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  textual_run *runs = alloca_array (textual_run, len);
  int nruns;
  int i;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  XSETWINDOW (window, w);
  hdc = FRAME_W32_DC(f);

#if 0	/* XXX: FIXME? */
  /* We can't work out the width before we've set the font in the DC */
  if (width < 0)
    width = w32_text_width (cachel, Dynarr_atp (buf, 0), Dynarr_length (buf));
#else
  assert(width>=0);
#endif

  /* Regularize the variables passed in. */
  if (clip_start < xpos)
    clip_start = xpos;
  clip_end = xpos + width;
  if (clip_start >= clip_end)
    /* It's all clipped out. */
    return;

  xpos -= xoffset;

  nruns = separate_textual_runs (text_storage, runs, Dynarr_atp (buf, 0),
				 Dynarr_length (buf));

  bg_pmap = cachel->background_pixmap;
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      struct Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;
      int need_clipping;
      RECT rect = { clip_start, dl->ypos - dl->ascent,
		    clip_end, dl->ypos + dl->descent - dl->clip };
      HRGN region;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      w32_update_gc (hdc, font, cachel->foreground,
		     cachel->background, Qnil, Qnil);

      this_width = w32_text_width_single_run (hdc, cachel, runs + i);
      need_clipping = (dl->clip || clip_start > xpos ||
		       clip_end < xpos + this_width);

      if (need_clipping)
	{
	  region = CreateRectRgn (rect.left, rect.top,
				  rect.right, rect.bottom);
	  SelectClipRgn (hdc, region);
	}

      /* TextOut only clears the area equal to the height of
	 the given font.  It is possible that a font is being displayed
	 on a line taller than it is, so this would cause us to fail to
	 clear some areas. */
      if (fi->ascent < dl->ascent || fi->descent < dl->descent-dl->clip)
	FillRect (hdc, &rect,
		  COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (cachel->background)));

      assert (runs[i].dimension == 1);	/* XXX FIXME */
      TextOut(hdc, xpos, dl->ypos, (char *) runs[i].ptr, runs[i].len);

      /* XXX FIXME? X does underline/strikethrough here
	 we will do it as part of face's font */

      if (need_clipping)
	{
	  SelectClipRgn (hdc, NULL);
	  DeleteObject (region);
	}

      xpos += this_width;
    }
}

/*****************************************************************************
 w32_redraw_exposed_window

 Given a bounding box for an area that needs to be redrawn, determine
 what parts of what lines are contained within and re-output their
 contents.
 Copied from redisplay-x.c
 ****************************************************************************/
static void
w32_redraw_exposed_window (struct window *w, int x, int y, int width,
			   int height)
{
  struct frame *f = XFRAME (w->frame);
  int line;
  int start_x, start_y, end_x, end_y;
  int orig_windows_structure_changed;

  display_line_dynarr *cdla = window_display_lines (w, CURRENT_DISP);

  if (!NILP (w->vchild))
    {
      w32_redraw_exposed_windows (w->vchild, x, y, width, height);
      return;
    }
  else if (!NILP (w->hchild))
    {
      w32_redraw_exposed_windows (w->hchild, x, y, width, height);
      return;
    }

  /* If the window doesn't intersect the exposed region, we're done here. */
  if (x >= WINDOW_RIGHT (w) || (x + width) <= WINDOW_LEFT (w)
      || y >= WINDOW_BOTTOM (w) || (y + height) <= WINDOW_TOP (w))
    {
      return;
    }
  else
    {
      start_x = max (WINDOW_LEFT (w), x);
      end_x = min (WINDOW_RIGHT (w), (x + width));
      start_y = max (WINDOW_TOP (w), y);
      end_y = min (WINDOW_BOTTOM (w), y + height);

      /* We do this to make sure that the 3D modelines get redrawn if
         they are in the exposed region. */
      orig_windows_structure_changed = f->windows_structure_changed;
      f->windows_structure_changed = 1;
    }

  if (window_needs_vertical_divider (w))
    {
      w32_output_vertical_divider (w, 0);
    }

  for (line = 0; line < Dynarr_length (cdla); line++)
    {
      struct display_line *cdl = Dynarr_atp (cdla, line);
      int top_y = cdl->ypos - cdl->ascent;
      int bottom_y = cdl->ypos + cdl->descent;

      if (bottom_y >= start_y)
	{
	  if (top_y > end_y)
	    {
	      if (line == 0)
		continue;
	      else
		break;
	    }
	  else
	    {
	      output_display_line (w, 0, cdla, line, start_x, end_x);
	    }
	}
    }

  f->windows_structure_changed = orig_windows_structure_changed;

  /* If there have never been any face cache_elements created, then this
     expose event doesn't actually have anything to do. */
  if (Dynarr_largest (w->face_cachels))
    redisplay_clear_bottom_of_window (w, cdla, start_y, end_y);
}

/*****************************************************************************
 w32_redraw_exposed_windows

 For each window beneath the given window in the window hierarchy,
 ensure that it is redrawn if necessary after an Expose event.
 ****************************************************************************/
static void
w32_redraw_exposed_windows (Lisp_Object window, int x, int y, int width,
			    int height)
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    w32_redraw_exposed_window (XWINDOW (window), x, y, width, height);
}

/*****************************************************************************
 w32_redraw_exposed_area

 For each window on the given frame, ensure that any area in the
 Exposed area is redrawn.
 ****************************************************************************/
void
w32_redraw_exposed_area (struct frame *f, int x, int y, int width, int height)
{
  /* If any window on the frame has had its face cache reset then the
     redisplay structures are effectively invalid.  If we attempt to
     use them we'll blow up.  We mark the frame as changed to ensure
     that redisplay will do a full update.  This probably isn't
     necessary but it can't hurt. */

  if (!f->window_face_cache_reset)
    w32_redraw_exposed_windows (f->root_window, x, y, width, height);
  else
    MARK_FRAME_CHANGED (f);
}


/*****************************************************************************
 w32_bevel_modeline

 Draw a 3d border around the modeline on window W.
 ****************************************************************************/
static void
w32_bevel_modeline (struct window *w, struct display_line *dl)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object color;
  RECT rect = {	WINDOW_MODELINE_LEFT (w), 
		dl->ypos - dl->ascent,
		WINDOW_MODELINE_RIGHT (w),
		dl->ypos + dl->descent};


  color = WINDOW_FACE_CACHEL_BACKGROUND (w, MODELINE_INDEX);
  w32_update_gc(FRAME_W32_DC(f), Qnil, Qnil, color, Qnil, Qnil);

#if 0	/* XXX Eh? */
  if (XINT (w->modeline_shadow_thickness) < 0)
    {
      GC temp;

      temp = top_shadow_gc;
      top_shadow_gc = bottom_shadow_gc;
      bottom_shadow_gc = temp;
    }
#endif

  DrawEdge (FRAME_W32_DC(f), &rect, BDR_RAISEDINNER, BF_RECT); 
}


/*****************************************************************************
 #### Display methods
/*****************************************************************************

/*****************************************************************************
 w32_divider_width

 Return the width of the vertical divider.
 ****************************************************************************/
static int
w32_divider_width (void)
{
  return W32_DIVIDER_WIDTH;
}

/*****************************************************************************
 w32_divider_height

 Return the height of the horizontal divider.
 ****************************************************************************/
static int
w32_divider_height (void)
{
  return 1;   /* XXX Copied from redisplay-X.c. What is this? */
}

/*****************************************************************************
 w32_eol_cursor_width

 Return the width of the end-of-line cursor.
 ****************************************************************************/
static int
w32_eol_cursor_width (void)
{
  return W32_EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 w32_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void
w32_output_begin (struct device *d)
{
}

/*****************************************************************************
 w32_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void
w32_output_end (struct device *d)
{
}

static int
w32_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);

  /* XXX FIXME: Do something more visible here, maybe involving a timer */
  FlashWindow (FRAME_W32_HANDLE (f), TRUE);
  FlashWindow (FRAME_W32_HANDLE (f), FALSE);
}

static void
w32_ring_bell (struct device *d, int volume, int pitch, int duration)
{
  /* XXX FIXME: I'm guessing pitch=Hz and duration is milliseconds */

  if ((pitch|duration) == -1)	/* Pitch and/or duration may be bogus */
    MessageBeep(-1);		/* Default system sound via speaker */
  else
    Beep(pitch, duration);
}


/*****************************************************************************
 w32_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 Ripped off with mininmal thought from the corresponding X routine.
 ****************************************************************************/
static void
w32_output_display_block (struct window *w, struct display_line *dl, int block,
			  int start, int end, int start_pixpos, int cursor_start,
			  int cursor_width, int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  Emchar_dynarr *buf = Dynarr_new (Emchar);
  Lisp_Object window;

  struct display_block *db = Dynarr_atp (dl->display_blocks, block);
  rune_dynarr *rba = db->runes;
  struct rune *rb;

  int elt = start;
  face_index findex;
  int xpos, width;
  Lisp_Object charset = Qunbound; /* Qnil is a valid charset when
				     MULE is not defined */
  XSETWINDOW (window, w);
  rb = Dynarr_atp (rba, start);

  if (!rb)
    {
      /* Nothing to do so don't do anything. */
      return;
    }
  else
    {
      findex = rb->findex;
      xpos = rb->xpos;
      width = 0;
      if (rb->type == RUNE_CHAR)
	charset = CHAR_CHARSET (rb->object.chr.ch);
    }

  if (end < 0)
    end = Dynarr_length (rba);
  Dynarr_reset (buf);

  while (elt < end)
    {
      rb = Dynarr_atp (rba, elt);

      if (rb->findex == findex && rb->type == RUNE_CHAR
	  && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
	  && EQ (charset, CHAR_CHARSET (rb->object.chr.ch)))
	{
	  Dynarr_add (buf, rb->object.chr.ch);
	  width += rb->width;
	  elt++;
	}
      else
	{
	  if (Dynarr_length (buf))
	    {
	      w32_output_string (w, dl, buf, xpos, 0, start_pixpos, width,
				 findex);
	      xpos = rb->xpos;
	      width = 0;
	    }
	  Dynarr_reset (buf);
	  width = 0;

	  if (rb->type == RUNE_CHAR)
	    {
	      findex = rb->findex;
	      xpos = rb->xpos;
	      charset = CHAR_CHARSET (rb->object.chr.ch);

	      if (rb->cursor_type == CURSOR_ON)
		{
		  if (rb->object.chr.ch == '\n')
		    {
		      w32_output_cursor (w, dl, xpos, cursor_width, rb);
		    }
		  else
		    {
		      Dynarr_add (buf, rb->object.chr.ch);
#if 0
		      w32_output_string (w, dl, buf, xpos, 0, start_pixpos,
					 rb->width, findex, 1,
					 cursor_start, cursor_width,
				         cursor_height);
#else
		      w32_output_cursor (w, dl, xpos, cursor_width, rb);
#endif
		      Dynarr_reset (buf);
		    }

		  xpos += rb->width;
		  elt++;
		}
	      else if (rb->object.chr.ch == '\n')
		{
		  /* Clear in case a cursor was formerly here. */
		  int height = dl->ascent + dl->descent - dl->clip;

		  w32_clear_region (window, findex, xpos, dl->ypos - dl->ascent,
				    rb->width, height);
		  elt++;
		}
	    }
	  else if (rb->type == RUNE_BLANK || rb->type == RUNE_HLINE)
	    {
	      if (rb->type == RUNE_BLANK)
		w32_output_blank (w, dl, rb);
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
		  w32_output_hline (w, dl, rb);
		}

	      if (rb->cursor_type == CURSOR_ON)
		w32_output_cursor (w, dl, xpos, cursor_width, rb);

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

	      XSETWINDOW (window, w);
	      instance = glyph_image_instance (rb->object.dglyph.glyph,
					       window, ERROR_ME_NOT, 1);
	      findex = rb->findex;

	      if (IMAGE_INSTANCEP (instance))
		switch (XIMAGE_INSTANCE_TYPE (instance))
		  {
		  case IMAGE_TEXT:
		    {
		      /* #### This is way losing.  See the comment in
			 add_glyph_rune(). */
		      Lisp_Object string =
			XIMAGE_INSTANCE_TEXT_STRING (instance);
		      convert_bufbyte_string_into_emchar_dynarr
			(XSTRING_DATA (string), XSTRING_LENGTH (string), buf);

		      if (rb->cursor_type == CURSOR_ON)
			w32_output_cursor (w, dl, xpos, cursor_width, rb);
		      else
			w32_output_string (w, dl, buf, xpos,
					   rb->object.dglyph.xoffset,
					   start_pixpos, -1, findex);
		      Dynarr_reset (buf);
		    }
		    break;

		  case IMAGE_MONO_PIXMAP:
		  case IMAGE_COLOR_PIXMAP:
#if 0
		    w32_output_pixmap (w, dl, instance, xpos,
				     rb->object.dglyph.xoffset, start_pixpos,
				     rb->width, findex, cursor_start,
				     cursor_width, cursor_height);
#endif
		    break;

		  case IMAGE_POINTER:
		    abort ();

		  case IMAGE_SUBWINDOW:
		    /* #### implement me */
		    break;

		  case IMAGE_NOTHING:
		    /* nothing is as nothing does */
		    break;

		  default:
		    abort ();
		  }

	      xpos += rb->width;
	      elt++;
	    }
	  else
	    abort ();
	}
    }

  if (Dynarr_length (buf))
    w32_output_string (w, dl, buf, xpos, 0, start_pixpos, width, findex);

  if (dl->modeline
      && !EQ (Qzero, w->modeline_shadow_thickness)
#if 1	/* Writing to the modeline overwrites the bevels */
      )
#else
      && (f->clear
	  || f->windows_structure_changed
	  || w->shadow_thickness_changed))
#endif
    w32_bevel_modeline (w, dl);

  Dynarr_free (buf);
  
}


/*****************************************************************************
 w32_output_vertical_divider

 Draw a vertical divider down the left side of the given window.
 ****************************************************************************/
static void
w32_output_vertical_divider (struct window *w, int clear)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object color;
  RECT rect;
  HBRUSH brush;

  /* We don't use the normal gutter measurements here because the
     horizontal scrollbars and toolbars do not stretch completely over
     to the right edge of the window.  Only the modeline does. */
  int modeline_height = window_modeline_height (w);

  assert(!W32_DIVIDER_SPACING);		/* This code doesn't handle this */

  /* XXX Not sure about this */
#ifdef HAVE_SCROLLBARS
  if (f->scrollbar_on_left)
    rect.left = WINDOW_LEFT (w);
  else
    rect.left = WINDOW_RIGHT (w) - W32_DIVIDER_WIDTH;
#else
  rect.left = WINDOW_LEFT (w);
#endif
  rect.right = rect.left + W32_DIVIDER_WIDTH;

#ifdef HAVE_SCROLLBARS
  if (f->scrollbar_on_top)
    rect.top = WINDOW_TOP (w);
  else
#endif
    rect.top = WINDOW_TEXT_TOP (w);
  rect.bottom = WINDOW_BOTTOM (w) - modeline_height;

  /* Draw the divider line */
  color = WINDOW_FACE_CACHEL_BACKGROUND (w, MODELINE_INDEX);
  w32_update_gc(FRAME_W32_DC(f), Qnil, Qnil, color, Qnil, Qnil);
  brush = COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (color));
  FillRect (FRAME_W32_DC(f), &rect, brush);
  DrawEdge (FRAME_W32_DC(f), &rect, BDR_RAISEDINNER, BF_RECT);
}


/****************************************************************************
 w32_text_width

 Given a string and a face, return the string's length in pixels when
 displayed in the font associated with the face.
 XXX FIXME: get redisplay_text_width_emchar_string() etc to pass in the
 window so we can get hold of the window's frame's gc
 ****************************************************************************/
static int
w32_text_width (struct face_cachel *cachel, CONST Emchar *str,
		Charcount len)
{
  int width_so_far = 0;
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  textual_run *runs = alloca_array (textual_run, len);
  int nruns;
  int i;
  HDC hdc=NULL;	/* XXXXX FIXME! only works for non-proportional fonts! */

  nruns = separate_textual_runs (text_storage, runs, str, len);

  for (i = 0; i < nruns; i++)
    width_so_far += w32_text_width_single_run (hdc, cachel, runs + i);

  return width_so_far;
}


/****************************************************************************
 w32_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
w32_clear_region (Lisp_Object locale, face_index findex, int x, int y,
		  int width, int height)
{
  struct window *w;
  struct frame *f;
  Lisp_Object background_pixmap = Qunbound;
  Lisp_Object temp;
  RECT rect = { x, y, x+width, y+height };
  HBRUSH brush;

  if (!(width && height))   /* We often seem to get called with width==0 */
    return;

  if (WINDOWP (locale))
    {
      w = XWINDOW (locale);
      f = XFRAME (w->frame);
    }
  else if (FRAMEP (locale))
    {
      w = NULL;
      f = XFRAME (locale);
    }
  else
    abort ();
  
  if (w)
    {
      temp = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, findex);

      if (IMAGE_INSTANCEP (temp)
	  && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (temp)))
	{
	  /* #### maybe we could implement such that a string
	     can be a background pixmap? */
	  background_pixmap = temp;
	}
    }
  else
    {
      temp = FACE_BACKGROUND_PIXMAP (Vdefault_face, locale);

      if (IMAGE_INSTANCEP (temp)
	  && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (temp)))
	{
	  background_pixmap = temp;
	}
    }

  if (!UNBOUNDP (background_pixmap))
    {
      if (XIMAGE_INSTANCE_PIXMAP_DEPTH (background_pixmap) == 0)
	{
	  Lisp_Object fcolor, bcolor;

	  if (w)
	    {
	      fcolor = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
	      bcolor = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
	    }
	  else
	    {
	      fcolor = FACE_FOREGROUND (Vdefault_face, locale);
	      bcolor = FACE_BACKGROUND (Vdefault_face, locale);
	    }

	  w32_update_gc (FRAME_W32_DC(f), Qnil, fcolor, bcolor, background_pixmap, Qnil);
      }

      /* XX FIXME: Get brush from background_pixmap here */
      assert(0);
    }
  else
    {
      Lisp_Object color = (w ? WINDOW_FACE_CACHEL_BACKGROUND (w, findex) :
			   FACE_BACKGROUND (Vdefault_face, locale));
      brush = COLOR_INSTANCE_W32_BRUSH (XCOLOR_INSTANCE (color));
    }

  FillRect (FRAME_W32_DC(f), &rect, brush);
}


/*****************************************************************************
 w32_clear_to_window_end

 Clear the area between ypos1 and ypos2.  Each margin area and the
 text area is handled separately since they may each have their own
 background color.
 ****************************************************************************/
static void
w32_clear_to_window_end (struct window *w, int ypos1, int ypos2)
{
  int height = ypos2 - ypos1;

  if (height)
    {
      struct frame *f = XFRAME (w->frame);
      Lisp_Object window;
      int bflag = (window_needs_vertical_divider (w) ? 0 : 1);
      layout_bounds bounds;

      bounds = calculate_display_line_boundaries (w, bflag);
      XSETWINDOW (window, w);

      if (window_is_leftmost (w))
	w32_clear_region (window, DEFAULT_INDEX, FRAME_LEFT_BORDER_START (f),
			  ypos1, FRAME_BORDER_WIDTH (f), height);

      if (bounds.left_in - bounds.left_out > 0)
	w32_clear_region (window,
			  get_builtin_face_cache_index (w, Vleft_margin_face),
			  bounds.left_out, ypos1,
			  bounds.left_in - bounds.left_out, height);

      if (bounds.right_in - bounds.left_in > 0)
	w32_clear_region (window, DEFAULT_INDEX, bounds.left_in, ypos1,
			  bounds.right_in - bounds.left_in, height);

      if (bounds.right_out - bounds.right_in > 0)
	w32_clear_region (window,
			  get_builtin_face_cache_index (w, Vright_margin_face),
			  bounds.right_in, ypos1,
			  bounds.right_out - bounds.right_in, height);

      if (window_is_rightmost (w))
	w32_clear_region (window, DEFAULT_INDEX, FRAME_RIGHT_BORDER_START (f),
			  ypos1, FRAME_BORDER_WIDTH (f), height);
    }

}


static void
w32_clear_frame (struct frame *f)
{
}




/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_redisplay_w32 (void)
{
  /* redisplay methods */
  CONSOLE_HAS_METHOD (w32, text_width);
  CONSOLE_HAS_METHOD (w32, output_display_block);
  CONSOLE_HAS_METHOD (w32, divider_width);
  CONSOLE_HAS_METHOD (w32, divider_height);
  CONSOLE_HAS_METHOD (w32, eol_cursor_width);
  CONSOLE_HAS_METHOD (w32, output_vertical_divider);
  CONSOLE_HAS_METHOD (w32, clear_to_window_end);
  CONSOLE_HAS_METHOD (w32, clear_region);
  CONSOLE_HAS_METHOD (w32, clear_frame);
  CONSOLE_HAS_METHOD (w32, output_begin);
  CONSOLE_HAS_METHOD (w32, output_end);
  CONSOLE_HAS_METHOD (w32, flash);
  CONSOLE_HAS_METHOD (w32, ring_bell);
}
