/* Common code between X and GTK -- redisplay-related.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003, 2005, 2009, 2010 Ben Wing.
   Copyright (C) 2010 Didier Verna

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

/* Author: Chuck Thompson */
/* Gtk flavor by William Perry */
/* X and GTK code merged by Ben Wing, 1-10 */

/* Lots of work done by Ben Wing for Mule */

/* Before including this file, you need to define either THIS_IS_X or
   THIS_IS_GTK.  See comments in console-xlike-inc.h. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "debug.h"
#include "device-impl.h"
#include "faces.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "gutter.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#ifdef MULE
#include "mule-ccl.h"
#endif
#include "charset.h"

#define NEED_GCCACHE_H
#define NEED_GLYPHS_H
#define NEED_OBJECTS_IMPL_H
#include "console-xlike-inc.h"

#include "sysproc.h" /* for select() */

#ifdef THIS_IS_X
#include "EmacsFrame.h"
#include "EmacsFrameP.h"

#include <X11/bitmaps/gray>
#endif /* THIS_IS_X */

#define EOL_CURSOR_WIDTH	5

/* Device methods */

#define XLIKE_text_width XFUN (text_width)
#define XLIKE_output_display_block XFUN (output_display_block)
#define XLIKE_divider_height XFUN (divider_height)
#define XLIKE_eol_cursor_width XFUN (eol_cursor_width)
#define XLIKE_output_vertical_divider XFUN (output_vertical_divider)
#define XLIKE_clear_region XFUN (clear_region)
#define XLIKE_clear_frame XFUN (clear_frame)
#define XLIKE_flash XFUN (flash)
#define XLIKE_ring_bell XFUN (ring_bell)
#define XLIKE_bevel_area XFUN (bevel_area)
#define XLIKE_output_string XFUN (output_string)
#define XLIKE_output_pixmap XFUN (output_pixmap)
#define XLIKE_output_xlike_pixmap XFUN (output_xlike_pixmap)
#define XLIKE_window_output_begin XFUN (window_output_begin)
#define XLIKE_window_output_end XFUN (window_output_end)

/* Miscellaneous split functions */

#define console_type_create_redisplay_XLIKE XLIKE_PASTE (console_type_create_redisplay, XLIKE_NAME)
#define XLIKE_get_gc XFUN (get_gc)
#define XLIKE_output_blank XFUN (output_blank)
#define XLIKE_text_width_single_run XFUN (text_width_single_run)

static void XLIKE_output_blank (struct window *w, struct display_line *dl,
				struct rune *rb, int start_pixpos,
				int cursor_start, int cursor_width);
static void XLIKE_output_horizontal_line (struct window *w,
					  struct display_line *dl,
					  struct rune *rb);
static void XLIKE_output_eol_cursor (struct window *w,
				     struct display_line *dl,
				     int xpos, face_index findex);
static void XLIKE_clear_frame_windows (Lisp_Object window);
static void XLIKE_bevel_area (struct window *w, face_index findex,
			      int x, int y, int width, int height,
			      int shadow_thickness, int edges,
			      enum edge_style style);
static void XLIKE_ring_bell (struct device *d, int volume, int pitch,
			     int duration);

#ifdef THIS_IS_X
static void XLIKE_window_output_begin (struct window *UNUSED (w));
static void XLIKE_window_output_end (struct window *w);
#endif /* THIS_IS_X */


/****************************************************************************/
/*                                                                          */
/*                           Separate textual runs                          */
/*                                                                          */
/****************************************************************************/


     /* Note: We do not use the Xmb*() functions and XFontSets, nor the
	Motif XFontLists and CompoundStrings.
	Those functions are generally losing for a number of reasons.
	Most important, they only support one locale (e.g. you could
	display Japanese and ASCII text, but not mixed Japanese/Chinese
	text).  You could maybe call setlocale() frequently to try to deal
	with this, but that would generally fail because an XFontSet is
	tied to one locale and won't have the other character sets in it.

	fontconfig (the font database for Xft) has some specifier-like
	properties, but it's not sufficient (witness the existence of
	Pango).  Pango might do the trick, but it's not a cross-platform
	solution; it would need significant advantages to be worth the
	effort.
     */

struct textual_run
{
  Lisp_Object charset;
  unsigned char *ptr;
  int len;
  int dimension;
};

/* Separate out the text in STR (an array of Ichars, not a string
   representation) of length LEN into a series of runs, stored in
   RUN_STORAGE.  RUN_STORAGE is guaranteed to hold enough space for all
   runs that could be generated from this text.  Each run points to the a
   stretch of text given simply by the position codes TEXT_STORAGE into a
   series of textual runs of a particular charset.  Also convert the
   characters as necessary into the format needed by XDrawImageString(),
   XDrawImageString16(), et al.  This means converting to one or two byte
   format, possibly tweaking the high bits, and possibly running a CCL
   program.  You must pre-allocate the space used and pass it in. (This is
   done so you can ALLOCA () the space.) (2 * len) bytes must be allocated
   for TEXT_STORAGE and (len * sizeof (struct textual_run)) bytes of
   RUN_STORAGE, where LEN is the length of the dynarr.

   bufchar might not be fixed width (in the case of UTF-8).

   Returns the number of runs actually used. */

/* Notes on Xft implementation

   - With Unicode, we're no longer going to have repertoires reified as
   charsets.  (Not that we ever really did, what with corporate variants,
   and so on.)  So we really should be querying the face for the desired
   font, rather than the character for the charset, and that's what would
   determine the separation into runs.
   - The widechar versions of fontconfig (and therefore Xft) functions
   seem to be just bigendian Unicode.  So there's actually no need to use
   the 8-bit versions in computing runs and runes, it would seem.
*/

#if !defined(USE_XFT) && !defined(MULE)
static int
separate_textual_runs_nomule (unsigned char *text_storage,
			      struct textual_run *run_storage,
			      const Ichar *str, Charcount len,
			      struct face_cachel *UNUSED (cachel))
{
  if (!len)
    return 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 1;
  run_storage[0].charset = Qnil;

  while (len--)
    *text_storage++ = *str++;
  return 1;
}
#endif

#if defined(USE_XFT) && !defined(MULE)
/*
  Note that in this configuration the "Croatian hack" of using an 8-bit,
  non-Latin-1 font to get localized display without Mule simply isn't
  available.  That's by design -- Unicode does not aid or abet that kind
  of punning.
  This means that the cast to XftChar16 gives the correct "conversion" to
  UCS-2.
  #### Is there an alignment issue with text_storage?
*/
static int
separate_textual_runs_xft_nomule (unsigned char *text_storage,
				  struct textual_run *run_storage,
				  const Ichar *str, Charcount len,
				  struct face_cachel *UNUSED (cachel))
{
  int i;
  if (!len)
    return 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 2;
  run_storage[0].charset = Qnil;

  for (i = 0; i < len; i++)
    {
      *(XftChar16 *)text_storage = str[i];
      text_storage += sizeof (XftChar16);
    }
  return 1;
}
#endif

#if defined(USE_XFT) && defined(MULE)
static int
separate_textual_runs_xft_mule (unsigned char *text_storage,
				struct textual_run *run_storage,
				const Ichar *str, Charcount len,
				struct face_cachel *UNUSED (cachel))
{
  Lisp_Object prev_charset = Qunbound;
  int runs_so_far = 0, i;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 2;
  run_storage[0].charset = Qnil;

  for (i = 0; i < len; i++)
    {
      Ichar ch = str[i];
      Lisp_Object charset = ichar_charset (ch);
      int ucs = ichar_to_unicode (ch);

      /* If UCS is less than zero or greater than 0xFFFF, set ucs2 to
	 REPLACMENT CHARACTER. */
      /* That means we can't handle characters outside of the BMP for now */
      ucs = (ucs & ~0xFFFF) ? 0xFFFD : ucs;

      if (!EQ (charset, prev_charset))
	{
	  if (runs_so_far)
	    run_storage[runs_so_far-1].len = (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
	  run_storage[runs_so_far].ptr = text_storage;
	  run_storage[runs_so_far].dimension = 2;
	  run_storage[runs_so_far].charset = charset;
	  prev_charset = charset;
	  runs_so_far++;
	}

      *(XftChar16 *)text_storage = ucs;
      text_storage += sizeof (XftChar16);
    }

  if (runs_so_far)
    run_storage[runs_so_far-1].len = (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
  return runs_so_far;
}
#endif

#if !defined(USE_XFT) && defined(MULE)
/*
  This is the most complex function of this group, due to the various
  indexing schemes used by different fonts.  For our purposes, they
  fall into three classes.  Some fonts are indexed compatibly with ISO
  2022; those fonts just use the Mule internal representation directly
  (typically the high bit must be reset; this is determined by the `graphic'
  flag).  Some fonts are indexed by Unicode, specifically by UCS-2.  These
  are all translated using `ichar_to_unicode'.  Finally some fonts have
  irregular indexes, and must be translated ad hoc.  In XEmacs ad hoc
  translations are accomplished with CCL programs. */
static int
separate_textual_runs_mule (unsigned char *text_storage,
			    struct textual_run *run_storage,
			    const Ichar *str, Charcount len,
			    struct face_cachel *cachel)
{
  Lisp_Object prev_charset = Qunbound;
  int runs_so_far = 0, i;
  Ibyte charset_leading_byte = LEADING_BYTE_ASCII;
  int dimension = 1, graphic = 0, need_ccl_conversion = 0;
  Lisp_Object ccl_prog;
  struct ccl_program char_converter;

  int translate_to_ucs_2 = 0;

  for (i = 0; i < len; i++)
    {
      Ichar ch = str[i];
      Lisp_Object charset;
      int byte1, byte2;		/* BREAKUP_ICHAR dereferences the addresses
				   of its arguments as pointer to int. */
      BREAKUP_ICHAR (ch, charset, byte1, byte2);

      if (!EQ (charset, prev_charset))
	{
	  /* At this point, dimension' and `prev_charset' refer to just-
	     completed run.  `runs_so_far' and `text_storage' refer to the
	     run about to start. */
	  if (runs_so_far)
	    {
	      /* Update metadata for previous run. */
	      run_storage[runs_so_far - 1].len =
		text_storage - run_storage[runs_so_far - 1].ptr;
	      if (2 == dimension) run_storage[runs_so_far - 1].len >>= 1;
	    }

	  /* Compute metadata for current run.
	     First, classify font.
	     If the font is indexed by UCS-2, set `translate_to_ucs_2'.
	     Else if the charset has a CCL program, set `need_ccl_conversion'.
	     Else if the font is indexed by an ISO 2022 "graphic register",
	         set `graphic'.
	     These flags are almost mutually exclusive, but we're sloppy
	     about resetting "shadowed" flags.  So the flags must be checked
	     in the proper order in computing byte1 and byte2, below. */
	  charset_leading_byte = XCHARSET_LEADING_BYTE (charset);
	  translate_to_ucs_2 =
	    bit_vector_bit (FACE_CACHEL_FONT_FINAL_STAGE (cachel),
			    charset_leading_byte - MIN_LEADING_BYTE);
	  if (translate_to_ucs_2)
	    {
	      dimension = 2;
	    }
	  else
	    {
	      dimension = XCHARSET_DIMENSION (charset);

	      /* Check for CCL charset.
		 If setup_ccl_program fails, we'll get a garbaged display.
		 This should never happen, and even if it does, it should
		 be harmless (unless the X server has buggy handling of
		 characters undefined in the font).  It may be marginally
		 more useful to users and debuggers than substituting a
		 fixed replacement character. */
	      ccl_prog = XCHARSET_CCL_PROGRAM (charset);
	      if ((!NILP (ccl_prog))
		  && (setup_ccl_program (&char_converter, ccl_prog) >= 0))
		{
		  need_ccl_conversion = 1;
		}
	      else 
		{
		  /* The charset must have an ISO 2022-compatible font index.
		     There are 2 "registers" (what such fonts use as index).
		     GL (graphic == 0) has the high bit of each octet reset,
		     GR (graphic == 1) has it set. */
		  graphic   = XCHARSET_GRAPHIC (charset);
		  need_ccl_conversion = 0;
		}
	    }

	  /* Initialize metadata for current run. */
	  run_storage[runs_so_far].ptr       = text_storage;
	  run_storage[runs_so_far].charset   = charset;
	  run_storage[runs_so_far].dimension = dimension;

	  /* Update loop variables. */
	  prev_charset = charset;
	  runs_so_far++;
	} 

      /* Must check flags in this order.  See comment above. */
      if (translate_to_ucs_2)
	{
	  int ucs = ichar_to_unicode (ch);
	  /* If UCS is less than zero or greater than 0xFFFF, set ucs2 to
	     REPLACMENT CHARACTER. */
	  ucs = (ucs & ~0xFFFF) ? 0xFFFD : ucs;

	  byte1 = ucs >> 8;
	  byte2 = ucs;
	}
      else if (need_ccl_conversion)
	{
	  char_converter.reg[0] = charset_leading_byte;
	  char_converter.reg[1] = byte1;
	  char_converter.reg[2] = byte2;
	  ccl_driver (&char_converter, 0, 0, 0, 0, CCL_MODE_ENCODING);
	  byte1 = char_converter.reg[1];
	  byte2 = char_converter.reg[2];
	}
      else if (graphic == 0)
	{
	  byte1 &= 0x7F;
	  byte2 &= 0x7F;
	}
      else
	{
	  byte1 |= 0x80;
	  byte2 |= 0x80;
	}

      *text_storage++ = (unsigned char)byte1;

      if (2 == dimension) *text_storage++ = (unsigned char)byte2;
    }

  if (runs_so_far)
    {
      run_storage[runs_so_far - 1].len =
	text_storage - run_storage[runs_so_far - 1].ptr;
      /* Dimension retains the relevant value for the run before it. */
      if (2 == dimension)
	run_storage[runs_so_far - 1].len >>= 1;
    }

  return runs_so_far;
}
#endif

static int
separate_textual_runs (unsigned char *text_storage,
		       struct textual_run *run_storage,
		       const Ichar *str, Charcount len,
		       struct face_cachel *cachel)
{
#if defined(USE_XFT) && defined(MULE)
  return separate_textual_runs_xft_mule (text_storage, run_storage,
					 str, len, cachel);
#endif
#if defined(USE_XFT) && !defined(MULE)
  return separate_textual_runs_xft_nomule (text_storage, run_storage,
					   str, len, cachel);
#endif
#if !defined(USE_XFT) && defined(MULE)
  return separate_textual_runs_mule (text_storage, run_storage,
				     str, len, cachel);
#endif
#if !defined(USE_XFT) && !defined(MULE)
  return separate_textual_runs_nomule (text_storage, run_storage,
				       str, len, cachel);
#endif
}

/****************************************************************************/
/*                                                                          */
/*                           Xlike output routines                          */
/*                                                                          */
/****************************************************************************/

static int
XLIKE_text_width_single_run (struct frame * USED_IF_XFT (f),
			     struct face_cachel *cachel,
			     struct textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);

  if (!fi->proportional_p)
    return fi->width * run->len;
#ifdef USE_XFT
  else if (FONT_INSTANCE_X_XFTFONT (fi))
    {
      static XGlyphInfo glyphinfo;
      struct device *d = XDEVICE (f->device);
      Display *dpy = DEVICE_X_DISPLAY (d);

      if (run->dimension == 2)
	{
	  XftTextExtents16 (dpy,
			    FONT_INSTANCE_X_XFTFONT (fi),
			    (XftChar16 *) run->ptr, run->len, &glyphinfo);
	}
      else
	{
	  XftTextExtents8 (dpy,
			   FONT_INSTANCE_X_XFTFONT (fi),
			   run->ptr, run->len, &glyphinfo);
	}
    
      return glyphinfo.xOff;
    }
#endif
  else if (FONT_INSTANCE_XLIKE_FONT (fi))
    {
      if (run->dimension == 2)
	{	
	  /* stderr_out ("Measuring wide characters\n"); */
	  return XLIKE_TEXT_WIDTH_WIDE (FONT_INSTANCE_XLIKE_FONT (fi),
					run->ptr, run->len);
	}
      else
	{
	  return XLIKE_TEXT_WIDTH (FONT_INSTANCE_XLIKE_FONT (fi),
				   run->ptr, run->len);
	}
    }
  else
    abort();
  return 0;			/* shut up GCC */
}


/*
   XLIKE_text_width

   Given a string and a merged face, return the string's length in pixels
   when displayed in the fonts associated with the face.
   */

static int
XLIKE_text_width (struct window *w, struct face_cachel *cachel,
		  const Ichar *str, Charcount len)
{
  /* !!#### Needs review */
  int width_so_far = 0;
  unsigned char *text_storage = (unsigned char *) ALLOCA (2 * len);
  struct textual_run *runs = alloca_array (struct textual_run, len);
  struct frame *f = WINDOW_XFRAME (w);
  int nruns;
  int i;

  nruns = separate_textual_runs (text_storage, runs, str, len, 
				 cachel);

  for (i = 0; i < nruns; i++)
    width_so_far += XLIKE_text_width_single_run (f, cachel, runs + i);

  return width_so_far;
}

/*****************************************************************************
 XLIKE_divider_height

 Return the height of the horizontal divider.  This is a function because
 divider_height is a device method.

 #### If we add etched horizontal divider lines this will have to get
 smarter.
 ****************************************************************************/
static int
XLIKE_divider_height (void)
{
#ifdef THIS_IS_X
  return 1;
#else /* THIS_IS_GTK */
  return 2;
#endif /* THIS_IS_GTK */
}

/*****************************************************************************
 XLIKE_eol_cursor_width

 Return the width of the end-of-line cursor.  This is a function
 because eol_cursor_width is a device method.
 ****************************************************************************/
static int
XLIKE_eol_cursor_width (void)
{
  return EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 XLIKE_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 ****************************************************************************/
static void
XLIKE_output_display_block (struct window *w, struct display_line *dl,
			    int block, int start, int end, int start_pixpos,
			    int cursor_start, int cursor_width,
			    int cursor_height)
{
#ifndef USE_XFT
  struct frame *f = XFRAME (w->frame);
#endif
  Ichar_dynarr *buf;
  Lisp_Object window;

  struct display_block *db = Dynarr_atp (dl->display_blocks, block);
  rune_dynarr *rba = db->runes;
  struct rune *rb;

  int elt = start;
  face_index findex;
  int xpos, width = 0;
  Lisp_Object charset = Qunbound; /* Qnil is a valid charset when
				     MULE is not defined */

  window = wrap_window (w);
  rb = Dynarr_atp (rba, start);

  if (!rb)
    /* Nothing to do so don't do anything. */
    return;

  findex = rb->findex;
  xpos = rb->xpos;
  if (rb->type == RUNE_CHAR)
    charset = ichar_charset (rb->object.chr.ch);

  if (end < 0)
    end = Dynarr_length (rba);
  buf = Dynarr_new (Ichar);

  while (elt < end)
    {
      rb = Dynarr_atp (rba, elt);

      if (rb->findex == findex && rb->type == RUNE_CHAR
	  && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
	  && EQ (charset, ichar_charset (rb->object.chr.ch)))
	{
	  Dynarr_add (buf, rb->object.chr.ch);
	  width += rb->width;
	  elt++;
	}
      else
	{
	  if (Dynarr_length (buf))
	    {
	      XLIKE_output_string (w, dl, buf, xpos, 0, start_pixpos, width,
				   findex, 0, cursor_start, cursor_width,
				   cursor_height);
	      xpos = rb->xpos;
	      width = 0;
	    }
	  Dynarr_reset (buf);
	  width = 0;

	  if (rb->type == RUNE_CHAR)
	    {
	      findex = rb->findex;
	      xpos = rb->xpos;
	      charset = ichar_charset (rb->object.chr.ch);

	      if (rb->cursor_type == CURSOR_ON)
		{
		  if (rb->object.chr.ch == '\n')
		    {
		      XLIKE_output_eol_cursor (w, dl, xpos, findex);
		    }
		  else
		    {
		      Dynarr_add (buf, rb->object.chr.ch);
		      XLIKE_output_string (w, dl, buf, xpos, 0, start_pixpos,
					   rb->width, findex, 1,
					   cursor_start, cursor_width,
					   cursor_height);
		      Dynarr_reset (buf);
		    }

		  xpos += rb->width;
		  elt++;
		}
	      else if (rb->object.chr.ch == '\n')
		{
		  /* Clear in case a cursor was formerly here. */
		  redisplay_clear_region (window, findex, xpos,
					  XLIKE_DISPLAY_LINE_YPOS (dl),
					  rb->width,
					  XLIKE_DISPLAY_LINE_HEIGHT (dl));
		  elt++;
		}
	    }
	  else if (rb->type == RUNE_BLANK || rb->type == RUNE_HLINE)
	    {
	      if (rb->type == RUNE_BLANK)
		XLIKE_output_blank (w, dl, rb, start_pixpos, cursor_start,
				    cursor_width);
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
		  XLIKE_output_horizontal_line (w, dl, rb);
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

	      redisplay_calculate_display_boxes (dl, rb->xpos, rb->object.dglyph.xoffset,
						 rb->object.dglyph.yoffset, start_pixpos,
                                                 rb->width, &dbox, &dga);

	      window = wrap_window (w);
	      instance = glyph_image_instance (rb->object.dglyph.glyph,
					       window, ERROR_ME_DEBUG_WARN, 1);
	      findex = rb->findex;

	      if (IMAGE_INSTANCEP (instance))
		{
		  switch (XIMAGE_INSTANCE_TYPE (instance))
		    {
		    case IMAGE_TEXT:
#ifdef THIS_IS_GTK
		      {
			/* !!#### Examine for Mule-izing */
			/* #### This is way losing.  See the comment in
			   add_glyph_rune(). */
			Lisp_Object string =
			  XIMAGE_INSTANCE_TEXT_STRING (instance);
			convert_ibyte_string_into_ichar_dynarr
			  (XSTRING_DATA (string), XSTRING_LENGTH (string),
			   buf);

			gtk_output_string (w, dl, buf, xpos,
					   rb->object.dglyph.xoffset,
					   start_pixpos, -1, findex,
					   (rb->cursor_type == CURSOR_ON),
					   cursor_start, cursor_width,
					   cursor_height);
			Dynarr_reset (buf);
		      }
		      break;
#else
		      ABORT ();
#endif /* THIS_IS_GTK */
		    case IMAGE_MONO_PIXMAP:
		    case IMAGE_COLOR_PIXMAP:
		      redisplay_output_pixmap (w, instance, &dbox, &dga,
					       findex, cursor_start,
					       cursor_width,
					       cursor_height, 0);
		      break;

		    case IMAGE_WIDGET:
		      if (EQ (XIMAGE_INSTANCE_WIDGET_TYPE (instance),
			      Qlayout))
			{
			  redisplay_output_layout (window, instance, &dbox,
						   &dga, findex,
						   cursor_start, cursor_width,
						   cursor_height);
			  break;
			}

		    case IMAGE_SUBWINDOW:
		      redisplay_output_subwindow (w, instance, &dbox, &dga,
						  findex, cursor_start,
						  cursor_width, cursor_height);
		      break;

		    case IMAGE_NOTHING:
		      /* nothing is as nothing does */
		      break;

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

  if (Dynarr_length (buf))
    XLIKE_output_string (w, dl, buf, xpos, 0, start_pixpos, width, findex,
			 0, cursor_start, cursor_width, cursor_height);

  if (dl->modeline
      && !EQ (Qzero, w->modeline_shadow_thickness)
#ifndef USE_XFT
      /* This optimization doesn't work right with some Xft fonts, which
	 leave antialiasing turds at the boundary.  I don't know if this
	 is an Xft bug or not, but I think it is.   See x_output_string. */
      && (f->clear
	  || f->windows_structure_changed
	  || w->shadow_thickness_changed)
#endif
      )
    bevel_modeline (w, dl);

  Dynarr_free (buf);
}

/* Called as gtk_get_gc from gtk-glue.c */

XLIKE_GC XLIKE_get_gc (struct frame *f, Lisp_Object font,
		       Lisp_Object fg, Lisp_Object bg,
		       Lisp_Object bg_pixmap, Lisp_Object bg_placement,
		       Lisp_Object lwidth);

/*****************************************************************************
 XLIKE_get_gc

 Given a number of parameters return a GC with those properties.
 ****************************************************************************/
XLIKE_GC
XLIKE_get_gc (struct frame *f, Lisp_Object font, 
	      Lisp_Object fg, Lisp_Object bg,
	      Lisp_Object bg_pixmap, Lisp_Object bg_placement, 
	      Lisp_Object lwidth)
{
  struct device *d = XDEVICE (f->device);
  XLIKE_GCVALUES gcv;
  unsigned long mask;

  memset (&gcv, ~0, sizeof (gcv));
  gcv.graphics_exposures = XLIKE_FALSE;
  /* Make absolutely sure that we don't pick up a clipping region in
     the GC returned by this function. */
  gcv.clip_mask = XLIKE_NONE;
  gcv.clip_x_origin = 0;
  gcv.clip_y_origin = 0;
  XLIKE_SET_GC_FILL (gcv, XLIKE_FILL_SOLID);
  mask = XLIKE_GC_EXPOSURES
    | XLIKE_GC_CLIP_MASK | XLIKE_GC_CLIP_X_ORIGIN | XLIKE_GC_CLIP_Y_ORIGIN;
  mask |= XLIKE_GC_FILL;

  if (!NILP (font)
#ifdef USE_XFT
      /* Only set the font if it's a core font */
      /* the renderfont will be set elsewhere (not part of gc) */
      && !FONT_INSTANCE_X_XFTFONT (XFONT_INSTANCE (font))
#endif
      )
    {
      gcv.font =
	XLIKE_FONT_NUM (FONT_INSTANCE_XLIKE_FONT (XFONT_INSTANCE (font)));
      mask |= XLIKE_GC_FONT;
    }

  /* evil kludge! */
  if (!NILP (fg) && !COLOR_INSTANCEP (fg) && !INTP (fg))
    {
      /* #### I fixed one case where this was getting hit.  It was a
         bad macro expansion (compiler bug). */
      stderr_out ("Help! x_get_gc got a bogus fg value! fg = ");
      debug_print (fg);
      fg = Qnil;
    }

  if (!NILP (fg))
    {
      if (COLOR_INSTANCEP (fg))
	XLIKE_SET_GC_COLOR (gcv.foreground, XCOLOR_INSTANCE_XLIKE_COLOR (fg));
      else
	XLIKE_SET_GC_PIXEL (gcv.foreground, XINT (fg));
      mask |= XLIKE_GC_FOREGROUND;
    }

  if (!NILP (bg))
    {
      if (COLOR_INSTANCEP (bg))
	XLIKE_SET_GC_COLOR (gcv.background, XCOLOR_INSTANCE_XLIKE_COLOR (bg));
      else
	XLIKE_SET_GC_PIXEL (gcv.background, XINT (bg));
      mask |= XLIKE_GC_BACKGROUND;
    }

  /* This special case comes from a request to draw text with a face which has
     the dim property. We'll use a stippled foreground GC. */
  if (EQ (bg_pixmap, Qdim))
    {
      assert (DEVICE_XLIKE_GRAY_PIXMAP (d) != XLIKE_NONE);

      XLIKE_SET_GC_FILL (gcv, XLIKE_FILL_STIPPLED);
      gcv.stipple = DEVICE_XLIKE_GRAY_PIXMAP (d);
      mask |= (XLIKE_GC_FILL | XLIKE_GC_STIPPLE);
    }
  else if (IMAGE_INSTANCEP (bg_pixmap)
	   && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pixmap)))
    {
      if (XIMAGE_INSTANCE_PIXMAP_DEPTH (bg_pixmap) == 0)
	{
	  XLIKE_SET_GC_FILL (gcv, XLIKE_FILL_OPAQUE_STIPPLED);
	  gcv.stipple = XIMAGE_INSTANCE_XLIKE_PIXMAP (bg_pixmap);
	  mask |= (XLIKE_GC_STIPPLE | XLIKE_GC_FILL);
	}
      else
	{
	  XLIKE_SET_GC_FILL (gcv, XLIKE_FILL_TILED);
	  gcv.tile = XIMAGE_INSTANCE_XLIKE_PIXMAP (bg_pixmap);
	  mask |= (XLIKE_GC_TILE | XLIKE_GC_FILL);
	}
      if (EQ (bg_placement, Qabsolute))
	{
#ifdef THIS_IS_GTK
	  /* #### WARNING: this does not currently work. -- dvl
	     gcv.ts_x_origin = - FRAME_GTK_X (f);
	     gcv.ts_y_origin = - FRAME_GTK_Y (f);
	     mask |= (XLIKE_GC_TS_X_ORIGIN | XLIKE_GC_TS_Y_ORIGIN);
	  */
#else
	  gcv.ts_x_origin = - FRAME_X_X (f);
	  gcv.ts_y_origin = - FRAME_X_Y (f);
	  mask |= (XLIKE_GC_TS_X_ORIGIN | XLIKE_GC_TS_Y_ORIGIN);
#endif
	}
    }

  if (!NILP (lwidth))
    {
      gcv.line_width = XINT (lwidth);
      mask |= XLIKE_GC_LINE_WIDTH;
    }

#if 0
  debug_out ("\nx_get_gc: calling gc_cache_lookup\n");
#endif
  return gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, mask);
}

/*****************************************************************************
 XLIKE_output_string

 Given a string and a starting position, output that string in the
 given face.  If cursor is true, draw a cursor around the string.
 Correctly handles multiple charsets in the string.

 The meaning of the parameters is something like this:

 W		Window that the text is to be displayed in.
 DL		Display line that this text is on.  The values in the
 		structure are used to determine the vertical position and
		clipping range of the text.
 BUF		Dynamic array of Ichars specifying what is actually to be
		drawn.
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
#ifdef THIS_IS_GTK
static
void gdk_draw_text_image (GdkDrawable *drawable,
			  GdkFont     *font,
			  GdkGC       *gc,
			  gint         x,
			  gint         y,
			  const gchar *text,
			  gint         text_length);

#endif /* THIS_IS_GTK */
void
XLIKE_output_string (struct window *w, struct display_line *dl,
		     Ichar_dynarr *buf, int xpos, int xoffset, int clip_start,
		     int width, face_index findex, int cursor,
		     int cursor_start, int cursor_width, int cursor_height)
{
  /* General variables */
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
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
  XLIKE_GC bgc, gc;
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int ypos = XLIKE_DISPLAY_LINE_YPOS (dl);
  int len = Dynarr_length (buf);
  unsigned char *text_storage = (unsigned char *) ALLOCA (2 * len);
  struct textual_run *runs = alloca_array (struct textual_run, len);
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

  if (clip_start < xpos)
    clip_start = xpos;
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
    bgc = XLIKE_get_gc (f, Qnil, cachel->foreground, cachel->background,
			bg_pmap, cachel->background_placement, Qnil);

  if (bgc)
    {
      XLIKE_FILL_RECTANGLE (dpy, x_win, bgc, clip_start,
			    ypos, clip_end - clip_start,
			    height);
    }

  nruns = separate_textual_runs (text_storage, runs, Dynarr_begin (buf),
				 Dynarr_length (buf), cachel);

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;
      int need_clipping;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      this_width = XLIKE_text_width_single_run (f, cachel, runs + i);
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
	      if (ypos1_string > ypos2_line)
		ypos1_string = ypos2_line;
	      if (ypos2_string > ypos2_line)
		ypos2_string = ypos2_line;

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
			     Qdim, Qnil, Qnil);
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

      /* The X specific called different functions (XDraw*String
         vs. XDraw*String16), but apparently gdk_draw_text takes care
         of that for us.

	 BUT, gdk_draw_text also does too much, by dividing the length
	 by 2.  So we fake them out my multiplying the length by the
	 dimension of the text.  This will do the right thing for
	 single-dimension runs as well of course.
      */
      (bgc ? gdk_draw_text : gdk_draw_text_image)
	(GDK_DRAWABLE (x_win), FONT_INSTANCE_GTK_FONT (fi), gc, xpos,
	 dl->ypos, (char *) runs[i].ptr, runs[i].len * runs[i].dimension);
#endif /* (not) THIS_IS_X */
	}

      /* We draw underlines in the same color as the text. */
      if (cachel->underline)
	{
	  int upos, uthick;
#ifdef THIS_IS_X
	  unsigned long upos_ext, uthick_ext;
	  XFontStruct *fs =
	    use_x_font ? FONT_INSTANCE_X_FONT (XFONT_INSTANCE (font)) : 0;
	  /* #### the logic of the next two may be suboptimal: we may want
	     to use the POSITION and/or THICKNESS information with Xft */
	  if (fs && XGetFontProperty (fs, XA_UNDERLINE_POSITION, &upos_ext))
	    upos = (int) upos_ext;
	  else
#else /* THIS_IS_GTK */
	  /* Cannot get at font properties in Gtk, so we resort to
             guessing */
#endif /* THIS_IS_GTK */
	    upos = dl->descent / 2;
#ifdef THIS_IS_X
	  if (fs && XGetFontProperty (fs, XA_UNDERLINE_THICKNESS, &uthick_ext))
	    uthick = (int) uthick_ext;
	  else
#endif /* THIS_IS_X */
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
#ifdef THIS_IS_X
	  int ascent, descent, upos, uthick;
	  unsigned long ascent_ext, descent_ext, uthick_ext;
	  XFontStruct *fs = FONT_INSTANCE_X_FONT (fi);
#else /* THIS_IS_GTK */
	  gint ascent, descent, upos, uthick;
	  GdkFont *gfont = FONT_INSTANCE_GTK_FONT (fi);
#endif /* THIS_IS_GTK */
	  
#ifdef THIS_IS_X
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
#else /* THIS_IS_GTK */
	/* Cannot get at font properties in Gtk, so we resort to
           guessing */

	  ascent = gfont->ascent;
	  descent = gfont->descent;
	  uthick = 1;
#endif /* THIS_IS_GTK */

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
	      /* The X specific called different functions (XDraw*String
		 vs. XDraw*String16), but apparently gdk_draw_text takes care
		 of that for us.

		 BUT, gdk_draw_text also does too much, by dividing the
		 length by 2.  So we fake them out my multiplying the
		 length by the dimension of the text.  This will do the
		 right thing for single-dimension runs as well of course.
	      */
	      gdk_draw_text_image (GDK_DRAWABLE (x_win),
				   FONT_INSTANCE_GTK_FONT (fi), cgc, xpos,
				   dl->ypos, (char *) runs[i].ptr,
				   runs[i].len * runs[i].dimension);
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
			     make_int (bar_width));
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
	  if (tmp_y < (int) ypos)
	    tmp_y = ypos;
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

#ifdef THIS_IS_GTK
static void
our_draw_bitmap (GdkDrawable *drawable,
		 GdkGC       *gc,
		 GdkPixmap   *src,
		 gint         xsrc,
		 gint         ysrc,
		 gint         xdest,
		 gint         ydest,
		 gint         width,
		 gint         height);
#endif /* THIS_IS_GTK */


static void
XLIKE_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x,
			   int y, int xoffset, int yoffset,
			   int width, int height,
			   XLIKE_COLOR fg, XLIKE_COLOR bg,
			   XLIKE_GC override_gc)
{
  struct device *d = XDEVICE (f->device);
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;
  XLIKE_GCVALUES gcv;
  unsigned long pixmap_mask;

  if (!override_gc)
    {
      memset (&gcv, ~0, sizeof (gcv));
      gcv.graphics_exposures = XLIKE_FALSE;
      XLIKE_SET_GC_COLOR (gcv.foreground, fg);
      XLIKE_SET_GC_COLOR (gcv.background, bg);
      pixmap_mask = XLIKE_GC_FOREGROUND | XLIKE_GC_BACKGROUND | XLIKE_GC_EXPOSURES;

      if (IMAGE_INSTANCE_XLIKE_MASK (p))
	{
	  gcv.function = XLIKE_GX_COPY;
	  gcv.clip_mask = IMAGE_INSTANCE_XLIKE_MASK (p);
	  gcv.clip_x_origin = x - xoffset;
	  gcv.clip_y_origin = y - yoffset;
	  pixmap_mask |= (XLIKE_GC_FUNCTION | XLIKE_GC_CLIP_MASK |
			  XLIKE_GC_CLIP_X_ORIGIN |
			  XLIKE_GC_CLIP_Y_ORIGIN);
	  /* Can't set a clip rectangle below because we already have a mask.
	     We could conceivably create a new clipmask by zeroing out
	     everything outside the clip region.  Is it worth it?
	     Is it possible to get an equivalent effect by changing the
	     args to XCopyArea below rather than messing with a clip box?
	     - dkindred@cs.cmu.edu
	     Yes. We don't clip at all now - andy@xemacs.org
	  */
	}

      gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, pixmap_mask);
    }
  else
    {
      gc = override_gc;
      /* override_gc might have a mask already--we don't want to nuke it.
	 Maybe we can insist that override_gc have no mask, or use
	 one of the suggestions above. */
    }

#ifdef THIS_IS_X
  /* depth of 0 means it's a bitmap, not a pixmap, and we should use
     XCopyPlane (1 = current foreground color, 0 = background) instead
     of XCopyArea, which means that the bits in the pixmap are actual
     pixel values, instead of symbolic of fg/bg. */
#endif /* THIS_IS_X */
  if (IMAGE_INSTANCE_PIXMAP_DEPTH (p) > 0)
    {
#ifdef THIS_IS_X
      XCopyArea (dpy,
		 IMAGE_INSTANCE_X_PIXMAP_SLICE
		 (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc, xoffset,
		 yoffset, width,
		 height, x, y);
#else /* THIS_IS_GTK */
      USED (dpy);
      gdk_draw_pixmap (GDK_DRAWABLE (x_win), gc,
		       IMAGE_INSTANCE_GTK_PIXMAP (p),
		       xoffset, yoffset, x, y, width, height);
#endif /* THIS_IS_GTK */
    }
  else
    {
#ifdef THIS_IS_X
      XCopyPlane (dpy, IMAGE_INSTANCE_X_PIXMAP_SLICE
		  (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc,
		  xoffset, yoffset, width, height, x, y, 1L);
#else /* THIS_IS_GTK */
      USED (dpy);
      our_draw_bitmap (GDK_DRAWABLE (x_win), gc,
		       IMAGE_INSTANCE_GTK_PIXMAP (p),
		       xoffset, yoffset, x, y, width, height);
#endif /* THIS_IS_GTK */
    }
}

static void
XLIKE_output_pixmap (struct window *w, Lisp_Object image_instance,
		     struct display_box *db, struct display_glyph_area *dga,
		     face_index findex, int cursor_start, int cursor_width,
		     int cursor_height, int UNUSED (bg_pixmap))
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);

  /* Output the pixmap. */
  {
    Lisp_Object tmp_pixel;
    XLIKE_COLOR tmp_bcolor, tmp_fcolor;

    tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
    tmp_fcolor = XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel);
    tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
    tmp_bcolor = XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel);

    XLIKE_output_xlike_pixmap (f, p, db->xpos, db->ypos,
			       dga->xoffset, dga->yoffset,
			       dga->width, dga->height,
			       tmp_fcolor, tmp_bcolor, 0);
  }

  /* Draw a cursor over top of the pixmap. */
  if (cursor_width && cursor_height && (cursor_start >= db->xpos)
      && !NILP (w->text_cursor_visible_p)
      && (cursor_start < db->xpos + dga->width))
    {
      XLIKE_GC gc;
      int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
      struct face_cachel *cursor_cachel =
	WINDOW_FACE_CACHEL (w,
			    get_builtin_face_cache_index
			    (w, Vtext_cursor_face));

      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil,
			 Qnil);

      if (cursor_width > db->xpos + dga->width - cursor_start)
	cursor_width = db->xpos + dga->width - cursor_start;

      if (focus)
	  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, cursor_start, db->ypos,
				cursor_width, cursor_height);
      else
	{
	  XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, cursor_start, db->ypos,
				cursor_width, cursor_height);
	}
    }
}

/*****************************************************************************
 XLIKE_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void
XLIKE_output_vertical_divider (struct window *w, int USED_IF_X (clear))
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  Lisp_Object tmp_pixel;
  XLIKE_GCVALUES gcv;
  XLIKE_GC background_gc;
  enum edge_style style;
  unsigned long mask;
  int x, y1, y2, width, shadow_thickness, spacing, line_width;
  face_index div_face =
    get_builtin_face_cache_index (w, Vvertical_divider_face);

  width = window_divider_width (w);
  shadow_thickness = XINT (w->vertical_divider_shadow_thickness);
  spacing = XINT (w->vertical_divider_spacing);
  line_width = XINT (w->vertical_divider_line_width);
  x = WINDOW_RIGHT (w) - width;
  y1 = WINDOW_TOP (w);
  y2 = WINDOW_BOTTOM (w);

  memset (&gcv, ~0, sizeof (gcv));

  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, div_face);

  /* First, get the GC's. */
  XLIKE_SET_GC_COLOR (gcv.background, XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  gcv.foreground = gcv.background;
  gcv.graphics_exposures = XLIKE_FALSE;
  mask = XLIKE_GC_FOREGROUND | XLIKE_GC_BACKGROUND | XLIKE_GC_EXPOSURES;

  background_gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, mask);

  /* Clear the divider area first.  This needs to be done when a
     window split occurs. */
#ifdef THIS_IS_X
  if (clear)
    XClearArea (dpy, x_win, x, y1, width, y2 - y1, False);
#else /* THIS_IS_GTK */
  USED (dpy);
  /* if (clear) */
  gdk_draw_rectangle (GDK_DRAWABLE (x_win), background_gc, TRUE,
		      x, y1, width, y2 - y1);
#endif /* THIS_IS_GTK */

#ifndef THIS_IS_GTK
   /* #### FIXME Why not? Formerly '#if 0' in the GDK code */
  /* Draw the divider line. */
  XLIKE_FILL_RECTANGLE (dpy, x_win, background_gc,
			x + spacing + shadow_thickness, y1,
			line_width, y2 - y1);
#endif /* not THIS_IS_GTK */

  if (shadow_thickness < 0)
    {
      shadow_thickness = -shadow_thickness;
      style = EDGE_BEVEL_IN;
    }
  else
    {
      style = EDGE_BEVEL_OUT;
    }

  /* Draw the shadows around the divider line */
  XLIKE_bevel_area (w, div_face, x + spacing, y1,
		    width - 2 * spacing, y2 - y1,
		    shadow_thickness, EDGE_ALL, style);
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

  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;
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
    gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       Qnil, Qnil, Qnil, Qnil);
  else
    gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       bg_pmap,
		       WINDOW_FACE_CACHEL_BACKGROUND_PLACEMENT (w, rb->findex),
		       Qnil);

  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);

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

      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			 Qnil, Qnil, Qnil);

      cursor_y = dl->ypos - fi->ascent;
      cursor_height = fi->height;
      if (cursor_y + cursor_height > y + height)
	cursor_height = y + height - cursor_y;

      if (focus)
	{
	  if (NILP (bar_cursor_value))
	    {
	      XLIKE_FILL_RECTANGLE (dpy, x_win, gc, cursor_start, cursor_y,
				    fi->width, cursor_height);
	    }
	  else
	    {
	      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background,
				 Qnil, Qnil, Qnil,
				 make_int (bar_width));
	      XLIKE_DRAW_LINE (dpy, x_win, gc, cursor_start + bar_width - 1,
			       cursor_y, cursor_start + bar_width - 1,
			       cursor_y + cursor_height - 1);
	    }
	}
      else if (NILP (bar_cursor_value))
	{
	  XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, cursor_start, cursor_y,
				fi->width - 1, cursor_height - 1);
	}
    }
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
  struct device *d = XDEVICE (f->device);

  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc;

  int x = rb->xpos;
  int width = rb->width;
  int height = XLIKE_DISPLAY_LINE_HEIGHT (dl);
  int ypos1, ypos2, ypos3, ypos4;

  ypos1 = XLIKE_DISPLAY_LINE_YPOS (dl);
  ypos2 = ypos1 + rb->object.hline.yoffset;
  ypos3 = ypos2 + rb->object.hline.thickness;
  ypos4 = dl->ypos + dl->descent - dl->clip;

  /* First clear the area not covered by the line. */
  if (height - rb->object.hline.thickness > 0)
    {
      gc = XLIKE_get_gc (f, Qnil,
			 WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
			 Qnil, Qnil, Qnil, Qnil);

      if (ypos2 - ypos1 > 0)
	XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos1, width, ypos2 - ypos1);
      if (ypos4 - ypos3 > 0)
	XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos1, width, ypos2 - ypos1);
    }

#ifdef THIS_IS_GTK
  {
    GtkStyle *style = FRAME_GTK_TEXT_WIDGET (f)->style;
    gtk_paint_hline (style, x_win, GTK_STATE_NORMAL, NULL,
		     FRAME_GTK_TEXT_WIDGET (f), "hline", x, x + width, ypos2);
  }
#else /* THIS_IS_X */
  /* Now draw the line. */
  gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		     Qnil, Qnil, Qnil, Qnil);

  if (ypos2 < ypos1)
    ypos2 = ypos1;
  if (ypos3 > ypos4)
    ypos3 = ypos4;

  if (ypos3 - ypos2 > 0)
    XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, ypos2, width, ypos3 - ypos2);
#endif /* THIS_IS_X */
}

/****************************************************************************
 XLIKE_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
XLIKE_clear_region (Lisp_Object UNUSED (locale), struct device* d,
		    struct frame* f, face_index UNUSED (findex), int x, int y,
		    int width, int height,
		    Lisp_Object fcolor, Lisp_Object bcolor,
		    Lisp_Object background_pixmap,
		    Lisp_Object background_placement)
{
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;

  if (!UNBOUNDP (background_pixmap))
    {
      gc = XLIKE_get_gc (f, Qnil, fcolor, bcolor,
			 background_pixmap, background_placement, Qnil);
    }

  if (gc)
    XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, y, width, height);
  else
    XLIKE_CLEAR_AREA (dpy, x_win, x, y, width, height);
}

/*****************************************************************************
 xlike_output_eol_cursor

 Draw a cursor at the end of a line.  The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
XLIKE_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
			 face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object window;

  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;
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

  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
		     Qnil, Qnil, Qnil);

  default_face_font_info (window, &defascent, 0, 0, &defheight, 0);

  /* make sure the cursor is entirely contained between y and y+height */
  cursor_height = min (defheight, height);
  cursor_y = max (y, min (y + height - cursor_height,
			  dl->ypos - defascent));

  if (focus)
    {
#ifdef HAVE_XIM
      XIM_SetSpotLocation (f, x - 2 , cursor_y + cursor_height - 2);
#endif /* HAVE_XIM */

      if (NILP (bar_cursor_value))
	{
	  XLIKE_FILL_RECTANGLE (dpy, x_win, gc, x, cursor_y, width,
				cursor_height);
	}
      else
	{
	  int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			     Qnil, Qnil,
			     make_int (bar_width));
	  XLIKE_DRAW_LINE (dpy, x_win, gc, x + bar_width - 1, cursor_y,
			   x + bar_width - 1, cursor_y + cursor_height - 1);
	}
    }
  else if (NILP (bar_cursor_value))
    {
      XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, x, cursor_y, width - 1,
			    cursor_height - 1);
    }
}

static void
XLIKE_clear_frame_window (Lisp_Object window)
{
  struct window *w = XWINDOW (window);

  if (!NILP (w->vchild))
    {
      XLIKE_clear_frame_windows (w->vchild);
      return;
    }

  if (!NILP (w->hchild))
    {
      XLIKE_clear_frame_windows (w->hchild);
      return;
    }

  redisplay_clear_to_window_end (w, WINDOW_TEXT_TOP (w),
				 WINDOW_TEXT_BOTTOM (w));
}

static void
XLIKE_clear_frame_windows (Lisp_Object window)
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    XLIKE_clear_frame_window (window);
}

static void
XLIKE_clear_frame (struct frame *f)
{
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (XDEVICE (f->device));
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
  int x, y, width, height;
  Lisp_Object frame;

  /* #### GEOM! This clears the internal border and gutter (and scrollbars)
     but not the toolbar.  Correct? */
  x = FRAME_LEFT_INTERNAL_BORDER_START (f);
  width = (FRAME_RIGHT_INTERNAL_BORDER_END (f) - x);
  /* #### This adjustment by 1 should be being done in the macros.
     There is some small differences between when the menubar is on
     and off that we still need to deal with.  The adjustment also occurs in
     redisplay_clear_top_of_window(). */
  y = FRAME_TOP_INTERNAL_BORDER_START (f) - 1;
  height = (FRAME_BOTTOM_INTERNAL_BORDER_END (f) - y);

  XLIKE_CLEAR_AREA (dpy, x_win, x, y, width, height);

  frame = wrap_frame (f);

  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      XLIKE_clear_frame_windows (f->root_window);
    }
#ifdef THIS_IS_X
  {
    struct device *d = XDEVICE (f->device);
    if (!(check_if_pending_expose_event (d)))
      XFlush (DEVICE_X_DISPLAY (d));
  }
#endif /* THIS_IS_X */
}

/* briefly swap the foreground and background colors.
 */

static int
XLIKE_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  XLIKE_DISPLAY dpy = GET_XLIKE_DISPLAY (d);
  XLIKE_WINDOW win = GET_XLIKE_WINDOW (f);
  XLIKE_GC gc = NULL;
  XLIKE_GCVALUES gcv;
  XLIKE_PIXEL tmp_fcolor, tmp_bcolor;
  Lisp_Object tmp_pixel, frame;
  struct window *w = XWINDOW (FRAME_ROOT_WINDOW (f));
  int flash_height;

  frame = wrap_frame (f);

  tmp_pixel = FACE_FOREGROUND (Vdefault_face, frame);
  tmp_fcolor = XLIKE_COLOR_TO_PIXEL (XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  tmp_pixel = FACE_BACKGROUND (Vdefault_face, frame);
  tmp_bcolor = XLIKE_COLOR_TO_PIXEL (XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  memset (&gcv, ~0, sizeof (gcv)); /* initialize all slots to ~0 */
  XLIKE_SET_GC_PIXEL (gcv.foreground, tmp_fcolor ^ tmp_bcolor);
  gcv.function = XLIKE_GX_XOR;
  gcv.graphics_exposures = XLIKE_FALSE;
  gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (XDEVICE (f->device)), &gcv,
			XLIKE_GC_FOREGROUND | XLIKE_GC_FUNCTION | XLIKE_GC_EXPOSURES);
  default_face_width_and_height (frame, 0, &flash_height);

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			    w->pixel_width, flash_height);
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left,
			    w->pixel_top + w->pixel_height - flash_height,
			    w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			  w->pixel_width, w->pixel_height);

  XLIKE_FLUSH (dpy);

#ifdef HAVE_SELECT
  {
    int usecs = 100000;
    struct timeval tv;
    tv.tv_sec  = usecs / 1000000L;
    tv.tv_usec = usecs % 1000000L;
    /* I'm sure someone is going to complain about this... */
    select (0, 0, 0, 0, &tv);
  }
#else
#ifdef HAVE_POLL
  poll (0, 0, 100);
#else /* !HAVE_POLL */
#error bite me
#endif /* HAVE_POLL */
#endif /* HAVE_SELECT */

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			    w->pixel_width, flash_height);
      XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left,
			    w->pixel_top + w->pixel_height - flash_height,
			    w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XLIKE_FILL_RECTANGLE (dpy, win, gc, w->pixel_left, w->pixel_top,
			  w->pixel_width, w->pixel_height);

  XLIKE_FLUSH (dpy);

  return 1;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_redisplay_XLIKE (void)
{
  /* redisplay methods */
  XLIKE_CONSOLE_HAS_METHOD (text_width);
  XLIKE_CONSOLE_HAS_METHOD (output_display_block);
  XLIKE_CONSOLE_HAS_METHOD (divider_height);
  XLIKE_CONSOLE_HAS_METHOD (eol_cursor_width);
  XLIKE_CONSOLE_HAS_METHOD (output_vertical_divider);
  XLIKE_CONSOLE_HAS_METHOD (clear_region);
  XLIKE_CONSOLE_HAS_METHOD (clear_frame);
  XLIKE_CONSOLE_HAS_METHOD (flash);
  XLIKE_CONSOLE_HAS_METHOD (ring_bell);
  XLIKE_CONSOLE_HAS_METHOD (bevel_area);
  XLIKE_CONSOLE_HAS_METHOD (output_string);
  XLIKE_CONSOLE_HAS_METHOD (output_pixmap);

#ifdef THIS_IS_X
  XLIKE_CONSOLE_HAS_METHOD (window_output_begin);
  XLIKE_CONSOLE_HAS_METHOD (window_output_end);
#endif
}
