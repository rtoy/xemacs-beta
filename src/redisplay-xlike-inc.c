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
#include "text.h"

#ifdef MULE
#include "mule-ccl.h"
#endif
#include "charset.h"

#define NEED_GCCACHE_H
#define NEED_GLYPHS_H
#define NEED_OBJECTS_IMPL_H

#if defined (HAVE_GTK) && !defined (THIS_IS_GTK)
#define THIS_IS_GTK
#endif
#include "console-xlike-inc.h"

#include "sysproc.h" /* for select() */

#ifdef THIS_IS_X
#include "EmacsFrame.h"
#include "EmacsFrameP.h"

#include <X11/bitmaps/gray>
#endif /* THIS_IS_X */

#ifdef THIS_IS_GTK
static void cr_set_foreground (cairo_t *cr, Lisp_Object color);
static void gtk_draw_rectangle (cairo_t *cr, gint x, gint y,
                                gint width, gint height);
static void gtk_fill_rectangle (cairo_t *cr, gint x, gint y,
                                gint width, gint height);
#endif

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
#define XLIKE_output_horizontal_line XFUN (output_horizontal_line)
#define XLIKE_output_eol_cursor XFUN (output_eol_cursor)
#define XLIKE_clear_frame_window XFUN (clear_frame_window)
#define XLIKE_clear_frame_windows XFUN (clear_frame_windows)
#define XLIKE_text_width XFUN (text_width)

static int XLIKE_text_width (struct frame *f, struct face_cachel *cachel,
                             const Ichar *str, Charcount len);
static void XLIKE_output_vertical_divider (struct window *w, int clear);
static void XLIKE_output_blank (struct window *w, struct display_line *dl,
				struct rune *rb, int start_pixpos,
				int cursor_start, int cursor_width);
static void XLIKE_output_horizontal_line (struct window *w,
					  struct display_line *dl,
					  struct rune *rb);
static void XLIKE_output_eol_cursor (struct window *w,
				     struct display_line *dl,
				     int xpos, face_index findex);
static void XLIKE_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x,
				       int y, int xoffset, int yoffset,
				       int width, int height,
				       XLIKE_COLOR fg, XLIKE_COLOR bg);
static void XLIKE_output_pixmap (struct window *w, Lisp_Object image_instance,
				struct display_box *db, struct display_glyph_area *dga,
				face_index findex, int cursor_start, int cursor_width,
				int cursor_height, int bg_pixmap);
static void XLIKE_clear_frame_windows (Lisp_Object window);
static void XLIKE_clear_frame (struct frame *f);
static void XLIKE_bevel_area (struct window *w, face_index findex,
			      int x, int y, int width, int height,
			      int shadow_thickness, int edges,
			      enum edge_style style);
static void XLIKE_ring_bell (struct device *d, int volume, int pitch,
			       int duration);
static void XLIKE_clear_region (Lisp_Object local, struct frame *f,
				face_index findex, int x, int y,
				int width, int height, Lisp_Object fg,
				Lisp_Object bg, Lisp_Object bg_pixmap,
				Lisp_Object bg_placement);
static int XLIKE_flash (struct device *d);

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
  Extbyte *ptr;
  int len;
  int dimension;
};

/* Separate out the text in STR of length LEN into a series of runs, stored in
   RUN_STORAGE.  RUN_STORAGE is guaranteed to hold enough space for all runs
   that could be generated from this text.  Each run points to the a stretch
   of text given simply by the position codes TEXT_STORAGE into a series of
   textual runs of a particular charset.  Also convert the characters as
   necessary into the format needed by XDrawImageString(),
   XDrawImageString16(), et al.  This means converting to one or two byte
   format, possibly tweaking the high bits, and possibly running a CCL
   program.  You must pre-allocate the space used and pass in a pointer,
   TEXT_STORAGE, and the ALLOCATE_RUNS_TEXT () macro is provided to do this
   using stack space. Some of the window-system-specific implementations take
   advantage of this step to do coding sytem translation, and the specific
   amount of space needed depends on the window system.

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

#if !defined(THIS_IS_GTK)
#if !defined(USE_XFT) && !defined(MULE)

#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)      \
    (storage = alloca_extbytes (len))
/* Not needed. */
#define ENSURE_CACHEL_COMPLETE (cachel, str, len) (void)

static Elemcount
separate_textual_runs_nomule (Extbyte *text_storage,
			      struct textual_run *run_storage,
			      const Ibyte *str, Bytecount len,
 			      struct face_cachel *UNUSED (cachel))
{
  if (!len)
    return 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 1;
  run_storage[0].charset = Qnil;

  memcpy (text_storage, str, len);

  return 1;
}
#endif

#ifdef USE_XFT
#ifdef WORDS_BIGENDIAN
#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)       \
       TO_EXTERNAL_FORMAT (DATA, (str, len),                     \
                           ALLOCA, (storage, storage_len),       \
                           Qutf_16)
#else
extern Lisp_Object Qutf_16_little_endian;
#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)       \
       TO_EXTERNAL_FORMAT (DATA, (str, len),                     \
                           ALLOCA, (storage, storage_len),       \
                           Qutf_16_little_endian)
#endif
#endif /* USE_XFT */

#if defined(USE_XFT) && !defined(MULE)

/*
  Note that in this configuration the "Croatian hack" of using an 8-bit,
  non-Latin-1 font to get localized display without Mule simply isn't
  available.  That's by design -- Unicode does not aid or abet that kind
  of punning.
  This means that the cast to XftChar16 gives the correct "conversion" to
  UCS-2. */
static Elemcount
separate_textual_runs_xft_nomule (Extbyte *text_storage,
				  struct textual_run *run_storage,
				  const Ibyte *str, Bytecount len,
				  struct face_cachel *UNUSED (cachel))
{
  int i;
  if (!len)
    return 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 2;
  run_storage[0].charset = Qnil;

  return 1;
}
#endif

#if defined(USE_XFT) && defined(MULE)

extern Lisp_Object Qutf_16_little_endian;

#ifdef WORDS_BIGENDIAN
#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)       \
       TO_EXTERNAL_FORMAT (DATA, (str, len),                     \
                           ALLOCA, (storage, storage_len),       \
                           Qutf_16)
#else
#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)       \
       TO_EXTERNAL_FORMAT (DATA, (str, len),                     \
                           ALLOCA, (storage, storage_len),       \
                           Qutf_16_little_endian)
#endif

static Elemcount
separate_textual_runs_xft_mule (Extbyte *text_storage,
				struct textual_run *run_storage,
				const Ibyte *str, Bytecount len,
				struct face_cachel *UNUSED (cachel))
{
  Lisp_Object prev_charset = Qunbound;
  const Ibyte *end = str + len; 
  Elemcount runs_so_far = 0;

  run_storage[0].ptr = text_storage;
  run_storage[0].len = len;
  run_storage[0].dimension = 2;
  run_storage[0].charset = Qnil;

  while (str < end)
    {
      Ichar ch = itext_ichar (str);
      Lisp_Object charset = ichar_charset (ch);

      if (!EQ (charset, prev_charset))
	{
	  if (runs_so_far)
	    run_storage[runs_so_far-1].len
              = (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
	  run_storage[runs_so_far].ptr = text_storage;
	  run_storage[runs_so_far].dimension = 2;
	  run_storage[runs_so_far].charset = charset;
	  prev_charset = charset;
	  runs_so_far++;
	}

      if (valid_utf_16_first_surrogate (*((XftChar16 *) (text_storage))))
        {
          text_storage += sizeof (XftChar16);
        }
      text_storage += sizeof (XftChar16);
      INC_IBYTEPTR (str);
    }

  if (runs_so_far)
    run_storage[runs_so_far-1].len
      = (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
  return runs_so_far;
}
#endif

#if !defined(USE_XFT) && defined(MULE)

#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)      \
  (storage = alloca_extbytes ((storage_len = (2 * len))))

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
static Elemcount
separate_textual_runs_mule (Extbyte *text_storage,
                            struct textual_run *run_storage,
			    const Ibyte *str, Bytecount len,
			    struct face_cachel *cachel)
{
  Lisp_Object prev_charset = Qunbound, ccl_prog;
  int runs_so_far = 0;
  Ibyte charset_leading_byte = LEADING_BYTE_ASCII;
  const Ibyte *end = str + len;
  int dimension = 1, graphic = 0, need_ccl_conversion = 0;
  struct ccl_program char_converter;

  int translate_to_ucs_2 = 0;

  while (str < end)
    {
      Ichar ch = itext_ichar (str);
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
	     REPLACEMENT CHARACTER. */
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
	  /* Only do the ASCII optimization if the attributes of the
	     charset reflect the attributes of the normal ASCII set,
	     so that the user can override with translate_to_ucs_2 or
	     a CCL conversion if he or she so wishes. */
	  if (0 && charset_leading_byte == LEADING_BYTE_ASCII)
	    {
	      const Ibyte *nonascii;

	      nonascii = skip_ascii (str, end);
	      memcpy (text_storage, str, nonascii - str);
	      text_storage += nonascii - str;
	      str = nonascii;
	      continue;
	    }

	  byte1 &= 0x7F;
	  byte2 &= 0x7F;
	}
      else
	{
	  byte1 |= 0x80;
	  byte2 |= 0x80;
	}

      *text_storage++ = (Extbyte) byte1;
      if (2 == dimension)
	{
	  *text_storage++ = (Extbyte) byte2;
	}

      INC_IBYTEPTR (str);
    }

  if (runs_so_far)
    {
      run_storage[runs_so_far - 1].len =
	text_storage - run_storage[runs_so_far - 1].ptr;
      /* Dimension retains the relevant value for the run before it. */
      if (2 == dimension)
	run_storage[runs_so_far - 1].len >>= 1;
    }

  assert (runs_so_far < 0xFFFF);

  return runs_so_far;
}
#endif

static int
separate_textual_runs (Extbyte *text_storage,
		       struct textual_run *run_storage,
		       const Ibyte *str, Bytecount len,
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

/* !THIS_IS_GTK */
#endif

/****************************************************************************/
/*                                                                          */
/*                           Xlike output routines                          */
/*                                                                          */
/****************************************************************************/
#ifndef THIS_IS_GTK
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

      assert (run->dimension == 2);

      XftTextExtents16 (dpy, FONT_INSTANCE_X_XFTFONT (fi),
                        (XftChar16 *) run->ptr, run->len, &glyphinfo);
    
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
#endif

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
  Ibyte *buffer, *bufp;
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
    {
      charset = XLIKE_ICHAR_CHARSET (rb->object.chr.ch);
    }

  if (end < 0)
    end = Dynarr_length (rba);

  buffer = bufp = alloca_ibytes (end * MAX_ICHAR_LEN);

  while (elt < end)
    {
      rb = Dynarr_atp (rba, elt);

      if (rb->findex == findex && rb->type == RUNE_CHAR
	  && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
	  && EQ (charset, XLIKE_ICHAR_CHARSET (rb->object.chr.ch)))
	{
          bufp += set_itext_ichar (bufp, rb->object.chr.ch);
	  width += rb->width;
	  elt++;
	}
      else
	{
	  if (bufp - buffer)
	    {
	      XLIKE_output_string (w, dl, buffer, bufp - buffer, xpos, 0,
                                   start_pixpos, width,
				   findex, 0, cursor_start, cursor_width,
				   cursor_height);
	      xpos = rb->xpos;
	      width = 0;
              bufp = buffer;
	    }
	  width = 0;

	  if (rb->type == RUNE_CHAR)
	    {
	      findex = rb->findex;
	      xpos = rb->xpos;
	      charset = XLIKE_ICHAR_CHARSET (rb->object.chr.ch);

	      if (rb->cursor_type == CURSOR_ON)
		{
		  if (rb->object.chr.ch == '\n')
		    {
		      XLIKE_output_eol_cursor (w, dl, xpos, findex);
		    }
		  else
		    {
		      XLIKE_output_string (w, dl, buffer, 
                                           set_itext_ichar (buffer,
                                                            rb->object.chr.ch),
                                           xpos, 0, start_pixpos,
					   rb->width, findex, 1,
					   cursor_start, cursor_width,
					   cursor_height);
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

                        XLIKE_output_string (w, dl, XSTRING_DATA (string),
                                             XSTRING_LENGTH (string), xpos,
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

  if (bufp - buffer)
    XLIKE_output_string (w, dl, buffer, bufp - buffer, xpos, 0, start_pixpos,
                         width, findex, 0, cursor_start, cursor_width,
                         cursor_height);

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
}

#ifdef THIS_IS_X

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
#ifndef HAVE_GTK
      gcv.font =
	XLIKE_FONT_NUM (FONT_INSTANCE_XLIKE_FONT (XFONT_INSTANCE (font)));
      mask |= XLIKE_GC_FONT;
#endif
    }

  /* evil kludge! */
  if (!NILP (fg) && !COLOR_INSTANCEP (fg) && !FIXNUMP (fg))
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
	XLIKE_SET_GC_PIXEL (gcv.foreground, XFIXNUM (fg));
      mask |= XLIKE_GC_FOREGROUND;
    }

  if (!NILP (bg))
    {
      if (COLOR_INSTANCEP (bg))
	XLIKE_SET_GC_COLOR (gcv.background, XCOLOR_INSTANCE_XLIKE_COLOR (bg));
      else
	XLIKE_SET_GC_PIXEL (gcv.background, XFIXNUM (bg));
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
#ifdef THIS_IS_X
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
#endif
  if (!NILP (lwidth))
    {
      gcv.line_width = XFIXNUM (lwidth);
      mask |= XLIKE_GC_LINE_WIDTH;
    }

#if 0
  debug_out ("\nx_get_gc: calling gc_cache_lookup\n");
#endif
  return gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, mask);
}

#endif

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
#ifndef THIS_IS_GTK
void
XLIKE_output_string (struct window *w, struct display_line *dl,
		     const Ibyte *buf, Bytecount len,
                     int xpos, int xoffset, int clip_start,
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
  Extbyte *text_storage;
  struct textual_run *runs = alloca_array (struct textual_run, len);
  int nruns, text_storage_len;
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
  xft_convert_color_1 (dpy, cmap, visual,					\
		     &(XCOLOR_INSTANCE_X_COLOR (color)), (dim))
#endif /* USE_XFT */

  if (width < 0)
    width = XLIKE_text_width (f, cachel, buf, len);

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

  ALLOCATE_RUNS_TEXT (text_storage, text_storage_len, buf, len);

  nruns = separate_textual_runs (text_storage, runs, buf, len, cachel);

  USED (text_storage_len);

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
	      XCreateBitmapFromData (dpy, x_win, (char *)gray_bits,
				     gray_width, gray_height);

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
		int rect_width
		  = XLIKE_text_width_single_run (f, cachel, run);
#ifndef USE_XFTTEXTENTS_TO_AVOID_FONT_DROPPINGS
		int rect_height = FONT_INSTANCE_ASCENT (fi)
				  + FONT_INSTANCE_DESCENT (fi) + 1;
#else
		int rect_height = FONT_INSTANCE_ASCENT (fi)
				  + FONT_INSTANCE_DESCENT (fi);
		XGlyphInfo gi;

                XftTextExtents16 (dpy, FONT_INSTANCE_X_XFTFONT (fi),
                                  (const XftChar16 *) run->ptr, run->len,
                                  &gi);
		rect_height = max (rect_height, gi.height);
#endif

		XftDrawRect (xftDraw, &bg,
			     xpos, ypos, rect_width, rect_height);
	      }
	
	    assert (runs[i].dimension == 2);

            XftDrawString16 (xftDraw, &fg, rf, xpos, dl->ypos,
                             (const XftChar16 *) runs[i].ptr, runs[i].len);
	  }
      }
#endif /* USE_XFT */

      if (use_x_font)
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

	  if (runs[i].dimension == 1)
	    (bgc ? XDrawString : XDrawImageString)
	      (dpy, x_win, gc, xpos, dl->ypos,
	       (char *) runs[i].ptr, runs[i].len);
	  else
	    (bgc ? XDrawString16 : XDrawImageString16)
	      (dpy, x_win, gc, xpos, dl->ypos,
	       (XChar2b *) runs[i].ptr, runs[i].len);
	}

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
		int rect_width
		  = XLIKE_text_width_single_run (f, cachel, &runs[i]);
		XftColor xft_color;

		xft_color = XFT_FROB_LISP_COLOR (cursor_cachel->background, 0);
		XftDrawRect (xftDraw, &xft_color,
			     xpos, ypos, rect_width, rect_height);

		xft_color = XFT_FROB_LISP_COLOR (cursor_cachel->foreground, 0);
		assert (runs[i].dimension == 2);

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
	      if (runs[i].dimension == 1)
		XDrawImageString (dpy, x_win, cgc, xpos, dl->ypos,
				  (char *) runs[i].ptr, runs[i].len);
	      else
		XDrawImageString16 (dpy, x_win, cgc, xpos, dl->ypos,
				    (XChar2b *) runs[i].ptr, runs[i].len);

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
/* not THIS_IS_GTK */
#endif

static void
XLIKE_output_pixmap (struct window *w, Lisp_Object image_instance,
		     struct display_box *db, struct display_glyph_area *dga,
		     face_index findex, int cursor_start, int cursor_width,
		     int cursor_height, int UNUSED (bg_pixmap))
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
#ifdef THIS_IS_X
  XLIKE_DISPLAY dpy = GET_XLIKE_X_DISPLAY (d);
  XLIKE_WINDOW x_win = GET_XLIKE_WINDOW (f);
#endif

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
			       tmp_fcolor, tmp_bcolor);
  }

  /* Draw a cursor over top of the pixmap. */
  if (cursor_width && cursor_height && (cursor_start >= db->xpos)
      && !NILP (w->text_cursor_visible_p)
      && (cursor_start < db->xpos + dga->width))
    {
      int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
      struct face_cachel *cursor_cachel =
	WINDOW_FACE_CACHEL (w,
			    get_builtin_face_cache_index
			    (w, Vtext_cursor_face));
      if (cursor_width > db->xpos + dga->width - cursor_start)
	cursor_width = db->xpos + dga->width - cursor_start;

#ifdef THIS_IS_X
      {
	XLIKE_GC gc;

	gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil,
			   Qnil);

	if (focus)
	  {
	    XLIKE_FILL_RECTANGLE (dpy, x_win, gc, cursor_start, db->ypos,
				  cursor_width, cursor_height);
	  }
	else
	  {
	    XLIKE_DRAW_RECTANGLE (dpy, x_win, gc, cursor_start, db->ypos,
				  cursor_width, cursor_height);
	  }
      }
#endif
#ifdef THIS_IS_GTK
      {
	GtkWidget *widget = FRAME_GTK_TEXT_WIDGET (f);
	GdkWindow *window = gtk_widget_get_window (widget);
#if GTK_CHECK_VERSION(3, 22, 0)
	cairo_region_t *region = gdk_window_get_visible_region (window);
	GdkDrawingContext *ctx = gdk_window_begin_draw_frame (window,
							      region);
        cairo_t *cr = gdk_drawing_context_get_cairo_context (ctx);
#else
	cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (widget));
#endif

	cr_set_foreground (cr, cursor_cachel->background);

	if (focus)
	  {
	    gtk_fill_rectangle (cr, cursor_start, db->ypos,
				cursor_width, cursor_height);
	  }
	else
	  {
	    gtk_draw_rectangle (cr, cursor_start, db->ypos,
				cursor_width, cursor_height);
	  }
#if GTK_CHECK_VERSION(3, 22, 0)
	gdk_window_end_draw_frame (gtk_widget_get_window (widget), ctx);
	cairo_region_destroy (region);
#else
	cairo_destroy (cr);
#endif
      }
#endif
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
