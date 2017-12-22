/* X output and frame manipulation routines.
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

/* Author: Chuck Thompson */

/* Lots of work done by Ben Wing for Mule */


/* Number of pixels below each line. */
int x_interline_space; /* #### this needs to be implemented, but per-font */

#define THIS_IS_X
#include "redisplay-xlike-inc.c"

/****************************************************************************/
/*                                                                          */
/*                           Separate textual runs                          */
/*                                                                          */
/****************************************************************************/


/* Note: We do not use the Xmb*() functions and XFontSets, nor the Motif
   XFontLists and CompoundStrings.  Those functions are generally losing for a
   number of reasons.  Most important, they only support one locale (e.g. you
   could display Japanese and ASCII text, but not mixed Japanese/Chinese
   text).  You could maybe call setlocale() frequently to try to deal with
   this, but that would generally fail because an XFontSet is tied to one
   locale and won't have the other character sets in it.

   fontconfig (the font database for Xft) has some specifier-like properties,
   but it's not sufficient (witness the existence of Pango).  Pango might do
   the trick, but it's not a cross-platform solution; it would need
   significant advantages to be worth the effort. */

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

#if !defined (USE_XFT) && !defined (MULE)

#define ALLOCATE_RUNS_TEXT(storage, storage_len, str, len)      \
    (storage = alloca_extbytes (len))

static int
separate_textual_runs_nomule (struct buffer * UNUSED (buf),
			      Extbyte *text_storage,
			      struct textual_run *run_storage,
                              const Ibyte *str, Bytecount len,
			      struct face_cachel *UNUSED(cachel))
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

#if defined (USE_XFT) && !defined (MULE)
/*
  Note that in this configuration the "Croatian hack" of using an 8-bit,
  non-Latin-1 font to get localized display without Mule simply isn't
  available.  That's by design -- Unicode does not aid or abet that kind
  of punning.
  This means that the cast to XftChar16 gives the correct "conversion" to
  UCS-2. */
static int
separate_textual_runs_xft_nomule (struct buffer * UNUSED (buf),
				  Extbyte *text_storage,
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

#if defined (USE_XFT) && defined (MULE)
static int
separate_textual_runs_xft_mule (struct buffer *buf,
				Extbyte *text_storage,
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
      Lisp_Object charset;
      int byte1, byte2;

      /* @@#### This use of CONVERR_SUBSTITUTE is somewhat bogus.
	 It will substitute a '?' if we can't convert.  Not clear whether
	 this will work or not.  Problem is that we really shouldn't
	 be doing things on a charset level. */
      buffer_ichar_to_charset_codepoint (ch, buf, &charset, &byte1, &byte2,
					 CONVERR_SUBSTITUTE);

      if (!EQ (charset, prev_charset))
	{
	  if (runs_so_far)
	    run_storage[runs_so_far-1].len =
              (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
	  run_storage[runs_so_far].ptr = text_storage;
	  run_storage[runs_so_far].dimension = 2;
	  run_storage[runs_so_far].charset = charset;
	  prev_charset = charset;
	  runs_so_far++;
	}

      if (valid_unicode_leading_surrogate (*((XftChar16 *) (text_storage))))
        {
          text_storage += sizeof (XftChar16);
        }
      text_storage += sizeof (XftChar16);
      INC_IBYTEPTR (str);
    }

  if (runs_so_far)
    run_storage[runs_so_far-1].len =
      (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
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
separate_textual_runs_mule (struct buffer *buf,
                            Extbyte *text_storage,
                            struct textual_run *run_storage,
			    const Ibyte *str, Bytecount len,
			    struct face_cachel *cachel)
{
  Lisp_Object prev_charset = Qunbound, ccl_prog;
  int runs_so_far = 0;
  const Ibyte *end = str + len;
  int dimension = 1, need_ccl_conversion = 0;
  struct ccl_program char_converter;

  int translate_to_ucs_2 = 0;

  while (str < end)
    {
      Ichar ch = itext_ichar (str);
      Lisp_Object charset;
      int byte1, byte2;

      buffer_ichar_to_charset_codepoint (ch, buf, &charset, &byte1, &byte2,
					 CONVERR_FAIL);

      /* If we can't convert, substitute a '~' (CANT_DISPLAY_CHAR). */
      /* @@#### This is extremely bogus.  We want it to substitute the
	 Unicode replacement character, but there's no charset for this.
	 We really shouldn't be doing things on a charset level. */
      if (NILP (charset))
	{
	  charset = Vcharset_ascii;
	  byte1 = 0;
	  byte2 = CANT_DISPLAY_CHAR;
	}

      /* NOTE: Formerly we used to retrieve the XCHARSET_GRAPHIC() here
	 and use it below to determine whether to push the bytes into
	 the 128-255 range.  This is now handled automatically by the
	 `offset' property of charsets, which should agree with `graphic'.
         Logically, the `offset' property describes the numeric indices
	 of the characters, such as when they are used to index an array
	 or font, while the `graphic' property indicates which register
	 to select when encoding using iso2022. */

      if (!EQ (charset, prev_charset))
	{
	  int offs;

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
	     These flags are almost mutually exclusive, but we're sloppy
	     about resetting "shadowed" flags.  So the flags must be checked
	     in the proper order in computing byte1 and byte2, below. */

	  offs = FACE_CACHEL_OFFSET_ENSURE (cachel, charset);

	  translate_to_ucs_2 = Stynarr_at (cachel->font_final_stage, offs);
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
	      /* Else, the charset must have an ISO 2022-compatible font index.
	       */
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
	  int ucs = ichar_to_unicode (ch, CONVERR_SUBSTITUTE);
	  /* If UCS is less than zero or greater than 0xFFFF, set ucs2 to
	     REPLACEMENT CHARACTER. */
	  ucs = (ucs & ~0xFFFF) ? UNICODE_REPLACEMENT_CHAR : ucs;

	  byte1 = ucs >> 8;
	  byte2 = ucs & 0xFF;
	}
      else if (need_ccl_conversion)
	{
	  internal_to_external_charset_codepoint (charset, byte1, byte2,
						  &byte1, &byte2, 1);
	  char_converter.reg[0] = XCHARSET_ID (charset);
	  char_converter.reg[1] = byte1;
	  char_converter.reg[2] = byte2;
	  ccl_driver (&char_converter, 0, buf, 0, 0, 0, CCL_MODE_ENCODING);
	  byte1 = char_converter.reg[1];
	  byte2 = char_converter.reg[2];
	}
      else if (EQ (charset, Vcharset_ascii) && byte2 != CANT_DISPLAY_CHAR)
	{
	  const Ibyte *nonascii;

	  nonascii = skip_ascii (str, end);
	  memcpy (text_storage, str, nonascii - str);
	  text_storage += nonascii - str;
	  str = nonascii;
	  continue;
	}

      if (2 == dimension)
	{
	  *text_storage++ = (Extbyte) byte1;
	}

      *text_storage++ = (Extbyte) byte2;
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
separate_textual_runs (struct buffer *buf,
                       Extbyte *text_storage,
		       struct textual_run *run_storage,
		       const Ibyte *str, Bytecount len,
		       struct face_cachel *cachel)
{
#if defined (USE_XFT) && defined (MULE)
  return separate_textual_runs_xft_mule (buf, text_storage, run_storage,
					 str, len, cachel);
#endif
#if defined (USE_XFT) && !defined (MULE)
  return separate_textual_runs_xft_nomule (buf, text_storage, run_storage,
					   str, len, cachel);
#endif
#if !defined (USE_XFT) && defined (MULE)
  return separate_textual_runs_mule (buf, text_storage, run_storage,
				     str, len, cachel);
#endif
#if !defined (USE_XFT) && !defined (MULE)
  return separate_textual_runs_nomule (buf, text_storage, run_storage,
				       str, len, cachel);
#endif
}

/****************************************************************************/
/*                                                                          */
/*                           X output routines                              */
/*                                                                          */
/****************************************************************************/

static int
x_text_width_single_run (Display * USED_IF_XFT (dpy),
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
      XGlyphInfo glyphinfo;

      assert (run->dimension == 2);

      XftTextExtents16 (dpy, FONT_INSTANCE_X_XFTFONT (fi),
                        (XftChar16 *) run->ptr, run->len, &glyphinfo);
    
      return glyphinfo.xOff;
    }
#endif
  else if (FONT_INSTANCE_X_FONT (fi))
    {
      if (run->dimension == 2)
	{	
	  /* stderr_out ("Measuring wide characters\n"); */
	  return XTextWidth16 (FONT_INSTANCE_X_FONT (fi),
                               (XChar2b *) (run->ptr), run->len);
	}
      else
	{
	  return XTextWidth (FONT_INSTANCE_X_FONT (fi), run->ptr, run->len);
	}
    }
  else
    abort();
  return 0;			/* shut up GCC */
}

/* x_text_width

   Given a string and a merged face, return the string's length in pixels
   when displayed in the fonts associated with the face. */

static int
x_text_width (struct frame *f, struct face_cachel *cachel, const Ibyte *str,
              Bytecount len)
{
  Extbyte *text_storage = NULL;
  int width_so_far = 0, text_storage_len = 0;
  struct textual_run *runs = alloca_array (struct textual_run, len);
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (f->device));
  int nruns;
  int i;

  ALLOCATE_RUNS_TEXT (text_storage, text_storage_len, str, len);
  nruns = separate_textual_runs (WINDOW_XBUFFER (FRAME_SELECTED_XWINDOW (f)),
                                 text_storage, runs, str, len, cachel);

  USED (text_storage_len);

  for (i = 0; i < nruns; i++)
    width_so_far += x_text_width_single_run (dpy, cachel, runs + i);

  return width_so_far;
}

static void x_output_shadows (struct frame *f, int x, int y, int width,
			      int height, GC top_shadow_gc,
			      GC bottom_shadow_gc, GC background_gc,
			      int shadow_thickness, int edges);

/*****************************************************************************
 x_window_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void
x_window_output_begin (struct window *UNUSED (w))
{
}

/*****************************************************************************
 x_window_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void
x_window_output_end (struct window *w)
{
  if (!(check_if_pending_expose_event (WINDOW_XDEVICE (w))))
    XFlush (DEVICE_X_DISPLAY (WINDOW_XDEVICE (w)));
}

/****************************************************************************
 x_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
****************************************************************************/
static void
x_clear_region (Lisp_Object UNUSED (locale), struct frame* f,
		    face_index UNUSED (findex),
		    int x, int y, int width, int height,
		    Lisp_Object fcolor, Lisp_Object bcolor,
		    Lisp_Object background_pixmap,
		    Lisp_Object background_placement)
{
  Display *dpy =  DEVICE_X_DISPLAY (XDEVICE (f->device));
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc = NULL;

  if (!UNBOUNDP (background_pixmap))
    {
      gc = x_get_gc (f, Qnil, fcolor, bcolor, background_pixmap,
                     background_placement, Qnil);
      XFillRectangle (dpy, x_win, gc, x, y, width, height);
    }
  else
    {
      XClearArea (dpy, x_win, x, y, width, height, False);
    }
}

static void
x_clear_frame (struct frame *f)
{
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (f->device));
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
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

  XClearArea (dpy, x_win, x, y, width, height, False);

  frame = wrap_frame (f);

  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      XLIKE_clear_frame_windows (f->root_window);
    }
  {
    struct device *d = XDEVICE (f->device);
    if (!(check_if_pending_expose_event (d)))
      XFlush (DEVICE_X_DISPLAY (d));
  }
}


/*****************************************************************************
 x_bevel_area

 Draw shadows for the given area in the given face.
 ****************************************************************************/
static void
x_bevel_area (struct window *w, face_index findex,
		  int x, int y, int width, int height,
		  int shadow_thickness, int edges, enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  Pixel top_shadow_pixel, bottom_shadow_pixel, background_pixel;
  Lisp_Object tmp_pixel;
  XColor tmp_color;
  XGCValues gcv;
  GC top_shadow_gc, bottom_shadow_gc, background_gc;

  int use_pixmap = 0;
  int flip_gcs = 0;
  unsigned long mask;

  assert (shadow_thickness >=0);
  memset (&gcv, ~0, sizeof (XGCValues));

  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
  tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));

  /* First, get the GC's. */
  top_shadow_pixel = tmp_color.pixel;
  bottom_shadow_pixel = tmp_color.pixel;
  background_pixel = tmp_color.pixel;

  x_generate_shadow_pixels (f, &top_shadow_pixel, &bottom_shadow_pixel,
			    background_pixel, ef->core.background_pixel);

  tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
  tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
  gcv.background = tmp_color.pixel;
  gcv.graphics_exposures = False;
  mask = GCForeground | GCBackground | GCGraphicsExposures;

  /* If we can't distinguish one of the shadows (the color is the same as the
     background), it's better to use a pixmap to generate a dithered gray. */
  if (top_shadow_pixel == background_pixel ||
      bottom_shadow_pixel == background_pixel)
    use_pixmap = 1;

  if (use_pixmap)
    {
      if (DEVICE_X_GRAY_PIXMAP (d) == None)
	{
	  DEVICE_X_GRAY_PIXMAP (d) =
	    XCreatePixmapFromBitmapData (dpy, x_win, (char *) gray_bits,
					 gray_width, gray_height, 1, 0, 1);
	}

      tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
      tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
      gcv.foreground = tmp_color.pixel;
      /* this is needed because the GC draws with a pixmap here */
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple = DEVICE_X_GRAY_PIXMAP (d);
      top_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv,
				       (mask | GCStipple | GCFillStyle));

      tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
      tmp_color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (tmp_pixel));
      bottom_shadow_pixel = tmp_color.pixel;

      flip_gcs = (bottom_shadow_pixel ==
		  WhitePixelOfScreen (DefaultScreenOfDisplay (dpy)));
    }
  else
    {
      gcv.foreground = top_shadow_pixel;
      top_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);
    }

  gcv.foreground = bottom_shadow_pixel;
  bottom_shadow_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);

  if (use_pixmap && flip_gcs)
    {
      GC tmp_gc = bottom_shadow_gc;
      bottom_shadow_gc = top_shadow_gc;
      top_shadow_gc = tmp_gc;
    }

  gcv.foreground = background_pixel;
  background_gc = gc_cache_lookup (DEVICE_X_GC_CACHE (d), &gcv, mask);

  /* possibly revert the GC's This will give a depressed look to the
     divider */
  if (style == EDGE_ETCHED_IN || style == EDGE_BEVEL_IN)
    {
      GC temp;

      temp = top_shadow_gc;
      top_shadow_gc = bottom_shadow_gc;
      bottom_shadow_gc = temp;
    }

  if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT)
    shadow_thickness /= 2;

  /* Draw the shadows around the divider line */
  x_output_shadows (f, x, y, width, height,
		    top_shadow_gc, bottom_shadow_gc,
		    background_gc, shadow_thickness, edges);

  if (style == EDGE_ETCHED_IN || style == EDGE_ETCHED_OUT)
    {
      /* Draw the shadows around the divider line */
      x_output_shadows (f, x + shadow_thickness, y + shadow_thickness,
			width - 2*shadow_thickness, height - 2*shadow_thickness,
			bottom_shadow_gc, top_shadow_gc,
			background_gc, shadow_thickness, edges);
    }
}

/*****************************************************************************
 x_output_shadows

 Draw a shadow around the given area using the given GC's.  It is the
 callers responsibility to set the GC's appropriately.
 ****************************************************************************/
static void
x_output_shadows (struct frame *f, int x, int y, int width, int height,
		  GC top_shadow_gc, GC bottom_shadow_gc,
		  GC UNUSED (background_gc), int shadow_thickness, int edges)
{
  struct device *d = XDEVICE (f->device);

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));

  XSegment top_shadow[20], bottom_shadow[20];
  int elt;

  if (shadow_thickness > 10)
    shadow_thickness = 10;
  else if (shadow_thickness < 0)
    shadow_thickness = 0;
  if (shadow_thickness > (width / 2))
    shadow_thickness = width / 2;
  if (shadow_thickness > (height / 2))
    shadow_thickness = height / 2;

  for (elt = 0; elt < shadow_thickness; elt++)
    {
      int seg1 = elt;
      int seg2 = (edges & EDGE_TOP) ? elt + shadow_thickness : elt;
      int bot_seg2 = (edges & EDGE_BOTTOM) ? elt + shadow_thickness : elt;

      if (edges & EDGE_TOP)
	{
	  top_shadow[seg1].x1 = x + elt;
	  top_shadow[seg1].x2 = x + width - elt - 1;
	  top_shadow[seg1].y1 = top_shadow[seg1].y2 = y + elt;
	}
      if (edges & EDGE_LEFT)
	{
	  top_shadow[seg2].x1 = top_shadow[seg2].x2 = x + elt;
	  top_shadow[seg2].y1 = y + elt;
	  top_shadow[seg2].y2 = y + height - elt - 1;
	}
      if (edges & EDGE_BOTTOM)
	{
	  bottom_shadow[seg1].x1 = x + elt;
	  bottom_shadow[seg1].x2 = x + width - elt - 1;
	  bottom_shadow[seg1].y1 = bottom_shadow[seg1].y2 = y + height - elt - 1;
	}
      if (edges & EDGE_RIGHT)
	{
	  bottom_shadow[bot_seg2].x1 = bottom_shadow[bot_seg2].x2 = x + width - elt - 1;
	  bottom_shadow[bot_seg2].y1 = y + elt;
	  bottom_shadow[bot_seg2].y2 = y + height - elt - 1;
	}
    }

  XDrawSegments (dpy, x_win, top_shadow_gc, top_shadow,
		 ((edges & EDGE_TOP) ? shadow_thickness : 0)
		 + ((edges & EDGE_LEFT) ? shadow_thickness : 0));
  XDrawSegments (dpy, x_win, bottom_shadow_gc, bottom_shadow,
		 ((edges & EDGE_BOTTOM) ? shadow_thickness : 0)
		 + ((edges & EDGE_RIGHT) ? shadow_thickness : 0));
}

/*****************************************************************************
 x_generate_shadow_pixels

 Given three pixels (top shadow, bottom shadow, background) massage
 the top and bottom shadow colors to guarantee that they differ.  The
 background pixels are not allowed to be modified.

 This function modifies its parameters.

 This code is modified from code blatantly stolen from lwlib/xlwmenu.c
 ****************************************************************************/
#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

void
x_generate_shadow_pixels (struct frame *f, unsigned long *top_shadow,
			  unsigned long *bottom_shadow,
			  unsigned long background,
			  unsigned long core_background)
{
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Colormap cmap = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL (d);

  XColor topc, botc;
  int top_frobbed = 0, bottom_frobbed = 0;

  /* If the top shadow is the same color as the background, try to
     adjust it. */
  if (*top_shadow == background)
    {
      topc.pixel = background;
      XQueryColor (dpy, cmap, &topc);
      /* don't overflow/wrap! */
      topc.red   = MINL (65535, (unsigned long) topc.red   * 6 / 5);
      topc.green = MINL (65535, (unsigned long) topc.green * 6 / 5);
      topc.blue  = MINL (65535, (unsigned long) topc.blue  * 6 / 5);
      if (x_allocate_nearest_color (dpy, cmap, visual, &topc))
	{
	  *top_shadow = topc.pixel;
	  top_frobbed = 1;
	}
    }

  /* If the bottom shadow is the same color as the background, try to
     adjust it. */
  if (*bottom_shadow == background)
    {
      botc.pixel = background;
      XQueryColor (dpy, cmap, &botc);
      botc.red   = (unsigned short) ((unsigned long) botc.red   * 3 / 5);
      botc.green = (unsigned short) ((unsigned long) botc.green * 3 / 5);
      botc.blue  = (unsigned short) ((unsigned long) botc.blue  * 3 / 5);
      if (x_allocate_nearest_color (dpy, cmap, visual, &botc))
	{
	  *bottom_shadow = botc.pixel;
	  bottom_frobbed = 1;
	}
    }

  /* If we had to adjust both shadows, then we have to do some
     additional work. */
  if (top_frobbed && bottom_frobbed)
    {
      int top_avg = ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
      int bot_avg = ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
      if (bot_avg > top_avg)
	{
	  Pixel tmp = *top_shadow;

	  *top_shadow = *bottom_shadow;
	  *bottom_shadow = tmp;
	}
      else if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == background)
	    *top_shadow = core_background;
	  else
	    *bottom_shadow = background;
	}
    }
}

/*****************************************************************************
 x_output_horizontal_line

 Output a horizontal line in the foreground of its face.
****************************************************************************/
static void
x_output_horizontal_line (struct window *w, struct display_line *dl,
                          struct rune *rb)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc;
  int x = rb->xpos;
  int width = rb->width;
  int height = DISPLAY_LINE_HEIGHT (dl);
  int ypos1, ypos2, ypos3, ypos4;

  ypos1 = XLIKE_DISPLAY_LINE_YPOS (dl);
  ypos2 = ypos1 + rb->object.hline.yoffset;
  ypos3 = ypos2 + dl->ascent / 2;
  ypos4 = dl->ypos + dl->descent - dl->clip;

  /* First clear the area not covered by the line. Clear rectangles
     above and below the line. */
  if (height - rb->object.hline.thickness > 0)
    {
      gc = XLIKE_get_gc (f, Qnil,
			 WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
			 Qnil, Qnil, Qnil, Qnil);

      if (ypos2 - ypos1 > 0)
	XFillRectangle (dpy, x_win, gc, x, ypos1, width, ypos2 - ypos1);
      if (ypos4 - ypos3 > 0)
	XFillRectangle (dpy, x_win, gc, x, ypos3, width, ypos4 - ypos3);
    }

  /* Now draw the line. */
  gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		     Qnil, Qnil, Qnil, Qnil);

  if (ypos2 < ypos1)
    ypos2 = ypos1;
  if (ypos3 > ypos4)
    ypos3 = ypos4;

  if (ypos3 - ypos2 > 0)
    {
      XFillRectangle (dpy, x_win, gc, x, ypos3, width,
                      rb->object.hline.thickness);
    }
}

/*****************************************************************************
 x_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
****************************************************************************/
static void
x_output_vertical_divider (struct window *w, int USED_IF_X (clear))
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  Lisp_Object tmp_pixel;
  XGCValues gcv;
  GC background_gc;
  enum edge_style style;
  unsigned long mask;
  int x, ytop, ybottom, width, shadow_thickness, spacing, line_width;
  face_index div_face =
    get_builtin_face_cache_index (w, Vvertical_divider_face);

  width = window_divider_width (w);
  shadow_thickness = XFIXNUM (w->vertical_divider_shadow_thickness);
  spacing = XFIXNUM (w->vertical_divider_spacing);
  line_width = XFIXNUM (w->vertical_divider_line_width);
  x = WINDOW_RIGHT (w) - width;
  ytop = WINDOW_TOP (w);
  ybottom = WINDOW_BOTTOM (w);

  memset (&gcv, ~0, sizeof (gcv));

  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, div_face);

  /* First, get the GC's. */
  XLIKE_SET_GC_COLOR (gcv.background, XCOLOR_INSTANCE_XLIKE_COLOR (tmp_pixel));
  gcv.foreground = gcv.background;
  gcv.graphics_exposures = False;
  mask = XLIKE_GC_FOREGROUND | XLIKE_GC_BACKGROUND | XLIKE_GC_EXPOSURES;

  background_gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (d), &gcv, mask);

  /* Clear the divider area first.  This needs to be done when a
     window split occurs. */
  if (clear)
    XClearArea (dpy, x_win, x, ytop, width, ybottom - ytop, False);

  /* Draw the divider line. */
  XFillRectangle (dpy, x_win, background_gc, x + spacing + shadow_thickness,
                  ytop, line_width, ybottom - ytop);
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
  XLIKE_bevel_area (w, div_face, x + spacing, ytop,
		    width - 2 * spacing, ybottom - ytop,
		    shadow_thickness, EDGE_ALL, style);
}

/* Make audible bell.  */

static void
x_ring_bell (struct device *d, int volume, int pitch, int duration)
{
  Display *display = DEVICE_X_DISPLAY (d);

  if (volume < 0) volume = 0;
  else if (volume > 100) volume = 100;
  if (pitch < 0 && duration < 0)
    {
      XBell (display, (volume * 2) - 100);
      XFlush (display);
    }
  else
    {
      XKeyboardState state;
      XKeyboardControl ctl;
      XSync (display, 0);
      /* #### grab server? */
      XGetKeyboardControl (display, &state);

      ctl.bell_pitch    = (pitch    >= 0 ? pitch    : (int) state.bell_pitch);
      ctl.bell_duration = (duration >= 0 ? duration : (int) state.bell_duration);
      XChangeKeyboardControl (display, KBBellPitch|KBBellDuration, &ctl);

      XBell (display, (volume * 2) - 100);

      ctl.bell_pitch    = state.bell_pitch;
      ctl.bell_duration = state.bell_duration;
      XChangeKeyboardControl (display, KBBellPitch|KBBellDuration, &ctl);

      /* #### ungrab server? */
      XSync (display, 0);
    }
}

/* briefly swap the foreground and background colors.
 */
static int
x_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc = NULL;
  XGCValues gcv;
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
  gcv.graphics_exposures = False;
  gc = gc_cache_lookup (DEVICE_XLIKE_GC_CACHE (XDEVICE (f->device)), &gcv,
			XLIKE_GC_FOREGROUND | XLIKE_GC_FUNCTION | XLIKE_GC_EXPOSURES);
  default_face_width_and_height (frame, 0, &flash_height);

  /* If window is tall, flash top and bottom line.  */
  if (EQ (Vvisible_bell, Qtop_bottom) && w->pixel_height > 3 * flash_height)
    {
      XFillRectangle (dpy, win, gc, w->pixel_left, w->pixel_top,
                      w->pixel_width, flash_height);
      XFillRectangle (dpy, win, gc, w->pixel_left,
                      w->pixel_top + w->pixel_height - flash_height,
                      w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XFillRectangle (dpy, win, gc, w->pixel_left, w->pixel_top, w->pixel_width,
                    w->pixel_height);

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
      XFillRectangle (dpy, win, gc, w->pixel_left, w->pixel_top,
                      w->pixel_width, flash_height);
      XFillRectangle (dpy, win, gc, w->pixel_left,
                      w->pixel_top + w->pixel_height - flash_height,
                      w->pixel_width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XFillRectangle (dpy, win, gc, w->pixel_left, w->pixel_top, w->pixel_width,
                    w->pixel_height);

  XLIKE_FLUSH (dpy);

  return 1;
}

/*****************************************************************************
 x_output_blank

 Output a blank by clearing the area it covers in the foreground color
 of its face.
****************************************************************************/
static void
x_output_blank (struct window *w, struct display_line *dl, struct rune *rb,
                int start_pixpos, int cursor_start, int cursor_width)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc;
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
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
                       Qnil, Qnil, Qnil);
  else
    gc = XLIKE_get_gc (f, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		       bg_pmap,
		       WINDOW_FACE_CACHEL_BACKGROUND_PLACEMENT (w, rb->findex),
		       Qnil);

  XFillRectangle (dpy, x_win, gc, x, y, width, height);

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
	      XFillRectangle (dpy, x_win, gc, cursor_start, cursor_y,
                              fi->width, cursor_height);
	    }
	  else
	    {
	      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	      gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background,
				 Qnil, Qnil, Qnil,
				 make_fixnum (bar_width));
	      XDrawLine (dpy, x_win, gc, cursor_start + bar_width - 1,
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

/* The end-of-line cursor is narrower than the normal cursor. */
static void
x_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
                     face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object window;

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc = NULL;
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
	  XFillRectangle (dpy, x_win, gc, x, cursor_y, width, cursor_height);
	}
      else
	{
	  int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	  gc = XLIKE_get_gc (f, Qnil, cursor_cachel->background, Qnil,
			     Qnil, Qnil,
			     make_fixnum (bar_width));
	  XDrawLine (dpy, x_win, gc, x + bar_width - 1, cursor_y,
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
x_output_xlike_pixmap (struct frame *f, Lisp_Image_Instance *p, int x, int y,
                       int xoffset, int yoffset, int width, int height,
                       XColor fg, XColor bg)
{
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC gc;
  XGCValues gcv;
  unsigned long pixmap_mask;

  memset (&gcv, ~0, sizeof (gcv));
  gcv.graphics_exposures = False;
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

  /* depth of 0 means it's a bitmap, not a pixmap, and we should use
     XCopyPlane (1 = current foreground color, 0 = background) instead
     of XCopyArea, which means that the bits in the pixmap are actual
     pixel values, instead of symbolic of fg/bg. */

  if (IMAGE_INSTANCE_PIXMAP_DEPTH (p) > 0)
    {
      XCopyArea (dpy,
		 IMAGE_INSTANCE_X_PIXMAP_SLICE
		 (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc, xoffset,
		 yoffset, width,
		 height, x, y);
    }
  else
    {
      XCopyPlane (dpy, IMAGE_INSTANCE_X_PIXMAP_SLICE
		  (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)), x_win, gc,
		  xoffset, yoffset, width, height, x, y, 1L);
    }
}

/*****************************************************************************
 x_output_string

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
x_output_string (struct window *w, struct display_line *dl, const Ibyte *buf,
                 Bytecount len, int xpos, int xoffset, int clip_start,
                 int width, face_index findex, int cursor, int cursor_start,
                 int cursor_width, int cursor_height)
{
  /* General variables */
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
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
  GC gc, bgc;
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
      XFillRectangle (dpy, x_win, bgc, clip_start, ypos,
                      clip_end - clip_start, height);
    }

  ALLOCATE_RUNS_TEXT (text_storage, text_storage_len, buf, len);

  nruns = separate_textual_runs (WINDOW_XBUFFER (w), text_storage, runs,
                                 buf, len, cachel);

  USED (text_storage_len);

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;
      int need_clipping;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      this_width = x_text_width_single_run (dpy, cachel, runs + i);
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
	  if (DEVICE_X_GRAY_PIXMAP (d) == None)
            {
              DEVICE_X_GRAY_PIXMAP (d)
                = XCreateBitmapFromData (dpy, x_win, (char *)gray_bits,
                                         gray_width, gray_height);
            }

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
		  = x_text_width_single_run (dpy, cachel, run);
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
	      XRectangle clip_box;

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
		  XDrawLine (dpy, x_win, gc, xpos, dl->ypos + upos,
                             xpos + this_width, dl->ypos + upos);
		}
	      else if (uthick > 1)
		{
		  XFillRectangle (dpy, x_win, gc, xpos, dl->ypos + upos,
                                  this_width, uthick);
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
		XDrawLine (dpy, x_win, gc, xpos, dl->ypos - upos,
                           xpos + this_width, dl->ypos - upos);
	      else if (uthick > 1)
                XFillRectangle (dpy, x_win, gc, xpos, dl->ypos + upos,
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
		  = x_text_width_single_run (dpy, cachel, &runs[i]);
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
	      XRectangle clip_box;
	      GC cgc;
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
	  XRectangle clip_box;
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
	  XDrawLine (dpy, x_win, gc, cursor_start + bar_width - 1, tmp_y,
                     cursor_start + bar_width - 1, tmp_y + tmp_height - 1);
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
