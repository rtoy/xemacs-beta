/* Common code between X and GTK.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2003, 2005, 2009, 2010 Ben Wing.

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

#if !defined (USE_XFT) && !defined (MULE)
static int
separate_textual_runs_nomule (struct buffer * UNUSED (buf),
			      unsigned char *text_storage,
			      struct textual_run *run_storage,
			      const Ichar *str, Charcount len,
			      struct face_cachel *UNUSED(cachel))
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

#if defined (USE_XFT) && !defined (MULE)
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
separate_textual_runs_xft_nomule (struct buffer * UNUSED (buf),
				  unsigned char *text_storage,
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
      *(XftChar16 *) text_storage = str[i];
      text_storage += sizeof (XftChar16);
    }
  return 1;
}
#endif

#if defined (USE_XFT) && defined (MULE)
static int
separate_textual_runs_xft_mule (struct buffer *buf,
				unsigned char *text_storage,
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
      Lisp_Object charset;
      int byte1, byte2;
      int ucs = ichar_to_unicode (ch, CONVERR_SUBSTITUTE);

      /* @@#### This use of CONVERR_SUBSTITUTE is somewhat bogus.
	 It will substitute a '?' if we can't convert.  Not clear whether
	 this will work or not.  Problem is that we really shouldn't
	 be doing things on a charset level. */
      buffer_ichar_to_charset_codepoint (ch, buf, &charset, &byte1, &byte2,
					 CONVERR_SUBSTITUTE);

      /* If UCS is greater than 0xFFFF, set ucs2 to REPLACMENT
	 CHARACTER. */
      /* That means we can't handle characters outside of the BMP for now */
      ucs = (ucs & ~0xFFFF) ? UNICODE_REPLACEMENT_CHAR : ucs;

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

      * (XftChar16 *) text_storage = ucs;
      text_storage += sizeof (XftChar16);
    }

  if (runs_so_far)
    run_storage[runs_so_far-1].len =
      (text_storage - run_storage[runs_so_far-1].ptr) >> 1;
  return runs_so_far;
}
#endif

#if !defined (USE_XFT) && defined (MULE)
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
separate_textual_runs_mule (struct buffer *buf,
			    unsigned char *text_storage,
			    struct textual_run *run_storage,
			    const Ichar *str, Charcount len,
			    struct face_cachel *cachel)
{
  Lisp_Object prev_charset = Qunbound;
  int runs_so_far = 0, i;
  int dimension = 1, need_ccl_conversion = 0;
  Lisp_Object ccl_prog;
  struct ccl_program char_converter;

  int translate_to_ucs_2 = 0;

  for (i = 0; i < len; i++)
    {
      Ichar ch = str[i];
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
      dimension = XCHARSET_DIMENSION (charset);

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
	     Else if the font is indexed by an ISO 2022 "graphic register",
	         set `graphic'.
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
	     REPLACMENT CHARACTER. */
	  ucs = (ucs & ~0xFFFF) ? UNICODE_REPLACEMENT_CHAR : ucs;

	  byte1 = ucs >> 8;
	  byte2 = ucs & 0xFF;
	}
      else if (need_ccl_conversion)
	{
	  internal_to_external_charset_codepoint (charset, byte1, byte2,
						  &byte1, &byte2);
	  char_converter.reg[0] = XCHARSET_ID (charset);
	  char_converter.reg[1] = byte1;
	  char_converter.reg[2] = byte2;
	  ccl_driver (&char_converter, 0, buf, 0, 0, 0, CCL_MODE_ENCODING);
	  byte1 = char_converter.reg[1];
	  byte2 = char_converter.reg[2];
	  external_to_internal_charset_codepoint (charset, byte1, byte2,
						  &byte1, &byte2);
	}

      if (dimension == 2)
	*text_storage++ = (unsigned char) byte1;
      *text_storage++ = (unsigned char) byte2;
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
separate_textual_runs (struct buffer *buf,
		       unsigned char *text_storage,
		       struct textual_run *run_storage,
		       const Ichar *str, Charcount len,
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
