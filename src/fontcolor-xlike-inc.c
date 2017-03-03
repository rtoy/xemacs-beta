/* Common code between X and GTK -- fonts and colors.
   Copyright (C) 1991-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996, 2001, 2002, 2003, 2010 Ben Wing.

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

/* Synched up with: Not in FSF. */

/* Before including this file, you need to define either THIS_IS_X or
   THIS_IS_GTK. */

/* See comment at top of console-xlike-inc.h for an explanation of
   how this file works. */

/* Pango is ready for prime-time now, as far as I understand it. The GTK
   people should be using that. Oh well. (Aidan Kehoe, Sat Nov 4 12:41:12
   CET 2006) */

#include "console-xlike-inc.h"

#ifdef DEBUG_XEMACS
# define DEBUG_FONTS1(format, arg)		\
do {						\
  if (debug_x_fonts)				\
    debug_out (format, arg);			\
} while (0)

# define DEBUG_FONTS2(format, arg1, arg2)	\
do {						\
  if (debug_x_fonts)				\
    debug_out (format, arg1, arg2);		\
} while (0)

# define DEBUG_FONTS3(format, arg1, arg2, arg3)	\
do {						\
  if (debug_x_fonts)				\
    debug_out (format, arg1, arg2, arg3);	\
} while (0)

# define DEBUG_FONTS4(format, arg1, arg2, arg3, arg4)	\
do {							\
  if (debug_x_fonts)					\
    debug_out (format, arg1, arg2, arg3, arg4);		\
} while (0)

# define DEBUG_FONTS_LISP1(format, arg)		\
do {						\
  if (debug_x_fonts)				\
    debug_out_lisp (format, arg);		\
} while (0)

# define DEBUG_FONTS_LISP2(format, arg1, arg2)	\
do {						\
  if (debug_x_fonts)				\
    debug_out_lisp (format, arg1, arg2);	\
} while (0)

# define DEBUG_FONTS_LISP3(format, arg1, arg2, arg3)	\
do {							\
  if (debug_x_fonts)					\
    debug_out_lisp (format, arg1, arg2, arg3);	\
} while (0)

# define DEBUG_FONTS_LISP4(format, arg1, arg2, arg3, arg4)	\
do {								\
  if (debug_x_fonts)						\
    debug_out_lisp (format, arg1, arg2, arg3, arg4);		\
} while (0)
#else /* not DEBUG_XEMACS */
# define DEBUG_FONTS1(format, arg)
# define DEBUG_FONTS2(format, arg1, arg2)
# define DEBUG_FONTS3(format, arg1, arg2, arg3)
# define DEBUG_FONTS4(format, arg1, arg2, arg3, arg4)
# define DEBUG_FONTS_LISP1(format, arg)
# define DEBUG_FONTS_LISP2(format, arg1, arg2)
# define DEBUG_FONTS_LISP3(format, arg1, arg2, arg3)
# define DEBUG_FONTS_LISP4(format, arg1, arg2, arg3, arg4)
#endif

#ifdef MULE

/* For some code it's reasonable to have only one copy and conditionalize
   at run-time.  For other code it isn't. */

static int 
count_hyphens (const Ibyte *str, Bytecount length, Ibyte **last_hyphen)
{
  int hyphen_count = 0; 
  const Ibyte *hyphening = str;
  const Ibyte *new_hyphening;

  for (hyphen_count = 0; 
       NULL != (new_hyphening = (Ibyte *) memchr ((const void *)hyphening, '-', length));
       hyphen_count++)
    {
      ++new_hyphening;
      length -= new_hyphening - hyphening; 
      hyphening = new_hyphening;
    }

  if (NULL != last_hyphen) 
    {
      *last_hyphen = (Ibyte *)hyphening;
    }

  return hyphen_count;
}

static int
XFUN (font_spec_matches_charset) (struct device * USED_IF_XFT (d),
				  Lisp_Object charset,
				  const Ibyte *nonreloc, Lisp_Object reloc,
				  Bytecount offset, Bytecount length,
				  enum font_specifier_matchspec_stages stage)
{
  Lisp_Object registries = Qnil;
  Elemcount i, registries_len;
  const Ibyte *the_nonreloc;
  Bytecount the_length;

  the_nonreloc = nonreloc;
  the_length = length;

  if (!the_nonreloc)
    the_nonreloc = XSTRING_DATA (reloc);
  fixup_internal_substring (nonreloc, reloc, offset, &the_length);
  the_nonreloc += offset;

#ifdef USE_XFT
  if (stage == STAGE_FINAL)
    {
      Display *dpy = DEVICE_X_DISPLAY (d);
      Extbyte *extname;
      XftFont *rf;

      if (!NILP (reloc))
	{
	  the_nonreloc = XSTRING_DATA (reloc);
	  extname = LISP_STRING_TO_EXTERNAL (reloc, Qx_font_name_encoding);
	  rf = xft_open_font_by_name (dpy, extname);
	  return 0;	 /* #### maybe this will compile and run ;) */
			 /* Jesus, Stephen, what the fuck? */
	}
    }
#endif

  /* Hmm, this smells bad. */
  if (NILP (charset))
    return 1;

  /* Hack! Short font names don't have the registry in them,
     so we just assume the user knows what they're doing in the
     case of ASCII.  For other charsets, you gotta give the
     long form; sorry buster.
     #### FMH: this screws fontconfig/Xft?
     STRATEGY: use fontconfig's ability to hack languages and character
     sets (lang and charset properties).
     #### Maybe we can use the fontconfig model to eliminate the difference
     between faces and fonts?  No - it looks like that would be an abuse
     (fontconfig doesn't know about colors, although Xft does).
     */
  if (EQ (charset, Vcharset_ascii) && 
      (!memchr (the_nonreloc, '*', the_length))
      && (5 > (count_hyphens (the_nonreloc, the_length, NULL))))
    {
      return 1;
    }

  if (STAGE_FINAL == stage)
    {
      registries = Qunicode_registries;
    }
  else if (STAGE_INITIAL == stage)
    {
      registries = XCHARSET_REGISTRIES (charset);
      if (NILP (registries))
	{
	  return 0;
	}
    }
  else assert (0);

  CHECK_VECTOR (registries);
  registries_len = XVECTOR_LENGTH (registries);

  for (i = 0; i < registries_len; ++i)
    {
      if (!(STRINGP (XVECTOR_DATA (registries)[i]))
     	  || (XSTRING_LENGTH (XVECTOR_DATA (registries)[i]) > the_length))
     	{
     	  continue;
     	}

       /* Check if the font spec ends in the registry specified. X11 says
     	  this comparison is case insensitive: XLFD, section 3.11:

     	  "Alphabetic case distinctions are allowed but are for human
     	  readability concerns only. Conforming X servers will perform
     	  matching on font name query or open requests independent of case." */
       if (0 == qxestrcasecmp (XSTRING_DATA (XVECTOR_DATA (registries)[i]), 
     			      the_nonreloc + (the_length - 
     					      XSTRING_LENGTH
     					      (XVECTOR_DATA (registries)[i]))))
     	 {
     	   return 1;
     	 }
    }
  return 0;
}

static Lisp_Object
xlistfonts_checking_charset (Lisp_Object device, const Ibyte *xlfd,
			     Lisp_Object charset, 
			     enum font_specifier_matchspec_stages stage)
{
  Extbyte **names;
  Lisp_Object result = Qnil;
  int count = 0, i;
  DECLARE_EISTRING (ei_single_result);
  Extbyte *fontext;

  DEBUG_FONTS2 ("xlistfonts_checking_charset called, XLFD %s stage %s",
		xlfd, stage == STAGE_INITIAL ? "initial" : "final");
  DEBUG_FONTS_LISP1 (" charset %s\n", charset);
  fontext = ITEXT_TO_EXTERNAL (xlfd, Qx_font_name_encoding); 
  names = XListFonts (GET_XLIKE_DISPLAY (XDEVICE (device)),
		      fontext, MAX_FONT_COUNT, &count);

  for (i = 0; i < count; ++i)
    {
      eireset (ei_single_result);
      eicpy_ext (ei_single_result, names[i], Qx_font_name_encoding);

      if (DEVMETH_OR_GIVEN (XDEVICE (device), font_spec_matches_charset,
			   (XDEVICE (device), charset,
			    eidata (ei_single_result), Qnil, 0,
			    -1, stage), 0))
	{
	  result = eimake_string (ei_single_result);
	  break;
	}
    }

  if (names)
    {
      XFreeFontNames (names);
    }

  DEBUG_FONTS_LISP1 ("xlistfonts_checking_charset returns %s\n", result);
  return result;
}

#ifdef USE_XFT
/* #### debug functions: find a better place for us */
const char *FcResultToString (FcResult r);
const char *
FcResultToString (FcResult r)
{
  static char buffer[256];
  switch (r)
    {
    case FcResultMatch:
      return "FcResultMatch";
    case FcResultNoMatch:
      return "FcResultNoMatch";
    case FcResultTypeMismatch:
      return "FcResultTypeMismatch";
    case FcResultNoId:
      return "FcResultNoId";
    default:
      snprintf (buffer, 255, "FcResultUndocumentedValue (%d)", r);
      return buffer;
    }
}

const char *FcTypeOfValueToString (FcValue v);
const char *
FcTypeOfValueToString (FcValue v)
{
  static char buffer[256];
  switch (v.type)
    {
    case FcTypeMatrix:
      return "FcTypeMatrix";
    case FcTypeString:
      return "FcTypeString";
    case FcTypeVoid:
      return "FcTypeVoid";
    case FcTypeDouble:
      return "FcTypeDouble";
    case FcTypeInteger:
      return "FcTypeInteger";
    case FcTypeBool:
      return "FcTypeBool";
    case FcTypeCharSet:
      return "FcTypeCharSet";
    case FcTypeLangSet:
      return "FcTypeLangSet";
    /* #### There is no union member of this type, but there are void* and
       FcPattern* members, as of fontconfig.h FC_VERSION 10002 */
    case FcTypeFTFace:
      return "FcTypeFTFace";
    default:
      snprintf (buffer, 255, "FcTypeUndocumentedType (%d)", v.type);
      return buffer;
    }
}

static FcCharSet *
mule_to_fc_charset (Lisp_Object cs)
{
  int ucode, i, j;
  FcCharSet *fccs;

  CHECK_CHARSET (cs);
  fccs = FcCharSetCreate ();
  /* #### do we also need to deal with 94 vs. 96 charsets?
     ie, how are SP and DEL treated in ASCII?  non-graphic should return -1 */
  if (1 == XCHARSET_DIMENSION (cs))
    /* Unicode tables are indexed by offsets from ASCII SP, not by ASCII */
    for (i = 0; i < 96; i++)
      {
	ucode = ((int *) XCHARSET_TO_UNICODE_TABLE (cs))[i];
	if (ucode >= 0)
	  /* #### should check for allocation failure */
	  FcCharSetAddChar (fccs, (FcChar32) ucode);
      }
  else if (2 == XCHARSET_DIMENSION (cs))
    /* Unicode tables are indexed by offsets from ASCII SP, not by ASCII */
    for (i = 0; i < 96; i++)
      for (j = 0; j < 96; j++)
      {
	ucode = ((int **) XCHARSET_TO_UNICODE_TABLE (cs))[i][j];
	if (ucode >= 0)
	  /* #### should check for allocation failure */
	  FcCharSetAddChar (fccs, (FcChar32) ucode);
      }
  else
    {
      FcCharSetDestroy (fccs);
      fccs = NULL;
    }
  return fccs;
}

struct charset_reporter {
  Lisp_Object *charset;
  /* This is a debug facility, require ASCII. */
  const Ascbyte *language;	/* ASCII, please */
  /* Technically this is FcChar8, but fsckin' GCC 4 bitches.
     RFC 3066 is a combination of ISO 639 and ISO 3166. */
  const Ascbyte *rfc3066;	/* ASCII, please */
};

static struct charset_reporter charset_table[] =
  {
    /* #### It's my branch, my favorite charsets get checked first!
       That's a joke, Son.
       Ie, I don't know what I'm doing, so my charsets first is as good as
       any other arbitrary order.  If you have a better idea, speak up! */
    { &Vcharset_ascii, "English", "en" },
    { &Vcharset_japanese_jisx0208, "Japanese", "ja" },
    { &Vcharset_japanese_jisx0212, "Japanese", "ja" },
    { &Vcharset_katakana_jisx0201, "Japanese", "ja" },
    { &Vcharset_latin_jisx0201, "Japanese", "ja" },
    { &Vcharset_japanese_jisx0208_1978, "Japanese", "ja" },
    { &Vcharset_greek_iso8859_7, "Greek", "el" },
    /* #### all the Chinese need checking
       Damn the blood-sucking ISO anyway. */
    { &Vcharset_chinese_gb2312, "simplified Chinese", "zh-cn" },
    { &Vcharset_korean_ksc5601, "Korean", "ko" },
    { &Vcharset_chinese_cns11643_1, "traditional Chinese", "zh-tw" },
    { &Vcharset_chinese_cns11643_2, "traditional Chinese", "zh-tw" },
    /* #### not obvious how to handle these
       We could (for experimental purposes) make the last element into
       an array of ISO 639 codes, and check for all of them.  If a font
       provides some but not others, warn. */
    { &Vcharset_latin_iso8859_1, NULL, NULL },
    { &Vcharset_latin_iso8859_2, NULL, NULL },
    { &Vcharset_latin_iso8859_3, NULL, NULL },
    { &Vcharset_latin_iso8859_4, NULL, NULL },
    { &Vcharset_latin_iso8859_9, NULL, NULL },
    { &Vcharset_latin_iso8859_15, NULL, NULL },
    { &Vcharset_thai_tis620, "Thai", "th" },
    /* We don't have an arabic charset.  bidi issues, I guess? */
    /* { &Vcharset_arabic_iso8859_6, "Arabic", "ar" }, */
    { &Vcharset_hebrew_iso8859_8, "Hebrew", "he" },
    /* #### probably close enough for Ukraine? */
    { &Vcharset_cyrillic_iso8859_5, "Russian", "ru" },
    /* #### these probably are not quite right */
    { &Vcharset_chinese_big5_1, "traditional Chinese", "zh-tw" },
    { &Vcharset_chinese_big5_2, "traditional Chinese", "zh-tw" },
    { NULL, NULL, NULL }
  };

/* Choose appropriate font name for debug messages.
   Use only in the top half of next function (enforced with #undef). */
#define DECLARE_DEBUG_FONTNAME(__xemacs_name)          \
  Eistring *__xemacs_name;                             \
  do                                                   \
    {	       					       \
      __xemacs_name = debug_xft > 2 ? eistr_fullname   \
                      : debug_xft > 1 ? eistr_longname \
                      : eistr_shortname;               \
    } while (0)

static Lisp_Object
xft_find_charset_font (Lisp_Object font, Lisp_Object charset,
		       enum font_specifier_matchspec_stages stage) 
{
  const Extbyte *patternext;
  Lisp_Object result = Qnil;

  /* #### with Xft need to handle second stage here -- sjt
     Hm.  Or maybe not.  That would be cool. :-) */
  if (stage == STAGE_FINAL)
    return Qnil;

  /* Fontconfig converts all FreeType names to UTF-8 before passing them
     back to callers---see fcfreetype.c (FcFreeTypeQuery).
     I don't believe this is documented.  */

  DEBUG_XFT1 (1, "confirming charset for font instance %s\n", 
	      XSTRING_DATA (font));

  /* #### this looks like a fair amount of work, but the basic design
     has never been rethought, and it should be

     what really should happen here is that we use FcFontSort (FcFontList?)
     to get a list of matching fonts, then pick the first (best) one that
     gives language or repertoire coverage.
  */

  FcInit ();			/* No-op if already initialized.
				   In fontconfig 2.3.2, this cannot return
				   failure, but that looks like a bug.  We
				   check for it with FcGetCurrentConfig(),
				   which *can* fail. */
  if (!FcConfigGetCurrent())
    stderr_out ("Failed fontconfig initialization\n");
  else
    {
      FcPattern *fontxft;	/* long-lived, freed at end of this block */
      FcResult fcresult;
      FcConfig *fcc;
      const Ascbyte *lang = "en";
      FcCharSet *fccs = NULL;
      DECLARE_EISTRING (eistr_shortname); /* user-friendly nickname */
      DECLARE_EISTRING (eistr_longname);  /* omit FC_LANG and FC_CHARSET */
      DECLARE_EISTRING (eistr_fullname);  /* everything */

      patternext = LISP_STRING_TO_EXTERNAL (font, Qfc_font_name_encoding);
      fcc = FcConfigGetCurrent ();

      /* parse the name, do the substitutions, and match the font */

      {
	FcPattern *p = FcNameParse ((FcChar8 *) patternext);
	PRINT_XFT_PATTERN (3, "FcNameParse'ed name is %s\n", p);
	/* #### Next two return FcBool, but what does the return mean? */
	/* The order is correct according the fontconfig docs. */
	FcConfigSubstitute (fcc, p, FcMatchPattern);
	PRINT_XFT_PATTERN (2, "FcConfigSubstitute'ed name is %s\n", p);
	FcDefaultSubstitute (p);
	PRINT_XFT_PATTERN (3, "FcDefaultSubstitute'ed name is %s\n", p);
	/* #### check fcresult of following match? */
	fcresult = FcResultMatch;
	fontxft = FcFontMatch (fcc, p, &fcresult);
	switch (fcresult)
	  {
	  /* case FcResultOutOfMemory: */
	  case FcResultNoMatch:
	  case FcResultTypeMismatch:
	  case FcResultNoId:
	    break;
	  case FcResultMatch:
	    /* this prints the long fontconfig name */
	    PRINT_XFT_PATTERN (1, "FcFontMatch'ed name is %s\n", fontxft);
	    break;
	  }
	FcPatternDestroy (p);
      }

      /* heuristic to give reasonable-length names for debug reports

         I considered #ifdef SUPPORT_FULL_FONTCONFIG_NAME etc but that's
	 pointless.  We're just going to remove this code once the font/
	 face refactoring is done, but until then it could be very useful.
      */
      {
	FcPattern *p = FcFontRenderPrepare (fcc, fontxft, fontxft);
	Extbyte *name;

	/* full name, including language coverage and repertoire
	   we delete 'charset' here because FcNameUnparse chokes on it */
	FcPatternDel (p, FC_CHARSET);
	name = (Extbyte *) FcNameUnparse (p);
	eicpy_ext (eistr_fullname,
		   (name ? name : "NOT FOUND"),
		   Qfc_font_name_encoding);
	if (name) free (name);

	/* long name, omitting coverage and repertoire, plus a number
	   of rarely useful properties */
	FcPatternDel (p, FC_LANG);
#ifdef FC_WIDTH
	FcPatternDel (p, FC_WIDTH);
#endif
	FcPatternDel (p, FC_SPACING);
	FcPatternDel (p, FC_HINTING);
	FcPatternDel (p, FC_VERTICAL_LAYOUT);
	FcPatternDel (p, FC_AUTOHINT);
	FcPatternDel (p, FC_GLOBAL_ADVANCE);
	FcPatternDel (p, FC_INDEX);
	FcPatternDel (p, FC_SCALE);
	FcPatternDel (p, FC_FONTVERSION);
	name = (Extbyte *) FcNameUnparse (p);
	eicpy_ext (eistr_longname,
		   (name ? name : "NOT FOUND"),
		   Qfc_font_name_encoding);
	if (name) free (name);

	/* nickname, just family and size, but
	   "family" names usually have style, slant, and weight */
	FcPatternDel (p, FC_FOUNDRY);
	FcPatternDel (p, FC_STYLE);
	FcPatternDel (p, FC_SLANT);
	FcPatternDel (p, FC_WEIGHT);
	FcPatternDel (p, FC_PIXEL_SIZE);
	FcPatternDel (p, FC_OUTLINE);
	FcPatternDel (p, FC_SCALABLE);
	FcPatternDel (p, FC_DPI);
	name = (Extbyte *) FcNameUnparse (p);
	eicpy_ext (eistr_shortname,
		   (name ? name : "NOT FOUND"),
		   Qfc_font_name_encoding);
	if (name) free (name);

	FcPatternDestroy (p);
      }

      /* The language approach may better in the long run, but we can't use
	 it based on Mule charsets; fontconfig doesn't provide a way to test
	 for unions of languages, etc.  That will require support from the
	 text module.

	 Optimization:  cache the generated FcCharSet in the Mule charset.
         Don't forget to destroy it if the Mule charset gets deallocated. */

      {
	/* This block possibly should be a function, but it generates
	   multiple values.  I find the "pass an address to return the
	   value in" idiom opaque, so prefer a block. */
	struct charset_reporter *cr;
	for (cr = charset_table;
	     cr->charset && !EQ (*(cr->charset), charset);
	     cr++)
	  ;

	if (cr->rfc3066)
	  {
	    DECLARE_DEBUG_FONTNAME (name);
	    CHECKING_LANG (0, eidata (name), cr->language);
	    lang = cr->rfc3066;
	  }
	else if (cr->charset)
	  {
	    /* what the hey, build 'em on the fly */
	    /* #### in the case of error this could return NULL! */
	    fccs = mule_to_fc_charset (charset);
	    /* #### Bad idea here */
	    lang = (const Ascbyte *) XSTRING_DATA (XSYMBOL (XCHARSET_NAME
							    (charset))->name);
	  }
	else
	  {
	    /* OK, we fell off the end of the table */
	    warn_when_safe_lispobj (intern ("xft"), intern ("alert"),
				    list2 (build_ascstring ("unknown charset in conversion to Xft font"),
					   charset));
	    /* default to "en"
	       #### THIS IS WRONG, WRONG, WRONG!!
	       It is why we never fall through to XLFD-checking. */
	  }

	ASSERT_ASCTEXT_ASCII (lang);

      if (fccs)
	{
	  /* check for character set coverage */
	  int i = 0;
	  FcCharSet *v;
	  FcResult r = FcPatternGetCharSet (fontxft, FC_CHARSET, i, &v);

	  if (r == FcResultTypeMismatch)
	    {
	      DEBUG_XFT0 (0, "Unexpected type return in charset value\n");
	      result = Qnil;
	    }
	  else if (r == FcResultMatch && FcCharSetIsSubset (fccs, v))
	    {
	      /* The full pattern with the bitmap coverage is massively
		 unwieldy, but the shorter names are just *wrong*.  We
		 should have the full thing internally as truename, and
		 filter stuff the client doesn't want to see on output.
		 Should we just store it into the truename right here? */
	      DECLARE_DEBUG_FONTNAME (name);
	      DEBUG_XFT2 (0, "Xft font %s supports %s\n",
			  eidata (name), lang);
#ifdef RETURN_LONG_FONTCONFIG_NAMES
	      result = eimake_string (eistr_fullname);
#else
	      result = eimake_string (eistr_longname);
#endif
	    }
	  else
	    {
	      DECLARE_DEBUG_FONTNAME (name);
	      DEBUG_XFT2 (0, "Xft font %s doesn't support %s\n",
			  eidata (name), lang);
	      result = Qnil;
	    }

	  /* clean up */
	  FcCharSetDestroy (fccs);
	}
      else
	{
	  /* check for language coverage */
	  int i = 0;
	  FcValue v;
	  /* the main event */
	  FcResult r = FcPatternGet (fontxft, FC_LANG, i, &v);

	  if (r == FcResultMatch)
	    {
	      if (v.type != FcTypeLangSet) /* excessive paranoia */
		{
		  ASSERT_ASCTEXT_ASCII (FcTypeOfValueToString (v));
		  /* Urk!  Fall back and punt to core font. */
		  DEBUG_XFT1 (0, "Unexpected type of lang value (%s)\n",
			       FcTypeOfValueToString (v));
		  result = Qnil;
		}
	      else if (FcLangSetHasLang (v.u.l, (FcChar8 *) lang)
		       != FcLangDifferentLang)
		{
		  DECLARE_DEBUG_FONTNAME (name);
		  DEBUG_XFT2 (0, "Xft font %s supports %s\n",
			      eidata (name), lang);
#ifdef RETURN_LONG_FONTCONFIG_NAMES
		  result = eimake_string (eistr_fullname);
#else
		  result = eimake_string (eistr_longname);
#endif
		}
	      else
		{
		  DECLARE_DEBUG_FONTNAME (name);
		  DEBUG_XFT2 (0, "Xft font %s doesn't support %s\n",
			      eidata (name), lang);
		  result = Qnil;
		}
	    }
	  else
	    {
	      ASSERT_ASCTEXT_ASCII (FcResultToString (r));
	      DEBUG_XFT1 (0, "Getting lang: unexpected result=%s\n",
			  FcResultToString (r));
	      result = Qnil;
	    }
	}

      /* clean up and maybe return */
      FcPatternDestroy (fontxft);
      if (!UNBOUNDP (result))
	return result;
      }
    }
  return Qnil;
}
#undef DECLARE_DEBUG_FONTNAME

#endif /* USE_XFT */

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
XFUN (find_charset_font) (Lisp_Object device, Lisp_Object font,
			  Lisp_Object charset,
			  enum font_specifier_matchspec_stages stage)
{
  Lisp_Object result = Qnil, registries = Qnil;
  int j, hyphen_count, registries_len = 0;
  Ibyte *hyphening, *new_hyphening;
  Bytecount xlfd_length;

  DECLARE_EISTRING (ei_xlfd_without_registry);
  DECLARE_EISTRING (ei_xlfd);

#ifdef USE_XFT 
  result = xft_find_charset_font (font, charset, stage);
  if (!NILP (result)) 
    {
      return result;
    }
#endif 

  switch (stage) 
    {
    case STAGE_INITIAL:
      {
	if (!(NILP (XCHARSET_REGISTRIES (charset))) 
	    && VECTORP (XCHARSET_REGISTRIES (charset)))
	  {
	    registries_len = XVECTOR_LENGTH (XCHARSET_REGISTRIES (charset));
	    registries = XCHARSET_REGISTRIES (charset);
	  }
	break;
      }
    case STAGE_FINAL:
      {
	registries_len = 1;
	registries = Qunicode_registries;
	break;
      }
    default:
      {
	assert (0);
	break;
      }
    }

  eicpy_lstr (ei_xlfd, font);
  hyphening = eidata (ei_xlfd);
  xlfd_length = eilen (ei_xlfd);

  /* Count the hyphens in the string, moving new_hyphening to just after the
     last one. */
  hyphen_count = count_hyphens (hyphening, xlfd_length, &new_hyphening);

  if (0 == registries_len || (5 > hyphen_count && 
			      !(1 == xlfd_length && '*' == *hyphening)))
    {
      /* No proper XLFD specified, or we can't modify the pattern to change
	 the registry and encoding to match what we want, or we have no
	 information on the registry needed.  */
      result = xlistfonts_checking_charset (device, eidata (ei_xlfd),
					    charset, stage);
      /* No need to loop through the available registries; return
	 immediately. */
      return result;
    }
  else if (1 == xlfd_length && '*' == *hyphening) 
    {
      /* It's a single asterisk. We can add the registry directly to the
         end. */
      eicpy_ch (ei_xlfd_without_registry, '*');
    }
  else 
    {
      /* It's a fully-specified XLFD. Work out where the registry and
         encoding are, and initialise ei_xlfd_without_registry to the string
         without them. */

      /* count_hyphens has set new_hyphening to just after the last
	 hyphen. Move back to just after the hyphen before it. */

      for (new_hyphening -= 2; new_hyphening > hyphening 
	     && '-' != *new_hyphening; --new_hyphening)
	;
      ++new_hyphening;

      eicpy_ei (ei_xlfd_without_registry, ei_xlfd); 

      /* Manipulate ei_xlfd_without_registry, using the information about
	 ei_xlfd, to which it's identical. */
      eidel (ei_xlfd_without_registry, new_hyphening - hyphening, -1, 
	    eilen (ei_xlfd) - (new_hyphening - hyphening), -1);

    }

  /* Now, loop through the registries and encodings defined for this
     charset, doing an XListFonts each time with the pattern modified to
     specify the regisry and encoding. This avoids huge amounts of IPC and
     duplicated searching; now we use the searching the X server was doing
     anyway, where before the X server did its search, transferred huge
     amounts of data, and then we proceeded to do a regexp search on that
     data. */
  for (j = 0; j < registries_len && NILP (result); ++j)
    {
      eireset (ei_xlfd);
      eicpy_ei (ei_xlfd, ei_xlfd_without_registry);

      eicat_lstr (ei_xlfd, XVECTOR_DATA (registries)[j]);

      result = xlistfonts_checking_charset (device, eidata (ei_xlfd),
					    charset, stage);
    }

  /* In the event that the charset is ASCII and we haven't matched
     anything up to now, even with a pattern of "*", add "iso8859-1"
     to the charset's registry and try again. Not returning a result
     for ASCII means our frame geometry calculations are
     inconsistent, and that we may crash. */

  if (1 == xlfd_length && EQ (charset, Vcharset_ascii) && NILP (result)
      && ('*' == eigetch (ei_xlfd_without_registry, 0)))

    {
      int have_latin1 = 0;

      /* Set this to, for example, is08859-1 if you want to see the
	 error behaviour. */

#define FALLBACK_ASCII_REGISTRY "iso8859-1" 

      for (j = 0; j < registries_len; ++j)
	{
	  if (0 == qxestrcasecmp (XSTRING_DATA (XVECTOR_DATA (registries)[j]),
				 (Ibyte *) FALLBACK_ASCII_REGISTRY))
	    {
	      have_latin1 = 1;
	      break;
	    }
	}

      if (!have_latin1)
	{
	  Lisp_Object new_registries = make_vector (registries_len + 1, Qnil);

	  XVECTOR_DATA (new_registries)[0]
	    = build_ascstring (FALLBACK_ASCII_REGISTRY);

	  memcpy (XVECTOR_DATA (new_registries) + 1,
		 XVECTOR_DATA (registries),
		 sizeof XVECTOR_DATA (registries)[0] * 
		 XVECTOR_LENGTH (registries));

	  /* Calling set_charset_registries instead of overwriting the
	     value directly, to allow the charset font caches to be
	     invalidated and a change to the default face to be
	     noted.  */
	  set_charset_registries (charset, new_registries);

	  warn_when_safe (Qface, Qwarning,
			  "Your ASCII charset registries contain nothing "
			  "sensible.  Adding `" FALLBACK_ASCII_REGISTRY "'.");

	  /* And recurse. */
	  result = 
	    DEVMETH_OR_GIVEN (XDEVICE (device), find_charset_font,
			      (device, font, charset, stage),
			      result);
	}
      else
	{
	  DECLARE_EISTRING (ei_connection_name);

	  /* We preserve a copy of the connection name for the error message
	     after the device is deleted. */
	  eicpy_lstr (ei_connection_name, 
		      DEVICE_CONNECTION (XDEVICE (device)));

	  stderr_out ("Cannot find a font for ASCII, deleting device on %s\n",
		      eidata (ei_connection_name));

	  io_error_delete_device (device);

	  /* Do a normal warning in the event that we have other, non-X
	     frames available. (If we don't, io_error_delete_device will
	     have exited.) */
	  warn_when_safe 
	    (Qface, Qerror,
	     "Cannot find a font for ASCII, deleting device on %s.\n"
	     "\n"
	     "Your X server fonts appear to be inconsistent; fix them, or\n"
	     "the next frame you create on that DISPLAY will crash this\n"
	     "XEmacs.   At a minimum, provide one font with an XLFD ending\n"
	     "in `" FALLBACK_ASCII_REGISTRY "', so we can work out what size\n"
	     "a frame should be. ",
	     eidata (ei_connection_name));
	}

    }

  /* This function used to return the font spec, in the case where a font
     didn't exist on the X server but it did match the charset. We're not
     doing that any more, because none of the other platform code does, and
     the old behaviour was badly-judged in other respects, so I don't trust
     the original author to have had a good reason for it. */

  return result;
}

#endif /* MULE */
