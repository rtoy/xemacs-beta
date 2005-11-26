/* Lisp font handling implementation for X with Xft.

Copyright (C) 2003 Eric Knauel and Matthias Neubauer
Copyright (C) 2005 Eric Knauel
Copyright (C) 2004, 2005 Free Software Foundation, Inc.

Authors:	Eric Knauel <knauel@informatik.uni-tuebingen.de>
		Matthias Neubauer <neubauer@informatik.uni-freiburg.de>
		Stephen J. Turnbull <stephen@xemacs.org>
Created:	27 Oct 2003
Updated:	05 Mar 2005 by Stephen J. Turnbull

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

/* Synched up with: Not in GNU Emacs. */

/* This module provides the Lisp interface to fonts in X11, including Xft,
   but (at least at first) not GTK+ or Qt.

   It should be renamed to fonts-x.h.

   Sealevel code should be in ../lwlib/lwlib-fonts.c or
   ../lwlib/lwlib-colors.c.
*/

#include <config.h>
#include "lisp.h"
#include "device.h"
#include "device-impl.h"
#include "console-x-impl.h"
#include "objects-x.h"
#include "objects-x-impl.h"
#include "hash.h"
#include "xft-fonts.h"

/* #### TO DO ####
   . The "x-xft-*" and "x_xft_*" nomenclature is mostly redundant, especially
     if we separate X fonts from Xft fonts, and use fontconfig more generally.
   . We should support the most recent Xft first, old Xft libraries later.
   . We may (think about it) wish to use fontconfig generally, even if we're
     not using Xft.  Either way, names that are really from fontconfig should
     use the Fc* namespace.
   . Mule-ize this file.
   . Separate X Font Struct ops from Xft Font ops; give precedence to Xft but
     allow fallback to X.
   . Push decisions about font choice, defaults, fallbacks to Lisp; if we
     really need efficiency, can reimplement in C later.
   . Implement symbols interned in this file in the Q* namespace.
   . Implement FcMatrix (Lisp vector).
   . Implement FcCharSets (Lisp chartable?  For implementation hints, see
     FcCharSetFirstPage and FcCharSetNextPage).
   . Implement FcConfigs.
   DONE
   . Fontconfig fontnames are encoded in UTF-8.
*/

Lisp_Object Qxft_font;
Lisp_Object Qfc_patternp;
Lisp_Object Qfc_fontsetp;
/* Lisp_Object Qfc_result_match; */ 	/* FcResultMatch */
Lisp_Object Qfc_result_type_mismatch;	/* FcResultTypeMismatch */
Lisp_Object Qfc_result_no_match; 	/* FcResultNoMatch */
Lisp_Object Qfc_result_no_id;		/* FcResultNoId */
Lisp_Object Qfc_internal_error;
Lisp_Object Vxlfd_font_name_regexp;	/* #### Really needed? */
Lisp_Object Vxft_version;
/* Lisp_Object Vfc_version; */		/* #### Should have this, too! */
Fixnum debug_xft;		/* Set to 1 enables lots of obnoxious messages.
				   Setting it to 2 or 3 enables even more. */

/****************************************************************
*                       FcPattern objects                       *
****************************************************************/

static void
finalize_fc_pattern (void *header, int UNUSED (for_disksave))
{
  struct fc_pattern *p = (struct fc_pattern *) header;
  if (p->fcpatPtr)
    {
      FcPatternDestroy (p->fcpatPtr);
      p->fcpatPtr = 0;
    }
}

static const struct memory_description fcpattern_description [] = {
  /* #### nothing here, is this right?? */
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("fc-pattern", fc_pattern,
			      0, 0, 0, finalize_fc_pattern, 0, 0,
			      fcpattern_description,
			      struct fc_pattern);

/*
 * Helper Functions
 */
static Lisp_Object make_xlfd_font_regexp (void);
static void string_list_to_fcobjectset (Lisp_Object list, FcObjectSet *os);

/* 
   extract the C representation of the Lisp string STR and convert it
   to the encoding used by the Fontconfig API for property and font
   names.  I suppose that Qnative is the right encoding, the manual
   doesn't say much about this topic.  This functions assumes that STR
   is a Lisp string.
*/
#define extract_fcapi_string(str) \
  ((FcChar8 *) NEW_LISP_STRING_TO_EXTERNAL ((str), Qnative))

/* fontconfig assumes that objects (property names) are statically allocated,
   and you will get bizarre results if you pass Lisp string data or strings
   allocated on the stack as objects.  fontconfig _does_ copy values, so we
   (I hope) don't have to worry about that member.

   Probably these functions don't get called so often that the memory leak
   due to strdup'ing every time we add a property would matter, but XEmacs
   _is_ a long-running process.  So we hash them.

   I suspect that using symbol names or even keywords does not provide
   assurance that the string won't move in memory.  So we hash them
   ourselves; hash.c hashtables do not interpret the value pointers. */
static FcChar8 *fc_standard_properties[] = {
  "antialias", "aspect", "autohint", "charset", "dpi", "family", "file",
  "foundry", "ftface", "globaladvance", "hinting", "index", "lang",
  "minspace", "outline", "pixelsize", "rasterizer", "rgba", "scalable",
  "scale", "size", "slant", "spacing", "style", "verticallayout", "weight",
  /* obsolete after Xft v. 1 */
  "charwidth", "charheight", "core", "encoding", "render"
};

static struct hash_table *fc_property_name_hash_table;

/* #### Maybe fc_intern should be exposed to LISP?  The idea is that
   fc-pattern-add could warn or error if the property isn't interned. */

static FcChar8 *
fc_intern (Lisp_Object property)
{
  const void *dummy;
  FcChar8 *prop = extract_fcapi_string (property);
  const void *val = gethash (prop, fc_property_name_hash_table, &dummy);

  /* extract_fcapi_string returns something alloca'd
     so we can just drop the old value of prop on the floor */
  if (val)
    prop = (FcChar8 *) val;
  else
    {
      prop = FcStrCopy (prop);
      puthash (prop, NULL, fc_property_name_hash_table);
    }
  return prop;
}

DEFUN("fc-pattern-p", Ffc_pattern_p, 1, 1, 0, /*
Returns t if OBJECT is of type fc-pattern, nil otherwise.
*/
      (object))
{
  return FCPATTERNP(object) ? Qt : Qnil;
}

DEFUN("fc-pattern-create", Ffc_pattern_create, 0, 0, 0, /* 
Return a new, empty fc-pattern object.
*/
      ())
{
  fc_pattern *fcpat =
    ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);

  fcpat->fcpatPtr = FcPatternCreate();
  return wrap_fcpattern(fcpat);
}

DEFUN("fc-name-parse", Ffc_name_parse, 1, 1, 0, /*
Parse an Fc font name and return its representation as a fc pattern object.
*/
      (name))
{
  struct fc_pattern *fcpat =
    ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);

  CHECK_STRING(name);		/* #### MEMORY LEAK!!  maybe not ... */

  fcpat->fcpatPtr = FcNameParse (extract_fcapi_string (name));
  return wrap_fcpattern(fcpat);
}

/* #### Ga-a-ack!  Xft's similar function is actually a different API.
   We provide both. */
DEFUN("fc-name-unparse", Ffc_name_unparse, 1, 1, 0, /*
Unparse an fc pattern object to a string.
*/
      (pattern))
{
  CHECK_FCPATTERN(pattern);
  {
  FcChar8 *temp = FcNameUnparse(XFCPATTERN_PTR(pattern));
  Lisp_Object res = build_ext_string (temp, Qxft_font_name_encoding);
  free (temp);
  return res;
  }
}

#if 0
/* #### This seems to not work? */
DEFUN("xft-name-unparse", Fxft_name_unparse, 1, 1, 0, /*
Unparse an fc pattern object to a string (using the Xft API).
*/
      (pattern))
{
  char temp[FCSTRLEN];
  Bool res;

  CHECK_FCPATTERN(pattern);
  res = XftNameUnparse(XFCPATTERN_PTR(pattern), temp, FCSTRLEN-1);
  return res ? build_ext_string (temp, Qxft_font_name_encoding) : Qnil;
}
#endif

DEFUN("fc-pattern-duplicate", Ffc_pattern_duplicate, 1, 1, 0, /* 
Make a copy of the fc pattern object PATTERN and return it.
*/
      (pattern))
{
  struct fc_pattern *copy = NULL;
  CHECK_FCPATTERN(pattern);

  copy = ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);
  copy->fcpatPtr = FcPatternDuplicate(XFCPATTERN_PTR(pattern));
  return wrap_fcpattern(copy);
}

DEFUN("fc-pattern-add", Ffc_pattern_add, 3, 3, 0, /*
Add attributes to the pattern object PATTERN.  PROPERTY is a string naming
the attribute to add, VALUE the value for this attribute.

VALUE may be a string, integer, float, or symbol, in which case the value
will be added as an FcChar8[], int, double, or FcBool respectively.
*/
      (pattern, property, value))
{
  Bool res = 0;
  Extbyte *obj;
  FcPattern *fcpat;

  CHECK_FCPATTERN(pattern);
  CHECK_STRING(property);

  obj = fc_intern (property);
  fcpat = XFCPATTERN_PTR (pattern);

  if (STRINGP(value)) 
    {
      FcChar8 *str = (FcChar8 *) extract_fcapi_string (value);
      res = FcPatternAddString (fcpat, obj, str);
    }
  else if (INTP(value)) 
    {
      res = FcPatternAddInteger (fcpat, obj, XINT(value));
    }
  else if (FLOATP(value)) 
    {
      res = FcPatternAddDouble (fcpat, obj, (double) XFLOAT_DATA(value));
    }
  else if (SYMBOLP(value)) 
    {
      res = FcPatternAddBool (fcpat, obj, !NILP(value));
    }
  /* else ... maybe we should wta here? */

  return res ? Qt : Qnil;
}

DEFUN("fc-pattern-del", Ffc_pattern_del, 2, 2, 0, /*
Remove attribute PROPERTY from fc pattern object OBJECT.
*/
      (pattern, property))
{
  Bool res;

  CHECK_FCPATTERN(pattern);
  CHECK_STRING(property);

  res = FcPatternDel(XFCPATTERN_PTR(pattern), 
		     extract_fcapi_string (property));
  return res ? Qt : Qnil;
}

/* Generic interface to FcPatternGet()
 * Don't support the losing symbol-for-property interface.
 */
DEFUN("fc-pattern-get", Ffc_pattern_get, 2, 4, 0, /*
From PATTERN, extract PROPERTY for the ID'th member, of type TYPE.

PATTERN is an Xft (fontconfig) pattern object.
PROPERTY is a string naming an fontconfig font property.
Optional ID is a nonnegative integer indexing the list of values for PROPERTY
  stored in PATTERN, defaulting to 0 (the first value).
Optional TYPE is a symbol, one of 'string, 'boolean, 'integer, 'float,
  'double, 'matrix, 'charset, or 'void, corresponding to the FcValue types.
  ('float is an alias for 'double).

The Lisp types returned will conform to TYPE:
  string		string
  boolean		`t' or `nil'
  integer		integer
  double (float)	float
  matrix		not implemented
  charset		not implemented
  void			not implemented

Symbols with names of the form "fc-result-DESCRIPTION" are returned when
the desired value is not available.  These are

  fc-result-type-mismatch       the value found has an unexpected type
  fc-result-no-match            there is no such attribute
  fc-result-no-id               there is no value for the requested ID

The types of the following standard properties are predefined by fontconfig.
The symbol 'fc-result-type-mismatch will be returned if the object exists but
TYPE does not match the predefined type.  It is best not to specify a type
for predefined properties, as a mistake here ensures error returns on the
correct type.

Each standard property has a convenience accessor defined in fontconfig.el,
named in the form "fc-pattern-get-PROPERTY".  The convenience functions are
preferred to `fc-pattern-get' since a typo in the string naming a property
will result in a silent null return, while a typo in a function name will
usually result in a compiler or runtime \"not fboundp\" error.  You may use
`defsubst' to define convenience functions for non-standard properties.

family         String  Font family name 
style          String  Font style. Overrides weight and slant 
slant          Int     Italic, oblique or roman 
weight         Int     Light, medium, demibold, bold or black 
size           Double  Point size 
aspect         Double  Stretches glyphs horizontally before hinting 
pixelsize      Double  Pixel size 
spacing        Int     Proportional, monospace or charcell 
foundry        String  Font foundry name 
antialias      Bool    Whether glyphs can be antialiased 
hinting        Bool    Whether the rasterizer should use hinting 
verticallayout Bool    Use vertical layout 
autohint       Bool    Use autohinter instead of normal hinter 
globaladvance  Bool    Use font global advance data 
file           String  The filename holding the font 
index          Int     The index of the font within the file 
ftface         FT_Face Use the specified FreeType face object 
rasterizer     String  Which rasterizer is in use 
outline        Bool    Whether the glyphs are outlines 
scalable       Bool    Whether glyphs can be scaled 
scale          Double  Scale factor for point->pixel conversions 
dpi            Double  Target dots per inch 
rgba           Int     unknown, rgb, bgr, vrgb, vbgr, none - subpixel geometry 
minspace       Bool    Eliminate leading from line spacing 
charset        CharSet Unicode chars encoded by the font 
lang           String  List of RFC-3066-style languages this font supports

The FT_Face, Matrix, CharSet types are unimplemented, so the corresponding
properties are not accessible from Lisp at this time.  If the value of a
property returned has type FT_Face, FcCharSet, or FcMatrix,
`fc-result-type-mismatch' is returned.

The following properties which were standard in Xft v.1 are obsolete in
Xft v.2:  encoding, charwidth, charheight, core, and render. */
      (pattern, property, id, type))
{
  FcChar8 *fc_property;		/* UExtbyte * */
  FcResult fc_result;
  FcValue fc_value;

  /*
    process arguments
  */
  CHECK_FCPATTERN (pattern);

#if 0
  /* Don't support the losing symbol-for-property interface. */
  property = SYMBOLP (property) ? symbol_name (XSYMBOL (property)) : property;
#endif
  if (STRINGP (property))
    {
      fc_property = (FcChar8 *) extract_fcapi_string (property);
    }
  else
    {
      /* if we allow symbols, this would need to be
	 list3 (Qlambda, list1 (Qobject),
		  list3 (Qor, list2 (Qstringp, Qobject),
			      list2 (Qsymbolp, Qobject)))
	 or something like that? */
      dead_wrong_type_argument (Qstringp, property);
    }

  if (!NILP (id)) CHECK_NATNUM (id);
  if (!NILP (type)) CHECK_SYMBOL (type);

  /* get property */
  fc_result = FcPatternGet (XFCPATTERN_PTR (pattern),
			    fc_property,
			    NILP (id) ? 0 : XINT(id),
			    &fc_value);

  switch (fc_result)
    {
    case FcResultMatch:
      /* wrap it and return */
      switch (fc_value.type)
	{
	case FcTypeInteger:
	  return ((!NILP (type) && !EQ (type, Qinteger))
		  ? Qfc_result_type_mismatch : make_int (fc_value.u.i));
	case FcTypeDouble:
	  return ((!NILP (type) && !EQ (type, intern ("double"))
		   && !EQ (type, Qfloat))
		  ? Qfc_result_type_mismatch : make_float (fc_value.u.d));
	case FcTypeString:
	  return ((!NILP (type) && !EQ (type, Qstring))
		  ? Qfc_result_type_mismatch
		  : build_ext_string (fc_value.u.s, Qxft_font_name_encoding));
	case FcTypeBool:
	  return ((!NILP (type) && !EQ (type, Qboolean))
		  ? Qfc_result_type_mismatch : fc_value.u.b ? Qt : Qnil);
	case FcTypeMatrix:
	  return Qfc_result_type_mismatch;
	  /* #### unimplemented
	  return ((!NILP (type) && !EQ (type, intern ("matrix")))
		  ? Qfc_result_type_mismatch : make_int (fc_value.u.m));
	  */
	case FcTypeCharSet:
	  return Qfc_result_type_mismatch;
	  /* #### unimplemented
	  return ((!NILP (type) && !EQ (type, intern ("charset")))
		  ? Qfc_result_type_mismatch : make_int (fc_value.u.c));
	  */
	}
    case FcResultTypeMismatch:
      return Qfc_result_type_mismatch;
    case FcResultNoMatch:
      return Qfc_result_no_match;
    case FcResultNoId:
      return Qfc_result_no_id;
    default:
      return Qfc_internal_error;
    }
}

#if 0
/* #### delete this after some testing!! don't forget the DEFSUBR */
/* #### This is a big mistake, no? --- crap, there's no implicit finalizer */
DEFUN("fc-pattern-destroy", Ffc_pattern_destroy, 1, 1, 0, /*
Explicitly deallocate a fc pattern object PATTERN. */
      (pattern))
{
  CHECK_FCPATTERN(pattern); 

  /* paranoia strikes deep */
  if (XFCPATTERN_PTR(pattern))
    {
      FcPatternDestroy(XFCPATTERN_PTR(pattern));
      XFCPATTERN_PTR(pattern) = 0;
    }
  return Qnil;
}
#endif

DEFUN("fc-font-match", Ffc_font_match, 2, 2, 0, /*
Return the font on DEVICE that most closely matches PATTERN.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern object.
Returns a fontconfig pattern object representing the closest match to the
given pattern, or an error code.  Possible error codes are
`fc-result-no-match' and `fc-result-no-id'. */
      (device, pattern))
{
  Display *dpy;
  FcResult res;

  struct fc_pattern *res_fcpat =
    ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);
  CHECK_FCPATTERN(pattern);	/* #### MEMORY LEAKS!!! */
  if (NILP(device))
    return Qnil;
  CHECK_X_DEVICE(device);
  if (!DEVICE_LIVE_P(XDEVICE(device)))
    return Qnil;

  dpy = DEVICE_X_DISPLAY(XDEVICE(device));
  /* More Xft vs fontconfig brain damage? */
  res_fcpat->fcpatPtr = XftFontMatch(dpy, DefaultScreen (dpy),
				     XFCPATTERN_PTR(pattern), &res);
  
  if (res_fcpat->fcpatPtr == NULL)
    switch (res) {
    case FcResultNoMatch:
      return Qfc_result_no_match;
    case FcResultNoId:
      return Qfc_result_no_id;
    default:
      return Qfc_internal_error;
    }
  else
    return wrap_fcpattern(res_fcpat);
}

/* NOTE NOTE NOTE This function destroys the FcFontSet passed to it. */
static Lisp_Object
fontset_to_list (FcFontSet *fontset)
{
  int idx;
  Lisp_Object fontlist = Qnil;
  fc_pattern *fcpat;

  /* #### improve this error message */
  if (!fontset)
    Fsignal (Qinvalid_state,
	     list1 (build_string ("failed to create FcFontSet")));
  for (idx = 0; idx < fontset->nfont; ++idx)
    {
      fcpat = 
	ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);
      fcpat->fcpatPtr = FcPatternDuplicate (fontset->fonts[idx]);
      fontlist = Fcons (wrap_fcpattern(fcpat), fontlist);
    }
  FcFontSetDestroy (fontset);
  return fontlist;
}

/* #### fix this name to correspond to Ben's new nomenclature */
DEFUN("fc-list-fonts-pattern-objects", Ffc_list_fonts_pattern_objects,
      3, 3, 0, /*
Return a list of fonts on DEVICE that match PATTERN for PROPERTIES.
Each font is represented by a fontconfig pattern object.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern to be matched.
PROPERTIES is a list of property names (strings) that should match.

#### DEVICE is unused, ignored, and may be removed if it's not needed to
match other font-listing APIs. */
      (UNUSED (device), pattern, properties))
{
  FcObjectSet *os;
  FcFontSet *fontset;

  CHECK_FCPATTERN (pattern);
  CHECK_LIST (properties);

  os = FcObjectSetCreate ();
  string_list_to_fcobjectset (properties, os);
  /* #### why don't we need to do the "usual substitutions"? */
  fontset = FcFontList (NULL, XFCPATTERN_PTR (pattern), os);
  FcObjectSetDestroy (os);

  return fontset_to_list (fontset);

}

/* #### maybe this can/should be folded into fc-list-fonts-pattern-objects? */
DEFUN("fc-font-sort", Ffc_font_sort, 2, 4, 0, /*
Return a list of all fonts sorted by proximity to PATTERN.
Each font is represented by a fontconfig pattern object.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern to be matched.
Optional argument TRIM, if non-nil, means to trim trailing fonts that do not
contribute new characters to the union repertoire.

#### Optional argument NOSUB, if non-nil, suppresses some of the usual
property substitutions.  DON'T USE THIS in production code, it is intended
for exploring behavior of fontconfig and will be removed when this code is
stable.

#### DEVICE is unused, ignored, and may be removed if it's not needed to
match other font-listing APIs. */
      (UNUSED (device), pattern, trim, nosub))
{
  CHECK_FCPATTERN (pattern);

  {
    FcConfig *fcc = FcConfigGetCurrent();
    FcFontSet *fontset;
    FcPattern *p = XFCPATTERN_PTR (pattern);
    FcResult fcresult;

    if (NILP(nosub))		/* #### temporary debug hack */
      FcDefaultSubstitute (p);
    FcConfigSubstitute (fcc, p, FcMatchPattern);
    fontset = FcFontSort (fcc, p, !NILP(trim), NULL, &fcresult);

    return fontset_to_list (fontset);
  }
}

/* #### this actually is an Xft function, should split those out
   or get rid of them entirely? */
/* #### be consistent about argument order. */
DEFUN("fc-font-real-pattern", Ffc_font_real_pattern, 2, 2, 0, /*
Temporarily open FONTNAME (a string) and return the actual
fc pattern matched by the Fc library.	*/
      (fontname, xdevice))
{
  FcPattern *copy;
  Display *dpy;
  XftFont *font;
  struct fc_pattern *fcpat =
    ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);

  CHECK_STRING (fontname);	/* #### MEMORY LEAK?!  maybe not ... */
  if (NILP(xdevice))
    return Qnil;
  CHECK_X_DEVICE (xdevice);
  if (!DEVICE_LIVE_P(XDEVICE(xdevice)))
    return Qnil;

  /* #### these gymnastics should be unnecessary, just use FcFontMatch */
  dpy = DEVICE_X_DISPLAY (XDEVICE (xdevice));
  font = XftFontOpenName (dpy, DefaultScreen(dpy),
			  extract_fcapi_string (fontname));
  if (font == NULL)
    return Qnil;
  copy = FcPatternDuplicate(font->pattern);
  XftFontClose(dpy, font);  
  if (copy == NULL)
    return Qnil;
  fcpat->fcpatPtr = copy;
  return wrap_fcpattern(fcpat);
}

DEFUN("xlfd-font-name-p", Fxlfd_font_name_p, 1, 1, 0, /*
Check whether the string FONTNAME is a XLFD font name. */
      (fontname))
{
  CHECK_STRING(fontname);
  /* #### should bind `case-fold-search' here? */
  return Fstring_match(Vxlfd_font_name_regexp, fontname, Qnil, Qnil);
}

/* FcPatternPrint: there is no point in having wrappers fc-pattern-print,
   Ffc_pattern_print since this function prints to stdout. */

/* Initialization of xft-fonts */

#define XE_XLFD_SEPARATOR	"-"
      /* XLFD specifies ISO 8859-1 encoding, but we can't handle non-ASCII
	 in Mule when this function is called.  So use HPC. */
#if 0
#define XE_XLFD_PREFIX		"\\(\\+[\040-\176\240-\377]*\\)?-"
#define XE_XLFD_OPT_TEXT	"\\([\040-\044\046-\176\240-\377]*\\)"
#define XE_XLFD_TEXT		"\\([\040-\044\046-\176\240-\377]+\\)"
#else
#define XE_XLFD_PREFIX		"\\(\\+[\040-\176]*\\)?-"
#define XE_XLFD_OPT_TEXT	"\\([^-]*\\)"
#define XE_XLFD_TEXT		"\\([^-]+\\)"
#endif

#define XE_XLFD_SLANT		"\\([0-9ior?*][iot]?\\)"
#define XE_XLFD_SPACING		"\\([cmp?*]\\)"
      /* Hyphen as minus conflicts with use as separator. */
#define XE_XLFD_OPT_NEGATE      "~?"
#define XE_XLFD_NUMBER		"\\([0-9?*]+\\)"
#define XE_XLFD_PSIZE		"\\([0-9?*]+\\|\\[[ 0-9+~.e?*]+\\]\\)"
  
/* Call this only from the init code
   #### This is really horrible, let's get rid of it, please. */
static Lisp_Object
make_xlfd_font_regexp (void)
{
  struct gcpro gcpro1;
  unsigned i;
  Lisp_Object reg = Qnil;
  const Extbyte *re[] = 	/* #### This could just be catenated by
				   cpp and passed to build_ext_string. */
    {
      /* Regular expression matching XLFDs as defined by XLFD v. 1.5.
	 Matches must be case-insensitive.
	 PSIZE is a pixel or point size, which may be a "matrix".  The
	 syntax of a matrix is not checked, just some lexical properties.
	 AFAICT none of the TEXT fields except adstyle is optional.

	 NB. It should not be a problem if this matches "too much", since
	 an "old" server will simply not be able to find a matching font. */
      "\\`",
      XE_XLFD_PREFIX,		/* prefix */
      XE_XLFD_TEXT,		/* foundry */
      XE_XLFD_SEPARATOR,
      XE_XLFD_TEXT,		/* family */
      XE_XLFD_SEPARATOR,
      XE_XLFD_TEXT,		/* weight */
      XE_XLFD_SEPARATOR,
      XE_XLFD_SLANT,		/* slant */
      XE_XLFD_SEPARATOR,
      XE_XLFD_TEXT,		/* swidth */
      XE_XLFD_SEPARATOR,
      XE_XLFD_OPT_TEXT,		/* adstyle */
      XE_XLFD_SEPARATOR,
      XE_XLFD_PSIZE,		/* pixelsize */
      XE_XLFD_SEPARATOR,
      XE_XLFD_PSIZE,		/* pointsize */
      XE_XLFD_SEPARATOR,
      XE_XLFD_NUMBER,		/* resx */
      XE_XLFD_SEPARATOR,
      XE_XLFD_NUMBER,		/* resy */
      XE_XLFD_SEPARATOR,
      XE_XLFD_SPACING,		/* spacing */
      XE_XLFD_SEPARATOR,
      XE_XLFD_OPT_NEGATE,	/* avgwidth */
      XE_XLFD_NUMBER,
      XE_XLFD_SEPARATOR,
      XE_XLFD_TEXT,		/* registry */
      XE_XLFD_SEPARATOR,
      XE_XLFD_TEXT,		/* encoding */
      "\\'"
    };
  
  GCPRO1 (reg);  
  for (i = 0; i < sizeof(re)/sizeof(Extbyte *); i++)
    {
      /* #### Currently this is Host Portable Coding, not ISO 8859-1. */
      reg = concat2(reg, build_ext_string (re[i], Qx_font_name_encoding));
    }

  RETURN_UNGCPRO (reg);
}
#undef XE_XLFD_SEPARATOR
#undef XE_XLFD_PREFIX
#undef XE_XLFD_OPT_TEXT
#undef XE_XLFD_TEXT
#undef XE_XLFD_OPT_SLANT
#undef XE_XLFD_OPT_SPACING
#undef XE_XLFD_OPT_NEGATE
#undef XE_XLFD_NUMBER
#undef XE_XLFD_PSIZE

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

static void 
string_list_to_fcobjectset (Lisp_Object list, FcObjectSet *os)
{
  EXTERNAL_LIST_LOOP_2 (elt, list)
    {
      FcChar8 *s;

      CHECK_STRING (elt);
      s = fc_intern (elt);
      fprintf (stderr, "%s\n", s);
      FcObjectSetAdd (os, s);
    }
}

void
syms_of_xft_fonts (void)
{
  INIT_LRECORD_IMPLEMENTATION(fc_pattern);

  DEFSYMBOL_MULTIWORD_PREDICATE(Qfc_patternp);

  DEFSYMBOL(Qfc_result_type_mismatch);
  DEFSYMBOL(Qfc_result_no_match);
  DEFSYMBOL(Qfc_result_no_id);
  DEFSYMBOL(Qfc_internal_error);
  DEFSYMBOL(Qxft_font);

  DEFSUBR(Ffc_pattern_p);
  DEFSUBR(Ffc_pattern_create);
  DEFSUBR(Ffc_name_parse);
  DEFSUBR(Ffc_name_unparse);
  DEFSUBR(Fxft_name_unparse);	/* URK! */
  DEFSUBR(Ffc_pattern_duplicate);
  DEFSUBR(Ffc_pattern_add);
  DEFSUBR(Ffc_pattern_del);
  DEFSUBR(Ffc_pattern_get);
#if 0
  DEFSUBR(Ffc_pattern_destroy);
#endif
  DEFSUBR(Ffc_list_fonts_pattern_objects);
  DEFSUBR(Ffc_font_sort);
  DEFSUBR(Ffc_font_match);
  DEFSUBR(Ffc_font_real_pattern);
  DEFSUBR(Fxlfd_font_name_p);
}

void
vars_of_xft_fonts (void)
{
  /* #### I know, but the right fix is use the generic debug facility. */
  DEFVAR_INT ("xft-debug-level", &debug_xft /*
Level of debugging messages to issue to stderr for Xft.
A nonnegative integer.  Set to 0 to suppress all warnings.
Default is 1 to ensure a minimum of debugging output at initialization.
Higher levels give even more information.
*/ );
  debug_xft = 1;

  DEFVAR_LISP("xft-version", &Vxft_version /*
The major version number of the Xft library being used.
*/ );
  Vxft_version = make_int(XFT_VERSION);

  Fprovide (intern ("xft"));
}

void
complex_vars_of_xft_fonts (void)
{
  DEFVAR_LISP("xft-xlfd-font-regexp", &Vxlfd_font_name_regexp /*
The regular expression used to match XLFD font names. */			       
	      );
  Vxlfd_font_name_regexp = make_xlfd_font_regexp();
}

void
reinit_vars_of_xft_fonts (void)
{
  int i, size = (int) countof (fc_standard_properties);
  
  FcInit ();

  fc_property_name_hash_table = make_string_hash_table (size);
  for (i = 0; i < size; ++i)
    puthash (fc_standard_properties[i], NULL, fc_property_name_hash_table);
}

