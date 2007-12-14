/* Lisp font handling implementation for X with Xft.

Copyright (C) 2003 Eric Knauel and Matthias Neubauer
Copyright (C) 2005 Eric Knauel
Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

Authors:	Eric Knauel <knauel@informatik.uni-tuebingen.de>
		Matthias Neubauer <neubauer@informatik.uni-freiburg.de>
		Stephen J. Turnbull <stephen@xemacs.org>
Created:	27 Oct 2003
Updated:	14 April 2007 by Stephen J. Turnbull

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
#include "font-mgr.h"

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

Lisp_Object Qfont_mgr;
Lisp_Object Qfc_patternp;
/* Lisp_Object Qfc_result_match; */ 	/* FcResultMatch */
Lisp_Object Qfc_result_type_mismatch;	/* FcResultTypeMismatch */
Lisp_Object Qfc_result_no_match; 	/* FcResultNoMatch */
Lisp_Object Qfc_result_no_id;		/* FcResultNoId */
Lisp_Object Qfc_internal_error;
Lisp_Object Vxlfd_font_name_regexp;	/* #### Really needed? */
Fixnum xft_version;
Fixnum fc_version;
Fixnum debug_xft;		/* Set to 1 enables lots of obnoxious messages.
				   Setting it to 2 or 3 enables even more. */
#ifdef FONTCONFIG_EXPOSE_CONFIG
Lisp_Object Qfc_configp;
static Lisp_Object Vfc_config_weak_list;
#endif

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

static void
print_fc_pattern (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED(escapeflag))
{
  struct fc_pattern *c = XFCPATTERN (obj);
  if (print_readably)
    printing_unreadable_object ("#<fc-pattern 0x%x>", c->header.uid);
  write_fmt_string (printcharfun, "#<fc-pattern 0x%x>", c->header.uid);
}

/* #### We really need an equal method and a hash method (required if you
   have an equal method).  For the equal method, we can probably use one
   or both of

   -- Function: FcBool FcPatternEqual (const FcPattern *pa, const
            FcPattern *pb);
       Returns whether PA and PB are exactly alike.

   -- Function: FcBool FcPatternEqualSubset (const FcPattern *pa, const
            FcPattern *pb, const FcObjectSet *os)
       Returns whether PA and PB have exactly the same values for all of
       the objects in OS.

   For the hash, we'll have to extract some subset of attributes.

   #### Crap.  It's altogether unobvious what we need.  x_color_instance
   does have a hash method, but fonts are apparently special.  I get the
   feeling that for this to work properly we're going to need to switch
   to fontconfig-based font specifications (although we can allow the
   platform syntaxes, the underlying specification object will need to
   conform to the fontconfig API, or more precisely the font-mgr API).

   I think the whole `font-truename' interface needs to be dropped. */

static const struct memory_description fcpattern_description [] = {
  /* #### nothing here, is this right?? */
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("fc-pattern", fc_pattern, 0,
			      0, print_fc_pattern, finalize_fc_pattern,
			      0, 0, fcpattern_description,
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
  (NEW_LISP_STRING_TO_EXTERNAL ((str), Qfc_font_name_encoding))

#define build_fcapi_string(str) \
  (build_ext_string ((Extbyte *) (str), Qfc_font_name_encoding))

/* #### This homebrew lashup should be replaced with FcConstants.

   fontconfig assumes that objects (property names) are statically allocated,
   and you will get bizarre results if you pass Lisp string data or strings
   allocated on the stack as objects.  fontconfig _does_ copy values, so we
   (I hope) don't have to worry about that member.

   Probably these functions don't get called so often that the memory leak
   due to strdup'ing every time we add a property would matter, but XEmacs
   _is_ a long-running process.  So we hash them.

   I suspect that using symbol names or even keywords does not provide
   assurance that the string won't move in memory.  So we hash them
   ourselves; hash.c hashtables do not interpret the value pointers.

   This array should be FcChar8**, but GCC 4.x bitches about signedness. */
static Extbyte *fc_standard_properties[] = {
  /* treated specially, ordered first */
  "family", "size",
  /* remaining are alphabetized by group */
  /* standard properties in fontconfig and Xft v.2 */
  "antialias", "aspect", "autohint", "charset", "dpi", "file",
  "foundry", "ftface", "globaladvance", "hinting", "index", "lang",
  "minspace", "outline", "pixelsize", "rasterizer", "rgba", "scalable",
  "scale", "slant", "spacing", "style", "verticallayout", "weight",
  /* common in modern fonts */
  "fontformat", "fontversion",
  /* obsolete after Xft v. 1 */
  "charwidth", "charheight", "core", "encoding", "render"
};

static struct hash_table *fc_property_name_hash_table;

/* #### Maybe fc_intern should be exposed to LISP?  The idea is that
   fc-pattern-add could warn or error if the property isn't interned. */

static const Extbyte *
fc_intern (Lisp_Object property)
{
  const void *dummy;
  const Extbyte *prop = extract_fcapi_string (property);
  const void *val = gethash (prop, fc_property_name_hash_table, &dummy);

  /* extract_fcapi_string returns something alloca'd
     so we can just drop the old value of prop on the floor */
  if (val)
    prop = (const Extbyte *) val;
  else
    {
      prop = (const Extbyte *) FcStrCopy ((FcChar8 *) prop);
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

  CHECK_STRING(name);

  fcpat->fcpatPtr = FcNameParse ((FcChar8 *) extract_fcapi_string (name));
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
  return build_fcapi_string (FcNameUnparse (XFCPATTERN_PTR (pattern)));
}

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
  const Extbyte *obj;
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

  res = FcPatternDel(XFCPATTERN_PTR(pattern), extract_fcapi_string (property));
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
  Extbyte *fc_property;
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
      fc_property = extract_fcapi_string (property);
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
		  : build_fcapi_string (fc_value.u.s));
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

DEFUN("fc-font-match", Ffc_font_match, 2, 2, 0, /*
Return the font on DEVICE that most closely matches PATTERN.

DEVICE is an X11 device.
PATTERN is a fontconfig pattern object.
Returns a fontconfig pattern object representing the closest match to the
given pattern, or an error code.  Possible error codes are
`fc-result-no-match' and `fc-result-no-id'. */
      (device, pattern))
{
  FcResult res;
  struct fc_pattern *res_fcpat;

  CHECK_FCPATTERN(pattern);
  if (NILP(device))
    return Qnil;
  CHECK_X_DEVICE(device);
  if (!DEVICE_LIVE_P(XDEVICE(device)))
    return Qnil;

  res_fcpat = ALLOC_LCRECORD_TYPE (struct fc_pattern, &lrecord_fc_pattern);
  {
    FcPattern *p = XFCPATTERN_PTR(pattern);
    FcConfig *fcc = FcConfigGetCurrent ();

    FcConfigSubstitute (fcc, p, FcMatchPattern);
    FcDefaultSubstitute (p);
    res_fcpat->fcpatPtr = FcFontMatch (fcc, p, &res);
  }

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

enum DestroyFontsetP { DestroyNo = 0, DestroyYes = 1 };

static Lisp_Object
fontset_to_list (FcFontSet *fontset, enum DestroyFontsetP destroyp)
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
  if (destroyp)
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

  return fontset_to_list (fontset, DestroyYes);

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

    return fontset_to_list (fontset, DestroyYes);
  }
}

#ifdef FONTCONFIG_EXPOSE_CONFIG

/* Configuration routines --- for debugging
   Don't depend on these routines being available in the future!

   3.2.10 Initialization
   ---------------------

   An FcConfig object holds the internal representation of a configuration.
   There is a default configuration which applications may use by passing
   0 to any function using the data within an FcConfig.
*/

static void
finalize_fc_config (void *header, int UNUSED (for_disksave))
{
  struct fc_config *p = (struct fc_config *) header;
  if (p->fccfgPtr && p->fccfgPtr != FcConfigGetCurrent())
    {
      /* If we get here, all of *our* references are garbage (see comment on
	 fc_config_create_using() for why), and the only reference that
	 fontconfig keeps is the current FcConfig. */
      FcConfigDestroy (p->fccfgPtr);
    }
  p->fccfgPtr = 0;
}

static void
print_fc_config (Lisp_Object obj, Lisp_Object printcharfun,
		 int UNUSED(escapeflag))
{
  struct fc_config *c = XFCCONFIG (obj);
  if (print_readably)
    printing_unreadable_object ("#<fc-config 0x%x>", c->header.uid);
  write_fmt_string (printcharfun, "#<fc-config 0x%x>", c->header.uid);
}

static const struct memory_description fcconfig_description [] = {
  /* #### nothing here, is this right?? */
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("fc-config", fc_config, 0,
			      0, print_fc_config, finalize_fc_config, 0, 0,
			      fcconfig_description,
			      struct fc_config);

/* We obviously need to be careful about garbage collecting the current
   FcConfig.  I infer from the documentation of FcConfigDestroy that that
   is the only reference maintained by fontconfig.
   So we keep track of our own references on a weak list, and only cons a
   new object if we don't already have a reference to it there. */

static Lisp_Object
fc_config_create_using (FcConfig * (*create_function) ())
{
  FcConfig *fc = (*create_function) ();
  Lisp_Object configs = XWEAK_LIST_LIST (Vfc_config_weak_list);

  /* Linear search: fc_configs are not going to multiply like conses. */
  {
    LIST_LOOP_2 (cfg, configs)
      if (fc == XFCCONFIG_PTR (cfg))
	return cfg;
  }

  {
    fc_config *fccfg =
      ALLOC_LCRECORD_TYPE (struct fc_config, &lrecord_fc_config);
    fccfg->fccfgPtr = fc;
    configs = Fcons (wrap_fcconfig (fccfg), configs);
    XWEAK_LIST_LIST (Vfc_config_weak_list) = configs;
    return wrap_fcconfig (fccfg);
  }
}

DEFUN("fc-config-p", Ffc_config_p, 1, 1, 0, /*
Returns t if OBJECT is of type fc-config, nil otherwise.
*/
      (object))
{
  return FCCONFIGP (object) ? Qt : Qnil;
}

DEFUN("fc-config-create", Ffc_config_create, 0, 0, 0, /*
 -- Function: FcConfig *FcConfigCreate (void)
     Creates an empty configuration. */
      ())
{
  return fc_config_create_using (&FcConfigCreate);
}

#if 0
/* I'm sorry, but we just don't do this in Lisp, OK?
   Don't even think about implementing this. */
DEFUN("fc-config-destroy", Ffc_config_destroy, 1, 1, 0, /*
 -- Function: void FcConfigDestroy (FcConfig *config)
     Destroys a configuration and any data associated with it.  Note
     that calling this function with the return value from
     FcConfigGetCurrent will place the library in an indeterminate
     state. */
      (config))
{
  signal_error (Qunimplemented, "No user-servicable parts!",
		intern ("fc-config-destroy");
}
#endif

DEFUN("fc-config-get-current", Ffc_config_get_current, 0, 0, 0, /*
 -- Function: FcConfig *FcConfigGetCurrent (void)
     Returns the current default configuration. */
      ())
{
  return fc_config_create_using (&FcConfigGetCurrent);
}

DEFUN("fc-config-up-to-date", Ffc_config_up_to_date, 1, 1, 0, /*
 -- Function: FcBool FcConfigUptoDate (FcConfig *config)
     Checks all of the files related to 'config' and returns whether the
     in-memory version is in sync with the disk version. */
      (config))
{
  CHECK_FCCONFIG (config);
  return FcConfigUptoDate (XFCCONFIG_PTR (config)) == FcFalse ? Qnil : Qt;
}

DEFUN("fc-config-build-fonts", Ffc_config_build_fonts, 1, 1, 0, /*
 -- Function: FcBool FcConfigBuildFonts (FcConfig *config)
     Builds the set of available fonts for the given configuration.
     Note that any changes to the configuration after this call have
     indeterminate effects.  Returns FcFalse if this operation runs out
     of memory.
XEmacs: signal out-of-memory, or return nil on success. */
      (config))
{
  CHECK_FCCONFIG (config);
  if (FcConfigBuildFonts (XFCCONFIG_PTR (config)) == FcFalse)
    out_of_memory ("FcConfigBuildFonts failed", config);
  return Qnil;
}

/* Calls its argument on `config', which must be defined by the caller. */

#define FCSTRLIST_TO_LISP_USING(source) do {			\
  FcChar8 *thing;					\
  FcStrList *thing_list;					\
  Lisp_Object value = Qnil;				\
  CHECK_FCCONFIG (config);				\
  thing_list = source (XFCCONFIG_PTR(config));		\
  /* Yes, we need to do this check -- sheesh, Keith! */	\
  if (!thing_list)					\
    return Qnil;					\
  while ((thing = FcStrListNext (thing_list)))		\
    value = Fcons (build_fcapi_string (thing), value);	\
  FcStrListDone (thing_list);				\
  return value;						\
  } while (0)

DEFUN("fc-config-get-config-dirs", Ffc_config_get_config_dirs, 1, 1, 0, /*
 -- Function: FcStrList *FcConfigGetConfigDirs (FcConfig *config)
     Returns the list of font directories specified in the
     configuration files for 'config'.  Does not include any
     subdirectories. */
      (config))
{
  FCSTRLIST_TO_LISP_USING (FcConfigGetConfigDirs);
}

DEFUN("fc-config-get-font-dirs", Ffc_config_get_font_dirs, 1, 1, 0, /*
 -- Function: FcStrList *FcConfigGetFontDirs (FcConfig *config)
     Returns the list of font directories in 'config'. This includes the
     configured font directories along with any directories below those
     in the filesystem. */
      (config))
{
  FCSTRLIST_TO_LISP_USING (FcConfigGetFontDirs);
}

DEFUN("fc-config-get-config-files", Ffc_config_get_config_files, 1, 1, 0, /*
 -- Function: FcStrList *FcConfigGetConfigFiles (FcConfig *config)
     Returns the list of known configuration files used to generate
     'config'.  Note that this will not include any configuration done
     with FcConfigParse. */
      (config))
{
 FCSTRLIST_TO_LISP_USING (FcConfigGetConfigFiles);
}

#undef FCSTRLIST_TO_LISP_USING

DEFUN("fc-config-get-cache", Ffc_config_get_cache, 1, 1, 0, /*
 -- Function: char *FcConfigGetCache (FcConfig *config)
     Returns the name of the file used to store per-user font
     information. */
      (config))
{
  CHECK_FCCONFIG (config);
  /* Surely FcConfigGetCache just casts an FcChar8* to char*. */
  return build_fcapi_string ((FcChar8 *) FcConfigGetCache (XFCCONFIG_PTR (config)));
}

DEFUN("fc-config-get-fonts", Ffc_config_get_fonts, 2, 2, 0, /*
 -- Function: FcFontSet *FcConfigGetFonts (FcConfig *config, FcSetName set)
     Returns one of the two sets of fonts from the configuration as
     specified by 'set'.
     `FcSetName'
       Specifies one of the two sets of fonts available in a
       configuration; FcSetSystem for those fonts specified in the
       configuration and FcSetApplication which holds fonts provided by
       the application. */
      (config, set))
{
  FcSetName name = FcSetSystem;
  FcFontSet *fs = NULL;

  CHECK_FCCONFIG (config);
  CHECK_SYMBOL (set);

  if (EQ (set, intern ("fc-set-system")))
    name = FcSetSystem;
  else if (EQ (set, intern ("fc-set-application")))
    name = FcSetApplication;
  else
    wtaerror ("must be in (fc-set-system fc-set-application)", set);

  fs = FcConfigGetFonts (XFCCONFIG_PTR (config), name);
  return fs ? fontset_to_list (fs, DestroyNo) : Qnil;
}

DEFUN("fc-config-set-current", Ffc_config_set_current, 1, 1, 0, /*
 -- Function: FcBool FcConfigSetCurrent (FcConfig *config)
     Sets the current default configuration to 'config'.  Implicitly
     calls FcConfigBuildFonts if necessary, returning FcFalse if that
     call fails.
XEmacs: signals out-of-memory if FcConfigBuildFonts fails, or args-out-of-range
if the resulting FcConfig has no fonts (which would crash XEmacs if installed).
*/
      (config))
{
  CHECK_FCCONFIG (config);
  /* *sigh* "Success" DOES NOT mean you have any fonts available.  It is
     easy to crash fontconfig, and XEmacs with it.  Without the following
     check, this will do it:
       (progn
         (fc-config-set-current (fc-config-create))
         (set-face-font 'default "serif-12"))
  */
  
  if (FcConfigBuildFonts (XFCCONFIG_PTR (config)) == FcFalse)
    out_of_memory ("FcConfigBuildFonts failed", config);
  /* #### We'd like to avoid this consing, and FcConfigGetFonts sometimes
     returns NULL, but it doesn't always.  This will do for now .... */
  if (NILP (Ffc_config_get_fonts (config, intern ("fc-set-system")))
      && NILP (Ffc_config_get_fonts (config, intern ("fc-set-application"))))
    signal_error (intern ("args-out-of-range"), "no fonts found", config);
  /* Should never happen, but I don't trust Keith anymore .... */
  if (FcConfigSetCurrent (XFCCONFIG_PTR (config)) == FcFalse)
    out_of_memory ("FcConfigBuildFonts failed in set", config);
  return Qnil;
}

DEFUN("fc-config-get-blanks", Ffc_config_get_blanks, 1, 1, 0, /*
 -- Function: FcBlanks *FcConfigGetBlanks (FcConfig *config)
     Returns the FcBlanks object associated with the given
     configuration, if no blanks were present in the configuration,
     this function will return 0.
XEmacs: should convert to a chartable.
#### Unimplemented. */
      (config))
{
  CHECK_FCCONFIG (config);
  signal_error (Qunimplemented, "no method to convert FcBlanks object",
		intern ("fc-config-get-blanks"));
}

DEFUN("fc-config-get-rescan-interval", Ffc_config_get_rescan_interval, 1, 1, 0, /*
 -- Function: int FcConfigGetRescanInterval (FcConfig *config)
     Returns the interval between automatic checks of the configuration
     (in seconds) specified in 'config'.  The configuration is checked
     during a call to FcFontList when this interval has passed since
     the last check. */
      (config))
{
  CHECK_FCCONFIG (config);
  return make_int (FcConfigGetRescanInterval (XFCCONFIG_PTR (config)));
}

DEFUN("fc-config-set-rescan-interval", Ffc_config_set_rescan_interval, 2, 2, 0, /*
 -- Function: FcBool FcConfigSetRescanInterval (FcConfig *config, int
          rescanInterval)
     Sets the rescan interval; returns FcFalse if an error occurred.
     XEmacs: signal such error, or return nil on success. */
      (config, rescan_interval))
{
  CHECK_FCCONFIG (config);
  CHECK_INT (rescan_interval);
  if (FcConfigSetRescanInterval (XFCCONFIG_PTR (config),
				 XINT (rescan_interval)) == FcFalse)
    signal_error (Qio_error, "FcConfigSetRescanInverval barfed",
		  intern ("fc-config-set-rescan-interval"));
  return Qnil;
}

/* #### This might usefully be made interactive. */
DEFUN("fc-config-app-font-add-file", Ffc_config_app_font_add_file, 2, 2, 0, /*
 -- Function: FcBool FcConfigAppFontAddFile (FcConfig *config, const
          char *file)
     Adds an application-specific font to the configuration. */
      (config, file))
{
  CHECK_FCCONFIG (config);
  CHECK_STRING (file);
  if (FcConfigAppFontAddFile
      (XFCCONFIG_PTR (config),
       /* #### FIXME! is this really Qnative? */
       (FcChar8 *) NEW_LISP_STRING_TO_EXTERNAL ((file), Qnative)) == FcFalse)
    return Qnil;
  else
    return Qt;
}

/* #### This might usefully be made interactive. */
DEFUN("fc-config-app-font-add-dir", Ffc_config_app_font_add_dir, 2, 2, 0, /*
 -- Function: FcBool FcConfigAppFontAddDir (FcConfig *config, const
          char *dir)
     Scans the specified directory for fonts, adding each one found to
     the application-specific set of fonts. */
      (config, dir))
{
  CHECK_FCCONFIG (config);
  CHECK_STRING (dir);
  if (FcConfigAppFontAddDir
      (XFCCONFIG_PTR (config),
       /* #### FIXME! is this really Qnative? */
       (FcChar8 *) NEW_LISP_STRING_TO_EXTERNAL ((dir), Qnative)) == FcFalse)
    return Qnil;
  else
    return Qt;
}

/* #### This might usefully be made interactive. */
DEFUN("fc-config-app-font-clear", Ffc_config_app_font_clear, 1, 1, 0, /*
 -- Function: void FcConfigAppFontClear (FcConfig *config)
     Clears the set of application-specific fonts. */
      (config))
{
  CHECK_FCCONFIG (config);
  FcConfigAppFontClear (XFCCONFIG_PTR (config));
  return Qnil;
}

/* These functions provide some control over how the default
   configuration of the library is initialized.  (This configuration is
   normally implicitly initialized.) */

DEFUN("fc-config-filename", Ffc_config_filename, 1, 1, 0, /*
 -- Function: char *FcConfigFilename (const char *name)
     Given the specified external entity name, return the associated
     filename.  This provides applications a way to convert various
     configuration file references into filename form.

     A null or empty 'name' indicates that the default configuration
     file should be used; which file this references can be overridden
     with the FC_CONFIG_FILE environment variable.  Next, if the name
     starts with '~', it refers to a file in the current users home
     directory.  Otherwise if the name doesn't start with '/', it
     refers to a file in the default configuration directory; the
     built-in default directory can be overridden with the
     FC_CONFIG_DIR environment variable. */
      (name))
{
  char *fcname = "";

  if (!NILP (name))
    {
      CHECK_STRING (name);
      /* #### FIXME! is this really Qnative? */
      fcname = NEW_LISP_STRING_TO_EXTERNAL (name, Qnative);
    }
  return (build_fcapi_string (FcConfigFilename ((FcChar8 *) fcname)));
}

DEFUN("fc-init-load-config", Ffc_init_load_config, 0, 0, 0, /*
 -- Function: FcConfig *FcInitLoadConfig (void)
     Loads the default configuration file and returns the resulting
     configuration.  Does not load any font information. */
      ())
{
  return fc_config_create_using (&FcInitLoadConfig);
}

DEFUN("fc-init-load-config-and-fonts", Ffc_init_load_config_and_fonts, 0, 0, 0, /*
 -- Function: FcConfig *FcInitLoadConfigAndFonts (void)
     Loads the default configuration file and builds information about
     the available fonts.  Returns the resulting configuration. */
      ())
{
  return fc_config_create_using (&FcInitLoadConfigAndFonts);
}

DEFUN("fc-init", Ffc_init, 0, 0, 0, /*
 -- Function: FcBool FcInit (void)
     Loads the default configuration file and the fonts referenced
     therein and sets the default configuration to that result.
     Returns whether this process succeeded or not.  If the default
     configuration has already been loaded, this routine does nothing
     and returns FcTrue. */
      ())
{
  return (FcInit () == FcTrue) ? Qt : Qnil;
}

DEFUN("fc-get-version", Ffc_get_version, 0, 0, 0, /*
 -- Function: int FcGetVersion (void)
     Returns the version number of the library.
XEmacs:  No, this should NOT return a pretty string.
     (let ((i (fc-get-version)))
       (format "%d.%d.%d" (/ i 10000) (mod (/ i 100) 100) (mod i 100)))
gives the usual x.y.z format.  This is the version of the .so.  It can be
checked against `fc-version', which is the version of fontconfig.h.
It's probably not a disaster if `(> (fc-get-version) fc-version)'. */
      ())
{
  return make_int (FcGetVersion ());
}

DEFUN("fc-init-reinitialize", Ffc_init_reinitialize, 0, 0, 0, /*
 -- Function: FcBool FcInitReinitialize (void)
     Forces the default configuration file to be reloaded and resets
     the default configuration. */
      ())
{
  return (FcInitReinitialize () == FcTrue) ? Qt : Qnil;
}

DEFUN("fc-init-bring-up-to-date", Ffc_init_bring_up_to_date, 0, 0, 0, /*
 -- Function: FcBool FcInitBringUptoDate (void)
     Checks the rescan interval in the default configuration, checking
     the configuration if the interval has passed and reloading the
     configuration when any changes are detected. */
      ())
{
  return (FcInitBringUptoDate () == FcTrue) ? Qt : Qnil;
}

#endif /* FONTCONFIG_EXPOSE_CONFIG */

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

/* Initialization of font-mgr */

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
      const Extbyte *s;

      CHECK_STRING (elt);
      s = fc_intern (elt);
      FcObjectSetAdd (os, s);
    }
}

void
syms_of_font_mgr (void)
{
  INIT_LRECORD_IMPLEMENTATION(fc_pattern);

  DEFSYMBOL_MULTIWORD_PREDICATE(Qfc_patternp);

  DEFSYMBOL(Qfc_result_type_mismatch);
  DEFSYMBOL(Qfc_result_no_match);
  DEFSYMBOL(Qfc_result_no_id);
  DEFSYMBOL(Qfc_internal_error);
  DEFSYMBOL(Qfont_mgr);

  DEFSUBR(Ffc_pattern_p);
  DEFSUBR(Ffc_pattern_create);
  DEFSUBR(Ffc_name_parse);
  DEFSUBR(Ffc_name_unparse);
  DEFSUBR(Ffc_pattern_duplicate);
  DEFSUBR(Ffc_pattern_add);
  DEFSUBR(Ffc_pattern_del);
  DEFSUBR(Ffc_pattern_get);
  DEFSUBR(Ffc_list_fonts_pattern_objects);
  DEFSUBR(Ffc_font_sort);
  DEFSUBR(Ffc_font_match);
  DEFSUBR(Fxlfd_font_name_p);

#ifdef FONTCONFIG_EXPOSE_CONFIG
  INIT_LRECORD_IMPLEMENTATION(fc_config);

  DEFSYMBOL_MULTIWORD_PREDICATE(Qfc_configp);

  DEFSUBR(Ffc_config_p);
  DEFSUBR(Ffc_config_create);
#if 0
  DEFSUBR(Ffc_config_destroy);
#endif
  DEFSUBR(Ffc_config_set_current);
  DEFSUBR(Ffc_config_get_current);
  DEFSUBR(Ffc_config_up_to_date);
  DEFSUBR(Ffc_config_build_fonts);
  DEFSUBR(Ffc_config_get_config_dirs);
  DEFSUBR(Ffc_config_get_font_dirs);
  DEFSUBR(Ffc_config_get_config_files);
  DEFSUBR(Ffc_config_get_cache);
  DEFSUBR(Ffc_config_get_fonts);
  DEFSUBR(Ffc_config_get_blanks);
  DEFSUBR(Ffc_config_get_rescan_interval);
  DEFSUBR(Ffc_config_set_rescan_interval);
  DEFSUBR(Ffc_config_app_font_add_file);
  DEFSUBR(Ffc_config_app_font_add_dir);
  DEFSUBR(Ffc_config_app_font_clear);
  DEFSUBR(Ffc_config_filename);
  DEFSUBR(Ffc_init_load_config);
  DEFSUBR(Ffc_init_load_config_and_fonts);
  DEFSUBR(Ffc_init);
  DEFSUBR(Ffc_get_version);
  DEFSUBR(Ffc_init_reinitialize);
  DEFSUBR(Ffc_init_bring_up_to_date);
#endif /* FONTCONFIG_EXPOSE_CONFIG */
}

void
vars_of_font_mgr (void)
{
  /* #### The next two DEFVARs belong somewhere else. */

  /* #### I know, but the right fix is use the generic debug facility. */
  DEFVAR_INT ("xft-debug-level", &debug_xft /*
Level of debugging messages to issue to stderr for Xft.
A nonnegative integer.  Set to 0 to suppress all warnings.
Default is 1 to ensure a minimum of debugging output at initialization.
Higher levels give even more information.
*/ );
  debug_xft = 1;

  DEFVAR_CONST_INT("xft-version", &xft_version /*
The major version number of the Xft library being used.
*/ );
  xft_version = XFT_VERSION;

  DEFVAR_CONST_INT("fc-version", &fc_version /*
The version number of fontconfig.h.  It can be checked against
`(fc-get-version)', which is the version of the .so.
It's probably not a disaster if `(> (fc-get-version) fc-version)'.
*/ );
  fc_version = FC_VERSION;

  Fprovide (intern ("font-mgr"));
}

void
complex_vars_of_font_mgr (void)
{
#ifdef FONTCONFIG_EXPOSE_CONFIG
  Vfc_config_weak_list = make_weak_list (WEAK_LIST_SIMPLE);
  staticpro (&Vfc_config_weak_list);
#endif

  DEFVAR_LISP("xft-xlfd-font-regexp", &Vxlfd_font_name_regexp /*
The regular expression used to match XLFD font names. */			       
	      );
  Vxlfd_font_name_regexp = make_xlfd_font_regexp();
}

void
reinit_vars_of_font_mgr (void)
{
  int i, size = (int) countof (fc_standard_properties);
  
  FcInit ();

  fc_property_name_hash_table = make_string_hash_table (size);
  for (i = 0; i < size; ++i)
    puthash (fc_standard_properties[i], NULL, fc_property_name_hash_table);
}

