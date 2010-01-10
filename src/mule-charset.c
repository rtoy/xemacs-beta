/* Functions to handle multilingual characters.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2004, 2005, 2010 Ben Wing.

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

/* Synched up with: FSF 20.3.  Not in FSF. */

/* Rewritten by Ben Wing <ben@xemacs.org>. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "chartab.h"
#include "elhash.h"
#include "device.h"
#include "faces.h"
#include "lstream.h"
#ifdef HAVE_CCL
#include "mule-ccl.h"
#endif /* HAVE_CCL */
#include "objects.h"
#include "specifier.h"

/* The various pre-defined charsets. */

Lisp_Object Vcharset_ascii;
Lisp_Object Vcharset_control_1;
Lisp_Object Vcharset_latin_iso8859_1;
Lisp_Object Vcharset_latin_iso8859_2;
Lisp_Object Vcharset_latin_iso8859_3;
Lisp_Object Vcharset_latin_iso8859_4;
Lisp_Object Vcharset_thai_tis620;
Lisp_Object Vcharset_arabic_iso8859_6;
Lisp_Object Vcharset_greek_iso8859_7;
Lisp_Object Vcharset_hebrew_iso8859_8;
Lisp_Object Vcharset_katakana_jisx0201;
Lisp_Object Vcharset_latin_jisx0201;
Lisp_Object Vcharset_cyrillic_iso8859_5;
Lisp_Object Vcharset_latin_iso8859_9;
Lisp_Object Vcharset_latin_iso8859_15;
Lisp_Object Vcharset_chinese_sisheng;
Lisp_Object Vcharset_japanese_jisx0208_1978;
Lisp_Object Vcharset_chinese_gb2312;
Lisp_Object Vcharset_japanese_jisx0208;
Lisp_Object Vcharset_korean_ksc5601;
Lisp_Object Vcharset_japanese_jisx0212;
Lisp_Object Vcharset_chinese_cns11643_1;
Lisp_Object Vcharset_chinese_cns11643_2;
#ifdef UNICODE_INTERNAL
Lisp_Object Vcharset_chinese_big5;
Lisp_Object Vcharset_japanese_shift_jis;
#else
Lisp_Object Vcharset_chinese_big5_1;
Lisp_Object Vcharset_chinese_big5_2;
#endif /* UNICODE_INTERNAL */
Lisp_Object Vcharset_composite;

#ifndef UNICODE_INTERNAL

/* GNU Emacs creates charsets called `mule-unicode-0100-24ff',
   `mule-unicode-2500-33ff', and `mule-unicode-e000-ffff'.
   Although it's conceivable we could also start from 0x100 on the
   assumption that ASCII and Latin-1 can always be represented otherwise,
   it's not quite so clean; and furthermore, by starting at 0, we end
   up with chunks that perfectly end right before the start of the
   surrogate space.  We just pick up right afterwards and don't have to
   worry about having a charset that crosses the surrogate space or
   overlaps one side of it.

   Since we're not being compatible with their charsets in our ranges,
   there's no sense in being compatible in other ways. */

#define FROB(low, high)				\
Lisp_Object Vcharset_unicode_##low##_##high;	\
Lisp_Object Qunicode_##low##_##high

FROB  (   0,  23ff);
FROB  (2400,  47ff);
FROB  (4800,  6bff);
FROB  (6c00,  8fff);
FROB  (9000,  b3ff);
FROB  (b400,  d7ff);
FROB  (e000, 103ff);
FROB (10400, 127ff);
FROB (12800, 14bff);
FROB (14c00, 16fff);
FROB (17000, 193ff);
FROB (19400, 1b7ff);
FROB (1b800, 1dbff);
FROB (1dc00, 1ffff);
FROB (20000, 223ff);
FROB (22400, 247ff);
FROB (24800, 26bff);
FROB (26c00, 28fff);
FROB (29000, 2b3ff);
FROB (2b400, 2d7ff);
FROB (2d800, 2fbff);
FROB (2fc00, 31fff);
/* WARNING: Any changes to this list need to be propagated to at least two
   other places in this file (in syms_of_mule_charset() and
   complex_vars_of_mule_charset(); maybe also to the CHARSET_ID enum just
   below. */

#undef FROB

/* Non-public definitions of charset IDs */
enum CHARSET_ID_OFFICIAL
{
  /* WARNING!!!  If you change this, you *MUST* change
     rep_bytes_by_first_byte[] in text.c correspondingly. */
  CHARSET_ID_LATIN_ISO8859_2 =
    CHARSET_ID_LATIN_ISO8859_1 + 1,  /* 0x82 Right half of ISO 8859-2 */
  CHARSET_ID_LATIN_ISO8859_3,        /* 0x83 Right half of ISO 8859-3 */
  CHARSET_ID_LATIN_ISO8859_4,        /* 0x84 Right half of ISO 8859-4 */
  CHARSET_ID_CYRILLIC_ISO8859_5,     /* 0x85 Right half of ISO 8859-5 */
  CHARSET_ID_THAI_TIS620,            /* 0x86 Right half of Thai TIS-620 */
  CHARSET_ID_GREEK_ISO8859_7,        /* 0x87 Right half of ISO 8859-7 */
  CHARSET_ID_HEBREW_ISO8859_8,       /* 0x88 Right half of ISO 8859-8 */
  CHARSET_ID_LATIN_ISO8859_9,        /* 0x89 Right half of ISO 8859-9 */
  CHARSET_ID_LATIN_ISO8859_15,       /* 0x8A Right half of ISO 8859-15 */
  CHARSET_ID_LATIN_JISX0201,         /* 0x8B Left  half of JIS X0201-1976 */
  CHARSET_ID_KATAKANA_JISX0201,      /* 0x8C Right half of JIS X0201-1976 */
  /* The following is dimension-2 with ENABLE_COMPOSITE_CHARS but
     dimension-1 otherwise.  All above are dimension-1; all below are
     dimension-2. */
  CHARSET_ID_COMPOSITE,              /* 0x8D Composite characters or a
					  fake set that replaces ESC 0 -
					  ESC 4 in a buffer */
  CHARSET_ID_JAPANESE_JISX0208_1978, /* 0x8E Japanese JIS X0208-1978 */
  CHARSET_ID_CHINESE_GB2312,         /* 0x8F Chinese Hanzi GB2312-1980 */
  CHARSET_ID_JAPANESE_JISX0208,      /* 0x90 Japanese JIS X0208-1983 */
  CHARSET_ID_KOREAN_KSC5601,         /* 0x91 Hangul KS C5601-1987 */
  CHARSET_ID_JAPANESE_JISX0212,      /* 0x92 Japanese JIS X0212-1990 */
  CHARSET_ID_CHINESE_CNS11643_1,     /* 0x93 Chinese CNS11643 Set 1 */
  CHARSET_ID_CHINESE_CNS11643_2,     /* 0x94 Chinese CNS11643 Set 2 */
  CHARSET_ID_CHINESE_BIG5_1,         /* 0x95 Big5 Level 1 */
  CHARSET_ID_CHINESE_BIG5_2,         /* 0x96 Big5 Level 2 */
  CHARSET_ID_UNICODE_0_23FF,	     /* 0x97 Unicode 0-23FF */
  CHARSET_ID_UNICODE_2400_47FF,      /* 0x98 Unicode 2400-47FF */
  CHARSET_ID_UNICODE_4800_6BFF,      /* 0x99 Unicode 4800-6BFF */
  CHARSET_ID_UNICODE_6C00_8FFF,      /* 0x9A Unicode 6C00-8FFF */
  CHARSET_ID_UNICODE_9000_B3FF,      /* 0x9B Unicode 9000-B3FF */
  CHARSET_ID_UNICODE_B400_D7FF,      /* 0x9C Unicode B400-D7FF */
  CHARSET_ID_UNICODE_E000_103FF,     /* 0x9D Unicode E000-103FF */
};

#endif /* not UNICODE_INTERNAL */

/* This is a flag to make_charset(), currently only for sanity checking */
enum charset_internal_external
  {
    CSET_INTERNAL = -1,
    CSET_EXTERNAL = -2,
  };

struct charset_lookup *chlook;

static const struct memory_description charset_lookup_description_1[] = {
  { XD_LISP_OBJECT_ARRAY,
    offsetof (struct charset_lookup, charset_by_attributes), 4*128*2 },
#ifndef UNICODE_INTERNAL
  { XD_LISP_OBJECT_ARRAY,
    offsetof (struct charset_lookup, charset_by_encodable_id),
    NUM_ENCODABLE_CHARSET_IDS },
#endif /* not UNICODE_INTERNAL */
  { XD_END }
};

static const struct sized_memory_description charset_lookup_description = {
  sizeof (struct charset_lookup),
  charset_lookup_description_1
};

static int next_charset_id;

Lisp_Object Qcharsetp;

/* Qdoc_string, Qdimension, Qchars, Qfinal defined in general.c */
Lisp_Object Qregistries, Qgraphic, Qregistry;
Lisp_Object Qdirection;
Lisp_Object Qreverse_direction_charset;
Lisp_Object Qshort_name, Qlong_name, Qunicode_map;

Lisp_Object Qto_unicode;

Lisp_Object
  Qlatin_iso8859_1,
  Qlatin_iso8859_2,
  Qlatin_iso8859_3,
  Qlatin_iso8859_4,
  Qthai_tis620,
  Qarabic_iso8859_6,
  Qgreek_iso8859_7,
  Qhebrew_iso8859_8,
  Qkatakana_jisx0201,
  Qlatin_jisx0201,
  Qcyrillic_iso8859_5,
  Qlatin_iso8859_9,
  Qlatin_iso8859_15,
  Qjapanese_jisx0208_1978,
  Qchinese_gb2312,
  Qjapanese_jisx0208,
  Qkorean_ksc5601,
  Qjapanese_jisx0212,
  Qchinese_cns11643_1,
  Qchinese_cns11643_2,
#ifdef UNICODE_INTERNAL
  Qchinese_big5,
  Qjapanese_shift_jis,
#else /* not UNICODE_INTERNAL */
  Qchinese_big5_1,
  Qchinese_big5_2,
#endif /* UNICODE_INTERNAL */
  Qchinese_sisheng,
  Qcomposite;

Lisp_Object Ql2r, Qr2l;

Lisp_Object Vcharset_hash_table, Vcharset_id_table;


/************************************************************************/
/*                            charset object                            */
/************************************************************************/

static Lisp_Object
mark_charset (Lisp_Object obj)
{
  Lisp_Charset *cs = XCHARSET (obj);

  mark_object (cs->short_name);
  mark_object (cs->long_name);
  mark_object (cs->unicode_map);
  mark_object (cs->doc_string);
  mark_object (cs->registries);
#ifdef HAVE_CCL
  mark_object (cs->ccl_program);
#endif /* HAVE_CCL */
  return cs->name;
}

static void
print_charset (Lisp_Object obj, Lisp_Object printcharfun,
	       int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_lcrecord
      (obj, XSTRING_DATA (XSYMBOL (XCHARSET_NAME (obj))->name));

  write_fmt_string_lisp (printcharfun, "#<charset %s %S %S %S", 4,
			 XCHARSET_NAME (obj), XCHARSET_SHORT_NAME (obj),
			 XCHARSET_LONG_NAME (obj), XCHARSET_DOC_STRING (obj));
  if (XCHARSET_DIMENSION (obj) == 1)
    write_fmt_string (printcharfun, " %d", XCHARSET_CHARS (obj, 1));
  else
    write_fmt_string (printcharfun, " %dx%d", XCHARSET_CHARS (obj, 0),
		      XCHARSET_CHARS (obj, 1));
  write_fmt_string (printcharfun, " %s cols=%d g%d ",
		    XCHARSET_DIRECTION (obj) == CHARSET_LEFT_TO_RIGHT ? "l2r" :
		    "r2l",
		    XCHARSET_COLUMNS (obj),
		    XCHARSET_GRAPHIC (obj));
  if (XCHARSET_FINAL (obj))
    write_fmt_string (printcharfun, "final='%c' ", XCHARSET_FINAL (obj));
  write_fmt_string (printcharfun, "reg=");
  print_internal (XCHARSET_REGISTRIES (obj), printcharfun, 0);
  write_fmt_string (printcharfun, " 0x%x>", XCHARSET (obj)->header.uid);
}

static const struct memory_description charset_description[] = {
  { XD_INT, offsetof (Lisp_Charset, dimension) },
  { XD_INT, offsetof (Lisp_Charset, from_unicode_levels) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, doc_string) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, registries) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, short_name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, long_name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, unicode_map) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, reverse_direction_charset) },
#ifdef HAVE_CCL
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, ccl_program) },
#endif /* HAVE_CCL */
  { XD_UNION, offsetof (Lisp_Charset, to_unicode_table),
#ifdef MAXIMIZE_UNICODE_TABLE_DEPTH
    2,
#else
    XD_INDIRECT (0, 0),
#endif /* MAXIMIZE_UNICODE_TABLE_DEPTH */
    { &to_unicode_description }, XD_FLAG_NO_KKCC },
  { XD_UNION, offsetof (Lisp_Charset, from_unicode_table),
    XD_INDIRECT (1, 0), { &from_unicode_description }, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("charset", charset,
			       1, /* dumpable flag */
                               mark_charset, print_charset, 0,
			       0, 0, charset_description, Lisp_Charset);

#ifndef UNICODE_INTERNAL

static int
get_unallocated_charset_id (int dimension)
{
  if (dimension == 1)
    {
      if (chlook->next_allocated_private_dim1_id > MAX_PRIVATE_DIM1_CHARSET_ID)
	return 0;
      else
	return chlook->next_allocated_private_dim1_id++;
    }
  else
    {
      text_checking_assert (dimension == 2);
      if (chlook->next_allocated_private_dim2_id > MAX_PRIVATE_DIM2_CHARSET_ID)
	return 0;
      else
	return chlook->next_allocated_private_dim2_id++;
    }

}

#endif /* not UNICODE_INTERNAL */


/************************************************************************/
/*                      Basic charset Lisp functions                    */
/************************************************************************/

/* Return the inclusive limits of the indexing bounds on the charset.
   Note that a one-dimension charset is of size [1xN], i.e. only the
   second index varies. */

void
get_charset_limits (Lisp_Object charset, int *low0, int *high0,
		    int *low1, int *high1)
{
  *low0 = XCHARSET_OFFSET (charset, 0);
  *low1 = XCHARSET_OFFSET (charset, 1);
  *high0 = *low0 + XCHARSET_CHARS (charset, 0) - 1;
  *high1 = *low1 + XCHARSET_CHARS (charset, 1) - 1;
#ifdef ERROR_CHECK_STRUCTURES
  if (XCHARSET_DIMENSION (charset) == 1)
    {
      assert (*low0 == 0);
      assert (*high0 == 0);
    }
#endif /* ERROR_CHECK_STRUCTURES */
}

/* Return the ISO-2022 type of the charset, one of CHARSET_TYPE_94,
   CHARSET_TYPE_96, CHARSET_TYPE_94X94, CHARSET_TYPE_96X96, or -1.
   Charset must be of the right size with the right offsets, and have
   a final. */

static int
get_charset_iso2022_type_1 (int dimension, int size0, int size1, int offset0,
			    int offset1, int final)
{
  int type;

  offset0 &= 127;
  offset1 &= 127;

  if (dimension == 1)
    {
      assert (size0 == 1);
      assert (offset0 == 0);
      if (size1 == 94 && offset1 == 33)
	type = CHARSET_TYPE_94;
      else if (size1 == 96 && offset1 == 32)
	type = CHARSET_TYPE_96;
      else
	return -1;
    }
  else
    {
      text_checking_assert (dimension == 2);
      if (size0 == 94 && offset0 == 33 &&
	  size1 == 94 && offset1 == 33)
	type = CHARSET_TYPE_94X94;
      else if (size0 == 96 && offset0 == 32 &&
	       size1 == 96 && offset1 == 32)
	type = CHARSET_TYPE_96X96;
      else
	return -1;
    }

  if (final)
    return type;
  else
    return -1;
}

/* Return the ISO-2022 type (94, 96, 94x94 or 96x96) charset, or -1 if not
   ISO-2022 compatible.  NOTE: This is *NOT* the same as whether an
   existing charset can be encoded in old-Mule.  That can only be
   determined by looking at the charset ID and seeing if it's in the
   encodable range (i.e. <= MAX_ENCODABLE_CHARSET_ID).  Encodable
   charsets may not be ISO-2022 compatible (e.g. no final) and ISO-2022
   compatible charsets may not encodable (e.g. no more charset ID's
   in the encodable range). */

int
get_charset_iso2022_type (Lisp_Object cs)
{
  if (EQ (cs, Vcharset_ascii))
    return CHARSET_TYPE_94; /* Yes, even though it has 128 characters in it */
  return get_charset_iso2022_type_1 (XCHARSET_DIMENSION (cs),
				     XCHARSET_CHARS (cs, 0),
				     XCHARSET_CHARS (cs, 1),
				     XCHARSET_OFFSET (cs, 0),
				     XCHARSET_OFFSET (cs, 1),
				     XCHARSET_FINAL (cs));
}

#if !defined (UNICODE_INTERNAL)

/* Return true if a charset can be encoded as a string or character in the
   old-Mule encoding, given the dimension, size and offset of the charset.
   Currently, this is similar to but not the same as whether it can be
   encoded as ISO-2022, since the presence of a final byte is
   unnecessary. @@#### In reality, any charset no bigger than 96x96 can be
   so encoded; we should relax this restriction. */

static int
old_mule_charset_encodable_by_properties (int dimension, int size0, int size1,
					  int offset0, int offset1)
{
  int size0_ok, size1_ok;

  offset0 &= 127;
  offset1 &= 127;

  size0_ok = (size0 == 94 && offset0 == 33) || (size0 == 96 && offset0 == 32);
  size1_ok = (size1 == 94 && offset1 == 33) || (size1 == 96 && offset1 == 32);

  
  if (dimension == 1)
    return size1_ok;
  else
    {
      text_checking_assert (dimension == 2);
      return size0_ok && size1_ok;
    }
}

int
old_mule_charset_encodable (Lisp_Object charset)
{
  return XCHARSET_ID (charset) <= MAX_ENCODABLE_CHARSET_ID;
}

#endif /* !defined (UNICODE_INTERNAL) */
     
DEFUN ("charsetp", Fcharsetp, 1, 1, 0, /*
Return non-nil if OBJECT is a charset.
*/
       (object))
{
  return CHARSETP (object) ? Qt : Qnil;
}

DEFUN ("find-charset", Ffind_charset, 1, 1, 0, /*
Retrieve the charset of the given name.
If CHARSET-OR-NAME is a charset object, it is simply returned.
Otherwise, CHARSET-OR-NAME should be a symbol.  If there is no such charset,
nil is returned.  Otherwise the associated charset object is returned.
*/
       (charset_or_name))
{
  if (CHARSETP (charset_or_name))
    return charset_or_name;

  CHECK_SYMBOL (charset_or_name);
  return Fgethash (charset_or_name, Vcharset_hash_table, Qnil);
}

DEFUN ("get-charset", Fget_charset, 1, 1, 0, /*
Retrieve the charset of the given name.
Same as `find-charset' except an error is signalled if there is no such
charset instead of returning nil.
*/
       (name))
{
  Lisp_Object charset = Ffind_charset (name);

  if (NILP (charset))
    invalid_argument ("No such charset", name);
  return charset;
}

/* We store the charsets in hash tables with the names as the key and the
   actual charset object as the value.  Occasionally we need to use them
   in a list format.  These routines provide us with that. */
struct charset_list_closure
{
  Lisp_Object *charset_list;
};

static int
add_charset_to_list_mapper (Lisp_Object UNUSED (key), Lisp_Object value,
			    void *charset_list_closure)
{
  struct charset_list_closure *chcl =
    (struct charset_list_closure*) charset_list_closure;
  Lisp_Object *charset_list = chcl->charset_list;

  *charset_list = Fcons (XCHARSET_NAME (value), *charset_list);
  return 0;
}

DEFUN ("charset-list", Fcharset_list, 0, 0, 0, /*
Return a list of the names of all defined charsets.
*/
       ())
{
  Lisp_Object charset_list = Qnil;
  struct charset_list_closure charset_list_closure;

  charset_list_closure.charset_list = &charset_list;
  elisp_maphash (add_charset_to_list_mapper, Vcharset_hash_table,
		 &charset_list_closure);

  return charset_list;
}

DEFUN ("charset-name", Fcharset_name, 1, 1, 0, /*
Return the name of charset CHARSET.
*/
       (charset))
{
  return XCHARSET_NAME (Fget_charset (charset));
}

static void
validate_charset_offset_or_size (Lisp_Object keyword, Lisp_Object value,
				 int *dim0, int *dim1)
{
  int minval, maxval;

  if (EQ (keyword, Qchars))
    minval = 1, maxval = 256;
  else
    {
      assert (EQ (keyword, Qoffset));
      minval = 0, maxval = 255;
    }

  if (INTP (value))
    {
      *dim0 = *dim1 = XINT (value);
      if (*dim0 < minval || *dim0 > maxval)
	goto bzzzzt;
    }
  else
    {
      int len = 0;
      Lisp_Object tem;
      {
	EXTERNAL_LIST_LOOP_1 (value)
	  len++;
      }
      if (len < 1 || len > 2)
	invalid_constant_2
	  ("Invalid value for property (list of 1 or 2 integers)",
	   keyword, value);
      tem = X1ST (value);
      CHECK_INT (tem);
      *dim0 = *dim1 = XINT (tem);
      if (*dim0 < minval || *dim0 > maxval)
	{
	  value = tem;
	  goto bzzzzt;
	}
      if (len == 2)
	{
	  tem = X2ND (value);
	  CHECK_INT (tem);
	  *dim1 = XINT (tem);
	  if (*dim1 < minval || *dim1 > maxval)
	    {
	      value = tem;
	      goto bzzzzt;
	    }
	}
    }

  return;

 bzzzzt:
  if (maxval == 256)
    invalid_constant_2 ("Invalid value for property (1-256)", keyword,
			value);
  else
    invalid_constant_2 ("Invalid value for property (0-255)", keyword,
			value);
}

Lisp_Object
charset_by_id (int id)
{
  return Fgethash (make_int (id), Vcharset_id_table, Qnil);
}
		    
/* Make a new charset.  ID is the charset ID, or -1 to find a new one. */
/* #### SJT Should generic properties be allowed? */
static Lisp_Object
make_charset (int id, int no_init_unicode_tables,
	      Lisp_Object name, int dimension,
	      int size0, int size1, int offset0, int offset1, int columns,
	      int graphic, Ibyte final, int direction,
	      Lisp_Object short_name, Lisp_Object long_name,
	      Lisp_Object doc_string, Lisp_Object registries,
	      Lisp_Object unicode_map, int overwrite,
	      int algo_low_or_internal_p
	      )
{
  Lisp_Object obj;
  int type = get_charset_iso2022_type_1 (dimension, size0, size1,
					 offset0, offset1, final);

  /* Major hack; ISO-2022 pretends that ASCII is a 94-byte charset when it's
     obviously not. */

  if (EQ (name, Qascii))
    type = CHARSET_TYPE_94;

  if (id < 0)
    {
#ifdef UNICODE_INTERNAL
      id = next_charset_id++;
#else
      if (!old_mule_charset_encodable_by_properties (dimension, size0, size1,
						     offset0, offset1))
	/* Make sure the ID's are above all encodable ID's */
	id = 1 + MAX_ENCODABLE_CHARSET_ID + next_charset_id++;
      else
	{
	  id = get_unallocated_charset_id (dimension);
	  if (id == 0)
	    id = 1 + MAX_ENCODABLE_CHARSET_ID + next_charset_id++;
	}
#endif /* not UNICODE_INTERNAL */
    }

  /* If this is a temporary place-holder charset, assign a temporary name
     based on the id. */
  if (EQ (name, Qunbound))
    {
      Ibyte tempname[80];

      qxesprintf (tempname, "___temporary___%d__", id);
      name = intern_int (tempname);
    }

  /* Set certain other default values.  SHORT_NAME cannot be computed
     until the actual name is generated (just above, for temporaries). */
  if (NILP (doc_string))
    doc_string = build_string ("");
  if (NILP (registries))
    /* @@#### Why shouldn't this be a list? */
    registries = make_vector (0, Qnil);
  if (NILP (short_name))
    short_name = XSYMBOL (name)->name;
  if (NILP (long_name))
    long_name = short_name;
  if (columns == -1)
    columns = dimension;

  if (!overwrite)
    {
      obj = wrap_charset (ALLOC_LCRECORD_TYPE (Lisp_Charset, &lrecord_charset));

      if (final)
	{
	  /* some charsets do not have final characters.  This includes
	     ASCII, Control-1, Composite, the two faux private charsets,
	     and various others, in the new, better world. */
	  assert (NILP (charset_by_attributes (type, final, direction)));
	  chlook->charset_by_attributes[type][final][direction] = obj;
	}

#ifndef UNICODE_INTERNAL
      if (id <= MAX_ENCODABLE_CHARSET_ID)
	{
	  assert (NILP (chlook->charset_by_encodable_id
			[id - MIN_ENCODABLE_CHARSET_ID]));
	  chlook->charset_by_encodable_id[id - MIN_ENCODABLE_CHARSET_ID] = obj;
	}
#endif /* not UNICODE_INTERNAL */
    }
  else
    {
      Lisp_Object ret;
      /* We should only be called this way from the ISO-2022 code */
      text_checking_assert (final > 0);
      /* Actually overwrite the properties of the existing charset.
	 We do this because until now charsets could never be "deleted",
	 so parts of the code don't bother to GC charsets. */
      obj = charset_by_attributes (type, final, direction);
#ifndef UNICODE_INTERNAL
      if (id <= MAX_ENCODABLE_CHARSET_ID)
	{
	  assert (EQ (chlook->charset_by_encodable_id
		      [id - MIN_ENCODABLE_CHARSET_ID],
		      obj));
	}
#endif /* not UNICODE_INTERNAL */

      ret = Fremhash (XCHARSET_NAME (obj), Vcharset_hash_table);
      assert (!NILP (ret));
      ret = Fremhash (make_int (id), Vcharset_id_table);
      assert (!NILP (ret));
    }

  XCHARSET_ID		(obj) = id;
  XCHARSET_NAME		(obj) = name;
  XCHARSET_SHORT_NAME	(obj) = short_name;
  XCHARSET_LONG_NAME	(obj) = long_name;
  XCHARSET_DIRECTION	(obj) = direction;
  XCHARSET_CHARS        (obj, 0) = size0;
  XCHARSET_CHARS        (obj, 1) = size1;
  XCHARSET_OFFSET       (obj, 0) = offset0;
  XCHARSET_OFFSET       (obj, 1) = offset1;
  XCHARSET_COLUMNS	(obj) = columns;
  XCHARSET_GRAPHIC	(obj) = graphic;
  XCHARSET_FINAL	(obj) = final;
  XCHARSET_DOC_STRING	(obj) = doc_string;
  XCHARSET_REGISTRIES	(obj) = registries;

  /* Sanity checking -- for internal charsets, make sure that the offset
     range agrees with the graphic range.  We don't do this for external
     charsets because the user might choose to set the values so they
     don't correspond. */
  if (algo_low_or_internal_p == CSET_INTERNAL)
    {
      if (dimension == 2)
	assert ((graphic == 0 && offset0 < 128) ||
		(graphic == 1 && offset0 >= 128));
      assert ((graphic == 0 && offset1 < 128) ||
	      (graphic == 1 && offset1 >= 128));
    }
  if (algo_low_or_internal_p < 0)
    algo_low_or_internal_p = -1;
#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
  XCHARSET_ALGO_LOW	(obj) = algo_low_or_internal_p;
#endif /* ALLOW_ALGORITHMIC_CONVERSION_TABLES */
#ifdef HAVE_CCL
  XCHARSET_CCL_PROGRAM	(obj) = Qnil;
#endif /* HAVE_CCL */
  XCHARSET_REVERSE_DIRECTION_CHARSET (obj) = Qnil;

  XCHARSET_DIMENSION (obj) = dimension;

  if (no_init_unicode_tables)
    assert (!overwrite);
  else
    {
      if (overwrite)
	free_charset_unicode_tables (obj);
      init_charset_unicode_tables (obj);
    }

  assert (NILP (Fgethash (make_int (id), Vcharset_id_table, Qnil)));
  Fputhash (make_int (id), obj, Vcharset_id_table);

  /* [[ Some charsets are "faux" and don't have names or really exist at
     all except in the charset ID table. ]]
     #### Explain this comment! --ben */
  assert (!NILP (name)); /* @@#### to figure out what's going on */
  if (!NILP (name))
    {
      assert (NILP (Fgethash (name, Vcharset_hash_table, Qnil)));
      Fputhash (name, obj, Vcharset_hash_table);
    }

  recalculate_unicode_precedence ();
  setup_charset_initial_specifier_tags (obj);

  /* Must be the very last thing we do, since it can signal an error in
     case `unicode-map' was given invalidly; then the charset will have
     no unicode map set */
  if (!NILP (unicode_map))
    init_charset_unicode_map (obj, unicode_map);

  return obj;
}

/* #### SJT Should generic properties be allowed? */
DEFUN ("make-charset", Fmake_charset, 3, 3, 0, /*
Define a new national character set.
This function is for use with international support.
With a Unicode-based engine, these charsets are used mostly in the codecs
that read-in and write-out text formatting according to one or another
national character sets.

NAME is a symbol, the name by which the character set is normally referred.
DOC-STRING is a string describing the character set.
PROPS is a property list, describing the specific nature of the
character set.  Recognized properties are:

`short-name'	Short version of the charset name (ex: Latin-1)
`long-name'	Long version of the charset name (ex: ISO8859-1 (Latin-1))
`registries'	Under X Windows, a vector of possible XLFD REGISTRY-ENCODING
                combinations for this character set.  Note that this is not a
                regular expression.
`dimension'	Number of octets used to index a character in this charset.
		Either 1 or 2.  Defaults to 1.
`chars'		Number of characters in each dimension.  This should be a
                single integer or a list of two integers, when dimension == 2.
                For a square character set, a single integer may be specified
                even when dimension == 2.  The largest possible value for
                each dimension is 256.  Defaults to 94.
`offset'        Minimum index in each dimension.  This should be a single
                integer or a list of integers, as for `chars'.  The possible
                indices in the appropriate dimension, as used in `make-char'
                and returned in `split-char', are between OFFSET and
                OFFSET + CHARS - 1, inclusive.  The default is derived from
		the size of this dimension and from the `graphic' property;
		if the `graphic' is 0, the default is 33 if the size
                of this dimension is <= 94, 32 if the size is 95 or 96, and
                0 otherwise.  If the `graphic' is 1, the defaults are 161,
		160 and 128, respectively.
`columns'	Number of columns used to display a character in this charset.
		Only used in TTY mode. (Under X, the actual width of a
		character can be derived from the font used to display the
		characters.) If unspecified, defaults to the dimension
		(this is almost	always the correct value).
`final'		Relevant only for ISO-2022-compatible charsets (those of size
                94, 96, 94x94 or 96x96), and allowed only for these charsets.
		Indicates the final byte of ISO 2022 escape sequence used to
		select this charset.  Note that each of these four sizes
		defines a separate namespace for final bytes.  Note also that
		ISO-2022 restricts the final byte to the range 0x30 - 0x7E if
                dimension == 1, and 0x30 - 0x5F if dimension == 2.  Also,
		final bytes in the range 0x30 - 0x3F are reserved for user-
		defined (not official) character sets.
`graphic'	Relevant mostly to ISO-2022.  Value should be 0 (use bytes
                with the high bit cleared) or 1 (use bytes with the high bit
		set).  This controls which register the charset will be
		selected into.  Generally, this should match up with the
		`offset' property.  If `offset' is specified but `graphic'
		is not, the value will be derived from the value of
		`offset': If `offset' is >= 128, the value of `graphic'
		will be 1, else 0.
`direction'	`l2r' (left-to-right) or `r2l' (right-to-left).
		Defaults to `l2r'.
`unicode-map'   Information describing how to map this charset to/from
                Unicode.
`ccl-program'	A compiled CCL program used to convert a character in
		this charset into an index into the font.  The CCL program
		is passed the octets of the character, which will be within
                the limits established by `offset' and `chars'.  CCL is not
                available when (featurep 'unicode-internal).
*/
       (name, doc_string, props))
{
  int id = -1, dimension = 1, size0 = 94, size1 = 94, graphic = -1,
    columns = -1, offset0 = -1, offset1 = -1;
  Ibyte final = 0;
  int direction = CHARSET_LEFT_TO_RIGHT;
  int type;
  Lisp_Object registries = Qnil;
  Lisp_Object unicode_map = Qnil;
  Lisp_Object charset = Qnil;
#ifdef HAVE_CCL
  Lisp_Object ccl_program = Qnil;
#endif /* HAVE_CCL */
  Lisp_Object short_name = Qnil, long_name = Qnil;
  Lisp_Object existing_charset = Qnil;
  int temporary = UNBOUNDP (name);

  /* NOTE: name == Qunbound is a directive from the iso2022 code to
     create a temporary charset for an unknown final.  We allow the final
     to be overwritten with a real charset later on. */

  if (!NILP (doc_string))
    CHECK_STRING (doc_string);
  if (!UNBOUNDP (name))
    {
      CHECK_SYMBOL (name);

      charset = Ffind_charset (name);
      if (!NILP (charset))
	invalid_operation ("Cannot redefine existing charset", name);
    }

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (keyword, value, props)
      {
	if (EQ (keyword, Qshort_name))
	  {
	    CHECK_STRING (value);
	    short_name = value;
	  }

	else if (EQ (keyword, Qlong_name))
	  {
	    CHECK_STRING (value);
	    long_name = value;
	  }

	else if (EQ (keyword, Qdimension))
	  {
	    CHECK_INT (value);
	    dimension = XINT (value);
	    if (dimension < 1 || dimension > 2)
	      invalid_constant ("Invalid value for `dimension'", value);
	  }
	else if (EQ (keyword, Qchars))
	  {
	    validate_charset_offset_or_size (Qchars, value, &size0, &size1);
	  }
	else if (EQ (keyword, Qoffset))
	  {
	    validate_charset_offset_or_size (Qoffset, value, &offset0,
					     &offset1);
	  }
	else if (EQ (keyword, Qcolumns))
	  {
	    CHECK_INT (value);
	    columns = XINT (value);
	    if (columns != 1 && columns != 2)
	      invalid_constant ("Invalid value for `columns'", value);
	  }

	else if (EQ (keyword, Qgraphic))
	  {
	    CHECK_INT (value);
	    graphic = XINT (value);
	    if (graphic < 0 || graphic > 1)
	      invalid_constant ("Invalid value for `graphic'", value);
	  }

	else if (EQ (keyword, Qregistries))
	  {
	    CHECK_VECTOR (value);
	    registries = value;
	  }
	
	else if (EQ (keyword, Qunicode_map))
	  {
	    CHECK_TRUE_LIST (value);
	    /* It will get validated further */
	    unicode_map = value;
	  }
	
	else if (EQ (keyword, Qregistry))
	  {
	    Lisp_Object quoted_registry; 

	    CHECK_STRING (value);
	    quoted_registry = Fregexp_quote (value);
	    if (qxestrcmp (XSTRING_DATA (quoted_registry),
			   XSTRING_DATA (value)))
	      {
		warn_when_safe 
		  (Qregistry, Qwarning,
		   "Regexps no longer allowed for charset-registry.  "
		   "Treating %s as string", XSTRING_DATA (value));
	      }
	    registries = vector1 (value);
	  }

	else if (EQ (keyword, Qdirection))
	  {
	    if (EQ (value, Ql2r))
	      direction = CHARSET_LEFT_TO_RIGHT;
	    else if (EQ (value, Qr2l))
	      direction = CHARSET_RIGHT_TO_LEFT;
	    else
	      invalid_constant ("Invalid value for `direction'", value);
	  }

	else if (EQ (keyword, Qfinal))
	  {
	    CHECK_CHAR_COERCE_INT (value);
	    final = XCHAR (value);
	    if (final < '0' || final > '~')
	      invalid_constant ("Invalid value for `final'", value);
	  }

#ifdef HAVE_CCL
	else if (EQ (keyword, Qccl_program))
	  {
            /* This errors if VALUE is not a valid CCL program. */
	    ccl_program = get_ccl_program (value);
	  }
#endif /* HAVE_CCL */
	else
	  invalid_constant ("Unrecognized property", keyword);
      }
  }

  if (dimension == 2 && final > 0x5F)
    invalid_constant
      ("Final must be in the range 0x30 - 0x5F for dimension == 2",
       make_char (final));

  /* Dimension-1 charsets are of size [1xN]; doing this way, rather than
     [NX1], greatly simplifies many things. */
  if (dimension == 1)
    {
      size0 = 1;
      offset0 = 0;
    }

  /* Derive the offset from the graphic, or vice-versa.  If neither is
     given, assume graphic is 0 and proceed from there. #### Perhaps we
     should signal an error in this case. */
  if (offset1 < 0 && graphic < 0)
    graphic = 0;
  if (offset0 < 0)
    {
      assert (graphic >= 0);
      offset0 = graphic == 0 ?
	(size0 <= 94 ? 33 : size0 == 95 || size0 == 96 ? 32 : 0) :
	(size0 <= 94 ? 161 : size0 == 95 || size0 == 96 ? 160 : 128);
    }
  if (offset1 < 0)
    {
      assert (graphic >= 0);
      offset1 = graphic == 0 ?
	(size1 <= 94 ? 33 : size1 == 95 || size1 == 96 ? 32 : 0) :
	(size1 <= 94 ? 161 : size1 == 95 || size1 == 96 ? 160 : 128);
    }
  if (size0 + offset0 > 256 || size1 + offset1 > 256)
    invalid_argument
      ("Offset + size cannot exceed 256 in any dimension", name);
  if (graphic < 0)
    {
      assert (offset0 >= 0 && offset1 >= 0);
      if (offset1 >= 128)
	graphic = 1;
      else
	graphic = 0;
    }

  type = get_charset_iso2022_type_1 (dimension, size0, size1,
				     offset0, offset1, final);
  if (final)
    {
      if (type == -1)
	invalid_argument ("Final can only be specified for ISO-2022 charsets",
			  make_char (final));
      existing_charset =
	charset_by_attributes (type, final, direction);

      if (!NILP (existing_charset) && !XCHARSET (existing_charset)->temporary)
	invalid_argument
	  ("Charset already defined for this DIMENSION/CHARS/FINAL/DIRECTION combo",
	   existing_charset);

      if (!NILP (existing_charset))
	/* Reuse same charset ID */
	id = XCHARSET_ID (existing_charset);
    }

  charset = make_charset (id, 0, name, dimension, size0, size1, offset0,
			  offset1, columns, graphic,
			  final, direction, short_name, long_name,
			  doc_string, registries, unicode_map,
			  !NILP (existing_charset),
			  CSET_EXTERNAL);

  XCHARSET (charset)->temporary = temporary;
#ifdef HAVE_CCL
  if (!NILP (ccl_program))
    XCHARSET_CCL_PROGRAM (charset) = ccl_program;
#endif /* HAVE_CCL */

  if (final)
    {
      Lisp_Object revdircs =
	charset_by_attributes (type, final,
			       direction == CHARSET_LEFT_TO_RIGHT ?
			       CHARSET_RIGHT_TO_LEFT : CHARSET_LEFT_TO_RIGHT);
      if (!NILP (revdircs))
	{
	  XCHARSET_REVERSE_DIRECTION_CHARSET (revdircs) = charset;
	  XCHARSET_REVERSE_DIRECTION_CHARSET (charset) = revdircs;
	}
    }

  return charset;
}

DEFUN ("make-reverse-direction-charset", Fmake_reverse_direction_charset,
       2, 2, 0, /*
Make a charset equivalent to CHARSET but which goes in the opposite direction.
NEW-NAME is the name of the new charset.  Return the new charset.
*/
       (charset, new_name))
{
  Lisp_Object new_charset = Qnil;
  int dimension, columns, graphic;
  Ibyte final;
  int direction;
  Lisp_Object registries, doc_string, short_name, long_name, unicode_map;
  Lisp_Charset *cs;

  charset = Fget_charset (charset);
  if (!NILP (XCHARSET_REVERSE_DIRECTION_CHARSET (charset)))
    invalid_operation ("Charset already has reverse-direction charset",
		       charset);

  CHECK_SYMBOL (new_name);
  if (!NILP (Ffind_charset (new_name)))
    invalid_operation ("Cannot redefine existing charset", new_name);

  cs = XCHARSET (charset);

  columns   = CHARSET_COLUMNS   (cs);
  dimension = CHARSET_DIMENSION (cs);

  graphic = CHARSET_GRAPHIC (cs);
  final = CHARSET_FINAL (cs);
  direction = CHARSET_RIGHT_TO_LEFT;
  if (CHARSET_DIRECTION (cs) == CHARSET_RIGHT_TO_LEFT)
    direction = CHARSET_LEFT_TO_RIGHT;
  doc_string = CHARSET_DOC_STRING (cs);
  short_name = CHARSET_SHORT_NAME (cs);
  long_name = CHARSET_LONG_NAME (cs);
  registries = CHARSET_REGISTRIES (cs);
  unicode_map = CHARSET_UNICODE_MAP (cs);

  new_charset = make_charset (-1, 0, new_name, dimension,
			      CHARSET_CHARS (cs, 0), CHARSET_CHARS (cs, 1),
			      CHARSET_OFFSET (cs, 0), CHARSET_OFFSET (cs, 1),
			      columns, graphic, final, direction, short_name,
			      long_name, doc_string, registries,
			      unicode_map, 0, CSET_EXTERNAL);

  CHARSET_REVERSE_DIRECTION_CHARSET (cs) = new_charset;
  XCHARSET_REVERSE_DIRECTION_CHARSET (new_charset) = charset;

  return new_charset;
}

DEFUN ("charset-reverse-direction-charset", Fcharset_reverse_direction_charset,
       1, 1, 0, /*
Return the reverse-direction charset parallel to CHARSET, if any.
This is the charset with the same properties (in particular, the same
dimension, number of characters per dimension, and final byte) as
CHARSET but whose characters are displayed in the opposite direction.
*/
       (charset))
{
  charset = Fget_charset (charset);
  return XCHARSET_REVERSE_DIRECTION_CHARSET (charset);
}

DEFUN ("charset-from-attributes", Fcharset_from_attributes, 3, 4, 0, /*
Return a charset with the given DIMENSION, CHARS, FINAL, and DIRECTION.
This applies to ISO-2022 conversion only.  DIMENSION should be 1 or 2.
CHARS must be 94 or 96.  DIRECTION is `r2l' or `l2r'.  If DIRECTION is
omitted, both directions will be checked (left-to-right will be returned if
character sets exist for both directions).
*/
       (dimension, chars, final, direction))
{
  int dm, ch, fi, di = -1;
  int type;
  Lisp_Object obj = Qnil;

  CHECK_INT (dimension);
  dm = XINT (dimension);
  if (dm < 1 || dm > 2)
    invalid_constant ("Invalid value for DIMENSION", dimension);

  CHECK_INT (chars);
  ch = XINT (chars);
  if (ch != 94 && ch != 96)
    invalid_constant ("Invalid value for CHARS, must be 94 or 96", chars);

  CHECK_CHAR_COERCE_INT (final);
  fi = XCHAR (final);
  if (fi < '0' || fi > '~')
    invalid_constant ("Invalid value for FINAL", final);

  if (EQ (direction, Ql2r))
    di = CHARSET_LEFT_TO_RIGHT;
  else if (EQ (direction, Qr2l))
    di = CHARSET_RIGHT_TO_LEFT;
  else if (!NILP (direction))
    invalid_constant ("Invalid value for DIRECTION", direction);

  if (dm == 2 && fi > 0x5F)
    invalid_constant
      ("Final must be in the range 0x30 - 0x5F for dimension == 2", final);

  if (dm == 1)
    type = (ch == 94) ? CHARSET_TYPE_94    : CHARSET_TYPE_96;
  else
    type = (ch == 94) ? CHARSET_TYPE_94X94 : CHARSET_TYPE_96X96;

  if (di == -1)
    {
      obj = charset_by_attributes (type, fi, CHARSET_LEFT_TO_RIGHT);
      if (NILP (obj))
	obj = charset_by_attributes (type, fi, CHARSET_RIGHT_TO_LEFT);
    }
  else
    obj = charset_by_attributes (type, fi, di);

  if (CHARSETP (obj))
    return XCHARSET_NAME (obj);
  return obj;
}

DEFUN ("charset-encodable-p", Fcharset_encodable_p, 1, 1, 0, /*
Return non-nil if CHARSET is (potentially) encodable in a string or buffer.
Under Unicode-internal, this is always true.  Under old-Mule, it depends on
the size of the charset (size-94, 94x94, 96 or 96x96 charsets with the
right offsets -- generally, ISO-2022-compatible charsets -- are encodable).
*/
       (charset))
{
  charset = Fget_charset (charset);
#ifdef UNICODE_INTERNAL
  return Qt;
#else
  return old_mule_charset_encodable (charset) ? Qt : Qnil;
#endif /* (not) UNICODE_INTERNAL */
}

DEFUN ("charset-short-name", Fcharset_short_name, 1, 1, 0, /*
Return short name of CHARSET.
*/
       (charset))
{
  return XCHARSET_SHORT_NAME (Fget_charset (charset));
}

DEFUN ("charset-long-name", Fcharset_long_name, 1, 1, 0, /*
Return long name of CHARSET.
*/
       (charset))
{
  return XCHARSET_LONG_NAME (Fget_charset (charset));
}

DEFUN ("charset-description", Fcharset_description, 1, 1, 0, /*
Return description of CHARSET.
*/
       (charset))
{
  return XCHARSET_DOC_STRING (Fget_charset (charset));
}

DEFUN ("charset-unicode-map", Fcharset_unicode_map, 1, 1, 0, /*
Return unicode map of CHARSET.
*/
       (charset))
{
  return XCHARSET_UNICODE_MAP (Fget_charset (charset));
}

DEFUN ("charset-dimension", Fcharset_dimension, 1, 1, 0, /*
Return dimension of CHARSET.
*/
       (charset))
{
  return make_int (XCHARSET_DIMENSION (Fget_charset (charset)));
}

DEFUN ("charset-property", Fcharset_property, 2, 2, 0, /*
Return property PROP of CHARSET, a charset object or symbol naming a charset.
Recognized properties are those listed in `make-charset', as well as
`name' and `doc-string'.
*/
       (charset, prop))
{
  Lisp_Charset *cs;

  charset = Fget_charset (charset);
  cs = XCHARSET (charset);

  CHECK_SYMBOL (prop);
  if (EQ (prop, Qname))        return CHARSET_NAME (cs);
  if (EQ (prop, Qshort_name))  return CHARSET_SHORT_NAME (cs);
  if (EQ (prop, Qlong_name))   return CHARSET_LONG_NAME (cs);
  if (EQ (prop, Qunicode_map)) return CHARSET_UNICODE_MAP (cs);
  if (EQ (prop, Qdoc_string))  return CHARSET_DOC_STRING (cs);
  if (EQ (prop, Qdimension))   return make_int (CHARSET_DIMENSION (cs));
  if (EQ (prop, Qcolumns))     return make_int (CHARSET_COLUMNS (cs));
  if (EQ (prop, Qgraphic))     return make_int (CHARSET_GRAPHIC (cs));
  if (EQ (prop, Qfinal))       return make_char (CHARSET_FINAL (cs));
  if (EQ (prop, Qchars))
    {
      if (CHARSET_DIMENSION (cs) == 1)
	return make_int (CHARSET_CHARS (cs, 1));
      else
	return list2 (make_int (CHARSET_CHARS (cs, 0)),
		      make_int (CHARSET_CHARS (cs, 1)));
    }
  if (EQ (prop, Qoffset))
    {
      if (CHARSET_DIMENSION (cs) == 1)
	return make_int (CHARSET_OFFSET (cs, 1));
      else
	return list2 (make_int (CHARSET_OFFSET (cs, 0)),
		      make_int (CHARSET_OFFSET (cs, 1)));
    }
  if (EQ (prop, Qregistries))    return CHARSET_REGISTRIES (cs);
#ifdef HAVE_CCL
  if (EQ (prop, Qccl_program)) return CHARSET_CCL_PROGRAM (cs);
#endif /* HAVE_CCL */
  if (EQ (prop, Qdirection))
    return CHARSET_DIRECTION (cs) == CHARSET_LEFT_TO_RIGHT ? Ql2r : Qr2l;
  if (EQ (prop, Qreverse_direction_charset))
    {
      Lisp_Object obj = CHARSET_REVERSE_DIRECTION_CHARSET (cs);
      /* #### Is this translation OK?  If so, error checking sufficient? */
      return CHARSETP (obj) ? XCHARSET_NAME (obj) : obj;
    }
  invalid_constant ("Unrecognized charset property name", prop);
  RETURN_NOT_REACHED (Qnil);
}

DEFUN ("charset-id", Fcharset_id, 1, 1, 0, /*
Return charset identification number of CHARSET.
When configured with `--with-unicode-internal' (see `make-char'), this is
simply an arbitrary value, retained for compatibility.  With old-Mule, this
is the internal charset ID of the charset, which is significant in how the
internal string and character encodings are constructed.  This function is
normally used only by CCL (which isn't available under Unicode-internal,
anyway).
*/
	(charset))
{
  return make_int (XCHARSET_ID (Fget_charset (charset)));
}

#ifdef HAVE_CCL

/* #### We need to figure out which properties we really want to
   allow to be set. */

DEFUN ("set-charset-ccl-program", Fset_charset_ccl_program, 2, 2, 0, /*
Set the `ccl-program' property of CHARSET to CCL-PROGRAM.
*/
       (charset, ccl_program))
{
  charset = Fget_charset (charset);
  XCHARSET_CCL_PROGRAM (charset) = get_ccl_program (ccl_program);

  face_property_was_changed (Vdefault_face, Qfont, Qglobal);
  return Qnil;
}

#endif /* HAVE_CCL */

void
set_charset_registries (Lisp_Object charset, Lisp_Object registries)
{
  XCHARSET_REGISTRIES (charset) = registries;
  invalidate_charset_font_caches (charset);
  face_property_was_changed (Vdefault_face, Qfont, Qglobal);
}

DEFUN ("set-charset-registries", Fset_charset_registries, 2, 3, 0, /*
Set the `registries' property of CHARSET to REGISTRIES.

REGISTRIES is an ordered vector of strings that describe the X11
CHARSET_REGISTRY and the CHARSET_ENCODINGs appropriate for this charset.
Separate each registry from the corresponding encoding with a dash.  The
strings are not regular expressions, in contrast to the old behavior of
the `charset-registry' property.

One reason to call this function might be if you're in Japan and you'd
prefer the backslash to display as a Yen sign; the corresponding syntax
would be:

(set-charset-registries 'ascii ["jisx0201.1976-0"])

If optional argument FORCE is non-nil, avoid sanity-checking the elements of
REGISTRIES. Normally the strings are checked to make sure they contain no
XLFD wild cards and that they contain at least one hyphen; the only context
in which one might want not to do this is in order to use a font which
doesn't have a full XLFD--and thus, an effective
CHARSET_REGISTRY-CHARSET_ENCODING of ""--to display ASCII.

We recommend strongly that you specify a full XLFD, since this makes
multilingual and variant font handling work much better. To get the full
XLFD of any font, start xfd with the short name as the pattern argument:

    xfd -fn 8x16kana

and use the text that appears at the top of the window. 
*/
       (charset, registries, force))
{
  int i; 
  charset = Fget_charset (charset);
  CHECK_VECTOR (registries);

  for (i = 0; i < XVECTOR_LENGTH (registries); ++i)
    {
      CHECK_STRING (XVECTOR_DATA (registries)[i]);

      if (!NILP (force))
        {
          continue;
        }

      if (NULL == qxestrchr (XSTRING_DATA (XVECTOR_DATA (registries)[i]), '-'))
	{
	  invalid_argument ("Not an X11 REGISTRY-ENCODING combination", 
			    XVECTOR_DATA (registries)[i]);
	}

      if (qxestrchr (XSTRING_DATA (XVECTOR_DATA (registries)[i]), '*') ||
	  qxestrchr (XSTRING_DATA (XVECTOR_DATA (registries)[i]), '?'))
	{
	  invalid_argument
	    ("XLFD wildcards not allowed in charset-registries", 
	     XVECTOR_DATA (registries)[i]);

	}
    }

  set_charset_registries (charset, registries);

  return Qnil;
}

DEFUN ("charsets-in-region", Fcharsets_in_region, 2, 4, 0, /*
Return a list of the charsets in the region between START and END.
BUFFER defaults to the current buffer if omitted.
Charsets will be determined for characters in the buffer according to
PRECEDENCE-LIST, a charset precedence list (see `make-char').  If
PRECEDENCE-LIST is not given, the buffer-local value of the default
Unicode precedence list will be used.
*/
       (start, end, buffer, precedence_list))
{
  struct buffer *buf = decode_buffer (buffer, 1);
  Charbpos pos, stop;	/* Limits of the region. */
  Lisp_Object_dynarr *charset_dyn;
  Lisp_Object_dynarr *precdyn;
  Lisp_Object charset_list = Qnil;
  int i;

  /* Get and coerce the buffer region */
  get_buffer_range_char (buf, start, end, &pos, &stop, 0);

  charset_dyn = Dynarr_new (Lisp_Object);

  /* Get the proper Unicode precedence list */
  if (NILP (precedence_list))
    precdyn = get_buffer_unicode_precedence (buf);
  else
    precdyn = convert_charset_list_to_precedence_dynarr (precedence_list);

  /* Find the actual charsets */
  find_charsets_in_buffer (charset_dyn, buf, pos, stop - pos, precdyn);

  /* Free it if and only if we created it */
  if (!NILP (precedence_list))
    free_precedence_dynarr (precdyn);

  /* Convert dynarr to list */
  for (i = 0; i < Dynarr_length (charset_dyn); i++)
    {
      Lisp_Object charset = Dynarr_at (charset_dyn, i);
      charset_list = Fcons (charset, charset_list);
    }

  Dynarr_free (charset_dyn);
  return Fnreverse (charset_list);
}


/************************************************************************/
/*                            memory usage                              */
/************************************************************************/

#ifdef MEMORY_USAGE_STATS

struct charset_stats
{
  int from_unicode;
  int to_unicode;
  int other;
};

static void
compute_charset_usage (Lisp_Object charset, struct charset_stats *stats,
		      struct overhead_stats *ovstats)
{
  struct Lisp_Charset *c = XCHARSET (charset);
  xzero (*stats);
  stats->other   += LISPOBJ_STORAGE_SIZE (c, sizeof (*c), ovstats);
  stats->from_unicode += compute_from_unicode_table_size (charset, ovstats);
  stats->to_unicode += compute_to_unicode_table_size (charset, ovstats);
}

DEFUN ("charset-memory-usage", Fcharset_memory_usage, 1, 1, 0, /*
Return stats about the memory usage of charset CHARSET.
The values returned are in the form of an alist of usage types and
byte counts.  The byte counts attempt to encompass all the memory used
by the charset (separate from the memory logically associated with a
charset or frame), including internal structures and any malloc()
overhead associated with them.  In practice, the byte counts are
underestimated for various reasons, e.g. because certain memory usage
is very hard to determine \(e.g. the amount of memory used inside the
Xt library or inside the X server).

Multiple slices of the total memory usage may be returned, separated
by a nil.  Each slice represents a particular view of the memory, a
particular way of partitioning it into groups.  Within a slice, there
is no overlap between the groups of memory, and each slice collectively
represents all the memory concerned.
*/
       (charset))
{
  struct charset_stats stats;
  struct overhead_stats ovstats;
  Lisp_Object val = Qnil;

  charset = Fget_charset (charset);
  xzero (ovstats);
  compute_charset_usage (charset, &stats, &ovstats);

  val = acons (Qfrom_unicode,       make_int (stats.from_unicode),      val);
  val = acons (Qto_unicode,         make_int (stats.to_unicode),        val);
  val = Fcons (Qnil, val);
  val = acons (Qactually_requested, make_int (ovstats.was_requested),   val);
  val = acons (Qmalloc_overhead,    make_int (ovstats.malloc_overhead), val);
  val = acons (Qgap_overhead,       make_int (ovstats.gap_overhead),    val);
  val = acons (Qdynarr_overhead,    make_int (ovstats.dynarr_overhead), val);

  return Fnreverse (val);
}

#endif /* MEMORY_USAGE_STATS */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_mule_charset (void)
{
  INIT_LRECORD_IMPLEMENTATION (charset);

  DEFSUBR (Fcharsetp);
  DEFSUBR (Ffind_charset);
  DEFSUBR (Fget_charset);
  DEFSUBR (Fcharset_list);
  DEFSUBR (Fcharset_name);
  DEFSUBR (Fmake_charset);
  DEFSUBR (Fmake_reverse_direction_charset);
  DEFSUBR (Fcharset_reverse_direction_charset);
  DEFSUBR (Fcharset_from_attributes);
  DEFSUBR (Fcharset_encodable_p);
  DEFSUBR (Fcharset_short_name);
  DEFSUBR (Fcharset_long_name);
  DEFSUBR (Fcharset_unicode_map);
  DEFSUBR (Fcharset_description);
  DEFSUBR (Fcharset_dimension);
  DEFSUBR (Fcharset_property);
  DEFSUBR (Fcharset_id);
#ifdef HAVE_CCL
  DEFSUBR (Fset_charset_ccl_program);
#endif /* HAVE_CCL */
  DEFSUBR (Fset_charset_registries);
  DEFSUBR (Fcharsets_in_region);

#ifdef MEMORY_USAGE_STATS
  DEFSUBR (Fcharset_memory_usage);
#endif

  DEFSYMBOL (Qcharsetp);
  DEFSYMBOL (Qregistries);
  DEFSYMBOL (Qfinal);
  DEFSYMBOL (Qgraphic);
  DEFSYMBOL (Qregistry);
  DEFSYMBOL (Qdirection);
  DEFSYMBOL (Qreverse_direction_charset);
  DEFSYMBOL (Qshort_name);
  DEFSYMBOL (Qlong_name);
  DEFSYMBOL (Qunicode_map);

  DEFSYMBOL (Qto_unicode);

  DEFSYMBOL (Ql2r);
  DEFSYMBOL (Qr2l);

  /* Charsets, compatible with FSF 20.3
     Naming convention is Script-Charset[-Edition] */
  DEFSYMBOL (Qlatin_iso8859_1);
  DEFSYMBOL (Qlatin_iso8859_2);
  DEFSYMBOL (Qlatin_iso8859_3);
  DEFSYMBOL (Qlatin_iso8859_4);
  DEFSYMBOL (Qthai_tis620);
  DEFSYMBOL (Qarabic_iso8859_6);
  DEFSYMBOL (Qgreek_iso8859_7);
  DEFSYMBOL (Qhebrew_iso8859_8);
  DEFSYMBOL (Qkatakana_jisx0201);
  DEFSYMBOL (Qlatin_jisx0201);
  DEFSYMBOL (Qcyrillic_iso8859_5);
  DEFSYMBOL (Qlatin_iso8859_9);
  DEFSYMBOL (Qlatin_iso8859_15);
  DEFSYMBOL (Qchinese_sisheng);
  DEFSYMBOL (Qjapanese_jisx0208_1978);
  DEFSYMBOL (Qchinese_gb2312);
  DEFSYMBOL (Qjapanese_jisx0208);
  DEFSYMBOL (Qkorean_ksc5601);
  DEFSYMBOL (Qjapanese_jisx0212);
  DEFSYMBOL (Qchinese_cns11643_1);
  DEFSYMBOL (Qchinese_cns11643_2);
#ifdef UNICODE_INTERNAL
  DEFSYMBOL (Qchinese_big5);
  DEFSYMBOL (Qjapanese_shift_jis);
#else /* not UNICODE_INTERNAL */
  DEFSYMBOL (Qchinese_big5_1);
  DEFSYMBOL (Qchinese_big5_2);
#endif /* UNICODE_INTERNAL */

  DEFSYMBOL (Qcomposite);

#ifndef UNICODE_INTERNAL
#define FROB(low, high)				\
  DEFSYMBOL (Qunicode_##low##_##high)

FROB  (   0,  23ff);
FROB  (2400,  47ff);
FROB  (4800,  6bff);
FROB  (6c00,  8fff);
FROB  (9000,  b3ff);
FROB  (b400,  d7ff);
FROB  (e000, 103ff);
FROB (10400, 127ff);
FROB (12800, 14bff);
FROB (14c00, 16fff);
FROB (17000, 193ff);
FROB (19400, 1b7ff);
FROB (1b800, 1dbff);
FROB (1dc00, 1ffff);
FROB (20000, 223ff);
FROB (22400, 247ff);
FROB (24800, 26bff);
FROB (26c00, 28fff);
FROB (29000, 2b3ff);
FROB (2b400, 2d7ff);
FROB (2d800, 2fbff);
FROB (2fc00, 31fff);
/* WARNING: Any changes to this list need to be propagated to at least two
   other places in this file (at the top and
   complex_vars_of_mule_charset(); maybe also to the CHARSET_ID enum near
   the top. */

#undef FROB
#endif
}

void
vars_of_mule_charset (void)
{
  int i, j, k;

  chlook = xnew_and_zero (struct charset_lookup); /* zero for Purify. */
  dump_add_root_block_ptr (&chlook, &charset_lookup_description);

  /* Table of charsets indexed by type/final-byte/direction. */
  for (i = 0; i < countof (chlook->charset_by_attributes); i++)
    for (j = 0; j < countof (chlook->charset_by_attributes[0]); j++)
      for (k = 0; k < countof (chlook->charset_by_attributes[0][0]); k++)
	chlook->charset_by_attributes[i][j][k] = Qnil;

#ifndef UNICODE_INTERNAL
  /* Table of encodable charsets indexed by charset ID. */
  for (i = 0; i < countof (chlook->charset_by_encodable_id); i++)
    chlook->charset_by_encodable_id[i] = Qnil;

  chlook->next_allocated_private_dim1_id = MIN_PRIVATE_DIM1_CHARSET_ID;
  chlook->next_allocated_private_dim2_id = MIN_PRIVATE_DIM2_CHARSET_ID;
#endif /* not UNICODE_INTERNAL */

  staticpro (&Vcharset_hash_table);
  Vcharset_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

  staticpro (&Vcharset_id_table);
  Vcharset_id_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
}

void
complex_vars_of_mule_charset (void)
{
  /* Predefined character sets.  We store them into variables for
     ease of access. */

#ifdef UNICODE_INTERNAL
#define MAKE_CSID(foo) -1
#else
#define MAKE_CSID(foo) CHARSET_ID_##foo
#endif

  /* @@####

  NOTE NOTE NOTE:

  Currently we still load the unicode tables using
  `load-unicode-tables' in unicode.el rather than setting the
  appropriate values below.  Perhaps we should change this, but
  perhaps we should move the creation of all these charsets into lisp. */

  staticpro (&Vcharset_ascii);
  Vcharset_ascii =
    make_charset (MAKE_CSID (ASCII), 0, Qascii, 1,
		  1, 128, 0, 0, 1, 0, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("ASCII"),
		  build_msg_string ("ASCII"),
		  build_msg_string ("ASCII (left half of ISO 8859-1)"),
		  vector1 (build_string ("iso8859-1")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_control_1);
  Vcharset_control_1 =
    make_charset (MAKE_CSID (CONTROL_1), 0, Qcontrol_1, 1,
		  1, 32, 0, 128, 1, 1, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("C1"),
		  build_msg_string ("Control characters"),
		  build_msg_string ("Control characters 128-159"),
		  vector1 (build_string ("iso8859-1")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_1);
  Vcharset_latin_iso8859_1 =
    make_charset (MAKE_CSID (LATIN_ISO8859_1), 0, Qlatin_iso8859_1, 1,
		  1, 96, 0, 160, 1, 1, 'A',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-1"),
		  build_msg_string ("RHP of Latin-1 (ISO 8859-1): ISO-IR-100"),
		  build_msg_string ("Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100"),
		  vector1 (build_string ("iso8859-1")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_2);
  Vcharset_latin_iso8859_2 =
    make_charset (MAKE_CSID (LATIN_ISO8859_2), 0, Qlatin_iso8859_2, 1,
		  1, 96, 0, 160, 1, 1, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-2"),
		  build_msg_string ("RHP of Latin-2 (ISO 8859-2): ISO-IR-101"),
		  build_msg_string ("Right-Hand Part of Latin Alphabet 2 (ISO/IEC 8859-2): ISO-IR-101"),
		  vector1 (build_string ("iso8859-2")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_3);
  Vcharset_latin_iso8859_3 =
    make_charset (MAKE_CSID (LATIN_ISO8859_3), 0, Qlatin_iso8859_3, 1,
		  1, 96, 0, 160, 1, 1, 'C',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-3"),
		  build_msg_string ("RHP of Latin-3 (ISO 8859-3): ISO-IR-109"),
		  build_msg_string ("Right-Hand Part of Latin Alphabet 3 (ISO/IEC 8859-3): ISO-IR-109"),
		  vector1 (build_string ("iso8859-3")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_4);
  Vcharset_latin_iso8859_4 =
    make_charset (MAKE_CSID (LATIN_ISO8859_4), 0, Qlatin_iso8859_4, 1,
		  1, 96, 0, 160, 1, 1, 'D',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-4"),
		  build_msg_string ("RHP of Latin-4 (ISO 8859-4): ISO-IR-110"),
		  build_msg_string ("Right-Hand Part of Latin Alphabet 4 (ISO/IEC 8859-4): ISO-IR-110"),
		  vector1 (build_string ("iso8859-4")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_thai_tis620);
  Vcharset_thai_tis620 =
    make_charset (MAKE_CSID (THAI_TIS620), 0, Qthai_tis620, 1,
		  1, 96, 0, 160, 1, 1, 'T',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Thai (TIS620)"),
		  build_msg_string ("RHP of Thai (TIS620): ISO-IR-166"),
		  build_msg_string ("Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166"),
		  vector1 (build_string ("tis620.2529-1")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_arabic_iso8859_6);
  Vcharset_arabic_iso8859_6 =
    /* This one is in the private charset ID space. */
    make_charset (-1, 0, Qarabic_iso8859_6, 1,
		  1, 96, 0, 160, 1, 1, 'G',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Arabic (ISO8859-6)"),
		  build_msg_string ("RHP of Arabic (ISO 8859-6): ISO-IR-127"),
		  build_msg_string ("Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"),
		  vector1 (build_string ("iso8859-6")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_greek_iso8859_7);
  Vcharset_greek_iso8859_7 =
    make_charset (MAKE_CSID (GREEK_ISO8859_7), 0, Qgreek_iso8859_7, 1,
		  1, 96, 0, 160, 1, 1, 'F',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Greek (ISO8859-7)"),
		  build_msg_string ("RHP of Greek (ISO 8859-7): ISO-IR-126"),
		  build_msg_string ("Right-Hand Part of Latin/Greek Alphabet (ISO/IEC 8859-7): ISO-IR-126"),
		  vector1 (build_string ("iso8859-7")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_hebrew_iso8859_8);
  Vcharset_hebrew_iso8859_8 =
    make_charset (MAKE_CSID (HEBREW_ISO8859_8), 0, Qhebrew_iso8859_8, 1,
		  1, 96, 0, 160, 1, 1, 'H',
		  CHARSET_RIGHT_TO_LEFT,
		  build_string ("Hebrew (ISO8859-8)"),
		  build_msg_string ("RHP of Hebrew (ISO 8859-8): ISO-IR-138"),
		  build_msg_string ("Right-Hand Part of Latin/Hebrew Alphabet (ISO/IEC 8859-8): ISO-IR-138"),
		  vector1 (build_string ("iso8859-8")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_katakana_jisx0201);
  Vcharset_katakana_jisx0201 =
    make_charset (MAKE_CSID (KATAKANA_JISX0201), 0, Qkatakana_jisx0201, 1,
		  1, 94, 0, 161, 1, 1, 'I',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (JISX0201 Kana)"),
		  build_msg_string ("Japanese Katakana (JISX0201.1976)"),
		  build_msg_string ("Katakana Part of JISX0201.1976"),
		  vector1 (build_string ("jisx0201.1976-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_jisx0201);
  Vcharset_latin_jisx0201 =
    make_charset (MAKE_CSID (LATIN_JISX0201), 0, Qlatin_jisx0201, 1,
		  1, 94, 0, 33, 1, 0, 'J',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (JISX0201 Roman)"),
		  build_msg_string ("Japanese Roman (JISX0201.1976)"),
		  build_msg_string ("Roman Part of JISX0201.1976"),
		  vector1 (build_string ("jisx0201.1976-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_cyrillic_iso8859_5);
  Vcharset_cyrillic_iso8859_5 =
    make_charset (MAKE_CSID (CYRILLIC_ISO8859_5), 0, Qcyrillic_iso8859_5, 1,
		  1, 96, 0, 160, 1, 1, 'L',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Cyrillic (ISO8859-5)"),
		  build_msg_string ("RHP of Cyrillic (ISO 8859-5): ISO-IR-144"),
		  build_msg_string ("Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144"),
		  vector1 (build_string ("iso8859-5")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_9);
  Vcharset_latin_iso8859_9 =
    make_charset (MAKE_CSID (LATIN_ISO8859_9), 0, Qlatin_iso8859_9, 1,
		  1, 96, 0, 160, 1, 1, 'M',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-5"),
		  build_msg_string ("RHP of Latin-5 (ISO 8859-9): ISO-IR-148"),
		  build_msg_string ("Right-Hand Part of Latin Alphabet 5 (ISO/IEC 8859-9): ISO-IR-148"),
		  vector1 (build_string ("iso8859-9")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_15);
  Vcharset_latin_iso8859_15 =
    make_charset (MAKE_CSID (LATIN_ISO8859_15), 0, Qlatin_iso8859_15, 1,
		  1, 96, 0, 160, 1, 1, 'b',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-9"),
		  build_msg_string ("RHP of Latin-9 (Euro Sign) (ISO 8859-15): ISO-IR-203"),
		  build_msg_string ("European Supplementary Latin Set (\"Latin 9\") (Euro Sign) (ISO/IEC 8859-15): ISO-IR-203\n"
"FIELD OF UTILIZATION: \"Communication and processing of text in European\n"
"languages. The set provides for the languages enumerated in ISO/IEC\n"
"8859-1. In addition, it contains the EURO SIGN and provides support for the\n"
"French, and Finnish languages in addition.\""),
		  vector1 (build_string ("iso8859-15")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_japanese_jisx0208_1978);
  Vcharset_japanese_jisx0208_1978 =
    make_charset (MAKE_CSID (JAPANESE_JISX0208_1978), 0,
		  Qjapanese_jisx0208_1978, 2,
		  94, 94, 33, 33, 2, 0, '@',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (JISX0208.1978)"),
		  build_msg_string ("Japanese (JISX0208.1978): ISO-IR-42"),
		  build_msg_string
		  ("JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"),
		  vector2 (build_string ("jisx0208.1978-0"),
			   build_string ("jisc6226.1978-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_chinese_gb2312);
  Vcharset_chinese_gb2312 =
    make_charset (MAKE_CSID (CHINESE_GB2312), 0, Qchinese_gb2312, 2,
		  94, 94, 33, 33, 2, 0, 'A',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese simplified (GB2312)"),
		  build_msg_string ("Chinese simplified (GB2312): ISO-IR-58"),
		  build_msg_string ("GB2312 Chinese simplified: ISO-IR-58"),
		  vector2 (build_string ("gb2312.1980-0"), 
			   build_string ("gb2312.80&gb8565.88-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_japanese_jisx0208);
  Vcharset_japanese_jisx0208 =
    make_charset (MAKE_CSID (JAPANESE_JISX0208), 0, Qjapanese_jisx0208, 2,
		  94, 94, 33, 33, 2, 0, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (JISX0208)"),
		  build_msg_string ("JISX0208.1983/1990 (Japanese): ISO-IR-87"),
		  build_msg_string ("JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"),
		  vector2 (build_string ("jisx0208.1983-0"),
			   build_string ("jisx0208.1990-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_korean_ksc5601);
  Vcharset_korean_ksc5601 =
    make_charset (MAKE_CSID (KOREAN_KSC5601), 0, Qkorean_ksc5601, 2,
		  94, 94, 33, 33, 2, 0, 'C',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Korean (KSC5601)"),
		  build_msg_string ("Korean (KSC5601): ISO-IR-149"),
		  build_msg_string ("KSC5601 Korean Hangul and Hanja: ISO-IR-149"),
		  vector1 (build_string ("ksc5601.1987-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_japanese_jisx0212);
  Vcharset_japanese_jisx0212 =
    make_charset (MAKE_CSID (JAPANESE_JISX0212), 0, Qjapanese_jisx0212, 2,
		  94, 94, 33, 33, 2, 0, 'D',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (JISX0212)"),
		  build_msg_string ("JISX0212 (Japanese): ISO-IR-159"),
		  build_msg_string ("JISX0212 Japanese supplement: ISO-IR-159"),
		  vector1 (build_string ("jisx0212.1990-0")), Qnil, 0,
		  CSET_INTERNAL);

#define CHINESE_CNS_PLANE(n) "cns11643.1992-" n
  staticpro (&Vcharset_chinese_cns11643_1);
  Vcharset_chinese_cns11643_1 =
    make_charset (MAKE_CSID (CHINESE_CNS11643_1), 0, Qchinese_cns11643_1, 2,
		  94, 94, 33, 33, 2, 0, 'G',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese traditional (CNS11643-1)"),
		  build_msg_string ("Chinese traditional (CNS11643-1): ISO-IR-171"),
		  build_msg_string
		  ("CNS11643 Plane 1 Chinese traditional: ISO-IR-171"),
		  vector1 (build_string (CHINESE_CNS_PLANE("1"))), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_chinese_cns11643_2);
  Vcharset_chinese_cns11643_2 =
    make_charset (MAKE_CSID (CHINESE_CNS11643_2), 0, Qchinese_cns11643_2, 2,
		  94, 94, 33, 33, 2, 0, 'H',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese traditional (CNS11643-2)"),
		  build_msg_string ("Chinese traditional (CNS11643-2): ISO-IR-172"),
		  build_msg_string
		  ("CNS11643 Plane 2 Chinese traditional: ISO-IR-172"),
		  vector1 (build_string (CHINESE_CNS_PLANE("2"))), Qnil, 0,
		  CSET_INTERNAL);
#ifdef UNICODE_INTERNAL
  /* We can support Big5 directly. */
  staticpro (&Vcharset_chinese_big5);
  Vcharset_chinese_big5 =
    /* Big5 claims to be a 94x157 charset, but with gaps in the middle.
       In particular, the rows are (theoretically) indexed from A1 - FE
       and the columns from 40 - 7E and A1 - FE.  In fact, there are gaps
       in the rows as well (rows C7 and C8 are missing, as well as rows
       FA - FE), but that appears to be due to accident -- i.e. they just
       ran out of chars and/or wanted to make room for expansion.  Note
       also that the gap at C7 and C8 is due to the Level-1/Level-2
       division of Big5 (see below).  The 94 rows are those between
       A1 and FE, inclusive.  The 157 columns count the sum of the columns
       in each disjoint set.  For us, we need to use the size of the range
       [40, FE], which is 191. */
    make_charset (-1, 0, Qchinese_big5, 2,
		  94, 191, 161, 64, 2, 0, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese traditional (Big5)"),
		  build_msg_string ("Chinese traditional (Big5)"),
		  build_msg_string
		  ("Big5 (Chinese traditional)"),
		  vector1 (build_string ("big5.eten-0")), Qnil, 0,
		  CSET_INTERNAL);
#else /* not UNICODE_INTERNAL */
  /* Old Mule situation; we can only handle up to 96x96 charsets.
     So we split it into two charsets.  According to Ken Lunde's CJKV
     book, Big5 itself is split into "Big Five Level 1" (rows A1-C6)
     and "Big Five Level 2" (rows C9-F9), with the latter containing
     less used characters.  We split the same way then coerce the
     result into a 94x94 block. */
  staticpro (&Vcharset_chinese_big5_1);
  Vcharset_chinese_big5_1 =
    make_charset (MAKE_CSID (CHINESE_BIG5_1), 0, Qchinese_big5_1, 2,
		  94, 94, 33, 33, 2, 0, '0',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese traditional (Big5), L1"),
		  build_msg_string ("Chinese traditional (Big5) (Level-1) A141-C67F"),
		  build_msg_string
		  ("Frequently used part (A141-C67F) of Big5 (Chinese traditional)"),
		  vector1 (build_string ("big5.eten-0")), Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_chinese_big5_2);
  Vcharset_chinese_big5_2 =
    make_charset (MAKE_CSID (CHINESE_BIG5_2), 0, Qchinese_big5_2, 2,
		  94, 94, 33, 33, 2, 0, '1',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Chinese traditional (Big5), L2"),
		  build_msg_string ("Chinese traditional (Big5) (Level-2) C940-FEFE"),
		  build_msg_string ("Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"),
		  vector1 (build_string ("big5.eten-0")), Qnil, 0,
		  CSET_INTERNAL);
#endif /* UNICODE_INTERNAL */
#ifdef UNICODE_INTERNAL
  /* We can support Shift-JIS directly.*/
  staticpro (&Vcharset_japanese_shift_jis);
  Vcharset_japanese_shift_jis =
    /* See comments in mule-coding.c.
       First byte is in the range [80-9F], [E0-EF]; second byte is in the
       range [40-7E], [80-FC] */
    make_charset (MAKE_CSID (SHIFT_JIS), 0, Qjapanese_shift_jis, 2,
		  112, 189, 128, 64, 2, 0, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Japanese (Shift-JIS)"),
		  build_msg_string ("Japanese (Shift-JIS)"),
		  build_msg_string
		  ("Shift-JIS Japanese encoding of JIS X 0208:1997"),
		  /* @@#### FIXME This is the X registry; is it right? */
		  vector1 (build_string ("sjis")), Qnil, 0,
		  CSET_INTERNAL);
#endif /* UNICODE_INTERNAL */

#ifdef ENABLE_COMPOSITE_CHARS
  /* #### For simplicity, we put composite chars into a 96x96 charset.
     This is going to lead to problems because you can run out of
     room, esp. as we don't yet recycle numbers. */
  /* #### Figure out what should go into the Unicode translation table,
     if we even support this crap at all */
  staticpro (&Vcharset_composite);
  Vcharset_composite =
    make_charset (MAKE_CSID (COMPOSITE), 0, Qcomposite, 2,
		  96, 96, 32, 32, 2, 0, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Composite"),
		  build_msg_string ("Composite characters"),
		  build_msg_string ("Composite characters"),
		  vector1 (build_string ("")), Qnil, 0, CSET_INTERNAL);
#else
  /* We create a hack so that we have a way of storing ESC 0 and ESC 1
     sequences as "characters", so that they will be output correctly. */
  staticpro (&Vcharset_composite);
  Vcharset_composite =
    make_charset (MAKE_CSID (COMPOSITE), 0, Qcomposite, 1,
		  1, 96, 0, 32, 1, 0, '|',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Composite hack"),
		  build_msg_string ("Composite characters hack"),
		  build_msg_string ("Composite characters hack"),
		  vector1 (build_string ("")), Qnil, 0, CSET_INTERNAL);
#endif /* ENABLE_COMPOSITE_CHARS */

  /* This one is in the private charset ID space. */
  staticpro (&Vcharset_chinese_sisheng);
  Vcharset_chinese_sisheng =
    make_charset (-1, 0, Qchinese_sisheng, 1,
		  1, 94, 0, 33, 1, 0, '0',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("SiSheng"),
		  build_msg_string ("SiSheng (PinYin/ZhuYin)"),
		  build_msg_string ("SiSheng characters for PinYin/ZhuYin"),
		  vector2 (build_string ("omron_udc_zh-0"),
			   build_string ("sisheng_cwnn-0")), Qnil, 0,
		  CSET_INTERNAL);

#if 0
#ifndef UNICODE_INTERNAL

#define FROB_1(id, lclow, lchigh, uclow, uchigh)			\
  staticpro (&Vcharset_unicode_##lclow##_##lchigh);			\
  Vcharset_unicode_##lclow##_##lchigh =					\
    make_charset (id, 1,						\
		  Qunicode_##lclow##_##lchigh, 2,			\
		  96, 96, 32, 32, 2, 0, 0,				\
		  CHARSET_LEFT_TO_RIGHT,				\
		  build_string ("Unicode " #uclow "-" #uchigh),		\
		  build_msg_string ("Unicode subset (U+" #uclow "..U+" #uchigh), \
		  build_msg_string ("Unicode subset (U+" #uclow "..U+" #uchigh "used for maintaining round-trip\n" \
"compatibility for Unicode characters that have no representation in any\n" \
"other charset."),							\
		  build_string ("ISO10646-1"), Qnil, 0, 0x##lclow)

#define FROB_OFFICIAL(lclow, lchigh, uclow, uchigh) \
  FROB_1 (MAKE_CSID (UNICODE_##uclow##_##uchigh), lclow, lchigh, uclow, uchigh)

#define FROB_PRIVATE(lclow, lchigh, uclow, uchigh) \
  FROB_1 (-1, lclow, lchigh, uclow, uchigh)

  FROB_OFFICIAL (0, 23ff, 0, 23FF);
  FROB_OFFICIAL (2400, 47ff, 2400, 47FF);
  FROB_OFFICIAL (4800, 6bff, 4800, 6BFF);
  FROB_OFFICIAL (6c00, 8fff, 6C00, 8FFF);
  FROB_OFFICIAL (9000, b3ff, 9000, B3FF);
  FROB_OFFICIAL (b400, d7ff, B400, D7FF);
  FROB_OFFICIAL (e000, 103ff, E000, 103FF);
  FROB_PRIVATE (10400, 127ff, 10400, 127FF);
  FROB_PRIVATE (12800, 14bff, 12800, 14BFF);
  FROB_PRIVATE (14c00, 16fff, 14C00, 16FFF);
  FROB_PRIVATE (17000, 193ff, 17000, 193FF);
  FROB_PRIVATE (19400, 1b7ff, 19400, 1B7FF);
  FROB_PRIVATE (1b800, 1dbff, 1B800, 1DBFF);
  FROB_PRIVATE (1dc00, 1ffff, 1DC00, 1FFFF);
  FROB_PRIVATE (20000, 223ff, 20000, 223FF);
  FROB_PRIVATE (22400, 247ff, 22400, 247FF);
  FROB_PRIVATE (24800, 26bff, 24800, 26BFF);
  FROB_PRIVATE (26c00, 28fff, 26C00, 28FFF);
  FROB_PRIVATE (29000, 2b3ff, 29000, 2B3FF);
  FROB_PRIVATE (2b400, 2d7ff, 2B400, 2D7FF);
  FROB_PRIVATE (2d800, 2fbff, 2D800, 2FBFF);
  FROB_PRIVATE (2fc00, 31fff, 2FC00, 31FFF);
/* WARNING: Any changes to this list need to be propagated to at least two
   other places in this file (at the top and syms_of_mule_charset(); maybe
   also to the CHARSET_ID enum near the top. */

#undef FROB_OFFICIAL
#undef FROB_PRIVATE
#undef FROB_1
#endif /* not UNICODE_INTERNAL */
#endif /* 0 */

  initialize_ascii_control_1_latin_1_unicode_translation ();
}

