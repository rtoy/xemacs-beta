/* Functions to handle multilingual characters.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2004, 2005, 2010 Ben Wing.

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
#include "mule-ccl.h"
#include "fontcolor.h"
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

Lisp_Object Qregister_charset_tags;

#ifdef FIXED_UNICODE_CHARSETS

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

/* @@#### If you really do this, you'd have to change
   the definition of MIN_OFFICIAL_DIM2_CHARSET_ID in text.h to be higher
   than any of these. */
enum fixed_unicode_charset_id
{
  CHARSET_ID_UNICODE_0_23FF = 0x8E,  /* Unicode 0-23FF */
  CHARSET_ID_UNICODE_2400_47FF,      /* Unicode 2400-47FF */
  CHARSET_ID_UNICODE_4800_6BFF,      /* Unicode 4800-6BFF */
  CHARSET_ID_UNICODE_6C00_8FFF,      /* Unicode 6C00-8FFF */
  CHARSET_ID_UNICODE_9000_B3FF,      /* Unicode 9000-B3FF */
  CHARSET_ID_UNICODE_B400_D7FF,      /* Unicode B400-D7FF */
  CHARSET_ID_UNICODE_E000_103FF,     /* Unicode E000-103FF */
};

#endif /* FIXED_UNICODE_CHARSETS */

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

/* Under old-Mule, when we run out of encodable ID's, we start using
   non-encodable ID's, which are higher than any encodable ID.  Under
   Unicode-internal, all charsets are assigned ID's from this pool.
   */
static int next_non_encodable_charset_id;

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
  mark_object (cs->tags);
  mark_object (cs->doc_string);
  mark_object (cs->registries);
  mark_object (cs->ccl_program);
  return cs->name;
}

static void
print_charset (Lisp_Object obj, Lisp_Object printcharfun,
	       int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_lisp_object
      (obj, XSTRING_DATA (XSYMBOL (XCHARSET_NAME (obj))->name));

  write_fmt_string_lisp (printcharfun, "#<charset %s %S %S %S", 4,
			 XCHARSET_NAME (obj), XCHARSET_SHORT_NAME (obj),
			 XCHARSET_LONG_NAME (obj), XCHARSET_DOC_STRING (obj));
  if (XCHARSET_DIMENSION (obj) == 1)
    write_fmt_string (printcharfun, " %d [%d-%d]",
		      XCHARSET_CHARS (obj, 1),
		      XCHARSET_MIN_CODE (obj, 1), XCHARSET_MAX_CODE (obj, 1));
  else
    write_fmt_string (printcharfun, " %dx%d [(%d,%d)-(%d,%d)]",
		      XCHARSET_CHARS (obj, 0), XCHARSET_CHARS (obj, 1),
		      XCHARSET_MIN_CODE (obj, 0), XCHARSET_MIN_CODE (obj, 1),
		      XCHARSET_MAX_CODE (obj, 0), XCHARSET_MAX_CODE (obj, 1));
  write_fmt_string (printcharfun, " %s cols=%d g%d ",
		    XCHARSET_DIRECTION (obj) == CHARSET_LEFT_TO_RIGHT ? "l2r" :
		    "r2l",
		    XCHARSET_COLUMNS (obj),
		    XCHARSET_GRAPHIC (obj));
  if (XCHARSET_FINAL (obj))
    write_fmt_string (printcharfun, "final='%c' ", XCHARSET_FINAL (obj));
  write_fmt_string (printcharfun, "reg=");
  print_internal (XCHARSET_REGISTRIES (obj), printcharfun, 0);
  write_fmt_string (printcharfun, " 0x%x>",
		    LISP_OBJECT_UID (obj));
}

static void
disksave_charset (Lisp_Object charset)
{
  /* If the charset Unicode tables were loaded from a file, then throw them
     away and arrange for them to be loaded again */
  if (CONSP (XCHARSET_UNICODE_MAP (charset)) &&
      STRINGP (X1ST (XCHARSET_UNICODE_MAP (charset))) &&
      !XCHARSET_DO_AUTOLOAD (charset))
    {
      free_charset_unicode_tables (charset);
      init_charset_unicode_tables (charset);
      XCHARSET_DO_AUTOLOAD (charset) = 1;
    }
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
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, tags) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, reverse_direction_charset) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, ccl_program) },
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

DEFINE_DUMPABLE_LISP_OBJECT ("charset", charset,
			     mark_charset, print_charset, 0,
			     0, 0, charset_description, Lisp_Charset);

#ifndef UNICODE_INTERNAL

static int
get_unallocated_encodable_charset_id (int dimension)
{
  if (dimension == 1)
    {
      if (chlook->next_allocated_dim1_id == -1)
	return -1;
      else
	{
	  /* First we use up official ID's, then private ID's, then
	     return -1 (non-encodable ID's will be used) */
	  int retval = chlook->next_allocated_dim1_id;
	  if (chlook->next_allocated_dim1_id == MAX_OFFICIAL_DIM1_CHARSET_ID)
	    chlook->next_allocated_dim1_id = MIN_PRIVATE_DIM1_CHARSET_ID;
	  else if (chlook->next_allocated_dim1_id ==
		   MAX_PRIVATE_DIM1_CHARSET_ID)
	    chlook->next_allocated_dim1_id = -1;
	  else
	    chlook->next_allocated_dim1_id++;
	  return retval;
	}
    }
  else
    {
      text_checking_assert (dimension == 2);

      /* Same logic here as above */
      if (chlook->next_allocated_dim2_id == -1)
	return -1;
      else
	{
	  int retval = chlook->next_allocated_dim2_id;
	  if (chlook->next_allocated_dim2_id == MAX_OFFICIAL_DIM2_CHARSET_ID)
	    chlook->next_allocated_dim2_id = MIN_PRIVATE_DIM2_CHARSET_ID;
	  else if (chlook->next_allocated_dim2_id ==
		   MAX_PRIVATE_DIM2_CHARSET_ID)
	    chlook->next_allocated_dim2_id = -1;
	  else
	    chlook->next_allocated_dim2_id++;
	  return retval;
	}
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
get_charset_limits (Lisp_Object charset, int *low0, int *low1,
		    int *high0, int *high1)
{
  *low0 = XCHARSET_MIN_CODE (charset, 0);
  *low1 = XCHARSET_MIN_CODE (charset, 1);
  *high0 = XCHARSET_MAX_CODE (charset, 0);
  *high1 = XCHARSET_MAX_CODE (charset, 1);
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

  if (FIXNUMP (value))
    {
      *dim0 = *dim1 = XFIXNUM (value);
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
      CHECK_FIXNUM (tem);
      *dim0 = *dim1 = XFIXNUM (tem);
      if (*dim0 < minval || *dim0 > maxval)
	{
	  value = tem;
	  goto bzzzzt;
	}
      if (len == 2)
	{
	  tem = X2ND (value);
	  CHECK_FIXNUM (tem);
	  *dim1 = XFIXNUM (tem);
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
  return Fgethash (make_fixnum (id), Vcharset_id_table, Qnil);
}

static void
remove_charset_from_hash_tables (Lisp_Object charset)
{
  Lisp_Object ret;

  ret = Fremhash (XCHARSET_NAME (charset), Vcharset_hash_table);
  assert (!NILP (ret));
  ret = Fremhash (make_fixnum (XCHARSET_ID (charset)), Vcharset_id_table);
  assert (!NILP (ret));
}

static Lisp_Object
maybe_remove_charset_from_hash_tables (Lisp_Object arg)
{
  Lisp_Object charset = XCAR (arg);
  /* t is a signal to actually do it; it will get set to nil if we
     completely successfully. */
  if (EQ (XCDR (arg), Qt))
    remove_charset_from_hash_tables (charset);
  return Qnil;
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
	      Lisp_Object unicode_map, Lisp_Object tags, int overwrite,
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
      id = next_non_encodable_charset_id++;
#else
      if (old_mule_charset_encodable_by_properties (dimension, size0, size1,
						    offset0, offset1))
	id = get_unallocated_encodable_charset_id (dimension);
      if (id < 0)
	id = next_non_encodable_charset_id++;
#endif /* not UNICODE_INTERNAL */
    }

  /* If this is a temporary place-holder charset, assign a temporary name
     based on the id. */
  if (EQ (name, Qunbound))
    {
      Ibyte tempname[80];

      qxesprintf (tempname, "___temporary___%d__", id);
      name = Fmake_symbol (build_istring (tempname)); /* Uninterned. */
    }

  /* Set certain other default values.  SHORT_NAME cannot be computed
     until the actual name is generated (just above, for temporaries). */
  if (NILP (doc_string))
    doc_string = build_ascstring ("");
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
      obj = ALLOC_NORMAL_LISP_OBJECT (charset);

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

      assert (id == XCHARSET_ID (obj));
      remove_charset_from_hash_tables (obj);
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
  XCHARSET_UNICODE_MAP	(obj) = Qnil;
  XCHARSET_TAGS 	(obj) = Qnil;

  /* Sanity checking -- for internal ISO-2022 charsets, make sure that the
     offset range agrees with the graphic range.  We don't do this for
     external charsets because the user might choose to set the values so
     they don't correspond. */
  if (final && algo_low_or_internal_p == CSET_INTERNAL)
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
  XCHARSET_CCL_PROGRAM	(obj) = Qnil;
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

  assert (NILP (Fgethash (make_fixnum (id), Vcharset_id_table, Qnil)));
  Fputhash (make_fixnum (id), obj, Vcharset_id_table);

  /* [[ Some charsets are "faux" and don't have names or really exist at
     all except in the charset ID table. ]]
     #### Explain this comment! --ben */
  assert (!NILP (name)); /* @@#### to figure out what's going on */
  if (!NILP (name))
    {
      assert (NILP (Fgethash (name, Vcharset_hash_table, Qnil)));
      Fputhash (name, obj, Vcharset_hash_table);
    }

  
  {
    /* Qt signals to remove the charset from the hash tables, in case of
       error */
    Lisp_Object cons = Fcons (obj, Qt);
    int depth = record_unwind_protect (maybe_remove_charset_from_hash_tables,
				       cons);
    struct gcpro gcpro1;

    GCPRO1 (cons);

    /* No need to GCPRO other things even though some functions below call
       Lisp, because all objects are pointed to through the charset, and
       the charset is in a hash table. */
    setup_charset_initial_specifier_tags (obj);

    /* We're called at initialization time before there are any buffers or
       other things necessary for calling Lisp, so don't get SNAFU'ed. */
    if (initialized)
      Fset_charset_tags (obj, tags);
    else
      /* Make sure when we're still initializing and can't handle tags,
	 that the tags actually passed in were nil.  Otherwise we're likely
	 to get errors or crashes elsewhere due to not-yet-defined tags,
	 etc. */
      assert (NILP (tags));

    if (!NILP (unicode_map))
      init_charset_unicode_map (obj, unicode_map);

    XCDR (cons) = Qnil; /* Don't remove from hash */
    UNGCPRO;
    unbind_to (depth);
  }

  return obj;
}


/* #### SJT Should generic properties be allowed? */
DEFUN ("make-charset", Fmake_charset, 3, 3, 0, /*
Define a new "charset", i.e. a coded character set.
This function is for use with international support.

"Charsets" are objects describing coded character sets, i.e. arbitrary sets
of characters indexed by one or two dimensions.  Charsets typically contain
the characters necessary to encode text in a particular script or writing
system, and often correspond to well-known coded character sets defined by
national standards and intended to be sufficient to encode text in a
particular nation's language.  Hence, they are often described as "national
character sets".  However, it's possible for them to simply consist of
an arbitrary collection of characters grouped together for some reason.

The indices of a character in a charset are integers in the range 0-255 are
are called "octets".  The dimension of a charset (one or two) determines
the number of octets needed to index a particular character.  For more
information on character octets, see `make-char'.

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
                Unicode.  This is either a list

                (FILENAME START END OFFSET FLAGS)

		specifying a filename to load the map from and corresponding
		arguments to `load-unicode-mapping-table', or a list

		((UNICODE-CODEPOINT CHARSET-CODEPOINT)
                 (UNICODE-CODEPOINT CHARSET-CODEPOINT)
                  ...
                )

                directly specifying Unicode codepoints and corresponding
		charset codepoints.  In the former format, any of the
		arguments other than FILENAME can be omitted, as with the
		arguments to `load-unicode-mapping-table'.  In the latter
		format, either one or two charset codepoints need to be
		given, depending on the dimension of the charset.
`ccl-program'	A compiled CCL program used to convert a character in
		this charset into an index into the font.  The CCL program
		is passed the octets of the character, which will be within
                the limits established by `offset' and `chars'.
`tags'		A list of symbols defining logical categories which the
		charset belongs to.  A charset tag can be used in place of
		an actual charset in charset precedence lists used in
                conjunction with conversion from Unicode to charset codepoints
                (see `unicode-to-char').
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
  Lisp_Object tags = Qnil;
  Lisp_Object charset = Qnil;
  Lisp_Object ccl_program = Qnil;
  Lisp_Object short_name = Qnil, long_name = Qnil;
  Lisp_Object existing_charset = Qnil;
  int temporary = UNBOUNDP (name);
  struct gcpro gcpro1;

  GCPRO1 (registries);

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
	    CHECK_FIXNUM (value);
	    dimension = XFIXNUM (value);
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
	    CHECK_FIXNUM (value);
	    columns = XFIXNUM (value);
	    if (columns != 1 && columns != 2)
	      invalid_constant ("Invalid value for `columns'", value);
	  }

	else if (EQ (keyword, Qgraphic))
	  {
	    CHECK_FIXNUM (value);
	    graphic = XFIXNUM (value);
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
	
	else if (EQ (keyword, Qtags))
	  {
	    CHECK_TRUE_LIST (value);
	    /* It will get validated further */
	    tags = value;
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

	else if (EQ (keyword, Qccl_program))
	  {
            /* This errors if VALUE is not a valid CCL program. */
	    ccl_program = get_ccl_program (value);
	  }
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
			  doc_string, registries, unicode_map, tags,
			  !NILP (existing_charset),
			  CSET_EXTERNAL);
  
  XCHARSET (charset)->temporary = temporary;
  if (!NILP (ccl_program))
    XCHARSET_CCL_PROGRAM (charset) = ccl_program;

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

  UNGCPRO;
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
  Lisp_Object tags;
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
  tags = CHARSET_TAGS (cs);

  new_charset = make_charset (-1, 0, new_name, dimension,
			      CHARSET_CHARS (cs, 0), CHARSET_CHARS (cs, 1),
			      CHARSET_OFFSET (cs, 0), CHARSET_OFFSET (cs, 1),
			      columns, graphic, final, direction, short_name,
			      long_name, doc_string, registries,
			      unicode_map, tags, 0, CSET_EXTERNAL);

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

  CHECK_FIXNUM (dimension);
  dm = XFIXNUM (dimension);
  if (dm < 1 || dm > 2)
    invalid_constant ("Invalid value for DIMENSION", dimension);

  CHECK_FIXNUM (chars);
  ch = XFIXNUM (chars);
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
Return short name of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return XCHARSET_SHORT_NAME (Fget_charset (charset));
}

DEFUN ("charset-long-name", Fcharset_long_name, 1, 1, 0, /*
Return long name of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return XCHARSET_LONG_NAME (Fget_charset (charset));
}

DEFUN ("charset-description", Fcharset_description, 1, 1, 0, /*
Return description of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return XCHARSET_DOC_STRING (Fget_charset (charset));
}

DEFUN ("charset-unicode-map", Fcharset_unicode_map, 1, 1, 0, /*
Return unicode map of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return XCHARSET_UNICODE_MAP (Fget_charset (charset));
}

DEFUN ("charset-tags", Fcharset_tags, 1, 1, 0, /*
Return the tags of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return XCHARSET_TAGS (Fget_charset (charset));
}

DEFUN ("charset-dimension", Fcharset_dimension, 1, 1, 0, /*
Return dimension of CHARSET.  See `make-charset'.
*/
       (charset))
{
  return make_fixnum (XCHARSET_DIMENSION (Fget_charset (charset)));
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
  if (EQ (prop, Qtags))        return CHARSET_TAGS (cs);
  if (EQ (prop, Qdoc_string))  return CHARSET_DOC_STRING (cs);
  if (EQ (prop, Qdimension))   return make_fixnum (CHARSET_DIMENSION (cs));
  if (EQ (prop, Qcolumns))     return make_fixnum (CHARSET_COLUMNS (cs));
  if (EQ (prop, Qgraphic))     return make_fixnum (CHARSET_GRAPHIC (cs));
  if (EQ (prop, Qfinal))
    {
      if (CHARSET_FINAL (cs))
	return make_char (CHARSET_FINAL (cs));
      else
	return Qnil;
    }
  if (EQ (prop, Qchars))
    {
      if (CHARSET_DIMENSION (cs) == 1)
	return make_fixnum (CHARSET_CHARS (cs, 1));
      else
	return list2 (make_fixnum (CHARSET_CHARS (cs, 0)),
		      make_fixnum (CHARSET_CHARS (cs, 1)));
    }
  if (EQ (prop, Qoffset))
    {
      if (CHARSET_DIMENSION (cs) == 1)
	return make_fixnum (CHARSET_OFFSET (cs, 1));
      else
	return list2 (make_fixnum (CHARSET_OFFSET (cs, 0)),
		      make_fixnum (CHARSET_OFFSET (cs, 1)));
    }
  if (EQ (prop, Qregistries))    return CHARSET_REGISTRIES (cs);
  if (EQ (prop, Qccl_program)) return CHARSET_CCL_PROGRAM (cs);
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
normally used only by CCL.
*/
	(charset))
{
  return make_fixnum (XCHARSET_ID (Fget_charset (charset)));
}

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
  Elemcount i; 
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

DEFUN ("set-charset-tags", Fset_charset_tags, 2, 2, 0, /*
Set the `tags' property of CHARSET to TAGS.
See `make-charset' and `define-charset-tag' for more info about charset tags.
Normally, you should not change the tags of a charset after it has been
created.  This function exists primarily so that the startup Lisp code can
set the tags on charsets created in C, which are created too early to have
their own tags set on them.
*/
       (charset, tags))
{
  charset = Fget_charset (charset);
  call2 (Qregister_charset_tags, charset, tags);
  XCHARSET_TAGS (charset) = tags;
  charset_created_recalculate_unicode_precedence ();
  return Qnil;
}


/************************************************************************/
/*                       Other charset functions                        */
/************************************************************************/

DEFUN ("charsets-in-region", Fcharsets_in_region, 2, 3, 0, /*
Return a list of the charsets in the region between START and END.
BUFFER defaults to the current buffer if omitted.
*/
       (start, end, buffer))
{
  struct buffer *buf = decode_buffer (buffer, 1);
  Charbpos pos, stop;	/* Limits of the region. */
  Lisp_Object_dynarr *charset_dyn;
  Lisp_Object charset_list = Qnil;
  int i;

  /* Get and coerce the buffer region */
  get_buffer_range_char (buf, start, end, &pos, &stop, 0);

  charset_dyn = Dynarr_new (Lisp_Object);

  /* Find the actual charsets */
  find_charsets_in_buffer (charset_dyn, buf, pos, stop - pos);

  /* Convert dynarr to list */
  for (i = 0; i < Dynarr_length (charset_dyn); i++)
    {
      Lisp_Object charset = Dynarr_at (charset_dyn, i);
      charset_list = Fcons (XCHARSET_NAME (charset), charset_list);
    }

  Dynarr_free (charset_dyn);
  return Fnreverse (charset_list);
}

/* Split up multiplexed CODEPOINT in charset CHARSET into C1 and C2.
   If nil, substitute DEFAULT_C1 and DEFAULT_C2; else make sure it's an
   integer and within range of the limits as specified in CHARSET. */

static void
split_combined_codepoint (Lisp_Object charset, Lisp_Object codepoint,
			  int default_c1, int default_c2,
			  int *c1, int *c2)
{
  if (NILP (codepoint))
    {
      *c1 = default_c1;
      *c2 = default_c2;
    }
  else
    {
      EMACS_INT cp;
      CHECK_NATNUM (codepoint);
      cp = XFIXNUM (codepoint);
      *c1 = cp >> 8;
      *c2 = cp & 255;
      check_integer_range (make_fixnum (*c1),
                           make_fixnum (XCHARSET_MIN_CODE (charset, 0)),
                           make_fixnum (XCHARSET_MAX_CODE (charset, 0)));
      check_integer_range (make_fixnum (*c2),
                           make_fixnum (XCHARSET_MIN_CODE (charset, 1)),
                           make_fixnum (XCHARSET_MAX_CODE (charset, 1)));
    }
}

DEFUN ("map-charset-chars", Fmap_charset_chars, 2, 5, 0, /*
Call FUNCTION for all or a range of characters in CHARSET.
FUNCTION is called with two arguments: a cons (FROM . TO), where FROM and TO
indicate a range of characters contained in CHARSET, and ARG.  For
compatibility with GNU Emacs, FUNCTION will be called with two arguments
whether or not ARG is given.

The optional 4th and 5th arguments FROM-CODE and TO-CODE specify a
range of codepoints in CHARSET to map over; if omitted, the entire
set of characters in CHARSET is mapped over.  If CHARSET has one dimension,
FROM-CODE and TO-CODE are simply codepoints.  However, if CHARSET has two
dimensions, FROM-CODE and TO-CODE are multiplexed codepoints, where the
octets C1 and C2 are stored in the high and low byte, respectively.
*/
       (func, charset, arg, from_code, to_code))
{
  int low_c1, low_c2, high_c1, high_c2;

  charset = Fget_charset (charset);
  split_combined_codepoint (charset, from_code, XCHARSET_MIN_CODE (charset, 0),
			    XCHARSET_MIN_CODE (charset, 1), &low_c1, &low_c2);
  split_combined_codepoint (charset, to_code, XCHARSET_MAX_CODE (charset, 0),
			    XCHARSET_MAX_CODE (charset, 1), &high_c1, &high_c2);

#ifndef UNICODE_INTERNAL
  {
    int i;

    for (i = low_c1; i <= high_c1; i++)
      {
	Ichar low =
	  charset_codepoint_to_ichar (charset, i, low_c2, CONVERR_FAIL);
	Ichar high =
	  charset_codepoint_to_ichar (charset, i, high_c2, CONVERR_FAIL);
	assert (low >= 0);
	assert (high >= 0);
	call2 (func, Fcons (make_char (low), make_char (high)), arg);
      }
  }
#else
  {
    /* #### Perhaps we should use a range table instead? */
    unsigned_char_dynarr *seen = Dynarr_new (unsigned_char);
    int depth = specpdl_depth ();
    int i, j;

    record_unwind_protect_freeing_dynarr (seen);

    /* First, make a note of all Unicode codepoints seen in charset. */
    for (i = low_c1; i <= high_c1; i++)
      for (j = low_c2; j <= high_c2; j++)
	{
	  int code = charset_codepoint_to_unicode (charset, i, j,
						   CONVERR_FAIL);
	  if (code >= 0)
	    Dynarr_set (seen, code, 1);
	}

    j = -1;
    /* Now, go through and snarf off the ranges */
    for (i = 0; i < Dynarr_length (seen); i++)
      {
	/* If we've seen a 1 ... */
	if (Dynarr_at (seen, i))
	  {
	    /* If this is the first time, make note of it */
	    if (j == -1)
	      j = i;
	  }
	/* Else, we've seen a 0.  If we have a beginning of range noted,
	   we've just passed the end of the range, so we've got a range. */
	else if (j != -1)
	  {
	    call2 (func, Fcons (make_char (j), make_char (i - 1)), arg);
	    j = -1;
	  }
      }

    /* If the last codepoint is valid, we still have a range to process */
    if (j != -1)
      call2 (func, Fcons (make_char (j), make_char (i - 1)), arg);

    unbind_to (depth);
  }
#endif /* UNICODE_INTERNAL */
  
  return Qnil;
}


/************************************************************************/
/*                            memory usage                              */
/************************************************************************/

#ifdef MEMORY_USAGE_STATS

struct charset_stats
{
  struct usage_stats u;
  Bytecount from_unicode;
  Bytecount to_unicode;
};

static void
compute_charset_usage (Lisp_Object charset, struct charset_stats *stats,
		      struct usage_stats *ustats)
{
  stats->from_unicode += compute_from_unicode_table_size (charset, ustats);
  stats->to_unicode += compute_to_unicode_table_size (charset, ustats);
}

static void
charset_memory_usage (Lisp_Object charset, struct generic_usage_stats *gustats)
{
  struct charset_stats *stats = (struct charset_stats *) gustats;

  compute_charset_usage (charset, stats, &stats->u);
}

#endif /* MEMORY_USAGE_STATS */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
mule_charset_objects_create (void)
{
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_METHOD (charset, memory_usage);
#endif
  OBJECT_HAS_PREMETHOD (charset, disksave);
}

void
syms_of_mule_charset (void)
{
  INIT_LISP_OBJECT (charset);

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
  DEFSUBR (Fcharset_tags);
  DEFSUBR (Fcharset_description);
  DEFSUBR (Fcharset_dimension);
  DEFSUBR (Fcharset_property);
  DEFSUBR (Fcharset_id);
  DEFSUBR (Fset_charset_ccl_program);
  DEFSUBR (Fset_charset_registries);
  DEFSUBR (Fset_charset_tags);
  DEFSUBR (Fcharsets_in_region);
  DEFSUBR (Fmap_charset_chars);

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

  DEFSYMBOL (Qregister_charset_tags);

#ifdef FIXED_UNICODE_CHARSETS

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

#endif /* FIXED_UNICODE_CHARSETS */
}

void
vars_of_mule_charset (void)
{
  int i, j, k;

#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_PROPERTY
    (charset, memusage_stats_list, list2 (Qfrom_unicode, Qto_unicode));
#endif /* MEMORY_USAGE_STATS */

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

  chlook->next_allocated_dim1_id = MIN_OFFICIAL_DIM1_CHARSET_ID;
  chlook->next_allocated_dim2_id = MIN_OFFICIAL_DIM2_CHARSET_ID;

  /* Under old-Mule, make sure the ID's are above all encodable ID's.
     Under Unicode-internal, this value starts at 0, and all charsets have
     ID's from this pool. */
  next_non_encodable_charset_id = 1 + MAX_ENCODABLE_CHARSET_ID;
#endif /* not UNICODE_INTERNAL */

  dump_add_opaque_int (&next_non_encodable_charset_id);
  staticpro (&Vcharset_hash_table);
  Vcharset_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, Qeq);

  staticpro (&Vcharset_id_table);
  Vcharset_id_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, Qeq);
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
		  build_ascstring ("ASCII"),
		  build_defer_string ("ASCII"),
		  build_defer_string ("ASCII (left half of ISO 8859-1)"),
		  vector1 (build_ascstring ("iso8859-1")), Qnil, Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_control_1);
  Vcharset_control_1 =
    make_charset (MAKE_CSID (CONTROL_1), 0, Qcontrol_1, 1,
		  1, 32, 0, 128, 1, 1, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_ascstring ("C1"),
		  build_defer_string ("Control characters"),
		  build_defer_string ("Control characters 128-159"),
		  vector1 (build_ascstring ("iso8859-1")), Qnil, Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_1);
  Vcharset_latin_iso8859_1 =
    make_charset (MAKE_CSID (LATIN_ISO8859_1), 0, Qlatin_iso8859_1, 1,
		  1, 96, 0, 160, 1, 1, 'A',
		  CHARSET_LEFT_TO_RIGHT,
		  build_ascstring ("Latin-1"),
		  build_defer_string ("RHP of Latin-1 (ISO 8859-1): ISO-IR-100"),
		  build_defer_string ("Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100"),
		  vector1 (build_ascstring ("iso8859-1")), Qnil, Qnil, 0,
		  CSET_INTERNAL);
  staticpro (&Vcharset_latin_iso8859_2);
  Vcharset_latin_iso8859_2 = Qnil;
  staticpro (&Vcharset_latin_iso8859_3);
  Vcharset_latin_iso8859_3 = Qnil;
  staticpro (&Vcharset_latin_iso8859_4);
  Vcharset_latin_iso8859_4 = Qnil;
  staticpro (&Vcharset_thai_tis620);
  Vcharset_thai_tis620 = Qnil;
  staticpro (&Vcharset_arabic_iso8859_6);
  Vcharset_arabic_iso8859_6 = Qnil;
  staticpro (&Vcharset_greek_iso8859_7);
  Vcharset_greek_iso8859_7 = Qnil;
  staticpro (&Vcharset_hebrew_iso8859_8);
  Vcharset_hebrew_iso8859_8 = Qnil;
  staticpro (&Vcharset_katakana_jisx0201);
  Vcharset_katakana_jisx0201 = Qnil;
  staticpro (&Vcharset_latin_jisx0201);
  Vcharset_latin_jisx0201 = Qnil;
  staticpro (&Vcharset_cyrillic_iso8859_5);
  Vcharset_cyrillic_iso8859_5 = Qnil;
  staticpro (&Vcharset_latin_iso8859_9);
  Vcharset_latin_iso8859_9 = Qnil;
  staticpro (&Vcharset_latin_iso8859_15);
  Vcharset_latin_iso8859_15 = Qnil;
  staticpro (&Vcharset_chinese_sisheng);
  Vcharset_chinese_sisheng = Qnil;
  staticpro (&Vcharset_japanese_jisx0208_1978);
  Vcharset_japanese_jisx0208_1978 = Qnil;
  staticpro (&Vcharset_chinese_gb2312);
  Vcharset_chinese_gb2312 = Qnil;
  staticpro (&Vcharset_japanese_jisx0208);
  Vcharset_japanese_jisx0208 = Qnil;
  staticpro (&Vcharset_korean_ksc5601);
  Vcharset_korean_ksc5601 = Qnil;
  staticpro (&Vcharset_japanese_jisx0212);
  Vcharset_japanese_jisx0212 = Qnil;
  staticpro (&Vcharset_chinese_cns11643_1);
  Vcharset_chinese_cns11643_1 = Qnil;
  staticpro (&Vcharset_chinese_cns11643_2);
  Vcharset_chinese_cns11643_2 = Qnil;
#ifdef UNICODE_INTERNAL
  /* We can support Shift-JIS and Big5 directly.*/
  staticpro (&Vcharset_japanese_shift_jis);
  Vcharset_japanese_shift_jis = Qnil;
  staticpro (&Vcharset_chinese_big5);
  Vcharset_chinese_big5 = Qnil;
#else
  staticpro (&Vcharset_chinese_big5_1);
  Vcharset_chinese_big5_1 = Qnil;
  staticpro (&Vcharset_chinese_big5_2);
  Vcharset_chinese_big5_2 = Qnil;
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
		  build_ascstring ("Composite"),
		  build_defer_string ("Composite characters"),
		  build_defer_string ("Composite characters"),
		  vector1 (build_ascstring ("")), Qnil, Qnil, 0, CSET_INTERNAL);
#else
  /* We create a hack so that we have a way of storing ESC 0 and ESC 1
     sequences as "characters", so that they will be output correctly. */
  staticpro (&Vcharset_composite);
  Vcharset_composite =
    make_charset (MAKE_CSID (COMPOSITE), 0, Qcomposite, 1,
		  1, 96, 0, 32, 1, 0, '|',
		  CHARSET_LEFT_TO_RIGHT,
		  build_ascstring ("Composite hack"),
		  build_defer_string ("Composite characters hack"),
		  build_defer_string ("Composite characters hack"),
		  vector1 (build_ascstring ("")), Qnil, Qnil, 0, CSET_INTERNAL);
#endif /* ENABLE_COMPOSITE_CHARS */

#ifdef FIXED_UNICODE_CHARSETS

#define FROB_1(id, lclow, lchigh, uclow, uchigh)			\
  staticpro (&Vcharset_unicode_##lclow##_##lchigh);			\
  Vcharset_unicode_##lclow##_##lchigh =					\
    make_charset (id, 1,						\
		  Qunicode_##lclow##_##lchigh, 2,			\
		  96, 96, 32, 32, 2, 0, 0,				\
		  CHARSET_LEFT_TO_RIGHT,				\
		  build_ascstring ("Unicode " #uclow "-" #uchigh),		\
		  build_defer_string ("Unicode subset (U+" #uclow "..U+" #uchigh), \
		  build_defer_string ("Unicode subset (U+" #uclow "..U+" #uchigh "used for maintaining round-trip\n" \
"compatibility for Unicode characters that have no representation in any\n" \
"other charset."),							\
		  build_ascstring ("ISO10646-1"), Qnil, Qnil, 0, 0x##lclow)

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

#endif /* (not) FIXED_UNICODE_CHARSETS */

  initialize_ascii_control_1_latin_1_unicode_translation ();
}

void
init_mule_charset (void)
{
  /* Retrieve all the charsets by name.  Do this after loadup, when they
     were created. */
  Vcharset_latin_iso8859_2 = Fget_charset (Qlatin_iso8859_2);
  Vcharset_latin_iso8859_3 = Fget_charset (Qlatin_iso8859_3);
  Vcharset_latin_iso8859_4 = Fget_charset (Qlatin_iso8859_4);
  Vcharset_thai_tis620 = Fget_charset (Qthai_tis620);
  Vcharset_arabic_iso8859_6 = Fget_charset (Qarabic_iso8859_6);
  Vcharset_greek_iso8859_7 = Fget_charset (Qgreek_iso8859_7);
  Vcharset_hebrew_iso8859_8 = Fget_charset (Qhebrew_iso8859_8);
  Vcharset_katakana_jisx0201 = Fget_charset (Qkatakana_jisx0201);
  Vcharset_latin_jisx0201 = Fget_charset (Qlatin_jisx0201);
  Vcharset_cyrillic_iso8859_5 = Fget_charset (Qcyrillic_iso8859_5);
  Vcharset_latin_iso8859_9 = Fget_charset (Qlatin_iso8859_9);
  Vcharset_latin_iso8859_15 = Fget_charset (Qlatin_iso8859_15);
  Vcharset_japanese_jisx0208_1978 = Fget_charset (Qjapanese_jisx0208_1978);
  Vcharset_chinese_gb2312 = Fget_charset (Qchinese_gb2312);
  Vcharset_japanese_jisx0208 = Fget_charset (Qjapanese_jisx0208);
  Vcharset_korean_ksc5601 = Fget_charset (Qkorean_ksc5601);
  Vcharset_japanese_jisx0212 = Fget_charset (Qjapanese_jisx0212);
  Vcharset_chinese_cns11643_1 = Fget_charset (Qchinese_cns11643_1);
  Vcharset_chinese_cns11643_2 = Fget_charset (Qchinese_cns11643_2);
#ifdef UNICODE_INTERNAL
  /* We can support Shift-JIS and Big5 directly.*/
  Vcharset_japanese_shift_jis = Fget_charset (Qjapanese_shift_jis);
  Vcharset_chinese_big5 = Fget_charset (Qchinese_big5);
#else
  Vcharset_chinese_big5_1 = Fget_charset (Qchinese_big5_1);
  Vcharset_chinese_big5_2 = Fget_charset (Qchinese_big5_2);
#endif /* UNICODE_INTERNAL */
}
