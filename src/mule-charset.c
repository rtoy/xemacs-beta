/* Functions to handle multilingual characters.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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
#include "mule-ccl.h"

/* The various pre-defined charsets. */

Lisp_Object Vcharset_ascii;
Lisp_Object Vcharset_control_1;
Lisp_Object Vcharset_latin_iso8859_1;
Lisp_Object Vcharset_latin_iso8859_2;
Lisp_Object Vcharset_latin_iso8859_3;
Lisp_Object Vcharset_latin_iso8859_4;
Lisp_Object Vcharset_thai_tis620;
Lisp_Object Vcharset_greek_iso8859_7;
Lisp_Object Vcharset_arabic_iso8859_6;
Lisp_Object Vcharset_hebrew_iso8859_8;
Lisp_Object Vcharset_katakana_jisx0201;
Lisp_Object Vcharset_latin_jisx0201;
Lisp_Object Vcharset_cyrillic_iso8859_5;
Lisp_Object Vcharset_latin_iso8859_9;
Lisp_Object Vcharset_latin_iso8859_15;
Lisp_Object Vcharset_japanese_jisx0208_1978;
Lisp_Object Vcharset_chinese_gb2312;
Lisp_Object Vcharset_japanese_jisx0208;
Lisp_Object Vcharset_korean_ksc5601;
Lisp_Object Vcharset_japanese_jisx0212;
Lisp_Object Vcharset_chinese_cns11643_1;
Lisp_Object Vcharset_chinese_cns11643_2;
Lisp_Object Vcharset_chinese_big5_1;
Lisp_Object Vcharset_chinese_big5_2;
Lisp_Object Vcharset_composite;

struct charset_lookup *chlook;

static const struct lrecord_description charset_lookup_description_1[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (struct charset_lookup, charset_by_leading_byte), NUM_LEADING_BYTES+4*128*2 },
  { XD_END }
};

static const struct struct_description charset_lookup_description = {
  sizeof (struct charset_lookup),
  charset_lookup_description_1
};

Lisp_Object Qcharsetp;

/* Qdoc_string, Qdimension, Qchars defined in general.c */
Lisp_Object Qregistry, Qfinal, Qgraphic;
Lisp_Object Qdirection;
Lisp_Object Qreverse_direction_charset;
Lisp_Object Qshort_name, Qlong_name;

Lisp_Object Qfrom_unicode, Qto_unicode;

Lisp_Object
  Qlatin_iso8859_1,
  Qlatin_iso8859_2,
  Qlatin_iso8859_3,
  Qlatin_iso8859_4,
  Qthai_tis620,
  Qgreek_iso8859_7,
  Qarabic_iso8859_6,
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
  Qchinese_big5_1,
  Qchinese_big5_2,
  Qcomposite;

Lisp_Object Ql2r, Qr2l;

Lisp_Object Vcharset_hash_table;


/************************************************************************/
/*                            charset object                            */
/************************************************************************/

static Lisp_Object
mark_charset (Lisp_Object obj)
{
  Lisp_Charset *cs = XCHARSET (obj);

  mark_object (cs->short_name);
  mark_object (cs->long_name);
  mark_object (cs->doc_string);
  mark_object (cs->registry);
  mark_object (cs->ccl_program);
  return cs->name;
}

static void
print_charset (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  Lisp_Charset *cs = XCHARSET (obj);

  if (print_readably)
    printing_unreadable_object ("#<charset %s 0x%x>",
				XSTRING_DATA (XSYMBOL (CHARSET_NAME (cs))->
					     name),
				cs->header.uid);

  write_fmt_string_lisp (printcharfun, "#<charset %s %S %S %S", 4,
			 CHARSET_NAME (cs), CHARSET_SHORT_NAME (cs),
			 CHARSET_LONG_NAME (cs), CHARSET_DOC_STRING (cs));
  write_fmt_string (printcharfun, " %s %s cols=%d g%d final='%c' reg=",
		    CHARSET_TYPE (cs) == CHARSET_TYPE_94    ? "94" :
		    CHARSET_TYPE (cs) == CHARSET_TYPE_96    ? "96" :
		    CHARSET_TYPE (cs) == CHARSET_TYPE_94X94 ? "94x94" :
		    "96x96",
		    CHARSET_DIRECTION (cs) == CHARSET_LEFT_TO_RIGHT ? "l2r" :
		    "r2l",
		    CHARSET_COLUMNS (cs),
		    CHARSET_GRAPHIC (cs),
		    CHARSET_FINAL (cs));
  print_internal (CHARSET_REGISTRY (cs), printcharfun, 0);
  write_fmt_string (printcharfun, " 0x%x>", cs->header.uid);
}

static void
finalize_charset (void *header, int for_disksave)
{
  /* See mule-charset.h, definition of Lisp_Charset. */
  Lisp_Object charset = wrap_charset ((Lisp_Charset *) header);
  if (for_disksave && XCHARSET_TO_UNICODE_TABLE (charset))
    {
      /* Control-1, ASCII, Composite don't have tables */
      free_charset_unicode_tables (charset);
      XCHARSET_TO_UNICODE_TABLE (charset) = 0;
      XCHARSET_FROM_UNICODE_TABLE (charset) = 0;
    }
}

static const struct lrecord_description charset_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, doc_string) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, registry) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, short_name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, long_name) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, reverse_direction_charset) },
  { XD_LISP_OBJECT, offsetof (Lisp_Charset, ccl_program) },
#if 0
  /* #### XD_UNION not yet implemented!  pdump version of XEmacs will
     not work! */
  { XD_UNION, offsetof (Lisp_Charset, to_unicode_table),
      XD_INDIRECT (offsetof (Lisp_Charset, dimension), 0),
      to_unicode_description },
  { XD_UNION, offsetof (Lisp_Charset, from_unicode_table),
      XD_INDIRECT (offsetof (Lisp_Charset, from_unicode_levels), 0),
      from_unicode_description },
#endif
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("charset", charset,
                               mark_charset, print_charset, finalize_charset,
			       0, 0, charset_description, Lisp_Charset);

/* Make a new charset. */
/* #### SJT Should generic properties be allowed? */
static Lisp_Object
make_charset (int id, Lisp_Object name, int rep_bytes,
	      int type, int columns, int graphic,
	      Ibyte final, int direction,  Lisp_Object short_name,
	      Lisp_Object long_name, Lisp_Object doc,
	      Lisp_Object reg, int overwrite)
{
  Lisp_Object obj;
  Lisp_Charset *cs;

  if (!overwrite)
    {
      cs = alloc_lcrecord_type (Lisp_Charset, &lrecord_charset);
      zero_lcrecord (cs);
      obj = wrap_charset (cs);

      if (final)
	{
	  /* some charsets do not have final characters.  This includes
	     ASCII, Control-1, Composite, and the two faux private
	     charsets. */
	  assert (NILP (chlook->
			charset_by_attributes[type][final][direction]));
	  chlook->charset_by_attributes[type][final][direction] = obj;
	}

      assert (NILP (chlook->charset_by_leading_byte[id - MIN_LEADING_BYTE]));
      chlook->charset_by_leading_byte[id - MIN_LEADING_BYTE] = obj;
    }
  else
    {
      Lisp_Object ret;
      /* Actually overwrite the properties of the existing charset.
	 We do this because until now charsets could never be "deleted",
	 so parts of the code don't bother to GC charsets. */
      obj = chlook->charset_by_leading_byte[id - MIN_LEADING_BYTE];
      cs = XCHARSET (obj);
      assert (EQ (chlook->charset_by_attributes[type][final][direction],
		  obj));
      
      ret = Fremhash (XCHARSET_NAME (obj), Vcharset_hash_table);
      assert (!NILP (ret));
    }

  CHARSET_ID		(cs) = id;
  CHARSET_NAME		(cs) = name;
  CHARSET_SHORT_NAME	(cs) = short_name;
  CHARSET_LONG_NAME	(cs) = long_name;
  CHARSET_REP_BYTES	(cs) = rep_bytes;
  CHARSET_DIRECTION	(cs) = direction;
  CHARSET_TYPE		(cs) = type;
  CHARSET_COLUMNS	(cs) = columns;
  CHARSET_GRAPHIC	(cs) = graphic;
  CHARSET_FINAL		(cs) = final;
  CHARSET_DOC_STRING	(cs) = doc;
  CHARSET_REGISTRY	(cs) = reg;
  CHARSET_CCL_PROGRAM	(cs) = Qnil;
  CHARSET_REVERSE_DIRECTION_CHARSET (cs) = Qnil;

  CHARSET_DIMENSION (cs) = (CHARSET_TYPE (cs) == CHARSET_TYPE_94 ||
			    CHARSET_TYPE (cs) == CHARSET_TYPE_96) ? 1 : 2;
  CHARSET_CHARS (cs) = (CHARSET_TYPE (cs) == CHARSET_TYPE_94 ||
			CHARSET_TYPE (cs) == CHARSET_TYPE_94X94) ? 94 : 96;

  if (id == LEADING_BYTE_ASCII || id == LEADING_BYTE_CONTROL_1
#ifdef ENABLE_COMPOSITE_CHARS
      || id == LEADING_BYTE_COMPOSITE
#endif
      )
    assert (!overwrite);
  else
    {
      if (overwrite)
	free_charset_unicode_tables (obj);
      init_charset_unicode_tables (obj);
    }

  /* Some charsets are "faux" and don't have names or really exist at
     all except in the leading-byte table. */
  if (!NILP (name))
    {
      assert (NILP (Fgethash (name, Vcharset_hash_table, Qnil)));
      Fputhash (name, obj, Vcharset_hash_table);
    }

  recalculate_unicode_precedence ();
  return obj;
}

static int
get_unallocated_leading_byte (int dimension)
{
  int lb;

  if (dimension == 1)
    {
      if (chlook->next_allocated_1_byte_leading_byte >
	  MAX_LEADING_BYTE_PRIVATE_1)
	lb = 0;
      else
	lb = chlook->next_allocated_1_byte_leading_byte++;
    }
  else
    {
      if (chlook->next_allocated_2_byte_leading_byte >
	  MAX_LEADING_BYTE_PRIVATE_2)
	lb = 0;
      else
	lb = chlook->next_allocated_2_byte_leading_byte++;
    }

  if (!lb)
    invalid_operation
      ("No more character sets free for this dimension", make_int (dimension));

  return lb;
}


/************************************************************************/
/*                      Basic charset Lisp functions                    */
/************************************************************************/

void
get_charset_limits (Lisp_Object charset, int *low, int *high)
{
  Lisp_Charset *cs = XCHARSET (charset);

  if      (EQ (charset, Vcharset_ascii))     *low =  0, *high = 127;
  else if (EQ (charset, Vcharset_control_1)) *low =  0, *high =  31;
  else if (CHARSET_CHARS (cs) == 94)         *low = 33, *high = 126;
  else	/* CHARSET_CHARS (cs) == 96) */	     *low = 32, *high = 127;
}
     
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
add_charset_to_list_mapper (Lisp_Object key, Lisp_Object value,
			    void *charset_list_closure)
{
  /* This function can GC */
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
  struct gcpro gcpro1;
  struct charset_list_closure charset_list_closure;

  GCPRO1 (charset_list);
  charset_list_closure.charset_list = &charset_list;
  elisp_maphash (add_charset_to_list_mapper, Vcharset_hash_table,
		 &charset_list_closure);
  UNGCPRO;

  return charset_list;
}

DEFUN ("charset-name", Fcharset_name, 1, 1, 0, /*
Return the name of charset CHARSET.
*/
       (charset))
{
  return XCHARSET_NAME (Fget_charset (charset));
}

/* #### SJT Should generic properties be allowed? */
DEFUN ("make-charset", Fmake_charset, 3, 3, 0, /*
Define a new character set.
This function is for use with Mule support.
NAME is a symbol, the name by which the character set is normally referred.
DOC-STRING is a string describing the character set.
PROPS is a property list, describing the specific nature of the
character set.  Recognized properties are:

'short-name	Short version of the charset name (ex: Latin-1)
'long-name	Long version of the charset name (ex: ISO8859-1 (Latin-1))
'registry	A regular expression matching the font registry field for
		this character set.
'dimension	Number of octets used to index a character in this charset.
		Either 1 or 2.  Defaults to 1.
'columns	Number of columns used to display a character in this charset.
		Only used in TTY mode. (Under X, the actual width of a
		character can be derived from the font used to display the
		characters.) If unspecified, defaults to the dimension
		(this is almost	always the correct value).
'chars		Number of characters in each dimension (94 or 96).
		Defaults to 94.  Note that if the dimension is 2, the
		character set thus described is 94x94 or 96x96.
'final		Final byte of ISO 2022 escape sequence.  Must be
		supplied.  Each combination of (DIMENSION, CHARS) defines a
		separate namespace for final bytes.  Note that ISO
		2022 restricts the final byte to the range
		0x30 - 0x7E if dimension == 1, and 0x30 - 0x5F if
		dimension == 2.  Note also that final bytes in the range
		0x30 - 0x3F are reserved for user-defined (not official)
		character sets.
'graphic	0 (use left half of font on output) or 1 (use right half
		of font on output).  Defaults to 0.  For example, for
		a font whose registry is ISO8859-1, the left half
		(octets 0x20 - 0x7F) is the `ascii' character set, while
		the right half (octets 0xA0 - 0xFF) is the `latin-1'
		character set.  With 'graphic set to 0, the octets
		will have their high bit cleared; with it set to 1,
		the octets will have their high bit set.
'direction	'l2r (left-to-right) or 'r2l (right-to-left).
		Defaults to 'l2r.
'ccl-program	A compiled CCL program used to convert a character in
		this charset into an index into the font.  This is in
		addition to the 'graphic property.  The CCL program
		is passed the octets of the character, with the high
		bit cleared and set depending upon whether the value
		of the 'graphic property is 0 or 1.
*/
       (name, doc_string, props))
{
  int id, dimension = 1, chars = 94, graphic = 0, columns = -1;
  Ibyte final = 0;
  int direction = CHARSET_LEFT_TO_RIGHT;
  int type;
  Lisp_Object registry = Qnil;
  Lisp_Object charset = Qnil;
  Lisp_Object ccl_program = Qnil;
  Lisp_Object short_name = Qnil, long_name = Qnil;
  Lisp_Object existing_charset;
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
	      invalid_constant ("Invalid value for 'dimension", value);
	  }

	else if (EQ (keyword, Qchars))
	  {
	    CHECK_INT (value);
	    chars = XINT (value);
	    if (chars != 94 && chars != 96)
	      invalid_constant ("Invalid value for 'chars", value);
	  }

	else if (EQ (keyword, Qcolumns))
	  {
	    CHECK_INT (value);
	    columns = XINT (value);
	    if (columns != 1 && columns != 2)
	      invalid_constant ("Invalid value for 'columns", value);
	  }

	else if (EQ (keyword, Qgraphic))
	  {
	    CHECK_INT (value);
	    graphic = XINT (value);
	    if (graphic < 0 || graphic > 1)
	      invalid_constant ("Invalid value for 'graphic", value);
	  }

	else if (EQ (keyword, Qregistry))
	  {
	    CHECK_STRING (value);
	    registry = value;
	  }

	else if (EQ (keyword, Qdirection))
	  {
	    if (EQ (value, Ql2r))
	      direction = CHARSET_LEFT_TO_RIGHT;
	    else if (EQ (value, Qr2l))
	      direction = CHARSET_RIGHT_TO_LEFT;
	    else
	      invalid_constant ("Invalid value for 'direction", value);
	  }

	else if (EQ (keyword, Qfinal))
	  {
	    CHECK_CHAR_COERCE_INT (value);
	    final = XCHAR (value);
	    if (final < '0' || final > '~')
	      invalid_constant ("Invalid value for 'final", value);
	  }

	else if (EQ (keyword, Qccl_program))
	  {
	    struct ccl_program test_ccl;

	    if (setup_ccl_program (&test_ccl, value) < 0)
	      invalid_argument ("Invalid value for 'ccl-program", value);
	    ccl_program = value;
	  }
	else
	  invalid_constant ("Unrecognized property", keyword);
      }
  }

  if (!final)
    invalid_argument ("'final must be specified", Qunbound);
  if (dimension == 2 && final > 0x5F)
    invalid_constant
      ("Final must be in the range 0x30 - 0x5F for dimension == 2",
       make_char (final));

  if (dimension == 1)
    type = (chars == 94) ? CHARSET_TYPE_94    : CHARSET_TYPE_96;
  else
    type = (chars == 94) ? CHARSET_TYPE_94X94 : CHARSET_TYPE_96X96;

  existing_charset = charset_by_attributes (type, final, direction);

  if (!NILP (existing_charset) && !XCHARSET (existing_charset)->temporary)
    invalid_argument
      ("Character set already defined for this DIMENSION/CHARS/FINAL/DIRECTION combo",
       existing_charset);

  if (!NILP (existing_charset))
    /* Reuse same leading byte */
    id = XCHARSET_ID (existing_charset);
  else
    id = get_unallocated_leading_byte (dimension);

  if (temporary)
    {
      Ibyte tempname[80];

      qxesprintf (tempname, "___temporary___%d__", id);
      name = intern_int (tempname);
    }
  if (NILP (doc_string))
    doc_string = build_string ("");
  if (NILP (registry))
    registry = build_string ("");
  if (NILP (short_name))
    short_name = XSYMBOL (name)->name;
  if (NILP (long_name))
    long_name = doc_string;
  if (columns == -1)
    columns = dimension;

  charset = make_charset (id, name, dimension + 2, type, columns, graphic,
			  final, direction, short_name, long_name,
			  doc_string, registry, !NILP (existing_charset));

  XCHARSET (charset)->temporary = temporary;
  if (!NILP (ccl_program))
    XCHARSET_CCL_PROGRAM (charset) = ccl_program;

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
  int id, dimension, columns, graphic;
  Ibyte final;
  int direction, type;
  Lisp_Object registry, doc_string, short_name, long_name;
  Lisp_Charset *cs;

  charset = Fget_charset (charset);
  if (!NILP (XCHARSET_REVERSE_DIRECTION_CHARSET (charset)))
    invalid_operation ("Charset already has reverse-direction charset",
		       charset);

  CHECK_SYMBOL (new_name);
  if (!NILP (Ffind_charset (new_name)))
    invalid_operation ("Cannot redefine existing charset", new_name);

  cs = XCHARSET (charset);

  type      = CHARSET_TYPE      (cs);
  columns   = CHARSET_COLUMNS   (cs);
  dimension = CHARSET_DIMENSION (cs);
  id = get_unallocated_leading_byte (dimension);

  graphic = CHARSET_GRAPHIC (cs);
  final = CHARSET_FINAL (cs);
  direction = CHARSET_RIGHT_TO_LEFT;
  if (CHARSET_DIRECTION (cs) == CHARSET_RIGHT_TO_LEFT)
    direction = CHARSET_LEFT_TO_RIGHT;
  doc_string = CHARSET_DOC_STRING (cs);
  short_name = CHARSET_SHORT_NAME (cs);
  long_name = CHARSET_LONG_NAME (cs);
  registry = CHARSET_REGISTRY (cs);

  new_charset = make_charset (id, new_name, dimension + 2, type, columns,
			      graphic, final, direction, short_name, long_name,
			      doc_string, registry, 0);

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
If DIRECTION is omitted, both directions will be checked (left-to-right
will be returned if character sets exist for both directions).
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
    invalid_constant ("Invalid value for CHARS", chars);

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
'name and 'doc-string.
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
  if (EQ (prop, Qdoc_string))  return CHARSET_DOC_STRING (cs);
  if (EQ (prop, Qdimension))   return make_int (CHARSET_DIMENSION (cs));
  if (EQ (prop, Qcolumns))     return make_int (CHARSET_COLUMNS (cs));
  if (EQ (prop, Qgraphic))     return make_int (CHARSET_GRAPHIC (cs));
  if (EQ (prop, Qfinal))       return make_char (CHARSET_FINAL (cs));
  if (EQ (prop, Qchars))       return make_int (CHARSET_CHARS (cs));
  if (EQ (prop, Qregistry))    return CHARSET_REGISTRY (cs);
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
  RETURN_NOT_REACHED (Qnil)
}

DEFUN ("charset-id", Fcharset_id, 1, 1, 0, /*
Return charset identification number of CHARSET.
*/
	(charset))
{
  return make_int (XCHARSET_LEADING_BYTE (Fget_charset (charset)));
}

/* #### We need to figure out which properties we really want to
   allow to be set. */

DEFUN ("set-charset-ccl-program", Fset_charset_ccl_program, 2, 2, 0, /*
Set the 'ccl-program property of CHARSET to CCL-PROGRAM.
*/
       (charset, ccl_program))
{
  struct ccl_program test_ccl;

  charset = Fget_charset (charset);
  if (setup_ccl_program (&test_ccl, ccl_program) < 0)
    invalid_argument ("Invalid ccl-program", ccl_program);
  XCHARSET_CCL_PROGRAM (charset) = ccl_program;
  face_property_was_changed (Vdefault_face, Qfont, Qglobal);
  return Qnil;
}

static void
invalidate_charset_font_caches (Lisp_Object charset)
{
  /* Invalidate font cache entries for charset on all devices. */
  Lisp_Object devcons, concons, hash_table;
  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      hash_table = Fgethash (charset, d->charset_font_cache, Qunbound);
      if (!UNBOUNDP (hash_table))
        Fclrhash (hash_table);
    }
}

/* Japanese folks may want to (set-charset-registry 'ascii "jisx0201") */
DEFUN ("set-charset-registry", Fset_charset_registry, 2, 2, 0, /*
Set the 'registry property of CHARSET to REGISTRY.
*/
       (charset, registry))
{
  charset = Fget_charset (charset);
  CHECK_STRING (registry);
  XCHARSET_REGISTRY (charset) = registry;
  invalidate_charset_font_caches (charset);
  face_property_was_changed (Vdefault_face, Qfont, Qglobal);
  return Qnil;
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
  stats->other   += malloced_storage_size (c, sizeof (*c), ovstats);
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
  DEFSUBR (Fcharset_short_name);
  DEFSUBR (Fcharset_long_name);
  DEFSUBR (Fcharset_description);
  DEFSUBR (Fcharset_dimension);
  DEFSUBR (Fcharset_property);
  DEFSUBR (Fcharset_id);
  DEFSUBR (Fset_charset_ccl_program);
  DEFSUBR (Fset_charset_registry);

#ifdef MEMORY_USAGE_STATS
  DEFSUBR (Fcharset_memory_usage);
#endif

  DEFSYMBOL (Qcharsetp);
  DEFSYMBOL (Qregistry);
  DEFSYMBOL (Qfinal);
  DEFSYMBOL (Qgraphic);
  DEFSYMBOL (Qdirection);
  DEFSYMBOL (Qreverse_direction_charset);
  DEFSYMBOL (Qshort_name);
  DEFSYMBOL (Qlong_name);

  DEFSYMBOL (Qfrom_unicode);
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
  DEFSYMBOL (Qgreek_iso8859_7);
  DEFSYMBOL (Qarabic_iso8859_6);
  DEFSYMBOL (Qhebrew_iso8859_8);
  DEFSYMBOL (Qkatakana_jisx0201);
  DEFSYMBOL (Qlatin_jisx0201);
  DEFSYMBOL (Qcyrillic_iso8859_5);
  DEFSYMBOL (Qlatin_iso8859_9);
  DEFSYMBOL (Qlatin_iso8859_15);
  DEFSYMBOL (Qjapanese_jisx0208_1978);
  DEFSYMBOL (Qchinese_gb2312);
  DEFSYMBOL (Qjapanese_jisx0208);
  DEFSYMBOL (Qkorean_ksc5601);
  DEFSYMBOL (Qjapanese_jisx0212);
  DEFSYMBOL (Qchinese_cns11643_1);
  DEFSYMBOL (Qchinese_cns11643_2);
  DEFSYMBOL (Qchinese_big5_1);
  DEFSYMBOL (Qchinese_big5_2);

  DEFSYMBOL (Qcomposite);
}

static int
init_charset_unicode_tables_mapper (Lisp_Object key, Lisp_Object value,
				    void *closure)
{
  init_charset_unicode_tables (value);
  return 0;
}

void
init_mule_charset (void)
{
  /* See mule-charset.h, definition of Lisp_Charset. */
  if (initialized)
    elisp_maphash (init_charset_unicode_tables_mapper, Vcharset_hash_table,
		   0);
}

void
vars_of_mule_charset (void)
{
  int i, j, k;

  chlook = xnew_and_zero (struct charset_lookup); /* zero for Purify. */
  dump_add_root_struct_ptr (&chlook, &charset_lookup_description);

  /* Table of charsets indexed by leading byte. */
  for (i = 0; i < countof (chlook->charset_by_leading_byte); i++)
    chlook->charset_by_leading_byte[i] = Qnil;

  /* Table of charsets indexed by type/final-byte/direction. */
  for (i = 0; i < countof (chlook->charset_by_attributes); i++)
    for (j = 0; j < countof (chlook->charset_by_attributes[0]); j++)
      for (k = 0; k < countof (chlook->charset_by_attributes[0][0]); k++)
	chlook->charset_by_attributes[i][j][k] = Qnil;

  chlook->next_allocated_1_byte_leading_byte = MIN_LEADING_BYTE_PRIVATE_1;
  chlook->next_allocated_2_byte_leading_byte = MIN_LEADING_BYTE_PRIVATE_2;

  staticpro (&Vcharset_hash_table);
  Vcharset_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
}

void
complex_vars_of_mule_charset (void)
{
  /* Predefined character sets.  We store them into variables for
     ease of access. */

  staticpro (&Vcharset_ascii);
  Vcharset_ascii =
    make_charset (LEADING_BYTE_ASCII, Qascii, 1,
		  CHARSET_TYPE_94, 1, 0, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("ASCII"),
		  build_msg_string ("ASCII"),
		  build_msg_string ("ASCII (ISO646 IRV)"),
		  build_string ("\\(iso8859-[0-9]*\\|-ascii\\)"), 0);
  staticpro (&Vcharset_control_1);
  Vcharset_control_1 =
    make_charset (LEADING_BYTE_CONTROL_1, Qcontrol_1, 2,
		  CHARSET_TYPE_94, 1, 1, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("C1"),
		  build_msg_string ("Control characters"),
		  build_msg_string ("Control characters 128-191"),
		  build_string (""), 0);
  staticpro (&Vcharset_latin_iso8859_1);
  Vcharset_latin_iso8859_1 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_1, Qlatin_iso8859_1, 2,
		  CHARSET_TYPE_96, 1, 1, 'A',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-1"),
		  build_msg_string ("ISO8859-1 (Latin-1)"),
		  build_msg_string ("ISO8859-1 (Latin-1)"),
		  build_string ("iso8859-1"), 0);
  staticpro (&Vcharset_latin_iso8859_2);
  Vcharset_latin_iso8859_2 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_2, Qlatin_iso8859_2, 2,
		  CHARSET_TYPE_96, 1, 1, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-2"),
		  build_msg_string ("ISO8859-2 (Latin-2)"),
		  build_msg_string ("ISO8859-2 (Latin-2)"),
		  build_string ("iso8859-2"), 0);
  staticpro (&Vcharset_latin_iso8859_3);
  Vcharset_latin_iso8859_3 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_3, Qlatin_iso8859_3, 2,
		  CHARSET_TYPE_96, 1, 1, 'C',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-3"),
		  build_msg_string ("ISO8859-3 (Latin-3)"),
		  build_msg_string ("ISO8859-3 (Latin-3)"),
		  build_string ("iso8859-3"), 0);
  staticpro (&Vcharset_latin_iso8859_4);
  Vcharset_latin_iso8859_4 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_4, Qlatin_iso8859_4, 2,
		  CHARSET_TYPE_96, 1, 1, 'D',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-4"),
		  build_msg_string ("ISO8859-4 (Latin-4)"),
		  build_msg_string ("ISO8859-4 (Latin-4)"),
		  build_string ("iso8859-4"), 0);
  staticpro (&Vcharset_thai_tis620);
  Vcharset_thai_tis620 =
    make_charset (LEADING_BYTE_THAI_TIS620, Qthai_tis620, 2,
		  CHARSET_TYPE_96, 1, 1, 'T',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("TIS620"),
		  build_msg_string ("TIS620 (Thai)"),
		  build_msg_string ("TIS620.2529 (Thai)"),
		  build_string ("tis620"),0);
  staticpro (&Vcharset_greek_iso8859_7);
  Vcharset_greek_iso8859_7 =
    make_charset (LEADING_BYTE_GREEK_ISO8859_7, Qgreek_iso8859_7, 2,
		  CHARSET_TYPE_96, 1, 1, 'F',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("ISO8859-7"),
		  build_msg_string ("ISO8859-7 (Greek)"),
		  build_msg_string ("ISO8859-7 (Greek)"),
		  build_string ("iso8859-7"), 0);
  staticpro (&Vcharset_arabic_iso8859_6);
  Vcharset_arabic_iso8859_6 =
    make_charset (LEADING_BYTE_ARABIC_ISO8859_6, Qarabic_iso8859_6, 2,
		  CHARSET_TYPE_96, 1, 1, 'G',
		  CHARSET_RIGHT_TO_LEFT,
		  build_string ("ISO8859-6"),
		  build_msg_string ("ISO8859-6 (Arabic)"),
		  build_msg_string ("ISO8859-6 (Arabic)"),
		  build_string ("iso8859-6"), 0);
  staticpro (&Vcharset_hebrew_iso8859_8);
  Vcharset_hebrew_iso8859_8 =
    make_charset (LEADING_BYTE_HEBREW_ISO8859_8, Qhebrew_iso8859_8, 2,
		  CHARSET_TYPE_96, 1, 1, 'H',
		  CHARSET_RIGHT_TO_LEFT,
		  build_string ("ISO8859-8"),
		  build_msg_string ("ISO8859-8 (Hebrew)"),
		  build_msg_string ("ISO8859-8 (Hebrew)"),
		  build_string ("iso8859-8"), 0);
  staticpro (&Vcharset_katakana_jisx0201);
  Vcharset_katakana_jisx0201 =
    make_charset (LEADING_BYTE_KATAKANA_JISX0201, Qkatakana_jisx0201, 2,
		  CHARSET_TYPE_94, 1, 1, 'I',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("JISX0201 Kana"),
		  build_msg_string ("JISX0201.1976 (Japanese Kana)"),
		  build_msg_string ("JISX0201.1976 Japanese Kana"),
		  build_string ("jisx0201.1976"), 0);
  staticpro (&Vcharset_latin_jisx0201);
  Vcharset_latin_jisx0201 =
    make_charset (LEADING_BYTE_LATIN_JISX0201, Qlatin_jisx0201, 2,
		  CHARSET_TYPE_94, 1, 0, 'J',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("JISX0201 Roman"),
		  build_msg_string ("JISX0201.1976 (Japanese Roman)"),
		  build_msg_string ("JISX0201.1976 Japanese Roman"),
		  build_string ("jisx0201.1976"), 0);
  staticpro (&Vcharset_cyrillic_iso8859_5);
  Vcharset_cyrillic_iso8859_5 =
    make_charset (LEADING_BYTE_CYRILLIC_ISO8859_5, Qcyrillic_iso8859_5, 2,
		  CHARSET_TYPE_96, 1, 1, 'L',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("ISO8859-5"),
		  build_msg_string ("ISO8859-5 (Cyrillic)"),
		  build_msg_string ("ISO8859-5 (Cyrillic)"),
		  build_string ("iso8859-5"), 0);
  staticpro (&Vcharset_latin_iso8859_9);
  Vcharset_latin_iso8859_9 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_9, Qlatin_iso8859_9, 2,
		  CHARSET_TYPE_96, 1, 1, 'M',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-5"),
		  build_msg_string ("ISO8859-9 (Latin-5)"),
		  build_msg_string ("ISO8859-9 (Latin-5)"),
		  build_string ("iso8859-9"), 0);
  staticpro (&Vcharset_latin_iso8859_15);
  Vcharset_latin_iso8859_15 =
    make_charset (LEADING_BYTE_LATIN_ISO8859_15, Qlatin_iso8859_15, 2,
		  CHARSET_TYPE_96, 1, 1, 'b',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Latin-9"),
		  build_msg_string ("ISO8859-15 (Latin-9)"),
		  build_msg_string ("ISO8859-15 (Latin-9)"),
		  build_string ("iso8859-15"), 0);
  staticpro (&Vcharset_japanese_jisx0208_1978);
  Vcharset_japanese_jisx0208_1978 =
    make_charset (LEADING_BYTE_JAPANESE_JISX0208_1978, Qjapanese_jisx0208_1978, 3,
		  CHARSET_TYPE_94X94, 2, 0, '@',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("JISX0208.1978"),
		  build_msg_string ("JISX0208.1978 (Japanese)"),
		  build_msg_string
		  ("JISX0208.1978 Japanese Kanji (so called \"old JIS\")"),
		  build_string ("\\(jisx0208\\|jisc6226\\)\\.1978"), 0);
  staticpro (&Vcharset_chinese_gb2312);
  Vcharset_chinese_gb2312 =
    make_charset (LEADING_BYTE_CHINESE_GB2312, Qchinese_gb2312, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'A',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("GB2312"),
		  build_msg_string ("GB2312)"),
		  build_msg_string ("GB2312 Chinese simplified"),
		  build_string ("gb2312"), 0);
  staticpro (&Vcharset_japanese_jisx0208);
  Vcharset_japanese_jisx0208 =
    make_charset (LEADING_BYTE_JAPANESE_JISX0208, Qjapanese_jisx0208, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'B',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("JISX0208"),
		  build_msg_string ("JISX0208.1983/1990 (Japanese)"),
		  build_msg_string ("JISX0208.1983/1990 Japanese Kanji"),
		  build_string ("jisx0208.19\\(83\\|90\\)"), 0);
  staticpro (&Vcharset_korean_ksc5601);
  Vcharset_korean_ksc5601 =
    make_charset (LEADING_BYTE_KOREAN_KSC5601, Qkorean_ksc5601, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'C',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("KSC5601"),
		  build_msg_string ("KSC5601 (Korean"),
		  build_msg_string ("KSC5601 Korean Hangul and Hanja"),
		  build_string ("ksc5601"), 0);
  staticpro (&Vcharset_japanese_jisx0212);
  Vcharset_japanese_jisx0212 =
    make_charset (LEADING_BYTE_JAPANESE_JISX0212, Qjapanese_jisx0212, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'D',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("JISX0212"),
		  build_msg_string ("JISX0212 (Japanese)"),
		  build_msg_string ("JISX0212 Japanese Supplement"),
		  build_string ("jisx0212"), 0);

#define CHINESE_CNS_PLANE_RE(n) "cns11643[.-]\\(.*[.-]\\)?" n "$"
  staticpro (&Vcharset_chinese_cns11643_1);
  Vcharset_chinese_cns11643_1 =
    make_charset (LEADING_BYTE_CHINESE_CNS11643_1, Qchinese_cns11643_1, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'G',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("CNS11643-1"),
		  build_msg_string ("CNS11643-1 (Chinese traditional)"),
		  build_msg_string
		  ("CNS 11643 Plane 1 Chinese traditional"),
		  build_string (CHINESE_CNS_PLANE_RE("1")), 0);
  staticpro (&Vcharset_chinese_cns11643_2);
  Vcharset_chinese_cns11643_2 =
    make_charset (LEADING_BYTE_CHINESE_CNS11643_2, Qchinese_cns11643_2, 3,
		  CHARSET_TYPE_94X94, 2, 0, 'H',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("CNS11643-2"),
		  build_msg_string ("CNS11643-2 (Chinese traditional)"),
		  build_msg_string
		  ("CNS 11643 Plane 2 Chinese traditional"),
		  build_string (CHINESE_CNS_PLANE_RE("2")), 0);
  staticpro (&Vcharset_chinese_big5_1);
  Vcharset_chinese_big5_1 =
    make_charset (LEADING_BYTE_CHINESE_BIG5_1, Qchinese_big5_1, 3,
		  CHARSET_TYPE_94X94, 2, 0, '0',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Big5"),
		  build_msg_string ("Big5 (Level-1)"),
		  build_msg_string
		  ("Big5 Level-1 Chinese traditional"),
		  build_string ("big5"), 0);
  staticpro (&Vcharset_chinese_big5_2);
  Vcharset_chinese_big5_2 =
    make_charset (LEADING_BYTE_CHINESE_BIG5_2, Qchinese_big5_2, 3,
		  CHARSET_TYPE_94X94, 2, 0, '1',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Big5"),
		  build_msg_string ("Big5 (Level-2)"),
		  build_msg_string
		  ("Big5 Level-2 Chinese traditional"),
		  build_string ("big5"), 0);


#ifdef ENABLE_COMPOSITE_CHARS
  /* #### For simplicity, we put composite chars into a 96x96 charset.
     This is going to lead to problems because you can run out of
     room, esp. as we don't yet recycle numbers. */
  staticpro (&Vcharset_composite);
  Vcharset_composite =
    make_charset (LEADING_BYTE_COMPOSITE, Qcomposite, 3,
		  CHARSET_TYPE_96X96, 2, 0, 0,
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Composite"),
		  build_msg_string ("Composite characters"),
		  build_msg_string ("Composite characters"),
		  build_string (""), 0);
#else
  /* We create a hack so that we have a way of storing ESC 0 and ESC 1
     sequences as "characters", so that they will be output correctly. */
  staticpro (&Vcharset_composite);
  Vcharset_composite =
    make_charset (LEADING_BYTE_COMPOSITE_REPLACEMENT, Qcomposite, 2,
		  CHARSET_TYPE_96, 1, 1, '|',
		  CHARSET_LEFT_TO_RIGHT,
		  build_string ("Composite hack"),
		  build_msg_string ("Composite characters hack"),
		  build_msg_string ("Composite characters hack"),
		  build_string (""), 0);
#endif /* ENABLE_COMPOSITE_CHARS */
}
