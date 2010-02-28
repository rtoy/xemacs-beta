/* Header for charsets.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2005, 2009, 2010 Ben Wing.

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

/* Synched up with: Mule 2.3.  Not in FSF. */

/* Rewritten by Ben Wing <ben@xemacs.org>. */

#ifndef INCLUDED_charset_h_
#define INCLUDED_charset_h_

#ifdef MULE
#include "unicode.h"
#endif

/* If defined, we always use the maximum depth for the translation tables.
   This will increase their size to a certain extent but speed up lookup,
   as it eliminates all branches. */
#define MAXIMIZE_UNICODE_TABLE_DEPTH

/* These are placeholders.  In case we decide to be more clever and allow
   for different sizes of the to-Unicode tables depending on the size of
   the associated charset, we will have to change the places where these
   constants are referenced.  Doing this requires having multiple versions
   of the blank to-Unicode tables; probably not worth it since the
   to-Unicode tables are not space hogs compared to the from-Unicode tables,
   in any case. */

#define CHARSET_MIN_OFFSET 0
#define CHARSET_MAX_SIZE 256

void get_charset_limits (Lisp_Object charset, int *low0, int *low1, int *high0,
			 int *high1);
int get_charset_iso2022_type (Lisp_Object charset);
void unicode_to_charset_codepoint_raw (int code,
					     Lisp_Object precarray,
					     charset_pred predicate,
					     Lisp_Object *charset, int *c1,
					     int *c2);
Ichar old_mule_non_ascii_charset_codepoint_to_ichar_raw (Lisp_Object charset,
							 int c1, int c2);
void old_mule_non_ascii_itext_to_charset_codepoint_raw (const Ibyte *ptr,
							Lisp_Object *charset,
							int *c1,
							int *c2);
Bytecount old_mule_non_ascii_charset_codepoint_to_itext_raw (Lisp_Object charset,
							     int c1, int c2,
							     Ibyte *ptr);
void old_mule_non_ascii_ichar_to_charset_codepoint_raw (Ichar c,
							Lisp_Object *charset,
							int *c1, int *c2);
Bytecount non_ascii_charset_codepoint_to_itext (Lisp_Object charset, int c1,
						int c2, Ibyte *ptr,
						enum converr fail);
int charset_codepoint_to_private_unicode (Lisp_Object charset, int c1, int c2);
void private_unicode_to_charset_codepoint (int priv, Lisp_Object *charset,
					   int *c1, int *c2);
extern Lisp_Object Vcharset_hash_table;


#ifndef MULE

/************************************************************************/
/*                            fake charset defs                         */
/************************************************************************/

/* used when MULE is not defined, so that Charset-type stuff can still
   be done */

#define Vcharset_ascii Qascii
#define Vcharset_control_1 Qcontrol_1
#define XCHARSET_CCL_PROGRAM(cs) Qnil
#define XCHARSET_DIMENSION(cs) 1
#define XCHARSET_NAME(cs) (cs)
DECLARE_INLINE_HEADER (
int
XCHARSET_MIN_CODE (Lisp_Object UNUSED (cs), int UNUSED (dim))
)
{
  return 0;
}

DECLARE_INLINE_HEADER (
int
XCHARSET_MAX_CODE (Lisp_Object UNUSED (cs), int dim)
)
{
  if (dim == 0)
    return 0;
  assert (dim == 1);
  return 255;
}

#define Fget_charset(cs) (cs)
#define Fcharset_list() list1 (Vcharset_ascii)

#else /* MULE */


/************************************************************************/
/*            Information about a particular character set              */
/************************************************************************/

struct Lisp_Charset
{
  struct LCRECORD_HEADER header;

  int id; /* ID for this charset.  If old-Mule, this is the actual leading
	     byte for this charset; otherwise, an arbitrary unique value. */
  Lisp_Object name; /* Unique symbol that identifies this charset */
  Lisp_Object doc_string; /* */
  Lisp_Object registries; /* list of regexps matching XLFD registry portion */
  Lisp_Object short_name;
  Lisp_Object long_name;
  Lisp_Object unicode_map;
  Lisp_Object tags;

  Lisp_Object reverse_direction_charset;

  Lisp_Object ccl_program;

  /* Unicode translation tables.  See unicode.c for the format of these
     tables, and discussion of how they are initialized.
  */
  void *to_unicode_table;
  void *from_unicode_table;
  int from_unicode_levels;
  /* A value (combination of two octets) that is not a legal index in this
     charset. #### Problematic with a full 256x256 charset.  To get around
     this partially, we choose a value that is outside the range of nearly
     all charsets, and unlikely in Unicode.  #### But to do it properly,
     we need a separate table tracking all entries that map to this value.
     Most likely we should use a hash table; you're unlikely to have many
     entries mapping to the same conversion value.  Use a simple NULL
     pointer for an empty hash table, in the common case where no entries
     at all have that value. */
  UINT_16_BIT badval;

  /* Final byte of this character set in ISO2022 designating escape
     sequence */
  Ibyte final;

  /* Number of columns a character in this charset takes up, on TTY
     devices.  Not used for X devices. */
  int columns;

  /* Direction of this character set */
  int direction;

  /* Number of bytes used in encoding of this character set (1 or 2) */
  int dimension;

  /* Number of chars in each dimension (usually 94 or 96) */
  int chars[2];

  /* Minimum offset of index in each dimension (usually 33 for dimension <= 94,
     32 for dimension 95 or 96, 0 otherwise). */
  int offset[2];

  /* Which half of font to be used to display this character set */
  int graphic;

#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
  /* If >= 0, indicates a charset where the conversion between Unicode
     and its members is "algorithmic" in a simple linear fashion, starting
     at the value of ALGO_LOW.  Currently used only under old-Mule for
     handling the crockish Unicode-subset charsets. */
  int algo_low;
#endif /* ALLOW_ALGORITHMIC_CONVERSION_TABLES */

#ifndef UNICODE_INTERNAL
  /* If set, this is a "just-in-time" charset created for use in
     representing Unicode codepoints that can't be converted to charset
     codepoints.  */
  unsigned int jit_charset_p :1;
#endif /* not UNICODE_INTERNAL */

  /* Number of Unicode mappings involving Unicode codepoints 0 - 127.
     NOTE: Currently we don't really need to keep track of how many mappings
     there are, just whether we've set a mapping.  But if we ever added
     functionality to remove a mapping, we'd have to keep track of the number
     of mappings to determine whether there are any currently. */
  int number_of_ascii_mappings;

  /* If set, this is a "temporary" charset created when we encounter
     an unknown final.  This is so that we can successfully compile
     and load such files.  We allow a real charset to be created on top
     of this temporary charset. */
  unsigned int temporary :1;
};
typedef struct Lisp_Charset Lisp_Charset;

DECLARE_LRECORD (charset, Lisp_Charset);
#define XCHARSET(x) XRECORD (x, charset, Lisp_Charset)
#define wrap_charset(p) wrap_record (p, charset)
#define CHARSETP(x) RECORDP (x, charset)
#define CHECK_CHARSET(x) CHECK_RECORD (x, charset)
#define CONCHECK_CHARSET(x) CONCHECK_RECORD (x, charset)

/* These are special types used in conjunction with ISO-2022, which only
   allows charsets of these dimensions.  In general, we allow charsets of
   any dimensions as long as no single dimension exceeeds 256. */
#define CHARSET_TYPE_94    0	/* This charset includes 94    characters. */
#define CHARSET_TYPE_96    1	/* This charset includes 96    characters. */
#define CHARSET_TYPE_94X94 2	/* This charset includes 94x94 characters. */
#define CHARSET_TYPE_96X96 3	/* This charset includes 96x96 characters. */

#define CHARSET_LEFT_TO_RIGHT	0
#define CHARSET_RIGHT_TO_LEFT	1

#define CHARSET_CHARS(cs, dim)	 ((cs)->chars[dim])
#define CHARSET_COLUMNS(cs)	 ((cs)->columns)
#define CHARSET_DIMENSION(cs)	 ((cs)->dimension)
#define CHARSET_DIRECTION(cs)	 ((cs)->direction)
#define CHARSET_DOC_STRING(cs)	 ((cs)->doc_string)
#define CHARSET_FINAL(cs)	 ((cs)->final)
#define CHARSET_FROM_UNICODE_LEVELS(cs) ((cs)->from_unicode_levels)
#define CHARSET_FROM_UNICODE_TABLE(cs) ((cs)->from_unicode_table)
#define CHARSET_GRAPHIC(cs)	 ((cs)->graphic)
#define CHARSET_ID(cs)		 ((cs)->id)
#define CHARSET_LONG_NAME(cs)	 ((cs)->long_name)
#define CHARSET_NAME(cs)	 ((cs)->name)
#define CHARSET_OFFSET(cs, dim)	 ((cs)->offset[dim])
#define CHARSET_REGISTRIES(cs)	 ((cs)->registries)
#define CHARSET_REVERSE_DIRECTION_CHARSET(cs) ((cs)->reverse_direction_charset)
#define CHARSET_SHORT_NAME(cs)	 ((cs)->short_name)
#define CHARSET_TO_UNICODE_TABLE(cs) ((cs)->to_unicode_table)
#define CHARSET_UNICODE_MAP(cs)	 ((cs)->unicode_map)
#define CHARSET_TAGS(cs)	 ((cs)->tags)
#define CHARSET_MIN_CODE(cs, dim) CHARSET_OFFSET (cs, dim)
#define CHARSET_MAX_CODE(cs, dim) \
  (CHARSET_OFFSET (cs, dim) + CHARSET_CHARS (cs, dim) - 1)


#define XCHARSET_CHARS(cs, dim)	  CHARSET_CHARS        (XCHARSET (cs), dim)
#define XCHARSET_COLUMNS(cs)	  CHARSET_COLUMNS      (XCHARSET (cs))
#define XCHARSET_DIMENSION(cs)	  CHARSET_DIMENSION    (XCHARSET (cs))
#define XCHARSET_DIRECTION(cs)	  CHARSET_DIRECTION    (XCHARSET (cs))
#define XCHARSET_DOC_STRING(cs)	  CHARSET_DOC_STRING   (XCHARSET (cs))
#define XCHARSET_FINAL(cs)	  CHARSET_FINAL        (XCHARSET (cs))
#define XCHARSET_FROM_UNICODE_LEVELS(cs) CHARSET_FROM_UNICODE_LEVELS (XCHARSET (cs))
#define XCHARSET_FROM_UNICODE_TABLE(cs) CHARSET_FROM_UNICODE_TABLE (XCHARSET (cs))
#define XCHARSET_GRAPHIC(cs)      CHARSET_GRAPHIC      (XCHARSET (cs))
#define XCHARSET_ID(cs)		  CHARSET_ID           (XCHARSET (cs))
#define XCHARSET_LONG_NAME(cs)	  CHARSET_LONG_NAME    (XCHARSET (cs))
#define XCHARSET_NAME(cs)	  CHARSET_NAME         (XCHARSET (cs))
#define XCHARSET_OFFSET(cs, dim)  CHARSET_OFFSET       (XCHARSET (cs), dim)
#define XCHARSET_REGISTRIES(cs)	  CHARSET_REGISTRIES     (XCHARSET (cs))
#define XCHARSET_REVERSE_DIRECTION_CHARSET(cs)   CHARSET_REVERSE_DIRECTION_CHARSET (XCHARSET (cs))
#define XCHARSET_SHORT_NAME(cs)	  CHARSET_SHORT_NAME   (XCHARSET (cs))
#define XCHARSET_TO_UNICODE_TABLE(cs) CHARSET_TO_UNICODE_TABLE (XCHARSET (cs))
#define XCHARSET_UNICODE_MAP(cs)  CHARSET_UNICODE_MAP         (XCHARSET (cs))
#define XCHARSET_TAGS(cs)         CHARSET_TAGS         (XCHARSET (cs))
#define XCHARSET_MIN_CODE(cs, dim) CHARSET_MIN_CODE (XCHARSET (cs), dim)
#define XCHARSET_MAX_CODE(cs, dim) CHARSET_MAX_CODE (XCHARSET (cs), dim)

#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
#define CHARSET_ALGO_LOW(cs)	 ((cs)->algo_low)
#define XCHARSET_ALGO_LOW(cs)	  CHARSET_ALGO_LOW     (XCHARSET (cs))
#endif /* ALLOW_ALGORITHMIC_CONVERSION_TABLES */

#define CHARSET_CCL_PROGRAM(cs)  ((cs)->ccl_program)
#define XCHARSET_CCL_PROGRAM(cs)  CHARSET_CCL_PROGRAM  (XCHARSET (cs))

struct charset_lookup
{
  /* Table of charsets indexed by type/final-byte/direction, for
     ISO2022-compatible charsets. */
  Lisp_Object charset_by_attributes[4][128][2];

#ifndef UNICODE_INTERNAL
  /* Table of charsets indexed by ID, for encodable ID's.  This is purely
     an optimization, as we also have a hash table mapping ID's to charsets
     for charsets of all sorts, encodable or not. */
  Lisp_Object charset_by_encodable_id[NUM_ENCODABLE_CHARSET_IDS];

  int next_allocated_dim1_id;
  int next_allocated_dim2_id;
#endif /* not UNICODE_INTERNAL */
};

extern struct charset_lookup *chlook;

Lisp_Object charset_by_id (int id);

#ifndef UNICODE_INTERNAL

/* Given an encodable charset ID, return the corresponding charset.
 */

DECLARE_INLINE_HEADER (
Lisp_Object
charset_by_encodable_id (int id)
)
{
#ifdef ERROR_CHECK_TEXT
  /* When error-checking is on, x86 GCC 2.95.2 -O3 miscompiles the
     following unless we introduce `tem'. */
  int tem = id;
  text_checking_assert (tem >= MIN_ENCODABLE_CHARSET_ID &&
			tem <= MAX_ENCODABLE_CHARSET_ID);
#endif
  return chlook->charset_by_encodable_id[id - MIN_ENCODABLE_CHARSET_ID];
}

#endif /* not UNICODE_INTERNAL */

DECLARE_INLINE_HEADER (
Lisp_Object
charset_by_attributes (int type, int final, int dir)
)
{
  text_checking_assert (type  >= 0 &&
			type  < countof (chlook->charset_by_attributes) &&
			final >= 0 &&
			final < countof (chlook->charset_by_attributes[0]) &&
			dir   >= 0 &&
			dir   < countof (chlook->charset_by_attributes[0][0]));
  return chlook->charset_by_attributes[type][final][dir];
}

#endif /* MULE */


/************************************************************************/
/*                     General character manipulation                   */
/************************************************************************/

#ifdef MULE

DECLARE_INLINE_HEADER (
int
valid_charset_codepoint_p (Lisp_Object charset, int c1, int c2)
)
{
  int l1, l2, h1, h2;
  get_charset_limits (charset, &l1, &l2, &h1, &h2);
  return c1 >= l1 && c1 <= h1 && c2 >= l2 && c2 <= h2;
}

#define ASSERT_VALID_CHARSET_CODEPOINT(charset, c1, c2)			\
do {									\
  text_checking_assert (CHARSETP (charset));				\
  text_checking_assert (valid_charset_codepoint_p (charset, c1, c2));	\
} while (0)
#define INLINE_ASSERT_VALID_CHARSET_CODEPOINT(charset, c1, c2)		\
do {									\
  inline_text_checking_assert (CHARSETP (charset));			\
  inline_text_checking_assert (valid_charset_codepoint_p (charset, c1, c2)); \
} while (0)

#else /* not MULE */

#define ASSERT_VALID_CHARSET_CODEPOINT(charset, a1, a2)		\
do								\
  {								\
    text_checking_assert (EQ (charset, Vcharset_ascii));	\
    text_checking_assert (a1 == 0);				\
    text_checking_assert (a2 >= 0 && a2 <= 255);		\
  }								\
while (0)
#define INLINE_ASSERT_VALID_CHARSET_CODEPOINT(charset, a1, a2)	\
do								\
  {								\
    inline_text_checking_assert (EQ (charset, Vcharset_ascii));	\
    inline_text_checking_assert (a1 == 0);			\
    inline_text_checking_assert (a2 >= 0 && a2 <= 255);		\
  }								\
while (0)

#endif /* MULE */



#define ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR(charset, c1, c2)	\
do									\
{									\
  if (!NILP (charset))							\
    ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);			\
} while (0)
#define INLINE_ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR(charset, c1, c2)	\
do									\
{									\
  if (!NILP (charset))							\
    INLINE_ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);		\
} while (0)

#define HANDLE_CHARSET_CODEPOINT_ERROR(errtext, errval, charset, c1, c2, fail)\
do									\
{									\
  if (NILP (*charset))							\
    {									\
      switch (fail)							\
	{								\
	case CONVERR_FAIL:						\
	  break;							\
									\
	case CONVERR_ABORT:						\
	default:							\
	  ABORT (); break;						\
									\
	case CONVERR_ERROR:						\
	  text_conversion_error (errtext, errval);			\
									\
	case CONVERR_SUCCEED:						\
	case CONVERR_SUBSTITUTE:					\
	  *charset = Vcharset_ascii;					\
	  *c1 = 0;							\
	  *c2 = CANT_CONVERT_CHAR_WHEN_ENCODING;			\
	  break;							\
	}								\
    }									\
}									\
while (0)

#define HANDLE_UNICODE_ERROR(errtext, errval, code, charset, c1, c2, fail) \
do									\
{									\
  if (code < 0)								\
    {									\
      switch (fail)							\
	{								\
	case CONVERR_FAIL:						\
	  break;							\
									\
	case CONVERR_ABORT:						\
	default:							\
	  ABORT (); break;						\
									\
	case CONVERR_ERROR:						\
	  text_conversion_error (errtext, errval);			\
									\
	case CONVERR_SUCCEED:						\
	case CONVERR_SUBSTITUTE:					\
	  code = UNICODE_REPLACEMENT_CHAR;				\
	  break;							\
									\
	case CONVERR_USE_PRIVATE:					\
	  code = charset_codepoint_to_private_unicode (charset, c1, c2); \
	  break;							\
	}								\
    }									\
}									\
while (0)

/* WARNING: Unlike the previous two, this should be called *AFTER*
   detecting an error condition, and will return the appropriate value
   rather than storing it. */

#define HANDLE_ICHAR_ERROR(errtext, errval, fail)	\
do							\
{							\
switch (fail)						\
  {							\
  case CONVERR_FAIL:					\
    return -1;						\
							\
  case CONVERR_ABORT:					\
  default:						\
    ABORT (); return -1;				\
							\
  case CONVERR_ERROR:					\
    text_conversion_error (errtext, errval);		\
							\
  case CONVERR_SUCCEED:					\
  case CONVERR_SUBSTITUTE:				\
    return CANT_CONVERT_CHAR_WHEN_DECODING;		\
  }							\
}							\
while (0)

#ifdef MULE

/* Convert a charset codepoint (CHARSET, one or two octets) to Unicode.
   Return -1 if can't convert. */

DECLARE_INLINE_HEADER (
int
charset_codepoint_to_unicode_raw_1 (Lisp_Object charset, int c1, int c2
				    INLINE_TEXT_CHECK_ARGS)
)
{
  int retval;

  INLINE_ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
#ifdef ALLOW_ALGORITHMIC_CONVERSION_TABLES
  {
    /* Conceivably a good idea not to have this in Unicode-internal, since
       it slows down this function slightly, and it may be called a huge
       number of times when reading in a file.  Probably doesn't matter,
       though. */
    int algo_low = XCHARSET_ALGO_LOW (charset);
    if (algo_low >= 0)
      {
	retval = (c1 - XCHARSET_OFFSET (charset, 0)) *
	  XCHARSET_CHARS (charset, 1) + (c2 - XCHARSET_OFFSET (charset, 1)) +
	  algo_low;
      }
  }
#endif /* ALLOW_ALGORITHMIC_CONVERSION_TABLES */
#ifndef MAXIMIZE_UNICODE_TABLE_DEPTH
  if (XCHARSET_DIMENSION (charset) == 1)
    retval = ((int *) XCHARSET_TO_UNICODE_TABLE (charset))
      [c2- CHARSET_MIN_OFFSET];
  else
#endif /* not MAXIMIZE_UNICODE_TABLE_DEPTH */
    retval = ((int **) XCHARSET_TO_UNICODE_TABLE (charset))
      [c1 - CHARSET_MIN_OFFSET][c2 - CHARSET_MIN_OFFSET];
  ASSERT_VALID_UNICODE_CODEPOINT_OR_ERROR (retval);
  return retval;
}

#define charset_codepoint_to_unicode_raw(charset, c1, c2) \
  charset_codepoint_to_unicode_raw_1 (charset, c1, c2 INLINE_TEXT_CHECK_CALL)

#endif /* MULE */

/* Convert a charset codepoint to Unicode, with error behavior specifiable.
   FAIL controls what happens when the charset codepoint cannot be
   converted to an official Unicode codepoint.

   This is inline because these functions are often called with a constant
   value for FAIL, and a good inlining optimizing compiler will strip away
   all the garbage so that a call to charset_codepoint_unicode with
   CONVERR_FAIL reduces directly to a call to
   charset_codepoint_to_unicode_raw_1(). */

DECLARE_INLINE_HEADER (
int
charset_codepoint_to_unicode (Lisp_Object charset, int c1, int c2,
			      enum converr USED_IF_MULE (fail))
)
{
#ifdef MULE
  int code;

  code = charset_codepoint_to_unicode_raw (charset, c1, c2);
  HANDLE_UNICODE_ERROR ("Can't convert charset codepoint to Unicode",
			XCHARSET_DIMENSION (charset) == 2 ?
			list3 (charset, make_int (c1), make_int (c2)) :
			list2 (charset, make_int (c2)),
			code, charset, c1, c2, fail);
  ASSERT_VALID_UNICODE_CODEPOINT_OR_ERROR (code);
  return code;
#else /* not MULE */
  ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
  return (int) c2;
#endif /* (not) MULE */
}

/* Convert Unicode codepoint to charset codepoint.  FAIL determines what to
   do upon failure to convert (if fail == CONVERR_FAIL, CHARSET will be nil
   when unable to convert).  Requires a precedence list of charsets, to
   determine the order that charsets are checked for conversion codepoints.
   Also takes a predicate to determine which charsets to check when looking
   for mappings; it's passed a charset object and should return non-zero if
   the charset should be checked.  If predicate is NULL, check all
   charsets. */

DECLARE_INLINE_HEADER (
void
filtered_unicode_to_charset_codepoint (int code,
				       Lisp_Object USED_IF_MULE (precarray),
				       charset_pred USED_IF_MULE (predicate),
				       Lisp_Object *charset, int *c1, int *c2,
				       enum converr fail)
)
{
  ASSERT_VALID_UNICODE_CODEPOINT (code);
#ifdef MULE
  if (code <= 0x7F && !XPRECEDENCE_ARRAY (precarray)->has_overriding_ascii
      && (!predicate || (*predicate) (Vcharset_ascii)))
    {
      *charset = Vcharset_ascii;
      *c1 = 0;
      *c2 = code;
    }
  else
    unicode_to_charset_codepoint_raw (code, precarray, predicate,
				      charset, c1, c2);
#else /* not MULE */
  if (code > 255)
    *charset = Qnil, *c1 = -1, *c2 = -1;
  else
    {
      *charset = Vcharset_ascii;
      *c1 = 0;
      *c2 = code;
    }
#endif /* (not) MULE */
  HANDLE_CHARSET_CODEPOINT_ERROR
    ("Can't convert Unicode codepoint to charset codepoint", make_int (code),
     charset, c1, c2, fail);
  ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR (*charset, *c1, *c2);
}

/* Convert Unicode codepoint to charset codepoint.  FAIL determines what to
   do upon failure to convert (if fail == CONVERR_FAIL, CHARSET will be nil
   when unable to convert).  Requires a precedence list of charsets, to
   determine the order that charsets are checked for conversion codepoints.
   
   Same as filtered_unicode_to_charset_codepoint() but without a filtering
   predicate. */

DECLARE_INLINE_HEADER (
void
unicode_to_charset_codepoint (int code, Lisp_Object precarray,
			      Lisp_Object *charset, int *c1, int *c2,
			      enum converr fail)
)
{
  filtered_unicode_to_charset_codepoint (code, precarray, NULL, charset,
					 c1, c2, fail);
}

/* Return a character whose charset is CHARSET and position-codes are C1
   and C2.  C1 and C2 must be within the range of the charset. (For
   charsets of dimension 1, C1 must be 0.)

   The allowed range of a charset is derived from way the charset is usually
   coded in a simple MBCS representation.

   For ISO-2022 charsets, the range of each position code is either 32/33
   to 127/126 or 160/161 to 255/254, with the choice of low or high range
   depending on the way the charset is usually coded in a simple MBCS
   representation (this choice is almost always derivable from the ISO-2022
   `graphic' property of the charset).  The choice of larger or smaller
   range (bounds to the left and right of the slash, respectively) depends
   on the size of the charset (94 or 94x94 vs. 96 or 96x96).

   See `make-char'.

   Return (Ichar) -1 in Unicode-internal if no (official) Unicode
   equivalent for this charset codepoint.  See also
   charset_codepoint_to_ichar(), which allows other possibilties for
   handling such cases.
   */


DECLARE_INLINE_HEADER (
Ichar
charset_codepoint_to_ichar_raw (Lisp_Object charset, int c1, int c2)
)
{
#ifdef UNICODE_INTERNAL
  return (Ichar) charset_codepoint_to_unicode_raw (charset, c1, c2);

#else /* not UNICODE_INTERNAL */
  Ichar retval;
  ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
# ifdef MULE
  if (EQ (charset, Vcharset_ascii))
    retval = c2;
  else
    retval = old_mule_non_ascii_charset_codepoint_to_ichar_raw (charset, c1,
								c2);
# else /* not MULE */
  retval = c2;
# endif /* (not) MULE */
  ASSERT_VALID_ICHAR_OR_ERROR (retval);
  return retval;
#endif /* (not) UNICODE_INTERNAL */
}

/* Convert a charset codepoint into a character, as for
   charset_codepoint_to_ichar_raw().  FAIL controls what happens when the
   charset codepoint cannot be converted to Unicode. */
DECLARE_INLINE_HEADER (
Ichar
charset_codepoint_to_ichar (Lisp_Object charset, int c1, int c2,
			    enum converr USED_IF_UNICODE_INTERNAL (fail))
)
{
#ifdef UNICODE_INTERNAL
  return (Ichar) charset_codepoint_to_unicode (charset, c1, c2, fail);
#else
  return charset_codepoint_to_ichar_raw (charset, c1, c2);
#endif /* (not) UNICODE_INTERNAL */
}

/* Given an Ichar and charset precedence dynarr, convert it to a charset
   codepoint.  CHARSET will be nil if no conversion possible.

   Under Unicode-internal:

   Requires a precedence list of charsets, to determine the order that
   charsets are checked for conversion codepoints.  Also takes a predicate
   to determine which charsets to check when looking for mappings; it's
   passed a charset object and should return non-zero if the charset should
   be checked.  If predicate is NULL, check all charsets.
*/
 
DECLARE_INLINE_HEADER (
void
filtered_ichar_to_charset_codepoint (Ichar ch, Lisp_Object 
				     USED_IF_UNICODE_INTERNAL (precarray),
				     charset_pred
				     USED_IF_UNICODE_INTERNAL (predicate),
				     Lisp_Object *charset, int *c1, int *c2,
				     enum converr
				     USED_IF_UNICODE_INTERNAL (fail))
)
{
  ASSERT_VALID_ICHAR (ch);
#ifdef UNICODE_INTERNAL
  filtered_unicode_to_charset_codepoint ((int) ch, precarray, predicate,
					 charset, c1, c2, fail);
  HANDLE_CHARSET_CODEPOINT_ERROR
    ("Can't convert character to charset codepoint", make_char (ch),
     charset, c1, c2, fail);
  ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR (*charset, *c1, *c2);
#else
# ifdef MULE
  if (ch <= 0x7F)
    {
      *charset = Vcharset_ascii;
      *c1 = 0;
      *c2 = (int) ch;
    }
  else
    old_mule_non_ascii_ichar_to_charset_codepoint_raw (ch, charset, c1, c2);
# else /* not MULE */
  *charset = Vcharset_ascii;
  *c1 = 0;
  *c2 = (int) ch;
# endif /* (not) MULE */
  /* This should not fail */
  ASSERT_VALID_CHARSET_CODEPOINT (*charset, *c1, *c2);
#endif /* (not) UNICODE_INTERNAL */
}

/* Given an Ichar and charset precedence dynarr, convert it to a charset
   codepoint.  CHARSET will be nil if no conversion possible.

   Requires a precedence list of charsets, to determine the order that
   charsets are checked for conversion codepoints.
*/

DECLARE_INLINE_HEADER (
void
ichar_to_charset_codepoint (Ichar ch, Lisp_Object precarray,
			    Lisp_Object *charset, int *c1, int *c2,
			    enum converr fail)
)
{
  filtered_ichar_to_charset_codepoint (ch, precarray, NULL, charset,
				       c1, c2, fail);
}

#ifndef UNICODE_INTERNAL
#define old_mule_ichar_charset(ch) \
  charset_by_encodable_id (old_mule_ichar_charset_id (ch))
#endif /* not UNICODE_INTERNAL */

/* Convert a charset codepoint into a character in the internal string
   representation.  Return number of bytes written out.  FAIL controls
   failure mode when charset conversion to Unicode is not possible. */
DECLARE_INLINE_HEADER (
Bytecount
charset_codepoint_to_itext (Lisp_Object charset, int c1, int c2, Ibyte *ptr,
			    enum converr USED_IF_MULE (fail))
)
{
  ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
#ifdef MULE
  if (EQ (charset, Vcharset_ascii))
    {
      ptr[0] = (Ibyte) c2;
      return 1;
    }
  return non_ascii_charset_codepoint_to_itext (charset, c1, c2, ptr, fail);
#else
  ptr[0] = (Ibyte) c2;
  return 1;
#endif /* (not) MULE */
}

#ifdef MULE

/* Convert a character in the internal string representation (guaranteed
   not to be ASCII under old-Mule) into a charset codepoint.  CHARSET will
   be nil if no conversion possible. */
DECLARE_INLINE_HEADER (
void
itext_to_charset_codepoint_raw (const Ibyte *ptr,
				Lisp_Object
				USED_IF_UNICODE_INTERNAL (precarray),
				charset_pred
				USED_IF_UNICODE_INTERNAL (predicate),
				Lisp_Object *charset, int *c1,
				int *c2)
)
{
#ifdef UNICODE_INTERNAL
  unicode_to_charset_codepoint_raw
    ((int) itext_ichar (ptr), precarray, predicate, charset, c1, c2);
#else
  old_mule_non_ascii_itext_to_charset_codepoint_raw (ptr, charset, c1, c2);
#endif /* (not) UNICODE_INTERNAL */
}

#endif /* MULE */

/* Convert a character in the internal string representation into a charset
   codepoint.  CHARSET will be nil if no conversion possible.

   Under Unicode-internal:

   Requires a precedence list of charsets, to determine the order that
   charsets are checked for conversion codepoints.  Also takes a predicate
   to determine which charsets to check when looking for mappings; it's
   passed a charset object and should return non-zero if the charset should
   be checked.  If predicate is NULL, check all charsets.

   Under old-Mule:

   Ignores PRECARRAY and PREDICATE.
 */
DECLARE_INLINE_HEADER (
void
filtered_itext_to_charset_codepoint_1 (const Ibyte *ptr,
				       Lisp_Object USED_IF_MULE (precarray),
				       charset_pred USED_IF_MULE (predicate),
				       Lisp_Object *charset,
				       int *c1, int *c2)
)
{
  ASSERT_VALID_ITEXT (ptr);
#ifdef MULE
  if (byte_ascii_p (*ptr)
# ifdef UNICODE_INTERNAL
      &&
      !XPRECEDENCE_ARRAY (precarray)->has_overriding_ascii &&
      (!predicate || (*predicate) (Vcharset_ascii))
# endif
      )
    {
      *charset = Vcharset_ascii;
      *c1 = 0;
      *c2 = *ptr;
    }
  else
    itext_to_charset_codepoint_raw (ptr, precarray, predicate,
				    charset, c1, c2);
#else /* not MULE */
  *charset = Vcharset_ascii;
  *c1 = 0;
  *c2 = *ptr;
#endif /* (not) MULE */
  ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR (*charset, *c1, *c2);
}

DECLARE_INLINE_HEADER (
void
filtered_itext_to_charset_codepoint (const Ibyte *ptr, Lisp_Object precarray,
				     charset_pred predicate,
				     Lisp_Object *charset, int *c1, int *c2,
				     enum converr fail)
)
{
  filtered_itext_to_charset_codepoint_1 (ptr, precarray, predicate, charset,
					 c1, c2);
  HANDLE_CHARSET_CODEPOINT_ERROR
    ("Can't convert character to charset codepoint",
     make_char (itext_ichar (ptr)), charset, c1, c2, fail);
  ASSERT_VALID_CHARSET_CODEPOINT_OR_ERROR (*charset, *c1, *c2);
}

DECLARE_INLINE_HEADER (
void
itext_to_charset_codepoint (const Ibyte *ptr, Lisp_Object precarray,
			    Lisp_Object *charset, int *c1, int *c2,
			    enum converr fail)
)
{
  filtered_itext_to_charset_codepoint (ptr, precarray, NULL, charset,
				       c1, c2, fail);
}

#ifdef MULE

/* Convert a charset codepoint (guaranteed not to be ASCII) into a
   character in the internal string representation and write to dynarr DST.
   Returns number of bytes added to the Dynarr.  FAIL controls failure
   mode when charset conversion to Unicode is not possible. */
DECLARE_INLINE_HEADER (
Bytecount
non_ascii_charset_codepoint_to_dynarr (Lisp_Object charset, int c1, int c2,
				       unsigned_char_dynarr *dst,
				       enum converr fail)
)
{
  /* Potentially, we could rewrite the routines that write out to an Ibyte*
     to work directly with Dynarrs, but it would be a lot of code
     duplication and it's not clear it would be any faster. */
  Ibyte work[MAX_ICHAR_LEN];
  Bytecount len;

  text_checking_assert (!EQ (charset, Vcharset_ascii));
  len = non_ascii_charset_codepoint_to_itext (charset, c1, c2, work, fail);
  if (len)
    Dynarr_add_many (dst, work, len);
  return len;
}

#endif /* MULE */

/* Convert a charset codepoint into a character in the internal string
   representation and write to dynarr DST.  Returns length of chars added
   to the Dynarr.  FAIL controls failure mode when charset conversion to
   Unicode is not possible. */
DECLARE_INLINE_HEADER (
int
charset_codepoint_to_dynarr (Lisp_Object charset, int c1, int c2,
			     unsigned_char_dynarr *dst,
			     enum converr USED_IF_MULE (fail))
)
{
  ASSERT_VALID_CHARSET_CODEPOINT (charset, c1, c2);
#ifdef MULE
  if (EQ (charset, Vcharset_ascii))
    {
      Dynarr_add (dst, (Ibyte) c2);
      return 1;
    }

  return non_ascii_charset_codepoint_to_dynarr (charset, c1, c2, dst, fail);
#else /* not MULE */
  Dynarr_add (dst, (Ibyte) c2);
  return 1;
#endif /* (not) MULE */
}

void set_charset_registries (Lisp_Object charset, Lisp_Object registries);

#endif /* not INCLUDED_charset_h_ */
