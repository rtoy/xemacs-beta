/* Conversion functions for I18N encodings, but not Unicode (in separate file).
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002, 2005, 2010 Ben Wing.

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

/* Synched up with: Mule 2.3.   Not in FSF. */

/* For previous history, see file-coding.c.

   September 10, 2001: Extracted from file-coding.c by Ben Wing.

   Later in September: Finished abstraction of detection system, rewrote
   all the detectors to include multiple levels of likelihood.
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "elhash.h"
#include "extents.h"
#include "file-coding.h"
#include "mule-ccl.h"
#include "rangetab.h"
#include "buffer.h"
#include "extents.h"
#include "unicode.h"

#if defined (ENABLE_COMPOSITE_CHARS) && defined (UNICODE_INTERNAL)
#error "No prayer of getting these two working in its current shape"
#endif

Lisp_Object Qshift_jis, Qbig5, Qccl, Qmultibyte;
Lisp_Object Qcharset_g0, Qcharset_g1, Qcharset_g2, Qcharset_g3;
Lisp_Object Qforce_g0_on_output, Qforce_g1_on_output;
Lisp_Object Qforce_g2_on_output, Qforce_g3_on_output;
Lisp_Object Qno_iso6429, Qiso2022_preserve;
Lisp_Object Qinput_charset_conversion, Qoutput_charset_conversion;
Lisp_Object Qshort, Qno_ascii_eol, Qno_ascii_cntl, Qlock_shift;

Lisp_Object Qiso_7, Qiso_8_designate, Qiso_8_1, Qiso_8_2, Qiso_lock_shift;

Lisp_Object Qcharsets;

static Lisp_Object Vshift_jis_precedence, Vbig5_precedence;


/************************************************************************/
/*                          MBCS coding system                          */
/************************************************************************/

/* Not defined because it requires careful thinking out, and currently it
   doesn't seem necessary to allow this. */
/* #define ALLOW_MULTIBYTE_CHARSET_OVERLAP */

struct multibyte_coding_system
{
  /* A dynarr containing the charsets given in the `charsets' property when
     creating the coding system */
  Lisp_Object_dynarr *charsets;
#ifdef ALLOW_MULTIBYTE_CHARSET_OVERLAP
  int overlap; /* true if the ranges of the charsets overlap */
#endif /* ALLOW_MULTIBYTE_CHARSET_OVERLAP */
};

#define CODING_SYSTEM_MBCS_CHARSETS(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, multibyte)->charsets)
#define XCODING_SYSTEM_MBCS_CHARSETS(codesys) \
  CODING_SYSTEM_MBCS_CHARSETS (XCODING_SYSTEM (codesys))

#ifdef ALLOW_MULTIBYTE_CHARSET_OVERLAP
#define CODING_SYSTEM_MBCS_OVERLAP(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, multibyte)->overlap)
#define XCODING_SYSTEM_MBCS_OVERLAP(codesys) \
  CODING_SYSTEM_MBCS_OVERLAP (XCODING_SYSTEM (codesys))
#endif /* ALLOW_MULTIBYTE_CHARSET_OVERLAP */

struct multibyte_coding_stream
{
  /* Equivalent of dynarr in struct multibyte_coding_system as a
     precedence-array object.  We can't store the object in struct
     multibyte_coding_system because it can't currently be dumped. */
  Lisp_Object charset_precedence;
  /* CH holds a partially built-up character, or -1 for none.
     Used during decoding. */
  int ch;
};

static const struct memory_description multibyte_coding_system_description[] = {
  { XD_BLOCK_PTR, offsetof (struct multibyte_coding_system, charsets),
    1, { &Lisp_Object_dynarr_description} },
  { XD_END }
};

static const struct memory_description multibyte_coding_stream_description[] = {
  { XD_LISP_OBJECT, offsetof (struct multibyte_coding_stream,
			      charset_precedence), },
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (multibyte);

static void
multibyte_init_coding_stream (struct coding_stream *str)
{
  struct multibyte_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, multibyte);
  int i;
  Lisp_Object_dynarr *charsets = XCODING_SYSTEM_MBCS_CHARSETS (str->codesys);

  begin_precedence_array_generation ();
  data->charset_precedence = allocate_precedence_array ();
  for (i = 0; i < Dynarr_length (charsets); i++)
    {
      add_charset_to_precedence_array (Dynarr_at (charsets, i),
                                       data->charset_precedence);
    }
  data->ch = -1;
}

static void
multibyte_mark_coding_stream (struct coding_stream *str)
{
  struct multibyte_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, multibyte);
  mark_object (data->charset_precedence);
}

/* See if we can derive a character out of the specified charsets
   that is of the right dimension, is valid according to the bounds, and
   can be made into an Ichar. */

static Ichar
try_to_derive_character (int c1, int c2, int dimension,
			 Lisp_Object precarr)
{
  int i;
  Ichar ich = -1;
  Lisp_Object_dynarr *precdyn = XPRECEDENCE_ARRAY_DYNARR (precarr);

  for (i = 0; i < Dynarr_length (precdyn); i++)
    {
      Lisp_Object charset = Dynarr_at (precdyn, i);
      if (XCHARSET_DIMENSION (charset) == dimension &&
	  valid_charset_codepoint_p (charset, c1, c2))
	{
	  /* Try to convert directly.  Under Unicode-internal, this just does
	     the conversion.  Under old-Mule, this only works for encodable
	     charsets and only when the character is encoded using the
	     same charset. */
	  if ((ich = charset_codepoint_to_ichar (charset, c1, c2,
						 CONVERR_FAIL)) >= 0)
	    break;
#ifndef UNICODE_INTERNAL
	  {
	    int code;
	    /* Under old-Mule, the second clause handles selecting a
	       character that is encoded with a different charset from the
	       charset being matched but is Unicode-equivalent to the
	       codepoint being matched. */
#ifdef ALLOW_MULTIBYTE_CHARSET_OVERLAP
	    /* This should work properly.  We still prefer finding a charset
	       match over a Unicode-equivalent match, which is good.

	       Three scenarios:

	       (1) Charset list is ('vietnamese-viscii, 'ascii).
                   `vietnamese-viscii' has range [0, 255] and ASCII [0, 127].
                   For VISCII codepoint 5, we get the Unicode equivalent char
                   ?\u1eb4, not the exact-match ASCII char ?\^E.
               (2) Same charset list but holes in `vietnamese-viscii' for some
                   of the VISCII codepoints with the same mapping as ASCII at
                   that codepoint, and imagine `vietnamese-viscii' is
                   encodable.  Where a hole occurs, we still get a
                   character `vietnamese-viscii' with codepoint under old-Mule.
                   #### Not clear if this is the correct behavior.  Perhaps
                   this means we should reject a match if it has no
                   Unicode mapping.  If we reject, we fall through and the
                   ASCII character gets returned.  Where no hole occurs, we
                   get a `vietnamese-viscii' character, as expected.
               (3) Same scenario as two, but holes in high bytes where there
                   is no ASCII behind.  We either get a `vietnamese-viscii'
                   character with no corresponding Unicode mapping or no
                   character at all.
	    */
#endif
	    if ((code = charset_codepoint_to_unicode (charset, c1, c2,
						      CONVERR_FAIL)) >= 0 &&
		/* @@#### current-buffer dependency */
		(ich = buffer_unicode_to_ichar (code, current_buffer,
						CONVERR_FAIL)) >= 0)
	      break;
	  }
#endif /* not UNICODE_INTERNAL */
	}
    }

  return ich;
}

static Bytecount
multibyte_decode (struct coding_stream *str, const UExtbyte *src,
		  Bytecount n, unsigned_char_dynarr *dst)
{
  struct multibyte_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, multibyte);
  Lisp_Object precarr = data->charset_precedence;

  while (n--)
    {
      UExtbyte c = *src++;
      Ichar ich = -1;

      if (data->ch >= 0)
	{
	  /* See if we can derive a two-byte character out of the
	     specified charsets */
	  ich = try_to_derive_character (data->ch, c, 2, precarr);
	  if (ich >= 0)
	    {
	      Dynarr_add_ichar (dst, ich);
	      data->ch = -1;
	    }
	  else
	    {
	      /* If not, then the first byte was definitely erroneous,
		 but we might still be able to derive a character
		 starting with the second byte. */
	      DECODE_ERROR_OCTET (data->ch, dst);
	      data->ch = -1;
	      goto retry_one_byte;
	    }
	}
      else
	{
	retry_one_byte:
	  /* See if we can one-byte character out of the specified
	     charsets */
	  ich = try_to_derive_character (0, c, 1, precarr);
	  /* If not, retry as a two-byte character. */
	  if (ich < 0)
	    {
	      data->ch = c;
	      continue;
	    }

	  Dynarr_add_ichar (dst, ich);
	}
    }

  if (str->st.eof)
    {
      if (data->ch >= 0)
	{
	  /* We have a straggler. */
	  DECODE_ERROR_OCTET (data->ch, dst);
	  data->ch = -1;
	}
    }

  return src - str->src;
}

static Bytecount
multibyte_encode (struct coding_stream *str, const Ibyte *src,
		  Bytecount n, unsigned_char_dynarr *dst)
{
#ifdef UNICODE_INTERNAL
  struct multibyte_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, multibyte);
  Lisp_Object precarr = data->charset_precedence;
#endif
  const Ibyte *srcend = src + n;
  while (src < srcend)
    {
      Ichar ich = itext_ichar (src);
      Lisp_Object charset = Qnil;
      int c1, c2;

      INC_IBYTEPTR (src);

      if (handle_possible_error_octet (ich, str, src, dst, NULL))
	{
	  ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	}

      /* Logic here similar to the logic in try_to_derive_character().
	 Under Unicode-internal, just try directly to derive a
	 codepoint.  Under old-Mule, for each charset, first try to
	 derive a codepoint directly, then through Unicode unification.
      */
#ifdef UNICODE_INTERNAL
      ichar_to_charset_codepoint (ich, precarr, &charset,
				  &c1, &c2, CONVERR_FAIL);
#else
      {
	int i;
	Lisp_Object_dynarr *precdyn =
	  XCODING_SYSTEM_MBCS_CHARSETS (str->codesys);
	for (i = 0; i < Dynarr_length (precdyn); i++)
	  {
	    Lisp_Object charset2 = Dynarr_at (precdyn, i);
	    int code;

	    if (ichar_to_one_charset_codepoint (ich, charset2,
						&c1, &c2))
	      {
		charset = charset2;
		break;
	      }
	    code = ichar_to_unicode (ich, CONVERR_FAIL);
	    if (code >= 0)
	      {
		if (unicode_to_one_charset_codepoint (code, charset2,
						      &c1, &c2))
		  {
		    charset = charset2;
		    break;
		  }
	      }
	  }
      }
#endif /* not UNICODE_INTERNAL */

#ifdef ALLOW_MULTIBYTE_CHARSET_OVERLAP
      /* If we've found a conversion, but there is charset overlap
	 in this coding system, we need to convert the other way to
	 see whether we get the right result; otherwise, we don't have
	 a good conversion, because we need to have two-way
	 reversibility. */
      if (!NILP (charset) &&
	  XCODING_SYSTEM_MBCS_OVERLAP (str->codesys))
	{
	  Ichar other_way =
	    try_to_derive_character (c1, c2,
				     XCHARSET_DIMENSION (charset),
				     precarr);
	  /* Under Unicode-internal, the conversion the other way
	     needs to produce the same character.  Under old-Mule,
	     it's OK if the characters are different as long as they
	     unify under Unicode. */
#ifdef UNICODE_INTERNAL
	  if (ich != other_way)
	    charset = Qnil;
#else
	  if (ich != other_way)
	    {
	      int code = ichar_to_unicode (ich, CONVERR_FAIL);
	      int other_code = ichar_to_unicode (other_way,
						 CONVERR_FAIL);
	      if (!(code >= 0 && other_code >= 0 &&
		    code == other_code))
		charset = Qnil;
	    }
#endif /* (not) UNICODE_INTERNAL */
	}
#endif /* ALLOW_MULTIBYTE_CHARSET_OVERLAP */

      /* If no charset, substitute a ?, and return or continue. */
      if (NILP (charset))
	{
	  handle_standard_encoding_error (str, src, dst);
	  ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	}

      /* Finally, add the character */
      if (XCHARSET_DIMENSION (charset) == 2)
	Dynarr_add (dst, c1);
      Dynarr_add (dst, c2);
    }
  
  return src - str->src;
}

static Bytecount
multibyte_convert (struct coding_stream *str, const unsigned char *src,
		   Bytecount n, unsigned_char_dynarr *dst)
{
  if (str->direction == CODING_DECODE)
    return multibyte_decode (str, (UExtbyte *) src, n, dst);
  else
    return multibyte_encode (str, (Ibyte *) src, n, dst);
}

static void
multibyte_init (Lisp_Object codesys)
{
  XCODING_SYSTEM_MBCS_CHARSETS (codesys) = Dynarr_new (Lisp_Object);
}

static void
multibyte_mark (Lisp_Object codesys)
{
  mark_Lisp_Object_dynarr (XCODING_SYSTEM_MBCS_CHARSETS (codesys));
}

static void
multibyte_finalize (Lisp_Object cs)
{
  if (XCODING_SYSTEM_MBCS_CHARSETS (cs))
    {
      Dynarr_free (XCODING_SYSTEM_MBCS_CHARSETS (cs));
      XCODING_SYSTEM_MBCS_CHARSETS (cs) = 0;
    }
}

/* Return true if range [FROM1,TO1] overlaps range [FROM2,TO2], where all
   endpoints are inclusive. */

static int
ranges_overlap (int from1, int to1, int from2, int to2)
{
  return !(to1 < from2 || to2 < from1);
}

static int
charsets_overlap (Lisp_Object cseta, Lisp_Object csetb)
{
  int alo1, alo2, ahi1, ahi2;
  int blo1, blo2, bhi1, bhi2;
  get_charset_limits (cseta, &alo1, &alo2, &ahi1, &ahi2);
  get_charset_limits (csetb, &blo1, &blo2, &bhi1, &bhi2);

  /* If we have a mixed one-dimensional and two-dimensional charset list,
     then the first byte of the two-dimensional charset codepoint has to
     completely avoid the single byte of the one-dimensional charset
     codepoint or we have overlap.  So to make comparison possible we
     "promote" a one-d charset to a two-d charset by switching rows and
     columns to match the fact that the first octet of a two-d charset
     overlaps the one-d charset's octet, and extend the second octet to
     cover the entire row. */
     
  if (XCHARSET_DIMENSION (cseta) == 1)
    {
      assert (alo1 == 0);
      assert (ahi1 == 0);
      alo1 = alo2, ahi1 = ahi2, alo2 = 0, ahi2 = 255;
    }
  if (XCHARSET_DIMENSION (csetb) == 1)
    {
      assert (blo1 == 0);
      assert (bhi1 == 0);
      blo1 = blo2, bhi1 = bhi2, blo2 = 0, bhi2 = 255;
    }

  /* Two rectangles overlap when both dimensions overlap -- if there is
     no overlap in either dimension, the rectangle is off to the side of
     the other rectangle even if there is overlap in the other dimension.
     */

  return (ranges_overlap (alo1, ahi1, blo1, bhi1) &&
	  ranges_overlap (alo2, ahi2, blo2, bhi2));
}

static int
multibyte_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  if (EQ (key, Qcharsets))
    {
      Lisp_Object_dynarr *charsets = Dynarr_new (Lisp_Object);
      
      /* Now add set the new values to a new dynarr, so we don't overwrite
	 the old one before we're sure things are OK. */
      Dynarr_reset (charsets);
      {
	EXTERNAL_LIST_LOOP_2 (elt, value)
	  {
	    Lisp_Object charset = Fget_charset (elt);
	    int i;
	    /* Check for duplicated and overlapping charsets */
	    for (i = 0; i < Dynarr_length (charsets); i++)
	      {
		Lisp_Object ocharset = Dynarr_at (charsets, i);
		if (EQ (ocharset, charset))
		  invalid_argument ("Duplicated charset in `charsets' list",
				    charset);
		if (charsets_overlap (charset, ocharset))
		  {
#ifdef ALLOW_MULTIBYTE_CHARSET_OVERLAP
		    XCODING_SYSTEM_MBCS_OVERLAP (codesys) = 1;
#else
		    /* Specifying Qunbound as the first element of the list
		       indicates that we want to pass in two frobs, not
		       a single frob that's a list of two elements.
		       See build_error_data(). */
		    invalid_argument ("Charset overlaps with existing charset",
				      list3 (Qunbound, charset, ocharset));
#endif /* (not) ALLOW_MULTIBYTE_CHARSET_OVERLAP */
		  }
	      }
	    Dynarr_add (charsets, charset);
	  }
      }

      Dynarr_free (XCODING_SYSTEM_MBCS_CHARSETS (codesys));
      XCODING_SYSTEM_MBCS_CHARSETS (codesys) = charsets;
    }
  else
    return 0;
  return 1;
}

static Lisp_Object
multibyte_getprop (Lisp_Object codesys, Lisp_Object prop)
{
  if (EQ (prop, Qcharsets))
    {
      Lisp_Object_dynarr *charsets = XCODING_SYSTEM_MBCS_CHARSETS (codesys);
      Lisp_Object list = Qnil;
      int i;

      for (i = 0; i < Dynarr_length (charsets); i++)
        list = Fcons (Dynarr_at (charsets, i), list);
      return Fnreverse (list);
    }
  return Qunbound;
}

static void
multibyte_print (Lisp_Object codesys, Lisp_Object printcharfun,
		 int UNUSED (escapeflag))
{
  Lisp_Object_dynarr *charsets = XCODING_SYSTEM_MBCS_CHARSETS (codesys);
  int i;

  for (i = 0; i < Dynarr_length (charsets); i++)
    write_fmt_string_lisp (printcharfun, i == 0 ? "(%s" : " %s", 1,
                           XCHARSET_NAME (Dynarr_at (charsets, i)));
  write_ascstring (printcharfun, ")");
}

/* @@#### Need MBCS detector; but probably need to redo the whole detection
   system to accommodate this properly */


/************************************************************************/
/*                          Shift-JIS methods                           */
/************************************************************************/

struct shift_jis_coding_system
{
  int dummy;
};

struct shift_jis_coding_stream
{
  /* CH holds a partially built-up character, or -1 for none. */
  int ch;
};

static const struct memory_description shift_jis_coding_system_description[] = {
  { XD_END }
};

static const struct memory_description shift_jis_coding_stream_description[] = {
  { XD_END }
};

/* Shift-JIS; Hankaku (half-width) KANA is also supported. */
DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (shift_jis);

/* Shift-JIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of JISX0201-Kana (DIMENSION1_CHARS94 character set) is
   encoded by "position-code + 0x80".  A character of JISX0208
   (DIMENSION2_CHARS94 character set) is encoded in 2-byte but two
   position-codes are divided and shifted so that it fits in the range
   below.

   --- CODE RANGE of Shift-JIS ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   JISX0201-Kana	0xA0 .. 0xDF
   JISX0208 (1st byte)	0x80 .. 0x9F and 0xE0 .. 0xEF
	    (2nd byte)	0x40 .. 0x7E and 0x80 .. 0xFC
   -------------------------------

*/

/* Is this the first byte of a Shift-JIS two-byte char? */

inline static int
byte_shift_jis_two_byte_1_p (int c)
{
  return (c >= 0x81 && c <= 0x9F) || (c >= 0xE0 && c <= 0xEF);
}

/* Is this the second byte of a Shift-JIS two-byte char? */

inline static int
byte_shift_jis_two_byte_2_p (int c)
{
  return (c >= 0x40 && c <= 0x7E) || (c >= 0x80 && c <= 0xFC);
}

inline static int
byte_shift_jis_katakana_p (int c)
{
  return c >= 0xA1 && c <= 0xDF;
}

/* Convert Shift-JIS data to internal format. */

static Bytecount
shift_jis_decode (struct coding_stream *str, const UExtbyte *src,
		  Bytecount n, unsigned_char_dynarr *dst)
{
  struct shift_jis_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, shift_jis);
  Bytecount orign = n;

  while (n--)
    {
      UExtbyte c = *src++;

      if (data->ch >= 0)
	{
	  /* Previous character was first byte of Shift-JIS Kanji char. */
	  if (byte_shift_jis_two_byte_2_p (c))
	    {
	      int e1, e2;

	      DECODE_SHIFT_JIS (data->ch, c, e1, e2);
	      non_ascii_charset_codepoint_to_dynarr
		(Vcharset_japanese_jisx0208, e1, e2, dst,
		 CONVERR_USE_PRIVATE);
	    }
	  else
	    {
	      DECODE_ERROR_OCTET (data->ch, dst);
	      DECODE_ERROR_OCTET (c, dst);
	    }
	  data->ch = -1;
	}
      else
	{
	  if (byte_shift_jis_two_byte_1_p (c))
	    data->ch = c;
	  else if (byte_shift_jis_katakana_p (c))
	    non_ascii_charset_codepoint_to_dynarr
	      (Vcharset_katakana_jisx0201, 0, c, dst,
	       CONVERR_USE_PRIVATE);
	  else if (byte_ascii_p (c))
	    DECODE_ADD_BINARY_CHAR (c, dst);
	  else
	    DECODE_ERROR_OCTET (c, dst);
	}
    }

  DECODE_OUTPUT_PARTIAL_CHAR (str, data, dst);

  return orign;
}

static Bytecount
shift_jis_encode (struct coding_stream *str, const Ibyte *src,
		  Bytecount n, unsigned_char_dynarr *dst)
{
  const Ibyte *srcend = src + n;
  while (src < srcend)
    {
      Ibyte c = *src;
      if (byte_ascii_p (c))
	{
	  Dynarr_add (dst, c);
	  src++;
	}
      else
	{
	  Lisp_Object charset;
	  int c1, c2;
	  Ichar ich = itext_ichar (src);
	  INC_IBYTEPTR (src);
	  if (handle_possible_error_octet (ich, str, src, dst, NULL))
	    {
	      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	    }
	  ichar_to_charset_codepoint
	    (ich, Vshift_jis_precedence, &charset, &c1, &c2,
	     CONVERR_FAIL);
	  if (EQ (charset, Vcharset_katakana_jisx0201))
	    Dynarr_add (dst, c2);
	  else if (EQ (charset, Vcharset_japanese_jisx0208) ||
		   EQ (charset, Vcharset_japanese_jisx0208_1978))
	    {
	      UExtbyte j1, j2;
	      ENCODE_SHIFT_JIS (c1, c2, j1, j2);
	      Dynarr_add (dst, j1);
	      Dynarr_add (dst, j2);
	    }
	  else
	    {
	      handle_standard_encoding_error (str, src, dst);
	      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	    }
	}
    }

  return src - str->src;
}

static Bytecount
shift_jis_convert (struct coding_stream *str, const unsigned char *src,
		   Bytecount n, unsigned_char_dynarr *dst)
{
  if (str->direction == CODING_DECODE)
    return shift_jis_decode (str, (UExtbyte *) src, n, dst);
  else
    return shift_jis_encode (str, (Ibyte *) src, n, dst);
}

static void
shift_jis_init_coding_stream (struct coding_stream *str)
{
  struct shift_jis_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, shift_jis);
  data->ch = -1;
}

DEFUN ("decode-shift-jis-char", Fdecode_shift_jis_char, 1, 1, 0, /*
Decode a JISX0208 character of Shift-JIS coding-system.
CODE is the character code in Shift-JIS as a cons of type bytes.
Return the corresponding character, or nil if no character can be found.
*/
       (code))
{
  int c1, c2, s1, s2;

  CHECK_CONS (code);
  CHECK_FIXNUM (XCAR (code));
  CHECK_FIXNUM (XCDR (code));
  s1 = XFIXNUM (XCAR (code));
  s2 = XFIXNUM (XCDR (code));
  if (byte_shift_jis_two_byte_1_p (s1) &&
      byte_shift_jis_two_byte_2_p (s2))
    {
      Ichar ch;
      DECODE_SHIFT_JIS (s1, s2, c1, c2);
      ch = charset_codepoint_to_ichar
	(Vcharset_japanese_jisx0208, c1, c2, CONVERR_FAIL);
      if (ch >= 0)
	return make_char (ch);
      else
	return Qnil;
    }
  else
    return Qnil;
}

DEFUN ("encode-shift-jis-char", Fencode_shift_jis_char, 1, 1, 0, /*
Encode a JISX0208 character CHARACTER to SHIFT-JIS coding-system.
Return the corresponding character code in SHIFT-JIS as a cons of two bytes.
*/
       (character))
{
  Lisp_Object charset;
  int c1, c2, s1, s2;

  CHECK_CHAR_COERCE_INT (character);
  ichar_to_charset_codepoint (XCHAR (character), Vshift_jis_precedence,
			      &charset, &c1, &c2, CONVERR_FAIL);
  if (EQ (charset, Vcharset_japanese_jisx0208) ||
      EQ (charset, Vcharset_japanese_jisx0208_1978))
    {
      ENCODE_SHIFT_JIS (c1, c2, s1, s2);
      return Fcons (make_fixnum (s1), make_fixnum (s2));
    }
  else
    return Qnil;
}


/************************************************************************/
/*                          Shift-JIS detector                          */
/************************************************************************/

DEFINE_DETECTOR (shift_jis);
DEFINE_DETECTOR_CATEGORY (shift_jis, shift_jis);

struct shift_jis_detector
{
  int seen_jisx0208_char_in_c1;
  int seen_jisx0208_char_in_upper;
  int seen_jisx0201_char;
  unsigned int seen_iso2022_esc:1;
  unsigned int seen_bad_first_byte:1;
  unsigned int seen_bad_second_byte:1;
  /* temporary */
  unsigned int in_second_byte:1;
  unsigned int first_byte_was_c1:1;
};

static void
shift_jis_detect (struct detection_state *st, const UExtbyte *src,
		  Bytecount n)
{
  struct shift_jis_detector *data = DETECTION_STATE_DATA (st, shift_jis);

  while (n--)
    {
      UExtbyte c = *src++;
      if (!data->in_second_byte)
	{
	  if (c >= 0x80 && c <= 0x9F)
	    data->first_byte_was_c1 = 1;
	  if (c >= 0xA0 && c <= 0xDF)
	    data->seen_jisx0201_char++;
	  else if ((c >= 0x80 && c <= 0x9F) || (c >= 0xE0 && c <= 0xEF))
	    data->in_second_byte = 1;
	  else if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	    data->seen_iso2022_esc = 1;
	  else if (c >= 0x80)
	    data->seen_bad_first_byte = 1;
	}
      else
	{
	  if ((c >= 0x40 && c <= 0x7E) || (c >= 0x80 && c <= 0xFC))
	    {
	      if (data->first_byte_was_c1 || (c >= 0x80 && c <= 0x9F))
		data->seen_jisx0208_char_in_c1++;
	      else
		data->seen_jisx0208_char_in_upper++;
	    }
	  else
	    data->seen_bad_second_byte = 1;
	  data->in_second_byte = 0;
	  data->first_byte_was_c1 = 0;
	}
    }

  if (data->seen_bad_second_byte)
    DET_RESULT (st, shift_jis) = DET_NEARLY_IMPOSSIBLE;
  else if (data->seen_bad_first_byte)
    DET_RESULT (st, shift_jis) = DET_QUITE_IMPROBABLE;
  else if (data->seen_iso2022_esc)
    DET_RESULT (st, shift_jis) = DET_SOMEWHAT_UNLIKELY;
  else if (data->seen_jisx0208_char_in_c1 >= 20 ||
	   (data->seen_jisx0208_char_in_c1 >= 10 &&
	    data->seen_jisx0208_char_in_upper >= 10))
    DET_RESULT (st, shift_jis) = DET_QUITE_PROBABLE;
  else if (data->seen_jisx0208_char_in_c1 > 3 ||
	   data->seen_jisx0208_char_in_upper >= 10 ||
	   /* Since the range is limited compared to what is often seen
	      is typical Latin-X charsets, the fact that we've seen a
	      bunch of them and none that are invalid is reasonably
	      strong statistical evidence of this encoding, or at least
	      not of the common Latin-X ones. */
	   data->seen_jisx0201_char >= 100)
    DET_RESULT (st, shift_jis) = DET_SOMEWHAT_LIKELY;
  else if (data->seen_jisx0208_char_in_c1 > 0 ||
	   data->seen_jisx0208_char_in_upper > 0 ||
	   data->seen_jisx0201_char > 0)
    DET_RESULT (st, shift_jis) = DET_SLIGHTLY_LIKELY;
  else
    DET_RESULT (st, shift_jis) = DET_AS_LIKELY_AS_UNLIKELY;
}


/************************************************************************/
/*                            Big5 methods                              */
/************************************************************************/

struct big5_coding_system
{
  int dummy;
};

struct big5_coding_stream
{
  /* CH holds a partially built-up character, or -1 for none. */
  int ch;
};

static const struct memory_description big5_coding_system_description[] = {
  { XD_END }
};

static const struct memory_description big5_coding_stream_description[] = {
  { XD_END }
};

/* BIG5 (used for Mandarin in Taiwan). */
DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (big5);

#ifndef UNICODE_INTERNAL

/* BIG5 is a coding system encoding two character sets: ASCII and
   Big5.  An ASCII character is encoded as is.  Big5 is a two-byte
   character set and is encoded in two-byte.

   --- CODE RANGE of BIG5 ---
   (character set)	(range)
   ASCII		0x00 .. 0x7F
   Big5 (1st byte)	0xA1 .. 0xFE
	(2nd byte)	0x40 .. 0x7E and 0xA1 .. 0xFE
   --------------------------

   Since the number of characters in Big5 is larger than maximum
   characters in Emacs' charset (96x96), it can't be handled as one
   charset.  So, in XEmacs, Big5 is divided into two: `charset-big5-1'
   and `charset-big5-2'.  Both <type>s are DIMENSION2_CHARS94.  The former
   contains frequently used characters and the latter contains less
   frequently used characters.  */

/* Number of Big5 characters which have the same code in 1st byte.  */

#define BIG5_SAME_ROW (0xFF - 0xA1 + 0x7F - 0x40)

/* Code conversion macros.  These are macros because they are used in
   inner loops during code conversion.

   Note that temporary variables in macros introduce the classic
   dynamic-scoping problems with variable names.  We use capital-
   lettered variables in the assumption that XEmacs does not use
   capital letters in variables except in a very formalized way
   (e.g. Qstring). */

/* Convert Big5 code (b1, b2) into a charset codepoint (c1, c2) in the
   pseudo-charsets `chinese-big5-1' or `chinese-big5-2'. */

/* There is a much simpler way to split the Big5 charset into two.
   For the moment I'm going to leave the algorithm as-is because it
   claims to separate out the most-used characters into a single
   charset, which perhaps will lead to optimizations in various
   places.

   The way the algorithm works is something like this:

   Big5 can be viewed as a 94x157 charset, where the row is
   encoded into the bytes 0xA1 .. 0xFE and the column is encoded
   into the bytes 0x40 .. 0x7E and 0xA1 .. 0xFE.  As for frequency,
   the split between low and high column numbers is apparently
   meaningless; ascending rows produce less and less frequent chars.
   Therefore, we assign the lower half of rows (0xA1 .. 0xC8) to
   the first charset, and the upper half (0xC9 .. 0xFE) to the
   second.  To do the conversion, we convert the character into
   a single number where 0 .. 156 is the first row, 157 .. 313
   is the second, etc.  That way, the characters are ordered by
   decreasing frequency.  Then we just chop the space in two
   and coerce the result into a 94x94 space.
   */

#define DECODE_BIG5(b1, b2, charset, c1, c2) do				\
{									\
  int B1 = b1, B2 = b2;							\
  int I									\
    = (B1 - 0xA1) * BIG5_SAME_ROW + B2 - (B2 < 0x7F ? 0x40 : 0x62);	\
									\
  if (B1 < 0xC9)							\
    {									\
      charset = Vcharset_chinese_big5_1;				\
    }									\
  else									\
    {									\
      charset = Vcharset_chinese_big5_2;				\
      I -= (BIG5_SAME_ROW) * (0xC9 - 0xA1);				\
    }									\
  c1 = I / (0xFF - 0xA1) + 0x21;					\
  c2 = I % (0xFF - 0xA1) + 0x21;					\
} while (0)

/* Convert charset codepoint in the pseudo-charsets `chinese-big5-1' and
   `chinese-big5-2' (c1, c2) into Big5 code (b1, b2). */

#define ENCODE_BIG5(charset, c1, c2, b1, b2) do				\
{									\
  int I = ((c1) - 0x21) * (0xFF - 0xA1) + ((c2) - 0x21);		\
									\
  if (EQ (charset, Vcharset_chinese_big5_2))				\
    {									\
      I += BIG5_SAME_ROW * (0xC9 - 0xA1);				\
    }									\
  b1 = I / BIG5_SAME_ROW + 0xA1;					\
  b2 = I % BIG5_SAME_ROW;						\
  b2 += b2 < 0x3F ? 0x40 : 0x62;					\
} while (0)

#endif /* not UNICODE_INTERNAL */

inline static int
byte_big5_two_byte_1_p (int c)
{
  return c >= 0xA1 && c <= 0xFE;
}

/* Is this the second byte of a Shift-JIS two-byte char? */

inline static int
byte_big5_two_byte_2_p (int c)
{
  return (c >= 0x40 && c <= 0x7E) || (c >= 0xA1 && c <= 0xFE);
}

/* Convert Big5 data to internal format. */

static Bytecount
big5_decode (struct coding_stream *str, const UExtbyte *src,
	     Bytecount n, unsigned_char_dynarr *dst)
{
  struct big5_coding_stream *data = CODING_STREAM_TYPE_DATA (str, big5);
  Bytecount orign = n;

  while (n--)
    {
      UExtbyte c = *src++;
      if (data->ch >= 0)
	{
	  /* Previous character was first byte of Big5 char. */
	  if (byte_big5_two_byte_2_p (c))
	    {
#ifdef UNICODE_INTERNAL
	      non_ascii_charset_codepoint_to_dynarr
		(Vcharset_chinese_big5, data->ch, c, dst,
		 CONVERR_USE_PRIVATE);
#else /* not UNICODE_INTERNAL */
	      Lisp_Object charset;
	      int b1, b2;
	      DECODE_BIG5 (data->ch, c, charset, b1, b2);
	      non_ascii_charset_codepoint_to_dynarr
		(charset, b1, b2, dst, CONVERR_USE_PRIVATE);
#endif /* UNICODE_INTERNAL */
	    }
	  else
	    {
	      DECODE_ERROR_OCTET (data->ch, dst);
	      DECODE_ERROR_OCTET (c, dst);
	    }
	  data->ch = -1;
	}
      else
	{
	  if (byte_big5_two_byte_1_p (c))
	    data->ch = c;
	  else if (byte_ascii_p (c))
	    DECODE_ADD_BINARY_CHAR (c, dst);
	  else
	    DECODE_ERROR_OCTET (c, dst);
	}
    }

  DECODE_OUTPUT_PARTIAL_CHAR (str, data, dst);

  return orign;
}

static Bytecount
big5_encode (struct coding_stream *str, const Ibyte *src,
	     Bytecount n, unsigned_char_dynarr *dst)
{
  const Ibyte *srcend = src + n;
  while (src < srcend)
    {
      Ibyte c = *src;
      if (byte_ascii_p (c))
	{
	  Dynarr_add (dst, c);
	  src++;
	}
      else
	{
	  Lisp_Object charset;
	  int c1, c2;
	  Ichar ich = itext_ichar (src);
	  INC_IBYTEPTR (src);
	  if (handle_possible_error_octet (ich, str, src, dst, NULL))
	    {
	      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	    }
	  ichar_to_charset_codepoint
	    (ich, Vbig5_precedence, &charset, &c1, &c2, CONVERR_FAIL);
#ifdef UNICODE_INTERNAL
	  if (EQ (charset, Vcharset_chinese_big5))
	    {
	      Dynarr_add (dst, c1);
	      Dynarr_add (dst, c2);
	    }
#else /* not UNICODE_INTERNAL */
	  if (EQ (charset, Vcharset_chinese_big5_1) ||
	      EQ (charset, Vcharset_chinese_big5_2))
	    {
	      UExtbyte b1, b2;
	      ENCODE_BIG5 (charset, c1, c2, b1, b2);
	      Dynarr_add (dst, b1);
	      Dynarr_add (dst, b2);
	    }
#endif /* UNICODE_INTERNAL */
	  else
	    {
	      handle_standard_encoding_error (str, src, dst);
	      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	    }
	}
    }

  return src - str->src;
}

static Bytecount
big5_convert (struct coding_stream *str, const unsigned char *src,
	      Bytecount n, unsigned_char_dynarr *dst)
{
  if (str->direction == CODING_DECODE)
    return big5_decode (str, (UExtbyte *) src, n, dst);
  else
    return big5_encode (str, (Ibyte *) src, n, dst);
}

static void
big5_init_coding_stream (struct coding_stream *str)
{
  struct big5_coding_stream *data = CODING_STREAM_TYPE_DATA (str, big5);
  data->ch = -1;
}

static Ichar
decode_big5_char (int b1, int b2)
{
#ifdef UNICODE_INTERNAL
  return charset_codepoint_to_ichar (Vcharset_chinese_big5, b1, b2,
				     CONVERR_FAIL);
#else /* not UNICODE_INTERNAL */
  if (byte_big5_two_byte_1_p (b1) &&
      byte_big5_two_byte_2_p (b2))
    {
      Lisp_Object charset;
      int c1, c2;

      DECODE_BIG5 (b1, b2, charset, c1, c2);
      return charset_codepoint_to_ichar (charset, c1, c2, CONVERR_FAIL);
    }
  else
    return -1;
#endif /* UNICODE_INTERNAL */
}

#ifndef UNICODE_INTERNAL

void
big5_char_to_fake_codepoint (int b1, int b2, Lisp_Object *charset, int *c1,
			     int *c2)
{
  if (byte_big5_two_byte_1_p (b1) &&
      byte_big5_two_byte_2_p (b2))
    {
      DECODE_BIG5 (b1, b2, *charset, *c1, *c2);
    }
  else
    {
      *charset = Qnil;
      *c1 = 0;
      *c2 = 0;
    }
}

#endif /* not UNICODE_INTERNAL */

DEFUN ("decode-big5-char", Fdecode_big5_char, 1, 1, 0, /*
Convert Big Five character codes in CODE into a character.
CODE is a cons of two integers specifying the codepoints in Big Five.
Return the corresponding character, or nil if the codepoints are out of range.

The term `decode' is used because the codepoints can be viewed as the
representation of the character in the external Big Five encoding, and thus
converting them to a character is analogous to any other operation that
decodes an external representation.
*/
       (code))
{
  Ichar ch;

  CHECK_CONS (code);
  CHECK_FIXNUM (XCAR (code));
  CHECK_FIXNUM (XCDR (code));
  ch = decode_big5_char (XFIXNUM (XCAR (code)), XFIXNUM (XCDR (code)));
  if (ch < 0)
    return Qnil;
  else
    return make_char (ch);
}

DEFUN ("encode-big5-char", Fencode_big5_char, 1, 1, 0, /*
Convert the specified Big Five character into its codepoints.
The codepoints are returned as a cons of two integers, specifying the
Big Five codepoints.  See `decode-big5-char' for the reason why the
term `encode' is used for this operation.
*/
       (character))
{
  Lisp_Object charset;
  int c1, c2;

  CHECK_CHAR_COERCE_INT (character);
  ichar_to_charset_codepoint (XCHAR (character), Vbig5_precedence,
			      &charset, &c1, &c2, CONVERR_FAIL);
#ifdef UNICODE_INTERNAL
  if (EQ (charset, Vcharset_chinese_big5))
    {
      return Fcons (make_fixnum (c1), make_fixnum (c2));
    }
#else /* not UNICODE_INTERNAL */
  if (EQ (charset, Vcharset_chinese_big5_1) ||
      EQ (charset, Vcharset_chinese_big5_2))
    {
      int b1, b2;
      ENCODE_BIG5 (charset, c1, c2, b1, b2);
      return Fcons (make_fixnum (b1), make_fixnum (b2));
    }
#endif /* UNICODE_INTERNAL */
  else
    return Qnil;
}


/************************************************************************/
/*                            Big5 detector                             */
/************************************************************************/

DEFINE_DETECTOR (big5);
DEFINE_DETECTOR_CATEGORY (big5, big5);

struct big5_detector
{
  int seen_big5_char;
  int seen_euc_char;
  unsigned int seen_iso2022_esc:1;
  unsigned int seen_bad_first_byte:1;
  unsigned int seen_bad_second_byte:1;

  /* temporary */
  unsigned int in_second_byte:1;
};

static void
big5_detect (struct detection_state *st, const UExtbyte *src,
	     Bytecount n)
{
  struct big5_detector *data = DETECTION_STATE_DATA (st, big5);

  while (n--)
    {
      UExtbyte c = *src++;
      if (!data->in_second_byte)
	{
	  if (c >= 0xA1 && c <= 0xFE)
	    data->in_second_byte = 1;
	  else if (c == ISO_CODE_ESC || c == ISO_CODE_SI || c == ISO_CODE_SO)
	    data->seen_iso2022_esc = 1;
	  else if (c >= 0x80)
	    data->seen_bad_first_byte = 1;
	}
      else
	{
	  data->in_second_byte = 0;
	  if (c >= 0xA1 && c <= 0xFE)
	    data->seen_euc_char++;
	  else if (c >= 0x40 && c <= 0x7E)
	    data->seen_big5_char++;
	  else
	    data->seen_bad_second_byte = 1;
	}
    }

  if (data->seen_bad_second_byte)
    DET_RESULT (st, big5) = DET_NEARLY_IMPOSSIBLE;
  else if (data->seen_bad_first_byte)
    DET_RESULT (st, big5) = DET_QUITE_IMPROBABLE;
  else if (data->seen_iso2022_esc)
    DET_RESULT (st, big5) = DET_SOMEWHAT_UNLIKELY;
  else if (data->seen_big5_char >= 4)
    DET_RESULT (st, big5) = DET_SOMEWHAT_LIKELY;
  else if (data->seen_euc_char)
    DET_RESULT (st, big5) = DET_SLIGHTLY_LIKELY;
  else
    DET_RESULT (st, big5) = DET_AS_LIKELY_AS_UNLIKELY;
}


/************************************************************************/
/*                           ISO2022 methods                            */
/************************************************************************/

/* Any ISO-2022-compliant coding system.  Includes JIS, EUC, CTEXT
   (Compound Text, the encoding of selections in X Windows).  See below for
   a complete description of ISO-2022. */

/* Flags indicating what we've seen so far when parsing an
   ISO2022 escape sequence. */
enum iso_esc_flag
{
  /* Partial sequences */
  ISO_ESC_NOTHING,	/* Nothing has been seen. */
  ISO_ESC,		/* We've seen ESC. */
  ISO_ESC_2_4,		/* We've seen ESC $.  This indicates
			   that we're designating a multi-byte, rather
			   than a single-byte, character set. */
  ISO_ESC_2_5,		/* We've seen ESC %.  This indicates the beginning
			   of an extended segment, particularly a
			   Unicode coding system; [[ the only one of these
			   we're prepared to deal with is UTF-8, which has
			   the next character as G.]] @@#### FIXME, I (ben)
                           previously implemented a different extension
                           as can be seen below.  Combine the two. */
  ISO_ESC_2_5_2F,	/* We've seen ESC % /. */
  ISO_ESC_2_5_2F_30,	/* We've seen ESC % / [01234]. */
  ISO_ESC_2_5_2F_30_M,	/* We've seen ESC % / [01234] size1. */
  ISO_ESC_2_5_2F_30_M_L,/* We've seen ESC % / [01234] size1 size2. */
  ISO_ESC_2_8,		/* We've seen ESC 0x28, i.e. 'ESC ('.
			   This means designate a 94-character
			   character set into G0. */
  ISO_ESC_2_9,		/* We've seen ESC 0x29, i.e. 'ESC )' -- designate a
			   94-character character set into G1. */
  ISO_ESC_2_10,		/* We've seen ESC 0x2A, i.e. 'ESC *'. */
  ISO_ESC_2_11,		/* We've seen ESC 0x2B, i.e. 'ESC +'. */
  ISO_ESC_2_12,		/* We've seen ESC 0x2C, i.e. 'ESC ,' -- designate a
			   96-character character set into G0.
			   (This is not ISO2022-standard.
			   The following 96-character
			   control sequences are standard,
			   though.) */
  ISO_ESC_2_13,		/* We've seen ESC 0x2D, i.e. 'ESC -' -- designate a
			   96-character character set into G1.
			   */
  ISO_ESC_2_14,		/* We've seen ESC 0x2E, i.e. 'ESC .'. */
  ISO_ESC_2_15,		/* We've seen ESC 0x2F, i.e. 'ESC /'. */
  ISO_ESC_2_4_8,	/* We've seen ESC $ 0x28 -- designate
			   a 94^N character set into G0. */
  ISO_ESC_2_4_9,	/* We've seen ESC $ 0x29, i.e. 'ESC $ )'. */
  ISO_ESC_2_4_10,	/* We've seen ESC $ 0x2A, i.e. 'ESC $ *'. */
  ISO_ESC_2_4_11,	/* We've seen ESC $ 0x2B, i.e. 'ESC $ +'. */
  ISO_ESC_2_4_12,	/* We've seen ESC $ 0x2C, i.e. 'ESC $ ,'. */
  ISO_ESC_2_4_13,	/* We've seen ESC $ 0x2D, i.e. 'ESC $ -'. */
  ISO_ESC_2_4_14,	/* We've seen ESC $ 0x2E, i.e. 'ESC $ .'. */
  ISO_ESC_2_4_15,	/* We've seen ESC $ 0x2F, i.e. 'ESC $ /'. */
  ISO_ESC_5_11,		/* We've seen ESC [ or 0x9B.  This
			   starts a directionality-control
			   sequence.  The next character
			   must be 0, 1, 2, or ]. */
  ISO_ESC_5_11_0,	/* We've seen 0x9B 0.  The next character must be ]. */
  ISO_ESC_5_11_1,	/* We've seen 0x9B 1.  The next character must be ]. */
  ISO_ESC_5_11_2,	/* We've seen 0x9B 2.  The next character must be ]. */

  /* Full sequences. */
  ISO_ESC_START_COMPOSITE, /* Private usage for START COMPOSING */
  ISO_ESC_END_COMPOSITE,   /* Private usage for END COMPOSING */
  ISO_ESC_SINGLE_SHIFT, /* We've seen a complete single-shift sequence. */
  ISO_ESC_LOCKING_SHIFT,/* We've seen a complete locking-shift sequence. */
  ISO_ESC_DESIGNATE,	/* We've seen a complete designation sequence. */
  ISO_ESC_DIRECTIONALITY,/* We've seen a complete ISO6429 directionality
			   sequence. */
  ISO_ESC_LITERAL	/* We've seen a literal character ala
			   escape-quoting. */
};

enum iso_error
{
  ISO_ERROR_BAD_FINAL,
  ISO_ERROR_UNKWOWN_ESC_SEQUENCE,
  ISO_ERROR_INVALID_CODE_POINT_CHARACTER,
};


/* Flags indicating current state while converting code. */

/************ Used during encoding and decoding: ************/
/* If set, the current directionality is right-to-left.  Otherwise, it's
   left-to-right. */
#define ISO_STATE_R2L		(1 << 0)

/************ Used during encoding: ************/
/* If set, we just saw a CR. */
#define ISO_STATE_CR		(1 << 1)

/************ Used during decoding: ************/
/* If set, we're currently parsing an escape sequence and the upper 16 bits
   should be looked at to indicate what partial escape sequence we've seen
   so far.  Otherwise, we're running through actual text. */
#define ISO_STATE_ESCAPE	(1 << 2)
/* If set, G2 is invoked into GL, but only for the next character. */
#define ISO_STATE_SS2		(1 << 3)
/* If set, G3 is invoked into GL, but only for the next character.  If both
   ISO_STATE_SS2 and ISO_STATE_SS3 are set, ISO_STATE_SS2 overrides; but
   this probably indicates an error in the text encoding. */
#define ISO_STATE_SS3		(1 << 4)
/* If set, we're currently processing a composite character (i.e. a
   character constructed by overstriking two or more characters). */
#define ISO_STATE_COMPOSITE	(1 << 5)
/* If set, we're processing UTF-8 encoded data within ISO-2022
   processing. */
#define ISO_STATE_UTF_8		(1 << 6)
/* If set, we're processing an X extended segment. */
#define ISO_STATE_X_EXTENDED	(1 << 7)


/* ISO_STATE_LOCK is the mask of flags that remain on until explicitly
   turned off when in the ISO2022 encoder/decoder.  Other flags are turned
   off at the end of processing each character or escape sequence. */
# define ISO_STATE_LOCK \
  (ISO_STATE_COMPOSITE | ISO_STATE_R2L | ISO_STATE_UTF_8 | \
   ISO_STATE_X_EXTENDED)

typedef struct charset_conversion_spec
{
  Lisp_Object from_charset;
  Lisp_Object to_charset;
} charset_conversion_spec;

typedef struct
{
  Dynarr_declare (charset_conversion_spec);
} charset_conversion_spec_dynarr;

struct iso2022_coding_system
{
  /* What are the charsets to be initially designated to G0, G1,
     G2, G3?  If t, no charset is initially designated.  If nil,
     no charset is initially designated and no charset is allowed
     to be designated. */
  Lisp_Object initial_charset[4];

  /* If true, a designation escape sequence needs to be sent on output
     for the charset in G[0-3] before that charset is used. */
  Boolbyte force_charset_on_output[4];

  charset_conversion_spec_dynarr *input_conv;
  charset_conversion_spec_dynarr *output_conv;

  unsigned int shoort		:1; /* C makes you speak Dutch */
  unsigned int no_ascii_eol	:1;
  unsigned int no_ascii_cntl	:1;
  unsigned int seven		:1;
  unsigned int lock_shift	:1;
  unsigned int no_iso6429	:1;
  unsigned int escape_quoted	:1;
  unsigned int iso2022_preserve :1;
};

#define CODING_SYSTEM_ISO2022_INITIAL_CHARSET(codesys, g) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->initial_charset[g])
#define CODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT(codesys, g) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->force_charset_on_output[g])
#define CODING_SYSTEM_ISO2022_SHORT(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->shoort)
#define CODING_SYSTEM_ISO2022_NO_ASCII_EOL(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->no_ascii_eol)
#define CODING_SYSTEM_ISO2022_NO_ASCII_CNTL(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->no_ascii_cntl)
#define CODING_SYSTEM_ISO2022_SEVEN(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->seven)
#define CODING_SYSTEM_ISO2022_LOCK_SHIFT(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->lock_shift)
#define CODING_SYSTEM_ISO2022_NO_ISO6429(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->no_iso6429)
#define CODING_SYSTEM_ISO2022_ESCAPE_QUOTED(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->escape_quoted)
#define CODING_SYSTEM_ISO2022_ISO2022_PRESERVE(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->iso2022_preserve)
#define CODING_SYSTEM_ISO2022_INPUT_CONV(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->input_conv)
#define CODING_SYSTEM_ISO2022_OUTPUT_CONV(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, iso2022)->output_conv)

#define XCODING_SYSTEM_ISO2022_INITIAL_CHARSET(codesys, g) \
  CODING_SYSTEM_ISO2022_INITIAL_CHARSET (XCODING_SYSTEM (codesys), g)
#define XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT(codesys, g) \
  CODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (XCODING_SYSTEM (codesys), g)
#define XCODING_SYSTEM_ISO2022_SHORT(codesys) \
  CODING_SYSTEM_ISO2022_SHORT (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_NO_ASCII_EOL(codesys) \
  CODING_SYSTEM_ISO2022_NO_ASCII_EOL (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_NO_ASCII_CNTL(codesys) \
  CODING_SYSTEM_ISO2022_NO_ASCII_CNTL (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_SEVEN(codesys) \
  CODING_SYSTEM_ISO2022_SEVEN (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_LOCK_SHIFT(codesys) \
  CODING_SYSTEM_ISO2022_LOCK_SHIFT (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_NO_ISO6429(codesys) \
  CODING_SYSTEM_ISO2022_NO_ISO6429 (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED(codesys) \
  CODING_SYSTEM_ISO2022_ESCAPE_QUOTED (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_ISO2022_PRESERVE(codesys) \
  CODING_SYSTEM_ISO2022_ISO2022_PRESERVE (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_INPUT_CONV(codesys) \
  CODING_SYSTEM_ISO2022_INPUT_CONV (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_ISO2022_OUTPUT_CONV(codesys) \
  CODING_SYSTEM_ISO2022_OUTPUT_CONV (XCODING_SYSTEM (codesys))

/* Additional information used by the ISO2022 decoder and detector. */
struct iso2022_coding_stream
{
  /* CHARSET holds the character sets currently assigned to the G0
     through G3 variables.  It is initialized from the array
     INITIAL_CHARSET in CODESYS. */
  Lisp_Object charset[4];

  /* Which registers are currently invoked into the left (GL) and
     right (GR) halves of the 8-bit encoding space? */
  int register_left, register_right;

  /* FLAGS holds flags indicating the current state of the encoding.  Some of
     these flags are actually part of the state-dependent data and should be
     moved there. */
  unsigned int flags;

  /**************** for decoding ****************/
  
  /* ISO_ESC holds a value indicating part of an escape sequence
     that has already been seen. */
  enum iso_esc_flag esc;

  /* This records the bytes we've seen so far in an escape sequence,
     in case the sequence is invalid (we spit out the bytes unchanged). */
  unsigned char esc_bytes[8];

  /* Index for next byte to store in ISO escape sequence. */
  int esc_bytes_index;

#ifdef ENABLE_COMPOSITE_CHARS
  /* Stuff seen so far when composing a string. */
  unsigned_char_dynarr *composite_chars;
#endif

  /* Used for handling UTF-8. */
  struct unicode_coding_stream unicode;

  /* CH holds a partially built-up character, or -1 for none. */
  int ch;

  /**************** for encoding ****************/

  /* Whether we need to explicitly designate the charset in the
     G? register before using it.  It is initialized from the
     array FORCE_CHARSET_ON_OUTPUT in CODESYS. */
  Boolbyte force_charset_on_output[4];
};

static const struct memory_description ccs_description_1[] =
{
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, from_charset) },
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, to_charset) },
  { XD_END }
};

static const struct sized_memory_description ccs_description =
{
  sizeof (charset_conversion_spec),
  ccs_description_1
};

static const struct memory_description ccsd_description_1[] =
{
  XD_DYNARR_DESC (charset_conversion_spec_dynarr, &ccs_description),
  { XD_END }
};

static const struct sized_memory_description ccsd_description =
{
  sizeof (charset_conversion_spec_dynarr),
  ccsd_description_1
};

static const struct memory_description iso2022_coding_system_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (struct iso2022_coding_system, 
				    initial_charset), 4 },
  { XD_BLOCK_PTR, offsetof (struct iso2022_coding_system, input_conv),
    1, { &ccsd_description } },
  { XD_BLOCK_PTR, offsetof (struct iso2022_coding_system, output_conv),
    1, { &ccsd_description } },
  { XD_END }
};

static const struct memory_description iso2022_coding_stream_description[] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (struct iso2022_coding_stream, 
				    charset), 4 },
#ifdef ENABLE_COMPOSITE_CHARS
  { XD_BLOCK_PTR, offsetof (struct iso2022_coding_stream, composite_chars),
    1, { &unsigned_char_dynarr_description} },
#endif
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (iso2022);

/* NOTE NOTE NOTE:

   A full description of ISO-2022 is available through ECMA, where it is
   known as ECMA-35.  See

   http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-035.pdf

   In general, see

   http://www.ecma-international.org/publications/standards/Standard.htm

   ISO makes you pay for their bloody standards, but ECMA provides the same
   ones for free.

   See also

   http://www.iana.org/assignments/character-sets

   for a listing of lots of character sets and alternative names.  But this
   doesn't seem to list everything; the following does a better job in some
   cases:

   http://www.dataparksearch.org/dpsearch-international.en.html

   The registry of character sets and their associated final characters is
   in

   http://www.itscj.ipsj.or.jp/ISO-IR/

   Although this is extremely confusing and difficult to figure out.

   Here's some more info I dug up somewhere; this is partial:

   Values of ID for 94-character sets include: 
   
   B US-ASCII (= left half of ISO 8859 sets) 
   I right half of JIS X0201-1976 (katakana) 
   J left half of JIS X0201-1976 (JIS-Roman) 
   
   Values of ID for multi-byte 94-character sets are: 
   
   @ JIS C 6226-1978 
   A GB 2312-1980 
   B JIS X0208-1990 
   C KSC 5601-1987 
   D JIS X0212-1990 
   E GB 2312-1980 plus GB 8565-1989 
   G CNS 11643-1986 level 1 
   H CNS 11643-1986 level 2 
   I CNS 11643-1992 plane 3 
   J CNS 11643-1992 plane 4 
   K CNS 11643-1992 plane 5 
   L CNS 11643-1992 plane 6 
   M CNS 11643-1992 plane 7 
   
   Values of ID for 96-character sets include: 
   
   A right half of ISO 8859-1:1987 (ISO Latin-1) 
   B right half of ISO 8859-2:1987 (ISO Latin-2) 
   C right half of ISO 8859-3:1988 (ISO Latin-3) 
   D right half of ISO 8859-4:1988 (ISO Latin-4) 
   F right half of ISO 8859-7:1987 (ISO Latin-Greek) 
   G right half of ISO 8859-6:1988 (ISO Latin-Arabic) 
   H right half of ISO 8859-8:1988 (ISO Latin-Hebrew) 
   L right half of ISO 8859-5:1989 (ISO Latin-Cyrillic) 
   M right half of ISO 8859-9:1989 (ISO Latin-5) 
   T Thai character set TIS 620-2533:1990 
   V right half of ISO 8859-10:1992 (ISO Latin-6) 


   Here is something from an old 1993 email, which tries to give a complete
   listing of character sets and final bytes, but it's old:

 ------------------------------------------------------------------
 
 Character set types
   ( ): 94-char set w/ 3-char sequence
   (!): 94-char set w/ 4-char sequence
   (-): 96-char set
   ($): multiple-byte set
   (%): non-2022 system with standard return
   (/): non-2022 system without standard return
 
 ISO#      Sponsor Source(s)                     GL  Esc Description
       Size                                          HEX
                                       type --------*
    2    94 ISO   646:1983                           40  646-old
    4    94 GB    BS 4730                            41  646-GB
    6    94 US    ANSI X3.4:1968                     42  646-IRV (ASCII)
    8-1  94 SE    NATS                               43  news SE-FI
    8-2  14 SE    NATS                               44  news SE-FI extra
    9-1  94 SE    NATS                               45  news DK-NO
    9-2  14 SE    NATS                               46  news DK-NO extra
   10    94 SE    SEN 850200 B                       47  646-SE
   11    94 SE    SEN 850200 C                       48  646-SE names
   13    63 JP    JIS C 6220:1969, JIS X 0201    14  49  Japanese Katakana
   14    94 JP    JIS C 6220:1969, JIS X 0201        4A  646-JP
   15    94 ECMA  Olivetti                           59  646-IT
   16    94 ECMA  Olivetti                           4C  646-PT
   17    94 ECMA  Olivetti                           5A  646-ES
   18    94 ECMA  Olivetti                           5B  Greek
   19    94 ECMA  Olivetti                           5C  Latin+Greek
   21    94 DE    DIN 66003                          4B  646-DE
   27    94 ECMA  Honeywell-Bull                     55  Latin+Greek
   31    82 ISO   5428                               58  Greek biblio.
   37    94 ISO   5427                               4E  Cyrillic
   38    77 DE    DIN 31624                          4F  biblio.
   39    59 DE    DIN 31625, ISO 6438                4D  African
   42  6802 JP    JIS C 6226:1978                   $40  Japanese Kanji
   47    94 GB    BBC                                56  viewdata
   49    81 IAEA  INIS                               57  646 subset
   50    38 IAEA  INIS                           49  5D  symbols
   51    82 IAEA  INIS                           49  5E  Cyrillic
   53    76 ISO   5426:1980                       2  50  biblio.
   54    42 ISO   5427:1981                      37  51  Cyrillic
   55    73 ISO   5428:1980                      31  53  Greek biblio.
   57    94 CN    GB 1988-80                         54  646-CN
   58  7445 CN    GB 2312-80                        $41  Chinese Hanzi
   59    94 MA    CODAR-U                            5F  Arabic (Morocco)
   60    94 NO    NS 4551-1                          60  646-NO
   68    94 CA    APL WG                             65  APL Canadian
   69    94 FR    NF Z 62-010:1982                   66  646-FR
   70    85 CCITT                                    62  videotex suppl.
   71    94 CCITT                                    63  videotex mosaic G1
   84    94 ECMA  IBM                                67  646-PT
   85    94 ECMA  IBM                                68  646-ES
   86    94 HU    MSZ 7795/3                         69  646-HU
   87  6877 JP    JIS C 6226:1983 = JIS X 0208      $42  Japanese Kanji
   89    86 ASMO  ASMO 449, ISO 9036                 6B  Arabic-7
   90    83 ISO   6937/2                          2  6C  extra
   91    59 JP    JIS C 6229:1984                    6D  OCR a
   92    92 JP    JIS C 6229:1984                    6E  OCR b
   93     4 JP    JIS C 6229:1984                92  6F  OCR b extra
   94    64 JP    JIS C 6229:1984                    70  OCR hand
   95     1 JP    JIS C 6229:1984                94  71  OCR hand extra
   96    51 JP    JIS C 6229:1984                    72  OCR Katakana
   98    14 JP    ISO 2033:1983                      73  E13B magnetic
  100    96 ISO   8859/1:1987, ECMA-94            6 -41  Latin-1
  101    96 ISO   8859/2:1987, ECMA-94            6 -42  Latin-2
  102    88 CCITT T.61                               75  teletex
  103    67 CCITT T.61                          102  76  teletex extra
  108   605 CA    CSA T 500:1983                    %41  NAPLPS
  109    96 ISO   8859/3:1988, ECMA-94            6 -43  Latin-3
  110    96 ISO   8859/4:1988, ECMA-94            6 -44  Latin-4
  111    96 ECMA  ECMA-113:1986                   6 -40  Cyrillic
  121    94 CA    CSA Z243.4:1985                    77  Canadian 1
  122    94 CA    CSA Z243.4:1985                    78  Canadian 2
  123    96 CA    CSA Z243.4:1985                 ? -45  extra
  125   256 ISO   9040 + 9041                       /42  virt. term.
  126    90 ISO   8859/7:1987, ECMA-118           6 -46  Latin/Greek
  127    51 ISO   8859/6:1987, ASMO 708           6 -47  Latin/Arabic
  128    94 CCITT T.101                              7C  DS 3 - G2
  129    65 CCITT T.101                           ? -7D  DS 3 - G3
  131       CCITT T.101                             %43  Data syntax I
  137    59 CCITT T.101                              79  DS 1 - mosaic
  138    58 ISO   8859/8:1988, ECMA-121           6 -48  Latin/Hebrew
  139    96 CS    CSN 369103                      2 -49  Czech, Slovak
  141    94 YU    JUS I.B1.002                       7A  Croatian, Slovenian
  142    87 GB    ISO 6937/2 + addendum           2 -4A  extra
  143    96 NL    IEC P27-1                       6 -4B  symbols
  144    96 ISO   8859/5:1988, ECMA-113           6 -4C  Latin/Cyrillic
  145    96 CCITT T.101                             %44  Data syntax II
  146    94 YU    JUS I.B1.003                       7B  Serbian
  147    94 YU    JUS I.B1.004                       7D  Macedonian
  148    96 ISO   8859/9:1989, ECMA-128           6 -4D  Latin-5
  149  8224 KR    KSC 5601:1987                     $43  Korean Hanja
  150    94 CCITT                                   !40  Greek
  151    94 CS    NC NC99-10:81                     !41  646-CU
  152    25 CEN   ISO 6937/2:1983                 6 -4E  extra
  153    68 SU    GOST 19768:1987                 ? -4F  Cyrillic
  154    90 ECMA                                  6 -50  Latin 1-5 G3
  155    29 ISO   10367 (not final)               6 -51  box drawing
  156    87 ISO   6937:1992 (not final)             -52  suppl 6937
  157    96 SE    ISO 4873, ISO 8859/10           6 -56  Latin-6
  158    42 SE    ISO 4873                        6 -58  Latin/Lapp supp
  159  6067 JP    JIS X 0212:1990                   $44  supp. to 0208
  160       CCITT T.101                             %45  Videotex syntax
  161       CCITT T.101                             %46  Audio syntax
  162       ISO   10646 (not finished)              /40  10646 2-oct lev 1
  163       ISO   10646 (not finished)              /41  10646 4-oct lev 1
  164    27 CCITT                                 ? -53  Hebrew
  165  8443 CCITT                                   $45  Chinese comb.
  166    88 TH    TIS 620-2533:1990               6 -54  Thai
  167    82 ECMA                                  6 -55  Arab, Fr, Ger
  168  6879 JP    JIS X 0208:1990 (repl #87)        $42  new Kanji set
  169  2304 CA    Blissymbolics Comm Int'l          $46  Blissymbol
  170    82 DK    ISO 646:1992                      !42  invariant 646
  171  6085 ECMA  CNS 11643:1986                    $47  Chinese set 1
  172  7650 ECMA  CNS 11643:1986                    $48  Chinese set 2
  173    92 CCITT (repl #72)                         64  videotex mosaic G3
  174       ISO   10646 (not finished)              /43  10646 2-oct lev 2
  175       ISO   10646 (not finished)              /44  10646 4-oct lev 2
  176       ISO   10646 (not finished)              /45  10646 2-oct lev 3
  177       ISO   10646 (not finished)              /46  10646 4-oct lev 3
  178       ISO   10646 (not finished)              %42 10646 UTF-1
  179 --------- still pending ------------------------------------------


*/

/* @@#### NOTE: We should implement extended segments in compound text.

   XFree86 uses ESC % G (ESC 0x25 0x47) to switch into UTF8 mode, and
   ESC % @ (ESC 0x25 0x40) to switch out.  X also defines a more general
   mechanism for switching to extended segments:

   ESC '%' '/' '0' M L name-of-encoding 0x02 ... [variable-length]
   ESC '%' '/' '1' M L name-of-encoding 0x02 ... [1 byte per char]
   ESC '%' '/' '2' M L name-of-encoding 0x02 ... [2 bytes per char]
   ESC '%' '/' '3' M L name-of-encoding 0x02 ... [3 bytes per char]
   ESC '%' '/' '4' M L name-of-encoding 0x02 ... [4 bytes per char]

   where the name is encoded in ISO 8859-1 and M and L indicate the length
   in bytes of the extended segment, including the name and terminating
   0x02, and the length is computed as ((M - 128) * 128) + (L - 128).

   According to emacs-unicode, (at least) the following names exist:

  '(("big5-0" big5 2 (chinese-big5-1 chinese-big5-2))
    ("ISO8859-14" iso-8859-14 1 latin-iso8859-14)
    ("ISO8859-15" iso-8859-15 1 latin-iso8859-15))

*/

/* The following note taken directly from FSF 21.0.103. */

/* The following note describes the coding system ISO2022 briefly.
   Since the intention of this note is to help understand the
   functions in this file, some parts are NOT ACCURATE or are OVERLY
   SIMPLIFIED.  For thorough understanding, please refer to the
   original document of ISO2022.  This is equivalent to the standard
   ECMA-35, obtainable from <URL:http://www.ecma.ch/> (*).

   ISO2022 provides many mechanisms to encode several character sets
   in 7-bit and 8-bit environments.  For 7-bit environments, all text
   is encoded using bytes less than 128.  This may make the encoded
   text a little bit longer, but the text passes more easily through
   several types of gateway, some of which strip off the MSB (Most
   Significant Bit).

   There are two kinds of character sets: control character sets and
   graphic character sets.  The former contain control characters such
   as `newline' and `escape' to provide control functions (control
   functions are also provided by escape sequences).  The latter
   contain graphic characters such as 'A' and '-'.  Emacs recognizes
   two control character sets and many graphic character sets.

   Graphic character sets are classified into one of the following
   four classes, according to the number of bytes (DIMENSION) and
   number of characters in one dimension (CHARS) of the set:
   - DIMENSION1_CHARS94
   - DIMENSION1_CHARS96
   - DIMENSION2_CHARS94
   - DIMENSION2_CHARS96

   In addition, each character set is assigned an identification tag,
   unique for each set, called the "final character" (denoted as <F>
   hereafter).  The <F> of each character set is decided by ECMA(*)
   when it is registered in ISO.  The code range of <F> is 0x30..0x7F
   (0x30..0x3F are for private use only).

   Note (*): ECMA = European Computer Manufacturers Association

   Here are examples of graphic character sets [NAME(<F>)]:
	o DIMENSION1_CHARS94 -- ASCII('B'), right-half-of-JISX0201('I'), ...
	o DIMENSION1_CHARS96 -- right-half-of-ISO8859-1('A'), ...
	o DIMENSION2_CHARS94 -- GB2312('A'), JISX0208('B'), ...
	o DIMENSION2_CHARS96 -- none for the moment

   A code area (1 byte=8 bits) is divided into 4 areas, C0, GL, C1, and GR.
	C0 [0x00..0x1F] -- control character plane 0
	GL [0x20..0x7F] -- graphic character plane 0
	C1 [0x80..0x9F] -- control character plane 1
	GR [0xA0..0xFF] -- graphic character plane 1

   A control character set is directly designated and invoked to C0 or
   C1 by an escape sequence.  The most common case is that:
   - ISO646's  control character set is designated/invoked to C0, and
   - ISO6429's control character set is designated/invoked to C1,
   and usually these designations/invocations are omitted in encoded
   text.  In a 7-bit environment, only C0 can be used, and a control
   character for C1 is encoded by an appropriate escape sequence to
   fit into the environment.  All control characters for C1 are
   defined to have corresponding escape sequences.

   A graphic character set is at first designated to one of four
   graphic registers (G0 through G3), then these graphic registers are
   invoked to GL or GR.  These designations and invocations can be
   done independently.  The most common case is that G0 is invoked to
   GL, G1 is invoked to GR, and ASCII is designated to G0.  Usually
   these invocations and designations are omitted in encoded text.
   In a 7-bit environment, only GL can be used.

   When a graphic character set of CHARS94 is invoked to GL, codes
   0x20 and 0x7F of the GL area work as control characters SPACE and
   DEL respectively, and codes 0xA0 and 0xFF of the GR area should not
   be used.

   There are two ways of invocation: locking-shift and single-shift.
   With locking-shift, the invocation lasts until the next different
   invocation, whereas with single-shift, the invocation affects the
   following character only and doesn't affect the locking-shift
   state.  Invocations are done by the following control characters or
   escape sequences:

   ----------------------------------------------------------------------
   abbrev  function	             cntrl escape seq	description
   ----------------------------------------------------------------------
   SI/LS0  (shift-in)		     0x0F  none		invoke G0 into GL
   SO/LS1  (shift-out)		     0x0E  none		invoke G1 into GL
   LS2     (locking-shift-2)	     none  ESC 'n'	invoke G2 into GL
   LS3     (locking-shift-3)	     none  ESC 'o'	invoke G3 into GL
   LS1R    (locking-shift-1 right)   none  ESC '~'      invoke G1 into GR (*)
   LS2R    (locking-shift-2 right)   none  ESC '}'      invoke G2 into GR (*)
   LS3R    (locking-shift 3 right)   none  ESC '|'      invoke G3 into GR (*)
   SS2     (single-shift-2)	     0x8E  ESC 'N'	invoke G2 for one char
   SS3     (single-shift-3)	     0x8F  ESC 'O'	invoke G3 for one char
   ----------------------------------------------------------------------
   (*) These are not used by any known coding system.

   Control characters for these functions are defined by macros
   ISO_CODE_XXX in `coding.h'.

   Designations are done by the following escape sequences:
   ----------------------------------------------------------------------
   escape sequence	description
   ----------------------------------------------------------------------
   ESC '(' <F>		designate DIMENSION1_CHARS94<F> to G0
   ESC ')' <F>		designate DIMENSION1_CHARS94<F> to G1
   ESC '*' <F>		designate DIMENSION1_CHARS94<F> to G2
   ESC '+' <F>		designate DIMENSION1_CHARS94<F> to G3
   ESC ',' <F>		designate DIMENSION1_CHARS96<F> to G0 (*)
   ESC '-' <F>		designate DIMENSION1_CHARS96<F> to G1
   ESC '.' <F>		designate DIMENSION1_CHARS96<F> to G2
   ESC '/' <F>		designate DIMENSION1_CHARS96<F> to G3
   ESC '$' '(' <F>	designate DIMENSION2_CHARS94<F> to G0 (**)
   ESC '$' ')' <F>	designate DIMENSION2_CHARS94<F> to G1
   ESC '$' '*' <F>	designate DIMENSION2_CHARS94<F> to G2
   ESC '$' '+' <F>	designate DIMENSION2_CHARS94<F> to G3
   ESC '$' ',' <F>	designate DIMENSION2_CHARS96<F> to G0 (*)
   ESC '$' '-' <F>	designate DIMENSION2_CHARS96<F> to G1
   ESC '$' '.' <F>	designate DIMENSION2_CHARS96<F> to G2
   ESC '$' '/' <F>	designate DIMENSION2_CHARS96<F> to G3
   ----------------------------------------------------------------------

   In this list, "DIMENSION1_CHARS94<F>" means a graphic character set
   of dimension 1, chars 94, and final character <F>, etc...

   Note (*): Although these designations are not allowed in ISO2022,
   Emacs accepts them on decoding, and produces them on encoding
   CHARS96 character sets in a coding system which is characterized as
   7-bit environment, non-locking-shift, and non-single-shift.

   Note (**): If <F> is '@', 'A', or 'B', the intermediate character
   '(' can be omitted.  We refer to this as "short-form" hereafter.

   Now you may notice that there are a lot of ways of encoding the
   same multilingual text in ISO2022.  Actually, there exist many
   coding systems such as Compound Text (used in X11's inter client
   communication, ISO-2022-JP (used in Japanese Internet), ISO-2022-KR
   (used in Korean Internet), EUC (Extended UNIX Code, used in Asian
   localized platforms), and all of these are variants of ISO2022.

   In addition to the above, Emacs handles two more kinds of escape
   sequences: ISO6429's direction specification and Emacs' private
   sequence for specifying character composition.

   ISO6429's direction specification takes the following form:
	o CSI ']'      -- end of the current direction
	o CSI '0' ']'  -- end of the current direction
	o CSI '1' ']'  -- start of left-to-right text
	o CSI '2' ']'  -- start of right-to-left text
   The control character CSI (0x9B: control sequence introducer) is
   abbreviated to the escape sequence ESC '[' in a 7-bit environment.

   Character composition specification takes the following form:
	o ESC '0' -- start relative composition
	o ESC '1' -- end composition
	o ESC '2' -- start rule-base composition (*)
	o ESC '3' -- start relative composition with alternate chars  (**)
	o ESC '4' -- start rule-base composition with alternate chars  (**)
  Since these are not standard escape sequences of any ISO standard,
  the use of them with these meanings is restricted to Emacs only.

  (*) This form is used only in Emacs 20.5 and older versions,
  but the newer versions can safely decode it.
  (**) This form is used only in Emacs 21.1 and newer versions,
  and the older versions can't decode it.

  Here's a list of example usages of these composition escape
  sequences (categorized by `enum composition_method').

  COMPOSITION_RELATIVE:
	ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE:
	ESC 2 CHAR [ RULE CHAR ] ESC 1
  COMPOSITION_WITH_ALTCHARS:
	ESC 3 ALTCHAR [ ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1
  COMPOSITION_WITH_RULE_ALTCHARS:
	ESC 4 ALTCHAR [ RULE ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1

 */

static void
reset_iso2022_decode (Lisp_Object coding_system,
		      struct iso2022_coding_stream *data)
{
  int i;
#ifdef ENABLE_COMPOSITE_CHARS
  unsigned_char_dynarr *old_composite_chars = data->composite_chars;
#endif

  xzero (*data);
  
  for (i = 0; i < 4; i++)
    {
      if (!NILP (coding_system))
	data->charset[i] =
	  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (coding_system, i);
      else
	data->charset[i] = Qt;
    }
  data->esc = ISO_ESC_NOTHING;
  data->register_right = 1;
#ifdef ENABLE_COMPOSITE_CHARS
  if (old_composite_chars)
    {
      data->composite_chars = old_composite_chars;
      Dynarr_reset (data->composite_chars);
    }
#endif
  data->ch = -1;
}

static int
charset_iso2022_compatible (Lisp_Object charset)
{
  return get_charset_iso2022_type (charset) != -1;
}

/* Recreate the Unicode precedence array.  We want the following:

   (1) Charsets currently designated should be at the top of the list.
   (2) Then ASCII and Control-1, if not already there. (Hack)
*/

static void
reset_iso2022_encode (Lisp_Object coding_system,
		      struct iso2022_coding_stream *data)
{
  int i;

  xzero (*data);
  
  for (i = 0; i < 4; i++)
    {
      data->charset[i] =
	XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (coding_system, i);
      data->force_charset_on_output[i] =
	XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (coding_system, i);
    }
  data->register_right = 1;
}

static void
iso2022_init_coding_stream (struct coding_stream *str)
{
  if (str->direction == CODING_DECODE)
    reset_iso2022_decode (str->codesys,
			  CODING_STREAM_TYPE_DATA (str, iso2022));
  else
    reset_iso2022_encode (str->codesys,
			  CODING_STREAM_TYPE_DATA (str, iso2022));
}
static void
iso2022_mark_iso2022_coding_stream (struct iso2022_coding_stream *data)
{
  int i;
  for (i = 0; i < 4; i++)
    mark_object (data->charset[i]);
}

static void
iso2022_mark_coding_stream (struct coding_stream *str)
{
  struct iso2022_coding_stream *data = CODING_STREAM_TYPE_DATA (str, iso2022);
  iso2022_mark_iso2022_coding_stream (data);
}

static int
fit_to_be_escape_quoted (int c)
{
  switch (c)
    {
    case ISO_CODE_ESC:
    case ISO_CODE_CSI:
    case ISO_CODE_SS2:
    case ISO_CODE_SS3:
    case ISO_CODE_SO:
    case ISO_CODE_SI:
      return 1;

    default:
      return 0;
    }
}

static Lisp_Object
charset_by_attributes_or_create_one (int type, Ibyte final, int dir)
{
  Lisp_Object charset = charset_by_attributes (type, final, dir);

  if (NILP (charset))
    {
      int chars, dim;

      switch (type)
	{
	case CHARSET_TYPE_94:
	  chars = 94; dim = 1;
	  break;
	case CHARSET_TYPE_96:
	  chars = 96; dim = 1;
	  break;
	case CHARSET_TYPE_94X94:
	  chars = 94; dim = 2;
	  break;
	case CHARSET_TYPE_96X96:
	  chars = 96; dim = 2;
	  break;
	default:
	  ABORT (); chars = 0; dim = 0;
	}

      charset = Fmake_charset (Qunbound, Qnil,
			       nconc2 (list6 (Qfinal, make_char (final),
					      Qchars, make_fixnum (chars),
					      Qdimension, make_fixnum (dim)),
				       list2 (Qdirection, 
					      dir == CHARSET_LEFT_TO_RIGHT ?
					      Ql2r : Qr2l)));
    }

  return charset;
}

/* Parse one byte of an ISO2022 escape sequence.
   If the result is an invalid escape sequence, return 0 and
   do not change anything in STR.  Otherwise, if the result is
   an incomplete escape sequence, update ISO2022.ESC and
   ISO2022.ESC_BYTES and return -1.  Otherwise, update
   all the state variables (but not ISO2022.ESC_BYTES) and
   return 1.

   If CHECK_INVALID_CHARSETS is non-zero, check for designation
   or invocation of an invalid character set and treat that as
   an unrecognized escape sequence.

*/

static int
parse_iso2022_esc (Lisp_Object codesys, struct iso2022_coding_stream *iso,
		   unsigned char c, unsigned int *flags,
		   int check_invalid_charsets)
{
  /* (1) If we're at the end of a designation sequence, CS is the
     charset being designated and REG is the register to designate
     it to.

     (2) If we're at the end of a locking-shift sequence, REG is
     the register to invoke and HALF (0 == left, 1 == right) is
     the half to invoke it into.

     (3) If we're at the end of a single-shift sequence, REG is
     the register to invoke. */
  Lisp_Object cs = Qnil;
  int reg, half;

  /* NOTE: This code does goto's all over the fucking place.
     The reason for this is that we're basically implementing
     a state machine here, and hierarchical languages like C
     don't really provide a clean way of doing this. */

  if (! (*flags & ISO_STATE_ESCAPE))
    /* At beginning of escape sequence; we need to reset our
       escape-state variables. */
    iso->esc = ISO_ESC_NOTHING;

  switch (iso->esc)
    {
    case ISO_ESC_NOTHING:
      iso->esc_bytes_index = 0;
      switch (c)
	{
	case ISO_CODE_ESC:	/* Start escape sequence */
	  *flags |= ISO_STATE_ESCAPE;
	  iso->esc = ISO_ESC;
	  goto not_done;

	case ISO_CODE_CSI:      /* ISO6429 (specifying directionality) */
	  *flags |= ISO_STATE_ESCAPE;
	  iso->esc = ISO_ESC_5_11;
	  goto not_done;

	case ISO_CODE_SO:	/* locking shift 1 */
	  reg = 1; half = 0;
	  goto locking_shift;
	case ISO_CODE_SI:	/* locking shift 0 */
	  reg = 0; half = 0;
	  goto locking_shift;

	case ISO_CODE_SS2:	/* single shift */
	  reg = 2;
	  goto single_shift;
	case ISO_CODE_SS3:	/* single shift */
	  reg = 3;
	  goto single_shift;

	default:			/* Other control characters */
	error:
	  *flags &= ISO_STATE_LOCK;
	  return 0;
	}

    case ISO_ESC:

      /* The only available ISO 2022 sequence in UTF-8 mode is ESC % @, to
	 exit from it. If we see any other escape sequence, pass it through
	 in the error handler.  */
      if (*flags & ISO_STATE_UTF_8 && '%' != c)
	{
	  return 0;
	}

      switch (c)
	{
	  /**** single shift ****/

	case 'N':	/* single shift 2 */
	  reg = 2;
	  goto single_shift;
	case 'O':	/* single shift 3 */
	  reg = 3;
	  goto single_shift;

	  /**** locking shift ****/

	case '~':	/* locking shift 1 right */
	  reg = 1; half = 1;
	  goto locking_shift;
	case 'n':	/* locking shift 2 */
	  reg = 2; half = 0;
	  goto locking_shift;
	case '}':	/* locking shift 2 right */
	  reg = 2; half = 1;
	  goto locking_shift;
	case 'o':	/* locking shift 3 */
	  reg = 3; half = 0;
	  goto locking_shift;
	case '|':	/* locking shift 3 right */
	  reg = 3; half = 1;
	  goto locking_shift;

	  /**** composite ****/

#ifdef ENABLE_COMPOSITE_CHARS
	case '0':
	  iso->esc = ISO_ESC_START_COMPOSITE;
	  *flags = (*flags & ISO_STATE_LOCK) |
	    ISO_STATE_COMPOSITE;
	  return 1;

	case '1':
	  iso->esc = ISO_ESC_END_COMPOSITE;
	  *flags = (*flags & ISO_STATE_LOCK) &
	    ~ISO_STATE_COMPOSITE;
	  return 1;
#else
	case '0': case '1': case '2': case '3': case '4':
	  /* We simply return a flag indicating that some composite
	     escape was seen.  The caller will use the particular
	     character to encode the appropriate "composite hack"
	     character out of Vcharset_composite, so that we will
	     preserve these values on output. */
	  iso->esc = ISO_ESC_START_COMPOSITE;
	  *flags &= ISO_STATE_LOCK;
	  return 1;
#endif /* ENABLE_COMPOSITE_CHARS */

	  /**** directionality ****/

	case '[':
	  iso->esc = ISO_ESC_5_11;
	  goto not_done;

	  /**** extended segments (escape to/from Unicode) ****/
	case '%':
	  iso->esc = ISO_ESC_2_5;
	  goto not_done;

	  /**** designation ****/

	case '$':	/* multibyte charset prefix */
	  iso->esc = ISO_ESC_2_4;
	  goto not_done;

	default:
	  if (0x28 <= c && c <= 0x2F)
	    {
	      iso->esc = (enum iso_esc_flag) (c - 0x28 + ISO_ESC_2_8);
	      goto not_done;
	    }

	  /* This function is called with CODESYS equal to nil when
	     doing coding-system detection. */
	  if (!NILP (codesys)
	      && XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
	      && fit_to_be_escape_quoted (c))
	    {
	      iso->esc = ISO_ESC_LITERAL;
	      *flags &= ISO_STATE_LOCK;
	      return 1;
	    }

	  /* bzzzt! */
	  goto error;
	}

      /* extended segments: ISO-IR 196 UTF-8 support. */
    case ISO_ESC_2_5:
      if ('G' == c)
	{
	  /* Activate UTF-8 mode. */
	  *flags &= ISO_STATE_LOCK;
	  *flags |= ISO_STATE_UTF_8;
	  iso->esc = ISO_ESC_NOTHING;
	  xzero (iso->unicode);
	  return 1;
	}
      else if ('@' == c)
	{
	  /* Deactive UTF-8 mode. */
	  *flags &= ISO_STATE_LOCK;
	  *flags &= ~(ISO_STATE_UTF_8);
	  iso->esc = ISO_ESC_NOTHING;
	  return 1;
	}
      else 
	{
	  /* Oops, we don't support the other UTF-? coding systems within
	     ISO 2022, only in their own context. */
	  goto error;
	}
      /**** directionality ****/

    case ISO_ESC_5_11:		/* ISO6429 direction control */
      if (c == ']')
	{
	  *flags &= (ISO_STATE_LOCK & ~ISO_STATE_R2L);
	  goto directionality;
	}
      if      (c == '0') iso->esc = ISO_ESC_5_11_0;
      else if (c == '1') iso->esc = ISO_ESC_5_11_1;
      else if (c == '2') iso->esc = ISO_ESC_5_11_2;
      else               goto error;
      goto not_done;

    case ISO_ESC_5_11_0:
      if (c == ']')
	{
	  *flags &= (ISO_STATE_LOCK & ~ISO_STATE_R2L);
	  goto directionality;
	}
      goto error;

    case ISO_ESC_5_11_1:
      if (c == ']')
	{
	  *flags = (ISO_STATE_LOCK & ~ISO_STATE_R2L);
	  goto directionality;
	}
      goto error;

    case ISO_ESC_5_11_2:
      if (c == ']')
	{
	  *flags = (*flags & ISO_STATE_LOCK) | ISO_STATE_R2L;
	  goto directionality;
	}
      goto error;

    directionality:
      iso->esc = ISO_ESC_DIRECTIONALITY;
      return 1;


      /**** designation ****/

    case ISO_ESC_2_4:
      if (0x28 <= c && c <= 0x2F)
	{
	  iso->esc = (enum iso_esc_flag) (c - 0x28 + ISO_ESC_2_4_8);
	  goto not_done;
	}
      if (0x40 <= c && c <= 0x42)
	{
	  cs = charset_by_attributes_or_create_one (CHARSET_TYPE_94X94, c,
						    *flags & ISO_STATE_R2L ?
						    CHARSET_RIGHT_TO_LEFT :
						    CHARSET_LEFT_TO_RIGHT);
	  reg = 0;
	  goto designated;
	}
      goto error;

    default:
      {
	int type = -1;

	if (iso->esc >= ISO_ESC_2_8 &&
	    iso->esc <= ISO_ESC_2_15)
	  {
	    type = ((iso->esc >= ISO_ESC_2_12) ?
		    CHARSET_TYPE_96 : CHARSET_TYPE_94);
	    reg = (iso->esc - ISO_ESC_2_8) & 3;
	  }
	else if (iso->esc >= ISO_ESC_2_4_8 &&
		 iso->esc <= ISO_ESC_2_4_15)
	  {
	    type = ((iso->esc >= ISO_ESC_2_4_12) ?
		    CHARSET_TYPE_96X96 : CHARSET_TYPE_94X94);
	    reg = (iso->esc - ISO_ESC_2_4_8) & 3;
	  }
	else
	  {
	    /* Can this ever be reached? -slb */
	    ABORT ();
	    goto error;
	  }

	if (c < '0' || c > '~' ||
	    (c > 0x5F && (type == CHARSET_TYPE_94X94 ||
	                  type == CHARSET_TYPE_96X96)))
	  goto error; /* bad final byte */

	cs = charset_by_attributes_or_create_one (type, c,
						  *flags & ISO_STATE_R2L ?
						  CHARSET_RIGHT_TO_LEFT :
						  CHARSET_LEFT_TO_RIGHT);
	goto designated;
      }
    }

 not_done:
  iso->esc_bytes[iso->esc_bytes_index++] = (unsigned char) c;
  return -1;

 single_shift:
  if (check_invalid_charsets && !CHARSETP (iso->charset[reg]))
    /* can't invoke something that ain't there. */
    goto error;
  iso->esc = ISO_ESC_SINGLE_SHIFT;
  *flags &= ISO_STATE_LOCK;
  if (reg == 2)
    *flags |= ISO_STATE_SS2;
  else
    *flags |= ISO_STATE_SS3;
  return 1;

 locking_shift:
  if (check_invalid_charsets &&
      !CHARSETP (iso->charset[reg]))
    /* can't invoke something that ain't there. */
    goto error;
  if (half)
    iso->register_right = reg;
  else
    iso->register_left = reg;
  *flags &= ISO_STATE_LOCK;
  iso->esc = ISO_ESC_LOCKING_SHIFT;
  return 1;

 designated:
  if (NILP (cs) && check_invalid_charsets)
    {
      /* This should never happen, since we automatically create temporary
	 charsets as necessary. --ben */
      ABORT ();
    }
  /* This function is called with CODESYS equal to nil when
     doing coding-system detection. */
  if (!NILP (codesys))
    {
      charset_conversion_spec_dynarr *dyn =
	XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys);

      if (dyn)
	{
	  int i;

	  for (i = 0; i < Dynarr_length (dyn); i++)
	    {
	      struct charset_conversion_spec *spec = Dynarr_atp (dyn, i);
	      if (EQ (cs, spec->from_charset))
		cs = spec->to_charset;
	    }
	}
    }

  iso->charset[reg] = cs;
  iso->esc = ISO_ESC_DESIGNATE;
  *flags &= ISO_STATE_LOCK;
  return 1;
}

/* If FLAGS is a null pointer or specifies right-to-left motion,
   output a switch-dir-to-left-to-right sequence to DST.
   Also update FLAGS if it is not a null pointer.
   If INTERNAL_P is set, we are outputting in internal format and
   need to handle the CSI differently. */

static void
restore_left_to_right_direction (Lisp_Object codesys,
				 unsigned_char_dynarr *dst,
				 unsigned int *flags,
				 int internal_p)
{
  if (!flags || (*flags & ISO_STATE_R2L))
    {
      if (XCODING_SYSTEM_ISO2022_SEVEN (codesys))
	{
	  Dynarr_add (dst, ISO_CODE_ESC);
	  Dynarr_add (dst, '[');
	}
      else if (internal_p)
	DECODE_ADD_BINARY_CHAR (ISO_CODE_CSI, dst);
      else
	Dynarr_add (dst, ISO_CODE_CSI);
      Dynarr_add (dst, '0');
      Dynarr_add (dst, ']');
      if (flags)
	*flags &= ~ISO_STATE_R2L;
    }
}

/* If FLAGS is a null pointer or specifies a direction different from
   DIRECTION (which should be either CHARSET_RIGHT_TO_LEFT or
   CHARSET_LEFT_TO_RIGHT), output the appropriate switch-dir escape
   sequence to DST.  Also update FLAGS if it is not a null pointer.
   If INTERNAL_P is set, we are outputting in internal format and
   need to handle the CSI differently. */

static void
ensure_correct_direction (int direction, Lisp_Object codesys,
			  unsigned_char_dynarr *dst, unsigned int *flags,
			  int internal_p)
{
  if ((!flags || (*flags & ISO_STATE_R2L)) &&
      direction == CHARSET_LEFT_TO_RIGHT)
    restore_left_to_right_direction (codesys, dst, flags, internal_p);
  else if (!XCODING_SYSTEM_ISO2022_NO_ISO6429 (codesys)
	   && (!flags || !(*flags & ISO_STATE_R2L)) &&
	   direction == CHARSET_RIGHT_TO_LEFT)
    {
      if (XCODING_SYSTEM_ISO2022_SEVEN (codesys))
	{
	  Dynarr_add (dst, ISO_CODE_ESC);
	  Dynarr_add (dst, '[');
	}
      else if (internal_p)
	DECODE_ADD_BINARY_CHAR (ISO_CODE_CSI, dst);
      else
	Dynarr_add (dst, ISO_CODE_CSI);
      Dynarr_add (dst, '2');
      Dynarr_add (dst, ']');
      if (flags)
	*flags |= ISO_STATE_R2L;
    }
}

/* Convert ISO2022-format data to internal format. */

static Bytecount
iso2022_decode (struct coding_stream *str, const UExtbyte *src,
		Bytecount n, unsigned_char_dynarr *dst)
{
#ifdef ENABLE_COMPOSITE_CHARS
  unsigned_char_dynarr *real_dst = dst;
#endif
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);
  unsigned int flags  = data->flags;
  Bytecount orign = n;

#ifdef ENABLE_COMPOSITE_CHARS
  if (flags & ISO_STATE_COMPOSITE)
    dst = data->composite_chars;
#endif /* ENABLE_COMPOSITE_CHARS */

  while (n--)
    {
      UExtbyte c = *src++;
      if (flags & ISO_STATE_ESCAPE)
	{	/* Within ESC sequence */
	  int retval = parse_iso2022_esc (str->codesys, data,
					  c, &flags, 1);

	  if (retval)
	    {
	      switch (data->esc)
		{
#ifdef ENABLE_COMPOSITE_CHARS
		case ISO_ESC_START_COMPOSITE:
		  if (data->composite_chars)
		    Dynarr_reset (data->composite_chars);
		  else
		    data->composite_chars = Dynarr_new (unsigned_char);
		  dst = data->composite_chars;
		  break;
		case ISO_ESC_END_COMPOSITE:
		  {
		    Ichar emch = lookup_composite_char (Dynarr_begin (dst),
							Dynarr_length (dst));
		    dst = real_dst;
		    Dynarr_add_ichar (dst, emch);
		    break;
		  }
#else
		case ISO_ESC_START_COMPOSITE:
		  {
		    if (charset_codepoint_to_dynarr
			(Vcharset_composite, 0, c - '0' + ' ',
			 dst, CONVERR_FAIL) == 0)
		      DECODE_ERROR_OCTET (c, dst);
		    break;
		  }
#endif /* ENABLE_COMPOSITE_CHARS */

		case ISO_ESC_LITERAL:
		  DECODE_ADD_BINARY_CHAR (c, dst);
		  break;

		default:
		  /* Everything else handled already */
		  break;
		}
	    }
	  else
	    {
	      /* Error recovery. */
	      /* Output the (possibly invalid) sequence */
	      int i;
	      for (i = 0; i < data->esc_bytes_index; i++)
		DECODE_ERROR_OCTET (data->esc_bytes[i], dst);
	      flags &= ISO_STATE_LOCK;
	      n++, src--;/* Repeat the loop with the same character. */
	    }
	  data->ch = -1;
	}
      else if (flags & ISO_STATE_UTF_8)
	{
	  if (ISO_CODE_ESC == c)
	    {
	      /* Allow the escape sequence parser to end the UTF-8 state. */
	      flags |= ISO_STATE_ESCAPE;
	      data->esc = ISO_ESC;
	      data->esc_bytes_index = 1;
	      continue;
	    }

	  /* We allow private codepoints when escape-quoted, because we want
	     to represent UTF-8 error chars as distinct from the
	     corresponding ISO 8859-1 characters in escape-quoted.  This is
	     because escape-quoted is used for auto-saving, and we want
	     auto saves and similar files to maintain error-octet chars as
	     such rather than converting them to ASCII or ISO 8859-1.

	     [[ However, we can't differentiate UTF-8 error chars as written
	     to disk, and UTF-8 errors in escape-quoted.  This is not a big
	     problem; non-Unicode-chars-encoded-as-UTF-8-in-ISO-2022 is not
	     deployed, in practice, so if such a sequence of octets occurs,
	     XEmacs generated it. ]] -- I don't quite understand this. --ben
	  */
	  decode_utf_8 (&data->unicode, dst, c, 0,
			XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (str->codesys));

          if (str->st.eof && data->unicode.counter)
            {
              indicate_invalid_utf_8 (data->unicode.indicated_length, 
				      data->unicode.counter,
				      data->unicode.ch,
				      dst, &data->unicode, 0);
              data->unicode.counter = 0;
              data->unicode.ch = 0;
            }
	}
      else if (byte_c0_p (c) || byte_c1_p (c))
	{ /* Control characters */

	  /***** Error-handling *****/

	  /* If we were in the middle of a character, dump out the
	     partial character. */
	  if (data->ch >= 0)
	    DECODE_ERROR_OCTET (data->ch, dst);
	  data->ch = -1;

	  /* If we just saw a single-shift character, dump it out.
	     This may dump out the wrong sort of single-shift character,
	     but least it will give an indication that something went
	     wrong. */
	  if (flags & ISO_STATE_SS2)
	    {
	      DECODE_ERROR_OCTET (ISO_CODE_SS2, dst);
	      flags &= ~ISO_STATE_SS2;
	    }
	  if (flags & ISO_STATE_SS3)
	    {
	      DECODE_ERROR_OCTET (ISO_CODE_SS3, dst);
	      flags &= ~ISO_STATE_SS3;
	    }

	  /***** Now handle the control characters. *****/

	  flags &= ISO_STATE_LOCK;

	  if (!parse_iso2022_esc (str->codesys, data, c, &flags, 1))
	    DECODE_ADD_BINARY_CHAR (c, dst);
	}
      else
	{			/* Graphic characters */
	  Lisp_Object charset;
	  int reg;

	  /* Now determine the charset. */
	  reg = ((flags & ISO_STATE_SS2) ? 2
		 : (flags & ISO_STATE_SS3) ? 3
		 : !byte_ascii_p (c) ? data->register_right
		 : data->register_left);
	  charset = data->charset[reg];

	  /* Error checking: */
	  if (! CHARSETP (charset)
	      || (((c & 0x7F) == ' ' || (c & 0x7F) == ISO_CODE_DEL)
		  && (XCHARSET_CHARS (charset, 0) == 94 ||
                      XCHARSET_CHARS (charset, 1) == 94)))
	    /* Mrmph.  We are trying to invoke a register that has no
	       or an invalid charset in it, or trying to add a character
	       outside the range of the charset.  Insert that char literally
	       to preserve it for the output. */
	    {
	      if (data->ch >= 0)
		DECODE_ERROR_OCTET (data->ch, dst);
	      data->ch = -1;
	      DECODE_ERROR_OCTET (c, dst);
	    }
	  else
	    {
	      /* Things are probably hunky-dorey. */

	      /* Fetch reverse charset, maybe. */
	      if (((flags & ISO_STATE_R2L) &&
		   XCHARSET_DIRECTION (charset) == CHARSET_LEFT_TO_RIGHT)
		  ||
		  (!(flags & ISO_STATE_R2L) &&
		   XCHARSET_DIRECTION (charset) == CHARSET_RIGHT_TO_LEFT))
		{
		  Lisp_Object new_charset =
		    XCHARSET_REVERSE_DIRECTION_CHARSET (charset);
		  if (!NILP (new_charset))
		    charset = new_charset;
		}

	      if (XCHARSET_DIMENSION (charset) == 2 && data->ch < 0)
		data->ch = c;
	      else
		{
		  int c1, c2;
		  c1 = XCHARSET_DIMENSION (charset) == 2 ? data->ch & 0x7F : 0;
		  c2 = c & 0x7F;

		  if (XCHARSET_OFFSET (charset, 0) >= 128)
		    c1 += 128;
		  if (XCHARSET_OFFSET (charset, 1) >= 128)
		    c2 += 128;

#ifdef UNICODE_INTERNAL
                  if (XCODING_SYSTEM_ISO2022_ISO2022_PRESERVE (str->codesys))
		    {
		      if (EQ (charset, Vcharset_ascii))
			Dynarr_add (dst, (unsigned char) c2);
		      else
			{
			  int priv =
			    charset_codepoint_to_private_unicode (charset, c1,
								  c2);
			  Dynarr_add_ichar (dst, (Ichar) priv);
			}
		    }
		  else
#endif /* UNICODE_INTERNAL */
		    {
		      /* @@#### Handle error differenly? Especially here! 
			 This is the main place where we convert an
			 ISO-2022-encoded char in a national charset to
			 Unicode. */
		      charset_codepoint_to_dynarr
			(charset, c1, c2, dst, CONVERR_USE_PRIVATE);
		    }
		  data->ch = -1;
		}
	    }

	  if (data->ch < 0)
	    flags &= ISO_STATE_LOCK;
	}

    }

  data->flags = flags;

  DECODE_OUTPUT_PARTIAL_CHAR (str, data, dst);

  return orign;
}


/***** ISO2022 encoder *****/

/* Designate CHARSET into register REG. */

static void
iso2022_designate (Lisp_Object charset, int reg,
		   struct coding_stream *str, unsigned_char_dynarr *dst)
{
  static const char inter94[] = "()*+";
  static const char inter96[] = ",-./";
  unsigned char final;
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);
  Lisp_Object old_charset = data->charset[reg];
  int type;

  data->charset[reg] = charset;
  if (!CHARSETP (charset))
    /* charset might be an initial nil or t. */
    return;
  type = get_charset_iso2022_type (charset);
  text_checking_assert (type >= 0);
  final = XCHARSET_FINAL (charset);
  if (!data->force_charset_on_output[reg] &&
      CHARSETP (old_charset) &&
      get_charset_iso2022_type (old_charset) == type &&
      XCHARSET_FINAL (old_charset) == final)
    return;

  data->force_charset_on_output[reg] = 0;

  {
    charset_conversion_spec_dynarr *dyn =
      XCODING_SYSTEM_ISO2022_OUTPUT_CONV (str->codesys);

    if (dyn)
      {
	int i;

	for (i = 0; i < Dynarr_length (dyn); i++)
	  {
	    struct charset_conversion_spec *spec = Dynarr_atp (dyn, i);
	    if (EQ (charset, spec->from_charset))
		charset = spec->to_charset;
	  }
      }
  }

  Dynarr_add (dst, ISO_CODE_ESC);

  switch (type)
    {
    case CHARSET_TYPE_94:
      Dynarr_add (dst, inter94[reg]);
      break;
    case CHARSET_TYPE_96:
      Dynarr_add (dst, inter96[reg]);
      break;
    case CHARSET_TYPE_94X94:
      Dynarr_add (dst, '$');
      if (reg != 0
	  || !(XCODING_SYSTEM_ISO2022_SHORT (str->codesys))
	  || final < '@'
	  || final > 'B')
	Dynarr_add (dst, inter94[reg]);
      break;
    case CHARSET_TYPE_96X96:
      Dynarr_add (dst, '$');
      Dynarr_add (dst, inter96[reg]);
      break;
    }
  Dynarr_add (dst, final);
}

static void
ensure_normal_shift (struct coding_stream *str, unsigned_char_dynarr *dst)
{
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);

  if (data->register_left != 0)
    {
      Dynarr_add (dst, ISO_CODE_SI);
      data->register_left = 0;
    }
}

static void
ensure_shift_out (struct coding_stream *str, unsigned_char_dynarr *dst)
{
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);

  if (data->register_left != 1)
    {
      Dynarr_add (dst, ISO_CODE_SO);
      data->register_left = 1;
    }
}

/* Convert internally-formatted data to ISO2022 format. */

static Bytecount
iso2022_encode (struct coding_stream *str, const Ibyte *src,
		Bytecount n, unsigned_char_dynarr *dst)
{
  Lisp_Object codesys = str->codesys;
  int i;
  struct iso2022_coding_stream *data = CODING_STREAM_TYPE_DATA (str, iso2022);
  unsigned int flags = data->flags;
  const Ibyte *srcend = src + n;

#ifdef ENABLE_COMPOSITE_CHARS
  /* flags for handling composite chars.  We do a little switcheroo
     on the source while we're outputting the composite char. */
  const Ibyte *saved_src = NULL;
  const Ibyte *saved_srcend = NULL;
  int in_composite = 0;

 back_to_square_n:
#endif
  while (src < srcend)
    {
      Ibyte c = *src;

      if (byte_ascii_p (c))
	{		/* Processing ASCII character */
	  if (flags & ISO_STATE_UTF_8)
	    {
	      Dynarr_add (dst, ISO_CODE_ESC);
	      Dynarr_add (dst, '%');
	      Dynarr_add (dst, '@');
	      flags &= ~(ISO_STATE_UTF_8);
	    }

	  restore_left_to_right_direction (codesys, dst, &flags, 0);

	  /* Make sure G0 contains ASCII */
	  if ((c > ' ' && c < ISO_CODE_DEL) ||
	      !XCODING_SYSTEM_ISO2022_NO_ASCII_CNTL (codesys))
	    {
	      ensure_normal_shift (str, dst);
	      iso2022_designate (Vcharset_ascii, 0, str, dst);
	    }

	  /* If necessary, restore everything to the default state
	     at end-of-line */
	  if (!(XCODING_SYSTEM_ISO2022_NO_ASCII_EOL (codesys)))
	    {
	      /* NOTE: CRLF encoding happens *BEFORE* other encoding.
		 Thus, even though we're working with internal-format
		 data, there may be CR's or CRLF sequences representing
		 newlines. */
	      if (c == '\r' || (c == '\n' && !(flags & ISO_STATE_CR)))
		{
		  restore_left_to_right_direction (codesys, dst, &flags, 0);

		  ensure_normal_shift (str, dst);

		  for (i = 0; i < 4; i++)
		    {
		      Lisp_Object initial_charset =
			XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i);
		      iso2022_designate (initial_charset, i, str, dst);
		    }
		}
	      if (c == '\r')
		flags |= ISO_STATE_CR;
	      else
		flags &= ~ISO_STATE_CR;
	    }
	  
	  if (XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
	      && fit_to_be_escape_quoted (c))
	    Dynarr_add (dst, ISO_CODE_ESC);
	  Dynarr_add (dst, c);
	  src++;
	}
      else
	{
	  /* Processing a non-ASCII character */
	  Lisp_Object charset = Qnil;
	  int c1, c2;
	  Ichar ich = itext_ichar (src);
	  int half = 0;

	  INC_IBYTEPTR (src);

	  if (handle_possible_error_octet (ich, str, src, dst, NULL))
	    {
	      ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
	    }

	  /* Convert character to a charset codepoint. */
	  /* First, try the charsets mentioned in the coding system. */

	  for (i = 0; i < 4; i++)
	    {
	      if (CHARSETP (data->charset[i]) &&
		  ichar_to_one_charset_codepoint (ich, data->charset[i],
						  &c1, &c2))
		{
		  charset = data->charset[i];
		  break;
		}
	    }

	  /* Also ASCII and Control-1.
	     @@#### Do we really want to do this? */
	  if (NILP (charset) &&
	      ichar_to_one_charset_codepoint (ich, Vcharset_ascii, &c1, &c2))
	    charset = Vcharset_ascii;

	  if (NILP (charset) &&
	      ichar_to_one_charset_codepoint (ich, Vcharset_control_1,
					      &c1, &c2))
	    charset = Vcharset_control_1;

	  if (NILP (charset))
	    {
	      /* Then try any ISO2022-compatible charset */
	      /* @@#### current_buffer dependency */
	      buffer_filtered_ichar_to_charset_codepoint
		(ich, current_buffer, charset_iso2022_compatible,
		 &charset, &c1, &c2, CONVERR_FAIL);
	    }
	  /* No point in trying to find a non-ISO2022-compatible
	     charset -- at this point we will encode in UTF-8
	     anyway */

	  /* ---------------------------------------------------- */
	  /* 1. Are we processing control-1?                      */
	  /* ---------------------------------------------------- */

	  if (EQ (charset, Vcharset_control_1))
	    {
	      if (XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
		  && fit_to_be_escape_quoted (c2))
		Dynarr_add (dst, ISO_CODE_ESC);
	      /* you asked for it ... */
	      Dynarr_add (dst, c2);
	    }

	  /* ---------------------------------------------------- */
	  /* 2. Are we processing a composite character?          */
	  /* ---------------------------------------------------- */

	  else if (EQ (charset, Vcharset_composite))
	    {
#ifdef ENABLE_COMPOSITE_CHARS
	      if (in_composite)
		{
		  /* #### Bother! We don't know how to
		     handle this yet. */
		  handle_standard_encoding_error (str, src, dst);
		}
	      else
		{
		  Ichar emch =
		    charset_codepoint_to_ichar
		    (Vcharset_composite, c1, c2, CONVERR_FAIL);
		  if (emch >= 0)
		    {
		      Lisp_Object lstr =
			composite_char_string (emch);
		      saved_src = src;
		      saved_srcend = srcend;
		      in_composite = 1;
		      src = XSTRING_DATA (lstr);
		      srcend = src + XSTRING_LENGTH (lstr);
		      Dynarr_add (dst, ISO_CODE_ESC);
		      Dynarr_add (dst, '0'); /* start composing */
		    }
		}
#else /* not ENABLE_COMPOSITE_CHARS */
	      c2 &= 127;
	      if (c2 >= 32 || c2 <= 36) /* Someone might have stuck in
					   something else */
		{
		  Dynarr_add (dst, ISO_CODE_ESC);
		  Dynarr_add (dst, c2 - 32 + '0');
		}
#endif /* (not) ENABLE_COMPOSITE_CHARS */
	    }

	  /* ---------------------------------------------------- */
	  /* 3. Do we need to represent as UTF-8?                 */
	  /* ---------------------------------------------------- */

	  /* If no ISO2022-compatible charset found, we must encode as
	     UTF-8 */
	  else if (NILP (charset) ||
		   /* This happens when non-Unicode-internal and the
		      character is stored in the buffer using an
		      encodable but non-ISO2022-compatible charset,
		      e.g. jit-ucs-charset-0 */
		   (!NILP (charset) &&
		    !charset_iso2022_compatible (charset)))
	    {
	      /* If the character set is to be encoded as UTF-8, the
		 escape is always the same. */
	      if (!(flags & ISO_STATE_UTF_8)) 
		{
		  Dynarr_add (dst, ISO_CODE_ESC);
		  Dynarr_add (dst, '%');
		  Dynarr_add (dst, 'G');
		  flags |= ISO_STATE_UTF_8;
		}

	      {
		int code = ichar_to_unicode (ich, CONVERR_FAIL);
		if (encode_unicode_to_dynarr
		    (code, str, src, dst, UNICODE_UTF_8, 0,
		     XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)) < 0)
		  {
		    ENCODING_ERROR_RETURN_OR_CONTINUE (str, src);
		  }
	      }
	    }

	  /* ---------------------------------------------------- */
	  /* 4. Found ISO-2022 compatible charset/character.      */
	  /* ---------------------------------------------------- */

	  else
	    {
	      /* Now, find the register containing this charset.  If
		 none, put this charset in an appropriate register and
		 output an appropriate escape sequence to designate
		 that the charset is in the register. */

	      int reg;

	      /* End the UTF-8 state. */
	      if (flags & ISO_STATE_UTF_8)
		{
		  Dynarr_add (dst, ISO_CODE_ESC);
		  Dynarr_add (dst, '%');
		  Dynarr_add (dst, '@');
		  flags &= ~(ISO_STATE_UTF_8);
		}

	      ensure_correct_direction (XCHARSET_DIRECTION (charset),
					codesys, dst, &flags, 0);

	      /* Now determine which register to use. */
	      reg = -1;
	      for (i = 0; i < 4; i++)
		{
		  if (EQ (charset, data->charset[i]) ||
		      EQ (charset,
			  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET
			  (codesys, i)))
		    {
		      reg = i;
		      break;
		    }
		}

	      if (reg == -1)
		{
		  if (XCHARSET_GRAPHIC (charset) != 0)
		    {
		      if (!NILP (data->charset[1]) &&
			  (!XCODING_SYSTEM_ISO2022_SEVEN (codesys) ||
			   XCODING_SYSTEM_ISO2022_LOCK_SHIFT (codesys)))
			reg = 1;
		      else if (!NILP (data->charset[2]))
			reg = 2;
		      else if (!NILP (data->charset[3]))
			reg = 3;
		      else
			reg = 0;
		    }
		  else
		    reg = 0;
		}

	      iso2022_designate (charset, reg, str, dst);

	      /* Now invoke that register. */
	      switch (reg)
		{
		case 0:
		  ensure_normal_shift (str, dst);
		  half = 0;
		  break;

		case 1:
		  if (XCODING_SYSTEM_ISO2022_SEVEN (codesys))
		    {
		      ensure_shift_out (str, dst);
		      half = 0;
		    }
		  else
		    half = 1;
		  break;

		case 2:
		  if (XCODING_SYSTEM_ISO2022_SEVEN (str->codesys))
		    {
		      Dynarr_add (dst, ISO_CODE_ESC);
		      Dynarr_add (dst, 'N');
		      half = 0;
		    }
		  else
		    {
		      Dynarr_add (dst, ISO_CODE_SS2);
		      half = 1;
		    }
		  break;

		case 3:
		  if (XCODING_SYSTEM_ISO2022_SEVEN (str->codesys))
		    {
		      Dynarr_add (dst, ISO_CODE_ESC);
		      Dynarr_add (dst, 'O');
		      half = 0;
		    }
		  else
		    {
		      Dynarr_add (dst, ISO_CODE_SS3);
		      half = 1;
		    }
		  break;
		  
		default:
		  ABORT ();
		}
	      {
		int offset = (half == 0 ? 0 : 0x80);
		if (XCHARSET_DIMENSION (charset) == 2)
		  Dynarr_add (dst, (c1 & 127) + offset);
		Dynarr_add (dst, (c2 & 127) + offset);
	      }
	    }
	}
    }

#ifdef ENABLE_COMPOSITE_CHARS
  if (in_composite)
    {
      src = saved_src;
      srcend = saved_srcend;
      in_composite = 0;
      Dynarr_add (dst, ISO_CODE_ESC);
      Dynarr_add (dst, '1'); /* end composing */
      goto back_to_square_n; /* Wheeeeeeeee ..... */
    }
#endif /* ENABLE_COMPOSITE_CHARS */

  if (str->st.eof)
    {
      restore_left_to_right_direction (codesys, dst, &flags, 0);
      ensure_normal_shift (str, dst);
      for (i = 0; i < 4; i++)
	{
	  Lisp_Object initial_charset =
	    XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i);
	  iso2022_designate (initial_charset, i, str, dst);
	}
    }

  data->flags = flags;

  /* Verbum caro factum est! */
  return src - str->src;
}

static Bytecount
iso2022_convert (struct coding_stream *str, const unsigned char *src,
		 Bytecount n, unsigned_char_dynarr *dst)
{
  if (str->direction == CODING_DECODE)
    return iso2022_decode (str, (UExtbyte *) src, n, dst);
  else
    return iso2022_encode (str, (Ibyte *) src, n, dst);
}

static void
iso2022_mark (Lisp_Object codesys)
{
  int i;

  for (i = 0; i < 4; i++)
    mark_object (XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i));
  if (XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys))
    {
      for (i = 0;
	   i < Dynarr_length (XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys));
	   i++)
	{
	  struct charset_conversion_spec *ccs =
	    Dynarr_atp (XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys), i);
	  mark_object (ccs->from_charset);
	  mark_object (ccs->to_charset);
	}
    }
  if (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (codesys))
    {
      for (i = 0;
	   i < Dynarr_length (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (codesys));
	   i++)
	{
	  struct charset_conversion_spec *ccs =
	    Dynarr_atp (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (codesys), i);
	  mark_object (ccs->from_charset);
	  mark_object (ccs->to_charset);
	}
    }
}

static void
iso2022_finalize (Lisp_Object cs)
{
  if (XCODING_SYSTEM_ISO2022_INPUT_CONV (cs))
    {
      Dynarr_free (XCODING_SYSTEM_ISO2022_INPUT_CONV (cs));
      XCODING_SYSTEM_ISO2022_INPUT_CONV (cs) = 0;
    }
  if (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (cs))
    {
      Dynarr_free (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (cs));
      XCODING_SYSTEM_ISO2022_OUTPUT_CONV (cs) = 0;
    }
}

static Lisp_Object
get_valid_iso2022_charset (Lisp_Object value)
{
  Lisp_Object charset = Fget_charset (value);
  if (get_charset_iso2022_type (charset) < 0)
    invalid_argument ("Charset cannot be used with ISO-2022", value);
  return charset;
}
  
/* Given a list of charset conversion specs as specified in a Lisp
   program, parse it into STORE_HERE. */

static void
parse_charset_conversion_specs (charset_conversion_spec_dynarr *store_here,
				Lisp_Object spec_list)
{
  EXTERNAL_LIST_LOOP_2 (car, spec_list)
    {
      Lisp_Object from, to;
      struct charset_conversion_spec spec;

      if (!CONSP (car) || !CONSP (XCDR (car)) || !NILP (XCDR (XCDR (car))))
	invalid_argument ("Invalid charset conversion spec", car);
      from = get_valid_iso2022_charset (XCAR (car));
      to = get_valid_iso2022_charset (XCAR (XCDR (car)));
      if (get_charset_iso2022_type (from) != get_charset_iso2022_type (to))
	invalid_operation_2
	  ("Attempted conversion between different charset types",
	   from, to);
      spec.from_charset = from;
      spec.to_charset = to;

      Dynarr_add (store_here, spec);
    }
}

/* Given a dynarr LOAD_HERE of internally-stored charset conversion
   specs, return the equivalent as the Lisp programmer would see it.

   If LOAD_HERE is 0, return Qnil. */

static Lisp_Object
unparse_charset_conversion_specs (charset_conversion_spec_dynarr *load_here,
				  int names)
{
  int i;
  Lisp_Object result;

  if (!load_here)
    return Qnil;
  for (i = 0, result = Qnil; i < Dynarr_length (load_here); i++)
    {
      struct charset_conversion_spec *ccs = Dynarr_atp (load_here, i);
      if (names)
	result = Fcons (list2 (XCHARSET_NAME (ccs->from_charset),
			       XCHARSET_NAME (ccs->to_charset)), result);
      else
	result = Fcons (list2 (ccs->from_charset, ccs->to_charset), result);
    }

  return Fnreverse (result);
}

static int
iso2022_putprop (Lisp_Object codesys,
		 Lisp_Object key,
		 Lisp_Object value)
{
#define FROB_INITIAL_CHARSET(charset_num)				\
  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, charset_num) =	\
    ((EQ (value, Qt) || EQ (value, Qnil)) ? value :			\
     get_valid_iso2022_charset (value))

  if      (EQ (key, Qcharset_g0)) FROB_INITIAL_CHARSET (0);
  else if (EQ (key, Qcharset_g1)) FROB_INITIAL_CHARSET (1);
  else if (EQ (key, Qcharset_g2)) FROB_INITIAL_CHARSET (2);
  else if (EQ (key, Qcharset_g3)) FROB_INITIAL_CHARSET (3);

#define FROB_FORCE_CHARSET(charset_num) \
  XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (codesys, charset_num) = \
    !NILP (value)

  else if (EQ (key, Qforce_g0_on_output)) FROB_FORCE_CHARSET (0);
  else if (EQ (key, Qforce_g1_on_output)) FROB_FORCE_CHARSET (1);
  else if (EQ (key, Qforce_g2_on_output)) FROB_FORCE_CHARSET (2);
  else if (EQ (key, Qforce_g3_on_output)) FROB_FORCE_CHARSET (3);

#define FROB_BOOLEAN_PROPERTY(prop) \
  XCODING_SYSTEM_ISO2022_##prop (codesys) = !NILP (value)

  else if (EQ (key, Qshort))            FROB_BOOLEAN_PROPERTY (SHORT);
  else if (EQ (key, Qno_ascii_eol))     FROB_BOOLEAN_PROPERTY (NO_ASCII_EOL);
  else if (EQ (key, Qno_ascii_cntl))    FROB_BOOLEAN_PROPERTY (NO_ASCII_CNTL);
  else if (EQ (key, Qseven))            FROB_BOOLEAN_PROPERTY (SEVEN);
  else if (EQ (key, Qlock_shift))       FROB_BOOLEAN_PROPERTY (LOCK_SHIFT);
  else if (EQ (key, Qno_iso6429))       FROB_BOOLEAN_PROPERTY (NO_ISO6429);
  else if (EQ (key, Qescape_quoted))    FROB_BOOLEAN_PROPERTY (ESCAPE_QUOTED);
  else if (EQ (key, Qiso2022_preserve)) FROB_BOOLEAN_PROPERTY (ISO2022_PRESERVE);

  else if (EQ (key, Qinput_charset_conversion))
    {
      XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys) =
	Dynarr_new (charset_conversion_spec);
      parse_charset_conversion_specs
	(XCODING_SYSTEM_ISO2022_INPUT_CONV (codesys), value);
    }
  else if (EQ (key, Qoutput_charset_conversion))
    {
      XCODING_SYSTEM_ISO2022_OUTPUT_CONV (codesys) =
	Dynarr_new (charset_conversion_spec);
      parse_charset_conversion_specs
	(XCODING_SYSTEM_ISO2022_OUTPUT_CONV (codesys), value);
    }
  else
    return 0;

  return 1;
}

#ifdef ENABLE_COMPOSITE_CHARS

static void
iso2022_copy_coding_stream (void *dst, void *src)
{
  struct iso2022_coding_stream *dstdata = (struct iso2022_coding_stream *) dst;
  struct iso2022_coding_stream *srcdata = (struct iso2022_coding_stream *) src;

  if (srcdata->composite_chars)
    {
      dstdata->composite_chars = Dynarr_new (unsigned_char);
      Dynarr_copy (dstdata->composite_chars, srcdata->composite_chars);
    }
}

static void
iso2022_finalize_coding_stream (struct coding_stream *str)
{
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);

  if (data->composite_chars)
    {
      Dynarr_free (data->composite_chars);
      data->composite_chars = 0;
    }
}

#endif /* ENABLE_COMPOSITE_CHARS */

static void
iso2022_init (Lisp_Object codesys)
{
  int i;
  for (i = 0; i < 4; i++)
    XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i) = Qnil;
}

static Lisp_Object
coding_system_charset (Lisp_Object coding_system, int gnum)
{
  Lisp_Object cs
    = XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (coding_system, gnum);

  return CHARSETP (cs) ? XCHARSET_NAME (cs) : Qnil;
}

static Lisp_Object
iso2022_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  if (EQ (prop, Qcharset_g0))
    return coding_system_charset (coding_system, 0);
  else if (EQ (prop, Qcharset_g1))
    return coding_system_charset (coding_system, 1);
  else if (EQ (prop, Qcharset_g2))
    return coding_system_charset (coding_system, 2);
  else if (EQ (prop, Qcharset_g3))
    return coding_system_charset (coding_system, 3);

#define FORCE_CHARSET(charset_num) \
  (XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT \
   (coding_system, charset_num) ? Qt : Qnil)

  else if (EQ (prop, Qforce_g0_on_output))
    return FORCE_CHARSET (0);
  else if (EQ (prop, Qforce_g1_on_output))
    return FORCE_CHARSET (1);
  else if (EQ (prop, Qforce_g2_on_output))
    return FORCE_CHARSET (2);
  else if (EQ (prop, Qforce_g3_on_output))
    return FORCE_CHARSET (3);

#define LISP_BOOLEAN(prop) \
  (XCODING_SYSTEM_ISO2022_##prop (coding_system) ? Qt : Qnil)

  else if (EQ (prop, Qshort))            return LISP_BOOLEAN (SHORT);
  else if (EQ (prop, Qno_ascii_eol))     return LISP_BOOLEAN (NO_ASCII_EOL);
  else if (EQ (prop, Qno_ascii_cntl))    return LISP_BOOLEAN (NO_ASCII_CNTL);
  else if (EQ (prop, Qseven))            return LISP_BOOLEAN (SEVEN);
  else if (EQ (prop, Qlock_shift))       return LISP_BOOLEAN (LOCK_SHIFT);
  else if (EQ (prop, Qno_iso6429))       return LISP_BOOLEAN (NO_ISO6429);
  else if (EQ (prop, Qescape_quoted))    return LISP_BOOLEAN (ESCAPE_QUOTED);
  else if (EQ (prop, Qiso2022_preserve)) return LISP_BOOLEAN (ISO2022_PRESERVE);
  
  else if (EQ (prop, Qinput_charset_conversion))
    return
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_INPUT_CONV (coding_system), 0);
  else if (EQ (prop, Qoutput_charset_conversion))
    return
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (coding_system), 0);
  else
    return Qunbound;
}

static void
iso2022_print (Lisp_Object cs, Lisp_Object printcharfun,
	       int UNUSED (escapeflag))
{
  int i;
  
  write_ascstring (printcharfun, "(");
  for (i = 0; i < 4; i++)
    {
      Lisp_Object charset = coding_system_charset (cs, i);
      if (i > 0)
	write_ascstring (printcharfun, ", ");
      write_fmt_string (printcharfun, "g%d=", i);
      print_internal (CHARSETP (charset) ? XCHARSET_NAME (charset) : charset, printcharfun, 0);
      if (XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (cs, i))
	write_ascstring (printcharfun, "(force)");
    }

#define FROB(prop)					        \
  if (!NILP (iso2022_getprop (cs, prop)))		        \
    {						                \
      write_fmt_string_lisp (printcharfun, ", %s", prop);	\
    }
  
  FROB (Qshort);
  FROB (Qno_ascii_eol);
  FROB (Qno_ascii_cntl);
  FROB (Qseven);
  FROB (Qlock_shift);
  FROB (Qno_iso6429);
  FROB (Qescape_quoted);
  FROB (Qiso2022_preserve);

#undef FROB

  {
    Lisp_Object val =
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_INPUT_CONV (cs), 1);
    if (!NILP (val))
      {
	write_fmt_string_lisp (printcharfun, ", input-charset-conversion=%s", val);
      }
    val =
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (cs), 1);
    if (!NILP (val))
      {
	write_fmt_string_lisp (printcharfun, ", output-charset-conversion=%s", val);
      }
    write_ascstring (printcharfun, ")");
  }
}


/************************************************************************/
/*                           ISO2022 detector                           */
/************************************************************************/

struct iso2022_detector
{
  int initted;
  struct iso2022_coding_stream *iso;
  unsigned int flags;

  /* for keeping temporary track of high-byte groups */
  int high_byte_count;
  unsigned int saw_single_shift_just_now:1;

  /* running state; we set the likelihoods at the end */
  unsigned int seen_high_byte:1;
  unsigned int seen_single_shift:1;
  unsigned int seen_locking_shift:1;
  unsigned int seen_designate:1;
  unsigned int bad_single_byte_sequences;
  unsigned int bad_multibyte_escape_sequences;
  unsigned int good_multibyte_escape_sequences;
  int even_high_byte_groups;
  int longest_even_high_byte;
  int odd_high_byte_groups;
};

static const struct memory_description iso2022_detector_description[] = {
  { XD_BLOCK_PTR, offsetof (struct iso2022_detector, iso),
    1, { &iso2022_coding_stream_description_0 } },
  { XD_END }
};  

DEFINE_DETECTOR_WITH_DESCRIPTION (iso2022);
/* ISO2022 system using only seven-bit bytes, no locking shift */
DEFINE_DETECTOR_CATEGORY (iso2022, iso_7);
/* ISO2022 system using eight-bit bytes, no locking shift, no single shift,
   using designation to switch charsets */
DEFINE_DETECTOR_CATEGORY (iso2022, iso_8_designate);
/* ISO2022 system using eight-bit bytes, no locking shift, no designation
   sequences, one-dimension characters in the upper half. */
DEFINE_DETECTOR_CATEGORY (iso2022, iso_8_1);
/* ISO2022 system using eight-bit bytes, no locking shift, no designation
   sequences, two-dimension characters in the upper half. */
DEFINE_DETECTOR_CATEGORY (iso2022, iso_8_2);
/* ISO2022 system using locking shift */
DEFINE_DETECTOR_CATEGORY (iso2022, iso_lock_shift);

static void
iso2022_detect (struct detection_state *st, const UExtbyte *src,
		Bytecount n)
{
  Bytecount orign = n;
  struct iso2022_detector *data = DETECTION_STATE_DATA (st, iso2022);

  /* #### There are serious deficiencies in the recognition mechanism
     here.  This needs to be much smarter if it's going to cut it.
     The sequence "\xff\x0f" is currently detected as LOCK_SHIFT while
     it should be detected as Latin-1.
     All the ISO2022 stuff in this file should be synced up with the
     code from FSF Emacs-21.0, in which Mule should be more or less stable.
     Perhaps we should wait till R2L works in FSF Emacs? */

  /* We keep track of running state on our own, and set the categories at the
     end; that way we can reflect the correct state each time we finish, but
     not get confused by those results the next time around. */

  if (!data->initted)
    {
      xzero (*data);
      data->iso = xnew_and_zero (struct iso2022_coding_stream);
      reset_iso2022_decode (Qnil, data->iso);
      data->initted = 1;
    }

  while (n--)
    {
      UExtbyte c = *src++;
      if (c >= 0x80)
	data->seen_high_byte = 1;
      if (c >= 0xA0)
	data->high_byte_count++;
      else
	{
	  if (data->high_byte_count &&
	      !data->saw_single_shift_just_now)
	    {
	      if (data->high_byte_count & 1)
		data->odd_high_byte_groups++;
	      else
		{
		  data->even_high_byte_groups++;
		  if (data->longest_even_high_byte < data->high_byte_count)
		    data->longest_even_high_byte = data->high_byte_count;
		}
	    }
	  data->high_byte_count = 0;
	  data->saw_single_shift_just_now = 0;
	}
      if (!(data->flags & ISO_STATE_ESCAPE)
	  && (byte_c0_p (c) || byte_c1_p (c)))
	{ /* control chars */
	  switch (c)
	    {
	      /* Allow and ignore control characters that you might
		 reasonably see in a text file */
	    case '\r':
	    case '\n':
	    case '\t':
	    case  7: /* bell */
	    case  8: /* backspace */
	    case 11: /* vertical tab */
	    case 12: /* form feed */
	    case 26: /* MS-DOS C-z junk */
	    case 31: /* '^_' -- for info */
	      goto label_continue_loop;

	    default:
	      break;
	    }
	}

      if ((data->flags & ISO_STATE_ESCAPE) || byte_c0_p (c)
          || byte_c1_p (c))
	{
	  switch (parse_iso2022_esc (Qnil, data->iso, c,
				     &data->flags, 0))
	    {
	    case 1: /* done */
	      if (data->iso->esc_bytes_index > 0)
		data->good_multibyte_escape_sequences++;
	      switch (data->iso->esc)
		{
		case ISO_ESC_DESIGNATE:
		  data->seen_designate = 1;
		  break;
		case ISO_ESC_LOCKING_SHIFT:
		  data->seen_locking_shift = 1;
		  break;
		case ISO_ESC_SINGLE_SHIFT:
		  data->saw_single_shift_just_now = 1;
		  data->seen_single_shift = 1;
		  break;
		default:
		  break;
		}
	      break;

	    case -1: /* not done */
	      break;
	      
	    case 0: /* error */
	      if (data->iso->esc == ISO_ESC_NOTHING)
		data->bad_single_byte_sequences++;
	      else
		data->bad_multibyte_escape_sequences++;
	    }
	}
    label_continue_loop:;
    }

  if (data->high_byte_count &&
      !data->saw_single_shift_just_now)
    {
      if (data->high_byte_count & 1)
	data->odd_high_byte_groups++;
      else
	{
	  data->even_high_byte_groups++;
	  if (data->longest_even_high_byte < data->high_byte_count)
	    data->longest_even_high_byte = data->high_byte_count;
	}
    }

  if (data->bad_multibyte_escape_sequences > 2 ||
      (data->bad_multibyte_escape_sequences > 0 &&
       data->good_multibyte_escape_sequences /
       data->bad_multibyte_escape_sequences < 10))
    /* Just making it up ... */
    SET_DET_RESULTS (st, iso2022, DET_NEARLY_IMPOSSIBLE);
  else if (data->bad_single_byte_sequences > 5 ||
	   (data->bad_single_byte_sequences > 0 &&
	    (data->good_multibyte_escape_sequences +
	     data->even_high_byte_groups +
	     data->odd_high_byte_groups) /
	    data->bad_single_byte_sequences < 10))
    SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
  else if (data->seen_locking_shift)
    {
      SET_DET_RESULTS (st, iso2022, DET_QUITE_IMPROBABLE);
      DET_RESULT (st, iso_lock_shift) = DET_QUITE_PROBABLE;
    }
  else if (!data->seen_high_byte)
    {
      SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
      if (data->good_multibyte_escape_sequences)
	DET_RESULT (st, iso_7) = DET_QUITE_PROBABLE;
      else if (data->seen_single_shift)
	DET_RESULT (st, iso_7) = DET_SOMEWHAT_LIKELY;
      else
	{
	  /* If we've just seen pure 7-bit data, no escape sequences,
	     then we can't give much likelihood; but if we've seen enough
	     of this data, we can assume some unlikelihood of any 8-bit
	     encoding */
	  if (orign + st->bytes_seen >= 1000)
	    DET_RESULT (st, iso_7) = DET_AS_LIKELY_AS_UNLIKELY;
	  else
	    SET_DET_RESULTS (st, iso2022, DET_AS_LIKELY_AS_UNLIKELY);
	}
    }
  else if (data->seen_designate)
    {
      SET_DET_RESULTS (st, iso2022, DET_QUITE_IMPROBABLE);
      if (data->seen_single_shift)
	/* #### Does this really make sense? */
	DET_RESULT (st, iso_8_designate) = DET_SOMEWHAT_UNLIKELY;
      else
	DET_RESULT (st, iso_8_designate) = DET_QUITE_PROBABLE;
    }
  else if (data->odd_high_byte_groups > 0 &&
	   data->even_high_byte_groups == 0)
    {
      SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
      if (data->seen_single_shift)
	DET_RESULT (st, iso_8_1) = DET_QUITE_PROBABLE;
      else
	DET_RESULT (st, iso_8_1) = DET_SOMEWHAT_LIKELY;
    }
  else if (data->odd_high_byte_groups == 0 &&
	   data->even_high_byte_groups > 0)
    {
#if 0
      SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
      if (data->even_high_byte_groups > 10)
	{
	  if (data->seen_single_shift)
	    DET_RESULT (st, iso_8_2) = DET_QUITE_PROBABLE;
	  else
	    DET_RESULT (st, iso_8_2) = DET_SOMEWHAT_LIKELY;
	  if (data->even_high_byte_groups < 50)
	    DET_RESULT (st, iso_8_1) = DET_SOMEWHAT_UNLIKELY;
	  /* else it stays at quite improbable */
	}
#else
      SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
      if (data->seen_single_shift)
	DET_RESULT (st, iso_8_2) = DET_QUITE_PROBABLE;
      else if (data->even_high_byte_groups > 10)
	DET_RESULT (st, iso_8_2) = DET_SOMEWHAT_LIKELY;
      else if (data->longest_even_high_byte > 6)
	DET_RESULT (st, iso_8_2) = DET_SLIGHTLY_LIKELY;
#endif
    }
  else if (data->odd_high_byte_groups > 0 &&
	   data->even_high_byte_groups > 0)
    {
      /* Well, this could be a Latin-1 text, with most high-byte
	 characters single, but sometimes two are together, though
	 this happens not as often. This is common for Western
	 European languages like German, French, Danish, Swedish, etc.
	 Then we would either have a rather small file and
	 even_high_byte_groups would be low.
	 Or we would have a larger file and the ratio of odd to even
	 groups would be very high. */
      SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
      if (data->even_high_byte_groups <= 3 ||
	  data->odd_high_byte_groups >= 10 * data->even_high_byte_groups)
	DET_RESULT (st, iso_8_1) = DET_SOMEWHAT_LIKELY;
    }
  else
    SET_DET_RESULTS (st, iso2022, DET_AS_LIKELY_AS_UNLIKELY);
}      

static void
iso2022_mark_detection_state (struct detection_state *st)
{
  struct iso2022_detector *data = DETECTION_STATE_DATA (st, iso2022);
  iso2022_mark_iso2022_coding_stream (data->iso);
}

static void
iso2022_finalize_detection_state (struct detection_state *st)
{
  struct iso2022_detector *data = DETECTION_STATE_DATA (st, iso2022);
  if (data->iso)
    {
      xfree (data->iso);
      data->iso = 0;
    }
}


/************************************************************************/
/*                               CCL methods                            */
/************************************************************************/

/* Converter written in CCL. */

struct ccl_coding_system
{
  /* For a CCL coding system, these specify the CCL programs used for
     decoding (input) and encoding (output). */
  Lisp_Object decode;
  Lisp_Object encode;
};

#define CODING_SYSTEM_CCL_DECODE(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, ccl)->decode)
#define CODING_SYSTEM_CCL_ENCODE(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, ccl)->encode)
#define XCODING_SYSTEM_CCL_DECODE(codesys) \
  CODING_SYSTEM_CCL_DECODE (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_CCL_ENCODE(codesys) \
  CODING_SYSTEM_CCL_ENCODE (XCODING_SYSTEM (codesys))

struct ccl_coding_stream
{
  /* state of the running CCL program */
  struct ccl_program ccl;
};

static const struct memory_description ccl_coding_system_description[] = {
  { XD_LISP_OBJECT, offsetof (struct ccl_coding_system, decode) },
  { XD_LISP_OBJECT, offsetof (struct ccl_coding_system, encode) },
  { XD_END }
};

static const struct memory_description ccl_coding_stream_description[] = {
  { XD_BLOCK_ARRAY, offsetof (struct ccl_coding_stream, ccl),
    1, { &ccl_program_description } },
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (ccl);

static void
ccl_mark (Lisp_Object codesys)
{
  mark_object (XCODING_SYSTEM_CCL_DECODE (codesys));
  mark_object (XCODING_SYSTEM_CCL_ENCODE (codesys));
}

static Bytecount
ccl_convert (struct coding_stream *str, const unsigned char *src,
	     Bytecount n, unsigned_char_dynarr *dst)
{
  struct ccl_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, ccl);
  Bytecount orign = n;

  data->ccl.last_block = str->st.eof;
  /* When applying a CCL program to a stream, SRC must not be NULL -- this
     is a special signal to the driver that read and write operations are
     not allowed.  The code does not actually look at what SRC points to if
     N == 0.
     */
  ccl_driver (&data->ccl, src ? src : (const unsigned char *) "",
	      /* @@#### current_buffer dependency */
	      current_buffer, dst, n, 0,
	      str->direction == CODING_DECODE ? CCL_MODE_DECODING :
	      CCL_MODE_ENCODING);
  return orign;
}

static void
ccl_init_coding_stream (struct coding_stream *str)
{
  struct ccl_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, ccl);

  setup_ccl_program (&data->ccl,
		     str->direction == CODING_DECODE ?
		     XCODING_SYSTEM_CCL_DECODE (str->codesys) :
		     XCODING_SYSTEM_CCL_ENCODE (str->codesys));
}

static void
ccl_mark_coding_stream (struct coding_stream *str)
{
  struct ccl_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, ccl);

  mark_ccl_program (&data->ccl);
}

static void
ccl_init (Lisp_Object codesys)
{
  XCODING_SYSTEM_CCL_DECODE (codesys) = Qnil;
  XCODING_SYSTEM_CCL_ENCODE (codesys) = Qnil;
}

static int
ccl_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  if (EQ (key, Qdecode))
    XCODING_SYSTEM_CCL_DECODE (codesys) = get_ccl_program (value);
  else if (EQ (key, Qencode))
    XCODING_SYSTEM_CCL_ENCODE (codesys) = get_ccl_program (value);
  return 1;
}

static Lisp_Object
ccl_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  if (EQ (prop, Qdecode))
    return XCODING_SYSTEM_CCL_DECODE (coding_system);
  else if (EQ (prop, Qencode))
    return XCODING_SYSTEM_CCL_ENCODE (coding_system);
  else
    return Qunbound;
}


/************************************************************************/
/*                             Initialization                           */
/************************************************************************/

void
syms_of_mule_coding (void)
{
  DEFSUBR (Fdecode_shift_jis_char);
  DEFSUBR (Fencode_shift_jis_char);
  DEFSUBR (Fdecode_big5_char);
  DEFSUBR (Fencode_big5_char);

  DEFSYMBOL (Qbig5);
  DEFSYMBOL (Qshift_jis);
  DEFSYMBOL (Qccl);
  DEFSYMBOL (Qmultibyte);

  DEFSYMBOL (Qcharset_g0);
  DEFSYMBOL (Qcharset_g1);
  DEFSYMBOL (Qcharset_g2);
  DEFSYMBOL (Qcharset_g3);
  DEFSYMBOL (Qforce_g0_on_output);
  DEFSYMBOL (Qforce_g1_on_output);
  DEFSYMBOL (Qforce_g2_on_output);
  DEFSYMBOL (Qforce_g3_on_output);
  DEFSYMBOL (Qinput_charset_conversion);
  DEFSYMBOL (Qoutput_charset_conversion);

  DEFSYMBOL (Qshort);
  DEFSYMBOL (Qno_ascii_eol);
  DEFSYMBOL (Qno_ascii_cntl);
  DEFSYMBOL (Qlock_shift);
  DEFSYMBOL (Qno_iso6429);
  DEFSYMBOL (Qiso2022_preserve);

  DEFSYMBOL (Qiso_7);
  DEFSYMBOL (Qiso_8_designate);
  DEFSYMBOL (Qiso_8_1);
  DEFSYMBOL (Qiso_8_2);
  DEFSYMBOL (Qiso_lock_shift);

  DEFSYMBOL (Qcharsets);
}

void
coding_system_type_create_mule_coding (void)
{
  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (multibyte, "multibyte-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (multibyte, convert);
  CODING_SYSTEM_HAS_METHOD (multibyte, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (multibyte, mark_coding_stream);
  CODING_SYSTEM_HAS_METHOD (multibyte, init);
  CODING_SYSTEM_HAS_METHOD (multibyte, mark);
  CODING_SYSTEM_HAS_METHOD (multibyte, finalize);
  CODING_SYSTEM_HAS_METHOD (multibyte, putprop);
  CODING_SYSTEM_HAS_METHOD (multibyte, getprop);
  CODING_SYSTEM_HAS_METHOD (multibyte, print);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (iso2022, "iso2022-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (iso2022, mark);
  CODING_SYSTEM_HAS_METHOD (iso2022, convert);
  CODING_SYSTEM_HAS_METHOD (iso2022, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (iso2022, mark_coding_stream);
#ifdef ENABLE_COMPOSITE_CHARS
  CODING_SYSTEM_HAS_METHOD (iso2022, finalize_coding_stream);
  CODING_SYSTEM_HAS_METHOD (iso2022, copy_coding_stream);
#endif /* ENABLE_COMPOSITE_CHARS */
  CODING_SYSTEM_HAS_METHOD (iso2022, init);
  CODING_SYSTEM_HAS_METHOD (iso2022, print);
  CODING_SYSTEM_HAS_METHOD (iso2022, finalize);
  CODING_SYSTEM_HAS_METHOD (iso2022, putprop);
  CODING_SYSTEM_HAS_METHOD (iso2022, getprop);

  INITIALIZE_DETECTOR_WITH_DESCRIPTION (iso2022);
  DETECTOR_HAS_METHOD (iso2022, detect);
  DETECTOR_HAS_METHOD (iso2022, finalize_detection_state);
  DETECTOR_HAS_METHOD (iso2022, mark_detection_state);
  INITIALIZE_DETECTOR_CATEGORY (iso2022, iso_7);
  INITIALIZE_DETECTOR_CATEGORY (iso2022, iso_8_designate);
  INITIALIZE_DETECTOR_CATEGORY (iso2022, iso_8_1);
  INITIALIZE_DETECTOR_CATEGORY (iso2022, iso_8_2);
  INITIALIZE_DETECTOR_CATEGORY (iso2022, iso_lock_shift);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (ccl, "ccl-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (ccl, mark);
  CODING_SYSTEM_HAS_METHOD (ccl, convert);
  CODING_SYSTEM_HAS_METHOD (ccl, init);
  CODING_SYSTEM_HAS_METHOD (ccl, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (ccl, mark_coding_stream);
  CODING_SYSTEM_HAS_METHOD (ccl, putprop);
  CODING_SYSTEM_HAS_METHOD (ccl, getprop);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (shift_jis,
					   "shift-jis-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (shift_jis, convert);
  CODING_SYSTEM_HAS_METHOD (shift_jis, init_coding_stream);

  INITIALIZE_DETECTOR (shift_jis);
  DETECTOR_HAS_METHOD (shift_jis, detect);
  INITIALIZE_DETECTOR_CATEGORY (shift_jis, shift_jis);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (big5, "big5-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (big5, convert);
  CODING_SYSTEM_HAS_METHOD (big5, init_coding_stream);

  INITIALIZE_DETECTOR (big5);
  DETECTOR_HAS_METHOD (big5, detect);
  INITIALIZE_DETECTOR_CATEGORY (big5, big5);
}

void
reinit_coding_system_type_create_mule_coding (void)
{
  REINITIALIZE_CODING_SYSTEM_TYPE (iso2022);
  REINITIALIZE_CODING_SYSTEM_TYPE (ccl);
  REINITIALIZE_CODING_SYSTEM_TYPE (shift_jis);
  REINITIALIZE_CODING_SYSTEM_TYPE (big5);
  REINITIALIZE_DETECTOR_WITH_DESCRIPTION (iso2022);
}

void
reinit_vars_of_mule_coding (void)
{
}

void
vars_of_mule_coding (void)
{
  Vshift_jis_precedence = Qnil;
  staticpro (&Vshift_jis_precedence);

  Vbig5_precedence = Qnil;
  staticpro (&Vshift_jis_precedence);
}

void
complex_vars_of_mule_coding (void)
{
}

void
init_mule_coding (void)
{
  /* #### Hack!  There should be a general multibyte codec to handle both
     of these, and similar variants. */
  assert (initialized);
  Vshift_jis_precedence =
    simple_convert_predence_list_to_array
    (list3 (Vcharset_japanese_jisx0208, Vcharset_japanese_jisx0208_1978,
	    Vcharset_katakana_jisx0201));

#ifdef UNICODE_INTERNAL
  Vbig5_precedence =
    simple_convert_predence_list_to_array
    (list1 (Vcharset_chinese_big5));
#else /* not UNICODE_INTERNAL */
  Vbig5_precedence =
    simple_convert_predence_list_to_array
    (list2 (Vcharset_chinese_big5_1, Vcharset_chinese_big5_2));
#endif /* UNICODE_INTERNAL */
}
