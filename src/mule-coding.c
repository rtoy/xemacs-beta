/* Conversion functions for I18N encodings, but not Unicode (in separate file).
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

/* Synched up with: Mule 2.3.   Not in FSF. */

/* For previous history, see file-coding.c.

   September 10, 2001: Extracted from file-coding.c by Ben Wing.

   Later in September: Finished abstraction of detection system, rewrote
   all the detectors to include multiple levels of likelihood.
*/

#include <config.h>
#include "lisp.h"

#include "charset.h"
#include "mule-ccl.h"
#include "file-coding.h"

Lisp_Object Qshift_jis, Qiso2022, Qbig5, Qccl;

Lisp_Object Qcharset_g0, Qcharset_g1, Qcharset_g2, Qcharset_g3;
Lisp_Object Qforce_g0_on_output, Qforce_g1_on_output;
Lisp_Object Qforce_g2_on_output, Qforce_g3_on_output;
Lisp_Object Qno_iso6429;
Lisp_Object Qinput_charset_conversion, Qoutput_charset_conversion;
Lisp_Object Qshort, Qno_ascii_eol, Qno_ascii_cntl, Qseven, Qlock_shift;

Lisp_Object Qiso_7, Qiso_8_designate, Qiso_8_1, Qiso_8_2, Qiso_lock_shift;


/************************************************************************/
/*                          Shift-JIS methods                           */
/************************************************************************/

/* Shift-JIS; Hankaku (half-width) KANA is also supported. */
DEFINE_CODING_SYSTEM_TYPE (shift_jis);

/* Shift-JIS is a coding system encoding three character sets: ASCII, right
   half of JISX0201-Kana, and JISX0208.  An ASCII character is encoded
   as is.  A character of JISX0201-Kana (DIMENSION1_CHARS94 character set) is
   encoded by "position-code + 0x80".  A character of JISX0208
   (DIMENSION2_CHARS94 character set) is encoded in 2-byte but two
   position-codes are divided and shifted so that it fit in the range
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

#define BYTE_SHIFT_JIS_TWO_BYTE_1_P(c) \
  (((c) >= 0x81 && (c) <= 0x9F) || ((c) >= 0xE0 && (c) <= 0xEF))

/* Is this the second byte of a Shift-JIS two-byte char? */

#define BYTE_SHIFT_JIS_TWO_BYTE_2_P(c) \
  (((c) >= 0x40 && (c) <= 0x7E) || ((c) >= 0x80 && (c) <= 0xFC))

#define BYTE_SHIFT_JIS_KATAKANA_P(c)	\
  ((c) >= 0xA1 && (c) <= 0xDF)

/* Convert Shift-JIS data to internal format. */

static Bytecount
shift_jis_convert (struct coding_stream *str, const UExtbyte *src,
		   unsigned_char_dynarr *dst, Bytecount n)
{
  unsigned int ch     = str->ch;
  Bytecount orign = n;

  if (str->direction == CODING_DECODE)
    {
      while (n--)
	{
	  UExtbyte c = *src++;

	  if (ch)
	    {
	      /* Previous character was first byte of Shift-JIS Kanji char. */
	      if (BYTE_SHIFT_JIS_TWO_BYTE_2_P (c))
		{
		  Intbyte e1, e2;

		  Dynarr_add (dst, LEADING_BYTE_JAPANESE_JISX0208);
		  DECODE_SHIFT_JIS (ch, c, e1, e2);
		  Dynarr_add (dst, e1);
		  Dynarr_add (dst, e2);
		}
	      else
		{
		  DECODE_ADD_BINARY_CHAR (ch, dst);
		  DECODE_ADD_BINARY_CHAR (c, dst);
		}
	      ch = 0;
	    }
	  else
	    {
	      if (BYTE_SHIFT_JIS_TWO_BYTE_1_P (c))
		ch = c;
	      else if (BYTE_SHIFT_JIS_KATAKANA_P (c))
		{
		  Dynarr_add (dst, LEADING_BYTE_KATAKANA_JISX0201);
		  Dynarr_add (dst, c);
		}
	      else
		DECODE_ADD_BINARY_CHAR (c, dst);
	    }
	}

      if (str->eof)
	DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
    }
  else
    {
      while (n--)
	{
	  Intbyte c = *src++;
	  if (BYTE_ASCII_P (c))
	    {
	      Dynarr_add (dst, c);
	      ch = 0;
	    }
	  else if (INTBYTE_LEADING_BYTE_P (c))
	    ch = (c == LEADING_BYTE_KATAKANA_JISX0201 ||
		  c == LEADING_BYTE_JAPANESE_JISX0208_1978 ||
		  c == LEADING_BYTE_JAPANESE_JISX0208) ? c : 0;
	  else if (ch)
	    {
	      if (ch == LEADING_BYTE_KATAKANA_JISX0201)
		{
		  Dynarr_add (dst, c);
		  ch = 0;
		}
	      else if (ch == LEADING_BYTE_JAPANESE_JISX0208_1978 ||
		       ch == LEADING_BYTE_JAPANESE_JISX0208)
		ch = c;
	      else
		{
		  UExtbyte j1, j2;
		  ENCODE_SHIFT_JIS (ch, c, j1, j2);
		  Dynarr_add (dst, j1);
		  Dynarr_add (dst, j2);
		  ch = 0;
		}
	    }
	}
    }
  
  str->ch    = ch;

  return orign;
}

DEFUN ("decode-shift-jis-char", Fdecode_shift_jis_char, 1, 1, 0, /*
Decode a JISX0208 character of Shift-JIS coding-system.
CODE is the character code in Shift-JIS as a cons of type bytes.
Return the corresponding character.
*/
       (code))
{
  int c1, c2, s1, s2;

  CHECK_CONS (code);
  CHECK_INT (XCAR (code));
  CHECK_INT (XCDR (code));
  s1 = XINT (XCAR (code));
  s2 = XINT (XCDR (code));
  if (BYTE_SHIFT_JIS_TWO_BYTE_1_P (s1) &&
      BYTE_SHIFT_JIS_TWO_BYTE_2_P (s2))
    {
      DECODE_SHIFT_JIS (s1, s2, c1, c2);
      return make_char (MAKE_CHAR (Vcharset_japanese_jisx0208,
				   c1 & 0x7F, c2 & 0x7F));
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
  BREAKUP_CHAR (XCHAR (character), charset, c1, c2);
  if (EQ (charset, Vcharset_japanese_jisx0208))
    {
      ENCODE_SHIFT_JIS (c1 | 0x80, c2 | 0x80, s1, s2);
      return Fcons (make_int (s1), make_int (s2));
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

/* BIG5 (used for Taiwanese). */
DEFINE_CODING_SYSTEM_TYPE (big5);

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

#define BYTE_BIG5_TWO_BYTE_1_P(c) \
  ((c) >= 0xA1 && (c) <= 0xFE)

/* Is this the second byte of a Shift-JIS two-byte char? */

#define BYTE_BIG5_TWO_BYTE_2_P(c) \
  (((c) >= 0x40 && (c) <= 0x7E) || ((c) >= 0xA1 && (c) <= 0xFE))

/* Number of Big5 characters which have the same code in 1st byte.  */

#define BIG5_SAME_ROW (0xFF - 0xA1 + 0x7F - 0x40)

/* Code conversion macros.  These are macros because they are used in
   inner loops during code conversion.

   Note that temporary variables in macros introduce the classic
   dynamic-scoping problems with variable names.  We use capital-
   lettered variables in the assumption that XEmacs does not use
   capital letters in variables except in a very formalized way
   (e.g. Qstring). */

/* Convert Big5 code (b1, b2) into its internal string representation
   (lb, c1, c2). */

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

#define DECODE_BIG5(b1, b2, lb, c1, c2) do				\
{									\
  int B1 = b1, B2 = b2;							\
  int I									\
    = (B1 - 0xA1) * BIG5_SAME_ROW + B2 - (B2 < 0x7F ? 0x40 : 0x62);	\
									\
  if (B1 < 0xC9)							\
    {									\
      lb = LEADING_BYTE_CHINESE_BIG5_1;					\
    }									\
  else									\
    {									\
      lb = LEADING_BYTE_CHINESE_BIG5_2;					\
      I -= (BIG5_SAME_ROW) * (0xC9 - 0xA1);				\
    }									\
  c1 = I / (0xFF - 0xA1) + 0xA1;					\
  c2 = I % (0xFF - 0xA1) + 0xA1;					\
} while (0)

/* Convert the internal string representation of a Big5 character
   (lb, c1, c2) into Big5 code (b1, b2). */

#define ENCODE_BIG5(lb, c1, c2, b1, b2) do				\
{									\
  int I = ((c1) - 0xA1) * (0xFF - 0xA1) + ((c2) - 0xA1);		\
									\
  if (lb == LEADING_BYTE_CHINESE_BIG5_2)				\
    {									\
      I += BIG5_SAME_ROW * (0xC9 - 0xA1);				\
    }									\
  b1 = I / BIG5_SAME_ROW + 0xA1;					\
  b2 = I % BIG5_SAME_ROW;						\
  b2 += b2 < 0x3F ? 0x40 : 0x62;					\
} while (0)

/* Convert Big5 data to internal format. */

static Bytecount
big5_convert (struct coding_stream *str, const UExtbyte *src,
	      unsigned_char_dynarr *dst, Bytecount n)
{
  unsigned int ch     = str->ch;
  Bytecount orign = n;

  if (str->direction == CODING_DECODE)
    {
      while (n--)
	{
	  UExtbyte c = *src++;
	  if (ch)
	    {
	      /* Previous character was first byte of Big5 char. */
	      if (BYTE_BIG5_TWO_BYTE_2_P (c))
		{
		  Intbyte b1, b2, b3;
		  DECODE_BIG5 (ch, c, b1, b2, b3);
		  Dynarr_add (dst, b1);
		  Dynarr_add (dst, b2);
		  Dynarr_add (dst, b3);
		}
	      else
		{
		  DECODE_ADD_BINARY_CHAR (ch, dst);
		  DECODE_ADD_BINARY_CHAR (c, dst);
		}
	      ch = 0;
	    }
	  else
	    {
	      if (BYTE_BIG5_TWO_BYTE_1_P (c))
		ch = c;
	      else
		DECODE_ADD_BINARY_CHAR (c, dst);
	    }
	}

      if (str->eof)
	DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
    }
  else
    {
      while (n--)
	{
	  Intbyte c = *src++;
	  if (BYTE_ASCII_P (c))
	    {
	      /* ASCII. */
	      Dynarr_add (dst, c);
	    }
	  else if (INTBYTE_LEADING_BYTE_P (c))
	    {
	      if (c == LEADING_BYTE_CHINESE_BIG5_1 ||
		  c == LEADING_BYTE_CHINESE_BIG5_2)
		{
		  /* A recognized leading byte. */
		  ch = c;
		  continue;	/* not done with this character. */
		}
	      /* otherwise just ignore this character. */
	    }
	  else if (ch == LEADING_BYTE_CHINESE_BIG5_1 ||
		   ch == LEADING_BYTE_CHINESE_BIG5_2)
	    {
	      /* Previous char was a recognized leading byte. */
	      ch = (ch << 8) | c;
	      continue;		/* not done with this character. */
	    }
	  else if (ch)
	    {
	      /* Encountering second byte of a Big5 character. */
	      UExtbyte b1, b2;

	      ENCODE_BIG5 (ch >> 8, ch & 0xFF, c, b1, b2);
	      Dynarr_add (dst, b1);
	      Dynarr_add (dst, b2);
	    }

	  ch = 0;
	}
    }

  str->ch    = ch;
  return orign;
}

Emchar
decode_big5_char (int b1, int b2)
{
  if (BYTE_BIG5_TWO_BYTE_1_P (b1) &&
      BYTE_BIG5_TWO_BYTE_2_P (b2))
    {
      int leading_byte;
      Lisp_Object charset;
      int c1, c2;

      DECODE_BIG5 (b1, b2, leading_byte, c1, c2);
      charset = CHARSET_BY_LEADING_BYTE (leading_byte);
      return MAKE_CHAR (charset, c1 & 0x7F, c2 & 0x7F);
    }
  else
    return -1;
}

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
  Emchar ch;

  CHECK_CONS (code);
  CHECK_INT (XCAR (code));
  CHECK_INT (XCDR (code));
  ch = decode_big5_char (XINT (XCAR (code)), XINT (XCDR (code)));
  if (ch == -1)
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
  int c1, c2, b1, b2;

  CHECK_CHAR_COERCE_INT (character);
  BREAKUP_CHAR (XCHAR (character), charset, c1, c2);
  if (EQ (charset, Vcharset_chinese_big5_1) ||
      EQ (charset, Vcharset_chinese_big5_2))
    {
      ENCODE_BIG5 (XCHARSET_LEADING_BYTE (charset), c1 | 0x80, c2 | 0x80,
		   b1, b2);
      return Fcons (make_int (b1), make_int (b2));
    }
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
	  if ((c >= 0x40 && c <= 0x7E) || (c >= 0xA1 && c <= 0xFE))
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
  else
    DET_RESULT (st, big5) = DET_AS_LIKELY_AS_UNLIKELY;
}


/************************************************************************/
/*                           ISO2022 methods                            */
/************************************************************************/

/* Any ISO-2022-compliant coding system.  Includes JIS, EUC, CTEXT
   (Compound Text, the encoding of selections in X Windows).  See below for
   a complete description of ISO-2022. */
DEFINE_CODING_SYSTEM_TYPE (iso2022);

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
  ISO_ESC_2_8,		/* We've seen ESC 0x28, i.e. ESC (.
			   This means designate a 94-character
			   character set into G0. */
  ISO_ESC_2_9,		/* We've seen ESC 0x29 -- designate a
			   94-character character set into G1. */
  ISO_ESC_2_10,		/* We've seen ESC 0x2A. */
  ISO_ESC_2_11,		/* We've seen ESC 0x2B. */
  ISO_ESC_2_12,		/* We've seen ESC 0x2C -- designate a
			   96-character character set into G0.
			   (This is not ISO2022-standard.
			   The following 96-character
			   control sequences are standard,
			   though.) */
  ISO_ESC_2_13,		/* We've seen ESC 0x2D -- designate a
			   96-character character set into G1.
			   */
  ISO_ESC_2_14,		/* We've seen ESC 0x2E. */
  ISO_ESC_2_15,		/* We've seen ESC 0x2F. */
  ISO_ESC_2_4_8,	/* We've seen ESC $ 0x28 -- designate
			   a 94^N character set into G0. */
  ISO_ESC_2_4_9,	/* We've seen ESC $ 0x29. */
  ISO_ESC_2_4_10,	/* We've seen ESC $ 0x2A. */
  ISO_ESC_2_4_11,	/* We've seen ESC $ 0x2B. */
  ISO_ESC_2_4_12,	/* We've seen ESC $ 0x2C. */
  ISO_ESC_2_4_13,	/* We've seen ESC $ 0x2D. */
  ISO_ESC_2_4_14,	/* We've seen ESC $ 0x2E. */
  ISO_ESC_2_4_15,	/* We've seen ESC $ 0x2F. */
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

/* ISO_STATE_LOCK is the mask of flags that remain on until explicitly
   turned off when in the ISO2022 encoder/decoder.  Other flags are turned
   off at the end of processing each character or escape sequence. */
# define ISO_STATE_LOCK \
  (ISO_STATE_COMPOSITE | ISO_STATE_R2L)

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
  unsigned char force_charset_on_output[4];

  charset_conversion_spec_dynarr *input_conv;
  charset_conversion_spec_dynarr *output_conv;

  unsigned int shoort		:1; /* C makes you speak Dutch */
  unsigned int no_ascii_eol	:1;
  unsigned int no_ascii_cntl	:1;
  unsigned int seven		:1;
  unsigned int lock_shift	:1;
  unsigned int no_iso6429	:1;
  unsigned int escape_quoted	:1;
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

  /* If we saw an invalid designation sequence for a particular
     register, we flag it here and switch to ASCII.  The next time we
     see a valid designation for this register, we turn off the flag
     and do the designation normally, but pretend the sequence was
     invalid.  The effect of all this is that (most of the time) the
     escape sequences for both the switch to the unknown charset, and
     the switch back to the known charset, get inserted literally into
     the buffer and saved out as such.  The hope is that we can
     preserve the escape sequences so that the resulting written out
     file makes sense.  If we don't do any of this, the designation
     to the invalid charset will be preserved but that switch back
     to the known charset will probably get eaten because it was
     the same charset that was already present in the register. */
  unsigned char invalid_designated[4];

  /* We try to do similar things as above for direction-switching
     sequences.  If we encountered a direction switch while an
     invalid designation was present, or an invalid designation
     just after a direction switch (i.e. no valid designation
     encountered yet), we insert the direction-switch escape
     sequence literally into the output stream, and later on
     insert the corresponding direction-restoring escape sequence
     literally also. */
  unsigned int switched_dir_and_no_valid_charset_yet :1;
  unsigned int invalid_switch_dir :1;

  /* Tells the decoder to output the escape sequence literally
     even though it was valid.  Used in the games we play to
     avoid lossage when we encounter invalid designations. */
  unsigned int output_literally :1;
  /* We encountered a direction switch followed by an invalid
     designation.  We didn't output the direction switch
     literally because we didn't know about the invalid designation;
     but we have to do so now. */
  unsigned int output_direction_sequence :1;

  /**************** for encoding ****************/

  /* Whether we need to explicitly designate the charset in the
     G? register before using it.  It is initialized from the
     array FORCE_CHARSET_ON_OUTPUT in CODESYS. */
  unsigned char force_charset_on_output[4];

  /* Other state variables that need to be preserved across
     invocations. */
  Lisp_Object current_charset;
  int current_half;
  int current_char_boundary;
};

static const struct lrecord_description ccs_description_1[] =
{
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, from_charset) },
  { XD_LISP_OBJECT, offsetof (charset_conversion_spec, to_charset) },
  { XD_END }
};

static const struct struct_description ccs_description =
{
  sizeof (charset_conversion_spec),
  ccs_description_1
};

static const struct lrecord_description ccsd_description_1[] =
{
  XD_DYNARR_DESC (charset_conversion_spec_dynarr, &ccs_description),
  { XD_END }
};

static const struct struct_description ccsd_description =
{
  sizeof (charset_conversion_spec_dynarr),
  ccsd_description_1
};

static const struct lrecord_description iso2022_coding_system_description[] = {
  { XD_LISP_OBJECT_ARRAY,
    coding_system_data_offset + offsetof (struct iso2022_coding_system, 
					  initial_charset), 4 },
  { XD_STRUCT_PTR,
    coding_system_data_offset + offsetof (struct iso2022_coding_system,
					  input_conv),
    1, &ccsd_description },
  { XD_STRUCT_PTR,
    coding_system_data_offset + offsetof (struct iso2022_coding_system,
					  output_conv),
    1, &ccsd_description },
  { XD_END }
};

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
	ESC 4 ALTCHAR [ RULE ALTCHAR ] ESC 0 CHAR [ CHAR ] ESC 1 */

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
}

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
  data->current_charset = Qnil;
  data->current_char_boundary = 1;
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
iso2022_rewind_coding_stream (struct coding_stream *str)
{
  iso2022_init_coding_stream (str);
}

static int
fit_to_be_escape_quoted (unsigned char c)
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
charset_by_attributes_or_create_one (int type, Intbyte final, int dir)
{
  Lisp_Object charset = CHARSET_BY_ATTRIBUTES (type, final, dir);

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
	  abort (); chars = 0; dim = 0;
	}

      charset = Fmake_charset (Qunbound, Qnil,
			       nconc2 (list6 (Qfinal, make_char (final),
					      Qchars, make_int (chars),
					      Qdimension, make_int (dim)),
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

   ********************************************************************

   #### Strategies for error annotation and coding orthogonalization

   We really want to separate out a number of things.  Conceptually,
   there is a nested syntax.

   At the top level is the ISO 2022 extension syntax, including charset
   designation and invocation, and certain auxiliary controls such as the
   ISO 6429 direction specification.  These are octet-oriented, with the
   single exception (AFAIK) of the "exit Unicode" sequence which uses the
   UTF's natural width (1 byte for UTF-7 and UTF-8, 2 bytes for UCS-2 and
   UTF-16, and 4 bytes for UCS-4 and UTF-32).  This will be treated as a
   (deprecated) special case in Unicode processing.

   The middle layer is ISO 2022 character interpretation.  This will depend
   on the current state of the ISO 2022 registers, and assembles octets
   into the character's internal representation.

   The lowest level is translating system control conventions.  At present
   this is restricted to newline translation, but one could imagine doing
   tab conversion or line wrapping here.  "Escape from Unicode" processing
   would be done at this level.

   At each level the parser will verify the syntax.  In the case of a
   syntax error or warning (such as a redundant escape sequence that affects
   no characters), the parser will take some action, typically inserting the
   erroneous octets directly into the output and creating an annotation
   which can be used by higher level I/O to mark the affected region.

   This should make it possible to do something sensible about separating
   newline convention processing from character construction, and about
   preventing ISO 2022 escape sequences from being recognized
   inappropriately.

   The basic strategy will be to have octet classification tables, and
   switch processing according to the table entry.

   It's possible that, by doing the processing with tables of functions or
   the like, the parser can be used for both detection and translation. */

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

  iso->output_literally = 0;
  iso->output_direction_sequence = 0;

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
      /* Various junk here to attempt to preserve the direction sequences
	 literally in the text if they would otherwise be swallowed due
	 to invalid designations that don't show up as actual charset
	 changes in the text. */
      if (iso->invalid_switch_dir)
	{
	  /* We already inserted a direction switch literally into the
	     text.  We assume (#### this may not be right) that the
	     next direction switch is the one going the other way,
	     and we need to output that literally as well. */
	  iso->output_literally = 1;
	  iso->invalid_switch_dir = 0;
	}
      else
	{
	  int jj;

	  /* If we are in the thrall of an invalid designation,
	   then stick the directionality sequence literally into the
	   output stream so it ends up in the original text again. */
	  for (jj = 0; jj < 4; jj++)
	    if (iso->invalid_designated[jj])
	      break;
	  if (jj < 4)
	    {
	      iso->output_literally = 1;
	      iso->invalid_switch_dir = 1;
	    }
	  else
	    /* Indicate that we haven't yet seen a valid designation,
	       so that if a switch-dir is directly followed by an
	       invalid designation, both get inserted literally. */
	    iso->switched_dir_and_no_valid_charset_yet = 1;
	}
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
	    abort ();
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
      abort ();
      /* #### This should never happen now that we automatically create
	 temporary charsets as necessary.  We should probably remove
	 this code. --ben */
      iso->invalid_designated[reg] = 1;
      iso->charset[reg] = Vcharset_ascii;
      iso->esc = ISO_ESC_DESIGNATE;
      *flags &= ISO_STATE_LOCK;
      iso->output_literally = 1;
      if (iso->switched_dir_and_no_valid_charset_yet)
	{
	  /* We encountered a switch-direction followed by an
	     invalid designation.  Ensure that the switch-direction
	     gets outputted; otherwise it will probably get eaten
	     when the text is written out again. */
	  iso->switched_dir_and_no_valid_charset_yet = 0;
	  iso->output_direction_sequence = 1;
	  /* And make sure that the switch-dir going the other
	     way gets outputted, as well. */
	  iso->invalid_switch_dir = 1;
	}
      return 1;
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
  if (iso->invalid_designated[reg])
    {
      iso->invalid_designated[reg] = 0;
      iso->output_literally = 1;
    }
  if (iso->switched_dir_and_no_valid_charset_yet)
    iso->switched_dir_and_no_valid_charset_yet = 0;
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
		unsigned_char_dynarr *dst, Bytecount n)
{
  unsigned int ch     = str->ch;
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
		    Intbyte comstr[MAX_EMCHAR_LEN];
		    Bytecount len;
		    Emchar emch = lookup_composite_char (Dynarr_atp (dst, 0),
							 Dynarr_length (dst));
		    dst = real_dst;
		    len = set_charptr_emchar (comstr, emch);
		    Dynarr_add_many (dst, comstr, len);
		    break;
		  }
#else
		case ISO_ESC_START_COMPOSITE:
		  {
		    Intbyte comstr[MAX_EMCHAR_LEN];
		    Bytecount len;
		    Emchar emch = MAKE_CHAR (Vcharset_composite, c - '0' + ' ',
					     0);
		    len = set_charptr_emchar (comstr, emch);
		    Dynarr_add_many (dst, comstr, len);
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

	  /* Attempted error recovery. */
	  if (data->output_direction_sequence)
	    ensure_correct_direction (flags & ISO_STATE_R2L ?
				      CHARSET_RIGHT_TO_LEFT :
				      CHARSET_LEFT_TO_RIGHT,
				      str->codesys, dst, 0, 1);
	  /* More error recovery. */
	  if (!retval || data->output_literally)
	    {
	      /* Output the (possibly invalid) sequence */
	      int i;
	      for (i = 0; i < data->esc_bytes_index; i++)
		DECODE_ADD_BINARY_CHAR (data->esc_bytes[i], dst);
	      flags &= ISO_STATE_LOCK;
	      if (!retval)
		n++, src--;/* Repeat the loop with the same character. */
	      else
		{
		  /* No sense in reprocessing the final byte of the
		     escape sequence; it could mess things up anyway.
		     Just add it now. */
		  DECODE_ADD_BINARY_CHAR (c, dst);
		}
	    }
	  ch = 0;
	}
      else if (BYTE_C0_P (c) || BYTE_C1_P (c))
	{ /* Control characters */

	  /***** Error-handling *****/

	  /* If we were in the middle of a character, dump out the
	     partial character. */
	  DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);

	  /* If we just saw a single-shift character, dump it out.
	     This may dump out the wrong sort of single-shift character,
	     but least it will give an indication that something went
	     wrong. */
	  if (flags & ISO_STATE_SS2)
	    {
	      DECODE_ADD_BINARY_CHAR (ISO_CODE_SS2, dst);
	      flags &= ~ISO_STATE_SS2;
	    }
	  if (flags & ISO_STATE_SS3)
	    {
	      DECODE_ADD_BINARY_CHAR (ISO_CODE_SS3, dst);
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
	  int lb;
	  int reg;

	  /* Now determine the charset. */
	  reg = ((flags & ISO_STATE_SS2) ? 2
		 : (flags & ISO_STATE_SS3) ? 3
		 : !BYTE_ASCII_P (c) ? data->register_right
		 : data->register_left);
	  charset = data->charset[reg];

	  /* Error checking: */
	  if (! CHARSETP (charset)
	      || data->invalid_designated[reg]
	      || (((c & 0x7F) == ' ' || (c & 0x7F) == ISO_CODE_DEL)
		  && XCHARSET_CHARS (charset) == 94))
	    /* Mrmph.  We are trying to invoke a register that has no
	       or an invalid charset in it, or trying to add a character
	       outside the range of the charset.  Insert that char literally
	       to preserve it for the output. */
	    {
	      DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
	      DECODE_ADD_BINARY_CHAR (c, dst);
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

	      lb = XCHARSET_LEADING_BYTE (charset);
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 1:	/* ASCII */
		  DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
		  Dynarr_add (dst, c & 0x7F);
		  break;

		case 2:	/* one-byte official */
		  DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
		  Dynarr_add (dst, lb);
		  Dynarr_add (dst, c | 0x80);
		  break;

		case 3:	/* one-byte private or two-byte official */
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
		      Dynarr_add (dst, PRE_LEADING_BYTE_PRIVATE_1);
		      Dynarr_add (dst, lb);
		      Dynarr_add (dst, c | 0x80);
		    }
		  else
		    {
		      if (ch)
			{
			  Dynarr_add (dst, lb);
			  Dynarr_add (dst, ch | 0x80);
			  Dynarr_add (dst, c | 0x80);
			  ch = 0;
			}
		      else
			ch = c;
		    }
		  break;

		default:	/* two-byte private */
		  if (ch)
		    {
		      Dynarr_add (dst, PRE_LEADING_BYTE_PRIVATE_2);
		      Dynarr_add (dst, lb);
		      Dynarr_add (dst, ch | 0x80);
		      Dynarr_add (dst, c | 0x80);
		      ch = 0;
		    }
		  else
		    ch = c;
		}
	    }

	  if (!ch)
	    flags &= ISO_STATE_LOCK;
	}

    }

  if (str->eof)
    DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);

  data->flags = flags;
  str->ch    = ch;
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
  int type;
  unsigned char final;
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);
  Lisp_Object old_charset = data->charset[reg];

  data->charset[reg] = charset;
  if (!CHARSETP (charset))
    /* charset might be an initial nil or t. */
    return;
  type = XCHARSET_TYPE (charset);
  final = XCHARSET_FINAL (charset);
  if (!data->force_charset_on_output[reg] &&
      CHARSETP (old_charset) &&
      XCHARSET_TYPE (old_charset) == type &&
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
iso2022_encode (struct coding_stream *str, const Intbyte *src,
		unsigned_char_dynarr *dst, Bytecount n)
{
  unsigned char charmask;
  Intbyte c;
  unsigned char char_boundary;
  unsigned int ch             = str->ch;
  Lisp_Object codesys         = str->codesys;
  int i;
  Lisp_Object charset;
  int half;
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);
  unsigned int flags          = data->flags;
  Bytecount orign = n;

#ifdef ENABLE_COMPOSITE_CHARS
  /* flags for handling composite chars.  We do a little switcheroo
     on the source while we're outputting the composite char. */
  Bytecount saved_n = 0;
  const Intbyte *saved_src = NULL;
  int in_composite = 0;
#endif /* ENABLE_COMPOSITE_CHARS */

  char_boundary = data->current_char_boundary;
  charset = data->current_charset;
  half = data->current_half;

#ifdef ENABLE_COMPOSITE_CHARS
 back_to_square_n:
#endif
  while (n--)
    {
      c = *src++;

      if (BYTE_ASCII_P (c))
	{		/* Processing ASCII character */
	  ch = 0;

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
	  char_boundary = 1;
	}

      else if (INTBYTE_LEADING_BYTE_P (c) || INTBYTE_LEADING_BYTE_P (ch))
	{ /* Processing Leading Byte */
	  ch = 0;
	  charset = CHARSET_BY_LEADING_BYTE (c);
	  if (LEADING_BYTE_PREFIX_P (c))
	    ch = c;
	  else if (!EQ (charset, Vcharset_control_1)
		   && !EQ (charset, Vcharset_composite))
	    {
	      int reg;

	      ensure_correct_direction (XCHARSET_DIRECTION (charset),
					codesys, dst, &flags, 0);

	      /* Now determine which register to use. */
	      reg = -1;
	      for (i = 0; i < 4; i++)
		{
		  if (EQ (charset, data->charset[i]) ||
		      EQ (charset,
			  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, i)))
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
		  abort ();
		}
	    }
	  char_boundary = 0;
	}
      else
	{			/* Processing Non-ASCII character */
	  charmask = (half == 0 ? 0x7F : 0xFF);
	  char_boundary = 1;
	  if (EQ (charset, Vcharset_control_1))
	    {
	      if (XCODING_SYSTEM_ISO2022_ESCAPE_QUOTED (codesys)
		  && fit_to_be_escape_quoted (c))
		Dynarr_add (dst, ISO_CODE_ESC);
	      /* you asked for it ... */
	      Dynarr_add (dst, c - 0x20);
	    }
#ifndef ENABLE_COMPOSITE_CHARS
	  else if (EQ (charset, Vcharset_composite))
	    {
	      if (c >= 160 || c <= 164) /* Someone might have stuck in
					   something else */
		{
		  Dynarr_add (dst, ISO_CODE_ESC);
		  Dynarr_add (dst, c - 160 + '0');
		}
	    }
#endif
	  else
	    {
	      switch (XCHARSET_REP_BYTES (charset))
		{
		case 2:
		  Dynarr_add (dst, c & charmask);
		  break;
		case 3:
		  if (XCHARSET_PRIVATE_P (charset))
		    {
		      Dynarr_add (dst, c & charmask);
		      ch = 0;
		    }
		  else if (ch)
		    {
#ifdef ENABLE_COMPOSITE_CHARS
		      if (EQ (charset, Vcharset_composite))
			{
			  if (in_composite)
			    {
			      /* #### Bother! We don't know how to
				 handle this yet. */
			      Dynarr_add (dst, '~');
			    }
			  else
			    {
			      Emchar emch = MAKE_CHAR (Vcharset_composite,
						       ch & 0x7F, c & 0x7F);
			      Lisp_Object lstr = composite_char_string (emch);
			      saved_n = n;
			      saved_src = src;
			      in_composite = 1;
			      src = XSTRING_DATA   (lstr);
			      n   = XSTRING_LENGTH (lstr);
			      Dynarr_add (dst, ISO_CODE_ESC);
			      Dynarr_add (dst, '0'); /* start composing */
			    }
			}
		      else
#endif /* ENABLE_COMPOSITE_CHARS */
			{
			  Dynarr_add (dst, ch & charmask);
			  Dynarr_add (dst, c & charmask);
			}
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		case 4:
		  if (ch)
		    {
		      Dynarr_add (dst, ch & charmask);
		      Dynarr_add (dst, c & charmask);
		      ch = 0;
		    }
		  else
		    {
		      ch = c;
		      char_boundary = 0;
		    }
		  break;
		default:
		  abort ();
		}
	    }
	}
    }

#ifdef ENABLE_COMPOSITE_CHARS
  if (in_composite)
    {
      n = saved_n;
      src = saved_src;
      in_composite = 0;
      Dynarr_add (dst, ISO_CODE_ESC);
      Dynarr_add (dst, '1'); /* end composing */
      goto back_to_square_n; /* Wheeeeeeeee ..... */
    }
#endif /* ENABLE_COMPOSITE_CHARS */

  if (char_boundary && str->eof)
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
  str->ch    = ch;
  data->current_char_boundary = char_boundary;
  data->current_charset = charset;
  data->current_half = half;

  /* Verbum caro factum est! */
  return orign;
}

static Bytecount
iso2022_convert (struct coding_stream *str,
		 const UExtbyte *src,
		 unsigned_char_dynarr *dst, Bytecount n)
{
  if (str->direction == CODING_DECODE)
    return iso2022_decode (str, src, dst, n);
  else
    return iso2022_encode (str, src, dst, n);
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

/* Given a list of charset conversion specs as specified in a Lisp
   program, parse it into STORE_HERE. */

static void
parse_charset_conversion_specs (charset_conversion_spec_dynarr *store_here,
				Lisp_Object spec_list)
{
  Lisp_Object rest;

  EXTERNAL_LIST_LOOP (rest, spec_list)
    {
      Lisp_Object car = XCAR (rest);
      Lisp_Object from, to;
      struct charset_conversion_spec spec;

      if (!CONSP (car) || !CONSP (XCDR (car)) || !NILP (XCDR (XCDR (car))))
	invalid_argument ("Invalid charset conversion spec", car);
      from = Fget_charset (XCAR (car));
      to = Fget_charset (XCAR (XCDR (car)));
      if (XCHARSET_TYPE (from) != XCHARSET_TYPE (to))
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
#define FROB_INITIAL_CHARSET(charset_num) \
  XCODING_SYSTEM_ISO2022_INITIAL_CHARSET (codesys, charset_num) = \
    ((EQ (value, Qt) || EQ (value, Qnil)) ? value : Fget_charset (value))

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

  else if (EQ (key, Qshort))         FROB_BOOLEAN_PROPERTY (SHORT);
  else if (EQ (key, Qno_ascii_eol))  FROB_BOOLEAN_PROPERTY (NO_ASCII_EOL);
  else if (EQ (key, Qno_ascii_cntl)) FROB_BOOLEAN_PROPERTY (NO_ASCII_CNTL);
  else if (EQ (key, Qseven))         FROB_BOOLEAN_PROPERTY (SEVEN);
  else if (EQ (key, Qlock_shift))    FROB_BOOLEAN_PROPERTY (LOCK_SHIFT);
  else if (EQ (key, Qno_iso6429))    FROB_BOOLEAN_PROPERTY (NO_ISO6429);
  else if (EQ (key, Qescape_quoted)) FROB_BOOLEAN_PROPERTY (ESCAPE_QUOTED);

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

static void
iso2022_finalize_coding_stream (struct coding_stream *str)
{
#ifdef ENABLE_COMPOSITE_CHARS
  struct iso2022_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, iso2022);

  if (data->composite_chars)
    Dynarr_free (data->composite_chars);
#endif
}

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

  else if (EQ (prop, Qshort))         return LISP_BOOLEAN (SHORT);
  else if (EQ (prop, Qno_ascii_eol))  return LISP_BOOLEAN (NO_ASCII_EOL);
  else if (EQ (prop, Qno_ascii_cntl)) return LISP_BOOLEAN (NO_ASCII_CNTL);
  else if (EQ (prop, Qseven))         return LISP_BOOLEAN (SEVEN);
  else if (EQ (prop, Qlock_shift))    return LISP_BOOLEAN (LOCK_SHIFT);
  else if (EQ (prop, Qno_iso6429))    return LISP_BOOLEAN (NO_ISO6429);
  else if (EQ (prop, Qescape_quoted)) return LISP_BOOLEAN (ESCAPE_QUOTED);
  
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
iso2022_print (Lisp_Object cs, Lisp_Object printcharfun, int escapeflag)
{
  int i;
  
  write_c_string ("(", printcharfun);
  for (i = 0; i < 4; i++)
    {
      Lisp_Object charset = coding_system_charset (cs, i);
      if (i > 0)
	write_c_string (", ", printcharfun);
      write_fmt_string (printcharfun, "g%d=", i);
      print_internal (CHARSETP (charset) ? XCHARSET_NAME (charset) : charset,
		      printcharfun, 0);
      if (XCODING_SYSTEM_ISO2022_FORCE_CHARSET_ON_OUTPUT (cs, i))
	write_c_string ("(force)", printcharfun);
    }

#define FROB(prop)				\
  if (!NILP (iso2022_getprop (cs, prop)))	\
    {						\
      write_c_string (", ", printcharfun);	\
      print_internal (prop, printcharfun, 0);	\
    }
  
  FROB (Qshort);
  FROB (Qno_ascii_eol);
  FROB (Qno_ascii_cntl);
  FROB (Qseven);
  FROB (Qlock_shift);
  FROB (Qno_iso6429);
  FROB (Qescape_quoted);

  {
    Lisp_Object val =
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_INPUT_CONV (cs), 1);
    if (!NILP (val))
      {
	write_c_string (", input-charset-conversion=", printcharfun);
	print_internal (val, printcharfun, 0);
      }
    val =
      unparse_charset_conversion_specs
      (XCODING_SYSTEM_ISO2022_OUTPUT_CONV (cs), 1);
    if (!NILP (val))
      {
	write_c_string (", output-charset-conversion=", printcharfun);
	print_internal (val, printcharfun, 0);
      }
    write_c_string (")", printcharfun);
  }
}


/************************************************************************/
/*                           ISO2022 detector                           */
/************************************************************************/

DEFINE_DETECTOR (iso2022);
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
  int odd_high_byte_groups;
};

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
		data->even_high_byte_groups++;
	    }
	  data->high_byte_count = 0;
	  data->saw_single_shift_just_now = 0;
	}
      if (!(data->flags & ISO_STATE_ESCAPE)
	  && (BYTE_C0_P (c) || BYTE_C1_P (c)))
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

      if ((data->flags & ISO_STATE_ESCAPE) || BYTE_C0_P (c)
          || BYTE_C1_P (c))
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
    }
  else if (data->odd_high_byte_groups > 0 &&
	   data->even_high_byte_groups > 0)
    SET_DET_RESULTS (st, iso2022, DET_SOMEWHAT_UNLIKELY);
  else
    SET_DET_RESULTS (st, iso2022, DET_AS_LIKELY_AS_UNLIKELY);
}      

static void
iso2022_finalize_detection_state (struct detection_state *st)
{
  struct iso2022_detector *data = DETECTION_STATE_DATA (st, iso2022);
  if (data->iso)
    xfree (data->iso);
}


/************************************************************************/
/*                               CCL methods                            */
/************************************************************************/

/* Converter written in CCL. */
DEFINE_CODING_SYSTEM_TYPE (ccl);

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

static const struct lrecord_description ccl_coding_system_description[] = {
  { XD_LISP_OBJECT,
    coding_system_data_offset + offsetof (struct ccl_coding_system, 
					  decode) },
  { XD_LISP_OBJECT,
    coding_system_data_offset + offsetof (struct ccl_coding_system, 
					  encode) },
  { XD_END }
};

static void
ccl_mark (Lisp_Object codesys)
{
  mark_object (XCODING_SYSTEM_CCL_DECODE (codesys));
  mark_object (XCODING_SYSTEM_CCL_ENCODE (codesys));
}

static Bytecount
ccl_convert (struct coding_stream *str, const UExtbyte *src,
	     unsigned_char_dynarr *dst, Bytecount n)
{
  struct ccl_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, ccl);
  Bytecount orign = n;

  data->ccl.last_block = str->eof;
  /* When applying a CCL program to a stream, SRC must not be NULL -- this
     is a special signal to the driver that read and write operations are
     not allowed.  The code does not actually look at what SRC points to if
     N == 0.
     */
  ccl_driver (&data->ccl, src ? src : (const unsigned char *) "",
	      dst, n, 0,
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
ccl_rewind_coding_stream (struct coding_stream *str)
{
  ccl_init_coding_stream (str);
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
  Lisp_Object sym;
  struct ccl_program test_ccl;
  Char_ASCII *suffix;

  /* Check key first.  */
  if (EQ (key, Qdecode))
    suffix = "-ccl-decode";
  else if (EQ (key, Qencode))
    suffix = "-ccl-encode";
  else
    return 0;

  /* If value is vector, register it as a ccl program
     associated with a newly created symbol for
     backward compatibility.

     #### Bogosity alert!  Do we really have to do this crap???? --ben */
  if (VECTORP (value))
    {
      sym = Fintern (concat2 (Fsymbol_name (XCODING_SYSTEM_NAME (codesys)),
			      build_string (suffix)),
		     Qnil);
      Fregister_ccl_program (sym, value);
    }
  else
    {
      CHECK_SYMBOL (value);
      sym = value;
    }
  /* check if the given ccl programs are valid.  */
  if (setup_ccl_program (&test_ccl, sym) < 0)
    invalid_argument ("Invalid CCL program", value);

  if (EQ (key, Qdecode))
    XCODING_SYSTEM_CCL_DECODE (codesys) = sym;
  else if (EQ (key, Qencode))
    XCODING_SYSTEM_CCL_ENCODE (codesys) = sym;

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
  DEFSYMBOL (Qiso2022);

  DEFSYMBOL (Qcharset_g0);
  DEFSYMBOL (Qcharset_g1);
  DEFSYMBOL (Qcharset_g2);
  DEFSYMBOL (Qcharset_g3);
  DEFSYMBOL (Qforce_g0_on_output);
  DEFSYMBOL (Qforce_g1_on_output);
  DEFSYMBOL (Qforce_g2_on_output);
  DEFSYMBOL (Qforce_g3_on_output);
  DEFSYMBOL (Qno_iso6429);
  DEFSYMBOL (Qinput_charset_conversion);
  DEFSYMBOL (Qoutput_charset_conversion);

  DEFSYMBOL (Qshort);
  DEFSYMBOL (Qno_ascii_eol);
  DEFSYMBOL (Qno_ascii_cntl);
  DEFSYMBOL (Qseven);
  DEFSYMBOL (Qlock_shift);

  DEFSYMBOL (Qiso_7);
  DEFSYMBOL (Qiso_8_designate);
  DEFSYMBOL (Qiso_8_1);
  DEFSYMBOL (Qiso_8_2);
  DEFSYMBOL (Qiso_lock_shift);
}

void
coding_system_type_create_mule_coding (void)
{
  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (iso2022, "iso2022-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (iso2022, mark);
  CODING_SYSTEM_HAS_METHOD (iso2022, convert);
  CODING_SYSTEM_HAS_METHOD (iso2022, finalize_coding_stream);
  CODING_SYSTEM_HAS_METHOD (iso2022, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (iso2022, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (iso2022, init);
  CODING_SYSTEM_HAS_METHOD (iso2022, print);
  CODING_SYSTEM_HAS_METHOD (iso2022, finalize);
  CODING_SYSTEM_HAS_METHOD (iso2022, putprop);
  CODING_SYSTEM_HAS_METHOD (iso2022, getprop);

  INITIALIZE_DETECTOR (iso2022);
  DETECTOR_HAS_METHOD (iso2022, detect);
  DETECTOR_HAS_METHOD (iso2022, finalize_detection_state);
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
  CODING_SYSTEM_HAS_METHOD (ccl, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (ccl, putprop);
  CODING_SYSTEM_HAS_METHOD (ccl, getprop);

  INITIALIZE_CODING_SYSTEM_TYPE (shift_jis, "shift-jis-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (shift_jis, convert);

  INITIALIZE_DETECTOR (shift_jis);
  DETECTOR_HAS_METHOD (shift_jis, detect);
  INITIALIZE_DETECTOR_CATEGORY (shift_jis, shift_jis);

  INITIALIZE_CODING_SYSTEM_TYPE (big5, "big5-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (big5, convert);

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
}

void
reinit_vars_of_mule_coding (void)
{
}

void
vars_of_mule_coding (void)
{
}
