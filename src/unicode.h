/* Copyright (c) 2010 Ben Wing.

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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_unicode_h_
#define INCLUDED_unicode_h_

#ifdef MULE

/************************************************************************/
/*                          Precedence arrays                           */
/************************************************************************/

struct precedence_array
{
  struct LCRECORD_HEADER header;

  /* Encapsulated dynarr listing all the charsets, in order, used for
     converting a Unicode codepoint to a charset codepoint */
  Lisp_Object_dynarr *precdyn;
  /* Have we seen ASCII yet? */
  unsigned int seen_ascii :1;
  /* Does the precedence array have a charset that's higher-precedence than
     ASCII and contains different mappings for any Unicode codepoints in
     the ASCII range?  This flag and the preceding one are used to
     determine whether we can short-circuit ASCII conversion */
  unsigned int has_overriding_ascii :1;
};

DECLARE_LRECORD (precedence_array, struct precedence_array);
#define XPRECEDENCE_ARRAY(x) XRECORD (x, precedence_array, struct precedence_array)
#define wrap_precedence_array(p) wrap_record (p, precedence_array)
#define PRECEDENCE_ARRAYP(x) RECORDP (x, precedence_array)
#define CHECK_PRECEDENCE_ARRAY(x) CHECK_RECORD (x, precedence_array)
#define CONCHECK_PRECEDENCE_ARRAY(x) CONCHECK_RECORD (x, precedence_array)

#define XPRECEDENCE_ARRAY_DYNARR(x) (XPRECEDENCE_ARRAY (x)->precdyn)

#endif /* MULE */

/************************************************************************/
/*                    Unicode error octet characters                    */
/************************************************************************/

/* Where to place the 256 private Unicode codepoints used for encoding
   erroneous octets in a UTF-8 or UTF-16 file.  Note: This MUST be below
   the space used for encoding unknown charset codepoints, which currently
   starts at 0x800000.  See charset_codepoint_to_private_unicode(). */
#define UNICODE_ERROR_OCTET_RANGE_START 0x200000
#define UNICODE_ERROR_OCTET_RANGE_END (UNICODE_ERROR_OCTET_RANGE_START + 0xFF)

DECLARE_INLINE_HEADER (
int
unicode_error_octet_code_p (int code)
)
{
  return (code >= UNICODE_ERROR_OCTET_RANGE_START &&
	  code <= UNICODE_ERROR_OCTET_RANGE_END);
}

#define unicode_error_octet_code_to_octet(code) \
  ((unsigned char) ((code) & 0xFF))

/************************************************************************/
/*                          UTF-16 properties                           */
/************************************************************************/

#define valid_utf_16_first_surrogate(ch) (((ch) & 0xFC00) == 0xD800)
#define valid_utf_16_last_surrogate(ch) (((ch) & 0xFC00) == 0xDC00)
#define valid_utf_16_surrogate(ch) (((ch) & 0xF800) == 0xD800)

/* See the Unicode FAQ, http://www.unicode.org/faq/utf_bom.html#35 for this
   algorithm. 
 
   (They also give another, really verbose one, as part of their explanation
   of the various planes of the encoding, but we won't use that.) */
 
#define UTF_16_LEAD_OFFSET (0xD800 - (0x10000 >> 10))
#define UTF_16_SURROGATE_OFFSET (0x10000 - (0xD800 << 10) - 0xDC00)

#define utf_16_surrogates_to_code(lead, trail) \
  (((lead) << 10) + (trail) + UTF_16_SURROGATE_OFFSET)

#define CODE_TO_UTF_16_SURROGATES(codepoint, lead, trail) do {	\
    int __ctu16s_code = (codepoint);				\
    lead = UTF_16_LEAD_OFFSET + (__ctu16s_code >> 10);		\
    trail = 0xDC00 + (__ctu16s_code & 0x3FF);			\
} while (0)

#endif /* INCLUDED_unicode_h_ */
