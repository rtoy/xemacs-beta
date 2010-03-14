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
  NORMAL_LISP_OBJECT_HEADER header;

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

DECLARE_LISP_OBJECT (precedence_array, struct precedence_array);
#define XPRECEDENCE_ARRAY(x) XRECORD (x, precedence_array, struct precedence_array)
#define wrap_precedence_array(p) wrap_record (p, precedence_array)
#define PRECEDENCE_ARRAYP(x) RECORDP (x, precedence_array)
#define CHECK_PRECEDENCE_ARRAY(x) CHECK_RECORD (x, precedence_array)
#define CONCHECK_PRECEDENCE_ARRAY(x) CONCHECK_RECORD (x, precedence_array)

#define XPRECEDENCE_ARRAY_DYNARR(x) (XPRECEDENCE_ARRAY (x)->precdyn)

#endif /* MULE */

/************************************************************************/
/*                          Encoding/decoding                           */
/************************************************************************/

/* Placed here because it's used in both unicode.c and mule-coding.c
   (ISO-2022 can have embedded UTF-8 in it) */
struct unicode_coding_stream
{
  /* decode */
  int counter;
  int indicated_length;
  int seen_char;
  int first_surrogate;
  int ch;
  /* encode */
  int wrote_bom;
};

enum unicode_encoding_type
{
  UNICODE_UTF_16,
  UNICODE_UTF_8,
  UNICODE_UTF_7,
  UNICODE_UCS_4,
  UNICODE_UTF_32
};

struct coding_stream;

void indicate_invalid_utf_8 (int indicated_length, int counter,
			     int ch, unsigned_char_dynarr *dst,
			     struct unicode_coding_stream *data,
			     int ignore_bom);
int encode_unicode_to_dynarr (int code, struct coding_stream *str,
			      const UExtbyte *src,
			      unsigned_char_dynarr *dst,
			      enum unicode_encoding_type type,
			      int little_endian,
			      int preserve_error_characters);
void decode_utf_8 (struct unicode_coding_stream *data,
		   unsigned_char_dynarr *dst, UExtbyte c, int ignore_bom,
		   int allow_private);
void decode_unicode_to_dynarr (int ucs, unsigned_char_dynarr *dst);


#endif /* INCLUDED_unicode_h_ */
