/* Formatted printing to an output stream. See the documentation for the Lisp
   variable `standard-output'.

   Also, since the emacs `format' language is basically that of C, this file
   makes analogues of the standard C sprintf(), vsprintf(), functions
   available to the XEmacs C code. These provide output to, e.g. malloc'd
   buffers, Lisp strings, fixed buffers and support the repositioning specs,
   necessary for decent internationalization and often not supported by the
   system C library.

   Copyright (C) 1995, 2016 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

   Rewritten by mly to use varargs.h.
   Rewritten from scratch by Ben Wing (February 1995) for Mule; expanded
   to full printf spec.
   Support for bignums, ratios, and bigfloats added April 2004 by Jerry James.
   Rewritten extensively October 2016 by Aidan Kehoe to support signed
   rationals where the base is not 10, direct output to streams, preservation
   of extents, arbitrary padding characters, specific bit lengths, non-ASCII
   digits; to provide C-level rational-to-base conversion to the rest of the
   XEmacs code.

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

/* Synched up with: Rewritten by Ben Wing.  This code has nothing in common
   with the FSF's doprnt.c. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#define EXPOSE_FIXED_BUFFER_INTERNALS
#include "lstream.h"
#include "insdel.h"
#include "lrecord.h"
#include "extents.h"
#include "chartab.h"

/* Conversion to string, radix handling for rationals. */

/* Print an unsigned NUMBER into BUFFER, which comprises SIZE octets, as a
   base (expt 2 POWOF2) string, using TABLE (usually Vfixnum_to_majuscule_map)
   to transform numbers to character values. Start at the *end*, and return
   the length of the string written. Note that this usually means there is
   slack before the actual text wanted. Do not zero-terminate. Throw an
   assertion failure if text checking is turned on and the conversion overruns
   BUFFER.

   See fixnum_to_string() for a more reasonable function for callers. */
static Bytecount
emacs_uint_to_string_rshift_1 (Ibyte *buffer, Bytecount size,
                               EMACS_UINT number, UINT_16_BIT powof2,
                               Lisp_Object table)
{
  Ibyte *end = buffer + size, *cursor = end;
  const Ibyte *this_digit, *ftmdata = XSTRING_DATA (table);
  EMACS_UINT mask = (1 << powof2) - 1;

  text_checking_assert (XSTRING_LENGTH (table) >=
                        ((EMACS_INT) mask + 1) * MAX_ICHAR_LEN);

  while (number)
    {
      this_digit = ftmdata + ((number & mask) * MAX_ICHAR_LEN);
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
      number >>= powof2;
    }

  if (end == cursor)
    {
      this_digit = ftmdata + 0;
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
    }

  text_checking_assert (cursor >= buffer);

  return end - cursor;
}

#define UNSIGNED_TYPE_TO_STRING_GENERAL_1(type, type_downcase)          \
  static Bytecount                                                      \
  type_downcase##_to_string_general_1 (Ibyte *buffer, Bytecount size,   \
                                       type number, UINT_16_BIT radix,  \
                                       Lisp_Object table)               \
  {                                                                     \
    Ibyte *end = buffer + size, *cursor = end, *this_digit;             \
    Ibyte *ftmdata = XSTRING_DATA (table);                              \
                                                                        \
    text_checking_assert (XSTRING_LENGTH (Vfixnum_to_majuscule_map)     \
                          >= (radix) * MAX_ICHAR_LEN);                  \
                                                                        \
    while (number)                                                      \
      {                                                                 \
        this_digit = ftmdata + ((number % radix) * MAX_ICHAR_LEN);      \
        cursor -= itext_ichar_len (this_digit);                         \
        itext_copy_ichar (this_digit, cursor);                          \
        number /= radix;                                                \
      }                                                                 \
                                                                        \
    if (end == cursor)                                                  \
      {                                                                 \
        this_digit = ftmdata + 0;                                       \
        cursor -= itext_ichar_len (this_digit);                         \
        itext_copy_ichar (this_digit, cursor);                          \
      }                                                                 \
                                                                        \
    text_checking_assert (cursor >= buffer);                            \
                                                                        \
    return end - cursor;                                                \
  }

/* Print an unsigned NUMBER into BUFFER, which comprises SIZE octets, as a
   base RADIX string, using TABLE (usually Vfixnum_to_majuscule_map) to
   transform numbers to character values.  Start at the *end*, and return the
   length of the string written. Note that this usually means there is slack
   before the actual text wanted. Do not zero-terminate. Throw an assertion
   failure if text checking is turned on and the conversion overruns BUFFER.

   See fixnum_to_string() for a more reasonable function for callers. */

UNSIGNED_TYPE_TO_STRING_GENERAL_1 (EMACS_UINT, emacs_uint);

#if SIZEOF_EMACS_INT == 8
#define uint_64_bit_to_string_general_1 emacs_uint_to_string_general_1
#else
/* Print an unsigned 64-bit UVAL into BUFFER, which comprises SIZE octets, as
   a base RADIX string, using TABLE (usually Vfixnum_to_majuscule_map) to
   transform numbers to character values.  Start at the *end*, and return the
   length of the string written. Note that this usually means there is slack
   before the actual text wanted. Do not zero-terminate. Throw an assertion
   failure if text checking is turned on and the conversion overruns BUFFER.

   This is used in the unsigned integer handling, for negative integers only.

   See fixnum_to_string() for a more reasonable function for callers. */

UNSIGNED_TYPE_TO_STRING_GENERAL_1 (UINT_64_BIT, uint_64_bit);
#endif

#define ONE_DIGIT(figure) \
  p += itext_copy_ichar (ftmdata + ((n / (figure)) * MAX_ICHAR_LEN), p)
#define ONE_DIGIT_ADVANCE(figure) (ONE_DIGIT (figure), n %= (figure))

#define DIGITS_1(figure) ONE_DIGIT (figure)
#define DIGITS_2(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_1 ((figure) / 10)
#define DIGITS_3(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_2 ((figure) / 10)
#define DIGITS_4(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_3 ((figure) / 10)
#define DIGITS_5(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_4 ((figure) / 10)
#define DIGITS_6(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_5 ((figure) / 10)
#define DIGITS_7(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_6 ((figure) / 10)
#define DIGITS_8(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_7 ((figure) / 10)
#define DIGITS_9(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_8 ((figure) / 10)
#define DIGITS_10(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_9 ((figure) / 10)

/* DIGITS_<11-20> are only used on machines with 64-bit longs. */

#define DIGITS_11(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_10 ((figure) / 10)
#define DIGITS_12(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_11 ((figure) / 10)
#define DIGITS_13(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_12 ((figure) / 10)
#define DIGITS_14(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_13 ((figure) / 10)
#define DIGITS_15(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_14 ((figure) / 10)
#define DIGITS_16(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_15 ((figure) / 10)
#define DIGITS_17(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_16 ((figure) / 10)
#define DIGITS_18(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_17 ((figure) / 10)
#define DIGITS_19(figure) ONE_DIGIT_ADVANCE (figure); DIGITS_18 ((figure) / 10)

/* Print a signed NUMBER to BUFFER in base 10, starting with its most
   significant digit at the beginning, and zero-terminating. SIZE is the
   number of octets comprising BUFFER. TABLE describes a mapping from fixnum
   to characters and is usually Vfixnum_to_majuscule_map. Return length of the
   printed representation, without the zero termination. Throw an assertion
   failure if printing has overrun BUFFER.

   fixnum_to_string_base_10 (buf, sizeof (buf), number,
   Vfixnum_to_majuscule_ascii) is equivalent to snprintf (buf, sizeof (buf),
   "%ld" number), except for behaviour on overrun (snprintf won't throw an
   assertion failure).

   Use the DECIMAL_PRINT_SIZE() macro to size a buffer for
   fixnum_to_string_base_10().

   Adding support for TABLE impacts the speed of this by about 1% on an
   optimized build, compared to the old long_to_string(), as of October
   2016. */
static Bytecount
fixnum_to_string_base_10 (Ibyte *buffer, Bytecount size, Fixnum number,
                          Lisp_Object table)
{
  Ibyte *p = buffer;
  Ibyte *ftmdata = XSTRING_DATA (table);
  /* So -number doesn't fail silently for LONG_MIN. */
  EMACS_UINT n;

  if (number < 0)
    {
      p += set_itext_ichar (p, '-');
      n = -number;
    }
  else
    {
      n = number;
    }

  if      (n < 10)                   { DIGITS_1 (1); }
  else if (n < 100)                  { DIGITS_2 (10); }
  else if (n < 1000)                 { DIGITS_3 (100); }
  else if (n < 10000)                { DIGITS_4 (1000); }
  else if (n < 100000)               { DIGITS_5 (10000); }
  else if (n < 1000000)              { DIGITS_6 (100000); }
  else if (n < 10000000)             { DIGITS_7 (1000000); }
  else if (n < 100000000)            { DIGITS_8 (10000000); }
  else if (n < 1000000000)           { DIGITS_9 (100000000); }
#if SIZEOF_LONG == 4
  /* ``if (1)'' serves only to preserve editor indentation. */
  else if (1)                        { DIGITS_10 (1000000000); }
#else  /* SIZEOF_LONG != 4 */
  else if (n < 10000000000L)         { DIGITS_10 (1000000000L); }
  else if (n < 100000000000L)        { DIGITS_11 (10000000000L); }
  else if (n < 1000000000000L)       { DIGITS_12 (100000000000L); }
  else if (n < 10000000000000L)      { DIGITS_13 (1000000000000L); }
  else if (n < 100000000000000L)     { DIGITS_14 (10000000000000L); }
  else if (n < 1000000000000000L)    { DIGITS_15 (100000000000000L); }
  else if (n < 10000000000000000L)   { DIGITS_16 (1000000000000000L); }
  else if (n < 100000000000000000L)  { DIGITS_17 (10000000000000000L); }
  else if (n < 1000000000000000000L) { DIGITS_18 (100000000000000000L); }
  else                               { DIGITS_19 (1000000000000000000L); }
#endif /* SIZEOF_LONG != 4 */

  *p = '\0';

  text_checking_assert ((p - buffer) <= size);

  return p - buffer;
}

#undef ONE_DIGIT
#undef ONE_DIGIT_ADVANCE

#undef DIGITS_1
#undef DIGITS_2
#undef DIGITS_3
#undef DIGITS_4
#undef DIGITS_5
#undef DIGITS_6
#undef DIGITS_7
#undef DIGITS_8
#undef DIGITS_9
#undef DIGITS_10
#undef DIGITS_11
#undef DIGITS_12
#undef DIGITS_13
#undef DIGITS_14
#undef DIGITS_15
#undef DIGITS_16
#undef DIGITS_17
#undef DIGITS_18
#undef DIGITS_19

/* Print NUMBER, a signed integer, into BUFFER, which comprises SIZE octets,
   as a base RADIX string. The returned number will start with its most
   significant digits at the beginning of BUFFER, and will be zero terminated.

   TABLE_OR_NIL describes a map from fixnums to digits. If Qnil, it defaults
   to Vfixnum_to_majuscule_map. Another useful value is
   Vfixnum_to_majuscule_ascii, to avoid language-specific digit characters
   (e.g. Persian, fullwidth Chinese).

   Return the length of the printed string, without the terminating zero. If
   assertions are enabled, throw an assertion failure if BUFFER overflows.

   See also bignum_to_string(), ratio_to_string(). */
Bytecount
fixnum_to_string (Ibyte *buffer, Bytecount size, Fixnum number,
                  UINT_16_BIT radix, Lisp_Object table_or_nil)
{
  Bytecount len = 0, slack;
  Lisp_Object table
    = NILP (table_or_nil) ? Vfixnum_to_majuscule_map : table_or_nil;
  Boolint minusp;
  EMACS_UINT uval;

  if (10 == radix)
    {
      return fixnum_to_string_base_10 (buffer, size, number, table);
    }

  if ((minusp = number < 0))
    {
      uval = -number;
    }
  else
    {
      uval = number;
    }

  if ((radix & (radix - 1)) == 0) /* Power of two? */
    {
      UINT_16_BIT powof2 = 0;

      text_checking_assert (radix != 0);

      while (radix != 1)
        {
          powof2++;
          radix >>= 1;
        }

      len = emacs_uint_to_string_rshift_1 (buffer, size, uval, powof2,
                                           table);
    }
  else
    {
      len = emacs_uint_to_string_general_1 (buffer, size, uval, radix,
                                            table);
    }

  if (minusp)
    {
      len += ichar_len ('-');
      (void) set_itext_ichar (buffer + size - len, '-');
    }

  slack = size - len;
  text_checking_assert (slack > 0);
  if (slack)
    {
      /* There will be overlap if LEN > size / 2, use memmove(). */
      memmove (buffer, buffer + slack, len);
    }

  /* Add a trailing '\0'. */
  buffer[len] = '\0';
  return len;
}

#ifdef HAVE_BIGNUM

/* Print BN, a signed bignum, in base RADIX using TABLE. If BUF points to a
   non-NULL Ibyte * pointer, take it to be a buffer into which to write, and
   use the value of *SIZE_INOUT as its size. Otherwise, allocate a new buffer
   using malloc() and store a pointer to it in *BUF. Store the size of the new
   buffer in *SIZE_INOUT.

   Start at the *end* with BN's least signficant digit, and return the length
   of the string written. Do not zero-terminate. Write a minus sign at the
   beginning if BN is negative. */
static Bytecount
bignum_to_string_1 (Ibyte **buf, Bytecount *size_inout, bignum bn,
                    EMACS_UINT radix, Lisp_Object table)
{
  Boolint minusp, heap_allocp = (*buf == NULL);
  Ibyte *buf1 = heap_allocp ?
    ((*size_inout = 128 * MAX_ICHAR_LEN),
     (*buf = xnew_array (Ibyte, *size_inout))) : *buf;
  Ibyte *end = buf1 + *size_inout, *cursor = end, *this_digit = NULL;
  Ibyte *ftmdata = XSTRING_DATA (table);
  /* Since, in contrast with the fixnum code, we are repeatedly checking the
   sign anyway, print the minus sign in this function if appropriate. */
  int signum = bignum_sign (bn);

  if ((minusp = signum < 0))
    {
      bignum_neg (scratch_bignum, bn);
      /* Reserve space for the minus sign in our accounting. */
      buf1 += ichar_len ('-');
    }
  else
    {
      bignum_set (scratch_bignum, bn);
    }

  while (signum)
    {
      this_digit
        = ftmdata + (bignum_div_rem_uint_16_bit (scratch_bignum,
                                                 scratch_bignum, radix)
                     * MAX_ICHAR_LEN);
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
      if (cursor - MAX_ICHAR_LEN <= buf1)
        {
          text_checking_assert (!heap_allocp);
          XREALLOC_ARRAY (*buf, Ibyte, *size_inout << 1);
          memcpy (*buf, *buf + *size_inout, *size_inout);
          *size_inout <<= 1;
          buf1 = *buf + minusp * ichar_len ('-');
        }
      signum = bignum_sign (scratch_bignum);
    }

  if (end == cursor)
    {
      this_digit = ftmdata + 0;
      cursor -= itext_ichar_len (this_digit);
      /* Haven't written anything yet, can't yet overflow */ 
      itext_copy_ichar (this_digit, cursor);
    }

  if (minusp)
    {
      cursor -= ichar_len ('-');
      (void) set_itext_ichar (cursor, '-');
    }

  text_checking_assert (cursor >= *buf);

  return end - cursor;
}

/* Print NUMBER, a signed bignum, as a base RADIX string. If BUFFER_INOUT
   points to a non-NULL Ibyte * pointer, take that to be a buffer into which
   to write, and use the value of *SIZE_INOUT as its size. Otherwise, allocate
   a new buffer using malloc() and store a pointer to it in
   *BUFFER_INOUT. Store the size of the new buffer in *SIZE_INOUT.

   The returned printed number will start with its most significant digits at
   the beginning of *BUFFER_INOUT, and will be zero-terminated.

   TABLE_OR_NIL describes a map from fixnums to digits. If Qnil, it defaults
   to Vfixnum_to_majuscule_map. Another useful value is
   Vfixnum_to_majuscule_ascii, to avoid language-specific digit characters
   (e.g. Persian, fullwidth Chinese).

   Return the length of the printed string, without the terminating zero. If
   Throw an assertion failure if BUFFER overflows and ERROR_CHECK_TEXT is
   turned on.  If *BUFFER_INOUT was NULL, the value written to it needs to be
   freed with free() once the caller is finished with it. */
Bytecount
bignum_to_string (Ibyte **buffer_inout, Bytecount size, bignum number,
                  UINT_16_BIT radix, Lisp_Object table_or_nil)
{
  Bytecount len = 0, slack;
  Lisp_Object table
    = NILP (table_or_nil) ? Vfixnum_to_majuscule_map : table_or_nil;

  len = bignum_to_string_1 (buffer_inout, &size, number, radix, table);

  slack = size - len;
  text_checking_assert (slack > 0);
  if (slack)
    {
      /* There will be overlap if LEN > size / 2, use memmove(). */
      memmove (*buffer_inout, *buffer_inout + slack, len);
    }

  /* Add a trailing '\0'. */
  (*buffer_inout)[len] = '\0';
  return len;
}
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO

static Bytecount
ratio_to_string_1 (Ibyte **buf, Bytecount size, ratio rat, UINT_16_BIT base,
                   Lisp_Object table)
{
  Bytecount len;
  Ibyte *cursor;

  if (NULL == *buf)
    {
#ifdef mpz_sizeinbase
      size = ((mpz_sizeinbase (ratio_numerator (rat), base) +
               mpz_sizeinbase (ratio_denominator (rat), base))
              * MAX_ICHAR_LEN) + ichar_len ('/') + ichar_len ('-') + 1;
      *buf = xnew_ibytes (size);
#else
#error "unimplemented"
#endif      
    }

  len = bignum_to_string_1 (buf, &size, ratio_denominator (rat), base, table); 
  cursor = *buf + size - len;

  cursor -= ichar_len ('/');
  len += set_itext_ichar (cursor, '/');
  size -= len;
  len += bignum_to_string_1 (buf, &size, ratio_numerator (rat), base, table);
  return len;
}

Bytecount
ratio_to_string (Ibyte **buffer_inout, Bytecount size, ratio number,
                 UINT_16_BIT radix, Lisp_Object table_or_nil)
{
  Bytecount len = 0, slack;
  Lisp_Object table
    = NILP (table_or_nil) ? Vfixnum_to_majuscule_map : table_or_nil;

  len = ratio_to_string_1 (buffer_inout, size, number, radix, table);

  slack = size - len;
  text_checking_assert (slack >= 0);
  if (slack)
    {
      /* There will be overlap if LEN > size / 2, use memmove(). */
      memmove (*buffer_inout, *buffer_inout + slack, len);
    }

  /* Add a trailing '\0'. */
  (*buffer_inout)[len] = '\0';
  return len;
}

#endif /* HAVE_RATIO */

DEFUN ("number-to-string", Fnumber_to_string, 1, 3, 0, /*
Convert NUMBER to a string by printing it in base RADIX.

Uses a minus sign if negative.  NUMBER may be an integer or a floating point
number.  If supported, it may also be a ratio.  There is no support for a
non-decimal RADIX if NUMBER is a float.

See `digit-char' for details of the limitations on RADIX and RADIX-TABLE.

arguments: (NUMBER &optional (RADIX 10) RADIX_TABLE)
*/
       (number, radix, radix_table))
{
  UINT_16_BIT radixing = 10;
  Lisp_Object fixnum_to_char_table;

  CHECK_NUMBER (number);

  if (!NILP (radix_table) && !EQ (radix_table, Vdigit_fixnum_map))
    {
      CHECK_CHAR_TABLE (radix_table);
      if (EQ (Vdigit_fixnum_ascii, radix_table))
        {
          fixnum_to_char_table = Vfixnum_to_majuscule_ascii;
        }
      else
        {
          /* The result of this isn't GCPROd, but the rest of this function
             won't GC and continue. */
          fixnum_to_char_table = build_fixnum_to_char_map (radix_table);
        }
    }
  else
    {
      fixnum_to_char_table = Vfixnum_to_majuscule_map;
    }

  if (!NILP (radix))
    {
      check_integer_range (radix, make_fixnum (2),
                           make_fixnum (XSTRING_LENGTH (fixnum_to_char_table)
                                        / MAX_ICHAR_LEN));
      radixing = (UINT_16_BIT) XFIXNUM (radix);
    }

  if (FLOATP (number))
    {
      Ascbyte pigbuf[350];	/* see comments in float_to_string */

      if (radixing != 10)
        {
          signal_error_2 (Qunimplemented,
                          "non-decimal printing of floats",
                          number, radix);
        }

      return make_string ((const Ibyte *) pigbuf,
                          float_to_string (pigbuf, XFLOAT_DATA (number)));
    }
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      Bytecount size, len;
      Ibyte *buf;
      Lisp_Object retval;

#ifdef bignum_size_binary
      size = bignum_size_binary (XBIGNUM_DATA (number));
      buf = alloca_ibytes (size);
#else
      size = -1;
      buf = NULL;
#endif
      len = bignum_to_string_1 (&buf, &size, XBIGNUM_DATA (number), radixing,
                                fixnum_to_char_table);
      retval = make_string (buf + size - len, len);

#ifndef bignum_size_decimal
      xfree (buf);
#endif

      return retval;
    }
#endif
#ifdef HAVE_RATIO
  if (RATIOP (number))
    {
      Bytecount size = ratio_size_in_base (XRATIO_DATA (number),
                                           radixing), len;
      Ibyte *buffer = alloca_ibytes (size);

      len = ratio_to_string_1 (&buffer, size, XRATIO_DATA (number), radixing,
                               fixnum_to_char_table);
      return make_string (buffer + size - len, len);
    }
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      Ascbyte *str;
      Lisp_Object retval;
      if (radixing != 10)
        {
          signal_error_2 (Qunimplemented,
                          "non-decimal printing of bigfloats",
                          number, radix);
        }

      str = bigfloat_to_string (XBIGFLOAT_DATA (number), 10);
      retval = build_ascstring (str);
      xfree (str);
      return retval;
    }
#endif

  {
    Ibyte to_print[SIZEOF_EMACS_INT * 8 * MAX_ICHAR_LEN + 1];

    return make_string (to_print,
                        fixnum_to_string (to_print, sizeof (to_print),
                                          XFIXNUM (number), radixing,
                                          fixnum_to_char_table));
  }
}

#define VALID_FLAGS "-+ #0&~!"
#define VALID_CONVERTERS "dic" "ouxXp" "feEgG" "sS" "b"
#define INT_CONVERTERS "dicoxXbp"
#define DOUBLE_CONVERTERS "feEgG"
#define STRING_CONVERTERS "sS"
#define BIGNUM_CONVERTERS "nPyYB"
#define RATIO_CONVERTERS "mQvVW"
#define BIGFLOAT_CONVERTERS "FhHkK"

typedef struct printf_spec printf_spec;
struct printf_spec
{
  Bytecount text_before_len; /* Length of the block of literal text before
                                this spec. */
  Elemcount argnum; /* Which argument does this spec want?  This is one-based:
                       The first argument given is numbered 1, the second is
                       2, etc.  This is to handle %##$x-type specs. */
  Bytecount text_before; /* Position of the first character of the block of
                            literal text before this spec. */
  Charcount minwidth;
  Charcount precision;
  Bytecount spec_length; /* Length of this format spec itself, starting at
                            text_before + text_before_len.  */
  Ibyte pad_char[MAX_ICHAR_LEN]; /* Usually space. */
  unsigned int converter:7; /* Converter character, or 0 for dummy marker
                               indicating literal text at the end of the
                               specification. */
  unsigned int hl_flag:3;
  unsigned int number_flag:2;
  unsigned int sign_flag:2;
  unsigned int precision_details:2;
  unsigned int left_justify:1;
  unsigned int zero_flag:1;
  unsigned int unsigned_flag:1;
  unsigned int forwarding_precisionp:1;
};

typedef union printf_arg printf_arg;
union printf_arg
{
  EMACS_INT l;
  EMACS_UINT ul;
  double d;
  Ibyte *bp;
  UINT_64_BIT u64;
  Lisp_Object obj;
};

/* We maintain a list of all the % specs in the specification, along with the
   offset and length of the block of literal text before each spec.  In
   addition, we have a "dummy" spec that represents all the literal text at
   the end of the specification.  Its converter is 0. */
typedef struct
{
  Dynarr_declare (struct printf_spec);
} printf_spec_dynarr;

/* Enums, macros for doprnt_1(). */

/* What sign information should be displayed? */
enum sign_flag {
  SIGN_FLAG_NOTHING = 0,
  SIGN_FLAG_PADCHAR,
  SIGN_FLAG_PLUS,
  SIGN_FLAG_MINUS
};

/* How should the PRECISION field be treated? */
enum precision_details {
  PRECISION_MAX_CHARACTERS = 0,
  PRECISION_DIGITS_AFTER_DECIMAL,
  PRECISION_MIN_TOTAL_DIGITS,
  PRECISION_MAX_SIGNIFICANT_DIGITS
};

/* Was the # (or &) flag specified, and if so, should it be interpreted as
   favouring Lisp or C syntax? */
enum number_flag {
  NUMBER_FLAG_NOTHING,
  NUMBER_FLAG_LISP_SYNTAX,
  NUMBER_FLAG_C_SYNTAX,
  NUMBER_FLAG_MASOCHISM /* This is C syntax, but with the sign after 0x, not
                           before. */
};

/* Was the h, the l, the hh, or the ll flag specified? */
enum hl_flag {
  HL_FLAG_NOTHING,
  HL_FLAG_HH,
  HL_FLAG_H,
  HL_FLAG_L,
  HL_FLAG_LL
};

#define NUMBER_FLAG_C_LIKEP(flag) (flag >= NUMBER_FLAG_C_SYNTAX)

#define HANDLE_SIGN_FLAG(sf) do switch (sf)                             \
    {                                                                   \
    case SIGN_FLAG_PADCHAR:                                             \
      fill_cursor += itext_copy_ichar (pfsp->pad_char, fill_cursor);    \
      break;                                                            \
    case SIGN_FLAG_PLUS:                                                \
      fill_cursor += set_itext_ichar (fill_cursor, '+');                \
      break;                                                            \
    case SIGN_FLAG_MINUS:                                               \
      fill_cursor += set_itext_ichar (fill_cursor, '-');                \
      break;                                                            \
    default:                                                            \
      break;                                                            \
    } while (0)

#define HANDLE_SIGN_AND_NUMBER(nf, sf)                                  \
  do switch (nf)                                                        \
    {                                                                   \
    case NUMBER_FLAG_NOTHING:                                           \
      HANDLE_SIGN_FLAG (sf);                                            \
      break;                                                            \
    case NUMBER_FLAG_C_SYNTAX:                                          \
      HANDLE_SIGN_FLAG (sf);                                            \
      fill_cursor += set_itext_ichar (fill_cursor, '0');                \
      fill_cursor += set_itext_ichar (fill_cursor, pfsp->converter);    \
      break;                                                            \
    case NUMBER_FLAG_LISP_SYNTAX:                                       \
      fill_cursor += set_itext_ichar (fill_cursor, '#');                \
      fill_cursor += set_itext_ichar (fill_cursor, pfsp->converter);    \
      HANDLE_SIGN_FLAG (sf);                                            \
      break;                                                            \
    case NUMBER_FLAG_MASOCHISM:                                         \
      fill_cursor += set_itext_ichar (fill_cursor, '0');                \
      fill_cursor += set_itext_ichar (fill_cursor, pfsp->converter);    \
      HANDLE_SIGN_FLAG (sf);                                            \
      break;                                                            \
    } while (0)

static void
write_string_1_lstream (Lisp_Object stream, const Ibyte *str, Bytecount size)
{
  /* This becomes just a jmp _Lstream_write in the assembler. */
  Lstream_write (XLSTREAM (stream), (const void *) str, size);
}

static void
write_lisp_string_lstream (Lisp_Object stream, Lisp_Object string,
                           Bytecount offset, Bytecount len)
{
  /* And this becomes just a jmp _Lstream_write_with_extents. */
  Lstream_write_with_extents (XLSTREAM (stream), string, offset, len);
}

/* Append the string of length LEN starting at OFFSET to STREAM. Preserve any
   extent information.  If RELOC is non-nil, use its string data as the base
   for OFFSET. Otherwise, use NONRELOC.

   RELOC is a Lisp string, or Qnil. NONRELOC is a pointer to internal-format
   text, or NULL, to specify to use RELOC's string data. OFFSET is the byte
   offset within NONRELOC to start.  LEN is the byte length from OFFSET to
   append.

   PFSP is a pointer to a struct printf_spec. If it is non-NULL, the following
   fields within it are respected:
   MINWIDTH is taken as the minimum character field width.
   If LEFT_JUSTIFY is non-zero, left-justify the string in its field;
    otherwise, right-justify.
   If ZERO_FLAG is set, pad with 0s; otherwise pad with PFSP->PAD_CHAR.

   Handling of PRECISION depends on the PRECISON_DETAILS enum value.
   If it is PRECISION_MAX_CHARACTERS, the string is first truncated on the
   right to that many characters.
   If it is PRECISION_MIN_TOTAL_DIGITS, the string is padded on the left with
   zero characters if that is necessary to bring up its character count to
   that number given in PRECISION. This overrides ZERO_FLAG. There can be
   additional space padding as specified by MINWIDTH.

   If SIGN_FLAG is SIGN_FLAG_PLUS, output a plus between any space padding
   and before any zero padding. If it is SIGN_FLAG_MINUS, output a minus in
   that position. If it is SIGN_FLAG_PADCHAR, output a space.
   If it is SIGN_FLAG_HEX_UPPERCASE or SIGN_FLAG_HEX_LOWERCASE, output 0X or
   0x as appropriate. If it is SIGN_FLAG_NOTHING, do not output anything extra
   in this position.

   FORMAT_OBJECT, if non-nil, is taken to be Lisp object that the format spec
   came from. It should not be specified if it is identical to RELOC. If
   FORMAT_OBJECT has extent information, the following fields in PFSP are also
   used to access it:

   SPEC_LENGTH is taken to be the length of this format spec within
   FORMAT_OBJECT.
   TEXT_BEFORE and TEXT_BEFORE_LEN are summed to find the offset within
   FORMAT_OBJECT where the format spec started. */

static Bytecount
doprnt_1 (Lisp_Object stream,
          const Ibyte *nonreloc, Lisp_Object reloc,
          Bytecount offset, Bytecount len,
          struct printf_spec *pfsp, Lisp_Object format_object)
{
  Bytecount result_len = 0, begin;
  const Ibyte *newnonreloc = NILP (reloc) ? nonreloc : XSTRING_DATA (reloc);
  void (*write_string_2) (Lisp_Object, const Ibyte *, Bytecount);
  void (*write_lisp_string_2) (Lisp_Object, Lisp_Object, Bytecount, Bytecount);

  text_checking_assert (!(EQ (reloc, format_object)) || NILP (reloc));

  if (LSTREAMP (stream))
    {
      /* This is an optimization, for the usual lstream case calling
         write_string_1() instead of write_string_1_lstream () doubles our
         run-time in my tests. */
      begin = Lstream_byte_count (XLSTREAM (stream));
      write_string_2 = write_string_1_lstream;
      write_lisp_string_2 = write_lisp_string_lstream;
    }
  else
    {
      begin = stream_extent_position (stream);
      write_string_2 = write_string_1;
      write_lisp_string_2 = write_lisp_string;
    }

  if (pfsp != NULL)
    {
      Charcount padding_to_add = -1, minwidth = pfsp->minwidth;
      Charcount maxlen = -1, insert_zeros_until = -1, zeros_to_add = -1;
      Boolint left_justify = pfsp->left_justify, zero_flag = pfsp->zero_flag;
      enum sign_flag sign_flag = (enum sign_flag) (pfsp->sign_flag);
      enum number_flag number_flag = (enum number_flag) (pfsp->number_flag);
      Ibyte *filling, *fill_cursor;
      Bytecount filllen, realfill;

      switch ((enum precision_details) (pfsp->precision_details))
        {
        case PRECISION_MAX_CHARACTERS:
          maxlen = pfsp->precision;
          break;
        case PRECISION_MIN_TOTAL_DIGITS:
          insert_zeros_until = pfsp->precision;
          break;
        case PRECISION_DIGITS_AFTER_DECIMAL:
        case PRECISION_MAX_SIGNIFICANT_DIGITS:
          ABORT (); /* These are for float values, which are unimplemented for
                       the moment, being passed through to the C
                       implementation of snprintf(). */
          break;
        }

      /* LEN / MAX_ICHAR_LEN is an inclusive lower bound on CCLEN. It
         represents the case where NEWNONRELOC comprises exclusively
         characters of the maximum byte length. If minwidth is zero or < (LEN
         / MAX_ICHAR_LEN), definitely no need to pad; otherwise we need to
         calculate the character length to work out whether we need to pad. */
      if ((minwidth != 0 && minwidth >= (len / MAX_ICHAR_LEN))

          /* Sigh, INSERT_ZEROS_UNTIL and ZERO_FLAG need to be interpreted
             separately. While it is not possible to combine
             PRECISION_MIN_TOTAL_DIGITS and ZERO_FLAG, it is possible to
             combine the former with PADDING_TO_ADD, which is managed by the
             same code. */
          || (insert_zeros_until > -1 &&
              insert_zeros_until >= (len / MAX_ICHAR_LEN))

          /* LEN is an inclusive upper bound on CCLEN. It represents the case
             where NEWNONRELOC comprises exclusively ASCII characters. If
             MAXLEN is greater than or equal to LEN, no need to truncate. */
          || (0 <= maxlen && maxlen < len))
        {
          Charcount cclen = bytecount_to_charcount (newnonreloc + offset, len);

          zeros_to_add = max (insert_zeros_until - cclen, 0);
          cclen += (sign_flag != SIGN_FLAG_NOTHING);
          cclen += (number_flag != NUMBER_FLAG_NOTHING) * 2;
          padding_to_add = minwidth - (cclen + zeros_to_add);

          if (maxlen >= 0 && (maxlen = min (maxlen, cclen), maxlen != cclen))
            {
              len = charcount_to_bytecount (newnonreloc + offset, maxlen);
              /* Truncation shouldn't happen with numbers. */
              text_checking_assert (sign_flag == SIGN_FLAG_NOTHING);
            }
        }
      filllen = (max (zeros_to_add, 0) + 1 /* sign flag */
                 + 2 /* number flag */ + EMACS_INT_ABS (padding_to_add))
        * MAX_ICHAR_LEN;
      filling = fill_cursor = alloca_ibytes (filllen);

      if (left_justify)
        {
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
        }
      else if (zero_flag)
        {
          /* If we're padding on the left (= right-justifying) with zero
             characters, then any sign information has to be before those
             zeroes. */
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
          while (padding_to_add-- > 0)
            {
              fill_cursor += set_itext_ichar (fill_cursor, '0');
            }
        }
      else
        {
          while (padding_to_add-- > 0)
            {
              fill_cursor += itext_copy_ichar (pfsp->pad_char, fill_cursor);
            }
          /* Sign information comes after the spaces. */
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
        }

      while (zeros_to_add-- > 0)
        {
          fill_cursor += set_itext_ichar (fill_cursor, '0');
        }

      realfill = fill_cursor - filling;
      text_checking_assert (realfill <= filllen);
      if (realfill)
        {
          write_string_2 (stream, filling, realfill);
          result_len += realfill;
          fill_cursor = filling;
        }

      if (NILP (reloc))
        {
          write_string_2 (stream, newnonreloc + offset, len);
        }
      else
        {
          /* 1. Handle RELOC possibly having its data relocated when Lisp code
             adjusts it such that its bytecount changes.
             2. Copy RELOC's extent information directly to this point in the
             output, do not stretch it. */
          write_lisp_string_2 (stream, reloc, offset, len);
        }

      result_len += len;

      /* Padding at end to left-justify ... */
      if (left_justify)
        {
          if (zero_flag)
            {
              while (padding_to_add-- > 0)
                {
                  fill_cursor += set_itext_ichar (fill_cursor, '0');
                }
            }
          else
            {
              while (padding_to_add-- > 0)
                {
                  fill_cursor
                    += itext_copy_ichar (pfsp->pad_char, fill_cursor);
                }
            }

          realfill = fill_cursor - filling;
          text_checking_assert (realfill < filllen);
          if (realfill)
            {
              write_string_2 (stream, filling, realfill);
              result_len += realfill;
            }
        }

      if (!NILP (format_object) && begin != -1 &&
          string_extent_info (format_object) != NULL)
        {
          stretch_string_extents (MARKERP (stream) ? Fmarker_buffer (stream)
                                  : stream, format_object, begin,
                                  pfsp->text_before + pfsp->text_before_len,
                                  pfsp->spec_length,
                                  stream_extent_position (stream) - begin);
        }
    }
  else
    {
      if (NILP (reloc))
        {
          write_string_2 (stream, newnonreloc + offset, len);
        }
      else
        {
          /* 1. Handle RELOC possibly having its data relocated when Lisp code
             adjusts it such that its bytecount changes.
             2. Copy RELOC's extent information directly to this point in the
             output, do not stretch it. */
          write_lisp_string_2 (stream, reloc, offset, len);
        }
      result_len += len;
    }

  return result_len;
}

#define NEXT_ASCBYTE(ch)						\
  do {									\
    if (fmt == fmt_end)							\
      {                                                                 \
        Dynarr_free (specs);                                            \
        syntax_error ("Premature end of format string", Qunbound);	\
      }                                                                 \
    ch = *fmt;								\
    if (!byte_ascii_p (ch))                                             \
      {                                                                 \
        Dynarr_free (specs);                                            \
        syntax_error ("Not a valid character, format converter spec",	\
                      make_char (itext_ichar (fmt)));                   \
      }                                                                 \
    fmt++;								\
  } while (0)

#define RESOLVE_FLAG_CONFLICTS(spec)				\
  do {								\
    if (spec.number_flag == NUMBER_FLAG_LISP_SYNTAX &&          \
        spec.sign_flag == SIGN_FLAG_PADCHAR)                    \
      {                                                         \
        spec.sign_flag = SIGN_FLAG_NOTHING;                     \
      }                                                         \
    if (spec.zero_flag && spec.sign_flag == SIGN_FLAG_PADCHAR)  \
      {                                                         \
        spec.zero_flag = 0;					\
      }                                                         \
  } while (0)

/* Given FERMAT, an internal-format string of length FERMAT_LENGTH, parse it
   into a Dynarr of struct printf_spec reflecting the % conversion specifiers
   within FERMAT, and return that Dynarr. In ARGS_NEEDED_OUT, return the
   greatest argument number encountered. Note this function has no access to
   the actual args specified, and in theory could run at compile time.

   Error if FERMAT ends in the middle of a conversion specifier, if its
   conversion specifiers contain non-ASCII non-pad characters, or if the
   minimum widths and precisions supplied are nonsensical.

   The Dynarr may be preallocated and supplied in SPECS. This is usually not
   convenient, and when SPECS is NULL, parse_doprnt_spec returns a fresh
   heap-allocated Dynarr, which should be freed once callers are finished with
   it, e.g. with record_unwind_protect_freeing_dynarr(). */
static printf_spec_dynarr *
parse_doprnt_spec (printf_spec_dynarr *specs,
                   const Ibyte *format, Bytecount format_length,
                   Elemcount *args_needed_out)
{
  const Ibyte *fmt = format;
  const Ibyte *fmt_end = format + format_length;
  Elemcount prev_argnum = 0, max_argnum = 0;

  if (specs == NULL)
    {
      specs = Dynarr_new (printf_spec);
      /* As a somewhat-representative survey, of the format specs used while
         building XEmacs, as of 20161013, there were 16168 where the length of
         specs was one, 21198 where it was two, 664 when it was 3, 86 when it
         was 4, 35 when it was 5, and 4 when it was 7. Nothing greater than
         that. */
      Dynarr_resize_to_fit (specs, 8);
    }

  while (1)
    {
      struct printf_spec spec;
      const Ibyte *text_end;
      Ibyte ch;

      if (fmt == fmt_end)
        {
          *args_needed_out = max_argnum;
          return specs;
        }

      xzero (spec);
      text_end = (Ibyte *) memchr (fmt, '%', fmt_end - fmt);
      if (!text_end)
	text_end = fmt_end;
      spec.text_before = fmt - format;
      spec.text_before_len = text_end - fmt;
      (void) set_itext_ichar (spec.pad_char, ' ');
      spec.precision = -1;
      
      fmt = text_end;
      if (fmt != fmt_end)
	{
          fmt += ichar_len ('%'); /* skip over % */

	  /* A % is special -- no arg number.  According to ANSI specs,
	     field width does not apply to %% conversion. */
	  if (fmt != fmt_end && itext_ichar_eql (fmt, '%'))
	    {
	      spec.converter = '%';
              spec.spec_length = fmt - text_end;
              /* Argnum is zero here, which is intended. */
	      Dynarr_add (specs, spec); 
              fmt += ichar_len ('%');
	      continue;
	    }

	  /* Is there a repositioning specifier? */
	  {
            /* Do a gross plausibility check instead of calling parse_integer
               on every converter we see. */
            Ichar cch = itext_ichar (fmt);
            Ibyte *fmt1;
            /* No language dependence in this parsing, so direct comparison is
               fine, and a bit more robust when erroring early in the
               build. */
            Lisp_Object posnum = (cch > '0' && cch <= '9') ?
              parse_integer (fmt, &fmt1, fmt_end - fmt, 10, 1,
                             Vdigit_fixnum_ascii) : Qnil;

            if (FIXNUMP (posnum) && XREALFIXNUM (posnum) > 0
                && itext_ichar_eql (fmt1, '$'))
              {
                /* There is a repositioning specifier: */
                prev_argnum = XREALFIXNUM (posnum);
                fmt = fmt1;
                INC_IBYTEPTR (fmt);
              }
            else 
              {
                prev_argnum++;
                /* Don't check POSNUM further (e.g. for sign, range), since it
                   may actually be a field width. */
              }

	    spec.argnum = prev_argnum;
            max_argnum = max (max_argnum, prev_argnum);
	  }

	  /* Parse off any flags. */
	  while (1)
	    {
              NEXT_ASCBYTE (ch);
	      switch (ch)
		{
		case '-': spec.left_justify  = 1; break;
		case '+': spec.sign_flag = SIGN_FLAG_PLUS; break;
		case ' ': 
                  if (spec.sign_flag != SIGN_FLAG_PLUS)
                    {
                      /* The plus flag overrides. */
                      spec.sign_flag = SIGN_FLAG_PADCHAR; 
                    }
                  break;
		case '#':
                  if (spec.number_flag != NUMBER_FLAG_MASOCHISM)
                    {
                      spec.number_flag = NUMBER_FLAG_C_SYNTAX;
                    }
                  break;
                case '&': spec.number_flag = NUMBER_FLAG_LISP_SYNTAX; break;
                case '~':
                  if (spec.number_flag != NUMBER_FLAG_LISP_SYNTAX)
                    {
                      spec.number_flag = NUMBER_FLAG_MASOCHISM;
                    }
                  break;
 		case '0': spec.zero_flag   = 1; break;
                case '!':
                  {
                    if (fmt == fmt_end)
                      {
                        Dynarr_free (specs);
                        syntax_error ("Premature end of format string",
                                      Qunbound);
                      }
                    fmt += itext_copy_ichar (fmt, spec.pad_char);
                    break;
                  }
 		default: /* Not a known flag. */
                  {
                    DEC_IBYTEPTR (fmt); /* Back up */
                    goto done_with_flags;
                  }
		}
	    }
        done_with_flags:
	  /* Parse off the minimum field width */
	  /*
	   * * means the field width was passed as an argument.
	   * Mark the current spec as one that forwards its
	   * field width and flags to the next spec in the array.
	   * Then create a new spec and continue with the parsing.
	   */
	  if (fmt != fmt_end && itext_ichar_eql (fmt, '*'))
	    {
	      spec.converter = '*';
              spec.spec_length = fmt - text_end;
	      RESOLVE_FLAG_CONFLICTS(spec);
	      Dynarr_add (specs, spec);
              /* Zero the spec, but preserve the pad char. */
              memset ((Binbyte *)(&spec), 0,
                      offsetof (struct printf_spec, pad_char));
              memset ((Binbyte *)(&spec) + offsetof (struct printf_spec,
                                                     pad_char)
                      + MAX_ICHAR_LEN, 0, sizeof (struct printf_spec)
                      - offsetof (struct printf_spec, pad_char)
                      - MAX_ICHAR_LEN);
	      spec.argnum = ++prev_argnum;
              max_argnum = max (max_argnum, prev_argnum);
              fmt += ichar_len ('*');
	    }
	  else
	    {
              Ichar cch = itext_ichar (fmt);
              Lisp_Object mwidth
                = (cch > '0' && cch <= '9') ?
                parse_integer (fmt, (Ibyte **) (&fmt), fmt_end - fmt, 10, 1,
                               Vdigit_fixnum_ascii) : Qnil;
              if (NILP (mwidth))
                {
                  spec.minwidth = -1; /* Failed to parse an integer, which is
                                         fine, use our default. */
                }
              else
                {
                  check_integer_range (mwidth, Qzero,
                                       /* This is a charcount used for
                                          allocation, whence the following
                                          limitation. See the multiplication
                                          below just above the call to
                                          snprintf ().  */
                                       make_fixnum (MOST_POSITIVE_FIXNUM /
                                                    MAX_ICHAR_LEN));
                  /* We have a (somewhat) sensible minwidth. */
                  spec.minwidth = XFIXNUM (mwidth); 
                }
	    }

	  /* Parse off any precision specified */
	  NEXT_ASCBYTE (ch);
	  if (ch == '.')
	    {
	      /*
	       * * means the precision was passed as an argument.
	       * Mark the current spec as one that forwards its
	       * fieldwidth, flags and precision to the next spec in
	       * the array.  Then create a new spec and continue
	       * with the parse.
	       */
	      if (fmt != fmt_end && itext_ichar_eql (fmt, '*'))
		{
		  spec.converter = '*';
                  spec.spec_length = fmt - text_end;
		  spec.forwarding_precisionp = 1;
		  RESOLVE_FLAG_CONFLICTS(spec);
		  Dynarr_add (specs, spec);
                  /* Zero the spec, but preserve the pad char. */
                  memset ((Binbyte *)(&spec), 0,
                          offsetof (struct printf_spec, pad_char));
                  memset ((Binbyte *)(&spec) + offsetof (struct printf_spec,
                                                         pad_char)
                          + MAX_ICHAR_LEN, 0, sizeof (struct printf_spec)
                          - offsetof (struct printf_spec, pad_char)
                          - MAX_ICHAR_LEN);
		  spec.argnum = ++prev_argnum;
                  max_argnum = max (max_argnum, prev_argnum);
                  fmt += ichar_len ('*');
		}
	      else
		{
                  /* Not checking for a possible digit before calling
                     parse_integer(), since this code will be called if we've
                     seen a '.', which is a small minority rather than a vast
                     majority of times. */
                  Lisp_Object precis
                    = parse_integer (fmt, (Ibyte **) (&fmt), fmt_end - fmt,
                                     10, 1, Vdigit_fixnum_ascii);
                  if (NILP (precis))
                    {
                      spec.precision = 0; /* There is a dot, but we failed to
                                             parse a fixnum. Treat as a
                                             zero-length precision, as the C
                                             standard says. */
                    }
                  else 
                    {
                      check_integer_range (precis,
                                           /* This is a charcount. */
                                           make_fixnum (MOST_NEGATIVE_FIXNUM
                                                        / MAX_ICHAR_LEN),
                                           make_fixnum (MOST_POSITIVE_FIXNUM /
                                                        MAX_ICHAR_LEN));
                      /* The C standard says a negative fixnum is to be
                         treated as not specified. */
                      spec.precision = max (XFIXNUM (precis), -1);
                    }
		}
	      NEXT_ASCBYTE (ch);
	    }
	  else
	    /* No precision specified */
	    spec.precision = -1;

	  /* Parse off h or l flag. */
          if (ch == 'h')
            {
              NEXT_ASCBYTE (ch);
              if (ch == 'h')
                {
                  spec.hl_flag = HL_FLAG_HH;
                  NEXT_ASCBYTE (ch);
                }
              else
                {
                  spec.hl_flag = HL_FLAG_H;
                }
            }
          else if (ch == 'l')
            {
              NEXT_ASCBYTE (ch);
              if (ch == 'l')
                {
                  spec.hl_flag = HL_FLAG_LL;
                  NEXT_ASCBYTE (ch);
                }
              else
                {
                  spec.hl_flag = HL_FLAG_L;
                }
            }

          /* In contrast with C, u is a *modifier* for a following d, b, x, X,
             or o converter. If seen on its own it is equivalent to ud, which
             is compatible with the old behaviour. */
          if (ch == 'u')
            {
              spec.unsigned_flag = 1;
              if (fmt != fmt_end)
                {
                  NEXT_ASCBYTE (ch);
                  if (!strchr (INT_CONVERTERS, ch))
                    {
                      /* We don't accept the u modifier for the
                         BIGNUM_CONVERTERS, above. If this is a problem, it is
                         only for C-level calls to write_fmt_string(), where
                         we can't rely on Lisp type information to distinguish
                         between fixnums and bignums. Rejecting the u modifier
                         has the advantage that Lisp (and C) callers are much
                         less likely to have an incidental n, P, y, Y or B
                         interpreted as a bignum specifier accidentally, which
                         is important because our bignum converters are
                         XEmacs-specific and u is very much *not*
                         XEmacs-specific. */
                      DEC_IBYTEPTR (fmt);
                      ch = 'd';
                    }
                }
              else
                {
                  ch = 'd';
                }
            }

	  spec.converter = ch;
          spec.spec_length = fmt - text_end;
	}

      RESOLVE_FLAG_CONFLICTS(spec);
      Dynarr_add (specs, spec);
    }

  RETURN_NOT_REACHED(specs); /* suppress compiler warning */
}

static void
get_doprnt_c_args (printf_arg *args, Elemcount args_needed,
                 printf_spec_dynarr *specs, va_list vargs)
{
  printf_arg *argp = args;
  union printf_arg arg;
  REGISTER int i;

  xzero (arg);
  for (i = 1; i <= args_needed; i++)
    {
      int j;
      Ascbyte ch;
      struct printf_spec *spec = 0;

      for (j = 0; j < Dynarr_length (specs); j++)
	{
	  spec = Dynarr_atp (specs, j);
	  if (spec->argnum == i)
	    break;
	}

      if (j == Dynarr_length (specs))
        {
          syntax_error ("No conversion spec for argument", make_fixnum (i));
        }

      ch = spec->converter;

      if (strchr (INT_CONVERTERS, ch))
	{
          /* Design decision; when dealing with C arguments, the l (ell)
             modifier means the argument is the same bit length as an
             EMACS_INT, whether that bit length be 32, 64 or 128. Other
             modifiers are as documented for Lisp, that is, 8, 16 and 64
             bits. No modifier means the argument will be read as a C int,
             which may not be what you want. */
          if ((spec->hl_flag == HL_FLAG_L ||
               /* Pointers should be the same bit length as an EMACS_INT. */
               (ch == 'p'))
              && SIZEOF_INT != SIZEOF_EMACS_INT)
            {
#if SIZEOF_EMACS_INT < SIZEOF_INT 
#error "you have a very strange system--unimplemented"
#endif
              arg.l = va_arg (vargs, EMACS_INT);
              spec->hl_flag = HL_FLAG_NOTHING;
            }
          else if (spec->hl_flag == HL_FLAG_LL && SIZEOF_INT < 8)
            {
              INT_64_BIT i64 = va_arg (vargs, INT_64_BIT);

              if (SIZEOF_EMACS_INT >= 8)
                {
                  arg.l = (EMACS_INT) i64;
                }
              else if (i64 < 0 && !spec->unsigned_flag)
                {
                  spec->sign_flag = SIGN_FLAG_MINUS;
                  spec->unsigned_flag = 1;
                  arg.u64 = -i64;
                }
              else
                {
                  spec->unsigned_flag = 1;
                  arg.u64 = i64;
                }
            }
          else
            {
              /* Default promotions promote to int, which is appropriate for
                 the other bit length modifiers (hh, h). */
              arg.l = va_arg (vargs, int);
            }
        }
      else if (ch == 's')
	arg.bp = va_arg (vargs, Ibyte *);
      else if (strchr (
#ifdef HAVE_BIGNUM
                       BIGNUM_CONVERTERS
#endif
#ifdef HAVE_RATIO
                       RATIO_CONVERTERS
#endif
#ifdef HAVE_BIGFLOAT
                       BIGFLOAT_CONVERTERS
#endif
                       "S", ch))
	arg.obj = va_arg (vargs, Lisp_Object);
      else if (strchr (DOUBLE_CONVERTERS, ch))
	arg.d = va_arg (vargs, double);
      /* We get here, e.g., if a repositioning field width or precision was
         supplied at the C level, something not implemented. */
      else ABORT ();

      *argp++ = arg;
      text_checking_assert (argp - args <= args_needed);
    }
}

#if FIXNUM_VALBITS >= 64
#define FIXNUM_SPEC_WIDEST_WIDTH_SPEC HL_FLAG_LL
#elif FIXNUM_VALBITS >= 32
#define FIXNUM_SPEC_WIDEST_WIDTH_SPEC HL_FLAG_L
#else
#define FIXNUM_SPEC_WIDEST_WIDTH_SPEC HL_FLAG_H
#endif

/* *SPEC is a printf_spec appropriate for printing a rational. *ARG or *OBJ
   reflect the corresponding argument supplied. If *OBJ or *ARG need to be
   coerced (e.g. a float needs to be truncated, a fixnum needs to be cast to a
   given bit field width), do so now . If the type of *OBJ means another
   conversion character is appropriate (e.g. %d was supplied, and the argument
   is a ratio), return a new conversion character. Do not change the
   conversion character within *SPEC. Error if *OBJ is not a number. */
static Ascbyte
rewrite_rational_spec (struct printf_spec *spec, printf_arg *arg,
                       Lisp_Object *obj)
{
  Ascbyte ch = spec->converter;

  if (CHARP (*obj))
    {
      /* Do accept a character argument for an integer format spec. */
      *obj = make_fixnum (XCHAR (*obj));
    }
  else if (FLOATP (*obj))
    {
      *obj = IGNORE_MULTIPLE_VALUES (Ftruncate (*obj, Qnil));
    }
#ifdef HAVE_BIGFLOAT
  else if (BIGFLOATP (*obj))
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, XBIGFLOAT_DATA (*obj));
      if (spec->unsigned_flag && bignum_sign (scratch_bignum) < 0)
        {
          dead_wrong_type_argument (Qnonnegativep, *obj);
        }
      *obj = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else /* !HAVE_BIGNUM */
      *obj = make_fixnum (bigfloat_to_long (XBIGFLOAT_DATA (*obj)));
#endif /* HAVE_BIGNUM */
    }
#endif /* HAVE_BIGFLOAT */
#ifdef HAVE_RATIO
  else if (RATIOP (*obj))
    {
      if (spec->unsigned_flag && ratio_sign (XRATIO_DATA (*obj)) < 0)
        {
          dead_wrong_type_argument (Qnonnegativep, *obj);
        }
      switch (ch)
        {
        case 'i': case 'd': ch = 'm'; break;
        case 'o': ch = 'Q'; break;
        case 'x': ch = 'v'; break;
        case 'X': ch = 'V'; break;
        case 'b': ch = 'W'; break;
          /* Fallthrough. */
        default:
          ch = 'm';
          break;
        }
    }
#endif
#ifdef HAVE_BIGNUM
  if (BIGNUMP (*obj))
    {
      if (spec->hl_flag != HL_FLAG_NOTHING)
        {
         /* Truncate it, and print as a fixnum. */
          if (spec->unsigned_flag)
            {
              arg->u64 = bignum_to_uint_64_bit (XBIGNUM_DATA (*obj));

              if (spec->hl_flag <= FIXNUM_SPEC_WIDEST_WIDTH_SPEC)
                {
                  *obj
                    = make_fixnum (arg->u64 & MOST_POSITIVE_FIXNUM_UNSIGNED);
                  /* Don't return here just yet, fallthrough to the fixnum
                     code. */
                  switch (ch)
                    {
                    case 'n': ch = 'd'; break;
                    case 'P': ch = 'o'; break;
                    case 'y': ch = 'x'; break;
                    case 'Y': ch = 'X'; break;
                    }
                }
              else
                {
                  if (spec->hl_flag == HL_FLAG_L)
                    {
                      arg->u64 &= 0xFFFFFFFF;
                    }

                  *obj = Qunbound;

                  spec->hl_flag = HL_FLAG_NOTHING;
                  return 'u';
                }
            }
          else
            {
              /* For consistent behaviour with C casts, use
                 bignum_to_uint_64_bit() and then convert the result into a
                 signed value. */
              INT_64_BIT i64val = bignum_to_uint_64_bit (XBIGNUM_DATA (*obj));

              switch ((enum hl_flag) (spec->hl_flag))
                {
                case HL_FLAG_HH:
                  *obj = make_fixnum ((signed char) i64val);
                  spec->hl_flag = HL_FLAG_NOTHING;
                  break;
                case HL_FLAG_H:
                  *obj = make_fixnum ((INT_16_BIT) i64val);
                  spec->hl_flag = HL_FLAG_NOTHING;
                  break;
                  /* The normal fixnum code can handle this, if the value is
                     not supplied as a Lisp object. */
                case HL_FLAG_L:
                  i64val = (INT_32_BIT) i64val;
                  /* FALLTHROUGH */
                case HL_FLAG_LL:
                  if (i64val < 0)
                    {
                      spec->sign_flag = SIGN_FLAG_MINUS;
                      arg->u64 = -i64val;
                    }
                  else
                    {
                      arg->u64 = i64val;
                    }
                  spec->unsigned_flag = 1;
                  *obj = Qunbound;
                  /* Can't let the fixval code handle it, it wants a number in
                     *obj. */
                  switch (ch)
                    {
                    case 'n': return 'd';
                    case 'P': return 'o';
                    case 'y': return 'x';
                    case 'Y': return 'X';
                    default: return ch;
                    }
                  break;
                }
            }
        }
      else
        {
          if (spec->unsigned_flag && bignum_sign (XBIGNUM_DATA (*obj)) < 0)
            {
              dead_wrong_type_argument (Qnatnump, *obj);
            }

          switch (ch)
            {
            case 'i': case 'd': ch = 'n'; break;
            case 'o': ch = 'P'; break;
            case 'x': ch = 'y'; break;
            case 'X': ch = 'Y'; break;
            case 'b': ch = 'B'; break;
            default:
              ch = 'n';
            }
        }
    }
#endif

  if (FIXNUMP (*obj) && spec->hl_flag != HL_FLAG_NOTHING)
    {
      if (spec->unsigned_flag)
        {
          switch ((enum hl_flag) (spec->hl_flag))
            {
            case HL_FLAG_HH:
              *obj = make_fixnum ((unsigned char) (XREALFIXNUM (*obj)));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            case HL_FLAG_H:
              *obj = make_fixnum ((UINT_16_BIT) (XREALFIXNUM (*obj)));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            case HL_FLAG_L:          
              arg->u64 = ((UINT_32_BIT) XREALFIXNUM (*obj));
              spec->hl_flag = HL_FLAG_NOTHING;
              *obj = Qunbound;
              return 'u';
            case HL_FLAG_LL:          
              arg->u64 = XREALFIXNUM (*obj);
              spec->hl_flag = HL_FLAG_NOTHING;
              *obj = Qunbound;
              return 'u';
            }
        }
      else
        {
          switch ((enum hl_flag) (spec->hl_flag))
            {
            case HL_FLAG_HH:
              *obj = make_fixnum ((signed char) XREALFIXNUM (*obj));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            case HL_FLAG_H:
              *obj = make_fixnum ((INT_16_BIT) XREALFIXNUM (*obj));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            case HL_FLAG_L:
              *obj = make_fixnum ((INT_32_BIT) XREALFIXNUM (*obj));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            case HL_FLAG_LL:
              /* No-op on a 32-bit machine. */
              *obj = make_fixnum ((INT_64_BIT) XREALFIXNUM (*obj));
              spec->hl_flag = HL_FLAG_NOTHING;
              break;
            }
        }
    }

  if (!NUMBERP (*obj))
    {
      syntax_error_2 ("Format specifier doesn't match arg type",
                      make_char (ch), *obj);
    }

  return ch;
}

/* *SPEC is a printf_spec appropriate for printing a floating point number.
   OBJ reflects the corresponding argument supplied.  If OBJ is a bigfloat,
   return a new conversion character appropriate for it. Do not change the
   conversion character within *SPEC. */
static Ascbyte
rewrite_floating_spec (struct printf_spec *spec, Lisp_Object obj)
{
  Ascbyte ch = spec->converter;

#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (obj))
    {
      switch (ch)
        {
        case 'f': ch = 'F'; break;
        case 'e': ch = 'h'; break;
        case 'E': ch = 'H'; break;
        case 'g': ch = 'k'; break;
        case 'G': ch = 'K'; break;
        }

      /* Leave obj alone. */
      return ch;
    }
#else
  USED (obj);
#endif

  /* The float spec code calls extract_float(), this is a far less complex
     function than rewrite_rational_spec(). */
  return ch;
}

#define FIXNUM_SPEC_PREAMBLE_1(buffer, size, radix, table, noargs) do   \
    {                                                                   \
      if (spec->precision > -1)                                         \
        {                                                               \
          spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;         \
          spec->zero_flag = 0;                                          \
        }                                                               \
      else if (spec->left_justify)                                      \
        {                                                               \
          spec->zero_flag = 0;                                          \
        }                                                               \
                                                                        \
      if (!UNBOUNDP (obj))                                              \
        {                                                               \
          if (!FIXNUMP (obj) || spec->hl_flag != HL_FLAG_NOTHING)       \
            {                                                           \
              ch = rewrite_rational_spec (spec, &arg, &obj);            \
              goto reconsider;                                          \
            }                                                           \
          arg.l = XREALFIXNUM (obj);                                    \
        }                                                               \
    } while (0)

#define FIXNUM_SPEC_PREAMBLE(buffer, size, radix, table) do             \
  {                                                                     \
    FIXNUM_SPEC_PREAMBLE_1 (buffer, size, radix, table, 0);             \
    if (spec->unsigned_flag)                                            \
      {                                                                 \
        if (UNBOUNDP (obj))                                             \
          {                                                             \
            ch = 'u';                                                   \
            goto reconsider;                                            \
          }                                                             \
        if (arg.l < 0)                                                  \
          {                                                             \
            dead_wrong_type_argument (Qnatnump,                         \
                                      make_integer (arg.l));            \
          }                                                             \
      }                                                                 \
  } while (0)


/* Most basic entry point into string formatting.  Generate output from a
   format-spec (either a Lisp string FORMAT_RELOC, or a C string
   FORMAT_NONRELOC of length FORMAT_LENGTH -- which *MUST NOT* come from Lisp
   string data, unless GC is inhibited).  SPECS is a printf_spec_dynarr
   reflecting the forrmat spec. Output goes to STREAM.  Returns the number of
   bytes stored into STREAM.  Arguments are either C-type arguments in ARGS,
   or an array of Lisp objects in LARGS.  (Behavior is different in the two
   cases -- you either get standard sprintf() behavior or `format' behavior.)

   The maximum index into the argument vector that is used depends on SPECS;
   see the ARGS_NEEDED_OUT argument to parse_doprnt_spec (). */

static Bytecount
emacs_doprnt (Lisp_Object stream,
              const Ibyte *format_nonreloc, Bytecount format_length,
              Lisp_Object format_reloc,
              printf_spec_dynarr *specs,
              const Lisp_Object *largs, const printf_arg *args)
{
  REGISTER int i;
  Bytecount byte_count = 0;
  Lisp_Object obj = Qunbound;
  struct gcpro gcpro1;

  /* A small fraction of the time OBJ can be a freshly-created bignum; a
     larger fraction of the time it can be a freshly-created string for the %S
     converter.  */
  GCPRO1 (obj);

  if (!NILP (format_reloc))
    {
      format_nonreloc = XSTRING_DATA (format_reloc);
      format_length = XSTRING_LENGTH (format_reloc);
    }

  text_checking_assert (format_length > -1);
  text_checking_assert (largs || args);
  
  for (i = 0; i < Dynarr_length (specs); i++)
    {
      struct printf_spec *spec = Dynarr_atp (specs, i);
      union printf_arg arg;
      Ascbyte ch;

      obj = Qunbound;

      /* Copy the text before */
      if (!NILP (format_reloc)) /* refetch in case of GC below */
        {
          format_nonreloc = XSTRING_DATA (format_reloc);
        }

      if (spec->text_before_len)
        {
          /* 28106 of the 61063 iterations of this loop when building core
             Lisp as of October 2016 have a zero text_before_len, it's worth
             checking that before calling doprnt_1(). */
          byte_count += doprnt_1 (stream, format_nonreloc, format_reloc,
                                  spec->text_before, spec->text_before_len,
                                  NULL, Qnil);
        }

      if (spec->argnum)
        {
          if (largs)
            {
              obj = largs[spec->argnum - 1];
            }
          else
            {
              arg = args[spec->argnum - 1];
            }
        }

      ch = spec->converter;

      /* These are organised roughly in decreasing order of frequency. */ 
    reconsider:
      switch (ch)
        {
        case 0:
          {
            /* This is the trailing spec, just present so text_before gets
               output, above.  */
            text_checking_assert (i + 1 == Dynarr_length (specs));
            continue;
          }
        case 's':
          {
            /* String, or some other Lisp object to be printed using
               #'princ. */
            Ibyte *string;
            Bytecount string_len;

            if (UNBOUNDP (obj))
              {
                string = arg.bp;
                text_checking_assert (string != NULL);
                string_len = qxestrlen (string);
                obj = Qnil;
              }
            else
              {
                if (!STRINGP (obj))
                  {
                    if (SYMBOLP (obj))
                      obj = XSYMBOL (obj)->name;
                    else
                      {
                        /* Convert to string using princ. OBJ is GCPRO'd. */
                        obj = prin1_to_string (obj, 1);
                      }
                  }
                string = NULL;
                string_len = XSTRING_LENGTH (obj);
              }

            byte_count += doprnt_1 (stream, string, obj, 0, string_len, spec,
                                    format_reloc);
            continue;
          }
        case 'd':
        case 'i':
          {
            /* Decimal fixnum. */
            Ibyte to_print[DECIMAL_PRINT_SIZE (EMACS_INT) + MAX_ICHAR_LEN + 1];
            Ibyte *cursor = to_print;
            Bytecount len;

            /* Print as unsigned if that's requested; handle various flags. */
            FIXNUM_SPEC_PREAMBLE (to_print, sizeof (to_print), 10,
                                  Vfixnum_to_majuscule_map);
            
            spec->number_flag = NUMBER_FLAG_NOTHING;

            len = fixnum_to_string_base_10 (to_print, sizeof (to_print),
                                            arg.l, Vfixnum_to_majuscule_map);

            if (arg.l < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                cursor += ichar_len ('-');
                len -= ichar_len ('-');
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, len, spec,
                                    format_reloc);
            continue;
          }
        case 'c':
          {
            /* Character. Accept fixnum arguments, but not other numbers. */
            Ibyte charbuf[MAX_ICHAR_LEN];
            Ichar a;
            
            if (UNBOUNDP (obj))
              {
                a = arg.l;
                obj = make_fixnum (a);
              }

            if (CHARP (obj))
              {
                a = XCHAR (obj);
              }
            else if (FIXNUMP (obj))
              {
		EMACS_INT fa = XREALFIXNUM (obj);
                if (!valid_ichar_p (fa))
                  {
                    UNGCPRO;
                    syntax_error ("Invalid integer value for %c spec", obj);
                  }
		a = (Ichar) fa;
              }
            else
              {
                UNGCPRO;
                syntax_error ("Value doesn't match char specifier", obj);
              }

            if (spec->precision != -1)
              {
                UNGCPRO;
                syntax_error ("Precision nonsensical for %c",
                              NILP (format_reloc) ?
                              make_string (format_nonreloc, format_length) :
                              format_reloc);
              }

            /* XEmacs; don't attempt (badly) to handle floats, bignums or
               ratios when 'c' is specified, error instead, "Format specifier
               doesn't match arg type". */
            byte_count += doprnt_1 (stream, charbuf, Qnil, 0,
                                    set_itext_ichar (charbuf, a),
                                    spec, format_reloc);
            continue;
          }
        case 'S':
          {
            Bytecount begin, delta;

	    /* Print the argument as a Lisp object using #'prin1. */
            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
              }

            if (spec->spec_length == (sizeof ("%S") - 1)
                && (begin = stream_extent_position (stream)) > -1)
              {
                /* Usual case; no modifiers, printing to something we can
                   query for a byte position--no need to create the string
                   which will be immediately garbage, print the object
                   directly to the stream instead. */
                print_internal (obj, stream, 1);
                delta = stream_extent_position (stream) - begin;
                if (!NILP (format_reloc)
                    && string_extent_info (format_reloc) != NULL)
                  {
                    stretch_string_extents (MARKERP (stream) ?
                                            Fmarker_buffer (stream)
                                            : stream, format_reloc, begin,
                                            spec->text_before +
                                            spec->text_before_len,
                                            spec->spec_length, delta);
                  }
                byte_count += delta;
              }
            else
              {
                obj = prin1_to_string (obj, 0);
                byte_count += doprnt_1 (stream, NULL, obj, 0,
                                        XSTRING_LENGTH (obj),
                                        spec, format_reloc);
              }
            continue;
          }
        case 'x':
        case 'X':
          {
            /* Hexadecimal fixnum. */
            Ibyte to_print[SIZEOF_EMACS_INT * 2 * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Lisp_Object table = (ch == 'X') ? Vfixnum_to_majuscule_map
              : Vfixnum_to_minuscule_map;
            Bytecount len;
            EMACS_UINT uval;
 
            /* Print as unsigned if that's requested; handle various flags. */
            FIXNUM_SPEC_PREAMBLE (to_print, sizeof (to_print), 16, table);

            if (arg.l < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                uval = -(arg.l);
              }
            else
              {
                uval = arg.l;
              }

            len = emacs_uint_to_string_rshift_1 (to_print, sizeof (to_print),
                                                 uval, 4, table);
            cursor = end - len;
            if (NUMBER_FLAG_C_LIKEP (spec->number_flag) && uval == 0)
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'o':
          {
            /* Octal fixnum. */
            Ibyte to_print[SIZEOF_EMACS_INT * 3 * MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;
            EMACS_UINT uval;

            FIXNUM_SPEC_PREAMBLE (to_print, sizeof (to_print), 8,
                                  Vfixnum_to_majuscule_map);
            if (arg.l < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                uval = -(arg.l);
              }
            else
              {
                uval = arg.l;
              }

            len = emacs_uint_to_string_rshift_1 (to_print, sizeof (to_print),
                                                 uval, 3,
                                                 Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                if (arg.l != 0)
                  {
                    /* Note, ASCII zero, not the current
                       Vfixnum_to_majuscule_map value for zero. */
                    *--cursor = '0';
                  }
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'f':
        case 'e':
        case 'E':
        case 'g':
        case 'G':
          {
            /* Printing floats; fall through to the C library's support,
               construct a spec to pass to snprintf().

               The size of this buffer is the length of this spec plus
               sufficient size to print two EMACS_INTS, needed if the minwidth
               and precision were supplied using repositioning specs. */
            const Bytecount constructed_size
              = spec->spec_length + DECIMAL_PRINT_SIZE (EMACS_INT) +
              DECIMAL_PRINT_SIZE (EMACS_INT) + 1;
            Ascbyte *constructed_spec = alloca_ascbytes (constructed_size);
            Ascbyte *p = constructed_spec, *text_to_print;

            /* See comment at float_to_string (). This is unsigned because
               otherwise the compiler defeats our overflow trapping
               below when optimizing. */
            EMACS_UINT alloca_sz = 350; 
            Bytecount text_to_print_length;

            if (!UNBOUNDP (obj))
              {
                /* This is a bit different from the rational cases, in that we
                   only want to rewrite the spec if we were explicitly handed
                   a bigfloat, we handle other number types ourselves. */
                if (BIGFLOATP (obj))
                  {
                    ch = rewrite_floating_spec (spec, obj);
                    goto reconsider;
                  }

                arg.d = extract_float (obj);
              }

            if ((alloca_sz += max (0, spec->minwidth) * MAX_ICHAR_LEN,
                 (Bytecount) alloca_sz < 350) ||
                (alloca_sz += max (0, spec->precision) * MAX_ICHAR_LEN,
                 (Bytecount) alloca_sz < 350))
              {
                UNGCPRO;
                signal_error (Qunimplemented,
                              "can't handle sum of minwidth and precision",
                              NILP (format_reloc) ?
                              make_string (format_nonreloc,
                                           format_length) : format_reloc);
              }

            /* We won't blow the stack, see ALLOCA() in lisp.h. */
            text_to_print = alloca_ascbytes ((Bytecount) alloca_sz);

            /* Mostly reconstruct the spec and use snprintf() to format
               the string. */
            *p++ = '%';
            switch (spec->sign_flag)
              {
              case SIGN_FLAG_PADCHAR:
                *p++ = ' ';
                break;
              case SIGN_FLAG_PLUS:
                *p++ = '+';
                break;
              case SIGN_FLAG_MINUS:
                text_checking_assert (0);
              case SIGN_FLAG_NOTHING:
                break;
              }
            if (spec->number_flag == NUMBER_FLAG_C_SYNTAX) *p++ = '#';
            if (spec->left_justify) *p++ = '-';
            if (spec->zero_flag) *p++ = '0';

            if (spec->minwidth >= 0)
              {
                p += fixnum_to_string_base_10 ((Ibyte *) p,
                                               constructed_size
                                               - (p - constructed_spec),
                                               spec->minwidth,
                                               Vfixnum_to_majuscule_ascii);
              }
            if (spec->precision >= 0)
              {
                *p++ = '.';
                p += fixnum_to_string_base_10 ((Ibyte *) p,
                                               constructed_size
                                               - (p - constructed_spec),
                                               spec->precision,
                                               Vfixnum_to_majuscule_ascii);
              }
            *p++ = ch;
            *p++ = '\0';
            text_checking_assert ((p - constructed_spec)
                                  < constructed_size);
#ifdef HAVE_SNPRINTF
            text_to_print_length = snprintf (text_to_print, alloca_sz,
                                             constructed_spec, arg.d);
#else
            text_to_print_length = sprintf (text_to_print,
                                            constructed_spec, arg.d);
#endif
            text_checking_assert (text_to_print_length
                                  < (Bytecount) alloca_sz);

            if (!NILP (format_reloc))
              {
                struct printf_spec minimal_spec;
                memset (&minimal_spec, 0, sizeof (minimal_spec));
                minimal_spec.precision = -1;
                minimal_spec.spec_length = spec->spec_length;
                minimal_spec.text_before = spec->text_before;
                minimal_spec.text_before_len = spec->text_before_len;

                byte_count += doprnt_1 (stream, (Ibyte *) text_to_print,
                                        Qnil, 0, text_to_print_length,
                                        &minimal_spec, format_reloc);
              }
            else
              {
                byte_count += doprnt_1 (stream, (Ibyte *) text_to_print,
                                        Qnil, 0, text_to_print_length,
                                        NULL, format_reloc);
              }
            continue;
          }
        case '%':
          {
            /* No field widths, no precisions, take any extents from the
               format string.  */
            byte_count += doprnt_1 (stream, format_nonreloc, format_reloc,
                                    spec->text_before + spec->text_before_len,
                                    ichar_len ('%'), NULL, Qnil);
            continue;
          }
        case '*':
          {
            /* The char '*' as converter means the field width, precision was
               specified as an argument.  Extract the data and forward it to
               the next spec, to which it will apply.  */
            struct printf_spec *nextspec = Dynarr_atp (specs, i + 1);

            /* No bignums or random Lisp objects, thanks, and restrict range
               as appropriate for a charcount. */
            check_integer_range (obj,
                                 make_fixnum (MOST_NEGATIVE_FIXNUM /
                                              MAX_ICHAR_LEN),
                                 make_fixnum (MOST_POSITIVE_FIXNUM /
                                              MAX_ICHAR_LEN));
            if (spec->forwarding_precisionp)
              {
                nextspec->precision = XREALFIXNUM (obj);
                nextspec->minwidth = spec->minwidth;
              }
            else
              {
                nextspec->minwidth = XREALFIXNUM (obj);
                if (XREALFIXNUM (obj) < 0)
                  {
                    spec->left_justify = 1;
                    nextspec->minwidth = - nextspec->minwidth;
                    spec->zero_flag = 0;
                  }
              }
            nextspec->left_justify = spec->left_justify;
            nextspec->sign_flag = spec->sign_flag;
            nextspec->number_flag = spec->number_flag;
            nextspec->zero_flag = spec->zero_flag;
            continue;
          }
        case 'b':
          {
            /* Binary fixnum. */
            Ibyte to_print[SIZEOF_EMACS_INT * 8 * MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;
            EMACS_UINT uval;

            FIXNUM_SPEC_PREAMBLE (to_print, sizeof (to_print), 2,
                                  Vfixnum_to_majuscule_map);
            if (arg.l < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                uval = -(arg.l);
              }
            else
              {
                uval = arg.l;
              }

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            len = emacs_uint_to_string_rshift_1 (to_print, sizeof (to_print),
                                                 uval, 1,
                                                 Vfixnum_to_majuscule_map);
            cursor = end - len;

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor, 
                                    spec, format_reloc);
            continue;
          }
        case 'p':
          {
            /* "Pointer". Print as a hex-format EMACS_UINT preceded by 0x. */
            Ibyte to_print[SIZEOF_EMACS_INT * 2 * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;

            if (!UNBOUNDP (obj))
              {
#ifdef HAVE_BIGNUM
                if (BIGNUMP (obj))
                  {
                    arg.ul = bignum_to_emacs_uint (XBIGNUM_DATA (obj));
                  }
                else
#endif
                if (FIXNUMP (obj))
                  {
                    arg.ul = XUINT (obj);
                  }
                else
                  {
                    dead_wrong_type_argument (Qintegerp, obj);
                  }
              }

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            spec->number_flag = NUMBER_FLAG_C_SYNTAX;
            spec->converter = 'x';
            len = emacs_uint_to_string_rshift_1 (to_print, sizeof (to_print),
                                                 arg.ul, 4,
                                                 Vfixnum_to_minuscule_map);
            cursor = end - len;
            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
#ifdef HAVE_BIGNUM
        case 'n': /* Decimal bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
                if (spec->hl_flag != HL_FLAG_NOTHING)
                  {
                    ch = rewrite_rational_spec (spec, &arg, &obj);
                    /* No need to change spec->converter, it isn't used as
                       part of #o or #x or anything of that sort. */
                    goto reconsider;
                  }
              }

            CHECK_BIGNUM (obj);

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = bignum_size_decimal (XBIGNUM_DATA (obj));
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            spec->number_flag = NUMBER_FLAG_NOTHING;

            len = bignum_to_string_1 (&to_print, &size, XBIGNUM_DATA (obj),
                                      10, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (bignum_sign (XBIGNUM_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'P': /* Octal bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
                if (spec->hl_flag != HL_FLAG_NOTHING)
                  {
                    spec->converter = 'o'; /* Needed for #o output. */
                    ch = rewrite_rational_spec (spec, &arg, &obj);
                    goto reconsider;
                  }
              }

            CHECK_BIGNUM (obj);

            signum = bignum_sign (XBIGNUM_DATA (obj));

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = bignum_size_octal (XBIGNUM_DATA (obj));
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            len = bignum_to_string_1 (&to_print, &size, XBIGNUM_DATA (obj),
                                      8, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (signum < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                if (signum == 0)
                  {
                    /* Note, ASCII zero, not the current
                       Vfixnum_to_majuscule_map value for zero. */
                    *--cursor = '0';
                  }
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'B': /* Base 2 bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
                if (spec->hl_flag != HL_FLAG_NOTHING)
                  {
                    spec->converter = 'b'; /* Needed for #b output. */
                    ch = rewrite_rational_spec (spec, &arg, &obj);
                    goto reconsider;
                  }
              }

            CHECK_BIGNUM (obj);

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = bignum_size_binary (XBIGNUM_DATA (obj));
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            len = bignum_to_string_1 (&to_print, &size, XBIGNUM_DATA (obj),
                                      2, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (bignum_sign (XBIGNUM_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'y':
        case 'Y': /* Hex bignum */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
                if (spec->hl_flag != HL_FLAG_NOTHING)
                  {
                    /* Needed for #x, 0x output. */
                    spec->converter = ch == 'y' ? 'x' : 'X';
                    ch = rewrite_rational_spec (spec, &arg, &obj);
                    goto reconsider;
                  }
              }

            CHECK_BIGNUM (obj);

            signum = bignum_sign (XBIGNUM_DATA (obj));

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = bignum_size_hex (XBIGNUM_DATA (obj));
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            len = bignum_to_string_1 (&to_print, &size, XBIGNUM_DATA (obj),
                                      16, ch == 'Y' ? Vfixnum_to_majuscule_map
                                      : Vfixnum_to_minuscule_map);
            cursor = end - len;

            if (signum < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }
            else if (NUMBER_FLAG_C_LIKEP (spec->number_flag) && signum == 0)
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, (const Ibyte *) cursor, Qnil,
                                    0, end - cursor, spec, format_reloc);
            continue;
          }
#endif /* HAVE_BIGNUM */
#ifdef HAVE_RATIO
        case 'm': /* Decimal ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            
            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
              }

            CHECK_RATIO (obj);

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = ratio_size_in_base (XRATIO_DATA (obj), 10);
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            spec->number_flag = NUMBER_FLAG_NOTHING;

            len = ratio_to_string_1 (&to_print, size, XRATIO_DATA (obj),
                                      10, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (ratio_sign (XRATIO_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'Q': /* Octal ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
              }

            CHECK_RATIO (obj);

            signum = ratio_sign (XRATIO_DATA (obj));

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = ratio_size_in_base (XRATIO_DATA (obj), 8);
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            len = ratio_to_string_1 (&to_print, size, XRATIO_DATA (obj),
                                      8, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (signum < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                if (signum == 0)
                  {
                    /* Note, ASCII zero, not the current
                       Vfixnum_to_majuscule_map value for zero. */
                    *--cursor = '0';
                  }
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'W': /* Base 2 ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
              }

            CHECK_RATIO (obj);

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = ratio_size_in_base (XRATIO_DATA (obj), 2);
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            len = ratio_to_string_1 (&to_print, size, XRATIO_DATA (obj),
                                      2, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (ratio_sign (XRATIO_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'v':
        case 'V': /* Hex ratio */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (UNBOUNDP (obj))
              {
                obj = arg.obj;
              }

            CHECK_RATIO (obj);

            signum = ratio_sign (XRATIO_DATA (obj));

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            size = ratio_size_in_base (XRATIO_DATA (obj), 16);
            to_print = alloca_ibytes (size);
            end = to_print + size; 

            len = ratio_to_string_1 (&to_print, size, XRATIO_DATA (obj),
                                      16, ch == 'V' ? Vfixnum_to_majuscule_map
                                      : Vfixnum_to_minuscule_map);
            cursor = end - len;

            if (signum < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }
            else if (NUMBER_FLAG_C_LIKEP (spec->number_flag) && signum == 0)
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_1 (stream, (const Ibyte *) cursor, Qnil,
                                    0, end - cursor, spec, format_reloc);
            continue;
          }
#endif /* HAVE_RATIO */
#ifdef HAVE_BIGFLOAT
        case 'F':
        case 'h':
        case 'H':
        case 'k':
        case 'K':
          if (UNBOUNDP (obj))
            {
              obj = arg.obj;
            }

          CHECK_BIGFLOAT (obj);
          {
            /* #### We don't even attempt to do this right. */
            Ibyte *text_to_print =
              (Ibyte *) bigfloat_to_string (XBIGFLOAT_DATA (obj), 10);
            byte_count += doprnt_1 (stream, text_to_print, Qnil,
                                    0, qxestrlen (text_to_print), spec,
                                    format_reloc);
            xfree (text_to_print);
            continue;
          }
#endif /* HAVE_BIGFLOAT */
        case 'u':
          {
            Ibyte buffer[(sizeof (INT_64_BIT) * 8 + 1) * MAX_ICHAR_LEN];

            Ibyte *end = buffer + sizeof (buffer), *cursor;
            Bytecount len;
            Lisp_Object table = spec->converter == 'x' ?
              Vfixnum_to_minuscule_map : Vfixnum_to_majuscule_map;
            UINT_16_BIT radix = 10;

            switch (spec->converter)
              {
              case 'b': radix = 2; break;
              case 'o': radix = 8; break;
              case 'x': radix = 16; break;
              case 'X': radix = 16; break;
              default: break;
              }

            FIXNUM_SPEC_PREAMBLE_1 (buffer, sizeof (buffer), radix, table, 1);

            len = uint_64_bit_to_string_general_1 (buffer, sizeof (buffer),
                                                   arg.u64, radix, table);
            cursor = end - len;

            if (radix == 10)
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }
            else if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                if (arg.u64 == 0)
                  {
                    if (8 == radix)
                      {
                        /* Note, ASCII zero, not the current
                           Vfixnum_to_majuscule_map value for zero. */
                        *--cursor = '0';
                      }
                    spec->number_flag = NUMBER_FLAG_NOTHING;
                  }
                else if (radix != 16)
                  {
                    spec->number_flag = NUMBER_FLAG_NOTHING;
                  }
              }

            byte_count += doprnt_1 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        default:
          {
            UNGCPRO;
	    syntax_error ("Invalid converter character", make_char (ch));
            break;
          }
        }
    }

  UNGCPRO;
  return byte_count;
}

/* Write a printf-style string to STREAM, an object accepted by
   output_string(), using FMT as the format string, and taking C arguments
   from the va_list VA.

   Conversion specifiers are interpreted as for Lisp (see the Lispref), with
   the two exceptions that an l (ell) length modifier to an integer format
   spec is interpreted to mean that the corresponding argument should be
   treated as having the bit length of an EMACS_INT, whether that length be
   32, 64, 128, and that %s means the corresponding argument is interpreted as
   an Ibyte pointer, to zero-terminated text.

   We also do not handle the $ repositioning specs; they make it harder to
   determine an upper bound on the number of specs. This can be revised if
   necessary, but it is unlikely to be that necessary on the C level.  */
void
write_fmt_string_va (Lisp_Object stream, const CIbyte *fmt, va_list va)
{
  const CIbyte *cursor = fmt;
  Bytecount len, speccount = 1;
  Elemcount nargs = 0;

  while (*cursor)
    {
      /* Count the number of format specs, so we can allocate the Dynarr on
         the stack for parse_doprnt_spec(). Note that * in a spec means an
         extra argument. */
      speccount += (*cursor == '%' || *cursor == '*'), cursor += 1;
      assert (*cursor != '$'); /* We don't handle the repositioning specs in
                                  emacs_snprintf, error early if someone
                                  passes us a format string with this.*/
    }

  len = cursor - fmt;

  if (speccount == 1)
    {
      write_string_1 (stream, (const Ibyte *) fmt, len);
    }
  else
    {
      printf_spec_dynarr specs;
      printf_spec *sbase = alloca_array (printf_spec, speccount);
      printf_arg *args;
  
      INIT_STACK_DYNARR (specs, printf_spec,
                         /* This is actually not exact, but will always be at
                            least the number of specs. */
                         speccount, sbase);

      parse_doprnt_spec (&specs, (const Ibyte *) fmt, len, &nargs);
      args = alloca_array (printf_arg, nargs);

      get_doprnt_c_args (args, nargs, &specs, va);
      emacs_doprnt (stream, (const Ibyte *) fmt, len, Qnil, &specs, NULL,
                    args);
    }
}

/* Write a printf-style string to STREAM; see output_string(). Arguments are C
   doubles, longs, uints, char *s, etc, rather than uniformly Lisp_Objects. */
void
write_fmt_string (Lisp_Object stream, const CIbyte *fmt, ...)
{
  va_list va;
  va_start (va, fmt);
  write_fmt_string_va (stream, fmt, va);
  va_end (va);
}

/* Write a printf-style string to STREAM, an object accepted by
   output_string(), using FMT as the format string, and taking Lisp_Object
   arguments from the va_list VA. */
void
write_fmt_string_lisp_va (Lisp_Object stream, const CIbyte *fmt, va_list va)
{
  Bytecount len = strlen (fmt);
  Elemcount nargs = 0, ii = 0;
  printf_spec_dynarr *specs = parse_doprnt_spec (NULL, (const Ibyte *) fmt,
                                                 len, &nargs);
  int count = record_unwind_protect_freeing_dynarr (specs);
  Lisp_Object *largs = alloca_array (Lisp_Object, nargs);
  struct gcpro gcpro1, gcpro2;

  for (ii = 0; ii < nargs; ii++)
    largs[ii] = va_arg (va, Lisp_Object);

  GCPRO2 (largs[0], stream);
  gcpro1.nvars = nargs;
  emacs_doprnt (stream, (const Ibyte *) fmt, len, Qnil, specs, largs, NULL);
  UNGCPRO;
  unbind_to (count);
}

/* Write a printf-style string to STREAM, where the arguments are Lisp objects
   and not C strings or integers; see output_string().

   We could implement this with write_fmt_string_lisp_va (stream, fmt, va),
   but write_fmt_string_lisp () itself is called far more often, and since
   write_fmt_string_lisp_va is externally visible, the compiler is unlikely to
   inline it. */
void
write_fmt_string_lisp (Lisp_Object stream, const CIbyte *fmt, ...)
{
  Bytecount len = strlen (fmt);
  Elemcount nargs = 0, ii = 0;
  printf_spec_dynarr *specs = parse_doprnt_spec (NULL, (const Ibyte *) fmt,
                                                 len, &nargs);
  int count = record_unwind_protect_freeing_dynarr (specs);
  Lisp_Object *largs = alloca_array (Lisp_Object, nargs);
  struct gcpro gcpro1, gcpro2;
  va_list va;

  va_start (va, fmt);
  for (ii = 0; ii < nargs; ii++)
    largs[ii] = va_arg (va, Lisp_Object);
  va_end (va);

  GCPRO2 (largs[0], stream);
  gcpro1.nvars = nargs;
  emacs_doprnt (stream, (const Ibyte *) fmt, len, Qnil, specs, largs, NULL);
  UNGCPRO;
  unbind_to (count);
}

/* Output portably to stderr or its equivalent (i.e. may be a console
   window under MS Windows); do external-format conversion and call GETTEXT
   on the format string.  Automatically flush when done.

   NOTE: CIbyte means "internal format" data.  This includes the "..."
   arguments.  For numerical arguments, we have to assume that vsprintf
   will be a good boy and format them as ASCII.  For Mule internal coding
   (and UTF-8 internal coding, if/when we get it), it is safe to pass
   string values in internal format to be formatted, because zero octets
   only occur in the NUL character itself.  Similarly, it is safe to pass
   pure ASCII literal strings for these functions.  *Everything else must
   be converted, including all external data.*

   This function is safe to use even when not initialized or when dying --
   we don't do conversion in such cases. */

void
stderr_out (const CIbyte *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  if (initialized && !inhibit_non_essential_conversion_operations)
    fmt = GETTEXT (fmt);

  write_fmt_string_va (Qexternal_debugging_output, fmt, args);
  va_end (args);
}

/* Output portably to stdout or its equivalent (i.e. may be a console
   window under MS Windows).  Works like stderr_out(). */
void
stdout_out (const CIbyte *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  if (initialized && !inhibit_non_essential_conversion_operations)
    fmt = GETTEXT (fmt);

  write_fmt_string_va (Qt, fmt, args);
  va_end (args);
}

/* Write a printf-style string to standard output, where the arguments are
   Lisp_Objects. */
void
stderr_out_lisp (const CIbyte *fmt, ...)
{
  va_list va;

  if (initialized && !inhibit_non_essential_conversion_operations)
    fmt = GETTEXT (fmt);

  va_start (va, fmt);
  write_fmt_string_lisp_va (Qexternal_debugging_output, fmt, va);
  va_end (va);
}

/* Return a Lisp string reflecting FORMAT_NONRELOC and VARGS, where VARGS
   reflects an array of Lisp_Objects and FORMAT_NONRELOC is a zero-terminated
   C string that *does not* come from Lisp string data. */
Lisp_Object
emacs_vsprintf_string_lisp (const CIbyte *format_nonreloc, va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream (), obj;
  struct gcpro gcpro1;

  GCPRO1 (stream);

  write_fmt_string_lisp_va (stream, format_nonreloc, vargs);

  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
  Lstream_delete (XLSTREAM (stream));
  RETURN_UNGCPRO (obj);
}

/* Like emacs_vsprintf_string_lisp() but accepts its extra args directly. */
Lisp_Object
emacs_sprintf_string_lisp (const CIbyte *format_nonreloc, ...)
{
  Lisp_Object obj;
  va_list va;

  va_start (va, format_nonreloc);
  obj = emacs_vsprintf_string_lisp (format_nonreloc, va);
  va_end (va);

  return obj;
}

/* Like emacs_vsprintf_string_lisp(), but writes into a malloc()ed memory
   block, stored in *RETVAL_OUT. Returned value is the length, without the
   terminating zero. */
Bytecount
emacs_vasprintf_lisp (Ibyte **retval_out, const CIbyte *format_nonreloc,
                      va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;
  Bytecount len;

  GCPRO1 (stream);

  write_fmt_string_lisp_va (stream, format_nonreloc, vargs);
  len = Lstream_byte_count (XLSTREAM (stream));
  *retval_out = xnew_ibytes (len + 1);
  memcpy (*retval_out, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  (*retval_out)[len] = '\0';
  Lstream_delete (XLSTREAM (stream));

  UNGCPRO;
  return len;
}

/* Like emacs_sprintf_string_lisp(), but writes into a malloc()ed memory
   block, stored in *RETVAL_OUT. Returned value is the length, without the
   terminating zero. */
Bytecount
emacs_asprintf_lisp (Ibyte **retval_out, const CIbyte *format_nonreloc,
                     ...)
{
  va_list va;
  Bytecount len;

  va_start (va, format_nonreloc);
  len = emacs_vasprintf_lisp (retval_out, format_nonreloc, va);
  va_end (va);

  return len;
}

/* vsprintf()-like replacement. Arguments are interpreted as C
   objects--doubles, char *s, EMACS_INTs, etc. Returns a Lisp string.  Data
   from Lisp strings is OK because we explicitly inhibit GC. */
Lisp_Object
emacs_vsprintf_string (const CIbyte *format, va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream (), obj;
  int count = begin_gc_forbidden ();

  write_fmt_string_va (stream, format, vargs);
  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
  Lstream_delete (XLSTREAM (stream));
  end_gc_forbidden (count);

  return obj;
}

/* sprintf()-like replacement. Arguments are interpreted as C objects
   --doubles, char *s, EMACS_INTs, etc. Returns a Lisp string.  Data from Lisp
   strings is OK because we explicitly inhibit GC. */
Lisp_Object
emacs_sprintf_string (const CIbyte *format, ...)
{
  va_list vargs;
  Lisp_Object retval;

  va_start (vargs, format);
  retval = emacs_vsprintf_string (format, vargs);
  va_end (vargs);
  return retval;
}

/* vasprintf() implementation. Arguments are interpreted as C
   objects--doubles, char *s, EMACS_INTs, etc.  Data from Lisp strings is OK
   because we explicitly inhibit GC.  Returns length (not including the
   terminating zero). Stores a pointer to be freed with free() into
   *RETVAL_OUT. */
Bytecount
emacs_vasprintf (Ibyte **retval_out, const CIbyte *format, va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  int count = begin_gc_forbidden ();
  Bytecount len;

  write_fmt_string_va (stream, format, vargs);

  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  *retval_out = xnew_ibytes (len + 1);
  memcpy (*retval_out, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  (*retval_out)[len] = '\0';
  Lstream_delete (XLSTREAM (stream));

  end_gc_forbidden (count);

  return len;
}

/* asprintf() implementation. Arguments are interpreted as C objects--doubles,
   char *s, EMACS_INTs, etc. Data from Lisp strings is OK because we
   explicitly inhibit GC.  Returns length (not including the terminating
   zero), Stores a pointer to be freed with free() into *RETVAL_OUT. */
Bytecount
emacs_asprintf (Ibyte **retval_out, const CIbyte *format, ...)
{
  va_list vargs;
  Bytecount len;

  va_start (vargs, format);
  len = emacs_vasprintf (retval_out, format, vargs);
  va_end (vargs);
  return len;
}

/* vsnprintf() replacement.  Writes output into OUTPUT, which has SIZE octets.
   Data from Lisp strings is OK because we cannot GC.

   Return the number of octets that would have been written were the buffer
   unlimited in size, which will be equivalent to the number of octets written
   if that number is less that SIZE - 1.

   Always zero-terminates. This differs from the standard C behaviour, which
   will not zero-terminate if the number of octets is greater than SIZE -
   1.  The terminating zero is not included in the returned value.

   We do not handle the $ repositioning specs; they make it harder to
   determine an upper bound on the number of specs. This can be revised if
   necessary, but it is unlikely to be that necessary on the C level.  */
Bytecount
emacs_vsnprintf (Ibyte *output, Bytecount size, const CIbyte *format,
                 va_list vargs)
{
  /* emacs_vsnprintf can be called from write_string_to_external_output_va()
     in print.c. That function is called in situations where XEmacs is about
     to crash, and to be as useful as possible, we need to avoid allocating
     Lisp objects or calling malloc (), whence the allocation of the fixed
     buffer lstream and the specs on the stack. This is not a practice that
     should be emulated anywhere else. */
  Bytecount retval, len, speccount = 1;
  const CIbyte *cursor = format;

  while (*cursor)
    {
      /* Count the number of format specs, so we can allocate the Dynarr on
         the stack for parse_doprnt_spec(). Note that * in a spec means an
         extra argument.
         We don't handle repositioning specs with '$' */
      speccount += (*cursor == '%' || *cursor == '*'), cursor += 1;
      assert (*cursor != '$'); /* We don't handle the repositioning specs in
                                  emacs_snprintf, error early if someone
                                  passes us a format string with this.*/
    }

  len = cursor - format;

  if (speccount == 1)
    {
      /* No actual format specs, and since we may be preparing for armageddon,
         there is some value to not going ahead with the emacs_doprnt() and
         instead just copying the data to where our caller wants it to go. */
      retval = len;
      /* For the same reason, use the more robust memmove() rather than
         memcpy(). */
      memmove (output, format, min (size - 1, len));
    }
  else
    {
      printf_spec_dynarr specs;
      printf_spec *sbase = alloca_array (printf_spec, speccount);
      printf_arg *args;
      Elemcount nargs;
      /* Beyond the upside of avoiding touching the Lisp allocation code,
         allocating this on the stack has the advantage that we're not bitten
         by the non-dumpability of lstreams, something that would mean
         separate code paths for dump time and normal runtime. */
      DECLARE_STACK_FIXED_BUFFER_LSTREAM (stream);

      INIT_STACK_FIXED_BUFFER_OUTPUT_STREAM (stream, output, size);

      INIT_STACK_DYNARR (specs, printf_spec,
                         /* This is actually not exact, but will always be at
                            least the number of specs. */
                         speccount, sbase);

      parse_doprnt_spec (&specs, (const Ibyte *) format, len, &nargs);
      /* Check we allocated enough on the stack for the specs. If we haven't,
         we've probably crashed already, though, since the dynarr code will
         have attempted to realloc stack-based data. */
      structure_checking_assert (sbase == specs.base);

      args = alloca_array (printf_arg, nargs);

      get_doprnt_c_args (args, nargs, &specs, vargs);

      retval = emacs_doprnt (stream, (const Ibyte *) format, len,
                             Qnil, &specs, NULL, args);
    }

  if (retval < size - 1)
    {
      output[retval] = '\0';
    }
  else
    {
      output[size - 1] = '\0';
    }

  return retval;
}

/* snprintf() replacement. Writes output into OUTPUT, which is a buffer
   comprising SIZE octets. Data from Lisp strings is OK because we cannot GC.

   Return the number of octets that would have been written were the buffer
   unlimited in size, which will be equivalent to the number of octets written
   if that number is less that SIZE - 1.

   Always zero-terminates. This differs from the standard C behaviour, which
   will not zero-terminate if the number of octets is greater than SIZE -
   1. The terminating zero is not included in the returned value. */
Bytecount
emacs_snprintf (Ibyte *output, Bytecount size, const CIbyte *format, ...)
{
  va_list vargs;
  Bytecount retval;

  va_start (vargs, format);
  retval = emacs_vsnprintf (output, size, format, vargs);
  va_end (vargs);

  return retval;
}

/* The implementation of #'format-into. Take a relocatable Lisp control string
   FORMAT_RELOC, together with an array of Lisp_Object arguments, and write
   into a stream, preserving extents as appropriate.

   All Lisp_Object arguments are GCPRO'd, including FORMAT_RELOC; this makes
   it acceptable to pass newly created objects into this function.

   Special case; if STREAM is the symbol Qstring, do not return STREAM,
   instead return a newly-constructed string reflecting FORMAT_RELOC and
   LARGS. */
Lisp_Object
format_into (Lisp_Object stream, Lisp_Object format_reloc, int nargs,
             const Lisp_Object *largs)
{
  Elemcount args_needed = 0;
  Bytecount format_length = XSTRING_LENGTH (format_reloc);
  printf_spec_dynarr *specs
    = parse_doprnt_spec (NULL, XSTRING_DATA (format_reloc),
                         format_length, &args_needed);
  int count = record_unwind_protect_freeing_dynarr (specs);
  Boolint stringp = EQ (stream, Qstring);
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (nargs < args_needed)
    {
      /* Allow too many args for string, but not too few */
      signal_error_1 (Qwrong_number_of_arguments,
                      list3 (Qformat, make_fixnum (nargs), format_reloc));
    }
#ifdef DEBUG_XEMACS
  else if (nargs > args_needed)
    {
      Lisp_Object argz[] = { build_ascstring ("Format string \""),
                             format_reloc,
                             build_ascstring ("\" takes "),
                             Fnumber_to_string (make_fixnum (args_needed),
                                                Qnil, Qnil),
                             build_ascstring (" arguments, "),
                             Fnumber_to_string (make_fixnum (nargs),
                                                Qnil, Qnil),
                             build_ascstring (" supplied.") };
      /* Don't use warn_when_safe, since that ultimately calls
         emacs_doprnt(). */
      warn_when_safe_lispobj (Qformat, Qinfo,
                              concatenate (countof (argz), argz, Qstring,
                                           0));
    
    }
#endif

  if (stringp)
    {
      stream = make_resizing_buffer_output_stream ();
    }

  GCPRO3 (largs[0], format_reloc, stream);
  gcpro1.nvars = nargs;

  emacs_doprnt (stream, NULL, format_length, format_reloc, specs, largs,
                NULL);

  UNGCPRO;
  unbind_to (count);

  if (stringp)
    {
      Lisp_Object obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
      Lstream_delete (XLSTREAM (stream));
      return obj;
    }

  return stream;
}

DEFUN ("format", Fformat, 1, MANY, 0, /*
Format a string out of a control-string and arguments.
The first argument is a control string.
The other arguments are substituted into it to make the result, a string.
It may contain %-sequences meaning to substitute the next argument.
%s means print all objects as-is, using `princ'.
%S means print all objects as s-expressions, using `prin1'.
%d or %i means print as a signed integer in decimal (%o octal, %x lowercase
  hex, %X uppercase hex, %b binary).

%c means print as a single character.
%f means print as a floating-point number in fixed notation (e.g. 785.200).
%e or %E means print as a floating-point number in scientific notation
  (e.g. 7.85200e+03).
%g or %G means print as a floating-point number in "pretty format";
  depending on the number, either %f or %e/%E format will be used, and
  trailing zeroes are removed from the fractional part.
The argument used for all but %s, %S, and %c must be a number.  It will be
  converted to an integer or a floating-point number as necessary.  In
  addition, the integer %-sequences accept character arguments as equivalent
  to the corresponding fixnums (see `char-int'), while the floating point
  sequences do not.

%$ means reposition to read a specific numbered argument; for example,
  %3$s would apply the `%s' to the third argument after the control string,
  and the next format directive would use the fourth argument, the
  following one the fifth argument, etc. (There must be a positive integer
  between the % and the $).

Zero or more of the flag characters `-', `+', ` ', `0', `!', '&', `~' and `#'
  may be specified between the optional repositioning spec and the conversion
  character; see below.

An optional minimum field width may be specified after any flag characters
  and before the conversion character; it specifies the minimum number of
  characters that the converted argument will take up.  Padding will be
  added on the left (or on the right, if the `-' flag is specified), as
  necessary.  Padding is with zeroes if the `0' flag is specified, with the
  character specified following the `!' flag if that is supplied (see below),
  or by default with spaces.

If the field width is specified as `*', the field width is assumed to have
  been specified as an argument.  Any repositioning specification that
  would normally specify the argument to be converted will now specify
  where to find this field width argument, not where to find the argument
  to be converted.  If there is no repositioning specification, the normal
  next argument is used.  The argument to be converted will be the next
  argument after the field width argument unless the precision is also
  specified as `*' (see below).

An optional period character and precision may be specified after any
  minimum field width.  It specifies the minimum number of digits to
  appear in %d, %i, %o, %x, and %X conversions (the number is padded
  on the left with zeroes as necessary); the number of digits printed
  after the decimal point for %f, %e, and %E conversions; the number
  of significant digits printed in %g and %G conversions; and the
  maximum number of non-padding characters printed in %s and %S
  conversions.  The default precision for floating-point conversions
  is six. Using a precision with %c is an error.
If the precision is specified as `*', the precision is assumed to have been
  specified as an argument.  The argument used will be the next argument
  after the field width argument, if any.  If the field width was not
  specified as an argument, any repositioning specification that would
  normally specify the argument to be converted will now specify where to
  find the precision argument.  If there is no repositioning specification,
  the normal next argument is used.

An optional length modifier may be specified after any minimum field width or
  precision.  The length modifiers available are `hh', `h', `l' (ell), `ll'
  \(ell-ell).  These mean that a following integer conversion character will
  print an integer truncated to eight, sixteen, thirty-two, or sixty-four
  bits, respectively.  Note that this contrasts with ANSI C, where the bit
  length depends on the machine choices for the bit width of various integer
  types.

An optional unsigned modifier, the character `u', may be specified after any
  length modifier and before an integer conversion character.  This specifies
  that the following integer conversion is to treat its argument as unsigned.
  If no length modifier is specified, this simply means that `format' will
  error if the corresponding integer is negative.  With a length modifier
  `format' will print a positive integer reflecting the twos' complement
  representation of the argument in the given number of bits. E.g. `(format
  "%hux" -1)' will return the string "ffff".  If no integer conversion
  character follows `u', the specification is regarded as equivalent to `ud',
  and the argument will be printed in decimal.

The ` ' and `+' flags mean prefix non-negative numbers with a space or
  plus sign, respectively.
The `#' flag means print numbers in an alternate, more verbose format:
  octal numbers begin with zero; hex numbers begin with a 0x or 0X;
  a decimal point is printed in %f, %e, and %E conversions even if no
  numbers are printed after it; and trailing zeroes are not omitted in
   %g and %G conversions.
The `&' flag is analogous to the `#' flag for rationals, but the syntax used
  to print numbers is that of Common Lisp, rather than C, so octal numbers are
  preceded by `#o', binary numbers by `#b' and hexadecimal numbers by `#X' or
  `#x' depending on the particular converter character specified.
The `!' flag is followed by a single character, to be used as a pad character
  instead of space.  It does not override the zero flag.
The `~' flag, when combined with `&' or `#', means to place any sign before
  the radix specifier.

Use %% to put a single % into the output.

Extent information in CONTROL-STRING and in ARGS are carried over into the
output, in the same way as `concatenate'.  Any text created by a character or
numeric %-sequence inherits the extents of the text around it, or of the text
abutting it if those extents' `start-open' and `end-open' properties have the
appropriate values.

arguments: (CONTROL-STRING &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object control_string = args[0];

  CHECK_STRING (control_string);
  return format_into (Qstring, control_string, nargs - 1, args + 1);
}

DEFUN ("format-into", Fformat_into, 2, MANY, 0, /*
Like `format', but write the constructed string into STREAM.

STREAM is an object accepted by `print' or `write-sequence' as an output
destination.  See the documentation of `standard-output'.  Note in particular
that a value for STREAM of `nil' means text will be written to the minibuffer,
rather than `format-into' returning a string, as is the case in Common Lisp.

Return STREAM.  See the documentation for `format' for details of
CONTROL-STRING and the other arguments.

As a special case, if STREAM is the symbol `string', `format-into' behaves as
does `format', returning a newly-constructed string reflecting CONTROL-STRING
and ARGS.

arguments: (STREAM CONTROL-STRING &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object stream = canonicalize_printcharfun (args[0]);
  Lisp_Object control_string = args[1];

  CHECK_STRING (control_string);
  /* #### Consider implementing the frame kludge of print_prepare (). */
  return format_into (stream, control_string, nargs - 2, args + 2);
}

/* If we were to implement GNU's #'format-message, this would be the place to
   do it. The design of #'format-message (and their `text-quoting-style') is
   wrong, though, since it replaces every backtick with a left single
   quotation mark and every apostrophe with a right single quotation mark. It
   is perfectly legitimate and expected that messages (and docstrings) contain
   both English text (where this replacement is sensible and correct) and
   examples of source code (where this replacement is actively unhelpful and
   likely to lead to confusion when examples don't run in *scratch*).

   The right design is for the XEmacs source code to use ‘ (U+2018) and ’
   (U+2019) when that is appropriate, ` and ' when those are appropriate.  It
   is easy enough to do the transformation of our source code
   semi-programmatically.

   If text-quoting-style is to do anything, it should transform ‘ to ` and ’
   to ' if it is clear the current redisplay device cannot handle the directed
   quotation marks. This could be implemented by parse_doprnt_spec() making
   new specs for each ‘ and ’ encountered.

   Now, given that we still (*still*) support non-Mule builds, which don't
   support the directed quotation marks characters, we can't do this, and so
   we should not implement this for the moment.

   Aidan Kehoe, Sa 19 Nov 2016 11:13:20 GMT */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_doprnt (void)
{
  DEFSUBR (Fnumber_to_string);
  DEFSUBR (Fformat);
  DEFSUBR (Fformat_into);
}
