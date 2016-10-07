/* Output like sprintf to resizing buffer.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.
   Rewritten by mly to use varargs.h.
   Rewritten from scratch by Ben Wing (February 1995) for Mule; expanded
   to full printf spec.
   Support for bignums, ratios, and bigfloats added April 2004 by Jerry James.

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

/* Synched up with: Rewritten by Ben Wing.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "lstream.h"


/* Radix handling for rationals. */

/* Print a signed NUMBER into BUFFER, which comprises SIZE octets, as a base
   (expt 2 POWOF2) string, using TABLE (usually Vfixnum_to_majuscule_map) to
   transform fixnums to character values. Start at the *end*, and return the
   length of the string written. Note that this usually means there is slack
   before the actual text wanted. Do not zero-terminate. Throw an assertion
   failure if text checking is turned on and the conversion overruns BUFFER.

   See fixnum_to_string() for a more reasonable function for callers. */
static Bytecount
fixnum_to_string_rshift_1 (Ibyte *buffer, Bytecount size, Fixnum number,
                           UINT_16_BIT powof2, Lisp_Object table)
{
  Ibyte *end = buffer + size, *cursor = end;
  const Ibyte *this_digit, *ftmdata = XSTRING_DATA (table);
  Boolint minusp = number < 0;
  EMACS_UINT uval = minusp ? -number : number, mask = (1 << powof2) - 1;

  text_checking_assert (XSTRING_LENGTH (table) >=
                        ((EMACS_INT) mask + 1) * MAX_ICHAR_LEN);

  while (uval)
    {
      this_digit = ftmdata + ((uval & mask) * MAX_ICHAR_LEN);
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
      uval >>= powof2;
    }

  if (end == cursor)
    {
      this_digit = ftmdata + 0;
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
    }

  if (minusp)
    {
      Ibyte chbuf[MAX_ICHAR_LEN];
      cursor -= set_itext_ichar (chbuf, '-');
      itext_copy_ichar (chbuf, cursor);
    }

  text_checking_assert (cursor >= buffer);

  return end - cursor;
}

/* Print an unsigned NUMBER into BUFFER, which comprises SIZE octets, as a
   base (expt 2 POWOF2) string, using TABLE (usually Vfixnum_to_majuscule_map)
   to transform fixnums to character values.  Start at the *end*, and return
   the length of the string written. Note that this usually means there is
   slack before the actual text wanted. Do not zero-terminate. Throw an
   assertion failure if text checking is turned on and the conversion overruns
   BUFFER.

   See fixnum_to_string() for a more reasonable function for callers. */
static Bytecount
emacs_uint_to_string_rshift_1 (Ibyte *buffer, Bytecount size, EMACS_UINT number,
                               UINT_16_BIT powof2, Lisp_Object table)
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

/* Print a signed NUMBER into BUFFER, which comprises SIZE octets, as a base
   RADIX string, using TABLE (usually Vfixnum_to_majuscule_map) to transform
   fixnums to character values.  Start at the *end*, and return the length of
   the string written. Note that this usually means there is slack before the
   actual text wanted. Do not zero-terminate. Throw an assertion failure if
   text checking is turned on and the conversion overruns BUFFER.

   See fixnum_to_string() for a more reasonable function for callers. */
static Bytecount
fixnum_to_string_general_1 (Ibyte *buffer, Bytecount size, Fixnum number,
                            Fixnum radix, Lisp_Object table)
{
  Ibyte *end = buffer + size, *cursor = end, *this_digit;
  Ibyte *ftmdata = XSTRING_DATA (table);
  Boolint minusp = number < 0;
  EMACS_UINT uval = minusp ? -number : number;

  text_checking_assert (XSTRING_LENGTH (Vfixnum_to_majuscule_map)
                        >= (radix) * MAX_ICHAR_LEN);

  while (uval)
    {
      this_digit = ftmdata + ((uval % radix) * MAX_ICHAR_LEN);
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
      uval /= radix;
    }

  if (end == cursor)
    {
      this_digit = ftmdata + 0;
      cursor -= itext_ichar_len (this_digit);
      itext_copy_ichar (this_digit, cursor);
    }

  if (minusp)
    {
      cursor -= ichar_itext_len ('-');
      set_itext_ichar (cursor, '-');
    }

  text_checking_assert (cursor >= buffer);

  return end - cursor;
}

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

/* Print NUMBER to BUFFER in base 10, starting with its most significant digit
   at the beginning, and zero-terminating. SIZE is the number of octets
   comprising BUFFER. TABLE describes a mapping from fixnum to characters and
   is usually Vfixnum_to_majuscule_map. Return length of the printed
   representation, without the zero termination. Throw an assertion failure if
   printing has overrun BUFFER.

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

   FLAGS can be RATIONAL_DOWNCASE, to print (e.g.) hexadecimal numbers as a-f
   rather than A-F, the default. Alternatively, it can be RATIONAL_FORCE_ASCII,
   to avoid language-specific digit characters (e.g. Persian, fullwidth
   Chinese). In this case (#### for the moment) RATIONAL_DOWNCASE is ignored and
   any alphabetic characters with case are returned with their upper case
   values.

   Return the length of the printed string, without the terminating zero. If
   assertions are enabled, throw an assertion failure if BUFFER overflows.

   See also bignum_to_string(), ratio_to_string(). */
Bytecount
fixnum_to_string (Ibyte *buffer, Bytecount size, Fixnum number,
                  UINT_16_BIT radix, int flags)
{
  Bytecount len = 0, slack;
  Lisp_Object table = (flags & RATIONAL_DOWNCASE)
    ? Vfixnum_to_minuscule_map
    : ((flags & RATIONAL_FORCE_ASCII) ? Vfixnum_to_majuscule_ascii
       : Vfixnum_to_majuscule_map);

  if (10 == radix)
    {
      return fixnum_to_string_base_10 (buffer, size, number, table);
    }
  else if ((number & (number - 1)) == 0) /* Power of two? */
    {
      EMACS_UINT powof2 = 0;

      text_checking_assert (radix != 0);

      while (radix != 1)
        {
          powof2++;
          radix >>= 1;
        }

      len = fixnum_to_string_rshift_1 (buffer, size - 1, number, powof2,
                                       table);
    }
  else
    {
      len = fixnum_to_string_general_1 (buffer, size - 1, number, radix,
                                        table);
    }

  slack = size - len;
  text_checking_assert (slack >= 0);
  if (slack)
    {
      /* There will be overlap if LEN > size / 2, use memmove(). */
      memmove (buffer, buffer + slack, len);
    }

  /* Add a trailing '\0'. */
  buffer[len] = '\0';
  return len;
}

extern bignum scratch_bignum;

static Bytecount
bignum_to_string_1 (Ibyte **buf, Bytecount *size_inout, bignum bn,
                    EMACS_UINT radix, Lisp_Object table)
{
  Boolint minusp = bignum_sign (bn) < 0, heap_allocp = size_inout < 0;
  Ibyte *buf1 = *size_inout > -1 ? *buf :
    ((*size_inout = 128 * MAX_ICHAR_LEN),
     (*buf = xnew_array (Ibyte, *size_inout)));
  Ibyte *end = buf1 + *size_inout, *cursor = end, *this_digit = NULL;
  Ibyte *ftmdata = XSTRING_DATA (table);

  if (minusp)
    {
      bignum_neg (scratch_bignum, bn);
      /* Reserve space for the minus sign in our accounting. */
      buf1 += ichar_itext_len ('-');
    }
  else
    {
      bignum_set (scratch_bignum, bn);
    }

  while ((bignum_sign (scratch_bignum)) != 0)
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
          buf1 = *buf + minusp * ichar_itext_len ('-');
        }
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
      Ibyte chbuf[MAX_ICHAR_LEN];
      cursor -= set_itext_ichar (chbuf, '-');
      itext_copy_ichar (this_digit, cursor);
    }

  text_checking_assert (cursor >= *buf);

  return end - cursor;
}

Bytecount
bignum_to_string (Ibyte **buffer_inout, Bytecount size, bignum number,
                  UINT_16_BIT radix, int flags)
{
  Bytecount len = 0, slack;
  Lisp_Object table = (flags & RATIONAL_DOWNCASE)
    ? Vfixnum_to_minuscule_map
    : ((flags & RATIONAL_FORCE_ASCII) ? Vfixnum_to_majuscule_ascii
       : Vfixnum_to_majuscule_map);

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

static Bytecount
ratio_to_string_1 (Ibyte **buf, Bytecount size, ratio rat, UINT_16_BIT base,
                   Lisp_Object table)
{
  Bytecount len;
  Ibyte *cursor;

  text_checking_assert (*buf);
  len = bignum_to_string_1 (buf, &size, ratio_denominator (rat),
                            base, table); 
  cursor = *buf + size - len;

  cursor -= ichar_itext_len ('/');
  len += set_itext_ichar (cursor, '/');
  size -= len;
  len += bignum_to_string_1 (buf, &size, ratio_numerator (rat), base, table);
  return len;
}

Bytecount
ratio_to_string (Ibyte **buffer_inout, Bytecount size, ratio number,
                 UINT_16_BIT radix, int flags)
{
  Bytecount len = 0, slack;
  Lisp_Object table = (flags & RATIONAL_DOWNCASE)
    ? Vfixnum_to_minuscule_map
    : ((flags & RATIONAL_FORCE_ASCII) ? Vfixnum_to_majuscule_ascii
       : Vfixnum_to_majuscule_map);

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

DEFUN ("number-to-string", Fnumber_to_string, 1, 1, 0, /*
Convert NUMBER to a string by printing it in decimal.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.
If supported, it may also be a ratio.
*/
       (number))
{
  CHECK_NUMBER (number);

  if (FLOATP (number))
    {
      Ascbyte pigbuf[350];	/* see comments in float_to_string */

      return make_string ((const Ibyte *) pigbuf,
                          float_to_string (pigbuf, XFLOAT_DATA (number)));
    }
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      Bytecount size, len;
      Ibyte *buf;
      Lisp_Object retval;

#ifdef bignum_size_decimal
      size = bignum_size_decimal (XBIGNUM_DATA (number));
      buf = alloca_ibytes (size);
#else
      size = -1;
      buf = NULL;
#endif
      len = bignum_to_string_1 (&buf, &size, XBIGNUM_DATA (number), 10,
                                Vfixnum_to_majuscule_map);
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
      Bytecount size = ratio_size_in_base (XRATIO_DATA (number), 10), len;
      Ibyte *buffer = alloca_ibytes (size);

      len = ratio_to_string_1 (&buffer, size, XRATIO_DATA (number), 10,
                               Vfixnum_to_majuscule_map);
      return make_string (buffer + size - len, len);
    }
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      Ascbyte *str = bigfloat_to_string (XBIGFLOAT_DATA (number), 10);
      Lisp_Object retval = build_ascstring (str);
      xfree (str);
      return retval;
    }
#endif

  {
    Ibyte buffer[DECIMAL_PRINT_SIZE (Fixnum)];

    return make_string (buffer,
                        fixnum_to_string_base_10 (buffer, sizeof (buffer),
                                                  XFIXNUM (number),
                                                  Vfixnum_to_majuscule_map));
  }
}

static const Ascbyte * const valid_flags = "-+ #0&~";

/* Don't add the bignum, ratio and bigfloat converters to these.
   valid_converters is just used to check format strings supplied by the user,
   and the bignum, ratio and bigfloat converters are only used internally
   within doprnt.c, they are not to be user-visible. */
static const Ascbyte * const valid_converters = "dic" "ouxX" "feEgG" "sS" "b";
static const Ascbyte * const int_converters = "dic";
static const Ascbyte * const base_converters = "oxXb";
static const Ascbyte * const double_converters = "feEgG";
static const Ascbyte * const string_converters = "sS";
#ifdef HAVE_BIGNUM
static const Ascbyte * const bignum_converters = "nPyYB";
#endif
#ifdef HAVE_RATIO
static const Ascbyte * const ratio_converters = "mQvVW";
#endif
#ifdef HAVE_BIGFLOAT
static const Ascbyte * const bigfloat_converters = "FhHkK";
#endif

typedef struct printf_spec printf_spec;
struct printf_spec
{
  Bytecount text_before; /* Position of the first character of the block of
                            literal text before this spec. */
  Bytecount text_before_len; /* Length of that text. */
  Ascbyte converter; /* Converter character, or 0 for dummy marker indicating
                        literal text at the end of the specification. */
  unsigned int left_justify:1;
  unsigned int number_flag:2;
  unsigned int zero_flag:1;
  unsigned int h_flag:1;
  unsigned int l_flag:1;
  unsigned int forwarding_precisionp:1;
  unsigned int sign_flag:2;
  unsigned int precision_details:2;
  Elemcount argnum; /* Which argument does this spec want?  This is one-based:
                       The first argument given is numbered 1, the second is
                       2, etc.  This is to handle %##$x-type specs. */
  Charcount minwidth;
  Charcount precision;
  Bytecount spec_length; /* Length of this format spec itself, starting at
                            text_before + text_before_len.  */
};

typedef union printf_arg printf_arg;
union printf_arg
{
  long l;
  unsigned long ul;
  double d;
  Ibyte *bp;
  Lisp_Object obj;
};

/* We maintain a list of all the % specs in the specification,
   along with the offset and length of the block of literal text
   before each spec.  In addition, we have a "dummy" spec that
   represents all the literal text at the end of the specification.
   Its converter is 0. */
typedef struct
{
  Dynarr_declare (struct printf_spec);
} printf_spec_dynarr;

typedef struct
{
  Dynarr_declare (union printf_arg);
} printf_arg_dynarr;

/* Enums, macros for doprnt_2(). */

/* What sign information should be displayed? */
enum sign_flag {
  SIGN_FLAG_NOTHING = 0,
  SIGN_FLAG_SPACE,
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

#define NUMBER_FLAG_C_LIKEP(flag) (flag >= NUMBER_FLAG_C_SYNTAX)

#define HANDLE_SIGN_FLAG(sf) do switch (sf)                     \
        {                                                       \
        case SIGN_FLAG_SPACE:                                   \
          Lstream_putc (lstr, ' '),                             \
            result_len += set_itext_ichar (chbuf, ' ');         \
          break;                                                \
        case SIGN_FLAG_PLUS:                                    \
          Lstream_putc (lstr, '+'),                             \
            result_len += set_itext_ichar (chbuf, '+');         \
          break;                                                \
        case SIGN_FLAG_MINUS:                                   \
          Lstream_putc (lstr, '-'),                             \
            result_len += set_itext_ichar (chbuf, '-');         \
          break;                                                \
        default:                                                \
          break;                                                \
        } while (0)

#define HANDLE_SIGN_AND_NUMBER(nf, sf)                              \
  do switch (nf)                                                    \
    {                                                               \
    case NUMBER_FLAG_NOTHING:                                       \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    case NUMBER_FLAG_C_SYNTAX:                                      \
      HANDLE_SIGN_FLAG (sf);                                        \
      Lstream_putc (lstr, '0'),                                     \
        result_len += set_itext_ichar (chbuf, '0');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      break;                                                        \
    case NUMBER_FLAG_LISP_SYNTAX:                                   \
      Lstream_putc (lstr, '#'),                                     \
        result_len += set_itext_ichar (chbuf, '#');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    case NUMBER_FLAG_MASOCHISM:                                     \
      Lstream_putc (lstr, '0'),                                     \
        result_len += set_itext_ichar (chbuf, '0');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    } while (0)

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
   If ZERO_FLAG is set, pad with 0's; otherwise pad with spaces.

   Handling of PRECISION depends on the PRECISON_DETAILS enum value.
   If it is PRECISION_MAX_CHARACTERS, the string is first truncated on the
   right to that many characters.
   If it is PRECISION_MIN_TOTAL_DIGITS, the string is padded on the left with
   zero characters if that is necessary to bring up its character count to
   that number given in PRECISION. This overrides ZERO_FLAG. There can be
   additional space padding as specified by MINWIDTH.

   If SIGN_FLAG is SIGN_FLAG_PLUS, output a plus between any space padding
   and before any zero padding. If it is SIGN_FLAG_MINUS, output a minus in
   that position. If it is SIGN_FLAG_SPACE, output a space.
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
doprnt_2 (Lisp_Object stream, const Ibyte *nonreloc, Lisp_Object reloc,
          Bytecount offset, Bytecount len,
          struct printf_spec *pfsp, Lisp_Object format_object)
{
  Lstream *lstr = XLSTREAM (stream);
  Bytecount result_len = 0, begin = Lstream_byte_count (lstr);
  const Ibyte *newnonreloc = ((NILP (reloc)) ? nonreloc : XSTRING_DATA (reloc));
  Ibyte chbuf [MAX_ICHAR_LEN];

  assert (!(EQ (reloc, format_object)) || NILP (reloc));

  if (pfsp != NULL)
    {
      Charcount spaces_to_add = -1, minwidth = pfsp->minwidth;
      Charcount maxlen = -1, insert_zeros_until = -1, zeros_to_add = -1;
      Boolint left_justify = pfsp->left_justify, zero_flag = pfsp->zero_flag;
      enum sign_flag sign_flag = (enum sign_flag) (pfsp->sign_flag);
      enum number_flag number_flag = (enum number_flag) (pfsp->number_flag);

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
             combine the former with SPACES_TO_ADD, which is managed by the
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
          spaces_to_add = minwidth - (cclen + zeros_to_add);

          if (maxlen >= 0 && (maxlen = min (maxlen, cclen), maxlen != cclen))
            {
              len = charcount_to_bytecount (newnonreloc + offset, maxlen);
              /* Truncation shouldn't happen with numbers. */
              text_checking_assert (sign_flag == SIGN_FLAG_NOTHING);
            }
        }

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
          while (spaces_to_add-- > 0)
            {
              Lstream_putc (lstr, '0'),
                result_len += set_itext_ichar (chbuf, '0');
            }
        }
      else
        {
          while (spaces_to_add-- > 0)
            Lstream_putc (lstr, ' '),
              result_len += set_itext_ichar (chbuf, ' ');
          /* Sign information comes after the spaces. */
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
        }

      while (zeros_to_add-- > 0)
        Lstream_putc (lstr, '0'), result_len += set_itext_ichar (chbuf, '0');

      Lstream_write (lstr, newnonreloc + offset, len);
      result_len += len;

      /* Padding at end to left-justify ... */
      if (left_justify)
        while (spaces_to_add-- > 0)
          Lstream_putc (lstr, zero_flag ? '0' : ' '),
            result_len += set_itext_ichar (chbuf, zero_flag ? '0' : ' ');

      if (!NILP (format_object) && string_extent_info (format_object) != NULL)
        {
          stretch_string_extents (stream, format_object, begin,
                                  pfsp->text_before + pfsp->text_before_len,
                                  pfsp->spec_length,
                                  Lstream_byte_count (lstr) - begin);
        }
    }
  else
    {
      Lstream_write (lstr, newnonreloc + offset, len);
      result_len += len;
    }
 
  if (!NILP (reloc) && string_extent_info (reloc) != NULL)
    {
      stretch_string_extents (stream, reloc, begin,
                              offset, len, Lstream_byte_count (lstr) - begin);
    }

  return result_len;
}

#define NEXT_ASCII_BYTE(ch)						\
  do {									\
    if (fmt == fmt_end)							\
      syntax_error ("Premature end of format string", Qunbound);	\
    ch = *fmt;								\
    if (ch >= 0200)							\
      syntax_error ("Non-ASCII character in format converter spec",	\
		    Qunbound);						\
    fmt++;								\
  } while (0)

#define RESOLVE_FLAG_CONFLICTS(spec)				\
  do {								\
    if (spec.number_flag == NUMBER_FLAG_LISP_SYNTAX &&          \
        spec.sign_flag == SIGN_FLAG_SPACE)                      \
      {                                                         \
        spec.sign_flag = SIGN_FLAG_NOTHING;                     \
      }                                                         \
    if (spec.zero_flag && spec.sign_flag == SIGN_FLAG_SPACE)    \
      spec.zero_flag = 0;					\
  } while (0)

static printf_spec_dynarr *
parse_doprnt_spec (const Ibyte *format, Bytecount format_length)
{
  const Ibyte *fmt = format;
  const Ibyte *fmt_end = format + format_length;
  printf_spec_dynarr *specs = Dynarr_new (printf_spec);
  Elemcount prev_argnum = 0;

  while (1)
    {
      struct printf_spec spec;
      const Ibyte *text_end;
      Ibyte ch;

      if (fmt == fmt_end)
	return specs;

      xzero (spec);
      text_end = (Ibyte *) memchr (fmt, '%', fmt_end - fmt);
      if (!text_end)
	text_end = fmt_end;
      spec.text_before = fmt - format;
      spec.text_before_len = text_end - fmt;
      spec.precision = -1;
      
      fmt = text_end;
      if (fmt != fmt_end)
	{
	  fmt++; /* skip over % */

	  /* A % is special -- no arg number.  According to ANSI specs,
	     field width does not apply to %% conversion. */
	  if (fmt != fmt_end && *fmt == '%')
	    {
	      spec.converter = '%';
              spec.spec_length = fmt - text_end;
	      Dynarr_add (specs, spec);
	      fmt++;
	      continue;
	    }

	  /* Is there a repositioning specifier? */
	  {
            Ibyte *fmt1;
            Lisp_Object posnum
              = parse_integer (fmt, &fmt1, fmt_end - fmt, 10, 1,
                               Vdigit_fixnum_ascii);

            if (FIXNUMP (posnum) && XREALFIXNUM (posnum) > 0
                && itext_ichar (fmt1) == '$')
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
	  }

	  /* Parse off any flags */
	  NEXT_ASCII_BYTE (ch);
	  while (strchr (valid_flags, ch))
	    {
	      switch (ch)
		{
		case '-': spec.left_justify  = 1; break;
		case '+': spec.sign_flag = SIGN_FLAG_PLUS; break;
		case ' ': 
                  if (spec.sign_flag != SIGN_FLAG_PLUS)
                    {
                      /* The plus flag overrides. */
                      spec.sign_flag = SIGN_FLAG_SPACE; 
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
                  /* So we get a helpful value on abort. */
 		default: text_checking_assert (format == NULL);
		}
	      NEXT_ASCII_BYTE (ch);
	    }

	  /* Parse off the minimum field width */
	  fmt--; /* back up */

	  /*
	   * * means the field width was passed as an argument.
	   * Mark the current spec as one that forwards its
	   * field width and flags to the next spec in the array.
	   * Then create a new spec and continue with the parsing.
	   */
	  if (fmt != fmt_end && *fmt == '*')
	    {
	      spec.converter = '*';
              spec.spec_length = fmt - text_end;
	      RESOLVE_FLAG_CONFLICTS(spec);
	      Dynarr_add (specs, spec);
	      xzero (spec);
	      spec.argnum = ++prev_argnum;
	      fmt++;
	    }
	  else
	    {
              Lisp_Object mwidth
                = parse_integer (fmt, (Ibyte **) (&fmt), fmt_end - fmt, 10, 1,
                                 Vdigit_fixnum_ascii);
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
	  NEXT_ASCII_BYTE (ch);
	  if (ch == '.')
	    {
	      /*
	       * * means the precision was passed as an argument.
	       * Mark the current spec as one that forwards its
	       * fieldwidth, flags and precision to the next spec in
	       * the array.  Then create a new spec and continue
	       * with the parse.
	       */
	      if (fmt != fmt_end && *fmt == '*')
		{
		  spec.converter = '*';
                  spec.spec_length = fmt - text_end;
		  spec.forwarding_precisionp = 1;
		  RESOLVE_FLAG_CONFLICTS(spec);
		  Dynarr_add (specs, spec);
		  xzero (spec);
		  spec.argnum = ++prev_argnum;
		  fmt++;
		}
	      else
		{
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
	      NEXT_ASCII_BYTE (ch);
	    }
	  else
	    /* No precision specified */
	    spec.precision = -1;

	  /* Parse off h or l flag */
	  if (ch == 'h' || ch == 'l')
	    {
	      if (ch == 'h')
		spec.h_flag = 1;
	      else
		spec.l_flag = 1;
	      NEXT_ASCII_BYTE (ch);
	    }

	  if (!strchr (valid_converters, ch))
	    syntax_error ("Invalid converter character", make_char (ch));
	  spec.converter = ch;
          spec.spec_length = fmt - text_end;
	}

      RESOLVE_FLAG_CONFLICTS(spec);
      Dynarr_add (specs, spec);
    }

  RETURN_NOT_REACHED(specs); /* suppress compiler warning */
}

static int
get_args_needed (printf_spec_dynarr *specs)
{
  int args_needed = 0;
  REGISTER int i;

  /* Figure out how many args are needed.  This may be less than
     the number of specs because a spec could be %% or could be
     missing (literal text at end of format string) or there
     could be specs where the field number is explicitly given.
     We just look for the maximum argument number that's referenced. */

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      char ch = Dynarr_at (specs, i).converter;
      if (ch && ch != '%')
	{
	  int argnum = Dynarr_at (specs, i).argnum;
	  if (argnum > args_needed)
	    args_needed = argnum;
	}
    }

  return args_needed;
}

static printf_arg_dynarr *
get_doprnt_args (printf_spec_dynarr *specs, va_list vargs)
{
  printf_arg_dynarr *args = Dynarr_new (printf_arg);
  union printf_arg arg;
  REGISTER int i;
  int args_needed = get_args_needed (specs);

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
	syntax_error ("No conversion spec for argument", make_fixnum (i));

      ch = spec->converter;

      if (strchr (int_converters, ch) || strchr (base_converters, ch))
	{
	  if (spec->l_flag)
	    arg.l = va_arg (vargs, long);
	  else
	    /* int even if ch == 'c' or spec->h_flag:
	       "the type used in va_arg is supposed to match the
	       actual type **after default promotions**."
	       Hence we read an int, not a short, if spec->h_flag. */
	    arg.l = va_arg (vargs, int);
	}
      else if ('u' == ch)
	{
	  if (spec->l_flag)
	    arg.ul = va_arg (vargs, unsigned long);
	  else
	    /* unsigned int even if spec->h_flag */
	    arg.ul = (unsigned long) va_arg (vargs, unsigned int);
	}
      else if (strchr (double_converters, ch))
	arg.d = va_arg (vargs, double);
      else if (strchr (string_converters, ch))
	arg.bp = va_arg (vargs, Ibyte *);
      /* We get here, e.g., if a repositioning field width or precision was
         supplied at the C level, something not implemented. */
      else ABORT ();

      Dynarr_add (args, arg);
    }

  return args;
}

static Ascbyte
rewrite_rational_spec (printf_spec *spec, Lisp_Object *obj)
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
  else if (BIGFLOATP (obj))
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, XBIGFLOAT_DATA (*obj));
      if (ch == 'u' && bignum_sign (scratch_bignum) < 0)
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
      switch (ch)
        {
        case 'i': case 'd': ch = 'm'; break;
        case 'o': ch = 'Q'; break;
        case 'x': ch = 'v'; break;
        case 'X': ch = 'V'; break;
        case 'b': ch = 'W'; break;
        case 'u':
          if (ratio_sign (XRATIO_DATA (*obj)) < 0)
            {
              dead_wrong_type_argument (Qnonnegativep, *obj);
            }
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
      if (spec->h_flag || spec->l_flag)
        {
          *obj = make_fixnum (bignum_to_long (XBIGNUM_DATA (*obj)));
        }
      else
        {
          switch (ch)
            {
            case 'i': case 'd': ch = 'n'; break;
            case 'o': ch = 'P'; break;
            case 'x': ch = 'y'; break;
            case 'X': ch = 'Y'; break;
            case 'b': ch = 'B'; break;
            case 'u':
              if (bignum_sign (XBIGNUM_DATA (*obj)) < 0)
                {
                  dead_wrong_type_argument (Qnatnump, *obj);
                }
              /* Fallthrough */
            default:
              ch = 'n';
            }
        }
    }
#endif

#ifdef HAVE_BIGNUM
  if (FIXNUMP (*obj) && ch == 'u' && XFIXNUM (*obj) < 0)
    {
      dead_wrong_type_argument (Qnatnump, *obj);
    }
#endif

  if (!NUMBERP (*obj))
    {
      syntax_error_2 ("Format specifier doesn't match arg type",
                      make_char (ch), *obj);
    }

  return ch;
}

static Ascbyte
rewrite_floating_spec (printf_spec *spec, printf_arg *arg, Lisp_Object *obj)
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

      /* Leave *obj alone. */
      return ch;
    }
#endif

  arg->d = extract_float (*obj);
  *obj = Qunbound;
  
  return ch;
}

#define FIXNUM_SPEC_PREAMBLE()                                          \
      do {                                                              \
        if (largs)                                                      \
          {                                                             \
            obj = largs[spec->argnum - 1];                              \
            if (!FIXNUMP (obj))                                         \
              {                                                         \
                ch = rewrite_rational_spec (spec,                       \
                                            largs + spec->argnum - 1);  \
                goto reconsider;                                        \
              }                                                         \
            val = XREALFIXNUM (obj);                                    \
          }                                                             \
        else                                                            \
          {                                                             \
            arg = Dynarr_at (args, spec->argnum - 1);                   \
            val = arg.l;                                                \
          }                                                             \
                                                                        \
        if (spec->precision > -1)                                       \
          {                                                             \
            spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;       \
            spec->zero_flag = 0;                                        \
          }                                                             \
                                                                        \
        if (spec->h_flag)                                               \
          {                                                             \
            val &= 0xFFFF;                                              \
          }                                                             \
        else if (spec->l_flag)                                          \
          {                                                             \
            val &= 0xFFFFFFFF;                                          \
          }                                                             \
      }                                                                 \
      while (0)

/* Most basic entry point into string formatting.

   Generate output from a format-spec (either a Lisp string FORMAT_RELOC, or a
   C string FORMAT_NONRELOC of length FORMAT_LENGTH -- which *MUST NOT* come
   from Lisp string data, unless GC is inhibited).  Output goes to STREAM.
   Returns the number of bytes stored into STREAM.  Arguments are either
   C-type arguments in va_list VARGS, or an array of Lisp objects in LARGS of
   size NARGS. (Behavior is different in the two cases -- you either get
   standard sprintf() behavior or `format' behavior.) */

static Bytecount
emacs_doprnt_1 (Lisp_Object stream, const Ibyte *format_nonreloc,
		Bytecount format_length, Lisp_Object format_reloc,
		int nargs, Lisp_Object *largs, va_list vargs)
{
  printf_spec_dynarr *specs = 0;
  printf_arg_dynarr *args = 0;
  REGISTER int i;
  Bytecount byte_count = 0;
  int count;

  if (!NILP (format_reloc))
    {
      format_nonreloc = XSTRING_DATA (format_reloc);
      format_length = XSTRING_LENGTH (format_reloc);
    }
  else if (format_length < 0)
    {
      format_length = qxestrlen (format_nonreloc);
    }

  specs = parse_doprnt_spec (format_nonreloc, format_length);
  count = record_unwind_protect_freeing_dynarr (specs);

  if (largs)
    {
      int args_needed = get_args_needed (specs);
      /* allow too many args for string, but not too few */
      if (nargs < args_needed)
        {
          signal_error_1 (Qwrong_number_of_arguments,
                          list3 (Qformat,
                                 make_fixnum (nargs),
                                 !NILP (format_reloc) ? format_reloc :
                                 make_string (format_nonreloc,
                                              format_length)));
        }
#ifdef DEBUG_XEMACS
      else if (nargs > args_needed)
        {
          Lisp_Object argz[] = { build_ascstring ("Format string \""),
                                 !NILP (format_reloc) ? format_reloc :
                                 make_string (format_nonreloc,
                                              format_length),
                                 build_ascstring ("\" takes "),
                                 Fnumber_to_string (make_fixnum (args_needed)),
                                 build_ascstring (" arguments, "),
                                 Fnumber_to_string (make_fixnum (nargs)),
                                 build_ascstring (" supplied.") };
          /* Don't use warn_when_safe, since that ultimately calls
             emacs_doprnt_1(). */
          warn_when_safe_lispobj (Qformat, Qinfo,
                                  concatenate (countof (argz), argz, Qstring,
                                               0));
        }
#endif
    }
  else
    {
      args = get_doprnt_args (specs, vargs);
      record_unwind_protect_freeing_dynarr (args);
    }

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      struct printf_spec *spec = Dynarr_atp (specs, i);
      union printf_arg arg;
      Ascbyte ch;
      Lisp_Object obj = Qnil;

      /* Copy the text before */
      if (!NILP (format_reloc)) /* refetch in case of GC below */
        {
          format_nonreloc = XSTRING_DATA (format_reloc);
        }

      byte_count += doprnt_2 (stream, format_nonreloc, format_reloc,
                              spec->text_before, spec->text_before_len, NULL,
                              Qnil);
      ch = spec->converter;

      /* These are organised roughly in decreasing order of frequency. */ 
    reconsider:
      switch (ch)
        {
        case 0:
          {
            /* This is the trailing spec, just present so the text_before gets
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
            Lisp_Object ls = Qnil;
            struct gcpro gcpro1;

            if (!largs)
              {
                string = Dynarr_at (args, spec->argnum - 1).bp;
                text_checking_assert (string != NULL);
                string_len = qxestrlen (string);
              }
            else
              {
                obj = largs[spec->argnum - 1];

                if (STRINGP (obj))
                  ls = obj;
                else if (SYMBOLP (obj))
                  ls = XSYMBOL (obj)->name;
                else
                  {
                    /* Convert to string using princ. */
                    ls = prin1_to_string (obj, 1);
                  }
                string = XSTRING_DATA (ls);
                string_len = XSTRING_LENGTH (ls);
              }

            GCPRO1 (ls);
            doprnt_2 (stream, string, ls, 0, string_len, spec, format_reloc);
            UNGCPRO;
            continue;
          }
        case 'd':
          {
            /* Decimal fixnum. */
            Ibyte to_print[DECIMAL_PRINT_SIZE (EMACS_INT) * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1], *cursor = to_print;
            Bytecount len;
            EMACS_INT val;

            FIXNUM_SPEC_PREAMBLE();
            
            spec->number_flag = NUMBER_FLAG_NOTHING;

            len = fixnum_to_string_base_10 (to_print, sizeof (to_print),
                                            val, Vfixnum_to_majuscule_map);

            if (val < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor);
                len -= ichar_itext_len ('-');
                
              }

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, len, spec,
                                    format_reloc);
            continue;
          }
        case 'i':
          {
            /* Decimal fixnum. Deletion pending, since it's equivalent to 'd',
               but kept here to allow access to long_to_string () for
               comparative testing.  */
            Ibyte to_print[DECIMAL_PRINT_SIZE (EMACS_INT) * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1], *cursor = to_print;
            Bytecount len;
            EMACS_INT val;

            FIXNUM_SPEC_PREAMBLE();
            
            spec->number_flag = NUMBER_FLAG_NOTHING;

            len = long_to_string (to_print, val);

            if (val < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor);
                len -= ichar_itext_len ('-');
                
              }

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, len, spec,
                                    format_reloc);
            continue;
          }
        case 'c':
          {
            /* Character. Accept fixnum arguments, but not other numbers. */
            Ibyte charbuf[MAX_ICHAR_LEN];
            Ichar a = CHAR_CODE_LIMIT; /* Not a valid ichar. */

            if (!largs)
              {
                a = (Ichar) Dynarr_at (args, spec->argnum - 1).l;
                obj = make_fixnum (a);
              }
            else
              {
                obj = largs[spec->argnum - 1];
              }

            if (CHARP (obj))
              {
                a = XCHAR (obj);
              }
            else if (FIXNUMP (obj))
              {
                a = XREALFIXNUM (obj);
                if (!valid_ichar_p (a))
                  {
                    syntax_error ("Invalid integer value for %c spec", obj);
                  }
              }
            else
              {
                syntax_error ("Value doesn't match char specifier", obj);
              }

            if (spec->precision != -1)
              {
                syntax_error ("Precision nonsensical for %c",
                            NILP (format_reloc) ?
                            make_string (format_nonreloc, format_length) :
                            format_reloc);
            }

            /* XEmacs; don't attempt (badly) to handle floats, bignums or
               ratios when 'c' is specified, error instead, "Format specifier
               doesn't match arg type". */
            byte_count += doprnt_2 (stream, charbuf, Qnil, 0,
                                    set_itext_ichar (charbuf, a),
                                    spec, format_reloc);
            continue;
          }
        case 'S':
          {
	    /* Print the argument as a Lisp object using #'prin1. */
            struct gcpro gcpro1;

            if (!largs)
              {
                obj = Dynarr_at (args, spec->argnum - 1).obj;
              }
            else
              {
                obj = largs[spec->argnum - 1];
              }

            obj = prin1_to_string (obj, 0);

            GCPRO1 (obj);
            byte_count += doprnt_2 (stream, NULL, obj, 0, XSTRING_LENGTH (obj),
                                    spec, format_reloc);
            UNGCPRO;
            continue;
          }
        case 'x':
        case 'X':
          {
            /* Hexadecimal fixnum. */
            Ibyte to_print[SIZEOF_EMACS_INT * 2 * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;
            EMACS_INT val;
 
            FIXNUM_SPEC_PREAMBLE();

            len = fixnum_to_string_rshift_1 (to_print, sizeof (to_print),
                                             val, 4,
                                             ch == 'X' ?
                                             Vfixnum_to_majuscule_map
                                             : Vfixnum_to_minuscule_map);
            cursor = end - len;
            if (val < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }
            else if (NUMBER_FLAG_C_LIKEP (spec->number_flag) && val == 0)
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'o':
          {
            /* Octal fixnum. */
            Ibyte to_print[SIZEOF_EMACS_INT * 3 * MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;
            EMACS_INT val;

            FIXNUM_SPEC_PREAMBLE();

            len = fixnum_to_string_rshift_1 (to_print, sizeof (to_print),
                                             val, 3, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (val < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                if (val != 0)
                  {
                    /* Note, ASCII zero, not the current
                       Vfixnum_to_majuscule_map value for zero. */
                    *--cursor = '0';
                  }
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            byte_count += doprnt_2 (stream, (const Ibyte *)cursor, Qnil,
                                    0, end - cursor, spec, format_reloc);
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

               The size Length of this spec plus sufficient size to print two
               EMACS_INTS, needed if the minwidth and precision were supplied
               using repositioning specs. */
            Ascbyte constructed_spec[spec->spec_length + 
                                     DECIMAL_PRINT_SIZE (EMACS_INT) +
                                     DECIMAL_PRINT_SIZE (EMACS_INT) + 1];
            Ascbyte *p = constructed_spec, *text_to_print;

            /* See comment at float_to_string (). This is unsigned because
               otherwise the compiler defeats our overflow trapping
               below when optimizing. */
            EMACS_UINT alloca_sz = 350; 
            Bytecount text_to_print_length;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                if (FLOATP (obj))
                  {
                    arg.d = XFLOAT_DATA (obj);
                  }
                else if (!UNBOUNDP (obj)) /* If it's Qunbound, it's already
                                             rewritten, use arg.d */
                  {
                    ch = rewrite_floating_spec (spec, &arg,
                                                largs + spec->argnum - 1);
                    goto reconsider;
                  }
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
              }

            if ((alloca_sz += max (0, spec->minwidth) * MAX_ICHAR_LEN,
                 (Bytecount) alloca_sz < 350) ||
                (alloca_sz += max (0, spec->precision) * MAX_ICHAR_LEN,
                 (Bytecount) alloca_sz < 350))
              {
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
              case SIGN_FLAG_SPACE:
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
                p += long_to_string (p, spec->minwidth);
              }
            if (spec->precision >= 0)
              {
                *p++ = '.';
                p += long_to_string (p, spec->precision);
              }
            *p++ = ch;
            *p++ = '\0';
            text_checking_assert ((p - constructed_spec)
                                  < (signed) (sizeof (constructed_spec)));

            text_to_print_length = snprintf (text_to_print, alloca_sz,
                                             constructed_spec, arg.d);
            text_checking_assert (text_to_print_length
                                  < (Bytecount) alloca_sz);

            if (!NILP (format_reloc))
              {
                struct printf_spec minimal_spec;
                bzero (&minimal_spec, sizeof (minimal_spec));
                minimal_spec.precision = -1;
                minimal_spec.spec_length = spec->spec_length;
                minimal_spec.text_before = spec->text_before;
                minimal_spec.text_before_len = spec->text_before_len;

                byte_count += doprnt_2 (stream, (Ibyte *) text_to_print,
                                        Qnil, 0, text_to_print_length,
                                        &minimal_spec, format_reloc);
              }
            else
              {
                byte_count += doprnt_2 (stream, (Ibyte *) text_to_print,
                                        Qnil, 0, text_to_print_length,
                                        NULL, format_reloc);
              }
            continue;
          }
        case '%':
          {
            Ibyte chbuf[MAX_ICHAR_LEN];
            /* No field widths, no precisions, take any extents from the
               format string.  */
            byte_count += doprnt_2 (stream, format_nonreloc, format_reloc,
                                    spec->text_before + spec->text_before_len,
                                    set_itext_ichar (chbuf, '%'), NULL, Qnil);
            continue;
          }
        case '*':
          {
            /* The char '*' as converter means the field width, precision was
               specified as an argument.  Extract the data and forward it to
               the next spec, to which it will apply.  */
            struct printf_spec *nextspec = Dynarr_atp (specs, i + 1);
            obj = largs[spec->argnum - 1];

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
            EMACS_INT val;

            FIXNUM_SPEC_PREAMBLE();

            if (NUMBER_FLAG_C_LIKEP (spec->number_flag))
              {
                spec->number_flag = NUMBER_FLAG_NOTHING;
              }

            len = fixnum_to_string_rshift_1 (to_print, sizeof (to_print),
                                             val, 1, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (val < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_2 (stream, cursor, Qnil,
                                    0, end - cursor, spec, format_reloc);
            continue;
          }
        case 'u':
          {
            /* Unsigned long. Fixnums are treated as if they don't have tag
               bits. */
            Ibyte to_print[SIZEOF_EMACS_INT * 2 * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1];
            Ibyte *cursor;
            Bytecount len;

            if (largs)
              {
                obj = largs[spec->argnum - 1];

#ifdef HAVE_BIGNUM
                text_checking_assert (!BIGNUMP (obj));
#endif
                if (FIXNUMP (obj))
                  {
#ifdef HAVE_BIGNUM
                    /* #### Can't we make a better design decision here? */
                    if (XFIXNUM (obj) < 0)
                      {
                        dead_wrong_type_argument (Qnatnump, obj);
                      }
#endif
                    arg.ul = XUINT (obj);
                  }
                else
                  {
                    dead_wrong_type_argument (Qintegerp, obj);
                  }
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
              }

            if (spec->precision > -1)
              {
                spec->precision_details = PRECISION_MIN_TOTAL_DIGITS;
                spec->zero_flag = 0;
              }

            /* Don't bother implementing unsigned_f_to_s_base_10, print the
               high-order digits and the lowest-order digit separately
               instead.  */
            len = fixnum_to_string_base_10 (to_print, sizeof (to_print),
                                            arg.ul / 10,
                                            Vfixnum_to_majuscule_map);
            cursor = to_print + len;
            if (arg.ul)
              {
                len
                  += itext_copy_ichar (XSTRING_DATA (Vfixnum_to_majuscule_map)
                                       + ((arg.ul % 10) * MAX_ICHAR_LEN),
                                       cursor);
              }
            byte_count += doprnt_2 (stream, to_print, Qnil, 0, len, spec,
                                    format_reloc);
            continue;
          }
        case 'p':
          {
            /* "Pointer". Print as a hex-format EMACS_UINT preceded by 0x. */
            Ibyte to_print[SIZEOF_EMACS_INT * 2 * MAX_ICHAR_LEN
                           + MAX_ICHAR_LEN + 1];
            Ibyte *end = to_print + sizeof (to_print), *cursor;
            Bytecount len;

            if (largs)
              {
                obj = largs[spec->argnum - 1];

#ifdef HAVE_BIGNUM
                if (BIGNUMP (obj))
                  {
                    arg.ul = bignum_to_emacs_int (XBIGNUM_DATA (obj));
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
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
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
            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
#ifdef HAVE_BIGNUM
        case 'n': /* Decimal bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (BIGNUMP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'P': /* Octal bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (BIGNUMP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'B': /* Base 2 bignum. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (BIGNUMP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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
                                      8, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (bignum_sign (XBIGNUM_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'y':
        case 'Y': /* Hex bignum */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (BIGNUMP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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
                                      16, ch == 'X' ? Vfixnum_to_majuscule_map
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

            byte_count += doprnt_2 (stream, (const Ibyte *) cursor, Qnil,
                                    0, end - cursor, spec, format_reloc);
            continue;
          }
#endif /* HAVE_BIGNUM */
#ifdef HAVE_RATIO
        case 'm': /* Decimal ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            
            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (RATIOP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'Q': /* Octal ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (RATIOP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'W': /* Base 2 ratio. */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (RATIOP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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
                                      8, Vfixnum_to_majuscule_map);
            cursor = end - len;

            if (ratio_sign (XRATIO_DATA (obj)) < 0)
              {
                spec->sign_flag = SIGN_FLAG_MINUS;
                /* Go forward past the minus. */
                INC_IBYTEPTR (cursor); 
              }

            byte_count += doprnt_2 (stream, cursor, Qnil, 0, end - cursor,
                                    spec, format_reloc);
            continue;
          }
        case 'v':
        case 'V': /* Hex ratio */
          {
            Ibyte *to_print, *cursor, *end;
            Bytecount len, size;
            int signum;

            if (largs)
              {
                obj = largs[spec->argnum - 1];
                text_checking_assert (RATIOP (obj));
              }
            else
              {
                arg = Dynarr_at (args, spec->argnum - 1);
                obj = arg.obj;
              }

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
                                      16, ch == 'X' ? Vfixnum_to_majuscule_map
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

            byte_count += doprnt_2 (stream, (const Ibyte *) cursor, Qnil,
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
          {
            /* #### We don't even attempt to do this right. */
            Ibyte *text_to_print =
              (Ibyte *) bigfloat_to_string (XBIGFLOAT_DATA (arg.obj), 10);
            byte_count += doprnt_2 (stream, text_to_print, Qnil,
                                    0, qxestrlen (text_to_print), spec,
                                    format_reloc);
            xfree (text_to_print);
            continue;
          }
#endif /* HAVE_BIGFLOAT */
        default:
          {
            text_checking_assert (0);
            break;
          }
        }
    }
      
  unbind_to (count);
  return byte_count;
}

/* Basic external entry point into string formatting.  See
 emacs_doprnt_1().
 */

Bytecount
emacs_doprnt_va (Lisp_Object stream, const Ibyte *format_nonreloc,
		 Bytecount format_length, Lisp_Object format_reloc,
		 va_list vargs)
{
  return emacs_doprnt_1 (stream, format_nonreloc, format_length,
			 format_reloc, 0, 0, vargs);
}

/* Basic external entry point into string formatting.  See
 emacs_doprnt_1().
 */

Bytecount
emacs_doprnt (Lisp_Object stream, const Ibyte *format_nonreloc,
	      Bytecount format_length, Lisp_Object format_reloc,
	      int nargs, const Lisp_Object *largs, ...)
{
  va_list vargs;
  Bytecount val;
  va_start (vargs, largs);
  val = emacs_doprnt_1 (stream, format_nonreloc, format_length,
			format_reloc, nargs, (Lisp_Object *) largs, vargs);
  va_end (vargs);
  return val;
}

/* Similar to `format' in that its arguments are Lisp objects rather than C
   objects. (For the versions that take C objects, see the
   emacs_[v]sprintf... functions below.) Accepts the format string as
   either a C string (FORMAT_NONRELOC, which *MUST NOT* come from Lisp
   string data, unless GC is inhibited) or a Lisp string (FORMAT_RELOC).
   Return resulting formatted string as a Lisp string.

   All arguments are GCPRO'd, including FORMAT_RELOC; this makes it OK to
   pass newly created objects into this function (as often happens).

   #### It shouldn't be necessary to specify the number of arguments.
   This would require some rewriting of the doprnt() functions, though.
   */

Lisp_Object
emacs_vsprintf_string_lisp (const CIbyte *format_nonreloc,
			    Lisp_Object format_reloc, int nargs,
			    const Lisp_Object *largs)
{
  Lisp_Object stream;
  Lisp_Object obj;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (largs[0], format_reloc);
  gcpro1.nvars = nargs;

  stream = make_resizing_buffer_output_stream ();
  emacs_doprnt (stream, (Ibyte *) format_nonreloc, format_nonreloc ?
		strlen (format_nonreloc) : 0,
		format_reloc, nargs, largs);
  Lstream_flush (XLSTREAM (stream));
  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
  Lstream_delete (XLSTREAM (stream));
  UNGCPRO;
  return obj;
}

/* Like emacs_vsprintf_string_lisp() but accepts its extra args directly
   (using variable arguments), rather than as an array. */

Lisp_Object
emacs_sprintf_string_lisp (const CIbyte *format_nonreloc,
			   Lisp_Object format_reloc, int nargs, ...)
{
  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
  va_list va;
  int i;
  Lisp_Object obj;

  va_start (va, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (va, Lisp_Object);
  va_end (va);
  obj = emacs_vsprintf_string_lisp (format_nonreloc, format_reloc, nargs,
				    args);
  return obj;
}

/* Like emacs_vsprintf_string_lisp() but returns a malloc()ed memory block.
   Return length out through LEN_OUT, if not null. */

Ibyte *
emacs_vsprintf_malloc_lisp (const CIbyte *format_nonreloc,
			    Lisp_Object format_reloc, int nargs,
			    const Lisp_Object *largs, Bytecount *len_out)
{
  Lisp_Object stream;
  Ibyte *retval;
  Bytecount len;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (largs[0], format_reloc);
  gcpro1.nvars = nargs;

  stream = make_resizing_buffer_output_stream ();
  emacs_doprnt (stream, (Ibyte *) format_nonreloc, format_nonreloc ?
		strlen (format_nonreloc) : 0,
		format_reloc, nargs, largs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  retval = xnew_ibytes (len + 1);
  memcpy (retval, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  retval[len] = '\0';
  Lstream_delete (XLSTREAM (stream));

  if (len_out)
    *len_out = len;
  UNGCPRO;
  return retval;
}

/* Like emacs_sprintf_string_lisp() but returns a malloc()ed memory block.
   Return length out through LEN_OUT, if not null. */

Ibyte *
emacs_sprintf_malloc_lisp (Bytecount *len_out, const CIbyte *format_nonreloc,
			   Lisp_Object format_reloc, int nargs, ...)
{
  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
  va_list va;
  int i;
  Ibyte *retval;

  va_start (va, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (va, Lisp_Object);
  va_end (va);
  retval = emacs_vsprintf_malloc_lisp (format_nonreloc, format_reloc, nargs,
				       args, len_out);
  return retval;
}

/* vsprintf()-like replacement.  Returns a Lisp string.  Data
   from Lisp strings is OK because we explicitly inhibit GC. */

Lisp_Object
emacs_vsprintf_string (const CIbyte *format, va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Lisp_Object obj;
  int count = begin_gc_forbidden ();

  emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
		   vargs);
  Lstream_flush (XLSTREAM (stream));
  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
  Lstream_delete (XLSTREAM (stream));
  end_gc_forbidden (count);
  return obj;
}

/* sprintf()-like replacement.  Returns a Lisp string.  Data
   from Lisp strings is OK because we explicitly inhibit GC. */

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

/* vsprintf()-like replacement.  Returns a malloc()ed memory block.  Data
   from Lisp strings is OK because we explicitly inhibit GC.  Return
   length out through LEN_OUT, if not null. */

Ibyte *
emacs_vsprintf_malloc (const CIbyte *format, va_list vargs,
		       Bytecount *len_out)
{
  int count = begin_gc_forbidden ();
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Ibyte *retval;
  Bytecount len;

  emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
		   vargs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  retval = xnew_ibytes (len + 1);
  memcpy (retval, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  retval[len] = '\0';
  end_gc_forbidden (count);
  Lstream_delete (XLSTREAM (stream));

  if (len_out)
    *len_out = len;
  return retval;
}

/* sprintf()-like replacement.  Returns a malloc()ed memory block.  Data
   from Lisp strings is OK because we explicitly inhibit GC.  Return length
   out through LEN_OUT, if not null. */

Ibyte *
emacs_sprintf_malloc (Bytecount *len_out, const CIbyte *format, ...)
{
  va_list vargs;
  Ibyte *retval;

  va_start (vargs, format);
  retval = emacs_vsprintf_malloc (format, vargs, len_out);
  va_end (vargs);
  return retval;
}

/* vsprintf() replacement.  Writes output into OUTPUT, which better
   have enough space for the output.  Data from Lisp strings is OK
   because we explicitly inhibit GC.  */

Bytecount
emacs_vsprintf (Ibyte *output, const CIbyte *format, va_list vargs)
{
  Bytecount retval;
  int count = begin_gc_forbidden ();
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Bytecount len;

  retval = emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
			    vargs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  memcpy (output, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  output[len] = '\0';
  end_gc_forbidden (count);
  Lstream_delete (XLSTREAM (stream));
  
  return retval;
}

/* sprintf() replacement.  Writes output into OUTPUT, which better
   have enough space for the output.  Data from Lisp strings is OK
   because we explicitly inhibit GC.  */

Bytecount
emacs_sprintf (Ibyte *output, const CIbyte *format, ...)
{
  va_list vargs;
  Bytecount retval;

  va_start (vargs, format);
  retval = emacs_vsprintf (output, format, vargs);
  va_end (vargs);
  return retval;
}


DEFUN ("format", Fformat, 1, MANY, 0, /*
Format a string out of a control-string and arguments.
The first argument is a control string.
The other arguments are substituted into it to make the result, a string.
It may contain %-sequences meaning to substitute the next argument.
%s means print all objects as-is, using `princ'.
%S means print all objects as s-expressions, using `prin1'.
%d or %i means print as an integer in decimal (%o octal, %x lowercase hex,
  %X uppercase hex, %b binary).
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
Zero or more of the flag characters `-', `+', ` ', `0', and `#' may be
  specified between the optional repositioning spec and the conversion
  character; see below.
An optional minimum field width may be specified after any flag characters
  and before the conversion character; it specifies the minimum number of
  characters that the converted argument will take up.  Padding will be
  added on the left (or on the right, if the `-' flag is specified), as
  necessary.  Padding is done with spaces, or with zeroes if the `0' flag
  is specified.
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

The ` ' and `+' flags mean prefix non-negative numbers with a space or
  plus sign, respectively.
The `#' flag means print numbers in an alternate, more verbose format:
  octal numbers begin with zero; hex numbers begin with a 0x or 0X;
  a decimal point is printed in %f, %e, and %E conversions even if no
  numbers are printed after it; and trailing zeroes are not omitted in
   %g and %G conversions.

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
  /* It should not be necessary to GCPRO ARGS, because the caller in the
     interpreter should take care of that.  */
  CHECK_STRING (args[0]);
  return emacs_vsprintf_string_lisp (0, args[0], nargs - 1, args + 1);
}

DEFUN ("format-into", Fformat_into, 2, MANY, 0, /*
Like `format', but write the constructed string into STREAM.

STREAM is a Lisp stream as created by, e.g. `make-string-output-stream'.

Return STREAM.  See the documentation for `format' for details of
CONTROL-STRING and the other arguments.

arguments: (STREAM CONTROL-STRING &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object stream = args[0], control_string = args[1];

  CHECK_LSTREAM (stream);
  CHECK_STRING (control_string);
  
  emacs_doprnt (stream, NULL, XSTRING_LENGTH (control_string),
                control_string, nargs - 2, args + 2);

  return stream;
}

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
