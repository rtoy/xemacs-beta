/* Numeric types for XEmacs using the MP library.
   Copyright (C) 2004 Jerry James.

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

/* Synched up with: Not in FSF. */

#include <config.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include "lisp.h"

static MINT *bignum_bytesize, *bignum_one, *bignum_two;
MINT *bignum_zero, *intern_bignum;
MINT *bignum_min_int, *bignum_max_int, *bignum_max_uint;
MINT *bignum_min_long, *bignum_max_long, *bignum_max_ulong;
MINT *bignum_min_llong, *bignum_max_llong, *bignum_max_ullong;
short div_rem;

char *
bignum_to_string (bignum b, int base)
{
  REGISTER unsigned int i;
  unsigned int bufsize = 128U, index = 0U;
  int sign;
  char *buffer = xnew_array (char, 128), *retval;
  MINT *quo = MP_ITOM (0);
  short rem;

  /* FIXME: signal something if base is < 2 or doesn't fit into a short. */

  /* Save the sign for later */
  sign = bignum_sign (b);

  if (sign == 0)
    {
      XREALLOC_ARRAY (buffer, char, 2);
      buffer[0] = '0';
      buffer[1] = '\0';
      return buffer;
    }
  /* Copy abs(b) into quo for destructive modification */
  else if (sign < 0)
    MP_MSUB (bignum_zero, b, quo);
  else
    MP_MOVE (b, quo);

  /* Loop over the digits of b (in BASE) and place each one into buffer */
  for (i = 0U; MP_MCMP(quo, bignum_zero) > 0; i++)
    {
      MP_SDIV (quo, base, quo, &rem);
      if (index == bufsize)
	{
	  bufsize <<= 1;
	  XREALLOC_ARRAY (buffer, char, bufsize);
	}
      buffer[index++] = rem < 10 ? rem + '0' : rem - 10 + 'a';
    }
  MP_MFREE (quo);

  /* Reverse the digits, maybe add a minus sign, and add a null terminator */
  bufsize = index + (sign < 0 ? 1 : 0) + 1;
  retval = xnew_array (char, bufsize);
  if (sign < 0)
    {
      retval[0] = '-';
      i = 1;
    }
  else
    i = 0;
  for (; i < bufsize - 1; i++)
    retval[i] = buffer[--index];
  retval[bufsize - 1] = '\0';
  xfree (buffer);
  return retval;
}

#define BIGNUM_TO_TYPE(type,accumtype) do {	\
    if (0 == sign)				\
      {						\
	return (type)0;				\
      }						\
						\
    bignum_init (quo);				\
						\
    if (sign < 0)				\
      {						\
	MP_MSUB (bignum_zero, b, quo);		\
      }						\
    else					\
      {						\
	MP_MOVE (b, quo);			\
      }						\
						\
  for (i = 0U; i < sizeof(type); i++)		\
    {						\
      MP_SDIV (quo, 256, quo, &rem);		\
      retval |= ((accumtype) rem) << (8 * i);	\
    }						\
  bignum_fini (quo);				\
} while (0)

int
bignum_to_int (bignum b)
{
  short rem, sign;
  unsigned int retval = 0;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (int, unsigned int);
  return ((int) retval) * sign;
}

unsigned int
bignum_to_uint (bignum b)
{
  short rem, sign;
  unsigned int retval = 0U;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (unsigned int, unsigned int);
  return retval;
}

long
bignum_to_long (bignum b)
{
  short rem, sign;
  unsigned long retval = 0L;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (long, unsigned long);
  return ((long) retval) * sign;
}

unsigned long
bignum_to_ulong (bignum b)
{
  short rem, sign;
  unsigned long retval = 0UL;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (unsigned long, unsigned long);
  return retval;
}

long long
bignum_to_llong (bignum b)
{
  short rem, sign;
  unsigned long long retval = 0LL;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (long long, unsigned long long);
  return ((long long) retval) * sign;
}

unsigned long long
bignum_to_ullong (bignum b)
{
  short rem, sign;
  unsigned long long retval = 0UL;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  BIGNUM_TO_TYPE (unsigned long long, unsigned long long);
  return retval;
}

double
bignum_to_double (bignum b)
{
  short rem, sign;
  double retval = 0.0, factor = 1.0;
  REGISTER unsigned int i;
  MINT *quo;

  sign = bignum_sign (b);
  bignum_init (quo);
  if (sign < 0)
    {
      MP_MSUB (bignum_zero, b, quo);
    }
  else
    {
      MP_MOVE (b, quo);
    }

  for (i = 0U; MP_MCMP (quo, bignum_zero) > 0; i++)
    {
      MP_SDIV (quo, 256, quo, &rem);
      retval += rem * factor;
      factor *= 256.0;
    }
  MP_MFREE (quo);
  return retval * sign;
}

static short
char_to_number (char c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  if (c >= 'a' && c <= 'z')
    return c - 'a' + 10;
  if (c >= 'A' && c <= 'Z')
    return c - 'A' + 10;
  return -1;
}

int
bignum_set_string (bignum b, const char *s, int base)
{
  MINT *mbase;
  short digit;
  int neg = 0;

  if (base == 0)
    {
      if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
	{
	  base = 16;
	  s += 2;
	}
      else if (*s == '0')
	{
	  base = 8;
	  s++;
	}
      else
	base = 10;
    }

  /* FIXME: signal something if base is < 2 or doesn't fit into a short. */

  if (*s == '-')
    {
      s++;
      neg = 1;
    }

  mbase = MP_ITOM ((short) base);
  MP_MOVE (bignum_zero, b);

  for (digit = char_to_number (*s); digit >= 0 && digit < base;
       digit = char_to_number (*++s))
    {
      MINT *temp;

      MP_MULT (b, mbase, b);
      temp = MP_ITOM (digit);
      MP_MADD (b, temp, b);
      MP_MFREE (temp);
    }
  MP_MFREE (mbase);

  if (neg)
    MP_MSUB (bignum_zero, b, b);

  return (digit >= 0) ? -1 : 0;
}

void
bignum_set_long (bignum b, long l)
{
  char hex[SIZEOF_LONG * 2U + 2U];
  MINT *temp;
  int neg = l < 0L;

  snprintf (hex, SIZEOF_LONG * 2U + 2U, "%lx",
	    neg ? (unsigned long) -l : (unsigned long) l);
  temp = MP_XTOM (hex);
  if (neg)
    MP_MSUB (bignum_zero, temp, b);
  else
    MP_MOVE (temp, b);
  MP_MFREE (temp);
}

void
bignum_set_ulong (bignum b, unsigned long l)
{
  char hex[SIZEOF_LONG * 2U + 2U];
  MINT *temp;

  snprintf (hex, SIZEOF_LONG * 2U + 2U, "%lx", l);
  temp = MP_XTOM (hex);
  MP_MOVE (temp, b);
  MP_MFREE (temp);
}

void
bignum_set_llong (bignum b, long long l)
{
  char hex[SIZEOF_LONG_LONG * 2U + 2U];
  MINT *temp;
  int neg = l < 0LL;

  snprintf (hex, SIZEOF_LONG_LONG * 2U + 2U, "%llx",
	    neg ? (unsigned long long) -l : (unsigned long long) l);
  temp = MP_XTOM (hex);
  if (neg)
    MP_MSUB (bignum_zero, temp, b);
  else
    MP_MOVE (temp, b);
  MP_MFREE (temp);
}

void
bignum_set_ullong (bignum b, unsigned long long l)
{
  char hex[SIZEOF_LONG_LONG * 2U + 2U];
  MINT *temp;

  snprintf (hex, SIZEOF_LONG_LONG * 2U + 2U, "%llx", l);
  temp = MP_XTOM (hex);
  MP_MOVE (temp, b);
  MP_MFREE (temp);
}

void
bignum_set_double (bignum b, double d)
{
  REGISTER unsigned int i;
  int negative = (d < 0) ? 1 : 0;
  MINT *multiplier = MP_ITOM (1);

  MP_MOVE (bignum_zero, b);
  if (negative)
    d = -d;
  for (i = 0UL; d > 0.0; d /= 256, i++)
    {
      MINT *temp = MP_ITOM ((short) fmod (d, 256.0));
      MP_MULT (multiplier, temp, temp);
      MP_MADD (b, temp, b);
      MP_MULT (multiplier, bignum_bytesize, multiplier);
      MP_MFREE (temp);
    }
  MP_MFREE (multiplier);
  if (negative)
    MP_MSUB (bignum_zero, b, b);
}

/* Return nonzero if b1 is exactly divisible by b2 */
int
bignum_divisible_p (bignum b1, bignum b2)
{
  int retval;
  MINT *rem = MP_ITOM (0);
  MP_MDIV (b1, b2, intern_bignum, rem);
  retval = (MP_MCMP (rem, bignum_zero) == 0);
  MP_MFREE (rem);
  return retval;
}

void bignum_ceil (bignum quotient, bignum N, bignum D)
{
  MP_MDIV (N, D, quotient, intern_bignum);

  if (MP_MCMP (intern_bignum, bignum_zero) != 0)
    {
      short signN = MP_MCMP (N, bignum_zero);
      short signD = MP_MCMP (D, bignum_zero);

      /* If the quotient is positive, add one, since we're rounding to
	 positive infinity. */
      if (signD < 0)
	{
	  if (signN <= 0)
	    {
	      MP_MADD (quotient, bignum_one, quotient);
	    }
	}
      else if (signN >= 0)
	{
	  MP_MADD (quotient, bignum_one, quotient);
	}
    }
}

void bignum_floor (bignum quotient, bignum N, bignum D)
{
  MP_MDIV (N, D, quotient, intern_bignum);

  if (MP_MCMP (intern_bignum, bignum_zero) != 0)
    {
      short signN = MP_MCMP (N, bignum_zero);
      short signD = MP_MCMP (D, bignum_zero);

      /* If the quotient is negative, subtract one, we're rounding to minus
	 infinity.  */
      if (signD < 0)
	{
	  if (signN >= 0)
	    {
	      MP_MSUB (quotient, bignum_one, quotient);
	    }
	}
      else if (signN < 0)
	{
	  MP_MSUB (quotient, bignum_one, quotient);
	}
    }
}

/* RESULT = N to the POWth power */
void
bignum_pow (bignum result, bignum n, unsigned long pow)
{
  MP_MOVE (bignum_one, result);
  for ( ; pow > 0UL; pow--)
    MP_MULT (result, n, result);
}

/* lcm(b1,b2) = b1 * b2 / gcd(b1, b2) */
void
bignum_lcm (bignum result, bignum b1, bignum b2)
{
  MP_MULT (b1, b2, result);
  MP_GCD (b1, b2, intern_bignum);
  MP_MDIV (result, intern_bignum, result, intern_bignum);
}

/* FIXME: We can't handle negative args, so right now we just make them
   positive before doing anything else.  How should we really handle negative
   args? */
#define bignum_bit_op(result, b1, b2, op)				\
  REGISTER unsigned int i;						\
  MINT *multiplier = MP_ITOM (1), *n1 = MP_ITOM (0), *n2 = MP_ITOM (0);	\
									\
  if (MP_MCMP (bignum_zero, b1) > 0)					\
    MP_MSUB (bignum_zero, b1, n1);					\
  else									\
    MP_MOVE (b1, n1);							\
  if (MP_MCMP (bignum_zero, b2) > 0)					\
    MP_MSUB (bignum_zero, b2, n2);					\
  else									\
    MP_MOVE (b2, n2);							\
									\
  MP_MOVE (bignum_zero, result);					\
									\
  for (i = 0UL; MP_MCMP (bignum_zero, n1) < 0 &&			\
	 MP_MCMP (bignum_zero, n2) < 0; i++)				\
    {									\
      short byte1, byte2;						\
      MINT *temp;							\
									\
      MP_SDIV (n1, 256, n1, &byte1);					\
      MP_SDIV (n2, 256, n2, &byte2);					\
      temp = MP_ITOM (byte1 op byte2);					\
      MP_MULT (multiplier, temp, temp);					\
      MP_MADD (result, temp, result);					\
      MP_MULT (multiplier, bignum_bytesize, multiplier);		\
      MP_MFREE (temp);							\
    }									\
  MP_MFREE (n2);							\
  MP_MFREE (n1);							\
  MP_MFREE (multiplier)

void
bignum_and (bignum result, bignum b1, bignum b2)
{
  bignum_bit_op (result, b1, b2, &);
}

void
bignum_ior (bignum result, bignum b1, bignum b2)
{
  bignum_bit_op (result, b1, b2, |);
}

void
bignum_xor (bignum result, bignum b1, bignum b2)
{
  bignum_bit_op (result, b1, b2, ^);
}

/* NOT is not well-defined for bignums ... where do you stop flipping bits?
   We just flip until we see the last one.  This is probably a bad idea. */
void
bignum_not (bignum result, bignum b)
{
  REGISTER unsigned int i;
  MINT *multiplier = MP_ITOM (1), *n = MP_ITOM (0);

  if (MP_MCMP (bignum_zero, b) > 0)
    MP_MSUB (bignum_zero, b, n);
  else
    MP_MOVE (b, n);

  MP_MOVE (bignum_zero, result);

  for (i = 0UL; MP_MCMP (bignum_zero, n) < 0; i++)
    {
      short byte;
      MINT *temp;

      MP_SDIV (n, 256, n, &byte);
      temp = MP_ITOM (~byte);
      MP_MULT (multiplier, temp, temp);
      MP_MADD (result, temp, result);
      MP_MULT (multiplier, bignum_bytesize, multiplier);
      MP_MFREE (temp);
    }
  MP_MFREE (n);
  MP_MFREE (multiplier);
}

void
bignum_setbit (bignum b, unsigned long bit)
{
  bignum_pow (intern_bignum, bignum_two, bit);
  bignum_ior (b, b, intern_bignum);
}

/* This is so evil, even I feel queasy. */
void
bignum_clrbit (bignum b, unsigned long bit)
{
  MINT *num = MP_ITOM (0);

  /* See if the bit is set, and subtract it off if so */
  MP_MOVE (b, intern_bignum);
  bignum_pow (num, bignum_two, bit);
  bignum_ior (intern_bignum, intern_bignum, num);
  if (MP_MCMP (b, intern_bignum) == 0)
    MP_MSUB (b, num, b);
  MP_MFREE (num);
}

int
bignum_testbit (bignum b, unsigned long bit)
{
  bignum_pow (intern_bignum, bignum_two, bit);
  bignum_and (intern_bignum, b, intern_bignum);
  return MP_MCMP (intern_bignum, bignum_zero);
}

void
bignum_lshift (bignum result, bignum b, unsigned long bits)
{
  bignum_pow (intern_bignum, bignum_two, bits);
  MP_MULT (b, intern_bignum, result);
}

void
bignum_rshift (bignum result, bignum b, unsigned long bits)
{
  bignum_pow (intern_bignum, bignum_two, bits);
  MP_MDIV (b, intern_bignum, result, intern_bignum);
}

void
bignum_random (bignum result, bignum limit)
{
  MINT *denominator = MP_ITOM (0), *divisor = MP_ITOM (0);
  bignum_set_long (denominator, RAND_MAX);
  MP_MADD (denominator, bignum_one, denominator);
  MP_MADD (limit, bignum_one, divisor);
  MP_MDIV (denominator, divisor, denominator, intern_bignum);
  MP_MFREE (divisor);

  do
    {
      MINT *limitcmp = MP_ITOM (1);

      /* Accumulate at least as many random bits as in LIMIT */
      MP_MOVE (bignum_zero, result);
      do
	{
	  bignum_lshift (limitcmp, limitcmp, FIXNUM_VALBITS);
	  bignum_lshift (result, result, FIXNUM_VALBITS);
	  bignum_set_long (intern_bignum, get_random ());
	  MP_MADD (intern_bignum, result, result);
	}
      while (MP_MCMP (limitcmp, limit) <= 0);
      MP_MDIV (result, denominator, result, intern_bignum);
      MP_MFREE (limitcmp);
    }
  while (MP_MCMP (limit, result) <= 0);

  MP_MFREE (denominator);
}

#ifdef HAVE_MP_SET_MEMORY_FUNCTIONS
/* We need the next two functions due to the extra parameter. */
static void *
mp_realloc (void *ptr, size_t UNUSED (old_size), size_t new_size)
{
  return xrealloc (ptr, new_size);
}

static void
mp_free (void *ptr, size_t UNUSED (size))
{
  xfree (ptr);
}
#endif

void
init_number_mp (void)
{
#ifdef HAVE_MP_SET_MEMORY_FUNCTIONS
  mp_set_memory_functions ((void *(*) (size_t)) xmalloc, mp_realloc, mp_free);
#endif

  bignum_zero = MP_ITOM (0);
  bignum_one = MP_ITOM (1);
  bignum_two = MP_ITOM (2);

  /* intern_bignum holds throwaway values from macro expansions in
     number-mp.h.  Its value is immaterial. */
  intern_bignum = MP_ITOM (0);

  /* The multiplier used to shift a number left by one byte's worth of bits */
  bignum_bytesize = MP_ITOM (256);

  /* The MP interface only supports turning short ints into MINTs, so we have
     to set these the hard way. */

  bignum_min_int = MP_ITOM (0);
  bignum_set_long (bignum_min_int, INT_MIN);

  bignum_max_int = MP_ITOM (0);
  bignum_set_long (bignum_max_int, INT_MAX);

  bignum_max_uint = MP_ITOM (0);
  bignum_set_ulong (bignum_max_uint, UINT_MAX);

  bignum_min_long = MP_ITOM (0);
  bignum_set_long (bignum_min_long, LONG_MIN);

  bignum_max_long = MP_ITOM (0);
  bignum_set_long (bignum_max_long, LONG_MAX);

  bignum_max_ulong = MP_ITOM (0);
  bignum_set_ulong (bignum_max_ulong, ULONG_MAX);

  bignum_min_llong = MP_ITOM (0);
  bignum_set_llong (bignum_min_llong, LLONG_MIN);

  bignum_max_llong = MP_ITOM (0);
  bignum_set_llong (bignum_max_llong, LLONG_MAX);

  bignum_max_ullong = MP_ITOM (0);
  bignum_set_ullong (bignum_max_ullong, ULLONG_MAX);
}
