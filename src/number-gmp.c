/* Numeric types for XEmacs using the GMP or MPIR library.
   Copyright (C) 2004,2013 Jerry James.

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
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

static mp_exp_t float_print_min, float_print_max;
gmp_randstate_t random_state;

#define GMP_GET_AND_SHIFT_LIMB(bn, n)                           \
  (((unsigned long long) (mpz_getlimbn (bn, n) & GMP_NUMB_MASK) \
    << (GMP_LIMB_BITS * n)))

/* Return the lower-order ((SIZEOF_LONG_LONG * BITS_PER CHAR) - 1) bits of
   bignum B as a signed long long, with the sign reflecting B's sign rather
   than the corresponding bit within the bignum's value. */
long long
bignum_to_llong (const bignum b)
{
  unsigned long long sign_mask
    = bignum_sign (b) < 0 ? ((signed long long) -1) : LLONG_MAX;
  return
    (
#if (GMP_NUMB_BITS * 9) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
#error "Unimplemented" /* But easy to implement! */
#endif
#if (GMP_NUMB_BITS * 8) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 7) |
#endif
#if (GMP_NUMB_BITS * 7) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 6) |    
#endif
#if (GMP_NUMB_BITS * 6) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 5) |    
#endif
#if (GMP_NUMB_BITS * 5) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 4) |    
#endif
#if (GMP_NUMB_BITS * 4) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 3) |    
#endif
#if (GMP_NUMB_BITS * 3) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 2) |
#endif
#if (GMP_NUMB_BITS * 2) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
     GMP_GET_AND_SHIFT_LIMB (b, 1) |
#endif
     GMP_GET_AND_SHIFT_LIMB (b, 0)) & sign_mask;
}

/* Return the lower-order (SIZEOF_LONG_LONG * BITS_PER CHAR) bits of bignum B
   as an unsigned long long. */
unsigned long long
bignum_to_ullong (const bignum b)
{
  return 
#if (GMP_NUMB_BITS * 9) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
#error "Unimplemented"
#endif
#if (GMP_NUMB_BITS * 8) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 7) |
#endif
#if (GMP_NUMB_BITS * 7) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 6) |    
#endif
#if (GMP_NUMB_BITS * 6) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 5) |    
#endif
#if (GMP_NUMB_BITS * 5) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 4) |    
#endif
#if (GMP_NUMB_BITS * 4) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 3) |    
#endif
#if (GMP_NUMB_BITS * 3) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 2) |
#endif
#if (GMP_NUMB_BITS * 2) <= (SIZEOF_LONG_LONG * BITS_PER_CHAR)
    GMP_GET_AND_SHIFT_LIMB (b, 1) |
#endif
    GMP_GET_AND_SHIFT_LIMB (b, 0);
}

void
bignum_set_llong (bignum b, long long l)
{
  if (l < 0LL)
    {
      /* This even works for LLONG_MIN.  Try it! */
      l = -l;
      mpz_import (b, 1U, 1, sizeof (l), 0, 0U, &l);
      mpz_neg (b, b);
    }
  else
    {
      mpz_import (b, 1U, 1, sizeof (l), 0, 0U, &l);
    }
}

CIbyte *
bigfloat_to_string (mpf_t f, int base)
{
  mp_exp_t expt;
  CIbyte *str = mpf_get_str (NULL, &expt, base, 0, f);
  const int sign = mpf_sgn (f);
  const int neg = (sign < 0) ? 1 : 0;
  int len = strlen (str) + 1;  /* Count the null terminator */

  if (sign == 0)
    {
      XREALLOC_ARRAY (str, CIbyte, 4);
      strncpy (str, "0.0", 4);
    }
  else if (float_print_min <= expt && expt <= float_print_max)
    {
      if (expt < 0)
	{
	  /* We need room for a radix point and leading zeroes */
	  const int space = -expt + 2;
	  XREALLOC_ARRAY (str, CIbyte, len + space);
	  memmove (&str[space + neg], &str[neg], len - neg);
	  memset (&str[neg], '0', space);
	  str[neg + 1] = '.';
	}
      else if (len <= expt + neg + 1)
	{
	  /* We need room for a radix point and trailing zeroes */
	  XREALLOC_ARRAY (str, CIbyte, expt + neg + 3);
	  memset (&str[len - 1], '0', expt + neg + 3 - len);
	  str[expt + neg] = '.';
	  str[expt + neg + 2] = '\0';
	}
      else
	{
	  /* We just need room for a radix point */
	  XREALLOC_ARRAY (str, CIbyte, len + 1);
	  memmove (&str[expt + neg + 1], &str[expt + neg], len - (expt + neg));
	  str[expt + neg] = '.';
	}
    }
  else
    {
      /* Computerized scientific notation: We need room for a possible radix
	 point, format identifier, and exponent */
      /* GMP's idea of the exponent is 1 greater than scientific notation's */
      expt--;
      {
	const int point = (len == neg + 2) ? 0 : 1;
	const int exponent = (expt < 0)
	  ? (int)(log ((double) (-expt)) / log ((double) base)) + 3
	  : (int)(log ((double) expt) / log ((double) base)) + 2;
	const int space = point + exponent;
	XREALLOC_ARRAY (str, CIbyte, len + space);
	if (point > 0)
	  {
	    memmove (&str[neg + 2], &str[neg + 1], len - neg);
	    str[neg + 1] = '.';
	  }
	sprintf (&str[len + point - 1], "E%ld", expt);
      }
    }
  return str;
}

/* We need the next two functions since GNU MP insists on giving us an extra
   parameter. */
static void *
gmp_realloc (void *ptr, size_t UNUSED (old_size), size_t new_size)
{
  return xrealloc (ptr, new_size);
}

static void
gmp_free (void *ptr, size_t UNUSED (size))
{
  xfree (ptr);
}

void
init_number_gmp (void)
{
  mp_set_memory_functions ((void *(*) (size_t)) xmalloc, gmp_realloc,
			   gmp_free);

  /* Numbers with smaller exponents than this are printed in scientific
     notation. */
  float_print_min = -4;

  /* Numbers with larger exponents than this are printed in scientific
     notation. */
  float_print_max = 8;

  /* Prepare the bignum/bigfloat random number generator */
  gmp_randinit_default (random_state);
  gmp_randseed_ui (random_state, qxe_getpid () + time (NULL));
}
