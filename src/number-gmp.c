/* Numeric types for XEmacs using the GNU MP library.
   Copyright (C) 2004 Jerry James.

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

#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

static mpf_t float_print_min, float_print_max;
gmp_randstate_t random_state;

CIbyte *
bigfloat_to_string(mpf_t f, int base)
{
  mp_exp_t expt;
  CIbyte *str = mpf_get_str (NULL, &expt, base, 0, f);
  const int sign = mpf_sgn (f);
  const int neg = (sign < 0) ? 1 : 0;
  int len = strlen (str) + 1;  /* Count the null terminator */

  if (sign == 0 || (mpf_cmp (float_print_min, f) <= 0 &&
                    mpf_cmp (f, float_print_max) <= 0))
    {
      /* Move digits down to insert a radix point */
      if (expt <= 0)
        {
          /* We need room for a radix point and leading zeroes */
          const int space = -expt + 2;
          XREALLOC_ARRAY (str, CIbyte, len + space);
          memmove (&str[space + neg], &str[neg], len - neg);
          memset (&str[neg], '0', space);
          str[neg + 1] = '.';
          len += space;
        }
      else
        {
          /* We just need room for a radix point */
          XREALLOC_ARRAY (str, CIbyte, len + 1);
          memmove (&str[expt + neg + 1], &str[expt + neg], len - (expt + neg));
          str[expt + neg] = '.';
          len++;
        }
    }
  else
    {
      /* Computerized scientific notation */
      /* We need room for a radix point, format identifier, and exponent */
      const int space = (expt < 0)
        ? (int)(log (-expt) / log (base)) + 3
        : (int)(log (expt) / log (base)) + 2;
      XREALLOC_ARRAY (str, CIbyte, len + space);
      memmove (&str[neg + 2], &str[neg + 1], len - neg);
      str[len + 1] = 'l';
      sprintf (&str[len + 2], "%ld", expt);
    }
  return str;
}

/* We need the next two functions since GNU MP insists on giving us an extra
   parameter. */
static void *gmp_realloc (void *ptr, size_t old_size /* unused */,
                          size_t new_size)
{
  return xrealloc (ptr, new_size);
}

static void gmp_free (void *ptr, size_t size /* unused */)
{
  xfree (ptr, void *);
}

void
init_number_gmp ()
{
  mp_set_memory_functions ((void *(*) (size_t))xmalloc, gmp_realloc, gmp_free);

  /* The smallest number that is printed without exponents */
  mpf_init_set_d (float_print_min, 0.001);

  /* The largest number that is printed without exponents */
  mpf_init_set_ui (float_print_max, 10000000UL);

  /* Prepare the bignum/bigfloat random number generator */
  gmp_randinit_default (random_state);
  gmp_randseed_ui (random_state, qxe_getpid () + time (NULL));
}
