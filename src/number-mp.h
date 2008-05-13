/* Definitions of numeric types for XEmacs using the MP library.
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

/* This library defines the following types:
   bignum       = MINT

   The MP library does not include support for ratios or bigfloats.
*/

#ifndef INCLUDED_number_mp_h_
#define INCLUDED_number_mp_h_

/* BSD MP libraries without MP_PREFIX define a function named pow in mp.h that
   has a different prototype from the one in math.h.  We don't use that
   function anyway, so we do this for safety purposes.  However, this means
   that number-mp.h must always be included before math.h. */
#define pow mp_pow
#include <mp.h>
#undef pow

#ifdef MP_PREFIX
#define MP_GCD   mp_gcd
#define MP_ITOM  mp_itom
#define MP_MADD  mp_madd
#define MP_MCMP  mp_mcmp
#define MP_MDIV  mp_mdiv
#define MP_MFREE mp_mfree
#define MP_MSUB  mp_msub
#define MP_MULT  mp_mult
#define MP_SDIV  mp_sdiv
#ifdef HAVE_MP_MOVE
#define MP_MOVE(x,y) mp_move (x, y)
#else
#define MP_MOVE(x,y) mp_madd (x, bignum_zero, y)
#endif
#else
#define MP_GCD   gcd
#define MP_ITOM  itom
#define MP_MADD  madd
#define MP_MCMP  mcmp
#define MP_MDIV  mdiv
#define MP_MFREE mfree
#define MP_MSUB  msub
#define MP_MULT  mult
#define MP_SDIV  sdiv
#ifdef HAVE_MP_MOVE
#define MP_MOVE(x,y) move (x, y)
#else
#define MP_MOVE(x,y) madd (x, bignum_zero, y)
#endif
#endif

typedef MINT *bignum;

extern void init_number_mp(void);


/********************************* Bignums **********************************/

#define HAVE_BIGNUM 1

extern MINT *bignum_zero, *intern_bignum;
extern MINT *bignum_min_int, *bignum_max_int, *bignum_max_uint;
extern MINT *bignum_min_long, *bignum_max_long, *bignum_max_ulong;
extern short div_rem;

/***** Bignum: basic functions *****/
#define bignum_init(b)              (b = MP_ITOM (0))
#define bignum_fini(b)              MP_MFREE (b)
#define bignum_hashcode(b)          bignum_to_uint (b)
#define bignum_sign(b)              MP_MCMP (b, bignum_zero)
#define bignum_evenp(b)             (MP_SDIV (b, 2, intern_bignum, &div_rem), \
				     div_rem == 0)
#define bignum_oddp(b)              (MP_SDIV (b, 2, intern_bignum, &div_rem), \
				     div_rem != 0)

/***** Bignum: size *****/
#define bignum_fits_int_p(b)        (MP_MCMP (b, bignum_min_int) >= 0 && \
				     MP_MCMP (b, bignum_max_int) <= 0)
#define bignum_fits_uint_p(b)       (MP_MCMP (b, bignum_zero) >= 0 &&	\
				     MP_MCMP (b, bignum_max_uint) <= 0)
#define bignum_fits_long_p(b)       (MP_MCMP (b, bignum_min_long) >= 0 && \
				     MP_MCMP (b, bignum_max_long) <= 0)
#define bignum_fits_ulong_p(b)      (MP_MCMP (b, bignum_zero) >= 0 &&	\
				     MP_MCMP (b, bignum_max_ulong) <= 0)

/***** Bignum: conversions *****/
extern char *bignum_to_string(bignum, int);
extern int bignum_to_int(bignum);
extern unsigned int bignum_to_uint(bignum);
extern long bignum_to_long(bignum);
extern unsigned long bignum_to_ulong(bignum);
extern double bignum_to_double(bignum);

/***** Bignum: converting assignments *****/
#define bignum_set(b1, b2)          MP_MOVE (b2, b1)
extern int bignum_set_string(bignum, const char *, int);
extern void bignum_set_long(bignum, long);
extern void bignum_set_ulong(bignum, unsigned long);
extern void bignum_set_double(bignum, double);

/***** Bignum: comparisons *****/
#define bignum_cmp(b1,b2)           MP_MCMP (b1, b2)
#define bignum_lt(b1,b2)            (MP_MCMP (b1, b2) < 0)
#define bignum_le(b1,b2)            (MP_MCMP (b1, b2) <= 0)
#define bignum_eql(b1,b2)           (MP_MCMP (b1, b2) == 0)
#define bignum_ge(b1,b2)            (MP_MCMP (b1, b2) >= 0)
#define bignum_gt(b1,b2)            (MP_MCMP (b1, b2) > 0)

/***** Bignum: arithmetic *****/
#define bignum_neg(b,b2)            MP_MSUB (bignum_zero, b2, b)
#define bignum_abs(b,b2)            (MP_MCMP (b2, bignum_zero) < 0	\
				     ? MP_MSUB (bignum_zero, b2, b)	\
				     : MP_MADD (bignum_zero, b2, b))
#define bignum_add(b,b1,b2)         MP_MADD (b1, b2, b)
#define bignum_sub(b,b1,b2)         MP_MSUB (b1, b2, b)
#define bignum_mul(b,b1,b2)         MP_MULT (b1, b2, b)
extern int bignum_divisible_p(bignum, bignum);
#define bignum_div(b,b1,b2)         MP_MDIV (b1, b2, b, intern_bignum)
extern void bignum_ceil(bignum, bignum, bignum);
extern void bignum_floor(bignum, bignum, bignum);
#define bignum_mod(b,b1,b2)         MP_MDIV (b1, b2, intern_bignum, b)
extern void bignum_pow(bignum, bignum, unsigned long);
#define bignum_gcd(res,b1,b2)       MP_GCD (b1, b2, res)
extern void bignum_lcm(bignum, bignum, bignum);

/***** Bignum: bit manipulations *****/
extern void bignum_and(bignum, bignum, bignum);
extern void bignum_ior(bignum, bignum, bignum);
extern void bignum_xor(bignum, bignum, bignum);
extern void bignum_not(bignum, bignum);
extern void bignum_setbit(bignum, unsigned long);
extern void bignum_clrbit(bignum, unsigned long);
extern int bignum_testbit(bignum, unsigned long);
extern void bignum_lshift(bignum, bignum, unsigned long);
extern void bignum_rshift(bignum, bignum, unsigned long);

/***** Bignum: random numbers *****/
extern void bignum_random_seed(unsigned long);
extern void bignum_random(bignum, bignum);

#endif /* INCLUDED_number_mp_h_ */
