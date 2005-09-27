/* Definitions of numeric types for XEmacs using the GNU MP library.
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
   bignum       = mpz_t
   ratio        = mpq_t
   bigfloat     = mpf_t
*/

#ifndef INCLUDED_number_gmp_h_
#define INCLUDED_number_gmp_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#ifdef _MSC_VER
/* "unary minus operator applied to unsigned type, result still unsigned":
   Occurs on line 1596 of gmp.h in version 4.1.4. */
#pragma warning ( disable : 4146 )
#endif
#include <gmp.h>
#ifdef _MSC_VER
#pragma warning ( default : 4146 )
#endif

typedef mpz_t bignum;
typedef mpq_t ratio;
typedef mpf_t bigfloat;

extern void init_number_gmp(void);


/********************************* Bignums **********************************/

#define HAVE_BIGNUM 1

extern gmp_randstate_t random_state;

/***** Bignum: basic functions *****/
#define bignum_init(b)                  mpz_init (b)
#define bignum_fini(b)                  mpz_clear (b)
#define bignum_hashcode(b)              mpz_get_ui (b)
#define bignum_sign(b)                  mpz_sgn (b)
#define bignum_evenp(b)                 mpz_even_p (b)
#define bignum_oddp(b)                  mpz_odd_p (b)

/***** Bignum: size *****/
#define bignum_fits_int_p(b)            mpz_fits_sint_p (b)
#define bignum_fits_uint_p(b)           mpz_fits_uint_p (b)
#define bignum_fits_long_p(b)           mpz_fits_slong_p (b)
#define bignum_fits_ulong_p(b)          mpz_fits_ulong_p (b)

/***** Bignum: conversions *****/
#define bignum_to_string(b,base)        mpz_get_str (NULL, base, b)
#define bignum_to_int(b)                ((int) mpz_get_si (b))
#define bignum_to_uint(b)               ((unsigned int) mpz_get_ui (b))
#define bignum_to_long(b)               mpz_get_si (b)
#define bignum_to_ulong(b)              mpz_get_ui (b)
#define bignum_to_double(b)             mpz_get_d (b)

/***** Bignum: converting assignments *****/
#define bignum_set(b1,b2)               mpz_set (b1, b2)
#define bignum_set_string(b,s,base)     mpz_set_str (b, s, base)
#define bignum_set_long(b,l)            mpz_set_si (b, l)
#define bignum_set_ulong(b,l)           mpz_set_ui (b, l)
#define bignum_set_double(b,f)          mpz_set_d (b, f)
#define bignum_set_ratio(b,r)           mpz_set_q (b, r)
#define bignum_set_bigfloat(b,f)        mpz_set_f (b, f)

/***** Bignum: comparisons *****/
#define bignum_cmp(b1,b2)               mpz_cmp (b1, b2)
#define bignum_lt(b1,b2)                (mpz_cmp (b1, b2) < 0)
#define bignum_le(b1,b2)                (mpz_cmp (b1, b2) <= 0)
#define bignum_eql(b1,b2)               (mpz_cmp (b1, b2) == 0)
#define bignum_ge(b1,b2)                (mpz_cmp (b1, b2) >= 0)
#define bignum_gt(b1,b2)                (mpz_cmp (b1, b2) > 0)

/***** Bignum: arithmetic *****/
#define bignum_neg(b,b2)                mpz_neg (b, b2)
#define bignum_abs(b,b2)                mpz_abs (b, b2)
#define bignum_add(b,b1,b2)             mpz_add (b, b1, b2)
#define bignum_sub(b,b1,b2)             mpz_sub (b, b1, b2)
#define bignum_mul(b,b1,b2)             mpz_mul (b, b1, b2)
#define bignum_divisible_p(b1,b2)       mpz_divisible_p (b1, b2)
#define bignum_div(b,b1,b2)             mpz_tdiv_q (b, b1, b2)
#define bignum_ceil(b,b1,b2)            mpz_cdiv_q (b, b1, b2)
#define bignum_floor(b,b1,b2)           mpz_fdiv_q (b, b1, b2)
#define bignum_mod(b,b1,b2)             mpz_mod (b, b1, b2)
#define bignum_pow(res,b,pow)           mpz_pow_ui (res, b, pow)
#define bignum_gcd(res,b1,b2)           mpz_gcd (res, b1, b2)
#define bignum_lcm(res,b1,b2)           mpz_lcm (res, b1, b2)

/***** Bignum: bit manipulations *****/
#define bignum_and(res,b1,b2)           mpz_and (res, b1, b2)
#define bignum_ior(res,b1,b2)           mpz_ior (res, b1, b2)
#define bignum_xor(res,b1,b2)           mpz_xor (res, b1, b2)
#define bignum_not(res,b)               mpz_com (res, b)
#define bignum_setbit(b,bit)            mpz_setbit (b, bit)
#define bignum_clrbit(b,bit)            mpz_clrbit (b, bit)
#define bignum_testbit(b,bit)           mpz_tstbit (b, bit)
#define bignum_lshift(res,b,bits)       mpz_mul_2exp (res, b, bits)
#define bignum_rshift(res,b,bits)       mpz_fdiv_q_2exp (res, b, bits)

/***** Bignum: random numbers *****/
#define bignum_random_seed(seed)        gmp_randseed_ui (random_state, seed)
#define bignum_random(res,limit)        mpz_urandomm (res, random_state, limit)


/********************************** Ratios **********************************/

#define HAVE_RATIO 1

/***** Ratio: basic functions *****/
#define ratio_init(r)                   mpq_init (r)
#define ratio_fini(r)                   mpq_clear (r)
#define ratio_hashcode(r) \
  (mpz_get_ui (mpq_denref (r)) * mpz_get_ui (mpq_numref (r)))
#define ratio_sign(r)                   mpq_sgn (r)
#define ratio_numerator(r)              mpq_numref (r)
#define ratio_denominator(r)            mpq_denref (r)
#define ratio_canonicalize(r)           mpq_canonicalize (r)

/***** Ratio: conversions *****/
#define ratio_to_string(r,base)         mpq_get_str (NULL, base, r)
#define ratio_to_int(r)                 ((int) (mpq_get_d (r)))
#define ratio_to_uint(r)                ((unsigned int) (mpq_get_d (r)))
#define ratio_to_long(r)                ((long) (mpq_get_d (r)))
#define ratio_to_ulong(r)               ((unsigned long) (mpq_get_d (r)))
#define ratio_to_double(r)              mpq_get_d (r)

/***** Ratio: converting assignments *****/
#define ratio_set(r1,r2)                mpq_set (r1, r2)
#define ratio_set_string(r,s,base)      mpq_set_str (r, s, base)
#define ratio_set_long(r,l)             mpq_set_si (r, l, 1UL)
#define ratio_set_ulong(r,l)            mpq_set_ui (r, l, 1UL)
#define ratio_set_double(r,f)           mpq_set_d (r, f)
#define ratio_set_bignum(r,b)           mpq_set_z (r, b)
#define ratio_set_bigfloat(r,f)         mpq_set_f (r, f)
#define ratio_set_long_ulong(r,num,den)    mpq_set_si (r, num, den)
#define ratio_set_ulong_ulong(r,num,den)   mpq_set_ui (r, num, den)
/* FIXME: Why does this canonicalize, but the previous 2 don't? */
#define ratio_set_bignum_bignum(r,num,den) do {	\
    mpz_set (mpq_numref (r), num);		\
    mpz_set (mpq_denref (r), den);		\
    mpq_canonicalize (r);			\
  } while (0)

/***** Ratio: comparisons *****/
#define ratio_cmp(r1,r2)                mpq_cmp (r1, r2)
#define ratio_lt(r1,r2)                 (mpq_cmp (r1, r2) < 0)
#define ratio_le(r1,r2)                 (mpq_cmp (r1, r2) <= 0)
#define ratio_eql(r1,r2)                mpq_equal (r1, r2)
#define ratio_ge(r1,r2)                 (mpq_cmp (r1, r2) >= 0)
#define ratio_gt(r1,r2)                 (mpq_cmp (r1, r2) > 0)

/***** Ratio: arithmetic *****/
#define ratio_neg(q,q2)                 mpq_neg (q, q2)
#define ratio_abs(q,q2)                 mpq_abs (q, q2)
#define ratio_inv(q,q2)                 mpq_inv (q, q2)
#define ratio_add(res,q1,q2)            mpq_add (res, q1, q2)
#define ratio_sub(res,q1,q2)            mpq_sub (res, q1, q2)
#define ratio_mul(res,q1,q2)            mpq_mul (res, q1, q2)
#define ratio_div(res,q1,q2)            mpq_div (res, q1, q2)


/******************************** Bigfloats *********************************/

#define HAVE_BIGFLOAT 1

/***** Bigfloat: basic functions *****/
#define bigfloat_init(f)                mpf_init (f)
#define bigfloat_init_prec(f,prec)      mpf_init2 (f, prec)
#define bigfloat_fini(f)                mpf_clear (f)
#define bigfloat_hashcode(f)            mpf_get_ui (f)
#define bigfloat_sign(f)                mpf_sgn (f)
#define bigfloat_get_prec(f)            mpf_get_prec (f)
#define bigfloat_set_prec(f, prec)      mpf_set_prec (f, prec)
#define bigfloat_set_default_prec(prec) mpf_set_default_prec(prec)
#define bigfloat_get_default_prec()     mpf_get_default_prec ()

/***** Bigfloat: conversions *****/
extern CIbyte *bigfloat_to_string (bigfloat f, int base);
#define bigfloat_to_int(f)              ((int) mpf_get_si (f))
#define bigfloat_to_uint(f)             ((unsigned int) mpf_get_ui (f))
#define bigfloat_to_long(f)             mpf_get_si (f)
#define bigfloat_to_ulong(f)            mpf_get_ui (f)
#define bigfloat_to_double(f)           mpf_get_d (f)

/***** Bigfloat: converting assignments *****/
#define bigfloat_set(f1,f2)             mpf_set (f1, f2)
#define bigfloat_set_string(f,str,base) mpf_set_str (f, str, base)
#define bigfloat_set_long(f,l)          mpf_set_si (f, l)
#define bigfloat_set_ulong(f,l)         mpf_set_ui (f, l)
#define bigfloat_set_double(d,f)        mpf_set_d (d, f)
#define bigfloat_set_bignum(f,b)        mpf_set_z (f, b)
#define bigfloat_set_ratio(f,r)         mpf_set_q (f, r)

/***** Bigfloat: comparisons *****/
#define bigfloat_cmp(f1,f2)             mpf_cmp (f1, f2)
#define bigfloat_lt(f1,f2)              (mpf_cmp (f1, f2) < 0)
#define bigfloat_le(f1,f2)              (mpf_cmp (f1, f2) <= 0)
#define bigfloat_eql(f1,f2)             (mpf_cmp (f1, f2) == 0)
#define bigfloat_eql_bits(f1,f2,bits)   mpf_eq (f1, f2, bits)
#define bigfloat_ge(f1,f2)              (mpf_cmp (f1, f2) >= 0)
#define bigfloat_gt(f1,f2)              (mpf_cmp (f1, f2) > 0)

/***** Bigfloat: arithmetic *****/
#define bigfloat_neg(f,f2)              mpf_neg (f, f2)
#define bigfloat_abs(f,f2)              mpf_abs (f, f2)
#define bigfloat_add(res,f1,f2)         mpf_add (res, f1, f2)
#define bigfloat_sub(res,f1,f2)         mpf_sub (res, f1, f2)
#define bigfloat_mul(res,f1,f2)         mpf_mul (res, f1, f2)
#define bigfloat_div(res,f1,f2)         mpf_div (res, f1, f2)
#define bigfloat_ceil(res,f)            mpf_ceil (res, f)
#define bigfloat_floor(res,f)           mpf_floor (res, f)
#define bigfloat_trunc(res,f)           mpf_trunc (res, f)
#define bigfloat_sqrt(res,f)            mpf_sqrt (res, f)
#define bigfloat_pow(res,f,exp)         mpf_pow_ui (res, f, exp)

#endif /* INCLUDED_number_gmp_h_ */
