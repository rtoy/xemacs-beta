/* Definitions of numeric types for XEmacs.
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

#ifndef INCLUDED_number_h_
#define INCLUDED_number_h_

/* The following types are always defined in the same manner:
   fixnum       = whatever fits in the Lisp_Object type
   integer      = union (fixnum, bignum)
   rational     = union (integer, ratio)
   float        = C double
   floating     = union(float, bigfloat)  Anybody got a better name?
   real         = union (rational, floating)
   number       = real  (should be union(real, complex) but no complex yet)

   It is up to the library-specific code to define the remaining types,
   namely: bignum, ratio, and bigfloat.  Not all of these types may be
   available.  The top-level configure script should define the symbols
   HAVE_BIGNUM, HAVE_RATIO, and HAVE_BIGFLOAT to indicate which it provides.
   If some type is not defined by the library, this is what happens:

   - bignum: bignump(x) is false for all x; any attempt to create a bignum
     causes an error to be raised.

   - ratio: we define our own structure consisting of two Lisp_Objects, which
     are presumed to be integers (i.e., either fixnums or bignums).  We do our
     own GCD calculation, which is bound to be slow, to keep the ratios
     reduced to canonical form.  (FIXME: Not yet implemented.)

   - bigfloat: bigfloat(x) is false for all x; any attempt to create a
     bigfloat causes an error to be raised.

   We (provide) the following symbols, so that Lisp code has some hope of
   using this correctly:

   - (provide 'bignum) if HAVE_BIGNUM
   - (provde 'ratio) if HAVE_RATIO
   - (provide 'bigfloat) if HAVE_BIGFLOAT
*/

/* Load the library definitions */
#ifdef WITH_GMP
#include "number-gmp.h"
#endif
#ifdef WITH_MP
#include "number-mp.h"
#endif


/********************************* Bignums **********************************/
#ifdef HAVE_BIGNUM

struct Lisp_Bignum
{
  struct lrecord_header lheader;
  bignum data;
};
typedef struct Lisp_Bignum Lisp_Bignum;

DECLARE_LRECORD (bignum, Lisp_Bignum);
#define XBIGNUM(x) XRECORD (x, bignum, Lisp_Bignum)
#define wrap_bignum(p) wrap_record (p, bignum)
#define BIGNUMP(x) RECORDP (x, bignum)
#define CHECK_BIGNUM(x) CHECK_RECORD (x, bignum)
#define CONCHECK_BIGNUM(x) CONCHECK_RECORD (x, bignum)

#define bignum_data(b) (b)->data
#define XBIGNUM_DATA(x) bignum_data (XBIGNUM (x))

#define BIGNUM_ARITH_RETURN(b,op) do				\
{								\
  Lisp_Object retval = make_bignum (0);				\
  bignum_##op (XBIGNUM_DATA (retval), XBIGNUM_DATA (b));	\
  return Fcanonicalize_number (retval);				\
} while (0)

#define BIGNUM_ARITH_RETURN1(b,op,arg) do			\
{								\
  Lisp_Object retval = make_bignum(0);				\
  bignum_##op (XBIGNUM_DATA (retval), XBIGNUM_DATA (b), arg);	\
  return Fcanonicalize_number (retval);				\
} while (0)

extern Lisp_Object make_bignum (long);
extern Lisp_Object make_bignum_bg (bignum);
extern bignum scratch_bignum, scratch_bignum2;

#else /* !HAVE_BIGNUM */

extern Lisp_Object Qbignump;
#define BIGNUMP(x)         0
#define CHECK_BIGNUM(x)    dead_wrong_type_argument (Qbignump, x)
#define CONCHECK_BIGNUM(x) dead_wrong_type_argument (Qbignump, x)
typedef void bignum;
#define make_bignum(l)     This XEmacs does not support bignums
#define make_bignum_bg(b)  This XEmacs does not support bignums

#endif /* HAVE_BIGNUM */

EXFUN (Fbignump, 1);


/********************************* Integers *********************************/
extern Lisp_Object Qintegerp;

#define INTEGERP(x) (INTP(x) || BIGNUMP(x))
#define CHECK_INTEGER(x) do {			\
 if (!INTEGERP (x))				\
   dead_wrong_type_argument (Qintegerp, x);	\
 } while (0)
#define CONCHECK_INTEGER(x) do {		\
 if (!INTEGERP (x))				\
   x = wrong_type_argument (Qintegerp, x);	\
}  while (0)

#ifdef HAVE_BIGNUM
#define make_integer(x) \
  (NUMBER_FITS_IN_AN_EMACS_INT (x) ? make_int (x) : make_bignum (x))
#else
#define make_integer(x) make_int (x)
#endif

extern Fixnum Vmost_negative_fixnum, Vmost_positive_fixnum;
EXFUN (Fintegerp, 1);
EXFUN (Fevenp, 1);
EXFUN (Foddp, 1);


/********************************** Ratios **********************************/
#ifdef HAVE_RATIO

struct Lisp_Ratio
{
  struct lrecord_header lheader;
  ratio data;
};
typedef struct Lisp_Ratio Lisp_Ratio;

DECLARE_LRECORD (ratio, Lisp_Ratio);
#define XRATIO(x) XRECORD (x, ratio, Lisp_Ratio)
#define wrap_ratio(p) wrap_record (p, ratio)
#define RATIOP(x) RECORDP (x, ratio)
#define CHECK_RATIO(x) CHECK_RECORD (x, ratio)
#define CONCHECK_RATIO(x) CONCHECK_RECORD (x, ratio)

#define ratio_data(r) (r)->data

#define XRATIO_DATA(r) ratio_data (XRATIO (r))
#define XRATIO_NUMERATOR(r) ratio_numerator (XRATIO_DATA (r))
#define XRATIO_DENOMINATOR(r) ratio_denominator (XRATIO_DATA (r))

#define RATIO_ARITH_RETURN(r,op) do			\
{							\
  Lisp_Object retval = make_ratio (0L, 1UL);		\
  ratio_##op (XRATIO_DATA (retval), XRATIO_DATA (r));	\
  return Fcanonicalize_number (retval);			\
} while (0)

#define RATIO_ARITH_RETURN1(r,op,arg) do			\
{								\
  Lisp_Object retval = make_ratio (0L, 1UL);			\
  ratio_##op (XRATIO_DATA (retval), XRATIO_DATA (r), arg);	\
  return Fcanonicalize_number (retval);				\
} while (0)

extern Lisp_Object make_ratio (long, unsigned long);
extern Lisp_Object make_ratio_bg (bignum, bignum);
extern Lisp_Object make_ratio_rt (ratio);
extern ratio scratch_ratio;

#else /* !HAVE_RATIO */

extern Lisp_Object Qratiop;
#define RATIOP(x)          0
#define CHECK_RATIO(x)     dead_wrong_type_argument (Qratiop, x)
#define CONCHECK_RATIO(x)  dead_wrong_type_argument (Qratiop, x)
typedef void ratio;
#define make_ratio(n,d)    This XEmacs does not support ratios
#define make_ratio_bg(n,d) This XEmacs does not support ratios

#endif /* HAVE_RATIO */

EXFUN (Fratiop, 1);


/******************************** Rationals *********************************/
extern Lisp_Object Qrationalp;

#define RATIONALP(x) (INTEGERP(x) || RATIOP(x))
#define CHECK_RATIONAL(x) do {			\
 if (!RATIONALP (x))				\
   dead_wrong_type_argument (Qrationalp, x);	\
 } while (0)
#define CONCHECK_RATIONAL(x) do {		\
 if (!RATIONALP (x))				\
   x = wrong_type_argument (Qrationalp, x);	\
}  while (0)

EXFUN (Frationalp, 1);
EXFUN (Fnumerator, 1);
EXFUN (Fdenominator, 1);


/******************************** Bigfloats *********************************/
#ifdef HAVE_BIGFLOAT
struct Lisp_Bigfloat
{
  struct lrecord_header lheader;
  bigfloat bf;
};
typedef struct Lisp_Bigfloat Lisp_Bigfloat;

DECLARE_LRECORD (bigfloat, Lisp_Bigfloat);
#define XBIGFLOAT(x) XRECORD (x, bigfloat, Lisp_Bigfloat)
#define wrap_bigfloat(p) wrap_record (p, bigfloat)
#define BIGFLOATP(x) RECORDP (x, bigfloat)
#define CHECK_BIGFLOAT(x) CHECK_RECORD (x, bigfloat)
#define CONCHECK_BIGFLOAT(x) CONCHECK_RECORD (x, bigfloat)

#define bigfloat_data(f) ((f)->bf)
#define XBIGFLOAT_DATA(x) bigfloat_data (XBIGFLOAT (x))
#define XBIGFLOAT_GET_PREC(x) bigfloat_get_prec (XBIGFLOAT_DATA (x))
#define XBIGFLOAT_SET_PREC(x,p) bigfloat_set_prec (XBIGFLOAT_DATA (x), p)

#define BIGFLOAT_ARITH_RETURN(f,op) do				\
{								\
  Lisp_Object retval = make_bigfloat_bf (f);			\
  bigfloat_##op (XBIGFLOAT_DATA (retval), XBIGFLOAT_DATA (f));	\
  return retval;						\
} while (0)

#define BIGFLOAT_ARITH_RETURN1(f,op,arg) do				\
{									\
  Lisp_Object retval = make_bigfloat_bf (f);				\
  bigfloat_##op (XBIGFLOAT_DATA (retval), XBIGFLOAT_DATA (f), arg);	\
  return retval;							\
} while (0)

extern Lisp_Object make_bigfloat (double, unsigned long);
extern Lisp_Object make_bigfloat_bf (bigfloat);
extern Lisp_Object Vdefault_float_precision;
extern bigfloat scratch_bigfloat, scratch_bigfloat2;

#else /* !HAVE_BIGFLOAT */

extern Lisp_Object Qbigfloatp;
#define BIGFLOATP(x)         0
#define CHECK_BIGFLOAT(x)    dead_wrong_type_argument (Qbigfloatp, x)
#define CONCHECK_BIGFLOAT(x) dead_wrong_type_argument (Qbigfloatp, x)
typedef void bigfloat;
#define make_bigfloat(f)     This XEmacs does not support bigfloats
#define make_bigfloat_bf(f)  This XEmacs does not support bigfloast

#endif /* HAVE_BIGFLOAT */

EXFUN (Fbigfloatp, 1);

/********************************* Floating *********************************/
extern Lisp_Object Qfloatingp, Qbigfloat;
extern Lisp_Object Qread_default_float_format, Vread_default_float_format;

#define FLOATINGP(x) (FLOATP (x) || BIGFLOATP (x))
#define CHECK_FLOATING(x) do {			\
 if (!FLOATINGP (x))				\
   dead_wrong_type_argument (Qfloatingp, x);	\
 } while (0)
#define CONCHECK_FLOATING(x) do {		\
 if (!FLOATINGP (x))				\
   x = wrong_type_argument (Qfloating, x);	\
}  while (0)

EXFUN (Ffloatp, 1);


/********************************** Reals ***********************************/
extern Lisp_Object Qrealp;

#define REALP(x) (RATIONALP (x) || FLOATINGP (x))
#define CHECK_REAL(x) do {			\
 if (!REALP (x))				\
   dead_wrong_type_argument (Qrealp, x);	\
 } while (0)
#define CONCHECK_REAL(x) do {			\
 if (!REALP (x))				\
   x = wrong_type_argument (Qrealp, x);		\
}  while (0)

EXFUN (Frealp, 1);


/********************************* Numbers **********************************/
extern Lisp_Object Qnumberp;

#define NUMBERP(x) REALP (x)
#define CHECK_NUMBER(x) do {			\
  if (!NUMBERP (x))				\
    dead_wrong_type_argument (Qnumberp, x);	\
} while (0)
#define CONCHECK_NUMBER(x) do {			\
  if (!NUMBERP (x))				\
    x = wrong_type_argument (Qnumberp, x);	\
} while (0)

EXFUN (Fcanonicalize_number, 1);

enum number_type {FIXNUM_T, BIGNUM_T, RATIO_T, FLOAT_T, BIGFLOAT_T};

extern enum number_type get_number_type (Lisp_Object);
extern enum number_type promote_args (Lisp_Object *, Lisp_Object *);

#endif /* INCLUDED_number_h_ */
