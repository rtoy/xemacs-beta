/* Numeric types for XEmacs.
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
#include "lisp.h"

Lisp_Object Qrationalp, Qfloatingp, Qrealp;
Lisp_Object Vdefault_float_precision;
Fixnum Vmost_negative_fixnum, Vmost_positive_fixnum;
static Lisp_Object Qunsupported_type;
static Lisp_Object Vbigfloat_max_prec;
static int number_initialized;

#ifdef HAVE_BIGNUM
bignum scratch_bignum, scratch_bignum2;
#endif
#ifdef HAVE_RATIO
ratio scratch_ratio;
#endif
#ifdef HAVE_BIGFLOAT
bigfloat scratch_bigfloat, scratch_bigfloat2;
#endif

/********************************* Bignums **********************************/
#ifdef HAVE_BIGNUM
static void
bignum_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  CIbyte *bstr = bignum_to_string (XBIGNUM_DATA (obj), 10);
  write_c_string (printcharfun, bstr);
  xfree (bstr, CIbyte *);
}

static void
bignum_finalize (void *header, int for_disksave)
{
  if (for_disksave)
    invalid_operation ("Can't dump an XEmacs containing bignum objects",
		       VOID_TO_LISP (header));
  bignum_fini (((Lisp_Bignum *)header)->data);
}

static int
bignum_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return bignum_eql (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2));
}

static Hashcode
bignum_hash (Lisp_Object obj, int depth)
{
  return bignum_hashcode (XBIGNUM_DATA (obj));
}

static const struct memory_description bignum_description[] = {
  { XD_OPAQUE_PTR, offsetof (Lisp_Bignum, data) },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("bignum", bignum, 0, 0,
			       bignum_print, bignum_finalize, bignum_equal,
			       bignum_hash, bignum_description, Lisp_Bignum);

Lisp_Object
string_to_bignum (const Ibyte *str, Bytecount len, int base)
{
  Lisp_Object b = make_bignum (0L);
  /* GMP bignum_set_string returns random values with initial + sign */
  if (*str == '+')
    str++;
  /* GMP bignum_set_string returns random values when fed an empty string */
  if (*str == '\0')
    return make_int (0);
  return (bignum_set_string (XBIGNUM_DATA (b), (const char *) str, base) < 0)
    ? Fsignal (Qinvalid_read_syntax,
	       list3 (build_msg_string
		      ("Invalid integer constant in reader"),
		      make_string (str, len),
		      make_int (10)))
    : b;
}

#else /* !HAVE_BIGNUM */

Lisp_Object Qbignump;

#endif /* HAVE_BIGNUM */

DEFUN ("bignump", Fbignump, 1, 1, 0, /*
Return t if OBJECT is a bignum, nil otherwise.
*/
       (object))
{
  return BIGNUMP (object) ? Qt : Qnil;
}


/********************************* Integers *********************************/
DEFUN ("integerp", Fintegerp, 1, 1, 0, /*
Return t if OBJECT is an integer, nil otherwise.
*/
       (object))
{
  return INTEGERP (object) ? Qt : Qnil;
}

DEFUN ("evenp", Fevenp, 1, 1, 0, /*
Return t if INTEGER is even, nil otherwise.
*/
       (integer))
{
  CONCHECK_INTEGER (integer);
  return (BIGNUMP (integer)
	  ? bignum_evenp (XBIGNUM_DATA (integer))
	  : XTYPE (integer) == Lisp_Type_Int_Even) ? Qt : Qnil;
}

DEFUN ("odd", Foddp, 1, 1, 0, /*
Return t if INTEGER is odd, nil otherwise.
*/
       (integer))
{
  CONCHECK_INTEGER (integer);
  return (BIGNUMP (integer)
	  ? bignum_oddp (XBIGNUM_DATA (integer))
	  : XTYPE (integer) == Lisp_Type_Int_Odd) ? Qt : Qnil;
}


/********************************** Ratios **********************************/
#ifdef HAVE_RATIO
static void
ratio_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  CIbyte *rstr = ratio_to_string (XRATIO_DATA (obj), 10);
  write_c_string (printcharfun, rstr);
  xfree (rstr, CIbyte *);
}

static void
ratio_finalize (void *header, int for_disksave)
{
  if (for_disksave)
    invalid_operation ("Can't dump an XEmacs containing ratio objects",
		       VOID_TO_LISP (header));
  ratio_fini (((Lisp_Ratio *)header)->data);
}
;

static int
ratio_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return ratio_eql (XRATIO_DATA (obj1), XRATIO_DATA (obj2));
}

static Hashcode
ratio_hash (Lisp_Object obj, int depth)
{
  return ratio_hashcode (XRATIO_DATA (obj));
}

static const struct memory_description ratio_description[] = {
  { XD_OPAQUE_PTR, offsetof (Lisp_Ratio, data) },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("ratio", ratio, 0, 0,
			       ratio_print, ratio_finalize, ratio_equal,
			       ratio_hash, ratio_description, Lisp_Ratio);

#else /* !HAVE_RATIO */

Lisp_Object Qratiop;

#endif /* HAVE_RATIO */

DEFUN ("ratiop", Fratiop, 1, 1, 0, /*
Return t if OBJECT is a ratio, nil otherwise.
*/
       (object))
{
  return RATIOP (object) ? Qt : Qnil;
}


/******************************** Rationals *********************************/
DEFUN ("rationalp", Frationalp, 1, 1, 0, /*
Return t if OBJECT is a rational, nil otherwise.
*/
       (object))
{
  return RATIONALP (object) ? Qt : Qnil;
}

DEFUN ("numerator", Fnumerator, 1, 1, 0, /*
Return the numerator of the canonical form of RATIONAL.
If RATIONAL is an integer, RATIONAL is returned.
*/
       (rational))
{
  CONCHECK_RATIONAL (rational);
#ifdef HAVE_RATIO
  return RATIOP (rational)
    ? make_bignum_bg (XRATIO_NUMERATOR (rational))
    : rational;
#else
  return rational;
#endif
}

DEFUN ("denominator", Fdenominator, 1, 1, 0, /*
Return the denominator of the canonical form of RATIONAL.
If RATIONAL is an integer, 1 is returned.
*/
       (rational))
{
  CONCHECK_RATIONAL (rational);
#ifdef HAVE_RATIO
  return RATIOP (rational)
    ? make_bignum_bg (XRATIO_DENOMINATOR (rational))
    : make_int (1);
#else
  return rational;
#endif
}


/******************************** Bigfloats *********************************/
#ifdef HAVE_BIGFLOAT
static void
bigfloat_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  CIbyte *fstr = bigfloat_to_string (XBIGFLOAT_DATA (obj), 10);
  write_c_string (printcharfun, fstr);
  xfree (fstr, CIbyte *);
}

static void
bigfloat_finalize (void *header, int for_disksave)
{
  if (for_disksave)
    invalid_operation ("Can't dump an XEmacs containing bigfloat objects",
		       VOID_TO_LISP (header));
  bigfloat_fini (((Lisp_Bigfloat *)header)->bf);
}

static int
bigfloat_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return bigfloat_eql (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2));
}

static Hashcode
bigfloat_hash (Lisp_Object obj, int depth)
{
  return bigfloat_hashcode (XBIGFLOAT_DATA (obj));
}

static const struct memory_description bigfloat_description[] = {
  { XD_OPAQUE_PTR, offsetof (Lisp_Bigfloat, bf) },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("bigfloat", bigfloat, 1, 0,
			       bigfloat_print, bigfloat_finalize,
			       bigfloat_equal, bigfloat_hash,
			       bigfloat_description, Lisp_Bigfloat);

#else /* !HAVE_BIGFLOAT */

Lisp_Object Qbigfloatp;

#endif /* HAVE_BIGFLOAT */

DEFUN ("bigfloatp", Fbigfloatp, 1, 1, 0, /*
Return t if OBJECT is a bigfloat, nil otherwise.
*/
       (object))
{
  return BIGFLOATP (object) ? Qt : Qnil;
}

static int
default_float_precision_changed (Lisp_Object sym, Lisp_Object *val,
				 Lisp_Object in_object, int flags)
{
  unsigned long prec;

  CONCHECK_INTEGER (*val);
#ifdef HAVE_BIGFLOAT
  if (INTP (*val))
    prec = XINT (*val);
  else 
    {
      if (!bignum_fits_ulong_p (XBIGNUM_DATA (*val)))
	args_out_of_range_3 (*val, Qzero, Vbigfloat_max_prec);
      prec = bignum_to_ulong (XBIGNUM_DATA (*val));
    }
  if (prec != 0UL)
    bigfloat_set_default_prec (prec);
#endif
  return 0;
}


/********************************* Floating *********************************/
Lisp_Object
make_floating (double d)
{
#ifdef HAVE_BIGFLOAT
  if (ZEROP (Vdefault_float_precision))
#endif
    return make_float (d);
#ifdef HAVE_BIGFLOAT
  else
    return make_bigfloat (d, 0UL);
#endif
}

DEFUN ("floatingp", Ffloatingp, 1, 1, 0, /*
Return t if OBJECT is a floating point number of any kind, nil otherwise.
*/
       (object))
{
  return FLOATINGP (object) ? Qt : Qnil;
}


/********************************** Reals ***********************************/
DEFUN ("realp", Frealp, 1, 1, 0, /*
Return t if OBJECT is a real, nil otherwise.
*/
       (object))
{
  return REALP (object) ? Qt : Qnil;
}


/********************************* Numbers **********************************/
DEFUN ("canonicalize-number", Fcanonicalize_number, 1, 1, 0, /*
Return the canonical form of NUMBER.
*/
       (number))
{
  /* The tests should go in order from larger, more expressive, or more
     complex types to smaller, less expressive, or simpler types so that a
     number can cascade all the way down to the simplest type if
     appropriate. */
#ifdef HAVE_RATIO
  if (RATIOP (number) &&
      bignum_fits_long_p (XRATIO_DENOMINATOR (number)) &&
      bignum_to_long (XRATIO_DENOMINATOR (number)) == 1L)
    number = make_bignum_bg (XRATIO_NUMERATOR (number));
#endif
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number) && bignum_fits_int_p (XBIGNUM_DATA (number)))
    {
      int n = bignum_to_int (XBIGNUM_DATA (number));
      if (NUMBER_FITS_IN_AN_EMACS_INT (n))
	number = make_int (n);
    }
#endif
  return number;
}

enum number_type
get_number_type (Lisp_Object arg)
{
  if (INTP (arg))
    return FIXNUM_T;
#ifdef HAVE_BIGNUM
  if (BIGNUMP (arg))
    return BIGNUM_T;
#endif
#ifdef HAVE_RATIO
  if (RATIOP (arg))
    return RATIO_T;
#endif
  if (FLOATP (arg))
    return FLOAT_T;
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (arg))
    return BIGFLOAT_T;
#endif
  /* Catch unintentional bad uses of this function */
  abort ();
  /* NOTREACHED */
  return FIXNUM_T;
}

/* Convert NUMBER to type TYPE.  If TYPE is BIGFLOAT_T then use the indicated
   PRECISION; otherwise, PRECISION is ignored. */
static Lisp_Object
internal_coerce_number (Lisp_Object number, enum number_type type,
			unsigned long precision)
{
  enum number_type current_type;

  if (CHARP (number))
    number = make_int (XCHAR (number));
  else if (MARKERP (number))
    number = make_int (marker_position (number));

  /* Note that CHECK_NUMBER ensures that NUMBER is a supported type.  Hence,
     we abort() in the #else sections below, because it shouldn't be possible
     to arrive there. */
  CHECK_NUMBER (number);
  current_type = get_number_type (number);
  switch (current_type)
    {
    case FIXNUM_T:
      switch (type)
	{
	case FIXNUM_T:
	  return number;
	case BIGNUM_T:
#ifdef HAVE_BIGNUM
	  return make_bignum (XREALINT (number));
#else
	  abort ();
#endif /* HAVE_BIGNUM */
	case RATIO_T:
#ifdef HAVE_RATIO
	  return make_ratio (XREALINT (number), 1UL);
#else
	  abort ();
#endif /* HAVE_RATIO */
	case FLOAT_T:
	  return make_float (XREALINT (number));
	case BIGFLOAT_T:
#ifdef HAVE_BIGFLOAT
	  return make_bigfloat (XREALINT (number), precision);
#else
	  abort ();
#endif /* HAVE_BIGFLOAT */
	}
    case BIGNUM_T:
#ifdef HAVE_BIGNUM
      switch (type)
	{
	case FIXNUM_T:
	  return make_int (bignum_to_long (XBIGNUM_DATA (number)));
	case BIGNUM_T:
	  return number;
	case RATIO_T:
#ifdef HAVE_RATIO
	  bignum_set_long (scratch_bignum, 1L);
	  return make_ratio_bg (XBIGNUM_DATA (number), scratch_bignum);
#else
	  abort ();
#endif /* HAVE_RATIO */
	case FLOAT_T:
	  return make_float (bignum_to_double (XBIGNUM_DATA (number)));
	case BIGFLOAT_T:
#ifdef HAVE_BIGFLOAT
	  {
	    Lisp_Object temp;
	    temp = make_bigfloat (0.0, precision);
	    bigfloat_set_bignum (XBIGFLOAT_DATA (temp), XBIGNUM_DATA (number));
	    return temp;
	  }
#else
	  abort ();
#endif /* HAVE_BIGFLOAT */
	}
#else
      abort ();
#endif /* HAVE_BIGNUM */
    case RATIO_T:
#ifdef HAVE_RATIO
      switch (type)
	{
	case FIXNUM_T:
	  bignum_div (scratch_bignum, XRATIO_NUMERATOR (number),
		      XRATIO_DENOMINATOR (number));
	  return make_int (bignum_to_long (scratch_bignum));
	case BIGNUM_T:
	  bignum_div (scratch_bignum, XRATIO_NUMERATOR (number),
		      XRATIO_DENOMINATOR (number));
	  return make_bignum_bg (scratch_bignum);
	case RATIO_T:
	  return number;
	case FLOAT_T:
	  return make_float (ratio_to_double (XRATIO_DATA (number)));
	case BIGFLOAT_T:
#ifdef HAVE_BIGFLOAT
	  {
	    Lisp_Object temp;
	    temp = make_bigfloat (0.0, precision);
	    bigfloat_set_ratio (XBIGFLOAT_DATA (temp), XRATIO_DATA (number));
	    return temp;
	  }
#else
	  abort ();
#endif /* HAVE_BIGFLOAT */
	}
#else
      abort ();
#endif /* HAVE_RATIO */
    case FLOAT_T:
      switch (type)
	{
	case FIXNUM_T:
	  return Ftruncate (number);
	case BIGNUM_T:
#ifdef HAVE_BIGNUM
	  bignum_set_double (scratch_bignum, XFLOAT_DATA (number));
	  return make_bignum_bg (scratch_bignum);
#else
	  abort ();
#endif /* HAVE_BIGNUM */
	case RATIO_T:
#ifdef HAVE_RATIO
	  ratio_set_double (scratch_ratio, XFLOAT_DATA (number));
	  return make_ratio_rt (scratch_ratio);
#else
	  abort ();
#endif /* HAVE_RATIO */
	case FLOAT_T:
	  return number;
	case BIGFLOAT_T:
#ifdef HAVE_BIGFLOAT
	  bigfloat_set_prec (scratch_bigfloat, precision);
	  bigfloat_set_double (scratch_bigfloat, XFLOAT_DATA (number));
	  return make_bigfloat_bf (scratch_bigfloat);
#else
	  abort ();
#endif /* HAVE_BIGFLOAT */
	}
    case BIGFLOAT_T:
#ifdef HAVE_BIGFLOAT
      switch (type)
	{
	case FIXNUM_T:
	  return make_int (bigfloat_to_long (XBIGFLOAT_DATA (number)));
	case BIGNUM_T:
#ifdef HAVE_BIGNUM
	  bignum_set_bigfloat (scratch_bignum, XBIGFLOAT_DATA (number));
	  return make_bignum_bg (scratch_bignum);
#else
	  abort ();
#endif /* HAVE_BIGNUM */
	case RATIO_T:
#ifdef HAVE_RATIO
	  ratio_set_bigfloat (scratch_ratio, XBIGFLOAT_DATA (number));
	  return make_ratio_rt (scratch_ratio);
#else
	  abort ();
#endif
	case FLOAT_T:
	  return make_float (bigfloat_to_double (XBIGFLOAT_DATA (number)));
	case BIGFLOAT_T:
	  /* FIXME: Do we need to change the precision? */
	  return number;
	}
#else
      abort ();
#endif /* HAVE_BIGFLOAT */
    }
  abort ();
  /* NOTREACHED */
  return Qzero;
}

/* This function promotes its arguments as necessary to make them both the
   same type.  It destructively modifies its arguments to do so.  Characters
   and markers are ALWAYS converted to integers. */
enum number_type
promote_args (Lisp_Object *arg1, Lisp_Object *arg2)
{
  enum number_type type1, type2;

  if (CHARP (*arg1))
    *arg1 = make_int (XCHAR (*arg1));
  else if (MARKERP (*arg1))
    *arg1 = make_int (marker_position (*arg1));
  if (CHARP (*arg2))
    *arg2 = make_int (XCHAR (*arg2));
  else if (MARKERP (*arg2))
    *arg2 = make_int (marker_position (*arg2));

  CHECK_NUMBER (*arg1);
  CHECK_NUMBER (*arg2);

  type1 = get_number_type (*arg1);
  type2 = get_number_type (*arg2);

  if (type1 < type2)
    {
      *arg1 = internal_coerce_number (*arg1, type2,
#ifdef HAVE_BIGFLOAT
				      type2 == BIGFLOAT_T
				      ? XBIGFLOAT_GET_PREC (*arg2) :
#endif
				      0UL);
      return type2;
    }
  
  if (type2 < type1)
    {
      *arg2 = internal_coerce_number (*arg2, type1,
#ifdef HAVE_BIGFLOAT
				      type1 == BIGFLOAT_T
				      ? XBIGFLOAT_GET_PREC (*arg1) :
#endif
				      0UL);
      return type1;
    }

  /* No conversion necessary */
  return type1;
}

DEFUN ("coerce-number", Fcoerce_number, 2, 3, 0, /*
Convert NUMBER to the indicated type, possibly losing information.
Do not call this function.  Use `coerce' instead.

TYPE is one of the symbols 'fixnum, 'integer, 'ratio, 'float, or 'bigfloat.
Not all of these types may be supported.

PRECISION is the number of bits of precision to use when converting to
bigfloat; it is ignored otherwise.  If nil, the default precision is used.

Note that some conversions lose information.  No error is signaled in such
cases; the information is silently lost.
*/
       (number, type, precision))
{
  CHECK_SYMBOL (type);
  if (EQ (type, Qfixnum))
    return internal_coerce_number (number, FIXNUM_T, 0UL);
  else if (EQ (type, Qinteger))
    {
      /* If bignums are available, we always convert to one first, then
	 downgrade to a fixnum if possible. */
#ifdef HAVE_BIGNUM
      return Fcanonicalize_number
	(internal_coerce_number (number, BIGNUM_T, 0UL));
#else
      return internal_coerce_number (number, FIXNUM_T, 0UL);
#endif
    }
#ifdef HAVE_RATIO
  else if (EQ (type, Qratio))
    return internal_coerce_number (number, RATIO_T, 0UL);
#endif
  else if (EQ (type, Qfloat))
    return internal_coerce_number (number, FLOAT_T, 0UL);
#ifdef HAVE_BIGFLOAT
  else if (EQ (type, Qbigfloat))
    {
      unsigned long prec;

      if (NILP (precision))
	prec = bigfloat_get_default_prec ();
      else
	{
	  CHECK_INTEGER (precision);
#ifdef HAVE_BIGNUM
	  if (INTP (precision))
#endif /* HAVE_BIGNUM */
	    prec = (unsigned long) XREALINT (precision);
#ifdef HAVE_BIGNUM
	  else
	    {
	      if (!bignum_fits_ulong_p (XBIGNUM_DATA (precision)))
		args_out_of_range (precision, Vbigfloat_max_prec);
	      prec = bignum_to_ulong (XBIGNUM_DATA (precision));
	    }
#endif /* HAVE_BIGNUM */
	}
      return internal_coerce_number (number, BIGFLOAT_T, prec);
    }
#endif /* HAVE_BIGFLOAT */

  Fsignal (Qunsupported_type, type);
  /* NOTREACHED */
  return Qnil;
}


void
syms_of_number (void)
{
#ifdef HAVE_BIGNUM
  INIT_LRECORD_IMPLEMENTATION (bignum);
#endif
#ifdef HAVE_RATIO
  INIT_LRECORD_IMPLEMENTATION (ratio);
#endif
#ifdef HAVE_BIGFLOAT
  INIT_LRECORD_IMPLEMENTATION (bigfloat);
#endif

  /* Type predicates */
  DEFSYMBOL (Qrationalp);
  DEFSYMBOL (Qfloatingp);
  DEFSYMBOL (Qrealp);
#ifndef HAVE_BIGNUM
  DEFSYMBOL (Qbignump);
#endif
#ifndef HAVE_RATIO
  DEFSYMBOL (Qratiop);
#endif
#ifndef HAVE_BIGFLOAT
  DEFSYMBOL (Qbigfloatp);
#endif

  /* Functions */
  DEFSUBR (Fbignump);
  DEFSUBR (Fintegerp);
  DEFSUBR (Fevenp);
  DEFSUBR (Foddp);
  DEFSUBR (Fratiop);
  DEFSUBR (Frationalp);
  DEFSUBR (Fnumerator);
  DEFSUBR (Fdenominator);
  DEFSUBR (Fbigfloatp);
  DEFSUBR (Ffloatingp);
  DEFSUBR (Frealp);
  DEFSUBR (Fcanonicalize_number);
  DEFSUBR (Fcoerce_number);

  /* Errors */
  DEFERROR_STANDARD (Qunsupported_type, Qwrong_type_argument);
}

void
vars_of_number (void)
{
  /* This variable is a Lisp variable rather than a number variable so that we
     can put bignums in it. */
  DEFVAR_LISP_MAGIC ("default-float-precision", &Vdefault_float_precision, /*
The default floating-point precision for newly created floating point values.
This should be 0 for the precision of the machine-supported floating point
type (the C double type), or an unsigned integer no greater than
bigfloat-max-prec (currently the size of a C unsigned long).
*/ default_float_precision_changed);
  Vdefault_float_precision = make_int (0);

  DEFVAR_CONST_LISP ("bigfloat-max-prec", &Vbigfloat_max_prec /*
The maximum number of bits of precision a bigfloat can have.
This is currently the value of ULONG_MAX on the target machine.
*/);

  DEFVAR_CONST_INT ("most-negative-fixnum", &Vmost_negative_fixnum /*
The fixnum closest in value to negative infinity.
*/);
  Vmost_negative_fixnum = EMACS_INT_MIN;

  DEFVAR_CONST_INT ("most-positive-fixnum", &Vmost_positive_fixnum /*
The fixnum closest in value to positive infinity.
*/);
  Vmost_positive_fixnum = EMACS_INT_MAX;

  Fprovide (intern ("number-types"));
#ifdef HAVE_BIGNUM
  Fprovide (intern ("bignum"));
#endif
#ifdef HAVE_RATIO
  Fprovide (intern ("ratio"));
#endif
#ifdef HAVE_BIGFLOAT
  Fprovide (intern ("bigfloat"));
#endif
}

void
init_number (void)
{
  if (!number_initialized)
    {
      number_initialized = 1;

#ifdef WITH_GMP
      init_number_gmp ();
#endif
#ifdef WITH_MP
      init_number_mp ();
#endif

#if defined(BIGNUM) && defined(BIGFLOAT)
      Vbigfloat_max_prec = make_bignum (0L);
      bignum_set_ulong (XBIGNUM_DATA (Vbigfloat_max_prec), ULONG_MAX);
#endif

#ifdef HAVE_BIGNUM
      bignum_init (scratch_bignum);
      bignum_init (scratch_bignum2);
#endif

#ifdef HAVE_RATIO
      ratio_init (scratch_ratio);
#endif

#ifdef HAVE_BIGFLOAT
      bigfloat_init (scratch_bigfloat);
      bigfloat_init (scratch_bigfloat2);
#endif
    }
}
