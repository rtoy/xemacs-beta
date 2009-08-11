/* Primitive operations on floating point for XEmacs Lisp interpreter.
   Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.30. */

/* ANSI C requires only these float functions:
   acos, asin, atan, atan2, ceil, cos, cosh, exp, fabs, floor, fmod,
   frexp, ldexp, log, log10, modf, pow, sin, sinh, sqrt, tan, tanh.

   Define HAVE_INVERSE_HYPERBOLIC if you have acosh, asinh, and atanh.
   Define HAVE_CBRT if you have cbrt().
   Define HAVE_RINT if you have rint().
   If you don't define these, then the appropriate routines will be simulated.

   Define HAVE_MATHERR if on a system supporting the SysV matherr() callback.
   (This should happen automatically.)

   Define FLOAT_CHECK_ERRNO if the float library routines set errno.
   This has no effect if HAVE_MATHERR is defined.

   Define FLOAT_CATCH_SIGILL if the float library routines signal SIGILL.
   (What systems actually do this?  Let me know. -jwz)

   Define FLOAT_CHECK_DOMAIN if the float library doesn't handle errors by
   either setting errno, or signalling SIGFPE/SIGILL.  Otherwise, domain and
   range checking will happen before calling the float routines.  This has
   no effect if HAVE_MATHERR is defined (since matherr will be called when
   a domain error occurs).
 */

#include <config.h>
#include "lisp.h"
#include "syssignal.h"
#include "sysfloat.h"

/* The code uses emacs_rint, so that it works to undefine HAVE_RINT
   if `rint' exists but does not work right.  */
#ifdef HAVE_RINT
#define emacs_rint rint
#else
static double
emacs_rint (double x)
{
  double r = floor (x + 0.5);
  double diff = fabs (r - x);
  /* Round to even and correct for any roundoff errors.  */
  if (diff >= 0.5 && (diff > 0.5 || r != 2.0 * floor (r / 2.0)))
    r += r < x ? 1.0 : -1.0;
  return r;
}
#endif

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */
static int in_float;

/* If an argument is out of range for a mathematical function,
   here is the actual argument value to use in the error message.  */
static Lisp_Object float_error_arg, float_error_arg2;
static const char *float_error_fn_name;

/* Evaluate the floating point expression D, recording NUM
   as the original argument for error messages.
   D is normally an assignment expression.
   Handle errors which may result in signals or may set errno.

   Note that float_error may be declared to return void, so you can't
   just cast the zero after the colon to (SIGTYPE) to make the types
   check properly.  */
#ifdef FLOAT_CHECK_ERRNO
#define IN_FLOAT(d, name, num)				\
  do {							\
    float_error_arg = num;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    if (errno != 0) in_float_error ();			\
  } while (0)
#define IN_FLOAT2(d, name, num, num2)			\
  do {							\
    float_error_arg = num;				\
    float_error_arg2 = num2;				\
    float_error_fn_name = name;				\
    in_float = 2; errno = 0; (d); in_float = 0;		\
    if (errno != 0) in_float_error ();			\
  } while (0)
#else
#define IN_FLOAT(d, name, num) (in_float = 1, (d), in_float = 0)
#define IN_FLOAT2(d, name, num, num2) (in_float = 2, (d), in_float = 0)
#endif


#define arith_error(op,arg) \
  Fsignal (Qarith_error, list2 (build_msg_string (op), arg))
#define range_error(op,arg) \
  Fsignal (Qrange_error, list2 (build_msg_string (op), arg))
#define range_error2(op,a1,a2) \
  Fsignal (Qrange_error, list3 (build_msg_string (op), a1, a2))
#define domain_error(op,arg) \
  Fsignal (Qdomain_error, list2 (build_msg_string (op), arg))
#define domain_error2(op,a1,a2) \
  Fsignal (Qdomain_error, list3 (build_msg_string (op), a1, a2))


/* Convert float to Lisp Integer if it fits, else signal a range
   error using the given arguments.  If bignums are available, range errors
   are never signaled.  */
static Lisp_Object
float_to_int (double x,
#ifdef HAVE_BIGNUM
	      const char *UNUSED (name), Lisp_Object UNUSED (num),
	      Lisp_Object UNUSED (num2)
#else
	      const char *name, Lisp_Object num, Lisp_Object num2
#endif
	      )
{
#ifdef HAVE_BIGNUM
  bignum_set_double (scratch_bignum, x);
  return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
  REGISTER EMACS_INT result = (EMACS_INT) x;

  if (result > EMACS_INT_MAX || result < EMACS_INT_MIN)
    {
      if (!UNBOUNDP (num2))
	range_error2 (name, num, num2);
      else
	range_error (name, num);
    }
  return make_int (result);
#endif /* HAVE_BIGNUM */
}


static void
in_float_error (void)
{
  switch (errno)
  {
  case 0:
    break;
  case EDOM:
    if (in_float == 2)
      domain_error2 (float_error_fn_name, float_error_arg, float_error_arg2);
    else
      domain_error (float_error_fn_name, float_error_arg);
    break;
  case ERANGE:
    range_error (float_error_fn_name, float_error_arg);
    break;
  default:
    arith_error (float_error_fn_name, float_error_arg);
    break;
  }
}


static Lisp_Object
mark_float (Lisp_Object UNUSED (obj))
{
  return Qnil;
}

static int
float_equal (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth))
{
  return (extract_float (obj1) == extract_float (obj2));
}

static Hashcode
float_hash (Lisp_Object obj, int UNUSED (depth))
{
  /* mod the value down to 32-bit range */
  /* #### change for 64-bit machines */
  return (unsigned long) fmod (extract_float (obj), 4e9);
}

static const struct memory_description float_description[] = {
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("float", float,
				     1, /*dumpable-flag*/
				     mark_float, print_float, 0, float_equal,
				     float_hash, float_description,
				     Lisp_Float);

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (Lisp_Object num)
{
  if (FLOATP (num))
    return XFLOAT_DATA (num);

  if (INTP (num))
    return (double) XINT (num);

#ifdef HAVE_BIGNUM
  if (BIGNUMP (num))
    return bignum_to_double (XBIGNUM_DATA (num));
#endif

#ifdef HAVE_RATIO
  if (RATIOP (num))
    return ratio_to_double (XRATIO_DATA (num));
#endif

#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (num))
    return bigfloat_to_double (XBIGFLOAT_DATA (num));
#endif

  return extract_float (wrong_type_argument (Qnumberp, num));
}

/* Trig functions.  */

DEFUN ("acos", Facos, 1, 1, 0, /*
Return the inverse cosine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("acos", number);
#endif
  IN_FLOAT (d = acos (d), "acos", number);
  return make_float (d);
}

DEFUN ("asin", Fasin, 1, 1, 0, /*
Return the inverse sine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("asin", number);
#endif
  IN_FLOAT (d = asin (d), "asin", number);
  return make_float (d);
}

DEFUN ("atan", Fatan, 1, 2, 0, /*
Return the inverse tangent of NUMBER.
If optional second argument NUMBER2 is provided,
return atan2 (NUMBER, NUMBER2).
*/
       (number, number2))
{
  double d = extract_float (number);

  if (NILP (number2))
    IN_FLOAT (d = atan (d), "atan", number);
  else
    {
      double d2 = extract_float (number2);
#ifdef FLOAT_CHECK_DOMAIN
      if (d == 0.0 && d2 == 0.0)
	domain_error2 ("atan", number, number2);
#endif
      IN_FLOAT2 (d = atan2 (d, d2), "atan", number, number2);
    }
  return make_float (d);
}

DEFUN ("cos", Fcos, 1, 1, 0, /*
Return the cosine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = cos (d), "cos", number);
  return make_float (d);
}

DEFUN ("sin", Fsin, 1, 1, 0, /*
Return the sine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = sin (d), "sin", number);
  return make_float (d);
}

DEFUN ("tan", Ftan, 1, 1, 0, /*
Return the tangent of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  double c = cos (d);
#ifdef FLOAT_CHECK_DOMAIN
  if (c == 0.0)
    domain_error ("tan", number);
#endif
  IN_FLOAT (d = (sin (d) / c), "tan", number);
  return make_float (d);
}

/* Bessel functions */
#if 0 /* Leave these out unless we find there's a reason for them.  */

DEFUN ("bessel-j0", Fbessel_j0, 1, 1, 0, /*
Return the bessel function j0 of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = j0 (d), "bessel-j0", number);
  return make_float (d);
}

DEFUN ("bessel-j1", Fbessel_j1, 1, 1, 0, /*
Return the bessel function j1 of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = j1 (d), "bessel-j1", number);
  return make_float (d);
}

DEFUN ("bessel-jn", Fbessel_jn, 2, 2, 0, /*
Return the order N bessel function output jn of NUMBER.
The first number (the order) is truncated to an integer.
*/
       (number1, number2))
{
  int i1 = extract_float (number1);
  double f2 = extract_float (number2);

  IN_FLOAT (f2 = jn (i1, f2), "bessel-jn", number1);
  return make_float (f2);
}

DEFUN ("bessel-y0", Fbessel_y0, 1, 1, 0, /*
Return the bessel function y0 of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = y0 (d), "bessel-y0", number);
  return make_float (d);
}

DEFUN ("bessel-y1", Fbessel_y1, 1, 1, 0, /*
Return the bessel function y1 of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = y1 (d), "bessel-y0", number);
  return make_float (d);
}

DEFUN ("bessel-yn", Fbessel_yn, 2, 2, 0, /*
Return the order N bessel function output yn of NUMBER.
The first number (the order) is truncated to an integer.
*/
       (number1, number2))
{
  int i1 = extract_float (number1);
  double f2 = extract_float (number2);

  IN_FLOAT (f2 = yn (i1, f2), "bessel-yn", number1);
  return make_float (f2);
}

#endif /* 0 (bessel functions) */

/* Error functions. */
#if 0 /* Leave these out unless we see they are worth having.  */

DEFUN ("erf", Ferf, 1, 1, 0, /*
Return the mathematical error function of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = erf (d), "erf", number);
  return make_float (d);
}

DEFUN ("erfc", Ferfc, 1, 1, 0, /*
Return the complementary error function of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = erfc (d), "erfc", number);
  return make_float (d);
}

DEFUN ("log-gamma", Flog_gamma, 1, 1, 0, /*
Return the log gamma of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = lgamma (d), "log-gamma", number);
  return make_float (d);
}

#endif /* 0 (error functions) */


/* Root and Log functions. */

DEFUN ("exp", Fexp, 1, 1, 0, /*
Return the exponential base e of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 709.7827)   /* Assume IEEE doubles here */
    range_error ("exp", number);
  else if (d < -709.0)
    return make_float (0.0);
  else
#endif
    IN_FLOAT (d = exp (d), "exp", number);
  return make_float (d);
}

DEFUN ("expt", Fexpt, 2, 2, 0, /*
Return the exponential NUMBER1 ** NUMBER2.
*/
       (number1, number2))
{
#ifdef HAVE_BIGNUM
  if (INTEGERP (number1) && INTP (number2))
    {
      if (INTP (number1))
	{
	  bignum_set_long (scratch_bignum2, XREALINT (number1));
	  bignum_pow (scratch_bignum, scratch_bignum2, XREALINT (number2));
	}
      else
	bignum_pow (scratch_bignum, XBIGNUM_DATA (number1),
		    XREALINT (number2));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
#endif

  if (INTP (number1) && /* common lisp spec */
      INTP (number2)) /* don't promote, if both are ints */
    {
      EMACS_INT retval;
      EMACS_INT x = XINT (number1);
      EMACS_INT y = XINT (number2);

      if (y < 0)
	{
	  if (x == 1)
	    retval = 1;
	  else if (x == -1)
	    retval = (y & 1) ? -1 : 1;
	  else
	    retval = 0;
	}
      else
	{
	  retval = 1;
	  while (y > 0)
	    {
	      if (y & 1)
		retval *= x;
	      x *= x;
	      y = (EMACS_UINT) y >> 1;
	    }
	}
      return make_int (retval);
    }

#if defined(HAVE_BIGFLOAT) && defined(bigfloat_pow)
  if (BIGFLOATP (number1) && INTEGERP (number2))
    {
      unsigned long exponent;

#ifdef HAVE_BIGNUM
      if (BIGNUMP (number2))
	exponent = bignum_to_ulong (XBIGNUM_DATA (number2));
      else
#endif
	exponent = XUINT (number2);
      bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number1));
      bigfloat_pow (scratch_bigfloat, XBIGFLOAT_DATA (number1), exponent);
      return make_bigfloat_bf (scratch_bigfloat);
    }
#endif

  {
    double f1 = extract_float (number1);
    double f2 = extract_float (number2);
    /* Really should check for overflow, too */
    if (f1 == 0.0 && f2 == 0.0)
      f1 = 1.0;
# ifdef FLOAT_CHECK_DOMAIN
    else if ((f1 == 0.0 && f2 < 0.0) || (f1 < 0 && f2 != floor(f2)))
      domain_error2 ("expt", number1, number2);
# endif /* FLOAT_CHECK_DOMAIN */
    IN_FLOAT2 (f1 = pow (f1, f2), "expt", number1, number2);
    return make_float (f1);
  }
}

DEFUN ("log", Flog, 1, 2, 0, /*
Return the natural logarithm of NUMBER.
If second optional argument BASE is given, return the logarithm of
NUMBER using that base.
*/
       (number, base))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error2 ("log", number, base);
#endif
  if (NILP (base))
    IN_FLOAT (d = log (d), "log", number);
  else
    {
      double b = extract_float (base);
#ifdef FLOAT_CHECK_DOMAIN
      if (b <= 0.0 || b == 1.0)
	domain_error2 ("log", number, base);
#endif
      if (b == 10.0)
	IN_FLOAT2 (d = log10 (d), "log", number, base);
      else
	IN_FLOAT2 (d = (log (d) / log (b)), "log", number, base);
    }
  return make_float (d);
}


DEFUN ("log10", Flog10, 1, 1, 0, /*
Return the logarithm base 10 of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error ("log10", number);
#endif
  IN_FLOAT (d = log10 (d), "log10", number);
  return make_float (d);
}


DEFUN ("sqrt", Fsqrt, 1, 1, 0, /*
Return the square root of NUMBER.
*/
       (number))
{
  double d;

#if defined(HAVE_BIGFLOAT) && defined(bigfloat_sqrt)
  if (BIGFLOATP (number))
    {
      bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
      bigfloat_sqrt (scratch_bigfloat, XBIGFLOAT_DATA (number));
      return make_bigfloat_bf (scratch_bigfloat);
    }
#endif /* HAVE_BIGFLOAT */
  d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 0.0)
    domain_error ("sqrt", number);
#endif
  IN_FLOAT (d = sqrt (d), "sqrt", number);
  return make_float (d);
}


DEFUN ("cube-root", Fcube_root, 1, 1, 0, /*
Return the cube root of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef HAVE_CBRT
  IN_FLOAT (d = cbrt (d), "cube-root", number);
#else
  if (d >= 0.0)
    IN_FLOAT (d = pow (d, 1.0/3.0), "cube-root", number);
  else
    IN_FLOAT (d = -pow (-d, 1.0/3.0), "cube-root", number);
#endif
  return make_float (d);
}

/* Inverse trig functions. */

DEFUN ("acosh", Facosh, 1, 1, 0, /*
Return the inverse hyperbolic cosine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 1.0)
    domain_error ("acosh", number);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = acosh (d), "acosh", number);
#else
  IN_FLOAT (d = log (d + sqrt (d*d - 1.0)), "acosh", number);
#endif
  return make_float (d);
}

DEFUN ("asinh", Fasinh, 1, 1, 0, /*
Return the inverse hyperbolic sine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = asinh (d), "asinh", number);
#else
  IN_FLOAT (d = log (d + sqrt (d*d + 1.0)), "asinh", number);
#endif
  return make_float (d);
}

DEFUN ("atanh", Fatanh, 1, 1, 0, /*
Return the inverse hyperbolic tangent of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d >= 1.0 || d <= -1.0)
    domain_error ("atanh", number);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = atanh (d), "atanh", number);
#else
  IN_FLOAT (d = 0.5 * log ((1.0 + d) / (1.0 - d)), "atanh", number);
#endif
  return make_float (d);
}

DEFUN ("cosh", Fcosh, 1, 1, 0, /*
Return the hyperbolic cosine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("cosh", number);
#endif
  IN_FLOAT (d = cosh (d), "cosh", number);
  return make_float (d);
}

DEFUN ("sinh", Fsinh, 1, 1, 0, /*
Return the hyperbolic sine of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("sinh", number);
#endif
  IN_FLOAT (d = sinh (d), "sinh", number);
  return make_float (d);
}

DEFUN ("tanh", Ftanh, 1, 1, 0, /*
Return the hyperbolic tangent of NUMBER.
*/
       (number))
{
  double d = extract_float (number);
  IN_FLOAT (d = tanh (d), "tanh", number);
  return make_float (d);
}

/* Rounding functions */

DEFUN ("abs", Fabs, 1, 1, 0, /*
Return the absolute value of NUMBER.
*/
       (number))
{
  if (FLOATP (number))
    {
      IN_FLOAT (number = make_float (fabs (XFLOAT_DATA (number))),
		"abs", number);
      return number;
    }

  if (INTP (number))
#ifdef HAVE_BIGNUM
    /* The most negative Lisp fixnum will overflow */
    return (XINT (number) >= 0) ? number : make_integer (- XINT (number));
#else
    return (XINT (number) >= 0) ? number : make_int (- XINT (number));
#endif

#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      if (bignum_sign (XBIGNUM_DATA (number)) >= 0)
	return number;
      bignum_abs (scratch_bignum, XBIGNUM_DATA (number));
      return make_bignum_bg (scratch_bignum);
    }
#endif

#ifdef HAVE_RATIO
  if (RATIOP (number))
    {
      if (ratio_sign (XRATIO_DATA (number)) >= 0)
	return number;
      ratio_abs (scratch_ratio, XRATIO_DATA (number));
      return make_ratio_rt (scratch_ratio);
    }
#endif

#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      if (bigfloat_sign (XBIGFLOAT_DATA (number)) >= 0)
	return number;
      bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
      bigfloat_abs (scratch_bigfloat, XBIGFLOAT_DATA (number));
      return make_bigfloat_bf (scratch_bigfloat);
    }
#endif

  return Fabs (wrong_type_argument (Qnumberp, number));
}

DEFUN ("float", Ffloat, 1, 1, 0, /*
Return the floating point number numerically equal to NUMBER.
*/
       (number))
{
  if (INTP (number))
    return make_float ((double) XINT (number));

#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
#ifdef HAVE_BIGFLOAT
      if (ZEROP (Vdefault_float_precision))
#endif
	return make_float (bignum_to_double (XBIGNUM_DATA (number)));
#ifdef HAVE_BIGFLOAT
      else
	{
	  bigfloat_set_prec (scratch_bigfloat, bigfloat_get_default_prec ());
	  bigfloat_set_bignum (scratch_bigfloat, XBIGNUM_DATA (number));
	  return make_bigfloat_bf (scratch_bigfloat);
	}
#endif /* HAVE_BIGFLOAT */
    }
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
  if (RATIOP (number))
    return make_float (ratio_to_double (XRATIO_DATA (number)));
#endif

  if (FLOATP (number))		/* give 'em the same float back */
    return number;

  return Ffloat (wrong_type_argument (Qnumberp, number));
}

DEFUN ("logb", Flogb, 1, 1, 0, /*
Return largest integer <= the base 2 log of the magnitude of NUMBER.
This is the same as the exponent of a float.
*/
       (number))
{
  double f = extract_float (number);

  if (f == 0.0)
    return make_int (EMACS_INT_MIN);
#ifdef HAVE_LOGB
  {
    Lisp_Object val;
    IN_FLOAT (val = make_int ((EMACS_INT) logb (f)), "logb", number);
    return val;
  }
#else
#ifdef HAVE_FREXP
  {
    int exqp;
    IN_FLOAT (frexp (f, &exqp), "logb", number);
    return make_int (exqp - 1);
  }
#else
  {
    int i;
    double d;
    EMACS_INT val;
    if (f < 0.0)
      f = -f;
    val = -1;
    while (f < 0.5)
      {
        for (i = 1, d = 0.5; d * d >= f; i += i)
          d *= d;
        f /= d;
        val -= i;
      }
    while (f >= 1.0)
      {
        for (i = 1, d = 2.0; d * d <= f; i += i)
          d *= d;
        f /= d;
        val += i;
      }
    return make_int (val);
  }
#endif /* ! HAVE_FREXP */
#endif /* ! HAVE_LOGB */
}

#ifdef WITH_NUMBER_TYPES
#define ROUNDING_CONVERT(conversion, return_float)      \
  CONVERT_WITH_NUMBER_TYPES(conversion, return_float)
#else
#define ROUNDING_CONVERT(conversion, return_float)      \
  CONVERT_WITHOUT_NUMBER_TYPES(conversion, return_float)
#endif

#define CONVERT_WITH_NUMBER_TYPES(conversion, return_float)     \
  if (!NILP (divisor))                                          \
    {                                                           \
      switch (promote_args (&number, &divisor))                 \
        {                                                       \
        case FIXNUM_T:                                          \
          return conversion##_two_fixnum (number, divisor,      \
                                          return_float);        \
          MAYBE_TWO_ARGS_WITH_NUMBER_TYPES (conversion,         \
                                            BIGNUM,             \
                                            return_float);      \
          MAYBE_TWO_ARGS_WITH_NUMBER_TYPES (conversion,         \
                                            RATIO,              \
                                            return_float);      \
          MAYBE_TWO_ARGS_WITH_NUMBER_TYPES (conversion,         \
                                            BIGFLOAT,           \
                                            return_float);      \
          default: /* FLOAT_T */                                \
            return conversion##_two_float (number,divisor,      \
                                           return_float);       \
        }                                                       \
     }                                                          \
                                                                \
  /* Try this first, the arg is probably a float:  */           \
  if (FLOATP (number))                                          \
    return conversion##_one_float (number, return_float);       \
                                                                \
  MAYBE_ONE_ARG_WITH_NUMBER_TYPES (conversion,                  \
                                   RATIO, return_float);        \
  MAYBE_ONE_ARG_WITH_NUMBER_TYPES (conversion,                  \
                                   BIGFLOAT, return_float);     \
  return conversion##_one_mundane_arg (number, divisor,         \
                                       return_float)
      

#define CONVERT_WITHOUT_NUMBER_TYPES(conversion, return_float)  \
  if (!NILP (divisor))						\
    {                                                           \
      /* The promote_args call if number types are available    \
         does these conversions, we do them too for symmetry: */\
      if (CHARP (number))                                       \
        {                                                       \
          number = make_int (XCHAR (number));                   \
        }                                                       \
      else if (MARKERP (number))				\
        {                                                       \
          number = make_int (marker_position (number));         \
        }                                                       \
                                                                \
      if (CHARP (divisor))                                      \
        {                                                       \
          divisor = make_int (XCHAR (divisor));                 \
        }                                                       \
      else if (MARKERP (divisor))				\
        {                                                       \
          divisor = make_int (marker_position (divisor));       \
        }                                                       \
                                                                \
      CHECK_INT_OR_FLOAT (divisor);                             \
      if (INTP (number) && INTP (divisor))                      \
        {                                                       \
          return conversion##_two_fixnum (number, divisor,      \
                                        return_float);          \
        }                                                       \
      else                                                      \
        {                                                       \
          return conversion##_two_float (number, divisor,       \
                                           return_float);       \
        }                                                       \
    }                                                           \
                                                                \
  /* Try this first, the arg is probably a float:  */           \
  if (FLOATP (number))                                          \
    return conversion##_one_float (number, return_float);       \
                                                                \
  return conversion##_one_mundane_arg (number, divisor,		\
				       return_float)		\

#ifdef WITH_NUMBER_TYPES

#ifdef HAVE_BIGNUM
#define MAYBE_TWO_ARGS_BIGNUM(conversion, return_float)               \
  case BIGNUM_T:                                                      \
  return conversion##_two_bignum (number, divisor, return_float)

#define MAYBE_ONE_ARG_BIGNUM(converse, return_float)                \
  if (BIGNUM_P (number))                                            \
    return conversion##_one_bignum (number, divisor, return_float) 
#else
#define MAYBE_TWO_ARGS_BIGNUM(conversion, return_float)
#define MAYBE_ONE_ARG_BIGNUM(converse, return_float) 
#endif

#ifdef HAVE_RATIO 
#define MAYBE_TWO_ARGS_RATIO(conversion, return_float)          \
  case RATIO_T:                                                 \
  return conversion##_two_ratio (number, divisor, return_float)

#define MAYBE_ONE_ARG_RATIO(conversion, return_float)               \
  if (RATIOP (number))                                              \
    return conversion##_one_ratio (number, divisor, return_float) 
#else
#define MAYBE_TWO_ARGS_RATIO(conversion, return_float)
#define MAYBE_ONE_ARG_RATIO(converse, return_float) 
#endif

#ifdef HAVE_BIGFLOAT
#define MAYBE_TWO_ARGS_BIGFLOAT(conversion, return_float)           \
  case BIGFLOAT_T:                                                  \
  return conversion##_two_bigfloat (number, divisor, return_float)

#define MAYBE_ONE_ARG_BIGFLOAT(conversion, return_float)            \
  if (BIGFLOATP (number))                                           \
    return conversion##_one_bigfloat (number, divisor, return_float) 
#else
#define MAYBE_TWO_ARGS_BIGFLOAT(conversion, return_float)
#define MAYBE_ONE_ARG_BIGFLOAT(converse, return_float) 
#endif

#define MAYBE_TWO_ARGS_WITH_NUMBER_TYPES(convers, upcase, return_float) \
  MAYBE_TWO_ARGS_##upcase(convers, return_float)

#define MAYBE_ONE_ARG_WITH_NUMBER_TYPES(convers, upcase, return_float) \
  MAYBE_ONE_ARG_##upcase(convers, return_float)

#endif /* WITH_NUMBER_TYPES */

#define MAYBE_EFF(str) (return_float ? "f" str : str)

/* The WITH_NUMBER_TYPES code calls promote_args, which accepts chars and
   markers as equivalent to ints. This block does the same for
   single-argument calls. */
#define MAYBE_CHAR_OR_MARKER(conversion) do {                           \
  if (CHARP (number))                                                   \
    {                                                                   \
      return conversion##_one_mundane_arg (make_int (XCHAR (number)),   \
                                           divisor, return_float);      \
    }                                                                   \
                                                                        \
  if (MARKERP (number))                                                 \
    {                                                                   \
      return conversion##_one_mundane_arg (make_int                     \
                                           (marker_position(number)),   \
                                           divisor, return_float);      \
    }                                                                   \
  } while (0)


/* The guts of the implementations of the various rounding functions: */

static Lisp_Object
ceiling_two_fixnum (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  EMACS_INT i1 = XREALINT (number);
  EMACS_INT i2 = XREALINT (divisor);
  EMACS_INT i3 = 0, i4 = 0;

  if (i2 == 0)
    Fsignal (Qarith_error, Qnil);

  /* With C89's integer /, the result is implementation-defined if either
     operand is negative, so use only nonnegative operands. Here we do
     basically the opposite of what floor_two_fixnum does, we add one in the
     non-negative case: */

  /* Make sure we use the same signs for the modulus calculation as for the
     quotient calculation: */
  if (i2 < 0)
    {
      if (i1 <= 0)
	{
	  i3 = -i1 / -i2;
	  /* Quotient is positive; add one to give the figure for
	     ceiling. */
	  if (0 != (-i1 % -i2))
	    {
	      ++i3;
	    }
	}
      else
	{
	  /* Quotient is negative; no need to add one. */
	  i3 = -(i1 / -i2);
	}
    }
  else
    {
      if (i1 < 0)
	{
	  /* Quotient is negative; no need to add one. */
	  i3 = -(-i1 / i2);
	}
      else
	{
	  i3 = i1 / i2;
	  /* Quotient is positive; add one to give the figure for
	     ceiling. */
	  if (0 != (i1 % i2))
	    {
	      ++i3;
	    }
	}
    }

  i4 = i1 - (i3 * i2);

  if (!return_float)
    {
      return values2 (make_int (i3), make_int (i4));
    }

  return values2 (make_float ((double)i3),
		  make_int (i4));
}

#ifdef HAVE_BIGNUM
static Lisp_Object
ceiling_two_bignum (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  Lisp_Object res0, res1;

  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bignum_ceil (scratch_bignum, XBIGNUM_DATA (number), XBIGNUM_DATA (divisor));

  res0 = return_float ? make_float (bignum_to_double (scratch_bignum)) :
	  Fcanonicalize_number (make_bignum_bg (scratch_bignum));

  if (bignum_divisible_p (XBIGNUM_DATA (number), XBIGNUM_DATA (divisor)))
    {
      res1 = Qzero;
    }
  else
    {
      bignum_mul (scratch_bignum, scratch_bignum, XBIGNUM_DATA (divisor));
      bignum_sub (scratch_bignum, XBIGNUM_DATA (number), scratch_bignum);
      res1 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
static Lisp_Object
ceiling_two_ratio (Lisp_Object number, Lisp_Object divisor,
		   int return_float)
{
  Lisp_Object res0, res1;

  if (ratio_sign (XRATIO_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  ratio_div (scratch_ratio, XRATIO_DATA (number), XRATIO_DATA (divisor));

  bignum_ceil (scratch_bignum, ratio_numerator (scratch_ratio),
	       ratio_denominator (scratch_ratio));

  res0 = return_float ? make_float (bignum_to_double (scratch_bignum)) :
	  Fcanonicalize_number (make_bignum_bg (scratch_bignum));

  if (bignum_divisible_p (ratio_numerator (scratch_ratio),
			  ratio_denominator (scratch_ratio)))
    {
      res1 = Qzero;
    }
  else
    {
      ratio_set_bignum (scratch_ratio, scratch_bignum);
      ratio_mul (scratch_ratio2, scratch_ratio, XRATIO_DATA (divisor));
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio2);
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
ceiling_two_bigfloat (Lisp_Object number, Lisp_Object divisor,
		      int return_float)
{
  Lisp_Object res0;

  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bigfloat_set_prec (scratch_bigfloat, max (XBIGFLOAT_GET_PREC (number),
					    XBIGFLOAT_GET_PREC (divisor)));
  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (number),
		XBIGFLOAT_DATA (divisor));
  bigfloat_ceil (scratch_bigfloat, scratch_bigfloat);

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }

  bigfloat_mul (scratch_bigfloat, scratch_bigfloat, XBIGFLOAT_DATA (divisor));
  bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (number), scratch_bigfloat);
  return values2 (res0,
		  Fcanonicalize_number (make_bigfloat_bf (scratch_bigfloat)));
}
#endif /* HAVE_BIGFLOAT */

#ifdef HAVE_RATIO
static Lisp_Object
ceiling_one_ratio (Lisp_Object number, Lisp_Object UNUSED (divisor),
		   int return_float)
{
  Lisp_Object res0, res1;

  bignum_ceil (scratch_bignum, XRATIO_NUMERATOR (number),
	       XRATIO_DENOMINATOR (number));

  res0 = return_float ? make_float (bignum_to_double (scratch_bignum)) :
	  Fcanonicalize_number (make_bignum_bg (scratch_bignum));

  if (bignum_divisible_p (XRATIO_NUMERATOR (number),
			  XRATIO_DENOMINATOR (number)))
    {
      res1 = Qzero;
    }
  else
    {
      ratio_set_bignum (scratch_ratio2, scratch_bignum);
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio2);
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
ceiling_one_bigfloat (Lisp_Object number, Lisp_Object UNUSED (divisor),
		      int return_float)
{
  Lisp_Object res0, res1;

  bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
  bigfloat_ceil (scratch_bigfloat, XBIGFLOAT_DATA (number));

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }

  bigfloat_sub (scratch_bigfloat2, XBIGFLOAT_DATA (number), scratch_bigfloat);

  res1 = make_bigfloat_bf (scratch_bigfloat2);
  return values2 (res0, res1);
}
#endif /* HAVE_BIGFLOAT */

static Lisp_Object
ceiling_two_float (Lisp_Object number, Lisp_Object divisor,
		   int return_float)
{
  double f1 = extract_float (number);
  double f2 = extract_float (divisor);
  double f0, remain;
  Lisp_Object res0;
	    
  if (f2 == 0.0)
    {
      Fsignal (Qarith_error, Qnil);
    }
	    
  IN_FLOAT2 (f0 = ceil (f1 / f2), MAYBE_EFF("ceiling"), number, divisor);
  IN_FLOAT2 (remain = f1 - (f0 * f2), MAYBE_EFF("ceiling"), number, divisor);

  if (return_float)
    {
      res0 = make_float(f0);
    }
  else
    {
      res0 = float_to_int (f0, MAYBE_EFF("ceiling"), number, divisor);
    }

  return values2 (res0, make_float (remain));
}

static Lisp_Object
ceiling_one_float (Lisp_Object number, int return_float)
{
  double d, remain;
  Lisp_Object res0;

  IN_FLOAT ((d = ceil (XFLOAT_DATA (number))), MAYBE_EFF("ceiling"), number);
  IN_FLOAT ((remain = XFLOAT_DATA (number) - d), MAYBE_EFF("ceiling"), number);

  if (return_float)
    {
      res0 = make_float (d);
    }
  else
    {
      res0 = float_to_int (d, MAYBE_EFF("ceiling"), number, Qunbound);
    }
  return values2 (res0, make_float (remain));
}

EXFUN (Fceiling, 2);
EXFUN (Ffceiling, 2);

static Lisp_Object
ceiling_one_mundane_arg (Lisp_Object number, Lisp_Object divisor,
			 int return_float)
{

  if (return_float)
    {
      if (INTP (number))
	{
	  return values2 (make_float ((double) XINT (number)), Qzero);
	}
#ifdef HAVE_BIGNUM
      else if (BIGNUMP (number))
	{
	  return values2 (make_float 
			  (bignum_to_double (XBIGNUM_DATA (number))),
			  Qzero);
	}
#endif
    }
  else
    {
#ifdef HAVE_BIGNUM
      if (INTEGERP (number))
#else
      if (INTP (number))
#endif
	{
	  return values2 (number, Qzero);
	}
    }
  
  MAYBE_CHAR_OR_MARKER (ceiling);

  return Ffceiling (wrong_type_argument (Qnumberp, number), divisor);
}

static Lisp_Object
floor_two_fixnum (Lisp_Object number, Lisp_Object divisor,
		  int return_float)
{
  EMACS_INT i1 = XREALINT (number);
  EMACS_INT i2 = XREALINT (divisor);
  EMACS_INT i3 = 0, i4 = 0;
  Lisp_Object res0;

  if (i2 == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  /* With C89's integer /, the result is implementation-defined if either
     operand is negative, so use only nonnegative operands. Notice also that
     we're forcing the quotient of any negative numbers towards minus
     infinity. */
  i3 = (i2 < 0
	? (i1 <= 0  ?  -i1 / -i2  :  -1 - ((i1 - 1) / -i2))
	: (i1 < 0  ?  -1 - ((-1 - i1) / i2)  :  i1 / i2));

  i4 = i1 - (i3 * i2);

  if (return_float)
    {
      res0 = make_float ((double)i3);
    }
  else
    {
      res0 = make_int (i3);
    }

  return values2 (res0, make_int (i4));
}

#ifdef HAVE_BIGNUM
static Lisp_Object
floor_two_bignum (Lisp_Object number, Lisp_Object divisor,
		  int return_float)
{
  Lisp_Object res0, res1;

  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bignum_floor (scratch_bignum, XBIGNUM_DATA (number),
		XBIGNUM_DATA (divisor));

  if (return_float)
    {
      res0 = make_float (bignum_to_double (scratch_bignum));
    }
  else
    {
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }

  if (bignum_divisible_p (XBIGNUM_DATA (number), XBIGNUM_DATA (divisor)))
    {
      res1 = Qzero;
    }
  else
    {
      bignum_mul (scratch_bignum, scratch_bignum, XBIGNUM_DATA (divisor));
      bignum_sub (scratch_bignum, XBIGNUM_DATA (number), scratch_bignum);
      res1 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
static Lisp_Object
floor_two_ratio (Lisp_Object number, Lisp_Object divisor,
		 int return_float)
{
  Lisp_Object res0, res1;

  if (ratio_sign (XRATIO_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  ratio_div (scratch_ratio, XRATIO_DATA (number), XRATIO_DATA (divisor));

  bignum_floor (scratch_bignum, ratio_numerator (scratch_ratio),
		ratio_denominator (scratch_ratio));

  res0 = return_float ? make_float (bignum_to_double (scratch_bignum)) :
	  Fcanonicalize_number (make_bignum_bg (scratch_bignum));

  if (bignum_divisible_p (ratio_numerator (scratch_ratio),
			  ratio_denominator (scratch_ratio)))
    {
      res1 = Qzero;
    }
  else
    {
      ratio_set_bignum (scratch_ratio, scratch_bignum);
      ratio_mul (scratch_ratio, scratch_ratio, XRATIO_DATA (divisor));
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio);
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
floor_two_bigfloat (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  Lisp_Object res0;

  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bigfloat_set_prec (scratch_bigfloat, max (XBIGFLOAT_GET_PREC (number),
					    XBIGFLOAT_GET_PREC (divisor)));
  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (number),
		XBIGFLOAT_DATA (divisor));
  bigfloat_floor (scratch_bigfloat, scratch_bigfloat);

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }

  bigfloat_mul (scratch_bigfloat2, scratch_bigfloat,
		XBIGFLOAT_DATA (divisor));
  bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (number), scratch_bigfloat2);

  return values2 (res0, make_bigfloat_bf (scratch_bigfloat));
}
#endif /* HAVE_BIGFLOAT */

#ifdef HAVE_RATIO
static Lisp_Object
floor_one_ratio (Lisp_Object number, Lisp_Object UNUSED (divisor),
		 int return_float)
{
  Lisp_Object res0, res1;

  bignum_floor (scratch_bignum, XRATIO_NUMERATOR (number),
		XRATIO_DENOMINATOR (number));

  res0 = return_float ? make_float (bignum_to_double (scratch_bignum)) :
	  Fcanonicalize_number (make_bignum_bg (scratch_bignum));

  if (bignum_divisible_p (XRATIO_NUMERATOR (number),
			  XRATIO_DENOMINATOR (number)))
    {
      res1 = Qzero;
    }
  else
    {
      ratio_set_bignum (scratch_ratio2, scratch_bignum);
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio2);
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
floor_one_bigfloat (Lisp_Object number, Lisp_Object UNUSED (divisor),
		    int return_float)
{
  Lisp_Object res0;

  bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
  bigfloat_floor (scratch_bigfloat, XBIGFLOAT_DATA (number));

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }

  bigfloat_sub (scratch_bigfloat2, XBIGFLOAT_DATA (number), scratch_bigfloat);
  return values2 (res0, make_bigfloat_bf (scratch_bigfloat2));
}
#endif /* HAVE_BIGFLOAT */

static Lisp_Object
floor_two_float (Lisp_Object number, Lisp_Object divisor,
		 int return_float)
{
  double f1 = extract_float (number);
  double f2 = extract_float (divisor);
  double f0, remain;
	    
  if (f2 == 0.0)
    {
      Fsignal (Qarith_error, Qnil);
    }
	    
  IN_FLOAT2 (f0 = floor (f1 / f2), MAYBE_EFF ("floor"), number, divisor);
  IN_FLOAT2 (remain = f1 - (f0 * f2), MAYBE_EFF ("floor"), number, divisor);

  if (return_float)
    {
      return values2 (make_float (f0), make_float (remain));
    }

  return values2 (float_to_int (f0, MAYBE_EFF ("floor"), number, divisor),
		  make_float (remain));
}

static Lisp_Object
floor_one_float (Lisp_Object number, int return_float)
{
  double d, d1;

  IN_FLOAT ((d = floor (XFLOAT_DATA (number))), MAYBE_EFF ("floor"), number);
  IN_FLOAT ((d1 = XFLOAT_DATA (number) - d), MAYBE_EFF ("floor"), number);

  if (return_float)
    {
      return values2 (make_float (d), make_float (d1));
    }
  else
    {
      return values2 (float_to_int (d, MAYBE_EFF ("floor"), number, Qunbound),
                      make_float (d1));
    }
}

EXFUN (Ffloor, 2);
EXFUN (Fffloor, 2);

static Lisp_Object
floor_one_mundane_arg (Lisp_Object number, Lisp_Object divisor,
		       int return_float)
{
#ifdef HAVE_BIGNUM
  if (INTEGERP (number))
#else
  if (INTP (number))
#endif
    {
      if (return_float)
	{
	  return values2 (make_float (extract_float (number)), Qzero);
	}
      else
	{
	  return values2 (number, Qzero);
	}
    }

  MAYBE_CHAR_OR_MARKER (floor);

  if (return_float)
    {
      return Fffloor (wrong_type_argument (Qnumberp, number), divisor);
    }
  else
    {
      return Ffloor (wrong_type_argument (Qnumberp, number), divisor);
    }
}

/* Algorithm taken from cl-extra.el, now to be found as cl-round in
   tests/automated/lisp-tests.el.  */
static Lisp_Object
round_two_fixnum (Lisp_Object number, Lisp_Object divisor, 
		  int return_float)
{
  EMACS_INT i1 = XREALINT (number);
  EMACS_INT i2 = XREALINT (divisor);
  EMACS_INT i0, hi2, flooring, floored, flsecond;

  if (i2 == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  hi2 = i2 < 0 ? -( -i2 / 2) : i2 / 2;

  flooring = hi2 + i1;

  floored = (i2 < 0
	? (flooring <= 0  ?  -flooring / -i2  :  -1 - ((flooring - 1) / -i2))
	: (flooring < 0  ?  -1 - ((-1 - flooring) / i2)  :  flooring / i2));

  flsecond = flooring - (floored * i2);

  if (0 == flsecond
      && (i2 == (hi2 + hi2))
      && (0 != (floored % 2)))
    {
      i0 = floored - 1;
      return values2 (return_float ? make_float ((double)i0) :
		      make_int (i0), make_int (hi2));
    }
  else
    {
      return values2 (return_float ? make_float ((double)floored) :
		      make_int (floored),
		      make_int (flsecond - hi2));
    }
}

#ifdef HAVE_BIGNUM
static void
round_two_bignum_1 (bignum number, bignum divisor,
		    Lisp_Object *res, Lisp_Object *remain)
{
  bignum flooring, floored, hi2, flsecond;

  if (bignum_divisible_p (number, divisor))
    {
      bignum_div (scratch_bignum, number, divisor);
      *res = make_bignum_bg (scratch_bignum);
      *remain = Qzero;
      return;
    }

  bignum_set_long (scratch_bignum, 2);

  bignum_div (scratch_bignum2, divisor, scratch_bignum);

  bignum_init (hi2);
  bignum_set (hi2, scratch_bignum2);

  bignum_add (scratch_bignum, scratch_bignum2, number);
  bignum_init (flooring);
  bignum_set (flooring, scratch_bignum);

  bignum_floor (scratch_bignum, flooring, divisor);
  bignum_init (floored);
  bignum_set (floored, scratch_bignum);

  bignum_mul (scratch_bignum2, scratch_bignum, divisor);
  bignum_sub (scratch_bignum, flooring, scratch_bignum2);
  bignum_init (flsecond);
  bignum_set (flsecond, scratch_bignum);

  bignum_set_long (scratch_bignum, 2);
  bignum_mul (scratch_bignum2, scratch_bignum, hi2);

  if (bignum_sign (flsecond) == 0
      && bignum_eql (divisor, scratch_bignum2)
      && (1 == bignum_testbit (floored, 0)))
    {
      bignum_set_long (scratch_bignum, 1);
      bignum_sub (floored, floored, scratch_bignum);
      *res = make_bignum_bg (floored);
      *remain = make_bignum_bg (hi2);
    }
  else
    {
      bignum_sub (scratch_bignum, flsecond,
		  hi2);
      *res = make_bignum_bg (floored);
      *remain = make_bignum_bg (scratch_bignum);
    }
}

static Lisp_Object
round_two_bignum (Lisp_Object number, Lisp_Object divisor, 
		  int return_float)
{
  Lisp_Object res0, res1;

  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  round_two_bignum_1 (XBIGNUM_DATA (number), XBIGNUM_DATA (divisor),
		      &res0, &res1);

  if (return_float)
    {
      res0 = make_float (bignum_to_double (XBIGNUM_DATA (res0)));
    }
  else
    {
      res0 = Fcanonicalize_number (res0);
    }

  return values2 (res0, Fcanonicalize_number (res1));
}
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
static Lisp_Object
round_two_ratio (Lisp_Object number, Lisp_Object divisor,
		 int return_float)
{
  Lisp_Object res0, res1;

  if (ratio_sign (XRATIO_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  ratio_div (scratch_ratio, XRATIO_DATA (number), XRATIO_DATA (divisor));
  
  round_two_bignum_1 (ratio_numerator (scratch_ratio),
		      ratio_denominator (scratch_ratio), &res0, &res1);

  if (!ZEROP (res1))
    {
      /* The numerator and denominator don't round exactly, calculate a
	 ratio remainder: */
      ratio_set_bignum (scratch_ratio2, XBIGNUM_DATA (res0));
      ratio_mul (scratch_ratio, scratch_ratio2, XRATIO_DATA (divisor));
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio);
      
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  res0 = return_float ?
    make_float ((double)bignum_to_double(XBIGNUM_DATA (res0))) :
    Fcanonicalize_number (res0);

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
/* This is the logic of emacs_rint above, no more and no less. */
static Lisp_Object
round_one_bigfloat_1 (bigfloat number)
{
  Lisp_Object res0;
  unsigned long prec = bigfloat_get_prec (number);

  assert ((bigfloat *)(&number) != (bigfloat *)&scratch_bigfloat
	  && (bigfloat *)(&number) != (bigfloat *)(&scratch_bigfloat2));

  bigfloat_set_prec (scratch_bigfloat, prec);
  bigfloat_set_prec (scratch_bigfloat2, prec);

  bigfloat_set_double (scratch_bigfloat, 0.5);
  bigfloat_add (scratch_bigfloat2, scratch_bigfloat, number);
  bigfloat_floor (scratch_bigfloat, scratch_bigfloat2);
  res0 = make_bigfloat_bf (scratch_bigfloat);

  bigfloat_sub (scratch_bigfloat2, scratch_bigfloat, number);
  bigfloat_abs (scratch_bigfloat, scratch_bigfloat2);

  bigfloat_set_double (scratch_bigfloat2, 0.5);

  do {
    if (!bigfloat_ge (scratch_bigfloat, scratch_bigfloat2))
      {
	break;
      }

    if (!bigfloat_gt (scratch_bigfloat, scratch_bigfloat2))
      {
	bigfloat_set_double (scratch_bigfloat2, 2.0);
	bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (res0),
		      scratch_bigfloat2);
	bigfloat_floor (scratch_bigfloat2, scratch_bigfloat);
	bigfloat_set_double (scratch_bigfloat, 2.0);
	bigfloat_mul (scratch_bigfloat2, scratch_bigfloat2,
		      scratch_bigfloat);
	if (bigfloat_eql (scratch_bigfloat2, XBIGFLOAT_DATA (res0)))
	  {
	    break;
	  }
      }

    if (bigfloat_lt (XBIGFLOAT_DATA (res0), number))
      {
	bigfloat_set_double (scratch_bigfloat2, 1.0);
      }
    else
      {
	bigfloat_set_double (scratch_bigfloat2, -1.0);
      }

    bigfloat_set (scratch_bigfloat, XBIGFLOAT_DATA (res0));

    bigfloat_add (XBIGFLOAT_DATA (res0), scratch_bigfloat2,
		  scratch_bigfloat);

  } while (0);

  return res0;
}

static Lisp_Object
round_two_bigfloat (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  Lisp_Object res0, res1;
  bigfloat divided;

  unsigned long prec = max (XBIGFLOAT_GET_PREC (number),
			    XBIGFLOAT_GET_PREC (divisor));

  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bigfloat_init (divided);
  bigfloat_set_prec (divided, prec);

  bigfloat_div (divided, XBIGFLOAT_DATA (number), XBIGFLOAT_DATA (divisor));

  res0 = round_one_bigfloat_1 (divided);

  bigfloat_set_prec (scratch_bigfloat, prec);
  bigfloat_set_prec (scratch_bigfloat2, prec);
            
  bigfloat_mul (scratch_bigfloat, XBIGFLOAT_DATA (res0),
		XBIGFLOAT_DATA (divisor));
  bigfloat_sub (scratch_bigfloat2, XBIGFLOAT_DATA (number),
		scratch_bigfloat);

  res1 = make_bigfloat_bf (scratch_bigfloat2);

  if (!return_float)
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, XBIGFLOAT_DATA (res0));
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (XBIGFLOAT_DATA (res0)));
#endif /* HAVE_BIGNUM */
    }

  return values2 (res0, res1);
}
#endif /* HAVE_BIGFLOAT */

#ifdef HAVE_RATIO
static Lisp_Object
round_one_ratio (Lisp_Object number, Lisp_Object UNUSED (divisor),
		 int return_float)
{
  Lisp_Object res0, res1;

  round_two_bignum_1 (XRATIO_NUMERATOR (number), XRATIO_DENOMINATOR (number),
		      &res0, &res1);

  if (!ZEROP (res1))
    {
      ratio_set_bignum (scratch_ratio2, XBIGNUM_DATA (res0));
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio2);
      res1 = Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }

  res0 = return_float ?
    make_float ((double)bignum_to_double(XBIGNUM_DATA (res0))) :
    Fcanonicalize_number (res0);

  return values2 (res0, res1);
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
round_one_bigfloat (Lisp_Object number, Lisp_Object UNUSED (divisor),
		    int return_float)
{
  Lisp_Object res0 = round_one_bigfloat_1 (XBIGFLOAT_DATA (number));
  Lisp_Object res1;

  bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (number), 
		XBIGFLOAT_DATA (res0));

  res1 = make_bigfloat_bf (scratch_bigfloat);

  if (!return_float)
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, XBIGFLOAT_DATA (res0));
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long
		       (XBIGFLOAT_DATA (res0)));
#endif /* HAVE_BIGNUM */
    }

  return values2 (res0, res1);
}
#endif /* HAVE_BIGFLOAT */

static Lisp_Object
round_two_float (Lisp_Object number, Lisp_Object divisor,
		 int return_float)
{
  double f1 = extract_float (number);
  double f2 = extract_float (divisor);
  double f0, remain;
	    
  if (f2 == 0.0)
    Fsignal (Qarith_error, Qnil);

  IN_FLOAT2 ((f0 = emacs_rint (f1 / f2)), MAYBE_EFF ("round"), number,
	     divisor); 
  IN_FLOAT2 (remain = f1 - (f0 * f2), MAYBE_EFF ("round"), number, divisor);

  if (return_float)
    {
      return values2 (make_float (f0), make_float (remain));
    }
  else
    {
      return values2 (float_to_int (f0, MAYBE_EFF("round"), number, divisor),
		      make_float (remain));
    }
}

static Lisp_Object
round_one_float (Lisp_Object number, int return_float)
{
  double d;
  /* Screw the prevailing rounding mode.  */
  IN_FLOAT ((d = emacs_rint (XFLOAT_DATA (number))), MAYBE_EFF ("round"),
    	number);

  if (return_float)
    {
      return values2 (make_float (d), make_float (XFLOAT_DATA (number) - d));
    }
  else
    {
      return values2 ((float_to_int (d, MAYBE_EFF ("round"), number,
				     Qunbound)), 
		      make_float (XFLOAT_DATA (number) - d));
    }
}

EXFUN (Fround, 2);
EXFUN (Ffround, 2);

static Lisp_Object
round_one_mundane_arg (Lisp_Object number, Lisp_Object divisor,
		       int return_float)
{
#ifdef HAVE_BIGNUM
  if (INTEGERP (number))
#else
  if (INTP (number))
#endif
    {
      if (return_float)
	{
	  return values2 (make_float (extract_float (number)), Qzero);
	}
      else
	{
	  return values2 (number, Qzero);
	}
    }

  MAYBE_CHAR_OR_MARKER (round);

  if (return_float)
    {
      return Ffround (wrong_type_argument (Qnumberp, number), divisor);  
    }
  else
    {
      return Fround (wrong_type_argument (Qnumberp, number), divisor);  
    }
}

static Lisp_Object
truncate_two_fixnum (Lisp_Object number, Lisp_Object divisor,
		     int return_float)
{
  EMACS_INT i1 = XREALINT (number);
  EMACS_INT i2 = XREALINT (divisor);
  EMACS_INT i0;

  if (i2 == 0)
    Fsignal (Qarith_error, Qnil);

  /* We're truncating towards zero, so apart from avoiding the C89
     implementation-defined behaviour with truncation and negative numbers,
     we don't need to do anything further: */
  i0 = (i2 < 0
	? (i1 <= 0  ?  -i1 / -i2  :  -(i1 / -i2))
	: (i1 < 0  ?  -(-i1 / i2)  :  i1 / i2));

  if (return_float)
    {
      return values2 (make_float ((double)i0), make_int (i1 - (i0 * i2)));
    }
  else
    {
      return values2 (make_int (i0), make_int (i1 - (i0 * i2)));
    }
}

#ifdef HAVE_BIGNUM
static Lisp_Object
truncate_two_bignum (Lisp_Object number, Lisp_Object divisor,
		     int return_float)
{
  Lisp_Object res0;

  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bignum_div (scratch_bignum, XBIGNUM_DATA (number),
	      XBIGNUM_DATA (divisor));

  if (return_float)
    {
      res0 = make_float (bignum_to_double (scratch_bignum));
    }
  else
    {
      res0 = make_bignum_bg (scratch_bignum);
    }

  if (bignum_divisible_p (XBIGNUM_DATA (number),
			  XBIGNUM_DATA (divisor)))
    {
      return values2 (Fcanonicalize_number (res0), Qzero);
    }

  bignum_mul (scratch_bignum2, scratch_bignum, XBIGNUM_DATA (divisor));
  bignum_sub (scratch_bignum, XBIGNUM_DATA (number), scratch_bignum2);

  return values2 (Fcanonicalize_number (res0),
		  Fcanonicalize_number (make_bignum_bg (scratch_bignum)));
}
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
static Lisp_Object
truncate_two_ratio (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  Lisp_Object res0;

  if (ratio_sign (XRATIO_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  ratio_div (scratch_ratio, XRATIO_DATA (number), XRATIO_DATA (divisor));

  bignum_div (scratch_bignum, ratio_numerator (scratch_ratio),
	      ratio_denominator (scratch_ratio));

  if (return_float)
    {
      res0 = make_float (bignum_to_double (scratch_bignum));
    }
  else
    {
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }

  if (bignum_divisible_p (ratio_numerator (scratch_ratio),
			  ratio_denominator (scratch_ratio)))
    {
      return values2 (res0, Qzero);
    }

  ratio_set_bignum (scratch_ratio2, scratch_bignum);
  ratio_mul (scratch_ratio, scratch_ratio2, XRATIO_DATA (divisor));
  ratio_sub (scratch_ratio2, XRATIO_DATA (number), scratch_ratio);

  return values2 (res0, Fcanonicalize_number (make_ratio_rt(scratch_ratio2)));
}
#endif

#ifdef HAVE_BIGFLOAT
static Lisp_Object
truncate_two_bigfloat (Lisp_Object number, Lisp_Object divisor,
		       int return_float)
{
  Lisp_Object res0;
  unsigned long prec = max (XBIGFLOAT_GET_PREC (number),
			    XBIGFLOAT_GET_PREC (divisor));

  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bigfloat_set_prec (scratch_bigfloat, prec);
  bigfloat_set_prec (scratch_bigfloat2, prec);

  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (number),
		XBIGFLOAT_DATA (divisor));
  bigfloat_trunc (scratch_bigfloat, scratch_bigfloat);

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }
            
  bigfloat_mul (scratch_bigfloat2, scratch_bigfloat, XBIGFLOAT_DATA (divisor));
  bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (number), scratch_bigfloat2);

  return values2 (res0, make_bigfloat_bf (scratch_bigfloat));
}
#endif /* HAVE_BIGFLOAT */

#ifdef HAVE_RATIO
static Lisp_Object
truncate_one_ratio (Lisp_Object number, Lisp_Object UNUSED (divisor),
		   int return_float)
{
  Lisp_Object res0;

  if (ratio_sign (XRATIO_DATA (number)) == 0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  bignum_div (scratch_bignum, XRATIO_NUMERATOR (number),
	      XRATIO_DENOMINATOR (number));
  if (return_float)
    {
      res0 = make_float (bignum_to_double (scratch_bignum));
    }
  else
    {
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }

  if (bignum_divisible_p (XRATIO_NUMERATOR (number),
			  XRATIO_DENOMINATOR (number)))
    {
      return values2 (res0, Qzero);
    }

  ratio_set_bignum (scratch_ratio2, scratch_bignum);
  ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio2);

  return values2 (res0, Fcanonicalize_number (make_ratio_rt (scratch_ratio)));
}
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
static Lisp_Object
truncate_one_bigfloat (Lisp_Object number, Lisp_Object UNUSED (divisor),
		       int return_float)
{
  Lisp_Object res0;

  bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
  bigfloat_set_prec (scratch_bigfloat2, XBIGFLOAT_GET_PREC (number));
  bigfloat_trunc (scratch_bigfloat, XBIGFLOAT_DATA (number));

  if (return_float)
    {
      res0 = make_bigfloat_bf (scratch_bigfloat);
    }
  else
    {
#ifdef HAVE_BIGNUM
      bignum_set_bigfloat (scratch_bignum, scratch_bigfloat);
      res0 = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
      res0 = make_int ((EMACS_INT) bigfloat_to_long (scratch_bigfloat));
#endif /* HAVE_BIGNUM */
    }

  bigfloat_sub (scratch_bigfloat2, XBIGFLOAT_DATA (number), scratch_bigfloat);

  return
    values2 (res0, 
	     Fcanonicalize_number (make_bigfloat_bf (scratch_bigfloat2)));
}
#endif /* HAVE_BIGFLOAT */

static Lisp_Object
truncate_two_float (Lisp_Object number, Lisp_Object divisor,
		    int return_float)
{
  double f1 = extract_float (number);
  double f2 = extract_float (divisor);
  double f0, remain;
  Lisp_Object res0;
	    
  if (f2 == 0.0)
    {
      Fsignal (Qarith_error, Qnil);
    }

  res0 = float_to_int (f1 / f2, MAYBE_EFF ("truncate"), number, Qunbound);
  f0 = extract_float (res0);

  IN_FLOAT2 (remain = f1 - (f0 * f2), MAYBE_EFF ("truncate"), number, divisor);

  if (return_float)
    {
      res0 = make_float (f0);
    }

  return values2 (res0, make_float (remain));
}

static Lisp_Object
truncate_one_float (Lisp_Object number, int return_float)
{
  Lisp_Object res0
    = float_to_int (XFLOAT_DATA (number), MAYBE_EFF ("truncate"),
		    number, Qunbound);
  if (return_float)
    {
      res0 = make_float ((double)XINT(res0));
      return values2 (res0, make_float ((XFLOAT_DATA (number)
					 - XFLOAT_DATA (res0))));
    }
  else
    {
      return values2 (res0, make_float (XFLOAT_DATA (number)
					- XREALINT (res0)));
    }
}

EXFUN (Fftruncate, 2);

static Lisp_Object
truncate_one_mundane_arg (Lisp_Object number, Lisp_Object divisor,
			  int return_float)
{
#ifdef HAVE_BIGNUM
  if (INTEGERP (number))
#else
  if (INTP (number))
#endif
    {
      if (return_float)
	{
	  return values2 (make_float (extract_float (number)), Qzero);
	}
      else
	{
	  return values2 (number, Qzero);
	}
    }

  MAYBE_CHAR_OR_MARKER (truncate);

  if (return_float)
    {
      return Fftruncate (wrong_type_argument (Qnumberp, number), divisor);
    }
  else
    {
      return Ftruncate (wrong_type_argument (Qnumberp, number), divisor);
    }
}

/* Rounding functions that will not necessarily return floats: */

DEFUN ("ceiling", Fceiling, 1, 2, 0, /*
Return the smallest integer no less than NUMBER.  (Round toward +inf.)

With optional argument DIVISOR, return the smallest integer no less than
the quotient of NUMBER and DIVISOR. 

This function returns multiple values; see `multiple-value-bind' and
`multiple-value-call'.  The second returned value is the remainder in the
calculation, which will be one minus the fractional part of NUMBER if DIVISOR
is omitted or one.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(ceiling, 0);
}

DEFUN ("floor", Ffloor, 1, 2, 0, /*
Return the largest integer no greater than NUMBER.  (Round towards -inf.)
With optional second argument DIVISOR, return the largest integer no
greater than the quotient of NUMBER and DIVISOR.

This function returns multiple values; see `multiple-value-call' and
`multiple-value-bind'.  The second returned value is the remainder in the
calculation, which will just be the fractional part if DIVISOR is omitted or
one.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(floor, 0);
}

DEFUN ("round", Fround, 1, 2, 0, /*
Return the nearest integer to NUMBER.
If NUMBER is exactly halfway between two integers, return the one that
is even.

Optional argument DIVISOR means return the nearest integer to NUMBER
divided by DIVISOR.

This function returns multiple values; see `multiple-value-call' and
`multiple-value-bind'.  The second returned value is the remainder
in the calculation.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(round, 0);
}

DEFUN ("truncate", Ftruncate, 1, 2, 0, /*
Truncate a floating point number to an integer.
Rounds the value toward zero.

Optional argument DIVISOR means truncate NUMBER divided by DIVISOR.

This function returns multiple values; see `multiple-value-call' and
`multiple-value-bind'.  The second returned value is the remainder.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(truncate, 0);
}

/* Float-rounding functions. */

DEFUN ("fceiling", Ffceiling, 1, 2, 0, /*
Return the smallest integer no less than NUMBER, as a float.
\(Round toward +inf.\)

With optional argument DIVISOR, return the smallest integer no less than the
quotient of NUMBER and DIVISOR, as a float.

This function returns multiple values; the second value is the remainder in
the calculation.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(ceiling, 1);
}

DEFUN ("ffloor", Fffloor, 1, 2, 0, /*
Return the largest integer no greater than NUMBER, as a float.
\(Round towards -inf.\)

With optional argument DIVISOR, return the largest integer no greater than
the quotient of NUMBER and DIVISOR, as a float.

This function returns multiple values; the second value is the remainder in
the calculation.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(floor, 1);
}

DEFUN ("fround", Ffround, 1, 2, 0, /*
Return the nearest integer to NUMBER, as a float.
If NUMBER is exactly halfway between two integers, return the one that is
even.

With optional argument DIVISOR, return the nearest integer to the quotient
of NUMBER and DIVISOR, as a float.

This function returns multiple values; the second value is the remainder in
the calculation.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(round, 1);
}

DEFUN ("ftruncate", Fftruncate, 1, 2, 0, /*
Truncate a floating point number to an integral float value.
Rounds the value toward zero.

With optional argument DIVISOR, truncate the quotient of NUMBER and DIVISOR,
to an integral float value.

This function returns multiple values; the second value is the remainder in
the calculation.
*/
       (number, divisor))
{
  ROUNDING_CONVERT(truncate, 1);
}

#ifdef FLOAT_CATCH_SIGILL
static SIGTYPE
float_error (int signo)
{
  if (! in_float)
    fatal_error_signal (signo);

  EMACS_REESTABLISH_SIGNAL (signo, arith_error);
  EMACS_UNBLOCK_SIGNAL (signo);

  in_float = 0;

  /* Was Fsignal(), but it just doesn't make sense for an error
     occurring inside a signal handler to be restartable, considering
     that anything could happen when the error is signaled and trapped
     and considering the asynchronous nature of signal handlers. */
  signal_error (Qarith_error, 0, float_error_arg);
}

/* Another idea was to replace the library function `infnan'
   where SIGILL is signaled.  */

#endif /* FLOAT_CATCH_SIGILL */

/* In C++, it is impossible to determine what type matherr expects
   without some more configure magic.
   We shouldn't be using matherr anyways - it's a non-standard SYSVism. */
#if defined (HAVE_MATHERR) && !defined(__cplusplus)
int
matherr (struct exception *x)
{
  Lisp_Object args;
  if (! in_float)
    /* Not called from emacs-lisp float routines; do the default thing. */
    return 0;

  /* if (!strcmp (x->name, "pow")) x->name = "expt"; */

  args = Fcons (build_string (x->name),
                Fcons (make_float (x->arg1),
                       ((in_float == 2)
                        ? Fcons (make_float (x->arg2), Qnil)
                        : Qnil)));
  switch (x->type)
    {
    case DOMAIN:    Fsignal (Qdomain_error,	 args); break;
    case SING:	    Fsignal (Qsingularity_error, args); break;
    case OVERFLOW:  Fsignal (Qoverflow_error,	 args); break;
    case UNDERFLOW: Fsignal (Qunderflow_error,	 args); break;
    default:	    Fsignal (Qarith_error,	 args); break;
    }
  return 1;	/* don't set errno or print a message */
}
#endif /* HAVE_MATHERR */

void
init_floatfns_very_early (void)
{
# ifdef FLOAT_CATCH_SIGILL
  EMACS_SIGNAL (SIGILL, float_error);
# endif
  in_float = 0;
}

void
syms_of_floatfns (void)
{
  INIT_LRECORD_IMPLEMENTATION (float);

  /* Trig functions.  */

  DEFSUBR (Facos);
  DEFSUBR (Fasin);
  DEFSUBR (Fatan);
  DEFSUBR (Fcos);
  DEFSUBR (Fsin);
  DEFSUBR (Ftan);

  /* Bessel functions */

#if 0
  DEFSUBR (Fbessel_y0);
  DEFSUBR (Fbessel_y1);
  DEFSUBR (Fbessel_yn);
  DEFSUBR (Fbessel_j0);
  DEFSUBR (Fbessel_j1);
  DEFSUBR (Fbessel_jn);
#endif /* 0 */

  /* Error functions. */

#if 0
  DEFSUBR (Ferf);
  DEFSUBR (Ferfc);
  DEFSUBR (Flog_gamma);
#endif /* 0 */

  /* Root and Log functions. */

  DEFSUBR (Fexp);
  DEFSUBR (Fexpt);
  DEFSUBR (Flog);
  DEFSUBR (Flog10);
  DEFSUBR (Fsqrt);
  DEFSUBR (Fcube_root);

  /* Inverse trig functions. */

  DEFSUBR (Facosh);
  DEFSUBR (Fasinh);
  DEFSUBR (Fatanh);
  DEFSUBR (Fcosh);
  DEFSUBR (Fsinh);
  DEFSUBR (Ftanh);

  /* Rounding functions */

  DEFSUBR (Fabs);
  DEFSUBR (Ffloat);
  DEFSUBR (Flogb);
  DEFSUBR (Fceiling);
  DEFSUBR (Ffloor);
  DEFSUBR (Fround);
  DEFSUBR (Ftruncate);

  /* Float-rounding functions. */

  DEFSUBR (Ffceiling);
  DEFSUBR (Fffloor);
  DEFSUBR (Ffround);
  DEFSUBR (Fftruncate);
}

void
vars_of_floatfns (void)
{
  Fprovide (intern ("lisp-float-type"));
}
