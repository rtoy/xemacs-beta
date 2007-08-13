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

#ifdef LISP_FLOAT_TYPE

/* Need to define a differentiating symbol -- see sysfloat.h */
#define THIS_FILENAME floatfns
#include "sysfloat.h"

#ifndef HAVE_RINT
static double
rint (double x)
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
static CONST char *float_error_fn_name;

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
  Fsignal (Qarith_error, list2 (build_string (op), arg))
#define range_error(op,arg) \
  Fsignal (Qrange_error, list2 (build_string (op), arg))
#define range_error2(op,a1,a2) \
  Fsignal (Qrange_error, list3 (build_string (op), a1, a2))
#define domain_error(op,arg) \
  Fsignal (Qdomain_error, list2 (build_string (op), arg))
#define domain_error2(op,a1,a2) \
  Fsignal (Qdomain_error, list3 (build_string (op), a1, a2))


/* Convert float to Lisp Integer if it fits, else signal a range
   error using the given arguments.  */
static Lisp_Object
float_to_int (double x, CONST char *name, Lisp_Object num, Lisp_Object num2)
{
  if (x >= ((EMACS_INT) 1 << (VALBITS-1))
      || x <= - ((EMACS_INT) 1 << (VALBITS-1)) - (EMACS_INT) 1)
  {
    if (!UNBOUNDP (num2))
      range_error2 (name, num, num2);
    else
      range_error (name, num);
  }
  return (make_int ((EMACS_INT) x));
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
mark_float (Lisp_Object obj)
{
  return Qnil;
}

static int
float_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (extract_float (obj1) == extract_float (obj2));
}

static unsigned long
float_hash (Lisp_Object obj, int depth)
{
  /* mod the value down to 32-bit range */
  /* #### change for 64-bit machines */
  return (unsigned long) fmod (extract_float (obj), 4e9);
}

static const struct lrecord_description float_description[] = {
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("float", float,
				     mark_float, print_float, 0, float_equal,
				     float_hash, float_description,
				     struct Lisp_Float);

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (Lisp_Object num)
{
  if (FLOATP (num))
    return XFLOAT_DATA (num);

  if (INTP (num))
    return (double) XINT (num);

  return extract_float (wrong_type_argument (Qnumberp, num));
}
#endif /* LISP_FLOAT_TYPE */


/* Trig functions.  */
#ifdef LISP_FLOAT_TYPE

DEFUN ("acos", Facos, 1, 1, 0, /*
Return the inverse cosine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("acos", arg);
#endif
  IN_FLOAT (d = acos (d), "acos", arg);
  return make_float (d);
}

DEFUN ("asin", Fasin, 1, 1, 0, /*
Return the inverse sine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("asin", arg);
#endif
  IN_FLOAT (d = asin (d), "asin", arg);
  return make_float (d);
}

DEFUN ("atan", Fatan, 1, 2, 0, /*
Return the inverse tangent of ARG.
*/
       (arg1, arg2))
{
  double d = extract_float (arg1);

  if (NILP (arg2))
    IN_FLOAT (d = atan (d), "atan", arg1);
  else
    {
      double d2 = extract_float (arg2);
#ifdef FLOAT_CHECK_DOMAIN
      if (d == 0.0 && d2 == 0.0)
	domain_error2 ("atan", arg1, arg2);
#endif
      IN_FLOAT2 (d = atan2 (d, d2), "atan", arg1, arg2);
    }
  return make_float (d);
}

DEFUN ("cos", Fcos, 1, 1, 0, /*
Return the cosine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = cos (d), "cos", arg);
  return make_float (d);
}

DEFUN ("sin", Fsin, 1, 1, 0, /*
Return the sine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = sin (d), "sin", arg);
  return make_float (d);
}

DEFUN ("tan", Ftan, 1, 1, 0, /*
Return the tangent of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  double c = cos (d);
#ifdef FLOAT_CHECK_DOMAIN
  if (c == 0.0)
    domain_error ("tan", arg);
#endif
  IN_FLOAT (d = (sin (d) / c), "tan", arg);
  return make_float (d);
}
#endif /* LISP_FLOAT_TYPE (trig functions) */


/* Bessel functions */
#if 0 /* Leave these out unless we find there's a reason for them.  */
/* #ifdef LISP_FLOAT_TYPE */

DEFUN ("bessel-j0", Fbessel_j0, 1, 1, 0, /*
Return the bessel function j0 of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = j0 (d), "bessel-j0", arg);
  return make_float (d);
}

DEFUN ("bessel-j1", Fbessel_j1, 1, 1, 0, /*
Return the bessel function j1 of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = j1 (d), "bessel-j1", arg);
  return make_float (d);
}

DEFUN ("bessel-jn", Fbessel_jn, 2, 2, 0, /*
Return the order N bessel function output jn of ARG.
The first arg (the order) is truncated to an integer.
*/
       (arg1, arg2))
{
  int i1 = extract_float (arg1);
  double f2 = extract_float (arg2);

  IN_FLOAT (f2 = jn (i1, f2), "bessel-jn", arg1);
  return make_float (f2);
}

DEFUN ("bessel-y0", Fbessel_y0, 1, 1, 0, /*
Return the bessel function y0 of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = y0 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-y1", Fbessel_y1, 1, 1, 0, /*
Return the bessel function y1 of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = y1 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-yn", Fbessel_yn, 2, 2, 0, /*
Return the order N bessel function output yn of ARG.
The first arg (the order) is truncated to an integer.
*/
       (arg1, arg2))
{
  int i1 = extract_float (arg1);
  double f2 = extract_float (arg2);

  IN_FLOAT (f2 = yn (i1, f2), "bessel-yn", arg1);
  return make_float (f2);
}

#endif /* 0 (bessel functions) */

/* Error functions. */
#if 0 /* Leave these out unless we see they are worth having.  */
/* #ifdef LISP_FLOAT_TYPE */

DEFUN ("erf", Ferf, 1, 1, 0, /*
Return the mathematical error function of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = erf (d), "erf", arg);
  return make_float (d);
}

DEFUN ("erfc", Ferfc, 1, 1, 0, /*
Return the complementary error function of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = erfc (d), "erfc", arg);
  return make_float (d);
}

DEFUN ("log-gamma", Flog_gamma, 1, 1, 0, /*
Return the log gamma of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = lgamma (d), "log-gamma", arg);
  return make_float (d);
}

#endif /* 0 (error functions) */


/* Root and Log functions. */

#ifdef LISP_FLOAT_TYPE
DEFUN ("exp", Fexp, 1, 1, 0, /*
Return the exponential base e of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 709.7827)   /* Assume IEEE doubles here */
    range_error ("exp", arg);
  else if (d < -709.0)
    return make_float (0.0);
  else
#endif
    IN_FLOAT (d = exp (d), "exp", arg);
  return make_float (d);
}
#endif /* LISP_FLOAT_TYPE */


DEFUN ("expt", Fexpt, 2, 2, 0, /*
Return the exponential ARG1 ** ARG2.
*/
       (arg1, arg2))
{
  if (INTP (arg1) && /* common lisp spec */
      INTP (arg2)) /* don't promote, if both are ints */
    {
      EMACS_INT retval;
      EMACS_INT x = XINT (arg1);
      EMACS_INT y = XINT (arg2);

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

#ifdef LISP_FLOAT_TYPE
  {
    double f1 = extract_float (arg1);
    double f2 = extract_float (arg2);
    /* Really should check for overflow, too */
    if (f1 == 0.0 && f2 == 0.0)
      f1 = 1.0;
# ifdef FLOAT_CHECK_DOMAIN
    else if ((f1 == 0.0 && f2 < 0.0) || (f1 < 0 && f2 != floor(f2)))
      domain_error2 ("expt", arg1, arg2);
# endif /* FLOAT_CHECK_DOMAIN */
    IN_FLOAT2 (f1 = pow (f1, f2), "expt", arg1, arg2);
    return make_float (f1);
  }
#else
  CHECK_INT_OR_FLOAT (arg1);
  CHECK_INT_OR_FLOAT (arg2);
  return Fexpt (arg1, arg2);
#endif /* LISP_FLOAT_TYPE */
}

#ifdef LISP_FLOAT_TYPE
DEFUN ("log", Flog, 1, 2, 0, /*
Return the natural logarithm of ARG.
If second optional argument BASE is given, return log ARG using that base.
*/
       (arg, base))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error2 ("log", arg, base);
#endif
  if (NILP (base))
    IN_FLOAT (d = log (d), "log", arg);
  else
    {
      double b = extract_float (base);
#ifdef FLOAT_CHECK_DOMAIN
      if (b <= 0.0 || b == 1.0)
	domain_error2 ("log", arg, base);
#endif
      if (b == 10.0)
	IN_FLOAT2 (d = log10 (d), "log", arg, base);
      else
	IN_FLOAT2 (d = (log (d) / log (b)), "log", arg, base);
    }
  return make_float (d);
}


DEFUN ("log10", Flog10, 1, 1, 0, /*
Return the logarithm base 10 of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error ("log10", arg);
#endif
  IN_FLOAT (d = log10 (d), "log10", arg);
  return make_float (d);
}


DEFUN ("sqrt", Fsqrt, 1, 1, 0, /*
Return the square root of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 0.0)
    domain_error ("sqrt", arg);
#endif
  IN_FLOAT (d = sqrt (d), "sqrt", arg);
  return make_float (d);
}


DEFUN ("cube-root", Fcube_root, 1, 1, 0, /*
Return the cube root of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef HAVE_CBRT
  IN_FLOAT (d = cbrt (d), "cube-root", arg);
#else
  if (d >= 0.0)
    IN_FLOAT (d = pow (d, 1.0/3.0), "cube-root", arg);
  else
    IN_FLOAT (d = -pow (-d, 1.0/3.0), "cube-root", arg);
#endif
  return make_float (d);
}
#endif /* LISP_FLOAT_TYPE */


/* Inverse trig functions. */
#ifdef LISP_FLOAT_TYPE
/* #if 0  Not clearly worth adding...  */

DEFUN ("acosh", Facosh, 1, 1, 0, /*
Return the inverse hyperbolic cosine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 1.0)
    domain_error ("acosh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = acosh (d), "acosh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d - 1.0)), "acosh", arg);
#endif
  return make_float (d);
}

DEFUN ("asinh", Fasinh, 1, 1, 0, /*
Return the inverse hyperbolic sine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = asinh (d), "asinh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d + 1.0)), "asinh", arg);
#endif
  return make_float (d);
}

DEFUN ("atanh", Fatanh, 1, 1, 0, /*
Return the inverse hyperbolic tangent of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d >= 1.0 || d <= -1.0)
    domain_error ("atanh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = atanh (d), "atanh", arg);
#else
  IN_FLOAT (d = 0.5 * log ((1.0 + d) / (1.0 - d)), "atanh", arg);
#endif
  return make_float (d);
}

DEFUN ("cosh", Fcosh, 1, 1, 0, /*
Return the hyperbolic cosine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("cosh", arg);
#endif
  IN_FLOAT (d = cosh (d), "cosh", arg);
  return make_float (d);
}

DEFUN ("sinh", Fsinh, 1, 1, 0, /*
Return the hyperbolic sine of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("sinh", arg);
#endif
  IN_FLOAT (d = sinh (d), "sinh", arg);
  return make_float (d);
}

DEFUN ("tanh", Ftanh, 1, 1, 0, /*
Return the hyperbolic tangent of ARG.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = tanh (d), "tanh", arg);
  return make_float (d);
}
#endif /* LISP_FLOAT_TYPE (inverse trig functions) */

/* Rounding functions */

DEFUN ("abs", Fabs, 1, 1, 0, /*
Return the absolute value of ARG.
*/
       (arg))
{
#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    {
      IN_FLOAT (arg = make_float (fabs (XFLOAT_DATA (arg))),
		"abs", arg);
      return arg;
    }
#endif /* LISP_FLOAT_TYPE */

  if (INTP (arg))
    return (XINT (arg) >= 0) ? arg : make_int (- XINT (arg));

  return Fabs (wrong_type_argument (Qnumberp, arg));
}

#ifdef LISP_FLOAT_TYPE
DEFUN ("float", Ffloat, 1, 1, 0, /*
Return the floating point number numerically equal to ARG.
*/
       (arg))
{
  if (INTP (arg))
    return make_float ((double) XINT (arg));

  if (FLOATP (arg))		/* give 'em the same float back */
    return arg;

  return Ffloat (wrong_type_argument (Qnumberp, arg));
}
#endif /* LISP_FLOAT_TYPE */


#ifdef LISP_FLOAT_TYPE
DEFUN ("logb", Flogb, 1, 1, 0, /*
Return largest integer <= the base 2 log of the magnitude of ARG.
This is the same as the exponent of a float.
*/
       (arg))
{
  double f = extract_float (arg);

  if (f == 0.0)
    return make_int (- (int)((((EMACS_UINT) 1) << (VALBITS - 1)))); /* most-negative-fixnum */
#ifdef HAVE_LOGB
  {
    Lisp_Object val;
    IN_FLOAT (val = make_int ((int) logb (f)), "logb", arg);
    return (val);
  }
#else
#ifdef HAVE_FREXP
  {
    int exqp;
    IN_FLOAT (frexp (f, &exqp), "logb", arg);
    return (make_int (exqp - 1));
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
    return (make_int (val));
  }
#endif /* ! HAVE_FREXP */
#endif /* ! HAVE_LOGB */
}
#endif /* LISP_FLOAT_TYPE */


DEFUN ("ceiling", Fceiling, 1, 1, 0, /*
Return the smallest integer no less than ARG.  (Round toward +inf.)
*/
       (arg))
{
#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    {
      double d;
      IN_FLOAT ((d = ceil (XFLOAT_DATA (arg))), "ceiling", arg);
      return (float_to_int (d, "ceiling", arg, Qunbound));
    }
#endif /* LISP_FLOAT_TYPE */

  if (INTP (arg))
    return arg;

  return Fceiling (wrong_type_argument (Qnumberp, arg));
}


DEFUN ("floor", Ffloor, 1, 2, 0, /*
Return the largest integer no greater than ARG.  (Round towards -inf.)
With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.
*/
       (arg, divisor))
{
  CHECK_INT_OR_FLOAT (arg);

  if (! NILP (divisor))
    {
      EMACS_INT i1, i2;

      CHECK_INT_OR_FLOAT (divisor);

#ifdef LISP_FLOAT_TYPE
      if (FLOATP (arg) || FLOATP (divisor))
	{
	  double f1 = extract_float (arg);
	  double f2 = extract_float (divisor);

	  if (f2 == 0)
	    Fsignal (Qarith_error, Qnil);

	  IN_FLOAT2 (f1 = floor (f1 / f2), "floor", arg, divisor);
	  return float_to_int (f1, "floor", arg, divisor);
	}
#endif /* LISP_FLOAT_TYPE */

      i1 = XINT (arg);
      i2 = XINT (divisor);

      if (i2 == 0)
	Fsignal (Qarith_error, Qnil);

      /* With C's /, the result is implementation-defined if either operand
	 is negative, so use only nonnegative operands.  */
      i1 = (i2 < 0
	    ? (i1 <= 0  ?  -i1 / -i2  :  -1 - ((i1 - 1) / -i2))
	    : (i1 < 0  ?  -1 - ((-1 - i1) / i2)  :  i1 / i2));

      return (make_int (i1));
    }

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    {
      double d;
      IN_FLOAT ((d = floor (XFLOAT_DATA (arg))), "floor", arg);
      return (float_to_int (d, "floor", arg, Qunbound));
    }
#endif /* LISP_FLOAT_TYPE */

  return arg;
}

DEFUN ("round", Fround, 1, 1, 0, /*
Return the nearest integer to ARG.
*/
       (arg))
{
#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    {
      double d;
      /* Screw the prevailing rounding mode.  */
      IN_FLOAT ((d = rint (XFLOAT_DATA (arg))), "round", arg);
      return (float_to_int (d, "round", arg, Qunbound));
    }
#endif /* LISP_FLOAT_TYPE */

  if (INTP (arg))
    return arg;

  return Fround (wrong_type_argument (Qnumberp, arg));
}

DEFUN ("truncate", Ftruncate, 1, 1, 0, /*
Truncate a floating point number to an integer.
Rounds the value toward zero.
*/
       (arg))
{
#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    return float_to_int (XFLOAT_DATA (arg), "truncate", arg, Qunbound);
#endif /* LISP_FLOAT_TYPE */

  if (INTP (arg))
    return arg;

  return Ftruncate (wrong_type_argument (Qnumberp, arg));
}

/* Float-rounding functions. */
#ifdef LISP_FLOAT_TYPE
/* #if 1  It's not clear these are worth adding... */

DEFUN ("fceiling", Ffceiling, 1, 1, 0, /*
Return the smallest integer no less than ARG, as a float.
\(Round toward +inf.\)
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = ceil (d), "fceiling", arg);
  return make_float (d);
}

DEFUN ("ffloor", Fffloor, 1, 1, 0, /*
Return the largest integer no greater than ARG, as a float.
\(Round towards -inf.\)
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = floor (d), "ffloor", arg);
  return make_float (d);
}

DEFUN ("fround", Ffround, 1, 1, 0, /*
Return the nearest integer to ARG, as a float.
*/
       (arg))
{
  double d = extract_float (arg);
  IN_FLOAT (d = rint (d), "fround", arg);
  return make_float (d);
}

DEFUN ("ftruncate", Fftruncate, 1, 1, 0, /*
Truncate a floating point number to an integral float value.
Rounds the value toward zero.
*/
       (arg))
{
  double d = extract_float (arg);
  if (d >= 0.0)
    IN_FLOAT (d = floor (d), "ftruncate", arg);
  else
    IN_FLOAT (d = ceil (d), "ftruncate", arg);
  return make_float (d);
}

#endif /* LISP_FLOAT_TYPE (float-rounding functions) */


#ifdef LISP_FLOAT_TYPE
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
  signal_error (Qarith_error, list1 (float_error_arg));
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
#endif /* LISP_FLOAT_TYPE */


void
init_floatfns_very_early (void)
{
#ifdef LISP_FLOAT_TYPE
# ifdef FLOAT_CATCH_SIGILL
  signal (SIGILL, float_error);
# endif
  in_float = 0;
#endif /* LISP_FLOAT_TYPE */
}

void
syms_of_floatfns (void)
{

  /* Trig functions.  */

#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Facos);
  DEFSUBR (Fasin);
  DEFSUBR (Fatan);
  DEFSUBR (Fcos);
  DEFSUBR (Fsin);
  DEFSUBR (Ftan);
#endif /* LISP_FLOAT_TYPE */

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

#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Fexp);
#endif /* LISP_FLOAT_TYPE */
  DEFSUBR (Fexpt);
#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Flog);
  DEFSUBR (Flog10);
  DEFSUBR (Fsqrt);
  DEFSUBR (Fcube_root);
#endif /* LISP_FLOAT_TYPE */

  /* Inverse trig functions. */

#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Facosh);
  DEFSUBR (Fasinh);
  DEFSUBR (Fatanh);
  DEFSUBR (Fcosh);
  DEFSUBR (Fsinh);
  DEFSUBR (Ftanh);
#endif /* LISP_FLOAT_TYPE */

  /* Rounding functions */

  DEFSUBR (Fabs);
#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Ffloat);
  DEFSUBR (Flogb);
#endif /* LISP_FLOAT_TYPE */
  DEFSUBR (Fceiling);
  DEFSUBR (Ffloor);
  DEFSUBR (Fround);
  DEFSUBR (Ftruncate);

  /* Float-rounding functions. */

#ifdef LISP_FLOAT_TYPE
  DEFSUBR (Ffceiling);
  DEFSUBR (Fffloor);
  DEFSUBR (Ffround);
  DEFSUBR (Fftruncate);
#endif /* LISP_FLOAT_TYPE */
}

void
vars_of_floatfns (void)
{
#ifdef LISP_FLOAT_TYPE
  Fprovide (intern ("lisp-float-type"));
#endif
}
