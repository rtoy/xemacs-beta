/* Primitive operations on Lisp data types for XEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002, 2003, 2005, 2010 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30.  Some of FSF's data.c is in
   XEmacs' symbols.c. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "gc.h"
#include "syssignal.h"
#include "sysfloat.h"
#include "syntax.h"
#include "casetab.h"

Lisp_Object Qnil, Qt, Qlambda, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message;
Lisp_Object Qerror, Qquit, Qsyntax_error, Qinvalid_read_syntax;
Lisp_Object Qlist_formation_error, Qstructure_formation_error;
Lisp_Object Qmalformed_list, Qmalformed_property_list;
Lisp_Object Qcircular_list, Qcircular_property_list;
Lisp_Object Qinvalid_argument, Qinvalid_constant, Qwrong_type_argument;
Lisp_Object Qargs_out_of_range;
Lisp_Object Qwrong_number_of_arguments, Qinvalid_function;
Lisp_Object Qinvalid_keyword_argument, Qno_catch;
Lisp_Object Qinternal_error, Qinvalid_state, Qstack_overflow, Qout_of_memory;
Lisp_Object Qvoid_variable, Qcyclic_variable_indirection;
Lisp_Object Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qinvalid_operation, Qinvalid_change, Qprinting_unreadable_object;
Lisp_Object Qsetting_constant;
Lisp_Object Qediting_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer;
Lisp_Object Qbuffer_read_only, Qextent_read_only;
Lisp_Object Qio_error, Qfile_error, Qconversion_error, Qend_of_file;
Lisp_Object Qtext_conversion_error;
Lisp_Object Qarith_error, Qrange_error, Qdomain_error;
Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
Lisp_Object Qintegerp, Qnatnump, Qnonnegativep, Qsymbolp;
Lisp_Object Qlistp, Qweak_listp;
Lisp_Object Qconsp, Qsubrp;
Lisp_Object Qcharacterp, Qstringp, Qarrayp, Qsequencep, Qvectorp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qbufferp;
Lisp_Object Qinteger_or_char_p, Qinteger_char_or_marker_p;
Lisp_Object Qnumberp, Qnumber_char_or_marker_p;
Lisp_Object Qbit_vectorp, Qbitp, Qcdr;

Lisp_Object Qerror_lacks_explanatory_string;
Lisp_Object Qfloatp;
Lisp_Object Q_junk_allowed,  Q_radix, Q_radix_table;

Lisp_Object Vdigit_fixnum_map, Vdigit_fixnum_ascii;
Lisp_Object Vfixnum_to_majuscule_map, Vfixnum_to_minuscule_map;
Lisp_Object Vfixnum_to_majuscule_ascii;

Fixnum Vmost_negative_fixnum, Vmost_positive_fixnum;

#ifdef DEBUG_XEMACS

int debug_issue_ebola_notices;

Fixnum debug_ebola_backtrace_length;

int
eq_with_ebola_notice (Lisp_Object obj1, Lisp_Object obj2)
{
  if (debug_issue_ebola_notices
      && ((CHARP (obj1) && FIXNUMP (obj2)) || (CHARP (obj2) && FIXNUMP (obj1))))
    {
      /* #### It would be really nice if this were a proper warning
         instead of brain-dead print to Qexternal_debugging_output.  */
      write_msg_string
	(Qexternal_debugging_output,
	 "Comparison between integer and character is constant nil (");
      Fprinc (obj1, Qexternal_debugging_output);
      write_msg_string (Qexternal_debugging_output, " and ");
      Fprinc (obj2, Qexternal_debugging_output);
      write_msg_string (Qexternal_debugging_output, ")\n");
      debug_short_backtrace (debug_ebola_backtrace_length);
    }
  return EQ (obj1, obj2);
}

#endif /* DEBUG_XEMACS */



Lisp_Object
wrong_type_argument (Lisp_Object predicate, Lisp_Object value)
{
  /* This function can GC */
  REGISTER Lisp_Object tem;
  do
    {
      value = Fsignal (Qwrong_type_argument, list2 (predicate, value));
      tem = call1 (predicate, value);
    }
  while (NILP (tem));
  return value;
}

DOESNT_RETURN
dead_wrong_type_argument (Lisp_Object predicate, Lisp_Object value)
{
  signal_error_1 (Qwrong_type_argument, list2 (predicate, value));
}

DEFUN ("wrong-type-argument", Fwrong_type_argument, 2, 2, 0, /*
Signal an error until the correct type value is given by the user.
This function loops, signalling a continuable `wrong-type-argument' error
with PREDICATE and VALUE as the data associated with the error and then
calling PREDICATE on the returned value, until the value gotten satisfies
PREDICATE.  At that point, the gotten value is returned.
*/
       (predicate, value))
{
  return wrong_type_argument (predicate, value);
}

DOESNT_RETURN
c_write_error (Lisp_Object obj)
{
  signal_error (Qsetting_constant,
		"Attempt to modify read-only object (c)", obj);
}

DOESNT_RETURN
lisp_write_error (Lisp_Object obj)
{
  signal_error (Qsetting_constant,
		"Attempt to modify read-only object (lisp)", obj);
}

DOESNT_RETURN
args_out_of_range (Lisp_Object a1, Lisp_Object a2)
{
  signal_error_1 (Qargs_out_of_range, list2 (a1, a2));
}

DOESNT_RETURN
args_out_of_range_3 (Lisp_Object a1, Lisp_Object a2, Lisp_Object a3)
{
  signal_error_1 (Qargs_out_of_range, list3 (a1, a2, a3));
}

void
check_integer_range (Lisp_Object val, Lisp_Object min, Lisp_Object max)
{
  Lisp_Object args[] = { min, val, max };
  int ii;

  for (ii = 0; ii < countof (args); ii++)
    {
      CHECK_INTEGER (args[ii]);
    }

  if (NILP (Fleq (countof (args), args)))
    args_out_of_range_3 (val, min, max);
}


/* Data type predicates */

DEFUN ("eq", Feq, 2, 2, 0, /*
Return t if the two args are the same Lisp object.
*/
       (object1, object2))
{
  return EQ_WITH_EBOLA_NOTICE (object1, object2) ? Qt : Qnil;
}

DEFUN ("consp", Fconsp, 1, 1, 0, /*
Return t if OBJECT is a cons cell.  `nil' is not a cons cell.

See the documentation for `cons' or the Lisp manual for more details on what
a cons cell is.
*/
       (object))
{
  return CONSP (object) ? Qt : Qnil;
}

DEFUN ("symbolp", Fsymbolp, 1, 1, 0, /*
Return t if OBJECT is a symbol.

A symbol is a Lisp object with a name. It can optionally have any and all of
a value, a property list and an associated function. 
*/
       (object))
{
  return SYMBOLP (object) ? Qt : Qnil;
}

DEFUN ("keywordp", Fkeywordp, 1, 1, 0, /*
Return t if OBJECT is a keyword.
*/
       (object))
{
  return KEYWORDP (object) ? Qt : Qnil;
}

DEFUN ("vectorp", Fvectorp, 1, 1, 0, /*
Return t if OBJECT is a vector.
*/
       (object))
{
  return VECTORP (object) ? Qt : Qnil;
}

DEFUN ("bit-vector-p", Fbit_vector_p, 1, 1, 0, /*
Return t if OBJECT is a bit vector.
*/
       (object))
{
  return BIT_VECTORP (object) ? Qt : Qnil;
}

DEFUN ("stringp", Fstringp, 1, 1, 0, /*
Return t if OBJECT is a string.
*/
       (object))
{
  return STRINGP (object) ? Qt : Qnil;
}

DEFUN ("arrayp", Farrayp, 1, 1, 0, /*
Return t if OBJECT is an array (string, vector, or bit vector).
*/
       (object))
{
  return ARRAYP (object) ? Qt : Qnil;
}

DEFUN ("sequencep", Fsequencep, 1, 1, 0, /*
Return t if OBJECT is a sequence (list or array).
*/
       (object))
{
  return SEQUENCEP (object) ? Qt : Qnil;
}

DEFUN ("markerp", Fmarkerp, 1, 1, 0, /*
Return t if OBJECT is a marker (editor pointer).
*/
       (object))
{
  return MARKERP (object) ? Qt : Qnil;
}

DEFUN ("subrp", Fsubrp, 1, 1, 0, /*
Return t if OBJECT is a built-in function.
*/
       (object))
{
  return SUBRP (object) ? Qt : Qnil;
}

DEFUN ("subr-min-args", Fsubr_min_args, 1, 1, 0, /*
Return minimum number of args built-in function SUBR may be called with.
*/
       (subr))
{
  CHECK_SUBR (subr);
  return make_fixnum (XSUBR (subr)->min_args);
}

DEFUN ("subr-max-args", Fsubr_max_args, 1, 1, 0, /*
Return maximum number of args built-in function SUBR may be called with,
or nil if it takes an arbitrary number of arguments or is a special operator.
*/
       (subr))
{
  int nargs;
  CHECK_SUBR (subr);
  nargs = XSUBR (subr)->max_args;
  if (nargs == MANY || nargs == UNEVALLED)
    return Qnil;
  else
    return make_fixnum (nargs);
}

DEFUN ("subr-interactive", Fsubr_interactive, 1, 1, 0, /*
Return the interactive spec of the subr object SUBR, or nil.
If non-nil, the return value will be a list whose first element is
`interactive' and whose second element is the interactive spec.
*/
       (subr))
{
  const CIbyte *prompt;
  CHECK_SUBR (subr);
  prompt = XSUBR (subr)->prompt;
  return prompt ? list2 (Qinteractive, build_msg_cistring (prompt)) : Qnil;
}


DEFUN ("characterp", Fcharacterp, 1, 1, 0, /*
Return t if OBJECT is a character.
Unlike in XEmacs v19 and FSF Emacs, a character is its own primitive type.
Any character can be converted into an equivalent integer using
`char-int'.  To convert the other way, use `int-char'; however,
only some integers can be converted into characters.  Such an integer
is called a `char-int'; see `char-int-p'.

Some functions that work on integers (e.g. the comparison functions
<, <=, =, /=, etc. and the arithmetic functions +, -, *, etc.)
accept characters and implicitly convert them into integers.  In
general, functions that work on characters also accept char-ints and
implicitly convert them into characters.  WARNING: Neither of these
behaviors is very desirable, and they are maintained for backward
compatibility with old E-Lisp programs that confounded characters and
integers willy-nilly.  These behaviors may change in the future; therefore,
do not rely on them.  Instead, use the character-specific functions such
as `char='.
*/
       (object))
{
  return CHARP (object) ? Qt : Qnil;
}

DEFUN ("char-to-int", Fchar_to_int, 1, 1, 0, /*
Convert CHARACTER into an equivalent integer.
The resulting integer will always be non-negative.  The integers in
the range 0 - 255 map to characters as follows:

0 - 31		Control set 0
32 - 127	ASCII
128 - 159	Control set 1
160 - 255	Right half of ISO-8859-1

If the `unicode-internal' feature is available, other integer values reflect
ISO-10646 values for character codes.  Otherwise, if the `mule' feature is
available, the values assigned to other characters may vary depending on the
particular version of XEmacs, the order in which character sets were loaded,
and you should not depend on them.

Some XEmacs builds have neither the `mule' nor the `unicode-internal' features
available, and in these builds no character has an integer value greater than
255.
*/
       (character))
{
  CHECK_CHAR (character);
  return make_fixnum (XCHAR (character));
}

DEFUN ("int-to-char", Fint_to_char, 1, 1, 0, /*
Convert integer INTEGER into the equivalent character.
Not all integers correspond to valid characters; use `char-int-p' to
determine whether this is the case.  If the integer cannot be converted,
nil is returned.
*/
       (integer))
{
  CHECK_INTEGER (integer);
  if (CHAR_INTP (integer))
    return make_char (XFIXNUM (integer));
  else
    return Qnil;
}

DEFUN ("fixnump", Ffixnump, 1, 1, 0, /*
Return t if OBJECT is a fixnum.

In this implementation, a fixnum is an immediate integer, and has a
maximum value described by the constant `most-positive-fixnum'.  This
contrasts with bignums, integers where the values are limited by your
available memory.
*/
       (object))
{
  return FIXNUMP (object) ? Qt : Qnil;
}
DEFUN ("integerp", Fintegerp, 1, 1, 0, /*
Return t if OBJECT is an integer, nil otherwise.

On builds without bignum support, this function is identical to `fixnump'.
*/
       (object))
{
  return INTEGERP (object) ? Qt : Qnil;
}

DEFUN ("natnump", Fnatnump, 1, 1, 0, /*
Return t if OBJECT is a nonnegative integer.
*/
       (object))
{
  return NATNUMP (object) ? Qt : Qnil;
}

DEFUN ("nonnegativep", Fnonnegativep, 1, 1, 0, /*
Return t if OBJECT is a nonnegative rational.
*/
       (object))
{
  return NATNUMP (object)
#ifdef HAVE_RATIO
    || (RATIOP (object) && ratio_sign (XRATIO_DATA (object)) >= 0)
#endif
#ifdef HAVE_BIGFLOAT
    || (BIGFLOATP (object) && bigfloat_sign (XBIGFLOAT_DATA (object)) >= 0)
#endif
    ? Qt : Qnil;
}

DEFUN ("numberp", Fnumberp, 1, 1, 0, /*
Return t if OBJECT is a number (floating point or rational).
*/
       (object))
{
  return NUMBERP (object) ? Qt : Qnil;
}

DEFUN ("floatp", Ffloatp, 1, 1, 0, /*
Return t if OBJECT is a floating point number.
*/
       (object))
{
  return FLOATP (object) ? Qt : Qnil;
}

DEFUN ("type-of", Ftype_of, 1, 1, 0, /*
Return a symbol representing the type of OBJECT.
*/
       (object))
{
  switch (XTYPE (object))
    {
    case Lisp_Type_Record:
      return intern (XRECORD_LHEADER_IMPLEMENTATION (object)->name);

    case Lisp_Type_Char: return Qcharacter;

    default: return Qfixnum;
    }
}


/* Extract and set components of lists */

DEFUN ("car", Fcar, 1, 1, 0, /*
Return the car of LIST.  If LIST is nil, return nil.
The car of a list or a dotted pair is its first element.

Error if LIST is not nil and not a cons cell.  See also `car-safe'.
*/
       (list))
{
  while (1)
    {
      if (CONSP (list))
	return XCAR (list);
      else if (NILP (list))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("car-safe", Fcar_safe, 1, 1, 0, /*
Return the car of OBJECT if it is a cons cell, or else nil.
*/
       (object))
{
  return CONSP (object) ? XCAR (object) : Qnil;
}

DEFUN ("cdr", Fcdr, 1, 1, 0, /*
Return the cdr of LIST.  If LIST is nil, return nil.
The cdr of a list is the list without its first element.  The cdr of a
dotted pair (A . B) is the second element, B.

Error if arg is not nil and not a cons cell.  See also `cdr-safe'.
*/
       (list))
{
  while (1)
    {
      if (CONSP (list))
	return XCDR (list);
      else if (NILP (list))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("cdr-safe", Fcdr_safe, 1, 1, 0, /*
Return the cdr of OBJECT if it is a cons cell, else nil.
*/
       (object))
{
  return CONSP (object) ? XCDR (object) : Qnil;
}

DEFUN ("setcar", Fsetcar, 2, 2, 0, /*
Set the car of CONS-CELL to be NEWCAR.  Return NEWCAR.
The car of a list or a dotted pair is its first element.
*/
       (cons_cell, newcar))
{
  if (!CONSP (cons_cell))
    cons_cell = wrong_type_argument (Qconsp, cons_cell);

  XCAR (cons_cell) = newcar;
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, 2, 2, 0, /*
Set the cdr of CONS-CELL to be NEWCDR.  Return NEWCDR.
The cdr of a list is the list without its first element.  The cdr of a
dotted pair (A . B) is the second element, B.
*/
       (cons_cell, newcdr))
{
  if (!CONSP (cons_cell))
    cons_cell = wrong_type_argument (Qconsp, cons_cell);

  XCDR (cons_cell) = newcdr;
  return newcdr;
}

/* Find the function at the end of a chain of symbol function indirections.

   If OBJECT is a symbol, find the end of its function chain and
   return the value found there.  If OBJECT is not a symbol, just
   return it.  If there is a cycle in the function chain, signal a
   cyclic-function-indirection error.

   This is like Findirect_function when VOID_FUNCTION_ERRORP is true.
   When VOID_FUNCTION_ERRORP is false, no error is signaled if the end
   of the chain ends up being Qunbound. */
Lisp_Object
indirect_function (Lisp_Object object, int void_function_errorp)
{
#define FUNCTION_INDIRECTION_SUSPICION_LENGTH 16
  Lisp_Object tortoise, hare;
  int count;

  for (hare = tortoise = object, count = 0;
       SYMBOLP (hare);
       hare = XSYMBOL (hare)->function, count++)
    {
      if (count < FUNCTION_INDIRECTION_SUSPICION_LENGTH) continue;

      if (count & 1)
	tortoise = XSYMBOL (tortoise)->function;
      if (EQ (hare, tortoise))
	return Fsignal (Qcyclic_function_indirection, list1 (object));
    }

  if (void_function_errorp && UNBOUNDP (hare))
    return signal_void_function_error (object);

  return hare;
}

DEFUN ("indirect-function", Findirect_function, 1, 1, 0, /*
Return the function at the end of OBJECT's function chain.
If OBJECT is a symbol, follow all function indirections and return
the final function binding.
If OBJECT is not a symbol, just return it.
Signal a void-function error if the final symbol is unbound.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.
*/
       (object))
{
  return indirect_function (object, 1);
}

/* Extract and set vector and string elements */

DEFUN ("aref", Faref, 2, 2, 0, /*
Return the element of ARRAY at index INDEX.
ARRAY may be a vector, bit vector, or string.  INDEX starts at 0.
*/
       (array, index_))
{
  EMACS_INT idx;

 retry:

  if      (FIXNUMP  (index_)) idx = XFIXNUM  (index_);
  else if (CHARP (index_)) idx = XCHAR (index_); /* yuck! */
#ifdef HAVE_BIGNUM
  else if (BIGNUMP (index_))
    {
      Lisp_Object canon = Fcanonicalize_number (index_);
      if (EQ (canon, index_))
	{
	  /* We don't support non-fixnum indices. */
	  goto range_error;
	}
      index_ = canon;
      goto retry;
    }
#endif
  else
    {
      index_ = wrong_type_argument (Qinteger_or_char_p, index_);
      goto retry;
    }

  if (idx < 0) goto range_error;

  if (VECTORP (array))
    {
      if (idx >= XVECTOR_LENGTH (array)) goto range_error;
      return XVECTOR_DATA (array)[idx];
    }
  else if (BIT_VECTORP (array))
    {
      if (idx >= (EMACS_INT) bit_vector_length (XBIT_VECTOR (array)))
	goto range_error;
      return make_fixnum (bit_vector_bit (XBIT_VECTOR (array), idx));
    }
  else if (STRINGP (array))
    {
      const Ibyte *data = XSTRING_DATA (array);
      Charcount ii = XSTRING_ASCII_BEGIN (array);

      sledgehammer_check_ascii_begin (array);

      if (idx < ii)
        {
          data += idx;
        }
      else
        {
          const Ibyte *endp = data + XSTRING_LENGTH (array);

          data += ii;

          while (ii < idx && data < endp)
            {
              INC_IBYTEPTR (data);
              ++ii;
            }
          
          if (data >= endp)
            {
              /* We shouldn't have been INC_IBYTEPTR'd past the end. */
              text_checking_assert (data == endp);
              goto range_error;
            }
        }

#ifdef ERROR_CHECK_TEXT
      assert (itext_ichar (data) == string_ichar (array, idx));
#endif

      return make_char (itext_ichar (data));
    }
  else
    {
      check_losing_bytecode ("aref", array);
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }

 range_error:
  args_out_of_range (array, index_);
  RETURN_NOT_REACHED (Qnil);
}

DEFUN ("aset", Faset, 3, 3, 0, /*
Store into the element of ARRAY at index INDEX the value NEWVAL.
ARRAY may be a vector, bit vector, or string.  INDEX starts at 0.
*/
       (array, index_, newval))
{
  EMACS_INT idx;

 retry:

  if      (FIXNUMP  (index_)) idx = XFIXNUM (index_);
  else if (CHARP (index_)) idx = XCHAR (index_); /* yuck! */
#ifdef HAVE_BIGNUM
  else if (BIGNUMP (index_))
    {
      Lisp_Object canon = Fcanonicalize_number (index_);
      if (EQ (canon, index_))
	{
	  /* We don't support non-fixnum indices. */
	  goto range_error;
	}
      index_ = canon;
      goto retry;
    }
#endif
  else
    {
      index_ = wrong_type_argument (Qinteger_or_char_p, index_);
      goto retry;
    }

  if (idx < 0) goto range_error;

  CHECK_LISP_WRITEABLE (array);
  if (VECTORP (array))
    {
      if (idx >= XVECTOR_LENGTH (array)) goto range_error;
      XVECTOR_DATA (array)[idx] = newval;
    }
  else if (BIT_VECTORP (array))
    {
      if (idx >= (EMACS_INT) bit_vector_length (XBIT_VECTOR (array)))
	goto range_error;
      CHECK_BIT (newval);
      set_bit_vector_bit (XBIT_VECTOR (array), idx, !ZEROP (newval));
    }
  else if (STRINGP (array))
    {
      CHECK_CHAR_COERCE_INT (newval);

      /* Let it do the range checking, keep it ON rather than O2N
         for Mule. */
      set_string_char (array, idx, XCHAR (newval));
      bump_string_modiff (array);
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }

  return newval;

 range_error:
  args_out_of_range (array, index_);
  RETURN_NOT_REACHED (Qnil);
}


/**********************************************************************/
/*                       Arithmetic functions                         */
/**********************************************************************/
#ifndef WITH_NUMBER_TYPES
typedef struct
{
  int int_p;
  union
  {
    EMACS_INT ival;
    double dval;
  } c;
} int_or_double;

static void
number_char_or_marker_to_int_or_double (Lisp_Object obj, int_or_double *p)
{
 retry:
  p->int_p = 1;
  if      (FIXNUMP    (obj)) p->c.ival = XFIXNUM  (obj);
  else if (CHARP   (obj)) p->c.ival = XCHAR (obj);
  else if (MARKERP (obj)) p->c.ival = marker_position (obj);
  else if (FLOATP  (obj)) p->c.dval = XFLOAT_DATA (obj), p->int_p = 0;
  else
    {
      obj = wrong_type_argument (Qnumber_char_or_marker_p, obj);
      goto retry;
    }
}

static double
number_char_or_marker_to_double (Lisp_Object obj)
{
 retry:
  if      (FIXNUMP    (obj)) return (double) XFIXNUM  (obj);
  else if (CHARP   (obj)) return (double) XCHAR (obj);
  else if (MARKERP (obj)) return (double) marker_position (obj);
  else if (FLOATP  (obj)) return XFLOAT_DATA (obj);
  else
    {
      obj = wrong_type_argument (Qnumber_char_or_marker_p, obj);
      goto retry;
    }
}
#endif /* WITH_NUMBER_TYPES */

static EMACS_INT
fixnum_char_or_marker_to_int (Lisp_Object obj)
{
 retry:
  if      (FIXNUMP    (obj)) return XFIXNUM  (obj);
  else if (CHARP   (obj)) return XCHAR (obj);
  else if (MARKERP (obj)) return marker_position (obj);
  else
    {
      /* On bignum builds, we can only be called from #'lognot, which
	 protects against this happening: */
      assert (!BIGNUMP (obj));
      obj = wrong_type_argument (Qinteger_char_or_marker_p, obj);
      goto retry;
    }
}

#ifdef WITH_NUMBER_TYPES

#ifdef HAVE_BIGNUM
#define BIGNUM_CASE(op)							\
        case LAZY_BIGNUM_T:                                             \
	  if (!bignum_##op (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2)))	\
	    return Qnil;						\
	  break;
#else
#define BIGNUM_CASE(op)
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
#define RATIO_CASE(op)							\
        case LAZY_RATIO_T:                                              \
	  if (!ratio_##op (XRATIO_DATA (obj1), XRATIO_DATA (obj2)))	\
	    return Qnil;						\
	  break;
#else
#define RATIO_CASE(op)
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
#define BIGFLOAT_CASE(op)						\
	case LAZY_BIGFLOAT_T:						\
	  if (!bigfloat_##op (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2))) \
	    return Qnil;						\
	  break;
#else
#define BIGFLOAT_CASE(op)
#endif /* HAVE_BIGFLOAT */

#define ARITHCOMPARE_MANY(c_op,op)				\
{								\
  REGISTER int i;						\
  Lisp_Object obj1, obj2;					\
								\
  for (i = 1; i < nargs; i++)					\
    {								\
      obj1 = args[i - 1];					\
      obj2 = args[i];						\
      switch (promote_args_lazy (&obj1, &obj2))                 \
	{							\
        case LAZY_FIXNUM_T:                                     \
          if (!(XREALFIXNUM (obj1) c_op XREALFIXNUM (obj2)))    \
	    return Qnil;					\
	  break;						\
	BIGNUM_CASE (op)					\
	RATIO_CASE (op)						\
        case LAZY_FLOAT_T:                                      \
	  if (!(XFLOAT_DATA (obj1) c_op XFLOAT_DATA (obj2)))	\
	    return Qnil;					\
	  break;						\
	BIGFLOAT_CASE (op)					\
        case LAZY_MARKER_T:                                     \
          if (!(byte_marker_position (obj1) c_op                \
                byte_marker_position (obj2)))                   \
            return Qnil;                                        \
          break;                                                \
	}							\
    }								\
  return Qt;							\
}
#else /* !WITH_NUMBER_TYPES */
/* We don't convert markers lazily here, although we could. It's more
   important that we do this lazily in bytecode, which is the case; see
   bytecode_arithcompare().
   */
#define ARITHCOMPARE_MANY(c_op,op)				\
{								\
  int_or_double iod1, iod2, *p = &iod1, *q = &iod2;		\
  Lisp_Object *args_end = args + nargs;				\
								\
  number_char_or_marker_to_int_or_double (*args++, p);		\
								\
  while (args < args_end)					\
    {								\
      number_char_or_marker_to_int_or_double (*args++, q);	\
								\
      if (!((p->int_p && q->int_p) ?				\
	    (p->c.ival c_op q->c.ival) :			\
	    ((p->int_p ? (double) p->c.ival : p->c.dval) c_op	\
	     (q->int_p ? (double) q->c.ival : q->c.dval))))	\
	return Qnil;						\
								\
      { /* swap */ int_or_double *r = p; p = q; q = r; }	\
    }								\
  return Qt;							\
}
#endif /* WITH_NUMBER_TYPES */

DEFUN ("=", Feqlsign, 1, MANY, 0, /*
Return t if all the arguments are numerically equal.
The arguments may be numbers, characters or markers.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (==, eql)
}

DEFUN ("<", Flss, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically increasing.

(That is, if there is a second argument, it must be numerically greater than
the first.  If there is a third, it must be numerically greater than the
second, and so on.)  At least one argument is required.

The arguments may be numbers, characters or markers.  

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (<, lt)
}

DEFUN (">", Fgtr, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically decreasing.

(That is, if there is a second argument, it must be numerically less than
the first.  If there is a third, it must be numerically less than the
second, and so forth.)  At least one argument is required.

The arguments may be numbers, characters or markers.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (>, gt)
}

DEFUN ("<=", Fleq, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically nondecreasing.
The arguments may be numbers, characters or markers.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (<=, le)
}

DEFUN (">=", Fgeq, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically nonincreasing.
The arguments may be numbers, characters or markers.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (>=, ge)
}

/* Unlike all the other comparisons, this is an O(N*N) algorithm.  But who
   cares?  Inspection of all elisp code distributed by xemacs.org shows that
   it is almost always called with 2 arguments, rarely with 3, and never with
   more than 3.  The constant factors of algorithms with better asymptotic
   complexity are higher, which means that those algorithms will run SLOWER
   than this one in the common case.  Optimize the common case! */
DEFUN ("/=", Fneq, 1, MANY, 0, /*
Return t if no two arguments are numerically equal.
The arguments may be numbers, characters or markers.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i, j;
  Lisp_Object obj1, obj2;

  for (i = 0; i < nargs - 1; i++)
    {
      obj1 = args[i];
      for (j = i + 1; j < nargs; j++)
	{
	  obj2 = args[j];
	  switch (promote_args (&obj1, &obj2))
	    {
	    case FIXNUM_T:
	      if (XREALFIXNUM (obj1) == XREALFIXNUM (obj2))
		return Qnil;
	      break;
#ifdef HAVE_BIGNUM
	    case BIGNUM_T:
	      if (bignum_eql (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2)))
		return Qnil;
	      break;
#endif
#ifdef HAVE_RATIO
	    case RATIO_T:
	      if (ratio_eql (XRATIO_DATA (obj1), XRATIO_DATA (obj2)))
		return Qnil;
	      break;
#endif
	    case FLOAT_T:
	      if (XFLOAT_DATA (obj1) == XFLOAT_DATA (obj2))
		return Qnil;
	      break;
#ifdef HAVE_BIGFLOAT
	    case BIGFLOAT_T:
	      if (bigfloat_eql (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2)))
		return Qnil;
	      break;
#endif
	    }
	}
    }
  return Qt;
#else /* !WITH_NUMBER_TYPES */
  Lisp_Object *args_end = args + nargs;
  Lisp_Object *p, *q;

  /* Unlike all the other comparisons, this is an N*N algorithm.
     We could use a hash table for nargs > 50 to make this linear. */
  for (p = args; p < args_end; p++)
    {
      int_or_double iod1, iod2;
      number_char_or_marker_to_int_or_double (*p, &iod1);

      for (q = p + 1; q < args_end; q++)
	{
	  number_char_or_marker_to_int_or_double (*q, &iod2);

	  if (!((iod1.int_p && iod2.int_p) ?
		(iod1.c.ival != iod2.c.ival) :
		((iod1.int_p ? (double) iod1.c.ival : iod1.c.dval) !=
		 (iod2.int_p ? (double) iod2.c.ival : iod2.c.dval))))
	    return Qnil;
	}
    }
  return Qt;
#endif /* WITH_NUMBER_TYPES */
}


/* Convert between an unsigned 32-bit value and some Lisp value that preserves
   all its bits. Use an integer if the value will fit (that is, if the value
   is <= MOST_POSITIVE_FIXNUM_UNSIGNED, or if we have bignums available);
   otherwise, return a cons of two sixteen-bit values.  Both types of return
   value need GC protection.

   This is used to pass 32-bit integers to and from the user.  Use
   make_time() and lisp_to_time() for time_t values.

   If you're thinking of using this to store a pointer into a Lisp Object
   for internal purposes (such as when calling record_unwind_protect()),
   try using make_opaque_ptr()/get_opaque_ptr() instead. */
Lisp_Object
uint32_t_to_lisp (UINT_32_BIT item)
{
  if (sizeof (item) < sizeof (EMACS_INT) /* Fits in a positive fixnum? */
      || item <= MOST_POSITIVE_FIXNUM_UNSIGNED)
    {
      return make_fixnum (item);
    }

#ifdef HAVE_BIGNUM
  return make_unsigned_integer ((EMACS_UINT) item);
#else
  return Fcons (make_fixnum (item >> 16), make_fixnum (item & 0xffff));
#endif
}

UINT_32_BIT
lisp_to_uint32_t (Lisp_Object item)
{
  if (INTEGERP (item))
    {
      check_integer_range (item, Qzero,
#if defined (HAVE_BIGNUM) || (SIZEOF_EMACS_INT > 4)
                           make_unsigned_integer (MAKE_32_BIT_UNSIGNED_CONSTANT
                                                  (0xffffffff))
#else
                           make_fixnum (MOST_POSITIVE_FIXNUM)
#endif
                           );

#ifdef HAVE_BIGNUM
      if (BIGNUMP (item))
        {
          /* EMACS_UINT will have at least 32 value bits, and we've checked
             the range above, this value is not greater than #xFFFFFFFF. */
          return (UINT_32_BIT) bignum_to_emacs_uint (XBIGNUM_DATA (item));
        }
#endif
      return (UINT_32_BIT) XFIXNUM (item);
    }
  else
    {
      Lisp_Object top = Fcar (item);
      Lisp_Object bot = Fcdr (item);

      check_integer_range (top, Qzero, make_fixnum (0xFFFF));
      check_integer_range (bot, Qzero, make_fixnum (0xFFFF));

      type_checking_assert (FIXNUMP (top) && FIXNUMP (bot));

      return ((UINT_32_BIT) XFIXNUM (top) << 16) |
	      (UINT_32_BIT) (XFIXNUM (bot) & 0xffff);
    }
}

Lisp_Object
int32_t_to_lisp (INT_32_BIT item)
{
  if (NUMBER_FITS_IN_A_FIXNUM ((EMACS_INT) item))
    {
      return make_fixnum (item);
    }

#ifdef HAVE_BIGNUM
  return make_integer ((EMACS_INT) item);
#else
  return Fcons (make_fixnum ((UINT_32_BIT) item >> 16),
                make_fixnum (item & 0xffff));
#endif
}

INT_32_BIT
lisp_to_int32_t (Lisp_Object item)
{
  if (INTEGERP (item))
    {
#if (FIXNUM_VALBITS > 32) || defined(HAVE_BIGNUM)
      check_integer_range (item, make_integer ((INT_32_BIT) (~0x7FFFFFFF)),
                           make_integer (0x7fffffff));
#endif
#ifdef HAVE_BIGNUM
      if (BIGNUMP (item))
        {
          /* EMACS_INT will have at least 32 value bits, and we've checked the
             range above, this value fits 32 bits. */
          return (INT_32_BIT) bignum_to_emacs_int (XBIGNUM_DATA (item));
        }
#endif
      return (UINT_32_BIT) XFIXNUM (item);
    }
  else
    {
      Lisp_Object top = Fcar (item);
      Lisp_Object bot = XCDR (item);

      check_integer_range (top, Qzero, make_fixnum (0xFFFF));
      check_integer_range (bot, Qzero, make_fixnum (0xFFFF));

      type_checking_assert (FIXNUMP (top) && FIXNUMP (bot));

      return (XFIXNUM (top) << 16) | (XFIXNUM (bot) & 0xffff);
    }
}

#ifndef HAVE_BIGNUM
static int
digit_to_number (int character, int base)
{
  /* Assumes ASCII */
  int digit = ((character >= '0' && character <= '9') ? character - '0'      :
	       (character >= 'a' && character <= 'z') ? character - 'a' + 10 :
	       (character >= 'A' && character <= 'Z') ? character - 'A' + 10 :
	       -1);

  return digit >= base ? -1 : digit;
}
#endif

DEFUN ("string-to-number", Fstring_to_number, 1, 2, 0, /*
Convert STRING to a number by parsing it as a number in base BASE.
This parses both integers and floating point numbers.
If they are supported, it also reads ratios.
It ignores leading spaces and tabs.

If BASE is nil or omitted, base 10 is used.
BASE must be an integer between 2 and 16 (inclusive).
Floating point numbers always use base 10.
*/
       (string, base))
{
  Ibyte *p;
  int b;

  CHECK_STRING (string);

  if (NILP (base))
    b = 10;
  else
    {
      check_integer_range (base, make_fixnum (2), make_fixnum (16));
      b = XFIXNUM (base);
    }

  p = XSTRING_DATA (string);

  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  while (*p == ' ' || *p == '\t')
    p++;

  if (isfloat_string ((const char *) p) && b == 10)
    {
#ifdef HAVE_BIGFLOAT
      if (ZEROP (Vdefault_float_precision))
#endif
	return make_float (atof ((const char *) p));
#ifdef HAVE_BIGFLOAT
      else
	{
	  /* The GMP version of bigfloat_set_string (mpf_set_str) has the
	     following limitation: if p starts with a '+' sign, it does
	     nothing; i.e., it leaves its bigfloat argument untouched.
	     Therefore, move p past any leading '+' signs. */
	  if (*p == '+')
	    p++;
	  bigfloat_set_prec (scratch_bigfloat, bigfloat_get_default_prec ());
	  bigfloat_set_string (scratch_bigfloat, (const char *) p, b);
	  return make_bigfloat_bf (scratch_bigfloat);
	}
#endif
    }

#ifdef HAVE_RATIO
  if (qxestrchr (p, '/') != NULL)
    {
      /* The GMP version of ratio_set_string (mpq_set_str) has the following
	 limitations:
	 - If p starts with a '+' sign, it does nothing; i.e., it leaves its
	   ratio argument untouched.
	 - If p has a '+' sign after the '/' (e.g., 300/+400), it sets the
	   numerator from the string, but *leaves the denominator unchanged*.
         - If p has trailing nonnumeric characters, it sets the numerator from
           the string, but leaves the denominator unchanged.
         - If p has more than one '/', (e.g., 1/2/3), then it sets the
           numerator from the string, but leaves the denominator unchanged.

         Therefore, move p past any leading '+' signs, temporarily drop a null
         after the numeric characters we are trying to convert, and then put
         the nulled character back afterward.  I am not going to fix problem
         #2; just don't write ratios that look like that. */
      Ibyte *end, save;

      if (*p == '+')
	p++;

      end = p;
      if (*end == '-')
	end++;
      while ((*end >= '0' && *end <= '9') ||
	     (b > 10 && *end >= 'a' && *end <= 'a' + b - 11) ||
	     (b > 10 && *end >= 'A' && *end <= 'A' + b - 11))
	end++;
      if (*end == '/')
	{
	  end++;
	  if (*end == '-')
	    end++;
	  while ((*end >= '0' && *end <= '9') ||
		 (b > 10 && *end >= 'a' && *end <= 'a' + b - 11) ||
		 (b > 10 && *end >= 'A' && *end <= 'A' + b - 11))
	    end++;
	}
      save = *end;
      *end = '\0';
      ratio_set_string (scratch_ratio, (const char *) p, b);
      *end = save;
      ratio_canonicalize (scratch_ratio);
      return Fcanonicalize_number (make_ratio_rt (scratch_ratio));
    }
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGNUM
  {
    /* The GMP version of bignum_set_string (mpz_set_str) has the following
       limitations:
       - If p starts with a '+' sign, it does nothing; i.e., it leaves its
         bignum argument untouched.
       - If p is the empty string, it does nothing.
       - If p has trailing nonnumeric characters, it does nothing.

       Therefore, move p past any leading '+' signs, temporarily drop a null
       after the numeric characters we are trying to convert, special case the
       empty string, and then put the nulled character back afterward. */
    Ibyte *end, save;
    Lisp_Object retval;

    if (*p == '+')
      p++;
    end = p;
    if (*end == '-')
      end++;
    while ((*end >= '0' && *end <= '9') ||
	   (b > 10 && *end >= 'a' && *end <= 'a' + b - 11) ||
	   (b > 10 && *end >= 'A' && *end <= 'A' + b - 11))
      end++;
    save = *end;
    *end = '\0';
    if (*p == '\0')
      retval = make_fixnum (0);
    else
      {
	bignum_set_string (scratch_bignum, (const char *) p, b);
	retval = Fcanonicalize_number (make_bignum_bg (scratch_bignum));
      }
    *end = save;
    return retval;
  }
#else
  if (b == 10)
    {
      /* Use the system-provided functions for base 10. */
#if   SIZEOF_EMACS_INT == SIZEOF_INT
      return make_fixnum (atoi ((char*) p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG
      return make_fixnum (atol ((char*) p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG_LONG
      return make_fixnum (atoll ((char*) p));
#endif
    }
  else
    {
      int negative = 1;
      EMACS_INT v = 0;

      if (*p == '-')
	{
	  negative = -1;
	  p++;
	}
      else if (*p == '+')
	p++;
      while (1)
	{
	  int digit = digit_to_number (*p++, b);
	  if (digit < 0)
	    break;
	  v = v * b + digit;
	}
      return make_fixnum (negative * v);
    }
#endif /* HAVE_BIGNUM */
}

static int
find_highest_value (Lisp_Object UNUSED (table), Ichar UNUSED (from),
                    Ichar UNUSED (to), Lisp_Object val, void *extra_arg)
{
  Lisp_Object *highest_pointer = (Lisp_Object *) extra_arg;
  Lisp_Object max_seen = *highest_pointer;

  CHECK_FIXNUM (val);

  if (XFIXNUM (max_seen) < XFIXNUM (val))
    {
      *highest_pointer = val;
    }

  return 0;
}

static int
fill_ichar_array (Lisp_Object UNUSED (table), Ichar start,
                  Ichar UNUSED (end), Lisp_Object val, void *extra_arg)
{
  Ichar *cctable = (Ichar *) extra_arg;
  EMACS_INT valint = XFIXNUM (val);

  /* Save the value if it hasn't been seen yet. */
  if (-1 == cctable[valint])
    {
      cctable[valint] = start;
    }
  else
    {
      /* Otherwise, save it if the existing value is not uppercase, and this
	 one is. Use the standard case table rather than any buffer-specific
	 one because a) this can be called early before current_buffer is
	 available and b) it's better to have these independent of particular
	 buffer case tables. */
      if (current_buffer != NULL && UPCASE (0, start) == start
          && UPCASE (0, cctable[valint]) != cctable[valint])
	{
	  cctable[valint] = start;
	}
      /* Maybe our own case infrastructure is not available yet. Use the C
         library's. */
      else if (current_buffer == NULL)
        {
	  /* The C library can't necessarily handle values outside of
	     the range EOF to CHAR_MAX, inclusive. */
	  assert (start == EOF || start <= CHAR_MAX);
	  if (isupper (start) && !isupper (cctable[valint]))
	    {
	      cctable[valint] = start;
	    }
        }
      /* Otherwise, save it if this character has a numerically lower value
         (preferring ASCII over fullwidth Chinese and so on). */
      else if (start < cctable[valint])
	{
	  cctable[valint] = start;
	}
    }

  return 0;
}

Lisp_Object
build_fixnum_to_char_map (Lisp_Object radix_table)
{
  Lisp_Object highest_value, result;
  struct chartab_range ctr = { CHARTAB_RANGE_ALL, 0, 0, Qnil, 0 };
  Ichar *cctable;
  EMACS_INT ii, cclen;
  Ibyte *data;

  /* What's the greatest fixnum value seen? In passing, check all the char
     table values are fixnums. */
  CHECK_FIXNUM (XCHAR_TABLE (radix_table)->default_);
  highest_value = XCHAR_TABLE (radix_table)->default_;

  map_char_table (radix_table, &ctr, find_highest_value, &highest_value);
  cclen = XFIXNUM (highest_value) + 1;

  cctable = (Ichar *)malloc (sizeof (Ichar) * cclen);
  if (cctable == NULL)
    {
      out_of_memory ("Could not allocate data for `digit-char'", Qunbound);
    }

  for (ii = 0; ii < cclen; ++ii)
    {
      cctable[ii] = (Ichar) -1;
    }

  map_char_table (radix_table, &ctr, fill_ichar_array, cctable);

  for (ii = 0; ii < cclen; ++ii)
    {
      if (cctable[ii] < 0)
	{
	  free (cctable);
	  invalid_argument ("No digit specified for weight", make_fixnum (ii));
	}
    }

  result = Fmake_string (make_fixnum (cclen * MAX_ICHAR_LEN), make_char (0));

  data = XSTRING_DATA (result);
  for (ii = 0; ii < cclen; ii++)
    {
      (void) set_itext_ichar (data + (MAX_ICHAR_LEN * ii), cctable[ii]);
    }

  init_string_ascii_begin (result);
  bump_string_modiff (result);
  sledgehammer_check_ascii_begin (result);

  free (cctable);

  return result;
}

DEFUN ("set-digit-fixnum-map", Fset_digit_fixnum_map, 1, 1, 0, /*
Set the value of `digit-fixnum-map', which see.

Also check that RADIX-TABLE is well-formed from the perspective of
`parse-integer' and `digit-char-p', and create an internal inverse mapping
for `digit-char', so that all three functions behave consistently.

RADIX-TABLE itself is not saved, a read-only copy of it is made and returned.
*/
       (radix_table))
{
  Lisp_Object ftctable = Qnil;

  CHECK_CHAR_TABLE (radix_table);

  /* Create a table for `digit-char', checking the consistency of
     radix_table while doing so. */
  ftctable = build_fixnum_to_char_map (radix_table);

  Vdigit_fixnum_map = Fcopy_char_table (radix_table);
  LISP_READONLY (Vdigit_fixnum_map) = 1;
  Vfixnum_to_majuscule_map = ftctable;
  Vfixnum_to_minuscule_map = Fcanoncase (ftctable, Qnil);

  return Vdigit_fixnum_map;
}

DEFUN ("digit-char-p", Fdigit_char_p, 1, 3, 0, /*
Return non-nil if CHARACTER represents a digit in base RADIX.

RADIX defaults to ten.  The actual non-nil value returned is the integer
value of the character in base RADIX.

RADIX-TABLE, if non-nil, is a character table describing characters' numeric
values. See `parse-integer' and `digit-fixnum-map'.
*/
       (character, radix, radix_table))
{
  Lisp_Object got = Qnil;
  EMACS_INT radixing, val;
  Ichar cc;

  CHECK_CHAR (character);
  cc = XCHAR (character);

  if (!NILP (radix))
    {
      check_integer_range (radix, Qzero,
                           NILP (radix_table) ?
                           /* If we are using the default radix table, the
                              maximum possible value for the radix is
                              available to us now. */
                           make_fixnum
                           (XSTRING_LENGTH (Vfixnum_to_majuscule_map)
                            / MAX_ICHAR_LEN)
                           /* Otherwise, calculating that is expensive. Check
                              at least that the radix is not a bignum, the
                              maximum count of characters available will not
                              exceed the size of a fixnum. */
                           : make_fixnum (MOST_POSITIVE_FIXNUM));
      radixing = XFIXNUM (radix);
    }
  else
    {
      radixing = 10;
    }

  if (NILP (radix_table))
    {
      radix_table = Vdigit_fixnum_map;
    }

  got = get_char_table (cc, radix_table);
  CHECK_FIXNUM (got);
  val = XFIXNUM (got);

  if (val < 0 || val >= radixing)
    {
      return Qnil;
    }

  return make_fixnum (val);
}

DEFUN ("digit-char", Fdigit_char, 1, 3, 0, /*
Return a character representing the integer WEIGHT in base RADIX.

RADIX defaults to ten.  If no such character exists, return nil. `digit-char'
prefers an upper case character if available.  RADIX must be a non-negative
integer of value less than the maximum value in RADIX-TABLE.

RADIX-TABLE, if non-nil, is a character table describing characters' numeric
values. It defaults to the value of `digit-fixnum-map'; see the documentation
for that variable and for `parse-integer'. This is not specified by Common
Lisp, and using a value other than the default, or `digit-fixnum-ascii' is
expensive, since the inverse map needs to be calculated.
*/
       (weight, radix, radix_table))
{
  EMACS_INT radixing = 10, weighting;
  Lisp_Object fixnum_to_char_table = Qnil;
  Ichar cc;

  CHECK_NATNUM (weight);

  if (!NILP (radix_table) && !EQ (radix_table, Vdigit_fixnum_map))
    {
      CHECK_CHAR_TABLE (radix_table);
      if (EQ (Vdigit_fixnum_ascii, radix_table))
        {
          fixnum_to_char_table = Vfixnum_to_majuscule_ascii;
        }
      else
        {
          /* The result of this isn't GCPROd, but the rest of this function
             won't GC and continue. */
          fixnum_to_char_table = build_fixnum_to_char_map (radix_table);
        }
    }
  else
    {
      fixnum_to_char_table = Vfixnum_to_majuscule_map;
    }

  if (!NILP (radix))
    {
      check_integer_range (radix, Qzero,
                           make_fixnum (XSTRING_LENGTH (fixnum_to_char_table)
                                        / MAX_ICHAR_LEN));
      radixing = XFIXNUM (radix);
    }

  /* If weight is in its canonical form (and there's no reason to think it
     isn't), Vfixnum_to_majuscule_map can't be long enough to handle
     this. */
  if (BIGNUMP (weight))
    {
      return Qnil;
    }

  weighting = XFIXNUM (weight);

  if (weighting < radixing)
    {
      cc = itext_ichar (XSTRING_DATA (fixnum_to_char_table)
			+ MAX_ICHAR_LEN * weighting);
      return make_char (cc);
    }

  return Qnil;
}

Lisp_Object
parse_integer (const Ibyte *buf, Ibyte **buf_end_out, Bytecount len,
               EMACS_INT base, int flags, Lisp_Object radix_table)
{
  const Ibyte *lim = buf + len, *p = buf;
  EMACS_UINT num = 0, onum = (EMACS_UINT) -1;
  EMACS_UINT fixnum_limit = MOST_POSITIVE_FIXNUM;
  EMACS_INT cint = 0;
  Boolint negativland = 0;
  Ichar c = -1;
  Lisp_Object result = Qnil, got = Qnil;

  if (NILP (radix_table))
    {
      radix_table = Vdigit_fixnum_map;
    }

#ifdef MIRROR_TABLE
  /* This function ignores the current buffer's syntax table.
     Respecting it will probably introduce more bugs than it fixes. */
  update_mirror_syntax_if_dirty (XCHAR_TABLE (Vstandard_syntax_table)->
                                 mirror_table);
#endif

  /* Ignore leading whitespace, if that leading whitespace has no
     numeric value. */
  while (p < lim)
    {
      c = itext_ichar (p);
      if (!(((got = get_char_table (c, radix_table), FIXNUMP (got))
             && ((cint = XFIXNUM (got), cint < 0) || cint >= base))
            && (SYNTAX (BUFFER_MIRROR_SYNTAX_TABLE (0), c)
		== Swhitespace)))
        {
          break;
        }

      INC_IBYTEPTR (p);
    }

  /* Drop sign information if appropriate. */
  if (c == '-')
    {
      negativland = 1;
      fixnum_limit = - MOST_NEGATIVE_FIXNUM;
      INC_IBYTEPTR (p);
    }
  else if (c == '+')
    {
      got = get_char_table (c, radix_table);
      cint = FIXNUMP (got) ? XFIXNUM (got) : -1;
      /* If ?+ has no integer weight, drop it. */
      if (cint < 0 || cint >= base)
        {
          INC_IBYTEPTR (p);
        }
    }

  while (p < lim)
    {
      c = itext_ichar (p);
      
      got = get_char_table (c, radix_table);
      if (!FIXNUMP (got))
        {
          goto loser;
        }

      cint = XFIXNUM (got);

      if (cint < 0 || cint >= base)
        {
          goto loser;
        }

      onum = num;
      num *= base;
      if (num > fixnum_limit || num < onum)
        {
          goto overflow;
        }

      num += cint;
      if (num > fixnum_limit)
        {
          goto overflow;
        }

      INC_IBYTEPTR (p);
    }

  if (onum == (EMACS_UINT) -1)
    {
      /* No digits seen, we may need to error. */
      goto loser;
    }

  if (negativland)
    {
      result = make_fixnum (- (EMACS_INT) num);
    }
  else
    {
      result = make_fixnum (num);
    }

  *buf_end_out = (Ibyte *) p;
  return result;

 overflow:
#ifndef HAVE_BIGNUM
  if ((flags & CHECK_OVERFLOW_SYNTAX))
    {
      /* No bignum support, but our callers want our syntax checking, rather
         than simply erroring on overflow. */
      result = Qunbound;

      while (p < lim)
        {
          c = itext_ichar (p);    

          got = get_char_table (c, radix_table);
          if (!FIXNUMP (got))
            {
              goto loser;
            }

          cint = XFIXNUM (got);
          if (cint < 0 || cint >= base)
            {
              goto loser;
            }

          /* Nothing to do in terms of checking for overflow, just advance
             through the string. */

          INC_IBYTEPTR (p);
        }

      *buf_end_out = (Ibyte *)  p;

      return result;
    }

  return Fsignal (Qunsupported_type,
                  list3 (build_ascstring ("bignum"), make_string (buf, len),
                         make_fixnum (base)));
#else /* HAVE_BIGNUM */
  result = make_bignum_emacs_uint (onum);

  bignum_set_emacs_int (scratch_bignum, base);
  bignum_set_emacs_int (scratch_bignum2, cint);
  bignum_mul (XBIGNUM_DATA (result), XBIGNUM_DATA (result), scratch_bignum);
  bignum_add (XBIGNUM_DATA (result), XBIGNUM_DATA (result), scratch_bignum2);
  INC_IBYTEPTR (p);

  assert (!bignum_fits_emacs_int_p (XBIGNUM_DATA (result))
          || (fixnum_limit
              < (EMACS_UINT) bignum_to_emacs_int (XBIGNUM_DATA (result))));

  while (p < lim)
    {
      c = itext_ichar (p);    

      got = get_char_table (c, radix_table);
      if (!FIXNUMP (got))
	{
	  goto loser;
	}

      cint = XFIXNUM (got);
      if (cint < 0 || cint >= base)
	{
	  goto loser;
	}

      bignum_set_emacs_int (scratch_bignum2, cint);
      bignum_mul (XBIGNUM_DATA (result), XBIGNUM_DATA (result),
                  scratch_bignum);
      bignum_add (XBIGNUM_DATA (result), XBIGNUM_DATA (result),
                  scratch_bignum2);

      INC_IBYTEPTR (p);
    }

  if (negativland)
    {
      bignum_set_long (scratch_bignum, -1L);
      bignum_mul (XBIGNUM_DATA (result), XBIGNUM_DATA (result),
                  scratch_bignum);
    }

  *buf_end_out = (Ibyte *)  p;
  return result;
#endif /* HAVE_BIGNUM */
 loser:

  if (p < lim && !(flags & JUNK_ALLOWED))
    {
      /* JUNK-ALLOWED is zero. If we have stopped parsing because we
	 encountered whitespace, then we need to check that the rest if the
	 string is whitespace and whitespace alone if we are not to error.

         Perhaps surprisingly, if JUNK-ALLOWED is zero, the parse is regarded
         as including the trailing whitespace, so the second value returned is
         always the length of the string. */
      while (p < lim)
	{
	  c = itext_ichar (p);
	  if (!(SYNTAX (BUFFER_MIRROR_SYNTAX_TABLE (0), c)
                == Swhitespace))
	    {
	      break;
	    }

	  INC_IBYTEPTR (p);
	}
    }

  *buf_end_out = (Ibyte *) p;

  if ((flags & JUNK_ALLOWED) || (p == lim && onum != (EMACS_UINT) -1))
    {
      if (!NILP (result))
        {

#ifdef HAVE_BIGNUM
          /* Bignum terminated by whitespace or by non-digit. */
          if (negativland)
            {
              bignum_set_long (scratch_bignum, -1L);
              bignum_mul (XBIGNUM_DATA (result), XBIGNUM_DATA (result),
                          scratch_bignum);
            }
#endif
          /* When HAVE_BIGNUM is not defined, this can be Qunbound. */
          return result;
        }

      if (onum == (EMACS_UINT) -1)
        {
          /* No integer digits seen, but junk allowed, so no indication to
             error. Return nil. */
          return Qnil;
        }

      if (negativland)
        {
          assert ((- (EMACS_INT) num) >= MOST_NEGATIVE_FIXNUM);
          result = make_fixnum (- (EMACS_INT) num);
        }
      else
        {
          assert ((EMACS_INT) num <= MOST_POSITIVE_FIXNUM);
          result = make_fixnum (num);
        }
    
      return result;
    }
    
  return Fsignal (Qinvalid_argument,
                  list3 (build_msg_string ("Invalid integer syntax"),
                         make_string (buf, len), make_fixnum (base)));
}

DEFUN ("parse-integer", Fparse_integer, 1, MANY, 0, /*
Parse and return the integer represented by STRING using RADIX.

START and END are bounding index designators, as used in `remove*'.  START
defaults to 0 and END defaults to nil, meaning the end of STRING.

If JUNK-ALLOWED is nil, error if STRING does not consist in its entirety of
the representation of an integer, with or without surrounding whitespace
characters.

If RADIX-TABLE is non-nil, it is a char table mapping from characters to
fixnums used with RADIX. Otherwise, `digit-fixnum-map' provides the
correspondence to use.

RADIX must always be a non-negative fixnum.  RADIX-TABLE constrains its
possible values further, and the maximum RADIX available is always the largest
positive value available RADIX-TABLE.

If RADIX-TABLE (or `digit-fixnum-map') assigns a numeric value to `-', its
digit value will be ignored if ?- is the first character in a string. The
number will be treated as negative.  To work around this, double it, or assign
another character the value zero and prefix non-negative strings with that.  A
digit value for ?+ will be respected even if it is the first character in the
representation of an integer.

arguments: (STRING &key (START 0) end (RADIX 10) junk-allowed radix-table)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object string = args[0], result;
  Charcount starting = 0, ending = MOST_POSITIVE_FIXNUM + 1, ii = 0;
  Bytecount byte_len;
  Ibyte *startp, *cursor, *end_read, *limit, *saved_start;
  EMACS_INT radixing;

  PARSE_KEYWORDS (Fparse_integer, nargs, args, 5,
                  (start, end, radix, junk_allowed, radix_table),
                  (start = Qzero, radix = make_fixnum (10)));

  CHECK_STRING (string);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start); 
  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (!NILP (radix_table))
    {
      CHECK_CHAR_TABLE (radix_table);
    }
  else
    {
      radix_table = Vdigit_fixnum_map;
    }

  check_integer_range (radix, Qzero,
                       EQ (radix_table, Vdigit_fixnum_map) ?
                       make_fixnum (XSTRING_LENGTH (Vfixnum_to_majuscule_map)
                                    / MAX_ICHAR_LEN)
                       /* Non-default radix table; calculating the upper limit
                          is is expensive. Check at least that the radix is
                          not a bignum, the maximum count of characters
                          available in our XEmacs will not exceed the size of
                          a fixnum. */
                       : make_fixnum (MOST_POSITIVE_FIXNUM));
  radixing = XFIXNUM (radix);

  startp = cursor = saved_start = XSTRING_DATA (string);
  byte_len = XSTRING_LENGTH (string);
  limit = startp + byte_len;

  while (cursor < limit && ii < ending)
    {
      INC_IBYTEPTR (cursor);
      if (ii < starting)
        {
          startp = cursor;
        }
      ii++;
    }

  if (ii < starting || (ii < ending && !NILP (end)))
    {
      check_sequence_range (string, start, end, Flength (string));
    }

  result = parse_integer (startp, &end_read, cursor - startp, radixing,
                          !NILP (junk_allowed) ? JUNK_ALLOWED : 0,
                          radix_table);

  /* This code hasn't been written to handle relocating string data. */ 
  assert (saved_start == XSTRING_DATA (string));

  return values2 (result, make_fixnum (string_index_byte_to_char
                                       (string, end_read - saved_start)));
}

DEFUN ("+", Fplus, 0, MANY, 0, /*
Return sum of any number of arguments.
The arguments should all be numbers, characters or markers.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum = make_fixnum (0), addend;

  for (i = 0; i < nargs; i++)
    {
      addend = args[i];
      switch (promote_args (&accum, &addend))
	{
	case FIXNUM_T:
	  accum = make_integer (XREALFIXNUM (accum) + XREALFIXNUM (addend));
	  break;
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  bignum_add (XBIGNUM_DATA (accum), XBIGNUM_DATA (accum),
		      XBIGNUM_DATA (addend));
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  ratio_add (XRATIO_DATA (accum), XRATIO_DATA (accum),
		     XRATIO_DATA (addend));
	  break;
#endif
	case FLOAT_T:
	  accum = make_float (XFLOAT_DATA (accum) + XFLOAT_DATA (addend));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  bigfloat_set_prec (XBIGFLOAT_DATA (accum),
			     max (XBIGFLOAT_GET_PREC (addend),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_add (XBIGFLOAT_DATA (accum), XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (addend));
	  break;
#endif
	}
    }
  return Fcanonicalize_number (accum);
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT iaccum = 0;
  Lisp_Object *args_end = args + nargs;

  while (args < args_end)
    {
      int_or_double iod;
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	iaccum += iod.c.ival;
      else
	{
	  double daccum = (double) iaccum + iod.c.dval;
	  while (args < args_end)
	    daccum += number_char_or_marker_to_double (*args++);
	  return make_float (daccum);
	}
    }

  return make_fixnum (iaccum);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("-", Fminus, 1, MANY, 0, /*
Negate number or subtract numbers, characters or markers.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum = args[0], subtrahend;

  if (nargs == 1)
    {
      if (CHARP (accum))
	accum = make_fixnum (XCHAR (accum));
      else if (MARKERP (accum))
	accum = make_fixnum (marker_position (accum));

      /* Invert the sign of accum */
      CHECK_NUMBER (accum);
      switch (get_number_type (accum))
	{
	case FIXNUM_T:
	  return make_integer (-XREALFIXNUM (accum));
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  bignum_neg (scratch_bignum, XBIGNUM_DATA (accum));
	  return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  ratio_neg (scratch_ratio, XRATIO_DATA (accum));
	  return make_ratio_rt (scratch_ratio);
#endif
	case FLOAT_T:
	  return make_float (-XFLOAT_DATA (accum));
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (accum));
	  bigfloat_neg (scratch_bigfloat, XBIGFLOAT_DATA (accum));
	  return make_bigfloat_bf (scratch_bigfloat);
#endif
	}
    }
  else
    {
      /* Subtrace the remaining arguments from accum */
      for (i = 1; i < nargs; i++)
	{
	  subtrahend = args[i];
	  switch (promote_args (&accum, &subtrahend))
	    {
	    case FIXNUM_T:
	      accum = make_integer (XREALFIXNUM (accum) - XREALFIXNUM (subtrahend));
	      break;
#ifdef HAVE_BIGNUM
	    case BIGNUM_T:
	      bignum_sub (scratch_bignum, XBIGNUM_DATA (accum),
			  XBIGNUM_DATA (subtrahend));
	      accum = make_bignum_bg (scratch_bignum);
	      break;
#endif
#ifdef HAVE_RATIO
	    case RATIO_T:
	      ratio_sub (scratch_ratio, XRATIO_DATA (accum),
			 XRATIO_DATA (subtrahend));
	      accum = make_ratio_rt (scratch_ratio);
	      break;
#endif
	    case FLOAT_T:
	      accum =
		make_float (XFLOAT_DATA (accum) - XFLOAT_DATA (subtrahend));
	      break;
#ifdef HAVE_BIGFLOAT
	    case BIGFLOAT_T:
	      bigfloat_set_prec (scratch_bigfloat,
				 max (XBIGFLOAT_GET_PREC (subtrahend),
				      XBIGFLOAT_GET_PREC (accum)));
	      bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (accum),
			    XBIGFLOAT_DATA (subtrahend));
	      accum = make_bigfloat_bf (scratch_bigfloat);
	      break;
#endif
	    }
	}
    }
  return Fcanonicalize_number (accum);
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT iaccum;
  double daccum;
  Lisp_Object *args_end = args + nargs;
  int_or_double iod;

  number_char_or_marker_to_int_or_double (*args++, &iod);
  if (iod.int_p)
    iaccum = nargs > 1 ? iod.c.ival : - iod.c.ival;
  else
    {
      daccum = nargs > 1 ? iod.c.dval : - iod.c.dval;
      goto do_float;
    }

  while (args < args_end)
    {
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	iaccum -= iod.c.ival;
      else
	{
	  daccum = (double) iaccum - iod.c.dval;
	  goto do_float;
	}
    }

  return make_fixnum (iaccum);

 do_float:
  for (; args < args_end; args++)
    daccum -= number_char_or_marker_to_double (*args);
  return make_float (daccum);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("*", Ftimes, 0, MANY, 0, /*
Return product of any number of arguments.
The arguments should all be numbers, characters or markers.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  /* Start with a bignum to avoid overflow */
  Lisp_Object accum = make_bignum (1L), multiplier;

  for (i = 0; i < nargs; i++)
    {
      multiplier = args[i];
      switch (promote_args (&accum, &multiplier))
	{
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  bignum_mul (XBIGNUM_DATA (accum), XBIGNUM_DATA (accum),
		      XBIGNUM_DATA (multiplier));
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  ratio_mul (XRATIO_DATA (accum), XRATIO_DATA (accum),
		     XRATIO_DATA (multiplier));
	  break;
#endif
	case FLOAT_T:
	  accum = make_float (XFLOAT_DATA (accum) * XFLOAT_DATA (multiplier));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  bigfloat_set_prec (XBIGFLOAT_DATA (accum),
			     max (XBIGFLOAT_GET_PREC (multiplier),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_mul (XBIGFLOAT_DATA (accum), XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (multiplier));
	  break;
#endif
	}
    }
  return Fcanonicalize_number (accum);
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT iaccum = 1;
  Lisp_Object *args_end = args + nargs;

  while (args < args_end)
    {
      int_or_double iod;
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	iaccum *= iod.c.ival;
      else
	{
	  double daccum = (double) iaccum * iod.c.dval;
	  while (args < args_end)
	    daccum *= number_char_or_marker_to_double (*args++);
	  return make_float (daccum);
	}
    }

  return make_fixnum (iaccum);
#endif /* WITH_NUMBER_TYPES */
}

#ifdef HAVE_RATIO
DEFUN ("div", Fdiv, 1, MANY, 0, /*
Same as `/', but dividing integers creates a ratio instead of truncating.
Note that this is a departure from Common Lisp, where / creates ratios when
dividing integers.  Having a separate function lets us avoid breaking existing
Emacs Lisp code that expects / to do integer division.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  REGISTER int i;
  Lisp_Object accum, divisor;

  if (nargs == 1)
    {
      i = 0;
      accum = Qone;
    }
  else
    {
      i = 1;
      accum = args[0];
    }
  for (; i < nargs; i++)
    {
      divisor = args[i];
      switch (promote_args (&accum, &divisor))
	{
	case FIXNUM_T:
	  if (XREALFIXNUM (divisor) == 0) goto divide_by_zero;
	  bignum_set_long (scratch_bignum, XREALFIXNUM (accum));
	  bignum_set_long (scratch_bignum2, XREALFIXNUM (divisor));
	  accum = make_ratio_bg (scratch_bignum, scratch_bignum2);
	  break;
	case BIGNUM_T:
	  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0) goto divide_by_zero;
	  accum = make_ratio_bg (XBIGNUM_DATA (accum), XBIGNUM_DATA (divisor));
	  break;
	case RATIO_T:
	  if (ratio_sign (XRATIO_DATA (divisor)) == 0) goto divide_by_zero;
	  ratio_div (scratch_ratio, XRATIO_DATA (accum),
		     XRATIO_DATA (divisor));
	  accum = make_ratio_rt (scratch_ratio);
	  break;
	case FLOAT_T:
	  if (XFLOAT_DATA (divisor) == 0.0) goto divide_by_zero;
	  accum = make_float (XFLOAT_DATA (accum) / XFLOAT_DATA (divisor));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
	    goto divide_by_zero;
	  bigfloat_set_prec (scratch_bigfloat,
			     max (XBIGFLOAT_GET_PREC (divisor),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (divisor));
	  accum = make_bigfloat_bf (scratch_bigfloat);
	  break;
#endif
	}
    }
  return Fcanonicalize_number (accum);

 divide_by_zero:
  Fsignal (Qarith_error, Qnil);
  return Qnil; /* not (usually) reached */
}
#endif /* HAVE_RATIO */

DEFUN ("/", Fquo, 1, MANY, 0, /*
Return FIRST divided by all the remaining arguments.
The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum, divisor;

  if (nargs == 1)
    {
      i = 0;
      accum = Qone;
    }
  else
    {
      i = 1;
      accum = args[0];
    }
  for (; i < nargs; i++)
    {
      divisor = args[i];
      switch (promote_args (&accum, &divisor))
	{
	case FIXNUM_T:
	  if (XREALFIXNUM (divisor) == 0) goto divide_by_zero;
	  accum = make_integer (XREALFIXNUM (accum) / XREALFIXNUM (divisor));
	  break;
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  if (bignum_sign (XBIGNUM_DATA (divisor)) == 0) goto divide_by_zero;
	  bignum_div (scratch_bignum, XBIGNUM_DATA (accum),
		      XBIGNUM_DATA (divisor));
	  accum = make_bignum_bg (scratch_bignum);
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  if (ratio_sign (XRATIO_DATA (divisor)) == 0) goto divide_by_zero;
	  ratio_div (scratch_ratio, XRATIO_DATA (accum),
		     XRATIO_DATA (divisor));
	  accum = make_ratio_rt (scratch_ratio);
	  break;
#endif
	case FLOAT_T:
	  if (XFLOAT_DATA (divisor) == 0.0) goto divide_by_zero;
	  accum = make_float (XFLOAT_DATA (accum) / XFLOAT_DATA (divisor));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  if (bigfloat_sign (XBIGFLOAT_DATA (divisor)) == 0)
	    goto divide_by_zero;
	  bigfloat_set_prec (scratch_bigfloat,
			     max (XBIGFLOAT_GET_PREC (divisor),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (divisor));
	  accum = make_bigfloat_bf (scratch_bigfloat);
	  break;
#endif
	}
    }
  return Fcanonicalize_number (accum);
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT iaccum;
  double daccum;
  Lisp_Object *args_end = args + nargs;
  int_or_double iod;

  if (nargs == 1)
    iaccum = 1;
  else
    {
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	iaccum = iod.c.ival;
      else
	{
	  daccum = iod.c.dval;
	  goto divide_floats;
	}
    }

  while (args < args_end)
    {
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	{
	  if (iod.c.ival == 0) goto divide_by_zero;
	  iaccum /= iod.c.ival;
	}
      else
	{
	  if (iod.c.dval == 0) goto divide_by_zero;
	  daccum = (double) iaccum / iod.c.dval;
	  goto divide_floats;
	}
    }

  return make_fixnum (iaccum);

 divide_floats:
  for (; args < args_end; args++)
    {
      double dval = number_char_or_marker_to_double (*args);
      if (dval == 0) goto divide_by_zero;
      daccum /= dval;
    }
  return make_float (daccum);
#endif /* WITH_NUMBER_TYPES */

 divide_by_zero:
  Fsignal (Qarith_error, Qnil);
  return Qnil; /* not (usually) reached */
}

DEFUN ("max", Fmax, 1, MANY, 0, /*
Return largest of all the arguments.
All arguments must be real numbers, characters or markers.

Markers in distinct buffers will be converted to fixnums, but markers in the
same buffer will be compared without conversion, and may be returned as the
value. Characters are always converted to fixnums, and are never returned.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i, maxindex = 0;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_fixnum (XCHAR (args[0]));
  for (i = 1; i < nargs; i++)
    {
      switch (promote_args_lazy (args + maxindex, args + i))
	{
        case LAZY_MARKER_T:
          if (byte_marker_position (args[maxindex])
	      < byte_marker_position (args[i]))
	    maxindex = i;
	  break;
	case LAZY_FIXNUM_T:
	  if (XREALFIXNUM (args[maxindex]) < XREALFIXNUM (args[i]))
	    maxindex = i;
	  break;
#ifdef HAVE_BIGNUM
	case LAZY_BIGNUM_T:
	  if (bignum_lt (XBIGNUM_DATA (args[maxindex]),
			 XBIGNUM_DATA (args[i])))
	    maxindex = i;
	  break;
#endif
#ifdef HAVE_RATIO
	case LAZY_RATIO_T:
	  if (ratio_lt (XRATIO_DATA (args[maxindex]), XRATIO_DATA (args[i])))
	    maxindex = i;
	  break;
#endif
	case LAZY_FLOAT_T:
	  if (XFLOAT_DATA (args[maxindex]) < XFLOAT_DATA (args[i]))
	    maxindex = i;
	  break;
#ifdef HAVE_BIGFLOAT
	case LAZY_BIGFLOAT_T:
	  if (bigfloat_lt (XBIGFLOAT_DATA (args[maxindex]),
			   XBIGFLOAT_DATA (args[i])))
	    maxindex = i;
	  break;
#endif
	}
    }
  return args[maxindex];
#else /* !WITH_NUMBER_TYPES */
  REGISTER int i, maxindex = 0;
  Lisp_Object max_so_far;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_fixnum (XCHAR (args[0]));
  max_so_far = args[0];
  for (i = 1; i < nargs; i++)
    {
      EMACS_INT ival1, ival2;
      int float_p;
      Lisp_Object obj2 = args[i]; 

 retry:
      float_p = 0;

      if (FIXNUMP (max_so_far)) ival1 = XFIXNUM (max_so_far);
      else if (CHARP (max_so_far)) ival1 = XCHAR (max_so_far);
      else if (MARKERP (max_so_far))
        {
          if (MARKERP (obj2)
              && (XMARKER (max_so_far)->buffer == XMARKER (obj2)->buffer))
            {
              if (byte_marker_position (max_so_far)
                  < byte_marker_position (obj2))
                {
                  max_so_far = obj2;
                }
              continue;
            }
	  /* Otherwise, convert to a fixnum in the normal way. */
          ival1 = marker_position (max_so_far);
        }
      else if (FLOATP (max_so_far)) ival1 = 0, float_p = 1;
      else
        {
          max_so_far = wrong_type_argument (Qnumber_char_or_marker_p,
                                            max_so_far);
          goto retry;
        }

      if (FIXNUMP (obj2)) ival2 = XFIXNUM  (obj2);
      else if (CHARP (obj2)) ival2 = XCHAR (obj2);
      else if (MARKERP (obj2)) ival2 = marker_position (obj2);
      else if (FLOATP  (obj2)) ival2 = 0, float_p = 1;
      else
        {
          obj2 = wrong_type_argument (Qnumber_char_or_marker_p, obj2);
          goto retry;
        }

      if (!float_p)
        {
          if (ival1 < ival2) max_so_far = make_fixnum (ival2);
        }
      else
        {
          double dval1 = FLOATP (max_so_far) ? XFLOAT_DATA (max_so_far)
            : (double) ival1;
          double dval2 = FLOATP (obj2) ? XFLOAT_DATA (obj2) : (double) ival2;
          if (dval1 < dval2)
            {
              max_so_far = FLOATP (obj2) ? obj2 : make_float (dval2);
            }
	}
    }

  return max_so_far;
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("min", Fmin, 1, MANY, 0, /*
Return smallest of all the arguments.
All arguments must be numbers, characters or markers.

Markers in distinct buffers will be converted to fixnums, but markers in the
same buffer will be compared without conversion, and may be returned as the
value. Characters are always converted to fixnums, and are never returned.

arguments: (FIRST &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i, minindex = 0;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_fixnum (XCHAR (args[0]));
  for (i = 1; i < nargs; i++)
    {
      switch (promote_args_lazy (args + minindex, args + i))
	{
        case LAZY_MARKER_T:
	  if (byte_marker_position (args[minindex])
              > byte_marker_position (args[i]))
	    minindex = i;
	  break;
	case LAZY_FIXNUM_T:
	  if (XREALFIXNUM (args[minindex]) > XREALFIXNUM (args[i]))
	    minindex = i;
	  break;
#ifdef HAVE_BIGNUM
	case LAZY_BIGNUM_T:
	  if (bignum_gt (XBIGNUM_DATA (args[minindex]),
			 XBIGNUM_DATA (args[i])))
	    minindex = i;
	  break;
#endif
#ifdef HAVE_RATIO
	case LAZY_RATIO_T:
	  if (ratio_gt (XRATIO_DATA (args[minindex]),
			XRATIO_DATA (args[i])))
	    minindex = i;
	  break;
#endif
	case LAZY_FLOAT_T:
	  if (XFLOAT_DATA (args[minindex]) > XFLOAT_DATA (args[i]))
	    minindex = i;
	  break;
#ifdef HAVE_BIGFLOAT
	case LAZY_BIGFLOAT_T:
	  if (bigfloat_gt (XBIGFLOAT_DATA (args[minindex]),
			   XBIGFLOAT_DATA (args[i])))
	    minindex = i;
	  break;
#endif
	}
    }
  return args[minindex];
#else /* !WITH_NUMBER_TYPES */
  REGISTER int i;
  Lisp_Object min_so_far;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_fixnum (XCHAR (args[0]));
  min_so_far = args[0];
  for (i = 1; i < nargs; i++)
    {
      EMACS_INT ival1, ival2;
      int float_p;
      Lisp_Object obj2 = args[i]; 

 retry:
      float_p = 0;

      if (FIXNUMP (min_so_far)) ival1 = XFIXNUM (min_so_far);
      else if (CHARP (min_so_far)) ival1 = XCHAR (min_so_far);
      else if (MARKERP (min_so_far))
        {
          if (MARKERP (obj2)
              && (XMARKER (min_so_far)->buffer == XMARKER (obj2)->buffer))
            {
              if (byte_marker_position (min_so_far)
                  > byte_marker_position (obj2))
                {
                  min_so_far = obj2;
                }
              continue;
            }
	  /* Otherwise, convert to a fixnum in the normal way. */
          ival1 = marker_position (min_so_far);
        }
      else if (FLOATP (min_so_far)) ival1 = 0, float_p = 1;
      else
        {
          min_so_far = wrong_type_argument (Qnumber_char_or_marker_p,
                                            min_so_far);
          goto retry;
        }

      if (FIXNUMP (obj2)) ival2 = XFIXNUM  (obj2);
      else if (CHARP (obj2)) ival2 = XCHAR (obj2);
      else if (MARKERP (obj2)) ival2 = marker_position (obj2);
      else if (FLOATP  (obj2)) ival2 = 0, float_p = 1;
      else
        {
          obj2 = wrong_type_argument (Qnumber_char_or_marker_p, obj2);
          goto retry;
        }

      if (!float_p)
        {
          if (ival1 > ival2) min_so_far = make_fixnum (ival2);
        }
      else
        {
          double dval1 = FLOATP (min_so_far) ? XFLOAT_DATA (min_so_far)
            : (double) ival1;
          double dval2 = FLOATP (obj2) ? XFLOAT_DATA (obj2) : (double) ival2;
          if (dval1 > dval2)
            {
              min_so_far = FLOATP (obj2) ? obj2 : make_float (dval2);
            }
	}
    }

  return min_so_far;
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("logand", Flogand, 0, MANY, 0, /*
Return bitwise-and of all the arguments.
Arguments may be integers, or markers or characters converted to integers.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_fixnum (~0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qinteger_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_fixnum (XCHAR (result));
  else if (MARKERP (result))
    result = make_fixnum (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qinteger_char_or_marker_p, args[i]);
      other = args[i];
      switch (promote_args (&result, &other))
	{
	case FIXNUM_T:
	  result = make_fixnum (XREALFIXNUM (result) & XREALFIXNUM (other));
	  break;
	case BIGNUM_T:
	  bignum_and (scratch_bignum, XBIGNUM_DATA (result),
		      XBIGNUM_DATA (other));
	  result = make_bignum_bg (scratch_bignum);
	  break;
	}
    }
  return Fcanonicalize_number (result);
#else /* !HAVE_BIGNUM */
  EMACS_INT bits = ~0;
  Lisp_Object *args_end = args + nargs;

  while (args < args_end)
    bits &= fixnum_char_or_marker_to_int (*args++);

  return make_fixnum (bits);
#endif /* HAVE_BIGNUM */
}

DEFUN ("logior", Flogior, 0, MANY, 0, /*
Return bitwise-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_fixnum (0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qinteger_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_fixnum (XCHAR (result));
  else if (MARKERP (result))
    result = make_fixnum (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qinteger_char_or_marker_p, args[i]);
      other = args[i];
      switch (promote_args (&result, &other))
	{
	case FIXNUM_T:
	  result = make_fixnum (XREALFIXNUM (result) | XREALFIXNUM (other));
	  break;
	case BIGNUM_T:
	  bignum_ior (scratch_bignum, XBIGNUM_DATA (result),
		      XBIGNUM_DATA (other));
	  result = make_bignum_bg (scratch_bignum);
	  break;
	}
    }
  return Fcanonicalize_number (result);
#else /* !HAVE_BIGNUM */
  EMACS_INT bits = 0;
  Lisp_Object *args_end = args + nargs;

  while (args < args_end)
    bits |= fixnum_char_or_marker_to_int (*args++);

  return make_fixnum (bits);
#endif /* HAVE_BIGNUM */
}

DEFUN ("logxor", Flogxor, 0, MANY, 0, /*
Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_fixnum (0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qinteger_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_fixnum (XCHAR (result));
  else if (MARKERP (result))
    result = make_fixnum (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qinteger_char_or_marker_p, args[i]);
      other = args[i];
      if (promote_args (&result, &other) == FIXNUM_T)
	{
	  result = make_fixnum (XREALFIXNUM (result) ^ XREALFIXNUM (other));
	}
      else
	{
	  bignum_xor (scratch_bignum, XBIGNUM_DATA (result),
		      XBIGNUM_DATA (other));
	  result = make_bignum_bg (scratch_bignum);
	}
    }
  return Fcanonicalize_number (result);
#else /* !HAVE_BIGNUM */
  EMACS_INT bits = 0;
  Lisp_Object *args_end = args + nargs;

  while (args < args_end)
    bits ^= fixnum_char_or_marker_to_int (*args++);

  return make_fixnum (bits);
#endif /* !HAVE_BIGNUM */
}

DEFUN ("lognot", Flognot, 1, 1, 0, /*
Return the bitwise complement of NUMBER.
NUMBER may be an integer, marker or character converted to integer.
*/
       (number))
{
  while (!(CHARP (number) || MARKERP (number) || INTEGERP (number)))
    number = wrong_type_argument (Qinteger_char_or_marker_p, number);

#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      bignum_not (scratch_bignum, XBIGNUM_DATA (number));
      return make_bignum_bg (scratch_bignum);
    }
#endif /* HAVE_BIGNUM */

  return make_fixnum (~ fixnum_char_or_marker_to_int (number));
}

static Lisp_Object
rem_two_fixnum (EMACS_INT number1, EMACS_INT number2)
{
  EMACS_UINT val1, val2;
  EMACS_INT sign;
  
  if (0 == number2)
    {
      Fsignal (Qarith_error, Qnil);
    }

  if (number1 < 0)
    {
      sign = -1;
      val1 = -number1;
    }
  else
    {
      sign = 1;
      val1 = number1;
    }

  val2 = EMACS_INT_ABS (number2);
      
  return make_fixnum ((EMACS_INT)(val1 % val2) * sign);
}

DEFUN ("%", Frem, 2, 2, 0, /*
Return remainder of NUMBER1 divided by NUMBER2.

Both must be integers, characters or markers.  This is the remainder in the C
sense, so the following equivalence (from the C standard) holds:

\(eql NUMBER1 (+ (* (/ NUMBER1 NUMBER2) NUMBER2) (% NUMBER1 NUMBER2)))

In this implementation, the result will have the sign of NUMBER1, something
not standardized in C.
*/
       (number1, number2))
{
#ifdef HAVE_BIGNUM
  while (!(CHARP (number1) || MARKERP (number1) || INTEGERP (number1)))
    number1 = wrong_type_argument (Qinteger_char_or_marker_p, number1);
  while (!(CHARP (number2) || MARKERP (number2) || INTEGERP (number2)))
    number2 = wrong_type_argument (Qinteger_char_or_marker_p, number2);

  if (promote_args (&number1, &number2) == FIXNUM_T)
    {
      return rem_two_fixnum (XREALFIXNUM (number1), XREALFIXNUM (number2));
    }
  else
    {
      if (bignum_sign (XBIGNUM_DATA (number2)) == 0)
        {
          Fsignal (Qarith_error, Qnil);
        }
      if (bignum_sign (XBIGNUM_DATA (number1)) > -1)
        {
          bignum_mod (scratch_bignum, XBIGNUM_DATA (number1),
                      XBIGNUM_DATA (number2));
        }
      else
        {
          bignum_neg (scratch_bignum, XBIGNUM_DATA (number1));
          bignum_mod (scratch_bignum2, scratch_bignum,
                      XBIGNUM_DATA (number2));
          bignum_neg (scratch_bignum, scratch_bignum2);
        }

      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
#else /* !HAVE_BIGNUM */
  EMACS_INT ival1 = fixnum_char_or_marker_to_int (number1);
  EMACS_INT ival2 = fixnum_char_or_marker_to_int (number2);

  return rem_two_fixnum (ival1, ival2);
#endif /* HAVE_BIGNUM */
}

/* Note, ANSI *requires* the presence of the fmod() library routine.
   If your system doesn't have it, complain to your vendor, because
   that is a bug. */

#ifndef HAVE_FMOD
double
fmod (double f1, double f2)
{
  if (f2 < 0.0)
    f2 = -f2;
  return f1 - f2 * floor (f1/f2);
}
#endif /* ! HAVE_FMOD */


DEFUN ("mod", Fmod, 2, 2, 0, /*
Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers, characters or markers.
If either argument is a float, a float will be returned.
*/
       (x, y))
{
#ifdef WITH_NUMBER_TYPES
  while (!(CHARP (x) || MARKERP (x) || REALP (x)))
    x = wrong_type_argument (Qnumber_char_or_marker_p, x);
  while (!(CHARP (y) || MARKERP (y) || REALP (y)))
    y = wrong_type_argument (Qnumber_char_or_marker_p, y);
  switch (promote_args (&x, &y))
    {
    case FIXNUM_T:
      {
	EMACS_INT ival;
	if (XREALFIXNUM (y) == 0) goto divide_by_zero;
	ival = XREALFIXNUM (x) % XREALFIXNUM (y);
	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XREALFIXNUM (y) < 0 ? ival > 0 : ival < 0)
	  ival += XREALFIXNUM (y);
	return make_fixnum (ival);
      }
#ifdef HAVE_BIGNUM
    case BIGNUM_T:
      if (bignum_sign (XBIGNUM_DATA (y)) == 0) goto divide_by_zero;
      bignum_mod (scratch_bignum, XBIGNUM_DATA (x), XBIGNUM_DATA (y));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#endif
#ifdef HAVE_RATIO
    case RATIO_T:
      if (ratio_sign (XRATIO_DATA (y)) == 0) goto divide_by_zero;
      ratio_div (scratch_ratio, XRATIO_DATA (x), XRATIO_DATA (y));
      bignum_div (scratch_bignum, ratio_numerator (scratch_ratio),
		  ratio_denominator (scratch_ratio));
      ratio_set_bignum (scratch_ratio, scratch_bignum);
      ratio_mul (scratch_ratio, scratch_ratio, XRATIO_DATA (y));
      ratio_sub (scratch_ratio, XRATIO_DATA (x), scratch_ratio);
      return Fcanonicalize_number (make_ratio_rt (scratch_ratio));
#endif
    case FLOAT_T:
    {
      double dval;
      if (XFLOAT_DATA (y) == 0.0) goto divide_by_zero;
      dval = fmod (XFLOAT_DATA (x), XFLOAT_DATA (y));
      /* If the "remainder" comes out with the wrong sign, fix it.  */
      if (XFLOAT_DATA (y) < 0 ? dval > 0 : dval < 0)
	dval += XFLOAT_DATA (y);
      return make_float (dval);
    }
#ifdef HAVE_BIGFLOAT
    case BIGFLOAT_T:
      bigfloat_set_prec (scratch_bigfloat,
			 max (XBIGFLOAT_GET_PREC (x), XBIGFLOAT_GET_PREC (y)));
      bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (x), XBIGFLOAT_DATA (y));
      bigfloat_trunc (scratch_bigfloat, scratch_bigfloat);
      bigfloat_mul (scratch_bigfloat, scratch_bigfloat, XBIGFLOAT_DATA (y));
      bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (x), scratch_bigfloat);
      return make_bigfloat_bf (scratch_bigfloat);
#endif
    }
#else /* !WITH_NUMBER_TYPES */
  int_or_double iod1, iod2;
  number_char_or_marker_to_int_or_double (x, &iod1);
  number_char_or_marker_to_int_or_double (y, &iod2);

  if (!iod1.int_p || !iod2.int_p)
    {
      double dval1 = iod1.int_p ? (double) iod1.c.ival : iod1.c.dval;
      double dval2 = iod2.int_p ? (double) iod2.c.ival : iod2.c.dval;
      if (dval2 == 0) goto divide_by_zero;
      dval1 = fmod (dval1, dval2);

      /* If the "remainder" comes out with the wrong sign, fix it.  */
      if (dval2 < 0 ? dval1 > 0 : dval1 < 0)
	dval1 += dval2;

      return make_float (dval1);
    }

  {
    EMACS_INT ival;
    if (iod2.c.ival == 0) goto divide_by_zero;

    ival = iod1.c.ival % iod2.c.ival;

    /* If the "remainder" comes out with the wrong sign, fix it.  */
    if (iod2.c.ival < 0 ? ival > 0 : ival < 0)
      ival += iod2.c.ival;

    return make_fixnum (ival);
  }
#endif /* WITH_NUMBER_TYPES */

 divide_by_zero:
  Fsignal (Qarith_error, Qnil);
  return Qnil; /* not (usually) reached */
}

DEFUN ("ash", Fash, 2, 2, 0, /*
Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, the sign bit is duplicated.
This function cannot be applied to bignums, as there is no leftmost sign bit
to be duplicated.  Use `lsh' instead.
*/
       (value, count))
{
  CHECK_FIXNUM_COERCE_CHAR (value);
  CONCHECK_FIXNUM (count);

  return make_fixnum (XFIXNUM (count) > 0 ?
		   XFIXNUM (value) <<  XFIXNUM (count) :
		   XFIXNUM (value) >> -XFIXNUM (count));
}

DEFUN ("lsh", Flsh, 2, 2, 0, /*
Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, zeros are shifted in on the left.
*/
       (value, count))
{
#ifdef HAVE_BIGNUM
  while (!(CHARP (value) || MARKERP (value) || INTEGERP (value)))
    wrong_type_argument (Qinteger_char_or_marker_p, value);
  CONCHECK_INTEGER (count);

  if (promote_args (&value, &count) == FIXNUM_T)
    {
      if (XREALFIXNUM (count) <= 0)
	return make_fixnum (XREALFIXNUM (value) >> -XREALFIXNUM (count));
      /* Use bignums to avoid overflow */
      bignum_set_long (scratch_bignum2, XREALFIXNUM (value));
      bignum_lshift (scratch_bignum, scratch_bignum2, XREALFIXNUM (count));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
  else
    {
      if (bignum_sign (XBIGNUM_DATA (count)) <= 0)
	{
	  bignum_neg (scratch_bignum, XBIGNUM_DATA (count));
          /* Sigh, this won't catch all overflows in the MPZ type under GMP,
             and there's no way to hook into the library so that an overflow
             errors rather than aborting. See
             http://mid.xemacs.org/5529.2096.e5823.ccba@parhasard.net . */
	  if (!bignum_fits_ulong_p (scratch_bignum))
            {
              args_out_of_range_3 (count,
				   make_bignum_ll (- (long long)(ULONG_MAX)),
                                   make_bignum_ull (ULONG_MAX));
            }
	  bignum_rshift (scratch_bignum2, XBIGNUM_DATA (value),
			 bignum_to_ulong (scratch_bignum));
	}
      else
	{
          /* See above re overflow. */
	  if (!bignum_fits_ulong_p (XBIGNUM_DATA (count)))
            {
              args_out_of_range_3 (count,
				   make_bignum_ll (- (long long) (ULONG_MAX)),
                                   make_bignum_ull (ULONG_MAX));
            }
	  bignum_lshift (scratch_bignum2, XBIGNUM_DATA (value),
			 bignum_to_ulong (XBIGNUM_DATA (count)));
	}
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum2));
    }
#else /* !HAVE_BIGNUM */
  CHECK_FIXNUM_COERCE_CHAR (value);
  CONCHECK_FIXNUM (count);

  return make_fixnum (XFIXNUM (count) > 0 ?
		   XUINT (value) <<  XFIXNUM (count) :
		   XUINT (value) >> -XFIXNUM (count));
#endif /* HAVE_BIGNUM */
}

DEFUN ("1+", Fadd1, 1, 1, 0, /*
Return NUMBER plus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
*/
       (number))
{
 retry:

  if (FIXNUMP    (number)) return make_integer (XFIXNUM (number) + 1);
  if (CHARP   (number)) return make_integer ((EMACS_INT) XCHAR (number) + 1);
  if (MARKERP (number)) return make_integer (marker_position (number) + 1);
  if (FLOATP  (number)) return make_float (XFLOAT_DATA (number) + 1.0);
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      bignum_set_long (scratch_bignum, 1L);
      bignum_add (scratch_bignum2, XBIGNUM_DATA (number), scratch_bignum);
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum2));
    }
#endif
#ifdef HAVE_RATIO
  if (RATIOP (number))
    {
      ratio_set_long (scratch_ratio, 1L);
      ratio_add (scratch_ratio, XRATIO_DATA (number), scratch_ratio);
      /* No need to canonicalize after adding 1 */
      return make_ratio_rt (scratch_ratio);
    }
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
      bigfloat_set_long (scratch_bigfloat, 1L);
      bigfloat_add (scratch_bigfloat, XBIGFLOAT_DATA (number),
		    scratch_bigfloat);
      return make_bigfloat_bf (scratch_bigfloat);
    }
#endif

  number = wrong_type_argument (Qnumber_char_or_marker_p, number);
  goto retry;
}

DEFUN ("1-", Fsub1, 1, 1, 0, /*
Return NUMBER minus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
*/
       (number))
{
 retry:

  if (FIXNUMP    (number)) return make_integer (XFIXNUM (number) - 1);
  if (CHARP   (number)) return make_integer ((EMACS_INT) XCHAR (number) - 1);
  if (MARKERP (number)) return make_integer (marker_position (number) - 1);
  if (FLOATP  (number)) return make_float (XFLOAT_DATA (number) - 1.0);
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      bignum_set_long (scratch_bignum, 1L);
      bignum_sub (scratch_bignum2, XBIGNUM_DATA (number), scratch_bignum);
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum2));
    }
#endif
#ifdef HAVE_RATIO
  if (RATIOP (number))
    {
      ratio_set_long (scratch_ratio, 1L);
      ratio_sub (scratch_ratio, XRATIO_DATA (number), scratch_ratio);
      /* No need to canonicalize after subtracting 1 */
      return make_ratio_rt (scratch_ratio);
    }
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      bigfloat_set_prec (scratch_bigfloat, XBIGFLOAT_GET_PREC (number));
      bigfloat_set_long (scratch_bigfloat, 1L);
      bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (number),
		    scratch_bigfloat);
      return make_bigfloat_bf (scratch_bigfloat);
    }
#endif

  number = wrong_type_argument (Qnumber_char_or_marker_p, number);
  goto retry;
}


/************************************************************************/
/*                              weak lists                              */
/************************************************************************/

/* A weak list is like a normal list except that elements automatically
   disappear when no longer in use, i.e. when no longer GC-protected.
   The basic idea is that we don't mark the elements during GC, but
   wait for them to be marked elsewhere.  If they're not marked, we
   remove them.  This is analogous to weak hash tables; see the explanation
   there for more info. */

Lisp_Object Vall_weak_lists; /* Gemarke es nicht!!! */

static Lisp_Object encode_weak_list_type (enum weak_list_type type);

static Lisp_Object
mark_weak_list (Lisp_Object UNUSED (obj))
{
  return Qnil; /* nichts ist gemarkt */
}

static void
print_weak_list (Lisp_Object obj, Lisp_Object printcharfun,
		 int escapeflag)
{
  if (print_readably)
    {
      printing_unreadable_lisp_object (obj, 0);
    }

  write_ascstring (printcharfun, "#<weak-list :type ");
  print_internal (encode_weak_list_type (XWEAK_LIST (obj)->type),
                  printcharfun, escapeflag);
  write_ascstring (printcharfun, " :list ");
  print_internal (XWEAK_LIST (obj)->list, printcharfun, escapeflag);
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static int
weak_list_equal (Lisp_Object obj1, Lisp_Object obj2, int depth, int foldcase)
{
  struct weak_list *w1 = XWEAK_LIST (obj1);
  struct weak_list *w2 = XWEAK_LIST (obj2);

  return ((w1->type == w2->type) &&
	  internal_equal_0 (w1->list, w2->list, depth + 1, foldcase));
}

static Hashcode
weak_list_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  struct weak_list *w = XWEAK_LIST (obj);

  return HASH2 ((Hashcode) w->type,
		internal_hash (w->list, depth + 1, equalp));
}

Lisp_Object
make_weak_list (enum weak_list_type type)
{
  Lisp_Object result = ALLOC_NORMAL_LISP_OBJECT (weak_list);
  struct weak_list *wl = XWEAK_LIST (result);

  wl->list = Qnil;
  wl->type = type;
  wl->next_weak = Vall_weak_lists;
  Vall_weak_lists = result;
  return result;
}

static const struct memory_description weak_list_description[] = {
  { XD_LISP_OBJECT, offsetof (struct weak_list, list), 
    0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_LO_LINK,     offsetof (struct weak_list, next_weak), 
    0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_DUMPABLE_LISP_OBJECT ("weak-list", weak_list,
			     mark_weak_list, print_weak_list,
			     0, weak_list_equal, weak_list_hash,
			     weak_list_description,
			     struct weak_list);
/*
   -- we do not mark the list elements (either the elements themselves
      or the cons cells that hold them) in the normal marking phase.
   -- at the end of marking, we go through all weak lists that are
      marked, and mark the cons cells that hold all marked
      objects, and possibly parts of the objects themselves.
      (See alloc.c, "after-mark".)
   -- after that, we prune away all the cons cells that are not marked.

   WARNING WARNING WARNING WARNING WARNING:

   The code in the following two functions is *unbelievably* tricky.
   Don't mess with it.  You'll be sorry.

   Linked lists just majorly suck, d'ya know?
*/

int
finish_marking_weak_lists (void)
{
  Lisp_Object rest;
  int did_mark = 0;

  for (rest = Vall_weak_lists;
       !NILP (rest);
       rest = XWEAK_LIST (rest)->next_weak)
    {
      Lisp_Object rest2;
      enum weak_list_type type = XWEAK_LIST (rest)->type;

      if (! marked_p (rest))
	/* The weak list is probably garbage.  Ignore it. */
	continue;

      for (rest2 = XWEAK_LIST (rest)->list;
	   /* We need to be trickier since we're inside of GC;
	      use CONSP instead of !NILP in case of user-visible
	      imperfect lists */
	   CONSP (rest2);
	   rest2 = XCDR (rest2))
	{
	  Lisp_Object elem;
	  /* If the element is "marked" (meaning depends on the type
	     of weak list), we need to mark the cons containing the
	     element, and maybe the element itself (if only some part
	     was already marked). */
	  int need_to_mark_cons = 0;
	  int need_to_mark_elem = 0;

	  /* If a cons is already marked, then its car is already marked
	     (either because of an external pointer or because of
	     a previous call to this function), and likewise for all
	     the rest of the elements in the list, so we can stop now. */
	  if (marked_p (rest2))
	    break;

	  elem = XCAR (rest2);

	  switch (type)
	    {
	    case WEAK_LIST_SIMPLE:
	      if (marked_p (elem))
		need_to_mark_cons = 1;
	      break;

	    case WEAK_LIST_ASSOC:
	      if (!CONSP (elem))
		{
		  /* just leave bogus elements there */
		  need_to_mark_cons = 1;
		  need_to_mark_elem = 1;
		}
	      else if (marked_p (XCAR (elem)) &&
		       marked_p (XCDR (elem)))
		{
		  need_to_mark_cons = 1;
		  /* We still need to mark elem, because it's
		     probably not marked. */
		  need_to_mark_elem = 1;
		}
	      break;

	    case WEAK_LIST_KEY_ASSOC:
	      if (!CONSP (elem))
		{
		  /* just leave bogus elements there */
		  need_to_mark_cons = 1;
		  need_to_mark_elem = 1;
		}
	      else if (marked_p (XCAR (elem)))
		{
		  need_to_mark_cons = 1;
		  /* We still need to mark elem and XCDR (elem);
		     marking elem does both */
		  need_to_mark_elem = 1;
		}
	      break;

	    case WEAK_LIST_VALUE_ASSOC:
	      if (!CONSP (elem))
		{
		  /* just leave bogus elements there */
		  need_to_mark_cons = 1;
		  need_to_mark_elem = 1;
		}
	      else if (marked_p (XCDR (elem)))
		{
		  need_to_mark_cons = 1;
		  /* We still need to mark elem and XCAR (elem);
		     marking elem does both */
		  need_to_mark_elem = 1;
		}
	      break;

	    case WEAK_LIST_FULL_ASSOC:
	      if (!CONSP (elem))
		{
		  /* just leave bogus elements there */
		  need_to_mark_cons = 1;
		  need_to_mark_elem = 1;
		}
	      else if (marked_p (XCAR (elem)) ||
		       marked_p (XCDR (elem)))
		{
		  need_to_mark_cons = 1;
		  /* We still need to mark elem and XCAR (elem);
		     marking elem does both */
		  need_to_mark_elem = 1;
		}
	      break;

	    default:
	      ABORT ();
	    }

	  if (need_to_mark_elem && ! marked_p (elem))
	    {
#ifdef USE_KKCC
	      kkcc_gc_stack_push_lisp_object_0 (elem);
#else /* NOT USE_KKCC */
	      mark_object (elem);
#endif /* NOT USE_KKCC */
	      did_mark = 1;
	    }

	  /* We also need to mark the cons that holds the elem or
	     assoc-pair.  We do *not* want to call (mark_object) here
	     because that will mark the entire list; we just want to
	     mark the cons itself.
	     */
	  if (need_to_mark_cons)
	    {
	      Lisp_Cons *c = XCONS (rest2);
	      if (!CONS_MARKED_P (c))
		{
		  MARK_CONS (c);
		  did_mark = 1;
		}
	    }
	}

      /* In case of imperfect list, need to mark the final cons
         because we're not removing it */
      if (!NILP (rest2) && ! marked_p (rest2))
	{
#ifdef USE_KKCC
	  kkcc_gc_stack_push_lisp_object_0 (rest2);
#else /* NOT USE_KKCC */
	  mark_object (rest2);
#endif /* NOT USE_KKCC */
	  did_mark = 1;
	}
    }

  return did_mark;
}

void
prune_weak_lists (void)
{
  Lisp_Object rest, prev = Qnil;

  for (rest = Vall_weak_lists;
       !NILP (rest);
       rest = XWEAK_LIST (rest)->next_weak)
    {
      if (! (marked_p (rest)))
	{
	  /* This weak list itself is garbage.  Remove it from the list. */
	  if (NILP (prev))
	    Vall_weak_lists = XWEAK_LIST (rest)->next_weak;
	  else
	    XWEAK_LIST (prev)->next_weak =
	      XWEAK_LIST (rest)->next_weak;
	}
      else
	{
	  Lisp_Object rest2, prev2 = Qnil;
	  Lisp_Object tortoise;
	  int go_tortoise = 0;

          for (rest2 = XWEAK_LIST (rest)->list, tortoise = rest2;
	       /* We need to be trickier since we're inside of GC;
		  use CONSP instead of !NILP in case of user-visible
		  imperfect lists */
	       CONSP (rest2);)
	    {
	      /* It suffices to check the cons for marking,
		 regardless of the type of weak list:

		 -- if the cons is pointed to somewhere else,
		    then it should stay around and will be marked.
		 -- otherwise, if it should stay around, it will
		    have been marked in finish_marking_weak_lists().
		 -- otherwise, it's not marked and should disappear.
		 */
	      if (! marked_p (rest2))
		{
		  /* bye bye :-( */
		  if (NILP (prev2))
		    XWEAK_LIST (rest)->list = XCDR (rest2);
		  else
		    XCDR (prev2) = XCDR (rest2);
		  rest2 = XCDR (rest2);
		  /* Ouch.  Circularity checking is even trickier
		     than I thought.  When we cut out a link
		     like this, we can't advance the turtle or
		     it'll catch up to us.  Imagine that we're
		     standing on floor tiles and moving forward --
		     what we just did here is as if the floor
		     tile under us just disappeared and all the
		     ones ahead of us slid one tile towards us.
		     In other words, we didn't move at all;
		     if the tortoise was one step behind us
		     previously, it still is, and therefore
		     it must not move. */
		}
	      else
		{
		  prev2 = rest2;

		  /* Implementing circularity checking is trickier here
		     than in other places because we have to guarantee
		     that we've processed all elements before exiting
		     due to a circularity. (In most places, an error
		     is issued upon encountering a circularity, so it
		     doesn't really matter if all elements are processed.)
		     The idea is that we process along with the hare
		     rather than the tortoise.  If at any point in
		     our forward process we encounter the tortoise,
		     we must have already visited the spot, so we exit.
		     (If we process with the tortoise, we can fail to
		     process cases where a cons points to itself, or
		     where cons A points to cons B, which points to
		     cons A.) */

		  rest2 = XCDR (rest2);
		  if (go_tortoise)
		    tortoise = XCDR (tortoise);
		  go_tortoise = !go_tortoise;
		  if (EQ (rest2, tortoise))
		    break;
		}
	    }

	  prev = rest;
	}
    }
}

static enum weak_list_type
decode_weak_list_type (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (EQ (symbol, Qsimple))	 return WEAK_LIST_SIMPLE;
  if (EQ (symbol, Qassoc))	 return WEAK_LIST_ASSOC;
  if (EQ (symbol, Qold_assoc))	 return WEAK_LIST_ASSOC;  /* EBOLA ALERT! */
  if (EQ (symbol, Qkey_assoc))	 return WEAK_LIST_KEY_ASSOC;
  if (EQ (symbol, Qvalue_assoc)) return WEAK_LIST_VALUE_ASSOC;
  if (EQ (symbol, Qfull_assoc))  return WEAK_LIST_FULL_ASSOC;

  invalid_constant ("Invalid weak list type", symbol);
  RETURN_NOT_REACHED (WEAK_LIST_SIMPLE);
}

static Lisp_Object
encode_weak_list_type (enum weak_list_type type)
{
  switch (type)
    {
    case WEAK_LIST_SIMPLE:      return Qsimple;
    case WEAK_LIST_ASSOC:       return Qassoc;
    case WEAK_LIST_KEY_ASSOC:   return Qkey_assoc;
    case WEAK_LIST_VALUE_ASSOC: return Qvalue_assoc;
    case WEAK_LIST_FULL_ASSOC:  return Qfull_assoc;
    default:
      ABORT ();
    }

  return Qnil; /* not (usually) reached */
}

DEFUN ("weak-list-p", Fweak_list_p, 1, 1, 0, /*
Return non-nil if OBJECT is a weak list.
*/
       (object))
{
  return WEAK_LISTP (object) ? Qt : Qnil;
}

DEFUN ("make-weak-list", Fmake_weak_list, 0, 1, 0, /*
Return a new weak list object of type TYPE.
A weak list object is an object that contains a list.  This list behaves
like any other list except that its elements do not count towards
garbage collection -- if the only pointer to an object is inside a weak
list (other than pointers in similar objects such as weak hash tables),
the object is garbage collected and automatically removed from the list.
This is used internally, for example, to manage the list holding the
children of an extent -- an extent that is unused but has a parent will
still be reclaimed, and will automatically be removed from its parent's
list of children.

Optional argument TYPE specifies the type of the weak list, and defaults
to `simple'.  Recognized types are

`simple'	Objects in the list disappear if not pointed to.
`assoc'		Objects in the list disappear if they are conses
		and either the car or the cdr of the cons is not
		pointed to.
`key-assoc'	Objects in the list disappear if they are conses
		and the car is not pointed to.
`value-assoc'	Objects in the list disappear if they are conses
		and the cdr is not pointed to.
`full-assoc'	Objects in the list disappear if they are conses
		and neither the car nor the cdr is pointed to.
*/
       (type))
{
  if (NILP (type))
    type = Qsimple;

  return make_weak_list (decode_weak_list_type (type));
}

DEFUN ("weak-list-type", Fweak_list_type, 1, 1, 0, /*
Return the type of the given weak-list object.
*/
       (weak))
{
  CHECK_WEAK_LIST (weak);
  return encode_weak_list_type (XWEAK_LIST (weak)->type);
}

DEFUN ("weak-list-list", Fweak_list_list, 1, 1, 0, /*
Return the list contained in a weak-list object.
*/
       (weak))
{
  CHECK_WEAK_LIST (weak);
  return XWEAK_LIST_LIST (weak);
}

DEFUN ("set-weak-list-list", Fset_weak_list_list, 2, 2, 0, /*
Change the list contained in a weak-list object.
*/
       (weak, new_list))
{
  CHECK_WEAK_LIST (weak);
  XWEAK_LIST_LIST (weak) = new_list;
  return new_list;
}

/************************************************************************/
/*                              ephemerons                              */
/************************************************************************/

/* The concept of ephemerons is due to:
 * Barry Hayes: Ephemerons: A New Finalization Mechanism. OOPSLA 1997: 176-183
 * The original idea is due to George Bosworth of Digitalk, Inc.
 *
 * For a discussion of finalization and weakness that also reviews
 * ephemerons, refer to:
 * Simon Peyton Jones, Simon Marlow, Conal Elliot:
 * Stretching the storage manager
 * Implementation of Functional Languages, 1999
 */

static Lisp_Object Vall_ephemerons; /* Gemarke es niemals ever!!! */
static Lisp_Object Vnew_all_ephemerons;
static Lisp_Object Vfinalize_list;

void
init_marking_ephemerons (void)
{
  Vnew_all_ephemerons = Qnil;
}

/* Move all live ephemerons with live keys over to
 * Vnew_all_ephemerons, marking the values and finalizers along the
 * way. */

int
continue_marking_ephemerons (void)
{
  Lisp_Object rest = Vall_ephemerons, next, prev = Qnil;
  int did_mark = 0;

  while (!NILP (rest))
    {
      next = XEPHEMERON_NEXT (rest);

      if (marked_p (rest))
	{
	  MARK_CONS (XCONS (XEPHEMERON (rest)->cons_chain));
	  if (marked_p (XEPHEMERON (rest)->key))
	    {
#ifdef USE_KKCC
	      kkcc_gc_stack_push_lisp_object_0 
		(XCAR (XEPHEMERON (rest)->cons_chain));
#else /* NOT USE_KKCC */
	      mark_object (XCAR (XEPHEMERON (rest)->cons_chain));
#endif /* NOT USE_KKCC */
	      did_mark = 1;
	      XSET_EPHEMERON_NEXT (rest, Vnew_all_ephemerons);
	      Vnew_all_ephemerons = rest;
	      if (NILP (prev))
		Vall_ephemerons = next;
	      else
		XSET_EPHEMERON_NEXT (prev, next);
	    }
	  else
	    prev = rest;
	}
      else
	prev = rest;

      rest = next;
    }

  return did_mark;
}

/* At this point, everything that's in Vall_ephemerons is dead.
 * Well, almost: we still need to run the finalizers, so we need to
 * resurrect them.
 */

int
finish_marking_ephemerons (void)
{
  Lisp_Object rest = Vall_ephemerons, next, prev = Qnil;
  int did_mark = 0;

  while (! NILP (rest))
    {
      next = XEPHEMERON_NEXT (rest);

      if (marked_p (rest))
	/* The ephemeron itself is live, but its key is garbage */
	{
	  /* tombstone */
	  XSET_EPHEMERON_VALUE (rest, Qnil);

	  if (! NILP (XEPHEMERON_FINALIZER (rest)))
	    {
	      MARK_CONS (XCONS (XEPHEMERON (rest)->cons_chain));
#ifdef USE_KKCC
	      kkcc_gc_stack_push_lisp_object_0
		(XCAR (XEPHEMERON (rest)->cons_chain));
#else /* NOT USE_KKCC */
	      mark_object (XCAR (XEPHEMERON (rest)->cons_chain));
#endif /* NOT USE_KKCC */

	      /* Register the finalizer */
	      XSET_EPHEMERON_NEXT (rest, Vfinalize_list);
	      Vfinalize_list = XEPHEMERON (rest)->cons_chain;
	      did_mark = 1;
	    }

	  /* Remove it from the list. */
	  if (NILP (prev))
	    Vall_ephemerons = next;
	  else
	    XSET_EPHEMERON_NEXT (prev, next);
	}
      else
	prev = rest;

      rest = next;
    }
  
  return did_mark;
}

void
prune_ephemerons (void)
{
  Vall_ephemerons = Vnew_all_ephemerons;
}

Lisp_Object
zap_finalize_list (void)
{
  Lisp_Object finalizers = Vfinalize_list;

  Vfinalize_list = Qnil;

  return finalizers;
}

static Lisp_Object
mark_ephemeron (Lisp_Object UNUSED (obj))
{
  return Qnil;
}

static void
print_ephemeron (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    {
      printing_unreadable_lisp_object (obj, 0);
    }

  write_ascstring (printcharfun, "#<ephemeron :key ");
  print_internal (XEPHEMERON (obj)->key, printcharfun, escapeflag);
  write_ascstring (printcharfun, " :value ");
  print_internal (XEPHEMERON (obj)->value, printcharfun, escapeflag);
  write_ascstring (printcharfun, " :finalizer ");
  print_internal (XEPHEMERON_FINALIZER (obj), printcharfun, escapeflag);
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static int
ephemeron_equal (Lisp_Object obj1, Lisp_Object obj2, int depth, int foldcase)
{
  return
    internal_equal_0 (XEPHEMERON_REF (obj1), XEPHEMERON_REF (obj2),
                      depth + 1, foldcase);
}

static Hashcode
ephemeron_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  return internal_hash (XEPHEMERON_REF (obj), depth + 1, equalp);
}

Lisp_Object
make_ephemeron (Lisp_Object key, Lisp_Object value, Lisp_Object finalizer)
{
  Lisp_Object temp = Qnil;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object result = ALLOC_NORMAL_LISP_OBJECT (ephemeron);
  struct ephemeron *eph = XEPHEMERON (result);

  eph->key = Qnil;
  eph->cons_chain = Qnil;
  eph->value = Qnil;

  result = wrap_ephemeron (eph);
  GCPRO2 (result, temp);

  eph->key = key;
  temp = Fcons (value, finalizer);
  eph->cons_chain = Fcons (temp, Vall_ephemerons);
  eph->value = value;

  Vall_ephemerons = result;

  UNGCPRO;
  return result;
}

/* Ephemerons are special cases in the KKCC mark algorithm, so nothing
   is marked here. */
static const struct memory_description ephemeron_description[] = {
  { XD_LISP_OBJECT, offsetof (struct ephemeron, key),
    0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof (struct ephemeron, cons_chain),
    0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof (struct ephemeron, value),
    0, { 0 }, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_NODUMP_LISP_OBJECT ("ephemeron", ephemeron,
			   mark_ephemeron, print_ephemeron,
			   0, ephemeron_equal, ephemeron_hash,
			   ephemeron_description,
			   struct ephemeron);

DEFUN ("make-ephemeron", Fmake_ephemeron, 2, 3, 0, /*
Return a new ephemeron with key KEY, value VALUE, and finalizer FINALIZER.
The ephemeron is a reference to VALUE which may be extracted with
`ephemeron-ref'.  VALUE is only reachable through the ephemeron as
long as KEY is reachable; the ephemeron does not contribute to the
reachability of KEY.  When KEY becomes unreachable while the ephemeron
itself is still reachable, VALUE is queued for finalization: FINALIZER
will possibly be called on VALUE some time in the future.  Moreover,
future calls to `ephemeron-ref' will return NIL.
*/
       (key, value, finalizer))
{
  return make_ephemeron (key, value, finalizer);
}

DEFUN ("ephemeron-ref",  Fephemeron_ref, 1, 1, 0, /*
Return the contents of ephemeron EPHEMERON.
If the contents have been GCed, return NIL.
*/
       (ephemeron))
{
  return XEPHEMERON_REF (ephemeron);
}

DEFUN ("ephemeron-p", Fephemeronp, 1, 1, 0, /*
Return non-nil if OBJECT is an ephemeron.
*/
       (object))
{
  return EPHEMERONP (object) ? Qt : Qnil;
}

/****************** Converting to and from specific C types ******************/

#ifdef HAVE_BIGNUM
#define LISP_INTEGER_TO_C_TYPE(c_type, objekt)                          \
  if (INTEGERP (objekt))                                                \
    do                                                                  \
      {                                                                 \
        check_integer_range (objekt, make_integer (min_lisp_to_c_type), \
                             make_integer (max_lisp_to_c_type));        \
        if (FIXNUMP (objekt))                                           \
          {                                                             \
            return (c_type) XREALFIXNUM (objekt);                       \
          }                                                             \
        else if (BIGNUMP (objekt))                                      \
          {                                                             \
            if (sizeof (c_type) >= SIZEOF_EMACS_INT &&                  \
                bignum_fits_emacs_int_p (XBIGNUM_DATA (objekt)))        \
              {                                                         \
                return (c_type) bignum_to_emacs_int (XBIGNUM_DATA       \
                                                    (objekt));          \
              }                                                         \
                                                                        \
            if (sizeof (c_type) >= SIZEOF_EMACS_INT &&                  \
                (c_type) (-1) > 0 &&                                    \
                bignum_fits_emacs_uint_p (XBIGNUM_DATA (objekt)))       \
              {                                                         \
                return (c_type) bignum_to_emacs_uint (XBIGNUM_DATA      \
                                                    (objekt));          \
              }                                                         \
                                                                        \
            if (sizeof (c_type) >= sizeof (long long) &&                \
                bignum_fits_llong_p (XBIGNUM_DATA (objekt)))            \
              {                                                         \
                return (c_type) bignum_to_llong (XBIGNUM_DATA (objekt)); \
              }                                                         \
                                                                        \
            if (sizeof (c_type) >= sizeof (long long) &&                \
                (c_type) (-1) > 0 &&                                    \
                bignum_fits_ullong_p (XBIGNUM_DATA (objekt)))           \
              {                                                         \
                return (c_type) bignum_to_ullong (XBIGNUM_DATA (objekt)); \
              }                                                         \
                                                                        \
            signal_error (Qunimplemented,                               \
                          "cannot decode this " #c_type,                \
                          objekt);                                      \
            RETURN_NOT_REACHED ((c_type) -1);                           \
          }                                                             \
      } while (0)
#define C_TYPE_TO_LISP_INTEGER(c_type, value)\
  return make_integer (value)
#else
#define LISP_INTEGER_TO_C_TYPE(c_type, objekt)        \
  if (FIXNUMP (objekt))                                                 \
    do                                                                  \
      {                                                                 \
        EMACS_INT ival = XREALFIXNUM (objekt);                          \
                                                                        \
        if (((c_type)(-1) > 0 && ival < 0) ||                           \
            (sizeof (c_type) >= sizeof (EMACS_INT) ?                    \
             ((c_type) ival < min_lisp_to_c_type ||                     \
              (c_type) ival > max_lisp_to_c_type) :                     \
             (ival < (EMACS_INT) min_lisp_to_c_type ||                  \
              ival > (EMACS_INT) max_lisp_to_c_type)))                  \
          {                                                             \
            args_out_of_range_3 (objekt,                                \
                                 make_float (min_lisp_to_c_type),       \
                                 make_float (max_lisp_to_c_type));      \
          }                                                             \
                                                                        \
        return (c_type) ival;                                           \
      } while (0)

#define C_TYPE_TO_LISP_INTEGER(c_type, value)                           \
  if (NUMBER_FITS_IN_A_FIXNUM (value))                                  \
    {                                                                   \
      return make_fixnum (value);                                       \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      Lisp_Object result = Fcons (make_fixnum (value & 0xFFFF), Qnil);  \
      Boolint negative = value < 0;                                     \
                                                                        \
      /* Only the most significant 16 bits will be negative in the      \
	 constructed cons. */                                           \
      value = (value >> 16);                                            \
      if (negative)                                                     \
	{                                                               \
	  value = -value;                                               \
	}                                                               \
                                                                        \
      while (value)                                                     \
	{                                                               \
	  result = Fcons (make_fixnum (value & 0xFFFF), result);        \
	  value = value >> 16;                                          \
	}                                                               \
                                                                        \
      if (negative)                                                     \
	{                                                               \
	  XSETCAR (result, make_fixnum (- (XFIXNUM (XCAR (result)))));  \
	}                                                               \
                                                                        \
      return result;                                                    \
    }                                                                   \
  RETURN_NOT_REACHED ((c_type)-1)
#endif

#define DEFINE_C_INTEGER_TYPE_LISP_CONVERSION(visibility, c_type)      \
  visibility c_type                                                     \
  lisp_to_##c_type (Lisp_Object objeto)                                 \
  {                                                                     \
    c_type min_lisp_to_c_type, max_lisp_to_c_type, result = 0;          \
    double dval;                                                        \
                                                                        \
    if (((c_type) -1) < 0) /* Signed type? */                           \
      {                                                                 \
        if (sizeof (c_type) == SIZEOF_SHORT)                            \
          {                                                             \
            max_lisp_to_c_type = (c_type) ((unsigned short) -1) / 2;    \
            min_lisp_to_c_type                                          \
              = (c_type) ((unsigned short)(max_lisp_to_c_type) + 1);    \
          }                                                             \
        else if (sizeof (c_type) == SIZEOF_INT)                         \
          {                                                             \
            max_lisp_to_c_type = (c_type) ((unsigned int) -1) / 2;      \
            min_lisp_to_c_type                                          \
              = (c_type) ((unsigned int)(max_lisp_to_c_type) + 1);      \
          }                                                             \
        else if (sizeof (c_type) == SIZEOF_LONG)                        \
          {                                                             \
            max_lisp_to_c_type = (c_type) (((unsigned long) -1) / 2);   \
            min_lisp_to_c_type                                          \
              = (c_type) ((unsigned long)(max_lisp_to_c_type) + 1);     \
          }                                                             \
        else if (sizeof (c_type) == SIZEOF_LONG_LONG)                   \
          {                                                             \
            max_lisp_to_c_type                                          \
              = (c_type) (((unsigned long long) -1) / 2);               \
            min_lisp_to_c_type                                          \
              = (c_type) ((unsigned long long)(max_lisp_to_c_type) + 1); \
          }                                                             \
        else                                                            \
          {                                                             \
            assert (0); /* Very very very unlikely. */                  \
          }                                                             \
      }                                                                 \
    else                                                                \
      {                                                                 \
        min_lisp_to_c_type = 0;                                         \
        max_lisp_to_c_type = (c_type)(-1);                              \
      }                                                                 \
                                                                        \
    LISP_INTEGER_TO_C_TYPE (c_type, objeto);                            \
                                                                        \
    if (CONSP (objeto))                                                 \
      {                                                                 \
        unsigned counter = 1;                                           \
        Lisp_Object orig = objeto;                                      \
                                                                        \
        if ((c_type)-1 < 0)                                             \
          {                                                             \
            check_integer_range (XCAR (objeto), make_fixnum (-32768),   \
                                 make_fixnum (32767));                  \
          }                                                             \
        else                                                            \
          {                                                             \
            check_integer_range (XCAR (objeto), Qzero,                  \
                                 make_fixnum (65535));                  \
          }                                                             \
                                                                        \
        result = XFIXNUM (XCAR (objeto));                               \
        objeto = XCDR (objeto);                                         \
                                                                        \
        while (CONSP (objeto))                                          \
          {                                                             \
            check_integer_range (XCAR (objeto), Qzero,                  \
                                 make_fixnum (65535));                  \
            counter++;                                                  \
            if (counter > sizeof (c_type) / 2)                          \
              {                                                         \
                invalid_argument ("Too many bits supplied "             \
                                  "for " #c_type,                       \
                                  orig);                                \
              }                                                         \
            result                                                      \
              = (result << 16) | (XFIXNUM (XCAR (objeto)) & 0xFFFF);    \
            objeto = XCDR (objeto);                                     \
          }                                                             \
                                                                        \
        return result;                                                  \
      }                                                                 \
                                                                        \
    dval = extract_float (objeto);                                      \
    result = dval;                                                      \
                                                                        \
    if (result < min_lisp_to_c_type || result > max_lisp_to_c_type)     \
      {                                                                 \
        args_out_of_range_3 (objeto, make_float (min_lisp_to_c_type),   \
                             make_float (max_lisp_to_c_type));          \
      }                                                                 \
                                                                        \
    if (dval != result)                                                 \
      {                                                                 \
        invalid_argument ("Fractional or two wide " #c_type,            \
                          objeto);                                      \
      }                                                                 \
                                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  visibility Lisp_Object                                                \
  c_type##_to_lisp (c_type value)                                       \
  {                                                                     \
    C_TYPE_TO_LISP_INTEGER (c_type, value);                             \
  }                                                                     \
  visibility Lisp_Object c_type##_to_lisp (c_type)

/* Definitions for lisp_to_OFF_T, OFF_T_to_lisp: */
DEFINE_C_INTEGER_TYPE_LISP_CONVERSION (extern, OFF_T); 

/* Definitions for lisp_to_uid_t, uid_t_to_lisp: */
DEFINE_C_INTEGER_TYPE_LISP_CONVERSION (extern, uid_t);

/* Definitions for lisp_to_uid_t, uid_t_to_lisp: */
DEFINE_C_INTEGER_TYPE_LISP_CONVERSION (extern, gid_t);

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

static SIGTYPE
arith_error (int signo)
{
  EMACS_REESTABLISH_SIGNAL (signo, arith_error);
  EMACS_UNBLOCK_SIGNAL (signo);
  signal_error (Qarith_error, 0, Qunbound);
}

void
init_data_very_early (void)
{
  /* Don't do this if just dumping out.
     We don't want to call `signal' in this case
     so that we don't have trouble with dumping
     signal-delivering routines in an inconsistent state.  */
  if (!initialized)
    return;
  EMACS_SIGNAL (SIGFPE, arith_error);
#ifdef uts
  EMACS_SIGNAL (SIGEMT, arith_error);
#endif /* uts */
}

void
init_errors_once_early (void)
{
  DEFSYMBOL (Qerror_conditions);
  DEFSYMBOL (Qerror_message);

  /* We declare the errors here because some other deferrors depend
     on some of the errors below. */

  /* ERROR is used as a signaler for random errors for which nothing
     else is right */

  DEFERROR (Qerror, "error", Qnil);
  DEFERROR_STANDARD (Qquit, Qnil);

  DEFERROR_STANDARD (Qinvalid_argument, Qerror);

  DEFERROR_STANDARD (Qsyntax_error, Qinvalid_argument);
  DEFERROR_STANDARD (Qinvalid_read_syntax, Qsyntax_error);
  DEFERROR_STANDARD (Qstructure_formation_error, Qsyntax_error);
  DEFERROR_STANDARD (Qlist_formation_error, Qstructure_formation_error);
  DEFERROR_STANDARD (Qmalformed_list, Qlist_formation_error);
  DEFERROR_STANDARD (Qmalformed_property_list, Qmalformed_list);
  DEFERROR_STANDARD (Qcircular_list, Qlist_formation_error);
  DEFERROR_STANDARD (Qcircular_property_list, Qcircular_list);

  DEFERROR_STANDARD (Qwrong_type_argument, Qinvalid_argument);
  DEFERROR_STANDARD (Qargs_out_of_range, Qinvalid_argument);
  DEFERROR_STANDARD (Qwrong_number_of_arguments, Qinvalid_argument);
  DEFERROR_STANDARD (Qinvalid_function, Qinvalid_argument);
  DEFERROR_STANDARD (Qinvalid_constant, Qinvalid_argument);
  DEFERROR_STANDARD (Qinvalid_keyword_argument, Qinvalid_argument);
  DEFERROR (Qno_catch, "No catch for tag", Qinvalid_argument);

  DEFERROR_STANDARD (Qinvalid_state, Qerror);
  DEFERROR (Qvoid_function, "Symbol's function definition is void",
	    Qinvalid_state);
  DEFERROR (Qcyclic_function_indirection,
	    "Symbol's chain of function indirections contains a loop",
	    Qinvalid_state);
  DEFERROR (Qvoid_variable, "Symbol's value as variable is void",
	    Qinvalid_state);
  DEFERROR (Qcyclic_variable_indirection,
	    "Symbol's chain of variable indirections contains a loop",
	    Qinvalid_state);
  DEFERROR_STANDARD (Qstack_overflow, Qinvalid_state);
  DEFERROR_STANDARD (Qinternal_error, Qinvalid_state);
  DEFERROR_STANDARD (Qout_of_memory, Qinvalid_state);

  DEFERROR_STANDARD (Qinvalid_operation, Qerror);
  DEFERROR_STANDARD (Qinvalid_change, Qinvalid_operation);
  DEFERROR (Qsetting_constant, "Attempt to set a constant symbol",
	    Qinvalid_change);
  DEFERROR_STANDARD (Qprinting_unreadable_object, Qinvalid_operation);
  DEFERROR (Qunimplemented, "Feature not yet implemented", Qinvalid_operation);

  DEFERROR_STANDARD (Qediting_error, Qinvalid_operation);
  DEFERROR_STANDARD (Qbeginning_of_buffer, Qediting_error);
  DEFERROR_STANDARD (Qend_of_buffer, Qediting_error);
  DEFERROR (Qbuffer_read_only, "Buffer is read-only", Qediting_error);
  DEFERROR (Qextent_read_only, "Extent is read-only", Qediting_error);

  DEFERROR (Qio_error, "IO Error", Qinvalid_operation);
  DEFERROR_STANDARD (Qfile_error, Qio_error);
  DEFERROR (Qend_of_file, "End of file or stream", Qfile_error);
  /* #### It's questionable whether conversion-error should be a subclass
     of io-error or directly of invalid-operation */
  DEFERROR_STANDARD (Qconversion_error, Qio_error);
  DEFERROR_STANDARD (Qtext_conversion_error, Qconversion_error);

  DEFERROR (Qarith_error, "Arithmetic error", Qinvalid_operation);
  DEFERROR (Qrange_error, "Arithmetic range error", Qarith_error);
  DEFERROR (Qdomain_error, "Arithmetic domain error", Qarith_error);
  DEFERROR (Qsingularity_error, "Arithmetic singularity error", Qdomain_error);
  DEFERROR (Qoverflow_error, "Arithmetic overflow error", Qdomain_error);
  DEFERROR (Qunderflow_error, "Arithmetic underflow error", Qdomain_error);

  /* Moved here from number.c, so it's available when none of the new numeric
     types are. */
  DEFERROR_STANDARD (Qunsupported_type, Qwrong_type_argument);
}

void
syms_of_data (void)
{
  INIT_LISP_OBJECT (weak_list);
  INIT_LISP_OBJECT (ephemeron);

  DEFSYMBOL (Qlambda);
  DEFSYMBOL (Qlistp);
  DEFSYMBOL (Qconsp);
  DEFSYMBOL (Qsubrp);
  DEFSYMBOL (Qsymbolp);
  DEFSYMBOL (Qintegerp);
  DEFSYMBOL (Qcharacterp);
  DEFSYMBOL (Qnatnump);
  DEFSYMBOL (Qnonnegativep);
  DEFSYMBOL (Qstringp);
  DEFSYMBOL (Qarrayp);
  DEFSYMBOL (Qsequencep);
  DEFSYMBOL (Qbufferp);
  DEFSYMBOL (Qbitp);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qbit_vectorp);
  DEFSYMBOL (Qvectorp);
  DEFSYMBOL (Qchar_or_string_p);
  DEFSYMBOL (Qmarkerp);
  DEFSYMBOL (Qinteger_or_marker_p);
  DEFSYMBOL (Qinteger_or_char_p);
  DEFSYMBOL (Qinteger_char_or_marker_p);
  DEFSYMBOL (Qnumberp);
  DEFSYMBOL (Qnumber_char_or_marker_p);
  DEFSYMBOL (Qcdr);
  DEFSYMBOL (Qerror_lacks_explanatory_string);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qweak_listp);
  DEFSYMBOL (Qfloatp);

  DEFKEYWORD (Q_radix);
  DEFKEYWORD (Q_junk_allowed);
  DEFKEYWORD (Q_radix_table);

  DEFSUBR (Fwrong_type_argument);

#ifdef HAVE_RATIO
  DEFSUBR (Fdiv);
#endif
  DEFSUBR (Feq);
  DEFSUBR (Fconsp);
  DEFSUBR (Fcharacterp);
  DEFSUBR (Fchar_to_int);
  DEFSUBR (Fint_to_char);
  DEFSUBR (Ffixnump);
  DEFSUBR (Fintegerp);
  DEFSUBR (Fnumberp);
  DEFSUBR (Ffloatp);
  DEFSUBR (Fnatnump);
  DEFSUBR (Fnonnegativep);
  DEFSUBR (Fsymbolp);
  DEFSUBR (Fkeywordp);
  DEFSUBR (Fstringp);
  DEFSUBR (Fvectorp);
  DEFSUBR (Fbit_vector_p);
  DEFSUBR (Farrayp);
  DEFSUBR (Fsequencep);
  DEFSUBR (Fmarkerp);
  DEFSUBR (Fsubrp);
  DEFSUBR (Fsubr_min_args);
  DEFSUBR (Fsubr_max_args);
  DEFSUBR (Fsubr_interactive);
  DEFSUBR (Ftype_of);
  DEFSUBR (Fcar);
  DEFSUBR (Fcdr);
  DEFSUBR (Fcar_safe);
  DEFSUBR (Fcdr_safe);
  DEFSUBR (Fsetcar);
  DEFSUBR (Fsetcdr);
  DEFSUBR (Findirect_function);
  DEFSUBR (Faref);
  DEFSUBR (Faset);

  DEFSUBR (Fstring_to_number);
  DEFSUBR (Fset_digit_fixnum_map);
  DEFSUBR (Fdigit_char_p);
  DEFSUBR (Fdigit_char);
  DEFSUBR (Fparse_integer);
  DEFSUBR (Feqlsign);
  DEFSUBR (Flss);
  DEFSUBR (Fgtr);
  DEFSUBR (Fleq);
  DEFSUBR (Fgeq);
  DEFSUBR (Fneq);
  DEFSUBR (Fplus);
  DEFSUBR (Fminus);
  DEFSUBR (Ftimes);
  DEFSUBR (Fquo);
  DEFSUBR (Frem);
  DEFSUBR (Fmod);
  DEFSUBR (Fmax);
  DEFSUBR (Fmin);
  DEFSUBR (Flogand);
  DEFSUBR (Flogior);
  DEFSUBR (Flogxor);
  DEFSUBR (Flsh);
  DEFSUBR (Fash);
  DEFSUBR (Fadd1);
  DEFSUBR (Fsub1);
  DEFSUBR (Flognot);

  DEFSUBR (Fweak_list_p);
  DEFSUBR (Fmake_weak_list);
  DEFSUBR (Fweak_list_type);
  DEFSUBR (Fweak_list_list);
  DEFSUBR (Fset_weak_list_list);

  DEFSUBR (Fmake_ephemeron);
  DEFSUBR (Fephemeron_ref);
  DEFSUBR (Fephemeronp);
}

void
vars_of_data (void)
{
  DUMP_ADD_WEAK_OBJECT_CHAIN (Vall_weak_lists);

  DUMP_ADD_WEAK_OBJECT_CHAIN (Vall_ephemerons);

  Vfinalize_list = Qnil;
  staticpro (&Vfinalize_list);

  DEFVAR_CONST_INT ("most-negative-fixnum", &Vmost_negative_fixnum /*
The fixnum closest in value to negative infinity.
*/);
  Vmost_negative_fixnum = MOST_NEGATIVE_FIXNUM;

  DEFVAR_CONST_INT ("most-positive-fixnum", &Vmost_positive_fixnum /*
The fixnum closest in value to positive infinity.
*/);
  Vmost_positive_fixnum = MOST_POSITIVE_FIXNUM;

  DEFVAR_CONST_LISP ("digit-fixnum-ascii", &Vdigit_fixnum_ascii /*
Version of `digit-fixnum-map' supporting only ASCII digits.

See the documentation for that variable, and for `parse-integer',
`digit-char-p', and `digit-char'.  `digit-fixnum-ascii' is most useful for
parsing text formats defined to support only ASCII digits.
*/);
  Vdigit_fixnum_ascii = Fmake_char_table (Qgeneric);
  set_char_table_default (Vdigit_fixnum_ascii, make_fixnum (-1));
  {
    int ii = 0;

    for (ii = 0; ii < 10; ++ii)
      {
        put_char_table (Vdigit_fixnum_ascii, '0' + ii, '0' + ii,
                        make_fixnum (ii));
      }

    for (ii = 10; ii < 36; ++ii)
      {
        put_char_table (Vdigit_fixnum_ascii, 'a' + (ii - 10),
                        'a' + (ii - 10), make_fixnum (ii));
        put_char_table (Vdigit_fixnum_ascii, 'A' + (ii - 10),
                        'A' + (ii - 10), make_fixnum (ii));
      }
  }
  LISP_READONLY (Vdigit_fixnum_ascii) = 1;

  DEFVAR_CONST_LISP ("digit-fixnum-map", &Vdigit_fixnum_map /*
Table used to determine a character's numeric value when parsing.

This is a character table with fixnum values. A value of -1 indicates this
character does not have an assigned numeric value. See `parse-integer',
`digit-char-p', and `digit-char'.
*/);
  Vdigit_fixnum_map = Fcopy_char_table (Vdigit_fixnum_ascii);
  {
    Ascbyte *fixnum_tab = alloca_ascbytes (36 * MAX_ICHAR_LEN), *ptr;
    int ii;
    Ichar cc;
    memset ((void *)fixnum_tab, 0, 36 * MAX_ICHAR_LEN);

    /* The whole point of fixnum_to_character_table is access as an array,
       avoid O(N) issues by giving every character MAX_ICHAR_LEN of
       bytes.  */
    for (ii = 0, ptr = fixnum_tab; ii < 36; ++ii, ptr += MAX_ICHAR_LEN)
      {
	cc = ii < 10 ? '0' + ii : 'A' + (ii - 10);
	(void) set_itext_ichar ((Ibyte *) ptr, cc);
      }

    /* Sigh, we can't call build_fixnum_to_char_map() on Vdigit_fixnum_map,
       this is too early in the boot sequence to map across a char table. Do
       it by hand. */
    ASSERT_ASCTEXT_ASCII_LEN (fixnum_tab, 36 * MAX_ICHAR_LEN);
    Vfixnum_to_majuscule_map
      = make_string ((const Ibyte*) fixnum_tab, 36 * MAX_ICHAR_LEN);
    staticpro (&Vfixnum_to_majuscule_map);

    /* For those occasional times we don't want localised numbers. */
    Vfixnum_to_majuscule_ascii
      = make_string ((const Ibyte*) fixnum_tab, 36 * MAX_ICHAR_LEN);
    staticpro (&Vfixnum_to_majuscule_ascii);

    memset ((void *)fixnum_tab, 0, 36 * MAX_ICHAR_LEN);

    for (ii = 0, ptr = fixnum_tab; ii < 36; ++ii, ptr += MAX_ICHAR_LEN)
      {
	cc = ii < 10 ? '0' + ii : 'a' + (ii - 10);
	(void) set_itext_ichar ((Ibyte *) ptr, cc);
      }
    Vfixnum_to_minuscule_map 
      = make_string ((const Ibyte*) fixnum_tab, 36 * MAX_ICHAR_LEN);
    staticpro (&Vfixnum_to_minuscule_map);
  }

#ifdef DEBUG_XEMACS
  DEFVAR_BOOL ("debug-issue-ebola-notices", &debug_issue_ebola_notices /*
If non-zero, note when your code may be suffering from char-int confoundance.
That is to say, if XEmacs encounters a usage of `eq', `memq', `equal',
etc. where an int and a char with the same value are being compared,
it will issue a notice on stderr to this effect, along with a backtrace.
In such situations, the result would be different in XEmacs 19 versus
XEmacs 20, and you probably don't want this.

Note that in order to see these notices, you have to byte compile your
code under XEmacs 20 -- any code byte-compiled under XEmacs 19 will
have its chars and ints all confounded in the byte code, making it
impossible to accurately determine Ebola infection.
*/ );

  debug_issue_ebola_notices = 0;

  DEFVAR_INT ("debug-ebola-backtrace-length",
	      &debug_ebola_backtrace_length /*
Length (in stack frames) of short backtrace printed out in Ebola notices.
See `debug-issue-ebola-notices'.
*/ );
  debug_ebola_backtrace_length = 32;

#endif /* DEBUG_XEMACS */
}
