/* Primitive operations on Lisp data types for XEmacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30.  Some of FSF's data.c is in
   XEmacs' symbols.c. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "syssignal.h"
#include "sysfloat.h"

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message;
Lisp_Object Qerror, Qquit, Qsyntax_error, Qinvalid_read_syntax;
Lisp_Object Qlist_formation_error, Qstructure_formation_error;
Lisp_Object Qmalformed_list, Qmalformed_property_list;
Lisp_Object Qcircular_list, Qcircular_property_list;
Lisp_Object Qinvalid_argument, Qinvalid_constant, Qwrong_type_argument;
Lisp_Object Qargs_out_of_range;
Lisp_Object Qwrong_number_of_arguments, Qinvalid_function, Qno_catch;
Lisp_Object Qinternal_error, Qinvalid_state, Qstack_overflow, Qout_of_memory;
Lisp_Object Qvoid_variable, Qcyclic_variable_indirection;
Lisp_Object Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qinvalid_operation, Qinvalid_change, Qprinting_unreadable_object;
Lisp_Object Qsetting_constant;
Lisp_Object Qediting_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qio_error, Qfile_error, Qconversion_error, Qend_of_file;
Lisp_Object Qtext_conversion_error;
Lisp_Object Qarith_error, Qrange_error, Qdomain_error;
Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
Lisp_Object Qintegerp, Qnatnump, Qnonnegativep, Qsymbolp;
Lisp_Object Qlistp, Qtrue_list_p, Qweak_listp;
Lisp_Object Qconsp, Qsubrp;
Lisp_Object Qcharacterp, Qstringp, Qarrayp, Qsequencep, Qvectorp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qbufferp;
Lisp_Object Qinteger_or_char_p, Qinteger_char_or_marker_p;
Lisp_Object Qnumberp, Qnumber_char_or_marker_p;
Lisp_Object Qbit_vectorp, Qbitp, Qcdr;

Lisp_Object Qerror_lacks_explanatory_string;
Lisp_Object Qfloatp;

#ifdef DEBUG_XEMACS

int debug_issue_ebola_notices;

Fixnum debug_ebola_backtrace_length;

int
eq_with_ebola_notice (Lisp_Object obj1, Lisp_Object obj2)
{
  if (debug_issue_ebola_notices
      && ((CHARP (obj1) && INTP (obj2)) || (CHARP (obj2) && INTP (obj1))))
    {
      /* #### It would be really nice if this were a proper warning
         instead of brain-dead print to Qexternal_debugging_output.  */
      write_c_string
	(Qexternal_debugging_output,
	 "Comparison between integer and character is constant nil (");
      Fprinc (obj1, Qexternal_debugging_output);
      write_c_string (Qexternal_debugging_output, " and ");
      Fprinc (obj2, Qexternal_debugging_output);
      write_c_string (Qexternal_debugging_output, ")\n");
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
check_int_range (EMACS_INT val, EMACS_INT min, EMACS_INT max)
{
  if (val < min || val > max)
    args_out_of_range_3 (make_int (val), make_int (min), make_int (max));
}

/* On some machines, XINT needs a temporary location.
   Here it is, in case it is needed.  */

EMACS_INT sign_extend_temp;

/* On a few machines, XINT can only be done by calling this.  */
/* XEmacs:  only used by m/convex.h */
EMACS_INT sign_extend_lisp_int (EMACS_INT num);
EMACS_INT
sign_extend_lisp_int (EMACS_INT num)
{
  if (num & (1L << (VALBITS - 1)))
    return num | ((-1L) << VALBITS);
  else
    return num & ((1L << VALBITS) - 1);
}


/* Data type predicates */

DEFUN ("eq", Feq, 2, 2, 0, /*
Return t if the two args are the same Lisp object.
*/
       (object1, object2))
{
  return EQ_WITH_EBOLA_NOTICE (object1, object2) ? Qt : Qnil;
}

DEFUN ("old-eq", Fold_eq, 2, 2, 0, /*
Return t if the two args are (in most cases) the same Lisp object.

Special kludge: A character is considered `old-eq' to its equivalent integer
even though they are not the same object and are in fact of different
types.  This is ABSOLUTELY AND UTTERLY HORRENDOUS but is necessary to
preserve byte-code compatibility with v19.  This kludge is known as the
\"char-int confoundance disease\" and appears in a number of other
functions with `old-foo' equivalents.

Do not use this function!
*/
       (object1, object2))
{
  /* #### blasphemy */
  return HACKEQ_UNSAFE (object1, object2) ? Qt : Qnil;
}

DEFUN ("null", Fnull, 1, 1, 0, /*
Return t if OBJECT is nil.
*/
       (object))
{
  return NILP (object) ? Qt : Qnil;
}

DEFUN ("consp", Fconsp, 1, 1, 0, /*
Return t if OBJECT is a cons cell.  `nil' is not a cons cell.
*/
       (object))
{
  return CONSP (object) ? Qt : Qnil;
}

DEFUN ("atom", Fatom, 1, 1, 0, /*
Return t if OBJECT is not a cons cell.  `nil' is not a cons cell.
*/
       (object))
{
  return CONSP (object) ? Qnil : Qt;
}

DEFUN ("listp", Flistp, 1, 1, 0, /*
Return t if OBJECT is a list.  `nil' is a list.
*/
       (object))
{
  return LISTP (object) ? Qt : Qnil;
}

DEFUN ("nlistp", Fnlistp, 1, 1, 0, /*
Return t if OBJECT is not a list.  `nil' is a list.
*/
       (object))
{
  return LISTP (object) ? Qnil : Qt;
}

DEFUN ("true-list-p", Ftrue_list_p, 1, 1, 0, /*
Return t if OBJECT is an acyclic, nil-terminated (ie, not dotted), list.
*/
       (object))
{
  return TRUE_LIST_P (object) ? Qt : Qnil;
}

DEFUN ("symbolp", Fsymbolp, 1, 1, 0, /*
Return t if OBJECT is a symbol.
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
  return (VECTORP	(object) ||
	  STRINGP	(object) ||
	  BIT_VECTORP	(object))
    ? Qt : Qnil;
}

DEFUN ("sequencep", Fsequencep, 1, 1, 0, /*
Return t if OBJECT is a sequence (list or array).
*/
       (object))
{
  return (LISTP		(object) ||
	  VECTORP	(object) ||
	  STRINGP	(object) ||
	  BIT_VECTORP	(object))
    ? Qt : Qnil;
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
  return make_int (XSUBR (subr)->min_args);
}

DEFUN ("subr-max-args", Fsubr_max_args, 1, 1, 0, /*
Return maximum number of args built-in function SUBR may be called with,
or nil if it takes an arbitrary number of arguments or is a special form.
*/
       (subr))
{
  int nargs;
  CHECK_SUBR (subr);
  nargs = XSUBR (subr)->max_args;
  if (nargs == MANY || nargs == UNEVALLED)
    return Qnil;
  else
    return make_int (nargs);
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
  return prompt ? list2 (Qinteractive, build_msg_string (prompt)) : Qnil;
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

If support for Mule does not exist, these are the only valid character
values.  When Mule support exists, the values assigned to other characters
may vary depending on the particular version of XEmacs, the order in which
character sets were loaded, etc., and you should not depend on them.
*/
       (character))
{
  CHECK_CHAR (character);
  return make_int (XCHAR (character));
}

DEFUN ("int-to-char", Fint_to_char, 1, 1, 0, /*
Convert integer INTEGER into the equivalent character.
Not all integers correspond to valid characters; use `char-int-p' to
determine whether this is the case.  If the integer cannot be converted,
nil is returned.
*/
       (integer))
{
  CHECK_INT (integer);
  if (CHAR_INTP (integer))
    return make_char (XINT (integer));
  else
    return Qnil;
}

DEFUN ("char-int-p", Fchar_int_p, 1, 1, 0, /*
Return t if OBJECT is an integer that can be converted into a character.
See `char-int'.
*/
       (object))
{
  return CHAR_INTP (object) ? Qt : Qnil;
}

DEFUN ("char-or-char-int-p", Fchar_or_char_int_p, 1, 1, 0, /*
Return t if OBJECT is a character or an integer that can be converted into one.
*/
       (object))
{
  return CHAR_OR_CHAR_INTP (object) ? Qt : Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, 1, 1, 0, /*
Return t if OBJECT is a character (or a char-int) or a string.
It is semi-hateful that we allow a char-int here, as it goes against
the name of this function, but it makes the most sense considering the
other steps we take to maintain compatibility with the old character/integer
confoundedness in older versions of E-Lisp.
*/
       (object))
{
  return CHAR_OR_CHAR_INTP (object) || STRINGP (object) ? Qt : Qnil;
}

#ifdef HAVE_BIGNUM
/* In this case, integerp is defined in number.c. */
DEFUN ("fixnump", Ffixnump, 1, 1, 0, /*
Return t if OBJECT is a fixnum.
*/
       (object))
{
  return INTP (object) ? Qt : Qnil;
}
#else
DEFUN ("integerp", Fintegerp, 1, 1, 0, /*
Return t if OBJECT is an integer.
*/
       (object))
{
  return INTP (object) ? Qt : Qnil;
}
#endif

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, 1, 1, 0, /*
Return t if OBJECT is an integer or a marker (editor pointer).
*/
       (object))
{
  return INTP (object) || MARKERP (object) ? Qt : Qnil;
}

DEFUN ("integer-or-char-p", Finteger_or_char_p, 1, 1, 0, /*
Return t if OBJECT is an integer or a character.
*/
       (object))
{
  return INTP (object) || CHARP (object) ? Qt : Qnil;
}

DEFUN ("integer-char-or-marker-p", Finteger_char_or_marker_p, 1, 1, 0, /*
Return t if OBJECT is an integer, character or a marker (editor pointer).
*/
       (object))
{
  return INTP (object) || CHARP (object) || MARKERP (object) ? Qt : Qnil;
}

DEFUN ("natnump", Fnatnump, 1, 1, 0, /*
Return t if OBJECT is a nonnegative integer.
*/
       (object))
{
  return NATNUMP (object)
#ifdef HAVE_BIGNUM
    || (BIGNUMP (object) && bignum_sign (XBIGNUM_DATA (object)) >= 0)
#endif
    ? Qt : Qnil;
}

DEFUN ("nonnegativep", Fnonnegativep, 1, 1, 0, /*
Return t if OBJECT is a nonnegative number.
*/
       (object))
{
  return NATNUMP (object)
#ifdef HAVE_BIGNUM
    || (BIGNUMP (object) && bignum_sign (XBIGNUM_DATA (object)) >= 0)
#endif
#ifdef HAVE_RATIO
    || (RATIOP (object) && ratio_sign (XRATIO_DATA (object)) >= 0)
#endif
#ifdef HAVE_BIGFLOAT
    || (BIGFLOATP (object) && bigfloat_sign (XBIGFLOAT_DATA (object)) >= 0)
#endif
    ? Qt : Qnil;
}

DEFUN ("bitp", Fbitp, 1, 1, 0, /*
Return t if OBJECT is a bit (0 or 1).
*/
       (object))
{
  return BITP (object) ? Qt : Qnil;
}

DEFUN ("numberp", Fnumberp, 1, 1, 0, /*
Return t if OBJECT is a number (floating point or integer).
*/
       (object))
{
#ifdef WITH_NUMBER_TYPES
  return NUMBERP (object) ? Qt : Qnil;
#else
  return INT_OR_FLOATP (object) ? Qt : Qnil;
#endif
}

DEFUN ("number-or-marker-p", Fnumber_or_marker_p, 1, 1, 0, /*
Return t if OBJECT is a number or a marker.
*/
       (object))
{
  return INT_OR_FLOATP (object) || MARKERP (object) ? Qt : Qnil;
}

DEFUN ("number-char-or-marker-p", Fnumber_char_or_marker_p, 1, 1, 0, /*
Return t if OBJECT is a number, character or a marker.
*/
       (object))
{
  return (INT_OR_FLOATP (object) ||
	  CHARP         (object) ||
	  MARKERP       (object))
    ? Qt : Qnil;
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

    default: return Qinteger;
    }
}


/* Extract and set components of lists */

DEFUN ("car", Fcar, 1, 1, 0, /*
Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.
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
Return the cdr of LIST.  If arg is nil, return nil.
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

  if      (INTP  (index_)) idx = XINT  (index_);
  else if (CHARP (index_)) idx = XCHAR (index_); /* yuck! */
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
      return make_int (bit_vector_bit (XBIT_VECTOR (array), idx));
    }
  else if (STRINGP (array))
    {
      if (idx >= string_char_length (array)) goto range_error;
      return make_char (string_ichar (array, idx));
    }
#ifdef LOSING_BYTECODE
  else if (COMPILED_FUNCTIONP (array))
    {
      /* Weird, gross compatibility kludge */
      return Felt (array, index_);
    }
#endif
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

  if      (INTP  (index_)) idx = XINT (index_);
  else if (CHARP (index_)) idx = XCHAR (index_); /* yuck! */
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
      if (idx >= string_char_length (array)) goto range_error;
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
  if      (INTP    (obj)) p->c.ival = XINT  (obj);
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
  if      (INTP    (obj)) return (double) XINT  (obj);
  else if (CHARP   (obj)) return (double) XCHAR (obj);
  else if (MARKERP (obj)) return (double) marker_position (obj);
  else if (FLOATP  (obj)) return XFLOAT_DATA (obj);
  else
    {
      obj = wrong_type_argument (Qnumber_char_or_marker_p, obj);
      goto retry;
    }
}

static EMACS_INT
integer_char_or_marker_to_int (Lisp_Object obj)
{
 retry:
  if      (INTP    (obj)) return XINT  (obj);
  else if (CHARP   (obj)) return XCHAR (obj);
  else if (MARKERP (obj)) return marker_position (obj);
  else
    {
      obj = wrong_type_argument (Qinteger_char_or_marker_p, obj);
      goto retry;
    }
}

#ifdef WITH_NUMBER_TYPES

#ifdef HAVE_BIGNUM
#define BIGNUM_CASE(op)							\
	case BIGNUM_T:							\
	  if (!bignum_##op (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2)))	\
	    return Qnil;						\
	  break;
#else
#define BIGNUM_CASE(op)
#endif /* HAVE_BIGNUM */

#ifdef HAVE_RATIO
#define RATIO_CASE(op)							\
	case RATIO_T:							\
	  if (!ratio_##op (XRATIO_DATA (obj1), XRATIO_DATA (obj2)))	\
	    return Qnil;						\
	  break;
#else
#define RATIO_CASE(op)
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGFLOAT
#define BIGFLOAT_CASE(op)						\
	case BIGFLOAT_T:						\
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
      switch (promote_args (&obj1, &obj2))			\
	{							\
	case FIXNUM_T:						\
	  if (!(XREALINT (obj1) c_op XREALINT (obj2)))		\
	    return Qnil;					\
	  break;						\
	BIGNUM_CASE (op)					\
	RATIO_CASE (op)						\
	case FLOAT_T:						\
	  if (!(XFLOAT_DATA (obj1) c_op XFLOAT_DATA (obj2)))	\
	    return Qnil;					\
	  break;						\
	BIGFLOAT_CASE (op)					\
	}							\
    }								\
  return Qt;							\
}
#else /* !WITH_NUMBER_TYPES */
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
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (==, eql)
}

DEFUN ("<", Flss, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically increasing.
The arguments may be numbers, characters or markers.
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (<, lt)
}

DEFUN (">", Fgtr, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically decreasing.
The arguments may be numbers, characters or markers.
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (>, gt)
}

DEFUN ("<=", Fleq, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically nondecreasing.
The arguments may be numbers, characters or markers.
*/
       (int nargs, Lisp_Object *args))
{
  ARITHCOMPARE_MANY (<=, le)
}

DEFUN (">=", Fgeq, 1, MANY, 0, /*
Return t if the sequence of arguments is monotonically nonincreasing.
The arguments may be numbers, characters or markers.
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
	      if (XREALINT (obj1) == XREALINT (obj2))
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

DEFUN ("zerop", Fzerop, 1, 1, 0, /*
Return t if NUMBER is zero.
*/
       (number))
{
 retry:
  if (INTP (number))
    return EQ (number, Qzero) ? Qt : Qnil;
#ifdef HAVE_BIGNUM
  else if (BIGNUMP (number))
    return bignum_sign (XBIGNUM_DATA (number)) == 0 ? Qt : Qnil;
#endif
#ifdef HAVE_RATIO
  else if (RATIOP (number))
    return ratio_sign (XRATIO_DATA (number)) == 0 ? Qt : Qnil;
#endif
  else if (FLOATP (number))
    return XFLOAT_DATA (number) == 0.0 ? Qt : Qnil;
#ifdef HAVE_BIGFLOAT
  else if (BIGFLOATP (number))
    return bigfloat_sign (XBIGFLOAT_DATA (number)) == 0 ? Qt : Qnil;
#endif
  else
    {
      number = wrong_type_argument (Qnumberp, number);
      goto retry;
    }
}

/* Convert between a 32-bit value and a cons of two 16-bit values.
   This is used to pass 32-bit integers to and from the user.
   Use time_to_lisp() and lisp_to_time() for time values.

   If you're thinking of using this to store a pointer into a Lisp Object
   for internal purposes (such as when calling record_unwind_protect()),
   try using make_opaque_ptr()/get_opaque_ptr() instead. */
Lisp_Object
word_to_lisp (unsigned int item)
{
  return Fcons (make_int (item >> 16), make_int (item & 0xffff));
}

unsigned int
lisp_to_word (Lisp_Object item)
{
  if (INTP (item))
    return XINT (item);
  else
    {
      Lisp_Object top = Fcar (item);
      Lisp_Object bot = Fcdr (item);
      CHECK_INT (top);
      CHECK_INT (bot);
      return (XINT (top) << 16) | (XINT (bot) & 0xffff);
    }
}


DEFUN ("number-to-string", Fnumber_to_string, 1, 1, 0, /*
Convert NUMBER to a string by printing it in decimal.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.
If supported, it may also be a ratio.
*/
       (number))
{
#ifdef WITH_NUMBER_TYPES
  CHECK_NUMBER (number);
#else
  CHECK_INT_OR_FLOAT (number);
#endif

  if (FLOATP (number))
    {
      char pigbuf[350];	/* see comments in float_to_string */

      float_to_string (pigbuf, XFLOAT_DATA (number));
      return build_string (pigbuf);
    }
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      char *str = bignum_to_string (XBIGNUM_DATA (number), 10);
      Lisp_Object retval = build_string (str);
      xfree (str, char *);
      return retval;
    }
#endif
#ifdef HAVE_RATIO
  if (RATIOP (number))
    {
      char *str = ratio_to_string (XRATIO_DATA (number), 10);
      Lisp_Object retval = build_string (str);
      xfree (str, char *);
      return retval;
    }
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (number))
    {
      char *str = bigfloat_to_string (XBIGFLOAT_DATA (number), 10);
      Lisp_Object retval = build_string (str);
      xfree (str, char *);
      return retval;
    }
#endif

  {
    char buffer[DECIMAL_PRINT_SIZE (long)];

    long_to_string (buffer, XINT (number));
    return build_string (buffer);
  }
}

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
  char *p;
  int b;

  CHECK_STRING (string);

  if (NILP (base))
    b = 10;
  else
    {
      CHECK_INT (base);
      b = XINT (base);
      check_int_range (b, 2, 16);
    }

  p = (char *) XSTRING_DATA (string);

  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  while (*p == ' ' || *p == '\t')
    p++;

  if (isfloat_string (p) && b == 10)
    {
#ifdef HAVE_BIGFLOAT
      if (ZEROP (Vdefault_float_precision))
#endif
	return make_float (atof (p));
#ifdef HAVE_BIGFLOAT
      else
	{
	  bigfloat_set_prec (scratch_bigfloat, bigfloat_get_default_prec ());
	  bigfloat_set_string (scratch_bigfloat, p, b);
	  return make_bigfloat_bf (scratch_bigfloat);
	}
#endif
    }

#ifdef HAVE_RATIO
  if (qxestrchr (p, '/') != NULL)
    {
      ratio_set_string (scratch_ratio, p, b);
      return make_ratio_rt (scratch_ratio);
    }
#endif /* HAVE_RATIO */

#ifdef HAVE_BIGNUM
  /* GMP bignum_set_string returns random values when fed an empty string */
  if (*p == '\0')
    return make_int (0);
  bignum_set_string (scratch_bignum, p, b);
  return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
  if (b == 10)
    {
      /* Use the system-provided functions for base 10. */
#if   SIZEOF_EMACS_INT == SIZEOF_INT
      return make_int (atoi (p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG
      return make_int (atol (p));
#elif SIZEOF_EMACS_INT == SIZEOF_LONG_LONG
      return make_int (atoll (p));
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
      return make_int (negative * v);
    }
#endif /* HAVE_BIGNUM */
}


DEFUN ("+", Fplus, 0, MANY, 0, /*
Return sum of any number of arguments.
The arguments should all be numbers, characters or markers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum = make_int (0), addend;

  for (i = 0; i < nargs; i++)
    {
      addend = args[i];
      switch (promote_args (&accum, &addend))
	{
	case FIXNUM_T:
	  accum = make_integer (XREALINT (accum) + XREALINT (addend));
	  break;
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  bignum_add (scratch_bignum, XBIGNUM_DATA (accum),
		      XBIGNUM_DATA (addend));
	  accum = make_bignum_bg (scratch_bignum);
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  ratio_add (scratch_ratio, XRATIO_DATA (accum),
		     XRATIO_DATA (addend));
	  accum = make_ratio_rt (scratch_ratio);
	  break;
#endif
	case FLOAT_T:
	  accum = make_float (XFLOAT_DATA (accum) + XFLOAT_DATA (addend));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  bigfloat_set_prec (scratch_bigfloat,
			     max (XBIGFLOAT_GET_PREC (addend),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_add (scratch_bigfloat, XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (addend));
	  accum = make_bigfloat_bf (scratch_bigfloat);
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

  return make_int (iaccum);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("-", Fminus, 1, MANY, 0, /*
Negate number or subtract numbers, characters or markers.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum = args[0], subtrahend;

  if (nargs == 1)
    {
      if (CHARP (accum))
	accum = make_int (XCHAR (accum));
      else if (MARKERP (accum))
	accum = make_int (marker_position (accum));

      /* Invert the sign of accum */
      CHECK_NUMBER (accum);
      switch (get_number_type (accum))
	{
	case FIXNUM_T:
	  return make_integer (-XREALINT (accum));
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
	      accum = make_integer (XREALINT (accum) - XREALINT (subtrahend));
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

  return make_int (iaccum);

 do_float:
  for (; args < args_end; args++)
    daccum -= number_char_or_marker_to_double (*args);
  return make_float (daccum);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("*", Ftimes, 0, MANY, 0, /*
Return product of any number of arguments.
The arguments should all be numbers, characters or markers.
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
	  bignum_mul (scratch_bignum, XBIGNUM_DATA (accum),
		      XBIGNUM_DATA (multiplier));
	  accum = make_bignum_bg (scratch_bignum);
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  ratio_mul (scratch_ratio, XRATIO_DATA (accum),
		     XRATIO_DATA (multiplier));
	  accum = make_ratio_rt (scratch_ratio);
	  break;
#endif
	case FLOAT_T:
	  accum = make_float (XFLOAT_DATA (accum) * XFLOAT_DATA (multiplier));
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  bigfloat_set_prec (scratch_bigfloat,
			     max (XBIGFLOAT_GET_PREC (multiplier),
				  XBIGFLOAT_GET_PREC (accum)));
	  bigfloat_mul (scratch_bigfloat, XBIGFLOAT_DATA (accum),
			XBIGFLOAT_DATA (multiplier));
	  accum = make_bigfloat_bf (scratch_bigfloat);
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

  return make_int (iaccum);
#endif /* WITH_NUMBER_TYPES */
}

#ifdef HAVE_RATIO
DEFUN ("div", Fdiv, 1, MANY, 0, /*
Same as `/', but dividing integers creates a ratio instead of truncating.
Note that this is a departure from Common Lisp, where / creates ratios when
dividing integers.  Having a separate function lets us avoid breaking existing
Emacs Lisp code that expects / to do integer division.
*/
       (int nargs, Lisp_Object *args))
{
  REGISTER int i;
  Lisp_Object accum, divisor;

  if (nargs == 1)
    {
      i = 0;
      accum = make_int (1);
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
	  if (XREALINT (divisor) == 0) goto divide_by_zero;
	  bignum_set_long (scratch_bignum, XREALINT (accum));
	  bignum_set_long (scratch_bignum2, XREALINT (divisor));
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
Return first argument divided by all the remaining arguments.
The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i;
  Lisp_Object accum, divisor;

  if (nargs == 1)
    {
      i = 0;
      accum = make_int (1);
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
	  if (XREALINT (divisor) == 0) goto divide_by_zero;
	  accum = make_integer (XREALINT (accum) / XREALINT (divisor));
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

  return make_int (iaccum);

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
The value is always a number; markers and characters are converted
to numbers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i, maxindex = 0;
  Lisp_Object comp1, comp2;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_int (XCHAR (args[0]));
  else if (MARKERP (args[0]))
    args[0] = make_int (marker_position (args[0]));
  for (i = 1; i < nargs; i++)
    {
    retry:
      comp1 = args[maxindex];
      comp2 = args[i];
      switch (promote_args (&comp1, &comp2))
	{
	case FIXNUM_T:
	  if (XREALINT (comp1) < XREALINT (comp2))
	    maxindex = i;
	  break;
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  if (bignum_lt (XBIGNUM_DATA (comp1), XBIGNUM_DATA (comp2)))
	    maxindex = i;
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  if (ratio_lt (XRATIO_DATA (comp1), XRATIO_DATA (comp2)))
	    maxindex = i;
	  break;
#endif
	case FLOAT_T:
	  if (XFLOAT_DATA (comp1) < XFLOAT_DATA (comp2))
	    maxindex = i;
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  if (bigfloat_lt (XBIGFLOAT_DATA (comp1), XBIGFLOAT_DATA (comp2)))
	    maxindex = i;
	  break;
#endif
	}
    }
  return args[maxindex];
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT imax;
  double dmax;
  Lisp_Object *args_end = args + nargs;
  int_or_double iod;

  number_char_or_marker_to_int_or_double (*args++, &iod);
  if (iod.int_p)
    imax = iod.c.ival;
  else
    {
      dmax = iod.c.dval;
      goto max_floats;
    }

  while (args < args_end)
    {
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	{
	  if (imax < iod.c.ival) imax = iod.c.ival;
	}
      else
	{
	  dmax = (double) imax;
	  if (dmax < iod.c.dval) dmax = iod.c.dval;
	  goto max_floats;
	}
    }

  return make_int (imax);

 max_floats:
  while (args < args_end)
    {
      double dval = number_char_or_marker_to_double (*args++);
      if (dmax < dval) dmax = dval;
    }
  return make_float (dmax);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("min", Fmin, 1, MANY, 0, /*
Return smallest of all the arguments.
All arguments must be numbers, characters or markers.
The value is always a number; markers and characters are converted
to numbers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef WITH_NUMBER_TYPES
  REGISTER int i, minindex = 0;
  Lisp_Object comp1, comp2;

  while (!(CHARP (args[0]) || MARKERP (args[0]) || REALP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);
  if (CHARP (args[0]))
    args[0] = make_int (XCHAR (args[0]));
  else if (MARKERP (args[0]))
    args[0] = make_int (marker_position (args[0]));
  for (i = 1; i < nargs; i++)
    {
      comp1 = args[minindex];
      comp2 = args[i];
      switch (promote_args (&comp1, &comp2))
	{
	case FIXNUM_T:
	  if (XREALINT (comp1) > XREALINT (comp2))
	    minindex = i;
	  break;
#ifdef HAVE_BIGNUM
	case BIGNUM_T:
	  if (bignum_gt (XBIGNUM_DATA (comp1), XBIGNUM_DATA (comp2)))
	    minindex = i;
	  break;
#endif
#ifdef HAVE_RATIO
	case RATIO_T:
	  if (ratio_gt (XRATIO_DATA (comp1), XRATIO_DATA (comp2)))
	    minindex = i;
	  break;
#endif
	case FLOAT_T:
	  if (XFLOAT_DATA (comp1) > XFLOAT_DATA (comp2))
	    minindex = i;
	  break;
#ifdef HAVE_BIGFLOAT
	case BIGFLOAT_T:
	  if (bigfloat_gt (XBIGFLOAT_DATA (comp1), XBIGFLOAT_DATA (comp2)))
	    minindex = i;
	  break;
#endif
	}
    }
  return args[minindex];
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT imin;
  double dmin;
  Lisp_Object *args_end = args + nargs;
  int_or_double iod;

  number_char_or_marker_to_int_or_double (*args++, &iod);
  if (iod.int_p)
    imin = iod.c.ival;
  else
    {
      dmin = iod.c.dval;
      goto min_floats;
    }

  while (args < args_end)
    {
      number_char_or_marker_to_int_or_double (*args++, &iod);
      if (iod.int_p)
	{
	  if (imin > iod.c.ival) imin = iod.c.ival;
	}
      else
	{
	  dmin = (double) imin;
	  if (dmin > iod.c.dval) dmin = iod.c.dval;
	  goto min_floats;
	}
    }

  return make_int (imin);

 min_floats:
  while (args < args_end)
    {
      double dval = number_char_or_marker_to_double (*args++);
      if (dmin > dval) dmin = dval;
    }
  return make_float (dmin);
#endif /* WITH_NUMBER_TYPES */
}

DEFUN ("logand", Flogand, 0, MANY, 0, /*
Return bitwise-and of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_int (~0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_int (XCHAR (result));
  else if (MARKERP (result))
    result = make_int (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qnumber_char_or_marker_p, args[i]);
      other = args[i];
      switch (promote_args (&result & &other))
	{
	case FIXNUM_T:
	  result = make_int (XREALINT (result), XREALINT (other));
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
    bits &= integer_char_or_marker_to_int (*args++);

  return make_int (bits);
#endif /* HAVE_BIGNUM */
}

DEFUN ("logior", Flogior, 0, MANY, 0, /*
Return bitwise-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_int (0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_int (XCHAR (result));
  else if (MARKERP (result))
    result = make_int (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qnumber_char_or_marker_p, args[i]);
      other = args[i];
      switch (promote_args (&result, &other))
	{
	case FIXNUM_T:
	  result = make_int (XREALINT (result) | XREALINT (other));
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
    bits |= integer_char_or_marker_to_int (*args++);

  return make_int (bits);
#endif /* HAVE_BIGNUM */
}

DEFUN ("logxor", Flogxor, 0, MANY, 0, /*
Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers or characters converted to integers.
*/
       (int nargs, Lisp_Object *args))
{
#ifdef HAVE_BIGNUM
  REGISTER int i;
  Lisp_Object result, other;

  if (nargs == 0)
    return make_int (0);

  while (!(CHARP (args[0]) || MARKERP (args[0]) || INTEGERP (args[0])))
    args[0] = wrong_type_argument (Qnumber_char_or_marker_p, args[0]);

  result = args[0];
  if (CHARP (result))
    result = make_int (XCHAR (result));
  else if (MARKERP (result))
    result = make_int (marker_position (result));
  for (i = 1; i < nargs; i++)
    {
      while (!(CHARP (args[i]) || MARKERP (args[i]) || INTEGERP (args[i])))
	args[i] = wrong_type_argument (Qnumber_char_or_marker_p, args[i]);
      other = args[i];
      if (promote_args (&result, &other) == FIXNUM_T)
	{
	  result = make_int (XREALINT (result) ^ XREALINT (other));
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
    bits ^= integer_char_or_marker_to_int (*args++);

  return make_int (bits);
#endif /* !HAVE_BIGNUM */
}

DEFUN ("lognot", Flognot, 1, 1, 0, /*
Return the bitwise complement of NUMBER.
NUMBER may be an integer, marker or character converted to integer.
*/
       (number))
{
#ifdef HAVE_BIGNUM
  if (BIGNUMP (number))
    {
      bignum_not (scratch_bignum, XBIGNUM_DATA (number));
      return make_bignum_bg (scratch_bignum);
    }
#endif /* HAVE_BIGNUM */
  return make_int (~ integer_char_or_marker_to_int (number));
}

DEFUN ("%", Frem, 2, 2, 0, /*
Return remainder of first arg divided by second.
Both must be integers, characters or markers.
*/
       (number1, number2))
{
#ifdef HAVE_BIGNUM
  while (!(CHARP (number1) || MARKERP (number1) || INTEGERP (number1)))
    number1 = wrong_type_argument (Qnumber_char_or_marker_p, number1);
  while (!(CHARP (number2) || MARKERP (number2) || INTEGERP (number2)))
    number2 = wrong_type_argument (Qnumber_char_or_marker_p, number2);

  if (promote_args (&number1, &number2) == FIXNUM_T)
    {
      if (XREALINT (number2) == 0)
	Fsignal (Qarith_error, Qnil);
      return make_int (XREALINT (number1) % XREALINT (number2));
    }
  else
    {
      if (bignum_sign (XBIGNUM_DATA (number2)) == 0)
	Fsignal (Qarith_error, Qnil);
      bignum_mod (scratch_bignum, XBIGNUM_DATA (number1),
		  XBIGNUM_DATA (number2));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
#else /* !HAVE_BIGNUM */
  EMACS_INT ival1 = integer_char_or_marker_to_int (number1);
  EMACS_INT ival2 = integer_char_or_marker_to_int (number2);

  if (ival2 == 0)
    Fsignal (Qarith_error, Qnil);

  return make_int (ival1 % ival2);
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
	if (XREALINT (y) == 0) goto divide_by_zero;
	ival = XREALINT (x) % XREALINT (y);
	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XREALINT (y) < 0 ? ival > 0 : ival < 0)
	  ival += XREALINT (y);
	return make_int (ival);
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

    return make_int (ival);
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
  CHECK_INT_COERCE_CHAR (value);
  CONCHECK_INT (count);

  return make_int (XINT (count) > 0 ?
		   XINT (value) <<  XINT (count) :
		   XINT (value) >> -XINT (count));
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
    wrong_type_argument (Qnumber_char_or_marker_p, value);
  CONCHECK_INTEGER (count);

  if (promote_args (&value, &count) == FIXNUM_T)
    {
      if (XREALINT (count) <= 0)
	return make_int (XREALINT (value) >> -XREALINT (count));
      /* Use bignums to avoid overflow */
      bignum_set_long (scratch_bignum2, XREALINT (value));
      bignum_lshift (scratch_bignum, scratch_bignum2, XREALINT (count));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
  else
    {
      if (bignum_sign (XBIGNUM_DATA (count)) <= 0)
	{
	  bignum_neg (scratch_bignum, XBIGNUM_DATA (count));
	  if (!bignum_fits_ulong_p (scratch_bignum))
	    args_out_of_range (Qnumber_char_or_marker_p, count);
	  bignum_rshift (scratch_bignum2, XBIGNUM_DATA (value),
			 bignum_to_ulong (scratch_bignum));
	}
      else
	{
	  if (!bignum_fits_ulong_p (XBIGNUM_DATA (count)))
	    args_out_of_range (Qnumber_char_or_marker_p, count);
	  bignum_lshift (scratch_bignum2, XBIGNUM_DATA (value),
			 bignum_to_ulong (XBIGNUM_DATA (count)));
	}
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum2));
    }
#else /* !HAVE_BIGNUM */
  CHECK_INT_COERCE_CHAR (value);
  CONCHECK_INT (count);

  return make_int (XINT (count) > 0 ?
		   XUINT (value) <<  XINT (count) :
		   XUINT (value) >> -XINT (count));
#endif /* HAVE_BIGNUM */
}

DEFUN ("1+", Fadd1, 1, 1, 0, /*
Return NUMBER plus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
*/
       (number))
{
 retry:

  if (INTP    (number)) return make_integer (XINT (number) + 1);
  if (CHARP   (number)) return make_integer (XCHAR (number) + 1);
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

  if (INTP    (number)) return make_integer (XINT (number) - 1);
  if (CHARP   (number)) return make_integer (XCHAR (number) - 1);
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

static Lisp_Object Vall_weak_lists; /* Gemarke es nicht!!! */

static Lisp_Object encode_weak_list_type (enum weak_list_type type);

static Lisp_Object
mark_weak_list (Lisp_Object obj)
{
  return Qnil; /* nichts ist gemarkt */
}

static void
print_weak_list (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<weak-list>");

  write_fmt_string_lisp (printcharfun, "#<weak-list %s %S>", 2,
			 encode_weak_list_type (XWEAK_LIST (obj)->type),
			 XWEAK_LIST (obj)->list);
}

static int
weak_list_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  struct weak_list *w1 = XWEAK_LIST (obj1);
  struct weak_list *w2 = XWEAK_LIST (obj2);

  return ((w1->type == w2->type) &&
	  internal_equal (w1->list, w2->list, depth + 1));
}

static Hashcode
weak_list_hash (Lisp_Object obj, int depth)
{
  struct weak_list *w = XWEAK_LIST (obj);

  return HASH2 ((Hashcode) w->type,
		internal_hash (w->list, depth + 1));
}

Lisp_Object
make_weak_list (enum weak_list_type type)
{
  Lisp_Object result;
  struct weak_list *wl =
    alloc_lcrecord_type (struct weak_list, &lrecord_weak_list);

  wl->list = Qnil;
  wl->type = type;
  result = wrap_weak_list (wl);
  wl->next_weak = Vall_weak_lists;
  Vall_weak_lists = result;
  return result;
}

static const struct memory_description weak_list_description[] = {
  { XD_LISP_OBJECT, offsetof (struct weak_list, list), 
  0, 0, XD_FLAG_NO_KKCC },
  { XD_LO_LINK,     offsetof (struct weak_list, next_weak), 
  0, 0, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("weak-list", weak_list,
			       1, /*dumpable-flag*/
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
	      abort ();
	    }

	  if (need_to_mark_elem && ! marked_p (elem))
	    {
#ifdef USE_KKCC
	      kkcc_gc_stack_push_lisp_object (elem);
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
	  kkcc_gc_stack_push_lisp_object (rest2);
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
      abort ();
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
/*                              weak boxes                              */
/************************************************************************/

static Lisp_Object Vall_weak_boxes; /* Gemarke es niemals ever!!! */

void
prune_weak_boxes (void)
{
  Lisp_Object rest, prev = Qnil;
  int removep = 0;

  for (rest = Vall_weak_boxes;
       !NILP(rest);
       rest = XWEAK_BOX (rest)->next_weak_box)
    {
      if (! (marked_p (rest)))
	/* This weak box itself is garbage. */
	removep = 1;

       if (! marked_p (XWEAK_BOX (rest)->value))
	 {
	   XSET_WEAK_BOX (rest, Qnil);
	   removep = 1;
	 }

       if (removep)
	 {
	   /* Remove weak box from list. */
	   if (NILP (prev))
	     Vall_weak_boxes = XWEAK_BOX (rest)->next_weak_box;
	   else
	     XWEAK_BOX (prev)->next_weak_box = XWEAK_BOX (rest)->next_weak_box;
	   removep = 0;
	 }
       else
	 prev = rest;
    }
}

static Lisp_Object
mark_weak_box (Lisp_Object obj)
{
  return Qnil;
}

static void
print_weak_box (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<weak_box>");
  write_fmt_string (printcharfun, "#<weak_box>");
}

static int
weak_box_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  struct weak_box *wb1 = XWEAK_BOX (obj1);
  struct weak_box *wb2 = XWEAK_BOX (obj2);

  return (internal_equal (wb1->value, wb2->value, depth + 1));
}

static Hashcode
weak_box_hash (Lisp_Object obj, int depth)
{
  struct weak_box *wb = XWEAK_BOX (obj);

  return internal_hash (wb->value, depth + 1);
}

Lisp_Object
make_weak_box (Lisp_Object value)
{
  Lisp_Object result;

  struct weak_box *wb =
    alloc_lcrecord_type (struct weak_box, &lrecord_weak_box);

  wb->value = value;
  result = wrap_weak_box (wb);
  wb->next_weak_box = Vall_weak_boxes;
  Vall_weak_boxes = result;
  return result;
}

static const struct memory_description weak_box_description[] = {
  { XD_LO_LINK, offsetof (struct weak_box, value) },
  { XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION ("weak_box", weak_box,
			       0, /*dumpable-flag*/
			       mark_weak_box, print_weak_box,
			       0, weak_box_equal, weak_box_hash,
			       weak_box_description,
			       struct weak_box);

DEFUN ("make-weak-box", Fmake_weak_box, 1, 1, 0, /*
Return a new weak box from value CONTENTS.
The weak box is a reference to CONTENTS which may be extracted with
`weak-box-ref'.  However, the weak box does not contribute to the
reachability of CONTENTS.  When CONTENTS is garbage-collected,
`weak-box-ref' will return NIL.
*/
       (value))
{
  return make_weak_box(value);
}

DEFUN ("weak-box-ref", Fweak_box_ref, 1, 1, 0, /*
Return the contents of weak box WEAK-BOX.
If the contents have been GCed, return NIL.
*/
       (wb))
{
  return XWEAK_BOX (wb)->value;
}

DEFUN ("weak-box-p", Fweak_boxp, 1, 1, 0, /*
Return non-nil if OBJECT is a weak box.
*/
       (object))
{
  return WEAK_BOXP (object) ? Qt : Qnil;
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
init_marking_ephemerons(void)
{
  Vnew_all_ephemerons = Qnil;
}

/* Move all live ephemerons with live keys over to
 * Vnew_all_ephemerons, marking the values and finalizers along the
 * way. */

int
continue_marking_ephemerons(void)
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
	      kkcc_gc_stack_push_lisp_object 
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
finish_marking_ephemerons(void)
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
	      kkcc_gc_stack_push_lisp_object 
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
prune_ephemerons(void)
{
  Vall_ephemerons = Vnew_all_ephemerons;
}

Lisp_Object
zap_finalize_list(void)
{
  Lisp_Object finalizers = Vfinalize_list;

  Vfinalize_list = Qnil;

  return finalizers;
}

static Lisp_Object
mark_ephemeron (Lisp_Object obj)
{
  return Qnil;
}

static void
print_ephemeron (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<ephemeron>");
  write_fmt_string (printcharfun, "#<ephemeron>");
}

static int
ephemeron_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return
    internal_equal (XEPHEMERON_REF (obj1), XEPHEMERON_REF(obj2), depth + 1);
}

static Hashcode
ephemeron_hash(Lisp_Object obj, int depth)
{
  return internal_hash (XEPHEMERON_REF (obj), depth + 1);
}

Lisp_Object
make_ephemeron(Lisp_Object key, Lisp_Object value, Lisp_Object finalizer)
{
  Lisp_Object result, temp = Qnil;
  struct gcpro gcpro1, gcpro2;

  struct ephemeron *eph =
    alloc_lcrecord_type (struct ephemeron, &lrecord_ephemeron);

  eph->key = Qnil;
  eph->cons_chain = Qnil;
  eph->value = Qnil;

  result = wrap_ephemeron(eph);
  GCPRO2 (result, temp);

  eph->key = key;
  temp = Fcons(value, finalizer);
  eph->cons_chain = Fcons(temp, Vall_ephemerons);
  eph->value = value;

  Vall_ephemerons = result;

  UNGCPRO;
  return result;
}

/* Ephemerons are special cases in the KKCC mark algorithm, so nothing
   is marked here. */
static const struct memory_description ephemeron_description[] = {
  { XD_LISP_OBJECT, offsetof(struct ephemeron, key),
    0, 0, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof(struct ephemeron, cons_chain),
    0, 0, XD_FLAG_NO_KKCC },
  { XD_LISP_OBJECT, offsetof(struct ephemeron, value),
    0, 0, XD_FLAG_NO_KKCC },
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("ephemeron", ephemeron,
			       0, /*dumpable-flag*/
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
  return make_ephemeron(key, value, finalizer);
}

DEFUN ("ephemeron-ref",  Fephemeron_ref, 1, 1, 0, /*
Return the contents of ephemeron EPHEMERON.
If the contents have been GCed, return NIL.
*/
       (eph))
{
  return XEPHEMERON_REF (eph);
}

DEFUN ("ephemeron-p", Fephemeronp, 1, 1, 0, /*
Return non-nil if OBJECT is an ephemeron.
*/
       (object))
{
  return EPHEMERONP (object) ? Qt : Qnil;
}

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

  DEFERROR (Qio_error, "IO Error", Qinvalid_operation);
  DEFERROR_STANDARD (Qfile_error, Qio_error);
  DEFERROR (Qend_of_file, "End of file or stream", Qfile_error);
  DEFERROR_STANDARD (Qconversion_error, Qio_error);
  DEFERROR_STANDARD (Qtext_conversion_error, Qconversion_error);

  DEFERROR (Qarith_error, "Arithmetic error", Qinvalid_operation);
  DEFERROR (Qrange_error, "Arithmetic range error", Qarith_error);
  DEFERROR (Qdomain_error, "Arithmetic domain error", Qarith_error);
  DEFERROR (Qsingularity_error, "Arithmetic singularity error", Qdomain_error);
  DEFERROR (Qoverflow_error, "Arithmetic overflow error", Qdomain_error);
  DEFERROR (Qunderflow_error, "Arithmetic underflow error", Qdomain_error);
}

void
syms_of_data (void)
{
  INIT_LRECORD_IMPLEMENTATION (weak_list);
  INIT_LRECORD_IMPLEMENTATION (ephemeron);
  INIT_LRECORD_IMPLEMENTATION (weak_box);

  DEFSYMBOL (Qquote);
  DEFSYMBOL (Qlambda);
  DEFSYMBOL (Qlistp);
  DEFSYMBOL (Qtrue_list_p);
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

  DEFSUBR (Fwrong_type_argument);

#ifdef HAVE_RATIO
  DEFSUBR (Fdiv);
#endif
  DEFSUBR (Feq);
  DEFSUBR (Fold_eq);
  DEFSUBR (Fnull);
  Ffset (intern ("not"), intern ("null"));
  DEFSUBR (Flistp);
  DEFSUBR (Fnlistp);
  DEFSUBR (Ftrue_list_p);
  DEFSUBR (Fconsp);
  DEFSUBR (Fatom);
  DEFSUBR (Fchar_or_string_p);
  DEFSUBR (Fcharacterp);
  DEFSUBR (Fchar_int_p);
  DEFSUBR (Fchar_to_int);
  DEFSUBR (Fint_to_char);
  DEFSUBR (Fchar_or_char_int_p);
#ifdef HAVE_BIGNUM
  DEFSUBR (Ffixnump);
#else
  DEFSUBR (Fintegerp);
#endif
  DEFSUBR (Finteger_or_marker_p);
  DEFSUBR (Finteger_or_char_p);
  DEFSUBR (Finteger_char_or_marker_p);
  DEFSUBR (Fnumberp);
  DEFSUBR (Fnumber_or_marker_p);
  DEFSUBR (Fnumber_char_or_marker_p);
  DEFSUBR (Ffloatp);
  DEFSUBR (Fnatnump);
  DEFSUBR (Fnonnegativep);
  DEFSUBR (Fsymbolp);
  DEFSUBR (Fkeywordp);
  DEFSUBR (Fstringp);
  DEFSUBR (Fvectorp);
  DEFSUBR (Fbitp);
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

  DEFSUBR (Fnumber_to_string);
  DEFSUBR (Fstring_to_number);
  DEFSUBR (Feqlsign);
  DEFSUBR (Flss);
  DEFSUBR (Fgtr);
  DEFSUBR (Fleq);
  DEFSUBR (Fgeq);
  DEFSUBR (Fneq);
  DEFSUBR (Fzerop);
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
  DEFSUBR (Fmake_weak_box);
  DEFSUBR (Fweak_box_ref);
  DEFSUBR (Fweak_boxp);
}

void
vars_of_data (void)
{
  /* This must not be staticpro'd */
  Vall_weak_lists = Qnil;
  dump_add_weak_object_chain (&Vall_weak_lists);

  Vall_ephemerons = Qnil;
  dump_add_weak_object_chain (&Vall_ephemerons);

  Vfinalize_list = Qnil;
  staticpro (&Vfinalize_list);

  Vall_weak_boxes = Qnil;
  dump_add_weak_object_chain (&Vall_weak_boxes);

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
