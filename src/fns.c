/* Random utility Lisp functions.
   Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2005, 2010 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30. */

/* This file has been Mule-ized. */

/* Note: FSF 19.30 has bool vectors.  We have bit vectors. */

/* Hacked on for Mule by Ben Wing, December 1994, January 1995. */

#include <config.h>

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#include "lisp.h"

#include "sysfile.h"
#include "sysproc.h" /* for qxe_getpid() */

#include "buffer.h"
#include "bytecode.h"
#include "casetab.h"
#include "device.h"
#include "events.h"
#include "extents.h"
#include "frame.h"
#include "process.h"
#include "systime.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"

/* NOTE: This symbol is also used in lread.c */
#define FEATUREP_SYNTAX

Lisp_Object Qstring_lessp;
Lisp_Object Qidentity;
Lisp_Object Qvector, Qarray, Qbit_vector;

Lisp_Object Qbase64_conversion_error;

Lisp_Object Vpath_separator;

static int internal_old_equal (Lisp_Object, Lisp_Object, int);
Lisp_Object safe_copy_tree (Lisp_Object arg, Lisp_Object vecp, int depth);

static Lisp_Object
mark_bit_vector (Lisp_Object UNUSED (obj))
{
  return Qnil;
}

static void
print_bit_vector (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Elemcount i;
  Lisp_Bit_Vector *v = XBIT_VECTOR (obj);
  Elemcount len = bit_vector_length (v);
  Elemcount last = len;

  if (INTP (Vprint_length))
    last = min (len, XINT (Vprint_length));
  write_ascstring (printcharfun, "#*");
  for (i = 0; i < last; i++)
    {
      if (bit_vector_bit (v, i))
	write_ascstring (printcharfun, "1");
      else
	write_ascstring (printcharfun, "0");
    }

  if (last != len)
    write_ascstring (printcharfun, "...");
}

static int
bit_vector_equal (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth),
		  int UNUSED (foldcase))
{
  Lisp_Bit_Vector *v1 = XBIT_VECTOR (obj1);
  Lisp_Bit_Vector *v2 = XBIT_VECTOR (obj2);

  return ((bit_vector_length (v1) == bit_vector_length (v2)) &&
	  !memcmp (v1->bits, v2->bits,
		   BIT_VECTOR_LONG_STORAGE (bit_vector_length (v1)) *
		   sizeof (long)));
}

static Hashcode
bit_vector_hash (Lisp_Object obj, int UNUSED (depth))
{
  Lisp_Bit_Vector *v = XBIT_VECTOR (obj);
  return HASH2 (bit_vector_length (v),
		memory_hash (v->bits,
			     BIT_VECTOR_LONG_STORAGE (bit_vector_length (v)) *
			     sizeof (long)));
}

static Bytecount
size_bit_vector (const void *lheader)
{
  Lisp_Bit_Vector *v = (Lisp_Bit_Vector *) lheader;
  return FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Bit_Vector, unsigned long, bits,
				       BIT_VECTOR_LONG_STORAGE (bit_vector_length (v)));
}

static const struct memory_description bit_vector_description[] = {
  { XD_END }
};


DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("bit-vector", bit_vector,
					1, /*dumpable-flag*/
					mark_bit_vector,
					print_bit_vector, 0,
					bit_vector_equal,
					bit_vector_hash,
					bit_vector_description,
					size_bit_vector,
					Lisp_Bit_Vector);


DEFUN ("identity", Fidentity, 1, 1, 0, /*
Return the argument unchanged.
*/
       (arg))
{
  return arg;
}

DEFUN ("random", Frandom, 0, 1, 0, /*
Return a pseudo-random number.
All fixnums are equally likely.  On most systems, this is 31 bits' worth.
With positive integer argument N, return random number in interval [0,N).
N can be a bignum, in which case the range of possible values is extended.
With argument t, set the random number seed from the current time and pid.
*/
       (limit))
{
  EMACS_INT val;
  unsigned long denominator;

  if (EQ (limit, Qt))
    seed_random (qxe_getpid () + time (NULL));
  if (NATNUMP (limit) && !ZEROP (limit))
    {
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  We do this by using the
	 quotient rather than the remainder.  At the high end of the RNG
	 it's possible to get a quotient larger than limit; discarding
	 these values eliminates the bias that would otherwise appear
	 when using a large limit.  */
      denominator = ((unsigned long)1 << INT_VALBITS) / XINT (limit);
      do
	val = get_random () / denominator;
      while (val >= XINT (limit));
    }
#ifdef HAVE_BIGNUM
  else if (BIGNUMP (limit))
    {
      bignum_random (scratch_bignum, XBIGNUM_DATA (limit));
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
    }
#endif
  else
    val = get_random ();

  return make_int (val);
}

/* Random data-structure functions */

#ifdef LOSING_BYTECODE

/* #### Delete this shit */

/* Charcount is a misnomer here as we might be dealing with the
   length of a vector or list, but emphasizes that we're not dealing
   with Bytecounts in strings */
static Charcount
length_with_bytecode_hack (Lisp_Object seq)
{
  if (!COMPILED_FUNCTIONP (seq))
    return XINT (Flength (seq));
  else
    {
      Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (seq);

      return (f->flags.interactivep ? COMPILED_INTERACTIVE :
	      f->flags.domainp      ? COMPILED_DOMAIN :
	      COMPILED_DOC_STRING)
	+ 1;
    }
}

#endif /* LOSING_BYTECODE */

void
check_losing_bytecode (const Ascbyte *function, Lisp_Object seq)
{
  if (COMPILED_FUNCTIONP (seq))
    signal_ferror_with_frob
      (Qinvalid_argument, seq,
       "As of 20.3, `%s' no longer works with compiled-function objects",
       function);
}

DEFUN ("length", Flength, 1, 1, 0, /*
Return the length of vector, bit vector, list or string SEQUENCE.
*/
       (sequence))
{
 retry:
  if (STRINGP (sequence))
    return make_int (string_char_length (sequence));
  else if (CONSP (sequence))
    {
      Elemcount len;
      GET_EXTERNAL_LIST_LENGTH (sequence, len);
      return make_int (len);
    }
  else if (VECTORP (sequence))
    return make_int (XVECTOR_LENGTH (sequence));
  else if (NILP (sequence))
    return Qzero;
  else if (BIT_VECTORP (sequence))
    return make_int (bit_vector_length (XBIT_VECTOR (sequence)));
  else
    {
      check_losing_bytecode ("length", sequence);
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
}

DEFUN ("safe-length", Fsafe_length, 1, 1, 0, /*
Return the length of a list, but avoid error or infinite loop.
This function never gets an error.  If LIST is not really a list,
it returns 0.  If LIST is circular, it returns a finite value
which is at least the number of distinct elements.
*/
       (list))
{
  Lisp_Object hare, tortoise;
  Elemcount len;

  for (hare = tortoise = list, len = 0;
       CONSP (hare) && (! EQ (hare, tortoise) || len == 0);
       hare = XCDR (hare), len++)
    {
      if (len & 1)
	tortoise = XCDR (tortoise);
    }

  return make_int (len);
}

/*** string functions. ***/

DEFUN ("string-equal", Fstring_equal, 2, 2, 0, /*
Return t if two strings have identical contents.
Case is significant.  Text properties are ignored.
\(Under XEmacs, `equal' also ignores text properties and extents in
strings, but this is not the case under FSF Emacs 19.  In FSF Emacs 20
`equal' is the same as in XEmacs, in that respect.)
Symbols are also allowed; their print names are used instead.
*/
       (string1, string2))
{
  Bytecount len;
  Lisp_Object p1, p2;

  if (SYMBOLP (string1))
    p1 = XSYMBOL (string1)->name;
  else
    {
      CHECK_STRING (string1);
      p1 = string1;
    }

  if (SYMBOLP (string2))
    p2 = XSYMBOL (string2)->name;
  else
    {
      CHECK_STRING (string2);
      p2 = string2;
    }

  return (((len = XSTRING_LENGTH (p1)) == XSTRING_LENGTH (p2)) &&
	  !memcmp (XSTRING_DATA (p1), XSTRING_DATA (p2), len)) ? Qt : Qnil;
}

DEFUN ("compare-strings", Fcompare_strings, 6, 7, 0, /*
Compare the contents of two strings, maybe ignoring case.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings,
and arguments that are outside the string (negative STARTi or ENDi
greater than length) are coerced to 0 or string length as appropriate.

Optional IGNORE-CASE non-nil means use case-insensitive comparison.
Case is significant by default.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning.
*/
     (str1, start1, end1, str2, start2, end2, ignore_case))
{
  Charcount ccstart1, ccend1, ccstart2, ccend2;
  Bytecount bstart1, blen1, bstart2, blen2;
  Charcount matching;
  int res;

  CHECK_STRING (str1);
  CHECK_STRING (str2);
  get_string_range_char (str1, start1, end1, &ccstart1, &ccend1,
			 GB_HISTORICAL_STRING_BEHAVIOR|GB_COERCE_RANGE);
  get_string_range_char (str2, start2, end2, &ccstart2, &ccend2,
			 GB_HISTORICAL_STRING_BEHAVIOR|GB_COERCE_RANGE);

  bstart1 = string_index_char_to_byte (str1, ccstart1);
  blen1 = string_offset_char_to_byte_len (str1, bstart1, ccend1 - ccstart1);
  bstart2 = string_index_char_to_byte (str2, ccstart2);
  blen2 = string_offset_char_to_byte_len (str2, bstart2, ccend2 - ccstart2);

  res = ((NILP (ignore_case) ? qxetextcmp_matching : qxetextcasecmp_matching)
	 (XSTRING_DATA (str1) + bstart1, blen1,
	  XSTRING_DATA (str2) + bstart2, blen2,
	  &matching));

  if (!res)
    return Qt;
  else if (res > 0)
    return make_int (1 + matching);
  else
    return make_int (-1 - matching);
}

DEFUN ("string-lessp", Fstring_lessp, 2, 2, 0, /*
Return t if first arg string is less than second in lexicographic order.
Under old-Mule, comparison of chars within a charset is will-defined but
comparison of chars from different charsets is produce well-defined
results; however, ASCII, Control-1 and Latin-1 are guaranteed to compare in
the expected fashion and will be below all other charsets.  Under
Unicode-internal, comparison will happen according to the Unicode codepoint
assigned to each character.
*/
       (string1, string2))
{
  Lisp_Object p1, p2;

  if (SYMBOLP (string1))
    p1 = XSYMBOL (string1)->name;
  else   
    { 
      CHECK_STRING (string1);
      p1 = string1;
    }

  if (SYMBOLP (string2))
    p2 = XSYMBOL (string2)->name;
  else
    {
      CHECK_STRING (string2);
      p2 = string2;
    }

  /* Since we've assigned Control-1 and Latin-1 the two lowest leading
     bytes, the statement above about them and their ordering w.r.t.  other
     charsets is guaranteed.  Also, UTF-8 preserves Unicode character order
     when comparing byte-by-byte.  So need no to do an actual char-by-char
     comparison. */

  if (qxememcmp4 (XSTRING_DATA (p1), XSTRING_LENGTH (p1),
		  XSTRING_DATA (p2), XSTRING_LENGTH (p2)) < 0)
    return Qt;
  else
    return Qnil;
}

DEFUN ("string-modified-tick", Fstring_modified_tick, 1, 1, 0, /*
Return STRING's tick counter, incremented for each change to the string.
Each string has a tick counter which is incremented each time the contents
of the string are changed (e.g. with `aset').  It wraps around occasionally.
*/
       (string))
{
  CHECK_STRING (string);
  if (CONSP (XSTRING_PLIST (string)) && INTP (XCAR (XSTRING_PLIST (string))))
    return XCAR (XSTRING_PLIST (string));
  else
    return Qzero;
}

void
bump_string_modiff (Lisp_Object str)
{
  Lisp_Object *ptr = &XSTRING_PLIST (str);

#ifdef I18N3
  /* #### remove the `string-translatable' property from the string,
     if there is one. */
#endif
  /* skip over extent info if it's there */
  if (CONSP (*ptr) && EXTENT_INFOP (XCAR (*ptr)))
    ptr = &XCDR (*ptr);
  if (CONSP (*ptr) && INTP (XCAR (*ptr)))
    XCAR (*ptr) = make_int (1+XINT (XCAR (*ptr)));
  else
    *ptr = Fcons (make_int (1), *ptr);
}


enum  concat_target_type { c_cons, c_string, c_vector, c_bit_vector };
static Lisp_Object concat (int nargs, Lisp_Object *args,
                           enum concat_target_type target_type,
                           int last_special);

Lisp_Object
concat2 (Lisp_Object string1, Lisp_Object string2)
{
  Lisp_Object args[2];
  args[0] = string1;
  args[1] = string2;
  return concat (2, args, c_string, 0);
}

Lisp_Object
concat3 (Lisp_Object string1, Lisp_Object string2, Lisp_Object string3)
{
  Lisp_Object args[3];
  args[0] = string1;
  args[1] = string2;
  args[2] = string3;
  return concat (3, args, c_string, 0);
}

Lisp_Object
vconcat2 (Lisp_Object vec1, Lisp_Object vec2)
{
  Lisp_Object args[2];
  args[0] = vec1;
  args[1] = vec2;
  return concat (2, args, c_vector, 0);
}

Lisp_Object
vconcat3 (Lisp_Object vec1, Lisp_Object vec2, Lisp_Object vec3)
{
  Lisp_Object args[3];
  args[0] = vec1;
  args[1] = vec2;
  args[2] = vec3;
  return concat (3, args, c_vector, 0);
}

DEFUN ("append", Fappend, 0, MANY, 0, /*
Concatenate all the arguments and make the result a list.
The result is a list whose elements are the elements of all the arguments.
Each argument may be a list, vector, bit vector, or string.
The last argument is not copied, just used as the tail of the new list.
Also see: `nconc'.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  return concat (nargs, args, c_cons, 1);
}

DEFUN ("concat", Fconcat, 0, MANY, 0, /*
Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters.

As of XEmacs 21.0, this function does NOT accept individual integers
as arguments.  Old code that relies on, for example, (concat "foo" 50)
returning "foo50" will fail.  To fix such code, either apply
`int-to-string' to the integer argument, or use `format'.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  return concat (nargs, args, c_string, 0);
}

DEFUN ("vconcat", Fvconcat, 0, MANY, 0, /*
Concatenate all the arguments and make the result a vector.
The result is a vector whose elements are the elements of all the arguments.
Each argument may be a list, vector, bit vector, or string.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  return concat (nargs, args, c_vector, 0);
}

DEFUN ("bvconcat", Fbvconcat, 0, MANY, 0, /*
Concatenate all the arguments and make the result a bit vector.
The result is a bit vector whose elements are the elements of all the
arguments.  Each argument may be a list, vector, bit vector, or string.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  return concat (nargs, args, c_bit_vector, 0);
}

/* Copy a (possibly dotted) list.  LIST must be a cons.
   Can't use concat (1, &alist, c_cons, 0) - doesn't handle dotted lists. */
static Lisp_Object
copy_list (Lisp_Object list)
{
  Lisp_Object list_copy = Fcons (XCAR (list), XCDR (list));
  Lisp_Object last = list_copy;
  Lisp_Object hare, tortoise;
  Elemcount len;

  for (tortoise = hare = XCDR (list), len = 1;
       CONSP (hare);
       hare = XCDR (hare), len++)
    {
      XCDR (last) = Fcons (XCAR (hare), XCDR (hare));
      last = XCDR (last);

      if (len < CIRCULAR_LIST_SUSPICION_LENGTH)
	continue;
      if (len & 1)
	tortoise = XCDR (tortoise);
      if (EQ (tortoise, hare))
	signal_circular_list_error (list);
    }

  return list_copy;
}

DEFUN ("copy-list", Fcopy_list, 1, 1, 0, /*
Return a copy of list LIST, which may be a dotted list.
The elements of LIST are not copied; they are shared
with the original.
*/
       (list))
{
 again:
  if (NILP  (list)) return list;
  if (CONSP (list)) return copy_list (list);

  list = wrong_type_argument (Qlistp, list);
  goto again;
}

DEFUN ("copy-sequence", Fcopy_sequence, 1, 1, 0, /*
Return a copy of list, vector, bit vector or string SEQUENCE.
The elements of a list or vector are not copied; they are shared
with the original. SEQUENCE may be a dotted list.
*/
       (sequence))
{
 again:
  if (NILP        (sequence)) return sequence;
  if (CONSP       (sequence)) return copy_list (sequence);
  if (STRINGP     (sequence)) return concat (1, &sequence, c_string,     0);
  if (VECTORP     (sequence)) return concat (1, &sequence, c_vector,     0);
  if (BIT_VECTORP (sequence)) return concat (1, &sequence, c_bit_vector, 0);

  check_losing_bytecode ("copy-sequence", sequence);
  sequence = wrong_type_argument (Qsequencep, sequence);
  goto again;
}

struct merge_string_extents_struct
{
  Lisp_Object string;
  Bytecount entry_offset;
  Bytecount entry_length;
};

static Lisp_Object
concat (int nargs, Lisp_Object *args,
        enum concat_target_type target_type,
        int last_special)
{
  Lisp_Object val;
  Lisp_Object tail = Qnil;
  int toindex;
  int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;
  struct merge_string_extents_struct *args_mse = 0;
  Ibyte *string_result = 0;
  Ibyte *string_result_ptr = 0;
  struct gcpro gcpro1;
  int sdep = specpdl_depth ();

  /* The modus operandi in Emacs is "caller gc-protects args".
     However, concat is called many times in Emacs on freshly
     created stuff.  So we help those callers out by protecting
     the args ourselves to save them a lot of temporary-variable
     grief. */

  GCPRO1 (args[0]);
  gcpro1.nvars = nargs;

#ifdef I18N3
  /* #### if the result is a string and any of the strings have a string
     for the `string-translatable' property, then concat should also
     concat the args but use the `string-translatable' strings, and store
     the result in the returned string's `string-translatable' property. */
#endif
  if (target_type == c_string)
    args_mse = alloca_array (struct merge_string_extents_struct, nargs);

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  /* Check and coerce the arguments. */
  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object seq = args[argnum];
      if (LISTP (seq))
        ;
      else if (VECTORP (seq) || STRINGP (seq) || BIT_VECTORP (seq))
        ;
#ifdef LOSING_BYTECODE
      else if (COMPILED_FUNCTIONP (seq))
        /* Urk!  We allow this, for "compatibility"... */
        ;
#endif
#if 0				/* removed for XEmacs 21 */
      else if (INTP (seq))
        /* This is too revolting to think about but maintains
           compatibility with FSF (and lots and lots of old code). */
        args[argnum] = Fnumber_to_string (seq);
#endif
      else
	{
          check_losing_bytecode ("concat", seq);
          args[argnum] = wrong_type_argument (Qsequencep, seq);
	}

      if (args_mse)
        {
          if (STRINGP (seq))
            args_mse[argnum].string = seq;
          else
            args_mse[argnum].string = Qnil;
        }
    }

  {
    /* Charcount is a misnomer here as we might be dealing with the
       length of a vector or list, but emphasizes that we're not dealing
       with Bytecounts in strings */
    Charcount total_length;

    for (argnum = 0, total_length = 0; argnum < nargs; argnum++)
      {
#ifdef LOSING_BYTECODE
        Charcount thislen = length_with_bytecode_hack (args[argnum]);
#else
        Charcount thislen = XINT (Flength (args[argnum]));
#endif
        total_length += thislen;
      }

    switch (target_type)
      {
      case c_cons:
        if (total_length == 0)
	  {
	    unbind_to (sdep);
	    /* In append, if all but last arg are nil, return last arg */
	    RETURN_UNGCPRO (last_tail);
	  }
        val = Fmake_list (make_int (total_length), Qnil);
        break;
      case c_vector:
        val = make_vector (total_length, Qnil);
        break;
      case c_bit_vector:
        val = make_bit_vector (total_length, Qzero);
        break;
      case c_string:
	/* We don't make the string yet because we don't know the
	   actual number of bytes.  This loop was formerly written
	   to call Fmake_string() here and then call set_string_char()
	   for each char.  This seems logical enough but is waaaaaaaay
	   slow -- set_string_char() has to scan the whole string up
	   to the place where the substitution is called for in order
	   to find the place to change, and may have to do some
	   realloc()ing in order to make the char fit properly.
	   O(N^2) yuckage. */
        val = Qnil;
	string_result =
	  (Ibyte *) MALLOC_OR_ALLOCA (total_length * MAX_ICHAR_LEN);
	string_result_ptr = string_result;
        break;
      default:
	val = Qnil;
        ABORT ();
      }
  }


  if (CONSP (val))
    tail = val, toindex = -1;	/* -1 in toindex is flag we are
				    making a list */
  else
    toindex = 0;

  prev = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Charcount thisleni = 0;
      Charcount thisindex = 0;
      Lisp_Object seq = args[argnum];
      Ibyte *string_source_ptr = 0;
      Ibyte *string_prev_result_ptr = string_result_ptr;

      if (!CONSP (seq))
	{
#ifdef LOSING_BYTECODE
	  thisleni = length_with_bytecode_hack (seq);
#else
	  thisleni = XINT (Flength (seq));
#endif
	}
      if (STRINGP (seq))
	string_source_ptr = XSTRING_DATA (seq);

      while (1)
	{
	  Lisp_Object elt;

	  /* We've come to the end of this arg, so exit. */
	  if (NILP (seq))
	    break;

	  /* Fetch next element of `seq' arg into `elt' */
	  if (CONSP (seq))
            {
              elt = XCAR (seq);
              seq = XCDR (seq);
            }
	  else
	    {
	      if (thisindex >= thisleni)
		break;

	      if (STRINGP (seq))
		{
		  elt = make_char (itext_ichar (string_source_ptr));
		  INC_IBYTEPTR (string_source_ptr);
		}
	      else if (VECTORP (seq))
                elt = XVECTOR_DATA (seq)[thisindex];
	      else if (BIT_VECTORP (seq))
		elt = make_int (bit_vector_bit (XBIT_VECTOR (seq),
						thisindex));
              else
		elt = Felt (seq, make_int (thisindex));
              thisindex++;
	    }

	  /* Store into result */
	  if (toindex < 0)
	    {
	      /* toindex negative means we are making a list */
	      XCAR (tail) = elt;
	      prev = tail;
	      tail = XCDR (tail);
	    }
	  else if (VECTORP (val))
	    XVECTOR_DATA (val)[toindex++] = elt;
	  else if (BIT_VECTORP (val))
	    {
	      CHECK_BIT (elt);
	      set_bit_vector_bit (XBIT_VECTOR (val), toindex++, XINT (elt));
	    }
	  else
	    {
	      CHECK_CHAR_COERCE_INT (elt);
	      string_result_ptr += set_itext_ichar (string_result_ptr,
						       XCHAR (elt));
	    }
	}
      if (args_mse)
	{
	  args_mse[argnum].entry_offset =
	    string_prev_result_ptr - string_result;
	  args_mse[argnum].entry_length =
	    string_result_ptr - string_prev_result_ptr;
	}
    }

  /* Now we finally make the string. */
  if (target_type == c_string)
    {
      val = make_string (string_result, string_result_ptr - string_result);
      for (argnum = 0; argnum < nargs; argnum++)
	{
	  if (STRINGP (args_mse[argnum].string))
	    copy_string_extents (val, args_mse[argnum].string,
				 args_mse[argnum].entry_offset, 0,
				 args_mse[argnum].entry_length);
	}
    }

  if (!NILP (prev))
    XCDR (prev) = last_tail;

  unbind_to (sdep);
  RETURN_UNGCPRO (val);
}

DEFUN ("copy-alist", Fcopy_alist, 1, 1, 0, /*
Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared.
*/
       (alist))
{
  Lisp_Object tail;

  if (NILP (alist))
    return alist;
  CHECK_CONS (alist);

  alist = concat (1, &alist, c_cons, 0);
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object car = XCAR (tail);

      if (CONSP (car))
	XCAR (tail) = Fcons (XCAR (car), XCDR (car));
    }
  return alist;
}

DEFUN ("copy-tree", Fcopy_tree, 1, 2, 0, /*
Return a copy of a list and substructures.
The argument is copied, and any lists contained within it are copied
recursively.  Circularities and shared substructures are not preserved.
Second arg VECP causes vectors to be copied, too.  Strings and bit vectors
are not copied.
*/
       (arg, vecp))
{
  return safe_copy_tree (arg, vecp, 0);
}

Lisp_Object
safe_copy_tree (Lisp_Object arg, Lisp_Object vecp, int depth)
{
  if (depth > 200)
    stack_overflow ("Stack overflow in copy-tree", arg);
    
  if (CONSP (arg))
    {
      Lisp_Object rest;
      rest = arg = Fcopy_sequence (arg);
      while (CONSP (rest))
	{
	  Lisp_Object elt = XCAR (rest);
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XCAR (rest) = safe_copy_tree (elt, vecp, depth + 1);
	  if (VECTORP (XCDR (rest))) /* hack for (a b . [c d]) */
	    XCDR (rest) = safe_copy_tree (XCDR (rest), vecp, depth +1);
	  rest = XCDR (rest);
	}
    }
  else if (VECTORP (arg) && ! NILP (vecp))
    {
      int i = XVECTOR_LENGTH (arg);
      int j;
      arg = Fcopy_sequence (arg);
      for (j = 0; j < i; j++)
	{
	  Lisp_Object elt = XVECTOR_DATA (arg) [j];
	  QUIT;
	  if (CONSP (elt) || VECTORP (elt))
	    XVECTOR_DATA (arg) [j] = safe_copy_tree (elt, vecp, depth + 1);
	}
    }
  return arg;
}

DEFUN ("substring", Fsubstring, 2, 3, 0, /*
Return the substring of STRING starting at START and ending before END.
END may be nil or omitted; then the substring runs to the end of STRING.
If START or END is negative, it counts from the end.
Relevant parts of the string-extent-data are copied to the new string.
*/
       (string, start, end))
{
  Charcount ccstart, ccend;
  Bytecount bstart, blen;
  Lisp_Object val;

  CHECK_STRING (string);
  CHECK_INT (start);
  get_string_range_char (string, start, end, &ccstart, &ccend,
			 GB_HISTORICAL_STRING_BEHAVIOR);
  bstart = string_index_char_to_byte (string, ccstart);
  blen = string_offset_char_to_byte_len (string, bstart, ccend - ccstart);
  val = make_string (XSTRING_DATA (string) + bstart, blen);
  /* Copy any applicable extent information into the new string. */
  copy_string_extents (val, string, 0, bstart, blen);
  return val;
}

DEFUN ("subseq", Fsubseq, 2, 3, 0, /*
Return the subsequence of SEQUENCE starting at START and ending before END.
END may be omitted; then the subsequence runs to the end of SEQUENCE.
If START or END is negative, it counts from the end.
The returned subsequence is always of the same type as SEQUENCE.
If SEQUENCE is a string, relevant parts of the string-extent-data
are copied to the new string.
*/
       (sequence, start, end))
{
  EMACS_INT len, s, e;

  CHECK_SEQUENCE (sequence);

  if (STRINGP (sequence))
    return Fsubstring (sequence, start, end);

  len = XINT (Flength (sequence));

  CHECK_INT (start);
  s = XINT (start);
  if (s < 0)
    s = len + s;

  if (NILP (end))
    e = len;
  else
    {
      CHECK_INT (end);
      e = XINT (end);
      if (e < 0)
	e = len + e;
    }

  if (!(0 <= s && s <= e && e <= len))
    args_out_of_range_3 (sequence, make_int (s), make_int (e));

  if (VECTORP (sequence))
    {
      Lisp_Object result = make_vector (e - s, Qnil);
      EMACS_INT i;
      Lisp_Object *in_elts  = XVECTOR_DATA (sequence);
      Lisp_Object *out_elts = XVECTOR_DATA (result);

      for (i = s; i < e; i++)
	out_elts[i - s] = in_elts[i];
      return result;
    }
  else if (LISTP (sequence))
    {
      Lisp_Object result = Qnil;
      EMACS_INT i;

      sequence = Fnthcdr (make_int (s), sequence);

      for (i = s; i < e; i++)
	{
	  result = Fcons (Fcar (sequence), result);
	  sequence = Fcdr (sequence);
	}

      return Fnreverse (result);
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Object result = make_bit_vector (e - s, Qzero);
      EMACS_INT i;

      for (i = s; i < e; i++)
	set_bit_vector_bit (XBIT_VECTOR (result), i - s,
			    bit_vector_bit (XBIT_VECTOR (sequence), i));
      return result;
    }
  else
    {
      ABORT (); /* unreachable, since CHECK_SEQUENCE (sequence) did not
                   error */
      return Qnil;
    }
}

/* Split STRING into a list of substrings.  The substrings are the
   parts of original STRING separated by SEPCHAR.  */
static Lisp_Object
split_string_by_ichar_1 (const Ibyte *string, Bytecount size,
			  Ichar sepchar)
{
  Lisp_Object result = Qnil;
  const Ibyte *end = string + size;

  while (1)
    {
      const Ibyte *p = string;
      while (p < end)
	{
	  if (itext_ichar (p) == sepchar)
	    break;
	  INC_IBYTEPTR (p);
	}
      result = Fcons (make_string (string, p - string), result);
      if (p < end)
	{
	  string = p;
	  INC_IBYTEPTR (string);	/* skip sepchar */
	}
      else
	break;
    }
  return Fnreverse (result);
}

/* The same as the above, except PATH is an external C string (it is
   converted using Qfile_name), and sepchar is hardcoded to SEPCHAR
   (':' or whatever).  */
Lisp_Object
split_external_path (const Extbyte *path)
{
  Bytecount newlen;
  Ibyte *newpath;
  if (!path)
    return Qnil;

  TO_INTERNAL_FORMAT (C_STRING, path, ALLOCA, (newpath, newlen), Qfile_name);

  /* #### Does this make sense?  It certainly does for
     split_env_path(), but it looks dubious here.  Does any code
     depend on split_external_path("") returning nil instead of an empty
     string?  */
  if (!newlen)
    return Qnil;

  return split_string_by_ichar_1 (newpath, newlen, SEPCHAR);
}

Lisp_Object
split_env_path (const CIbyte *evarname, const Ibyte *default_)
{
  const Ibyte *path = 0;
  if (evarname)
    path = egetenv (evarname);
  if (!path)
    path = default_;
  if (!path)
    return Qnil;
  return split_string_by_ichar_1 (path, qxestrlen (path), SEPCHAR);
}

/* Ben thinks this function should not exist or be exported to Lisp.
   We use it to define split-path-string in subr.el (not!).  */

DEFUN ("split-string-by-char", Fsplit_string_by_char, 2, 2, 0, /*
Split STRING into a list of substrings originally separated by SEPCHAR.
*/
       (string, sepchar))
{
  CHECK_STRING (string);
  CHECK_CHAR (sepchar);
  return split_string_by_ichar_1 (XSTRING_DATA (string),
				   XSTRING_LENGTH (string),
				   XCHAR (sepchar));
}

/* #### This was supposed to be in subr.el, but is used VERY early in
   the bootstrap process, so it goes here.  Damn.  */

DEFUN ("split-path", Fsplit_path, 1, 1, 0, /*
Explode a search path into a list of strings.
The path components are separated with the characters specified
with `path-separator'.
*/
       (path))
{
  CHECK_STRING (path);

  while (!STRINGP (Vpath_separator)
	 || (string_char_length (Vpath_separator) != 1))
    Vpath_separator = signal_continuable_error
      (Qinvalid_state,
       "`path-separator' should be set to a single-character string",
       Vpath_separator);

  return (split_string_by_ichar_1
	  (XSTRING_DATA (path), XSTRING_LENGTH (path),
	   itext_ichar (XSTRING_DATA (Vpath_separator))));
}


DEFUN ("nthcdr", Fnthcdr, 2, 2, 0, /*
Take cdr N times on LIST, and return the result.
*/
       (n, list))
{
  /* This function can GC */
  REGISTER EMACS_INT i;
  REGISTER Lisp_Object tail = list;
  CHECK_NATNUM (n);
  for (i = XINT (n); i; i--)
    {
      if (CONSP (tail))
	tail = XCDR (tail);
      else if (NILP (tail))
	return Qnil;
      else
	{
	  tail = wrong_type_argument (Qlistp, tail);
	  i++;
	}
    }
  return tail;
}

DEFUN ("nth", Fnth, 2, 2, 0, /*
Return the Nth element of LIST.
N counts from zero.  If LIST is not that long, nil is returned.
*/
       (n, list))
{
  /* This function can GC */
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, 2, 2, 0, /*
Return element of SEQUENCE at index N.
*/
       (sequence, n))
{
  /* This function can GC */
 retry:
  CHECK_INT_COERCE_CHAR (n); /* yuck! */
  if (LISTP (sequence))
    {
      Lisp_Object tem = Fnthcdr (n, sequence);
      /* #### Utterly, completely, fucking disgusting.
       * #### The whole point of "elt" is that it operates on
       * #### sequences, and does error- (bounds-) checking.
       */
      if (CONSP (tem))
	return XCAR (tem);
      else
#if 1
	/* This is The Way It Has Always Been. */
	return Qnil;
#else
        /* This is The Way Mly and Cltl2 say It Should Be. */
        args_out_of_range (sequence, n);
#endif
    }
  else if (STRINGP     (sequence) ||
           VECTORP     (sequence) ||
           BIT_VECTORP (sequence))
    return Faref (sequence, n);
#ifdef LOSING_BYTECODE
  else if (COMPILED_FUNCTIONP (sequence))
    {
      EMACS_INT idx = XINT (n);
      if (idx < 0)
        {
        lose:
          args_out_of_range (sequence, n);
        }
      /* Utter perversity */
      {
	Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (sequence);
        switch (idx)
          {
          case COMPILED_ARGLIST:
            return compiled_function_arglist (f);
          case COMPILED_INSTRUCTIONS:
            return compiled_function_instructions (f);
          case COMPILED_CONSTANTS:
            return compiled_function_constants (f);
          case COMPILED_STACK_DEPTH:
            return compiled_function_stack_depth (f);
          case COMPILED_DOC_STRING:
	    return compiled_function_documentation (f);
          case COMPILED_DOMAIN:
	    return compiled_function_domain (f);
          case COMPILED_INTERACTIVE:
	    if (f->flags.interactivep)
	      return compiled_function_interactive (f);
	    /* if we return nil, can't tell interactive with no args
	       from noninteractive. */
	    goto lose;
          default:
            goto lose;
          }
      }
    }
#endif /* LOSING_BYTECODE */
  else
    {
      check_losing_bytecode ("elt", sequence);
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
}

DEFUN ("last", Flast, 1, 2, 0, /*
Return the tail of list LIST, of length N (default 1).
LIST may be a dotted list, but not a circular list.
Optional argument N must be a non-negative integer.
If N is zero, then the atom that terminates the list is returned.
If N is greater than the length of LIST, then LIST itself is returned.
*/
       (list, n))
{
  EMACS_INT int_n, count;
  Lisp_Object retval, tortoise, hare;

  CHECK_LIST (list);

  if (NILP (n))
    int_n = 1;
  else
    {
      CHECK_NATNUM (n);
      int_n = XINT (n);
    }

  for (retval = tortoise = hare = list, count = 0;
       CONSP (hare);
       hare = XCDR (hare),
	 (int_n-- <= 0 ? ((void) (retval = XCDR (retval))) : (void)0),
	 count++)
    {
      if (count < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

      if (count & 1)
	tortoise = XCDR (tortoise);
      if (EQ (hare, tortoise))
	signal_circular_list_error (list);
    }

  return retval;
}

DEFUN ("nbutlast", Fnbutlast, 1, 2, 0, /*
Modify LIST to remove the last N (default 1) elements.
If LIST has N or fewer elements, nil is returned and LIST is unmodified.
*/
       (list, n))
{
  EMACS_INT int_n;

  CHECK_LIST (list);

  if (NILP (n))
    int_n = 1;
  else
    {
      CHECK_NATNUM (n);
      int_n = XINT (n);
    }

  {
    Lisp_Object last_cons = list;

    EXTERNAL_LIST_LOOP_1 (list)
      {
	if (int_n-- < 0)
	  last_cons = XCDR (last_cons);
      }

    if (int_n >= 0)
      return Qnil;

    XCDR (last_cons) = Qnil;
    return list;
  }
}

DEFUN ("butlast", Fbutlast, 1, 2, 0, /*
Return a copy of LIST with the last N (default 1) elements removed.
If LIST has N or fewer elements, nil is returned.
*/
       (list, n))
{
  EMACS_INT int_n;

  CHECK_LIST (list);

  if (NILP (n))
    int_n = 1;
  else
    {
      CHECK_NATNUM (n);
      int_n = XINT (n);
    }

  {
    Lisp_Object retval = Qnil;
    Lisp_Object tail = list;

    EXTERNAL_LIST_LOOP_1 (list)
      {
	if (--int_n < 0)
	  {
	    retval = Fcons (XCAR (tail), retval);
	    tail = XCDR (tail);
	  }
      }

    return Fnreverse (retval);
  }
}

DEFUN ("member", Fmember, 2, 2, 0, /*
Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_3 (list_elt, list, tail)
    {
      if (internal_equal (elt, list_elt, 0))
        return tail;
    }
  return Qnil;
}

DEFUN ("old-member", Fold_member, 2, 2, 0, /*
Return non-nil if ELT is an element of LIST.  Comparison done with `old-equal'.
The value is actually the tail of LIST whose car is ELT.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_3 (list_elt, list, tail)
    {
      if (internal_old_equal (elt, list_elt, 0))
        return tail;
    }
  return Qnil;
}

DEFUN ("memq", Fmemq, 2, 2, 0, /*
Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_3 (list_elt, list, tail)
    {
      if (EQ_WITH_EBOLA_NOTICE (elt, list_elt))
        return tail;
    }
  return Qnil;
}

DEFUN ("old-memq", Fold_memq, 2, 2, 0, /*
Return non-nil if ELT is an element of LIST.  Comparison done with `old-eq'.
The value is actually the tail of LIST whose car is ELT.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_3 (list_elt, list, tail)
    {
      if (HACKEQ_UNSAFE (elt, list_elt))
        return tail;
    }
  return Qnil;
}

Lisp_Object
memq_no_quit (Lisp_Object elt, Lisp_Object list)
{
  LIST_LOOP_3 (list_elt, list, tail)
    {
      if (EQ_WITH_EBOLA_NOTICE (elt, list_elt))
        return tail;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, 2, 2, 0, /*
Return non-nil if KEY is `equal' to the car of an element of ALIST.
The value is actually the element of ALIST whose car equals KEY.
*/
       (key, alist))
{
  /* This function can GC. */
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (internal_equal (key, elt_car, 0))
	return elt;
    }
  return Qnil;
}

DEFUN ("old-assoc", Fold_assoc, 2, 2, 0, /*
Return non-nil if KEY is `old-equal' to the car of an element of ALIST.
The value is actually the element of ALIST whose car equals KEY.
*/
       (key, alist))
{
  /* This function can GC. */
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (internal_old_equal (key, elt_car, 0))
	return elt;
    }
  return Qnil;
}

Lisp_Object
assoc_no_quit (Lisp_Object key, Lisp_Object alist)
{
  int speccount = specpdl_depth ();
  specbind (Qinhibit_quit, Qt);
  return unbind_to_1 (speccount, Fassoc (key, alist));
}

DEFUN ("assq", Fassq, 2, 2, 0, /*
Return non-nil if KEY is `eq' to the car of an element of ALIST.
The value is actually the element of ALIST whose car is KEY.
Elements of ALIST that are not conses are ignored.
*/
       (key, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (EQ_WITH_EBOLA_NOTICE (key, elt_car))
	return elt;
    }
  return Qnil;
}

DEFUN ("old-assq", Fold_assq, 2, 2, 0, /*
Return non-nil if KEY is `old-eq' to the car of an element of ALIST.
The value is actually the element of ALIST whose car is KEY.
Elements of ALIST that are not conses are ignored.
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
       (key, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (HACKEQ_UNSAFE (key, elt_car))
	return elt;
    }
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (Lisp_Object key, Lisp_Object alist)
{
  /* This cannot GC. */
  LIST_LOOP_2 (elt, alist)
    {
      Lisp_Object elt_car = XCAR (elt);
      if (EQ_WITH_EBOLA_NOTICE (key, elt_car))
	return elt;
    }
  return Qnil;
}

DEFUN ("rassoc", Frassoc, 2, 2, 0, /*
Return non-nil if VALUE is `equal' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr equals VALUE.
*/
       (value, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (internal_equal (value, elt_cdr, 0))
	return elt;
    }
  return Qnil;
}

DEFUN ("old-rassoc", Fold_rassoc, 2, 2, 0, /*
Return non-nil if VALUE is `old-equal' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr equals VALUE.
*/
       (value, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (internal_old_equal (value, elt_cdr, 0))
	return elt;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, 2, 2, 0, /*
Return non-nil if VALUE is `eq' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr is VALUE.
*/
       (value, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (EQ_WITH_EBOLA_NOTICE (value, elt_cdr))
	return elt;
    }
  return Qnil;
}

DEFUN ("old-rassq", Fold_rassq, 2, 2, 0, /*
Return non-nil if VALUE is `old-eq' to the cdr of an element of ALIST.
The value is actually the element of ALIST whose cdr is VALUE.
*/
       (value, alist))
{
  EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
    {
      if (HACKEQ_UNSAFE (value, elt_cdr))
	return elt;
    }
  return Qnil;
}

/* Like Frassq, but caller must ensure that ALIST is properly
   nil-terminated and ebola-free. */
Lisp_Object
rassq_no_quit (Lisp_Object value, Lisp_Object alist)
{
  LIST_LOOP_2 (elt, alist)
    {
      Lisp_Object elt_cdr = XCDR (elt);
      if (EQ_WITH_EBOLA_NOTICE (value, elt_cdr))
	return elt;
    }
  return Qnil;
}


DEFUN ("delete", Fdelete, 2, 2, 0, /*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (delete element foo))' to be sure
of changing the value of `foo'.
Also see: `remove'.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (list_elt, list,
				(internal_equal (elt, list_elt, 0)));
  return list;
}

DEFUN ("old-delete", Fold_delete, 2, 2, 0, /*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `old-equal'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (old-delete element foo))' to be sure
of changing the value of `foo'.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (list_elt, list,
				(internal_old_equal (elt, list_elt, 0)));
  return list;
}

DEFUN ("delq", Fdelq, 2, 2, 0, /*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `eq'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (delq element foo))' to be sure of
changing the value of `foo'.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (list_elt, list,
				(EQ_WITH_EBOLA_NOTICE (elt, list_elt)));
  return list;
}

DEFUN ("old-delq", Fold_delq, 2, 2, 0, /*
Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `old-eq'.
If the first member of LIST is ELT, there is no way to remove it by side
effect; therefore, write `(setq foo (old-delq element foo))' to be sure of
changing the value of `foo'.
*/
       (elt, list))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (list_elt, list,
				(HACKEQ_UNSAFE (elt, list_elt)));
  return list;
}

/* Like Fdelq, but caller must ensure that LIST is properly
   nil-terminated and ebola-free. */

Lisp_Object
delq_no_quit (Lisp_Object elt, Lisp_Object list)
{
  LIST_LOOP_DELETE_IF (list_elt, list,
		       (EQ_WITH_EBOLA_NOTICE (elt, list_elt)));
  return list;
}

/* Be VERY careful with this.  This is like delq_no_quit() but
   also calls free_cons() on the removed conses.  You must be SURE
   that no pointers to the freed conses remain around (e.g.
   someone else is pointing to part of the list).  This function
   is useful on internal lists that are used frequently and where
   the actual list doesn't escape beyond known code bounds. */

Lisp_Object
delq_no_quit_and_free_cons (Lisp_Object elt, Lisp_Object list)
{
  REGISTER Lisp_Object tail = list;
  REGISTER Lisp_Object prev = Qnil;

  while (!NILP (tail))
    {
      REGISTER Lisp_Object tem = XCAR (tail);
      if (EQ (elt, tem))
	{
	  Lisp_Object cons_to_free = tail;
	  if (NILP (prev))
	    list = XCDR (tail);
	  else
	    XCDR (prev) = XCDR (tail);
	  tail = XCDR (tail);
	  free_cons (cons_to_free);
	}
      else
	{
	  prev = tail;
	  tail = XCDR (tail);
	}
    }
  return list;
}

DEFUN ("remassoc", Fremassoc, 2, 2, 0, /*
Delete by side effect any elements of ALIST whose car is `equal' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassoc key foo))' to be sure of changing
the value of `foo'.
*/
       (key, alist))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (elt, alist,
				(CONSP (elt) &&
				 internal_equal (key, XCAR (elt), 0)));
  return alist;
}

Lisp_Object
remassoc_no_quit (Lisp_Object key, Lisp_Object alist)
{
  int speccount = specpdl_depth ();
  specbind (Qinhibit_quit, Qt);
  return unbind_to_1 (speccount, Fremassoc (key, alist));
}

DEFUN ("remassq", Fremassq, 2, 2, 0, /*
Delete by side effect any elements of ALIST whose car is `eq' to KEY.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassq key foo))' to be sure of changing
the value of `foo'.
*/
       (key, alist))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (elt, alist,
				(CONSP (elt) &&
				 EQ_WITH_EBOLA_NOTICE (key, XCAR (elt))));
  return alist;
}

/* no quit, no errors; be careful */

Lisp_Object
remassq_no_quit (Lisp_Object key, Lisp_Object alist)
{
  LIST_LOOP_DELETE_IF (elt, alist,
		       (CONSP (elt) &&
			EQ_WITH_EBOLA_NOTICE (key, XCAR (elt))));
  return alist;
}

DEFUN ("remrassoc", Fremrassoc, 2, 2, 0, /*
Delete by side effect any elements of ALIST whose cdr is `equal' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `equal' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassoc value foo))' to be sure of changing
the value of `foo'.
*/
       (value, alist))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (elt, alist,
				(CONSP (elt) &&
				 internal_equal (value, XCDR (elt), 0)));
  return alist;
}

DEFUN ("remrassq", Fremrassq, 2, 2, 0, /*
Delete by side effect any elements of ALIST whose cdr is `eq' to VALUE.
The modified ALIST is returned.  If the first member of ALIST has a car
that is `eq' to VALUE, there is no way to remove it by side effect;
therefore, write `(setq foo (remrassq value foo))' to be sure of changing
the value of `foo'.
*/
       (value, alist))
{
  EXTERNAL_LIST_LOOP_DELETE_IF (elt, alist,
				(CONSP (elt) &&
				 EQ_WITH_EBOLA_NOTICE (value, XCDR (elt))));
  return alist;
}

/* Like Fremrassq, fast and unsafe; be careful */
Lisp_Object
remrassq_no_quit (Lisp_Object value, Lisp_Object alist)
{
  LIST_LOOP_DELETE_IF (elt, alist,
		       (CONSP (elt) &&
			EQ_WITH_EBOLA_NOTICE (value, XCDR (elt))));
  return alist;
}

DEFUN ("nreverse", Fnreverse, 1, 1, 0, /*
Reverse LIST by destructively modifying cdr pointers.
Return the beginning of the reversed list.
Also see: `reverse'.
*/
       (list))
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object prev = Qnil;
  Lisp_Object tail = list;

  /* We gcpro our args; see `nconc' */
  GCPRO2 (prev, tail);
  while (!NILP (tail))
    {
      REGISTER Lisp_Object next;
      CONCHECK_CONS (tail);
      next = XCDR (tail);
      XCDR (tail) = prev;
      prev = tail;
      tail = next;
    }
  UNGCPRO;
  return prev;
}

DEFUN ("reverse", Freverse, 1, 1, 0, /*
Reverse LIST, copying.  Return the beginning of the reversed list.
See also the function `nreverse', which is used more often.
*/
       (list))
{
  Lisp_Object reversed_list = Qnil;
  EXTERNAL_LIST_LOOP_2 (elt, list)
    {
      reversed_list = Fcons (elt, reversed_list);
    }
  return reversed_list;
}

static Lisp_Object list_merge (Lisp_Object org_l1, Lisp_Object org_l2,
                               Lisp_Object lisp_arg,
                               int (*pred_fn) (Lisp_Object, Lisp_Object,
                                               Lisp_Object lisp_arg));

/* The sort function should return > 0 if OBJ1 < OBJ2, < 0 otherwise.
   NOTE: This is backwards from the way qsort() works. */

Lisp_Object
list_sort (Lisp_Object list,
           Lisp_Object lisp_arg,
           int (*pred_fn) (Lisp_Object obj1, Lisp_Object obj2,
                           Lisp_Object lisp_arg))
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object back, tem;
  Lisp_Object front = list;
  Lisp_Object len = Flength (list);

  if (XINT (len) < 2)
    return list;

  len = make_int (XINT (len) / 2 - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO3 (front, back, lisp_arg);
  front = list_sort (front, lisp_arg, pred_fn);
  back = list_sort (back, lisp_arg, pred_fn);
  UNGCPRO;
  return list_merge (front, back, lisp_arg, pred_fn);
}


static int
merge_pred_function (Lisp_Object obj1, Lisp_Object obj2,
                     Lisp_Object pred)
{
  Lisp_Object tmp;

  /* prevents the GC from happening in call2 */
  /* Emacs' GC doesn't actually relocate pointers, so this probably
     isn't strictly necessary */
  int speccount = begin_gc_forbidden ();
  tmp = call2 (pred, obj1, obj2);
  unbind_to (speccount);

  if (NILP (tmp))
    return -1;
  else
    return 1;
}

DEFUN ("sort", Fsort, 2, 2, 0, /*
Sort LIST, stably, comparing elements using PREDICATE.
Returns the sorted list.  LIST is modified by side effects.
PREDICATE is called with two elements of LIST, and should return T
if the first element is "less" than the second.
*/
       (list, predicate))
{
  return list_sort (list, predicate, merge_pred_function);
}

Lisp_Object
merge (Lisp_Object org_l1, Lisp_Object org_l2,
       Lisp_Object pred)
{
  return list_merge (org_l1, org_l2, pred, merge_pred_function);
}


static Lisp_Object
list_merge (Lisp_Object org_l1, Lisp_Object org_l2,
            Lisp_Object lisp_arg,
            int (*pred_fn) (Lisp_Object, Lisp_Object, Lisp_Object lisp_arg))
{
  Lisp_Object value;
  Lisp_Object tail;
  Lisp_Object tem;
  Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */

  GCPRO4 (org_l1, org_l2, lisp_arg, value);

  while (1)
    {
      if (NILP (l1))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NILP (l2))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}

      if (((*pred_fn) (Fcar (l2), Fcar (l1), lisp_arg)) < 0)
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NILP (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}


/************************************************************************/
/*	  	        property-list functions				*/
/************************************************************************/

/* For properties of text, we need to do order-insensitive comparison of
   plists.  That is, we need to compare two plists such that they are the
   same if they have the same set of keys, and equivalent values.
   So (a 1 b 2) would be equal to (b 2 a 1).

   NIL_MEANS_NOT_PRESENT is as in `plists-eq' etc.
   LAXP means use `equal' for comparisons.
 */
int
plists_differ (Lisp_Object a, Lisp_Object b, int nil_means_not_present,
	       int laxp, int depth, int foldcase)
{
  int eqp = (depth == -1);	/* -1 as depth means use eq, not equal. */
  int la, lb, m, i, fill;
  Lisp_Object *keys, *vals;
  Boolbyte *flags;
  Lisp_Object rest;

  if (NILP (a) && NILP (b))
    return 0;

  Fcheck_valid_plist (a);
  Fcheck_valid_plist (b);

  la = XINT (Flength (a));
  lb = XINT (Flength (b));
  m = (la > lb ? la : lb);
  fill = 0;
  keys  = alloca_array (Lisp_Object, m);
  vals  = alloca_array (Lisp_Object, m);
  flags = alloca_array (Boolbyte, m);

  /* First extract the pairs from A. */
  for (rest = a; !NILP (rest); rest = XCDR (XCDR (rest)))
    {
      Lisp_Object k = XCAR (rest);
      Lisp_Object v = XCAR (XCDR (rest));
      /* Maybe be Ebolified. */
      if (nil_means_not_present && NILP (v)) continue;
      keys [fill] = k;
      vals [fill] = v;
      flags[fill] = 0;
      fill++;
    }
  /* Now iterate over B, and stop if we find something that's not in A,
     or that doesn't match.  As we match, mark them. */
  for (rest = b; !NILP (rest); rest = XCDR (XCDR (rest)))
    {
      Lisp_Object k = XCAR (rest);
      Lisp_Object v = XCAR (XCDR (rest));
      /* Maybe be Ebolified. */
      if (nil_means_not_present && NILP (v)) continue;
      for (i = 0; i < fill; i++)
	{
	  if (!laxp ? EQ (k, keys [i]) :
	      internal_equal_0 (k, keys [i], depth, foldcase))
	    {
	      if (eqp
		  /* We narrowly escaped being Ebolified here. */
		  ? !EQ_WITH_EBOLA_NOTICE (v, vals [i])
		  : !internal_equal_0 (v, vals [i], depth, foldcase))
		/* a property in B has a different value than in A */
		goto MISMATCH;
	      flags [i] = 1;
	      break;
	    }
	}
      if (i == fill)
	/* there are some properties in B that are not in A */
	goto MISMATCH;
    }
  /* Now check to see that all the properties in A were also in B */
  for (i = 0; i < fill; i++)
    if (flags [i] == 0)
      goto MISMATCH;

  /* Ok. */
  return 0;

 MISMATCH:
  return 1;
}

DEFUN ("plists-eq", Fplists_eq, 2, 3, 0, /*
Return non-nil if property lists A and B are `eq'.
A property list is an alternating list of keywords and values.
 This function does order-insensitive comparisons of the property lists:
 For example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
 Comparison between values is done using `eq'.  See also `plists-equal'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is ignored.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.
*/
       (a, b, nil_means_not_present))
{
  return (plists_differ (a, b, !NILP (nil_means_not_present), 0, -1, 0)
	  ? Qnil : Qt);
}

DEFUN ("plists-equal", Fplists_equal, 2, 3, 0, /*
Return non-nil if property lists A and B are `equal'.
A property list is an alternating list of keywords and values.  This
 function does order-insensitive comparisons of the property lists: For
 example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
 Comparison between values is done using `equal'.  See also `plists-eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is ignored.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.
*/
       (a, b, nil_means_not_present))
{
  return (plists_differ (a, b, !NILP (nil_means_not_present), 0, 1, 0)
	  ? Qnil : Qt);
}


DEFUN ("lax-plists-eq", Flax_plists_eq, 2, 3, 0, /*
Return non-nil if lax property lists A and B are `eq'.
A property list is an alternating list of keywords and values.
 This function does order-insensitive comparisons of the property lists:
 For example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
 Comparison between values is done using `eq'.  See also `plists-equal'.
A lax property list is like a regular one except that comparisons between
 keywords is done using `equal' instead of `eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is ignored.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.
*/
       (a, b, nil_means_not_present))
{
  return (plists_differ (a, b, !NILP (nil_means_not_present), 1, -1, 0)
	  ? Qnil : Qt);
}

DEFUN ("lax-plists-equal", Flax_plists_equal, 2, 3, 0, /*
Return non-nil if lax property lists A and B are `equal'.
A property list is an alternating list of keywords and values.  This
 function does order-insensitive comparisons of the property lists: For
 example, the property lists '(a 1 b 2) and '(b 2 a 1) are equal.
 Comparison between values is done using `equal'.  See also `plists-eq'.
A lax property list is like a regular one except that comparisons between
 keywords is done using `equal' instead of `eq'.
If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is ignored.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.
*/
       (a, b, nil_means_not_present))
{
  return (plists_differ (a, b, !NILP (nil_means_not_present), 1, 1, 0)
	  ? Qnil : Qt);
}

/* Return the value associated with key PROPERTY in property list PLIST.
   Return nil if key not found.  This function is used for internal
   property lists that cannot be directly manipulated by the user.
   */

Lisp_Object
internal_plist_get (Lisp_Object plist, Lisp_Object property)
{
  Lisp_Object tail;

  for (tail = plist; !NILP (tail); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), property))
	return XCAR (XCDR (tail));
    }

  return Qunbound;
}

/* Set PLIST's value for PROPERTY to VALUE.  Analogous to
   internal_plist_get(). */

void
internal_plist_put (Lisp_Object *plist, Lisp_Object property,
		    Lisp_Object value)
{
  Lisp_Object tail;

  for (tail = *plist; !NILP (tail); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), property))
	{
	  XCAR (XCDR (tail)) = value;
	  return;
	}
    }

  *plist = Fcons (property, Fcons (value, *plist));
}

int
internal_remprop (Lisp_Object *plist, Lisp_Object property)
{
  Lisp_Object tail, prev;

  for (tail = *plist, prev = Qnil;
       !NILP (tail);
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), property))
	{
	  if (NILP (prev))
	    *plist = XCDR (XCDR (tail));
	  else
	    XCDR (XCDR (prev)) = XCDR (XCDR (tail));
	  return 1;
	}
      else
	prev = tail;
    }

  return 0;
}

/* Called on a malformed property list.  BADPLACE should be some
   place where truncating will form a good list -- i.e. we shouldn't
   result in a list with an odd length. */

static Lisp_Object
bad_bad_bunny (Lisp_Object *plist, Lisp_Object *badplace, Error_Behavior errb)
{
  if (ERRB_EQ (errb, ERROR_ME))
    return Fsignal (Qmalformed_property_list, list2 (*plist, *badplace));
  else
    {
      if (ERRB_EQ (errb, ERROR_ME_WARN))
	{
	  warn_when_safe_lispobj
	    (Qlist, Qwarning,
	     list2 (build_msg_string
		    ("Malformed property list -- list has been truncated"),
		    *plist));
	  /* #### WARNING: This is more dangerous than it seems; perhaps
             not a good idea.  It also violates the principle of least
             surprise -- passing in ERROR_ME_WARN causes truncation, but
             ERROR_ME and ERROR_ME_NOT don't. */
	  *badplace = Qnil;
	}
      return Qunbound;
    }
}

/* Called on a circular property list.  BADPLACE should be some place
   where truncating will result in an even-length list, as above.
   If doesn't particularly matter where we truncate -- anywhere we
   truncate along the entire list will break the circularity, because
   it will create a terminus and the list currently doesn't have one.
*/

static Lisp_Object
bad_bad_turtle (Lisp_Object *plist, Lisp_Object *badplace, Error_Behavior errb)
{
  if (ERRB_EQ (errb, ERROR_ME))
    return Fsignal (Qcircular_property_list, list1 (*plist));
  else
    {
      if (ERRB_EQ (errb, ERROR_ME_WARN))
	{
	  warn_when_safe_lispobj
	    (Qlist, Qwarning,
	     list2 (build_msg_string
		    ("Circular property list -- list has been truncated"),
		    *plist));
	  /* #### WARNING: This is more dangerous than it seems; perhaps
             not a good idea.  It also violates the principle of least
             surprise -- passing in ERROR_ME_WARN causes truncation, but
             ERROR_ME and ERROR_ME_NOT don't. */
	  *badplace = Qnil;
	}
      return Qunbound;
    }
}

/* Advance the tortoise pointer by two (one iteration of a property-list
   loop) and the hare pointer by four and verify that no malformations
   or circularities exist.  If so, return zero and store a value into
   RETVAL that should be returned by the calling function.  Otherwise,
   return 1.  See external_plist_get().
 */

static int
advance_plist_pointers (Lisp_Object *plist,
			Lisp_Object **tortoise, Lisp_Object **hare,
			Error_Behavior errb, Lisp_Object *retval)
{
  int i;
  Lisp_Object *tortsave = *tortoise;

  /* Note that our "fixing" may be more brutal than necessary,
     but it's the user's own problem, not ours, if they went in and
     manually fucked up a plist. */

  for (i = 0; i < 2; i++)
    {
      /* This is a standard iteration of a defensive-loop-checking
         loop.  We just do it twice because we want to advance past
	 both the property and its value.

	 If the pointer indirection is confusing you, remember that
	 one level of indirection on the hare and tortoise pointers
	 is only due to pass-by-reference for this function.  The other
	 level is so that the plist can be fixed in place. */

      /* When we reach the end of a well-formed plist, **HARE is
	 nil.  In that case, we don't do anything at all except
	 advance TORTOISE by one.  Otherwise, we advance HARE
	 by two (making sure it's OK to do so), then advance
	 TORTOISE by one (it will always be OK to do so because
	 the HARE is always ahead of the TORTOISE and will have
	 already verified the path), then make sure TORTOISE and
	 HARE don't contain the same non-nil object -- if the
	 TORTOISE and the HARE ever meet, then obviously we're
	 in a circularity, and if we're in a circularity, then
	 the TORTOISE and the HARE can't cross paths without
	 meeting, since the HARE only gains one step over the
	 TORTOISE per iteration. */

      if (!NILP (**hare))
	{
	  Lisp_Object *haresave = *hare;
	  if (!CONSP (**hare))
	    {
	      *retval = bad_bad_bunny (plist, haresave, errb);
	      return 0;
	    }
	  *hare = &XCDR (**hare);
	  /* In a non-plist, we'd check here for a nil value for
	     **HARE, which is OK (it just means the list has an
	     odd number of elements).  In a plist, it's not OK
	     for the list to have an odd number of elements. */
	  if (!CONSP (**hare))
	    {
	      *retval = bad_bad_bunny (plist, haresave, errb);
	      return 0;
	    }
	  *hare = &XCDR (**hare);
	}

      *tortoise = &XCDR (**tortoise);
      if (!NILP (**hare) && EQ (**tortoise, **hare))
	{
	  *retval = bad_bad_turtle (plist, tortsave, errb);
	  return 0;
	}
    }

  return 1;
}

/* Return the value of PROPERTY from PLIST, or Qunbound if
   property is not on the list.

   PLIST is a Lisp-accessible property list, meaning that it
   has to be checked for malformations and circularities.

   If ERRB is ERROR_ME, an error will be signalled.  Otherwise, the
   function will never signal an error; and if ERRB is ERROR_ME_WARN,
   on finding a malformation or a circularity, it issues a warning and
   attempts to silently fix the problem.

   A pointer to PLIST is passed in so that PLIST can be successfully
   "fixed" even if the error is at the beginning of the plist. */

Lisp_Object
external_plist_get (Lisp_Object *plist, Lisp_Object property,
		    int laxp, Error_Behavior errb)
{
  Lisp_Object *tortoise = plist;
  Lisp_Object *hare = plist;

  while (!NILP (*tortoise))
    {
      Lisp_Object *tortsave = tortoise;
      Lisp_Object retval;

      /* We do the standard tortoise/hare march.  We isolate the
	 grungy stuff to do this in advance_plist_pointers(), though.
	 To us, all this function does is advance the tortoise
	 pointer by two and the hare pointer by four and make sure
	 everything's OK.  We first advance the pointers and then
	 check if a property matched; this ensures that our
	 check for a matching property is safe. */

      if (!advance_plist_pointers (plist, &tortoise, &hare, errb, &retval))
	return retval;

      if (!laxp ? EQ (XCAR (*tortsave), property)
	  : internal_equal (XCAR (*tortsave), property, 0))
	return XCAR (XCDR (*tortsave));
    }

  return Qunbound;
}

/* Set PLIST's value for PROPERTY to VALUE, given a possibly
   malformed or circular plist.  Analogous to external_plist_get(). */

void
external_plist_put (Lisp_Object *plist, Lisp_Object property,
		    Lisp_Object value, int laxp, Error_Behavior errb)
{
  Lisp_Object *tortoise = plist;
  Lisp_Object *hare = plist;

  while (!NILP (*tortoise))
    {
      Lisp_Object *tortsave = tortoise;
      Lisp_Object retval;

      /* See above */
      if (!advance_plist_pointers (plist, &tortoise, &hare, errb, &retval))
	return;

      if (!laxp ? EQ (XCAR (*tortsave), property)
	  : internal_equal (XCAR (*tortsave), property, 0))
	{
	  XCAR (XCDR (*tortsave)) = value;
	  return;
	}
    }

  *plist = Fcons (property, Fcons (value, *plist));
}

int
external_remprop (Lisp_Object *plist, Lisp_Object property,
		  int laxp, Error_Behavior errb)
{
  Lisp_Object *tortoise = plist;
  Lisp_Object *hare = plist;

  while (!NILP (*tortoise))
    {
      Lisp_Object *tortsave = tortoise;
      Lisp_Object retval;

      /* See above */
      if (!advance_plist_pointers (plist, &tortoise, &hare, errb, &retval))
	return 0;

      if (!laxp ? EQ (XCAR (*tortsave), property)
	  : internal_equal (XCAR (*tortsave), property, 0))
	{
	  /* Now you see why it's so convenient to have that level
	     of indirection. */
	  *tortsave = XCDR (XCDR (*tortsave));
	  return 1;
	}
    }

  return 0;
}

DEFUN ("plist-get", Fplist_get, 2, 3, 0, /*
Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...).
PROPERTY is usually a symbol.
This function returns the value corresponding to the PROPERTY,
or DEFAULT if PROPERTY is not one of the properties on the list.
*/
       (plist, property, default_))
{
  Lisp_Object value = external_plist_get (&plist, property, 0, ERROR_ME);
  return UNBOUNDP (value) ? default_ : value;
}

DEFUN ("plist-put", Fplist_put, 3, 3, 0, /*
Change value in PLIST of PROPERTY to VALUE.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2 ...).
PROPERTY is usually a symbol and VALUE is any object.
If PROPERTY is already a property on the list, its value is set to VALUE,
otherwise the new PROPERTY VALUE pair is added.
The new plist is returned; use `(setq x (plist-put x property value))'
to be sure to use the new value.  PLIST is modified by side effect.
*/
       (plist, property, value))
{
  external_plist_put (&plist, property, value, 0, ERROR_ME);
  return plist;
}

DEFUN ("plist-remprop", Fplist_remprop, 2, 2, 0, /*
Remove from PLIST the property PROPERTY and its value.
PLIST is a property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2 ...).
PROPERTY is usually a symbol.
The new plist is returned; use `(setq x (plist-remprop x property))'
to be sure to use the new value.  PLIST is modified by side effect.
*/
       (plist, property))
{
  external_remprop (&plist, property, 0, ERROR_ME);
  return plist;
}

DEFUN ("plist-member", Fplist_member, 2, 2, 0, /*
Return t if PROPERTY has a value specified in PLIST.
*/
       (plist, property))
{
  Lisp_Object value = Fplist_get (plist, property, Qunbound);
  return UNBOUNDP (value) ? Qnil : Qt;
}

DEFUN ("check-valid-plist", Fcheck_valid_plist, 1, 1, 0, /*
Given a plist, signal an error if there is anything wrong with it.
This means that it's a malformed or circular plist.
*/
       (plist))
{
  Lisp_Object *tortoise;
  Lisp_Object *hare;

 start_over:
  tortoise = &plist;
  hare = &plist;
  while (!NILP (*tortoise))
    {
      Lisp_Object retval;

      /* See above */
      if (!advance_plist_pointers (&plist, &tortoise, &hare, ERROR_ME,
				   &retval))
	goto start_over;
    }

  return Qnil;
}

DEFUN ("valid-plist-p", Fvalid_plist_p, 1, 1, 0, /*
Given a plist, return non-nil if its format is correct.
If it returns nil, `check-valid-plist' will signal an error when given
the plist; that means it's a malformed or circular plist.
*/
       (plist))
{
  Lisp_Object *tortoise;
  Lisp_Object *hare;

  tortoise = &plist;
  hare = &plist;
  while (!NILP (*tortoise))
    {
      Lisp_Object retval;

      /* See above */
      if (!advance_plist_pointers (&plist, &tortoise, &hare, ERROR_ME_NOT,
				   &retval))
	return Qnil;
    }

  return Qt;
}

DEFUN ("canonicalize-plist", Fcanonicalize_plist, 1, 2, 0, /*
Destructively remove any duplicate entries from a plist.
In such cases, the first entry applies.

If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is removed.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.

The new plist is returned.  If NIL-MEANS-NOT-PRESENT is given, the
 return value may not be EQ to the passed-in value, so make sure to
 `setq' the value back into where it came from.
*/
       (plist, nil_means_not_present))
{
  Lisp_Object head = plist;

  Fcheck_valid_plist (plist);

  while (!NILP (plist))
    {
      Lisp_Object prop = Fcar (plist);
      Lisp_Object next = Fcdr (plist);

      CHECK_CONS (next); /* just make doubly sure we catch any errors */
      if (!NILP (nil_means_not_present) && NILP (Fcar (next)))
	{
	  if (EQ (head, plist))
	    head = Fcdr (next);
	  plist = Fcdr (next);
	  continue;
	}
      /* external_remprop returns 1 if it removed any property.
	 We have to loop till it didn't remove anything, in case
	 the property occurs many times. */
      while (external_remprop (&XCDR (next), prop, 0, ERROR_ME))
	DO_NOTHING;
      plist = Fcdr (next);
    }

  return head;
}

DEFUN ("lax-plist-get", Flax_plist_get, 2, 3, 0, /*
Extract a value from a lax property list.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol.
This function returns the value corresponding to PROPERTY,
or DEFAULT if PROPERTY is not one of the properties on the list.
*/
       (lax_plist, property, default_))
{
  Lisp_Object value = external_plist_get (&lax_plist, property, 1, ERROR_ME);
  return UNBOUNDP (value) ? default_ : value;
}

DEFUN ("lax-plist-put", Flax_plist_put, 3, 3, 0, /*
Change value in LAX-PLIST of PROPERTY to VALUE.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol and VALUE is any object.
If PROPERTY is already a property on the list, its value is set to
VALUE, otherwise the new PROPERTY VALUE pair is added.
The new plist is returned; use `(setq x (lax-plist-put x property value))'
to be sure to use the new value.  LAX-PLIST is modified by side effect.
*/
       (lax_plist, property, value))
{
  external_plist_put (&lax_plist, property, value, 1, ERROR_ME);
  return lax_plist;
}

DEFUN ("lax-plist-remprop", Flax_plist_remprop, 2, 2, 0, /*
Remove from LAX-PLIST the property PROPERTY and its value.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
PROPERTY is usually a symbol.
The new plist is returned; use `(setq x (lax-plist-remprop x property))'
to be sure to use the new value.  LAX-PLIST is modified by side effect.
*/
       (lax_plist, property))
{
  external_remprop (&lax_plist, property, 1, ERROR_ME);
  return lax_plist;
}

DEFUN ("lax-plist-member", Flax_plist_member, 2, 2, 0, /*
Return t if PROPERTY has a value specified in LAX-PLIST.
LAX-PLIST is a lax property list, which is a list of the form
\(PROPERTY1 VALUE1 PROPERTY2 VALUE2...), where comparisons between
properties is done using `equal' instead of `eq'.
*/
       (lax_plist, property))
{
  return UNBOUNDP (Flax_plist_get (lax_plist, property, Qunbound)) ? Qnil : Qt;
}

DEFUN ("canonicalize-lax-plist", Fcanonicalize_lax_plist, 1, 2, 0, /*
Destructively remove any duplicate entries from a lax plist.
In such cases, the first entry applies.

If optional arg NIL-MEANS-NOT-PRESENT is non-nil, then a property with
 a nil value is removed.  This feature is a virus that has infected
 old Lisp implementations, but should not be used except for backward
 compatibility.

The new plist is returned.  If NIL-MEANS-NOT-PRESENT is given, the
 return value may not be EQ to the passed-in value, so make sure to
 `setq' the value back into where it came from.
*/
       (lax_plist, nil_means_not_present))
{
  Lisp_Object head = lax_plist;

  Fcheck_valid_plist (lax_plist);

  while (!NILP (lax_plist))
    {
      Lisp_Object prop = Fcar (lax_plist);
      Lisp_Object next = Fcdr (lax_plist);

      CHECK_CONS (next); /* just make doubly sure we catch any errors */
      if (!NILP (nil_means_not_present) && NILP (Fcar (next)))
	{
	  if (EQ (head, lax_plist))
	    head = Fcdr (next);
	  lax_plist = Fcdr (next);
	  continue;
	}
      /* external_remprop returns 1 if it removed any property.
	 We have to loop till it didn't remove anything, in case
	 the property occurs many times. */
      while (external_remprop (&XCDR (next), prop, 1, ERROR_ME))
	DO_NOTHING;
      lax_plist = Fcdr (next);
    }

  return head;
}

/* In C because the frame props stuff uses it */

DEFUN ("destructive-alist-to-plist", Fdestructive_alist_to_plist, 1, 1, 0, /*
Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is destroyed in the process of constructing the plist.
See also `alist-to-plist'.
*/
       (alist))
{
  Lisp_Object head = alist;
  while (!NILP (alist))
    {
      /* remember the alist element. */
      Lisp_Object el = Fcar (alist);

      Fsetcar (alist, Fcar (el));
      Fsetcar (el, Fcdr (el));
      Fsetcdr (el, Fcdr (alist));
      Fsetcdr (alist, el);
      alist = Fcdr (Fcdr (alist));
    }

  return head;
}

DEFUN ("get", Fget, 2, 3, 0, /*
Return the value of OBJECT's PROPERTY property.
This is the last VALUE stored with `(put OBJECT PROPERTY VALUE)'.
If there is no such property, return optional third arg DEFAULT
\(which defaults to `nil').  OBJECT can be a symbol, string, extent,
face, or glyph.  See also `put', `remprop', and `object-plist'.
*/
       (object, property, default_))
{
  /* Various places in emacs call Fget() and expect it not to quit,
     so don't quit. */
  Lisp_Object val;

  if (LRECORDP (object) && XRECORD_LHEADER_IMPLEMENTATION (object)->getprop)
    val = XRECORD_LHEADER_IMPLEMENTATION (object)->getprop (object, property);
  else
    invalid_operation ("Object type has no properties", object);

  return UNBOUNDP (val) ? default_ : val;
}

DEFUN ("put", Fput, 3, 3, 0, /*
Set OBJECT's PROPERTY to VALUE.
It can be subsequently retrieved with `(get OBJECT PROPERTY)'.
OBJECT can be a symbol, face, extent, or string.
For a string, no properties currently have predefined meanings.
For the predefined properties for extents, see `set-extent-property'.
For the predefined properties for faces, see `set-face-property'.
See also `get', `remprop', and `object-plist'.
*/
       (object, property, value))
{
  /* This function cannot GC */
  CHECK_LISP_WRITEABLE (object);

  if (LRECORDP (object) && XRECORD_LHEADER_IMPLEMENTATION (object)->putprop)
    {
      if (! XRECORD_LHEADER_IMPLEMENTATION (object)->putprop
	  (object, property, value))
	invalid_change ("Can't set property on object", property);
    }
  else
    invalid_change ("Object type has no settable properties", object);

  return value;
}

DEFUN ("remprop", Fremprop, 2, 2, 0, /*
Remove, from OBJECT's property list, PROPERTY and its corresponding value.
OBJECT can be a symbol, string, extent, face, or glyph.  Return non-nil
if the property list was actually modified (i.e. if PROPERTY was present
in the property list).  See also `get', `put', and `object-plist'.
*/
       (object, property))
{
  int ret = 0;

  CHECK_LISP_WRITEABLE (object);

  if (LRECORDP (object) && XRECORD_LHEADER_IMPLEMENTATION (object)->remprop)
    {
      ret = XRECORD_LHEADER_IMPLEMENTATION (object)->remprop (object, property);
      if (ret == -1)
	invalid_change ("Can't remove property from object", property);
    }
  else
    invalid_change ("Object type has no removable properties", object);

  return ret ? Qt : Qnil;
}

DEFUN ("object-plist", Fobject_plist, 1, 1, 0, /*
Return a property list of OBJECT's properties.
For a symbol, this is equivalent to `symbol-plist'.
OBJECT can be a symbol, string, extent, face, or glyph.
Do not modify the returned property list directly;
this may or may not have the desired effects.  Use `put' instead.
*/
       (object))
{
  if (LRECORDP (object) && XRECORD_LHEADER_IMPLEMENTATION (object)->plist)
    return XRECORD_LHEADER_IMPLEMENTATION (object)->plist (object);
  else
    invalid_operation ("Object type has no properties", object);

  return Qnil;
}


static Lisp_Object
tweaked_internal_equal (Lisp_Object obj1, Lisp_Object obj2,
			Lisp_Object depth)
{
  return make_int (internal_equal (obj1, obj2, XINT (depth)));
}

int
internal_equal_trapping_problems (Lisp_Object warning_class,
				  const Ascbyte *warning_string,
				  int flags,
				  struct call_trapping_problems_result *p,
				  int retval,
				  Lisp_Object obj1, Lisp_Object obj2,
				  int depth)
{
  Lisp_Object glorp =
    va_call_trapping_problems (warning_class, warning_string,
			       flags, p,
			       (lisp_fn_t) tweaked_internal_equal,
			       3, obj1, obj2, make_int (depth));
  if (UNBOUNDP (glorp))
    return retval;
  else
    return XINT (glorp);
}

int
internal_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  if (depth > 200)
    stack_overflow ("Stack overflow in equal", Qunbound);
  QUIT;
  if (EQ_WITH_EBOLA_NOTICE (obj1, obj2))
    return 1;
  /* Note that (equal 20 20.0) should be nil */
  if (XTYPE (obj1) != XTYPE (obj2))
    return 0;
  if (LRECORDP (obj1))
    {
      const struct lrecord_implementation
	*imp1 = XRECORD_LHEADER_IMPLEMENTATION (obj1),
	*imp2 = XRECORD_LHEADER_IMPLEMENTATION (obj2);

      return (imp1 == imp2) &&
	/* EQ-ness of the objects was noticed above */
	(imp1->equal && (imp1->equal) (obj1, obj2, depth, 0));
    }

  return 0;
}

enum array_type
  {
    ARRAY_NONE = 0,
    ARRAY_STRING,
    ARRAY_VECTOR,
    ARRAY_BIT_VECTOR
  };

static enum array_type
array_type (Lisp_Object obj)
{
  if (STRINGP (obj))
    return ARRAY_STRING;
  if (VECTORP (obj))
    return ARRAY_VECTOR;
  if (BIT_VECTORP (obj))
    return ARRAY_BIT_VECTOR;
  return ARRAY_NONE;
}

int
internal_equalp (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  if (depth > 200)
    stack_overflow ("Stack overflow in equalp", Qunbound);
  QUIT;

  /* 1. Objects that are `eq' are equal.  This will catch the common case
     of two equal fixnums or the same object seen twice. */
  if (EQ_WITH_EBOLA_NOTICE (obj1, obj2))
    return 1;

  /* 2. If both numbers, compare with `='. */
  if (NUMBERP (obj1) && NUMBERP (obj2))
    {
      return (0 == bytecode_arithcompare (obj1, obj2));
    }

  /* 3. If characters, compare case-insensitively. */
  if (CHARP (obj1) && CHARP (obj2))
    return CANONCASE (0, XCHAR (obj1)) == CANONCASE (0, XCHAR (obj2));

  /* 4. If arrays of different types, compare their lengths, and
        then compare element-by-element. */
  {
    enum array_type artype1, artype2;
    artype1 = array_type (obj1);
    artype2 = array_type (obj2);
    if (artype1 != artype2 && artype1 && artype2)
      {
	EMACS_INT i;
	EMACS_INT l1 = XINT (Flength (obj1));
	EMACS_INT l2 = XINT (Flength (obj2));
	/* Both arrays, but of different lengths */
	if (l1 != l2)
	  return 0;
	for (i = 0; i < l1; i++)
	  if (!internal_equalp (Faref (obj1, make_int (i)),
				Faref (obj2, make_int (i)), depth + 1))
	    return 0;
	return 1;
      }
  }
  /* 5. Else, they must be the same type.  If so, call the equal() method,
        telling it to fold case.  For objects that care about case-folding
	their contents, the equal() method will call internal_equal_0(). */
  if (XTYPE (obj1) != XTYPE (obj2))
    return 0;
  if (LRECORDP (obj1))
    {
      const struct lrecord_implementation
	*imp1 = XRECORD_LHEADER_IMPLEMENTATION (obj1),
	*imp2 = XRECORD_LHEADER_IMPLEMENTATION (obj2);

      return (imp1 == imp2) &&
	/* EQ-ness of the objects was noticed above */
	(imp1->equal && (imp1->equal) (obj1, obj2, depth, 1));
    }

  return 0;
}

int
internal_equal_0 (Lisp_Object obj1, Lisp_Object obj2, int depth, int foldcase)
{
  if (foldcase)
    return internal_equalp (obj1, obj2, depth);
  else
    return internal_equal (obj1, obj2, depth);
}

/* Note that we may be calling sub-objects that will use
   internal_equal() (instead of internal_old_equal()).  Oh well.
   We will get an Ebola note if there's any possibility of confusion,
   but that seems unlikely. */

static int
internal_old_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  if (depth > 200)
    stack_overflow ("Stack overflow in equal", Qunbound);
  QUIT;
  if (HACKEQ_UNSAFE (obj1, obj2))
    return 1;
  /* Note that (equal 20 20.0) should be nil */
  if (XTYPE (obj1) != XTYPE (obj2))
    return 0;

  return internal_equal (obj1, obj2, depth);
}

DEFUN ("equal", Fequal, 2, 2, 0, /*
Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
Conses are compared by comparing the cars and the cdrs.
Vectors and strings are compared element by element.
Numbers are compared by value.  Symbols must match exactly.
*/
       (object1, object2))
{
  return internal_equal (object1, object2, 0) ? Qt : Qnil;
}

DEFUN ("equalp", Fequalp, 2, 2, 0, /*
Return t if two Lisp objects have similar structure and contents.

This is like `equal', except that it accepts numerically equal
numbers of different types (float, integer, bignum, bigfloat), and also
compares strings and characters case-insensitively.

Type objects that are arrays (that is, strings, bit-vectors, and vectors)
of the same length and with contents that are `equalp' are themselves
`equalp', regardless of whether the two objects have the same type.

Other objects whose primary purpose is as containers of other objects are
`equalp' if they would otherwise be equal (same length, type, etc.) and
their contents are `equalp'.  This goes for conses, weak lists,
weak boxes, ephemerons, specifiers, hash tables, char tables and range
tables.  However, objects that happen to contain other objects but are not
primarily designed for this purpose (e.g. compiled functions, events or
display-related objects such as glyphs, faces or extents) are currently
compared using `equalp' the same way as using `equal'.

More specifically, two hash tables are `equalp' if they have the same test
(see `hash-table-test'), the same number of entries, and the same value for
`hash-table-weakness', and if, for each entry in one hash table, its key is
equivalent to a key in the other hash table using the hash table test, and
its value is `equalp' to the other hash table's value for that key.
*/
       (object1, object2))
{
  return internal_equalp (object1, object2, 0) ? Qt : Qnil;
}

DEFUN ("old-equal", Fold_equal, 2, 2, 0, /*
Return t if two Lisp objects have similar structure and contents.
They must have the same data type.
\(Note, however, that an exception is made for characters and integers;
this is known as the "char-int confoundance disease." See `eq' and
`old-eq'.)
This function is provided only for byte-code compatibility with v19.
Do not use it.
*/
       (object1, object2))
{
  return internal_old_equal (object1, object2, 0) ? Qt : Qnil;
}


DEFUN ("fillarray", Ffillarray, 2, 2, 0, /*
Destructively modify ARRAY by replacing each element with ITEM.
ARRAY is a vector, bit vector, or string.
*/
       (array, item))
{
 retry:
  if (STRINGP (array))
    {
      Bytecount old_bytecount = XSTRING_LENGTH (array);
      Bytecount new_bytecount;
      Bytecount item_bytecount;
      Ibyte item_buf[MAX_ICHAR_LEN];
      Ibyte *p;
      Ibyte *end;

      CHECK_CHAR_COERCE_INT (item);

      CHECK_LISP_WRITEABLE (array);
      sledgehammer_check_ascii_begin (array);
      item_bytecount = set_itext_ichar (item_buf, XCHAR (item));
      new_bytecount = item_bytecount * (Bytecount) string_char_length (array);

      resize_string (array, -1, new_bytecount - old_bytecount);

      for (p = XSTRING_DATA (array), end = p + new_bytecount;
	   p < end;
	   p += item_bytecount)
	memcpy (p, item_buf, item_bytecount);
      *p = '\0';

      XSET_STRING_ASCII_BEGIN (array,
			       item_bytecount == 1 ?
			       min (new_bytecount, MAX_STRING_ASCII_BEGIN) :
			       0);
      bump_string_modiff (array);
      sledgehammer_check_ascii_begin (array);
    }
  else if (VECTORP (array))
    {
      Lisp_Object *p = XVECTOR_DATA (array);
      Elemcount len = XVECTOR_LENGTH (array);
      CHECK_LISP_WRITEABLE (array);
      while (len--)
	*p++ = item;
    }
  else if (BIT_VECTORP (array))
    {
      Lisp_Bit_Vector *v = XBIT_VECTOR (array);
      Elemcount len = bit_vector_length (v);
      int bit;
      CHECK_BIT (item);
      bit = XINT (item);
      CHECK_LISP_WRITEABLE (array);
      while (len--)
	set_bit_vector_bit (v, len, bit);
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
  return array;
}

Lisp_Object
nconc2 (Lisp_Object arg1, Lisp_Object arg2)
{
  Lisp_Object args[2];
  struct gcpro gcpro1;
  args[0] = arg1;
  args[1] = arg2;

  GCPRO1 (args[0]);
  gcpro1.nvars = 2;

  RETURN_UNGCPRO (bytecode_nconc2 (args));
}

Lisp_Object
bytecode_nconc2 (Lisp_Object *args)
{
 retry:

  if (CONSP (args[0]))
    {
      /* (setcdr (last args[0]) args[1]) */
      Lisp_Object tortoise, hare;
      Elemcount count;

      for (hare = tortoise = args[0], count = 0;
	   CONSP (XCDR (hare));
	   hare = XCDR (hare), count++)
	{
	  if (count < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

	  if (count & 1)
	    tortoise = XCDR (tortoise);
	  if (EQ (hare, tortoise))
	    signal_circular_list_error (args[0]);
	}
      XCDR (hare) = args[1];
      return args[0];
    }
  else if (NILP (args[0]))
    {
      return args[1];
    }
  else
    {
      args[0] = wrong_type_argument (args[0], Qlistp);
      goto retry;
    }
}

DEFUN ("nconc", Fnconc, 0, MANY, 0, /*
Concatenate any number of lists by altering them.
Only the last argument is not altered, and need not be a list.
Also see: `append'.
If the first argument is nil, there is no way to modify it by side
effect; therefore, write `(setq foo (nconc foo list))' to be sure of
changing the value of `foo'.

arguments: (&rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  int argnum = 0;
  struct gcpro gcpro1;

  /* The modus operandi in Emacs is "caller gc-protects args".
     However, nconc (particularly nconc2 ()) is called many times
     in Emacs on freshly created stuff (e.g. you see the idiom
     nconc2 (Fcopy_sequence (foo), bar) a lot).  So we help those
     callers out by protecting the args ourselves to save them
     a lot of temporary-variable grief. */

  GCPRO1 (args[0]);
  gcpro1.nvars = nargs;

  while (argnum < nargs)
    {
      Lisp_Object val;
    retry:
      val = args[argnum];
      if (CONSP (val))
	{
	  /* `val' is the first cons, which will be our return value.  */
	  /* `last_cons' will be the cons cell to mutate.  */
	  Lisp_Object last_cons = val;
	  Lisp_Object tortoise = val;

	  for (argnum++; argnum < nargs; argnum++)
	    {
	      Lisp_Object next = args[argnum];
	    retry_next:
	      if (CONSP (next) || argnum == nargs -1)
		{
		  /* (setcdr (last val) next) */
		  Elemcount count;

		  for (count = 0;
		       CONSP (XCDR (last_cons));
		       last_cons = XCDR (last_cons), count++)
		    {
		      if (count < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

		      if (count & 1)
			tortoise = XCDR (tortoise);
		      if (EQ (last_cons, tortoise))
			signal_circular_list_error (args[argnum-1]);
		    }
		  XCDR (last_cons) = next;
		}
	      else if (NILP (next))
		{
		  continue;
		}
	      else
		{
		  next = wrong_type_argument (Qlistp, next);
		  goto retry_next;
		}
	    }
	  RETURN_UNGCPRO (val);
        }
      else if (NILP (val))
	argnum++;
      else if (argnum == nargs - 1) /* last arg? */
	RETURN_UNGCPRO (val);
      else
	{
	  args[argnum] = wrong_type_argument (Qlistp, val);
	  goto retry;
	}
    }
  RETURN_UNGCPRO (Qnil);  /* No non-nil args provided. */
}


/* This is the guts of several mapping functions.

   Call FUNCTION CALL_COUNT times, with NSEQUENCES arguments each time,
   taking the elements from SEQUENCES.  If VALS is non-NULL, store the
   results into VALS, a C array of Lisp_Objects; else, if LISP_VALS is
   non-nil, store the results into LISP_VALS, a sequence with sufficient
   room for CALL_COUNT results. Else, do not accumulate any result.

   If VALS is non-NULL, NSEQUENCES is one, and SEQUENCES[0] is a cons,
   mapcarX will store the elements of SEQUENCES[0] in stack and GCPRO them,
   so FUNCTION cannot insert a non-cons into SEQUENCES[0] and throw off
   mapcarX.

   Otherwise, mapcarX signals a wrong-type-error if it encounters a
   non-cons, non-array when traversing SEQUENCES.  Common Lisp specifies in
   MAPPING-DESTRUCTIVE-INTERACTION that it is an error when FUNCTION
   destructively modifies SEQUENCES in a way that might affect the ongoing
   traversal operation.

   If SOME_OR_EVERY is SOME_OR_EVERY_SOME, return the (possibly multiple)
   values given by FUNCTION the first time it is non-nil, and abandon the
   iterations.  LISP_VALS in this case must be an object created by
   make_opaque_ptr, dereferenced as pointing to a Lisp object. If
   SOME_OR_EVERY is SOME_OR_EVERY_EVERY, store Qnil at the Lisp_Object
   pointer address provided by LISP_VALS if FUNCTION gives nil; otherwise
   leave it alone. */

#define SOME_OR_EVERY_NEITHER 0
#define SOME_OR_EVERY_SOME    1
#define SOME_OR_EVERY_EVERY   2

static void
mapcarX (Elemcount call_count, Lisp_Object *vals, Lisp_Object lisp_vals,
	 Lisp_Object function, int nsequences, Lisp_Object *sequences, 
	 int some_or_every)
{
  Lisp_Object called, *args;
  struct gcpro gcpro1, gcpro2;
  int i, j;
  enum lrecord_type lisp_vals_type;

  assert (LRECORDP (lisp_vals));
  lisp_vals_type = (enum lrecord_type) XRECORD_LHEADER (lisp_vals)->type;

  args = alloca_array (Lisp_Object, nsequences + 1);
  args[0] = function;
  for (i = 1; i <= nsequences; ++i)
    {
      args[i] = Qnil;
    }

  if (vals != NULL)
    {
      GCPRO2 (args[0], vals[0]);
      gcpro1.nvars = nsequences + 1;
      gcpro2.nvars = 0;
    }
  else
    {
      GCPRO1 (args[0]);
      gcpro1.nvars = nsequences + 1;
    }

  /* Be extra nice in the event that we've been handed one list and one
     only; make it possible for FUNCTION to set cdrs not yet processed to
     non-cons, non-nil objects without ill-effect, if we have been handed
     the stack space to do that. */
  if (vals != NULL && 1 == nsequences && CONSP (sequences[0]))
    {
      Lisp_Object lst = sequences[0];
      Lisp_Object *val = vals;
      for (i = 0; i < call_count; ++i)
	{
	  *val++ = XCAR (lst);
	  lst = XCDR (lst);
	}
      gcpro2.nvars = call_count;

      for (i = 0; i < call_count; ++i)
	{
	  args[1] = vals[i];
	  vals[i] = Ffuncall (nsequences + 1, args);
	}
    }
  else
    {
      Binbyte *sequence_types = alloca_array (Binbyte, nsequences);
      for (j = 0; j < nsequences; ++j)
	{
	  sequence_types[j] = XRECORD_LHEADER (sequences[j])->type;
	}

      for (i = 0; i < call_count; ++i)
	{
	  for (j = 0; j < nsequences; ++j)
	    {
	      switch (sequence_types[j])
		{
		case lrecord_type_cons:
		  {
		    if (!CONSP (sequences[j]))
		      {
			/* This means FUNCTION has probably messed
			   around with a cons in one of the sequences,
			   since we checked the type
			   (CHECK_SEQUENCE()) and the length and
			   structure (with Flength()) correctly in our
			   callers. */
			dead_wrong_type_argument (Qconsp, sequences[j]);
		      }
		    args[j + 1] = XCAR (sequences[j]);
		    sequences[j] = XCDR (sequences[j]);
		    break;
		  }
		case lrecord_type_vector:
		  {
		    args[j + 1] = XVECTOR_DATA (sequences[j])[i];
		    break;
		  }
		case lrecord_type_string:
		  {
		    args[j + 1] = make_char (string_ichar (sequences[j], i));
		    break;
		  }
		case lrecord_type_bit_vector:
		  {
		    args[j + 1]
		      = make_int (bit_vector_bit (XBIT_VECTOR (sequences[j]),
						  i));
		    break;
		  }
		default:
		  ABORT();
		}
	    }
	  called = Ffuncall (nsequences + 1, args);
	  if (vals != NULL)
	    {
	      vals[i] = IGNORE_MULTIPLE_VALUES (called);
	      gcpro2.nvars += 1;
	    }
	  else
	    {
	      switch (lisp_vals_type)
		{
		case lrecord_type_symbol:
		  break;
		case lrecord_type_cons:
		  {
		    if (SOME_OR_EVERY_NEITHER == some_or_every)
		      {
			called = IGNORE_MULTIPLE_VALUES (called);
			if (!CONSP (lisp_vals))
			  {
			    /* If FUNCTION has inserted a non-cons non-nil
			       cdr into the list before we've processed the
			       relevant part, error. */
			    dead_wrong_type_argument (Qconsp, lisp_vals);
			  }

			XSETCAR (lisp_vals, called);
			lisp_vals = XCDR (lisp_vals);
			break;
		      }

		    if (SOME_OR_EVERY_SOME == some_or_every)
		      {
			if (!NILP (IGNORE_MULTIPLE_VALUES (called)))
			  {
			    XCAR (lisp_vals) = called;
			    UNGCPRO;
			    return;
			  }
			break;
		      }

		    if (SOME_OR_EVERY_EVERY == some_or_every)
		      {
			called = IGNORE_MULTIPLE_VALUES (called);
			if (NILP (called))
			  {
			    XCAR (lisp_vals) = Qnil;
			    UNGCPRO;
			    return;
			  }
			break;
		      }

		    goto bad_show_or_every_flag;
		  }
		case lrecord_type_vector:
		  {
		    called = IGNORE_MULTIPLE_VALUES (called);
		    i < XVECTOR_LENGTH (lisp_vals) ?
		      (XVECTOR_DATA (lisp_vals)[i] = called) :
		      /* Let #'aset error. */
		      Faset (lisp_vals, make_int (i), called);
		    break;
		  }
		case lrecord_type_string:
		  {
		    /* If this ever becomes a code hotspot, we can keep
		       around pointers into the data of the string, checking
		       each time that it hasn't been relocated. */
		    called = IGNORE_MULTIPLE_VALUES (called);
		    Faset (lisp_vals, make_int (i), called);
		    break;
		  }
		case lrecord_type_bit_vector:
		  {
		    called = IGNORE_MULTIPLE_VALUES (called);
		    (BITP (called) &&
		     i < bit_vector_length (XBIT_VECTOR (lisp_vals))) ?
		      set_bit_vector_bit (XBIT_VECTOR (lisp_vals), i,
					  XINT (called)) :
		      (void) Faset (lisp_vals, make_int (i), called);
		    break;
		  }
		bad_show_or_every_flag:
		default:
		  {
		    ABORT();
		    break;
		  }
		}
	    }
	}
    }
  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, 3, MANY, 0, /*
Call FUNCTION on each element of SEQUENCE, and concat results to a string.
Between each pair of results, insert SEPARATOR.

Each result, and SEPARATOR, should be strings.  Thus, using " " as SEPARATOR
results in spaces between the values returned by FUNCTION.  SEQUENCE itself
may be a list, a vector, a bit vector, or a string.

With optional SEQUENCES, call FUNCTION each time with as many arguments as
there are SEQUENCES, plus one for the element from SEQUENCE.  One element
from each sequence will be used each time FUNCTION is called, and
`mapconcat' will give up once the shortest sequence is exhausted.

arguments: (FUNCTION SEQUENCE SEPARATOR &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object function = args[0];
  Lisp_Object sequence = args[1];
  Lisp_Object separator = args[2];
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object *args0;
  EMACS_INT i, nargs0;

  args[2] = sequence;
  args[1] = separator;

  for (i = 2; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  if (len == 0) return build_ascstring ("");

  nargs0 = len + len - 1;
  args0 = alloca_array (Lisp_Object, nargs0);

  /* Special-case this, it's very common and doesn't require any
     funcalls. Upside of doing it here, instead of cl-macs.el: no consing,
     apart from the final string, we allocate everything on the stack. */
  if (EQ (function, Qidentity) && 3 == nargs && CONSP (sequence))
    {
      for (i = 0; i < len; ++i)
	{
	  args0[i] = XCAR (sequence);
	  sequence = XCDR (sequence);
	}
    }
  else
    {
      mapcarX (len, args0, Qnil, function, nargs - 2, args + 2,
	       SOME_OR_EVERY_NEITHER);
    }

  for (i = len - 1; i >= 0; i--)
    args0[i + i] = args0[i];

  for (i = 1; i < nargs0; i += 2)
    args0[i] = separator;

  return Fconcat (nargs0, args0);
}

DEFUN ("mapcar*", FmapcarX, 2, MANY, 0, /*
Call FUNCTION on each element of SEQUENCE; return a list of the results.
The result is a list of the same length as SEQUENCE.
SEQUENCE may be a list, a vector, a bit vector, or a string.

With optional SEQUENCES, call FUNCTION each time with as many arguments as
there are SEQUENCES, plus one for the element from SEQUENCE.  One element
from each sequence will be used each time FUNCTION is called, and `mapcar'
stops calling FUNCTION once the shortest sequence is exhausted.

arguments: (FUNCTION SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object function = args[0];
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object *args0;
  int i;

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  args0 = alloca_array (Lisp_Object, len);
  mapcarX (len, args0, Qnil, function, nargs - 1, args + 1,
	   SOME_OR_EVERY_NEITHER);

  return Flist ((int) len, args0);
}

DEFUN ("mapvector", Fmapvector, 2, MANY, 0, /*
Call FUNCTION on each element of SEQUENCE; return a vector of the results.
The result is a vector of the same length as SEQUENCE.
SEQUENCE may be a list, a vector, a bit vector, or a string.

With optional SEQUENCES, call FUNCTION each time with as many arguments as
there are SEQUENCES, plus one for the element from SEQUENCE.  One element
from each sequence will be used each time FUNCTION is called, and
`mapvector' stops calling FUNCTION once the shortest sequence is exhausted.

arguments: (FUNCTION SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object function = args[0];
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object result;
  struct gcpro gcpro1;
  int i;

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  result = make_vector (len, Qnil);
  GCPRO1 (result);
  /* Don't pass result as the lisp_object argument, we want mapcarX to protect 
     a single list argument's elements from being garbage-collected. */
  mapcarX (len, XVECTOR_DATA (result), Qnil, function, nargs - 1, args +1,
	   SOME_OR_EVERY_NEITHER);
  UNGCPRO;

  return result;
}

DEFUN ("mapcan", Fmapcan, 2, MANY, 0, /*
Call FUNCTION on each element of SEQUENCE; chain the results together.

FUNCTION must normally return a list; the results will be concatenated
together using `nconc'.

With optional SEQUENCES, call FUNCTION each time with as many arguments as
there are SEQUENCES, plus one for the element from SEQUENCE.  One element
from each sequence will be used each time FUNCTION is called, and
`mapcan' stops calling FUNCTION once the shortest sequence is exhausted.

arguments: (FUNCTION SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object function = args[0], nconcing;
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object *args0;
  struct gcpro gcpro1;
  int i;

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  args0 = alloca_array (Lisp_Object, len + 1);
  mapcarX (len, args0 + 1, Qnil, function, nargs - 1, args + 1,
	   SOME_OR_EVERY_NEITHER);

  if (len < 2)
    {
      return len ? args0[1] : Qnil;
    }

  /* bytecode_nconc2 can signal and return, we need to GCPRO the args, since
     mapcarX is no longer doing this for us. */
  args0[0] = Fcons (Qnil, Qnil);
  GCPRO1 (args0[0]);
  gcpro1.nvars = len + 1;

  for (i = 0; i < len; ++i)
    {
      nconcing = bytecode_nconc2 (args0 + i);
      args0[i + 1] = nconcing;
    }

  RETURN_UNGCPRO (XCDR (nconcing));
}

DEFUN ("mapc", Fmapc, 2, MANY, 0, /*
Call FUNCTION on each element of SEQUENCE.

SEQUENCE may be a list, a vector, a bit vector, or a string.
This function is like `mapcar' but does not accumulate the results,
which is more efficient if you do not use the results.

With optional SEQUENCES, call FUNCTION each time with as many arguments as
there are SEQUENCES, plus one for the elements from SEQUENCE.  One element
from each sequence will be used each time FUNCTION is called, and
`mapc' stops calling FUNCTION once the shortest sequence is exhausted.

Return SEQUENCE.

arguments: (FUNCTION SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object sequence = args[1];
  struct gcpro gcpro1;
  int i;

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  /* We need to GCPRO sequence, because mapcarX will modify the
     elements of the args array handed to it, and this may involve
     elements of sequence getting garbage collected. */
  GCPRO1 (sequence);
  mapcarX (len, NULL, Qnil, args[0], nargs - 1, args + 1,
	   SOME_OR_EVERY_NEITHER);
  RETURN_UNGCPRO (sequence);
}

DEFUN ("map", Fmap, 3, MANY, 0, /*
Map FUNCTION across one or more sequences, returning a sequence.

TYPE is the sequence type to return, FUNCTION is the function, SEQUENCE is
the first argument sequence, SEQUENCES are the other argument sequences.

FUNCTION will be called with (1+ (length SEQUENCES)) arguments, and must be
capable of accepting this number of arguments.

Certain TYPEs are recognised internally by `map', but others are not, and
`coerce' may throw an error on an attempt to convert to a TYPE it does not
understand.  A null TYPE means do not accumulate any values.

arguments: (TYPE FUNCTION SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object type = args[0];
  Lisp_Object function = args[1];
  Lisp_Object result = Qnil;
  Lisp_Object *args0 = NULL;
  Elemcount len = EMACS_INT_MAX;
  int i;
  struct gcpro gcpro1;

  for (i = 2; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  if (!NILP (type))
    {
      args0 = alloca_array (Lisp_Object, len);
    }

  mapcarX (len, args0, Qnil, function, nargs - 2, args + 2,
	   SOME_OR_EVERY_NEITHER);

  if (EQ (type, Qnil))
    {
      return result;
    }

  if (EQ (type, Qvector) || EQ (type, Qarray))
    {
      result = Fvector (len, args0);
    }
  else if (EQ (type, Qstring))
    {
      result = Fstring (len, args0);
    }
  else if (EQ (type, Qlist))
    {
      result = Flist (len, args0);
    }
  else if (EQ (type, Qbit_vector))
    {
      result = Fbit_vector (len, args0);
    }
  else
    {
      result = Flist (len, args0);
      GCPRO1 (result);
      result = call2 (Qcoerce, result, type);
      UNGCPRO;
    }

  return result;
}

DEFUN ("map-into", Fmap_into, 2, MANY, 0, /*
Modify RESULT-SEQUENCE using the return values of FUNCTION on SEQUENCES.

RESULT-SEQUENCE and SEQUENCES can be lists or arrays.

FUNCTION must accept at least as many arguments as there are SEQUENCES
\(possibly zero).  If RESULT-SEQUENCE and the elements of SEQUENCES are not
the same length, stop when the shortest is exhausted; any elements of
RESULT-SEQUENCE beyond that are unmodified.

Return RESULT-SEQUENCE.

arguments: (RESULT-SEQUENCE FUNCTION &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object result_sequence = args[0];
  Lisp_Object function = args[1];
  int i;

  args[0] = function;
  args[1] = result_sequence;

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  mapcarX (len, NULL, result_sequence, function, nargs - 2, args + 2,
	   SOME_OR_EVERY_NEITHER);

  return result_sequence;
}

DEFUN ("some", Fsome, 2, MANY, 0, /* 
Return true if PREDICATE gives non-nil for an element of SEQUENCE.

If so, return the value (possibly multiple) given by PREDICATE.

With optional SEQUENCES, call PREDICATE each time with as many arguments as
there are SEQUENCES (plus one for the element from SEQUENCE).

arguments: (PREDICATE SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object result_box = Fcons (Qnil, Qnil);
  struct gcpro gcpro1;
  Elemcount len = EMACS_INT_MAX;
  int i;

  GCPRO1 (result_box);

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  mapcarX (len, NULL, result_box, args[0], nargs - 1, args +1,
	   SOME_OR_EVERY_SOME);

  RETURN_UNGCPRO (XCAR (result_box));
}

DEFUN ("every", Fevery, 2, MANY, 0, /* 
Return true if PREDICATE is true of every element of SEQUENCE.

With optional SEQUENCES, call PREDICATE each time with as many arguments as
there are SEQUENCES (plus one for the element from SEQUENCE).

In contrast to `some', `every' never returns multiple values.

arguments: (PREDICATE SEQUENCE &rest SEQUENCES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object result_box = Fcons (Qt, Qnil);
  struct gcpro gcpro1;
  Elemcount len = EMACS_INT_MAX;
  int i;

  GCPRO1 (result_box);

  for (i = 1; i < nargs; ++i)
    {
      CHECK_SEQUENCE (args[i]);
      len = min (len, XINT (Flength (args[i])));
    }

  mapcarX (len, NULL, result_box, args[0], nargs - 1, args +1,
	   SOME_OR_EVERY_EVERY);

  RETURN_UNGCPRO (XCAR (result_box));
}

/* Call FUNCTION with NLISTS arguments repeatedly, each Nth argument
   corresponding to the result of calling (nthcdr ITERATION-COUNT LISTS[N]),
   until that #'nthcdr expression gives nil for some element of LISTS.

   If MAPLP is zero, return LISTS[0]. Otherwise, return a list of the return
   values from FUNCTION; if NCONCP is non-zero, nconc them together.

   In contrast to mapcarX, we don't require our callers to check LISTS for
   well-formedness, we signal wrong-type-argument if it's not a list, or
   circular-list if it's circular. */

static Lisp_Object
maplist (Lisp_Object function, int nlists, Lisp_Object *lists, int maplp,
	 int nconcp)
{
  Lisp_Object result = maplp ? lists[0] : Fcons (Qnil, Qnil), funcalled;
  Lisp_Object nconcing[2], accum = result, *args;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int i, j, continuing = (nlists > 0), called_count = 0;

  args = alloca_array (Lisp_Object, nlists + 1);
  args[0] = function;
  for (i = 1; i <= nlists; ++i)
    {
      args[i] = Qnil;
    }

  if (nconcp)
    {
      nconcing[0] = result;
      nconcing[1] = Qnil;
      GCPRO3 (args[0], nconcing[0], result);
      gcpro1.nvars = 1;
      gcpro2.nvars = 2;
    }
  else
    {
      GCPRO2 (args[0], result);
      gcpro1.nvars = 1;
    }

  while (continuing)
    {
      for (j = 0; j < nlists; ++j)
	{
	  if (CONSP (lists[j]))
	    {
	      args[j + 1] = lists[j];
	      lists[j] = XCDR (lists[j]);
	    }
	  else if (NILP (lists[j]))
	    {
	      continuing = 0;
	      break;
	    }
	  else
	    {
	      dead_wrong_type_argument (Qlistp, lists[j]);
	    }
	}
      if (!continuing) break;
      funcalled = IGNORE_MULTIPLE_VALUES (Ffuncall (nlists + 1, args));
      if (!maplp)
	{
	  if (nconcp)
	    {
	      /* This order of calls means we check that each list is
		 well-formed once and once only. The last result does
		 not have to be a list. */
	      nconcing[1] = funcalled;
	      nconcing[0] = bytecode_nconc2 (nconcing);
	    }
	  else
	    {
	      /* Add to the end, avoiding the need to call nreverse
		 once we're done: */
	      XSETCDR (accum, Fcons (funcalled, Qnil));
	      accum = XCDR (accum);
	    }
	}

      if (++called_count % CIRCULAR_LIST_SUSPICION_LENGTH) continue;

      for (j = 0; j < nlists; ++j)
	{
	  EXTERNAL_LIST_LOOP_1 (lists[j])
	    {
	      /* Just check the lists aren't circular, using the
		 EXTERNAL_LIST_LOOP_1 macro. */
	    }
	}
    }

  if (!maplp)
    {
      result = XCDR (result);
    }

  RETURN_UNGCPRO (result);
}

DEFUN ("maplist", Fmaplist, 2, MANY, 0, /*
Call FUNCTION on each sublist of LIST and LISTS.
Like `mapcar', except applies to lists and their cdr's rather than to
the elements themselves."

arguments: (FUNCTION LIST &rest LISTS)
*/
       (int nargs, Lisp_Object *args))
{
  return maplist (args[0], nargs - 1, args + 1, 0, 0);
}

DEFUN ("mapl", Fmapl, 2, MANY, 0, /*
Like `maplist', but do not accumulate values returned by the function.

arguments: (FUNCTION LIST &rest LISTS)
*/
       (int nargs, Lisp_Object *args))
{
  return maplist (args[0], nargs - 1, args + 1, 1, 0);
}

DEFUN ("mapcon", Fmapcon, 2, MANY, 0, /*
Like `maplist', but chains together the values returned by FUNCTION.

FUNCTION must return a list (unless it happens to be the last
iteration); the results will be concatenated together using `nconc'.

arguments: (FUNCTION LIST &rest LISTS)
*/
       (int nargs, Lisp_Object *args))
{
  return maplist (args[0], nargs - 1, args + 1, 0, 1);
}

/* Extra random functions */

DEFUN ("replace-list", Freplace_list, 2, 2, 0, /*
Destructively replace the list OLD with NEW.
This is like (copy-sequence NEW) except that it reuses the
conses in OLD as much as possible.  If OLD and NEW are the same
length, no consing will take place.
*/
       (old, new_))
{
  Lisp_Object oldtail = old, prevoldtail = Qnil;

  EXTERNAL_LIST_LOOP_2 (elt, new_)
    {
      if (!NILP (oldtail))
	{
	  CHECK_CONS (oldtail);
	  XCAR (oldtail) = elt;
	}
      else if (!NILP (prevoldtail))
	{
	  XCDR (prevoldtail) = Fcons (elt, Qnil);
	  prevoldtail = XCDR (prevoldtail);
	}
      else
	old = oldtail = Fcons (elt, Qnil);

      if (!NILP (oldtail))
	{
	  prevoldtail = oldtail;
	  oldtail = XCDR (oldtail);
	}
    }

  if (!NILP (prevoldtail))
    XCDR (prevoldtail) = Qnil;
  else
    old = Qnil;

  return old;
}


Lisp_Object
add_suffix_to_symbol (Lisp_Object symbol, const Ascbyte *ascii_string)
{
  return Fintern (concat2 (Fsymbol_name (symbol),
			   build_ascstring (ascii_string)),
		  Qnil);
}

Lisp_Object
add_prefix_to_symbol (const Ascbyte *ascii_string, Lisp_Object symbol)
{
  return Fintern (concat2 (build_ascstring (ascii_string),
			   Fsymbol_name (symbol)),
		  Qnil);
}


/* #### this function doesn't belong in this file! */

#ifdef HAVE_GETLOADAVG
#ifdef HAVE_SYS_LOADAVG_H
#include <sys/loadavg.h>
#endif
#else
int getloadavg (double loadavg[], int nelem); /* Defined in getloadavg.c */
#endif

DEFUN ("load-average", Fload_average, 0, 1, 0, /*
Return list of 1 minute, 5 minute and 15 minute load averages.
Each of the three load averages is multiplied by 100,
then converted to integer.

When USE-FLOATS is non-nil, floats will be used instead of integers.
These floats are not multiplied by 100.

If the 5-minute or 15-minute load averages are not available, return a
shortened list, containing only those averages which are available.

On some systems, this won't work due to permissions on /dev/kmem,
in which case you can't use this.
*/
       (use_floats))
{
  double load_ave[3];
  int loads = getloadavg (load_ave, countof (load_ave));
  Lisp_Object ret = Qnil;

  if (loads == -2)
    signal_error (Qunimplemented,
		  "load-average not implemented for this operating system",
		  Qunbound);
  else if (loads < 0)
    invalid_operation ("Could not get load-average", lisp_strerror (errno));

  while (loads-- > 0)
    {
      Lisp_Object load = (NILP (use_floats) ?
			  make_int ((int) (100.0 * load_ave[loads]))
			  : make_float (load_ave[loads]));
      ret = Fcons (load, ret);
    }
  return ret;
}


Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, 1, 1, 0, /*
Return non-nil if feature FEXP is present in this Emacs.
Use this to conditionalize execution of lisp code based on the
 presence or absence of emacs or environment extensions.
FEXP can be a symbol, a number, or a list.
If it is a symbol, that symbol is looked up in the `features' variable,
 and non-nil will be returned if found.
If it is a number, the function will return non-nil if this Emacs
 has an equal or greater version number than FEXP.
If it is a list whose car is the symbol `and', it will return
 non-nil if all the features in its cdr are non-nil.
If it is a list whose car is the symbol `or', it will return non-nil
 if any of the features in its cdr are non-nil.
If it is a list whose car is the symbol `not', it will return
 non-nil if the feature is not present.

Examples:

  (featurep 'xemacs)
    => ; Non-nil on XEmacs.

  (featurep '(and xemacs gnus))
    => ; Non-nil on XEmacs with Gnus loaded.

  (featurep '(or tty-frames (and emacs 19.30)))
    => ; Non-nil if this Emacs supports TTY frames.

  (featurep '(or (and xemacs 19.15) (and emacs 19.34)))
    => ; Non-nil on XEmacs 19.15 and later, or FSF Emacs 19.34 and later.

  (featurep '(and xemacs 21.02))
    => ; Non-nil on XEmacs 21.2 and later.

NOTE: The advanced arguments of this function (anything other than a
symbol) are not yet supported by FSF Emacs.  If you feel they are useful
for supporting multiple Emacs variants, lobby Richard Stallman at
<bug-gnu-emacs@gnu.org>.
*/
       (fexp))
{
#ifndef FEATUREP_SYNTAX
  CHECK_SYMBOL (fexp);
  return NILP (Fmemq (fexp, Vfeatures)) ? Qnil : Qt;
#else  /* FEATUREP_SYNTAX */
  static double featurep_emacs_version;

  /* Brute force translation from Erik Naggum's lisp function. */
  if (SYMBOLP (fexp))
    {
      /* Original definition */
      return NILP (Fmemq (fexp, Vfeatures)) ? Qnil : Qt;
    }
  else if (INTP (fexp) || FLOATP (fexp))
    {
      double d = extract_float (fexp);

      if (featurep_emacs_version == 0.0)
	{
	  featurep_emacs_version = XINT (Vemacs_major_version) +
	    (XINT (Vemacs_minor_version) / 100.0);
	}
      return featurep_emacs_version >= d ? Qt : Qnil;
    }
  else if (CONSP (fexp))
    {
      Lisp_Object tem = XCAR (fexp);
      if (EQ (tem, Qnot))
	{
	  Lisp_Object negate;

	  tem = XCDR (fexp);
	  negate = Fcar (tem);
	  if (!NILP (tem))
	    return NILP (call1 (Qfeaturep, negate)) ? Qt : Qnil;
	  else
	    return Fsignal (Qinvalid_read_syntax, list1 (tem));
	}
      else if (EQ (tem, Qand))
	{
	  tem = XCDR (fexp);
	  /* Use Fcar/Fcdr for error-checking. */
	  while (!NILP (tem) && !NILP (call1 (Qfeaturep, Fcar (tem))))
	    {
	      tem = Fcdr (tem);
	    }
	  return NILP (tem) ? Qt : Qnil;
	}
      else if (EQ (tem, Qor))
	{
	  tem = XCDR (fexp);
	  /* Use Fcar/Fcdr for error-checking. */
	  while (!NILP (tem) && NILP (call1 (Qfeaturep, Fcar (tem))))
	    {
	      tem = Fcdr (tem);
	    }
	  return NILP (tem) ? Qnil : Qt;
	}
      else
	{
	  return Fsignal (Qinvalid_read_syntax, list1 (XCDR (fexp)));
	}
    }
  else
    {
      return Fsignal (Qinvalid_read_syntax, list1 (fexp));
    }
}
#endif /* FEATUREP_SYNTAX */

DEFUN ("provide", Fprovide, 1, 1, 0, /*
Announce that FEATURE is a feature of the current Emacs.
This function updates the value of the variable `features'.
*/
       (feature))
{
  Lisp_Object tem;
  CHECK_SYMBOL (feature);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));
  return feature;
}

DEFUN ("require", Frequire, 1, 3, 0, /*
Ensure that FEATURE is present in the Lisp environment.
FEATURE is a symbol naming a collection of resources (functions, etc).
Optional FILENAME is a library from which to load resources; it defaults to
the print name of FEATURE.
Optional NOERROR, if non-nil, causes require to return nil rather than signal
`file-error' if loading the library fails.

If feature FEATURE is present in `features', update `load-history' to reflect
the require and return FEATURE.  Otherwise, try to load it from a library.
The normal messages at start and end of loading are suppressed.
If the library is successfully loaded and it calls `(provide FEATURE)', add
FEATURE to `features', update `load-history' and return FEATURE.
If the load succeeds but FEATURE is not provided by the library, signal
`invalid-state'.

The byte-compiler treats top-level calls to `require' specially, by evaluating
them at compile time (and then compiling them normally).  Thus a library may
request that definitions that should be inlined such as macros and defsubsts
be loaded into its compilation environment.  Achieving this in other contexts
requires an explicit \(eval-and-compile ...\) block.
*/
       (feature, filename, noerror))
{
  Lisp_Object tem;
  CHECK_SYMBOL (feature);
  tem = Fmemq (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qrequire, feature));
  if (!NILP (tem))
    return feature;
  else
    {
      int speccount = specpdl_depth ();

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      tem = call4 (Qload, NILP (filename) ? Fsymbol_name (feature) : filename,
		   noerror, Qrequire, Qnil);
      /* If load failed entirely, return nil.  */
      if (NILP (tem))
	return unbind_to_1 (speccount, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	invalid_state ("Required feature was not provided", feature);

      /* Once loading finishes, don't undo it.  */
      Vautoload_queue = Qt;
      return unbind_to_1 (speccount, feature);
    }
}

/* base64 encode/decode functions.

   Originally based on code from GNU recode.  Ported to FSF Emacs by
   Lars Magne Ingebrigtsen and Karl Heuer.  Ported to XEmacs and
   subsequently heavily hacked by Hrvoje Niksic.  */

#define MIME_LINE_LENGTH 72

#define IS_ASCII(Character) \
  ((Character) < 128)
#define IS_BASE64(Character) \
  (IS_ASCII (Character) && base64_char_to_value[Character] >= 0)

/* Table of characters coding the 64 values.  */
static Ascbyte base64_value_to_char[64] =
{
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',	/*  0- 9 */
  'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',	/* 10-19 */
  'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',	/* 20-29 */
  'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',	/* 30-39 */
  'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',	/* 40-49 */
  'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7',	/* 50-59 */
  '8', '9', '+', '/'					/* 60-63 */
};

/* Table of base64 values for first 128 characters.  */
static short base64_char_to_value[128] =
{
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*   0-  9 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  10- 19 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  20- 29 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,	/*  30- 39 */
  -1,  -1,  -1,  62,  -1,  -1,  -1,  63,  52,  53,	/*  40- 49 */
  54,  55,  56,  57,  58,  59,  60,  61,  -1,  -1,	/*  50- 59 */
  -1,  -1,  -1,  -1,  -1,  0,   1,   2,   3,   4,	/*  60- 69 */
  5,   6,   7,   8,   9,   10,  11,  12,  13,  14,	/*  70- 79 */
  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,	/*  80- 89 */
  25,  -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,	/*  90- 99 */
  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,	/* 100-109 */
  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,	/* 110-119 */
  49,  50,  51,  -1,  -1,  -1,  -1,  -1			/* 120-127 */
};

/* The following diagram shows the logical steps by which three octets
   get transformed into four base64 characters.

		 .--------.  .--------.  .--------.
		 |aaaaaabb|  |bbbbcccc|  |ccdddddd|
		 `--------'  `--------'  `--------'
                    6   2      4   4       2   6
	       .--------+--------+--------+--------.
	       |00aaaaaa|00bbbbbb|00cccccc|00dddddd|
	       `--------+--------+--------+--------'

	       .--------+--------+--------+--------.
	       |AAAAAAAA|BBBBBBBB|CCCCCCCC|DDDDDDDD|
	       `--------+--------+--------+--------'

   The octets are divided into 6 bit chunks, which are then encoded into
   base64 characters.  */

static DECLARE_DOESNT_RETURN (base64_conversion_error (const Ascbyte *,
						       Lisp_Object));

static DOESNT_RETURN
base64_conversion_error (const Ascbyte *reason, Lisp_Object frob)
{
  signal_error (Qbase64_conversion_error, reason, frob);
}

#define ADVANCE_INPUT(c, stream)					\
 ((ec = Lstream_get_ichar (stream)) == -1 ? 0 :			\
  ((ec > 255) ?								\
   (base64_conversion_error ("Non-ascii character in base64 input",	\
    make_char (ec)), 0)							\
   : (c = (Ibyte)ec), 1))

static Bytebpos
base64_encode_1 (Lstream *istream, Ibyte *to, int line_break)
{
  EMACS_INT counter = 0;
  Ibyte *e = to;
  Ichar ec;
  unsigned int value;

  while (1)
    {
      Ibyte c = 0;
      if (!ADVANCE_INPUT (c, istream))
	break;

      /* Wrap line every 76 characters.  */
      if (line_break)
	{
	  if (counter < MIME_LINE_LENGTH / 4)
	    counter++;
	  else
	    {
	      *e++ = '\n';
	      counter = 1;
	    }
	}

      /* Process first byte of a triplet.  */
      *e++ = base64_value_to_char[0x3f & c >> 2];
      value = (0x03 & c) << 4;

      /* Process second byte of a triplet.  */
      if (!ADVANCE_INPUT (c, istream))
	{
	  *e++ = base64_value_to_char[value];
	  *e++ = '=';
	  *e++ = '=';
	  break;
	}

      *e++ = base64_value_to_char[value | (0x0f & c >> 4)];
      value = (0x0f & c) << 2;

      /* Process third byte of a triplet.  */
      if (!ADVANCE_INPUT (c, istream))
	{
	  *e++ = base64_value_to_char[value];
	  *e++ = '=';
	  break;
	}

      *e++ = base64_value_to_char[value | (0x03 & c >> 6)];
      *e++ = base64_value_to_char[0x3f & c];
    }

  return e - to;
}
#undef ADVANCE_INPUT

/* Get next character from the stream, except that non-base64
   characters are ignored.  This is in accordance with rfc2045.  EC
   should be an Ichar, so that it can hold -1 as the value for EOF.  */
#define ADVANCE_INPUT_IGNORE_NONBASE64(ec, stream, streampos) do {	\
  ec = Lstream_get_ichar (stream);					\
  ++streampos;								\
  /* IS_BASE64 may not be called with negative arguments so check for	\
     EOF first. */							\
  if (ec < 0 || IS_BASE64 (ec) || ec == '=')				\
    break;								\
} while (1)

#define STORE_BYTE(pos, val, ccnt) do {					\
  pos += set_itext_ichar (pos, (Ichar)((Binbyte)(val)));		\
  ++ccnt;								\
} while (0)

static Bytebpos
base64_decode_1 (Lstream *istream, Ibyte *to, Charcount *ccptr)
{
  Charcount ccnt = 0;
  Ibyte *e = to;
  EMACS_INT streampos = 0;

  while (1)
    {
      Ichar ec;
      unsigned long value;

      /* Process first byte of a quadruplet.  */
      ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
      if (ec < 0)
	break;
      if (ec == '=')
	base64_conversion_error ("Illegal `=' character while decoding base64",
		      make_int (streampos));
      value = base64_char_to_value[ec] << 18;

      /* Process second byte of a quadruplet.  */
      ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
      if (ec < 0)
	base64_conversion_error ("Premature EOF while decoding base64",
				 Qunbound);
      if (ec == '=')
	base64_conversion_error ("Illegal `=' character while decoding base64",
		      make_int (streampos));
      value |= base64_char_to_value[ec] << 12;
      STORE_BYTE (e, value >> 16, ccnt);

      /* Process third byte of a quadruplet.  */
      ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
      if (ec < 0)
	base64_conversion_error ("Premature EOF while decoding base64",
				 Qunbound);

      if (ec == '=')
	{
	  ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
	  if (ec < 0)
	    base64_conversion_error ("Premature EOF while decoding base64",
				     Qunbound);
	  if (ec != '=')
	    base64_conversion_error
	      ("Padding `=' expected but not found while decoding base64",
	       make_int (streampos));
	  continue;
	}

      value |= base64_char_to_value[ec] << 6;
      STORE_BYTE (e, 0xff & value >> 8, ccnt);

      /* Process fourth byte of a quadruplet.  */
      ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
      if (ec < 0)
	base64_conversion_error ("Premature EOF while decoding base64",
				 Qunbound);
      if (ec == '=')
	continue;

      value |= base64_char_to_value[ec];
      STORE_BYTE (e, 0xff & value, ccnt);
    }

  *ccptr = ccnt;
  return e - to;
}
#undef ADVANCE_INPUT
#undef ADVANCE_INPUT_IGNORE_NONBASE64
#undef STORE_BYTE

DEFUN ("base64-encode-region", Fbase64_encode_region, 2, 3, "r", /*
Base64-encode the region between START and END.
Return the length of the encoded text.
Optional third argument NO-LINE-BREAK means do not break long lines
into shorter lines.
*/
       (start, end, no_line_break))
{
  Ibyte *encoded;
  Bytebpos encoded_length;
  Charcount allength, length;
  struct buffer *buf = current_buffer;
  Charbpos begv, zv, old_pt = BUF_PT (buf);
  Lisp_Object input;
  int speccount = specpdl_depth ();

  get_buffer_range_char (buf, start, end, &begv, &zv, 0);
  barf_if_buffer_read_only (buf, begv, zv);

  /* We need to allocate enough room for encoding the text.
     We need 33 1/3% more space, plus a newline every 76
     characters, and then we round up. */
  length = zv - begv;
  allength = length + length/3 + 1;
  allength += allength / MIME_LINE_LENGTH + 1 + 6;

  input = make_lisp_buffer_input_stream (buf, begv, zv, 0);
  /* We needn't multiply allength with MAX_ICHAR_LEN because all the
     base64 characters will be single-byte.  */
  encoded = (Ibyte *) MALLOC_OR_ALLOCA (allength);
  encoded_length = base64_encode_1 (XLSTREAM (input), encoded,
				    NILP (no_line_break));
  if (encoded_length > allength)
    ABORT ();
  Lstream_delete (XLSTREAM (input));

  /* Now we have encoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  buffer_insert_raw_string_1 (buf, begv, encoded, encoded_length, 0);
  unbind_to (speccount);
  buffer_delete_range (buf, begv + encoded_length, zv + encoded_length, 0);

  /* Simulate FSF Emacs implementation of this function: if point was
     in the region, place it at the beginning.  */
  if (old_pt >= begv && old_pt < zv)
    BUF_SET_PT (buf, begv);

  /* We return the length of the encoded text. */
  return make_int (encoded_length);
}

DEFUN ("base64-encode-string", Fbase64_encode_string, 1, 2, 0, /*
Base64 encode STRING and return the result.
Optional argument NO-LINE-BREAK means do not break long lines
into shorter lines.
*/
       (string, no_line_break))
{
  Charcount allength, length;
  Bytebpos encoded_length;
  Ibyte *encoded;
  Lisp_Object input, result;
  int speccount = specpdl_depth();

  CHECK_STRING (string);

  length = string_char_length (string);
  allength = length + length/3 + 1;
  allength += allength / MIME_LINE_LENGTH + 1 + 6;

  input = make_lisp_string_input_stream (string, 0, -1);
  encoded = (Ibyte *) MALLOC_OR_ALLOCA (allength);
  encoded_length = base64_encode_1 (XLSTREAM (input), encoded,
				    NILP (no_line_break));
  if (encoded_length > allength)
    ABORT ();
  Lstream_delete (XLSTREAM (input));
  result = make_string (encoded, encoded_length);
  unbind_to (speccount);
  return result;
}

DEFUN ("base64-decode-region", Fbase64_decode_region, 2, 2, "r", /*
Base64-decode the region between START and END.
Return the length of the decoded text.
If the region can't be decoded, return nil and don't modify the buffer.
Characters out of the base64 alphabet are ignored.
*/
       (start, end))
{
  struct buffer *buf = current_buffer;
  Charbpos begv, zv, old_pt = BUF_PT (buf);
  Ibyte *decoded;
  Bytebpos decoded_length;
  Charcount length, cc_decoded_length;
  Lisp_Object input;
  int speccount = specpdl_depth();

  get_buffer_range_char (buf, start, end, &begv, &zv, 0);
  barf_if_buffer_read_only (buf, begv, zv);

  length = zv - begv;

  input = make_lisp_buffer_input_stream (buf, begv, zv, 0);
  /* We need to allocate enough room for decoding the text. */
  decoded = (Ibyte *) MALLOC_OR_ALLOCA (length * MAX_ICHAR_LEN);
  decoded_length = base64_decode_1 (XLSTREAM (input), decoded, &cc_decoded_length);
  if (decoded_length > length * MAX_ICHAR_LEN)
    ABORT ();
  Lstream_delete (XLSTREAM (input));

  /* Now we have decoded the region, so we insert the new contents
     and delete the old.  (Insert first in order to preserve markers.)  */
  BUF_SET_PT (buf, begv);
  buffer_insert_raw_string_1 (buf, begv, decoded, decoded_length, 0);
  unbind_to (speccount);
  buffer_delete_range (buf, begv + cc_decoded_length,
		       zv + cc_decoded_length, 0);

  /* Simulate FSF Emacs implementation of this function: if point was
     in the region, place it at the beginning.  */
  if (old_pt >= begv && old_pt < zv)
    BUF_SET_PT (buf, begv);

  return make_int (cc_decoded_length);
}

DEFUN ("base64-decode-string", Fbase64_decode_string, 1, 1, 0, /*
Base64-decode STRING and return the result.
Characters out of the base64 alphabet are ignored.
*/
       (string))
{
  Ibyte *decoded;
  Bytebpos decoded_length;
  Charcount length, cc_decoded_length;
  Lisp_Object input, result;
  int speccount = specpdl_depth();

  CHECK_STRING (string);

  length = string_char_length (string);
  /* We need to allocate enough room for decoding the text. */
  decoded = (Ibyte *) MALLOC_OR_ALLOCA (length * MAX_ICHAR_LEN);

  input = make_lisp_string_input_stream (string, 0, -1);
  decoded_length = base64_decode_1 (XLSTREAM (input), decoded,
				    &cc_decoded_length);
  if (decoded_length > length * MAX_ICHAR_LEN)
    ABORT ();
  Lstream_delete (XLSTREAM (input));

  result = make_string (decoded, decoded_length);
  unbind_to (speccount);
  return result;
}

Lisp_Object Qyes_or_no_p;

void
syms_of_fns (void)
{
  INIT_LRECORD_IMPLEMENTATION (bit_vector);

  DEFSYMBOL (Qstring_lessp);
  DEFSYMBOL (Qidentity);
  DEFSYMBOL (Qvector);
  DEFSYMBOL (Qarray);
  DEFSYMBOL (Qstring);
  DEFSYMBOL (Qlist);
  DEFSYMBOL (Qbit_vector);

  DEFSYMBOL (Qyes_or_no_p);

  DEFERROR_STANDARD (Qbase64_conversion_error, Qtext_conversion_error);

  DEFSUBR (Fidentity);
  DEFSUBR (Frandom);
  DEFSUBR (Flength);
  DEFSUBR (Fsafe_length);
  DEFSUBR (Fstring_equal);
  DEFSUBR (Fcompare_strings);
  DEFSUBR (Fstring_lessp);
  DEFSUBR (Fstring_modified_tick);
  DEFSUBR (Fappend);
  DEFSUBR (Fconcat);
  DEFSUBR (Fvconcat);
  DEFSUBR (Fbvconcat);
  DEFSUBR (Fcopy_list);
  DEFSUBR (Fcopy_sequence);
  DEFSUBR (Fcopy_alist);
  DEFSUBR (Fcopy_tree);
  DEFSUBR (Fsubstring);
  DEFSUBR (Fsubseq);
  DEFSUBR (Fnthcdr);
  DEFSUBR (Fnth);
  DEFSUBR (Felt);
  DEFSUBR (Flast);
  DEFSUBR (Fbutlast);
  DEFSUBR (Fnbutlast);
  DEFSUBR (Fmember);
  DEFSUBR (Fold_member);
  DEFSUBR (Fmemq);
  DEFSUBR (Fold_memq);
  DEFSUBR (Fassoc);
  DEFSUBR (Fold_assoc);
  DEFSUBR (Fassq);
  DEFSUBR (Fold_assq);
  DEFSUBR (Frassoc);
  DEFSUBR (Fold_rassoc);
  DEFSUBR (Frassq);
  DEFSUBR (Fold_rassq);
  DEFSUBR (Fdelete);
  DEFSUBR (Fold_delete);
  DEFSUBR (Fdelq);
  DEFSUBR (Fold_delq);
  DEFSUBR (Fremassoc);
  DEFSUBR (Fremassq);
  DEFSUBR (Fremrassoc);
  DEFSUBR (Fremrassq);
  DEFSUBR (Fnreverse);
  DEFSUBR (Freverse);
  DEFSUBR (Fsort);
  DEFSUBR (Fplists_eq);
  DEFSUBR (Fplists_equal);
  DEFSUBR (Flax_plists_eq);
  DEFSUBR (Flax_plists_equal);
  DEFSUBR (Fplist_get);
  DEFSUBR (Fplist_put);
  DEFSUBR (Fplist_remprop);
  DEFSUBR (Fplist_member);
  DEFSUBR (Fcheck_valid_plist);
  DEFSUBR (Fvalid_plist_p);
  DEFSUBR (Fcanonicalize_plist);
  DEFSUBR (Flax_plist_get);
  DEFSUBR (Flax_plist_put);
  DEFSUBR (Flax_plist_remprop);
  DEFSUBR (Flax_plist_member);
  DEFSUBR (Fcanonicalize_lax_plist);
  DEFSUBR (Fdestructive_alist_to_plist);
  DEFSUBR (Fget);
  DEFSUBR (Fput);
  DEFSUBR (Fremprop);
  DEFSUBR (Fobject_plist);
  DEFSUBR (Fequal);
  DEFSUBR (Fequalp);
  DEFSUBR (Fold_equal);
  DEFSUBR (Ffillarray);
  DEFSUBR (Fnconc);
  DEFSUBR (FmapcarX);
  DEFSUBR (Fmapvector);
  DEFSUBR (Fmapcan);
  DEFSUBR (Fmapc);
  DEFSUBR (Fmapconcat);
  DEFSUBR (Fmap);
  DEFSUBR (Fmap_into);
  DEFSUBR (Fsome);
  DEFSUBR (Fevery);
  Ffset (intern ("mapc-internal"), Fsymbol_function (intern ("mapc")));
  Ffset (intern ("mapcar"), Fsymbol_function (intern ("mapcar*")));
  DEFSUBR (Fmaplist);
  DEFSUBR (Fmapl);
  DEFSUBR (Fmapcon);

  DEFSUBR (Freplace_list);
  DEFSUBR (Fload_average);
  DEFSUBR (Ffeaturep);
  DEFSUBR (Frequire);
  DEFSUBR (Fprovide);
  DEFSUBR (Fbase64_encode_region);
  DEFSUBR (Fbase64_encode_string);
  DEFSUBR (Fbase64_decode_region);
  DEFSUBR (Fbase64_decode_string);

  DEFSUBR (Fsplit_string_by_char);
  DEFSUBR (Fsplit_path);	/* #### */
}

void
vars_of_fns (void)
{
  DEFVAR_LISP ("path-separator", &Vpath_separator /*
The directory separator in search paths, as a string.
*/ );
  {
    Ascbyte c = SEPCHAR;
    Vpath_separator = make_string ((Ibyte *) &c, 1);
  }
}

void
init_provide_once (void)
{
  DEFVAR_LISP ("features", &Vfeatures /*
A list of symbols which are the features of the executing emacs.
Used by `featurep' and `require', and altered by `provide'.
*/ );
  Vfeatures = Qnil;

  Fprovide (intern ("base64"));
}
