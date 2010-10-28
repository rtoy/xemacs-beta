/* Random utility Lisp functions.
   Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2010 Ben Wing.

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

Lisp_Object Qstring_lessp, Qsort, Qmerge, Qfill, Qreplace;
Lisp_Object Qidentity;
Lisp_Object Qvector, Qarray, Qbit_vector, QsortX, Q_from_end, Q_initial_value;
Lisp_Object Qmapconcat, QmapcarX, Qmapvector, Qmapcan, Qmapc, Qmap, Qmap_into;
Lisp_Object Qsome, Qevery, Qmaplist, Qmapl, Qmapcon, Qreduce;
Lisp_Object Q_start1, Q_start2, Q_end1, Q_end2;

Lisp_Object Qbase64_conversion_error;

Lisp_Object Vpath_separator;

static int internal_old_equal (Lisp_Object, Lisp_Object, int);
Lisp_Object safe_copy_tree (Lisp_Object arg, Lisp_Object vecp, int depth);

static DOESNT_RETURN
mapping_interaction_error (Lisp_Object func, Lisp_Object object)
{
  invalid_state_2 ("object modified while traversing it", func, object);
}

static void
check_sequence_range (Lisp_Object sequence, Lisp_Object start,
		      Lisp_Object end, Lisp_Object length)
{
  Elemcount starting = XINT (start), ending, len = XINT (length);

  ending = NILP (end) ? XINT (length) : XINT (end);

  if (!(0 <= starting && starting <= ending && ending <= len))
    {
      args_out_of_range_3 (sequence, start, make_int (ending));
    }
}

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

/* This needs to be algorithmically identical to internal_array_hash in
   elhash.c when equalp is one, so arrays and bit vectors with the same
   contents hash the same. It would be possible to enforce this by giving
   internal_ARRAYLIKE_hash its own file and including it twice, but right
   now that doesn't seem worth it. */
static Hashcode
internal_bit_vector_equalp_hash (Lisp_Bit_Vector *v)
{
  int ii, size = bit_vector_length (v);
  Hashcode hash = 0;

  if (size <= 5)
    {
      for (ii = 0; ii < size; ii++)
        {
          hash = HASH2
            (hash,
             FLOAT_HASHCODE_FROM_DOUBLE ((double) (bit_vector_bit (v, ii))));
        }
      return hash;
    }

  /* just pick five elements scattered throughout the array.
     A slightly better approach would be to offset by some
     noise factor from the points chosen below. */
  for (ii = 0; ii < 5; ii++)
    hash = HASH2 (hash,
                  FLOAT_HASHCODE_FROM_DOUBLE
                  ((double) (bit_vector_bit (v, ii * size / 5))));

  return hash;
}

static Hashcode
bit_vector_hash (Lisp_Object obj, int UNUSED (depth), Boolint equalp)
{
  Lisp_Bit_Vector *v = XBIT_VECTOR (obj);
  if (equalp)
    {
      return HASH2 (bit_vector_length (v),
                    internal_bit_vector_equalp_hash (v));
    }

  return HASH2 (bit_vector_length (v),
		memory_hash (v->bits,
			     BIT_VECTOR_LONG_STORAGE (bit_vector_length (v)) *
			     sizeof (long)));
}

static Bytecount
size_bit_vector (Lisp_Object obj)
{
  Lisp_Bit_Vector *v = XBIT_VECTOR (obj);
  return FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Bit_Vector, unsigned long, bits,
				       BIT_VECTOR_LONG_STORAGE (bit_vector_length (v)));
}

static const struct memory_description bit_vector_description[] = {
  { XD_END }
};


DEFINE_DUMPABLE_SIZABLE_LISP_OBJECT ("bit-vector", bit_vector,
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

/* This is almost the above, but is defined by Common Lisp. We need it in C
   for shortest_length_among_sequences(), below, for the various sequence
   functions that can usefully operate on circular lists. */

DEFUN ("list-length", Flist_length, 1, 1, 0, /*
Return the length of LIST.  Return nil if LIST is circular.
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

  return EQ (hare, tortoise) && len != 0 ? Qnil : make_int (len);
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
Comparison is simply done on a character-by-character basis using the
numeric value of a character. (Note that this may not produce
particularly meaningful results under Mule if characters from
different charsets are being compared.)

Symbols are also allowed; their print names are used instead.

Currently we don't do proper language-specific collation or handle
multiple character sets.  This may be changed when Unicode support
is implemented.
*/
       (string1, string2))
{
  Lisp_Object p1, p2;
  Charcount end, len2;
  int i;

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

  end  = string_char_length (p1);
  len2 = string_char_length (p2);
  if (end > len2)
    end = len2;

  {
    Ibyte *ptr1 = XSTRING_DATA (p1);
    Ibyte *ptr2 = XSTRING_DATA (p2);

    /* #### It is not really necessary to do this: We could compare
       byte-by-byte and still get a reasonable comparison, since this
       would compare characters with a charset in the same way.  With
       a little rearrangement of the leading bytes, we could make most
       inter-charset comparisons work out the same, too; even if some
       don't, this is not a big deal because inter-charset comparisons
       aren't really well-defined anyway. */
    for (i = 0; i < end; i++)
      {
	if (itext_ichar (ptr1) != itext_ichar (ptr2))
	  return itext_ichar (ptr1) < itext_ichar (ptr2) ? Qt : Qnil;
	INC_IBYTEPTR (ptr1);
	INC_IBYTEPTR (ptr2);
      }
  }
  /* Can't do i < len2 because then comparison between "foo" and "foo^@"
     won't work right in I18N2 case */
  return end < len2 ? Qt : Qnil;
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

DEFUN ("subseq", Fsubseq, 2, 3, 0, /*
Return the subsequence of SEQUENCE starting at START and ending before END.
END may be omitted; then the subsequence runs to the end of SEQUENCE.

If START or END is negative, it counts from the end, in contravention of
Common Lisp.
The returned subsequence is always of the same type as SEQUENCE.
If SEQUENCE is a string, relevant parts of the string-extent-data
are copied to the new string.

See also `substring-no-properties', which only operates on strings, and does
not copy extent data.
*/
       (sequence, start, end))
{
  Elemcount len, ss, ee = EMACS_INT_MAX, ii;
  Lisp_Object result = Qnil;

  CHECK_SEQUENCE (sequence);
  CHECK_INT (start);
  ss = XINT (start);

  if (!NILP (end))
    {
      CHECK_INT (end);
      ee = XINT (end);
    }

  if (STRINGP (sequence))
    {
      Bytecount bstart, blen;

      get_string_range_char (sequence, start, end, &ss, &ee,
                             GB_HISTORICAL_STRING_BEHAVIOR);
      bstart = string_index_char_to_byte (sequence, ss);
      blen = string_offset_char_to_byte_len (sequence, bstart, ee - ss);

      result = make_string (XSTRING_DATA (sequence) + bstart, blen);
      /* Copy any applicable extent information into the new string. */
      copy_string_extents (result, sequence, 0, bstart, blen);
    }
  else if (CONSP (sequence))
    {
      Lisp_Object result_tail, saved = sequence;

      if (ss < 0 || ee < 0)
        {
          len = XINT (Flength (sequence));
	  if (ss < 0)
	    {
	      ss = len + ss;
	      start = make_integer (ss);
	    }

	  if (ee < 0)
	    {
	      ee  = len + ee;
	      end = make_integer (ee);
	    }
	  else
	    {
	      ee = min (ee, len);
	    }
        }

      if (0 != ss)
        {
          sequence = Fnthcdr (make_int (ss), sequence);
        }

      if (ss < ee && !NILP (sequence))
        {
	  result = result_tail = Fcons (Fcar (sequence), Qnil);
	  sequence = Fcdr (sequence);
	  ii = ss + 1;

	  {
	    EXTERNAL_LIST_LOOP_2 (elt, sequence)
	      {
		if (!(ii < ee))
		  {
		    break;
		  }

		XSETCDR (result_tail, Fcons (elt, Qnil));
		result_tail = XCDR (result_tail);
		ii++;
	      }
	  }
        }

      if (NILP (result) || (ii < ee && !NILP (end)))
        {
          /* We were handed a cons, which definitely has elements. nil
             result means either ss >= ee or SEQUENCE was nil after the
             nthcdr; in both cases that means START and END were incorrectly
             specified for this sequence. ii < ee with a non-nil end means
             the user handed us a bogus end value. */
          check_sequence_range (saved, start, end, Flength (saved));
        }
    }
  else
    {
      len = XINT (Flength (sequence));
      if (ss < 0)
	{
	  ss = len + ss;
	  start = make_integer (ss);
	}

      if (ee < 0)
	{
	  ee = len + ee;
	  end = make_integer (ee);
	}
      else
	{
	  ee = min (len, ee);
	}

      check_sequence_range (sequence, start, end, make_int (len));

      if (VECTORP (sequence))
        {
          result = Fvector (ee - ss, XVECTOR_DATA (sequence) + ss);
        }
      else if (BIT_VECTORP (sequence))
        {
          result = make_bit_vector (ee - ss, Qzero);

          for (ii = ss; ii < ee; ii++)
            {
              set_bit_vector_bit (XBIT_VECTOR (result), ii - ss,
                                  bit_vector_bit (XBIT_VECTOR (sequence), ii));
            }
        }
      else if (NILP (sequence))
        {
          DO_NOTHING;
        }
      else
        {
          /* Won't happen, since CHECK_SEQUENCE didn't error. */
          ABORT ();
        }
    }

  return result;
}

DEFUN ("substring-no-properties", Fsubstring_no_properties, 1, 3, 0, /* 
Return a substring of STRING, without copying the extents.
END may be nil or omitted; then the substring runs to the end of STRING.
If START or END is negative, it counts from the end.

With one argument, copy STRING without its properties.
*/
       (string, start, end))
{
  Charcount ccstart, ccend;
  Bytecount bstart, blen;
  Lisp_Object val;

  get_string_range_char (string, start, end, &ccstart, &ccend,
                         GB_HISTORICAL_STRING_BEHAVIOR);
  bstart = string_index_char_to_byte (string, ccstart);
  blen = string_offset_char_to_byte_len (string, bstart, ccend - ccstart);
  val = make_string (XSTRING_DATA (string) + bstart, blen);

  return val;
}

/* Split STRING into a list of substrings.  The substrings are the
   parts of original STRING separated by SEPCHAR.

   If UNESCAPE is non-zero, ESCAPECHAR specifies a character that will quote
   SEPCHAR, and cause it not to split STRING. A double ESCAPECHAR is
   necessary for ESCAPECHAR to appear once in a substring. */

static Lisp_Object
split_string_by_ichar_1 (const Ibyte *string, Bytecount size,
                         Ichar sepchar, int unescape, Ichar escapechar)
{
  Lisp_Object result = Qnil;
  const Ibyte *end = string + size;

  if (unescape)
    {
      Ibyte unescape_buffer[64], *unescape_buffer_ptr = unescape_buffer,
        escaped[MAX_ICHAR_LEN], *unescape_cursor;
      Bytecount unescape_buffer_size = countof (unescape_buffer),
        escaped_len = set_itext_ichar (escaped, escapechar);
      Boolint deleting_escapes, previous_escaped;
      Ichar pchar;

      while (1)
        {
          const Ibyte *p = string, *cursor;
          deleting_escapes = 0;
          previous_escaped = 0;

          while (p < end)
            {
              pchar = itext_ichar (p);

              if (pchar == sepchar)
                {
                  if (!previous_escaped)
                    {
                      break;
                    }
                }
              else if (pchar == escapechar
                       /* Doubled escapes don't escape: */
                       && !previous_escaped)
                {
                  ++deleting_escapes;
                  previous_escaped = 1;
                }
              else
                {
                  previous_escaped = 0;
                }

              INC_IBYTEPTR (p);
            }

          if (deleting_escapes)
            {
              if (((p - string) - (escaped_len * deleting_escapes))
                  > unescape_buffer_size)
                {
                  unescape_buffer_size =
                    ((p - string) - (escaped_len * deleting_escapes)) * 1.5;
                  unescape_buffer_ptr = alloca_ibytes (unescape_buffer_size);
                }

              cursor = string;
              unescape_cursor = unescape_buffer_ptr;
              previous_escaped = 0;

              while (cursor < p)
                {
                  pchar = itext_ichar (cursor);

                  if (pchar != escapechar || previous_escaped)
                    {
                      memcpy (unescape_cursor, cursor,
                              itext_ichar_len (cursor));
                      INC_IBYTEPTR (unescape_cursor);
                    }

                  previous_escaped = !previous_escaped
                    && (pchar == escapechar);

                  INC_IBYTEPTR (cursor);
                }

              result = Fcons (make_string (unescape_buffer_ptr,
                                           unescape_cursor
                                           - unescape_buffer_ptr),
                              result);
            }
          else
            {
              result = Fcons (make_string (string, p - string), result);
            }
          if (p < end)
            {
              string = p;
              INC_IBYTEPTR (string);	/* skip sepchar */
            }
          else
            break;
        }
    }
  else
    {
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

  return split_string_by_ichar_1 (newpath, newlen, SEPCHAR, 0, 0);
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
  return split_string_by_ichar_1 (path, qxestrlen (path), SEPCHAR, 0, 0);
}

/* Ben thinks this function should not exist or be exported to Lisp.
   We use it to define split-path-string in subr.el (not!).  */

DEFUN ("split-string-by-char", Fsplit_string_by_char, 2, 3, 0, /*
Split STRING into a list of substrings originally separated by SEPCHAR.

With optional ESCAPE-CHAR, any instances of SEPCHAR preceded by that
character will not split the string, and a double instance of ESCAPE-CHAR
will be necessary for a single ESCAPE-CHAR to appear in the output string.
*/
       (string, sepchar, escape_char))
{
  Ichar escape_ichar = 0;

  CHECK_STRING (string);
  CHECK_CHAR (sepchar);
  if (!NILP (escape_char))
    {
      CHECK_CHAR (escape_char);
      escape_ichar = XCHAR (escape_char);
    }
  return split_string_by_ichar_1 (XSTRING_DATA (string),
                                  XSTRING_LENGTH (string),
                                  XCHAR (sepchar),
                                  !NILP (escape_char), escape_ichar);
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
	   itext_ichar (XSTRING_DATA (Vpath_separator)), 0, 0));
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
Otherwise, LIST may be dotted, but not circular.
*/
       (list, n))
{
  Elemcount int_n = 1;

  CHECK_LIST (list);

  if (!NILP (n))
    {
      CHECK_NATNUM (n);
      int_n = XINT (n);
    }

  if (CONSP (list))
    {
      Lisp_Object last_cons = list;

      EXTERNAL_LIST_LOOP_3 (elt, list, tail)
	{
	  if (int_n-- < 0)
	    {
	      last_cons = XCDR (last_cons);
	    }

	  if (!CONSP (XCDR (tail)))
	    {
	      break;
	    }
	}

      if (int_n >= 0)
	{
	  return Qnil;
	}

      XCDR (last_cons) = Qnil;
    }

  return list;
}

DEFUN ("butlast", Fbutlast, 1, 2, 0, /*
Return a copy of LIST with the last N (default 1) elements removed.

If LIST has N or fewer elements, nil is returned.
Otherwise, LIST may be dotted, but not circular, and `(butlast LIST 0)'
converts a dotted into a true list.
*/
       (list, n))
{
  Lisp_Object retval = Qnil, retval_tail = Qnil;
  Elemcount int_n = 1;

  CHECK_LIST (list);

  if (!NILP (n))
    {
      CHECK_NATNUM (n);
      int_n = XINT (n);
    }

  if (CONSP (list))
    {
      Lisp_Object tail = list;

      EXTERNAL_LIST_LOOP_3 (elt, list, list_tail)
	{
	  if (--int_n < 0)
	    {
	      if (NILP (retval_tail))
		{
		  retval = retval_tail = Fcons (XCAR (tail), Qnil);
		}
	      else
		{
		  XSETCDR (retval_tail, Fcons (XCAR (tail), Qnil));
		  retval_tail = XCDR (retval_tail);
		}

	      tail = XCDR (tail);
	    }

	  if (!CONSP (XCDR (list_tail)))
	    {
	      break;
	    }
	}
    }

  return retval;
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

static Lisp_Object
c_merge_predicate_key (Lisp_Object obj1, Lisp_Object obj2,
                       Lisp_Object pred, Lisp_Object key_func)
{
  struct gcpro gcpro1;
  Lisp_Object args[3];

  /* We could use call2() and call3() here, but we're called O(nlogn) times
     for a sequence of length n, it make some sense to inline them. */
  args[0] = key_func;
  args[1] = obj1;
  args[2] = Qnil;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);

  obj1 = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));

  args[1] = obj2;
  obj2 = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));

  args[0] = pred;
  args[1] = obj1;
  args[2] = obj2;

  RETURN_UNGCPRO (IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args)));
}

static Lisp_Object
c_merge_predicate_nokey (Lisp_Object obj1, Lisp_Object obj2,
                         Lisp_Object pred, Lisp_Object UNUSED (key_func))
{
  struct gcpro gcpro1;
  Lisp_Object args[3];

  /* This is (almost) the implementation of call2, it makes some sense to
     inline it here. */
  args[0] = pred;
  args[1] = obj1;
  args[2] = obj2;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);

  RETURN_UNGCPRO (IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args)));
}

Lisp_Object
list_merge (Lisp_Object org_l1, Lisp_Object org_l2,
            Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object,
                                        Lisp_Object, Lisp_Object),
            Lisp_Object predicate, Lisp_Object key_func)
{
  Lisp_Object value;
  Lisp_Object tail;
  Lisp_Object tem;
  Lisp_Object l1, l2;
  Lisp_Object tortoises[2];
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  int l1_count = 0, l2_count = 0;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;
  tortoises[0] = org_l1;
  tortoises[1] = org_l2; 

  if (NULL == c_predicate)
    {
      c_predicate = EQ (key_func, Qidentity) ?
        c_merge_predicate_nokey : c_merge_predicate_key;
    }

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */

  GCPRO5 (org_l1, org_l2, predicate, value, tortoises[0]);
  gcpro5.nvars = 2;

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

      if (NILP (c_predicate (Fcar (l2), Fcar (l1), predicate, key_func)))
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;

	  if (l1_count++ > CIRCULAR_LIST_SUSPICION_LENGTH)
	    {
	      if (l1_count & 1)
		{
		  if (!CONSP (tortoises[0]))
		    {
		      mapping_interaction_error (Qmerge, tortoises[0]);
		    }

		  tortoises[0] = XCDR (tortoises[0]);
		}

	      if (EQ (org_l1, tortoises[0]))
		{
		  signal_circular_list_error (org_l1);
		}
	    }
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;

	  if (l2_count++ > CIRCULAR_LIST_SUSPICION_LENGTH)
	    {
	      if (l2_count & 1)
		{
		  if (!CONSP (tortoises[1]))
		    {
		      mapping_interaction_error (Qmerge, tortoises[1]);
		    }

		  tortoises[1] = XCDR (tortoises[1]);
		}

	      if (EQ (org_l2, tortoises[1]))
		{
		  signal_circular_list_error (org_l2);
		}
	    }
	}

      if (NILP (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);

      tail = tem;
    }
}

static void
array_merge (Lisp_Object *dest, Elemcount dest_len,
             Lisp_Object *front, Elemcount front_len,
             Lisp_Object *back, Elemcount back_len,
             Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object,
                                         Lisp_Object, Lisp_Object),
             Lisp_Object predicate, Lisp_Object key_func)
{
  Elemcount ii, fronting, backing;
  Lisp_Object *front_staging = front;
  Lisp_Object *back_staging = back;
  struct gcpro gcpro1, gcpro2;

  assert (dest_len == (back_len + front_len));

  if (0 == dest_len)
    {
      return;
    }

  if (front >= dest && front < (dest + dest_len))
    {
      front_staging = alloca_array (Lisp_Object, front_len);

      for (ii = 0; ii < front_len; ++ii)
        {
          front_staging[ii] = front[ii];
        }
    }

  if (back >= dest && back < (dest + dest_len))
    {
      back_staging = alloca_array (Lisp_Object, back_len);

      for (ii = 0; ii < back_len; ++ii)
        {
          back_staging[ii] = back[ii];
        }
    }

  GCPRO2 (front_staging[0], back_staging[0]);
  gcpro1.nvars = front_len;
  gcpro2.nvars = back_len;

  for (ii = fronting = backing = 0; ii < dest_len; ++ii)
    {
      if (fronting >= front_len)
        {
          while (ii < dest_len)
            {
              dest[ii] = back_staging[backing];
              ++ii, ++backing;
            }
          UNGCPRO;
          return;
        }

      if (backing >= back_len)
        {
          while (ii < dest_len)
            {
              dest[ii] = front_staging[fronting];
              ++ii, ++fronting;
            }
          UNGCPRO;
          return;
        }

      if (NILP (c_predicate (back_staging[backing], front_staging[fronting],
                             predicate, key_func)))
        {
          dest[ii] = front_staging[fronting];
          ++fronting;
        }
      else
        {
          dest[ii] = back_staging[backing];
          ++backing;
        }
    }

  UNGCPRO;
}

static Lisp_Object
list_array_merge_into_list (Lisp_Object list,
                            Lisp_Object *array, Elemcount array_len,
                            Lisp_Object (*c_predicate) (Lisp_Object,
                                                        Lisp_Object,
                                                        Lisp_Object,
                                                        Lisp_Object),
                            Lisp_Object predicate, Lisp_Object key_func,
                            Boolint reverse_order)
{
  Lisp_Object tail = Qnil, value = Qnil, tortoise = list;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Elemcount array_index = 0;
  int looped = 0;

  GCPRO4 (list, tail, value, tortoise);

  while (1)
    {
      if (NILP (list))
        {
          UNGCPRO;

          if (NILP (tail))
            {
              return Flist (array_len, array);
            }

          Fsetcdr (tail, Flist (array_len - array_index, array + array_index));
          return value;
        }

      if (array_index >= array_len)
        {
          UNGCPRO;
          if (NILP (tail))
            {
              return list;
            }

          Fsetcdr (tail, list);
          return value;
        }


      if (reverse_order ?
          !NILP (c_predicate (Fcar (list), array [array_index], predicate,
                              key_func)) :
          NILP (c_predicate (array [array_index], Fcar (list), predicate,
                             key_func)))
        {
          if (NILP (tail))
            {
              value = tail = list;
            }
          else
            {
              Fsetcdr (tail, list);
              tail = XCDR (tail);
            }

          list = Fcdr (list);
        }
      else
        {
          if (NILP (tail))
            {
              value = tail = Fcons (array [array_index], Qnil);
            }
          else
            {
              Fsetcdr (tail, Fcons (array [array_index], tail));
              tail = XCDR (tail);
            }
          ++array_index;
        }

      if (++looped > CIRCULAR_LIST_SUSPICION_LENGTH)
        {
          if (looped & 1)
            {
              tortoise = XCDR (tortoise);
            }

          if (EQ (list, tortoise))
            {
              signal_circular_list_error (list);
            }
        }
    }
}

static void
list_list_merge_into_array (Lisp_Object *output, Elemcount output_len,
                            Lisp_Object list_one, Lisp_Object list_two,
                            Lisp_Object (*c_predicate) (Lisp_Object,
                                                        Lisp_Object,
                                                        Lisp_Object,
                                                        Lisp_Object),
                            Lisp_Object predicate, Lisp_Object key_func)
{
  Elemcount output_index = 0;

  while (output_index < output_len)
    {
      if (NILP (list_one))
        {
          while (output_index < output_len)
            {
              output [output_index] = Fcar (list_two);
              list_two = Fcdr (list_two), ++output_index;
            }
          return;
        }

      if (NILP (list_two))
        {
          while (output_index < output_len)
            {
              output [output_index] = Fcar (list_one);
              list_one = Fcdr (list_one), ++output_index;
            }
          return;
        }

      if (NILP (c_predicate (Fcar (list_two), Fcar (list_one), predicate,
                             key_func)))
        {
          output [output_index] = XCAR (list_one);
          list_one = XCDR (list_one);
        }
      else
        {
          output [output_index] = XCAR (list_two);
          list_two = XCDR (list_two);
        }

      ++output_index;

      /* No need to check for circularity. */
    }
}

static void
list_array_merge_into_array (Lisp_Object *output, Elemcount output_len,
                             Lisp_Object list,
                             Lisp_Object *array, Elemcount array_len,
                             Lisp_Object (*c_predicate) (Lisp_Object,
                                                         Lisp_Object,
                                                         Lisp_Object,
                                                         Lisp_Object),
                             Lisp_Object predicate, Lisp_Object key_func,
                             Boolint reverse_order)
{
  Elemcount output_index = 0, array_index = 0;

  while (output_index < output_len)
    {
      if (NILP (list))
        {
          if (array_len - array_index != output_len - output_index)
            {
	      mapping_interaction_error (Qmerge, list);
            }

          while (array_index < array_len)
            {
              output [output_index++] = array [array_index++];
            }

          return;
        }

      if (array_index >= array_len)
        {
          while (output_index < output_len)
            {
              output [output_index++] = Fcar (list);
              list = Fcdr (list);
            }

          return;
        }

      if (reverse_order ? 
          !NILP (c_predicate (Fcar (list), array [array_index], predicate,
                              key_func)) :
          NILP (c_predicate (array [array_index], Fcar (list), predicate,
                             key_func)))
        {
          output [output_index] = XCAR (list);
          list = XCDR (list);
        }
      else
        {
          output [output_index] = array [array_index];
          ++array_index;
        }

      ++output_index;
    }
}

#define STRING_DATA_TO_OBJECT_ARRAY(strdata, c_array, counter, len)     \
  do {                                                                  \
    c_array = alloca_array (Lisp_Object, len);                          \
    for (counter = 0; counter < len; ++counter)                         \
      {                                                                 \
        c_array[counter] = make_char (itext_ichar (strdata));           \
        INC_IBYTEPTR (strdata);                                         \
      }                                                                 \
  } while (0)

#define BIT_VECTOR_TO_OBJECT_ARRAY(v, c_array, counter, len) do {       \
    c_array = alloca_array (Lisp_Object, len);                          \
    for (counter = 0; counter < len; ++counter)                         \
      {                                                                 \
        c_array[counter] = make_int (bit_vector_bit (v, counter));      \
      }                                                                 \
  } while (0)

/* This macro might eventually find a better home than here. */

#define CHECK_KEY_ARGUMENT(key)                                         \
    do {								\
      if (NILP (key))							\
	{								\
	  key = Qidentity;						\
	}								\
                                                                        \
      if (!EQ (key, Qidentity))                                         \
        {                                                               \
          key = indirect_function (key, 1);                             \
        }                                                               \
    } while (0)

DEFUN ("merge", Fmerge, 4, MANY, 0, /*
Destructively merge SEQUENCE-ONE and SEQUENCE-TWO, producing a new sequence.

TYPE is the type of sequence to return.  PREDICATE is a `less-than'
predicate on the elements.

Optional keyword argument KEY is a function used to extract an object to be
used for comparison from each element of SEQUENCE-ONE and SEQUENCE-TWO.

arguments: (TYPE SEQUENCE-ONE SEQUENCE-TWO PREDICATE &key (KEY #'IDENTITY))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object type = args[0], sequence_one = args[1], sequence_two = args[2],
    predicate = args[3], result = Qnil;
  Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object);

  PARSE_KEYWORDS (Fmerge, nargs, args, 1, (key), NULL);

  CHECK_SEQUENCE (sequence_one);
  CHECK_SEQUENCE (sequence_two);

  CHECK_KEY_ARGUMENT (key);

  c_predicate = EQ (key, Qidentity) ?
    c_merge_predicate_nokey : c_merge_predicate_key;

  if (EQ (type, Qlist) && (LISTP (sequence_one) || LISTP (sequence_two)))
    {
      if (NILP (sequence_two))
        {
          result = Fappend (2, args + 1);
        }
      else if (NILP (sequence_one))
        {
          args[3] = Qnil; /* Overwriting PREDICATE, and losing its GC
                             protection, but that doesn't matter. */
          result = Fappend (2, args + 2);
        }
      else if (CONSP (sequence_one) && CONSP (sequence_two))
	{
	  result = list_merge (sequence_one, sequence_two, c_predicate,
                               predicate, key);
	}
      else
        {
          Lisp_Object *array_storage, swap;
          Elemcount array_length, i;
          Boolint reverse_order = 0;

          if (!CONSP (sequence_one))
            {
              /* Make sequence_one the cons, sequence_two the array: */
              swap = sequence_one;
              sequence_one = sequence_two;
              sequence_two = swap;
              reverse_order = 1;
            }

          if (VECTORP (sequence_two))
            {
              array_storage = XVECTOR_DATA (sequence_two);
              array_length = XVECTOR_LENGTH (sequence_two);
            }
          else if (STRINGP (sequence_two))
            {
              Ibyte *strdata = XSTRING_DATA (sequence_two);
              array_length = string_char_length (sequence_two);
              /* No need to GCPRO, characters are immediate. */
              STRING_DATA_TO_OBJECT_ARRAY (strdata, array_storage, i,
                                           array_length);

            }
          else
            {
              Lisp_Bit_Vector *v = XBIT_VECTOR (sequence_two);
              array_length = bit_vector_length (v);
              /* No need to GCPRO, fixnums are immediate. */
              BIT_VECTOR_TO_OBJECT_ARRAY (v, array_storage, i, array_length);
            }

          result = list_array_merge_into_list (sequence_one,
                                               array_storage, array_length,
                                               c_predicate,
                                               predicate, key,
                                               reverse_order);
        }
    }
  else
    {
      Elemcount sequence_one_len = XINT (Flength (sequence_one)),
        sequence_two_len = XINT (Flength (sequence_two)), i;
      Elemcount output_len = 1 + sequence_one_len + sequence_two_len;
      Lisp_Object *output = alloca_array (Lisp_Object, output_len),
        *sequence_one_storage = NULL, *sequence_two_storage = NULL;
      Boolint do_coerce = !(EQ (type, Qvector) || EQ (type, Qstring)
                            || EQ (type, Qbit_vector) || EQ (type, Qlist));
      Ibyte *strdata = NULL;
      Lisp_Bit_Vector *v = NULL;
      struct gcpro gcpro1;

      output[0] = do_coerce ? Qlist : type;
      for (i = 1; i < output_len; ++i)
	{
	  output[i] = Qnil;
	}

      GCPRO1 (output[0]);
      gcpro1.nvars = output_len;

      if (VECTORP (sequence_one))
        {
          sequence_one_storage = XVECTOR_DATA (sequence_one);
        }
      else if (STRINGP (sequence_one))
        {
          strdata = XSTRING_DATA (sequence_one);
          STRING_DATA_TO_OBJECT_ARRAY (strdata, sequence_one_storage,
                                       i, sequence_one_len);
        }
      else if (BIT_VECTORP (sequence_one))
        {
          v = XBIT_VECTOR (sequence_one);
          BIT_VECTOR_TO_OBJECT_ARRAY (v, sequence_one_storage,
                                      i, sequence_one_len);
        }

      if (VECTORP (sequence_two))
        {
          sequence_two_storage = XVECTOR_DATA (sequence_two);
        }
      else if (STRINGP (sequence_two))
        {
          strdata = XSTRING_DATA (sequence_two);
          STRING_DATA_TO_OBJECT_ARRAY (strdata, sequence_two_storage,
                                       i, sequence_two_len);
        }
      else if (BIT_VECTORP (sequence_two))
        {
          v = XBIT_VECTOR (sequence_two);
          BIT_VECTOR_TO_OBJECT_ARRAY (v, sequence_two_storage,
                                      i, sequence_two_len);
        }

      if (LISTP (sequence_one) && LISTP (sequence_two))
        {
          list_list_merge_into_array (output + 1, output_len - 1,
                                      sequence_one, sequence_two,
                                      c_predicate, predicate,
                                      key);
        }
      else if (LISTP (sequence_one))
        {
          list_array_merge_into_array (output + 1, output_len - 1,
                                       sequence_one,
                                       sequence_two_storage,
                                       sequence_two_len,
                                       c_predicate, predicate,
                                       key, 0);
        }
      else if (LISTP (sequence_two))
        {
          list_array_merge_into_array (output + 1, output_len - 1,
                                       sequence_two,
                                       sequence_one_storage,
                                       sequence_one_len,
                                       c_predicate, predicate,
                                       key, 1);
        }
      else
        {
          array_merge (output + 1, output_len - 1,
                       sequence_one_storage, sequence_one_len,
                       sequence_two_storage, sequence_two_len,
                       c_predicate, predicate,
                       key);
        }

      result = Ffuncall (output_len, output);

      if (do_coerce)
	{
	  result = call2 (Qcoerce, result, type);
	}

      UNGCPRO;
    }

  return result;
}

/* The sort function should return non-nil if OBJ1 < OBJ2, nil otherwise.
   NOTE: This is backwards from the way qsort() works. */
Lisp_Object
list_sort (Lisp_Object list,
           Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object, 
                                       Lisp_Object, Lisp_Object),
           Lisp_Object predicate, Lisp_Object key_func)
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object back, tem;
  Lisp_Object front = list;
  Lisp_Object len = Flength (list);

  if (XINT (len) < 2)
    return list;

  if (NULL == c_predicate)
    {
      c_predicate = EQ (key_func, Qidentity) ? c_merge_predicate_nokey :
        c_merge_predicate_key;
    }

  len = make_int (XINT (len) / 2 - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO4 (front, back, predicate, key_func);
  front = list_sort (front, c_predicate, predicate, key_func);
  back = list_sort (back, c_predicate, predicate, key_func);

  RETURN_UNGCPRO (list_merge (front, back, c_predicate, predicate, key_func));
}

static void
array_sort (Lisp_Object *array, Elemcount array_len,
            Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object, 
                                        Lisp_Object, Lisp_Object),
            Lisp_Object predicate, Lisp_Object key_func)
{
  Elemcount split;

  if (array_len < 2)
    return;

  split = array_len / 2;

  array_sort (array, split, c_predicate, predicate, key_func);
  array_sort (array + split, array_len - split, c_predicate, predicate,
	      key_func);
  array_merge (array, array_len, array, split, array + split,
	       array_len - split, c_predicate, predicate, key_func);
}            

DEFUN ("sort*", FsortX, 2, MANY, 0, /*
Sort SEQUENCE, comparing elements using PREDICATE.
Returns the sorted sequence.  SEQUENCE is modified by side effect.

PREDICATE is called with two elements of SEQUENCE, and should return t if
the first element is `less' than the second.

Optional keyword argument KEY is a function used to extract an object to be
used for comparison from each element of SEQUENCE.

In this implementation, sorting is always stable; but call `stable-sort' if
this stability is important to you, other implementations may not make the
same guarantees.

arguments: (SEQUENCE PREDICATE &key (KEY #'IDENTITY))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence = args[0], predicate = args[1];
  Lisp_Object *sequence_carray;
  Lisp_Object (*c_predicate) (Lisp_Object, Lisp_Object, Lisp_Object,
                              Lisp_Object);
  Elemcount sequence_len, i;

  PARSE_KEYWORDS (FsortX, nargs, args, 1, (key), NULL);

  CHECK_SEQUENCE (sequence);

  CHECK_KEY_ARGUMENT (key);

  c_predicate = EQ (key, Qidentity) ?
    c_merge_predicate_nokey : c_merge_predicate_key;

  if (LISTP (sequence))
    {
      sequence = list_sort (sequence, c_predicate, predicate, key);
    }
  else if (VECTORP (sequence))
    {
      array_sort (XVECTOR_DATA (sequence), XVECTOR_LENGTH (sequence),
                  c_predicate, predicate, key);
    }
  else if (STRINGP (sequence))
    {
      Ibyte *strdata = XSTRING_DATA (sequence);

      sequence_len = string_char_length (sequence);

      STRING_DATA_TO_OBJECT_ARRAY (strdata, sequence_carray, i, sequence_len);

      /* No GCPRO necessary, characters are immediate. */
      array_sort (sequence_carray, sequence_len, c_predicate, predicate, key);

      strdata = XSTRING_DATA (sequence);

      CHECK_LISP_WRITEABLE (sequence);
      for (i = 0; i < sequence_len; ++i)
        {
          strdata += set_itext_ichar (strdata, XCHAR (sequence_carray[i]));
        }

      init_string_ascii_begin (sequence);
      bump_string_modiff (sequence);
      sledgehammer_check_ascii_begin (sequence);
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *v = XBIT_VECTOR (sequence);
      sequence_len = bit_vector_length (v);

      BIT_VECTOR_TO_OBJECT_ARRAY (v, sequence_carray, i, sequence_len);

      /* No GCPRO necessary, bits are immediate. */
      array_sort (sequence_carray, sequence_len, c_predicate, predicate, key);

      for (i = 0; i < sequence_len; ++i)
        {
          set_bit_vector_bit (v, i, XINT (sequence_carray [i]));
        }
    }

  return sequence;
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
face, glyph, or process.  See also `put', `remprop', `object-plist', and
`object-setplist'.
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
OBJECT can be a symbol, string, extent, face, glyph, or process.
Return non-nil if the property list was actually modified (i.e. if PROPERTY
was present in the property list).  See also `get', `put', `object-plist',
and `object-setplist'.
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

DEFUN ("object-setplist", Fobject_setplist, 2, 2, 0, /*
Set OBJECT's property list to NEWPLIST, and return NEWPLIST.
For a symbol, this is equivalent to `setplist'.

OBJECT can be a symbol or a process, other objects with visible plists do
not allow their modification with `object-setplist'.
*/
       (object, newplist))
{
  if (LRECORDP (object) && XRECORD_LHEADER_IMPLEMENTATION (object)->setplist)
    {
      return XRECORD_LHEADER_IMPLEMENTATION (object)->setplist (object,
								newplist);
    }

  invalid_operation ("Not possible to set object's plist", object);
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


static Lisp_Object replace_string_range_1 (Lisp_Object dest,
					   Lisp_Object start,
					   Lisp_Object end,
					   const Ibyte *source,
					   const Ibyte *source_limit,
					   Lisp_Object item);

/* Fill the substring of DEST beginning at START and ending before END with
   the character ITEM. If DEST does not have sufficient space for END -
   START characters at START, write as many as is possible without changing
   the character length of DEST.  Update the string modification flag and do
   any sledgehammer checks we have turned on.

   START must be a Lisp integer. END can be nil, indicating the length of the
   string, or a Lisp integer.  The condition (<= 0 START END (length DEST))
   must hold, or fill_string_range() will signal an error. */
static Lisp_Object
fill_string_range (Lisp_Object dest, Lisp_Object item, Lisp_Object start,
		   Lisp_Object end)
{
  return replace_string_range_1 (dest, start, end, NULL, NULL, item);
}

DEFUN ("fill", Ffill, 2, MANY, 0, /*
Destructively modify SEQUENCE by replacing each element with ITEM.
SEQUENCE is a list, vector, bit vector, or string.

Optional keyword START is the index of the first element of SEQUENCE
to be modified, and defaults to zero.  Optional keyword END is the
exclusive upper bound on the elements of SEQUENCE to be modified, and
defaults to the length of SEQUENCE.

arguments: (SEQUENCE ITEM &key (START 0) (END (length SEQUENCE)))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence = args[0];
  Lisp_Object item = args[1];
  Elemcount starting = 0, ending = EMACS_INT_MAX, ii, len;

  PARSE_KEYWORDS (Ffill, nargs, args, 2, (start, end), (start = Qzero));

  CHECK_NATNUM (start);
  starting = XINT (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = XINT (end);
    }

 retry:
  if (STRINGP (sequence))
    {
      CHECK_CHAR_COERCE_INT (item);
      CHECK_LISP_WRITEABLE (sequence);

      fill_string_range (sequence, item, start, end);
    }
  else if (VECTORP (sequence))
    {
      Lisp_Object *p = XVECTOR_DATA (sequence);

      CHECK_LISP_WRITEABLE (sequence);
      len = XVECTOR_LENGTH (sequence);

      check_sequence_range (sequence, start, end, make_int (len));
      ending = min (ending, len);

      for (ii = starting; ii < ending; ++ii)
        {
          p[ii] = item;
        }
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *v = XBIT_VECTOR (sequence);
      int bit;

      CHECK_BIT (item);
      bit = XINT (item);
      CHECK_LISP_WRITEABLE (sequence);
      len = bit_vector_length (v);

      check_sequence_range (sequence, start, end, make_int (len));
      ending = min (ending, len);

      for (ii = starting; ii < ending; ++ii)
        {
          set_bit_vector_bit (v, ii, bit);
        }
    }
  else if (LISTP (sequence))
    {
      Elemcount counting = 0;

      EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
        {
          if (counting >= starting)
            {
              if (counting < ending)
                {
                  XSETCAR (tail, item);
                }
              else if (counting == ending)
                {
                  break;
                }
            }
          ++counting;
        }

      if (counting < starting || (counting != ending && !NILP (end)))
	{
	  check_sequence_range (args[0], start, end, Flength (args[0]));
	}
    }
  else
    {
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
  return sequence;
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


/* Replace the substring of DEST beginning at START and ending before END
   with the text at SOURCE, which is END - START characters long and
   SOURCE_LIMIT - SOURCE octets long.  If DEST does not have sufficient
   space for END - START characters at START, write as many as is possible
   without changing the length of DEST.  Update the string modification flag
   and do any sledgehammer checks we have turned on in this build.

   START must be a Lisp integer. END can be nil, indicating the length of the
   string, or a Lisp integer.  The condition (<= 0 START END (length DEST))
   must hold, or replace_string_range() will signal an error. */
static Lisp_Object
replace_string_range (Lisp_Object dest, Lisp_Object start, Lisp_Object end,
                      const Ibyte *source, const Ibyte *source_limit)
{
  return replace_string_range_1 (dest, start, end, source, source_limit,
				 Qnil);
}

/* This is the guts of several mapping functions.

   Call FUNCTION CALL_COUNT times, with NSEQUENCES arguments each time,
   taking the elements from SEQUENCES.  If VALS is non-NULL, store the
   results into VALS, a C array of Lisp_Objects; else, if LISP_VALS is
   non-nil, store the results into LISP_VALS, a sequence with sufficient
   room for CALL_COUNT results (but see the documentation of SOME_OR_EVERY.) 
   Else, do not accumulate any result.

   If VALS is non-NULL, NSEQUENCES is one, and SEQUENCES[0] is a cons,
   mapcarX will store the elements of SEQUENCES[0] in stack and GCPRO them,
   so FUNCTION cannot insert a non-cons into SEQUENCES[0] and throw off
   mapcarX.

   Otherwise, mapcarX signals an invalid state error (see
   mapping_interaction_error(), above) if it encounters a non-cons,
   non-array when traversing SEQUENCES.  Common Lisp specifies in
   MAPPING-DESTRUCTIVE-INTERACTION that it is an error when FUNCTION
   destructively modifies SEQUENCES in a way that might affect the ongoing
   traversal operation.

   CALLER is a symbol describing the Lisp-visible function that was called,
   and any errors thrown because SEQUENCES was modified will reflect it.

   If CALLER is Qsome, return the (possibly multiple) values given by
   FUNCTION the first time it is non-nil, and abandon the iterations.
   LISP_VALS must be the result of calling STORE_VOID_IN_LISP on the address
   of a Lisp object, and the return value will be stored at that address.
   If CALLER is Qevery, LISP_VALS must also reflect a pointer to a Lisp
   object, and Qnil will be stored at that address if FUNCTION gives nil;
   otherwise it will be left alone. */

static void
mapcarX (Elemcount call_count, Lisp_Object *vals, Lisp_Object lisp_vals,
	 Lisp_Object function, int nsequences, Lisp_Object *sequences, 
	 Lisp_Object caller)
{
  Lisp_Object called, *args;
  struct gcpro gcpro1, gcpro2;
  Ibyte *lisp_vals_staging, *cursor;
  int i, j;

  assert ((EQ (caller, Qsome) || EQ (caller, Qevery)) ? vals == NULL : 1);

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
	  vals[i] = IGNORE_MULTIPLE_VALUES (Ffuncall (nsequences + 1, args));
	}
    }
  else
    {
      enum lrecord_type lisp_vals_type;
      Binbyte *sequence_types = alloca_array (Binbyte, nsequences);
      for (j = 0; j < nsequences; ++j)
	{
	  sequence_types[j] = XRECORD_LHEADER (sequences[j])->type;
	}

      if (!EQ (caller, Qsome) && !EQ (caller, Qevery))
        {
          assert (LRECORDP (lisp_vals));

          lisp_vals_type
            = (enum lrecord_type) XRECORD_LHEADER (lisp_vals)->type;

	  if (lrecord_type_string == lisp_vals_type)
	    {
	      lisp_vals_staging = cursor
		= alloca_ibytes (call_count * MAX_ICHAR_LEN);
	    }
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
			/* This means FUNCTION has messed around with a cons
			   in one of the sequences, since we checked the
			   type (CHECK_SEQUENCE()) and the length and
			   structure (with Flength()) correctly in our
			   callers. */
                        mapping_interaction_error (caller, sequences[j]);
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
          else if (EQ (Qsome, caller))
            {
              if (!NILP (IGNORE_MULTIPLE_VALUES (called)))
                {
                  Lisp_Object *result
                    = (Lisp_Object *) GET_VOID_FROM_LISP (lisp_vals);
                  *result = called;
                  UNGCPRO;
                  return;
                }
            }
          else if (EQ (Qevery, caller))
            {
	      if (NILP (IGNORE_MULTIPLE_VALUES (called)))
                {
                  Lisp_Object *result
                    = (Lisp_Object *) GET_VOID_FROM_LISP (lisp_vals);
                  *result = Qnil;
                  UNGCPRO;
                  return;
                }
            }
          else
            {
              called = IGNORE_MULTIPLE_VALUES (called);
              switch (lisp_vals_type)
                {
                case lrecord_type_symbol:
		  /* Discard the result of funcall. */
                  break;
                case lrecord_type_cons:
                  {
                    if (!CONSP (lisp_vals))
                      {
                        /* If FUNCTION has inserted a non-cons non-nil
                           cdr into the list before we've processed the
                           relevant part, error. */
                        mapping_interaction_error (caller, lisp_vals);
                      }
                    XSETCAR (lisp_vals, called);
                    lisp_vals = XCDR (lisp_vals);
                    break;
                  }
                case lrecord_type_vector:
                  {
                    i < XVECTOR_LENGTH (lisp_vals) ?
                      (XVECTOR_DATA (lisp_vals)[i] = called) :
                      /* Let #'aset error. */
                      Faset (lisp_vals, make_int (i), called);
                    break;
                  }
                case lrecord_type_string:
                  {
		    CHECK_CHAR_COERCE_INT (called);
		    cursor += set_itext_ichar (cursor, XCHAR (called));
                    break;
                  }
                case lrecord_type_bit_vector:
                  {
                    (BITP (called) &&
                     i < bit_vector_length (XBIT_VECTOR (lisp_vals))) ?
                      set_bit_vector_bit (XBIT_VECTOR (lisp_vals), i,
                                          XINT (called)) :
                      (void) Faset (lisp_vals, make_int (i), called);
                    break;
                  }
                default:
                  {
                    ABORT();
                    break;
                  }
                }
            }
	}

      if (!EQ (caller, Qsome) && !EQ (caller, Qevery) &&
	  lrecord_type_string == lisp_vals_type)
	{
	  replace_string_range (lisp_vals, Qzero, make_int (call_count),
				lisp_vals_staging, cursor);
	}
    }

  UNGCPRO;
}

/* Given NSEQUENCES objects at the address pointed to by SEQUENCES, return
   the length of the shortest sequence. Error if all are circular, or if any
   one of them is not a sequence. */
static Elemcount
shortest_length_among_sequences (int nsequences, Lisp_Object *sequences)
{
  Elemcount len = EMACS_INT_MAX;
  Lisp_Object length;
  int i;

  for (i = 0; i < nsequences; ++i)
    {
      if (CONSP (sequences[i]))
        {
          length = Flist_length (sequences[i]);
          if (!NILP (length))
            {
              len = min (len, XINT (length));
            }
        }
      else
        {
          CHECK_SEQUENCE (sequences[i]);
          length = Flength (sequences[i]);
          len = min (len, XINT (length));
        }
    }

  if (NILP (length))
    {
      signal_circular_list_error (sequences[0]);
    }

  return len;
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

  len = shortest_length_among_sequences (nargs - 2, args + 2);

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
      mapcarX (len, args0, Qnil, function, nargs - 2, args + 2, Qmapconcat);
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
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);
  Lisp_Object *args0;

  args0 = alloca_array (Lisp_Object, len);
  mapcarX (len, args0, Qnil, function, nargs - 1, args + 1, QmapcarX);

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
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);
  Lisp_Object result = make_vector (len, Qnil);

  struct gcpro gcpro1;
  GCPRO1 (result);
  /* Don't pass result as the lisp_object argument, we want mapcarX to protect 
     a single list argument's elements from being garbage-collected. */
  mapcarX (len, XVECTOR_DATA (result), Qnil, function, nargs - 1, args +1,
           Qmapvector);
  RETURN_UNGCPRO (result);
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
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);
  Lisp_Object function = args[0], *result = alloca_array (Lisp_Object, len);

  mapcarX (len, result, Qnil, function, nargs - 1, args + 1, Qmapcan);

  /* #'nconc GCPROs its args in case of signals and error. */
  return Fnconc (len, result);
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
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);
  Lisp_Object sequence = args[1];
  struct gcpro gcpro1;
  /* We need to GCPRO sequence, because mapcarX will modify the
     elements of the args array handed to it, and this may involve
     elements of sequence getting garbage collected. */
  GCPRO1 (sequence);
  mapcarX (len, NULL, Qnil, args[0], nargs - 1, args + 1, Qmapc);
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
  Elemcount len = shortest_length_among_sequences (nargs - 2, args + 2);
  struct gcpro gcpro1;

  if (!NILP (type))
    {
      args0 = alloca_array (Lisp_Object, len);
    }

  mapcarX (len, args0, Qnil, function, nargs - 2, args + 2, Qmap);

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
  Elemcount len;
  Lisp_Object result_sequence = args[0];
  Lisp_Object function = args[1];

  args[0] = function;
  args[1] = result_sequence;

  len = shortest_length_among_sequences (nargs - 1, args + 1);

  mapcarX (len, NULL, result_sequence, function, nargs - 2, args + 2,
           Qmap_into);

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
  Lisp_Object result = Qnil,
    result_ptr = STORE_VOID_IN_LISP ((void *) &result);
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);

  mapcarX (len, NULL, result_ptr, args[0], nargs - 1, args +1, Qsome);

  return result;
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
  Lisp_Object result = Qt, result_ptr = STORE_VOID_IN_LISP ((void *) &result);
  Elemcount len = shortest_length_among_sequences (nargs - 1, args + 1);

  mapcarX (len, NULL, result_ptr, args[0], nargs - 1, args +1, Qevery);

  return result;
}

/* Call FUNCTION with NLISTS arguments repeatedly, each Nth argument
   corresponding to the result of calling (nthcdr ITERATION-COUNT LISTS[N]),
   until that #'nthcdr expression gives nil for some element of LISTS.

   CALLER is a symbol reflecting the Lisp-visible function that was called,
   and any errors thrown because SEQUENCES was modified will reflect it.

   If CALLER is Qmapl, return LISTS[0]. Otherwise, return a list of the
   return values from FUNCTION; if caller is Qmapcan, nconc them together.

   In contrast to mapcarX, we don't require our callers to check LISTS for
   well-formedness, we signal wrong-type-argument if it's not a list, or
   circular-list if it's circular. */

static Lisp_Object
maplist (Lisp_Object function, int nlists, Lisp_Object *lists,
         Lisp_Object caller)
{
  Lisp_Object nconcing[2], accum = Qnil, *args, *tortoises, funcalled;
  Lisp_Object result = EQ (caller, Qmapl) ? lists[0] : Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int i, j, continuing = (nlists > 0), called_count = 0;

  args = alloca_array (Lisp_Object, nlists + 1);
  args[0] = function;
  for (i = 1; i <= nlists; ++i)
    {
      args[i] = Qnil;
    }

  tortoises = alloca_array (Lisp_Object, nlists);
  memcpy (tortoises, lists, nlists * sizeof (Lisp_Object));

  if (EQ (caller, Qmapcon))
    {
      nconcing[0] = Qnil;
      nconcing[1] = Qnil;
      GCPRO4 (args[0], nconcing[0], tortoises[0], result);
      gcpro1.nvars = 1;
      gcpro2.nvars = 2;
      gcpro3.nvars = nlists;
    }
  else
    {
      GCPRO3 (args[0], tortoises[0], result);
      gcpro1.nvars = 1;
      gcpro2.nvars = nlists;
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
	      lists[j] = wrong_type_argument (Qlistp, lists[j]);
	    }
	}
      if (!continuing) break;
      funcalled = IGNORE_MULTIPLE_VALUES (Ffuncall (nlists + 1, args));

      if (EQ (caller, Qmapl))
	{
          DO_NOTHING;
        }
      else if (EQ (caller, Qmapcon))
        {
          nconcing[1] = funcalled;
          accum = bytecode_nconc2 (nconcing);
          if (NILP (result))
            {
              result = accum;
            }
          /* Only check a given stretch of result for well-formedness
             once: */
          nconcing[0] = funcalled;
        }
      else if (NILP (accum))
        {
          accum = result = Fcons (funcalled, Qnil);
        }
      else
        {
          /* Add to the end, avoiding the need to call nreverse
             once we're done: */
          XSETCDR (accum, Fcons (funcalled, Qnil));
          accum = XCDR (accum);
	}

      if (++called_count > CIRCULAR_LIST_SUSPICION_LENGTH)
        {
          if (called_count & 1)
            {
              for (j = 0; j < nlists; ++j)
                {
                  tortoises[j] = XCDR (tortoises[j]);
                  if (EQ (lists[j], tortoises[j]))
                    {
                      signal_circular_list_error (lists[j]);
                    }
                }
            }
          else
            {
              for (j = 0; j < nlists; ++j)
                {
                  if (EQ (lists[j], tortoises[j]))
                    {
                      signal_circular_list_error (lists[j]);
                    }
                }
            }
        }
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
  return maplist (args[0], nargs - 1, args + 1, Qmaplist);
}

DEFUN ("mapl", Fmapl, 2, MANY, 0, /*
Like `maplist', but do not accumulate values returned by the function.

arguments: (FUNCTION LIST &rest LISTS)
*/
       (int nargs, Lisp_Object *args))
{
  return maplist (args[0], nargs - 1, args + 1, Qmapl);
}

DEFUN ("mapcon", Fmapcon, 2, MANY, 0, /*
Like `maplist', but chains together the values returned by FUNCTION.

FUNCTION must return a list (unless it happens to be the last
iteration); the results will be concatenated together using `nconc'.

arguments: (FUNCTION LIST &rest LISTS)
*/
       (int nargs, Lisp_Object *args))
{
  return maplist (args[0], nargs - 1, args + 1, Qmapcon);
}

/* Extra random functions */

DEFUN ("reduce", Freduce, 2, MANY, 0, /*
Combine the elements of sequence using FUNCTION, a binary operation.

For example, `(reduce #'+ SEQUENCE)' returns the sum of all elements in
SEQUENCE, and `(reduce #'union SEQUENCE)' returns the union of all elements
in SEQUENCE.

Keywords supported:  :start :end :from-end :initial-value :key
See `remove*' for the meaning of :start, :end, :from-end and :key.

:initial-value specifies an element (typically an identity element, such as
0) that is conceptually prepended to the sequence (or appended, when
:from-end is given).

If the sequence has one element, that element is returned directly.
If the sequence has no elements, :initial-value is returned if given;
otherwise, FUNCTION is called with no arguments, and its result returned.

arguments: (FUNCTION SEQUENCE &key (START 0) (END (length SEQUENCE)) FROM-END INITIAL-VALUE (KEY #'identity))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object function = args[0], sequence = args[1], accum = Qunbound;
  Elemcount starting, ending = EMACS_INT_MAX, ii = 0;

  PARSE_KEYWORDS (Freduce, nargs, args, 5,
                  (start, end, from_end, initial_value, key),
                  (start = Qzero, initial_value = Qunbound));

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);

  CHECK_KEY_ARGUMENT (key);

#define KEY(key, item) (EQ (Qidentity, key) ? item :			\
			IGNORE_MULTIPLE_VALUES (call1 (key, item)))
#define CALL2(function, accum, item)				\
  IGNORE_MULTIPLE_VALUES (call2 (function, accum, item))

  starting = XINT (start);
  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = XINT (end);
    }

  if (!(starting <= ending))
    {
      check_sequence_range (sequence, start, end, Flength (sequence));
    }

  if (VECTORP (sequence))
    {
      Lisp_Vector *vv = XVECTOR (sequence);

      check_sequence_range (sequence, start, end, make_int (vv->size));

      ending = min (ending, vv->size);

      if (!UNBOUNDP (initial_value))
        {
          accum = initial_value;
        }
      else if (ending - starting)
        {
          if (NILP (from_end))
            {
              accum = KEY (key, vv->contents[starting]);
              starting++;
            }
          else
            {
              accum = KEY (key, vv->contents[ending - 1]);
              ending--;
            }
        }

      if (NILP (from_end))
        {
          for (ii = starting; ii < ending; ++ii)
            {
              accum = CALL2 (function, accum, KEY (key, vv->contents[ii]));
            }
        }
      else
        {
          for (ii = ending - 1; ii >= starting; --ii)
            {
              accum = CALL2 (function, KEY (key, vv->contents[ii]), accum);
            }
        }
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *bv = XBIT_VECTOR (sequence);

      check_sequence_range (sequence, start, end, make_int (bv->size));

      ending = min (ending, bv->size);

      if (!UNBOUNDP (initial_value))
        {
          accum = initial_value;
        }
      else if (ending - starting)
        {
          if (NILP (from_end))
            {
              accum = KEY (key, make_int (bit_vector_bit (bv, starting)));
              starting++;
            }
          else
            {
              accum = KEY (key, make_int (bit_vector_bit (bv, ending - 1)));
              ending--;
            }
        }

      if (NILP (from_end))
        {
          for (ii = starting; ii < ending; ++ii)
            {
              accum = CALL2 (function, accum,
                             KEY (key, make_int (bit_vector_bit (bv, ii))));
            }
        }
      else
        {
          for (ii = ending - 1; ii >= starting; --ii)
            {
              accum = CALL2 (function, KEY (key,
                                            make_int (bit_vector_bit (bv,
                                                                      ii))),
                             accum);
            }
        }
    }
  else if (STRINGP (sequence))
    {
      if (NILP (from_end))
        {
          Bytecount byte_len = XSTRING_LENGTH (sequence);
          Bytecount cursor_offset = 0;
          const Ibyte *startp = XSTRING_DATA (sequence);
          const Ibyte *cursor = startp;

          for (ii = 0; ii != starting && cursor_offset < byte_len; ++ii)
            {
              INC_IBYTEPTR (cursor);
              cursor_offset = cursor - startp;
            }

          if (!UNBOUNDP (initial_value))
            {
              accum = initial_value;
            }
          else if (ending - starting)
            {
              accum = KEY (key, make_char (itext_ichar (cursor)));
              starting++;
              startp = XSTRING_DATA (sequence);
              cursor = startp + cursor_offset;

              if (byte_len != XSTRING_LENGTH (sequence)
                  || !valid_ibyteptr_p (cursor))
                {
                  mapping_interaction_error (Qreduce, sequence);
                }

              INC_IBYTEPTR (cursor);
              cursor_offset = cursor - startp;
            }

          while (cursor_offset < byte_len && ii < ending)
            {
              accum = CALL2 (function, accum, 
                             KEY (key, make_char (itext_ichar (cursor))));

	      startp = XSTRING_DATA (sequence);
	      cursor = startp + cursor_offset;

              if (byte_len != XSTRING_LENGTH (sequence)
                  || !valid_ibyteptr_p (cursor))
                {
                  mapping_interaction_error (Qreduce, sequence);
                }

              INC_IBYTEPTR (cursor);
              cursor_offset = cursor - startp;
              ++ii;
            }

	  if (ii < starting || (ii < ending && !NILP (end)))
	    {
	      check_sequence_range (sequence, start, end, Flength (sequence));
	      ABORT ();
	    }
        }
      else
        {
          Elemcount len = string_char_length (sequence);
          Bytecount cursor_offset, byte_len = XSTRING_LENGTH (sequence);
          const Ibyte *cursor;

	  check_sequence_range (sequence, start, end, make_int (len));

          ending = min (ending, len);
          cursor = string_char_addr (sequence, ending - 1);
          cursor_offset = cursor - XSTRING_DATA (sequence);

          if (!UNBOUNDP (initial_value))
            {
              accum = initial_value;
            }
          else if (ending - starting)
            {
              accum = KEY (key, make_char (itext_ichar (cursor)));
              ending--;
              if (ending > 0)
                {
		  cursor = XSTRING_DATA (sequence) + cursor_offset;

                  if (!valid_ibyteptr_p (cursor))
                    {
                      mapping_interaction_error (Qreduce, sequence);
                    }

                  DEC_IBYTEPTR (cursor);
                  cursor_offset = cursor - XSTRING_DATA (sequence);
                }
            }

          for (ii = ending - 1; ii >= starting; --ii)
            {
              accum = CALL2 (function, KEY (key,
                                            make_char (itext_ichar (cursor))),
                             accum);
              if (ii > 0)
                {
                  cursor = XSTRING_DATA (sequence) + cursor_offset;

                  if (byte_len != XSTRING_LENGTH (sequence)
                      || !valid_ibyteptr_p (cursor))
                    {
                      mapping_interaction_error (Qreduce, sequence);
                    }

                  DEC_IBYTEPTR (cursor);
                  cursor_offset = cursor - XSTRING_DATA (sequence);
                }
            }
        }
    }
  else if (LISTP (sequence))
    {
      if (NILP (from_end))
        {
	  struct gcpro gcpro1;
	  Lisp_Object tailed = Qnil;

	  GCPRO1 (tailed);

          if (!UNBOUNDP (initial_value))
            {
              accum = initial_value;
            }
          else if (ending - starting)
            {
              EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
                {
		  /* KEY may amputate the list behind us; make sure what
		     remains to be processed is still reachable.  */
		  tailed = tail;
                  if (ii == starting)
                    {
                      accum = KEY (key, elt);
                      starting++;
                      break;
                    }
                  ++ii;
                }
            }

	  ii = 0;

          if (ending - starting)
            {
              EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
                {
		  /* KEY or FUNCTION may amputate the list behind us; make
		     sure what remains to be processed is still
		     reachable.  */
		  tailed = tail;
                  if (ii >= starting)
                    {
                      if (ii < ending)
                        {
                          accum = CALL2 (function, accum, KEY (key, elt));
                        }
                      else if (ii == ending)
                        {
                          break;
                        }
                    }
                  ++ii;
                }
            }

	  UNGCPRO;

	  if (ii < starting || (ii < ending && !NILP (end)))
	    {
	      check_sequence_range (sequence, start, end, Flength (sequence));
	      ABORT ();
	    }
        }
      else
        {
          Boolint need_accum = 0;
          Lisp_Object *subsequence = NULL;
          Elemcount counting = 0, len = 0;
	  struct gcpro gcpro1;

	  len = XINT (Flength (sequence));
	  check_sequence_range (sequence, start, end, make_int (len));
	  ending = min (ending, len);

          /* :from-end with a list; make an alloca copy of the relevant list
             data, attempting to go backwards isn't worth the trouble. */
          if (!UNBOUNDP (initial_value))
            {
              accum = initial_value;
              if (ending - starting && starting < ending)
                {
                  subsequence = alloca_array (Lisp_Object, ending - starting);
                }
            }
          else if (ending - starting && starting < ending)
            {
              subsequence = alloca_array (Lisp_Object, ending - starting);
              need_accum = 1;
            }

          if (ending - starting && starting < ending)
            {
              EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
                {
                  if (counting >= starting)
                    {
                      if (counting < ending)
                        {
                          subsequence[ii++] = elt;
                        }
                      else if (counting == ending)
                        {
                          break;
                        }
                    }
		  ++counting;
                }
            }

	  if (subsequence != NULL)
	    {
	      len = ending - starting;
	      /* If we could be sure that neither FUNCTION nor KEY modify
		 SEQUENCE, this wouldn't be necessary, since all the
		 elements of SUBSEQUENCE would definitely always be
		 reachable via SEQUENCE.  */
	      GCPRO1 (subsequence[0]);
	      gcpro1.nvars = len;
	    }

          if (need_accum)
            {
              accum = KEY (key, subsequence[len - 1]);
              --len;
            }

          for (ii = len; ii != 0;)
            {
              --ii;
              accum = CALL2 (function, KEY (key, subsequence[ii]), accum);
            }

	  if (subsequence != NULL)
	    {
	      UNGCPRO;
	    }
        }
    }

  /* At this point, if ACCUM is unbound, SEQUENCE has no elements; we
     need to return the result of calling FUNCTION with zero
     arguments. */
  if (UNBOUNDP (accum))
    {
      accum = IGNORE_MULTIPLE_VALUES (call0 (function));
    }

  return accum;
}

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

/* This function is the implementation of fill_string_range() and
   replace_string_range(); see the comments for those functions. */
static Lisp_Object
replace_string_range_1 (Lisp_Object dest, Lisp_Object start, Lisp_Object end,
			const Ibyte *source, const Ibyte *source_limit,
			Lisp_Object item)
{
  Ibyte *destp = XSTRING_DATA (dest), *p = destp,
    *pend = p + XSTRING_LENGTH (dest), *pcursor, item_buf[MAX_ICHAR_LEN];
  Bytecount prefix_bytecount, source_len = source_limit - source;
  Charcount ii = 0, starting = XINT (start), ending, len;
  Elemcount delta;

  while (ii < starting && p < pend)
    {
      INC_IBYTEPTR (p);
      ii++;
    }

  pcursor = p;

  if (NILP (end))
    {
      while (pcursor < pend)
	{
	  INC_IBYTEPTR (pcursor);
	  ii++;
	}

      ending = len = ii;
    }
  else
    {
      ending = XINT (end);
      while (ii < ending && pcursor < pend)
	{
	  INC_IBYTEPTR (pcursor);
	  ii++;
	}
    }

  if (pcursor == pend)
    {
      /* We have the length, check it for our callers. */
      check_sequence_range (dest, start, end, make_int (ii));
    }

  if (!(p == pend || p == pcursor))
    {
      prefix_bytecount = p - destp;

      if (!NILP (item))
	{
	  assert (source == NULL && source_limit == NULL);
	  source_len = set_itext_ichar (item_buf, XCHAR (item));
	  delta = (source_len * (ending - starting)) - (pcursor - p);
	}
      else
	{
	  assert (source != NULL && source_limit != NULL);
	  delta = source_len - (pcursor - p);
	}

      if (delta)
        {
          resize_string (dest, prefix_bytecount, delta);
          destp = XSTRING_DATA (dest);
          pcursor = destp + prefix_bytecount + (pcursor - p);
          p = destp + prefix_bytecount;
        }

      if (CHARP (item))
	{
	  while (starting < ending)
	    {
	      memcpy (p, item_buf, source_len);
	      p += source_len;
	      starting++;
	    }
	}
      else
	{
	  while (starting < ending && source < source_limit)
	    {
	      source_len = itext_copy_ichar (source, p);
	      p += source_len, source += source_len;
	    }
	}

      init_string_ascii_begin (dest);
      bump_string_modiff (dest);
      sledgehammer_check_ascii_begin (dest);
    }

  return dest;
}

DEFUN ("replace", Freplace, 2, MANY, 0, /*
Replace the elements of SEQUENCE-ONE with the elements of SEQUENCE-TWO.

SEQUENCE-ONE is destructively modified, and returned.  Its length is not
changed.

Keywords :start1 and :end1 specify a subsequence of SEQUENCE-ONE, and
:start2 and :end2 a subsequence of SEQUENCE-TWO.  See `search' for more
information.

arguments: (SEQUENCE-ONE SEQUENCE-TWO &key (START1 0) (END1 (length SEQUENCE-ONE)) (START2 0) (END2 (length SEQUENCE-TWO)))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence1 = args[0], sequence2 = args[1],
    result = sequence1;
  Elemcount starting1, ending1 = EMACS_INT_MAX, starting2;
  Elemcount ending2 = EMACS_INT_MAX, counting = 0, startcounting;
  Boolint sequence1_listp, sequence2_listp,
    overwriting = EQ (sequence1, sequence2);

  PARSE_KEYWORDS (Freplace, nargs, args, 4, (start1, end1, start2, end2),
                  (start1 = start2 = Qzero));

  CHECK_SEQUENCE (sequence1);
  CHECK_LISP_WRITEABLE (sequence1);

  CHECK_SEQUENCE (sequence2);

  CHECK_NATNUM (start1);
  starting1 = XINT (start1);
  CHECK_NATNUM (start2);
  starting2 = XINT (start2);

  if (!NILP (end1))
    {
      CHECK_NATNUM (end1);
      ending1 = XINT (end1);

      if (!(starting1 <= ending1))
        {
          args_out_of_range_3 (sequence1, start1, end1);
        }
    }

  if (!NILP (end2))
    {
      CHECK_NATNUM (end2);
      ending2 = XINT (end2);

      if (!(starting2 <= ending2))
        {
          args_out_of_range_3 (sequence1, start2, end2);
        }
    }

  sequence1_listp = LISTP (sequence1);
  sequence2_listp = LISTP (sequence2);

  overwriting = overwriting && starting2 <= starting1;

  if (sequence1_listp && !ZEROP (start1))
    {
      sequence1 = Fnthcdr (start1, sequence1);

      if (NILP (sequence1))
        {
          check_sequence_range (args[0], start1, end1, Flength (args[0]));
          /* Give up early here. */
          return result;
        }

      ending1 -= starting1;
      starting1 = 0;
    }

  if (sequence2_listp && !ZEROP (start2))
    {
      sequence2 = Fnthcdr (start2, sequence2);

      if (NILP (sequence2))
        {
          check_sequence_range (args[1], start1, end1, Flength (args[1]));
          /* Nothing available to replace sequence1's contents. */
          return result;
        }

      ending2 -= starting2;
      starting2 = 0;
    }

  if (overwriting)
    {
      if (EQ (start1, start2))
        {
          return result;
        }

      /* Our ranges may overlap. Save the data that might be overwritten. */

      if (CONSP (sequence2))
        {
          Elemcount len = XINT (Flength (sequence2));
          Lisp_Object *subsequence
            = alloca_array (Lisp_Object, min (ending2, len));
          Elemcount ii = 0;

          LIST_LOOP_2 (elt, sequence2)
            {
              if (counting == ending2)
                {
                  break;
                }

              subsequence[ii++] = elt;
              counting++;
            }

          check_sequence_range (sequence1, start1, end1,
                                /* The XINT (start2) is intentional here; we
                                   called #'length after doing (nthcdr
                                   start2 sequence2). */
                                make_int (XINT (start2) + len));
          check_sequence_range (sequence2, start2, end2,
                                make_int (XINT (start2) + len));

          while (starting1 < ending1
                 && starting2 < ending2 && !NILP (sequence1))
            {
              XSETCAR (sequence1, subsequence[starting2]);
              sequence1 = XCDR (sequence1);
              starting1++;
              starting2++;
            }
        }
      else if (STRINGP (sequence2))
        {
          Ibyte *p = XSTRING_DATA (sequence2),
            *pend = p + XSTRING_LENGTH (sequence2), *pcursor,
            *staging;
          Bytecount ii = 0;

          while (ii < starting2 && p < pend)
            {
              INC_IBYTEPTR (p);
              ii++;
            }

          pcursor = p;

          while (ii < ending2 && starting1 < ending1 && pcursor < pend)
            {
              INC_IBYTEPTR (pcursor);
              starting1++;
              ii++;
            }

          if (pcursor == pend)
            {
              check_sequence_range (sequence1, start1, end1, make_int (ii));
              check_sequence_range (sequence2, start2, end2, make_int (ii));
            }
          else
            {
              assert ((pcursor - p) > 0);
              staging = alloca_ibytes (pcursor - p);
              memcpy (staging, p, pcursor - p);
              replace_string_range (result, start1,
                                    make_int (starting1),
                                    staging, staging + (pcursor - p));
            }
        }
      else 
        {
          Elemcount seq_len = XINT (Flength (sequence2)), ii = 0,
            subseq_len = min (min (ending1 - starting1, seq_len - starting1),
                              min (ending2 - starting2, seq_len - starting2));
          Lisp_Object *subsequence = alloca_array (Lisp_Object, subseq_len);

          check_sequence_range (sequence1, start1, end1, make_int (seq_len));
          check_sequence_range (sequence2, start2, end2, make_int (seq_len));

          while (starting2 < ending2 && ii < seq_len)
            {
              subsequence[ii] = Faref (sequence2, make_int (starting2));
              ii++, starting2++;
            }

          ii = 0;

          while (starting1 < ending1 && ii < seq_len)
            {
              Faset (sequence1, make_int (starting1), subsequence[ii]);
              ii++, starting1++;
            }
        }
    }
  else if (sequence1_listp && sequence2_listp)
    {
      Lisp_Object sequence1_tortoise = sequence1,
        sequence2_tortoise = sequence2;
      Elemcount shortest_len = 0;

      counting = startcounting = min (ending1, ending2);

      while (counting-- > 0 && !NILP (sequence1) && !NILP (sequence2))
        {
          XSETCAR (sequence1,
                   CONSP (sequence2) ? XCAR (sequence2)
                   : Fcar (sequence2));
          sequence1 = CONSP (sequence1) ? XCDR (sequence1)
            : Fcdr (sequence1);
          sequence2 = CONSP (sequence2) ? XCDR (sequence2)
            : Fcdr (sequence2);

          shortest_len++;

          if (startcounting - counting > CIRCULAR_LIST_SUSPICION_LENGTH)
            {
              if (counting & 1)
                {
                  sequence1_tortoise = XCDR (sequence1_tortoise);
                  sequence2_tortoise = XCDR (sequence2_tortoise);
                }

              if (EQ (sequence1, sequence1_tortoise))
                {
                  signal_circular_list_error (sequence1);
                }

              if (EQ (sequence2, sequence2_tortoise))
                {
                  signal_circular_list_error (sequence2);
                }
            }
        }

      if (NILP (sequence1))
        {
          check_sequence_range (sequence1, start1, end1,
                                make_int (XINT (start1) + shortest_len));
        }
      else if (NILP (sequence2))
        {
          check_sequence_range (sequence2, start2, end2,
                                make_int (XINT (start2) + shortest_len));
        }
    }
  else if (sequence1_listp)
    {
      if (STRINGP (sequence2))
        {
          Ibyte *s2_data = XSTRING_DATA (sequence2),
            *s2_end = s2_data + XSTRING_LENGTH (sequence2);
          Elemcount char_count = 0;
          Lisp_Object character;

          while (char_count < starting2 && s2_data < s2_end)
            {
              INC_IBYTEPTR (s2_data);
              char_count++;
            }

          while (starting1 < ending1 && starting2 < ending2
                 && s2_data < s2_end && !NILP (sequence1))
            {
              character = make_char (itext_ichar (s2_data));
              CONSP (sequence1) ?
                XSETCAR (sequence1, character)
                : Fsetcar (sequence1, character);
              sequence1 = XCDR (sequence1);
              starting1++;
              starting2++;
              char_count++;
              INC_IBYTEPTR (s2_data);
            }

          if (NILP (sequence1))
            {
              check_sequence_range (sequence1, start1, end1,
                                    make_int (XINT (start1) + starting1));
            }

          if (s2_data == s2_end)
            {
              check_sequence_range (sequence2, start2, end2,
                                    make_int (char_count));
            }
        }
      else
        {
          Elemcount len2 = XINT (Flength (sequence2));
          check_sequence_range (sequence2, start2, end2, make_int (len2));

          ending2 = min (ending2, len2);
          while (starting2 < ending2
                 && starting1 < ending1 && !NILP (sequence1))
            {
              CHECK_CONS (sequence1);
              XSETCAR (sequence1, Faref (sequence2, make_int (starting2)));
              sequence1 = XCDR (sequence1);
              starting1++;
              starting2++;
            }

          if (NILP (sequence1))
            {
              check_sequence_range (sequence1, start1, end1,
                                    make_int (XINT (start1) + starting1));
            }
        }
    }
  else if (sequence2_listp)
    {
      if (STRINGP (sequence1))
        {
          Elemcount ii = 0, count, len = string_char_length (sequence1);
          Ibyte *staging, *cursor;
          Lisp_Object obj;

          check_sequence_range (sequence1, start1, end1, make_int (len));
          ending1 = min (ending1, len);
          count = ending1 - starting1;
          staging = cursor = alloca_ibytes (count * MAX_ICHAR_LEN);

          while (ii < count && !NILP (sequence2))
            {
              obj = CONSP (sequence2) ? XCAR (sequence2)
                : Fcar (sequence2);

              CHECK_CHAR_COERCE_INT (obj);
              cursor += set_itext_ichar (cursor, XCHAR (obj));
              ii++;
              sequence2 = XCDR (sequence2);
            }

          if (NILP (sequence2))
            {
              check_sequence_range (sequence2, start2, end2,
                                    make_int (XINT (start2) + ii));
            }

          replace_string_range (result, start1, make_int (XINT (start1) + ii),
                                staging, cursor);
        }
      else
        {
          Elemcount len = XINT (Flength (sequence1));

          check_sequence_range (sequence1, start2, end1, make_int (len));
          ending1 = min (ending2, min (ending1, len));

          while (starting1 < ending1 && !NILP (sequence2))
            {
              Faset (sequence1, make_int (starting1),
                     CONSP (sequence2) ? XCAR (sequence2)
                     : Fcar (sequence2));
              sequence2 = XCDR (sequence2);
              starting1++;
              starting2++;
            }

          if (NILP (sequence2))
            {
              check_sequence_range (sequence2, start2, end2,
                                    make_int (XINT (start2) + starting2));
            }
        }
    }
  else
    {
      if (STRINGP (sequence1) && STRINGP (sequence2))
        {
          Ibyte *p2 = XSTRING_DATA (sequence2),
            *p2end = p2 + XSTRING_LENGTH (sequence2), *p2cursor;
          Charcount ii = 0, len1 = string_char_length (sequence1);

          while (ii < starting2 && p2 < p2end)
            {
              INC_IBYTEPTR (p2);
              ii++;
            }

          p2cursor = p2;
          ending1 = min (ending1, len1);

          while (ii < ending2 && starting1 < ending1 && p2cursor < p2end)
            {
              INC_IBYTEPTR (p2cursor);
              ii++;
              starting1++;
            }

          if (p2cursor == p2end)
            {
              check_sequence_range (sequence2, start2, end2, make_int (ii));
            }

          /* This isn't great; any error message won't necessarily reflect
             the END1 that was supplied to #'replace. */
          replace_string_range (result, start1, make_int (starting1),
                                p2, p2cursor);
        }
      else if (STRINGP (sequence1))
        {
          Ibyte *staging, *cursor;
          Elemcount count, len1 = string_char_length (sequence1);
          Elemcount len2 = XINT (Flength (sequence2)), ii = 0;
          Lisp_Object obj;

          check_sequence_range (sequence1, start1, end1, make_int (len1));
          check_sequence_range (sequence2, start2, end2, make_int (len2));

          ending1 = min (ending1, len1);
          ending2 = min (ending2, len2);
          count = min (ending1 - starting1, ending2 - starting2);
          staging = cursor = alloca_ibytes (count * MAX_ICHAR_LEN);

          ii = 0;
          while (ii < count)
            {
              obj = Faref (sequence2, make_int (starting2));

              CHECK_CHAR_COERCE_INT (obj);
              cursor += set_itext_ichar (cursor, XCHAR (obj));
              starting2++, ii++;
            }

          replace_string_range (result, start1,
                                make_int (XINT (start1) + count),
                                staging, cursor);
        }
      else if (STRINGP (sequence2))
        {
          Ibyte *p2 = XSTRING_DATA (sequence2),
            *p2end = p2 + XSTRING_LENGTH (sequence2);
          Elemcount len1 = XINT (Flength (sequence1)), ii = 0;

          check_sequence_range (sequence1, start1, end1, make_int (len1));
          ending1 = min (ending1, len1);

          while (ii < starting2 && p2 < p2end)
            {
              INC_IBYTEPTR (p2);
              ii++;
            }

          while (p2 < p2end && starting1 < ending1 && starting2 < ending2)
            {
              Faset (sequence1, make_int (starting1),
                     make_char (itext_ichar (p2)));
              INC_IBYTEPTR (p2);
              starting1++;
              starting2++;
              ii++;
            }

          if (p2 == p2end)
            {
              check_sequence_range (sequence2, start2, end2, make_int (ii));
            }
        }
      else
        {
          Elemcount len1 = XINT (Flength (sequence1)),
            len2 = XINT (Flength (sequence2));

          check_sequence_range (sequence1, start1, end1, make_int (len1));
          check_sequence_range (sequence2, start2, end2, make_int (len2));

          ending1 = min (ending1, len1);
          ending2 = min (ending2, len2);
          
          while (starting1 < ending1 && starting2 < ending2)
            {
              Faset (sequence1, make_int (starting1),
                     Faref (sequence2, make_int (starting2)));
              starting1++;
              starting2++;
            }
        }
    }

  return result;
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
  assert (encoded_length <= allength);
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
  assert (encoded_length <= allength);
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
  assert (decoded_length <= length * MAX_ICHAR_LEN);
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
  assert (decoded_length <= length * MAX_ICHAR_LEN);
  Lstream_delete (XLSTREAM (input));

  result = make_string (decoded, decoded_length);
  unbind_to (speccount);
  return result;
}

Lisp_Object Qyes_or_no_p;

void
syms_of_fns (void)
{
  INIT_LISP_OBJECT (bit_vector);

  DEFSYMBOL (Qstring_lessp);
  DEFSYMBOL (Qsort);
  DEFSYMBOL (Qmerge);
  DEFSYMBOL (Qfill);
  DEFSYMBOL (Qidentity);
  DEFSYMBOL (Qvector);
  DEFSYMBOL (Qarray);
  DEFSYMBOL (Qstring);
  DEFSYMBOL (Qlist);
  DEFSYMBOL (Qbit_vector);
  defsymbol (&QsortX, "sort*");
  DEFSYMBOL (Qreduce);
  DEFSYMBOL (Qreplace);

  DEFSYMBOL (Qmapconcat);
  defsymbol (&QmapcarX, "mapcar*");
  DEFSYMBOL (Qmapvector);
  DEFSYMBOL (Qmapcan);
  DEFSYMBOL (Qmapc);
  DEFSYMBOL (Qmap);
  DEFSYMBOL (Qmap_into);
  DEFSYMBOL (Qsome);
  DEFSYMBOL (Qevery);
  DEFSYMBOL (Qmaplist);
  DEFSYMBOL (Qmapl);
  DEFSYMBOL (Qmapcon);

  DEFKEYWORD (Q_from_end);
  DEFKEYWORD (Q_initial_value);
  DEFKEYWORD (Q_start1);
  DEFKEYWORD (Q_start2);
  DEFKEYWORD (Q_end1);
  DEFKEYWORD (Q_end2);

  DEFSYMBOL (Qyes_or_no_p);

  DEFERROR_STANDARD (Qbase64_conversion_error, Qconversion_error);

  DEFSUBR (Fidentity);
  DEFSUBR (Frandom);
  DEFSUBR (Flength);
  DEFSUBR (Fsafe_length);
  DEFSUBR (Flist_length);
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
  DEFSUBR (FsortX);
  Ffset (intern ("sort"), QsortX);
  DEFSUBR (Fmerge);
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
  DEFSUBR (Fobject_setplist);
  DEFSUBR (Fequal);
  DEFSUBR (Fequalp);
  DEFSUBR (Fold_equal);
  DEFSUBR (Ffill);
  Ffset (intern ("fillarray"), Qfill);

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

  DEFSUBR (Freduce);
  DEFSUBR (Freplace_list);
  DEFSUBR (Freplace);
  DEFSUBR (Fload_average);
  DEFSUBR (Ffeaturep);
  DEFSUBR (Frequire);
  DEFSUBR (Fprovide);
  DEFSUBR (Fbase64_encode_region);
  DEFSUBR (Fbase64_encode_string);
  DEFSUBR (Fbase64_decode_region);
  DEFSUBR (Fbase64_decode_string);

  DEFSUBR (Fsubstring_no_properties);
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
