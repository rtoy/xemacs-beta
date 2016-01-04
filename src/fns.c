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

extern Fixnum max_lisp_eval_depth;
extern int lisp_eval_depth;

Lisp_Object Qmapl, Qmapcon, Qmaplist, Qbase64_conversion_error;

Lisp_Object Vpath_separator;

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
With positive integer argument LIMIT, return random number in interval [0,
LIMIT).  LIMIT can be a bignum, in which case the range of possible values
is extended.  With argument t, set the random number seed from the current
time and pid.
*/
       (limit))
{
  EMACS_INT val;
  unsigned long denominator;

  if (EQ (limit, Qt))
    seed_random (qxe_getpid () + time (NULL));
  if (NATNUMP (limit) && !ZEROP (limit))
    {
#ifdef HAVE_BIGNUM
      if (BIGNUMP (limit))
        {
          bignum_random (scratch_bignum, XBIGNUM_DATA (limit));
          return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
        }
#endif
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  We do this by using the
	 quotient rather than the remainder.  At the high end of the RNG
	 it's possible to get a quotient larger than limit; discarding
	 these values eliminates the bias that would otherwise appear
	 when using a large limit.  */
      denominator = ((unsigned long)1 << FIXNUM_VALBITS) / XFIXNUM (limit);
      do
	val = get_random () / denominator;
      while (val >= XFIXNUM (limit));
    }
  else
    val = get_random ();

  return make_fixnum (val);
}

/* Random data-structure functions */

void
check_losing_bytecode (const Ascbyte *function, Lisp_Object seq)
{
  if (COMPILED_FUNCTIONP (seq))
    signal_ferror_with_frob
      (Qinvalid_argument, seq,
       "As of 20.3, `%s' no longer works with compiled-function objects",
       function);
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

  return make_fixnum (len);
}

/* This is almost the above, but is defined by Common Lisp. We need it in C
   for shortest_length_among_sequences(), below, for the various sequence
   functions that can usefully operate on circular lists. */

DEFUN ("list-length", Flist_length, 1, 1, 0, /*
Return the length of LIST.  Return nil if LIST is circular.
Error if LIST is dotted.
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

  if (!LISTP (hare))
    {
      signal_malformed_list_error (list);
    }

  return EQ (hare, tortoise) && len != 0 ? Qnil : make_fixnum (len);
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
    return make_fixnum (1 + matching);
  else
    return make_fixnum (-1 - matching);
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
  if (CONSP (XSTRING_PLIST (string)) && FIXNUMP (XCAR (XSTRING_PLIST (string))))
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
  if (CONSP (*ptr) && FIXNUMP (XCAR (*ptr)))
    XCAR (*ptr) = make_fixnum (1+XFIXNUM (XCAR (*ptr)));
  else
    *ptr = Fcons (make_fixnum (1), *ptr);
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
#if 0				/* removed for XEmacs 21 */
      else if (FIXNUMP (seq))
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
        Charcount thislen = XFIXNUM (Flength (args[argnum]));
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
        val = Fmake_list (make_fixnum (total_length), Qnil);
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
	  thisleni = XFIXNUM (Flength (seq));
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
		elt = make_fixnum (bit_vector_bit (XBIT_VECTOR (seq),
						thisindex));
              else
		elt = Felt (seq, make_fixnum (thisindex));
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
	      set_bit_vector_bit (XBIT_VECTOR (val), toindex++, XFIXNUM (elt));
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

  CHECK_STRING (string);
  get_string_range_char (string, start, end, &ccstart, &ccend,
                         GB_HISTORICAL_STRING_BEHAVIOR);
  bstart = string_index_char_to_byte (string, ccstart);
  blen = string_offset_char_to_byte_len (string, bstart, ccend - ccstart);
  val = make_string (XSTRING_DATA (string) + bstart, blen);

  return val;
}

/* Split STRING into a list of substrings.  The substrings are the parts of
   original STRING separated by SEPCHAR.

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

/* Ben thinks [or thought in 1998] this function should not exist or be
   exported to Lisp. It's used to define #'split-path in subr.el, and for
   parsing Carbon font names under that window system. */

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

DEFUN ("nthcdr", Fnthcdr, 2, 2, 0, /*
Take cdr N times on LIST, and return the result.
*/
       (n, list))
{
  /* This function can GC */
  REGISTER EMACS_INT i;
  REGISTER Lisp_Object tail = list;
  CHECK_NATNUM (n);
  for (i = BIGNUMP (n) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (n); i; i--)
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
      int_n = BIGNUMP (n) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (n);
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
      int_n = BIGNUMP (n) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (n);
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
      int_n = BIGNUMP (n) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (n);
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

  la = XFIXNUM (Flength (a));
  lb = XFIXNUM (Flength (b));
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
  return make_fixnum (internal_equal (obj1, obj2, XFIXNUM (depth)));
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
			       3, obj1, obj2, make_fixnum (depth));
  if (UNBOUNDP (glorp))
    return retval;
  else
    return XFIXNUM (glorp);
}

int
internal_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  if (depth + lisp_eval_depth > max_lisp_eval_depth)
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
  if (depth + lisp_eval_depth > max_lisp_eval_depth)
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
	EMACS_INT l1 = XFIXNUM (Flength (obj1));
	EMACS_INT l2 = XFIXNUM (Flength (obj2));
	/* Both arrays, but of different lengths */
	if (l1 != l2)
	  return 0;
	for (i = 0; i < l1; i++)
	  if (!internal_equalp (Faref (obj1, make_fixnum (i)),
				Faref (obj2, make_fixnum (i)), depth + 1))
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

#ifdef SUPPORT_CONFOUNDING_FUNCTIONS

/* Note that we may be calling sub-objects that will use
   internal_equal() (instead of internal_old_equal()).  Oh well.
   We will get an Ebola note if there's any possibility of confusion,
   but that seems unlikely. */

static int
internal_old_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  if (depth + lisp_eval_depth > max_lisp_eval_depth)
    stack_overflow ("Stack overflow in equal", Qunbound);
  QUIT;
  if (HACKEQ_UNSAFE (obj1, obj2))
    return 1;
  /* Note that (equal 20 20.0) should be nil */
  if (XTYPE (obj1) != XTYPE (obj2))
    return 0;

  return internal_equal (obj1, obj2, depth);
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

#endif

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
			  make_fixnum ((int) (100.0 * load_ave[loads]))
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
  else if (FIXNUMP (fexp) || FLOATP (fexp))
    {
      double d = extract_float (fexp);

      if (featurep_emacs_version == 0.0)
	{
	  featurep_emacs_version = XFIXNUM (Vemacs_major_version) +
	    (XFIXNUM (Vemacs_minor_version) / 100.0);
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
		      make_fixnum (streampos));
      value = base64_char_to_value[ec] << 18;

      /* Process second byte of a quadruplet.  */
      ADVANCE_INPUT_IGNORE_NONBASE64 (ec, istream, streampos);
      if (ec < 0)
	base64_conversion_error ("Premature EOF while decoding base64",
				 Qunbound);
      if (ec == '=')
	base64_conversion_error ("Illegal `=' character while decoding base64",
		      make_fixnum (streampos));
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
	       make_fixnum (streampos));
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
  return make_fixnum (encoded_length);
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

  return make_fixnum (cc_decoded_length);
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

void
syms_of_fns (void)
{
  DEFSYMBOL (Qmapl);
  DEFSYMBOL (Qmapcon);
  DEFSYMBOL (Qmaplist);

  DEFERROR_STANDARD (Qbase64_conversion_error, Qconversion_error);

  DEFSUBR (Fidentity);
  DEFSUBR (Frandom);
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
  DEFSUBR (Fnthcdr);
  DEFSUBR (Fnth);
  DEFSUBR (Flast);
  DEFSUBR (Fbutlast);
  DEFSUBR (Fnbutlast);
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

#ifdef SUPPORT_CONFOUNDING_FUNCTIONS
  DEFSUBR (Fold_member);
  DEFSUBR (Fold_memq);
  DEFSUBR (Fold_assoc);
  DEFSUBR (Fold_assq);
  DEFSUBR (Fold_rassq);
  DEFSUBR (Fold_rassoc);
  DEFSUBR (Fold_delete);
  DEFSUBR (Fold_delq);
  DEFSUBR (Fold_equal);
  DEFSUBR (Fold_eq);
#endif

  DEFSUBR (Fnconc);
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

  DEFSUBR (Fsubstring_no_properties);
  DEFSUBR (Fsplit_string_by_char);
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
