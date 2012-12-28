/* Various functions that operate on sequences, split out from fns.c
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

#include <config.h>
#include "lisp.h"
#include "extents.h"

Lisp_Object Qadjoin, Qarray, QassocX, Qbit_vector, Qcar_less_than_car;
Lisp_Object QdeleteX, Qdelete_duplicates, Qevery, Qfill, Qfind, Qidentity;
Lisp_Object Qintersection, Qmap, Qmap_into, Qmapc, Qmapcan, QmapcarX;
Lisp_Object Qmapconcat, Qmapvector, Qmerge, Qmismatch, Qnintersection;
Lisp_Object Qnset_difference, Qnsubstitute, Qnunion, Qposition, QrassocX;
Lisp_Object Qreduce, QremoveX, Qreplace, Qset_difference, Qsome, QsortX;
Lisp_Object Qstring_lessp, Qsubsetp, Qsubstitute, Qvector;

Lisp_Object Q_count, Q_descend_structures, Q_end1, Q_end2, Q_from_end;
Lisp_Object Q_if_, Q_if_not, Q_initial_value, Q_stable, Q_start1, Q_start2;
Lisp_Object Q_test_not;

extern Fixnum max_lisp_eval_depth;
extern int lisp_eval_depth;

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
  Lisp_Object args[] = { Qzero, start, NILP (end) ? length : end, length };

  if (NILP (Fleq (countof (args), args)))
    {
      args_out_of_range_3 (sequence, start, end);
    }
}

DEFUN ("length", Flength, 1, 1, 0, /*
Return the length of vector, bit vector, list or string SEQUENCE.
*/
       (sequence))
{
 retry:
  if (STRINGP (sequence))
    return make_fixnum (string_char_length (sequence));
  else if (CONSP (sequence))
    {
      Elemcount len;
      GET_EXTERNAL_LIST_LENGTH (sequence, len);
      return make_fixnum (len);
    }
  else if (VECTORP (sequence))
    return make_fixnum (XVECTOR_LENGTH (sequence));
  else if (NILP (sequence))
    return Qzero;
  else if (BIT_VECTORP (sequence))
    return make_fixnum (bit_vector_length (XBIT_VECTOR (sequence)));
  else
    {
      check_losing_bytecode ("length", sequence);
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
}

/* Various test functions for #'member*, #'assoc* and the other functions
   that take both TEST and KEY arguments.  */

Boolint
check_eq_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		Lisp_Object item, Lisp_Object elt)
{
  return EQ (item, elt);
}

static Boolint
check_eq_key (Lisp_Object UNUSED (test), Lisp_Object key, Lisp_Object item,
	      Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (key, elt));
  return EQ (item, elt);
}

/* The next two are not used by #'member* and #'assoc*, since we can decide
   on #'eq vs. #'equal when we have the type of ITEM.  */
static Boolint
check_eql_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		       Lisp_Object elt1, Lisp_Object elt2)
{
  return EQ (elt1, elt2)
    || (NON_FIXNUM_NUMBER_P (elt1) && internal_equal (elt1, elt2, 0));
}

static Boolint
check_eql_key (Lisp_Object UNUSED (test), Lisp_Object key, Lisp_Object item,
	       Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (key, elt));
  return EQ (item, elt)
    || (NON_FIXNUM_NUMBER_P (item) && internal_equal (item, elt, 0));
}

static Boolint
check_equal_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		   Lisp_Object item, Lisp_Object elt)
{
  return internal_equal (item, elt, 0);
}

static Boolint
check_equal_key (Lisp_Object UNUSED (test), Lisp_Object key, Lisp_Object item,
		 Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (key, elt));
  return internal_equal (item, elt, 0);
}

static Boolint
check_equalp_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		   Lisp_Object item, Lisp_Object elt)
{
  return internal_equalp (item, elt, 0);
}

static Boolint
check_equalp_key (Lisp_Object UNUSED (test), Lisp_Object key,
		  Lisp_Object item, Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (key, elt));
  return internal_equalp (item, elt, 0);
}

static Boolint
check_string_match_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
			  Lisp_Object item, Lisp_Object elt)
{
  return !NILP (Fstring_match (item, elt, Qnil, Qnil));
}

static Boolint
check_string_match_key (Lisp_Object UNUSED (test), Lisp_Object key,
			Lisp_Object item, Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (key, elt));
  return !NILP (Fstring_match (item, elt, Qnil, Qnil));
}

static Boolint
check_other_nokey (Lisp_Object test, Lisp_Object UNUSED (key),
		   Lisp_Object item, Lisp_Object elt)
{
  Lisp_Object args[] = { test, item, elt };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  item = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  return !NILP (item);
}

static Boolint
check_other_key (Lisp_Object test, Lisp_Object key,
		 Lisp_Object item, Lisp_Object elt)
{
  Lisp_Object args[] = { item, key, elt };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[2] = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args) - 1, args + 1));
  args[1] = item;
  args[0] = test;
  item = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  return !NILP (item);
}

static Boolint
check_if_nokey (Lisp_Object test, Lisp_Object UNUSED (key),
		Lisp_Object UNUSED (item), Lisp_Object elt)
{
  elt = IGNORE_MULTIPLE_VALUES (call1 (test, elt));
  return !NILP (elt);
}

static Boolint
check_if_key (Lisp_Object test, Lisp_Object key,
	      Lisp_Object UNUSED (item), Lisp_Object elt)
{
  Lisp_Object args[] = { key, elt };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  args[0] = test;
  elt = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  return !NILP (elt);
}

static Boolint
check_match_eq_key (Lisp_Object UNUSED (test), Lisp_Object key,
		    Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return EQ (args[0], args[1]);
}

static Boolint
check_match_eql_key (Lisp_Object UNUSED (test), Lisp_Object key,
		       Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return EQ (args[0], args[1]) ||
    (NON_FIXNUM_NUMBER_P (args[0]) && internal_equal (args[0], args[1], 0));
}

static Boolint
check_match_equal_key (Lisp_Object UNUSED (test), Lisp_Object key,
		       Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return internal_equal (args[0], args[1], 0);
}

static Boolint
check_match_equalp_key (Lisp_Object UNUSED (test), Lisp_Object key,
			Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return internal_equalp (args[0], args[1], 0);
}

static Boolint
check_match_other_key (Lisp_Object test, Lisp_Object key,
		       Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[2] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  args[1] = args[0];
  args[0] = test;

  elt1 = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  return !NILP (elt1);
}

static Boolint
check_lss_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		 Lisp_Object elt1, Lisp_Object elt2)
{
  return bytecode_arithcompare (elt1, elt2) < 0;
}

static Boolint
check_lss_key (Lisp_Object UNUSED (test), Lisp_Object key,
	       Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return bytecode_arithcompare (args[0], args[1]) < 0;
}

Boolint
check_lss_key_car (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
		   Lisp_Object elt1, Lisp_Object elt2)
{
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (elt1, elt2);
  elt1 = CONSP (elt1) ? XCAR (elt1) : Fcar (elt1);
  elt2 = CONSP (elt2) ? XCAR (elt2) : Fcar (elt2);
  UNGCPRO;

  return bytecode_arithcompare (elt1, elt2) < 0;
}

Boolint
check_string_lessp_nokey (Lisp_Object UNUSED (test), Lisp_Object UNUSED (key),
			  Lisp_Object elt1, Lisp_Object elt2)
{
  return !NILP (Fstring_lessp (elt1, elt2));
}

static Boolint
check_string_lessp_key (Lisp_Object UNUSED (test), Lisp_Object key,
			Lisp_Object elt1, Lisp_Object elt2)
{
  Lisp_Object args[] = { key, elt1, elt2 };
  struct gcpro gcpro1;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  args[0] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args));
  args[1] = key;
  args[1] = IGNORE_MULTIPLE_VALUES (Ffuncall (2, args + 1));
  UNGCPRO;

  return !NILP (Fstring_lessp (args[0], args[1]));
}

static Boolint
check_string_lessp_key_car (Lisp_Object UNUSED (test),
			    Lisp_Object UNUSED (key),
			    Lisp_Object elt1, Lisp_Object elt2)
{
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (elt1, elt2);
  elt1 = CONSP (elt1) ? XCAR (elt1) : Fcar (elt1);
  elt2 = CONSP (elt2) ? XCAR (elt2) : Fcar (elt2);
  UNGCPRO;

  return !NILP (Fstring_lessp (elt1, elt2));
}

static check_test_func_t
get_check_match_function_1 (Lisp_Object item,
			    Lisp_Object *test_inout, Lisp_Object test_not,
			    Lisp_Object if_, Lisp_Object if_not,
			    Lisp_Object key, Boolint *test_not_unboundp_out,
			    check_test_func_t *test_func_out)
{
  Lisp_Object test = *test_inout;
  check_test_func_t result = NULL, test_func = NULL;
  Boolint force_if = 0;

  if (!NILP (if_))
    {
      if (!(NILP (test) && NILP (test_not) && NILP (if_not)))
	{
	  invalid_argument ("only one keyword among :test :test-not "
			    ":if :if-not allowed", if_);
	}

      test = *test_inout = if_;
      force_if = 1;
    }
  else if (!NILP (if_not))
    {
      if (!(NILP (test) && NILP (test_not)))
	{
	  invalid_argument ("only one keyword among :test :test-not "
			    ":if :if-not allowed", if_not);
	}

      test_not = if_not;
      force_if = 1;
    }

  if (NILP (test))
    {
      if (!NILP (test_not))
	{
	  test = *test_inout = test_not;
	  if (NULL != test_not_unboundp_out)
	    {
	      *test_not_unboundp_out = 0; 
	    }
	}
      else
	{
	  test = Qeql;
	  if (NULL != test_not_unboundp_out)
	    {
	      *test_not_unboundp_out = 1; 
	    }
	}
    }
  else if (!NILP (test_not))
    {
      invalid_argument_2 ("conflicting :test and :test-not keyword arguments",
			  test, test_not);
    }

  test = indirect_function (test, 1);

  if (NILP (key) || 
      EQ (indirect_function (key, 1), XSYMBOL_FUNCTION (Qidentity)))
    {
      key = Qidentity;
    }

  if (force_if)
    {
      result = EQ (key, Qidentity) ? check_if_nokey : check_if_key;

      if (NULL != test_func_out)
	{
	  *test_func_out = result;
	}

      return result;
    }

  if (!UNBOUNDP (item) && EQ (test, XSYMBOL_FUNCTION (Qeql)))
    {
      test = XSYMBOL_FUNCTION (NON_FIXNUM_NUMBER_P (item) ? Qequal : Qeq);
    }

#define FROB(known_test, eq_condition)				\
  if (EQ (test, XSYMBOL_FUNCTION (Q##known_test))) do		\
    {								\
      if (eq_condition)						\
	{							\
	  test = XSYMBOL_FUNCTION (Qeq);			\
	  goto force_eq_check;					\
	}							\
								\
      if (!EQ (Qidentity, key))					\
	{							\
	  test_func = check_##known_test##_key;			\
	  result = check_match_##known_test##_key;		\
	}							\
      else							\
	{							\
	  result = test_func = check_##known_test##_nokey;	\
	}							\
    } while (0)

  FROB (eql, 0);
  else if (SUBRP (test))
    {
    force_eq_check:
      FROB (eq, 0);
      else FROB (equal, (SYMBOLP (item) || FIXNUMP (item) || CHARP (item)));
      else FROB (equalp, (SYMBOLP (item)));
      else if (EQ (test, XSYMBOL_FUNCTION (Qstring_match)))
	{
	  if (EQ (Qidentity, key))
	    {
	      test_func = result = check_string_match_nokey;
	    }
	  else
	    {
	      test_func = check_string_match_key;
	      result = check_other_key;
	    }
	}
    }

  if (NULL == result)
    {
      if (EQ (Qidentity, key))
	{
	  test_func = result = check_other_nokey;
	}
      else
	{
	  test_func = check_other_key;
	  result = check_match_other_key;
	}
    }

  if (NULL != test_func_out)
    {
      *test_func_out = test_func;
    }

  return result;
}
#undef FROB

/* Given TEST, TEST_NOT, IF, IF_NOT, KEY, and ITEM, return a C function
   pointer appropriate for use in deciding whether a given element of a
   sequence satisfies TEST.

   Set *test_not_unboundp_out to 1 if TEST_NOT was not bound; set it to zero
   if it was bound, and set *test_inout to the value it was bound to. If
   TEST was not bound, leave *test_inout alone; the value is not used by
   check_eq_*key() or check_equal_*key(), which are the defaults, depending
   on the type of ITEM.

   The returned function takes arguments (TEST, KEY, ITEM, ELT), where ITEM
   is the item being searched for and ELT is the element of the sequence
   being examined.

   Error if both TEST and TEST_NOT were specified, which Common Lisp says is
   undefined behaviour. */

static check_test_func_t
get_check_test_function (Lisp_Object item,
			 Lisp_Object *test_inout, Lisp_Object test_not,
			 Lisp_Object if_, Lisp_Object if_not,
			 Lisp_Object key, Boolint *test_not_unboundp_out)
{
  check_test_func_t result = NULL;
  get_check_match_function_1 (item, test_inout, test_not, if_, if_not,
			      key, test_not_unboundp_out, &result);
  return result;
}

/* Given TEST, TEST_NOT, IF, IF_NOT and KEY, return a C function pointer
   appropriate for use in deciding whether two given elements of a sequence
   satisfy TEST.

   Set *test_not_unboundp_out to 1 if TEST_NOT was not bound; set it to zero
   if it was bound, and set *test_inout to the value it was bound to. If
   TEST was not bound, leave *test_inout alone; the value is not used by
   check_eql_*key().

   The returned function takes arguments (TEST, KEY, ELT1, ELT2), where ELT1
   and ELT2 are elements of the sequence being examined.

   The value that would be given by get_check_test_function() is returned in
   *TEST_FUNC_OUT, which allows calling functions to do their own key checks
   if they're processing one element at a time.

   Error if both TEST and TEST_NOT were specified, which Common Lisp says is
   undefined behaviour. */

static check_test_func_t
get_check_match_function (Lisp_Object *test_inout, Lisp_Object test_not,
			  Lisp_Object if_, Lisp_Object if_not,
			  Lisp_Object key, Boolint *test_not_unboundp_out,
			  check_test_func_t *test_func_out)
{
  return get_check_match_function_1 (Qunbound, test_inout, test_not,
				     if_, if_not, key,
				     test_not_unboundp_out, test_func_out);
}

/* Given PREDICATE and KEY, return a C function pointer appropriate for use
   in deciding whether one given element of a sequence is less than
   another. */

static check_test_func_t
get_merge_predicate (Lisp_Object predicate, Lisp_Object key)
{
  predicate = indirect_function (predicate, 1);

  if (NILP (key))
    {
      key = Qidentity;
    }
  else
    {
      key = indirect_function (key, 1);
      if (EQ (key, XSYMBOL_FUNCTION (Qidentity)))
	{
	  key = Qidentity;
	}
    }

  if (EQ (key, Qidentity) && EQ (predicate,
				 XSYMBOL_FUNCTION (Qcar_less_than_car)))
    {
      key = XSYMBOL_FUNCTION (Qcar);
      predicate = XSYMBOL_FUNCTION (Qlss);
    }

  if (EQ (predicate, XSYMBOL_FUNCTION (Qlss)))
    {
      if (EQ (key, Qidentity))
	{
	  return check_lss_nokey;
	}

      if (EQ (key, XSYMBOL_FUNCTION (Qcar)))
	{
	  return check_lss_key_car;
	}

      return check_lss_key;
    }

  if (EQ (predicate, XSYMBOL_FUNCTION (Qstring_lessp)))
    {
      if (EQ (key, Qidentity))
	{
	  return check_string_lessp_nokey;
	}

      if (EQ (key, XSYMBOL_FUNCTION (Qcar)))
	{
	  return check_string_lessp_key_car;
	}

      return check_string_lessp_key;
    }

  if (EQ (key, Qidentity))
    {
      return check_other_nokey;
    }

  return check_match_other_key;
}


static Lisp_Object string_count_from_end (Lisp_Object, Lisp_Object ,
                                          check_test_func_t, Boolint,
                                          Lisp_Object, Lisp_Object,
                                          Lisp_Object, Lisp_Object);

static Lisp_Object list_count_from_end (Lisp_Object, Lisp_Object,
                                        check_test_func_t, Boolint,
                                        Lisp_Object, Lisp_Object,
                                        Lisp_Object, Lisp_Object);

/* Count the number of occurrences of ITEM in SEQUENCE; if SEQUENCE is a
   list, store the cons cell of which the car is the last ITEM in SEQUENCE,
   at the address given by tail_out. */

static Lisp_Object
count_with_tail (Lisp_Object *tail_out, int nargs, Lisp_Object *args,
		 Lisp_Object caller)
{
  Lisp_Object item = args[0], sequence = args[1];
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, encountered = 0;
  Elemcount len, ii = 0, counting = MOST_POSITIVE_FIXNUM;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS_8 (caller, nargs, args, 9,
		    (test, key, start, end, from_end, test_not, count,
		     if_, if_not), (start = Qzero), 2, 0);

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (!NILP (count))
    {
      CHECK_INTEGER (count);
      counting = BIGNUMP (count) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (count);

      /* Our callers should have filtered out non-positive COUNT. */
      assert (counting >= 0);
      /* And we're not prepared to handle COUNT from any other caller at the
	 moment. */
      assert (EQ (caller, QremoveX)|| EQ (caller, QdeleteX));
    }

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  *tail_out = Qnil;

  if (CONSP (sequence))
    {
      if (EQ (caller, Qcount) && !NILP (from_end)
          && (!EQ (key, Qnil) ||
              check_test == check_other_nokey || check_test == check_if_nokey))
        {
          /* #'count, #'count-if, and #'count-if-not are documented to have
             a given traversal order if :from-end t is passed in, even
             though forward traversal of the sequence has the same result
             and is algorithmically less expensive for lists and strings.
             This order isn't necessary for other callers, though. */
          return list_count_from_end (item, sequence, check_test,
                                      test_not_unboundp, test, key,
                                      start, end);
        }

      /* If COUNT is non-nil and FROM-END is t, we can give the tail
         containing the last match, since that's what #'remove* is
         interested in (a zero or negative COUNT won't ever reach
         count_with_tail(), our callers will return immediately on seeing
         it). */
      if (!NILP (count) && !NILP (from_end))
        {
          counting = MOST_POSITIVE_FIXNUM;
        }

      {
	GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
          {
            if (!(ii < ending))
              {
                break;
              }

            if (starting <= ii &&
                check_test (test, key, item, elt) == test_not_unboundp)
              {
                encountered++;
                *tail_out = tail;

                if (encountered == counting)
                  {
                    break;
                  }
              }

            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      if ((ii < starting || (ii < ending && !NILP (end))) &&
          encountered != counting)
        {
          check_sequence_range (args[1], start, end, Flength (args[1]));
        }
    }
  else if (STRINGP (sequence))
    {
      Ibyte *startp = XSTRING_DATA (sequence), *cursor = startp;
      Bytecount byte_len = XSTRING_LENGTH (sequence), cursor_offset = 0;
      Lisp_Object character = Qnil;

      if (EQ (caller, Qcount) && !NILP (from_end)
          && (!EQ (key, Qnil) ||
              check_test == check_other_nokey || check_test == check_if_nokey))
        {
          /* See comment above in the list code. */
          return string_count_from_end (item, sequence,
                                        check_test, test_not_unboundp,
                                        test, key, start, end);
        }

      while (cursor_offset < byte_len && ii < ending && encountered < counting)
        {
          if (ii >= starting)
            {
              character = make_char (itext_ichar (cursor));
              
              if (check_test (test, key, item, character)
                  == test_not_unboundp)
                {
                  encountered++;
                }

              startp = XSTRING_DATA (sequence);
              cursor = startp + cursor_offset;
              if (byte_len != XSTRING_LENGTH (sequence)
                  || !valid_ibyteptr_p (cursor))
                {
                  mapping_interaction_error (caller, sequence);
                }
            }

          INC_IBYTEPTR (cursor);
          cursor_offset = cursor - startp;
          ii++;
        }

      if (ii < starting || (ii < ending && !NILP (end)))
        {
          check_sequence_range (sequence, start, end, Flength (sequence));
        }
    }
  else
    {
      Lisp_Object object = Qnil;

      len = XFIXNUM (Flength (sequence));
      check_sequence_range (sequence, start, end, make_fixnum (len));

      ending = min (ending, len);
      if (0 == len)
	{
	  /* Catches the case where we have nil.  */
	  return make_integer (encountered);
	}

      if (NILP (from_end))
	{
	  for (ii = starting; ii < ending && encountered < counting; ii++)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (check_test (test, key, item, object) == test_not_unboundp)
		{
		  encountered++;
		}
	    }
	}
      else
	{
	  for (ii = ending - 1; ii >= starting && encountered < counting; ii--)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (check_test (test, key, item, object) == test_not_unboundp)
		{
		  encountered++;
		}
	    }
	}
    }

  return make_integer (encountered);
}

static Lisp_Object
list_count_from_end (Lisp_Object item, Lisp_Object sequence,
                     check_test_func_t check_test, Boolint test_not_unboundp,
                     Lisp_Object test, Lisp_Object key,
                     Lisp_Object start, Lisp_Object end)
{
  Elemcount length = XFIXNUM (Flength (sequence)), ii = 0, starting = XFIXNUM (start);
  Elemcount ending = NILP (end) ? length : XFIXNUM (end), encountered = 0;
  Lisp_Object *storage;
  struct gcpro gcpro1;

  check_sequence_range (sequence, start, end, make_integer (length));

  storage = alloca_array (Lisp_Object, ending - starting);

  {
    EXTERNAL_LIST_LOOP_2 (elt, sequence)
      {
        if (starting <= ii && ii < ending)
          {
            storage[ii - starting] = elt;
          }
        ii++;
      }
  }

  GCPRO1 (storage[0]);
  gcpro1.nvars = ending - starting;

  for (ii = ending - 1; ii >= starting; ii--)
    {
      if (check_test (test, key, item, storage[ii - starting])
          == test_not_unboundp)
        {
          encountered++;
        }
    }

  UNGCPRO;

  return make_integer (encountered);
}

static Lisp_Object
string_count_from_end (Lisp_Object item, Lisp_Object sequence,
                       check_test_func_t check_test, Boolint test_not_unboundp,
                       Lisp_Object test, Lisp_Object key,
                       Lisp_Object start, Lisp_Object end)
{
  Elemcount length = string_char_length (sequence), ii = 0;
  Elemcount starting = XFIXNUM (start), ending = NILP (end) ? length : XFIXNUM (end);
  Elemcount encountered = 0;
  Ibyte *cursor = XSTRING_DATA (sequence);
  Ibyte *endp = cursor + XSTRING_LENGTH (sequence);
  Ichar *storage;

  check_sequence_range (sequence, start, end, make_integer (length));

  storage = alloca_array (Ichar, ending - starting);

  while (cursor < endp && ii < ending)
    {
      if (starting <= ii && ii < ending)
        {
          storage [ii - starting] = itext_ichar (cursor);
        }

      ii++;
      INC_IBYTEPTR (cursor);
    }

  for (ii = ending - 1; ii >= starting; ii--)
    {
      if (check_test (test, key, item, make_char (storage [ii - starting]))
          == test_not_unboundp)
        {
          encountered++;
        }
    }

  return make_integer (encountered);
}

DEFUN ("count", Fcount, 2, MANY, 0, /*
Count the number of occurrences of ITEM in SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (ITEM SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) END FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object tail = Qnil;

  /* count_with_tail() accepts more keywords than we do, check those we've
     been given. */
  PARSE_KEYWORDS (Fcount, nargs, args, 8,
		  (test, test_not, if_, if_not, key, start, end, from_end),
		  NULL);

  return count_with_tail (&tail, nargs, args, Qcount);
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
  Elemcount len, ss, ee = MOST_POSITIVE_FIXNUM, ii;
  Lisp_Object result = Qnil;

  CHECK_SEQUENCE (sequence);
  CHECK_FIXNUM (start);
  ss = XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_FIXNUM (end);
      ee = XFIXNUM (end);
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
          len = XFIXNUM (Flength (sequence));
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
          sequence = Fnthcdr (make_fixnum (ss), sequence);
        }

      ii = ss + 1;

      if (ss < ee && !NILP (sequence))
        {
	  result = result_tail = Fcons (Fcar (sequence), Qnil);
	  sequence = Fcdr (sequence);

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
      len = XFIXNUM (Flength (sequence));
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

      check_sequence_range (sequence, start, end, make_fixnum (len));

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

DEFUN ("elt", Felt, 2, 2, 0, /*
Return element of SEQUENCE at index N.
*/
       (sequence, n))
{
  /* This function can GC */
 retry:
  CHECK_FIXNUM_COERCE_CHAR (n); /* yuck! */
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
  else
    {
      check_losing_bytecode ("elt", sequence);
      sequence = wrong_type_argument (Qsequencep, sequence);
      goto retry;
    }
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
  if (depth + lisp_eval_depth > max_lisp_eval_depth)
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

/* Return the first index of ITEM in LIST. In CONS_OUT, return the cons cell
   before that containing the element. If the element is in the first cons
   cell, return Qnil in CONS_OUT.  TEST, KEY, START, END are as in
   #'remove*; CHECK_TEST and TEST_NOT_UNBOUNDP should have been initialized
   with get_check_match_function() or get_check_test_function().  A non-zero
   REVERSE_TEST_ORDER means call TEST with the element from LIST as its
   first argument and ITEM as its second. Error if LIST is ill-formed, or
   circular. */
static Lisp_Object
list_position_cons_before (Lisp_Object *cons_out,
                           Lisp_Object item, Lisp_Object list,
                           check_test_func_t check_test,
                           Boolint test_not_unboundp,
                           Lisp_Object test, Lisp_Object key,
                           Boolint reverse_test_order,
                           Lisp_Object start, Lisp_Object end)
{
  struct gcpro gcpro1;
  Lisp_Object tail_before = Qnil;
  Elemcount ii = 0, starting = XFIXNUM (start);
  Elemcount ending = NILP (end) ? MOST_POSITIVE_FIXNUM : XFIXNUM (end);

  GCPRO1 (tail_before);

  if (check_test == check_eq_nokey)
    {
      /* TEST is #'eq, no need to call any C functions, and the test order
         won't be visible. */
      EXTERNAL_LIST_LOOP_3 (elt, list, tail)
	{
          if (starting <= ii && ii < ending &&
              EQ (item, elt) == test_not_unboundp)
            {
              *cons_out = tail_before;
              RETURN_UNGCPRO (make_integer (ii));
            }
          else
            {
              if (ii >= ending)
                {
                  break;
                }
            }
          ii++;
          tail_before = tail;
	}
    }
  else
    {
      GC_EXTERNAL_LIST_LOOP_3 (elt, list, tail)
        {
          if (starting <= ii && ii < ending &&
              (reverse_test_order ? 
               check_test (test, key, elt, item) :
               check_test (test, key, item, elt)) == test_not_unboundp)
            {
              *cons_out = tail_before;
	      XUNGCPRO (elt);
	      UNGCPRO;
	      return make_integer (ii);
            }
          else
            {
              if (ii >= ending)
                {
                  break;
                }
            }
          ii++;
          tail_before = tail;
        }
      END_GC_EXTERNAL_LIST_LOOP (elt);
    }

  RETURN_UNGCPRO (Qnil);
}

DEFUN ("member*", FmemberX, 2, MANY, 0, /*
Return the first sublist of LIST with car ITEM, or nil if no such sublist.

The keyword :test specifies a two-argument function that is used to compare
ITEM with elements in LIST; if omitted, it defaults to `eql'.

The keyword :test-not is similar, but specifies a negated function.  That
is, ITEM is considered equal to an element in LIST if the given function
returns nil.  Common Lisp deprecates :test-not, and if both are specified,
XEmacs signals an error.

:key specifies a one-argument function that transforms elements of LIST into
\"comparison keys\" before the test predicate is applied.  For example,
if :key is #'car, then ITEM is compared with the car of elements from LIST.
The :key function, however, is not applied to ITEM, and does not affect the
elements in the returned list, which are taken directly from the elements in
LIST.

arguments: (ITEM LIST &key (TEST #'eql) TEST-NOT (KEY #'identity))
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], list = args[1], result = Qnil, position0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (FmemberX, nargs, args, 5, (test, if_not, if_, test_not, key),
		  NULL);
  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);
  position0
    = list_position_cons_before (&result, item, list, check_test,
                                 test_not_unboundp, test, key, 0, Qzero, Qnil);

  return CONSP (result) ? XCDR (result) : ZEROP (position0) ? list : Qnil;
}

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
          if (EQ (key, XSYMBOL_FUNCTION (Qidentity)))                   \
            {                                                           \
              key = Qidentity;                                          \
            }                                                           \
        }                                                               \
    } while (0)

#define KEY(key, item) (EQ (Qidentity, key) ? item : \
                        IGNORE_MULTIPLE_VALUES (call1 (key, item)))

DEFUN ("adjoin", Fadjoin, 2, MANY, 0, /*
Return ITEM consed onto the front of LIST, if not already in LIST.

Otherwise, return LIST unmodified.

See `member*' for the meaning of the keywords.

arguments: (ITEM LIST &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], list = args[1], keyed = Qnil, ignore = Qnil;
  struct gcpro gcpro1;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Fadjoin, nargs, args, 3, (test, key, test_not),
		  NULL);

  CHECK_KEY_ARGUMENT (key);

  keyed = KEY (key, item);

  GCPRO1 (keyed);
  check_test = get_check_test_function (keyed, &test, test_not, Qnil, Qnil,
					key, &test_not_unboundp);
  if (NILP (list_position_cons_before (&ignore, keyed, list, check_test,
                                       test_not_unboundp, test, key, 0, Qzero,
                                       Qnil)))
    {
      RETURN_UNGCPRO (Fcons (item, list));
    }

  RETURN_UNGCPRO (list);
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

DEFUN ("assoc*", FassocX, 2, MANY, 0, /*
Find the first item whose car matches ITEM in ALIST.

See `member*' for the meaning of :test, :test-not and :key.

arguments: (ITEM ALIST &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], alist = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (FassocX, nargs, args, 5, (test, if_, if_not, test_not, key),
		  NULL);

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  if (check_test == check_eq_nokey)
    {
      /* TEST is #'eq, no need to call any C functions. */
      EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
	{
	  if (EQ (item, elt_car) == test_not_unboundp)
	    {
	      return elt;
	    }
	}
    }
  else
    {
      GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
	{
	  if (CONSP (elt) && 
	      check_test (test, key, item, XCAR (elt)) == test_not_unboundp)
              {
		XUNGCPRO (elt);
		return elt;
              }
	}
      END_GC_EXTERNAL_LIST_LOOP (elt);
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

DEFUN ("rassoc*", FrassocX, 2, MANY, 0, /*
Find the first item whose cdr matches ITEM in ALIST.

See `member*' for the meaning of :test, :test-not and :key.

arguments: (ITEM ALIST &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], alist = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (FrassocX, nargs, args, 5, (test, if_, if_not, test_not, key),
		  NULL);

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  if (check_test == check_eq_nokey)
    {
      /* TEST is #'eq, no need to call any C functions. */
      EXTERNAL_ALIST_LOOP_4 (elt, elt_car, elt_cdr, alist)
	{
	  if (EQ (item, elt_cdr) == test_not_unboundp)
	    {
	      return elt;
	    }
	}
    }
  else
    {
      GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
	{
	  if (CONSP (elt) &&
	      check_test (test, key, item, XCDR (elt)) == test_not_unboundp)
	    {
	      XUNGCPRO (elt);
	      return elt;
	    }
	}
      END_GC_EXTERNAL_LIST_LOOP (elt);
    }
		  
  return Qnil;
}

/* This is the implementation of both #'find and #'position. */
static Lisp_Object
position (Lisp_Object *object_out, Lisp_Object item, Lisp_Object sequence,
          check_test_func_t check_test, Boolint test_not_unboundp,
          Lisp_Object test, Lisp_Object key, Lisp_Object start, Lisp_Object end,
          Lisp_Object from_end, Lisp_Object default_, Lisp_Object caller)
{
  Lisp_Object result = Qnil;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, len, ii = 0;

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = FIXNUMP (start) ? XFIXNUM (start) : 1 + MOST_POSITIVE_FIXNUM;

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = FIXNUMP (end) ? XFIXNUM (end) : 1 + MOST_POSITIVE_FIXNUM;
    }

  *object_out = default_;

  if (CONSP (sequence))
    {
      if (!(starting < ending))
	{
	  check_sequence_range (sequence, start, end, Flength (sequence));
	  /* starting could be equal to ending, in which case nil is what
	     we want to return. */
	  return Qnil;
	}

      {
	GC_EXTERNAL_LIST_LOOP_2 (elt, sequence)
          {
            if (starting <= ii && ii < ending
                && check_test (test, key, item, elt) == test_not_unboundp)
              {
                result = make_integer (ii);
                *object_out = elt;

                if (NILP (from_end))
                  {
		    XUNGCPRO (elt);
                    return result;
                  }
              }
            else if (ii == ending)
              {
                break;
              }
            
            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      if (ii < starting || (ii < ending && !NILP (end)))
	{
	  check_sequence_range (sequence, start, end, Flength (sequence));
	}
    }
  else if (STRINGP (sequence))
    {
      Ibyte *startp = XSTRING_DATA (sequence), *cursor = startp;
      Bytecount byte_len = XSTRING_LENGTH (sequence), cursor_offset = 0;
      Lisp_Object character = Qnil;

      while (cursor_offset < byte_len && ii < ending)
	{
	  if (ii >= starting)
	    {
	      character = make_char (itext_ichar (cursor));

	      if (check_test (test, key, item, character) == test_not_unboundp)
		{
		  result = make_integer (ii);
		  *object_out = character;

		  if (NILP (from_end))
		    {
		      return result;
		    }
		}

	      startp = XSTRING_DATA (sequence);
	      cursor = startp + cursor_offset;
	      if (byte_len != XSTRING_LENGTH (sequence)
		  || !valid_ibyteptr_p (cursor))
		{
		  mapping_interaction_error (caller, sequence);
		}
	    }

	  INC_IBYTEPTR (cursor);
	  cursor_offset = cursor - startp;
	  ii++;
	}

      if (ii < starting || (ii < ending && !NILP (end)))
	{
	  check_sequence_range (sequence, start, end, Flength (sequence));
	}
    }
  else
    {
      Lisp_Object object = Qnil;
      len = XFIXNUM (Flength (sequence));
      check_sequence_range (sequence, start, end, make_fixnum (len));

      ending = min (ending, len);
      if (0 == len)
	{
	  /* Catches the case where we have nil.  */
	  return result;
	}

      if (NILP (from_end))
	{
	  for (ii = starting; ii < ending; ii++)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (check_test (test, key, item, object) == test_not_unboundp)
		{
		  result = make_integer (ii);
		  *object_out = object;
		  return result;
		}
	    }
	}
      else
	{
	  for (ii = ending - 1; ii >= starting; ii--)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (check_test (test, key, item, object) == test_not_unboundp)
		{
		  result = make_integer (ii);
		  *object_out = object;
		  return result;
		}
	    }
	}
    }

  return result;
}

DEFUN ("position", Fposition, 2, MANY, 0, /*
Return the index of the first occurrence of ITEM in SEQUENCE.

Return nil if not found. See `remove*' for the meaning of the keywords.

arguments: (ITEM SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object object = Qnil, item = args[0], sequence = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Fposition, nargs, args, 8,
		  (test, if_, test_not, if_not, key, start, end, from_end),
		  (start = Qzero));

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  return position (&object, item, sequence, check_test, test_not_unboundp,
                   test, key, start, end, from_end, Qnil, Qposition);
}

DEFUN ("find", Ffind, 2, MANY, 0, /*
Find the first occurrence of ITEM in SEQUENCE.

Return the matching ITEM, or nil if not found.  See `remove*' for the
meaning of the keywords.

The keyword :default, not specified by Common Lisp, designates an object to
return instead of nil if ITEM is not found.

arguments: (ITEM SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) DEFAULT FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object object = Qnil, item = args[0], sequence = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Ffind, nargs, args, 9,
		  (test, if_, test_not, if_not, key, start, end, from_end,
                   default_),
		  (start = Qzero));

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  position (&object, item, sequence, check_test, test_not_unboundp,
            test, key, start, end, from_end, default_, Qposition);

  return object;
}

/* Like #'delq, but caller must ensure that LIST is properly
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

DEFUN ("delete*", FdeleteX, 2, MANY, 0, /*
Remove all occurrences of ITEM in SEQUENCE, destructively.

If SEQUENCE is a non-nil list, this modifies the list directly.  A non-list
SEQUENCE will not be destructively modified, rather, if ITEM occurs in it, a
new SEQUENCE of the same type without ITEM will be returned.

See `remove*' for a non-destructive alternative, and for explanation of the
keyword arguments.

arguments: (ITEM SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) FROM-END COUNT TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], sequence = args[1];
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, counting = MOST_POSITIVE_FIXNUM;
  Elemcount len, ii = 0, encountered = 0, presenting = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (FdeleteX, nargs, args, 9,
		  (test, if_not, if_, test_not, key, start, end, from_end,
		   count), (start = Qzero, count = Qunbound));

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (!UNBOUNDP (count))
    {
      if (!NILP (count))
	{
	  CHECK_INTEGER (count);
          if (FIXNUMP (count))
            {
              counting = XFIXNUM (count);
            }
#ifdef HAVE_BIGNUM
          else
            {
              counting = bignum_sign (XBIGNUM_DATA (count)) > 0 ?
                1 + MOST_POSITIVE_FIXNUM : MOST_NEGATIVE_FIXNUM - 1;
            }
#endif

	  if (counting < 1)
	    {
	      return sequence;
	    }

          if (!NILP (from_end))
            {
              /* Sigh, this is inelegant. Force count_with_tail () to ignore
                 the count keyword, so we get the actual number of matching
                 elements, and can start removing from the beginning for the
                 from-end case.  */
              for (ii = XSUBR (GET_DEFUN_LISP_OBJECT (FdeleteX))->min_args;
                   ii < nargs; ii += 2)
                {
                  if (EQ (args[ii], Q_count))
                    {
                      args[ii + 1] = Qnil;
                      break;
                    }
                }
              ii = 0;
            }
        }
    }

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  if (CONSP (sequence))
    {
      Lisp_Object prev_tail_list_elt = Qnil, ignore = Qnil;
      Elemcount list_len = 0, deleted = 0;
      struct gcpro gcpro1;

      if (!NILP (count) && !NILP (from_end))
	{
	  /* Both COUNT and FROM-END were specified; we need to traverse the
	     list twice. */
	  Lisp_Object present = count_with_tail (&ignore, nargs, args,
						 QdeleteX);

	  if (ZEROP (present))
	    {
	      return sequence;
	    }

	  presenting = XFIXNUM (present);

	  /* If there are fewer items in the list than we have permission to
	     delete, we don't need to differentiate between the :from-end
	     nil and :from-end t cases. Otherwise, presenting is the number
	     of matching items we need to ignore before we start to
	     delete. */
	  presenting = presenting <= counting ? 0 : presenting - counting;
	}

      GCPRO1 (prev_tail_list_elt);
      ii = -1;

      {
	GC_EXTERNAL_LIST_LOOP_4 (list_elt, sequence, tail, list_len)
          {
            ii++;

            if (starting <= ii && ii < ending &&
                (check_test (test, key, item, list_elt) == test_not_unboundp)
                && (presenting ? encountered++ >= presenting
                    : encountered++ < counting))
              {
                if (NILP (prev_tail_list_elt))
                  {
                    sequence = XCDR (tail);
                  }
                else
                  {
                    XSETCDR (prev_tail_list_elt, XCDR (tail));
                  }

                /* Keep tortoise from ever passing hare. */ 
                list_len = 0; 
                deleted++;
              }
            else
              {
                prev_tail_list_elt = tail;
                if (ii >= ending || (!presenting && encountered > counting))
                  {
                    break;
                  }
              }
          }
	END_GC_EXTERNAL_LIST_LOOP (list_elt);
      }

      UNGCPRO;

      if ((ii < starting || (ii < ending && !NILP (end))) &&
	  !(presenting ? encountered == presenting : encountered == counting)) 
	{
	  check_sequence_range (args[1], start, end,
                                make_fixnum (deleted + XFIXNUM (Flength (args[1]))));
	}

      return sequence;
    }
  else if (STRINGP (sequence))
    {
      Ibyte *staging = alloca_ibytes (XSTRING_LENGTH (sequence));
      Ibyte *staging_cursor = staging, *startp = XSTRING_DATA (sequence);
      Ibyte *cursor = startp;
      Bytecount cursor_offset = 0, byte_len = XSTRING_LENGTH (sequence);
      Lisp_Object character, result = sequence;

      if (!NILP (count) && !NILP (from_end))
	{
	  Lisp_Object present = count_with_tail (&character, nargs, args,
						 QdeleteX);

	  if (ZEROP (present))
	    {
	      return sequence;
	    }

	  presenting = XFIXNUM (present);

	  /* If there are fewer items in the list than we have permission to
	     delete, we don't need to differentiate between the :from-end
	     nil and :from-end t cases. Otherwise, presenting is the number
	     of matching items we need to ignore before we start to
	     delete. */
	  presenting = presenting <= counting ? 0 : presenting - counting;
	}

      ii = 0;
      while (cursor_offset < byte_len)
	{
	  if (ii >= starting && ii < ending)
	    {
	      character = make_char (itext_ichar (cursor));

	      if ((check_test (test, key, item, character)
		   == test_not_unboundp)
		  && (presenting ? encountered++ >= presenting :
		      encountered++ < counting))
		{
		  DO_NOTHING;
		}
	      else
		{
		  staging_cursor
		    += set_itext_ichar (staging_cursor, XCHAR (character));
		}

	      startp = XSTRING_DATA (sequence);
	      cursor = startp + cursor_offset;
	      if (byte_len != XSTRING_LENGTH (sequence)
		  || !valid_ibyteptr_p (cursor))
		{
		  mapping_interaction_error (QdeleteX, sequence);
		}
	    }
	  else
	    {
	      staging_cursor += itext_copy_ichar (cursor, staging_cursor);
	    }

	  INC_IBYTEPTR (cursor);
	  cursor_offset = cursor - startp;
	  ii++;
	}

      if (ii < starting || (ii < ending && !NILP (end)))
	{
	  check_sequence_range (sequence, start, end, Flength (sequence));
	}

      if (0 != encountered)
	{
	  result = make_string (staging, staging_cursor - staging);
	  copy_string_extents (result, sequence, 0, 0,
			       staging_cursor - staging);
	  sequence = result;
	}

      return sequence;
    }
  else
    {
      Lisp_Object position0 = Qnil, object = Qnil;
      Lisp_Object *staging = NULL, *staging_cursor, *staging_limit;
      Elemcount positioning;

      len = XFIXNUM (Flength (sequence));

      check_sequence_range (sequence, start, end, make_fixnum (len));

      position0 = position (&object, item, sequence, check_test,
                            test_not_unboundp, test, key, start, end,
                            from_end, Qnil, QdeleteX);
      if (NILP (position0))
	{
	  return sequence;
	}

      ending = min (ending, len);
      positioning = XFIXNUM (position0);
      encountered = 1;

      if (NILP (from_end))
	{
	  staging = alloca_array (Lisp_Object, len - 1);
	  staging_cursor = staging;

	  ii = 0;
	  while (ii < positioning)
	    {
	      *staging_cursor++ = Faref (sequence, make_fixnum (ii));
	      ii++;
	    }

	  ii = positioning + 1;
	  while (ii < ending)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (encountered < counting
		  && (check_test (test, key, item, object)
		      == test_not_unboundp))
		{
		  encountered++;
		}
	      else
		{
		  *staging_cursor++ = object;
		}
	      ii++;
	    }

	  while (ii < len)
	    {
	      *staging_cursor++ = Faref (sequence, make_fixnum (ii));
	      ii++;
	    }
	}
      else
	{
	  staging = alloca_array (Lisp_Object, len - 1);
	  staging_cursor = staging_limit = staging + len - 1;

	  ii = len - 1;
	  while (ii > positioning)
	    {
	      *--staging_cursor = Faref (sequence, make_fixnum (ii));
	      ii--;
	    }

	  ii = positioning - 1;
	  while (ii >= starting)
	    {
	      object = Faref (sequence, make_fixnum (ii));
	      if (encountered < counting
		  && (check_test (test, key, item, object) ==
		      test_not_unboundp))
		{
		  encountered++;
		}
	      else
		{
		  *--staging_cursor = object;
		}

	      ii--;
	    }

	  while (ii >= 0)
	    {
	      *--staging_cursor = Faref (sequence, make_fixnum (ii));
	      ii--;
	    }

	  staging = staging_cursor;
	  staging_cursor = staging_limit;
	}

      if (VECTORP (sequence))
	{
	  return Fvector (staging_cursor - staging, staging);
	}
      else if (BIT_VECTORP (sequence))
	{
	  return Fbit_vector (staging_cursor - staging, staging);
	}

      /* A nil sequence will have given us a nil #'position,
	 above.  */
      ABORT (); 

      return Qnil;
    }
}

DEFUN ("remove*", FremoveX, 2, MANY, 0, /*
Remove all occurrences of ITEM in SEQUENCE, non-destructively.

If SEQUENCE is a list, `remove*' makes a copy if that is necessary to avoid
corrupting the original SEQUENCE.

The keywords :test and :test-not specify two-argument test and negated-test
predicates, respectively; :test defaults to `eql'.  :key specifies a
one-argument function that transforms elements of SEQUENCE into \"comparison
keys\" before the test predicate is applied.  See `member*' for more
information on these keywords.

:start and :end, if given, specify indices of a subsequence of SEQUENCE to
be processed.  Indices are 0-based and processing involves the subsequence
starting at the index given by :start and ending just before the index given
by :end.

:count, if given, limits the number of items removed to the number
specified.  :from-end, if given, causes processing to proceed starting from
the end instead of the beginning; in this case, this matters only if :count
is given.

arguments: (ITEM SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) FROM-END COUNT TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object item = args[0], sequence = args[1], matched_count = Qnil,
    tail = Qnil;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, counting = MOST_POSITIVE_FIXNUM;
  Elemcount ii = 0, encountered = 0, presenting = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (FremoveX, nargs, args, 9,
		  (test, if_not, if_, test_not, key, start, end, from_end,
		   count), (start = Qzero));

  if (!CONSP (sequence))
    {
      return FdeleteX (nargs, args);
    }

  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (!NILP (count))
    {
      CHECK_INTEGER (count);
      if (FIXNUMP (count))
        {
          counting = XFIXNUM (count);
        }
#ifdef HAVE_BIGNUM
      else
        {
          counting = bignum_sign (XBIGNUM_DATA (count)) > 0 ?
            1 + MOST_POSITIVE_FIXNUM : -1 + MOST_NEGATIVE_FIXNUM;
        }
#endif

      if (counting <= 0)
	{
	  return sequence;
	}

      if (!NILP (from_end))
        {
	  /* Sigh, this is inelegant. Force count_with_tail () to ignore the
	     count keyword, so we get the actual number of matching
	     elements, and can start removing from the beginning for the
	     from-end case.  */
          for (ii = XSUBR (GET_DEFUN_LISP_OBJECT (FremoveX))->min_args;
               ii < nargs; ii += 2)
            {
              if (EQ (args[ii], Q_count))
                {
                  args[ii + 1] = Qnil;
                  break;
                }
            }
          ii = 0;
        }
    }

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  matched_count = count_with_tail (&tail, nargs, args, QremoveX);

  if (!ZEROP (matched_count))
    {
      Lisp_Object result = Qnil, result_tail = Qnil;
      struct gcpro gcpro1, gcpro2;

      if (!NILP (count) && !NILP (from_end))
	{
	  presenting = XFIXNUM (matched_count);

	  /* If there are fewer matching elements in the list than we have
	     permission to delete, we don't need to differentiate between
	     the :from-end nil and :from-end t cases. Otherwise, presenting
	     is the number of matching items we need to ignore before we
	     start to delete. */
	  presenting = presenting <= counting ? 0 : presenting - counting;
	}

      GCPRO2 (result, tail);
      {
	GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tailing)
          {
            if (EQ (tail, tailing))
              {
		XUNGCPRO (elt);
		UNGCPRO;

                if (NILP (result))
                  {
                    return XCDR (tail);
                  }

                XSETCDR (result_tail, XCDR (tail));
		return result;
              }
            else if (starting <= ii && ii < ending &&
                     (check_test (test, key, item, elt) == test_not_unboundp)
                     && (presenting ? encountered++ >= presenting
                         : encountered++ < counting))
              {
                DO_NOTHING;
              }
            else if (NILP (result))
              {
                result = result_tail = Fcons (elt, Qnil);
              }
            else
              {
                XSETCDR (result_tail, Fcons (elt, Qnil));
                result_tail = XCDR (result_tail);
              }

            if (ii == ending)
              {
                break;
              }

            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt); 
      }
      UNGCPRO;

      if (ii < starting || (ii < ending && !NILP (end)))
	{
	  check_sequence_range (args[0], start, end, Flength (args[0]));
	}

      return result;
    }

  return sequence;
}

Lisp_Object
remassoc_no_quit (Lisp_Object key, Lisp_Object alist)
{
  LIST_LOOP_DELETE_IF (elt, alist,
		       (CONSP (elt) &&
                        internal_equal (key, XCAR (elt), 0)));
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

/* Like Fremrassq, fast and unsafe; be careful */
Lisp_Object
remrassq_no_quit (Lisp_Object value, Lisp_Object alist)
{
  LIST_LOOP_DELETE_IF (elt, alist,
		       (CONSP (elt) &&
			EQ_WITH_EBOLA_NOTICE (value, XCDR (elt))));
  return alist;
}

/* Remove duplicate elements between START and END from LIST, a non-nil
   list; if COPY is zero, do so destructively. Items to delete are selected
   according to the algorithm used when :from-end t is passed to
   #'delete-duplicates.  Error if LIST is ill-formed or circular.

   TEST and KEY are as in #'remove*; CHECK_TEST and TEST_NOT_UNBOUNDP should
   reflect them, having been initialised with get_check_match_function() or
   get_check_test_function(). */
static Lisp_Object
list_delete_duplicates_from_end (Lisp_Object list,
				 check_test_func_t check_test,
				 Boolint test_not_unboundp,
				 Lisp_Object test, Lisp_Object key,
				 Lisp_Object start,
				 Lisp_Object end, Boolint copy)
{
  Lisp_Object checking = Qnil, result = list;
  Lisp_Object keyed, positioned, position_cons = Qnil, result_tail;
  Elemcount len = XFIXNUM (Flength (list)), pos, starting = XFIXNUM (start);
  Elemcount ending = (NILP (end) ? len : XFIXNUM (end)), greatest_pos_seen = -1;
  Elemcount ii = 0;
  struct gcpro gcpro1;

  /* We can't delete (or remove) as we go, because that breaks START and
     END.  We could if END were nil, and that would change an ON(N + 2)
     algorithm to an ON^2 algorithm. Here and now it doesn't matter, though,
     #'delete-duplicates is relatively expensive no matter what. */
  struct Lisp_Bit_Vector *deleting
    = (Lisp_Bit_Vector *) ALLOCA (sizeof (struct Lisp_Bit_Vector)
				  + (sizeof (long)
				     * (BIT_VECTOR_LONG_STORAGE (len)
					- 1)));

  check_sequence_range (list, start, end, make_integer (len));

  deleting->size = len;
  memset (&(deleting->bits), 0,
	  sizeof (long) * BIT_VECTOR_LONG_STORAGE (len));

  GCPRO1 (keyed);

  {
    GC_EXTERNAL_LIST_LOOP_3 (elt, list, tail)
      {
        if (!(starting <= ii && ii <= ending) || bit_vector_bit (deleting, ii))
          {
            ii++;
            continue;
          }

        keyed = KEY (key, elt);
        checking = XCDR (tail);
        pos = ii + 1;

        while (!NILP ((positioned = list_position_cons_before
                       (&position_cons, keyed, checking, check_test,
                        test_not_unboundp, test, key, 0,
                        make_fixnum (max (starting - pos, 0)),
                        make_fixnum (ending - pos)))))
          {
            pos = XFIXNUM (positioned) + pos;
            set_bit_vector_bit (deleting, pos, 1);
            greatest_pos_seen = max (greatest_pos_seen, pos);
            checking = NILP (position_cons) ?
              XCDR (checking) : XCDR (XCDR (position_cons));
            pos += 1;
          }
        ii++;
      }
    END_GC_EXTERNAL_LIST_LOOP (elt); 
  }

  UNGCPRO;

  ii = 0;

  if (greatest_pos_seen > -1)
    {
      if (copy)
	{
	  result = result_tail = Fcons (XCAR (list), Qnil);
	  list = XCDR (list);
	  ii = 1;

	  {
            EXTERNAL_LIST_LOOP_3 (elt, list, tail)
	      {
		if (ii == greatest_pos_seen)
		  {
		    XSETCDR (result_tail, XCDR (tail));
		    break;
		  }
		else if (!bit_vector_bit (deleting, ii))
		  {
		    XSETCDR (result_tail, Fcons (elt, Qnil));
		    result_tail = XCDR (result_tail);
		  }
		ii++;
	      }
	  }
	}
      else
	{
	  EXTERNAL_LIST_LOOP_DELETE_IF (elt, list,
					bit_vector_bit (deleting, ii++));
	}
    }

  return result;
}

DEFUN ("delete-duplicates", Fdelete_duplicates, 1, MANY, 0, /*
Remove all duplicate elements from SEQUENCE, destructively.

If SEQUENCE is a list and has duplicates, modify and return it.  Note that
SEQUENCE may start with an element to be deleted; because of this, if
modifying a variable, be sure to write `(setq VARIABLE (delete-duplicates
VARIABLE))' to be certain to have a list without duplicate elements.

If SEQUENCE is an array and has duplicates, return a newly-allocated array
of the same type comprising all unique elements of SEQUENCE.

If there are no duplicate elements in SEQUENCE, return it unmodified.

See `remove*' for the meaning of the keywords.  See `remove-duplicates' for
a non-destructive version of this function.

arguments: (SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) END FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence = args[0], keyed = Qnil;
  Lisp_Object positioned = Qnil, ignore = Qnil;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, len, ii = 0, jj = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Fdelete_duplicates, nargs, args, 6,
		  (test, key, test_not, start, end, from_end),
		  (start = Qzero));

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  CHECK_KEY_ARGUMENT (key);

  get_check_match_function (&test, test_not, Qnil, Qnil, key,
			    &test_not_unboundp, &check_test);

  if (CONSP (sequence))
    {
      if (NILP (from_end))
	{
	  Lisp_Object prev_tail = Qnil;
          Elemcount deleted = 0;

	  GCPRO2 (keyed, prev_tail);

          {
	    GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
              {
                if (starting <= ii && ii < ending)
                  {
                    keyed = KEY (key, elt);
                    positioned
                      = list_position_cons_before (&ignore, keyed,
                                                   XCDR (tail), check_test,
                                                   test_not_unboundp, test, key,
                                                   0, make_fixnum (max (starting
                                                                     - (ii + 1),
                                                                     0)),
                                                   make_fixnum (ending
                                                             - (ii + 1)));
                    if (!NILP (positioned))
                      {
                        sequence = XCDR (tail);
                        deleted++;
                      }
                    else
                      {
                        break;
                      }
                  }
                else
                  {
                    break;
                  }

                ii++;
              }
	    END_GC_EXTERNAL_LIST_LOOP (elt);
          }
          {
	    GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
              {
                if (!(starting <= ii && ii <= ending))
                  {
                    prev_tail = tail;
                    ii++;
                    continue;
                  }

                keyed = KEY (key, elt);
                positioned
                  = list_position_cons_before (&ignore, keyed, XCDR (tail),
                                               check_test, test_not_unboundp,
                                               test, key, 0,
                                               make_fixnum (max (starting
                                                              - (ii + 1), 0)),
                                               make_fixnum (ending - (ii + 1)));
                if (!NILP (positioned))
                  {
                    /* We know this isn't the first iteration of the loop,
                       because we advanced above to the point where we have at
                       least one non-duplicate entry at the head of the
                       list. */
                    XSETCDR (prev_tail, XCDR (tail));
                    len = 0;
                    deleted++;
                  }
                else
                  {
                    prev_tail = tail;
                    if (ii >= ending)
                      {
                        break;
                      }
                  }

                ii++;
              }
	    END_GC_EXTERNAL_LIST_LOOP (elt);
          }

	  UNGCPRO;

	  if ((ii < starting || (ii < ending && !NILP (end))))
	    {
	      check_sequence_range (args[0], start, end,
                                    make_fixnum (deleted
                                              + XFIXNUM (Flength (args[0]))));
	    }
	}
      else
	{
	  sequence = list_delete_duplicates_from_end (sequence, check_test,
						      test_not_unboundp, 
						      test, key, start, end,
						      0);
	}
    }
  else if (STRINGP (sequence))
    {
      Lisp_Object elt = Qnil;

      if (EQ (Qidentity, key))
	{
	  /* We know all the elements will be characters; set check_test to
	     reflect that. This isn't useful if KEY is not #'identity, since
	     it may return non-characters for the elements. */
	  check_test = get_check_test_function (make_char ('a'),
						&test, test_not,
						Qnil, Qnil, key,
						&test_not_unboundp);
	}

      if (NILP (from_end))
	{
	  Bytecount byte_len = XSTRING_LENGTH (sequence), cursor_offset = 0;
	  Ibyte *staging = alloca_ibytes (byte_len), *staging_cursor = staging;
	  Ibyte *cursor = XSTRING_DATA (sequence), *startp = cursor;
	  Elemcount deleted = 0;

	  GCPRO1 (elt);

	  while (cursor_offset < byte_len)
	    {
	      if (starting <= ii && ii < ending)
		{
		  Ibyte *cursor0 = cursor;
		  Bytecount cursor0_offset;
		  Boolint delete_this = 0;

		  elt = KEY (key, make_char (itext_ichar (cursor)));
		  INC_IBYTEPTR (cursor0);
		  cursor0_offset = cursor0 - startp;

		  for (jj = ii + 1; jj < ending && cursor0_offset < byte_len;
		       jj++)
		    {
		      if (check_test (test, key, elt,
				      make_char (itext_ichar (cursor0)))
			  == test_not_unboundp)
			{
			  delete_this = 1;
			  deleted++;
			  break;
			}

		      startp = XSTRING_DATA (sequence);
		      cursor0 = startp + cursor0_offset;
		      if (byte_len != XSTRING_LENGTH (sequence)
			  || !valid_ibyteptr_p (cursor0))
			{
			  mapping_interaction_error (Qdelete_duplicates,
						     sequence);
			}

		      INC_IBYTEPTR (cursor0);
		      cursor0_offset = cursor0 - startp;
		    }

		  startp = XSTRING_DATA (sequence);
		  cursor = startp + cursor_offset;

		  if (byte_len != XSTRING_LENGTH (sequence)
		      || !valid_ibyteptr_p (cursor))
		    {
		      mapping_interaction_error (Qdelete_duplicates, sequence);
		    }

		  if (!delete_this)
		    {
		      staging_cursor
			+= itext_copy_ichar (cursor, staging_cursor);
							 
		    }
		}
	      else
		{
		  staging_cursor += itext_copy_ichar (cursor, staging_cursor);
		}

	      INC_IBYTEPTR (cursor);
	      cursor_offset = cursor - startp;
	      ii++;
	    }

	  UNGCPRO;

	  if (ii < starting || (ii < ending && !NILP (end)))
	    {
	      check_sequence_range (sequence, start, end, Flength (sequence));
	    }

	  if (0 != deleted)
	    {
	      sequence = make_string (staging, staging_cursor - staging);
	    }
	}
      else
	{
	  Elemcount deleted = 0;
	  Ibyte *staging = alloca_ibytes ((len = string_char_length (sequence))
                                          * MAX_ICHAR_LEN);
	  Ibyte *staging_cursor = staging, *startp = XSTRING_DATA (sequence);
	  Ibyte *endp = startp + XSTRING_LENGTH (sequence);
	  struct Lisp_Bit_Vector *deleting
	    = (Lisp_Bit_Vector *) ALLOCA (sizeof (struct Lisp_Bit_Vector)
					  + (sizeof (long)
					     * (BIT_VECTOR_LONG_STORAGE (len)
						- 1)));

	  check_sequence_range (sequence, start, end, make_integer (len));

	  /* For the from_end t case; transform contents to an array with
	     elements addressable in constant time, use the same algorithm
	     as for vectors. */
	  deleting->size = len;
	  memset (&(deleting->bits), 0,
		  sizeof (long) * BIT_VECTOR_LONG_STORAGE (len));
	  
	  while (startp < endp)
	    {
	      itext_copy_ichar (startp, staging + (ii * MAX_ICHAR_LEN));
	      INC_IBYTEPTR (startp);
	      ii++;
	    }

	  GCPRO1 (elt);

	  ending = min (ending, len);

	  for (ii = ending - 1; ii >= starting; ii--)
	    {
	      elt = KEY (key, make_char (itext_ichar (staging +
						      (ii * MAX_ICHAR_LEN))));
	      for (jj = ii - 1; jj >= starting; jj--)
		{
		  if (check_test (test, key, elt,
				  make_char (itext_ichar
					     (staging + (jj * MAX_ICHAR_LEN))))
		      == test_not_unboundp)
		    {
		      set_bit_vector_bit (deleting, ii, 1);
		      deleted++;
		      break;
		    }
		}
	    }

	  UNGCPRO;

	  if (0 != deleted)
	    {
	      startp = XSTRING_DATA (sequence);

	      for (ii = 0; ii < len; ii++)
		{
		  if (!bit_vector_bit (deleting, ii))
		    {
		      staging_cursor
			+= itext_copy_ichar (startp, staging_cursor);
		    }

		  INC_IBYTEPTR (startp);
		}

	      sequence = make_string (staging, staging_cursor - staging);
	    }
	}
    }
  else if (VECTORP (sequence))
    {
      Elemcount deleted = 0;
      Lisp_Object *content = XVECTOR_DATA (sequence);
      struct Lisp_Bit_Vector *deleting;
      Lisp_Object elt = Qnil;

      len = XVECTOR_LENGTH (sequence);
      check_sequence_range (sequence, start, end, make_integer (len));

      deleting = (Lisp_Bit_Vector *) ALLOCA (sizeof (struct Lisp_Bit_Vector)
                                             + (sizeof (long)
                                                * (BIT_VECTOR_LONG_STORAGE (len)
                                                   - 1)));
      deleting->size = len;
      memset (&(deleting->bits), 0,
	      sizeof (long) * BIT_VECTOR_LONG_STORAGE (len));

      GCPRO1 (elt);

      ending = min (ending, len);

      if (NILP (from_end))
	{
	  for (ii = starting; ii < ending; ii++)
	    {
	      elt = KEY (key, content[ii]);

	      for (jj = ii + 1; jj < ending; jj++)
		{
		  if (check_test (test, key, elt, content[jj])
		      == test_not_unboundp)
		    {
		      set_bit_vector_bit (deleting, ii, 1);
		      deleted++;
		      break;
		    }
		}
	    }
	}
      else
	{
	  for (ii = ending - 1; ii >= starting; ii--)
	    {
	      elt = KEY (key, content[ii]);

	      for (jj = ii - 1; jj >= starting; jj--)
		{
		  if (check_test (test, key, elt, content[jj])
		      == test_not_unboundp)
		    {
		      set_bit_vector_bit (deleting, ii, 1);
		      deleted++;
		      break;
		    }
		}
	    }
	}

      UNGCPRO;

      if (deleted)
	{
	  Lisp_Object res = make_vector (len - deleted, Qnil),
	    *res_content = XVECTOR_DATA (res);

	  for (ii = jj = 0; ii < len; ii++)
	    {
	      if (!bit_vector_bit (deleting, ii))
		{
		  res_content[jj++] = content[ii];
		}
	    }

	  sequence = res;
	}
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *bv = XBIT_VECTOR (sequence);
      Elemcount deleted = 0;
      /* I'm a little irritated at this. Basically, the only reasonable
	 thing delete-duplicates should do if handed a bit vector is return
	 something of maximum length two and minimum length 0 (because
	 that's the possible number of distinct elements if EQ is regarded
	 as identity, which it should be).  But to support arbitrary TEST
	 and KEY arguments, which may be non-deterministic from our
	 perspective, we need the same algorithm as for vectors. */
      struct Lisp_Bit_Vector *deleting;
      Lisp_Object elt = Qnil;

      len = bit_vector_length (bv);

      if (EQ (Qidentity, key))
	{
	  /* We know all the elements will be bits; set check_test to
	     reflect that. This isn't useful if KEY is not #'identity, since
	     it may return non-bits for the elements. */
	  check_test = get_check_test_function (Qzero, &test, test_not,
						Qnil, Qnil, key,
						&test_not_unboundp);
	}

      check_sequence_range (sequence, start, end, make_integer (len));

      deleting = (Lisp_Bit_Vector *) ALLOCA (sizeof (struct Lisp_Bit_Vector)
                                             + (sizeof (long)
                                                * (BIT_VECTOR_LONG_STORAGE (len)
                                                   - 1)));
      deleting->size = len;
      memset (&(deleting->bits), 0,
	      sizeof (long) * BIT_VECTOR_LONG_STORAGE (len));

      ending = min (ending, len);

      GCPRO1 (elt);

      if (NILP (from_end))
	{
	  for (ii = starting; ii < ending; ii++)
	    {
	      elt = KEY (key, make_fixnum (bit_vector_bit (bv, ii)));

	      for (jj = ii + 1; jj < ending; jj++)
		{
		  if (check_test (test, key, elt,
				  make_fixnum (bit_vector_bit (bv, jj)))
		      == test_not_unboundp)
		    {
		      set_bit_vector_bit (deleting, ii, 1);
		      deleted++;
		      break;
		    }
		}
	    }
	}
      else
	{
	  for (ii = ending - 1; ii >= starting; ii--)
	    {
	      elt = KEY (key, make_fixnum (bit_vector_bit (bv, ii)));

	      for (jj = ii - 1; jj >= starting; jj--)
		{
		  if (check_test (test, key, elt,
				  make_fixnum (bit_vector_bit (bv, jj)))
		      == test_not_unboundp)
		    {
		      set_bit_vector_bit (deleting, ii, 1);
		      deleted++;
		      break;
		    }
		}
	    }
	}

      UNGCPRO;

      if (deleted)
	{
	  Lisp_Object res = make_bit_vector (len - deleted, Qzero);
	  Lisp_Bit_Vector *resbv = XBIT_VECTOR (res);

	  for (ii = jj = 0; ii < len; ii++)
	    {
	      if (!bit_vector_bit (deleting, ii))
		{
		  set_bit_vector_bit (resbv, jj++, bit_vector_bit (bv, ii));
		}
	    }

	  sequence = res;
	}
    }

  return sequence;
}

DEFUN ("remove-duplicates", Fremove_duplicates, 1, MANY, 0, /*
Remove duplicate elements from SEQUENCE, non-destructively.

If there are no duplicate elements in SEQUENCE, return it unmodified;
otherwise, return a new object.  If SEQUENCE is a list, the new object may
share list structure with SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) END FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence = args[0], keyed, positioned = Qnil;
  Lisp_Object result = sequence, result_tail = result, cursor = Qnil;
  Lisp_Object cons_with_shared_tail = Qnil;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, ii = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Fremove_duplicates, nargs, args, 6,
		  (test, key, test_not, start, end, from_end),
		  (start = Qzero));

  CHECK_SEQUENCE (sequence);

  if (!CONSP (sequence))
    {
      return Fdelete_duplicates (nargs, args);
    }

  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (NILP (key))
    {
      key = Qidentity;
    }

  get_check_match_function (&test, test_not, Qnil, Qnil, key,
			    &test_not_unboundp, &check_test);

  if (NILP (from_end))
    {
      Lisp_Object ignore = Qnil;

      GCPRO2 (keyed, result);

      {
	GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
          {
            if (starting <= ii && ii <= ending)
              {
                keyed = KEY (key, elt);
                positioned
                  = list_position_cons_before (&ignore, keyed, XCDR (tail),
                                               check_test, test_not_unboundp,
                                               test, key, 0,
                                               make_fixnum (max (starting
                                                              - (ii + 1), 0)),
                                               make_fixnum (ending - (ii + 1)));
                if (!NILP (positioned))
                  {
                    sequence = result = result_tail = XCDR (tail);
                  }
                else
                  {
                    break;
                  }
              }
            else
              {
                break;
              }

            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      {
	GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
          {
            if (!(starting <= ii && ii <= ending))
              {
                ii++;
                continue;
              }

            /* For this algorithm, each time we encounter an object to be
               removed, copy the output list from the tail beyond the last
               removed cons to this one. Otherwise, the tail of the output list
               is shared with the input list, which is OK. */

            keyed = KEY (key, elt);
            positioned
              = list_position_cons_before (&ignore, keyed, XCDR (tail),
                                           check_test, test_not_unboundp,
                                           test, key, 0,
                                           make_fixnum (max (starting - (ii + 1),
                                                          0)),
                                           make_fixnum (ending - (ii + 1)));
            if (!NILP (positioned))
              {
                if (EQ (result, sequence))
                  {
                    result = cons_with_shared_tail
                      = Fcons (XCAR (sequence), XCDR (sequence));
                  }

                result_tail = cons_with_shared_tail;
                cursor = XCDR (cons_with_shared_tail);

                while (!EQ (cursor, tail) && !NILP (cursor))
                  {
                    XSETCDR (result_tail, Fcons (XCAR (cursor), Qnil));
                    result_tail = XCDR (result_tail);
                    cursor = XCDR (cursor);
                  }

                XSETCDR (result_tail, XCDR (tail));
                cons_with_shared_tail = result_tail;
              }

            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      UNGCPRO;

      if ((ii < starting || (ii < ending && !NILP (end))))
	{
	  check_sequence_range (args[0], start, end, Flength (args[0]));
	}
    }
  else
    {
      result = list_delete_duplicates_from_end (sequence, check_test,
						test_not_unboundp, test, key,
						start, end, 1);
    }

  return result;
}
#undef KEY

DEFUN ("nreverse", Fnreverse, 1, 1, 0, /*
Reverse SEQUENCE, destructively.

Return the beginning of the reversed sequence, which will be a distinct Lisp
object if SEQUENCE is a list with length greater than one.  See also
`reverse', the non-destructive version of this function.
*/
       (sequence))
{
  CHECK_SEQUENCE (sequence);

  if (CONSP (sequence))
    {
      struct gcpro gcpro1, gcpro2;
      Lisp_Object prev = Qnil;
      Lisp_Object tail = sequence;

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
  else if (VECTORP (sequence))
    {
      Elemcount length = XVECTOR_LENGTH (sequence), ii = length;
      Elemcount half = length / 2;
      Lisp_Object swap = Qnil;
      CHECK_LISP_WRITEABLE (sequence);

      while (ii > half)
	{
	  swap = XVECTOR_DATA (sequence) [length - ii];
	  XVECTOR_DATA (sequence) [length - ii]
	    = XVECTOR_DATA (sequence) [ii - 1];
	  XVECTOR_DATA (sequence) [ii - 1] = swap;
	  --ii;
	}
    }
  else if (STRINGP (sequence))
    {
      Elemcount length = XSTRING_LENGTH (sequence);
      Ibyte *staging = alloca_ibytes (length), *staging_end = staging + length;
      Ibyte *cursor = XSTRING_DATA (sequence), *endp = cursor + length;

      CHECK_LISP_WRITEABLE (sequence);
      while (cursor < endp)
	{
	  staging_end -= itext_ichar_len (cursor);
	  itext_copy_ichar (cursor, staging_end);
	  INC_IBYTEPTR (cursor);
	}

      assert (staging == staging_end);

      memcpy (XSTRING_DATA (sequence), staging, length);
      init_string_ascii_begin (sequence);
      bump_string_modiff (sequence);
      sledgehammer_check_ascii_begin (sequence);
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *bv = XBIT_VECTOR (sequence);
      Elemcount length = bit_vector_length (bv), ii = length;
      Elemcount half = length / 2;
      int swap = 0;

      CHECK_LISP_WRITEABLE (sequence);
      while (ii > half)
	{
	  swap = bit_vector_bit (bv, length - ii);
	  set_bit_vector_bit (bv, length - ii, bit_vector_bit (bv, ii - 1));
	  set_bit_vector_bit (bv, ii - 1, swap);
	  --ii;
	}
    }
  else 
    {
      assert (NILP (sequence));
    }

  return sequence;
}

DEFUN ("reverse", Freverse, 1, 1, 0, /*
Reverse SEQUENCE, copying.  Return the reversed sequence.
See also the function `nreverse', which is used more often.
*/
       (sequence))
{
  Lisp_Object result = Qnil;

  CHECK_SEQUENCE (sequence);

  if (CONSP (sequence))
    {
      EXTERNAL_LIST_LOOP_2 (elt, sequence)
	{
	  result = Fcons (elt, result);
	}
    }
  else if (VECTORP (sequence))
    {
      Elemcount length = XVECTOR_LENGTH (sequence), ii = length;
      Lisp_Object *staging = alloca_array (Lisp_Object, length);

      while (ii > 0)
	{
	  staging[length - ii] = XVECTOR_DATA (sequence) [ii - 1];
	  --ii;
	}

      result = Fvector (length, staging);
    }
  else if (STRINGP (sequence))
    {
      Elemcount length = XSTRING_LENGTH (sequence);
      Ibyte *staging = alloca_ibytes (length), *staging_end = staging + length;
      Ibyte *cursor = XSTRING_DATA (sequence), *endp = cursor + length;

      while (cursor < endp)
	{
	  staging_end -= itext_ichar_len (cursor);
	  itext_copy_ichar (cursor, staging_end);
	  INC_IBYTEPTR (cursor);
	}

      assert (staging == staging_end);

      result = make_string (staging, length);
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *bv = XBIT_VECTOR (sequence), *res;
      Elemcount length = bit_vector_length (bv), ii = length;

      result = make_bit_vector (length, Qzero);
      res = XBIT_VECTOR (result);

      while (ii > 0)
	{
	  set_bit_vector_bit (res, length - ii, bit_vector_bit (bv, ii - 1));
	  --ii;
	}
    }
  else 
    {
      assert (NILP (sequence));
    }

  return result;
}

Lisp_Object
list_merge (Lisp_Object org_l1, Lisp_Object org_l2,
	    check_test_func_t check_merge,
	    Lisp_Object predicate, Lisp_Object key)
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

  /* It is sufficient to protect org_l1 and org_l2.  When l1 and l2 are
     updated, we copy the new values back into the org_ vars.  */

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

      if (check_merge (predicate, key, Fcar (l2), Fcar (l1)) == 0)
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
	     check_test_func_t check_merge,
             Lisp_Object predicate, Lisp_Object key)
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

      if (check_merge (predicate, key, back_staging[backing],
		       front_staging[fronting]) == 0)
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
			    check_test_func_t check_merge,
                            Lisp_Object predicate, Lisp_Object key,
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
	  check_merge (predicate, key, Fcar (list), array [array_index])
	  : !check_merge (predicate, key, array [array_index], Fcar (list)))
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
			    check_test_func_t check_merge,
                            Lisp_Object predicate, Lisp_Object key)
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

      if (check_merge (predicate, key, Fcar (list_two), Fcar (list_one))
	  == 0)
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
			     check_test_func_t check_merge,
                             Lisp_Object predicate, Lisp_Object key,
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
	  check_merge (predicate, key, Fcar (list), array [array_index]) :
	  !check_merge (predicate, key, array [array_index], Fcar (list)))
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
	c_array[counter] = make_fixnum (bit_vector_bit (v, counter));	\
      }                                                                 \
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
  check_test_func_t check_merge = NULL;

  PARSE_KEYWORDS (Fmerge, nargs, args, 1, (key), NULL);

  CHECK_SEQUENCE (sequence_one);
  CHECK_SEQUENCE (sequence_two);

  CHECK_KEY_ARGUMENT (key);

  check_merge = get_merge_predicate (predicate, key);

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
	  result = list_merge (sequence_one, sequence_two, check_merge,
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
                                               check_merge, predicate, key,
                                               reverse_order);
        }
    }
  else
    {
      Elemcount sequence_one_len = XFIXNUM (Flength (sequence_one)),
        sequence_two_len = XFIXNUM (Flength (sequence_two)), i;
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
                                      check_merge, predicate, key);
        }
      else if (LISTP (sequence_one))
        {
          list_array_merge_into_array (output + 1, output_len - 1,
                                       sequence_one,
                                       sequence_two_storage,
                                       sequence_two_len,
                                       check_merge, predicate, key, 0);
        }
      else if (LISTP (sequence_two))
        {
          list_array_merge_into_array (output + 1, output_len - 1,
                                       sequence_two,
                                       sequence_one_storage,
                                       sequence_one_len,
                                       check_merge, predicate, key, 1);
        }
      else
        {
          array_merge (output + 1, output_len - 1,
                       sequence_one_storage, sequence_one_len,
                       sequence_two_storage, sequence_two_len,
                       check_merge, predicate,
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

Lisp_Object
list_sort (Lisp_Object list, check_test_func_t check_merge,
	   Lisp_Object predicate, Lisp_Object key)
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object back, tem;
  Lisp_Object front = list;
  Lisp_Object len = Flength (list);

  if (XFIXNUM (len) < 2)
    return list;

  len = make_fixnum (XFIXNUM (len) / 2 - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO4 (front, back, predicate, key);
  front = list_sort (front, check_merge, predicate, key);
  back = list_sort (back, check_merge, predicate, key);

  RETURN_UNGCPRO (list_merge (front, back, check_merge, predicate, key));
}

static void
array_sort (Lisp_Object *array, Elemcount array_len,
	    check_test_func_t check_merge,
	    Lisp_Object predicate, Lisp_Object key)
{
  Elemcount split;

  if (array_len < 2)
    return;

  split = array_len / 2;

  array_sort (array, split, check_merge, predicate, key);
  array_sort (array + split, array_len - split, check_merge, predicate,
	      key);
  array_merge (array, array_len, array, split, array + split,
	       array_len - split, check_merge, predicate, key);
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
  check_test_func_t check_merge = NULL;
  Elemcount sequence_len, i;

  PARSE_KEYWORDS (FsortX, nargs, args, 1, (key), NULL);

  CHECK_SEQUENCE (sequence);

  CHECK_KEY_ARGUMENT (key);

  check_merge = get_merge_predicate (predicate, key);

  if (LISTP (sequence))
    {
      sequence = list_sort (sequence, check_merge, predicate, key);
    }
  else if (VECTORP (sequence))
    {
      array_sort (XVECTOR_DATA (sequence), XVECTOR_LENGTH (sequence),
                  check_merge, predicate, key);
    }
  else if (STRINGP (sequence))
    {
      Ibyte *strdata = XSTRING_DATA (sequence);

      sequence_len = string_char_length (sequence);

      STRING_DATA_TO_OBJECT_ARRAY (strdata, sequence_carray, i, sequence_len);

      /* No GCPRO necessary, characters are immediate. */
      array_sort (sequence_carray, sequence_len, check_merge, predicate, key);

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
      array_sort (sequence_carray, sequence_len, check_merge, predicate, key);

      for (i = 0; i < sequence_len; ++i)
        {
          set_bit_vector_bit (v, i, XFIXNUM (sequence_carray [i]));
        }
    }

  return sequence;
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
  Elemcount starting, ending = MOST_POSITIVE_FIXNUM + 1, ii, len;

  PARSE_KEYWORDS (Ffill, nargs, args, 2, (start, end), (start = Qzero));

  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (end);
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

      check_sequence_range (sequence, start, end, make_fixnum (len));
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
      bit = XFIXNUM (item);
      CHECK_LISP_WRITEABLE (sequence);
      len = bit_vector_length (v);

      check_sequence_range (sequence, start, end, make_fixnum (len));
      ending = min (ending, len);

      for (ii = starting; ii < ending; ++ii)
        {
          set_bit_vector_bit (v, ii, bit);
        }
    }
  else if (LISTP (sequence))
    {
      Elemcount counting = 0;

      {
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
  Ibyte *lisp_vals_staging = NULL, *cursor = NULL;
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
      enum lrecord_type lisp_vals_type = lrecord_type_symbol;
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
          else if (ARRAYP (lisp_vals))
            {
              CHECK_LISP_WRITEABLE (lisp_vals);
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
		      = make_fixnum (bit_vector_bit (XBIT_VECTOR (sequences[j]),
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
                      Faset (lisp_vals, make_fixnum (i), called);
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
                                          XFIXNUM (called)) :
                      (void) Faset (lisp_vals, make_fixnum (i), called);
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

      if (lisp_vals_staging != NULL)
	{
          CHECK_LISP_WRITEABLE (lisp_vals);
	  replace_string_range (lisp_vals, Qzero, make_fixnum (call_count),
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
  Elemcount len = 1 + MOST_POSITIVE_FIXNUM;
  Lisp_Object length = Qnil;
  int i;

  for (i = 0; i < nsequences; ++i)
    {
      if (CONSP (sequences[i]))
        {
          length = Flist_length (sequences[i]);
          if (!NILP (length))
            {
              len = min (len, XFIXNUM (length));
            }
        }
      else
        {
          CHECK_SEQUENCE (sequences[i]);
          length = Flength (sequences[i]);
          len = min (len, XFIXNUM (length));
        }
    }

  if (len == 1 + MOST_POSITIVE_FIXNUM)
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
  Elemcount len = MOST_POSITIVE_FIXNUM;
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

See also `find-if', which returns the corresponding element of SEQUENCE,
rather than the value given by PREDICATE, and accepts bounding index
keywords.

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


DEFUN ("reduce", Freduce, 2, MANY, 0, /*
Combine the elements of SEQUENCE using FUNCTION, a binary operation.

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
  Elemcount starting, ending = MOST_POSITIVE_FIXNUM + 1, ii = 0;

  PARSE_KEYWORDS (Freduce, nargs, args, 5,
                  (start, end, from_end, initial_value, key),
                  (start = Qzero, initial_value = Qunbound));

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (start);
  CHECK_KEY_ARGUMENT (key);

#define KEY(key, item) (EQ (Qidentity, key) ? item :			\
			IGNORE_MULTIPLE_VALUES (call1 (key, item)))
#define CALL2(function, accum, item)				\
  IGNORE_MULTIPLE_VALUES (call2 (function, accum, item))

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (end);
    }

  if (VECTORP (sequence))
    {
      Lisp_Vector *vv = XVECTOR (sequence);
      struct gcpro gcpro1;

      check_sequence_range (sequence, start, end, make_fixnum (vv->size));

      ending = min (ending, vv->size);

      GCPRO1 (accum);

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

      UNGCPRO;
    }
  else if (BIT_VECTORP (sequence))
    {
      Lisp_Bit_Vector *bv = XBIT_VECTOR (sequence);
      struct gcpro gcpro1;

      check_sequence_range (sequence, start, end, make_fixnum (bv->size));
      ending = min (ending, bv->size);

      GCPRO1 (accum);

      if (!UNBOUNDP (initial_value))
        {
          accum = initial_value;
        }
      else if (ending - starting)
        {
          if (NILP (from_end))
            {
              accum = KEY (key, make_fixnum (bit_vector_bit (bv, starting)));
              starting++;
            }
          else
            {
              accum = KEY (key, make_fixnum (bit_vector_bit (bv, ending - 1)));
              ending--;
            }
        }

      if (NILP (from_end))
        {
          for (ii = starting; ii < ending; ++ii)
            {
              accum = CALL2 (function, accum,
                             KEY (key, make_fixnum (bit_vector_bit (bv, ii))));
            }
        }
      else
        {
          for (ii = ending - 1; ii >= starting; --ii)
            {
              accum = CALL2 (function, KEY (key,
                                            make_fixnum (bit_vector_bit (bv,
                                                                      ii))),
                             accum);
            }
        }

      UNGCPRO;

    }
  else if (STRINGP (sequence))
    {
      struct gcpro gcpro1;

      GCPRO1 (accum);

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
          else if (ending - starting && cursor_offset < byte_len)
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
	      ii++;
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
	    }
        }
      else
        {
          Elemcount len = string_char_length (sequence);
          Bytecount cursor_offset, byte_len = XSTRING_LENGTH (sequence);
          const Ibyte *cursor;

	  check_sequence_range (sequence, start, end, make_fixnum (len));
          ending = min (ending, len);
          starting = XFIXNUM (start);

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

      UNGCPRO;
    }
  else if (LISTP (sequence))
    {
      if (NILP (from_end))
        {
	  struct gcpro gcpro1;

	  GCPRO1 (accum);

          if (!UNBOUNDP (initial_value))
            {
              accum = initial_value;
            }
          else if (ending - starting)
            {
	      GC_EXTERNAL_LIST_LOOP_2 (elt, sequence)
                {
                  if (ii == starting)
                    {
                      accum = KEY (key, elt);
                      starting++;
                      break;
                    }
                  ++ii;
                }
	      END_GC_EXTERNAL_LIST_LOOP (elt);
            }

	  ii = 0;

          if (ending - starting)
            {
	      GC_EXTERNAL_LIST_LOOP_2 (elt, sequence)
                {
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
	      END_GC_EXTERNAL_LIST_LOOP (elt);
            }

	  UNGCPRO;

	  if (ii < starting || (ii < ending && !NILP (end)))
	    {
	      check_sequence_range (sequence, start, end, Flength (sequence));
	    }
        }
      else
        {
          Boolint need_accum = 0;
          Lisp_Object *subsequence = NULL;
          Elemcount counting = 0, len = 0;
	  struct gcpro gcpro1;

	  len = XFIXNUM (Flength (sequence));
	  check_sequence_range (sequence, start, end, make_fixnum (len));
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
  Charcount ii = 0, ending, len;
  Charcount starting = BIGNUMP (start) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (start);
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
      ending = BIGNUMP (end) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (end);
      while (ii < ending && pcursor < pend)
	{
	  INC_IBYTEPTR (pcursor);
	  ii++;
	}
    }

  if (pcursor == pend)
    {
      /* We have the length, check it for our callers. */
      check_sequence_range (dest, start, end, make_fixnum (ii));
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
  Elemcount starting1, ending1 = MOST_POSITIVE_FIXNUM + 1, starting2;
  Elemcount ending2 = MOST_POSITIVE_FIXNUM + 1, counting = 0, startcounting;
  Boolint sequence1_listp, sequence2_listp,
    overwriting = EQ (sequence1, sequence2);

  PARSE_KEYWORDS (Freplace, nargs, args, 4, (start1, end1, start2, end2),
                  (start1 = start2 = Qzero));

  CHECK_SEQUENCE (sequence1);
  CHECK_LISP_WRITEABLE (sequence1);

  CHECK_SEQUENCE (sequence2);

  CHECK_NATNUM (start1);
  starting1 = BIGNUMP (start1) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (start1);
  CHECK_NATNUM (start2);
  starting2 = BIGNUMP (start2) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (start2);

  if (!NILP (end1))
    {
      CHECK_NATNUM (end1);
      ending1 = BIGNUMP (end1) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (end1);
    }

  if (!NILP (end2))
    {
      CHECK_NATNUM (end2);
      ending2 = BIGNUMP (end2) ? MOST_POSITIVE_FIXNUM + 1 : XFIXNUM (end2);
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
          Elemcount len = XFIXNUM (Flength (sequence2));
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
                                /* The XFIXNUM (start2) is intentional here; we
                                   called #'length after doing (nthcdr
                                   start2 sequence2). */
                                make_fixnum (XFIXNUM (start2) + len));
          check_sequence_range (sequence2, start2, end2,
                                make_fixnum (XFIXNUM (start2) + len));

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
              check_sequence_range (sequence1, start1, end1, make_fixnum (ii));
              check_sequence_range (sequence2, start2, end2, make_fixnum (ii));
            }
          else
            {
              assert ((pcursor - p) > 0);
              staging = alloca_ibytes (pcursor - p);
              memcpy (staging, p, pcursor - p);
              replace_string_range (result, start1,
                                    make_fixnum (starting1),
                                    staging, staging + (pcursor - p));
            }
        }
      else 
        {
          Elemcount seq_len = XFIXNUM (Flength (sequence2)), ii = 0,
            subseq_len = min (min (ending1 - starting1, seq_len - starting1),
                              min (ending2 - starting2, seq_len - starting2));
          Lisp_Object *subsequence = alloca_array (Lisp_Object, subseq_len);

          check_sequence_range (sequence1, start1, end1, make_fixnum (seq_len));
          check_sequence_range (sequence2, start2, end2, make_fixnum (seq_len));

          while (starting2 < ending2 && ii < seq_len)
            {
              subsequence[ii] = Faref (sequence2, make_fixnum (starting2));
              ii++, starting2++;
            }

          ii = 0;

          while (starting1 < ending1 && ii < seq_len)
            {
              Faset (sequence1, make_fixnum (starting1), subsequence[ii]);
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
          check_sequence_range (args[0], start1, end1,
                                make_fixnum (XFIXNUM (start1) + shortest_len));
        }
      else if (NILP (sequence2))
        {
          check_sequence_range (args[1], start2, end2,
                                make_fixnum (XFIXNUM (start2) + shortest_len));
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
                                    make_fixnum (XFIXNUM (start1) + starting1));
            }

          if (s2_data == s2_end)
            {
              check_sequence_range (sequence2, start2, end2,
                                    make_fixnum (char_count));
            }
        }
      else
        {
          Elemcount len2 = XFIXNUM (Flength (sequence2));
          check_sequence_range (sequence2, start2, end2, make_fixnum (len2));

          ending2 = min (ending2, len2);
          while (starting2 < ending2
                 && starting1 < ending1 && !NILP (sequence1))
            {
              CHECK_CONS (sequence1);
              XSETCAR (sequence1, Faref (sequence2, make_fixnum (starting2)));
              sequence1 = XCDR (sequence1);
              starting1++;
              starting2++;
            }

          if (NILP (sequence1))
            {
              check_sequence_range (args[0], start1, end1,
                                    make_fixnum (XFIXNUM (start1) + starting1));
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

          check_sequence_range (sequence1, start1, end1, make_fixnum (len));
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
                                    make_fixnum (XFIXNUM (start2) + ii));
            }

          replace_string_range (result, start1, make_fixnum (XFIXNUM (start1) + ii),
                                staging, cursor);
        }
      else
        {
          Elemcount len = XFIXNUM (Flength (sequence1));

          check_sequence_range (sequence1, start2, end1, make_fixnum (len));
          ending1 = min (ending2, min (ending1, len));

          while (starting1 < ending1 && !NILP (sequence2))
            {
              Faset (sequence1, make_fixnum (starting1),
                     CONSP (sequence2) ? XCAR (sequence2)
                     : Fcar (sequence2));
              sequence2 = XCDR (sequence2);
              starting1++;
              starting2++;
            }

          if (NILP (sequence2))
            {
              check_sequence_range (args[1], start2, end2,
                                    make_fixnum (XFIXNUM (start2) + starting2));
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

          check_sequence_range (sequence1, start1, end1, make_fixnum (len1));

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
              check_sequence_range (sequence2, start2, end2, make_fixnum (ii));
            }

          /* This isn't great; any error message won't necessarily reflect
             the END1 that was supplied to #'replace. */
          replace_string_range (result, start1, make_fixnum (starting1),
                                p2, p2cursor);
        }
      else if (STRINGP (sequence1))
        {
          Ibyte *staging, *cursor;
          Elemcount count, len1 = string_char_length (sequence1);
          Elemcount len2 = XFIXNUM (Flength (sequence2)), ii = 0;
          Lisp_Object obj;

          check_sequence_range (sequence1, start1, end1, make_fixnum (len1));
          check_sequence_range (sequence2, start2, end2, make_fixnum (len2));

          ending1 = min (ending1, len1);
          ending2 = min (ending2, len2);
          count = min (ending1 - starting1, ending2 - starting2);
          staging = cursor = alloca_ibytes (count * MAX_ICHAR_LEN);

          ii = 0;
          while (ii < count)
            {
              obj = Faref (sequence2, make_fixnum (starting2));

              CHECK_CHAR_COERCE_INT (obj);
              cursor += set_itext_ichar (cursor, XCHAR (obj));
              starting2++, ii++;
            }

          replace_string_range (result, start1,
                                make_fixnum (XFIXNUM (start1) + count),
                                staging, cursor);
        }
      else if (STRINGP (sequence2))
        {
          Ibyte *p2 = XSTRING_DATA (sequence2),
            *p2end = p2 + XSTRING_LENGTH (sequence2);
          Elemcount len1 = XFIXNUM (Flength (sequence1)), ii = 0;

          check_sequence_range (sequence1, start1, end1, make_fixnum (len1));
          ending1 = min (ending1, len1);

          while (ii < starting2 && p2 < p2end)
            {
              INC_IBYTEPTR (p2);
              ii++;
            }

          while (p2 < p2end && starting1 < ending1 && starting2 < ending2)
            {
              Faset (sequence1, make_fixnum (starting1),
                     make_char (itext_ichar (p2)));
              INC_IBYTEPTR (p2);
              starting1++;
              starting2++;
              ii++;
            }

          if (p2 == p2end)
            {
              check_sequence_range (sequence2, start2, end2, make_fixnum (ii));
            }
        }
      else
        {
          Elemcount len1 = XFIXNUM (Flength (sequence1)),
            len2 = XFIXNUM (Flength (sequence2));

          check_sequence_range (sequence1, start1, end1, make_fixnum (len1));
          check_sequence_range (sequence2, start2, end2, make_fixnum (len2));

          ending1 = min (ending1, len1);
          ending2 = min (ending2, len2);
          
          while (starting1 < ending1 && starting2 < ending2)
            {
              Faset (sequence1, make_fixnum (starting1),
                     Faref (sequence2, make_fixnum (starting2)));
              starting1++;
              starting2++;
            }
        }
    }

  return result;
}

DEFUN ("nsubstitute", Fnsubstitute, 3, MANY, 0, /*
Substitute NEW for OLD in SEQUENCE.

This is a destructive function; it reuses the storage of SEQUENCE whenever
possible.  See `remove*' for the meaning of the keywords.

arguments: (NEW OLD SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) FROM-END COUNT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object new_ = args[0], item = args[1], sequence = args[2];
  Lisp_Object object_, position0;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, encountered = 0;
  Elemcount len, ii = 0, counting = MOST_POSITIVE_FIXNUM, presenting = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Fnsubstitute, nargs, args, 9,
		  (test, if_, if_not, test_not, key, start, end, count,
		   from_end), (start = Qzero));

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  if (!NILP (count))
    {
      CHECK_INTEGER (count);
      if (FIXNUMP (count))
        {
          counting = XFIXNUM (count);
        }
#ifdef HAVE_BIGNUM
      else
        {
          counting = bignum_sign (XBIGNUM_DATA (count)) > 0 ?
            1 + MOST_POSITIVE_FIXNUM : -1 + MOST_NEGATIVE_FIXNUM;
        }
#endif

      if (counting <= 0)
	{
	  return sequence;
	}
    }

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  if (CONSP (sequence))
    {
      if (!NILP (count) && !NILP (from_end))
	{
	  Lisp_Object present = count_with_tail (&object_, nargs - 1, args + 1,
						 Qnsubstitute);

	  if (ZEROP (present))
	    {
	      return sequence;
	    }

	  presenting = XFIXNUM (present);
	  presenting = presenting <= counting ? 0 : presenting - counting;
	}

      {
	GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tail)
          {
            if (!(ii < ending))
              {
                break;
              }

            if (starting <= ii &&
                check_test (test, key, item, elt) == test_not_unboundp
                && (presenting ? encountered++ >= presenting
                    : encountered++ < counting))
              {
                CHECK_LISP_WRITEABLE (tail);
                XSETCAR (tail, new_);
              }
            else if (!presenting && encountered >= counting)
              {
                break;
              }

            ii++;
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      if ((ii < starting || (ii < ending && !NILP (end)))
	  && encountered < counting)
	{
	  check_sequence_range (args[0], start, end, Flength (args[0]));
	}
    }
  else if (STRINGP (sequence))
    {
      Ibyte *staging, new_bytes[MAX_ICHAR_LEN], *staging_cursor;
      Ibyte *startp = XSTRING_DATA (sequence), *cursor = startp;
      Bytecount cursor_offset = 0, byte_len = XSTRING_LENGTH (sequence);
      Bytecount new_len;
      Lisp_Object character;

      CHECK_CHAR_COERCE_INT (new_);

      new_len = set_itext_ichar (new_bytes, XCHAR (new_));

      /* Worst case scenario; new char is four octets long, all the old ones
	 were one octet long, all the old ones match.  */
      staging = alloca_ibytes (XSTRING_LENGTH (sequence) * new_len);
      staging_cursor = staging;

      if (!NILP (count) && !NILP (from_end))
	{
	  Lisp_Object present = count_with_tail (&character, nargs - 1,
						 args + 1, Qnsubstitute);

	  if (ZEROP (present))
	    {
	      return sequence;
	    }

	  presenting = XFIXNUM (present);

	  /* If there are fewer items in the string than we have
	     permission to change, we don't need to differentiate
	     between the :from-end nil and :from-end t
	     cases. Otherwise, presenting is the number of matching
	     items we need to ignore before we start to change. */
	  presenting = presenting <= counting ? 0 : presenting - counting;
	}

      ii = 0;
      while (cursor_offset < byte_len && ii < ending)
	{
	  if (ii >= starting)
	    {
	      character = make_char (itext_ichar (cursor));

	      if ((check_test (test, key, item, character)
		   == test_not_unboundp)
		  && (presenting ? encountered++ >= presenting :
		      encountered++ < counting))
		{
		  staging_cursor
		    += itext_copy_ichar (new_bytes, staging_cursor);
		}
	      else
		{
		  staging_cursor
		    += itext_copy_ichar (cursor, staging_cursor);
		}

	      startp = XSTRING_DATA (sequence);
	      cursor = startp + cursor_offset;

	      if (byte_len != XSTRING_LENGTH (sequence)
		  || !valid_ibyteptr_p (cursor))
		{
		  mapping_interaction_error (Qnsubstitute, sequence);
		}
	    }
	  else
	    {
	      staging_cursor += itext_copy_ichar (cursor, staging_cursor);
	    }

	  INC_IBYTEPTR (cursor);
	  cursor_offset = cursor - startp;
	  ii++;
	}

      if (ii < starting || (ii < ending && !NILP (end)))
	{
	  check_sequence_range (sequence, start, end, Flength (sequence));
	}

      if (0 != encountered)
	{
	  CHECK_LISP_WRITEABLE (sequence);
	  replace_string_range (sequence, Qzero, make_fixnum (ii),
				staging, staging_cursor);
	}
    }
  else
    {
      Elemcount positioning;
      Lisp_Object object = Qnil;

      len = XFIXNUM (Flength (sequence));
      check_sequence_range (sequence, start, end, make_fixnum (len));

      position0 = position (&object, item, sequence, check_test,
                            test_not_unboundp, test, key, start, end, from_end,
                            Qnil, Qnsubstitute);

      if (NILP (position0))
	{
	  return sequence;
	}

      positioning = XFIXNUM (position0);
      ending = min (len, ending);

      Faset (sequence, position0, new_);
      encountered = 1;

      if (NILP (from_end))
	{
	  for (ii = positioning + 1; ii < ending; ii++)
	    {
	      object_ = Faref (sequence, make_fixnum (ii));

	      if (check_test (test, key, item, object_) == test_not_unboundp
		  && encountered++ < counting)
		{
		  Faset (sequence, make_fixnum (ii), new_);
		}
	      else if (encountered == counting)
		{
		  break;
		}
	    }
	}
      else
	{
	  for (ii = positioning - 1; ii >= starting; ii--)
	    {
	      object_ = Faref (sequence, make_fixnum (ii));

	      if (check_test (test, key, item, object_) == test_not_unboundp
		  && encountered++ < counting)
		{
		  Faset (sequence, make_fixnum (ii), new_);
		}
	      else if (encountered == counting)
		{
		  break;
		}
	    }
	}
    }

  return sequence;
}

DEFUN ("substitute", Fsubstitute, 3, MANY, 0, /*
Substitute NEW for OLD in SEQUENCE.

This is a non-destructive function; it makes a copy of SEQUENCE if necessary
to avoid corrupting the original SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (NEW OLD SEQUENCE &key (TEST #'eql) (KEY #'identity) (START 0) (END (length SEQUENCE)) COUNT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object new_ = args[0], item = args[1], sequence = args[2], tail = Qnil;
  Lisp_Object result = Qnil, result_tail = Qnil;
  Lisp_Object object, position0, matched_count;
  Elemcount starting = 0, ending = MOST_POSITIVE_FIXNUM, encountered = 0;
  Elemcount ii = 0, counting = MOST_POSITIVE_FIXNUM, presenting = 0;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1;

  PARSE_KEYWORDS (Fsubstitute, nargs, args, 9,
		  (test, if_, if_not, test_not, key, start, end, count,
		   from_end), (start = Qzero, count = Qunbound));

  CHECK_SEQUENCE (sequence);

  CHECK_NATNUM (start);
  starting = BIGNUMP (start) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
      ending = BIGNUMP (end) ? 1 + MOST_POSITIVE_FIXNUM : XFIXNUM (end);
    }

  check_test = get_check_test_function (item, &test, test_not, if_, if_not,
					key, &test_not_unboundp);

  if (!UNBOUNDP (count))
    {
      if (!NILP (count))
	{
          CHECK_INTEGER (count);
          if (FIXNUMP (count))
            {
              counting = XFIXNUM (count);
            }
#ifdef HAVE_BIGNUM
          else
            {
              counting = bignum_sign (XBIGNUM_DATA (count)) > 0 ?
                1 + MOST_POSITIVE_FIXNUM : -1 + MOST_NEGATIVE_FIXNUM;
            }
#endif

          if (counting <= 0)
            {
              return sequence;
            }
	}
    }

  if (!CONSP (sequence))
    {
      position0 = position (&object, item, sequence, check_test,
                            test_not_unboundp, test, key, start, end, from_end,
                            Qnil, Qsubstitute);

      if (NILP (position0))
	{
	  return sequence;
	}
      else
	{
	  args[2] = Fcopy_sequence (sequence);
	  return Fnsubstitute (nargs, args);
	}
    }

  matched_count = count_with_tail (&tail, nargs - 1, args + 1, Qsubstitute);

  if (ZEROP (matched_count))
    {
      return sequence;
    }

  if (!NILP (count) && !NILP (from_end))
    {
      presenting = XFIXNUM (matched_count);
      presenting = presenting <= counting ? 0 : presenting - counting;
    }

  GCPRO1 (result);
  {
    GC_EXTERNAL_LIST_LOOP_3 (elt, sequence, tailing)
      {
        if (EQ (tail, tailing))
          {
	    XUNGCPRO (elt);
	    UNGCPRO;

            if (NILP (result))
              {
                return XCDR (tail);
              }
	  
            XSETCDR (result_tail, XCDR (tail));
	    return result;
          }
        else if (starting <= ii && ii < ending &&
                 (check_test (test, key, item, elt) == test_not_unboundp)
                 && (presenting ? encountered++ >= presenting
                     : encountered++ < counting))
          {
            if (NILP (result))
              {
                result = result_tail = Fcons (new_, Qnil);
              }
            else
              {
                XSETCDR (result_tail, Fcons (new_, Qnil));
                result_tail = XCDR (result_tail);
              }
          }
        else if (NILP (result))
          {
            result = result_tail = Fcons (elt, Qnil);
          }
        else
          {
            XSETCDR (result_tail, Fcons (elt, Qnil));
            result_tail = XCDR (result_tail);
          }

        if (ii == ending)
          {
            break;
          }

        ii++;
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }
  UNGCPRO;

  if (ii < starting || (ii < ending && !NILP (end)))
    {
      check_sequence_range (args[0], start, end, Flength (args[0]));
    }

  return result;
}

static Lisp_Object
subst (Lisp_Object new_, Lisp_Object old, Lisp_Object tree, int depth)
{
  if (depth + lisp_eval_depth > max_lisp_eval_depth)
    {
      stack_overflow ("Stack overflow in subst", tree); 
    }

  if (EQ (tree, old))
    {
      return new_;
    }
  else if (CONSP (tree))
    {
      Lisp_Object aa = subst (new_, old, XCAR (tree), depth + 1);
      Lisp_Object dd = subst (new_, old, XCDR (tree), depth + 1);

      if (EQ (aa, XCAR (tree)) && EQ (dd, XCDR (tree)))
	{
	  return tree;
	}
      else
	{
	  return Fcons (aa, dd);
	}
    }
  else
    {
      return tree;
    }
}

static Lisp_Object
sublis (Lisp_Object alist, Lisp_Object tree, 
	check_test_func_t check_test, Boolint test_not_unboundp,
	Lisp_Object test, Lisp_Object key, int depth)
{
  Lisp_Object keyed = KEY (key, tree), aa, dd;

  if (depth + lisp_eval_depth > max_lisp_eval_depth)
    {
      stack_overflow ("Stack overflow in sublis", tree); 
    }

  {
    GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
      {
        if (CONSP (elt) &&
	    check_test (test, key, XCAR (elt), keyed) == test_not_unboundp)
          {
	    XUNGCPRO (elt);
	    return XCDR (elt);
          }
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }

  if (!CONSP (tree))
    {
      return tree;
    }

  aa = sublis (alist, XCAR (tree), check_test, test_not_unboundp, test, key,
	       depth + 1);
  dd = sublis (alist, XCDR (tree), check_test, test_not_unboundp, test, key,
	       depth + 1);

  if (EQ (aa, XCAR (tree)) && EQ (dd, XCDR (tree)))
    {
      return tree;
    }

  return Fcons (aa, dd);
}

DEFUN ("sublis", Fsublis, 2, MANY, 0, /*
Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.

See `member*' for the meaning of :test, :test-not and :key.

arguments: (ALIST TREE &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object alist = args[0], tree = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Fsublis, nargs, args, 5, (test, if_, test_not, if_not, key),
		  (key = Qidentity));

  if (NILP (key))
    {
      key = Qidentity;
    }

  get_check_match_function (&test, test_not, if_, if_not, 
			    /* sublis() is going to apply the key, don't ask
			       for a match function that will do it for
			       us. */
			    Qidentity, &test_not_unboundp, &check_test);

  if (CONSP (alist) && NILP (XCDR (alist)) && CONSP (XCAR (alist))
      && EQ (key, Qidentity) && 1 == test_not_unboundp 
      && (check_eq_nokey == check_test ||
	  (check_eql_nokey == check_test &&
	   !NON_FIXNUM_NUMBER_P (XCAR (XCAR (alist))))))
    {
      /* #'subst with #'eq is very cheap indeed; call it. */
      return subst (XCDR (XCAR (alist)), XCAR (XCAR (alist)), tree, 0);
    }

  return sublis (alist, tree, check_test, test_not_unboundp, test, key, 0);
}

static Lisp_Object
nsublis (Lisp_Object alist, Lisp_Object tree,
	 check_test_func_t check_test,
	 Boolint test_not_unboundp,
	 Lisp_Object test, Lisp_Object key, int depth)
{
  Lisp_Object tree_saved = tree, tortoise = tree, keyed = Qnil;
  struct gcpro gcpro1, gcpro2;
  int count = 0;

  if (depth + lisp_eval_depth > max_lisp_eval_depth)
    {
      stack_overflow ("Stack overflow in nsublis", tree); 
    }

  GCPRO2 (tree_saved, keyed);

  while (CONSP (tree))
    {
      Boolint replaced = 0;
      keyed = KEY (key, XCAR (tree));

      {
	GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
	  {
	    if (CONSP (elt) &&
		check_test (test, key, XCAR (elt), keyed) == test_not_unboundp)
	      {
		CHECK_LISP_WRITEABLE (tree);
		/* See comment in sublis() on using elt_cdr. */
		XSETCAR (tree, XCDR (elt));
		replaced = 1;
		break;
	      }
	  }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      if (!replaced)
	{
	  if (CONSP (XCAR (tree)))
	    {
	      nsublis (alist, XCAR (tree), check_test, test_not_unboundp,
		       test, key, depth + 1);
	    }
	}

      keyed = KEY (key, XCDR (tree));
      replaced = 0;

      {
	GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
	  {
	    if (CONSP (elt) &&
		check_test (test, key, XCAR (elt), keyed) == test_not_unboundp)
	      {
		CHECK_LISP_WRITEABLE (tree);
		XSETCDR (tree, XCDR (elt));
		tree = Qnil;
		break;
	      }
	  }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      if (!NILP (tree))
	{
	  tree = XCDR (tree);
	}

      if (++count > CIRCULAR_LIST_SUSPICION_LENGTH)
	{
	  if (count & 1)
	    {
	      tortoise = XCDR (tortoise);
	    }

	  if (EQ (tortoise, tree))
	    {
	      signal_circular_list_error (tree);
	    }
	}
    }

  RETURN_UNGCPRO (tree_saved);
}

DEFUN ("nsublis", Fnsublis, 2, MANY, 0, /*
Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.

See `member*' for the meaning of :test, :test-not and :key.

arguments: (ALIST TREE &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object alist = args[0], tree = args[1], tailed = Qnil, keyed = Qnil;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Fnsublis, nargs, args, 5, (test, if_, test_not, if_not, key),
		  (key = Qidentity));

  if (NILP (key))
    {
      key = Qidentity;
    }

  get_check_match_function (&test, test_not, if_, if_not, 
			    /* nsublis() is going to apply the key, don't ask
			       for a match function that will do it for
			       us. */
			    Qidentity, &test_not_unboundp, &check_test);

  GCPRO2 (tailed, keyed);

  keyed = KEY (key, tree);

  {
    /* nsublis() won't attempt to replace a cons handed to it, do that
       ourselves. */
    GC_EXTERNAL_LIST_LOOP_2 (elt, alist)
      {
        if (CONSP (elt) &&
	    check_test (test, key, XCAR (elt), keyed) == test_not_unboundp)
          {
	    XUNGCPRO (elt);
            return XCDR (elt);
          }
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }

  UNGCPRO;

  return nsublis (alist, tree, check_test, test_not_unboundp, test, key, 0);
}

DEFUN ("subst", Fsubst, 3, MANY, 0, /*
Substitute NEW for OLD everywhere in TREE (non-destructively).

Return a copy of TREE with all elements `eql' to OLD replaced by NEW.

See `member*' for the meaning of :test, :test-not and :key.

arguments: (NEW OLD TREE &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object result, alist = noseeum_cons (noseeum_cons (args[1], args[0]),
                                            Qnil);
  args[1] = alist;
  result = Fsublis (nargs - 1, args + 1);
  free_cons (XCAR (alist));
  free_cons (alist);

  return result;
}

DEFUN ("nsubst", Fnsubst, 3, MANY, 0, /*
Substitute NEW for OLD everywhere in TREE (destructively).

Any element of TREE which is `eql' to OLD is changed to NEW (via a call to
`setcar').

See `member*' for the meaning of the keywords.  The keyword
:descend-structures, not specified by Common Lisp, allows callers to specify
that non-cons objects (vectors and range tables, among others) should also
undergo substitution.

arguments: (NEW OLD TREE &key (TEST #'eql) (KEY #'identity) TEST-NOT DESCEND-STRUCTURES)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object new_ = args[0], old = args[1], tree = args[2], result, alist;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Fnsubst, nargs, args, 6, (test, if_, test_not, if_not, key,
                                            descend_structures), NULL);
  if (!NILP (descend_structures))
    {
      check_test = get_check_test_function (old, &test, test_not, if_, if_not,
                                            key, &test_not_unboundp);

      return nsubst_structures (new_, old, tree, check_test, test_not_unboundp,
                                test, key);

    }

  alist = noseeum_cons (noseeum_cons (old, new_), Qnil);
  args[1] = alist;
  result = Fnsublis (nargs - 1, args + 1);
  free_cons (XCAR (alist));
  free_cons (alist);

  return result;
}

static Boolint
tree_equal (Lisp_Object tree1, Lisp_Object tree2,
	    check_test_func_t check_test, Boolint test_not_unboundp,
	    Lisp_Object test, Lisp_Object key, int depth)
{
  Lisp_Object tortoise1 = tree1, tortoise2 = tree2;
  struct gcpro gcpro1, gcpro2;
  int count = 0;
  Boolint result;

  if (depth + lisp_eval_depth > max_lisp_eval_depth)
    {
      stack_overflow ("Stack overflow in tree-equal", tree1); 
    }

  GCPRO2 (tree1, tree2);

  while (CONSP (tree1) && CONSP (tree2)
	 && tree_equal (XCAR (tree1), XCAR (tree2), check_test,
			test_not_unboundp, test, key, depth + 1))
    {
      tree1 = XCDR (tree1);
      tree2 = XCDR (tree2);

      if (++count > CIRCULAR_LIST_SUSPICION_LENGTH)
	{
	  if (count & 1)
	    {
	      tortoise1 = XCDR (tortoise1);
	      tortoise2 = XCDR (tortoise2);
	    }

	  if (EQ (tortoise1, tree1))
	    {
	      signal_circular_list_error (tree1);
	    }

	  if (EQ (tortoise2, tree2))
	    {
	      signal_circular_list_error (tree2);
	    }
	}
    }

  if (CONSP (tree1) || CONSP (tree2))
    {
      UNGCPRO;
      return 0;
    }

  result = check_test (test, key, tree1, tree2) == test_not_unboundp;
  UNGCPRO;

  return result;
}

DEFUN ("tree-equal", Ftree_equal, 2, MANY, 0, /*
Return t if TREE1 and TREE2 have `eql' leaves.

Atoms are compared by `eql', unless another test is specified using
:test; cons cells are compared recursively.

See `union' for the meaning of :test, :test-not and :key.

arguments: (TREE1 TREE2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object tree1 = args[0], tree2 = args[1];
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;

  PARSE_KEYWORDS (Ftree_equal, nargs, args, 3, (test, key, test_not),
		  (key = Qidentity));

  get_check_match_function (&test, test_not, Qnil, Qnil, key,
			    &test_not_unboundp, &check_test);

  return tree_equal (tree1, tree2, check_test, test_not_unboundp, test, key,
		     0) ? Qt : Qnil;
}

static Lisp_Object
mismatch_from_end (Lisp_Object sequence1, Lisp_Object start1, Lisp_Object end1,
                   Lisp_Object sequence2, Lisp_Object start2, Lisp_Object end2,
                   check_test_func_t check_match, Boolint test_not_unboundp,
                   Lisp_Object test, Lisp_Object key,
                   Boolint UNUSED (return_sequence1_index))
{
  Elemcount sequence1_len = XFIXNUM (Flength (sequence1));
  Elemcount sequence2_len = XFIXNUM (Flength (sequence2)), ii = 0;
  Elemcount starting1, ending1, starting2, ending2;
  Lisp_Object *sequence1_storage = NULL, *sequence2_storage = NULL;
  struct gcpro gcpro1, gcpro2;

  check_sequence_range (sequence1, start1, end1, make_fixnum (sequence1_len));
  starting1 = XFIXNUM (start1);
  ending1 = FIXNUMP (end1) ? XFIXNUM (end1) : 1 + MOST_POSITIVE_FIXNUM;
  ending1 = min (ending1, sequence1_len);

  check_sequence_range (sequence2, start2, end2, make_fixnum (sequence2_len));
  starting2 = XFIXNUM (start2);
  ending2 = FIXNUMP (end2) ? XFIXNUM (end2) : 1 + MOST_POSITIVE_FIXNUM;
  ending2 = min (ending2, sequence2_len);

  if (LISTP (sequence1))
    {
      Lisp_Object *saving;
      sequence1_storage = saving
        = alloca_array (Lisp_Object, ending1 - starting1);

      {
        EXTERNAL_LIST_LOOP_2 (elt, sequence1)
          {
            if (starting1 <= ii && ii < ending1)
              {
                *saving++ = elt;
              }
            else if (ii == ending1)
              {
                break;
              }

            ++ii;
          }
      }
    }
  else if (STRINGP (sequence1))
    {
      const Ibyte *cursor = string_char_addr (sequence1, starting1);

      STRING_DATA_TO_OBJECT_ARRAY (cursor, sequence1_storage, ii,
                                   ending1 - starting1);
      
    }
  else if (BIT_VECTORP (sequence1))
    {
      Lisp_Bit_Vector *vv = XBIT_VECTOR (sequence1);
      sequence1_storage = alloca_array (Lisp_Object, ending1 - starting1);
      for (ii = starting1; ii < ending1; ++ii)
        {
          sequence1_storage[ii - starting1]
            = make_fixnum (bit_vector_bit (vv, ii));
        }
    }
  else
    {
      sequence1_storage = XVECTOR_DATA (sequence1) + starting1;
    }

  ii = 0;

  if (LISTP (sequence2))
    {
      Lisp_Object *saving;
      sequence2_storage = saving
        = alloca_array (Lisp_Object, ending2 - starting2);

      {
        EXTERNAL_LIST_LOOP_2 (elt, sequence2)
          {
            if (starting2 <= ii && ii < ending2)
              {
                *saving++ = elt;
              }
            else if (ii == ending2)
              {
                break;
              }

            ++ii;
          }
      }
    }
  else if (STRINGP (sequence2))
    {
      const Ibyte *cursor = string_char_addr (sequence2, starting2);

      STRING_DATA_TO_OBJECT_ARRAY (cursor, sequence2_storage, ii,
                                   ending2 - starting2);
      
    }
  else if (BIT_VECTORP (sequence2))
    {
      Lisp_Bit_Vector *vv = XBIT_VECTOR (sequence2);
      sequence2_storage = alloca_array (Lisp_Object, ending2 - starting2);
      for (ii = starting2; ii < ending2; ++ii)
        {
          sequence2_storage[ii - starting2]
            = make_fixnum (bit_vector_bit (vv, ii));
        }
    }
  else
    {
      sequence2_storage = XVECTOR_DATA (sequence2) + starting2;
    }
  
  GCPRO2 (sequence1_storage[0], sequence2_storage[0]);
  gcpro1.nvars = ending1 - starting1;
  gcpro2.nvars = ending2 - starting2;

  while (ending1 > starting1 && ending2 > starting2)
    {
      --ending1;
      --ending2;

      if (check_match (test, key, sequence1_storage[ending1 - starting1],
                       sequence2_storage[ending2 - starting2])
          != test_not_unboundp)
        {
          UNGCPRO;
          return make_integer (ending1 + 1);
        }
    }

  UNGCPRO;

  if (ending1 > starting1 || ending2 > starting2)
    {
      return make_integer (ending1);
    }

  return Qnil;
}

static Lisp_Object
mismatch_list_list (Lisp_Object sequence1, Lisp_Object start1, Lisp_Object end1,
                    Lisp_Object sequence2, Lisp_Object start2, Lisp_Object end2,
                    check_test_func_t check_match, Boolint test_not_unboundp,
                    Lisp_Object test, Lisp_Object key,
                    Boolint UNUSED (return_list_index))
{
  Lisp_Object sequence1_tortoise = sequence1, sequence2_tortoise = sequence2;
  Lisp_Object orig_sequence1 = sequence1, orig_sequence2 = sequence2;
  Elemcount ending1 = MOST_POSITIVE_FIXNUM, ending2 = MOST_POSITIVE_FIXNUM;
  Elemcount starting1, starting2, counting, startcounting;
  Elemcount shortest_len = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  starting1 = FIXNUMP (start1) ? XFIXNUM (start1) : 1 + MOST_POSITIVE_FIXNUM;
  starting2 = FIXNUMP (start2) ? XFIXNUM (start2) : 1 + MOST_POSITIVE_FIXNUM;

  if (!NILP (end1))
    {
      ending1 = FIXNUMP (end1) ? XFIXNUM (end1) : 1 + MOST_POSITIVE_FIXNUM;
    }

  if (!NILP (end2))
    {
      ending2 = FIXNUMP (end2) ? XFIXNUM (end2) : 1 + MOST_POSITIVE_FIXNUM;
    }

  if (!ZEROP (start1))
    {
      sequence1 = Fnthcdr (start1, sequence1);

      if (NILP (sequence1))
        {
          check_sequence_range (sequence1_tortoise, start1, end1,
                                Flength (sequence1_tortoise));
          /* Give up early here. */
          return Qnil;
        }

      ending1 -= starting1;
      starting1 = 0;
      sequence1_tortoise = sequence1;
    }

  if (!ZEROP (start2))
    {
      sequence2 = Fnthcdr (start2, sequence2);

      if (NILP (sequence2))
        {
          check_sequence_range (sequence2_tortoise, start2, end2,
                                Flength (sequence2_tortoise));
          return Qnil;
        }

      ending2 -= starting2;
      starting2 = 0;
      sequence2_tortoise = sequence2;
    }
      
  GCPRO4 (sequence1, sequence2, sequence1_tortoise, sequence2_tortoise);

  counting = startcounting = min (ending1, ending2);

  while (counting-- > 0 && !NILP (sequence1) && !NILP (sequence2))
    {
      if (check_match (test, key,
                       CONSP (sequence1) ? XCAR (sequence1)
                       : Fcar (sequence1),
                       CONSP (sequence2) ? XCAR (sequence2)
                       : Fcar (sequence2) ) != test_not_unboundp)
        {
          UNGCPRO;
          return make_integer (XFIXNUM (start1) + shortest_len);
        }

      sequence1 = CONSP (sequence1) ? XCDR (sequence1) : Fcdr (sequence1);
      sequence2 = CONSP (sequence2) ? XCDR (sequence2) : Fcdr (sequence2);

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

  UNGCPRO;

  if (NILP (sequence1))
    {
      Lisp_Object args[] = { start1, make_fixnum (shortest_len) };
      check_sequence_range (orig_sequence1, start1, end1,
                            Fplus (countof (args), args));
    }

  if (NILP (sequence2))
    {
      Lisp_Object args[] = { start2, make_fixnum (shortest_len) };
      check_sequence_range (orig_sequence2, start2, end2,
                            Fplus (countof (args), args));
    }

  if ((!NILP (end1) && shortest_len != ending1 - starting1) ||
      (!NILP (end2) && shortest_len != ending2 - starting2))
    {
      return make_integer (XFIXNUM (start1) + shortest_len);
    }

  if ((NILP (end1) && CONSP (sequence1)) || (NILP (end2) && CONSP (sequence2)))
    {
      return make_integer (XFIXNUM (start1) + shortest_len); 
    }

  return Qnil;
}

static Lisp_Object
mismatch_list_string (Lisp_Object list, Lisp_Object list_start,
                      Lisp_Object list_end,
                      Lisp_Object string, Lisp_Object string_start,
                      Lisp_Object string_end,
                      check_test_func_t check_match,
                      Boolint test_not_unboundp,
                      Lisp_Object test, Lisp_Object key,
                      Boolint return_list_index)
{
  Ibyte *string_data = XSTRING_DATA (string), *startp = string_data;
  Bytecount string_offset = 0, string_len = XSTRING_LENGTH (string);
  Elemcount char_count = 0, list_starting, list_ending;
  Elemcount string_starting, string_ending;
  Lisp_Object character, orig_list = list;
  struct gcpro gcpro1;

  list_ending = FIXNUMP (list_end) ? XFIXNUM (list_end) : 1 + MOST_POSITIVE_FIXNUM;
  list_starting = FIXNUMP (list_start) ? XFIXNUM (list_start) : 1 + MOST_POSITIVE_FIXNUM;

  string_ending = FIXNUMP (string_end) ? XFIXNUM (string_end) : 1 + MOST_POSITIVE_FIXNUM;
  string_starting
    = FIXNUMP (string_start) ? XFIXNUM (string_start) : 1 + MOST_POSITIVE_FIXNUM;

  while (char_count < string_starting && string_offset < string_len)
    {
      INC_IBYTEPTR (string_data);
      string_offset = string_data - startp;
      char_count++;
    }

  if (!ZEROP (list_start))
    {
      list = Fnthcdr (list_start, list);
      if (NILP (list))
        {
          check_sequence_range (orig_list, list_start, list_end,
                                Flength (orig_list));
          return Qnil;
        }

      list_ending -= list_starting;
      list_starting = 0;
    }

  GCPRO1 (list);

  while (list_starting < list_ending && string_starting < string_ending
         && string_offset < string_len && !NILP (list))
    {
      character = make_char (itext_ichar (string_data));

      if (return_list_index)
        {
          if (check_match (test, key, CONSP (list) ? XCAR (list) : Fcar (list),
                           character)
              != test_not_unboundp)
            {
              UNGCPRO;
              return make_integer (XFIXNUM (list_start) + char_count);
            }
        }
      else
        {
          if (check_match (test, key, character,
                           CONSP (list) ? XCAR (list) : Fcar (list))
              != test_not_unboundp)
            {
              UNGCPRO;
              return make_integer (char_count);
            }
        }

      list = CONSP (list) ? XCDR (list) : Fcdr (list);

      startp = XSTRING_DATA (string);
      string_data = startp + string_offset;
      if (string_len != XSTRING_LENGTH (string)
          || !valid_ibyteptr_p (string_data))
        {
          mapping_interaction_error (Qmismatch, string);
        }

      list_starting++;
      string_starting++;
      char_count++;
      INC_IBYTEPTR (string_data);
      string_offset = string_data - startp;
    }

  UNGCPRO;

  if (NILP (list))
    {
      Lisp_Object args[] = { list_start, make_fixnum (char_count) };
      check_sequence_range (orig_list, list_start, list_end,
                            Fplus (countof (args), args));
    }

  if (string_data == XSTRING_DATA (string) + XSTRING_LENGTH (string))
    {
      check_sequence_range (string, string_start, string_end,
                            make_fixnum (char_count));
    }

  if ((NILP (string_end) ?
       string_offset < string_len : string_starting < string_ending) ||
      (NILP (list_end) ? !NILP (list) : list_starting < list_ending))
    {
      return make_integer (return_list_index ? XFIXNUM (list_start) + char_count :
                           char_count);
    }
  
  return Qnil;
}

static Lisp_Object
mismatch_list_array (Lisp_Object list, Lisp_Object list_start,
                     Lisp_Object list_end,
                     Lisp_Object array, Lisp_Object array_start,
                     Lisp_Object array_end,
                     check_test_func_t check_match,
                     Boolint test_not_unboundp,
                     Lisp_Object test, Lisp_Object key,
                     Boolint return_list_index)
{
  Elemcount ii = 0, list_starting, list_ending;
  Elemcount array_starting, array_ending, array_len;
  Lisp_Object orig_list = list;
  struct gcpro gcpro1;

  list_ending = FIXNUMP (list_end) ? XFIXNUM (list_end) : 1 + MOST_POSITIVE_FIXNUM;
  list_starting = FIXNUMP (list_start) ? XFIXNUM (list_start) : 1 + MOST_POSITIVE_FIXNUM;

  array_ending = FIXNUMP (array_end) ? XFIXNUM (array_end) : 1 + MOST_POSITIVE_FIXNUM;
  array_starting = FIXNUMP (array_start) ? XFIXNUM (array_start) : 1 + MOST_POSITIVE_FIXNUM;
  array_len = XFIXNUM (Flength (array));

  array_ending = min (array_ending, array_len);

  check_sequence_range (array, array_start, array_end, make_fixnum (array_len));

  if (!ZEROP (list_start))
    {
      list = Fnthcdr (list_start, list);
      if (NILP (list))
        {
          check_sequence_range (orig_list, list_start, list_end,
                                Flength (orig_list));
          return Qnil;
        }

      list_ending -= list_starting;
      list_starting = 0;
    }

  GCPRO1 (list);

  while (list_starting < list_ending && array_starting < array_ending
         && !NILP (list))
    {
      if (return_list_index)
        {
          if (check_match (test, key, CONSP (list) ? XCAR (list) : Fcar (list),
                           Faref (array, make_fixnum (array_starting)))
              != test_not_unboundp)
            {
              UNGCPRO;
              return make_integer (XFIXNUM (list_start) + ii);
            }
        }
      else
        {
          if (check_match (test, key, Faref (array, make_fixnum (array_starting)),
                           CONSP (list) ? XCAR (list) : Fcar (list))
              != test_not_unboundp)
            {
              UNGCPRO;
              return make_integer (array_starting);
            }
        }

      list = CONSP (list) ? XCDR (list) : Fcdr (list);
      list_starting++;
      array_starting++;
      ii++;
    }

  UNGCPRO;

  if (NILP (list))
    {
      Lisp_Object args[] = { list_start, make_fixnum (ii) };
      check_sequence_range (orig_list, list_start, list_end,
                            Fplus (countof (args), args));
    }

  if (array_starting < array_ending ||
      (NILP (list_end) ? !NILP (list) : list_starting < list_ending))
    {
      return make_integer (return_list_index ? XFIXNUM (list_start) + ii :
                           array_starting);
    }

  return Qnil;
}

static Lisp_Object
mismatch_string_array (Lisp_Object string, Lisp_Object string_start,
                       Lisp_Object string_end,
                       Lisp_Object array, Lisp_Object array_start,
                       Lisp_Object array_end,
                       check_test_func_t check_match, Boolint test_not_unboundp,
                       Lisp_Object test, Lisp_Object key,
                       Boolint return_string_index)
{
  Ibyte *string_data = XSTRING_DATA (string), *startp = string_data;
  Bytecount string_offset = 0, string_len = XSTRING_LENGTH (string);
  Elemcount char_count = 0, array_starting, array_ending, array_length;
  Elemcount string_starting, string_ending;
  Lisp_Object character;

  array_starting = FIXNUMP (array_start) ? XFIXNUM (array_start) : 1 + MOST_POSITIVE_FIXNUM;
  array_ending = FIXNUMP (array_end) ? XFIXNUM (array_end) : 1 + MOST_POSITIVE_FIXNUM;
  array_length = XFIXNUM (Flength (array));
  check_sequence_range (array, array_start, array_end, make_fixnum (array_length));
  array_ending = min (array_ending, array_length);

  string_ending = FIXNUMP (string_end) ? XFIXNUM (string_end) : 1 + MOST_POSITIVE_FIXNUM;
  string_starting
    = FIXNUMP (string_start) ? XFIXNUM (string_start) : 1 + MOST_POSITIVE_FIXNUM;

  while (char_count < string_starting && string_offset < string_len)
    {
      INC_IBYTEPTR (string_data);
      string_offset = string_data - startp;
      char_count++;
    }

  while (array_starting < array_ending && string_starting < string_ending
         && string_offset < string_len)
    {
      character = make_char (itext_ichar (string_data));

      if (return_string_index)
        {
          if (check_match (test, key, character,
                           Faref (array, make_fixnum (array_starting)))
              != test_not_unboundp)
            {
              return make_integer (char_count);
            }
        }
      else
        {
          if (check_match (test, key,
                           Faref (array, make_fixnum (array_starting)),
                           character)
              != test_not_unboundp)
            {
              return make_integer (XFIXNUM (array_start) + char_count);
            }
        }

      startp = XSTRING_DATA (string);
      string_data = startp + string_offset;
      if (string_len != XSTRING_LENGTH (string)
          || !valid_ibyteptr_p (string_data))
        {
          mapping_interaction_error (Qmismatch, string);
        }

      array_starting++;
      string_starting++;
      char_count++;
      INC_IBYTEPTR (string_data);
      string_offset = string_data - startp;
    }

  if (string_data == XSTRING_DATA (string) + XSTRING_LENGTH (string))
    {
      check_sequence_range (string, string_start, string_end,
                            make_fixnum (char_count));
    }

  if ((NILP (string_end) ?
       string_offset < string_len : string_starting < string_ending) ||
      (NILP (array_end) ? !NILP (array) : array_starting < array_ending))
    {
      return make_integer (return_string_index ? char_count :
                           XFIXNUM (array_start) + char_count);
    }
  
  return Qnil;
}

static Lisp_Object
mismatch_string_string (Lisp_Object string1,
                        Lisp_Object string1_start, Lisp_Object string1_end,
                        Lisp_Object string2, Lisp_Object string2_start,
                        Lisp_Object string2_end,
                        check_test_func_t check_match,
                        Boolint test_not_unboundp,
                        Lisp_Object test, Lisp_Object key,
                        Boolint UNUSED (return_string1_index))
{
  Ibyte *string1_data = XSTRING_DATA (string1), *startp1 = string1_data;
  Bytecount string1_offset = 0, string1_len = XSTRING_LENGTH (string1);
  Ibyte *string2_data = XSTRING_DATA (string2), *startp2 = string2_data;
  Bytecount string2_offset = 0, string2_len = XSTRING_LENGTH (string2);
  Elemcount char_count1 = 0, string1_starting, string1_ending;
  Elemcount char_count2 = 0, string2_starting, string2_ending;
  Lisp_Object character1, character2;

  string1_ending = FIXNUMP (string1_end) ? XFIXNUM (string1_end) : 1 + MOST_POSITIVE_FIXNUM;
  string1_starting
    = FIXNUMP (string1_start) ? XFIXNUM (string1_start) : 1 + MOST_POSITIVE_FIXNUM;

  string2_starting
    = FIXNUMP (string2_start) ? XFIXNUM (string2_start) : 1 + MOST_POSITIVE_FIXNUM;
  string2_ending = FIXNUMP (string2_end) ? XFIXNUM (string2_end) : 1 + MOST_POSITIVE_FIXNUM;

  while (char_count1 < string1_starting && string1_offset < string1_len)
    {
      INC_IBYTEPTR (string1_data);
      string1_offset = string1_data - startp1;
      char_count1++;
    }

  while (char_count2 < string2_starting && string2_offset < string2_len)
    {
      INC_IBYTEPTR (string2_data);
      string2_offset = string2_data - startp2;
      char_count2++;
    }

  while (string2_starting < string2_ending && string1_starting < string1_ending
         && string1_offset < string1_len && string2_offset < string2_len)
    {
      character1 = make_char (itext_ichar (string1_data));
      character2 = make_char (itext_ichar (string2_data));

      if (check_match (test, key, character1, character2)
          != test_not_unboundp)
        {
          return make_integer (char_count1);
        }

      startp1 = XSTRING_DATA (string1);
      string1_data = startp1 + string1_offset;
      if (string1_len != XSTRING_LENGTH (string1)
          || !valid_ibyteptr_p (string1_data))
        {
          mapping_interaction_error (Qmismatch, string1);
        }

      startp2 = XSTRING_DATA (string2);
      string2_data = startp2 + string2_offset;
      if (string2_len != XSTRING_LENGTH (string2)
          || !valid_ibyteptr_p (string2_data))
        {
          mapping_interaction_error (Qmismatch, string2);
        }

      string2_starting++;
      string1_starting++;
      char_count1++;
      char_count2++;
      INC_IBYTEPTR (string1_data);
      string1_offset = string1_data - startp1;
      INC_IBYTEPTR (string2_data);
      string2_offset = string2_data - startp2;
    }

  if (string1_data == XSTRING_DATA (string1) + XSTRING_LENGTH (string1))
    {
      check_sequence_range (string1, string1_start, string1_end,
                            make_fixnum (char_count1));
    }

  if (string2_data == XSTRING_DATA (string2) + XSTRING_LENGTH (string2))
    {
      check_sequence_range (string2, string2_start, string2_end,
                            make_fixnum (char_count2));
    }

  if ((!NILP (string1_end) && string1_starting < string1_ending) ||
      (!NILP (string2_end) && string2_starting < string2_ending))
    {
      return make_integer (char_count1);
    }

  if ((NILP (string1_end) && string1_data
       < (XSTRING_DATA (string1) + XSTRING_LENGTH (string1))) ||
      (NILP (string2_end) && string2_data
       < (XSTRING_DATA (string2) + XSTRING_LENGTH (string2))))
    {
      return make_integer (char_count1);
    }
  
  return Qnil;
}

static Lisp_Object
mismatch_array_array (Lisp_Object array1, Lisp_Object start1, Lisp_Object end1,
                      Lisp_Object array2, Lisp_Object start2, Lisp_Object end2,
                      check_test_func_t check_match, Boolint test_not_unboundp,
                      Lisp_Object test, Lisp_Object key,
                      Boolint UNUSED (return_array1_index))
{
  Elemcount len1 = XFIXNUM (Flength (array1)), len2 = XFIXNUM (Flength (array2));
  Elemcount ending1 = MOST_POSITIVE_FIXNUM, ending2 = MOST_POSITIVE_FIXNUM;
  Elemcount starting1, starting2; 

  check_sequence_range (array1, start1, end1, make_fixnum (len1));
  check_sequence_range (array2, start2, end2, make_fixnum (len2));

  starting1 = FIXNUMP (start1) ? XFIXNUM (start1) : 1 + MOST_POSITIVE_FIXNUM;
  starting2 = FIXNUMP (start2) ? XFIXNUM (start2) : 1 + MOST_POSITIVE_FIXNUM;

  if (!NILP (end1))
    {
      ending1 = FIXNUMP (end1) ? XFIXNUM (end1) : 1 + MOST_POSITIVE_FIXNUM;
    }

  if (!NILP (end2))
    {
      ending2 = FIXNUMP (end2) ? XFIXNUM (end2) : 1 + MOST_POSITIVE_FIXNUM;
    }

  ending1 = min (ending1, len1);
  ending2 = min (ending2, len2);
          
  while (starting1 < ending1 && starting2 < ending2)
    {
      if (check_match (test, key, Faref (array1, make_fixnum (starting1)),
                       Faref (array2, make_fixnum (starting2)))
          != test_not_unboundp)
        {
          return make_integer (starting1);
        }
      starting1++;
      starting2++;
    }

  if (starting1 < ending1 || starting2 < ending2)
    {
      return make_integer (starting1);
    }

  return Qnil;
}

typedef Lisp_Object
(*mismatch_func_t) (Lisp_Object sequence1, Lisp_Object start1, Lisp_Object end1,
                    Lisp_Object sequence2, Lisp_Object start2, Lisp_Object end2,
                    check_test_func_t check_match, Boolint test_not_unboundp,
                    Lisp_Object test, Lisp_Object key,
                    Boolint return_list_index);

static mismatch_func_t
get_mismatch_func (Lisp_Object sequence1, Lisp_Object sequence2,
                   Lisp_Object from_end, Boolint *return_sequence1_index_out)
{
  CHECK_SEQUENCE (sequence1);
  CHECK_SEQUENCE (sequence2);

  if (!NILP (from_end))
    {
      *return_sequence1_index_out = 1;
      return mismatch_from_end;
    }

  if (LISTP (sequence1))
    {
      if (LISTP (sequence2))
        {
          *return_sequence1_index_out = 1;
          return mismatch_list_list;
        }

      if (STRINGP (sequence2))
        {
          *return_sequence1_index_out = 1;
          return mismatch_list_string;
        }

      *return_sequence1_index_out = 1;
      return mismatch_list_array;
    }

  if (STRINGP (sequence1))
    {
      if (STRINGP (sequence2))
        {
          *return_sequence1_index_out = 1;
          return mismatch_string_string;
        }

      if (LISTP (sequence2))
        {
          *return_sequence1_index_out = 0;
          return mismatch_list_string;
        }

      *return_sequence1_index_out = 1;
      return mismatch_string_array;
    }

  if (ARRAYP (sequence1))
    {
      if (STRINGP (sequence2))
        {
          *return_sequence1_index_out = 0;
          return mismatch_string_array;
        }

      if (LISTP (sequence2))
        {
          *return_sequence1_index_out = 0;
          return mismatch_list_array;
        }

      *return_sequence1_index_out = 1;
      return mismatch_array_array;
    }

  RETURN_NOT_REACHED (NULL);
  return NULL;
}

DEFUN ("mismatch", Fmismatch, 2, MANY, 0, /*
Compare SEQUENCE1 with SEQUENCE2, return index of first mismatching element.

Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.  A
non-nil return value always reflects an index into SEQUENCE1.

See `search' for the meaning of the keywords."

arguments: (SEQUENCE1 SEQUENCE2 &key (TEST #'eql) (KEY #'identity) (START1 0) END1 (START2 0) END2 FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence1 = args[0], sequence2 = args[1];
  Boolint test_not_unboundp = 1, return_first_index = 0;
  check_test_func_t check_match = NULL;
  mismatch_func_t mismatch = NULL;

  PARSE_KEYWORDS (Fmismatch, nargs, args, 8,
                  (test, key, from_end, start1, end1, start2, end2, test_not),
                  (start1 = start2 = Qzero));

  CHECK_SEQUENCE (sequence1);
  CHECK_SEQUENCE (sequence2);

  CHECK_NATNUM (start1);
  CHECK_NATNUM (start2);

  if (!NILP (end1))
    {
      CHECK_NATNUM (end1);
    }

  if (!NILP (end2))
    {
      CHECK_NATNUM (end2);
    }

  check_match = get_check_match_function (&test, test_not, Qnil, Qnil, key,
                                          &test_not_unboundp, NULL);
  mismatch = get_mismatch_func (sequence1, sequence2, from_end,
                                &return_first_index);

  if (return_first_index)
    {
      return mismatch (sequence1, start1, end1, sequence2, start2, end2,
                       check_match, test_not_unboundp, test, key, 1);
    }

  return mismatch (sequence2, start2, end2, sequence1, start1, end1,
                   check_match, test_not_unboundp, test, key, 0);
}

DEFUN ("search", Fsearch, 2, MANY, 0, /*
Search for SEQUENCE1 as a subsequence of SEQUENCE2.

Return the index of the leftmost element of the first match found; return
nil if there are no matches.

In this function, :start1 and :end1 specify a subsequence of SEQUENCE1, and
:start2 and :end2 specify a subsequence of SEQUENCE2.  See `remove*' for
details of the other keywords.

arguments: (SEQUENCE1 SEQUENCE2 &key (TEST #'eql) (KEY #'identity) (START1 0) END1 (START2 0) END2 FROM-END TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence1 = args[0], sequence2 = args[1], position0 = Qnil;
  Boolint test_not_unboundp = 1, return_first = 0;
  check_test_func_t check_test = NULL, check_match = NULL;
  mismatch_func_t mismatch = NULL;
  Elemcount starting1 = 0, ending1 = 1 + MOST_POSITIVE_FIXNUM, starting2 = 0;
  Elemcount ending2 = 1 + MOST_POSITIVE_FIXNUM, ii = 0;
  Elemcount length1;
  Lisp_Object object = Qnil;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Fsearch, nargs, args, 8,
                  (test, key, from_end, start1, end1, start2, end2, test_not),
                  (start1 = start2 = Qzero));

  CHECK_SEQUENCE (sequence1);
  CHECK_SEQUENCE (sequence2);
  CHECK_KEY_ARGUMENT (key);

  CHECK_NATNUM (start1);
  starting1 = FIXNUMP (start1) ? XFIXNUM (start1) : 1 + MOST_POSITIVE_FIXNUM;
  CHECK_NATNUM (start2);
  starting2 = FIXNUMP (start2) ? XFIXNUM (start2) : 1 + MOST_POSITIVE_FIXNUM;

  if (!NILP (end1))
    {
      Lisp_Object len1 = Flength (sequence1);

      CHECK_NATNUM (end1);
      check_sequence_range (sequence1, start1, end1, len1);
      ending1 = min (XFIXNUM (end1), XFIXNUM (len1));
    }
  else
    {
      end1 = Flength (sequence1);
      check_sequence_range (sequence1, start1, end1, end1);
      ending1 = XFIXNUM (end1);
    }

  length1 = ending1 - starting1;

  if (!NILP (end2))
    {
      Lisp_Object len2 = Flength (sequence2);

      CHECK_NATNUM (end2);
      check_sequence_range (sequence2, start2, end2, len2);
      ending2 = min (XFIXNUM (end2), XFIXNUM (len2));
    }
  else
    {
      end2 = Flength (sequence2);
      check_sequence_range (sequence2, start2, end2, end2);
      ending2 = XFIXNUM (end2);
    }

  check_match = get_check_match_function (&test, test_not, Qnil, Qnil, key,
                                          &test_not_unboundp, &check_test);
  mismatch = get_mismatch_func (sequence1, sequence2, from_end, &return_first);

  if (bytecode_arithcompare (start1, make_integer (ending1)) >= 0)
    {
      if (NILP (from_end))
        {
          return start2;
        }

      if (NILP (end2))
        {
          return Flength (sequence2);
        }

      return end2;
    }

  if (NILP (from_end))
    {
      Lisp_Object mismatch_start1 = Fadd1 (start1);
      Lisp_Object first = KEY (key, Felt (sequence1, start1));
      GCPRO2 (first, mismatch_start1);
      
      ii = starting2;
      while (ii < ending2)
        {
          position0 = position (&object, first, sequence2, check_test,
                                test_not_unboundp, test, key, make_fixnum (ii),
                                end2, Qnil, Qnil, Qsearch);
          if (NILP (position0))
            {
              UNGCPRO;
              return Qnil;
            }

          if (length1 + XFIXNUM (position0) <= ending2 &&
              (return_first ?
               NILP (mismatch (sequence1, mismatch_start1, end1,
                               sequence2,
                               make_fixnum (1 + XFIXNUM (position0)),
                               make_fixnum (length1 + XFIXNUM (position0)),
                               check_match, test_not_unboundp, test, key, 1)) :
               NILP (mismatch (sequence2,
                               make_fixnum (1 + XFIXNUM (position0)),
                               make_fixnum (length1 + XFIXNUM (position0)),
                               sequence1, mismatch_start1, end1,
                               check_match, test_not_unboundp, test, key, 0))))


            {
              UNGCPRO;
              return position0;
            }

          ii = XFIXNUM (position0) + 1;
        }

      UNGCPRO;
    }
  else
    {
      Lisp_Object mismatch_end1 = make_integer (ending1 - 1);
      Lisp_Object last = KEY (key, Felt (sequence1, mismatch_end1));
      GCPRO2 (last, mismatch_end1);

      ii = ending2;
      while (ii > starting2)
        {
          position0 = position (&object, last, sequence2, check_test,
                                test_not_unboundp, test, key, start2,
                                make_fixnum (ii), Qt, Qnil, Qsearch);

          if (NILP (position0))
            {
              UNGCPRO;
              return Qnil;
            }

          if (XFIXNUM (position0) - length1 + 1 >= starting2 &&
              (return_first ?
               NILP (mismatch (sequence1, start1, mismatch_end1,
                               sequence2,
                               make_fixnum (XFIXNUM (position0) - length1 + 1),
                               make_fixnum (XFIXNUM (position0)),
                               check_match, test_not_unboundp, test, key, 1)) :
               NILP (mismatch (sequence2,
                               make_fixnum (XFIXNUM (position0) - length1 + 1),
                               make_fixnum (XFIXNUM (position0)),
                               sequence1, start1, mismatch_end1,
                               check_match, test_not_unboundp, test, key, 0))))
            {
              UNGCPRO;
              return make_fixnum (XFIXNUM (position0) - length1 + 1);
            }

          ii = XFIXNUM (position0);
        }

      UNGCPRO;
    }

  return Qnil;
}

/* These two functions do set operations, those that can be visualised with
   Venn diagrams. */
static Lisp_Object
venn (Lisp_Object caller, int nargs, Lisp_Object *args, Boolint intersectionp)
{
  Lisp_Object liszt1 = args[0], liszt2 = args[1];
  Lisp_Object result = EQ (caller, Qsubsetp) ? Qt : Qnil, result_tail = Qnil;
  Lisp_Object keyed = Qnil, ignore = Qnil;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS_8 (caller, nargs, args, 4, (test, key, test_not, stable),
                    NULL, 2, 0);

  CHECK_LIST (liszt1);
  CHECK_LIST (liszt2);

  CHECK_KEY_ARGUMENT (key);
 
  /* #### Consider refactoring these tests into callers, and/or optimizing
     tests. */
  if (EQ (caller, Qsubsetp))
    {
      if (NILP (liszt1))
	{
	  return Qt;
	}
      if (NILP (liszt2))
        {
	  return Qnil;
        }
    }

  if (NILP (liszt1) && intersectionp)
    {
      return Qnil;
    }

  if (NILP (liszt2))
    {
      return intersectionp ? Qnil : liszt1;
    }

  get_check_match_function (&test, test_not, Qnil, Qnil, key,
                            &test_not_unboundp, &check_test);

  GCPRO2 (keyed, result);

  {
    GC_EXTERNAL_LIST_LOOP_2 (elt, liszt1)
      {
        keyed = KEY (key, elt);
        if (NILP (list_position_cons_before (&ignore, keyed, liszt2,
                                             check_test, test_not_unboundp,
                                             test, key, 0, Qzero, Qnil))
            != intersectionp)
          {
            if (EQ (Qsubsetp, caller))
              {
                result = Qnil;
                break;
              }
            else if (NILP (stable))
              {
                result = Fcons (elt, result);
              }
            else if (NILP (result))
              {
                result = result_tail = Fcons (elt, Qnil);
              }
            else
              {
                XSETCDR (result_tail, Fcons (elt, Qnil));
                result_tail = XCDR (result_tail);
              }
          }
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }

  UNGCPRO;

  return result;
}

static Lisp_Object
nvenn (Lisp_Object caller, int nargs, Lisp_Object *args, Boolint intersectionp)
{
  Lisp_Object liszt1 = args[0], liszt2 = args[1], tortoise_elt, ignore = Qnil;
  Lisp_Object elt = Qnil, tail = Qnil, keyed = Qnil, prev_tail = Qnil;
  Elemcount count;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  PARSE_KEYWORDS_8 (caller, nargs, args, 3, (test, key, test_not),
                    NULL, 2, 0);

  CHECK_LIST (liszt1);
  CHECK_LIST (liszt2);

  CHECK_KEY_ARGUMENT (key);

  if (NILP (liszt1) && intersectionp)
    {
      return Qnil;
    }

  if (NILP (liszt2))
    {
      return intersectionp ? Qnil : liszt1;
    }

  get_check_match_function (&test, test_not, Qnil, Qnil, key,
                            &test_not_unboundp, &check_test);

  tortoise_elt = tail = liszt1, count = 0;

  GCPRO4 (tail, keyed, liszt1, tortoise_elt);

  while (CONSP (tail) ? (elt = XCAR (tail), 1) : NILP (tail) ? 0 :
         (signal_malformed_list_error (liszt1), 0))
    {
      keyed = KEY (key, elt);      
      if (NILP (list_position_cons_before (&ignore, keyed, liszt2,
                                           check_test, test_not_unboundp,
                                           test, key, 0, Qzero, Qnil))
          == intersectionp)
        {
          if (NILP (prev_tail))
            {
              liszt1 = XCDR (tail);
            }
          else
            {
              XSETCDR (prev_tail, XCDR (tail));
            }

          tail = XCDR (tail);
          /* List is definitely not circular now! */
          count = 0;
        }
      else
        {
          prev_tail = tail;
          tail = XCDR (tail);
        }

      if (count++ < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

      if (count & 1)
        {
          tortoise_elt = XCDR (tortoise_elt);
        }

      if (EQ (elt, tortoise_elt))
        {
          signal_circular_list_error (liszt1);
        }
    }

  UNGCPRO;

  return liszt1;
}

DEFUN ("intersection", Fintersection, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-intersection operation.

The result list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

A non-nil value for the :stable keyword, not specified by Common Lisp, means
return the items in the order they appear in LIST1.

See `union' for the meaning of :test, :test-not and :key."

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT STABLE)
*/
       (int nargs, Lisp_Object *args))
{
  return venn (Qintersection, nargs, args, 1);
}

DEFUN ("nintersection", Fnintersection, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-intersection operation.

The result list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 whenever
possible.

See `union' for the meaning of :test, :test-not and :key."

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  return nvenn (Qnintersection, nargs, args, 1);
}

DEFUN ("subsetp", Fsubsetp, 2, MANY, 0, /*
Return non-nil if every element of LIST1 also appears in LIST2.

See `union' for the meaning of the keyword arguments.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  return venn (Qsubsetp, nargs, args, 0);
}

DEFUN ("set-difference", Fset_difference, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-difference operation.

The result list contains all items that appear in LIST1 but not LIST2.  This
is a non-destructive function; it makes a copy of the data if necessary to
avoid corrupting the original LIST1 and LIST2.

See `union' for the meaning of :test, :test-not and :key.

A non-nil value for the :stable keyword, not specified by Common Lisp, means
return the items in the order they appear in LIST1.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT STABLE)
*/
       (int nargs, Lisp_Object *args))
{
  return venn (Qset_difference, nargs, args, 0);
}

DEFUN ("nset-difference", Fnset_difference, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-difference operation.

The result list contains all items that appear in LIST1 but not LIST2.  This
is a destructive function; it reuses the storage of LIST1 whenever possible.

See `union' for the meaning of :test, :test-not and :key."

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  return nvenn (Qnset_difference, nargs, args, 0);
}

DEFUN ("nunion", Fnunion, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.

This is a destructive function, it reuses the storage of LIST1 whenever
possible.

See `union' for the meaning of :test, :test-not and :key.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  args[0] = nvenn (Qnunion, nargs, args, 0);
  return bytecode_nconc2 (args);
}

DEFUN ("union", Funion, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

The keywords :test and :test-not specify two-argument test and negated-test
predicates, respectively; :test defaults to `eql'.  See `member*' for more
information.

:key specifies a one-argument function that transforms elements of LIST1
and LIST2 into \"comparison keys\" before the test predicate is applied.
For example, if :key is #'car, then the car of elements from LIST1 is
compared with the car of elements from LIST2.  The :key function, however,
does not affect the elements in the returned list, which are taken directly
from the elements in LIST1 and LIST2.

A non-nil value for the :stable keyword, not specified by Common Lisp, means
return the items of LIST1 in order, followed by the remaining items of LIST2
in the order they occur in LIST2.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT STABLE)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object liszt1 = args[0], liszt2 = args[1], ignore = Qnil;
  Lisp_Object keyed = Qnil, result, result_tail;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_test = NULL, check_match = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Funion, nargs, args, 4, (test, key, test_not, stable), NULL);

  CHECK_LIST (liszt1);
  CHECK_LIST (liszt2);

  CHECK_KEY_ARGUMENT (key);

  if (NILP (liszt1))
    {
      return liszt2;
    }

  if (NILP (liszt2))
    {
      return liszt1;
    }

  check_match = get_check_match_function (&test, test_not, Qnil, Qnil, key,
                                          &test_not_unboundp, &check_test);

  GCPRO2 (keyed, result);

  if (NILP (stable))
    {
      result = liszt2;
      {
	GC_EXTERNAL_LIST_LOOP_2 (elt, liszt1)
          {
            keyed = KEY (key, elt);
            if (NILP (list_position_cons_before (&ignore, keyed, liszt2,
                                                 check_test, test_not_unboundp,
                                                 test, key, 0, Qzero, Qnil)))
              {
                /* The Lisp version of #'union used to check which list was
                   longer, and use that as the tail of the constructed
                   list. That fails when the order of arguments to TEST is
                   specified, as is the case for these functions. We could
                   pass the reverse_check argument to
                   list_position_cons_before, but that means any key argument
                   is called an awful lot more, so it's a space win but not
                   a time win. */
                result = Fcons (elt, result);
              }
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }
    }
  else
    {
      result = result_tail = Qnil;

      /* The standard `union' doesn't produce a "stable" union -- it
         iterates over the second list instead of the first one, and returns
         the values in backwards order.  According to the CLTL2
         documentation, `union' is not required to preserve the ordering of
         elements in any fashion; providing the functionality for a stable
         union is an XEmacs extension. */
      {
	GC_EXTERNAL_LIST_LOOP_2 (elt, liszt2)
          {
            if (NILP (list_position_cons_before (&ignore, elt, liszt1,
                                                 check_match, test_not_unboundp,
                                                 test, key, 1, Qzero, Qnil)))
              {
                if (NILP (result))
                  {
                    result = result_tail = Fcons (elt, Qnil);
                  }
                else
                  {
                    XSETCDR (result_tail, Fcons (elt, Qnil));
                    result_tail = XCDR (result_tail);
                  }
              }
          }
	END_GC_EXTERNAL_LIST_LOOP (elt);
      }

      result = NILP (result) ? liszt1 : nconc2 (Fcopy_list (liszt1), result);
    }

  UNGCPRO;

  return result;
}

DEFUN ("set-exclusive-or", Fset_exclusive_or, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-exclusive-or operation.

The result list contains all items that appear in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

See `union' for the meaning of :test, :test-not and :key.

A non-nil value for the :stable keyword, not specified by Common Lisp, means
return the items in the order they appear in LIST1, followed by the
remaining items in the order they appear in LIST2.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT STABLE)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object liszt1 = args[0], liszt2 = args[1];
  Lisp_Object result = Qnil, result_tail = Qnil, keyed = Qnil, ignore = Qnil;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_match = NULL, check_test = NULL;
  struct gcpro gcpro1, gcpro2;

  PARSE_KEYWORDS (Fset_exclusive_or, nargs, args, 4,
                  (test, key, test_not, stable), NULL);

  CHECK_LIST (liszt1);
  CHECK_LIST (liszt2);

  CHECK_KEY_ARGUMENT (key);

  if (NILP (liszt2))
    {
      return liszt1;
    }

  check_match = get_check_match_function (&test, test_not, Qnil, Qnil, key,
                                          &test_not_unboundp, &check_test);

  GCPRO2 (keyed, result);
  {
    GC_EXTERNAL_LIST_LOOP_2 (elt, liszt1)
      {
        keyed = KEY (key, elt);
        if (NILP (list_position_cons_before (&ignore, keyed, liszt2,
                                             check_test, test_not_unboundp,
                                             test, key, 0, Qzero, Qnil)))
          {
            if (NILP (stable))
              {
                result = Fcons (elt, result);
              }
            else if (NILP (result))
              {
                result = result_tail = Fcons (elt, Qnil);
              }
            else
              {
                XSETCDR (result_tail, Fcons (elt, Qnil));
                result_tail = XCDR (result_tail);
              }
          }
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }

  {
    GC_EXTERNAL_LIST_LOOP_2 (elt, liszt2)
      {
        if (NILP (list_position_cons_before (&ignore, elt, liszt1,
                                             check_match, test_not_unboundp,
                                             test, key, 1, Qzero, Qnil)))
          {
            if (NILP (stable))
              {
                result = Fcons (elt, result);
              }
            else if (NILP (result))
              {
                result = result_tail = Fcons (elt, Qnil);
              }
            else
              {
                XSETCDR (result_tail, Fcons (elt, Qnil));
                result_tail = XCDR (result_tail);
              }
          }
      }
    END_GC_EXTERNAL_LIST_LOOP (elt);
  }

  UNGCPRO;

  return result;
}

DEFUN ("nset-exclusive-or", Fnset_exclusive_or, 2, MANY, 0, /*
Combine LIST1 and LIST2 using a set-exclusive-or operation.

The result list contains all items that appear in exactly one of LIST1 and
LIST2.  This is a destructive function; it reuses the storage of LIST1 and
LIST2 whenever possible.

See `union' for the meaning of :test, :test-not and :key.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object liszt1 = args[0], liszt2 = args[1], elt = Qnil, tail = Qnil;
  Lisp_Object result = Qnil, tortoise_elt = Qnil, keyed = Qnil, swap;
  Lisp_Object prev_tail = Qnil, ignore = Qnil;
  Elemcount count;
  Boolint test_not_unboundp = 1;
  check_test_func_t check_match = NULL, check_test = NULL;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  PARSE_KEYWORDS (Fnset_exclusive_or, nargs, args, 4,
                  (test, key, test_not, stable), NULL);

  CHECK_LIST (liszt1);
  CHECK_LIST (liszt2);

  CHECK_KEY_ARGUMENT (key);

  if (NILP (liszt2))
    {
      return liszt1;
    }

  check_match = get_check_match_function (&test, test_not, Qnil, Qnil, key,
                                          &test_not_unboundp, &check_test);

  tortoise_elt = tail = liszt1, count = 0; 

  GCPRO4 (tail, keyed, result, tortoise_elt);

  while (CONSP (tail) ? (elt = XCAR (tail), 1) : NILP (tail) ? 0 :
         (signal_malformed_list_error (liszt1), 0))
    {
      keyed = KEY (key, elt);      
      if (NILP (list_position_cons_before (&ignore, keyed, liszt2,
                                           check_test, test_not_unboundp,
                                           test, key, 0, Qzero, Qnil)))
        {
          swap = XCDR (tail);

          if (NILP (prev_tail))
            {
              liszt1 = XCDR (tail);
            }
          else
            {
              XSETCDR (prev_tail, swap);
            }

          XSETCDR (tail, result);
          result = tail;
          tail = swap;

          /* List is definitely not circular now! */
          count = 0;
        }
      else
        {
          prev_tail = tail;
          tail = XCDR (tail);
        }

      if (count++ < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

      if (count & 1)
        {
          tortoise_elt = XCDR (tortoise_elt);
        }

      if (EQ (elt, tortoise_elt))
        {
          signal_circular_list_error (liszt1);
        }
    }

  tortoise_elt = tail = liszt2, count = 0; 

  while (CONSP (tail) ? (elt = XCAR (tail), 1) : NILP (tail) ? 0 :
         (signal_malformed_list_error (liszt2), 0))
    {
      /* Need to leave the key calculation to list_position_cons_before(). */
      if (NILP (list_position_cons_before (&ignore, elt, liszt1,
                                           check_match, test_not_unboundp,
                                           test, key, 1, Qzero, Qnil)))
        {
          swap = XCDR (tail);
          XSETCDR (tail, result);
          result = tail;
          tail = swap;
          count = 0;
        }
      else
        {
          tail = XCDR (tail);
        }

      if (count++ < CIRCULAR_LIST_SUSPICION_LENGTH) continue;

      if (count & 1)
        {
          tortoise_elt = XCDR (tortoise_elt);
        }

      if (EQ (elt, tortoise_elt))
        {
          signal_circular_list_error (liszt1);
        }
    }

  UNGCPRO;

  return result;
}

void
syms_of_sequence (void)
{
  DEFSYMBOL (Qstring_lessp);
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
  DEFSYMBOL (Qposition);
  DEFSYMBOL (Qfind);
  defsymbol (&QdeleteX, "delete*");
  defsymbol (&QremoveX, "remove*");

  DEFSYMBOL (Qmapconcat);
  defsymbol (&QmapcarX, "mapcar*");
  DEFSYMBOL (Qmapvector);
  DEFSYMBOL (Qmapcan);
  DEFSYMBOL (Qmapc);
  DEFSYMBOL (Qmap);
  DEFSYMBOL (Qmap_into);
  DEFSYMBOL (Qsome);
  DEFSYMBOL (Qevery);
  DEFSYMBOL (Qnsubstitute);
  DEFSYMBOL (Qdelete_duplicates);
  DEFSYMBOL (Qsubstitute);
  DEFSYMBOL (Qmismatch);
  DEFSYMBOL (Qintersection);
  DEFSYMBOL (Qnintersection);
  DEFSYMBOL (Qsubsetp);
  DEFSYMBOL (Qcar_less_than_car);
  DEFSYMBOL (Qset_difference);
  DEFSYMBOL (Qnset_difference);
  DEFSYMBOL (Qnunion);

  DEFKEYWORD (Q_from_end);
  DEFKEYWORD (Q_initial_value);
  DEFKEYWORD (Q_start1);
  DEFKEYWORD (Q_start2);
  DEFKEYWORD (Q_end1);
  DEFKEYWORD (Q_end2);
  defkeyword (&Q_if_, ":if");
  DEFKEYWORD (Q_if_not);
  DEFKEYWORD (Q_test_not);
  DEFKEYWORD (Q_count);
  DEFKEYWORD (Q_stable);
  DEFKEYWORD (Q_descend_structures);

  DEFSUBR (Flength);
  DEFSUBR (Fcount);
  DEFSUBR (Fsubseq);
  DEFSUBR (Felt);
  DEFSUBR (Fcopy_tree);
  DEFSUBR (Fmember);
  DEFSUBR (Fmemq);
  DEFSUBR (FmemberX);
  DEFSUBR (Fadjoin);
  DEFSUBR (Fassoc);
  DEFSUBR (Fassq);
  DEFSUBR (FassocX);
  DEFSUBR (Frassoc);
  DEFSUBR (Frassq);
  DEFSUBR (FrassocX);
  DEFSUBR (Fposition);
  DEFSUBR (Ffind);
  DEFSUBR (FdeleteX);
  DEFSUBR (FremoveX);
  DEFSUBR (Fdelete_duplicates);
  DEFSUBR (Fremove_duplicates);
  DEFSUBR (Fnreverse);
  DEFSUBR (Freverse);
  DEFSUBR (Fmerge);
  DEFSUBR (FsortX);
  DEFSUBR (Ffill);
  DEFSUBR (Fmapconcat);
  DEFSUBR (FmapcarX);
  DEFSUBR (Fmapvector);
  DEFSUBR (Fmapcan);
  DEFSUBR (Fmapc);
  Ffset (intern ("mapc-internal"), Qmapc);
  Ffset (intern ("mapcar"), QmapcarX);
  DEFSUBR (Fmap);
  DEFSUBR (Fmap_into);
  DEFSUBR (Fsome);
  DEFSUBR (Fevery);
  DEFSUBR (Freduce);
  DEFSUBR (Freplace);
  DEFSUBR (Fnsubstitute);
  DEFSUBR (Fsubstitute);
  DEFSUBR (Fsublis);
  DEFSUBR (Fnsublis);
  DEFSUBR (Fsubst);
  DEFSUBR (Fnsubst);
  DEFSUBR (Ftree_equal);
  DEFSUBR (Fmismatch);
  DEFSUBR (Fsearch);
  DEFSUBR (Fintersection);
  DEFSUBR (Fnintersection);
  DEFSUBR (Fsubsetp);
  DEFSUBR (Fset_difference);
  DEFSUBR (Fnset_difference);
  DEFSUBR (Fnunion);
  DEFSUBR (Funion);
  DEFSUBR (Fset_exclusive_or);
  DEFSUBR (Fnset_exclusive_or);
}
