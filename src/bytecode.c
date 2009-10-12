/* Execution of byte code produced by bytecomp.el.
   Implementation of compiled-function objects.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 1995, 2002 Ben Wing.

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


/* Authorship:

   FSF: long ago.

hacked on by jwz@jwz.org 1991-06
  o  added a compile-time switch to turn on simple sanity checking;
  o  put back the obsolete byte-codes for error-detection;
  o  added a new instruction, unbind_all, which I will use for
     tail-recursion elimination;
  o  made temp_output_buffer_show be called with the right number
     of args;
  o  made the new bytecodes be called with args in the right order;
  o  added metering support.

by Hallvard:
  o  added relative jump instructions;
  o  all conditionals now only do QUIT if they jump.

   Ben Wing: some changes for Mule, 1995-06.

   Martin Buchholz: performance hacking, 1998-09.
   See Internals Manual, Evaluation.
 */

#include <config.h>
#include "lisp.h"
#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "opaque.h"
#include "syntax.h"
#include "window.h"

#ifdef NEW_GC
static Lisp_Object
make_compiled_function_args (int totalargs)
{
  Lisp_Compiled_Function_Args *args;
  args = (Lisp_Compiled_Function_Args *) 
    alloc_lrecord 
    (FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Compiled_Function_Args, 
				   Lisp_Object, args, totalargs),
     &lrecord_compiled_function_args);
  args->size = totalargs;
  return wrap_compiled_function_args (args);
}

static Bytecount
size_compiled_function_args (const void *lheader)
{
  return FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Compiled_Function_Args, 
				       Lisp_Object, args,
				       ((Lisp_Compiled_Function_Args *) 
					lheader)->size);
}

static const struct memory_description compiled_function_args_description[] = {
  { XD_LONG,              offsetof (Lisp_Compiled_Function_Args, size) },
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Compiled_Function_Args, args), 
    XD_INDIRECT(0, 0) },
  { XD_END }
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("compiled-function-args",
					compiled_function_args,
					1, /*dumpable-flag*/
					0, 0, 0, 0, 0,
					compiled_function_args_description,
					size_compiled_function_args,
					Lisp_Compiled_Function_Args);
#endif /* NEW_GC */

EXFUN (Ffetch_bytecode, 1);

Lisp_Object Qbyte_code, Qcompiled_functionp, Qinvalid_byte_code;

enum Opcode /* Byte codes */
{
  Bvarref  		= 010,
  Bvarset  		= 020,
  Bvarbind 		= 030,
  Bcall    		= 040,
  Bunbind  		= 050,

  Bnth     		= 070,
  Bsymbolp 		= 071,
  Bconsp   		= 072,
  Bstringp 		= 073,
  Blistp   		= 074,
  Bold_eq  		= 075,
  Bold_memq 		= 076,
  Bnot    		= 077,
  Bcar    		= 0100,
  Bcdr 	  		= 0101,
  Bcons   		= 0102,
  Blist1  		= 0103,
  Blist2  		= 0104,
  Blist3  		= 0105,
  Blist4  		= 0106,
  Blength 		= 0107,
  Baref   		= 0110,
  Baset   		= 0111,
  Bsymbol_value 	= 0112,
  Bsymbol_function 	= 0113,
  Bset    		= 0114,
  Bfset   		= 0115,
  Bget    		= 0116,
  Bsubstring 		= 0117,
  Bconcat2 		= 0120,
  Bconcat3 		= 0121,
  Bconcat4 		= 0122,
  Bsub1 		= 0123,
  Badd1 		= 0124,
  Beqlsign 		= 0125,
  Bgtr 			= 0126,
  Blss 			= 0127,
  Bleq 			= 0130,
  Bgeq 			= 0131,
  Bdiff 		= 0132,
  Bnegate 		= 0133,
  Bplus 		= 0134,
  Bmax 			= 0135,
  Bmin 			= 0136,
  Bmult 		= 0137,

  Bpoint 		= 0140,
  Beq 			= 0141, /* was Bmark,
				   but no longer generated as of v18 */
  Bgoto_char 		= 0142,
  Binsert 		= 0143,
  Bpoint_max 		= 0144,
  Bpoint_min 		= 0145,
  Bchar_after 		= 0146,
  Bfollowing_char 	= 0147,
  Bpreceding_char 	= 0150,
  Bcurrent_column 	= 0151,
  Bindent_to 		= 0152,
  Bequal 		= 0153, /* was Bscan_buffer,
				   but no longer generated as of v18 */
  Beolp 		= 0154,
  Beobp 		= 0155,
  Bbolp 		= 0156,
  Bbobp 		= 0157,
  Bcurrent_buffer 	= 0160,
  Bset_buffer 		= 0161,
  Bsave_current_buffer 	= 0162, /* was Bread_char,
				   but no longer generated as of v19 */
  Bmemq 		= 0163, /* was Bset_mark,
				   but no longer generated as of v18 */
  Binteractive_p 	= 0164, /* Needed since interactive-p takes
				   unevalled args */
  Bforward_char 	= 0165,
  Bforward_word 	= 0166,
  Bskip_chars_forward 	= 0167,
  Bskip_chars_backward 	= 0170,
  Bforward_line 	= 0171,
  Bchar_syntax 		= 0172,
  Bbuffer_substring 	= 0173,
  Bdelete_region 	= 0174,
  Bnarrow_to_region 	= 0175,
  Bwiden 		= 0176,
  Bend_of_line 		= 0177,

  Bconstant2 		= 0201,
  Bgoto 		= 0202,
  Bgotoifnil 		= 0203,
  Bgotoifnonnil 	= 0204,
  Bgotoifnilelsepop 	= 0205,
  Bgotoifnonnilelsepop 	= 0206,
  Breturn 		= 0207,
  Bdiscard 		= 0210,
  Bdup 			= 0211,

  Bsave_excursion 	= 0212,
  Bsave_window_excursion= 0213,
  Bsave_restriction 	= 0214,
  Bcatch 		= 0215,

  Bunwind_protect 	= 0216,
  Bcondition_case 	= 0217,
  Btemp_output_buffer_setup = 0220,
  Btemp_output_buffer_show  = 0221,

  Bunbind_all 		= 0222,

  Bset_marker 		= 0223,
  Bmatch_beginning 	= 0224,
  Bmatch_end 		= 0225,
  Bupcase 		= 0226,
  Bdowncase 		= 0227,

  Bstring_equal 	= 0230,
  Bstring_lessp     	= 0231,
  Bold_equal 	 	= 0232,
  Bnthcdr 	 	= 0233,
  Belt 		 	= 0234,
  Bold_member 	 	= 0235,
  Bold_assq 	 	= 0236,
  Bnreverse 	 	= 0237,
  Bsetcar 	 	= 0240,
  Bsetcdr 	 	= 0241,
  Bcar_safe 	 	= 0242,
  Bcdr_safe 	 	= 0243,
  Bnconc 	 	= 0244,
  Bquo 		 	= 0245,
  Brem 		 	= 0246,
  Bnumberp 	 	= 0247,
  Bintegerp 	 	= 0250,

  BRgoto 		= 0252,
  BRgotoifnil 		= 0253,
  BRgotoifnonnil 	= 0254,
  BRgotoifnilelsepop 	= 0255,
  BRgotoifnonnilelsepop = 0256,

  BlistN 		= 0257,
  BconcatN 		= 0260,
  BinsertN 		= 0261,

  Bbind_multiple_value_limits   = 0262,         /* New in 21.5. */
  Bmultiple_value_list_internal = 0263,         /* New in 21.5. */
  Bmultiple_value_call          = 0264,         /* New in 21.5. */
  Bthrow                        = 0265,         /* New in 21.5. */

  Bmember 		= 0266, /* new in v20 */
  Bassq 		= 0267, /* new in v20 */

  Bconstant 		= 0300
};
typedef enum Opcode Opcode;


Lisp_Object * execute_rare_opcode (Lisp_Object *stack_ptr,
				   const Opbyte *program_ptr,
				   Opcode opcode);

/* Define BYTE_CODE_METER to enable generation of a byte-op usage histogram.
   This isn't defined in FSF Emacs and isn't defined in XEmacs v19. */
/* #define BYTE_CODE_METER */


#ifdef BYTE_CODE_METER

Lisp_Object Vbyte_code_meter, Qbyte_code_meter;
int byte_metering_on;

static void
meter_code (Opcode prev_opcode, Opcode this_opcode)
{
  if (byte_metering_on)
    {
      Lisp_Object *p = XVECTOR_DATA (XVECTOR_DATA (Vbyte_code_meter)[this_opcode]);
      p[0] = INT_PLUS1 (p[0]);
      if (prev_opcode)
	p[prev_opcode] = INT_PLUS1 (p[prev_opcode]);
    }
}

#endif /* BYTE_CODE_METER */


static Lisp_Object
bytecode_negate (Lisp_Object obj)
{
 retry:

  if (INTP    (obj)) return make_integer (- XINT (obj));
  if (FLOATP  (obj)) return make_float (- XFLOAT_DATA (obj));
  if (CHARP   (obj)) return make_integer (- ((int) XCHAR (obj)));
  if (MARKERP (obj)) return make_integer (- ((int) marker_position (obj)));
#ifdef HAVE_BIGNUM
  if (BIGNUMP (obj)) BIGNUM_ARITH_RETURN (obj, neg);
#endif
#ifdef HAVE_RATIO
  if (RATIOP (obj)) RATIO_ARITH_RETURN (obj, neg);
#endif
#ifdef HAVE_BIGFLOAT
  if (BIGFLOATP (obj)) BIGFLOAT_ARITH_RETURN (obj, neg);
#endif

  obj = wrong_type_argument (Qnumber_char_or_marker_p, obj);
  goto retry;
}

static Lisp_Object
bytecode_nreverse (Lisp_Object list)
{
  REGISTER Lisp_Object prev = Qnil;
  REGISTER Lisp_Object tail = list;

  while (!NILP (tail))
    {
      REGISTER Lisp_Object next;
      CHECK_CONS (tail);
      next = XCDR (tail);
      XCDR (tail) = prev;
      prev = tail;
      tail = next;
    }
  return prev;
}


/* We have our own two-argument versions of various arithmetic ops.
   Only two-argument arithmetic operations have their own byte codes. */
static int
bytecode_arithcompare (Lisp_Object obj1, Lisp_Object obj2)
{
#ifdef WITH_NUMBER_TYPES
  switch (promote_args (&obj1, &obj2))
    {
    case FIXNUM_T:
      {
	EMACS_INT ival1 = XREALINT (obj1), ival2 = XREALINT (obj2);
	return ival1 < ival2 ? -1 : ival1 > ival2 ? 1 : 0;
      }
#ifdef HAVE_BIGNUM
    case BIGNUM_T:
      return bignum_cmp (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2));
#endif
#ifdef HAVE_RATIO
    case RATIO_T:
      return ratio_cmp (XRATIO_DATA (obj1), XRATIO_DATA (obj2));
#endif
#ifdef HAVE_BIGFLOAT
    case BIGFLOAT_T:
      return bigfloat_cmp (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2));
#endif
    default: /* FLOAT_T */
      {
	double dval1 = XFLOAT_DATA (obj1), dval2 = XFLOAT_DATA (obj2);
	return dval1 < dval2 ? -1 : dval1 > dval2 ? 1 : 0;
      }
    }
#else /* !WITH_NUMBER_TYPES */
  retry:

  {
    EMACS_INT ival1, ival2;

    if      (INTP    (obj1)) ival1 = XINT  (obj1);
    else if (CHARP   (obj1)) ival1 = XCHAR (obj1);
    else if (MARKERP (obj1)) ival1 = marker_position (obj1);
    else goto arithcompare_float;

    if      (INTP    (obj2)) ival2 = XINT  (obj2);
    else if (CHARP   (obj2)) ival2 = XCHAR (obj2);
    else if (MARKERP (obj2)) ival2 = marker_position (obj2);
    else goto arithcompare_float;

    return ival1 < ival2 ? -1 : ival1 > ival2 ? 1 : 0;
  }

 arithcompare_float:

  {
    double dval1, dval2;

    if      (FLOATP  (obj1)) dval1 = XFLOAT_DATA (obj1);
    else if (INTP    (obj1)) dval1 = (double) XINT  (obj1);
    else if (CHARP   (obj1)) dval1 = (double) XCHAR (obj1);
    else if (MARKERP (obj1)) dval1 = (double) marker_position (obj1);
    else
      {
	obj1 = wrong_type_argument (Qnumber_char_or_marker_p, obj1);
	goto retry;
      }

    if      (FLOATP  (obj2)) dval2 = XFLOAT_DATA (obj2);
    else if (INTP    (obj2)) dval2 = (double) XINT  (obj2);
    else if (CHARP   (obj2)) dval2 = (double) XCHAR (obj2);
    else if (MARKERP (obj2)) dval2 = (double) marker_position (obj2);
    else
      {
	obj2 = wrong_type_argument (Qnumber_char_or_marker_p, obj2);
	goto retry;
      }

    return dval1 < dval2 ? -1 : dval1 > dval2 ? 1 : 0;
  }
#endif /* WITH_NUMBER_TYPES */
}

static Lisp_Object
bytecode_arithop (Lisp_Object obj1, Lisp_Object obj2, Opcode opcode)
{
#ifdef WITH_NUMBER_TYPES
  switch (promote_args (&obj1, &obj2))
    {
    case FIXNUM_T:
      {
	EMACS_INT ival1 = XREALINT (obj1), ival2 = XREALINT (obj2);
	switch (opcode)
	  {
	  case Bplus: ival1 += ival2; break;
	  case Bdiff: ival1 -= ival2; break;
	  case Bmult:
#ifdef HAVE_BIGNUM
	    /* Due to potential overflow, we compute using bignums */
	    bignum_set_long (scratch_bignum, ival1);
	    bignum_set_long (scratch_bignum2, ival2);
	    bignum_mul (scratch_bignum, scratch_bignum, scratch_bignum2);
	    return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else
	    ival1 *= ival2; break;
#endif
	  case Bquo:
	    if (ival2 == 0)
	      signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	    ival1 /= ival2;
	    break;
	  case Bmax:  if (ival1 < ival2) ival1 = ival2; break;
	  case Bmin:  if (ival1 > ival2) ival1 = ival2; break;
	  }
	return make_integer (ival1);
      }
#ifdef HAVE_BIGNUM
    case BIGNUM_T:
      switch (opcode)
	{
	case Bplus:
	  bignum_add (scratch_bignum, XBIGNUM_DATA (obj1),
		      XBIGNUM_DATA (obj2));
	  break;
	case Bdiff:
	  bignum_sub (scratch_bignum, XBIGNUM_DATA (obj1),
		      XBIGNUM_DATA (obj2));
	  break;
	case Bmult:
	  bignum_mul (scratch_bignum, XBIGNUM_DATA (obj1),
		      XBIGNUM_DATA (obj2));
	  break;
	case Bquo:
	  if (bignum_sign (XBIGNUM_DATA (obj2)) == 0)
	    signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	  bignum_div (scratch_bignum, XBIGNUM_DATA (obj1),
		      XBIGNUM_DATA (obj2));
	  break;
	case Bmax:
	  return bignum_gt (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2))
	    ? obj1 : obj2;
	case Bmin:
	  return bignum_lt (XBIGNUM_DATA (obj1), XBIGNUM_DATA (obj2))
	    ? obj1 : obj2;
	}
      return Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#endif
#ifdef HAVE_RATIO
    case RATIO_T:
      switch (opcode)
	{
	case Bplus:
	  ratio_add (scratch_ratio, XRATIO_DATA (obj1), XRATIO_DATA (obj2));
	  break;
	case Bdiff:
	  ratio_sub (scratch_ratio, XRATIO_DATA (obj1), XRATIO_DATA (obj2));
	  break;
	case Bmult:
	  ratio_mul (scratch_ratio, XRATIO_DATA (obj1), XRATIO_DATA (obj2));
	  break;
	case Bquo:
	  if (ratio_sign (XRATIO_DATA (obj2)) == 0)
	    signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	  ratio_div (scratch_ratio, XRATIO_DATA (obj1), XRATIO_DATA (obj2));
	  break;
	case Bmax:
	  return ratio_gt (XRATIO_DATA (obj1), XRATIO_DATA (obj2))
	    ? obj1 : obj2;
	case Bmin:
	  return ratio_lt (XRATIO_DATA (obj1), XRATIO_DATA (obj2))
	    ? obj1 : obj2;
	}
      return make_ratio_rt (scratch_ratio);
#endif
#ifdef HAVE_BIGFLOAT
    case BIGFLOAT_T:
      bigfloat_set_prec (scratch_bigfloat, max (XBIGFLOAT_GET_PREC (obj1),
						XBIGFLOAT_GET_PREC (obj2)));
      switch (opcode)
	{
	case Bplus:
	  bigfloat_add (scratch_bigfloat, XBIGFLOAT_DATA (obj1),
			XBIGFLOAT_DATA (obj2));
	  break;
	case Bdiff:
	  bigfloat_sub (scratch_bigfloat, XBIGFLOAT_DATA (obj1),
			XBIGFLOAT_DATA (obj2));
	  break;
	case Bmult:
	  bigfloat_mul (scratch_bigfloat, XBIGFLOAT_DATA (obj1),
			XBIGFLOAT_DATA (obj2));
	  break;
	case Bquo:
	  if (bigfloat_sign (XBIGFLOAT_DATA (obj2)) == 0)
	    signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	  bigfloat_div (scratch_bigfloat, XBIGFLOAT_DATA (obj1),
			XBIGFLOAT_DATA (obj2));
	  break;
	case Bmax:
	  return bigfloat_gt (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2))
	    ? obj1 : obj2;
	case Bmin:
	  return bigfloat_lt (XBIGFLOAT_DATA (obj1), XBIGFLOAT_DATA (obj2))
	    ? obj1 : obj2;
	}
      return make_bigfloat_bf (scratch_bigfloat);
#endif
    default: /* FLOAT_T */
      {
	double dval1 = XFLOAT_DATA (obj1), dval2 = XFLOAT_DATA (obj2);
	switch (opcode)
	  {
	  case Bplus: dval1 += dval2; break;
	  case Bdiff: dval1 -= dval2; break;
	  case Bmult: dval1 *= dval2; break;
	  case Bquo:
	    if (dval2 == 0.0)
	      signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	    dval1 /= dval2;
	    break;
	  case Bmax:  if (dval1 < dval2) dval1 = dval2; break;
	  case Bmin:  if (dval1 > dval2) dval1 = dval2; break;
	  }
	return make_float (dval1);
      }
    }
#else /* !WITH_NUMBER_TYPES */
  EMACS_INT ival1, ival2;
  int float_p;

 retry:

  float_p = 0;

  if      (INTP    (obj1)) ival1 = XINT  (obj1);
  else if (CHARP   (obj1)) ival1 = XCHAR (obj1);
  else if (MARKERP (obj1)) ival1 = marker_position (obj1);
  else if (FLOATP  (obj1)) ival1 = 0, float_p = 1;
  else
    {
      obj1 = wrong_type_argument (Qnumber_char_or_marker_p, obj1);
      goto retry;
    }

  if      (INTP    (obj2)) ival2 = XINT  (obj2);
  else if (CHARP   (obj2)) ival2 = XCHAR (obj2);
  else if (MARKERP (obj2)) ival2 = marker_position (obj2);
  else if (FLOATP  (obj2)) ival2 = 0, float_p = 1;
  else
    {
      obj2 = wrong_type_argument (Qnumber_char_or_marker_p, obj2);
      goto retry;
    }

  if (!float_p)
    {
      switch (opcode)
	{
	case Bplus: ival1 += ival2; break;
	case Bdiff: ival1 -= ival2; break;
	case Bmult: ival1 *= ival2; break;
	case Bquo:
	  if (ival2 == 0)
	    signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	  ival1 /= ival2;
	  break;
	case Bmax:  if (ival1 < ival2) ival1 = ival2; break;
	case Bmin:  if (ival1 > ival2) ival1 = ival2; break;
	}
      return make_int (ival1);
    }
  else
    {
      double dval1 = FLOATP (obj1) ? XFLOAT_DATA (obj1) : (double) ival1;
      double dval2 = FLOATP (obj2) ? XFLOAT_DATA (obj2) : (double) ival2;
      switch (opcode)
	{
	case Bplus: dval1 += dval2; break;
	case Bdiff: dval1 -= dval2; break;
	case Bmult: dval1 *= dval2; break;
	case Bquo:
	  if (dval2 == 0)
	    signal_error_2 (Qarith_error, "division by zero", obj1, obj2);
	  dval1 /= dval2;
	  break;
	case Bmax:  if (dval1 < dval2) dval1 = dval2; break;
	case Bmin:  if (dval1 > dval2) dval1 = dval2; break;
	}
      return make_float (dval1);
    }
#endif /* WITH_NUMBER_TYPES */
}


/* Read next uint8 from the instruction stream. */
#define READ_UINT_1 ((unsigned int) (unsigned char) *program_ptr++)

/* Read next uint16 from the instruction stream. */
#define READ_UINT_2						\
  (program_ptr += 2,						\
   (((unsigned int) (unsigned char) program_ptr[-1]) * 256 +	\
    ((unsigned int) (unsigned char) program_ptr[-2])))

/* Read next int8 from the instruction stream. */
#define READ_INT_1 ((int) (signed char) *program_ptr++)

/* Read next int16 from the instruction stream. */
#define READ_INT_2					\
  (program_ptr += 2,					\
   (((int) (  signed char) program_ptr[-1]) * 256 +	\
    ((int) (unsigned char) program_ptr[-2])))

/* Read next int8 from instruction stream; don't advance program_pointer */
#define PEEK_INT_1 ((int) (signed char) program_ptr[0])

/* Read next int16 from instruction stream; don't advance program_pointer */
#define PEEK_INT_2					\
  ((((int) (  signed char) program_ptr[1]) * 256) |	\
    ((int) (unsigned char) program_ptr[0]))

/* Do relative jumps from the current location.
   We only do a QUIT if we jump backwards, for efficiency.
   No infloops without backward jumps! */
#define JUMP_RELATIVE(jump) do {	\
  int JR_jump = (jump);			\
  if (JR_jump < 0) QUIT;		\
  program_ptr += JR_jump;		\
} while (0)

#define JUMP  JUMP_RELATIVE (PEEK_INT_2)
#define JUMPR JUMP_RELATIVE (PEEK_INT_1)

#define JUMP_NEXT  ((void) (program_ptr += 2))
#define JUMPR_NEXT ((void) (program_ptr += 1))

/* Push x onto the execution stack. */
#define PUSH(x) (*++stack_ptr = (x))

/* Pop a value, which may be multiple, off the execution stack. */
#define POP_WITH_MULTIPLE_VALUES (*stack_ptr--)

/* Pop a value off the execution stack, treating multiple values as single. */
#define POP (IGNORE_MULTIPLE_VALUES (POP_WITH_MULTIPLE_VALUES))

#define DISCARD_PRESERVING_MULTIPLE_VALUES(n) (stack_ptr -= (n))

/* Discard n values from the execution stack.  */
#define DISCARD(n) do {                                         \
    if (1 != multiple_value_current_limit)                      \
      {                                                         \
        int i, en = n;                                          \
        for (i = 0; i < en; i++)                                \
          {                                                     \
            *stack_ptr = ignore_multiple_values (*stack_ptr);   \
            stack_ptr--;                                        \
          }                                                     \
      }                                                         \
    else                                                        \
      {                                                         \
        stack_ptr -= (n);                                       \
      }                                                         \
  } while (0)

/* Get the value, which may be multiple, at the top of the execution stack;
   and leave it there. */
#define TOP_WITH_MULTIPLE_VALUES (*stack_ptr)

#define TOP_ADDRESS (stack_ptr)

/* Get the value which is at the top of the execution stack,
   but don't pop it. */
#define TOP (IGNORE_MULTIPLE_VALUES (TOP_WITH_MULTIPLE_VALUES))

#define TOP_LVALUE (*stack_ptr)



/* See comment before the big switch in execute_optimized_program(). */
#define GCPRO_STACK  (gcpro1.nvars = stack_ptr - stack_beg)

/* The actual interpreter for byte code.
   This function has been seriously optimized for performance.
   Don't change the constructs unless you are willing to do
   real benchmarking and profiling work -- martin */


Lisp_Object
execute_optimized_program (const Opbyte *program,
			   int stack_depth,
			   Lisp_Object *constants_data)
{
  /* This function can GC */
  REGISTER const Opbyte *program_ptr = (Opbyte *) program;
  Lisp_Object *stack_beg = alloca_array (Lisp_Object, stack_depth + 1);
  REGISTER Lisp_Object *stack_ptr = stack_beg;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

#ifdef BYTE_CODE_METER
  Opcode this_opcode = 0;
  Opcode prev_opcode;
#endif

#ifdef ERROR_CHECK_BYTE_CODE
  Lisp_Object *stack_end = stack_beg + stack_depth;
#endif

  /* We used to GCPRO the whole interpreter stack before entering this while
     loop (21.5.14 and before), but that interferes with collection of weakly
     referenced objects.  Although strictly speaking there's no promise that
     weak references will disappear by any given point in time, they should
     be collected at the first opportunity.  Waiting until exit from the
     function caused test failures because "stale" objects "above" the top of
     the stack were still GCPROed, and they were not getting collected until
     after exit from the (byte-compiled) test!

     Now the idea is to dynamically adjust the array of GCPROed objects to
     include only the "active" region of the stack.

     We use the "GCPRO1 the array base and set the nvars member" method.  It
     would be slightly inefficient but correct to use GCPRO1_ARRAY here.  It
     would just redundantly set nvars.
     #### Maybe it would be clearer to use GCPRO1_ARRAY and do GCPRO_STACK
     after the switch?

     GCPRO_STACK is something of a misnomer, because it suggests that a
     struct gcpro is initialized each time.  This is false; only the nvars
     member of a single struct gcpro is being adjusted.  This works because
     each time a new object is assigned to a stack location, the old object
     loses its reference and is effectively UNGCPROed, and the new object is
     automatically GCPROed as long as nvars is correct.  Only when we
     return from the interpreter do we need to finalize the struct gcpro
     itself, and that's done at case Breturn.
  */
  GCPRO1 (stack_ptr[1]);

  while (1)
    {
      REGISTER Opcode opcode = (Opcode) READ_UINT_1;

      GCPRO_STACK;		/* Get nvars right before maybe signaling. */
#ifdef ERROR_CHECK_BYTE_CODE
      if (stack_ptr > stack_end)
	stack_overflow ("byte code stack overflow", Qunbound);
      if (stack_ptr < stack_beg)
	stack_overflow ("byte code stack underflow", Qunbound);
#endif

#ifdef BYTE_CODE_METER
      prev_opcode = this_opcode;
      this_opcode = opcode;
      meter_code (prev_opcode, this_opcode);
#endif

      switch (opcode)
	{
	  REGISTER int n;

	default:
	  if (opcode >= Bconstant)
	    PUSH (constants_data[opcode - Bconstant]);
	  else
	    {
	      /* We're not sure what these do, so better safe than sorry. */
	      /* GCPRO_STACK; */
	      stack_ptr = execute_rare_opcode (stack_ptr, program_ptr, opcode);
	    }
	  break;

	case Bvarref:
	case Bvarref+1:
	case Bvarref+2:
	case Bvarref+3:
	case Bvarref+4:
	case Bvarref+5: n = opcode - Bvarref; goto do_varref;
	case Bvarref+7: n = READ_UINT_2;      goto do_varref;
	case Bvarref+6: n = READ_UINT_1; /* most common */
	do_varref:
	{
	  Lisp_Object symbol = constants_data[n];
	  Lisp_Object value = XSYMBOL (symbol)->value;
	  if (SYMBOL_VALUE_MAGIC_P (value))
	    /* I GCPRO_STACKed Fsymbol_value elsewhere, but I dunno why. */
	    /* GCPRO_STACK; */
	    value = Fsymbol_value (symbol);
	  PUSH (value);
	  break;
	}

	case Bvarset:
	case Bvarset+1:
	case Bvarset+2:
	case Bvarset+3:
	case Bvarset+4:
	case Bvarset+5: n = opcode - Bvarset; goto do_varset;
	case Bvarset+7: n = READ_UINT_2;      goto do_varset;
	case Bvarset+6: n = READ_UINT_1; /* most common */
	do_varset:
	{
	  Lisp_Object symbol = constants_data[n];
	  Lisp_Symbol *symbol_ptr = XSYMBOL (symbol);
	  Lisp_Object old_value = symbol_ptr->value;
	  Lisp_Object new_value = POP;
	  if (!SYMBOL_VALUE_MAGIC_P (old_value) || UNBOUNDP (old_value))
	    symbol_ptr->value = new_value;
	  else {
	    /* Fset may call magic handlers */
	    /* GCPRO_STACK; */
	    Fset (symbol, new_value);
	  }
	    
	  break;
	}

	case Bvarbind:
	case Bvarbind+1:
	case Bvarbind+2:
	case Bvarbind+3:
	case Bvarbind+4:
	case Bvarbind+5: n = opcode - Bvarbind; goto do_varbind;
	case Bvarbind+7: n = READ_UINT_2;       goto do_varbind;
	case Bvarbind+6: n = READ_UINT_1; /* most common */
	do_varbind:
	{
	  Lisp_Object symbol = constants_data[n];
	  Lisp_Symbol *symbol_ptr = XSYMBOL (symbol);
	  Lisp_Object old_value = symbol_ptr->value;
	  Lisp_Object new_value = POP;
	  if (!SYMBOL_VALUE_MAGIC_P (old_value) || UNBOUNDP (old_value))
	    {
	      specpdl_ptr->symbol    = symbol;
	      specpdl_ptr->old_value = old_value;
	      specpdl_ptr->func      = 0;
	      specpdl_ptr++;
	      specpdl_depth_counter++;

	      symbol_ptr->value = new_value;

#ifdef ERROR_CHECK_CATCH
	      check_specbind_stack_sanity ();
#endif
	    }
	  else
	    {
	      /* does an Fset, may call magic handlers */
	      /* GCPRO_STACK; */
	      specbind_magic (symbol, new_value);
	    }
	  break;
	}

	case Bcall:
	case Bcall+1:
	case Bcall+2:
	case Bcall+3:
	case Bcall+4:
	case Bcall+5:
	case Bcall+6:
	case Bcall+7:
	  n = (opcode <  Bcall+6 ? opcode - Bcall :
	       opcode == Bcall+6 ? READ_UINT_1 : READ_UINT_2);
	  /* #### Shouldn't this be just before the Ffuncall?
	     Neither Fget nor Fput can GC. */
	  /* GCPRO_STACK; */
	  DISCARD (n);
#ifdef BYTE_CODE_METER
	  if (byte_metering_on && SYMBOLP (TOP))
	    {
	      Lisp_Object val = Fget (TOP, Qbyte_code_meter, Qnil);
	      if (INTP (val))
		Fput (TOP, Qbyte_code_meter, make_int (XINT (val) + 1));
	    }
#endif
          TOP_LVALUE = TOP; /* Ignore multiple values. */
	  TOP_LVALUE = Ffuncall (n + 1, TOP_ADDRESS);
	  break;

	case Bunbind:
	case Bunbind+1:
	case Bunbind+2:
	case Bunbind+3:
	case Bunbind+4:
	case Bunbind+5:
	case Bunbind+6:
	case Bunbind+7:
	  UNBIND_TO (specpdl_depth() -
		     (opcode <  Bunbind+6 ? opcode-Bunbind :
		      opcode == Bunbind+6 ? READ_UINT_1 : READ_UINT_2));
	  break;


	case Bgoto:
	  JUMP;
	  break;

	case Bgotoifnil:
	  if (NILP (POP))
	    JUMP;
	  else
	    JUMP_NEXT;
	  break;

	case Bgotoifnonnil:
	  if (!NILP (POP))
	    JUMP;
	  else
	    JUMP_NEXT;
	  break;

	case Bgotoifnilelsepop:
	  /* Discard any multiple value: */
	  if (NILP (TOP_LVALUE = TOP))
	    JUMP;
	  else
	    {
	      DISCARD (1);
	      JUMP_NEXT;
	    }
	  break;

	case Bgotoifnonnilelsepop:
	  /* Discard any multiple value: */
	  if (!NILP (TOP_LVALUE = TOP))
	    JUMP;
	  else
	    {
	      DISCARD (1);
	      JUMP_NEXT;
	    }
	  break;


	case BRgoto:
	  JUMPR;
	  break;

	case BRgotoifnil:
	  if (NILP (POP))
	    JUMPR;
	  else
	    JUMPR_NEXT;
	  break;

	case BRgotoifnonnil:
	  if (!NILP (POP))
	    JUMPR;
	  else
	    JUMPR_NEXT;
	  break;

	case BRgotoifnilelsepop:
	  if (NILP (TOP_LVALUE = TOP))
	    JUMPR;
	  else
	    {
	      DISCARD (1);
	      JUMPR_NEXT;
	    }
	  break;

	case BRgotoifnonnilelsepop:
	  if (!NILP (TOP_LVALUE = TOP))
	    JUMPR;
	  else
	    {
	      DISCARD (1);
	      JUMPR_NEXT;
	    }
	  break;

	case Breturn:
	  UNGCPRO;
#ifdef ERROR_CHECK_BYTE_CODE
	  /* Binds and unbinds are supposed to be compiled balanced.  */
	  if (specpdl_depth() != speccount)
	    invalid_byte_code ("unbalanced specbinding stack", Qunbound);
#endif
	  return TOP_WITH_MULTIPLE_VALUES;

	case Bdiscard:
	  DISCARD (1);
	  break;

	case Bdup:
	  {
	    Lisp_Object arg = TOP_WITH_MULTIPLE_VALUES;
	    PUSH (arg);
	    break;
	  }

	case Bconstant2:
	  PUSH (constants_data[READ_UINT_2]);
	  break;

	case Bcar:
          {
            /* Fcar can GC via wrong_type_argument. */
            /* GCPRO_STACK; */
            Lisp_Object arg = TOP;
            TOP_LVALUE = CONSP (arg) ? XCAR (arg) : Fcar (arg);
            break;
          }

	case Bcdr:
          {
            /* Fcdr can GC via wrong_type_argument. */
            /* GCPRO_STACK; */
            Lisp_Object arg = TOP;
            TOP_LVALUE = CONSP (arg) ? XCDR (arg) : Fcdr (arg);
            break;
          }

	case Bunbind_all:
	  /* To unbind back to the beginning of this frame.  Not used yet,
	     but will be needed for tail-recursion elimination. */
	  unbind_to (speccount);
	  break;

	case Bnth:
	  {
	    Lisp_Object arg = POP;
	    /* Fcar and Fnthcdr can GC via wrong_type_argument. */
	    /* GCPRO_STACK; */
	    TOP_LVALUE = Fcar (Fnthcdr (TOP, arg));
	    break;
	  }

	case Bsymbolp:
	  TOP_LVALUE = SYMBOLP (TOP) ? Qt : Qnil;
	  break;

	case Bconsp:
	  TOP_LVALUE = CONSP (TOP) ? Qt : Qnil;
	  break;

	case Bstringp:
	  TOP_LVALUE = STRINGP (TOP) ? Qt : Qnil;
	  break;

	case Blistp:
	  TOP_LVALUE = LISTP (TOP) ? Qt : Qnil;
	  break;

	case Bnumberp:
#ifdef WITH_NUMBER_TYPES
	  TOP_LVALUE = NUMBERP (TOP) ? Qt : Qnil;
#else
	  TOP_LVALUE = INT_OR_FLOATP (TOP) ? Qt : Qnil;
#endif
	  break;

	case Bintegerp:
#ifdef HAVE_BIGNUM
	  TOP_LVALUE = INTEGERP (TOP) ? Qt : Qnil;
#else
	  TOP_LVALUE = INTP (TOP) ? Qt : Qnil;
#endif
	  break;

	case Beq:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = EQ_WITH_EBOLA_NOTICE (TOP, arg) ? Qt : Qnil;
	    break;
	  }

	case Bnot:
	  TOP_LVALUE = NILP (TOP) ? Qt : Qnil;
	  break;

	case Bcons:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fcons (TOP, arg);
	    break;
	  }

	case Blist1:
	  TOP_LVALUE = Fcons (TOP, Qnil);
	  break;


	case BlistN:
	  n = READ_UINT_1;
	  goto do_list;

	case Blist2:
	case Blist3:
	case Blist4:
	  /* common case */
	  n = opcode - (Blist1 - 1);
	do_list:
	  {
	    Lisp_Object list = Qnil;
	  list_loop:
	    list = Fcons (TOP, list);
	    if (--n)
	      {
		DISCARD (1);
		goto list_loop;
	      }
	    TOP_LVALUE = list;
	    break;
	  }


	case Bconcat2:
	case Bconcat3:
	case Bconcat4:
	  n = opcode - (Bconcat2 - 2);
	  goto do_concat;

	case BconcatN:
	  /* common case */
	  n = READ_UINT_1;
	do_concat:
	  DISCARD (n - 1);
	  /* Apparently `concat' can GC; Fconcat GCPROs its arguments. */
	  /* GCPRO_STACK; */
          TOP_LVALUE = TOP; /* Ignore multiple values. */
	  TOP_LVALUE = Fconcat (n, TOP_ADDRESS);
	  break;


	case Blength:
	  TOP_LVALUE = Flength (TOP);
	  break;

	case Baset:
	  {
	    Lisp_Object arg2 = POP;
	    Lisp_Object arg1 = POP;
	    TOP_LVALUE = Faset (TOP, arg1, arg2);
	    break;
	  }

	case Bsymbol_value:
	  /* Why does this need GCPRO_STACK?  If not, remove others, too. */
	  /* GCPRO_STACK; */
	  TOP_LVALUE = Fsymbol_value (TOP);
	  break;

	case Bsymbol_function:
	  TOP_LVALUE = Fsymbol_function (TOP);
	  break;

	case Bget:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fget (TOP, arg, Qnil);
	    break;
	  }

	case Bsub1:
          {
#ifdef HAVE_BIGNUM
            TOP_LVALUE = Fsub1 (TOP);
#else
            Lisp_Object arg = TOP;
            TOP_LVALUE = INTP (arg) ? INT_MINUS1 (arg) : Fsub1 (arg);
#endif
	  break;
          }
	case Badd1:
          {
#ifdef HAVE_BIGNUM
            TOP_LVALUE = Fadd1 (TOP);
#else
            Lisp_Object arg = TOP;
            TOP_LVALUE = INTP (arg) ? INT_PLUS1 (arg) : Fadd1 (arg);
#endif
	  break;
          }

	case Beqlsign:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithcompare (TOP, arg) == 0 ? Qt : Qnil;
	    break;
	  }

	case Bgtr:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithcompare (TOP, arg) > 0 ? Qt : Qnil;
	    break;
	  }

	case Blss:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithcompare (TOP, arg) < 0 ? Qt : Qnil;
	    break;
	  }

	case Bleq:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithcompare (TOP, arg) <= 0 ? Qt : Qnil;
	    break;
	  }

	case Bgeq:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithcompare (TOP, arg) >= 0 ? Qt : Qnil;
	    break;
	  }


	case Bnegate:
	  TOP_LVALUE = bytecode_negate (TOP);
	  break;

	case Bnconc:
	  DISCARD (1);
	  /* nconc2 GCPROs before calling this. */
	  /* GCPRO_STACK; */
          TOP_LVALUE = TOP; /* Ignore multiple values. */
	  TOP_LVALUE = bytecode_nconc2 (TOP_ADDRESS);
	  break;

	case Bplus:
	  {
	    Lisp_Object arg2 = POP;
	    Lisp_Object arg1 = TOP;
#ifdef HAVE_BIGNUM
	    TOP_LVALUE = bytecode_arithop (arg1, arg2, opcode);
#else
	    TOP_LVALUE = INTP (arg1) && INTP (arg2) ?
	      INT_PLUS (arg1, arg2) :
	      bytecode_arithop (arg1, arg2, opcode);
#endif
	    break;
	  }

	case Bdiff:
	  {
	    Lisp_Object arg2 = POP;
	    Lisp_Object arg1 = TOP;
#ifdef HAVE_BIGNUM
	    TOP_LVALUE = bytecode_arithop (arg1, arg2, opcode);
#else
	    TOP_LVALUE = INTP (arg1) && INTP (arg2) ?
	      INT_MINUS (arg1, arg2) :
	      bytecode_arithop (arg1, arg2, opcode);
#endif
	    break;
	  }

	case Bmult:
	case Bquo:
	case Bmax:
	case Bmin:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = bytecode_arithop (TOP, arg, opcode);
	    break;
	  }

	case Bpoint:
	  PUSH (make_int (BUF_PT (current_buffer)));
	  break;

	case Binsert:
	  /* Says it can GC. */
	  /* GCPRO_STACK; */
          TOP_LVALUE = TOP; /* Ignore multiple values. */
	  TOP_LVALUE = Finsert (1, TOP_ADDRESS);
	  break;

	case BinsertN:
	  n = READ_UINT_1;
	  DISCARD (n - 1);
	  /* See Binsert. */
	  /* GCPRO_STACK; */
          TOP_LVALUE = TOP; /* Ignore multiple values. */
	  TOP_LVALUE = Finsert (n, TOP_ADDRESS);
	  break;

	case Baref:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Faref (TOP, arg);
	    break;
	  }

	case Bmemq:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fmemq (TOP, arg);
	    break;
	  }

	case Bset:
	  {
	    Lisp_Object arg = POP;
	    /* Fset may call magic handlers */
	    /* GCPRO_STACK; */
	    TOP_LVALUE = Fset (TOP, arg);
	    break;
	  }

	case Bequal:
	  {
	    Lisp_Object arg = POP;
	    /* Can QUIT, so can GC, right? */
	    /* GCPRO_STACK; */
	    TOP_LVALUE = Fequal (TOP, arg);
	    break;
	  }

	case Bnthcdr:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fnthcdr (TOP, arg);
	    break;
	  }

	case Belt:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Felt (TOP, arg);
	    break;
	  }

	case Bmember:
	  {
	    Lisp_Object arg = POP;
	    /* Can QUIT, so can GC, right? */
	    /* GCPRO_STACK; */
	    TOP_LVALUE = Fmember (TOP, arg);
	    break;
	  }

	case Bgoto_char:
	  TOP_LVALUE = Fgoto_char (TOP, Qnil);
	  break;

	case Bcurrent_buffer:
	  {
	    Lisp_Object buffer = wrap_buffer (current_buffer);

	    PUSH (buffer);
	    break;
	  }

	case Bset_buffer:
	  /* #### WAG: set-buffer may cause Fset's of buffer locals
	     Didn't prevent crash. :-( */
	  /* GCPRO_STACK; */
	  TOP_LVALUE = Fset_buffer (TOP);
	  break;

	case Bpoint_max:
	  PUSH (make_int (BUF_ZV (current_buffer)));
	  break;

	case Bpoint_min:
	  PUSH (make_int (BUF_BEGV (current_buffer)));
	  break;

	case Bskip_chars_forward:
	  {
	    Lisp_Object arg = POP;
	    /* Can QUIT, so can GC, right? */
	    /* GCPRO_STACK; */
	    TOP_LVALUE = Fskip_chars_forward (TOP, arg, Qnil);
	    break;
	  }

	case Bassq:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fassq (TOP, arg);
	    break;
	  }

	case Bsetcar:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fsetcar (TOP, arg);
	    break;
	  }

	case Bsetcdr:
	  {
	    Lisp_Object arg = POP;
	    TOP_LVALUE = Fsetcdr (TOP, arg);
	    break;
	  }

	case Bnreverse:
	  TOP_LVALUE = bytecode_nreverse (TOP);
	  break;

	case Bcar_safe:
	  TOP_LVALUE = CONSP (TOP) ? XCAR (TOP) : Qnil;
	  break;

	case Bcdr_safe:
	  TOP_LVALUE = CONSP (TOP) ? XCDR (TOP) : Qnil;
	  break;

	}
    }
}

/* It makes a worthwhile performance difference (5%) to shunt
   lesser-used opcodes off to a subroutine, to keep the switch in
   execute_optimized_program small.  If you REALLY care about
   performance, you want to keep your heavily executed code away from
   rarely executed code, to minimize cache misses.

   Don't make this function static, since then the compiler might inline it. */
Lisp_Object *
execute_rare_opcode (Lisp_Object *stack_ptr,
		     const Opbyte *UNUSED (program_ptr),
		     Opcode opcode)
{
  REGISTER int n;

  switch (opcode)
    {

    case Bsave_excursion:
      record_unwind_protect (save_excursion_restore,
			     save_excursion_save ());
      break;

    case Bsave_window_excursion:
      {
	int count = specpdl_depth ();
	record_unwind_protect (save_window_excursion_unwind,
			       call1 (Qcurrent_window_configuration, Qnil));
	TOP_LVALUE = Fprogn (TOP);
	unbind_to (count);
	break;
      }

    case Bsave_restriction:
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save (current_buffer));
      break;

    case Bcatch:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = internal_catch (TOP, Feval, arg, 0, 0, 0);
	break;
      }

    case Bskip_chars_backward:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fskip_chars_backward (TOP, arg, Qnil);
	break;
      }

    case Bunwind_protect:
      record_unwind_protect (Fprogn, POP);
      break;

    case Bcondition_case:
      {
	Lisp_Object arg2 = POP; /* handlers */
	Lisp_Object arg1 = POP; /* bodyform */
	TOP_LVALUE = condition_case_3 (arg1, TOP, arg2);
	break;
      }

    case Bset_marker:
      {
	Lisp_Object arg2 = POP;
	Lisp_Object arg1 = POP;
	TOP_LVALUE = Fset_marker (TOP, arg1, arg2);
	break;
      }

    case Brem:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Frem (TOP, arg);
	break;
      }

    case Bmatch_beginning:
      TOP_LVALUE = Fmatch_beginning (TOP);
      break;

    case Bmatch_end:
      TOP_LVALUE = Fmatch_end (TOP);
      break;

    case Bupcase:
      TOP_LVALUE = Fupcase (TOP, Qnil);
      break;

    case Bdowncase:
      TOP_LVALUE = Fdowncase (TOP, Qnil);
      break;

    case Bfset:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Ffset (TOP, arg);
	break;
      }

    case Bstring_equal:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fstring_equal (TOP, arg);
	break;
      }

    case Bstring_lessp:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fstring_lessp (TOP, arg);
	break;
      }

    case Bsubstring:
      {
	Lisp_Object arg2 = POP;
	Lisp_Object arg1 = POP;
	TOP_LVALUE = Fsubstring (TOP, arg1, arg2);
	break;
      }

    case Bcurrent_column:
      PUSH (make_int (current_column (current_buffer)));
      break;

    case Bchar_after:
      TOP_LVALUE = Fchar_after (TOP, Qnil);
      break;

    case Bindent_to:
      TOP_LVALUE = Findent_to (TOP, Qnil, Qnil);
      break;

    case Bwiden:
      PUSH (Fwiden (Qnil));
      break;

    case Bfollowing_char:
      PUSH (Ffollowing_char (Qnil));
      break;

    case Bpreceding_char:
      PUSH (Fpreceding_char (Qnil));
      break;

    case Beolp:
      PUSH (Feolp (Qnil));
      break;

    case Beobp:
      PUSH (Feobp (Qnil));
      break;

    case Bbolp:
      PUSH (Fbolp (Qnil));
      break;

    case Bbobp:
      PUSH (Fbobp (Qnil));
      break;

    case Bsave_current_buffer:
      record_unwind_protect (save_current_buffer_restore,
			     Fcurrent_buffer ());
      break;

    case Binteractive_p:
      PUSH (Finteractive_p ());
      break;

    case Bforward_char:
      TOP_LVALUE = Fforward_char (TOP, Qnil);
      break;

    case Bforward_word:
      TOP_LVALUE = Fforward_word (TOP, Qnil);
      break;

    case Bforward_line:
      TOP_LVALUE = Fforward_line (TOP, Qnil);
      break;

    case Bchar_syntax:
      TOP_LVALUE = Fchar_syntax (TOP, Qnil);
      break;

    case Bbuffer_substring:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fbuffer_substring (TOP, arg, Qnil);
	break;
      }

    case Bdelete_region:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fdelete_region (TOP, arg, Qnil);
	break;
      }

    case Bnarrow_to_region:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fnarrow_to_region (TOP, arg, Qnil);
	break;
      }

    case Bend_of_line:
      TOP_LVALUE = Fend_of_line (TOP, Qnil);
      break;

    case Btemp_output_buffer_setup:
      temp_output_buffer_setup (TOP);
      TOP_LVALUE = Vstandard_output;
      break;

    case Btemp_output_buffer_show:
      {
	Lisp_Object arg = POP;
	temp_output_buffer_show (TOP, Qnil);
	TOP_LVALUE = arg;
	/* GAG ME!! */
	/* pop binding of standard-output */
	unbind_to (specpdl_depth() - 1);
	break;
      }

    case Bold_eq:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = HACKEQ_UNSAFE (TOP, arg) ? Qt : Qnil;
	break;
      }

    case Bold_memq:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fold_memq (TOP, arg);
	break;
      }

    case Bold_equal:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fold_equal (TOP, arg);
	break;
      }

    case Bold_member:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fold_member (TOP, arg);
	break;
      }

    case Bold_assq:
      {
	Lisp_Object arg = POP;
	TOP_LVALUE = Fold_assq (TOP, arg);
	break;
      }

    case Bbind_multiple_value_limits:
      {
        Lisp_Object upper = POP, first = TOP, speccount;

        CHECK_NATNUM (upper);
        CHECK_NATNUM (first);

        speccount = make_int (bind_multiple_value_limits (XINT (first),
                                                          XINT (upper)));
        PUSH (upper);
        PUSH (speccount);
        break;
      }

    case Bmultiple_value_call:
      {
        n = XINT (POP);
        DISCARD_PRESERVING_MULTIPLE_VALUES (n - 1);
        /* Discard multiple values for the first (function) argument: */
        TOP_LVALUE = TOP;
        TOP_LVALUE = multiple_value_call (n, TOP_ADDRESS);
        break;
      }

    case Bmultiple_value_list_internal:
      {
        DISCARD_PRESERVING_MULTIPLE_VALUES (3);
        TOP_LVALUE = multiple_value_list_internal (4, TOP_ADDRESS);
        break;
      }

    case Bthrow:
      {
        Lisp_Object arg = POP_WITH_MULTIPLE_VALUES;
        
        /* We never throw to a catch tag that is a multiple value: */
        throw_or_bomb_out (TOP, arg, 0, Qnil, Qnil);
        break;
      }

    default:
      ABORT();
      break;
    }
  return stack_ptr;
}


DOESNT_RETURN
invalid_byte_code (const CIbyte *reason, Lisp_Object frob)
{
  signal_error (Qinvalid_byte_code, reason, frob);
}

/* Check for valid opcodes.  Change this when adding new opcodes.  */
static void
check_opcode (Opcode opcode)
{
  if ((opcode < Bvarref) ||
      (opcode == 0251)   ||
      (opcode > Bassq && opcode < Bconstant))
    invalid_byte_code ("invalid opcode in instruction stream",
		       make_int (opcode));
}

/* Check that IDX is a valid offset into the `constants' vector */
static void
check_constants_index (int idx, Lisp_Object constants)
{
  if (idx < 0 || idx >= XVECTOR_LENGTH (constants))
    signal_ferror
      (Qinvalid_byte_code,
       "reference %d to constants array out of range 0, %ld",
       idx, XVECTOR_LENGTH (constants) - 1);
}

/* Get next character from Lisp instructions string. */
#define READ_INSTRUCTION_CHAR(lvalue) do {				\
  (lvalue) = itext_ichar (ptr);					\
  INC_IBYTEPTR (ptr);							\
  *icounts_ptr++ = program_ptr - program;				\
  if (lvalue > UCHAR_MAX)						\
    invalid_byte_code							\
      ("Invalid character in byte code string", make_char (lvalue));	\
} while (0)

/* Get opcode from Lisp instructions string. */
#define READ_OPCODE do {		\
  unsigned int c;			\
  READ_INSTRUCTION_CHAR (c);		\
  opcode = (Opcode) c;			\
} while (0)

/* Get next operand, a uint8, from Lisp instructions string. */
#define READ_OPERAND_1 do {		\
  READ_INSTRUCTION_CHAR (arg);		\
  argsize = 1;				\
} while (0)

/* Get next operand, a uint16, from Lisp instructions string. */
#define READ_OPERAND_2 do {		\
  unsigned int arg1, arg2;		\
  READ_INSTRUCTION_CHAR (arg1);		\
  READ_INSTRUCTION_CHAR (arg2);		\
  arg = arg1 + (arg2 << 8);		\
  argsize = 2;				\
} while (0)

/* Write 1 byte to PTR, incrementing PTR */
#define WRITE_INT8(value, ptr) do {	\
  *((ptr)++) = (value);			\
} while (0)

/* Write 2 bytes to PTR, incrementing PTR */
#define WRITE_INT16(value, ptr) do {			\
  WRITE_INT8 (((unsigned) (value)) & 0x00ff, (ptr));	\
  WRITE_INT8 (((unsigned) (value)) >> 8    , (ptr));	\
} while (0)

/* We've changed our minds about the opcode we've already written. */
#define REWRITE_OPCODE(new_opcode) ((void) (program_ptr[-1] = new_opcode))

/* Encode an op arg within the opcode, or as a 1 or 2-byte operand. */
#define WRITE_NARGS(base_opcode) do {		\
  if (arg <= 5)					\
    {						\
      REWRITE_OPCODE (base_opcode + arg);	\
    }						\
  else if (arg <= UCHAR_MAX)			\
    {						\
      REWRITE_OPCODE (base_opcode + 6);		\
      WRITE_INT8 (arg, program_ptr);		\
    }						\
  else						\
    {						\
      REWRITE_OPCODE (base_opcode + 7);		\
      WRITE_INT16 (arg, program_ptr);		\
    }						\
} while (0)

/* Encode a constants reference within the opcode, or as a 2-byte operand. */
#define WRITE_CONSTANT do {			\
  check_constants_index(arg, constants);	\
  if (arg <= UCHAR_MAX - Bconstant)		\
    {						\
      REWRITE_OPCODE (Bconstant + arg);		\
    }						\
  else						\
    {						\
      REWRITE_OPCODE (Bconstant2);		\
      WRITE_INT16 (arg, program_ptr);		\
    }						\
} while (0)

#define WRITE_OPCODE WRITE_INT8 (opcode, program_ptr)

/* Compile byte code instructions into free space provided by caller, with
   size >= (2 * string_char_length (instructions) + 1) * sizeof (Opbyte).
   Returns length of compiled code. */
static void
optimize_byte_code (/* in */
		    Lisp_Object instructions,
		    Lisp_Object constants,
		    /* out */
		    Opbyte * const program,
		    int * const program_length,
		    int * const varbind_count)
{
  Bytecount instructions_length = XSTRING_LENGTH (instructions);
  Elemcount comfy_size = (Elemcount) (2 * instructions_length);

  int * const icounts = alloca_array (int, comfy_size);
  int * icounts_ptr = icounts;

  /* We maintain a table of jumps in the source code. */
  struct jump
  {
    int from;
    int to;
  };
  struct jump * const jumps = alloca_array (struct jump, comfy_size);
  struct jump *jumps_ptr = jumps;

  Opbyte *program_ptr = program;

  const Ibyte *ptr = XSTRING_DATA (instructions);
  const Ibyte * const end = ptr + instructions_length;

  *varbind_count = 0;

  while (ptr < end)
    {
      Opcode opcode;
      int arg;
      int argsize = 0;
      READ_OPCODE;
      WRITE_OPCODE;

      switch (opcode)
	{
	  Lisp_Object val;

	case Bvarref+7: READ_OPERAND_2; goto do_varref;
	case Bvarref+6: READ_OPERAND_1; goto do_varref;
	case Bvarref:   case Bvarref+1: case Bvarref+2:
	case Bvarref+3: case Bvarref+4: case Bvarref+5:
	  arg = opcode - Bvarref;
	do_varref:
	  check_constants_index (arg, constants);
	   val = XVECTOR_DATA (constants) [arg];
	   if (!SYMBOLP (val))
	     invalid_byte_code ("variable reference to non-symbol", val);
	   if (EQ (val, Qnil) || EQ (val, Qt) || (SYMBOL_IS_KEYWORD (val)))
	     invalid_byte_code ("variable reference to constant symbol", val);
	   WRITE_NARGS (Bvarref);
	   break;

	case Bvarset+7: READ_OPERAND_2; goto do_varset;
	case Bvarset+6: READ_OPERAND_1; goto do_varset;
	case Bvarset:   case Bvarset+1: case Bvarset+2:
	case Bvarset+3: case Bvarset+4: case Bvarset+5:
	  arg = opcode - Bvarset;
	do_varset:
	  check_constants_index (arg, constants);
	  val = XVECTOR_DATA (constants) [arg];
	  if (!SYMBOLP (val))
	    wtaerror ("attempt to set non-symbol", val);
	  if (EQ (val, Qnil) || EQ (val, Qt))
	    signal_error (Qsetting_constant, 0, val);
	  /* Ignore assignments to keywords by converting to Bdiscard.
	     For backward compatibility only - we'd like to make this an error.  */
	  if (SYMBOL_IS_KEYWORD (val))
	    REWRITE_OPCODE (Bdiscard);
	  else
	    WRITE_NARGS (Bvarset);
	  break;

	case Bvarbind+7: READ_OPERAND_2; goto do_varbind;
	case Bvarbind+6: READ_OPERAND_1; goto do_varbind;
	case Bvarbind:   case Bvarbind+1: case Bvarbind+2:
	case Bvarbind+3: case Bvarbind+4: case Bvarbind+5:
	  arg = opcode - Bvarbind;
	do_varbind:
	  (*varbind_count)++;
	  check_constants_index (arg, constants);
	  val = XVECTOR_DATA (constants) [arg];
	  if (!SYMBOLP (val))
	    wtaerror ("attempt to let-bind non-symbol", val);
	  if (EQ (val, Qnil) || EQ (val, Qt) || (SYMBOL_IS_KEYWORD (val)))
	    signal_error (Qsetting_constant,
			  "attempt to let-bind constant symbol", val);
	  WRITE_NARGS (Bvarbind);
	  break;

	case Bcall+7: READ_OPERAND_2; goto do_call;
	case Bcall+6: READ_OPERAND_1; goto do_call;
	case Bcall:   case Bcall+1: case Bcall+2:
	case Bcall+3: case Bcall+4: case Bcall+5:
	  arg = opcode - Bcall;
	do_call:
	  WRITE_NARGS (Bcall);
	  break;

	case Bunbind+7: READ_OPERAND_2; goto do_unbind;
	case Bunbind+6: READ_OPERAND_1; goto do_unbind;
	case Bunbind:   case Bunbind+1: case Bunbind+2:
	case Bunbind+3: case Bunbind+4: case Bunbind+5:
	  arg = opcode - Bunbind;
	do_unbind:
	  WRITE_NARGS (Bunbind);
	  break;

	case Bgoto:
	case Bgotoifnil:
	case Bgotoifnonnil:
	case Bgotoifnilelsepop:
	case Bgotoifnonnilelsepop:
	  READ_OPERAND_2;
	  /* Make program_ptr-relative */
	  arg += icounts - (icounts_ptr - argsize);
	  goto do_jump;

	case BRgoto:
	case BRgotoifnil:
	case BRgotoifnonnil:
	case BRgotoifnilelsepop:
	case BRgotoifnonnilelsepop:
	  READ_OPERAND_1;
	  /* Make program_ptr-relative */
	  arg -= 127;
	do_jump:
	  /* Record program-relative goto addresses in `jumps' table */
	  jumps_ptr->from = icounts_ptr - icounts - argsize;
	  jumps_ptr->to   = jumps_ptr->from + arg;
	  jumps_ptr++;
	  if (arg >= -1 && arg <= argsize)
	    invalid_byte_code ("goto instruction is its own target", Qunbound);
	  if (arg <= SCHAR_MIN ||
	      arg >  SCHAR_MAX)
	    {
	      if (argsize == 1)
		REWRITE_OPCODE (opcode + Bgoto - BRgoto);
	      WRITE_INT16 (arg, program_ptr);
	    }
	  else
	    {
	      if (argsize == 2)
		REWRITE_OPCODE (opcode + BRgoto - Bgoto);
	      WRITE_INT8 (arg, program_ptr);
	    }
	  break;

	case Bconstant2:
	  READ_OPERAND_2;
	  WRITE_CONSTANT;
	  break;

	case BlistN:
	case BconcatN:
	case BinsertN:
	  READ_OPERAND_1;
	  WRITE_INT8 (arg, program_ptr);
	  break;

	default:
	  if (opcode < Bconstant)
	    check_opcode (opcode);
	  else
	    {
	      arg = opcode - Bconstant;
	      WRITE_CONSTANT;
	    }
	  break;
	}
    }

  /* Fix up jumps table to refer to NEW offsets. */
  {
    struct jump *j;
    for (j = jumps; j < jumps_ptr; j++)
      {
#ifdef ERROR_CHECK_BYTE_CODE
	assert (j->from < icounts_ptr - icounts);
	assert (j->to   < icounts_ptr - icounts);
#endif
	j->from = icounts[j->from];
	j->to   = icounts[j->to];
#ifdef ERROR_CHECK_BYTE_CODE
	assert (j->from < program_ptr - program);
	assert (j->to   < program_ptr - program);
	check_opcode ((Opcode) (program[j->from-1]));
#endif
	check_opcode ((Opcode) (program[j->to]));
      }
  }

  /* Fixup jumps in byte-code until no more fixups needed */
  {
    int more_fixups_needed = 1;

    while (more_fixups_needed)
      {
	struct jump *j;
	more_fixups_needed = 0;
	for (j = jumps; j < jumps_ptr; j++)
	{
	  int from = j->from;
	  int to   = j->to;
	  int jump = to - from;
	  Opbyte *p = program + from;
	  Opcode opcode = (Opcode) p[-1];
	  if (!more_fixups_needed)
	    check_opcode ((Opcode) p[jump]);
	  assert (to >= 0 && program + to < program_ptr);
	  switch (opcode)
	    {
	      case Bgoto:
	      case Bgotoifnil:
	      case Bgotoifnonnil:
	      case Bgotoifnilelsepop:
	      case Bgotoifnonnilelsepop:
		WRITE_INT16 (jump, p);
		break;

	      case BRgoto:
	      case BRgotoifnil:
	      case BRgotoifnonnil:
	      case BRgotoifnilelsepop:
	      case BRgotoifnonnilelsepop:
		if (jump >  SCHAR_MIN &&
		    jump <= SCHAR_MAX)
		  {
		    WRITE_INT8 (jump, p);
		  }
		else		/* barf */
		  {
		    struct jump *jj;
		    for (jj = jumps; jj < jumps_ptr; jj++)
		      {
			assert (jj->from < program_ptr - program);
			assert (jj->to   < program_ptr - program);
			if (jj->from > from) jj->from++;
			if (jj->to   > from) jj->to++;
		      }
		    p[-1] += Bgoto - BRgoto;
		    more_fixups_needed = 1;
		    memmove (p+1, p, program_ptr++ - p);
		    WRITE_INT16 (jump, p);
		  }
		break;

	    default:
	      ABORT();
	      break;
	    }
	}
      }
  }

  /* *program_ptr++ = 0; */
  *program_length = program_ptr - program;
}

/* Optimize the byte code and store the optimized program, only
   understood by bytecode.c, in an opaque object in the
   instructions slot of the Compiled_Function object. */
void
optimize_compiled_function (Lisp_Object compiled_function)
{
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (compiled_function);
  int program_length;
  int varbind_count;
  Opbyte *program;

  {
    int minargs = 0, maxargs = 0, totalargs = 0;
    int optional_p = 0, rest_p = 0, i = 0;
    {
      LIST_LOOP_2 (arg, f->arglist)
	{
	  if (EQ (arg, Qand_optional))
	    optional_p = 1;
	  else if (EQ (arg, Qand_rest))
	    rest_p = 1;
	  else
	    {
	      if (rest_p)
		{
		  maxargs = MANY;
		  totalargs++;
		  break;
		}
	      if (!optional_p)
		minargs++;
	      maxargs++;
	      totalargs++;
	    }
	}
    }
  
    if (totalargs)
#ifdef NEW_GC
      f->arguments = make_compiled_function_args (totalargs); 
#else /* not NEW_GC */
      f->args = xnew_array (Lisp_Object, totalargs);
#endif /* not NEW_GC */

    {
      LIST_LOOP_2 (arg, f->arglist)
	{
	  if (!EQ (arg, Qand_optional) && !EQ (arg, Qand_rest))
#ifdef NEW_GC
	    XCOMPILED_FUNCTION_ARGS_DATA (f->arguments)[i++] = arg;
#else /* not NEW_GC */
	    f->args[i++] = arg;
#endif /* not NEW_GC */
	}
    }

    f->max_args = maxargs;
    f->min_args = minargs;
    f->args_in_array = totalargs;
  }
  
  /* If we have not actually read the bytecode string
     and constants vector yet, fetch them from the file.  */
  if (CONSP (f->instructions))
    Ffetch_bytecode (compiled_function);

  if (STRINGP (f->instructions))
    {
      /* XSTRING_LENGTH() is more efficient than string_char_length(),
	 which would be slightly more `proper' */
      program = alloca_array (Opbyte, 1 + 2 * XSTRING_LENGTH (f->instructions));
      optimize_byte_code (f->instructions, f->constants,
			  program, &program_length, &varbind_count);
      f->specpdl_depth = (unsigned short) (XINT (Flength (f->arglist)) +
                                           varbind_count);
      f->instructions =
	make_opaque (program, program_length * sizeof (Opbyte));
    }

  assert (OPAQUEP (f->instructions));
}

/************************************************************************/
/*		The compiled-function object type			*/
/************************************************************************/

static void
print_compiled_function (Lisp_Object obj, Lisp_Object printcharfun,
			 int escapeflag)
{
  /* This function can GC */
  Lisp_Compiled_Function *f =
    XCOMPILED_FUNCTION (obj); /* GC doesn't relocate */
  int docp = f->flags.documentationp;
  int intp = f->flags.interactivep;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  write_c_string (printcharfun, print_readably ? "#[" : "#<compiled-function ");
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  if (!print_readably)
    {
      Lisp_Object ann = compiled_function_annotation (f);
      if (!NILP (ann))
	write_fmt_string_lisp (printcharfun, "(from %S) ", 1, ann);
    }
#endif /* COMPILED_FUNCTION_ANNOTATION_HACK */
  /* COMPILED_ARGLIST = 0 */
  print_internal (compiled_function_arglist (f), printcharfun, escapeflag);

  /* COMPILED_INSTRUCTIONS = 1 */
  write_c_string (printcharfun, " ");
  {
    struct gcpro ngcpro1;
    Lisp_Object instructions = compiled_function_instructions (f);
    NGCPRO1 (instructions);
    if (STRINGP (instructions) && !print_readably)
      {
	/* We don't usually want to see that junk in the bytecode. */
	write_fmt_string (printcharfun, "\"...(%ld)\"",
			  (long) string_char_length (instructions));
      }
    else
      print_internal (instructions, printcharfun, escapeflag);
    NUNGCPRO;
  }

  /* COMPILED_CONSTANTS = 2 */
  write_c_string (printcharfun, " ");
  print_internal (compiled_function_constants (f), printcharfun, escapeflag);

  /* COMPILED_STACK_DEPTH = 3 */
  write_fmt_string (printcharfun, " %d", compiled_function_stack_depth (f));

  /* COMPILED_DOC_STRING = 4 */
  if (docp || intp)
    {
      write_c_string (printcharfun, " ");
      print_internal (compiled_function_documentation (f), printcharfun,
		      escapeflag);
    }

  /* COMPILED_INTERACTIVE = 5 */
  if (intp)
    {
      write_c_string (printcharfun, " ");
      print_internal (compiled_function_interactive (f), printcharfun,
		      escapeflag);
    }

  UNGCPRO;
  write_c_string (printcharfun, print_readably ? "]" : ">");
}


static Lisp_Object
mark_compiled_function (Lisp_Object obj)
{
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (obj);
  int i;

  mark_object (f->instructions);
  mark_object (f->arglist);
  mark_object (f->doc_and_interactive);
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  mark_object (f->annotated);
#endif
  for (i = 0; i < f->args_in_array; i++)
#ifdef NEW_GC
    mark_object (XCOMPILED_FUNCTION_ARGS_DATA (f->arguments)[i]);
#else /* not NEW_GC */
    mark_object (f->args[i]);
#endif /* not NEW_GC */

  /* tail-recurse on constants */
  return f->constants;
}

static int
compiled_function_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Compiled_Function *f1 = XCOMPILED_FUNCTION (obj1);
  Lisp_Compiled_Function *f2 = XCOMPILED_FUNCTION (obj2);
  return
    (f1->flags.documentationp == f2->flags.documentationp &&
     f1->flags.interactivep   == f2->flags.interactivep   &&
     f1->flags.domainp        == f2->flags.domainp        && /* I18N3 */
     internal_equal (compiled_function_instructions (f1),
		     compiled_function_instructions (f2), depth + 1) &&
     internal_equal (f1->constants,    f2->constants,    depth + 1) &&
     internal_equal (f1->arglist,      f2->arglist,      depth + 1) &&
     internal_equal (f1->doc_and_interactive,
		     f2->doc_and_interactive, depth + 1));
}

static Hashcode
compiled_function_hash (Lisp_Object obj, int depth)
{
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (obj);
  return HASH3 ((f->flags.documentationp << 2) +
		(f->flags.interactivep << 1) +
		f->flags.domainp,
		internal_hash (f->instructions, depth + 1),
		internal_hash (f->constants,    depth + 1));
}

static const struct memory_description compiled_function_description[] = {
  { XD_INT,         offsetof (Lisp_Compiled_Function, args_in_array) },
#ifdef NEW_GC
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, arguments) },
#else /* not NEW_GC */
  { XD_BLOCK_PTR,   offsetof (Lisp_Compiled_Function, args),
    XD_INDIRECT (0, 0), { &lisp_object_description } },
#endif /* not NEW_GC */
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, instructions) },
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, constants) },
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, arglist) },
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, doc_and_interactive) },
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  { XD_LISP_OBJECT, offsetof (Lisp_Compiled_Function, annotated) },
#endif
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("compiled-function", compiled_function,
				     1, /*dumpable_flag*/
				     mark_compiled_function,
				     print_compiled_function, 0,
				     compiled_function_equal,
				     compiled_function_hash,
				     compiled_function_description,
				     Lisp_Compiled_Function);


DEFUN ("compiled-function-p", Fcompiled_function_p, 1, 1, 0, /*
Return t if OBJECT is a byte-compiled function object.
*/
       (object))
{
  return COMPILED_FUNCTIONP (object) ? Qt : Qnil;
}

/************************************************************************/
/*		compiled-function object accessor functions		*/
/************************************************************************/

Lisp_Object
compiled_function_arglist (Lisp_Compiled_Function *f)
{
  return f->arglist;
}

Lisp_Object
compiled_function_instructions (Lisp_Compiled_Function *f)
{
  if (! OPAQUEP (f->instructions))
    return f->instructions;

  {
    /* Invert action performed by optimize_byte_code() */
    Lisp_Opaque *opaque = XOPAQUE (f->instructions);

    Ibyte * const buffer =
      alloca_ibytes (OPAQUE_SIZE (opaque) * MAX_ICHAR_LEN);
    Ibyte *bp = buffer;

    const Opbyte * const program = (const Opbyte *) OPAQUE_DATA (opaque);
    const Opbyte *program_ptr = program;
    const Opbyte * const program_end = program_ptr + OPAQUE_SIZE (opaque);

    while (program_ptr < program_end)
      {
	Opcode opcode = (Opcode) READ_UINT_1;
	bp += set_itext_ichar (bp, opcode);
	switch (opcode)
	  {
	  case Bvarref+7:
	  case Bvarset+7:
	  case Bvarbind+7:
	  case Bcall+7:
	  case Bunbind+7:
	  case Bconstant2:
	    bp += set_itext_ichar (bp, READ_UINT_1);
	    bp += set_itext_ichar (bp, READ_UINT_1);
	    break;

	  case Bvarref+6:
	  case Bvarset+6:
	  case Bvarbind+6:
	  case Bcall+6:
	  case Bunbind+6:
	  case BlistN:
	  case BconcatN:
	  case BinsertN:
	    bp += set_itext_ichar (bp, READ_UINT_1);
	    break;

	  case Bgoto:
	  case Bgotoifnil:
	  case Bgotoifnonnil:
	  case Bgotoifnilelsepop:
	  case Bgotoifnonnilelsepop:
	    {
	      int jump = READ_INT_2;
	      Opbyte buf2[2];
	      Opbyte *buf2p = buf2;
	      /* Convert back to program-relative address */
	      WRITE_INT16 (jump + (program_ptr - 2 - program), buf2p);
	      bp += set_itext_ichar (bp, buf2[0]);
	      bp += set_itext_ichar (bp, buf2[1]);
	      break;
	    }

	  case BRgoto:
	  case BRgotoifnil:
	  case BRgotoifnonnil:
	  case BRgotoifnilelsepop:
	  case BRgotoifnonnilelsepop:
	    bp += set_itext_ichar (bp, READ_INT_1 + 127);
	    break;

	  default:
	    break;
	  }
      }
    return make_string (buffer, bp - buffer);
  }
}

Lisp_Object
compiled_function_constants (Lisp_Compiled_Function *f)
{
  return f->constants;
}

int
compiled_function_stack_depth (Lisp_Compiled_Function *f)
{
  return f->stack_depth;
}

/* The compiled_function->doc_and_interactive slot uses the minimal
   number of conses, based on compiled_function->flags; it may take
   any of the following forms:

	doc
	interactive
	domain
	(doc . interactive)
	(doc . domain)
	(interactive . domain)
	(doc . (interactive . domain))
 */

/* Caller must check flags.interactivep first */
Lisp_Object
compiled_function_interactive (Lisp_Compiled_Function *f)
{
  assert (f->flags.interactivep);
  if (f->flags.documentationp && f->flags.domainp)
    return XCAR (XCDR (f->doc_and_interactive));
  else if (f->flags.documentationp)
    return XCDR (f->doc_and_interactive);
  else if (f->flags.domainp)
    return XCAR (f->doc_and_interactive);
  else
    return f->doc_and_interactive;
}

/* Caller need not check flags.documentationp first */
Lisp_Object
compiled_function_documentation (Lisp_Compiled_Function *f)
{
  if (! f->flags.documentationp)
    return Qnil;
  else if (f->flags.interactivep && f->flags.domainp)
    return XCAR (f->doc_and_interactive);
  else if (f->flags.interactivep)
    return XCAR (f->doc_and_interactive);
  else if (f->flags.domainp)
    return XCAR (f->doc_and_interactive);
  else
    return f->doc_and_interactive;
}

/* Caller need not check flags.domainp first */
Lisp_Object
compiled_function_domain (Lisp_Compiled_Function *f)
{
  if (! f->flags.domainp)
    return Qnil;
  else if (f->flags.documentationp && f->flags.interactivep)
    return XCDR (XCDR (f->doc_and_interactive));
  else if (f->flags.documentationp)
    return XCDR (f->doc_and_interactive);
  else if (f->flags.interactivep)
    return XCDR (f->doc_and_interactive);
  else
    return f->doc_and_interactive;
}

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK

Lisp_Object
compiled_function_annotation (Lisp_Compiled_Function *f)
{
  return f->annotated;
}

#endif

/* used only by Snarf-documentation; there must be doc already. */
void
set_compiled_function_documentation (Lisp_Compiled_Function *f,
				     Lisp_Object new_doc)
{
  assert (f->flags.documentationp);
  assert (INTP (new_doc) || STRINGP (new_doc));

  if (f->flags.interactivep && f->flags.domainp)
    XCAR (f->doc_and_interactive) = new_doc;
  else if (f->flags.interactivep)
    XCAR (f->doc_and_interactive) = new_doc;
  else if (f->flags.domainp)
    XCAR (f->doc_and_interactive) = new_doc;
  else
    f->doc_and_interactive = new_doc;
}


DEFUN ("compiled-function-arglist", Fcompiled_function_arglist, 1, 1, 0, /*
Return the argument list of the compiled-function object FUNCTION.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return compiled_function_arglist (XCOMPILED_FUNCTION (function));
}

DEFUN ("compiled-function-instructions", Fcompiled_function_instructions, 1, 1, 0, /*
Return the byte-opcode string of the compiled-function object FUNCTION.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return compiled_function_instructions (XCOMPILED_FUNCTION (function));
}

DEFUN ("compiled-function-constants", Fcompiled_function_constants, 1, 1, 0, /*
Return the constants vector of the compiled-function object FUNCTION.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return compiled_function_constants (XCOMPILED_FUNCTION (function));
}

DEFUN ("compiled-function-stack-depth", Fcompiled_function_stack_depth, 1, 1, 0, /*
Return the maximum stack depth of the compiled-function object FUNCTION.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return make_int (compiled_function_stack_depth (XCOMPILED_FUNCTION (function)));
}

DEFUN ("compiled-function-doc-string", Fcompiled_function_doc_string, 1, 1, 0, /*
Return the doc string of the compiled-function object FUNCTION, if available.
Functions that had their doc strings snarfed into the DOC file will have
an integer returned instead of a string.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return compiled_function_documentation (XCOMPILED_FUNCTION (function));
}

DEFUN ("compiled-function-interactive", Fcompiled_function_interactive, 1, 1, 0, /*
Return the interactive spec of the compiled-function object FUNCTION, or nil.
If non-nil, the return value will be a list whose first element is
`interactive' and whose second element is the interactive spec.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return XCOMPILED_FUNCTION (function)->flags.interactivep
    ? list2 (Qinteractive,
	     compiled_function_interactive (XCOMPILED_FUNCTION (function)))
    : Qnil;
}

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK

DEFUN ("compiled-function-annotation", Fcompiled_function_annotation, 1, 1, 0, /*
Return the annotation of the compiled-function object FUNCTION, or nil.
The annotation is a piece of information indicating where this
compiled-function object came from.  Generally this will be
a symbol naming a function; or a string naming a file, if the
compiled-function object was not defined in a function; or nil,
if the compiled-function object was not created as a result of
a `load'.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return compiled_function_annotation (XCOMPILED_FUNCTION (function));
}

#endif /* COMPILED_FUNCTION_ANNOTATION_HACK */

DEFUN ("compiled-function-domain", Fcompiled_function_domain, 1, 1, 0, /*
Return the domain of the compiled-function object FUNCTION, or nil.
This is only meaningful if I18N3 was enabled when emacs was compiled.
*/
       (function))
{
  CHECK_COMPILED_FUNCTION (function);
  return XCOMPILED_FUNCTION (function)->flags.domainp
    ? compiled_function_domain (XCOMPILED_FUNCTION (function))
    : Qnil;
}



DEFUN ("fetch-bytecode", Ffetch_bytecode, 1, 1, 0, /*
If the byte code for compiled function FUNCTION is lazy-loaded, fetch it now.
*/
       (function))
{
  Lisp_Compiled_Function *f;
  CHECK_COMPILED_FUNCTION (function);
  f = XCOMPILED_FUNCTION (function);

  if (OPAQUEP (f->instructions) || STRINGP (f->instructions))
    return function;

  if (CONSP (f->instructions))
    {
      Lisp_Object tem = read_doc_string (f->instructions);
      if (!CONSP (tem))
	signal_error (Qinvalid_byte_code,
			   "Invalid lazy-loaded byte code", tem);
      /* v18 or v19 bytecode file.  Need to Ebolify. */
      if (f->flags.ebolified && VECTORP (XCDR (tem)))
	ebolify_bytecode_constants (XCDR (tem));
      f->instructions = XCAR (tem);
      f->constants    = XCDR (tem);
      return function;
    }
  ABORT ();
  return Qnil; /* not (usually) reached */
}

DEFUN ("optimize-compiled-function", Foptimize_compiled_function, 1, 1, 0, /*
Convert compiled function FUNCTION into an optimized internal form.
*/
       (function))
{
  Lisp_Compiled_Function *f;
  CHECK_COMPILED_FUNCTION (function);
  f = XCOMPILED_FUNCTION (function);

  if (OPAQUEP (f->instructions)) /* Already optimized? */
    return Qnil;

  optimize_compiled_function (function);
  return Qnil;
}

DEFUN ("byte-code", Fbyte_code, 3, 3, 0, /*
Function used internally in byte-compiled code.
First argument INSTRUCTIONS is a string of byte code.
Second argument CONSTANTS is a vector of constants.
Third argument STACK-DEPTH is the maximum stack depth used in this function.
If STACK-DEPTH is incorrect, Emacs may crash.
*/
       (instructions, constants, stack_depth))
{
  /* This function can GC */
  int varbind_count;
  int program_length;
  Opbyte *program;

  CHECK_STRING (instructions);
  CHECK_VECTOR (constants);
  CHECK_NATNUM (stack_depth);

  /* Optimize the `instructions' string, just like when executing a
     regular compiled function, but don't save it for later since this is
     likely to only be executed once. */
  program = alloca_array (Opbyte, 1 + 2 * XSTRING_LENGTH (instructions));
  optimize_byte_code (instructions, constants, program,
		      &program_length, &varbind_count);
  SPECPDL_RESERVE (varbind_count);
  return execute_optimized_program (program,
				    XINT (stack_depth),
				    XVECTOR_DATA (constants));
}


void
syms_of_bytecode (void)
{
  INIT_LRECORD_IMPLEMENTATION (compiled_function);
#ifdef NEW_GC
  INIT_LRECORD_IMPLEMENTATION (compiled_function_args);
#endif /* NEW_GC */

  DEFERROR_STANDARD (Qinvalid_byte_code, Qinvalid_state);
  DEFSYMBOL (Qbyte_code);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qcompiled_functionp);

  DEFSUBR (Fbyte_code);
  DEFSUBR (Ffetch_bytecode);
  DEFSUBR (Foptimize_compiled_function);

  DEFSUBR (Fcompiled_function_p);
  DEFSUBR (Fcompiled_function_instructions);
  DEFSUBR (Fcompiled_function_constants);
  DEFSUBR (Fcompiled_function_stack_depth);
  DEFSUBR (Fcompiled_function_arglist);
  DEFSUBR (Fcompiled_function_interactive);
  DEFSUBR (Fcompiled_function_doc_string);
  DEFSUBR (Fcompiled_function_domain);
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  DEFSUBR (Fcompiled_function_annotation);
#endif

#ifdef BYTE_CODE_METER
  DEFSYMBOL (Qbyte_code_meter);
#endif
}

void
vars_of_bytecode (void)
{
#ifdef BYTE_CODE_METER

  DEFVAR_LISP ("byte-code-meter", &Vbyte_code_meter /*
A vector of vectors which holds a histogram of byte code usage.
\(aref (aref byte-code-meter 0) CODE) indicates how many times the byte
opcode CODE has been executed.
\(aref (aref byte-code-meter CODE1) CODE2), where CODE1 is not 0,
indicates how many times the byte opcodes CODE1 and CODE2 have been
executed in succession.
*/ );
  DEFVAR_BOOL ("byte-metering-on", &byte_metering_on /*
If non-nil, keep profiling information on byte code usage.
The variable `byte-code-meter' indicates how often each byte opcode is used.
If a symbol has a property named `byte-code-meter' whose value is an
integer, it is incremented each time that symbol's function is called.
*/ );

  byte_metering_on = 0;
  Vbyte_code_meter = make_vector (256, Qzero);
  {
    int i = 256;
    while (i--)
      XVECTOR_DATA (Vbyte_code_meter)[i] = make_vector (256, Qzero);
  }
#endif /* BYTE_CODE_METER */
}
