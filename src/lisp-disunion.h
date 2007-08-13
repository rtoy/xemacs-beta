/* Fundamental definitions for XEmacs Lisp interpreter -- non-union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.30.  Split out from lisp.h. */
/* This file has diverged greatly from FSF Emacs.  Syncing is no
   longer desired or possible */

/*
 * Format of a non-union-type Lisp Object
 *
 *   For the USE_MINIMAL_TAGBITS implementation:
 *
 *             3         2         1         0
 *       bit  10987654321098765432109876543210
 *            --------------------------------
 *            VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVTT
 *
 *   For the non-USE_MINIMAL_TAGBITS implementation:
 *
 *             3         2         1         0
 *       bit  10987654321098765432109876543210
 *            --------------------------------
 *            TTTMVVVVVVVVVVVVVVVVVVVVVVVVVVVV
 *
 *    V = value bits
 *    T = type bits
 *    M = mark bits
 *
 * For integral Lisp types, i.e. integers and characters, the value
 * bits are the Lisp object.
 *
 *     The object is obtained by masking off the type and mark
 *     bits.  In the USE_MINIMAL_TAGBITS implementation, bit 1 is
 *     used as a value bit by splitting the Lisp integer type into
 *     two subtypes, Lisp_Type_Int_Even and Lisp_Type_Int_Odd.  By
 *     this trickery we get 31 bits for integers instead of 30.
 *
 *     In the non-USE_MINIMAL_TAGBITS world, Lisp integers are 28
 *     bits, or more properly (LONGBITS - GCTYPEBITS - 1) bits.
 *
 * For non-integral types, the value bits of Lisp_Object contain a
 * pointer to structure containing the object.  The pointer is
 * obtained by masking off the type and mark bits.
 *
 *     In the USE_MINIMAL_TAGBITS implementation, all
 *     pointer-based types are coalesced under a single type called
 *     Lisp_Type_Record.  The type bits for this type are required
 *     by the implementation to be 00, just like the least
 *     significant bits of word-aligned struct pointers on 32-bit
 *     hardware.  Because of this, Lisp_Object pointers don't have
 *     to be masked and are full-sized.
 *
 *     In the non-USE_MINIMAL_TAGBITS implementation, the type and
 *     mark bits must be masked off and pointers are limited to 28
 *     bits (really LONGBITS - GCTYPEBITS - 1 bits).
 */

#ifdef USE_MINIMAL_TAGBITS
# define Qzero Lisp_Type_Int_Even
# define VALMASK (((1L << (VALBITS)) - 1L) << (GCTYPEBITS))
#else
# define Qzero Lisp_Type_Int
# define VALMASK ((1L << (VALBITS)) - 1L)
# define GCTYPEMASK ((1L << (GCTYPEBITS)) - 1L)
#endif

typedef EMACS_INT Lisp_Object;

#define Qnull_pointer 0

/*
 * There are no mark bits in the USE_MINIMAL_TAGBITS implementation.
 * Integers and characters don't need to be marked.  All other types 
 * are lrecord-based, which means they get marked by incrementing
 * their ->implementation pointer.
 */
#if GCMARKBITS > 0
 /*
  * XMARKBIT accesses the markbit.  Markbits are used only in particular
  * slots of particular structure types.  Other markbits are always
  * zero.  Outside of garbage collection, all mark bits are always zero.
  */
# define MARKBIT (1UL << (VALBITS))
# define XMARKBIT(a) ((a) & (MARKBIT))

# define XMARK(a) ((void) ((a) |= (MARKBIT)))
# define XUNMARK(a) ((void) ((a) &= (~(MARKBIT))))
#else
# define XUNMARK(a) DO_NOTHING
#endif

/*
 * Extract the type bits from a Lisp_Object.  If using USE_MINIMAL_TAGBITS, 
 * the least significant two bits are the type bits.  Otherwise the
 * most significant GCTYPEBITS bits are the type bits. 
 *
 * In the non-USE_MINIMAL_TAGBITS case, one needs to override this
 * if there must be high bits set in data space.  Masking the bits
 * (doing the result of the below & ((1 << (GCTYPEBITS)) - 1) would
 * work on all machines, but would penalize machines which don't
 * need it)
 */
#ifdef USE_MINIMAL_TAGBITS
# define XTYPE(a) ((enum Lisp_Type) (((EMACS_UINT)(a)) & ~(VALMASK)))
#else
# define XTYPE(a) ((enum Lisp_Type) (((EMACS_UINT)(a)) >> ((VALBITS) + 1)))
#endif

/*
 * This applies only to the non-USE_MINIMAL_TAGBITS Lisp_Object.
 *
 * In the past, during garbage collection, XGCTYPE needed to be used
 * for extracting types so that the mark bit was ignored.  XGCTYPE
 * did and exatr & operation to remove the mark bit.  But the mark
 * bit has been since moved so that the type bits could be extracted
 * with a single shift operation, making XGCTYPE no more expensive
 * than XTYPE, so the two operations are now equivalent.	
 */
#ifndef XGCTYPE
# define XGCTYPE(a) XTYPE(a)
#endif

#define EQ(x,y) ((x) == (y))

#ifdef USE_MINIMAL_TAGBITS
# define GC_EQ(x,y) EQ(x,y)
#else
# define GC_EQ(x,y) (XGCTYPE (x) == XGCTYPE (y) && XPNTR (x) == XPNTR (y))
#endif

/*
 * Extract the value of a Lisp_Object as a signed integer.
 *
 * The right shifts below are non-portable because >> is allowed to
 * sign extend or not signed extend signed integers depending on the
 * compiler implementors preference.  But this right-shifting of
 * signed ints has been here forever, so the apparently reality is
 * that all compilers of any consequence do sign extension, which is
 * what is needed here.
 */
#ifndef XREALINT   /* Some machines need to do this differently.  */
# ifdef USE_MINIMAL_TAGBITS
#  define XREALINT(a) ((a) >> (LONGBITS-VALBITS-1))
# else
#  define XREALINT(a) (((a) << (LONGBITS-VALBITS)) >> (LONGBITS-VALBITS))
# endif
#endif

/*
 * Extract the pointer value bits of a pointer based type.
 */
#ifdef USE_MINIMAL_TAGBITS
# define XPNTRVAL(a) (a) /* This depends on Lisp_Type_Record == 0 */
# define XCHARVAL(a) ((a) >> (LONGBITS-VALBITS))
#else
# define XPNTRVAL(a) ((a) & VALMASK)
# define XCHARVAL(a) XPNTRVAL(a)
#endif

#ifdef HAVE_SHM
/* In this representation, data is found in two widely separated segments.  */
extern int pure_size;
#  define XPNTR(a) \
  (XPNTRVAL (a) | (XPNTRVAL (a) > pure_size ? DATA_SEG_BITS : PURE_SEG_BITS))
# else /* not HAVE_SHM */
#  ifdef DATA_SEG_BITS
/* This case is used for the rt-pc.
   In the diffs I was given, it checked for ptr = 0
   and did not adjust it in that case.
   But I don't think that zero should ever be found
   in a Lisp object whose data type says it points to something. */
#   define XPNTR(a) (XPNTRVAL (a) | DATA_SEG_BITS)
#  else
#   define XPNTR(a) XPNTRVAL (a)
#  endif
#endif /* not HAVE_SHM */

#ifdef USE_MINIMAL_TAGBITS

/* XSETINT depends on Lisp_Type_Int_Even == 1 and Lisp_Type_Int_Odd == 3 */
# define XSETINT(var, value) \
  ((void) ((var) = ((value) << (LONGBITS-VALBITS-1)) + 1))
# define XSETCHAR(var, value) \
  ((void) ((var) = ((value) << (LONGBITS-VALBITS)) + Lisp_Type_Char))
# define XSETOBJ(var, type_tag, value) \
  ((void) ((var) = ((EMACS_UINT) (value))))

#else

# define XSETINT(a, b) XSETOBJ (a, Lisp_Type_Int, b)
# define XSETCHAR(var, value) XSETOBJ (var, Lisp_Type_Char, value)
# define XSETOBJ(var, type_tag, value)			\
  ((void) ((var) = (((EMACS_UINT) (type_tag) << ((VALBITS) + 1))	\
                   + ((EMACS_INT) (value) & VALMASK))))
#endif

/* Use this for turning a (void *) into a Lisp_Object, as when the
  Lisp_Object is passed into a toolkit callback function */
#define VOID_TO_LISP(larg,varg) ((void) ((larg) = ((Lisp_Object) (varg))))
#define CVOID_TO_LISP VOID_TO_LISP

/* Use this for turning a Lisp_Object into a (void *), as when the
   Lisp_Object is passed into a toolkit callback function */
#define LISP_TO_VOID(larg) ((void *) (larg))
#define LISP_TO_CVOID(varg) ((CONST void *) (larg))

/* Convert a Lisp_Object into something that can't be used as an
   lvalue.  Useful for type-checking. */
#define NON_LVALUE(larg) ((larg) + 0)
