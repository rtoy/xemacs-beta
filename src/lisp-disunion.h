/* Fundamental definitions for XEmacs Lisp interpreter -- non-union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: FSF 19.30.  Split out from lisp.h. */
/* This file has diverged greatly from FSF Emacs.  Syncing is no
   longer desirable or possible */

/*
 Format of a non-union-type Lisp Object

             3         2         1         0
       bit  10987654321098765432109876543210
            --------------------------------
            VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVTT

   Integers are treated specially, and look like this:

             3         2         1         0
       bit  10987654321098765432109876543210
            --------------------------------
            VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVT

 For integral Lisp types, i.e. integers and characters, the value
 bits are the Lisp object.  Some people call such Lisp_Objects "immediate".

 The object is obtained by masking off the type bits.
     Bit 1 is used as a value bit by splitting the Lisp integer type
 into two subtypes, Lisp_Type_Fixnum_Even and Lisp_Type_Fixnum_Odd.
 By this trickery we get 31 bits for integers instead of 30.

 For non-integral types, the value bits of a Lisp_Object contain
 a pointer to a structure containing the object.  The pointer is
 obtained by masking off the type and mark bits.

     All pointer-based types are coalesced under a single type called
 Lisp_Type_Record.  The type bits for this type are required by the
 implementation to be 00, just like the least significant bits of
 word-aligned struct pointers on 32-bit hardware.  This requires that
 all structs implementing Lisp_Objects have an alignment of at least 4
 bytes.  Because of this, Lisp_Object pointers don't have to be masked
 and are full-sized.

 There are no mark bits in the Lisp_Object itself (there used to be).

 Integers and characters don't need to be marked.  All other types are
 lrecord-based, which means they get marked by setting the mark bit in
 the struct lrecord_header.

 Here is a brief description of the following macros:

 XTYPE     The type bits of a Lisp_Object
 XPNTRVAL  The value bits of a Lisp_Object storing a pointer
 XCHARVAL  The value bits of a Lisp_Object storing a Ichar
 XREALFIXNUM  The value bits of a Lisp_Object storing an integer, signed
 XUINT     The value bits of a Lisp_Object storing an integer, unsigned
 FIXNUMP      Non-zero if this Lisp_Object is an integer
 Qzero     Lisp Integer 0
 EQ        Non-zero if two Lisp_Objects are identical, not merely equal. */


typedef EMACS_INT Lisp_Object;

#define Lisp_Type_Fixnum_Bit (Lisp_Type_Fixnum_Even & Lisp_Type_Fixnum_Odd)
#define VALMASK (((1UL << VALBITS) - 1UL) << GCTYPEBITS)
#define XTYPE(x) ((enum Lisp_Type) (((EMACS_UINT)(x)) & ~VALMASK))
#define XPNTRVAL(x) (x) /* This depends on Lisp_Type_Record == 0 */
#define XCHARVAL(x) ((x) >> GCBITS)
#define XREALFIXNUM(x) ((x) >> FIXNUM_GCBITS)
#define XUINT(x) ((EMACS_UINT)(x) >> FIXNUM_GCBITS)

#define wrap_pointer_1(ptr) ((Lisp_Object) (ptr))

DECLARE_INLINE_HEADER (
Lisp_Object
make_fixnum_verify (EMACS_INT val)
)
{
  Lisp_Object obj = (Lisp_Object) ((val << FIXNUM_GCBITS) | Lisp_Type_Fixnum_Bit);
  type_checking_assert (XREALFIXNUM (obj) == val);
  return obj;
}

#define make_fixnum(x) ((Lisp_Object) (((x) << FIXNUM_GCBITS) | Lisp_Type_Fixnum_Bit))

#define make_char_1(x) ((Lisp_Object) (((x) << GCBITS) | Lisp_Type_Char))

#define FIXNUMP(x) ((EMACS_UINT)(x) & Lisp_Type_Fixnum_Bit)
#define FIXNUM_PLUS(x,y)  ((x)+(y)-Lisp_Type_Fixnum_Bit)
#define FIXNUM_MINUS(x,y) ((x)-(y)+Lisp_Type_Fixnum_Bit)
#define FIXNUM_PLUS1(x)   FIXNUM_PLUS  (x, make_fixnum (1))
#define FIXNUM_MINUS1(x)  FIXNUM_MINUS (x, make_fixnum (1))

#define Qzero make_fixnum (0)
#define Qnull_pointer ((Lisp_Object) 0)
#define EQ(x,y) ((x) == (y))

/* WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   You can only GET_LISP_FROM_VOID something that had previously been
   STORE_LISP_IN_VOID'd.  If you want to go the other way, use
   STORE_VOID_IN_LISP and GET_VOID_FROM_LISP, or use make_opaque_ptr(). */

/* Convert a Lisp object to a void * pointer, as when it needs to be passed
   to a toolkit callback function */
#define STORE_LISP_IN_VOID(larg) ((void *) (larg))

/* Convert a void * pointer back into a Lisp object, assuming that the
   pointer was generated by STORE_LISP_IN_VOID. */
#define GET_LISP_FROM_VOID(varg) ((Lisp_Object) (varg))

/* Convert a Lisp_Object into something that can't be used as an
   lvalue.  Useful for type-checking. */
#define NON_LVALUE(larg) ((larg) + 0)
