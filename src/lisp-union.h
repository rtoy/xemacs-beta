/* Fundamental definitions for XEmacs Lisp interpreter -- union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.
   Copyright (C) 2002, 2005 Ben Wing.

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

/* Divergent from FSF.  */

/* Definition of Lisp_Object type as a union.
   The declaration order of the objects within the struct members
   of the union is dependent on ENDIAN-ness.
   See lisp-disunion.h for more details.  */

typedef
union Lisp_Object
{
  /* if non-valbits are at lower addresses */
#ifdef WORDS_BIGENDIAN
  struct
  {
    EMACS_UINT val : VALBITS;
    enum_field (Lisp_Type) type : GCTYPEBITS;
  } gu;

  struct
  {
    signed EMACS_INT val : INT_VALBITS;
    unsigned int bits : INT_GCBITS;
  } s;

  struct
  {
    EMACS_UINT val : INT_VALBITS;
    unsigned int bits : INT_GCBITS;
  } u;
#else /* non-valbits are at higher addresses */
  struct
  {
    enum_field (Lisp_Type) type : GCTYPEBITS;
    EMACS_UINT val : VALBITS;
  } gu;

  struct
  {
    unsigned int bits : INT_GCBITS;
    signed EMACS_INT val : INT_VALBITS;
  } s;

  struct
  {
    unsigned int bits : INT_GCBITS;
    EMACS_UINT val : INT_VALBITS;
  } u;

#endif /* non-valbits are at higher addresses */

  EMACS_UINT ui;
  signed EMACS_INT i;

  /* This was formerly declared `void *v' etc. but that causes
     GCC to accept any (yes, any) pointer as the argument of
     a function declared to accept a Lisp_Object. */
  struct nosuchstruct *v;
}
Lisp_Object;

#define XCHARVAL(x) ((x).gu.val)
#define XPNTRVAL(x) ((x).ui)

#define XREALINT(x) ((x).s.val)
#define XUINT(x) ((x).u.val)
#define XTYPE(x) ((x).gu.type)
#define EQ(x,y) ((x).v == (y).v)

DECLARE_INLINE_HEADER (
Lisp_Object
make_int_verify (EMACS_INT val)
)
{
  Lisp_Object obj;
  obj.s.bits = 1;
  obj.s.val = val;
  type_checking_assert (XREALINT (obj) == val);
  return obj;
}

DECLARE_INLINE_HEADER (
Lisp_Object
make_int (EMACS_INT val)
)
{
  Lisp_Object obj;
  obj.s.bits = 1;
  obj.s.val = val;
  return obj;
}

DECLARE_INLINE_HEADER (
Lisp_Object
make_char_1 (Ichar val)
)
{
  Lisp_Object obj;
  obj.gu.type = Lisp_Type_Char;
  obj.gu.val = val;
  return obj;
}

DECLARE_INLINE_HEADER (
Lisp_Object
wrap_pointer_1 (const void *ptr)
)
{
  Lisp_Object obj;
  obj.ui = (EMACS_UINT) ptr;
  return obj;
}

extern MODULE_API Lisp_Object Qnull_pointer, Qzero;

#define INTP(x) ((x).s.bits)
#define INT_PLUS(x,y)  make_int (XINT (x) + XINT (y))
#define INT_MINUS(x,y) make_int (XINT (x) - XINT (y))
#define INT_PLUS1(x)   make_int (XINT (x) + 1)
#define INT_MINUS1(x)  make_int (XINT (x) - 1)

/* WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   You can only VOID_TO_LISP something that had previously been
   LISP_TO_VOID'd.  You cannot go the other way, i.e. create a bogus
   Lisp_Object.  If you want to stuff a void * into a Lisp_Object, use
   make_opaque_ptr(). */

/* Convert between a (void *) and a Lisp_Object, as when the
   Lisp_Object is passed to a toolkit callback function */
DECLARE_INLINE_HEADER (
Lisp_Object
VOID_TO_LISP (const void *arg)
)
{
  Lisp_Object larg;
  larg.v = (struct nosuchstruct *) arg;
  return larg;
}

#define LISP_TO_VOID(larg) ((void *) ((larg).v))

/* Convert a Lisp_Object into something that can't be used as an
   lvalue.  Useful for type-checking. */
#if (__GNUC__ > 1)
#define NON_LVALUE(larg) ({ (larg); })
#else
/* Well, you can't really do it without using a function call, and
   there's no real point in that; no-union-type is the rule, and that
   will catch errors. */
#define NON_LVALUE(larg) (larg)
#endif
