/* Fundamental definitions for XEmacs Lisp interpreter -- union objects.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.

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
   of the union is dependent on ENDIAN-ness and USE_MINIMAL_TAGBITS.
   See lisp-disunion.h for more details.  */

typedef
union Lisp_Object
{
  /* if non-valbits are at lower addresses */
#if defined(WORDS_BIGENDIAN) == defined(USE_MINIMAL_TAGBITS)
  struct
  {
    EMACS_UINT val : VALBITS;
#if GCMARKBITS > 0
    unsigned int markbit: GCMARKBITS;
#endif
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
#if GCMARKBITS > 0
    unsigned int markbit: GCMARKBITS;
#endif
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

  /* This was formerly declared 'void *v' etc. but that causes
     GCC to accept any (yes, any) pointer as the argument of
     a function declared to accept a Lisp_Object. */
  struct nosuchstruct *v;
  CONST struct nosuchstruct *cv;
}
Lisp_Object;

#define XCHARVAL(x) ((x).gu.val)

#ifdef USE_MINIMAL_TAGBITS
# define XSETINT(var, value) do {	\
  Lisp_Object *_xzx = &(var);		\
  _xzx->s.val = (value);		\
  _xzx->s.bits = 1;			\
} while (0)
# define XSETCHAR(var, value) do {	\
  Lisp_Object *_xzx = &(var);		\
  _xzx->gu.val = (EMACS_UINT) (value);	\
  _xzx->gu.type = Lisp_Type_Char;	\
} while (0)
# define XSETOBJ(var, vartype, value)	\
  ((void) ((var).ui = (EMACS_UINT) (value)))
# define XPNTRVAL(x) ((x).ui)
#else /* ! USE_MINIMAL_TAGBITS */
# define XSETOBJ(var, vartype, value) do {	\
  Lisp_Object *_xzx = &(var);			\
  _xzx->gu.val = (EMACS_UINT) (value);		\
  _xzx->gu.type = (vartype);			\
  _xzx->gu.markbit = 0;				\
} while (0)
# define XSETINT(var, value) XSETOBJ (var, Lisp_Type_Int, value)
# define XSETCHAR(var, value) XSETOBJ (var, Lisp_Type_Char, value)
# define XPNTRVAL(x) ((x).gu.val)
#endif /* ! USE_MINIMAL_TAGBITS */

INLINE Lisp_Object make_int (EMACS_INT val);
INLINE Lisp_Object
make_int (EMACS_INT val)
{
  Lisp_Object obj;
  XSETINT(obj, val);
  return obj;
}

INLINE Lisp_Object make_char (Emchar val);
INLINE Lisp_Object
make_char (Emchar val)
{
  Lisp_Object obj;
  XSETCHAR(obj, val);
  return obj;
}

extern Lisp_Object Qnull_pointer, Qzero;

#define XREALINT(x) ((x).s.val)
#define XUINT(x) ((x).u.val)
#define XTYPE(x) ((x).gu.type)
#define XGCTYPE(x) XTYPE (x)
#define EQ(x,y) ((x).v == (y).v)

#ifdef USE_MINIMAL_TAGBITS
#define INTP(x) ((x).s.bits)
#define GC_EQ(x,y) EQ (x, y)
#else
#define INTP(x) (XTYPE(x) == Lisp_Type_Int)
#define GC_EQ(x,y) ((x).gu.val == (y).gu.val && XTYPE (x) == XTYPE (y))
#endif

#if GCMARKBITS > 0
/* XMARKBIT accesses the markbit.  Markbits are used only in
   particular slots of particular structure types.  Other markbits are
   always zero.  Outside of garbage collection, all mark bits are
   always zero. */
# define XMARKBIT(x) ((x).gu.markbit)
# define XMARK(x) ((void) (XMARKBIT (x) = 1))
# define XUNMARK(x) ((void) (XMARKBIT (x) = 0))
#else
# define XUNMARK(x) DO_NOTHING
#endif

/* Convert between a (void *) and a Lisp_Object, as when the
   Lisp_Object is passed to a toolkit callback function */
#define VOID_TO_LISP(larg,varg) \
     ((void) ((larg).v = (struct nosuchstruct *) (varg)))
#define CVOID_TO_LISP(larg,varg) \
     ((void) ((larg).cv = (CONST struct nosuchstruct *) (varg)))
#define LISP_TO_VOID(larg) ((void *) ((larg).v))
#define LISP_TO_CVOID(larg) ((CONST void *) ((larg).cv))

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
