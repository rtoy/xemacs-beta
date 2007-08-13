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

/* Synched up with: FSF 19.30.  Split out from lisp.h. */

typedef
union Lisp_Object
{
  struct
  {
#if (!!defined (WORDS_BIGENDIAN) != !!defined (LOWTAGS))
    /* Big-endian lowtags, little-endian hightags */
    unsigned EMACS_INT type_mark: GCTYPEBITS + GCMARKBITS;
    signed EMACS_INT val: VALBITS;
#else /* If WORDS_BIGENDIAN, or little-endian hightags */
    signed EMACS_INT val: VALBITS;
    unsigned EMACS_INT mark_type: GCTYPEBITS + GCMARKBITS;
#endif /* BIG/LITTLE_ENDIAN vs HIGH/LOWTAGS */
  } s;
  struct
  {
#if (!!defined (WORDS_BIGENDIAN) == !!defined (LOWTAGS))
    unsigned EMACS_INT val: VALBITS;
#endif
#ifdef __GNUC__ /* Non-ANSI extension */
    enum Lisp_Type type: GCTYPEBITS;
#else
    unsigned EMACS_INT type: GCTYPEBITS;
#endif /* __GNUC__ */
    /* The markbit is not really part of the value of a Lisp_Object,
       and is always zero except during garbage collection.  */
#if GCMARKBITS > 0
    unsigned EMACS_INT markbit: GCMARKBITS;
#endif
#if (!!defined (WORDS_BIGENDIAN)  != !!defined (LOWTAGS))
    unsigned EMACS_INT val: VALBITS;
#endif
  } gu;
#ifdef USE_MINIMAL_TAGBITS
  struct
  {
#if (!!defined (WORDS_BIGENDIAN) != !!defined (LOWTAGS))
    unsigned bit: GCTYPEBITS - 1;
#endif
    signed EMACS_INT val: VALBITS + 1;
#if (!!defined (WORDS_BIGENDIAN) == !!defined (LOWTAGS))
    unsigned bit: GCTYPEBITS - 1;
#endif
  } si;
  struct
  {
#if (!!defined (WORDS_BIGENDIAN) != !!defined (LOWTAGS))
    unsigned bit: GCTYPEBITS - 1;
#endif
    unsigned EMACS_INT val: VALBITS + 1;
#if (!!defined (WORDS_BIGENDIAN) == !!defined (LOWTAGS))
    unsigned bit: GCTYPEBITS - 1;
#endif
  } u_i;
#endif /* USE_MINIMAL_TAGBITS */
  EMACS_UINT ui;
  EMACS_INT i;
  /* GCC bites yet again.  I fart in the general direction of
     the GCC authors.

     This was formerly declared 'void *v' etc. but that causes
     GCC to accept any (yes, any) pointer as the argument of
     a function declared to accept a Lisp_Object. */
  struct __nosuchstruct__ *v;
  CONST struct __nosuchstruct__ *cv;             /* C wanks */
}
Lisp_Object;

#ifndef USE_MINIMAL_TAGBITS
#ifndef XMAKE_LISP
#if (__GNUC__ > 1)
/* Use GCC's struct initializers feature */
#define XMAKE_LISP(vartype,value) \
   ((union Lisp_Object) { gu: { markbit: 0, \
                                type: (vartype), \
                                val: ((unsigned EMACS_INT) value) } })
#endif /* __GNUC__ */
#endif /* !XMAKE_LISP */
#endif /* ! USE_MINIMAL_TAGBITS */

#ifdef XMAKE_LISP
#define Qzero (XMAKE_LISP (Lisp_Type_Int, 0))
#define make_int(a) (XMAKE_LISP (Lisp_Type_Int, (a)))
#define make_char(a) (XMAKE_LISP (Lisp_Type_Char, (a)))
#else
extern Lisp_Object Qzero;
#endif

extern Lisp_Object Qnull_pointer;

#define EQ(x,y) ((x).v == (y).v)
#define GC_EQ(x,y) ((x).gu.val == (y).gu.val && (x).gu.type == (y).gu.type)

#define XTYPE(a) ((enum Lisp_Type) (a).gu.type)
#define XGCTYPE(a) XTYPE (a)

/* This was commented out a long time ago.  I uncommented it, but it
   makes the Alpha crash, and that's the only system that would use
   this, so it stays commented out. */
#if 0 /* EXPLICIT_SIGN_EXTEND */
/* Make sure we sign-extend; compilers have been known to fail to do so.  */
#define XREALINT(a) (((a).i << ((LONGBITS) - (VALBITS))) >> ((LONGBITS) - (VALBITS)))
#else
#ifdef USE_MINIMAL_TAGBITS
# define XREALINT(a) ((a).si.val)
#else
# define XREALINT(a) ((a).s.val)
#endif
#endif /* EXPLICIT_SIGN_EXTEND */

#ifdef USE_MINIMAL_TAGBITS
# define XUINT(a) ((a).u_i.val)
#else
# define XUINT(a) XPNTRVAL(a)
#endif

#ifdef USE_MINIMAL_TAGBITS
# define XPNTRVAL(a) ((a).ui)
# define XCHARVAL(a) ((a).gu.val)
#else
# define XPNTRVAL(a) ((a).gu.val)
# define XCHARVAL(a) XPNTRVAL(a)
#endif

#ifdef HAVE_SHM
/* In this representation, data is found in two widely separated segments.  */
extern int pure_size;
# define XPNTR(a) \
  ((void *)(XPNTRVAL(a)) | (XPNTRVAL(a) > pure_size ? DATA_SEG_BITS : PURE_SEG_BITS)))
#else /* not HAVE_SHM */
# ifdef DATA_SEG_BITS
/* This case is used for the rt-pc and hp-pa.
   In the diffs I was given, it checked for ptr = 0
   and did not adjust it in that case.
   But I don't think that zero should ever be found
   in a Lisp object whose data type says it points to something.
 */
#  define XPNTR(a) ((void *)((XPNTRVAL(a)) | DATA_SEG_BITS))
# else /* not DATA_SEG_BITS */
#  define XPNTR(a) ((void *) (XPNTRVAL(a)))
# endif /* not DATA_SEG_BITS */
#endif /* not HAVE_SHM */

#ifdef USE_MINIMAL_TAGBITS
# define XSETINT(a, b) \
    do { Lisp_Object *_xzx = &(a) ; \
         (*_xzx).si.val = (b) ; \
         (*_xzx).si.bit = 1; \
       } while (0)
# define XSETCHAR(a, b) \
    do { Lisp_Object *_xzx = &(a) ; \
         (*_xzx).gu.val = (b) ; \
         (*_xzx).gu.type = Lisp_Type_Char; \
       } while (0)
#else
# define XSETINT(a, b) ((void) ((a) = make_int (b)))
# define XSETCHAR(a, b) ((void) ((a) = make_char (b)))
#endif

/* XSETOBJ was formerly named XSET.  The name change was made to catch
   C code that attempts to use this macro.  You should always use the
   individual settor macros (XSETCONS, XSETBUFFER, etc.) instead. */

#ifdef USE_MINIMAL_TAGBITS
# define XSETOBJ(var, vartype, value) \
   ((void) ((var).ui = (EMACS_UINT)(value)))
#else
# ifdef XMAKE_LISP
#  define XSETOBJ(a, type, b) ((void) ((a) = XMAKE_LISP (type, b)))
# else
/* This is haired up to avoid evaluating var twice...
   This is necessary only in the "union" version.
   The "int" version has never done double evaluation.
 */
/* XEmacs change: put the assignment to val first; otherwise you
   can trip up the error_check_*() stuff */
#  define XSETOBJ(var, vartype, value)			\
   do {							\
	 Lisp_Object *tmp_xset_var = &(var);		\
	 (*tmp_xset_var).s.val = ((EMACS_INT) (value));	\
	 (*tmp_xset_var).gu.markbit = 0;		\
	 (*tmp_xset_var).gu.type = (vartype);		\
      } while (0)
# endif /* ! XMAKE_LISP */
#endif /* ! USE_MINIMAL_TAGBITS */

#if GCMARKBITS > 0
/*
 * XMARKBIT access the markbit.  Markbits are used only in particular
 * slots of particular structure types.  Other markbits are always
 * zero.  Outside of garbage collection, all mark bits are always
 * zero.
 */
# define XMARKBIT(a) ((a).gu.markbit)
# define XMARK(a) ((void) (XMARKBIT (a) = 1))
# define XUNMARK(a) ((void) (XMARKBIT (a) = 0))
#else
# define XUNMARK(a) DO_NOTHING
#endif

/* Use this for turning a (void *) into a Lisp_Object, as when the
  Lisp_Object is passed into a toolkit callback function */
#define VOID_TO_LISP(larg,varg) \
     ((void) ((larg).v = (struct __nosuchstruct__ *) (varg)))
#define CVOID_TO_LISP(larg,varg) \
     ((void) ((larg).cv = (CONST struct __nosuchstruct__ *) (varg)))

/* Use this for turning a Lisp_Object into a  (void *), as when the
  Lisp_Object is passed into a toolkit callback function */
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
