/* Fundamental definitions for XEmacs Lisp interpreter.
   Copyright (C) 1985-1987, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1996 Richard Mlynarik.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: FSF 19.30. */

#ifndef _XEMACS_LISP_H_
#define _XEMACS_LISP_H_

/************************************************************************/
/*                        general definitions                           */
/************************************************************************/

/* We include the following generally useful header files so that you
   don't have to worry about prototypes when using the standard C
   library functions and macros.  These files shouldn't be excessively
   large so they shouldn't cause that much of a slowdown. */

#include <stdlib.h>
#include <string.h>		/* primarily for memcpy, etc. */
#include <stdio.h>		/* NULL, etc. */
#include <ctype.h>
#include <stdarg.h>
#include <limits.h>

/* Define INT_MAX, DBL_DIG if not in limits.h */
#ifndef INT_MAX
#define INT_MAX ((int) ((1U << (INTBITS - 1)) - 1))
#endif
#ifndef DBL_DIG
#define DBL_DIG 16
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef INCLUDED_FCNTL
# define INCLUDED_FCNTL
# include <fcntl.h>
#endif /* INCLUDED_FCNTL */

#ifdef __lucid
# include <sysent.h>
#endif

#include "blocktype.h"		/* A generally useful include */
#include "dynarr.h"		/* A generally useful include */
#include "symsinit.h"		/* compiler warning suppression */

/* Also define min() and max(). (Some compilers put them in strange
   places that won't be referenced by the above include files, such
   as 'macros.h' under Solaris.) */

#ifndef min
#define min(a,b) (((a) <= (b)) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif

/* Emacs needs to use its own definitions of certain system calls on
   some systems (like SunOS 4.1 and USG systems, where the read system
   call is interruptible but Emacs expects it not to be; and under
   MULE, where all filenames need to be converted to external format).
   To do this, we #define read to be sys_read, which is defined in
   sysdep.c.  We first #undef read, in case some system file defines
   read as a macro.  sysdep.c doesn't encapsulate read, so the call to
   read inside of sys_read will do the right thing.

   DONT_ENCAPSULATE is used in files such as sysdep.c that want to
   call the actual system calls rather than the encapsulated versions.
   Those files can call sys_read to get the (possibly) encapsulated
   versions.

   IMPORTANT: the redefinition of the system call must occur *after* the
   inclusion of any header files that declare or define the system call;
   otherwise lots of unfriendly things can happen.  This goes for all
   encapsulated system calls.

   We encapsulate the most common system calls here; we assume their
   declarations are in one of the standard header files included above.
   Other encapsulations are declared in the appropriate sys*.h file. */

#if defined (ENCAPSULATE_READ) && !defined (DONT_ENCAPSULATE)
# undef read
# define read sys_read
#endif
#if !defined (ENCAPSULATE_READ) && defined (DONT_ENCAPSULATE)
# define sys_read read
#endif

#if defined (ENCAPSULATE_WRITE) && !defined (DONT_ENCAPSULATE)
# undef write
# define write sys_write
#endif
#if !defined (ENCAPSULATE_WRITE) && defined (DONT_ENCAPSULATE)
# define sys_write write
#endif

#if defined (ENCAPSULATE_OPEN) && !defined (DONT_ENCAPSULATE)
# undef open
# define open sys_open
#endif
#if !defined (ENCAPSULATE_OPEN) && defined (DONT_ENCAPSULATE)
# define sys_open open
#endif

#if defined (ENCAPSULATE_CLOSE) && !defined (DONT_ENCAPSULATE)
# undef close
# define close sys_close
#endif
#if !defined (ENCAPSULATE_CLOSE) && defined (DONT_ENCAPSULATE)
# define sys_close close
#endif

/* Now the stdio versions ... */

#if defined (ENCAPSULATE_FREAD) && !defined (DONT_ENCAPSULATE)
# undef fread
# define fread sys_fread
#endif
#if !defined (ENCAPSULATE_FREAD) && defined (DONT_ENCAPSULATE)
# define sys_fread fread
#endif

#if defined (ENCAPSULATE_FWRITE) && !defined (DONT_ENCAPSULATE)
# undef fwrite
# define fwrite sys_fwrite
#endif
#if !defined (ENCAPSULATE_FWRITE) && defined (DONT_ENCAPSULATE)
# define sys_fwrite fwrite
#endif

#if defined (ENCAPSULATE_FOPEN) && !defined (DONT_ENCAPSULATE)
# undef fopen
# define fopen sys_fopen
#endif
#if !defined (ENCAPSULATE_FOPEN) && defined (DONT_ENCAPSULATE)
# define sys_fopen fopen
#endif

#if defined (ENCAPSULATE_FCLOSE) && !defined (DONT_ENCAPSULATE)
# undef fclose
# define fclose sys_fclose
#endif
#if !defined (ENCAPSULATE_FCLOSE) && defined (DONT_ENCAPSULATE)
# define sys_fclose fclose
#endif

/* Memory allocation */
void malloc_warning (CONST char *);
void *xmalloc (size_t size);
void *xmalloc_and_zero (size_t size);
void *xrealloc (void *, size_t size);
char *xstrdup (CONST char *);
/* generally useful */
#define countof(x) (sizeof(x)/sizeof(x[0]))
#define slot_offset(type, slot_name) \
  ((unsigned) (((char *) (&(((type *)0)->slot_name))) - ((char *)0)))
#define xnew(type) ((type *) xmalloc (sizeof (type)))
#define xnew_array(type, len) ((type *) xmalloc ((len) * sizeof (type)))
#define xnew_and_zero(type) ((type *) xmalloc_and_zero (sizeof (type)))
#define xnew_array_and_zero(type, len) ((type *) xmalloc_and_zero ((len) * sizeof (type)))
#define XREALLOC_ARRAY(ptr, type, len) ((void) (ptr = (type *) xrealloc (ptr, (len) * sizeof (type))))
#define alloca_array(type, len) ((type *) alloca ((len) * sizeof (type)))

/* also generally useful if you want to avoid arbitrary size limits
   but don't need a full dynamic array.  Assumes that BASEVAR points
   to a malloced array of TYPE objects (or possibly a NULL pointer,
   if SIZEVAR is 0), with the total size stored in SIZEVAR.  This
   macro will realloc BASEVAR as necessary so that it can hold at
   least NEEDED_SIZE objects.  The reallocing is done by doubling,
   which ensures constant amortized time per element. */
#define DO_REALLOC(basevar, sizevar, needed_size, type)	do	\
{								\
  /* Avoid side-effectualness. */				\
  /* Dammit! Macros suffer from dynamic scope! */		\
  /* We demand inline functions! */				\
  int do_realloc_needed_size = (needed_size);			\
  int newsize = 0;						\
  while ((sizevar) < (do_realloc_needed_size)) {		\
    newsize = 2*(sizevar);					\
    if (newsize < 32)						\
      newsize = 32;						\
    (sizevar) = newsize;					\
  }								\
  if (newsize)							\
    XREALLOC_ARRAY (basevar, type, newsize);			\
} while (0)

#ifdef ERROR_CHECK_MALLOC
void xfree_1 (void *);
#define xfree(lvalue) do		\
{					\
  void **ptr = (void **) &(lvalue);	\
  xfree_1 (*ptr);			\
  *ptr = (void *) 0xDEADBEEF;		\
} while (0)
#else
void xfree (void *);
#define xfree_1 xfree
#endif /* ERROR_CHECK_MALLOC */

/* We assume an ANSI C compiler and libraries and memcpy, memset, memcmp */
/*  (This definition is here because system header file macros may want
 *   to call bzero (eg FD_ZERO) */
#ifndef bzero
# define bzero(m, l) memset ((m), 0, (l))
#endif

#ifndef PRINTF_ARGS
# if defined (__GNUC__) && (__GNUC__ >= 2)
#  define PRINTF_ARGS(string_index,first_to_check) \
          __attribute__ ((format (printf, string_index, first_to_check)))
# else
#  define PRINTF_ARGS(string_index,first_to_check)
# endif /* GNUC */
#endif

#ifndef DOESNT_RETURN
# if defined __GNUC__
#  if ((__GNUC__ > 2) || (__GNUC__ == 2) && (__GNUC_MINOR__ >= 5))
#   define DOESNT_RETURN void volatile
#   define DECLARE_DOESNT_RETURN(decl) \
           extern void volatile decl __attribute__ ((noreturn))
#   define DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS(decl,str,idx) \
     /* Should be able to state multiple independent __attribute__s, but  \
        the losing syntax doesn't work that way, and screws losing cpp */ \
           extern void volatile decl \
                  __attribute__ ((noreturn, format (printf, str, idx)))
#  else
#   define DOESNT_RETURN void volatile
#   define DECLARE_DOESNT_RETURN(decl) extern void volatile decl
#   define DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS(decl,str,idx) \
           extern void volatile decl PRINTF_ARGS(str,idx)
#  endif /* GNUC 2.5 */
# else
#  define DOESNT_RETURN void
#  define DECLARE_DOESNT_RETURN(decl) extern void decl
#  define DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS(decl,str,idx) \
          extern void decl PRINTF_ARGS(str,idx)
# endif /* GNUC */
#endif

#ifndef ALIGNOF
# if defined (__GNUC__) && (__GNUC__ >= 2)
#  define ALIGNOF(x) __alignof (x)
# else
#  define ALIGNOF(x) sizeof (x)
# endif
#endif

#define ALIGN_SIZE(len, unit) \
  ((((len) + (unit) - 1) / (unit)) * (unit))

/* #### Yuck, this is kind of evil */
#define ALIGN_PTR(ptr, unit) \
  ((void *) ALIGN_SIZE ((long) (ptr), unit))

#ifdef QUANTIFY
#include "quantify.h"
#define QUANTIFY_START_RECORDING quantify_start_recording_data ()
#define QUANTIFY_STOP_RECORDING  quantify_stop_recording_data  ()
#else /* !QUANTIFY */
#define QUANTIFY_START_RECORDING
#define QUANTIFY_STOP_RECORDING
#endif /* !QUANTIFY */

#ifndef DO_NOTHING
#define DO_NOTHING do {} while (0)
#endif

/* We define assert iff USE_ASSERTIONS or DEBUG_XEMACS is defined.
   Otherwise we it to NULL.  Quantify has shown that the time the
   assert checks take is measurable so let's not include them in
   production binaries. */

#ifdef USE_ASSERTIONS
/* Highly dubious kludge */
/*   (thanks, Jamie, I feel better now -- ben) */
DECLARE_DOESNT_RETURN (assert_failed (CONST char *, int, CONST char *));
# define abort() (assert_failed (__FILE__, __LINE__, "abort()"))
# define assert(x) ((x) ? (void) 0 : assert_failed (__FILE__, __LINE__, #x))
#else
# ifdef DEBUG_XEMACS
#  define assert(x) ((x) ? (void) 0 : (void) abort ())
# else
#  define assert(x)
# endif
#endif

/*#ifdef DEBUG_XEMACS*/
#define REGISTER
#define register
/*#else*/
/*#define REGISTER register*/
/*#endif*/

#if defined (__GNUC__) && (__GNUC__ >= 2)
/* Entomological studies have revealed that the following junk is
   necessary under GCC.  GCC has a compiler bug where incorrect
   code will be generated if you use a global temporary variable
   in a macro and the macro occurs twice in the same expression.
   As it happens, we can avoid this problem using a GCC language
   extension.  Thus we play weird games with syntax to avoid having
   to provide two definitions for lots of macros.

   The approximate way this works is as follows:

   1. Use these macros whenever you want to avoid evaluating an
      argument more than once in a macro. (It's almost always a
      good idea to make your macros safe like this.)
   2. Choose a name for the temporary variable you will store
      the parameter in.  It should begin with `MT' and
      be distinguishing, since it will (or may) be a global
      variable.
   3. In the same header file as the macro, put in a
      MAC_DECLARE_EXTERN for the temporary variable.  This
      resolves to an external variable declaration for some
      compilers.
   4. Put a MAC_DEFINE for the variable in a C file somewhere.
      This resolves to a variable definition for some compilers.
   5. Write your macro with no semicolons or commas in it.
      Remember to use parentheses to surround macro arguments,
      but you do not need to surround each separate statement
      or the temporary variable with parentheses.
   6. Write your macro like this:

#define foo(bar,baz)						\
MAC_BEGIN							\
  MAC_DECLARE (struct frobozz *, MTfoobar, bar)			\
  SOME_EXPRESSION						\
  MAC_SEP							\
  SOME OTHER EXPRESSION						\
MAC_END

   7. You only need to use MAC_SEP if you have more than one
      expression in the macro, not counting any MAC_DECLARE
      statements.

  DONT_DECLARE_MAC_VARS is used in signal.c, for asynchronous signals.
  All functions that may be called from within an asynchronous signal
  handler must declare local variables (with MAC_DECLARE_LOCAL) for
  the (normally global) variables used in these sorts of macros.
  Otherwise, a signal could occur in the middle of processing one
  of these macros and the signal handler could use the same macro,
  resulting in the global variable getting overwritten and yielding
  nasty evil crashes that are very difficult to track down.
*/
# define MAC_BEGIN ({
# define MAC_DECLARE(type, var, value) type var = (value);
# define MAC_SEP ;
# define MAC_END ; })
# define MAC_DECLARE_EXTERN(type, var)
# define MAC_DECLARE_LOCAL(type, var)
# define MAC_DEFINE(type, var)
#else
# define MAC_BEGIN (
# define MAC_DECLARE(type, var, value) var = (value),
# define MAC_SEP ,
# define MAC_END )
# ifdef DONT_DECLARE_MAC_VARS
#  define MAC_DECLARE_EXTERN(type, var)
# else
#  define MAC_DECLARE_EXTERN(type, var) extern type var;
# endif
# define MAC_DECLARE_LOCAL(type, var) type var;
# define MAC_DEFINE(type, var) type var;
#endif

/* For Lo, the Lord didst appear and look upon the face of the code,
   and the Lord was unhappy with the strange syntax that had come
   into vogue with the cryptic name of "C".  And so the Lord didst
   decree, that from now on all programmers shall use Pascal syntax,
   a syntax truly and in sooth ordained in heaven.  Amen. */


/************************************************************************/
/*                                typedefs                              */
/************************************************************************/

/* We put typedefs here so that prototype declarations don't choke.
   Note that we don't actually declare the structures here (except
   maybe for simple structures like Dynarrs); that keeps them private
   to the routines that actually use them. */

/* The data representing the text in a buffer is logically a set
   of Bufbytes, declared as follows. */

typedef unsigned char Bufbyte;

/* The data representing a string in "external" format (simple
   binary format) is logically a set of Extbytes, declared as follows. */

typedef unsigned char Extbyte;

/* To the user, a buffer is made up of characters, declared as follows.
   In the non-Mule world, characters and Bufbytes are equivalent.
   In the Mule world, a character requires (typically) 1 to 4
   Bufbytes for its representation in a buffer. */

typedef int Emchar;

/* Different ways of referring to a position in a buffer.  We use
   the typedefs in preference to 'int' to make it clearer what
   sort of position is being used.  See extents.c for a description
   of the different positions.  We put them here instead of in
   buffer.h (where they rightfully belong) to avoid syntax errors
   in function prototypes. */

typedef int Bufpos;
typedef int Bytind;
typedef int Memind;

/* Counts of bytes or chars */

typedef int Bytecount;
typedef int Charcount;

/* Length in bytes of a string in external format */
typedef int Extcount;

typedef struct lstream Lstream;

typedef unsigned int face_index;

typedef struct
{
  Dynarr_declare (struct face_cachel);
} face_cachel_dynarr;

typedef unsigned int glyph_index;

typedef struct
{
  Dynarr_declare (struct glyph_cachel);
} glyph_cachel_dynarr;

struct buffer;                  /* "buffer.h" */
struct console;			/* "console.h" */
struct device;			/* "device.h" */
struct extent_fragment;
struct extent;
typedef struct extent *EXTENT;
struct frame;			/* "frame.h" */
struct window;                  /* "window.h" */
struct Lisp_Event;              /* "events.h" */
struct Lisp_Face;
struct Lisp_Process;            /* "process.c" */
struct stat;                    /* <sys/stat.h> */
struct Lisp_Color_Instance;
struct Lisp_Font_Instance;
struct Lisp_Image_Instance;
struct display_line;
struct redisplay_info;
struct window_mirror;
struct scrollbar_instance;
struct font_metric_info;
struct face_cachel;
struct console_type_entry;

typedef struct
{
  Dynarr_declare (Bufbyte);
} Bufbyte_dynarr;

typedef struct
{
  Dynarr_declare (Extbyte);
} Extbyte_dynarr;

typedef struct
{
  Dynarr_declare (Emchar);
} Emchar_dynarr;

typedef unsigned char unsigned_char;
typedef struct
{
  Dynarr_declare (unsigned char);
} unsigned_char_dynarr;

typedef struct
{
  Dynarr_declare (int);
} int_dynarr;

typedef struct
{
  Dynarr_declare (Bufpos);
} Bufpos_dynarr;

typedef struct
{
  Dynarr_declare (Bytind);
} Bytind_dynarr;

typedef struct
{
  Dynarr_declare (Charcount);
} Charcount_dynarr;

typedef struct
{
  Dynarr_declare (Bytecount);
} Bytecount_dynarr;

typedef struct
{
  Dynarr_declare (struct console_type_entry);
} console_type_entry_dynarr;

/* Need to declare this here. */
enum external_data_format
{
  /* Binary format.  This is the simplest format and is what we
     use in the absence of a more appropriate format.  This converts
     according to the `binary' coding system:

     a) On input, bytes 0 - 255 are converted into characters 0 - 255.
     b) On output, characters 0 - 255 are converted into bytes 0 - 255
        and other characters are converted into `X'.
   */
  FORMAT_BINARY,

  /* Format used for filenames.  In the original Mule, this is
     user-definable with the `pathname-coding-system' variable.
     For the moment, we just use the `binary' coding system. */
  FORMAT_FILENAME,

  /* Format used for output to the terminal.  This should be controlled
     by the `terminal-coding-system' variable.  Under kterm, this will
     be some ISO2022 system.  On some DOS machines, this is Shift-JIS. */
  FORMAT_TERMINAL,

  /* Format used for input from the terminal.  This should be controlled
     by the `keyboard-coding-system' variable. */
  FORMAT_KEYBOARD,

  /* Format used for the external Unix environment -- argv[], stuff
     from getenv(), stuff from the /etc/passwd file, etc.

     Perhaps should be the same as FORMAT_FILENAME. */
  FORMAT_OS,

  /* Compound-text format.  This is the standard X format used for
     data stored in properties, selections, and the like.  This is
     an 8-bit no-lock-shift ISO2022 coding system. */
  FORMAT_CTEXT
};

#define FORMAT_NATIVE FORMAT_FILENAME

enum run_hooks_condition
{
  RUN_HOOKS_TO_COMPLETION,
  RUN_HOOKS_UNTIL_SUCCESS,
  RUN_HOOKS_UNTIL_FAILURE
};

#ifdef HAVE_TOOLBARS
enum toolbar_pos
{
  TOP_TOOLBAR,
  BOTTOM_TOOLBAR,
  LEFT_TOOLBAR,
  RIGHT_TOOLBAR
};
#endif

#ifndef ERROR_CHECK_TYPECHECK

typedef enum error_behavior
{
  ERROR_ME,
  ERROR_ME_NOT,
  ERROR_ME_WARN
} Error_behavior;

#define ERRB_EQ(a, b) ((a) == (b))

#else

/* By defining it like this, we provide strict type-checking
   for code that lazily uses ints. */

typedef struct _error_behavior_struct_
{
  int really_unlikely_name_to_have_accidentally_in_a_non_errb_structure;
} Error_behavior;

extern Error_behavior ERROR_ME;
extern Error_behavior ERROR_ME_NOT;
extern Error_behavior ERROR_ME_WARN;

#define ERRB_EQ(a, b)							   \
 ((a).really_unlikely_name_to_have_accidentally_in_a_non_errb_structure == \
  (b).really_unlikely_name_to_have_accidentally_in_a_non_errb_structure)

#endif

enum munge_me_out_the_door
{
  MUNGE_ME_FUNCTION_KEY,
  MUNGE_ME_KEY_TRANSLATION
};


/************************************************************************/
/*                   Definition of Lisp_Object data type                */
/************************************************************************/

#ifdef USE_MINIMAL_TAGBITS
# define LRECORD_CONS
# define LRECORD_VECTOR
# define LRECORD_SYMBOL
# define LRECORD_STRING
#endif

/* Define the fundamental Lisp data structures */

/* This is the set of Lisp data types */

#ifndef USE_MINIMAL_TAGBITS

enum Lisp_Type
{
  /* Integer.  XINT(obj) is the integer value. */
  Lisp_Type_Int,

  /* XRECORD_LHEADER (object) points to a struct lrecord_header
     lheader->implementation determines the type (and GC behaviour)
     of the object. */
  Lisp_Type_Record,

#ifndef LRECORD_CONS
  /* Cons.  XCONS (object) points to a struct Lisp_Cons. */
  Lisp_Type_Cons,
#endif

#ifndef LRECORD_STRING
  /* String.  XSTRING (object) points to a struct Lisp_String.
     The length of the string, and its contents, are stored therein. */
  Lisp_Type_String,
#endif

#ifndef LRECORD_VECTOR
  /* Vector of Lisp objects.  XVECTOR(object) points to a struct Lisp_Vector.
     The length of the vector, and its contents, are stored therein. */
  Lisp_Type_Vector,
#endif /* !LRECORD_VECTOR */

#ifndef LRECORD_SYMBOL
  /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol. */
  Lisp_Type_Symbol,
#endif /* !LRECORD_SYMBOL */

  Lisp_Type_Char
};

# define POINTER_TYPE_P(type) \
  ((type) != Lisp_Type_Int && (type) != Lisp_Type_Char)

#else

enum Lisp_Type
{
  Lisp_Type_Record,
  Lisp_Type_Int_Even,
  Lisp_Type_Char,
  Lisp_Type_Int_Odd
};

#define POINTER_TYPE_P(type) ((type) == Lisp_Type_Record)

#endif

/* This should be the underlying type into which a Lisp_Object must fit.
   In a strict ANSI world, this must be `int', since ANSI says you can't
   use bitfields on any type other than `int'.  However, on a machine
   where `int' and `long' are not the same size, this should be the
   longer of the two.  (This also must be something into which a pointer
   to an arbitrary object will fit, modulo any DATA_SEG_BITS cruft.)
 */
#if (LONGBITS > INTBITS)
# define EMACS_INT long
# define EMACS_UINT unsigned long
#else
# define EMACS_INT int
# define EMACS_UINT unsigned int
#endif

/* Overridden by m/next.h */
#ifndef ASSERT_VALID_POINTER
# define ASSERT_VALID_POINTER(pnt) (assert ((((EMACS_UINT) pnt) & 3) == 0))
#endif

/* These values are overridden by the m- file on some machines.  */
#ifndef GCTYPEBITS
# ifdef USE_MINIMAL_TAGBITS
#  define GCTYPEBITS 2L
# else
#  define GCTYPEBITS 3L
# endif
#endif

/* Valid values for GCMARKBITS are 0 and 1. */
#ifdef USE_MINIMAL_TAGBITS
# define GCMARKBITS 0L
#else
# define GCMARKBITS 1L
#endif

#ifndef VALBITS
# define VALBITS ((LONGBITS)-(GCTYPEBITS)-(GCMARKBITS))
#endif

#ifdef NO_UNION_TYPE
# include "lisp-disunion.h"
#else /* !NO_UNION_TYPE */
# include "lisp-union.h"
#endif /* !NO_UNION_TYPE */

/* WARNING WARNING WARNING.  You must ensure on your own that proper
   GC protection is provided for the elements in this array. */
typedef struct
{
  Dynarr_declare (Lisp_Object);
} Lisp_Object_dynarr;

/* Close your eyes now lest you vomit or spontaneously combust ... */

#define HACKEQ_UNSAFE(obj1, obj2)				\
  (EQ (obj1, obj2) || (!POINTER_TYPE_P (XGCTYPE (obj1))		\
		       && !POINTER_TYPE_P (XGCTYPE (obj2))	\
		       && XCHAR_OR_INT (obj1) == XCHAR_OR_INT (obj2)))

#ifdef DEBUG_XEMACS
extern int debug_issue_ebola_notices;
int eq_with_ebola_notice (Lisp_Object, Lisp_Object);
#define EQ_WITH_EBOLA_NOTICE(obj1, obj2)				\
  (debug_issue_ebola_notices ? eq_with_ebola_notice (obj1, obj2)	\
   : EQ (obj1, obj2))
#else
#define EQ_WITH_EBOLA_NOTICE(obj1, obj2) EQ (obj1, obj2)
#endif

/* OK, you can open them again */

/************************************************************************/
/*                   Definitions of basic Lisp objects                  */
/************************************************************************/

#include "lrecord.h"

/********** unbound ***********/

/* Qunbound is a special Lisp_Object (actually of type
   symbol-value-forward), that can never be visible to
   the Lisp caller and thus can be used in the C code
   to mean "no such value". */

#define UNBOUNDP(val) EQ (val, Qunbound)
#define GC_UNBOUNDP(val) GC_EQ (val, Qunbound)

/*********** cons ***********/

/* In a cons, the markbit of the car is the gc mark bit */

struct Lisp_Cons
{
#ifdef LRECORD_CONS
  struct lrecord_header lheader;
#endif
  Lisp_Object car, cdr;
};

#if 0 /* FSFmacs */
/* Like a cons, but records info on where the text lives that it was read from */
/* This is not really in use now */

struct Lisp_Buffer_Cons
{
  Lisp_Object car, cdr;
  struct buffer *buffer;
  int bufpos;
};
#endif

#ifdef LRECORD_CONS

DECLARE_LRECORD (cons, struct Lisp_Cons);
#define XCONS(x) XRECORD (x, cons, struct Lisp_Cons)
#define XSETCONS(x, p) XSETRECORD (x, p, cons)
#define CONSP(x) RECORDP (x, cons)
#define GC_CONSP(x) GC_RECORDP (x, cons)
#define CHECK_CONS(x) CHECK_RECORD (x, cons)
#define CONCHECK_CONS(x) CONCHECK_RECORD (x, cons)

#define CONS_MARKED_P(c) MARKED_RECORD_HEADER_P(&((c)->lheader))
#define MARK_CONS(c) MARK_RECORD_HEADER (&((c)->lheader))

#else /* ! LRECORD_CONS */

DECLARE_NONRECORD (cons, Lisp_Type_Cons, struct Lisp_Cons);
#define XCONS(a) XNONRECORD (a, cons, Lisp_Type_Cons, struct Lisp_Cons)
#define XSETCONS(c, p) XSETOBJ (c, Lisp_Type_Cons, p)
#define CONSP(x) (XTYPE (x) == Lisp_Type_Cons)
#define GC_CONSP(x) (XGCTYPE (x) == Lisp_Type_Cons)
#define CHECK_CONS(x) CHECK_NONRECORD (x, Lisp_Type_Cons, Qconsp)
#define CONCHECK_CONS(x) CONCHECK_NONRECORD (x, Lisp_Type_Cons, Qconsp)

/* Define these because they're used in a few places, inside and
   out of alloc.c */
#define CONS_MARKED_P(c) XMARKBIT (c->car)
#define MARK_CONS(c) XMARK (c->car)

#endif /* ! LRECORD_CONS */

#define NILP(x)  EQ (x, Qnil)
#define GC_NILP(x)  GC_EQ (x, Qnil)
#define CHECK_LIST(x) \
  do { if ((!CONSP (x)) && !NILP (x)) dead_wrong_type_argument (Qlistp, x); } while (0)
#define CONCHECK_LIST(x) \
  do { if ((!CONSP (x)) && !NILP (x)) x = wrong_type_argument (Qlistp, x); } while (0)
#define XCAR(a) (XCONS (a)->car)
#define XCDR(a) (XCONS (a)->cdr)

/* For a list that's known to be in valid list format --
   will abort() if the list is not in valid format */
#define LIST_LOOP(consvar, list) \
  for (consvar = list; !NILP (consvar); consvar = XCDR (consvar))

/* For a list that's known to be in valid list format, where we may
   be deleting the current element out of the list --
   will abort() if the list is not in valid format */
#define LIST_LOOP_DELETING(consvar, nextconsvar, list)			\
  for (consvar = list;							\
       !NILP (consvar) ? (nextconsvar = XCDR (consvar), 1) : 0;		\
       consvar = nextconsvar)

/* For a list that may not be in valid list format --
   will signal an error if the list is not in valid format */
#define EXTERNAL_LIST_LOOP(consvar, listp)				\
  for (consvar = listp; !NILP (consvar); consvar = XCDR (consvar))	\
     if (!CONSP (consvar))						\
       signal_simple_error ("Invalid list format", listp);		\
     else

/* For a property list (alternating keywords/values) that may not be
   in valid list format -- will signal an error if the list is not in
   valid format.  CONSVAR is used to keep track of the iterations
   without modifying LISTP.

   We have to be tricky to still keep the same C format.*/
#define EXTERNAL_PROPERTY_LIST_LOOP(consvar, keyword, value, listp)	\
  for (consvar = listp;							\
       (CONSP (consvar) && CONSP (XCDR (consvar)) ?			\
	(keyword = XCAR (consvar), value = XCAR (XCDR (consvar))) :	\
	(keyword = Qunbound, value = Qunbound)),			\
       !NILP (consvar);							\
       consvar = XCDR (XCDR (consvar)))					\
    if (UNBOUNDP (keyword))						\
      signal_simple_error ("Invalid property list format", listp);	\
    else

/*********** string ***********/

/* In a string or vector, the sign bit of the `size' is the gc mark bit */

/* (The size and data fields have underscores prepended to catch old
   code that attempts to reference the fields directly) */
struct Lisp_String
{
#ifdef LRECORD_STRING
  struct lrecord_header lheader;
#endif
  Bytecount _size;
  Bufbyte *_data;
  Lisp_Object plist;
};

#ifdef LRECORD_STRING

DECLARE_LRECORD (string, struct Lisp_String);
#define XSTRING(x) XRECORD (x, string, struct Lisp_String)
#define XSETSTRING(x, p) XSETRECORD (x, p, string)
#define STRINGP(x) RECORDP (x, string)
#define GC_STRINGP(x) GC_RECORDP (x, string)
#define CHECK_STRING(x) CHECK_RECORD (x, string)
#define CONCHECK_STRING(x) CONCHECK_RECORD (x, string)

#else /* ! LRECORD_STRING */

DECLARE_NONRECORD (string, Lisp_Type_String, struct Lisp_String);
#define XSTRING(x) XNONRECORD (x, string, Lisp_Type_String, struct Lisp_String)
#define XSETSTRING(x, p) XSETOBJ (x, Lisp_Type_String, p)
#define STRINGP(x) (XTYPE (x) == Lisp_Type_String)
#define GC_STRINGP(x) (XGCTYPE (x) == Lisp_Type_String)
#define CHECK_STRING(x) CHECK_NONRECORD (x, Lisp_Type_String, Qstringp)
#define CONCHECK_STRING(x) CONCHECK_NONRECORD (x, Lisp_Type_String, Qstringp)

#endif /* ! LRECORD_STRING */

#ifdef MULE

Charcount bytecount_to_charcount (CONST Bufbyte *ptr, Bytecount len);
Bytecount charcount_to_bytecount (CONST Bufbyte *ptr, Charcount len);

#else /* not MULE */

# define bytecount_to_charcount(ptr, len) (len)
# define charcount_to_bytecount(ptr, len) (len)

#endif /* not MULE */

#define string_length(s) ((s)->_size)
#define XSTRING_LENGTH(s) string_length (XSTRING (s))
#define string_data(s) ((s)->_data + 0)
#define XSTRING_DATA(s) string_data (XSTRING (s))
#define string_byte(s, i) ((s)->_data[i] + 0)
#define XSTRING_BYTE(s, i) string_byte (XSTRING (s), i)
#define string_byte_addr(s, i) (&((s)->_data[i]))
#define set_string_length(s, len) ((void) ((s)->_size = (len)))
#define set_string_data(s, ptr) ((void) ((s)->_data = (ptr)))
#define set_string_byte(s, i, c) ((void) ((s)->_data[i] = (c)))

void resize_string (struct Lisp_String *s, Bytecount pos, Bytecount delta);

#ifdef MULE

INLINE Charcount string_char_length (struct Lisp_String *s);
INLINE Charcount
string_char_length (struct Lisp_String *s)
{
  return bytecount_to_charcount (string_data (s), string_length (s));
}

# define string_char(s, i) charptr_emchar_n (string_data (s), i)
# define string_char_addr(s, i) charptr_n_addr (string_data (s), i)
void set_string_char (struct Lisp_String *s, Charcount i, Emchar c);

#else /* not MULE */

# define string_char_length(s) string_length (s)
# define string_char(s, i) ((Emchar) string_byte (s, i))
# define string_char_addr(s, i) string_byte_addr (s, i)
# define set_string_char(s, i, c) set_string_byte (s, i, c)

#endif /* not MULE */

/*********** vector ***********/

struct Lisp_Vector
{
#ifdef LRECORD_VECTOR
  struct lcrecord_header header;
#endif
  long size;
  /* next is now chained through v->contents[size], terminated by Qzero.
     This means that pure vectors don't need a "next" */
  /* struct Lisp_Vector *next; */
  Lisp_Object contents[1];
};

#ifdef LRECORD_VECTOR

DECLARE_LRECORD (vector, struct Lisp_Vector);
#define XVECTOR(x) XRECORD (x, vector, struct Lisp_Vector)
#define XSETVECTOR(x, p) XSETRECORD (x, p, vector)
#define VECTORP(x) RECORDP (x, vector)
#define GC_VECTORP(x) GC_RECORDP (x, vector)
#define CHECK_VECTOR(x) CHECK_RECORD (x, vector)
#define CONCHECK_VECTOR(x) CONCHECK_RECORD (x, vector)

#else

DECLARE_NONRECORD (vector, Lisp_Type_Vector, struct Lisp_Vector);
#define XVECTOR(x) XNONRECORD (x, vector, Lisp_Type_Vector, struct Lisp_Vector)
#define XSETVECTOR(x, p) XSETOBJ (x, Lisp_Type_Vector, p)
#define VECTORP(x) (XTYPE (x) == Lisp_Type_Vector)
#define GC_VECTORP(x) (XGCTYPE (x) == Lisp_Type_Vector)
#define CHECK_VECTOR(x) CHECK_NONRECORD (x, Lisp_Type_Vector, Qvectorp)
#define CONCHECK_VECTOR(x) CONCHECK_NONRECORD (x, Lisp_Type_Vector, Qvectorp)

#endif

#define vector_length(v) ((v)->size)
#define XVECTOR_LENGTH(s) vector_length (XVECTOR (s))
#define vector_data(v) ((v)->contents)
#define XVECTOR_DATA(s) vector_data (XVECTOR (s))
#ifndef LRECORD_VECTOR
# define vector_next(v) ((v)->contents[(v)->size])
#endif

/*********** bit vector ***********/

#if (LONGBITS < 16)
#error What the hell?!
#elif (LONGBITS < 32)
# define LONGBITS_LOG2 4
# define LONGBITS_POWER_OF_2 16
#elif (LONGBITS < 64)
# define LONGBITS_LOG2 5
# define LONGBITS_POWER_OF_2 32
#elif (LONGBITS < 128)
# define LONGBITS_LOG2 6
# define LONGBITS_POWER_OF_2 64
#else
#error You really have 128-bit integers?!
#endif

struct Lisp_Bit_Vector
{
  struct lrecord_header lheader;
  Lisp_Object next;
  long size;
  unsigned int bits[1];
};

DECLARE_LRECORD (bit_vector, struct Lisp_Bit_Vector);
#define XBIT_VECTOR(x) XRECORD (x, bit_vector, struct Lisp_Bit_Vector)
#define XSETBIT_VECTOR(x, p) XSETRECORD (x, p, bit_vector)
#define BIT_VECTORP(x) RECORDP (x, bit_vector)
#define GC_BIT_VECTORP(x) GC_RECORDP (x, bit_vector)
#define CHECK_BIT_VECTOR(x) CHECK_RECORD (x, bit_vector)
#define CONCHECK_BIT_VECTOR(x) CONCHECK_RECORD (x, bit_vector)

#define BITP(x) (INTP (x) && (XINT (x) == 0 || XINT (x) == 1))
#define GC_BITP(x) (GC_INTP (x) && (XINT (x) == 0 || XINT (x) == 1))

#define CHECK_BIT(x) \
  do { if (!BITP (x)) dead_wrong_type_argument (Qbitp, x); } while (0)
#define CONCHECK_BIT(x) \
  do { if (!BITP (x)) x = wrong_type_argument (Qbitp, x); } while (0)

#define bit_vector_length(v) ((v)->size)
#define bit_vector_next(v) ((v)->next)

INLINE int bit_vector_bit (struct Lisp_Bit_Vector *v, int i);
INLINE int
bit_vector_bit (struct Lisp_Bit_Vector *v, int i)
{
  unsigned int ui = (unsigned int) i;

  return (((v)->bits[ui >> LONGBITS_LOG2] >> (ui & (LONGBITS_POWER_OF_2 - 1)))
	  & 1);
}

INLINE void set_bit_vector_bit (struct Lisp_Bit_Vector *v, int i, int value);
INLINE void
set_bit_vector_bit (struct Lisp_Bit_Vector *v, int i, int value)
{
  unsigned int ui = (unsigned int) i;
  if (value)
    (v)->bits[ui >> LONGBITS_LOG2] |= (1 << (ui & (LONGBITS_POWER_OF_2 - 1)));
  else
    (v)->bits[ui >> LONGBITS_LOG2] &= ~(1 << (ui & (LONGBITS_POWER_OF_2 - 1)));
}

/* Number of longs required to hold LEN bits */
#define BIT_VECTOR_LONG_STORAGE(len) \
  ((len + LONGBITS_POWER_OF_2 - 1) >> LONGBITS_LOG2)


/*********** symbol ***********/

/* In a symbol, the markbit of the plist is used as the gc mark bit */

struct Lisp_Symbol
{
#ifdef LRECORD_SYMBOL
  struct lrecord_header lheader;
#endif
  /* next symbol in this obarray bucket */
  struct Lisp_Symbol *next;
  struct Lisp_String *name;
  Lisp_Object value;
  Lisp_Object function;
  Lisp_Object obarray;
  Lisp_Object plist;
};

#define SYMBOL_IS_KEYWORD(sym) (string_byte (XSYMBOL(sym)->name, 0) == ':')
#define KEYWORDP(obj) (SYMBOLP (obj) && SYMBOL_IS_KEYWORD (obj))

#ifdef LRECORD_SYMBOL

DECLARE_LRECORD (symbol, struct Lisp_Symbol);
#define XSYMBOL(x) XRECORD (x, symbol, struct Lisp_Symbol)
#define XSETSYMBOL(x, p) XSETRECORD (x, p, symbol)
#define SYMBOLP(x) RECORDP (x, symbol)
#define GC_SYMBOLP(x) GC_RECORDP (x, symbol)
#define CHECK_SYMBOL(x) CHECK_RECORD (x, symbol)
#define CONCHECK_SYMBOL(x) CONCHECK_RECORD (x, symbol)

#else

DECLARE_NONRECORD (symbol, Lisp_Type_Symbol, struct Lisp_Symbol);
#define XSYMBOL(x) XNONRECORD (x, symbol, Lisp_Type_Symbol, struct Lisp_Symbol)
#define XSETSYMBOL(s, p) XSETOBJ ((s), Lisp_Type_Symbol, (p))
#define SYMBOLP(x) (XTYPE (x) == Lisp_Type_Symbol)
#define GC_SYMBOLP(x) (XGCTYPE (x) == Lisp_Type_Symbol)
#define CHECK_SYMBOL(x) CHECK_NONRECORD (x, Lisp_Type_Symbol, Qsymbolp)
#define CONCHECK_SYMBOL(x) CONCHECK_NONRECORD (x, Lisp_Type_Symbol, Qsymbolp)

#endif

#define symbol_next(s) ((s)->next)
#define symbol_name(s) ((s)->name)
#define symbol_value(s) ((s)->value)
#define symbol_function(s) ((s)->function)
#define symbol_plist(s) ((s)->plist)

/*********** subr ***********/

typedef Lisp_Object (*lisp_fn_t) ();

struct Lisp_Subr
{
  struct lrecord_header lheader;
  short min_args, max_args;
  CONST char *prompt;
  CONST char *doc;
  CONST char *name;
  lisp_fn_t subr_fn;
};

DECLARE_LRECORD (subr, struct Lisp_Subr);
#define XSUBR(x) XRECORD (x, subr, struct Lisp_Subr)
#define XSETSUBR(x, p) XSETRECORD (x, p, subr)
#define SUBRP(x) RECORDP (x, subr)
#define GC_SUBRP(x) GC_RECORDP (x, subr)
#define CHECK_SUBR(x) CHECK_RECORD (x, subr)
#define CONCHECK_SUBR(x) CONCHECK_RECORD (x, subr)

#define subr_function(subr) (subr)->subr_fn
#define subr_name(subr) (subr)->name

/*********** marker ***********/

struct Lisp_Marker
{
  struct lrecord_header lheader;
  struct Lisp_Marker *next, *prev;
  struct buffer *buffer;
  Memind memind;
  char insertion_type;
};

DECLARE_LRECORD (marker, struct Lisp_Marker);
#define XMARKER(x) XRECORD (x, marker, struct Lisp_Marker)
#define XSETMARKER(x, p) XSETRECORD (x, p, marker)
#define MARKERP(x) RECORDP (x, marker)
#define GC_MARKERP(x) GC_RECORDP (x, marker)
#define CHECK_MARKER(x) CHECK_RECORD (x, marker)
#define CONCHECK_MARKER(x) CONCHECK_RECORD (x, marker)

/* The second check was looking for GCed markers still in use */
/* if (INTP (XMARKER (x)->lheader.next.v)) abort (); */

#define marker_next(m) ((m)->next)
#define marker_prev(m) ((m)->prev)

/*********** char ***********/

#define CHARP(x) (XTYPE (x) == Lisp_Type_Char)
#define GC_CHARP(x) (XGCTYPE (x) == Lisp_Type_Char)

#ifdef ERROR_CHECK_TYPECHECK

INLINE Emchar XCHAR (Lisp_Object obj);
INLINE Emchar
XCHAR (Lisp_Object obj)
{
  assert (CHARP (obj));
  return XCHARVAL (obj);
}

#else

#define XCHAR(x) XCHARVAL (x)

#endif

#define CHECK_CHAR(x) CHECK_NONRECORD (x, Lisp_Type_Char, Qcharacterp)
#define CONCHECK_CHAR(x) CONCHECK_NONRECORD (x, Lisp_Type_Char, Qcharacterp)


/*********** float ***********/

#ifdef LISP_FLOAT_TYPE

struct Lisp_Float
{
  struct lrecord_header lheader;
  union { double d; struct Lisp_Float *next; } data;
};

DECLARE_LRECORD (float, struct Lisp_Float);
#define XFLOAT(x) XRECORD (x, float, struct Lisp_Float)
#define XSETFLOAT(x, p) XSETRECORD (x, p, float)
#define FLOATP(x) RECORDP (x, float)
#define GC_FLOATP(x) GC_RECORDP (x, float)
#define CHECK_FLOAT(x) CHECK_RECORD (x, float)
#define CONCHECK_FLOAT(x) CONCHECK_RECORD (x, float)

#define float_next(f) ((f)->data.next)
#define float_data(f) ((f)->data.d)

#define XFLOATINT(n) extract_float (n)

#define CHECK_INT_OR_FLOAT(x)					\
  do { if ( !INTP (x) && !FLOATP (x))				\
       dead_wrong_type_argument (Qnumberp, (x)); } while (0)
#define CONCHECK_INT_OR_FLOAT(x)				\
  do { if ( !INTP (x) && !FLOATP (x))				\
       x = wrong_type_argument (Qnumberp, (x)); } while (0)

/* These are always continuable because they change their arguments
   even when no error is signalled. */

#define CHECK_INT_OR_FLOAT_COERCE_MARKER(x) do		\
{ if (INTP (x) || FLOATP (x))				\
    ;							\
  else if (MARKERP (x))					\
    x = make_int (marker_position (x));			\
  else							\
    x = wrong_type_argument (Qnumber_or_marker_p, x);	\
} while (0)

#define CHECK_INT_OR_FLOAT_COERCE_CHAR_OR_MARKER(x) do		\
{ if (INTP (x) || FLOATP (x))					\
    ;								\
  else if (CHARP (x))						\
    x = make_int (XCHAR (x));					\
  else if (MARKERP (x))						\
    x = make_int (marker_position (x));				\
  else								\
    x = wrong_type_argument (Qnumber_char_or_marker_p, x);	\
} while (0)

# define INT_OR_FLOATP(x) (INTP (x) || FLOATP (x))
# define GC_INT_OR_FLOATP(x) (GC_INTP (x) || GC_FLOATP (x))

#else /* not LISP_FLOAT_TYPE */

#define XFLOAT(x) --- error!  No float support. ---
#define XSETFLOAT(x, p) --- error!  No float support. ---
#define FLOATP(x) 0
#define GC_FLOATP(x) 0
#define CHECK_FLOAT(x) --- error!  No float support. ---
#define CONCHECK_FLOAT(x) --- error!  No float support. ---

#define XFLOATINT(n) XINT(n)
#define CHECK_INT_OR_FLOAT CHECK_INT
#define CONCHECK_INT_OR_FLOAT CONCHECK_INT
#define CHECK_INT_OR_FLOAT_COERCE_MARKER CHECK_INT_COERCE_MARKER
#define CHECK_INT_OR_FLOAT_COERCE_CHAR_OR_MARKER \
     CHECK_INT_COERCE_CHAR_OR_MARKER
#define INT_OR_FLOATP(x) (INTP (x))
# define GC_INT_OR_FLOATP(x) (GC_INTP (x))

#endif /* not LISP_FLOAT_TYPE */

#ifdef USE_MINIMAL_TAGBITS
# define INTP(x) \
  (XTYPE (x) == Lisp_Type_Int_Even || XTYPE(x) == Lisp_Type_Int_Odd)
# define GC_INTP(x) \
  (XGCTYPE (x) == Lisp_Type_Int_Even || XGCTYPE(x) == Lisp_Type_Int_Odd)
#else
# define INTP(x) (XTYPE (x) == Lisp_Type_Int)
# define GC_INTP(x) (XGCTYPE (x) == Lisp_Type_Int)
#endif

#define ZEROP(x) EQ (x, Qzero)
#define GC_ZEROP(x) GC_EQ (x, Qzero)

#ifdef ERROR_CHECK_TYPECHECK

INLINE EMACS_INT XINT (Lisp_Object obj);
INLINE EMACS_INT
XINT (Lisp_Object obj)
{
  assert (INTP (obj));
  return XREALINT (obj);
}

#else

#define XINT(obj) XREALINT (obj)

#endif

#ifdef ERROR_CHECK_TYPECHECK

INLINE EMACS_INT XCHAR_OR_INT (Lisp_Object obj);
INLINE EMACS_INT
XCHAR_OR_INT (Lisp_Object obj)
{
  assert (INTP (obj) || CHARP (obj));
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

#else

#define XCHAR_OR_INT(obj) (CHARP ((obj)) ? XCHAR ((obj)) : XINT ((obj)))

#endif

#ifdef USE_MINIMAL_TAGBITS
/*
 * can't use CHECK_NONRECORD and CONCHECK_NONRECORD here because in
 * the USE_MINIMAL_TAGBITS implementation Lisp integers have two types.
 */
# define CHECK_INT(x)				do {	\
 if (! INTP (x))					\
   dead_wrong_type_argument (Qintegerp, x);		\
 } while (0)
# define CONCHECK_INT(x)			do {	\
 if (! INTP (x))					\
   x = wrong_type_argument (Qintegerp, x);		\
 } while (0)
#else
# define CHECK_INT(x) CHECK_NONRECORD (x, Lisp_Type_Int, Qintegerp)
# define CONCHECK_INT(x) CONCHECK_NONRECORD (x, Lisp_Type_Int, Qintegerp)
#endif

#define NATNUMP(x) (INTP (x) && XINT (x) >= 0)
#define GC_NATNUMP(x) (GC_INTP (x) && XINT (x) >= 0)

#define CHECK_NATNUM(x) \
  do { if (!NATNUMP (x)) dead_wrong_type_argument (Qnatnump, x); } while (0)
#define CONCHECK_NATNUM(x) \
  do { if (!NATNUMP (x)) x = wrong_type_argument (Qnatnump, x); } while (0)

/* next three always continuable because they coerce their arguments. */
#define CHECK_INT_COERCE_CHAR(x) do			\
{ if (INTP (x))						\
    ;							\
  else if (CHARP (x))					\
    x = make_int (XCHAR (x));				\
  else							\
    x = wrong_type_argument (Qinteger_or_char_p, x);	\
} while (0)

#define CHECK_INT_COERCE_MARKER(x) do			\
{ if (INTP (x))						\
    ;							\
  else if (MARKERP (x))					\
    x = make_int (marker_position (x));			\
  else							\
    x = wrong_type_argument (Qinteger_or_marker_p, x);	\
} while (0)

#define CHECK_INT_COERCE_CHAR_OR_MARKER(x) do			\
{ if (INTP (x))							\
    ;								\
  else if (CHARP (x))						\
    x = make_int (XCHAR (x));					\
  else if (MARKERP (x))						\
    x = make_int (marker_position (x));				\
  else								\
    x = wrong_type_argument (Qinteger_char_or_marker_p, x);	\
} while (0)

/*********** pure space ***********/

#define CHECK_IMPURE(obj) \
  do { if (purified (obj)) pure_write_error (); } while (0)

/*********** structures ***********/

typedef struct structure_keyword_entry structure_keyword_entry;
struct structure_keyword_entry
{
  Lisp_Object keyword;
  int (*validate) (Lisp_Object keyword, Lisp_Object value,
		   Error_behavior errb);
};

typedef struct
{
  Dynarr_declare (structure_keyword_entry);
} structure_keyword_entry_dynarr;

typedef struct structure_type structure_type;
struct structure_type
{
  Lisp_Object type;
  structure_keyword_entry_dynarr *keywords;
  int (*validate) (Lisp_Object data, Error_behavior errb);
  Lisp_Object (*instantiate) (Lisp_Object data);
};

typedef struct
{
  Dynarr_declare (structure_type);
} structure_type_dynarr;

struct structure_type *define_structure_type (Lisp_Object type,
					      int (*validate)
					      (Lisp_Object data,
					       Error_behavior errb),
					      Lisp_Object (*instantiate)
					      (Lisp_Object data));
void define_structure_type_keyword (struct structure_type *st,
				    Lisp_Object keyword,
				    int (*validate) (Lisp_Object keyword,
						     Lisp_Object value,
						     Error_behavior errb));

/*********** weak lists ***********/

enum weak_list_type
{
  /* element disappears if it's unmarked. */
  WEAK_LIST_SIMPLE,
  /* element disappears if it's a cons and either its car or
     cdr is unmarked. */
  WEAK_LIST_ASSOC,
  /* element disappears if it's a cons and its car is unmarked. */
  WEAK_LIST_KEY_ASSOC,
  /* element disappears if it's a cons and its cdr is unmarked. */
  WEAK_LIST_VALUE_ASSOC
};

struct weak_list
{
  struct lcrecord_header header;
  Lisp_Object list; /* don't mark through this! */
  enum weak_list_type type;
  Lisp_Object next_weak; /* don't mark through this! */
};

DECLARE_LRECORD (weak_list, struct weak_list);
#define XWEAK_LIST(x) XRECORD (x, weak_list, struct weak_list)
#define XSETWEAK_LIST(x, p) XSETRECORD (x, p, weak_list)
#define WEAK_LISTP(x) RECORDP (x, weak_list)
#define GC_WEAK_LISTP(x) GC_RECORDP (x, weak_list)
#define CHECK_WEAK_LIST(x) CHECK_RECORD (x, weak_list)
#define CONCHECK_WEAK_LIST(x) CONCHECK_RECORD (x, weak_list)

#define weak_list_list(w) ((w)->list)
#define XWEAK_LIST_LIST(w) (XWEAK_LIST (w)->list)

Lisp_Object make_weak_list (enum weak_list_type type);
/* The following two are only called by the garbage collector */
int finish_marking_weak_lists (int (*obj_marked_p) (Lisp_Object),
			       void (*markobj) (Lisp_Object));
void prune_weak_lists (int (*obj_marked_p) (Lisp_Object));

/*********** lcrecord lists ***********/

struct lcrecord_list
{
  struct lcrecord_header header;
  Lisp_Object free;
  int size;
  CONST struct lrecord_implementation *implementation;
};

DECLARE_LRECORD (lcrecord_list, struct lcrecord_list);
#define XLCRECORD_LIST(x) XRECORD (x, lcrecord_list, struct lcrecord_list)
#define XSETLCRECORD_LIST(x, p) XSETRECORD (x, p, lcrecord_list)
#define LCRECORD_LISTP(x) RECORDP (x, lcrecord_list)
#define GC_LCRECORD_LISTP(x) GC_RECORDP (x, lcrecord_list)
/* #define CHECK_LCRECORD_LIST(x) CHECK_RECORD (x, lcrecord_list)
   Lcrecord lists should never escape to the Lisp level, so
   functions should not be doing this. */

Lisp_Object make_lcrecord_list (int size,
				CONST struct lrecord_implementation
				*implementation);
Lisp_Object allocate_managed_lcrecord (Lisp_Object lcrecord_list);
void free_managed_lcrecord (Lisp_Object lcrecord_list, Lisp_Object lcrecord);


/************************************************************************/
/*         Definitions of primitive Lisp functions and variables        */
/************************************************************************/


/* DEFUN - Define a built-in Lisp-visible C function or `subr'.
 `lname' should be the name to give the function in Lisp,
    as a null-terminated C string.
 `Fname' should be the C equivalent of `lname', using only characters
    valid in a C identifier, with an "F" prepended.
    The name of the C constant structure that records information
    on this function for internal use is "S" concatenated with Fname.
 `minargs' should be a number, the minimum number of arguments allowed.
 `maxargs' should be a number, the maximum number of arguments allowed,
    or else MANY or UNEVALLED.
    MANY means pass a vector of evaluated arguments,
	 in the form of an integer number-of-arguments
	 followed by the address of a vector of Lisp_Objects
	 which contains the argument values.
    UNEVALLED means pass the list of unevaluated arguments.
 `prompt' says how to read arguments for an interactive call.
    See the doc string for `interactive'.
    A null string means call interactively with no arguments.
 `arglist' are the comma-separated arguments (always Lisp_Objects) for
    the function.
  The docstring for the function is placed as a "C" comment between
    the prompt and the `args' argument.  make-docfile reads the
    comment and creates the DOC file form it.
*/

#define SUBR_MAX_ARGS 12
#define MANY -2
#define UNEVALLED -1

/* Can't be const, because then subr->doc is read-only and
   Snarf_documentation chokes */

#ifdef USE_INDEXED_LRECORD_IMPLEMENTATION
# define subr_lheader_initializer { 0, 0, 0 }
#else
# define subr_lheader_initializer { lrecord_subr }
#endif

#define DEFUN(lname, Fname, minargs, maxargs, prompt, arglist)		\
  Lisp_Object Fname (DEFUN_##maxargs arglist) ; /* See below */	\
  static struct Lisp_Subr S##Fname = { subr_lheader_initializer,	\
	minargs, maxargs, prompt, 0, lname, (lisp_fn_t) Fname };	\
  Lisp_Object Fname (DEFUN_##maxargs arglist)

/* Heavy ANSI C preprocessor hackery to get DEFUN to declare a
   prototype that matches maxargs, and add the obligatory
   `Lisp_Object' type declaration to the formal C arguments.  */

#define DEFUN_MANY(named_int, named_Lisp_Object) named_int, named_Lisp_Object
#define DEFUN_UNEVALLED(args) Lisp_Object args
#define DEFUN_0() void
#define DEFUN_1(a)									Lisp_Object a
#define DEFUN_2(a,b)				DEFUN_1(a),				Lisp_Object b
#define DEFUN_3(a,b,c)				DEFUN_2(a,b),				Lisp_Object c
#define DEFUN_4(a,b,c,d)			DEFUN_3(a,b,c),				Lisp_Object d
#define DEFUN_5(a,b,c,d,e)			DEFUN_4(a,b,c,d),			Lisp_Object e
#define DEFUN_6(a,b,c,d,e,f)			DEFUN_5(a,b,c,d,e),			Lisp_Object f
#define DEFUN_7(a,b,c,d,e,f,g)			DEFUN_6(a,b,c,d,e,f),			Lisp_Object g
#define DEFUN_8(a,b,c,d,e,f,g,h)		DEFUN_7(a,b,c,d,e,f,g),			Lisp_Object h
#define DEFUN_9(a,b,c,d,e,f,g,h,i)		DEFUN_8(a,b,c,d,e,f,g,h),		Lisp_Object i
#define DEFUN_10(a,b,c,d,e,f,g,h,i,j)		DEFUN_9(a,b,c,d,e,f,g,h,i),		Lisp_Object j
#define DEFUN_11(a,b,c,d,e,f,g,h,i,j,k)		DEFUN_10(a,b,c,d,e,f,g,h,i,j),		Lisp_Object k
#define DEFUN_12(a,b,c,d,e,f,g,h,i,j,k,l)	DEFUN_11(a,b,c,d,e,f,g,h,i,j,k),	Lisp_Object l

/* WARNING: If you add defines here for higher values of maxargs,
   make sure to also fix the clauses in primitive_funcall(),
   and change the define of SUBR_MAX_ARGS above.  */

#include "symeval.h"

/* Depth of special binding/unwind-protect stack.  Use as arg to `unbind_to' */
int specpdl_depth (void);


/************************************************************************/
/*                         Checking for QUIT                            */
/************************************************************************/

/* Asynchronous events set something_happened, and then are processed
   within the QUIT macro.  At this point, we are guaranteed to not be in
   any sensitive code. */

extern volatile int something_happened;
int check_what_happened (void);

extern volatile int quit_check_signal_happened;
extern volatile int quit_check_signal_tick_count;
int check_quit (void);

void signal_quit (void);

/* Nonzero if ought to quit now.  */
#define QUITP								\
  ((quit_check_signal_happened ? check_quit () : 0),			\
   (!NILP (Vquit_flag) && (NILP (Vinhibit_quit)				\
			   || EQ (Vquit_flag, Qcritical))))

/* QUIT used to call QUITP, but there are some places where QUITP
   is called directly, and check_what_happened() should only be called
   when Emacs is actually ready to quit because it could do things
   like switch threads. */
#define INTERNAL_QUITP							\
  ((something_happened ? check_what_happened () : 0),			\
   (!NILP (Vquit_flag) &&						\
    (NILP (Vinhibit_quit) || EQ (Vquit_flag, Qcritical))))

#define INTERNAL_REALLY_QUITP						\
  (check_what_happened (),						\
   (!NILP (Vquit_flag) &&						\
    (NILP (Vinhibit_quit) || EQ (Vquit_flag, Qcritical))))

/* Check quit-flag and quit if it is non-nil.  Also do any other things
   that might have gotten queued until it was safe. */
#define QUIT do { if (INTERNAL_QUITP) signal_quit (); } while (0)

#define REALLY_QUIT do { if (INTERNAL_REALLY_QUITP) signal_quit (); } while (0)


/************************************************************************/
/*                               hashing                                */
/************************************************************************/

/* #### for a 64-bit machine, we should substitute a prime just over 2^32 */
#define GOOD_HASH 65599 /* prime number just over 2^16; Dragon book, p. 435 */
#define HASH2(a,b)               (GOOD_HASH * (a)                     + (b))
#define HASH3(a,b,c)             (GOOD_HASH * HASH2 (a,b)             + (c))
#define HASH4(a,b,c,d)           (GOOD_HASH * HASH3 (a,b,c)           + (d))
#define HASH5(a,b,c,d,e)         (GOOD_HASH * HASH4 (a,b,c,d)         + (e))
#define HASH6(a,b,c,d,e,f)       (GOOD_HASH * HASH5 (a,b,c,d,e)       + (f))
#define HASH7(a,b,c,d,e,f,g)     (GOOD_HASH * HASH6 (a,b,c,d,e,f)     + (g))
#define HASH8(a,b,c,d,e,f,g,h)   (GOOD_HASH * HASH7 (a,b,c,d,e,f,g)   + (h))
#define HASH9(a,b,c,d,e,f,g,h,i) (GOOD_HASH * HASH8 (a,b,c,d,e,f,g,h) + (i))

/* Enough already! */

#define LISP_HASH(obj) ((unsigned long) LISP_TO_VOID (obj))
unsigned long string_hash (CONST void *xv);
unsigned long memory_hash (CONST void *xv, int size);
unsigned long internal_hash (Lisp_Object obj, int depth);
unsigned long internal_array_hash (Lisp_Object *arr, int size, int depth);


/************************************************************************/
/*                       String translation                             */
/************************************************************************/

#ifdef I18N3
#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#else
char *dgettext       (CONST char *, CONST char *);
char *gettext        (CONST char *);
char *textdomain     (CONST char *);
char *bindtextdomain (CONST char *, CONST char *);
#endif /* HAVE_LIBINTL_H */

#define GETTEXT(x)  gettext(x)
#define LISP_GETTEXT(x)  Fgettext (x)
#else /* !I18N3 */
#define GETTEXT(x)  (x)
#define LISP_GETTEXT(x)  (x)
#endif /* !I18N3 */

/* DEFER_GETTEXT is used to identify strings which are translated when
   they are referenced instead of when they are defined.
   These include Qerror_messages and initialized arrays of strings.
*/
#define DEFER_GETTEXT(x) (x)


/************************************************************************/
/*                   Garbage collection / GC-protection                 */
/************************************************************************/

/* number of bytes of structure consed since last GC */

extern EMACS_INT consing_since_gc;

/* threshold for doing another gc */

extern EMACS_INT gc_cons_threshold;

/* Structure for recording stack slots that need marking */

/* This is a chain of structures, each of which points at a Lisp_Object
   variable whose value should be marked in garbage collection.
   Normally every link of the chain is an automatic variable of a function,
   and its `val' points to some argument or local variable of the function.
   On exit to the function, the chain is set back to the value it had on
   entry.  This way, no link remains in the chain when the stack frame
   containing the link disappears.

   Every function that can call Feval must protect in this fashion all
   Lisp_Object variables whose contents will be used again. */

extern struct gcpro *gcprolist;

struct gcpro
{
  struct gcpro *next;
  Lisp_Object *var;		/* Address of first protected variable */
  int nvars;			/* Number of consecutive protected variables */
};

/* Normally, you declare variables gcpro1, gcpro2, ... and use the
   GCPROn() macros.  However, if you need to have nested gcpro's,
   declare ngcpro1, ngcpro2, ... and use NGCPROn().  If you need
   to nest another level, use nngcpro1, nngcpro2, ... and use
   NNGCPROn().  If you need to nest yet another level, create
   the appropriate macros. */

#ifdef DEBUG_GCPRO

void debug_gcpro1 ();
void debug_gcpro2 ();
void debug_gcpro3 ();
void debug_gcpro4 ();
void debug_gcpro5 ();
void debug_ungcpro();

#define GCPRO1(v) \
 debug_gcpro1 (__FILE__, __LINE__,&gcpro1,&v)
#define GCPRO2(v1,v2) \
 debug_gcpro2 (__FILE__, __LINE__,&gcpro1,&gcpro2,&v1,&v2)
#define GCPRO3(v1,v2,v3) \
 debug_gcpro3 (__FILE__, __LINE__,&gcpro1,&gcpro2,&gcpro3,&v1,&v2,&v3)
#define GCPRO4(v1,v2,v3,v4) \
 debug_gcpro4 (__FILE__, __LINE__,&gcpro1,&gcpro2,&gcpro3,&gcpro4,\
	       &v1,&v2,&v3,&v4)
#define GCPRO5(v1,v2,v3,v4,v5) \
 debug_gcpro5 (__FILE__, __LINE__,&gcpro1,&gcpro2,&gcpro3,&gcpro4,&gcpro5,\
	       &v1,&v2,&v3,&v4,&v5)
#define UNGCPRO \
 debug_ungcpro(__FILE__, __LINE__,&gcpro1)

#define NGCPRO1(v) \
 debug_gcpro1 (__FILE__, __LINE__,&ngcpro1,&v)
#define NGCPRO2(v1,v2) \
 debug_gcpro2 (__FILE__, __LINE__,&ngcpro1,&ngcpro2,&v1,&v2)
#define NGCPRO3(v1,v2,v3) \
 debug_gcpro3 (__FILE__, __LINE__,&ngcpro1,&ngcpro2,&ngcpro3,&v1,&v2,&v3)
#define NGCPRO4(v1,v2,v3,v4) \
 debug_gcpro4 (__FILE__, __LINE__,&ngcpro1,&ngcpro2,&ngcpro3,&ngcpro4,\
	       &v1,&v2,&v3,&v4)
#define NGCPRO5(v1,v2,v3,v4,v5) \
 debug_gcpro5 (__FILE__, __LINE__,&ngcpro1,&ngcpro2,&ngcpro3,&ngcpro4,\
	       &ngcpro5,&v1,&v2,&v3,&v4,&v5)
#define NUNGCPRO \
 debug_ungcpro(__FILE__, __LINE__,&ngcpro1)

#define NNGCPRO1(v) \
 debug_gcpro1 (__FILE__, __LINE__,&nngcpro1,&v)
#define NNGCPRO2(v1,v2) \
 debug_gcpro2 (__FILE__, __LINE__,&nngcpro1,&nngcpro2,&v1,&v2)
#define NNGCPRO3(v1,v2,v3) \
 debug_gcpro3 (__FILE__, __LINE__,&nngcpro1,&nngcpro2,&nngcpro3,&v1,&v2,&v3)
#define NNGCPRO4(v1,v2,v3,v4) \
 debug_gcpro4 (__FILE__, __LINE__,&nngcpro1,&nngcpro2,&nngcpro3,&nngcpro4,\
	       &v1,&v2,&v3,&v4)
#define NNGCPRO5(v1,v2,v3,v4,v5) \
 debug_gcpro5 (__FILE__, __LINE__,&nngcpro1,&nngcpro2,&nngcpro3,&nngcpro4,\
	       &nngcpro5,&v1,&v2,&v3,&v4,&v5)
#define NUNNGCPRO \
 debug_ungcpro(__FILE__, __LINE__,&nngcpro1)

#else /* ! DEBUG_GCPRO */

#define GCPRO1(varname) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname; gcpro1.nvars = 1; \
  gcprolist = &gcpro1; }

#define GCPRO2(varname1, varname2) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1;   gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcprolist = &gcpro2; }

#define GCPRO3(varname1, varname2, varname3) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1;   gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2;   gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcprolist = &gcpro3; }

#define GCPRO4(varname1, varname2, varname3, varname4) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1;   gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2;   gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3;   gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcprolist = &gcpro4; }

#define GCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1;   gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2;   gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3;   gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4;   gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcprolist = &gcpro5; }

#define UNGCPRO (gcprolist = gcpro1.next)

#define NGCPRO1(varname) \
 {ngcpro1.next = gcprolist; ngcpro1.var = &varname; ngcpro1.nvars = 1; \
  gcprolist = &ngcpro1; }

#define NGCPRO2(varname1, varname2) \
 {ngcpro1.next = gcprolist; ngcpro1.var = &varname1; ngcpro1.nvars = 1; \
  ngcpro2.next = &ngcpro1;  ngcpro2.var = &varname2; ngcpro2.nvars = 1; \
  gcprolist = &ngcpro2; }

#define NGCPRO3(varname1, varname2, varname3) \
 {ngcpro1.next = gcprolist; ngcpro1.var = &varname1; ngcpro1.nvars = 1; \
  ngcpro2.next = &ngcpro1;  ngcpro2.var = &varname2; ngcpro2.nvars = 1; \
  ngcpro3.next = &ngcpro2;  ngcpro3.var = &varname3; ngcpro3.nvars = 1; \
  gcprolist = &ngcpro3; }

#define NGCPRO4(varname1, varname2, varname3, varname4) \
 {ngcpro1.next = gcprolist; ngcpro1.var = &varname1; ngcpro1.nvars = 1; \
  ngcpro2.next = &ngcpro1;  ngcpro2.var = &varname2; ngcpro2.nvars = 1; \
  ngcpro3.next = &ngcpro2;  ngcpro3.var = &varname3; ngcpro3.nvars = 1; \
  ngcpro4.next = &ngcpro3;  ngcpro4.var = &varname4; ngcpro4.nvars = 1; \
  gcprolist = &ngcpro4; }

#define NGCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {ngcpro1.next = gcprolist; ngcpro1.var = &varname1; ngcpro1.nvars = 1; \
  ngcpro2.next = &ngcpro1;  ngcpro2.var = &varname2; ngcpro2.nvars = 1; \
  ngcpro3.next = &ngcpro2;  ngcpro3.var = &varname3; ngcpro3.nvars = 1; \
  ngcpro4.next = &ngcpro3;  ngcpro4.var = &varname4; ngcpro4.nvars = 1; \
  ngcpro5.next = &ngcpro4;  ngcpro5.var = &varname5; ngcpro5.nvars = 1; \
  gcprolist = &ngcpro5; }

#define NUNGCPRO (gcprolist = ngcpro1.next)

#define NNGCPRO1(varname) \
 {nngcpro1.next = gcprolist; nngcpro1.var = &varname; nngcpro1.nvars = 1; \
  gcprolist = &nngcpro1; }

#define NNGCPRO2(varname1, varname2) \
 {nngcpro1.next = gcprolist; nngcpro1.var = &varname1; nngcpro1.nvars = 1; \
  nngcpro2.next = &nngcpro1; nngcpro2.var = &varname2; nngcpro2.nvars = 1; \
  gcprolist = &nngcpro2; }

#define NNGCPRO3(varname1, varname2, varname3) \
 {nngcpro1.next = gcprolist; nngcpro1.var = &varname1; nngcpro1.nvars = 1; \
  nngcpro2.next = &nngcpro1; nngcpro2.var = &varname2; nngcpro2.nvars = 1; \
  nngcpro3.next = &nngcpro2; nngcpro3.var = &varname3; nngcpro3.nvars = 1; \
  gcprolist = &nngcpro3; }

#define NNGCPRO4(varname1, varname2, varname3, varname4) \
 {nngcpro1.next = gcprolist; nngcpro1.var = &varname1; nngcpro1.nvars = 1; \
  nngcpro2.next = &nngcpro1; nngcpro2.var = &varname2; nngcpro2.nvars = 1; \
  nngcpro3.next = &nngcpro2; nngcpro3.var = &varname3; nngcpro3.nvars = 1; \
  nngcpro4.next = &nngcpro3; nngcpro4.var = &varname4; nngcpro4.nvars = 1; \
  gcprolist = &nngcpro4; }

#define NNGCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {nngcpro1.next = gcprolist; nngcpro1.var = &varname1; nngcpro1.nvars = 1; \
  nngcpro2.next = &nngcpro1; nngcpro2.var = &varname2; nngcpro2.nvars = 1; \
  nngcpro3.next = &nngcpro2; nngcpro3.var = &varname3; nngcpro3.nvars = 1; \
  nngcpro4.next = &nngcpro3; nngcpro4.var = &varname4; nngcpro4.nvars = 1; \
  nngcpro5.next = &nngcpro4; nngcpro5.var = &varname5; nngcpro5.nvars = 1; \
  gcprolist = &nngcpro5; }

#define NNUNGCPRO (gcprolist = nngcpro1.next)

#endif /* ! DEBUG_GCPRO */

/* Another try to fix SunPro C compiler warnings */
/* "end-of-loop code not reached" */
/* "statement not reached */
#ifdef __SUNPRO_C
#define RETURN__ if (1) return
#define RETURN_NOT_REACHED(value)
#else
#define RETURN__ return
#define RETURN_NOT_REACHED(value) return value;
#endif

/* Evaluate expr, UNGCPRO, and then return the value of expr.  */
#define RETURN_UNGCPRO(expr) do			\
{						\
  Lisp_Object ret_ungc_val = (expr);		\
  UNGCPRO;					\
  RETURN__ ret_ungc_val;			\
} while (0)

/* Evaluate expr, NUNGCPRO, UNGCPRO, and then return the value of expr.  */
#define RETURN_NUNGCPRO(expr) do		\
{						\
  Lisp_Object ret_ungc_val = (expr);		\
  NUNGCPRO;					\
  UNGCPRO;					\
  RETURN__ ret_ungc_val;			\
} while (0)

/* Evaluate expr, NNUNGCPRO, NUNGCPRO, UNGCPRO, and then return the
   value of expr.  */
#define RETURN_NNUNGCPRO(expr) do		\
{						\
  Lisp_Object ret_ungc_val = (expr);		\
  NNUNGCPRO;					\
  NUNGCPRO;					\
  UNGCPRO;					\
  RETURN__ ret_ungc_val;			\
} while (0)

/* Evaluate expr, return it if it's not Qunbound. */
#define RETURN_IF_NOT_UNBOUND(expr) do		\
{						\
  Lisp_Object ret_nunb_val = (expr);		\
  if (!UNBOUNDP (ret_nunb_val))			\
    RETURN__ ret_nunb_val;			\
} while (0)

/* Call staticpro (&var) to protect static variable `var'. */
void staticpro (Lisp_Object *);

/* Nonzero means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern int initialized;

#ifdef MEMORY_USAGE_STATS

/* This structure is used to keep statistics on the amount of memory
   in use.

   WAS_REQUESTED stores the actual amount of memory that was requested
   of the allocation function.  The *_OVERHEAD fields store the
   additional amount of memory that was grabbed by the functions to
   facilitate allocation, reallocation, etc.  MALLOC_OVERHEAD is for
   memory allocated with malloc(); DYNARR_OVERHEAD is for dynamic
   arrays; GAP_OVERHEAD is for gap arrays.  Note that for (e.g.)
   dynamic arrays, there is both MALLOC_OVERHEAD and DYNARR_OVERHEAD
   memory: The dynamic array allocates memory above and beyond what
   was asked of it, and when it in turns allocates memory using
   malloc(), malloc() allocates memory beyond what it was asked
   to allocate.

   Functions that accept a structure of this sort do not initialize
   the fields to 0, and add any existing values to whatever was there
   before; this way, you can get a cumulative effect. */

struct overhead_stats
{
  int was_requested;
  int malloc_overhead;
  int dynarr_overhead;
  int gap_overhead;
};

#endif /* MEMORY_USAGE_STATS */

/* Some systems (e.g., NT) use a different path separator than Unix,
   in addition to a device separator.  Default the path separator
   to '/', and don't test for a device separator in IS_ANY_SEP.  */

#ifdef WINDOWSNT
extern Lisp_Object Vdirectory_sep_char;
#endif

#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif
#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif
#ifndef IS_DEVICE_SEP
#ifndef DEVICE_SEP
#define IS_DEVICE_SEP(_c_) 0
#else
#define IS_DEVICE_SEP(_c_) ((_c_) == DEVICE_SEP)
#endif
#endif
#ifndef IS_ANY_SEP
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_))
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef int intptr_t;
typedef unsigned int uintptr_t;
#elif SIZEOF_VOID_P == SIZEOF_LONG
typedef long intptr_t;
typedef unsigned long uintptr_t;
#elif defined(SIZEOF_LONG_LONG) && SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef long long intptr_t;
typedef unsigned long long uintptr_t;
#else
/* Just pray. May break, may not. */
typedef char *intptr_t;
typedef char *uintptr_t;
#endif

#include "emacsfns.h"

#endif /* _XEMACS_LISP_H_ */
