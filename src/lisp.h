/* Fundamental definitions for XEmacs Lisp interpreter.
   Copyright (C) 1985-1987, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1996 Richard Mlynarik.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2004, 2005 Ben Wing.

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

/* Authorship:

   Based on code from pre-release FSF 19, c. 1991.
   Various changes by Jamie Zawinski 1991-1994:
     converting to ANSI C, splitting out function prototypes to a separate
     file (later moved back for unknown reasons by Steve Baur?), debug-gcpro
     stuff (now moribund).
   ANSI-fication of DEFUN macros by Felix Lee, c. 1992?
   NOT_REACHED, DOESNT_RETURN, PRINTF_ARGS by Richard Mlynarik, c. 1994.
   Many changes over the years corresponding to Lisp_Object definition
     changes, esp. by Richard Mlynarik (c. 1993) and Kyle Jones (c. 1998).
     See alloc.c for more details.
   Overhauled and reordered by Ben Wing, 1995-1996, and many things added:
     Dynarrs, REALLOC macros, asserts, typedefs, inline header code,
     first LIST_LOOP macros, CONCHECK_*, all error-checking code
     (e.g. error-checking versions of XFOO macros), structure read syntax,
     weak lists, lcrecord lists, reworking of quit handling, object hashing,
     nested GCPRO, character objects and Ebola checking, memory usage stats,
     others.
   LOADHIST changes from Steve Baur, c. 1997?
   Various macro-related changes by Martin Buchholz, 1998-1999:
     LIST_LOOP macros greatly expanded and tortoise-hared;
     RETURN_SANS_WARNINGS; reworked DEFUN macros; EXFUN macros (???).
   Various macro-related changes by Jerry James, 2003:
     MODULE_API introduced;
     Compiler-specific definitions modernized and moved to compiler.h.
*/

#ifndef INCLUDED_lisp_h_
#define INCLUDED_lisp_h_

/************************************************************************/
/*			  general definitions				*/
/************************************************************************/

/* Conventions in comments:

   "Mule-izing" is the process of going through a file and eliminating
   assumptions that the internal format (Ibyte * text) is the same as the
   external format used by library routines.  Mule-ization should also
   include replacing *ALL* raw references to `char' or `unsigned char' with
   one of the self-documenting types created below.  How exactly to do the
   conversion, and how to write correctly Mule-ized code, is described in
   the internals manual.  Files that say "This file is Mule-ized" have
   been reviewed at some point; that's not to say that incorrect code hasn't
   crept in, though.

   "Unicode-splitting" is the process of fixing a file so that it will
   handle external text in Unicode under Microsoft Windows, as appropriate.
   ("splitting" because it needs to handle either Unicode or variable-width
   multibyte depending on the OS -- NT or 9x).  See intl-win32.c.

   #### is a way of marking problems of any sort.

   !!#### marks places that are not properly Mule-ized.

   &&#### marks places that need to be fixed in order for the "8-bit mule"
   conversion to work correctly, i.e. in order to support multiple different
   buffer formats under Mule, including a fixed 8-bit format.

   ^^#### marks places that need to be fixed in order to eliminate the
   assumption that Ibyte * text is composed of 1-byte units (e.g. UTF-16
   is composed of 2-byte units and might be a possible format to consider
   for Ibyte * text).

   %%#### marks places that need work for KKCC (the new garbage collector).

   */

/* -------------------------- include files --------------------- */

/* We include the following generally useful header files so that you
   don't have to worry about prototypes when using the standard C
   library functions and macros.  These files shouldn't be excessively
   large so they shouldn't cause that much of a slowdown. */

#include <stdlib.h>
#include <string.h>		/* primarily for memcpy, etc. */
#include <stdio.h>		/* NULL, etc. */
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>		/* offsetof */
#include <sys/types.h>
#include <limits.h>

/* -------------------------- error-checking ------------------------ */

/* The large categories established by configure can be subdivided into
   smaller subcategories, for problems in specific modules.  You can't
   control this using configure, but you can manually stick in a define as
   necessary. */

#ifdef ERROR_CHECK_STRUCTURES
/* Check for problems with the catch list and specbind stack */
#define ERROR_CHECK_CATCH
/* Check for insufficient use of call_trapping_problems(), particularly
   due to glyph-related changes causing eval or QUIT within redisplay */
#define ERROR_CHECK_TRAPPING_PROBLEMS
#endif

#ifdef ERROR_CHECK_TYPES
#define type_checking_assert(assertion) assert (assertion)
#define type_checking_assert_at_line(assertion, file, line) \
  assert_at_line (assertion, file, line)
#define type_checking_assert_with_message(assertion, msg) \
  assert_with_message (assertion, msg)
#else
#define type_checking_assert(assertion)
#define type_checking_assert_at_line(assertion, file, line)
#define type_checking_assert_with_message(assertion, msg)
#endif
#ifdef ERROR_CHECK_GC
#define gc_checking_assert(assertion) assert (assertion)
#define gc_checking_assert_at_line(assertion, file, line) \
  assert_at_line (assertion, file, line)
#define gc_checking_assert_with_message(assertion, msg) \
  assert_with_message (assertion, msg)
#else
#define gc_checking_assert(assertion)
#define gc_checking_assert_at_line(assertion, file, line)
#define gc_checking_assert_with_message(assertion, msg)
#endif
#ifdef ERROR_CHECK_TEXT
#define text_checking_assert(assertion) assert (assertion)
#define text_checking_assert_at_line(assertion, file, line) \
  assert_at_line (assertion, file, line)
#define text_checking_assert_with_message(assertion, msg) \
  assert_with_message (assertion, msg)
#else
#define text_checking_assert(assertion)
#define text_checking_assert_at_line(assertion, file, line)
#define text_checking_assert_with_message(assertion, msg)
#endif
#ifdef ERROR_CHECK_TRAPPING_PROBLEMS
#define trapping_problems_checking_assert(assertion) assert (assertion)
#define trapping_problems_checking_assert_at_line(assertion, file, line) \
  assert_at_line (assertion, file, line)
#define trapping_problems_checking_assert_with_message(assertion, msg) \
  assert_with_message (assertion, msg)
#else
#define trapping_problems_checking_assert(assertion)
#define trapping_problems_checking_assert_at_line(assertion, file, line)
#define trapping_problems_checking_assert_with_message(assertion, msg)
#endif

/************************************************************************/
/**                     Definitions of basic types                     **/
/************************************************************************/

/* ------------- generic 8/16/32/64/128-bit integral types ------------ */

#if SIZEOF_SHORT == 2
#define INT_16_BIT short
#define UINT_16_BIT unsigned short
#elif SIZEOF_INT == 2
/* Bwa ha ha.  As if XEmacs could actually support such systems. */
#define INT_16_BIT int
#define UINT_16_BIT unsigned int
#else
#error Unable to find a 16-bit integral type
#endif

#if SIZEOF_INT == 4
#define INT_32_BIT int
#define UINT_32_BIT unsigned int
#define MAKE_32_BIT_UNSIGNED_CONSTANT(num) num##U
#elif SIZEOF_LONG == 4
/* Bwa ha ha again. */
#define INT_32_BIT long
#define UINT_32_BIT unsigned long
#define MAKE_32_BIT_UNSIGNED_CONSTANT(num) num##UL
#elif SIZEOF_SHORT == 4
/* And again. */
#define INT_32_BIT short
#define UINT_32_BIT unsigned short
#define MAKE_32_BIT_UNSIGNED_CONSTANT(num) num##U
#elif 1 /* Unable to find a 32-bit integral type! */
#error What kind of strange-ass system are you running on?
#endif

#if SIZEOF_LONG == 8
#define INT_64_BIT long
#define UINT_64_BIT unsigned long
#define MAKE_64_BIT_UNSIGNED_CONSTANT(num) num##UL
#elif SIZEOF_LONG_LONG == 8
#define INT_64_BIT long long
#define UINT_64_BIT unsigned long long
#define MAKE_64_BIT_UNSIGNED_CONSTANT(num) num##ULL
/* No error otherwise; just leave undefined */
#endif

#if SIZEOF_LONG_LONG == 16
#define INT_128_BIT long long
#define UINT_128_BIT unsigned long long
#define MAKE_128_BIT_UNSIGNED_CONSTANT(num) num##ULL
/* No error otherwise; just leave undefined */
#endif

/* #### Fill this in for other systems */
#if defined (INT_64_BIT) && !(defined (i386) || defined (__i386__))
#define EFFICIENT_INT_64_BIT INT_64_BIT
#define EFFICIENT_UINT_64_BIT UINT_64_BIT
#endif

#if defined (INT_128_BIT)
#define EFFICIENT_INT_128_BIT INT_128_BIT
#define EFFICIENT_UINT_128_BIT UINT_128_BIT
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#elif defined (HAVE_INTPTR_T_IN_SYS_TYPES_H)
/* included elsewhere */
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef int intptr_t;
typedef unsigned int uintptr_t;
#elif SIZEOF_VOID_P == SIZEOF_LONG
typedef long intptr_t;
typedef unsigned long uintptr_t;
#elif defined (SIZEOF_LONG_LONG) && SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef long long intptr_t;
typedef unsigned long long uintptr_t;
#else
/* Just pray. May break, may not. */
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif

#if SIZEOF_VOID_P == 8
#define DEADBEEF_CONSTANT 0xCAFEBABEDEADBEEF
#elif SIZEOF_VOID_P == 4
#define DEADBEEF_CONSTANT 0xDEADBEEF
#else
#error "What size are your pointers, really?"
#endif /* SIZEOF_VOID_P == 8 */

/* ---------------------- definition of EMACS_INT --------------------- */

/* EMACS_INT is the underlying integral type into which a Lisp_Object must fit.
   In particular, it must be large enough to contain a pointer.
   config.h can override this, e.g. to use `long long' for bigger lisp ints.

   #### In point of fact, it would NOT be a good idea for config.h to mess
   with EMACS_INT.  A lot of code makes the basic assumption that EMACS_INT
   is the size of a pointer. */

#ifndef SIZEOF_EMACS_INT
# define SIZEOF_EMACS_INT SIZEOF_VOID_P
#endif

#ifndef EMACS_INT
# if   SIZEOF_EMACS_INT == SIZEOF_LONG
#  define EMACS_INT long
# elif SIZEOF_EMACS_INT == SIZEOF_INT
#  define EMACS_INT int
# elif SIZEOF_EMACS_INT == SIZEOF_LONG_LONG
#  define EMACS_INT long long
# else
#  error Unable to determine suitable type for EMACS_INT
# endif
#endif

#ifndef EMACS_UINT
# define EMACS_UINT unsigned EMACS_INT
#endif

#define BITS_PER_EMACS_INT (SIZEOF_EMACS_INT * BITS_PER_CHAR)

/* -------------------------- basic byte typedefs --------------------- */

/* The definitions we put here and in the next section use typedefs to
   attribute specific meaning to types that by themselves are pretty
   general.

   REMEMBER!  These typedefs are purely for documentation purposes; from
   the C code's perspective, they are exactly equivalent to `char *',
   `unsigned char *', etc., so you can freely use them with library
   functions declared as such.

   (See also "Byte/Character Types" in text.c)
 
   The basic semantics for `char':

   a) [Ibyte] pointer to internally-formatted text
   b) [Extbyte] pointer to text in some external format, which can be
                defined as all formats other than the internal one
   c) [Ascbyte] pure ASCII text
   d) [Binbyte] binary data that is not meant to be interpreted as text
   e) [Rawbyte] general data in memory, where we don't care about whether
                it's text or binary
   f) [Boolbyte] a zero or a one
   g) [Bitbyte] a byte used for bit fields
   h) [Chbyte] null-semantics `char *'; used when casting an argument to
               an external API where the the other types may not be
               appropriate


   Prefixing codes:

   C = plain char, when the base type is unsigned
   U = unsigned
   S = signed

   Ideally, XEmacs code should NEVER directly use `char' or any type
   derived from it.  This is for Mule-cleanliness.  If you find yourself
   wanting or needing to use `char' and one of the above six semantics does
   not apply, add a new type of semantics; don't use `char' directly.

   See text.c under "Byte Types", and following sections.
*/

/* The data representing the text in a buffer is logically a set
   of Ibytes, declared as follows. */

typedef unsigned char Ibyte;

/* The following should be used when you are working with internal data
   but for whatever reason need to have it declared a "char *".  Examples
   are function arguments whose values are most commonly literal strings,
   or where you have to apply a stdlib string function to internal data.

   In general, you should avoid this where possible and use Ascbyte if the
   text is just ASCII (e.g. string literals) or otherwise Ibyte, for
   consistency.  For example, the new Mule workspace contains Ibyte
   versions of the stdlib string functions. */

typedef char CIbyte;

/* The data representing a string in "external" format (binary or any
   external encoding) is logically a set of Extbytes, declared as
   follows.  Extbyte is guaranteed to be just a char, so for example
   strlen (Extbyte *) is OK.  Extbyte is only a documentation device
   for referring to external text. */

typedef char Extbyte;
typedef unsigned char UExtbyte;

#define EXTTEXT_ZTERM_SIZE (sizeof (Extbyte))

/* A byte in a string in entirely US-ASCII format: (Nothing outside
 the range 00 - 7F) */

typedef char Ascbyte;
typedef unsigned char UAscbyte;

/* A generic memory pointer, no text or binary semantics assumed.
   In general, there should be no manipulation of the memory pointed to
   by these pointers other than just copying it around. */

typedef unsigned char Rawbyte;
typedef char CRawbyte;

/* A byte in a string in binary (not meant as text) format: */

typedef unsigned char Binbyte;
typedef char CBinbyte;
typedef signed char SBinbyte;

/* A byte used to represent a boolean value: 0 or 1.
   Normally use plain Boolint, and only use Boolbyte to save space. */

typedef char Boolbyte;

/* A byte composed of bitfields.  Hardly ever used. */

typedef unsigned char Bitbyte;

/* A no-semantics `char'.  Used (pretty-much) ONLY for casting arguments to
   functions accepting a `char *', `unsigned char *', etc. where the other
   types don't exactly apply and what you are logically concerned with is
   the type of the function's argument and not its semantics.

   DO NOT DO NOT DO NOT DO NOT use this as a sloppy replacement for one of
   the other types.  If you're not using this as part of casting an
   argument to a function call, and you're not Ben Wing, you're using it
   wrong.  Go find another one of the types. */

typedef char Chbyte;
typedef unsigned char UChbyte;
typedef signed char SChbyte;

/* ------------------------ other text-related typedefs ------------------- */

/* To the user, a buffer is made up of characters.  In the non-Mule world,
   characters and Ibytes are equivalent, restricted to the range 0 - 255.
   In the Mule world, many more characters are possible (21 bits worth,
   more or less), and a character requires (typically) 1 to 4 Ibytes for
   its representation in a buffer or string.  Note that the representation
   of a character by itself, in a variable, is very different from its
   representation in a string of text (in a buffer or Lisp string).

   Under Mule, text can be represented in more than one way.  The "default"
   format is variable-width (1 to 4 bytes) and compatible with ASCII --
   ASCII chars are stored in one byte, as themselves, and all other chars
   use only high bytes.  The default format is currently the only format
   used for text stored anywhere but in a buffer.  In a buffer, other
   formats -- fixed-width formats (1, 2, or 4 bytes) -- are possible, for
   speed.

   See text.c/text.h for a detailed discussion of all of this. */

/* A character, as represented on its own. */

typedef int Ichar;

/* The "raw value" of a character as stored in the buffer.  In the default
   format, this is just the same as the character.  In fixed-width formats,
   this is the actual value in the buffer, which will be limited to the
   range as established by the format.  This is used when searching for a
   character in a buffer -- it's faster to convert the character to the raw
   value and look for that, than repeatedly convert each raw value in the
   buffer into a character. */

typedef int Raw_Ichar;

/* Internal text as a series of textual units (8-bit bytes in the old
   "Mule" encoding -- still the standard internal encoding -- and in UTF-8,
   but 16-bit bytes in UTF-16 and 32-bit bytes in UTF-32).  See text.c. */

#ifdef UTF16_IBYTE_FORMAT
#define NON_ASCII_INTERNAL_FORMAT
typedef unsigned short Itext;
#else
typedef Ibyte Itext;
#endif
typedef EMACS_INT Textcount;

#define ITEXT_SIZE (sizeof (Itext))
/* Use this to emphasize that we are adding space for the zero-terminator */
#define ITEXT_ZTERM_SIZE ITEXT_SIZE

/* Wexttext is wchar_t on WIN32_NATIVE (and perhaps other systems that
   support wchar_t's in library functions), and Extbyte otherwise.  This is
   used whenever we have to do any sort of manipulation of
   externally-encoded strings -- generally a very bad idea, and unsafe, but
   in some cases we have no choice (especially at startup, and esp. prior
   to pdump, where we haven't loaded the Unicode tables necessary for
   conversion under Windows).  On platforms where the external encoding may
   be Unicode (i.e. Windows), we always do our manipulations in Unicode,
   converting to and from multibyte if necessary -- otherwise we'd have to
   conditionalize on Unicode vs. multibyte all over the place, which is
   just a nightmare. */
#ifdef WIN32_NATIVE
#define WEXTTEXT_IS_WIDE
typedef wchar_t Wexttext;
#else
typedef Extbyte Wexttext;
#endif

#if !defined (__cplusplus) || !defined (CPLUSPLUS_INTEGRAL_CLASSES_NOT_YET)

/* Counts of bytes or chars */

typedef EMACS_INT Bytecount;
typedef EMACS_INT Charcount;

/* Different ways of referring to a position in a buffer.  We use
   the typedefs in preference to 'EMACS_INT' to make it clearer what
   sort of position is being used.  See text.c for a description
   of the different positions.

   Note that buffer positions are 1-based, and there's a gap in the middle
   of a buffer; that's why we have separate typedefs.  For Lisp strings and
   other strings of text, we just use Bytecount and Charcount. */

typedef EMACS_INT Charbpos;
typedef EMACS_INT Bytebpos;
typedef EMACS_INT Membpos;

/* Different ways of referring to a position that can be either in a buffer
   or string; used when passing around an object that can be either a
   buffer or string, and an associated position.  Conceptually, they
   resolve as follows:

   Typedef		Buffer			String
   ------------------------------------------------------
   Charxpos		Charbpos		Charcount
   Bytexpos		Bytebpos		Bytecount
   Memxpos		Membpos			Bytecount
   
   */

typedef EMACS_INT Charxpos;
typedef EMACS_INT Bytexpos;
typedef EMACS_INT Memxpos;

#else /* __cplusplus */

/* Implement strong type-checking of the above integral types by declaring
   them to be classes and using operator overloading.  Unfortunately this
   is a huge pain in the ass because C++ doesn't strongly distinguish
   "bool" and "size_t" from int.  The problem is especially bad with "bool"
   -- if you want to be able to say `if (len--)' where len is e.g. a
   Bytecount, you need to declare a conversion operator to bool(); and
   since bool is just an alias for int, you suddenly get tons and tons of
   ambiguities, which need to be resolved by lots of laborious declarations
   for every single possible type combination.  Hence the multitude of
   declarations in DECLARE_INTCLASS_ARITH_COMPARE().  The bool/int
   equivalence also means that we have to forcibly block the combinations
   we don't want by creating overloaded versions of them and declaring them
   private. */
   
class Bytecount;
class Bytebpos;
class Bytexpos;
class Charcount;
class Charbpos;
class Charxpos;
class Membpos;
class Memxpos;

/* Declare the arithmetic and comparison operations for an integral class,
   i.e. one of the above classes.  If this is a "position" class, where the
   difference between two positions is a different class (a "count" class),
   then use POSCL for the position class and COUNTCL for the count class.
   If this is a simple class, where all operations yield the same class,
   substitute the same class for POSCL and COUNTCL. */

#define DECLARE_INTCLASS_ARITH_COMPARE(poscl, countcl)			      \
  poscl operator += (const countcl& l) { data += l.data; return *this; }      \
  poscl operator -= (const countcl& l) { data -= l.data; return *this; }      \
  poscl operator + (const countcl& l) const { return poscl (data + l.data); } \
  poscl operator - (const countcl& l) const { return poscl (data - l.data); } \
  poscl operator += (const int& l) { data += l; return *this; }		      \
  poscl operator -= (const int& l) { data -= l; return *this; }		      \
  poscl operator + (const int& l) const { return poscl (data + l); }	      \
  poscl operator - (const int& l) const { return poscl (data - l); }	      \
  poscl operator += (const unsigned int& l) { data += l; return *this; }      \
  poscl operator -= (const unsigned int& l) { data -= l; return *this; }      \
  poscl operator + (const unsigned int& l) const			      \
    { return poscl (data + l); }					      \
  poscl operator - (const unsigned int& l) const			      \
    { return poscl (data - l); }					      \
  poscl operator += (const long& l) { data += l; return *this; }	      \
  poscl operator -= (const long& l) { data -= l; return *this; }	      \
  poscl operator + (const long& l) const { return poscl (data + l); }	      \
  poscl operator - (const long& l) const { return poscl (data - l); }	      \
  poscl operator += (const unsigned long& l) { data += l; return *this; }     \
  poscl operator -= (const unsigned long& l) { data -= l; return *this; }     \
  poscl operator + (const unsigned long& l) const			      \
    { return poscl (data + l); }					      \
  poscl operator - (const unsigned long& l) const			      \
    { return poscl (data - l); }					      \
  poscl operator += (const short& l) { data += l; return *this; }	      \
  poscl operator -= (const short& l) { data -= l; return *this; }	      \
  poscl operator + (const short& l) const { return poscl (data + l); }	      \
  poscl operator - (const short& l) const { return poscl (data - l); }	      \
  poscl operator += (const unsigned short& l) { data += l; return *this; }    \
  poscl operator -= (const unsigned short& l) { data -= l; return *this; }    \
  poscl operator + (const unsigned short& l) const			      \
    { return poscl (data + l); }					      \
  poscl operator - (const unsigned short& l) const			      \
    { return poscl (data - l); }					      \
									      \
  poscl operator *= (const countcl& l) { data *= l.data; return *this; }      \
  poscl operator /= (const countcl& l) { data /= l.data; return *this; }      \
  poscl operator * (const countcl& l) const { return poscl (data * l.data); } \
  poscl operator / (const countcl& l) const { return poscl (data / l.data); } \
  poscl operator *= (const int& l) { data *= l; return *this; }		      \
  poscl operator /= (const int& l) { data /= l; return *this; }		      \
  poscl operator * (const int& l) const { return poscl (data * l); }	      \
  poscl operator / (const int& l) const { return poscl (data / l); }	      \
  poscl operator *= (const unsigned int& l) { data *= l; return *this; }      \
  poscl operator /= (const unsigned int& l) { data /= l; return *this; }      \
  poscl operator * (const unsigned int& l) const { return poscl (data * l); } \
  poscl operator / (const unsigned int& l) const { return poscl (data / l); } \
  poscl operator *= (const long& l) { data *= l; return *this; }	      \
  poscl operator /= (const long& l) { data /= l; return *this; }	      \
  poscl operator * (const long& l) const { return poscl (data * l); }	      \
  poscl operator / (const long& l) const { return poscl (data / l); }	      \
  poscl operator *= (const unsigned long& l) { data *= l; return *this; }     \
  poscl operator /= (const unsigned long& l) { data /= l; return *this; }     \
  poscl operator * (const unsigned long& l) const			      \
    { return poscl (data * l); }					      \
  poscl operator / (const unsigned long& l) const			      \
    { return poscl (data / l); }					      \
  poscl operator *= (const short& l) { data *= l; return *this; }	      \
  poscl operator /= (const short& l) { data /= l; return *this; }	      \
  poscl operator * (const short& l) const { return poscl (data * l); }	      \
  poscl operator / (const short& l) const { return poscl (data / l); }	      \
  poscl operator *= (const unsigned short& l) { data *= l; return *this; }    \
  poscl operator /= (const unsigned short& l) { data /= l; return *this; }    \
  poscl operator * (const unsigned short& l) const			      \
    { return poscl (data * l); }					      \
  poscl operator / (const unsigned short& l) const			      \
    { return poscl (data / l); }					      \
									      \
  poscl operator &= (const countcl& l) { data &= l.data; return *this; }      \
  poscl operator |= (const countcl& l) { data |= l.data; return *this; }      \
  poscl operator & (const countcl& l) const { return poscl (data & l.data); } \
  poscl operator | (const countcl& l) const { return poscl (data | l.data); } \
  poscl operator &= (const int& l) { data &= l; return *this; }		      \
  poscl operator |= (const int& l) { data |= l; return *this; }		      \
  poscl operator & (const int& l) const { return poscl (data & l); }	      \
  poscl operator | (const int& l) const { return poscl (data | l); }	      \
  poscl operator &= (const unsigned int& l) { data &= l; return *this; }      \
  poscl operator |= (const unsigned int& l) { data |= l; return *this; }      \
  poscl operator & (const unsigned int& l) const { return poscl (data & l); } \
  poscl operator | (const unsigned int& l) const { return poscl (data | l); } \
  poscl operator &= (const long& l) { data &= l; return *this; }	      \
  poscl operator |= (const long& l) { data |= l; return *this; }	      \
  poscl operator & (const long& l) const { return poscl (data & l); }	      \
  poscl operator | (const long& l) const { return poscl (data | l); }	      \
  poscl operator &= (const unsigned long& l) { data &= l; return *this; }     \
  poscl operator |= (const unsigned long& l) { data |= l; return *this; }     \
  poscl operator & (const unsigned long& l) const			      \
    { return poscl (data & l); }					      \
  poscl operator | (const unsigned long& l) const			      \
    { return poscl (data | l); }					      \
  poscl operator &= (const short& l) { data &= l; return *this; }	      \
  poscl operator |= (const short& l) { data |= l; return *this; }	      \
  poscl operator & (const short& l) const { return poscl (data & l); }	      \
  poscl operator | (const short& l) const { return poscl (data | l); }	      \
  poscl operator &= (const unsigned short& l) { data &= l; return *this; }    \
  poscl operator |= (const unsigned short& l) { data |= l; return *this; }    \
  poscl operator & (const unsigned short& l) const			      \
    { return poscl (data & l); }					      \
  poscl operator | (const unsigned short& l) const			      \
    { return poscl (data | l); }					      \
									      \
  poscl operator - ()           { return poscl (-data); }		      \
  poscl operator-- ()           { data--; return *this; }		      \
  poscl operator-- (int)	     { data--; return poscl (data + 1); }     \
  poscl operator++ ()           { data++; return *this; }		      \
  poscl operator++ (int)        { data++; return poscl (data - 1); }	      \
									      \
  bool operator < (const poscl& l) const { return data < l.data; }	      \
  bool operator <= (const poscl& l) const { return data <= l.data; }	      \
  bool operator > (const poscl& l) const { return data > l.data; }	      \
  bool operator >= (const poscl& l) const { return data >= l.data; }	      \
  bool operator == (const poscl& l) const { return data == l.data; }	      \
  bool operator != (const poscl& l) const { return data != l.data; }	      \
  bool operator < (const int& l) const { return data < (EMACS_INT) l; }	      \
  bool operator <= (const int& l) const { return data <= (EMACS_INT) l; }     \
  bool operator > (const int& l) const { return data > (EMACS_INT) l; }	      \
  bool operator >= (const int& l) const { return data >= (EMACS_INT) l; }     \
  bool operator == (const int& l) const { return data == (EMACS_INT) l; }     \
  bool operator != (const int& l) const { return data != (EMACS_INT) l; }     \
  bool operator < (const unsigned int& l) const				      \
    { return data < (EMACS_INT) l; }					      \
  bool operator <= (const unsigned int& l) const			      \
    { return data <= (EMACS_INT) l; }					      \
  bool operator > (const unsigned int& l) const				      \
    { return data > (EMACS_INT) l; }					      \
  bool operator >= (const unsigned int& l) const			      \
    { return data >= (EMACS_INT) l; }					      \
  bool operator == (const unsigned int& l) const			      \
    { return data == (EMACS_INT) l; }					      \
  bool operator != (const unsigned int& l) const			      \
    { return data != (EMACS_INT) l; }					      \
  bool operator < (const long& l) const { return data < (EMACS_INT) l; }      \
  bool operator <= (const long& l) const { return data <= (EMACS_INT) l; }    \
  bool operator > (const long& l) const { return data > (EMACS_INT) l; }      \
  bool operator >= (const long& l) const { return data >= (EMACS_INT) l; }    \
  bool operator == (const long& l) const { return data == (EMACS_INT) l; }    \
  bool operator != (const long& l) const { return data != (EMACS_INT) l; }    \
  bool operator < (const unsigned long& l) const			      \
    { return data < (EMACS_INT) l; }					      \
  bool operator <= (const unsigned long& l) const			      \
    { return data <= (EMACS_INT) l; }					      \
  bool operator > (const unsigned long& l) const			      \
    { return data > (EMACS_INT) l; }					      \
  bool operator >= (const unsigned long& l) const			      \
    { return data >= (EMACS_INT) l; }					      \
  bool operator == (const unsigned long& l) const			      \
    { return data == (EMACS_INT) l; }					      \
  bool operator != (const unsigned long& l) const			      \
    { return data != (EMACS_INT) l; }					      \
  bool operator < (const short& l) const { return data < (EMACS_INT) l; }     \
  bool operator <= (const short& l) const { return data <= (EMACS_INT) l; }   \
  bool operator > (const short& l) const { return data > (EMACS_INT) l; }     \
  bool operator >= (const short& l) const { return data >= (EMACS_INT) l; }   \
  bool operator == (const short& l) const { return data == (EMACS_INT) l; }   \
  bool operator != (const short& l) const { return data != (EMACS_INT) l; }   \
  bool operator < (const unsigned short& l) const			      \
    { return data < (EMACS_INT) l; }					      \
  bool operator <= (const unsigned short& l) const			      \
    { return data <= (EMACS_INT) l; }					      \
  bool operator > (const unsigned short& l) const			      \
    { return data > (EMACS_INT) l; }					      \
  bool operator >= (const unsigned short& l) const			      \
    { return data >= (EMACS_INT) l; }					      \
  bool operator == (const unsigned short& l) const			      \
    { return data == (EMACS_INT) l; }					      \
  bool operator != (const unsigned short& l) const			      \
    { return data != (EMACS_INT) l; }					      \
  bool operator ! () const { return !data; }

/* Declare the "bad" or disallowed arithmetic and comparion operations
   between class GOOD and class BAD.  Meant to go inside the private
   section of class GOOD. */

#define DECLARE_BAD_INTCLASS_ARITH_COMPARE(good, bad)	\
  good operator += (const bad& l) { return badret; }	\
  good operator -= (const bad& l) { return badret; }	\
  good operator *= (const bad& l) { return badret; }	\
  good operator /= (const bad& l) { return badret; }	\
  good operator + (const bad& l) { return badret; }	\
  good operator - (const bad& l) { return badret; }	\
  good operator * (const bad& l) { return badret; }	\
  good operator / (const bad& l) { return badret; }	\
							\
  bool operator < (const bad& l)  { return 0; }		\
  bool operator <= (const bad& l) { return 0; }		\
  bool operator > (const bad& l)  { return 0; }		\
  bool operator >= (const bad& l) { return 0; }		\
  bool operator == (const bad& l) { return 0; }		\
  bool operator != (const bad& l) { return 0; }

/* Declare the "bad" or disallowed arithmetic operations between class GOOD
   and another of the same class, for a position class.  Meant to go inside
   the private section of class GOOD. */

#define DECLARE_BAD_POS_CLASS_ARITH(good)		\
  good operator += (const good& l) { return badret; }	\
  good operator -= (const good& l) { return badret; }	\
  good operator *= (const good& l) { return badret; }	\
  good operator /= (const good& l) { return badret; }	\
  good operator + (const good& l) { return badret; }	\
  good operator * (const good& l) { return badret; }	\
  good operator / (const good& l) { return badret; }

/* Basic declaration at the top of all integral classes.  Don't call
   directly, use one of the more specific versions below. */

#define DECLARE_INTCLASS(cl)			\
 public:					\
  EMACS_INT data;				\
  cl () { data = 0xCDCDCDCD; }			\
  cl (int i) { data = i; }			\
  cl (unsigned int i) { data = i; }		\
  cl (long i) { data = i; }			\
  cl (unsigned long i) { data = i; }		\
  cl (short i) { data = i; }			\
  cl (unsigned short i) { data = i; }		\
  operator EMACS_INT ()  const { return data; }

/* Basic declaration at the top of all count classes. */

#define DECLARE_COUNT_CLASS(cl)				\
  DECLARE_INTCLASS (cl)					\
  DECLARE_INTCLASS_ARITH_COMPARE (cl, cl)		\
 private:						\
  static cl badret;

/* Basic declaration at the bottom of the prelude of all position classes.
   Don't call directly. */

#define DECLARE_POS_CLASS_SECOND_HALF(cl, countcl)			     \
  DECLARE_INTCLASS_ARITH_COMPARE (cl, countcl)				     \
  countcl operator - (const cl& l) const { return countcl (data - l.data); } \
 private:								     \
  static cl badret;							     \
  DECLARE_BAD_POS_INTCLASS_ARITH (cl)

/* Basic declaration at the top of all buffer position classes. */

#define DECLARE_BPOS_CLASS(cl, countcl)		\
  DECLARE_INTCLASS (cl)				\
  DECLARE_POS_CLASS_SECOND_HALF (cl, countcl)

/* Basic declaration at the top of all X-position classes (that can refer
   to buffers or strings).  CL1 and CL2 are the equivalent more specific
   classes referring only to buffers or strings, respectively. */

#define DECLARE_XPOS_CLASS(cl, countcl, cl1, cl2)	\
  DECLARE_INTCLASS (cl)					\
  cl (const cl1& x) { data = x.data; }			\
  cl (const cl2& x) { data = x.data; }			\
  operator cl1 () const { return cl1 (data); }		\
  operator cl2 () const { return cl2 (data); }		\
  DECLARE_POS_CLASS_SECOND_HALF (cl, countcl)

/* Declare the "bad" or disallowed arithmetic and comparion operations
   between class CHARCL (a character class) and various non-character
   classes.  Meant to go inside the private section of class GOOD. */

#define DECLARE_BAD_CHAR_INTCLASS_ARITH_COMPARE(charcl)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (charcl, Bytecount)	\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (charcl, Bytebpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (charcl, Bytexpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (charcl, Membpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (charcl, Memxpos)

/* Declare the "bad" or disallowed arithmetic and comparion operations
   between class BYTECL (a byte class) and various non-byte classes.
   Meant to go inside the private section of class GOOD. */

#define DECLARE_BAD_BYTE_INTCLASS_ARITH_COMPARE(bytecl)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charcount)	\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charbpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charxpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Membpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Memxpos)

/* Declare the "bad" or disallowed arithmetic and comparion operations
   between class BYTECL (a mem class) and various non-mem classes.
   Meant to go inside the private section of class GOOD. */

#define DECLARE_BAD_MEM_INTCLASS_ARITH_COMPARE(bytecl)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charcount)	\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charbpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Charxpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Bytebpos)		\
  DECLARE_BAD_INTCLASS_ARITH_COMPARE (bytecl, Bytexpos)

class Charcount
{
  DECLARE_COUNT_CLASS (Charcount)
  DECLARE_BAD_CHAR_INTCLASS_ARITH_COMPARE (Charcount)
};

class Charbpos
{
  DECLARE_BPOS_CLASS (Charbpos, Charcount)
  DECLARE_BAD_CHAR_INTCLASS_ARITH_COMPARE (Charbpos)
};

class Charxpos
{
  DECLARE_XPOS_CLASS (Charxpos, Charcount, Charbpos, Charcount)
  DECLARE_BAD_CHAR_INTCLASS_ARITH_COMPARE (Charxpos)
};

class Bytecount
{
  DECLARE_COUNT_CLASS (Bytecount)
  DECLARE_BAD_BYTE_INTCLASS_ARITH_COMPARE (Bytecount)
};

class Bytebpos
{
  DECLARE_BPOS_CLASS (Bytebpos, Bytecount)
  DECLARE_BAD_BYTE_INTCLASS_ARITH_COMPARE (Bytebpos)
};

class Bytexpos
{
  DECLARE_XPOS_CLASS (Bytexpos, Bytecount, Bytebpos, Bytecount)
  DECLARE_BAD_BYTE_INTCLASS_ARITH_COMPARE (Bytexpos)
};

class Membpos
{
  DECLARE_BPOS_CLASS (Membpos, Bytecount)
  DECLARE_BAD_MEM_INTCLASS_ARITH_COMPARE (Membpos)
};

class Memxpos
{
  DECLARE_XPOS_CLASS (Memxpos, Bytecount, Membpos, Bytecount)
  DECLARE_BAD_MEM_INTCLASS_ARITH_COMPARE (Memxpos)
};

#define DECLARE_POINTER_TYPE_ARITH_COUNT(pointer, countcl)		\
inline pointer operator += (const pointer & x, const countcl& y)	\
{ x += y.data; return x; }						\
inline pointer operator -= (const pointer & x, const countcl& y)	\
{ x -= y.data; return x; }						\
inline pointer operator + (const pointer x, const countcl& y)		\
{ return x + y.data; }							\
inline pointer operator - (const pointer x, const countcl& y)		\
{ return x - y.data; }

#define DECLARE_INTEGRAL_TYPE_ARITH_COUNT(integral, countcl)	\
inline integral operator += (integral & x, const countcl& y)	\
{ x += y.data; return x; }					\
inline integral operator -= (integral & x, const countcl& y)	\
{ x -= y.data; return x; }					\
inline countcl operator + (integral x, const countcl& y)	\
{ return countcl (x + y.data); }				\
inline countcl operator - (integral x, const countcl& y)	\
{ return countcl (x - y.data); }

#define DECLARE_INTEGRAL_TYPE_COMPARE(integral, cl)	\
inline bool operator < (integral x, const cl& y)	\
  { return (EMACS_INT) x < y.data; }			\
inline bool operator <= (integral x, const cl& y)	\
  { return (EMACS_INT) x <= y.data; }			\
inline bool operator > (integral x, const cl& y)	\
  { return (EMACS_INT) x > y.data; }			\
inline bool operator >= (integral x, const cl& y)	\
  { return (EMACS_INT) x >= y.data; }			\
inline bool operator == (integral x, const cl& y)	\
  { return (EMACS_INT) x == y.data; }			\
inline bool operator != (integral x, const cl& y)	\
  { return (EMACS_INT) x != y.data; }

#if 0
/* Unfortunately C++ doesn't let you overload the ?: operator, so we have
   to manually deal with ambiguities using casting */
#define DECLARE_INTEGRAL_TYPE_TRISTATE(integral, cl)	\
inline cl operator ?: (bool b, integral x, const cl& y)	\
  { return b ? cl (x) : y; }				\
inline cl operator ?: (bool b, const cl& x, integral y)	\
  { return b ? x : cl (y); }
#endif /* 0 */

/* DECLARE_POINTER_TYPE_ARITH_COUNT (const Ibyte *, Bytecount);
   DECLARE_POINTER_TYPE_ARITH_COUNT (const Extbyte *, Bytecount); */
DECLARE_POINTER_TYPE_ARITH_COUNT (Ibyte *, Bytecount);
DECLARE_POINTER_TYPE_ARITH_COUNT (Extbyte *, Bytecount);

DECLARE_INTEGRAL_TYPE_ARITH_COUNT (int, Bytecount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (int, Charcount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (unsigned int, Bytecount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (unsigned int, Charcount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (long, Bytecount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (long, Charcount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (unsigned long, Bytecount);
DECLARE_INTEGRAL_TYPE_ARITH_COUNT (unsigned long, Charcount);

DECLARE_INTEGRAL_TYPE_COMPARE (int, Bytecount);
DECLARE_INTEGRAL_TYPE_COMPARE (int, Charcount);
DECLARE_INTEGRAL_TYPE_COMPARE (unsigned int, Bytecount);
DECLARE_INTEGRAL_TYPE_COMPARE (unsigned int, Charcount);
DECLARE_INTEGRAL_TYPE_COMPARE (long, Bytecount);
DECLARE_INTEGRAL_TYPE_COMPARE (long, Charcount);
DECLARE_INTEGRAL_TYPE_COMPARE (unsigned long, Bytecount);
DECLARE_INTEGRAL_TYPE_COMPARE (unsigned long, Charcount);

#if 0 /* doesn't work */
inline Bytecount operator - (const Ibyte *x, const Ibyte *y)	\
  { return Bytecount (x - y); }
#endif

#endif /* __cplusplus */

/* Counts of elements */
typedef EMACS_INT Elemcount;
/* Hash codes */
typedef unsigned long Hashcode;
/* Booleans */
typedef int Boolint;

/* ------------------------ basic compiler defines ------------------- */

#include "compiler.h"

/* ------------------------ alignment definitions ------------------- */

/* No type has a greater alignment requirement than max_align_t.
   (except perhaps for types we don't use, like long double) */
typedef union
{
  struct { long l; } l;
  struct { void *p; } p;
  struct { void (*f)(void); } f;
  struct { double d; } d;
} max_align_t;

/* ALIGNOF returns the required alignment of a type -- i.e. a value such
   that data of this type must begin at a memory address which is a
   multiple of that value.  For simple types, this is often the same size
   as the type itself. */

#ifndef ALIGNOF
# if defined (__GNUC__) && (__GNUC__ >= 2)
/* gcc has an extension that gives us exactly what we want. */
#  define ALIGNOF(type) __alignof__ (type)
# elif ! defined (__cplusplus)
/* The following is mostly portable, except that:
   - it doesn't work for inside out declarations like void (*) (void).
     (so just call ALIGNOF with a typedef'ed name)
   - it doesn't work with C++.  The C++ committee has decided,
     in its infinite wisdom, that:
     "Types must be declared in declarations, not in expressions." */
#  define ALIGNOF(type) offsetof (struct { char c; type member; }, member)
# else
/* C++ is annoying, but it has a big bag of tricks.
   The following doesn't have the "inside out" declaration bug C does. */
template<typename T> struct alignment_trick { char c; T member; };
#  define ALIGNOF(type) offsetof (alignment_trick<type>, member)
# endif
#endif /* ALIGNOF */

/* ALIGN_SIZE returns the smallest size greater than or equal to LEN which
   is a multiple of UNIT.  This can be used to assure that data that
   follows a block of the returned size is of correct alignment for a type
   whose alignment (as returned by ALIGNOF) is UNIT (provided that the
   block itself is correctly aligned for this type; memory returned by
   malloc() is guaranteed to be correctly aligned for all types). */

#define ALIGN_SIZE(len, unit) \
  ((((len) + (unit) - 1) / (unit)) * (unit))

/* ALIGN_FOR_TYPE returns the smallest size greater than or equal to LEN
   which is aligned for the given type.  This can be used to assure that
   data that follows a block of the returned size is of correct alignment
   for the type (provided that the block itself is correctly aligned for
   this type; memory returned by malloc() is guaranteed to be correctly
   aligned for all types). */

#define ALIGN_FOR_TYPE(len, type) ALIGN_SIZE (len, ALIGNOF (type))

/* MAX_ALIGN_SIZE returns the smallest size greater than or equal to LEN
   which guarantees that data following a block of such size is correctly
   aligned for all types (provided that the block itself is so aligned,
   which is the case for memory returned by malloc()). */

#define MAX_ALIGN_SIZE(len) ALIGN_FOR_TYPE (len, max_align_t)

/* ALIGN_PTR returns the smallest pointer >= PTR which is aligned for
   data of TYPE. */
#define ALIGN_PTR(ptr, type) ((void *) ALIGN_FOR_TYPE ((size_t) (ptr), type))

BEGIN_C_DECLS

/* ------------------------ assertions ------------------- */

/* We define assert iff USE_ASSERTIONS or DEBUG_XEMACS is defined.
   Otherwise we define it to be empty.  Quantify has shown that the
   time the assert checks take is measurable so let's not include them
   in production binaries.

   If ASSERTIONS_DONT_ABORT defined, we will continue after assertion
   failures.

   assert_at_line() is used for asserts inside of inline functions called
   from error-checking macros.  If we're not tricky, we just get the file
   and line of the inline function, which is not very useful. */

/* Highly dubious kludge */
/*   (thanks, Jamie, I feel better now -- ben) */
MODULE_API void assert_failed (const Ascbyte *, int, const Ascbyte *);
#define ABORT() (assert_failed (__FILE__, __LINE__, "ABORT()"))

#ifdef USE_ASSERTIONS
# define assert(x) ((x) ? (void) 0 : assert_failed (__FILE__, __LINE__, #x))
# define assert_with_message(x, msg) \
  ((x) ? (void) 0 : assert_failed (__FILE__, __LINE__, msg))
# define assert_at_line(x, file, line) \
  ((x) ? (void) 0 : assert_failed (file, line, #x))
#elif defined (DEBUG_XEMACS)
# define assert(x) ((x) ? (void) 0 : (void) ABORT ())
# define assert_with_message(x, msg) assert (x)
# define assert_at_line(x, file, line) assert (x)
#else
/* This used to be ((void) (0)) but that triggers lots of unused variable
   warnings.  It's pointless to force all that code to be rewritten, with
   added ifdefs.  Any reasonable compiler will eliminate an expression with
   no effects. */
# define assert(x) ((void) (x))
# define assert_with_message(x, msg) assert (x)
# define assert_at_line(x, file, line) assert (x)
#endif

/************************************************************************/
/**                         Memory allocation                          **/
/************************************************************************/

/* ------------------------ simple memory allocation ------------------- */

/* Basic memory allocation and freeing functions */
void malloc_warning (const Ascbyte *);
MODULE_API void *xmalloc (Bytecount size) ATTRIBUTE_MALLOC;
MODULE_API void *xmalloc_and_zero (Bytecount size) ATTRIBUTE_MALLOC;
MODULE_API void *xrealloc (void *, Bytecount size) ATTRIBUTE_MALLOC;
MODULE_API Chbyte *xstrdup (const Chbyte *) ATTRIBUTE_MALLOC;

/* Basic free function */

MODULE_API void xfree_1 (void *);
#ifdef ERROR_CHECK_MALLOC
/* This used to use a temporary variable, which both avoided the multiple
   evaluation and obviated the need for the TYPE argument.  But that triggered
   complaints under strict aliasing. #### There should be a better way. */
#define xfree(lvalue, type) do						\
{									\
  xfree_1 (lvalue);							\
  VOIDP_CAST (lvalue) = (void *) DEADBEEF_CONSTANT;                     \
} while (0)
#else
#define xfree(lvalue,type) xfree_1 (lvalue)
#endif /* ERROR_CHECK_MALLOC */

/* ------------------------ stack allocation -------------------------- */

/* Allocating excessively large blocks on the stack can cause crashes.
   We provide MALLOC_OR_ALLOCA() below for places where it's likely that
   large amounts will be allocated; it mallocs the block if it's too big.
   Unfortunately, that requires a call to unbind_to() at the end of the
   function, and it's not feasible to rewrite all calls to alloca() this
   way.

   Instead, we use the portable C alloca() substitute in alloca.c above a
   certain size.  This actually uses malloc(), but checks the current stack
   pointer to see if data from previous alloca() calls needs to be freed.
   However, this can lead to large heap sizes -- especially since cleanup
   can only happen in a parent function, and will never happen if (as will
   often be the case) it's the same function in the same place in the code
   that keeps tripping the alloca() limit.

   So we set up a system to periodically force cleanup.  Currently we
   do cleanup:

   -- Only when there's C alloca() data, and then
   -- Every stack alloca() or allocation of Lisp data, every call to
      next_event_internal() [typically near the top of the stack],
      or every 10th funcall

   This should not be a big penalty because

   (a) If there are few C alloca() chunks, checking them will be fast
   (b) If not, we've allocated a huge amount of heap space (remember, each
       chunk represents > 256K of heap), and we really want them gone
*/

/* We use a larger maximum when the choice is alloca() vs. the C alloca()
   substitute than when the choice is vs. malloc(), because in the former
   case, our alternative choice is less palatable because the memory may
   not be freed for awhile. */

#define MAX_ALLOCA_VS_C_ALLOCA 262144
#define MAX_ALLOCA_VS_MALLOC 65536

#define MAX_FUNCALLS_BETWEEN_ALLOCA_CLEANUP 10

extern MODULE_API Bytecount __temp_alloca_size__;
extern Bytecount funcall_alloca_count;

#ifdef ERROR_CHECK_MALLOC
extern MODULE_API int regex_malloc_disallowed;
#define REGEX_MALLOC_CHECK() assert (!regex_malloc_disallowed)
#else
#define REGEX_MALLOC_CHECK() ((void) 0)
#endif

/* Do stack or heap alloca() depending on size.

NOTE: The use of a global temporary like this is unsafe if ALLOCA() occurs
twice anywhere in the same expression; but that seems highly unlikely.  The
alternative is to force all callers to declare a local temporary if the
expression has side effects -- something easy to forget. */

#define ALLOCA(size)					\
  (REGEX_MALLOC_CHECK (),				\
   __temp_alloca_size__ = (size),			\
   __temp_alloca_size__  > MAX_ALLOCA_VS_C_ALLOCA ?	\
   xemacs_c_alloca (__temp_alloca_size__) :		\
   (need_to_check_c_alloca ? xemacs_c_alloca (0) : 0,	\
    alloca (__temp_alloca_size__)))

/* Version of ALLOCA() that is guaranteed to work inside of function calls
   (i.e., we call the C alloca if regular alloca() is broken inside of
   function calls). */
#ifdef BROKEN_ALLOCA_IN_FUNCTION_CALLS
#define ALLOCA_FUNCALL_OK(size) xemacs_c_alloca (size)
#else
#define ALLOCA_FUNCALL_OK(size) ALLOCA (size)
#endif

MODULE_API void *xemacs_c_alloca (unsigned int size) ATTRIBUTE_MALLOC;

MODULE_API int record_unwind_protect_freeing (void *ptr);

DECLARE_INLINE_HEADER (
void *
xmalloc_and_record_unwind (Bytecount size)
)
{
  void *ptr = xmalloc (size);
  record_unwind_protect_freeing (ptr);
  return ptr;
}

/* WARNING: If you use this, you must unbind_to() at the end of your
   function! */

#define MALLOC_OR_ALLOCA(size)				\
  (REGEX_MALLOC_CHECK (),				\
   __temp_alloca_size__ = (size),			\
   __temp_alloca_size__  > MAX_ALLOCA_VS_MALLOC ?	\
   xmalloc_and_record_unwind (__temp_alloca_size__) :	\
   (need_to_check_c_alloca ? xemacs_c_alloca (0) : 0,	\
    alloca (__temp_alloca_size__)))

/* -------------- convenience functions for memory allocation ------------- */

#define countof(x) ((int) (sizeof(x)/sizeof((x)[0])))
#define xnew(type) ((type *) xmalloc (sizeof (type)))
#define xnew_array(type, len) ((type *) xmalloc ((len) * sizeof (type)))
#define xnew_and_zero(type) ((type *) xmalloc_and_zero (sizeof (type)))
#define xzero(lvalue) ((void) memset (&(lvalue), '\0', sizeof (lvalue)))
#define xnew_array_and_zero(type, len) ((type *) xmalloc_and_zero ((len) * sizeof (type)))

#define alloca_new(type) ((type *) ALLOCA (sizeof (type)))
#define alloca_array(type, len) ((type *) ALLOCA ((len) * sizeof (type)))

#define alloca_itexts(num) alloca_array (Itext, num)
#define alloca_ibytes(num) alloca_array (Ibyte, num)
#define alloca_extbytes(num) alloca_array (Extbyte, num)
#define alloca_rawbytes(num) alloca_array (Rawbyte, num)
#define alloca_binbytes(num) alloca_array (Binbyte, num)
#define alloca_ascbytes(num) alloca_array (Ascbyte, num)
#define xmalloc_itexts(num) xnew_array (Itext, num)
#define xnew_ibytes(num) xnew_array (Ibyte, num)
#define xnew_extbytes(num) xnew_array (Extbyte, num)
#define xnew_rawbytes(num) xnew_array (Rawbyte, num)
#define xnew_binbytes(num) xnew_array (Binbyte, num)
#define xnew_ascbytes(num) xnew_array (Ascbyte, num)

/* Make an alloca'd copy of a Ibyte * */
#define IBYTE_STRING_TO_ALLOCA(p, lval)		\
do {						\
  Ibyte **_bsta_ = (Ibyte **) &(lval);		\
  const Ibyte *_bsta_2 = (p);			\
  Bytecount _bsta_3 = qxestrlen (_bsta_2);	\
  *_bsta_ = alloca_ibytes (1 + _bsta_3);	\
  memcpy (*_bsta_, _bsta_2, 1 + _bsta_3);	\
} while (0)

/* ----------------- convenience functions for reallocation --------------- */

#define XREALLOC_ARRAY(ptr, type, len) \
  ((void) (ptr = (type *) xrealloc (ptr, (len) * sizeof (type))))

/* also generally useful if you want to avoid arbitrary size limits
   but don't need a full dynamic array.  Assumes that BASEVAR points
   to a malloced array of TYPE objects (or possibly a NULL pointer,
   if SIZEVAR is 0), with the total size stored in SIZEVAR.  This
   macro will realloc BASEVAR as necessary so that it can hold at
   least NEEDED_SIZE objects.  The reallocing is done by doubling,
   which ensures constant amortized time per element. */
#define DO_REALLOC(basevar, sizevar, needed_size, type)	do {	\
  Bytecount do_realloc_needed_size = (needed_size);		\
  REGEX_MALLOC_CHECK ();					\
  if ((sizevar) < do_realloc_needed_size)			\
    {								\
      if ((sizevar) < 32)					\
	(sizevar) = 32;						\
      while ((sizevar) < do_realloc_needed_size)		\
	(sizevar) *= 2;						\
      XREALLOC_ARRAY (basevar, type, (sizevar));		\
    }								\
} while (0)

/************************************************************************/
/**                 Definitions of more complex types                  **/
/************************************************************************/

/* Note that the simplest typedefs are near the top of this file. */

/* We put typedefs here so that prototype declarations don't choke.
   Note that we don't actually declare the structures here (except
   maybe for simple structures like Dynarrs); that keeps them private
   to the routines that actually use them. */

/* ------------------------------- */
/*    Error_Behavior typedefs      */
/* ------------------------------- */

#ifndef ERROR_CHECK_TYPES

typedef enum error_behavior
{
  ERROR_ME,
  ERROR_ME_NOT,
  ERROR_ME_WARN,
  ERROR_ME_DEBUG_WARN
} Error_Behavior;

#define ERRB_EQ(a, b) ((a) == (b))

#else

/* By defining it like this, we provide strict type-checking
   for code that lazily uses ints. */

typedef struct _error_behavior_struct_
{
  int really_unlikely_name_to_have_accidentally_in_a_non_errb_structure;
} Error_Behavior;

extern Error_Behavior ERROR_ME;
extern Error_Behavior ERROR_ME_NOT;
extern Error_Behavior ERROR_ME_WARN;
extern Error_Behavior ERROR_ME_DEBUG_WARN;

#define ERRB_EQ(a, b)							   \
 ((a).really_unlikely_name_to_have_accidentally_in_a_non_errb_structure == \
  (b).really_unlikely_name_to_have_accidentally_in_a_non_errb_structure)

#endif

/* ------------------------------- */
/*  Empty structures and typedefs  */
/* ------------------------------- */

struct buffer;                  /* "buffer.h" */
struct console;			/* "console.h" */
struct device;			/* "device.h" */
struct extent_fragment;
struct extent;
struct frame;			/* "frame.h" */
struct window;                  /* "window.h" */
struct utimbuf;                 /* "systime.h" or <utime.h> */
struct display_line;
struct display_glyph_area;
struct display_box;
struct redisplay_info;
struct window_mirror;
struct scrollbar_instance;
struct font_metric_info;
struct face_cachel;
struct console_type_entry;

/* This is shared by process.h, events.h and others in future.
   See events.h for description */
typedef unsigned EMACS_INT USID;
typedef int face_index;
typedef int glyph_index;
typedef struct lstream Lstream; /* lstream.h */
typedef struct extent *EXTENT; /* extents-impl.h */
typedef struct Lisp_Event Lisp_Event; /* "events.h" */
typedef struct Lisp_Face Lisp_Face;   /* "faces-impl.h" */
typedef struct Lisp_Process Lisp_Process; /* "procimpl.h" */
typedef struct Lisp_Color_Instance Lisp_Color_Instance; /* objects-impl.h */
typedef struct Lisp_Font_Instance Lisp_Font_Instance; /* objects-impl.h */
typedef struct Lisp_Image_Instance Lisp_Image_Instance; /* glyphs.h */
typedef struct Lisp_Gui_Item Lisp_Gui_Item;

/* ------------------------------- */
/*          enum typedefs          */
/* ------------------------------- */

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

enum edge_style
{
  EDGE_ETCHED_IN,
  EDGE_ETCHED_OUT,
  EDGE_BEVEL_IN,
  EDGE_BEVEL_OUT
};

enum munge_me_out_the_door
{
  MUNGE_ME_FUNCTION_KEY,
  MUNGE_ME_KEY_TRANSLATION
};

/* ------------------------------- */
/*                misc             */
/* ------------------------------- */

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


/************************************************************************/
/*		     Definition of Lisp_Object data type		*/
/************************************************************************/

/* Define the fundamental Lisp data structures */

/* This is the set of Lisp data types */

enum Lisp_Type
{
  Lisp_Type_Record,
  Lisp_Type_Int_Even,
  Lisp_Type_Char,
  Lisp_Type_Int_Odd
};

#define POINTER_TYPE_P(type) ((type) == Lisp_Type_Record)

/* Overridden by m/next.h */
#ifndef ASSERT_VALID_POINTER
# define ASSERT_VALID_POINTER(pnt) (assert ((((EMACS_UINT) pnt) & 3) == 0))
#endif

#define GCMARKBITS  0
#define GCTYPEBITS  2
#define GCBITS      2
#define INT_GCBITS  1

#define INT_VALBITS (BITS_PER_EMACS_INT - INT_GCBITS)
#define VALBITS (BITS_PER_EMACS_INT - GCBITS)
#define EMACS_INT_MAX ((EMACS_INT) ((1UL << (INT_VALBITS - 1)) -1UL))
#define EMACS_INT_MIN (-(EMACS_INT_MAX) - 1)
/* WARNING: evaluates its arg twice. */
#define NUMBER_FITS_IN_AN_EMACS_INT(num) \
  ((num) <= EMACS_INT_MAX && (num) >= EMACS_INT_MIN)

#ifdef USE_UNION_TYPE
# include "lisp-union.h"
#else /* !USE_UNION_TYPE */
# include "lisp-disunion.h"
#endif /* !USE_UNION_TYPE */

#define XPNTR(x) ((void *) XPNTRVAL(x))

/* Close your eyes now lest you vomit or spontaneously combust ... */

#define HACKEQ_UNSAFE(obj1, obj2)				\
  (EQ (obj1, obj2) || (!POINTER_TYPE_P (XTYPE (obj1))		\
		       && !POINTER_TYPE_P (XTYPE (obj2))	\
		       && XCHAR_OR_INT (obj1) == XCHAR_OR_INT (obj2)))

#ifdef DEBUG_XEMACS
extern MODULE_API int debug_issue_ebola_notices;
MODULE_API int eq_with_ebola_notice (Lisp_Object, Lisp_Object);
#define EQ_WITH_EBOLA_NOTICE(obj1, obj2)				\
  (debug_issue_ebola_notices ? eq_with_ebola_notice (obj1, obj2)	\
   : EQ (obj1, obj2))
#else
#define EQ_WITH_EBOLA_NOTICE(obj1, obj2) EQ (obj1, obj2)
#endif

/* OK, you can open them again */

END_C_DECLS

/************************************************************************/
/**		     Definitions of basic Lisp objects		       **/
/************************************************************************/

#include "lrecord.h"

BEGIN_C_DECLS

/* ------------------------ dynamic arrays ------------------- */

#ifdef NEW_GC
#ifdef ERROR_CHECK_STRUCTURES
#define Dynarr_declare(type)				\
  struct lrecord_header header;				\
  type *base;						\
  const struct lrecord_implementation *lisp_imp;	\
  int locked;						\
  int elsize;						\
  int cur;						\
  int largest;						\
  int max
#else
#define Dynarr_declare(type)				\
  struct lrecord_header header;				\
  type *base;						\
  const struct lrecord_implementation *lisp_imp;	\
  int elsize;						\
  int cur;						\
  int largest;						\
  int max
#endif /* ERROR_CHECK_STRUCTURES */
#else /* not NEW_GC */
#ifdef ERROR_CHECK_STRUCTURES
#define Dynarr_declare(type)				\
  struct lrecord_header header;				\
  type *base;						\
  int locked;						\
  int elsize;						\
  int cur;						\
  int largest;						\
  int max
#else
#define Dynarr_declare(type)				\
  struct lrecord_header header;				\
  type *base;						\
  int elsize;						\
  int cur;						\
  int largest;						\
  int max
#endif /* ERROR_CHECK_STRUCTURES */
#endif /* not NEW_GC */

typedef struct dynarr
{
  Dynarr_declare (void);
} Dynarr;

MODULE_API void *Dynarr_newf (int elsize);
MODULE_API void Dynarr_resize (void *dy, Elemcount size);
MODULE_API void Dynarr_insert_many (void *d, const void *el, int len, int start);
MODULE_API void Dynarr_delete_many (void *d, int start, int len);
MODULE_API void Dynarr_free (void *d);

#ifdef NEW_GC
MODULE_API void *Dynarr_lisp_newf (int elsize,
				   const struct lrecord_implementation 
				   *dynarr_imp,
				   const struct lrecord_implementation *imp);

#define Dynarr_lisp_new(type, dynarr_imp, imp)			\
  ((type##_dynarr *) Dynarr_lisp_newf (sizeof (type), dynarr_imp, imp))
#define Dynarr_lisp_new2(dynarr_type, type, dynarr_imp, imp)	\
  ((dynarr_type *) Dynarr_lisp_newf (sizeof (type)), dynarr_imp, imp)
#endif /* NEW_GC */
#define Dynarr_new(type) ((type##_dynarr *) Dynarr_newf (sizeof (type)))
#define Dynarr_new2(dynarr_type, type) \
  ((dynarr_type *) Dynarr_newf (sizeof (type)))
#define Dynarr_at(d, pos) ((d)->base[pos])
#define Dynarr_atp(d, pos) (&Dynarr_at (d, pos))
#define Dynarr_begin(d) Dynarr_atp (d, 0)
#define Dynarr_end(d) Dynarr_atp (d, Dynarr_length (d) - 1)
#define Dynarr_sizeof(d) ((d)->cur * (d)->elsize)

#ifdef ERROR_CHECK_STRUCTURES
DECLARE_INLINE_HEADER (
Dynarr *
Dynarr_verify_1 (void *d, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  assert_at_line (dy->cur >= 0 && dy->cur <= dy->largest &&
		  dy->largest <= dy->max, file, line);
  return dy;
}

DECLARE_INLINE_HEADER (
Dynarr *
Dynarr_verify_mod_1 (void *d, const Ascbyte *file, int line)
)
{
  Dynarr *dy = (Dynarr *) d;
  assert_at_line (!dy->locked, file, line);
  assert_at_line (dy->cur >= 0 && dy->cur <= dy->largest &&
		  dy->largest <= dy->max, file, line);
  return dy;
}

#define Dynarr_verify(d) Dynarr_verify_1 (d, __FILE__, __LINE__)
#define Dynarr_verify_mod(d) Dynarr_verify_mod_1 (d, __FILE__, __LINE__)
#define Dynarr_lock(d) (Dynarr_verify_mod (d)->locked = 1)
#define Dynarr_unlock(d) ((d)->locked = 0)
#else
#define Dynarr_verify(d) (d)
#define Dynarr_verify_mod(d) (d)
#define Dynarr_lock(d)
#define Dynarr_unlock(d)
#endif /* ERROR_CHECK_STRUCTURES */

#define Dynarr_length(d) (Dynarr_verify (d)->cur)
#define Dynarr_largest(d) (Dynarr_verify (d)->largest)
#define Dynarr_reset(d) (Dynarr_verify_mod (d)->cur = 0)
#define Dynarr_add_many(d, el, len) Dynarr_insert_many (d, el, len, (d)->cur)
#define Dynarr_insert_many_at_start(d, el, len)	\
  Dynarr_insert_many (d, el, len, 0)
#define Dynarr_add_literal_string(d, s) Dynarr_add_many (d, s, sizeof (s) - 1)
#define Dynarr_add_lisp_string(d, s, codesys)			\
do {								\
  Lisp_Object dyna_ls_s = (s);					\
  Lisp_Object dyna_ls_cs = (codesys);				\
  Extbyte *dyna_ls_eb;						\
  Bytecount dyna_ls_bc;						\
								\
  LISP_STRING_TO_SIZED_EXTERNAL (dyna_ls_s, dyna_ls_eb,		\
				 dyna_ls_bc, dyna_ls_cs);	\
  Dynarr_add_many (d, dyna_ls_eb, dyna_ls_bc);			\
} while (0)

#ifdef NEW_GC
#define Dynarr_add(d, el)					\
do {								\
  const struct lrecord_implementation *imp = (d)->lisp_imp;	\
  if (Dynarr_verify_mod (d)->cur >= (d)->max)			\
    Dynarr_resize ((d), (d)->cur+1);				\
  ((d)->base)[(d)->cur] = (el);					\
								\
  if (imp)							\
    set_lheader_implementation					\
     ((struct lrecord_header *)&(((d)->base)[(d)->cur]), imp);	\
								\
  (d)->cur++;							\
  if ((d)->cur > (d)->largest)					\
    (d)->largest = (d)->cur;					\
} while (0)
#else /* not NEW_GC */
#define Dynarr_add(d, el) (						     \
  Dynarr_verify_mod (d)->cur >= (d)->max ? Dynarr_resize ((d), (d)->cur+1) : \
      (void) 0,								     \
  ((d)->base)[(d)->cur++] = (el),					     \
  (d)->cur > (d)->largest ? (d)->largest = (d)->cur : (int) 0)
#endif /* not NEW_GC */
    

/* The following defines will get you into real trouble if you aren't
   careful.  But they can save a lot of execution time when used wisely. */
#define Dynarr_increment(d) (Dynarr_verify_mod (d)->cur++)
#define Dynarr_set_size(d, n) (Dynarr_verify_mod (d)->cur = n)

#define Dynarr_pop(d)					\
  (assert ((d)->cur > 0), Dynarr_verify_mod (d)->cur--,	\
   Dynarr_at (d, (d)->cur))
#define Dynarr_delete(d, i) Dynarr_delete_many (d, i, 1)
#define Dynarr_delete_by_pointer(d, p) \
  Dynarr_delete_many (d, (p) - ((d)->base), 1)

#define Dynarr_delete_object(d, el)		\
do						\
{						\
  REGISTER int i;				\
  for (i = Dynarr_length (d) - 1; i >= 0; i--)	\
    {						\
      if (el == Dynarr_at (d, i))		\
	Dynarr_delete_many (d, i, 1);		\
    }						\
} while (0)

#ifdef MEMORY_USAGE_STATS
struct overhead_stats;
Bytecount Dynarr_memory_usage (void *d, struct overhead_stats *stats);
#endif

void *stack_like_malloc (Bytecount size);
void stack_like_free (void *val);

/* ------------------------------- */
/*         Dynarr typedefs         */
/* ------------------------------- */

/* Dynarr typedefs -- basic types first */

typedef struct
{
  Dynarr_declare (Ibyte);
} Ibyte_dynarr;

typedef struct
{
  Dynarr_declare (Extbyte);
} Extbyte_dynarr;

typedef struct
{
  Dynarr_declare (Ichar);
} Ichar_dynarr;

typedef struct
{
  Dynarr_declare (char);
} char_dynarr;

typedef struct
{
  Dynarr_declare (char *);
} char_ptr_dynarr;

typedef unsigned char unsigned_char;
typedef struct
{
  Dynarr_declare (unsigned char);
} unsigned_char_dynarr;

typedef unsigned long unsigned_long;
typedef struct
{
  Dynarr_declare (unsigned long);
} unsigned_long_dynarr;

typedef struct
{
  Dynarr_declare (int);
} int_dynarr;

typedef struct
{
  Dynarr_declare (Charbpos);
} Charbpos_dynarr;

typedef struct
{
  Dynarr_declare (Bytebpos);
} Bytebpos_dynarr;

typedef struct
{
  Dynarr_declare (Charcount);
} Charcount_dynarr;

typedef struct
{
  Dynarr_declare (Bytecount);
} Bytecount_dynarr;

/* Dynarr typedefs -- more complex types */

typedef struct
{
  Dynarr_declare (struct face_cachel);
} face_cachel_dynarr;

#ifdef NEW_GC
DECLARE_LRECORD (face_cachel_dynarr, face_cachel_dynarr);
#define XFACE_CACHEL_DYNARR(x) \
  XRECORD (x, face_cachel_dynarr, face_cachel_dynarr)
#define wrap_face_cachel_dynarr(p) wrap_record (p, face_cachel_dynarr)
#define FACE_CACHEL_DYNARRP(x) RECORDP (x, face_cachel_dynarr)
#define CHECK_FACE_CACHEL_DYNARR(x) CHECK_RECORD (x, face_cachel_dynarr)
#define CONCHECK_FACE_CACHEL_DYNARR(x) CONCHECK_RECORD (x, face_cachel_dynarr)
#endif /* NEW_GC */

typedef struct
{
  Dynarr_declare (struct glyph_cachel);
} glyph_cachel_dynarr;

#ifdef NEW_GC
DECLARE_LRECORD (glyph_cachel_dynarr, glyph_cachel_dynarr);
#define XGLYPH_CACHEL_DYNARR(x) \
  XRECORD (x, glyph_cachel_dynarr, glyph_cachel_dynarr)
#define wrap_glyph_cachel_dynarr(p) wrap_record (p, glyph_cachel_dynarr)
#define GLYPH_CACHEL_DYNARRP(x) RECORDP (x, glyph_cachel_dynarr)
#define CHECK_GLYPH_CACHEL_DYNARR(x) CHECK_RECORD (x, glyph_cachel_dynarr)
#define CONCHECK_GLYPH_CACHEL_DYNARR(x) \
  CONCHECK_RECORD (x, glyph_cachel_dynarr)
#endif /* NEW_GC */

typedef struct
{
  Dynarr_declare (struct console_type_entry);
} console_type_entry_dynarr;

/* WARNING WARNING WARNING.  You must ensure on your own that proper
   GC protection is provided for the elements in this array. */
typedef struct
{
  Dynarr_declare (Lisp_Object);
} Lisp_Object_dynarr;

typedef struct
{
  Dynarr_declare (Lisp_Object *);
} Lisp_Object_ptr_dynarr;

/*------------------------------ unbound -------------------------------*/

/* Qunbound is a special Lisp_Object (actually of type
   symbol-value-forward), that can never be visible to
   the Lisp caller and thus can be used in the C code
   to mean "no such value". */

#define UNBOUNDP(val) EQ (val, Qunbound)

/* Evaluate expr, return it if it's not Qunbound. */
#define RETURN_IF_NOT_UNBOUND(expr) do	\
{					\
  Lisp_Object ret_nunb_val = (expr);	\
  if (!UNBOUNDP (ret_nunb_val))		\
    RETURN_SANS_WARNINGS ret_nunb_val;	\
} while (0)

/*------------------------------- cons ---------------------------------*/

/* In a cons, the markbit of the car is the gc mark bit */

struct Lisp_Cons
{
  struct lrecord_header lheader;
  Lisp_Object car_, cdr_;
};
typedef struct Lisp_Cons Lisp_Cons;

#if 0 /* FSFmacs */
/* Like a cons, but records info on where the text lives that it was read from */
/* This is not really in use now */

struct Lisp_Buffer_Cons
{
  Lisp_Object car, cdr;
  struct buffer *buffer;
  int charbpos;
};
#endif

DECLARE_MODULE_API_LRECORD (cons, Lisp_Cons);
#define XCONS(x) XRECORD (x, cons, Lisp_Cons)
#define wrap_cons(p) wrap_record (p, cons)
#define CONSP(x) RECORDP (x, cons)
#define CHECK_CONS(x) CHECK_RECORD (x, cons)
#define CONCHECK_CONS(x) CONCHECK_RECORD (x, cons)

#ifdef NEW_GC
#define CONS_MARKED_P(c) MARKED_P (&((c)->lheader))
#define MARK_CONS(c) MARK (&((c)->lheader))
#else /* not NEW_GC */
#define CONS_MARKED_P(c) MARKED_RECORD_HEADER_P(&((c)->lheader))
#define MARK_CONS(c) MARK_RECORD_HEADER (&((c)->lheader))
#endif /* not NEW_GC */

extern MODULE_API Lisp_Object Qnil;

#define NILP(x)  EQ (x, Qnil)
#define cons_car(a) ((a)->car_)
#define cons_cdr(a) ((a)->cdr_)
#define XCAR(a) (XCONS (a)->car_)
#define XCDR(a) (XCONS (a)->cdr_)
#define XCADR(a) (XCAR (XCDR (a)))
#define XCDDR(a) (XCDR (XCDR (a)))
#define XCADDR(a) (XCAR (XCDDR (a)))
#define XCDDDR(a) (XCDR (XCDDR (a)))
#define XCADDDR(a) (XCAR (XCDDDR (a)))
#define XCDDDDR(a) (XCDR (XCDDDR (a)))
#define XCADDDDR(a) (XCAR (XCDDDDR (a)))
#define XCDDDDDR(a) (XCDR (XCDDDDR (a)))
#define XCADDDDDR(a) (XCAR (XCDDDDDR (a)))
#define XCDDDDDDR(a) (XCDR (XCDDDDDR (a)))
#define X1ST(a) XCAR (a)
#define X2ND(a) XCADR (a)
#define X3RD(a) XCADDR (a)
#define X4TH(a) XCADDDR (a)
#define X5TH(a) XCADDDDR (a)
#define X6TH(a) XCADDDDDR (a)

#define XSETCAR(a, b) (XCONS (a)->car_ = (b))
#define XSETCDR(a, b) (XCONS (a)->cdr_ = (b))
#define LISTP(x) (CONSP(x) || NILP(x))

#define CHECK_LIST(x) do {			\
  if (!LISTP (x))				\
    dead_wrong_type_argument (Qlistp, x);	\
} while (0)

#define CONCHECK_LIST(x) do {			\
  if (!LISTP (x))				\
    x = wrong_type_argument (Qlistp, x);	\
} while (0)

/*---------------------- list traversal macros -------------------------*/

/* Note: These macros are for traversing through a list in some format,
   and executing code that you specify on each member of the list.

   There are two kinds of macros, those requiring surrounding braces, and
   those not requiring this.  Which type of macro will be indicated.
   The general format for using a brace-requiring macro is

   {
     LIST_LOOP_3 (elt, list, tail)
       execute_code_here;
   }

   or

   {
     LIST_LOOP_3 (elt, list, tail)
       {
         execute_code_here;
       }
   }

   You can put variable declarations between the brace and beginning of
   macro, but NOTHING ELSE.

   The brace-requiring macros typically declare themselves any arguments
   that are initialized and iterated by the macros.  If for some reason
   you need to declare these arguments yourself (e.g. to do something on
   them before the iteration starts, use the _NO_DECLARE versions of the
   macros.)
*/

/* There are two basic kinds of macros: those that handle "internal" lists
   that are known to be correctly structured (i.e. first element is a cons
   or nil, and the car of each cons is also a cons or nil, and there are
   no circularities), and those that handle "external" lists, where the
   list may have any sort of invalid formation.  This is reflected in
   the names: those with "EXTERNAL_" work with external lists, and those
   without this prefix work with internal lists.  The internal-list
   macros will hit an assertion failure if the structure is ill-formed;
   the external-list macros will signal an error in this case, either a
   malformed-list error or a circular-list error.
*/

/* LIST_LOOP is a simple, old-fashioned macro.  It doesn't require brace
   surrounding, and iterates through a list, which may or may not known to
   be syntactically correct.  It accepts two args, TAIL (set progressively
   to each cons starting with the first), and LIST, the list to iterate
   over.  TAIL needs to be defined by the caller.

   In each iteration, you can retrieve the current list item using XCAR
   (tail), or destructively modify the list using XSETCAR (tail,
   ...). */

#define LIST_LOOP(tail, list)		\
  for (tail = list;			\
       !NILP (tail);			\
       tail = XCDR (tail))

/* The following macros are the "core" macros for list traversal.

   *** ALL OF THESE MACROS MUST BE DECLARED INSIDE BRACES -- SEE ABOVE. ***

   LIST_LOOP_2 and EXTERNAL_LIST_LOOP_2 are the standard, most-often used
   macros.  They take two arguments, an element variable ELT and the list
   LIST.  ELT is automatically declared, and set to each element in turn
   from LIST.

   LIST_LOOP_3 and EXTERNAL_LIST_LOOP_3 are the same, but they have a third
   argument TAIL, another automatically-declared variable.  At each iteration,
   this one points to the cons cell for which ELT is the car.

   EXTERNAL_LIST_LOOP_4 is like EXTERNAL_LIST_LOOP_3 but takes an additional
   LEN argument, again automatically declared, which counts the number of
   iterations gone by.  It is 0 during the first iteration.

   EXTERNAL_LIST_LOOP_4_NO_DECLARE is like EXTERNAL_LIST_LOOP_4 but none
   of the variables are automatically declared, and so you need to declare
   them yourself. (ELT and TAIL are Lisp_Objects, and LEN is an EMACS_INT.)
*/

#define LIST_LOOP_2(elt, list)		\
  LIST_LOOP_3(elt, list, unused_tail_##elt)

#define LIST_LOOP_3(elt, list, tail)	\
  Lisp_Object elt, tail;		\
  for (tail = list;			\
       NILP (tail) ?			\
	 0 : (elt = XCAR (tail), 1);	\
       tail = XCDR (tail))

/* The following macros are for traversing lisp lists.
   Signal an error if LIST is not properly acyclic and nil-terminated.

   Use tortoise/hare algorithm to check for cycles, but only if it
   looks like the list is getting too long.  Not only is the hare
   faster than the tortoise; it even gets a head start! */

/* Optimized and safe macros for looping over external lists.  */
#define CIRCULAR_LIST_SUSPICION_LENGTH 1024

#define EXTERNAL_LIST_LOOP_1(list)					\
Lisp_Object ELL1_elt, ELL1_hare, ELL1_tortoise;				\
EMACS_INT ELL1_len;							\
PRIVATE_EXTERNAL_LIST_LOOP_6 (ELL1_elt, list, ELL1_len, ELL1_hare,	\
		      ELL1_tortoise, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_LIST_LOOP_2(elt, list)					\
Lisp_Object elt, hare_##elt, tortoise_##elt;				\
EMACS_INT len_##elt;							\
PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len_##elt, hare_##elt,		\
		      tortoise_##elt, CIRCULAR_LIST_SUSPICION_LENGTH)


#define GC_EXTERNAL_LIST_LOOP_2(elt, list)				\
do {									\
  XGCDECL3 (elt);							\
  Lisp_Object elt, hare_##elt, tortoise_##elt;				\
  EMACS_INT len_##elt;							\
  XGCPRO3 (elt, elt, hare_##elt, tortoise_##elt);			\
  PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len_##elt, hare_##elt,	\
				tortoise_##elt,				\
				CIRCULAR_LIST_SUSPICION_LENGTH)

#define END_GC_EXTERNAL_LIST_LOOP(elt)		\
  XUNGCPRO (elt);				\
}						\
while (0)

#define EXTERNAL_LIST_LOOP_3(elt, list, tail)				\
Lisp_Object elt, tail, tortoise_##elt;					\
EMACS_INT len_##elt;							\
PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len_##elt, tail,		\
		      tortoise_##elt, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_LIST_LOOP_4_NO_DECLARE(elt, list, tail, len)		\
Lisp_Object tortoise_##elt;						\
PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len, tail,			\
		      tortoise_##elt, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_LIST_LOOP_4(elt, list, tail, len)			\
Lisp_Object elt, tail, tortoise_##elt;					\
EMACS_INT len;								\
PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len, tail,			\
		      tortoise_##elt, CIRCULAR_LIST_SUSPICION_LENGTH)


#define PRIVATE_EXTERNAL_LIST_LOOP_6(elt, list, len, hare,		\
				     tortoise, suspicion_length)	\
  for (tortoise = hare = list, len = 0;					\
									\
       (CONSP (hare) ? ((elt = XCAR (hare)), 1) :			\
	(NILP (hare) ? 0 :						\
	 (signal_malformed_list_error (list), 0)));			\
									\
       hare = XCDR (hare),						\
	 (void)								\
	 ((++len > suspicion_length)					\
	  &&								\
	  ((((len & 1) != 0) && (tortoise = XCDR (tortoise), 0)),	\
	   (EQ (hare, tortoise) && (signal_circular_list_error (list), 0)))))

/* GET_LIST_LENGTH and GET_EXTERNAL_LIST_LENGTH:

   These two macros return the length of LIST (either an internal or external
   list, according to which macro is used), stored into LEN (which must
   be declared by the caller).  Circularities are trapped in external lists
   (and cause errors).  Neither macro need be declared inside brackets. */

#define GET_LIST_LENGTH(list, len) do {		\
  Lisp_Object GLL_tail;				\
  for (GLL_tail = list, len = 0;		\
       !NILP (GLL_tail);			\
       GLL_tail = XCDR (GLL_tail), ++len)	\
    DO_NOTHING;					\
} while (0)

#define GET_EXTERNAL_LIST_LENGTH(list, len)				\
do {									\
  Lisp_Object GELL_elt, GELL_tail;					\
  EXTERNAL_LIST_LOOP_4_NO_DECLARE (GELL_elt, list, GELL_tail, len)	\
    ;									\
} while (0)

/* For a list that's known to be in valid list format, where we may
   be deleting the current element out of the list --
   will ABORT() if the list is not in valid format */
#define LIST_LOOP_DELETING(consvar, nextconsvar, list)		\
  for (consvar = list;						\
       !NILP (consvar) ? (nextconsvar = XCDR (consvar), 1) :0;	\
       consvar = nextconsvar)

/* LIST_LOOP_DELETE_IF and EXTERNAL_LIST_LOOP_DELETE_IF:

   These two macros delete all elements of LIST (either an internal or
   external list, according to which macro is used) satisfying
   CONDITION, a C expression referring to variable ELT.  ELT is
   automatically declared.  Circularities are trapped in external
   lists (and cause errors).  Neither macro need be declared inside
   brackets. */

#define LIST_LOOP_DELETE_IF(elt, list, condition) do {		\
  /* Do not use ##list when creating new variables because	\
     that may not be just a variable name. */			\
  Lisp_Object prev_tail_##elt = Qnil;				\
  LIST_LOOP_3 (elt, list, tail_##elt)				\
    {								\
      if (condition)						\
	{							\
	  if (NILP (prev_tail_##elt))				\
	    list = XCDR (tail_##elt);				\
	  else							\
	    XCDR (prev_tail_##elt) = XCDR (tail_##elt);	\
	}							\
      else							\
	prev_tail_##elt = tail_##elt;				\
    }								\
} while (0)

#define EXTERNAL_LIST_LOOP_DELETE_IF(elt, list, condition) do {	\
  Lisp_Object prev_tail_##elt = Qnil;				\
  EXTERNAL_LIST_LOOP_4 (elt, list, tail_##elt, len_##elt)	\
    {								\
      if (condition)						\
	{							\
	  if (NILP (prev_tail_##elt))				\
	    list = XCDR (tail_##elt);				\
	  else							\
	    XCDR (prev_tail_##elt) = XCDR (tail_##elt);		\
          /* Keep tortoise from ever passing hare. */		\
	  len_##elt = 0;					\
	}							\
      else							\
	prev_tail_##elt = tail_##elt;				\
    }								\
} while (0)


/* Macros for looping over internal alists.

   *** ALL OF THESE MACROS MUST BE DECLARED INSIDE BRACES -- SEE ABOVE. ***

   ALIST_LOOP_3 loops over an alist, at each iteration setting CAR and CDR
   to the car and cdr of the acons.  CAR and CDR are automatically
   declared.

   ALIST_LOOP_4 is similar to ALIST_LOOP_3 but contains an additional
   variable ACONS at the beginning for access to the acons itself.All of
   the variables ACONS, CAR and CDR are automatically declared.
*/

#define ALIST_LOOP_3(car, cdr, alist)				\
Lisp_Object _al3_acons_##car, car, cdr, _al3_tail_##car;	\
  for (_al3_tail_##car = alist;					\
       NILP (_al3_tail_##car) ?					\
	 0 : (_al3_acons_##car = XCAR (_al3_tail_##car),	\
	      car = XCAR (_al3_acons_##car),			\
	      cdr = XCDR (_al3_acons_##car), 1);		\
       _al3_tail_##car = XCDR (_al3_tail_##car))

#define ALIST_LOOP_4(acons, car, cdr, list)			\
Lisp_Object acons, car, cdr, _al4_tail_##car;			\
  for (_al4_tail_##car = list;					\
       NILP (_al4_tail_##car) ?					\
	 0 : (elt = XCAR (_al4_tail_##car), car = XCAR (elt),	\
	      cdr = XCDR (elt), 1);				\
       _al4_tail_##car = XCDR (tail))

/* Macros for looping over external alists.

   *** ALL OF THESE MACROS MUST BE DECLARED INSIDE BRACES -- SEE ABOVE. ***

   EXTERNAL_ALIST_LOOP_4 is similar to EXTERNAL_LIST_LOOP_2, but it
   assumes the elements are aconses (the elements in an alist) and
   sets two additional argument variables ELT_CAR and ELT_CDR to the
   car and cdr of the acons.  All of the variables ELT, ELT_CAR and
   ELT_CDR are automatically declared.

   EXTERNAL_ALIST_LOOP_5 adds a TAIL argument to EXTERNAL_ALIST_LOOP_4,
   just like EXTERNAL_LIST_LOOP_3 does, and again TAIL is automatically
   declared.

   EXTERNAL_ALIST_LOOP_6 adds a LEN argument to EXTERNAL_ALIST_LOOP_5,
   just like EXTERNAL_LIST_LOOP_4 does, and again LEN is automatically
   declared.

   EXTERNAL_ALIST_LOOP_6_NO_DECLARE does not declare any of its arguments,
   just like EXTERNAL_LIST_LOOP_4_NO_DECLARE, and so these must be declared
   manually.
 */

/* Optimized and safe macros for looping over external alists. */
#define EXTERNAL_ALIST_LOOP_4(elt, elt_car, elt_cdr, list)	\
Lisp_Object elt, elt_car, elt_cdr;				\
Lisp_Object hare_##elt, tortoise_##elt;				\
EMACS_INT len_##elt;						\
PRIVATE_EXTERNAL_ALIST_LOOP_8 (elt, elt_car, elt_cdr, list,	\
		       len_##elt, hare_##elt, tortoise_##elt,	\
		       CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_ALIST_LOOP_5(elt, elt_car, elt_cdr, list, tail)	\
Lisp_Object elt, elt_car, elt_cdr, tail;				\
Lisp_Object tortoise_##elt;						\
EMACS_INT len_##elt;							\
PRIVATE_EXTERNAL_ALIST_LOOP_8 (elt, elt_car, elt_cdr, list,		\
		       len_##elt, tail, tortoise_##elt,			\
		       CIRCULAR_LIST_SUSPICION_LENGTH)			\

#define EXTERNAL_ALIST_LOOP_6(elt, elt_car, elt_cdr, list, tail, len)	\
Lisp_Object elt, elt_car, elt_cdr, tail;				\
EMACS_INT len;								\
Lisp_Object tortoise_##elt;						\
PRIVATE_EXTERNAL_ALIST_LOOP_8 (elt, elt_car, elt_cdr, list,		\
		       len, tail, tortoise_##elt,			\
		       CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_ALIST_LOOP_6_NO_DECLARE(elt, elt_car, elt_cdr, list,	\
					 tail, len)			\
Lisp_Object tortoise_##elt;						\
PRIVATE_EXTERNAL_ALIST_LOOP_8 (elt, elt_car, elt_cdr, list,		\
		       len, tail, tortoise_##elt,			\
		       CIRCULAR_LIST_SUSPICION_LENGTH)


#define PRIVATE_EXTERNAL_ALIST_LOOP_8(elt, elt_car, elt_cdr, list, len, \
				      hare, tortoise, suspicion_length)	\
PRIVATE_EXTERNAL_LIST_LOOP_6 (elt, list, len, hare, tortoise,		\
			      suspicion_length)				\
  if (CONSP (elt) ? (elt_car = XCAR (elt), elt_cdr = XCDR (elt), 0) :1)	\
    continue;								\
  else

/* Macros for looping over external property lists.

   *** ALL OF THESE MACROS MUST BE DECLARED INSIDE BRACES -- SEE ABOVE. ***

   EXTERNAL_PROPERTY_LIST_LOOP_3 maps over an external list assumed to
   be a property list, consisting of alternating pairs of keys
   (typically symbols or keywords) and values.  Each iteration
   processes one such pair out of LIST, assigning the two elements to
   KEY and VALUE respectively.  Malformed lists and circularities are
   trapped as usual, and in addition, property lists with an odd number
   of elements also signal an error.

   EXTERNAL_PROPERTY_LIST_LOOP_4 adds a TAIL argument to
   EXTERNAL_PROPERTY_LIST_LOOP_3, just like EXTERNAL_LIST_LOOP_3 does,
   and again TAIL is automatically declared.

   EXTERNAL_PROPERTY_LIST_LOOP_5 adds a LEN argument to
   EXTERNAL_PROPERTY_LIST_LOOP_4, just like EXTERNAL_LIST_LOOP_4 does,
   and again LEN is automatically declared.  Note that in this case,
   LEN counts the iterations, NOT the total number of list elements
   processed, which is 2 * LEN.

   EXTERNAL_PROPERTY_LIST_LOOP_5_NO_DECLARE does not declare any of its
   arguments, just like EXTERNAL_LIST_LOOP_4_NO_DECLARE, and so these
   must be declared manually.  */

/* Optimized and safe macros for looping over external property lists. */
#define EXTERNAL_PROPERTY_LIST_LOOP_3(key, value, list)			\
Lisp_Object key, value, hare_##key, tortoise_##key;			\
EMACS_INT len_##key;							\
EXTERNAL_PROPERTY_LIST_LOOP_7 (key, value, list, len_##key, hare_##key,	\
		     tortoise_##key, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_PROPERTY_LIST_LOOP_4(key, value, list, tail)		\
Lisp_Object key, value, tail, tortoise_##key;				\
EMACS_INT len_##key;							\
EXTERNAL_PROPERTY_LIST_LOOP_7 (key, value, list, len_##key, tail,	\
		     tortoise_##key, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_PROPERTY_LIST_LOOP_5(key, value, list, tail, len)	\
Lisp_Object key, value, tail, tortoise_##key;				\
EMACS_INT len;								\
EXTERNAL_PROPERTY_LIST_LOOP_7 (key, value, list, len, tail,		\
		     tortoise_##key, CIRCULAR_LIST_SUSPICION_LENGTH)

#define EXTERNAL_PROPERTY_LIST_LOOP_5_NO_DECLARE(key, value, list,	\
						 tail, len)		\
Lisp_Object tortoise_##key;						\
EXTERNAL_PROPERTY_LIST_LOOP_7 (key, value, list, len, tail,		\
		     tortoise_##key, CIRCULAR_LIST_SUSPICION_LENGTH)


#define EXTERNAL_PROPERTY_LIST_LOOP_7(key, value, list, len, hare,	\
                             tortoise, suspicion_length)		\
  for (tortoise = hare = list, len = 0;					\
									\
       ((CONSP (hare) &&						\
	 (key = XCAR (hare),						\
	  hare = XCDR (hare),						\
	  (CONSP (hare) ? 1 :						\
	   (signal_malformed_property_list_error (list), 0)))) ?	\
	(value = XCAR (hare), 1) :					\
	(NILP (hare) ? 0 :						\
	 (signal_malformed_property_list_error (list), 0)));		\
									\
       hare = XCDR (hare),						\
	 ((++len < suspicion_length) ?					\
	  ((void) 0) :							\
	  (((len & 1) ?							\
	    ((void) (tortoise = XCDR (XCDR (tortoise)))) :		\
	    ((void) 0))							\
	   ,								\
	   (EQ (hare, tortoise) ?					\
	    ((void) signal_circular_property_list_error (list)) :	\
	    ((void) 0)))))

#define PRIVATE_PROPERTY_LIST_LOOP_4(tail, key, value, plist)	\
  for (tail = plist;						\
       NILP (tail) ? 0 :					\
	 (key   = XCAR (tail), tail = XCDR (tail),		\
	  value = XCAR (tail), tail = XCDR (tail), 1);		\
       )

#define PROPERTY_LIST_LOOP_3(key, value, plist)			\
  Lisp_Object key, value, tail_##key;				\
  PRIVATE_PROPERTY_LIST_LOOP_4 (tail_##key, key, value, plist)

#define GC_PROPERTY_LIST_LOOP_3(key, value, plist)		\
do {								\
  XGCDECL3 (key);						\
  Lisp_Object key, value, tail_##key;				\
  XGCPRO3 (key, key, value, tail_##key);			\
  PRIVATE_PROPERTY_LIST_LOOP_4 (tail_##key, key, value, plist)

#define END_GC_PROPERTY_LIST_LOOP(key)		\
  XUNGCPRO (key);				\
}						\
while (0)

/* Return 1 if LIST is properly acyclic and nil-terminated, else 0. */
DECLARE_INLINE_HEADER (
int
TRUE_LIST_P (Lisp_Object object)
)
{
  Lisp_Object hare, tortoise;
  EMACS_INT len;

  for (hare = tortoise = object, len = 0;
       CONSP (hare);
       hare = XCDR (hare), len++)
    {
      if (len < CIRCULAR_LIST_SUSPICION_LENGTH)
	continue;

      if (len & 1)
	tortoise = XCDR (tortoise);
      else if (EQ (hare, tortoise))
	return 0;
    }

  return NILP (hare);
}

/* Signal an error if LIST is not properly acyclic and nil-terminated. */
#define CHECK_TRUE_LIST(list) do {			\
  Lisp_Object CTL_list = (list);			\
  Lisp_Object CTL_hare, CTL_tortoise;			\
  EMACS_INT CTL_len;					\
							\
  for (CTL_hare = CTL_tortoise = CTL_list, CTL_len = 0;	\
       CONSP (CTL_hare);				\
       CTL_hare = XCDR (CTL_hare), CTL_len++)		\
    {							\
      if (CTL_len < CIRCULAR_LIST_SUSPICION_LENGTH)	\
	continue;					\
							\
      if (CTL_len & 1)					\
	CTL_tortoise = XCDR (CTL_tortoise);		\
      else if (EQ (CTL_hare, CTL_tortoise))		\
	Fsignal (Qcircular_list, list1 (CTL_list));	\
    }							\
							\
  if (! NILP (CTL_hare))				\
    signal_malformed_list_error (CTL_list);		\
} while (0)

/*------------------------------ string --------------------------------*/

#ifdef NEW_GC
struct Lisp_String_Direct_Data
{
  struct lrecord_header header;
  Bytecount size;
  Ibyte data[1];
};
typedef struct Lisp_String_Direct_Data Lisp_String_Direct_Data;

DECLARE_MODULE_API_LRECORD (string_direct_data, Lisp_String_Direct_Data);
#define XSTRING_DIRECT_DATA(x) \
  XRECORD (x, string_direct_data, Lisp_String_Direct_Data)
#define wrap_string_direct_data(p) wrap_record (p, string_direct_data)
#define STRING_DIRECT_DATAP(x) RECORDP (x, string_direct_data)
#define CHECK_STRING_DIRECT_DATA(x) CHECK_RECORD (x, string_direct_data)
#define CONCHECK_STRING_DIRECT_DATA(x) CONCHECK_RECORD (x, string_direct_data)

#define XSTRING_DIRECT_DATA_SIZE(x) XSTRING_DIRECT_DATA (x)->size
#define XSTRING_DIRECT_DATA_DATA(x) XSTRING_DIRECT_DATA (x)->data


struct Lisp_String_Indirect_Data
{
  struct lrecord_header header;
  Bytecount size;
  Ibyte *data;
};
typedef struct Lisp_String_Indirect_Data Lisp_String_Indirect_Data;

DECLARE_MODULE_API_LRECORD (string_indirect_data, Lisp_String_Indirect_Data);
#define XSTRING_INDIRECT_DATA(x) \
  XRECORD (x, string_indirect_data, Lisp_String_Indirect_Data)
#define wrap_string_indirect_data(p) wrap_record (p, string_indirect_data)
#define STRING_INDIRECT_DATAP(x) RECORDP (x, string_indirect_data)
#define CHECK_STRING_INDIRECT_DATA(x) CHECK_RECORD (x, string_indirect_data)
#define CONCHECK_STRING_INDIRECT_DATA(x) \
  CONCHECK_RECORD (x, string_indirect_data)

#define XSTRING_INDIRECT_DATA_SIZE(x) XSTRING_INDIRECT_DATA (x)->size
#define XSTRING_INDIRECT_DATA_DATA(x) XSTRING_INDIRECT_DATA (x)->data


#define XSTRING_DATA_SIZE(s) ((s)->indirect)?		\
  XSTRING_INDIRECT_DATA_SIZE ((s)->data_object):	\
  XSTRING_DIRECT_DATA_SIZE ((s)->data_object)
#define XSTRING_DATA_DATA(s) ((s)->indirect)?		\
  XSTRING_INDIRECT_DATA_DATA ((s)->data_object):	\
  XSTRING_DIRECT_DATA_DATA ((s)->data_object)

#define XSET_STRING_DATA_SIZE(s, len)				\
  if ((s)->indirect)						\
    XSTRING_INDIRECT_DATA_SIZE ((s)->data_object) = len;	\
  else								\
    XSTRING_DIRECT_DATA_SIZE ((s)->data_object) = len
#define XSET_STRING_DATA_DATA(s, ptr)				\
  if ((s)->indirect)						\
    XSTRING_INDIRECT_DATA_DATA ((s)->data_object) = ptr;	\
  else								\
    XSTRING_DIRECT_DATA_DATA ((s)->data_object) = ptr
#endif /* NEW_GC */

struct Lisp_String
{
  union
    {
      struct lrecord_header lheader;
      struct
	{
	  /* WARNING: Everything before ascii_begin must agree exactly with
	     struct lrecord_header */
	  unsigned int type :8;
#ifdef NEW_GC
	  unsigned int lisp_readonly :1;
	  unsigned int free :1;
	  /* Number of chars at beginning of string that are one byte in length
	     (byte_ascii_p) */
	  unsigned int ascii_begin :22;
#else /* not NEW_GC */
	  unsigned int mark :1;
	  unsigned int c_readonly :1;
	  unsigned int lisp_readonly :1;
	  /* Number of chars at beginning of string that are one byte in length
	     (byte_ascii_p) */
	  unsigned int ascii_begin :21;
#endif /* not NEW_GC */
	} v;
    } u;
#ifdef NEW_GC
  int indirect;
  Lisp_Object data_object;
#else /* not NEW_GC */
  Bytecount size_;
  Ibyte *data_;
#endif /* not NEW_GC */
  Lisp_Object plist;
};
typedef struct Lisp_String Lisp_String;

#ifdef NEW_GC
#define MAX_STRING_ASCII_BEGIN ((1 << 22) - 1)
#else /* not NEW_GC */
#define MAX_STRING_ASCII_BEGIN ((1 << 21) - 1)
#endif /* not NEW_GC */

DECLARE_MODULE_API_LRECORD (string, Lisp_String);
#define XSTRING(x) XRECORD (x, string, Lisp_String)
#define wrap_string(p) wrap_record (p, string)
#define STRINGP(x) RECORDP (x, string)
#define CHECK_STRING(x) CHECK_RECORD (x, string)
#define CONCHECK_STRING(x) CONCHECK_RECORD (x, string)

/* Most basic macros for strings -- basically just accessing or setting
   fields -- are here.  Everything else is in text.h, since they depend on
   stuff there. */

/* Operations on Lisp_String *'s; only ones left */
#ifdef NEW_GC
#define set_lispstringp_direct(s) ((s)->indirect = 0)
#define set_lispstringp_indirect(s) ((s)->indirect = 1)
#define set_lispstringp_length(s, len) XSET_STRING_DATA_SIZE (s, len)
#define set_lispstringp_data(s, ptr) XSET_STRING_DATA_DATA (s, ptr)
#else /* not NEW_GC */
#define set_lispstringp_length(s, len) ((void) ((s)->size_ = (len)))
#define set_lispstringp_data(s, ptr) ((void) ((s)->data_ = (ptr)))
#endif /* not NEW_GC */

/* Operations on strings as Lisp_Objects.  Don't manipulate Lisp_String *'s
   in any new code. */
#ifdef NEW_GC
#define STRING_DATA_OBJECT(s) ((s)->data_object)
#define XSTRING_DATA_OBJECT(s) (STRING_DATA_OBJECT (XSTRING (s)))
#define XSTRING_LENGTH(s) (XSTRING_DATA_SIZE (XSTRING (s)))
#else /* not NEW_GC */
#define XSTRING_LENGTH(s) (XSTRING (s)->size_)
#endif /* not NEW_GC */
#define XSTRING_PLIST(s) (XSTRING (s)->plist)
#ifdef NEW_GC
#define XSTRING_DATA(s) (XSTRING_DATA_DATA (XSTRING (s)))
#else /* not NEW_GC */
#define XSTRING_DATA(s) (XSTRING (s)->data_ + 0)
#endif /* not NEW_GC */
#define XSTRING_ASCII_BEGIN(s) (XSTRING (s)->u.v.ascii_begin + 0)
#define XSET_STRING_LENGTH(s, ptr) set_lispstringp_length (XSTRING (s), ptr)
#define XSET_STRING_DATA(s, ptr) set_lispstringp_data (XSTRING (s), ptr)
/* WARNING: If you modify an existing string, you must call
   bump_string_modiff() afterwards. */
#define XSET_STRING_ASCII_BEGIN(s, val) \
  ((void) (XSTRING (s)->u.v.ascii_begin = (val)))
#define XSTRING_FORMAT(s) FORMAT_DEFAULT

/* Return the true aligned size of a struct whose last member is a
   variable-length array field.  (this is known as the "struct hack") */
/* Implementation: in practice, structtype and fieldtype usually have
   the same alignment, but we can't be sure.  We need to use
   ALIGN_SIZE to be absolutely sure of getting the correct alignment.
   To help the compiler's optimizer, we use a ternary expression that
   only a very stupid compiler would fail to correctly simplify. */
#define FLEXIBLE_ARRAY_STRUCT_SIZEOF(structtype,	\
				     fieldtype,		\
				     fieldname,		\
				     array_length)	\
(ALIGNOF (structtype) == ALIGNOF (fieldtype)		\
 ? (offsetof (structtype, fieldname) +			\
    (offsetof (structtype, fieldname[1]) -		\
     offsetof (structtype, fieldname[0])) *		\
    (array_length))					\
 : (ALIGN_FOR_TYPE					\
    ((offsetof (structtype, fieldname) +		\
      (offsetof (structtype, fieldname[1]) -		\
       offsetof (structtype, fieldname[0])) *		\
      (array_length)),					\
     structtype)))

/*------------------------------ vector --------------------------------*/

struct Lisp_Vector
{
  struct LCRECORD_HEADER header;
  long size;
  Lisp_Object contents[1];
};
typedef struct Lisp_Vector Lisp_Vector;

DECLARE_LRECORD (vector, Lisp_Vector);
#define XVECTOR(x) XRECORD (x, vector, Lisp_Vector)
#define wrap_vector(p) wrap_record (p, vector)
#define VECTORP(x) RECORDP (x, vector)
#define CHECK_VECTOR(x) CHECK_RECORD (x, vector)
#define CONCHECK_VECTOR(x) CONCHECK_RECORD (x, vector)

#define vector_length(v) ((v)->size)
#define XVECTOR_LENGTH(s) vector_length (XVECTOR (s))
#define vector_data(v) ((v)->contents)
#define XVECTOR_DATA(s) vector_data (XVECTOR (s))

/*---------------------------- bit vectors -----------------------------*/

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
  struct LCRECORD_HEADER lheader;
  Elemcount size;
  unsigned long bits[1];
};
typedef struct Lisp_Bit_Vector Lisp_Bit_Vector;

DECLARE_LRECORD (bit_vector, Lisp_Bit_Vector);
#define XBIT_VECTOR(x) XRECORD (x, bit_vector, Lisp_Bit_Vector)
#define wrap_bit_vector(p) wrap_record (p, bit_vector)
#define BIT_VECTORP(x) RECORDP (x, bit_vector)
#define CHECK_BIT_VECTOR(x) CHECK_RECORD (x, bit_vector)
#define CONCHECK_BIT_VECTOR(x) CONCHECK_RECORD (x, bit_vector)

#define BITP(x) (INTP (x) && (XINT (x) == 0 || XINT (x) == 1))

#define CHECK_BIT(x) do {		\
  if (!BITP (x))			\
    dead_wrong_type_argument (Qbitp, x);\
} while (0)

#define CONCHECK_BIT(x) do {		\
  if (!BITP (x))			\
    x = wrong_type_argument (Qbitp, x);	\
} while (0)

#define bit_vector_length(v) ((v)->size)

DECLARE_INLINE_HEADER (
int
bit_vector_bit (Lisp_Bit_Vector *v, Elemcount n)
)
{
  return ((v->bits[n >> LONGBITS_LOG2] >> (n & (LONGBITS_POWER_OF_2 - 1)))
	  & 1);
}

DECLARE_INLINE_HEADER (
void
set_bit_vector_bit (Lisp_Bit_Vector *v, Elemcount n, int value)
)
{
  if (value)
    v->bits[n >> LONGBITS_LOG2] |= (1UL << (n & (LONGBITS_POWER_OF_2 - 1)));
  else
    v->bits[n >> LONGBITS_LOG2] &= ~(1UL << (n & (LONGBITS_POWER_OF_2 - 1)));
}

/* Number of longs required to hold LEN bits */
#define BIT_VECTOR_LONG_STORAGE(len) \
  (((len) + LONGBITS_POWER_OF_2 - 1) >> LONGBITS_LOG2)

/* For when we want to include a bit vector in another structure, and we
   know it's of a fixed size. */
#define DECLARE_INLINE_LISP_BIT_VECTOR(numbits) struct {	\
  struct LCRECORD_HEADER lheader;				\
  Elemcount size;						\
  unsigned long bits[BIT_VECTOR_LONG_STORAGE(numbits)];		\
}

/*------------------------------ symbol --------------------------------*/

typedef struct Lisp_Symbol Lisp_Symbol;
struct Lisp_Symbol
{
  struct lrecord_header lheader;
  /* next symbol in this obarray bucket */
  Lisp_Symbol *next;
  Lisp_Object name;
  Lisp_Object value;
  Lisp_Object function;
  Lisp_Object plist;
};

#define SYMBOL_IS_KEYWORD(sym)						\
  ((string_byte (symbol_name (XSYMBOL (sym)), 0) == ':')		\
   && EQ (sym, oblookup (Vobarray,					\
			 XSTRING_DATA (symbol_name (XSYMBOL (sym))),	\
			 XSTRING_LENGTH (symbol_name (XSYMBOL (sym))))))
#define KEYWORDP(obj) (SYMBOLP (obj) && SYMBOL_IS_KEYWORD (obj))

DECLARE_MODULE_API_LRECORD (symbol, Lisp_Symbol);
#define XSYMBOL(x) XRECORD (x, symbol, Lisp_Symbol)
#define wrap_symbol(p) wrap_record (p, symbol)
#define SYMBOLP(x) RECORDP (x, symbol)
#define CHECK_SYMBOL(x) CHECK_RECORD (x, symbol)
#define CONCHECK_SYMBOL(x) CONCHECK_RECORD (x, symbol)

#define symbol_next(s) ((s)->next)
#define symbol_name(s) ((s)->name)
#define symbol_value(s) ((s)->value)
#define symbol_function(s) ((s)->function)
#define symbol_plist(s) ((s)->plist)

#define XSYMBOL_NEXT(s) (XSYMBOL (s)->next)
#define XSYMBOL_NAME(s) (XSYMBOL (s)->name)
#define XSYMBOL_VALUE(s) (XSYMBOL (s)->value)
#define XSYMBOL_FUNCTION(s) (XSYMBOL (s)->function)
#define XSYMBOL_PLIST(s) (XSYMBOL (s)->plist)


/*------------------------------- subr ---------------------------------*/

/* A function that takes no arguments and returns a Lisp_Object.
   We could define such types for n arguments, if needed. */
typedef Lisp_Object (*lisp_fn_t) (void);

struct Lisp_Subr
{
  struct lrecord_header lheader;
  short min_args;
  short max_args;
  /* #### We should make these const Ascbyte * or const Ibyte *, not const
     char *. */
  const char *prompt;
  const char *doc;
  const char *name;
  lisp_fn_t subr_fn;
};
typedef struct Lisp_Subr Lisp_Subr;

DECLARE_LRECORD (subr, Lisp_Subr);
#define XSUBR(x) XRECORD (x, subr, Lisp_Subr)
#define wrap_subr(p) wrap_record (p, subr)
#define SUBRP(x) RECORDP (x, subr)
#define CHECK_SUBR(x) CHECK_RECORD (x, subr)
#define CONCHECK_SUBR(x) CONCHECK_RECORD (x, subr)

#define subr_function(subr) ((subr)->subr_fn)
#define SUBR_FUNCTION(subr,max_args) \
  ((Lisp_Object (*) (EXFUN_##max_args)) (subr)->subr_fn)
#define subr_name(subr) ((subr)->name)

/*------------------------------ marker --------------------------------*/


typedef struct Lisp_Marker Lisp_Marker;
struct Lisp_Marker
{
  struct lrecord_header lheader;
  Lisp_Marker *next;
  Lisp_Marker *prev;
  struct buffer *buffer;
  Membpos membpos;
  char insertion_type;
};

DECLARE_MODULE_API_LRECORD (marker, Lisp_Marker);
#define XMARKER(x) XRECORD (x, marker, Lisp_Marker)
#define wrap_marker(p) wrap_record (p, marker)
#define MARKERP(x) RECORDP (x, marker)
#define CHECK_MARKER(x) CHECK_RECORD (x, marker)
#define CONCHECK_MARKER(x) CONCHECK_RECORD (x, marker)

/* The second check was looking for GCed markers still in use */
/* if (INTP (XMARKER (x)->lheader.next.v)) ABORT (); */

#define marker_next(m) ((m)->next)
#define marker_prev(m) ((m)->prev)

/*-------------------basic int (no connection to char)------------------*/

#define ZEROP(x) EQ (x, Qzero)

#ifdef ERROR_CHECK_TYPES

#define XINT(x) XINT_1 (x, __FILE__, __LINE__) 

DECLARE_INLINE_HEADER (
EMACS_INT
XINT_1 (Lisp_Object obj, const Ascbyte *file, int line)
)
{
  assert_at_line (INTP (obj), file, line);
  return XREALINT (obj);
}

#else /* no error checking */

#define XINT(obj) XREALINT (obj)

#endif /* no error checking */

#define CHECK_INT(x) do {			\
  if (!INTP (x))				\
    dead_wrong_type_argument (Qintegerp, x);	\
} while (0)

#define CONCHECK_INT(x) do {			\
  if (!INTP (x))				\
    x = wrong_type_argument (Qintegerp, x);	\
} while (0)

#define NATNUMP(x) (INTP (x) && XINT (x) >= 0)

#define CHECK_NATNUM(x) do {			\
  if (!NATNUMP (x))				\
    dead_wrong_type_argument (Qnatnump, x);	\
} while (0)

#define CONCHECK_NATNUM(x) do {			\
  if (!NATNUMP (x))				\
    x = wrong_type_argument (Qnatnump, x);	\
} while (0)

/*------------------------------- char ---------------------------------*/

/* NOTE: There are basic functions for converting between a character and
   the string representation of a character in text.h, as well as lots of
   other character-related stuff.  There are other functions/macros for
   working with Ichars in charset.h, for retrieving the charset of an
   Ichar, the length of an Ichar when converted to text, etc.
*/

#ifdef MULE

MODULE_API int non_ascii_valid_ichar_p (Ichar ch);

/* Return whether the given Ichar is valid.
 */

DECLARE_INLINE_HEADER (
int
valid_ichar_p (Ichar ch)
)
{
  return (! (ch & ~0xFF)) || non_ascii_valid_ichar_p (ch);
}

#else /* not MULE */

/* This works when CH is negative, and correctly returns non-zero only when CH
   is in the range [0, 255], inclusive. */
#define valid_ichar_p(ch) (! (ch & ~0xFF))

#endif /* not MULE */

#ifdef ERROR_CHECK_TYPES

DECLARE_INLINE_HEADER (
int
CHARP_1 (Lisp_Object obj, const Ascbyte *file, int line)
)
{
  if (XTYPE (obj) != Lisp_Type_Char)
    return 0;
  assert_at_line (valid_ichar_p (XCHARVAL (obj)), file, line);
  return 1;
}

#define CHARP(x) CHARP_1 (x, __FILE__, __LINE__) 

DECLARE_INLINE_HEADER (
Ichar
XCHAR_1 (Lisp_Object obj, const Ascbyte *file, int line)
)
{
  Ichar ch;
  assert_at_line (CHARP (obj), file, line);
  ch = XCHARVAL (obj);
  assert_at_line (valid_ichar_p (ch), file, line);
  return ch;
}

#define XCHAR(x) XCHAR_1 (x, __FILE__, __LINE__) 

#else /* not ERROR_CHECK_TYPES */

#define CHARP(x) (XTYPE (x) == Lisp_Type_Char)
#define XCHAR(x) ((Ichar) XCHARVAL (x))

#endif /* (else) not ERROR_CHECK_TYPES */

#define CONCHECK_CHAR(x) do {			\
 if (!CHARP (x))				\
   x = wrong_type_argument (Qcharacterp, x);	\
 } while (0)

#define CHECK_CHAR(x) do {			\
 if (!CHARP (x))				\
   dead_wrong_type_argument (Qcharacterp, x);	\
 } while (0)


DECLARE_INLINE_HEADER (
Lisp_Object
make_char (Ichar val)
)
{
  type_checking_assert (valid_ichar_p (val));
  /* This is defined in lisp-union.h or lisp-disunion.h */
  return make_char_1 (val);
}

/*------------------------- int-char connection ------------------------*/

#ifdef ERROR_CHECK_TYPES

#define XCHAR_OR_INT(x) XCHAR_OR_INT_1 (x, __FILE__, __LINE__) 

DECLARE_INLINE_HEADER (
EMACS_INT
XCHAR_OR_INT_1 (Lisp_Object obj, const Ascbyte *file, int line)
)
{
  assert_at_line (INTP (obj) || CHARP (obj), file, line);
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

#else /* no error checking */

/* obj is multiply eval'ed and not an lvalue; use an inline function instead
   of a macro. */
DECLARE_INLINE_HEADER (
EMACS_INT
XCHAR_OR_INT (Lisp_Object obj)
)
{
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

#endif /* no error checking */

/* True of X is an integer whose value is the valid integral equivalent of a
   character. */

#define CHAR_INTP(x) (INTP (x) && valid_ichar_p (XINT (x)))

/* True of X is a character or an integral value that can be converted into a
   character. */
#define CHAR_OR_CHAR_INTP(x) (CHARP (x) || CHAR_INTP (x))

DECLARE_INLINE_HEADER (
Ichar
XCHAR_OR_CHAR_INT (Lisp_Object obj)
)
{
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

/* Signal an error if CH is not a valid character or integer Lisp_Object.
   If CH is an integer Lisp_Object, convert it to a character Lisp_Object,
   but merely by repackaging, without performing tests for char validity.
   */

#define CHECK_CHAR_COERCE_INT(x) do {		\
  if (CHARP (x))				\
     ;						\
  else if (CHAR_INTP (x))			\
    x = make_char (XINT (x));			\
  else						\
    x = wrong_type_argument (Qcharacterp, x);	\
} while (0)

/* next three always continuable because they coerce their arguments. */
#define CHECK_INT_COERCE_CHAR(x) do {			\
  if (INTP (x))						\
    ;							\
  else if (CHARP (x))					\
    x = make_int (XCHAR (x));				\
  else							\
    x = wrong_type_argument (Qinteger_or_char_p, x);	\
} while (0)

#define CHECK_INT_COERCE_MARKER(x) do {			\
  if (INTP (x))						\
    ;							\
  else if (MARKERP (x))					\
    x = make_int (marker_position (x));			\
  else							\
    x = wrong_type_argument (Qinteger_or_marker_p, x);	\
} while (0)

#define CHECK_INT_COERCE_CHAR_OR_MARKER(x) do {			\
  if (INTP (x))							\
    ;								\
  else if (CHARP (x))						\
    x = make_int (XCHAR (x));					\
  else if (MARKERP (x))						\
    x = make_int (marker_position (x));				\
  else								\
    x = wrong_type_argument (Qinteger_char_or_marker_p, x);	\
} while (0)

/*------------------------------ float ---------------------------------*/

/* Note: the 'unused_next_' field exists only to ensure that the
   `next' pointer fits within the structure, for the purposes of the
   free list.  This makes a difference in the unlikely case of
   sizeof(double) being smaller than sizeof(void *). */

struct Lisp_Float
{
  struct lrecord_header lheader;
  union { double d; struct Lisp_Float *unused_next_; } data;
};
typedef struct Lisp_Float Lisp_Float;

DECLARE_LRECORD (float, Lisp_Float);
#define XFLOAT(x) XRECORD (x, float, Lisp_Float)
#define wrap_float(p) wrap_record (p, float)
#define FLOATP(x) RECORDP (x, float)
#define CHECK_FLOAT(x) CHECK_RECORD (x, float)
#define CONCHECK_FLOAT(x) CONCHECK_RECORD (x, float)

#define float_data(f) ((f)->data.d)
#define XFLOAT_DATA(x) float_data (XFLOAT (x))

#define XFLOATINT(n) extract_float (n)

#define CHECK_INT_OR_FLOAT(x) do {		\
  if (!INT_OR_FLOATP (x))			\
    dead_wrong_type_argument (Qnumberp, x);	\
} while (0)

#define CONCHECK_INT_OR_FLOAT(x) do {		\
  if (!INT_OR_FLOATP (x))			\
    x = wrong_type_argument (Qnumberp, x);	\
} while (0)

# define INT_OR_FLOATP(x) (INTP (x) || FLOATP (x))

/*--------------------------- readonly objects -------------------------*/

#ifndef NEW_GC
#define CHECK_C_WRITEABLE(obj)					\
  do { if (c_readonly (obj)) c_write_error (obj); } while (0)

#define C_READONLY(obj) (C_READONLY_RECORD_HEADER_P(XRECORD_LHEADER (obj)))
#endif /* not NEW_GC */

#define CHECK_LISP_WRITEABLE(obj)					\
  do { if (lisp_readonly (obj)) lisp_write_error (obj); } while (0)

#define LISP_READONLY(obj) (LISP_READONLY_RECORD_HEADER_P(XRECORD_LHEADER (obj)))

/*----------------------------- structures ----------------------------*/

typedef struct structure_keyword_entry structure_keyword_entry;
struct structure_keyword_entry
{
  Lisp_Object keyword;
  int (*validate) (Lisp_Object keyword, Lisp_Object value,
		   Error_Behavior errb);
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
  int (*validate) (Lisp_Object data, Error_Behavior errb);
  Lisp_Object (*instantiate) (Lisp_Object data);
};

typedef struct
{
  Dynarr_declare (structure_type);
} structure_type_dynarr;

struct structure_type *define_structure_type (Lisp_Object type,
					      int (*validate)
					      (Lisp_Object data,
					       Error_Behavior errb),
					      Lisp_Object (*instantiate)
					      (Lisp_Object data));
void define_structure_type_keyword (struct structure_type *st,
				    Lisp_Object keyword,
				    int (*validate) (Lisp_Object keyword,
						     Lisp_Object value,
						     Error_Behavior errb));

/*---------------------------- weak boxes ------------------------------*/

struct weak_box
{
  struct LCRECORD_HEADER header;
  Lisp_Object value;

  Lisp_Object next_weak_box; /* don't mark through this! */
};

void prune_weak_boxes (void);
Lisp_Object make_weak_box (Lisp_Object value);
Lisp_Object weak_box_ref (Lisp_Object value);

DECLARE_LRECORD (weak_box, struct weak_box);
#define XWEAK_BOX(x) XRECORD (x, weak_box, struct weak_box)
#define XSET_WEAK_BOX(x, v) (XWEAK_BOX (x)->value = (v))
#define wrap_weak_box(p) wrap_record (p, weak_box)
#define WEAK_BOXP(x) RECORDP (x, weak_box)
#define CHECK_WEAK_BOX(x) CHECK_RECORD (x, weak_box)
#define CONCHECK_WEAK_BOX(x) CONCHECK_RECORD (x, weak_box)

/*--------------------------- ephemerons ----------------------------*/

struct ephemeron 
{
  struct LCRECORD_HEADER header;

  Lisp_Object key;

  /* This field holds a pair.  The cdr of this cons points to the next
     ephemeron in Vall_ephemerons.  The car points to another pair
     whose car is the value and whose cdr is the finalizer.

     This representation makes it very easy to unlink an ephemeron
     from Vall_ephemerons and chain it into
     Vall_ephemerons_to_finalize. */

  Lisp_Object cons_chain;

  Lisp_Object value;
};

void prune_ephemerons (void);
Lisp_Object ephemeron_value(Lisp_Object ephi);
void init_marking_ephemerons(void);
int continue_marking_ephemerons(void);
int finish_marking_ephemerons(void);
Lisp_Object zap_finalize_list(void);
Lisp_Object make_ephemeron(Lisp_Object key, Lisp_Object value, Lisp_Object finalizer);

DECLARE_LRECORD(ephemeron, struct ephemeron);
#define XEPHEMERON(x) XRECORD (x, ephemeron, struct ephemeron)
#define XEPHEMERON_REF(x) (XEPHEMERON (x)->value)
#define XEPHEMERON_NEXT(x) (XCDR (XEPHEMERON(x)->cons_chain))
#define XEPHEMERON_FINALIZER(x) (XCDR (XCAR (XEPHEMERON (x)->cons_chain)))
#define XSET_EPHEMERON_NEXT(x, n) (XSETCDR (XEPHEMERON(x)->cons_chain, n))
#define XSET_EPHEMERON_VALUE(x, v) (XEPHEMERON(x)->value = (v))
#define XSET_EPHEMERON_KEY(x, k) (XEPHEMERON(x)->key = (k))
#define wrap_ephemeron(p) wrap_record (p, ephemeron)
#define EPHEMERONP(x) RECORDP (x, ephemeron)
#define CHECK_EPHEMERON(x) CHECK_RECORD (x, ephemeron)
#define CONCHECK_EPHEMERON(x) CONCHECK_RECORD (x, ephemeron)


/*---------------------------- weak lists ------------------------------*/

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
  WEAK_LIST_VALUE_ASSOC,
  /* element disappears if it's a cons and neither its car nor
     its cdr is marked. */
  WEAK_LIST_FULL_ASSOC
};

struct weak_list
{
  struct LCRECORD_HEADER header;
  Lisp_Object list; /* don't mark through this! */
  enum weak_list_type type;
  Lisp_Object next_weak; /* don't mark through this! */
};

DECLARE_LRECORD (weak_list, struct weak_list);
#define XWEAK_LIST(x) XRECORD (x, weak_list, struct weak_list)
#define wrap_weak_list(p) wrap_record (p, weak_list)
#define WEAK_LISTP(x) RECORDP (x, weak_list)
#define CHECK_WEAK_LIST(x) CHECK_RECORD (x, weak_list)
#define CONCHECK_WEAK_LIST(x) CONCHECK_RECORD (x, weak_list)

#define weak_list_list(w) ((w)->list)
#define XWEAK_LIST_LIST(w) (XWEAK_LIST (w)->list)

Lisp_Object make_weak_list (enum weak_list_type type);
/* The following two are only called by the garbage collector */
int finish_marking_weak_lists (void);
void prune_weak_lists (void);

END_C_DECLS

/************************************************************************/
/*      Definitions related to the format of text and of characters     */
/************************************************************************/

/* Note:

   "internally formatted text" and the term "internal format" in
   general are likely to refer to the format of text in buffers and
   strings; "externally formatted text" and the term "external format"
   refer to any text format used in the O.S. or elsewhere outside of
   XEmacs.  The format of text and of a character are related and
   there must be a one-to-one relationship (hopefully through a
   relatively simple algorithmic means of conversion) between a string
   of text and an equivalent array of characters, but the conversion
   between the two is NOT necessarily trivial.

   In a non-Mule XEmacs, allowed characters are numbered 0 through
   255, where no fixed meaning is assigned to them, but (when
   representing text, rather than bytes in a binary file) in practice
   the lower half represents ASCII and the upper half some other 8-bit
   character set (chosen by setting the font, case tables, syntax
   tables, etc. appropriately for the character set through ad-hoc
   means such as the `iso-8859-1' file and the
   `standard-display-european' function).

   #### Finish this.

	*/
#include "text.h"


/************************************************************************/
/*	   Definitions of primitive Lisp functions and variables	*/
/************************************************************************/


/* DEFUN - Define a built-in Lisp-visible C function or `subr'.
 `lname' should be the name to give the function in Lisp,
    as a null-terminated C string.
 `Fname' should be the C equivalent of `lname', using only characters
    valid in a C identifier, with an "F" prepended.
    The name of the C constant structure that records information
    on this function for internal use is "S" concatenated with Fname.
 `min_args' should be a number, the minimum number of arguments allowed.
 `max_args' should be a number, the maximum number of arguments allowed,
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
    comment and creates the DOC file from it.
*/

#define EXFUN_0 void
#define EXFUN_1 Lisp_Object
#define EXFUN_2 Lisp_Object,Lisp_Object
#define EXFUN_3 Lisp_Object,Lisp_Object,Lisp_Object
#define EXFUN_4 Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object
#define EXFUN_5 Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object
#define EXFUN_6 Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object, \
Lisp_Object
#define EXFUN_7 Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object, \
Lisp_Object,Lisp_Object
#define EXFUN_8 Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object,Lisp_Object, \
Lisp_Object,Lisp_Object,Lisp_Object
#define EXFUN_MANY int, Lisp_Object*
#define EXFUN_UNEVALLED Lisp_Object
#define EXFUN(sym, max_args) Lisp_Object sym (EXFUN_##max_args)
#define EXFUN_NORETURN(sym, max_args) \
  DECLARE_DOESNT_RETURN_TYPE (Lisp_Object, sym (EXFUN_##max_args))

#define SUBR_MAX_ARGS 8
#define MANY -2
#define UNEVALLED -1

/* Can't be const, because then subr->doc is read-only and
   Snarf_documentation chokes */

#ifdef NEW_GC
#define DEFUN(lname, Fname, min_args, max_args, prompt, arglist)	\
  Lisp_Object Fname (EXFUN_##max_args);					\
  static struct Lisp_Subr MC_ALLOC_S##Fname =			        \
  {									\
    { /* struct lrecord_header */					\
      lrecord_type_subr, /* lrecord_type_index */			\
      1, /* lisp_readonly bit */					\
      0, /* free */							\
      0  /* uid */							\
    },									\
    min_args,								\
    max_args,								\
    prompt,								\
    0,	/* doc string */						\
    lname,								\
    (lisp_fn_t) Fname							\
  };									\
  static struct Lisp_Subr *S##Fname;					\
  Lisp_Object Fname (DEFUN_##max_args arglist)

#define DEFUN_NORETURN(lname, Fname, min_args, max_args, prompt, arglist) \
  DECLARE_DOESNT_RETURN_TYPE (Lisp_Object, Fname (EXFUN_##max_args));	  \
  static struct Lisp_Subr MC_ALLOC_S##Fname =				  \
  {									  \
    { /* struct lrecord_header */					  \
      lrecord_type_subr, /* lrecord_type_index */			  \
      1, /* lisp_readonly bit */					  \
      0, /* free */							  \
      0  /* uid */							  \
    },									  \
    min_args,								  \
    max_args,								  \
    prompt,								  \
    0,	/* doc string */						  \
    lname,								  \
    (lisp_fn_t) Fname							  \
  };									  \
  static struct Lisp_Subr *S##Fname;					  \
  DOESNT_RETURN_TYPE (Lisp_Object) Fname (DEFUN_##max_args arglist)
#define GET_DEFUN_LISP_OBJECT(Fname) \
  wrap_subr (S##Fname);
#else /* not NEW_GC */
#define DEFUN(lname, Fname, min_args, max_args, prompt, arglist)	\
  Lisp_Object Fname (EXFUN_##max_args);					\
  static struct Lisp_Subr S##Fname =					\
  {									\
    { /* struct lrecord_header */					\
      lrecord_type_subr, /* lrecord_type_index */			\
      1, /* mark bit */							\
      1, /* c_readonly bit */						\
      1, /* lisp_readonly bit */					\
      0  /* unused */                                                   \
    },									\
    min_args,								\
    max_args,								\
    prompt,								\
    0,	/* doc string */						\
    lname,								\
    (lisp_fn_t) Fname							\
  };									\
  Lisp_Object Fname (DEFUN_##max_args arglist)

#define DEFUN_NORETURN(lname, Fname, min_args, max_args, prompt, arglist) \
  DECLARE_DOESNT_RETURN_TYPE (Lisp_Object, Fname (EXFUN_##max_args));	\
  static struct Lisp_Subr S##Fname =					\
  {									\
    { /* struct lrecord_header */					\
      lrecord_type_subr, /* lrecord_type_index */			\
      1, /* mark bit */							\
      1, /* c_readonly bit */						\
      1, /* lisp_readonly bit */					\
      0  /* unused */                                                   \
    },									\
    min_args,								\
    max_args,								\
    prompt,								\
    0,	/* doc string */						\
    lname,								\
    (lisp_fn_t) Fname							\
  };									\
  DOESNT_RETURN_TYPE (Lisp_Object) Fname (DEFUN_##max_args arglist)
#define GET_DEFUN_LISP_OBJECT(Fname) \
  wrap_subr (&S##Fname);
#endif /* not NEW_GC */

/* Heavy ANSI C preprocessor hackery to get DEFUN to declare a
   prototype that matches max_args, and add the obligatory
   `Lisp_Object' type declaration to the formal C arguments.  */

#define DEFUN_MANY(named_int, named_Lisp_Object) named_int, named_Lisp_Object
#define DEFUN_UNEVALLED(args) Lisp_Object args
#define DEFUN_0() void
#define DEFUN_1(a)					Lisp_Object a
#define DEFUN_2(a,b)		 DEFUN_1(a),		Lisp_Object b
#define DEFUN_3(a,b,c)		 DEFUN_2(a,b),		Lisp_Object c
#define DEFUN_4(a,b,c,d)	 DEFUN_3(a,b,c),	Lisp_Object d
#define DEFUN_5(a,b,c,d,e)	 DEFUN_4(a,b,c,d),	Lisp_Object e
#define DEFUN_6(a,b,c,d,e,f)	 DEFUN_5(a,b,c,d,e),	Lisp_Object f
#define DEFUN_7(a,b,c,d,e,f,g)	 DEFUN_6(a,b,c,d,e,f),	Lisp_Object g
#define DEFUN_8(a,b,c,d,e,f,g,h) DEFUN_7(a,b,c,d,e,f,g),Lisp_Object h

/* WARNING: If you add defines here for higher values of max_args,
   make sure to also fix the clauses in PRIMITIVE_FUNCALL(),
   and change the define of SUBR_MAX_ARGS above.  */

#include "symeval.h"

BEGIN_C_DECLS

/* `specpdl' is the special binding/unwind-protect stack.

   Knuth says (see the Jargon File):
   At MIT, `pdl' [abbreviation for `Push Down List'] used to
   be a more common synonym for `stack'.
   Everywhere else `stack' seems to be the preferred term.

   specpdl_depth is the current depth of `specpdl'.
   Save this for use later as arg to `unbind_to_1'.  */
extern MODULE_API int specpdl_depth_counter;
#define specpdl_depth() specpdl_depth_counter


#define CHECK_FUNCTION(fun) do {		\
 while (NILP (Ffunctionp (fun)))		\
   signal_invalid_function_error (fun);		\
 } while (0)


/************************************************************************/
/*			   Checking for QUIT				*/
/************************************************************************/

/* NOTE NOTE NOTE: Invoking QUIT can cause random Lisp code to be executed!
   This can happen in numerous ways.  For example, on many platforms, QUIT
   needs to drain the event queue to see whether there's a C-g in the works.
   A side effect of this is that, if there's a menu-press event, menu filters
   (i.e. Lisp code) will be invoked.  Lisp code could also happen if there's
   an asynchronous timeout, or if the debugger is invoked as a result of
   debug-on-quit and the user returns by hitting `r', etc. etc.

   However, GC CANNOT HAPPEN.  It is forbidden everywhere within the QUIT-
   processing code, because most callers cannot tolerate GC during QUIT
   since it's just too prevalent. */

/* The exact workings of this mechanism are described in detail in signal.c. */

/* Asynchronous events set something_happened, and then are processed
   within the QUIT macro.  At this point, we are guaranteed to not be in
   any sensitive code. */

extern MODULE_API volatile int something_happened;
extern MODULE_API int dont_check_for_quit;
MODULE_API void check_what_happened (void);

extern MODULE_API volatile int quit_check_signal_happened;
extern volatile int quit_check_signal_tick_count;
MODULE_API void check_quit (void);

MODULE_API void signal_quit (void);

int begin_dont_check_for_quit (void);
int begin_do_check_for_quit (void);

/* Nonzero if the values of `quit-flag' and `inhibit-quit' indicate
   that a quit should be signalled. */
#define QUIT_FLAG_SAYS_SHOULD_QUIT				\
  (!NILP (Vquit_flag) &&					\
   (NILP (Vinhibit_quit)					\
    || (EQ (Vquit_flag, Qcritical) && !dont_check_for_quit)))

/* Nonzero if ought to quit now.  This is the "efficient" version, which
   respects the flags set to indicate whether the full quit check should
   be done.  Therefore it may be inaccurate (i.e. lagging reality), esp.
   when poll for quit is used.

   This is defined for code that wants to allow quitting, but needs to
   do some cleanup if that happens. (You could always register the cleanup
   code using record_unwind_protect(), but sometimes it makes more sense
   to do it using QUITP.) To use this macro, just call it at the
   appropriate time, and if its value is non-zero, do your cleanup code
   and then call QUIT.

   A different version (below) is used for the actual QUIT macro.  */
#define QUITP							\
  ((quit_check_signal_happened ? check_quit () : (void) 0),	\
   QUIT_FLAG_SAYS_SHOULD_QUIT)

/* This is the version actually called by QUIT.  The difference
   between it and QUITP is that it also has side effects in that it
   will handle anything else that has recently signalled itself
   asynchronously and wants to be handled now.  Currently this
   includes executing asynchronous timeouts that may have been set
   from Lisp or from the poll-for-quit or poll-for-sigchld
   timers. (#### It seems that, to be slightly more accurate, we
   should also process poll-for-quit timers in the above version.
   However, this mechanism is inherently approximate, so it really
   doesn't matter much.) In the future, it might also include doing a
   thread context switch.  Callers of QUITP generally don't except
   random side effects to happen (#### unfortunately, random side effects
   can happen anyway, e.g. through menu filters -- see comment above),
   so we have this different version. */
#define INTERNAL_QUITP						\
  ((something_happened ? check_what_happened () : (void) 0),	\
   QUIT_FLAG_SAYS_SHOULD_QUIT)

/* Check quit-flag and quit if it is non-nil.  Also do any other things
   that are triggered by asynchronous events and might want to be
   handled. */
#define QUIT do { if (INTERNAL_QUITP) signal_quit (); } while (0)


/************************************************************************/
/*				 hashing				*/
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

#define LISP_HASH(obj) ((unsigned long) LISP_TO_VOID (obj))
Hashcode memory_hash (const void *xv, Bytecount size);
Hashcode internal_hash (Lisp_Object obj, int depth);
Hashcode internal_array_hash (Lisp_Object *arr, int size, int depth);


/************************************************************************/
/*			 String translation				*/
/************************************************************************/

/* When support for message translation exists, GETTEXT() translates a
   string from English into the language defined by
   `current-language-environment'.  This is done by looking the string
   up in a large predefined table; if no translation is found, the
   original string is returned, and the failure is possibly logged so
   that the translation can later be entered into the table.

   In addition to this, there is a mechanism to snarf message strings
   out of the source code so that they can be entered into the tables.
   This is what make-msgfile.lex does.

   Handling `format' strings is more difficult: The format string
   should get translated, but not under all circumstances.  When the
   format string is a Lisp string, what should happen is that
   Fformat() should format the untranslated args[0] and return that,
   and also call Fgettext() on args[0] and, if that is different,
   format it and store it in the `string-translatable' property of the
   returned string.  See Fgettext().

   CGETTEXT() is the same as GETTEXT() but works with char * strings
   instead of Ibyte * strings.

   build_msg_string() is a shorthand for build_string (GETTEXT (x)).
   build_msg_intstring() is a shorthand for build_intstring (GETTEXT (x)).
   */

#define GETTEXT(x) (x)
#define CGETTEXT(x) (x)
#define LISP_GETTEXT(x) (x)

/* DEFER_GETTEXT is used to identify strings which are translated when
   they are referenced instead of when they are defined.
   These include Qerror_messages and initialized arrays of strings.
*/
#define DEFER_GETTEXT(x) (x)


/************************************************************************/
/*		     Garbage collection / GC-protection			*/
/************************************************************************/

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

extern MODULE_API struct gcpro *gcprolist;

END_C_DECLS

/* #### Catching insufficient gcpro:

   The C++ code below catches GCPRO without UNGCPRO or vice-versa.
   Catching cases where there's no GCPRO or UNGCPRO but should be, however,
   is much harder, but could be done:

   1. Lisp_Object becomes a real object.  Its creator and destructor need to
      figure out whether the object is on the stack (by looking at the range
      that `this' is within), and if so, add the pointer to a list of all
      stack-based Lisp_Objects.

   2. The assignment method needs to do reference-counting on actual Lisp
      objects -- in particular, we need to know if there are any references
      to a Lisp object that are *NOT* from stack-based Lisp_Objects.

   3. When we get to a point in the code where we might garbage collect --
      i.e. Ffuncall(), Feval(), or Fgarbage_collect() is called -- we look
      at our list of stack-based Lisp_Objects, and if there are any that
      point to Lisp objects with no non-stack references, see if there are
      any gcpros pointing to the object, and if not, set a flag indicating
      that the object is "destroyed". (Don't abort yet because the function
      might not use the object any more.)

   4. When we detag a pointer using XFOO(), abort if its "destroyed" flag
      is set.

   --ben
*/

struct gcpro
{
  struct gcpro *next;
  const Lisp_Object *var;	/* Address of first protected variable */
  int nvars;			/* Number of consecutive protected variables */
#if defined (__cplusplus) && defined (ERROR_CHECK_GC)
  /* Try to catch GCPRO without UNGCPRO, or vice-versa.  G++ complains (at
     least with sufficient numbers of warnings enabled, i.e. -Weffc++) if a
     copy constructor or assignment operator is not defined. */
  gcpro () : next (0), var (0), nvars (0) { }
  gcpro (const gcpro& g) : next (g.next), var (g.var), nvars (g.nvars) { }
  gcpro& operator= (const gcpro& g) { next = g.next; var = g.var;
				      nvars = g.nvars;
				      return *this;}
  ~gcpro () { assert (!next); }
#endif /* defined (__cplusplus) && defined (ERROR_CHECK_GC) */
};

/* Normally, you declare variables gcpro1, gcpro2, ... and use the
   GCPROn() macros.  However, if you need to have nested gcpro's,
   declare ngcpro1, ngcpro2, ... and use NGCPROn().  If you need
   to nest another level, use nngcpro1, nngcpro2, ... and use
   NNGCPROn().  If you need to nest yet another level, create
   the appropriate macros. */

/* NOTE: About comments like "This function does not GC": These are there to
   try to track whether GCPROing is necessary.  Strictly speaking, some
   functions that say this might actually GC, but only when it is never
   possible to return (more specifically, in the process of signalling an
   error, the debugger may be invoked, and could GC).  For GCPRO purposes,
   you only have to worry about functions that can GC and then return.
   The QUIT macro cannot GC any more, although this wasn't true at some point,
   and so some "This function can GC" comments may be inaccurate.
*/

BEGIN_C_DECLS

#define XGCDECL1(x) struct gcpro x##cpro1
#define XGCDECL2(x) struct gcpro x##cpro1, x##cpro2
#define XGCDECL3(x) struct gcpro x##cpro1, x##cpro2, x##cpro3
#define XGCDECL4(x) struct gcpro x##cpro1, x##cpro2, x##cpro3, x##cpro4
#define XGCDECL5(x) struct gcpro x##cpro1, x##cpro2, x##cpro3, x##cpro4, x##cpro5

#ifdef DEBUG_GCPRO

MODULE_API void debug_gcpro1 (Ascbyte *, int, struct gcpro *, Lisp_Object *);
MODULE_API void debug_gcpro2 (Ascbyte *, int, struct gcpro *, struct gcpro *,
			      Lisp_Object *, Lisp_Object *);
MODULE_API void debug_gcpro3 (Ascbyte *, int, struct gcpro *, struct gcpro *,
			      struct gcpro *, Lisp_Object *, Lisp_Object *,
			      Lisp_Object *);
MODULE_API void debug_gcpro4 (Ascbyte *, int, struct gcpro *, struct gcpro *,
			      struct gcpro *, struct gcpro *, Lisp_Object *,
			      Lisp_Object *, Lisp_Object *, Lisp_Object *);
MODULE_API void debug_gcpro5 (Ascbyte *, int, struct gcpro *, struct gcpro *,
			      struct gcpro *, struct gcpro *, struct gcpro *,
			      Lisp_Object *, Lisp_Object *, Lisp_Object *,
			      Lisp_Object *, Lisp_Object *);
MODULE_API void debug_ungcpro(Ascbyte *, int, struct gcpro *);

#define XGCPRO1(x,v)							\
 debug_gcpro1 (__FILE__, __LINE__,&x##cpro1,&v)
#define XGCPRO2(x,v1,v2)						\
 debug_gcpro2 (__FILE__, __LINE__,&x##cpro1,&x##cpro2,&v1,&v2)
#define XGCPRO3(x,v1,v2,v3)						\
 debug_gcpro3 (__FILE__, __LINE__,&x##cpro1,&x##cpro2,&x##cpro3,	\
	       &v1,&v2,&v3)
#define XGCPRO4(x,v1,v2,v3,v4)						\
 debug_gcpro4 (__FILE__, __LINE__,&x##cpro1,&x##cpro2,&x##cpro3,	\
	       &x##cpro4,						\
	       &v1,&v2,&v3,&v4)
#define XGCPRO5(x,v1,v2,v3,v4,v5)					\
 debug_gcpro5 (__FILE__, __LINE__,&x##cpro1,&x##cpro2,&x##cpro3,	\
	       &x##cpro4,&x##cpro5,					\
	       &v1,&v2,&v3,&v4,&v5)
#define XUNGCPRO(x) \
 debug_ungcpro(__FILE__, __LINE__,&x##cpro1)

#else /* ! DEBUG_GCPRO */

#define XGCPRO1(x, var1) ((void) (					\
  x##cpro1.next = gcprolist, x##cpro1.var = &var1, x##cpro1.nvars = 1,	\
  gcprolist = &x##cpro1 ))

#define XGCPRO2(x, var1, var2) ((void) (				\
  x##cpro1.next = gcprolist, x##cpro1.var = &var1, x##cpro1.nvars = 1,	\
  x##cpro2.next = &x##cpro1, x##cpro2.var = &var2, x##cpro2.nvars = 1,	\
  gcprolist = &x##cpro2 ))

#define XGCPRO3(x, var1, var2, var3) ((void) (				\
  x##cpro1.next = gcprolist, x##cpro1.var = &var1, x##cpro1.nvars = 1,	\
  x##cpro2.next = &x##cpro1, x##cpro2.var = &var2, x##cpro2.nvars = 1,	\
  x##cpro3.next = &x##cpro2, x##cpro3.var = &var3, x##cpro3.nvars = 1,	\
  gcprolist = &x##cpro3 ))

#define XGCPRO4(x, var1, var2, var3, var4) ((void) (			\
  x##cpro1.next = gcprolist, x##cpro1.var = &var1, x##cpro1.nvars = 1,	\
  x##cpro2.next = &x##cpro1, x##cpro2.var = &var2, x##cpro2.nvars = 1,	\
  x##cpro3.next = &x##cpro2, x##cpro3.var = &var3, x##cpro3.nvars = 1,	\
  x##cpro4.next = &x##cpro3, x##cpro4.var = &var4, x##cpro4.nvars = 1,	\
  gcprolist = &x##cpro4 ))

#define XGCPRO5(x, var1, var2, var3, var4, var5) ((void) (		\
  x##cpro1.next = gcprolist, x##cpro1.var = &var1, x##cpro1.nvars = 1,	\
  x##cpro2.next = &x##cpro1, x##cpro2.var = &var2, x##cpro2.nvars = 1,	\
  x##cpro3.next = &x##cpro2, x##cpro3.var = &var3, x##cpro3.nvars = 1,	\
  x##cpro4.next = &x##cpro3, x##cpro4.var = &var4, x##cpro4.nvars = 1,	\
  x##cpro5.next = &x##cpro4, x##cpro5.var = &var5, x##cpro5.nvars = 1,	\
  gcprolist = &x##cpro5 ))

#define XGCPRO1_ARRAY(x, array, n) ((void) (				 \
  x##cpro1.next = gcprolist,  x##cpro1.var = array,  x##cpro1.nvars = n, \
  gcprolist = &x##cpro1 ))

#define XGCPRO2_ARRAY(x, array1, n1, array2, n2) ((void) (		  \
  x##cpro1.next = gcprolist,  x##cpro1.var = array1, x##cpro1.nvars = n1, \
  x##cpro2.next = &x##cpro1, x##cpro2.var = array2, x##cpro2.nvars = n2,  \
  gcprolist = &x##cpro2 ))

#define XGCPRO3_ARRAY(x, array1, n1, array2, n2, array3, n3) ((void) (	  \
  x##cpro1.next = gcprolist,  x##cpro1.var = array1, x##cpro1.nvars = n1, \
  x##cpro2.next = &x##cpro1, x##cpro2.var = array2, x##cpro2.nvars = n2,  \
  x##cpro3.next = &x##cpro2, x##cpro3.var = array3, x##cpro3.nvars = n3,  \
  gcprolist = &x##cpro3 ))

#if defined (__cplusplus) && defined (ERROR_CHECK_GC)
/* We need to reset each gcpro to avoid triggering the assert() in
   ~gcpro().  This happens in UNGCPRO and longjmp(). */
#define UNWIND_GCPRO_TO(val)						   \
do									   \
{									   \
  struct gcpro *__gcpro_stop = (val);					   \
  /* Try to catch UNGCPRO without GCPRO.  We arrange for there to be a	   \
     sentinel at the end of the gcprolist, so it should never be NULL. */  \
  assert (__gcpro_stop);						   \
  while (gcprolist != __gcpro_stop)					   \
    {									   \
      struct gcpro *__gcpro_next = gcprolist->next;			   \
      gcprolist->next = 0;						   \
      gcprolist = __gcpro_next;						   \
      assert (gcprolist);						   \
    }									   \
} while (0)
#else
#define UNWIND_GCPRO_TO(val) (gcprolist = (val))
#endif /* defined (__cplusplus) && defined (ERROR_CHECK_GC) */

#define XUNGCPRO(x) UNWIND_GCPRO_TO (x##cpro1.next)

#endif /* ! DEBUG_GCPRO */

#define GCDECL1 XGCDECL1 (g)
#define GCDECL2 XGCDECL2 (g)
#define GCDECL3 XGCDECL3 (g)
#define GCDECL4 XGCDECL4 (g)
#define GCDECL5 XGCDECL5 (g)

#define GCPRO1(a) XGCPRO1 (g,a)
#define GCPRO2(a,b) XGCPRO2 (g,a,b)
#define GCPRO3(a,b,c) XGCPRO3 (g,a,b,c)
#define GCPRO4(a,b,c,d) XGCPRO4 (g,a,b,c,d)
#define GCPRO5(a,b,c,d,e) XGCPRO5 (g,a,b,c,d,e)

#define GCPRO1_ARRAY(a1,n1) XGCPRO1_ARRAY(g,a1,n1)
#define GCPRO2_ARRAY(a1,n1,a2,n2) XGCPRO2_ARRAY (g,a1,n1,a2,n2)
#define GCPRO3_ARRAY(a1,n1,a2,n2,a3,n3) XGCPRO3_ARRAY (g,a1,n1,a2,n2,a3,n3)

#define UNGCPRO XUNGCPRO (g)

#define NGCDECL1 XGCDECL1 (ng)
#define NGCDECL2 XGCDECL2 (ng)
#define NGCDECL3 XGCDECL3 (ng)
#define NGCDECL4 XGCDECL4 (ng)
#define NGCDECL5 XGCDECL5 (ng)

#define NGCPRO1(a) XGCPRO1 (ng,a)
#define NGCPRO2(a,b) XGCPRO2 (ng,a,b)
#define NGCPRO3(a,b,c) XGCPRO3 (ng,a,b,c)
#define NGCPRO4(a,b,c,d) XGCPRO4 (ng,a,b,c,d)
#define NGCPRO5(a,b,c,d,e) XGCPRO5 (ng,a,b,c,d,e)

#define NGCPRO1_ARRAY(a1,n1) XGCPRO1_ARRAY(ng,a1,n1)
#define NGCPRO2_ARRAY(a1,n1,a2,n2) XGCPRO2_ARRAY (ng,a1,n1,a2,n2)
#define NGCPRO3_ARRAY(a1,n1,a2,n2,a3,n3) XGCPRO3_ARRAY (ng,a1,n1,a2,n2,a3,n3)

#define NUNGCPRO XUNGCPRO (ng)

#define NNGCDECL1 XGCDECL1 (nng)
#define NNGCDECL2 XGCDECL2 (nng)
#define NNGCDECL3 XGCDECL3 (nng)
#define NNGCDECL4 XGCDECL4 (nng)
#define NNGCDECL5 XGCDECL5 (nng)

#define NNGCPRO1(a) XGCPRO1 (nng,a)
#define NNGCPRO2(a,b) XGCPRO2 (nng,a,b)
#define NNGCPRO3(a,b,c) XGCPRO3 (nng,a,b,c)
#define NNGCPRO4(a,b,c,d) XGCPRO4 (nng,a,b,c,d)
#define NNGCPRO5(a,b,c,d,e) XGCPRO5 (nng,a,b,c,d,e)

#define NNGCPRO1_ARRAY(a1,n1) XGCPRO1_ARRAY(nng,a1,n1)
#define NNGCPRO2_ARRAY(a1,n1,a2,n2) XGCPRO2_ARRAY (nng,a1,n1,a2,n2)
#define NNGCPRO3_ARRAY(a1,n1,a2,n2,a3,n3) XGCPRO3_ARRAY (nng,a1,n1,a2,n2,a3,n3)

#define NNUNGCPRO XUNGCPRO (nng)

/* Evaluate expr, UNGCPRO, and then return the value of expr.  */
#define RETURN_UNGCPRO(expr) do		\
{					\
  Lisp_Object ret_ungc_val = (expr);	\
  UNGCPRO;				\
  RETURN_SANS_WARNINGS ret_ungc_val;	\
} while (0)

/* Evaluate expr, NUNGCPRO, UNGCPRO, and then return the value of expr.  */
#define RETURN_NUNGCPRO(expr) do	\
{					\
  Lisp_Object ret_ungc_val = (expr);	\
  NUNGCPRO;				\
  UNGCPRO;				\
  RETURN_SANS_WARNINGS ret_ungc_val;	\
} while (0)

/* Evaluate expr, NNUNGCPRO, NUNGCPRO, UNGCPRO, and then return the
   value of expr.  */
#define RETURN_NNUNGCPRO(expr) do	\
{					\
  Lisp_Object ret_ungc_val = (expr);	\
  NNUNGCPRO;				\
  NUNGCPRO;				\
  UNGCPRO;				\
  RETURN_SANS_WARNINGS ret_ungc_val;	\
} while (0)

extern Lisp_Object_ptr_dynarr *staticpros;
extern Lisp_Object_ptr_dynarr *staticpros_nodump;
#ifdef DEBUG_XEMACS

/* Help debug crashes gc-marking a staticpro'ed object. */

MODULE_API void staticpro_1 (Lisp_Object *, Ascbyte *);
MODULE_API void staticpro_nodump_1 (Lisp_Object *, Ascbyte *);
#define staticpro(ptr) staticpro_1 (ptr, #ptr)
#define staticpro_nodump(ptr) staticpro_nodump_1 (ptr, #ptr)

#ifdef HAVE_SHLIB
MODULE_API void unstaticpro_nodump_1 (Lisp_Object *, Ascbyte *);
#define unstaticpro_nodump(ptr) unstaticpro_nodump_1 (ptr, #ptr)
#endif

#else

/* Call staticpro (&var) to protect static variable `var'. */
MODULE_API void staticpro (Lisp_Object *);

/* Call staticpro_nodump (&var) to protect static variable `var'. */
/* var will not be saved at dump time */
MODULE_API void staticpro_nodump (Lisp_Object *);

#ifdef HAVE_SHLIB
/* Call unstaticpro_nodump (&var) to stop protecting static variable `var'. */
MODULE_API void unstaticpro_nodump (Lisp_Object *);
#endif

#endif

#ifdef NEW_GC
extern Lisp_Object_dynarr *mcpros;
#ifdef DEBUG_XEMACS
/* Help debug crashes gc-marking a mcpro'ed object. */
MODULE_API void mcpro_1 (Lisp_Object, char *);
#define mcpro(ptr) mcpro_1 (ptr, #ptr)
#else /* not DEBUG_XEMACS */
/* Call mcpro (&var) to protect mc variable `var'. */
MODULE_API void mcpro (Lisp_Object);
#endif /* not DEBUG_XEMACS */
#endif /* NEW_GC */

void register_post_gc_action (void (*fun) (void *), void *arg);
int begin_gc_forbidden (void);
void end_gc_forbidden (int count);
extern int gc_currently_forbidden;

END_C_DECLS


/************************************************************************/
/*		              Misc definitions        	                */
/************************************************************************/

/************************************************************************/
/*		             Other numeric types      	                */
/************************************************************************/
#ifdef WITH_NUMBER_TYPES
#include "number.h"
#else
#define make_integer(x) make_int(x)
#endif


/************************************************************************/
/*                              prototypes                              */
/************************************************************************/

/* NOTE: Prototypes should go HERE, not in various header files, unless
   they specifically reference a type that's not defined in lisp.h.
   (And even then, you might consider adding the type to lisp.h.)

   The idea is that header files typically contain the innards of objects,
   and we want to minimize the number of "dependencies" of one file on
   the specifics of such objects.  Putting prototypes here minimizes the
   number of header files that need to be included -- good for a number
   of reasons. --ben */

/*--------------- prototypes for various public c functions ------------*/

/* Prototypes for all init/syms_of/vars_of initialization functions. */
#include "symsinit.h"

BEGIN_C_DECLS

/* Defined in abbrev.c */
MODULE_API EXFUN (Fexpand_abbrev, 0);

/* Defined in alloc.c */
MODULE_API EXFUN (Fcons, 2);
MODULE_API EXFUN (Flist, MANY);
EXFUN (Fmake_byte_code, MANY);
MODULE_API EXFUN (Fmake_list, 2);
MODULE_API EXFUN (Fmake_string, 2);
MODULE_API EXFUN (Fmake_symbol, 1);
MODULE_API EXFUN (Fmake_vector, 2);
MODULE_API EXFUN (Fvector, MANY);

#ifndef NEW_GC
void release_breathing_space (void);
#endif /* not NEW_GC */
Lisp_Object noseeum_cons (Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object make_vector (Elemcount, Lisp_Object);
MODULE_API Lisp_Object vector1 (Lisp_Object);
MODULE_API Lisp_Object vector2 (Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object vector3 (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object make_bit_vector (Elemcount, Lisp_Object);
Lisp_Object make_bit_vector_from_byte_vector (unsigned char *, Elemcount);
Lisp_Object noseeum_make_marker (void);
#ifndef NEW_GC
void garbage_collect_1 (void);
#endif /* not NEW_GC */
MODULE_API Lisp_Object acons (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object cons3 (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object list1 (Lisp_Object);
MODULE_API Lisp_Object list2 (Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object list3 (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object list4 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object);
MODULE_API Lisp_Object list5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object);
MODULE_API Lisp_Object list6 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object);
DECLARE_DOESNT_RETURN (memory_full (void));
void disksave_object_finalization (void);
extern int purify_flag;
#ifndef NEW_GC
extern EMACS_INT gc_generation_number[1];
#endif /* not NEW_GC */
int c_readonly (Lisp_Object);
int lisp_readonly (Lisp_Object);
MODULE_API void copy_lisp_object (Lisp_Object dst, Lisp_Object src);
MODULE_API Lisp_Object build_intstring (const Ibyte *);
MODULE_API Lisp_Object build_string (const CIbyte *);
MODULE_API Lisp_Object build_ext_string (const Extbyte *, Lisp_Object);
MODULE_API Lisp_Object build_msg_intstring (const Ibyte *);
MODULE_API Lisp_Object build_msg_string (const CIbyte *);
MODULE_API Lisp_Object make_string (const Ibyte *, Bytecount);
MODULE_API Lisp_Object make_ext_string (const Extbyte *, EMACS_INT, Lisp_Object);
void init_string_ascii_begin (Lisp_Object string);
Lisp_Object make_uninit_string (Bytecount);
MODULE_API Lisp_Object make_float (double);
Lisp_Object make_string_nocopy (const Ibyte *, Bytecount);
void free_cons (Lisp_Object);
void free_list (Lisp_Object);
void free_alist (Lisp_Object);
void free_marker (Lisp_Object);
int object_dead_p (Lisp_Object);
void mark_object (Lisp_Object obj);
#ifndef NEW_GC
#ifdef USE_KKCC
#ifdef DEBUG_XEMACS
void kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj, int level, int pos);
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj, level, pos)
void kkcc_backtrace (void);
#else
void kkcc_gc_stack_push_lisp_object_1 (Lisp_Object obj);
#define kkcc_gc_stack_push_lisp_object(obj, level, pos) \
  kkcc_gc_stack_push_lisp_object_1 (obj)
#define kkcc_backtrace()
#endif
#endif /* USE_KKCC */
#endif /* not NEW_GC */
int marked_p (Lisp_Object obj);
extern int funcall_allocation_flag;
extern int need_to_garbage_collect;
extern MODULE_API int need_to_check_c_alloca;
extern int need_to_signal_post_gc;
extern Lisp_Object Qpost_gc_hook, Qgarbage_collecting;
void recompute_funcall_allocation_flag (void);

#ifdef MEMORY_USAGE_STATS
Bytecount malloced_storage_size (void *, Bytecount, struct overhead_stats *);
Bytecount fixed_type_block_overhead (Bytecount);
#endif

#ifdef EVENT_DATA_AS_OBJECTS
Lisp_Object make_key_data (void);
Lisp_Object make_button_data (void);
Lisp_Object make_motion_data (void);
Lisp_Object make_process_data (void);
Lisp_Object make_timeout_data (void);
Lisp_Object make_magic_data (void);
Lisp_Object make_magic_eval_data (void);
Lisp_Object make_eval_data (void);
Lisp_Object make_misc_user_data (void);
void free_key_data (Lisp_Object);
void free_button_data (Lisp_Object);
void free_motion_data (Lisp_Object);
void free_process_data (Lisp_Object);
void free_timeout_data (Lisp_Object);
void free_magic_data (Lisp_Object);
void free_magic_eval_data (Lisp_Object);
void free_eval_data (Lisp_Object);
void free_misc_user_data (Lisp_Object);
#endif /* EVENT_DATA_AS_OBJECTS */

/* Defined in buffer.c */
Lisp_Object get_truename_buffer (Lisp_Object);
void switch_to_buffer (Lisp_Object, Lisp_Object);
extern int find_file_compare_truenames;
extern int find_file_use_truenames;
Ibyte *get_initial_directory (Ibyte *pathname, Bytecount size);
void set_buffer_internal (struct buffer *b);
struct buffer *decode_buffer (Lisp_Object buffer, int allow_string);

void record_buffer (Lisp_Object buf);
Lisp_Object get_buffer (Lisp_Object name,
			int error_if_deleted_or_does_not_exist);
int map_over_sharing_buffers (struct buffer *buf,
			      int (*mapfun) (struct buffer *buf,
					     void *closure),
			      void *closure);
void cleanup_buffer_undo_lists (void);

extern struct buffer *current_buffer;

extern void init_initial_directory (void);   /* initialize initial_directory */

EXFUN (Fbuffer_disable_undo, 1);
MODULE_API EXFUN (Fbuffer_modified_p, 1);
MODULE_API EXFUN (Fbuffer_name, 1);
MODULE_API EXFUN (Fcurrent_buffer, 0);
EXFUN (Ferase_buffer, 1);
EXFUN (Fget_buffer, 1);
EXFUN (Fget_buffer_create, 1);
EXFUN (Fget_file_buffer, 1);
MODULE_API EXFUN (Fkill_buffer, 1);
EXFUN (Fother_buffer, 3);
EXFUN (Frecord_buffer, 1);
MODULE_API EXFUN (Fset_buffer, 1);
EXFUN (Fset_buffer_modified_p, 2);

extern Lisp_Object QSscratch, Qafter_change_function, Qafter_change_functions;
extern Lisp_Object Qbefore_change_function, Qbefore_change_functions;
extern Lisp_Object Qbuffer_or_string_p, Qdefault_directory, Qfirst_change_hook;
extern Lisp_Object Qpermanent_local, Vafter_change_function;
extern Lisp_Object Vafter_change_functions, Vbefore_change_function;
extern Lisp_Object Vbefore_change_functions, Vbuffer_alist, Vbuffer_defaults;
extern Lisp_Object Vinhibit_read_only, Vtransient_mark_mode;

/* Defined in bytecode.c */
EXFUN (Fbyte_code, 3);

DECLARE_DOESNT_RETURN (invalid_byte_code
		       (const CIbyte *reason, Lisp_Object frob));

/* Defined in callint.c */
EXFUN (Fcall_interactively, 3);
EXFUN (Fprefix_numeric_value, 1);

/* Defined in casefiddle.c */
EXFUN (Fdowncase, 2);
EXFUN (Fupcase, 2);
EXFUN (Fupcase_initials, 2);
EXFUN (Fupcase_initials_region, 3);
EXFUN (Fupcase_region, 3);

/* Defined in casetab.c */
EXFUN (Fset_standard_case_table, 1);

/* Defined in chartab.c */
EXFUN (Freset_char_table, 1);

/* Defined in cmds.c */
EXFUN (Fbeginning_of_line, 2);
EXFUN (Fend_of_line, 2);
EXFUN (Fforward_char, 2);
EXFUN (Fforward_line, 2);

/* Defined in data.c */
EXFUN (Fadd1, 1);
EXFUN (Faref, 2);
EXFUN (Faset, 3);
EXFUN (Fcar, 1);
EXFUN (Fcar_safe, 1);
EXFUN (Fcdr, 1);
EXFUN (Fcdr_safe, 1);
EXFUN (Fgeq, MANY);
EXFUN (Fgtr, MANY);
EXFUN (Findirect_function, 1);
EXFUN (Fleq, MANY);
EXFUN (Flistp, 1);
EXFUN (Flss, MANY);
EXFUN (Fmax, MANY);
EXFUN (Fmin, MANY);
EXFUN (Fminus, MANY);
EXFUN (Fnumber_to_string, 1);
EXFUN (Fplus, MANY);
EXFUN (Fquo, MANY);
EXFUN (Frem, 2);
EXFUN (Fsetcar, 2);
EXFUN (Fsetcdr, 2);
EXFUN (Fsub1, 1);
EXFUN (Fsubr_max_args, 1);
EXFUN (Fsubr_min_args, 1);
EXFUN (Ftimes, MANY);

DECLARE_DOESNT_RETURN (c_write_error (Lisp_Object));
DECLARE_DOESNT_RETURN (lisp_write_error (Lisp_Object));
DECLARE_DOESNT_RETURN (args_out_of_range (Lisp_Object, Lisp_Object));
DECLARE_DOESNT_RETURN (args_out_of_range_3 (Lisp_Object, Lisp_Object,
					    Lisp_Object));
MODULE_API Lisp_Object wrong_type_argument (Lisp_Object, Lisp_Object);
MODULE_API
DECLARE_DOESNT_RETURN (dead_wrong_type_argument (Lisp_Object, Lisp_Object));
void check_int_range (EMACS_INT, EMACS_INT, EMACS_INT);

EXFUN (Fint_to_char, 1);
EXFUN (Fchar_to_int, 1);

enum arith_comparison {
  arith_equal,
  arith_notequal,
  arith_less,
  arith_grtr,
  arith_less_or_equal,
  arith_grtr_or_equal };
Lisp_Object arithcompare (Lisp_Object, Lisp_Object, enum arith_comparison);

/* Do NOT use word_to_lisp or wasteful_word_to_lisp to decode time_t's
   unless you KNOW arg is non-negative.  They cannot return negative
   values!  Use make_time.  */
Lisp_Object word_to_lisp (unsigned int);
unsigned int lisp_to_word (Lisp_Object);

/* Defined in dired.c */
Lisp_Object make_directory_hash_table (const Ibyte *);
Lisp_Object wasteful_word_to_lisp (unsigned int);

/* Defined in doc.c */
EXFUN (Fsubstitute_command_keys, 1);

Lisp_Object unparesseuxify_doc_string (int fd, EMACS_INT position,
				       Ibyte *name_nonreloc,
				       Lisp_Object name_reloc,
				       int standard_doc_file);
Lisp_Object read_doc_string (Lisp_Object);

/* Defined in doprnt.c */
Bytecount emacs_doprnt_va (Lisp_Object stream, const Ibyte *format_nonreloc,
			   Bytecount format_length, Lisp_Object format_reloc,
			   va_list vargs);
Bytecount emacs_doprnt (Lisp_Object stream, const Ibyte *format_nonreloc,
			Bytecount format_length, Lisp_Object format_reloc,
			int nargs, const Lisp_Object *largs, ...);
Lisp_Object emacs_vsprintf_string_lisp (const CIbyte *format_nonreloc,
				   Lisp_Object format_reloc, int nargs,
				   const Lisp_Object *largs);
Lisp_Object emacs_sprintf_string_lisp (const CIbyte *format_nonreloc,
				 Lisp_Object format_reloc, int nargs, ...);
Ibyte *emacs_vsprintf_malloc_lisp (const CIbyte *format_nonreloc,
				     Lisp_Object format_reloc, int nargs,
				     const Lisp_Object *largs,
				     Bytecount *len_out);
Ibyte *emacs_sprintf_malloc_lisp (Bytecount *len_out,
				    const CIbyte *format_nonreloc,
				    Lisp_Object format_reloc, int nargs, ...);
Lisp_Object emacs_vsprintf_string (const CIbyte *format, va_list vargs);
Lisp_Object emacs_sprintf_string (const CIbyte *format, ...)
     PRINTF_ARGS (1, 2);
Ibyte *emacs_vsprintf_malloc (const CIbyte *format, va_list vargs,
				Bytecount *len_out);
Ibyte *emacs_sprintf_malloc (Bytecount *len_out, const CIbyte *format, ...)
     PRINTF_ARGS (2, 3);
Bytecount emacs_vsprintf (Ibyte *output, const CIbyte *format,
			  va_list vargs);
Bytecount emacs_sprintf (Ibyte *output, const CIbyte *format, ...)
     PRINTF_ARGS (2, 3);


/* Defined in editfns.c */
EXFUN (Fbobp, 1);
EXFUN (Fbolp, 1);
EXFUN (Fbuffer_substring, 3);
EXFUN (Fchar_after, 2);
EXFUN (Fchar_to_string, 1);
EXFUN (Fdelete_region, 3);
EXFUN (Feobp, 1);
EXFUN (Feolp, 1);
EXFUN (Ffollowing_char, 1);
EXFUN (Fformat, MANY);
EXFUN (Fgoto_char, 2);
EXFUN (Finsert, MANY);
EXFUN (Finsert_buffer_substring, 3);
EXFUN (Finsert_char, 4);
EXFUN (Fnarrow_to_region, 3);
EXFUN (Fpoint, 1);
EXFUN (Fpoint_marker, 2);
EXFUN (Fpoint_max, 1);
EXFUN (Fpoint_min, 1);
EXFUN (Fpreceding_char, 1);
EXFUN (Fsystem_name, 0);
EXFUN (Fuser_home_directory, 0);
EXFUN (Fuser_login_name, 1);
EXFUN (Fwiden, 1);

void uncache_home_directory (void);
Ibyte *get_home_directory (void);
Ibyte *user_login_name (uid_t *);
void buffer_insert1 (struct buffer *, Lisp_Object);
Lisp_Object make_string_from_buffer (struct buffer *, Charbpos, Charcount);
Lisp_Object make_string_from_buffer_no_extents (struct buffer *, Charbpos, Charcount);
Lisp_Object make_time (time_t);
Lisp_Object save_excursion_save (void);
Lisp_Object save_restriction_save (struct buffer *buf);
Lisp_Object save_excursion_restore (Lisp_Object);
Lisp_Object save_restriction_restore (Lisp_Object);
void widen_buffer (struct buffer *b, int no_clip);
int beginning_of_line_p (struct buffer *b, Charbpos pt);

/* Defined in emacsfns.c */
Lisp_Object save_current_buffer_restore (Lisp_Object);

/* Defined in emacs.c */
EXFUN_NORETURN (Fkill_emacs, 1);
EXFUN (Frunning_temacs_p, 0);
EXFUN (Fforce_debugging_signal, 1);

SIGTYPE fatal_error_signal (int);
Lisp_Object make_arg_list (int, Wexttext **);
void make_argc_argv (Lisp_Object, int *, Wexttext ***);
void free_argc_argv (Wexttext **);
Lisp_Object split_external_path (const Extbyte *path);
Lisp_Object split_env_path (const CIbyte *evarname, const Ibyte *default_);

/* Nonzero means don't do interactive redisplay and don't change tty modes */
extern int noninteractive, noninteractive1;
extern int inhibit_non_essential_conversion_operations;
extern int preparing_for_armageddon;
extern Fixnum emacs_priority;
extern int suppress_early_error_handler_backtrace;
void debug_break (void);
int debug_can_access_memory (void *ptr, Bytecount len);
DECLARE_DOESNT_RETURN (really_abort (void));
void zero_out_command_line_status_vars (void);

/* Defined in emodules.c */
#ifdef HAVE_SHLIB
EXFUN (Flist_modules, 0);
EXFUN (Fload_module, 3);
extern int unloading_module;
#endif

/* Defined in eval.c */
MODULE_API EXFUN (Fapply, MANY);
EXFUN (Fbacktrace, 2);
EXFUN (Fcommand_execute, 3);
EXFUN (Fcommandp, 1);
MODULE_API EXFUN (Feval, 1);
MODULE_API EXFUN (Ffuncall, MANY);
EXFUN (Ffunctionp, 1);
EXFUN (Finteractive_p, 0);
EXFUN (Fprogn, UNEVALLED);
MODULE_API EXFUN (Fsignal, 2);
MODULE_API EXFUN_NORETURN (Fthrow, 2);
MODULE_API EXFUN (Fcall_with_condition_handler, MANY);
EXFUN (Ffunction_max_args, 1);
EXFUN (Ffunction_min_args, 1);

MODULE_API DECLARE_DOESNT_RETURN (signal_error_1 (Lisp_Object, Lisp_Object));
void maybe_signal_error_1 (Lisp_Object, Lisp_Object, Lisp_Object,
			   Error_Behavior);
Lisp_Object maybe_signal_continuable_error_1 (Lisp_Object, Lisp_Object,
					      Lisp_Object, Error_Behavior);
MODULE_API DECLARE_DOESNT_RETURN (signal_ferror (Lisp_Object, const CIbyte *,
						 ...)) PRINTF_ARGS(2, 3);
void maybe_signal_ferror (Lisp_Object, Lisp_Object, Error_Behavior,
			  const CIbyte *, ...) PRINTF_ARGS (4, 5);
Lisp_Object signal_continuable_ferror (Lisp_Object, const CIbyte *, ...)
     PRINTF_ARGS (2, 3);
Lisp_Object maybe_signal_continuable_ferror (Lisp_Object, Lisp_Object,
					     Error_Behavior,
					     const CIbyte *, ...)
     PRINTF_ARGS (4, 5);

Lisp_Object build_error_data (const CIbyte *reason, Lisp_Object frob);
DECLARE_DOESNT_RETURN (signal_error (Lisp_Object, const CIbyte *,
				     Lisp_Object));
void maybe_signal_error (Lisp_Object, const CIbyte *, Lisp_Object,
			 Lisp_Object, Error_Behavior);
Lisp_Object signal_continuable_error (Lisp_Object, const CIbyte *,
				      Lisp_Object);
Lisp_Object maybe_signal_continuable_error (Lisp_Object, const CIbyte *,
					    Lisp_Object,
					    Lisp_Object, Error_Behavior);
DECLARE_DOESNT_RETURN (signal_ferror_with_frob (Lisp_Object, Lisp_Object,
						const CIbyte *, ...))
       PRINTF_ARGS(3, 4);
void maybe_signal_ferror_with_frob (Lisp_Object, Lisp_Object, Lisp_Object,
				    Error_Behavior,
				    const CIbyte *, ...) PRINTF_ARGS (5, 6);
Lisp_Object signal_continuable_ferror_with_frob (Lisp_Object, Lisp_Object,
						 const CIbyte *,
						 ...) PRINTF_ARGS (3, 4);
Lisp_Object maybe_signal_continuable_ferror_with_frob (Lisp_Object,
						       Lisp_Object,
						       Lisp_Object,
						       Error_Behavior,
						       const CIbyte *, ...)
     PRINTF_ARGS (5, 6);
DECLARE_DOESNT_RETURN (signal_error_2 (Lisp_Object, const CIbyte *,
				       Lisp_Object, Lisp_Object));
void maybe_signal_error_2 (Lisp_Object, const CIbyte *, Lisp_Object,
			   Lisp_Object, Lisp_Object, Error_Behavior);
Lisp_Object signal_continuable_error_2 (Lisp_Object, const CIbyte *,
					Lisp_Object, Lisp_Object);
Lisp_Object maybe_signal_continuable_error_2 (Lisp_Object, const CIbyte *,
					      Lisp_Object, Lisp_Object,
					      Lisp_Object,
					      Error_Behavior);


MODULE_API DECLARE_DOESNT_RETURN (signal_malformed_list_error (Lisp_Object));
MODULE_API DECLARE_DOESNT_RETURN (signal_malformed_property_list_error
				  (Lisp_Object));
MODULE_API DECLARE_DOESNT_RETURN (signal_circular_list_error (Lisp_Object));
MODULE_API DECLARE_DOESNT_RETURN (signal_circular_property_list_error
				  (Lisp_Object));

DECLARE_DOESNT_RETURN (syntax_error (const CIbyte *reason,
				     Lisp_Object frob));
DECLARE_DOESNT_RETURN (syntax_error_2 (const CIbyte *reason,
				       Lisp_Object frob1,
				       Lisp_Object frob2));
void maybe_syntax_error (const CIbyte *, Lisp_Object, Lisp_Object,
			 Error_Behavior);
DECLARE_DOESNT_RETURN (sferror (const CIbyte *reason, Lisp_Object frob));
DECLARE_DOESNT_RETURN (sferror_2 (const CIbyte *reason, Lisp_Object frob1,
				  Lisp_Object frob2));
void maybe_sferror (const CIbyte *, Lisp_Object, Lisp_Object,
		    Error_Behavior);
MODULE_API DECLARE_DOESNT_RETURN (invalid_argument (const CIbyte *reason,
						    Lisp_Object frob));
MODULE_API DECLARE_DOESNT_RETURN (invalid_argument_2 (const CIbyte *reason,
						      Lisp_Object frob1,
						      Lisp_Object frob2));
void maybe_invalid_argument (const CIbyte *, Lisp_Object, Lisp_Object,
			     Error_Behavior);
MODULE_API DECLARE_DOESNT_RETURN (invalid_operation (const CIbyte *reason,
						     Lisp_Object frob));
MODULE_API DECLARE_DOESNT_RETURN (invalid_operation_2 (const CIbyte *reason,
						       Lisp_Object frob1,
						       Lisp_Object frob2));
MODULE_API void maybe_invalid_operation (const CIbyte *, Lisp_Object,
					 Lisp_Object, Error_Behavior);
DECLARE_DOESNT_RETURN (invalid_state (const CIbyte *reason,
					 Lisp_Object frob));
DECLARE_DOESNT_RETURN (invalid_state_2 (const CIbyte *reason,
					   Lisp_Object frob1,
					   Lisp_Object frob2));
void maybe_invalid_state (const CIbyte *, Lisp_Object, Lisp_Object,
			  Error_Behavior);
DECLARE_DOESNT_RETURN (invalid_change (const CIbyte *reason,
					 Lisp_Object frob));
DECLARE_DOESNT_RETURN (invalid_change_2 (const CIbyte *reason,
					   Lisp_Object frob1,
					   Lisp_Object frob2));
void maybe_invalid_change (const CIbyte *, Lisp_Object, Lisp_Object,
			   Error_Behavior);
MODULE_API DECLARE_DOESNT_RETURN (invalid_constant (const CIbyte *reason,
						    Lisp_Object frob));
DECLARE_DOESNT_RETURN (invalid_constant_2 (const CIbyte *reason,
					   Lisp_Object frob1,
					   Lisp_Object frob2));
void maybe_invalid_constant (const CIbyte *, Lisp_Object, Lisp_Object,
			     Error_Behavior);
DECLARE_DOESNT_RETURN (wtaerror (const CIbyte *reason, Lisp_Object frob));
MODULE_API DECLARE_DOESNT_RETURN (out_of_memory (const CIbyte *reason,
						 Lisp_Object frob));
DECLARE_DOESNT_RETURN (stack_overflow (const CIbyte *reason,
				       Lisp_Object frob));
MODULE_API DECLARE_DOESNT_RETURN (printing_unreadable_object (const CIbyte *,
							      ...))
       PRINTF_ARGS (1, 2);

Lisp_Object signal_void_function_error (Lisp_Object);
Lisp_Object signal_invalid_function_error (Lisp_Object);
Lisp_Object signal_wrong_number_of_arguments_error (Lisp_Object, int);

Lisp_Object run_hook_with_args_in_buffer (struct buffer *, int, Lisp_Object *,
					  enum run_hooks_condition);
Lisp_Object run_hook_with_args (int, Lisp_Object *, enum run_hooks_condition);
void va_run_hook_with_args (Lisp_Object, int, ...);
void va_run_hook_with_args_in_buffer (struct buffer *, Lisp_Object, int, ...);
Lisp_Object run_hook (Lisp_Object);
MODULE_API Lisp_Object apply1 (Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call0 (Lisp_Object);
MODULE_API Lisp_Object call1 (Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call2 (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call3 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object);
MODULE_API Lisp_Object call4 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call5 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call6 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object);
MODULE_API Lisp_Object call7 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object call8 (Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object, Lisp_Object,
			      Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object call0_in_buffer (struct buffer *, Lisp_Object);
Lisp_Object call1_in_buffer (struct buffer *, Lisp_Object, Lisp_Object);
Lisp_Object call2_in_buffer (struct buffer *, Lisp_Object, Lisp_Object,
			     Lisp_Object);
Lisp_Object call3_in_buffer (struct buffer *, Lisp_Object, Lisp_Object,
			     Lisp_Object, Lisp_Object);
Lisp_Object call4_in_buffer (struct buffer *, Lisp_Object, Lisp_Object,
			     Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object call5_in_buffer (struct buffer *, Lisp_Object, Lisp_Object,
			     Lisp_Object, Lisp_Object, Lisp_Object,
			     Lisp_Object);
Lisp_Object call6_in_buffer (struct buffer *, Lisp_Object, Lisp_Object,
			     Lisp_Object, Lisp_Object, Lisp_Object,
			     Lisp_Object, Lisp_Object);
Lisp_Object eval_in_buffer (struct buffer *, Lisp_Object);

struct call_trapping_problems_result
{
  int caught_error, caught_throw;
  Lisp_Object error_conditions, data;
  Lisp_Object backtrace;
  Lisp_Object thrown_tag;
  Lisp_Object thrown_value;
};

#define NO_INHIBIT_ERRORS (1<<0)
#define NO_INHIBIT_THROWS (1<<1)
#define INTERNAL_INHIBIT_ERRORS (1<<0)
#define INTERNAL_INHIBIT_THROWS (1<<1)
#define INHIBIT_WARNING_ISSUE (1<<2)
#define ISSUE_WARNINGS_AT_DEBUG_LEVEL (1<<3)
#define INHIBIT_QUIT (1<<4)
#define UNINHIBIT_QUIT (1<<5)
#define INHIBIT_GC (1<<6)
#define INHIBIT_EXISTING_PERMANENT_DISPLAY_OBJECT_DELETION (1<<7)
#define INHIBIT_EXISTING_CODING_SYSTEM_DELETION (1<<8)
#define INHIBIT_EXISTING_CHARSET_DELETION (1<<9)
#define INHIBIT_PERMANENT_DISPLAY_OBJECT_CREATION (1<<10)
#define INHIBIT_CODING_SYSTEM_CREATION (1<<11)
#define INHIBIT_CHARSET_CREATION (1<<12)
#define INHIBIT_EXISTING_BUFFER_TEXT_MODIFICATION (1<<13)
#define INHIBIT_ANY_CHANGE_AFFECTING_REDISPLAY (1<<14)
#define INHIBIT_ENTERING_DEBUGGER (1<<15)
#define CALL_WITH_SUSPENDED_ERRORS (1<<16)
#define POSTPONE_WARNING_ISSUE (1<<17)

enum check_allowed_operation
{
  OPERATION_DELETE_OBJECT,
  OPERATION_CREATE_OBJECT,
  OPERATION_MODIFY_BUFFER_TEXT,
  OPERATION_MODIFY_OBJECT_PROPERTY
};

int get_inhibit_flags (void);
void check_allowed_operation (int what, Lisp_Object obj, Lisp_Object prop);
void note_object_created (Lisp_Object obj);
void note_object_deleted (Lisp_Object obj);
Lisp_Object call_with_condition_handler (Lisp_Object (*handler) (Lisp_Object,
								 Lisp_Object,
								 Lisp_Object),
					 Lisp_Object handler_arg,
					 Lisp_Object (*fun) (Lisp_Object),
					 Lisp_Object arg);
int set_trapping_problems_flags (int flags);
Lisp_Object call_trapping_problems (Lisp_Object warning_class,
				    const Ascbyte *warning_string,
				    int flags,
				    struct call_trapping_problems_result
				    *problem,
				    Lisp_Object (*fun) (void *),
				    void *arg);
Lisp_Object va_call_trapping_problems (Lisp_Object warning_class,
				       const Ascbyte *warning_string,
				       int flags,
				       struct call_trapping_problems_result
				       *problem,
				       lisp_fn_t fun, int nargs, ...);
Lisp_Object call0_trapping_problems (const Ascbyte *, Lisp_Object, int);
Lisp_Object call1_trapping_problems (const Ascbyte *, Lisp_Object, Lisp_Object,
				   int);
Lisp_Object call2_trapping_problems (const Ascbyte *, Lisp_Object, Lisp_Object,
				   Lisp_Object, int);
Lisp_Object call3_trapping_problems (const Ascbyte *, Lisp_Object, Lisp_Object,
				   Lisp_Object, Lisp_Object, int);
Lisp_Object call4_trapping_problems (const Ascbyte *, Lisp_Object, Lisp_Object,
				   Lisp_Object, Lisp_Object, Lisp_Object,
				   int);
Lisp_Object call5_trapping_problems (const Ascbyte *, Lisp_Object, Lisp_Object,
				   Lisp_Object, Lisp_Object, Lisp_Object,
				   Lisp_Object, int);
Lisp_Object eval_in_buffer_trapping_problems (const Ascbyte *, struct buffer *,
					    Lisp_Object, int);
Lisp_Object run_hook_trapping_problems (Lisp_Object, Lisp_Object, int);
Lisp_Object safe_run_hook_trapping_problems (Lisp_Object, Lisp_Object, int);
Lisp_Object run_hook_with_args_in_buffer_trapping_problems (Lisp_Object,
							    struct buffer *,
							    int nargs,
							    Lisp_Object *args,
							    enum
							    run_hooks_condition
							    cond, int flags);
Lisp_Object run_hook_with_args_trapping_problems (Lisp_Object,
						  int nargs,
						  Lisp_Object *args,
						  enum run_hooks_condition
						  cond,
						  int flags);
Lisp_Object va_run_hook_with_args_trapping_problems (Lisp_Object,
						     Lisp_Object hook_var,
						     int nargs, ...);
Lisp_Object va_run_hook_with_args_in_buffer_trapping_problems (Lisp_Object,
							       struct buffer *,
							       Lisp_Object,
							       int nargs, ...);
Lisp_Object call_with_suspended_errors (lisp_fn_t, Lisp_Object,
					Lisp_Object,
					Error_Behavior, int, ...);
/* C Code should be using internal_catch, record_unwind_p, condition_case_1 */
int proper_redisplay_wrapping_in_place (void);
Lisp_Object internal_catch (Lisp_Object, Lisp_Object (*) (Lisp_Object),
			    Lisp_Object, int * volatile,
			    Lisp_Object * volatile,
			    Lisp_Object * volatile);
Lisp_Object condition_case_1 (Lisp_Object,
			      Lisp_Object (*) (Lisp_Object),
			      Lisp_Object,
			      Lisp_Object (*) (Lisp_Object, Lisp_Object),
			      Lisp_Object);
Lisp_Object condition_case_3 (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API Lisp_Object unbind_to_1 (int, Lisp_Object);
#define unbind_to(obj) unbind_to_1 (obj, Qnil)
void specbind (Lisp_Object, Lisp_Object);
MODULE_API int record_unwind_protect (Lisp_Object (*) (Lisp_Object),
				      Lisp_Object);
int record_unwind_protect_freeing_dynarr (void *ptr);
int record_unwind_protect_restoring_int (int *addr, int val);
int internal_bind_int (int *addr, int newval);
int internal_bind_lisp_object (Lisp_Object *addr, Lisp_Object newval);
void do_autoload (Lisp_Object, Lisp_Object); /* GCPROs both arguments */
Lisp_Object un_autoload (Lisp_Object);
void warn_when_safe_lispobj (Lisp_Object, Lisp_Object, Lisp_Object);
MODULE_API void warn_when_safe (Lisp_Object, Lisp_Object, const CIbyte *,
				...) PRINTF_ARGS (3, 4);
extern int backtrace_with_internal_sections;

extern Lisp_Object Vstack_trace_on_error;

/* Defined in event-stream.c */
EXFUN (Faccept_process_output, 3);
EXFUN (Fadd_timeout, 4);
EXFUN (Fdisable_timeout, 1);
EXFUN (Fdiscard_input, 0);
EXFUN (Fdispatch_event, 1);
EXFUN (Fenqueue_eval_event, 2);
EXFUN (Fnext_event, 2);
EXFUN (Fread_key_sequence, 3);
EXFUN (Fsit_for, 2);
EXFUN (Fsleep_for, 1);

void wait_delaying_user_input (int (*) (void *), void *);
int detect_input_pending (int how_many);
void reset_this_command_keys (Lisp_Object, int);
Lisp_Object enqueue_misc_user_event (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object enqueue_misc_user_event_pos (Lisp_Object, Lisp_Object,
					 Lisp_Object, int, int, int, int);
extern int modifier_keys_are_sticky;

/* Defined in event-Xt.c */
void signal_special_Xt_user_event (Lisp_Object, Lisp_Object, Lisp_Object);


/* Defined in events.c */
EXFUN (Fcopy_event, 2);
EXFUN (Fevent_to_character, 4);

void clear_event_resource (void);
Lisp_Object allocate_event (void);

EXFUN (Fevent_x_pixel, 1);
EXFUN (Fevent_y_pixel, 1);


/* Defined in file-coding.c */
EXFUN (Fcoding_category_list, 0);
EXFUN (Fcoding_category_system, 1);
EXFUN (Fcoding_priority_list, 0);
EXFUN (Fcoding_system_description, 1);
EXFUN (Fcoding_system_documentation, 1);
EXFUN (Fcoding_system_list, 1);
EXFUN (Fcoding_system_name, 1);
EXFUN (Fcoding_system_p, 1);
EXFUN (Fcoding_system_property, 2);
EXFUN (Fcoding_system_type, 1);
EXFUN (Fcopy_coding_system, 2);
EXFUN (Fdecode_big5_char, 1);
EXFUN (Fdecode_coding_region, 4);
EXFUN (Fdecode_shift_jis_char, 1);
EXFUN (Fdefine_coding_system_alias, 2);
EXFUN (Fdetect_coding_region, 3);
EXFUN (Fdefault_encoding_detection_enabled_p, 0);
EXFUN (Fencode_big5_char, 1);
EXFUN (Fencode_coding_region, 4);
EXFUN (Fencode_shift_jis_char, 1);
EXFUN (Ffind_coding_system, 1);
EXFUN (Fget_coding_system, 1);
EXFUN (Fmake_coding_system, 4);
EXFUN (Fset_coding_category_system, 2);
EXFUN (Fset_coding_priority_list, 1);
EXFUN (Fsubsidiary_coding_system, 2);

extern Lisp_Object Qshift_jis, Qiso2022, Qbig5, Qccl;
extern Lisp_Object Qcharset_g0;
extern Lisp_Object Qcharset_g1, Qcharset_g2, Qcharset_g3, Qcoding_system_error;
extern Lisp_Object Qcoding_systemp, Qcr, Qcrlf, Qdecode, Qencode;
extern Lisp_Object Qeol_cr, Qeol_crlf, Qeol_lf, Qeol_type, Qescape_quoted;
extern Lisp_Object Qforce_g0_on_output, Qforce_g1_on_output;
extern Lisp_Object Qforce_g2_on_output, Qforce_g3_on_output;
extern Lisp_Object Qinput_charset_conversion, Qlf, Qlock_shift;
extern Lisp_Object Qmnemonic, Qno_ascii_cntl, Qno_ascii_eol;
extern Lisp_Object Qno_conversion, Qraw_text;
extern Lisp_Object Qno_iso6429, Qoutput_charset_conversion;
extern Lisp_Object Qpost_read_conversion, Qpre_write_conversion, Qseven;
extern Lisp_Object Qshort, Vcoding_system_for_read;
extern Lisp_Object Vcoding_system_for_write;
extern Lisp_Object Vfile_name_coding_system, Vkeyboard_coding_system;
extern Lisp_Object Vterminal_coding_system;
extern Lisp_Object Qcanonicalize_after_coding;
int coding_system_is_for_text_file (Lisp_Object coding_system);
Lisp_Object find_coding_system_for_text_file (Lisp_Object name, int eol_wrap);
MODULE_API Lisp_Object get_coding_system_for_text_file (Lisp_Object name,
							int eol_wrap);
int coding_system_is_binary (Lisp_Object coding_system);


/* Defined in fileio.c */
EXFUN (Fdirectory_file_name, 1);
EXFUN (Fdo_auto_save, 2);
EXFUN (Fexpand_file_name, 2);
EXFUN (Ffile_accessible_directory_p, 1);
EXFUN (Ffile_directory_p, 1);
EXFUN (Ffile_executable_p, 1);
EXFUN (Ffile_exists_p, 1);
EXFUN (Ffile_name_absolute_p, 1);
EXFUN (Ffile_name_as_directory, 1);
EXFUN (Ffile_name_directory, 1);
EXFUN (Ffile_name_nondirectory, 1);
EXFUN (Ffile_readable_p, 1);
EXFUN (Ffile_symlink_p, 1);
EXFUN (Ffile_truename, 2);
EXFUN (Ffind_file_name_handler, 2);
EXFUN (Finsert_file_contents_internal, 7);
EXFUN (Fmake_temp_name, 1);
EXFUN (Fsubstitute_in_file_name, 1);
EXFUN (Funhandled_file_name_directory, 1);
EXFUN (Fverify_visited_file_modtime, 1);

void record_auto_save (void);
void force_auto_save_soon (void);
DECLARE_DOESNT_RETURN (report_error_with_errno (Lisp_Object errtype,
						const CIbyte *string,
						Lisp_Object data));
DECLARE_DOESNT_RETURN (report_file_type_error (Lisp_Object errtype,
					       Lisp_Object oserrmess,
					       const CIbyte *string,
					       Lisp_Object data));
DECLARE_DOESNT_RETURN (report_file_error (const CIbyte *, Lisp_Object));
Lisp_Object lisp_strerror (int);
Lisp_Object expand_and_dir_to_file (Lisp_Object, Lisp_Object);
int internal_delete_file (Lisp_Object);
Ibyte *find_end_of_directory_component (const Ibyte *path,
					Bytecount len);

/* Defined in filelock.c */
EXFUN (Funlock_buffer, 0);

void lock_file (Lisp_Object);
void unlock_file (Lisp_Object);
void unlock_all_files (void);
void unlock_buffer (struct buffer *);

/* Defined in floatfns.c */
EXFUN (Ftruncate, 1);

double extract_float (Lisp_Object);

/* Defined in fns.c */
MODULE_API EXFUN (Fappend, MANY);
EXFUN (Fassoc, 2);
EXFUN (Fassq, 2);
EXFUN (Fcanonicalize_lax_plist, 2);
EXFUN (Fcanonicalize_plist, 2);
EXFUN (Fcheck_valid_plist, 1);
EXFUN (Fconcat, MANY);
EXFUN (Fcopy_alist, 1);
EXFUN (Fcopy_list, 1);
EXFUN (Fcopy_sequence, 1);
EXFUN (Fcopy_tree, 2);
EXFUN (Fdelete, 2);
EXFUN (Fdelq, 2);
EXFUN (Fdestructive_alist_to_plist, 1);
EXFUN (Felt, 2);
MODULE_API EXFUN (Fequal, 2);
MODULE_API EXFUN (Fget, 3);
EXFUN (Flast, 2);
EXFUN (Flax_plist_get, 3);
EXFUN (Flax_plist_remprop, 2);
MODULE_API EXFUN (Flength, 1);
EXFUN (Fmapcar, 2);
EXFUN (Fmember, 2);
EXFUN (Fmemq, 2);
EXFUN (Fnconc, MANY);
MODULE_API EXFUN (Fnreverse, 1);
EXFUN (Fnthcdr, 2);
EXFUN (Fold_assq, 2);
EXFUN (Fold_equal, 2);
EXFUN (Fold_member, 2);
EXFUN (Fold_memq, 2);
EXFUN (Fplist_get, 3);
EXFUN (Fplist_member, 2);
EXFUN (Fplist_put, 3);
MODULE_API EXFUN (Fprovide, 1);
MODULE_API EXFUN (Fput, 3);
EXFUN (Frassq, 2);
EXFUN (Fremassq, 2);
EXFUN (Freplace_list, 2);
MODULE_API EXFUN (Freverse, 1);
EXFUN (Fsafe_length, 1);
EXFUN (Fsort, 2);
EXFUN (Fstring_equal, 2);
EXFUN (Fstring_lessp, 2);
EXFUN (Fsubstring, 3);
EXFUN (Fvalid_plist_p, 1);

Lisp_Object list_sort (Lisp_Object, Lisp_Object,
		       int (*) (Lisp_Object, Lisp_Object, Lisp_Object));
Lisp_Object merge (Lisp_Object, Lisp_Object, Lisp_Object);

void bump_string_modiff (Lisp_Object);
Lisp_Object memq_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object assoc_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object assq_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object rassq_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object delq_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object delq_no_quit_and_free_cons (Lisp_Object, Lisp_Object);
Lisp_Object remassoc_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object remassq_no_quit (Lisp_Object, Lisp_Object);
Lisp_Object remrassq_no_quit (Lisp_Object, Lisp_Object);

int plists_differ (Lisp_Object, Lisp_Object, int, int, int);
Lisp_Object internal_plist_get (Lisp_Object, Lisp_Object);
void internal_plist_put (Lisp_Object *, Lisp_Object, Lisp_Object);
int internal_remprop (Lisp_Object *, Lisp_Object);
Lisp_Object external_plist_get (Lisp_Object *, Lisp_Object,
				int, Error_Behavior);
void external_plist_put (Lisp_Object *, Lisp_Object,
			 Lisp_Object, int, Error_Behavior);
int external_remprop (Lisp_Object *, Lisp_Object, int, Error_Behavior);
int internal_equal_trapping_problems (Lisp_Object warning_class,
    				      const Ascbyte *warning_string,
				      int flags,
				      struct call_trapping_problems_result *p,
				      int retval,
				      Lisp_Object obj1, Lisp_Object obj2,
				      int depth);
int internal_equal (Lisp_Object, Lisp_Object, int);
int internal_equalp (Lisp_Object obj1, Lisp_Object obj2, int depth);
Lisp_Object concat2 (Lisp_Object, Lisp_Object);
Lisp_Object concat3 (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object vconcat2 (Lisp_Object, Lisp_Object);
Lisp_Object vconcat3 (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object nconc2 (Lisp_Object, Lisp_Object);
Lisp_Object bytecode_nconc2 (Lisp_Object *);
void check_losing_bytecode (const char *, Lisp_Object);

Lisp_Object add_suffix_to_symbol (Lisp_Object symbol,
				  const Ascbyte *ascii_string);
Lisp_Object add_prefix_to_symbol (const Ascbyte *ascii_string,
				  Lisp_Object symbol);

/* Defined in free-hook.c */
EXFUN (Freally_free, 1);

/* Defined in glyphs.c */
EXFUN (Fmake_glyph_internal, 1);

Error_Behavior decode_error_behavior_flag (Lisp_Object);
Lisp_Object encode_error_behavior_flag (Error_Behavior);

/* Defined in glyphs-shared.c */
void shared_resource_validate (Lisp_Object instantiator);
Lisp_Object shared_resource_normalize (Lisp_Object inst,
				       Lisp_Object console_type,
				       Lisp_Object dest_mask,
				       Lisp_Object tag);
extern Lisp_Object Q_resource_type, Q_resource_id;

/* Defined in gui.c */
DECLARE_DOESNT_RETURN (gui_error (const Ascbyte *reason,
				  Lisp_Object frob));
DECLARE_DOESNT_RETURN (gui_error_2 (const Ascbyte *reason,
				    Lisp_Object frob0, Lisp_Object frob1));
/* Defined in indent.c */
EXFUN (Findent_to, 3);
EXFUN (Fvertical_motion, 3);

int byte_spaces_at_point (struct buffer *, Bytebpos);
int column_at_point (struct buffer *, Charbpos, int);
int string_column_at_point (Lisp_Object, Charbpos, int);
int current_column (struct buffer *);
void invalidate_current_column (void);
Charbpos vmotion (struct window *, Charbpos, int, int *);
Charbpos vmotion_pixels (Lisp_Object, Charbpos, int, int, int *);

/* Defined in insdel.c */
void set_buffer_point (struct buffer *buf, Charbpos pos, Bytebpos bipos);

/* Defined in intl.c */
EXFUN (Fgettext, 1);

/* Defined in keymap.c */
EXFUN (Fdefine_key, 3);
EXFUN (Fkey_description, 1);
EXFUN (Flookup_key, 3);
EXFUN (Fmake_sparse_keymap, 1);

void where_is_to_char (Lisp_Object, Eistring *);

/* Defined in lread.c */
EXFUN (Fread, 1);

void ebolify_bytecode_constants (Lisp_Object);
void close_load_descs (void);
int locate_file (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object *, int);
EXFUN (Flocate_file_clear_hashing, 1);
int isfloat_string (const char *);
#ifdef HAVE_RATIO
int isratio_string (const char *);
#endif

/* Well, I've decided to enable this. -- ben */
/* And I've decided to make it work right.  -- sb */
#define LOADHIST
/* Define the following symbol to enable load history of dumped files */
#define LOADHIST_DUMPED
/* Define the following symbol to enable load history of C source */
#define LOADHIST_BUILTIN

#ifdef LOADHIST /* this is just a stupid idea */
#define LOADHIST_ATTACH(x) \
 do { if (initialized) Vcurrent_load_list = Fcons (x, Vcurrent_load_list); } \
 while (0)
#else /*! LOADHIST */
# define LOADHIST_ATTACH(x)
#endif /*! LOADHIST */

/* Defined in macros.c */
EXFUN (Fexecute_kbd_macro, 2);

/* Defined in marker.c */
EXFUN (Fcopy_marker, 2);
EXFUN (Fmake_marker, 0);
EXFUN (Fmarker_buffer, 1);
EXFUN (Fmarker_position, 1);
EXFUN (Fset_marker, 3);
EXFUN (Fset_marker_insertion_type, 2);

Bytebpos byte_marker_position (Lisp_Object);
Charbpos marker_position (Lisp_Object);
void set_byte_marker_position (Lisp_Object, Bytebpos);
void set_marker_position (Lisp_Object, Charbpos);
void unchain_marker (Lisp_Object);
Lisp_Object noseeum_copy_marker (Lisp_Object, Lisp_Object);
Lisp_Object set_marker_restricted (Lisp_Object, Lisp_Object, Lisp_Object);
#ifdef MEMORY_USAGE_STATS
int compute_buffer_marker_usage (struct buffer *, struct overhead_stats *);
#endif
void init_buffer_markers (struct buffer *b);
void uninit_buffer_markers (struct buffer *b);

/* Defined in minibuf.c */
extern int minibuf_level;
Charcount scmp_1 (const Ibyte *, const Ibyte *, Charcount, int);
#define scmp(s1, s2, len) scmp_1 (s1, s2, len, completion_ignore_case)
extern int completion_ignore_case;
int regexp_ignore_completion_p (const Ibyte *, Lisp_Object,
				Bytecount, Bytecount);
Lisp_Object clear_echo_area (struct frame *, Lisp_Object, int);
Lisp_Object clear_echo_area_from_print (struct frame *, Lisp_Object, int);
void echo_area_append (struct frame *, const Ibyte *, Lisp_Object,
		       Bytecount, Bytecount, Lisp_Object);
void echo_area_message (struct frame *, const Ibyte *, Lisp_Object,
			Bytecount, Bytecount, Lisp_Object);
Lisp_Object echo_area_status (struct frame *);
int echo_area_active (struct frame *);
Lisp_Object echo_area_contents (struct frame *);
void message_internal (const Ibyte *, Lisp_Object, Bytecount, Bytecount);
void message_append_internal (const Ibyte *, Lisp_Object,
			      Bytecount, Bytecount);
MODULE_API void message (const char *, ...) PRINTF_ARGS (1, 2);
void message_append (const char *, ...) PRINTF_ARGS (1, 2);
void message_no_translate (const char *, ...) PRINTF_ARGS (1, 2);
void clear_message (void);

/* Defined in mule-charset.c */
EXFUN (Fmake_charset, 3);

extern Lisp_Object Ql2r, Qr2l;

/* Defined in print.c */
EXFUN (Fdisplay_error, 2);
EXFUN (Ferror_message_string, 1);
EXFUN (Fprin1, 2);
EXFUN (Fprin1_to_string, 2);
EXFUN (Fprinc, 2);
EXFUN (Fprint, 2);

Lisp_Object prin1_to_string (Lisp_Object, int);

/* Lower-level ways to output data: */
void default_object_printer (Lisp_Object, Lisp_Object, int);
void print_internal (Lisp_Object, Lisp_Object, int);
void debug_print (Lisp_Object);
void debug_p4 (Lisp_Object obj);
void debug_p3 (Lisp_Object obj);
void debug_short_backtrace (int);
void debug_backtrace (void);
/* NOTE: Do not call this with the data of a Lisp_String.  Use princ.
 * Note: stream should be defaulted before calling
 *  (eg Qnil means stdout, not Vstandard_output, etc) */
MODULE_API void write_c_string (Lisp_Object stream, const CIbyte *str);
/* Same goes for this function. */
MODULE_API void write_string (Lisp_Object stream, const Ibyte *str);
/* Same goes for this function. */
void write_string_1 (Lisp_Object stream, const Ibyte *str, Bytecount size);
void write_eistring (Lisp_Object stream, const Eistring *ei);

/* Higher-level (printf-style) ways to output data: */
MODULE_API void write_fmt_string (Lisp_Object stream, const CIbyte *fmt, ...);
MODULE_API void write_fmt_string_lisp (Lisp_Object stream, const CIbyte *fmt,
				       int nargs, ...);
void stderr_out (const CIbyte *, ...) PRINTF_ARGS (1, 2);
void stderr_out_lisp (const CIbyte *, int nargs, ...);
void stdout_out (const CIbyte *, ...) PRINTF_ARGS (1, 2);
void external_out (int dest, const CIbyte *fmt, ...) PRINTF_ARGS (2, 3);
void debug_out (const CIbyte *, ...) PRINTF_ARGS (1, 2);
DECLARE_DOESNT_RETURN (fatal (const CIbyte *, ...)) PRINTF_ARGS(1, 2);

/* Internal functions: */
Lisp_Object canonicalize_printcharfun (Lisp_Object printcharfun);
void temp_output_buffer_setup (Lisp_Object);
void temp_output_buffer_show (Lisp_Object, Lisp_Object);
void print_cons (Lisp_Object, Lisp_Object, int);
void print_vector (Lisp_Object, Lisp_Object, int);
void print_string (Lisp_Object, Lisp_Object, int);
void print_symbol (Lisp_Object, Lisp_Object, int);
void print_float (Lisp_Object, Lisp_Object, int);
/* The number of bytes required to store the decimal printed
   representation of an integral type.  Add a few bytes for truncation,
   optional sign prefix, and null byte terminator.
   2.40824 == log (256) / log (10).

   We don't use floating point since Sun cc (buggily?) cannot use
   floating point computations to define a compile-time integral
   constant. */
#define DECIMAL_PRINT_SIZE(integral_type) \
(((2410824 * sizeof (integral_type)) / 1000000) + 3)
void long_to_string (char *, long);
void ulong_to_bit_string (char *, unsigned long);
extern int print_escape_newlines;
extern MODULE_API int print_readably;
Lisp_Object internal_with_output_to_temp_buffer (Lisp_Object,
						 Lisp_Object (*) (Lisp_Object),
						 Lisp_Object, Lisp_Object);
void float_to_string (char *, double);
void internal_object_printer (Lisp_Object, Lisp_Object, int);

/* Defined in rangetab.c */
EXFUN (Fclear_range_table, 1);
EXFUN (Fget_range_table, 3);
EXFUN (Fmake_range_table, 1);
EXFUN (Fput_range_table, 4);

extern Lisp_Object Qstart_closed_end_open;
extern Lisp_Object Qstart_open_end_open;
extern Lisp_Object Qstart_closed_end_closed;
extern Lisp_Object Qstart_open_end_closed;

void put_range_table (Lisp_Object, EMACS_INT, EMACS_INT, Lisp_Object);
int unified_range_table_bytes_needed (Lisp_Object);
int unified_range_table_bytes_used (void *);
void unified_range_table_copy_data (Lisp_Object, void *);
Lisp_Object unified_range_table_lookup (void *, EMACS_INT, Lisp_Object);
int unified_range_table_nentries (void *);
void unified_range_table_get_range (void *, int, EMACS_INT *, EMACS_INT *,
				    Lisp_Object *);

/* Defined in search.c */
EXFUN (Fmatch_beginning, 1);
EXFUN (Fmatch_end, 1);
EXFUN (Fskip_chars_backward, 3);
EXFUN (Fskip_chars_forward, 3);
EXFUN (Fstring_match, 4);

struct re_pattern_buffer;
struct re_registers;
Charbpos scan_buffer (struct buffer *, Ichar, Charbpos, Charbpos, EMACS_INT,
		      EMACS_INT *, int);
Charbpos find_next_newline (struct buffer *, Charbpos, int);
Charbpos find_next_newline_no_quit (struct buffer *, Charbpos, int);
Bytebpos byte_find_next_newline_no_quit (struct buffer *, Bytebpos, int);
Bytecount byte_find_next_ichar_in_string (Lisp_Object, Ichar, Bytecount,
					 EMACS_INT);
Charbpos find_before_next_newline (struct buffer *, Charbpos, Charbpos, int);
struct re_pattern_buffer *compile_pattern (Lisp_Object pattern,
					   struct re_registers *regp,
					   Lisp_Object translate,
					   Lisp_Object searchobj,
					   struct buffer *searchbuf,
					   int posix, Error_Behavior errb);
Bytecount fast_string_match (Lisp_Object, const Ibyte *,
			     Lisp_Object, Bytecount,
			     Bytecount, int, Error_Behavior, int);
Bytecount fast_lisp_string_match (Lisp_Object, Lisp_Object);
extern Fixnum warn_about_possibly_incompatible_back_references;


/* Defined in signal.c */
void init_interrupts_late (void);

/* Defined in sound.c */
EXFUN (Fding, 3);

void init_device_sound (struct device *);
DECLARE_DOESNT_RETURN (report_sound_error (const Ascbyte *, Lisp_Object));

/* Defined in specifier.c */
EXFUN (Fadd_spec_to_specifier, 5);
EXFUN (Fspecifier_spec_list, 4);

Lisp_Object specifier_instance (Lisp_Object, Lisp_Object, Lisp_Object,
				Error_Behavior, int, int, Lisp_Object);
Lisp_Object specifier_instance_no_quit (Lisp_Object, Lisp_Object, Lisp_Object,
					Error_Behavior, int, Lisp_Object);

/* Defined in symbols.c */
EXFUN (Fboundp, 1);
EXFUN (Fbuilt_in_variable_type, 1);
EXFUN (Fdefault_boundp, 1);
EXFUN (Fdefault_value, 1);
EXFUN (Ffboundp, 1);
EXFUN (Ffset, 2);
EXFUN (Fintern, 2);
EXFUN (Fintern_soft, 3);
EXFUN (Fkill_local_variable, 1);
EXFUN (Fset, 2);
EXFUN (Fset_default, 2);
EXFUN (Fsymbol_function, 1);
EXFUN (Fsymbol_name, 1);
EXFUN (Fsymbol_plist, 1);
EXFUN (Fsymbol_value, 1);

unsigned int hash_string (const Ibyte *, Bytecount);
Lisp_Object intern_int (const Ibyte *str);
MODULE_API Lisp_Object intern (const CIbyte *str);
Lisp_Object intern_converting_underscores_to_dashes (const CIbyte *str);
Lisp_Object oblookup (Lisp_Object, const Ibyte *, Bytecount);
void map_obarray (Lisp_Object, int (*) (Lisp_Object, void *), void *);
Lisp_Object indirect_function (Lisp_Object, int);
Lisp_Object symbol_value_in_buffer (Lisp_Object, Lisp_Object);
void kill_buffer_local_variables (struct buffer *);
int symbol_value_buffer_local_info (Lisp_Object, struct buffer *);
Lisp_Object find_symbol_value (Lisp_Object);
Lisp_Object find_symbol_value_quickly (Lisp_Object, int);
Lisp_Object top_level_value (Lisp_Object);
void reject_constant_symbols (Lisp_Object sym, Lisp_Object newval,
			      int function_p,
			      Lisp_Object follow_past_lisp_magic);

/* Defined in syntax.c */
Charbpos scan_words (struct buffer *, Charbpos, int);
EXFUN (Fchar_syntax, 2);
EXFUN (Fforward_word, 2);
extern Lisp_Object Vstandard_syntax_table;
void signal_syntax_cache_extent_changed (EXTENT extent);
void signal_syntax_cache_extent_adjust (struct buffer *buf);
void init_buffer_syntax_cache (struct buffer *buf);
void mark_buffer_syntax_cache (struct buffer *buf);
void uninit_buffer_syntax_cache (struct buffer *buf);
extern Lisp_Object Qsyntax_table;

/* Defined in sysdep.c */
long get_random (void);
void seed_random (long arg);

/* Defined in text.c */
void find_charsets_in_ibyte_string (unsigned char *charsets,
				      const Ibyte *str,
				      Bytecount len);
void find_charsets_in_ichar_string (unsigned char *charsets,
				     const Ichar *str,
				     Charcount len);
int ibyte_string_displayed_columns (const Ibyte *str, Bytecount len);
int ichar_string_displayed_columns (const Ichar *str, Charcount len);
Charcount ibyte_string_nonascii_chars (const Ibyte *str, Bytecount len);
void convert_ibyte_string_into_ichar_dynarr (const Ibyte *str,
						Bytecount len,
						Ichar_dynarr *dyn);
Charcount convert_ibyte_string_into_ichar_string (const Ibyte *str,
						     Bytecount len,
						     Ichar *arr);
void convert_ichar_string_into_ibyte_dynarr (Ichar *arr, int nels,
						Ibyte_dynarr *dyn);
Ibyte *convert_ichar_string_into_malloced_string (Ichar *arr, int nels,
						    Bytecount *len_out);
Bytecount copy_text_between_formats (const Ibyte *src, Bytecount srclen,
				     Internal_Format srcfmt,
				     Lisp_Object srcobj,
				     Ibyte *dst, Bytecount dstlen,
				     Internal_Format dstfmt,
				     Lisp_Object dstobj,
				     Bytecount *src_used);
Bytecount copy_buffer_text_out (struct buffer *buf, Bytebpos pos,
				Bytecount len, Ibyte *dst, Bytecount dstlen,
				Internal_Format dstfmt, Lisp_Object dstobj,
				Bytecount *src_used);

/* flags for get_buffer_pos_char(), get_buffer_range_char(), etc. */
/* At most one of GB_COERCE_RANGE and GB_NO_ERROR_IF_BAD should be
   specified.  At most one of GB_NEGATIVE_FROM_END and GB_NO_ERROR_IF_BAD
   should be specified. */

#define GB_ALLOW_PAST_ACCESSIBLE	(1 << 0)
#define GB_ALLOW_NIL			(1 << 1)
#define GB_CHECK_ORDER			(1 << 2)
#define GB_COERCE_RANGE			(1 << 3)
#define GB_NO_ERROR_IF_BAD		(1 << 4)
#define GB_NEGATIVE_FROM_END		(1 << 5)
#define GB_HISTORICAL_STRING_BEHAVIOR	(GB_NEGATIVE_FROM_END | GB_ALLOW_NIL)

Charbpos get_buffer_pos_char (struct buffer *b, Lisp_Object pos,
			    unsigned int flags);
Bytebpos get_buffer_pos_byte (struct buffer *b, Lisp_Object pos,
			    unsigned int flags);
void get_buffer_range_char (struct buffer *b, Lisp_Object from, Lisp_Object to,
			    Charbpos *from_out, Charbpos *to_out,
			    unsigned int flags);
void get_buffer_range_byte (struct buffer *b, Lisp_Object from, Lisp_Object to,
			    Bytebpos *from_out, Bytebpos *to_out,
			    unsigned int flags);
Charcount get_string_pos_char (Lisp_Object string, Lisp_Object pos,
			       unsigned int flags);
Bytecount get_string_pos_byte (Lisp_Object string, Lisp_Object pos,
			       unsigned int flags);
void get_string_range_char (Lisp_Object string, Lisp_Object from,
			    Lisp_Object to, Charcount *from_out,
			    Charcount *to_out, unsigned int flags);
void get_string_range_byte (Lisp_Object string, Lisp_Object from,
			    Lisp_Object to, Bytecount *from_out,
			    Bytecount *to_out, unsigned int flags);
Charxpos get_buffer_or_string_pos_char (Lisp_Object object, Lisp_Object pos,
					unsigned int flags);
Bytexpos get_buffer_or_string_pos_byte (Lisp_Object object, Lisp_Object pos,
					unsigned int flags);
void get_buffer_or_string_range_char (Lisp_Object object, Lisp_Object from,
				      Lisp_Object to, Charxpos *from_out,
				      Charxpos *to_out, unsigned int flags);
void get_buffer_or_string_range_byte (Lisp_Object object, Lisp_Object from,
				      Lisp_Object to, Bytexpos *from_out,
				      Bytexpos *to_out, unsigned int flags);
Charxpos buffer_or_string_accessible_begin_char (Lisp_Object object);
Charxpos buffer_or_string_accessible_end_char (Lisp_Object object);
Bytexpos buffer_or_string_accessible_begin_byte (Lisp_Object object);
Bytexpos buffer_or_string_accessible_end_byte (Lisp_Object object);
Charxpos buffer_or_string_absolute_begin_char (Lisp_Object object);
Charxpos buffer_or_string_absolute_end_char (Lisp_Object object);
Bytexpos buffer_or_string_absolute_begin_byte (Lisp_Object object);
Bytexpos buffer_or_string_absolute_end_byte (Lisp_Object object);
Charbpos charbpos_clip_to_bounds (Charbpos lower, Charbpos num,
				  Charbpos upper);
Bytebpos bytebpos_clip_to_bounds (Bytebpos lower, Bytebpos num,
				  Bytebpos upper);
Charxpos charxpos_clip_to_bounds (Charxpos lower, Charxpos num,
				  Charxpos upper);
Bytexpos bytexpos_clip_to_bounds (Bytexpos lower, Bytexpos num,
				  Bytexpos upper);
Charxpos buffer_or_string_clip_to_accessible_char (Lisp_Object object,
						   Charxpos pos);
Bytexpos buffer_or_string_clip_to_accessible_byte (Lisp_Object object,
						   Bytexpos pos);
Charxpos buffer_or_string_clip_to_absolute_char (Lisp_Object object,
						 Charxpos pos);
Bytexpos buffer_or_string_clip_to_absolute_byte (Lisp_Object object,
						 Bytexpos pos);


#ifdef ENABLE_COMPOSITE_CHARS

Ichar lookup_composite_char (Ibyte *str, int len);
Lisp_Object composite_char_string (Ichar ch);
#endif /* ENABLE_COMPOSITE_CHARS */

EXFUN (Ffind_charset, 1);
EXFUN (Fget_charset, 1);
EXFUN (Fcharset_list, 0);

extern Lisp_Object Vcharset_ascii;
extern Lisp_Object Vcharset_control_1;
extern Lisp_Object Vcharset_latin_iso8859_1;
extern Lisp_Object Vcharset_latin_iso8859_2;
extern Lisp_Object Vcharset_latin_iso8859_3;
extern Lisp_Object Vcharset_latin_iso8859_4;
extern Lisp_Object Vcharset_thai_tis620;
extern Lisp_Object Vcharset_greek_iso8859_7;
extern Lisp_Object Vcharset_arabic_iso8859_6;
extern Lisp_Object Vcharset_hebrew_iso8859_8;
extern Lisp_Object Vcharset_katakana_jisx0201;
extern Lisp_Object Vcharset_latin_jisx0201;
extern Lisp_Object Vcharset_cyrillic_iso8859_5;
extern Lisp_Object Vcharset_latin_iso8859_9;
extern Lisp_Object Vcharset_latin_iso8859_15;
extern Lisp_Object Vcharset_japanese_jisx0208_1978;
extern Lisp_Object Vcharset_chinese_gb2312;
extern Lisp_Object Vcharset_japanese_jisx0208;
extern Lisp_Object Vcharset_korean_ksc5601;
extern Lisp_Object Vcharset_japanese_jisx0212;
extern Lisp_Object Vcharset_chinese_cns11643_1;
extern Lisp_Object Vcharset_chinese_cns11643_2;
extern Lisp_Object Vcharset_chinese_big5_1;
extern Lisp_Object Vcharset_chinese_big5_2;
extern Lisp_Object Vcharset_composite;

Ichar Lstream_get_ichar_1 (Lstream *stream, int first_char);
int Lstream_fput_ichar (Lstream *stream, Ichar ch);
void Lstream_funget_ichar (Lstream *stream, Ichar ch);

DECLARE_INLINE_HEADER (Ibyte *qxestrdup (const Ibyte *s))
{
  return (Ibyte *) xstrdup ((const Chbyte *) s);
}

DECLARE_INLINE_HEADER (Bytecount qxestrlen (const Ibyte *s))
{
  return strlen ((const Chbyte *) s);
}

DECLARE_INLINE_HEADER (Charcount qxestrcharlen (const Ibyte *s))
{
  return bytecount_to_charcount (s, qxestrlen (s));
}

DECLARE_INLINE_HEADER (int qxestrcmp (const Ibyte *s1,
				      const Ibyte *s2))
{
  return strcmp ((const Chbyte *) s1, (const Chbyte *) s2);
}

DECLARE_INLINE_HEADER (int qxestrcmp_ascii (const Ibyte *s1,
					    const Ascbyte *s2))
{
  return strcmp ((const Chbyte *) s1, s2);
}

DECLARE_INLINE_HEADER (int qxestrncmp (const Ibyte *string1,
				       const Ibyte *string2,
				       Bytecount count))
{
  return strncmp ((const Chbyte *) string1, (const Chbyte *) string2,
		  (size_t) count);
}

DECLARE_INLINE_HEADER (int qxestrncmp_ascii (const Ibyte *string1,
					     const Ascbyte *string2,
					     Bytecount count))
{
  return strncmp ((const Chbyte *) string1, string2, (size_t) count);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrcpy (Ibyte *strDest,
					 const Ibyte *strSource))
{
  return (Ibyte *) strcpy ((Chbyte *) strDest, (const Chbyte *) strSource);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrcpy_ascii (Ibyte *strDest,
					       const Ascbyte *strSource))
{
  return (Ibyte *) strcpy ((Chbyte *) strDest, strSource);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrncpy (Ibyte *strDest,
					  const Ibyte *strSource,
					  Bytecount count))
{
  return (Ibyte *) strncpy ((Chbyte *) strDest, (const Chbyte *) strSource,
			      (size_t) count);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrncpy_ascii (Ibyte *strDest,
						const Ascbyte *strSource,
						Bytecount count))
{
  return (Ibyte *) strncpy ((Chbyte *) strDest, strSource, (size_t) count);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrcat (Ibyte *strDest,
					 const Ibyte *strSource))
{
  return (Ibyte *) strcat ((Chbyte *) strDest, (const Chbyte *) strSource);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrcat_ascii (Ibyte *strDest,
					       const Ascbyte *strSource))
{
  return (Ibyte *) strcat ((Chbyte *) strDest, strSource);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrncat (Ibyte *strDest,
					  const Ibyte *strSource,
					  Bytecount count))
{
  return (Ibyte *) strncat ((Chbyte *) strDest, (const Chbyte *) strSource,
			      (size_t) count);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrncat_ascii (Ibyte *strDest,
						const Ascbyte *strSource,
						Bytecount count))
{
  return (Ibyte *) strncat ((Chbyte *) strDest, strSource, (size_t) count);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrchr (const Ibyte *s, Ichar c))
{
  assert (c >= 0 && c <= 255);
  return (Ibyte *) strchr ((const Chbyte *) s, c);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrrchr (const Ibyte *s, Ichar c))
{
  assert (c >= 0 && c <= 255);
  return (Ibyte *) strrchr ((const Chbyte *) s, c);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrstr (const Ibyte *string1,
					 const Ibyte *string2))
{
  return (Ibyte *) strstr ((const Chbyte *) string1, (const Chbyte *) string2);
}

DECLARE_INLINE_HEADER (Bytecount qxestrcspn (const Ibyte *string,
					     const CIbyte *strCharSet))
{
  return (Bytecount) strcspn ((const Chbyte *) string, strCharSet);
}

DECLARE_INLINE_HEADER (Bytecount qxestrspn (const Ibyte *string,
					    const CIbyte *strCharSet))
{
  return (Bytecount) strspn ((const Chbyte *) string, strCharSet);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrpbrk (const Ibyte *string,
					  const CIbyte *strCharSet))
{
  return (Ibyte *) strpbrk ((const Chbyte *) string, strCharSet);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrtok (Ibyte *strToken,
					 const CIbyte *strDelimit))
{
  return (Ibyte *) strtok ((Chbyte *) strToken, strDelimit);
}

DECLARE_INLINE_HEADER (double qxestrtod (const Ibyte *nptr,
					 Ibyte **endptr))
{
  return strtod ((const Chbyte *) nptr, (Chbyte **) endptr);
}

DECLARE_INLINE_HEADER (long qxestrtol (const Ibyte *nptr, Ibyte **endptr,
				       int base))
{
  return strtol ((const Chbyte *) nptr, (Chbyte **) endptr, base);
}

DECLARE_INLINE_HEADER (unsigned long qxestrtoul (const Ibyte *nptr,
						 Ibyte **endptr,
						 int base))
{
  return strtoul ((const Chbyte *) nptr, (Chbyte **) endptr, base);
}

DECLARE_INLINE_HEADER (int qxeatoi (const Ibyte *string))
{
  return atoi ((const Chbyte *) string);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrupr (Ibyte *s))
{
  return (Ibyte *) strupr ((Chbyte *) s);
}

DECLARE_INLINE_HEADER (Ibyte *qxestrlwr (Ibyte *s))
{
  return (Ibyte *) strlwr ((Chbyte *) s);
}

int qxesprintf (Ibyte *buffer, const CIbyte *format, ...)
     PRINTF_ARGS (2, 3);

DECLARE_INLINE_HEADER (int qxesscanf_ascii_1 (Ibyte *buffer,
					      const Ascbyte *format,
					      void *ptr))
{
  /* #### DAMNIT! No vsscanf! */
  return sscanf ((Chbyte *) buffer, format, ptr);
}

/* Do not use POSIX locale routines.  Not Mule-correct. */
#define qxestrcoll DO NOT USE.
#define qxestrxfrm DO NOT USE.

int qxestrcasecmp (const Ibyte *s1, const Ibyte *s2);
int qxestrcasecmp_ascii (const Ibyte *s1, const Ascbyte *s2);
int qxestrcasecmp_i18n (const Ibyte *s1, const Ibyte *s2);
int ascii_strcasecmp (const Ascbyte *s1, const Ascbyte *s2);
int lisp_strcasecmp (Lisp_Object s1, Lisp_Object s2);
int lisp_strcasecmp_i18n (Lisp_Object s1, Lisp_Object s2);
int qxestrncasecmp (const Ibyte *s1, const Ibyte *s2, Bytecount len);
int qxestrncasecmp_ascii (const Ibyte *s1, const Ascbyte *s2,
			  Bytecount len);
int qxestrncasecmp_i18n (const Ibyte *s1, const Ibyte *s2, Bytecount len);
int ascii_strncasecmp (const Ascbyte *s1, const Ascbyte *s2,
		       Bytecount len);
int qxememcmp (const Ibyte *s1, const Ibyte *s2, Bytecount len);
int qxememcmp4 (const Ibyte *s1, Bytecount len1,
		const Ibyte *s2, Bytecount len2);
int qxememcasecmp (const Ibyte *s1, const Ibyte *s2, Bytecount len);
int qxememcasecmp4 (const Ibyte *s1, Bytecount len1,
		    const Ibyte *s2, Bytecount len2);
int qxetextcmp (const Ibyte *s1, Bytecount len1,
		const Ibyte *s2, Bytecount len2);
int qxetextcmp_matching (const Ibyte *s1, Bytecount len1,
			 const Ibyte *s2, Bytecount len2,
			 Charcount *matching);
int qxetextcasecmp (const Ibyte *s1, Bytecount len1,
		    const Ibyte *s2, Bytecount len2);
int qxetextcasecmp_matching (const Ibyte *s1, Bytecount len1,
			     const Ibyte *s2, Bytecount len2,
			     Charcount *matching);

void buffer_mule_signal_inserted_region (struct buffer *buf, Charbpos start,
					 Bytecount bytelength,
					 Charcount charlength);
void buffer_mule_signal_deleted_region (struct buffer *buf, Charbpos start,
					Charbpos end, Bytebpos byte_start,
					Bytebpos byte_end);

typedef struct
{
  const char *srctext;
  void *dst;
  Bytecount dst_size;
} alloca_convert_vals;

typedef struct
{
  Dynarr_declare (alloca_convert_vals);
} alloca_convert_vals_dynarr;

extern alloca_convert_vals_dynarr *active_alloca_convert;

MODULE_API int find_pos_of_existing_active_alloca_convert (const char *
							   srctext);

/* Defined in unicode.c */
extern const struct sized_memory_description to_unicode_description;
extern const struct sized_memory_description from_unicode_description;
void init_charset_unicode_tables (Lisp_Object charset);
void free_charset_unicode_tables (Lisp_Object charset);
void recalculate_unicode_precedence (void);
extern Lisp_Object Qunicode;
extern Lisp_Object Qutf_16, Qutf_8, Qucs_4, Qutf_7, Qutf_32;
#ifdef MEMORY_USAGE_STATS
Bytecount compute_from_unicode_table_size (Lisp_Object charset,
					      struct overhead_stats *stats);
Bytecount compute_to_unicode_table_size (Lisp_Object charset,
					    struct overhead_stats *stats);
#endif /* MEMORY_USAGE_STATS */

/* Defined in undo.c */
EXFUN (Fundo_boundary, 0);

Lisp_Object truncate_undo_list (Lisp_Object, int, int);
void record_extent (Lisp_Object, int);
void record_insert (struct buffer *, Charbpos, Charcount);
void record_delete (struct buffer *, Charbpos, Charcount);
void record_change (struct buffer *, Charbpos, Charcount);

/* Defined in unex*.c */
#ifdef WIN32_NATIVE
int unexec (Ibyte *, Ibyte *, uintptr_t, uintptr_t, uintptr_t);
#else
int unexec (Extbyte *, Extbyte *, uintptr_t, uintptr_t, uintptr_t);
#endif
#ifdef RUN_TIME_REMAP
int run_time_remap (char *);
#endif

/* Defined in vm-limit.c */
void memory_warnings (void *, void (*) (const char *));

/*--------------- prototypes for constant symbols  ------------*/

/* #### We should get rid of this and put the prototypes back up there in
   #### the per-file stuff, where they belong. */

/* Use the following when you have to add a bunch of symbols. */

/*

(defun redo-symbols (beg end)
  "Snarf any symbols out of the region and print them into a temporary buffer,
which is displayed when the function finishes.  The symbols are laid out with
`extern Lisp_Object ' before each one, with as many as can fit on one line
\(the maximum line width is controlled by the constant `max-line-length' in the
code)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let (syms)
      (while (re-search-forward "\\s-\\(Q[A-Za-z_0-9]+\\)" end t)
	(push (match-string 1) syms))
      (setq syms (sort syms #'string-lessp))
      (with-output-to-temp-buffer "*Symbols*"
	(let* ((col 0)
	       (start "extern Lisp_Object ")
	       (startlen (length start))
	       ;; with a default-width frame of 80 chars, you can only fit
	       ;; 79 before wrapping.  you can see this to a lower value if
	       ;; you don't want it right up against the right margin.
	       (max-line-length 79))
	  (dolist (sym syms)
	    (cond (;; if something already on line (this will always be the
		   ;; case except the very first iteration), see what
		   ;; space we've got. (need to take into account 2
		   ;; for the comma+space, 1 for the semicolon at the
		   ;; end.) if enough space, do it.
		   (and (> col 0) (< (+ col (length sym) 2)
				     (1- max-line-length)))
		   (princ ", ")
		   (princ sym)
		   (incf col 2)
		   (incf col (length sym)))
		  (t
		   ;; either we're first iteration or we ran out of space.
		   ;; if the latter, terminate the previous line.  this
		   ;; loop is written on purpose so that it always prints
		   ;; at least one item, even if that would go over.
		   (when (> col 0)
		     (princ ";\n")
		     (setq col 0))
		   (princ start)
		   (incf col startlen)
		   (princ sym)
		   (incf col (length sym)))))
	  ;; finally terminate the last line.
	  (princ ";\n"))))))

*/

extern Lisp_Object Qactivate_menubar_hook, Qand_optional, Qand_rest;
extern Lisp_Object Qarith_error, Qarrayp, Qautoload, Qbackground;
extern Lisp_Object Qbackground_pixmap, Qbeginning_of_buffer, Qbitp, Qblinking;
extern Lisp_Object Qbuffer_glyph_p, Qbuffer_live_p, Qbuffer_read_only;
extern Lisp_Object Qbyte_code, Qcall_interactively, Qcategory_designator_p;
extern Lisp_Object Qcategory_table_value_p, Qcdr, Qchar_or_string_p;
extern Lisp_Object Qcharacterp, Qcircular_list, Qcircular_property_list;
extern Lisp_Object Qcolor_pixmap_image_instance_p, Qcommandp;
extern Lisp_Object Qcompletion_ignore_case, Qconsole_live_p, Qconst_specifier;
extern Lisp_Object Qconversion_error, Qcurrent_menubar;
extern Lisp_Object Qcyclic_variable_indirection, Qdefun, Qdevice_live_p, Qdim;
extern Lisp_Object Qdirection, Qdisabled, Qdisabled_command_hook;
extern Lisp_Object Qdisplay_table, Qdll_error, Qdomain_error, Qediting_error;
extern Lisp_Object Qend_of_buffer, Qend_of_file, Qend_open, Qerror;
extern Lisp_Object Qerror_conditions, Qerror_lacks_explanatory_string;
extern Lisp_Object Qerror_message, Qevent_live_p, Qexit, Qextent_live_p;
extern Lisp_Object Qexternal_debugging_output, Qfeaturep, Qfile_error;
extern Lisp_Object Qfile_name_sans_extension, Qfinal;
extern Lisp_Object Qforeground, Qformat, Qframe_live_p, Qgraphic;
extern Lisp_Object Qgui_error, Qicon_glyph_p, Qidentity, Qinhibit_quit;
extern Lisp_Object Qinhibit_read_only, Qinteger_char_or_marker_p;
extern Lisp_Object Qinteger_or_char_p, Qinteger_or_marker_p;
extern Lisp_Object Qinteractive, Qinternal_error;
extern Lisp_Object Qinvalid_byte_code, Qinvalid_change, Qinvalid_constant;
extern Lisp_Object Qinvalid_function, Qinvalid_operation;
extern Lisp_Object Qinvalid_read_syntax, Qinvalid_state, Qio_error, Qlambda;
extern Lisp_Object Qlayout, Qlist_formation_error, Qlistp, Qload;
extern Lisp_Object Qlong_name, Qmacro, Qmakunbound, Qmalformed_list;
extern Lisp_Object Qmalformed_property_list, Qmark, Qmodule;
extern Lisp_Object Qmono_pixmap_image_instance_p, Qmouse_leave_buffer_hook;
extern Lisp_Object Qnative_layout, Qnatnump, Qnetwork_error, Qno_catch;
extern Lisp_Object Qnonnegativep, Qnothing_image_instance_p;
extern Lisp_Object Qnumber_char_or_marker_p, Qnumberp, Qout_of_memory;
extern Lisp_Object Qoverflow_error, Qpoint, Qpointer_glyph_p;
extern Lisp_Object Qpointer_image_instance_p, Qprint_length;
extern Lisp_Object Qprint_string_length, Qprinting_unreadable_object;
extern Lisp_Object Qprogn, Qquit, Qquote, Qrange_error;
extern Lisp_Object Qread_char, Qread_from_minibuffer;
extern Lisp_Object Qreally_early_error_handler, Qregion_beginning;
extern Lisp_Object Qregion_end, Qregistries, Qreverse_direction_charset;
extern Lisp_Object Qrun_hooks, Qsans_modifiers, Qsave_buffers_kill_emacs;
extern Lisp_Object Qself_insert_command, Qself_insert_defer_undo, Qsequencep;
extern Lisp_Object Qset, Qsetting_constant, Qshort_name, Qsingularity_error;
extern Lisp_Object Qsound_error, Qstack_overflow, Qstandard_input;
extern Lisp_Object Qstandard_output, Qstart_open, Qstring_lessp;
extern Lisp_Object Qstructure_formation_error, Qsubwindow;
extern Lisp_Object Qsubwindow_image_instance_p;
extern Lisp_Object Qtext_conversion_error, Qtext_image_instance_p, Qtop_level;
extern Lisp_Object Qtrue_list_p, Qunderflow_error, Qunderline;
extern Lisp_Object Quser_files_and_directories, Qvalues;
extern Lisp_Object Qvariable_documentation, Qvariable_domain, Qvoid_function;
extern Lisp_Object Qvoid_variable, Qwindow_live_p, Qwrong_number_of_arguments;
extern Lisp_Object Qwrong_type_argument, Qyes_or_no_p;

extern MODULE_API Lisp_Object Qintegerp, Qinvalid_argument, Qprocess_error;
extern MODULE_API Lisp_Object Qsyntax_error, Qt, Qunbound;

#define SYMBOL(fou) extern Lisp_Object fou
#define SYMBOL_MODULE_API(fou) extern MODULE_API Lisp_Object fou
#define SYMBOL_KEYWORD(la_cle_est_fou) extern Lisp_Object la_cle_est_fou
#define SYMBOL_GENERAL(tout_le_monde, est_fou) \
  extern Lisp_Object tout_le_monde

#include "general-slots.h"

#undef SYMBOL
#undef SYMBOL_MODULE_API
#undef SYMBOL_KEYWORD
#undef SYMBOL_GENERAL

/*--------------- prototypes for variables of type Lisp_Object  ------------*/

/* #### We should get rid of this and put the prototypes back up there in
   #### the per-file stuff, where they belong. */

extern Lisp_Object Vactivate_menubar_hook;
extern Lisp_Object Vautoload_queue, Vblank_menubar;
extern Lisp_Object Vcommand_history;
extern Lisp_Object Vcommand_line_args, Vconfigure_info_directory;
extern Lisp_Object Vconfigure_site_directory, Vconfigure_site_module_directory;
extern Lisp_Object Vconsole_list, Vcontrolling_terminal;
extern Lisp_Object Vcurrent_compiled_function_annotation, Vcurrent_load_list;
extern Lisp_Object Vcurrent_mouse_event, Vcurrent_prefix_arg, Vdata_directory;
extern Lisp_Object Vdirectory_sep_char, Vdisabled_command_hook;
extern Lisp_Object Vdoc_directory, Vinternal_doc_file_name;
extern Lisp_Object Vecho_area_buffer, Vemacs_major_version;
extern Lisp_Object Vemacs_minor_version, Vexec_directory, Vexec_path;
extern Lisp_Object Vexecuting_macro, Vfeatures, Vfile_domain;
extern Lisp_Object Vinvocation_directory, Vinvocation_name;
extern Lisp_Object Vlast_command, Vlast_command_char;
extern Lisp_Object Vlast_command_event, Vlast_input_event;
extern Lisp_Object Vload_file_name_internal, Vload_history;
extern Lisp_Object Vload_path, Vmark_even_if_inactive, Vmenubar_configuration;
extern Lisp_Object Vminibuf_preprompt, Vminibuf_prompt, Vminibuffer_zero;
extern Lisp_Object Vmodule_directory, Vmswindows_downcase_file_names;
extern Lisp_Object Vmswindows_get_true_file_attributes, Vobarray;
extern Lisp_Object Vprint_length, Vprint_level, Vprocess_environment;
extern Lisp_Object Vrecent_keys_ring, Vshell_file_name, Vsite_directory;
extern Lisp_Object Vsite_module_directory;
extern Lisp_Object Vstandard_input, Vstandard_output, Vstdio_str;
extern Lisp_Object Vsynchronous_sounds, Vsystem_name;
extern Lisp_Object Vthis_command_keys, Vunread_command_event;
extern Lisp_Object Vx_initial_argv_list;

extern MODULE_API Lisp_Object Vinhibit_quit, Vquit_flag;

END_C_DECLS

#endif /* INCLUDED_lisp_h_ */
