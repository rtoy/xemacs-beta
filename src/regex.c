/* Extended regular expression matching and search library,
   version 0.12, extended for XEmacs.
   (Implements POSIX draft P10003.2/D11.2, except for
   internationalization features.)

   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 2001, 2002, 2003, 2010 Ben Wing.

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
/* Synched up with: FSF 19.29. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

/* We assume non-Mule if emacs isn't defined. */
#ifndef emacs
#undef MULE
#endif

/* XEmacs addition */
#ifdef REL_ALLOC
#define REGEX_REL_ALLOC /* may be undefined below */
#endif

/* XEmacs: define this to add in a speedup for patterns anchored at
   the beginning of a line.  Keep the ifdefs so that it's easier to
   tell where/why this code has diverged from v19. */
#define REGEX_BEGLINE_CHECK

/* XEmacs: the current mmap-based ralloc handles small blocks very
   poorly, so we disable it here. */

#if defined (HAVE_MMAP) || defined (DOUG_LEA_MALLOC)
# undef REGEX_REL_ALLOC
#endif

/* The `emacs' switch turns on certain matching commands
   that make sense only in Emacs. */
#ifdef emacs

#include "lisp.h"
#include "buffer.h"
#include "syntax.h"

#if (defined (DEBUG_XEMACS) && !defined (DEBUG))
#define DEBUG
#endif

#define RE_TRANSLATE_1(ch) TRT_TABLE_OF (translate, (Ichar) ch)
#define TRANSLATE_P(tr) (!NILP (tr))

/* Converts the pointer to the char to BEG-based offset from the start.	 */
#define PTR_TO_OFFSET(d) (MATCHING_IN_FIRST_STRING			\
			  ? (d) - string1 : (d) - (string2 - size1))

#else  /* not emacs */

#include <stdlib.h>
#include <sys/types.h>
#include <stddef.h> /* needed for ptrdiff_t under Solaris */
#include <string.h>

#include "compiler.h"   /* Get compiler-specific definitions like UNUSED */

#define ABORT abort

/* If we are not linking with Emacs proper,
   we can't use the relocating allocator
   even if config.h says that we can.  */
#undef REGEX_REL_ALLOC

/* defined in lisp.h */
#ifdef REGEX_MALLOC
#ifndef DECLARE_NOTHING
#define DECLARE_NOTHING struct nosuchstruct
#endif
#endif

#define itext_ichar(str)				((Ichar) (str)[0])
#define itext_ichar_fmt(str, fmt, object)		((Ichar) (str)[0])
#define itext_ichar_ascii_fmt(str, fmt, object)	((Ichar) (str)[0])

#if (LONGBITS > INTBITS)
# define EMACS_INT long
#else
# define EMACS_INT int
#endif

typedef int Ichar;

#define INC_IBYTEPTR(p) ((p)++)
#define INC_IBYTEPTR_FMT(p, fmt) ((p)++)
#define DEC_IBYTEPTR(p) ((p)--)
#define DEC_IBYTEPTR_FMT(p, fmt) ((p)--)
#define MAX_ICHAR_LEN 1
#define itext_ichar_len(ptr) 1
#define itext_ichar_len_fmt(ptr, fmt) 1

/* Define the syntax stuff for \<, \>, etc.  */

/* This must be nonzero for the wordchar and notwordchar pattern
   commands in re_match_2.  */
#ifndef Sword
#define Sword 1
#endif

#ifdef SYNTAX_TABLE

extern char *re_syntax_table;

#else /* not SYNTAX_TABLE */

/* How many characters in the character set.  */
#define CHAR_SET_SIZE 256

static char re_syntax_table[CHAR_SET_SIZE];

static void
init_syntax_once (void)
{
  static int done = 0;

  if (!done)
    {
      const char *word_syntax_chars =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";

      memset (re_syntax_table, 0, sizeof (re_syntax_table));

      while (*word_syntax_chars)
	re_syntax_table[(unsigned int) (*word_syntax_chars++)] = Sword;

      done = 1;
    }
}

#endif /* SYNTAX_TABLE */

#define SYNTAX(ignored, c) re_syntax_table[c]
#undef SYNTAX_FROM_CACHE
#define SYNTAX_FROM_CACHE SYNTAX

#define RE_TRANSLATE_1(c) translate[(unsigned char) (c)]
#define TRANSLATE_P(tr) tr

#endif /* emacs */

/* This is for other GNU distributions with internationalized messages.  */
#if defined (I18N3) && (defined (HAVE_LIBINTL_H) || defined (_LIBC))
# include <libintl.h>
#else
# define gettext(msgid) (msgid)
#endif


/* Get the interface, including the syntax bits.  */
#include "regex.h"

/* isalpha etc. are used for the character classes.  */
#include <ctype.h>

#ifndef emacs /* For the emacs build, we need these in the header. */

/* 1 if C is an ASCII character.  */
#define ISASCII(c) ((c) < 0200)

/* 1 if C is a unibyte character.  */
#define ISUNIBYTE(c) 0

#ifdef isblank
# define ISBLANK(c) isblank (c)
#else
# define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#endif
#ifdef isgraph
# define ISGRAPH(c) isgraph (c)
#else
# define ISGRAPH(c) (isprint (c) && !isspace (c))
#endif

/* Solaris defines ISPRINT so we must undefine it first.  */
#undef ISPRINT
#define ISPRINT(c) isprint (c)
#define ISDIGIT(c) isdigit (c)
#define ISALNUM(c) isalnum (c)
#define ISALPHA(c) isalpha (c)
#define ISCNTRL(c) iscntrl (c)
#define ISLOWER(c) islower (c)
#define ISPUNCT(c) ispunct (c)
#define ISSPACE(c) isspace (c)
#define ISUPPER(c) isupper (c)
#define ISXDIGIT(c) isxdigit (c)

#define ISWORD(c) ISALPHA (c)

#ifdef _tolower
# define TOLOWER(c) _tolower (c)
#else
# define TOLOWER(c) tolower (c)
#endif

#endif /* emacs */

#ifndef NULL
#define NULL (void *)0
#endif

/* We remove any previous definition of `SIGN_EXTEND_CHAR',
   since ours (we hope) works properly with all combinations of
   machines, compilers, `char' and `unsigned char' argument types.
   (Per Bothner suggested the basic approach.)  */
#undef SIGN_EXTEND_CHAR
#if __STDC__
#define SIGN_EXTEND_CHAR(c) ((signed char) (c))
#else  /* not __STDC__ */
/* As in Harbison and Steele.  */
#define SIGN_EXTEND_CHAR(c) ((((unsigned char) (c)) ^ 128) - 128)
#endif

/* Should we use malloc or alloca?  If REGEX_MALLOC is not defined, we
   use `alloca' instead of `malloc'.  This is because using malloc in
   re_search* or re_match* could cause memory leaks when C-g is used in
   Emacs; also, malloc is slower and causes storage fragmentation.  On
   the other hand, malloc is more portable, and easier to debug.

   Because we sometimes use alloca, some routines have to be macros,
   not functions -- `alloca'-allocated space disappears at the end of the
   function it is called in.  */

#ifndef emacs
#define ALLOCA alloca
#define xmalloc malloc
#define xrealloc realloc
#define xfree free
#endif

#ifdef emacs
#define ALLOCA_GARBAGE_COLLECT()		\
do						\
{						\
  if (need_to_check_c_alloca)			\
    xemacs_c_alloca (0);			\
} while (0)
#elif defined (C_ALLOCA)
#define ALLOCA_GARBAGE_COLLECT() alloca (0)
#else
#define ALLOCA_GARBAGE_COLLECT()
#endif

#ifndef emacs
/* So we can use just it to conditionalize on */
#undef ERROR_CHECK_MALLOC
#endif

#ifdef ERROR_CHECK_MALLOC
/* When REL_ALLOC, malloc() is problematic because it could potentially
   cause all rel-alloc()ed data -- including buffer text -- to be relocated.
   We deal with this by checking for such relocation whenever we have
   executed a statement that may call malloc() -- or alloca(), which may
   end up calling malloc() in some circumstances -- and recomputing all
   of our string pointers in re_match_2_internal() and re_search_2().
   However, if malloc() or alloca() happens and we don't know about it,
   we could still be screwed.  So we set up a system where we indicate all
   places where we are prepared for malloc() or alloca(), and in any
   other circumstances, calls to those functions (from anywhere inside of
   XEmacs!) will ABORT().  We do this even when REL_ALLOC is not defined
   so that we catch these problems sooner, since many developers and beta
   testers will not be running with REL_ALLOC. */
int regex_malloc_disallowed;
#define BEGIN_REGEX_MALLOC_OK() regex_malloc_disallowed = 0
#define END_REGEX_MALLOC_OK() regex_malloc_disallowed = 1
#define UNBIND_REGEX_MALLOC_CHECK() unbind_to (depth)
#else
#define BEGIN_REGEX_MALLOC_OK()
#define END_REGEX_MALLOC_OK()
#define UNBIND_REGEX_MALLOC_CHECK()
#endif


#ifdef REGEX_MALLOC

#define REGEX_ALLOCATE xmalloc
#define REGEX_REALLOCATE(source, osize, nsize) xrealloc (source, nsize)
#define REGEX_FREE xfree

#else /* not REGEX_MALLOC  */

/* Emacs already defines alloca, sometimes.  */
#ifndef alloca

/* Make alloca work the best possible way.  */
#ifdef __GNUC__
#define alloca __builtin_alloca
#elif defined (__DECC) /* XEmacs: added next 3 lines, similar to config.h.in */
#include <alloca.h>
#pragma intrinsic(alloca)
#else /* not __GNUC__ */
#if HAVE_ALLOCA_H
#include <alloca.h>
#else /* not __GNUC__ or HAVE_ALLOCA_H */
#ifndef _AIX /* Already did AIX, up at the top.  */
void *alloca ();
#endif /* not _AIX */
#endif /* HAVE_ALLOCA_H */
#endif /* __GNUC__ */

#endif /* not alloca */

#define REGEX_ALLOCATE ALLOCA

  /* !!#### Needs review */
/* Assumes a `char *destination' variable.  */
#define REGEX_REALLOCATE(source, osize, nsize)				\
  (destination = (char *) ALLOCA (nsize),				\
   memmove (destination, source, osize),				\
   destination)

/* No need to do anything to free, after alloca.
   Do nothing!  But inhibit gcc warning.  */
#define REGEX_FREE(arg,type) ((void)0)

#endif /* REGEX_MALLOC */

/* Define how to allocate the failure stack.  */

#ifdef REGEX_REL_ALLOC
#define REGEX_ALLOCATE_STACK(size)				\
  r_alloc ((unsigned char **) &failure_stack_ptr, (size))
#define REGEX_REALLOCATE_STACK(source, osize, nsize)		\
  r_re_alloc ((unsigned char **) &failure_stack_ptr, (nsize))
#define REGEX_FREE_STACK(ptr)					\
  r_alloc_free ((unsigned char **) &failure_stack_ptr)

#else /* not REGEX_REL_ALLOC */

#ifdef REGEX_MALLOC

#define REGEX_ALLOCATE_STACK xmalloc
#define REGEX_REALLOCATE_STACK(source, osize, nsize) xrealloc (source, nsize)
#define REGEX_FREE_STACK(arg) xfree (arg)

#else /* not REGEX_MALLOC */

#define REGEX_ALLOCATE_STACK ALLOCA

#define REGEX_REALLOCATE_STACK(source, osize, nsize)			\
   REGEX_REALLOCATE (source, osize, nsize)
/* No need to explicitly free anything.  */
#define REGEX_FREE_STACK(arg)

#endif /* REGEX_MALLOC */
#endif /* REGEX_REL_ALLOC */


/* True if `size1' is non-NULL and PTR is pointing anywhere inside
   `string1' or just past its end.  This works if PTR is NULL, which is
   a good thing.  */
#define FIRST_STRING_P(ptr) 					\
  (size1 && string1 <= (ptr) && (ptr) <= string1 + size1)

/* (Re)Allocate N items of type T using malloc, or fail.  */
#define TALLOC(n, t) ((t *) xmalloc ((n) * sizeof (t)))
#define RETALLOC(addr, n, t) ((addr) = (t *) xrealloc (addr, (n) * sizeof (t)))
#define REGEX_TALLOC(n, t) ((t *) REGEX_ALLOCATE ((n) * sizeof (t)))

#define BYTEWIDTH 8 /* In bits.  */

#define STREQ(s1, s2) (strcmp (s1, s2) == 0)

#undef MAX
#undef MIN
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

/* Type of source-pattern and string chars.  */
typedef const unsigned char re_char;

typedef char re_bool;
#define false 0
#define true 1


#ifdef emacs

#ifdef MULE

Lisp_Object Vthe_lisp_rangetab;

void
vars_of_regex (void)
{
  Vthe_lisp_rangetab = Fmake_range_table (Qstart_closed_end_closed);
  staticpro (&Vthe_lisp_rangetab);
}

#else /* not MULE */

void
vars_of_regex (void)
{
}

#endif /* MULE */

/* Convert an offset from the start of the logical text string formed by
   concatenating the two strings together into a character position in the
   Lisp buffer or string that the text represents.  Knows that
   when handling buffer text, the "string" we're passed in is always
   BEGV - ZV. */

static Charxpos
offset_to_charxpos (Lisp_Object lispobj, int off)
{
  if (STRINGP (lispobj))
    return string_index_byte_to_char (lispobj, off);
  else if (BUFFERP (lispobj))
    return bytebpos_to_charbpos (XBUFFER (lispobj),
				 off + BYTE_BUF_BEGV (XBUFFER (lispobj)));
  else
    return 0;
}

#ifdef REL_ALLOC

/* STRING1 is the value of STRING1 given to re_match_2().  LISPOBJ is
   the Lisp object (if any) from which the string is taken.  If LISPOBJ
   is a buffer, return a relocation offset to be added to all pointers to
   string data so that they will be accurate again, after an allocation or
   reallocation that potentially relocated the buffer data.
*/
static inline Bytecount
offset_post_relocation (Lisp_Object lispobj, Ibyte *orig_buftext)
{
  if (!BUFFERP (lispobj))
    return 0;
  return (BYTE_BUF_BYTE_ADDRESS (XBUFFER (lispobj),
				 BYTE_BUF_BEGV (XBUFFER (lispobj))) -
	  orig_buftext);
}

#endif /* REL_ALLOC */

#ifdef ERROR_CHECK_MALLOC

/* NOTE that this can run malloc() so you need to adjust afterwards. */

static int
bind_regex_malloc_disallowed (int value)
{
  /* Tricky, because the act of binding can run malloc(). */
  int old_regex_malloc_disallowed = regex_malloc_disallowed;
  int depth;
  regex_malloc_disallowed = 0;
  depth = record_unwind_protect_restoring_int (&regex_malloc_disallowed,
					       old_regex_malloc_disallowed);
  regex_malloc_disallowed = value;
  return depth;
}

#endif /* ERROR_CHECK_MALLOC */

#endif /* emacs */


/* These are the command codes that appear in compiled regular
   expressions.  Some opcodes are followed by argument bytes.  A
   command code can specify any interpretation whatsoever for its
   arguments.  Zero bytes may appear in the compiled regular expression.  */

typedef enum
{
  no_op = 0,

  /* Succeed right away--no more backtracking.  */
  succeed,

        /* Followed by one byte giving n, then by n literal bytes.  */
  exactn,

        /* Matches any (more or less) character.  */
  anychar,

        /* Matches any one char belonging to specified set.  First
           following byte is number of bitmap bytes.  Then come bytes
           for a bitmap saying which chars are in.  Bits in each byte
           are ordered low-bit-first.  A character is in the set if its
           bit is 1.  A character too large to have a bit in the map is
           automatically not in the set.  */
  charset,

        /* Same parameters as charset, but match any character that is
           not one of those specified.  */
  charset_not,

        /* Start remembering the text that is matched, for storing in a
           register.  Followed by one byte with the register number, in
           the range 1 to the pattern buffer's re_ngroups
           field.  Then followed by one byte with the number of groups
           inner to this one.  (This last has to be part of the
           start_memory only because we need it in the on_failure_jump
           of re_match_2.)  */
  start_memory,

        /* Stop remembering the text that is matched and store it in a
           memory register.  Followed by one byte with the register
           number, in the range 1 to `re_ngroups' in the
           pattern buffer, and one byte with the number of inner groups,
           just like `start_memory'.  (We need the number of inner
           groups here because we don't have any easy way of finding the
           corresponding start_memory when we're at a stop_memory.)  */
  stop_memory,

        /* Match a duplicate of something remembered. Followed by one
           byte containing the register number.  */
  duplicate,

        /* Fail unless at beginning of line.  */
  begline,

        /* Fail unless at end of line.  */
  endline,

        /* Succeeds if at beginning of buffer (if emacs) or at beginning
           of string to be matched (if not).  */
  begbuf,

        /* Analogously, for end of buffer/string.  */
  endbuf,

        /* Followed by two byte relative address to which to jump.  */
  jump,

	/* Same as jump, but marks the end of an alternative.  */
  jump_past_alt,

        /* Followed by two-byte relative address of place to resume at
           in case of failure.  */
  on_failure_jump,

        /* Like on_failure_jump, but pushes a placeholder instead of the
           current string position when executed.  */
  on_failure_keep_string_jump,

        /* Throw away latest failure point and then jump to following
           two-byte relative address.  */
  pop_failure_jump,

        /* Change to pop_failure_jump if know won't have to backtrack to
           match; otherwise change to jump.  This is used to jump
           back to the beginning of a repeat.  If what follows this jump
           clearly won't match what the repeat does, such that we can be
           sure that there is no use backtracking out of repetitions
           already matched, then we change it to a pop_failure_jump.
           Followed by two-byte address.  */
  maybe_pop_jump,

        /* Jump to following two-byte address, and push a dummy failure
           point. This failure point will be thrown away if an attempt
           is made to use it for a failure.  A `+' construct makes this
           before the first repeat.  Also used as an intermediary kind
           of jump when compiling an alternative.  */
  dummy_failure_jump,

	/* Push a dummy failure point and continue.  Used at the end of
	   alternatives.  */
  push_dummy_failure,

        /* Followed by two-byte relative address and two-byte number n.
           After matching N times, jump to the address upon failure.  */
  succeed_n,

        /* Followed by two-byte relative address, and two-byte number n.
           Jump to the address N times, then fail.  */
  jump_n,

        /* Set the following two-byte relative address to the
           subsequent two-byte number.  The address *includes* the two
           bytes of number.  */
  set_number_at,

  wordchar,	/* Matches any word-constituent character.  */
  notwordchar,	/* Matches any char that is not a word-constituent.  */

  wordbeg,	/* Succeeds if at word beginning.  */
  wordend,	/* Succeeds if at word end.  */

  wordbound,	/* Succeeds if at a word boundary.  */
  notwordbound	/* Succeeds if not at a word boundary.  */

#ifdef emacs
  ,before_dot,	/* Succeeds if before point.  */
  at_dot,	/* Succeeds if at point.  */
  after_dot,	/* Succeeds if after point.  */

	/* Matches any character whose syntax is specified.  Followed by
           a byte which contains a syntax code, e.g., Sword.  */
  syntaxspec,

	/* Matches any character whose syntax is not that specified.  */
  notsyntaxspec

#endif /* emacs */

#ifdef MULE
    /* need extra stuff to be able to properly work with XEmacs/Mule
       characters (which may take up more than one byte) */

  ,charset_mule, /* Matches any character belonging to specified set.
		    The set is stored in "unified range-table
		    format"; see rangetab.c.  Unlike the `charset'
		    opcode, this can handle arbitrary characters. */

  charset_mule_not   /* Same parameters as charset_mule, but match any
			character that is not one of those specified.  */

  /* 97/2/17 jhod: The following two were merged back in from the Mule
     2.3 code to enable some language specific processing */
  ,categoryspec,     /* Matches entries in the character category tables */
  notcategoryspec    /* The opposite of the above */
#endif /* MULE */

} re_opcode_t;

/* Common operations on the compiled pattern.  */

/* Store NUMBER in two contiguous bytes starting at DESTINATION.  */

#define STORE_NUMBER(destination, number)				\
  do {									\
    (destination)[0] = (number) & 0377;					\
    (destination)[1] = (number) >> 8;					\
  } while (0)

/* Same as STORE_NUMBER, except increment DESTINATION to
   the byte after where the number is stored.  Therefore, DESTINATION
   must be an lvalue.  */

#define STORE_NUMBER_AND_INCR(destination, number)			\
  do {									\
    STORE_NUMBER (destination, number);					\
    (destination) += 2;							\
  } while (0)

/* Put into DESTINATION a number stored in two contiguous bytes starting
   at SOURCE.  */

#define EXTRACT_NUMBER(destination, source)				\
  do {									\
    (destination) = *(source) & 0377;					\
    (destination) += SIGN_EXTEND_CHAR (*((source) + 1)) << 8;		\
  } while (0)

#ifdef DEBUG
static void
extract_number (int *dest, re_char *source)
{
  int temp = SIGN_EXTEND_CHAR (*(source + 1));
  *dest = *source & 0377;
  *dest += temp << 8;
}

#ifndef EXTRACT_MACROS /* To debug the macros.  */
#undef EXTRACT_NUMBER
#define EXTRACT_NUMBER(dest, src) extract_number (&dest, src)
#endif /* not EXTRACT_MACROS */

#endif /* DEBUG */

/* Same as EXTRACT_NUMBER, except increment SOURCE to after the number.
   SOURCE must be an lvalue.  */

#define EXTRACT_NUMBER_AND_INCR(destination, source)			\
  do {									\
    EXTRACT_NUMBER (destination, source);				\
    (source) += 2; 							\
  } while (0)

#ifdef DEBUG
static void
extract_number_and_incr (int *destination, unsigned char **source)
{
  extract_number (destination, *source);
  *source += 2;
}

#ifndef EXTRACT_MACROS
#undef EXTRACT_NUMBER_AND_INCR
#define EXTRACT_NUMBER_AND_INCR(dest, src) \
  extract_number_and_incr (&dest, &src)
#endif /* not EXTRACT_MACROS */

#endif /* DEBUG */

/* If DEBUG is defined, Regex prints many voluminous messages about what
   it is doing (if the variable `debug' is nonzero).  If linked with the
   main program in `iregex.c', you can enter patterns and strings
   interactively.  And if linked with the main program in `main.c' and
   the other test files, you can run the already-written tests.  */

#if defined (DEBUG)

/* We use standard I/O for debugging.  */
#include <stdio.h>

#ifndef emacs
/* XEmacs provides its own version of assert() */
/* It is useful to test things that ``must'' be true when debugging.  */
#include <assert.h>
#endif

extern int debug_regexps;

#define DEBUG_STATEMENT(e) e
#define DEBUG_RUNTIME_FLAGS debug_regexps

#define DEBUG_PRINT1(x) if (debug_regexps) printf (x)
#define DEBUG_PRINT2(x1, x2) if (debug_regexps) printf (x1, x2)
#define DEBUG_PRINT3(x1, x2, x3) if (debug_regexps) printf (x1, x2, x3)
#define DEBUG_PRINT4(x1, x2, x3, x4) if (debug_regexps) printf (x1, x2, x3, x4)
#define DEBUG_PRINT_COMPILED_PATTERN(p, s, e) 				\
  if (debug_regexps) print_partial_compiled_pattern (s, e)
#define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)			\
  if (debug_regexps) print_double_string (w, s1, sz1, s2, sz2)

#define DEBUG_FAIL_PRINT1(x) \
  if (debug_regexps & RE_DEBUG_FAILURE_POINT) printf (x)
#define DEBUG_FAIL_PRINT2(x1, x2) \
  if (debug_regexps & RE_DEBUG_FAILURE_POINT) printf (x1, x2)
#define DEBUG_FAIL_PRINT3(x1, x2, x3) \
  if (debug_regexps & RE_DEBUG_FAILURE_POINT) printf (x1, x2, x3)
#define DEBUG_FAIL_PRINT4(x1, x2, x3, x4) \
  if (debug_regexps & RE_DEBUG_FAILURE_POINT) printf (x1, x2, x3, x4)
#define DEBUG_FAIL_PRINT_COMPILED_PATTERN(p, s, e)	\
  if (debug_regexps & RE_DEBUG_FAILURE_POINT)		\
    print_partial_compiled_pattern (s, e)
#define DEBUG_FAIL_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)	\
  if (debug_regexps & RE_DEBUG_FAILURE_POINT)			\
    print_double_string (w, s1, sz1, s2, sz2)

#define DEBUG_MATCH_PRINT1(x) \
  if (debug_regexps & RE_DEBUG_MATCHING) printf (x)
#define DEBUG_MATCH_PRINT2(x1, x2) \
  if (debug_regexps & RE_DEBUG_MATCHING) printf (x1, x2)
#define DEBUG_MATCH_PRINT3(x1, x2, x3) \
  if (debug_regexps & RE_DEBUG_MATCHING) printf (x1, x2, x3)
#define DEBUG_MATCH_PRINT4(x1, x2, x3, x4) \
  if (debug_regexps & RE_DEBUG_MATCHING) printf (x1, x2, x3, x4)
#define DEBUG_MATCH_PRINT_COMPILED_PATTERN(p, s, e)	\
  if (debug_regexps & RE_DEBUG_MATCHING)		\
    print_partial_compiled_pattern (s, e)
#define DEBUG_MATCH_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)	\
  if (debug_regexps & RE_DEBUG_MATCHING)			\
    print_double_string (w, s1, sz1, s2, sz2)


/* Print the fastmap in human-readable form.  */

static void
print_fastmap (char *fastmap)
{
  int was_a_range = 0;
  int i = 0;

  while (i < (1 << BYTEWIDTH))
    {
      if (fastmap[i++])
	{
	  was_a_range = 0;
          putchar (i - 1);
          while (i < (1 << BYTEWIDTH)  &&  fastmap[i])
            {
              was_a_range = 1;
              i++;
            }
	  if (was_a_range)
            {
              putchar ('-');
              putchar (i - 1);
            }
        }
    }
  putchar ('\n');
}


/* Print a compiled pattern string in human-readable form, starting at
   the START pointer into it and ending just before the pointer END.  */

static void
print_partial_compiled_pattern (re_char *start, re_char *end)
{
  int mcnt, mcnt2;
  unsigned char *p = (unsigned char *) start;
  re_char *pend = end;

  if (start == NULL)
    {
      puts ("(null)");
      return;
    }

  /* Loop over pattern commands.  */
  while (p < pend)
    {
      printf ("%ld:\t", (long)(p - start));

      switch ((re_opcode_t) *p++)
	{
        case no_op:
          printf ("/no_op");
          break;

	case exactn:
	  mcnt = *p++;
          printf ("/exactn/%d", mcnt);
          while (mcnt--)
	    {
	      putchar ('/');
	      putchar (*p++);
            }
          break;

	case start_memory:
          mcnt = *p++;
          printf ("/start_memory/%d/%d", mcnt, *p++);
          break;

	case stop_memory:
          mcnt = *p++;
	  printf ("/stop_memory/%d/%d", mcnt, *p++);
          break;

	case duplicate:
	  printf ("/duplicate/%d", *p++);
	  break;

	case anychar:
	  printf ("/anychar");
	  break;

	case charset:
        case charset_not:
          {
            REGISTER int c, last = -100;
	    REGISTER int in_range = 0;

	    printf ("/charset [%s",
	            (re_opcode_t) *(p - 1) == charset_not ? "^" : "");

            assert (p + *p < pend);

            for (c = 0; c < 256; c++)
	      if (((unsigned char) (c / 8) < *p)
		  && (p[1 + (c/8)] & (1 << (c % 8))))
		{
		  /* Are we starting a range?  */
		  if (last + 1 == c && ! in_range)
		    {
		      putchar ('-');
		      in_range = 1;
		    }
		  /* Have we broken a range?  */
		  else if (last + 1 != c && in_range)
		    {
		      putchar (last);
		      in_range = 0;
		    }

		  if (! in_range)
		    putchar (c);

		  last = c;
              }

	    if (in_range)
	      putchar (last);

	    putchar (']');

	    p += 1 + *p;
	  }
	  break;

#ifdef MULE
	case charset_mule:
        case charset_mule_not:
          {
	    int nentries, i;

	    printf ("/charset_mule [%s",
	            (re_opcode_t) *(p - 1) == charset_mule_not ? "^" : "");
	    printf (" flags: 0x%02x ", *p++);
	    nentries = unified_range_table_nentries (p);
	    for (i = 0; i < nentries; i++)
	      {
		EMACS_INT first, last;
		Lisp_Object dummy_val;

		unified_range_table_get_range (p, i, &first, &last,
					       &dummy_val);
		if (first < 0x80)
		  putchar (first);
		else
		  printf ("(0x%lx)", (long)first);
		if (first != last)
		  {
		    putchar ('-');
		    if (last < 0x80)
		      putchar (last);
		    else
		      printf ("(0x%lx)", (long)last);
		  }
	      }
	    putchar (']');
	    p += unified_range_table_bytes_used (p);
	  }
	  break;
#endif

	case begline:
	  printf ("/begline");
          break;

	case endline:
          printf ("/endline");
          break;

	case on_failure_jump:
          extract_number_and_incr (&mcnt, &p);
  	  printf ("/on_failure_jump to %ld", (long)(p + mcnt - start));
          break;

	case on_failure_keep_string_jump:
          extract_number_and_incr (&mcnt, &p);
  	  printf ("/on_failure_keep_string_jump to %ld", (long)(p + mcnt - start));
          break;

	case dummy_failure_jump:
          extract_number_and_incr (&mcnt, &p);
  	  printf ("/dummy_failure_jump to %ld", (long)(p + mcnt - start));
          break;

	case push_dummy_failure:
          printf ("/push_dummy_failure");
          break;

        case maybe_pop_jump:
          extract_number_and_incr (&mcnt, &p);
  	  printf ("/maybe_pop_jump to %ld", (long)(p + mcnt - start));
	  break;

        case pop_failure_jump:
	  extract_number_and_incr (&mcnt, &p);
  	  printf ("/pop_failure_jump to %ld", (long)(p + mcnt - start));
	  break;

        case jump_past_alt:
	  extract_number_and_incr (&mcnt, &p);
  	  printf ("/jump_past_alt to %ld", (long)(p + mcnt - start));
	  break;

        case jump:
	  extract_number_and_incr (&mcnt, &p);
  	  printf ("/jump to %ld", (long)(p + mcnt - start));
	  break;

        case succeed_n:
          extract_number_and_incr (&mcnt, &p);
          extract_number_and_incr (&mcnt2, &p);
	  printf ("/succeed_n to %ld, %d times", (long)(p + mcnt - start), mcnt2);
          break;

        case jump_n:
          extract_number_and_incr (&mcnt, &p);
          extract_number_and_incr (&mcnt2, &p);
	  printf ("/jump_n to %ld, %d times", (long)(p + mcnt - start), mcnt2);
          break;

        case set_number_at:
          extract_number_and_incr (&mcnt, &p);
          extract_number_and_incr (&mcnt2, &p);
	  printf ("/set_number_at location %ld to %d", (long)(p + mcnt - start), mcnt2);
          break;

        case wordbound:
	  printf ("/wordbound");
	  break;

	case notwordbound:
	  printf ("/notwordbound");
          break;

	case wordbeg:
	  printf ("/wordbeg");
	  break;

	case wordend:
	  printf ("/wordend");

#ifdef emacs
	case before_dot:
	  printf ("/before_dot");
          break;

	case at_dot:
	  printf ("/at_dot");
          break;

	case after_dot:
	  printf ("/after_dot");
          break;

	case syntaxspec:
          printf ("/syntaxspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
          break;

	case notsyntaxspec:
          printf ("/notsyntaxspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;

#ifdef MULE
/* 97/2/17 jhod Mule category patch */
	case categoryspec:
	  printf ("/categoryspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;

	case notcategoryspec:
	  printf ("/notcategoryspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;
/* end of category patch */
#endif /* MULE */
#endif /* emacs */

	case wordchar:
	  printf ("/wordchar");
          break;

	case notwordchar:
	  printf ("/notwordchar");
          break;

	case begbuf:
	  printf ("/begbuf");
          break;

	case endbuf:
	  printf ("/endbuf");
          break;

        default:
          printf ("?%d", *(p-1));
	}

      putchar ('\n');
    }

  printf ("%ld:\tend of pattern.\n", (long)(p - start));
}


static void
print_compiled_pattern (struct re_pattern_buffer *bufp)
{
  re_char *buffer = bufp->buffer;

  print_partial_compiled_pattern (buffer, buffer + bufp->used);
  printf ("%ld bytes used/%ld bytes allocated.\n", bufp->used,
	  bufp->allocated);

  if (bufp->fastmap_accurate && bufp->fastmap)
    {
      printf ("fastmap: ");
      print_fastmap (bufp->fastmap);
    }

  printf ("re_nsub: %ld\t", (long)bufp->re_nsub);
  printf ("re_ngroups: %ld\t", (long)bufp->re_ngroups);
  printf ("regs_alloc: %d\t", bufp->regs_allocated);
  printf ("can_be_null: %d\t", bufp->can_be_null);
  printf ("newline_anchor: %d\n", bufp->newline_anchor);
  printf ("no_sub: %d\t", bufp->no_sub);
  printf ("not_bol: %d\t", bufp->not_bol);
  printf ("not_eol: %d\t", bufp->not_eol);
  printf ("syntax: %d\n", bufp->syntax);
  /* Perhaps we should print the translate table?  */
  /* and maybe the category table? */

  if (bufp->external_to_internal_register)
    {
      int i;

      printf ("external_to_internal_register:\n");
      for (i = 0; i <= bufp->re_nsub; i++)
	{
	  if (i > 0)
	    printf (", ");
	  printf ("%d -> %d", i, bufp->external_to_internal_register[i]);
	}
      printf ("\n");
    }
}


static void
print_double_string (re_char *where, re_char *string1, int size1,
		     re_char *string2, int size2)
{
  if (where == NULL)
    printf ("(null)");
  else
    {
      int this_char;

      if (FIRST_STRING_P (where))
        {
          for (this_char = where - string1; this_char < size1; this_char++)
            putchar (string1[this_char]);

          where = string2;
        }

      for (this_char = where - string2; this_char < size2; this_char++)
        putchar (string2[this_char]);
    }
}

#else /* not DEBUG */

#ifndef emacs
#undef assert
#define assert(e) ((void) (1))
#define malloc_checking_assert assert
#endif

#define DEBUG_STATEMENT(e)
#define DEBUG_RUNTIME_FLAGS 0

#define DEBUG_PRINT1(x)
#define DEBUG_PRINT2(x1, x2)
#define DEBUG_PRINT3(x1, x2, x3)
#define DEBUG_PRINT4(x1, x2, x3, x4)
#define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)
#define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)

#define DEBUG_FAIL_PRINT1(x)
#define DEBUG_FAIL_PRINT2(x1, x2)
#define DEBUG_FAIL_PRINT3(x1, x2, x3)
#define DEBUG_FAIL_PRINT4(x1, x2, x3, x4)
#define DEBUG_FAIL_PRINT_COMPILED_PATTERN(p, s, e)
#define DEBUG_FAIL_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)

#define DEBUG_MATCH_PRINT1(x)
#define DEBUG_MATCH_PRINT2(x1, x2)
#define DEBUG_MATCH_PRINT3(x1, x2, x3)
#define DEBUG_MATCH_PRINT4(x1, x2, x3, x4)
#define DEBUG_MATCH_PRINT_COMPILED_PATTERN(p, s, e)
#define DEBUG_MATCH_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)

#endif /* DEBUG */

/* Set by `re_set_syntax' to the current regexp syntax to recognize.  Can
   also be assigned to arbitrarily: each pattern buffer stores its own
   syntax, so it can be changed between regex compilations.  */
/* This has no initializer because initialized variables in Emacs
   become read-only after dumping.  */
reg_syntax_t re_syntax_options;


/* Specify the precise syntax of regexps for compilation.  This provides
   for compatibility for various utilities which historically have
   different, incompatible syntaxes.

   The argument SYNTAX is a bit mask comprised of the various bits
   defined in regex.h.  We return the old syntax.  */

reg_syntax_t
re_set_syntax (reg_syntax_t syntax)
{
  reg_syntax_t ret = re_syntax_options;

  re_syntax_options = syntax;
  return ret;
}

/* This table gives an error message for each of the error codes listed
   in regex.h.  Obviously the order here has to be same as there.
   POSIX doesn't require that we do anything for REG_NOERROR,
   but why not be nice?  */

static const char *re_error_msgid[] =
{
  "Success",					/* REG_NOERROR */
  "No match",					/* REG_NOMATCH */
  "Invalid regular expression",			/* REG_BADPAT */
  "Invalid collation character",		/* REG_ECOLLATE */
  "Invalid character class name",		/* REG_ECTYPE */
  "Trailing backslash",				/* REG_EESCAPE */
  "Invalid back reference",			/* REG_ESUBREG */
  "Unmatched [ or [^",				/* REG_EBRACK */
  "Unmatched ( or \\(",				/* REG_EPAREN */
  "Unmatched \\{",				/* REG_EBRACE */
  "Invalid content of \\{\\}",			/* REG_BADBR */
  "Invalid range end",				/* REG_ERANGE */
  "Memory exhausted",				/* REG_ESPACE */
  "Invalid preceding regular expression",	/* REG_BADRPT */
  "Premature end of regular expression",	/* REG_EEND */
  "Regular expression too big",			/* REG_ESIZE */
  "Unmatched ) or \\)",				/* REG_ERPAREN */
#ifdef emacs
  "Invalid syntax designator",			/* REG_ESYNTAX */
#endif
#ifdef MULE
  "Ranges may not span charsets",		/* REG_ERANGESPAN */
  "Invalid category designator",		/* REG_ECATEGORY */
#endif
};

/* Avoiding alloca during matching, to placate r_alloc.  */

/* About these various flags:

   MATCH_MAY_ALLOCATE indicates that it's OK to do allocation in the
   searching and matching functions.  In this case, we use local variables
   to hold the values allocated.  If not, we use *global* variables, which
   are pre-allocated.  NOTE: XEmacs ***MUST*** run with MATCH_MAY_ALLOCATE,
   because the regexp routines may get called reentrantly as a result of
   QUIT processing (e.g. under Windows: re_match -> QUIT -> quit_p -> drain
   events -> process WM_INITMENU -> call filter -> re_match; see stack
   trace in signal.c), so we cannot have any global variables (unless we do
   lots of trickiness including some unwind-protects, which isn't worth it
   at this point).

   REL_ALLOC means that the relocating allocator is in use, for buffers
   and such.  REGEX_REL_ALLOC means that we use rel-alloc to manage the
   fail stack, which may grow quite large.  REGEX_MALLOC means we use
   malloc() in place of alloca() to allocate the fail stack -- only
   applicable if REGEX_REL_ALLOC is not defined.
*/

/* Define MATCH_MAY_ALLOCATE unless we need to make sure that the
   searching and matching functions should not call alloca.  On some
   systems, alloca is implemented in terms of malloc, and if we're
   using the relocating allocator routines, then malloc could cause a
   relocation, which might (if the strings being searched are in the
   ralloc heap) shift the data out from underneath the regexp
   routines. [To clarify: The purpose of rel-alloc is to allow data to
   be moved in memory from one place to another so that all data
   blocks can be consolidated together and excess memory released back
   to the operating system.  This requires that all the blocks that
   are managed by rel-alloc go at the very end of the program's heap,
   after all regularly malloc()ed data.  malloc(), however, is used to
   owning the end of the heap, so that when more memory is needed, it
   just expands the heap using sbrk().  This is reconciled by using a
   malloc() (such as malloc.c, gmalloc.c, or recent versions of
   malloc() in libc) where the sbrk() call can be replaced with a
   user-specified call -- in this case, to rel-alloc's r_alloc_sbrk()
   routine.  This routine calls the real sbrk(), but then shifts all
   the rel-alloc-managed blocks forward to the end of the heap again,
   so that malloc() gets the memory it needs in the location it needs
   it at.  The regex routines may well have pointers to buffer data as
   their arguments, and buffers are managed by rel-alloc if rel-alloc
   has been enabled, so calling malloc() may potentially screw things
   up badly if it runs out of space and asks for more from the OS.]

   [[Here's another reason to avoid allocation: Emacs processes input
   from X in a signal handler; processing X input may call malloc; if
   input arrives while a matching routine is calling malloc, then
   we're scrod.  But Emacs can't just block input while calling
   matching routines; then we don't notice interrupts when they come
   in.  So, Emacs blocks input around all regexp calls except the
   matching calls, which it leaves unprotected, in the faith that they
   will not malloc.]] This previous paragraph is irrelevant under XEmacs,
   as we *do not* do anything so stupid as process input from within a
   signal handler.

   However, the regexp routines may get called reentrantly as a result of
   QUIT processing (e.g. under Windows: re_match -> QUIT -> quit_p -> drain
   events -> process WM_INITMENU -> call filter -> re_match; see stack
   trace in signal.c), so we cannot have any global variables (unless we do
   lots of trickiness including some unwind-protects, which isn't worth it
   at this point).  Hence we MUST have MATCH_MAY_ALLOCATE defined.

   Also, the first paragraph does not make complete sense to me -- what
   about the use of rel-alloc to handle the fail stacks?  Shouldn't these
   reallocations potentially cause buffer data to be relocated as well?  I
   must be missing something, though -- perhaps the writer above is
   assuming that the failure stack(s) will always be allocated after the
   buffer data, and thus reallocating them with rel-alloc won't move buffer
   data. (In fact, a cursory glance at the code in ralloc.c seems to
   confirm this.) --ben */

/* Normally, this is fine.  */
#define MATCH_MAY_ALLOCATE

/* When using GNU C, we are not REALLY using the C alloca, no matter
   what config.h may say.  So don't take precautions for it.  */
#ifdef __GNUC__
#undef C_ALLOCA
#endif

/* The match routines may not allocate if (1) they would do it with malloc
   and (2) it's not safe for them to use malloc.
   Note that if REL_ALLOC is defined, matching would not use malloc for the
   failure stack, but we would still use it for the register vectors;
   so REL_ALLOC should not affect this.  */

/* XEmacs can handle REL_ALLOC and malloc() OK */
#if !defined (emacs) && (defined (C_ALLOCA) || defined (REGEX_MALLOC)) && defined (REL_ALLOC)
#undef MATCH_MAY_ALLOCATE
#endif

#if !defined (MATCH_MAY_ALLOCATE) && defined (emacs)
#error regex must be handle reentrancy; MATCH_MAY_ALLOCATE must be defined
#endif


/* Registers are set to a sentinel when they haven't yet matched. This
   declaration is ahead of most of the register-specific stuff in this file
   because its value is examined in the failure stack code. */
static unsigned char reg_unset_dummy;
#define REG_UNSET_VALUE (&reg_unset_dummy)
#define REG_UNSET(e) ((e) == REG_UNSET_VALUE)

/* Failure stack declarations and macros; both re_compile_fastmap and
   re_match_2 use a failure stack.  These have to be macros because of
   REGEX_ALLOCATE_STACK.  */


/* Number of failure points for which to initially allocate space
   when matching.  If this number is exceeded, we allocate more
   space, so it is not a hard limit.  */
#ifndef INIT_FAILURE_ALLOC
#define INIT_FAILURE_ALLOC 20
#endif

/* Roughly the maximum number of failure points on the stack.  Would be
   exactly that if always used MAX_FAILURE_SPACE each time we failed.
   This is a variable only so users of regex can assign to it; we never
   change it ourselves.  */
#if defined (MATCH_MAY_ALLOCATE)
/* 4400 was enough to cause a crash on Alpha OSF/1,
   whose default stack limit is 2mb.  */
int re_max_failures = 40000;
#else
int re_max_failures = 4000;
#endif

union fail_stack_elt
{
  re_char *pointer;
  int integer;
};

typedef union fail_stack_elt fail_stack_elt_t;

typedef struct
{
  fail_stack_elt_t *stack;
  Elemcount size;
  Elemcount avail;			/* Offset of next open position.  */
} fail_stack_type;

#define FAIL_STACK_EMPTY()     (fail_stack.avail == 0)
#define FAIL_STACK_PTR_EMPTY() (fail_stack_ptr->avail == 0)
#define FAIL_STACK_FULL()      (fail_stack.avail == fail_stack.size)


/* Define macros to initialize and free the failure stack.
   Do `return -2' if the alloc fails.  */

#ifdef MATCH_MAY_ALLOCATE
#define INIT_FAIL_STACK()				\
  do {							\
    fail_stack.stack = (fail_stack_elt_t *)		\
      REGEX_ALLOCATE_STACK (INIT_FAILURE_ALLOC *	\
			    sizeof (fail_stack_elt_t));	\
							\
    if (fail_stack.stack == NULL)			\
      {							\
        UNBIND_REGEX_MALLOC_CHECK ();			\
	return -2;					\
      }							\
							\
    fail_stack.size = INIT_FAILURE_ALLOC;		\
    fail_stack.avail = 0;				\
  } while (0)

#define RESET_FAIL_STACK()  REGEX_FREE_STACK (fail_stack.stack)
#else
#define INIT_FAIL_STACK()						\
  do {									\
    fail_stack.avail = 0;						\
  } while (0)

#define RESET_FAIL_STACK()
#endif


/* Double the size of FAIL_STACK, up to approximately `re_max_failures' items.

   Return 1 if succeeds, and 0 if either ran out of memory
   allocating space for it or it was already too large.

   REGEX_REALLOCATE_STACK requires `destination' be declared.   */

#define DOUBLE_FAIL_STACK(fail_stack)					\
  ((fail_stack).size > re_max_failures * MAX_FAILURE_ITEMS		\
   ? 0									\
   : ((fail_stack).stack = (fail_stack_elt_t *)				\
        REGEX_REALLOCATE_STACK ((fail_stack).stack, 			\
          (fail_stack).size * sizeof (fail_stack_elt_t),		\
          ((fail_stack).size << 1) * sizeof (fail_stack_elt_t)),	\
									\
      (fail_stack).stack == NULL					\
      ? 0								\
      : ((fail_stack).size <<= 1, 					\
         1)))

#if !defined (emacs) || !defined (REL_ALLOC)
#define RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS()
#else

/* Update a relocatable pointer to reflect that its associated buffer has
   been relocated. Don't change NULL pointers or registers that have not
   been set. If assertions are turned on, sanity-check the value passed in,
   making sure it reflects buffer data. Appropriate for use both in
   re_match_2_internal() and re_search_2(). */
#define RELOCATE_IF_OK(val) RELOCATE_IF_OK_1 (val, 1)

/* Same as above, but make no assertions about how plausible the value of
   VAL is. */
#define RELOCATE_IF_OK_NO_ASSERT(val) RELOCATE_IF_OK_1 (val, 0)

/* The implementation of RELOCATE_IF_OK() and RELOCATE_IF_OK_NO_ASSERT(). */
#define RELOCATE_IF_OK_1(val, assert_ok)				\
  do									\
    {									\
      if (assert_ok)							\
	{								\
	  malloc_checking_assert					\
	    (((re_char *) (val) >= (re_char *) string1			\
	      && (re_char *) (val) <= (re_char *) string1 + size1)	\
	     || ((re_char *)(val) >= (re_char *) string2		\
		 && (re_char *)(val)					\
		 <= (re_char *) string2 + size2) ||			\
	     (val) == NULL || REG_UNSET ((unsigned char *)(val)));	\
	}								\
      if ((val) != NULL && !REG_UNSET ((unsigned char *)(val)))		\
	{								\
	  (val) += rmdp_offset;						\
	}								\
    } while (0)

/* Within re_match_2_internal(), check whether the current search string
   reflects a Lisp buffer that has just had its text reallocated. If so,
   update the local saved pointer values to reflect the new text
   addresses. The local values in question are the local values within
   re_match_2_internal(), together with regular expression register values
   and those values on the fail stack. */
#define RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS()                      \
  do                                                                    \
    {                                                                   \
      Bytecount rmdp_offset                                             \
        = offset_post_relocation (lispobj, orig_buftext);               \
                                                                        \
      if (rmdp_offset)                                                  \
        {                                                               \
          /* This block will be executed rarely enough that it would be \
             reasonable to make it a non-inline function. However, as a \
             function it would need to take 17 arguments to modify,     \
             which is very unwieldy. */                                 \
          Elemcount ii, jj, high_reg, low_reg;                          \
                                                                        \
          RELOCATE_IF_OK (d);                                           \
          RELOCATE_IF_OK (dend);                                        \
          RELOCATE_IF_OK (end_match_1);                                 \
          RELOCATE_IF_OK (end_match_2);                                 \
          RELOCATE_IF_OK (match_end);                                   \
                                                                        \
          if (bufp->re_ngroups)                                         \
            {                                                           \
              /* Register zero is managed specially, see the code in    \
                 succeed_label. */                                      \
              for (ii = 1; ii < num_regs; ii++)                         \
                {                                                       \
                  RELOCATE_IF_OK (regstart[ii]);                        \
                  RELOCATE_IF_OK (regend[ii]);                          \
                  RELOCATE_IF_OK (old_regstart[ii]);                    \
                  RELOCATE_IF_OK (old_regend[ii]);                      \
                  RELOCATE_IF_OK (best_regstart[ii]);                   \
                  RELOCATE_IF_OK (best_regend[ii]);                     \
                }                                                       \
            }                                                           \
                                                                        \
            /* Check the relevant elements on the fail stack. */        \
          ii = fail_stack.avail - 1;                                    \
          while (ii >= 0)                                               \
            {                                                           \
              DEBUG_STATEMENT (ii--); /* Skip the failure id. */        \
                                                                        \
              /* string_place: */                                       \
              RELOCATE_IF_OK (fail_stack.stack[ii].pointer); ii--;      \
                                                                        \
              ii--; /* pattern_place, not relocatable. */               \
                                                                        \
              /* Neither high_reg nor low_reg are relocatable, but we   \
                 need their values to loop through the saved            \
                 registers.*/                                           \
              high_reg = fail_stack.stack[ii].integer; ii--;            \
              low_reg = fail_stack.stack[ii].integer; ii--;             \
                                                                        \
              malloc_checking_assert (ii >= (high_reg - low_reg));      \
              for (jj = high_reg; jj >= low_reg; jj--)                  \
                {                                                       \
                  ii--; /* reg_info[this_reg].word */                   \
                                                                        \
                  /* regend[this_reg]: */                               \
                  RELOCATE_IF_OK (fail_stack.stack[ii].pointer); ii--;  \
                                                                        \
                  /* regstart[this_reg]: */                             \
                  RELOCATE_IF_OK (fail_stack.stack[ii].pointer); ii--;  \
                }                                                       \
            }                                                           \
                                                                        \
          /* We use the following when examining all the other values   \
             for plausibility, which makes it impractical to examine    \
             them for plausibility themselves. */                       \
                                                                        \
          RELOCATE_IF_OK_NO_ASSERT (string1);                           \
          RELOCATE_IF_OK_NO_ASSERT (string2);                           \
          RELOCATE_IF_OK_NO_ASSERT (end1);                              \
          RELOCATE_IF_OK_NO_ASSERT (end2);                              \
                                                                        \
          /* Careful, orig_buftext is a relocatable pointer too. */     \
          RELOCATE_IF_OK_NO_ASSERT (orig_buftext);                      \
        }                                                               \
    } while (0)
#endif /* !defined (emacs) || !defined (REL_ALLOC) */

#if !defined (emacs) || !defined (REL_ALLOC)
#define RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS()
#else
/* Within re_search_2(), check whether the current search string reflects a
   Lisp buffer that has just had its text reallocated. If so, update the
   function-local saved pointer values to reflect the new text
   addresses. There are no regular expression register values to update, nor
   is there a fail stack. */
#define RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS()                       \
do                                                                        \
{                                                                         \
  Bytecount rmdp_offset = offset_post_relocation (lispobj, orig_buftext); \
                                                                          \
  if (rmdp_offset)                                                        \
    {                                                                     \
      RELOCATE_IF_OK (str1);                                              \
      RELOCATE_IF_OK (str2);                                              \
      RELOCATE_IF_OK (string1);                                           \
      RELOCATE_IF_OK (string2);                                           \
      RELOCATE_IF_OK (d);                                                 \
                                                                          \
      /* Careful, orig_buftext is a relocatable pointer too. */           \
      RELOCATE_IF_OK_NO_ASSERT (orig_buftext);                            \
    }                                                                     \
} while (0)

#endif /* emacs */

/* Within re_comppile_fastmap(), push pointer POINTER onto FAIL_STACK.
   Return 1 if able to do so and 0 if ran out of memory allocating
   space to do so.

   re_compile_fastmap() has no access to buffer data, starts with an empty
   fail_stack, and cleans up the values it adds on exit. This means
   considerations of whether pointers are relocatable don't apply. */
#define PUSH_PATTERN_OP(POINTER, FAIL_STACK)				\
  ((FAIL_STACK_FULL ()							\
    && !DOUBLE_FAIL_STACK (FAIL_STACK))					\
   ? 0									\
   : ((FAIL_STACK).stack[(FAIL_STACK).avail++].pointer = POINTER,	\
      1))

#define PUSH_FAILURE_POINTER_1(item)					\
  fail_stack.stack[fail_stack.avail++].pointer = (re_char *) (item)

/* Push a non-relocatable (non-buffer-text) pointer value onto the failure
   stack.  Assumes the variable `fail_stack'. Should only be called from
   within `PUSH_FAILURE_POINT', itself only from within
   re_match_2_internal().
   If assertions are turned on, assert that item is not one of the
   relocatable values we know about. */
#define PUSH_FAILURE_POINTER(item) do {					\
    malloc_checking_assert (NULL == item ||				\
			    !((item >= string1 && item <= end1) ||	\
			      (item >= string2 && item <= end2) ||	\
			      REG_UNSET ((const re_char *)item)));	\
    PUSH_FAILURE_POINTER_1 (item);					\
  } while (0)

/* Push a pointer to buffer text onto the failure stack. If assertions are
   turned on, sanity-check the pointer. Note that no type info is saved, and
   the relevant code in RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS() is
   fragile and dependent on the order of operations in PUSH_FAILURE_POINT(),
   just as the code in POP_FAILURE_POINT is. */
#define PUSH_FAILURE_RELOCATABLE(item) do {				\
    malloc_checking_assert ((item >= string1 && item <= end1) ||	\
			    (item >= string2 && item <= end2) ||	\
			    item == NULL ||				\
			    REG_UNSET ((unsigned char *)item));		\
    PUSH_FAILURE_POINTER_1 (item);					\
  } while (0)

/* This pushes an integer-valued item onto the failure stack.
   Assumes the variable `fail_stack'.  Probably should only
   be called from within `PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_INT(item)					\
  fail_stack.stack[fail_stack.avail++].integer = (item)

/* Push a fail_stack_elt_t value onto the failure stack.
   Assumes the variable `fail_stack'.  Probably should only
   be called from within `PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_ELT(item)					\
  fail_stack.stack[fail_stack.avail++] =  (item)

/* These three POP... operations complement the three PUSH... operations.
   All assume that `fail_stack' is nonempty.  */
#define POP_FAILURE_POINTER() fail_stack.stack[--fail_stack.avail].pointer

#ifdef emacs
static inline re_char *
pop_failure_relocatable_1 (fail_stack_type *fail_stack_ptr, re_char *string1,
			   re_char *end1, re_char *string2, re_char *end2)
{
  re_char *item = fail_stack_ptr->stack[fail_stack_ptr->avail - 1].pointer;
  malloc_checking_assert ((item >= string1 && item <= end1) ||
			  (item >= string2 && item <= end2) ||
			  item == NULL || REG_UNSET ((unsigned char *)item));
  fail_stack_ptr->avail -= 1;
  return item;
}
#define POP_FAILURE_RELOCATABLE()		\
  pop_failure_relocatable_1 (&fail_stack, string1, end1, string2, end2)
#else
#define POP_FAILURE_RELOCATABLE POP_FAILURE_POINTER 
#endif    

#define POP_FAILURE_INT() fail_stack.stack[--fail_stack.avail].integer
#define POP_FAILURE_ELT() fail_stack.stack[--fail_stack.avail]

/* Push the information about the state we will need if we ever fail back to
   it.

   Requires variables fail_stack, regstart, regend, reg_info, and num_regs
   be declared.  DOUBLE_FAIL_STACK requires `destination' be declared.

   Does `return FAILURE_CODE' if runs out of memory. 

   In practical terms, only to be called from within re_match_2_internal. */

#if !defined (REGEX_MALLOC) && !defined (REGEX_REL_ALLOC)
#define DECLARE_DESTINATION char *destination
#else
#define DECLARE_DESTINATION DECLARE_NOTHING
#endif

#define PUSH_FAILURE_POINT(pattern_place, string_place, failure_code)	\
do {									\
  DECLARE_DESTINATION;							\
  /* Must be int, so when we don't save any registers, the arithmetic	\
     of 0 + -1 isn't done as unsigned.  */				\
  int this_reg;								\
									\
  DEBUG_STATEMENT ((failure_id++, nfailure_points_pushed++));		\
									\
  if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
    {									\
      DEBUG_FAIL_PRINT2 ("\nPUSH_FAILURE_POINT #%d:\n", failure_id);	\
      DEBUG_FAIL_PRINT2 ("  Before push, next avail: %ld\n",		\
			 (long) (fail_stack).avail);			\
      DEBUG_FAIL_PRINT2 ("                     size: %ld\n",		\
			 (long) (fail_stack).size);			\
      									\
      DEBUG_FAIL_PRINT2 ("  slots needed: %d\n", NUM_FAILURE_ITEMS);	\
      DEBUG_FAIL_PRINT2 ("     available: %ld\n",			\
			 (long) REMAINING_AVAIL_SLOTS);			\
    }									\
									\
  /* Ensure we have enough space allocated for what we will push.  */	\
  while (REMAINING_AVAIL_SLOTS < NUM_FAILURE_ITEMS)			\
    {									\
      BEGIN_REGEX_MALLOC_OK ();						\
      if (!DOUBLE_FAIL_STACK (fail_stack))				\
	{								\
          END_REGEX_MALLOC_OK ();					\
	  UNBIND_REGEX_MALLOC_CHECK ();					\
	  return failure_code;						\
	}								\
      END_REGEX_MALLOC_OK ();						\
      RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();			\
									\
      if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
	{								\
	  DEBUG_FAIL_PRINT2 ("\n  Doubled stack; size now: %ld\n",	\
			     (long) (fail_stack).size);			\
	  DEBUG_FAIL_PRINT2 ("  slots available: %ld\n",		\
			     (long) REMAINING_AVAIL_SLOTS);		\
	}								\
    }									\
									\
  /* Push the info, starting with the registers.  */			\
  for (this_reg = lowest_active_reg; this_reg <= highest_active_reg;	\
       this_reg++)							\
    {									\
      PUSH_FAILURE_RELOCATABLE (regstart[this_reg]);			\
      PUSH_FAILURE_RELOCATABLE (regend[this_reg]);			\
      PUSH_FAILURE_ELT (reg_info[this_reg].word);			\
									\
      DEBUG_STATEMENT (num_regs_pushed++);				\
									\
      if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
	{								\
	  DEBUG_FAIL_PRINT2 ("  Pushing reg: %d\n", this_reg);		\
									\
	  DEBUG_FAIL_PRINT2 ("    start: 0x%lx\n",			\
			     (long) regstart[this_reg]);		\
	  DEBUG_FAIL_PRINT2 ("    end: 0x%lx\n",			\
			     (long) regend[this_reg]);			\
	  DEBUG_FAIL_PRINT2 ("    info: 0x%lx\n      ",			\
			     * (long *) (&reg_info[this_reg]));		\
	  DEBUG_FAIL_PRINT2 (" match_null=%d",				\
			     REG_MATCH_NULL_STRING_P (reg_info		\
						      [this_reg]));	\
	  DEBUG_FAIL_PRINT2 (" active=%d",				\
			     IS_ACTIVE (reg_info[this_reg]));		\
	  DEBUG_FAIL_PRINT2 (" matched_something=%d",			\
			     MATCHED_SOMETHING (reg_info[this_reg]));	\
	  DEBUG_FAIL_PRINT2 (" ever_matched_something=%d",		\
			     EVER_MATCHED_SOMETHING (reg_info		\
						     [this_reg]));	\
	  DEBUG_FAIL_PRINT1 ("\n");					\
	}								\
    }									\
									\
  PUSH_FAILURE_INT (lowest_active_reg);					\
  PUSH_FAILURE_INT (highest_active_reg);				\
									\
  PUSH_FAILURE_POINTER (pattern_place);					\
									\
  PUSH_FAILURE_RELOCATABLE (string_place);				\
									\
  /* Can't put this within the DEBUG_RUNTIME_FLAGS, the decision is made	\
     at compile time */							\
  DEBUG_STATEMENT (PUSH_FAILURE_INT (failure_id));			\
									\
  if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
    {									\
      DEBUG_FAIL_PRINT2 ("  Pushing  low active reg: %d\n",		\
			 lowest_active_reg);				\
      DEBUG_FAIL_PRINT2 ("  Pushing high active reg: %d\n",		\
			 highest_active_reg);				\
      DEBUG_FAIL_PRINT2 ("  Pushing pattern 0x%lx: \n",			\
			 (long) pattern_place);				\
      DEBUG_FAIL_PRINT_COMPILED_PATTERN (bufp, pattern_place, pend);	\
      DEBUG_FAIL_PRINT2 ("  Pushing string 0x%lx: `",			\
			 (long) string_place);				\
      DEBUG_FAIL_PRINT_DOUBLE_STRING (string_place, string1, size1,	\
				      string2, size2);			\
      DEBUG_FAIL_PRINT1 ("'\n");					\
      DEBUG_FAIL_PRINT2 ("  Pushing failure id: %u\n", failure_id);	\
    }									\
} while (0)

/* This is the number of items that are pushed and popped on the stack
   for each register.  */
#define NUM_REG_ITEMS  3

/* Individual items aside from the registers.  */
#ifdef DEBUG
#define NUM_NONREG_ITEMS 5 /* Includes failure point id.  */
#else
#define NUM_NONREG_ITEMS 4
#endif

/* We push at most this many items on the stack.  */
/* We used to use (num_regs - 1), which is the number of registers
   this regexp will save; but that was changed to 5
   to avoid stack overflow for a regexp with lots of parens.  */
#define MAX_FAILURE_ITEMS (5 * NUM_REG_ITEMS + NUM_NONREG_ITEMS)

/* We actually push this many items.  */
#define NUM_FAILURE_ITEMS						\
  ((highest_active_reg - lowest_active_reg + 1) * NUM_REG_ITEMS 	\
    + NUM_NONREG_ITEMS)

/* How many items can still be added to the stack without overflowing it.  */
#define REMAINING_AVAIL_SLOTS ((fail_stack).size - (fail_stack).avail)


/* Pops what PUSH_FAIL_STACK pushes.

   We restore into the following parameters, all of which should be lvalues:
     STR -- the saved data position.
     PAT -- the saved pattern position.
     LOW_REG, HIGH_REG -- the highest and lowest active registers.
     REGSTART, REGEND -- arrays of string positions.
     REG_INFO -- array of information about each subexpression.

   Also assumes the variables `fail_stack' and (if debugging), `bufp',
   `pend', `string1', `size1', `string2', and `size2'.  */

#define POP_FAILURE_POINT(str, pat, low_reg, high_reg,			\
                          regstart, regend, reg_info)			\
do {									\
  DEBUG_STATEMENT (int ffailure_id;)					\
  int this_reg;								\
  const unsigned char *string_temp;					\
									\
  /* Remove failure points and point to how many regs pushed.  */	\
  assert (fail_stack.avail >= NUM_NONREG_ITEMS);			\
									\
  if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
    {									\
      DEBUG_FAIL_PRINT1 ("POP_FAILURE_POINT:\n");			\
      DEBUG_FAIL_PRINT2 ("  Before pop, next avail: %ld\n",		\
			 (long) fail_stack.avail);			\
      DEBUG_FAIL_PRINT2 ("                    size: %ld\n",		\
			 (long) fail_stack.size);			\
    }									\
									\
  DEBUG_STATEMENT (ffailure_id = POP_FAILURE_INT());			\
									\
  /* If the saved string location is NULL, it came from an		\
     on_failure_keep_string_jump opcode, and we want to throw away the	\
     saved NULL, thus retaining our current position in the string.  */	\
  string_temp = POP_FAILURE_RELOCATABLE ();				\
  if (string_temp != NULL)						\
    str = string_temp;							\
									\
  pat = (unsigned char *) POP_FAILURE_POINTER ();			\
									\
  /* Restore register info.  */						\
  high_reg = POP_FAILURE_INT ();					\
  low_reg = POP_FAILURE_INT ();						\
									\
  if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
    {									\
      DEBUG_FAIL_PRINT2 ("  Popping failure id: %d\n", ffailure_id);	\
      DEBUG_FAIL_PRINT2 ("  Popping string 0x%lx: `",  (long) str);	\
      DEBUG_FAIL_PRINT_DOUBLE_STRING (str, string1, size1,		\
				      string2, size2);			\
      DEBUG_FAIL_PRINT1 ("'\n");					\
      DEBUG_FAIL_PRINT2 ("  Popping pattern 0x%lx: ", (long) pat);	\
      DEBUG_FAIL_PRINT_COMPILED_PATTERN (bufp, pat, pend);		\
      DEBUG_FAIL_PRINT2 ("  Popping high active reg: %d\n", high_reg);	\
      DEBUG_FAIL_PRINT2 ("  Popping  low active reg: %d\n", low_reg);	\
    }									\
									\
  for (this_reg = high_reg; this_reg >= low_reg; this_reg--)		\
    {									\
      reg_info[this_reg].word = POP_FAILURE_ELT ();			\
      regend[this_reg] = POP_FAILURE_RELOCATABLE ();			\
      regstart[this_reg] = POP_FAILURE_RELOCATABLE ();			\
									\
      if (DEBUG_RUNTIME_FLAGS & RE_DEBUG_FAILURE_POINT)			\
	{								\
	  DEBUG_FAIL_PRINT2 ("    Popping reg: %d\n", this_reg);	\
	  DEBUG_FAIL_PRINT2 ("      info: 0x%lx\n",			\
			     * (long *) &reg_info[this_reg]);		\
	  DEBUG_FAIL_PRINT2 ("      end: 0x%lx\n",			\
			     (long) regend[this_reg]);			\
	  DEBUG_FAIL_PRINT2 ("      start: 0x%lx\n",			\
			     (long) regstart[this_reg]);		\
	}								\
    }									\
									\
  set_regs_matched_done = 0;						\
  DEBUG_STATEMENT (nfailure_points_popped++);				\
} while (0) /* POP_FAILURE_POINT */


/* Structure for per-register (a.k.a. per-group) information.
   Other register information, such as the
   starting and ending positions (which are addresses), and the list of
   inner groups (which is a bits list) are maintained in separate
   variables.

   We are making a (strictly speaking) nonportable assumption here: that
   the compiler will pack our bit fields into something that fits into
   the type of `word', i.e., is something that fits into one item on the
   failure stack.  */

typedef union
{
  fail_stack_elt_t word;
  struct
  {
      /* This field is one if this group can match the empty string,
         zero if not.  If not yet determined,  `MATCH_NULL_UNSET_VALUE'.  */
#define MATCH_NULL_UNSET_VALUE 3
    unsigned int match_null_string_p : 2;
    unsigned int is_active : 1;
    unsigned int matched_something : 1;
    unsigned int ever_matched_something : 1;
  } bits;
} register_info_type;

#define REG_MATCH_NULL_STRING_P(R)  ((R).bits.match_null_string_p)
#define IS_ACTIVE(R)  ((R).bits.is_active)
#define MATCHED_SOMETHING(R)  ((R).bits.matched_something)
#define EVER_MATCHED_SOMETHING(R)  ((R).bits.ever_matched_something)


/* Call this when have matched a real character; it sets `matched' flags
   for the subexpressions which we are currently inside.  Also records
   that those subexprs have matched.  */
#define SET_REGS_MATCHED()						\
  do									\
    {									\
      if (!set_regs_matched_done)					\
	{								\
	  int r;							\
	  set_regs_matched_done = 1;					\
	  for (r = lowest_active_reg; r <= highest_active_reg; r++)	\
	    {								\
	      MATCHED_SOMETHING (reg_info[r])				\
		= EVER_MATCHED_SOMETHING (reg_info[r])			\
		= 1;							\
	    }								\
	}								\
    }									\
  while (0)

/* Subroutine declarations and macros for regex_compile.  */

/* Fetch the next character in the uncompiled pattern---translating it
   if necessary.  */
#define PATFETCH(c)							\
  do {									\
    PATFETCH_RAW (c);							\
    c = RE_TRANSLATE (c);						\
  } while (0)

/* Fetch the next character in the uncompiled pattern, with no
   translation.  */
#define PATFETCH_RAW(c)							\
  do {if (p == pend) return REG_EEND;					\
    assert (p < pend);							\
    c = itext_ichar (p); 						\
    INC_IBYTEPTR (p);							\
  } while (0)

/* Go backwards one character in the pattern.  */
#define PATUNFETCH DEC_IBYTEPTR (p)

/* If `translate' is non-null, return translate[D], else just D.  We
   cast the subscript to translate because some data is declared as
   `char *', to avoid warnings when a string constant is passed.  But
   when we use a character as a subscript we must make it unsigned.  */
#define RE_TRANSLATE(d) \
  (TRANSLATE_P (translate) ? RE_TRANSLATE_1 (d) : (d))

/* Macros for outputting the compiled pattern into `buffer'.  */

/* If the buffer isn't allocated when it comes in, use this.  */
#define INIT_BUF_SIZE  32

/* Make sure we have at least N more bytes of space in buffer.  */
#define GET_BUFFER_SPACE(n)						\
    while (buf_end - bufp->buffer + (n) > (ptrdiff_t) bufp->allocated)	\
      EXTEND_BUFFER ()

/* Make sure we have one more byte of buffer space and then add C to it.  */
#define BUF_PUSH(c)							\
  do {									\
    GET_BUFFER_SPACE (1);						\
    *buf_end++ = (unsigned char) (c);					\
  } while (0)


/* Ensure we have two more bytes of buffer space and then append C1 and C2.  */
#define BUF_PUSH_2(c1, c2)						\
  do {									\
    GET_BUFFER_SPACE (2);						\
    *buf_end++ = (unsigned char) (c1);					\
    *buf_end++ = (unsigned char) (c2);					\
  } while (0)


/* As with BUF_PUSH_2, except for three bytes.  */
#define BUF_PUSH_3(c1, c2, c3)						\
  do {									\
    GET_BUFFER_SPACE (3);						\
    *buf_end++ = (unsigned char) (c1);					\
    *buf_end++ = (unsigned char) (c2);					\
    *buf_end++ = (unsigned char) (c3);					\
  } while (0)


/* Store a jump with opcode OP at LOC to location TO.  We store a
   relative address offset by the three bytes the jump itself occupies.  */
#define STORE_JUMP(op, loc, to) \
  store_op1 (op, loc, (to) - (loc) - 3)

/* Likewise, for a two-argument jump.  */
#define STORE_JUMP2(op, loc, to, arg) \
  store_op2 (op, loc, (to) - (loc) - 3, arg)

/* Like `STORE_JUMP', but for inserting.  Assume `buf_end' is the
   buffer end.  */
#define INSERT_JUMP(op, loc, to) \
  insert_op1 (op, loc, (to) - (loc) - 3, buf_end)

/* Like `STORE_JUMP2', but for inserting.  Assume `buf_end' is the
   buffer end.  */
#define INSERT_JUMP2(op, loc, to, arg) \
  insert_op2 (op, loc, (to) - (loc) - 3, arg, buf_end)


/* This is not an arbitrary limit: the arguments which represent offsets
   into the pattern are two bytes long.  So if 2^16 bytes turns out to
   be too small, many things would have to change.  */
#define MAX_BUF_SIZE (1L << 16)


/* Extend the buffer by twice its current size via realloc and
   reset the pointers that pointed into the old block to point to the
   correct places in the new one.  If extending the buffer results in it
   being larger than MAX_BUF_SIZE, then flag memory exhausted.  */
#define EXTEND_BUFFER()							 \
  do {									 \
    re_char *old_buffer = bufp->buffer;					 \
    if (bufp->allocated == MAX_BUF_SIZE)				 \
      return REG_ESIZE;							 \
    bufp->allocated <<= 1;						 \
    if (bufp->allocated > MAX_BUF_SIZE)					 \
      bufp->allocated = MAX_BUF_SIZE;					 \
    bufp->buffer =							 \
      (unsigned char *) xrealloc (bufp->buffer, bufp->allocated);	 \
    if (bufp->buffer == NULL)						 \
      return REG_ESPACE;						 \
    /* If the buffer moved, move all the pointers into it.  */		 \
    if (old_buffer != bufp->buffer)					 \
      {									 \
        buf_end = (buf_end - old_buffer) + bufp->buffer;		 \
        begalt = (begalt - old_buffer) + bufp->buffer;			 \
        if (fixup_alt_jump)						 \
          fixup_alt_jump = (fixup_alt_jump - old_buffer) + bufp->buffer; \
        if (laststart)							 \
          laststart = (laststart - old_buffer) + bufp->buffer;		 \
        if (pending_exact)						 \
          pending_exact = (pending_exact - old_buffer) + bufp->buffer;	 \
      }									 \
  } while (0)


/* Since we have one byte reserved for the register number argument to
   {start,stop}_memory, the maximum number of groups we can report
   things about is what fits in that byte.  */
#define MAX_REGNUM 255

/* But patterns can have more than `MAX_REGNUM' registers.  We just
   ignore the excess.
   #### not true!  groups past this will fail in lots of ways, if we
   ever have to backtrack.
  */
typedef int regnum_t;

#define INIT_REG_TRANSLATE_SIZE 5

/* Macros for the compile stack.  */

/* Since offsets can go either forwards or backwards, this type needs to
   be able to hold values from -(MAX_BUF_SIZE - 1) to MAX_BUF_SIZE - 1.  */
typedef int pattern_offset_t;

typedef struct
{
  pattern_offset_t begalt_offset;
  pattern_offset_t fixup_alt_jump;
  pattern_offset_t inner_group_offset;
  pattern_offset_t laststart_offset;
  regnum_t regnum;
} compile_stack_elt_t;


typedef struct
{
  compile_stack_elt_t *stack;
  int size;
  int avail;			/* Offset of next open position.  */
} compile_stack_type;


#define INIT_COMPILE_STACK_SIZE 32

#define COMPILE_STACK_EMPTY  (compile_stack.avail == 0)
#define COMPILE_STACK_FULL  (compile_stack.avail == compile_stack.size)

/* The next available element.  */
#define COMPILE_STACK_TOP (compile_stack.stack[compile_stack.avail])

/* Set the bit for character C in a bit vector.  */
#define SET_LIST_BIT(c)				\
  (buf_end[((unsigned char) (c)) / BYTEWIDTH]	\
   |= 1 << (((unsigned char) c) % BYTEWIDTH))

#ifdef MULE

/* Set the "bit" for character C in a range table. */
#define SET_RANGETAB_BIT(c) put_range_table (rtab, c, c, Qt)

#endif

#ifdef emacs
/* Parse the longest number we can, but don't produce a bignum, that can't
   correspond to anything we're interested in and would needlessly complicate
   code. Also avoid the silent overflow issues of the non-emacs code below. */
#define GET_UNSIGNED_NUMBER(num) do \
    {                                                                   \
      Ibyte *_gus_numend = NULL;                                        \
      Lisp_Object _gus_numno;                                           \
      /* most-positive-fixnum on 32 bit XEmacs is 10 decimal digits,    \
         nine will keep us in fixnum territory no matter our            \
         architecture */                                                \
      Bytecount limit = min (pend - p, 9);                              \
                                                                        \
      /* Require that any digits are ASCII. We already require that     \
         the user type ASCII in order to type {,(,|, etc, and there is  \
         the potential for security holes in the future if we allow     \
         non-ASCII digits to specify groups in regexps and other        \
         code that parses regexps is not aware of this. */              \
      _gus_numno = parse_integer (p, &_gus_numend, limit, 10, 1,        \
                                  Vdigit_fixnum_ascii);                 \
      PATFETCH (c);                                                     \
      if (c != '-' && FIXNUMP (_gus_numno))                             \
        {                                                               \
          num = XREALFIXNUM (_gus_numno);                               \
          p = _gus_numend;                                              \
          if (p != pend)                                                \
            {                                                           \
              PATFETCH (c);                                             \
            }                                                           \
        }                                                               \
    } while (0)
#else
/* Get the next unsigned number in the uncompiled pattern.  */
#define GET_UNSIGNED_NUMBER(num) 					\
  { if (p != pend)							\
     {									\
       PATFETCH (c); 							\
       while (ISDIGIT (c)) 						\
         { 								\
           if (num < 0)							\
              num = 0;							\
           num = num * 10 + c - '0'; 					\
           if (p == pend) 						\
              break; 							\
           PATFETCH (c);						\
         } 								\
       } 								\
    }
#endif

/* Map a string to the char class it names (if any). BEG points to the string
   to be parsed and LIMIT is the length, in bytes, of that string.

   XEmacs; this only handles the NAME part of the [:NAME:] specification of a
   character class name. The GNU emacs version of this function attempts to
   handle the string from [: onwards, and is called re_wctype_parse. Our
   approach means the function doesn't need to be called with every character
   class encountered.

   LENGTH would be a Bytecount if this function didn't need to be compiled
   also for executables that don't include lisp.h

   Return RECC_ERROR if STRP doesn't match a known character class. */
re_wctype_t
re_wctype (const re_char *beg, int limit)
{
  /* Sort tests in the length=five case by frequency the classes to minimize
     number of times we fail the comparison.  The frequencies of character class
     names used in Emacs sources as of 2016-07-27:

     $ find \( -name \*.c -o -name \*.el \) -exec grep -h '\[:[a-z]*:]' {} + |
           sed 's/]/]\n/g' |grep -o '\[:[a-z]*:]' |sort |uniq -c |sort -nr
         213 [:alnum:]
         104 [:alpha:]
          62 [:space:]
          39 [:digit:]
          36 [:blank:]
          26 [:word:]
          26 [:upper:]
          21 [:lower:]
          10 [:xdigit:]
          10 [:punct:]
          10 [:ascii:]
           4 [:nonascii:]
           4 [:graph:]
           2 [:print:]
           2 [:cntrl:]
           1 [:ff:]

     If you update this list, consider also updating chain of or'ed conditions
     in execute_charset function. XEmacs; our equivalent is the condition
     checking class_bits in the charset_mule and charset_mule_not opcodes.
   */

  switch (limit) {
  case 4:
    if (!memcmp (beg, "word", 4))      return RECC_WORD;
    break;
  case 5:
    if (!memcmp (beg, "alnum", 5))     return RECC_ALNUM;
    if (!memcmp (beg, "alpha", 5))     return RECC_ALPHA;
    if (!memcmp (beg, "space", 5))     return RECC_SPACE;
    if (!memcmp (beg, "digit", 5))     return RECC_DIGIT;
    if (!memcmp (beg, "blank", 5))     return RECC_BLANK;
    if (!memcmp (beg, "upper", 5))     return RECC_UPPER;
    if (!memcmp (beg, "lower", 5))     return RECC_LOWER;
    if (!memcmp (beg, "punct", 5))     return RECC_PUNCT;
    if (!memcmp (beg, "ascii", 5))     return RECC_ASCII;
    if (!memcmp (beg, "graph", 5))     return RECC_GRAPH;
    if (!memcmp (beg, "print", 5))     return RECC_PRINT;
    if (!memcmp (beg, "cntrl", 5))     return RECC_CNTRL;
    break;
  case 6:
    if (!memcmp (beg, "xdigit", 6))    return RECC_XDIGIT;
    break;
  case 7:
    if (!memcmp (beg, "unibyte", 7))   return RECC_UNIBYTE;
    break;
  case 8:
    if (!memcmp (beg, "nonascii", 8))  return RECC_NONASCII;
    break;
  case 9:
    if (!memcmp (beg, "multibyte", 9)) return RECC_MULTIBYTE;
    break;
  }

  return RECC_ERROR;
}

/* True if CH is in the char class CC.  */
int
re_iswctype (int ch, re_wctype_t cc
             RE_ISWCTYPE_ARG_DECL)
{
  switch (cc)
    {
    case RECC_ALNUM: return ISALNUM (ch) != 0;
    case RECC_ALPHA: return ISALPHA (ch) != 0;
    case RECC_BLANK: return ISBLANK (ch) != 0;
    case RECC_CNTRL: return ISCNTRL (ch) != 0;
    case RECC_DIGIT: return ISDIGIT (ch) != 0;
    case RECC_GRAPH: return ISGRAPH (ch) != 0;
    case RECC_PRINT: return ISPRINT (ch) != 0;
    case RECC_PUNCT: return ISPUNCT (ch) != 0;
    case RECC_SPACE: return ISSPACE (ch) != 0;
#ifdef emacs
    case RECC_UPPER: 
      return NILP (lispbuf->case_fold_search) ? ISUPPER (ch) != 0
        : !NOCASEP (lispbuf, ch);
    case RECC_LOWER: 
      return NILP (lispbuf->case_fold_search) ? ISLOWER (ch) != 0
        : !NOCASEP (lispbuf, ch);
#else
    case RECC_UPPER: return ISUPPER (ch) != 0;
    case RECC_LOWER: return ISLOWER (ch) != 0;
#endif
    case RECC_XDIGIT: return ISXDIGIT (ch) != 0;
    case RECC_ASCII: return ISASCII (ch) != 0;
    case RECC_NONASCII: case RECC_MULTIBYTE: return !ISASCII (ch);
    case RECC_UNIBYTE: return ISUNIBYTE (ch) != 0;
    case RECC_WORD: return ISWORD (ch) != 0;
    case RECC_ERROR: return false;
    default:
      abort ();
    }
}

#ifdef MULE

static re_bool
re_wctype_can_match_non_ascii (re_wctype_t cc)
{
  switch (cc)
    {
    case RECC_ASCII:
    case RECC_UNIBYTE:
    case RECC_CNTRL:
    case RECC_DIGIT:
    case RECC_XDIGIT:
    case RECC_BLANK:
      return false;
    default:
      return true;
    }
}

#endif /* MULE */

#ifdef emacs

/* Return a bit-pattern to use in the range-table bits to match multibyte
   chars of class CC.  */
static unsigned char
re_wctype_to_bit (re_wctype_t cc)
{
  switch (cc)
    {
    case RECC_PRINT: case RECC_GRAPH:
    case RECC_ALPHA: return BIT_ALPHA;
    case RECC_ALNUM: case RECC_WORD: return BIT_WORD;
    case RECC_LOWER: return BIT_LOWER;
    case RECC_UPPER: return BIT_UPPER;
    case RECC_PUNCT: return BIT_PUNCT;
    case RECC_SPACE: return BIT_SPACE;
    case RECC_MULTIBYTE: case RECC_NONASCII: 
    case RECC_ASCII: case RECC_DIGIT: case RECC_XDIGIT: case RECC_CNTRL:
    case RECC_BLANK: case RECC_UNIBYTE: case RECC_ERROR: return 0;
    default:
      ABORT ();
      return 0;
    }
}

#endif /* emacs */

static void store_op1 (re_opcode_t op, unsigned char *loc, int arg);
static void store_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2);
static void insert_op1 (re_opcode_t op, unsigned char *loc, int arg,
			unsigned char *end);
static void insert_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2,
			unsigned char *end);
static re_bool at_begline_loc_p (re_char *pattern, re_char *p,
				 reg_syntax_t syntax);
static re_bool at_endline_loc_p (re_char *p, re_char *pend, int syntax);
static re_bool group_in_compile_stack (compile_stack_type compile_stack,
				       regnum_t regnum);
static reg_errcode_t compile_range (re_char **p_ptr, re_char *pend,
				    RE_TRANSLATE_TYPE translate,
				    reg_syntax_t syntax,
				    unsigned char *b);
#ifdef MULE
static reg_errcode_t compile_extended_range (re_char **p_ptr,
					     re_char *pend,
					     RE_TRANSLATE_TYPE translate,
					     reg_syntax_t syntax,
					     Lisp_Object rtab);
#endif /* MULE */
#ifdef emacs
reg_errcode_t compile_char_class (re_wctype_t cc, Lisp_Object rtab,
                                  Bitbyte *flags_out);
#endif

static re_bool group_match_null_string_p (unsigned char **p,
					  unsigned char *end,
					  register_info_type *reg_info);
static re_bool alt_match_null_string_p (unsigned char *p, unsigned char *end,
					register_info_type *reg_info);
static re_bool common_op_match_null_string_p (unsigned char **p,
					      unsigned char *end,
					      register_info_type *reg_info);
static int bcmp_translate (re_char *s1, re_char *s2,
			   REGISTER int len, RE_TRANSLATE_TYPE translate
#ifdef emacs
			   , Internal_Format fmt, Lisp_Object lispobj
#endif
			   );
static int re_match_2_internal (struct re_pattern_buffer *bufp,
				re_char *string1, int size1,
				re_char *string2, int size2, int pos,
				struct re_registers *regs, int stop
				RE_LISP_CONTEXT_ARGS_DECL);

#ifndef MATCH_MAY_ALLOCATE

/* If we cannot allocate large objects within re_match_2_internal,
   we make the fail stack and register vectors global.
   The fail stack, we grow to the maximum size when a regexp
   is compiled.
   The register vectors, we adjust in size each time we
   compile a regexp, according to the number of registers it needs.  */

static fail_stack_type fail_stack;

/* Size with which the following vectors are currently allocated.
   That is so we can make them bigger as needed,
   but never make them smaller.  */
static int regs_allocated_size;

static re_char **     regstart, **     regend;
static re_char ** old_regstart, ** old_regend;
static re_char **best_regstart, **best_regend;
static register_info_type *reg_info;
static re_char **reg_dummy;
static register_info_type *reg_info_dummy;

/* Make the register vectors big enough for NUM_REGS registers,
   but don't make them smaller.  */

static
regex_grow_registers (int num_regs)
{
  if (num_regs > regs_allocated_size)
    {
      RETALLOC (regstart,	num_regs, re_char *);
      RETALLOC (regend,		num_regs, re_char *);
      RETALLOC (old_regstart,	num_regs, re_char *);
      RETALLOC (old_regend,	num_regs, re_char *);
      RETALLOC (best_regstart,	num_regs, re_char *);
      RETALLOC (best_regend,	num_regs, re_char *);
      RETALLOC (reg_info,	num_regs, register_info_type);
      RETALLOC (reg_dummy,	num_regs, re_char *);
      RETALLOC (reg_info_dummy, num_regs, register_info_type);

      regs_allocated_size = num_regs;
    }
}

#endif /* not MATCH_MAY_ALLOCATE */

/* `regex_compile' compiles PATTERN (of length SIZE) according to SYNTAX.
   Returns one of error codes defined in `regex.h', or zero for success.

   Assumes the `allocated' (and perhaps `buffer') and `translate'
   fields are set in BUFP on entry.

   If it succeeds, results are put in BUFP (if it returns an error, the
   contents of BUFP are undefined):
     `buffer' is the compiled pattern;
     `syntax' is set to SYNTAX;
     `used' is set to the length of the compiled pattern;
     `fastmap_accurate' is zero;
     `re_ngroups' is the number of groups/subexpressions (including shy
        groups) in PATTERN;
     `re_nsub' is the number of non-shy groups in PATTERN;
     `not_bol' and `not_eol' are zero;

   The `fastmap' and `newline_anchor' fields are neither
   examined nor set.  */

/* Return, freeing storage we allocated.  */
#define FREE_STACK_RETURN(value)			\
do							\
{							\
  xfree (compile_stack.stack);	\
  return value;						\
} while (0)

static reg_errcode_t
regex_compile (re_char *pattern, int size, reg_syntax_t syntax,
	       struct re_pattern_buffer *bufp)
{
  /* We fetch characters from PATTERN here.  We declare these as int
     (or possibly long) so that chars above 127 can be used as
     array indices.  The macros that fetch a character from the pattern
     make sure to coerce to unsigned char before assigning, so we won't
     get bitten by negative numbers here. */
  /* XEmacs change: used to be unsigned char. */
  REGISTER EMACS_INT c, c1;

  /* A random temporary spot in PATTERN.  */
  re_char *p1;

  /* Points to the end of the buffer, where we should append.  */
  REGISTER unsigned char *buf_end;

  /* Keeps track of unclosed groups.  */
  compile_stack_type compile_stack;

  /* Points to the current (ending) position in the pattern.  */
  re_char *p = pattern;
  re_char *pend = pattern + size;

  /* How to translate the characters in the pattern.  */
  RE_TRANSLATE_TYPE translate = bufp->translate;

  /* Address of the count-byte of the most recently inserted `exactn'
     command.  This makes it possible to tell if a new exact-match
     character can be added to that command or if the character requires
     a new `exactn' command.  */
  unsigned char *pending_exact = 0;

  /* Address of start of the most recently finished expression.
     This tells, e.g., postfix * where to find the start of its
     operand.  Reset at the beginning of groups and alternatives.  */
  unsigned char *laststart = 0;

  /* Address of beginning of regexp, or inside of last group.  */
  unsigned char *begalt;

  /* Place in the uncompiled pattern (i.e., the {) to
     which to go back if the interval is invalid.  */
  re_char *beg_interval;

  /* Address of the place where a forward jump should go to the end of
     the containing expression.  Each alternative of an `or' -- except the
     last -- ends with a forward jump of this sort.  */
  unsigned char *fixup_alt_jump = 0;

  /* Counts open-groups as they are encountered.  Remembered for the
     matching close-group on the compile stack, so the same register
     number is put in the stop_memory as the start_memory.  */
  regnum_t regnum = 0;

#ifdef DEBUG
  if (debug_regexps & RE_DEBUG_COMPILATION)
    {
      int debug_count;

      DEBUG_PRINT1 ("\nCompiling pattern: ");
      for (debug_count = 0; debug_count < size; debug_count++)
        putchar (pattern[debug_count]);
      putchar ('\n');
    }
#endif /* DEBUG */

  /* Initialize the compile stack.  */
  compile_stack.stack = TALLOC (INIT_COMPILE_STACK_SIZE, compile_stack_elt_t);
  if (compile_stack.stack == NULL)
    return REG_ESPACE;

  compile_stack.size = INIT_COMPILE_STACK_SIZE;
  compile_stack.avail = 0;

  /* Initialize the pattern buffer.  */
  bufp->syntax = syntax;
  bufp->fastmap_accurate = 0;
  bufp->not_bol = bufp->not_eol = 0;

  /* Set `used' to zero, so that if we return an error, the pattern
     printer (for debugging) will think there's no pattern.  We reset it
     at the end.  */
  bufp->used = 0;

  /* Always count groups, whether or not bufp->no_sub is set.  */
  bufp->re_nsub = 0;
  bufp->re_ngroups = 0;

  bufp->warned_about_incompatible_back_references = 0;

  if (bufp->external_to_internal_register == 0)
    {
      bufp->external_to_internal_register_size = INIT_REG_TRANSLATE_SIZE;
      RETALLOC (bufp->external_to_internal_register,
		bufp->external_to_internal_register_size,
		int);
    }

  {
    int i;

    bufp->external_to_internal_register[0] = 0;
    for (i = 1; i < bufp->external_to_internal_register_size; i++)
      bufp->external_to_internal_register[i] = (int) 0xDEADBEEF;
  }

#if !defined (emacs) && !defined (SYNTAX_TABLE)
  /* Initialize the syntax table.  */
   init_syntax_once ();
#endif

  if (bufp->allocated == 0)
    {
      if (bufp->buffer)
	{ /* If zero allocated, but buffer is non-null, try to realloc
             enough space.  This loses if buffer's address is bogus, but
             that is the user's responsibility.  */
          RETALLOC (bufp->buffer, INIT_BUF_SIZE, unsigned char);
        }
      else
        { /* Caller did not allocate a buffer.  Do it for them.  */
          bufp->buffer = TALLOC (INIT_BUF_SIZE, unsigned char);
        }
      if (!bufp->buffer) FREE_STACK_RETURN (REG_ESPACE);

      bufp->allocated = INIT_BUF_SIZE;
    }

  begalt = buf_end = bufp->buffer;

  /* Loop through the uncompiled pattern until we're at the end.  */
  while (p != pend)
    {
      PATFETCH (c);

      switch (c)
        {
        case '^':
          {
            if (   /* If at start of pattern, it's an operator.  */
                   p == pattern + 1
                   /* If context independent, it's an operator.  */
                || syntax & RE_CONTEXT_INDEP_ANCHORS
                   /* Otherwise, depends on what's come before.  */
                || at_begline_loc_p (pattern, p, syntax))
              BUF_PUSH (begline);
            else
              goto normal_char;
          }
          break;


        case '$':
          {
            if (   /* If at end of pattern, it's an operator.  */
                   p == pend
                   /* If context independent, it's an operator.  */
                || syntax & RE_CONTEXT_INDEP_ANCHORS
                   /* Otherwise, depends on what's next.  */
                || at_endline_loc_p (p, pend, syntax))
               BUF_PUSH (endline);
             else
               goto normal_char;
           }
           break;


	case '+':
        case '?':
          if ((syntax & RE_BK_PLUS_QM)
              || (syntax & RE_LIMITED_OPS))
            goto normal_char;
        handle_plus:
        case '*':
          /* If there is no previous pattern... */
          if (!laststart)
            {
              if (syntax & RE_CONTEXT_INVALID_OPS)
                FREE_STACK_RETURN (REG_BADRPT);
              else if (!(syntax & RE_CONTEXT_INDEP_OPS))
                goto normal_char;
            }

          {
	    /* true means zero/many matches are allowed. */
	    re_bool zero_times_ok = c != '+';
            re_bool many_times_ok = c != '?';

            /* true means match shortest string possible. */
            re_bool minimal = false;

            /* If there is a sequence of repetition chars, collapse it
               down to just one (the right one).  We can't combine
               interval operators with these because of, e.g., `a{2}*',
               which should only match an even number of `a's.	*/
            while (p != pend)
              {
                PATFETCH (c);

                if (c == '*' || (!(syntax & RE_BK_PLUS_QM)
                                 && (c == '+' || c == '?')))
                  ;

                else if (syntax & RE_BK_PLUS_QM && c == '\\')
                  {
                    if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

                    PATFETCH (c1);
                    if (!(c1 == '+' || c1 == '?'))
                      {
                        PATUNFETCH;
                        PATUNFETCH;
                        break;
                      }

                    c = c1;
                  }
                else
                  {
                    PATUNFETCH;
                    break;
                  }

                /* If we get here, we found another repeat character.  */
                if (!(syntax & RE_NO_MINIMAL_MATCHING))
                  {
                    /* "*?" and "+?" and "??" are okay (and mean match
                       minimally), but other sequences (such as "*??" and
                       "+++") are rejected (reserved for future use). */
                    if (minimal || c != '?')
                      FREE_STACK_RETURN (REG_BADRPT);
                    minimal = true;
                  }
                else
                  {
                    zero_times_ok |= c != '+';
                    many_times_ok |= c != '?';
                  }
              }

            /* Star, etc. applied to an empty pattern is equivalent
               to an empty pattern.  */
            if (!laststart)
              break;

	    /* Now we know whether zero matches is allowed
	       and whether two or more matches is allowed
               and whether we want minimal or maximal matching. */
            if (minimal)
              {
                if (!many_times_ok)
                  {
                    /* "a??" becomes:
                       0: /on_failure_jump to 6
                       3: /jump to 9
                       6: /exactn/1/A
                       9: end of pattern.
                     */
                    GET_BUFFER_SPACE (6);
                    INSERT_JUMP (jump, laststart, buf_end + 3);
                    buf_end += 3;
                    INSERT_JUMP (on_failure_jump, laststart, laststart + 6);
                    buf_end += 3;
                  }
                else if (zero_times_ok)
                  {
                    /* "a*?" becomes:
                       0: /jump to 6
                       3: /exactn/1/A
                       6: /on_failure_jump to 3
                       9: end of pattern.
                     */
                    GET_BUFFER_SPACE (6);
                    INSERT_JUMP (jump, laststart, buf_end + 3);
                    buf_end += 3;
                    STORE_JUMP (on_failure_jump, buf_end, laststart + 3);
                    buf_end += 3;
                  }
                else
                  {
                    /* "a+?" becomes:
                       0: /exactn/1/A
                       3: /on_failure_jump to 0
                       6: end of pattern.
                     */
                    GET_BUFFER_SPACE (3);
                    STORE_JUMP (on_failure_jump, buf_end, laststart);
                    buf_end += 3;
                  }
              }
            else
              {
                /* Are we optimizing this jump?  */
                re_bool keep_string_p = false;

                if (many_times_ok)
                  { /* More than one repetition is allowed, so put in
                       at the end a backward relative jump from
                       `buf_end' to before the next jump we're going
                       to put in below (which jumps from laststart to
                       after this jump).

                       But if we are at the `*' in the exact sequence `.*\n',
                       insert an unconditional jump backwards to the .,
                       instead of the beginning of the loop.  This way we only
                       push a failure point once, instead of every time
                       through the loop.  */
                    assert (p - 1 > pattern);

                    /* Allocate the space for the jump.  */
                    GET_BUFFER_SPACE (3);

                    /* We know we are not at the first character of the
                       pattern, because laststart was nonzero.  And we've
                       already incremented `p', by the way, to be the
                       character after the `*'.  Do we have to do something
                       analogous here for null bytes, because of
                       RE_DOT_NOT_NULL? */
                    if (*(p - 2) == '.'
                        && zero_times_ok
                        && p < pend && *p == '\n'
                        && !(syntax & RE_DOT_NEWLINE))
                      { /* We have .*\n.  */
                        STORE_JUMP (jump, buf_end, laststart);
                        keep_string_p = true;
                      }
                    else
                      /* Anything else.  */
                      STORE_JUMP (maybe_pop_jump, buf_end, laststart - 3);

                    /* We've added more stuff to the buffer.  */
                    buf_end += 3;
                  }

                /* On failure, jump from laststart to buf_end + 3,
                   which will be the end of the buffer after this jump
                   is inserted.  */
                GET_BUFFER_SPACE (3);
                INSERT_JUMP (keep_string_p ? on_failure_keep_string_jump
					   : on_failure_jump,
                             laststart, buf_end + 3);
                buf_end += 3;

                if (!zero_times_ok)
                  {
                    /* At least one repetition is required, so insert a
                       `dummy_failure_jump' before the initial
                       `on_failure_jump' instruction of the loop. This
                       effects a skip over that instruction the first time
                       we hit that loop.  */
                    GET_BUFFER_SPACE (3);
                    INSERT_JUMP (dummy_failure_jump, laststart, laststart + 6);
                    buf_end += 3;
                  }
              }
            pending_exact = 0;
          }
	  break;


	case '.':
          laststart = buf_end;
          BUF_PUSH (anychar);
          break;

#ifdef MULE
#define MAYBE_START_OVER_WITH_EXTENDED(ch)	\
	  if (ch >= 0x80) do			\
	    {					\
	      goto start_over_with_extended;	\
	    } while (0)
#else
#define MAYBE_START_OVER_WITH_EXTENDED(ch) (void)(ch)
#endif

        case '[':
          {
	    /* XEmacs change: this whole section */
            re_bool had_char_class = false;

            if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

            /* Ensure that we have enough space to push a charset: the
               opcode, the length count, and the bitset; 34 bytes in all.  */
	    GET_BUFFER_SPACE (34);

            laststart = buf_end;

            /* We test `*p == '^' twice, instead of using an if
               statement, so we only need one BUF_PUSH.  */
            BUF_PUSH (*p == '^' ? charset_not : charset);
            if (*p == '^')
              p++;

            /* Remember the first position in the bracket expression.  */
            p1 = p;

            /* Push the number of bytes in the bitmap.  */
            BUF_PUSH ((1 << BYTEWIDTH) / BYTEWIDTH);

            /* Clear the whole map.  */
            memset (buf_end, 0, (1 << BYTEWIDTH) / BYTEWIDTH);

            /* charset_not matches newline according to a syntax bit.  */
            if ((re_opcode_t) buf_end[-2] == charset_not
                && (syntax & RE_HAT_LISTS_NOT_NEWLINE))
              SET_LIST_BIT ('\n');

            /* Read in characters and ranges, setting map bits.  */
            for (;;)
              {
                if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

                PATFETCH (c);

		/* Frumble-bumble, we may have found some extended chars.
		   Need to start over, process everything using the general
		   extended-char mechanism, and need to use charset_mule and
		   charset_mule_not instead of charset and charset_not. */
		MAYBE_START_OVER_WITH_EXTENDED (c);

                /* \ might escape characters inside [...] and [^...].  */
                if ((syntax & RE_BACKSLASH_ESCAPE_IN_LISTS) && c == '\\')
                  {
                    if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

                    PATFETCH (c1);

		    MAYBE_START_OVER_WITH_EXTENDED (c1);

                    SET_LIST_BIT (c1);
                    continue;
                  }

                /* Could be the end of the bracket expression.  If it's
                   not (i.e., when the bracket expression is `[]' so
                   far), the ']' character bit gets set way below.  */
                if (c == ']' && p != p1 + 1)
                  break;

                /* Look ahead to see if it's a range when the last thing
                   was a character class.  */
                if (had_char_class && c == '-' && *p != ']')
                  FREE_STACK_RETURN (REG_ERANGE);

                /* Look ahead to see if it's a range when the last thing
                   was a character: if this is a hyphen not at the
                   beginning or the end of a list, then it's the range
                   operator.  */
                if (c == '-'
                    && !(p - 2 >= pattern && p[-2] == '[')
		    && !(p - 3 >= pattern && p[-3] == '[' && p[-2] == '^')
                    && *p != ']')
                  {
                    reg_errcode_t ret;

		    MAYBE_START_OVER_WITH_EXTENDED (*(unsigned char *)p);

		    ret = compile_range (&p, pend, translate, syntax,
					 buf_end);

                    if (ret != REG_NOERROR) FREE_STACK_RETURN (ret);
                  }

                else if (p[0] == '-' && p[1] != ']')
                  { /* This handles ranges made up of characters only.  */
                    reg_errcode_t ret;

		    /* Move past the `-'.  */
                    PATFETCH (c1);

		    MAYBE_START_OVER_WITH_EXTENDED (*(unsigned char *)p);

		    ret = compile_range (&p, pend, translate, syntax, buf_end);

                    if (ret != REG_NOERROR) FREE_STACK_RETURN (ret);
                  }

                /* See if we're at the beginning of a possible character
                   class.  */

                else if (syntax & RE_CHAR_CLASSES && c == '[' && *p == ':')
                  { 
                    re_char *str = p + 1;

                    PATFETCH (c);
                    c1 = 0;

                    /* If pattern is `[[:'.  */
                    if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

                    for (;;)
                      {
		        PATFETCH (c);
                        if ((c == ':' && *p == ']') || p == pend)
                          {
                            break;
                          }

                        c1++;
                      }

                    /* If isn't a word bracketed by `[:' and `:]':
                       undo the ending character, the letters, and leave
                       the leading `:' and `[' (but set bits for them).  */
                    if (c == ':' && *p == ']')
                      {
			re_wctype_t cc = re_wctype (str, c1);
                        int ch;

			if (cc == RECC_ERROR)
			  FREE_STACK_RETURN (REG_ECTYPE);

                        /* Throw away the ] at the end of the character
                           class.  */
                        PATFETCH (c);

                        if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

#ifdef MULE
			if (re_wctype_can_match_non_ascii (cc))
			  {
			    goto start_over_with_extended;
			  }
#endif /* MULE */
			for (ch = 0; ch < (1 << BYTEWIDTH); ++ch)
			  {
			    if (re_iswctype (ch, cc
                                             RE_ISWCTYPE_ARG (current_buffer)))
			      {
				SET_LIST_BIT (ch);
			      }
			  }

                        had_char_class = true;
                      }
                    else
                      {
                        c1++;
                        while (c1--)
                          PATUNFETCH;
                        SET_LIST_BIT ('[');
                        SET_LIST_BIT (':');
                        had_char_class = false;
                      }
                  }
                else
                  {
                    had_char_class = false;
                    SET_LIST_BIT (c);
                  }
              }

            /* Discard any (non)matching list bytes that are all 0 at the
               end of the map.  Decrease the map-length byte too.  */
            while ((int) buf_end[-1] > 0 && buf_end[buf_end[-1] - 1] == 0)
              buf_end[-1]--;
            buf_end += buf_end[-1];
	  }
	  break;

#ifdef MULE
        start_over_with_extended:
          {
            REGISTER Lisp_Object rtab = Qnil;
            Bitbyte flags = 0;
            int bytes_needed = sizeof (flags);
            re_bool had_char_class = false;

            /* There are extended chars here, which means we need to use the
               unified range-table format. */
            if (buf_end[-2] == charset)
              buf_end[-2] = charset_mule;
            else
              buf_end[-2] = charset_mule_not;
            buf_end--;
            p = p1; /* go back to the beginning of the charset, after
                       a possible ^. */
            rtab = Vthe_lisp_rangetab;
            Fclear_range_table (rtab);

            /* charset_not matches newline according to a syntax bit.  */
            if ((re_opcode_t) buf_end[-1] == charset_mule_not
                && (syntax & RE_HAT_LISTS_NOT_NEWLINE))
              SET_RANGETAB_BIT ('\n');

            /* Read in characters and ranges, setting map bits.  */
            for (;;)
              {
                if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

                PATFETCH (c);

                /* \ might escape characters inside [...] and [^...].  */
                if ((syntax & RE_BACKSLASH_ESCAPE_IN_LISTS) && c == '\\')
                  {
                    if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

                    PATFETCH (c1);

                    SET_RANGETAB_BIT (c1);
                    continue;
                  }

                /* Could be the end of the bracket expression.  If it's
                   not (i.e., when the bracket expression is `[]' so
                   far), the ']' character bit gets set way below.  */
                if (c == ']' && p != p1 + 1)
                  break;

                /* Look ahead to see if it's a range when the last thing
                   was a character class.  */
                if (had_char_class && c == '-' && *p != ']')
                  FREE_STACK_RETURN (REG_ERANGE);

                /* Look ahead to see if it's a range when the last thing
                   was a character: if this is a hyphen not at the
                   beginning or the end of a list, then it's the range
                   operator.  */
                if (c == '-'
                    && !(p - 2 >= pattern && p[-2] == '[')
                    && !(p - 3 >= pattern && p[-3] == '[' && p[-2] == '^')
                    && *p != ']')
                  {
                    reg_errcode_t ret;

                    ret = compile_extended_range (&p, pend, translate, syntax,
                                                  rtab);

                    if (ret != REG_NOERROR) FREE_STACK_RETURN (ret);
                  }

                else if (p[0] == '-' && p[1] != ']')
                  { /* This handles ranges made up of characters only.  */
                    reg_errcode_t ret;

                    /* Move past the `-'.  */
                    PATFETCH (c1);
                    
                    ret = compile_extended_range (&p, pend, translate,
                                                  syntax, rtab);
                    if (ret != REG_NOERROR) FREE_STACK_RETURN (ret);
                  }

                /* See if we're at the beginning of a possible character
                   class.  */

                else if (syntax & RE_CHAR_CLASSES && c == '[' && *p == ':')
                  {
                    const re_char *str = p + 1;

                    PATFETCH (c);
                    c1 = 0;

                    /* If pattern is `[[:'.  */
                    if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

                    for (;;)
                      {
                        PATFETCH (c);
                        if ((c == ':' && *p == ']') || p == pend)
                          {
                            break;
                          }

                        c1++;
                      }

                    /* If isn't a word bracketed by `[:' and `:]':
                       undo the ending character, the letters, and leave
                       the leading `:' and `[' (but set bits for them).  */
                    if (c == ':' && *p == ']')
                      {
                        re_wctype_t cc = re_wctype (str, c1);
                        reg_errcode_t ret = REG_NOERROR;

                        if (cc == RECC_ERROR)
                          FREE_STACK_RETURN (REG_ECTYPE);

                        /* Throw away the ] at the end of the character
                           class.  */
                        PATFETCH (c);

                        if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

                        ret = compile_char_class (cc, rtab, &flags);

                        if (ret != REG_NOERROR) FREE_STACK_RETURN (ret);

                        had_char_class = true;
                      }
                    else
                      {
                        c1++;
                        while (c1--)
                          PATUNFETCH;
                        SET_RANGETAB_BIT ('[');
                        SET_RANGETAB_BIT (':');
                        had_char_class = false;
                      }
                  }
                else
                  {
                    had_char_class = false;
                    SET_RANGETAB_BIT (c);
                  }
              }

            bytes_needed += unified_range_table_bytes_needed (rtab);
            GET_BUFFER_SPACE (bytes_needed);
            *buf_end++ = flags;
            unified_range_table_copy_data (rtab, buf_end);
            buf_end += unified_range_table_bytes_used (buf_end);
            break;
          }
#endif /* MULE */

	case '(':
          if (syntax & RE_NO_BK_PARENS)
            goto handle_open;
          else
            goto normal_char;


        case ')':
          if (syntax & RE_NO_BK_PARENS)
            goto handle_close;
          else
            goto normal_char;


        case '\n':
          if (syntax & RE_NEWLINE_ALT)
            goto handle_alt;
          else
            goto normal_char;


	case '|':
          if (syntax & RE_NO_BK_VBAR)
            goto handle_alt;
          else
            goto normal_char;


        case '{':
           if (syntax & RE_INTERVALS && syntax & RE_NO_BK_BRACES)
             goto handle_interval;
           else
             goto normal_char;


        case '\\':
          if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

          /* Do not translate the character after the \, so that we can
             distinguish, e.g., \B from \b, even if we normally would
             translate, e.g., B to b.  */
          PATFETCH_RAW (c);

          switch (c)
            {
            case '(':
              if (syntax & RE_NO_BK_PARENS)
                goto normal_backslash;

            handle_open:
              {
                regnum_t r;
		int shy = 0;

                if (!(syntax & RE_NO_SHY_GROUPS)
                    && p != pend
                    && *p == '?')
                  {
                    p++;
                    PATFETCH (c);
                    switch (c)
                      {
                      case ':': /* shy groups */
                        shy = 1;
                        break;

                      /* All others are reserved for future constructs. */
                      default:
                        FREE_STACK_RETURN (REG_BADPAT);
                      }
                  }

		r = ++regnum;
		bufp->re_ngroups++;
		if (!shy)
		  {
		    bufp->re_nsub++;
		    while (bufp->external_to_internal_register_size <=
			   bufp->re_nsub)
		      {
			int i;
			int old_size =
			  bufp->external_to_internal_register_size;
			bufp->external_to_internal_register_size += 5;
			RETALLOC (bufp->external_to_internal_register,
				  bufp->external_to_internal_register_size,
				  int);
			/* debugging */
			for (i = old_size;
			     i < bufp->external_to_internal_register_size; i++)
			  bufp->external_to_internal_register[i] =
			    (int) 0xDEADBEEF;
		      }

		    bufp->external_to_internal_register[bufp->re_nsub] =
		      bufp->re_ngroups;
		  }

                if (COMPILE_STACK_FULL)
                  {
                    RETALLOC (compile_stack.stack, compile_stack.size << 1,
                              compile_stack_elt_t);
                    if (compile_stack.stack == NULL) return REG_ESPACE;

                    compile_stack.size <<= 1;
                  }

                /* These are the values to restore when we hit end of this
                   group.  They are all relative offsets, so that if the
                   whole pattern moves because of realloc, they will still
                   be valid.  */
                COMPILE_STACK_TOP.begalt_offset = begalt - bufp->buffer;
                COMPILE_STACK_TOP.fixup_alt_jump
                  = fixup_alt_jump ? fixup_alt_jump - bufp->buffer + 1 : 0;
                COMPILE_STACK_TOP.laststart_offset = buf_end - bufp->buffer;
                COMPILE_STACK_TOP.regnum = r;

                /* We will eventually replace the 0 with the number of
                   groups inner to this one.  But do not push a
                   start_memory for groups beyond the last one we can
                   represent in the compiled pattern.
		   #### bad bad bad.  this will fail in lots of ways, if we
		   ever have to backtrack for these groups.
		*/
                if (r <= MAX_REGNUM)
                  {
                    COMPILE_STACK_TOP.inner_group_offset
                      = buf_end - bufp->buffer + 2;
                    BUF_PUSH_3 (start_memory, r, 0);
                  }

                compile_stack.avail++;

                fixup_alt_jump = 0;
                laststart = 0;
                begalt = buf_end;
                /* If we've reached MAX_REGNUM groups, then this open
                   won't actually generate any code, so we'll have to
                   clear pending_exact explicitly.  */
                pending_exact = 0;
              }
              break;


            case ')':
              if (syntax & RE_NO_BK_PARENS) goto normal_backslash;

              if (COMPILE_STACK_EMPTY) {
                if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
                  goto normal_backslash;
                else
                  FREE_STACK_RETURN (REG_ERPAREN);
	      }

            handle_close:
              if (fixup_alt_jump)
                { /* Push a dummy failure point at the end of the
                     alternative for a possible future
                     `pop_failure_jump' to pop.  See comments at
                     `push_dummy_failure' in `re_match_2'.  */
                  BUF_PUSH (push_dummy_failure);

                  /* We allocated space for this jump when we assigned
                     to `fixup_alt_jump', in the `handle_alt' case below.  */
                  STORE_JUMP (jump_past_alt, fixup_alt_jump, buf_end - 1);
                }

              /* See similar code for backslashed left paren above.  */
              if (COMPILE_STACK_EMPTY) {
                if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
                  goto normal_char;
                else
                  FREE_STACK_RETURN (REG_ERPAREN);
	      }

              /* Since we just checked for an empty stack above, this
                 ``can't happen''.  */
              assert (compile_stack.avail != 0);
              {
                /* We don't just want to restore into `regnum', because
                   later groups should continue to be numbered higher,
                   as in `(ab)c(de)' -- the second group is #2.  */
                regnum_t this_group_regnum;

                compile_stack.avail--;
                begalt = bufp->buffer + COMPILE_STACK_TOP.begalt_offset;
                fixup_alt_jump
                  = COMPILE_STACK_TOP.fixup_alt_jump
                    ? bufp->buffer + COMPILE_STACK_TOP.fixup_alt_jump - 1
                    : 0;
                laststart = bufp->buffer + COMPILE_STACK_TOP.laststart_offset;
                this_group_regnum = COMPILE_STACK_TOP.regnum;
		/* If we've reached MAX_REGNUM groups, then this open
		   won't actually generate any code, so we'll have to
		   clear pending_exact explicitly.  */
		pending_exact = 0;

                /* We're at the end of the group, so now we know how many
                   groups were inside this one.  */
                if (this_group_regnum <= MAX_REGNUM)
                  {
                    unsigned char *inner_group_loc
                      = bufp->buffer + COMPILE_STACK_TOP.inner_group_offset;

                    *inner_group_loc = regnum - this_group_regnum;
                    BUF_PUSH_3 (stop_memory, this_group_regnum,
                                regnum - this_group_regnum);
                  }
              }
              break;


            case '|':					/* `\|'.  */
              if (syntax & RE_LIMITED_OPS || syntax & RE_NO_BK_VBAR)
                goto normal_backslash;
            handle_alt:
              if (syntax & RE_LIMITED_OPS)
                goto normal_char;

              /* Insert before the previous alternative a jump which
                 jumps to this alternative if the former fails.  */
              GET_BUFFER_SPACE (3);
              INSERT_JUMP (on_failure_jump, begalt, buf_end + 6);
              pending_exact = 0;
              buf_end += 3;

              /* The alternative before this one has a jump after it
                 which gets executed if it gets matched.  Adjust that
                 jump so it will jump to this alternative's analogous
                 jump (put in below, which in turn will jump to the next
                 (if any) alternative's such jump, etc.).  The last such
                 jump jumps to the correct final destination.  A picture:
                          _____ _____
                          |   | |   |
                          |   v |   v
                         a | b   | c

                 If we are at `b', then fixup_alt_jump right now points to a
                 three-byte space after `a'.  We'll put in the jump, set
                 fixup_alt_jump to right after `b', and leave behind three
                 bytes which we'll fill in when we get to after `c'.  */

              if (fixup_alt_jump)
                STORE_JUMP (jump_past_alt, fixup_alt_jump, buf_end);

              /* Mark and leave space for a jump after this alternative,
                 to be filled in later either by next alternative or
                 when know we're at the end of a series of alternatives.  */
              fixup_alt_jump = buf_end;
              GET_BUFFER_SPACE (3);
              buf_end += 3;

              laststart = 0;
              begalt = buf_end;
              break;


            case '{':
              /* If \{ is a literal.  */
              if (!(syntax & RE_INTERVALS)
                     /* If we're at `\{' and it's not the open-interval
                        operator.  */
                  || ((syntax & RE_INTERVALS) && (syntax & RE_NO_BK_BRACES))
                  || (p - 2 == pattern  &&  p == pend))
                goto normal_backslash;

            handle_interval:
              {
                /* If got here, then the syntax allows intervals.  */

                /* At least (most) this many matches must be made.  */
                int lower_bound = -1, upper_bound = -1;

                beg_interval = p - 1;

                if (p == pend)
                  {
                    if (syntax & RE_NO_BK_BRACES)
                      goto unfetch_interval;
                    else
                      FREE_STACK_RETURN (REG_EBRACE);
                  }

                GET_UNSIGNED_NUMBER (lower_bound);

                if (c == ',')
                  {
                    GET_UNSIGNED_NUMBER (upper_bound);
                    if (upper_bound < 0) upper_bound = RE_DUP_MAX;
                  }
                else
                  /* Interval such as `{1}' => match exactly once. */
                  upper_bound = lower_bound;

                if (lower_bound < 0 || upper_bound > RE_DUP_MAX
                    || lower_bound > upper_bound)
                  {
                    if (syntax & RE_NO_BK_BRACES)
                      goto unfetch_interval;
                    else
                      FREE_STACK_RETURN (REG_BADBR);
                  }

                if (!(syntax & RE_NO_BK_BRACES))
                  {
		    if (c != '\\')
		      FREE_STACK_RETURN (REG_BADBR);
		    if (p == pend)
		      FREE_STACK_RETURN (REG_EESCAPE);
                    PATFETCH (c);
                  }

                if (c != '}')
                  {
                    if (syntax & RE_NO_BK_BRACES)
                      goto unfetch_interval;
                    else
                      FREE_STACK_RETURN (REG_BADBR);
                  }

                /* We just parsed a valid interval.  */

                /* If it's invalid to have no preceding re.  */
                if (!laststart)
                  {
                    if (syntax & RE_CONTEXT_INVALID_OPS)
                      FREE_STACK_RETURN (REG_BADRPT);
                    else if (syntax & RE_CONTEXT_INDEP_OPS)
                      laststart = buf_end;
                    else
                      goto unfetch_interval;
                  }

                /* If the upper bound is zero, don't want to succeed at
                   all; jump from `laststart' to `b + 3', which will be
                   the end of the buffer after we insert the jump.  */
                 if (upper_bound == 0)
                   {
                     GET_BUFFER_SPACE (3);
                     INSERT_JUMP (jump, laststart, buf_end + 3);
                     buf_end += 3;
                   }

                 /* Otherwise, we have a nontrivial interval.  When
                    we're all done, the pattern will look like:
                      set_number_at <jump count> <upper bound>
                      set_number_at <succeed_n count> <lower bound>
                      succeed_n <after jump addr> <succeed_n count>
                      <body of loop>
                      jump_n <succeed_n addr> <jump count>
                    (The upper bound and `jump_n' are omitted if
                    `upper_bound' is 1, though.)  */
                 else
                   { /* If the upper bound is > 1, we need to insert
                        more at the end of the loop.  */
                     int nbytes = 10 + (upper_bound > 1) * 10;

                     GET_BUFFER_SPACE (nbytes);

                     /* Initialize lower bound of the `succeed_n', even
                        though it will be set during matching by its
                        attendant `set_number_at' (inserted next),
                        because `re_compile_fastmap' needs to know.
                        Jump to the `jump_n' we might insert below.  */
                     INSERT_JUMP2 (succeed_n, laststart,
                                   buf_end + 5 + (upper_bound > 1) * 5,
                                   lower_bound);
                     buf_end += 5;

                     /* Code to initialize the lower bound.  Insert
                        before the `succeed_n'.  The `5' is the last two
                        bytes of this `set_number_at', plus 3 bytes of
                        the following `succeed_n'.  */
                     insert_op2 (set_number_at, laststart, 5, lower_bound, buf_end);
                     buf_end += 5;

                     if (upper_bound > 1)
                       { /* More than one repetition is allowed, so
                            append a backward jump to the `succeed_n'
                            that starts this interval.

                            When we've reached this during matching,
                            we'll have matched the interval once, so
                            jump back only `upper_bound - 1' times.  */
                         STORE_JUMP2 (jump_n, buf_end, laststart + 5,
                                      upper_bound - 1);
                         buf_end += 5;

                         /* The location we want to set is the second
                            parameter of the `jump_n'; that is `b-2' as
                            an absolute address.  `laststart' will be
                            the `set_number_at' we're about to insert;
                            `laststart+3' the number to set, the source
                            for the relative address.  But we are
                            inserting into the middle of the pattern --
                            so everything is getting moved up by 5.
                            Conclusion: (b - 2) - (laststart + 3) + 5,
                            i.e., b - laststart.

                            We insert this at the beginning of the loop
                            so that if we fail during matching, we'll
                            reinitialize the bounds.  */
                         insert_op2 (set_number_at, laststart,
				     buf_end - laststart,
                                     upper_bound - 1, buf_end);
                         buf_end += 5;
                       }
                   }
                pending_exact = 0;
                beg_interval = NULL;
              }
              break;

            unfetch_interval:
              /* If an invalid interval, match the characters as literals.  */
               assert (beg_interval);
               p = beg_interval;
               beg_interval = NULL;

               /* normal_char and normal_backslash need `c'.  */
               PATFETCH (c);

               if (!(syntax & RE_NO_BK_BRACES))
                 {
                   if (p > pattern  &&  p[-1] == '\\')
                     goto normal_backslash;
                 }
               goto normal_char;

#ifdef emacs
            /* There is no way to specify the before_dot and after_dot
               operators.  rms says this is ok.  --karl  */
            case '=':
              BUF_PUSH (at_dot);
              break;

            case 's':
              laststart = buf_end;
              PATFETCH (c);
	      /* XEmacs addition */
	      if (c >= 0x80 || syntax_spec_code[c] == 0377)
		FREE_STACK_RETURN (REG_ESYNTAX);
              BUF_PUSH_2 (syntaxspec, syntax_spec_code[c]);
              break;

            case 'S':
              laststart = buf_end;
              PATFETCH (c);
	      /* XEmacs addition */
	      if (c >= 0x80 || syntax_spec_code[c] == 0377)
		FREE_STACK_RETURN (REG_ESYNTAX);
              BUF_PUSH_2 (notsyntaxspec, syntax_spec_code[c]);
              break;

#ifdef MULE
/* 97.2.17 jhod merged in to XEmacs from mule-2.3 */
	    case 'c':
	      laststart = buf_end;
	      PATFETCH_RAW (c);
	      if (c < 32 || c > 127)
		FREE_STACK_RETURN (REG_ECATEGORY);
	      BUF_PUSH_2 (categoryspec, c);
	      break;

	    case 'C':
	      laststart = buf_end;
	      PATFETCH_RAW (c);
	      if (c < 32 || c > 127)
		FREE_STACK_RETURN (REG_ECATEGORY);
	      BUF_PUSH_2 (notcategoryspec, c);
	      break;
/* end of category patch */
#endif /* MULE */
#endif /* emacs */


            case 'w':
              laststart = buf_end;
              BUF_PUSH (wordchar);
              break;


            case 'W':
              laststart = buf_end;
              BUF_PUSH (notwordchar);
              break;


            case '<':
              BUF_PUSH (wordbeg);
              break;

            case '>':
              BUF_PUSH (wordend);
              break;

            case 'b':
              BUF_PUSH (wordbound);
              break;

            case 'B':
              BUF_PUSH (notwordbound);
              break;

            case '`':
              BUF_PUSH (begbuf);
              break;

            case '\'':
              BUF_PUSH (endbuf);
              break;

            case '1': case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
	      {
		regnum_t reg = -1, regint;

		if (syntax & RE_NO_BK_REFS)
		  goto normal_char;

                PATUNFETCH;
                GET_UNSIGNED_NUMBER (reg);
		  
                /* Progressively divide down the backreference until we find
                   one that corresponds to an existing register. */
                while (reg > 10 &&
                       (syntax & RE_NO_MULTI_DIGIT_BK_REFS
                        || (reg > bufp->re_nsub)))
                  {
                    PATUNFETCH;
                    reg /= 10;
                  }

                if (reg > bufp->re_nsub)
                  {
                    /* \N with one digit with a non-existing group has always
                       been a syntax error. */
                    FREE_STACK_RETURN (REG_ESUBREG);
                  }

		regint = bufp->external_to_internal_register[reg];
		/* Can't back reference to a subexpression if inside of it.  */
		if (group_in_compile_stack (compile_stack, regint))
		  {
                    /* Check REG, not REGINT. */
                    while (reg > 10)
                      {
                        PATUNFETCH;
                        reg = reg / 10;
                      }
                    goto normal_char;
		  }

#ifdef emacs
		if (reg > 9 &&
		    bufp->warned_about_incompatible_back_references == 0)
		  {
		    bufp->warned_about_incompatible_back_references = 1;
		    warn_when_safe (intern ("regex"), Qinfo,
				    "Back reference \\%d now has new "
				    "semantics in %s", reg, pattern);
		  }
#endif

		laststart = buf_end;
		BUF_PUSH_2 (duplicate, regint);
	      }
              break;


            case '+':
            case '?':
              if (syntax & RE_BK_PLUS_QM)
                goto handle_plus;
              else
                goto normal_backslash;

            default:
            normal_backslash:
              /* You might think it would be useful for \ to mean
                 not to translate; but if we don't translate it,
                 it will never match anything.  */
              c = RE_TRANSLATE (c);
              goto normal_char;
            }
          break;


	default:
        /* Expects the character in `c'.  */
	/* `p' points to the location after where `c' came from. */
	normal_char:
	  {
	    /* The following conditional synced to GNU Emacs 22.1.  */
	    /* If no exactn currently being built.  */
	    if (!pending_exact

		/* If last exactn not at current position.  */
		|| pending_exact + *pending_exact + 1 != buf_end

		/* We have only one byte following the exactn for the count. */
		|| *pending_exact >= (1 << BYTEWIDTH) - MAX_ICHAR_LEN

		/* If followed by a repetition operator.
		   If the lookahead fails because of end of pattern, any
		   trailing backslash will get caught later.  */
		|| (p != pend && (*p == '*' || *p == '^'))
		|| ((syntax & RE_BK_PLUS_QM)
		    ? p + 1 < pend && *p == '\\' && (p[1] == '+' || p[1] == '?')
		    : p != pend && (*p == '+' || *p == '?'))
		|| ((syntax & RE_INTERVALS)
		    && ((syntax & RE_NO_BK_BRACES)
			? p != pend && *p == '{'
			: p + 1 < pend && (p[0] == '\\' && p[1] == '{'))))
	      {
		/* Start building a new exactn.  */

		laststart = buf_end;

		BUF_PUSH_2 (exactn, 0);
		pending_exact = buf_end - 1;
	      }

#ifndef MULE
	    BUF_PUSH (c);
	    (*pending_exact)++;
#else
	    {
	      Bytecount bt_count;
	      Ibyte tmp_buf[MAX_ICHAR_LEN];
	      int i;

	      bt_count = set_itext_ichar (tmp_buf, c);

	      for (i = 0; i < bt_count; i++)
		{
		  BUF_PUSH (tmp_buf[i]);
		  (*pending_exact)++;
		}
	    }
#endif
	    break;
	  }
        } /* switch (c) */
    } /* while p != pend */


  /* Through the pattern now.  */

  if (fixup_alt_jump)
    STORE_JUMP (jump_past_alt, fixup_alt_jump, buf_end);

  if (!COMPILE_STACK_EMPTY)
    FREE_STACK_RETURN (REG_EPAREN);

  /* If we don't want backtracking, force success
     the first time we reach the end of the compiled pattern.  */
  if (syntax & RE_NO_POSIX_BACKTRACKING)
    BUF_PUSH (succeed);

  xfree (compile_stack.stack);

  /* We have succeeded; set the length of the buffer.  */
  bufp->used = buf_end - bufp->buffer;

#ifdef DEBUG
  if (debug_regexps & RE_DEBUG_COMPILATION)
    {
      DEBUG_PRINT1 ("\nCompiled pattern: \n");
      print_compiled_pattern (bufp);
    }
#endif /* DEBUG */

#ifndef MATCH_MAY_ALLOCATE
  /* Initialize the failure stack to the largest possible stack.  This
     isn't necessary unless we're trying to avoid calling alloca in
     the search and match routines.  */
  {
    int num_regs = bufp->re_ngroups + 1;

    /* Since DOUBLE_FAIL_STACK refuses to double only if the current size
       is strictly greater than re_max_failures, the largest possible stack
       is 2 * re_max_failures failure points.  */
    if (fail_stack.size < (2 * re_max_failures * MAX_FAILURE_ITEMS))
      {
	fail_stack.size = (2 * re_max_failures * MAX_FAILURE_ITEMS);

	if (! fail_stack.stack)
	  fail_stack.stack
	    = (fail_stack_elt_t *) xmalloc (fail_stack.size
					    * sizeof (fail_stack_elt_t));
	else
	  fail_stack.stack
	    = (fail_stack_elt_t *) xrealloc (fail_stack.stack,
					     (fail_stack.size
					      * sizeof (fail_stack_elt_t)));
      }

    regex_grow_registers (num_regs);
  }
#endif /* not MATCH_MAY_ALLOCATE */

  return REG_NOERROR;
} /* regex_compile */

/* Subroutines for `regex_compile'.  */

/* Store OP at LOC followed by two-byte integer parameter ARG.  */

static void
store_op1 (re_opcode_t op, unsigned char *loc, int arg)
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg);
}


/* Like `store_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
store_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2)
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg1);
  STORE_NUMBER (loc + 3, arg2);
}


/* Copy the bytes from LOC to END to open up three bytes of space at LOC
   for OP followed by two-byte integer parameter ARG.  */

static void
insert_op1 (re_opcode_t op, unsigned char *loc, int arg, unsigned char *end)
{
  REGISTER unsigned char *pfrom = end;
  REGISTER unsigned char *pto = end + 3;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op1 (op, loc, arg);
}


/* Like `insert_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
insert_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2,
	    unsigned char *end)
{
  REGISTER unsigned char *pfrom = end;
  REGISTER unsigned char *pto = end + 5;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op2 (op, loc, arg1, arg2);
}


/* P points to just after a ^ in PATTERN.  Return true if that ^ comes
   after an alternative or a begin-subexpression.  We assume there is at
   least one character before the ^.  */

static re_bool
at_begline_loc_p (re_char *pattern, re_char *p, reg_syntax_t syntax)
{
  re_char *prev = p - 2;
  re_bool prev_prev_backslash = prev > pattern && prev[-1] == '\\';

  return
       /* After a subexpression?  */
       (*prev == '(' && (syntax & RE_NO_BK_PARENS || prev_prev_backslash))
       /* After an alternative?  */
    || (*prev == '|' && (syntax & RE_NO_BK_VBAR || prev_prev_backslash));
}


/* The dual of at_begline_loc_p.  This one is for $.  We assume there is
   at least one character after the $, i.e., `P < PEND'.  */

static re_bool
at_endline_loc_p (re_char *p, re_char *pend, int syntax)
{
  re_char *next = p;
  re_bool next_backslash = *next == '\\';
  re_char *next_next = p + 1 < pend ? p + 1 : 0;

  return
       /* Before a subexpression?  */
       (syntax & RE_NO_BK_PARENS ? *next == ')'
        : next_backslash && next_next && *next_next == ')')
       /* Before an alternative?  */
    || (syntax & RE_NO_BK_VBAR ? *next == '|'
        : next_backslash && next_next && *next_next == '|');
}


/* Returns true if REGNUM is in one of COMPILE_STACK's elements and
   false if it's not.  */

static re_bool
group_in_compile_stack (compile_stack_type compile_stack, regnum_t regnum)
{
  int this_element;

  for (this_element = compile_stack.avail - 1;
       this_element >= 0;
       this_element--)
    if (compile_stack.stack[this_element].regnum == regnum)
      return true;

  return false;
}


/* Read the ending character of a range (in a bracket expression) from the
   uncompiled pattern *P_PTR (which ends at PEND).  We assume the
   starting character is in `P[-2]'.  (`P[-1]' is the character `-'.)
   Then we set the translation of all bits between the starting and
   ending characters (inclusive) in the compiled pattern B.

   Return an error code.

   We use these short variable names so we can use the same macros as
   `regex_compile' itself.

   Under Mule, this is only called when both chars of the range are
   ASCII. */

static reg_errcode_t
compile_range (re_char **p_ptr, re_char *pend, RE_TRANSLATE_TYPE translate,
	       reg_syntax_t syntax, unsigned char *buf_end)
{
  Ichar this_char;

  re_char *p = *p_ptr;
  int range_start, range_end;

  if (p == pend)
    return REG_ERANGE;

  /* Even though the pattern is a signed `char *', we need to fetch
     with unsigned char *'s; if the high bit of the pattern character
     is set, the range endpoints will be negative if we fetch using a
     signed char *.

     We also want to fetch the endpoints without translating them; the
     appropriate translation is done in the bit-setting loop below.  */
  /* The SVR4 compiler on the 3B2 had trouble with unsigned const char *.  */
  range_start = ((const unsigned char *) p)[-2];
  range_end   = ((const unsigned char *) p)[0];

  /* Have to increment the pointer into the pattern string, so the
     caller isn't still at the ending character.  */
  (*p_ptr)++;

  /* If the start is after the end, the range is empty.  */
  if (range_start > range_end)
    return syntax & RE_NO_EMPTY_RANGES ? REG_ERANGE : REG_NOERROR;

  /* Here we see why `this_char' has to be larger than an `unsigned
     char' -- the range is inclusive, so if `range_end' == 0xff
     (assuming 8-bit characters), we would otherwise go into an infinite
     loop, since all characters <= 0xff.  */
  for (this_char = range_start; this_char <= range_end; this_char++)
    {
      SET_LIST_BIT (RE_TRANSLATE (this_char));
    }

  return REG_NOERROR;
}

#ifdef MULE

static reg_errcode_t
compile_extended_range (re_char **p_ptr, re_char *pend,
			RE_TRANSLATE_TYPE translate,
			reg_syntax_t syntax, Lisp_Object rtab)
{
  Ichar this_char, range_start, range_end;
  const Ibyte *p;

  if (*p_ptr == pend)
    return REG_ERANGE;

  p = (const Ibyte *) *p_ptr;
  range_end = itext_ichar (p);
  p--; /* back to '-' */
  DEC_IBYTEPTR (p); /* back to start of range */
  /* We also want to fetch the endpoints without translating them; the
     appropriate translation is done in the bit-setting loop below.  */
  range_start = itext_ichar (p);
  INC_IBYTEPTR (*p_ptr);

  /* If the start is after the end, the range is empty.  */
  if (range_start > range_end)
    return syntax & RE_NO_EMPTY_RANGES ? REG_ERANGE : REG_NOERROR;

  /* Can't have ranges spanning different charsets, except maybe for
     ranges entirely within the first 256 chars. */

  if ((range_start >= 0x100 || range_end >= 0x100)
      && ichar_leading_byte (range_start) !=
      ichar_leading_byte (range_end))
    return REG_ERANGESPAN;

  /* #### This might be way inefficient if the range encompasses 10,000
     chars or something.  To be efficient, you'd have to do something like
     this:

     range_table a;
     range_table b;
     map over translation table in [range_start, range_end] of
       (put the mapped range in a;
        put the translation in b)
     invert the range in a and truncate to [range_start, range_end]
     compute the union of a, b
     union the result into rtab
   */
  for (this_char = range_start; this_char <= range_end; this_char++)
    {
      SET_RANGETAB_BIT (RE_TRANSLATE (this_char));
    }

  if (this_char <= range_end)
    put_range_table (rtab, this_char, range_end, Qt);

  return REG_NOERROR;
}

#endif /* MULE */

#ifdef emacs

reg_errcode_t
compile_char_class (re_wctype_t cc, Lisp_Object rtab, Bitbyte *flags_out)
{
  *flags_out |= re_wctype_to_bit (cc);

  switch (cc)
    {
    case RECC_ASCII:
      put_range_table (rtab, 0, 0x7f, Qt);
      break;

    case RECC_XDIGIT:
      put_range_table (rtab, 'a', 'f', Qt);
      put_range_table (rtab, 'A', 'f', Qt);
      /* fallthrough */
    case RECC_DIGIT:
      put_range_table (rtab, '0', '9', Qt);
      break;

    case RECC_BLANK:
      put_range_table (rtab, ' ', ' ', Qt);
      put_range_table (rtab, '\t', '\t', Qt);
      break;

    case RECC_PRINT:
      put_range_table (rtab, ' ', 0x7e, Qt);
      put_range_table (rtab, 0x80, MOST_POSITIVE_FIXNUM, Qt);
      break;

    case RECC_GRAPH:
      put_range_table (rtab, '!', 0x7e, Qt);
      put_range_table (rtab, 0x80, MOST_POSITIVE_FIXNUM, Qt);
      break;

    case RECC_NONASCII:
    case RECC_MULTIBYTE:
      put_range_table (rtab, 0x80, MOST_POSITIVE_FIXNUM, Qt);
      break;

    case RECC_CNTRL:
      put_range_table (rtab, 0x00, 0x1f, Qt);
      break;

    case RECC_UNIBYTE:
      /* Never true in XEmacs. */
      break;

      /* The following all have their own bits in the class_bits argument to
         charset_mule and charset_mule_not, they don't use the range table
         information. */
    case RECC_ALPHA:
    case RECC_WORD:
    case RECC_ALNUM: /* Equivalent to RECC_WORD */
    case RECC_LOWER:
    case RECC_PUNCT:
    case RECC_SPACE:
    case RECC_UPPER:
      break;
    }

  return REG_NOERROR;
}

#endif /* MULE */

/* re_compile_fastmap computes a ``fastmap'' for the compiled pattern in
   BUFP.  A fastmap records which of the (1 << BYTEWIDTH) possible
   characters can start a string that matches the pattern.  This fastmap
   is used by re_search to skip quickly over impossible starting points.

   The caller must supply the address of a (1 << BYTEWIDTH)-byte data
   area as BUFP->fastmap.

   We set the `fastmap', `fastmap_accurate', and `can_be_null' fields in
   the pattern buffer.

   Returns 0 if we succeed, -2 if an internal error.   */

int
re_compile_fastmap (struct re_pattern_buffer *bufp
		    RE_LISP_SHORT_CONTEXT_ARGS_DECL)
{
  int j, k;
#ifdef MATCH_MAY_ALLOCATE
  fail_stack_type fail_stack;
#endif
  DECLARE_DESTINATION;
  /* We don't push any register information onto the failure stack.  */

  /* &&#### this should be changed for 8-bit-fixed, for efficiency.  see
     comment marked with &&#### in re_search_2. */
    
  REGISTER char *fastmap = bufp->fastmap;
  unsigned char *pattern = bufp->buffer;
  long size = bufp->used;
  unsigned char *p = pattern;
  REGISTER unsigned char *pend = pattern + size;

#ifdef REGEX_REL_ALLOC
  /* This holds the pointer to the failure stack, when
     it is allocated relocatably.  */
  fail_stack_elt_t *failure_stack_ptr;
#endif

  /* Assume that each path through the pattern can be null until
     proven otherwise.  We set this false at the bottom of switch
     statement, to which we get only if a particular path doesn't
     match the empty string.  */
  re_bool path_can_be_null = true;

  /* We aren't doing a `succeed_n' to begin with.  */
  re_bool succeed_n_p = false;

#ifdef ERROR_CHECK_MALLOC
  /* The pattern comes from string data, not buffer data.  We don't access
     any buffer data, so we don't have to worry about malloc() (but the
     disallowed flag may have been set by a caller). */
  int depth = bind_regex_malloc_disallowed (0);
#endif

  assert (fastmap != NULL && p != NULL);

  INIT_FAIL_STACK ();
  memset (fastmap, 0, 1 << BYTEWIDTH);  /* Assume nothing's valid.  */
  bufp->fastmap_accurate = 1;	    /* It will be when we're done.  */
  bufp->can_be_null = 0;

  while (1)
    {
      if (p == pend || *p == succeed)
	{
	  /* We have reached the (effective) end of pattern.  */
	  if (!FAIL_STACK_EMPTY ())
	    {
	      bufp->can_be_null |= path_can_be_null;

	      /* Reset for next path.  */
	      path_can_be_null = true;

	      p = (unsigned char *) fail_stack.stack[--fail_stack.avail].pointer;

	      continue;
	    }
	  else
	    break;
	}

      /* We should never be about to go beyond the end of the pattern.  */
      assert (p < pend);

      switch ((re_opcode_t) *p++)
	{

        /* I guess the idea here is to simply not bother with a fastmap
           if a backreference is used, since it's too hard to figure out
           the fastmap for the corresponding group.  Setting
           `can_be_null' stops `re_search_2' from using the fastmap, so
           that is all we do.  */
	case duplicate:
	  bufp->can_be_null = 1;
          goto done;


      /* Following are the cases which match a character.  These end
         with `break'.  */

	case exactn:
          fastmap[p[1]] = 1;
	  break;


        case charset:
	  /* XEmacs: Under Mule, these bit vectors will
	     only contain values for characters below 0x80. */
          for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH)))
              fastmap[j] = 1;
	  break;


	case charset_not:
	  /* Chars beyond end of map must be allowed.  */
#ifdef MULE
	  for (j = *p * BYTEWIDTH; j < 0x80; j++)
            fastmap[j] = 1;
	  /* And all extended characters must be allowed, too. */
	  for (j = 0x80; j < 0xA0; j++)
	    fastmap[j] = 1;
#else /* not MULE */
	  for (j = *p * BYTEWIDTH; j < (1 << BYTEWIDTH); j++)
            fastmap[j] = 1;
#endif /* MULE */

	  for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))))
              fastmap[j] = 1;
          break;

#ifdef MULE
	case charset_mule:
	  {
	    int nentries;
	    int i;
	    Bitbyte flags = *p++;

	    if (flags)
	      {
                /* We need to consult the syntax table, fastmap won't
                   work. */
                bufp->can_be_null = 1;
                goto done;
	      }

	    nentries = unified_range_table_nentries (p);
	    for (i = 0; i < nentries; i++)
	      {
		EMACS_INT first, last;
		Lisp_Object dummy_val;
		int jj;
		Ibyte strr[MAX_ICHAR_LEN];

		unified_range_table_get_range (p, i, &first, &last,
					       &dummy_val);
		for (jj = first; jj <= last && jj < 0x80; jj++)
		  fastmap[jj] = 1;
		/* Ranges below 0x100 can span charsets, but there
		   are only two (Control-1 and Latin-1), and
		   either first or last has to be in them. */
		set_itext_ichar (strr, first);
		fastmap[*strr] = 1;
		if (last < 0x100)
		  {
		    set_itext_ichar (strr, last);
		    fastmap[*strr] = 1;
		  }
                else if (MOST_POSITIVE_FIXNUM == last)
                  {
		    /* This is RECC_MULTIBYTE or RECC_NONASCII; true for all
                       non-ASCII characters. */
		    jj = 0x80;
		    while (jj < 0xA0)
		      {
			fastmap[jj++] = 1;
		      }
                  }
	      }
	  }
	  break;

	case charset_mule_not:
	  {
	    int nentries;
	    int i;
	    int smallest_prev = 0;
	    Bitbyte flags = *p++;

	    if (flags)
              {
                /* We need to consult the syntax table, fastmap won't
                   work. */
                bufp->can_be_null = 1;
                goto done;
              }

	    nentries = unified_range_table_nentries (p);
	    for (i = 0; i < nentries; i++)
	      {
		EMACS_INT first, last;
		Lisp_Object dummy_val;
		int jj;

		unified_range_table_get_range (p, i, &first, &last,
					       &dummy_val);
		for (jj = smallest_prev; jj < first && jj < 0x80; jj++)
		  fastmap[jj] = 1;
		smallest_prev = last + 1;
		if (smallest_prev >= 0x80)
		  break;
	      }

	    /* Also set lead bytes after the end */
	    for (i = smallest_prev; i < 0x80; i++)
	      fastmap[i] = 1;

	    /* Calculating which leading bytes are actually allowed
	       here is rather difficult, so we just punt and allow
	       all of them. */
	    for (i = 0x80; i < 0xA0; i++)
	      fastmap[i] = 1;
	  }
	  break;
#endif /* MULE */


        case anychar:
	  {
	    int fastmap_newline = fastmap['\n'];

	    /* `.' matches anything ...  */
#ifdef MULE
	    /* "anything" only includes bytes that can be the
	       first byte of a character. */
	    for (j = 0; j < 0xA0; j++)
	      fastmap[j] = 1;
#else
	    for (j = 0; j < (1 << BYTEWIDTH); j++)
	      fastmap[j] = 1;
#endif

	    /* ... except perhaps newline.  */
	    if (!(bufp->syntax & RE_DOT_NEWLINE))
	      fastmap['\n'] = fastmap_newline;

	    /* Return if we have already set `can_be_null'; if we have,
	       then the fastmap is irrelevant.  Something's wrong here.  */
	    else if (bufp->can_be_null)
	      goto done;

	    /* Otherwise, have to check alternative paths.  */
	    break;
	  }

#ifndef emacs
	case wordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (ignored, j) == Sword)
	      fastmap[j] = 1;
	  break;

	case notwordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (ignored, j) != Sword)
	      fastmap[j] = 1;
	  break;
#else /* emacs */
	case wordchar:
	case notwordchar:
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
	case notsyntaxspec:
	case syntaxspec:
	  /* This match depends on text properties.  These end with
	     aborting optimizations.  */
	  bufp->can_be_null = 1;
	  goto done;
#if 0 /* all of the following code is unused now that the `syntax-table'
	 property exists -- it's trickier to do this than just look in
	 the buffer.  &&#### but we could just use the syntax-cache stuff
	 instead; why don't we? --ben */
	case wordchar:
	  k = (int) Sword;
	  goto matchsyntax;

	case notwordchar:
	  k = (int) Sword;
	  goto matchnotsyntax;
	  
        case syntaxspec:
	  k = *p++;
	matchsyntax:
#ifdef MULE
	  for (j = 0; j < 0x80; j++)
	    if (SYNTAX
		(XCHAR_TABLE (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf)), j) ==
		(enum syntaxcode) k)
	      fastmap[j] = 1;
	  for (j = 0x80; j < 0xA0; j++)
	    {
	      if (leading_byte_prefix_p ((unsigned char) j))
		/* too complicated to calculate this right */
		fastmap[j] = 1;
	      else
		{
		  int multi_p;
		  Lisp_Object cset;

		  cset = charset_by_leading_byte (j);
		  if (CHARSETP (cset))
		    {
		      if (charset_syntax (lispbuf, cset, &multi_p)
			  == Sword || multi_p)
			fastmap[j] = 1;
		    }
		}
	    }
#else /* not MULE */
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX
		(XCHAR_TABLE (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf)), j) ==
		(enum syntaxcode) k)
	      fastmap[j] = 1;
#endif /* MULE */
	  break;


	case notsyntaxspec:
	  k = *p++;
	matchnotsyntax:
#ifdef MULE
	  for (j = 0; j < 0x80; j++)
	    if (SYNTAX
		(XCHAR_TABLE
		 (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf)), j) !=
		(enum syntaxcode) k)
	      fastmap[j] = 1;
	  for (j = 0x80; j < 0xA0; j++)
	    {
	      if (leading_byte_prefix_p ((unsigned char) j))
		/* too complicated to calculate this right */
		fastmap[j] = 1;
	      else
		{
		  int multi_p;
		  Lisp_Object cset;

		  cset = charset_by_leading_byte (j);
		  if (CHARSETP (cset))
		    {
		      if (charset_syntax (lispbuf, cset, &multi_p)
			  != Sword || multi_p)
			fastmap[j] = 1;
		    }
		}
	    }
#else /* not MULE */
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX
		(XCHAR_TABLE
		 (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf)), j) !=
		(enum syntaxcode) k)
	      fastmap[j] = 1;
#endif /* MULE */
	  break;
#endif /* 0 */

#ifdef MULE
/* 97/2/17 jhod category patch */
	case categoryspec:
	case notcategoryspec:
	  bufp->can_be_null = 1;
	  UNBIND_REGEX_MALLOC_CHECK ();
	  return 0;
/* end if category patch */
#endif /* MULE */

      /* All cases after this match the empty string.  These end with
         `continue'.  */
	case before_dot:
	case at_dot:
	case after_dot:
          continue;
#endif /* emacs */


        case no_op:
        case begline:
        case endline:
	case begbuf:
	case endbuf:
#ifndef emacs
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
#endif
        case push_dummy_failure:
          continue;


	case jump_n:
        case pop_failure_jump:
	case maybe_pop_jump:
	case jump:
        case jump_past_alt:
	case dummy_failure_jump:
          EXTRACT_NUMBER_AND_INCR (j, p);
	  p += j;
	  if (j > 0)
	    continue;

          /* Jump backward implies we just went through the body of a
             loop and matched nothing.  Opcode jumped to should be
             `on_failure_jump' or `succeed_n'.  Just treat it like an
             ordinary jump.  For a * loop, it has pushed its failure
             point already; if so, discard that as redundant.  */
          if ((re_opcode_t) *p != on_failure_jump
	      && (re_opcode_t) *p != succeed_n)
	    continue;

          p++;
          EXTRACT_NUMBER_AND_INCR (j, p);
          p += j;

          /* If what's on the stack is where we are now, pop it.  */
          if (!FAIL_STACK_EMPTY ()
	      && fail_stack.stack[fail_stack.avail - 1].pointer == p)
            fail_stack.avail--;

          continue;


        case on_failure_jump:
        case on_failure_keep_string_jump:
	handle_on_failure_jump:
          EXTRACT_NUMBER_AND_INCR (j, p);

          /* For some patterns, e.g., `(a?)?', `p+j' here points to the
             end of the pattern.  We don't want to push such a point,
             since when we restore it above, entering the switch will
             increment `p' past the end of the pattern.  We don't need
             to push such a point since we obviously won't find any more
             fastmap entries beyond `pend'.  Such a pattern can match
             the null string, though.  */
          if (p + j < pend)
            {
              if (!PUSH_PATTERN_OP (p + j, fail_stack))
		{
		  RESET_FAIL_STACK ();
		  UNBIND_REGEX_MALLOC_CHECK ();
		  return -2;
		}
            }
          else
            bufp->can_be_null = 1;

          if (succeed_n_p)
            {
              EXTRACT_NUMBER_AND_INCR (k, p);	/* Skip the n.  */
              succeed_n_p = false;
	    }

          continue;


	case succeed_n:
          /* Get to the number of times to succeed.  */
          p += 2;

          /* Increment p past the n for when k != 0.  */
          EXTRACT_NUMBER_AND_INCR (k, p);
          if (k == 0)
	    {
              p -= 4;
  	      succeed_n_p = true;  /* Spaghetti code alert.  */
              goto handle_on_failure_jump;
            }
          continue;


	case set_number_at:
          p += 4;
          continue;


	case start_memory:
        case stop_memory:
	  p += 2;
	  continue;


	default:
          ABORT (); /* We have listed all the cases.  */
        } /* switch *p++ */

      /* Getting here means we have found the possible starting
         characters for one path of the pattern -- and that the empty
         string does not match.  We need not follow this path further.
         Instead, look at the next alternative (remembered on the
         stack), or quit if no more.  The test at the top of the loop
         does these things.  */
      path_can_be_null = false;
      p = pend;
    } /* while p */

  /* Set `can_be_null' for the last path (also the first path, if the
     pattern is empty).  */
  bufp->can_be_null |= path_can_be_null;

 done:
  RESET_FAIL_STACK ();
  UNBIND_REGEX_MALLOC_CHECK ();
  return 0;
} /* re_compile_fastmap */

/* Set REGS to hold NUM_REGS registers, storing them in STARTS and
   ENDS.  Subsequent matches using PATTERN_BUFFER and REGS will use
   this memory for recording register information.  STARTS and ENDS
   must be allocated using the malloc library routine, and must each
   be at least NUM_REGS * sizeof (regoff_t) bytes long.

   If NUM_REGS == 0, then subsequent matches should allocate their own
   register data.

   Unless this function is called, the first search or match using
   PATTERN_BUFFER will allocate its own register data, without
   freeing the old data.  */

void
re_set_registers (struct re_pattern_buffer *bufp, struct re_registers *regs,
		  int num_regs, regoff_t *starts, regoff_t *ends)
{
  if (num_regs)
    {
      bufp->regs_allocated = REGS_REALLOCATE;
      regs->num_regs = num_regs;
      regs->start = starts;
      regs->end = ends;
    }
  else
    {
      bufp->regs_allocated = REGS_UNALLOCATED;
      regs->num_regs = 0;
      regs->start = regs->end = (regoff_t *) 0;
    }
}

/* Searching routines.  */

/* Like re_search_2, below, but only one string is specified, and
   doesn't let you say where to stop matching. */

int
re_search (struct re_pattern_buffer *bufp, const char *string, int size,
	   int startpos, int range, struct re_registers *regs
	   RE_LISP_CONTEXT_ARGS_DECL)
{
  return re_search_2 (bufp, NULL, 0, string, size, startpos, range,
		      regs, size RE_LISP_CONTEXT_ARGS);
}

/* Using the compiled pattern in BUFP->buffer, first tries to match the
   virtual concatenation of STRING1 and STRING2, starting first at index
   STARTPOS, then at STARTPOS + 1, and so on.

   STRING1 and STRING2 have length SIZE1 and SIZE2, respectively.

   RANGE is how far to scan while trying to match.  RANGE = 0 means try
   only at STARTPOS; in general, the last start tried is STARTPOS +
   RANGE.

   All sizes and positions refer to bytes (not chars); under Mule, the code
   knows about the format of the text and will only check at positions
   where a character starts.

   With MULE, RANGE is a byte position, not a char position.  The last
   start tried is the character starting <= STARTPOS + RANGE.

   In REGS, return the indices of the virtual concatenation of STRING1
   and STRING2 that matched the entire BUFP->buffer and its contained
   subexpressions.

   Do not consider matching one past the index STOP in the virtual
   concatenation of STRING1 and STRING2.

   We return either the position in the strings at which the match was
   found, -1 if no match, or -2 if error (such as failure
   stack overflow).  */

int
re_search_2 (struct re_pattern_buffer *bufp, const char *str1,
	     int size1, const char *str2, int size2, int startpos,
	     int range, struct re_registers *regs, int stop
	     RE_LISP_CONTEXT_ARGS_DECL)
{
  int val;
  re_char *string1 = (re_char *) str1;
  re_char *string2 = (re_char *) str2;
  REGISTER char *fastmap = bufp->fastmap;
  REGISTER RE_TRANSLATE_TYPE translate = bufp->translate;
  int total_size = size1 + size2;
  int endpos = startpos + range;
#ifdef REGEX_BEGLINE_CHECK
  int anchored_at_begline = 0;
#endif
  re_char *d;
#ifdef emacs
  Internal_Format fmt = buffer_or_other_internal_format (lispobj);
#ifdef REL_ALLOC
  Ibyte *orig_buftext =
    BUFFERP (lispobj) ?
    BYTE_BUF_BYTE_ADDRESS (XBUFFER (lispobj),
			   BYTE_BUF_BEGV (XBUFFER (lispobj))) :
    0;
#endif
#ifdef ERROR_CHECK_MALLOC
  int depth;
#endif
#endif /* emacs */
#if 1
  int forward_search_p;
#endif

  /* Check for out-of-range STARTPOS.  */
  if (startpos < 0 || startpos > total_size)
    return -1;

  /* Fix up RANGE if it might eventually take us outside
     the virtual concatenation of STRING1 and STRING2.  */
  if (endpos < 0)
    range = 0 - startpos;
  else if (endpos > total_size)
    range = total_size - startpos;

#if 1
  forward_search_p = range > 0;
#endif

  /* If the search isn't to be a backwards one, don't waste time in a
     search for a pattern that must be anchored.  */
  if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == begbuf && range > 0)
    {
      if (startpos > 0)
	return -1;
      else
	{
	  d = ((const unsigned char *)
	       (startpos >= size1 ? string2 - size1 : string1) + startpos);
	  range = itext_ichar_len_fmt (d, fmt);
	}
    }

#ifdef emacs
  /* In a forward search for something that starts with \=.
     don't keep searching past point.  */
  if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == at_dot && range > 0)
    {
      if (!BUFFERP (lispobj))
	return -1;
      range = (BYTE_BUF_PT (XBUFFER (lispobj))
	       - BYTE_BUF_BEGV (XBUFFER (lispobj)) - startpos);
      if (range < 0)
	return -1;
    }
#endif /* emacs */

#ifdef ERROR_CHECK_MALLOC
  /* Do this after the above return()s. */
  depth = bind_regex_malloc_disallowed (1);
#endif

  /* Update the fastmap now if not correct already.  */
  BEGIN_REGEX_MALLOC_OK ();
  if (fastmap && !bufp->fastmap_accurate)
    if (re_compile_fastmap (bufp RE_LISP_SHORT_CONTEXT_ARGS) == -2)
      {
	END_REGEX_MALLOC_OK ();
	UNBIND_REGEX_MALLOC_CHECK ();
	return -2;
      }

  END_REGEX_MALLOC_OK ();
  RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS ();

#ifdef REGEX_BEGLINE_CHECK
  {
    long i = 0;

    while (i < bufp->used)
      {
	if (bufp->buffer[i] == start_memory ||
	    bufp->buffer[i] == stop_memory)
	  i += 2;
	else
	  break;
      }
    anchored_at_begline = i < bufp->used && bufp->buffer[i] == begline;
  }
#endif

#ifdef emacs
  BEGIN_REGEX_MALLOC_OK ();
  update_mirror_syntax_if_dirty (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf));
  scache = setup_syntax_cache (scache, lispobj, lispbuf,
			       offset_to_charxpos (lispobj, startpos),
			       1);
  END_REGEX_MALLOC_OK ();
  RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
#endif

  /* Loop through the string, looking for a place to start matching.  */
  for (;;)
    {
#ifdef REGEX_BEGLINE_CHECK
      /* If the regex is anchored at the beginning of a line (i.e. with a
	 ^), then we can speed things up by skipping to the next
	 beginning-of-line.  However, to determine "beginning of line" we
	 need to look at the previous char, so can't do this check if at
	 beginning of either string. (Well, we could if at the beginning of
	 the second string, but it would require additional code, and this
	 is just an optimization.) */
      if (anchored_at_begline && startpos > 0 && startpos != size1)
	{
	  if (range > 0)
	    {
	      /* whose stupid idea was it anyway to make this
		 function take two strings to match?? */
	      int lim = 0;
	      re_char *orig_d;
	      re_char *stop_d;

	      /* Compute limit as below in fastmap code, so we are guaranteed
		 to remain within a single string. */
	      if (startpos < size1 && startpos + range >= size1)
		lim = range - (size1 - startpos);

	      d = ((const unsigned char *)
		   (startpos >= size1 ? string2 - size1 : string1) + startpos);
	      orig_d = d;
	      stop_d = d + range - lim;

	      /* We want to find the next location (including the current
		 one) where the previous char is a newline, so back up one
		 and search forward for a newline. */
	      DEC_IBYTEPTR_FMT (d, fmt);	/* Ok, since startpos != size1. */

	      /* Written out as an if-else to avoid testing `translate'
		 inside the loop.  */
	      if (TRANSLATE_P (translate))
		while (d < stop_d &&
		       RE_TRANSLATE_1 (itext_ichar_fmt (d, fmt, lispobj))
		       != '\n')
		  INC_IBYTEPTR_FMT (d, fmt);
	      else
		while (d < stop_d &&
		       itext_ichar_ascii_fmt (d, fmt, lispobj) != '\n')
		  INC_IBYTEPTR_FMT (d, fmt);

	      /* If we were stopped by a newline, skip forward over it.
		 Otherwise we will get in an infloop when our start position
		 was at begline. */
	      if (d < stop_d)
		INC_IBYTEPTR_FMT (d, fmt);
	      range -= d - orig_d;
	      startpos += d - orig_d;
#if 1
	      assert (!forward_search_p || range >= 0);
#endif
	    }
	  else if (range < 0)
	    {
	      /* We're lazy, like in the fastmap code below */
	      Ichar c;

	      d = ((const unsigned char *)
		   (startpos >= size1 ? string2 - size1 : string1) + startpos);
	      DEC_IBYTEPTR_FMT (d, fmt);
	      c = itext_ichar_fmt (d, fmt, lispobj);
	      c = RE_TRANSLATE (c);
	      if (c != '\n')
		goto advance;
	    }
	}
#endif /* REGEX_BEGLINE_CHECK */

      /* If a fastmap is supplied, skip quickly over characters that
         cannot be the start of a match.  If the pattern can match the
         null string, however, we don't need to skip characters; we want
         the first null string.  */
      if (fastmap && startpos < total_size && !bufp->can_be_null)
	{
	  /* For the moment, fastmap always works as if buffer
	     is in default format, so convert chars in the search strings
	     into default format as we go along, if necessary.

	     &&#### fastmap needs rethinking for 8-bit-fixed so
	     it's faster.  We need it to reflect the raw
	     8-bit-fixed values.  That isn't so hard if we assume
	     that the top 96 bytes represent a single 1-byte
	     charset.  For 16-bit/32-bit stuff it's probably not
	     worth it to make the fastmap represent the raw, due to
	     its nature -- we'd have to use the LSB for the
	     fastmap, and that causes lots of problems with Mule
	     chars, where it essentially wipes out the usefulness
	     of the fastmap entirely. */
	  if (range > 0)	/* Searching forwards.  */
	    {
	      int lim = 0;
	      int irange = range;

              if (startpos < size1 && startpos + range >= size1)
                lim = range - (size1 - startpos);

	      d = ((const unsigned char *)
		   (startpos >= size1 ? string2 - size1 : string1) + startpos);

              /* Written out as an if-else to avoid testing `translate'
                 inside the loop.  */
	      if (TRANSLATE_P (translate))
		{
		  while (range > lim)
		    {
		      re_char *old_d = d;
#ifdef MULE
		      Ibyte tempch[MAX_ICHAR_LEN];
		      Ichar buf_ch =
			RE_TRANSLATE_1 (itext_ichar_fmt (d, fmt, lispobj));
		      set_itext_ichar (tempch, buf_ch);
		      if (fastmap[*tempch])
			break;
#else
		      if (fastmap[(unsigned char) RE_TRANSLATE_1 (*d)])
			break;
#endif /* MULE */
		      INC_IBYTEPTR_FMT (d, fmt);
		      range -= (d - old_d);
#if 1
		      assert (!forward_search_p || range >= 0);
#endif
		    }
		}
#ifdef MULE
	      else if (fmt != FORMAT_DEFAULT)
		{
		  while (range > lim)
		    {
		      re_char *old_d = d;
		      Ibyte tempch[MAX_ICHAR_LEN];
		      Ichar buf_ch = itext_ichar_fmt (d, fmt, lispobj);
		      set_itext_ichar (tempch, buf_ch);
		      if (fastmap[*tempch])
			break;
		      INC_IBYTEPTR_FMT (d, fmt);
		      range -= (d - old_d);
#if 1
		      assert (!forward_search_p || range >= 0);
#endif
		    }
		}
#endif /* MULE */
	      else
		{
		  while (range > lim && !fastmap[*d])
		    {
		      re_char *old_d = d;
		      INC_IBYTEPTR (d);
		      range -= (d - old_d);
#if 1
		assert (!forward_search_p || range >= 0);
#endif
		    }
		}

	      startpos += irange - range;
	    }
	  else				/* Searching backwards.  */
	    {
	      /* #### It's not clear why we don't just write a loop, like
		 for the moving-forward case.  Perhaps the writer got lazy,
		 since backward searches aren't so common. */
	      d = ((const unsigned char *)
		   (startpos >= size1 ? string2 - size1 : string1) + startpos);
#ifdef MULE
	      {
		Ibyte tempch[MAX_ICHAR_LEN];
		Ichar buf_ch =
		  RE_TRANSLATE (itext_ichar_fmt (d, fmt, lispobj));
		set_itext_ichar (tempch, buf_ch);
		if (!fastmap[*tempch])
		  goto advance;
	      }
#else
	      if (!fastmap[(unsigned char) RE_TRANSLATE (*d)])
		goto advance;
#endif /* MULE */
	    }
	}

      /* If can't match the null string, and that's all we have left, fail.  */
      if (range >= 0 && startpos == total_size && fastmap
          && !bufp->can_be_null)
	{
	  UNBIND_REGEX_MALLOC_CHECK ();
	  return -1;
	}

#ifdef emacs /* XEmacs added, w/removal of immediate_quit */
      if (!no_quit_in_re_search)
	{
	  BEGIN_REGEX_MALLOC_OK ();
	  QUIT;
	  END_REGEX_MALLOC_OK ();
	  RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	}

#endif
      BEGIN_REGEX_MALLOC_OK ();
      val = re_match_2_internal (bufp, string1, size1, string2, size2,
				 startpos, regs, stop
				 RE_LISP_CONTEXT_ARGS);
#ifndef REGEX_MALLOC
      ALLOCA_GARBAGE_COLLECT ();
#endif
      END_REGEX_MALLOC_OK ();
      RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS ();

      if (val >= 0)
	{
	  UNBIND_REGEX_MALLOC_CHECK ();
	  return startpos;
	}

      if (val == -2)
	{
	  UNBIND_REGEX_MALLOC_CHECK ();
	  return -2;
	}

      RE_SEARCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
    advance:
      if (!range)
	break;
      else if (range > 0)
	{
	  Bytecount d_size;
	  d = ((const unsigned char *)
	       (startpos >= size1 ? string2 - size1 : string1) + startpos);
	  d_size = itext_ichar_len_fmt (d, fmt);
	  range -= d_size;
#if 1
		assert (!forward_search_p || range >= 0);
#endif
	  startpos += d_size;
	}
      else
	{
	  Bytecount d_size;
	  /* Note startpos > size1 not >=.  If we are on the
	     string1/string2 boundary, we want to backup into string1. */
	  d = ((const unsigned char *)
	       (startpos > size1 ? string2 - size1 : string1) + startpos);
	  DEC_IBYTEPTR_FMT (d, fmt);
	  d_size = itext_ichar_len_fmt (d, fmt);
	  range += d_size;
#if 1
		assert (!forward_search_p || range >= 0);
#endif
	  startpos -= d_size;
	}
    }
  UNBIND_REGEX_MALLOC_CHECK ();
  return -1;
} /* re_search_2 */


/* Declarations and macros for re_match_2.  */

/* This converts PTR, a pointer into one of the search strings `string1'
   and `string2' into an offset from the beginning of that string.  */
#define POINTER_TO_OFFSET(ptr)			\
  (FIRST_STRING_P (ptr)				\
   ? ((regoff_t) ((ptr) - string1))		\
   : ((regoff_t) ((ptr) - string2 + size1)))

/* Macros for dealing with the split strings in re_match_2.  */

#define MATCHING_IN_FIRST_STRING  (dend == end_match_1)

/* Call before fetching a character with *d.  This switches over to
   string2 if necessary.  */
#define REGEX_PREFETCH()						\
  while (d == dend)						    	\
    {									\
      /* End of string2 => fail.  */					\
      if (dend == end_match_2) 						\
        goto fail;							\
      /* End of string1 => advance to string2.  */ 			\
      d = string2;						        \
      dend = end_match_2;						\
    }


/* Test if at very beginning or at very end of the virtual concatenation
   of `string1' and `string2'.  If only one string, it's `string2'.  */
#define AT_STRINGS_BEG(d) ((d) == (size1 ? string1 : string2) || !size2)
#define AT_STRINGS_END(d) ((d) == end2)

/* XEmacs change:
   If the given position straddles the string gap, return the equivalent
   position that is before or after the gap, respectively; otherwise,
   return the same position. */
#define POS_BEFORE_GAP_UNSAFE(d) ((d) == string2 ? end1 : (d))
#define POS_AFTER_GAP_UNSAFE(d) ((d) == end1 ? string2 : (d))

/* Test if CH is a word-constituent character. (XEmacs change) */
#define WORDCHAR_P(ch)						\
  (SYNTAX (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf), ch) == Sword)

/* Free everything we malloc.  */
#ifdef MATCH_MAY_ALLOCATE
#define FREE_VAR(var,type) if (var) REGEX_FREE (var, type); var = NULL
#define FREE_VARIABLES()						\
  do {									\
    UNBIND_REGEX_MALLOC_CHECK ();					\
    REGEX_FREE_STACK (fail_stack.stack);				\
    FREE_VAR (regstart, re_char **);					\
    FREE_VAR (regend, re_char **);					\
    FREE_VAR (old_regstart, re_char **);				\
    FREE_VAR (old_regend, re_char **);					\
    FREE_VAR (best_regstart, re_char **);				\
    FREE_VAR (best_regend, re_char **);					\
    FREE_VAR (reg_info, register_info_type *);				\
    FREE_VAR (reg_dummy, re_char **);					\
    FREE_VAR (reg_info_dummy, register_info_type *);			\
  } while (0)
#else /* not MATCH_MAY_ALLOCATE */
#define FREE_VARIABLES()			\
  do {						\
    UNBIND_REGEX_MALLOC_CHECK ();		\
  } while (0)
#endif /* MATCH_MAY_ALLOCATE */

/* These values must meet several constraints.  They must not be valid
   register values; since we have a limit of 255 registers (because
   we use only one byte in the pattern for the register number), we can
   use numbers larger than 255.  They must differ by 1, because of
   NUM_FAILURE_ITEMS above.  And the value for the lowest register must
   be larger than the value for the highest register, so we do not try
   to actually save any registers when none are active.  */
#define NO_HIGHEST_ACTIVE_REG (1 << BYTEWIDTH)
#define NO_LOWEST_ACTIVE_REG (NO_HIGHEST_ACTIVE_REG + 1)

/* Matching routines.  */

#ifndef emacs   /* XEmacs never uses this.  */
/* re_match is like re_match_2 except it takes only a single string.  */

int
re_match (struct re_pattern_buffer *bufp, const char *string, int size,
	  int pos, struct re_registers *regs
	  RE_LISP_CONTEXT_ARGS_DECL)
{
  int result = re_match_2_internal (bufp, NULL, 0, (re_char *) string, size,
				    pos, regs, size
				    RE_LISP_CONTEXT_ARGS);
  ALLOCA_GARBAGE_COLLECT ();
  return result;
}
#endif /* not emacs */

/* re_match_2 matches the compiled pattern in BUFP against the
   (virtual) concatenation of STRING1 and STRING2 (of length SIZE1 and
   SIZE2, respectively).  We start matching at POS, and stop matching
   at STOP.

   If REGS is non-null and the `no_sub' field of BUFP is nonzero, we
   store offsets for the substring each group matched in REGS.  See the
   documentation for exactly how many groups we fill.

   We return -1 if no match, -2 if an internal error (such as the
   failure stack overflowing).  Otherwise, we return the length of the
   matched substring.  */

int
re_match_2 (struct re_pattern_buffer *bufp, const char *string1,
	    int size1, const char *string2, int size2, int pos,
	    struct re_registers *regs, int stop
	    RE_LISP_CONTEXT_ARGS_DECL)
{
  int result;

#ifdef emacs
  /* Update the mirror syntax table if it's dirty now, this would otherwise
     cause a malloc() in charset_mule in re_match_2_internal() when checking
     characters' syntax. */
  update_mirror_syntax_if_dirty (BUFFER_MIRROR_SYNTAX_TABLE (lispbuf));
  scache = setup_syntax_cache (scache, lispobj, lispbuf,
			       offset_to_charxpos (lispobj, pos),
			       1);
#endif

  result = re_match_2_internal (bufp, (re_char *) string1, size1,
				(re_char *) string2, size2,
				pos, regs, stop
				RE_LISP_CONTEXT_ARGS);

  ALLOCA_GARBAGE_COLLECT ();
  return result;
}

/* This is a separate function so that we can force an alloca cleanup
   afterwards.  */
static int
re_match_2_internal (struct re_pattern_buffer *bufp, re_char *string1,
		     int size1, re_char *string2, int size2, int pos,
		     struct re_registers *regs, int stop
		     RE_LISP_CONTEXT_ARGS_MULE_DECL)
{
  /* General temporaries.  */
  int mcnt;
  unsigned char *p1;
  int should_succeed; /* XEmacs change */

  /* Just past the end of the corresponding string.  */
  re_char *end1, *end2;

  /* Pointers into string1 and string2, just past the last characters in
     each to consider matching.  */
  re_char *end_match_1, *end_match_2;

  /* Where we are in the data, and the end of the current string.  */
  re_char *d, *dend;

  /* Where we are in the pattern, and the end of the pattern.  */
  unsigned char *p = bufp->buffer;
  REGISTER unsigned char *pend = p + bufp->used;

  /* Mark the opcode just after a start_memory, so we can test for an
     empty subpattern when we get to the stop_memory.  */
  re_char *just_past_start_mem = 0;

  /* We use this to map every character in the string.  */
  RE_TRANSLATE_TYPE translate = bufp->translate;

  /* Failure point stack.  Each place that can handle a failure further
     down the line pushes a failure point on this stack.  It consists of
     restart, regend, and reg_info for all registers corresponding to
     the subexpressions we're currently inside, plus the number of such
     registers, and, finally, two char *'s.  The first char * is where
     to resume scanning the pattern; the second one is where to resume
     scanning the strings.  If the latter is zero, the failure point is
     a ``dummy''; if a failure happens and the failure point is a dummy,
     it gets discarded and the next one is tried.  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, this is global.  */
  fail_stack_type fail_stack;
#endif
#ifdef DEBUG
  static int failure_id;
  int nfailure_points_pushed = 0, nfailure_points_popped = 0;
#endif

#ifdef REGEX_REL_ALLOC
  /* This holds the pointer to the failure stack, when
     it is allocated relocatably.  */
  fail_stack_elt_t *failure_stack_ptr;
#endif

  /* We fill all the registers internally, independent of what we
     return, for use in backreferences.  The number here includes
     an element for register zero.  */
  int num_regs = bufp->re_ngroups + 1;

  /* The currently active registers.  */
  int lowest_active_reg = NO_LOWEST_ACTIVE_REG;
  int highest_active_reg = NO_HIGHEST_ACTIVE_REG;

  /* Information on the contents of registers. These are pointers into
     the input strings; they record just what was matched (on this
     attempt) by a subexpression part of the pattern, that is, the
     regnum-th regstart pointer points to where in the pattern we began
     matching and the regnum-th regend points to right after where we
     stopped matching the regnum-th subexpression.  (The zeroth register
     keeps track of what the whole pattern matches.)  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **regstart, **regend;
#endif

  /* If a group that's operated upon by a repetition operator fails to
     match anything, then the register for its start will need to be
     restored because it will have been set to wherever in the string we
     are when we last see its open-group operator.  Similarly for a
     register's end.  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **old_regstart, **old_regend;
#endif

  /* The is_active field of reg_info helps us keep track of which (possibly
     nested) subexpressions we are currently in. The matched_something
     field of reg_info[reg_num] helps us tell whether or not we have
     matched any of the pattern so far this time through the reg_num-th
     subexpression.  These two fields get reset each time through any
     loop their register is in.  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, this is global.  */
  register_info_type *reg_info;
#endif

  /* The following record the register info as found in the above
     variables when we find a match better than any we've seen before.
     This happens as we backtrack through the failure points, which in
     turn happens only if we have not yet matched the entire string. */
  int best_regs_set = false;
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **best_regstart, **best_regend;
#endif

  /* Logically, this is `best_regend[0]'.  But we don't want to have to
     allocate space for that if we're not allocating space for anything
     else (see below).  Also, we never need info about register 0 for
     any of the other register vectors, and it seems rather a kludge to
     treat `best_regend' differently than the rest.  So we keep track of
     the end of the best match so far in a separate variable.  We
     initialize this to NULL so that when we backtrack the first time
     and need to test it, it's not garbage.  */
  re_char *match_end = NULL;

  /* This helps SET_REGS_MATCHED avoid doing redundant work.  */
  int set_regs_matched_done = 0;

  /* Used when we pop values we don't care about.  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **reg_dummy;
  register_info_type *reg_info_dummy;
#endif

#ifdef DEBUG
  /* Counts the total number of registers pushed.  */
  int num_regs_pushed = 0;
#endif

  /* 1 if this match ends in the same string (string1 or string2)
     as the best previous match.  */
  re_bool same_str_p;

  /* 1 if this match is the best seen so far.  */
  re_bool best_match_p;

#ifdef emacs
  Internal_Format fmt = buffer_or_other_internal_format (lispobj);
#ifdef REL_ALLOC
  Ibyte *orig_buftext =
    BUFFERP (lispobj) ?
    BYTE_BUF_BYTE_ADDRESS (XBUFFER (lispobj),
			   BYTE_BUF_BEGV (XBUFFER (lispobj))) :
    0;
#endif

#ifdef ERROR_CHECK_MALLOC
  int depth = bind_regex_malloc_disallowed (1);
#endif
#endif /* emacs */

  DEBUG_MATCH_PRINT1 ("\n\nEntering re_match_2.\n");

  BEGIN_REGEX_MALLOC_OK ();
  INIT_FAIL_STACK ();
  END_REGEX_MALLOC_OK ();

#ifdef MATCH_MAY_ALLOCATE
  /* Do not bother to initialize all the register variables if there are
     no groups in the pattern, as it takes a fair amount of time.  If
     there are groups, we include space for register 0 (the whole
     pattern), even though we never use it, since it simplifies the
     array indexing.  We should fix this.  */
  if (bufp->re_ngroups)
    {
      BEGIN_REGEX_MALLOC_OK ();
      regstart       = REGEX_TALLOC (num_regs, re_char *);
      regend         = REGEX_TALLOC (num_regs, re_char *);
      old_regstart   = REGEX_TALLOC (num_regs, re_char *);
      old_regend     = REGEX_TALLOC (num_regs, re_char *);
      best_regstart  = REGEX_TALLOC (num_regs, re_char *);
      best_regend    = REGEX_TALLOC (num_regs, re_char *);
      reg_info       = REGEX_TALLOC (num_regs, register_info_type);
      reg_dummy      = REGEX_TALLOC (num_regs, re_char *);
      reg_info_dummy = REGEX_TALLOC (num_regs, register_info_type);
      END_REGEX_MALLOC_OK ();

      if (!(regstart && regend && old_regstart && old_regend && reg_info
            && best_regstart && best_regend && reg_dummy && reg_info_dummy))
        {
          FREE_VARIABLES ();
          return -2;
        }
    }
  else
    {
      /* We must initialize all our variables to NULL, so that
         `FREE_VARIABLES' doesn't try to free them.  */
      regstart = regend = old_regstart = old_regend = best_regstart
        = best_regend = reg_dummy = NULL;
      reg_info = reg_info_dummy = (register_info_type *) NULL;
    }
#endif /* MATCH_MAY_ALLOCATE */

#if defined (emacs) && defined (REL_ALLOC)
  {
    /* If the allocations above (or the call to setup_syntax_cache() in
       re_match_2) caused a rel-alloc relocation, then fix up the data
       pointers */
    Bytecount offset = offset_post_relocation (lispobj, orig_buftext);
    if (offset)
      {
	string1 += offset;
	string2 += offset;
      }
  }
#endif /* defined (emacs) && defined (REL_ALLOC) */

  /* The starting position is bogus.  */
  if (pos < 0 || pos > size1 + size2)
    {
      FREE_VARIABLES ();
      return -1;
    }

  /* Initialize subexpression text positions to -1 to mark ones that no
     start_memory/stop_memory has been seen for. Also initialize the
     register information struct.  */
  for (mcnt = 1; mcnt < num_regs; mcnt++)
    {
      regstart[mcnt] = regend[mcnt] = old_regstart[mcnt] = old_regend[mcnt]
	= best_regstart[mcnt] = best_regend[mcnt] = REG_UNSET_VALUE;
      REG_MATCH_NULL_STRING_P (reg_info[mcnt]) = MATCH_NULL_UNSET_VALUE;
      IS_ACTIVE (reg_info[mcnt]) = 0;
      MATCHED_SOMETHING (reg_info[mcnt]) = 0;
      EVER_MATCHED_SOMETHING (reg_info[mcnt]) = 0;
    }
  /* We move `string1' into `string2' if the latter's empty -- but not if
     `string1' is null.  */
  if (size2 == 0 && string1 != NULL)
    {
      string2 = string1;
      size2 = size1;
      string1 = 0;
      size1 = 0;
    }
  end1 = string1 + size1;
  end2 = string2 + size2;

  /* Compute where to stop matching, within the two strings.  */
  if (stop <= size1)
    {
      end_match_1 = string1 + stop;
      end_match_2 = string2;
    }
  else
    {
      end_match_1 = end1;
      end_match_2 = string2 + stop - size1;
    }

  /* `p' scans through the pattern as `d' scans through the data.
     `dend' is the end of the input string that `d' points within.  `d'
     is advanced into the following input string whenever necessary, but
     this happens before fetching; therefore, at the beginning of the
     loop, `d' can be pointing at the end of a string, but it cannot
     equal `string2'.  */
  if (size1 > 0 && pos <= size1)
    {
      d = string1 + pos;
      dend = end_match_1;
    }
  else
    {
      d = string2 + pos - size1;
      dend = end_match_2;
    }

  DEBUG_MATCH_PRINT1 ("The compiled pattern is: \n");
  DEBUG_MATCH_PRINT_COMPILED_PATTERN (bufp, p, pend);
  DEBUG_MATCH_PRINT1 ("The string to match is: `");
  DEBUG_MATCH_PRINT_DOUBLE_STRING (d, string1, size1, string2, size2);
  DEBUG_MATCH_PRINT1 ("'\n");

  /* This loops over pattern commands.  It exits by returning from the
     function if the match is complete, or it drops through if the match
     fails at this starting point in the input data.  */
  for (;;)
    {
      DEBUG_MATCH_PRINT2 ("\n0x%lx: ", (long) p);
#ifdef emacs /* XEmacs added, w/removal of immediate_quit */
      if (!no_quit_in_re_search)
	{
	  BEGIN_REGEX_MALLOC_OK ();
	  QUIT;
	  END_REGEX_MALLOC_OK ();
	  RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	}
#endif

      if (p == pend)
	{ /* End of pattern means we might have succeeded.  */
          DEBUG_MATCH_PRINT1 ("end of pattern ... ");

	  /* If we haven't matched the entire string, and we want the
             longest match, try backtracking.  */
          if (d != end_match_2)
	    {
	      same_str_p = (FIRST_STRING_P (match_end)
			    == MATCHING_IN_FIRST_STRING);

	      /* AIX compiler got confused when this was combined
		 with the previous declaration.  */
	      if (same_str_p)
		best_match_p = d > match_end;
	      else
		best_match_p = !MATCHING_IN_FIRST_STRING;

              DEBUG_MATCH_PRINT1 ("backtracking.\n");

              if (!FAIL_STACK_EMPTY ())
                { /* More failure points to try.  */

                  /* If exceeds best match so far, save it.  */
                  if (!best_regs_set || best_match_p)
                    {
                      best_regs_set = true;
                      match_end = d;

                      DEBUG_MATCH_PRINT1 ("\nSAVING match as best so far.\n");

                      for (mcnt = 1; mcnt < num_regs; mcnt++)
                        {
                          best_regstart[mcnt] = regstart[mcnt];
                          best_regend[mcnt] = regend[mcnt];
                        }
                    }
                  goto fail;
                }

              /* If no failure points, don't restore garbage.  And if
                 last match is real best match, don't restore second
                 best one. */
              else if (best_regs_set && !best_match_p)
                {
  	        restore_best_regs:
                  /* Restore best match.  It may happen that `dend ==
                     end_match_1' while the restored d is in string2.
                     For example, the pattern `x.*y.*z' against the
                     strings `x-' and `y-z-', if the two strings are
                     not consecutive in memory.  */
                  DEBUG_MATCH_PRINT1 ("Restoring best registers.\n");

                  d = match_end;
                  dend = ((d >= string1 && d <= end1)
		           ? end_match_1 : end_match_2);

		  for (mcnt = 1; mcnt < num_regs; mcnt++)
		    {
		      regstart[mcnt] = best_regstart[mcnt];
		      regend[mcnt] = best_regend[mcnt];
		    }
                }
            } /* d != end_match_2 */

	succeed_label:
          DEBUG_MATCH_PRINT1 ("Accepting match.\n");

          /* If caller wants register contents data back, do it.  */
	  {
	    int num_nonshy_regs = bufp->re_nsub + 1;
	    if (regs && !bufp->no_sub)
	      {
		/* Have the register data arrays been allocated?  */
		if (bufp->regs_allocated == REGS_UNALLOCATED)
		  { /* No.  So allocate them with malloc.  We need one
		       extra element beyond `num_regs' for the `-1' marker
		       GNU code uses.  */
		    regs->num_regs = MAX (RE_NREGS, num_nonshy_regs + 1);
		    BEGIN_REGEX_MALLOC_OK ();
		    regs->start = TALLOC (regs->num_regs, regoff_t);
		    regs->end = TALLOC (regs->num_regs, regoff_t);
		    END_REGEX_MALLOC_OK ();
		    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
		    if (regs->start == NULL || regs->end == NULL)
		      {
			FREE_VARIABLES ();
			return -2;
		      }
		    bufp->regs_allocated = REGS_REALLOCATE;
		  }
		else if (bufp->regs_allocated == REGS_REALLOCATE)
		  { /* Yes.  If we need more elements than were already
		       allocated, reallocate them.  If we need fewer, just
		       leave it alone.  */
		    if (regs->num_regs < num_nonshy_regs + 1)
		      {
			regs->num_regs = num_nonshy_regs + 1;
			BEGIN_REGEX_MALLOC_OK ();
			RETALLOC (regs->start, regs->num_regs, regoff_t);
			RETALLOC (regs->end, regs->num_regs, regoff_t);
			END_REGEX_MALLOC_OK ();
			RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
			if (regs->start == NULL || regs->end == NULL)
			  {
			    FREE_VARIABLES ();
			    return -2;
			  }
		      }
		  }
		else
		  {
		    /* The braces fend off a "empty body in an else-statement"
		       warning under GCC when assert expands to nothing.  */
		    assert (bufp->regs_allocated == REGS_FIXED);
		  }

		/* Convert the pointer data in `regstart' and `regend' to
		   indices.  Register zero has to be set differently,
		   since we haven't kept track of any info for it.  */
		if (regs->num_regs > 0)
		  {
		    regs->start[0] = pos;
		    regs->end[0] = (MATCHING_IN_FIRST_STRING
				    ? ((regoff_t) (d - string1))
				    : ((regoff_t) (d - string2 + size1)));
		  }

		/* Map over the NUM_NONSHY_REGS non-shy internal registers.
		   Copy each into the corresponding external register.
		   MCNT indexes external registers. */
		for (mcnt = 1; mcnt < MIN (num_nonshy_regs, regs->num_regs);
		     mcnt++)
		  {
		    int internal_reg = bufp->external_to_internal_register[mcnt];
		    if (REG_UNSET (regstart[internal_reg]) ||
			REG_UNSET (regend[internal_reg]))
		      regs->start[mcnt] = regs->end[mcnt] = -1;
		    else
		      {
			regs->start[mcnt] =
			  (regoff_t) POINTER_TO_OFFSET (regstart[internal_reg]);
			regs->end[mcnt] =
			  (regoff_t) POINTER_TO_OFFSET (regend[internal_reg]);
		      }
		  }
	      } /* regs && !bufp->no_sub */

	    /* If we have regs and the regs structure has more elements than
	       were in the pattern, set the extra elements starting with
	       NUM_NONSHY_REGS to -1.  If we (re)allocated the registers,
	       this is the case, because we always allocate enough to have
	       at least one -1 at the end.

	       We do this even when no_sub is set because some applications
	       (XEmacs) reuse register structures which may contain stale
	       information, and permit attempts to access those registers.

	       It would be possible to require the caller to do this, but we'd
	       have to change the API for this function to reflect that, and
	       audit all callers.  Note: as of 2003-04-17 callers in XEmacs
	       do clear the registers, but it's safer to leave this code in
	       because of reallocation.
	    */
	    if (regs && regs->num_regs > 0)
	      for (mcnt = num_nonshy_regs; mcnt < regs->num_regs; mcnt++)
		regs->start[mcnt] = regs->end[mcnt] = -1;
	  }
          DEBUG_MATCH_PRINT4 ("%u failure points pushed, %u popped (%u remain).\n",
                        nfailure_points_pushed, nfailure_points_popped,
                        nfailure_points_pushed - nfailure_points_popped);
          DEBUG_MATCH_PRINT2 ("%u registers pushed.\n", num_regs_pushed);

          mcnt = d - pos - (MATCHING_IN_FIRST_STRING
			    ? string1
			    : string2 - size1);

          DEBUG_MATCH_PRINT2 ("Returning %d from re_match_2.\n", mcnt);

          FREE_VARIABLES ();
          return mcnt;
        }

      /* Otherwise match next pattern command.  */
      switch ((re_opcode_t) *p++)
	{
        /* Ignore these.  Used to ignore the n of succeed_n's which
           currently have n == 0.  */
        case no_op:
          DEBUG_MATCH_PRINT1 ("EXECUTING no_op.\n");
          break;

	case succeed:
          DEBUG_MATCH_PRINT1 ("EXECUTING succeed.\n");
	  goto succeed_label;

        /* Match exactly a string of length n in the pattern.  The
           following byte in the pattern defines n, and the n bytes after
           that make up the string to match. (Under Mule, this will be in
           the default internal format.) */
	case exactn:
	  mcnt = *p++;
          DEBUG_MATCH_PRINT2 ("EXECUTING exactn %d.\n", mcnt);

          /* This is written out as an if-else so we don't waste time
             testing `translate' inside the loop.  */
          if (TRANSLATE_P (translate))
	    {
	      do
		{
#ifdef MULE
		  Bytecount pat_len;

		  REGEX_PREFETCH ();
		  if (RE_TRANSLATE_1 (itext_ichar_fmt (d, fmt, lispobj))
		      != itext_ichar (p))
                    goto fail;

		  pat_len = itext_ichar_len (p);
		  p += pat_len;
		  INC_IBYTEPTR_FMT (d, fmt);
		  
		  mcnt -= pat_len;
#else /* not MULE */
		  REGEX_PREFETCH ();
		  if ((unsigned char) RE_TRANSLATE_1 (*d++) != *p++)
                    goto fail;
		  mcnt--;
#endif
		}
	      while (mcnt > 0);
	    }
	  else
	    {
#ifdef MULE
	      /* If buffer format is default, then we can shortcut and just
		 compare the text directly, byte by byte.  Otherwise, we
		 need to go character by character. */
	      if (fmt != FORMAT_DEFAULT)
		{
		  do
		    {
		      Bytecount pat_len;

		      REGEX_PREFETCH ();
		      if (itext_ichar_fmt (d, fmt, lispobj) !=
			  itext_ichar (p))
			goto fail;

		      pat_len = itext_ichar_len (p);
		      p += pat_len;
		      INC_IBYTEPTR_FMT (d, fmt);
		  
		      mcnt -= pat_len;
		    }
		  while (mcnt > 0);
		}
	      else
#endif
		{
		  do
		    {
		      REGEX_PREFETCH ();
		      if (*d++ != *p++) goto fail;
		      mcnt--;
		    }
		  while (mcnt > 0);
		}
	    }
	  SET_REGS_MATCHED ();
          break;


        /* Match any character except possibly a newline or a null.  */
	case anychar:
          DEBUG_MATCH_PRINT1 ("EXECUTING anychar.\n");

          REGEX_PREFETCH ();

          if ((!(bufp->syntax & RE_DOT_NEWLINE) &&
	       RE_TRANSLATE (itext_ichar_fmt (d, fmt, lispobj)) == '\n')
              || (bufp->syntax & RE_DOT_NOT_NULL &&
		  RE_TRANSLATE (itext_ichar_fmt (d, fmt, lispobj)) ==
		  '\000'))
	    goto fail;

          SET_REGS_MATCHED ();
          DEBUG_MATCH_PRINT2 ("  Matched `%d'.\n", *d);
	  INC_IBYTEPTR_FMT (d, fmt); /* XEmacs change */
	  break;


	case charset:
	case charset_not:
	  {
	    REGISTER Ichar c;
	    re_bool not_p = (re_opcode_t) *(p - 1) == charset_not;

            DEBUG_MATCH_PRINT2 ("EXECUTING charset%s.\n", not_p ? "_not" : "");

	    REGEX_PREFETCH ();
	    c = itext_ichar_fmt (d, fmt, lispobj);
	    c = RE_TRANSLATE (c); /* The character to match.  */

            /* Cast to `unsigned int' instead of `unsigned char' in case the
               bit list is a full 32 bytes long.  */
	    if ((unsigned int)c < (unsigned int) (*p * BYTEWIDTH)
		&& p[1 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
	      not_p = !not_p;

	    p += 1 + *p;

	    if (!not_p) goto fail;

	    SET_REGS_MATCHED ();
            INC_IBYTEPTR_FMT (d, fmt); /* XEmacs change */
	    break;
	  }

#ifdef MULE
	case charset_mule:
	case charset_mule_not:
	  {
	    REGISTER Ichar c;
	    re_bool not_p = (re_opcode_t) *(p - 1) == charset_mule_not;
	    Bitbyte class_bits = *p++;

            DEBUG_MATCH_PRINT2 ("EXECUTING charset_mule%s.\n",
                                not_p ? "_not" : "");
	    REGEX_PREFETCH ();
	    c = itext_ichar_fmt (d, fmt, lispobj);
	    c = RE_TRANSLATE (c); /* The character to match.  */

	    if ((class_bits &&
		 ((class_bits & BIT_WORD && ISWORD (c)) /* = ALNUM */
                  || (class_bits & BIT_ALPHA && ISALPHA (c))
		  || (class_bits & BIT_SPACE && ISSPACE (c))
		  || (class_bits & BIT_PUNCT && ISPUNCT (c))
                  || (TRANSLATE_P (translate) ?
                      (class_bits & (BIT_UPPER | BIT_LOWER)
                       && !NOCASEP (lispbuf, c))
                      : ((class_bits & BIT_UPPER && ISUPPER (c))
                         || (class_bits & BIT_LOWER && ISLOWER (c))))))
                || EQ (Qt, unified_range_table_lookup (p, c, Qnil)))
	      {
		not_p = !not_p;
	      }

	    p += unified_range_table_bytes_used (p);

	    if (!not_p) goto fail;

	    SET_REGS_MATCHED ();
	    INC_IBYTEPTR_FMT (d, fmt);
	    break;
	  }
#endif /* MULE */


        /* The beginning of a group is represented by start_memory.
           The arguments are the register number in the next byte, and the
           number of groups inner to this one in the next.  The text
           matched within the group is recorded (in the internal
           registers data structure) under the register number.  */
        case start_memory:
	  DEBUG_MATCH_PRINT3 ("EXECUTING start_memory %d (%d):\n", *p, p[1]);

          /* Find out if this group can match the empty string.  */
	  p1 = p;		/* To send to group_match_null_string_p.  */

          if (REG_MATCH_NULL_STRING_P (reg_info[*p]) == MATCH_NULL_UNSET_VALUE)
	    REG_MATCH_NULL_STRING_P (reg_info[*p])
	      = group_match_null_string_p (&p1, pend, reg_info);

	  DEBUG_MATCH_PRINT2 ("  group CAN%s match null string\n",
			REG_MATCH_NULL_STRING_P (reg_info[*p]) ? "NOT" : "");

          /* Save the position in the string where we were the last time
             we were at this open-group operator in case the group is
             operated upon by a repetition operator, e.g., with `(a*)*b'
             against `ab'; then we want to ignore where we are now in
             the string in case this attempt to match fails.  */
          old_regstart[*p] = REG_MATCH_NULL_STRING_P (reg_info[*p])
                             ? REG_UNSET (regstart[*p]) ? d : regstart[*p]
                             : regstart[*p];
	  DEBUG_MATCH_PRINT2 ("  old_regstart: %d\n",
			 POINTER_TO_OFFSET (old_regstart[*p]));

          regstart[*p] = d;
	  DEBUG_MATCH_PRINT2 ("  regstart: %d\n", POINTER_TO_OFFSET (regstart[*p]));

          IS_ACTIVE (reg_info[*p]) = 1;
          MATCHED_SOMETHING (reg_info[*p]) = 0;

	  /* Clear this whenever we change the register activity status.  */
	  set_regs_matched_done = 0;

          /* This is the new highest active register.  */
          highest_active_reg = *p;

          /* If nothing was active before, this is the new lowest active
             register.  */
          if (lowest_active_reg == NO_LOWEST_ACTIVE_REG)
            lowest_active_reg = *p;

          /* Move past the register number and inner group count.  */
          p += 2;
	  just_past_start_mem = p;

          break;


        /* The stop_memory opcode represents the end of a group.  Its
           arguments are the same as start_memory's: the register
           number, and the number of inner groups.  */
	case stop_memory:
	  DEBUG_MATCH_PRINT3 ("EXECUTING stop_memory %d (%d):\n", *p, p[1]);

          /* We need to save the string position the last time we were at
             this close-group operator in case the group is operated
             upon by a repetition operator, e.g., with `((a*)*(b*)*)*'
             against `aba'; then we want to ignore where we are now in
             the string in case this attempt to match fails.  */
          old_regend[*p] = REG_MATCH_NULL_STRING_P (reg_info[*p])
                           ? REG_UNSET (regend[*p]) ? d : regend[*p]
			   : regend[*p];
	  DEBUG_MATCH_PRINT2 ("      old_regend: %d\n",
			 POINTER_TO_OFFSET (old_regend[*p]));

          regend[*p] = d;
	  DEBUG_MATCH_PRINT2 ("      regend: %d\n", POINTER_TO_OFFSET (regend[*p]));

          /* This register isn't active anymore.  */
          IS_ACTIVE (reg_info[*p]) = 0;

	  /* Clear this whenever we change the register activity status.  */
	  set_regs_matched_done = 0;

          /* If this was the only register active, nothing is active
             anymore.  */
          if (lowest_active_reg == highest_active_reg)
            {
              lowest_active_reg = NO_LOWEST_ACTIVE_REG;
              highest_active_reg = NO_HIGHEST_ACTIVE_REG;
            }
          else
            { /* We must scan for the new highest active register, since
                 it isn't necessarily one less than now: consider
                 (a(b)c(d(e)f)g).  When group 3 ends, after the f), the
                 new highest active register is 1.  */
              unsigned char r = *p - 1;
              while (r > 0 && !IS_ACTIVE (reg_info[r]))
                r--;

              /* If we end up at register zero, that means that we saved
                 the registers as the result of an `on_failure_jump', not
                 a `start_memory', and we jumped to past the innermost
                 `stop_memory'.  For example, in ((.)*) we save
                 registers 1 and 2 as a result of the *, but when we pop
                 back to the second ), we are at the stop_memory 1.
                 Thus, nothing is active.  */
	      if (r == 0)
                {
                  lowest_active_reg = NO_LOWEST_ACTIVE_REG;
                  highest_active_reg = NO_HIGHEST_ACTIVE_REG;
                }
              else
		{
		  highest_active_reg = r;

		  /* 98/9/21 jhod:  We've also gotta set lowest_active_reg, don't we? */
		  r = 1;
		  while (r < highest_active_reg && !IS_ACTIVE(reg_info[r]))
		    r++;
		  lowest_active_reg = r;
		}
	    }

          /* If just failed to match something this time around with a
             group that's operated on by a repetition operator, try to
             force exit from the ``loop'', and restore the register
             information for this group that we had before trying this
             last match.  */
          if ((!MATCHED_SOMETHING (reg_info[*p])
               || just_past_start_mem == p - 1)
	      && (p + 2) < pend)
            {
              re_bool is_a_jump_n = false;

              p1 = p + 2;
              mcnt = 0;
              switch ((re_opcode_t) *p1++)
                {
                  case jump_n:
		    is_a_jump_n = true;
                  case pop_failure_jump:
		  case maybe_pop_jump:
		  case jump:
		  case dummy_failure_jump:
                    EXTRACT_NUMBER_AND_INCR (mcnt, p1);
		    if (is_a_jump_n)
		      p1 += 2;
                    break;

                  default:
                    /* do nothing */ ;
                }
	      p1 += mcnt;

              /* If the next operation is a jump backwards in the pattern
	         to an on_failure_jump right before the start_memory
                 corresponding to this stop_memory, exit from the loop
                 by forcing a failure after pushing on the stack the
                 on_failure_jump's jump in the pattern, and d.  */
              if (mcnt < 0 && (re_opcode_t) *p1 == on_failure_jump
                  && (re_opcode_t) p1[3] == start_memory && p1[4] == *p)
		{
                  /* If this group ever matched anything, then restore
                     what its registers were before trying this last
                     failed match, e.g., with `(a*)*b' against `ab' for
                     regstart[1], and, e.g., with `((a*)*(b*)*)*'
                     against `aba' for regend[3].

                     Also restore the registers for inner groups for,
                     e.g., `((a*)(b*))*' against `aba' (register 3 would
                     otherwise get trashed).  */

                  if (EVER_MATCHED_SOMETHING (reg_info[*p]))
		    {
		      int r;

                      EVER_MATCHED_SOMETHING (reg_info[*p]) = 0;

		      /* Restore this and inner groups' (if any) registers.  */
                      for (r = *p; r < *p + *(p + 1); r++)
                        {
                          regstart[r] = old_regstart[r];

                          /* xx why this test?  */
                          if (old_regend[r] >= regstart[r])
                            regend[r] = old_regend[r];
                        }
                    }
		  p1++;
                  EXTRACT_NUMBER_AND_INCR (mcnt, p1);
                  PUSH_FAILURE_POINT (p1 + mcnt, d, -2);

                  goto fail;
                }
            }

          /* Move past the register number and the inner group count.  */
          p += 2;
          break;


	/* \<digit> has been turned into a `duplicate' command which is
           followed by the numeric value of <digit> as the register number.
	   (Already passed through external-to-internal-register mapping,
	   so it refers to the actual group number, not the non-shy-only
	   numbering used in the external world.) */
        case duplicate:
	  {
	    REGISTER re_char *d2, *dend2;
	    /* Get which register to match against.  */
	    int regno = *p++;
	    DEBUG_MATCH_PRINT2 ("EXECUTING duplicate %d.\n", regno);

	    /* Can't back reference a group which we've never matched.  */
            if (REG_UNSET (regstart[regno]) || REG_UNSET (regend[regno]))
              goto fail;

            /* Where in input to try to start matching.  */
            d2 = regstart[regno];

            /* Where to stop matching; if both the place to start and
               the place to stop matching are in the same string, then
               set to the place to stop, otherwise, for now have to use
               the end of the first string.  */

            dend2 = ((FIRST_STRING_P (regstart[regno])
		      == FIRST_STRING_P (regend[regno]))
		     ? regend[regno] : end_match_1);
	    for (;;)
	      {
		/* If necessary, advance to next segment in register
                   contents.  */
		while (d2 == dend2)
		  {
		    if (dend2 == end_match_2) break;
		    if (dend2 == regend[regno]) break;

                    /* End of string1 => advance to string2. */
                    d2 = string2;
                    dend2 = regend[regno];
		  }
		/* At end of register contents => success */
		if (d2 == dend2) break;

		/* If necessary, advance to next segment in data.  */
		REGEX_PREFETCH ();

		/* How many characters left in this segment to match.  */
		mcnt = dend - d;

		/* Want how many consecutive characters we can match in
                   one shot, so, if necessary, adjust the count.  */
                if (mcnt > dend2 - d2)
		  mcnt = dend2 - d2;

		/* Compare that many; failure if mismatch, else move
                   past them.  */
		if (TRANSLATE_P (translate)
                    ? bcmp_translate (d, d2, mcnt, translate
#ifdef emacs
				      , fmt, lispobj
#endif
				      )
                    : memcmp (d, d2, mcnt))
		  goto fail;
		d += mcnt, d2 += mcnt;

		/* Do this because we've match some characters.  */
		SET_REGS_MATCHED ();
	      }
	  }
	  break;


        /* begline matches the empty string at the beginning of the string
           (unless `not_bol' is set in `bufp'), and, if
           `newline_anchor' is set, after newlines.  */
	case begline:
          DEBUG_MATCH_PRINT1 ("EXECUTING begline.\n");

          if (AT_STRINGS_BEG (d))
            {
              if (!bufp->not_bol) break;
            }
          else
	    {
	      re_char *d2 = d;
	      DEC_IBYTEPTR (d2);
	      if (itext_ichar_ascii_fmt (d2, fmt, lispobj) == '\n' &&
		  bufp->newline_anchor)
		break;
	    }
          /* In all other cases, we fail.  */
          goto fail;


        /* endline is the dual of begline.  */
	case endline:
          DEBUG_MATCH_PRINT1 ("EXECUTING endline.\n");

          if (AT_STRINGS_END (d))
            {
              if (!bufp->not_eol) break;
            }

          /* We have to ``prefetch'' the next character.  */
          else if ((d == end1 ?
		    itext_ichar_ascii_fmt (string2, fmt, lispobj) :
		    itext_ichar_ascii_fmt (d, fmt, lispobj)) == '\n'
                   && bufp->newline_anchor)
            {
              break;
            }
          goto fail;


	/* Match at the very beginning of the data.  */
        case begbuf:
          DEBUG_MATCH_PRINT1 ("EXECUTING begbuf.\n");
          if (AT_STRINGS_BEG (d))
            break;
          goto fail;


	/* Match at the very end of the data.  */
        case endbuf:
          DEBUG_MATCH_PRINT1 ("EXECUTING endbuf.\n");
	  if (AT_STRINGS_END (d))
	    break;
          goto fail;


        /* on_failure_keep_string_jump is used to optimize `.*\n'.  It
           pushes NULL as the value for the string on the stack.  Then
           `pop_failure_point' will keep the current value for the
           string, instead of restoring it.  To see why, consider
           matching `foo\nbar' against `.*\n'.  The .* matches the foo;
           then the . fails against the \n.  But the next thing we want
           to do is match the \n against the \n; if we restored the
           string value, we would be back at the foo.

           Because this is used only in specific cases, we don't need to
           check all the things that `on_failure_jump' does, to make
           sure the right things get saved on the stack.  Hence we don't
           share its code.  The only reason to push anything on the
           stack at all is that otherwise we would have to change
           `anychar's code to do something besides goto fail in this
           case; that seems worse than this.  */
        case on_failure_keep_string_jump:
          DEBUG_MATCH_PRINT1 ("EXECUTING on_failure_keep_string_jump");

          EXTRACT_NUMBER_AND_INCR (mcnt, p);
          DEBUG_MATCH_PRINT3 (" %d (to 0x%lx):\n", mcnt, (long) (p + mcnt));

          PUSH_FAILURE_POINT (p + mcnt, (unsigned char *) 0, -2);
          break;


	/* Uses of on_failure_jump:

           Each alternative starts with an on_failure_jump that points
           to the beginning of the next alternative.  Each alternative
           except the last ends with a jump that in effect jumps past
           the rest of the alternatives.  (They really jump to the
           ending jump of the following alternative, because tensioning
           these jumps is a hassle.)

           Repeats start with an on_failure_jump that points past both
           the repetition text and either the following jump or
           pop_failure_jump back to this on_failure_jump.  */
	case on_failure_jump:
        on_failure:
          DEBUG_MATCH_PRINT1 ("EXECUTING on_failure_jump");

          EXTRACT_NUMBER_AND_INCR (mcnt, p);
          DEBUG_MATCH_PRINT3 (" %d (to 0x%lx)", mcnt, (long) (p + mcnt));

          /* If this on_failure_jump comes right before a group (i.e.,
             the original * applied to a group), save the information
             for that group and all inner ones, so that if we fail back
             to this point, the group's information will be correct.
             For example, in \(a*\)*\1, we need the preceding group,
             and in \(\(a*\)b*\)\2, we need the inner group.  */

          /* We can't use `p' to check ahead because we push
             a failure point to `p + mcnt' after we do this.  */
          p1 = p;

          /* We need to skip no_op's before we look for the
             start_memory in case this on_failure_jump is happening as
             the result of a completed succeed_n, as in \(a\)\{1,3\}b\1
             against aba.  */
          while (p1 < pend && (re_opcode_t) *p1 == no_op)
            p1++;

          if (p1 < pend && (re_opcode_t) *p1 == start_memory)
            {
              /* We have a new highest active register now.  This will
                 get reset at the start_memory we are about to get to,
                 but we will have saved all the registers relevant to
                 this repetition op, as described above.  */
              highest_active_reg = *(p1 + 1) + *(p1 + 2);
              if (lowest_active_reg == NO_LOWEST_ACTIVE_REG)
                lowest_active_reg = *(p1 + 1);
            }

          DEBUG_MATCH_PRINT1 (":\n");
          PUSH_FAILURE_POINT (p + mcnt, d, -2);
          break;


        /* A smart repeat ends with `maybe_pop_jump'.
	   We change it to either `pop_failure_jump' or `jump'.  */
        case maybe_pop_jump:
          EXTRACT_NUMBER_AND_INCR (mcnt, p);
          DEBUG_MATCH_PRINT2 ("EXECUTING maybe_pop_jump %d.\n", mcnt);
          {
	    REGISTER unsigned char *p2 = p;

            /* Compare the beginning of the repeat with what in the
               pattern follows its end. If we can establish that there
               is nothing that they would both match, i.e., that we
               would have to backtrack because of (as in, e.g., `a*a')
               then we can change to pop_failure_jump, because we'll
               never have to backtrack.

               This is not true in the case of alternatives: in
               `(a|ab)*' we do need to backtrack to the `ab' alternative
               (e.g., if the string was `ab').  But instead of trying to
               detect that here, the alternative has put on a dummy
               failure point which is what we will end up popping.  */

	    /* Skip over open/close-group commands.
	       If what follows this loop is a ...+ construct,
	       look at what begins its body, since we will have to
	       match at least one of that.  */
	    while (1)
	      {
		if (p2 + 2 < pend
		    && ((re_opcode_t) *p2 == stop_memory
			|| (re_opcode_t) *p2 == start_memory))
		  p2 += 3;
		else if (p2 + 6 < pend
			 && (re_opcode_t) *p2 == dummy_failure_jump)
		  p2 += 6;
		else
		  break;
	      }

	    p1 = p + mcnt;
	    /* p1[0] ... p1[2] are the `on_failure_jump' corresponding
	       to the `maybe_finalize_jump' of this case.  Examine what
	       follows.  */

            /* If we're at the end of the pattern, we can change.  */
            if (p2 == pend)
	      {
		/* Consider what happens when matching ":\(.*\)"
		   against ":/".  I don't really understand this code
		   yet.  */
  	        p[-3] = (unsigned char) pop_failure_jump;
                DEBUG_MATCH_PRINT1
                  ("  End of pattern: change to `pop_failure_jump'.\n");
              }

            else if ((re_opcode_t) *p2 == exactn
		     || (bufp->newline_anchor && (re_opcode_t) *p2 == endline))
	      {
		REGISTER unsigned char c
                  = *p2 == (unsigned char) endline ? '\n' : p2[2];

                if ((re_opcode_t) p1[3] == exactn && p1[5] != c)
                  {
  		    p[-3] = (unsigned char) pop_failure_jump;
                    DEBUG_MATCH_PRINT3 ("  %c != %c => pop_failure_jump.\n",
                                  c, p1[5]);
                  }

		else if ((re_opcode_t) p1[3] == charset
			 || (re_opcode_t) p1[3] == charset_not)
		  {
		    int not_p = (re_opcode_t) p1[3] == charset_not;

		    if (c < (unsigned char) (p1[4] * BYTEWIDTH)
			&& p1[5 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
		      not_p = !not_p;

                    /* `not_p' is equal to 1 if c would match, which means
                        that we can't change to pop_failure_jump.  */
		    if (!not_p)
                      {
  		        p[-3] = (unsigned char) pop_failure_jump;
                        DEBUG_MATCH_PRINT1 ("  No match => pop_failure_jump.\n");
                      }
		  }
	      }
            else if ((re_opcode_t) *p2 == charset)
	      {
#ifdef DEBUG
		REGISTER unsigned char c
                  = *p2 == (unsigned char) endline ? '\n' : p2[2];
#endif

                if ((re_opcode_t) p1[3] == exactn
                    && ! ((int) p2[1] * BYTEWIDTH > (int) p1[5]
                          && (p2[2 + p1[5] / BYTEWIDTH]
                              & (1 << (p1[5] % BYTEWIDTH)))))
                  {
  		    p[-3] = (unsigned char) pop_failure_jump;
                    DEBUG_MATCH_PRINT3 ("  %c != %c => pop_failure_jump.\n",
                                  c, p1[5]);
                  }

		else if ((re_opcode_t) p1[3] == charset_not)
		  {
		    int idx;
		    /* We win if the charset_not inside the loop
		       lists every character listed in the charset after.  */
		    for (idx = 0; idx < (int) p2[1]; idx++)
		      if (! (p2[2 + idx] == 0
			     || (idx < (int) p1[4]
				 && ((p2[2 + idx] & ~ p1[5 + idx]) == 0))))
			break;

		    if (idx == p2[1])
                      {
  		        p[-3] = (unsigned char) pop_failure_jump;
                        DEBUG_MATCH_PRINT1 ("  No match => pop_failure_jump.\n");
                      }
		  }
		else if ((re_opcode_t) p1[3] == charset)
		  {
		    int idx;
		    /* We win if the charset inside the loop
		       has no overlap with the one after the loop.  */
		    for (idx = 0;
			 idx < (int) p2[1] && idx < (int) p1[4];
			 idx++)
		      if ((p2[2 + idx] & p1[5 + idx]) != 0)
			break;

		    if (idx == p2[1] || idx == p1[4])
                      {
  		        p[-3] = (unsigned char) pop_failure_jump;
                        DEBUG_MATCH_PRINT1 ("  No match => pop_failure_jump.\n");
                      }
		  }
	      }
	  }
	  p -= 2;		/* Point at relative address again.  */
	  if ((re_opcode_t) p[-1] != pop_failure_jump)
	    {
	      p[-1] = (unsigned char) jump;
              DEBUG_MATCH_PRINT1 ("  Match => jump.\n");
	      goto unconditional_jump;
	    }
        /* Note fall through.  */


	/* The end of a simple repeat has a pop_failure_jump back to
           its matching on_failure_jump, where the latter will push a
           failure point.  The pop_failure_jump takes off failure
           points put on by this pop_failure_jump's matching
           on_failure_jump; we got through the pattern to here from the
           matching on_failure_jump, so didn't fail.  */
        case pop_failure_jump:
          {
            /* We need to pass separate storage for the lowest and
               highest registers, even though we don't care about the
               actual values.  Otherwise, we will restore only one
               register from the stack, since lowest will == highest in
               `pop_failure_point'.  */
            int dummy_low_reg, dummy_high_reg;
            unsigned char *pdummy;
            re_char *sdummy = NULL;

            USED (sdummy); /* Silence warning. */

            DEBUG_MATCH_PRINT1 ("EXECUTING pop_failure_jump.\n");
            POP_FAILURE_POINT (sdummy, pdummy,
                               dummy_low_reg, dummy_high_reg,
                               reg_dummy, reg_dummy, reg_info_dummy);
          }
          /* Note fall through.  */


        /* Unconditionally jump (without popping any failure points).  */
        case jump:
	unconditional_jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);	/* Get the amount to jump.  */
          DEBUG_MATCH_PRINT2 ("EXECUTING jump %d ", mcnt);
	  p += mcnt;				/* Do the jump.  */
          DEBUG_MATCH_PRINT2 ("(to 0x%lx).\n", (long) p);
	  break;


        /* We need this opcode so we can detect where alternatives end
           in `group_match_null_string_p' et al.  */
        case jump_past_alt:
          DEBUG_MATCH_PRINT1 ("EXECUTING jump_past_alt.\n");
          goto unconditional_jump;


        /* Normally, the on_failure_jump pushes a failure point, which
           then gets popped at pop_failure_jump.  We will end up at
           pop_failure_jump, also, and with a pattern of, say, `a+', we
           are skipping over the on_failure_jump, so we have to push
           something meaningless for pop_failure_jump to pop.  */
        case dummy_failure_jump:
          DEBUG_MATCH_PRINT1 ("EXECUTING dummy_failure_jump.\n");
          /* It doesn't matter what we push for the string here.  What
             the code at `fail' tests is the value for the pattern.  */
          PUSH_FAILURE_POINT ((unsigned char *) 0, (unsigned char *) 0, -2);
          goto unconditional_jump;


        /* At the end of an alternative, we need to push a dummy failure
           point in case we are followed by a `pop_failure_jump', because
           we don't want the failure point for the alternative to be
           popped.  For example, matching `(a|ab)*' against `aab'
           requires that we match the `ab' alternative.  */
        case push_dummy_failure:
          DEBUG_MATCH_PRINT1 ("EXECUTING push_dummy_failure.\n");
          /* See comments just above at `dummy_failure_jump' about the
             two zeroes.  */
          PUSH_FAILURE_POINT ((re_char *) 0, (re_char *) 0, -2);
          break;

        /* Have to succeed matching what follows at least n times.
           After that, handle like `on_failure_jump'.  */
        case succeed_n:
          EXTRACT_NUMBER (mcnt, p + 2);
          DEBUG_MATCH_PRINT2 ("EXECUTING succeed_n %d.\n", mcnt);

          assert (mcnt >= 0);
          /* Originally, this is how many times we HAVE to succeed.  */
          if (mcnt > 0)
            {
               mcnt--;
	       p += 2;
               STORE_NUMBER_AND_INCR (p, mcnt);
               DEBUG_MATCH_PRINT3 ("  Setting 0x%lx to %d.\n", (long) p, mcnt);
            }
	  else if (mcnt == 0)
            {
              DEBUG_MATCH_PRINT2 ("  Setting two bytes from 0x%lx to no_op.\n",
			    (long) (p+2));
	      p[2] = (unsigned char) no_op;
              p[3] = (unsigned char) no_op;
              goto on_failure;
            }
          break;

        case jump_n:
          EXTRACT_NUMBER (mcnt, p + 2);
          DEBUG_MATCH_PRINT2 ("EXECUTING jump_n %d.\n", mcnt);

          /* Originally, this is how many times we CAN jump.  */
          if (mcnt)
            {
               mcnt--;
               STORE_NUMBER (p + 2, mcnt);
	       goto unconditional_jump;
            }
          /* If don't have to jump any more, skip over the rest of command.  */
	  else
	    p += 4;
          break;

	case set_number_at:
	  {
            DEBUG_MATCH_PRINT1 ("EXECUTING set_number_at.\n");

            EXTRACT_NUMBER_AND_INCR (mcnt, p);
            p1 = p + mcnt;
            EXTRACT_NUMBER_AND_INCR (mcnt, p);
            DEBUG_MATCH_PRINT3 ("  Setting 0x%lx to %d.\n", (long) p1, mcnt);
	    STORE_NUMBER (p1, mcnt);
            break;
          }

        case wordbound:
          DEBUG_MATCH_PRINT1 ("EXECUTING wordbound.\n");
	  should_succeed = 1;
	matchwordbound:
	  {
	    /* XEmacs change */
	    /* Straightforward and (I hope) correct implementation.
	       Probably should be optimized by arranging to compute
	       charpos only once. */
	    /* emch1 is the character before d, syn1 is the syntax of
	       emch1, emch2 is the character at d, and syn2 is the
	       syntax of emch2. */
	    Ichar emch1, emch2;
	    int syn1 = 0,
	        syn2 = 0;
	    re_char *d_before, *d_after;
	    int result,
		at_beg = AT_STRINGS_BEG (d),
		at_end = AT_STRINGS_END (d);
#ifdef emacs
	    Charxpos charpos;
#endif

	    if (at_beg && at_end)
	      {
		result = 0;
	      }
	    else
	      {
		if (!at_beg)
		  {
		    d_before = POS_BEFORE_GAP_UNSAFE (d);
		    DEC_IBYTEPTR_FMT (d_before, fmt);
		    emch1 = itext_ichar_fmt (d_before, fmt, lispobj);
#ifdef emacs
		    charpos = offset_to_charxpos (lispobj,
						  PTR_TO_OFFSET (d)) - 1;
		    BEGIN_REGEX_MALLOC_OK ();
		    UPDATE_SYNTAX_CACHE (scache, charpos);
#endif
		    syn1 = SYNTAX_FROM_CACHE (scache, emch1);
		    END_REGEX_MALLOC_OK ();
		  }
		if (!at_end)
		  {
		    d_after = POS_AFTER_GAP_UNSAFE (d);
		    emch2 = itext_ichar_fmt (d_after, fmt, lispobj);
#ifdef emacs
		    charpos = offset_to_charxpos (lispobj, PTR_TO_OFFSET (d));
		    BEGIN_REGEX_MALLOC_OK ();
		    UPDATE_SYNTAX_CACHE_FORWARD (scache, charpos);
#endif
		    syn2 = SYNTAX_FROM_CACHE (scache, emch2);
		    END_REGEX_MALLOC_OK ();
		  }
		RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();

		if (at_beg)
		  result = (syn2 == Sword);
		else if (at_end)
		  result = (syn1 == Sword);
		else
		  result = ((syn1 == Sword) != (syn2 == Sword));
	      }

	    if (result == should_succeed)
	      break;
	    goto fail;
	  }

	case notwordbound:
          DEBUG_MATCH_PRINT1 ("EXECUTING notwordbound.\n");
	  should_succeed = 0;
	  goto matchwordbound;

	case wordbeg:
          DEBUG_MATCH_PRINT1 ("EXECUTING wordbeg.\n");
	  if (AT_STRINGS_END (d))
	    goto fail;
	  {
	    /* XEmacs: this originally read:

	    if (WORDCHAR_P (d) && (AT_STRINGS_BEG (d) || !WORDCHAR_P (d - 1)))
	      break;

	      */
	    re_char *dtmp = POS_AFTER_GAP_UNSAFE (d);
	    Ichar emch = itext_ichar_fmt (dtmp, fmt, lispobj);
	    int tempres;
#ifdef emacs
	    Charxpos charpos = offset_to_charxpos (lispobj, PTR_TO_OFFSET (d));
#endif
	    BEGIN_REGEX_MALLOC_OK ();
#ifdef emacs
	    UPDATE_SYNTAX_CACHE (scache, charpos);
#endif
	    tempres = (SYNTAX_FROM_CACHE (scache, emch) != Sword);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	    if (tempres)
	      goto fail;
	    if (AT_STRINGS_BEG (d))
	      break;
	    dtmp = POS_BEFORE_GAP_UNSAFE (d);
	    DEC_IBYTEPTR_FMT (dtmp, fmt);
	    emch = itext_ichar_fmt (dtmp, fmt, lispobj);
	    BEGIN_REGEX_MALLOC_OK ();
#ifdef emacs
	    UPDATE_SYNTAX_CACHE_BACKWARD (scache, charpos - 1);
#endif
	    tempres = (SYNTAX_FROM_CACHE (scache, emch) != Sword);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	    if (tempres)
	      break;
	    goto fail;
	  }

	case wordend:
          DEBUG_MATCH_PRINT1 ("EXECUTING wordend.\n");
	  if (AT_STRINGS_BEG (d))
	    goto fail;
	  {
	    /* XEmacs: this originally read:

	    if (!AT_STRINGS_BEG (d) && WORDCHAR_P (d - 1)
		&& (!WORDCHAR_P (d) || AT_STRINGS_END (d)))
	      break;

	      The or condition is incorrect (reversed).
	      */
	    re_char *dtmp;
	    Ichar emch;
	    int tempres;
#ifdef emacs
	    Charxpos charpos = offset_to_charxpos (lispobj, PTR_TO_OFFSET (d));
	    BEGIN_REGEX_MALLOC_OK ();
	    UPDATE_SYNTAX_CACHE (scache, charpos);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
#endif
	    dtmp = POS_BEFORE_GAP_UNSAFE (d);
	    DEC_IBYTEPTR_FMT (dtmp, fmt);
	    emch = itext_ichar_fmt (dtmp, fmt, lispobj);
	    BEGIN_REGEX_MALLOC_OK ();
	    tempres = (SYNTAX_FROM_CACHE (scache, emch) != Sword);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	    if (tempres)
	      goto fail;
	    if (AT_STRINGS_END (d))
	      break;
	    dtmp = POS_AFTER_GAP_UNSAFE (d);
	    emch = itext_ichar_fmt (dtmp, fmt, lispobj);
	    BEGIN_REGEX_MALLOC_OK ();
#ifdef emacs
	    UPDATE_SYNTAX_CACHE_FORWARD (scache, charpos + 1);
#endif
	    tempres = (SYNTAX_FROM_CACHE (scache, emch) != Sword);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	    if (tempres)
	      break;
	    goto fail;
	  }

#ifdef emacs
  	case before_dot:
          DEBUG_MATCH_PRINT1 ("EXECUTING before_dot.\n");
 	  if (!BUFFERP (lispobj)
	      || (BUF_PTR_BYTE_POS (XBUFFER (lispobj), (unsigned char *) d)
		  >= BUF_PT (XBUFFER (lispobj))))
  	    goto fail;
  	  break;

  	case at_dot:
          DEBUG_MATCH_PRINT1 ("EXECUTING at_dot.\n");
 	  if (!BUFFERP (lispobj)
	      || (BUF_PTR_BYTE_POS (XBUFFER (lispobj), (unsigned char *) d)
		  != BUF_PT (XBUFFER (lispobj))))
  	    goto fail;
  	  break;

  	case after_dot:
          DEBUG_MATCH_PRINT1 ("EXECUTING after_dot.\n");
 	  if (!BUFFERP (lispobj)
	      || (BUF_PTR_BYTE_POS (XBUFFER (lispobj), (unsigned char *) d)
		  <= BUF_PT (XBUFFER (lispobj))))
  	    goto fail;
  	  break;

	case syntaxspec:
          DEBUG_MATCH_PRINT2 ("EXECUTING syntaxspec %d.\n", mcnt);
	  mcnt = *p++;
	  goto matchsyntax;

        case wordchar:
          DEBUG_MATCH_PRINT1 ("EXECUTING Emacs wordchar.\n");
	  mcnt = (int) Sword;
        matchsyntax:
	  should_succeed = 1;
	matchornotsyntax:
	  {
	    int matches;
	    Ichar emch;

	    REGEX_PREFETCH ();
	    BEGIN_REGEX_MALLOC_OK ();
	    UPDATE_SYNTAX_CACHE
	      (scache, offset_to_charxpos (lispobj, PTR_TO_OFFSET (d)));
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();

	    emch = itext_ichar_fmt (d, fmt, lispobj);
	    BEGIN_REGEX_MALLOC_OK ();
	    matches = (SYNTAX_FROM_CACHE (scache, emch) ==
		       (enum syntaxcode) mcnt);
	    END_REGEX_MALLOC_OK ();
	    RE_MATCH_RELOCATE_MOVEABLE_DATA_POINTERS ();
	    INC_IBYTEPTR_FMT (d, fmt);
	    if (matches != should_succeed)
	      goto fail;
	    SET_REGS_MATCHED ();
	  }
	  break;

	case notsyntaxspec:
          DEBUG_MATCH_PRINT2 ("EXECUTING notsyntaxspec %d.\n", mcnt);
	  mcnt = *p++;
	  goto matchnotsyntax;

        case notwordchar:
          DEBUG_MATCH_PRINT1 ("EXECUTING Emacs notwordchar.\n");
	  mcnt = (int) Sword;
        matchnotsyntax:
	  should_succeed = 0;
	  goto matchornotsyntax;

#ifdef MULE
/* 97/2/17 jhod Mule category code patch */
	case categoryspec:
	  should_succeed = 1;
        matchornotcategory:
	  {
	    Ichar emch;

	    mcnt = *p++;
	    REGEX_PREFETCH ();
	    emch = itext_ichar_fmt (d, fmt, lispobj);
	    INC_IBYTEPTR_FMT (d, fmt);
	    if (check_category_char (emch, BUFFER_CATEGORY_TABLE (lispbuf),
				     mcnt, should_succeed))
	      goto fail;
	    SET_REGS_MATCHED ();
	  }
	  break;

	case notcategoryspec:
	  should_succeed = 0;
	  goto matchornotcategory;
/* end of category patch */
#endif /* MULE */
#else /* not emacs */
	case wordchar:
          DEBUG_MATCH_PRINT1 ("EXECUTING non-Emacs wordchar.\n");
	  REGEX_PREFETCH ();
          if (!WORDCHAR_P ((int) (*d)))
            goto fail;
	  SET_REGS_MATCHED ();
          d++;
	  break;

	case notwordchar:
          DEBUG_MATCH_PRINT1 ("EXECUTING non-Emacs notwordchar.\n");
	  REGEX_PREFETCH ();
          if (!WORDCHAR_P ((int) (*d)))
            goto fail;
          SET_REGS_MATCHED ();
          d++;
	  break;
#endif /* emacs */

        default:
          ABORT ();
	}
      continue;  /* Successfully executed one pattern command; keep going.  */


    /* We goto here if a matching operation fails. */
    fail:
      if (!FAIL_STACK_EMPTY ())
	{ /* A restart point is known.  Restore to that state.  */
          DEBUG_MATCH_PRINT1 ("\nFAIL:\n");
          POP_FAILURE_POINT (d, p,
                             lowest_active_reg, highest_active_reg,
                             regstart, regend, reg_info);

          /* If this failure point is a dummy, try the next one.  */
          if (!p)
	    goto fail;

          /* If we failed to the end of the pattern, don't examine *p.  */
	  assert (p <= pend);
          if (p < pend)
            {
              re_bool is_a_jump_n = false;

              /* If failed to a backwards jump that's part of a repetition
                 loop, need to pop this failure point and use the next one.  */
              switch ((re_opcode_t) *p)
                {
                case jump_n:
                  is_a_jump_n = true;
                case maybe_pop_jump:
                case pop_failure_jump:
                case jump:
                  p1 = p + 1;
                  EXTRACT_NUMBER_AND_INCR (mcnt, p1);
                  p1 += mcnt;

                  if ((is_a_jump_n && (re_opcode_t) *p1 == succeed_n)
                      || (!is_a_jump_n
                          && (re_opcode_t) *p1 == on_failure_jump))
                    goto fail;
                  break;
                default:
                  /* do nothing */ ;
                }
            }

          if (d >= string1 && d <= end1)
	    dend = end_match_1;
        }
      else
        break;   /* Matching at this starting point really fails.  */
    } /* for (;;) */

  if (best_regs_set)
    goto restore_best_regs;

  FREE_VARIABLES ();

  return -1;         			/* Failure to match.  */
} /* re_match_2_internal */

/* Subroutine definitions for re_match_2.  */


/* We are passed P pointing to a register number after a start_memory.

   Return true if the pattern up to the corresponding stop_memory can
   match the empty string, and false otherwise.

   If we find the matching stop_memory, sets P to point to one past its number.
   Otherwise, sets P to an undefined byte less than or equal to END.

   We don't handle duplicates properly (yet).  */

static re_bool
group_match_null_string_p (unsigned char **p, unsigned char *end,
			   register_info_type *reg_info)
{
  int mcnt;
  /* Point to after the args to the start_memory.  */
  unsigned char *p1 = *p + 2;

  while (p1 < end)
    {
      /* Skip over opcodes that can match nothing, and return true or
	 false, as appropriate, when we get to one that can't, or to the
         matching stop_memory.  */

      switch ((re_opcode_t) *p1)
        {
        /* Could be either a loop or a series of alternatives.  */
        case on_failure_jump:
          p1++;
          EXTRACT_NUMBER_AND_INCR (mcnt, p1);

          /* If the next operation is not a jump backwards in the
	     pattern.  */

	  if (mcnt >= 0)
	    {
              /* Go through the on_failure_jumps of the alternatives,
                 seeing if any of the alternatives cannot match nothing.
                 The last alternative starts with only a jump,
                 whereas the rest start with on_failure_jump and end
                 with a jump, e.g., here is the pattern for `a|b|c':

                 /on_failure_jump/0/6/exactn/1/a/jump_past_alt/0/6
                 /on_failure_jump/0/6/exactn/1/b/jump_past_alt/0/3
                 /exactn/1/c

                 So, we have to first go through the first (n-1)
                 alternatives and then deal with the last one separately.  */


              /* Deal with the first (n-1) alternatives, which start
                 with an on_failure_jump (see above) that jumps to right
                 past a jump_past_alt.  */

              while ((re_opcode_t) p1[mcnt-3] == jump_past_alt)
                {
                  /* `mcnt' holds how many bytes long the alternative
                     is, including the ending `jump_past_alt' and
                     its number.  */

                  if (!alt_match_null_string_p (p1, p1 + mcnt - 3,
				                      reg_info))
                    return false;

                  /* Move to right after this alternative, including the
		     jump_past_alt.  */
                  p1 += mcnt;

                  /* Break if it's the beginning of an n-th alternative
                     that doesn't begin with an on_failure_jump.  */
                  if ((re_opcode_t) *p1 != on_failure_jump)
                    break;

		  /* Still have to check that it's not an n-th
		     alternative that starts with an on_failure_jump.  */
		  p1++;
                  EXTRACT_NUMBER_AND_INCR (mcnt, p1);
                  if ((re_opcode_t) p1[mcnt-3] != jump_past_alt)
                    {
		      /* Get to the beginning of the n-th alternative.  */
                      p1 -= 3;
                      break;
                    }
                }

              /* Deal with the last alternative: go back and get number
                 of the `jump_past_alt' just before it.  `mcnt' contains
                 the length of the alternative.  */
              EXTRACT_NUMBER (mcnt, p1 - 2);

              if (!alt_match_null_string_p (p1, p1 + mcnt, reg_info))
                return false;

              p1 += mcnt;	/* Get past the n-th alternative.  */
            } /* if mcnt > 0 */
          break;


        case stop_memory:
	  assert (p1[1] == **p);
          *p = p1 + 2;
          return true;


        default:
          if (!common_op_match_null_string_p (&p1, end, reg_info))
            return false;
        }
    } /* while p1 < end */

  return false;
} /* group_match_null_string_p */


/* Similar to group_match_null_string_p, but doesn't deal with alternatives:
   It expects P to be the first byte of a single alternative and END one
   byte past the last. The alternative can contain groups.  */

static re_bool
alt_match_null_string_p (unsigned char *p, unsigned char *end,
			 register_info_type *reg_info)
{
  int mcnt;
  unsigned char *p1 = p;

  while (p1 < end)
    {
      /* Skip over opcodes that can match nothing, and break when we get
         to one that can't.  */

      switch ((re_opcode_t) *p1)
        {
	/* It's a loop.  */
        case on_failure_jump:
          p1++;
          EXTRACT_NUMBER_AND_INCR (mcnt, p1);
          p1 += mcnt;
          break;

	default:
          if (!common_op_match_null_string_p (&p1, end, reg_info))
            return false;
        }
    }  /* while p1 < end */

  return true;
} /* alt_match_null_string_p */


/* Deals with the ops common to group_match_null_string_p and
   alt_match_null_string_p.

   Sets P to one after the op and its arguments, if any.  */

static re_bool
common_op_match_null_string_p (unsigned char **p, unsigned char *end,
			       register_info_type *reg_info)
{
  int mcnt;
  re_bool ret;
  int reg_no;
  unsigned char *p1 = *p;

  switch ((re_opcode_t) *p1++)
    {
    case no_op:
    case begline:
    case endline:
    case begbuf:
    case endbuf:
    case wordbeg:
    case wordend:
    case wordbound:
    case notwordbound:
#ifdef emacs
    case before_dot:
    case at_dot:
    case after_dot:
#endif
      break;

    case start_memory:
      reg_no = *p1;
      assert (reg_no > 0 && reg_no <= MAX_REGNUM);
      ret = group_match_null_string_p (&p1, end, reg_info);

      /* Have to set this here in case we're checking a group which
         contains a group and a back reference to it.  */

      if (REG_MATCH_NULL_STRING_P (reg_info[reg_no]) == MATCH_NULL_UNSET_VALUE)
        REG_MATCH_NULL_STRING_P (reg_info[reg_no]) = ret;

      if (!ret)
        return false;
      break;

    /* If this is an optimized succeed_n for zero times, make the jump.  */
    case jump:
      EXTRACT_NUMBER_AND_INCR (mcnt, p1);
      if (mcnt >= 0)
        p1 += mcnt;
      else
        return false;
      break;

    case succeed_n:
      /* Get to the number of times to succeed.  */
      p1 += 2;
      EXTRACT_NUMBER_AND_INCR (mcnt, p1);

      if (mcnt == 0)
        {
          p1 -= 4;
          EXTRACT_NUMBER_AND_INCR (mcnt, p1);
          p1 += mcnt;
        }
      else
        return false;
      break;

    case duplicate:
      if (!REG_MATCH_NULL_STRING_P (reg_info[*p1]))
        return false;
      break;

    case set_number_at:
      p1 += 4;

    default:
      /* All other opcodes mean we cannot match the empty string.  */
      return false;
  }

  *p = p1;
  return true;
} /* common_op_match_null_string_p */


/* Return zero if TRANSLATE[S1] and TRANSLATE[S2] are identical for LEN
   bytes; nonzero otherwise.  */

static int
bcmp_translate (re_char *s1, re_char *s2,
		REGISTER int len, RE_TRANSLATE_TYPE translate
#ifdef emacs
		, Internal_Format USED_IF_MULE (fmt),
		Lisp_Object USED_IF_MULE (lispobj)
#endif
		)
{
  REGISTER re_char *p1 = s1, *p2 = s2;
#ifdef MULE
  re_char *p1_end = s1 + len;
  re_char *p2_end = s2 + len;

  while (p1 != p1_end && p2 != p2_end)
    {
      Ichar p1_ch, p2_ch;

      p1_ch = itext_ichar_fmt (p1, fmt, lispobj);
      p2_ch = itext_ichar_fmt (p2, fmt, lispobj);

      if (RE_TRANSLATE_1 (p1_ch)
	  != RE_TRANSLATE_1 (p2_ch))
	return 1;
      INC_IBYTEPTR_FMT (p1, fmt);
      INC_IBYTEPTR_FMT (p2, fmt);
    }
#else /* not MULE */
  while (len)
    {
      if (RE_TRANSLATE_1 (*p1++) != RE_TRANSLATE_1 (*p2++)) return 1;
      len--;
    }
#endif /* MULE */
  return 0;
}

/* Entry points for GNU code.  */

/* re_compile_pattern is the GNU regular expression compiler: it
   compiles PATTERN (of length SIZE) and puts the result in BUFP.
   Returns 0 if the pattern was valid, otherwise an error string.

   Assumes the `allocated' (and perhaps `buffer') and `translate' fields
   are set in BUFP on entry.

   We call regex_compile to do the actual compilation.  */

const char *
re_compile_pattern (const char *pattern, int length,
		    struct re_pattern_buffer *bufp)
{
  reg_errcode_t ret;

  /* GNU code is written to assume at least RE_NREGS registers will be set
     (and at least one extra will be -1).  */
  bufp->regs_allocated = REGS_UNALLOCATED;

  /* And GNU code determines whether or not to get register information
     by passing null for the REGS argument to re_match, etc., not by
     setting no_sub.  */
  bufp->no_sub = 0;

  /* Match anchors at newline.  */
  bufp->newline_anchor = 1;

  ret = regex_compile ((unsigned char *) pattern, length, re_syntax_options,
		       bufp);

  if (!ret)
    return NULL;
  return gettext (re_error_msgid[(int) ret]);
}

/* Entry points compatible with 4.2 BSD regex library.  We don't define
   them unless specifically requested.  */

#ifdef _REGEX_RE_COMP

/* BSD has one and only one pattern buffer.  */
static struct re_pattern_buffer re_comp_buf;

char *
re_comp (const char *s)
{
  reg_errcode_t ret;

  if (!s)
    {
      if (!re_comp_buf.buffer)
	return gettext ("No previous regular expression");
      return 0;
    }

  if (!re_comp_buf.buffer)
    {
      re_comp_buf.buffer = (unsigned char *) xmalloc (200);
      if (re_comp_buf.buffer == NULL)
        return gettext (re_error_msgid[(int) REG_ESPACE]);
      re_comp_buf.allocated = 200;

      re_comp_buf.fastmap = (char *) xmalloc (1 << BYTEWIDTH);
      if (re_comp_buf.fastmap == NULL)
	return gettext (re_error_msgid[(int) REG_ESPACE]);
    }

  /* Since `re_exec' always passes NULL for the `regs' argument, we
     don't need to initialize the pattern buffer fields which affect it.  */

  /* Match anchors at newlines.  */
  re_comp_buf.newline_anchor = 1;

  ret = regex_compile ((unsigned char *)s, strlen (s), re_syntax_options,
		       &re_comp_buf);

  if (!ret)
    return NULL;

  /* Yes, we're discarding `const' here if !HAVE_LIBINTL.  */
  return (char *) gettext (re_error_msgid[(int) ret]);
}


int
re_exec (const char *s)
{
  const int len = strlen (s);
  return
    0 <= re_search (&re_comp_buf, s, len, 0, len, (struct re_registers *) 0);
}
#endif /* _REGEX_RE_COMP */

/* POSIX.2 functions.  Don't define these for Emacs.  */

#ifndef emacs

/* regcomp takes a regular expression as a string and compiles it.

   PREG is a regex_t *.  We do not expect any fields to be initialized,
   since POSIX says we shouldn't.  Thus, we set

     `buffer' to the compiled pattern;
     `used' to the length of the compiled pattern;
     `syntax' to RE_SYNTAX_POSIX_EXTENDED if the
       REG_EXTENDED bit in CFLAGS is set; otherwise, to
       RE_SYNTAX_POSIX_BASIC;
     `newline_anchor' to REG_NEWLINE being set in CFLAGS;
     `fastmap' and `fastmap_accurate' to zero;
     `re_nsub' to the number of subexpressions in PATTERN.
     (non-shy of course.  POSIX probably doesn't know about
     shy ones, and in any case they should be invisible.)

   PATTERN is the address of the pattern string.

   CFLAGS is a series of bits which affect compilation.

     If REG_EXTENDED is set, we use POSIX extended syntax; otherwise, we
     use POSIX basic syntax.

     If REG_NEWLINE is set, then . and [^...] don't match newline.
     Also, regexec will try a match beginning after every newline.

     If REG_ICASE is set, then we considers upper- and lowercase
     versions of letters to be equivalent when matching.

     If REG_NOSUB is set, then when PREG is passed to regexec, that
     routine will report only success or failure, and nothing about the
     registers.

   It returns 0 if it succeeds, nonzero if it doesn't.  (See regex.h for
   the return codes and their meanings.)  */

int
regcomp (regex_t *preg, const char *pattern, int cflags)
{
  reg_errcode_t ret;
  unsigned int syntax
    = (cflags & REG_EXTENDED) ?
      RE_SYNTAX_POSIX_EXTENDED : RE_SYNTAX_POSIX_BASIC;

  /* regex_compile will allocate the space for the compiled pattern.  */
  preg->buffer = 0;
  preg->allocated = 0;
  preg->used = 0;

  /* Don't bother to use a fastmap when searching.  This simplifies the
     REG_NEWLINE case: if we used a fastmap, we'd have to put all the
     characters after newlines into the fastmap.  This way, we just try
     every character.  */
  preg->fastmap = 0;

  if (cflags & REG_ICASE)
    {
      int i;

      preg->translate = (char *) xmalloc (CHAR_SET_SIZE);
      if (preg->translate == NULL)
        return (int) REG_ESPACE;

      /* Map uppercase characters to corresponding lowercase ones.  */
      for (i = 0; i < CHAR_SET_SIZE; i++)
        preg->translate[i] = ISUPPER (i) ? tolower (i) : i;
    }
  else
    preg->translate = NULL;

  /* If REG_NEWLINE is set, newlines are treated differently.  */
  if (cflags & REG_NEWLINE)
    { /* REG_NEWLINE implies neither . nor [^...] match newline.  */
      syntax &= ~RE_DOT_NEWLINE;
      syntax |= RE_HAT_LISTS_NOT_NEWLINE;
      /* It also changes the matching behavior.  */
      preg->newline_anchor = 1;
    }
  else
    preg->newline_anchor = 0;

  preg->no_sub = !!(cflags & REG_NOSUB);

  /* POSIX says a null character in the pattern terminates it, so we
     can use strlen here in compiling the pattern.  */
  ret = regex_compile ((unsigned char *) pattern, strlen (pattern), syntax, preg);

  /* POSIX doesn't distinguish between an unmatched open-group and an
     unmatched close-group: both are REG_EPAREN.  */
  if (ret == REG_ERPAREN) ret = REG_EPAREN;

  return (int) ret;
}


/* regexec searches for a given pattern, specified by PREG, in the
   string STRING.

   If NMATCH is zero or REG_NOSUB was set in the cflags argument to
   `regcomp', we ignore PMATCH.  Otherwise, we assume PMATCH has at
   least NMATCH elements, and we set them to the offsets of the
   corresponding matched substrings.

   EFLAGS specifies `execution flags' which affect matching: if
   REG_NOTBOL is set, then ^ does not match at the beginning of the
   string; if REG_NOTEOL is set, then $ does not match at the end.

   We return 0 if we find a match and REG_NOMATCH if not.  */

int
regexec (const regex_t *preg, const char *string, size_t nmatch,
	 regmatch_t pmatch[], int eflags)
{
  int ret;
  struct re_registers regs;
  regex_t private_preg;
  int len = strlen (string);
  re_bool want_reg_info = !preg->no_sub && nmatch > 0;

  private_preg = *preg;

  private_preg.not_bol = !!(eflags & REG_NOTBOL);
  private_preg.not_eol = !!(eflags & REG_NOTEOL);

  /* The user has told us exactly how many registers to return
     information about, via `nmatch'.  We have to pass that on to the
     matching routines.  */
  private_preg.regs_allocated = REGS_FIXED;

  if (want_reg_info)
    {
      regs.num_regs = (int) nmatch;
      regs.start = TALLOC ((int) nmatch, regoff_t);
      regs.end = TALLOC ((int) nmatch, regoff_t);
      if (regs.start == NULL || regs.end == NULL)
        return (int) REG_NOMATCH;
    }

  /* Perform the searching operation.  */
  ret = re_search (&private_preg, string, len,
                   /* start: */ 0, /* range: */ len,
                   want_reg_info ? &regs : (struct re_registers *) 0);

  /* Copy the register information to the POSIX structure.  */
  if (want_reg_info)
    {
      if (ret >= 0)
        {
          int r;

          for (r = 0; r < (int) nmatch; r++)
            {
              pmatch[r].rm_so = regs.start[r];
              pmatch[r].rm_eo = regs.end[r];
            }
        }

      /* If we needed the temporary register info, free the space now.  */
      xfree (regs.start);
      xfree (regs.end);
    }

  /* We want zero return to mean success, unlike `re_search'.  */
  return ret >= 0 ? (int) REG_NOERROR : (int) REG_NOMATCH;
}


/* Returns a message corresponding to an error code, ERRCODE, returned
   from either regcomp or regexec.   We don't use PREG here.  */

size_t
regerror (int errcode, const regex_t *UNUSED (preg), char *errbuf,
	  size_t errbuf_size)
{
  const char *msg;
  Bytecount msg_size;

  if (errcode < 0
      || errcode >= (int) (sizeof (re_error_msgid) /
			   sizeof (re_error_msgid[0])))
    /* Only error codes returned by the rest of the code should be passed
       to this routine.  If we are given anything else, or if other regex
       code generates an invalid error code, then the program has a bug.
       Dump core so we can fix it.  */
    ABORT ();

  msg = gettext (re_error_msgid[errcode]);

  msg_size = strlen (msg) + 1; /* Includes the null.  */

  if (errbuf_size != 0)
    {
      if (msg_size > (Bytecount) errbuf_size)
        {
          strncpy (errbuf, msg, errbuf_size - 1);
          errbuf[errbuf_size - 1] = 0;
        }
      else
        strcpy (errbuf, msg);
    }

  return (size_t) msg_size;
}


/* Free dynamically allocated space used by PREG.  */

void
regfree (regex_t *preg)
{
  if (preg->buffer != NULL)
    xfree (preg->buffer);
  preg->buffer = NULL;

  preg->allocated = 0;
  preg->used = 0;

  if (preg->fastmap != NULL)
    xfree (preg->fastmap);
  preg->fastmap = NULL;
  preg->fastmap_accurate = 0;

  if (preg->translate != NULL)
    xfree (preg->translate);
  preg->translate = NULL;
}

#endif /* not emacs  */

