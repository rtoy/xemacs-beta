/* Declarations having to do with XEmacs syntax tables.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.28. */

#ifndef _XEMACS_SYNTAX_H_
#define _XEMACS_SYNTAX_H_

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
extern Lisp_Object Vstandard_syntax_table;

/* A syntax table is a Lisp vector of length 0400, whose elements are integers.

The low 7 bits of the integer is a code, as follows. The 8th bit is
used as the prefix bit flag (see below).
*/

enum syntaxcode
  {
    Swhitespace, /* for a whitespace character */
    Spunct,	 /* for random punctuation characters */
    Sword,	 /* for a word constituent */
    Ssymbol,	 /* symbol constituent but not word constituent */
    Sopen,	 /* for a beginning delimiter */
    Sclose,      /* for an ending delimiter */
    Squote,	 /* for a prefix character like Lisp ' */
    Sstring,	 /* for a string-grouping character like Lisp " */
    Smath,	 /* for delimiters like $ in Tex. */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Sinherit,    /* use the standard syntax table for this character */
    Sextword,	 /* extended word; works mostly like a word constituent.
		    See the comment in syntax.c. */
    Smax	 /* Upper bound on codes that are meaningful */
  };

extern Lisp_Object Qsyntax_table_p;
Lisp_Object Fsyntax_table_p (Lisp_Object);
Lisp_Object Fsyntax_table (Lisp_Object);
Lisp_Object Fset_syntax_table (Lisp_Object, Lisp_Object);

/* Return the raw syntax code for a particular character and table */
#define RAW_SYNTAX_CODE_UNSAFE(table, c) \
  (XINT (vector_data (XVECTOR (table))[(unsigned char) (c)]))

/* Return the syntax code for a particular character and table, taking
   into account inheritance. */

/* Unfortunately, we cannot write SYNTAX_CODE() as a safe macro in
   general.  I tried just using an inline function but that causes
   significant slowdown (esp. in regex routines) because this macro
   is called so many millions of times.  So instead we resort to
   SYNTAX_CODE_UNSAFE(), which is used most of the time.  Under
   GCC we can actually write this as a safe macro, and we do because
   it's likely to lead to speedups. */

#ifdef __GNUC__
#define SYNTAX_CODE_UNSAFE(table, c)					 \
 ({ Emchar _ch_ = (c);							 \
    int _rawcode_ = RAW_SYNTAX_CODE_UNSAFE (table, _ch_);		 \
    if ((enum syntaxcode) (_rawcode_ & 0177) == Sinherit)		 \
      _rawcode_ = RAW_SYNTAX_CODE_UNSAFE (Vstandard_syntax_table, _ch_); \
    _rawcode_; })
#else
#define SYNTAX_CODE_UNSAFE(table, c)			\
  (RAW_SYNTAX_CODE_UNSAFE (table, c) == Sinherit	\
   ? RAW_SYNTAX_CODE_UNSAFE (Vstandard_syntax_table, c)	\
   : RAW_SYNTAX_CODE_UNSAFE (table, c))
#endif

INLINE int SYNTAX_CODE (Lisp_Object table, Emchar c);
INLINE int
SYNTAX_CODE (Lisp_Object table, Emchar c)
{
  return SYNTAX_CODE_UNSAFE (table, c);
}
      
#define SYNTAX_UNSAFE(table, c) \
  ((enum syntaxcode) (SYNTAX_CODE_UNSAFE (table, c) & 0177))

#define SYNTAX_FROM_CODE(code) ((enum syntaxcode) ((code) & 0177))
#define SYNTAX(table, c) SYNTAX_FROM_CODE (SYNTAX_CODE (table, c))

INLINE int WORD_SYNTAX_P (Lisp_Object table, Emchar c);
INLINE int
WORD_SYNTAX_P (Lisp_Object table, Emchar c)
{
  int syncode = SYNTAX (table, c);
  return syncode == Sword || syncode == Sextword;
}

/* The prefix flag bit for backward-prefix-chars is now put into bit 7. */

#define SYNTAX_PREFIX_UNSAFE(table, c) \
  ((SYNTAX_CODE_UNSAFE (table, c) >> 7) & 1)
#define SYNTAX_PREFIX(table, c) \
  ((SYNTAX_CODE (table, c) >> 7) & 1)

/* The next 8 bits of the number is a character,
 the matching delimiter in the case of Sopen or Sclose. */

#define SYNTAX_MATCH(table, c) \
  ((SYNTAX_CODE (table, c) >> 8) & 0377)

/* The next 8 bits are used to implement up to two comment styles
   in a single buffer. They have the following meanings:

  1. first of a one or two character comment-start sequence of style a.
  2. first of a one or two character comment-start sequence of style b.
  3. second of a two-character comment-start sequence of style a.
  4. second of a two-character comment-start sequence of style b.
  5. first of a one or two character comment-end sequence of style a.
  6. first of a one or two character comment-end sequence of style b.
  7. second of a two-character comment-end sequence of style a.
  8. second of a two-character comment-end sequence of style b.
 */

#define SYNTAX_COMMENT_BITS(table, c) \
  ((SYNTAX_CODE (table, c) >> 16) &0xff)

#define SYNTAX_FIRST_OF_START_A  0x80
#define SYNTAX_FIRST_OF_START_B  0x40
#define SYNTAX_SECOND_OF_START_A 0x20
#define SYNTAX_SECOND_OF_START_B 0x10
#define SYNTAX_FIRST_OF_END_A    0x08
#define SYNTAX_FIRST_OF_END_B    0x04
#define SYNTAX_SECOND_OF_END_A   0x02
#define SYNTAX_SECOND_OF_END_B   0x01

#define SYNTAX_COMMENT_STYLE_A   0xaa
#define SYNTAX_COMMENT_STYLE_B   0x55
#define SYNTAX_FIRST_CHAR_START  0xc0
#define SYNTAX_FIRST_CHAR_END    0x0c
#define SYNTAX_FIRST_CHAR        0xcc
#define SYNTAX_SECOND_CHAR_START 0x30
#define SYNTAX_SECOND_CHAR_END   0x03
#define SYNTAX_SECOND_CHAR       0x33

#define SYNTAX_START_P(table, a, b)					\
  ((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_START)		\
   && (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_START))

#define SYNTAX_END_P(table, a, b)					\
  ((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_END)		\
   && (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_END))

#define SYNTAX_STYLES_MATCH_START_P(table, a, b, mask)			    \
  ((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_START & (mask))	    \
   && (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_START & (mask)))

#define SYNTAX_STYLES_MATCH_END_P(table, a, b, mask)			  \
  ((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_END & (mask))	  \
   && (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_END & (mask)))

#define SYNTAX_STYLES_MATCH_1CHAR_P(table, a, mask)	\
  ((SYNTAX_COMMENT_BITS (table, a) & (mask)))

#define STYLE_FOUND_P(table, a, b, startp, style)	\
  ((SYNTAX_COMMENT_BITS (table, a) &			\
    ((startp) ? SYNTAX_FIRST_CHAR_START :		\
     SYNTAX_FIRST_CHAR_END) & (style))			\
   && (SYNTAX_COMMENT_BITS (table, b) &			\
    ((startp) ? SYNTAX_SECOND_CHAR_START : 		\
     SYNTAX_SECOND_CHAR_END) & (style)))

#define SYNTAX_COMMENT_MASK_START(table, a, b)			\
  ((STYLE_FOUND_P (table, a, b, 1, SYNTAX_COMMENT_STYLE_A)	\
    ? SYNTAX_COMMENT_STYLE_A					\
    : (STYLE_FOUND_P (table, a, b, 1, SYNTAX_COMMENT_STYLE_B)	\
         ? SYNTAX_COMMENT_STYLE_B				\
	 : 0)))

#define SYNTAX_COMMENT_MASK_END(table, a, b)			\
  ((STYLE_FOUND_P (table, a, b, 0, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (STYLE_FOUND_P (table, a, b, 0, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
      : 0)))

#define STYLE_FOUND_1CHAR_P(table, a, style)	\
  ((SYNTAX_COMMENT_BITS (table, a) & (style)))

#define SYNTAX_COMMENT_1CHAR_MASK(table, a)			\
  ((STYLE_FOUND_1CHAR_P (table, a, SYNTAX_COMMENT_STYLE_A)	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (STYLE_FOUND_1CHAR_P (table, a, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
	 : 0)))

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern CONST unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern CONST unsigned char syntax_code_spec[];

Lisp_Object scan_lists (struct buffer *buf, int from, int count,
			int depth, int sexpflag, int no_error);
int char_quoted (struct buffer *buf, int pos);

/* NOTE: This does not refer to the mirror table, but to the
   syntax table itself. */
Lisp_Object syntax_match (Lisp_Object table, Emchar ch);

extern int no_quit_in_re_search;
extern struct buffer *regex_emacs_buffer;

#endif /* _XEMACS_SYNTAX_H_ */
