/* Declarations having to do with XEmacs syntax tables.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 2002, 2003, 2005 Ben Wing.

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

#ifndef INCLUDED_syntax_h_
#define INCLUDED_syntax_h_

#include "buffer.h"
#include "chartab.h"

/* A syntax table is a type of char table.

The low 7 bits of the integer is a code, as follows. The 8th bit is
used as the prefix bit flag (see below).

The values in a syntax table are either integers or conses of
integers and chars.  The lowest 7 bits of the integer are the syntax
class.  If this is Sinherit, then the actual syntax value needs to
be retrieved from the standard syntax table.
*/

#ifdef MIRROR_TABLE

/*

[[ Since the logic involved in finding the actual integer isn't very
complex, you'd think the time required to retrieve it is not a
factor.  If you thought that, however, you'd be wrong, due to the
high number of times (many per character) that the syntax value is
accessed in functions such as scan_lists().  To speed this up,
we maintain a mirror syntax table that contains the actual
integers.  We can do this successfully because syntax tables are
now an abstract type, where we control all access. ]]

I'm not sure I believe this any more, and maintaining these tables is
more difficult with the new char tables, and is time consuming in and of
itself.  For the moment, we're just disabling this; redo if profiling shows
the need. --ben

*/

#endif /* MIRROR_TABLE */

enum syntaxcode
{
  Swhitespace,	/* whitespace character */
  Spunct,	/* random punctuation character */
  Sword,	/* word constituent */
  Ssymbol,	/* symbol constituent but not word constituent */
  Sopen,	/* a beginning delimiter */
  Sclose,	/* an ending delimiter */
  Squote,	/* a prefix character like Lisp ' */
  Sstring,	/* a string-grouping character like Lisp " */
  Smath,	/* delimiters like $ in TeX. */
  Sescape,	/* a character that begins a C-style escape */
  Scharquote,	/* a character that quotes the following character */
  Scomment,	/* a comment-starting character */
  Sendcomment,	/* a comment-ending character */
  Sinherit,	/* use the standard syntax table for this character */
  Scomment_fence, /* Starts/ends comment which is delimited on the
		     other side by a char with the same syntaxcode.  */
  Sstring_fence,  /* Starts/ends string which is delimited on the
		     other side by a char with the same syntaxcode.  */
  Smax	 /* Upper bound on codes that are meaningful */
};

/* Retrieve the syntax table from a buffer. */
DECLARE_INLINE_HEADER (
Lisp_Object
BUFFER_SYNTAX_TABLE (struct buffer *buf)
)
{
  return buf ? buf->syntax_table : Vstandard_syntax_table;
}

/* Retrieve the mirror syntax table from a buffer. */
DECLARE_INLINE_HEADER (
Lisp_Object
BUFFER_MIRROR_SYNTAX_TABLE (struct buffer *buf)
)
{
#ifdef MIRROR_TABLE
  return buf ? buf->mirror_syntax_table :
    XCHAR_TABLE (Vstandard_syntax_table)->mirror_table;
#else
  return BUFFER_SYNTAX_TABLE (buf);
#endif /* MIRROR_TABLE */
}


#ifdef MULE

/* Retrieve the category table from a buffer. */
DECLARE_INLINE_HEADER (
Lisp_Object
BUFFER_CATEGORY_TABLE (struct buffer *buf)
)
{
  return buf ? buf->category_table : Vstandard_category_table;
}

#endif /* MULE */

void update_syntax_table (Lisp_Object table);

#ifdef MIRROR_TABLE

DECLARE_INLINE_HEADER (
void
update_mirror_syntax_if_dirty (Lisp_Object table)
)
{
  if (XCHAR_TABLE (table)->dirty)
    update_syntax_table (table);
}

#endif /* MIRROR_TABLE */

#define SYNTAX_FROM_CODE(code) ((enum syntaxcode) ((code) & 0177))

/* Return the syntax code for a particular character and mirror table. */

DECLARE_INLINE_HEADER (
int
SYNTAX_CODE (Lisp_Object table, Ichar c)
)
{
  Lisp_Object code;
#ifdef MIRROR_TABLE
  type_checking_assert (XCHAR_TABLE (table)->mirror_table_p);
  update_mirror_syntax_if_dirty (table);
  return XINT (get_char_table_1 (c, table));
#else
  code = get_char_table (c, table);

  /* #### It's possible this code will be time consuming because of getting
     run in an inner-loop.  But it's all inlined. */

  if (CONSP (code))
    code = XCAR (code);
  if (SYNTAX_FROM_CODE (XINT (code)) == Sinherit)
    {
      code = get_char_table (c, Vstandard_syntax_table);
      if (CONSP (code))
	code = XCAR (code);
    }

  return XINT (code);
#endif /* MIRROR_TABLE */
}

#ifdef NOT_WORTH_THE_EFFORT

/* Same but skip the dirty check. */

DECLARE_INLINE_HEADER (
int
SYNTAX_CODE_1 (Lisp_Object table, Ichar c)
)
{
  type_checking_assert (XCHAR_TABLE (table)->mirror_table_p);
  return (enum syntaxcode) XINT (get_char_table_1 (c, table));
}

#endif /* NOT_WORTH_THE_EFFORT */

#define SYNTAX(table, c) SYNTAX_FROM_CODE (SYNTAX_CODE (table, c))

DECLARE_INLINE_HEADER (
int
WORD_SYNTAX_P (Lisp_Object table, Ichar c)
)
{
  return SYNTAX (table, c) == Sword;
}

/* OK, here's a graphic diagram of the format of the syntax values:

   Bit number:

 [ 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 ]
 [ 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 ]

   <-----> <-----> <-------------> <-------------> ^  <----------->
    ELisp  unused  |comment bits |     unused      |   syntax code
     tag           | | | | | | | |                 |
    stuff          | | | | | | | |                 |
                   | | | | | | | |                 |
                   | | | | | | | |                 `--> prefix flag
                   | | | | | | | |
                   | | | | | | | `--> comment end style B, second char
                   | | | | | | `----> comment end style A, second char
                   | | | | | `------> comment end style B, first char
                   | | | | `--------> comment end style A, first char
                   | | | `----------> comment start style B, second char
                   | | `------------> comment start style A, second char
                   | `--------------> comment start style B, first char
                   `----------------> comment start style A, first char

  In a 64-bit integer, there would be 32 more unused bits between
  the tag and the comment bits.

  Clearly, such a scheme will not work for Mule, because the matching
  paren could be any character and as such requires 21 bits, which
  we don't got.

  Remember that under Mule we use char tables instead of vectors.
  So what we do is use another char table for the matching paren
  and store a pointer to it in the first char table. (This frees
  code from having to worry about passing two tables around.)
*/


/* The prefix flag bit for backward-prefix-chars is now put into bit 7. */

#define SYNTAX_PREFIX(table, c) \
  ((SYNTAX_CODE (table, c) >> 7) & 1)

/* Bits 23-16 are used to implement up to two comment styles
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

#if 0

/* #### Entirely unused.  Should they be deleted? */

/* #### These are now more or less equivalent to
   SYNTAX_COMMENT_MATCH_START ...*/
/* a and b must be first and second start chars for a common type */
#define SYNTAX_START_P(table, a, b)                                     \
  (((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_START) >> 2)    \
   & (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_START))

/* ... and  SYNTAX_COMMENT_MATCH_END */
/* a and b must be first and second end chars for a common type */
#define SYNTAX_END_P(table, a, b)                                       \
  (((SYNTAX_COMMENT_BITS (table, a) & SYNTAX_FIRST_CHAR_END) >> 2)      \
   & (SYNTAX_COMMENT_BITS (table, b) & SYNTAX_SECOND_CHAR_END))

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

#endif /* 0 */

/* This array, indexed by a character, contains the syntax code which
   that character signifies (as a char).
   For example, (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern const unsigned char syntax_spec_code[0400];

/* Indexed by syntax code, give the letter that describes it. */

extern const unsigned char syntax_code_spec[];

Lisp_Object scan_lists (struct buffer *buf, Charbpos from, int count,
			int depth, int sexpflag, int no_error);
int char_quoted (struct buffer *buf, Charbpos pos);

/* NOTE: This does not refer to the mirror table, but to the
   syntax table itself. */
Lisp_Object syntax_match (Lisp_Object table, Ichar ch);

extern int no_quit_in_re_search;


/****************************** syntax caches ********************************/

extern int lookup_syntax_properties;

/* Now that the `syntax-table' property exists, and can override the syntax
   table or directly specify the syntax, we cache the last place we
   retrieved the syntax-table property.  This is because, when moving
   linearly through text (e.g. in the regex routines or the scanning
   routines in syntax.c), we only need to recalculate at the next place the
   syntax-table property changes (i.e. not every position), and when we do
   need to recalculate, we can update the info from the previous info
   faster than if we did the whole calculation from scratch. */
struct syntax_cache
{
#ifdef NEW_GC
  struct lrecord_header header;
#endif /* NEW_GC */
  int use_code;				/* Whether to use syntax_code or
					   syntax_table.  This is set
					   depending on whether the
					   syntax-table property is a
					   syntax table or a syntax
					   code. */
  int no_syntax_table_prop;		/* If non-zero, there was no
					   `syntax-table' property on the
					   current range, and so we're
					   using the buffer's syntax table.
					   This is important to note because
					   sometimes the buffer's syntax
					   table can be changed. */
  Lisp_Object object;			/* The buffer or string the current
					   syntax cache applies to, or
					   Qnil for a string of text not
					   coming from a buffer or string. */
  struct buffer *buffer;		/* The buffer that supplies the
					   syntax tables, or 0 for the
					   standard syntax table.  If
					   OBJECT is a buffer, this will
					   always be the same buffer. */
  int syntax_code;			/* Syntax code of current char. */
  Lisp_Object syntax_table;		/* Syntax table for current pos. */
#ifdef MIRROR_TABLE
  Lisp_Object mirror_table;		/* Mirror table for this table. */
#endif /* MIRROR_TABLE */
  Lisp_Object start, end;		/* Markers to keep track of the
					   known region in a buffer.
					   Formerly we used an internal
					   extent, but it seems that having
					   an extent over the entire buffer
					   causes serious slowdowns in
					   extent operations!  Yuck! */
  Charxpos next_change;			/* Position of the next extent
                                           change. */
  Charxpos prev_change;			/* Position of the previous extent
					   change. */
};

#ifdef NEW_GC
typedef struct syntax_cache Lisp_Syntax_Cache;

DECLARE_LRECORD (syntax_cache, Lisp_Syntax_Cache);

#define XSYNTAX_CACHE(x) \
  XRECORD (x, syntax_cache, Lisp_Syntax_Cache)
#define wrap_syntax_cache(p) wrap_record (p, syntax_cache)
#define SYNTAX_CACHE_P(x) RECORDP (x, syntax_cache)
#define CHECK_SYNTAX_CACHE(x) CHECK_RECORD (x, syntax_cache)
#define CONCHECK_SYNTAX_CACHE(x) CONCHECK_RECORD (x, syntax_cache)
#endif /* NEW_GC */



extern const struct sized_memory_description syntax_cache_description;

/* Note that the external interface to the syntax-cache uses charpos's, but
   internally we use bytepos's, for speed. */

void update_syntax_cache (struct syntax_cache *cache, Charxpos pos, int count);
struct syntax_cache *setup_syntax_cache (struct syntax_cache *cache,
					 Lisp_Object object,
					 struct buffer *buffer,
					 Charxpos from, int count);
struct syntax_cache *setup_buffer_syntax_cache (struct buffer *buffer,
						Charxpos from, int count);

/* Make syntax cache state good for CHARPOS, assuming it is
   currently good for a position before CHARPOS.  */
DECLARE_INLINE_HEADER (
void
UPDATE_SYNTAX_CACHE_FORWARD (struct syntax_cache *cache, Charxpos pos)
)
{
  /* #### Formerly this function, and the next one, had

     if (pos < cache->prev_change || pos >= cache->next_change)

     just like for plain UPDATE_SYNTAX_CACHE.  However, sometimes the
     value of POS may be invalid (particularly, it may be 0 for a buffer).
     FSF has the check at only one end, so let's try the same. */
  if (pos >= cache->next_change)
    update_syntax_cache (cache, pos, 1);
}

/* Make syntax cache state good for CHARPOS, assuming it is
   currently good for a position after CHARPOS.  */
DECLARE_INLINE_HEADER (
void
UPDATE_SYNTAX_CACHE_BACKWARD (struct syntax_cache *cache, Charxpos pos)
)
{
  if (pos < cache->prev_change)
    update_syntax_cache (cache, pos, -1);
}

/* Make syntax cache state good for CHARPOS */
DECLARE_INLINE_HEADER (
void
UPDATE_SYNTAX_CACHE (struct syntax_cache *cache, Charxpos pos)
)
{
  if (pos < cache->prev_change || pos >= cache->next_change)
    update_syntax_cache (cache, pos, 0);
}

#define SYNTAX_FROM_CACHE(cache, c)			\
   SYNTAX_FROM_CODE (SYNTAX_CODE_FROM_CACHE (cache, c))

#ifdef MIRROR_TABLE
#define SYNTAX_CODE_FROM_CACHE(cache, c)				\
  ((cache)->use_code ? (cache)->syntax_code				\
   : SYNTAX_CODE ((cache)->mirror_table, c))
#else
#define SYNTAX_CODE_FROM_CACHE(cache, c)				\
  ((cache)->use_code ? (cache)->syntax_code				\
   : SYNTAX_CODE ((cache)->syntax_table, c))
#endif /* MIRROR_TABLE */

#ifdef NOT_WORTH_THE_EFFORT
/* If we really cared about the theoretical performance hit of the dirty
   check in SYNTAX_CODE, we could use SYNTAX_CODE_1 and endeavor to always
   keep the mirror table clean, e.g. by checking for dirtiness at the time
   we set up the syntax cache.  There are lots of potential problems, of
   course -- incomplete understanding of the possible pathways into the
   code, with some that are bypassing the setups, Lisp code being executed
   in the meantime that could change things (e.g. QUIT is called in many
   functions and could execute arbitrary Lisp very easily), etc.  The QUIT
   problem is the biggest one, probably, and one of the main reasons it's
   probably just not worth it. */
#define SYNTAX_CODE_FROM_CACHE(cache, c)				\
  ((cache)->use_code ? (cache)->syntax_code				\
   : SYNTAX_CODE_1 ((cache)->mirror_table, c))
#endif


/***************************** syntax code macros ****************************/

#define SYNTAX_CODE_PREFIX(c) \
  ((c >> 7) & 1)

#define SYNTAX_CODE_COMMENT_BITS(c) \
  ((c >> 16) &0xff)

#define SYNTAX_CODES_START_P(a, b)                                    \
  (((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START) >> 2)    \
   & (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_START))

#define SYNTAX_CODES_END_P(a, b)                                    \
  (((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END) >> 2)    \
   & (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END))

#define SYNTAX_CODES_COMMENT_MASK_START(a, b)			\
  (SYNTAX_CODES_MATCH_START_P (a, b, SYNTAX_COMMENT_STYLE_A) 	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (SYNTAX_CODES_MATCH_START_P (a, b, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
      : 0))
#define SYNTAX_CODES_COMMENT_MASK_END(a, b)			\
  (SYNTAX_CODES_MATCH_END_P (a, b, SYNTAX_COMMENT_STYLE_A) 	\
   ? SYNTAX_COMMENT_STYLE_A					\
   : (SYNTAX_CODES_MATCH_END_P (a, b, SYNTAX_COMMENT_STYLE_B)	\
      ? SYNTAX_COMMENT_STYLE_B					\
      : 0))

#define SYNTAX_CODE_START_FIRST_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START)

#define SYNTAX_CODE_START_SECOND_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_SECOND_CHAR_START)

#define SYNTAX_CODE_END_FIRST_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END)

#define SYNTAX_CODE_END_SECOND_P(a) \
  (SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_SECOND_CHAR_END)


#define SYNTAX_CODES_MATCH_START_P(a, b, mask)				\
  ((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_START & (mask))	\
   && (SYNTAX_CODE_COMMENT_BITS (b)					\
       & SYNTAX_SECOND_CHAR_START & (mask)))

#define SYNTAX_CODES_MATCH_END_P(a, b, mask)				\
  ((SYNTAX_CODE_COMMENT_BITS (a) & SYNTAX_FIRST_CHAR_END & (mask))	\
   && (SYNTAX_CODE_COMMENT_BITS (b) & SYNTAX_SECOND_CHAR_END & (mask)))

#define SYNTAX_CODE_MATCHES_1CHAR_P(a, mask)	\
  ((SYNTAX_CODE_COMMENT_BITS (a) & (mask)))

#define SYNTAX_CODE_COMMENT_1CHAR_MASK(a)			\
  ((SYNTAX_CODE_MATCHES_1CHAR_P (a, SYNTAX_COMMENT_STYLE_A)	\
    ? SYNTAX_COMMENT_STYLE_A					\
    : (SYNTAX_CODE_MATCHES_1CHAR_P (a, SYNTAX_COMMENT_STYLE_B)	\
       ? SYNTAX_COMMENT_STYLE_B					\
       : 0)))


#endif /* INCLUDED_syntax_h_ */
