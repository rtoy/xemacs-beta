/* XEmacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985-1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2003 Ben Wing.

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

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "syntax.h"
#include "extents.h"

/* Here is a comment from Ken'ichi HANDA <handa@etl.go.jp>
   explaining the purpose of the Sextword syntax category:

Japanese words are not separated by spaces, which makes finding word
boundaries very difficult.  Theoretically it's impossible without
using natural language processing techniques.  But, by defining
pseudo-words as below (much simplified for letting you understand it
easily) for Japanese, we can have a convenient forward-word function
for Japanese.

	A Japanese word is a sequence of characters that consists of
	zero or more Kanji characters followed by zero or more
	Hiragana characters.

Then, the problem is that now we can't say that a sequence of
word-constituents makes up a WORD.  For instance, both Hiragana "A"
and Kanji "KAN" are word-constituents but the sequence of these two
letters can't be a single word.

So, we introduced Sextword for Japanese letters.  A character of
Sextword is a word-constituent but a word boundary may exist between
two such characters.  */

/* Mule 2.4 doesn't seem to have Sextword - I'm removing it -- mrb */
/* Recovered by tomo */

#define ST_COMMENT_STYLE 0x101
#define ST_STRING_STYLE  0x102

Lisp_Object Qsyntax_table;
int lookup_syntax_properties;

Lisp_Object Qsyntax_table_p;

int words_include_escapes;

int parse_sexp_ignore_comments;

/* The following two variables are provided to tell additional information
   to the regex routines.  We do it this way rather than change the
   arguments to re_search_2() in an attempt to maintain some call
   compatibility with other versions of the regex code. */

/* Tell the regex routines not to QUIT.  Normally there is a QUIT
   each iteration in re_search_2(). */
int no_quit_in_re_search;

/* The standard syntax table is stored where it will automatically
   be used in all new buffers.  */
Lisp_Object Vstandard_syntax_table;

Lisp_Object Vsyntax_designator_chars_string;

Lisp_Object Vtemp_table_for_use_updating_syntax_tables;

/* A value that is guaranteed not be in a syntax table. */
Lisp_Object Vbogus_syntax_table_value;

static void syntax_cache_table_was_changed (struct buffer *buf);

/* This is the internal form of the parse state used in parse-partial-sexp.  */

struct lisp_parse_state
{
  int depth;		/* Depth at end of parsing */
  Ichar instring;	/* -1 if not within string, else desired terminator */
  int incomment;	/* Nonzero if within a comment at end of parsing */
  int comstyle;		/* comment style a=0, or b=1, or ST_COMMENT_STYLE */
  int quoted;		/* Nonzero if just after an escape char at end of
			   parsing */
  Charbpos thislevelstart;/* Char number of most recent start-of-expression
                           at current level */
  Charbpos prevlevelstart;/* Char number of start of containing expression */
  Charbpos location;	/* Char number at which parsing stopped */
  int mindepth;		/* Minimum depth seen while scanning  */
  Charbpos comstr_start;/* Position just after last comment/string starter */
  Lisp_Object levelstarts;/* Char numbers of starts-of-expression
			     of levels (starting from outermost).  */
};

/* These variables are a cache for finding the start of a defun.
   find_start_pos    is the place for which the defun start was found.
   find_start_value  is the defun start position found for it.
   find_start_buffer is the buffer it was found in.
   find_start_begv   is the BEGV value when it was found.
   find_start_modiff is the value of MODIFF when it was found.  */

static Charbpos find_start_pos;
static Charbpos find_start_value;
static struct buffer *find_start_buffer;
static Charbpos find_start_begv;
static int find_start_modiff;

/* Find a defun-start that is the last one before POS (or nearly the last).
   We record what we find, so that another call in the same area
   can return the same value right away.  */

static Charbpos
find_defun_start (struct buffer *buf, Charbpos pos)
{
  Charbpos tem;
  struct syntax_cache *scache;
  
  /* Use previous finding, if it's valid and applies to this inquiry.  */
  if (buf == find_start_buffer
      /* Reuse the defun-start even if POS is a little farther on.
	 POS might be in the next defun, but that's ok.
	 Our value may not be the best possible, but will still be usable.  */
      && pos <= find_start_pos + 1000
      && pos >= find_start_value
      && BUF_BEGV (buf) == find_start_begv
      && BUF_MODIFF (buf) == find_start_modiff)
    return find_start_value;

  /* Back up to start of line.  */
  tem = find_next_newline (buf, pos, -1);

  scache = setup_buffer_syntax_cache (buf, tem, 1);
  while (tem > BUF_BEGV (buf))
    {
      UPDATE_SYNTAX_CACHE_BACKWARD (scache, tem);

      /* Open-paren at start of line means we found our defun-start.  */
      if (SYNTAX_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, tem)) == Sopen)
	break;
      /* Move to beg of previous line.  */
      tem = find_next_newline (buf, tem, -2);
    }

  /* Record what we found, for the next try.  */
  find_start_value  = tem;
  find_start_buffer = buf;
  find_start_modiff = BUF_MODIFF (buf);
  find_start_begv   = BUF_BEGV (buf);
  find_start_pos    = pos;

  return find_start_value;
}

DEFUN ("syntax-table-p", Fsyntax_table_p, 1, 1, 0, /*
Return t if OBJECT is a syntax table.
Any vector of 256 elements will do.
*/
       (object))
{
  return (CHAR_TABLEP (object)
	  && XCHAR_TABLE_TYPE (object) == CHAR_TABLE_TYPE_SYNTAX)
    ? Qt : Qnil;
}

static Lisp_Object
check_syntax_table (Lisp_Object obj, Lisp_Object default_)
{
  if (NILP (obj))
    obj = default_;
  while (NILP (Fsyntax_table_p (obj)))
    obj = wrong_type_argument (Qsyntax_table_p, obj);
  return obj;
}

DEFUN ("syntax-table", Fsyntax_table, 0, 1, 0, /*
Return the current syntax table.
This is the one specified by the current buffer, or by BUFFER if it
is non-nil.
*/
       (buffer))
{
  return decode_buffer (buffer, 0)->syntax_table;
}

#ifdef DEBUG_XEMACS

DEFUN ("mirror-syntax-table", Fmirror_syntax_table, 0, 1, 0, /*
Return the current mirror syntax table, for debugging purposes.
This is the one specified by the current buffer, or by BUFFER if it
is non-nil.
*/
       (buffer))
{
  return decode_buffer (buffer, 0)->mirror_syntax_table;
}

DEFUN ("syntax-cache-info", Fsyntax_cache_info, 0, 1, 0, /*
Return info about the syntax cache in BUFFER.
BUFFER defaults to the current buffer if nil.
*/
       (buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  struct syntax_cache *cache = buf->syntax_cache;
  return list4 (cache->start, cache->end, make_int (cache->prev_change),
		make_int (cache->next_change));
}

#endif /* DEBUG_XEMACS */

DEFUN ("standard-syntax-table", Fstandard_syntax_table, 0, 0, 0, /*
Return the standard syntax table.
This is the one used for new buffers.
*/
       ())
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, 0, 1, 0, /*
Return a new syntax table which is a copy of SYNTAX-TABLE.
SYNTAX-TABLE defaults to the standard syntax table.
*/
       (syntax_table))
{
  if (NILP (Vstandard_syntax_table))
    return Fmake_char_table (Qsyntax);

  syntax_table = check_syntax_table (syntax_table, Vstandard_syntax_table);
  return Fcopy_char_table (syntax_table);
}

DEFUN ("set-syntax-table", Fset_syntax_table, 1, 2, 0, /*
Select SYNTAX-TABLE as the new syntax table for BUFFER.
BUFFER defaults to the current buffer if omitted.
*/
       (syntax_table, buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  syntax_table = check_syntax_table (syntax_table, Qnil);
  buf->syntax_table = syntax_table;
  buf->mirror_syntax_table = XCHAR_TABLE (syntax_table)->mirror_table;
  syntax_cache_table_was_changed (buf);
  /* Indicate that this buffer now has a specified syntax table.  */
  buf->local_var_flags |= XINT (buffer_local_flags.syntax_table);
  return syntax_table;
}


static void
init_syntax_cache (struct syntax_cache *cache, Lisp_Object object,
		    struct buffer *buffer, int infinite)
{
  xzero (*cache);
  cache->object = object;
  cache->buffer = buffer;
  cache->no_syntax_table_prop = 1;
  cache->syntax_table =
    BUFFER_SYNTAX_TABLE (cache->buffer);
  cache->mirror_table =
    BUFFER_MIRROR_SYNTAX_TABLE (cache->buffer);
  cache->start = Qnil;
  cache->end = Qnil;
  if (infinite)
    {
      cache->prev_change = EMACS_INT_MIN;
      cache->next_change = EMACS_INT_MAX;
    }
  else
    {
      cache->prev_change = -1;
      cache->next_change = -1;
    }
}

struct syntax_cache *
setup_syntax_cache (struct syntax_cache *cache, Lisp_Object object,
		    struct buffer *buffer, Charxpos from, int count)
{
  if (BUFFERP (object))
    cache = XBUFFER (object)->syntax_cache;
  if (!lookup_syntax_properties)
    init_syntax_cache (cache, object, buffer, 1);
  else if (!BUFFERP (object))
    init_syntax_cache (cache, object, buffer, 0);
  if (lookup_syntax_properties)
    {
      if (count <= 0)
	{
	  from--;
	  from = buffer_or_string_clip_to_accessible_char (cache->object,
							   from);
	}
      if (!(from >= cache->prev_change && from < cache->next_change))
	update_syntax_cache (cache, from, count);
    }
#ifdef NOT_WORTH_THE_EFFORT
  update_mirror_syntax_if_dirty (cache->mirror_table);
#endif /* NOT_WORTH_THE_EFFORT */
  return cache;
}

struct syntax_cache *
setup_buffer_syntax_cache (struct buffer *buffer, Charxpos from, int count)
{
  return setup_syntax_cache (NULL, wrap_buffer (buffer), buffer, from, count);
}

static const struct memory_description syntax_cache_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, object) },
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, buffer) },
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, syntax_table) },
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, mirror_table) },
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, start) },
  { XD_LISP_OBJECT, offsetof (struct syntax_cache, end) },
  { XD_END }
};

const struct sized_memory_description syntax_cache_description = {
  sizeof (struct syntax_cache),
  syntax_cache_description_1
};

void
mark_buffer_syntax_cache (struct buffer *buf)
{
  struct syntax_cache *cache = buf->syntax_cache;
  if (!cache) /* Vbuffer_defaults and such don't have caches */
    return;
  mark_object (cache->object);
  if (cache->buffer)
    mark_object (wrap_buffer (cache->buffer));
  mark_object (cache->syntax_table);
  mark_object (cache->mirror_table);
  mark_object (cache->start);
  mark_object (cache->end);
}

static void
reset_buffer_cache_range (struct syntax_cache *cache, Lisp_Object buffer)
{
  Fset_marker (cache->start, make_int (1), buffer);
  Fset_marker (cache->end, make_int (1), buffer);
  Fset_marker_insertion_type (cache->start, Qt);
  Fset_marker_insertion_type (cache->end, Qnil);
  cache->prev_change = -1;
  cache->next_change = -1;
}

void
init_buffer_syntax_cache (struct buffer *buf)
{
  struct syntax_cache *cache;
  buf->syntax_cache = xnew_and_zero (struct syntax_cache);
  cache = buf->syntax_cache;
  cache->object = wrap_buffer (buf);
  cache->buffer = buf;
  cache->no_syntax_table_prop = 1;
  cache->syntax_table = BUFFER_SYNTAX_TABLE (cache->buffer);
  cache->mirror_table = BUFFER_MIRROR_SYNTAX_TABLE (cache->buffer);
  cache->start = Fmake_marker ();
  cache->end = Fmake_marker ();
  reset_buffer_cache_range (cache, cache->object);
}

void
uninit_buffer_syntax_cache (struct buffer *buf)
{
  xfree (buf->syntax_cache, struct syntax_cache *);
  buf->syntax_cache = 0;
}


static void
syntax_cache_table_was_changed (struct buffer *buf)
{
  struct syntax_cache *cache = buf->syntax_cache;
  if (cache->no_syntax_table_prop)
    {
      cache->syntax_table =
	BUFFER_SYNTAX_TABLE (buf);
      cache->mirror_table =
	BUFFER_MIRROR_SYNTAX_TABLE (buf);
    }
}

/* The syntax-table property on the range covered by EXTENT may be changing,
   either because EXTENT has a syntax-table property and is being attached
   or detached (this includes having its endpoints changed), or because
   the value of EXTENT's syntax-table property is changing. */

void
signal_syntax_table_extent_changed (EXTENT extent)
{
  Lisp_Object buffer = Fextent_object (wrap_extent (extent));
  if (BUFFERP (buffer))
    {
      struct syntax_cache *cache = XBUFFER (buffer)->syntax_cache;
      Bytexpos start = extent_endpoint_byte (extent, 0);
      Bytexpos end = extent_endpoint_byte (extent, 1);
      Bytexpos start2 = byte_marker_position (cache->start);
      Bytexpos end2 = byte_marker_position (cache->end);
      /* If the extent is entirely before or entirely after the cache range,
	 it doesn't overlap.  Otherwise, invalidate the range. */
      if (!(end < start2 || start > end2))
	reset_buffer_cache_range (cache, buffer);
    }
}

/* Extents have been adjusted for insertion or deletion, so we need to
   refetch the start and end position of the extent */
void
signal_syntax_table_extent_adjust (struct buffer *buf)
{
  struct syntax_cache *cache = buf->syntax_cache;
  /* If the cache was invalid before, leave it that way.  We only want
     to update the limits of validity when they were actually valid. */
  if (cache->prev_change < 0)
    return;
  cache->prev_change = marker_position (cache->start);
  cache->next_change = marker_position (cache->end);
}

/* 
   Update syntax_cache to an appropriate setting for position POS

   The sign of COUNT gives the relative position of POS wrt the
   previously valid interval.  (not currently used)

   `syntax_cache.*_change' are the next and previous positions at
   which syntax_code and c_s_t will need to be recalculated.

   #### Currently this code uses 'get-char-property', which will
   return the "last smallest" extent at a given position. In cases
   where overlapping extents are defined, this code will simply use
   whatever is returned by get-char-property.

   It might be worth it at some point to merge provided syntax tables
   outward to the current buffer (#### rewrite in English please?!). */

void
update_syntax_cache (struct syntax_cache *cache, Charxpos cpos,
		     int UNUSED (count))
{
  Lisp_Object tmp_table;
  Bytexpos pos;
  Bytexpos lim;
  Bytexpos next, prev;
  int at_begin = 0, at_end = 0;

  if (NILP (cache->object))
    return;

  pos = buffer_or_string_charxpos_to_bytexpos (cache->object, cpos);

  tmp_table = get_char_property (pos, Qsyntax_table, cache->object,
				 EXTENT_AT_AFTER, 0);
  lim = next_single_property_change (pos, Qsyntax_table, cache->object,
				     -1);
  if (lim < 0)
    {
      next = buffer_or_string_absolute_end_byte (cache->object);
      at_begin = 1;
    }
  else
    next = lim;

  if (pos < buffer_or_string_absolute_end_byte (cache->object))
    pos = next_bytexpos (cache->object, pos);
  lim = previous_single_property_change (pos, Qsyntax_table, cache->object,
					 -1);
  if (lim < 0)
    {
      prev = buffer_or_string_absolute_begin_byte (cache->object);
      at_end = 1;
    }
  else
    prev = lim;

  cache->prev_change =
    buffer_or_string_bytexpos_to_charxpos (cache->object, prev);
  cache->next_change =
    buffer_or_string_bytexpos_to_charxpos (cache->object, next);

  if (BUFFERP (cache->object))
    {
      /* If we are at the beginning or end of buffer, check to see if there's
	 a zero-length `syntax-table' extent there (highly unlikely); if not,
	 then we can safely make the end closed, so it will take in newly
	 inserted text. (If such an extent is inserted, we will be informed
	 through signal_syntax_table_extent_changed().) */
      Fset_marker (cache->start, make_int (cache->prev_change), cache->object);
      Fset_marker_insertion_type
	(cache->start,
	 at_begin && NILP (extent_at (prev, cache->object, Qsyntax_table,
				      NULL, EXTENT_AT_AT, 0))
	 ? Qnil : Qt);
      Fset_marker (cache->end, make_int (cache->next_change), cache->object);
      Fset_marker_insertion_type
	(cache->end,
	 at_end && NILP (extent_at (next, cache->object, Qsyntax_table,
				    NULL, EXTENT_AT_AT, 0))
	 ? Qt : Qnil);
    }  
  
  if (!NILP (Fsyntax_table_p (tmp_table)))
    {
      cache->use_code = 0;
      cache->syntax_table = tmp_table;
      cache->mirror_table = XCHAR_TABLE (tmp_table)->mirror_table;
      cache->no_syntax_table_prop = 0;
#ifdef NOT_WORTH_THE_EFFORT
      update_mirror_syntax_if_dirty (cache->mirror_table);
#endif /* NOT_WORTH_THE_EFFORT */
    } 
  else if (CONSP (tmp_table) && INTP (XCAR (tmp_table)))
    {
      cache->use_code = 1;
      cache->syntax_code = XINT (XCAR (tmp_table));
      cache->no_syntax_table_prop = 0;
    }
  else 
    {
      cache->use_code = 0;
      cache->no_syntax_table_prop = 1;
      cache->syntax_table = BUFFER_SYNTAX_TABLE (cache->buffer);
      cache->mirror_table = BUFFER_MIRROR_SYNTAX_TABLE (cache->buffer);
#ifdef NOT_WORTH_THE_EFFORT
      update_mirror_syntax_if_dirty (cache->mirror_table);
#endif /* NOT_WORTH_THE_EFFORT */
    }
}

/* Convert a letter which signifies a syntax code
   into the code it signifies.
   This is used by modify-syntax-entry, and other things. */

const unsigned char syntax_spec_code[0400] =
{ 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  (char) Swhitespace, 0377, (char) Sstring, 0377,
      (char) Smath, 0377, 0377, (char) Squote,
  (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
  (char) Sinherit, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A ... */
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
  0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
  0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
  0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
  0377, 0377, 0377, 0377, (char) Sstring_fence, 0377, 0377, 0377
};

const unsigned char syntax_code_spec[] =  " .w_()'\"$\\/<>@!|";

DEFUN ("syntax-designator-chars", Fsyntax_designator_chars, 0, 0, 0, /*
Return a string of the recognized syntax designator chars.
The chars are ordered by their internal syntax codes, which are
numbered starting at 0.
*/
       ())
{
  return Vsyntax_designator_chars_string;
}

DEFUN ("char-syntax", Fchar_syntax, 1, 2, 0, /*
Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent,
the character `?w' is returned.
The characters that correspond to various syntax codes
are listed in the documentation of `modify-syntax-entry'.
Optional second argument SYNTAX-TABLE defaults to the current buffer's
syntax table.
*/
       (character, syntax_table))
{
  Lisp_Object mirrortab;

  if (NILP (character))
    {
      character = make_char ('\000');
    }
  CHECK_CHAR_COERCE_INT (character);
  syntax_table = check_syntax_table (syntax_table,
				     current_buffer->syntax_table);
  mirrortab = XCHAR_TABLE (syntax_table)->mirror_table;
  return make_char (syntax_code_spec[(int) SYNTAX (mirrortab,
						   XCHAR (character))]);
}

#ifdef MULE

enum syntaxcode
charset_syntax (struct buffer *UNUSED (buf), Lisp_Object UNUSED (charset),
		int *multi_p_out)
{
  *multi_p_out = 1;
  /* !!#### get this right */
  return Spunct;
}

#endif

Lisp_Object
syntax_match (Lisp_Object syntax_table, Ichar ch)
{
  Lisp_Object code = get_char_table (ch, syntax_table);
  Lisp_Object code2 = code;

  if (CONSP (code))
    code2 = XCAR (code);
  if (SYNTAX_FROM_CODE (XINT (code2)) == Sinherit)
    code = get_char_table (ch, Vstandard_syntax_table);

  return CONSP (code) ? XCDR (code) : Qnil;
}

DEFUN ("matching-paren", Fmatching_paren, 1, 2, 0, /*
Return the matching parenthesis of CHARACTER, or nil if none.
Optional second argument SYNTAX-TABLE defaults to the current buffer's
syntax table.
*/
       (character, syntax_table))
{
  Lisp_Object mirrortab;
  enum syntaxcode code;

  CHECK_CHAR_COERCE_INT (character);
  syntax_table = check_syntax_table (syntax_table,
				     current_buffer->syntax_table);
  mirrortab = XCHAR_TABLE (syntax_table)->mirror_table;
  code = SYNTAX (mirrortab, XCHAR (character));
  if (code == Sopen || code == Sclose || code == Sstring)
    return syntax_match (syntax_table, XCHAR (character));
  return Qnil;
}



#ifdef MULE
/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   There is no word boundary between two word-constituent ASCII
   characters.  */
#define WORD_BOUNDARY_P(c1, c2)			\
  (!(ichar_ascii_p (c1) && ichar_ascii_p (c2))	\
   && word_boundary_p (c1, c2))
#endif

/* Return the position across COUNT words from FROM.
   If that many words cannot be found before the end of the buffer, return 0.
   COUNT negative means scan backward and stop at word beginning.  */

Charbpos
scan_words (struct buffer *buf, Charbpos from, int count)
{
  Charbpos limit = count > 0 ? BUF_ZV (buf) : BUF_BEGV (buf);
  Ichar ch0, ch1;
  enum syntaxcode code;
  struct syntax_cache *scache = setup_buffer_syntax_cache (buf, from, count);

  /* #### is it really worth it to hand expand both cases? JV */
  while (count > 0)
    {
      QUIT;

      while (1)
	{
	  if (from == limit)
	    return 0;

	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  ch0 = BUF_FETCH_CHAR (buf, from);
	  code = SYNTAX_FROM_CACHE (scache, ch0);

	  from++;
	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}

      QUIT;

      while (from != limit)
	{
	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  ch1 = BUF_FETCH_CHAR (buf, from);
	  code = SYNTAX_FROM_CACHE (scache, ch1);
	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword
#ifdef MULE
		|| WORD_BOUNDARY_P (ch0, ch1)
#endif
		)
	      break;
#ifdef MULE
	  ch0 = ch1;
#endif
	  from++;
	}
      count--;
    }

  while (count < 0)
    {
      QUIT;

      while (1)
	{
	  if (from == limit)
	    return 0;

	  UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
	  ch1 = BUF_FETCH_CHAR (buf, from - 1);
	  code = SYNTAX_FROM_CACHE (scache, ch1);
	  from--;

	  if (words_include_escapes
	      && (code == Sescape || code == Scharquote))
	    break;
	  if (code == Sword)
	    break;
	}

      QUIT;

      while (from != limit)
	{
	  UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
	  ch0 = BUF_FETCH_CHAR (buf, from - 1);
	  code = SYNTAX_FROM_CACHE (scache, ch0);

	  if (!(words_include_escapes
		&& (code == Sescape || code == Scharquote)))
	    if (code != Sword
#ifdef MULE
		|| WORD_BOUNDARY_P (ch0, ch1)
#endif
		)
	      break;
#ifdef MULE
	  ch1 = ch0;
#endif
	  from--;
	}
      count++;
    }

  return from;
}

DEFUN ("forward-word", Fforward_word, 0, 2, "_p", /*
Move point forward COUNT words (backward if COUNT is negative).
Normally t is returned, but if an edge of the buffer is reached,
point is left there and nil is returned.

The characters that are moved over may be added to the current selection
\(i.e. active region) if the Shift key is held down, a motion key is used
to invoke this command, and `shifted-motion-keys-select-region' is t; see
the documentation for this variable for more details.

COUNT defaults to 1, and BUFFER defaults to the current buffer.
*/
       (count, buffer))
{
  Charbpos val;
  struct buffer *buf = decode_buffer (buffer, 0);
  EMACS_INT n;

  if (NILP (count))
    n = 1;
  else
    {
      CHECK_INT (count);
      n = XINT (count);
    }

  val = scan_words (buf, BUF_PT (buf), n);
  if (val)
    {
      BUF_SET_PT (buf, val);
      return Qt;
    }
  else
    {
      BUF_SET_PT (buf, n > 0 ? BUF_ZV (buf) : BUF_BEGV (buf));
      return Qnil;
    }
}

static void scan_sexps_forward (struct buffer *buf,
				struct lisp_parse_state *,
				Charbpos from, Charbpos end,
				int targetdepth, int stopbefore,
				Lisp_Object oldstate,
				int commentstop);

static int
find_start_of_comment (struct buffer *buf, Charbpos from, Charbpos stop,
		       int comstyle)
{
  Ichar c;
  enum syntaxcode code;

  /* Look back, counting the parity of string-quotes,
     and recording the comment-starters seen.
     When we reach a safe place, assume that's not in a string;
     then step the main scan to the earliest comment-starter seen
     an even number of string quotes away from the safe place.

     OFROM[I] is position of the earliest comment-starter seen
     which is I+2X quotes from the comment-end.
     PARITY is current parity of quotes from the comment end.  */
  int parity = 0;
  Ichar my_stringend = 0;
  int string_lossage = 0;
  Charbpos comment_end = from;
  Charbpos comstart_pos = 0;
  int comstart_parity = 0;
  int styles_match_p = 0;
  /* mask to match comment styles against; for ST_COMMENT_STYLE, this
     will get set to SYNTAX_COMMENT_STYLE_B, but never get checked */
  int mask = comstyle ? SYNTAX_COMMENT_STYLE_B : SYNTAX_COMMENT_STYLE_A;
  struct syntax_cache *scache = buf->syntax_cache;

  /* At beginning of range to scan, we're outside of strings;
     that determines quote parity to the comment-end.  */
  while (from != stop)
    {
      int syncode;

      /* Move back and examine a character.  */
      from--;
      UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);

      c = BUF_FETCH_CHAR (buf, from);
      syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
      code = SYNTAX_FROM_CODE (syncode);

      /* is this a 1-char comment end sequence? if so, try
	 to see if style matches previously extracted mask */
      if (code == Sendcomment)
	{
	  styles_match_p =
	    SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode) & mask;
	}

      /* or are we looking at a 1-char comment start sequence
	 of the style matching mask? */
      else if (code == Scomment)
	{
	  styles_match_p =
	    SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode) & mask;
	}

      /* otherwise, is this a 2-char comment end or start sequence? */
      else if (from > stop)
	do
	  {
	    /* 2-char comment end sequence? */
	    if (SYNTAX_CODE_END_SECOND_P (syncode))
	      {
		int prev_syncode;
		UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
		prev_syncode =
		  SYNTAX_CODE_FROM_CACHE (scache,
					  BUF_FETCH_CHAR (buf, from - 1));

		if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		  {
		    code = Sendcomment;
		    styles_match_p =
		      SYNTAX_CODES_COMMENT_MASK_END (prev_syncode,
						     syncode) & mask;
		    from--;
		    UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);
		    c = BUF_FETCH_CHAR (buf, from);

		    /* Found a comment-end sequence, so skip past the
		       check for a comment-start */
		    break;
		  }
	      }

	    /* 2-char comment start sequence? */
	    if (SYNTAX_CODE_START_SECOND_P (syncode))
	      {
		int prev_syncode;
		UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
		prev_syncode =
		  SYNTAX_CODE_FROM_CACHE (scache,
					  BUF_FETCH_CHAR (buf, from - 1));

		if (SYNTAX_CODES_START_P (prev_syncode, syncode))
		  {
		    code = Scomment;
		    styles_match_p =
		      SYNTAX_CODES_COMMENT_MASK_START (prev_syncode,
						       syncode) & mask;
		    from--;
		    UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);
		    c = BUF_FETCH_CHAR (buf, from);
		  }
	      }
	  } while (0);

      /* Ignore escaped characters.  */
      if (char_quoted (buf, from))
	continue;

      /* Track parity of quotes.  */
      if (code == Sstring)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend = c;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != c)
	    string_lossage = 1;
	}

      if (code == Sstring_fence || code == Scomment_fence)
	{
	  parity ^= 1;
	  if (my_stringend == 0)
	    my_stringend =
	      code == Sstring_fence ? ST_STRING_STYLE : ST_COMMENT_STYLE;
	  /* If we have two kinds of string delimiters.
	     There's no way to grok this scanning backwards.  */
	  else if (my_stringend != (code == Sstring_fence 
				    ? ST_STRING_STYLE : ST_COMMENT_STYLE))
	    string_lossage = 1;
	}

      /* Record comment-starters according to that
	 quote-parity to the comment-end.  */
      if (code == Scomment && styles_match_p)
	{
	  comstart_parity = parity;
	  comstart_pos = from;
	}

      /* If we find another earlier comment-ender,
	 any comment-starts earlier than that don't count
	 (because they go with the earlier comment-ender).  */
      if (code == Sendcomment && styles_match_p)
	break;

      /* Assume a defun-start point is outside of strings.  */
      if (code == Sopen
	  && (from == stop || BUF_FETCH_CHAR (buf, from - 1) == '\n'))
	break;
    }

  if (comstart_pos == 0)
    from = comment_end;
  /* If the earliest comment starter
     is followed by uniform paired string quotes or none,
     we know it can't be inside a string
     since if it were then the comment ender would be inside one.
     So it does start a comment.  Skip back to it.  */
  else if (comstart_parity == 0 && !string_lossage)
    from = comstart_pos;
  else
    {
      /* We had two kinds of string delimiters mixed up
	 together.  Decode this going forwards.
	 Scan fwd from the previous comment ender
	 to the one in question; this records where we
	 last passed a comment starter.  */

      struct lisp_parse_state state;
      scan_sexps_forward (buf, &state, find_defun_start (buf, comment_end),
			  comment_end - 1, -10000, 0, Qnil, 0);
      if (state.incomment)
	from = state.comstr_start;
      else
	/* We can't grok this as a comment; scan it normally.  */
	from = comment_end;
      UPDATE_SYNTAX_CACHE_FORWARD (scache, from - 1);
    }
  return from;
}

static Charbpos
find_end_of_comment (struct buffer *buf, Charbpos from, Charbpos stop,
		     int comstyle)
{
  int c;
  int prev_code;
  /* mask to match comment styles against; for ST_COMMENT_STYLE, this
     will get set to SYNTAX_COMMENT_STYLE_B, but never get checked */
  int mask = comstyle ? SYNTAX_COMMENT_STYLE_B : SYNTAX_COMMENT_STYLE_A;
  struct syntax_cache *scache = buf->syntax_cache;

  /* This is only called by functions which have already set up the
     syntax_cache and are keeping it up-to-date */
  while (1)
    {
      if (from == stop)
	{
	  return -1;
	}

      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
      c = BUF_FETCH_CHAR (buf, from);

      /* Test for generic comments */
      if (comstyle == ST_COMMENT_STYLE)
 	{
	  if (SYNTAX_FROM_CACHE (scache, c) == Scomment_fence)
	    {
	      from++;
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	      break;
	    }
	  from++;
	  continue; /* No need to test other comment styles in a
                       generic comment */
	}
      else

	if (SYNTAX_FROM_CACHE (scache, c) == Sendcomment
	    && SYNTAX_CODE_MATCHES_1CHAR_P
	    (SYNTAX_CODE_FROM_CACHE (scache, c), mask))
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section */
	  {
	    from++;
	    UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	    break;
	  }

      prev_code = SYNTAX_CODE_FROM_CACHE (scache, c);
      from++;
      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
      if (from < stop
	  && SYNTAX_CODES_MATCH_END_P
	  (prev_code,
	   SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from)),
	   mask)

	  )
	/* we have encountered a comment end of the same style
	   as the comment sequence which began this comment
	   section */
	{
	  from++;
	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  break;
	}
    }
  return from;
}


/* #### between FSF 19.23 and 19.28 there are some changes to the logic
   in this function (and minor changes to find_start_of_comment(),
   above, which is part of Fforward_comment() in FSF).  Attempts to port
   that logic made this function break, so I'm leaving it out.  If anyone
   ever complains about this function not working properly, take a look
   at those changes.  --ben */

DEFUN ("forward-comment", Fforward_comment, 0, 2, 0, /*
Move forward across up to COUNT comments, or backwards if COUNT is negative.
Stop scanning if we find something other than a comment or whitespace.
Set point to where scanning stops.
If COUNT comments are found as expected, with nothing except whitespace
between them, return t; otherwise return nil.
Point is set in either case.
COUNT defaults to 1, and BUFFER defaults to the current buffer.
*/
       (count, buffer))
{
  Charbpos from;
  Charbpos stop;
  Ichar c;
  enum syntaxcode code;
  int syncode;
  EMACS_INT n;
  struct buffer *buf = decode_buffer (buffer, 0);
  struct syntax_cache *scache;
  
  if (NILP (count))
    n = 1;
  else
    {
      CHECK_INT (count);
      n = XINT (count);
    }

  from = BUF_PT (buf);

  scache = setup_buffer_syntax_cache (buf, from, n);
  while (n > 0)
    {
      QUIT;

      stop = BUF_ZV (buf);
      while (from < stop)
	{
	  int comstyle = 0;     /* mask for finding matching comment style */

	  if (char_quoted (buf, from))
	    {
	      from++;
	      continue;
	    }

	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Scomment)
	    {
	      /* we have encountered a single character comment start
		 sequence, and we are ignoring all text inside comments.
		 we must record the comment style this character begins
		 so that later, only a comment end of the same style actually
		 ends the comment section */
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (code == Scomment_fence)
	    {
	      from++;
	      code = Scomment;
	      comstyle = ST_COMMENT_STYLE;
 	    }

	  else if (from < stop
		   && SYNTAX_CODE_START_FIRST_P (syncode))
	    {
	      int next_syncode;
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from + 1);
	      next_syncode =
		SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from + 1));

	      if (SYNTAX_CODES_START_P (syncode, next_syncode))
		{
		  /* we have encountered a 2char comment start sequence and we
		     are ignoring all text inside comments. we must record
		     the comment style this sequence begins so that later,
		     only a comment end of the same style actually ends
		     the comment section */
		  code = Scomment;
		  comstyle =
		    SYNTAX_CODES_COMMENT_MASK_START (syncode, next_syncode)
		    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from++;
		}
	    }

	  if (code == Scomment)
	    {
	      Charbpos newfrom = find_end_of_comment (buf, from, stop,
						      comstyle);
	      if (newfrom < 0)
		{
		  /* we stopped because from==stop */
		  BUF_SET_PT (buf, stop);
		  return Qnil;
		}
	      from = newfrom;

	      /* We have skipped one comment.  */
	      break;
	    }
	  else if (code != Swhitespace
		   && code != Sendcomment
		   && code != Scomment )
	    {
	      BUF_SET_PT (buf, from);
	      return Qnil;
	    }
	  from++;
	}

      /* End of comment reached */
      n--;
    }

  while (n < 0)
    {
      QUIT;

      stop = BUF_BEGV (buf);
      while (from > stop)
	{
          int comstyle = 0;     /* mask for finding matching comment style */

	  from--;
	  if (char_quoted (buf, from))
	    {
	      from--;
	      continue;
	    }

	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Sendcomment)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (code == Scomment_fence)
	    {
	      code = Sendcomment;
	      comstyle = ST_COMMENT_STYLE;
	    }

	  else if (from > stop
		   && SYNTAX_CODE_END_SECOND_P (syncode))
	    {
	      int prev_syncode;
	      UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
	      prev_syncode =
		SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from - 1));
	      if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		{
		  /* We must record the comment style encountered so that
		     later, we can match only the proper comment begin
		     sequence of the same style.  */
		  code = Sendcomment;
		  comstyle = SYNTAX_CODES_COMMENT_MASK_END
		    (prev_syncode, syncode) == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from--;
		}
	    }

	  if (code == Sendcomment)
 	    {
 	      from = find_start_of_comment (buf, from, stop, comstyle);
 	      break;
            }

	  else if (code != Swhitespace
		   && code != Scomment
		   && code != Sendcomment)
	    {
	      BUF_SET_PT (buf, from + 1);
	      return Qnil;
	    }
	}

      n++;
    }

  BUF_SET_PT (buf, from);
  return Qt;
}


Lisp_Object
scan_lists (struct buffer *buf, Charbpos from, int count, int depth,
	    int sexpflag, int noerror)
{
  Charbpos stop;
  Ichar c;
  int quoted;
  int mathexit = 0;
  enum syntaxcode code;
  int syncode;
  int min_depth = depth;    /* Err out if depth gets less than this. */
  struct syntax_cache *scache;
  
  if (depth > 0) min_depth = 0;

  scache = setup_buffer_syntax_cache (buf, from, count);
  while (count > 0)
    {
      QUIT;

      stop = BUF_ZV (buf);
      while (from < stop)
	{
          int comstyle = 0;     /* mask for finding matching comment style */

	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
	  code = SYNTAX_FROM_CODE (syncode);
	  from++;

	  /* a 1-char comment start sequence */
	  if (code == Scomment && parse_sexp_ignore_comments)
	    {
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode) ==
		SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  /* else, a 2-char comment start sequence? */
	  else if (from < stop
		   && SYNTAX_CODE_START_FIRST_P (syncode)
		   && parse_sexp_ignore_comments)
	    {
	      int next_syncode;
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	      next_syncode =
		SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from));

	      if (SYNTAX_CODES_START_P (syncode, next_syncode))
		{
		  /* we have encountered a comment start sequence and we
		     are ignoring all text inside comments. we must record
		     the comment style this sequence begins so that later,
		     only a comment end of the same style actually ends
		     the comment section */
		  code = Scomment;
		  comstyle = SYNTAX_CODES_COMMENT_MASK_START
		    (syncode, next_syncode) == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
		  from++;
		}
	    }
	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);

	  if (SYNTAX_CODE_PREFIX (syncode))
	    continue;

	  switch (code)
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      from++;
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it. */
	      while (from < stop)
		{
		  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
		  switch (SYNTAX_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from)))
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  from++;
		}
	      goto done;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	    case Scomment:
	      if (!parse_sexp_ignore_comments)
		break;
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	      {
		Charbpos newfrom =
		  find_end_of_comment (buf, from, stop, comstyle);
		if (newfrom < 0)
		  {
		    /* we stopped because from == stop in search forward */
		    from = stop;
		    if (depth == 0)
		      goto done;
		    goto lose;
		  }
		from = newfrom;
	      }
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == BUF_FETCH_CHAR (buf, from))
		from++;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	    if (!--depth) goto done;
	    if (depth < min_depth)
	      {
		if (noerror)
		  return Qnil;
		syntax_error ("Containing expression ends prematurely",
			      Qunbound);
	      }
	    break;

	    case Sstring_fence:
	    case Sstring:
              {
		Ichar stringterm;

		if (code != Sstring_fence)
		  {
		    /* XEmacs change: call syntax_match on character */
		    Ichar ch = BUF_FETCH_CHAR (buf, from - 1);
		    Lisp_Object stermobj =
		      syntax_match (scache->syntax_table, ch);

		if (CHARP (stermobj))
		  stringterm = XCHAR (stermobj);
		else
		  stringterm = ch;
		  }
		else
		  stringterm = '\0'; /* avoid compiler warnings */

                while (1)
		  {
		    if (from >= stop)
		      goto lose;
		    UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
		    c = BUF_FETCH_CHAR (buf, from);
		    if (code == Sstring
			? c == stringterm
			: SYNTAX_FROM_CACHE (scache, c) == Sstring_fence)
		      break;

		    switch (SYNTAX_FROM_CACHE (scache, c))
		      {
		      case Scharquote:
		      case Sescape:
			from++;
			break;
		      default:
			break;
		      }
		    from++;
		  }
                from++;
                if (!depth && sexpflag) goto done;
                break;
              }

            default:
              break;
	    }
	}

      /* Reached end of buffer.  Error if within object,
	 return nil if between */
      if (depth) goto lose;

      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      QUIT;

      stop = BUF_BEGV (buf);
      while (from > stop)
	{
          int comstyle = 0;     /* mask for finding matching comment style */

	  from--;
	  UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);
          quoted = char_quoted (buf, from);
	  if (quoted)
	    {
	    from--;
	      UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);
	    }

	  c = BUF_FETCH_CHAR (buf, from);
	  syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
	  code = SYNTAX_FROM_CODE (syncode);

	  if (code == Sendcomment && parse_sexp_ignore_comments)
	    {
	      /* we have found a single char end comment. we must record
		 the comment style encountered so that later, we can match
		 only the proper comment begin sequence of the same style */
	      comstyle = SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
		== SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	    }

	  else if (from > stop
		   && SYNTAX_CODE_END_SECOND_P (syncode)
		   && !char_quoted (buf, from - 1)
		   && parse_sexp_ignore_comments)
	    {
	      int prev_syncode;
	      UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
	      prev_syncode =
		SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from - 1));

	      if (SYNTAX_CODES_END_P (prev_syncode, syncode))
		{
	      /* we must record the comment style encountered so that
		 later, we can match only the proper comment begin
		 sequence of the same style */
	      code = Sendcomment;
		  comstyle = SYNTAX_CODES_COMMENT_MASK_END
		    (prev_syncode, syncode) == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	      from--;
	    }
	    }

	  if (SYNTAX_CODE_PREFIX (syncode))
	    continue;

	  switch (quoted ? Sword : code)
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished after
		 passing it. */
	      while (from > stop)
		{
		  UPDATE_SYNTAX_CACHE_BACKWARD (scache, from);
		  quoted = char_quoted (buf, from - 1);

		  if (quoted)
		    from--;
		  if (! (quoted
                         || (syncode =
			     SYNTAX_FROM_CACHE (scache, BUF_FETCH_CHAR (buf,
								from - 1)))
			 == Sword
			 || syncode == Ssymbol
			 || syncode == Squote))
            	    goto done2;
		  from--;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == BUF_FETCH_CHAR (buf, from - 1))
		from--;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	    if (!--depth) goto done2;
	    if (depth < min_depth)
	      {
		if (noerror)
		  return Qnil;
		syntax_error ("Containing expression ends prematurely",
			      Qunbound);
	      }
	    break;

	    case Scomment_fence:
	      comstyle = ST_COMMENT_STYLE;
	    case Sendcomment:
	      if (parse_sexp_ignore_comments)
		from = find_start_of_comment (buf, from, stop, comstyle);
	      break;

	    case Sstring_fence:
	    case Sstring:
              {
		Ichar stringterm;

		if (code != Sstring_fence)
		  {
		/* XEmacs change: call syntax_match() on character */
                Ichar ch = BUF_FETCH_CHAR (buf, from);
		    Lisp_Object stermobj =
		      syntax_match (scache->syntax_table, ch);

		if (CHARP (stermobj))
		  stringterm = XCHAR (stermobj);
		else
		  stringterm = ch;
		  }
		else
		  stringterm = '\0'; /* avoid compiler warnings */

                while (1)
		  {
		    if (from == stop) goto lose;

		    UPDATE_SYNTAX_CACHE_BACKWARD (scache, from - 1);
		    c = BUF_FETCH_CHAR (buf, from - 1);

		    if ((code == Sstring
			? c == stringterm
			 : SYNTAX_FROM_CACHE (scache, c) == Sstring_fence)
			&& !char_quoted (buf, from - 1))
		      {
		      break;
		      }

		    from--;
		  }
                from--;
                if (!depth && sexpflag) goto done2;
                break;
              }
            }
	}

      /* Reached start of buffer.  Error if within object,
	 return nil if between */
      if (depth) goto lose;

      return Qnil;

    done2:
      count++;
    }


  return (make_int (from));

lose:
  if (!noerror)
    syntax_error ("Unbalanced parentheses", Qunbound);
  return Qnil;
}

int
char_quoted (struct buffer *buf, Charbpos pos)
{
  enum syntaxcode code;
  Charbpos beg = BUF_BEGV (buf);
  int quoted = 0;
  Charbpos startpos = pos;
  struct syntax_cache *scache = buf->syntax_cache;

  while (pos > beg)
    {
      UPDATE_SYNTAX_CACHE_BACKWARD (scache, pos - 1);
      code = SYNTAX_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, pos - 1));

      if (code != Scharquote && code != Sescape)
	break;
      pos--;
      quoted = !quoted;
    }

  UPDATE_SYNTAX_CACHE (scache, startpos);
  return quoted;
}

DEFUN ("scan-lists", Fscan_lists, 3, 5, 0, /*
Scan from character number FROM by COUNT lists.
Returns the character number of the position thus found.

If DEPTH is nonzero, paren depth begins counting from that value,
only places where the depth in parentheses becomes zero
are candidates for stopping; COUNT such places are counted.
Thus, a positive value for DEPTH means go out levels.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
and the depth is wrong, an error is signaled.
If the depth is right but the count is not used up, nil is returned.

If optional arg BUFFER is non-nil, scanning occurs in that buffer instead
of in the current buffer.

If optional arg NOERROR is non-nil, scan-lists will return nil instead of
signalling an error.
*/
       (from, count, depth, buffer, noerror))
{
  struct buffer *buf;

  CHECK_INT (from);
  CHECK_INT (count);
  CHECK_INT (depth);
  buf = decode_buffer (buffer, 0);

  return scan_lists (buf, XINT (from), XINT (count), XINT (depth), 0,
		     !NILP (noerror));
}

DEFUN ("scan-sexps", Fscan_sexps, 2, 4, 0, /*
Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned.

If optional arg BUFFER is non-nil, scanning occurs in that buffer instead
of in the current buffer.

If optional arg NOERROR is non-nil, scan-sexps will return nil instead of
signalling an error.
*/
       (from, count, buffer, noerror))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  CHECK_INT (from);
  CHECK_INT (count);

  return scan_lists (buf, XINT (from), XINT (count), 0, 1, !NILP (noerror));
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, 0, 1, 0, /*
Move point backward over any number of chars with prefix syntax.
This includes chars with "quote" or "prefix" syntax (' or p).

Optional arg BUFFER defaults to the current buffer.
*/
       (buffer))
{
  struct buffer *buf = decode_buffer (buffer, 0);
  Charbpos beg = BUF_BEGV (buf);
  Charbpos pos = BUF_PT (buf);
  Ichar c = '\0'; /* initialize to avoid compiler warnings */
  struct syntax_cache *scache;
  
  scache = setup_buffer_syntax_cache (buf, pos, -1);

  while (pos > beg && !char_quoted (buf, pos - 1)
	 /* Previous statement updates syntax table.  */
	 && (SYNTAX_FROM_CACHE (scache, c = BUF_FETCH_CHAR (buf, pos - 1)) == Squote
	     || SYNTAX_CODE_PREFIX (SYNTAX_CODE_FROM_CACHE (scache, c))))
    pos--;

  BUF_SET_PT (buf, pos);

  return Qnil;
}

/* Parse forward from FROM to END,
   assuming that FROM has state OLDSTATE (nil means FROM is start of function),
   and return a description of the state of the parse at END.
   If STOPBEFORE is nonzero, stop at the start of an atom.
   If COMMENTSTOP is nonzero, stop at the start of a comment.  */

static void
scan_sexps_forward (struct buffer *buf, struct lisp_parse_state *stateptr,
		    Charbpos from, Charbpos end,
		    int targetdepth, int stopbefore,
		    Lisp_Object oldstate,
		    int commentstop)
{
  struct lisp_parse_state state;

  enum syntaxcode code;
  struct level { int last, prev; };
  struct level levelstart[100];
  struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  int boundary_stop = commentstop == -1;
  Lisp_Object tem;
  struct syntax_cache *scache;
  
  scache = setup_buffer_syntax_cache (buf, from, 1);
  if (NILP (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
      state.comstyle = 0;	/* comment style a by default */
      state.comstr_start = -1;	/* no comment/string seen.  */
    }
  else
    {
      tem = Fcar (oldstate);    /* elt 0, depth */
      if (!NILP (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 3, instring */
      state.instring = ( !NILP (tem) 
			 ? ( INTP (tem) ? XINT (tem) : ST_STRING_STYLE) 
			 : -1);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 4, incomment */
      state.incomment = !NILP (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 5, follows-quote */
      start_quoted = !NILP (tem);

      /* if the eighth element of the list is nil, we are in comment style
	 a; if it is t, we are in comment style b; if it is 'syntax-table,
	 we are in a generic comment */
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 7, comment style a/b/fence */
      state.comstyle = NILP (tem) ? 0 : ( EQ (tem, Qsyntax_table)
					  ? ST_COMMENT_STYLE : 1 );

      oldstate = Fcdr (oldstate); /* elt 8, start of last comment/string */
      tem = Fcar (oldstate);
      state.comstr_start = NILP (tem) ? -1 : XINT (tem);

      /* elt 9, char numbers of starts-of-expression of levels
         (starting from outermost). */
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);    /* elt 9, intermediate data for
				   continuation of parsing (subject
				   to change). */
      while (!NILP (tem))	/* >= second enclosing sexps.  */
	{
	  curlevel->last = XINT (Fcar (tem));
	  if (++curlevel == endlevel)
	    stack_overflow ("Nesting too deep for parser",
			    make_int (curlevel - levelstart));
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  tem = Fcdr (tem);
	}
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state. */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      Ichar c;
      int syncode;

      QUIT;

      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
      c = BUF_FETCH_CHAR (buf, from);
      syncode = SYNTAX_CODE_FROM_CACHE (scache, c);
      code = SYNTAX_FROM_CODE (syncode);
      from++;

	  /* record the comment style we have entered so that only the
	     comment-ender sequence (or single char) of the same style
	     actually terminates the comment section. */
      if (code == Scomment)
	{
	  state.comstyle =
	    SYNTAX_CODE_COMMENT_1CHAR_MASK (syncode)
	    == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	  state.comstr_start = from - 1;
	}

      /* a generic comment delimiter? */
      else if (code == Scomment_fence)
	{
	  state.comstyle = ST_COMMENT_STYLE;
	  state.comstr_start = from - 1;
	  code = Scomment;
	}

      else if (from < end &&
	       SYNTAX_CODE_START_FIRST_P (syncode))
	{
	  int next_syncode;
	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  next_syncode =
	    SYNTAX_CODE_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from));

	  if (SYNTAX_CODES_START_P (syncode, next_syncode))
	{
	  code = Scomment;
	      state.comstyle = SYNTAX_CODES_COMMENT_MASK_START
		(syncode, next_syncode) == SYNTAX_COMMENT_STYLE_A ? 0 : 1;
	      state.comstr_start = from - 1;
	  from++;
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	    }
	}

      if (SYNTAX_CODE_PREFIX (syncode))
	continue;
      switch (code)
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	startquoted:
	  if (from == end) goto endquoted;
	  from++;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	symstarted:
	  while (from < end)
	    {
	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	      switch (SYNTAX_FROM_CACHE (scache, BUF_FETCH_CHAR (buf, from)))
		{
		case Scharquote:
		case Sescape:
		  from++;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      from++;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment:
	  state.incomment = 1;
	  if (commentstop || boundary_stop) goto done;
	startincomment:
	  if (commentstop == 1)
	    goto done;
	  UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	  {
	    Charbpos newfrom = find_end_of_comment (buf, from, end,
						    state.comstyle);
	    if (newfrom < 0)
	      {
		/* we terminated search because from == end */
		from = end;
		goto done;
	      }
	    from = newfrom;
	  }
	  state.incomment = 0;
	  state.comstyle = 0;		     /* reset the comment style */
	  if (boundary_stop) goto done;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = from - 1;
	  if (++curlevel == endlevel)
	    stack_overflow ("Nesting too deep for parser",
			    make_int (curlevel - levelstart));
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (targetdepth == depth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (targetdepth == depth) goto done;
	  break;

	case Sstring:
	case Sstring_fence:
	  state.comstr_start = from - 1;
            if (stopbefore) goto stop; /* this arg means stop at sexp start */
            curlevel->last = from - 1;
	  if (code == Sstring_fence)
	    {
	      state.instring = ST_STRING_STYLE;
	    }
	  else
	    {
	      /* XEmacs change: call syntax_match() on character */
	      Ichar ch = BUF_FETCH_CHAR (buf, from - 1);
	      Lisp_Object stermobj =
		syntax_match (scache->syntax_table, ch);

	      if (CHARP (stermobj))
		state.instring = XCHAR (stermobj);
	      else
		state.instring = ch;
          }
	  if (boundary_stop) goto done;
	startinstring:
	  while (1)
	    {
	      enum syntaxcode temp_code;

	      if (from >= end) goto done;

	      UPDATE_SYNTAX_CACHE_FORWARD (scache, from);
	      c = BUF_FETCH_CHAR (buf, from);
	      temp_code = SYNTAX_FROM_CACHE (scache, c);

	      if (
		  state.instring != ST_STRING_STYLE &&
		  temp_code == Sstring &&
		  c == state.instring) break;

	      switch (temp_code)
		{
		case Sstring_fence:
		  if (state.instring == ST_STRING_STYLE)
		    goto string_end;
		  break;
		case Scharquote:
		case Sescape:
                  {
                    from++;
                  startquotedinstring:
                    if (from >= end) goto endquoted;
                    break;
                  }
                default:
                  break;
		}
	      from++;
	    }
	string_end:
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  from++;
	  if (boundary_stop) goto done;
	  break;

	case Smath:
	  break;

        case Swhitespace:
        case Spunct:
        case Squote:
        case Sendcomment:
	case Scomment_fence:
	case Sinherit:
        case Smax:
          break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from--;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  state.levelstarts = Qnil;
  while (--curlevel >= levelstart)
    state.levelstarts = Fcons (make_int (curlevel->last),
			       state.levelstarts);

  *stateptr = state;
}

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, 2, 7, 0, /*
Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.
Value is a list of nine elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (It is the character that will terminate the string,
     or t if the string should be terminated by an explicit
     `syntax-table' property.)
 4. t if inside a comment.
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. nil if in comment style a, or not in a comment; t if in comment style b;
    `syntax-table' if given by an explicit `syntax-table' property.
 8. character address of start of last comment or string; nil if none.
 9. Intermediate data for continuation of parsing (subject to change).
If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a nine-element list like what this function returns.
It is used to initialize the state of the parse.  Its second and third
elements are ignored.
Sixth arg COMMENTSTOP non-nil means stop at the start of a comment. If it
is `syntax-table', stop after the start of a comment or a string, or after
the end of a comment or string.
Seventh arg BUFFER specifies the buffer to do the parsing in, and defaults
to the current buffer.
*/
       (from, to, targetdepth, stopbefore, oldstate, commentstop, buffer))
{
  struct lisp_parse_state state;
  int target;
  Charbpos start, end;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object val;

  if (!NILP (targetdepth))
    {
      CHECK_INT (targetdepth);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  get_buffer_range_char (buf, from, to, &start, &end, 0);
  scan_sexps_forward (buf, &state, start, end,
		      target, !NILP (stopbefore), oldstate,
		      (NILP (commentstop)
		       ? 0 : (EQ (commentstop, Qsyntax_table) ? -1 : 1)));
  BUF_SET_PT (buf, state.location);

  /* reverse order */
  val = Qnil;
  val = Fcons (state.levelstarts, val);
  val = Fcons ((state.incomment || (state.instring >= 0))
	       ? make_int (state.comstr_start) : Qnil, val);
  val = Fcons (state.comstyle  ? (state.comstyle == ST_COMMENT_STYLE
				  ? Qsyntax_table : Qt) : Qnil, val);
  val = Fcons (make_int (state.mindepth),   val);
  val = Fcons (state.quoted    ? Qt : Qnil, val);
  val = Fcons (state.incomment ? Qt : Qnil, val);
  val = Fcons (state.instring < 0
	       ? Qnil
	       : (state.instring == ST_STRING_STYLE
		  ? Qt : make_int (state.instring)), val);
  val = Fcons (state.thislevelstart < 0 ? Qnil :
	       make_int (state.thislevelstart), val);
  val = Fcons (state.prevlevelstart < 0 ? Qnil :
	       make_int (state.prevlevelstart), val);
  val = Fcons (make_int (state.depth), val);

  return val;
}


/* Updating of the mirror syntax table.

   Each syntax table has a corresponding mirror table in it.  Whenever we
   make a change to a syntax table, we set a dirty flag.  When accessing a
   value from the mirror table and the table is dirty, we call
   update_syntax_table() to clean it up.

   #### We really only need to map over the changed range.

   If we change the standard syntax table, we need to map over
   all tables because any of them could be inheriting from the
   standard syntax table.

   When `set-syntax-table' is called, we set the buffer's mirror
   syntax table as well.
   */

static int
copy_to_mirrortab (struct chartab_range *range, Lisp_Object UNUSED (table),
		   Lisp_Object val, void *arg)
{
  Lisp_Object mirrortab = VOID_TO_LISP (arg);

  if (CONSP (val))
    val = XCAR (val);
  if (SYNTAX_FROM_CODE (XINT (val)) != Sinherit)
    put_char_table (mirrortab, range, val);
  return 0;
}

static int
copy_if_not_already_present (struct chartab_range *range,
			     Lisp_Object UNUSED (table),
			     Lisp_Object val, void *arg)
{
  Lisp_Object mirrortab = VOID_TO_LISP (arg);
  if (CONSP (val))
    val = XCAR (val);
  if (SYNTAX_FROM_CODE (XINT (val)) != Sinherit)
    {
      Lisp_Object existing =
	updating_mirror_get_range_char_table (range, mirrortab,
					      Vbogus_syntax_table_value);
      if (NILP (existing))
	/* nothing at all */
	put_char_table (mirrortab, range, val);
      else if (!EQ (existing, Vbogus_syntax_table_value))
	/* full */
	;
      else
	{
	  Freset_char_table (Vtemp_table_for_use_updating_syntax_tables);
	  copy_char_table_range
	    (mirrortab, Vtemp_table_for_use_updating_syntax_tables, range);
	  put_char_table (mirrortab, range, val);
	  copy_char_table_range
	    (Vtemp_table_for_use_updating_syntax_tables, mirrortab, range);
	}
    }

  return 0;
}

static void
update_just_this_syntax_table (Lisp_Object table)
{
  struct chartab_range range;
  Lisp_Object mirrortab = XCHAR_TABLE (table)->mirror_table;

  assert (!XCHAR_TABLE (table)->mirror_table_p);
  range.type = CHARTAB_RANGE_ALL;
  Freset_char_table (mirrortab);

  /* First, copy the tables values other than inherit into the mirror
     table.  Then, for tables other than the standard syntax table, map
     over the standard table, copying values into the mirror table only if
     entries don't already exist in that table. (The copying step requires
     another mapping.)
     */

  map_char_table (table, &range, copy_to_mirrortab, LISP_TO_VOID (mirrortab));
  /* second clause catches bootstrapping problems when initializing the
     standard syntax table */
  if (!EQ (table, Vstandard_syntax_table) && !NILP (Vstandard_syntax_table))
    map_char_table (Vstandard_syntax_table, &range,
		    copy_if_not_already_present, LISP_TO_VOID (mirrortab));
  /* The resetting made the default be Qnil.  Put it back to Spunct. */
  set_char_table_default (mirrortab, make_int (Spunct));
  XCHAR_TABLE (mirrortab)->dirty = 0;
}

/* Called from chartab.c when a change is made to a syntax table.
   If this is the standard syntax table, we need to recompute
   *all* syntax tables (yuck).  Otherwise we just recompute this
   one. */

void
update_syntax_table (Lisp_Object table)
{
  Lisp_Object nonmirror = XCHAR_TABLE (table)->mirror_table;
  assert (XCHAR_TABLE (table)->mirror_table_p);
  if (EQ (nonmirror, Vstandard_syntax_table))
    {
      Lisp_Object syntab;

      for (syntab = Vall_syntax_tables; !NILP (syntab);
	   syntab = XCHAR_TABLE (syntab)->next_table)
	update_just_this_syntax_table (syntab);
    }
  else
    update_just_this_syntax_table (nonmirror);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_syntax (void)
{
  DEFSYMBOL (Qsyntax_table_p);
  DEFSYMBOL (Qsyntax_table);

  DEFSUBR (Fsyntax_table_p);
  DEFSUBR (Fsyntax_table);
#ifdef DEBUG_XEMACS
  DEFSUBR (Fmirror_syntax_table);
  DEFSUBR (Fsyntax_cache_info);
#endif /* DEBUG_XEMACS */
  DEFSUBR (Fstandard_syntax_table);
  DEFSUBR (Fcopy_syntax_table);
  DEFSUBR (Fset_syntax_table);
  DEFSUBR (Fsyntax_designator_chars);
  DEFSUBR (Fchar_syntax);
  DEFSUBR (Fmatching_paren);
  /* DEFSUBR (Fmodify_syntax_entry); now in Lisp. */
  /* DEFSUBR (Fdescribe_syntax); now in Lisp. */

  DEFSUBR (Fforward_word);

  DEFSUBR (Fforward_comment);
  DEFSUBR (Fscan_lists);
  DEFSUBR (Fscan_sexps);
  DEFSUBR (Fbackward_prefix_chars);
  DEFSUBR (Fparse_partial_sexp);
}

void
vars_of_syntax (void)
{
  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments /*
Non-nil means `forward-sexp', etc., should treat comments as whitespace.
*/ );
  parse_sexp_ignore_comments = 0;

  DEFVAR_BOOL ("lookup-syntax-properties", &lookup_syntax_properties /*
Non-nil means `forward-sexp', etc., respect the `syntax-table' property.
This property can be placed on buffers or strings and can be used to explicitly
specify the syntax table to be used for looking up the syntax of the chars
having this property, or to directly specify the syntax of the chars.

The value of this property should be either a syntax table, or a cons
of the form (SYNTAXCODE . MATCHCHAR), SYNTAXCODE being the numeric
syntax code, MATCHCHAR being nil or the character to match (which is
relevant only when the syntax code is open/close-type).
*/ );
  lookup_syntax_properties = 1;

  DEFVAR_BOOL ("words-include-escapes", &words_include_escapes /*
Non-nil means `forward-word', etc., should treat escape chars part of words.
*/ );
  words_include_escapes = 0;

  no_quit_in_re_search = 0;

  Vbogus_syntax_table_value = make_float (0.0);
  staticpro (&Vbogus_syntax_table_value);
}

static void
define_standard_syntax (const char *p, enum syntaxcode syn)
{
  for (; *p; p++)
    Fput_char_table (make_char (*p), make_int (syn), Vstandard_syntax_table);
}

void
complex_vars_of_syntax (void)
{
  Ichar i;
  const char *p;
  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-syntax-table
     so that copy-syntax-table will know not to try to copy from garbage */
  Vstandard_syntax_table = Qnil;
  Vstandard_syntax_table = Fcopy_syntax_table (Qnil);
  staticpro (&Vstandard_syntax_table);

  Vtemp_table_for_use_updating_syntax_tables = Fmake_char_table (Qgeneric);
  staticpro (&Vtemp_table_for_use_updating_syntax_tables);
  
  Vsyntax_designator_chars_string = make_string_nocopy (syntax_code_spec,
							Smax);
  staticpro (&Vsyntax_designator_chars_string);

  set_char_table_default (Vstandard_syntax_table, make_int (Spunct));

  for (i = 0; i <= 32; i++)	/* Control 0 plus SPACE */
    Fput_char_table (make_char (i), make_int (Swhitespace),
		     Vstandard_syntax_table);
  for (i = 127; i <= 159; i++)	/* DEL plus Control 1 */
    Fput_char_table (make_char (i), make_int (Swhitespace),
		     Vstandard_syntax_table);

  define_standard_syntax ("abcdefghijklmnopqrstuvwxyz"
			  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			  "0123456789"
			  "$%", Sword);
  define_standard_syntax ("\"", Sstring);
  define_standard_syntax ("\\", Sescape);
  define_standard_syntax ("_-+*/&|<>=", Ssymbol);
  define_standard_syntax (".,;:?!#@~^'`", Spunct);

  for (p = "()[]{}"; *p; p+=2)
    {
      Fput_char_table (make_char (p[0]),
		       Fcons (make_int (Sopen), make_char (p[1])),
		       Vstandard_syntax_table);
      Fput_char_table (make_char (p[1]),
		       Fcons (make_int (Sclose), make_char (p[0])),
		       Vstandard_syntax_table);
    }
}
