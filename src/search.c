/* String search routines for XEmacs.
   Copyright (C) 1985, 1986, 1987, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: FSF 19.29, except for region-cache stuff. */

/* Hacked on for Mule by Ben Wing, December 1994 and August 1995. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "insdel.h"
#include "opaque.h"
#ifdef REGION_CACHE_NEEDS_WORK
#include "region-cache.h"
#endif
#include "syntax.h"

#include <sys/types.h>
#include "regex.h"
#include "casetab.h"
#include "chartab.h"

#define TRANSLATE(table, pos)	\
 (!NILP (table) ? TRT_TABLE_OF (table, (Ichar) pos) : pos)

#define REGEXP_CACHE_SIZE 20

/* If the regexp is non-nil, then the buffer contains the compiled form
   of that regexp, suitable for searching.  */
struct regexp_cache
{
  struct regexp_cache *next;
  Lisp_Object regexp;
  struct re_pattern_buffer buf;
  char fastmap[0400];
  /* Nonzero means regexp was compiled to do full POSIX backtracking.  */
  char posix;
};

/* The instances of that struct.  */
static struct regexp_cache searchbufs[REGEXP_CACHE_SIZE];

/* The head of the linked list; points to the most recently used buffer.  */
static struct regexp_cache *searchbuf_head;


/* Every call to re_match, etc., must pass &search_regs as the regs
   argument unless you can show it is unnecessary (i.e., if re_match
   is certainly going to be called again before region-around-match
   can be called).

   Since the registers are now dynamically allocated, we need to make
   sure not to refer to the Nth register before checking that it has
   been allocated by checking search_regs.num_regs.

   The regex code keeps track of whether it has allocated the search
   buffer using bits in the re_pattern_buffer.  This means that whenever
   you compile a new pattern, it completely forgets whether it has
   allocated any registers, and will allocate new registers the next
   time you call a searching or matching function.  Therefore, we need
   to call re_set_registers after compiling a new pattern or after
   setting the match registers, so that the regex functions will be
   able to free or re-allocate it properly.  */

/* Note: things get trickier under Mule because the values returned from
   the regexp routines are in Bytebpos's but we need them to be in Charbpos's.
   We take the easy way out for the moment and just convert them immediately.
   We could be more clever by not converting them until necessary, but
   that gets real ugly real fast since the buffer might have changed and
   the positions might be out of sync or out of range.
   */
static struct re_registers search_regs;

/* Every function that sets the match data _must_ clear unused search
   registers on success.  An unsuccessful search or match _must_ preserve
   the search registers.  The traditional documentation implied that
   any match operation might trash the registers, but in fact failures
   have always preserved the match data (in GNU Emacs as well).  Some
   plausible code depends on this behavior (cf. `w3-configuration-data'
   in library "w3-cfg").

   Ordinary string searchs use set_search_regs to set the whole-string
   match.  That function takes care of clearing the unused subexpression
   registers.
   */
static void set_search_regs (struct buffer *buf, Charbpos beg, Charcount len);
static void clear_search_regs (void);

/* The buffer in which the last search was performed, or
   Qt if the last search was done in a string;
   Qnil if no searching has been done yet.  */
static Lisp_Object last_thing_searched;

/* error condition signalled when regexp compile_pattern fails */

Lisp_Object Qinvalid_regexp;

/* Regular expressions used in forward/backward-word */
Lisp_Object Vforward_word_regexp, Vbackward_word_regexp;

Fixnum warn_about_possibly_incompatible_back_references;

/* range table for use with skip_chars.  Only needed for Mule. */
Lisp_Object Vskip_chars_range_table;

static Charbpos simple_search (struct buffer *buf, Ibyte *base_pat,
			       Bytecount len, Bytebpos pos, Bytebpos lim,
			       EMACS_INT n, Lisp_Object trt);
static Charbpos boyer_moore (struct buffer *buf, Ibyte *base_pat,
			     Bytecount len, Bytebpos pos, Bytebpos lim,
			     EMACS_INT n, Lisp_Object trt,
			     Lisp_Object inverse_trt, int charset_base);
static Charbpos search_buffer (struct buffer *buf, Lisp_Object str,
			       Charbpos charbpos, Charbpos buflim, EMACS_INT n,
			       int RE, Lisp_Object trt,
			       Lisp_Object inverse_trt, int posix);

static DECLARE_DOESNT_RETURN (matcher_overflow (void));

static DOESNT_RETURN
matcher_overflow ()
{
  stack_overflow ("Stack overflow in regexp matcher", Qunbound);
}

/* Compile a regexp and signal a Lisp error if anything goes wrong.
   PATTERN is the pattern to compile.
   CP is the place to put the result.
   TRANSLATE is a translation table for ignoring case, or Qnil for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

static int
compile_pattern_1 (struct regexp_cache *cp, Lisp_Object pattern,
		   struct re_registers *UNUSED (regp), Lisp_Object translate,
		   int posix, Error_Behavior errb)
{
  const char *val;
  reg_syntax_t old;

  cp->regexp = Qnil;
  cp->buf.translate = translate;
  cp->posix = posix;
  old = re_set_syntax (RE_SYNTAX_EMACS
		       | (posix ? 0 : RE_NO_POSIX_BACKTRACKING));
  val = (const char *)
    re_compile_pattern ((char *) XSTRING_DATA (pattern),
			XSTRING_LENGTH (pattern), &cp->buf);
  re_set_syntax (old);
  if (val)
    {
      maybe_signal_error (Qinvalid_regexp, 0, build_string (val),
			  Qsearch, errb);
      return 0;
    }

  cp->regexp = Fcopy_sequence (pattern);
  return 1;
}

/* Compile a regexp if necessary, but first check to see if there's one in
   the cache.
   PATTERN is the pattern to compile.
   TRANSLATE is a translation table for ignoring case, or Qnil for none.
   REGP is the structure that says where to store the "register"
   values that will result from matching this pattern.
   If it is 0, we should compile the pattern not to record any
   subexpression bounds.
   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */

struct re_pattern_buffer *
compile_pattern (Lisp_Object pattern, struct re_registers *regp,
		 Lisp_Object translate, Lisp_Object UNUSED (searchobj),
		 struct buffer *UNUSED (searchbuf), int posix,
		 Error_Behavior errb)
{
  struct regexp_cache *cp, **cpp;

  for (cpp = &searchbuf_head; ; cpp = &cp->next)
    {
      cp = *cpp;
      /* &&#### once we fix up the fastmap code in regex.c for 8-bit-fixed,
         we need to record and compare the buffer and format, since the
         fastmap will reflect the state of the buffer -- and things get
         more complicated if the buffer has changed formats or (esp.) has
         kept the format but changed its interpretation!  may need to have
         the code that changes the interpretation go through and invalidate
         cache entries for that buffer. */
      if (!NILP (Fstring_equal (cp->regexp, pattern))
	  && EQ (cp->buf.translate, translate)
	  && cp->posix == posix)
	break;

      /* If we're at the end of the cache, compile into the last cell.  */
      if (cp->next == 0)
	{
	  if (!compile_pattern_1 (cp, pattern, regp, translate,
				  posix, errb))
	    return 0;
	  break;
	}
    }

  /* When we get here, cp (aka *cpp) contains the compiled pattern,
     either because we found it in the cache or because we just compiled it.
     Move it to the front of the queue to mark it as most recently used.  */
  *cpp = cp->next;
  cp->next = searchbuf_head;
  searchbuf_head = cp;

  /* Advise the searching functions about the space we have allocated
     for register data.  */
  if (regp)
    re_set_registers (&cp->buf, regp, regp->num_regs, regp->start, regp->end);

  return &cp->buf;
}

/* Error condition used for failing searches */
Lisp_Object Qsearch_failed;

static DECLARE_DOESNT_RETURN (signal_failure (Lisp_Object));

static DOESNT_RETURN
signal_failure (Lisp_Object arg)
{
  for (;;)
    Fsignal (Qsearch_failed, list1 (arg));
}

/* Convert the search registers from Bytebpos's to Charbpos's.  Needs to be
   done after each regexp match that uses the search regs.

   We could get a potential speedup by not converting the search registers
   until it's really necessary, e.g. when match-data or replace-match is
   called.  However, this complexifies the code a lot (e.g. the buffer
   could have changed and the Bytebpos's stored might be invalid) and is
   probably not a great time-saver. */

static void
fixup_search_regs_for_buffer (struct buffer *buf)
{
  int i;
  int num_regs = search_regs.num_regs;

  for (i = 0; i < num_regs; i++)
    {
      if (search_regs.start[i] >= 0)
	search_regs.start[i] = bytebpos_to_charbpos (buf,
						     search_regs.start[i]);
      if (search_regs.end[i] >= 0)
	search_regs.end[i] = bytebpos_to_charbpos (buf, search_regs.end[i]);
    }
}

/* Similar but for strings. */
static void
fixup_search_regs_for_string (Lisp_Object string)
{
  int i;
  int num_regs = search_regs.num_regs;

  /* #### bytecount_to_charcount() is not that efficient.  This function
     could be faster if it did its own conversion (using INC_IBYTEPTR()
     and such), because the register ends are likely to be somewhat ordered.
     (Even if not, you could sort them.)

     Think about this if this function is a time hog, which it's probably
     not. */
  for (i = 0; i < num_regs; i++)
    {
      if (search_regs.start[i] > 0)
	{
	  search_regs.start[i] =
	    string_index_byte_to_char (string, search_regs.start[i]);
	}
      if (search_regs.end[i] > 0)
	{
	  search_regs.end[i] =
	    string_index_byte_to_char (string, search_regs.end[i]);
	}
    }
}


static Lisp_Object
looking_at_1 (Lisp_Object string, struct buffer *buf, int posix)
{
  Lisp_Object val;
  Bytebpos p1, p2;
  Bytecount s1, s2;
  REGISTER int i;
  struct re_pattern_buffer *bufp;
  struct syntax_cache scache_struct;
  struct syntax_cache *scache = &scache_struct;
  
  CHECK_STRING (string);
  bufp = compile_pattern (string, &search_regs,
			  (!NILP (buf->case_fold_search)
			   ? XCASE_TABLE_DOWNCASE (buf->case_table) : Qnil),
			  wrap_buffer (buf), buf, posix, ERROR_ME);

  QUIT;

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BYTE_BUF_BEGV (buf);
  p2 = BYTE_BUF_CEILING_OF (buf, p1);
  s1 = p2 - p1;
  s2 = BYTE_BUF_ZV (buf) - p2;

  /* By making the regex object, regex buffer, and syntax cache arguments
     to re_{search,match}{,_2}, we've removed the need to do nasty things
     to deal with regex reentrancy. (See stack trace in signal.c for proof
     that this can happen.)

     #### there is still a potential problem with the regex cache --
     the compiled regex could be overwritten.  we'd need 20-fold
     reentrancy, though.  Fix this. */

  i = re_match_2 (bufp, (char *) BYTE_BUF_BYTE_ADDRESS (buf, p1),
		  s1, (char *) BYTE_BUF_BYTE_ADDRESS (buf, p2), s2,
		  BYTE_BUF_PT (buf) - BYTE_BUF_BEGV (buf), &search_regs,
		  BYTE_BUF_ZV (buf) - BYTE_BUF_BEGV (buf), wrap_buffer (buf),
		  buf, scache);

  if (i == -2)
    matcher_overflow ();

  val = (0 <= i ? Qt : Qnil);
  if (NILP (val))
    return Qnil;
  {
    int num_regs = search_regs.num_regs;
    for (i = 0; i < num_regs; i++)
      if (search_regs.start[i] >= 0)
	{
	  search_regs.start[i] += BYTE_BUF_BEGV (buf);
	  search_regs.end[i] += BYTE_BUF_BEGV (buf);
	}
  }
  last_thing_searched = wrap_buffer (buf);
  fixup_search_regs_for_buffer (buf);
  return val;
}

DEFUN ("looking-at", Flooking_at, 1, 2, 0, /*
Return t if text after point matches regular expression REGEXP.
When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

Optional argument BUFFER defaults to the current buffer.
*/
       (regexp, buffer))
{
  return looking_at_1 (regexp, decode_buffer (buffer, 0), 0);
}

DEFUN ("posix-looking-at", Fposix_looking_at, 1, 2, 0, /*
Return t if text after point matches regular expression REGEXP.
Find the longest match, in accord with Posix regular expression rules.
When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

Optional argument BUFFER defaults to the current buffer.
*/
       (regexp, buffer))
{
  return looking_at_1 (regexp, decode_buffer (buffer, 0), 1);
}

static Lisp_Object
string_match_1 (Lisp_Object regexp, Lisp_Object string, Lisp_Object start,
		struct buffer *buf, int UNUSED (posix))
{
  Bytecount val;
  Charcount s;
  struct re_pattern_buffer *bufp;

  /* Some FSF junk with running_asynch_code, to preserve the match
     data.  Not necessary because we don't call process filters
     asynchronously (i.e. from within QUIT). */

  CHECK_STRING (regexp);
  CHECK_STRING (string);

  if (NILP (start))
    s = 0;
  else
    {
      Charcount len = string_char_length (string);

      CHECK_INT (start);
      s = XINT (start);
      if (s < 0 && -s <= len)
	s = len + s;
      else if (0 > s || s > len)
	args_out_of_range (string, start);
    }


  bufp = compile_pattern (regexp, &search_regs,
			  (!NILP (buf->case_fold_search)
			   ? XCASE_TABLE_DOWNCASE (buf->case_table) : Qnil),
			  string, buf, 0, ERROR_ME);
  QUIT;
  {
    Bytecount bis = string_index_char_to_byte (string, s);
    struct syntax_cache scache_struct;
    struct syntax_cache *scache = &scache_struct;
  
    /* By making the regex object, regex buffer, and syntax cache arguments
       to re_{search,match}{,_2}, we've removed the need to do nasty things
       to deal with regex reentrancy. (See stack trace in signal.c for proof
       that this can happen.)
       
       #### there is still a potential problem with the regex cache --
       the compiled regex could be overwritten.  we'd need 20-fold
       reentrancy, though.  Fix this. */

    val = re_search (bufp, (char *) XSTRING_DATA (string),
		     XSTRING_LENGTH (string), bis,
		     XSTRING_LENGTH (string) - bis,
		     &search_regs, string, buf, scache);
  }
  if (val == -2)
    matcher_overflow ();
  if (val < 0) return Qnil;
  last_thing_searched = Qt;
  fixup_search_regs_for_string (string);
  return make_int (string_index_byte_to_char (string, val));
}

DEFUN ("string-match", Fstring_match, 2, 4, 0, /*
Return index of start of first match for REGEXP in STRING, or nil.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.

Optional arg BUFFER controls how case folding and syntax and category
lookup is done (according to the value of `case-fold-search' in that buffer
and that buffer's case tables, syntax tables, and category table).  If nil
or unspecified, it defaults *NOT* to the current buffer but instead:

-- the value of `case-fold-search' in the current buffer is still respected
   because of idioms like

      (let ((case-fold-search nil))
         (string-match "^foo.*bar" string))

   but the case, syntax, and category tables come from the standard tables,
   which are accessed through functions `default-{case,syntax,category}-table'
   and serve as the parents of the tables in particular buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.
*/
       (regexp, string, start, buffer))
{
/* &&#### implement new interp for buffer arg; check code to see if it
   makes more sense than prev */
  return string_match_1 (regexp, string, start, decode_buffer (buffer, 0), 0);
}

DEFUN ("posix-string-match", Fposix_string_match, 2, 4, 0, /*
Return index of start of first match for REGEXP in STRING, or nil.
Find the longest match, in accord with Posix regular expression rules.
If third arg START is non-nil, start search at that index in STRING.
For index of first char beyond the match, do (match-end 0).
`match-end' and `match-beginning' also give indices of substrings
matched by parenthesis constructs in the pattern.

Optional arg BUFFER controls how case folding is done (according to
the value of `case-fold-search' in that buffer and that buffer's case
tables) and defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.
*/
       (regexp, string, start, buffer))
{
  return string_match_1 (regexp, string, start, decode_buffer (buffer, 0), 1);
}

/* Match REGEXP against STRING, searching all of STRING,
   and return the index of the match, or negative on failure.
   This does not clobber the match data. */

Bytecount
fast_string_match (Lisp_Object regexp, const Ibyte *nonreloc,
		   Lisp_Object reloc, Bytecount offset,
		   Bytecount length, int case_fold_search,
		   Error_Behavior errb, int no_quit)
{
  Bytecount val;
  Ibyte *newnonreloc = (Ibyte *) nonreloc;
  struct re_pattern_buffer *bufp;
  struct syntax_cache scache_struct;
  struct syntax_cache *scache = &scache_struct;

  bufp = compile_pattern (regexp, 0,
			  (case_fold_search
			   ? XCASE_TABLE_DOWNCASE (Vstandard_case_table)
			   : Qnil),
			  reloc, 0, 0, errb);
  if (!bufp)
    return -1; /* will only do this when errb != ERROR_ME */
  if (!no_quit)
    QUIT;
  else
    no_quit_in_re_search = 1;

  fixup_internal_substring (nonreloc, reloc, offset, &length);

  /* Don't need to protect against GC inside of re_search() due to QUIT;
     QUIT is GC-inhibited. */
  if (!NILP (reloc))
    newnonreloc = XSTRING_DATA (reloc);

  /* By making the regex object, regex buffer, and syntax cache arguments
     to re_{search,match}{,_2}, we've removed the need to do nasty things
     to deal with regex reentrancy. (See stack trace in signal.c for proof
     that this can happen.)

     #### there is still a potential problem with the regex cache --
     the compiled regex could be overwritten.  we'd need 20-fold
     reentrancy, though.  Fix this. */
  
  val = re_search (bufp, (char *) newnonreloc + offset, length, 0,
		   length, 0, reloc, 0, scache);

  no_quit_in_re_search = 0;
  return val;
}

Bytecount
fast_lisp_string_match (Lisp_Object regex, Lisp_Object string)
{
  return fast_string_match (regex, 0, string, 0, -1, 0, ERROR_ME, 0);
}


#ifdef REGION_CACHE_NEEDS_WORK
/* The newline cache: remembering which sections of text have no newlines.  */

/* If the user has requested newline caching, make sure it's on.
   Otherwise, make sure it's off.
   This is our cheezy way of associating an action with the change of
   state of a buffer-local variable.  */
static void
newline_cache_on_off (struct buffer *buf)
{
  if (NILP (buf->cache_long_line_scans))
    {
      /* It should be off.  */
      if (buf->newline_cache)
        {
          free_region_cache (buf->newline_cache);
          buf->newline_cache = 0;
        }
    }
  else
    {
      /* It should be on.  */
      if (buf->newline_cache == 0)
        buf->newline_cache = new_region_cache ();
    }
}
#endif

/* Search in BUF for COUNT instances of the character TARGET between
   START and END.

   If COUNT is positive, search forwards; END must be >= START.
   If COUNT is negative, search backwards for the -COUNTth instance;
      END must be <= START.
   If COUNT is zero, do anything you please; run rogue, for all I care.

   If END is zero, use BEGV or ZV instead, as appropriate for the
   direction indicated by COUNT.

   If we find COUNT instances, set *SHORTAGE to zero, and return the
   position after the COUNTth match.  Note that for reverse motion
   this is not the same as the usual convention for Emacs motion commands.

   If we don't find COUNT instances before reaching END, set *SHORTAGE
   to the number of TARGETs left unfound, and return END.

   If ALLOW_QUIT is non-zero, call QUIT periodically. */

static Bytebpos
byte_scan_buffer (struct buffer *buf, Ichar target, Bytebpos st, Bytebpos en,
		  EMACS_INT count, EMACS_INT *shortage, int allow_quit)
{
  Bytebpos lim = en > 0 ? en :
    ((count > 0) ? BYTE_BUF_ZV (buf) : BYTE_BUF_BEGV (buf));

  /* #### newline cache stuff in this function not yet ported */
  assert (count != 0);

  if (shortage)
    *shortage = 0;

  if (count > 0)
    {
#ifdef MULE
      Internal_Format fmt = buf->text->format;
      /* Check for char that's unrepresentable in the buffer -- it
         certainly can't be there. */
      if (!ichar_fits_in_format (target, fmt, wrap_buffer (buf)))
	{
	  *shortage = count;
	  return lim;
	}
      /* Due to the Mule representation of characters in a buffer, we can
	 simply search for characters in the range 0 - 127 directly; for
	 8-bit-fixed, we can do this for all characters.  In other cases,
	 we do it the "hard" way.  Note that this way works for all
	 characters and all formats, but the other way is faster. */
      else if (! (fmt == FORMAT_8_BIT_FIXED ||
		  (fmt == FORMAT_DEFAULT && ichar_ascii_p (target))))
	{
	  Raw_Ichar raw = ichar_to_raw (target, fmt, wrap_buffer (buf));
	  while (st < lim && count > 0)
	    {
	      if (BYTE_BUF_FETCH_CHAR_RAW (buf, st) == raw)
		count--;
	      INC_BYTEBPOS (buf, st);
	    }
	}
      else
#endif
	{
	  Raw_Ichar raw = ichar_to_raw (target, fmt, wrap_buffer (buf));
	  while (st < lim && count > 0)
	    {
	      Bytebpos ceil;
	      Ibyte *bufptr;

	      ceil = BYTE_BUF_CEILING_OF (buf, st);
	      ceil = min (lim, ceil);
	      bufptr = (Ibyte *) memchr (BYTE_BUF_BYTE_ADDRESS (buf, st),
					   raw, ceil - st);
	      if (bufptr)
		{
		  count--;
		  st = BYTE_BUF_PTR_BYTE_POS (buf, bufptr) + 1;
		}
	      else
		st = ceil;
	    }
	}

      if (shortage)
	*shortage = count;
      if (allow_quit)
	QUIT;
      return st;
    }
  else
    {
#ifdef MULE
      Internal_Format fmt = buf->text->format;
      /* Check for char that's unrepresentable in the buffer -- it
         certainly can't be there. */
      if (!ichar_fits_in_format (target, fmt, wrap_buffer (buf)))
	{
	  *shortage = -count;
	  return lim;
	}
      else if (! (fmt == FORMAT_8_BIT_FIXED ||
		  (fmt == FORMAT_DEFAULT && ichar_ascii_p (target))))
	{
	  Raw_Ichar raw = ichar_to_raw (target, fmt, wrap_buffer (buf));
	  while (st > lim && count < 0)
	    {
	      DEC_BYTEBPOS (buf, st);
	      if (BYTE_BUF_FETCH_CHAR_RAW (buf, st) == raw)
		count++;
	    }
	}
      else
#endif
	{
	  Raw_Ichar raw = ichar_to_raw (target, fmt, wrap_buffer (buf));
	  while (st > lim && count < 0)
	    {
	      Bytebpos floor;
	      Ibyte *bufptr;
	      Ibyte *floorptr;

	      floor = BYTE_BUF_FLOOR_OF (buf, st);
	      floor = max (lim, floor);
	      /* No memrchr() ... */
	      bufptr = BYTE_BUF_BYTE_ADDRESS_BEFORE (buf, st);
	      floorptr = BYTE_BUF_BYTE_ADDRESS (buf, floor);
	      while (bufptr >= floorptr)
		{
		  st--;
		  /* At this point, both ST and BUFPTR refer to the same
		     character.  When the loop terminates, ST will
		     always point to the last character we tried. */
		  if (*bufptr == (Ibyte) raw)
		    {
		      count++;
		      break;
		    }
		  bufptr--;
		}
	    }
	}

      if (shortage)
	*shortage = -count;
      if (allow_quit)
	QUIT;
      if (count)
	return st;
      else
	{
	/* We found the character we were looking for; we have to return
	   the position *after* it due to the strange way that the return
	   value is defined. */
	  INC_BYTEBPOS (buf, st);
	  return st;
	}
    }
}

Charbpos
scan_buffer (struct buffer *buf, Ichar target, Charbpos start, Charbpos end,
	     EMACS_INT count, EMACS_INT *shortage, int allow_quit)
{
  Bytebpos byte_retval;
  Bytebpos byte_start, byte_end;

  byte_start = charbpos_to_bytebpos (buf, start);
  if (end)
    byte_end = charbpos_to_bytebpos (buf, end);
  else
    byte_end = 0;
  byte_retval = byte_scan_buffer (buf, target, byte_start, byte_end, count,
			      shortage, allow_quit);
  return bytebpos_to_charbpos (buf, byte_retval);
}

Bytebpos
byte_find_next_newline_no_quit (struct buffer *buf, Bytebpos from, int count)
{
  return byte_scan_buffer (buf, '\n', from, 0, count, 0, 0);
}

Charbpos
find_next_newline_no_quit (struct buffer *buf, Charbpos from, int count)
{
  return scan_buffer (buf, '\n', from, 0, count, 0, 0);
}

Charbpos
find_next_newline (struct buffer *buf, Charbpos from, int count)
{
  return scan_buffer (buf, '\n', from, 0, count, 0, 1);
}

Bytecount
byte_find_next_ichar_in_string (Lisp_Object str, Ichar target, Bytecount st,
			       EMACS_INT count)
{
  Bytebpos lim = XSTRING_LENGTH (str) -1;
  Ibyte *s = XSTRING_DATA (str);

  assert (count >= 0);

#ifdef MULE
  /* Due to the Mule representation of characters in a buffer,
     we can simply search for characters in the range 0 - 127
     directly.  For other characters, we do it the "hard" way.
     Note that this way works for all characters but the other
     way is faster. */
  if (target >= 0200)
    {
      while (st < lim && count > 0)
	{
	  if (string_ichar (str, st) == target)
	    count--;
	  INC_BYTECOUNT (s, st);
	}
    }
  else
#endif
    {
      while (st < lim && count > 0)
	{
	  Ibyte *bufptr = (Ibyte *) memchr (itext_n_addr (s, st),
						(int) target, lim - st);
	  if (bufptr)
	    {
	      count--;
	      st =  (Bytebpos) (bufptr - s) + 1;
	    }
	  else
	    st = lim;
	}
    }
  return st;
}

/* Like find_next_newline, but returns position before the newline,
   not after, and only search up to TO.  This isn't just
   find_next_newline (...)-1, because you might hit TO.  */
Charbpos
find_before_next_newline (struct buffer *buf, Charbpos from, Charbpos to,
			  int count)
{
  EMACS_INT shortage;
  Charbpos pos = scan_buffer (buf, '\n', from, to, count, &shortage, 1);

  if (shortage == 0)
    pos--;

  return pos;
}

/* This function synched with FSF 21.1 */
static Lisp_Object
skip_chars (struct buffer *buf, int forwardp, int syntaxp,
	    Lisp_Object string, Lisp_Object lim)
{
  REGISTER Ibyte *p, *pend;
  REGISTER Ichar c;
  /* We store the first 256 chars in an array here and the rest in
     a range table. */
  unsigned char fastmap[0400];
  int negate = 0;
  REGISTER int i;
  Charbpos limit;
  struct syntax_cache *scache;
  
  if (NILP (lim))
    limit = forwardp ? BUF_ZV (buf) : BUF_BEGV (buf);
  else
    {
      CHECK_INT_COERCE_MARKER (lim);
      limit = XINT (lim);

      /* In any case, don't allow scan outside bounds of buffer.  */
      if (limit > BUF_ZV   (buf)) limit = BUF_ZV   (buf);
      if (limit < BUF_BEGV (buf)) limit = BUF_BEGV (buf);
    }

  CHECK_STRING (string);
  p = XSTRING_DATA (string);
  pend = p + XSTRING_LENGTH (string);
  memset (fastmap, 0, sizeof (fastmap));

  Fclear_range_table (Vskip_chars_range_table);

  if (p != pend && *p == '^')
    {
      negate = 1;
      p++;
    }

  /* Find the characters specified and set their elements of fastmap.
     If syntaxp, each character counts as itself.
     Otherwise, handle backslashes and ranges specially  */

  while (p != pend)
    {
      c = itext_ichar (p);
      INC_IBYTEPTR (p);
      if (syntaxp)
	{
	  if (c < 0400 && syntax_spec_code[c] < (unsigned char) Smax)
	    fastmap[c] = 1;
	  else
	    invalid_argument ("Invalid syntax designator", make_char (c));
	}
      else
	{
	  if (c == '\\')
	    {
	      if (p == pend) break;
	      c = itext_ichar (p);
	      INC_IBYTEPTR (p);
	    }
	  if (p != pend && *p == '-')
	    {
	      Ichar cend;

	      /* Skip over the dash.  */
	      p++;
	      if (p == pend) break;
	      cend = itext_ichar (p);
	      while (c <= cend && c < 0400)
		{
		  fastmap[c] = 1;
		  c++;
		}
	      if (c <= cend)
		Fput_range_table (make_int (c), make_int (cend), Qt,
				  Vskip_chars_range_table);
	      INC_IBYTEPTR (p);
	    }
	  else
	    {
	      if (c < 0400)
		fastmap[c] = 1;
	      else
		Fput_range_table (make_int (c), make_int (c), Qt,
				  Vskip_chars_range_table);
	    }
	}
    }

  /* #### Not in FSF 21.1 */
  if (syntaxp && fastmap['-'] != 0)
    fastmap[' '] = 1;

  /* If ^ was the first character, complement the fastmap.
     We don't complement the range table, however; we just use negate
     in the comparisons below. */

  if (negate)
    for (i = 0; i < (int) (sizeof (fastmap)); i++)
      fastmap[i] ^= 1;

  {
    Charbpos start_point = BUF_PT (buf);
    Charbpos pos = start_point;
    Charbpos pos_byte = BYTE_BUF_PT (buf);

    if (syntaxp)
      {
	scache = setup_buffer_syntax_cache (buf, pos, forwardp ? 1 : -1);
	/* All syntax designators are normal chars so nothing strange
	   to worry about */
	if (forwardp)
	  {
	    if (pos < limit)
	      while (fastmap[(unsigned char)
			     syntax_code_spec
			     [(int) SYNTAX_FROM_CACHE
			      (scache, BYTE_BUF_FETCH_CHAR (buf, pos_byte))]])
		{
		  pos++;
		  INC_BYTEBPOS (buf, pos_byte);
		  if (pos >= limit)
		    break;
		  UPDATE_SYNTAX_CACHE_FORWARD (scache, pos);
		}
	  }
	else
	  {
	    while (pos > limit)
	      {
		Charbpos savepos = pos_byte;
		pos--;
		DEC_BYTEBPOS (buf, pos_byte);
		UPDATE_SYNTAX_CACHE_BACKWARD (scache, pos);
		if (!fastmap[(unsigned char)
			     syntax_code_spec
			     [(int) SYNTAX_FROM_CACHE
			      (scache, BYTE_BUF_FETCH_CHAR (buf, pos_byte))]])
		  {
		    pos++;
		    pos_byte = savepos;
		    break;
		  }
	      }
	  }
      }
    else
      {
	if (forwardp)
	  {
	    while (pos < limit)
	      {
		Ichar ch = BYTE_BUF_FETCH_CHAR (buf, pos_byte);
		if ((ch < 0400) ? fastmap[ch] :
		    (NILP (Fget_range_table (make_int (ch),
					     Vskip_chars_range_table,
					     Qnil))
		     == negate))
		  {
		    pos++;
		    INC_BYTEBPOS (buf, pos_byte);
		  }
		else
		  break;
	      }
	  }
	else
	  {
	    while (pos > limit)
	      {
		Charbpos prev_pos_byte = pos_byte;
		Ichar ch;

		DEC_BYTEBPOS (buf, prev_pos_byte);
		ch = BYTE_BUF_FETCH_CHAR (buf, prev_pos_byte);
		if ((ch < 0400) ? fastmap[ch] :
		    (NILP (Fget_range_table (make_int (ch),
					     Vskip_chars_range_table,
					     Qnil))
		     == negate))
		  {
		    pos--;
		    pos_byte = prev_pos_byte;
		  }
                else
                  break;
	      }
	  }
      }
    QUIT;
    BOTH_BUF_SET_PT (buf, pos, pos_byte);
    return make_int (BUF_PT (buf) - start_point);
  }
}

DEFUN ("skip-chars-forward", Fskip_chars_forward, 1, 3, 0, /*
Move point forward, stopping before a char not in STRING, or at pos LIMIT.
STRING is like the inside of a `[...]' in a regular expression
except that `]' is never special and `\\' quotes `^', `-' or `\\'.
Thus, with arg "a-zA-Z", this skips letters stopping before first nonletter.
With arg "^a-zA-Z", skips nonletters stopping before first letter.
Returns the distance traveled, either zero or positive.

Optional argument BUFFER defaults to the current buffer.
*/
       (string, limit, buffer))
{
  return skip_chars (decode_buffer (buffer, 0), 1, 0, string, limit);
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, 1, 3, 0, /*
Move point backward, stopping after a char not in STRING, or at pos LIMIT.
See `skip-chars-forward' for details.
Returns the distance traveled, either zero or negative.

Optional argument BUFFER defaults to the current buffer.
*/
       (string, limit, buffer))
{
  return skip_chars (decode_buffer (buffer, 0), 0, 0, string, limit);
}


DEFUN ("skip-syntax-forward", Fskip_syntax_forward, 1, 3, 0, /*
Move point forward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop before a char whose syntax is not in SYNTAX, or at position LIMIT.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or positive.

Optional argument BUFFER defaults to the current buffer.
*/
       (syntax, limit, buffer))
{
  return skip_chars (decode_buffer (buffer, 0), 1, 1, syntax, limit);
}

DEFUN ("skip-syntax-backward", Fskip_syntax_backward, 1, 3, 0, /*
Move point backward across chars in specified syntax classes.
SYNTAX is a string of syntax code characters.
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIMIT.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or negative.

Optional argument BUFFER defaults to the current buffer.
*/
       (syntax, limit, buffer))
{
  return skip_chars (decode_buffer (buffer, 0), 0, 1, syntax, limit);
}


/* Subroutines of Lisp buffer search functions. */

static Lisp_Object
search_command (Lisp_Object string, Lisp_Object limit, Lisp_Object noerror,
		Lisp_Object count, Lisp_Object buffer, int direction,
		int RE, int posix)
{
  REGISTER Charbpos np;
  Charbpos lim;
  EMACS_INT n = direction;
  struct buffer *buf;

  if (!NILP (count))
    {
      CHECK_INT (count);
      n *= XINT (count);
    }

  buf = decode_buffer (buffer, 0);
  CHECK_STRING (string);
  if (NILP (limit))
    lim = n > 0 ? BUF_ZV (buf) : BUF_BEGV (buf);
  else
    {
      CHECK_INT_COERCE_MARKER (limit);
      lim = XINT (limit);
      if (n > 0 ? lim < BUF_PT (buf) : lim > BUF_PT (buf))
	invalid_argument ("Invalid search limit (wrong side of point)",
			  Qunbound);
      if (lim > BUF_ZV (buf))
	lim = BUF_ZV (buf);
      if (lim < BUF_BEGV (buf))
	lim = BUF_BEGV (buf);
    }

  np = search_buffer (buf, string, BUF_PT (buf), lim, n, RE,
		      (!NILP (buf->case_fold_search)
		       ? XCASE_TABLE_CANON (buf->case_table)
                       : Qnil),
		      (!NILP (buf->case_fold_search)
		       ? XCASE_TABLE_EQV (buf->case_table)
                       : Qnil), posix);

  if (np <= 0)
    {
      if (NILP (noerror))
	{
	  signal_failure (string);
	  RETURN_NOT_REACHED (Qnil);
	}
      if (!EQ (noerror, Qt))
	{
	  if (lim < BUF_BEGV (buf) || lim > BUF_ZV (buf))
	    ABORT ();
	  BUF_SET_PT (buf, lim);
	  return Qnil;
#if 0 /* This would be clean, but maybe programs depend on
	 a value of nil here.  */
	  np = lim;
#endif
	}
      else
        return Qnil;
    }

  if (np < BUF_BEGV (buf) || np > BUF_ZV (buf))
    ABORT ();

  BUF_SET_PT (buf, np);

  return make_int (np);
}

static int
trivial_regexp_p (Lisp_Object regexp)
{
  Bytecount len = XSTRING_LENGTH (regexp);
  Ibyte *s = XSTRING_DATA (regexp);
  while (--len >= 0)
    {
      switch (*s++)
	{
	/* #### howcum ']' doesn't appear here, but ... */
	case '.': case '*': case '+': case '?': case '[': case '^': case '$':
	  return 0;
	case '\\':
	  if (--len < 0)
	    return 0;
	  switch (*s++)
	    {
	    /* ... ')' does appear here?  ('<' and '>' can appear singly.) */
	    /* #### are there other constructs to check? */
	    case '|': case '(': case ')': case '`': case '\'': case 'b':
	    case 'B': case '<': case '>': case 'w': case 'W': case 's':
	    case 'S': case '=': case '{': case '}':
#ifdef MULE
	    /* 97/2/25 jhod Added for category matches */
	    case 'c': case 'C':
#endif /* MULE */
	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      return 0;
	    }
	}
    }
  return 1;
}

/* Search for the n'th occurrence of STRING in BUF,
   starting at position CHARBPOS and stopping at position BUFLIM,
   treating PAT as a literal string if RE is false or as
   a regular expression if RE is true.

   If N is positive, searching is forward and BUFLIM must be greater
   than CHARBPOS.
   If N is negative, searching is backward and BUFLIM must be less
   than CHARBPOS.

   Returns -x if only N-x occurrences found (x > 0),
   or else the position at the beginning of the Nth occurrence
   (if searching backward) or the end (if searching forward).

   POSIX is nonzero if we want full backtracking (POSIX style)
   for this pattern.  0 means backtrack only enough to get a valid match.  */
static Charbpos
search_buffer (struct buffer *buf, Lisp_Object string, Charbpos charbpos,
	       Charbpos buflim, EMACS_INT n, int RE, Lisp_Object trt,
	       Lisp_Object inverse_trt, int posix)
{
  Bytecount len = XSTRING_LENGTH (string);
  Ibyte *base_pat = XSTRING_DATA (string);
  REGISTER EMACS_INT i, j;
  Bytebpos p1, p2;
  Bytecount s1, s2;
  Bytebpos pos, lim;

  /* Some FSF junk with running_asynch_code, to preserve the match
     data.  Not necessary because we don't call process filters
     asynchronously (i.e. from within QUIT). */

  /* Searching 0 times means noop---don't move, don't touch registers.  */
  if (n == 0)
    return charbpos;

  /* Null string is found at starting position.  */
  if (len == 0)
    {
      set_search_regs (buf, charbpos, 0);
      return charbpos;
    }

  pos = charbpos_to_bytebpos (buf, charbpos);
  lim = charbpos_to_bytebpos (buf, buflim);
  if (RE && !trivial_regexp_p (string))
    {
      struct re_pattern_buffer *bufp;

      bufp = compile_pattern (string, &search_regs, trt,
			      wrap_buffer (buf), buf, posix, ERROR_ME);

      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BYTE_BUF_BEGV (buf);
      p2 = BYTE_BUF_CEILING_OF (buf, p1);
      s1 = p2 - p1;
      s2 = BYTE_BUF_ZV (buf) - p2;

      while (n != 0)
	{
	  Bytecount val;
	  struct syntax_cache scache_struct;
	  struct syntax_cache *scache = &scache_struct;
  
	  QUIT;
	  /* By making the regex object, regex buffer, and syntax cache
	     arguments to re_{search,match}{,_2}, we've removed the need to
	     do nasty things to deal with regex reentrancy. (See stack
	     trace in signal.c for proof that this can happen.)

	     #### there is still a potential problem with the regex cache --
	     the compiled regex could be overwritten.  we'd need 20-fold
	     reentrancy, though.  Fix this. */

	  val = re_search_2 (bufp,
			     (char *) BYTE_BUF_BYTE_ADDRESS (buf, p1), s1,
			     (char *) BYTE_BUF_BYTE_ADDRESS (buf, p2), s2,
                             pos - BYTE_BUF_BEGV (buf), lim - pos, &search_regs,
			     n > 0 ? lim - BYTE_BUF_BEGV (buf) :
			     pos - BYTE_BUF_BEGV (buf), wrap_buffer (buf),
			     buf, scache);

	  if (val == -2)
	    {
	      matcher_overflow ();
	    }
	  if (val >= 0)
	    {
	      int num_regs = search_regs.num_regs;
	      j = BYTE_BUF_BEGV (buf);
	      for (i = 0; i < num_regs; i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      last_thing_searched = wrap_buffer (buf);
	      /* Set pos to the new position. */
	      pos = n > 0 ? search_regs.end[0] : search_regs.start[0];
	      fixup_search_regs_for_buffer (buf);
	      /* And charbpos too. */
	      charbpos = n > 0 ? search_regs.end[0] : search_regs.start[0];
	    }
	  else
	    return (n > 0 ? 0 - n : n);
	  if (n > 0) n--; else n++;
	}
      return charbpos;
    }
  else				/* non-RE case */
    {
      int charset_base = -1;
      int boyer_moore_ok = 1;
      Ibyte *patbuf = alloca_ibytes (len * MAX_ICHAR_LEN);
      Ibyte *pat = patbuf;

#ifdef MULE
      int entirely_one_byte_p = buf->text->entirely_one_byte_p;
      int nothing_greater_than_0xff =
        buf->text->num_8_bit_fixed_chars == BUF_Z(buf) - BUF_BEG (buf);

      while (len > 0)
	{
	  Ibyte tmp_str[MAX_ICHAR_LEN];
	  Ichar c, translated, inverse;
	  Bytecount orig_bytelen, new_bytelen, inv_bytelen;

	  /* If we got here and the RE flag is set, it's because
	     we're dealing with a regexp known to be trivial, so the
	     backslash just quotes the next character.  */
	  if (RE && *base_pat == '\\')
	    {
	      len--;
	      base_pat++;
	    }
	  c = itext_ichar (base_pat);
	  translated = TRANSLATE (trt, c);
	  inverse = TRANSLATE (inverse_trt, c);

	  orig_bytelen = itext_ichar_len (base_pat);
	  inv_bytelen = set_itext_ichar (tmp_str, inverse);
	  new_bytelen = set_itext_ichar (tmp_str, translated);

          if (-1 == charset_base)
            {
              /* Keep track of which charset and character set row
                 contains the characters that need translation.

                 Zero out the bits corresponding to the last byte. */
              charset_base = c & ~ICHAR_FIELD3_MASK;
            }

          if (boyer_moore_ok && (translated != c || inverse != c))
            {
	      Ichar starting_c = c;
	      int charset_base_code;

	      do 
		{
		  c = TRANSLATE (inverse_trt, c);

                  /* If a character cannot occur in the buffer, ignore
                     it. */
                  if (c > 0x7F && entirely_one_byte_p)
                    continue;

                  if (c > 0xFF && nothing_greater_than_0xff)
                    continue;

                  charset_base_code = c & ~ICHAR_FIELD3_MASK;

                  if (charset_base_code != charset_base)
                    {
                      /* If two different rows, or two different charsets,
                         appear, needing translation, then we cannot use
                         boyer_moore search.  See the comment at the head of
                         boyer_moore(). */
                      boyer_moore_ok = 0;
                      break;
                    }
                } while (c != starting_c);

              if (boyer_moore_ok && (charset_base != 
                                     (translated & ~ICHAR_FIELD3_MASK)))
                {
                  /* In the rare event that the CANON entry for this
                     character is not in the desired set, choose one
                     that is, from the equivalence set. It doesn't much
                     matter which. */
                  Ichar starting_ch = translated;
                  do
                    {
                      translated = TRANSLATE (inverse_trt, translated);

                      if (charset_base == (translated & ~ICHAR_FIELD3_MASK))
                        break;

                    } while (starting_ch != translated);

                  assert (starting_ch != translated);

                  new_bytelen = set_itext_ichar (tmp_str, translated);
                }
            }

	  memcpy (pat, tmp_str, new_bytelen);
	  pat += new_bytelen;
	  base_pat += orig_bytelen;
	  len -= orig_bytelen;
	}
#else /* not MULE */
      while (--len >= 0)
	{
	  /* If we got here and the RE flag is set, it's because
	     we're dealing with a regexp known to be trivial, so the
	     backslash just quotes the next character.  */
	  if (RE && *base_pat == '\\')
	    {
	      len--;
	      base_pat++;
	    }
	  *pat++ = TRANSLATE (trt, *base_pat++);
	}
#endif /* MULE */
      len = pat - patbuf;
      pat = base_pat = patbuf;
      if (boyer_moore_ok)
	return boyer_moore (buf, base_pat, len, pos, lim, n,
			    trt, inverse_trt, charset_base);
      else
	return simple_search (buf, base_pat, len, pos, lim, n, trt);
    }
}

/* Do a simple string search N times for the string PAT, whose length is
   LEN/LEN_BYTE, from buffer position POS until LIM.  TRT is the
   translation table.

   Return the character position where the match is found.
   Otherwise, if M matches remained to be found, return -M.

   This kind of search works regardless of what is in PAT and
   regardless of what is in TRT.  It is used in cases where
   boyer_moore cannot work.  */

static Charbpos
simple_search (struct buffer *buf, Ibyte *base_pat, Bytecount len,
	       Bytebpos pos, Bytebpos lim, EMACS_INT n, Lisp_Object trt)
{
  int forward = n > 0;
  Bytecount buf_len = 0; /* Shut up compiler. */

  if (lim > pos)
    while (n > 0)
      {
	while (1)
	  {
	    Bytecount this_len = len;
	    Bytebpos this_pos = pos;
	    Ibyte *p = base_pat;
	    if (pos >= lim)
	      goto stop;

	    while (this_len > 0)
	      {
		Ichar pat_ch, buf_ch;
		Bytecount pat_len;

		pat_ch = itext_ichar (p);
		buf_ch = BYTE_BUF_FETCH_CHAR (buf, this_pos);

		buf_ch = TRANSLATE (trt, buf_ch);

		if (buf_ch != pat_ch)
		  break;

		pat_len = itext_ichar_len (p);
		p += pat_len;
		this_len -= pat_len;
		INC_BYTEBPOS (buf, this_pos);
	      }
	    if (this_len == 0)
	      {
		buf_len = this_pos - pos;
		pos = this_pos;
		break;
	      }
	    INC_BYTEBPOS (buf, pos);
	  }
	n--;
      }
  else
    {
      /* If lim < len, then there are too few buffer positions to hold the
	 pattern between the beginning of the buffer and lim.  Adjust to
	 ensure pattern fits.  If we don't do this, we can assert in the
	 DEC_BYTEBPOS below. */
      if (lim < len)
	lim = len;
      while (n < 0)
	{
	  while (1)
	    {
	      Bytecount this_len = len;
	      Bytebpos this_pos = pos;
	      Ibyte *p;
	      if (pos <= lim)
		goto stop;
	      p = base_pat + len;

	      while (this_len > 0)
		{
		  Ichar pat_ch, buf_ch;

		  DEC_IBYTEPTR (p);
		  DEC_BYTEBPOS (buf, this_pos);
		  pat_ch = itext_ichar (p);
		  buf_ch = BYTE_BUF_FETCH_CHAR (buf, this_pos);

		  buf_ch = TRANSLATE (trt, buf_ch);

		  if (buf_ch != pat_ch)
		    break;

		  this_len -= itext_ichar_len (p);
		}
	      if (this_len == 0)
		{
		  buf_len = pos - this_pos;
		  pos = this_pos;
		  break;
		}
	      DEC_BYTEBPOS (buf, pos);
	    }
	  n++;
	}
    }
 stop:
  if (n == 0)
    {
      Charbpos beg, end, retval;
      if (forward)
	{
	  beg = bytebpos_to_charbpos (buf, pos - buf_len);
	  retval = end = bytebpos_to_charbpos (buf, pos);
	}
      else
	{
	  retval = beg = bytebpos_to_charbpos (buf, pos);
	  end = bytebpos_to_charbpos (buf, pos + buf_len);
	}
      set_search_regs (buf, beg, end - beg);

      return retval;
    }
  else if (n > 0)
    return -n;
  else
    return n;
}

/* Do Boyer-Moore search N times for the string PAT,
   whose length is LEN/LEN_BYTE,
   from buffer position POS/POS_BYTE until LIM/LIM_BYTE.
   DIRECTION says which direction we search in.
   TRT and INVERSE_TRT are translation tables.

   This kind of search works if all the characters in PAT that have
   nontrivial translation are the same aside from the last byte.  This
   makes it possible to translate just the last byte of a character,
   and do so after just a simple test of the context.

   If that criterion is not satisfied, do not call this function.  You will
   get an assertion failure. */
	    
static Charbpos
boyer_moore (struct buffer *buf, Ibyte *base_pat, Bytecount len,
	     Bytebpos pos, Bytebpos lim, EMACS_INT n, Lisp_Object trt,
	     Lisp_Object inverse_trt, int USED_IF_MULE (charset_base))
{
  /* #### Someone really really really needs to comment the workings
     of this junk somewhat better.

     BTW "BM" stands for Boyer-Moore, which is one of the standard
     string-searching algorithms.  It's the best string-searching
     algorithm out there, provided that:

     a) You're not fazed by algorithm complexity. (Rabin-Karp, which
     uses hashing, is much much easier to code but not as fast.)
     b) You can freely move backwards in the string that you're
     searching through.

     As the comment below tries to explain (but garbles in typical
     programmer-ese), the idea is that you don't have to do a
     string match at every successive position in the text.  For
     example, let's say the pattern is "a very long string".  We
     compare the last character in the string (`g') with the
     corresponding character in the text.  If it mismatches, and
     it is, say, `z', then we can skip forward by the entire
     length of the pattern because `z' does not occur anywhere
     in the pattern.  If the mismatching character does occur
     in the pattern, we can usually still skip forward by more
     than one: e.g. if it is `l', then we can skip forward
     by the length of the substring "ong string" -- i.e. the
     largest end section of the pattern that does not contain
     the mismatched character.  So what we do is compute, for
     each possible character, the distance we can skip forward
     (the "stride") and use it in the string matching.  This
     is what the BM_tab holds. */
  REGISTER EMACS_INT *BM_tab;
  EMACS_INT *BM_tab_base;
  REGISTER Bytecount dirlen;
  EMACS_INT infinity;
  Bytebpos limit;
  Bytecount stride_for_teases = 0;
  REGISTER EMACS_INT i, j;
  Ibyte *pat, *pat_end;
  REGISTER Ibyte *cursor, *p_limit, *ptr2;
  Ibyte simple_translate[0400];
  REGISTER int direction = ((n > 0) ? 1 : -1);
#ifdef MULE
  Ibyte translate_prev_byte = 0;
  Ibyte translate_anteprev_byte = 0;
  /* These need to be rethought in the event that the internal format
     changes, or in the event that num_8_bit_fixed_chars disappears
     (entirely_one_byte_p can be trivially worked out by checking is the
     byte count equal to the char count.)  */
  int buffer_entirely_one_byte_p = buf->text->entirely_one_byte_p;
  int buffer_nothing_greater_than_0xff =
    buf->text->num_8_bit_fixed_chars == BUF_Z(buf) - BUF_BEG (buf);
#endif
#ifdef C_ALLOCA
  EMACS_INT BM_tab_space[0400];
  BM_tab = &BM_tab_space[0];
#else
  BM_tab = alloca_array (EMACS_INT, 256);
#endif
  
  /* The general approach is that we are going to maintain that we
     know the first (closest to the present position, in whatever
     direction we're searching) character that could possibly be
     the last (furthest from present position) character of a
     valid match.  We advance the state of our knowledge by
     looking at that character and seeing whether it indeed
     matches the last character of the pattern.  If it does, we
     take a closer look.  If it does not, we move our pointer (to
     putative last characters) as far as is logically possible.
     This amount of movement, which I call a stride, will be the
     length of the pattern if the actual character appears nowhere
     in the pattern, otherwise it will be the distance from the
     last occurrence of that character to the end of the pattern.
     As a coding trick, an enormous stride is coded into the table
     for characters that match the last character.  This allows
     use of only a single test, a test for having gone past the
     end of the permissible match region, to test for both
     possible matches (when the stride goes past the end
     immediately) and failure to match (where you get nudged past
     the end one stride at a time).

     Here we make a "mickey mouse" BM table.  The stride of the
     search is determined only by the last character of the
     putative match.  If that character does not match, we will
     stride the proper distance to propose a match that
     superimposes it on the last instance of a character that
     matches it (per trt), or misses it entirely if there is
     none. */

  dirlen = len * direction;
  infinity = dirlen - (lim + pos + len + len) * direction;
  /* Record position after the end of the pattern.  */
  pat_end = base_pat + len;
  if (direction < 0)
    base_pat = pat_end - 1;
  BM_tab_base = BM_tab;
  BM_tab += 0400;
  j = dirlen;		/* to get it in a register */
  /* A character that does not appear in the pattern induces a
     stride equal to the pattern length. */
  while (BM_tab_base != BM_tab)
    {
      *--BM_tab = j;
      *--BM_tab = j;
      *--BM_tab = j;
      *--BM_tab = j;
    }
  /* We use this for translation, instead of TRT itself.  We
     fill this in to handle the characters that actually occur
     in the pattern.  Others don't matter anyway!  */
  xzero (simple_translate);
  for (i = 0; i < 0400; i++)
    simple_translate[i] = (Ibyte) i;
  i = 0;

  while (i != infinity)
    {
      Ibyte *ptr = base_pat + i;
      i += direction;
      if (i == dirlen)
	i = infinity;
      if (!NILP (trt))
	{
#ifdef MULE
	  Ichar ch, untranslated;
	  int this_translated = 1;

	  /* Is *PTR the last byte of a character?  */
	  if (pat_end - ptr == 1 || ibyte_first_byte_p (ptr[1]))
	    {
	      Ibyte *charstart = ptr;
	      while (!ibyte_first_byte_p (*charstart))
		charstart--;
	      untranslated = itext_ichar (charstart);

              /* We shouldn't have been passed a string with varying
                 character sets or rows. That's what simple_search is
                 for.  */
              assert (charset_base == (untranslated & ~ICHAR_FIELD3_MASK));

              ch = TRANSLATE (trt, untranslated);
              if (!ibyte_first_byte_p (*ptr))
                {
                  translate_prev_byte = ptr[-1];
                  if (!ibyte_first_byte_p (translate_prev_byte))
                    translate_anteprev_byte = ptr[-2];
                }

              if (charset_base != (ch & ~ICHAR_FIELD3_MASK))
                {
                  /* In the very rare event that the CANON entry for this
                     character is not in the desired set, choose one that
                     is, from the equivalence set. It doesn't much matter
                     which, since we're building our own cheesy equivalence
                     table instead of using that belonging to the case
                     table directly.

                     We can get here if search_buffer has worked out that
                     the buffer is entirely single width. */
                  Ichar starting_ch = ch;
                  do
                    {
                      ch = TRANSLATE (inverse_trt, ch);
                      if (charset_base == (ch & ~ICHAR_FIELD3_MASK))
                        break;

                    } while (starting_ch != ch);

                  /* If starting_ch is equal to ch, the case table is
                     corrupt. (Any mapping in the canon table should be
                     reflected in the equivalence table, and we know from
                     the canon table that untranslated maps to starting_ch
                     and that untranslated has the correct value for
                     charset_base.) */
                  assert (starting_ch != ch);
		}
	    }
	  else
	    {
	      ch = *ptr;
	      this_translated = 0;
	    }
	  if (ch > 0400)
	    j = ((unsigned char) ch | 0200);
	  else
	    j = (unsigned char) ch;
	      
	  if (i == infinity)
	    stride_for_teases = BM_tab[j];
	  BM_tab[j] = dirlen - i;
	  /* A translation table is accompanied by its inverse -- see
	     comment in casetab.c. */
	  if (this_translated)
	    {
	      Ichar starting_ch = ch;
	      EMACS_INT starting_j = j;
	      do
		{
		  ch = TRANSLATE (inverse_trt, ch);

                  if (ch > 0x7F && buffer_entirely_one_byte_p)
                    continue;

                  if (ch > 0xFF && buffer_nothing_greater_than_0xff)
                    continue;

                  if (ch > 0400)
                    j = ((unsigned char) ch | 0200);
                  else
                    j = (unsigned char) ch;

                  /* For all the characters that map into CH, set up
                     simple_translate to map the last byte into
                     STARTING_J.  */
                  simple_translate[j] = (Ibyte) starting_j;
                  BM_tab[j] = dirlen - i;

		} while (ch != starting_ch);
	    }
#else
	  EMACS_INT k;
	  j = *ptr;
	  k = (j = TRANSLATE (trt, j));
	  if (i == infinity)
	    stride_for_teases = BM_tab[j];
	  BM_tab[j] = dirlen - i;
	  /* A translation table is accompanied by its inverse --
	     see comment in casetab.c. */
	  while ((j = TRANSLATE (inverse_trt, j)) != k)
	    {
	      simple_translate[j] = (Ibyte) k;
	      BM_tab[j] = dirlen - i;
	    }
#endif
	}
      else
	{
	  j = *ptr;

	  if (i == infinity)
	    stride_for_teases = BM_tab[j];
	  BM_tab[j] = dirlen - i;
	}
      /* stride_for_teases tells how much to stride if we get a
	 match on the far character but are subsequently
	 disappointed, by recording what the stride would have been
	 for that character if the last character had been
	 different. */
    }
  infinity = dirlen - infinity;
  pos += dirlen - ((direction > 0) ? direction : 0);
  /* loop invariant - pos points at where last char (first char if
     reverse) of pattern would align in a possible match.  */
  while (n != 0)
    {
      Bytebpos tail_end;
      Ibyte *tail_end_ptr;
      /* It's been reported that some (broken) compiler thinks
	 that Boolean expressions in an arithmetic context are
	 unsigned.  Using an explicit ?1:0 prevents this.  */
      if ((lim - pos - ((direction > 0) ? 1 : 0)) * direction < 0)
	return n * (0 - direction);
      /* First we do the part we can by pointers (maybe
	 nothing) */
      QUIT;
      pat = base_pat;
      limit = pos - dirlen + direction;
      /* XEmacs change: definitions of CEILING_OF and FLOOR_OF
	 have changed.  See buffer.h. */
      limit = ((direction > 0)
	       ? BYTE_BUF_CEILING_OF (buf, limit) - 1
	       : BYTE_BUF_FLOOR_OF (buf, limit + 1));
      /* LIMIT is now the last (not beyond-last!) value POS can
	 take on without hitting edge of buffer or the gap.  */
      limit = ((direction > 0)
	       ? min (lim - 1, min (limit, pos + 20000))
	       : max (lim, max (limit, pos - 20000)));
      tail_end = BYTE_BUF_CEILING_OF (buf, pos);
      tail_end_ptr = BYTE_BUF_BYTE_ADDRESS (buf, tail_end);

      if ((limit - pos) * direction > 20)
	{
	  /* We have to be careful because the code can generate addresses
	     that don't point to the beginning of characters. */
	  p_limit = BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, limit);
	  ptr2 = (cursor = BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, pos));
	  /* In this loop, pos + cursor - ptr2 is the surrogate
	     for pos */
	  while (1)	/* use one cursor setting as long as i can */
	    {
	      if (direction > 0) /* worth duplicating */
		{
		  /* Use signed comparison if appropriate to make
		     cursor+infinity sure to be > p_limit.
		     Assuming that the buffer lies in a range of
		     addresses that are all "positive" (as ints)
		     or all "negative", either kind of comparison
		     will work as long as we don't step by
		     infinity.  So pick the kind that works when
		     we do step by infinity.  */
		  if ((EMACS_INT) (p_limit + infinity) >
		      (EMACS_INT) p_limit)
		    while ((EMACS_INT) cursor <=
			   (EMACS_INT) p_limit)
		      cursor += BM_tab[*cursor];
		  else
		    while ((EMACS_UINT) cursor <=
			   (EMACS_UINT) p_limit)
		      cursor += BM_tab[*cursor];
		}
	      else
		{
		  if ((EMACS_INT) (p_limit + infinity) <
		      (EMACS_INT) p_limit)
		    while ((EMACS_INT) cursor >=
			   (EMACS_INT) p_limit)
		      cursor += BM_tab[*cursor];
		  else
		    while ((EMACS_UINT) cursor >=
			   (EMACS_UINT) p_limit)
		      cursor += BM_tab[*cursor];
		}
	      /* If you are here, cursor is beyond the end of the
		 searched region.  This can happen if you match on
		 the far character of the pattern, because the
		 "stride" of that character is infinity, a number
		 able to throw you well beyond the end of the
		 search.  It can also happen if you fail to match
		 within the permitted region and would otherwise
		 try a character beyond that region */
	      if ((cursor - p_limit) * direction <= len)
		break;	/* a small overrun is genuine */
	      cursor -= infinity; /* large overrun = hit */
	      i = dirlen - direction;
	      if (!NILP (trt))
		{
		  while ((i -= direction) + direction != 0)
		    {
#ifdef MULE
		      Ichar ch;
		      cursor -= direction;
		      /* Translate only the last byte of a character.  */
		      if ((cursor == tail_end_ptr
			   || ibyte_first_byte_p (cursor[1]))
			  && (ibyte_first_byte_p (cursor[0])
			      || (translate_prev_byte == cursor[-1]
				  && (ibyte_first_byte_p (translate_prev_byte)
				      || translate_anteprev_byte == cursor[-2]))))
			ch = simple_translate[*cursor];
		      else
			ch = *cursor;
		      if (pat[i] != ch)
			break;
#else
		      if (pat[i] != TRANSLATE (trt, *(cursor -= direction)))
			break;
#endif
		    }
		}
	      else
		{
		  while ((i -= direction) + direction != 0)
		    if (pat[i] != *(cursor -= direction))
		      break;
		}
	      cursor += dirlen - i - direction;	/* fix cursor */
	      if (i + direction == 0)
		{
		  cursor -= direction;

		  {
		    Bytebpos bytstart = (pos + cursor - ptr2 +
				       ((direction > 0)
					? 1 - len : 0));
		    Charbpos bufstart = bytebpos_to_charbpos (buf, bytstart);
		    Charbpos bufend = bytebpos_to_charbpos (buf, bytstart + len);

		    set_search_regs (buf, bufstart, bufend - bufstart);
		  }

		  if ((n -= direction) != 0)
		    cursor += dirlen; /* to resume search */
		  else
		    return ((direction > 0)
			    ? search_regs.end[0] : search_regs.start[0]);
		}
	      else
		cursor += stride_for_teases; /* <sigh> we lose -  */
	    }
	  pos += cursor - ptr2;
	}
      else
	/* Now we'll pick up a clump that has to be done the hard
	   way because it covers a discontinuity */
	{
	  /* XEmacs change: definitions of CEILING_OF and FLOOR_OF
	     have changed.  See buffer.h. */
	  limit = ((direction > 0)
		   ? BYTE_BUF_CEILING_OF (buf, pos - dirlen + 1) - 1
		   : BYTE_BUF_FLOOR_OF (buf, pos - dirlen));
	  limit = ((direction > 0)
		   ? min (limit + len, lim - 1)
		   : max (limit - len, lim));
	  /* LIMIT is now the last value POS can have
	     and still be valid for a possible match.  */
	  while (1)
	    {
	      /* This loop can be coded for space rather than
		 speed because it will usually run only once.
		 (the reach is at most len + 21, and typically
		 does not exceed len) */
	      while ((limit - pos) * direction >= 0)
		/* *not* BYTE_BUF_FETCH_CHAR.  We are working here
		   with bytes, not characters. */
		pos += BM_tab[*BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, pos)];
	      /* now run the same tests to distinguish going off
		 the end, a match or a phony match. */
	      if ((pos - limit) * direction <= len)
		break;	/* ran off the end */
	      /* Found what might be a match.
		 Set POS back to last (first if reverse) char pos.  */
	      pos -= infinity;
	      i = dirlen - direction;
	      while ((i -= direction) + direction != 0)
		{
#ifdef MULE
		  Ichar ch;
		  Ibyte *ptr;
#endif
		  pos -= direction;
#ifdef MULE
		  ptr = BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, pos);
		  if ((ptr == tail_end_ptr
		       || ibyte_first_byte_p (ptr[1]))
		      && (ibyte_first_byte_p (ptr[0])
			  || (translate_prev_byte == ptr[-1]
			      && (ibyte_first_byte_p (translate_prev_byte)
				  || translate_anteprev_byte == ptr[-2]))))
		    ch = simple_translate[*ptr];
		  else
		    ch = *ptr;
		  if (pat[i] != ch)
		    break;
		      
#else
		  if (pat[i] !=
		      TRANSLATE (trt,
				 *BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, pos)))
		    break;
#endif
		}
	      /* Above loop has moved POS part or all the way back
		 to the first char pos (last char pos if reverse).
		 Set it once again at the last (first if reverse)
		 char.  */
	      pos += dirlen - i- direction;
	      if (i + direction == 0)
		{
		  pos -= direction;

		  {
		    Bytebpos bytstart = (pos +
				       ((direction > 0)
					? 1 - len : 0));
		    Charbpos bufstart = bytebpos_to_charbpos (buf, bytstart);
		    Charbpos bufend = bytebpos_to_charbpos (buf, bytstart + len);

		    set_search_regs (buf, bufstart, bufend - bufstart);
		  }

		  if ((n -= direction) != 0)
		    pos += dirlen; /* to resume search */
		  else
		    return ((direction > 0)
			    ? search_regs.end[0] : search_regs.start[0]);
		}
	      else
		pos += stride_for_teases;
	    }
	}
      /* We have done one clump.  Can we continue? */
      if ((lim - pos) * direction < 0)
	return (0 - n) * direction;
    }
  return bytebpos_to_charbpos (buf, pos);
}

/* Record the whole-match data (beginning BEG and end BEG + LEN) and the
   buffer for a match just found.  */

static void
set_search_regs (struct buffer *buf, Charbpos beg, Charcount len)
{
  /* Make sure we have registers in which to store
     the match position.  */
  if (search_regs.num_regs == 0)
    {
      search_regs.start = xnew (regoff_t);
      search_regs.end   = xnew (regoff_t);
      search_regs.num_regs = 1;
    }

  clear_search_regs ();
  search_regs.start[0] = beg;
  search_regs.end[0] = beg + len;
  last_thing_searched = wrap_buffer (buf);
}

/* Clear search registers so match data will be null. */

static void
clear_search_regs (void)
{
  /* This function has been Mule-ized. */
  int i;

  for (i = 0; i < search_regs.num_regs; i++)
    search_regs.start[i] = search_regs.end[i] = -1;
}


/* Given a string of words separated by word delimiters,
   compute a regexp that matches those exact words
   separated by arbitrary punctuation.  */

static Lisp_Object
wordify (Lisp_Object buffer, Lisp_Object string)
{
  Charcount i, len;
  EMACS_INT punct_count = 0, word_count = 0;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object syntax_table = buf->mirror_syntax_table;

  CHECK_STRING (string);
  len = string_char_length (string);

  for (i = 0; i < len; i++)
    if (!WORD_SYNTAX_P (syntax_table, string_ichar (string, i)))
      {
	punct_count++;
	if (i > 0 && WORD_SYNTAX_P (syntax_table,
				    string_ichar (string, i - 1)))
          word_count++;
      }
  if (WORD_SYNTAX_P (syntax_table, string_ichar (string, len - 1)))
    word_count++;
  if (!word_count) return build_string ("");

  {
    /* The following value is an upper bound on the amount of storage we
       need.  In non-Mule, it is exact. */
    Ibyte *storage =
      alloca_ibytes (XSTRING_LENGTH (string) - punct_count +
                          5 * (word_count - 1) + 4);
    Ibyte *o = storage;

    *o++ = '\\';
    *o++ = 'b';

    for (i = 0; i < len; i++)
      {
	Ichar ch = string_ichar (string, i);

	if (WORD_SYNTAX_P (syntax_table, ch))
	  o += set_itext_ichar (o, ch);
	else if (i > 0
		 && WORD_SYNTAX_P (syntax_table,
				   string_ichar (string, i - 1))
		 && --word_count)
	  {
	    *o++ = '\\';
	    *o++ = 'W';
	    *o++ = '\\';
	    *o++ = 'W';
	    *o++ = '*';
	  }
      }

    *o++ = '\\';
    *o++ = 'b';

    return make_string (storage, o - storage);
  }
}

DEFUN ("search-backward", Fsearch_backward, 1, 5, "sSearch backward: ", /*
Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend before that position.
The value nil is equivalent to (point-min).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (string, limit, noerror, count, buffer))
{
  return search_command (string, limit, noerror, count, buffer, -1, 0, 0);
}

DEFUN ("search-forward", Fsearch_forward, 1, 5, "sSearch: ", /*
Search forward from point for STRING.
Set point to the end of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend after that position.  The
value nil is equivalent to (point-max).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (string, limit, noerror, count, buffer))
{
  return search_command (string, limit, noerror, count, buffer, 1, 0, 0);
}

DEFUN ("word-search-backward", Fword_search_backward, 1, 5,
       "sWord search backward: ", /*
Search backward from point for STRING, ignoring differences in punctuation.
Set point to the beginning of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend before that position.
The value nil is equivalent to (point-min).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (string, limit, noerror, count, buffer))
{
  return search_command (wordify (buffer, string), limit, noerror, count,
			 buffer, -1, 1, 0);
}

DEFUN ("word-search-forward", Fword_search_forward, 1, 5, "sWord search: ", /*
Search forward from point for STRING, ignoring differences in punctuation.
Set point to the end of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend after that position.  The
value nil is equivalent to (point-max).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (string, limit, noerror, count, buffer))
{
  return search_command (wordify (buffer, string), limit, noerror, count,
			 buffer, 1, 1, 0);
}

DEFUN ("re-search-backward", Fre_search_backward, 1, 5,
       "sRE search backward: ", /*
Search backward from point for match for regular expression REGEXP.
Set point to the beginning of the match, and return point.
The match found is the one starting last in the buffer
and yet ending before the origin of the search.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend before that position.
The value nil is equivalent to (point-min).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (regexp, limit, noerror, count, buffer))
{
  return search_command (regexp, limit, noerror, count, buffer, -1, 1, 0);
}

DEFUN ("re-search-forward", Fre_search_forward, 1, 5, "sRE search: ", /*
Search forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend after that position.  The
value nil is equivalent to (point-max).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (regexp, limit, noerror, count, buffer))
{
  return search_command (regexp, limit, noerror, count, buffer, 1, 1, 0);
}

DEFUN ("posix-search-backward", Fposix_search_backward, 1, 5,
       "sPosix search backward: ", /*
Search backward from point for match for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the beginning of the match, and return point.
The match found is the one starting last in the buffer
and yet ending before the origin of the search.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend before that position.
The value nil is equivalent to (point-min).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (regexp, limit, noerror, count, buffer))
{
  return search_command (regexp, limit, noerror, count, buffer, -1, 1, 1);
}

DEFUN ("posix-search-forward", Fposix_search_forward, 1, 5, "sPosix search: ", /*
Search forward from point for regular expression REGEXP.
Find the longest match in accord with Posix regular expression rules.
Set point to the end of the occurrence found, and return point.

Optional second argument LIMIT bounds the search; it is a buffer
position.  The match found must not extend after that position.  The
value nil is equivalent to (point-max).

Optional third argument NOERROR, if t, means just return nil (no
error) if the search fails.  If neither nil nor t, set point to LIMIT
and return nil.

Optional fourth argument COUNT is a repeat count--search for
successive occurrences.

Optional fifth argument BUFFER specifies the buffer to search in and
defaults to the current buffer.

When the match is successful, this function modifies the match data
that `match-beginning', `match-end' and `match-data' access; save the
match data with `match-data' and restore it with `store-match-data' if
you want to preserve them.  If the match fails, the match data from the
previous success match is preserved.

See also the function `replace-match'.
*/
       (regexp, limit, noerror, count, buffer))
{
  return search_command (regexp, limit, noerror, count, buffer, 1, 1, 1);
}


static Lisp_Object
free_created_dynarrs (Lisp_Object cons)
{
  Dynarr_free (get_opaque_ptr (XCAR (cons)));
  Dynarr_free (get_opaque_ptr (XCDR (cons)));
  free_opaque_ptr (XCAR (cons));
  free_opaque_ptr (XCDR (cons));
  free_cons (cons);
  return Qnil;
}

DEFUN ("replace-match", Freplace_match, 1, 5, 0, /*
Replace text matched by last search with REPLACEMENT.
Leaves point at end of replacement text.
Optional boolean FIXEDCASE inhibits matching case of REPLACEMENT to source.
Optional boolean LITERAL inhibits interpretation of escape sequences.
Optional STRING provides the source text to replace.
Optional STRBUFFER may be a buffer, providing match context, or an integer
 specifying the subexpression to replace.

If FIXEDCASE is non-nil, do not alter case of replacement text.
Otherwise maybe capitalize the whole text, or maybe just word initials,
based on the replaced text.
If the replaced text has only capital letters and has at least one
multiletter word, convert REPLACEMENT to all caps.
If the replaced text has at least one word starting with a capital letter,
then capitalize each word in REPLACEMENT.

If LITERAL is non-nil, insert REPLACEMENT literally.
Otherwise treat `\\' as special:
  `\\&' in REPLACEMENT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'.
  Case changes made with `\\u', `\\l', `\\U', and `\\L' override
  all other case changes that may be made in the replaced text.

If non-nil, STRING is the source string, and a new string with the specified
replacements is created and returned.  Otherwise the current buffer is the
source text.

If non-nil, STRBUFFER may be an integer, interpreted as the index of the
subexpression to replace in the source text, or a buffer to provide the
syntax table and case table.  If nil, then the \"subexpression\" is 0, i.e.,
the whole match, and the current buffer provides the syntax and case tables.
If STRING is nil, STRBUFFER must be nil or an integer.

Specifying a subexpression is only useful after a regular expression match,
since a fixed string search has no non-trivial subexpressions.

It is not possible to specify both a buffer and a subexpression.  If that is
desired, the idiom `(with-current-buffer BUFFER (replace-match ... INTEGER))'
may be appropriate.

If STRING is nil but the last thing matched (or searched) was a string, or
STRING is a string but the last thing matched was a buffer, an
`invalid-argument' error will be signaled.  (XEmacs does not check that the
last thing searched is the source string, but it is not useful to use a
different string as source.)

If no match (including searches) has been successful or the requested
subexpression was not matched, an `args-out-of-range' error will be
signaled.  (If no match has ever been conducted in this instance of
XEmacs, an `invalid-operation' error will be signaled.  This is very
rare.)
*/
       (replacement, fixedcase, literal, string, strbuffer))
{
  /* This function can GC */
  enum { nochange, all_caps, cap_initial } case_action;
  Charbpos pos, last;
  int some_multiletter_word;
  int some_lowercase;
  int some_uppercase;
  int some_nonuppercase_initial;
  Ichar c, prevc;
  Charcount inslen;
  struct buffer *buf;
  Lisp_Object syntax_table;
  int mc_count;
  Lisp_Object buffer;
  int_dynarr *ul_action_dynarr = 0;
  int_dynarr *ul_pos_dynarr = 0;
  int sub = 0;
  int speccount;

  CHECK_STRING (replacement);

  /* Because GNU decided to be incompatible here, we support the following
     baroque and bogus API for the STRING and STRBUFFER arguments:
          types            interpretations
     STRING   STRBUFFER   STRING   STRBUFFER
     nil      nil         none     0 = index of subexpression to replace
     nil      integer     none     index of subexpression to replace
     nil      other       ***** error *****
     string   nil         source   current buffer provides syntax table
                                   subexpression = 0 (whole match)
     string   buffer      source   buffer providing syntax table
                                   subexpression = 0 (whole match)
     string   integer     source   current buffer provides syntax table
                                   subexpression = STRBUFFER
     string   other       ***** error *****
  */

  /* Do STRBUFFER first; if STRING is nil, we'll overwrite BUF and BUFFER. */

  /* If the match data were abstracted into a special "match data" type
     instead of the typical half-assed "let the implementation be visible"
     form it's in, we could extend it to include the last string matched
     and the buffer used for that matching.  But of course we can't change
     it as it is.
  */
  if (NILP (strbuffer) || BUFFERP (strbuffer))
    {
      buf = decode_buffer (strbuffer, 0);
    }
  else if (!NILP (strbuffer))
    {
      CHECK_INT (strbuffer);
      sub = XINT (strbuffer);
      if (sub < 0 || sub >= (int) search_regs.num_regs)
	invalid_argument ("match data register invalid", strbuffer);
      if (search_regs.start[sub] < 0)
	invalid_argument ("match data register not set", strbuffer);
      buf = current_buffer;
    }
  else
    invalid_argument ("STRBUFFER must be nil, a buffer, or an integer",
		      strbuffer);
  buffer = wrap_buffer (buf);

  if (! NILP (string))
    {
      CHECK_STRING (string);
      if (!EQ (last_thing_searched, Qt))
	invalid_argument ("last thing matched was not a string", Qunbound);
    }
  else
    {
      if (!BUFFERP (last_thing_searched))
	invalid_argument ("last thing matched was not a buffer", Qunbound);
      buffer = last_thing_searched;
      buf = XBUFFER (buffer);
    }

  syntax_table = buf->mirror_syntax_table;

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */

  if (search_regs.num_regs == 0)
    signal_error (Qinvalid_operation,
		  "replace-match called before any match found", Qunbound);

  if (NILP (string))
    {
      if (search_regs.start[sub] < BUF_BEGV (buf)
	  || search_regs.start[sub] > search_regs.end[sub]
	  || search_regs.end[sub] > BUF_ZV (buf))
	args_out_of_range (make_int (search_regs.start[sub]),
			   make_int (search_regs.end[sub]));
    }
  else
    {
      if (search_regs.start[0] < 0
	  || search_regs.start[0] > search_regs.end[0]
	  || search_regs.end[0] > string_char_length (string))
	args_out_of_range (make_int (search_regs.start[0]),
			   make_int (search_regs.end[0]));
    }

  if (NILP (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */

      last = search_regs.end[sub];
      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;
      some_lowercase = 0;
      some_nonuppercase_initial = 0;
      some_uppercase = 0;

      for (pos = search_regs.start[sub]; pos < last; pos++)
	{
	  if (NILP (string))
	    c = BUF_FETCH_CHAR (buf, pos);
	  else
	    c = string_ichar (string, pos);

	  if (LOWERCASEP (buf, c))
	    {
	      /* Cannot be all caps if any original char is lower case */

	      some_lowercase = 1;
	      if (!WORD_SYNTAX_P (syntax_table, prevc))
		some_nonuppercase_initial = 1;
	      else
		some_multiletter_word = 1;
	    }
	  else if (!NOCASEP (buf, c))
	    {
	      some_uppercase = 1;
	      if (!WORD_SYNTAX_P (syntax_table, prevc))
		;
	      else
		some_multiletter_word = 1;
	    }
	  else
	    {
	      /* If the initial is a caseless word constituent,
		 treat that like a lowercase initial.  */
	      if (!WORD_SYNTAX_P (syntax_table, prevc))
		some_nonuppercase_initial = 1;
	    }

	  prevc = c;
	}

      /* Convert to all caps if the old text is all caps
	 and has at least one multiletter word.  */
      if (! some_lowercase && some_multiletter_word)
	case_action = all_caps;
      /* Capitalize each word, if the old text has all capitalized words.  */
      else if (!some_nonuppercase_initial && some_multiletter_word)
	case_action = cap_initial;
      else if (!some_nonuppercase_initial && some_uppercase)
	/* Should x -> yz, operating on X, give Yz or YZ?
	   We'll assume the latter.  */
	case_action = all_caps;
      else
	case_action = nochange;
    }

  /* Do replacement in a string.  */
  if (!NILP (string))
    {
      Lisp_Object before, after;

      speccount = specpdl_depth ();
      before = Fsubstring (string, Qzero, make_int (search_regs.start[sub]));
      after = Fsubstring (string, make_int (search_regs.end[sub]), Qnil);

      /* Do case substitution into REPLACEMENT if desired.  */
      if (NILP (literal))
	{
	  Charcount stlen = string_char_length (replacement);
	  Charcount strpos;
	  /* XEmacs change: rewrote this loop somewhat to make it
	     cleaner.  Also added \U, \E, etc. */
	  Charcount literal_start = 0;
	  /* We build up the substituted string in ACCUM.  */
	  Lisp_Object accum;

	  accum = Qnil;

	  /* OK, the basic idea here is that we scan through the
	     replacement string until we find a backslash, which
	     represents a substring of the original string to be
	     substituted.  We then append onto ACCUM the literal
	     text before the backslash (LASTPOS marks the
	     beginning of this) followed by the substring of the
	     original string that needs to be inserted. */
	  for (strpos = 0; strpos < stlen; strpos++)
	    {
	      /* If LITERAL_END is set, we've encountered a backslash
		 (the end of literal text to be inserted). */
	      Charcount literal_end = -1;
	      /* If SUBSTART is set, we need to also insert the
		 text from SUBSTART to SUBEND in the original string. */
	      Charcount substart = -1;
	      Charcount subend   = -1;

	      c = string_ichar (replacement, strpos);
	      if (c == '\\' && strpos < stlen - 1)
		{
		  c = string_ichar (replacement, ++strpos);
		  if (c == '&')
		    {
		      literal_end = strpos - 1;
		      substart = search_regs.start[0];
		      subend = search_regs.end[0];
		    }
		  /* #### This logic is totally broken,
		     since we can have backrefs like "\99", right? */
		  else if (c >= '1' && c <= '9' &&
			   c <= search_regs.num_regs + '0')
		    {
		      if (search_regs.start[c - '0'] >= 0)
			{
			  literal_end = strpos - 1;
			  substart = search_regs.start[c - '0'];
			  subend = search_regs.end[c - '0'];
			}
		    }
		  else if (c == 'U' || c == 'u' || c == 'L' || c == 'l' ||
			   c == 'E')
		    {
		      /* Keep track of all case changes requested, but don't
			 make them now.  Do them later so we override
			 everything else. */
		      if (!ul_pos_dynarr)
			{
			  ul_pos_dynarr = Dynarr_new (int);
			  ul_action_dynarr = Dynarr_new (int);
			  record_unwind_protect
			    (free_created_dynarrs,
			     noseeum_cons
			     (make_opaque_ptr (ul_pos_dynarr),
			      make_opaque_ptr (ul_action_dynarr)));
			}
		      literal_end = strpos - 1;
		      Dynarr_add (ul_pos_dynarr,
				  (!NILP (accum)
				  ? string_char_length (accum)
				  : 0) + (literal_end - literal_start));
		      Dynarr_add (ul_action_dynarr, c);
		    }
		  else if (c == '\\')
		    /* So we get just one backslash. */
		    literal_end = strpos;
		}
	      if (literal_end >= 0)
		{
		  Lisp_Object literal_text = Qnil;
		  Lisp_Object substring = Qnil;
		  if (literal_end != literal_start)
		    literal_text = Fsubstring (replacement,
					       make_int (literal_start),
					       make_int (literal_end));
		  if (substart >= 0 && subend != substart)
		    substring = Fsubstring (string,
					    make_int (substart),
					    make_int (subend));
		  if (!NILP (literal_text) || !NILP (substring))
		    accum = concat3 (accum, literal_text, substring);
		  literal_start = strpos + 1;
		}
	    }

	  if (strpos != literal_start)
	    /* some literal text at end to be inserted */
	    replacement = concat2 (accum, Fsubstring (replacement,
						      make_int (literal_start),
						      make_int (strpos)));
	  else
	    replacement = accum;
	}

      /* replacement can be nil. */
      if (NILP (replacement))
	replacement = build_string ("");

      if (case_action == all_caps)
	replacement = Fupcase (replacement, buffer);
      else if (case_action == cap_initial)
	replacement = Fupcase_initials (replacement, buffer);

      /* Now finally, we need to process the \U's, \E's, etc. */
      if (ul_pos_dynarr)
	{
	  int i = 0;
	  int cur_action = 'E';
	  Charcount stlen = string_char_length (replacement);
	  Charcount strpos;

	  for (strpos = 0; strpos < stlen; strpos++)
	    {
	      Ichar curchar = string_ichar (replacement, strpos);
	      Ichar newchar = -1;
	      if (i < Dynarr_length (ul_pos_dynarr) &&
		  strpos == Dynarr_at (ul_pos_dynarr, i))
		{
		  int new_action = Dynarr_at (ul_action_dynarr, i);
		  i++;
		  if (new_action == 'u')
		    newchar = UPCASE (buf, curchar);
		  else if (new_action == 'l')
		    newchar = DOWNCASE (buf, curchar);
		  else
		    cur_action = new_action;
		}
	      if (newchar == -1)
		{
		  if (cur_action == 'U')
		    newchar = UPCASE (buf, curchar);
		  else if (cur_action == 'L')
		    newchar = DOWNCASE (buf, curchar);
		  else
		    newchar = curchar;
		}
	      if (newchar != curchar)
		set_string_char (replacement, strpos, newchar);
	    }
	}

      /* frees the Dynarrs if necessary. */
      unbind_to (speccount);
      return concat3 (before, replacement, after);
    }

  mc_count = begin_multiple_change (buf, search_regs.start[sub],
				    search_regs.end[sub]);

  /* begin_multiple_change() records an unwind-protect, so we need to
     record this value now. */
  speccount = specpdl_depth ();

  /* We insert the replacement text before the old text, and then
     delete the original text.  This means that markers at the
     beginning or end of the original will float to the corresponding
     position in the replacement.  */
  BUF_SET_PT (buf, search_regs.start[sub]);
  if (!NILP (literal))
    Finsert (1, &replacement);
  else
    {
      Charcount stlen = string_char_length (replacement);
      Charcount strpos;
      struct gcpro gcpro1;
      GCPRO1 (replacement);
      for (strpos = 0; strpos < stlen; strpos++)
	{
	  /* on the first iteration assert(offset==0),
	     exactly complementing BUF_SET_PT() above.
	     During the loop, it keeps track of the amount inserted.
	   */
	  Charcount offset = BUF_PT (buf) - search_regs.start[sub];

	  c = string_ichar (replacement, strpos);
	  if (c == '\\' && strpos < stlen - 1)
	    {
	      /* XXX FIXME: replacing just a substring non-literally
		 using backslash refs to the match looks dangerous.  But
		 <15366.18513.698042.156573@ns.caldera.de> from Torsten Duwe
		 <duwe@caldera.de> claims Finsert_buffer_substring already
		 handles this correctly.
	      */
	      c = string_ichar (replacement, ++strpos);
	      if (c == '&')
		Finsert_buffer_substring
                  (buffer,
                   make_int (search_regs.start[0] + offset),
                   make_int (search_regs.end[0] + offset));
	      /* #### This logic is totally broken,
		 since we can have backrefs like "\99", right? */
	      else if (c >= '1' && c <= '9' &&
		       c <= search_regs.num_regs + '0')
		{
		  if (search_regs.start[c - '0'] >= 1)
		    Finsert_buffer_substring
                      (buffer,
                       make_int (search_regs.start[c - '0'] + offset),
                       make_int (search_regs.end[c - '0'] + offset));
		}
	      else if (c == 'U' || c == 'u' || c == 'L' || c == 'l' ||
		       c == 'E')
		{
		  /* Keep track of all case changes requested, but don't
		     make them now.  Do them later so we override
		     everything else. */
		  if (!ul_pos_dynarr)
		    {
		      ul_pos_dynarr = Dynarr_new (int);
		      ul_action_dynarr = Dynarr_new (int);
		      record_unwind_protect
			(free_created_dynarrs,
			 Fcons (make_opaque_ptr (ul_pos_dynarr),
				make_opaque_ptr (ul_action_dynarr)));
		    }
		  Dynarr_add (ul_pos_dynarr, BUF_PT (buf));
		  Dynarr_add (ul_action_dynarr, c);
		}
	      else
		buffer_insert_emacs_char (buf, c);
	    }
	  else
	    buffer_insert_emacs_char (buf, c);
	}
      UNGCPRO;
    }

  inslen = BUF_PT (buf) - (search_regs.start[sub]);
  buffer_delete_range (buf, search_regs.start[sub] + inslen,
		       search_regs.end[sub] +  inslen, 0);

  if (case_action == all_caps)
    Fupcase_region (make_int (BUF_PT (buf) - inslen),
		    make_int (BUF_PT (buf)),  buffer);
  else if (case_action == cap_initial)
    Fupcase_initials_region (make_int (BUF_PT (buf) - inslen),
			     make_int (BUF_PT (buf)), buffer);

  /* Now go through and make all the case changes that were requested
     in the replacement string. */
  if (ul_pos_dynarr)
    {
      Charbpos eend = BUF_PT (buf);
      int i = 0;
      int cur_action = 'E';

      for (pos = BUF_PT (buf) - inslen; pos < eend; pos++)
	{
	  Ichar curchar = BUF_FETCH_CHAR (buf, pos);
	  Ichar newchar = -1;
	  if (i < Dynarr_length (ul_pos_dynarr) &&
	      pos == Dynarr_at (ul_pos_dynarr, i))
	    {
	      int new_action = Dynarr_at (ul_action_dynarr, i);
	      i++;
	      if (new_action == 'u')
		newchar = UPCASE (buf, curchar);
	      else if (new_action == 'l')
		newchar = DOWNCASE (buf, curchar);
	      else
		cur_action = new_action;
	    }
	  if (newchar == -1)
	    {
	      if (cur_action == 'U')
		newchar = UPCASE (buf, curchar);
	      else if (cur_action == 'L')
		newchar = DOWNCASE (buf, curchar);
	      else
		newchar = curchar;
	    }
	  if (newchar != curchar)
	    buffer_replace_char (buf, pos, newchar, 0, 0);
	}
    }

  /* frees the Dynarrs if necessary. */
  unbind_to (speccount);
  end_multiple_change (buf, mc_count);

  return Qnil;
}

static Lisp_Object
match_limit (Lisp_Object num, int beginningp)
{
  int n;

  CHECK_INT (num);
  n = XINT (num);
  if (n < 0 || n >= search_regs.num_regs)
    args_out_of_range (num, make_int (search_regs.num_regs));
  if (search_regs.num_regs == 0 ||
      search_regs.start[n] < 0)
    return Qnil;
  return make_int (beginningp ? search_regs.start[n] : search_regs.end[n]);
}

DEFUN ("match-beginning", Fmatch_beginning, 1, 1, 0, /*
Return position of start of text matched by last regexp search.
NUM, specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
*/
       (num))
{
  return match_limit (num, 1);
}

DEFUN ("match-end", Fmatch_end, 1, 1, 0, /*
Return position of end of text matched by last regexp search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
*/
       (num))
{
  return match_limit (num, 0);
}

DEFUN ("match-data", Fmatch_data, 0, 2, 0, /*
Return a list containing all info on what the last regexp search matched.
Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.
All the elements are markers or nil (nil if the Nth pair didn't match)
if the last match was on a buffer; integers or nil if a string was matched.
Use `store-match-data' to reinstate the data in this list.

If INTEGERS (the optional first argument) is non-nil, always use integers
\(rather than markers) to represent buffer positions.
If REUSE is a list, reuse it as part of the value.  If REUSE is long enough
to hold all the values, and if INTEGERS is non-nil, no consing is done.
*/
       (integers, reuse))
{
  Lisp_Object tail, prev;
  Lisp_Object *data;
  int i;
  Charcount len;

  if (NILP (last_thing_searched))
    /*error ("match-data called before any match found", Qunbound);*/
    return Qnil;

  data = alloca_array (Lisp_Object, 2 * search_regs.num_regs);

  len = -1;
  for (i = 0; i < search_regs.num_regs; i++)
    {
      Charbpos start = search_regs.start[i];
      if (start >= 0)
	{
	  if (EQ (last_thing_searched, Qt)
	      || !NILP (integers))
	    {
	      data[2 * i] = make_int (start);
	      data[2 * i + 1] = make_int (search_regs.end[i]);
	    }
	  else if (BUFFERP (last_thing_searched))
	    {
	      data[2 * i] = Fmake_marker ();
	      Fset_marker (data[2 * i],
			   make_int (start),
			   last_thing_searched);
	      data[2 * i + 1] = Fmake_marker ();
	      Fset_marker (data[2 * i + 1],
			   make_int (search_regs.end[i]),
			   last_thing_searched);
	    }
	  else
	    /* last_thing_searched must always be Qt, a buffer, or Qnil.  */
	    ABORT ();

	  len = i;
	}
      else
	data[2 * i] = data [2 * i + 1] = Qnil;
    }
  if (!CONSP (reuse))
    return Flist (2 * len + 2, data);

  /* If REUSE is a list, store as many value elements as will fit
     into the elements of REUSE.  */
  for (prev = Qnil, i = 0, tail = reuse; CONSP (tail); i++, tail = XCDR (tail))
    {
      if (i < 2 * len + 2)
	XCAR (tail) = data[i];
      else
	XCAR (tail) = Qnil;
      prev = tail;
    }

  /* If we couldn't fit all value elements into REUSE,
     cons up the rest of them and add them to the end of REUSE.  */
  if (i < 2 * len + 2)
    XCDR (prev) = Flist (2 * len + 2 - i, data + i);

  return reuse;
}


DEFUN ("store-match-data", Fstore_match_data, 1, 1, 0, /*
Set internal data on last search match from elements of LIST.
LIST should have been created by calling `match-data' previously,
or be nil, to clear the internal match data.
*/
       (list))
{
  REGISTER int i;
  REGISTER Lisp_Object marker;
  int num_regs;
  int length;

  /* Some FSF junk with running_asynch_code, to preserve the match
     data.  Not necessary because we don't call process filters
     asynchronously (i.e. from within QUIT). */

  CONCHECK_LIST (list);

  /* Unless we find a marker with a buffer in LIST, assume that this
     match data came from a string.  */
  last_thing_searched = Qt;

  /* Allocate registers if they don't already exist.  */
  length = XINT (Flength (list)) / 2;
  num_regs = search_regs.num_regs;

  if (length > num_regs)
    {
      if (search_regs.num_regs == 0)
	{
	  search_regs.start = xnew_array (regoff_t, length);
	  search_regs.end   = xnew_array (regoff_t, length);
	}
      else
	{
	  XREALLOC_ARRAY (search_regs.start, regoff_t, length);
	  XREALLOC_ARRAY (search_regs.end,   regoff_t, length);
	}

      search_regs.num_regs = length;
    }

  for (i = 0; i < num_regs; i++)
    {
      marker = Fcar (list);
      if (NILP (marker))
	{
	  search_regs.start[i] = -1;
	  list = Fcdr (list);
	}
      else
	{
	  if (MARKERP (marker))
	    {
	      if (XMARKER (marker)->buffer == 0)
		marker = Qzero;
	      else
		last_thing_searched = wrap_buffer (XMARKER (marker)->buffer);
	    }

	  CHECK_INT_COERCE_MARKER (marker);
	  search_regs.start[i] = XINT (marker);
	  list = Fcdr (list);

	  marker = Fcar (list);
	  if (MARKERP (marker) && XMARKER (marker)->buffer == 0)
	    marker = Qzero;

	  CHECK_INT_COERCE_MARKER (marker);
	  search_regs.end[i] = XINT (marker);
	}
      list = Fcdr (list);
    }

  return Qnil;
}

/* Quote a string to inactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, 1, 1, 0, /*
Return a regexp string which matches exactly STRING and nothing else.
*/
       (string))
{
  REGISTER Ibyte *in, *out, *end;
  REGISTER Ibyte *temp;

  CHECK_STRING (string);

  temp = alloca_ibytes (XSTRING_LENGTH (string) * 2);

  /* Now copy the data into the new string, inserting escapes. */

  in = XSTRING_DATA (string);
  end = in + XSTRING_LENGTH (string);
  out = temp;

  while (in < end)
    {
      Ichar c = itext_ichar (in);

      if (c == '[' || c == ']'
	  || c == '*' || c == '.' || c == '\\'
	  || c == '?' || c == '+'
	  || c == '^' || c == '$')
	*out++ = '\\';
      out += set_itext_ichar (out, c);
      INC_IBYTEPTR (in);
    }

  return make_string (temp, out - temp);
}

DEFUN ("set-word-regexp", Fset_word_regexp, 1, 1, 0, /*
Set the regexp to be used to match a word in regular-expression searching.
#### Not yet implemented.  Currently does nothing.
#### Do not use this yet.  Its calling interface is likely to change.
*/
       (UNUSED (regexp)))
{
  return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_search (void)
{

  DEFERROR_STANDARD (Qsearch_failed, Qinvalid_operation);
  DEFERROR_STANDARD (Qinvalid_regexp, Qsyntax_error);
  Fput (Qinvalid_regexp, Qerror_lacks_explanatory_string, Qt);

  DEFSUBR (Flooking_at);
  DEFSUBR (Fposix_looking_at);
  DEFSUBR (Fstring_match);
  DEFSUBR (Fposix_string_match);
  DEFSUBR (Fskip_chars_forward);
  DEFSUBR (Fskip_chars_backward);
  DEFSUBR (Fskip_syntax_forward);
  DEFSUBR (Fskip_syntax_backward);
  DEFSUBR (Fsearch_forward);
  DEFSUBR (Fsearch_backward);
  DEFSUBR (Fword_search_forward);
  DEFSUBR (Fword_search_backward);
  DEFSUBR (Fre_search_forward);
  DEFSUBR (Fre_search_backward);
  DEFSUBR (Fposix_search_forward);
  DEFSUBR (Fposix_search_backward);
  DEFSUBR (Freplace_match);
  DEFSUBR (Fmatch_beginning);
  DEFSUBR (Fmatch_end);
  DEFSUBR (Fmatch_data);
  DEFSUBR (Fstore_match_data);
  DEFSUBR (Fregexp_quote);
  DEFSUBR (Fset_word_regexp);
}

void
reinit_vars_of_search (void)
{
  int i;

  last_thing_searched = Qnil;
  staticpro_nodump (&last_thing_searched);

  for (i = 0; i < REGEXP_CACHE_SIZE; ++i)
    {
      searchbufs[i].buf.allocated = 100;
      searchbufs[i].buf.buffer = (unsigned char *) xmalloc (100);
      searchbufs[i].buf.fastmap = searchbufs[i].fastmap;
      searchbufs[i].regexp = Qnil;
      staticpro_nodump (&searchbufs[i].regexp);
      searchbufs[i].next = (i == REGEXP_CACHE_SIZE-1 ? 0 : &searchbufs[i+1]);
    }
  searchbuf_head = &searchbufs[0];
}

void
vars_of_search (void)
{
  DEFVAR_LISP ("forward-word-regexp", &Vforward_word_regexp /*
*Regular expression to be used in `forward-word'.
#### Not yet implemented.
*/ );
  Vforward_word_regexp = Qnil;

  DEFVAR_LISP ("backward-word-regexp", &Vbackward_word_regexp /*
*Regular expression to be used in `backward-word'.
#### Not yet implemented.
*/ );
  Vbackward_word_regexp = Qnil;

  DEFVAR_INT ("warn-about-possibly-incompatible-back-references",
	      &warn_about_possibly_incompatible_back_references /*
If true, issue warnings when new-semantics back references occur.
This is to catch places where old code might inadvertently have changed
semantics.  This will occur in old code only where more than nine groups
occur and a back reference to one of them is directly followed by a digit.
*/ );
  warn_about_possibly_incompatible_back_references = 1;

  Vskip_chars_range_table = Fmake_range_table (Qstart_closed_end_closed);
  staticpro (&Vskip_chars_range_table);
}
