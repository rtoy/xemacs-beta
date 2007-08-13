/* Primitives for word-abbrev mode.
   Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.30.  Note that there are many more functions in
   FSF's abbrev.c.  These have been moved into Lisp in XEmacs. */

/* Authorship:

   FSF: Original version; a long time ago.
   JWZ or Mly: Mostly moved into Lisp; maybe 1992.
   Ben Wing: Some changes for Mule for 19.12.
   Hrvoje Niksic: Largely rewritten in June 1997.
*/

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "insdel.h"
#include "syntax.h"
#include "window.h"

/* An abbrev table is an obarray.
   Each defined abbrev is represented by a symbol in that obarray
   whose print name is the abbreviation.
   The symbol's value is a string which is the expansion.
   If its function definition is non-nil, it is called
   after the expansion is done.
   The plist slot of the abbrev symbol is its usage count. */

/* The table of global abbrevs.  These are in effect
   in any buffer in which abbrev mode is turned on. */
Lisp_Object Vglobal_abbrev_table;

int abbrev_all_caps;

/* Non-nil => use this location as the start of abbrev to expand
 (rather than taking the word before point as the abbrev) */
Lisp_Object Vabbrev_start_location;

/* Buffer that Vabbrev_start_location applies to */
Lisp_Object Vabbrev_start_location_buffer;

/* The symbol representing the abbrev most recently expanded */
Lisp_Object Vlast_abbrev;

/* A string for the actual text of the abbrev most recently expanded.
   This has more info than Vlast_abbrev since case is significant.  */
Lisp_Object Vlast_abbrev_text;

/* Character address of start of last abbrev expanded */
int last_abbrev_point;

Lisp_Object oblookup (Lisp_Object, CONST Bufbyte *, Bytecount);

/* Hook to run before expanding any abbrev.  */
Lisp_Object Vpre_abbrev_expand_hook, Qpre_abbrev_expand_hook;


/* Match the buffer text against names of symbols in obarray.  Returns
   the matching symbol, or 0 if not found.  */

static struct Lisp_Symbol *
abbrev_match (struct buffer *buf, Lisp_Object obarray)
{
  Bufpos point = BUF_PT (buf);
  Bufpos maxlen = point - BUF_BEGV (buf);
  Charcount idx;

  struct Lisp_Char_Table *chartab = XCHAR_TABLE (buf->mirror_syntax_table);
  struct Lisp_String *abbrev;
  struct Lisp_Vector *obvec;
  struct Lisp_Symbol *sym;
  Charcount abbrev_length;
  Lisp_Object tail;
  int i, found;

  CHECK_VECTOR (obarray);
  obvec = XVECTOR (obarray);

  /* The obarray-traversing code is copied from `map_obarray'. */
  found = 0;
  for (i = vector_length (obvec) - 1; i >= 0; i--)
    {
      tail = vector_data (obvec)[i];
      if (SYMBOLP (tail))
	while (1)
	  {
	    sym = XSYMBOL (tail);
	    if (UNBOUNDP (symbol_value (sym)) || NILP (symbol_value (sym)))
	      {
		/* The symbol value of nil means that abbrev got
                   undefined. */
		goto next;
	      }
	    abbrev = symbol_name (sym);
	    abbrev_length = string_char_length (abbrev);
	    if (abbrev_length > maxlen)
	      {
		/* This abbrev is too large -- it wouldn't fit. */
		goto next;
	      }
	    /* If `bar' is an abbrev, and a user presses `fubar<SPC>',
	       we don't normally want to expand it.  OTOH, if the
	       abbrev begins with non-word syntax, it is OK to
	       abbreviate it anywhere.  */
	    if (abbrev_length < maxlen && abbrev_length > 0
		&& (WORD_SYNTAX_P (chartab, string_char (abbrev, 0)))
		&& (WORD_SYNTAX_P (chartab,
				   BUF_FETCH_CHAR (buf, point
						   - (abbrev_length + 1)))))
	      {
		goto next;
	      }
	    /* Match abbreviation string against buffer text.  */
	    for (idx = abbrev_length - 1; idx >= 0; idx--)
	      {
		if (DOWNCASE (buf, BUF_FETCH_CHAR (buf, point -
						   (abbrev_length - idx)))
		    != DOWNCASE (buf, string_char (abbrev, idx)))
		  break;
	      }
	    if (idx < 0)
	      {
		found = 1;
		break;
	      }
	  next:
	    sym = symbol_next (XSYMBOL (tail));
	    if (!sym)
	      break;
	    XSETSYMBOL (tail, sym);
	  } /* while */
      if (found)
	break;
    } /* for */

  return found ? sym : 0;
}

/* Take the word before point, and look it up in OBARRAY, and return
   the symbol (or nil).  This used to be the default method of
   searching, with the obvious limitation that the abbrevs may consist
   only of word characters.  It is an order of magnitued faster than
   the proper `abbrev_match', but then again, vi is an order of
   magnitude faster than Emacs.  */
static struct Lisp_Symbol *
abbrev_oblookup (struct buffer *buf, Lisp_Object obarray)
{
  Bufpos wordstart, wordend;
  Bufbyte *word, *p;
  Bytecount idx;
  Lisp_Object lookup;

  CHECK_VECTOR (obarray);

  if (!NILP (Vabbrev_start_location))
    {
      wordstart = get_buffer_pos_char (buf, Vabbrev_start_location,
				       GB_COERCE_RANGE);
      Vabbrev_start_location = Qnil;
      if (wordstart != BUF_ZV (buf)
	  && BUF_FETCH_CHAR (buf, wordstart) == '-')
	{
	  buffer_delete_range (buf, wordstart, wordstart + 1, 0);
	}
      wordend = BUF_PT (buf);
    }
  else
    {
      Bufpos point = BUF_PT (buf);

      wordstart = scan_words (buf, point, -1);
      if (!wordstart)
	return 0;

      wordend = scan_words (buf, wordstart, 1);
      if (!wordend)
	return 0;
      if (wordend > BUF_ZV (buf))
	wordend = BUF_ZV (buf);
      if (wordend > point)
	wordend = point;
      /* Unlike the original function, we allow expansion only after
	 the abbrev, not preceded by a number of spaces.  This is
	 because of consistency with abbrev_match. */
      if (wordend < point)
	return 0;
      if (wordend <= wordstart)
	return 0;
    }

  p = word = (Bufbyte *) alloca (MAX_EMCHAR_LEN * (wordend - wordstart));
  for (idx = wordstart; idx < wordend; idx++)
    {
      Emchar c = BUF_FETCH_CHAR (buf, idx);
      if (UPPERCASEP (buf, c))
	c = DOWNCASE (buf, c);
      p += set_charptr_emchar (p, c);
    }
  lookup = oblookup (obarray, word, p - word);
  if (SYMBOLP (lookup) && !NILP (symbol_value (XSYMBOL (lookup))))
    return XSYMBOL (lookup);
  else
    return NULL;
}

/* Return non-zero if OBARRAY contains an interned symbol ` '. */
static int
obarray_has_blank_p (Lisp_Object obarray)
{
  Lisp_Object lookup;

  lookup = oblookup (obarray, (Bufbyte *)" ", 1);
  return SYMBOLP (lookup);
}

/* Analyze case in the buffer substring, and report it.  */
static void
abbrev_count_case (struct buffer *buf, Bufpos pos, Charcount length,
		   int *lccount, int *uccount)
{
  Emchar c;

  *lccount = *uccount = 0;
  while (length--)
    {
      c = BUF_FETCH_CHAR (buf, pos);
      if (UPPERCASEP (buf, c))
	++*uccount;
      else if (LOWERCASEP (buf, c))
	++*lccount;
      ++pos;
    }
}

DEFUN ("expand-abbrev", Fexpand_abbrev, 0, 0, "", /*
Expand the abbrev before point, if any.
Effective when explicitly called even when `abbrev-mode' is nil.
Returns t if expansion took place.
*/
       ())
{
  /* This function can GC */
  struct buffer *buf = current_buffer;
  int oldmodiff = BUF_MODIFF (buf);
  Lisp_Object pre_modiff_p;
  Bufpos point;			/* position of point */
  Bufpos abbrev_start;		/* position of abbreviation beginning */

  struct Lisp_Symbol *(*fun) (struct buffer *, Lisp_Object);

  struct Lisp_Symbol *abbrev_symbol;
  struct Lisp_String *abbrev_string;
  Lisp_Object expansion, count, hook;
  Charcount abbrev_length, idx;
  int lccount, uccount;

  run_hook (Qpre_abbrev_expand_hook);
  /* If the hook changes the buffer, treat that as having "done an
     expansion".  */
  pre_modiff_p = (BUF_MODIFF (buf) != oldmodiff ? Qt : Qnil);

  abbrev_symbol = NULL;
  if (!BUFFERP (Vabbrev_start_location_buffer) ||
      XBUFFER (Vabbrev_start_location_buffer) != buf)
    Vabbrev_start_location = Qnil;
  /* We use the more general `abbrev_match' if the obarray blank flag
     is not set, and Vabbrev_start_location is nil.  Otherwise, use
     `abbrev_oblookup'. */
#define MATCHFUN(tbl) ((obarray_has_blank_p (tbl)		 \
			&& NILP (Vabbrev_start_location))	 \
		       ? abbrev_match : abbrev_oblookup)
  if (!NILP (buf->abbrev_table))
    {
      fun = MATCHFUN (buf->abbrev_table);
      abbrev_symbol = fun (buf, buf->abbrev_table);
    }
  if (!abbrev_symbol && !NILP (Vglobal_abbrev_table))
    {
      fun = MATCHFUN (Vglobal_abbrev_table);
      abbrev_symbol = fun (buf, Vglobal_abbrev_table);
    }
  if (!abbrev_symbol)
    return pre_modiff_p;

  /* NOTE: we hope that `pre-abbrev-expand-hook' didn't do something
     nasty, such as changed (or killed) the buffer.  */
  point = BUF_PT (buf);

  /* OK, we're out of the must-be-fast part.  An abbreviation matched.
     Now find the parameters, insert the expansion, and make it all
     look pretty. */
  abbrev_string = symbol_name (abbrev_symbol);
  abbrev_length = string_char_length (abbrev_string);
  abbrev_start = point - abbrev_length;

  expansion = symbol_value (abbrev_symbol);
  CHECK_STRING (expansion);

  count = symbol_plist (abbrev_symbol); /* Gag */
  if (NILP (count))
    count = make_int (0);
  else
    CHECK_NATNUM (count);
  symbol_plist (abbrev_symbol) = make_int (1 + XINT (count));

  /* Count the case in the original text. */
  abbrev_count_case (buf, abbrev_start, abbrev_length, &lccount, &uccount);

  /* Remember the last abbrev text, location, etc. */
  XSETSYMBOL (Vlast_abbrev, abbrev_symbol);
  Vlast_abbrev_text =
    make_string_from_buffer (buf, abbrev_start, abbrev_length);
  last_abbrev_point = abbrev_start;

  /* Add an undo boundary, in case we are doing this for a
     self-inserting command which has avoided making one so far.  */
  if (INTERACTIVE)
    Fundo_boundary ();

  /* Remove the abbrev */
  buffer_delete_range (buf, abbrev_start, point, 0);
  /* And insert the expansion. */
  buffer_insert_lisp_string (buf, expansion);
  point = BUF_PT (buf);

  /* Now fiddle with the case. */
  if (uccount && !lccount)
    {
      /* Abbrev was all caps */
      if (!abbrev_all_caps
	  && scan_words (buf, point, -1) > scan_words (buf, abbrev_start, 1))
	{
	  Fupcase_initials_region (make_int (abbrev_start), make_int (point),
				   make_buffer (buf));
	}
      else
	{
	  /* If expansion is one word, or if user says so, upcase it all. */
	  Fupcase_region (make_int (abbrev_start), make_int (point),
			  make_buffer (buf));
	}
    }
  else if (uccount)
    {
      /* Abbrev included some caps.  Cap first initial of expansion */
      Bufpos pos = abbrev_start;
      /* Find the initial.  */
      while (pos < point
	     && !WORD_SYNTAX_P (XCHAR_TABLE (buf->mirror_syntax_table),
				BUF_FETCH_CHAR (buf, pos)))
	pos++;
      /* Change just that.  */
      Fupcase_initials_region (make_int (pos), make_int (pos + 1),
			       make_buffer (buf));
    }

  hook = symbol_function (abbrev_symbol);
  if (!NILP (hook) && !UNBOUNDP (hook))
    call0 (hook);

  return Qt;
}


void
syms_of_abbrev (void)
{
  defsymbol (&Qpre_abbrev_expand_hook, "pre-abbrev-expand-hook");
  DEFSUBR (Fexpand_abbrev);
}

void
vars_of_abbrev (void)
{
  DEFVAR_LISP ("global-abbrev-table", &Vglobal_abbrev_table /*
The abbrev table whose abbrevs affect all buffers.
Each buffer may also have a local abbrev table.
If it does, the local table overrides the global one
for any particular abbrev defined in both.
*/ );
  Vglobal_abbrev_table = Qnil;  /* setup by Lisp code */

  DEFVAR_LISP ("last-abbrev", &Vlast_abbrev /*
The abbrev-symbol of the last abbrev expanded.
See the function `abbrev-symbol'.
*/ );

  DEFVAR_LISP ("last-abbrev-text", &Vlast_abbrev_text /*
The exact text of the last abbrev expanded.
nil if the abbrev has already been unexpanded.
*/ );

  DEFVAR_INT ("last-abbrev-location", &last_abbrev_point /*
The location of the start of the last abbrev expanded.
*/ );

  Vlast_abbrev = Qnil;
  Vlast_abbrev_text = Qnil;
  last_abbrev_point = 0;

  DEFVAR_LISP ("abbrev-start-location", &Vabbrev_start_location /*
Buffer position for `expand-abbrev' to use as the start of the abbrev.
nil means use the word before point as the abbrev.
Calling `expand-abbrev' sets this to nil.
*/ );
  Vabbrev_start_location = Qnil;

  DEFVAR_LISP ("abbrev-start-location-buffer", &Vabbrev_start_location_buffer /*
Buffer that `abbrev-start-location' has been set for.
Trying to expand an abbrev in any other buffer clears `abbrev-start-location'.
*/ );
  Vabbrev_start_location_buffer = Qnil;

  DEFVAR_BOOL ("abbrev-all-caps", &abbrev_all_caps /*
*Non-nil means expand multi-word abbrevs all caps if abbrev was so.
*/ );
  abbrev_all_caps = 0;

  DEFVAR_LISP ("pre-abbrev-expand-hook", &Vpre_abbrev_expand_hook /*
Function or functions to be called before abbrev expansion is done.
This is the first thing that `expand-abbrev' does, and so this may change
the current abbrev table before abbrev lookup happens.
*/ );
  Vpre_abbrev_expand_hook = Qnil;
}
