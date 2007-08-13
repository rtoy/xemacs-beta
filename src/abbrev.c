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

/* Hook to run before expanding any abbrev.  */
Lisp_Object Vpre_abbrev_expand_hook, Qpre_abbrev_expand_hook;


/* Expand the word before point, if it is an abbrev.
   Returns Qt if an expansion is done. */

DEFUN ("expand-abbrev", Fexpand_abbrev, 0, 0, "", /*
Expand the abbrev before point, if there is an abbrev there.
Effective when explicitly called even when `abbrev-mode' is nil.
Returns t if expansion took place.
*/
       ())
{
  /* This function can GC */
  REGISTER Bufbyte *buffer, *p;
  REGISTER Bufpos wordstart, wordend, idx;
  Charcount whitecnt;
  Charcount uccount = 0, lccount = 0;
  REGISTER Lisp_Object sym;
  Lisp_Object expansion, hook, value;
  struct buffer *buf = current_buffer;
  Lisp_Object lbuf;
  int oldmodiff = BUF_MODIFF (buf);

  XSETBUFFER (lbuf, buf);
  run_hook (Qpre_abbrev_expand_hook);
  /* If the hook changes the buffer, treat that as having "done an
     expansion".  */
  value = (BUF_MODIFF (buf) != oldmodiff ? Qt : Qnil);

  wordstart = 0;
  if (!BUFFERP (Vabbrev_start_location_buffer) ||
      XBUFFER (Vabbrev_start_location_buffer) != buf)
    Vabbrev_start_location = Qnil;
  if (!NILP (Vabbrev_start_location))
    {
      wordstart = get_buffer_pos_char (buf, Vabbrev_start_location, GB_COERCE_RANGE);
      Vabbrev_start_location = Qnil;
      if (wordstart < BUF_BEGV (buf) || wordstart > BUF_ZV (buf))
        wordstart = 0;
      if (wordstart && wordstart != BUF_ZV (buf) &&
          BUF_FETCH_CHAR (buf, wordstart) == '-')
	buffer_delete_range (buf, wordstart, wordstart + 1, 0);
    }
  if (!wordstart)
    wordstart = scan_words (buf, BUF_PT (buf), -1);

  if (!wordstart)
    return value;

  wordend = scan_words (buf, wordstart, 1);
  if (!wordend)
    return value;

  if (wordend > BUF_PT (buf))
    wordend = BUF_PT (buf);
  whitecnt = BUF_PT (buf) - wordend;
  if (wordend <= wordstart)
    return value;

  p = buffer = (Bufbyte *) alloca (MAX_EMCHAR_LEN*(wordend - wordstart));

  for (idx = wordstart; idx < wordend; idx++)
    {
      REGISTER Emchar c = BUF_FETCH_CHAR (buf, idx);
      if (UPPERCASEP (buf, c))
	c = DOWNCASE (buf, c), uccount++;
      else if (! NOCASEP (buf, c))
	lccount++;
      p += set_charptr_emchar (p, c);
    }

  if (VECTORP (buf->abbrev_table))
    sym = oblookup (buf->abbrev_table,
		    buffer,
		    p - buffer);
  else
    sym = Qzero;
  if (INTP (sym) || NILP (XSYMBOL (sym)->value))
    sym = oblookup (Vglobal_abbrev_table,
		    buffer,
		    p - buffer);
  if (INTP (sym) || NILP (XSYMBOL (sym)->value))
    return value;

  if (INTERACTIVE && !EQ (minibuf_window, Fselected_window (Qnil)))
    {
      /* Add an undo boundary, in case we are doing this for
         a self-inserting command which has avoided making one so far.  */
      BUF_SET_PT (buf, wordend);
      Fundo_boundary ();
    }
  BUF_SET_PT (buf, wordstart);
  Vlast_abbrev_text =
    make_string_from_buffer (buf, wordstart, wordend - wordstart);
  buffer_delete_range (buf, wordstart, wordend, 0);

  /* Now sym is the abbrev symbol. */
  Vlast_abbrev = sym;
  last_abbrev_point = wordstart;

  if (INTP (XSYMBOL (sym)->plist))
    XSETINT (XSYMBOL (sym)->plist,
	     XINT (XSYMBOL (sym)->plist) + 1);	/* Increment use count */

  expansion = XSYMBOL (sym)->value;
  buffer_insert_lisp_string (buf, expansion);
  BUF_SET_PT (buf, BUF_PT (buf) + whitecnt);

  if (uccount && !lccount)
    {
      /* Abbrev was all caps */
      /* If expansion is multiple words, normally capitalize each word */
      /* This used to be if (!... && ... >= ...) Fcapitalize; else Fupcase
	 but Megatest 68000 compiler can't handle that */
      if (!abbrev_all_caps)
	if (scan_words (buf, BUF_PT (buf), -1) >
	    scan_words (buf, wordstart, 1))
	  {
	    Fupcase_initials_region (make_int (wordstart),
				     make_int (BUF_PT (buf)),
				     lbuf);
	    goto caped;
	  }
      /* If expansion is one word, or if user says so, upcase it all. */
      Fupcase_region (make_int (wordstart), make_int (BUF_PT (buf)),
		      lbuf);
    caped: ;
    }
  else if (uccount)
    {
      /* Abbrev included some caps.  Cap first initial of expansion */
      Bufpos pos = wordstart;

      /* Find the initial.  */
      while (pos < BUF_PT (buf)
             && !WORD_SYNTAX_P (XCHAR_TABLE (buf->mirror_syntax_table),
				BUF_FETCH_CHAR (buf, pos)))
        pos++;

      /* Change just that.  */
      Fupcase_initials_region (make_int (pos), make_int (pos + 1), lbuf);
    }

  hook = XSYMBOL (sym)->function;
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
*Set non-nil means expand multi-word abbrevs all caps if abbrev was so.
*/ );
  abbrev_all_caps = 0;

  DEFVAR_LISP ("pre-abbrev-expand-hook", &Vpre_abbrev_expand_hook /*
Function or functions to be called before abbrev expansion is done.
This is the first thing that `expand-abbrev' does, and so this may change
the current abbrev table before abbrev lookup happens.
*/ );
  Vpre_abbrev_expand_hook = Qnil;
}
