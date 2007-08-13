/* XEmacs case conversion functions.
   Copyright (C) 1985, 1992, 1993, 1994 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "insdel.h"
#include "syntax.h"

enum case_action {CASE_UP, CASE_DOWN, CASE_CAPITALIZE, CASE_CAPITALIZE_UP};

static Lisp_Object
casify_object (struct buffer *buf, enum case_action flag, Lisp_Object obj)
{
  REGISTER Emchar c;
  REGISTER Charcount i, len;
  REGISTER int inword = flag == CASE_DOWN;
  Lisp_Object syntax_table = buf->syntax_table;

  while (1)
    {
      if (CHAR_OR_CHAR_INTP (obj))
	{
	  CHECK_CHAR_COERCE_INT (obj);
	  c = XCHAR (obj);
	  if (IN_TRT_TABLE_DOMAIN (c))
	    {
	      if (inword)
		obj = make_char (DOWNCASE (buf, c));
	      else if (!UPPERCASEP (buf, c))
		obj = make_char (UPCASE1 (buf, c));
	    }
	  return obj;
	}
      if (STRINGP (obj))
	{
	  obj = Fcopy_sequence (obj);
	  len = string_char_length (XSTRING (obj));
	  for (i = 0; i < len; i++)
	    {
	      c = string_char (XSTRING (obj), i);
	      if (inword)
		c = DOWNCASE (buf, c);
	      else if (!UPPERCASEP (buf, c)
		       && (!inword || flag != CASE_CAPITALIZE_UP))
		c = UPCASE1 (buf, c);
	      set_string_char (XSTRING (obj), i, c);
	      if ((int) flag >= (int) CASE_CAPITALIZE)
		inword = WORD_SYNTAX_P (syntax_table, c);
	    }
	  return obj;
	}
      obj = wrong_type_argument (Qchar_or_string_p, obj);
    }
}

DEFUN ("upcase", Fupcase, Supcase, 1, 2, 0 /*
Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'.
Optional second arg BUFFER specifies which buffer's case tables to use,
 and defaults to the current buffer.
*/ )
  (obj, buffer)
     Lisp_Object obj, buffer;
{
  return casify_object (decode_buffer (buffer, 0), CASE_UP, obj);
}

DEFUN ("downcase", Fdowncase, Sdowncase, 1, 2, 0 /*
Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
 and defaults to the current buffer.
*/ )
  (obj, buffer)
     Lisp_Object obj, buffer;
{
  return casify_object (decode_buffer (buffer, 0), CASE_DOWN, obj);
}

DEFUN ("capitalize", Fcapitalize, Scapitalize, 1, 2, 0 /*
Convert argument to capitalized form and return that.
This means that each word's first character is upper case
and the rest is lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
 and defaults to the current buffer.
*/ )
  (obj, buffer)
     Lisp_Object obj, buffer;
{
  return casify_object (decode_buffer (buffer, 0), CASE_CAPITALIZE, obj);
}

/* Like Fcapitalize but change only the initials.  */

DEFUN ("upcase-initials", Fupcase_initials, Supcase_initials, 1, 2, 0 /*
Convert the initial of each word in the argument to upper case.
Do not change the other letters of each word.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional second arg BUFFER specifies which buffer's case tables to use,
 and defaults to the current buffer.
*/ )
  (obj, buffer)
     Lisp_Object obj, buffer;
{
  return casify_object (decode_buffer (buffer, 0), CASE_CAPITALIZE_UP, obj);
}

/* flag is CASE_UP, CASE_DOWN or CASE_CAPITALIZE or CASE_CAPITALIZE_UP.
   b and e specify range of buffer to operate on. */

static void
casify_region (struct buffer *buf, enum case_action flag, Lisp_Object b,
	       Lisp_Object e)
{
  /* This function can GC */
  REGISTER Bufpos i;
  Bufpos start, end;
  REGISTER Emchar c;
  REGISTER int inword = flag == CASE_DOWN;
  Lisp_Object syntax_table = buf->syntax_table;
  int mccount;

  if (EQ (b, e))
    /* Not modifying because nothing marked */
    return;

  get_buffer_range_char (buf, b, e, &start, &end, 0);

  mccount = begin_multiple_change (buf, start, end);
  record_change (buf, start, end - start);
  BUF_MODIFF (buf)++;

  for (i = start; i < end; i++)
    {
      c = BUF_FETCH_CHAR (buf, i);
      if (inword && flag != CASE_CAPITALIZE_UP)
	c = DOWNCASE (buf, c);
      else if (!UPPERCASEP (buf, c)
	       && (!inword || flag != CASE_CAPITALIZE_UP))
	c = UPCASE1 (buf, c);

      buffer_replace_char (buf, i, c, 1, (i == start));
      /* !!#### need to revalidate the start and end pointers in case
	 the buffer was changed */
      if ((int) flag >= (int) CASE_CAPITALIZE)
	inword = WORD_SYNTAX_P (syntax_table, c);
    }
  end_multiple_change (buf, mccount);
}

DEFUN ("upcase-region", Fupcase_region, Supcase_region, 2, 3, "r" /*
Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
 the region to operate on.  When used as a command, the text between
 point and the mark is operated on.
See also `capitalize-region'.
Optional third arg BUFFER defaults to the current buffer.
*/ )
  (b, e, buffer)
     Lisp_Object b, e, buffer;
{
  /* This function can GC */
  casify_region (decode_buffer (buffer, 1), CASE_UP, b, e);
  return Qnil;
}

DEFUN ("downcase-region", Fdowncase_region, Sdowncase_region, 2, 3, "r" /*
Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
 the region to operate on.  When used as a command, the text between
 point and the mark is operated on.
Optional third arg BUFFER defaults to the current buffer.
*/ )
  (b, e, buffer)
     Lisp_Object b, e, buffer;
{
  /* This function can GC */
  casify_region (decode_buffer (buffer, 1), CASE_DOWN, b, e);
  return Qnil;
}

DEFUN ("capitalize-region", Fcapitalize_region, Scapitalize_region, 2, 3, "r" /*
Convert the region to capitalized form.
Capitalized form means each word's first character is upper case
 and the rest of it is lower case.
In programs, give two arguments, the starting and ending
 character positions to operate on.
Optional third arg BUFFER defaults to the current buffer.
*/ )
  (b, e, buffer)
     Lisp_Object b, e, buffer;
{
  /* This function can GC */
  casify_region (decode_buffer (buffer, 1), CASE_CAPITALIZE, b, e);
  return Qnil;
}

/* Like Fcapitalize_region but change only the initials.  */

DEFUN ("upcase-initials-region", Fupcase_initials_region,
       Supcase_initials_region, 2, 3, "r" /*
Upcase the initial of each word in the region.
Subsequent letters of each word are not changed.
In programs, give two arguments, the starting and ending
 character positions to operate on.
Optional third arg BUFFER defaults to the current buffer.
*/ )
  (b, e, buffer)
     Lisp_Object b, e, buffer;
{
  casify_region (decode_buffer (buffer, 1), CASE_CAPITALIZE_UP, b, e);
  return Qnil;
}


static Lisp_Object
operate_on_word (struct buffer *buf, Lisp_Object arg, int *newpoint)
{
  Bufpos farend;

  CHECK_INT (arg);
  farend = scan_words (buf, BUF_PT (buf), XINT (arg));
  if (!farend)
    farend = XINT (arg) > 0 ? BUF_ZV (buf) : BUF_BEGV (buf);

  *newpoint = ((BUF_PT (buf) > farend) ? BUF_PT (buf) : farend);
  return (make_int (farend));
}

DEFUN ("upcase-word", Fupcase_word, Supcase_word, 1, 2, "p" /*
Convert following word (or ARG words) to upper case, moving over.
With negative argument, convert previous words but do not move.
See also `capitalize-word'.
Optional second arg BUFFER defaults to the current buffer.
*/ )
  (arg, buffer)
     Lisp_Object arg, buffer;
{
  /* This function can GC */
  Lisp_Object beg, end;
  Bufpos newpoint;
  struct buffer *buf = decode_buffer (buffer, 1);

  beg = make_int (BUF_PT (buf));
  end = operate_on_word (buf, arg, &newpoint);
  casify_region (buf, CASE_UP, beg, end);
  BUF_SET_PT (buf, newpoint);
  return Qnil;
}

DEFUN ("downcase-word", Fdowncase_word, Sdowncase_word, 1, 2, "p" /*
Convert following word (or ARG words) to lower case, moving over.
With negative argument, convert previous words but do not move.
Optional second arg BUFFER defaults to the current buffer.
*/ )
  (arg, buffer)
     Lisp_Object arg, buffer;
{
  /* This function can GC */
  Lisp_Object beg, end;
  Bufpos newpoint;
  struct buffer *buf = decode_buffer (buffer, 1);

  beg = make_int (BUF_PT (buf));
  end = operate_on_word (buf, arg, &newpoint);
  casify_region (buf, CASE_DOWN, beg, end);
  BUF_SET_PT (buf, newpoint);
  return Qnil;
}

DEFUN ("capitalize-word", Fcapitalize_word, Scapitalize_word, 1, 2, "p" /*
Capitalize the following word (or ARG words), moving over.
This gives the word(s) a first character in upper case
 and the rest lower case.
With negative argument, capitalize previous words but do not move.
Optional second arg BUFFER defaults to the current buffer.
*/ )
  (arg, buffer)
     Lisp_Object arg, buffer;
{
  /* This function can GC */
  Lisp_Object beg, end;
  Bufpos newpoint;
  struct buffer *buf = decode_buffer (buffer, 1);

  beg = make_int (BUF_PT (buf));
  end = operate_on_word (buf, arg, &newpoint);
  casify_region (buf, CASE_CAPITALIZE, beg, end);
  BUF_SET_PT (buf, newpoint);
  return Qnil;
}


void
syms_of_casefiddle (void)
{
  defsubr (&Supcase);
  defsubr (&Sdowncase);
  defsubr (&Scapitalize);
  defsubr (&Supcase_initials);
  defsubr (&Supcase_region);
  defsubr (&Sdowncase_region);
  defsubr (&Scapitalize_region);
  defsubr (&Supcase_initials_region);
  defsubr (&Supcase_word);
  defsubr (&Sdowncase_word);
  defsubr (&Scapitalize_word);
}
