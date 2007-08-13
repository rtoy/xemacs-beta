/* Lisp object printing and output streams.
   Copyright (C) 1985, 1986, 1988, 1992-1995 Free Software Foundation, Inc.
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

/* Synched up with: Not synched with FSF. */

/* This file has been Mule-ized. */

/* Seriously hacked on by Ben Wing for Mule. */

#include <config.h>
#include "lisp.h"

#ifndef standalone
#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "console-tty.h"
#include "console-stream.h"
#include "extents.h"
#include "frame.h"
#include "emacsfns.h"
#include "insdel.h"
#include "lstream.h"

#endif /* not standalone */

static void print_error_message (Lisp_Object data, Lisp_Object stream);

Lisp_Object Vstandard_output, Qstandard_output;

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output;
Lisp_Object Qalternate_debugging_output;

/* Avoid actual stack overflow in print.  */
static int print_depth;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE 200
Lisp_Object being_printed[PRINT_CIRCLE];

/* Maximum length of list or vector to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;
Lisp_Object Qprint_length;

/* Maximum length of string to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_string_length;
Lisp_Object Qprint_string_length;

/* Maximum depth of list to print in full; noninteger means
   effectively infinity.  */

Lisp_Object Vprint_level;

/* Label to use when making echo-area messages. */

Lisp_Object Vprint_message_label;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;
int print_readably;

/* Non-nil means print #: before uninterned symbols.
   Neither t nor nil means so that and don't clear Vprint_gensym_alist
   on entry to and exit from print functions.  */
Lisp_Object Vprint_gensym;
Lisp_Object Vprint_gensym_alist;

Lisp_Object Qprint_escape_newlines;
Lisp_Object Qprint_readably;

Lisp_Object Qdisplay_error;
Lisp_Object Qprint_message_label;

/* Force immediate output of all printed data.  Used for debugging. */
int print_unbuffered;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */



int stdout_needs_newline;

/* Write a string (in internal format) to stdio stream STREAM. */

void
write_string_to_stdio_stream (FILE *stream, struct console *con,
			      CONST Bufbyte *str,
			      Bytecount offset, Bytecount len,
			      enum external_data_format fmt)
{
  int extlen;
  CONST Extbyte *extptr;

  GET_CHARPTR_EXT_DATA_ALLOCA (str + offset, len, fmt, extptr, extlen);
  if (stream)
    fwrite (extptr, 1, extlen, stream);
  else
    {
      assert (CONSOLE_TTY_P (con));
      Lstream_write (XLSTREAM (CONSOLE_TTY_DATA (con)->outstream),
		     extptr, extlen);
    }
  if (stream == stdout || stream == stderr ||
      (!stream && CONSOLE_TTY_DATA (con)->is_stdio))
    {
      if (termscript)
	{
	  fwrite (extptr, 1, extlen, termscript);
	  fflush (termscript);
	}
      stdout_needs_newline = (extptr[extlen - 1] != '\n');
    }
}

/* Write a string to the output location specified in FUNCTION.
   Arguments NONRELOC, RELOC, OFFSET, and LEN are as in
   buffer_insert_string_1() in insdel.c. */

static void
output_string (Lisp_Object function, CONST Bufbyte *nonreloc,
	       Lisp_Object reloc, Bytecount offset, Bytecount len)
{
  /* This function can GC */
  Charcount ccoff, cclen;
  /* We change the value of nonreloc (fetching it from reloc as
     necessary), but we don't want to pass this changed value on to
     other functions that take both a nonreloc and a reloc, or things
     may get confused and an assertion failure in
     fixup_internal_substring() may get triggered. */
  CONST Bufbyte *newnonreloc = nonreloc;
  struct gcpro gcpro1, gcpro2;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return;

  /* Perhaps not necessary but probably safer. */
  GCPRO2 (function, reloc);

  fixup_internal_substring (newnonreloc, reloc, offset, &len);

  if (STRINGP (reloc))
    newnonreloc = XSTRING_DATA (reloc);

  ccoff = bytecount_to_charcount (newnonreloc, offset);
  cclen = bytecount_to_charcount (newnonreloc + offset, len);

  if (LSTREAMP (function))
    {
      /* Lstream_write() could easily cause GC inside of it, if the
	 stream is a print-stream. (It will call output_string()
	 recursively.) This is probably the fastest way to fix this
	 problem. (alloca() is very fast on machines that have it
	 built-in, and you avoid some nasty problems with recursion
	 that could result from using a static buffer somewhere.)

	 The other possibility is to inhibit GC, but that of course
	 would require an unwind-protect, which is usually a lot
	 slower than the small amount of memcpy()ing that happens
	 here. */
      if (STRINGP (reloc))
	{
	  Bufbyte *copied = (Bufbyte *) alloca (len);
	  memcpy (copied, newnonreloc + offset, len);
	  Lstream_write (XLSTREAM (function), copied, len);
	}
      else
	Lstream_write (XLSTREAM (function), newnonreloc + offset, len);

      if (print_unbuffered)
	Lstream_flush (XLSTREAM (function));
    }

#ifndef standalone
  else if (BUFFERP (function))
    {
      CHECK_LIVE_BUFFER (function);
      buffer_insert_string (XBUFFER (function), nonreloc, reloc, offset, len);
    }
  else if (MARKERP (function))
    {
      /* marker_position will err if marker doesn't point anywhere */
      Bufpos spoint = marker_position (function);

      buffer_insert_string_1 (XBUFFER (Fmarker_buffer (function)),
			      spoint, nonreloc, reloc, offset, len,
			      0);
      Fset_marker (function, make_int (spoint + cclen),
		   Fmarker_buffer (function));
    }
  else if (FRAMEP (function))
    {
      struct frame *f = XFRAME (function);
      if (!EQ (Vprint_message_label, echo_area_status (f)))
	clear_echo_area_from_print (f, Qnil, 1);
      echo_area_append (f, nonreloc, reloc, offset, len, Vprint_message_label);
    }
#endif /* not standalone */
  else if (EQ (function, Qt) || EQ (function, Qnil))
    {
      write_string_to_stdio_stream (stdout, 0, newnonreloc, offset, len,
				    FORMAT_TERMINAL);
    }
  else
    {
      Charcount iii;

      for (iii = ccoff; iii < cclen + ccoff; iii++)
	{
	  call1 (function,
		 make_char (charptr_emchar_n (newnonreloc, iii)));
	  if (STRINGP (reloc))
	    newnonreloc = XSTRING_DATA (reloc);
	}
    }

  UNGCPRO;
}

struct print_stream
{
  FILE *file;
  Lisp_Object fun;
};

#define get_print_stream(stream) \
  ((struct print_stream *) Lstream_data (stream))

DEFINE_LSTREAM_IMPLEMENTATION ("print", lstream_print,
			       sizeof (struct print_stream));

static Lisp_Object
make_print_output_stream (FILE *file, Lisp_Object fun)
{
  Lstream *str = Lstream_new (lstream_print, "w");
  struct print_stream *ps = get_print_stream (str);
  Lisp_Object val = Qnil;

  Lstream_set_character_mode (str);
  ps->file = file;
  ps->fun = fun;
  XSETLSTREAM (val, str);
  return val;
}

/* #### This isn't being used anywhere at the moment.  Is it supposed
   to be? */
#if 0
static void
reset_print_stream (Lstream *str, FILE *file, Lisp_Object fun)
{
  struct print_stream *ps = get_print_stream (str);

  Lstream_reopen (str);
  ps->file = file;
  ps->fun = fun;
}
#endif

static Lisp_Object
print_marker (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return get_print_stream (XLSTREAM (obj))->fun;
}

static int
print_writer (Lstream *stream, CONST unsigned char *data, int size)
{
  struct print_stream *ps = get_print_stream (stream);

  if (ps->file)
    {
      write_string_to_stdio_stream (ps->file, 0, data, 0, size,
				    FORMAT_TERMINAL);
      /* Make sure it really gets written now. */
      if (print_unbuffered)
	fflush (ps->file);
    }
  else
    output_string (ps->fun, data, Qnil, 0, size);
  return size;
}

static Lisp_Object
canonicalize_printcharfun (Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;

  if (EQ (printcharfun, Qt) || NILP (printcharfun))
    {
#ifndef standalone
      printcharfun = Fselected_frame (Qnil); /* print to minibuffer */
#endif
    }
  return printcharfun;
}


static Lisp_Object
print_prepare (Lisp_Object printcharfun)
{
  FILE *stdio_stream = 0;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress)
    return Qnil;

  printcharfun = canonicalize_printcharfun (printcharfun);
  if (EQ (printcharfun, Qnil))
    {
      stdio_stream = stdout;
    }
#if 0 /* Don't bother */
  else if (SUBRP (indirect_function (printcharfun, 0))
           && (XSUBR (indirect_function (printcharfun, 0))
               == Sexternal_debugging_output))
    {
      stdio_stream = stderr;
    }
#endif
  if (!CONSP (Vprint_gensym))
    Vprint_gensym_alist = Qnil;

  return make_print_output_stream (stdio_stream, printcharfun);
}

static void
print_finish (Lisp_Object stream)
{
  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress)
    return;

  if (!CONSP (Vprint_gensym))
    Vprint_gensym_alist = Qnil;

  Lstream_delete (XLSTREAM (stream));
}

#if 1 /* Prefer space over "speed" */
#define write_char_internal(string_of_length_1, stream) \
  write_string_1 ((CONST Bufbyte *) (string_of_length_1), 1, (stream))
#else
#define write_char_internal(string_of_length_1, stream) \
  output_string ((stream), (CONST Bufbyte *) (string_of_length_1), Qnil, 0, 1)
#endif

/* NOTE:  Do not call this with the data of a Lisp_String,
 *  as printcharfun might cause a GC, which might cause
 *  the string's data to be relocated.
 *  Use print_object_internal (string, printcharfun, 0)
 *  to princ a Lisp_String
 * Note: "stream" should be the result of "canonicalize_printcharfun"
 *  (ie Qnil means stdout, not Vstandard_output, etc)
 */
void
write_string_1 (CONST Bufbyte *str, Bytecount size, Lisp_Object stream)
{
  /* This function can GC */
  assert (size >= 0);
  output_string (stream, str, Qnil, 0, size);
}

void
write_c_string (CONST char *str, Lisp_Object stream)
{
  /* This function can GC */
  write_string_1 ((CONST Bufbyte *) str, strlen (str), stream);
}


DEFUN ("write-char", Fwrite_char, 1, 2, 0, /*
Output character CH to stream STREAM.
STREAM defaults to the value of `standard-output' (which see).
*/
       (ch, stream))
{
  /* This function can GC */
  Bufbyte str[MAX_EMCHAR_LEN];
  Bytecount len;

  CHECK_CHAR_COERCE_INT (ch);
  len = set_charptr_emchar (str, XCHAR (ch));
  output_string (canonicalize_printcharfun (stream), str, Qnil, 0, len);
  return ch;
}

#ifndef standalone

void
temp_output_buffer_setup (CONST char *bufname)
{
  /* This function can GC */
  struct buffer *old = current_buffer;
  Lisp_Object buf;

#ifdef I18N3
  /* #### This function should accept a Lisp_Object instead of a char *,
     so that proper translation on the buffer name can occur. */
#endif

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->read_only = Qnil;
  Ferase_buffer (Qnil);

  XSETBUFFER (buf, current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (CONST char *bufname,
                                     Lisp_Object (*function) (Lisp_Object arg),
                                     Lisp_Object arg,
                                     Lisp_Object same_frame)
{
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object buf = Qnil;

  GCPRO3 (buf, arg, same_frame);

  temp_output_buffer_setup (GETTEXT (bufname));
  buf = Vstandard_output;

  arg = (*function) (arg);

  temp_output_buffer_show (buf, same_frame);
  UNGCPRO;

  return unbind_to (speccount, arg);
}

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, 1, UNEVALLED, 0, /*
Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.
The buffer is cleared out initially, and marked as unmodified when done.
All output done by BODY is inserted in that buffer by default.
The buffer is displayed in another window, but not selected.
The value of the last form in BODY is returned.
If BODY does not finish normally, the buffer BUFNAME is not displayed.

If variable `temp-buffer-show-function' is non-nil, call it at the end
to get the buffer displayed.  It gets one argument, the buffer to display.
*/
       (args))
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object name;
  int speccount = specpdl_depth ();
  Lisp_Object buf, val;

#ifdef I18N3
  /* #### should set the buffer to be translating.  See print_internal(). */
#endif

  GCPRO1 (args);
  name = Feval (Fcar (args));
  UNGCPRO;

  CHECK_STRING (name);
  temp_output_buffer_setup ((char *) XSTRING_DATA (name));
  buf = Vstandard_output;

  val = Fprogn (Fcdr (args));

  temp_output_buffer_show (buf, Qnil);

  return unbind_to (speccount, val);
}
#endif /* not standalone */

DEFUN ("terpri", Fterpri, 0, 1, 0, /*
Output a newline to STREAM.
If STREAM is omitted or nil, the value of `standard-output' is used.
*/
       (stream))
{
  /* This function can GC */
  Bufbyte str[1];
  str[0] = '\n';
  output_string (canonicalize_printcharfun (stream), str, Qnil, 0, 1);
  return Qt;
}

DEFUN ("prin1", Fprin1, 1, 2, 0, /*
Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object the_stream = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (object, stream, the_stream);
  print_depth = 0;
  the_stream = print_prepare (stream);
  print_internal (object, the_stream, 1);
  print_finish (the_stream);
  UNGCPRO;
  return object;
}

/* Stream to which prin1-to-string prints. */
static Lisp_Object Vprin1_to_string_stream;

DEFUN ("prin1-to-string", Fprin1_to_string, 1, 2, 0, /*
Return a string containing the printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible, unless the optional
second argument NOESCAPE is non-nil.
*/
       (object, noescape))
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  Lstream *stream;
  struct gcpro gcpro1;

  /* We avoid creating a new stream for every invocation of
     prin1_to_string, for better memory usage.  */

  if (NILP (Vprin1_to_string_stream))
    Vprin1_to_string_stream = make_resizing_buffer_output_stream ();
  stream = XLSTREAM (Vprin1_to_string_stream);
  Lstream_rewind (stream);

  /* In case a caller forgot to protect. */
  GCPRO1 (object);
  print_depth = 0;
  print_internal (object, Vprin1_to_string_stream, NILP (noescape));
  Lstream_flush (stream);
  UNGCPRO;
  result = make_string (resizing_buffer_stream_ptr (stream),
			Lstream_byte_count (stream));
  return result;
}

DEFUN ("princ", Fprinc, 1, 2, 0, /*
Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.
Output stream is STREAM, or value of standard-output (which see).
*/
       (obj, stream))
{
  /* This function can GC */
  Lisp_Object the_stream = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (obj, stream, the_stream);
  the_stream = print_prepare (stream);
  print_depth = 0;
  print_internal (obj, the_stream, 0);
  print_finish (the_stream);
  UNGCPRO;
  return obj;
}

DEFUN ("print", Fprint, 1, 2, 0, /*
Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (obj, stream))
{
  /* This function can GC */
  Lisp_Object the_stream = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (obj, stream, the_stream);
  the_stream = print_prepare (stream);
  print_depth = 0;
  write_char_internal ("\n", the_stream);
  print_internal (obj, the_stream, 1);
  write_char_internal ("\n", the_stream);
  print_finish (the_stream);
  UNGCPRO;
  return obj;
}

#include "emacsfns.h"

/* Synched with Emacs 19.34 -- underlying implementation (incarnated
   in print_error_message) is completely divergent, though.  */
DEFUN ("error-message-string", Ferror_message_string, 1, 1, 0, /*
Convert an error value (ERROR-SYMBOL . DATA) to an error message.
*/
  (data))
{
  /* This function can GC */

  /* This should maybe use Vprin1_to_string_stream...  However, it's
     called sufficiently rarely, so I don't think it should matter.  */
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;
  GCPRO1 (stream);

  print_error_message (data, stream);
  Lstream_flush (XLSTREAM (stream));
  UNGCPRO;
  return make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		      Lstream_byte_count (XLSTREAM (stream)));
}

/* Print an error message for the error DATA onto Lisp output stream
   STREAM (suitable for the print functions).

   This is a complete implementation of `display-error', which used to
   be in Lisp (see prim/cmdloop.el).  It was ported to C so we can use
   it in Ferror_message_string.  Fdisplay_error and
   Ferror_message_string are trivial wrappers to this function.  */
static void
print_error_message (Lisp_Object error_object, Lisp_Object stream)
{
  /* This function can GC */
  Lisp_Object type;
  Lisp_Object method = Qnil;
  Lisp_Object tail = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (tail);

  type = Fcar_safe (error_object);

  if (! (CONSP (error_object) && SYMBOLP (type)
	 && CONSP (Fget (type, Qerror_conditions, Qnil))))
    goto error_throw;

  tail = XCDR (error_object);
  while (!NILP (tail))
    {
      if (CONSP (tail))
	tail = XCDR (tail);
      else
	goto error_throw;
    }
  tail = Fget (type, Qerror_conditions, Qnil);
  while (!NILP (tail))
    {
      if (!(CONSP (tail) && SYMBOLP (XCAR (tail))))
	goto error_throw;
      else if (!NILP (Fget (XCAR (tail), Qdisplay_error, Qnil)))
	{
	  method = Fget (XCAR (tail), Qdisplay_error, Qnil);
	  goto error_throw;
	}
      else
	tail = XCDR (tail);
    }
  /* Default method */
  {
    int first = 1;
    Lisp_Object printcharfun = canonicalize_printcharfun (stream);
    int speccount = specpdl_depth ();

    specbind (Qprint_message_label, Qerror);
    tail = Fcdr (error_object);
    if (EQ (type, Qerror))
      {
	Fprinc (Fcar (tail), stream);
	tail = Fcdr (tail);
      }
    else
      {
	Lisp_Object errmsg = Fget (type, Qerror_message, Qnil);
	if (NILP (errmsg))
	  Fprinc (type, stream);
	else
	  Fprinc (errmsg, stream);
      }
    while (!NILP (tail))
      {
	write_c_string (first ? ": " : ", ", printcharfun);
	Fprin1 (Fcar (tail), stream);
	tail = Fcdr (tail);
	first = 0;
      }
    unbind_to (speccount, Qnil);
    UNGCPRO;
    return;
    /* Unreached */
  }

 error_throw:
  UNGCPRO;
  if (NILP (method))
    {
      write_c_string ("Peculiar error ",
		      canonicalize_printcharfun (stream));
      Fprin1 (error_object, stream);
      return;
    }
  else
    {
      call2 (method, error_object, stream);
    }
}

DEFUN ("display-error", Fdisplay_error, 2, 2, 0, /*
Display an error message for ERROR-OBJECT to STREAM.
*/
       (error_object, stream))
{
  /* This function can GC */
  print_error_message (error_object, stream);
  return Qnil;
}


#ifdef LISP_FLOAT_TYPE

Lisp_Object Vfloat_output_format;
Lisp_Object Qfloat_output_format;

void
float_to_string (char *buf, double data)
/*
 * This buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtably
 * 20d float_output_format, with the negative of the C-constant "HUGE"
 * from <math.h>.
 *
 * On the vax the worst case is -1e38 in 20d format which takes 61 bytes.
 *
 * I assume that IEEE-754 format numbers can take 329 bytes for the worst
 * case of -1e307 in 20d float_output_format. What is one to do (short of
 * re-writing _doprnt to be more sane)?
 * 			-wsr
 */
{
  Bufbyte *cp, c;
  int width;

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    sprintf (buf, "%.16g", data);
  else			/* oink oink */
    {
      /* Check that the spec we have is fully valid.
	 This means not only valid for printf,
	 but meant for floats, and reasonable.  */
      cp = XSTRING_DATA (Vfloat_output_format);

      if (cp[0] != '%')
	goto lose;
      if (cp[1] != '.')
	goto lose;

      cp += 2;
      for (width = 0; (c = *cp, isdigit (c)); cp++)
	{
	  width *= 10;
	  width += c - '0';
	}

      if (*cp != 'e' && *cp != 'f' && *cp != 'g' && *cp != 'E' && *cp != 'G')
	goto lose;

      if (width < (int) (*cp != 'e' && *cp != 'E') || width > DBL_DIG)
	goto lose;

      if (cp[1] != 0)
	goto lose;

      sprintf (buf, (char *) XSTRING_DATA (Vfloat_output_format),
	       data);
    }

  /* added by jwz: don't allow "1.0" to print as "1"; that destroys
     the read-equivalence of lisp objects.  (* x 1) and (* x 1.0) do
     not do the same thing, so it's important that the printed
     representation of that form not be corrupted by the printer.
   */
  {
    Bufbyte *s = (Bufbyte *) buf; /* don't use signed chars here!
				     isdigit() can't hack them! */
    if (*s == '-') s++;
    for (; *s; s++)
      /* if there's a non-digit, then there is a decimal point, or
	 it's in exponential notation, both of which are ok. */
      if (!isdigit (*s))
	goto DONE_LABEL;
    /* otherwise, we need to hack it. */
    *s++ = '.';
    *s++ = '0';
    *s = 0;
  }
 DONE_LABEL:

  /* Some machines print "0.4" as ".4".  I don't like that. */
  if (buf [0] == '.' || (buf [0] == '-' && buf [1] == '.'))
    {
      int i;
      for (i = strlen (buf) + 1; i >= 0; i--)
	buf [i+1] = buf [i];
      buf [(buf [0] == '-' ? 1 : 0)] = '0';
    }
}
#endif /* LISP_FLOAT_TYPE */

static void
print_vector_internal (CONST char *start, CONST char *end,
                       Lisp_Object obj,
                       Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  int i;
  int len = XVECTOR_LENGTH (obj);
  int last = len;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (INTP (Vprint_length))
    {
      int max = XINT (Vprint_length);
      if (max < len) last = max;
    }

  write_c_string (start, printcharfun);
  for (i = 0; i < last; i++)
    {
      Lisp_Object elt = XVECTOR_DATA (obj)[i];
      if (i != 0) write_char_internal (" ", printcharfun);
      print_internal (elt, printcharfun, escapeflag);
    }
  UNGCPRO;
  if (last != len)
    write_c_string (" ...", printcharfun);
  write_c_string (end, printcharfun);
}

void
print_cons (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;

  /* If print_readably is on, print (quote -foo-) as '-foo-
     (Yeah, this should really be what print-pretty does, but we
     don't have the rest of a pretty printer, and this actually
     has non-negligible impact on size/speed of .elc files.)
  */
  if (print_readably &&
      EQ (XCAR (obj), Qquote) &&
      CONSP (XCDR (obj)) &&
      NILP (XCDR (XCDR (obj))))
    {
      obj = XCAR (XCDR (obj));
      GCPRO2 (obj, printcharfun);
      write_char_internal ("'", printcharfun);
      UNGCPRO;
      print_internal (obj, printcharfun, escapeflag);
      return;
    }

  GCPRO2 (obj, printcharfun);
  write_char_internal ("(", printcharfun);
  {
    int i = 0;
    int max = 0;

    if (INTP (Vprint_length))
      max = XINT (Vprint_length);
    while (CONSP (obj))
      {
	if (i++)
	  write_char_internal (" ", printcharfun);
	if (max && i > max)
	  {
	    write_c_string ("...", printcharfun);
	    break;
	  }
	print_internal (XCAR (obj), printcharfun,
			escapeflag);
	obj = XCDR (obj);
      }
  }
  if (!NILP (obj) && !CONSP (obj))
    {
      write_c_string (" . ", printcharfun);
      print_internal (obj, printcharfun, escapeflag);
    }
  UNGCPRO;
  write_char_internal (")", printcharfun);
  return;
}

void
print_vector (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  print_vector_internal ("[", "]", obj, printcharfun, escapeflag);
}

void
print_string (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_String *s = XSTRING (obj);
  /* We distinguish between Bytecounts and Charcounts, to make
     Vprint_string_length work correctly under Mule.  */
  Charcount size = string_char_length (s);
  Charcount max = size;
  Bytecount bcmax = string_length (s);
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (INTP (Vprint_string_length) &&
      XINT (Vprint_string_length) < max)
    {
      max = XINT (Vprint_string_length);
      bcmax = charcount_to_bytecount (string_data (s), max);
    }
  if (max < 0)
    {
      max = 0;
      bcmax = 0;
    }

  if (!escapeflag)
    {
      /* This deals with GC-relocation and Mule. */
      output_string (printcharfun, 0, obj, 0, bcmax);
      if (max < size)
	write_c_string (" ...", printcharfun);
    }
  else
    {
      Bytecount i, last = 0;

      write_char_internal ("\"", printcharfun);
      for (i = 0; i < bcmax; i++)
	{
	  Bufbyte ch = string_byte (s, i);
	  if (ch == '\"' || ch == '\\'
	      || (ch == '\n' && print_escape_newlines))
	    {
	      if (i > last)
		{
		  output_string (printcharfun, 0, obj, last,
				 i - last);
		}
	      if (ch == '\n')
		{
		  write_c_string ("\\n", printcharfun);
		}
	      else
		{
		  write_char_internal ("\\", printcharfun);
		  /* This is correct for Mule because the
		     character is either \ or " */
		  write_char_internal ((char *) (string_data (s) + i),
				       printcharfun);
		}
	      last = i + 1;
	    }
	}
      if (bcmax > last)
	{
	  output_string (printcharfun, 0, obj, last,
			 bcmax - last);
	}
      if (max < size)
	write_c_string (" ...", printcharfun);
      write_char_internal ("\"", printcharfun);
    }
  UNGCPRO;
  return;
}


static void
default_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			int escapeflag)
{
  struct lcrecord_header *header =
    (struct lcrecord_header *) XPNTR (obj);
  char buf[200];

  if (print_readably)
    error ("printing unreadable object #<%s 0x%x>",
	   LHEADER_IMPLEMENTATION (&header->lheader)->name,
	   header->uid);

  sprintf (buf, "#<%s 0x%x>",
	   LHEADER_IMPLEMENTATION (&header->lheader)->name,
	   header->uid);
  write_c_string (buf, printcharfun);
}

void
internal_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			 int escapeflag)
{
  char buf[200];
  sprintf (buf, "#<INTERNAL OBJECT (XEmacs bug?) (%s) 0x%p>",
	   XRECORD_LHEADER_IMPLEMENTATION (obj)->name,
	   (void *) XPNTR (obj));
  write_c_string (buf, printcharfun);
}

void
print_internal (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  char buf[256];

  QUIT;

  /* Emacs won't print whilst GCing, but an external debugger might */
  if (gc_in_progress) return;

#ifdef I18N3
  /* #### Both input and output streams should have a flag associated
     with them indicating whether output to that stream, or strings
     read from the stream, get translated using Fgettext().  Such a
     stream is called a "translating stream".  For the minibuffer and
     external-debugging-output this is always true on output, and
     with-output-to-temp-buffer sets the flag to true for the buffer
     it creates.  This flag should also be user-settable.  Perhaps it
     should be split up into two flags, one for input and one for
     output. */
#endif

  /* Detect circularities and truncate them.
     No need to offer any alternative--this is better than an error.  */
  if (CONSP (obj) || VECTORP (obj) || COMPILED_FUNCTIONP (obj))
    {
      int i;
      for (i = 0; i < print_depth; i++)
	if (EQ (obj, being_printed[i]))
	  {
	    sprintf (buf, "#%d", i);
	    write_c_string (buf, printcharfun);
	    return;
	  }
    }


  being_printed[print_depth] = obj;
  print_depth++;

  if (print_depth > PRINT_CIRCLE)
    error ("Apparently circular structure being printed");

  switch (XTYPE (obj))
    {
#ifdef USE_MINIMAL_TAGBITS
    case Lisp_Type_Int_Even:
    case Lisp_Type_Int_Odd:
#else
    case Lisp_Type_Int:
#endif
      {
	sprintf (buf, "%ld", (long) XINT (obj));
	write_c_string (buf, printcharfun);
	break;
      }

    case Lisp_Type_Char:
      {
	/* God intended that this be #\..., you know. */
	Emchar ch = XCHAR (obj);
	write_c_string ("?", printcharfun);
	if (ch == '\n')
	  strcpy (buf, "\\n");
	else if (ch == '\r')
	  strcpy (buf, "\\r");
	else if (ch == '\t')
	  strcpy (buf, "\\t");
	else if (ch < 32) {
	  sprintf (buf, "\\^%c", ch + 64);
	  if ((ch + 64) == '\\') {
	    strcat(buf, "\\");
	  }
	} else if (ch == 127)
	  strcpy (buf, "\\^?");
	else if (ch >= 128 && ch < 160)
	  {
	    Bytecount i;
	    strcpy (buf, "\\^");
	    i = set_charptr_emchar ((unsigned char *) (buf + 2), ch + 64);
	    buf[2+i] = '\0';
	  }
	else if (ch < 127
		 && !isdigit (ch)
		 && !isalpha (ch)
		 && ch != '^') /* must not backslash this or it will
				  be interpreted as the start of a
				  control char */
	  sprintf (buf, "\\%c", ch);
	else
	  {
	    Bytecount i;
	    i = set_charptr_emchar ((unsigned char *) buf, ch);
	    buf[i] = '\0';
	  }
	write_c_string (buf, printcharfun);
	break;
      }

#ifndef LRECORD_STRING
    case Lisp_Type_String:
      {
	print_string(obj, printcharfun, escapeflag);
	break;
      }
#endif /* ! LRECORD_STRING */

#ifndef LRECORD_CONS
    case Lisp_Type_Cons:
      {
	struct gcpro gcpro1, gcpro2;

	/* If deeper than spec'd depth, print placeholder.  */
	if (INTP (Vprint_level)
	    && print_depth > XINT (Vprint_level))
	  {
	    GCPRO2 (obj, printcharfun);
	    write_c_string ("...", printcharfun);
	    UNGCPRO;
	    break;
	  }

	print_cons (obj, printcharfun, escapeflag);
	break;
      }
#endif /* ! LRECORD_CONS */

#ifndef LRECORD_VECTOR
    case Lisp_Type_Vector:
      {
	struct gcpro gcpro1, gcpro2;

	/* If deeper than spec'd depth, print placeholder.  */
	if (INTP (Vprint_level)
	    && print_depth > XINT (Vprint_level))
	  {
	    GCPRO2 (obj, printcharfun);
	    write_c_string ("...", printcharfun);
	    UNGCPRO;
	    break;
	  }

	/* God intended that this be #(...), you know. */
	print_vector_internal ("[", "]", obj, printcharfun, escapeflag);
	break;
      }
#endif /* !LRECORD_VECTOR */

#ifndef LRECORD_SYMBOL
    case Lisp_Type_Symbol:
      {
        print_symbol (obj, printcharfun, escapeflag);
        break;
      }
#endif /* !LRECORD_SYMBOL */

    case Lisp_Type_Record:
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);
	struct gcpro gcpro1, gcpro2;

#if defined(LRECORD_CONS) || defined(LRECORD_VECTOR)
	if (CONSP (obj) || VECTORP(obj))
	  {
	    /* If deeper than spec'd depth, print placeholder.  */
	    if (INTP (Vprint_level)
		&& print_depth > XINT (Vprint_level))
	      {
		GCPRO2 (obj, printcharfun);
		write_c_string ("...", printcharfun);
		UNGCPRO;
		break;
	      }
	  }
#endif

	GCPRO2 (obj, printcharfun);
	if (LHEADER_IMPLEMENTATION (lheader)->printer)
	  ((LHEADER_IMPLEMENTATION (lheader)->printer)
	   (obj, printcharfun, escapeflag));
	else
	  default_object_printer (obj, printcharfun, escapeflag);
	UNGCPRO;
	break;
      }

    default:
      {
	/* We're in trouble if this happens!
	   Probably should just abort () */
	if (print_readably)
	  error ("printing illegal data type #o%03o",
		 (int) XTYPE (obj));
	write_c_string ("#<EMACS BUG: ILLEGAL DATATYPE ",
			printcharfun);
	sprintf (buf, "(#o%3o)", (int) XTYPE (obj));
	write_c_string (buf, printcharfun);
	write_c_string
	  (" Save your buffers immediately and please report this bug>",
	   printcharfun);
	break;
      }
    }

  print_depth--;
}

static void
print_compiled_function_internal (CONST char *start, CONST char *end,
				  Lisp_Object obj,
				  Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  struct Lisp_Compiled_Function *b =
    XCOMPILED_FUNCTION (obj); /* GC doesn't relocate */
  int docp = b->flags.documentationp;
  int intp = b->flags.interactivep;
  struct gcpro gcpro1, gcpro2;
  char buf[100];
  GCPRO2 (obj, printcharfun);

  write_c_string (start, printcharfun);
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  if (!print_readably)
    {
      Lisp_Object ann = compiled_function_annotation (b);
      if (!NILP (ann))
	{
	  write_c_string ("(from ", printcharfun);
	  print_internal (ann, printcharfun, 1);
	  write_c_string (") ", printcharfun);
	}
    }
#endif /* COMPILED_FUNCTION_ANNOTATION_HACK */
  /* COMPILED_ARGLIST = 0 */
  print_internal (b->arglist, printcharfun, escapeflag);
  /* COMPILED_BYTECODE = 1 */
  write_char_internal (" ", printcharfun);
  /* we don't really want to see that junk in the bytecode instructions. */
  if (STRINGP (b->bytecodes) && !print_readably)
    {
      sprintf (buf, "\"...(%ld)\"", (long) XSTRING_LENGTH (b->bytecodes));
      write_c_string (buf, printcharfun);
    }
  else
    print_internal (b->bytecodes, printcharfun, escapeflag);
  /* COMPILED_CONSTANTS = 2 */
  write_char_internal (" ", printcharfun);
  print_internal (b->constants, printcharfun, escapeflag);
  /* COMPILED_STACK_DEPTH = 3 */
  sprintf (buf, " %d", b->maxdepth);
  write_c_string (buf, printcharfun);
  /* COMPILED_DOC_STRING = 4 */
  if (docp || intp)
    {
      write_char_internal (" ", printcharfun);
      print_internal (compiled_function_documentation (b), printcharfun,
		      escapeflag);
    }
  /* COMPILED_INTERACTIVE = 5 */
  if (intp)
    {
      write_char_internal (" ", printcharfun);
      print_internal (compiled_function_interactive (b), printcharfun,
		      escapeflag);
    }
  UNGCPRO;
  write_c_string (end, printcharfun);
}

void
print_compiled_function (Lisp_Object obj, Lisp_Object printcharfun,
			 int escapeflag)
{
  /* This function can GC */
  print_compiled_function_internal (((print_readably) ? "#[" :
				     "#<compiled-function "),
				    ((print_readably) ? "]" : ">"),
				    obj, printcharfun, escapeflag);
}

#ifdef LISP_FLOAT_TYPE
void
print_float (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char pigbuf[350];	/* see comments in float_to_string */

  float_to_string (pigbuf, float_data (XFLOAT (obj)));
  write_c_string (pigbuf, printcharfun);
}
#endif /* LISP_FLOAT_TYPE */

void
print_symbol (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  /* #### Bug!! (intern "") isn't printed in some distinguished way */
  /* ####  (the reader also loses on it) */
  struct Lisp_String *name = XSYMBOL (obj)->name;
  Bytecount size = string_length (name);
  struct gcpro gcpro1, gcpro2;

  if (!escapeflag)
    {
      /* This deals with GC-relocation */
      Lisp_Object nameobj;
      XSETSTRING (nameobj, name);
      output_string (printcharfun, 0, nameobj, 0, size);
      return;
    }
  GCPRO2 (obj, printcharfun);

  /* If we print an uninterned symbol as part of a complex object and
     the flag print-gensym is non-nil, prefix it with #n= to read the
     object back with the #n# reader syntax later if needed.  */
  if (!NILP (Vprint_gensym) && NILP (XSYMBOL (obj)->obarray))
    {
      if (print_depth > 1)
	{
	  Lisp_Object tem = Fassq (obj, Vprint_gensym_alist);
	  if (CONSP (tem))
	    {
	      write_char_internal ("#", printcharfun);
	      print_internal (XCDR (tem), printcharfun, escapeflag);
	      write_char_internal ("#", printcharfun);
	      return;
	    }
	  else
	    {
	      if (CONSP (Vprint_gensym_alist))
		{
		  /* Vprint_gensym_alist is exposed to Lisp, so we
                     have to be careful.  */
		  CHECK_CONS (XCAR (Vprint_gensym_alist));
		  CHECK_INT (XCDR (XCAR (Vprint_gensym_alist)));
		  XSETINT (tem, XINT (XCDR (XCAR (Vprint_gensym_alist))) + 1);
		}
	      else
		XSETINT (tem, 1);
	      Vprint_gensym_alist = Fcons (Fcons (obj, tem), Vprint_gensym_alist);

	      write_char_internal ("#", printcharfun);
	      print_internal (tem, printcharfun, escapeflag);
	      write_char_internal ("=", printcharfun);
	    }
	}
      write_c_string ("#:", printcharfun);
    }

  /* Does it look like an integer or a float? */
  {
    Bufbyte *data = string_data (name);
    Bytecount confusing = 0;

    if (size == 0)
      goto not_yet_confused;    /* Really confusing */
    else if (isdigit (data[0]))
      confusing = 0;
    else if (size == 1)
      goto not_yet_confused;
    else if (data[0] == '-' || data[0] == '+')
      confusing = 1;
    else
      goto not_yet_confused;

    for (; confusing < size; confusing++)
      {
        if (!isdigit (data[confusing]))
          {
            confusing = 0;
            break;
          }
      }
  not_yet_confused:

#ifdef LISP_FLOAT_TYPE
    if (!confusing)
      confusing = isfloat_string ((char *) data);
#endif
    if (confusing)
      write_char_internal ("\\", printcharfun);
  }

  {
    Lisp_Object nameobj;
    Bytecount i;
    Bytecount last = 0;

    XSETSTRING (nameobj, name);
    for (i = 0; i < size; i++)
      {
	Bufbyte c = string_byte (name, i);

	if (c == '\"' || c == '\\' || c == '\'' || c == ';' || c == '#' ||
	    c == '(' || c == ')' || c == ',' || c =='.' || c == '`' ||
	    c == '[' || c == ']' || c == '?' || c <= 040)
	  {
	    if (i > last)
	      {
		output_string (printcharfun, 0, nameobj, last,
			       i - last);
	      }
	    write_char_internal ("\\", printcharfun);
	    last = i;
	  }
      }
    output_string (printcharfun, 0, nameobj, last, size - last);
  }
  UNGCPRO;
}

int alternate_do_pointer;
char alternate_do_string[5000];

DEFUN ("alternate-debugging-output", Falternate_debugging_output, 1, 1, 0, /*
Append CHARACTER to the array `alternate_do_string'.
This can be used in place of `external-debugging-output' as a function
to be passed to `print'.  Before calling `print', set `alternate_do_pointer'
to 0.

*/
       (character))
{
  Bufbyte str[MAX_EMCHAR_LEN];
  Bytecount len;
  int extlen;
  CONST Extbyte *extptr;

  CHECK_CHAR_COERCE_INT (character);
  len = set_charptr_emchar (str, XCHAR (character));
  GET_CHARPTR_EXT_DATA_ALLOCA (str, len, FORMAT_TERMINAL, extptr, extlen);
  memcpy (alternate_do_string + alternate_do_pointer, extptr, extlen);
  alternate_do_pointer += extlen;
  alternate_do_string[alternate_do_pointer] = 0;
  return character;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output, 1, 3, 0, /*
Write CHAR-OR-STRING to stderr or stdout.
If optional arg STDOUT-P is non-nil, write to stdout; otherwise, write
to stderr.  You can use this function to write directly to the terminal.
This function can be used as the STREAM argument of Fprint() or the like.

If you have opened a termscript file (using `open-termscript'), then
the output also will be logged to this file.
*/
       (char_or_string, stdout_p, device))
{
  FILE *file = 0;
  struct console *con = 0;

  if (NILP (device))
    {
      if (!NILP (stdout_p))
	file = stdout;
      else
	file = stderr;
    }
  else
    {
      CHECK_LIVE_DEVICE (device);
      if (!DEVICE_TTY_P (XDEVICE (device)) &&
	  !DEVICE_STREAM_P (XDEVICE (device)))
	signal_simple_error ("Must be tty or stream device", device);
      con = XCONSOLE (DEVICE_CONSOLE (XDEVICE (device)));
      if (DEVICE_TTY_P (XDEVICE (device)))
	file = 0;
      else if (!NILP (stdout_p))
	file = CONSOLE_STREAM_DATA (con)->outfd;
      else
	file = CONSOLE_STREAM_DATA (con)->errfd;
    }

  if (STRINGP (char_or_string))
    write_string_to_stdio_stream (file, con,
				  XSTRING_DATA (char_or_string),
				  0, XSTRING_LENGTH (char_or_string),
				  FORMAT_TERMINAL);
  else
    {
      Bufbyte str[MAX_EMCHAR_LEN];
      Bytecount len;

      CHECK_CHAR_COERCE_INT (char_or_string);
      len = set_charptr_emchar (str, XCHAR (char_or_string));
      write_string_to_stdio_stream (file, con, str, 0, len, FORMAT_TERMINAL);
    }

  return char_or_string;
}

DEFUN ("open-termscript", Fopen_termscript, 1, 1, "FOpen termscript file: ", /*
Start writing all terminal output to FILE as well as the terminal.
FILE = nil means just close any termscript file currently open.
*/
       (file))
{
  /* This function can GC */
  if (termscript != 0)
    fclose (termscript);
  termscript = 0;

  if (! NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      termscript = fopen ((char *) XSTRING_DATA (file), "w");
      if (termscript == NULL)
	report_file_error ("Opening termscript", Fcons (file, Qnil));
    }
  return Qnil;
}

#if 1
/* Debugging kludge -- unbuffered */
static int debug_print_length = 50;
static int debug_print_level = 15;
Lisp_Object debug_temp;
void debug_print_no_newline (Lisp_Object debug_print_obj);
void
debug_print_no_newline (Lisp_Object debug_print_obj)
{
  /* This function can GC */
  int old_print_readably = print_readably;
  int old_print_depth = print_depth;
  Lisp_Object old_print_length = Vprint_length;
  Lisp_Object old_print_level = Vprint_level;
  Lisp_Object old_inhibit_quit = Vinhibit_quit;
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (old_print_level, old_print_length, old_inhibit_quit);

  if (gc_in_progress)
    stderr_out ("** gc-in-progress!  Bad idea to print anything! **\n");

  print_depth = 0;
  print_readably = 0;
  print_unbuffered++;
  /* Could use unwind-protect, but why bother? */
  if (debug_print_length > 0)
    Vprint_length = make_int (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_int (debug_print_level);
  print_internal (debug_print_obj, Qexternal_debugging_output, 1);
  Vinhibit_quit = old_inhibit_quit;
  Vprint_level = old_print_level;
  Vprint_length = old_print_length;
  print_depth = old_print_depth;
  print_readably = old_print_readably;
  print_unbuffered--;
  UNGCPRO;
}

void debug_print (Lisp_Object debug_print_obj);
void
debug_print (Lisp_Object debug_print_obj)
{
  debug_print_no_newline (debug_print_obj);
  stderr_out ("\r\n");
  fflush (stderr);
}

/* Debugging kludge -- unbuffered */
void
debug_backtrace (void)
{
  /* This function can GC */
  int old_print_readably = print_readably;
  int old_print_depth = print_depth;
  Lisp_Object old_print_length = Vprint_length;
  Lisp_Object old_print_level = Vprint_level;
  Lisp_Object old_inhibit_quit = Vinhibit_quit;
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (old_print_level, old_print_length, old_inhibit_quit);

  if (gc_in_progress)
    stderr_out ("** gc-in-progress!  Bad idea to print anything! **\n");

  print_depth = 0;
  print_readably = 0;
  print_unbuffered++;
  /* Could use unwind-protect, but why bother? */
  if (debug_print_length > 0)
    Vprint_length = make_int (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_int (debug_print_level);
  Fbacktrace (Qexternal_debugging_output, Qt);
  stderr_out ("\n");
  fflush (stderr);
  Vinhibit_quit = old_inhibit_quit;
  Vprint_level = old_print_level;
  Vprint_length = old_print_length;
  print_depth = old_print_depth;
  print_readably = old_print_readably;
  print_unbuffered--;
  UNGCPRO;
}

void
debug_short_backtrace (int length)
{
  int first = 1;
  struct backtrace *bt = backtrace_list;
  stderr_out ("   [");
  fflush (stderr);
  while (length > 0 && bt)
    {
      if (!first)
	{
	  stderr_out (", ");
	  fflush (stderr);
	}
      if (COMPILED_FUNCTIONP (*bt->function))
	{
#if defined(COMPILED_FUNCTION_ANNOTATION_HACK)
	  Lisp_Object ann = Fcompiled_function_annotation (*bt->function);
#else
	  Lisp_Object ann = Qnil;
#endif
	  if (!NILP (ann))
	    {
	      stderr_out ("<compiled-function from ");
	      fflush (stderr);
	      debug_print_no_newline (ann);
	      stderr_out (">");
	      fflush (stderr);
	    }
	  else
	    {
	      stderr_out ("<compiled-function of unknown origin>");
	      fflush (stderr);
	    }
	}
      else
	debug_print_no_newline (*bt->function);
      first = 0;
      length--;
      bt = bt->next;
    }
  stderr_out ("]\n");
  fflush (stderr);
}

#endif /* debugging kludge */


void
syms_of_print (void)
{
  defsymbol (&Qprint_escape_newlines, "print-escape-newlines");
  defsymbol (&Qprint_readably, "print-readably");

  defsymbol (&Qstandard_output, "standard-output");

#ifdef LISP_FLOAT_TYPE
  defsymbol (&Qfloat_output_format, "float-output-format");
#endif

  defsymbol (&Qprint_length, "print-length");

  defsymbol (&Qprint_string_length, "print-string-length");

  defsymbol (&Qdisplay_error, "display-error");
  defsymbol (&Qprint_message_label, "print-message-label");

  DEFSUBR (Fprin1);
  DEFSUBR (Fprin1_to_string);
  DEFSUBR (Fprinc);
  DEFSUBR (Fprint);
  DEFSUBR (Ferror_message_string);
  DEFSUBR (Fdisplay_error);
  DEFSUBR (Fterpri);
  DEFSUBR (Fwrite_char);
  DEFSUBR (Falternate_debugging_output);
  defsymbol (&Qalternate_debugging_output, "alternate-debugging-output");
  DEFSUBR (Fexternal_debugging_output);
  DEFSUBR (Fopen_termscript);
  defsymbol (&Qexternal_debugging_output, "external-debugging-output");
#ifndef standalone
  DEFSUBR (Fwith_output_to_temp_buffer);
#endif /* not standalone */
}

void
lstream_type_create_print (void)
{
  LSTREAM_HAS_METHOD (print, writer);
  LSTREAM_HAS_METHOD (print, marker);
}

void
vars_of_print (void)
{
  alternate_do_pointer = 0;

  DEFVAR_LISP ("standard-output", &Vstandard_output /*
Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the minibuffer line).
*/ );
  Vstandard_output = Qt;

#ifdef LISP_FLOAT_TYPE
  DEFVAR_LISP ("float-output-format", &Vfloat_output_format /*
The format descriptor string that lisp uses to print floats.
This is a %-spec like those accepted by `printf' in C,
but with some restrictions.  It must start with the two characters `%.'.
After that comes an integer precision specification,
and then a letter which controls the format.
The letters allowed are `e', `f' and `g'.
Use `e' for exponential notation "DIG.DIGITSeEXPT"
Use `f' for decimal point notation "DIGITS.DIGITS".
Use `g' to choose the shorter of those two formats for the number at hand.
The precision in any of these cases is the number of digits following
the decimal point.  With `f', a precision of 0 means to omit the
decimal point.  0 is not allowed with `f' or `g'.

A value of nil means to use `%.16g'.

Regardless of the value of `float-output-format', a floating point number
will never be printed in such a way that it is ambiguous with an integer;
that is, a floating-point number will always be printed with a decimal
point and/or an exponent, even if the digits following the decimal point
are all zero.  This is to preserve read-equivalence.
*/ );
  Vfloat_output_format = Qnil;
#endif /* LISP_FLOAT_TYPE */

  DEFVAR_LISP ("print-length", &Vprint_length /*
Maximum length of list or vector to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_length = Qnil;

  DEFVAR_LISP ("print-string-length", &Vprint_string_length /*
Maximum length of string to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_string_length = Qnil;

  DEFVAR_LISP ("print-level", &Vprint_level /*
Maximum depth of list nesting to print before abbreviating.
A value of nil means no limit.
*/ );
  Vprint_level = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines /*
Non-nil means print newlines in strings as backslash-n.
*/ );
  print_escape_newlines = 0;

  DEFVAR_BOOL ("print-readably", &print_readably /*
If non-nil, then all objects will be printed in a readable form.
If an object has no readable representation, then an error is signalled.
When print-readably is true, compiled-function objects will be written in
 #[...] form instead of in #<compiled-function [...]> form, and two-element
 lists of the form (quote object) will be written as the equivalent 'object.
Do not SET this variable; bind it instead.
*/ );
  print_readably = 0;

  /* #### I think this should default to t.  But we'd better wait
     until we see that it works out.  */
  DEFVAR_LISP ("print-gensym", &Vprint_gensym /*
If non-nil, then uninterned symbols will be printed specially.
Uninterned symbols are those which are not present in `obarray', that is,
those which were made with `make-symbol' or by calling `intern' with a
second argument.

When print-gensym is true, such symbols will be preceded by "#:",
which causes the reader to create a new symbol instead of interning
and returning an existing one.  Beware: the #: syntax creates a new
symbol each time it is seen, so if you print an object which contains
two pointers to the same uninterned symbol, `read' will not duplicate
that structure.

If the value of `print-gensym' is a cons cell, then in addition
refrain from clearing `print-gensym-alist' on entry to and exit from
printing functions, so that the use of #...# and #...= can carry over
for several separately printed objects.
*/ );
  Vprint_gensym = Qnil;

  DEFVAR_LISP ("print-gensym-alist", &Vprint_gensym_alist /*
Association list of elements (GENSYM . N) to guide use of #N# and #N=.
In each element, GENSYM is an uninterned symbol that has been associated
with #N= for the specified value of N.
*/ );
  Vprint_gensym_alist = Qnil;

  DEFVAR_LISP ("print-message-label", &Vprint_message_label /*
Label for minibuffer messages created with `print'.  This should
generally be bound with `let' rather than set.  (See `display-message'.)
*/ );
  Vprint_message_label = Qprint;

  Vprin1_to_string_stream = Qnil;
  staticpro (&Vprin1_to_string_stream);
}
