/* Lisp object printing and output streams.
   Copyright (C) 1985, 1986, 1988, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2005, 2010 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* This file has been Mule-ized. */

/* Seriously divergent from FSF by this point.

   Seriously hacked on by Ben Wing for Mule.  All stdio code also by Ben,
   as well as the debugging code (initial version of debug_print(), though,
   by Jamie Zawinski) and the _fmt interfaces.  Also a fair amount of work
   by Hrvoje, e.g. floating-point code and rewriting to avoid O(N^2)
   consing when outputting to the echo area.  Print-circularity code by
   Martin? */

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "device-impl.h"
#include "extents.h"
#include "frame.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"

#include "console-tty-impl.h"
#include "console-stream-impl.h"
#ifdef WIN32_NATIVE
#include "console-msw.h"
#endif

#include "sysfile.h"
#include "elhash.h"
#include "chartab.h"

#include <float.h>
/* Define if not in float.h */
#ifndef DBL_DIG
#define DBL_DIG 16
#endif

Lisp_Object Vstandard_output, Qstandard_output;

/* The subroutine object for external-debugging-output is kept here
   for the convenience of the debugger.  */
Lisp_Object Qexternal_debugging_output, Qalternate_debugging_output;

#ifdef HAVE_MS_WINDOWS
Lisp_Object Qmswindows_debugging_output;
#endif

/* Avoid actual stack overflow in print.  */
static int print_depth;

/* Detect most circularities to print finite output.  */
#define PRINT_CIRCLE_LIMIT 200
static Lisp_Object being_printed[PRINT_CIRCLE_LIMIT];

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

/* Maximum length of char tables, range tables, etc. to print; noninteger
   means effectively infinity */
Lisp_Object Vprint_table_nonreadably_length;

/* Label to use when making echo-area messages. */

Lisp_Object Vprint_message_label;

/* Nonzero means print newlines in strings as \n.  */
Boolint print_escape_newlines;

Boolint print_readably;

/* Non-zero means print #: before uninterned symbols, and use the #n= and
   #n# syntax for them.  */
Boolint print_gensym;

/* Non-zero means print recursive structures using #n= and #n# syntax.  */
Boolint print_circle;

/* Non-zero means keep continuous numbers for #n= and #n# syntax between
   several print functions. Setting or binding the corresponding Lisp
   variable to a non-nil value silently *clears* Vprint_number_table. */
Boolint print_continuous_numbering;

/* Vprint_number_table is a hash table mapping objects to their statuses for
   this print operation. The statuses are represented by integers. */
Lisp_Object Vprint_number_table;

/* These describe the bit fields of the integers in Vprint_number_table. */
enum PRINT_NUMBER_FIELDS {
  /* Lowest four bits describe the number of times a given object has
     been seen, allowing entries to be manipulated cheaply by
     inchash_eq() when encountered. */
  PRINT_NUMBER_SEEN_MASK = 0xF,

  /* The next twenty-five bits give the sequence number for the object,
     corresponding to the order in which print_preprocess encountered the
     objects; as such, it's related to print_number_index. */
  PRINT_NUMBER_ORDINAL_MASK = 0x1FFFFFF0,
  PRINT_NUMBER_ORDINAL_SHIFT = 4,

  /* And the next bit describes whether the object has already been printed
     in this print operation (or in these print operations, if
     print-continuous-numbering is relevant).  */
  PRINT_NUMBER_PRINTED_MASK = 0x20000000,
};

/* Reflects the number of repeated or possibly-repeated objects encountered
   by print_preprocess(); reset whenever Vprint_number_table is cleared. */
Elemcount print_number_index;

Lisp_Object Qdisplay_error;
Lisp_Object Qprint_message_label;

Lisp_Object Qwrite_sequence;

/* Force immediate output of all printed data.  Used for debugging. */
int print_unbuffered;

/* Non-zero if in debug-printing */
int in_debug_print;

FILE *termscript;	/* Stdio stream being used for copy of all output.  */

static Bytecount write_string_to_alternate_debugging_output (const Ibyte *str,
							     Bytecount len);

/* To avoid consing in debug_prin1, we package up variables we need to bind
   into an opaque object. */
struct debug_bindings 
{
  int inhibit_non_essential_conversion_operations;
  int print_depth;
  int print_readably;
  int print_unbuffered;
  int print_circle;
  int in_debug_print;
  int gc_currently_forbidden;
  Lisp_Object Vprint_length;
  Lisp_Object Vprint_level;
  Lisp_Object Vinhibit_quit;
};

static int begin_inhibit_non_essential_conversion_operations (void);



int stdout_needs_newline;
int stdout_clear_before_next_output;

/* Basic function to actually write to a stdio stream or TTY console. */

static Bytecount
write_string_to_stdio_stream_1 (FILE *stream, struct console *con,
				const Ibyte *ptr, Bytecount len,
				int must_flush)
{
  Extbyte *extptr = 0;
  Bytecount extlen = 0, result;
  Boolint output_is_std_handle =
    stream ? stream == stdout || stream == stderr :
    CONSOLE_TTY_DATA (con)->is_stdio;
  
  if (stream || output_is_std_handle)
    {
      if (initialized && !inhibit_non_essential_conversion_operations)
	TO_EXTERNAL_FORMAT (DATA, (ptr, len),
			    ALLOCA, (extptr, extlen),
			    Qterminal);
      else
	{
#ifdef NON_ASCII_INTERNAL_FORMAT
#error Do something here
#else
	  extptr = (Extbyte *) ptr;
	  extlen = (Bytecount) len;
#endif
	}
    }

  if (stream)
    {
#ifdef WIN32_NATIVE
      HANDLE errhand = GetStdHandle (STD_INPUT_HANDLE);
      int no_useful_stderr = errhand == 0 || errhand == INVALID_HANDLE_VALUE;

      if (!no_useful_stderr)
	no_useful_stderr = !PeekNamedPipe (errhand, 0, 0, 0, 0, 0);
      /* we typically have no useful stdout/stderr under windows if we're
	 being invoked graphically. */
      if (no_useful_stderr)
	mswindows_output_console_string (ptr, len);
      else
#endif
	{
	  result = retry_fwrite (extptr, 1, extlen, stream);
#ifdef WIN32_NATIVE
	  /* Q122442 says that pipes are "treated as files, not as
	     devices", and that this is a feature. Before I found that
	     article, I thought it was a bug. Thanks MS, I feel much
	     better now. - kkm */
	  must_flush = 1;
#endif
	  if (must_flush)
	    fflush (stream);
	}
    }
  else
    /* The stream itself does conversion to external format */
    result = Lstream_write (XLSTREAM (CONSOLE_TTY_DATA (con)->outstream),
			    ptr, len);

  if (output_is_std_handle)
    {
      if (termscript)
	{
	  retry_fwrite (extptr, 1, extlen, termscript);
	  fflush (termscript);
	}
      stdout_needs_newline = (ptr[len - 1] != '\n');
    }

  return result;
}

/* Write to a stdio stream or TTY console, first clearing the left side
   if necessary. */

static Bytecount
write_string_to_stdio_stream (FILE *stream, struct console *con,
			      const Ibyte *ptr, Bytecount len,
			      int must_flush)
{
  if (stdout_clear_before_next_output &&
      (stream ? stream == stdout || stream == stderr :
       CONSOLE_TTY_DATA (con)->is_stdio))
    {
      if (stdout_needs_newline)
	write_string_to_stdio_stream_1 (stream, con, (Ibyte *) "\n", 1,
					must_flush);
      stdout_clear_before_next_output = 0;
    }

  return write_string_to_stdio_stream_1 (stream, con, ptr, len, must_flush);
}

/*
    EXT_PRINT_STDOUT    = stdout or its equivalent (may be a
                          console window under MS Windows)
    EXT_PRINT_STDERR    = stderr or its equivalent (may be a
                          console window under MS Windows)
    EXT_PRINT_ALTERNATE = an internal character array; see
                          `alternate-debugging-output'
    EXT_PRINT_MSWINDOWS = Under MS Windows, the "debugging output" that
                          debuggers can hook into; uses OutputDebugString()
                          system call
    EXT_PRINT_ALL       = all of the above except stdout
*/

enum ext_print
  {
    EXT_PRINT_STDOUT = 1,
    EXT_PRINT_STDERR = 2,
    EXT_PRINT_ALTERNATE = 4,
    EXT_PRINT_MSWINDOWS = 8,
    EXT_PRINT_ALL = 14
  };

static Bytecount
write_string_to_external_output (const Ibyte *ptr, Bytecount len,
				 int dest)
{
  Bytecount result = 0, output;
  if (dest & EXT_PRINT_STDOUT)
    {
      output = write_string_to_stdio_stream (stdout, 0, ptr, len, 1);
      result = min (result, output);
    }
  if (dest & EXT_PRINT_STDERR)
    {
      output = write_string_to_stdio_stream (stderr, 0, ptr, len, 1);
      result = min (result, output);      
    }
  if (dest & EXT_PRINT_ALTERNATE)
    {
      output = write_string_to_alternate_debugging_output (ptr, len);
      result = min (result, output);
    }
#ifdef WIN32_NATIVE
  if (dest & EXT_PRINT_MSWINDOWS)
    {
      output = write_string_to_mswindows_debugging_output (ptr, len);
      result = min (result, output);
    }
#endif

  return result;
}

/* This function can be called from fatal_error_signal() and so should make as
   few assumptions as possible about what allocation is
   available.

   emacs_vsnprintf(), which does the work of formatting, jumps through
   significant hoops to ensure that all its (usually) dynamic data structures
   are precalculated and allocated on the stack, and so it sidesteps the
   typical dependence of the doprnt.c functions on a working C heap (the
   Dynarr for the parsed specs requires malloc()) and a working Lisp object
   allocation system (for the lstream objects).

   In addition, if there are actually no format specs in FMT,
   emacs_vsnprintf() just does an memmove(), which is even less likely to go
   pear-shaped.

   Both emacs_vsnprintf() and write_string_to_external_output_va() will fail
   if we run out of stack space. Oh well. */
static Bytecount
write_string_to_external_output_va (const CIbyte *fmt, va_list args,
				    int dest)
{
  Ibyte kludge[4096];
  Bytecount klen;

  if (initialized && !inhibit_non_essential_conversion_operations)
    fmt = GETTEXT (fmt);

  klen = emacs_vsnprintf (kludge, sizeof (kludge), fmt, args);

  write_string_to_external_output (kludge,
                                   min (klen, (Bytecount) sizeof (kludge)),
                                   dest);

  return klen;
}

/* Output portably to print destination as specified by DEST. */

Bytecount
external_out (int dest, const CIbyte *fmt, ...)
{
  Bytecount result;
  va_list args;
  va_start (args, fmt);
  result = write_string_to_external_output_va (fmt, args, dest);
  va_end (args);

  return result;
}

DOESNT_RETURN
fatal (const CIbyte *fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  stderr_out ("\nXEmacs: fatal error: ");
  write_string_to_external_output_va (fmt, args, EXT_PRINT_STDERR);
  stderr_out ("\n");

  va_end (args);
  exit (1);
}

/* Write a string to the output location specified in FUNCTION.
   Arguments NONRELOC, RELOC, OFFSET, and LEN are as in
   buffer_insert_string_1() in insdel.c.

   FUNCTION is one of

   -- an lstream
   -- a buffer (insert at point and advance point)
   -- a marker (insert at marker and advance marker)
   -- a frame (append to echo area; clear echo area first if
               `print-message-label' has changed since the last time)
   -- t or nil (send to stdout)
   -- a Lisp function of one argument (call to get data output)

   Use Qexternal_debugging_output to get output to stderr.
*/

static Bytecount
output_string (Lisp_Object function, const Ibyte *nonreloc,
	       Lisp_Object reloc, Bytecount offset, Bytecount len)
{
  /* This function can GC */
  /* We change the value of nonreloc (fetching it from reloc as
     necessary), but we don't want to pass this changed value on to
     other functions that take both a nonreloc and a reloc, or things
     may get confused and an assertion failure in
     fixup_internal_substring() may get triggered. */
  const Ibyte *newnonreloc;
  Bytecount result;

  /* Emacs won't print while GCing, but an external debugger might */
#ifdef NO_PRINT_DURING_GC
  if (gc_in_progress) return 0;
#endif

  fixup_internal_substring (nonreloc, reloc, offset, &len);

  newnonreloc = STRINGP (reloc) ? XSTRING_DATA (reloc) : nonreloc;

  if (LSTREAMP (function))
    {
      struct gcpro gcpro1, gcpro2;
      GCPRO2 (reloc, function);

      if (STRINGP (reloc))
	{
          /* We used to inhibit GC here. There's no need, the only Lstreams
             that may funcall + GC are the Lisp buffer lstreams, and the
             buffer insertion code is perfectly able to handle relocation of
             string data, as we see in the next clause. There used to be a
             print_stream lstream that called output_string recursively, and
             that would have tripped the problem, but no more. Plus, we now
             have write_with_extents (), which knows it has been handed a Lisp
             string, and can take appropriate action to re-fetch string
             data. */
	  result = Lstream_write_with_extents (XLSTREAM (function), reloc,
					       offset, len);
	}
      else
        {
          result = Lstream_write (XLSTREAM (function), newnonreloc + offset,
				  len);
        }

      if (print_unbuffered)
	Lstream_flush (XLSTREAM (function));

      RETURN_UNGCPRO (result);
    }
  else if (BUFFERP (function))
    {
      struct gcpro gcpro1;

      CHECK_LIVE_BUFFER (function);

      GCPRO1 (reloc);
      
      buffer_insert_string (XBUFFER (function), nonreloc, reloc, offset, len);

      RETURN_UNGCPRO (len);
    }
  else if (MARKERP (function))
    {
      struct gcpro gcpro1;
      GCPRO1 (reloc);

      buffer_insert_string_1 (XMARKER (function)->buffer,
			      /* marker_position() will err if marker
				 doesn't point anywhere.  */
			      marker_position (function), nonreloc, reloc,
			      offset, len, -1, 0);
      set_byte_marker_position (function,
				byte_marker_position (function) + len);
      RETURN_UNGCPRO (len); /* We will have errored on failure. */
    }
  else if (FRAMEP (function))
    {
      /* This gets used by functions not invoking print_prepare(),
         such as Fwrite_char, Fterpri, etc..  */
      struct frame *f = XFRAME (function);
      struct gcpro gcpro1;

      CHECK_LIVE_FRAME (function);

      GCPRO1 (reloc);

      if (!EQ (Vprint_message_label, echo_area_status (f)))
	clear_echo_area_from_print (f, Qnil, 1);
      echo_area_append (f, nonreloc, reloc, offset, len, Vprint_message_label);

      RETURN_UNGCPRO (len);
    }
  else if (EQ (function, Qt) || EQ (function, Qnil))
    {
      return write_string_to_stdio_stream (stdout, 0, newnonreloc + offset,
					   len, print_unbuffered);
    }
  else if (EQ (function, Qexternal_debugging_output))
    {
      /* This is not strictly necessary, and somewhat of a hack, but it avoids
	 having each character passed separately to
	 `external-debugging-output'.  The API says to pass each character
	 separately because that is the Lisp Way. */
      return write_string_to_stdio_stream (stderr, 0, newnonreloc + offset,
					   len, print_unbuffered);
    }
  else
    {
      Bytecount end = offset + len;
      struct gcpro gcpro1;

      GCPRO1 (reloc);

      while (offset < end)
	{
	  /* call1() GCPROs FUNCTION. */
	  call1 (function, make_char (itext_ichar (newnonreloc + offset)));

          if (STRINGP (reloc))
            {
              newnonreloc = XSTRING_DATA (reloc);

              /* FUNCTION may have modified the byte length of RELOC and
                 relocated it, update our pointer. */
              if (offset >= XSTRING_LENGTH (reloc) || 
                  !valid_ibyteptr_p (newnonreloc + offset))
                {
                  /* Error if we would run off the end of the string, or print
                     corrupt data. We don't do the character accounting that
                     print_string does, since we don't have the character
                     count info available for free, and it doesn't make sense
                     to add it for something that will happen this rarely. */
                  invalid_state ("string modified while printing it", reloc);
                  break;
                }
            }

          offset += itext_ichar_len (newnonreloc + offset);
	}

      RETURN_UNGCPRO (len);
    }
}

static int
print_continuous_numbering_changed (Lisp_Object UNUSED (sym),
                                    Lisp_Object *val,
                                    Lisp_Object UNUSED (in_object),
                                    int UNUSED (flags))
{
  if (!NILP (*val) && !print_continuous_numbering)
    {
      Fclrhash (Vprint_number_table);
      print_number_index = 0;
    }

  return 0;
}

#define RESET_PRINT_NUMBER_TABLE do {           \
    if (!print_continuous_numbering)            \
      {                                         \
        Fclrhash (Vprint_number_table);         \
        print_number_index = 0;                 \
      }                                         \
  } while (0)

Lisp_Object
canonicalize_printcharfun (Lisp_Object printcharfun)
{
  if (NILP (printcharfun))
    printcharfun = Vstandard_output;

  if (!noninteractive && (EQ (printcharfun, Qt) || NILP (printcharfun)))
    printcharfun = Fselected_frame (Qnil); /* print to minibuffer */

  return printcharfun;
}

static Lisp_Object
print_prepare (Lisp_Object printcharfun, Lisp_Object *frame_kludge)
{
  /* Emacs won't print while GCing, but an external debugger might */
#ifdef NO_PRINT_DURING_GC
  if (gc_in_progress)
    return Qnil;
#endif

  RESET_PRINT_NUMBER_TABLE;

  printcharfun = canonicalize_printcharfun (printcharfun);

  /* Here we could safely return the canonicalized PRINTCHARFUN.
     However, if PRINTCHARFUN is a frame, printing of complex
     structures becomes very expensive, because `append-message'
     (called by echo_area_append) gets called as many times as
     output_string() is called (and that's a *lot*).  append-message
     tries to keep top of the message-stack in sync with the contents
     of " *Echo Area" buffer, consing a new string for each component
     of the printed structure.  For instance, if you print (a a),
     append-message will cons up the following strings:

         "("
	 "(a"
	 "(a "
	 "(a a"
	 "(a a)"

     and will use only the last one.  With larger objects, this turns
     into an O(n^2) consing frenzy that locks up XEmacs in incessant
     garbage collection.

     We prevent this by creating a resizing_buffer stream and letting
     the printer write into it.  print_finish() will notice this
     stream, and invoke echo_area_append() with the stream's buffer,
     only once.  */
  if (FRAMEP (printcharfun))
    {
      CHECK_LIVE_FRAME (printcharfun);
      *frame_kludge = printcharfun;
      printcharfun = make_resizing_buffer_output_stream ();
    }

  return printcharfun;
}

static void
print_finish (Lisp_Object stream, Lisp_Object frame_kludge)
{
  /* Emacs won't print while GCing, but an external debugger might */
#ifdef NO_PRINT_DURING_GC
  if (gc_in_progress)
    return;
#endif

  RESET_PRINT_NUMBER_TABLE;

  /* See the comment in print_prepare().  */
  if (FRAMEP (frame_kludge))
    {
      struct frame *f = XFRAME (frame_kludge);
      Lstream *str = XLSTREAM (stream);
      Lisp_Object printed = Qnil;
      struct gcpro gcpro1;

      CHECK_LIVE_FRAME (frame_kludge);
      Lstream_flush (str);

      GCPRO1 (printed);

      if (!EQ (Vprint_message_label, echo_area_status (f)))
	clear_echo_area_from_print (f, Qnil, 1);

      if (Lstream_extent_info (str) != NULL)
        {
          /* Only create the string if there is associated extent info,
             otherwise no need to allocate something that will be immediately
             GCed. */
          printed = resizing_buffer_to_lisp_string (str);          
          echo_area_append (f, NULL, printed, 0, XSTRING_LENGTH (printed),
                            Vprint_message_label);
        }
      else
        {
          echo_area_append (f, resizing_buffer_stream_ptr (str), Qnil, 0,
                            Lstream_byte_count (str), Vprint_message_label);
        }
          
      Lstream_delete (str);
      UNGCPRO;
    }
}

/* Write a Lisp string to STREAM, preserving extent data if STREAM can handle
   it, and protecting its string data from relocation when appropriate. */
Bytecount
write_lisp_string (Lisp_Object stream, Lisp_Object string, Bytecount offset,
                   Bytecount len)
{
  /* This function can GC */
  return output_string (stream, NULL, string, offset, len);
}  

/* Write internal-format data to STREAM.  See output_string() for
   interpretation of STREAM.

   NOTE: Do not call this with the data of a Lisp_String, as printcharfun
   might cause the octet length of the string to be changed, which might cause
   the string's data to be relocated.  You will also discard any extent data,
   which is usually the wrong thing to do. To write a Lisp string, use
   write_lisp_string (), above.

   You could also use print_internal (string, printcharfun, 0), but that will
   be truncated depending on the value of PRINT-STRING-LENGTH, which you
   probably don't want.

   Also note that STREAM should be the result of canonicalize_printcharfun()
   (i.e. Qnil means stdout, not Vstandard_output, etc.)  */
Bytecount
write_string_1 (Lisp_Object stream, const Ibyte *str, Bytecount size)
{
  /* This function can GC */
  text_checking_assert (size >= 0);
  return output_string (stream, str, Qnil, 0, size);
}

Bytecount
write_eistring (Lisp_Object stream, const Eistring *ei)
{
  return write_string_1 (stream, eidata (ei), eilen (ei));
}

DEFUN ("write-char", Fwrite_char, 1, 2, 0, /*
Output character CHARACTER to stream STREAM.
STREAM defaults to the value of `standard-output' (which see).
*/
       (character, stream))
{
  /* This function can GC */
  Ibyte str[MAX_ICHAR_LEN];
  Bytecount len;

  CHECK_CHAR_COERCE_INT (character);
  len = set_itext_ichar (str, XCHAR (character));
  output_string (canonicalize_printcharfun (stream), str, Qnil, 0, len);
  return character;
}

DEFUN ("write-sequence", Fwrite_sequence, 1, MANY, 0, /*
Output string, list, vector or bit-vector SEQUENCE to STREAM.

STREAM defaults to the value of `standard-output', which see.

Keywords :start and :end, if given, specify indices of a subsequence
of SEQUENCE to output.  They default to 0 and nil, meaning write the
entire sequence.

Elements of SEQUENCE can be characters (all are accepted by this function,
though they may be corrupted depending on the coding system associated with
STREAM) or integers below #x100, which are treated as equivalent to the
characters with the corresponding code. This function is from Common Lisp,
rather GNU Emacs API, so GNU Emacs' character-integer equivalence doesn't
hold.

Returns SEQUENCE (not the subsequence of SEQUENCE that has been written to
STREAM).

arguments: (SEQUENCE &optional STREAM &key (START 0) END)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object sequence = args[0], stream = (nargs > 1) ? args[1] : Qnil;
  Lisp_Object reloc = Qnil;
  Charcount starting = 0, ending = 1 + MOST_POSITIVE_FIXNUM;
  Ibyte *nonreloc = NULL, *all = NULL, *allptr = all; 
  Bytecount bstart = 0, blen = 0;
  Elemcount ii = 0;

  PARSE_KEYWORDS_8 (Qwrite_sequence, nargs, args, 2, (start, end), 
                    (start = Qzero), 2, 0);

  CHECK_SEQUENCE (sequence);
  CHECK_NATNUM (start);

  if (!NILP (end))
    {
      CHECK_NATNUM (end);
    }

  stream = canonicalize_printcharfun (stream);

  if (BIGNUMP (start) || BIGNUMP (end))
    {
      /* None of the sequences can have bignum lengths. */
      check_sequence_range (sequence, start, end, Flength (sequence));

      RETURN_NOT_REACHED (sequence);
    }

  starting = XFIXNUM (start);
  if (FIXNUMP (end))
    {
      ending = XFIXNUM (end);
    }

  if (STRINGP (sequence))
    {
      Ibyte *stringp = XSTRING_DATA (sequence);
      Ibyte *strend = stringp + XSTRING_LENGTH (sequence);

      reloc = sequence;

      for (ii = 0; ii < starting && stringp < strend; ++ii)
        {
          INC_IBYTEPTR (stringp);
        }

      if (ii != starting)
        {
          /* Bad value for start. */
          check_sequence_range (sequence, start, end,
                                Flength (sequence));
          RETURN_NOT_REACHED (sequence);
        }

      bstart = stringp - XSTRING_DATA (sequence);

      for (; ii < ending && stringp < strend; ++ii)
        {
          INC_IBYTEPTR (stringp);
        }

      if (ii != ending && ending != (1 + MOST_POSITIVE_FIXNUM))
        {
          /* Bad value for end. */
          check_sequence_range (sequence, start, end,
                                Flength (sequence));
          RETURN_NOT_REACHED (sequence);
        }

      blen = stringp - (XSTRING_DATA (sequence) + bstart);
    }
  else if (CONSP (sequence))
    {
      if (NILP (end))
        {
          /* Error on circular list, with an unspecied END. */
          Lisp_Object length = Flength (sequence);
          check_sequence_range (sequence, start, end, length);
          ending = XFIXNUM (length);
        }

      /* Worst case scenario; all characters, all the longest
         possible. More likely: lots of small integers. */
      nonreloc = allptr
        = alloca_ibytes (((ending - starting)) * MAX_ICHAR_LEN);
      ii = 0;
      {
        /* EXTERNAL_LIST_LOOP because this may not be a true list. */
        EXTERNAL_LIST_LOOP_2 (elt, sequence)
          {
            if (ii >= starting)
              {
                if (ii >= ending)
                  {
                    break;
                  }

                if (!CHARP (elt))
                  {
                    check_integer_range (elt, Qzero, make_fixnum (0xff));
                  }
                allptr += set_itext_ichar (allptr,
                                           XCHAR_OR_CHAR_INT (elt));
              }
            ++ii;
          }
      }

      bstart = 0;
      blen = allptr - nonreloc;
    }
  else
    {
      Lisp_Object length = Flength (sequence);

      check_sequence_range (sequence, start, end, length);
      ending = NILP (end) ? XFIXNUM (length) : XFIXNUM (end);

      if (VECTORP (sequence))
        {
          Lisp_Object *vdata = XVECTOR_DATA (sequence);
          /* Worst case scenario; all characters, all the longest
             possible. More likely: lots of small integers. */
          nonreloc = allptr
            = alloca_ibytes (((ending - starting)) * MAX_ICHAR_LEN);

          for (ii = starting; ii < ending; ++ii)
            {
              if (!CHARP (vdata[ii]))
                {
                  check_integer_range (vdata[ii], Qzero, make_fixnum (0xff));
                }

              allptr += set_itext_ichar (allptr,
                                         XCHAR_OR_CHAR_INT (vdata[ii]));
            }
        }
      else if (BIT_VECTORP (sequence))
        {
          Lisp_Bit_Vector *vv = XBIT_VECTOR (sequence);

          nonreloc = allptr
            = alloca_ibytes (((ending - starting) * ichar_len ((Ichar)1)));
          for (ii = starting; ii < ending; ++ii)
            {
              allptr += set_itext_ichar (allptr, bit_vector_bit (vv, ii));
            }
        }
      else if (NILP (sequence))
        {
          nonreloc = allptr = alloca_ibytes (1);
        }

      bstart = 0;
      blen = allptr - nonreloc;
    }

  output_string (stream, nonreloc, reloc, bstart, blen);
  return sequence;
}

void
temp_output_buffer_setup (Lisp_Object bufname)
{
  /* This function can GC */
  struct buffer *old = current_buffer;
  Lisp_Object buf;

#ifdef I18N3
  /* #### This function should accept a Lisp_Object instead of a char *,
     so that proper translation on the buffer name can occur. */
#endif

  Fset_buffer (Fget_buffer_create (bufname));

  current_buffer->read_only = Qnil;
  Ferase_buffer (Qnil);

  buf = wrap_buffer (current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (Lisp_Object bufname,
                                     Lisp_Object (*function) (Lisp_Object arg),
                                     Lisp_Object arg,
                                     Lisp_Object same_frame)
{
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object buf = Qnil;

  GCPRO3 (buf, arg, same_frame);

  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;

  arg = (*function) (arg);

  temp_output_buffer_show (buf, same_frame);
  UNGCPRO;

  return unbind_to_1 (speccount, arg);
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

arguments: (BUFNAME &rest BODY)
*/
       (args))
{
  /* This function can GC */
  Lisp_Object name = Qnil;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  Lisp_Object val = Qnil;

#ifdef I18N3
  /* #### should set the buffer to be translating.  See print_internal(). */
#endif

  GCPRO2 (name, val);
  name = IGNORE_MULTIPLE_VALUES (Feval (XCAR (args)));

  CHECK_STRING (name);

  temp_output_buffer_setup (name);
  UNGCPRO;

  val = Fprogn (XCDR (args));

  temp_output_buffer_show (Vstandard_output, Qnil);

  return unbind_to_1 (speccount, val);
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
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (object, stream);

  stream = print_prepare (stream, &frame);
  print_internal (object, stream, 1);
  print_finish (stream, frame);

  UNGCPRO;
  return object;
}

Lisp_Object
prin1_to_string (Lisp_Object object, int noescape)
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Lstream *str = XLSTREAM (stream);
  /* gcpro OBJECT in case a caller forgot to do so */
  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (object, stream, result);

  print_internal (object, stream, !noescape);
  UNGCPRO;
  result = resizing_buffer_to_lisp_string (str);
  Lstream_delete (str);
  return result;
}

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

  RESET_PRINT_NUMBER_TABLE;
  result = prin1_to_string (object, !(EQ(noescape, Qnil)));
  RESET_PRINT_NUMBER_TABLE;

  return result;
}

DEFUN ("princ", Fprinc, 1, 2, 0, /*
Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (object, stream);
  stream = print_prepare (stream, &frame);
  print_internal (object, stream, 0);
  print_finish (stream, frame);
  UNGCPRO;
  return object;
}

DEFUN ("print", Fprint, 1, 2, 0, /*
Output the printed representation of OBJECT, with newlines around it.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see).
*/
       (object, stream))
{
  /* This function can GC */
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (object, stream);
  stream = print_prepare (stream, &frame);
  write_ascstring (stream, "\n");
  print_internal (object, stream, 1);
  write_ascstring (stream, "\n");
  print_finish (stream, frame);
  UNGCPRO;
  return object;
}

/* Print an error message for the error DATA to STREAM.  This is a
   complete implementation of `display-error', which used to be in
   Lisp (see prim/cmdloop.el).  It was ported to C so it can be used
   efficiently by Ferror_message_string.  Fdisplay_error and
   Ferror_message_string are trivial wrappers around this function.

   STREAM should be the result of canonicalize_printcharfun().  */
static void
print_error_message (Lisp_Object error_object, Lisp_Object stream)
{
  /* This function can GC */
  Lisp_Object type = Fcar_safe (error_object);
  Lisp_Object method = Qnil;
  Lisp_Object tail;

  /* No need to GCPRO anything under the assumption that ERROR_OBJECT
     is GCPRO'd.  */

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
    int speccount = specpdl_depth ();
    Lisp_Object frame = Qnil;
    struct gcpro gcpro1;
    GCPRO1 (stream);

    specbind (Qprint_message_label, Qerror);
    stream = print_prepare (stream, &frame);

    tail = Fcdr (error_object);
    if (EQ (type, Qerror))
      {
	print_internal (Fcar (tail), stream, 0);
	tail = Fcdr (tail);
      }
    else
      {
	Lisp_Object errmsg = Fget (type, Qerror_message, Qnil);
	if (NILP (errmsg))
	  print_internal (type, stream, 0);
	else
	  print_internal (LISP_GETTEXT (errmsg), stream, 0);
      }
    while (!NILP (tail))
      {
	write_ascstring (stream, first ? ": " : ", ");
	/* Most errors have an explanatory string as their first argument,
	   and it looks better not to put the quotes around it. */
	print_internal (Fcar (tail), stream,
			!(first && STRINGP (Fcar (tail))) ||
			!NILP (Fget (type, Qerror_lacks_explanatory_string,
				     Qnil)));
	tail = Fcdr (tail);
	first = 0;
      }
    print_finish (stream, frame);
    UNGCPRO;
    unbind_to (speccount);
    return;
    /* not reached */
  }

 error_throw:
  if (NILP (method))
    {
      write_ascstring (stream, GETTEXT ("Peculiar error "));
      print_internal (error_object, stream, 1);
      return;
    }
  else
    {
      call2 (method, error_object, stream);
    }
}

DEFUN ("error-message-string", Ferror_message_string, 1, 1, 0, /*
Convert ERROR-OBJECT to an error message, and return it.

The format of ERROR-OBJECT should be (ERROR-SYMBOL . DATA).  The
message is equivalent to the one that would be issued by
`display-error' with the same argument.
*/
       (error_object))
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  struct gcpro gcpro1;
  GCPRO1 (stream);

  print_error_message (error_object, stream);
  result = resizing_buffer_to_lisp_string (XLSTREAM (stream));
  Lstream_delete (XLSTREAM (stream));

  UNGCPRO;
  return result;
}

DEFUN ("display-error", Fdisplay_error, 2, 2, 0, /*
Display ERROR-OBJECT on STREAM in a user-friendly way.
*/
       (error_object, stream))
{
  /* This function can GC */
  print_error_message (error_object, canonicalize_printcharfun (stream));
  return Qnil;
}


Lisp_Object Vfloat_output_format;

/*
 * This buffer should be at least as large as the max string size of the
 * largest float, printed in the biggest notation.  This is undoubtedly
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
Bytecount
float_to_string (char *buf, double data)
{
  Ibyte *cp, c;
  int width;
  Bytecount plen;

  if (NILP (Vfloat_output_format)
      || !STRINGP (Vfloat_output_format))
  lose:
    plen = sprintf (buf, "%.16g", data);
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

      plen = sprintf (buf, (char *) XSTRING_DATA (Vfloat_output_format),
                      data);
    }

  /* added by jwz: don't allow "1.0" to print as "1"; that destroys
     the read-equivalence of lisp objects.  (* x 1) and (* x 1.0) do
     not do the same thing, so it's important that the printed
     representation of that form not be corrupted by the printer.
   */
  {
    Ibyte *s = (Ibyte *) buf; /* don't use signed chars here!
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
    plen += 2;
  }
 DONE_LABEL:

  /* Some machines print "0.4" as ".4".  I don't like that. */
  if (buf [0] == '.' || (buf [0] == '-' && buf [1] == '.'))
    {
      int i;
      for (i = plen + 1; i >= 0; i--)
	buf [i+1] = buf [i];
      buf [(buf [0] == '-' ? 1 : 0)] = '0';
      plen += 1;
    }

  return plen;
}

void
print_vector (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  Elemcount i, len = XVECTOR_LENGTH (obj), last = len;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (FIXNUMP (Vprint_length))
    {
      Elemcount max = XFIXNUM (Vprint_length);
      if (max < len) last = max;
    }

  write_ascstring (printcharfun, "[");
  for (i = 0; i < last; i++)
    {
      Lisp_Object elt = XVECTOR_DATA (obj)[i];
      if (i != 0) write_ascstring (printcharfun, " ");
      print_internal (elt, printcharfun, escapeflag);
    }
  UNGCPRO;
  if (last != len)
    write_ascstring (printcharfun, " ...");
  write_ascstring (printcharfun, "]");
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
      write_ascstring (printcharfun, "\'");
      UNGCPRO;
      print_internal (obj, printcharfun, escapeflag);
      return;
    }

  GCPRO2 (obj, printcharfun);
  write_ascstring (printcharfun, "(");

  {
    int len;
    int max = FIXNUMP (Vprint_length) ? XFIXNUM (Vprint_length) : INT_MAX;
    Lisp_Object tortoise;
    /* Use tortoise/hare to make sure circular lists don't infloop */

    for (tortoise = obj, len = 0;
	 CONSP (obj);
	 obj = XCDR (obj), len++)
      {
	if (len > 0)
          {
            write_ascstring (printcharfun, " ");

            /* Note that print_cons is the only object method that does any
               circularity checking itself, because a cons that is the cdr
               of OBJ is not handed to print_internal in the ordinary course
               of events. All the other possibly-repeated structures always
               hand sub-objects to print_internal(). */
            if (print_circle &&
                FIXNUMP (Fgethash (obj, Vprint_number_table, Qnil)))
              {
                write_ascstring (printcharfun, ". ");
                print_internal (obj, printcharfun, escapeflag);
                /* We have printed the list's tail, print_cons() is done. */
                break;
              }

            if (EQ (obj, tortoise))
              {
                if (print_readably)
		  {
		    printing_unreadable_object_fmt ("circular list");
		  }

		write_ascstring (printcharfun, "... <circular list>");
                break;
              }

            if (len & 1)
	      {
		tortoise = XCDR (tortoise);
	      }

            if (len > max)
              {
                write_ascstring (printcharfun, "...");
                break;
              }
          }

	print_internal (XCAR (obj), printcharfun, escapeflag);
      }
  }

  if (!LISTP (obj))
    {
      write_ascstring (printcharfun, " . ");
      print_internal (obj, printcharfun, escapeflag);
    }

  UNGCPRO;

  write_ascstring (printcharfun, ")");
  return;
}

void
print_string (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  Bytecount bcmax = XSTRING_LENGTH (obj), bcsize = bcmax;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (obj, printcharfun);

  if (FIXNUMP (Vprint_string_length)
      && XFIXNUM (Vprint_string_length) < bcmax)
    {
      /* The byte length of OBJ is an inclusive upper bound on its character
	 length. If PRINT-STRING-LENGTH is less than BCMAX, check OBJ's
	 character length to get an exact length to print. Otherwise, print
	 the entire string without worrying about its character length. */
      Charcount cmax = min (string_char_length (obj),
			    max (0, XREALFIXNUM (Vprint_string_length)));
      bcmax = string_index_char_to_byte (obj, cmax);
    }

  if (!escapeflag)
    {
      /* This deals with GC-relocation and Mule. */
      output_string (printcharfun, 0, obj, 0, bcmax);
      if (bcmax < XSTRING_LENGTH (obj))
	write_ascstring (printcharfun, " ...");
    }
  else
    {
      Bytecount i, last = 0;
      Charcount ci = 0;

      write_ascstring (printcharfun, "\"");
      for (i = 0; i < bcmax;
           i += itext_ichar_len (string_byte_addr (obj, i)), ci++)
	{
	  Ichar ch = itext_ichar (string_byte_addr (obj, i));
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
		  write_ascstring (printcharfun, "\\n");
		}
	      else
		{
		  /* This is correct for Mule because the character is either
		     \ or " */
		  Ibyte temp[] = { '\\', string_byte (obj, i) };
                  write_string_1 (printcharfun, temp, sizeof (temp));
		}

              /* If PRINTCHARFUN modified OBJ's byte length, attempt to ensure
                 we print the right number of characters, ensure we don't run
                 off the end.

                 PRINTCHARFUN could also have silently added characters of
                 varying byte length at different positions, maintaining the
                 byte length of OBJ but corrupting our calculation of
                 BCMAX. If it does that, our misbehaviour is its own damn
                 fault.  PRINTCHARFUN really shouldn't be modifying OBJ at
                 all, cf. the MAPPING-DESTRUCTIVE-INTERACTION Common Lisp
                 issue. */
              if (XSTRING_LENGTH (obj) != bcsize)
                {
                  bcmax = bcsize = XSTRING_LENGTH (obj);
                  i = string_index_char_to_byte (obj, ci);

                  if (FIXNUMP (Vprint_string_length)
                      && XFIXNUM (Vprint_string_length) < bcmax)
                    {
                      Charcount cmax =
                        min (string_char_length (obj),
                             max (0, XREALFIXNUM (Vprint_string_length)));
                      bcmax = string_index_char_to_byte (obj, cmax);
                    }
                }
	      last = i + 1;
	    }
	}
      if (bcmax > last)
	{
	  output_string (printcharfun, 0, obj, last,
			 bcmax - last);
	}
      if (bcmax < XSTRING_LENGTH (obj))
	write_ascstring (printcharfun, " ...");
      write_ascstring (printcharfun, "\"");
    }
  UNGCPRO;
}

DOESNT_RETURN
printing_unreadable_object_fmt (const Ascbyte *fmt, ...)
{
  Lisp_Object obj;
  va_list args;

  va_start (args, fmt);
  obj = emacs_vsprintf_string (GETTEXT (fmt), args);
  va_end (args);

  /* Fsignal GC-protects its args */
  signal_error (Qprinting_unreadable_object, 0, obj);
}

DOESNT_RETURN
printing_unreadable_lisp_object (Lisp_Object obj, const Ibyte *name)
{
  struct lrecord_header *header = (struct lrecord_header *) XPNTR (obj);
  const struct lrecord_implementation *imp =
    XRECORD_LHEADER_IMPLEMENTATION (obj);

  if (name)
    printing_unreadable_object_fmt ("#<%s %s 0x%x>", imp->name, name, header->uid);
  else
    printing_unreadable_object_fmt ("#<%s 0x%x>", imp->name, header->uid);
}

void
external_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			 int UNUSED (escapeflag))
{
  struct lrecord_header *header = (struct lrecord_header *) XPNTR (obj);
  const struct lrecord_implementation *imp =
    XRECORD_LHEADER_IMPLEMENTATION (obj);

  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);

  write_fmt_string (printcharfun, "#<%s 0x%x>", imp->name, header->uid);
}

void
internal_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			 int UNUSED (escapeflag))
{
  if (print_readably)
    printing_unreadable_object_fmt
      ("#<INTERNAL OBJECT (XEmacs bug?) (%s) 0x%x>",
       XRECORD_LHEADER_IMPLEMENTATION (obj)->name, LISP_OBJECT_UID (obj));

  /* Internal objects shouldn't normally escape to the Lisp level;
     that's why we say "XEmacs bug?".  This can happen, however, when
     printing backtraces. */
  write_fmt_string (printcharfun,
		    "#<INTERNAL OBJECT (XEmacs bug?) (%s) 0x%x>",
		    XRECORD_LHEADER_IMPLEMENTATION (obj)->name,
		    LISP_OBJECT_UID (obj));
}

enum printing_badness
{
  BADNESS_INTEGER_OBJECT,
  BADNESS_POINTER_OBJECT,
  BADNESS_POINTER_OBJECT_WITH_DATA,
  BADNESS_NO_TYPE
};

static void
printing_major_badness (Lisp_Object printcharfun,
			const Ascbyte *badness_string, int type, void *val,
			void *val2, enum printing_badness badness)
{
  Ibyte buf[666];

  switch (badness)
    {
    case BADNESS_INTEGER_OBJECT:
      qxesprintf (buf, "%s type %d object %ld", badness_string, type,
		  (EMACS_INT) val);
      break;

    case BADNESS_POINTER_OBJECT:
      qxesprintf (buf, "%s type %d object %p", badness_string, type, val);
      break;

    case BADNESS_POINTER_OBJECT_WITH_DATA:
      qxesprintf (buf, "%s type %d object %p data %p", badness_string, type,
		  val, val2);
      break;

    case BADNESS_NO_TYPE:
      qxesprintf (buf, "%s object %p", badness_string, val);
      break;
    }

  /* Don't abort or signal if called from debug_print() or already
     crashing */
  if (!inhibit_non_essential_conversion_operations)
    {
#ifdef ERROR_CHECK_TYPES
      ABORT ();
#else  /* not ERROR_CHECK_TYPES */
      if (print_readably)
	signal_ferror (Qinternal_error, "SERIOUS XEMACS BUG: printing %s; "
		       "save your buffers immediately and please report "
		       "this bug", buf);
#endif /* not ERROR_CHECK_TYPES */
    }
  write_fmt_string (printcharfun,
		    "#<SERIOUS XEMACS BUG: %s Save your buffers immediately "
		    "and please report this bug>", buf);
}

/* Not static only because of print_preprocess_cons. */
Elemcount print_preprocess_inchash_eq (Lisp_Object, Lisp_Object, Elemcount *); 

Elemcount
print_preprocess_inchash_eq (Lisp_Object obj, Lisp_Object table,
                             Elemcount *seen_object_count)
{
  htentry *hte = inchash_eq (obj, table, 1);
  Elemcount extracted;

  /* If the hash table had to be resized, hte is NULL. */
  if (hte == NULL)
    {
      hte = find_htentry (obj, XHASH_TABLE (table));
    }

  extracted = XFIXNUM (hte->value);
  if (1 == extracted)
    {
      *seen_object_count += 1;
      hte->value
        = make_fixnum (1 | (*seen_object_count << PRINT_NUMBER_ORDINAL_SHIFT));
    }
  else if ((extracted & PRINT_NUMBER_SEEN_MASK) == PRINT_NUMBER_SEEN_MASK)
    {
      /* Avoid the number overflowing the bit field. */
      extracted = (extracted & ~PRINT_NUMBER_SEEN_MASK) | 2;
      hte->value = make_fixnum (extracted);
    }

  return extracted & PRINT_NUMBER_SEEN_MASK;
}

/* Fill in Vprint_number_table according to the structure of OBJ. OBJ itself
   and all its elements will be added to Vprint_number_table recursively if
   its type has the print_preprocess method implemented. Objects with the
   print_preprocess method implemented include cons, vector, compiled
   function, hash table, char table, range table, and symbol. Symbol is an
   exceptional type in that it is impossible to construct a recursive symbol
   structure, but is here for the print-gensym feature. */

void
print_preprocess (Lisp_Object object, Lisp_Object print_number_table,
                  Elemcount *seen_object_count)
{
  if (!LRECORDP (object) || !HAS_OBJECT_METH_P (object, print_preprocess))
    {
      return;
    }

  if (SYMBOLP (object) && IN_OBARRAY (object))
    {
      /* Handle symbols specially. We do this here rather than in symbols.c
         because we don't want to have all the other print_preprocess methods 
         worry about print_preprocess_inchash_eq. */
      return;
    }

  if (print_preprocess_inchash_eq (object, print_number_table,
                                   seen_object_count) > 1)
    {
      return;
    }

  OBJECT_METH (object, print_preprocess, (object, print_number_table,
                                          seen_object_count));
}

typedef struct { Lisp_Object key; Elemcount count; } preprocess_sort_t;

static int
print_seen_once (Lisp_Object UNUSED (key), Lisp_Object value,
                 void * UNUSED (extra_arg))
{
  return 1 == ((XFIXNUM (value) & PRINT_NUMBER_SEEN_MASK));
}

static int
print_nonsymbol_seen_once (Lisp_Object key, Lisp_Object value,
                           void * UNUSED (extra_arg))
{
  /* print_continuous_numbering is used for symbols, so we don't delete them
     from the print info hash table. It's less useful for other objects at
     the moment, though. */
  return !SYMBOLP (key) && (1 == ((XFIXNUM (value) & PRINT_NUMBER_SEEN_MASK)));
}

static int
print_sort_get_numbers (Lisp_Object key, Lisp_Object value, void *extra_arg)
{
  preprocess_sort_t **preprocess_sort_ptr = (preprocess_sort_t **) extra_arg;
  preprocess_sort_t *preprocess_sort = *preprocess_sort_ptr;

  *preprocess_sort_ptr += 1;
  preprocess_sort->key = key;
  preprocess_sort->count = XFIXNUM (value);

  return 0;
}

static int
print_sort_compare_ordinals (const void *object1, const void *object2)
{
  Elemcount a = ((preprocess_sort_t *) object1)->count
    & PRINT_NUMBER_ORDINAL_MASK;
  Elemcount b = ((preprocess_sort_t *) object2)->count
    & PRINT_NUMBER_ORDINAL_MASK;

  return a - b;
}

enum print_gensym_status
  {
    PRINT_GENSYM_DONE,
    PRINT_GENSYM_PRINT,
    PRINT_GENSYM_PRINT_AND_CLEANUP_TABLE,
  };

/* Check for any circular objects or repeated uninterned symbols.

   If OBJ is a repeated structure (or symbol) and it has been printed
   already, print it now in the #%d# format, and return 1, to indicate
   print_internal is done.

   If OBJ is a repeated structure and it has not yet been printed, print
   #%d= before the object, mark it as printed, and return zero, to indicate
   print_internal should continue as usual.

   If OBJ is not a repeated structure, do nothing, and return zero, to
   indicate print_internal should continue as usual. */
static enum print_gensym_status
print_gensym_or_circle (Lisp_Object obj, Lisp_Object printcharfun)
{
  Lisp_Object seen = Fgethash (obj, Vprint_number_table, Qnil);
  if (NILP (seen))
    {
      Elemcount old_print_number_index = print_number_index;

      print_preprocess (obj, Vprint_number_table, &print_number_index);

      if (old_print_number_index != print_number_index)
        {
          Elemcount new_print_number_index, ii;

          /* We support up to 25 bits' worth of repeated objects, which is
             33 million or so, far more than we support in, say, a
             compiled-function constants vector. */
          assert (print_number_index <=
                  (PRINT_NUMBER_ORDINAL_MASK >> PRINT_NUMBER_ORDINAL_SHIFT));

          /* If any objects have been seen once and once only, remove them
             from Vprint_number_table. This is a bit of an arbitrary
             decision; we could keep them around for the sake of
             print_continuous_numbering, but there's the reasonable worry
             about Vprint_number_table getting awkwardly large. */
          elisp_map_remhash (print_continuous_numbering ? 
                             print_nonsymbol_seen_once : print_seen_once,
                             Vprint_number_table, NULL);

          new_print_number_index
            = XFIXNUM (Fhash_table_count (Vprint_number_table));

          if (new_print_number_index != print_number_index
              && new_print_number_index != old_print_number_index)
            {
              preprocess_sort_t *preprocess_sort
                = alloca_array (preprocess_sort_t, new_print_number_index);
              preprocess_sort_t *preprocess_sort_ptr = preprocess_sort;

              /* There are new objects in Vprint_number_table, but their
                 ordinal values don't necessarily represent the order they
                 were seen in, there will be gaps corresponding to the
                 non-symbols that were seen only once.  Correct this. */
              elisp_maphash_unsafe (print_sort_get_numbers, Vprint_number_table,
                                    &preprocess_sort_ptr);

              qsort (preprocess_sort, new_print_number_index,
                     sizeof (preprocess_sort_t), print_sort_compare_ordinals);

              for (ii = old_print_number_index;
                   ii < new_print_number_index;
                   ii++)
                {
                  Fputhash (preprocess_sort[ii].key, 
                            make_fixnum ((preprocess_sort[ii].count
                                       & ~PRINT_NUMBER_ORDINAL_MASK)
                                      | ((ii + 1)
                                         << PRINT_NUMBER_ORDINAL_SHIFT)),
                            Vprint_number_table);
                }
            }

          print_number_index = new_print_number_index;

          /* The new objects may include OBJ; update SEEN to reflect
             this. */
          seen = Fgethash (obj, Vprint_number_table, Qnil);
          if (FIXNUMP (seen))
            {
              goto prefix_this;
            }
        }
    }
  else
    {
    prefix_this:
      if ((XFIXNUM (seen) & PRINT_NUMBER_SEEN_MASK) == 1
          && !(print_continuous_numbering && SYMBOLP (obj)))
        {
          return PRINT_GENSYM_PRINT_AND_CLEANUP_TABLE;
        }
      else if (XFIXNUM (seen) & PRINT_NUMBER_PRINTED_MASK)
        {
          write_fmt_string (printcharfun, "#%ld#",
                            (XFIXNUM (seen) & PRINT_NUMBER_ORDINAL_MASK)
                            >> PRINT_NUMBER_ORDINAL_SHIFT);

          /* We're finished printing this object. */
          return PRINT_GENSYM_DONE;
        }
      else
        {
          write_fmt_string (printcharfun, "#%ld=",
                            (XFIXNUM (seen) & PRINT_NUMBER_ORDINAL_MASK)
                            >> PRINT_NUMBER_ORDINAL_SHIFT);

          /* We set PRINT_NUMBER_PRINTED_MASK immediately here, so the
             object itself is written as #%d# when printing its contents. */
          Fputhash (obj,
                    make_fixnum (XFIXNUM (seen) | PRINT_NUMBER_PRINTED_MASK),
                    Vprint_number_table);

          /* This is the first time the object has been seen while
             printing the recursive object; we still have to go ahead
             and do the actual print. */
        }
    }

  return PRINT_GENSYM_PRINT;
}

Lisp_Object
nsubst_structures_descend (Lisp_Object new_, Lisp_Object old,
			   Lisp_Object tree,
                           Lisp_Object number_table, Boolint test_not_unboundp)
{
  Lisp_Object seen;
  
  if (!LRECORDP (tree) || !HAS_OBJECT_METH_P (tree, nsubst_structures_descend))
    {
      return tree;
    }

  seen = Fgethash (tree, number_table, Qnil);

  if (FIXNUMP (seen))
    {
      if (XFIXNUM (seen) & PRINT_NUMBER_PRINTED_MASK)
        {
          return tree;
        }

      Fputhash (tree, make_fixnum (XFIXNUM (seen) | PRINT_NUMBER_PRINTED_MASK),
                number_table);
    }

  OBJECT_METH (tree, nsubst_structures_descend,
	       (new_, old, tree, number_table, test_not_unboundp));

  return tree;
}

/* Descend TREE, replacing the Lisp object OLD each time it is encountered
   with the Lisp object NEW_. TREE can be recursive or circular, and this is
   handled correctly. */
Lisp_Object
nsubst_structures (Lisp_Object new_, Lisp_Object old, Lisp_Object tree,
                   check_test_func_t check_test, Boolint test_not_unboundp,
                   Lisp_Object UNUSED (test), Lisp_Object UNUSED (key))
{
  Lisp_Object number_table, result;
  Elemcount ordinal = 0;
  struct gcpro gcpro1;

  if (check_test != check_eq_nokey || !LRECORDP (old))
    {
      signal_error (Qunimplemented,
                    ":descend-structures not yet finished, nsubst",
                    Qunbound);
    }

  if (!LRECORDP (tree) || !HAS_OBJECT_METH_P (tree, nsubst_structures_descend))
    {
      return tree;
    }

  number_table = make_lisp_hash_table (16, HASH_TABLE_NON_WEAK, Qeq);
  GCPRO1 (number_table);

  print_preprocess (tree, number_table, &ordinal);

  /* This function can GC by means of the hash table test functions, when
     replacing hash table entries. */
  result = nsubst_structures_descend (new_, old, tree, number_table,
                                      test_not_unboundp);
  Fclrhash (number_table);

  RETURN_UNGCPRO (result);
}

void
print_internal (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  int specdepth = 0;
  struct gcpro gcpro1, gcpro2;
  Boolint cleanup_table = 0;

  QUIT;

#ifdef NO_PRINT_DURING_GC
  /* Emacs won't print while GCing, but an external debugger might */
  if (gc_in_progress) return;
#endif

  /* Just to be safe ... */
  GCPRO2 (obj, printcharfun);

  /* WARNING WARNING WARNING!!!  Don't put anything here that might
     dereference memory.  Instead, put it down inside of
     the case Lisp_Type_Record, after the appropriate checks to make sure
     we're not dereferencing bad memory.  The idea is that, ideally,
     calling debug_print() should *NEVER* make the program crash, even when
     something very bad has happened. --ben */

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

  being_printed[print_depth] = obj;

  /* Avoid calling internal_bind_int, which conses, when called from
     debug_prin1.  In that case, we have bound print_depth to 0 anyway. */
  if (!inhibit_non_essential_conversion_operations)
    {
      specdepth = internal_bind_int (&print_depth, print_depth + 1);

      if (print_depth > PRINT_CIRCLE_LIMIT)
        {
          signal_error (Qstack_overflow,
                        "Apparently circular structure being printed",
                        Qunbound);
        }
    }

  switch (XTYPE (obj))
    {
    case Lisp_Type_Fixnum_Even:
    case Lisp_Type_Fixnum_Odd:
      {
	Ibyte buf[DECIMAL_PRINT_SIZE (Fixnum)];
        write_string_1 (printcharfun, buf,
                        fixnum_to_string (buf, sizeof (buf),
                                          XREALFIXNUM (obj), 10, Qnil));
	break;
      }

    case Lisp_Type_Char:
      {
	/* God intended that this be #\..., you know. */
	char buf[16];
	Ichar ch = XCHAR (obj);
	char *p = buf;
	*p++ = '?';
	if (ch < 32)
	  {
	    *p++ = '\\';
	    switch (ch)
	      {
	      case '\t': *p++ = 't'; break;
	      case '\n': *p++ = 'n'; break;
	      case '\r': *p++ = 'r'; break;
	      default:
		*p++ = '^';
		*p++ = ch + 64;
		if ((ch + 64) == '\\')
		  *p++ = '\\';
		break;
	      }
	  }
	else if (ch < 127)
	  {
	    /* syntactically special characters should be escaped. */
	    switch (ch)
	      {
	      case ' ':
	      case '"':
	      case '#':
	      case '\'':
	      case '(':
	      case ')':
	      case ',':
	      case '.':
	      case ';':
	      case '?':
	      case '[':
	      case '\\':
	      case ']':
	      case '`':
		*p++ = '\\';
	      }
	    *p++ = ch;
	  }
	else if (ch == 127)
	  {
	    *p++ = '\\', *p++ = '^', *p++ = '?';
	  }
	else if (ch < 160)
	  {
	    *p++ = '\\', *p++ = '^';
	    p += set_itext_ichar ((Ibyte *) p, ch + 64);
	  }
	else
	  {
	    p += set_itext_ichar ((Ibyte *) p, ch);
	  }

	output_string (printcharfun, (Ibyte *) buf, Qnil, 0, p - buf);

	break;
      }

    case Lisp_Type_Record:
      {
	struct lrecord_header *lheader = XRECORD_LHEADER (obj);

	/* Try to check for various sorts of bogus pointers or bad memory
	   if we're in a situation where it may be likely -- i.e. called
	   from debug_print() or we're already crashing.  In such cases,
	   (further) crashing is counterproductive.

	   We don't normally do these because they may be expensive or
	   weird (e.g. under Unix we typically have to set a SIGSEGV
	   handler and try to trigger a seg fault). */

	if (!lheader)
	  {
	    /* i.e. EQ Qnull_pointer */
	    printing_major_badness (printcharfun, "NULL POINTER LRECORD", 0,
				    0, 0, BADNESS_NO_TYPE);
	    break;
	  }

	/* First check to see if the lrecord header itself is garbage. */
	if (inhibit_non_essential_conversion_operations &&
	    !debug_can_access_memory (lheader, sizeof (*lheader)))
	  {
	    printing_major_badness (printcharfun,
				    "BAD MEMORY in LRECORD HEADER", 0,
				    lheader, 0, BADNESS_NO_TYPE);
	      break;
	  }

	/* Check to see if the lrecord type is garbage. */
#ifndef NEW_GC
	if (lheader->type == lrecord_type_free)
	  {
	    printing_major_badness (printcharfun, "FREED LRECORD", 0,
				    lheader, 0, BADNESS_NO_TYPE);
	    break;
	  }
	if (lheader->type == lrecord_type_undefined)
	  {
	    printing_major_badness (printcharfun, "LRECORD_TYPE_UNDEFINED", 0,
				    lheader, 0, BADNESS_NO_TYPE);
	    break;
	  }
#endif /* not NEW_GC */
	if ((int) (lheader->type) >= lrecord_type_count)
	  {
	    printing_major_badness (printcharfun, "ILLEGAL LRECORD TYPE",
				    (int) (lheader->type),
				    lheader, 0, BADNESS_POINTER_OBJECT);
	    break;
	  }

	/* Check to see if the lrecord implementation is missing or garbage. */
	{
	  const struct lrecord_implementation *imp =
	    LHEADER_IMPLEMENTATION (lheader);

	  if (!imp)
	    {
	      printing_major_badness
		(printcharfun, "NO IMPLEMENTATION FOR LRECORD TYPE",
		 (int) (lheader->type),
		 lheader, 0, BADNESS_POINTER_OBJECT);
	      break;
	    }

	  if (inhibit_non_essential_conversion_operations)
	    {
	      if (!debug_can_access_memory (imp, sizeof (*imp)))
		{
		  printing_major_badness
		    (printcharfun, "BAD MEMORY IN LRECORD IMPLEMENTATION",
		     (int) (lheader->type),
		     lheader, 0, BADNESS_POINTER_OBJECT);
		}
	    }
	}

	/* Check to see if any of the memory of the lrecord is inaccessible.
	   Note that we already checked above to see if the first part of
	   the lrecord (the header) is inaccessible, which will catch most
	   cases of a totally bad pointer.  */

	if (inhibit_non_essential_conversion_operations)
	  {
	    if (!debug_can_access_memory
		(lheader, detagged_lisp_object_size (lheader)))
	      {
		printing_major_badness (printcharfun,
					"BAD MEMORY IN LRECORD",
					(int) (lheader->type),
					lheader, 0, BADNESS_POINTER_OBJECT);
		break;
	      }

	    /* For strings, also check the data of the string itself. */
	    if (STRINGP (obj))
	      {
#ifdef NEW_GC
		if (!debug_can_access_memory (XSTRING_DATA (obj), 
					      XSTRING_LENGTH (obj)))
		  {
		    write_fmt_string
		      (printcharfun,
		       "#<EMACS BUG: %p (BAD STRING DATA %p)>",
		       lheader, XSTRING_DATA (obj));
		    break;
		  }
#else /* not NEW_GC */
		Lisp_String *l = (Lisp_String *) lheader;
		if (l->size_ && !debug_can_access_memory (l->data_, l->size_))
		  {
		    printing_major_badness (printcharfun,
		       "BAD STRING DATA", (int) (lheader->type),
					    lheader, l->data_,
					    BADNESS_POINTER_OBJECT_WITH_DATA);
		    break;
		  }
#endif /* not NEW_GC */
	      }
	  }

        if (LRECORDP (obj) &&
            ((print_circle && HAS_OBJECT_METH_P (obj, print_preprocess)) ||
             (print_gensym && SYMBOLP (obj) && !IN_OBARRAY (obj))))
          {
            enum print_gensym_status status
              = print_gensym_or_circle (obj, printcharfun);

            cleanup_table = (PRINT_GENSYM_PRINT_AND_CLEANUP_TABLE == status);

            if (PRINT_GENSYM_DONE == status)
              {
                break;
              }
          }
        else if (!print_circle &&
                 /* Could this structure be recursive? */
                 LRECORDP (obj)
                 && HAS_OBJECT_METH_P (obj, nsubst_structures_descend))
          {
	    int i;
	    for (i = 0; i < print_depth - 1; i++)
	      if (EQ (obj, being_printed[i]))
		{
		  Ibyte buf[DECIMAL_PRINT_SIZE (long) + MAX_ICHAR_LEN];

		  (void) set_itext_ichar (buf, '#');
                  write_string_1 (printcharfun, buf, 
                                  ichar_len ('#')
                                  + fixnum_to_string (buf + ichar_len ('#'),
                                                      sizeof (buf), i, 10,
                                                      Qnil));
		  break;
		}
	    if (i < print_depth - 1) /* Did we print something? */
	      break;
	  }

	if (CONSP (obj) || VECTORP (obj))
	  {
	    /* If deeper than spec'd depth, print placeholder.  */
	    if (FIXNUMP (Vprint_level)
		&& print_depth > XFIXNUM (Vprint_level))
	      {
		write_ascstring (printcharfun, "...");
		break;
	      }
	  }

	/* Either use a custom-written printer, or use
	   internal_object_printer or external_object_printer, depending on
	   whether the object is internal (not visible at Lisp level) or
	   external. */
	assert (LHEADER_IMPLEMENTATION (lheader)->printer);
	((LHEADER_IMPLEMENTATION (lheader)->printer)
	 (obj, printcharfun, escapeflag));
	break;
      }

    default:
      {
	/* We're in trouble if this happens! */
	printing_major_badness (printcharfun, "ILLEGAL LISP OBJECT TAG TYPE",
				XTYPE (obj), STORE_LISP_IN_VOID (obj), 0,
				BADNESS_INTEGER_OBJECT);
	break;
      }
    }

  if (cleanup_table)
    {
      /* If any objects have been seen once and once only, remove them from
         Vprint_number_table. This is a bit of an arbitrary decision; we
         could keep them around for the sake of print_continuous_numbering,
         but there's the reasonable worry about Vprint_number_table getting
         awkwardly large. */
      elisp_map_remhash (print_continuous_numbering ? 
                         print_nonsymbol_seen_once : print_seen_once,
                         Vprint_number_table, NULL);

    }

  if (!inhibit_non_essential_conversion_operations)
    unbind_to (specdepth);
  UNGCPRO;
}

void
print_float (Lisp_Object obj, Lisp_Object printcharfun,
	     int UNUSED (escapeflag))
{
  Ascbyte pigbuf[350];	/* see comments in float_to_string */

  write_string_1 (printcharfun, (const Ibyte *) pigbuf,
                  float_to_string (pigbuf, XFLOAT_DATA (obj)));
}

void
print_symbol (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  /* This function can GC */
  Lisp_Object name = symbol_name (XSYMBOL (obj));
  Bytecount size = XSTRING_LENGTH (name);
  struct gcpro gcpro1, gcpro2;

  if (!escapeflag)
    {
      /* This deals with GC-relocation */
      output_string (printcharfun, 0, name, 0, size);
      return;
    }

  if (0 == size)
    {
      /* Compatible with GNU, but not with Common Lisp, where the syntax
         for this symbol is ||. */
      write_ascstring (printcharfun,
                       (print_gensym && !IN_OBARRAY (obj)) ? "#:" : "##");
      return;
    }

  GCPRO2 (obj, printcharfun);

  if (print_gensym && !IN_OBARRAY (obj))
    {
      write_ascstring (printcharfun, "#:");
    }

  /* Does it look like a rational or a float? */
  {
    Ibyte *data = XSTRING_DATA (name), *pend = data + XSTRING_LENGTH (name);
    Fixnum nondigits = 0, fixval = -1;
    Boolint confusing = 1;
    Ichar cc = itext_ichar (data);
    Lisp_Object got = Qnil;
    
    if (cc == '-' || cc == '+')
      {
        INC_IBYTEPTR (data);
        if (data == pend)
          {
            confusing = 0;
          }
      }

    /* No need to check for '.' when working out whether the symbol looks like
       a number, '.' will get a backslash on printing no matter what,
       disqualifying it from being a number when read. */
    while (confusing && data < pend)
      {
        cc = itext_ichar (data);

        switch (cc)
          {
            /* A symbol like 2e10 could be confused with a float: */
          case 'e':
          case 'E':
            /* And one like 123/456 could be confused with a ratio: */
          case '/':
            nondigits++;
            confusing = nondigits < 2
              /* If it starts with an E or a slash, that's fine, it can't be a
                 number. */
              && data != XSTRING_DATA (name)
              /* And if it ends with an e or a slash that's fine too. */
              && (data + itext_ichar_len (data)) != pend;
            break;

            /* There can be a sign in the exponent. Such a sign needs to be
               directly after an e and to have trailing digits after it to be
               valid float syntax. Common Lisp does not allow signs in the
               denominator in its ratio syntax, so this cannot be a ratio. */
          case '+':
          case '-':
            confusing = (1 == nondigits)
              && data != XSTRING_DATA (name)
              && (data + itext_ichar_len (data)) != pend;
            if (confusing)
              {
                Ibyte *lastp = data;
                Ichar clast;

                DEC_IBYTEPTR (lastp);
                clast = itext_ichar (lastp);

                confusing = clast == 'E' || clast == 'e';
              }
            break;

            /* A symbol that is all decimal digits could be confused with an
               integer: */
          default:
            got = get_char_table (cc, Vdigit_fixnum_map);
            fixval = FIXNUMP (got) ? XREALFIXNUM (got) : -1;
            if (fixval < 0 || fixval > 9)
              {
                confusing = 0;
              }
            break;
          }

        INC_IBYTEPTR (data);
      }

    if (confusing)
      write_ascstring (printcharfun, "\\");
  }

  {
    Bytecount i, last = 0;
    Charcount ci = 0;

    for (i = 0; i < size;
         i += itext_ichar_len (string_byte_addr (name, i)), ci++)
      {
	switch (itext_ichar (string_byte_addr (name, i)))
	  {
	  case  0: case  1: case  2: case  3:
	  case  4: case  5: case  6: case  7:
	  case  8: case  9: case 10: case 11:
	  case 12: case 13: case 14: case 15:
	  case 16: case 17: case 18: case 19:
	  case 20: case 21: case 22: case 23:
	  case 24: case 25: case 26: case 27:
	  case 28: case 29: case 30: case 31:
	  case ' ': case '\"': case '\\': case '\'':
	  case ';': case '#' : case '(' : case ')':
	  case ',': case '.' : case '`' :
	  case '[': case ']' : case '?' :
	    if (i > last)
              output_string (printcharfun, 0, name, last, i - last);

	    write_ascstring (printcharfun, "\\");

            /* If PRINTCHARFUN modified NAME's byte length, ensure we don't
               run off the end, attempt to ensure we print the right number of
               characters.  Cf. similar issues with print_string, above. */
            if (XSTRING_LENGTH (name) != size)
              {
                size = XSTRING_LENGTH (name);
                i = string_index_char_to_byte (name, ci);
              }
	    last = i;
	  }
      }
    output_string (printcharfun, 0, name, last, size - last);
  }
  UNGCPRO;
}


/*************************************************************************/
/*                    debug-printing: implementation                     */
/*************************************************************************/

/* Useful on systems or in places where writing to stdout is unavailable or
   not working. */

static int alternate_do_pointer;
static int alternate_do_size;
static char *alternate_do_string;

DEFUN ("alternate-debugging-output", Falternate_debugging_output, 1, 1, 0, /*
Append CHARACTER to the array `alternate_do_string'.
This can be used in place of `external-debugging-output' as a function
to be passed to `print'.  Before calling `print', set `alternate_do_pointer'
to 0.
*/
       (character))
{
  Ibyte str[MAX_ICHAR_LEN];
  Bytecount len;

  CHECK_CHAR_COERCE_INT (character);
  len = set_itext_ichar (str, XCHAR (character));
  write_string_to_alternate_debugging_output (str, len);
  
  return character;
}

static Bytecount
write_string_to_alternate_debugging_output (const Ibyte *str, Bytecount len)
{
  int extlen;
  const Extbyte *extptr;
#if 0 /* We want to see the internal representation, don't we? */
  if (initialized && !inhibit_non_essential_conversion_operations)
    TO_EXTERNAL_FORMAT (DATA, (str, len),
			ALLOCA, (extptr, extlen),
			Qterminal);
  else
#endif /* 0 */
    {
      extlen = len;
      extptr = (Extbyte *) str;
    }

  /* If not yet initialized, just skip it. */
  if (alternate_do_string == NULL)
    return 0;

  if (alternate_do_pointer + extlen >= alternate_do_size)
    {
      alternate_do_size =
	max (alternate_do_size * 2, alternate_do_pointer + extlen + 1);
      XREALLOC_ARRAY (alternate_do_string, CIbyte, alternate_do_size);
    }
  memcpy (alternate_do_string + alternate_do_pointer, extptr, extlen);
  alternate_do_pointer += extlen;
  alternate_do_string[alternate_do_pointer] = 0;
  return extlen;
}


DEFUN ("set-device-clear-left-side", Fset_device_clear_left_side, 2, 2, 0, /*
Set whether to output a newline before the next output to a stream device.
This will happen only if the most recently-outputted character was not
a newline -- i.e. it will make sure the left side is "clear" of text.
*/
       (device, value))
{
  if (!NILP (device))
    CHECK_LIVE_DEVICE (device);
  if (NILP (device) || DEVICE_STREAM_P (XDEVICE (device)))
    /* #### This should be per-device */
    stdout_clear_before_next_output = !NILP (value);
  return Qnil;
}

DEFUN ("device-left-side-clear-p", Fdevice_left_side_clear_p, 0, 1, 0, /*
For stream devices, true if the most recent-outputted character was a newline.
*/
       (device))
{
  if (!NILP (device))
    CHECK_LIVE_DEVICE (device);
  if (NILP (device) || DEVICE_STREAM_P (XDEVICE (device)))
    /* #### This should be per-device */
    return stdout_needs_newline ? Qt : Qnil;
  return Qnil;
}

DEFUN ("external-debugging-output", Fexternal_debugging_output, 1, 3, 0, /*
Write CHAR-OR-STRING to stderr or stdout.
If optional arg STDOUT-P is non-nil, write to stdout; otherwise, write
to stderr.  You can use this function to write directly to the terminal.
This function can be used as the STREAM argument of Fprint() or the like.

Under MS Windows, this writes output to the console window (which is
created, if necessary), unless XEmacs is being run noninteractively
\(i.e. using the `-batch' argument).

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
	wtaerror ("Must be tty or stream device", device);
      con = XCONSOLE (DEVICE_CONSOLE (XDEVICE (device)));
      if (DEVICE_TTY_P (XDEVICE (device)))
	file = 0;
      else if (!NILP (stdout_p))
	file = CONSOLE_STREAM_DATA (con)->out;
      else
	file = CONSOLE_STREAM_DATA (con)->err;
    }

  if (STRINGP (char_or_string))
    write_string_to_stdio_stream (file, con,
				  XSTRING_DATA (char_or_string),
				  XSTRING_LENGTH (char_or_string),
				  print_unbuffered);
  else
    {
      Ibyte str[MAX_ICHAR_LEN];
      Bytecount len;

      CHECK_CHAR_COERCE_INT (char_or_string);
      len = set_itext_ichar (str, XCHAR (char_or_string));
      write_string_to_stdio_stream (file, con, str, len, print_unbuffered);
    }

  return char_or_string;
}

DEFUN ("open-termscript", Fopen_termscript, 1, 1, "FOpen termscript file: ", /*
Start writing all terminal output to FILENAME as well as the terminal.
FILENAME = nil means just close any termscript file currently open.
*/
       (filename))
{
  /* This function can GC */
  if (termscript != 0)
    {
      retry_fclose (termscript);
      termscript = 0;
    }

  if (! NILP (filename))
    {
      filename = Fexpand_file_name (filename, Qnil);
      termscript = qxe_fopen (XSTRING_DATA (filename), "w");
      if (termscript == NULL)
	report_file_error ("Opening termscript", filename);
    }
  return Qnil;
}

static Lisp_Object
restore_inhibit_non_essential_conversion_operations (Lisp_Object obj)
{
  inhibit_non_essential_conversion_operations = XFIXNUM (obj);
  return Qnil;
}

/* Bind the value of inhibit_non_essential_conversion_operations to 1
   in a way that involves no consing. */
static int
begin_inhibit_non_essential_conversion_operations (void)
{
  int depth =
    record_unwind_protect
    (restore_inhibit_non_essential_conversion_operations,
     make_fixnum (inhibit_non_essential_conversion_operations));
  inhibit_non_essential_conversion_operations = 1;
  return depth;
}

static int debug_print_length   = 50;
static int debug_print_level    = 15;
static int debug_print_readably = -1;

/* Restore values temporarily bound by debug_prin1.  We use this approach to
   avoid consing in debug_prin1.  That is verboten, since debug_print can be
   called by cons debugging code. */
static Lisp_Object
debug_print_exit (Lisp_Object val)
{
  struct debug_bindings *bindings =
    (struct debug_bindings *) GET_VOID_FROM_LISP (val);
  inhibit_non_essential_conversion_operations =
    bindings->inhibit_non_essential_conversion_operations;
  print_depth = bindings->print_depth;
  print_readably = bindings->print_readably;
  print_unbuffered = bindings->print_unbuffered;
  print_circle = bindings->print_circle;
  in_debug_print = bindings->in_debug_print;
  gc_currently_forbidden = bindings->gc_currently_forbidden;
  Vprint_length = bindings->Vprint_length;
  Vprint_level = bindings->Vprint_level;
  Vinhibit_quit = bindings->Vinhibit_quit;
  return Qnil;
}

/* Save values and bind them to new values suitable for debug output.  We
   try very hard to avoid any Lisp allocation (i.e. consing) during the
   operation of debug printing, since we might be calling it from inside GC
   or other sensitive places.  This means we have to be a bit careful with
   record_unwind_protect to not create any temporary Lisp objects. */

static int
debug_print_enter (struct debug_bindings *bindings)
{
  /* by doing this, we trick various things that are non-essential
     but might cause crashes into not getting executed. */
  int specdepth;

  bindings->inhibit_non_essential_conversion_operations =
    inhibit_non_essential_conversion_operations;
  bindings->print_depth = print_depth;
  bindings->print_readably = print_readably;
  bindings->print_unbuffered = print_unbuffered;
  bindings->print_circle = print_circle;
  bindings->in_debug_print = in_debug_print;
  bindings->gc_currently_forbidden = gc_currently_forbidden;
  bindings->Vprint_length = Vprint_length;
  bindings->Vprint_level = Vprint_level;
  bindings->Vinhibit_quit = Vinhibit_quit;
  specdepth = record_unwind_protect (debug_print_exit,
				     STORE_VOID_IN_LISP (bindings));

  inhibit_non_essential_conversion_operations = 1;
  print_depth = 0;
  print_readably = debug_print_readably != -1 ? debug_print_readably : 0;
  print_unbuffered++;
  print_circle = 1;
  in_debug_print = 1;
  gc_currently_forbidden = 1;
  if (debug_print_length > 0)
    Vprint_length = make_fixnum (debug_print_length);
  if (debug_print_level > 0)
    Vprint_level = make_fixnum (debug_print_level);
  Vinhibit_quit = Qt;

  return specdepth;
}

/* Print an object, `prin1'-style, to various possible debugging outputs.
   Make sure it's completely unbuffered so that, in the event of a crash
   somewhere, we see as much as possible that happened before it.
   */
static void
debug_prin1 (Lisp_Object debug_print_obj, int flags)
{
  /* This function cannot GC, since GC is forbidden */
  struct debug_bindings bindings;
  int specdepth = debug_print_enter (&bindings);

  if ((flags & EXT_PRINT_STDOUT) || (flags & EXT_PRINT_STDERR))
    print_internal (debug_print_obj, Qexternal_debugging_output, 1);
  if (flags & EXT_PRINT_ALTERNATE)
    print_internal (debug_print_obj, Qalternate_debugging_output, 1);
#ifdef WIN32_NATIVE
  if (flags & EXT_PRINT_MSWINDOWS)
    {
      /* Write out to the debugger, as well */
      print_internal (debug_print_obj, Qmswindows_debugging_output, 1);
    }
#endif

  unbind_to (specdepth);
}

void
debug_p4 (Lisp_Object obj)
{
  if (STRINGP (obj))
    debug_out ("\"%s\"", XSTRING_DATA (obj));
  else if (CONSP (obj))
    {
      int first = 1;
      do {
	debug_out (first ? "(" : " ");
	first = 0;
	debug_p4 (XCAR (obj));
	obj = XCDR (obj);
      } while (CONSP (obj));
      if (NILP (obj))
	debug_out (")");
      else
	{
	  debug_out (" . ");
	  debug_p4 (obj);
	  debug_out (")");
	}
    }
  else if (VECTORP (obj))
    {
      Elemcount size = XVECTOR_LENGTH (obj), i, first = 1;

      for (i = 0; i < size; i++)
	{
	  debug_out (first ? "[" : " ");
	  first = 0;
	  debug_p4 (XVECTOR_DATA (obj)[i]);
	  debug_out ("]");
	}
    }
  else if (SYMBOLP (obj))
    {
      Lisp_Object name = XSYMBOL_NAME (obj);
      if (!STRINGP (name))
	debug_out ("<<bad symbol>>");
      else
	debug_out ("%s", XSTRING_DATA (name));
    }
  else if (FIXNUMP (obj))
    {
      debug_out ("%ld", XFIXNUM (obj));
    }
  else if (FLOATP (obj))
    {
      debug_out ("%g", XFLOAT_DATA (obj));
    }
  else
    {
      struct lrecord_header *header =
	(struct lrecord_header *) XPNTR (obj);

      if (header->type >= lrecord_type_last_built_in_type)
	debug_out ("<< bad object type=%d 0x%lx>>", header->type,
		   (EMACS_INT) header);
      else
	debug_out ("#<%s addr=0x%lx uid=0x%lx>",
		   LHEADER_IMPLEMENTATION (header)->name,
		   (EMACS_INT) header,
		   (EMACS_INT) ((struct lrecord_header *) header)->uid);
    }
}

static int
ext_print_begin (int dest)
{
  int depth = begin_inhibit_non_essential_conversion_operations ();
  if (dest & EXT_PRINT_ALTERNATE)
    alternate_do_pointer = 0;
  if (dest & (EXT_PRINT_STDERR | EXT_PRINT_STDOUT))
    stdout_clear_before_next_output = 1;
  return depth;
}

static void
ext_print_end (int dest, int depth)
{
  if (dest & (EXT_PRINT_MSWINDOWS | EXT_PRINT_STDERR | EXT_PRINT_STDOUT))
    external_out (dest & (EXT_PRINT_MSWINDOWS | EXT_PRINT_STDERR |
			  EXT_PRINT_STDOUT), "\n");
  unbind_to (depth);
}

static void
external_debug_print (Lisp_Object object, int dest)
{
  int depth = ext_print_begin (dest);
  debug_prin1 (object, dest);
  ext_print_end (dest, depth);
}


/*************************************************************************/
/*                 debug-printing: external entry points                 */
/*************************************************************************/

/* All of the following functions output simultaneously to the following
   destinations:

   (1) stderr
   (2) alternate_do_string -- a string containing debug output, for situations
       where stderr may be unavailable (e.g. on MS Windows)
   (3) on MS Windows, the "debugging output" (output using OutputDebugString,
       which shows up in a debugger)

   Furthermore, they inhibit DFC-style conversion, so they will work during
   initialization or death, or when called from within the DFC conversion
   routines. */

/* Printf-style debugging output. */

Bytecount
debug_out (const CIbyte *fmt, ...)
{
  int depth =  begin_inhibit_non_essential_conversion_operations ();
  Bytecount result;
  va_list args;
  va_start (args, fmt);
  result = write_string_to_external_output_va (fmt, args, EXT_PRINT_ALL);
  va_end (args);
  unbind_to (depth);
  return result;
}

/* Basic entry point: Print out a Lisp object to the debugging output. */

void
debug_print (Lisp_Object debug_print_obj)
{
  return external_debug_print (debug_print_obj, EXT_PRINT_ALL);
}

/* Printf-style output when the objects being printed are Lisp objects.
   Calling style is e.g.

   debug_out_lisp ("Called foo(%s %s)\n", arg0, arg1)
*/

Bytecount
debug_out_lisp (const CIbyte *format, ...)
{
  /* This function cannot GC, since GC is forbidden */
  struct debug_bindings bindings;
  int specdepth = debug_print_enter (&bindings);
  Bytecount len, result;
  va_list va;
  Ibyte *msgout;

  va_start (va, format);
  len = emacs_vasprintf_lisp (&msgout, format, va);
  va_end (va);

  result = write_string_to_external_output (msgout, len, EXT_PRINT_ALL);
  xfree (msgout);
  unbind_to (specdepth);

  return result;
}

/* Getting tired of typing debug_print() ... */
void dp (Lisp_Object debug_print_obj);
void
dp (Lisp_Object debug_print_obj)
{
  debug_print (debug_print_obj);
}

/* Alternate debug printer: Return a char * pointer to the output */
char *dpa (Lisp_Object debug_print_obj);
char *
dpa (Lisp_Object debug_print_obj)
{
  external_debug_print (debug_print_obj, EXT_PRINT_ALTERNATE);
  
  return alternate_do_string;
}

/* Do a backtrace to stderr. */
void
debug_backtrace (void)
{
  /* This function cannot GC, since GC is forbidden */
  struct debug_bindings bindings;
  int specdepth = debug_print_enter (&bindings);

  Fbacktrace (Qexternal_debugging_output, Qt);
  stderr_out ("\n");

  unbind_to (specdepth);
}

/* Getting tired of typing debug_backtrace() ... */
void db (void);
void
db (void)
{
  debug_backtrace ();
}

/* Do a "short" backtrace. */

void
debug_short_backtrace (int length)
{
  int first = 1;
  struct backtrace *bt = backtrace_list;

  debug_out ("   [");
  while (length > 0 && bt)
    {
      if (!first)
	{
	  debug_out (", ");
	}
      if (COMPILED_FUNCTIONP (*bt->function))
	{
#if defined (COMPILED_FUNCTION_ANNOTATION_HACK)
	  Lisp_Object ann =
	    compiled_function_annotation (XCOMPILED_FUNCTION (*bt->function));
#else
	  Lisp_Object ann = Qnil;
#endif
	  if (!NILP (ann))
	    {
	      debug_out ("<compiled-function from ");
	      debug_prin1 (ann, EXT_PRINT_ALL);
	      debug_out (">");
	    }
	  else
	    {
	      debug_out ("<compiled-function of unknown origin>");
	    }
	}
      else
	debug_prin1 (*bt->function, EXT_PRINT_ALL);
      first = 0;
      length--;
      bt = bt->next;
    }
  debug_out ("]\n");
}


void
syms_of_print (void)
{
  DEFSYMBOL (Qstandard_output);

  DEFSYMBOL (Qprint_length);

  DEFSYMBOL (Qprint_string_length);

  DEFSYMBOL (Qdisplay_error);
  DEFSYMBOL (Qprint_message_label);
  DEFSYMBOL (Qwrite_sequence);

  DEFSUBR (Fprin1);
  DEFSUBR (Fprin1_to_string);
  DEFSUBR (Fprinc);
  DEFSUBR (Fprint);
  DEFSUBR (Ferror_message_string);
  DEFSUBR (Fdisplay_error);
  DEFSUBR (Fwrite_char);
  DEFSUBR (Fwrite_sequence);
  DEFSUBR (Falternate_debugging_output);
  DEFSUBR (Fset_device_clear_left_side);
  DEFSUBR (Fdevice_left_side_clear_p);
  DEFSUBR (Fexternal_debugging_output);
  DEFSUBR (Fopen_termscript);
  DEFSYMBOL (Qexternal_debugging_output);
  DEFSYMBOL (Qalternate_debugging_output);
#ifdef HAVE_MS_WINDOWS
  DEFSYMBOL (Qmswindows_debugging_output);
#endif
  DEFSUBR (Fwith_output_to_temp_buffer);
}

void
reinit_vars_of_print (void)
{
  alternate_do_pointer = 0;
}

void
vars_of_print (void)
{
  DEFVAR_LISP ("standard-output", &Vstandard_output /*
Output stream `print' uses by default for outputting a character.
This may be any function of one argument.
It may also be a buffer (output is inserted before point)
or a marker (output is inserted and the marker is advanced)
or the symbol t (output appears in the minibuffer line).
*/ );
  Vstandard_output = Qt;

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

  DEFVAR_LISP ("print-table-nonreadably-length",
	       &Vprint_table_nonreadably_length /*
Maximum length of table objects to print before abbreviating.
This applies only when printing non-readably (i.e. `print-readably' is nil).
A value of nil means no limit.
*/ );
  Vprint_table_nonreadably_length = make_fixnum (20);

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

  DEFVAR_BOOL ("print-gensym", &print_gensym /*
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

If the value of `print-continuous-numbering' is non-nil, the table used by
`print-gensym' and `print-circle' (which see) will not be reset on entry to
and exit from printing functions, so that the use of #...# and #...= can
carry over for several separately printed objects.
*/ );
  print_gensym = 1;

  DEFVAR_BOOL ("print-circle", &print_circle /*
Non-nil means print recursive structures using #N= and #N# syntax.

If nil, XEmacs detects recursive structures and truncates them in an
unreadable fashion.

If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.

If the value of `print-continuous-numbering' is non-nil, the table used by
`print-gensym' (which see) and `print-circle' will not be reset on entry to
and exit from printing functions, so that the use of #...# and #...= can
carry over for several separately printed objects.
*/);
  print_circle = 0;

  DEFVAR_BOOL_MAGIC ("print-continuous-numbering",
                     &print_continuous_numbering /*
Non-nil means number continuously across print calls, mostly for symbols.
This affects the numbers printed for #N= labels and #M# references.
See also `print-circle' and `print-gensym'.
This variable should not be set with `setq'; bind it with a `let' instead.
*/ ,
                     print_continuous_numbering_changed);
  print_continuous_numbering = 0;

  staticpro (&Vprint_number_table);
  Vprint_number_table = make_lisp_hash_table (16, HASH_TABLE_KEY_WEAK, Qeq);

  DEFVAR_LISP ("print-message-label", &Vprint_message_label /*
Label for minibuffer messages created with `print'.  This should
generally be bound with `let' rather than set.  (See `display-message'.)
*/ );
  Vprint_message_label = Qprint;

  /* The exact size doesn't matter since we realloc when necessary.
     Use CIbyte instead of Ibyte so that debuggers show the associated
     string automatically. */
  alternate_do_size = 5000;
  alternate_do_string = xnew_array (CIbyte, 5000);
}
