/* Record indices of function doc strings stored in a file.
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 2001, 2002, 2004 Ben Wing.

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

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "file-coding.h"
#include "insdel.h"
#include "keymap.h"
#include "lstream.h"
#include "sysfile.h"

Lisp_Object Vinternal_doc_file_name;

Lisp_Object QSsubstitute;

/* Work out what source file a function or variable came from, taking the
   information from the documentation file. */

static Lisp_Object
extract_object_file_name (int fd, EMACS_INT doc_pos,
			  Ibyte *name_nonreloc, Lisp_Object name_reloc,
			  int standard_doc_file)
{
  Ibyte buf[DOC_MAX_FILENAME_LENGTH];
  Ibyte *buffer = buf;
  int buffer_size = sizeof (buf), space_left;
  Ibyte *from, *to;
  REGISTER Ibyte *p = buffer;
  Lisp_Object return_me;
  Lisp_Object fdstream = Qnil, instream = Qnil;
  struct gcpro gcpro1, gcpro2;
  EMACS_INT position, seenS = 0;

  GCPRO2 (fdstream, instream);

  position = doc_pos > DOC_MAX_FILENAME_LENGTH  ? 
    doc_pos - DOC_MAX_FILENAME_LENGTH : 0; 

  if (0 > lseek (fd, position, 0))
    {
      if (name_nonreloc)
	name_reloc = build_intstring (name_nonreloc);
      return_me = list3 (build_msg_string
			 ("Position out of range in doc string file"),
			  name_reloc, make_int (position));
      goto done;
    }

  fdstream = make_filedesc_input_stream (fd, 0, -1, 0);
  Lstream_set_buffering (XLSTREAM (fdstream), LSTREAM_UNBUFFERED, 0);
  instream =
    make_coding_input_stream
      (XLSTREAM (fdstream), standard_doc_file ? Qescape_quoted : Qbinary,
       CODING_DECODE, 0);
  Lstream_set_buffering (XLSTREAM (instream), LSTREAM_UNBUFFERED, 0);

  space_left = buffer_size - (p - buffer);
  while (space_left > 0)
    {
      int nread;

      nread = Lstream_read (XLSTREAM (instream), p, space_left);
      if (nread < 0)
	{
	  return_me = list1 (build_msg_string
			     ("Read error on documentation file"));
	  goto done;
	}

      p[nread] = 0;

      if (!nread)
	break;

      p += nread;
      space_left = buffer_size - (p - buffer);
    }

  /* First, search backward for the "\037S" that marks the beginning of the
     file name, then search forward from that to the newline or to the end
     of the buffer. */
  from = p; 

  while (from > buf)
    {
      --from;
      if (seenS)
	{
	  if ('\037' == *from) 
	    {
	      /* Got a file name; adjust `from' to point to it, break out of
		 the loop.  */
	      from += 2;
	      break; 
	    }
	}
      /* Is *from 'S' ? */
      seenS = ('S' == *from);
    }

  if (buf == from)
    {
      /* We've scanned back to the beginning of the buffer without hitting
	 the file name. Either the file name plus the symbol name is longer
	 than DOC_MAX_FILENAME_LENGTH--which shouldn't happen, because it'll
	 trigger an assertion failure in make-docfile, the DOC file is
	 corrupt, or it was produced by a version of make-docfile that
	 doesn't store the file name with the symbol name and docstring.  */ 
      return_me = list1 (build_msg_string
			 ("Object file name not stored in doc file"));
      goto done;
    }

  to = from;
  /* Search for the end of the file name. */
  while (++to < p)
    {
      if ('\n' == *to || '\037' == *to)
	{
	  break;
	}
    }

  /* Don't require the file name to end in a newline. */
  return_me = make_string (from, to - from);

 done:
  if (!NILP (instream))
    {
      Lstream_delete (XLSTREAM (instream));
      Lstream_delete (XLSTREAM (fdstream));
    }

  UNGCPRO;
  return return_me;
}

Lisp_Object
unparesseuxify_doc_string (int fd, EMACS_INT position,
                           Ibyte *name_nonreloc, Lisp_Object name_reloc,
			   int standard_doc_file)
{
  Ibyte buf[512 * 32 + 1];
  Ibyte *buffer = buf;
  int buffer_size = sizeof (buf);
  Ibyte *from, *to;
  REGISTER Ibyte *p = buffer;
  Lisp_Object return_me;
  Lisp_Object fdstream = Qnil, instream = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (fdstream, instream);

  if (0 > lseek (fd, position, 0))
    {
      if (name_nonreloc)
	name_reloc = build_intstring (name_nonreloc);
      return_me = list3 (build_msg_string
			 ("Position out of range in doc string file"),
			  name_reloc, make_int (position));
      goto done;
    }

  fdstream = make_filedesc_input_stream (fd, 0, -1, 0);
  Lstream_set_buffering (XLSTREAM (fdstream), LSTREAM_UNBUFFERED, 0);
  instream =
    make_coding_input_stream
      /* Major trouble if we are too clever when reading byte-code
	 instructions!

	 #### We should have a way of handling escape-quoted elc files
	 (i.e. files with non-ASCII/Latin-1 chars in them).  Currently this
	 is "solved" in bytecomp.el by never inserting lazy references in
	 such files. */
      (XLSTREAM (fdstream), standard_doc_file ? Qescape_quoted : Qbinary,
       CODING_DECODE, 0);
  Lstream_set_buffering (XLSTREAM (instream), LSTREAM_UNBUFFERED, 0);
  
  /* Read the doc string into a buffer.
     Use the fixed buffer BUF if it is big enough; otherwise allocate one.
     We store the buffer in use in BUFFER and its size in BUFFER_SIZE.  */

  while (1)
    {
      int space_left = buffer_size - (p - buffer);
      int nread;

      /* Switch to a bigger buffer if we need one.  */
      if (space_left == 0)
	{
          Ibyte *old_buffer = buffer;
	  if (buffer == buf)
	    {
	      buffer = xnew_ibytes (buffer_size *= 2);
	      memcpy (buffer, old_buffer, p - old_buffer);
	    }
	  else
            XREALLOC_ARRAY (buffer, Ibyte, buffer_size *= 2);
          p += buffer - old_buffer;
	  space_left = buffer_size - (p - buffer);
	}

      /* Don't read too much at one go.  */
      if (space_left > 1024 * 8)
	space_left = 1024 * 8;
      nread = Lstream_read (XLSTREAM (instream), p, space_left);
      if (nread < 0)
	{
	  return_me = list1 (build_msg_string
			     ("Read error on documentation file"));
	  goto done;
	}
      p[nread] = 0;
      if (!nread)
	break;
      {
	Ibyte *p1 = qxestrchr (p, '\037'); /* End of doc string marker */
	if (p1)
	  {
	    *p1 = 0;
	    p = p1;
	    break;
	  }
      }
      p += nread;
    }

  /* Scan the text and remove quoting with ^A (char code 1).
     ^A^A becomes ^A, ^A0 becomes a null char, and ^A_ becomes a ^_.  */
  from = to = buffer;
  while (from < p)
    {
      if (*from != 1 /*^A*/)
	*to++ = *from++;
      else
	{
	  int c = *(++from);

	  from++;
          switch (c)
            {
            case 1:   *to++ =  c;     break;
            case '0': *to++ = '\0';   break;
            case '_': *to++ = '\037'; break;
            default:
              return_me = list2 (build_msg_string
	("Invalid data in documentation file -- ^A followed by weird code"),
                                 make_int (c));
              goto done;
            }
	}
    }

  return_me = make_string (buffer, to - buffer);

 done:
  if (!NILP (instream))
    {
      Lstream_delete (XLSTREAM (instream));
      Lstream_delete (XLSTREAM (fdstream));
    }
  UNGCPRO;
  if (buffer != buf) /* We must have allocated buffer above */
    xfree (buffer, Ibyte *);
  return return_me;
}

#define string_join(dest, s1, s2)					\
  memcpy (dest, XSTRING_DATA (s1), XSTRING_LENGTH (s1));		\
  memcpy (dest + XSTRING_LENGTH (s1), XSTRING_DATA (s2),		\
          XSTRING_LENGTH (s2));						\
          dest[XSTRING_LENGTH (s1) + XSTRING_LENGTH (s2)] = '\0'

/* Extract a doc string from a file.  FILEPOS says where to get it.
   (This could actually be byte code instructions/constants instead
   of a doc string.)
   If it is an integer, use that position in the standard DOC file.
   If it is (FILE . INTEGER), use FILE as the file name
   and INTEGER as the position in that file.
   But if INTEGER is negative, make it positive.
   (A negative integer is used for user variables, so we can distinguish
   them without actually fetching the doc string.)  */

static Lisp_Object
get_doc_string (Lisp_Object filepos)
{
  REGISTER int fd;
  REGISTER Ibyte *name_nonreloc = 0;
  EMACS_INT position;
  Lisp_Object file, tem;
  Lisp_Object name_reloc = Qnil;
  int standard_doc_file = 0;

  if (INTP (filepos))
    {
      file = Vinternal_doc_file_name;
      standard_doc_file = 1;
      position = XINT (filepos);
    }
  else if (CONSP (filepos) && INTP (XCDR (filepos)))
    {
      file = XCAR (filepos);
      position = XINT (XCDR (filepos));
      if (position < 0)
	position = - position;
    }
  else
    return Qnil;

  if (!STRINGP (file))
    return Qnil;

  /* Put the file name in NAME as a C string.
     If it is relative, combine it with Vdoc_directory.  */

  tem = Ffile_name_absolute_p (file);
  if (NILP (tem))
    {
      Bytecount minsize;
      /* XEmacs: Move this check here.  OK if called during loadup to
	 load byte code instructions. */
      if (!STRINGP (Vdoc_directory))
	return Qnil;

      minsize = XSTRING_LENGTH (Vdoc_directory);
      /* sizeof ("../lib-src/") == 12 */
      if (minsize < 12)
	minsize = 12;
      name_nonreloc = alloca_ibytes (minsize + XSTRING_LENGTH (file) + 8);
      string_join (name_nonreloc, Vdoc_directory, file);
    }
  else
    name_reloc = file;

  fd = qxe_open (name_nonreloc ? name_nonreloc :
		 XSTRING_DATA (name_reloc), O_RDONLY | OPEN_BINARY, 0);
  if (fd < 0)
    {
      if (purify_flag)
	{
	    /* sizeof ("../lib-src/") == 12 */
	  name_nonreloc = alloca_ibytes (12 + XSTRING_LENGTH (file) + 8);
	  /* Preparing to dump; DOC file is probably not installed.
	     So check in ../lib-src. */
	  qxestrcpy_ascii (name_nonreloc, "../lib-src/");
	  qxestrcat (name_nonreloc, XSTRING_DATA (file));

	  fd = qxe_open (name_nonreloc, O_RDONLY | OPEN_BINARY, 0);
	}

      if (fd < 0)
	report_file_error ("Cannot open doc string file",
			   name_nonreloc ? build_intstring (name_nonreloc) :
			   name_reloc);
    }

  tem = unparesseuxify_doc_string (fd, position, name_nonreloc, name_reloc,
				   standard_doc_file);
  retry_close (fd);

  if (!STRINGP (tem))
    signal_error_1 (Qinvalid_byte_code, tem);

  return tem;
}

/* Get a string from position FILEPOS and pass it through the Lisp reader.
   We use this for fetching the bytecode string and constants vector
   of a compiled function from the .elc file.  */

Lisp_Object
read_doc_string (Lisp_Object filepos)
{
  Lisp_Object string = get_doc_string (filepos);

  if (!STRINGP (string))
    invalid_state ("loading bytecode failed to return string", string);
  return Fread (string);
}

static Lisp_Object
get_object_file_name (Lisp_Object filepos)
{
  REGISTER int fd;
  REGISTER Ibyte *name_nonreloc = 0;
  EMACS_INT position;
  Lisp_Object file, tem;
  Lisp_Object name_reloc = Qnil;
  int standard_doc_file = 0;

  if (INTP (filepos))
    {
      file = Vinternal_doc_file_name;
      standard_doc_file = 1;
      position = XINT (filepos);
    }
  else if (CONSP (filepos) && INTP (XCDR (filepos)))
    {
      file = XCAR (filepos);
      position = XINT (XCDR (filepos));
      if (position < 0)
	position = - position;
    }
  else
    return Qnil;

  if (!STRINGP (file))
    return Qnil;

  /* Put the file name in NAME as a C string.
     If it is relative, combine it with Vdoc_directory.  */

  tem = Ffile_name_absolute_p (file);
  if (NILP (tem))
    {
      Bytecount minsize;
      /* XEmacs: Move this check here.  OK if called during loadup to
	 load byte code instructions. */
      if (!STRINGP (Vdoc_directory))
	return Qnil;

      minsize = XSTRING_LENGTH (Vdoc_directory);
      /* sizeof ("../lib-src/") == 12 */
      if (minsize < 12)
	minsize = 12;
      name_nonreloc = alloca_ibytes (minsize + XSTRING_LENGTH (file) + 8);
      string_join (name_nonreloc, Vdoc_directory, file);
    }
  else
    name_reloc = file;

  fd = qxe_open (name_nonreloc ? name_nonreloc :
		 XSTRING_DATA (name_reloc), O_RDONLY | OPEN_BINARY, 0);
  if (fd < 0)
    {
      if (purify_flag)
	{
	    /* sizeof ("../lib-src/") == 12 */
	  name_nonreloc = alloca_ibytes (12 + XSTRING_LENGTH (file) + 8);
	  /* Preparing to dump; DOC file is probably not installed.
	     So check in ../lib-src. */
	  qxestrcpy_ascii (name_nonreloc, "../lib-src/");
	  qxestrcat (name_nonreloc, XSTRING_DATA (file));

	  fd = qxe_open (name_nonreloc, O_RDONLY | OPEN_BINARY, 0);
	}

      if (fd < 0)
	report_file_error ("Cannot open doc string file",
			   name_nonreloc ? build_intstring (name_nonreloc) :
			   name_reloc);
    }

  tem = extract_object_file_name (fd, position, name_nonreloc, name_reloc,
			      standard_doc_file);
  retry_close (fd);

  if (!STRINGP (tem))
    signal_error_1 (Qinvalid_byte_code, tem);

  return tem;
}


static void
weird_doc (Lisp_Object sym, const CIbyte *weirdness, const CIbyte *type,
	   int pos)
{
  if (!strcmp (weirdness, GETTEXT ("duplicate"))) return;
  message ("Note: Strange doc (%s) for %s %s @ %d",
           weirdness, type, XSTRING_DATA (XSYMBOL (sym)->name), pos);
}

DEFUN ("built-in-symbol-file", Fbuilt_in_symbol_file, 1, 1, 0, /*
Return the C source file built-in symbol SYM comes from. 
Don't use this.  Use the more general `symbol-file' (q.v.) instead. 
*/
       (symbol))
{
  /* This function can GC */
  Lisp_Object fun;
  Lisp_Object filename = Qnil;

  if (EQ(Ffboundp(symbol), Qt))
    {
      fun = Findirect_function (symbol);

      if (SUBRP (fun))
	{
	  if (XSUBR (fun)->doc == 0)
	    return Qnil;

	  if ((EMACS_INT) XSUBR (fun)->doc >= 0)
	    {
	      weird_doc (symbol, "No file info available for function",
			 GETTEXT("function"), 0);
	      return Qnil;
	    }
	  else
	    filename = get_object_file_name 
	      (make_int (- (EMACS_INT) XSUBR (fun)->doc));
	}
    }
  else if (EQ(Fboundp(symbol), Qt))
    {
      Lisp_Object doc_offset = Fget (symbol, Qvariable_documentation, Qnil);

      if (!NILP(doc_offset)) 
	{
	  if (INTP(doc_offset))
	    {
	      filename = get_object_file_name 
		(XINT (doc_offset) > 0 ? doc_offset
		 : make_int (- XINT (doc_offset)));
	    }
	  else if (CONSP(doc_offset))
	    {
	      filename = get_object_file_name(doc_offset);
	    }
	}
    }
  return filename;
}

DEFUN ("documentation", Fdocumentation, 1, 2, 0, /*
Return the documentation string of FUNCTION.
Unless a non-nil second argument RAW is given, the
string is passed through `substitute-command-keys'.
*/
       (function, raw))
{
  /* This function can GC */
  Lisp_Object fun;
  Lisp_Object doc;

  fun = Findirect_function (function);

  if (SUBRP (fun))
    {
      if (XSUBR (fun)->doc == 0)
	return Qnil;
      if ((EMACS_INT) XSUBR (fun)->doc >= 0)
	doc = build_string (XSUBR (fun)->doc);
      else
        doc = get_doc_string (make_int (- (EMACS_INT) XSUBR (fun)->doc));
    }
  else if (COMPILED_FUNCTIONP (fun))
    {
      Lisp_Object tem;
      Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);
      if (! (f->flags.documentationp))
        return Qnil;
      tem = compiled_function_documentation (f);
      if (STRINGP (tem))
	doc = tem;
      else if (NATNUMP (tem) || CONSP (tem))
	doc = get_doc_string (tem);
      else
        return Qnil;
    }
  else if (KEYMAPP (fun))
    return build_msg_string ("Prefix command (definition is a keymap of subcommands).");
  else if (STRINGP (fun) || VECTORP (fun))
    return build_msg_string ("Keyboard macro.");
  else if (CONSP (fun))
    {
      Lisp_Object funcar = Fcar (fun);

      if (!SYMBOLP (funcar))
	return Fsignal (Qinvalid_function, list1 (fun));
      else if (EQ (funcar, Qlambda)
             || EQ (funcar, Qautoload))
	{
	  Lisp_Object tem, tem1;
	  tem1 = Fcdr (Fcdr (fun));
	  tem = Fcar (tem1);
	  if (STRINGP (tem))
	    doc = tem;
	  /* Handle a doc reference--but these never come last
	     in the function body, so reject them if they are last.  */
	  else if ((NATNUMP (tem) || CONSP (tem))
		   && ! NILP (XCDR (tem1)))
	    doc = get_doc_string (tem);
	  else
	    return Qnil;
	}
      else if (EQ (funcar, Qmacro))
	return Fdocumentation (Fcdr (fun), raw);
      else
	goto oops;
    }
  else
    {
    oops:
      return Fsignal (Qinvalid_function, list1 (fun));
    }

  if (NILP (raw))
    {
      struct gcpro gcpro1;
#ifdef I18N3
      Lisp_Object domain = Qnil;
      if (COMPILED_FUNCTIONP (fun))
	domain = compiled_function_domain (XCOMPILED_FUNCTION (fun));
      if (NILP (domain))
	doc = Fgettext (doc);
      else
	doc = Fdgettext (domain, doc);
#endif

      GCPRO1 (doc);
      doc = Fsubstitute_command_keys (doc);
      UNGCPRO;
    }
  return doc;
}

DEFUN ("documentation-property", Fdocumentation_property, 2, 3, 0, /*
Return the documentation string that is SYMBOL's PROP property.
This is like `get', but it can refer to strings stored in the
`doc-directory/DOC' file; and if the value is a string, it is passed
through `substitute-command-keys'.  A non-nil third argument avoids this
translation.
*/
       (symbol, prop, raw))
{
  /* This function can GC */
  Lisp_Object doc = Qnil;
#ifdef I18N3
  REGISTER Lisp_Object domain;
#endif
  struct gcpro gcpro1;

  GCPRO1 (doc);

  doc = Fget (symbol, prop, Qnil);
  if (INTP (doc))
    doc = get_doc_string (XINT (doc) > 0 ? doc : make_int (- XINT (doc)));
  else if (CONSP (doc))
    doc = get_doc_string (doc);
#ifdef I18N3
  if (!NILP (doc))
    {
      domain = Fget (symbol, Qvariable_domain, Qnil);
      if (NILP (domain))
	doc = Fgettext (doc);
      else
	doc = Fdgettext (domain, doc);
    }
#endif
  if (NILP (raw) && STRINGP (doc))
    doc = Fsubstitute_command_keys (doc);
  UNGCPRO;
  return doc;
}


DEFUN ("Snarf-documentation", Fsnarf_documentation, 1, 1, 0, /*
Used during Emacs initialization, before dumping runnable Emacs,
to find pointers to doc strings stored in `.../lib-src/DOC' and
record them in function definitions.
One arg, FILENAME, a string which does not include a directory.
The file is written to `../lib-src', and later found in `exec-directory'
when doc strings are referred to in the dumped Emacs.
*/
       (filename))
{
  int fd;
  Ibyte buf[1024 + 1];
  REGISTER int filled;
  REGISTER int pos;
  REGISTER Ibyte *p, *end;
  Lisp_Object sym, fun, tem;
  Ibyte *name;

  /* This function should not pass the data it's reading through a coding
     stream.  The reason is that the only purpose of this function is to
     find the file offsets for the documentation of the various functions,
     not do anything with the documentation itself.  If we pass through a
     coding stream, the pointers will get messed up when we start reading
     ISO 2022 data because our pointers will reflect internal format, not
     external format. */
  
  if (!purify_flag)
    invalid_operation ("Snarf-documentation can only be called in an undumped Emacs", Qunbound);

  CHECK_STRING (filename);

  {
    name = alloca_ibytes (XSTRING_LENGTH (filename) + 14);
    qxestrcpy_ascii (name, "../lib-src/");
  }

  qxestrcat (name, XSTRING_DATA (filename));

  fd = qxe_open (name, O_RDONLY | OPEN_BINARY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file", build_intstring (name));
  Vinternal_doc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += retry_read (fd, &buf[filled], sizeof (buf) - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      /* p points to ^_Ffunctionname\n or ^_Vvarname\n.  */
      if (p != end)
	{
	  end = qxestrchr (p, '\n');
	  sym = oblookup (Vobarray, p + 2, end - p - 2);
	  if (SYMBOLP (sym))
	    {
              Lisp_Object offset = make_int (pos + end + 1 - buf);
	      /* Attach a docstring to a variable */
	      if (p[1] == 'V')
		{
		  /* Install file-position as variable-documentation property
		     and make it negative for a user-variable
		     (doc starts with a `*').  */
		  Lisp_Object old = Fget (sym, Qvariable_documentation, Qzero);
                  if (!ZEROP (old))
		    {
		      weird_doc (sym, GETTEXT ("duplicate"),
				 GETTEXT ("variable"), pos);
		      /* In the case of duplicate doc file entries, always
			 take the later one.  But if the doc is not an int
			 (a string, say) leave it alone. */
		      if (!INTP (old))
			goto weird;
		    }
		  Fput (sym, Qvariable_documentation,
                        ((end[1] == '*')
                         ? make_int (- XINT (offset))
                         : offset));
		}
	      /* Attach a docstring to a function.
                 The type determines where the docstring is stored.  */
	      else if (p[1] == 'F')
		{
                  fun = indirect_function (sym,0);

		  if (CONSP (fun) && EQ (XCAR (fun), Qmacro))
		    fun = XCDR (fun);

                  if (UNBOUNDP (fun))
		    {
#if 0 /* There are lots of legitimate cases where this message will appear
	 (e.g. any function that's only defined when MULE is defined,
	 provided that the function is used somewhere in a dumped Lisp
	 file, so that the symbol is interned in the dumped XEmacs), and
	 there's not a lot that can be done to eliminate the warning other
	 than kludges like moving the function to a Mule-only source file,
	 which often results in ugly code.  Furthermore, the only point of
	 this warning is to warn you when you have a DEFUN that you forget
	 to DEFSUBR, but the compiler will also warn you, because the DEFUN
	 declares a static object, and the object will be unused -- you'll
	 get something like

/src/xemacs/mule/src/abbrev.c:269: warning: `SFexpand_abbrev' defined but not used

	 So I'm disabling this. --ben */

		      /* May have been #if'ed out or something */
		      weird_doc (sym, GETTEXT ("not fboundp"),
				 GETTEXT ("function"), pos);
#endif
		      goto weird;
		    }
		  else if (SUBRP (fun))
		    {
		      /* Lisp_Subrs have a slot for it.  */
		      if (XSUBR (fun)->doc)
			{
			  weird_doc (sym, GETTEXT ("duplicate"),
				     GETTEXT ("subr"), pos);
			  goto weird;
			}
		      XSUBR (fun)->doc = (char *) (- XINT (offset));
		    }
		  else if (CONSP (fun))
		    {
                      /* If it's a lisp form, stick it in the form.  */
		      tem = XCAR (fun);
		      if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
			{
			  tem = Fcdr (Fcdr (fun));
			  if (CONSP (tem) &&
			      INTP (XCAR (tem)))
			    {
			      Lisp_Object old = XCAR (tem);
			      if (!ZEROP (old))
				{
				  weird_doc (sym, GETTEXT ("duplicate"),
					     (EQ (tem, Qlambda)
					      ? GETTEXT ("lambda")
					      : GETTEXT ("autoload")),
					     pos);
				  /* In the case of duplicate doc file entries,
				     always take the later one.  But if the doc
				     is not an int (a string, say) leave it
				     alone. */
				  if (!INTP (old))
				    goto weird;
				}
			      XCAR (tem) = offset;
			    }
                          else if (!CONSP (tem))
			    {
			      weird_doc (sym, GETTEXT ("!CONSP(tem)"),
					 GETTEXT ("function"), pos);
			      goto cont;
			    }
                          else
			    {
			      /* DOC string is a string not integer 0 */
#if 0
			      weird_doc (sym, GETTEXT ("!INTP(XCAR(tem))"),
					 GETTEXT ("function"), pos);
#endif
			      goto cont;
			    }
                        }
                      else
			{
			  weird_doc (sym, GETTEXT ("not lambda or autoload"),
				     GETTEXT ("function"), pos);
			  goto cont;
			}
		    }
		  else if (COMPILED_FUNCTIONP (fun))
		    {
                      /* Compiled-Function objects sometimes have
                         slots for it.  */
                      Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);

		      /* This compiled-function object must have a
			 slot for the docstring, since we've found a
			 docstring for it.  Unless there were multiple
			 definitions of it, and the latter one didn't
			 have any doc, which is a legal if slightly
			 bogus situation, so don't blow up. */

                      if (! (f->flags.documentationp))
			{
			  weird_doc (sym, GETTEXT ("no doc slot"),
				     GETTEXT ("bytecode"), pos);
			  goto weird;
			}
		      else
			{
			  Lisp_Object old =
			    compiled_function_documentation (f);
			  if (!ZEROP (old))
			    {
			      weird_doc (sym, GETTEXT ("duplicate"),
					 GETTEXT ("bytecode"), pos);
			      /* In the case of duplicate doc file entries,
				 always take the later one.  But if the doc is
				 not an int (a string, say) leave it alone. */
			      if (!INTP (old))
				goto weird;
			    }
			  set_compiled_function_documentation (f, offset);
			}
                    }
                  else
                    {
                      /* Otherwise the function is undefined or
                         otherwise weird.   Ignore it. */
                      weird_doc (sym, GETTEXT ("weird function"),
				 GETTEXT ("function"), pos);
                      goto weird;
                    }
                }
	      else
                {
                /* lose: */
                  signal_error (Qfile_error, "DOC file invalid at position",
				make_int (pos));
                weird:
                  /* goto lose */;
                }
            }
	}
    cont:
      pos += end - buf;
      filled -= end - buf;
      memmove (buf, end, filled);
    }
  retry_close (fd);
  return Qnil;
}

#if 1	/* Don't warn about functions whose doc was lost because they were
	   wrapped by advice-freeze.el... */
static int
kludgily_ignore_lost_doc_p (Lisp_Object sym)
{
# define kludge_prefix "ad-Orig-"
  Lisp_Object name = XSYMBOL (sym)->name;
  return (XSTRING_LENGTH (name) > (Bytecount) (sizeof (kludge_prefix)) &&
	  !qxestrncmp_ascii (XSTRING_DATA (name), kludge_prefix,
			 sizeof (kludge_prefix) - 1));
# undef kludge_prefix
}
#else
# define kludgily_ignore_lost_doc_p(sym) 0
#endif


static int
verify_doc_mapper (Lisp_Object sym, void *arg)
{
  Lisp_Object closure = * (Lisp_Object *) arg;

  if (!NILP (Ffboundp (sym)))
    {
      int doc = 0;
      Lisp_Object fun = XSYMBOL (sym)->function;
      if (CONSP (fun) &&
	  EQ (XCAR (fun), Qmacro))
	fun = XCDR (fun);

      if (SUBRP (fun))
	doc = (EMACS_INT) XSUBR (fun)->doc;
      else if (SYMBOLP (fun))
	doc = -1;
      else if (KEYMAPP (fun))
	doc = -1;
      else if (CONSP (fun))
	{
	  Lisp_Object tem = XCAR (fun);
	  if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
	    {
	      doc = -1;
	      tem = Fcdr (Fcdr (fun));
	      if (CONSP (tem) &&
		  INTP (XCAR (tem)))
		doc = XINT (XCAR (tem));
	    }
	}
      else if (COMPILED_FUNCTIONP (fun))
	{
          Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);
          if (! (f->flags.documentationp))
            doc = -1;
          else
            {
              Lisp_Object tem = compiled_function_documentation (f);
              if (INTP (tem))
                doc = XINT (tem);
            }
	}

      if (doc == 0 && !kludgily_ignore_lost_doc_p (sym))
	{
	  message ("Warning: doc lost for function %s.",
		   XSTRING_DATA (XSYMBOL (sym)->name));
	  XCDR (closure) = Qt;
	}
    }
  if (!NILP (Fboundp (sym)))
    {
      Lisp_Object doc = Fget (sym, Qvariable_documentation, Qnil);
      if (ZEROP (doc))
	{
	  message ("Warning: doc lost for variable %s.",
		   XSTRING_DATA (XSYMBOL (sym)->name));
	  XCDR (closure) = Qt;
	}
    }
  return 0; /* Never stop */
}

DEFUN ("Verify-documentation", Fverify_documentation, 0, 0, 0, /*
Used to make sure everything went well with Snarf-documentation.
Writes to stderr if not.
*/
       ())
{
  Lisp_Object closure = Fcons (Qnil, Qnil);
  struct gcpro gcpro1;
  GCPRO1 (closure);
  map_obarray (Vobarray, verify_doc_mapper, &closure);
  if (!NILP (Fcdr (closure)))
    message ("\n"
"This is usually because some files were preloaded by loaddefs.el or\n"
"site-load.el, but were not passed to make-docfile by Makefile.\n");
  UNGCPRO;
  return NILP (Fcdr (closure)) ? Qt : Qnil;
}


DEFUN ("substitute-command-keys", Fsubstitute_command_keys, 1, 1, 0, /*
Substitute key descriptions for command names in STRING.
Return a new string which is STRING with substrings of the form \\=\\[COMMAND]
replaced by either:  a keystroke sequence that will invoke COMMAND,
or "M-x COMMAND" if COMMAND is not on any keys.
Substrings of the form \\=\\{MAPVAR} are replaced by summaries
\(made by `describe-bindings') of the value of MAPVAR, taken as a keymap.
Substrings of the form \\=\\<MAPVAR> specify to use the value of MAPVAR
as the keymap for future \\=\\[COMMAND] substrings.
\\=\\= quotes the following character and is discarded;
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.
*/
       (string))
{
  /* This function can GC */
  Ibyte *buf;
  int changed = 0;
  REGISTER Ibyte *strdata;
  REGISTER Ibyte *bufp;
  Bytecount strlength;
  Bytecount idx;
  Bytecount bsize;
  Ibyte *new_;
  Lisp_Object tem = Qnil;
  Lisp_Object keymap = Qnil;
  Lisp_Object name = Qnil;
  Ibyte *start;
  Bytecount length;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  if (NILP (string))
    return Qnil;

  CHECK_STRING (string);
  GCPRO4 (string, tem, keymap, name);

  /* There is the possibility that the string is not destined for a
     translating stream, and it could be argued that we should do the
     same thing here as in Fformat(), but there are very few times
     when this will be the case and many calls to this function
     would have to have `gettext' calls added. (I18N3) */
  string = LISP_GETTEXT (string);

  /* KEYMAP is either nil (which means search all the active keymaps)
     or a specified local map (which means search just that and the
     global map).  If non-nil, it might come from Voverriding_local_map,
     or from a \\<mapname> construct in STRING itself..  */
#if 0 /* FSFmacs */
  /* This is really weird and garbagey.  If keymap is nil and there's
     an overriding-local-map, `where-is-internal' will correctly note
     this, so there's no reason to do it here.  Maybe FSFmacs
     `where-is-internal' is broken. */
  /*
  keymap = current_kboard->Voverriding_terminal_local_map;
  if (NILP (keymap))
    keymap = Voverriding_local_map;
  */
#endif

  strlength = XSTRING_LENGTH (string);
  bsize = ITEXT_ZTERM_SIZE + strlength;
  buf = xnew_ibytes (bsize);
  bufp = buf;

  /* Have to reset strdata every time GC might be called */
  strdata = XSTRING_DATA (string);
  for (idx = 0; idx < strlength; )
    {
      Ibyte *strp = strdata + idx;

      if (strp[0] != '\\')
	{
	  /* just copy other chars */
	  /* As it happens, this will work with Mule even if the
	     character quoted is multi-byte; the remaining multi-byte
	     characters will just be copied by this loop. */
	  *bufp++ = *strp;
	  idx++;
	}
      else switch (strp[1])
	{
	default:
	  {
	    /* just copy unknown escape sequences */
	    *bufp++ = *strp;
	    idx++;
	    break;
	  }
	case '=':
	  {
	    /* \= quotes the next character;
	       thus, to put in \[ without its special meaning, use \=\[.  */
	    /* As it happens, this will work with Mule even if the
	       character quoted is multi-byte; the remaining multi-byte
	       characters will just be copied by this loop. */
	    changed = 1;
	    *bufp++ = strp[2];
	    idx += 3;
	    break;
	  }
	case '[':
	  {
	    changed = 1;
	    idx += 2;		/* skip \[ */
	    strp += 2;
	    start = strp;

	    while ((idx < strlength)
		   && *strp != ']')
	      {
		strp++;
		idx++;
	      }
	    length = strp - start;
	    idx++;		/* skip ] */

	    tem = Fintern (make_string (start, length), Qnil);
	    tem = Fwhere_is_internal (tem, keymap, Qt, Qnil, Qnil);

#if 0 /* FSFmacs */
	    /* Disregard menu bar bindings; it is positively annoying to
	       mention them when there's no menu bar, and it isn't terribly
	       useful even when there is a menu bar.  */
	    if (!NILP (tem))
	      {
		firstkey = Faref (tem, Qzero);
		if (EQ (firstkey, Qmenu_bar))
		  tem = Qnil;
	      }
#endif

	    if (NILP (tem))	/* but not on any keys */
	      {
		new_ = (Ibyte *) xrealloc (buf, bsize += 4);
		bufp += new_ - buf;
		buf = new_;
		memcpy (bufp, "M-x ", 4);
		bufp += 4;
		goto subst;
	      }
	    else
	      {			/* function is on a key */
		tem = Fkey_description (tem);
		goto subst_string;
	      }
	  }
	case '{':
	case '<':
	  {
	    Lisp_Object buffer = Fget_buffer_create (QSsubstitute);
	    struct buffer *buf_ = XBUFFER (buffer);

	    Fbuffer_disable_undo (buffer);
	    Ferase_buffer (buffer);

	    /* \{foo} is replaced with a summary of keymap (symbol-value foo).
	       \<foo> just sets the keymap used for \[cmd].  */
	    changed = 1;
	    idx += 2;		/* skip \{ or \< */
	    strp += 2;
	    start = strp;

	    while ((idx < strlength)
		   && *strp != '}' && *strp != '>')
	      {
		strp++;
		idx++;
	      }
	    length = strp - start;
	    idx++;		/* skip } or > */

	    /* Get the value of the keymap in TEM, or nil if undefined.
	       Do this while still in the user's current buffer
	       in case it is a local variable.  */
	    name = Fintern (make_string (start, length), Qnil);
	    tem = Fboundp (name);
	    if (! NILP (tem))
	      {
		tem = Fsymbol_value (name);
		if (! NILP (tem))
		  tem = get_keymap (tem, 0, 1);
	      }

	    if (NILP (tem))
	      {
		buffer_insert_c_string (buf_, "(uses keymap \"");
		buffer_insert_lisp_string (buf_, Fsymbol_name (name));
		buffer_insert_c_string (buf_, "\", which is not currently defined) ");

		if (start[-1] == '<') keymap = Qnil;
	      }
	    else if (start[-1] == '<')
	      keymap = tem;
	    else
	      describe_map_tree (tem, 1, Qnil, Qnil, 0, buffer);

	    tem = make_string_from_buffer (buf_, BUF_BEG (buf_),
					   BUF_Z (buf_) - BUF_BEG (buf_));
	    Ferase_buffer (buffer);
	  }
	  goto subst_string;

	subst_string:
	  start = XSTRING_DATA (tem);
	  length = XSTRING_LENGTH (tem);
	subst:
	  bsize += length;
	  new_ = (Ibyte *) xrealloc (buf, bsize);
	  bufp += new_ - buf;
	  buf = new_;
	  memcpy (bufp, start, length);
	  bufp += length;

	  /* Reset STRDATA in case gc relocated it.  */
	  strdata = XSTRING_DATA (string);

	  break;
	}
    }

  if (changed)			/* don't bother if nothing substituted */
    tem = make_string (buf, bufp - buf);
  else
    tem = string;
  xfree (buf, Ibyte *);
  UNGCPRO;
  return tem;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_doc (void)
{
  DEFSUBR (Fbuilt_in_symbol_file);
  DEFSUBR (Fdocumentation);
  DEFSUBR (Fdocumentation_property);
  DEFSUBR (Fsnarf_documentation);
  DEFSUBR (Fverify_documentation);
  DEFSUBR (Fsubstitute_command_keys);
}

void
vars_of_doc (void)
{
  DEFVAR_LISP ("internal-doc-file-name", &Vinternal_doc_file_name /*
Name of file containing documentation strings of built-in symbols.
*/ );
  Vinternal_doc_file_name = Qnil;

  QSsubstitute = build_string (" *substitute*");
  staticpro (&QSsubstitute);
}
