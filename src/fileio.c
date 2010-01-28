/* File IO for XEmacs.
   Copyright (C) 1985-1988, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002, 2003, 2004, 2010 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30. */
/* More syncing: FSF Emacs 19.34.6 by Marc Paquette <marcpa@cam.org>
   (Note: Sync messages from Marc Paquette may indicate
   incomplete synching, so beware.) */
/* Some functions synched with FSF 21.0.103. */
/* Mule-ized completely except for the #if 0-code including decrypt-string
   and encrypt-string. --ben 7-2-00 */
/* #if 0-code Mule-ized, 2-22-03. --ben */


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "events.h"
#include "file-coding.h"
#include "frame.h"
#include "insdel.h"
#include "lstream.h"
#include "profile.h"
#include "process.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window-impl.h"

#include "sysfile.h"
#include "sysproc.h"
#include "syspwd.h"
#include "systime.h"
#include "sysdir.h"

#ifdef HPUX
#include <netio.h>
#endif /* HPUX */

#ifdef WIN32_ANY
#define WIN32_FILENAMES
#include "syswindows.h"
#define IS_DRIVE(x) isalpha (x)
/* Need to lower-case the drive letter, or else expanded
   filenames will sometimes compare inequal, because
   `expand-file-name' doesn't always down-case the drive letter.  */
#define DRIVE_LETTER(x) tolower (x)
#endif /* WIN32_NATIVE || CYGWIN */

int lisp_to_time (Lisp_Object, time_t *);
Lisp_Object time_to_lisp (time_t);

/* Nonzero during writing of auto-save files */
static int auto_saving;

/* Set by auto_save_1 to mode of original file so Fwrite_region_internal
   will create a new file with the same mode as the original */
static int auto_save_mode_bits;

/* Alist of elements (REGEXP . HANDLER) for file names
   whose I/O is done with a special handler.  */
Lisp_Object Vfile_name_handler_alist;

/* Format for auto-save files */
Lisp_Object Vauto_save_file_format;

/* Lisp functions for translating file formats */
Lisp_Object Qformat_decode, Qformat_annotate_function;

/* Functions to be called to process text properties in inserted file.  */
Lisp_Object Vafter_insert_file_functions;

/* Functions to be called to create text property annotations for file.  */
Lisp_Object Vwrite_region_annotate_functions;

/* During build_annotations, each time an annotation function is called,
   this holds the annotations made by the previous functions.  */
Lisp_Object Vwrite_region_annotations_so_far;

/* File name in which we write a list of all our auto save files.  */
Lisp_Object Vauto_save_list_file_name;

/* Prefix used to construct Vauto_save_list_file_name. */
Lisp_Object Vauto_save_list_file_prefix;

/* When non-nil, it prevents auto-save list file creation. */
int inhibit_auto_save_session;

int disable_auto_save_when_buffer_shrinks;

Lisp_Object Vdirectory_sep_char;

#ifdef HAVE_FSYNC
/* Nonzero means skip the call to fsync in Fwrite-region.  */
int write_region_inhibit_fsync;
#endif

/* These variables describe handlers that have "already" had a chance
   to handle the current operation.

   Vinhibit_file_name_handlers is a list of file name handlers.
   Vinhibit_file_name_operation is the operation being handled.
   If we try to handle that operation, we ignore those handlers.  */

static Lisp_Object Vinhibit_file_name_handlers;
static Lisp_Object Vinhibit_file_name_operation;

Lisp_Object Qfile_already_exists;
Lisp_Object Qexcl;

Lisp_Object Qauto_save_hook;
Lisp_Object Qauto_save_error;
Lisp_Object Qauto_saving;

Lisp_Object Qcar_less_than_car;

Lisp_Object Qcompute_buffer_file_truename;

Lisp_Object QSin_expand_file_name;

EXFUN (Frunning_temacs_p, 0);

/* DATA can be anything acceptable to signal_error ().
 */

DOESNT_RETURN
report_file_type_error (Lisp_Object errtype, Lisp_Object oserrmess,
			const Ascbyte *reason, Lisp_Object data)
{
  struct gcpro gcpro1;
  Lisp_Object errdata = build_error_data (NULL, data);

  GCPRO1 (errdata);
  errdata = Fcons (build_msg_string (reason),
		   Fcons (oserrmess, errdata));
  signal_error_1 (errtype, errdata);
  /* UNGCPRO; not reached */
}

DOESNT_RETURN
report_error_with_errno (Lisp_Object errtype,
			 const Ascbyte *reason, Lisp_Object data)
{
  report_file_type_error (errtype, lisp_strerror (errno), reason, data);
}

/* signal a file error when errno contains a meaningful value. */

DOESNT_RETURN
report_file_error (const Ascbyte *reason, Lisp_Object data)
{
  report_error_with_errno (Qfile_error, reason, data);
}


/* Just like strerror(3), except return a lisp string instead of char *.
   The string needs to be converted since it may be localized.
*/
Lisp_Object
lisp_strerror (int errnum)
{
  Extbyte *ret = strerror (errnum);
  if (!ret)
    {
      Ibyte ffff[99];
      qxesprintf (ffff, "Unknown error %d", errnum);
      return build_istring (ffff);
    }
  return build_extstring (ret, Qstrerror_encoding);
}

static Lisp_Object
close_file_unwind (Lisp_Object fd)
{
  if (CONSP (fd))
    {
      if (INTP (XCAR (fd)))
	retry_close (XINT (XCAR (fd)));

      free_cons (fd);
    }
  else
    retry_close (XINT (fd));

  return Qnil;
}

static Lisp_Object
delete_stream_unwind (Lisp_Object stream)
{
  Lstream_delete (XLSTREAM (stream));
  return Qnil;
}

/* Restore point, having saved it as a marker.  */

static Lisp_Object
restore_point_unwind (Lisp_Object point_marker)
{
  BUF_SET_PT (current_buffer, marker_position (point_marker));
  return Fset_marker (point_marker, Qnil, Qnil);
}


Lisp_Object Qexpand_file_name;
Lisp_Object Qfile_truename;
Lisp_Object Qsubstitute_in_file_name;
Lisp_Object Qdirectory_file_name;
Lisp_Object Qfile_name_directory;
Lisp_Object Qfile_name_nondirectory;
Lisp_Object Qfile_name_sans_extension;
Lisp_Object Qunhandled_file_name_directory;
Lisp_Object Qfile_name_as_directory;
Lisp_Object Qcopy_file;
Lisp_Object Qmake_directory_internal;
Lisp_Object Qdelete_directory;
Lisp_Object Qdelete_file;
Lisp_Object Qrename_file;
Lisp_Object Qadd_name_to_file;
Lisp_Object Qmake_symbolic_link;
Lisp_Object Qmake_temp_name;
Lisp_Object Qfile_exists_p;
Lisp_Object Qfile_executable_p;
Lisp_Object Qfile_readable_p;
Lisp_Object Qfile_symlink_p;
Lisp_Object Qfile_writable_p;
Lisp_Object Qfile_directory_p;
Lisp_Object Qfile_regular_p;
Lisp_Object Qfile_accessible_directory_p;
Lisp_Object Qfile_modes;
Lisp_Object Qset_file_modes;
Lisp_Object Qfile_newer_than_file_p;
Lisp_Object Qinsert_file_contents;
Lisp_Object Qwrite_region;
Lisp_Object Qverify_visited_file_modtime;
Lisp_Object Qset_visited_file_modtime;

/* If FILENAME is handled specially on account of its syntax,
   return its handler function.  Otherwise, return nil.  */

DEFUN ("find-file-name-handler", Ffind_file_name_handler, 1, 2, 0, /*
Return FILENAME's handler function for OPERATION, if it has one.
Otherwise, return nil.
A file name is handled if one of the regular expressions in
`file-name-handler-alist' matches it.

If OPERATION equals `inhibit-file-name-operation', then we ignore
any handlers that are members of `inhibit-file-name-handlers',
but we still do run any other handlers.  This lets handlers
use the standard functions without calling themselves recursively.

Otherwise, OPERATION is the name of a funcall'able function.
*/
       (filename, operation))
{
  /* This function does not GC */
  /* This function can be called during GC */
  /* This function must not munge the match data.  */
  Lisp_Object inhibited_handlers;

  CHECK_STRING (filename);

  if (EQ (operation, Vinhibit_file_name_operation))
    inhibited_handlers = Vinhibit_file_name_handlers;
  else
    inhibited_handlers = Qnil;

  {
    EXTERNAL_LIST_LOOP_2 (elt, Vfile_name_handler_alist)
      {
	if (CONSP (elt))
	  {
	    Lisp_Object string = XCAR (elt);
	    if (STRINGP (string)
		&& (fast_lisp_string_match (string, filename) >= 0))
	      {
		Lisp_Object handler = XCDR (elt);
		if (NILP (Fmemq (handler, inhibited_handlers)))
		  return handler;
	      }
	  }
      }
  }
  return Qnil;
}

static Lisp_Object
call2_check_string (Lisp_Object fn, Lisp_Object arg0, Lisp_Object arg1)
{
  /* This function can call lisp */
  Lisp_Object result = call2 (fn, arg0, arg1);
  CHECK_STRING (result);
  return result;
}

static Lisp_Object
call2_check_string_or_nil (Lisp_Object fn, Lisp_Object arg0, Lisp_Object arg1)
{
  /* This function can call lisp */
  Lisp_Object result = call2 (fn, arg0, arg1);
  if (!NILP (result))
    CHECK_STRING (result);
  return result;
}

static Lisp_Object
call3_check_string (Lisp_Object fn, Lisp_Object arg0,
                    Lisp_Object arg1, Lisp_Object arg2)
{
  /* This function can call lisp */
  Lisp_Object result = call3 (fn, arg0, arg1, arg2);
  CHECK_STRING (result);
  return result;
}



Ibyte *
find_end_of_directory_component (const Ibyte *path, Bytecount len)
{
  const Ibyte *p = path + len;

  while (p != path && !IS_DIRECTORY_SEP (p[-1])
#ifdef WIN32_FILENAMES
	 /* only recognise drive specifier at the beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" and "/:foo" cases correctly  */
	      && ((p == path + 2 && !IS_DIRECTORY_SEP (*path))
		  || (p == path + 4 && IS_DIRECTORY_SEP (*path))))
#endif
	 ) p--;

  return (Ibyte *) p;
}

DEFUN ("file-name-directory", Ffile_name_directory, 1, 1, 0, /*
Return the directory component in file name FILENAME.
Return nil if FILENAME does not include a directory.
Otherwise return a directory spec.
Given a Unix syntax file name, returns a string ending in slash.
*/
       (filename))
{
  /* This function can GC.  GC checked 2000-07-28 ben */
  /* This function synched with Emacs 21.0.103. */
  Ibyte *beg;
  Ibyte *p;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_directory);
  if (!NILP (handler))
    return call2_check_string_or_nil (handler, Qfile_name_directory, filename);

#ifdef FILE_SYSTEM_CASE
  filename = FILE_SYSTEM_CASE (filename);
#endif
  beg = XSTRING_DATA (filename);
  /* XEmacs: no need to alloca-copy here */
  p = find_end_of_directory_component (beg, XSTRING_LENGTH (filename));

  if (p == beg)
    return Qnil;
#ifdef WIN32_NATIVE
  /* Expansion of "c:" to drive and default directory.  */
  if (p[-1] == ':')
    {
      Ibyte *res;
      Ibyte *wd = mswindows_getdcwd (toupper (*beg) - 'A' + 1);
      
      res = alloca_ibytes ((wd ? qxestrlen (wd) : 0) + 10); /* go overboard */
      res[0] = '\0';
      if (p == beg + 4 && IS_DIRECTORY_SEP (*beg) && beg[1] == ':')
	{
	  qxestrncpy (res, beg, 2);
	  beg += 2;
	  res[2] = '\0';
	}

      if (wd)
	{
	  int size;
	  qxestrcat (res, wd);
	  size = qxestrlen (res);
	  if (!IS_DIRECTORY_SEP (res[size - 1]))
	    {
	      res[size] = DIRECTORY_SEP;
	      res[size + 1] = '\0';
	    }
	  beg = res;
	  p = beg + qxestrlen (beg);
	}
      else
	{
	  return Qnil;
	}
      if (wd)
	xfree (wd, Ibyte *);
    }

#if 0 /* No! This screws up efs, which calls file-name-directory on URL's
	 and expects the slashes to be left alone.  This is here because of
	 an analogous call in FSF 21. */
  {
    Bytecount len = p - beg;
    Ibyte *newbeg = alloca_ibytes (len + 1);

    qxestrncpy (newbeg, beg, len);
    newbeg[len] = '\0';
    newbeg = mswindows_canonicalize_filename (newbeg);
    return build_istring (newbeg);
  }
#endif
#endif /* not WIN32_NATIVE */
  return make_string (beg, p - beg);
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory, 1, 1, 0, /*
Return file name FILENAME sans its directory.
For example, in a Unix-syntax file name,
this is everything after the last slash,
or the entire name if it contains no slash.
*/
       (filename))
{
  /* This function can GC.  GC checked 2000-07-28 ben */
  /* This function synched with Emacs 21.0.103. */
  Ibyte *beg, *p, *end;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_nondirectory);
  if (!NILP (handler))
    return call2_check_string (handler, Qfile_name_nondirectory, filename);

  beg = XSTRING_DATA (filename);
  end = p = beg + XSTRING_LENGTH (filename);

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef WIN32_FILENAMES
	 /* only recognise drive specifier at beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" case correctly  */
	      && (p == beg + 2 || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 )
    p--;

  return make_string (p, end - p);
}

DEFUN ("unhandled-file-name-directory", Funhandled_file_name_directory, 1, 1, 0, /*
Return a directly usable directory name somehow associated with FILENAME.
A `directly usable' directory name is one that may be used without the
intervention of any file handler.
If FILENAME is a directly usable file itself, return
\(file-name-directory FILENAME).
The `call-process' and `start-process' functions use this function to
get a current directory to run processes in.
*/
       (filename))
{
  /* This function can GC.  GC checked 2000-07-28 ben */
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qunhandled_file_name_directory);
  if (!NILP (handler))
    return call2 (handler, Qunhandled_file_name_directory,
		  filename);

  return Ffile_name_directory (filename);
}


static Ibyte *
file_name_as_directory (Ibyte *out, Ibyte *in)
{
  /* This function cannot GC */
  int size = qxestrlen (in);

  if (size == 0)
    {
      out[0] = '.';
      out[1] = DIRECTORY_SEP;
      out[2] = '\0';
    }
  else
    {
      qxestrcpy (out, in);
      /* Append a slash if necessary */
      if (!IS_ANY_SEP (out[size-1]))
	{
	  out[size] = DIRECTORY_SEP;
	  out[size + 1] = '\0';
	}
    }
  return out;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory, 1, 1, 0, /*
Return a string representing file FILENAME interpreted as a directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
The result can be used as the value of `default-directory'
or passed as second argument to `expand-file-name'.
For a Unix-syntax file name, just appends a slash,
except for (file-name-as-directory \"\") => \"./\".
*/
       (filename))
{
  /* This function can GC.  GC checked 2000-07-28 ben */
  Ibyte *buf;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_as_directory);
  if (!NILP (handler))
    return call2_check_string (handler, Qfile_name_as_directory, filename);

  buf = alloca_ibytes (XSTRING_LENGTH (filename) + 10);
  file_name_as_directory (buf, XSTRING_DATA (filename));
  if (qxestrcmp (buf, XSTRING_DATA (filename)))
    return build_istring (buf);
  else
    return filename;
}

/*
 * Convert from directory name to filename.
 * On UNIX, it's simple: just make sure there isn't a terminating /
 *
 * Value is nonzero if the string output is different from the input.
 */

static int
directory_file_name (const Ibyte *src, Ibyte *dst)
{
  /* This function cannot GC */
  long slen = qxestrlen (src);
  /* Process as Unix format: just remove any final slash.
     But leave "/" unchanged; do not change it to "".  */
  qxestrcpy (dst, src);
  if (slen > 1
      && IS_DIRECTORY_SEP (dst[slen - 1])
#ifdef WIN32_FILENAMES
      && !IS_ANY_SEP (dst[slen - 2])
#endif /* WIN32_FILENAMES */
      )
    dst[slen - 1] = 0;
  return 1;
}

DEFUN ("directory-file-name", Fdirectory_file_name, 1, 1, 0, /*
Return the file name of the directory named DIRECTORY.
This is the name of the file that holds the data for the directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
In Unix-syntax, this function just removes the final slash.
*/
       (directory))
{
  /* This function can GC.  GC checked 2000-07-28 ben */
  Ibyte *buf;
  Lisp_Object handler;

  CHECK_STRING (directory);

#if 0 /* #### WTF? */
  if (NILP (directory))
    return Qnil;
#endif

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_file_name);
  if (!NILP (handler))
    return call2_check_string (handler, Qdirectory_file_name, directory);
  buf = alloca_ibytes (XSTRING_LENGTH (directory) + 20);
  directory_file_name (XSTRING_DATA (directory), buf);
  return build_istring (buf);
}

/* Fmake_temp_name used to be a simple wrapper around mktemp(), but it
   proved too broken for our purposes (it supported only 26 or 62
   unique names under some implementations).  For example, this
   arbitrary limit broke generation of Gnus Incoming* files.

   This implementation is better than what one usually finds in libc.
   --hniksic */

static unsigned int temp_name_rand;

DEFUN ("make-temp-name", Fmake_temp_name, 1, 1, 0, /*
Generate a temporary file name starting with PREFIX.
The Emacs process number forms part of the result, so there is no
danger of generating a name being used by another process.

In addition, this function makes an attempt to choose a name that
does not specify an existing file.  To make this work, PREFIX should
be an absolute file name.

This function is analagous to mktemp(3) under POSIX, and as with it, there
exists a race condition between the test for the existence of the new file
and its creation.  See `make-temp-file' for a function which avoids this
race condition by specifying the appropriate flags to `write-region'. 
*/
       (prefix))
{
  static const char tbl[64] =
  {
    'A','B','C','D','E','F','G','H',
    'I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X',
    'Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n',
    'o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3',
    '4','5','6','7','8','9','-','_'
  };

  Bytecount len;
  Ibyte *p, *data;
  Lisp_Object handler;

  CHECK_STRING (prefix);
  handler = Ffind_file_name_handler (prefix, Qmake_temp_name);
  if (!NILP (handler))
    return call2_check_string (handler, Qmake_temp_name, prefix);

  /* I was tempted to apply Fexpand_file_name on PREFIX here, but it's
     a bad idea because:

     1) It might change the prefix, so the resulting string might not
     begin with PREFIX.  This violates the principle of least
     surprise.

     2) It breaks under many unforeseeable circumstances, such as with
     the code that uses (make-temp-name "") instead of
     (make-temp-name "./").

    [[ 3) It might yield unexpected (to stat(2)) results in the presence
     of EFS and file name handlers.]] Now that we check for a handler,
     that's less of a concern. --ben */

  len = XSTRING_LENGTH (prefix);
  data = alloca_ibytes (len + 7);
  memcpy (data, XSTRING_DATA (prefix), len);
  p = data + len;
  p[6] = '\0';

  /* VAL is created by adding 6 characters to PREFIX.  The first three
     are the PID of this process, in base 64, and the second three are
     a pseudo-random number seeded from process startup time.  This
     ensures 262144 unique file names per PID per PREFIX per machine.  */

  {
    unsigned int pid = (unsigned int) qxe_getpid ();
    *p++ = tbl[(pid >>  0) & 63];
    *p++ = tbl[(pid >>  6) & 63];
    *p++ = tbl[(pid >> 12) & 63];
  }

  /* Here we try to minimize useless stat'ing when this function is
     invoked many times successively with the same PREFIX.  We achieve
     this by using a very pseudo-random number generator to generate
     file names unique to this process, with a very long cycle. */

  while (1)
    {
      struct stat ignored;

      p[0] = tbl[(temp_name_rand >>  0) & 63];
      p[1] = tbl[(temp_name_rand >>  6) & 63];
      p[2] = tbl[(temp_name_rand >> 12) & 63];

      /* Poor man's congruential RN generator.  Replace with ++count
         for debugging.  */
      temp_name_rand += 25229;
      temp_name_rand %= 225307;

      QUIT;

      if (qxe_stat (data, &ignored) < 0)
	{
	  /* We want to return only if errno is ENOENT.  */
	  if (errno == ENOENT)
	    return make_string (data, len + 6);

	  /* The error here is dubious, but there is little else we
	     can do.  The alternatives are to return nil, which is
	     as bad as (and in many cases worse than) throwing the
	     error, or to ignore the error, which will likely result
	     in inflooping.  */
	  report_file_error ("Cannot create temporary name for prefix",
			     prefix);
	  return Qnil; /* not reached */
	}
    }
}



DEFUN ("expand-file-name", Fexpand_file_name, 1, 2, 0, /*
Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
 (does not start with slash); if DEFAULT-DIRECTORY is nil or missing,
the current buffer's value of `default-directory' is used.
File name components that are `.' are removed, and
so are file name components followed by `..', along with the `..' itself;
note that these simplifications are done without checking the resulting
file names in the file system.
An initial `~/' expands to your home directory.
An initial `~USER/' expands to USER's home directory.
See also the function `substitute-in-file-name'.
*/
       (name, default_directory))
{
  /* This function can GC.  GC-checked 2000-11-18.
     This function synched with Emacs 21.0.103. */
  Ibyte *nm;

  Ibyte *newdir, *p, *o;
  int tlen;
  Ibyte *target;
#ifdef WIN32_FILENAMES
  int drive = 0;
  int collapse_newdir = 1;
  /* XEmacs note: This concerns the special '/:' syntax for preventing
     wildcards and such.  We don't support this currently but I'm
     keeping the code here in case we do. */
  int is_escaped = 0;
#endif
#ifndef WIN32_NATIVE
  struct passwd *pw;
#endif
  int length;
  Lisp_Object handler = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_expand_file_name);

  /* both of these get set below */
  GCPRO3 (name, default_directory, handler);

  CHECK_STRING (name);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (name, Qexpand_file_name);
  if (!NILP (handler))
    RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name,
				   call3_check_string
				   (handler, Qexpand_file_name,
				    name, default_directory));

  /* Use the buffer's default-directory if DEFAULT_DIRECTORY is omitted.  */
  if (NILP (default_directory))
    default_directory = current_buffer->directory;
  if (! STRINGP (default_directory))
    default_directory = build_ascstring (DEFAULT_DIRECTORY_FALLBACK);

  if (!NILP (default_directory))
    {
      handler = Ffind_file_name_handler (default_directory, Qexpand_file_name);
      if (!NILP (handler))
	RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name,
				       call3_check_string
                                       (handler, Qexpand_file_name,
				        name, default_directory));
    }

  o = XSTRING_DATA (default_directory);

  /* Make sure DEFAULT_DIRECTORY is properly expanded.
     It would be better to do this down below where we actually use
     default_directory.  Unfortunately, calling Fexpand_file_name recursively
     could invoke GC, and the strings might be relocated.  This would
     be annoying because we have pointers into strings lying around
     that would need adjusting, and people would add new pointers to
     the code and forget to adjust them, resulting in intermittent bugs.
     Putting this call here avoids all that crud.

     The EQ test avoids infinite recursion.  */
  if (! NILP (default_directory) && !EQ (default_directory, name)
      /* Save time in some common cases - as long as default_directory
	 is not relative, it can be canonicalized with name below (if it
	 is needed at all) without requiring it to be expanded now.  */
#ifdef WIN32_FILENAMES
      /* Detect Windows file names with drive specifiers.  */
      && ! (IS_DRIVE (o[0]) && (IS_DEVICE_SEP (o[1]) && IS_DIRECTORY_SEP (o[2])))
      /* Detect Windows file names in UNC format.  */
      && ! (IS_DIRECTORY_SEP (o[0]) && IS_DIRECTORY_SEP (o[1]))
#endif /* not WIN32_FILENAMES */
#ifndef WIN32_NATIVE
      /* Detect Unix absolute file names (/... alone is not absolute on
	 Windows).  */
      && ! (IS_DIRECTORY_SEP (o[0]))
#endif /* not WIN32_NATIVE */
      )

    default_directory = Fexpand_file_name (default_directory, Qnil);

#ifdef FILE_SYSTEM_CASE
  name = FILE_SYSTEM_CASE (name);
#endif

 /* #### dmoore - this is ugly, clean this up.  Looks like nm pointing
    into name should be safe during all of this, though. */
  nm = XSTRING_DATA (name);

#ifdef WIN32_FILENAMES
  /* We will force directory separators to be either all \ or /, so make
     a local copy to modify, even if there ends up being no change. */
  nm = qxestrcpy (alloca_ibytes (qxestrlen (nm) + 1), nm);

  /* Note if special escape prefix is present, but remove for now.  */
  if (nm[0] == '/' && nm[1] == ':')
    {
      is_escaped = 1;
      nm += 2;
    }

  /* Find and remove drive specifier if present; this makes nm absolute
     even if the rest of the name appears to be relative. */
  {
    Ibyte *colon = qxestrrchr (nm, ':');

    if (colon)
      {
      /* Only recognize colon as part of drive specifier if there is a
	 single alphabetic character preceding the colon (and if the
	 character before the drive letter, if present, is a directory
	 separator); this is to support the remote system syntax used by
	 ange-ftp, and the "po:username" syntax for POP mailboxes. */
    look_again:
      if (nm == colon)
	nm++;
      else if (IS_DRIVE (colon[-1])
	       && (colon == nm + 1 || IS_DIRECTORY_SEP (colon[-2])))
	{
	  drive = colon[-1];
	  nm = colon + 1;
	}
      else
	{
	  while (--colon >= nm)
	    if (colon[0] == ':')
	      goto look_again;
	}
      }
  }

  /* If we see "c://somedir", we want to strip the first slash after the
     colon when stripping the drive letter.  Otherwise, this expands to
     "//somedir".  */
  if (drive && IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    nm++;
#endif /* WIN32_FILENAMES */

#ifdef WIN32_FILENAMES
  /* Discard any previous drive specifier if nm is now in UNC format. */
  if (IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    {
      drive = 0;
    }
#endif

  /* If nm is absolute, look for /./ or /../ sequences; if none are
     found, we can probably return right away.  We will avoid allocating
     a new string if name is already fully expanded.  */
  if (
      IS_DIRECTORY_SEP (nm[0])
#ifdef WIN32_NATIVE
      && (drive || IS_DIRECTORY_SEP (nm[1])) && !is_escaped
#endif
      )
    {
      /* If it turns out that the filename we want to return is just a
	 suffix of FILENAME, we don't need to go through and edit
	 things; we just need to construct a new string using data
	 starting at the middle of FILENAME.  If we set lose to a
	 non-zero value, that means we've discovered that we can't do
	 that cool trick.  */
      int lose = 0;

      p = nm;
      while (*p)
	{
	  /* Since we know the name is absolute, we can assume that each
	     element starts with a "/".  */

	  /* "." and ".." are hairy.  */
	  if (IS_DIRECTORY_SEP (p[0])
	      && p[1] == '.'
	      && (IS_DIRECTORY_SEP (p[2])
		  || p[2] == 0
		  || (p[2] == '.' && (IS_DIRECTORY_SEP (p[3])
				      || p[3] == 0))))
	    lose = 1;
	  /* We want to replace multiple `/' in a row with a single
	     slash.  */
	  else if (p > nm
		   && IS_DIRECTORY_SEP (p[0])
		   && IS_DIRECTORY_SEP (p[1]))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
#ifdef WIN32_FILENAMES
	  if (drive || IS_DIRECTORY_SEP (nm[1]))
	    {
	      Ibyte *newnm;
	  
	      if (IS_DIRECTORY_SEP (nm[1]))
		{
		  newnm = mswindows_canonicalize_filename (nm);
		  if (qxestrcmp (newnm, XSTRING_DATA (name)) != 0)
		    name = build_istring (newnm);
		}
	      else
		{
		  /* drive must be set, so this is okay */
		  newnm = mswindows_canonicalize_filename (nm - 2);
		  if (qxestrcmp (newnm, XSTRING_DATA (name)) != 0)
		    {
		      name = build_istring (newnm);
		      XSTRING_DATA (name)[0] = DRIVE_LETTER (drive);
		      XSTRING_DATA (name)[1] = ':';
		    }
		}
	      xfree (newnm, Ibyte *);
	      RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name, name);
	    }
#endif /* WIN32_FILENAMES */
#ifndef WIN32_NATIVE
	  if (nm == XSTRING_DATA (name))
	    RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name, name);
	  RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name,
					 build_istring (nm));
#endif /* not WIN32_NATIVE */
	}
    }

  /* At this point, nm might or might not be an absolute file name.  We
     need to expand ~ or ~user if present, otherwise prefix nm with
     default_directory if nm is not absolute, and finally collapse /./
     and /foo/../ sequences.

     We set newdir to be the appropriate prefix if one is needed:
       - the relevant user directory if nm starts with ~ or ~user
       - the specified drive's working dir (DOS/NT only) if nm does not
         start with /
       - the value of default_directory.

     Note that these prefixes are not guaranteed to be absolute (except
     for the working dir of a drive).  Therefore, to ensure we always
     return an absolute name, if the final prefix is not absolute we
     append it to the current working directory.  */

  newdir = 0;

  if (nm[0] == '~')		/* prefix ~ */
    {
      if (IS_DIRECTORY_SEP (nm[1])
	  || nm[1] == 0)	/* ~ by itself */
	{
	  Ibyte *homedir = get_home_directory ();

	  if (!homedir)
	    newdir = (Ibyte *) "";
	  else
	    newdir = homedir;

	  nm++;
#ifdef WIN32_FILENAMES
	  collapse_newdir = 0;
#endif
	}
      else			/* ~user/filename */
	{
	  for (p = nm; *p && (!IS_DIRECTORY_SEP (*p)); p++)
	    DO_NOTHING;
	  o = alloca_ibytes (p - nm + 1);
	  memcpy (o, nm, p - nm);
	  o [p - nm] = 0;

	  /* #### While NT is single-user (for the moment) you still
	     can have multiple user profiles users defined, each with
	     its HOME.  So maybe possibly we should think about handling
	     ~user. --ben */
#ifndef WIN32_NATIVE
#ifdef CYGWIN
	  {
	    Ibyte *user;

	    if ((user = user_login_name (NULL)) != NULL)
	      {
		/* Does the user login name match the ~name? */
		if (qxestrcmp (user, o + 1) == 0)
		  {
		    newdir = get_home_directory ();
		    nm = p;
		  }
	      }
	  }
          if (!newdir)
            {
#endif /* CYGWIN */
	  /* Jamie reports that getpwnam() can get wedged by SIGIO/SIGALARM
	     occurring in it. (It can call select()). */
	  slow_down_interrupts ();
	  pw = (struct passwd *) qxe_getpwnam (o + 1);
	  speed_up_interrupts ();
	  if (pw)
	    {
	      newdir = (Ibyte *) pw->pw_dir;
	      nm = p;
	      /* FSF: if WIN32_NATIVE, collapse_newdir = 0;
		 not possible here. */
	    }
#ifdef CYGWIN
	    }
#endif
#endif /* not WIN32_NATIVE */

	  /* If we don't find a user of that name, leave the name
	     unchanged; don't move nm forward to p.  */
	}
    }

#ifdef WIN32_FILENAMES
  /* On DOS and Windows, nm is absolute if a drive name was specified;
     use the drive's current directory as the prefix if needed.  */
  if (!newdir && drive)
    {
#ifdef WIN32_NATIVE
      /* Get default directory if needed to make nm absolute. */
      if (!IS_DIRECTORY_SEP (nm[0]))
	{
	  Ibyte *newcwd = mswindows_getdcwd (toupper (drive) - 'A' + 1);
	  if (newcwd)
	    {
	      IBYTE_STRING_TO_ALLOCA (newcwd, newdir);
	      xfree (newcwd, Ibyte *);
	    }
	  else
	    newdir = NULL;
	}
#endif /* WIN32_NATIVE */
      if (!newdir)
	{
	  /* Either nm starts with /, or drive isn't mounted. */
	  newdir = alloca_ibytes (4);
	  newdir[0] = DRIVE_LETTER (drive);
	  newdir[1] = ':';
	  newdir[2] = '/';
	  newdir[3] = 0;
	}
    }
#endif /* WIN32_FILENAMES */

  /* Finally, if no prefix has been specified and nm is not absolute,
     then it must be expanded relative to default_directory. */

  if (1
#ifndef WIN32_NATIVE
      /* /... alone is not absolute on DOS and Windows. */
      && !IS_DIRECTORY_SEP (nm[0])
#endif
#ifdef WIN32_FILENAMES
      && !(IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
#endif
      && !newdir)
    {
      newdir = XSTRING_DATA (default_directory);
#ifdef WIN32_FILENAMES
      /* Note if special escape prefix is present, but remove for now.  */
      if (newdir[0] == '/' && newdir[1] == ':')
	{
	  is_escaped = 1;
	  newdir += 2;
	}
#endif
    }

#ifdef WIN32_FILENAMES
  if (newdir)
    {
      /* First ensure newdir is an absolute name. */
      if (
	  /* Detect Windows file names with drive specifiers.  */
	  ! (IS_DRIVE (newdir[0])
	     && IS_DEVICE_SEP (newdir[1]) && IS_DIRECTORY_SEP (newdir[2]))
	  /* Detect Windows file names in UNC format.  */
	  && ! (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
	  /* XEmacs: added these two lines: Detect drive spec by itself */
	  && ! (IS_DEVICE_SEP (newdir[1]) && newdir[2] == 0)
	  /* Detect unix format.  */
#ifndef WIN32_NATIVE
	  && ! (IS_DIRECTORY_SEP (newdir[0]))
#endif
	  )
	{
	  /* Effectively, let newdir be (expand-file-name newdir cwd).
	     Because of the admonition against calling expand-file-name
	     when we have pointers into lisp strings, we accomplish this
	     indirectly by prepending newdir to nm if necessary, and using
	     cwd (or the wd of newdir's drive) as the new newdir. */

	  if (IS_DRIVE (newdir[0]) && newdir[1] == ':')
	    {
	      drive = newdir[0];
	      newdir += 2;
	    }
	  if (!IS_DIRECTORY_SEP (nm[0]))
	    {
	      Ibyte *tmp = alloca_ibytes (qxestrlen (newdir) +
					  qxestrlen (nm) + 2);
	      file_name_as_directory (tmp, newdir);
	      qxestrcat (tmp, nm);
	      nm = tmp;
	    }
	  if (drive)
	    {
#ifdef WIN32_NATIVE
	      Ibyte *newcwd = mswindows_getdcwd (toupper (drive) - 'A' + 1);
	      if (newcwd)
		{
		  IBYTE_STRING_TO_ALLOCA (newcwd, newdir);
		  xfree (newcwd, Ibyte *);
		}
	      else
#endif
		IBYTE_STRING_TO_ALLOCA ((Ibyte *) "/", newdir);
	    }
	  else
	    IBYTE_STRING_TO_ALLOCA (get_initial_directory (0, 0), newdir);
	}

      /* Strip off drive name from prefix, if present. */
      if (IS_DRIVE (newdir[0]) && newdir[1] == ':')
	{
	  drive = newdir[0];
	  newdir += 2;
	}

      /* Keep only a prefix from newdir if nm starts with slash
         (//server/share for UNC, nothing otherwise).  */
      if (IS_DIRECTORY_SEP (nm[0]) 
#ifndef WIN32_NATIVE
	  && IS_DIRECTORY_SEP (nm[1])
#endif
	  && collapse_newdir)
	{
	  if (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
	    {
	      /* !!#### Use ei API */
	      newdir = qxestrcpy (alloca_ibytes (qxestrlen (newdir) + 1),
				  newdir);
	      p = newdir + 2;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      p++;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      *p = 0;
	    }
	  else
	    newdir = (Ibyte *) "";
	}
    }
#endif /* WIN32_FILENAMES */

  if (newdir)
    {
      /* Get rid of any slash at the end of newdir, unless newdir is
	 just / or // (an incomplete UNC name).  */
      length = qxestrlen (newdir);
      if (length > 1 && IS_DIRECTORY_SEP (newdir[length - 1])
#ifdef WIN32_FILENAMES
	  && !(length == 2 && IS_DIRECTORY_SEP (newdir[0]))
#endif
	  )
	{
	  Ibyte *temp = alloca_ibytes (length);
	  memcpy (temp, newdir, length - 1);
	  temp[length - 1] = 0;
	  newdir = temp;
	}
      tlen = length + 1;
    }
  else
    tlen = 0;

  /* Now concatenate the directory and name to new space in the stack frame */
  tlen += qxestrlen (nm) + 1;
#ifdef WIN32_FILENAMES
  /* Reserve space for drive specifier and escape prefix, since either
     or both may need to be inserted.  (The Microsoft x86 compiler
     produces incorrect code if the following two lines are combined.)  */
  target = alloca_ibytes (tlen + 4);
  target += 4;
#else  /* not WIN32_FILENAMES */
  target = alloca_ibytes (tlen);
#endif /* not WIN32_FILENAMES */
  *target = 0;

  if (newdir)
    {
      if (nm[0] == 0 || IS_DIRECTORY_SEP (nm[0]))
	{
#ifdef WIN32_FILENAMES
	  /* If newdir is effectively "C:/", then the drive letter will have
	     been stripped and newdir will be "/".  Concatenating with an
	     absolute directory in nm produces "//", which will then be
	     incorrectly treated as a network share.  Ignore newdir in
	     this case (keeping the drive letter).  */
	  if (!(drive && nm[0] && IS_DIRECTORY_SEP (newdir[0]) 
		&& newdir[1] == '\0'))
#endif
	    qxestrcpy (target, newdir);
	}
      else
	file_name_as_directory (target, newdir);
    }

  qxestrcat (target, nm);

  /* ASSERT (IS_DIRECTORY_SEP (target[0])) if not VMS */

  /* Now canonicalize by removing `//', `/.' and `/foo/..' if they
     appear.  */

  p = target;
  o = target;

  while (*p)
    {
      if (!IS_DIRECTORY_SEP (*p))
	{
	  *o++ = *p++;
	}
      else if (IS_DIRECTORY_SEP (p[0])
	       && p[1] == '.'
	       && (IS_DIRECTORY_SEP (p[2])
		   || p[2] == 0))
	{
	  /* If "/." is the entire filename, keep the "/".  Otherwise,
	     just delete the whole "/.".  */
	  if (o == target && p[2] == '\0')
	    *o++ = *p;
	  p += 2;
	}
      else if (IS_DIRECTORY_SEP (p[0]) && p[1] == '.' && p[2] == '.'
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (IS_DIRECTORY_SEP (p[3]) || p[3] == 0))
	{
	  while (o != target && (--o) && !IS_DIRECTORY_SEP (*o))
	    ;
	  /* Keep initial / only if this is the whole name.  */
	  if (o == target && IS_ANY_SEP (*o) && p[3] == 0)
	    ++o;
	  p += 3;
	}
      else if (p > target
	       && IS_DIRECTORY_SEP (p[0]) && IS_DIRECTORY_SEP (p[1]))
	{
	  /* Collapse multiple `/' in a row.  */
	  *o++ = *p++;
	  while (IS_DIRECTORY_SEP (*p))
	    ++p;
	}
      else
	{
	  *o++ = *p++;
	}
    }

#ifdef WIN32_FILENAMES
  /* At last, set drive name, except for network file name.  */
  if (drive)
    {
      target -= 2;
      target[0] = DRIVE_LETTER (drive);
      target[1] = ':';
    }
#ifdef WIN32_NATIVE
  else
    {
      assert (IS_DIRECTORY_SEP (target[0]) && IS_DIRECTORY_SEP (target[1]));
    }
#endif
  /* Reinsert the escape prefix if required.  */
  if (is_escaped)
    {
      target -= 2;
      target[0] = '/';
      target[1] = ':';
    }

  *o = '\0';

  {
    Ibyte *newtarget = mswindows_canonicalize_filename (target);
    Lisp_Object result = build_istring (newtarget);
    xfree (newtarget, Ibyte *);

    RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name, result);
  }
#else /* not WIN32_FILENAMES */
  RETURN_UNGCPRO_EXIT_PROFILING (QSin_expand_file_name,
				 make_string (target, o - target));
#endif /* not WIN32_FILENAMES */
}

DEFUN ("file-truename", Ffile_truename, 1, 2, 0, /*
Return the canonical name of FILENAME.
Second arg DEFAULT is directory to start with if FILENAME is relative
 (does not start with slash); if DEFAULT is nil or missing,
 the current buffer's value of `default-directory' is used.
No component of the resulting pathname will be a symbolic link, as
 in the realpath() function.
*/
       (filename, default_))
{
  /* This function can GC.  GC checked 2000-07-28 ben. */
  Lisp_Object expanded_name;
  struct gcpro gcpro1;

  CHECK_STRING (filename);

  expanded_name = Fexpand_file_name (filename, default_);

  if (!STRINGP (expanded_name))
    return Qnil;

  GCPRO1 (expanded_name);

  {
    Lisp_Object handler =
      Ffind_file_name_handler (expanded_name, Qfile_truename);

    if (!NILP (handler))
      RETURN_UNGCPRO
	(call2_check_string (handler, Qfile_truename, expanded_name));
  }

  {
    Ibyte resolved_path[PATH_MAX_INTERNAL];
    Bytecount elen = XSTRING_LENGTH (expanded_name);
    Ibyte *path;
    Ibyte *p;

    LISP_STRING_TO_ALLOCA (expanded_name, path);

#if defined (WIN32_FILENAMES) && defined (CYGWIN)
    /* When using win32 filenames in cygwin we want file-truename to
       detect that c:/windows == /windows for example. */
    if (! (IS_DIRECTORY_SEP (path[0]) && IS_DIRECTORY_SEP (path[1])))
      {
	LOCAL_FILE_FORMAT_TO_INTERNAL_MSWIN (path, p);
	path = p;
      }
#endif
    p = path;

    /* Try doing it all at once. */
    if (!qxe_realpath (path, resolved_path, 0))
      {
	/* Didn't resolve it -- have to do it one component at a time. */
	/* "realpath" is a typically useless, stupid un*x piece of crap.
	   It claims to return a useful value in the "error" case, but since
	   there is no indication provided of how far along the pathname
	   the function went before erring, there is no way to use the
	   partial result returned.  What a piece of junk.

	   The above comment refers to historical versions of
	   realpath().  The Unix98 specs state:

	   "On successful completion, realpath() returns a
	   pointer to the resolved name. Otherwise, realpath()
	   returns a null pointer and sets errno to indicate the
	   error, and the contents of the buffer pointed to by
	   resolved_name are undefined."

	   Since we depend on undocumented semantics of various system
	   realpath()s, we just use our own version in realpath.c.

	   Note also that our own version differs in its semantics from any
	   standard version, since it accepts and returns internal-format
	   text, not external-format. */
	for (;;)
	  {
	    Ibyte *pos;

#ifdef WIN32_FILENAMES
	    if (IS_DRIVE (p[0]) && IS_DEVICE_SEP (p[1]) 
		&& IS_DIRECTORY_SEP (p[2]))
	      /* don't test c: on windows */
	      p = p+2;
	    else if (IS_DIRECTORY_SEP (p[0]) && IS_DIRECTORY_SEP (p[1]))
	      /* start after // */
	      p = p+1;
#endif
	    for (pos = p + 1; pos < path + elen; pos++)
	      if (IS_DIRECTORY_SEP (*pos))
		{
		  *(p = pos) = 0;
		  break;
		}
	    if (p != pos)
	      p = 0;

	    if (qxe_realpath (path, resolved_path, 0))
	      {
		if (p)
		  *p = DIRECTORY_SEP;
		else
		  break;

	      }
	    else if (errno == ENOENT || errno == EACCES)
	      {
		/* Failed on this component.  Just tack on the rest of
		   the string and we are done. */
		int rlen = qxestrlen (resolved_path);

		/* "On failure, it returns NULL, sets errno to indicate
		   the error, and places in resolved_path the absolute pathname
		   of the path component which could not be resolved." */

 		if (p)
		  {
		    int plen = elen - (p - path);

		    if (rlen > 1 && IS_DIRECTORY_SEP (resolved_path[rlen - 1]))
		      rlen = rlen - 1;

		    if (plen + rlen + 1 > countof (resolved_path))
		      goto toolong;

		    resolved_path[rlen] = DIRECTORY_SEP;
		    memcpy (resolved_path + rlen + 1, p + 1, plen + 1 - 1);
		  }
		break;
	      }
	    else
	      goto lose;
	  }
      }

    {
      Lisp_Object resolved_name;
      int rlen = qxestrlen (resolved_path);
      if (elen > 0 && IS_DIRECTORY_SEP (string_byte (expanded_name, elen - 1))
          && !(rlen > 0 && IS_DIRECTORY_SEP (resolved_path[rlen - 1])))
	{
	  if (rlen + 1 > countof (resolved_path))
	    goto toolong;
	  resolved_path[rlen++] = DIRECTORY_SEP;
	  resolved_path[rlen] = '\0';
	}
      resolved_name = make_string (resolved_path, rlen);
      RETURN_UNGCPRO (resolved_name);
    }

  toolong:
    errno = ENAMETOOLONG;
    goto lose;
  lose:
    report_file_error ("Finding truename", expanded_name);
  }
  RETURN_UNGCPRO (Qnil);
}


DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name, 1, 1, 0, /*
Substitute environment variables referred to in FILENAME.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character, not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.
If `/~' appears, all of FILENAME through that `/' is discarded.
*/
       (filename))
{
  /* This function can GC.  GC checked 2000-07-28 ben. */
  Ibyte *nm;

  Ibyte *s, *p, *o, *x, *endp, *got;
  Ibyte *target = 0;
  int total = 0;
  int substituted = 0, seen_braces;
  Ibyte *xnm;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qsubstitute_in_file_name);
  if (!NILP (handler))
    return call2_check_string_or_nil (handler, Qsubstitute_in_file_name,
				      filename);

  nm = XSTRING_DATA (filename);
  endp = nm + XSTRING_LENGTH (filename);

  /* If /~ or // appears, discard everything through first slash. */

  for (p = nm; p != endp; p++)
    {
      if ((p[0] == '~'
#if defined (WIN32_FILENAMES)
	   /* // at start of file name is meaningful in WindowsNT systems */
	   || (IS_DIRECTORY_SEP (p[0]) && p - 1 != nm)
#else /* not (WIN32_FILENAMES) */
	   || IS_DIRECTORY_SEP (p[0])
#endif /* not (WIN32_FILENAMES) */
	   )
	  && p != nm
	  && (IS_DIRECTORY_SEP (p[-1])))
	{
	  nm = p;
	  substituted = 1;
	}
#ifdef WIN32_FILENAMES
      /* see comment in expand-file-name about drive specifiers */
      else if (IS_DRIVE (p[0]) && p[1] == ':'
	       && p > nm && IS_DIRECTORY_SEP (p[-1]))
	{
	  nm = p;
	  substituted = 1;
	}
#endif /* WIN32_FILENAMES */
    }

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p != '$')
      p++;
    else
      {
	p++;
	if (p == endp)
	  {
	    /* No substitution, no error. */
	    break;
	  }
	else if (*p == '$')
	  {
	    /* "$$" means a single "$" */
	    p++;
	    total -= 1;
	    substituted = 1;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}')
	      {
		/* No substitution, no error. Keep looking. */
		p = o;
		continue;
	      }
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = alloca_ibytes (s - o + 1);
	qxestrncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef WIN32_NATIVE
	qxestrupr (target); /* $home == $HOME etc.  */
#endif /* WIN32_NATIVE */

	/* Get variable value */
	got = egetenv ((CIbyte *) target);
	if (got)
	  {
	    total += qxestrlen (got);
	    substituted = 1;
	  }
      }

  if (!substituted)
    return filename;

  /* If substitution required, recopy the filename and do it */
  /* Make space in stack frame for the new copy */
  xnm = alloca_ibytes (XSTRING_LENGTH (filename) + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	seen_braces = 0;
	if (p == endp)
	  {
	    *x++ = '$';
	    break;
	  }
	else if (*p == '$')
	  {
	    *x++ = *p++;
	    continue;
	  }
	else if (*p == '{')
	  {
	    seen_braces = 1;
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}')
	      {
		/* Don't syntax error, don't substitute */
		*x++ = '{';
		p = o;
		continue;
	      }
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = alloca_ibytes (s - o + 1);
	qxestrncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef WIN32_NATIVE
	qxestrupr (target); /* $home == $HOME etc.  */
#endif /* WIN32_NATIVE */

	/* Get variable value */
	got = egetenv ((CIbyte *) target);
	if (got)
	  {
	    qxestrcpy (x, got);
	    x += qxestrlen (got);
	  }
	else
	  {
	    *x++ = '$';
	    if (seen_braces)
	      {
		*x++ = '{';
                /* Preserve the original case. */
		qxestrncpy (x, o, s - o);
		x += s - o;
		*x++ = '}';
	      }
	    else
	      {
                /* Preserve the original case. */
		qxestrncpy (x, o, s - o);
		x += s - o;
	      }
	  }
      }

  *x = 0;

  /* If /~ or // appears, discard everything through first slash. */

  for (p = xnm; p != x; p++)
    if ((p[0] == '~'
#if defined (WIN32_FILENAMES)
	 || (IS_DIRECTORY_SEP (p[0]) && p - 1 != xnm)
#else /* not WIN32_FILENAMES */
	 || IS_DIRECTORY_SEP (p[0])
#endif /* not WIN32_FILENAMES */
	 )
	/* don't do p[-1] if that would go off the beginning --jwz */
	&& p != nm && p > xnm && IS_DIRECTORY_SEP (p[-1]))
      xnm = p;
#ifdef WIN32_FILENAMES
    else if (IS_DRIVE (p[0]) && p[1] == ':'
	     && p > nm && IS_DIRECTORY_SEP (p[-1]))
	xnm = p;
#endif

  return make_string (xnm, x - xnm);
}

/* A slightly faster and more convenient way to get
   (directory-file-name (expand-file-name FOO)).  */

Lisp_Object
expand_and_dir_to_file (Lisp_Object filename, Lisp_Object defdir)
{
  /* This function can call Lisp.  GC checked 2000-07-28 ben */
  Lisp_Object abspath;
  struct gcpro gcpro1;

  abspath = Fexpand_file_name (filename, defdir);
  GCPRO1 (abspath);
  /* Remove final slash, if any (unless path is root).
     stat behaves differently depending!  */
  if (XSTRING_LENGTH (abspath) > 1
      && IS_DIRECTORY_SEP (string_byte (abspath, XSTRING_LENGTH (abspath) - 1))
      && !IS_DEVICE_SEP (string_byte (abspath, XSTRING_LENGTH (abspath) - 2)))
    /* We cannot take shortcuts; they might be wrong for magic file names.  */
    abspath = Fdirectory_file_name (abspath);
  UNGCPRO;
  return abspath;
}

/* Signal an error if the file ABSNAME already exists.
   If INTERACTIVE is nonzero, ask the user whether to proceed,
   and bypass the error if the user says to go ahead.
   QUERYSTRING is a name for the action that is being considered
   to alter the file.
   *STATPTR is used to store the stat information if the file exists.
   If the file does not exist, STATPTR->st_mode is set to 0.  */

static void
barf_or_query_if_file_exists (Lisp_Object absname, const Ascbyte *querystring,
			      int interactive, struct stat *statptr)
{
  /* This function can call Lisp.  GC checked 2000-07-28 ben */
  struct stat statbuf;

  /* stat is a good way to tell whether the file exists,
     regardless of what access permissions it has.  */
  if (qxe_stat (XSTRING_DATA (absname), &statbuf) >= 0)
    {
      Lisp_Object tem;

      if (interactive)
	{
	  Lisp_Object prompt;
	  struct gcpro gcpro1;

	  prompt =
	    emacs_sprintf_string
	      (GETTEXT ("File %s already exists; %s anyway? "),
	       XSTRING_DATA (absname), GETTEXT (querystring));

	  GCPRO1 (prompt);
	  tem = call1 (Qyes_or_no_p, prompt);
	  UNGCPRO;
	}
      else
        tem = Qnil;

      if (NILP (tem))
	Fsignal (Qfile_already_exists,
		 list2 (build_msg_string ("File already exists"),
			absname));
      if (statptr)
	*statptr = statbuf;
    }
  else
    {
      if (statptr)
	statptr->st_mode = 0;
    }
  return;
}

DEFUN ("copy-file", Fcopy_file, 2, 4,
       "fCopy file: \nFCopy %s to file: \np\nP", /*
Copy FILENAME to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil.
*/
       (filename, newname, ok_if_already_exists, keep_time))
{
  /* This function can call Lisp.  GC checked 2000-07-28 ben */
  int ifd, ofd, n;
  char buf[16 * 1024];
  struct stat st, out_st;
  Lisp_Object handler;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2;
  /* Lisp_Object args[6]; */
  int input_file_statable_p;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename);
  CHECK_STRING (newname);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the input file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qcopy_file);
  /* Likewise for output file name.  */
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qcopy_file);
  if (!NILP (handler))
  {
    UNGCPRO;
    return call5 (handler, Qcopy_file, filename, newname,
		  ok_if_already_exists, keep_time);
  }

  /* When second argument is a directory, copy the file into it.
     (copy-file "foo" "bar/") == (copy-file "foo" "bar/foo")
   */
  if (!NILP (Ffile_directory_p (newname)))
    {
      Lisp_Object args[3];
      struct gcpro ngcpro1;
      int i = 1;

      args[0] = newname;
      args[1] = Qnil; args[2] = Qnil;
      NGCPRO1 (*args);
      ngcpro1.nvars = 3;
      if (!IS_DIRECTORY_SEP (string_byte (newname,
					   XSTRING_LENGTH (newname) - 1)))

	args[i++] = Fchar_to_string (Vdirectory_sep_char);
      args[i++] = Ffile_name_nondirectory (filename);
      newname = Fconcat (i, args);
      NUNGCPRO;
    }

  if (NILP (ok_if_already_exists)
      || INTP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "copy to it",
				  INTP (ok_if_already_exists), &out_st);
  else if (qxe_stat (XSTRING_DATA (newname), &out_st) < 0)
    out_st.st_mode = 0;

  ifd = qxe_interruptible_open (XSTRING_DATA (filename),
				O_RDONLY | OPEN_BINARY, 0);
  if (ifd < 0)
    report_file_error ("Opening input file", filename);

  record_unwind_protect (close_file_unwind, make_int (ifd));

  /* We can only copy regular files and symbolic links.  Other files are not
     copyable by us. */
  input_file_statable_p = (qxe_fstat (ifd, &st) >= 0);

#ifndef WIN32_NATIVE
  if (out_st.st_mode != 0
      && st.st_dev == out_st.st_dev && st.st_ino == out_st.st_ino)
    {
      errno = 0;
      report_file_error ("Input and output files are the same",
			 list3 (Qunbound, filename, newname));
    }
#endif

#if defined (S_ISREG) && defined (S_ISLNK)
  if (input_file_statable_p)
    {
      if (!(S_ISREG (st.st_mode))
	  /* XEmacs: have to allow S_ISCHR in order to copy /dev/null */
#ifdef S_ISCHR
	  && !(S_ISCHR (st.st_mode))
#endif
	  && !(S_ISLNK (st.st_mode)))
	{
#if defined (EISDIR)
	  /* Get a better looking error message. */
	  errno = EISDIR;
#endif /* EISDIR */
	report_file_error ("Non-regular file", filename);
	}
    }
#endif /* S_ISREG && S_ISLNK */

  ofd = qxe_open (XSTRING_DATA (newname),
		  O_WRONLY | O_CREAT | O_TRUNC | OPEN_BINARY, CREAT_MODE);
  if (ofd < 0)
    report_file_error ("Opening output file", newname);

  {
    Lisp_Object ofd_locative = noseeum_cons (make_int (ofd), Qnil);

    record_unwind_protect (close_file_unwind, ofd_locative);

    while ((n = read_allowing_quit (ifd, buf, sizeof (buf))) > 0)
    {
      if (write_allowing_quit (ofd, buf, n) != n)
	report_file_error ("I/O error", newname);
    }

    /* Closing the output clobbers the file times on some systems.  */
    if (retry_close (ofd) < 0)
      report_file_error ("I/O error", newname);

    if (input_file_statable_p)
      {
	if (!NILP (keep_time))
	  {
	    EMACS_TIME atime, mtime;
	    EMACS_SET_SECS_USECS (atime, st.st_atime, 0);
	    EMACS_SET_SECS_USECS (mtime, st.st_mtime, 0);
	    if (set_file_times (newname, atime, mtime))
	      report_file_error ("I/O error", list1 (newname));
	  }
	qxe_chmod (XSTRING_DATA (newname), st.st_mode & 07777);
      }

    /* We'll close it by hand */
    XCAR (ofd_locative) = Qnil;

    /* Close ifd */
    unbind_to (speccount);
  }

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-directory-internal", Fmake_directory_internal, 1, 1, 0, /*
Create a directory.  One argument, a file name string.
*/
       (dirname_))
{
  /* This function can GC.  GC checked 1997.04.06. */
  Lisp_Object handler;
  struct gcpro gcpro1;
  DECLARE_EISTRING (dir);

  CHECK_STRING (dirname_);
  dirname_ = Fexpand_file_name (dirname_, Qnil);

  GCPRO1 (dirname_);
  handler = Ffind_file_name_handler (dirname_, Qmake_directory_internal);
  UNGCPRO;
  if (!NILP (handler))
    return (call2 (handler, Qmake_directory_internal, dirname_));

  eicpy_lstr (dir, dirname_);
  if (eigetch_char (dir, eicharlen (dir) - 1) == '/')
    eidel (dir, eilen (dir) - 1, -1, 1, -1);

  if (qxe_mkdir (eidata (dir), 0777) != 0)
    report_file_error ("Creating directory", dirname_);

  return Qnil;
}

DEFUN ("delete-directory", Fdelete_directory, 1, 1, "FDelete directory: ", /*
Delete a directory.  One argument, a file name or directory name string.
*/
       (dirname_))
{
  /* This function can GC.  GC checked 1997.04.06. */
  Lisp_Object handler;
  struct gcpro gcpro1;

  CHECK_STRING (dirname_);

  GCPRO1 (dirname_);
  dirname_ = Fexpand_file_name (dirname_, Qnil);
  dirname_ = Fdirectory_file_name (dirname_);

  handler = Ffind_file_name_handler (dirname_, Qdelete_directory);
  UNGCPRO;
  if (!NILP (handler))
    return (call2 (handler, Qdelete_directory, dirname_));

  if (qxe_rmdir (XSTRING_DATA (dirname_)) != 0)
    report_file_error ("Removing directory", dirname_);

  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, 1, 1, "fDelete file: ", /*
Delete the file named FILENAME (a string).
If FILENAME has multiple names, it continues to exist with the other names.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.06. */
  Lisp_Object handler;
  struct gcpro gcpro1;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  GCPRO1 (filename);
  handler = Ffind_file_name_handler (filename, Qdelete_file);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qdelete_file, filename);

  if (0 > qxe_unlink (XSTRING_DATA (filename)))
    report_file_error ("Removing old name", filename);
  return Qnil;
}

static Lisp_Object
internal_delete_file_1 (Lisp_Object UNUSED (ignore),
			Lisp_Object UNUSED (ignore2))
{
  return Qt;
}

/* Delete file FILENAME, returning 1 if successful and 0 if failed.  */

int
internal_delete_file (Lisp_Object filename)
{
  /* This function can GC.  GC checked 1997.04.06. */
  return NILP (condition_case_1 (Qt, Fdelete_file, filename,
				 internal_delete_file_1, Qnil));
}

DEFUN ("rename-file", Frename_file, 2, 3,
       "fRename file: \nFRename %s to file: \np", /*
Rename FILENAME as NEWNAME.  Both args must be strings.
If file has names other than FILENAME, it continues to have those names.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
*/
       (filename, newname, ok_if_already_exists))
{
  /* This function can GC.  GC checked 1997.04.06. */
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename);
  CHECK_STRING (newname);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qrename_file);
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qrename_file);
  if (!NILP (handler))
  {
    UNGCPRO;
    return call4 (handler, Qrename_file,
		  filename, newname, ok_if_already_exists);
  }

  /* When second argument is a directory, rename the file into it.
     (rename-file "foo" "bar/") == (rename-file "foo" "bar/foo")
   */
  if (!NILP (Ffile_directory_p (newname)))
    {
      Lisp_Object args[3];
      struct gcpro ngcpro1;
      int i = 1;

      args[0] = newname;
      args[1] = Qnil; args[2] = Qnil;
      NGCPRO1 (*args);
      ngcpro1.nvars = 3;
      if (string_byte (newname, XSTRING_LENGTH (newname) - 1) != '/')
	args[i++] = build_ascstring ("/");
      args[i++] = Ffile_name_nondirectory (filename);
      newname = Fconcat (i, args);
      NUNGCPRO;
    }

  if (NILP (ok_if_already_exists)
      || INTP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "rename to it",
				  INTP (ok_if_already_exists), 0);

  /* We have configure check for rename() and emulate using
     link()/unlink() if necessary. */
  if (0 > qxe_rename (XSTRING_DATA (filename), XSTRING_DATA (newname)))
    {
      if (errno == EXDEV)
	{
	  Fcopy_file (filename, newname,
		      /* We have already prompted if it was an integer,
			 so don't have copy-file prompt again.  */
		      (NILP (ok_if_already_exists) ? Qnil : Qt),
                      Qt);
	  Fdelete_file (filename);
	}
      else
	{
	  report_file_error ("Renaming", list3 (Qunbound, filename, newname));
	}
    }
  UNGCPRO;
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, 2, 3,
       "fAdd name to file: \nFName to add to %s: \np", /*
Give FILENAME additional name NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
*/
       (filename, newname, ok_if_already_exists))
{
  /* This function can GC.  GC checked 1997.04.06. */
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename);
  CHECK_STRING (newname);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, filename,
			   newname, ok_if_already_exists));

  /* If the new name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (newname, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, filename,
			   newname, ok_if_already_exists));

  if (NILP (ok_if_already_exists)
      || INTP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "make it a new name",
				  INTP (ok_if_already_exists), 0);
  /* #### Emacs 20.6 contains an implementation of link() in w32.c.
     Need to port. */
#ifndef HAVE_LINK
  signal_error_2 (Qunimplemented, "Adding new name", filename, newname);
#else /* HAVE_LINK */
  qxe_unlink (XSTRING_DATA (newname));
  if (0 > qxe_link (XSTRING_DATA (filename), XSTRING_DATA (newname)))
    {
      report_file_error ("Adding new name",
			 list3 (Qunbound, filename, newname));
    }
#endif /* HAVE_LINK */

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-symbolic-link", Fmake_symbolic_link, 2, 3,
       "FMake symbolic link to file: \nFMake symbolic link to file %s: \np", /*
Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
This happens for interactive use with M-x.

On platforms where symbolic links are not available, any file handlers will
be run, but the check for the existence of LINKNAME will not be done, and
the symbolic link will not be created.
*/
       (filename, linkname, ok_if_already_exists))
{
  /* This function can GC.  GC checked 1997.06.04. */
  /* XEmacs change: run handlers even if local machine doesn't have symlinks */
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, linkname);
  CHECK_STRING (filename);
  CHECK_STRING (linkname);
  /* If the link target has a ~, we must expand it to get
     a truly valid file name.  Otherwise, do not expand;
     we want to permit links to relative file names.  */
  if (string_byte (filename, 0) == '~')
    filename = Fexpand_file_name (filename, Qnil);
  linkname = Fexpand_file_name (linkname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename, linkname,
			   ok_if_already_exists));

  /* If the new link name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (linkname, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename,
			   linkname, ok_if_already_exists));

#ifdef HAVE_SYMLINK
  if (NILP (ok_if_already_exists)
      || INTP (ok_if_already_exists))
    barf_or_query_if_file_exists (linkname, "make it a link",
				  INTP (ok_if_already_exists), 0);

  qxe_unlink (XSTRING_DATA (linkname));
  if (0 > qxe_symlink (XSTRING_DATA (filename),
		       XSTRING_DATA (linkname)))
    {
      report_file_error ("Making symbolic link",
			 list3 (Qunbound, filename, linkname));
    }
#endif

  UNGCPRO;
  return Qnil;
}

#ifdef HPUX_NET

DEFUN ("sysnetunam", Fsysnetunam, 2, 2, 0, /*
Open a network connection to PATH using LOGIN as the login string.
*/
       (path, login))
{
  int netresult;
  const Extbyte *path_ext;
  const Extbyte *login_ext;

  CHECK_STRING (path);
  CHECK_STRING (login);

  /* netunam, being a strange-o system call only used once, is not
     encapsulated. */

  LISP_STRING_TO_EXTERNAL (path, path_ext, Qfile_name);
  LISP_STRING_TO_EXTERNAL (login, login_ext, Quser_name_encoding);

  netresult = netunam (path_ext, login_ext);

  return netresult == -1 ? Qnil : Qt;
}
#endif /* HPUX_NET */

DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, 1, 1, 0, /*
Return t if file FILENAME specifies an absolute path name.
On Unix, this is a name starting with a `/' or a `~'.
*/
       (filename))
{
  /* This function does not GC */
  Ibyte *ptr;

  CHECK_STRING (filename);
  ptr = XSTRING_DATA (filename);
  return (IS_DIRECTORY_SEP (*ptr) || *ptr == '~'
#ifdef WIN32_FILENAMES
	  || (IS_DRIVE (*ptr) && ptr[1] == ':' && IS_DIRECTORY_SEP (ptr[2]))
#endif
	  ) ? Qt : Qnil;
}

/* Return nonzero if file FILENAME exists and can be executed.  */

static int
check_executable (Lisp_Object filename)
{
#ifdef WIN32_NATIVE
  struct stat st;
  if (qxe_stat (XSTRING_DATA (filename), &st) < 0)
    return 0;
  return ((st.st_mode & S_IEXEC) != 0);
#else /* not WIN32_NATIVE */
#ifdef HAVE_EACCESS
  return qxe_eaccess (XSTRING_DATA (filename), X_OK) >= 0;
#else
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.  */
  return qxe_access (XSTRING_DATA (filename), X_OK) >= 0;
#endif /* HAVE_EACCESS */
#endif /* not WIN32_NATIVE */
}

/* Return nonzero if file FILENAME exists and can be written.  */

static int
check_writable (const Ibyte *filename)
{
#ifdef WIN32_ANY
  // Since this has to work for a directory, we can't just call 'CreateFile'
  PSECURITY_DESCRIPTOR pDesc; /* Must be freed with LocalFree */
  /* these need not be freed, they point into pDesc */
  PSID psidOwner;
  PSID psidGroup;
  PACL pDacl;
  PACL pSacl;
  /* end of insides of descriptor */
  DWORD error;
  DWORD attributes;
  HANDLE tokenHandle;
  GENERIC_MAPPING genericMapping;
  DWORD accessMask;
  PRIVILEGE_SET PrivilegeSet;
  DWORD dwPrivSetSize = sizeof( PRIVILEGE_SET );
  BOOL fAccessGranted = FALSE;
  DWORD dwAccessAllowed;
  Extbyte *fnameext;

  LOCAL_FILE_FORMAT_TO_TSTR (filename, fnameext);

  // First check for a normal file with the old-style readonly bit
  attributes = qxeGetFileAttributes(fnameext);
  if (FILE_ATTRIBUTE_READONLY == (attributes & (FILE_ATTRIBUTE_DIRECTORY|FILE_ATTRIBUTE_READONLY)))
    return 0;

  /* Win32 prototype lacks const. */
  error = qxeGetNamedSecurityInfo(fnameext, SE_FILE_OBJECT, 
				  DACL_SECURITY_INFORMATION|GROUP_SECURITY_INFORMATION|OWNER_SECURITY_INFORMATION,
				  &psidOwner, &psidGroup, &pDacl, &pSacl, &pDesc);
  if(error != ERROR_SUCCESS) { // FAT?
    attributes = qxeGetFileAttributes(fnameext);
    return (attributes & FILE_ATTRIBUTE_DIRECTORY) || (0 == (attributes & FILE_ATTRIBUTE_READONLY));
  }

  genericMapping.GenericRead = FILE_GENERIC_READ;
  genericMapping.GenericWrite = FILE_GENERIC_WRITE;
  genericMapping.GenericExecute = FILE_GENERIC_EXECUTE;
  genericMapping.GenericAll = FILE_ALL_ACCESS;

  if(!ImpersonateSelf(SecurityDelegation)) {
    return 0;
  }
  if(!OpenThreadToken(GetCurrentThread(), TOKEN_ALL_ACCESS, TRUE, &tokenHandle)) {
    return 0;
  }

  accessMask = GENERIC_WRITE;
  MapGenericMask(&accessMask, &genericMapping);

  if(!AccessCheck(pDesc, tokenHandle, accessMask, &genericMapping,
		  &PrivilegeSet,       // receives privileges used in check
		  &dwPrivSetSize,      // size of PrivilegeSet buffer
		  &dwAccessAllowed,    // receives mask of allowed access rights
		  &fAccessGranted)) 
    {
      CloseHandle(tokenHandle);
      RevertToSelf();
      LocalFree(pDesc);
      return 0;
    }
  CloseHandle(tokenHandle);
  RevertToSelf();
  LocalFree(pDesc);
  return fAccessGranted == TRUE;
#elif defined (HAVE_EACCESS)
  return (qxe_eaccess (filename, W_OK) >= 0);
#else
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.
     Opening with O_WRONLY could work for an ordinary file,
     but would lose for directories.  */
  return (qxe_access (filename, W_OK) >= 0);
#endif /* (not) defined (HAVE_EACCESS) */
}

DEFUN ("file-exists-p", Ffile_exists_p, 1, 1, 0, /*
Return t if file FILENAME exists.  (This does not mean you can read it.)
See also `file-readable-p' and `file-attributes'.
*/
       (filename))
{
  /* This function can call lisp; GC checked 2000-07-11 ben */
  Lisp_Object abspath;
  Lisp_Object handler;
  struct stat statbuf;
  struct gcpro gcpro1;

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_exists_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_exists_p, abspath);

  return qxe_stat (XSTRING_DATA (abspath), &statbuf) >= 0 ? Qt : Qnil;
}

DEFUN ("file-executable-p", Ffile_executable_p, 1, 1, 0, /*
Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.
*/
       (filename))

{
  /* This function can GC.  GC checked 07-11-2000 ben. */
  Lisp_Object abspath;
  Lisp_Object handler;
  struct gcpro gcpro1;

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_executable_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_executable_p, abspath);

  return check_executable (abspath) ? Qt : Qnil;
}

DEFUN ("file-readable-p", Ffile_readable_p, 1, 1, 0, /*
Return t if file FILENAME exists and you can read it.
See also `file-exists-p' and `file-attributes'.
*/
       (filename))
{
  /* This function can GC */
  Lisp_Object abspath = Qnil;
  Lisp_Object handler;
  struct gcpro gcpro1;
  GCPRO1 (abspath);

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath, Qfile_readable_p);
  if (!NILP (handler))
    RETURN_UNGCPRO (call2 (handler, Qfile_readable_p, abspath));

#if defined (WIN32_FILENAMES)
  /* Under MS-DOS and Windows, open does not work for directories.  */
  UNGCPRO;
  if (qxe_access (XSTRING_DATA (abspath), 0) == 0)
    return Qt;
  else
    return Qnil;
#else /* not WIN32_FILENAMES */
  {
    int desc = qxe_interruptible_open (XSTRING_DATA (abspath),
				       O_RDONLY | OPEN_BINARY, 0);
    UNGCPRO;
    if (desc < 0)
      return Qnil;
    retry_close (desc);
    return Qt;
  }
#endif /* not WIN32_FILENAMES */
}

/* Having this before file-symlink-p mysteriously caused it to be forgotten
   on the RT/PC.  */
DEFUN ("file-writable-p", Ffile_writable_p, 1, 1, 0, /*
Return t if file FILENAME can be written or created by you.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath, dir;
  Lisp_Object handler;
  struct stat statbuf;
  struct gcpro gcpro1;

  CHECK_STRING (filename);
  abspath = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_writable_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_writable_p, abspath);

  if (qxe_stat (XSTRING_DATA (abspath), &statbuf) >= 0)
    return (check_writable (XSTRING_DATA (abspath))
	    ? Qt : Qnil);


  GCPRO1 (abspath);
  dir = Ffile_name_directory (abspath);
  UNGCPRO;
  return (check_writable (!NILP (dir) ? XSTRING_DATA (dir) : (Ibyte *) "")
	  ? Qt : Qnil);
}

DEFUN ("file-symlink-p", Ffile_symlink_p, 1, 1, 0, /*
Return non-nil if file FILENAME is the name of a symbolic link.
The value is the name of the file to which it is linked.
Otherwise returns nil.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  /* XEmacs change: run handlers even if local machine doesn't have symlinks */
#ifdef HAVE_READLINK
  Ibyte *buf;
  int bufsize;
  int valsize;
  Lisp_Object val;
#endif
  Lisp_Object handler;
  struct gcpro gcpro1;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (filename);
  handler = Ffind_file_name_handler (filename, Qfile_symlink_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_symlink_p, filename);

#ifdef HAVE_READLINK
  bufsize = 100;
  while (1)
    {
      buf = xnew_array_and_zero (Ibyte, bufsize);
      valsize = qxe_readlink (XSTRING_DATA (filename),
			      buf, bufsize);
      if (valsize < bufsize) break;
      /* Buffer was not long enough */
      xfree (buf, Ibyte *);
      bufsize *= 2;
    }
  if (valsize == -1)
    {
      xfree (buf, Ibyte *);
      return Qnil;
    }
  val = make_string (buf, valsize);
  xfree (buf, Ibyte *);
  return val;
#elif defined (WIN32_NATIVE)
  if (mswindows_shortcuts_are_symlinks)
    {
      /* We want to resolve the directory component and leave the rest
	 alone. */
      Ibyte *path = XSTRING_DATA (filename);
      Ibyte *dirend =
	find_end_of_directory_component (path, XSTRING_LENGTH (filename));
      Ibyte *fname;
      DECLARE_EISTRING (dir);
      
      if (dirend != path)
	{
	  Ibyte *resdir;
	  DECLARE_EISTRING (resname);
	  
	  eicpy_raw (dir, path, dirend - path);
	  PATHNAME_RESOLVE_LINKS (eidata (dir), resdir);
	  eicpy_rawz (resname, resdir);
	  eicat_rawz (resname, dirend);
	  path = eidata (resname);
	}
      
      fname = mswindows_read_link (path);
      if (!fname)
	return Qnil;
      {
	Lisp_Object val = build_istring (fname);
	xfree (fname, Ibyte *);
	return val;
      }
    }
  return Qnil;
#else
  return Qnil;
#endif
}

DEFUN ("file-directory-p", Ffile_directory_p, 1, 1, 0, /*
Return t if file FILENAME is the name of a directory as a file.
A directory name spec may be given instead; then the value is t
if the directory so specified exists and really is a directory.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath;
  struct stat st;
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (current_buffer->directory);
  abspath = expand_and_dir_to_file (filename,
				    current_buffer->directory);
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_directory_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_directory_p, abspath);

  if (qxe_stat (XSTRING_DATA (abspath), &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFDIR ? Qt : Qnil;
}

DEFUN ("file-accessible-directory-p", Ffile_accessible_directory_p, 1, 1, 0, /*
Return t if file FILENAME is the name of a directory as a file,
and files in that directory can be opened by you.  In order to use a
directory as a buffer's current directory, this predicate must return true.
A directory name spec may be given instead; then the value is t
if the directory so specified exists and really is a readable and
searchable directory.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_accessible_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_accessible_directory_p,
		  filename);

#if !defined (WIN32_NATIVE)
  if (NILP (Ffile_directory_p (filename)))
      return (Qnil);
  else
    return Ffile_executable_p (filename);
#else
  {
    int tem;
    struct gcpro gcpro1;
    /* It's an unlikely combination, but yes we really do need to gcpro:
       Suppose that file-accessible-directory-p has no handler, but
       file-directory-p does have a handler; this handler causes a GC which
       relocates the string in `filename'; and finally file-directory-p
       returns non-nil.  Then we would end up passing a garbaged string
       to file-executable-p.  */
    GCPRO1 (filename);
    tem = (NILP (Ffile_directory_p (filename))
	   || NILP (Ffile_executable_p (filename)));
    UNGCPRO;
    return tem ? Qnil : Qt;
  }
#endif /* !defined(WIN32_NATIVE) */
}

DEFUN ("file-regular-p", Ffile_regular_p, 1, 1, 0, /*
Return t if file FILENAME is the name of a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath;
  struct stat st;
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (current_buffer->directory);
  abspath = expand_and_dir_to_file (filename, current_buffer->directory);
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_regular_p);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_regular_p, abspath);

  if (qxe_stat (XSTRING_DATA (abspath), &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFREG ? Qt : Qnil;
}

DEFUN ("file-modes", Ffile_modes, 1, 1, 0, /*
Return mode bits of file named FILENAME, as an integer.
*/
       (filename))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath;
  struct stat st;
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (current_buffer->directory);
  abspath = expand_and_dir_to_file (filename,
				    current_buffer->directory);
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qfile_modes);
  UNGCPRO;
  if (!NILP (handler))
    return call2 (handler, Qfile_modes, abspath);

  if (qxe_stat (XSTRING_DATA (abspath), &st) < 0)
    return Qnil;
  /* Syncing with FSF 19.34.6 note: not in FSF, #if 0'ed out here. */
#if 0
#ifdef WIN32_NATIVE
  if (check_executable (abspath))
    st.st_mode |= S_IEXEC;
#endif /* WIN32_NATIVE */
#endif /* 0 */

  return make_int (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, 2, 2, 0, /*
Set mode bits of file named FILENAME to MODE (an integer).
Only the 12 low bits of MODE are used.
*/
       (filename, mode))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath;
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (current_buffer->directory);
  abspath = Fexpand_file_name (filename, current_buffer->directory);
  UNGCPRO;

  CHECK_INT (mode);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  GCPRO1 (abspath);
  handler = Ffind_file_name_handler (abspath, Qset_file_modes);
  UNGCPRO;
  if (!NILP (handler))
    return call3 (handler, Qset_file_modes, abspath, mode);

  if (qxe_chmod (XSTRING_DATA (abspath), XINT (mode)) < 0)
    report_file_error ("Doing chmod", abspath);

  return Qnil;
}

DEFUN ("set-default-file-modes", Fset_default_file_modes, 1, 1, 0, /*
Set the file permission bits for newly created files.
The argument MODE should be an integer; if a bit in MODE is 1,
subsequently created files will not have the permission corresponding
to that bit enabled.  Only the low 9 bits are used.
This setting is inherited by subprocesses.
*/
       (mode))
{
  CHECK_INT (mode);

  umask ((~ XINT (mode)) & 0777);

  return Qnil;
}

DEFUN ("default-file-modes", Fdefault_file_modes, 0, 0, 0, /*
Return the default file protection for created files.
The umask value determines which permissions are enabled in newly
created files.  If a permission's bit in the umask is 1, subsequently
created files will not have that permission enabled.
*/
       ())
{
  int mode;

  mode = umask (0);
  umask (mode);

  return make_int ((~ mode) & 0777);
}

DEFUN ("unix-sync", Funix_sync, 0, 0, "", /*
Tell Unix to finish all pending disk updates.
*/
       ())
{
#ifndef WIN32_NATIVE
  sync ();
#endif
  return Qnil;
}


DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, 2, 2, 0, /*
Return t if file FILE1 is newer than file FILE2.
If FILE1 does not exist, the answer is nil;
otherwise, if FILE2 does not exist, the answer is t.
*/
       (file1, file2))
{
  /* This function can GC.  GC checked 1997.04.10. */
  Lisp_Object abspath1, abspath2;
  struct stat st;
  int mtime1;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2, gcpro3;

  CHECK_STRING (file1);
  CHECK_STRING (file2);

  abspath1 = Qnil;
  abspath2 = Qnil;

  GCPRO3 (abspath1, abspath2, current_buffer->directory);
  abspath1 = expand_and_dir_to_file (file1, current_buffer->directory);
  abspath2 = expand_and_dir_to_file (file2, current_buffer->directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (abspath1, Qfile_newer_than_file_p);
  if (NILP (handler))
    handler = Ffind_file_name_handler (abspath2, Qfile_newer_than_file_p);
  UNGCPRO;
  if (!NILP (handler))
    return call3 (handler, Qfile_newer_than_file_p, abspath1,
		  abspath2);

  if (qxe_stat (XSTRING_DATA (abspath1), &st) < 0)
    return Qnil;

  mtime1 = st.st_mtime;

  if (qxe_stat (XSTRING_DATA (abspath2), &st) < 0)
    return Qt;

  return (mtime1 > st.st_mtime) ? Qt : Qnil;
}


/* Stack sizes > 2**16 is a good way to elicit compiler bugs */
/* #define READ_BUF_SIZE (2 << 16) */
#define READ_BUF_SIZE (1 << 15)

DEFUN ("insert-file-contents-internal", Finsert_file_contents_internal,
       1, 7, 0, /*
Insert contents of file FILENAME after point; no coding-system frobbing.
This function is identical to `insert-file-contents' except for the
handling of the CODESYS and USED-CODESYS arguments.

The file is decoded according to CODESYS; if omitted, no conversion
happens.  If USED-CODESYS is non-nil, it should be a symbol, and the actual
coding system that was used for the decoding is stored into it.  It will in
general be different from CODESYS if CODESYS specifies automatic encoding
detection or end-of-line detection.

Currently START and END refer to byte positions (as opposed to character
positions), even in Mule and under MS Windows. (Fixing this, particularly
under Mule, is very difficult.)
*/
       (filename, visit, start, end, replace, codesys, used_codesys))
{
  /* This function can call lisp */
  struct stat st;
  int fd;
  int saverrno = 0;
  Charcount inserted = 0;
  int speccount;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object val;
  int total;
  Ibyte read_buf[READ_BUF_SIZE];
  int mc_count;
  struct buffer *buf = current_buffer;
  Lisp_Object curbuf;
  int not_regular = 0;
  int do_speedy_insert =
    coding_system_is_binary (Fget_coding_system (codesys));

  if (buf->base_buffer && ! NILP (visit))
    invalid_operation ("Cannot do file visiting in an indirect buffer", Qunbound);

  /* No need to call Fbarf_if_buffer_read_only() here.
     That's called in begin_multiple_change() or wherever. */

  val = Qnil;

  /* #### dmoore - should probably check in various places to see if
     curbuf was killed and if so signal an error? */

  curbuf = wrap_buffer (buf);

  GCPRO4 (filename, val, visit, curbuf);

  mc_count = (NILP (replace)) ?
    begin_multiple_change (buf, BUF_PT  (buf), BUF_PT (buf)) :
    begin_multiple_change (buf, BUF_BEG (buf), BUF_Z  (buf));

  speccount = specpdl_depth (); /* begin_multiple_change also adds
				   an unwind_protect */

  filename = Fexpand_file_name (filename, Qnil);

  if (!NILP (used_codesys))
    CHECK_SYMBOL (used_codesys);

  if ( (!NILP (start) || !NILP (end)) && !NILP (visit) )
    invalid_operation ("Attempt to visit less than an entire file", Qunbound);

  fd = -1;

  if (qxe_stat (XSTRING_DATA (filename), &st) < 0)
    {
    badopen:
      if (NILP (visit))
	report_file_error ("Opening input file", filename);
      st.st_mtime = -1;
      goto notfound;
    }

#ifdef S_IFREG
  /* Signal an error if we are accessing a non-regular file, with
     REPLACE, START or END being non-nil.  */
  if (!S_ISREG (st.st_mode))
    {
      not_regular = 1;

      if (!NILP (visit))
	goto notfound;

      if (!NILP (replace) || !NILP (start) || !NILP (end))
	{
	  end_multiple_change (buf, mc_count);

	  RETURN_UNGCPRO
	    (Fsignal (Qfile_error,
		      list2 (build_msg_string("not a regular file"),
			     filename)));
	}
    }
#endif /* S_IFREG */

  if (!NILP (start))
    CHECK_INT (start);
  else
    start = Qzero;

  if (!NILP (end))
    CHECK_INT (end);

  if (fd < 0)
    {
      if ((fd = qxe_interruptible_open (XSTRING_DATA (filename),
					O_RDONLY | OPEN_BINARY, 0)) < 0)
	goto badopen;
    }

  /* Replacement should preserve point as it preserves markers.  */
  if (!NILP (replace))
    record_unwind_protect (restore_point_unwind, Fpoint_marker (Qnil, Qnil));

  record_unwind_protect (close_file_unwind, make_int (fd));

  /* Supposedly happens on VMS.  */
  if (st.st_size < 0)
    signal_error (Qfile_error, "File size is negative", Qunbound);

  if (NILP (end))
    {
      if (!not_regular)
	{
	  end = make_int (st.st_size);
	  if (XINT (end) != st.st_size)
	    out_of_memory ("Maximum buffer size exceeded", Qunbound);
	}
    }

  /* If requested, replace the accessible part of the buffer
     with the file contents.  Avoid replacing text at the
     beginning or end of the buffer that matches the file contents;
     that preserves markers pointing to the unchanged parts. */
  /* The replace-mode code is currently implemented by comparing the
     file on disk with the contents in the buffer, character by character.
     That works only if the characters on disk are exactly what will go into
     the buffer -- i.e. `binary' conversion.

     FSF tries to implement this in all situations, even the non-binary
     conversion, by (in that case) loading the whole converted file into a
     separate memory area, then doing the comparison.  I really don't see
     the point of this, and it will fail spectacularly if the file is many
     megabytes in size.  To try to get around this, we could certainly read
     from the beginning and decode as necessary before comparing, but doing
     the same at the end gets very difficult because of the possibility of
     modal coding systems -- trying to decode data from any point forward
     without decoding previous data might always give you different results
     from starting at the beginning.  We could try further tricks like
     keeping track of which coding systems are non-modal and providing some
     extra method for such coding systems to be given a chunk of data that
     came from a specified location in a specified file and ask the coding
     systems to return a "sync point" from which the data can be read
     forward and have results guaranteed to be the same as reading from the
     beginning to that point, but I really don't think it's worth it.  If
     we implemented the FSF "brute-force" method, we would have to put a
     reasonable maximum file size on the files.  Is any of this worth it?
     --ben


       It's probably not worth it, and despite what you might take from the
       above, we don't do it currently; that is, for non-"binary" coding
       systems, we don't try to implement replace-mode at all. See the
       do_speedy_insert variable above. The upside of this is that our API
       is consistent and not buggy. -- Aidan Kehoe, Fri Oct 27 21:02:30 CEST
       2006
     */

  if (!NILP (replace))
    {
      if (!do_speedy_insert)
	buffer_delete_range (buf, BUF_BEG (buf), BUF_Z (buf),
			     !NILP (visit) ? INSDEL_NO_LOCKING : 0);
      else
	{
	  char buffer[1 << 14];
	  Charbpos same_at_start = BUF_BEGV (buf);
	  Charbpos same_at_end = BUF_ZV (buf);
	  int overlap;

	  /* Count how many chars at the start of the file
	     match the text at the beginning of the buffer.  */
	  while (1)
	    {
	      int nread;
	      Charbpos charbpos;
	      nread = read_allowing_quit (fd, buffer, sizeof (buffer));
	      if (nread < 0)
		report_file_error ("Reading", filename);
	      else if (nread == 0)
		break;
	      charbpos = 0;
	      while (charbpos < nread && same_at_start < BUF_ZV (buf)
		     && BUF_FETCH_CHAR (buf, same_at_start) ==
		     buffer[charbpos])
		same_at_start++, charbpos++;
	      /* If we found a discrepancy, stop the scan.
		 Otherwise loop around and scan the next bufferful.  */
	      if (charbpos != nread)
		break;
	    }
	  /* If the file matches the buffer completely,
	     there's no need to replace anything.  */
	  if (same_at_start - BUF_BEGV (buf) == st.st_size)
	    {
	      retry_close (fd);
	      unbind_to (speccount);
	      /* Truncate the buffer to the size of the file.  */
	      buffer_delete_range (buf, same_at_start, same_at_end,
				   !NILP (visit) ? INSDEL_NO_LOCKING : 0);
	      goto handled;
	    }
	  /* Count how many chars at the end of the file
	     match the text at the end of the buffer.  */
	  while (1)
	    {
	      int total_read, nread;
	      Charcount charbpos, curpos, trial;

	      /* At what file position are we now scanning?  */
	      curpos = st.st_size - (BUF_ZV (buf) - same_at_end);
	      /* If the entire file matches the buffer tail, stop the scan.  */
	      if (curpos == 0)
		break;
	      /* How much can we scan in the next step?  */
	      trial = min (curpos, (Charbpos) sizeof (buffer));
	      if (lseek (fd, curpos - trial, 0) < 0)
		report_file_error ("Setting file position", filename);

	      total_read = 0;
	      while (total_read < trial)
		{
		  nread = read_allowing_quit (fd, buffer + total_read,
					      trial - total_read);
		  if (nread <= 0)
		    report_file_error ("IO error reading file", filename);
		  total_read += nread;
		}
	      /* Scan this bufferful from the end, comparing with
		 the Emacs buffer.  */
	      charbpos = total_read;
	      /* Compare with same_at_start to avoid counting some buffer text
		 as matching both at the file's beginning and at the end.  */
	      while (charbpos > 0 && same_at_end > same_at_start
		     && BUF_FETCH_CHAR (buf, same_at_end - 1) ==
		     buffer[charbpos - 1])
		same_at_end--, charbpos--;
	      /* If we found a discrepancy, stop the scan.
		 Otherwise loop around and scan the preceding bufferful.  */
	      if (charbpos != 0)
		break;
	      /* If display current starts at beginning of line,
		 keep it that way.  */
	      if (XBUFFER (XWINDOW (Fselected_window (Qnil))->buffer) == buf)
		XWINDOW (Fselected_window (Qnil))->start_at_line_beg =
		  !NILP (Fbolp (wrap_buffer (buf)));
	    }

	  /* Don't try to reuse the same piece of text twice.  */
	  overlap = same_at_start - BUF_BEGV (buf) -
	    (same_at_end + st.st_size - BUF_ZV (buf));
	  if (overlap > 0)
	    same_at_end += overlap;

	  /* Arrange to read only the nonmatching middle part of the file.  */
	  start = make_int (same_at_start - BUF_BEGV (buf));
	  end = make_int (st.st_size - (BUF_ZV (buf) - same_at_end));

	  buffer_delete_range (buf, same_at_start, same_at_end,
			       !NILP (visit) ? INSDEL_NO_LOCKING : 0);
	  /* Insert from the file at the proper position.  */
	  BUF_SET_PT (buf, same_at_start);
	}
    }

  if (!not_regular)
    {
      total = XINT (end) - XINT (start);

      /* Make sure point-max won't overflow after this insertion.  */
      if (total != XINT (make_int (total)))
	out_of_memory ("Maximum buffer size exceeded", Qunbound);
    }
  else
    /* For a special file, all we can do is guess.  The value of -1
       will make the stream functions read as much as possible.  */
    total = -1;

  if (XINT (start) != 0
      /* why was this here? asked jwz.  The reason is that the replace-mode
	 connivings above will normally put the file pointer other than
	 where it should be. */
      || (!NILP (replace) && do_speedy_insert))
    {
      if (lseek (fd, XINT (start), 0) < 0)
	report_file_error ("Setting file position", filename);
    }

  {
    Charbpos cur_point = BUF_PT (buf);
    struct gcpro ngcpro1;
    Lisp_Object stream = make_filedesc_input_stream (fd, 0, total,
						     LSTR_ALLOW_QUIT);

    NGCPRO1 (stream);
    Lstream_set_buffering (XLSTREAM (stream), LSTREAM_BLOCKN_BUFFERED, 65536);
    stream = make_coding_input_stream
      (XLSTREAM (stream), get_coding_system_for_text_file (codesys, 1),
       CODING_DECODE, 0);
    Lstream_set_buffering (XLSTREAM (stream), LSTREAM_BLOCKN_BUFFERED, 65536);

    record_unwind_protect (delete_stream_unwind, stream);

    /* No need to limit the amount of stuff we attempt to read. (It would
       be incorrect, anyway, when Mule is enabled.) Instead, the limiting
       occurs inside of the filedesc stream. */
    while (1)
      {
	Bytecount this_len;
	Charcount cc_inserted;

	QUIT;
	this_len = Lstream_read (XLSTREAM (stream), read_buf,
				 sizeof (read_buf));

	if (this_len <= 0)
	  {
	    if (this_len < 0)
	      saverrno = errno;
	    break;
	  }

	cc_inserted = buffer_insert_raw_string_1 (buf, cur_point, read_buf,
						  this_len,
						  !NILP (visit)
						  ? INSDEL_NO_LOCKING : 0);
	inserted  += cc_inserted;
	cur_point += cc_inserted;
      }
    if (!NILP (used_codesys))
      {
	Fset (used_codesys,
	      XCODING_SYSTEM_NAME
	      (coding_stream_detected_coding_system (XLSTREAM (stream))));
      }
    NUNGCPRO;
  }

  /* Close the file/stream */
  unbind_to (speccount);

  if (saverrno != 0)
    {
      errno = saverrno;
      report_file_error ("Reading", filename);
    }

 notfound:
 handled:

  end_multiple_change (buf, mc_count);

  if (!NILP (visit))
    {
      if (!EQ (buf->undo_list, Qt))
	buf->undo_list = Qnil;
      buf->modtime = st.st_mtime;
      buf->filename = filename;
      /* XEmacs addition: */
      /* This function used to be in C, ostensibly so that
	 it could be called here.  But that's just silly.
	 There's no reason C code can't call out to Lisp
	 code, and it's a lot cleaner this way. */
      /*  Note: compute-buffer-file-truename is called for
	  side-effect!  Its return value is intentionally
	  ignored. */
      if (!NILP (Ffboundp (Qcompute_buffer_file_truename)))
	call1 (Qcompute_buffer_file_truename, wrap_buffer (buf));
      BUF_SAVE_MODIFF (buf) = BUF_MODIFF (buf);
      buf->auto_save_modified = BUF_MODIFF (buf);
      buf->saved_size = make_int (BUF_SIZE (buf));
#ifdef CLASH_DETECTION
      if (!NILP (buf->file_truename))
	unlock_file (buf->file_truename);
      unlock_file (filename);
#endif /* CLASH_DETECTION */
      if (not_regular)
	RETURN_UNGCPRO (Fsignal (Qfile_error,
				 list2 (build_msg_string ("not a regular file"),
			         filename)));

      /* If visiting nonexistent file, return nil.  */
      if (buf->modtime == -1)
	report_file_error ("Opening input file",
			   filename);
    }

  /* Decode file format */
  if (inserted > 0)
    {
      Lisp_Object insval = call3 (Qformat_decode,
                                  Qnil, make_int (inserted), visit);
      CHECK_INT (insval);
      inserted = XINT (insval);
    }

  if (inserted > 0)
    {
      GC_EXTERNAL_LIST_LOOP_2 (p, Vafter_insert_file_functions)
	{
	  Lisp_Object insval = call1 (p, make_int (inserted));
	  if (!NILP (insval))
	    {
	      CHECK_NATNUM (insval);
	      inserted = XINT (insval);
	    }
	}
      END_GC_EXTERNAL_LIST_LOOP (p);
    }

  UNGCPRO;

  if (!NILP (val))
    return (val);
  else
    return (list2 (filename, make_int (inserted)));
}


static int a_write (Lisp_Object outstream, Lisp_Object instream, int pos,
		    Lisp_Object *annot);
static Lisp_Object build_annotations (Lisp_Object start, Lisp_Object end);

/* If build_annotations switched buffers, switch back to BUF.
   Kill the temporary buffer that was selected in the meantime.  */

static Lisp_Object
build_annotations_unwind (Lisp_Object buf)
{
  Lisp_Object tembuf;

  if (XBUFFER (buf) == current_buffer)
    return Qnil;
  tembuf = Fcurrent_buffer ();
  Fset_buffer (buf);
  Fkill_buffer (tembuf);
  return Qnil;
}

DEFUN ("write-region-internal", Fwrite_region_internal, 3, 8,
       "r\nFWrite region to file: ", /*
Write current region into specified file; no coding-system frobbing.

This function is almost identical to `write-region'; see that function for
documentation of the START, END, FILENAME, APPEND, VISIT, and LOCKNAME
arguments.  CODESYS specifies the encoding to be used for the file; if it is
nil, no code conversion occurs. (With `write-region' the coding system is
determined automatically if not specified.)

MUSTBENEW specifies that a check for an existing file of the same name
should be made.  If it is 'excl, XEmacs will error on detecting such a file
and never write it.  If it is some other non-nil value, the user will be
prompted to confirm the overwriting of an existing file.  If it is nil,
existing files are silently overwritten when file system permissions allow
this.

As a special kludge to support auto-saving, when START is nil START and
END are set to the beginning and end, respectively, of the buffer,
regardless of any restrictions.  Don't use this feature.  It is documented
here because write-region handler writers need to be aware of it.

*/
       (start, end, filename, append, visit, lockname, codesys,
        mustbenew))
{
  /* This function can call lisp.  GC checked 2000-07-28 ben */
  int desc;
  int failure;
  int save_errno = 0;
  struct stat st;
  Lisp_Object fn = Qnil;
  int speccount = specpdl_depth ();
  int visiting_other = STRINGP (visit);
  int visiting = (EQ (visit, Qt) || visiting_other);
  int quietly = (!visiting && !NILP (visit));
  Lisp_Object visit_file = Qnil;
  Lisp_Object annotations = Qnil;
  struct buffer *given_buffer;
  Charbpos start1, end1;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  struct gcpro ngcpro1, ngcpro2;
  Lisp_Object curbuf = wrap_buffer (current_buffer);


  /* start, end, visit, and append are never modified in this fun
     so we don't protect them. */
  GCPRO5 (visit_file, filename, codesys, lockname, annotations);
  NGCPRO2 (curbuf, fn);

  /* [[ dmoore - if Fexpand_file_name or handlers kill the buffer,
     we should signal an error rather than blissfully continuing
     along.  ARGH, this function is going to lose lose lose.  We need
     to protect the current_buffer from being destroyed, but the
     multiple return points make this a pain in the butt. ]] we do
     protect curbuf now. --ben */

  codesys = get_coding_system_for_text_file (codesys, 0);

  if (current_buffer->base_buffer && ! NILP (visit))
    invalid_operation ("Cannot do file visiting in an indirect buffer",
		       curbuf);

  if (!NILP (start) && !STRINGP (start))
    get_buffer_range_char (current_buffer, start, end, &start1, &end1, 0);

  {
    Lisp_Object handler;

    if (!NILP (mustbenew) && !EQ (mustbenew, Qexcl))
      barf_or_query_if_file_exists (filename, "overwrite", 1, NULL);

    if (visiting_other)
      visit_file = Fexpand_file_name (visit, Qnil);
    else
      visit_file = filename;
    filename = Fexpand_file_name (filename, Qnil);

    if (NILP (lockname))
      lockname = visit_file;

    /* We used to UNGCPRO here.  BAD!  visit_file is used below after
       more Lisp calling. */
    /* If the file name has special constructs in it,
       call the corresponding file handler.  */
    handler = Ffind_file_name_handler (filename, Qwrite_region);
    /* If FILENAME has no handler, see if VISIT has one.  */
    if (NILP (handler) && STRINGP (visit))
      handler = Ffind_file_name_handler (visit, Qwrite_region);

    if (!NILP (handler))
      {
        Lisp_Object val = call8 (handler, Qwrite_region, start, end,
                                 filename, append, visit, lockname, codesys);
	if (visiting)
	  {
	    BUF_SAVE_MODIFF (current_buffer) = BUF_MODIFF (current_buffer);
	    current_buffer->saved_size = make_int (BUF_SIZE (current_buffer));
	    current_buffer->filename = visit_file;
	    MARK_MODELINE_CHANGED;
	  }
	NUNGCPRO;
	UNGCPRO;
	return val;
      }
  }

#ifdef CLASH_DETECTION
  if (!auto_saving)
    lock_file (lockname);
#endif /* CLASH_DETECTION */

  /* Special kludge to simplify auto-saving.  */
  if (NILP (start))
    {
      start1 = BUF_BEG (current_buffer);
      end1 = BUF_Z (current_buffer);
    }

  record_unwind_protect (build_annotations_unwind, Fcurrent_buffer ());

  given_buffer = current_buffer;
  annotations = build_annotations (start, end);
  if (current_buffer != given_buffer)
    {
      start1 = BUF_BEGV (current_buffer);
      end1 = BUF_ZV (current_buffer);
    }

  fn = filename;
  desc = -1;
  if (!NILP (append))
    {
      desc = qxe_open (XSTRING_DATA (fn), O_WRONLY | OPEN_BINARY
                       | (EQ (mustbenew, Qexcl) ? O_EXCL : 0), 0);
    }
  if (desc < 0)
    {
      desc = qxe_open (XSTRING_DATA (fn),
		       O_WRONLY | (EQ (mustbenew, Qexcl) ? O_EXCL : O_TRUNC)
                       | O_CREAT | OPEN_BINARY,
		       auto_saving ? auto_save_mode_bits : CREAT_MODE);
    }

  if (desc < 0)
    {
#ifdef CLASH_DETECTION
      save_errno = errno;
      if (!auto_saving) unlock_file (lockname);
      errno = save_errno;
#endif /* CLASH_DETECTION */
      report_file_error ("Opening output file", filename);
    }

  {
    Lisp_Object desc_locative = Fcons (make_int (desc), Qnil);
    Lisp_Object instream = Qnil, outstream = Qnil;
    struct gcpro nngcpro1, nngcpro2;
    NNGCPRO2 (instream, outstream);

    record_unwind_protect (close_file_unwind, desc_locative);

    if (!NILP (append))
      {
	if (lseek (desc, 0, 2) < 0)
	  {
#ifdef CLASH_DETECTION
	    if (!auto_saving) unlock_file (lockname);
#endif /* CLASH_DETECTION */
	    report_file_error ("Lseek error",
			       filename);
	  }
      }

    failure = 0;

    /* Note: I tried increasing the buffering size, along with
       various other tricks, but nothing seemed to make much of
       a difference in the time it took to save a large file.
       (Actually that's not true.  With a local disk, changing
       the buffer size doesn't seem to make much difference.
       With an NFS-mounted disk, it could make a lot of difference
       because you're affecting the number of network requests
       that need to be made, and there could be a large latency
       for each request.  So I've increased the buffer size
       to 64K.) */
    outstream = make_filedesc_output_stream (desc, 0, -1, 0);
    Lstream_set_buffering (XLSTREAM (outstream),
			   LSTREAM_BLOCKN_BUFFERED, 65536);
    outstream =
      make_coding_output_stream (XLSTREAM (outstream), codesys,
				 CODING_ENCODE, 0);
    Lstream_set_buffering (XLSTREAM (outstream),
			   LSTREAM_BLOCKN_BUFFERED, 65536);
    if (STRINGP (start))
      {
	instream = make_lisp_string_input_stream (start, 0, -1);
	start1 = 0;
      }
    else
      instream = make_lisp_buffer_input_stream (current_buffer, start1, end1,
						LSTR_SELECTIVE |
						LSTR_IGNORE_ACCESSIBLE);
    failure = (0 > (a_write (outstream, instream, start1,
			     &annotations)));
    save_errno = errno;
    /* Note that this doesn't close the desc since we created the
       stream without the LSTR_CLOSING flag, but it does
       flush out any buffered data. */
    if (Lstream_close (XLSTREAM (outstream)) < 0)
      {
	failure = 1;
	save_errno = errno;
      }
    Lstream_close (XLSTREAM (instream));

#ifdef HAVE_FSYNC
    /* Note fsync appears to change the modtime on BSD4.2 (both vax and sun).
       Disk full in NFS may be reported here.  */
    /* mib says that closing the file will try to write as fast as NFS can do
       it, and that means the fsync here is not crucial for autosave files.  */
    if (!auto_saving && !write_region_inhibit_fsync && fsync (desc) < 0
	/* If fsync fails with EINTR, don't treat that as serious.  */
	&& errno != EINTR)
      {
	failure = 1;
	save_errno = errno;
      }
#endif /* HAVE_FSYNC */

    /* Spurious "file has changed on disk" warnings used to be seen on
       systems where close() can change the modtime.  This is known to
       happen on various NFS file systems, on Windows, and on Linux.
       Rather than handling this on a per-system basis, we
       unconditionally do the qxe_stat() after the retry_close(). */

    /* NFS can report a write failure now.  */
    if (retry_close (desc) < 0)
      {
	failure = 1;
	save_errno = errno;
      }

    /* Discard the close unwind-protect.  Execute the one for
       build_annotations (switches back to the original current buffer
       as necessary). */
    XCAR (desc_locative) = Qnil;
    unbind_to (speccount);

    NNUNGCPRO;
  }

  qxe_stat (XSTRING_DATA (fn), &st);

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (lockname);
#endif /* CLASH_DETECTION */

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (visiting)
    current_buffer->modtime = st.st_mtime;

  if (failure)
    {
      errno = save_errno;
      report_file_error ("Writing file", fn);
    }

  if (visiting)
    {
      BUF_SAVE_MODIFF (current_buffer) = BUF_MODIFF (current_buffer);
      current_buffer->saved_size = make_int (BUF_SIZE (current_buffer));
      current_buffer->filename = visit_file;
      MARK_MODELINE_CHANGED;
    }
  else if (quietly)
    {
      NUNGCPRO;
      UNGCPRO;
      return Qnil;
    }

  if (!auto_saving)
    {
      if (visiting_other)
        message ("Wrote %s", XSTRING_DATA (visit_file));
      else
	{
	  Lisp_Object fsp = Qnil;
	  struct gcpro nngcpro1;

	  NNGCPRO1 (fsp);
	  fsp = Ffile_symlink_p (fn);
	  if (NILP (fsp))
	    message ("Wrote %s", XSTRING_DATA (fn));
	  else
	    message ("Wrote %s (symlink to %s)",
		     XSTRING_DATA (fn), XSTRING_DATA (fsp));
	  NNUNGCPRO;
	}
    }
  NUNGCPRO;
  UNGCPRO;
  return Qnil;
}

/* #### This is such a load of shit!!!!  There is no way we should define
   something so stupid as a subr, just sort the fucking list more
   intelligently. */
DEFUN ("car-less-than-car", Fcar_less_than_car, 2, 2, 0, /*
Return t if (car A) is numerically less than (car B).
*/
       (a, b))
{
  Lisp_Object objs[2];
  objs[0] = Fcar (a);
  objs[1] = Fcar (b);
  return Flss (2, objs);
}

/* Heh heh heh, let's define this too, just to aggravate the person who
   wrote the above comment. */
DEFUN ("cdr-less-than-cdr", Fcdr_less_than_cdr, 2, 2, 0, /*
Return t if (cdr A) is numerically less than (cdr B).
*/
       (a, b))
{
  Lisp_Object objs[2];
  objs[0] = Fcdr (a);
  objs[1] = Fcdr (b);
  return Flss (2, objs);
}

/* Build the complete list of annotations appropriate for writing out
   the text between START and END, by calling all the functions in
   write-region-annotate-functions and merging the lists they return.
   If one of these functions switches to a different buffer, we assume
   that buffer contains altered text.  Therefore, the caller must
   make sure to restore the current buffer in all cases,
   as save-excursion would do.  */

static Lisp_Object
build_annotations (Lisp_Object start, Lisp_Object end)
{
  /* This function can GC */
  Lisp_Object annotations;
  Lisp_Object p, res;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object original_buffer = wrap_buffer (current_buffer);


  annotations = Qnil;
  p = Vwrite_region_annotate_functions;
  GCPRO2 (annotations, p);
  while (!NILP (p))
    {
      struct buffer *given_buffer = current_buffer;
      Vwrite_region_annotations_so_far = annotations;
      res = call2 (Fcar (p), start, end);
      /* If the function makes a different buffer current,
	 assume that means this buffer contains altered text to be output.
	 Reset START and END from the buffer bounds
	 and discard all previous annotations because they should have
	 been dealt with by this function.  */
      if (current_buffer != given_buffer)
	{
	  start = make_int (BUF_BEGV (current_buffer));
	  end = make_int (BUF_ZV (current_buffer));
	  annotations = Qnil;
	}
      Flength (res);     /* Check basic validity of return value */
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = Fcdr (p);
    }

  /* Now do the same for annotation functions implied by the file-format */
  if (auto_saving && (!EQ (Vauto_save_file_format, Qt)))
    p = Vauto_save_file_format;
  else
    p = current_buffer->file_format;
  while (!NILP (p))
    {
      struct buffer *given_buffer = current_buffer;
      Vwrite_region_annotations_so_far = annotations;
      res = call4 (Qformat_annotate_function, Fcar (p), start, end,
		   original_buffer);
      if (current_buffer != given_buffer)
	{
	  start = make_int (BUF_BEGV (current_buffer));
	  end = make_int (BUF_ZV (current_buffer));
	  annotations = Qnil;
	}
      Flength (res);
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = Fcdr (p);
    }
  UNGCPRO;
  return annotations;
}

/* Write to stream OUTSTREAM the characters from INSTREAM (it is read until
   EOF is encountered), assuming they start at position POS in the buffer
   of string that STREAM refers to.  Intersperse with them the annotations
   from *ANNOT that fall into the range of positions we are reading from,
   each at its appropriate position.

   Modify *ANNOT by discarding elements as we output them.
   The return value is negative in case of system call failure.  */

/* 4K should probably be fine.  We just need to reduce the number of
   function calls to reasonable level.  The Lstream stuff itself will
   batch to 64K to reduce the number of system calls. */

#define A_WRITE_BATCH_SIZE 4096

static int
a_write (Lisp_Object outstream, Lisp_Object instream, int pos,
	 Lisp_Object *annot)
{
  Lisp_Object tem;
  int nextpos;
  unsigned char largebuf[A_WRITE_BATCH_SIZE];
  Lstream *instr = XLSTREAM (instream);
  Lstream *outstr = XLSTREAM (outstream);

  while (LISTP (*annot))
    {
      tem = Fcar_safe (Fcar (*annot));
      if (INTP (tem))
	nextpos = XINT (tem);
      else
	nextpos = INT_MAX;
#ifdef MULE
      /* If there are annotations left and we have Mule, then we
	 have to do the I/O one ichar at a time so we can
	 determine when to insert the annotation. */
      if (!NILP (*annot))
	{
	  Ichar ch;
	  while (pos != nextpos && (ch = Lstream_get_ichar (instr)) != EOF)
	    {
	      if (Lstream_put_ichar (outstr, ch) < 0)
		return -1;
	      pos++;
	    }
	}
      else
#endif /* MULE */
	{
	  while (pos != nextpos)
	    {
	      /* Otherwise there is no point to that.  Just go in batches. */
	      int chunk = min (nextpos - pos, A_WRITE_BATCH_SIZE);

	      chunk = Lstream_read (instr, largebuf, chunk);
	      if (chunk < 0)
		return -1;
	      if (chunk == 0) /* EOF */
		break;
	      if (Lstream_write (outstr, largebuf, chunk) < 0)
		return -1;
	      pos += chunk;
	    }
	}
      if (pos == nextpos)
	{
	  tem = Fcdr (Fcar (*annot));
	  if (STRINGP (tem))
	    {
	      if (Lstream_write (outstr, XSTRING_DATA (tem),
				 XSTRING_LENGTH (tem)) < 0)
		return -1;
	    }
	  *annot = Fcdr (*annot);
	}
      else
	return 0;
    }
  return -1;
}



#if 0
#include <des_crypt.h>

#define CRYPT_BLOCK_SIZE 8	/* bytes */
#define CRYPT_KEY_SIZE 8	/* bytes */

DEFUN ("encrypt-string", Fencrypt_string, 2, 2, 0, /*
Encrypt STRING using KEY.
*/
       (string, key))
{
  /* !!#### Needs work */
  Extbyte *encrypted_string, *raw_key;
  Extbyte *string_ext, *key_ext;
  Bytecount string_size_ext, key_size_ext, rounded_size, extra, key_size;

  CHECK_STRING (string);
  CHECK_STRING (key);

  LISP_STRING_TO_SIZED_EXTERNAL (string, string_ext, string_size_ext, Qbinary);
  LISP_STRING_TO_SIZED_EXTERNAL (key, key_ext, key_size_ext, Qbinary);

  extra = string_size_ext % CRYPT_BLOCK_SIZE;
  rounded_size = string_size_ext + extra;
  encrypted_string = ALLOCA (rounded_size + 1);
  memcpy (encrypted_string, string_ext, string_size_ext);
  memset (encrypted_string + rounded_size - extra, 0, extra + 1);

  key_size = min (CRYPT_KEY_SIZE, key_size_ext);

  raw_key = ALLOCA (CRYPT_KEY_SIZE + 1);
  memcpy (raw_key, key_ext, key_size);
  memset (raw_key + key_size, 0, (CRYPT_KEY_SIZE + 1) - key_size);

  ecb_crypt (raw_key, encrypted_string, rounded_size,
	     DES_ENCRYPT | DES_SW);
  return make_extstring (encrypted_string, rounded_size, Qbinary);
}

DEFUN ("decrypt-string", Fdecrypt_string, 2, 2, 0, /*
Decrypt STRING using KEY.
*/
       (string, key))
{
  Extbyte *decrypted_string, *raw_key;
  Extbyte *string_ext, *key_ext;
  Bytecount string_size_ext, key_size_ext, string_size, key_size;

  CHECK_STRING (string);
  CHECK_STRING (key);

  LISP_STRING_TO_SIZED_EXTERNAL (string, string_ext, string_size_ext, Qbinary);
  LISP_STRING_TO_SIZED_EXTERNAL (key, key_ext, key_size_ext, Qbinary);

  string_size = string_size_ext + 1;
  decrypted_string = ALLOCA (string_size);
  memcpy (decrypted_string, string_ext, string_size);
  decrypted_string[string_size - 1] = '\0';

  key_size = min (CRYPT_KEY_SIZE, key_size_ext);

  raw_key = ALLOCA (CRYPT_KEY_SIZE + 1);
  memcpy (raw_key, key_ext, key_size);
  memset (raw_key + key_size, 0, (CRYPT_KEY_SIZE + 1) - key_size);


  ecb_crypt (raw_key, decrypted_string, string_size, D | DES_SW);
  return make_extstring (decrypted_string, string_size - 1, Qbinary);
}
#endif /* 0 */


DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime, 1, 1, 0, /*
Return t if last mod time of BUFFER's visited file matches what BUFFER records.
This means that the file has not been changed since it was visited or saved.
*/
       (buffer))
{
  /* This function can call lisp; GC checked 2000-07-11 ben */
  struct buffer *b;
  struct stat st;
  Lisp_Object handler;

  CHECK_BUFFER (buffer);
  b = XBUFFER (buffer);

  if (!STRINGP (b->filename)) return Qt;
  if (b->modtime == 0) return Qt;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (b->filename,
                                     Qverify_visited_file_modtime);
  if (!NILP (handler))
    return call2 (handler, Qverify_visited_file_modtime, buffer);

  if (qxe_stat (XSTRING_DATA (b->filename), &st) < 0)
    {
      /* If the file doesn't exist now and didn't exist before,
	 we say that it isn't modified, provided the error is a tame one.  */
      if (errno == ENOENT || errno == EACCES || errno == ENOTDIR)
	st.st_mtime = -1;
      else
	st.st_mtime = 0;
    }
  if (st.st_mtime == b->modtime
      /* If both are positive, accept them if they are off by one second.  */
      || (st.st_mtime > 0 && b->modtime > 0
	  && (st.st_mtime == b->modtime + 1
	      || st.st_mtime == b->modtime - 1)))
    return Qt;
  return Qnil;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime, 0, 0, 0, /*
Clear out records of last mod time of visited file.
Next attempt to save will certainly not complain of a discrepancy.
*/
       ())
{
  current_buffer->modtime = 0;
  return Qnil;
}

DEFUN ("visited-file-modtime", Fvisited_file_modtime, 0, 0, 0, /*
Return the current buffer's recorded visited file modification time.
The value is a list of the form (HIGH . LOW), like the time values
that `file-attributes' returns.
*/
       ())
{
  return time_to_lisp ((time_t) current_buffer->modtime);
}

DEFUN ("set-visited-file-modtime", Fset_visited_file_modtime, 0, 1, 0, /*
Update buffer's recorded modification time from the visited file's time.
Useful if the buffer was not read from the file normally
or if the file itself has been changed for some known benign reason.
An argument specifies the modification time value to use
\(instead of that of the visited file), in the form of a list
\(HIGH . LOW) or (HIGH LOW).
*/
       (time_list))
{
  /* This function can call lisp */
  if (!NILP (time_list))
    {
      time_t the_time;
      lisp_to_time (time_list, &the_time);
      current_buffer->modtime = (int) the_time;
    }
  else
    {
      Lisp_Object filename = Qnil;
      struct stat st;
      Lisp_Object handler;
      struct gcpro gcpro1, gcpro2, gcpro3;

      GCPRO3 (filename, time_list, current_buffer->filename);
      filename = Fexpand_file_name (current_buffer->filename, Qnil);

      /* If the file name has special constructs in it,
	 call the corresponding file handler.  */
      handler = Ffind_file_name_handler (filename, Qset_visited_file_modtime);
      UNGCPRO;
      if (!NILP (handler))
	/* The handler can find the file name the same way we did.  */
	return call2 (handler, Qset_visited_file_modtime, Qnil);
      else if (qxe_stat (XSTRING_DATA (filename), &st) >= 0)
	current_buffer->modtime = st.st_mtime;
    }

  return Qnil;
}

static Lisp_Object
auto_save_error (Lisp_Object UNUSED (condition_object),
		 Lisp_Object UNUSED (ignored))
{
  /* This function can call lisp */
  if (gc_in_progress)
    return Qnil;
  /* Don't try printing an error message after everything is gone! */
  if (preparing_for_armageddon)
    return Qnil;
  clear_echo_area (selected_frame (), Qauto_saving, 1);
  Fding (Qt, Qauto_save_error, Qnil);
  message ("Auto-saving...error for %s", XSTRING_DATA (current_buffer->name));
  Fsleep_for (make_int (1));
  message ("Auto-saving...error!for %s", XSTRING_DATA (current_buffer->name));
  Fsleep_for (make_int (1));
  message ("Auto-saving...error for %s", XSTRING_DATA (current_buffer->name));
  Fsleep_for (make_int (1));
  return Qnil;
}

static Lisp_Object
auto_save_1 (Lisp_Object UNUSED (ignored))
{
  /* This function can call lisp */
  /* #### I think caller is protecting current_buffer? */
  struct stat st;
  Lisp_Object fn = current_buffer->filename;
  Lisp_Object a  = current_buffer->auto_save_file_name;

  if (!STRINGP (a))
    return (Qnil);

  /* Get visited file's mode to become the auto save file's mode.  */
  if (STRINGP (fn) &&
      qxe_stat (XSTRING_DATA (fn), &st) >= 0)
    /* But make sure we can overwrite it later!  */
    auto_save_mode_bits = st.st_mode | 0600;
  else
    /* default mode for auto-save files of buffers with no file is
       readable by owner only.  This may annoy some small number of
       people, but the alternative removes all privacy from email. */
    auto_save_mode_bits = 0600;

  return
    Fwrite_region_internal (Qnil, Qnil, a, Qnil, Qlambda, Qnil,
#if 1 /* #### Kyle wants it changed to not use escape-quoted.  Think
	 carefully about how this works. */
	        	    Qescape_quoted,
#else
			    current_buffer->buffer_file_coding_system,
#endif
			    Qnil);
}

static Lisp_Object
auto_save_expand_name_error (Lisp_Object condition_object,
			     Lisp_Object UNUSED (ignored))
{
  warn_when_safe_lispobj
    (Qfile, Qerror,
     Fcons (build_msg_string ("Invalid auto-save list-file"),
			  Fcons (Vauto_save_list_file_name,
				 condition_object)));
  return Qnil;
}

static Lisp_Object
auto_save_expand_name (Lisp_Object name)
{
  struct gcpro gcpro1;

  /* note that caller did NOT gc protect name, so we do it. */
  /* [[dmoore - this might not be necessary, if condition_case_1
     protects it.  but I don't think it does.]] indeed it doesn't. --ben */
  GCPRO1 (name);
  RETURN_UNGCPRO (Fexpand_file_name (name, Qnil));
}


static Lisp_Object
do_auto_save_unwind (Lisp_Object fd)
{
  retry_close (XINT (fd));
  return (fd);
}

/* Fdo_auto_save() checks whether a GC is in progress when it is called,
   and if so, tries to avoid touching lisp objects.

   The only time that Fdo_auto_save() is called while GC is in progress
   is if we're going down, as a result of an ABORT() or a kill signal.
   It's fairly important that we generate autosave files in that case!
 */

DEFUN ("do-auto-save", Fdo_auto_save, 0, 2, "", /*
Auto-save all buffers that need it.
This is all buffers that have auto-saving enabled
and are changed since last auto-saved.
Auto-saving writes the buffer into a file
so that your editing is not lost if the system crashes.
This file is not the file you visited; that changes only when you save.
Normally we run the normal hook `auto-save-hook' before saving.

Non-nil first argument means do not print any message if successful.
Non-nil second argument means save only current buffer.
*/
       (no_message, current_only))
{
  /* This function can call lisp */
  struct buffer *b;
  Lisp_Object tail, buf;
  int auto_saved = 0;
  int do_handled_files;
  Lisp_Object oquit = Qnil;
  Lisp_Object listfile = Qnil;
  Lisp_Object old;
  int listdesc = -1;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;

  old = wrap_buffer (current_buffer);
  GCPRO3 (oquit, listfile, old);
  check_quit (); /* make Vquit_flag accurate */
  /* Ordinarily don't quit within this function,
     but don't make it impossible to quit (in case we get hung in I/O).  */
  oquit = Vquit_flag;
  Vquit_flag = Qnil;

  /* No further GCPRO needed, because (when it matters) all Lisp_Object
     variables point to non-strings reached from Vbuffer_alist.  */

  if (minibuf_level != 0 || preparing_for_armageddon)
    no_message = Qt;

  run_hook (Qauto_save_hook);

  if (STRINGP (Vauto_save_list_file_name))
    listfile = condition_case_1 (Qt,
				 auto_save_expand_name,
				 Vauto_save_list_file_name,
				 auto_save_expand_name_error, Qnil);

  internal_bind_int (&auto_saving, 1);

  /* First, save all files which don't have handlers.  If Emacs is
     crashing, the handlers may tweak what is causing Emacs to crash
     in the first place, and it would be a shame if Emacs failed to
     autosave perfectly ordinary files because it couldn't handle some
     ange-ftp'd file.  */
  for (do_handled_files = 0; do_handled_files < 2; do_handled_files++)
    {
      for (tail = Vbuffer_alist;
	   CONSP (tail);
	   tail = XCDR (tail))
	{
	  buf = XCDR (XCAR (tail));
	  b = XBUFFER (buf);

	  if (!NILP (current_only)
	      && b != current_buffer)
	    continue;

	  /* Don't auto-save indirect buffers.
	     The base buffer takes care of it.  */
	  if (b->base_buffer)
	    continue;

	  /* Check for auto save enabled
	     and file changed since last auto save
	     and file changed since last real save.  */
	  if (STRINGP (b->auto_save_file_name)
	      && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b)
	      && b->auto_save_modified < BUF_MODIFF (b)
	      /* -1 means we've turned off autosaving for a while--see below.  */
	      && XINT (b->saved_size) >= 0
	      && (do_handled_files
		  || NILP (Ffind_file_name_handler (b->auto_save_file_name,
						    Qwrite_region))))
	    {
	      EMACS_TIME before_time, after_time;

	      EMACS_GET_TIME (before_time);
	      /* If we had a failure, don't try again for 20 minutes.  */
	      if (!preparing_for_armageddon
		  && b->auto_save_failure_time >= 0
		  && (EMACS_SECS (before_time) - b->auto_save_failure_time <
		      1200))
		continue;

	      if (!preparing_for_armageddon &&
		  (XINT (b->saved_size) * 10
		   > (BUF_Z (b) - BUF_BEG (b)) * 13)
		  /* A short file is likely to change a large fraction;
		     spare the user annoying messages.  */
		  && XINT (b->saved_size) > 5000
		  /* These messages are frequent and annoying for `*mail*'.  */
		  && !NILP (b->filename)
		  && NILP (no_message)
		  && disable_auto_save_when_buffer_shrinks)
		{
		  /* It has shrunk too much; turn off auto-saving here.
		     Unless we're about to crash, in which case auto-save it
		     anyway.
		     */
		  message
		    ("Buffer %s has shrunk a lot; auto save turned off there",
		     XSTRING_DATA (b->name));
		  /* Turn off auto-saving until there's a real save,
		     and prevent any more warnings.  */
		  b->saved_size = make_int (-1);
		  if (!gc_in_progress)
		    Fsleep_for (make_int (1));
		  continue;
		}
	      set_buffer_internal (b);
	      if (!auto_saved && NILP (no_message))
		{
		  static const Ibyte *msg = (const Ibyte *) "Auto-saving...";
		  echo_area_message (selected_frame (), msg, Qnil,
				     0, qxestrlen (msg),
				     Qauto_saving);
		}

	      /* Open the auto-save list file, if necessary.
		 We only do this now so that the file only exists
		 if we actually auto-saved any files. */
	      if (!auto_saved && !inhibit_auto_save_session
		  && !NILP (Vauto_save_list_file_prefix)
		  && STRINGP (listfile) && listdesc < 0)
		{
		  listdesc =
		    qxe_open (XSTRING_DATA (listfile),
			      O_WRONLY | O_TRUNC | O_CREAT | OPEN_BINARY,
			      CREAT_MODE);

		  /* Arrange to close that file whether or not we get
		     an error. */
		  if (listdesc >= 0)
		    record_unwind_protect (do_auto_save_unwind,
					   make_int (listdesc));
		}

	      /* Record all the buffers that we are auto-saving in
		 the special file that lists them.  For each of
		 these buffers, record visited name (if any) and
		 auto save name.  */
	      if (listdesc >= 0)
		{
		  const Extbyte *auto_save_file_name_ext;
		  Bytecount auto_save_file_name_ext_len;

		  TO_EXTERNAL_FORMAT (LISP_STRING, b->auto_save_file_name,
				      ALLOCA, (auto_save_file_name_ext,
					       auto_save_file_name_ext_len),
				      Qescape_quoted);
		  if (!NILP (b->filename))
		    {
		      const Extbyte *filename_ext;
		      Bytecount filename_ext_len;

		      TO_EXTERNAL_FORMAT (LISP_STRING, b->filename,
					  ALLOCA, (filename_ext,
						   filename_ext_len),
					  Qescape_quoted);
		      retry_write (listdesc, filename_ext, filename_ext_len);
		    }
		  retry_write (listdesc, "\n", 1);
		  retry_write (listdesc, auto_save_file_name_ext,
			 auto_save_file_name_ext_len);
		  retry_write (listdesc, "\n", 1);
		}

	      /* dmoore - In a bad scenario we've set b=XBUFFER(buf)
		 based on values in Vbuffer_alist.  auto_save_1 may
		 cause lisp handlers to run.  Those handlers may kill
		 the buffer and then GC.  Since the buffer is killed,
		 it's no longer in Vbuffer_alist so it might get reaped
		 by the GC.  We also need to protect tail. */
	      /* #### There is probably a lot of other code which has
		 pointers into buffers which may get blown away by
		 handlers. */
	      {
		struct gcpro ngcpro1, ngcpro2;
		NGCPRO2 (buf, tail);
		condition_case_1 (Qt,
				  auto_save_1, Qnil,
				  auto_save_error, Qnil);
		NUNGCPRO;
	      }
	      /* Handler killed our saved current-buffer!  Pick any. */
	      if (!BUFFER_LIVE_P (XBUFFER (old)))
		old = wrap_buffer (current_buffer);

	      set_buffer_internal (XBUFFER (old));
	      auto_saved++;

	      /* Handler killed their own buffer! */
	      if (!BUFFER_LIVE_P(b))
		continue;

	      b->auto_save_modified = BUF_MODIFF (b);
	      b->saved_size = make_int (BUF_SIZE (b));
	      EMACS_GET_TIME (after_time);
	      /* If auto-save took more than 60 seconds,
		 assume it was an NFS failure that got a timeout.  */
	      if (EMACS_SECS (after_time) - EMACS_SECS (before_time) > 60)
		b->auto_save_failure_time = EMACS_SECS (after_time);
	    }
	}
    }

  /* Prevent another auto save till enough input events come in.  */
  if (auto_saved)
    record_auto_save ();

  /* If we didn't save anything into the listfile, remove the old
     one because nothing needed to be auto-saved.  Do this afterwards
     rather than before in case we get a crash attempting to autosave
     (in that case we'd still want the old one around). */
  if (listdesc < 0 && !auto_saved && STRINGP (listfile))
    qxe_unlink (XSTRING_DATA (listfile));

  /* Show "...done" only if the echo area would otherwise be empty. */
  if (auto_saved && NILP (no_message)
      && NILP (clear_echo_area (selected_frame (), Qauto_saving, 0)))
    {
      static const Ibyte *msg = (const Ibyte *)"Auto-saving...done";
      echo_area_message (selected_frame (), msg, Qnil, 0,
			 qxestrlen (msg), Qauto_saving);
    }

  Vquit_flag = oquit;

  RETURN_UNGCPRO (unbind_to (speccount));
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved, 0, 0, 0, /*
Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again.
*/
       ())
{
  current_buffer->auto_save_modified = BUF_MODIFF (current_buffer);
  current_buffer->saved_size = make_int (BUF_SIZE (current_buffer));
  current_buffer->auto_save_failure_time = -1;
  return Qnil;
}

DEFUN ("clear-buffer-auto-save-failure", Fclear_buffer_auto_save_failure, 0, 0, 0, /*
Clear any record of a recent auto-save failure in the current buffer.
*/
       ())
{
  current_buffer->auto_save_failure_time = -1;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, 0, 0, 0, /*
Return t if buffer has been auto-saved since last read in or saved.
*/
       ())
{
  return (BUF_SAVE_MODIFF (current_buffer) <
	  current_buffer->auto_save_modified) ? Qt : Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_fileio (void)
{
  DEFSYMBOL (Qexpand_file_name);
  DEFSYMBOL (Qfile_truename);
  DEFSYMBOL (Qsubstitute_in_file_name);
  DEFSYMBOL (Qdirectory_file_name);
  DEFSYMBOL (Qfile_name_directory);
  DEFSYMBOL (Qfile_name_nondirectory);
  DEFSYMBOL (Qfile_name_sans_extension);
  DEFSYMBOL (Qunhandled_file_name_directory);
  DEFSYMBOL (Qfile_name_as_directory);
  DEFSYMBOL (Qcopy_file);
  DEFSYMBOL (Qmake_directory_internal);
  DEFSYMBOL (Qdelete_directory);
  DEFSYMBOL (Qdelete_file);
  DEFSYMBOL (Qrename_file);
  DEFSYMBOL (Qadd_name_to_file);
  DEFSYMBOL (Qmake_symbolic_link);
  DEFSYMBOL (Qmake_temp_name);
  DEFSYMBOL (Qfile_exists_p);
  DEFSYMBOL (Qfile_executable_p);
  DEFSYMBOL (Qfile_readable_p);
  DEFSYMBOL (Qfile_symlink_p);
  DEFSYMBOL (Qfile_writable_p);
  DEFSYMBOL (Qfile_directory_p);
  DEFSYMBOL (Qfile_regular_p);
  DEFSYMBOL (Qfile_accessible_directory_p);
  DEFSYMBOL (Qfile_modes);
  DEFSYMBOL (Qset_file_modes);
  DEFSYMBOL (Qfile_newer_than_file_p);
  DEFSYMBOL (Qinsert_file_contents);
  DEFSYMBOL (Qwrite_region);
  DEFSYMBOL (Qverify_visited_file_modtime);
  DEFSYMBOL (Qset_visited_file_modtime);
  DEFSYMBOL (Qcar_less_than_car); /* Vomitous! */
  DEFSYMBOL (Qexcl);

  DEFSYMBOL (Qauto_save_hook);
  DEFSYMBOL (Qauto_save_error);
  DEFSYMBOL (Qauto_saving);

  DEFSYMBOL (Qformat_decode);
  DEFSYMBOL (Qformat_annotate_function);

  DEFSYMBOL (Qcompute_buffer_file_truename);

  DEFERROR_STANDARD (Qfile_already_exists, Qfile_error);

  DEFSUBR (Ffind_file_name_handler);

  DEFSUBR (Ffile_name_directory);
  DEFSUBR (Ffile_name_nondirectory);
  DEFSUBR (Funhandled_file_name_directory);
  DEFSUBR (Ffile_name_as_directory);
  DEFSUBR (Fdirectory_file_name);
  DEFSUBR (Fmake_temp_name);
  DEFSUBR (Fexpand_file_name);
  DEFSUBR (Ffile_truename);
  DEFSUBR (Fsubstitute_in_file_name);
  DEFSUBR (Fcopy_file);
  DEFSUBR (Fmake_directory_internal);
  DEFSUBR (Fdelete_directory);
  DEFSUBR (Fdelete_file);
  DEFSUBR (Frename_file);
  DEFSUBR (Fadd_name_to_file);
  DEFSUBR (Fmake_symbolic_link);
#ifdef HPUX_NET
  DEFSUBR (Fsysnetunam);
#endif /* HPUX_NET */
  DEFSUBR (Ffile_name_absolute_p);
  DEFSUBR (Ffile_exists_p);
  DEFSUBR (Ffile_executable_p);
  DEFSUBR (Ffile_readable_p);
  DEFSUBR (Ffile_writable_p);
  DEFSUBR (Ffile_symlink_p);
  DEFSUBR (Ffile_directory_p);
  DEFSUBR (Ffile_accessible_directory_p);
  DEFSUBR (Ffile_regular_p);
  DEFSUBR (Ffile_modes);
  DEFSUBR (Fset_file_modes);
  DEFSUBR (Fset_default_file_modes);
  DEFSUBR (Fdefault_file_modes);
  DEFSUBR (Funix_sync);
  DEFSUBR (Ffile_newer_than_file_p);
  DEFSUBR (Finsert_file_contents_internal);
  DEFSUBR (Fwrite_region_internal);
  DEFSUBR (Fcar_less_than_car); /* Vomitous! */
  DEFSUBR (Fcdr_less_than_cdr); /* Yeah oh yeah bucko .... */
#if 0
  DEFSUBR (Fencrypt_string);
  DEFSUBR (Fdecrypt_string);
#endif
  DEFSUBR (Fverify_visited_file_modtime);
  DEFSUBR (Fclear_visited_file_modtime);
  DEFSUBR (Fvisited_file_modtime);
  DEFSUBR (Fset_visited_file_modtime);

  DEFSUBR (Fdo_auto_save);
  DEFSUBR (Fset_buffer_auto_saved);
  DEFSUBR (Fclear_buffer_auto_save_failure);
  DEFSUBR (Frecent_auto_save_p);
}

void
vars_of_fileio (void)
{
  QSin_expand_file_name =
    build_defer_string ("(in expand-file-name)");
  staticpro (&QSin_expand_file_name);

  DEFVAR_LISP ("auto-save-file-format", &Vauto_save_file_format /*
*Format in which to write auto-save files.
Should be a list of symbols naming formats that are defined in `format-alist'.
If it is t, which is the default, auto-save files are written in the
same format as a regular save would use.
*/ );
  Vauto_save_file_format = Qt;

  DEFVAR_LISP ("file-name-handler-alist", &Vfile_name_handler_alist /*
*Alist of elements (REGEXP . HANDLER) for file names handled specially.
If a file name matches REGEXP, then all I/O on that file is done by calling
HANDLER.

The first argument given to HANDLER is the name of the I/O primitive
to be handled; the remaining arguments are the arguments that were
passed to that primitive.  For example, if you do
    (file-exists-p FILENAME)
and FILENAME is handled by HANDLER, then HANDLER is called like this:
    (funcall HANDLER 'file-exists-p FILENAME)
The function `find-file-name-handler' checks this list for a handler
for its argument.
*/ );
  Vfile_name_handler_alist = Qnil;

  DEFVAR_LISP ("after-insert-file-functions", &Vafter_insert_file_functions /*
A list of functions to be called at the end of `insert-file-contents'.
Each is passed one argument, the number of bytes inserted.  It should return
the new byte count, and leave point the same.  If `insert-file-contents' is
intercepted by a handler from `file-name-handler-alist', that handler is
responsible for calling the after-insert-file-functions if appropriate.
*/ );
  Vafter_insert_file_functions = Qnil;

  DEFVAR_LISP ("write-region-annotate-functions",
	       &Vwrite_region_annotate_functions /*
A list of functions to be called at the start of `write-region'.
Each is passed two arguments, START and END, as for `write-region'.
It should return a list of pairs (POSITION . STRING) of strings to be
effectively inserted at the specified positions of the file being written
\(1 means to insert before the first byte written).  The POSITIONs must be
sorted into increasing order.  If there are several functions in the list,
the several lists are merged destructively.
*/ );
  Vwrite_region_annotate_functions = Qnil;

  DEFVAR_LISP ("write-region-annotations-so-far",
	       &Vwrite_region_annotations_so_far /*
When an annotation function is called, this holds the previous annotations.
These are the annotations made by other annotation functions
that were already called.  See also `write-region-annotate-functions'.
*/ );
  Vwrite_region_annotations_so_far = Qnil;

  DEFVAR_LISP ("inhibit-file-name-handlers", &Vinhibit_file_name_handlers /*
A list of file name handlers that temporarily should not be used.
This applies only to the operation `inhibit-file-name-operation'.
*/ );
  Vinhibit_file_name_handlers = Qnil;

  DEFVAR_LISP ("inhibit-file-name-operation", &Vinhibit_file_name_operation /*
The operation for which `inhibit-file-name-handlers' is applicable.
*/ );
  Vinhibit_file_name_operation = Qnil;

  DEFVAR_LISP ("auto-save-list-file-name", &Vauto_save_list_file_name /*
File name in which we write a list of all auto save file names.
*/ );
  Vauto_save_list_file_name = Qnil;

#ifdef HAVE_FSYNC
  DEFVAR_BOOL ("write-region-inhibit-fsync", &write_region_inhibit_fsync /*
*Non-nil means don't call fsync in `write-region'.
This variable affects calls to `write-region' as well as save commands.
A non-nil value may result in data loss!
*/ );
  write_region_inhibit_fsync = 0;
#endif

  DEFVAR_LISP ("auto-save-list-file-prefix", &Vauto_save_list_file_prefix /*
Prefix for generating auto-save-list-file-name.
Emacs's pid and the system name will be appended to
this prefix to create a unique file name.
*/ );
  Vauto_save_list_file_prefix = build_ascstring ("~/.saves-");

  DEFVAR_BOOL ("inhibit-auto-save-session", &inhibit_auto_save_session /*
When non-nil, inhibit auto save list file creation.
*/ );
  inhibit_auto_save_session = 0;

  DEFVAR_BOOL ("disable-auto-save-when-buffer-shrinks",
	       &disable_auto_save_when_buffer_shrinks /*
If non-nil, auto-saving is disabled when a buffer shrinks too much.
This is to prevent you from losing your edits if you accidentally
delete a large chunk of the buffer and don't notice it until too late.
Saving the buffer normally turns auto-save back on.
*/ );
  disable_auto_save_when_buffer_shrinks = 1;

  DEFVAR_LISP ("directory-sep-char", &Vdirectory_sep_char /*
Directory separator character for built-in functions that return file names.
The value should be either ?/ or ?\\ (any other value is treated as ?\\).
This variable affects the built-in functions only on Windows,
on other platforms, it is initialized so that Lisp code can find out
what the normal separator is.
*/ );
  Vdirectory_sep_char = make_char (DEFAULT_DIRECTORY_SEP);
}

void
reinit_vars_of_fileio (void)
{
  /* We want temp_name_rand to be initialized to a value likely to be
     unique to the process, not to the executable.  The danger is that
     two different XEmacs processes using the same binary on different
     machines creating temp files in the same directory will be
     unlucky enough to have the same pid.  If we randomize using
     process startup time, then in practice they will be unlikely to
     collide. We use the microseconds field so that scripts that start
     simultaneous XEmacs processes on multiple machines will have less
     chance of collision.  */
  {
    EMACS_TIME thyme;

    EMACS_GET_TIME (thyme);
    temp_name_rand = (unsigned int) (EMACS_SECS (thyme) ^ EMACS_USECS (thyme));
  }
}
