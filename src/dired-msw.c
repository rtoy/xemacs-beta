/* fast dired replacement routines for mswindows.
   Copyright (C) 1998 Darryl Okahata
   Portions Copyright (C) 1992, 1994 by Sebastian Kremer <sk@thp.uni-koeln.de>
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

/* Synched up with: Not in FSF. */


/*
 * Parts of this code (& comments) were taken from ls-lisp.el
 * Author: Sebastian Kremer <sk@thp.uni-koeln.de>
 */

/*
 * insert-directory
 * - must insert _exactly_one_line_ describing FILE if WILDCARD and
 * FULL-DIRECTORY-P is nil.
 * The single line of output must display FILE's name as it was
 * given, namely, an absolute path name.
 * - must insert exactly one line for each file if WILDCARD or
 * FULL-DIRECTORY-P is t, plus one optional "total" line
 * before the file lines, plus optional text after the file lines.
 * Lines are delimited by "\n", so filenames containing "\n" are not
 * allowed.
 * File lines should display the basename.
 * - must be consistent with
 * - functions dired-move-to-filename, (these two define what a file line is)
 * dired-move-to-end-of-filename,
 * dired-between-files, (shortcut for (not (dired-move-to-filename)))
 * dired-insert-headerline
 * dired-after-subdir-garbage (defines what a "total" line is)
 * - variable dired-subdir-regexp
 */

/*
 * Insert directory listing for FILE, formatted according to SWITCHES.
 * Leaves point after the inserted text.
 * SWITCHES may be a string of options, or a list of strings.
 * Optional third arg WILDCARD means treat FILE as shell wildcard.
 * Optional fourth arg FULL-DIRECTORY-P means file is a directory and
 * switches do not contain `d', so that a full listing is expected.
 *
 * This works by running a directory listing program
 * whose name is in the variable `insert-directory-program'.
 * If WILDCARD, it also runs the shell specified by `shell-file-name'."
 */

/*
 * Set INDENT_LISTING to non-zero if the inserted text should be shifted
 * over by two spaces.
 */
#define INDENT_LISTING 0

#define ROUND_FILE_SIZES 4096


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "regex.h"
#include "syntax.h"

#include "console-msw.h"

#include "sysdir.h"
#include "sysfile.h"
#include "sysfloat.h"
#include "sysproc.h"
#include "syspwd.h"
#include "systime.h"


static int mswindows_ls_sort_case_insensitive;
static Fixnum mswindows_ls_round_file_size;

Lisp_Object Qmswindows_insert_directory;
Lisp_Object Qwildcard_to_regexp;

extern Lisp_Object Vmswindows_downcase_file_names; /* in device-msw.c */

enum mswindows_sortby
{
  MSWINDOWS_SORT_BY_NAME,
  MSWINDOWS_SORT_BY_NAME_NOCASE,
  MSWINDOWS_SORT_BY_MOD_DATE,
  MSWINDOWS_SORT_BY_SIZE
};


static enum mswindows_sortby mswindows_sort_method;
static int mswindows_reverse_sort;

/* We create our own structure because the cFileName field in
   WIN32_FIND_DATA is in external format and of fixed size, which we
   may exceed when translating.  */

typedef struct
{
  DWORD dwFileAttributes;
  FILETIME ftCreationTime;
  FILETIME ftLastAccessTime;
  FILETIME ftLastWriteTime;
  DWORD nFileSizeHigh;
  DWORD nFileSizeLow;
  Ibyte *cFileName;
} Win32_file;
  
typedef struct
{
  Dynarr_declare (Win32_file);
} Win32_file_dynarr;



#define CMPDWORDS(t1a, t1b, t2a, t2b) \
(((t1a) == (t2a)) ? (((t1b) == (t2b)) ? 0 : (((t1b) < (t2b)) ? -1 : 1)) \
 : (((t1a) < (t2a)) ? -1 : 1))


static int
mswindows_ls_sort_fcn (const void *elem1, const void *elem2)
{
  Win32_file *e1, *e2;
  int status;

  e1 = (Win32_file *) elem1;
  e2 = (Win32_file *) elem2;

  switch (mswindows_sort_method)
    {
    case MSWINDOWS_SORT_BY_NAME:
      status = qxestrcmp (e1->cFileName, e2->cFileName);
      break;
    case MSWINDOWS_SORT_BY_NAME_NOCASE:
      status = qxestrcasecmp (e1->cFileName, e2->cFileName);
      break;
    case MSWINDOWS_SORT_BY_MOD_DATE:
      status = CMPDWORDS (e1->ftLastWriteTime.dwHighDateTime,
			  e1->ftLastWriteTime.dwLowDateTime,
			  e2->ftLastWriteTime.dwHighDateTime,
			  e2->ftLastWriteTime.dwLowDateTime);
      break;
    case MSWINDOWS_SORT_BY_SIZE:
      status = CMPDWORDS (e1->nFileSizeHigh, e1->nFileSizeLow,
			  e2->nFileSizeHigh, e2->nFileSizeLow);
      break;
    default:
      status = 0;
      break;
    }
  if (mswindows_reverse_sort)
    {
      status = -status;
    }
  return (status);
}

static void
mswindows_sort_files (Win32_file_dynarr *files,
		      enum mswindows_sortby sort_by, int reverse)
{
  mswindows_sort_method = sort_by;
  mswindows_reverse_sort = reverse;
  qsort (Dynarr_atp (files, 0), Dynarr_length (files),
	 sizeof (Win32_file), mswindows_ls_sort_fcn);
}

static Win32_file_dynarr *
mswindows_get_files (Lisp_Object dirfile, int nowild, Lisp_Object pattern,
		     int hide_dot, int hide_system)
{
  Win32_file_dynarr *files = Dynarr_new (Win32_file);
  struct re_pattern_buffer *bufp = NULL;
  int findex;
  DECLARE_EISTRING (win32pattern);
  HANDLE fh;
  int				errm;

  while (1)
    {
      if (!NILP (pattern))
	{
	  /* PATTERN might be a flawed regular expression.  Rather than
	     catching and signalling our own errors, we just call
	     compile_pattern to do the work for us.  */
	  bufp = compile_pattern (pattern, 0, Qnil, Qnil, 0, 0, ERROR_ME);
	}
      /* Now *bufp is the compiled form of PATTERN; don't call anything
	 which might compile a new regexp until we're done with the loop! */

      /* for Win32, we need to insure that the pathname ends with "\*". */
      eicpy_lstr (win32pattern, dirfile);
      if (!nowild)
	{
	  Charcount len = eicharlen (win32pattern) - 1;
	  if (!IS_DIRECTORY_SEP (eigetch_char (win32pattern, len)))
	    eicat_c (win32pattern, "\\");
	  eicat_c (win32pattern, "*");
	}
      eito_external (win32pattern, Qmswindows_tstr);

      /*
       * Here, we use FindFirstFile()/FindNextFile() instead of opendir(),
       * qxe_stat(), & friends, because qxe_stat() is VERY expensive in
       * terms of time.  Hence, we take the time to write complicated
       * Win32-specific code, instead of simple Unix-style stuff.
       */
      findex = 0;
      fh = INVALID_HANDLE_VALUE;
      errm = SetErrorMode (SEM_FAILCRITICALERRORS
			   | SEM_NOOPENFILEERRORBOX);

      while (1)
	{
	  Bytecount len;
	  DECLARE_EISTRING (filename);
	  int result;
	  WIN32_FIND_DATAW finddat;
	  Win32_file file;
	  struct syntax_cache scache_struct;
	  struct syntax_cache *scache = &scache_struct;

	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      fh = qxeFindFirstFile (eiextdata (win32pattern), &finddat);
	      if (fh == INVALID_HANDLE_VALUE)
		{
		  SetErrorMode (errm);
		  report_file_error ("Opening directory", dirfile);
		}
	    }
	  else
	    {
	      if (! qxeFindNextFile (fh, &finddat))
		{
		  if (GetLastError() == ERROR_NO_MORE_FILES)
		    {
		      break;
		    }
		  FindClose(fh);
		  SetErrorMode (errm);
		  report_file_error ("Reading directory", dirfile);
		}
	    }

	  file.dwFileAttributes = finddat.dwFileAttributes;
	  file.ftCreationTime = finddat.ftCreationTime;
	  file.ftLastAccessTime = finddat.ftLastAccessTime;
	  file.ftLastWriteTime = finddat.ftLastWriteTime;
	  file.nFileSizeHigh = finddat.nFileSizeHigh;
	  file.nFileSizeLow = finddat.nFileSizeLow;
	  eicpy_ext (filename, (Extbyte *) finddat.cFileName,
	             Qmswindows_tstr);

	  if (!NILP (Vmswindows_downcase_file_names))
	    eilwr (filename);
	  len = eilen (filename);
	  result = (NILP (pattern)
		    || (0 <= re_search (bufp, (char *) eidata (filename), 
					len, 0, len, 0, Qnil, 0, scache)));
	  if (result)
	    {
	      if ( ! (eigetch_char (filename, 0) == '.' &&
		      ((hide_system &&
			(eigetch_char (filename, 1) == '\0' ||
			 (eigetch_char (filename, 1) == '.' &&
			  eigetch_char (filename, 2) == '\0'))) ||
		       hide_dot)))
		{
		  file.cFileName = xnew_ibytes (len + ITEXT_ZTERM_SIZE);
		  memcpy (file.cFileName, eidata (filename), len);
		  file.cFileName[len] = '\0';
		  Dynarr_add (files, file);
		}
	    }
	}
      if (fh != INVALID_HANDLE_VALUE)
	FindClose (fh);
      break;
    }

  SetErrorMode (errm);
  return (files);
}

static Lisp_Object
mswindows_format_file (Win32_file *file, int display_size, int add_newline)
{
  Lisp_Object luser;
  double file_size;
  DECLARE_EISTRING (puta);
  CIbyte buf[666];

  file_size =
    file->nFileSizeHigh * (double)UINT_MAX + file->nFileSizeLow;
#if INDENT_LISTING
  eicat_c (puta, "  ");
#endif
  if (display_size)
    {
      sprintf (buf, "%6d ", (int)((file_size + 1023.) / 1024.));
      eicat_c (puta, buf);
    }
  if (file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    eicat_c (puta, "d");
  else
    eicat_c (puta, "-");
  buf[0] = buf[3] = buf[6] = 'r';
  if (file->dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    buf[1] = buf[4] = buf[7] = '-';
  else
    buf[1] = buf[4] = buf[7] = 'w';
  {
    int is_executable = 0;

    if (file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      is_executable = 1;
    else if (qxestrcharlen (file->cFileName) > 4)
      {
	Ibyte *end = file->cFileName + qxestrlen (file->cFileName);
	DEC_IBYTEPTR (end);
	DEC_IBYTEPTR (end);
	DEC_IBYTEPTR (end);
	DEC_IBYTEPTR (end);
	if (qxestrcasecmp_ascii (end, ".exe") == 0
	    || qxestrcasecmp_ascii (end, ".com") == 0
	    || qxestrcasecmp_ascii (end, ".bat") == 0
#if 0
	    || qxestrcasecmp_ascii (end, ".pif") == 0
#endif
	    )
	  is_executable = 1;
      }
    if (is_executable)
      buf[2] = buf[5] = buf[8] = 'x';
    else
      buf[2] = buf[5] = buf[8] = '-';
  }
  buf[9] = '\0';
  eicat_c (puta, buf);
  if (file->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      eicat_c (puta, "   2 ");
  else
      eicat_c (puta, "   1 ");
  luser = Fuser_login_name (Qnil);
  if (!STRINGP (luser))
    sprintf (buf, "%-9d", 0);
  else
    {
      Ibyte *str;

      str = XSTRING_DATA (luser);
      sprintf (buf, "%-8s ", str);
    }
  eicat_raw (puta, (Ibyte *) buf, strlen (buf));
  {
    CIbyte *cptr = buf;
    sprintf (buf, "%-8d ", getgid ());
    cptr += 9;
    if (file_size > 99999999.0)
      {
	file_size = (file_size + 1023.0) / 1024.;
	if (file_size > 999999.0)
	  sprintf (cptr, "%6.0fMB ", (file_size + 1023.0) / 1024.);
	else
	  sprintf (cptr, "%6.0fKB ", file_size);
      }
    else
      sprintf (cptr, "%8.0f ", file_size);
    while (*cptr)
      ++cptr;
    {
      time_t t, now;
      Ibyte *ctimebuf;

      if (
#if 0
	  /*
	   * This doesn't work.
	   * This code should be correct ...
	   */
	  FileTimeToLocalFileTime (&file->ftLastWriteTime, &localtime) &&
	  ((t = mswindows_convert_time (localtime)) != 0) &&
#else
	  /*
	   * But this code "works" ...
	   */
	  ((t = mswindows_convert_time (file->ftLastWriteTime)) != 0) &&
#endif
	  ((ctimebuf = qxe_ctime (&t)) != NULL))
	{
	  memcpy (cptr, &ctimebuf[4], 7);
	  now = time (NULL);
	  if (now - t > (365. / 2.0) * 86400.)
	    {
	      /* more than 6 months */
	      cptr[7] = ' ';
	      memcpy (&cptr[8], &ctimebuf[20], 4);
	    }
	  else
	    {
	      /* less than 6 months */
	      memcpy (&cptr[7], &ctimebuf[11], 5);
	    }
	  cptr += 12;
	  *cptr++ = ' ';
	  *cptr++ = '\0';
	}
    }
  }

  eicat_c (puta, buf);
  eicat_raw (puta, file->cFileName, qxestrlen (file->cFileName));
  if (add_newline)
    eicat_c (puta, "\n");

  return eimake_string (puta);
}


DEFUN ("mswindows-insert-directory", Fmswindows_insert_directory, 2, 4, 0, /*
Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.
*/
       (file, switches, wildcard, full_directory_p))
{
  Lisp_Object handler, wildpat = Qnil, basename = Qnil;
  int nfiles = 0, i;
  int hide_system = 1, hide_dot = 1, reverse = 0, display_size = 0;
  Win32_file_dynarr *files;
  enum mswindows_sortby sort_by =
    (mswindows_ls_sort_case_insensitive ? MSWINDOWS_SORT_BY_NAME_NOCASE
     : MSWINDOWS_SORT_BY_NAME);
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (file, wildpat, basename);

  CHECK_STRING (file);
  if (!NILP (wildpat))
    CHECK_STRING (wildpat);

  handler = Ffind_file_name_handler (file, Qmswindows_insert_directory);
  if (!NILP (handler))
    {
      UNGCPRO;
      return call5 (handler, Qmswindows_insert_directory, file, switches,
		    wildcard, full_directory_p);
    }

  if (!NILP (switches))
    {
      Ibyte *cptr, *cptr_end;

      CHECK_STRING (switches);
      cptr = XSTRING_DATA (switches);
      cptr_end = cptr + XSTRING_LENGTH (switches);
      while (cptr < cptr_end)
	{
	  Ichar ch = itext_ichar (cptr);
	  switch (ch)
	    {
	    case 'A':
	      hide_dot = 0;
	      break;
	    case 'a':
	      hide_system = 0;
	      hide_dot = 0;
	      break;
	    case 'r':
	      reverse = 1;
	      break;
	    case 's':
	      display_size = 1;
	      break;
	    case 'S':
	      sort_by = MSWINDOWS_SORT_BY_SIZE;
	      break;
	    case 't':
	      sort_by = MSWINDOWS_SORT_BY_MOD_DATE;
	      break;
	    }
	  INC_IBYTEPTR (cptr);
	}
    }

  if (!NILP (wildcard))
    {
      Lisp_Object newfile;

      file = Fdirectory_file_name (file);
      basename = Ffile_name_nondirectory (file);
      wildpat = call1 (Qwildcard_to_regexp, basename);
      newfile = Ffile_name_directory (file);
      if (NILP (newfile))
	newfile = Ffile_name_directory (Fexpand_file_name (file, Qnil));
      file = newfile;
    }
  
  files = mswindows_get_files (file,
			       NILP (wildcard) && NILP (full_directory_p),
			       wildpat, hide_dot, hide_system);

  if (Dynarr_length (files) > 1)
    mswindows_sort_files (files, sort_by, reverse);
  if (!NILP (wildcard) || !NILP (full_directory_p))
    {
      /*
       * By using doubles, we can handle files up to 2^53 bytes in
       * size (IEEE doubles have 53 bits of resolution).  However,
       * as we divide by 1024 (or 2^10), the total size is
       * accurate up to 2^(53+10) --> 2^63 bytes.
       *
       * Hopefully, we won't have to handle these file sizes anytime
       * soon.
       */
      double total_size, file_size, block_size;

      if ((block_size = mswindows_ls_round_file_size) <= 0)
	{
	  block_size = 0;
	}
      total_size = 0;
      for (i = 0; i < Dynarr_length (files); ++i)
	{
	  Win32_file *file = Dynarr_atp (files, i);
	  file_size =
	    file->nFileSizeHigh * (double)UINT_MAX +
	      file->nFileSizeLow;
	  if (block_size > 0)
	    {
	      /*
	       * Round file_size up to the next nearest block size.
	       */
	      file_size =
		floor ((file_size + block_size - 1) / block_size)
		  * block_size;
	    }
	  /* Here, we round to the nearest 1K */
	  total_size += floor ((file_size + 512.) / 1024.);
	}
      {
	write_fmt_string (wrap_buffer (current_buffer),
#if INDENT_LISTING
			  /* ANSI C compilers auto-concatenate adjacent
                             strings */
			  "  "
#endif
			  "total %.0f\n", total_size);
      }
    }
  for (i = 0; i < Dynarr_length (files); ++i)
    {
      struct gcpro ngcpro1;
      Lisp_Object fmtfile =
	mswindows_format_file (Dynarr_atp (files, i), display_size, TRUE);
      NGCPRO1 (fmtfile);
      buffer_insert1 (current_buffer, fmtfile);
      NUNGCPRO;
    }
  for (i = 0; i < Dynarr_length (files); ++i)
    {
      Win32_file *file = Dynarr_atp (files, i);
      xfree (file->cFileName, Ibyte *);
    }
  Dynarr_free (files);

  UNGCPRO;
  return Qnil;
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_dired_mswindows (void)
{
  DEFSYMBOL (Qmswindows_insert_directory);
  DEFSYMBOL (Qwildcard_to_regexp);

  DEFSUBR (Fmswindows_insert_directory);
}


void
vars_of_dired_mswindows (void)
{
  DEFVAR_BOOL ("mswindows-ls-sort-case-insensitive",
	       &mswindows_ls_sort_case_insensitive /*
*Non-nil means filenames are sorted in a case-insensitive fashion.
Nil means filenames are sorted in a case-sensitive fashion, just like Unix.
*/ );
  mswindows_ls_sort_case_insensitive = 1;

  DEFVAR_INT ("mswindows-ls-round-file-size", &mswindows_ls_round_file_size /*
*If non-zero, file sizes are rounded in terms of this block size when
the file totals are being calculated.  This is useful for getting a more
accurate estimate of allocated disk space.  Note that this only affects
the total size calculation; the individual displayed file sizes are not
changed.  This block size should also be a power of 2 (but this is not
enforced), as filesystem block (cluster) sizes are typically powers-of-2.
*/ );
  /*
   * Here, we choose 4096 because it's the cluster size for both FAT32
   * and NTFS (?).  This is probably much too small for people using
   * plain FAT, but, hopefully, plain FAT will go away someday.
   *
   * We should allow something like a alist here, to make the size
   * dependent on the drive letter, etc..
   */
  mswindows_ls_round_file_size = 4096;
}
