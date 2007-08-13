 /* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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
#include "elhash.h"
#include "regex.h"

#include "sysfile.h"
#include "sysdir.h"

Lisp_Object Vcompletion_ignored_extensions;

Lisp_Object Qdirectory_files;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;

DEFUN ("directory-files", Fdirectory_files, 1, 5, 0, /*
Return a list of names of files in DIRECTORY.
There are four optional arguments:
If FULL is non-nil, absolute pathnames of the files are returned.
If MATCH is non-nil, only pathnames containing that regexp are returned.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
If FILES-ONLY is the symbol t, then only the "files" in the directory
 will be returned; subdirectories will be excluded.  If FILES-ONLY is not
 nil and not t, then only the subdirectories will be returned.  Otherwise,
 if FILES-ONLY is nil (the default) then both files and subdirectories will
 be returned.
*/
       (dirname, full, match, nosort, files_only))
{
  /* This function can GC.  GC checked 1997.04.06. */
  DIR *d;
  Bytecount dirname_length;
  Lisp_Object list, name, dirfilename = Qnil;
  Lisp_Object handler;
  struct re_pattern_buffer *bufp = NULL;

  char statbuf [MAXNAMLEN+2];
  char *statbuf_tail;
  Lisp_Object tail_cons = Qnil;
  char slashfilename[MAXNAMLEN+2];
  char *filename = slashfilename;

  struct gcpro gcpro1, gcpro2, gcpro3;
  GCPRO3 (dirname, dirfilename, tail_cons);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname, Qdirectory_files);
  if (!NILP (handler))
  {
    UNGCPRO;
    if (!NILP (files_only))
      return call6 (handler, Qdirectory_files, dirname, full, match, nosort,
                    files_only);
    else
      return call5 (handler, Qdirectory_files, dirname, full, match,
		    nosort);
  }

  /* #### why do we do Fexpand_file_name after file handlers here,
     but earlier everywhere else? */
  dirname = Fexpand_file_name (dirname, Qnil);
  dirfilename = Fdirectory_file_name (dirname);

  {
    /* XEmacs: this should come before the opendir() because it might error. */
    Lisp_Object name_as_dir = Ffile_name_as_directory (dirname);
    CHECK_STRING (name_as_dir);
    memcpy (statbuf, ((char *) XSTRING_DATA (name_as_dir)),
           XSTRING_LENGTH (name_as_dir));
    statbuf_tail = statbuf + XSTRING_LENGTH (name_as_dir);
  }

  /* XEmacs: this should come after Ffile_name_as_directory() to avoid
     potential regexp cache smashage.  This should come before the
     opendir() because it might signal an error.
   */
  if (!NILP (match))
    {
      CHECK_STRING (match);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signalling our own errors, we just call
	 compile_pattern to do the work for us.  */
#ifdef VMS
      bufp =
	compile_pattern (match, 0,
			 (char *) MIRROR_DOWNCASE_TABLE_AS_STRING
			 (XBUFFER (Vbuffer_defaults)), 0, ERROR_ME);
#else
      bufp = compile_pattern (match, 0, 0, 0, ERROR_ME);
#endif
    }

  /* Now *bufp is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  /* Do this opendir after anything which might signal an error; if
     an error is signalled while the directory stream is open, we
     have to make sure it gets closed, and setting up an
     unwind_protect to do so would be a pain.  */
  d = opendir ((char *) XSTRING_DATA (dirfilename));
  if (! d)
    report_file_error ("Opening directory", list1 (dirname));

  list = Qnil;
  tail_cons = Qnil;
  dirname_length = XSTRING_LENGTH (dirname);
#ifndef VMS
  if (dirname_length == 0
      || !IS_ANY_SEP (XSTRING_BYTE (dirname, dirname_length - 1)))
  {
    *filename++ = DIRECTORY_SEP;
    dirname_length++;
  }
#endif /* VMS */

  /* Loop reading blocks */
  while (1)
    {
      DIRENTRY *dp = readdir (d);
      int len;

      if (!dp) break;
      len = NAMLEN (dp);
      if (DIRENTRY_NONEMPTY (dp))
	{
	  int result;
	  Lisp_Object oinhibit_quit = Vinhibit_quit;
	  strncpy (filename, dp->d_name, len);
	  filename[len] = 0;
	  /* re_search can now QUIT, so prevent it to avoid
	     filedesc lossage */
	  Vinhibit_quit = Qt;
	  result = (NILP (match)
	      || (0 <= re_search (bufp, filename, len, 0, len, 0)));
          Vinhibit_quit = oinhibit_quit;
          if (result)
	    {
	      if (!NILP (files_only))
		{
		  int dir_p;
		  struct stat st;

		  memcpy (statbuf_tail, filename, len);
		  statbuf_tail [len] = 0;

		  if (stat (statbuf, &st) < 0)
		    dir_p = 0;
		  else
		    dir_p = ((st.st_mode & S_IFMT) == S_IFDIR);

		  if (EQ (files_only, Qt) && dir_p)
		    continue;
		  else if (!EQ (files_only, Qt) && !dir_p)
		    continue;
		}

	      if (!NILP (full))
		name = concat2 (dirname, build_string (slashfilename));
	      else
		name = make_string ((Bufbyte *) filename, len);

	      if (NILP (tail_cons))
		{
		  list = list1 (name);
		  tail_cons = list;
		}
	      else
		{
		  XCDR (tail_cons) = list1 (name);
		  tail_cons = XCDR (tail_cons);
		}
	    }
	}
    }
  closedir (d);
  UNGCPRO;
  if (!NILP (nosort))
    return list;
  return Fsort (Fnreverse (list), Qstring_lessp);
}

static Lisp_Object file_name_completion (Lisp_Object file,
                                         Lisp_Object dirname,
                                         int all_flag, int ver_flag);

DEFUN ("file-name-completion", Ffile_name_completion, 2, 2, 0, /*
Complete file name FILE in directory DIR.
Returns the longest string common to all filenames in DIR
that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIR contains no name starting with FILE.

Filenames which end with any member of `completion-ignored-extensions'
are not considered as possible completions for FILE unless there is no
other possible completion.  `completion-ignored-extensions' is not applied
to the names of directories.
*/
       (file, dirname))
{
  /* This function can GC.  GC checked 1996.04.06. */
  Lisp_Object handler;

  /* If the directory name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, dirname);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, file, dirname);

  return file_name_completion (file, dirname, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions, 2, 2, 0, /*
Return a list of all completions of file name FILE in directory DIR.
These are all file names in directory DIR which begin with FILE.

Filenames which end with any member of `completion-ignored-extensions'
are not considered as possible completions for FILE unless there is no
other possible completion.  `completion-ignored-extensions' is not applied
to the names of directories.
*/
       (file, dirname))
{
  /* This function can GC. GC checked 1997.06.04. */
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (dirname);
  dirname = Fexpand_file_name (dirname, Qnil);
  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (dirname, Qfile_name_all_completions);
  UNGCPRO;
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, file,
		  dirname);

  return file_name_completion (file, dirname, 1, 0);
}

static int
file_name_completion_stat (Lisp_Object dirname, DIRENTRY *dp,
			   struct stat *st_addr)
{
  Bytecount len = NAMLEN (dp);
  Bytecount pos = XSTRING_LENGTH (dirname);
  int value;
  char *fullname = (char *) alloca (len + pos + 2);

  memcpy (fullname, XSTRING_DATA (dirname), pos);
#ifndef VMS
  if (!IS_DIRECTORY_SEP (fullname[pos - 1]))
    fullname[pos++] = DIRECTORY_SEP;
#endif

  memcpy (fullname + pos, dp->d_name, len);
  fullname[pos + len] = 0;

#ifdef S_IFLNK
  /* We want to return success if a link points to a nonexistent file,
     but we want to return the status for what the link points to,
     in case it is a directory.  */
  value = lstat (fullname, st_addr);
  if (S_ISLNK (st_addr->st_mode))
    stat (fullname, st_addr);
#else
  value = stat (fullname, st_addr);
#endif
  return value;
}

static Lisp_Object
file_name_completion (Lisp_Object file, Lisp_Object dirname, int all_flag,
		      int ver_flag)
{
  /* This function can GC */
  DIR *d = 0;
  int matchcount = 0;
  Lisp_Object bestmatch = Qnil;
  Charcount bestmatchsize = 0;
  struct stat st;
  int passcount;
  int speccount = specpdl_depth ();
  Charcount file_name_length;
  DIRENTRY *((*readfunc) (DIR *)) = readdir;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (file, dirname, bestmatch);

  CHECK_STRING (file);

#ifdef VMS
  /* Filename completion on VMS ignores case, since VMS filesys does.  */
  specbind (Qcompletion_ignore_case, Qt);

  if (ver_flag)
    readfunc = readdirver;
#endif /* VMS */

#ifdef FILE_SYSTEM_CASE
  file = FILE_SYSTEM_CASE (file);
#endif
  dirname = Fexpand_file_name (dirname, Qnil);
  file_name_length = string_char_length (XSTRING (file));

  /* With passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.

     ** It would not actually be helpful to the user to ignore any possible
     completions when making a list of them.**  */

  for (passcount = !!all_flag; NILP (bestmatch) && passcount < 2; passcount++)
    {
      d = opendir ((char *) XSTRING_DATA (Fdirectory_file_name (dirname)));
      if (!d)
	report_file_error ("Opening directory", list1 (dirname));

      /* Loop reading blocks */
      while (1)
	{
	  DIRENTRY *dp;
	  Bytecount len;
	  /* scmp() works in characters, not bytes, so we have to compute
	     this value: */
	  Charcount cclen;
          int directoryp;
          int ignored_extension_p = 0;
	  Bufbyte *d_name;

	  dp = (*readfunc) (d);
	  if (!dp) break;

	  d_name = (Bufbyte *) dp->d_name;
	  len = NAMLEN (dp);
	  cclen = bytecount_to_charcount (d_name, len);

	  /* Can't just use QUIT because we have to make sure the file
             descriptor gets closed. */
	  if (QUITP)
	    {
	      closedir (d);
	      signal_quit ();
	    }

	  if (! DIRENTRY_NONEMPTY (dp)
	      || cclen < file_name_length
	      || 0 <= scmp (d_name, XSTRING_DATA (file), file_name_length))
	    continue;

          if (file_name_completion_stat (dirname, dp, &st) < 0)
            continue;

          directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
          if (directoryp)
	    {
#ifndef TRIVIAL_DIRECTORY_ENTRY
#define TRIVIAL_DIRECTORY_ENTRY(n) (!strcmp (n, ".") || !strcmp (n, ".."))
#endif
	      /* "." and ".." are never interesting as completions, but are
		 actually in the way in a directory containing only one file.  */
	      if (!passcount && TRIVIAL_DIRECTORY_ENTRY (dp->d_name))
		continue;
	    }
	  else
            {
	      /* Compare extensions-to-be-ignored against end of this file name */
	      /* if name is not an exact match against specified string.  */
	      if (!passcount && cclen > file_name_length)
		{
		  Lisp_Object tem;
		  /* and exit this for loop if a match is found */
		  for (tem = Vcompletion_ignored_extensions;
		       CONSP (tem);
                       tem = XCDR (tem))
		    {
		      Lisp_Object elt = XCAR (tem);
		      Charcount skip;

		      if (!STRINGP (elt)) continue;
		      skip = cclen - string_char_length (XSTRING (elt));
		      if (skip < 0) continue;

		      if (0 > scmp (charptr_n_addr (d_name, skip),
				    XSTRING_DATA (elt),
				    string_char_length (XSTRING (elt))))
			{
			  ignored_extension_p = 1;
			  break;
			}
		    }
		}
	    }

	  /* If an ignored-extensions match was found,
	     don't process this name as a completion.  */
	  if (!passcount && ignored_extension_p)
	    continue;

	  if (!passcount && regexp_ignore_completion_p (d_name, Qnil, 0, cclen))
            continue;

          /* Update computation of how much all possible completions match */
          matchcount++;

          if (all_flag || NILP (bestmatch))
            {
              Lisp_Object name = Qnil;
              struct gcpro ngcpro1;
              NGCPRO1 (name);
              /* This is a possible completion */
              name = make_string (d_name, len);
              if (directoryp) /* Completion is a directory; end it with '/' */
                name = Ffile_name_as_directory (name);
              if (all_flag)
                {
                  bestmatch = Fcons (name, bestmatch);
                }
              else
                {
                  bestmatch = name;
                  bestmatchsize = string_char_length (XSTRING (name));
                }
              NUNGCPRO;
            }
          else
            {
              Charcount compare = min (bestmatchsize, cclen);
              Bufbyte *p1 = XSTRING_DATA (bestmatch);
              Bufbyte *p2 = d_name;
              Charcount matchsize = scmp (p1, p2, compare);

              if (matchsize < 0)
                matchsize = compare;
              if (completion_ignore_case)
                {
                  /* If this is an exact match except for case,
                     use it as the best match rather than one that is not
                     an exact match.  This way, we get the case pattern
                     of the actual match.  */
                  if ((matchsize == cclen
                       && matchsize + !!directoryp
                       < string_char_length (XSTRING (bestmatch)))
                      ||
                      /* If there is no exact match ignoring case,
                         prefer a match that does not change the case
                         of the input.  */
                      (((matchsize == cclen)
                        ==
                        (matchsize + !!directoryp
                         == string_char_length (XSTRING (bestmatch))))
                       /* If there is more than one exact match aside from
                          case, and one of them is exact including case,
                          prefer that one.  */
                       && 0 > scmp_1 (p2, XSTRING_DATA (file),
				      file_name_length, 0)
                       && 0 <= scmp_1 (p1, XSTRING_DATA (file),
				       file_name_length, 0)))
                    {
                      bestmatch = make_string (d_name, len);
                      if (directoryp)
                        bestmatch = Ffile_name_as_directory (bestmatch);
                    }
                }

              /* If this dirname all matches,
                 see if implicit following slash does too.  */
              if (directoryp
                  && compare == matchsize
                  && bestmatchsize > matchsize
                  && IS_ANY_SEP (charptr_emchar_n (p1, matchsize)))
                matchsize++;
              bestmatchsize = matchsize;
            }
        }
      closedir (d);
    }

  unbind_to (speccount, Qnil);

  UNGCPRO;

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == file_name_length)
    return Qt;
  return Fsubstring (bestmatch, make_int (0), make_int (bestmatchsize));
}


Lisp_Object
make_directory_hash_table (char *path)
{
  DIR *d;
  DIRENTRY *dp;
  Bytecount len;
  Lisp_Object hash = make_lisp_hashtable (100, HASHTABLE_NONWEAK,
					  HASHTABLE_EQUAL);
  if ((d = opendir (path)))
    {
      while ((dp = readdir (d)))
	{
	  len = NAMLEN (dp);
	  if (DIRENTRY_NONEMPTY (dp))
	    Fputhash (make_string ((Bufbyte *) dp->d_name, len), Qt, hash);
	}
      closedir (d);
    }
  return hash;
}

#ifdef VMS

DEFUN ("file-name-all-versions", Ffile_name_all_versions, 2, 2, 0, /*
Return a list of all versions of file name FILE in directory DIR.
*/
       (file, dirname))
{
  /* This function can GC */
  return file_name_completion (file, dirname, 1, 1);
}

DEFUN ("file-version-limit", Ffile_version_limit, 1, 1, 0, /*
Return the maximum number of versions allowed for FILE.
Returns nil if the file cannot be opened or if there is no version limit.
*/
       (filename))
{
  /* This function can GC */
  Lisp_Object retval;
  struct FAB    fab;
  struct RAB    rab;
  struct XABFHC xabfhc;
  int status;

  filename = Fexpand_file_name (filename, Qnil);
  CHECK_STRING (filename);
  fab      = cc$rms_fab;
  xabfhc   = cc$rms_xabfhc;
  fab.fab$l_fna = XSTRING_DATA (filename);
  fab.fab$b_fns = strlen (fab.fab$l_fna);
  fab.fab$l_xab = (char *) &xabfhc;
  status = sys$open (&fab, 0, 0);
  if (status != RMS$_NORMAL)	/* Probably non-existent file */
    return Qnil;
  sys$close (&fab, 0, 0);
  if (xabfhc.xab$w_verlimit == 32767)
    return Qnil;		/* No version limit */
  else
    return make_int (xabfhc.xab$w_verlimit);
}

#endif /* VMS */


Lisp_Object
wasteful_word_to_lisp (unsigned int item)
{
  /* Compatibility: in other versions, file-attributes returns a LIST
     of two 16 bit integers... */
  Lisp_Object cons = word_to_lisp (item);
  XCDR (cons) = Fcons (XCDR (cons), Qnil);
  return cons;
}

DEFUN ("file-attributes", Ffile_attributes, 1, 1, 0, /*
Return a list of attributes of file FILENAME.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes. (-1, if number is out of range).
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil.
*/
       (filename))
{
  /* This function can GC. GC checked 1997.06.04. */
  Lisp_Object values[12];
  Lisp_Object dirname = Qnil;
  struct stat s;
  char modes[10];
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, dirname);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_attributes);
  if (!NILP (handler))
    {
      UNGCPRO;
      return call2 (handler, Qfile_attributes, filename);
    }

  if (lstat ((char *) XSTRING_DATA (filename), &s) < 0)
    {
      UNGCPRO;
      return Qnil;
    }

#ifdef BSD4_2
  dirname = Ffile_name_directory (filename);
#endif

#ifdef MSDOS
  {
    char *tmpnam = (char *) XSTRING_DATA (Ffile_name_nondirectory (filename));
    int l = strlen (tmpnam);

    if (l >= 5
	&& S_ISREG (s.st_mode)
	&& (stricmp (&tmpnam[l - 4], ".com") == 0 ||
	    stricmp (&tmpnam[l - 4], ".exe") == 0 ||
	    stricmp (&tmpnam[l - 4], ".bat") == 0))
      {
	s.st_mode |= S_IEXEC;
      }
  }
#endif /* MSDOS */

  switch (s.st_mode & S_IFMT)
    {
    default:
      values[0] = Qnil;
      break;
    case S_IFDIR:
      values[0] = Qt;
      break;
#ifdef S_IFLNK
    case S_IFLNK:
      values[0] = Ffile_symlink_p (filename);
      break;
#endif
    }
  values[1] = make_int (s.st_nlink);
  values[2] = make_int (s.st_uid);
  values[3] = make_int (s.st_gid);
  values[4] = wasteful_word_to_lisp (s.st_atime);
  values[5] = wasteful_word_to_lisp (s.st_mtime);
  values[6] = wasteful_word_to_lisp (s.st_ctime);
  values[7] = make_int ((EMACS_INT) s.st_size);
  /* If the size is out of range, give back -1.  */
  /* #### Fix when Emacs gets bignums! */
  if (XINT (values[7]) != s.st_size)
    XSETINT (values[7], -1);
  filemodestring (&s, modes);
  values[8] = make_string ((Bufbyte *) modes, 10);
#if defined (BSD4_2) || defined (BSD4_3)	/* file gid will be dir gid */
  {
    struct stat sdir;

    if (!NILP (dirname) && stat ((char *) XSTRING_DATA (dirname), &sdir) == 0)
      values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
    else                        /* if we can't tell, assume worst */
      values[9] = Qt;
  }
#else                           /* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 or BSD4_3 */
  values[10] = make_int (s.st_ino);
  values[11] = make_int (s.st_dev);
  UNGCPRO;
  return Flist (countof (values), values);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_dired (void)
{
  defsymbol (&Qdirectory_files, "directory-files");
  defsymbol (&Qfile_name_completion, "file-name-completion");
  defsymbol (&Qfile_name_all_completions, "file-name-all-completions");
  defsymbol (&Qfile_attributes, "file-attributes");

  DEFSUBR (Fdirectory_files);
  DEFSUBR (Ffile_name_completion);
#ifdef VMS
  DEFSUBR (Ffile_name_all_versions);
  DEFSUBR (Ffile_version_limit);
#endif /* VMS */
  DEFSUBR (Ffile_name_all_completions);
  DEFSUBR (Ffile_attributes);
}

void
vars_of_dired (void)
{
  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions /*
*Completion ignores filenames ending in any string in this list.
This variable does not affect lists of possible completions,
but does affect the commands that actually do completions.
It is used by the functions `file-name-completion' and
`file-name-all-completions'.
*/ );
  Vcompletion_ignored_extensions = Qnil;
}
