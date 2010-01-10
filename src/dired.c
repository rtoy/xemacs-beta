 /* Lisp functions for making directory listings.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
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

/* Synched up with: FSF 19.30. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "elhash.h"
#include "opaque.h"
#include "regex.h"
#include "syntax.h"
#include "sysdep.h"

#include "sysdir.h"
#include "sysfile.h"
#include "syspwd.h"
#include "systime.h"

#ifdef WIN32_NATIVE
#include "syswindows.h"
#endif

Lisp_Object Vcompletion_ignored_extensions;
Lisp_Object Qdirectory_files;
Lisp_Object Qfile_name_completion;
Lisp_Object Qfile_name_all_completions;
Lisp_Object Qfile_attributes;

static Lisp_Object
close_directory_unwind (Lisp_Object unwind_obj)
{
  DIR *d = (DIR *)get_opaque_ptr (unwind_obj);
  qxe_closedir (d);
  free_opaque_ptr (unwind_obj);
  return Qnil;
}

DEFUN ("directory-files", Fdirectory_files, 1, 5, 0, /*
Return a list of names of files in DIRECTORY.
There are four optional arguments:
If FULL is non-nil, absolute pathnames of the files are returned.
If MATCH is non-nil, only pathnames whose basename contain that regexp are
 returned.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
If FILES-ONLY is the symbol t, then only the "files" in the directory
 will be returned; subdirectories will be excluded.  If FILES-ONLY is not
 nil and not t, then only the subdirectories will be returned.  Otherwise,
 if FILES-ONLY is nil (the default) then both files and subdirectories will
 be returned.
*/
       (directory, full, match, nosort, files_only))
{
  /* This function can GC */
  DIR *d;
  Lisp_Object list = Qnil;
  Bytecount directorylen;
  Lisp_Object handler;
  struct re_pattern_buffer *bufp = NULL;
  int speccount = specpdl_depth ();
  Ibyte *statbuf, *statbuf_tail;

  struct gcpro gcpro1, gcpro2;
  GCPRO2 (directory, list);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_files);
  if (!NILP (handler))
    {
      UNGCPRO;
      if (!NILP (files_only))
	return call6 (handler, Qdirectory_files, directory, full, match,
		      nosort, files_only);
      else
	return call5 (handler, Qdirectory_files, directory, full, match,
		      nosort);
    }

  /* #### why do we do Fexpand_file_name after file handlers here,
     but earlier everywhere else? */
  directory = Fexpand_file_name (directory, Qnil);
  directory = Ffile_name_as_directory (directory);
  directorylen = XSTRING_LENGTH (directory);

  statbuf = alloca_ibytes (directorylen + MAXNAMLEN + 1);
  memcpy (statbuf, XSTRING_DATA (directory), directorylen);
  statbuf_tail = statbuf + directorylen;

  /* XEmacs: this should come after Ffile_name_as_directory() to avoid
     potential regexp cache smashage.  It comes before the opendir()
     because it might signal an error.  */
  if (!NILP (match))
    {
      CHECK_STRING (match);

      /* MATCH might be a flawed regular expression.  Rather than
	 catching and signalling our own errors, we just call
	 compile_pattern to do the work for us.  */
      bufp = compile_pattern (match, 0, Qnil, Qnil, 0, 0, ERROR_ME);
    }

  /* Now *bufp is the compiled form of MATCH; don't call anything
     which might compile a new regexp until we're done with the loop!  */

  /* Do this opendir after anything which might signal an error.
     NOTE: the above comment is old; previously, there was no
     unwind-protection in case of error, but now there is.  */
  d = qxe_opendir (XSTRING_DATA (directory));
  if (!d)
    report_file_error ("Opening directory", directory);

  record_unwind_protect (close_directory_unwind, make_opaque_ptr ((void *)d));

  /* Loop reading blocks */
  while (1)
    {
      DIRENTRY *dp = qxe_readdir (d);
      int len;
      struct syntax_cache scache_struct;
      struct syntax_cache *scache = &scache_struct;

      if (!dp)
	break;
      len = NAMLEN (dp);
      if (DIRENTRY_NONEMPTY (dp)
	  && (NILP (match)
	      || (0 <= re_search (bufp, dp->d_name, len, 0, len, 0, Qnil, 0,
				  scache))))
	{
	  if (!NILP (files_only))
	    {
	      struct stat st;
	      int dir_p = 0;

	      memcpy (statbuf_tail, dp->d_name, len);
	      statbuf_tail[len] = 0;

	      if (qxe_stat (statbuf, &st) == 0
		  && (st.st_mode & S_IFMT) == S_IFDIR)
		dir_p = 1;

	      if (EQ (files_only, Qt) && dir_p)
		continue;
	      else if (!EQ (files_only, Qt) && !dir_p)
		continue;
	    }

	  {
	    Lisp_Object name =
	      make_string ((Ibyte *)dp->d_name, len);
	    if (!NILP (full))
	      name = concat2 (directory, name);

	    list = Fcons (name, list);
	  }
	}
    }
  unbind_to (speccount);	/* This will close the dir */

  if (NILP (nosort))
    list = Fsort (Fnreverse (list), Qstring_lessp);

  RETURN_UNGCPRO (list);
}

static Lisp_Object file_name_completion (Lisp_Object file,
                                         Lisp_Object directory,
                                         int all_flag, int ver_flag);

DEFUN ("file-name-completion", Ffile_name_completion, 2, 2, 0, /*
Complete file name PARTIAL-FILENAME in directory DIRECTORY.
Return the longest prefix common to all file names in DIRECTORY
that start with PARTIAL-FILENAME.
If there is only one and PARTIAL-FILENAME matches it exactly, return t.
Return nil if DIRECTORY contains no name starting with PARTIAL-FILENAME.

File names which end with any member of `completion-ignored-extensions'
are not considered as possible completions for PARTIAL-FILENAME unless
there is no other possible completion. `completion-ignored-extensions'
is not applied to the names of directories.
*/
       (partial_filename, directory))
{
  /* This function can GC.  GC checked 1996.04.06. */
  Lisp_Object handler;

  /* If the directory name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, partial_filename, directory);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (partial_filename, Qfile_name_completion);
  if (!NILP (handler))
    return call3 (handler, Qfile_name_completion, partial_filename, directory);

  return file_name_completion (partial_filename, directory, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions, 2, 2, 0, /*
Return a list of all completions of PARTIAL-FILENAME in DIRECTORY.
These are all file names in DIRECTORY which begin with PARTIAL-FILENAME.
*/
       (partial_filename, directory))
{
  /* This function can GC. GC checked 1997.06.04. */
  Lisp_Object handler;
  struct gcpro gcpro1;

  GCPRO1 (directory);
  directory = Fexpand_file_name (directory, Qnil);
  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qfile_name_all_completions);
  UNGCPRO;
  if (!NILP (handler))
    return call3 (handler, Qfile_name_all_completions, partial_filename,
		  directory);

  return file_name_completion (partial_filename, directory, 1, 0);
}

static int
file_name_completion_stat (Lisp_Object directory, DIRENTRY *dp,
			   struct stat *st_addr)
{
  Bytecount len = NAMLEN (dp);
  Bytecount pos = XSTRING_LENGTH (directory);
  int value;
  Ibyte *fullname = alloca_ibytes (len + pos + 2);

  memcpy (fullname, XSTRING_DATA (directory), pos);
  if (!IS_DIRECTORY_SEP (fullname[pos - 1]))
    fullname[pos++] = DIRECTORY_SEP;

  memcpy (fullname + pos, dp->d_name, len);
  fullname[pos + len] = 0;

#ifdef S_IFLNK
  /* We want to return success if a link points to a nonexistent file,
     but we want to return the status for what the link points to,
     in case it is a directory.  */
  value = qxe_lstat (fullname, st_addr);
  if (S_ISLNK (st_addr->st_mode))
    qxe_stat (fullname, st_addr);
#else
  value = qxe_stat (fullname, st_addr);
#endif
  return value;
}

static Lisp_Object
file_name_completion_unwind (Lisp_Object locative)
{
  DIR *d;
  Lisp_Object obj = XCAR (locative);

  if (!NILP (obj))
    {
      d = (DIR *)get_opaque_ptr (obj);
      qxe_closedir (d);
      free_opaque_ptr (obj);
    }
  free_cons (locative);
  return Qnil;
}

static Lisp_Object
file_name_completion (Lisp_Object file, Lisp_Object directory, int all_flag,
		      int UNUSED (ver_flag))
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
  Lisp_Object locative;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (file, directory, bestmatch);

  CHECK_STRING (file);

#ifdef WIN32_NATIVE
  /* Filename completion on Windows ignores case, since Windows
     filesystems do.  */
  specbind (Qcompletion_ignore_case, Qt);
#endif /* WIN32_NATIVE */

#ifdef FILE_SYSTEM_CASE
  file = FILE_SYSTEM_CASE (file);
#endif
  directory = Fexpand_file_name (directory, Qnil);
  file_name_length = string_char_length (file);

  /* With passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.

     ** It would not actually be helpful to the user to ignore any possible
     completions when making a list of them.**  */

  /* We cannot use close_directory_unwind() because we change the
     directory.  The old code used to just avoid signaling errors, and
     call closedir, but it was wrong, because it made sane handling of
     QUIT impossible and, besides, various utility functions like
     regexp_ignore_completion_p can signal errors.  */
  locative = noseeum_cons (Qnil, Qnil);
  record_unwind_protect (file_name_completion_unwind, locative);

  for (passcount = !!all_flag; NILP (bestmatch) && passcount < 2; passcount++)
    {
      d = qxe_opendir (XSTRING_DATA (Fdirectory_file_name (directory)));
      if (!d)
	report_file_error ("Opening directory", directory);
      XCAR (locative) = make_opaque_ptr ((void *)d);

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
	  Ibyte *d_name;

	  dp = qxe_readdir (d);
	  if (!dp) break;

	  /* Cast to Ibyte* is OK, as qxe_readdir() Mule-encapsulates.  */
	  d_name = (Ibyte *) dp->d_name;
	  len = NAMLEN (dp);
	  cclen = bytecount_to_charcount (d_name, len);

	  QUIT;

	  if (! DIRENTRY_NONEMPTY (dp)
	      || cclen < file_name_length
	      || 0 <= scmp (d_name, XSTRING_DATA (file), file_name_length))
	    continue;

          if (file_name_completion_stat (directory, dp, &st) < 0)
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
		  /* and exit this for loop if a match is found */
		  EXTERNAL_LIST_LOOP_2 (elt, Vcompletion_ignored_extensions)
		    {
		      Charcount skip;

		      CHECK_STRING (elt);

		      skip = cclen - string_char_length (elt);
		      if (skip < 0) continue;

		      if (0 > scmp (itext_n_addr (d_name, skip),
				    XSTRING_DATA (elt),
				    string_char_length (elt)))
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

	  if (!passcount && regexp_ignore_completion_p (d_name, Qnil, 0, len))
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
                  bestmatchsize = string_char_length (name);
                }
              NUNGCPRO;
            }
          else
            {
              Charcount compare = min (bestmatchsize, cclen);
              Ibyte *p1 = XSTRING_DATA (bestmatch);
              Ibyte *p2 = d_name;
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
                       < string_char_length (bestmatch))
                      ||
                      /* If there is no exact match ignoring case,
                         prefer a match that does not change the case
                         of the input.  */
                      (((matchsize == cclen)
                        ==
                        (matchsize + !!directoryp
                         == string_char_length (bestmatch)))
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

              /* If this directory all matches,
                 see if implicit following slash does too.  */
              if (directoryp
                  && compare == matchsize
                  && bestmatchsize > matchsize
                  && IS_ANY_SEP (itext_ichar_n (p1, matchsize)))
                matchsize++;
              bestmatchsize = matchsize;
            }
        }
      qxe_closedir (d);
      free_opaque_ptr (XCAR (locative));
      XCAR (locative) = Qnil;
    }

  unbind_to (speccount);

  UNGCPRO;

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == file_name_length)
    return Qt;
  return Fsubstring (bestmatch, Qzero, make_int (bestmatchsize));
}


static Lisp_Object user_name_completion (Lisp_Object user,
                                         int all_flag,
                                         int *uniq);

DEFUN ("user-name-completion", Fuser_name_completion, 1, 1, 0, /*
Complete user name from PARTIAL-USERNAME.
Return the longest prefix common to all user names starting with
PARTIAL-USERNAME.  If there is only one and PARTIAL-USERNAME matches
it exactly, returns t.  Return nil if there is no user name starting
with PARTIAL-USERNAME.
*/
       (partial_username))
{
  return user_name_completion (partial_username, 0, NULL);
}

DEFUN ("user-name-completion-1", Fuser_name_completion_1, 1, 1, 0, /*
Complete user name from PARTIAL-USERNAME.

This function is identical to `user-name-completion', except that
the cons of the completion and an indication of whether the
completion was unique is returned.

The car of the returned value is the longest prefix common to all user
names that start with PARTIAL-USERNAME.  If there is only one and
PARTIAL-USERNAME matches it exactly, the car is t.  The car is nil if
there is no user name starting with PARTIAL-USERNAME.  The cdr of the
result is non-nil if and only if the completion returned in the car
was unique.
*/
       (partial_username))
{
  int uniq;
  Lisp_Object completed = user_name_completion (partial_username, 0, &uniq);
  return Fcons (completed, uniq ? Qt : Qnil);
}

DEFUN ("user-name-all-completions", Fuser_name_all_completions, 1, 1, 0, /*
Return a list of all user name completions from PARTIAL-USERNAME.
These are all the user names which begin with PARTIAL-USERNAME.
*/
       (partial_username))
{
  return user_name_completion (partial_username, 1, NULL);
}

struct user_name
{
  Ibyte *ptr;
  Bytecount len;
};

struct user_cache
{
  struct user_name *user_names;
  int length;
  int size;
  EMACS_TIME last_rebuild_time;
};
static struct user_cache user_cache;

static void
free_user_cache (struct user_cache *cache)
{
  int i;
  for (i = 0; i < cache->length; i++)
    xfree (cache->user_names[i].ptr, Ibyte *);
  xfree (cache->user_names, struct user_name *);
  xzero (*cache);
}

static Lisp_Object
user_name_completion_unwind (Lisp_Object cache_incomplete_p)
{
#ifndef WIN32_NATIVE
  endpwent ();
  speed_up_interrupts ();
#endif

  if (! NILP (XCAR (cache_incomplete_p)))
    free_user_cache (&user_cache);

  free_cons (cache_incomplete_p);

  return Qnil;
}

#define  USER_CACHE_TTL  (24*60*60)  /* Time to live: 1 day, in seconds */

static Lisp_Object
user_name_completion (Lisp_Object user, int all_flag, int *uniq)
{
  /* This function can GC */
  int matchcount = 0;
  Lisp_Object bestmatch = Qnil;
  Charcount bestmatchsize = 0;
  Charcount user_name_length;
  EMACS_TIME t;
  int i;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (user, bestmatch);

  CHECK_STRING (user);

  user_name_length = string_char_length (user);

  /* Cache user name lookups because it tends to be quite slow.
   * Rebuild the cache occasionally to catch changes */
  EMACS_GET_TIME (t);
  if (user_cache.user_names &&
      (EMACS_SECS (t) - EMACS_SECS (user_cache.last_rebuild_time)
       > USER_CACHE_TTL))
    free_user_cache (&user_cache);

  if (!user_cache.user_names)
    {
#ifndef WIN32_NATIVE
      struct passwd *pwd;
#else
      DWORD entriesread;
      DWORD totalentries;
      DWORD resume_handle = 0;
#endif

      Lisp_Object cache_incomplete_p = noseeum_cons (Qt, Qnil);
      int speccount = specpdl_depth ();

      record_unwind_protect (user_name_completion_unwind, cache_incomplete_p);
#ifndef WIN32_NATIVE
      slow_down_interrupts ();
      setpwent ();
      while ((pwd = qxe_getpwent ()))
        {
          QUIT;
	  DO_REALLOC (user_cache.user_names, user_cache.size,
		      user_cache.length + 1, struct user_name);
	  user_cache.user_names[user_cache.length].ptr =
	    (Ibyte *) xstrdup (pwd->pw_name);
	  user_cache.user_names[user_cache.length].len = strlen (pwd->pw_name);
	  user_cache.length++;
        }
#else
      if (xNetUserEnum)
	{
	  do
	    {
	      USER_INFO_0 *bufptr;
	      NET_API_STATUS status_status_statui_statum_statu;
	      int i;

	      QUIT;
	      status_status_statui_statum_statu =
		xNetUserEnum (NULL, 0, 0, (LPBYTE *) &bufptr, 1024,
			      &entriesread, &totalentries, &resume_handle);
	      if (status_status_statui_statum_statu != NERR_Success &&
		  status_status_statui_statum_statu != ERROR_MORE_DATA)
		invalid_operation ("Error enumerating users",
				   make_int (GetLastError ()));
	      for (i = 0; i < (int) entriesread; i++)
		{
		  DO_REALLOC (user_cache.user_names, user_cache.size,
			      user_cache.length + 1, struct user_name);
		  TO_INTERNAL_FORMAT (C_STRING,
				      bufptr[i].usri0_name,
				      MALLOC,
				      (user_cache.
				       user_names[user_cache.length].ptr,
				       user_cache.
				       user_names[user_cache.length].len),
				      Qmswindows_unicode);
		  user_cache.length++;
		}
	      xNetApiBufferFree (bufptr);
	    }
	  while (entriesread != totalentries);
	}
      else /* Win 9x */
	{
	  Extbyte name[2 * (UNLEN + 1)];
	  DWORD length = sizeof (name);
	  
	  if (qxeGetUserName (name, &length))
	    {
	      DO_REALLOC (user_cache.user_names, user_cache.size,
			  user_cache.length + 1, struct user_name);
	      TO_INTERNAL_FORMAT (C_STRING, name,
				  MALLOC,
				  (user_cache.
				   user_names[user_cache.length].ptr,
				   user_cache.
				   user_names[user_cache.length].len),
				  Qmswindows_tstr);
	      user_cache.length++;
	    }
	}
#endif

      XCAR (cache_incomplete_p) = Qnil;
      unbind_to (speccount);

      EMACS_GET_TIME (user_cache.last_rebuild_time);
    }

  for (i = 0; i < user_cache.length; i++)
    {
      Ibyte *u_name = user_cache.user_names[i].ptr;
      Bytecount len   = user_cache.user_names[i].len;
      /* scmp() works in chars, not bytes, so we have to compute this: */
      Charcount cclen = bytecount_to_charcount (u_name, len);

      QUIT;

      if (cclen < user_name_length
	  || 0 <= scmp_1 (u_name, XSTRING_DATA (user), user_name_length, 0))
        continue;

      matchcount++;    /* count matching completions */

      if (all_flag || NILP (bestmatch))
        {
          Lisp_Object name = Qnil;
          struct gcpro ngcpro1;
          NGCPRO1 (name);
          /* This is a possible completion */
          name = make_string (u_name, len);
          if (all_flag)
            {
              bestmatch = Fcons (name, bestmatch);
            }
          else
            {
              bestmatch = name;
              bestmatchsize = string_char_length (name);
            }
          NUNGCPRO;
        }
      else
        {
          Charcount compare = min (bestmatchsize, cclen);
          Ibyte *p1 = XSTRING_DATA (bestmatch);
          Ibyte *p2 = u_name;
          Charcount matchsize = scmp_1 (p1, p2, compare, 0);

          if (matchsize < 0)
            matchsize = compare;

          bestmatchsize = matchsize;
        }
    }

  UNGCPRO;

  if (uniq)
    *uniq = (matchcount == 1);

  if (all_flag || NILP (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == user_name_length)
    return Qt;
  return Fsubstring (bestmatch, Qzero, make_int (bestmatchsize));
}


Lisp_Object
make_directory_hash_table (const Ibyte *path)
{
  DIR *d;
  if ((d = qxe_opendir (path)))
    {
      DIRENTRY *dp;
      Lisp_Object hash =
	make_lisp_hash_table (20, HASH_TABLE_NON_WEAK, HASH_TABLE_EQUAL);

      while ((dp = qxe_readdir (d)))
	{
	  Bytecount len = NAMLEN (dp);
	  if (DIRENTRY_NONEMPTY (dp))
	    /* Cast to Ibyte* is OK, as qxe_readdir() Mule-encapsulates.  */
	    Fputhash (make_string ((Ibyte *) dp->d_name, len), Qt, hash);
	}
      qxe_closedir (d);
      return hash;
    }
  else
    return Qnil;
}

#if 0
/* ... never used ... should use list2 directly anyway ... */
/* NOTE: This function can never return a negative value. */
Lisp_Object
wasteful_word_to_lisp (unsigned int item)
{
  /* Compatibility: in other versions, file-attributes returns a LIST
     of two 16 bit integers... */
  Lisp_Object cons = word_to_lisp (item);
  XCDR (cons) = Fcons (XCDR (cons), Qnil);
  return cons;
}
#endif

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
 7. Size in bytes. (-1, if number out of range and no bignum support.)
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
  Lisp_Object directory = Qnil;
  struct stat s;
  char modes[10];
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, directory);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_attributes);
  if (!NILP (handler))
    {
      UNGCPRO;
      return call2 (handler, Qfile_attributes, filename);
    }

  if (qxe_lstat (XSTRING_DATA (filename), &s) < 0)
    {
      UNGCPRO;
      return Qnil;
    }

#ifdef BSD4_2
  directory = Ffile_name_directory (filename);
#endif

#if 0 /* #### shouldn't this apply to WIN32_NATIVE and maybe CYGWIN? */
  {
    Ibyte *tmpnam = XSTRING_DATA (Ffile_name_nondirectory (filename));
    Bytecount l = qxestrlen (tmpnam);

    if (l >= 5
	&& S_ISREG (s.st_mode)
	&& (qxestrcasecmp (&tmpnam[l - 4], ".com") == 0 ||
	    qxestrcasecmp (&tmpnam[l - 4], ".exe") == 0 ||
	    qxestrcasecmp (&tmpnam[l - 4], ".bat") == 0))
      {
	s.st_mode |= S_IEXEC;
      }
  }
#endif

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
  values[4] = make_time (s.st_atime);
  values[5] = make_time (s.st_mtime);
  values[6] = make_time (s.st_ctime);

#ifndef HAVE_BIGNUM
  values[7] = make_integer (NUMBER_FITS_IN_AN_EMACS_INT (s.st_size) ? 
                            (EMACS_INT)s.st_size : -1);
#else
  values[7] = make_integer (s.st_size);
#endif 

  filemodestring (&s, modes);
  values[8] = make_string ((Ibyte *) modes, 10);
#if defined (BSD4_2) || defined (BSD4_3)	/* file gid will be dir gid */
  {
    struct stat sdir;

    if (!NILP (directory) && qxe_stat (XSTRING_DATA (directory), &sdir) == 0)
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
  DEFSYMBOL (Qdirectory_files);
  DEFSYMBOL (Qfile_name_completion);
  DEFSYMBOL (Qfile_name_all_completions);
  DEFSYMBOL (Qfile_attributes);

  DEFSUBR (Fdirectory_files);
  DEFSUBR (Ffile_name_completion);
  DEFSUBR (Ffile_name_all_completions);
  DEFSUBR (Fuser_name_completion);
  DEFSUBR (Fuser_name_completion_1);
  DEFSUBR (Fuser_name_all_completions);
  DEFSUBR (Ffile_attributes);
}

void
vars_of_dired (void)
{
  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions /*
*Completion ignores filenames ending in any string in this list.
This variable does not affect lists of possible completions,
but does affect the commands that actually do completions.
It is used by the function `file-name-completion'.
*/ );
  Vcompletion_ignored_extensions = Qnil;
}
