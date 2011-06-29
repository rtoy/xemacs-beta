/*
 * realpath.c -- canonicalize pathname by removing symlinks
 * Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
 * Copyright (C) 2001, 2002, 2004 Ben Wing.
 *

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

/* Synched up with: Not in FSF. */

/* This file has been Mule-ized, June 2001 by Ben Wing.

   Everything in this file now works in terms of internal, not external,
   data.  This is the only way to be safe, and it makes the code cleaner. */

#include <config.h>
#include "lisp.h"

#include "profile.h"

#include "sysfile.h"
#include "sysdir.h"

#define MAX_READLINKS 32

#ifdef WIN32_ANY
#include "syswindows.h"
#ifndef ELOOP
#define ELOOP 10062 /* = WSAELOOP in winsock.h */
#endif
#endif

Lisp_Object QSin_qxe_realpath;

/* Length of start of absolute filename. */
static int 
abs_start (const Ibyte *name)
{
#ifdef WIN32_ANY
  if (isalpha (*name) && IS_DEVICE_SEP (name[1])
      && IS_DIRECTORY_SEP (name[2]))
    return 3;
  else if (IS_DIRECTORY_SEP (*name))
    return IS_DIRECTORY_SEP (name[1]) ? 2 : 1;
  else 
    return 0;
#else /* not WIN32_ANY */
  return IS_DIRECTORY_SEP (*name) ? 1 : 0;
#endif
}

/* Find real name of a file by resolving symbolic links and/or shortcuts
   under Windows (.LNK links), if such support is enabled.

   If no link found, and LINKS_ONLY is false, look up the correct case in
   the file system of the last component.

   Under Windows, UNC servers and shares are lower-cased.  Directories must
   be given without trailing '/'. One day, this could read Win2K's reparse
   points.

   Returns length of characters copied info BUF.
   DOES NOT ZERO TERMINATE!!!!!
*/

#ifdef REALPATH_CORRECTS_CASE /* Darwin */
#include <sys/param.h>
#include <stdlib.h>
#endif

static int
readlink_or_correct_case (const Ibyte *name, Ibyte *buf, Bytecount size,
#ifndef WIN32_ANY
			  Boolint UNUSED (links_only)
#else
			  Boolint links_only
#endif
			  )
{
#ifndef WIN32_ANY
#ifdef REALPATH_CORRECTS_CASE
  /* Darwin's realpath corrects file name case, so we want to use that
     here, as well as our own, non-case-correcting, implementation
     further down in this file.

     It might be reasonable to incorporate case correction in our own
     realpath implementation, which would help things with
     case-insensitive file systems on Linux; one way to do this would
     be to make sure that init_initial_directory and
     get_initial_directory always give the correct case.  */
  int n = qxe_readlink (name, buf, (size_t) size);
  Extbyte realpath_buf[PATH_MAX], *tmp;
  DECLARE_EISTRING (realpathing);

  if (n >= 0 || errno != EINVAL)
    return n;

  eicpy_rawz (realpathing, name);
  eito_external (realpathing, Qfile_name);
  tmp = realpath (eiextdata (realpathing), realpath_buf);

  if (!tmp)
    return -1;

  if (0 == memcmp (eiextdata (realpathing), realpath_buf,
                   eiextlen (realpathing)))
    {
      /* No case change needed; tell the caller that. */
      errno = EINVAL;
      return -1;
    }

  eireset (realpathing);
  eicpy_ext (realpathing, realpath_buf, Qfile_name);
  if (eilen (realpathing) > size)
    {
      errno = ERANGE;
      return -1;
    }

  memcpy (buf, eidata (realpathing), eilen (realpathing));
  return eilen (realpathing);
#else /* !REALPATH_CORRECTS_CASE */
  return qxe_readlink (name, buf, (size_t) size);
#endif /* REALPATH_CORRECTS_CASE */
#else /* defined (WIN32_ANY) */
# ifdef CYGWIN
  int n = qxe_readlink (name, buf, (size_t) size);
  if (n >= 0 || errno != EINVAL)
    return n;

  /* The file may exist, but isn't a symlink. Try to find the
     right name. */
  LOCAL_FILE_FORMAT_TO_INTERNAL_MSWIN (name, name);
# else
  if (mswindows_shortcuts_are_symlinks)
    {
      Ibyte *tmp = mswindows_read_link (name);

      if (tmp != NULL)
	{
	  /* Fucking fixed buffers. */
	  Bytecount len = qxestrlen (tmp);
	  if (len > size)
	    {
	      errno = ENAMETOOLONG;
	      return -1;
	    }
	  memcpy (buf, tmp, len);
	  xfree (tmp);
	  return len;
	}
    }
# endif

  if (links_only)
    {
      errno = EINVAL;
      return -1;
    }

  {
    int len = 0;
    int err = 0;
    const Ibyte *lastname;
    int count = 0;
    const Ibyte *nn;
    DECLARE_EISTRING (result);
  
    assert (*name);
  
    /* Sort of check we have a valid filename. */
    if (qxestrpbrk (name, "*?|<>\""))
      {
	errno = ENOENT;
	return -1;
      }
    else if (qxestrlen (name) >= PATH_MAX_INTERNAL)
      {
	errno = ENAMETOOLONG;
	return -1;
      }
  
    /* Find start of filename */
    lastname = name + qxestrlen (name);
    while (lastname > name && !IS_DIRECTORY_SEP (lastname[-1]))
      --lastname;

    /* Count slashes in unc path */
    if (abs_start (name) == 2)
      for (nn = name; *nn; nn++)
	if (IS_DIRECTORY_SEP (*nn))
	  count++;

    if (count >= 2 && count < 4)
      {
	eicpy_rawz (result, lastname);
	eilwr (result);
      }
    else
      {
	WIN32_FIND_DATAW find_data;
	Extbyte *nameext;
	HANDLE dir_handle;

	nameext = ITEXT_TO_TSTR (name);
	dir_handle = qxeFindFirstFile (nameext, &find_data);
	if (dir_handle == INVALID_HANDLE_VALUE)
	  {
	    errno = ENOENT;
	    return -1;
	  }
	eicpy_ext (result, (Extbyte *) find_data.cFileName, Qmswindows_tstr);
	FindClose (dir_handle);
      }

    if ((len = eilen (result)) <= size)
      {
	DECLARE_EISTRING (eilastname);

	eicpy_rawz (eilastname, lastname);
	if (eicmp_ei (eilastname, result) == 0)
          /* Signal that the name is already OK. */
          err = EINVAL;
	else
	  memcpy (buf, eidata (result), len);
      }
    else
      err = ENAMETOOLONG;

    errno = err;
    return err ? -1 : len;
  }
#endif /* WIN32_ANY */
}

/* Mule Note: This function works with and returns
   internally-formatted strings.

   if LINKS_ONLY is true, don't do case canonicalization under
   Windows. */

Ibyte *
qxe_realpath (const Ibyte *path, Ibyte *resolved_path, Boolint links_only)
{
  Ibyte copy_path[PATH_MAX_INTERNAL];
  Ibyte *new_path = resolved_path;
  Ibyte *max_path;
  Ibyte *retval = NULL;
#if defined (HAVE_READLINK) || defined (WIN32_ANY)
  int readlinks = 0;
  Ibyte link_path[PATH_MAX_INTERNAL];
  int n;
  int abslen = abs_start (path);
#endif

  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_qxe_realpath);

 restart:

  /* Make a copy of the source path since we may need to modify it. */
  qxestrcpy (copy_path, path);
  path = copy_path;
  max_path = copy_path + PATH_MAX_INTERNAL - 2;

  if (0)
    ;
#ifdef WIN32_ANY
  /* Check for c:/... or //server/... */
  else if (abslen == 3 || abslen == 2)
    {
      /* Make sure drive letter is lowercased. */
      if (abslen == 3)
	{
	  *new_path = tolower (*path);
	  new_path++;
	  path++;
	  abslen--;
	}
      /* Coerce directory chars. */
      while (abslen-- > 0)
	{
	  if (IS_DIRECTORY_SEP (*path))
	    *new_path++ = DIRECTORY_SEP;
	  else
	    *new_path++ = *path;
	  path++;
	}
    }
#endif
#ifdef WIN32_NATIVE
  /* No drive letter, but a beginning slash? Prepend drive letter. */
  else if (abslen == 1)
    {
      get_initial_directory (new_path, PATH_MAX_INTERNAL - 1);
      new_path += 3;
      path++;
    }
  /* Just a path name, prepend the current directory */
  else
    {
      get_initial_directory (new_path, PATH_MAX_INTERNAL - 1);
      new_path += qxestrlen (new_path);
      if (!IS_DIRECTORY_SEP (new_path[-1]))
	*new_path++ = DIRECTORY_SEP;
    }
#else
  /* If it's a relative pathname use get_initial_directory for starters. */
  else if (abslen == 0)
    {
      get_initial_directory (new_path, PATH_MAX_INTERNAL - 1);
      new_path += qxestrlen (new_path);
      if (!IS_DIRECTORY_SEP (new_path[-1]))
	*new_path++ = DIRECTORY_SEP;
    }
  else
    {
      /* Copy first directory sep. May have two on cygwin. */
      qxestrncpy (new_path, path, abslen);
      new_path += abslen;
      path += abslen;
    }
#endif
  /* Expand each slash-separated pathname component. */
  while (*path != '\0')
    {
      /* Ignore stray "/". */
      if (IS_DIRECTORY_SEP (*path))
	{
	  path++;
	  continue;
	}

      if (*path == '.')
	{
	  /* Ignore ".". */
	  if (path[1] == '\0' || IS_DIRECTORY_SEP (path[1]))
	    {
	      path++;
	      continue;
	    }

	  /* Handle ".." */
	  if (path[1] == '.' &&
	      (path[2] == '\0' || IS_DIRECTORY_SEP (path[2])))
	    {
	      path += 2;

	      /* Ignore ".." at root. */
	      if (new_path == resolved_path + abs_start (resolved_path))
		continue;

	      /* Handle ".." by backing up. */
	      --new_path;
	      while (!IS_DIRECTORY_SEP (new_path[-1]))
		--new_path;
	      continue;
	    }
	}

      /* Safely copy the next pathname component. */
      while (*path != '\0' && !IS_DIRECTORY_SEP (*path))
	{
	  if (path > max_path)
	    {
	      errno = ENAMETOOLONG;
	      goto done;
	    }
	  *new_path++ = *path++;
	}

#if defined (HAVE_READLINK) || defined (WIN32_ANY)
      /* See if latest pathname component is a symlink or needs case
	 correction. */
      *new_path = '\0';
      n = readlink_or_correct_case (resolved_path, link_path,
				    PATH_MAX_INTERNAL - 1, links_only);

      if (n < 0)
	{
	  /* EINVAL means the file exists but isn't a symlink or doesn't
	     need case correction. */
#ifdef WIN32_ANY
	  if (errno != EINVAL && errno != ENOENT)
#else
	  if (errno != EINVAL) 
#endif
	    goto done;
	}
      else
	{
	  /* Protect against infinite loops. */
	  if (readlinks++ > MAX_READLINKS)
	    {
	      errno = ELOOP;
	      goto done;
	    }

	  /* Note: readlink doesn't add the null byte. */
	  link_path[n] = '\0';
	  
	  abslen = abs_start (link_path);
	  if (abslen > 0)
	    {
	      /* Start over for an absolute symlink. */
	      new_path = resolved_path;
	      qxestrcat (link_path, path);
	      path = link_path;
	      goto restart;
	    }

	  /* Otherwise back up over this component. */
	  for (--new_path; !IS_DIRECTORY_SEP (*new_path); --new_path)
	    assert (new_path > resolved_path);

	  /* Safe sex check. */
	  if (qxestrlen (path) + n >= PATH_MAX_INTERNAL)
	    {
	      errno = ENAMETOOLONG;
	      goto done;
	    }

	  /* Insert symlink contents into path. */
	  qxestrcat (link_path, path);
	  qxestrcpy (copy_path, link_path);
	  path = copy_path;
	}
#endif /* HAVE_READLINK || WIN32_ANY */
      *new_path++ = DIRECTORY_SEP;
    }

  /* Delete trailing slash but don't whomp a lone slash. */
  if (new_path != resolved_path + abs_start (resolved_path) &&
      IS_DIRECTORY_SEP (new_path[-1]))
    new_path--;

  /* Make sure it's null terminated. */
  *new_path = '\0';

  retval = resolved_path;
done:
  PROFILE_RECORD_EXITING_SECTION (QSin_qxe_realpath);
  return retval;
}

void
vars_of_realpath (void)
{
  QSin_qxe_realpath =
    build_defer_string ("(in qxe_realpath)");
  staticpro (&QSin_qxe_realpath);
}
