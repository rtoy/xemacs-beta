/*
 * realpath.c -- canonicalize pathname by removing symlinks
 * Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
 * Copyright (C) 2001, 2002 Ben Wing.
 *

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

/* This file has been Mule-ized, June 2001 by Ben Wing.

   Everything in this file now works in terms of internal, not external,
   data.  This is the only way to be safe, and it makes the code cleaner. */

#include <config.h>
#include "lisp.h"

#include "sysfile.h"
#include "sysdir.h"

#define MAX_READLINKS 32

#ifdef WIN32_ANY
#include "syswindows.h"
#ifndef ELOOP
#define ELOOP 10062 /* = WSAELOOP in winsock.h */
#endif
#endif

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

/* Find real name of a file by resolving symbolic links and (under Windows)
   looking up the correct case of the file as it appears on the file
   system.

   Under Windows, UNC servers and shares are lower-cased.  Directories must
   be given without trailing '/'. One day, this could read Win2K's reparse
   points. */

static int
readlink_and_correct_case (const Ibyte *name, Ibyte *buf,
			   int size)
{
#ifndef WIN32_ANY
  return qxe_readlink (name, buf, size);
#else
# ifdef CYGWIN
  Ibyte *tmp;
  int n = qxe_readlink (name, buf, size);
  if (n >= 0 || errno != EINVAL)
    return n;

  /* The file may exist, but isn't a symlink. Try to find the
     right name. */
  tmp =
    alloca_ibytes (cygwin_posix_to_win32_path_list_buf_size ((char *) name));
  cygwin_posix_to_win32_path_list ((char *) name, (char *) tmp);
  name = tmp;
# endif

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
    else if (qxestrlen (name) >= PATH_MAX)
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

	C_STRING_TO_TSTR (name, nameext);
	dir_handle = qxeFindFirstFile (nameext, &find_data);
	if (dir_handle == INVALID_HANDLE_VALUE)
	  {
	    errno = ENOENT;
	    return -1;
	  }
	eicpy_ext (result, (Extbyte *) find_data.cFileName, Qmswindows_tstr);
	FindClose (dir_handle);
      }

    if ((len = eilen (result)) < size)
      {
	DECLARE_EISTRING (eilastname);

	eicpy_rawz (eilastname, lastname);
	if (eicmp_ei (eilastname, result) == 0)
	  /* Signal that the name is already OK. */
	  err = EINVAL;
	else
	  memcpy (buf, eidata (result), len + 1);
      }
    else
      err = ENAMETOOLONG;

    errno = err;
    return err ? -1 : len;
  }
#endif /* WIN32_ANY */
}

/* Mule Note: This function works with and returns
   internally-formatted strings. */

Ibyte *
qxe_realpath (const Ibyte *path, Ibyte *resolved_path)
{
  Ibyte copy_path[PATH_MAX];
  Ibyte *new_path = resolved_path;
  Ibyte *max_path;
#if defined (HAVE_READLINK) || defined (WIN32_ANY)
  int readlinks = 0;
  Ibyte link_path[PATH_MAX];
  int n;
  int abslen = abs_start (path);
#endif

 restart:

  /* Make a copy of the source path since we may need to modify it. */
  qxestrcpy (copy_path, path);
  path = copy_path;
  max_path = copy_path + PATH_MAX - 2;

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
      get_initial_directory (new_path, PATH_MAX - 1);
      new_path += 3;
      path++;
    }
  /* Just a path name, prepend the current directory */
  else
    {
      get_initial_directory (new_path, PATH_MAX - 1);
      new_path += qxestrlen (new_path);
      if (!IS_DIRECTORY_SEP (new_path[-1]))
	*new_path++ = DIRECTORY_SEP;
    }
#else
  /* If it's a relative pathname use get_initial_directory for starters. */
  else if (abslen == 0)
    {
      get_initial_directory (new_path, PATH_MAX - 1);
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
	      return NULL;
	    }
	  *new_path++ = *path++;
	}

#if defined (HAVE_READLINK) || defined (WIN32_ANY)
      /* See if latest pathname component is a symlink or needs case
	 correction. */
      *new_path = '\0';
      n = readlink_and_correct_case (resolved_path, link_path, PATH_MAX - 1);

      if (n < 0)
	{
	  /* EINVAL means the file exists but isn't a symlink or doesn't
	     need case correction. */
#ifdef WIN32_ANY
	  if (errno != EINVAL && errno != ENOENT)
#else
	  if (errno != EINVAL) 
#endif
	    return NULL;
	}
      else
	{
	  /* Protect against infinite loops. */
	  if (readlinks++ > MAX_READLINKS)
	    {
	      errno = ELOOP;
	      return NULL;
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
	  if (qxestrlen (path) + n >= PATH_MAX)
	    {
	      errno = ENAMETOOLONG;
	      return NULL;
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

  return resolved_path;
}
