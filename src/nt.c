/* Utility and Unix shadow routines under MS Windows (WIN32_NATIVE defined).
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

*/

/* Authorship:

   Geoff Voelker (voelker@cs.washington.edu) 7-29-94
   Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au>
   Sync'ed with Emacs 19.34.6 by Marc Paquette <marcpa@cam.org>
   (Note: Sync messages from Marc Paquette may indicate
   incomplete synching, so beware.)
   Synched (completely!) with Emacs 20.6 by Ben Wing, 6-23-00.
   Largely rewritten by Ben Wing for XEmacs Mule support.
   Synched (completely!) with Emacs 21.1.103 by Ben Wing, 6-13-01.
*/

/* This file Mule-ized by Ben Wing, 6-23-00. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"

#include "sysdir.h"
#include "sysfile.h"
#include "sysproc.h"
#include "syspwd.h"
#include "syssignal.h"
#include "systime.h"

#include "syswindows.h"

/* Control whether stat() attempts to determine file type and link count
   exactly, at the expense of slower operation.  Since true hard links
   are supported on NTFS volumes, this is only relevant on NT.  */
Lisp_Object Vmswindows_get_true_file_attributes;

/*  Vmswindows_generate_fake_inodes; deleted */

Fixnum mswindows_fake_unix_uid;

/* Emulate getpwuid, getpwnam and others.  */

static struct passwd the_passwd =
{
  "",
  "",
  0,
  0,
  0,
  "",
  "",
  "",
};

uid_t
getuid (void) 
{
  return mswindows_fake_unix_uid;
}

uid_t 
geteuid (void) 
{ 
  /* Emacs 20.6 says: [[I could imagine arguing for checking to see
     whether the user is in the Administrators group and returning a
     UID of 0 for that case, but I don't know how wise that would be
     in the long run.]]  */
  return mswindows_fake_unix_uid;
}

gid_t
getgid (void) 
{ 
  return the_passwd.pw_gid;
}

gid_t
getegid (void) 
{ 
  return getgid ();
}

struct passwd *
getpwuid (uid_t uid)
{
  if (uid == mswindows_fake_unix_uid)
    {
      the_passwd.pw_gid = the_passwd.pw_uid = uid;
      return &the_passwd;
    }
  else
    return NULL;
}

struct passwd *
getpwnam (const Ibyte *name)
{
  struct passwd *pw;
  
  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (qxestrcasecmp_i18n (name, pw->pw_name))
    return NULL;

  return pw;
}

static void
init_user_info (void)
{
  /* This code is pretty much of ad hoc nature. There is no unix-like
     UIDs under Windows NT. There is no concept of root user, because
     all security is ACL-based. Instead, let's use a simple variable,
     nt-fake-unix-uid, which would allow the user to have a uid of
     choice. --kkm, 02/03/2000 */
#if 0
  /* Find the user's real name by opening the process token and
     looking up the name associated with the user-sid in that token.

     Use the relative portion of the identifier authority value from
     the user-sid as the user id value (same for group id using the
     primary group sid from the process token). */

  TOKEN_USER sidinfo;
  Extbyte name[256], domain[256];
  Charcount length = sizeof (name) / XETCHAR_SIZE;
  Charcount dlength = sizeof (domain) / XETCHAR_SIZE;
  DWORD trash;
  HANDLE token = NULL;
  SID_NAME_USE user_type;

  if (OpenProcessToken (GetCurrentProcess (), TOKEN_QUERY, &token)
      && GetTokenInformation (token, TokenUser, &sidinfo, sizeof (sidinfo),
			      &trash)
      && qxeLookupAccountSid (NULL, sidinfo.User.Sid, name, &length,
			      domain, &dlength, &user_type))
    {
      TSTR_TO_C_STRING_MALLOC (name, the_passwd.pw_name);
      /* Determine a reasonable uid value. */
      if (qxestrcasecmp ("administrator", the_passwd.pw_name) == 0)
	{
	  the_passwd.pw_uid = 0;
	  the_passwd.pw_gid = 0;
	}
      else
	{
	  SID_IDENTIFIER_AUTHORITY * pSIA;
	  TOKEN_PRIMARY_GROUP group;

	  pSIA = GetSidIdentifierAuthority (sidinfo.User.Sid);
	  /* I believe the relative portion is the last 4 bytes (of 6)
	     with msb first. */
	  the_passwd.pw_uid = ((pSIA->Value[2] << 24) +
			       (pSIA->Value[3] << 16) +
			       (pSIA->Value[4] << 8)  +
			       (pSIA->Value[5] << 0));
	  /* restrict to conventional uid range for normal users */
	  the_passwd.pw_uid = the_passwd.pw_uid % 60001;

	  /* Get group id */
	  if (GetTokenInformation (token, TokenPrimaryGroup,
				   &group, sizeof (group), &trash))
	    {
	      SID_IDENTIFIER_AUTHORITY * pSIA;

	      pSIA = GetSidIdentifierAuthority (group.PrimaryGroup);
	      the_passwd.pw_gid = ((pSIA->Value[2] << 24) +
				   (pSIA->Value[3] << 16) +
				   (pSIA->Value[4] << 8)  +
				   (pSIA->Value[5] << 0));
	      /* I don't know if this is necessary, but for safety... */
	      the_passwd.pw_gid = the_passwd.pw_gid % 60001;
	    }
	  else
	    the_passwd.pw_gid = the_passwd.pw_uid;
	}
    }
  /* If security calls are not supported (presumably because we
       are running under Windows 95), fallback to this. */
  else if (qxeGetUserName (name, &length))
    {
      TSTR_TO_C_STRING_MALLOC (name, the_passwd.pw_name);
      if (qxestrcasecmp ("administrator", the_passwd.pw_name) == 0)
	the_passwd.pw_uid = 0;
      else
	the_passwd.pw_uid = 123;
      the_passwd.pw_gid = the_passwd.pw_uid;
    }
  else
    {
      the_passwd.pw_name = "unknown";
      the_passwd.pw_uid = 123;
      the_passwd.pw_gid = 123;
    }

  if (token)
    CloseHandle (token);
#else
  /* Obtain only logon id here, uid part is moved to getuid */
  DWORD length = UNLEN + 1;
  Extbyte name[MAX_XETCHAR_SIZE * (UNLEN + 1)];
  if (qxeGetUserName (name, &length))
    TSTR_TO_C_STRING_MALLOC (name, the_passwd.pw_name);
  else
    the_passwd.pw_name = "unknown";
#endif

#if 0
  /* Ensure HOME and SHELL are defined. */
  /*
   * With XEmacs, setting $HOME is deprecated.
   */
  if (egetenv ("HOME") == NULL)
    eputenv ("HOME=c:/");
#endif

  /* Set dir from environment variables. */
  the_passwd.pw_dir = (char *) qxestrdup (get_home_directory ());
  /* We used to set pw_shell here, but the order is wrong (SHELL gets
     initted in process.c, called later in the init process) and pw_shell
     is not used anywhere. */
}

/* Parse the root part of file name, if present.  Return length and
   optionally store pointer to Ibyte after root.  */
static Bytecount
parse_root (Ibyte *name, Ibyte **pPath)
{
  Ibyte *start = name;

  if (name == NULL)
    return 0;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      /* skip past drive specifier */
      name += 2;
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      int slashes = 2;
      name += 2;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  name++;
	}
      while (*name);
      if (IS_DIRECTORY_SEP (name[0]))
	name++;
    }

  if (pPath)
    *pPath = name;

  return name - start;
}

/* Get long base name for name; name is assumed to be absolute.  */
static Ibyte *
get_long_basename (Ibyte *name)
{
  WIN32_FIND_DATAW find_data;
  HANDLE dir_handle;
  Extbyte *nameext;

  /* must be valid filename, no wild cards or other invalid characters */
  if (qxestrpbrk (name, "*?|<>\""))
    return 0;

  C_STRING_TO_TSTR (name, nameext);
  dir_handle = qxeFindFirstFile (nameext, &find_data);
  if (dir_handle != INVALID_HANDLE_VALUE)
    {
      Ibyte *fileint;

      TSTR_TO_C_STRING_MALLOC (find_data.cFileName, fileint);
      FindClose (dir_handle);
      return fileint;
    }
  return 0;
}

/* Get long name for file, if possible (assumed to be absolute).  */
Ibyte *
mswindows_get_long_filename (Ibyte *name)
{
  Ibyte *full = mswindows_canonicalize_filename (name);
  Ibyte *p;
  Ibyte *q;
  DECLARE_EISTRING (o);
  Bytecount len;

  /* Copy root part verbatim.  */
  len = parse_root (full, &p);
  eicpy_raw (o, full, len);

  while (p != NULL && *p)
    {
      Ibyte *component;

      q = p;
      p = qxestrchr (q, '\\');
      if (p) *p = '\0';
      component = get_long_basename (full);
      if (component)
	{
	  eicat_rawz (o, component);
	  if (p != NULL)
	    {
	      *p++ = '\\';
	      eicat_ch (o, '\\');
	    }
	  xfree (component);
	}
      else
	{
	  xfree (full);
	  return 0;
	}
    }

  xfree (full);
  return eicpyout_malloc (o, 0);
}

static int
is_unc_volume (const Ibyte *filename)
{
  const Ibyte *ptr = filename;

  if (!IS_DIRECTORY_SEP (ptr[0]) || !IS_DIRECTORY_SEP (ptr[1]) || !ptr[2])
    return 0;

  if (qxestrpbrk (ptr + 2, "*?|<>\"\\/"))
    return 0;

  return 1;
}

/* NOTE: Value returned is still in external format.  Callers need to
   convert. */
#define REG_ROOT "SOFTWARE\\XEmacs\\XEmacs"

static LPBYTE 
nt_get_resource (Ibyte *key, LPDWORD lpdwtype)
{
  LPBYTE lpvalue;
  HKEY hrootkey = NULL;
  DWORD cbData;
  Extbyte *keyext;

  C_STRING_TO_TSTR (key, keyext);
  
  /* Check both the current user and the local machine to see if 
     we have any resources.  */
  
  if (qxeRegOpenKeyEx (HKEY_CURRENT_USER, XETEXT (REG_ROOT), 0, KEY_READ,
		       &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;

      if (qxeRegQueryValueEx (hrootkey, keyext, NULL, NULL, NULL,
			      &cbData) == ERROR_SUCCESS 
	  && (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL 
	  && qxeRegQueryValueEx (hrootkey, keyext, NULL, lpdwtype, lpvalue,
			      &cbData) == ERROR_SUCCESS)
	return (lpvalue);

      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  if (qxeRegOpenKeyEx (HKEY_LOCAL_MACHINE, XETEXT (REG_ROOT), 0, KEY_READ,
		       &hrootkey) == ERROR_SUCCESS)
    {
      lpvalue = NULL;
	
      if (qxeRegQueryValueEx (hrootkey, keyext, NULL, NULL, NULL,
			      &cbData) == ERROR_SUCCESS &&
	  (lpvalue = (LPBYTE) xmalloc (cbData)) != NULL &&
	  qxeRegQueryValueEx (hrootkey, keyext, NULL, lpdwtype, lpvalue,
			      &cbData) == ERROR_SUCCESS)
	return (lpvalue);
	
      if (lpvalue) xfree (lpvalue);
	
      RegCloseKey (hrootkey);
    } 
  
  return (NULL);
}

void
init_mswindows_environment (void)
{
  /* Check for environment variables and use registry if they don't exist */
  /* Emacs 20.6 sets default values for these; not necessary here because
     we already supply them. (except SHELL, which is set in init_user_info().)
     Emacs 20.6 messes with TMPDIR; not necessary here. */
  {
    int i;
    LPBYTE lpval;
    DWORD dwType;

    static Char_ASCII *env_vars[] = 
    {
      "HOME",
      "EMACSLOADPATH",
      "EMACSDEBUGPATHS",
      "SHELL",
      "CMDPROXY",
      "EMACSDATA",
      "EMACSPATH",
      "EMACSPACKAGEPATH",
      "EMACSLOCKMETHOD",
      "INFOPATH"
    };
#if defined (HEAP_IN_DATA) && !defined (PDUMP)
    cache_system_info ();
#endif

#if 0 /* FSF 21.1 */
    /* !!#### i think i already do the equivalent elsewhere.
       delete when i'm sure i do.
       (but maybe i should be playing with LANG when the user changes
       the locale, so that subprocesses get it right.) */
       /* Get default locale info and use it for LANG.  */
      if (GetLocaleInfo (LOCALE_USER_DEFAULT,
			 LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
			 locale_name, sizeof (locale_name)))
	{
	  for (i = 0; i < (sizeof (env_vars) / sizeof (env_vars[0])); i++)
	    {
	      if (strcmp (env_vars[i].name, "LANG") == 0)
		{
		  env_vars[i].def_value = locale_name;
		  break;
		}
	    }
	}
#endif /* 0 */

    for (i = 0; i < countof (env_vars); i++) 
      {
	if (!egetenv (env_vars[i]) &&
	    (lpval = nt_get_resource (env_vars[i], &dwType)) != NULL)
	  {
	    if (dwType == REG_EXPAND_SZ)
	      {
		Extbyte *buf = NULL;
		Ibyte *envval;
		Charcount cch;

		cch = qxeExpandEnvironmentStrings ((Extbyte *) lpval, buf, 0);
		buf = (Extbyte *) ALLOCA (cch * XETCHAR_SIZE);
		qxeExpandEnvironmentStrings ((Extbyte *) lpval, buf, cch);
		TSTR_TO_C_STRING (buf, envval);
		eputenv (env_vars[i], envval);
	      }
	    else if (dwType == REG_SZ)
	      {
		Ibyte *envval;

		TSTR_TO_C_STRING (lpval, envval);
		eputenv (env_vars[i], envval);
	      }

	    xfree (lpval);
	  }
      }
  }

  /* Another special case: on NT, the PATH variable is actually named
     "Path" although cmd.exe (perhaps NT itself) arranges for
     environment variable lookup and setting to be case insensitive.
     However, Emacs assumes a fully case sensitive environment, so we
     need to change "Path" to "PATH" to match the expectations of
     various elisp packages.

     The same applies to COMSPEC.  */
  {
    Lisp_Object tail;

    EXTERNAL_LIST_LOOP (tail, Vprocess_environment)
      {
	Lisp_Object str = XCAR (tail);
	if (STRINGP (str))
	  {
	    Ibyte *dat = XSTRING_DATA (str);
	    if (qxestrncasecmp (dat, "PATH=", 5) == 0)
	      memcpy (dat, "PATH=", 5);
	    else if (qxestrncasecmp (dat, "COMSPEC=", 8) == 0)
	      memcpy (dat, "COMSPEC=", 8);
	  }
      }
  }

  init_user_info ();
}

/* Emacs 20.6 contains a routine get_emacs_configuration() here to set
   EMACS_CONFIGURATION. */ 
#ifndef HAVE_X_WINDOWS
/* X11R6 on NT provides the single parameter version of this command. */

#include <sys/timeb.h>

/* Emulate gettimeofday (Ulrich Leodolter, 1/11/95).  */
void 
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  struct _timeb tb;
  _ftime (&tb);

  tv->tv_sec = tb.time;
  tv->tv_usec = tb.millitm * 1000L;
  if (tz) 
    {
      tz->tz_minuteswest = tb.timezone;	/* minutes west of Greenwich  */
      tz->tz_dsttime = tb.dstflag;	/* type of dst correction  */
    }
}

#endif /* HAVE_X_WINDOWS */


/* ------------------------------------------------------------------------- */
/*               IO support and wrapper functions for Win32 API.             */
/* ------------------------------------------------------------------------- */

typedef struct volume_info_data
{
  struct volume_info_data *next;

  /* time when info was obtained */
  DWORD timestamp;

  /* actual volume info */
  Ibyte *root_dir;
  DWORD serialnum;
  DWORD maxcomp;
  DWORD flags;
  Ibyte *name;
  Ibyte *type;
} volume_info_data;

/* Global referenced by various functions.  */
static volume_info_data volume_info;

/* Vector to indicate which drives are local and fixed (for which cached
   data never expires).  */
static BOOL fixed_drives[26];

/* Consider cached volume information to be stale if older than 10s,
   at least for non-local drives.  Info for fixed drives is never stale.  */
#define DRIVE_INDEX( c ) ( (c) <= 'Z' ? (c) - 'A' : (c) - 'a' )
#define VOLINFO_STILL_VALID( root_dir, info )		\
  ( ( isalpha (root_dir[0]) &&				\
      fixed_drives[ DRIVE_INDEX (root_dir[0]) ] )	\
    || GetTickCount () - info->timestamp < 10000 )

/* Cache support functions.  */

/* Simple linked list with linear search is sufficient.  */
static volume_info_data *volume_cache = NULL;

static volume_info_data *
lookup_volume_info (Ibyte *root_dir)
{
  volume_info_data *info;

  for (info = volume_cache; info; info = info->next)
    if (qxestrcasecmp_i18n (info->root_dir, root_dir) == 0)
      break;
  return info;
}

static void
add_volume_info (Ibyte *root_dir, volume_info_data *info)
{
  info->root_dir = qxestrdup (root_dir);
  info->next = volume_cache;
  volume_cache = info;
}


/* Wrapper for GetVolumeInformation, which uses caching to avoid
   performance penalty (~2ms on 486 for local drives, 7.5ms for local
   cdrom drive, ~5-10ms or more for remote drives on LAN).  */
static volume_info_data *
get_cached_volume_information (Ibyte *root_dir)
{
  volume_info_data *info;
  Ibyte *default_root;

  /* NULL for root_dir means use root from current directory.  */
  if (root_dir == NULL)
    {
      Charcount nchars = qxeGetCurrentDirectory (0, NULL);
      Extbyte *rootext;

      if (!nchars)
	return NULL;
      rootext = alloca_extbytes (nchars * XETCHAR_SIZE);
      if (!qxeGetCurrentDirectory (nchars, rootext))
	return NULL;
      TSTR_TO_C_STRING (rootext, default_root);
      parse_root (default_root, &root_dir);
      *root_dir = 0;
      root_dir = default_root;
    }

  /* Local fixed drives can be cached permanently.  Removable drives
     cannot be cached permanently, since the volume name and serial
     number (if nothing else) can change.  Remote drives should be
     treated as if they are removable, since there is no sure way to
     tell whether they are or not.  Also, the UNC association of drive
     letters mapped to remote volumes can be changed at any time (even
     by other processes) without notice.
   
     As a compromise, so we can benefit from caching info for remote
     volumes, we use a simple expiry mechanism to invalidate cache
     entries that are more than ten seconds old.  */

#if 0
  /* No point doing this, because WNetGetConnection is even slower than
     GetVolumeInformation, consistently taking ~50ms on a 486 (FWIW,
     GetDriveType is about the only call of this type which does not
     involve network access, and so is extremely quick).  */

  /* Map drive letter to UNC if remote. */
  if (isalpha (root_dir[0]) && !fixed [DRIVE_INDEX (root_dir[0])])
    {
      Extbyte remote_name[256 * XETCHAR_SIZE];
      Ibyte drive[3] = { root_dir[0], ':' };
      Extbyte *driveext;

      C_STRING_TO_TSTR (drive, driveext);
      if (qxeWNetGetConnection (driveext, remote_name,
				sizeof (remote_name) / XETCHAR_SIZE)
	  == NO_ERROR)
	/* do something */ ;
    }
#endif

  info = lookup_volume_info (root_dir);

  if (info == NULL || ! VOLINFO_STILL_VALID (root_dir, info))
    {
      Extbyte name[256 * MAX_XETCHAR_SIZE];
      DWORD serialnum;
      DWORD maxcomp;
      DWORD flags;
      Extbyte type[256 * MAX_XETCHAR_SIZE];

      /* Info is not cached, or is stale. */
      if (!qxeGetVolumeInformation (root_dir,
				    name, sizeof (name) / XETCHAR_SIZE,
				    &serialnum,
				    &maxcomp,
				    &flags,
				    type, sizeof (type) / XETCHAR_SIZE))
	return NULL;

      /* Cache the volume information for future use, overwriting existing
	 entry if present.  */
      if (info == NULL)
	{
	  info = (volume_info_data *) xmalloc (sizeof (volume_info_data));
	  add_volume_info (root_dir, info);
	}
      else
	{
	  xfree (info->name);
	  xfree (info->type);
	}

      TSTR_TO_C_STRING_MALLOC (name, info->name);
      info->serialnum = serialnum;
      info->maxcomp = maxcomp;
      info->flags = flags;
      TSTR_TO_C_STRING_MALLOC (type, info->type);
      info->timestamp = GetTickCount ();
    }

  return info;
}

/* Get information on the volume where name is held; set path pointer to
   start of pathname in name (past UNC header\volume header if present).  */
static int
get_volume_info (const Ibyte *name, const Ibyte **pPath)
{
  /* We probably only need a couple of bytes, but let's be generous in
     case this function gets changed */
  Ibyte *temp = alloca_array (Ibyte, qxestrlen (name) + 10);
  Ibyte *rootname = NULL;  /* default to current volume */
  volume_info_data *info;

  if (name == NULL)
    return FALSE;

  /* find the root name of the volume if given */
  if (isalpha (name[0]) && name[1] == ':')
    {
      rootname = temp;
      temp[0] = *name++;
      temp[1] = *name++;
      temp[2] = '\\';
      temp[3] = 0;
    }
  else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
    {
      Ibyte *str = temp;
      int slashes = 4;
      rootname = temp;
      do
        {
	  if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	    break;
	  *str++ = *name++;
	}
      while (*name);

      *str++ = '\\';
      *str = 0;
    }

  if (pPath)
    *pPath = name;
    
  info = get_cached_volume_information (rootname);
  if (info != NULL)
    {
      /* Set global referenced by other functions.  */
      volume_info = *info;
      return TRUE;
    }
  return FALSE;
}

/* XEmacs: Everything referring to map_win32_filename() aka map_w32_filename()
   removed; it was only for NT 3.1, which we hereby do not support. (NT 3.5
   predates Windows 95!) */

static int
is_exec (const Ibyte *name)
{
  Ibyte *p = qxestrrchr (name, '.');
  return (p != NULL && (qxestrcasecmp (p, ".exe") == 0 ||
			qxestrcasecmp (p, ".com") == 0 ||
			qxestrcasecmp (p, ".bat") == 0 ||
			qxestrcasecmp (p, ".cmd") == 0));
}

/* Emulate the Unix directory procedures opendir, closedir, 
   and readdir.  We can't use the procedures supplied in sysdep.c,
   so we provide them here.  */

struct direct dir_static;       /* simulated directory contents */
static HANDLE dir_find_handle = INVALID_HANDLE_VALUE;
/*  dir_is_fat deleted */
static Ibyte *dir_pathname;
static WIN32_FIND_DATAW dir_find_data;

/* Support shares on a network resource as subdirectories of a read-only
   root directory. */
static HANDLE wnet_enum_handle = INVALID_HANDLE_VALUE;
static HANDLE open_unc_volume (const Ibyte *);
static Ibyte *read_unc_volume (HANDLE);
static int close_unc_volume (HANDLE);

DIR *
mswindows_opendir (const Ibyte *filename)
{
  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we defer the open until read time.  */

  if (dir_find_handle != INVALID_HANDLE_VALUE)
    return NULL;
  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    return NULL;

  if (is_unc_volume (filename))
    {
      wnet_enum_handle = open_unc_volume (filename);
      if (wnet_enum_handle == INVALID_HANDLE_VALUE)
	return NULL;
    }

  if (!(dirp = xnew_and_zero (DIR)))
    return NULL;

  if (dir_pathname)
    xfree (dir_pathname);
  dir_pathname = qxestrdup (filename);

  return dirp;
}

int
mswindows_closedir (DIR *dirp)
{
  int retval;

  /* If we have a find-handle open, close it.  */
  if (dir_find_handle != INVALID_HANDLE_VALUE)
    {
      retval = FindClose (dir_find_handle) ? 0 : -1;
      dir_find_handle = INVALID_HANDLE_VALUE;
    }
  else if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      retval = close_unc_volume (wnet_enum_handle);
      wnet_enum_handle = INVALID_HANDLE_VALUE;
    }
  xfree (dirp);

  return retval;
}

struct direct *
mswindows_readdir (DIR *dirp)
{
  Ibyte *val;
  int need_to_free = 0;

  if (wnet_enum_handle != INVALID_HANDLE_VALUE)
    {
      if (!(val = read_unc_volume (wnet_enum_handle)))
	return NULL;
      need_to_free = 1;
    }
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  else if (dir_find_handle == INVALID_HANDLE_VALUE)
    {
      DECLARE_EISTRING (filename);
      Ichar lastch;

      eicpy_rawz (filename, dir_pathname);
      lastch = eigetch_char (filename, eicharlen (filename) - 1);
      if (!IS_DIRECTORY_SEP (lastch))
	eicat_ch (filename, '\\');
      eicat_ch (filename, '*');
      eito_external (filename, Qmswindows_tstr);

      dir_find_handle = qxeFindFirstFile (eiextdata (filename),
					  &dir_find_data);

      if (dir_find_handle == INVALID_HANDLE_VALUE)
	return NULL;
      TSTR_TO_C_STRING (dir_find_data.cFileName, val);
    }
  else
    {
      if (!qxeFindNextFile (dir_find_handle, &dir_find_data))
	return NULL;
      TSTR_TO_C_STRING (dir_find_data.cFileName, val);
    }
  
  /* XEmacs never uses this value, so don't bother making it match
     value returned by qxe_stat().  */
  dir_static.d_ino = 1;
  
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;

  {
    DECLARE_EISTRING (found);
    Bytecount namlen;

    eicpy_rawz (found, val);
    if (need_to_free)
      xfree (val);

    if (!NILP (Vmswindows_downcase_file_names))
      eilwr (found);

    namlen = min (eilen (found), sizeof (dir_static.d_name) - 1);
    strncpy (dir_static.d_name, (char *) eidata (found), namlen);
    dir_static.d_name[namlen] = '\0';
    dir_static.d_namlen = (unsigned short) namlen;
  }

  return &dir_static;
}

static HANDLE
open_unc_volume (const Ibyte *path)
{
  NETRESOURCEW nr; 
  HANDLE henum;
  int result;

  nr.dwScope = RESOURCE_GLOBALNET; 
  nr.dwType = RESOURCETYPE_DISK; 
  nr.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER; 
  nr.dwUsage = RESOURCEUSAGE_CONTAINER; 
  nr.lpLocalName = NULL;
  C_STRING_TO_TSTR (path, nr.lpRemoteName);
  nr.lpComment = NULL; 
  nr.lpProvider = NULL;   

  result = qxeWNetOpenEnum (RESOURCE_GLOBALNET, RESOURCETYPE_DISK,  
			    RESOURCEUSAGE_CONNECTABLE, &nr, &henum);

  if (result == NO_ERROR)
    return henum;
  else
    return INVALID_HANDLE_VALUE;
}

static Ibyte *
read_unc_volume (HANDLE henum)
{
  int count;
  int result;
  Extbyte buf[16384];
  Ibyte *ptr;
  Bytecount bufsize = sizeof (buf);

  count = 1;
  /* #### we should just be querying the size and then allocating the
     right amount, like for all similar API's.  but the docs say this ?!

     An application cannot set the lpBuffer parameter to NULL and
     retrieve the required buffer size from the lpBufferSize
     parameter. Instead, the application should allocate a buffer of a
     reasonable size --  16 kilobytes (K) is typical -- and use the value
     of lpBufferSize for error detection.
     */

  result = qxeWNetEnumResource (wnet_enum_handle, &count, buf, &bufsize);
  if (result != NO_ERROR)
    return NULL;

  /* WNetEnumResource returns \\resource\share...skip forward to "share". */
  TSTR_TO_C_STRING (((LPNETRESOURCEW) buf)->lpRemoteName, ptr);
  INC_IBYTEPTR (ptr);
  INC_IBYTEPTR (ptr);
  while (*ptr && !IS_DIRECTORY_SEP (itext_ichar (ptr)))
    INC_IBYTEPTR (ptr);
  INC_IBYTEPTR (ptr);

  return qxestrdup (ptr);
}

static int
close_unc_volume (HANDLE henum)
{
  if (henum != INVALID_HANDLE_VALUE)
    return WNetCloseEnum (henum) == NO_ERROR ? 0 : -1;
  else
    return -1;
}

static DWORD
unc_volume_file_attributes (const Ibyte *path)
{
  HANDLE henum;
  DWORD attrs;

  henum = open_unc_volume (path);
  if (henum == INVALID_HANDLE_VALUE)
    return -1;

  attrs = FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_DIRECTORY;

  close_unc_volume (henum);

  return attrs;
}

int
mswindows_access (const Ibyte *path, int mode)
{
  DWORD attributes;

  /* MSVC implementation doesn't recognize D_OK.  */
  if (is_unc_volume (path))
    {
      attributes = unc_volume_file_attributes (path);
      if (attributes == -1)
	{
	  errno = EACCES;
	  return -1;
	}
    }
  else
    {
      Extbyte *pathext;

      C_STRING_TO_TSTR (path, pathext);
      if ((attributes = qxeGetFileAttributes (pathext)) == -1)
	{
	  /* Should try mapping GetLastError to errno; for now just indicate
	     that path doesn't exist.  */
	  errno = EACCES;
	  return -1;
	}
    }
  if ((mode & X_OK) != 0 && !is_exec (path))
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & W_OK) != 0 && (attributes & FILE_ATTRIBUTE_READONLY) != 0)
    {
      errno = EACCES;
      return -1;
    }
  if ((mode & D_OK) != 0 && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
    {
      errno = EACCES;
      return -1;
    }
  return 0;
}

/* This only works on NTFS volumes, but is useful to have.  */
/* #### NT 5.0 has a function CreateHardLink to do this directly,
   and it may do more things. */
int
mswindows_link (const Ibyte *old, const Ibyte *new)
{
  HANDLE fileh;
  int result = -1;

  if (old == NULL || new == NULL)
    {
      errno = ENOENT;
      return -1;
    }

  C_STRING_TO_TSTR (old, old);
  fileh = qxeCreateFile (old, 0, 0, NULL, OPEN_EXISTING,
			 FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (fileh != INVALID_HANDLE_VALUE)
    {
      int wlen;
      WCHAR *newuni;

      /* Confusingly, the "alternate" stream name field does not apply
         when restoring a hard link, and instead contains the actual
         stream data for the link (ie. the name of the link to create).
         The WIN32_STREAM_ID structure before the cStreamName field is
         the stream header, which is then immediately followed by the
         stream data.  */

      struct
	{
	  WIN32_STREAM_ID wid;
	  WCHAR wbuffer[MAX_PATH];	/* extra space for link name */
	} data;

      TO_EXTERNAL_FORMAT (C_STRING, new,
			  ALLOCA, (newuni, wlen), Qmswindows_unicode);
      if (wlen / sizeof (WCHAR) < MAX_PATH)
	{
	  LPVOID context = NULL;
	  DWORD wbytes = 0;

	  wcscpy (data.wid.cStreamName, newuni);
	  data.wid.dwStreamId = BACKUP_LINK;
	  data.wid.dwStreamAttributes = 0;
	  data.wid.Size.LowPart = wlen; /* in bytes, not chars! */
	  data.wid.Size.HighPart = 0;
	  data.wid.dwStreamNameSize = 0;

	  if (BackupWrite (fileh, (LPBYTE)&data,
			   offsetof (WIN32_STREAM_ID, cStreamName)
			   + data.wid.Size.LowPart,
			   &wbytes, FALSE, FALSE, &context)
	      && BackupWrite (fileh, NULL, 0, &wbytes, TRUE, FALSE, &context))
	    {
	      /* succeeded */
	      result = 0;
	    }
	  else
	    {
	      /* Should try mapping GetLastError to errno; for now just
		 indicate a general error (eg. links not supported).  */
	      errno = EINVAL;  // perhaps EMLINK?
	    }
	}

      CloseHandle (fileh);
    }
  else
    errno = ENOENT;

  return result;
}

/* sys_open() merged into sysdep.c sys_open() */

int
mswindows_rename (const Ibyte *oldname, const Ibyte *newname)
{
  int result;
  Ibyte *temp;

  /* MoveFile on Windows 95 doesn't correctly change the short file name
     alias in a number of circumstances (it is not easy to predict when
     just by looking at oldname and newname, unfortunately).  In these
     cases, renaming through a temporary name avoids the problem.

     A second problem on Windows 95 is that renaming through a temp name when
     newname is uppercase fails (the final long name ends up in
     lowercase, although the short alias might be uppercase) UNLESS the
     long temp name is not 8.3.

     So, on Windows 95 we always rename through a temp name, and we make sure
     the temp name has a long extension to ensure correct renaming.  */

  /* XEmacs: We sprintf() part of OLDNAME into part of OLDNAME + a number,
     so the following calculation should certainly be enough. */

  temp = qxestrcpy (alloca_ibytes (2 * qxestrlen (oldname) + 100), oldname);

  if (mswindows_windows9x_p)
    {
      Ibyte *o;
      Ibyte *p;
      int i = 0;

      if (o = qxestrrchr (oldname, '\\'))
	o++;
      else
	o = (Ibyte *) oldname;

      if (p = qxestrrchr (temp, '\\'))
	p++;
      else
	p = temp;

      do
	{
	  Extbyte *oldext, *tempext;
	  /* Force temp name to require a manufactured 8.3 alias - this
	     seems to make the second rename work properly.  */
	  qxesprintf (p, "_.%s.%u", o, i);
	  i++;
	  C_STRING_TO_EXTERNAL (oldname, oldext, Qfile_name);
	  C_STRING_TO_EXTERNAL (temp, tempext, Qfile_name);
	  result = rename (oldext, tempext);
	}
      /* This loop must surely terminate!  */
      while (result < 0 && errno == EEXIST);
      if (result < 0)
	return -1;
    }

  /* Emulate Unix behaviour - newname is deleted if it already exists
     (at least if it is a file; don't do this for directories).

     Since we mustn't do this if we are just changing the case of the
     file name (we would end up deleting the file we are trying to
     rename!), we let rename detect if the destination file already
     exists - that way we avoid the possible pitfalls of trying to
     determine ourselves whether two names really refer to the same
     file, which is not always possible in the general case.  (Consider
     all the permutations of shared or subst'd drives, etc.)  */
  {
    Extbyte *newext, *tempext;
    
    C_STRING_TO_EXTERNAL (newname, newext, Qfile_name);
    C_STRING_TO_EXTERNAL (temp, tempext, Qfile_name);
    result = rename (tempext, newext);

    if (result < 0
	&& errno == EEXIST
	&& _chmod (newext, 0666) == 0
	&& _unlink (newext) == 0)
      result = rename (tempext, newext);
  }

  return result;
}

int
mswindows_unlink (const Ibyte *path)
{
  Extbyte *pathout;

  C_STRING_TO_EXTERNAL (path, pathout, Qfile_name);
  /* On Unix, unlink works without write permission. */
  _chmod (pathout, 0666);
  return _unlink (pathout);
}

static FILETIME utc_base_ft;
static long double utc_base;
static int init = 0;
static LARGE_INTEGER utc_base_li;

/* XEmacs: We seem to have a new definition of
   mswindows_convert_time(), although I'm not sure why. --ben */

time_t
mswindows_convert_time (FILETIME uft)
{
  time_t ret;
#ifndef MAXLONGLONG
  SYSTEMTIME st;
  struct tm t;
  FILETIME ft;
  TIME_ZONE_INFORMATION tzi;
  DWORD tzid;
#else
  LARGE_INTEGER lft;
#endif

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);

      utc_base_li.LowPart = utc_base_ft.dwLowDateTime;
      utc_base_li.HighPart = utc_base_ft.dwHighDateTime;

      init = 1;
    }

#ifdef MAXLONGLONG

  /* On a compiler that supports long integers, do it the easy way */
  lft.LowPart = uft.dwLowDateTime;
  lft.HighPart = uft.dwHighDateTime;
  ret = (time_t) ((lft.QuadPart - utc_base_li.QuadPart) / 10000000);

#else

  /* Do it the hard way using mktime. */
  FileTimeToLocalFileTime(&uft, &ft);
  FileTimeToSystemTime (&ft, &st);
  tzid = GetTimeZoneInformation (&tzi);
  t.tm_year = st.wYear - 1900;
  t.tm_mon = st.wMonth - 1;
  t.tm_mday = st.wDay;
  t.tm_hour = st.wHour;
  t.tm_min = st.wMinute;
  t.tm_sec = st.wSecond;
  t.tm_isdst = (tzid == TIME_ZONE_ID_DAYLIGHT);
  /* st.wMilliseconds not applicable */
  ret = mktime(&t);
  if (ret == -1)
    {
      ret = 0;
    }

#endif

  return ret;
}

static void
convert_from_time_t (time_t time, FILETIME * pft)
{
  long double tmp;

  if (!init)
    {
      /* Determine the delta between 1-Jan-1601 and 1-Jan-1970. */
      SYSTEMTIME st;

      st.wYear = 1970;
      st.wMonth = 1;
      st.wDay = 1;
      st.wHour = 0;
      st.wMinute = 0;
      st.wSecond = 0;
      st.wMilliseconds = 0;

      SystemTimeToFileTime (&st, &utc_base_ft);
      utc_base = (long double) utc_base_ft.dwHighDateTime
	* 4096 * 1024 * 1024 + utc_base_ft.dwLowDateTime;
      init = 1;
    }

  /* time in 100ns units since 1-Jan-1601 */
  tmp = (long double) time * 1e7 + utc_base;
  pft->dwHighDateTime = (DWORD) (tmp / (4096.0 * 1024 * 1024));
  pft->dwLowDateTime = (DWORD) (tmp - (4096.0 * 1024 * 1024) *
                                pft->dwHighDateTime);
}

#if 0
/* A comment from Emacs 20.6:

   No reason to keep this; faking inode values either by hashing or even
   using the file index from GetInformationByHandle, is not perfect and
   so by default Emacs doesn't use the inode values on Windows.
   Instead, we now determine file-truename correctly (except for
   possible drive aliasing etc).  */

/* XEmacs: Removed the fake-inodes code here, which was if 0'd out.
   If you want it, look in w32.c in Emacs 20.6. */
#endif

/* #### aichner@ecf.teradyne.com reported that with the library
   provided stat/fstat, (file-exist "d:\\tmp\\") =>> nil,
   (file-exist "d:\\tmp") =>> t, when d:\tmp exists. Whenever
   we opt to use non-encapsulated stat(), this should serve as
   a compatibility test. --kkm */

/* Provide fstat and utime as well as stat for consistent handling of
   file timestamps. */
int
mswindows_fstat (int desc, struct stat *buf)
{
  HANDLE fh = (HANDLE) _get_osfhandle (desc);
  BY_HANDLE_FILE_INFORMATION info;
  DWORD fake_inode;
  int permission;

  switch (GetFileType (fh) & ~FILE_TYPE_REMOTE)
    {
    case FILE_TYPE_DISK:
      buf->st_mode = _S_IFREG;
      if (!GetFileInformationByHandle (fh, &info))
	{
	  errno = EACCES;
	  return -1;
	}
      break;
    case FILE_TYPE_PIPE:
      buf->st_mode = _S_IFIFO;
      goto non_disk;
    case FILE_TYPE_CHAR:
    case FILE_TYPE_UNKNOWN:
    default:
      buf->st_mode = _S_IFCHR;
    non_disk:
      memset (&info, 0, sizeof (info));
      info.dwFileAttributes = 0;
      info.ftCreationTime = utc_base_ft;
      info.ftLastAccessTime = utc_base_ft;
      info.ftLastWriteTime = utc_base_ft;
    }

  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
  else
    {
      buf->st_nlink = (short) info.nNumberOfLinks;
      /* Might as well use file index to fake inode values, but this
	 is not guaranteed to be unique unless we keep a handle open
	 all the time (even then there are situations where it is
	 not unique).  Reputedly, there are at most 48 bits of info
      (on NTFS, presumably less on FAT). */
      fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
    }

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = (unsigned short) (fake_inode ^ (fake_inode >> 16));
  else
    buf->st_ino = (unsigned short) fake_inode;

  /* consider files to belong to current user */
  buf->st_uid = 0;
  buf->st_gid = 0;

  buf->st_dev = info.dwVolumeSerialNumber;
  buf->st_rdev = info.dwVolumeSerialNumber;

  buf->st_size = info.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = mswindows_convert_time (info.ftLastWriteTime);
  buf->st_atime = mswindows_convert_time (info.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = mswindows_convert_time (info.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;
  else
    {
#if 0 /* no way of knowing the filename */
      Ibyte *p = qxestrrchr (name, '.');
      if (p != NULL &&
	  (qxestrcasecmp (p, ".exe") == 0 ||
	   qxestrcasecmp (p, ".com") == 0 ||
	   qxestrcasecmp (p, ".bat") == 0 ||
	   qxestrcasecmp (p, ".cmd") == 0))
	permission |= _S_IEXEC;
#endif
    }

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

/* MSVC stat function can't cope with UNC names and has other bugs, so
   replace it with our own.  This also allows us to calculate consistent
   inode values without hacks in the main Emacs code. */
int
mswindows_stat (const Ibyte *path, struct stat *buf)
{
  Ibyte *name, *r;
  WIN32_FIND_DATAW wfd;
  HANDLE fh;
  DWORD fake_inode;
  int permission;
  Bytecount len;
  int rootdir = FALSE;
  Extbyte *nameext;
  int errm;

  if (path == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }

  name = qxestrcpy (alloca_ibytes (qxestrlen (path) + 10), path);
  errm = SetErrorMode (SEM_FAILCRITICALERRORS
		       | SEM_NOOPENFILEERRORBOX);

  get_volume_info (name, &path);
  /* must be valid filename, no wild cards or other invalid characters */
  if (qxestrpbrk (name, "*?|<>\""))
    {
      errno = ENOENT;
      return -1;
    }

  /* If name is "c:/.." or "/.." then stat "c:/" or "/".  */
  r = IS_DEVICE_SEP (name[1]) ? &name[2] : name;
  if (IS_DIRECTORY_SEP (r[0]) && r[1] == '.' && r[2] == '.' && r[3] == '\0')
    {
      r[1] = r[2] = '\0';
    }

  /* Remove trailing directory separator, unless name is the root
     directory of a drive or UNC volume in which case ensure there
     is a trailing separator. */
  len = qxestrlen (name);
  rootdir = (path >= name + len - 1
	     && (IS_DIRECTORY_SEP (*path) || *path == 0));

  if (is_unc_volume (name))
    {
      DWORD attrs = unc_volume_file_attributes (name);

      if (attrs == -1)
	return -1;

      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = attrs;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      /* XEmacs deleted: strcpy (wfd.cFileName, name);
	 Not used later on. */
    }
  else if (rootdir)
    {
      if (!IS_DIRECTORY_SEP (name[len-1]))
	qxestrcat (name, (Ibyte *) "\\");
      C_STRING_TO_TSTR (name, nameext);
      if (qxeGetDriveType (nameext) < 2)
	{
	  SetErrorMode (errm);
	  errno = ENOENT;
	  return -1;
	}
      memset (&wfd, 0, sizeof (wfd));
      wfd.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
      wfd.ftCreationTime = utc_base_ft;
      wfd.ftLastAccessTime = utc_base_ft;
      wfd.ftLastWriteTime = utc_base_ft;
      /* XEmacs deleted: strcpy (wfd.cFileName, name);
	 Not used later on. */
    }
  else
    {
      if (IS_DIRECTORY_SEP (name[len-1]))
	name[len - 1] = 0;

      /* (This is hacky, but helps when doing file completions on
	 network drives.)  Optimize by using information available from
	 active readdir if possible.  */
      if (dir_pathname)
	{
	  len = qxestrlen (dir_pathname);
	  if (len && IS_DIRECTORY_SEP (dir_pathname[len-1]))
	    len--;
	}
      if (dir_find_handle != INVALID_HANDLE_VALUE
	  && dir_pathname
	  && qxestrncasecmp_i18n (dir_pathname, name, len) == 0
	  && IS_DIRECTORY_SEP (name[len])
	  && qxestrcasecmp_i18n (name + len + 1,
				 (Ibyte *) dir_static.d_name) == 0)
	{
	  /* This was the last entry returned by readdir.  */
	  wfd = dir_find_data;
	}
      else
	{
	  C_STRING_TO_TSTR (name, nameext);
	  fh = qxeFindFirstFile (nameext, &wfd);
	  if (fh == INVALID_HANDLE_VALUE)
	    {
	      SetErrorMode (errm);
	      errno = ENOENT;
	      return -1;
	    }
	  FindClose (fh);
	  /* XEmacs: Don't need to convert wfd.cFileName because
	     not used later on. */
	}
    }

  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      buf->st_mode = _S_IFDIR;
      buf->st_nlink = 2;	/* doesn't really matter */
      fake_inode = 0;		/* this doesn't either I think */
    }
  else 
    {
      if (!NILP (Vmswindows_get_true_file_attributes))
	C_STRING_TO_TSTR (name, nameext);
      if (!NILP (Vmswindows_get_true_file_attributes)
	  /* No access rights required to get info.  */
	  && (fh = qxeCreateFile (nameext, 0, 0, NULL, OPEN_EXISTING, 0, NULL))
	  != INVALID_HANDLE_VALUE)
	{
	  /* This is more accurate in terms of gettting the correct number
	     of links, but is quite slow (it is noticable when Emacs is
	     making a list of file name completions). */
	  BY_HANDLE_FILE_INFORMATION info;

	  if (GetFileInformationByHandle (fh, &info))
	    {
	      buf->st_nlink = (short) info.nNumberOfLinks;
	      /* Might as well use file index to fake inode values, but this
		 is not guaranteed to be unique unless we keep a handle open
		 all the time (even then there are situations where it is
		 not unique).  Reputedly, there are at most 48 bits of info
		 (on NTFS, presumably less on FAT). */
	      fake_inode = info.nFileIndexLow ^ info.nFileIndexHigh;
	    }
	  else
	    {
	      buf->st_nlink = 1;
	      fake_inode = 0;
	    }

	  switch (GetFileType (fh))
	    {
	    case FILE_TYPE_DISK:
	      buf->st_mode = _S_IFREG;
	      break;
	    case FILE_TYPE_PIPE:
	      buf->st_mode = _S_IFIFO;
	      break;
	    case FILE_TYPE_CHAR:
	    case FILE_TYPE_UNKNOWN:
	    default:
	      buf->st_mode = _S_IFCHR;
	    }
	  CloseHandle (fh);
	}
      else
	{
	  /* Don't bother to make this information more accurate.  */
	  buf->st_mode = _S_IFREG;
	  buf->st_nlink = 1;
	  fake_inode = 0;
	}
    }

  SetErrorMode (errm);

#if 0
  /* XEmacs: Removed the fake-inodes code here, which was if 0'd out.
     If you want it, look in w32.c in Emacs 20.6. */
#endif

  /* MSVC defines _ino_t to be short; other libc's might not.  */
  if (sizeof (buf->st_ino) == 2)
    buf->st_ino = (unsigned short) (fake_inode ^ (fake_inode >> 16));
  else
    buf->st_ino = (unsigned short) fake_inode;

  /* consider files to belong to current user */
  buf->st_uid = the_passwd.pw_uid;
  buf->st_gid = the_passwd.pw_gid;

  /* volume_info is set by get_volume_info */
  buf->st_dev = volume_info.serialnum;
  buf->st_rdev = volume_info.serialnum;


  buf->st_size = wfd.nFileSizeLow;

  /* Convert timestamps to Unix format. */
  buf->st_mtime = mswindows_convert_time (wfd.ftLastWriteTime);
  buf->st_atime = mswindows_convert_time (wfd.ftLastAccessTime);
  if (buf->st_atime == 0) buf->st_atime = buf->st_mtime;
  buf->st_ctime = mswindows_convert_time (wfd.ftCreationTime);
  if (buf->st_ctime == 0) buf->st_ctime = buf->st_mtime;

  /* determine rwx permissions */
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    permission = _S_IREAD;
  else
    permission = _S_IREAD | _S_IWRITE;
  
  if (wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    permission |= _S_IEXEC;
  else if (is_exec (name))
    permission |= _S_IEXEC;

  buf->st_mode |= permission | (permission >> 3) | (permission >> 6);

  return 0;
}

int
mswindows_utime (Lisp_Object path, struct utimbuf *times)
{
  /* #### Supposedly we're providing this because standard utime()
     might not work; or at the very least to get consistent results
     since we replace other time-handling routines in stat.  But out
     replacement doesn't seem to work, probably due to some subtle bug
     in this routine, which should be investigated eventually.  So for
     the moment, we just use utime(), which conceivably might be
     slightly off in comparison with our own routines?  Seems strange,
     and so far no problems seen. --ben */

  struct utimbuf deftime;
#if 0
  HANDLE fh;
#endif
  static FILETIME mtime;
  static FILETIME atime;
  Extbyte *filename;

  if (times == NULL)
    {
      deftime.modtime = deftime.actime = time (NULL);
      times = &deftime;
    }

  LISP_STRING_TO_TSTR (path, filename);
  /* APA: SetFileTime fails to set mtime correctly (always 1-Jan-1970) */
#if 0
  /* Need write access to set times.  */
  fh = qxeCreateFile (filename, GENERIC_WRITE,
		      FILE_SHARE_READ | FILE_SHARE_WRITE,
		      0, OPEN_EXISTING, 0, NULL);
  if (fh)
    {
      convert_from_time_t (times->actime, &atime);
      convert_from_time_t (times->modtime, &mtime);
      if (!SetFileTime (fh, NULL, &atime, &mtime))
	{
	  CloseHandle (fh);
	  errno = EACCES;
	  return -1;
	}
      CloseHandle (fh);
    }
  else
    {
      errno = EINVAL;
      return -1;
    }
  return 0;
#else
  {
    struct _utimbuf newtimes;

    newtimes.actime = times->actime;
    newtimes.modtime = times->modtime;

  if (XEUNICODE_P)
    return _wutime ((const wchar_t *) filename, &newtimes);
  else
    return _utime (filename, &newtimes);
  }
#endif
}

Ibyte *
mswindows_getdcwd (int drivelet)
{
  Extbyte *cwdext;
  Ibyte *cwd;

  if (XEUNICODE_P)
    cwdext = (Extbyte *) _wgetdcwd (drivelet, NULL, 0);
  else
    cwdext = _getdcwd (drivelet, NULL, 0);
  TSTR_TO_C_STRING_MALLOC (cwdext, cwd);
  xfree (cwdext);
  return cwd;
}


/*--------------------------------------------------------------------*/
/*                        Memory-mapped files                         */
/*--------------------------------------------------------------------*/

int
open_input_file (file_data *p_file, const Ibyte *filename)
{
  /* Synched with FSF 20.6.  We fixed some warnings. */
  HANDLE file;
  HANDLE file_mapping;
  void *file_base;
  DWORD size, upper_size;
  Extbyte *fileext;

  C_STRING_TO_TSTR (filename, fileext);

  file = qxeCreateFile (fileext, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  size = GetFileSize (file, &upper_size);
  file_mapping = qxeCreateFileMapping (file, NULL, PAGE_READONLY, 
				       0, size, NULL);
  if (!file_mapping) 
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0) 
    return FALSE;

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

int
open_output_file (file_data *p_file, const Ibyte *filename,
		  unsigned long size)
{
  /* Synched with FSF 20.6.  We fixed some warnings. */
  HANDLE file;
  HANDLE file_mapping;
  void *file_base;
  Extbyte *fileext;

  C_STRING_TO_TSTR (filename, fileext);

  file = qxeCreateFile (fileext, GENERIC_READ | GENERIC_WRITE, 0, NULL,
			CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE) 
    return FALSE;

  file_mapping = qxeCreateFileMapping (file, NULL, PAGE_READWRITE, 
				       0, size, NULL);
  if (!file_mapping) 
    return FALSE;
  
  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == NULL) 
    return FALSE;
  
  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

#if 1 /* !defined(MINGW) */
/* Return pointer to section header for section containing the given
   relative virtual address. */
static IMAGE_SECTION_HEADER *
rva_to_section (DWORD rva, IMAGE_NT_HEADERS *nt_header)
{
  /* Synched with FSF 20.6.  We added MINGW stuff. */
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      /* Some linkers (eg. the NT SDK linker I believe) swapped the
	 meaning of these two values - or rather, they ignored
	 VirtualSize entirely and always set it to zero.  This affects
	 some very old exes (eg. gzip dated Dec 1993).  Since
	 mswindows_executable_type relies on this function to work reliably,
	 we need to cope with this.  */
      DWORD real_size = max (section->SizeOfRawData,
			     section->Misc.VirtualSize);
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + real_size)
	return section;
      section++;
    }
  return NULL;
}
#endif

void
mswindows_executable_type (const Ibyte *filename, int *is_dos_app,
			   int *is_cygnus_app)
{
  /* Synched with FSF 20.6.  We added MINGW stuff and casts. */
  file_data executable;
  Ibyte *p;

  /* Default values in case we can't tell for sure.  */
  *is_dos_app = FALSE;
  *is_cygnus_app = FALSE;

  if (!open_input_file (&executable, filename))
    return;

  p = qxestrrchr (filename, '.');

  /* We can only identify DOS .com programs from the extension. */
  if (p && qxestrcasecmp (p, ".com") == 0)
    *is_dos_app = TRUE;
  else if (p && (qxestrcasecmp (p, ".bat") == 0 ||
		 qxestrcasecmp (p, ".cmd") == 0))
    {
      /* A DOS shell script - it appears that CreateProcess is happy to
	 accept this (somewhat surprisingly); presumably it looks at
	 COMSPEC to determine what executable to actually invoke.
	 Therefore, we have to do the same here as well. */
      /* Actually, I think it uses the program association for that
	 extension, which is defined in the registry.  */
      p = egetenv ("COMSPEC");
      if (p)
	mswindows_executable_type (p, is_dos_app, is_cygnus_app);
    }
  else
    {
      /* Look for DOS .exe signature - if found, we must also check that
	 it isn't really a 16- or 32-bit Windows exe, since both formats
	 start with a DOS program stub.  Note that 16-bit Windows
	 executables use the OS/2 1.x format. */

#if 0 /* defined( MINGW ) */
      /* mingw doesn't have enough headers to detect cygwin
	 apps, just do what we can. */
      FILHDR *exe_header;

      exe_header = (FILHDR *) executable.file_base;
      if (exe_header->e_magic != DOSMAGIC)
	goto unwind;

      if ((char *) exe_header->e_lfanew > (char *) executable.size)
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	} 
      else if (exe_header->nt_signature != NT_SIGNATURE)
	{
	  *is_dos_app = TRUE;
	}
#else
      IMAGE_DOS_HEADER *dos_header;
      IMAGE_NT_HEADERS *nt_header;

      dos_header = (PIMAGE_DOS_HEADER) executable.file_base;
      if (dos_header->e_magic != IMAGE_DOS_SIGNATURE)
	goto unwind;
	  
      nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header +
				       dos_header->e_lfanew);
	  
      if ((char *) nt_header > (char *) dos_header + executable.size) 
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	} 
      else if (nt_header->Signature != IMAGE_NT_SIGNATURE &&
	       LOWORD (nt_header->Signature) != IMAGE_OS2_SIGNATURE)
	{
	  *is_dos_app = TRUE;
	}
      else if (nt_header->Signature == IMAGE_NT_SIGNATURE)
	{
	  /* Look for cygwin.dll in DLL import list. */
	  IMAGE_DATA_DIRECTORY import_dir =
	    nt_header->OptionalHeader.
	      DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
	  IMAGE_IMPORT_DESCRIPTOR *imports;
	  IMAGE_SECTION_HEADER *section;

	  section = rva_to_section (import_dir.VirtualAddress, nt_header);
	  imports =
	    (IMAGE_IMPORT_DESCRIPTOR *) RVA_TO_PTR (import_dir.VirtualAddress,
						    section, executable);

	  for ( ; imports->Name; imports++)
	    {
	      Extbyte *dllname_ext =
		(Extbyte *) RVA_TO_PTR (imports->Name, section, executable);
	      Ibyte *dllname;

	      EXTERNAL_TO_C_STRING (dllname_ext, dllname, Qbinary);

	      /* The exact name of the cygwin dll has changed with
		 various releases, but hopefully this will be reasonably
		 future proof.  */
	      if (qxestrncasecmp (dllname, (Ibyte *) "cygwin", 6) == 0)
		{
		  *is_cygnus_app = TRUE;
		  break;
		}
	    }
	}
#endif
    }

 unwind:
  close_file_data (&executable);
}

/* Close the system structures associated with the given file.  */
void
close_file_data (file_data *p_file)
{
  UnmapViewOfFile (p_file->file_base);
  CloseHandle (p_file->file_mapping);
  CloseHandle (p_file->file);
}


/* Some miscellaneous functions that are Windows specific, but not GUI
   specific (ie. are applicable in terminal or batch mode as well).  */

DEFUN ("mswindows-short-file-name", Fmswindows_short_file_name, 1, 1, "", /*
  Return the short file name version (8.3) of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their short names.
*/
       (filename))
{
  Extbyte shortname[MAX_PATH * MAX_XETCHAR_SIZE];
  Extbyte *fileext;
  Ibyte *shortint;

  CHECK_STRING (filename);

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  LISP_STRING_TO_TSTR (filename, fileext);
  /* luckily, this returns the short version of each element in the path.  */
  if (qxeGetShortPathName (fileext, shortname,
			   sizeof (shortname) / XETCHAR_SIZE) == 0)
    return Qnil;

  TSTR_TO_C_STRING (shortname, shortint);
  MSWINDOWS_NORMALIZE_FILENAME (shortint);

  return build_string (shortint);
}


DEFUN ("mswindows-long-file-name", Fmswindows_long_file_name, 1, 1, "", /*
  Return the long file name version of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their long names.
*/
       (filename))
{
  Ibyte *longname, *canon;
  Lisp_Object ret;

  CHECK_STRING (filename);

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  if (!(longname = mswindows_get_long_filename (XSTRING_DATA (filename))))
    return Qnil;

  canon = mswindows_canonicalize_filename (longname);
  ret = build_string (canon);
  xfree (canon);
  xfree (longname);
  return ret;
}


void
init_nt (void)
{
  /* Initial preparation for subprocess support: replace our standard
     handles with non-inheritable versions.

     #### Do we still need this?  This is left over from the old process
     support. */
  {
    HANDLE parent;
    HANDLE stdin_save =  INVALID_HANDLE_VALUE;
    HANDLE stdout_save = INVALID_HANDLE_VALUE;
    HANDLE stderr_save = INVALID_HANDLE_VALUE;

    parent = GetCurrentProcess ();

    /* ignore errors when duplicating and closing; typically the
       handles will be invalid when running as a gui program. */
    DuplicateHandle (parent, 
		     GetStdHandle (STD_INPUT_HANDLE), 
		     parent,
		     &stdin_save, 
		     0, 
		     FALSE, 
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_OUTPUT_HANDLE),
		     parent,
		     &stdout_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    DuplicateHandle (parent,
		     GetStdHandle (STD_ERROR_HANDLE),
		     parent,
		     &stderr_save,
		     0,
		     FALSE,
		     DUPLICATE_SAME_ACCESS);
    
    retry_fclose (stdin);
    retry_fclose (stdout);
    retry_fclose (stderr);

    if (stdin_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdin_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_RDONLY);
    _fdopen (0, "r");

    if (stdout_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stdout_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (1, "w");

    if (stderr_save != INVALID_HANDLE_VALUE)
      _open_osfhandle ((long) stderr_save, O_TEXT);
    else
      _open ("nul", O_TEXT | O_NOINHERIT | O_WRONLY);
    _fdopen (2, "w");
  }

  /* determine which drives are fixed, for get_cached_volume_information */
  {
    /* GetDriveType must have trailing backslash. */
    Ibyte drive[] = "A:\\";

    /* Loop over all possible drive letters */
    while (*drive <= 'Z')
    {
      Extbyte *driveext;

      C_STRING_TO_TSTR (drive, driveext);

      /* Record if this drive letter refers to a fixed drive. */
      fixed_drives[DRIVE_INDEX (*drive)] =
	(qxeGetDriveType (driveext) == DRIVE_FIXED);

      (*drive)++;
    }

    /* Reset the volume info cache.  */
    volume_cache = NULL;
  }
}

void
syms_of_nt (void)
{
  DEFSUBR (Fmswindows_short_file_name);
  DEFSUBR (Fmswindows_long_file_name);
}

void
vars_of_nt (void)
{
  DEFVAR_INT ("mswindows-fake-unix-uid", &mswindows_fake_unix_uid /*
*Set uid returned by `user-uid' and `user-real-uid'.
Under NT and 9x, there are no uids, and even no almighty user called root.
By setting this variable, you can have any uid of choice.  Default is 0.
Changes to this variable take effect immediately.
*/ );
  mswindows_fake_unix_uid = 0;

  DEFVAR_LISP ("mswindows-get-true-file-attributes", &Vmswindows_get_true_file_attributes /*
Non-nil means determine accurate link count in file-attributes.
This option slows down file-attributes noticeably, so is disabled by
default.  Note that it is only useful for files on NTFS volumes,
where hard links are supported.
*/ );
  Vmswindows_get_true_file_attributes = Qnil;
}

/* end of nt.c */
