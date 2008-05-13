/* Utility routines for XEmacs on Windows 9x, NT and Cygwin.
   Copyright (C) 2000, 2001, 2002, 2004 Ben Wing.

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
02111-1307, USA. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-msw.h"
#include "hash.h"
#include "profile.h"

#include "sysfile.h"
#include "sysproc.h"
#include "syssignal.h"
#include "systime.h"



/*

Info on Windows issues:

  (Info-goto-node "(internals)Interface to MS Windows")

   ------- @file{src/config.h}.in vs. @file{nt/xemacs.mak} -------

   See @file{src/config.h.in} more more info.
*/

/* Control conversion of upper case file names to lower case.
   nil means no, t means yes. */
Lisp_Object Vmswindows_downcase_file_names;

struct hash_table *mswindows_read_link_hash;

int mswindows_windows9x_p;
Boolint mswindows_shortcuts_are_symlinks;

pfSwitchToThread_t xSwitchToThread;

pfNetUserEnum_t xNetUserEnum;
pfNetApiBufferFree_t xNetApiBufferFree;

/* Convert a filename in standard Win32 format into our internal format
   (which may be significantly different if we're running on Cygwin), and
   turn it into a file: URL.  Return a newly malloc()ed string.

   #### This comes from code that just prepended `file:', which is not
   good.  See comment in mswindows_dde_callback(), case XTYP_EXECUTE.
  */
Ibyte *
urlify_filename (Ibyte *filename)
{
  Ibyte *pseudo_url;
  
  WIN32_TO_LOCAL_FILE_FORMAT (filename, filename);
  pseudo_url = xnew_array (Ibyte, 5 + qxestrlen (filename) + 1);
  qxestrcpy_ascii (pseudo_url, "file:");
  qxestrcat (pseudo_url, filename);
  /* URL's only have /, no backslash */
  for (filename = pseudo_url; *filename; filename++)
    {
      if (*filename == '\\')
	*filename = '/';
    }

  return pseudo_url;
}

/* Convert a Win32 file name in tstr format into a local-format file name
   in internal format. */

Lisp_Object
tstr_to_local_file_format (Extbyte *path)
{
  Ibyte *ttlff;

  TSTR_TO_C_STRING (path, ttlff);
  WIN32_TO_LOCAL_FILE_FORMAT (ttlff, ttlff);

  return build_intstring (ttlff);
}

/* Normalize filename by converting all path separators to the specified
   separator.  Also conditionally convert all-upper-case path name
   components to lower case.  Return a newly malloc()ed string.
*/

Ibyte *
mswindows_canonicalize_filename (Ibyte *name)
{
  Ibyte *fp = name;
  DECLARE_EISTRING (newname);
  DECLARE_EISTRING (component);
  int do_casefrob = 1;

  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so filenames can be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if (name[0] >= 'A' && name[0] <= 'Z' && name[1] == ':')
    {
      eicat_ch (newname, name[0] + 'a' - 'A');
      eicat_ch (newname, ':');
      fp += 2;
    }

  while (1)
    {
      Ichar ch = itext_ichar (fp);
      if (LOWERCASEP (0, ch))
	do_casefrob = 0;	/* don't convert this element */

      if (ch == 0 || IS_ANY_SEP (ch))
	{
	  if (do_casefrob && !NILP (Vmswindows_downcase_file_names))
	    eilwr (component);
	  do_casefrob = 1;
	  eicat_ei (newname, component);
	  eireset (component);
	  if (IS_DIRECTORY_SEP (ch))
	    eicat_ch (newname, DIRECTORY_SEP);
	  else if (ch)
	    eicat_ch (newname, ch);
	  else
	    break;
	}
      else
	eicat_ch (component, ch);

      INC_IBYTEPTR (fp);
    }

  return eicpyout_malloc (newname, 0);
}

Extbyte *
mswindows_get_module_file_name (void)
{
  Extbyte *path = NULL;
  int bufsize = 4096;
  int cchpathsize;
  
  while (1)
    {
      path = (Extbyte *) xrealloc (path, bufsize * XETCHAR_SIZE);
      cchpathsize = qxeGetModuleFileName (NULL, path, bufsize);
      if (!cchpathsize)
	return 0;
      if (cchpathsize + 1 <= bufsize)
	break;
      bufsize *= 2;
    }

  return path;
}

static void
init_potentially_nonexistent_functions (void)
{
  HMODULE h_kernel = qxeGetModuleHandle (XETEXT ("kernel32"));
  /* the following does not seem to get mapped in automatically */
  HMODULE h_netapi = qxeLoadLibrary (XETEXT ("netapi32.dll"));

  if (h_kernel)
    {
      xSwitchToThread =
	(pfSwitchToThread_t) GetProcAddress (h_kernel, "SwitchToThread");
    }

  if (h_netapi)
    {
      xNetUserEnum =
	(pfNetUserEnum_t) GetProcAddress (h_netapi, "NetUserEnum");
      xNetApiBufferFree =
	(pfNetApiBufferFree_t) GetProcAddress (h_netapi, "NetApiBufferFree");
    }
}

static Lisp_Object
mswindows_lisp_error_1 (int errnum, int no_recurse)
{
  LPTSTR lpMsgBuf;
  Lisp_Object result;
  Ibyte *inres;
  Bytecount len;
  int i;

  /* The docs for FormatMessage say:

     If you pass a specific LANGID in this parameter, FormatMessage
     will return a message for that LANGID only. If the function
     cannot find a message for that LANGID, it returns
     ERROR_RESOURCE_LANG_NOT_FOUND. If you pass in zero, FormatMessage
     looks for a message for LANGIDs in the following order:

     Language neutral 
     Thread LANGID, based on the thread's locale value 
     User default LANGID, based on the user's default locale value 
     System default LANGID, based on the system default locale value 
     US English

     If FormatMessage doesn't find a message for any of the preceding
     LANGIDs, it returns any language message string that is present. If
     that fails, it returns ERROR_RESOURCE_LANG_NOT_FOUND. (Note, this is
     returned through GetLastError(), not the return value.)

     #### what the hell is "language neutral"?  i can find no info on this.
     so let's do our own language first.
     */

  for (i = 0; ; i++)
    {
      int lang = 0;
      int retval;

      switch (i)
	{
#ifdef MULE
	  /* Urk!  Windows 95 doesn't let you set the thread locale!
	     so we have to maintain our own. */
	case 0: lang = LANGIDFROMLCID (mswindows_current_locale ()); break;
	case 1: lang = 0; break;
#else
	case 0: lang = 0; break;
#endif
	default: ABORT ();
	}

      retval = qxeFormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER
				 | FORMAT_MESSAGE_FROM_SYSTEM,
				 NULL, errnum, lang,
				 /* yeah, i'm casting a char ** to a char *.
				    ya gotta problem widdat? */
				 (Extbyte *) &lpMsgBuf, 0, NULL);

      if (!retval)
	{
	  if (lang != 0)
	    continue;

	  if (no_recurse)
	    return emacs_sprintf_string
	      ("Unknown error code %d (error return %ld from FormatMessage())",
	       errnum, GetLastError ());
	  else
	    return emacs_sprintf_string
	      ("Unknown error code %d (error return %s from FormatMessage())",
	       /* It's OK, emacs_sprintf_string disables GC explicitly */
	       errnum, XSTRING_DATA (mswindows_lisp_error_1 (errnum, 1)));
	}
      else
	break;
    }

  TSTR_TO_C_STRING (lpMsgBuf, inres);
  len = qxestrlen (inres);
  /* Messages tend to end with a period and newline */
  if (len >= 3 && !qxestrcmp_ascii (inres + len - 3, ".\r\n"))
    len -= 3;
  result = make_string (inres, len);
  
  LocalFree (lpMsgBuf);
  return result;
}

Lisp_Object
mswindows_lisp_error (int errnum)
{
  return mswindows_lisp_error_1 (errnum, 0);
}

void
mswindows_output_last_error (char *frob)
{
  int errval = GetLastError ();
  Lisp_Object errmess = mswindows_lisp_error (errval);
  
  stderr_out ("last error during %s is %d: %s\n",
	      frob, errval, XSTRING_DATA (errmess));
}

DOESNT_RETURN
mswindows_report_process_error (const char *string, Lisp_Object data,
				int errnum)
{
  report_file_type_error (Qprocess_error, mswindows_lisp_error (errnum),
			  string, data);
}

DEFUN ("mswindows-shell-execute", Fmswindows_shell_execute, 2, 4, 0, /*
Get Windows to perform OPERATION on DOCUMENT.
This is a wrapper around the ShellExecute system function, which
invokes the application registered to handle OPERATION for DOCUMENT.
OPERATION is typically \"open\", \"print\" or \"explore\" (but can be
nil for the default action), and DOCUMENT is typically the name of a
document file or URL, but can also be a program executable to run or
a directory to open in the Windows Explorer.

If DOCUMENT is a program executable, PARAMETERS can be a string
containing command line parameters, but otherwise should be nil.

SHOW-FLAG can be used to control whether the invoked application is hidden
or minimized.  If SHOW-FLAG is nil, the application is displayed normally,
otherwise it is an integer representing a ShowWindow flag:

  0 - start hidden
  1 - start normally
  3 - start maximized
  6 - start minimized
*/
       (operation, document, parameters, show_flag))
{
  /* Encode filename and current directory.  */
  Lisp_Object current_dir = Ffile_name_directory (document);
  int ret;

  CHECK_STRING (document);

  if (NILP (current_dir))
    current_dir = current_buffer->directory;

  {
    Extbyte *opext = NULL;
    Extbyte *parmext = NULL;
    Extbyte *path = NULL;
    Extbyte *doc = NULL;

    if (STRINGP (operation))
      LISP_STRING_TO_TSTR (operation, opext);
    /* #### What about path names, which may be links? */
    if (STRINGP (parameters))
      LISP_STRING_TO_TSTR (parameters, parmext);
    if (STRINGP (current_dir))
      LOCAL_FILE_FORMAT_TO_TSTR (current_dir, path);
    if (STRINGP (document))
      LOCAL_FILE_FORMAT_MAYBE_URL_TO_TSTR (document, doc);

    ret = (int) qxeShellExecute (NULL, opext, doc, parmext, path,
				 (INTP (show_flag) ?
				  XINT (show_flag) : SW_SHOWDEFAULT));
  }

  if (ret <= 32)
    {
      /* Convert to more standard errors */
#define FROB(a, b) if (ret == a) ret = b
      FROB (SE_ERR_ACCESSDENIED, ERROR_ACCESS_DENIED);
      FROB (SE_ERR_ASSOCINCOMPLETE, ERROR_NO_ASSOCIATION);
      FROB (SE_ERR_DDEBUSY, ERROR_DDE_FAIL);
      FROB (SE_ERR_DDEFAIL, ERROR_DDE_FAIL);
      FROB (SE_ERR_DDETIMEOUT, ERROR_DDE_FAIL);
      FROB (SE_ERR_DLLNOTFOUND, ERROR_DLL_NOT_FOUND);
      FROB (SE_ERR_FNF, ERROR_FILE_NOT_FOUND);
      FROB (SE_ERR_NOASSOC, ERROR_NO_ASSOCIATION);
      FROB (SE_ERR_OOM, ERROR_NOT_ENOUGH_MEMORY);
      FROB (SE_ERR_PNF, ERROR_PATH_NOT_FOUND);
      FROB (SE_ERR_SHARE, ERROR_SHARING_VIOLATION);
#undef FROB
      
      mswindows_report_process_error ("Running ShellExecute",
				      ret == ERROR_PATH_NOT_FOUND ?
				      list4 (Qunbound, operation, document,
					     current_dir) :
				      list3 (Qunbound, operation, document),
				      ret);
    }

  return Qt;
}

#ifdef CYGWIN
DEFUN ("mswindows-cygwin-to-win32-path", Fmswindows_cygwin_to_win32_path, 1, 1, 0, /*
Get the cygwin environment to convert the Unix PATH to win32 format.
No expansion is performed, all conversion is done by the cygwin runtime.
*/
       (path))
{
  Ibyte *p;
  CHECK_STRING (path);

  /* There appears to be a bug in the cygwin conversion routines in
     that they are not idempotent. */
  p = XSTRING_DATA (path);
  if (isalpha (p[0]) && (IS_DEVICE_SEP (p[1])))
    return path;

  /* Use mule and cygwin-safe APIs top get at file data. */
  LOCAL_TO_WIN32_FILE_FORMAT (p, p);
  return build_intstring (p);
}
#endif

struct read_link_hash
{
  Ibyte *resolved;
  DWORD ticks;
};

static Ibyte *
mswindows_read_link_1 (const Ibyte *fname)
{
#ifdef NO_CYGWIN_COM_SUPPORT
  return NULL;
#else
  Ibyte *retval = NULL;
  Extbyte *fnameext;
  HANDLE fh;
  struct read_link_hash *rlh;
  DWORD ticks;

  /* The call below to resolve a link is rather time-consuming.
     I tried implementing a simple cache based on creation and write time
     of the file, but that didn't help enough -- maybe 30% faster but still
     a lot of time spent here.  So just do something cheesy and don't
     check again if we've recently (< a second) done so. */

  if (!mswindows_read_link_hash)
    mswindows_read_link_hash = make_string_hash_table (1000);
  C_STRING_TO_TSTR (fname, fnameext);

  /* See if we can find a cached value. */

  /* The intermediate cast fools gcc into not outputting strict-aliasing
     complaints */
  ticks = GetTickCount ();
  if (!gethash (fname, mswindows_read_link_hash,
		(const void **) (void *) &rlh))
    {
      rlh = xnew_and_zero (struct read_link_hash);
      puthash (qxestrdup (fname), rlh, mswindows_read_link_hash);
    }
  else if (ticks - rlh->ticks < 1000)
    {
      return rlh->resolved ? qxestrdup (rlh->resolved) : NULL;
    }

  rlh->ticks = ticks;

  /* Retrieve creation/write time of link file. */

  /* No access rights required to get info.  */
  if ((fh = qxeCreateFile (fnameext, 0, 0, NULL, OPEN_EXISTING, 0, NULL))
      == INVALID_HANDLE_VALUE)
    {
      CloseHandle (fh);
      return NULL;
    }

  CloseHandle (fh);

  /* ####
		   
  Note the following in the docs:
		   
  Note: The IShellLink interface has an ANSI version
  (IShellLinkA) and a Unicode version (IShellLinkW). The
  version that will be used depends on whether you compile
  for ANSI or Unicode. However, Microsoft® Windows 95 and
  Microsoft® Windows 98 only support IShellLinkA.
		   
  We haven't yet implemented COM support in the
  Unicode-splitting library.  I don't quite understand how
  COM works yet, but it looks like what's happening is
  that the ShellLink class implements both the IShellLinkA
  and IShellLinkW interfaces.  To make this work at
  run-time, we have to do something like this:
		   
  -- define a new interface qxeIShellLink that uses
  Extbyte * instead of LPSTR or LPWSTR. (not totally
  necessary since Extbyte * == LPSTR).
		   
  -- define a new class qxeShellLink that implements
  qxeIShellLink.  the methods on this class need to create
  a shadow ShellLink object to do all the real work, and
  call the corresponding function from either the
  IShellLinkA or IShellLinkW interfaces on this object,
  depending on whether XEUNICODE_P is defined.
		   
  -- with appropriate preprocessor magic, of course, we
  could make things appear transparent; but we've decided
  not to do preprocessor magic for the moment.
  */

  /* #### Not Unicode-split for the moment; we have to do it
     ourselves. */
  if (XEUNICODE_P)
    {
      IShellLinkW *psl;

      if (CoCreateInstance (
			    XECOMID (CLSID_ShellLink),
			    NULL,
			    CLSCTX_INPROC_SERVER,
			    XECOMID (IID_IShellLinkW),
			    &VOIDP_CAST (psl)) == S_OK)
	{
	  IPersistFile *ppf;

	  if (XECOMCALL2 (psl, QueryInterface,
			  XECOMID (IID_IPersistFile),
			  &VOIDP_CAST (ppf)) == S_OK)
	    {
	      Extbyte *fname_unicode;
	      WIN32_FIND_DATAW wfd;
	      LPWSTR resolved = alloca_array (WCHAR, PATH_MAX_EXTERNAL + 1);

	      /* Always Unicode.  Not obvious from the
		 IPersistFile documentation, but look under
		 "Shell Link" for example code. */
	      fname_unicode = fnameext;

	      if (XECOMCALL2 (ppf, Load,
			      (LPWSTR) fname_unicode,
			      STGM_READ) == S_OK &&
		  /* #### YUCK!  Docs read

		  cchMaxPath 

		  Maximum number of bytes to copy to the buffer pointed
		  to by the pszFile parameter.

		  But "cch" means "count of characters", not bytes.
		  I'll assume the doc writers messed up and the
		  programmer was correct.  Also, this approach is safe
		  even if it's actually the other way around. */
#if defined (CYGWIN_HEADERS) && W32API_INSTALLED_VER < W32API_VER(2,2)
		  /* Another Cygwin prototype error,
		     fixed in v2.2 of w32api */
		  XECOMCALL4 (psl, GetPath, (LPSTR) resolved,
			      PATH_MAX_EXTERNAL, &wfd, 0)
#else
		  XECOMCALL4 (psl, GetPath, resolved,
			      PATH_MAX_EXTERNAL, &wfd, 0)
#endif
		  == S_OK)
		TSTR_TO_C_STRING_MALLOC (resolved, retval);

	      XECOMCALL0 (ppf, Release);
	    }

	  XECOMCALL0 (psl, Release);
	}
    }
  else
    {
      IShellLinkA *psl;

      if (CoCreateInstance (
			    XECOMID (CLSID_ShellLink),
			    NULL,
			    CLSCTX_INPROC_SERVER,
			    XECOMID (IID_IShellLinkA),
			    &VOIDP_CAST (psl)) == S_OK)
	{
	  IPersistFile *ppf;

	  if (XECOMCALL2 (psl, QueryInterface,
			  XECOMID (IID_IPersistFile),
			  &VOIDP_CAST (ppf)) == S_OK)
	    {
	      Extbyte *fname_unicode;
	      WIN32_FIND_DATAA wfd;
	      LPSTR resolved = alloca_array (CHAR, PATH_MAX_EXTERNAL + 1);

	      /* Always Unicode.  Not obvious from the
		 IPersistFile documentation, but look under
		 "Shell Link" for example code. */
	      C_STRING_TO_EXTERNAL (fname, fname_unicode,
				    Qmswindows_unicode);

	      if (XECOMCALL2 (ppf, Load,
			      (LPWSTR) fname_unicode,
			      STGM_READ) == S_OK
		  && XECOMCALL4 (psl, GetPath, resolved,
				 PATH_MAX_EXTERNAL, &wfd, 0) == S_OK)
		TSTR_TO_C_STRING_MALLOC (resolved, retval);

	      XECOMCALL0 (ppf, Release);
	    }

	  XECOMCALL0 (psl, Release);
	}
    }

  /* Cache newly found value */
  if (rlh->resolved)
    xfree (rlh->resolved, Ibyte *);
  rlh->resolved = retval ? qxestrdup (retval) : NULL;

  return retval;
#endif /* NO_CYGWIN_COM_SUPPORT */
}

/* Resolve a file that may be a shortcut.  Accepts either a file ending
   with .LNK or without the ending.  If a shortcut is found, returns
   a value that you must xfree(); otherwise NULL. */

Ibyte *
mswindows_read_link (const Ibyte *fname)
{
  int len = qxestrlen (fname);
  if (len > 4 && !qxestrcasecmp_ascii (fname + len - 4, ".LNK"))
    return mswindows_read_link_1 (fname);
  else
    {
      DECLARE_EISTRING (name2);

      eicpy_rawz (name2, fname);
      eicat_ascii (name2, ".LNK");
      return mswindows_read_link_1 (eidata (name2));
    }
}


#if defined (WIN32_NATIVE) || defined (CYGWIN_BROKEN_SIGNALS)

/* setitimer() does not exist on native MS Windows, and appears broken
   on Cygwin (random lockups when BROKEN_SIGIO is defined), so we
   emulate in both cases by using multimedia timers.  Furthermore,
   the lockups still occur on Cygwin even when we do nothing but
   use the standard signalling mechanism -- so we have to emulate
   that, too. (But only for timeouts -- we have to use the standard
   mechanism for SIGCHLD.  Yuck.)
 */

/*--------------------------------------------------------------------*/
/*                             Signal support                         */
/*--------------------------------------------------------------------*/

#define sigmask(nsig) (1U << nsig)

/* We can support as many signals as fit into word */
#define SIG_MAX 32

/* Signal handlers. Initial value = 0 = SIG_DFL */
static mswindows_sighandler signal_handlers[SIG_MAX] = {0};

/* Signal block mask: bit set to 1 means blocked */
unsigned signal_block_mask = 0;

/* Signal pending mask: bit set to 1 means sig is pending */
unsigned signal_pending_mask = 0;

mswindows_sighandler
mswindows_sigset (int nsig, mswindows_sighandler handler)
{
  /* We delegate some signals to the system function */
  if (nsig == SIGFPE || nsig == SIGABRT || nsig == SIGINT)
    return signal (nsig, handler);

  if (nsig < 0 || nsig > SIG_MAX)
    {
      errno = EINVAL;
      return NULL;
    }

  /* Store handler ptr */
  {
    mswindows_sighandler old_handler = signal_handlers[nsig];
    signal_handlers[nsig] = handler;
    return old_handler;
  }
}

int
mswindows_sighold (int nsig)
{
  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  signal_block_mask |= sigmask (nsig);
  return 0;
}

int
mswindows_sigrelse (int nsig)
{
  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  signal_block_mask &= ~sigmask (nsig);

  if (signal_pending_mask & sigmask (nsig))
    mswindows_raise (nsig);

  return 0;
}

int
mswindows_sigpause (int UNUSED (nsig))
{
  /* This is currently not called, because the only call to sigpause
     inside XEmacs is with SIGCHLD parameter. Just in case, we put an
     assert here, so anyone who adds a call to sigpause will be surprised
     (or surprise someone else...) */
  assert (0);
  return 0;
}

int
mswindows_raise (int nsig)
{
  /* We delegate some raises to the system routine */
  if (nsig == SIGFPE || nsig == SIGABRT || nsig == SIGINT)
    return raise (nsig);

  if (nsig < 0 || nsig > SIG_MAX)
    return errno = EINVAL;

  /* If the signal is blocked, remember to issue later */
  if (signal_block_mask & sigmask (nsig))
    {
      signal_pending_mask |= sigmask (nsig);
      return 0;
    }

  if (signal_handlers[nsig] == SIG_IGN)
    return 0;

  if (signal_handlers[nsig] != SIG_DFL)
    {
      (*signal_handlers[nsig]) (nsig);
      return 0;
    }

  /* Default signal actions */
  if (nsig == SIGALRM || nsig == SIGPROF)
    exit (3);

  /* Other signals are ignored by default */
  return 0;
}


/*--------------------------------------------------------------------*/
/*                               Async timers                         */
/*--------------------------------------------------------------------*/

/* We emulate two timers, one for SIGALRM, another for SIGPROF.

   itimerproc() function has an implementation limitation: it does
   not allow to set *both* interval and period. If an attempt is
   made to set both, and then they are unequal, the function
   asserts.

   Minimum timer resolution on Win32 systems varies, and is greater
   than or equal than 1 ms. The resolution is always wrapped not to
   attempt to get below the system defined limit.
   */

/* Timer precision, denominator of one fraction: for 100 ms
   interval, request 10 ms precision
   */
const int setitimer_helper_timer_prec = 10;

/* Last itimervals, as set by calls to setitimer */
static struct itimerval it_alarm;
static struct itimerval it_prof;

/* Timer IDs as returned by MM */
MMRESULT tid_alarm = 0;
MMRESULT tid_prof = 0;

static void CALLBACK
setitimer_helper_proc (UINT UNUSED (uID), UINT UNUSED (uMsg), DWORD dwUser,
		       DWORD UNUSED (dw1), DWORD UNUSED (dw2))
{
  /* Just raise the signal indicated by the dwUser parameter */
  mswindows_raise (dwUser);
}

/* Divide time in ms specified by IT by DENOM. Return 1 ms
   if division results in zero */
static UINT
setitimer_helper_period (const struct itimerval *it, UINT denom)
{
  static TIMECAPS time_caps;

  UINT res;
  const struct timeval *tv = 
    (it->it_value.tv_sec == 0 && it->it_value.tv_usec == 0)
    ? &it->it_interval : &it->it_value;
  
  /* Zero means stop timer */
  if (tv->tv_sec == 0 && tv->tv_usec == 0)
    return 0;
  
  /* Convert to ms and divide by denom */
  res = (tv->tv_sec * 1000 + (tv->tv_usec + 500) / 1000) / denom;
  
  /* Converge to minimum timer resolution */
  if (time_caps.wPeriodMin == 0)
      timeGetDevCaps (&time_caps, sizeof(time_caps));

  if (res < time_caps.wPeriodMin)
    res = time_caps.wPeriodMin;

  return res;
}

static int
setitimer_helper (const struct itimerval *itnew,
		  struct itimerval *itold, struct itimerval *itcurrent,
		  MMRESULT *tid, DWORD sigkind)
{
  UINT delay, resolution, event_type;

  /* First stop the old timer */
  if (*tid)
    {
      timeKillEvent (*tid);
      timeEndPeriod (setitimer_helper_period (itcurrent,
					      setitimer_helper_timer_prec));
      *tid = 0;
    }

  /* Return old itimerval if requested */
  if (itold)
    *itold = *itcurrent;

  *itcurrent = *itnew;

  /* Determine if to start new timer */
  delay = setitimer_helper_period (itnew, 1);
  if (delay)
    {
      resolution = setitimer_helper_period (itnew,
					    setitimer_helper_timer_prec);
      event_type = (itnew->it_value.tv_sec == 0 &&
		    itnew->it_value.tv_usec == 0)
	? TIME_ONESHOT : TIME_PERIODIC;
      timeBeginPeriod (resolution);
      *tid = timeSetEvent (delay, resolution, setitimer_helper_proc, sigkind,
			   event_type);
    }

  return !delay || *tid;
}
 
int
mswindows_setitimer (int kind, const struct itimerval *itnew,
		     struct itimerval *itold)
{
  /* In this version, both interval and value are allowed
     only if they are equal. */
  assert ((itnew->it_value.tv_sec == 0 && itnew->it_value.tv_usec == 0)
	  || (itnew->it_interval.tv_sec == 0 &&
	      itnew->it_interval.tv_usec == 0)
	  || (itnew->it_value.tv_sec == itnew->it_interval.tv_sec &&
	      itnew->it_value.tv_usec == itnew->it_interval.tv_usec));

  if (kind == ITIMER_REAL)
    return setitimer_helper (itnew, itold, &it_alarm, &tid_alarm, SIGALRM);
  else if (kind == ITIMER_PROF)
    return setitimer_helper (itnew, itold, &it_prof, &tid_prof, SIGPROF);
  else
    return errno = EINVAL;
}

#endif /* defined (WIN32_NATIVE) || defined (CYGWIN_BROKEN_SIGNALS) */


void
syms_of_win32 (void)
{
  DEFSUBR (Fmswindows_shell_execute);
#ifdef CYGWIN
  DEFSUBR (Fmswindows_cygwin_to_win32_path);
#endif
}

void
vars_of_win32 (void)
{
  DEFVAR_LISP ("mswindows-downcase-file-names",
	       &Vmswindows_downcase_file_names /*
Non-nil means convert all-upper case file names to lower case.
This applies when performing completions and file name expansion.
*/ );
  Vmswindows_downcase_file_names = Qnil;

  DEFVAR_BOOL ("mswindows-shortcuts-are-symlinks",
	       &mswindows_shortcuts_are_symlinks /*
Non-nil means shortcuts (.LNK files) are treated as symbolic links.
This works also for symlinks created under Cygwin, because they use .LNK
files to implement symbolic links.
*/ );
  mswindows_shortcuts_are_symlinks = 1;
}

void
init_win32 (void)
{
  init_potentially_nonexistent_functions ();
}

void
init_win32_very_very_early (void)
{
  mswindows_windows9x_p = GetVersion () & 0x80000000;
}
