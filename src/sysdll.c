/* sysdll.c --- system dependent support for dynamic linked libraries
   Copyright (C) 1998 Free Software Foundation, Inc.
   Author:  William Perry <wmperry@aventail.com>

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
02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include "lisp.h"
#include "sysdll.h"

#ifdef DLSYM_NEEDS_UNDERSCORE
#define MAYBE_PREPEND_UNDERSCORE(n) do {		\
  CIbyte *buf = alloca_array (CIbyte, strlen (n) + 2);	\
  *buf = '_';						\
  strcpy (buf + 1, n);					\
  n = buf;						\
} while (0)
#else
#define MAYBE_PREPEND_UNDERSCORE(n)
#endif

/* This whole file is conditional upon HAVE_SHLIB */
#ifdef HAVE_SHLIB

/* Thankfully, most systems follow the ELFish dlopen() method.
*/
#if defined(HAVE_DLOPEN)
#include <dlfcn.h>

#ifndef RTLD_LAZY
# ifdef DL_LAZY
#  define RTLD_LAZY DL_LAZY
# else
#  define RTLD_LAZY 1
# endif
#endif /* RTLD_LAZY isn't defined under FreeBSD - ick */

#ifndef RTLD_NOW
# ifdef DL_NOW
#  define RTLD_NOW DL_NOW
# else
#  define RTLD_NOW 2
# endif
#endif

int
dll_init (const Extbyte *arg)
{
  return 0;
}

dll_handle
dll_open (Lisp_Object fname)
{
  Extbyte *soname;

  if (NILP (fname))
    {
      soname = NULL;
    }
  else
    {
      LISP_STRING_TO_EXTERNAL (fname, soname, Qdll_filename_encoding);
    }
  return (dll_handle) dlopen (soname, RTLD_NOW);
}

int
dll_close (dll_handle h)
{
  return dlclose ((void *) h);
}

dll_func
dll_function (dll_handle h, const CIbyte *n)
{
  MAYBE_PREPEND_UNDERSCORE (n);
  return (dll_func) dlsym ((void *) h, n);
}

dll_var
dll_variable (dll_handle h, const CIbyte *n)
{
  MAYBE_PREPEND_UNDERSCORE (n);
  return (dll_var)dlsym ((void *)h, n);
}

Lisp_Object
dll_error ()
{
  const Extbyte *msg;
#if defined(HAVE_DLERROR) || defined(dlerror)
  msg = (const Extbyte *) dlerror ();
#elif defined(HAVE__DLERROR)
  msg = (const Extbyte *) _dlerror();
#else
  msg = (const Extbyte *) "Shared library error";
#endif
  return build_ext_string (msg, Qnative);
}

#elif defined(HAVE_SHL_LOAD)
/* This is the HP/UX version */
#include <dl.h>
int
dll_init (const Extbyte *arg)
{
  return 0;
}

dll_handle
dll_open (Lisp_Object fname)
{
  Extbyte *soname;

  if (NILP (fname))
    {
      soname = NULL;
    }
  else
    {
      LISP_STRING_TO_EXTERNAL (fname, soname, Qdll_filename_encoding);
    }
  return (dll_handle) shl_load (soname, BIND_DEFERRED, 0L);
}

int
dll_close (dll_handle h)
{
  return shl_unload ((shl_t) h);
}

dll_func
dll_function (dll_handle h, const CIbyte *n)
{
  long handle = 0L;

  if (shl_findsym ((shl_t *) &h, n, TYPE_PROCEDURE, &handle))
    return NULL;

  return (dll_func) handle;
}

dll_var
dll_variable (dll_handle h, const CIbyte *n)
{
  long handle = 0L;

  if (shl_findsym ((shl_t *) &h, n, TYPE_DATA, &handle))
    return NULL;

  return (dll_var) handle;
}

Lisp_Object
dll_error ()
{
  /* #### WTF?!  Shouldn't this at least attempt to get strerror or
     something?  --hniksic */
  return build_string ("Generic shared library error", Qnative);
}

#elif defined (WIN32_NATIVE) || defined (CYGWIN)

#include "syswindows.h"
#include "sysfile.h"

int
dll_init (const Extbyte *arg)
{
  return 0;
}

dll_handle
dll_open (Lisp_Object fname)
{
  Extbyte *soname;

  if (NILP (fname))
    {
      soname = NULL;
    }
  else
    {
      LOCAL_FILE_FORMAT_TO_TSTR (fname, soname);
    }
  return (dll_handle) qxeLoadLibrary (soname);
}

int
dll_close (dll_handle h)
{
  return FreeLibrary ((HMODULE) h);
}

dll_func
dll_function (dll_handle h, const CIbyte *n)
{
  return (dll_func) GetProcAddress ((HINSTANCE) h, n);
}

dll_func
dll_variable (dll_handle h, const CIbyte *n)
{
  return (dll_func) GetProcAddress ((HINSTANCE) h, n);
}

Lisp_Object
dll_error ()
{
  CIbyte err[32];
  snprintf (err, 32, "Windows DLL Error %lu", GetLastError ());
  return build_string (err);
}
#elif defined(HAVE_DYLD)
/* This section supports MacOSX dynamic libraries. Dynamically
   loadable libraries must be compiled as bundles, not dynamiclibs.
*/

#include <mach-o/dyld.h>

int
dll_init (const Extbyte *arg)
{
  return 0;
}

dll_handle
dll_open (Lisp_Object fname)
{
  Extbyte *soname;
  NSObjectFileImage file;
  NSModule out;
  NSObjectFileImageReturnCode ret;

  if (NILP (fname))
    {
      soname = NULL;
    }
  else
    {
      LISP_STRING_TO_EXTERNAL (fname, soname, Qdll_filename_encoding);
    }
  ret = NSCreateObjectFileImageFromFile(soname, &file);
  if (ret != NSObjectFileImageSuccess) {
    return NULL;
  }
  out = NSLinkModule(file, soname,
		     NSLINKMODULE_OPTION_BINDNOW |
		     NSLINKMODULE_OPTION_PRIVATE |
		     NSLINKMODULE_OPTION_RETURN_ON_ERROR);
  return (dll_handle)out;
}

int
dll_close (dll_handle h)
{
  return NSUnLinkModule((NSModule)h, NSUNLINKMODULE_OPTION_NONE);
}

dll_func
dll_function (dll_handle h, const CIbyte *n)
{
  NSSymbol sym;
  MAYBE_PREPEND_UNDERSCORE (n);
  sym = NSLookupSymbolInModule((NSModule)h, n);
  if (sym == 0) return 0;
  return (dll_func)NSAddressOfSymbol(sym);
}

dll_var
dll_variable (dll_handle h, const CIbyte *n)
{
  NSSymbol sym;
  MAYBE_PREPEND_UNDERSCORE (n);
  sym = NSLookupSymbolInModule((NSModule)h, n);
  if (sym == 0) return 0;
  return (dll_var)NSAddressOfSymbol(sym);
}

Lisp_Object
dll_error ()
{
  NSLinkEditErrors c;
  int errorNumber;
  const CIbyte *fileNameWithError, *errorString;
  NSLinkEditError(&c, &errorNumber, &fileNameWithError, &errorString);
  return build_ext_string (errorString, Qnative);
}
#else
/* Catchall if we don't know about this system's method of dynamic loading */
int
dll_init (const Extbyte *arg)
{
  return -1;
}

dll_handle
dll_open (Lisp_Object fname)
{
  return NULL;
}

int
dll_close (dll_handle h)
{
  return 0;
}

dll_func
dll_function (dll_handle h, const CIbyte *n)
{
  return NULL;
}

dll_func
dll_variable (dll_handle h, const CIbyte *n)
{
  return NULL;
}

Lisp_Object
dll_error ()
{
  return build_string ("Shared libraries not implemented on this system");
}
#endif /* System conditionals */

#endif /* HAVE_SHLIB */
