/* sysdll.c --- system dependent support for dynamic linked libraries
   Copyright (C) 1998 Free Software Foundation, Inc.
   Copyright (C) 2010 Ben Wing.

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

/* This file has been Mule-ized, Ben Wing, 1-26-10. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include "lisp.h"
#include "sysdll.h"

#ifdef DLSYM_NEEDS_UNDERSCORE
#define MAYBE_PREPEND_UNDERSCORE(n) do {		\
  Ibyte *buf = alloca_array (Ibyte, qxestrlen (n) + 2);	\
  *buf = '_';						\
  qxestrcpy (buf + 1, n);				\
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
      soname = LISP_STRING_TO_EXTERNAL (fname, Qdll_filename_encoding);
    }
  return (dll_handle) dlopen (soname, RTLD_NOW);
}

int
dll_close (dll_handle h)
{
  return dlclose ((void *) h);
}

dll_func
dll_function (dll_handle h, const Ibyte *n)
{
  Extbyte *next;
  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_function_name_encoding);
  return (dll_func) dlsym ((void *) h, next);
}

dll_var
dll_variable (dll_handle h, const Ibyte *n)
{
  Extbyte *next;
  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_variable_name_encoding);
  return (dll_var)dlsym ((void *)h, next);
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
  return build_extstring (msg, Qerror_message_encoding);
}

#elif defined(HAVE_SHL_LOAD)
/* This is the HP/UX version */
#include <dl.h>
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
      soname = LISP_STRING_TO_EXTERNAL (fname, Qdll_filename_encoding);
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
  return build_ascstring ("Generic shared library error");
}

#elif defined (WIN32_NATIVE) || defined (CYGWIN)

#include "syswindows.h"
#include "sysfile.h"

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
      LISP_LOCAL_FILE_FORMAT_TO_TSTR (fname, soname);
    }
  return (dll_handle) qxeLoadLibrary (soname);
}

int
dll_close (dll_handle h)
{
  return FreeLibrary ((HMODULE) h);
}

dll_func
dll_function (dll_handle h, const Ibyte *n)
{
  Extbyte *next = ITEXT_TO_EXTERNAL (n, Qmswindows_multibyte);
  return (dll_func) GetProcAddress ((HINSTANCE) h, next);
}

dll_func
dll_variable (dll_handle h, const Ibyte *n)
{
  Extbyte *next = ITEXT_TO_EXTERNAL (n, Qmswindows_multibyte);
  return (dll_func) GetProcAddress ((HINSTANCE) h, next);
}

Lisp_Object
dll_error ()
{
  Ascbyte err[32];
  snprintf (err, 32, "Windows DLL Error %lu", GetLastError ());
  return build_ascstring (err);
}
#elif defined (HAVE_DYLD)
/* This section supports MacOSX dynamic libraries. Dynamically
   loadable libraries must be compiled as bundles, not dynamiclibs.
*/

#include <mach-o/dyld.h>

dll_handle
dll_open (Lisp_Object fname)
{
  Extbyte *soname;
  NSObjectFileImage file;
  NSModule out;
  NSObjectFileImageReturnCode ret;

  /*
   * MacOS X dll support is for bundles, not the current executable, so return
   * NULL is this case.  However, dll_function() uses a special hack where a
   * NULL handle can be used to find executable symbols.  This satisfies the
   * needs of ui-gtk.c but is not a general solution.
   */
  if (NILP (fname))
    {
      return NULL;
    }
  else
    {
      soname = LISP_STRING_TO_EXTERNAL (fname, Qdll_filename_encoding);
    }
  ret = NSCreateObjectFileImageFromFile (soname, &file);
  if (ret != NSObjectFileImageSuccess)
    return NULL;
  out = NSLinkModule (file, soname,
		      NSLINKMODULE_OPTION_BINDNOW |
		      NSLINKMODULE_OPTION_PRIVATE |
		      NSLINKMODULE_OPTION_RETURN_ON_ERROR);
  return (dll_handle) out;
}

int
dll_close (dll_handle h)
{
  return NSUnLinkModule ((NSModule) h, NSUNLINKMODULE_OPTION_NONE);
}

/* Given an address, return the mach_header for the image containing it
 * or zero if the given address is not contained in any loaded images.
 *
 * Note: image_for_address(), my_find_image() and search_linked_libs() are
 * based on code from the dlcompat library
 * (http://www.opendarwin.org/projects/dlcompat).
 */

static const struct mach_header *
image_for_address (void *address)
{
  unsigned long i;
  unsigned long count = _dyld_image_count ();
  const struct mach_header *mh = 0;

  for (i = 0; i < count; i++)
    {
      unsigned long addr = (unsigned long) address -
	_dyld_get_image_vmaddr_slide (i);
      mh = _dyld_get_image_header (i);

      if (mh)
	{
	  struct load_command *lc =
	    (struct load_command *) ((Rawbyte *) mh +
				     sizeof(struct mach_header));
	  unsigned long j;

	  for (j = 0; j < mh->ncmds;
	       j++, lc = (struct load_command *) ((Rawbyte *)lc + lc->cmdsize))
	    {
	      if (LC_SEGMENT == lc->cmd &&
		  addr >= ((struct segment_command *)lc)->vmaddr &&
		  addr <
		  ((struct segment_command *)lc)->vmaddr +
		  ((struct segment_command *)lc)->vmsize)
		{
		  goto image_found;
		}
	    }
	}

      mh = 0;
    }

 image_found:
  return mh;
}

static const struct mach_header *
my_find_image (const char *name)
{
  const struct mach_header *mh = (struct mach_header *)
    NSAddImage (name, NSADDIMAGE_OPTION_RETURN_ONLY_IF_LOADED |
		NSADDIMAGE_OPTION_RETURN_ON_ERROR);

  if (!mh)
    {
      int count = _dyld_image_count ();
      int j;

      for (j = 0; j < count; j++)
	{
	  const char *id = _dyld_get_image_name (j);

	  if (!strcmp (id, name))
	    {
	      mh = _dyld_get_image_header (j);
	      break;
	    }
	}
    }

  return mh;
}

/*
 * dyld adds libraries by first adding the directly dependant libraries in
 * link order, and then adding the dependencies for those libraries, so we
 * should do the same... but we don't bother adding the extra dependencies, if
 * the symbols are neither in the loaded image nor any of it's direct
 * dependencies, then it probably isn't there.
 */
static NSSymbol
search_linked_libs (const struct mach_header * mh, const Ibyte *symbol)
{
  unsigned long n;
  NSSymbol nssym = 0;

  struct load_command *lc =
    (struct load_command *) ((Rawbyte *) mh + sizeof (struct mach_header));

  for (n = 0; n < mh->ncmds;
       n++, lc = (struct load_command *) ((Rawbyte *) lc + lc->cmdsize))
    {
      if ((LC_LOAD_DYLIB == lc->cmd) || (LC_LOAD_WEAK_DYLIB == lc->cmd))
	{
	  struct mach_header *wh;

	  if ((wh = (struct mach_header *)
	       my_find_image((Rawbyte *)
			     (((struct dylib_command *) lc)->
			      dylib.name.offset + (Rawbyte *) lc))))
	    {
	      Extbyte *symext =
		ITEXT_TO_EXTERNAL (symbol, Qdll_symbol_encoding);
	      if (NSIsSymbolNameDefinedInImage (wh, symext))
		{
		  nssym =
		    NSLookupSymbolInImage
		    (wh,
		     symext,
		     NSLOOKUPSYMBOLINIMAGE_OPTION_BIND |
		     NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
		  break;
		}
	    }
	}
    }

  return nssym;
}

dll_func
dll_function (dll_handle h, const Ibyte *n)
{
  NSSymbol sym = 0;
  Extbyte *next;

  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_function_name_encoding);

  /* NULL means the program image and shared libraries, not bundles. */

  if (h == NULL)
    {
      /* NOTE: This assumes that this function is included in the main program
	 and not in a shared library. */
      const struct mach_header* my_mh =
	image_for_address ((void*) &dll_function);

      if (NSIsSymbolNameDefinedInImage (my_mh, next))
	{
	  sym =
	    NSLookupSymbolInImage
	    (my_mh,
	     next,
	     NSLOOKUPSYMBOLINIMAGE_OPTION_BIND |
	     NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
	}

      if (!sym)
	{
	  sym = search_linked_libs (my_mh, n);
	}
    }
  else
    {
      sym = NSLookupSymbolInModule ((NSModule)h, next);
    }

   if (sym == 0) return 0;
   return (dll_func) NSAddressOfSymbol (sym);
 }

dll_var
dll_variable (dll_handle h, const Ibyte *n)
{
  NSSymbol sym;
  Extbyte *next;

  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_variable_name_encoding);

  sym = NSLookupSymbolInModule ((NSModule) h, n);
  if (sym == 0) return 0;
  return (dll_var) NSAddressOfSymbol (sym);
}

Lisp_Object
dll_error (void)
{
  NSLinkEditErrors c;
  int errorNumber;
  const Extbyte *fileNameWithError, *errorString;
  NSLinkEditError (&c, &errorNumber, &fileNameWithError, &errorString);
  return build_extstring (errorString, Qerror_message_encoding);
}
#elif HAVE_LTDL
/* Libtool's libltdl */
#include <ltdl.h>

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
      soname = LISP_STRING_TO_EXTERNAL (fname, Qdll_filename_encoding);
    }
  return (dll_handle) lt_dlopen (soname);
}

int
dll_close (dll_handle h)
{
  return lt_dlclose ((lt_dlhandle) h);
}

dll_func
dll_function (dll_handle h, const Ibyte *n)
{
  Extbyte *next;
  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_function_name_encoding);
  return (dll_func) lt_dlsym ((lt_dlhandle) h, next);
}

dll_var
dll_variable (dll_handle h, const Ibyte *n)
{
  Extbyte *next;
  MAYBE_PREPEND_UNDERSCORE (n);
  next = ITEXT_TO_EXTERNAL (n, Qdll_variable_name_encoding);
  return (dll_var) lt_dlsym ((lt_dlhandle) h, next);
}

Lisp_Object
dll_error (void)
{
  return build_extstring (lt_dlerror (), Qerror_message_encoding);
}
#else
/* Catchall if we don't know about this system's method of dynamic loading */
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
dll_function (dll_handle h, const Ibyte *n)
{
  return NULL;
}

dll_func
dll_variable (dll_handle h, const Ibyte *n)
{
  return NULL;
}

Lisp_Object
dll_error (void)
{
  return build_ascstring ("Shared libraries not implemented on this system");
}
#endif /* System conditionals */

#endif /* HAVE_SHLIB */
