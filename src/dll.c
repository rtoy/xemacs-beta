/* Lisp interface to dynamic loading.
   Copyright (C) 1998  Joshua Rowe.
   Additional cleanup by Hrvoje Niksic.

This file is part of XEmacs.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* A shared object may have the following symbols defined:
      syms_of
      vars_of
      complex_vars_of
   They are called in that order.  Each takes and returns void
   arguments.

   All of this needs lots and LOTS of work.  Some things to work on:

   1) A good foreign interface.  We probably need to get rid of
   syms_of and similar junk, and define a more normal interfacing to
   the outside world, e.g. an init_emacs_module() function.  See below
   for more discussion about it.  Also, we need a modules/ directory
   with a few nice sample modules, a sample Makefile, etc. so people
   can start hacking.

   2) I'm getting coredumps very often -- practically every time I
   compile without USE_MINIMAL_TAGBITS, and even with it sometimes.  I
   wasn't able to resolve these.

   3) All of this is sooo simple-minded.  As it gets more complex,
   we'll have to look at how others have done similar things
   (e.g. Perl and Zsh 3.1), to avoid botching it up.  */

#include <config.h>
#include "lisp.h"
#include "emacsfns.h"
#include "buffer.h"

#include <stdio.h>
#include "sysdll.h"
#include <errno.h>


DEFUN ("dll-open", Fdll_open, 1, 1, "FShared object: ", /*
Load LIBRARY as a shared object file.

After the LIBRARY is dynamically linked with the executable, the
following functions are called:

  syms_of(),		containing definitions of symbols and subr's;
  vars_of(),		containing definitions of variables;
  complex_vars_of(),	containing complex definitions of variables.

After this point, any lisp symbols defined in the shared object are
available for use.
*/
       (library))
{
  /* This function can GC */
  dll_handle *handle;
  char *file;
  void (*function)();

  CHECK_STRING (library);
  library = Fexpand_file_name (library, Qnil);

  file = XSTRING_DATA (library);
  /* #### Is this right? */
  GET_C_CHARPTR_EXT_FILENAME_DATA_ALLOCA (file, file);

  handle = dll_open (file);
  if (handle == NULL)
    {
      signal_error (Qerror,
		    list3 (build_translated_string ("Cannot load shared library"),
			   library, build_translated_string (dll_error (handle))));
    }

  /* #### This looks unnecessary here, because at this time one
     initialization function is fully sufficient.  However, I am not
     removing this support, since we may wish to add mechanisms for
     static linking, which would have invoke these function via normal
     paths.

     #### But then this is not sufficient, because one could as well
     honor specifier_vars_of_foo(), etc.  Maybe we should scrap it
     after all.

     #### What if one of the first two functions signal an error?
     Should we take care to execute the other two?  My fingers are
     getting itchy!  */

  function = dll_function (handle, "syms_of");
  if (function)
    (*function) ();

  function = dll_function (handle, "vars_of");
  if (function)
    (*function) ();

  function = dll_function (handle, "complex_vars_of");
  if (function)
    (*function) ();

  return Qnil;
}

void syms_of_dll ()
{
  DEFSUBR (Fdll_open);
}
