/* Virtual diry bit implementation for XEmacs.
   Copyright (C) 2005 Marcus Crestani.

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

#include <config.h>
#include "lisp.h"

void
fake_error (void)
{
  fprintf (stderr, "Incremental garbage collection not yet available on this");
  fprintf (stderr, "system.\nDon't try to set allow-incremental-gc to t.\n");
  ABORT ();
}

void 
vdb_install_signal_handler (void)
{
  allow_incremental_gc = 0;
}

void
vdb_protect (void *UNUSED (ptr), EMACS_INT UNUSED (len))
{
  fake_error ();
}

void
vdb_unprotect (void *UNUSED (ptr), EMACS_INT UNUSED (len))
{
  fake_error ();
}
