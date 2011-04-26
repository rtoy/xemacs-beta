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

#include "lisp.h"

#ifndef INCLUDED_vdb_h_
#define INCLUDED_vdb_h_


/*--- prototypes -------------------------------------------------------*/

BEGIN_C_DECLS

/* Platform dependent signal handling: */

/* Install the platform-dependent signal handler. */
void vdb_install_signal_handler (void);

/* Platform dependent memory protection. */
void vdb_protect (void *ptr, EMACS_INT len);
void vdb_unprotect (void *ptr, EMACS_INT len);



/* Common (platform independent) virtual diry bit stuff: */

/* Start the write barrier.  This function is called when a garbage
   collection is suspendend and the client is resumed. */
void vdb_start_dirty_bits_recording (void);
/* Stop the write barrier.  This function is called when the client is
   suspendend and garbage collection is resumed. */
void vdb_stop_dirty_bits_recording (void);

/* Record page faults: Add the object pointed to by addr to the write
   barrer's internal data structure that stores modified objects.
   This function is called by the write barrier's fault handler. */
void vdb_designate_modified (void *addr);

/* Propagate page faults to garbage collector: Read out the write
   barrier's internal data structure that stores modified objects and
   pass the information to the garbage collector.  This function is
   called by vdb_stop_dirty_bits_recording().  Return how many objects
   have to be re-examined by the garbage collector. */
int vdb_read_dirty_bits (void);

/* Provides Lisp functions for testing vdb implementation. */
void syms_of_vdb (void);

END_C_DECLS

#endif /* INCLUDED_vdb_h_ */
