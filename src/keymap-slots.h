/* Definitions of marked slots in keymaps.
   Copyright (C) 1985, 1991-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2010 Ben Wing.

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

/* Synched up with: Not in FSF.  Split out of keymap.c. */

/* We define the Lisp_Objects in the keymap structure in a separate
   file because there are numerous places we want to iterate over them,
   such as when defining them in the structure, initializing them, or
   marking them.

   To use, define MARKED_SLOT before including this file.  No need to
   undefine; that happens automatically.

   MARKED_SLOT_NOCOMPARE is used to indicate a slot that should not be
   compared in the equal() method or hashed in the hash() method --
   basically, a slot used for caching, debugging, etc. instead of for
   defining a property of the keymap.
*/

#ifndef MARKED_SLOT_NOCOMPARE
#define MARKED_SLOT_NOCOMPARE MARKED_SLOT
#endif

  MARKED_SLOT (parents)		 /* Keymaps to be searched after this one.
				    An ordered list */
  MARKED_SLOT (prompt)           /* Qnil or a string to print in the minibuffer
                                    when reading from this keymap */
  MARKED_SLOT (table)		 /* The contents of this keymap */
  MARKED_SLOT_NOCOMPARE (inverse_table)	 /* The inverse mapping of the above */
  MARKED_SLOT (default_binding)  /* Use this if no other binding is found
                                    (this overrides parent maps and the
                                    normal global-map lookup). */
  MARKED_SLOT_NOCOMPARE (sub_maps_cache) /* Cache of directly inferior
					    keymaps; This holds an alist,
					    of the key and the maps, or the
					    modifier bit and the map.  If
					    this is the symbol t, then the
					    cache needs to be recomputed. */
  MARKED_SLOT_NOCOMPARE (name)           /* Just for debugging convenience */

#undef MARKED_SLOT
#undef MARKED_SLOT_NOCOMPARE
