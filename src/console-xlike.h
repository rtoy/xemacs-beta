/* Shared event code between X and GTK.
   Copyright (C) 2003, 2005 Ben Wing.

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

#ifndef INCLUDED_console_xlike_h_
#define INCLUDED_console_xlike_h_

#include <X11/Xlib.h>

struct xlike_event_key_data
{
  int MetaMask, HyperMask, SuperMask, AltMask, ModeMask;
  KeySym lock_interpretation;

  XModifierKeymap *x_modifier_keymap;

  KeySym *x_keysym_map;
  int x_keysym_map_min_code;
  int x_keysym_map_max_code;
  int x_keysym_map_keysyms_per_code;
  Lisp_Object x_keysym_map_hash_table;
};

void xlike_init_modifier_mapping (struct device *d,
				  struct xlike_event_key_data *xd);
void xlike_reset_key_mapping (struct device *d,
			      struct xlike_event_key_data *xd);
void xlike_reset_modifier_mapping (struct device *d,
				   struct xlike_event_key_data *xd);
void free_xlike_event_key_data (struct xlike_event_key_data *xd);
Lisp_Object xlike_keysym_to_emacs_keysym (long keysym, int simple_p);

#endif /* INCLUDED_console_xlike_h_ */
