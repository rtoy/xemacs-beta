/* prototypes for keymap-hacking
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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


#ifndef INCLUDED_keymap_h_
#define INCLUDED_keymap_h_

typedef struct Lisp_Keymap Lisp_Keymap;

DECLARE_LISP_OBJECT (keymap, Lisp_Keymap);
#define XKEYMAP(x) XRECORD (x, keymap, Lisp_Keymap)
#define wrap_keymap(p) wrap_record (p, keymap)
#define KEYMAPP(x) RECORDP (x, keymap)
#define CHECK_KEYMAP(x) CHECK_RECORD (x, keymap)
#define CONCHECK_KEYMAP(x) CONCHECK_RECORD (x, keymap)

EXFUN (Fkeymap_prompt, 2);
EXFUN (Fkeymapp, 1);
EXFUN (Fmake_keymap, 1);
EXFUN (Fwhere_is_internal, 5);

extern Lisp_Object Qalt, Qcontrol, Qhyper, Qmeta, Qshift, Qsuper;

#define FROB(num)				\
extern Lisp_Object Qbutton##num;
#include "keymap-buttons.h"
extern Lisp_Object Vmeta_prefix_char;

Lisp_Object get_keymap (Lisp_Object object, int errorp, int autoload);
Lisp_Object event_binding (Lisp_Object event0, int accept_default);
Lisp_Object event_binding_in (Lisp_Object event0, Lisp_Object keymap,
			      int accept_default);

Lisp_Object munging_key_map_event_binding (Lisp_Object event0,
					   enum munge_me_out_the_door munge);
int relevant_keymaps_to_search (Lisp_Object keys,
				int max_maps, Lisp_Object maps[]);
void describe_map_tree (Lisp_Object startmap, int partial,
			Lisp_Object shadow, Lisp_Object prefix,
			int mice_only_p, Lisp_Object buffer);

void key_desc_list_to_event (Lisp_Object list, Lisp_Object event);

int event_matches_key_specifier_p (Lisp_Object event,
				   Lisp_Object key_specifier);

#endif /* INCLUDED_keymap_h_ */
