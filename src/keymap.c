/* Manipulation of keymaps
   Copyright (C) 1985, 1991-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2010 Ben Wing.
   Totally redesigned by jwz in 1991.

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

/* Synched up with: Mule 2.0.  Not synched with FSF.  Substantially
   different from FSF. */


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "bytecode.h"
#include "console-impl.h"
#include "elhash.h"
#include "events.h"
#include "extents.h"
#include "frame.h"
#include "insdel.h"
#include "keymap.h"
#include "window.h"


/* A keymap contains six slots:

   parents	   Ordered list of keymaps to search after
                   this one if no match is found.
		   Keymaps can thus be arranged in a hierarchy.

   table	   A hash table, hashing keysyms to their bindings.
   		   It will be one of the following:

		   -- a symbol, e.g. `home'
		   -- a character, representing something printable
		      (not ?\C-c meaning C-c, for instance)
		   -- an integer representing a modifier combination

   inverse_table   A hash table, hashing bindings to the list of keysyms
		   in this keymap which are bound to them.  This is to make
		   the Fwhere_is_internal() function be fast.  It needs to be
		   fast because we want to be able to call it in realtime to
		   update the keyboard-equivalents on the pulldown menus.
                   Values of the table are either atoms (keysyms)
                   or a dotted list of keysyms.

   sub_maps_cache  An alist; for each entry in this keymap whose binding is
		   a keymap (that is, Fkeymapp()) this alist associates that
		   keysym with that binding.  This is used to optimize both
		   Fwhere_is_internal() and Faccessible_keymaps().  This slot
		   gets set to the symbol `t' every time a change is made to
		   this keymap, causing it to be recomputed when next needed.

   prompt          See `set-keymap-prompt'.

   default_binding See `set-keymap-default-binding'.

   Sequences of keys are stored in the obvious way: if the sequence of keys
   "abc" was bound to some command `foo', the hierarchy would look like

      keymap-1: associates "a" with keymap-2
      keymap-2: associates "b" with keymap-3
      keymap-3: associates "c" with foo

   However, bucky bits ("modifiers" to the X-minded) are represented in the
   keymap hierarchy as well. (This lets us use EQable objects as hash keys.)
   Each combination of modifiers (e.g. control-hyper) gets its own submap
   off of the main map.  The hash key for a modifier combination is
   an integer, computed by MAKE_MODIFIER_HASH_KEY().

   If the key `C-a' was bound to some command, the hierarchy would look like

      keymap-1: associates the integer XEMACS_MOD_CONTROL with keymap-2
      keymap-2: associates "a" with the command

   Similarly, if the key `C-H-a' was bound to some command, the hierarchy
   would look like

      keymap-1: associates the integer (XEMACS_MOD_CONTROL | XEMACS_MOD_HYPER)
                with keymap-2
      keymap-2: associates "a" with the command

   Note that a special exception is made for the meta modifier, in order
   to deal with ESC/meta lossage.  Any key combination containing the
   meta modifier is first indexed off of the main map into the meta
   submap (with hash key XEMACS_MOD_META) and then indexed off of the
   meta submap with the meta modifier removed from the key combination.
   For example, when associating a command with C-M-H-a, we'd have

      keymap-1: associates the integer XEMACS_MOD_META with keymap-2
      keymap-2: associates the integer (XEMACS_MOD_CONTROL | XEMACS_MOD_HYPER)
                with keymap-3
      keymap-3: associates "a" with the command

   Note that keymap-2 might have normal bindings in it; these would be
   for key combinations containing only the meta modifier, such as
   M-y or meta-backspace.

   If the command that "a" was bound to in keymap-3 was itself a keymap,
   then that would make the key "C-M-H-a" be a prefix character.

   Note that this new model of keymaps takes much of the magic away from
   the Escape key: the value of the variable `esc-map' is no longer indexed
   in the `global-map' under the ESC key.  It's indexed under the integer
   XEMACS_MOD_META.  This is not user-visible, however; none of the "bucky"
   maps are.

   There is a hack in Flookup_key() that makes (lookup-key global-map "\^[")
   and (define-key some-random-map "\^[" my-esc-map) work as before, for
   compatibility.

   Since keymaps are opaque, the only way to extract information from them
   is with the functions lookup-key, key-binding, local-key-binding, and
   global-key-binding, which work just as before, and the new function
   map-keymap, which is roughly analogous to maphash.

   Note that map-keymap perpetuates the illusion that the "bucky" submaps
   don't exist: if you map over a keymap with bucky submaps, it will also
   map over those submaps.  It does not, however, map over other random
   submaps of the keymap, just the bucky ones.

   One implication of this is that when you map over `global-map', you will
   also map over `esc-map'.  It is merely for compatibility that the esc-map
   is accessible at all; I think that's a bad thing, since it blurs the
   distinction between ESC and "meta" even more.  "M-x" is no more a two-
   key sequence than "C-x" is.

 */

struct Lisp_Keymap
{
  NORMAL_LISP_OBJECT_HEADER header;
#define MARKED_SLOT(x) Lisp_Object x;
#include "keymap-slots.h"
};

#define MAKE_MODIFIER_HASH_KEY(modifier) make_fixnum (modifier)
#define MODIFIER_HASH_KEY_BITS(x) (FIXNUMP (x) ? XFIXNUM (x) : 0)



/* Actually allocate storage for these variables */

Lisp_Object Vcurrent_global_map; /* Always a keymap */

static Lisp_Object Vglobal_tty_map, Vglobal_window_system_map;

static Lisp_Object Vmouse_grabbed_buffer;

/* Alist of minor mode variables and keymaps.  */
static Lisp_Object Qminor_mode_map_alist;

static Lisp_Object Voverriding_local_map;

static Lisp_Object Vkey_translation_map;

static Lisp_Object Vvertical_divider_map;

/* This is incremented whenever a change is made to a keymap.  This is
   so that things which care (such as the menubar code) can recompute
   privately-cached data when the user has changed keybindings.
 */
Fixnum keymap_tick;

/* Prefixing a key with this character is the same as sending a meta bit. */
Lisp_Object Vmeta_prefix_char;

Lisp_Object Qkeymapp;
Lisp_Object Vsingle_space_string;
Lisp_Object Qsuppress_keymap;
Lisp_Object Qmodeline_map;
Lisp_Object Qtoolbar_map;

Lisp_Object Qremap;
Lisp_Object Qxemacs_command_remapping; /* Uninterned, so there's no conflict
                                          with any key named remap. */

EXFUN (Fkeymap_fullness, 1);
EXFUN (Fset_keymap_name, 2);
EXFUN (Fsingle_key_description, 1);

static Lisp_Object remap_command (Lisp_Object keymap, Lisp_Object old,
                                  Lisp_Object new_);
static void describe_command (Lisp_Object definition, Lisp_Object buffer);
static void describe_map (Lisp_Object keymap, Lisp_Object elt_prefix,
			  void (*elt_describer) (Lisp_Object, Lisp_Object),
			  int partial,
			  Lisp_Object shadow,
			  int mice_only_p,
			  Lisp_Object buffer);
static Lisp_Object keymap_submaps (Lisp_Object keymap);
static int get_relevant_keymaps (Lisp_Object, Lisp_Object, int,
                                 Lisp_Object maps[]);
static Lisp_Object lookup_keys (Lisp_Object, int, Lisp_Object *, int);
static void map_keymap (Lisp_Object keymap_table, int sort_first,
                        void (*function) (const Lisp_Key_Data *key,
                                          Lisp_Object binding,
                                          void *fn_arg),
                        void *fn_arg);

Lisp_Object Qcontrol, Qctrl, Qmeta, Qsuper, Qhyper, Qalt, Qshift;

#define INCLUDE_BUTTON_ZERO
#define FROB(num)				\
Lisp_Object Qbutton##num;			\
Lisp_Object Qbutton##num##up;
#include "keymap-buttons.h"

Lisp_Object Qmenu_selection;

/* Emacs compatibility */
#define FROB(num)				\
Lisp_Object Qmouse_##num;			\
Lisp_Object Qdown_mouse_##num;
#include "keymap-buttons.h"

/* Kludge kludge kludge */
Lisp_Object QLFD, QTAB, QRET, QESC, QDEL, QSPC, QBS;

#define CHECK_REMAPPING_POSITION(object)        do                      \
    {                                                                   \
      if (!(NILP (object) || FIXNUMP (object) || MARKERP (object)       \
            || EVENTP (object)))                                        \
        {                                                               \
          wtaerror ("Not a valid POSITION", object);                    \
        }                                                               \
    } while (0)


/************************************************************************/
/*                     The keymap Lisp object                           */
/************************************************************************/

/* Keymaps are equal if all of their attributes are equal. */
static int
keymap_equal (Lisp_Object obj1, Lisp_Object obj2, int depth,
	      int UNUSED (foldcase))
{
  Lisp_Keymap *k1 = XKEYMAP (obj1);
  Lisp_Keymap *k2 = XKEYMAP (obj2);

  depth++;

  return
    (
#define MARKED_SLOT(x) \
     internal_equal (k1->x, k2->x, depth) &&
#define MARKED_SLOT_NOCOMPARE(x)
#include "keymap-slots.h"
     1
     );
}

static Hashcode
keymap_hash (Lisp_Object obj, int depth, Boolint UNUSED (equalp))
{
  Lisp_Keymap *k = XKEYMAP (obj);
  Hashcode hash = 0xCAFEBABE; /* why not? */

  depth++;

#define MARKED_SLOT(x) \
  hash = HASH2 (hash, internal_hash (k->x, depth, 0));
#define MARKED_SLOT_NOCOMPARE(x)
#include "keymap-slots.h"

  return hash;
}

static Lisp_Object
mark_keymap (Lisp_Object obj)
{
  Lisp_Keymap *keymap = XKEYMAP (obj);
#define MARKED_SLOT(x) mark_object (keymap->x);
#include "keymap-slots.h"
  return Qnil;
}

static void
print_keymap (Lisp_Object obj, Lisp_Object printcharfun,
	      int UNUSED (escapeflag))
{
  /* This function can GC */
  Lisp_Keymap *keymap = XKEYMAP (obj);
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);
  write_ascstring (printcharfun, "#<keymap ");
  if (!NILP (keymap->name))
    {
      write_fmt_string_lisp (printcharfun, "%S ", 1, keymap->name);
    }
  write_fmt_string (printcharfun, "size %ld 0x%x>",
		    (long) XFIXNUM (Fkeymap_fullness (obj)),
		    LISP_OBJECT_UID (obj));
}

static const struct memory_description keymap_description[] = {
#define MARKED_SLOT(x) { XD_LISP_OBJECT, offsetof (Lisp_Keymap, x) },
#include "keymap-slots.h"
  { XD_END }
};

DEFINE_DUMPABLE_LISP_OBJECT ("keymap", keymap,
			     mark_keymap, print_keymap, 0,
			     keymap_equal, keymap_hash,
			     keymap_description,
			     Lisp_Keymap);

/************************************************************************/
/*                Traversing keymaps and their parents                  */
/************************************************************************/

static Lisp_Object
traverse_keymaps (Lisp_Object start_keymap, Lisp_Object start_parents,
                  Lisp_Object (*mapper) (Lisp_Object keymap, void *mapper_arg),
                  void *mapper_arg)
{
  /* This function can GC */
  Lisp_Object keymap;
  Lisp_Object tail = start_parents;
  Lisp_Object malloc_sucks[10];
  Lisp_Object malloc_bites = Qnil;
  int stack_depth = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  GCPRO4 (*malloc_sucks, malloc_bites, start_keymap, tail);
  gcpro1.nvars = 0;

  start_keymap = get_keymap (start_keymap, 1, 1);
  keymap = start_keymap;
  /* Hack special-case parents at top-level */
  tail = !NILP (tail) ? tail : XKEYMAP (keymap)->parents;

  for (;;)
    {
      Lisp_Object result;

      QUIT;
      result = mapper (keymap, mapper_arg);
      if (!NILP (result))
	{
	  while (CONSP (malloc_bites))
	    {
	      Lisp_Object victim = malloc_bites;
	      malloc_bites = XCDR (victim);
	      free_cons (victim);
	    }
	  UNGCPRO;
	  return result;
	}
      if (NILP (tail))
	{
	  if (stack_depth == 0)
	    {
	      UNGCPRO;
	      return Qnil;          /* Nothing found */
	    }
	  stack_depth--;
	  if (CONSP (malloc_bites))
	    {
	      Lisp_Object victim = malloc_bites;
	      tail = XCAR (victim);
	      malloc_bites = XCDR (victim);
	      free_cons (victim);
	    }
	  else
	    {
	      tail = malloc_sucks[stack_depth];
	      gcpro1.nvars = stack_depth;
	    }
	  keymap = XCAR (tail);
	  tail = XCDR (tail);
	}
      else
	{
	  Lisp_Object parents;

	  keymap = XCAR (tail);
	  tail = XCDR (tail);
	  parents = XKEYMAP (keymap)->parents;
	  if (!CONSP (parents))
	    ;
	  else if (NILP (tail))
	    /* Tail-recurse */
	    tail = parents;
	  else
	    {
	      if (CONSP (malloc_bites))
		malloc_bites = noseeum_cons (tail, malloc_bites);
	      else if (stack_depth < countof (malloc_sucks))
		{
		  malloc_sucks[stack_depth++] = tail;
		  gcpro1.nvars = stack_depth;
		}
	      else
		{
		  /* *&@##[*&^$ C. @#[$*&@# Unix.  Losers all. */
		  int i;
		  for (i = 0, malloc_bites = Qnil;
		       i < countof (malloc_sucks);
		       i++)
		    malloc_bites = noseeum_cons (malloc_sucks[i],
						 malloc_bites);
		  gcpro1.nvars = 0;
		}
	      tail = parents;
	    }
	}
      keymap = get_keymap (keymap, 1, 1);
      if (EQ (keymap, start_keymap))
	{
	  invalid_argument ("Cyclic keymap indirection", start_keymap);
	}
    }
}


/************************************************************************/
/*                     Some low-level functions                         */
/************************************************************************/

static int
bucky_sym_to_bucky_bit (Lisp_Object sym)
{
  if (EQ (sym, Qcontrol)) return XEMACS_MOD_CONTROL;
  if (EQ (sym, Qmeta))    return XEMACS_MOD_META;
  if (EQ (sym, Qsuper))   return XEMACS_MOD_SUPER;
  if (EQ (sym, Qhyper))   return XEMACS_MOD_HYPER;
  if (EQ (sym, Qalt))     return XEMACS_MOD_ALT;
  if (EQ (sym, Qsymbol))  return XEMACS_MOD_ALT; /* #### - reverse compat */
  if (EQ (sym, Qshift))   return XEMACS_MOD_SHIFT;

  return 0;
}

static Lisp_Object
control_meta_superify (Lisp_Object frob, int modifiers)
{
  if (modifiers == 0)
    return frob;
  frob = Fcons (frob, Qnil);
  if (modifiers & XEMACS_MOD_SHIFT)   frob = Fcons (Qshift,   frob);
  if (modifiers & XEMACS_MOD_ALT)     frob = Fcons (Qalt,     frob);
  if (modifiers & XEMACS_MOD_HYPER)   frob = Fcons (Qhyper,   frob);
  if (modifiers & XEMACS_MOD_SUPER)   frob = Fcons (Qsuper,   frob);
  if (modifiers & XEMACS_MOD_CONTROL) frob = Fcons (Qcontrol, frob);
  if (modifiers & XEMACS_MOD_META)    frob = Fcons (Qmeta,    frob);
  return frob;
}

static Lisp_Object
make_key_description (const Lisp_Key_Data *key, int prettify)
{
  Lisp_Object keysym = KEY_DATA_KEYSYM (key);
  int modifiers = KEY_DATA_MODIFIERS (key);
  if (prettify && CHARP (keysym))
    {
      /* This is a little slow, but (control a) is prettier than (control 65).
	 It's now ok to do this for digit-chars too, since we've fixed the
	 bug where \9 read as the integer 9 instead of as the symbol with
	 "9" as its name.
       */
      /* !!#### I'm not sure how correct this is. */
      Ibyte str [1 + MAX_ICHAR_LEN];
      Bytecount count = set_itext_ichar (str, XCHAR (keysym));
      str[count] = 0;
      keysym = intern_istring (str);
    }
  return control_meta_superify (keysym, modifiers);
}


/************************************************************************/
/*                   Low-level keymap-store functions                   */
/************************************************************************/

static Lisp_Object
raw_lookup_key (Lisp_Object keymap,
                const Lisp_Key_Data *raw_keys, int raw_keys_count,
                int keys_so_far, int accept_default);

/* Relies on caller to gc-protect args */
static Lisp_Object
keymap_lookup_directly (Lisp_Object keymap,
                        Lisp_Object keysym, int modifiers)
{
  Lisp_Keymap *k;

  modifiers &= ~(
#define FROB(num) XEMACS_MOD_BUTTON##num |
#include "keymap-buttons.h"
                 0);
  assert ((modifiers & ~(XEMACS_MOD_CONTROL | XEMACS_MOD_META |
			 XEMACS_MOD_SUPER | XEMACS_MOD_HYPER |
			 XEMACS_MOD_ALT | XEMACS_MOD_SHIFT))
	  == 0);

  k = XKEYMAP (keymap);

  /* If the keysym is a one-character symbol, use the char code instead. */
  if (SYMBOLP (keysym) && string_char_length (XSYMBOL (keysym)->name) == 1)
    {
      Lisp_Object i_fart_on_gcc =
	make_char (string_ichar (XSYMBOL (keysym)->name, 0));
      keysym = i_fart_on_gcc;
    }

  if (modifiers & XEMACS_MOD_META)     /* Utterly hateful ESC lossage */
    {
      Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (XEMACS_MOD_META),
                                     k->table, Qnil);
      if (NILP (submap))
        return Qnil;
      k = XKEYMAP (submap);
      modifiers &= ~XEMACS_MOD_META;
    }

  if (modifiers != 0)
    {
      Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (modifiers),
                                     k->table, Qnil);
      if (NILP (submap))
        return Qnil;
      k = XKEYMAP (submap);
    }
  return Fgethash (keysym, k->table, Qnil);
}

static void
keymap_store_inverse_internal (Lisp_Object inverse_table,
                               Lisp_Object keysym,
                               Lisp_Object value)
{
  Lisp_Object keys = Fgethash (value, inverse_table, Qunbound);

  if (UNBOUNDP (keys))
    {
      keys = keysym;
      /* Don't cons this unless necessary */
      /* keys = Fcons (keysym, Qnil); */
      Fputhash (value, keys, inverse_table);
    }
  else if (!CONSP (keys))
    {
      /* Now it's necessary to cons */
      keys = Fcons (keys, keysym);
      Fputhash (value, keys, inverse_table);
    }
  else
    {
      while (CONSP (XCDR (keys)))
	keys = XCDR (keys);
      XCDR (keys) = Fcons (XCDR (keys), keysym);
      /* No need to call puthash because we've destructively
         modified the list tail in place */
    }
}


static void
keymap_delete_inverse_internal (Lisp_Object inverse_table,
                                Lisp_Object keysym,
                                Lisp_Object value)
{
  Lisp_Object keys = Fgethash (value, inverse_table, Qunbound);
  Lisp_Object new_keys = keys;
  Lisp_Object tail;
  Lisp_Object *prev;

  assert (!UNBOUNDP (keys));

  for (prev = &new_keys, tail = new_keys;
       ;
       prev = &(XCDR (tail)), tail = XCDR (tail))
    {
      if (EQ (tail, keysym))
	{
	  *prev = Qnil;
	  break;
	}
      else if (EQ (keysym, XCAR (tail)))
	{
	  *prev = XCDR (tail);
	  break;
	}
    }

  if (NILP (new_keys))
    Fremhash (value, inverse_table);
  else if (!EQ (keys, new_keys))
    /* Removed the first elt */
    Fputhash (value, new_keys, inverse_table);
  /* else the list's tail has been modified, so we don't need to
     touch the hash table again (the pointer in there is ok).
   */
}

/* Prevent luser from shooting herself in the foot using something like
   (define-key ctl-x-4-map "p" global-map) */
static void
check_keymap_definition_loop (Lisp_Object def, Lisp_Keymap *to_keymap)
{
  def = get_keymap (def, 0, 0);

  if (KEYMAPP (def))
    {
      Lisp_Object maps;

      if (XKEYMAP (def) == to_keymap)
	invalid_argument ("Cyclic keymap definition", def);

      for (maps = keymap_submaps (def);
	   CONSP (maps);
	   maps = XCDR (maps))
	check_keymap_definition_loop (XCDR (XCAR (maps)), to_keymap);
    }
}

static void
keymap_store_internal (Lisp_Object keysym, Lisp_Keymap *keymap,
		       Lisp_Object def)
{
  Lisp_Object prev_def = Fgethash (keysym, keymap->table, Qnil);

  if (EQ (prev_def, def))
      return;

  check_keymap_definition_loop (def, keymap);

  if (!NILP (prev_def))
    keymap_delete_inverse_internal (keymap->inverse_table,
                                    keysym, prev_def);
  if (NILP (def))
    {
      Fremhash (keysym, keymap->table);
    }
  else
    {
      Fputhash (keysym, def, keymap->table);
      keymap_store_inverse_internal (keymap->inverse_table,
                                     keysym, def);
    }
  keymap_tick++;
}


static Lisp_Object
create_bucky_submap (Lisp_Keymap *k, int modifiers,
                     Lisp_Object parent_for_debugging_info)
{
  Lisp_Object submap = Fmake_sparse_keymap (Qnil);
  /* User won't see this, but it is nice for debugging Emacs */
  XKEYMAP (submap)->name
    = control_meta_superify (parent_for_debugging_info, modifiers);
  /* Invalidate cache */
  k->sub_maps_cache = Qt;
  keymap_store_internal (MAKE_MODIFIER_HASH_KEY (modifiers), k, submap);
  return submap;
}


/* Relies on caller to gc-protect keymap, keysym, value */
static void
keymap_store (Lisp_Object keymap, const Lisp_Key_Data *key,
              Lisp_Object value)
{
  Lisp_Object keysym = KEY_DATA_KEYSYM (key);
  int modifiers = KEY_DATA_MODIFIERS (key);
  Lisp_Keymap *k = XKEYMAP (keymap);

  modifiers &= ~(
#define FROB(num) XEMACS_MOD_BUTTON##num |
#include "keymap-buttons.h"
                 0);
  assert ((modifiers & ~(XEMACS_MOD_CONTROL | XEMACS_MOD_META
			 | XEMACS_MOD_SUPER | XEMACS_MOD_HYPER
			 | XEMACS_MOD_ALT | XEMACS_MOD_SHIFT)) == 0);

  /* If the keysym is a one-character symbol, use the char code instead. */
  if (SYMBOLP (keysym) && string_char_length (XSYMBOL (keysym)->name) == 1)
    keysym = make_char (string_ichar (XSYMBOL (keysym)->name, 0));

  if (modifiers & XEMACS_MOD_META)     /* Utterly hateful ESC lossage */
    {
      Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (XEMACS_MOD_META),
				     k->table, Qnil);
      if (NILP (submap))
	submap = create_bucky_submap (k, XEMACS_MOD_META, keymap);
      k = XKEYMAP (submap);
      modifiers &= ~XEMACS_MOD_META;
    }

  if (modifiers != 0)
    {
      Lisp_Object submap = Fgethash (MAKE_MODIFIER_HASH_KEY (modifiers),
				     k->table, Qnil);
      if (NILP (submap))
	submap = create_bucky_submap (k, modifiers, keymap);
      k = XKEYMAP (submap);
    }
  k->sub_maps_cache = Qt; /* Invalidate cache */
  keymap_store_internal (keysym, k, value);
}


/************************************************************************/
/*                   Listing the submaps of a keymap                    */
/************************************************************************/

struct keymap_submaps_closure
{
  Lisp_Object *result_locative;
};

static int
keymap_submaps_mapper_0 (Lisp_Object UNUSED (key), Lisp_Object value,
                         void *UNUSED (keymap_submaps_closure))
{
  /* This function can GC */
  /* Perform any autoloads, etc */
  Fkeymapp (value);
  return 0;
}

static int
keymap_submaps_mapper (Lisp_Object key, Lisp_Object value,
                       void *keymap_submaps_closure)
{
  /* This function can GC */
  Lisp_Object *result_locative;
  struct keymap_submaps_closure *cl =
    (struct keymap_submaps_closure *) keymap_submaps_closure;
  result_locative = cl->result_locative;

  if (!NILP (Fkeymapp (value)))
    *result_locative = Fcons (Fcons (key, value), *result_locative);
  return 0;
}

static Boolint map_keymap_sort_predicate (Lisp_Object pred, Lisp_Object key,
					  Lisp_Object obj1, Lisp_Object obj2);
					  

static Lisp_Object
keymap_submaps (Lisp_Object keymap)
{
  /* This function can GC */
  Lisp_Keymap *k = XKEYMAP (keymap);

  if (EQ (k->sub_maps_cache, Qt)) /* Unknown */
    {
      Lisp_Object result = Qnil;
      struct gcpro gcpro1, gcpro2;
      struct keymap_submaps_closure keymap_submaps_closure;

      GCPRO2 (keymap, result);
      keymap_submaps_closure.result_locative = &result;
      /* Do this first pass to touch (and load) any autoloaded maps */
      elisp_maphash (keymap_submaps_mapper_0, k->table,
		     &keymap_submaps_closure);
      result = Qnil;
      elisp_maphash (keymap_submaps_mapper, k->table,
		     &keymap_submaps_closure);
      /* keep it sorted so that the result of accessible-keymaps is ordered */
      k->sub_maps_cache = list_sort (result, map_keymap_sort_predicate,
                                     Qnil, Qnil);
      UNGCPRO;
    }
  return k->sub_maps_cache;
}


/************************************************************************/
/*                    Basic operations on keymaps                       */
/************************************************************************/

static Lisp_Object
make_keymap (Elemcount size)
{
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (keymap);
  Lisp_Keymap *keymap = XKEYMAP (obj);

#define MARKED_SLOT(x) keymap->x = Qnil;
#include "keymap-slots.h"

  if (size != 0) /* hack for copy-keymap */
    {
      keymap->table =
	make_lisp_hash_table (size, HASH_TABLE_NON_WEAK, Qeq);
      /* Inverse table is often less dense because of duplicate key-bindings.
         If not, it will grow anyway. */
      keymap->inverse_table =
	make_lisp_hash_table (size * 3 / 4, HASH_TABLE_NON_WEAK,
			      Qeq);
    }
  return obj;
}

DEFUN ("make-keymap", Fmake_keymap, 0, 1, 0, /*
Construct and return a new keymap object.
All entries in it are nil, meaning "command undefined".

Optional argument NAME specifies a name to assign to the keymap,
as in `set-keymap-name'.  This name is only a debugging convenience;
it is not used except when printing the keymap.
*/
       (name))
{
  Lisp_Object keymap = make_keymap (60);
  if (!NILP (name))
    Fset_keymap_name (keymap, name);
  return keymap;
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, 0, 1, 0, /*
Construct and return a new keymap object.
All entries in it are nil, meaning "command undefined".  The only
difference between this function and `make-keymap' is that this function
returns a "smaller" keymap (one that is expected to contain fewer
entries).  As keymaps dynamically resize, this distinction is not great.

Optional argument NAME specifies a name to assign to the keymap,
as in `set-keymap-name'.  This name is only a debugging convenience;
it is not used except when printing the keymap.
*/
       (name))
{
  Lisp_Object keymap = make_keymap (8);
  if (!NILP (name))
    Fset_keymap_name (keymap, name);
  return keymap;
}

DEFUN ("keymap-parents", Fkeymap_parents, 1, 1, 0, /*
Return the `parent' keymaps of KEYMAP, or nil.
The parents of a keymap are searched for keybindings when a key sequence
isn't bound in this one.  `(current-global-map)' is the default parent
of all keymaps.
*/
       (keymap))
{
  keymap = get_keymap (keymap, 1, 1);
  return Fcopy_sequence (XKEYMAP (keymap)->parents);
}



static Lisp_Object
traverse_keymaps_noop (Lisp_Object UNUSED (keymap), void *UNUSED (arg))
{
  return Qnil;
}

DEFUN ("set-keymap-parents", Fset_keymap_parents, 2, 2, 0, /*
Set the `parent' keymaps of KEYMAP to PARENTS.
The parents of a keymap are searched for keybindings when a key sequence
isn't bound in this one.  `(current-global-map)' is the default parent
of all keymaps.
*/
       (keymap, parents))
{
  /* This function can GC */
  Lisp_Object k;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (keymap, parents);
  keymap = get_keymap (keymap, 1, 1);

  if (KEYMAPP (parents))	/* backwards-compatibility */
    parents = list1 (parents);
  if (!NILP (parents))
    {
      Lisp_Object tail = parents;
      while (!NILP (tail))
	{
	  QUIT;
	  CHECK_CONS (tail);
	  k = XCAR (tail);
	  /* Require that it be an actual keymap object, rather than a symbol
	     with a (crockish) symbol-function which is a keymap */
	  CHECK_KEYMAP (k); /* get_keymap (k, 1, 1); */
	  tail = XCDR (tail);
	}
    }

  /* Check for circularities */
  traverse_keymaps (keymap, parents, traverse_keymaps_noop, 0);
  keymap_tick++;
  XKEYMAP (keymap)->parents = Fcopy_sequence (parents);
  UNGCPRO;
  return parents;
}

DEFUN ("set-keymap-name", Fset_keymap_name, 2, 2, 0, /*
Set the `name' of the KEYMAP to NEW-NAME.
The name is only a debugging convenience; it is not used except
when printing the keymap.
*/
       (keymap, new_name))
{
  keymap = get_keymap (keymap, 1, 1);

  XKEYMAP (keymap)->name = new_name;
  return new_name;
}

DEFUN ("keymap-name", Fkeymap_name, 1, 1, 0, /*
Return the `name' of KEYMAP.
The name is only a debugging convenience; it is not used except
when printing the keymap.
*/
       (keymap))
{
  keymap = get_keymap (keymap, 1, 1);

  return XKEYMAP (keymap)->name;
}

DEFUN ("set-keymap-prompt", Fset_keymap_prompt, 2, 2, 0, /*
Set the `prompt' of KEYMAP to string NEW-PROMPT, or `nil'
if no prompt is desired.  The prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.
*/
       (keymap, new_prompt))
{
  keymap = get_keymap (keymap, 1, 1);

  if (!NILP (new_prompt))
    CHECK_STRING (new_prompt);

  XKEYMAP (keymap)->prompt = new_prompt;
  return new_prompt;
}

static Lisp_Object
keymap_prompt_mapper (Lisp_Object keymap, void *UNUSED (arg))
{
  return XKEYMAP (keymap)->prompt;
}


DEFUN ("keymap-prompt", Fkeymap_prompt, 1, 2, 0, /*
Return the `prompt' of KEYMAP.
If non-nil, the prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.
*/
       (keymap, use_inherited))
{
  /* This function can GC */
  Lisp_Object prompt;

  keymap = get_keymap (keymap, 1, 1);
  prompt = XKEYMAP (keymap)->prompt;
  if (!NILP (prompt) || NILP (use_inherited))
    return prompt;
  else
    return traverse_keymaps (keymap, Qnil, keymap_prompt_mapper, 0);
}

DEFUN ("set-keymap-default-binding", Fset_keymap_default_binding, 2, 2, 0, /*
Sets the default binding of KEYMAP to COMMAND, or `nil'
if no default is desired.  The default-binding is returned when
no other binding for a key-sequence is found in the keymap.
If a keymap has a non-nil default-binding, neither the keymap's
parents nor the current global map are searched for key bindings.
*/
       (keymap, command))
{
  /* This function can GC */
  keymap = get_keymap (keymap, 1, 1);

  XKEYMAP (keymap)->default_binding = command;
  return command;
}

DEFUN ("keymap-default-binding", Fkeymap_default_binding, 1, 1, 0, /*
Return the default binding of KEYMAP, or `nil' if it has none.
The default-binding is returned when no other binding for a key-sequence
is found in the keymap.
If a keymap has a non-nil default-binding, neither the keymap's
parents nor the current global map are searched for key bindings.
*/
       (keymap))
{
  /* This function can GC */
  keymap = get_keymap (keymap, 1, 1);
  return XKEYMAP (keymap)->default_binding;
}

DEFUN ("keymapp", Fkeymapp, 1, 1, 0, /*
Return t if OBJECT is a keymap object.
The keymap may be autoloaded first if necessary.
*/
       (object))
{
  /* This function can GC */
  return KEYMAPP (get_keymap (object, 0, 0)) ? Qt : Qnil;
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD is non-zero and OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.
   If AUTOLOAD is nonzero, callers must assume GC is possible.

   ERRORP controls how we respond if OBJECT isn't a keymap.
   If ERRORP is non-zero, signal an error; otherwise, just return Qnil.

   Note that most of the time, we don't want to pursue autoloads.
   Functions like Faccessible_keymaps which scan entire keymap trees
   shouldn't load every autoloaded keymap.  I'm not sure about this,
   but it seems to me that only read_key_sequence, Flookup_key, and
   Fdefine_key should cause keymaps to be autoloaded.  */

Lisp_Object
get_keymap (Lisp_Object object, int errorp, int autoload)
{
  /* This function can GC */
  while (1)
    {
      Lisp_Object tem = indirect_function (object, 0);

      if (KEYMAPP (tem))
	return tem;
      /* Should we do an autoload?  */
      else if (autoload
               /* (autoload "filename" doc nil keymap) */
               && SYMBOLP (object)
               && CONSP (tem)
               && EQ (XCAR (tem), Qautoload)
               && EQ (Fcar (Fcdr (Fcdr (Fcdr (Fcdr (tem))))), Qkeymap))
	{
	  /* do_autoload GCPROs both arguments */
	  do_autoload (tem, object);
	}
      else if (errorp)
	object = wrong_type_argument (Qkeymapp, object);
      else
	return Qnil;
    }
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is an ASCII code, or a cons of (KEYSYM . MODIFIERS).
 */
static Lisp_Object
get_keyelt (Lisp_Object object, int accept_default)
{
  /* This function can GC */
  Lisp_Object map;

 tail_recurse:
  if (!CONSP (object))
    return object;

  {
    struct gcpro gcpro1;
    GCPRO1 (object);
    map = XCAR (object);
    map = get_keymap (map, 0, 1);
    UNGCPRO;
  }
  /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
  if (!NILP (map))
    {
      Lisp_Object idx = Fcdr (object);
      Lisp_Key_Data indirection;
      if (CHARP (idx))
	{
	  Lisp_Object event = Fmake_event (Qnil, Qnil);
          struct gcpro gcpro1;
          GCPRO1 (event);
	  character_to_event (XCHAR (idx), XEVENT (event),
			      XCONSOLE (Vselected_console),
			      high_bit_is_meta, 0);
	  indirection.keysym = XEVENT_KEY_KEYSYM (event);
	  indirection.modifiers = XEVENT_KEY_MODIFIERS (event);
	  UNGCPRO;
	}
      else if (CONSP (idx))
	{
	  if (!FIXNUMP (XCDR (idx)))
	    return Qnil;
	  indirection.keysym = XCAR (idx);
	  indirection.modifiers = (unsigned char) XFIXNUM (XCDR (idx));
	}
      else if (SYMBOLP (idx))
	{
	  indirection.keysym = idx;
	  SET_KEY_DATA_MODIFIERS (&indirection, XFIXNUM (XCDR (idx)));
	}
      else
	{
	  /* Random junk */
	  return Qnil;
	}
      return raw_lookup_key (map, &indirection, 1, 0, accept_default);
    }
  else if (STRINGP (XCAR (object)))
    {
      /* If the keymap contents looks like (STRING . DEFN),
	 use DEFN.
	 Keymap alist elements like (CHAR MENUSTRING . DEFN)
	 will be used by HierarKey menus.  */
      object = XCDR (object);
      goto tail_recurse;
    }
  else
    {
      /* Anything else is really the value.  */
      return object;
    }
}

static Lisp_Object
keymap_lookup_1 (Lisp_Object keymap, const Lisp_Key_Data *key,
                 int accept_default)
{
  /* This function can GC */
  return get_keyelt (keymap_lookup_directly (keymap,
					     KEY_DATA_KEYSYM (key), 
                                             KEY_DATA_MODIFIERS (key)),
		     accept_default);
}


/************************************************************************/
/*                          Copying keymaps                             */
/************************************************************************/

struct copy_keymap_inverse_closure
{
  Lisp_Object inverse_table;
};

static int
copy_keymap_inverse_mapper (Lisp_Object key, Lisp_Object value,
                            void *copy_keymap_inverse_closure)
{
  struct copy_keymap_inverse_closure *closure =
    (struct copy_keymap_inverse_closure *) copy_keymap_inverse_closure;

  /* copy-sequence deals with dotted lists. */
  if (CONSP (value))
    value = Fcopy_list (value);
  Fputhash (key, value, closure->inverse_table);

  return 0;
}


static Lisp_Object
copy_keymap_internal (Lisp_Keymap *keymap)
{
  Lisp_Object nkm = make_keymap (0);
  Lisp_Keymap *new_keymap = XKEYMAP (nkm);
  struct copy_keymap_inverse_closure copy_keymap_inverse_closure;
  copy_keymap_inverse_closure.inverse_table = keymap->inverse_table;

  new_keymap->parents        = Fcopy_sequence (keymap->parents);
  new_keymap->sub_maps_cache = Qnil; /* No submaps */
  new_keymap->table          = Fcopy_hash_table (keymap->table);
  new_keymap->inverse_table  = Fcopy_hash_table (keymap->inverse_table);
  new_keymap->default_binding = keymap->default_binding;
  /* After copying the inverse map, we need to copy the conses which
     are its values, lest they be shared by the copy, and mangled.
   */
  elisp_maphash (copy_keymap_inverse_mapper, keymap->inverse_table,
		 &copy_keymap_inverse_closure);
  return nkm;
}


static Lisp_Object copy_keymap (Lisp_Object keymap);

struct copy_keymap_closure
{
  Lisp_Keymap *self;
};

static int
copy_keymap_mapper (Lisp_Object key, Lisp_Object value,
                    void *copy_keymap_closure)
{
  /* This function can GC */
  struct copy_keymap_closure *closure =
    (struct copy_keymap_closure *) copy_keymap_closure;

  /* When we encounter a keymap which is indirected through a
     symbol, we need to copy the sub-map.  In v18, the form
       (lookup-key (copy-keymap global-map) "\C-x")
     returned a new keymap, not the symbol `Control-X-prefix'.
   */
  value = get_keymap (value, 0, 1); /* #### autoload GC-safe here? */
  if (KEYMAPP (value))
    keymap_store_internal (key, closure->self,
			   copy_keymap (value));
  return 0;
}

static Lisp_Object
copy_keymap (Lisp_Object keymap)
{
  /* This function can GC */
  struct copy_keymap_closure copy_keymap_closure;

  keymap = copy_keymap_internal (XKEYMAP (keymap));
  copy_keymap_closure.self = XKEYMAP (keymap);
  elisp_maphash (copy_keymap_mapper,
		 XKEYMAP (keymap)->table,
		 &copy_keymap_closure);
  return keymap;
}

DEFUN ("copy-keymap", Fcopy_keymap, 1, 1, 0, /*
Return a copy of the keymap KEYMAP.
The copy starts out with the same definitions of KEYMAP,
but changing either the copy or KEYMAP does not affect the other.
Any key definitions that are subkeymaps are recursively copied.
*/
       (keymap))
{
  /* This function can GC */
  keymap = get_keymap (keymap, 1, 1);
  return copy_keymap (keymap);
}


static int
keymap_fullness (Lisp_Object keymap)
{
  /* This function can GC */
  int fullness;
  Lisp_Object sub_maps;
  struct gcpro gcpro1, gcpro2;

  keymap = get_keymap (keymap, 1, 1);
  fullness = XFIXNUM (Fhash_table_count (XKEYMAP (keymap)->table));
  GCPRO2 (keymap, sub_maps);
  for (sub_maps = keymap_submaps (keymap);
       !NILP (sub_maps);
       sub_maps = XCDR (sub_maps))
    {
      if (MODIFIER_HASH_KEY_BITS (XCAR (XCAR (sub_maps))) != 0)
	{
	  Lisp_Object bucky_map = XCDR (XCAR (sub_maps));
	  fullness--; /* don't count bucky maps themselves. */
	  fullness += keymap_fullness (bucky_map);
	}
    }
  UNGCPRO;
  return fullness;
}

DEFUN ("keymap-fullness", Fkeymap_fullness, 1, 1, 0, /*
Return the number of bindings in the keymap.
*/
       (keymap))
{
  /* This function can GC */
  return make_fixnum (keymap_fullness (get_keymap (keymap, 1, 1)));
}


/************************************************************************/
/*                        Defining keys in keymaps                      */
/************************************************************************/

/* Given a keysym (should be a symbol, int, char), make sure it's valid
   and perform any necessary canonicalization. */

static void
define_key_check_and_coerce_keysym (Lisp_Object spec,
				    Lisp_Object *keysym,
				    int modifiers)
{
  /* Now, check and massage the trailing keysym specifier. */
  if (SYMBOLP (*keysym))
    {
      if (string_char_length (XSYMBOL (*keysym)->name) == 1)
	{
	  Lisp_Object ream_gcc_up_the_ass =
	    make_char (string_ichar (XSYMBOL (*keysym)->name, 0));
	  *keysym = ream_gcc_up_the_ass;
	  goto fixnum_keysym;
	}
    }
  else if (CHAR_OR_CHAR_INTP (*keysym))
    {
      CHECK_CHAR_COERCE_INT (*keysym);
    fixnum_keysym:
      if (XCHAR (*keysym) < ' '
	  /* || (XCHAR (*keysym) >= 128 && XCHAR (*keysym) < 160) */)
	/* yuck!  Can't make the above restriction; too many compatibility
	   problems ... */
	invalid_argument ("keysym char must be printable", *keysym);
      /* #### This bites!  I want to be able to write (control shift a) */
      if (modifiers & XEMACS_MOD_SHIFT)
	invalid_argument
	  ("The `shift' modifier may not be applied to ASCII keysyms",
	   spec);
    }
  else
    {
      invalid_argument ("Unknown keysym specifier", *keysym);
    }

  if (SYMBOLP (*keysym))
    {
      Ibyte *name = XSTRING_DATA (XSYMBOL (*keysym)->name);

      /* GNU Emacs uses symbols with the printed representation of keysyms in
	 their names, like `M-x', and we use the syntax '(meta x).  So, to
	 avoid confusion, notice the M-x syntax and signal an error -
	 because otherwise it would be interpreted as a regular keysym, and
	 would even show up in the list-buffers output, causing confusion
	 to the naive.

	 We can get away with this because none of the X keysym names contain
	 a hyphen (some contain underscore, however).

	 It might be useful to reject keysyms which are not x-valid-keysym-
	 name-p, but that would interfere with various tricks we do to
	 sanitize the Sun keyboards, and would make it trickier to
	 conditionalize a .emacs file for multiple X servers.
	 */
      if (((int) qxestrlen (name) >= 2 && name[1] == '-'
	   /* Check for a function binding if the symbol looks like
	      c-..., otherwise command remapping and C mode interact
	      badly. */
           && NILP (Ffunctionp (XSYMBOL_FUNCTION (*keysym))))
#if 1
          ||
	  /* Ok, this is a bit more dubious - prevent people from doing things
	     like (global-set-key 'RET 'something) because that will have the
	     same problem as above.  (Gag!)  Maybe we should just silently
	     accept these as aliases for the "real" names?
	     */
	  (XSTRING_LENGTH (XSYMBOL (*keysym)->name) <= 3 &&
	   (!qxestrcmp_ascii (name, "LFD") ||
	    !qxestrcmp_ascii (name, "TAB") ||
	    !qxestrcmp_ascii (name, "RET") ||
	    !qxestrcmp_ascii (name, "ESC") ||
	    !qxestrcmp_ascii (name, "DEL") ||
	    !qxestrcmp_ascii (name, "SPC") ||
	    !qxestrcmp_ascii (name, "BS")))
#endif /* unused */
          )
	invalid_argument
          ("Invalid (GNU Emacs) key format (see doc of define-key)",
	   *keysym);

      /* #### Ok, this is a bit more dubious - make people not lose if they
	 do things like (global-set-key 'RET 'something) because that would
	 otherwise have the same problem as above.  (Gag!)  We silently
	 accept these as aliases for the "real" names.
	 */
      else if (!qxestrncmp_ascii (name, "kp_", 3))
	{
	  /* Likewise, the obsolete keysym binding of kp_.* should not lose. */
	  DECLARE_EISTRING (temp);
	  eicpy_raw (temp, name, qxestrlen (name));
	  eisetch_char (temp, 2, '-');
	  *keysym = Fintern_soft (eimake_string (temp), Qnil, Qnil);
	}
      else if (EQ (*keysym, QLFD))
	*keysym = QKlinefeed;
      else if (EQ (*keysym, QTAB))
	*keysym = QKtab;
      else if (EQ (*keysym, QRET))
	*keysym = QKreturn;
      else if (EQ (*keysym, QESC))
	*keysym = QKescape;
      else if (EQ (*keysym, QDEL))
	*keysym = QKdelete;
      else if (EQ (*keysym, QSPC))
	*keysym = QKspace;
      else if (EQ (*keysym, QBS))
	*keysym = QKbackspace;
      /* Emacs compatibility */
#define FROB(num)				\
      else if (EQ(*keysym, Qdown_mouse_##num))	\
        *keysym = Qbutton##num;			\
      else if (EQ(*keysym, Qmouse_##num))	\
	*keysym = Qbutton##num##up;
#include "keymap-buttons.h"
    }
}


/* Given any kind of key-specifier, return a keysym and modifier mask.
   Proper canonicalization is performed:

   -- integers are converted into the equivalent characters.
   -- one-character strings are converted into the equivalent characters.
 */

static void
define_key_parser (Lisp_Object spec, Lisp_Key_Data *returned_value)
{
  if (CHAR_OR_CHAR_INTP (spec))
    {
      Lisp_Object event = Fmake_event (Qnil, Qnil);
      struct gcpro gcpro1;
      GCPRO1 (event);
      character_to_event (XCHAR_OR_CHAR_INT (spec), XEVENT (event),
			  XCONSOLE (Vselected_console), high_bit_is_meta, 0);
      SET_KEY_DATA_KEYSYM (returned_value, XEVENT_KEY_KEYSYM (event));
      SET_KEY_DATA_MODIFIERS (returned_value, 
                              XEVENT_KEY_MODIFIERS (event));
      UNGCPRO;
    }
  else if (EVENTP (spec))
    {
      switch (XEVENT_TYPE (spec))
	{
	case key_press_event:
          {
            SET_KEY_DATA_KEYSYM (returned_value, XEVENT_KEY_KEYSYM (spec));
            SET_KEY_DATA_MODIFIERS (returned_value, XEVENT_KEY_MODIFIERS (spec));
	    break;
          }
	case button_press_event:
	case button_release_event:
	  {
	    int down = (XEVENT_TYPE (spec) == button_press_event);
	    switch (XEVENT_BUTTON_BUTTON (spec))
	      {
#define FROB(num)						\
	      case num:						\
		SET_KEY_DATA_KEYSYM (returned_value,		\
		                     (down ? Qbutton##num :	\
				      Qbutton##num##up));	\
		break;
#include "keymap-buttons.h"
	      default:
		SET_KEY_DATA_KEYSYM (returned_value, (down ? Qbutton0 :
						      Qbutton0up)); 
		break;
	      }
	    SET_KEY_DATA_MODIFIERS (returned_value,
				    XEVENT_BUTTON_MODIFIERS (spec));
	    break;
	  }
	default:
	  wtaerror ("unable to bind this type of event", spec);
	}
    }
  else if (SYMBOLP (spec))
    {
      /* Be nice, allow = to mean (=) */
      if (bucky_sym_to_bucky_bit (spec) != 0)
        invalid_argument ("Key is a modifier name", spec);
      define_key_check_and_coerce_keysym (spec, &spec, 0);
      SET_KEY_DATA_KEYSYM (returned_value, spec);
      SET_KEY_DATA_MODIFIERS (returned_value, 0);
    }
  else if (CONSP (spec))
    {
      int modifiers = 0;
      Lisp_Object keysym = Qnil;
      Lisp_Object rest = spec;

      /* First, parse out the leading modifier symbols. */
      while (CONSP (rest))
	{
	  int modifier;

	  keysym = XCAR (rest);
	  modifier = bucky_sym_to_bucky_bit (keysym);
	  modifiers |= modifier;
	  if (!NILP (XCDR (rest)))
	    {
	      if (! modifier)
		invalid_argument ("Unknown modifier", keysym);
	    }
	  else
	    {
	      if (modifier)
		sferror ("Nothing but modifiers here",
				     spec);
	    }
	  rest = XCDR (rest);
	  QUIT;
	}
      if (!NILP (rest))
        signal_error (Qlist_formation_error,
			   "List must be nil-terminated", spec);

      define_key_check_and_coerce_keysym (spec, &keysym, modifiers);
      SET_KEY_DATA_KEYSYM(returned_value, keysym);
      SET_KEY_DATA_MODIFIERS (returned_value, modifiers);
    }
  else
    {
      invalid_argument ("Unknown key-sequence specifier",
			   spec);
    }
}

/* Used by character-to-event */
void
key_desc_list_to_event (Lisp_Object list, Lisp_Object event,
                        int allow_menu_events)
{
  Lisp_Key_Data raw_key;

  if (allow_menu_events &&
      CONSP (list) &&
      /* #### where the hell does this come from? */
      EQ (XCAR (list), Qmenu_selection))
    {
      Lisp_Object fn, arg;
      if (! NILP (Fcdr (Fcdr (list))))
	invalid_argument ("Invalid menu event desc", list);
      arg = Fcar (Fcdr (list));
      if (SYMBOLP (arg))
	fn = Qcall_interactively;
      else
	fn = Qeval;
      XSET_EVENT_TYPE (event, misc_user_event);
      XSET_EVENT_CHANNEL (event, wrap_frame (selected_frame ()));
      XSET_EVENT_MISC_USER_FUNCTION (event, fn);
      XSET_EVENT_MISC_USER_OBJECT (event, arg);
      return;
    }

  define_key_parser (list, &raw_key);

  /* The first zero is needed for Apple's i686-apple-darwin8-g++-4.0.1,
     otherwise the build fails with:

     In function ‘void key_desc_list_to_event(Lisp_Object, Lisp_Object, int)’:
     cc1plus: error: expected primary-expression
     cc1plus: error: expected `)'  */
  if (0 ||
#define INCLUDE_BUTTON_ZERO
#define FROB(num)				\
      EQ (raw_key.keysym, Qbutton##num) ||	\
      EQ (raw_key.keysym, Qbutton##num##up) ||
#include "keymap-buttons.h"
      0)
    invalid_operation ("Mouse-clicks can't appear in saved keyboard macros",
		       Qunbound);

  XSET_EVENT_CHANNEL (event, Vselected_console);
  XSET_EVENT_TYPE (event, key_press_event);
  XSET_EVENT_KEY_KEYSYM (event, raw_key.keysym);
  XSET_EVENT_KEY_MODIFIERS (event, KEY_DATA_MODIFIERS (&raw_key));
}


int
event_matches_key_specifier_p (Lisp_Object event, Lisp_Object key_specifier)
{
  Lisp_Object event2 = Qnil;
  int retval;
  struct gcpro gcpro1;

  if (XEVENT_TYPE (event) != key_press_event || NILP (key_specifier) ||
      (FIXNUMP (key_specifier) && !CHAR_INTP (key_specifier)))
    return 0;

  /* if the specifier is an integer such as 27, then it should match
     both of the events `escape' and `control ['.  Calling
     Fcharacter_to_event() will only match `escape'. */
  if (CHAR_OR_CHAR_INTP (key_specifier))
    return (XCHAR_OR_CHAR_INT (key_specifier)
	    == event_to_character (event, 0, 0));

  /* Otherwise, we cannot call event_to_character() because we may
     be dealing with non-ASCII keystrokes.  In any case, if I ask
     for `control [' then I should get exactly that, and not
     `escape'.

     However, we have to behave differently on TTY's, where `control ['
     is silently converted into `escape' by the keyboard driver.
     In this case, ASCII is the only thing we know about, so we have
     to compare the ASCII values. */

  GCPRO1 (event2);
  if (EVENTP (key_specifier))
    event2 = Fcopy_event (key_specifier, Qnil);
  else
    event2 = Fcharacter_to_event (key_specifier, Qnil, Qnil, Qnil);
  if (XEVENT (event2)->event_type != key_press_event)
    retval = 0;
  else if (CONSOLE_TTY_P (XCONSOLE (XEVENT_CHANNEL (event))))
    {
      int ch1, ch2;

      ch1 = event_to_character (event, 0, 0);
      ch2 = event_to_character (event2, 0, 0);
      retval = (ch1 >= 0 && ch2 >= 0 && ch1 == ch2);
    }
  else if (EQ (XEVENT_KEY_KEYSYM (event), XEVENT_KEY_KEYSYM (event2)) &&
	   XEVENT_KEY_MODIFIERS (event) == XEVENT_KEY_MODIFIERS (event2))
    retval = 1;
  else
    retval = 0;
  Fdeallocate_event (event2);
  UNGCPRO;
  return retval;
}

static int
meta_prefix_char_p (const Lisp_Key_Data *key)
{
  Lisp_Object event = Fmake_event (Qnil, Qnil);
  struct gcpro gcpro1;
  int retval;

  GCPRO1 (event);

  XSET_EVENT_TYPE (event, key_press_event);
  XSET_EVENT_CHANNEL (event, Vselected_console);
  XSET_EVENT_KEY_KEYSYM (event, KEY_DATA_KEYSYM (key));
  XSET_EVENT_KEY_MODIFIERS (event, KEY_DATA_MODIFIERS (key));
  retval = event_matches_key_specifier_p (event, Vmeta_prefix_char);
  UNGCPRO;
  return retval;
}

DEFUN ("event-matches-key-specifier-p", Fevent_matches_key_specifier_p, 2, 2, 0, /*
Return non-nil if EVENT matches KEY-SPECIFIER.
This can be useful, e.g., to determine if the user pressed `help-char' or
`quit-char'.

KEY-SPECIFIER can be a character, integer, a symbol, a list of modifiers
and symbols, or an event.

What this actually happens is this:

\(1) Return no, if EVENT is not a key press event or if KEY-SPECIFIER is nil
    or an integer that cannot be converted to a character.

\(2) If KEY-SPECIFIER is a character or integer,
    (event-to-character EVENT nil nil nil) is called, and the characters are
    compared to get the result.  The reason for special-casing this and doing
    it this way is to ensure that, e.g., a KEY-SPECIFIER of 27 matches both
    a key-press `escape' and a key-press `control ['. #### Think about META
    argument to event-to-character.

\(3) If KEY-SPECIFIER is an event, fine; else, convert to an event using
    \(character-to-event KEY-SPECIFIER nil nil nil).  If EVENT is not on a TTY,
    we just compare keysyms and modifiers and return yes if both are equal.
    For TTY, we do character-level comparison by converting both to a character
    with (event-to-character ... nil nil nil) and comparing the characters.

*/
       (event, key_specifier))
{
  CHECK_LIVE_EVENT (event);
  return (event_matches_key_specifier_p (event, key_specifier) ? Qt : Qnil);
}
#define MACROLET(k, m) do {			\
  SET_KEY_DATA_KEYSYM (returned_value, k);	\
  SET_KEY_DATA_MODIFIERS (returned_value, m);	\
  RETURN_SANS_WARNINGS;				\
} while (0)
/* ASCII grunge.
   Given a keysym, return another keysym/modifier pair which could be
   considered the same key in an ASCII world.  Backspace returns ^H, for
   example.
 */
static void
define_key_alternate_name (Lisp_Key_Data *key,
                           Lisp_Key_Data *returned_value)
{
  Lisp_Object keysym = KEY_DATA_KEYSYM (key);
  int modifiers = KEY_DATA_MODIFIERS (key);
  int modifiers_sans_control = (modifiers & (~XEMACS_MOD_CONTROL));
  int modifiers_sans_meta = (modifiers & (~XEMACS_MOD_META));
  SET_KEY_DATA_KEYSYM (returned_value, Qnil); /* By default, no "alternate" key */
  SET_KEY_DATA_MODIFIERS (returned_value, 0);
  if (modifiers_sans_meta == XEMACS_MOD_CONTROL)
    {
      if (EQ (keysym, QKspace))
        MACROLET (make_char ('@'), modifiers);
      else if (!CHARP (keysym))
        return;
      else switch (XCHAR (keysym))
        {
        case '@':               /* c-@ => c-space */
          MACROLET (QKspace, modifiers);
        case 'h':               /* c-h => backspace */
          MACROLET (QKbackspace, modifiers_sans_control);
        case 'i':               /* c-i => tab */
          MACROLET (QKtab, modifiers_sans_control);
        case 'j':               /* c-j => linefeed */
          MACROLET (QKlinefeed, modifiers_sans_control);
        case 'm':               /* c-m => return */
          MACROLET (QKreturn, modifiers_sans_control);
        case '[':               /* c-[ => escape */
          MACROLET (QKescape, modifiers_sans_control);
        default:
          return;
	}
    }
  else if (modifiers_sans_meta != 0)
    return;
  else if (EQ (keysym, QKbackspace)) /* backspace => c-h */
    MACROLET (make_char ('h'), (modifiers | XEMACS_MOD_CONTROL));
  else if (EQ (keysym, QKtab))       /* tab => c-i */
    MACROLET (make_char ('i'), (modifiers | XEMACS_MOD_CONTROL));
  else if (EQ (keysym, QKlinefeed))  /* linefeed => c-j */
    MACROLET (make_char ('j'), (modifiers | XEMACS_MOD_CONTROL));
  else if (EQ (keysym, QKreturn))    /* return => c-m */
    MACROLET (make_char ('m'), (modifiers | XEMACS_MOD_CONTROL));
  else if (EQ (keysym, QKescape))    /* escape => c-[ */
    MACROLET (make_char ('['), (modifiers | XEMACS_MOD_CONTROL));
  else
    return;
#undef MACROLET
}

static void
ensure_meta_prefix_char_keymapp (Lisp_Object keys, int indx,
                                 Lisp_Object keymap)
{
  /* This function can GC */
  Lisp_Object new_keys;
  int i;
  Lisp_Object mpc_binding;
  Lisp_Key_Data meta_key;
  if (NILP (Vmeta_prefix_char) ||
      (FIXNUMP (Vmeta_prefix_char) && !CHAR_INTP (Vmeta_prefix_char)))
    return;

  define_key_parser (Vmeta_prefix_char, &meta_key);
  mpc_binding = keymap_lookup_1 (keymap, &meta_key, 0);
  if (NILP (mpc_binding) || !NILP (Fkeymapp (mpc_binding)))
    return;

  if (indx == 0)
    new_keys = keys;
  else if (STRINGP (keys))
    new_keys = Fsubseq (keys, Qzero, make_fixnum (indx));
  else if (VECTORP (keys))
    {
      new_keys = make_vector (indx, Qnil);
      for (i = 0; i < indx; i++)
	XVECTOR_DATA (new_keys) [i] = XVECTOR_DATA (keys) [i];
    }
  else
    {
      new_keys = Qnil;
      ABORT ();
    }

  if (EQ (keys, new_keys))
    signal_ferror_with_frob (Qinvalid_operation, mpc_binding,
			     "can't bind %s: %s has a non-keymap binding",
			     (CIbyte *) XSTRING_DATA (Fkey_description (keys)),
			     (CIbyte *) XSTRING_DATA (Fsingle_key_description
						      (Vmeta_prefix_char)));
  else
    signal_ferror_with_frob (Qinvalid_operation, mpc_binding,
			     "can't bind %s: %s %s has a non-keymap binding",
			     (CIbyte *) XSTRING_DATA (Fkey_description (keys)),
			     (CIbyte *) XSTRING_DATA (Fkey_description
						      (new_keys)),
			     (CIbyte *) XSTRING_DATA (Fsingle_key_description
						      (Vmeta_prefix_char)));
}

DEFUN ("define-key", Fdefine_key, 3, 3, 0, /*
Define key sequence KEYS, in KEYMAP, as DEF.
KEYMAP is a keymap object.
KEYS is the key sequence to bind, described below.
DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap);
 a command (a Lisp function suitable for interactive calling);
 a string or key sequence vector (treated as a keyboard macro);
 a keymap (to define a prefix key);
 a symbol; when the key is looked up, the symbol will stand for its
    function definition, that should at that time be one of the above,
    or another symbol whose function definition is used, and so on.
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right);
 or a cons (KEYMAP . CHAR), meaning use definition of CHAR in map KEYMAP.

A `key sequence' is a vector of one or more keystrokes.
A `keystroke' is a list containing a key and zero or more modifiers.  The
key must be the last element of the list.
A `key' is a symbol corresponding to a key on the keyboard, or to a mouse
gesture.  Mouse clicks are denoted by symbols prefixed with "button",
followed by a digit for which button, and optionally "up".  Thus `button1'
means the down-stroke and `button1up' means the up-stroke when clicking
mouse button 1.
A `modifier' is a symbol naming a physical key which is only "noticed" by
XEmacs when chorded with another key.  The `shift' modifier is a special
case.  You cannot use `(meta shift a)' to mean `(meta A)', since for
characters that have ASCII equivalents, the state of the shift key is
implicit in the keysym (a vs. A).  You also cannot say `(shift =)' to mean
`+', as that correspondence varies from keyboard to keyboard.  The shift
modifier can only be applied to keys that do not have a second keysym on the
same key, such as `backspace' and `tab'.  A mouse click may be combined with
modifiers to create a compound "keystroke".

The keys, mouse gestures, and modifiers that are available depend on your
console and its driver.  At a minimum the ASCII graphic characters will be
available as keys, and shift, control, and meta as modifiers.

To find out programmatically what a key is bound to, use `key-binding' to
check all applicable keymaps, or `lookup-key' to check a specific keymap.
The documentation for `key-binding' also contains a description of which
keymaps are applicable in various situations.  `where-is-internal' does
the opposite of `key-binding', i.e. searches keymaps for the keys that
map to a particular binding.

If you are confused about why a particular key sequence is generating a
particular binding, and looking through the keymaps doesn't help, setting
the variable `debug-emacs-events' may help.  If not, try checking
what's in `function-key-map' and `key-translation-map'.

When running under a window system, typically the repertoire of keys is
vastly expanded.  XEmacs does its best to use the names defined on each
platform.  Also, when running under a window system, XEmacs can tell the
difference between the keystrokes control-h, control-shift-h, and backspace.
If the symbols differ, you can bind different actions to each.  For mouse
clicks, different commands may be bound to the up and down strokes, though
that is probably not what you want, so be careful.

Variant representations:

Besides the canonical representation as a vector of lists of symbols,
`define-key' also accepts a number of abbreviations, aliases, and variants
for convenience, compatibility, and internal use.

A key sequence can also be the vector [remap COMMAND]; this shadows any
bindings for COMMAND in KEYMAP, using DEF instead of COMMAND.  See
`command-remapping' and `remap-command'.  Specify [(remap) KEYSTROKE] if
your keyboard has a key with the name `remap' and you'd like to use it as a
prefix.

A keystroke may be represented by a key; this is treated as though it were a
list containing that key as the only element.  A keystroke may also be
represented by an event object, as returned by the `next-command-event' and
`read-key-sequence' functions.  A key sequence may be represented by a
single keystroke; this is treated as a vector containing that keystroke as
its only element.

A key may be represented by a character or its equivalent integer code,
if and only if it is equivalent to a character with a code in the range
32 - 255.

For backward compatibility, a key sequence may also be represented by a
string.  In this case, it represents the key sequence(s) that would
produce that sequence of ASCII characters in a purely ASCII world.  An
alternative string representation is keyboard macro notation, which can
be translated to the canonical representation with `kbd'.

Examples:

The key sequence `A' (which invokes `self-insert-command') is represented
by all of these forms:
	A	?A	65	(A)	(?A)	(65)
	[A]	[?A]	[65]	[(A)]	[(?A)]	[(65)]

The key sequence `control-a' is represented by these forms:
	(control A)	(control ?A)	(control 65)
	[(control A)]	[(control ?A)]	[(control 65)]

The key sequence `control-c control-a' is represented by these forms:
	[(control c) (control a)]	[(control ?c) (control ?a)]
	[(control 99) (control 65)]	etc.

The keystroke `control-b' *may not* be represented by the number 2 (the
ASCII code for ^B) or the character `?\^B'.

The `break' key may be represented only by the symbol `break'.

Mouse button clicks work just like keypresses: (control button1) means
pressing the left mouse button while holding down the control key.

A string containing the ASCII backspace character, "\\^H", would represent
two key sequences: `(control h)' and `backspace'.  Binding a
command to this will actually bind both of those key sequences.  Likewise
for the following pairs:

		control h	backspace
		control i   	tab
		control m   	return
		control j   	linefeed
		control [   	escape
		control @	control space

After binding a command to two key sequences with a form like

	(define-key global-map "\\^X\\^I" \'command-1)

it is possible to redefine only one of those sequences like so:

	(define-key global-map [(control x) (control i)] \'command-2)
	(define-key global-map [(control x) tab] \'command-3)
*/
       (keymap, keys, def))
{
  /* This function can GC */
  int idx;
  int metized = 0;
  int len;
  int ascii_hack;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (VECTORP (keys))
    len = XVECTOR_LENGTH (keys);
  else if (STRINGP (keys))
    len = string_char_length (keys);
  else if (CHAR_OR_CHAR_INTP (keys) || SYMBOLP (keys) || CONSP (keys))
    {
      if (!CONSP (keys)) keys = list1 (keys);
      len = 1;
      keys = make_vector (1, keys); /* this is kinda sleazy. */
    }
  else
    {
      keys = wrong_type_argument (Qsequencep, keys);
      len = XFIXNUM (Flength (keys));
    }
  if (len == 0)
    return Qnil;

  GCPRO3 (keymap, keys, def);

  /* ASCII grunge.
     When the user defines a key which, in a strictly ASCII world, would be
     produced by two different keys (^J and linefeed, or ^H and backspace,
     for example) then the binding will be made for both keysyms.

     This is done if the user binds a command to a string, as in
     (define-key map "\^H" 'something), but not when using the canonical
     syntax (define-key map '(control h) 'something).
     */
  ascii_hack = (STRINGP (keys));

  keymap = get_keymap (keymap, 1, 1);

  /* Allow access to any keys named remap, use our uninterned symbol. For
     compatibility, don't sanity-check (aref KEYS 1) or DEF. */
  if (2 == len && VECTORP (keys) && EQ (Qremap, XVECTOR_DATA (keys) [0]))
    {
      RETURN_UNGCPRO (remap_command (keymap, XVECTOR_DATA (keys) [1], def));
    }

  idx = 0;
  while (1)
    {
      Lisp_Object c;
      Lisp_Key_Data raw_key1;
      Lisp_Key_Data raw_key2;
      if (STRINGP (keys))
	c = make_char (string_ichar (keys, idx));
      else
	c = XVECTOR_DATA (keys) [idx];

      define_key_parser (c, &raw_key1);

      if (!metized && ascii_hack && meta_prefix_char_p (&raw_key1))
	{
	  if (idx == (len - 1))
	    {
	      /* This is a hack to prevent a binding for the meta-prefix-char
		 from being made in a map which already has a non-empty "meta"
		 submap.  That is, we can't let both "escape" and "meta" have
		 a binding in the same keymap.  This implies that the idiom
		 (define-key my-map "\e" my-escape-map)
		 (define-key my-escape-map "a" 'my-command)
		 no longer works.  That's ok.  Instead the luser should do
		 (define-key my-map "\ea" 'my-command)
		 or, more correctly
		 (define-key my-map "\M-a" 'my-command)
		 and then perhaps
		 (defvar my-escape-map (lookup-key my-map "\e"))
		 if the luser really wants the map in a variable.
		 */
	      Lisp_Object meta_map;
              struct gcpro ngcpro1;

              NGCPRO1 (c);
              meta_map = Fgethash (MAKE_MODIFIER_HASH_KEY (XEMACS_MOD_META),
				   XKEYMAP (keymap)->table, Qnil);
	      if (!NILP (meta_map)
		  && keymap_fullness (meta_map) != 0)
		invalid_operation_2
		  ("Map contains meta-bindings, can't bind",
		   Fsingle_key_description (Vmeta_prefix_char), keymap);
              NUNGCPRO;
	    }
	  else
	    {
	      metized = 1;
	      idx++;
	      continue;
	    }
	}

      if (ascii_hack)
	define_key_alternate_name (&raw_key1, &raw_key2);
      else
	{
	  raw_key2.keysym = Qnil;
	  raw_key2.modifiers = 0;
	}

      if (metized)
	{
	  raw_key1.modifiers |= XEMACS_MOD_META;
	  raw_key2.modifiers |= XEMACS_MOD_META;
	  metized = 0;
	}

      /* This crap is to make sure that someone doesn't bind something like
	 "C-x M-a" while "C-x ESC" has a non-keymap binding. */
      if (raw_key1.modifiers & XEMACS_MOD_META)
	ensure_meta_prefix_char_keymapp (keys, idx, keymap);

      if (++idx == len)
	{
	  keymap_store (keymap, &raw_key1, def);
	  if (ascii_hack && !NILP (raw_key2.keysym))
	    keymap_store (keymap, &raw_key2, def);
	  UNGCPRO;
	  return def;
	}

      {
        Lisp_Object cmd;
        struct gcpro ngcpro1;
        NGCPRO1 (c);

        cmd = keymap_lookup_1 (keymap, &raw_key1, 0);
	if (NILP (cmd))
	  {
	    cmd = Fmake_sparse_keymap (Qnil);
	    XKEYMAP (cmd)->name /* for debugging */
	      = list2 (make_key_description (&raw_key1, 1), keymap);
	    keymap_store (keymap, &raw_key1, cmd);
	  }
	if (NILP (Fkeymapp (cmd)))
          sferror_2 ("Invalid prefix keys in sequence",
				 c, keys);

	if (ascii_hack && !NILP (raw_key2.keysym) &&
	    NILP (keymap_lookup_1 (keymap, &raw_key2, 0)))
	  keymap_store (keymap, &raw_key2, cmd);

	keymap = get_keymap (cmd, 1, 1);
        NUNGCPRO;
      }
    }
}

static Lisp_Object
remap_command (Lisp_Object keymap, Lisp_Object old, Lisp_Object new_)
{
  Lisp_Key_Data parsed;
  Lisp_Object cmd;

  define_key_parser (Qxemacs_command_remapping, &parsed);
  cmd = keymap_lookup_1 (keymap, &parsed, 0);
  if (NILP (cmd))
    {
      cmd = Fmake_sparse_keymap (Qnil);
      XKEYMAP (cmd)->name /* for debugging */
        = list2 (make_key_description (&parsed, 1), keymap);
      keymap_store (keymap, &parsed, cmd);
    }

  assert (!NILP (Fkeymapp (cmd)));
  define_key_parser (old, &parsed);
  keymap_store (cmd, &parsed, new_);
  return new_;
}


DEFUN ("remap-command", Fremap_command, 3, 3, 0, /*
Ensure that NEW is called when previously OLD would be, in KEYMAP.

NEW and OLD are both command symbols.  KEYMAP is a keymap object.

This is equivalent to `(define-key KEYMAP [remap OLD] NEW])'.  See also
`substitute-key-definition', an older way of doing a similar thing.
*/
       (keymap, old, new_))
{
  keymap = get_keymap (keymap, 1, 1);
  CHECK_COMMAND (old);
  CHECK_COMMAND (new_);

  return remap_command (keymap, old, new_);
}

static Lisp_Object
command_remapping (Lisp_Object definition, int nmaps, Lisp_Object *maps)
{
  Lisp_Object remapping = Qnil;
  Lisp_Object keys[2] = { Qxemacs_command_remapping, definition };
  int jj;

  for (jj = 0; jj < nmaps; jj++)
    {
      remapping = lookup_keys (maps[jj], countof (keys), keys, 0);
      if (!NILP (remapping) && !FIXNUMP (remapping))
        {
          return remapping;
        }
    }

  return Qnil;
}

DEFUN ("command-remapping", Fcommand_remapping, 1, 3, 0, /*
Return the remapping for command COMMAND.

Return nil if COMMAND is not remapped (or not a symbol).  The remapping is
the command that is executed when some key sequence in the relevant keymaps
would normally execute COMMAND, but this has been intercepted by
`remap-command' or the [remap COMMAND] syntax for KEYS in `define-key'.

If the optional argument POSITION is non-nil, it specifies an event, and the
remapping occurs in the keymaps associated with it.  It can also be a number
or marker, in which case the keymap properties at the specified buffer
position instead of point are used.  The KEYMAPS argument is ignored if
POSITION is non-nil.

If the optional argument KEYMAPS is non-nil, it should be a list of
keymaps to search for command remapping.  Otherwise, search for the
remapping in all currently active keymaps.
*/
       (command, position, keymaps))
{
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps, maps_count = countof (maps);

  CHECK_COMMAND (command);
  CHECK_LIST (keymaps);
  CHECK_REMAPPING_POSITION (position);

  /* Get keymaps as an array */
  if (NILP (keymaps) || !NILP (position))
    {
      nmaps = get_relevant_keymaps (Qnil, position, maps_count, gubbish);
    }
  else
    {
      Elemcount jj = 0;
      nmaps = XFIXNUM (Flength (keymaps));
      if (nmaps > maps_count)
        {
          gubbish = alloca_array (Lisp_Object, nmaps);
        }

      {
        LIST_LOOP_2 (elt, keymaps)
          {
            gubbish[jj++] = elt;
          }
      }
    }

  if (nmaps > maps_count)
    {
      gubbish = alloca_array (Lisp_Object, nmaps);
      nmaps = get_relevant_keymaps (Qnil, position, nmaps, gubbish);
    }

  return command_remapping (command, nmaps, gubbish);
}

struct commands_remapped_to_closure
{
  Lisp_Object command;
  Lisp_Object result;
};

static void
commands_remapped_to_mapper (const Lisp_Key_Data *key, Lisp_Object binding,
                             void *data)
{
  struct commands_remapped_to_closure *crtc
    = (struct commands_remapped_to_closure *) data;

  if (EQ (binding, crtc->command))
    {
      crtc->result = Fcons (key->keysym, crtc->result);
    }
}

static Lisp_Object
commands_remapped_to_traverser (Lisp_Object k, void *arg)
{
  Lisp_Object remapping
    = lookup_keys (k, 1, &Qxemacs_command_remapping, 0);
  if (KEYMAPP (remapping))
    {
      map_keymap (XKEYMAP (remapping)->table, 0, commands_remapped_to_mapper,
                  arg);
    }

  return Qnil;
}

DEFUN ("commands-remapped-to", Fcommands_remapped_to, 1, 3, 0, /*
Return a list of symbols for which COMMAND is their remapping in KEYMAPS.

This is the inverse operation of `command-remapping', which see.
*/
       (command, keymaps, position))
{
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps, maps_count = countof (maps), jj;
  struct commands_remapped_to_closure closure = { command, Qnil };
  struct gcpro gcpro1;
  
  CHECK_COMMAND (command);
  CHECK_LIST (keymaps);
  CHECK_REMAPPING_POSITION (position);

  /* Get keymaps as an array */
  if (NILP (keymaps) || !NILP (position))
    {
      nmaps = get_relevant_keymaps (Qnil, position, maps_count, gubbish);
    }
  else
    {
      jj = 0;
      nmaps = XFIXNUM (Flength (keymaps));
      if (nmaps > maps_count)
        {
          gubbish = alloca_array (Lisp_Object, nmaps);
        }

      {
        LIST_LOOP_2 (elt, keymaps)
          {
            gubbish[jj++] = elt;
          }
      }
    }

  if (nmaps > maps_count)
    {
      gubbish = alloca_array (Lisp_Object, nmaps);
      nmaps = get_relevant_keymaps (Qnil, position, nmaps, gubbish);
    }

  GCPRO1 (closure.result);
  
  for (jj = 0; jj < nmaps; jj++)
    {
      traverse_keymaps (maps[jj], Qnil, commands_remapped_to_traverser,
                        (void *) (&closure));
    }

  RETURN_UNGCPRO (closure.result);
}

/************************************************************************/
/*                      Looking up keys in keymaps                      */
/************************************************************************/

/* We need a very fast (i.e., non-consing) version of lookup-key in order
   to make where-is-internal really fly. */

struct raw_lookup_key_mapper_closure
{
  int remaining;
  const Lisp_Key_Data *raw_keys;
  int raw_keys_count;
  int keys_so_far;
  int accept_default;
};

static Lisp_Object raw_lookup_key_mapper (Lisp_Object k, void *);

/* Caller should gc-protect args (keymaps may autoload) */
static Lisp_Object
raw_lookup_key (Lisp_Object keymap,
                const Lisp_Key_Data *raw_keys, int raw_keys_count,
                int keys_so_far, int accept_default)
{
  /* This function can GC */
  struct raw_lookup_key_mapper_closure c;
  c.remaining = raw_keys_count - 1;
  c.raw_keys = raw_keys;
  c.raw_keys_count = raw_keys_count;
  c.keys_so_far = keys_so_far;
  c.accept_default = accept_default;

  return traverse_keymaps (keymap, Qnil, raw_lookup_key_mapper, &c);
}

static Lisp_Object
raw_lookup_key_mapper (Lisp_Object k, void *arg)
{
  /* This function can GC */
  struct raw_lookup_key_mapper_closure *c =
    (struct raw_lookup_key_mapper_closure *) arg;
  int accept_default = c->accept_default;
  int remaining = c->remaining;
  int keys_so_far = c->keys_so_far;
  const Lisp_Key_Data *raw_keys = c->raw_keys;
  Lisp_Object cmd;

  if (! meta_prefix_char_p (&(raw_keys[0])))
    {
      /* Normal case: every case except the meta-hack (see below). */
      cmd = keymap_lookup_1 (k, &(raw_keys[0]), accept_default);

      if (remaining == 0)
	/* Return whatever we found if we're out of keys */
	;
      else if (NILP (cmd))
	/* Found nothing (though perhaps parent map may have binding) */
	;
      else if (NILP (Fkeymapp (cmd)))
	/* Didn't find a keymap, and we have more keys.
	 * Return a fixnum to indicate that keys were too long.
	 */
	cmd = make_fixnum (keys_so_far + 1);
      else
	cmd = raw_lookup_key (cmd, raw_keys + 1, remaining,
			      keys_so_far + 1, accept_default);
    }
  else
    {
      /* This is a hack so that looking up a key-sequence whose last
       * element is the meta-prefix-char will return the keymap that
       * the "meta" keys are stored in, if there is no binding for
       * the meta-prefix-char (and if this map has a "meta" submap).
       * If this map doesn't have a "meta" submap, then the
       * meta-prefix-char is looked up just like any other key.
       */
      if (remaining == 0)
	{
	  /* First look for the prefix-char directly */
	  cmd = keymap_lookup_1 (k, &(raw_keys[0]), accept_default);
	  if (NILP (cmd))
	    {
	      /* Do kludgy return of the meta-map */
	      cmd = Fgethash (MAKE_MODIFIER_HASH_KEY (XEMACS_MOD_META),
			      XKEYMAP (k)->table, Qnil);
	    }
	}
      else
	{
	  /* Search for the prefix-char-prefixed sequence directly */
	  cmd = keymap_lookup_1 (k, &(raw_keys[0]), accept_default);
	  cmd = get_keymap (cmd, 0, 1);
	  if (!NILP (cmd))
	    cmd = raw_lookup_key (cmd, raw_keys + 1, remaining,
				  keys_so_far + 1, accept_default);
	  else if ((raw_keys[1].modifiers & XEMACS_MOD_META) == 0)
	    {
	      Lisp_Key_Data metified;
	      metified.keysym = raw_keys[1].keysym;
	      metified.modifiers = raw_keys[1].modifiers |
		(unsigned char) XEMACS_MOD_META;

	      /* Search for meta-next-char sequence directly */
	      cmd = keymap_lookup_1 (k, &metified, accept_default);
	      if (remaining == 1)
		;
	      else
		{
		  cmd = get_keymap (cmd, 0, 1);
		  if (!NILP (cmd))
		    cmd = raw_lookup_key (cmd, raw_keys + 2, remaining - 1,
					  keys_so_far + 2,
					  accept_default);
		}
	    }
	}
    }
  if (accept_default && NILP (cmd))
    cmd = XKEYMAP (k)->default_binding;
  return cmd;
}

/* Value is number if `keys' is too long; NIL if valid but has no definition.*/
/* Caller should gc-protect arguments */
static Lisp_Object
lookup_keys (Lisp_Object keymap, int nkeys, Lisp_Object *keys,
             int accept_default)
{
  /* This function can GC */
  Lisp_Key_Data kkk[20];
  Lisp_Key_Data *raw_keys;
  int i;

  if (nkeys == 0)
    return Qnil;

  if (nkeys < countof (kkk))
    raw_keys = kkk;
  else
    raw_keys = alloca_array (Lisp_Key_Data, nkeys);

  for (i = 0; i < nkeys; i++)
    {
      define_key_parser (keys[i], &(raw_keys[i]));
    }
  return raw_lookup_key (keymap, raw_keys, nkeys, 0, accept_default);
}

static Lisp_Object
lookup_events (Lisp_Object event_head, int nmaps, Lisp_Object keymaps[],
               int accept_default)
{
  /* This function can GC */
  Lisp_Key_Data kkk[20];
  Lisp_Object event;

  int nkeys;
  Lisp_Key_Data *raw_keys;
  Lisp_Object tem = Qnil;
  struct gcpro gcpro1, gcpro2;
  int iii;

  CHECK_LIVE_EVENT (event_head);

  nkeys = event_chain_count (event_head);

  if (nkeys < countof (kkk))
    raw_keys = kkk;
  else
    raw_keys = alloca_array (Lisp_Key_Data, nkeys);

  nkeys = 0;
  EVENT_CHAIN_LOOP (event, event_head)
    define_key_parser (event, &(raw_keys[nkeys++]));
  GCPRO2 (keymaps[0], event_head);
  gcpro1.nvars = nmaps;
  /* ####raw_keys[].keysym slots aren't gc-protected.  We rely (but shouldn't)
   * on somebody else somewhere (obarray) having a pointer to all keysyms. */
  for (iii = 0; iii < nmaps; iii++)
    {
      tem = raw_lookup_key (keymaps[iii], raw_keys, nkeys, 0,
			    accept_default);
      if (FIXNUMP (tem))
	{
	  /* Too long in some local map means don't look at global map */
	  tem = Qnil;
	  break;
	}
      else if (!NILP (tem))
	break;
    }
  UNGCPRO;
  return tem;
}

DEFUN ("lookup-key", Flookup_key, 2, 3, 0, /*
In keymap KEYMAP, look up key-sequence KEYS.  Return the definition.
Nil is returned if KEYS is unbound.  See documentation of `define-key'
for valid key definitions and key-sequence specifications.
A number is returned if KEYS is "too long"; that is, the leading
characters fail to be a valid sequence of prefix characters in KEYMAP.
The number is how many key strokes at the front of KEYS it takes to
reach a non-prefix command.
*/
       (keymap, keys, accept_default))
{
  /* This function can GC */
  if (VECTORP (keys))
    return lookup_keys (keymap,
			XVECTOR_LENGTH (keys),
			XVECTOR_DATA (keys),
			!NILP (accept_default));
  else if (SYMBOLP (keys) || CHAR_OR_CHAR_INTP (keys) || CONSP (keys))
    return lookup_keys (keymap, 1, &keys, !NILP (accept_default));
  else if (STRINGP (keys))
    {
      int length = string_char_length (keys);
      int i;
      Lisp_Key_Data *raw_keys = alloca_array (Lisp_Key_Data, length);
      if (length == 0)
	return Qnil;

      for (i = 0; i < length; i++)
	{
          Ichar n = string_ichar (keys, i);
	  define_key_parser (make_char (n), &(raw_keys[i]));
	}
      return raw_lookup_key (keymap, raw_keys, length, 0,
			     !NILP (accept_default));
    }
  else
    {
      keys = wrong_type_argument (Qsequencep, keys);
      return Flookup_key (keymap, keys, accept_default);
    }
}

/* Given a key sequence, returns a list of keymaps to search for bindings.
   Does all manner of semi-hairy heuristics, like looking in the current
   buffer's map before looking in the global map and looking in the local
   map of the buffer in which the mouse was clicked in event0 is a click.

   It would be kind of nice if this were in Lisp so that this semi-hairy
   semi-heuristic command-lookup behavior could be readily understood and
   customised.  However, this needs to be pretty fast, or performance of
   keyboard macros goes to shit; putting this in lisp slows macros down
   2-3x.  And they're already slower than v18 by 5-6x.
 */

struct relevant_maps
  {
    int nmaps;
    int max_maps;
    Lisp_Object *maps;
    struct gcpro *gcpro;
  };

static void get_relevant_extent_keymaps (Lisp_Object pos,
                                         Lisp_Object buffer_or_string,
                                         Lisp_Object glyph,
                                         struct relevant_maps *closure);
static void get_relevant_minor_maps (Lisp_Object buffer,
                                     struct relevant_maps *closure);

static void
relevant_map_push (Lisp_Object map, struct relevant_maps *closure)
{
  int nmaps = closure->nmaps;

  if (!KEYMAPP (map))
    return;
  closure->nmaps = nmaps + 1;
  if (nmaps < closure->max_maps)
    {
      closure->maps[nmaps] = map;
      closure->gcpro->nvars = nmaps;
    }
}

static int
get_relevant_keymaps (Lisp_Object keys, Lisp_Object position, int max_maps,
                      Lisp_Object maps[])
{
  /* This function can GC */
  Lisp_Object terminal = Qnil;
  struct gcpro gcpro1;
  struct relevant_maps closure;
  struct console *con;

  GCPRO1 (*maps);
  gcpro1.nvars = 0;
  closure.nmaps = 0;
  closure.max_maps = max_maps;
  closure.maps = maps;
  closure.gcpro = &gcpro1;

  if (EVENTP (keys))
    terminal = event_chain_tail (keys);
  else if (VECTORP (keys))
    {
      int len = XVECTOR_LENGTH (keys);
      if (len > 0)
	terminal = XVECTOR_DATA (keys)[len - 1];
    }

  if (EVENTP (terminal))
    {
      CHECK_LIVE_EVENT (terminal);
      con = event_console_or_selected (terminal);
    }
  else
    con = XCONSOLE (Vselected_console);

  if (KEYMAPP (con->overriding_terminal_local_map)
      || KEYMAPP (Voverriding_local_map))
    {
      if (KEYMAPP (con->overriding_terminal_local_map))
	relevant_map_push (con->overriding_terminal_local_map, &closure);
      if (KEYMAPP (Voverriding_local_map))
	relevant_map_push (Voverriding_local_map, &closure);
    }
  else if (!EVENTP (terminal)
           || (XEVENT (terminal)->event_type != button_press_event
               && XEVENT (terminal)->event_type != button_release_event))
    {
      Lisp_Object tem = wrap_buffer (current_buffer);

      /* It's not a mouse event; order of keymaps searched is:
	 o  keymap of any/all extents under the mouse
	 o  minor-mode maps
	 o  local-map of current-buffer
	 o  global-tty-map or global-window-system-map
	 o  global-map
	 */
      /* The terminal element of the lookup may be nil or a keysym.
         In those cases we don't want to check for an extent
         keymap. */
      if (EVENTP (terminal))
	{
	  get_relevant_extent_keymaps (make_fixnum (BUF_PT (current_buffer)),
				       tem, Qnil, &closure);
	}
      get_relevant_minor_maps (tem, &closure);

      tem = current_buffer->keymap;
      if (!NILP (tem))
	relevant_map_push (tem, &closure);
    }
#ifdef HAVE_WINDOW_SYSTEM
  else
    {
      /* It's a mouse event; order of keymaps searched is:
	 o  vertical-divider-map, if event is over a divider
	 o  local-map of mouse-grabbed-buffer
	 o  keymap of any/all extents under the mouse
	 if the mouse is over a modeline:
	 o  modeline-map of buffer corresponding to that modeline
	 o  else, local-map of buffer under the mouse
	 o  minor-mode maps
	 o  local-map of current-buffer
	 o  global-tty-map or global-window-system-map
	 o  global-map
	 */
      Lisp_Object window = Fevent_window (terminal);

      if (!NILP (Fevent_over_vertical_divider_p (terminal)))
	{
	  if (KEYMAPP (Vvertical_divider_map))
	    relevant_map_push (Vvertical_divider_map, &closure);
	}

      if (BUFFERP (Vmouse_grabbed_buffer))
	{
	  Lisp_Object map = XBUFFER (Vmouse_grabbed_buffer)->keymap;

	  get_relevant_minor_maps (Vmouse_grabbed_buffer, &closure);
	  if (!NILP (map))
	    relevant_map_push (map, &closure);
	}

      if (!NILP (window))
	{
	  Lisp_Object buffer = Fwindow_buffer (window);

	  if (!NILP (buffer))
	    {
	      if (!NILP (Fevent_over_modeline_p (terminal)))
		{
		  Lisp_Object map = symbol_value_in_buffer (Qmodeline_map,
							    buffer);

		  get_relevant_extent_keymaps
		    (Fevent_modeline_position (terminal),
		     XBUFFER (buffer)->generated_modeline_string,
		     Fevent_glyph_extent (terminal), &closure);

		  if (!UNBOUNDP (map) && !NILP (map))
		    relevant_map_push (get_keymap (map, 1, 1), &closure);
		}
	      else
		{
		  get_relevant_extent_keymaps (Fevent_point (terminal), buffer,
					       Fevent_glyph_extent (terminal),
					       &closure);
		}

	      if (!EQ (buffer, Vmouse_grabbed_buffer)) /* already pushed */
		{
		  Lisp_Object map = XBUFFER (buffer)->keymap;

		  get_relevant_minor_maps (buffer, &closure);
		  if (!NILP(map))
		    relevant_map_push (map, &closure);
		}
	    }
	}
      else if (!NILP (Fevent_over_toolbar_p (terminal)))
	{
	  Lisp_Object map = Fsymbol_value (Qtoolbar_map);

	  if (!UNBOUNDP (map) && !NILP (map))
	    relevant_map_push (map, &closure);
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */

  if (FIXNUMP (position))
    {
      get_relevant_extent_keymaps (position, wrap_buffer (current_buffer),
                                   Qnil, &closure);
    }
  else if (MARKERP (position) && !NILP (Fmarker_buffer (position)))
    {
      get_relevant_extent_keymaps (Fmarker_position (position),
                                   Fmarker_buffer (position),
                                   Qnil, &closure);
    }
  else if (EVENTP (position))
    {
      Lisp_Object ew = Fevent_window (position);

      get_relevant_extent_keymaps (Fevent_point (position),
                                   WINDOWP (ew) ?
                                   Fwindow_buffer (Fevent_window (position))
                                   : Qnil, Qnil, &closure);
    }
  else
    {
      assert (NILP (position));
    }

  if (CONSOLE_TTY_P (con))
    relevant_map_push (Vglobal_tty_map, &closure);
  else
    relevant_map_push (Vglobal_window_system_map, &closure);
  
  {
    int nmaps = closure.nmaps;
    /* Silently truncate at 100 keymaps to prevent infinite lossage */
    if (nmaps >= max_maps && max_maps > 0)
      maps[max_maps - 1] = Vcurrent_global_map;
    else
      maps[nmaps] = Vcurrent_global_map;
    UNGCPRO;
    return nmaps + 1;
  }
}

/* Returns a set of keymaps extracted from the extents at POS in
   BUFFER_OR_STRING.  The GLYPH arg, if specified, is one more extent
   to look for a keymap in, and if it has one, its keymap will be the
   first element in the list returned.  This is so we can correctly
   search the keymaps associated with glyphs which may be physically
   disjoint from their extents: for example, if a glyph is out in the
   margin, we should still consult the keymap of that glyph's extent,
   which may not itself be under the mouse.
 */

static void
get_relevant_extent_keymaps (Lisp_Object pos, Lisp_Object buffer_or_string,
                             Lisp_Object glyph,
                             struct relevant_maps *closure)
{
  /* This function can GC */
  /* the glyph keymap, if any, comes first.
     (Processing it twice is no big deal: noop.) */
  if (!NILP (glyph))
    {
      Lisp_Object keymap = Fextent_property (glyph, Qkeymap, Qnil);
      if (!NILP (keymap))
	relevant_map_push (get_keymap (keymap, 1, 1), closure);
    }

  /* Next check the extents at the text position, if any */
  if (!NILP (pos))
    {
      Lisp_Object extent;
      for (extent = Fextent_at (pos, buffer_or_string, Qkeymap, Qnil, Qnil);
	   !NILP (extent);
	   extent = Fextent_at (pos, buffer_or_string, Qkeymap, extent, Qnil))
	{
	  Lisp_Object keymap = Fextent_property (extent, Qkeymap, Qnil);
	  if (!NILP (keymap))
	    relevant_map_push (get_keymap (keymap, 1, 1), closure);
	  QUIT;
	}
    }
}

static Lisp_Object
minor_mode_keymap_predicate (Lisp_Object assoc, Lisp_Object buffer)
{
  /* This function can GC */
  if (CONSP (assoc))
    {
      Lisp_Object sym = XCAR (assoc);
      if (SYMBOLP (sym))
	{
	  Lisp_Object val = symbol_value_in_buffer (sym, buffer);
	  if (!NILP (val) && !UNBOUNDP (val))
	    {
	      return get_keymap (XCDR (assoc), 0, 1);
	    }
	}
    }
  return Qnil;
}

static void
get_relevant_minor_maps (Lisp_Object buffer, struct relevant_maps *closure)
{
  /* This function can GC */
  Lisp_Object alist;

  /* Will you ever lose badly if you make this circular! */
  for (alist = symbol_value_in_buffer (Qminor_mode_map_alist, buffer);
       CONSP (alist);
       alist = XCDR (alist))
    {
      Lisp_Object m = minor_mode_keymap_predicate (XCAR (alist),
						   buffer);
      if (!NILP (m)) relevant_map_push (m, closure);
      QUIT;
    }
}

/* #### Would map-current-keymaps be a better thing?? */
DEFUN ("current-keymaps", Fcurrent_keymaps, 0, 1, 0, /*
Return a list of the current keymaps that will be searched for bindings.
This lists keymaps such as the current local map and the minor-mode maps,
 but does not list the parents of those keymaps.
EVENT-OR-KEYS controls which keymaps will be listed.
If EVENT-OR-KEYS is a mouse event (or a vector whose last element is a
 mouse event), the keymaps for that mouse event will be listed (see
 `key-binding').  Otherwise, the keymaps for key presses will be listed.
See `key-binding' for a description of which keymaps are searched in
various situations.
*/
       (event_or_keys))
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps;

  GCPRO1 (event_or_keys);
  nmaps = get_relevant_keymaps (event_or_keys, Qnil, countof (maps),
				gubbish);
  if (nmaps > countof (maps))
    {
      gubbish = alloca_array (Lisp_Object, nmaps);
      nmaps = get_relevant_keymaps (event_or_keys, Qnil, nmaps, gubbish);
    }
  UNGCPRO;
  return Flist (nmaps, gubbish);
}

DEFUN ("key-binding", Fkey_binding, 1, 4, 0, /*
Return the binding for command KEYS in current keymaps.

KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.

NO-REMAP, if non-nil, specifies that any substitutions that have been
specified by `remap-command' (or, equivalently, by `(define-key KEYMAP
\[remap OLD] NEW)') should be ignored.

POSITION, if non-nil, specifies a marker (and its associated buffer) or an
integer position (in the current buffer) to examine for relevant keymaps.
It can also be an event, in which case the associated buffer and position of
that event will be used.

The binding is probably a symbol with a function definition; see the
documentation for `lookup-key' for more information.

For key-presses, the order of keymaps searched is:
  - the `keymap' property of any extent(s) at point;
  - any applicable minor-mode maps;
  - the current local map of the current-buffer;
  - either `global-tty-map' or `global-window-system-map', depending on
    whether the current console is a TTY or non-TTY console;
  - the current global map.

For mouse-clicks, the order of keymaps searched is:
  - the current-local-map of the `mouse-grabbed-buffer' if any;
  - vertical-divider-map, if the event happened over a vertical divider
  - the `keymap' property of any extent(s) at the position of the click
    (this includes modeline extents);
  - the modeline-map of the buffer corresponding to the modeline under
    the mouse (if the click happened over a modeline);
  - the value of `toolbar-map' in the current-buffer (if the click
    happened over a toolbar);
  - the current local map of the buffer under the mouse (does not
    apply to toolbar clicks);
  - any applicable minor-mode maps;
  - either `global-tty-map' or `global-window-system-map', depending on
    whether the current console is a TTY or non-TTY console;
  - the current global map.

Note that if `overriding-local-map' or `overriding-terminal-local-map'
is non-nil, *only* those two maps and the current global map are searched.

Note also that key sequences actually received from the keyboard driver
may be processed in various ways to generate the key sequence that is
actually looked up in the keymaps.  In particular:

-- Keysyms are individually passed through `keyboard-translate-table' before
   any other processing.
-- After this, key sequences as a whole are passed through
   `key-translation-map'.
-- The resulting key sequence is actually looked up in the keymaps.
-- If there's no binding found, the key sequence is passed through
   `function-key-map' and looked up again.
-- If no binding is found and `retry-undefined-key-binding-unshifted' is
   set (it usually is) and the final keysym is an uppercase character,
   we lowercase it and start over from the `key-translation-map' stage.
-- If no binding is found and we're on MS Windows and have international
   support, we successively remap the key sequence using the keyboard layouts
   of various default locales (current language environment, user default,
   system default, US ASCII) and try again.  This makes (e.g.) sequences
   such as `C-x b' work in a Russian locale, where the alphabetic keys are
   actually generating Russian characters and not the Roman letters written
   on the keycaps. (Not yet implemented)
-- Finally, if the last keystroke matches `help-char', we automatically
   generate and display a list of possible key sequences and bindings
   given the prefix so far generated.
*/
       (keys, accept_default, no_remap, position))
{
  /* This function can GC */
  int i;
  Lisp_Object maps[100];
  int nmaps;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (keys, accept_default); /* get_relevant_keymaps may autoload */

  nmaps = get_relevant_keymaps (keys, position, countof (maps), maps);

  UNGCPRO;

  if (EVENTP (keys))           /* unadvertised "feature" for the future */
    return lookup_events (keys, nmaps, maps, !NILP (accept_default));

  for (i = 0; i < nmaps; i++)
    {
      Lisp_Object tem = Flookup_key (maps[i], keys, accept_default);

      if (FIXNUMP (tem))
	{
	  /* Too long in some local map means don't look at global map */
	  return Qnil;
	}

      if (!NILP (tem) && NILP (no_remap) && SYMBOLP (tem))
        {
          Lisp_Object remap = command_remapping (tem, nmaps, maps);
          return NILP (remap) ? tem : remap;
        }
    }
  return Qnil;
}

static Lisp_Object
process_event_binding_result (Lisp_Object result)
{
  if (EQ (result, Qundefined))
    /* The suppress-keymap function binds keys to `undefined' - special-case
       that here, so that being bound to that has the same error-behavior as
       not being defined at all.
       */
    result = Qnil;
  if (!NILP (result))
    {
      Lisp_Object map;
      /* Snap out possible keymap indirections */
      map = get_keymap (result, 0, 1);
      if (!NILP (map))
	result = map;
    }

  return result;
}

/* Attempts to find a command corresponding to the event-sequence
   whose head is event0 (sequence is threaded though event_next).

   The return value will be

      -- nil (there is no binding; this will also be returned
              whenever the event chain is "too long", i.e. there
	      is a non-nil, non-keymap binding for a prefix of
	      the event chain)
      -- a keymap (part of a command has been specified)
      -- a command (anything that satisfies `commandp'; this includes
                    some symbols, lists, subrs, strings, vectors, and
		    compiled-function objects) */
Lisp_Object
event_binding (Lisp_Object event0, int accept_default)
{
  /* This function can GC */
  Lisp_Object maps[100], result;
  int nmaps;

  assert (EVENTP (event0));

  nmaps = get_relevant_keymaps (event0, Qnil, countof (maps), maps);
  if (nmaps > countof (maps))
    nmaps = countof (maps);

  result = process_event_binding_result (lookup_events (event0, nmaps, maps,
							accept_default));

  if (!NILP (result) && SYMBOLP (result))
    {
      Lisp_Object remap = command_remapping (result, nmaps, maps);
      if (!NILP (remap))
	{
	  result = remap;
	}
    }

  return result;
}

/* like event_binding, but specify a keymap to search */

Lisp_Object
event_binding_in (Lisp_Object event0, Lisp_Object keymap, int accept_default)
{
  /* This function can GC */
  if (!KEYMAPP (keymap))
    return Qnil;

  return process_event_binding_result (lookup_events (event0, 1, &keymap,
						      accept_default));
}

/* Attempts to find a function key mapping corresponding to the
   event-sequence whose head is event0 (sequence is threaded through
   event_next).  The return value will be the same as for event_binding(). */
Lisp_Object
munging_key_map_event_binding (Lisp_Object event0,
			       enum munge_me_out_the_door munge)
{
  Lisp_Object keymap = (munge == MUNGE_ME_FUNCTION_KEY) ?
    CONSOLE_FUNCTION_KEY_MAP (event_console_or_selected (event0)) :
    Vkey_translation_map;

  if (NILP (keymap))
    return Qnil;

  return process_event_binding_result (lookup_events (event0, 1, &keymap, 1));
}


/************************************************************************/
/*               Setting/querying the global and local maps             */
/************************************************************************/

DEFUN ("use-global-map", Fuse_global_map, 1, 1, 0, /*
Select KEYMAP as the global keymap.
*/
       (keymap))
{
  /* This function can GC */
  keymap = get_keymap (keymap, 1, 1);
  Vcurrent_global_map = keymap;
  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, 1, 2, 0, /*
Select KEYMAP as the local keymap in BUFFER.
If KEYMAP is nil, that means no local keymap.
If BUFFER is nil, the current buffer is assumed.
*/
       (keymap, buffer))
{
  /* This function can GC */
  struct buffer *b = decode_buffer (buffer, 0);
  if (!NILP (keymap))
    keymap = get_keymap (keymap, 1, 1);

  b->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, 0, 1, 0, /*
Return BUFFER's local keymap, or nil if it has none.
If BUFFER is nil, the current buffer is assumed.
*/
       (buffer))
{
  struct buffer *b = decode_buffer (buffer, 0);
  return b->keymap;
}

DEFUN ("current-global-map", Fcurrent_global_map, 0, 0, 0, /*
Return the current global keymap.
*/
       ())
{
  return Vcurrent_global_map;
}


/************************************************************************/
/*                    Mapping over keymap elements                      */
/************************************************************************/

/* Since keymaps are arranged in a hierarchy, one keymap per bucky bit or
   prefix key, it's not entirely obvious what map-keymap should do, but
   what it does is: map over all keys in this map; then recursively map
   over all submaps of this map that are "bucky" submaps.  This means that,
   when mapping over a keymap, it appears that "x" and "C-x" are in the
   same map, although "C-x" is really in the "control" submap of this one.
   However, since we don't recursively descend the submaps that are bound
   to prefix keys (like C-x, C-h, etc) the caller will have to recurse on
   those explicitly, if that's what they want.

   So the end result of this is that the bucky keymaps (the ones indexed
   under the large integers returned from MAKE_MODIFIER_HASH_KEY()) are
   invisible from elisp.  They're just an implementation detail that code
   outside of this file doesn't need to know about.
 */

struct map_keymap_unsorted_closure
{
  void (*fn) (const Lisp_Key_Data *, Lisp_Object binding, void *arg);
  void *arg;
  int modifiers;
};

/* used by map_keymap() */
static int
map_keymap_unsorted_mapper (Lisp_Object keysym, Lisp_Object value,
                            void *map_keymap_unsorted_closure)
{
  /* This function can GC */
  struct map_keymap_unsorted_closure *closure =
    (struct map_keymap_unsorted_closure *) map_keymap_unsorted_closure;
  int modifiers = closure->modifiers;
  int mod_bit;
  mod_bit = MODIFIER_HASH_KEY_BITS (keysym);
  if (mod_bit != 0)
    {
      int omod = modifiers;
      closure->modifiers = (modifiers | mod_bit);
      value = get_keymap (value, 1, 0);
      elisp_maphash (map_keymap_unsorted_mapper,
		     XKEYMAP (value)->table,
		     map_keymap_unsorted_closure);
      closure->modifiers = omod;
    }
  else
    {
      Lisp_Key_Data key;
      key.keysym = keysym;
      key.modifiers = modifiers;
      ((*closure->fn) (&key, value, closure->arg));
    }
  return 0;
}


struct map_keymap_sorted_closure
{
  Lisp_Object *result_locative;
};

/* used by map_keymap_sorted() */
static int
map_keymap_sorted_mapper (Lisp_Object key, Lisp_Object value,
                          void *map_keymap_sorted_closure)
{
  struct map_keymap_sorted_closure *cl =
    (struct map_keymap_sorted_closure *) map_keymap_sorted_closure;
  Lisp_Object *list = cl->result_locative;
  *list = Fcons (Fcons (key, value), *list);
  return 0;
}


/* used by map_keymap_sorted(), describe_map_sort_predicate(),
   and keymap_submaps().
 */
static Boolint
map_keymap_sort_predicate (Lisp_Object UNUSED (pred), Lisp_Object UNUSED (key),
			   Lisp_Object obj1, Lisp_Object obj2)
{
  /* obj1 and obj2 are conses with keysyms in their cars.  Cdrs are ignored.
   */
  int bit1, bit2;
  int sym1_p = 0;
  int sym2_p = 0;
  extern Lisp_Object Qcharacter_of_keysym;

  obj1 = XCAR (obj1);
  obj2 = XCAR (obj2);

  if (EQ (obj1, obj2))
    return 0;
  bit1 = MODIFIER_HASH_KEY_BITS (obj1);
  bit2 = MODIFIER_HASH_KEY_BITS (obj2);

  /* If either is a symbol with a Qcharacter_of_keysym property, then sort
     it by that code instead of alphabetically.
     */
  if (! bit1 && SYMBOLP (obj1))
    {
      Lisp_Object code = Fget (obj1, Qcharacter_of_keysym, Qnil);
      if (CHAR_OR_CHAR_INTP (code))
	{
	  obj1 = code;
	  CHECK_CHAR_COERCE_INT (obj1);
	  sym1_p = 1;
	}
    }
  if (! bit2 && SYMBOLP (obj2))
    {
      Lisp_Object code = Fget (obj2, Qcharacter_of_keysym, Qnil);
      if (CHAR_OR_CHAR_INTP (code))
	{
	  obj2 = code;
	  CHECK_CHAR_COERCE_INT (obj2);
	  sym2_p = 1;
	}
    }

  /* all symbols (non-ASCIIs) come after characters (ASCIIs) */
  if (XTYPE (obj1) != XTYPE (obj2))
    return SYMBOLP (obj2);

  if (! bit1 && CHARP (obj1)) /* they're both ASCII */
    {
      int o1 = XCHAR (obj1);
      int o2 = XCHAR (obj2);
      if (o1 == o2 &&		/* If one started out as a symbol and the */
	  sym1_p != sym2_p)	/* other didn't, the symbol comes last. */
	return sym2_p;

      return o1 < o2;		/* else just compare them */
    }

  /* else they're both symbols.  If they're both buckys, then order them. */
  if (bit1 && bit2)
    return bit1 < bit2;

  /* if only one is a bucky, then it comes later */
  if (bit1 || bit2)
    return bit2;

  /* otherwise, string-sort them. */
  {
    Ibyte *s1 = XSTRING_DATA (XSYMBOL (obj1)->name);
    Ibyte *s2 = XSTRING_DATA (XSYMBOL (obj2)->name);
    return 0 > qxestrcmp (s1, s2);
  }
}


/* used by map_keymap() */
static void
map_keymap_sorted (Lisp_Object keymap_table,
                   int modifiers,
                   void (*function) (const Lisp_Key_Data *key,
                                     Lisp_Object binding,
                                     void *map_keymap_sorted_closure),
                   void *map_keymap_sorted_closure)
{
  /* This function can GC */
  struct gcpro gcpro1;
  Lisp_Object contents = Qnil;

  if (XFIXNUM (Fhash_table_count (keymap_table)) == 0)
    return;

  GCPRO1 (contents);

  {
    struct map_keymap_sorted_closure c1;
    c1.result_locative = &contents;
    elisp_maphash (map_keymap_sorted_mapper, keymap_table, &c1);
  }
  contents = list_sort (contents, map_keymap_sort_predicate, Qnil, Qidentity);
  for (; !NILP (contents); contents = XCDR (contents))
    {
      Lisp_Object keysym = XCAR (XCAR (contents));
      Lisp_Object binding = XCDR (XCAR (contents));
      int sub_bits = MODIFIER_HASH_KEY_BITS (keysym);
      if (sub_bits != 0)
	map_keymap_sorted (XKEYMAP (get_keymap (binding,
						1, 1))->table,
			   (modifiers | sub_bits),
			   function,
			   map_keymap_sorted_closure);
      else
	{
	  Lisp_Key_Data k;
	  k.keysym = keysym;
	  k.modifiers = modifiers;
	  ((*function) (&k, binding, map_keymap_sorted_closure));
	}
    }
  UNGCPRO;
}


/* used by Fmap_keymap() */
static void
map_keymap_mapper (const Lisp_Key_Data *key,
                   Lisp_Object binding,
                   void *function)
{
  /* This function can GC */
  Lisp_Object fn;
  fn = GET_LISP_FROM_VOID (function);

  /* Don't expose our remapping here. */
  if (EQ (KEY_DATA_KEYSYM (key), Qxemacs_command_remapping))
    {
      return;
    }

  call2 (fn, make_key_description (key, 1), binding);
}


static void
map_keymap (Lisp_Object keymap_table, int sort_first,
            void (*function) (const Lisp_Key_Data *key,
                              Lisp_Object binding,
                              void *fn_arg),
            void *fn_arg)
{
  /* This function can GC */
  if (sort_first)
    map_keymap_sorted (keymap_table, 0, function, fn_arg);
  else
    {
      struct map_keymap_unsorted_closure map_keymap_unsorted_closure;
      map_keymap_unsorted_closure.fn = function;
      map_keymap_unsorted_closure.arg = fn_arg;
      map_keymap_unsorted_closure.modifiers = 0;
      elisp_maphash (map_keymap_unsorted_mapper, keymap_table,
		     &map_keymap_unsorted_closure);
    }
}

DEFUN ("map-keymap", Fmap_keymap, 2, 3, 0, /*
Apply FUNCTION to each element of KEYMAP.
FUNCTION will be called with two arguments: a key-description list, and
the binding.  The order in which the elements of the keymap are passed to
the function is unspecified.  If the function inserts new elements into
the keymap, it may or may not be called with them later.  No element of
the keymap will ever be passed to the function more than once.

The function will not be called on elements of this keymap's parents
\(see the function `keymap-parents') or upon keymaps which are contained
within this keymap (multi-character definitions).
It will be called on "meta" characters since they are not really
two-character sequences.

If the optional third argument SORT-FIRST is non-nil, then the elements of
the keymap will be passed to the mapper function in a canonical order.
Otherwise, they will be passed in hash (that is, random) order, which is
faster.
*/
     (function, keymap, sort_first))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;

 /* tolerate obviously transposed args */
  if (!NILP (Fkeymapp (function)))
    {
      Lisp_Object tmp = function;
      function = keymap;
      keymap = tmp;
    }
  GCPRO2 (function, keymap);
  keymap = get_keymap (keymap, 1, 1);
  map_keymap (XKEYMAP (keymap)->table, !NILP (sort_first),
	      map_keymap_mapper, STORE_LISP_IN_VOID (function));
  UNGCPRO;
  return Qnil;
}



/************************************************************************/
/*                          Accessible keymaps                          */
/************************************************************************/

struct accessible_keymaps_closure
  {
    Lisp_Object tail;
  };


static void
accessible_keymaps_mapper_1 (Lisp_Object keysym, Lisp_Object contents,
                             int modifiers,
                             struct accessible_keymaps_closure *closure)
{
  /* This function can GC */
  int subbits = MODIFIER_HASH_KEY_BITS (keysym);

  if (subbits != 0)
    {
      Lisp_Object submaps;

      contents = get_keymap (contents, 1, 1);
      submaps = keymap_submaps (contents);
      for (; !NILP (submaps); submaps = XCDR (submaps))
	{
	  accessible_keymaps_mapper_1 (XCAR (XCAR (submaps)),
                                       XCDR (XCAR (submaps)),
                                       (subbits | modifiers),
                                       closure);
	}
    }
  else
    {
      Lisp_Object thisseq = Fcar (Fcar (closure->tail));
      Lisp_Object cmd = get_keyelt (contents, 1);
      Lisp_Object vec;
      int j;
      int len;
      Lisp_Key_Data key;
      key.keysym = keysym;
      key.modifiers = modifiers;

      assert (!NILP (cmd));
      cmd = get_keymap (cmd, 0, 1);
      assert (KEYMAPP (cmd));

      vec = make_vector (XVECTOR_LENGTH (thisseq) + 1, Qnil);
      len = XVECTOR_LENGTH (thisseq);
      for (j = 0; j < len; j++)
	XVECTOR_DATA (vec) [j] = XVECTOR_DATA (thisseq) [j];
      XVECTOR_DATA (vec) [j] = make_key_description (&key, 1);

      nconc2 (closure->tail, list1 (Fcons (vec, cmd)));
    }
}


static Lisp_Object
accessible_keymaps_keymap_mapper (Lisp_Object thismap, void *arg)
{
  /* This function can GC */
  struct accessible_keymaps_closure *closure =
    (struct accessible_keymaps_closure *) arg;
  Lisp_Object submaps = keymap_submaps (thismap);

  for (; !NILP (submaps); submaps = XCDR (submaps))
    {
      accessible_keymaps_mapper_1 (XCAR (XCAR (submaps)),
				   XCDR (XCAR (submaps)),
				   0,
				   closure);
    }
  return Qnil;
}


DEFUN ("accessible-keymaps", Faccessible_keymaps, 1, 2, 0, /*
Find all keymaps accessible via prefix characters from KEYMAP.
Returns a list of elements of the form (KEYS . MAP), where the sequence
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
so that the KEYS increase in length.  The first element is ([] . KEYMAP).
An optional argument PREFIX, if non-nil, should be a key sequence;
then the value includes only maps for prefixes that start with PREFIX.
*/
       (keymap, prefix))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object accessible_keymaps = Qnil;
  struct accessible_keymaps_closure c;
  c.tail = Qnil;
  GCPRO4 (accessible_keymaps, c.tail, prefix, keymap);

  keymap = get_keymap (keymap, 1, 1);

 retry:
  if (NILP (prefix))
    {
      prefix = make_vector (0, Qnil);
    }
  else if (VECTORP (prefix) || STRINGP (prefix))
    {
      int len = XFIXNUM (Flength (prefix));
      Lisp_Object def;
      Lisp_Object p;
      int iii;
      struct gcpro ngcpro1;

      if (len == 0)
	{
	  prefix = Qnil;
	  goto retry;
	}

      def = Flookup_key (keymap, prefix, Qnil);
      def = get_keymap (def, 0, 1);
      if (!KEYMAPP (def))
	goto RETURN;

      keymap = def;
      p = make_vector (len, Qnil);
      NGCPRO1 (p);
      for (iii = 0; iii < len; iii++)
	{
	  Lisp_Key_Data key;
	  define_key_parser (Faref (prefix, make_fixnum (iii)), &key);
	  XVECTOR_DATA (p)[iii] = make_key_description (&key, 1);
	}
      NUNGCPRO;
      prefix = p;
    }
  else
    {
      prefix = wrong_type_argument (Qarrayp, prefix);
      goto retry;
    }

  accessible_keymaps = list1 (Fcons (prefix, keymap));

  /* For each map in the list maps, look at any other maps it points
     to and stick them at the end if they are not already in the list */

  for (c.tail = accessible_keymaps;
       !NILP (c.tail);
       c.tail = XCDR (c.tail))
    {
      Lisp_Object thismap = Fcdr (Fcar (c.tail));
      CHECK_KEYMAP (thismap);
      traverse_keymaps (thismap, Qnil,
                        accessible_keymaps_keymap_mapper, &c);
    }
 RETURN:
  UNGCPRO;
  return accessible_keymaps;
}



/************************************************************************/
/*              Pretty descriptions of key sequences                    */
/************************************************************************/

DEFUN ("key-description", Fkey_description, 1, 1, 0, /*
Return a pretty description of key-sequence KEYS.
Control characters turn into "C-foo" sequences, meta into "M-foo",
spaces are put between sequence elements, etc...
*/
       (keys))
{
  if (CHAR_OR_CHAR_INTP (keys) || CONSP (keys) || SYMBOLP (keys)
      || EVENTP (keys))
    {
      return Fsingle_key_description (keys);
    }
  else if (VECTORP (keys) ||
	   STRINGP (keys))
    {
      Lisp_Object string = Qnil;
      /* Lisp_Object sep = Qnil; */
      int size = XFIXNUM (Flength (keys));
      int i;

      for (i = 0; i < size; i++)
	{
	  Lisp_Object s2 = Fsingle_key_description
	    (STRINGP (keys)
	     ? make_char (string_ichar (keys, i))
	     : XVECTOR_DATA (keys)[i]);

	  if (i == 0)
	    string = s2;
	  else
	    {
	      /* if (NILP (sep)) Lisp_Object sep = build_ascstring (" ") */;
	      string = concat2 (string, concat2 (Vsingle_space_string, s2));
	    }
	}
      return string;
    }
  return Fkey_description (wrong_type_argument (Qsequencep, keys));
}

DEFUN ("single-key-description", Fsingle_key_description, 1, 1, 0, /*
Return a pretty description of command character KEY.
Control characters turn into C-whatever, etc.
This differs from `text-char-description' in that it returns a description
of a key read from the user rather than a character from a buffer.
*/
       (key))
{
  if (SYMBOLP (key))
    key = Fcons (key, Qnil); /* sleaze sleaze */

  if (EVENTP (key) || CHAR_OR_CHAR_INTP (key))
    {
      DECLARE_EISTRING_MALLOC (buf);
      Lisp_Object str;
      
      if (!EVENTP (key))
	{
	  Lisp_Object event = Fmake_event (Qnil, Qnil);
	  CHECK_CHAR_COERCE_INT (key);
	  character_to_event (XCHAR (key), XEVENT (event),
			      XCONSOLE (Vselected_console),
			      high_bit_is_meta, 1);
	  format_event_object (buf, event, 1);
	  Fdeallocate_event (event);
	}
      else
	format_event_object (buf, key, 1);
      str = eimake_string (buf);
      eifree (buf);
      return str;
    }

  if (CONSP (key))
    {
      DECLARE_EISTRING (bufp);

      Lisp_Object rest;
      LIST_LOOP (rest, key)
	{
	  Lisp_Object keysym = XCAR (rest);
	  if (EQ (keysym, Qcontrol))    eicat_ascii (bufp, "C-");
	  else if (EQ (keysym, Qctrl))  eicat_ascii (bufp, "C-");
	  else if (EQ (keysym, Qmeta))  eicat_ascii (bufp, "M-");
	  else if (EQ (keysym, Qsuper)) eicat_ascii (bufp, "S-");
	  else if (EQ (keysym, Qhyper)) eicat_ascii (bufp, "H-");
	  else if (EQ (keysym, Qalt))	eicat_ascii (bufp, "A-");
	  else if (EQ (keysym, Qshift)) eicat_ascii (bufp, "Sh-");
	  else if (CHAR_OR_CHAR_INTP (keysym))
	    eicat_ch (bufp, XCHAR_OR_CHAR_INT (keysym));
	  else
	    {
	      CHECK_SYMBOL (keysym);
#if 0                           /* This is bogus */
	      if (EQ (keysym, QKlinefeed))	 eicat_ascii (bufp, "LFD");
	      else if (EQ (keysym, QKtab))	 eicat_ascii (bufp, "TAB");
	      else if (EQ (keysym, QKreturn))	 eicat_ascii (bufp, "RET");
	      else if (EQ (keysym, QKescape))	 eicat_ascii (bufp, "ESC");
	      else if (EQ (keysym, QKdelete))	 eicat_ascii (bufp, "DEL");
	      else if (EQ (keysym, QKspace))	 eicat_ascii (bufp, "SPC");
	      else if (EQ (keysym, QKbackspace)) eicat_ascii (bufp, "BS");
	      else
#endif
		eicat_lstr (bufp, XSYMBOL (keysym)->name);
	      if (!NILP (XCDR (rest)))
		invalid_argument ("Invalid key description", key);
	    }
	}
      return eimake_string (bufp);
    }
  return Fsingle_key_description
    (wrong_type_argument (intern ("char-or-event-p"), key));
}

DEFUN ("text-char-description", Ftext_char_description, 1, 1, 0, /*
Return a pretty description of file-character CHR.
Unprintable characters turn into "^char" or \\NNN, depending on the value
of the `ctl-arrow' variable.
This differs from `single-key-description' in that it returns a description
of a character from a buffer rather than a key read from the user.
*/
       (chr))
{
  Ibyte buf[200];
  Ibyte *p;
  Ichar c;
  Lisp_Object ctl_arrow = current_buffer->ctl_arrow;
  int ctl_p = !NILP (ctl_arrow);
  Ichar printable_min = (CHAR_OR_CHAR_INTP (ctl_arrow)
			  ? XCHAR_OR_CHAR_INT (ctl_arrow)
			  : ((EQ (ctl_arrow, Qt) || NILP (ctl_arrow))
			     ? 256 : 160));

  if (EVENTP (chr))
    {
      Lisp_Object ch = Fevent_to_character (chr, Qnil, Qnil, Qnil);
      if (NILP (ch))
	return
	  signal_continuable_error
	    (Qinvalid_argument,
	     "key has no character equivalent (that we know of)",
	     Fcopy_event (chr, Qnil));
      chr = ch;
    }

  CHECK_CHAR_COERCE_INT (chr);

  c = XCHAR (chr);
  p = buf;

  if (c >= printable_min)
    {
      p += set_itext_ichar (p, c);
    }
  else if (c < 040 && ctl_p)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else if (c >= 0200 || c < 040)
    {
      *p++ = '\\';
#ifdef MULE
      /* !!#### This syntax is not readable.  It will
	 be interpreted as a 3-digit octal number rather
	 than a 7-digit octal number. */
      if (c >= 0400)
	{
	  *p++ = '0' + ((c & 07000000) >> 18);
	  *p++ = '0' + ((c & 0700000) >> 15);
	  *p++ = '0' + ((c & 070000) >> 12);
	  *p++ = '0' + ((c & 07000) >> 9);
	}
#endif
      *p++ = '0' + ((c & 0700) >> 6);
      *p++ = '0' + ((c & 0070) >> 3);
      *p++ = '0' + ((c & 0007));
    }
  else
    {
      p += set_itext_ichar (p, c);
    }

  *p = 0;
  return build_istring (buf);
}


/************************************************************************/
/*              where-is (mapping bindings to keys)                     */
/************************************************************************/

static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object *maps, int nmaps,
                   Lisp_Object firstonly, Eistring *target_buffer);

DEFUN ("where-is-internal", Fwhere_is_internal, 1, 5, 0, /*
Return list of keys that invoke DEFINITION in KEYMAPS.
KEYMAPS can be either a keymap (meaning search in that keymap and the
current global keymap) or a list of keymaps (meaning search in exactly
those keymaps and no others).

If optional 3rd arg FIRSTONLY is non-nil, return a vector representing
 the first key sequence found, rather than a list of all possible key
 sequences.

Optional 4th argument NOINDIRECT is ignored.  (GNU Emacs uses it to allow
searching for an indirect keymap by inhibiting following of indirections to
keymaps or slots, but XEmacs doesn't need it because keymaps are a type.)

If optional 5th argument EVENT-OR-KEYS is non-nil and KEYMAPS is nil,
search in the currently applicable maps for EVENT-OR-KEYS (this is
equivalent to specifying `(current-keymaps EVENT-OR-KEYS)' as the
argument to KEYMAPS).
*/
       (definition, keymaps, firstonly, UNUSED (noindirect), event_or_keys))
{
  /* This function can GC */
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps;

  /* Get keymaps as an array */
  if (NILP (keymaps))
    {
      nmaps = get_relevant_keymaps (event_or_keys, Qnil, countof (maps),
                                    gubbish);
      if (nmaps > countof (maps))
	{
	  gubbish = alloca_array (Lisp_Object, nmaps);
	  nmaps = get_relevant_keymaps (event_or_keys, Qnil, nmaps, gubbish);
	}
    }
  else if (CONSP (keymaps))
    {
      Lisp_Object rest;
      int i;

      nmaps = XFIXNUM (Flength (keymaps));
      if (nmaps > countof (maps))
	{
	  gubbish = alloca_array (Lisp_Object, nmaps);
	}
      for (rest = keymaps, i = 0; !NILP (rest);
	   rest = XCDR (keymaps), i++)
	{
	  gubbish[i] = get_keymap (XCAR (keymaps), 1, 1);
	}
    }
  else
    {
      nmaps = 1;
      gubbish[0] = get_keymap (keymaps, 1, 1);
      if (!EQ (gubbish[0], Vcurrent_global_map))
	{
	  gubbish[1] = Vcurrent_global_map;
	  nmaps++;
	}
    }

  if (!NILP (command_remapping (definition, nmaps, gubbish)))
    {
      return Qnil;
    }

  return where_is_internal (definition, gubbish, nmaps, firstonly, 0);
}

/* This function is like
   (key-description (where-is-internal definition nil t))
   except that it writes its output into a (char *) buffer that you
   provide; it doesn't cons (or allocate memory) at all, so it's
   very fast.  This is used by menubar.c.
 */
void
where_is_to_char (Lisp_Object definition, Eistring *buffer)
{
  /* This function can GC */
  Lisp_Object maps[100];
  Lisp_Object *gubbish = maps;
  int nmaps;

  /* Get keymaps as an array */
  nmaps = get_relevant_keymaps (Qnil, Qnil, countof (maps), gubbish);
  if (nmaps > countof (maps))
    {
      gubbish = alloca_array (Lisp_Object, nmaps);
      nmaps = get_relevant_keymaps (Qnil, Qnil, nmaps, gubbish);
    }

  where_is_internal (definition, maps, nmaps, Qt, buffer);
}


static Lisp_Object
raw_keys_to_keys (Lisp_Key_Data *keys, int count)
{
  Lisp_Object result = make_vector (count, Qnil);
  while (count--)
    XVECTOR_DATA (result) [count] = make_key_description (&(keys[count]), 1);
  return result;
}


static void
format_raw_keys (Lisp_Key_Data *keys, int count, Eistring *buf)
{
  int i;
  Lisp_Object event = Fmake_event (Qnil, Qnil);
  XSET_EVENT_TYPE (event, key_press_event);
  XSET_EVENT_CHANNEL (event, Vselected_console);
  for (i = 0; i < count; i++)
    {
      XSET_EVENT_KEY_KEYSYM (event, keys[i].keysym);
      XSET_EVENT_KEY_MODIFIERS (event, KEY_DATA_MODIFIERS (&keys[i]));
      format_event_object (buf, event, 1);
      if (i < count - 1)
	eicat_ascii (buf, " ");
    }
  Fdeallocate_event (event);
}


/* definition is the thing to look for.
   map is a keymap.
   shadow is an array of shadow_count keymaps; if there is a different
   binding in any of the keymaps of a key that we are considering
   returning, then we reconsider.
   firstonly means give up after finding the first match;
   keys_so_far and modifiers_so_far describe which map we're looking in;
   If we're in the "meta" submap of the map that "C-x 4" is bound to,
   then keys_so_far will be {(control x), \4}, and modifiers_so_far
   will be XEMACS_MOD_META.  That is, keys_so_far is the chain of keys that we
   have followed, and modifiers_so_far_so_far is the bits (partial keys)
   beyond that.

   (keys_so_far is a global buffer and the keys_count arg says how much
   of it we're currently interested in.)

   If target_buffer is provided, then we write a key-description into it,
   to avoid consing a string.  This only works with firstonly on.
   */

struct where_is_closure
  {
    Lisp_Object definition;
    Lisp_Object *shadow;
    int shadow_count;
    int firstonly;
    int keys_count;
    int modifiers_so_far;
    Eistring *target_buffer;
    Lisp_Key_Data *keys_so_far;
    int keys_so_far_total_size;
    int keys_so_far_malloced;
  };

static Lisp_Object where_is_recursive_mapper (Lisp_Object map, void *arg);

static Lisp_Object
where_is_recursive_mapper (Lisp_Object map, void *arg)
{
  /* This function can GC */
  struct where_is_closure *c = (struct where_is_closure *) arg;
  Lisp_Object definition = c->definition;
  const int firstonly = c->firstonly;
  const int keys_count = c->keys_count;
  const int modifiers_so_far = c->modifiers_so_far;
  Eistring *target_buffer = c->target_buffer;
  Lisp_Object keys = Fgethash (definition,
                               XKEYMAP (map)->inverse_table,
                               Qnil);
  Lisp_Object submaps;
  Lisp_Object result = Qnil;

  if (!NILP (keys))
    {
      /* One or more keys in this map match the definition we're looking for.
	 Verify that these bindings aren't shadowed by other bindings
	 in the shadow maps.  Either nil or number as value from
	 raw_lookup_key() means undefined.  */
      Lisp_Key_Data *so_far = c->keys_so_far;

      for (;;) /* loop over all keys that match */
	{
	  Lisp_Object k = CONSP (keys) ? XCAR (keys) : keys;
	  int i;

	  so_far [keys_count].keysym = k;
	  SET_KEY_DATA_MODIFIERS (&so_far [keys_count], modifiers_so_far);

	  /* now loop over all shadow maps */
	  for (i = 0; i < c->shadow_count; i++)
	    {
	      Lisp_Object shadowed = raw_lookup_key (c->shadow[i],
						     so_far,
						     keys_count + 1,
						     0, 1);

	      if (NILP (shadowed) || CHARP (shadowed) ||
		  EQ (shadowed, definition))
		continue; /* we passed this test; it's not shadowed here. */
	      else
		/* ignore this key binding, since it actually has a
		   different binding in a shadowing map */
		goto c_doesnt_have_proper_loop_exit_statements;
	    }

	  /* OK, the key is for real */
	  if (target_buffer)
	    {
	      assert (firstonly);
	      format_raw_keys (so_far, keys_count + 1, target_buffer);
	      return make_fixnum (1);
	    }
	  else if (firstonly)
	    return raw_keys_to_keys (so_far, keys_count + 1);
	  else
	    result = Fcons (raw_keys_to_keys (so_far, keys_count + 1),
			    result);

	c_doesnt_have_proper_loop_exit_statements:
	  /* now on to the next matching key ... */
	  if (!CONSP (keys)) break;
	  keys = XCDR (keys);
	}
    }

  /* Now search the sub-keymaps of this map.
     If we're in "firstonly" mode and have already found one, this
     point is not reached.  If we get one from lower down, either
     return it immediately (in firstonly mode) or tack it onto the
     end of the ones we've gotten so far.
     */
  for (submaps = keymap_submaps (map);
       !NILP (submaps);
       submaps = XCDR (submaps))
    {
      Lisp_Object key    = XCAR (XCAR (submaps));
      Lisp_Object submap = XCDR (XCAR (submaps));
      int lower_modifiers;
      int lower_keys_count = keys_count;
      int bucky;

      submap = get_keymap (submap, 0, 0);

      if (EQ (submap, map))
	/* Arrgh!  Some loser has introduced a loop... */
	continue;

      /* If this is not a keymap, then that's probably because someone
	 did an `fset' of a symbol that used to point to a map such that
	 it no longer does.  Sigh.  Ignore this, and invalidate the cache
	 so that it doesn't happen to us next time too.
	 */
      if (NILP (submap))
	{
	  XKEYMAP (map)->sub_maps_cache = Qt;
	  continue;
	}

      /* Don't expose the command remapping to #'where-is-internal */
      if (EQ (key, Qxemacs_command_remapping))
        {
          continue;
        }

      /* If the map is a "bucky" map, then add a bit to the
	 modifiers_so_far list.
	 Otherwise, add a new raw_key onto the end of keys_so_far.
	 */
      bucky = MODIFIER_HASH_KEY_BITS (key);
      if (bucky != 0)
	lower_modifiers = (modifiers_so_far | bucky);
      else
	{
	  Lisp_Key_Data *so_far = c->keys_so_far;
	  lower_modifiers = 0;
	  so_far [lower_keys_count].keysym = key;
	  SET_KEY_DATA_MODIFIERS (&so_far [lower_keys_count], modifiers_so_far);
	  lower_keys_count++;
	}

      if (lower_keys_count >= c->keys_so_far_total_size)
	{
	  int size = lower_keys_count + 50;
	  if (! c->keys_so_far_malloced)
	    {
	      Lisp_Key_Data *new_ = xnew_array (Lisp_Key_Data, size);
	      memcpy ((void *)new_, (const void *)c->keys_so_far,
		      c->keys_so_far_total_size * sizeof (Lisp_Key_Data));
	      xfree (c->keys_so_far);
	      c->keys_so_far = new_;
	    }
	  else
	    XREALLOC_ARRAY (c->keys_so_far, Lisp_Key_Data, size);

	  c->keys_so_far_total_size = size;
	  c->keys_so_far_malloced = 1;
	}

      {
	Lisp_Object lower;

	c->keys_count = lower_keys_count;
	c->modifiers_so_far = lower_modifiers;

	lower = traverse_keymaps (submap, Qnil, where_is_recursive_mapper, c);

	c->keys_count = keys_count;
	c->modifiers_so_far = modifiers_so_far;

	if (!firstonly)
	  result = nconc2 (lower, result);
	else if (!NILP (lower))
	  return lower;
      }
    }
  return result;
}


static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object *maps, int nmaps,
                   Lisp_Object firstonly, Eistring *target_buffer)
{
  /* This function can GC */
  Lisp_Object result = Qnil;
  int i;
  Lisp_Key_Data raw[20];
  struct where_is_closure c;

  c.definition = definition;
  c.shadow = maps;
  c.firstonly = !NILP (firstonly);
  c.target_buffer = target_buffer;
  c.keys_so_far = raw;
  c.keys_so_far_total_size = countof (raw);
  c.keys_so_far_malloced = 0;

  /* Loop over each of the maps, accumulating the keys found.
     For each map searched, all previous maps shadow this one
     so that bogus keys aren't listed. */
  for (i = 0; i < nmaps; i++)
    {
      Lisp_Object this_result;
      c.shadow_count = i;
      /* Reset the things set in each iteration */
      c.keys_count = 0;
      c.modifiers_so_far = 0;

      this_result = traverse_keymaps (maps[i], Qnil, where_is_recursive_mapper,
				      &c);
      if (!NILP (firstonly))
	{
	  result = this_result;
	  if (!NILP (result))
	    break;
	}
      else
	result = nconc2 (this_result, result);
    }

  if (NILP (firstonly))
    result = Fnreverse (result);

  if (c.keys_so_far_malloced)
    xfree (c.keys_so_far);
  return result;
}


/************************************************************************/
/*                         Describing keymaps                           */
/************************************************************************/

DEFUN ("describe-bindings-internal", Fdescribe_bindings_internal, 1, 5, 0, /*
Insert a list of all defined keys and their definitions in MAP.
Optional second argument ALL says whether to include even "uninteresting"
definitions (ie symbols with a non-nil `suppress-keymap' property.
Third argument SHADOW is a list of keymaps whose bindings shadow those
of map; if a binding is present in any shadowing map, it is not printed.
Fourth argument PREFIX, if non-nil, should be a key sequence;
only bindings which start with that key sequence will be printed.
Fifth argument MOUSE-ONLY-P says to only print bindings for mouse clicks.
*/
       (map, all, shadow, prefix, mouse_only_p))
{
  /* This function can GC */

  /* #### At some point, this function should be changed to accept a
     BUFFER argument.  Currently, the BUFFER argument to
     describe_map_tree is being used only internally.  */
  describe_map_tree (map, NILP (all), shadow, prefix,
		     !NILP (mouse_only_p), Fcurrent_buffer ());
  return Qnil;
}


/* Insert a description of the key bindings in STARTMAP,
    followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
    (such as `undefined').
   If SHADOW is non-nil, it is a list of other maps;
    don't mention keys which would be shadowed by any of them
   If PREFIX is non-nil, only list bindings which start with those keys.
 */

void
describe_map_tree (Lisp_Object startmap, int partial, Lisp_Object shadow,
                   Lisp_Object prefix, int mice_only_p, Lisp_Object buffer)
{
  /* This function can GC */
  Lisp_Object maps = Qnil;
  struct gcpro gcpro1, gcpro2;  /* get_keymap may autoload */
  GCPRO2 (maps, shadow);

  maps = Faccessible_keymaps (startmap, prefix);

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      Lisp_Object sub_shadow = Qnil;
      Lisp_Object elt = Fcar (maps);
      Lisp_Object tail;
      int no_prefix = (VECTORP (Fcar (elt))
                       && XFIXNUM (Flength (Fcar (elt))) == 0);
      struct gcpro ngcpro1, ngcpro2, ngcpro3;
      NGCPRO3 (sub_shadow, elt, tail);

      for (tail = shadow; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object shmap = XCAR (tail);

	  /* If the sequence by which we reach this keymap is zero-length,
	     then the shadow maps for this keymap are just SHADOW.  */
	  if (no_prefix)
	    ;
	  /* If the sequence by which we reach this keymap actually has
	     some elements, then the sequence's definition in SHADOW is
	     what we should use.  */
	  else
	    {
	      shmap = Flookup_key (shmap, Fcar (elt), Qt);
	      if (CHARP (shmap))
		shmap = Qnil;
	    }

	  if (!NILP (shmap))
	    {
	      Lisp_Object shm = get_keymap (shmap, 0, 1);
	      /* If shmap is not nil and not a keymap, it completely
		 shadows this map, so don't describe this map at all.  */
	      if (!KEYMAPP (shm))
		goto SKIP;
	      sub_shadow = Fcons (shm, sub_shadow);
	    }
	}

      {
        /* Describe the contents of map MAP, assuming that this map
           itself is reached by the sequence of prefix keys KEYS (a vector).
           PARTIAL and SHADOW are as in `describe_map_tree'.  */
        Lisp_Object keysdesc
          = ((!no_prefix)
             ? concat2 (Fkey_description (Fcar (elt)), Vsingle_space_string)
             : Qnil);
        describe_map (Fcdr (elt), keysdesc,
                      describe_command,
                      partial,
                      sub_shadow,
                      mice_only_p,
		      buffer);
      }
    SKIP:
      NUNGCPRO;
    }
  UNGCPRO;
}


static void
describe_command (Lisp_Object definition, Lisp_Object buffer)
{
  /* This function can GC */
  int keymapp = !NILP (Fkeymapp (definition));
  struct gcpro gcpro1;
  GCPRO1 (definition);

  Findent_to (make_fixnum (16), make_fixnum (3), buffer);
  if (keymapp)
    buffer_insert_ascstring (XBUFFER (buffer), "<< ");

  if (SYMBOLP (definition))
    {
      buffer_insert1 (XBUFFER (buffer), Fsymbol_name (definition));
    }
  else if (STRINGP (definition) || VECTORP (definition))
    {
      buffer_insert_ascstring (XBUFFER (buffer), "Kbd Macro: ");
      buffer_insert1 (XBUFFER (buffer), Fkey_description (definition));
    }
  else if (COMPILED_FUNCTIONP (definition))
    buffer_insert_ascstring (XBUFFER (buffer), "Anonymous Compiled Function");
  else if (CONSP (definition) && EQ (XCAR (definition), Qlambda))
    buffer_insert_ascstring (XBUFFER (buffer), "Anonymous Lambda");
  else if (KEYMAPP (definition))
    {
      Lisp_Object name = XKEYMAP (definition)->name;
      if (STRINGP (name) || (SYMBOLP (name) && !NILP (name)))
	{
	  buffer_insert_ascstring (XBUFFER (buffer), "Prefix command ");
	  if (SYMBOLP (name)
	      && EQ (find_symbol_value (name), definition))
	    buffer_insert1 (XBUFFER (buffer), Fsymbol_name (name));
	  else
	    {
	      buffer_insert1 (XBUFFER (buffer), Fprin1_to_string (name, Qnil));
	    }
	}
      else
	buffer_insert_ascstring (XBUFFER (buffer), "Prefix Command");
    }
  else
    buffer_insert_ascstring (XBUFFER (buffer), "??");

  if (keymapp)
    buffer_insert_ascstring (XBUFFER (buffer), " >>");
  buffer_insert_ascstring (XBUFFER (buffer), "\n");
  UNGCPRO;
}

struct describe_map_closure
  {
    Lisp_Object *list;	 /* pointer to the list to update */
    Lisp_Object partial; /* whether to ignore suppressed commands */
    Lisp_Object shadow;	 /* list of maps shadowing this one */
    Lisp_Object self;	 /* this map */
    Lisp_Object self_root; /* this map, or some map that has this map as
                              a parent.  this is the base of the tree */
    int mice_only_p;	 /* whether we are to display only button bindings */
  };

struct describe_map_shadow_closure
  {
    const Lisp_Key_Data *raw_key;
    Lisp_Object self;
  };

static Lisp_Object
describe_map_mapper_shadow_search (Lisp_Object map, void *arg)
{
  struct describe_map_shadow_closure *c =
    (struct describe_map_shadow_closure *) arg;

  if (EQ (map, c->self))
    return Qzero;		/* Not shadowed; terminate search */

  return !NILP (keymap_lookup_directly (map,
					KEY_DATA_KEYSYM (c->raw_key),
					KEY_DATA_MODIFIERS (c->raw_key)))
    ? Qt : Qnil;
}


static Lisp_Object
keymap_lookup_inherited_mapper (Lisp_Object km, void *arg)
{
  Lisp_Key_Data *k = (Lisp_Key_Data *) arg;
  return keymap_lookup_directly (km, KEY_DATA_KEYSYM (k), KEY_DATA_MODIFIERS (k));
}


static void
describe_map_mapper (const Lisp_Key_Data *key,
                     Lisp_Object binding,
		     void *describe_map_closure)
{
  /* This function can GC */
  struct describe_map_closure *closure =
    (struct describe_map_closure *) describe_map_closure;
  Lisp_Object keysym = KEY_DATA_KEYSYM (key);
  int modifiers = KEY_DATA_MODIFIERS (key);

  /* Don't mention suppressed commands.  */
  if (SYMBOLP (binding)
      && !NILP (closure->partial)
      && !NILP (Fget (binding, closure->partial, Qnil)))
    return;

  /* If we're only supposed to display mouse bindings and this isn't one,
     then bug out. */
  if (closure->mice_only_p &&
      (! (
#define INCLUDE_BUTTON_ZERO
#define FROB(num) EQ (keysym, Qbutton##num) || \
                  EQ (keysym, Qbutton##num##up) ||
#include "keymap-buttons.h"
	  0)))
    return;

  /* If this command in this map is shadowed by some other map, ignore it. */
  {
    Lisp_Object tail;

    for (tail = closure->shadow; CONSP (tail); tail = XCDR (tail))
      {
	QUIT;
	if (!NILP (traverse_keymaps (XCAR (tail), Qnil,
				     keymap_lookup_inherited_mapper,
				     /* Cast to discard `const' */
				     (void *)key)))
	  return;
      }
  }

  /* If this key is in some map of which this map is a parent, then ignore
     it (in that case, it has been shadowed).
     */
  {
    Lisp_Object sh;
    struct describe_map_shadow_closure c;
    c.raw_key = key;
    c.self = closure->self;

    sh = traverse_keymaps (closure->self_root, Qnil,
                           describe_map_mapper_shadow_search, &c);
    if (!NILP (sh) && !ZEROP (sh))
      return;
  }

  /* Otherwise add it to the list to be sorted. */
  *(closure->list) = Fcons (Fcons (Fcons (keysym, make_fixnum (modifiers)),
                                   binding),
			    *(closure->list));
}

static Boolint
describe_map_sort_predicate (Lisp_Object pred, Lisp_Object key_func,
			     Lisp_Object obj1, Lisp_Object obj2)
			     
{
  /* obj1 and obj2 are conses of the form
     ( ( <keysym> . <modifiers> ) . <binding> )
     keysym and modifiers are used, binding is ignored.
   */
  int bit1, bit2;
  obj1 = XCAR (obj1);
  obj2 = XCAR (obj2);
  bit1 = XFIXNUM (XCDR (obj1));
  bit2 = XFIXNUM (XCDR (obj2));
  if (bit1 != bit2)
    return bit1 < bit2;
  else
    return map_keymap_sort_predicate (pred, key_func, obj1, obj2);
}

/* Elide 2 or more consecutive numeric keysyms bound to the same thing,
   or 2 or more symbolic keysyms that are bound to the same thing and
   have consecutive character-set-properties.
 */
static int
elide_next_two_p (Lisp_Object list)
{
  Lisp_Object s1, s2;
  extern Lisp_Object Qcharacter_of_keysym;

  if (NILP (XCDR (list)))
    return 0;

  /* next two bindings differ */
  if (!EQ (XCDR (XCAR (list)),
	   XCDR (XCAR (XCDR (list)))))
    return 0;

  /* next two modifier-sets differ */
  if (!EQ (XCDR (XCAR (XCAR (list))),
	   XCDR (XCAR (XCAR (XCDR (list))))))
    return 0;

  s1 = XCAR (XCAR (XCAR (list)));
  s2 = XCAR (XCAR (XCAR (XCDR (list))));

  if (SYMBOLP (s1))
    {
      Lisp_Object code = Fget (s1, Qcharacter_of_keysym, Qnil);
      if (CHAR_OR_CHAR_INTP (code))
	{
	  s1 = code;
	  CHECK_CHAR_COERCE_INT (s1);
	}
      else return 0;
    }
  if (SYMBOLP (s2))
    {
      Lisp_Object code = Fget (s2, Qcharacter_of_keysym, Qnil);
      if (CHAR_OR_CHAR_INTP (code))
	{
	  s2 = code;
	  CHECK_CHAR_COERCE_INT (s2);
	}
      else return 0;
    }

  return (XCHAR (s1)     == XCHAR (s2) ||
	  XCHAR (s1) + 1 == XCHAR (s2));
}


static Lisp_Object
describe_map_parent_mapper (Lisp_Object keymap, void *arg)
{
  /* This function can GC */
  struct describe_map_closure *describe_map_closure =
    (struct describe_map_closure *) arg;
  describe_map_closure->self = keymap;
  map_keymap (XKEYMAP (keymap)->table,
              0, /* don't sort: we'll do it later */
              describe_map_mapper, describe_map_closure);
  return Qnil;
}


/* Describe the contents of map MAP, assuming that this map itself is
   reached by the sequence of prefix keys KEYS (a string or vector).
   PARTIAL, SHADOW, NOMENU are as in `describe_map_tree' above.  */

static void
describe_map (Lisp_Object keymap, Lisp_Object elt_prefix,
	      void (*elt_describer) (Lisp_Object, Lisp_Object),
	      int partial,
	      Lisp_Object shadow,
	      int mice_only_p,
	      Lisp_Object buffer)
{
  /* This function can GC */
  struct describe_map_closure describe_map_closure;
  Lisp_Object list = Qnil;
  struct buffer *buf = XBUFFER (buffer);
  Ichar printable_min = (CHAR_OR_CHAR_INTP (buf->ctl_arrow)
			  ? XCHAR_OR_CHAR_INT (buf->ctl_arrow)
			  : ((EQ (buf->ctl_arrow, Qt)
			      || EQ (buf->ctl_arrow, Qnil))
			     ? 256 : 160));
  int elided = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  extern Lisp_Object Qcharacter_of_keysym;

  keymap = get_keymap (keymap, 1, 1);
  describe_map_closure.partial = (partial ? Qsuppress_keymap : Qnil);
  describe_map_closure.shadow = shadow;
  describe_map_closure.list = &list;
  describe_map_closure.self_root = keymap;
  describe_map_closure.mice_only_p = mice_only_p;

  GCPRO4 (keymap, elt_prefix, shadow, list);

  traverse_keymaps (keymap, Qnil,
                    describe_map_parent_mapper, &describe_map_closure);

  if (!NILP (list))
    {
      list = list_sort (list, describe_map_sort_predicate, Qnil, Qnil);
      buffer_insert_ascstring (buf, "\n");
      while (!NILP (list))
	{
          Lisp_Object elt = XCAR (XCAR (list));
	  Lisp_Object keysym = XCAR (elt);
	  int modifiers = XFIXNUM (XCDR (elt));

	  if (!NILP (elt_prefix))
	    buffer_insert_lisp_string (buf, elt_prefix);

	  if (modifiers & XEMACS_MOD_META)
	    buffer_insert_ascstring (buf, "M-");
	  if (modifiers & XEMACS_MOD_CONTROL)
	    buffer_insert_ascstring (buf, "C-");
	  if (modifiers & XEMACS_MOD_SUPER)
	    buffer_insert_ascstring (buf, "S-");
	  if (modifiers & XEMACS_MOD_HYPER)
	    buffer_insert_ascstring (buf, "H-");
	  if (modifiers & XEMACS_MOD_ALT)
	    buffer_insert_ascstring (buf, "Alt-");
	  if (modifiers & XEMACS_MOD_SHIFT)
	    buffer_insert_ascstring (buf, "Sh-");
	  if (SYMBOLP (keysym))
	    {
	      Lisp_Object code = Fget (keysym, Qcharacter_of_keysym, Qnil);
	      Ichar c = (CHAR_OR_CHAR_INTP (code)
			  ? XCHAR_OR_CHAR_INT (code) : (Ichar) -1);
	      /* Calling Fsingle_key_description() would cons more */
#if 0                           /* This is bogus */
	      if (EQ (keysym, QKlinefeed))
		buffer_insert_ascstring (buf, "LFD");
	      else if (EQ (keysym, QKtab))
		buffer_insert_ascstring (buf, "TAB");
	      else if (EQ (keysym, QKreturn))
		buffer_insert_ascstring (buf, "RET");
	      else if (EQ (keysym, QKescape))
		buffer_insert_ascstring (buf, "ESC");
	      else if (EQ (keysym, QKdelete))
		buffer_insert_ascstring (buf, "DEL");
	      else if (EQ (keysym, QKspace))
		buffer_insert_ascstring (buf, "SPC");
	      else if (EQ (keysym, QKbackspace))
		buffer_insert_ascstring (buf, "BS");
	      else
#endif
                if (c >= printable_min)
		  buffer_insert_emacs_char (buf, c);
		else buffer_insert1 (buf, Fsymbol_name (keysym));
	    }
	  else if (CHARP (keysym))
	    buffer_insert_emacs_char (buf, XCHAR (keysym));
	  else
	    buffer_insert_ascstring (buf, "---bad keysym---");

	  if (elided)
	    elided = 0;
	  else
	    {
	      int k = 0;

	      while (elide_next_two_p (list))
		{
		  k++;
		  list = XCDR (list);
		}
	      if (k != 0)
		{
		  if (k == 1)
		    buffer_insert_ascstring (buf, ", ");
		  else
		    buffer_insert_ascstring (buf, " .. ");
		  elided = 1;
		  continue;
		}
	    }

	  /* Print a description of the definition of this character.  */
	  (*elt_describer) (XCDR (XCAR (list)), buffer);
	  list = XCDR (list);
	}
    }
  UNGCPRO;
}


void
syms_of_keymap (void)
{
  INIT_LISP_OBJECT (keymap);

  DEFSYMBOL (Qminor_mode_map_alist);

  DEFSYMBOL (Qkeymapp);

  DEFSYMBOL (Qsuppress_keymap);

  DEFSYMBOL (Qmodeline_map);
  DEFSYMBOL (Qtoolbar_map);
  DEFSYMBOL (Qremap);

  DEFSUBR (Fkeymap_parents);
  DEFSUBR (Fset_keymap_parents);
  DEFSUBR (Fkeymap_name);
  DEFSUBR (Fset_keymap_name);
  DEFSUBR (Fkeymap_prompt);
  DEFSUBR (Fset_keymap_prompt);
  DEFSUBR (Fkeymap_default_binding);
  DEFSUBR (Fset_keymap_default_binding);

  DEFSUBR (Fkeymapp);
  DEFSUBR (Fmake_keymap);
  DEFSUBR (Fmake_sparse_keymap);

  DEFSUBR (Fcopy_keymap);
  DEFSUBR (Fkeymap_fullness);
  DEFSUBR (Fmap_keymap);
  DEFSUBR (Fevent_matches_key_specifier_p);
  DEFSUBR (Fdefine_key);
  DEFSUBR (Fremap_command);
  DEFSUBR (Fcommands_remapped_to);
  DEFSUBR (Fcommand_remapping);
  DEFSUBR (Flookup_key);
  DEFSUBR (Fkey_binding);
  DEFSUBR (Fuse_global_map);
  DEFSUBR (Fuse_local_map);
  DEFSUBR (Fcurrent_local_map);
  DEFSUBR (Fcurrent_global_map);
  DEFSUBR (Fcurrent_keymaps);
  DEFSUBR (Faccessible_keymaps);
  DEFSUBR (Fkey_description);
  DEFSUBR (Fsingle_key_description);
  DEFSUBR (Fwhere_is_internal);
  DEFSUBR (Fdescribe_bindings_internal);

  DEFSUBR (Ftext_char_description);

  DEFSYMBOL (Qcontrol);
  DEFSYMBOL (Qctrl);
  DEFSYMBOL (Qmeta);
  DEFSYMBOL (Qsuper);
  DEFSYMBOL (Qhyper);
  DEFSYMBOL (Qalt);
  DEFSYMBOL (Qshift);
#define INCLUDE_BUTTON_ZERO
#define FROB(num)				\
  DEFSYMBOL (Qbutton##num);			\
  DEFSYMBOL (Qbutton##num##up);
#include "keymap-buttons.h"
#define FROB(num)				\
  DEFSYMBOL (Qmouse_##num);			\
  DEFSYMBOL (Qdown_mouse_##num);
#include "keymap-buttons.h"
  DEFSYMBOL (Qmenu_selection);
  DEFSYMBOL (QLFD);
  DEFSYMBOL (QTAB);
  DEFSYMBOL (QRET);
  DEFSYMBOL (QESC);
  DEFSYMBOL (QDEL);
  DEFSYMBOL (QSPC);
  DEFSYMBOL (QBS);
}

void
vars_of_keymap (void)
{
  DEFVAR_LISP ("meta-prefix-char", &Vmeta_prefix_char /*
Meta-prefix character.
This character followed by some character `foo' turns into `Meta-foo'.
This can be any form recognized as a single key specifier.
To disable the meta-prefix-char, set it to a negative number.
*/ );
  Vmeta_prefix_char = make_char (033);

  DEFVAR_LISP ("mouse-grabbed-buffer", &Vmouse_grabbed_buffer /*
A buffer which should be consulted first for all mouse activity.
When a mouse-click is processed, it will first be looked up in the
local-map of this buffer, and then through the normal mechanism if there
is no binding for that click.  This buffer's value of `mode-motion-hook'
will be consulted instead of the `mode-motion-hook' of the buffer of the
window under the mouse.  You should *bind* this, not set it.
*/ );
  Vmouse_grabbed_buffer = Qnil;

  DEFVAR_LISP ("overriding-local-map", &Voverriding_local_map /*
Keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and extent-local keymaps.
You should *bind* this, not set it.
*/ );
  Voverriding_local_map = Qnil;

  Fset (Qminor_mode_map_alist, Qnil);

  DEFVAR_LISP ("key-translation-map", &Vkey_translation_map /*
Keymap of key translations that can override keymaps.

This keymap works like `function-key-map', but is searched before it,
and applies even for keys that have ordinary bindings.

The `read-key-sequence' function replaces any subsequence bound by
`key-translation-map' with its binding.  More precisely, when the active
keymaps have no binding for the current key sequence but
`key-translation-map' binds a suffix of the sequence to a vector or string,
`read-key-sequence' replaces the matching suffix with its binding, and
continues with the new sequence.  See `key-binding' for details.

The events that come from bindings in `key-translation-map' are not
themselves looked up in `key-translation-map'.

#### FIXME: stolen from `function-key-map'; need better example.
#### I guess you could implement a Dvorak keyboard with this?
For example, suppose `key-translation-map' binds `ESC O P' to [f1].
Typing `ESC O P' to `read-key-sequence' would return
\[#<keypress-event f1>].  Typing `C-x ESC O P' would return
\[#<keypress-event control-X> #<keypress-event f1>].  If [f1]
were a prefix key, typing `ESC O P x' would return
\[#<keypress-event f1> #<keypress-event x>].
*/ );
  Vkey_translation_map = Qnil;

  DEFVAR_LISP ("global-tty-map", &Vglobal_tty_map /*
Global keymap that applies only to TTY's.
Key bindings are looked up in this map just before looking in the global map,
but only when the current console is a TTY console.  See also
`global-window-system-map'.
*/ );
  Vglobal_tty_map = Qnil;

  DEFVAR_LISP ("global-window-system-map", &Vglobal_window_system_map /*
Global keymap that applies only to window systems.
Key bindings are looked up in this map just before looking in the global map,
but only when the current console is not a TTY console.  See also
`global-tty-map'.
*/ );
  Vglobal_window_system_map = Qnil;

  DEFVAR_LISP ("vertical-divider-map", &Vvertical_divider_map /*
Keymap which handles mouse clicks over vertical dividers.
*/ );
  Vvertical_divider_map = Qnil;

  DEFVAR_INT ("keymap-tick", &keymap_tick /*
Incremented for each change to any keymap.
*/ );
  keymap_tick = 0;

  staticpro (&Vcurrent_global_map);

  Vsingle_space_string = make_string ((const Ibyte *) " ", 1);
  staticpro (&Vsingle_space_string);

  Qxemacs_command_remapping
    = Fmake_symbol (build_ascstring ("xemacs-command-remapping"));
  staticpro (&Qxemacs_command_remapping);
}

void
complex_vars_of_keymap (void)
{
  /* This function can GC */
  Lisp_Object ESC_prefix = intern ("ESC-prefix");
  Lisp_Object meta_disgustitute;

  Vcurrent_global_map = Fmake_keymap (Qnil);
  Vglobal_tty_map = Fmake_keymap (intern ("global-tty-map"));
  Vglobal_window_system_map =
    Fmake_keymap (intern ("global-window-system-map"));

  meta_disgustitute = Fmake_keymap (Qnil);
  Ffset (ESC_prefix, meta_disgustitute);
  /* no need to protect meta_disgustitute, though */
  keymap_store_internal (MAKE_MODIFIER_HASH_KEY (XEMACS_MOD_META),
                         XKEYMAP (Vcurrent_global_map),
                         meta_disgustitute);
  XKEYMAP (Vcurrent_global_map)->sub_maps_cache = Qt;

  Vkey_translation_map = Fmake_sparse_keymap (intern ("key-translation-map"));
}
