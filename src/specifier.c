/* Specifier implementation
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002, 2005, 2010 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Design by Ben Wing;
   Written by Ben Wing based on prototype for 19.12 by Chuck Thompson.
   Magic specifiers by Kirill Katsnelson.
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "chartab.h"
#include "device-impl.h"
#include "elhash.h"
#include "frame.h"
#include "glyphs.h"
#include "opaque.h"
#include "rangetab.h"
#include "specifier.h"
#include "window.h"

Lisp_Object Qspecifierp;
Lisp_Object Qremove_tag_set_prepend, Qremove_tag_set_append;
Lisp_Object Qremove_locale, Qremove_locale_type;

Lisp_Object Qconsole_type, Qdevice_class;

static Lisp_Object Vuser_defined_tags;
/* This is a hash table mapping charsets to "tag lists".  A tag list here
   is an assoc list mapping charset tags to size-two vectors (one for the
   initial stage, one for the final stage) containing t or nil, indicating
   whether the charset tag matches the charset for the given stage.  These
   values are determined at the time a charset tag is defined by calling
   the charset predicate on all the existing charsets, and at the time a
   charset is defined by calling the predicate on all existing charset
   tags. */
static Lisp_Object Vcharset_tag_lists;

typedef struct specifier_type_entry specifier_type_entry;
struct specifier_type_entry
{
  Lisp_Object symbol;
  struct specifier_methods *meths;
};

typedef struct
{
  Dynarr_declare (specifier_type_entry);
} specifier_type_entry_dynarr;

static specifier_type_entry_dynarr *the_specifier_type_entry_dynarr;

static const struct memory_description ste_description_1[] = {
  { XD_LISP_OBJECT, offsetof (specifier_type_entry, symbol) },
  { XD_BLOCK_PTR,  offsetof (specifier_type_entry, meths), 1,
    { &specifier_methods_description } },
  { XD_END }
};

static const struct sized_memory_description ste_description = {
  sizeof (specifier_type_entry),
  ste_description_1
};

static const struct memory_description sted_description_1[] = {
  XD_DYNARR_DESC (specifier_type_entry_dynarr, &ste_description),
  { XD_END }
};

static const struct sized_memory_description sted_description = {
  sizeof (specifier_type_entry_dynarr),
  sted_description_1
};

static Lisp_Object Vspecifier_type_list;

static Lisp_Object Vcached_specifiers;
/* Do NOT mark through this, or specifiers will never be GC'd. */
static Lisp_Object Vall_specifiers;

static Lisp_Object Vunlock_ghost_specifiers;

/* #### The purpose of this is to check for inheritance loops
   in specifiers that can inherit from other specifiers, but it's
   not yet implemented.

   #### Look into this for 19.14. */
/* static Lisp_Object_dynarr current_specifiers; */

static void recompute_cached_specifier_everywhere (Lisp_Object specifier);

EXFUN (Fspecifier_specs, 4);
EXFUN (Fremove_specifier, 4);


/************************************************************************/
/*                       Specifier object methods                       */
/************************************************************************/

/* Remove dead objects from the specified assoc list. */

static Lisp_Object
cleanup_assoc_list (Lisp_Object list)
{
  Lisp_Object loop, prev, retval;

  loop = retval = list;
  prev = Qnil;

  while (!NILP (loop))
    {
      Lisp_Object entry = XCAR (loop);
      Lisp_Object key = XCAR (entry);

      /* remember, dead windows can become alive again. */
      if (!WINDOWP (key) && object_dead_p (key))
	{
	  if (NILP (prev))
	    {
	      /* Removing the head. */
	      retval = XCDR (retval);
	    }
	  else
	    {
	      Fsetcdr (prev, XCDR (loop));
	    }
	}
      else
	prev = loop;

      loop = XCDR (loop);
    }

  return retval;
}

/* Remove dead objects from the various lists so that they
   don't keep getting marked as long as this specifier exists and
   therefore wasting memory. */

void
cleanup_specifiers (void)
{
  Lisp_Object rest;

  for (rest = Vall_specifiers;
       !NILP (rest);
       rest = XSPECIFIER (rest)->next_specifier)
    {
      Lisp_Specifier *sp = XSPECIFIER (rest);
      /* This effectively changes the specifier specs.
	 However, there's no need to call
	 recompute_cached_specifier_everywhere() or the
	 after-change methods because the only specs we
	 are removing are for dead objects, and they can
	 never have any effect on the specifier values:
	 specifiers can only be instantiated over live
	 objects, and you can't derive a dead object
	 from a live one. */
      sp->device_specs = cleanup_assoc_list (sp->device_specs);
      sp->frame_specs = cleanup_assoc_list (sp->frame_specs);
      sp->buffer_specs = cleanup_assoc_list (sp->buffer_specs);
      /* windows are handled specially because dead windows
	 can be resurrected */
    }
}

void
kill_specifier_buffer_locals (Lisp_Object buffer)
{
  Lisp_Object rest;

  for (rest = Vall_specifiers;
       !NILP (rest);
       rest = XSPECIFIER (rest)->next_specifier)
    {
      Lisp_Specifier *sp = XSPECIFIER (rest);

      /* Make sure we're actually going to be changing something.
	 Fremove_specifier() always calls
	 recompute_cached_specifier_everywhere() (#### but should
	 be smarter about this). */
      if (!NILP (assq_no_quit (buffer, sp->buffer_specs)))
	Fremove_specifier (rest, buffer, Qnil, Qnil);
    }
}

static Lisp_Object
mark_specifier (Lisp_Object obj)
{
  Lisp_Specifier *specifier = XSPECIFIER (obj);

  mark_object (specifier->global_specs);
  mark_object (specifier->device_specs);
  mark_object (specifier->frame_specs);
  mark_object (specifier->window_specs);
  mark_object (specifier->buffer_specs);
  mark_object (specifier->magic_parent);
  mark_object (specifier->fallback);
  if (!GHOST_SPECIFIER_P (XSPECIFIER (obj)))
    MAYBE_SPECMETH (specifier, mark, (obj));
  return Qnil;
}

/* The idea here is that the specifier specs point to locales
   (windows, buffers, frames, and devices), and we want to make sure
   that the specs disappear automatically when the associated locale
   is no longer in use.  For all but windows, "no longer in use"
   corresponds exactly to when the object is deleted (non-deleted
   objects are always held permanently in special lists, and deleted
   objects are never on these lists and never reusable).  To handle
   this, we just have cleanup_specifiers() called periodically
   (at the beginning of garbage collection); it removes all dead
   objects.

   For windows, however, it's trickier because dead objects can be
   converted to live ones again if the dead object is in a window
   configuration.  Therefore, for windows, "no longer in use"
   corresponds to when the window object is garbage-collected.
   We now use weak lists for this purpose.

*/

void
prune_specifiers (void)
{
  Lisp_Object rest, prev = Qnil;

  for (rest = Vall_specifiers;
       !NILP (rest);
       rest = XSPECIFIER (rest)->next_specifier)
    {
      if (! marked_p (rest))
	{
	  Lisp_Specifier* sp = XSPECIFIER (rest);
	  /* A bit of assertion that we're removing both parts of the
	     magic one altogether */
	  assert (!MAGIC_SPECIFIER_P (sp)
		  || (BODILY_SPECIFIER_P (sp) && marked_p (sp->fallback))
		  || (GHOST_SPECIFIER_P (sp) && marked_p (sp->magic_parent)));
	  /* This specifier is garbage.  Remove it from the list. */
	  if (NILP (prev))
	    Vall_specifiers = sp->next_specifier;
	  else
	    XSPECIFIER (prev)->next_specifier = sp->next_specifier;
	}
      else
	prev = rest;
    }
}

static void
print_specifier (Lisp_Object obj, Lisp_Object printcharfun,
		 int UNUSED (escapeflag))
{
  Lisp_Specifier *sp = XSPECIFIER (obj);
  int count = specpdl_depth ();
  Lisp_Object the_specs;

  if (print_readably)
    printing_unreadable_object_fmt ("#<%s-specifier 0x%x>",
				    sp->methods->name, LISP_OBJECT_UID (obj));

  write_fmt_string (printcharfun, "#<%s-specifier global=", sp->methods->name);
#if 0
  /* #### Not obvious this is useful, and overrides user settings; if we
     resurrect this, create variables like `print-specifier-length' so it
     can be controlled. */
  specbind (Qprint_string_length, make_fixnum (100));
  specbind (Qprint_length, make_fixnum (5));
#endif
  the_specs = Fspecifier_specs (obj, Qglobal, Qnil, Qnil);
  if (NILP (the_specs))
    /* there are no global specs */
    write_ascstring (printcharfun, "<unspecified>");
  else
    print_internal (the_specs, printcharfun, 1);
  if (!NILP (sp->fallback))
    {
      write_fmt_string_lisp (printcharfun, " fallback=%S", sp->fallback);
    }
  unbind_to (count);
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

#ifndef NEW_GC
static void
finalize_specifier (Lisp_Object obj)
{
  Lisp_Specifier *sp = XSPECIFIER (obj);
  if (!GHOST_SPECIFIER_P (sp) && sp->caching)
    {
      xfree (sp->caching);
      sp->caching = 0;
    }
}
#endif /* not NEW_GC */

static int
specifier_equal (Lisp_Object obj1, Lisp_Object obj2, int depth, int foldcase)
{
  Lisp_Specifier *s1 = XSPECIFIER (obj1);
  Lisp_Specifier *s2 = XSPECIFIER (obj2);
  int retval;
  Lisp_Object old_inhibit_quit = Vinhibit_quit;

  /* This function can be called from within redisplay.
     internal_equal can trigger a quit.  That leads to Bad Things. */
  Vinhibit_quit = Qt;

  depth++;
  retval =
    (s1->methods == s2->methods &&
     internal_equal_0 (s1->global_specs, s2->global_specs, depth, foldcase) &&
     internal_equal_0 (s1->device_specs, s2->device_specs, depth, foldcase) &&
     internal_equal_0 (s1->frame_specs,  s2->frame_specs,  depth, foldcase) &&
     internal_equal_0 (s1->window_specs, s2->window_specs, depth, foldcase) &&
     internal_equal_0 (s1->buffer_specs, s2->buffer_specs, depth, foldcase) &&
     internal_equal_0 (s1->fallback,     s2->fallback,     depth, foldcase));

  if (retval && HAS_SPECMETH_P (s1, equal))
    retval = SPECMETH (s1, equal, (obj1, obj2, depth - 1));

  Vinhibit_quit = old_inhibit_quit;
  return retval;
}

static Hashcode
specifier_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  Lisp_Specifier *s = XSPECIFIER (obj);

  /* specifier hashing is a bit problematic because there are so
     many places where data can be stored.  We pick what are perhaps
     the most likely places where interesting stuff will be. */
  return HASH5 ((HAS_SPECMETH_P (s, hash) ?
		 SPECMETH (s, hash, (obj, depth, equalp)) : 0),
		(Hashcode) s->methods,
		internal_hash (s->global_specs, depth + 1, equalp),
		internal_hash (s->frame_specs,  depth + 1, equalp),
		internal_hash (s->buffer_specs, depth + 1, equalp));
}

inline static Bytecount
aligned_sizeof_specifier (Bytecount specifier_type_specific_size)
{
  return MAX_ALIGN_SIZE (offsetof (Lisp_Specifier, data)
			 + specifier_type_specific_size);
}

static Bytecount
sizeof_specifier (Lisp_Object obj)
{
  const Lisp_Specifier *p = XSPECIFIER (obj);
  return aligned_sizeof_specifier (GHOST_SPECIFIER_P (p)
				   ? 0
				   : p->methods->extra_data_size);
}

static const struct memory_description specifier_methods_description_1[] = {
  { XD_LISP_OBJECT, offsetof (struct specifier_methods, predicate_symbol) },
  { XD_END }
};

const struct sized_memory_description specifier_methods_description = {
  sizeof (struct specifier_methods),
  specifier_methods_description_1
};

static const struct memory_description specifier_caching_description_1[] = {
  { XD_END }
};

#ifdef NEW_GC
DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("specifier-caching", specifier_caching,
				      0, specifier_caching_description_1,
				      struct specifier_caching);
#else /* not NEW_GC */
static const struct sized_memory_description specifier_caching_description = {
  sizeof (struct specifier_caching),
  specifier_caching_description_1
};
#endif /* not NEW_GC */

static const struct sized_memory_description specifier_extra_description_map[]
= {
  { offsetof (Lisp_Specifier, methods) },
  { offsetof (struct specifier_methods, extra_description) },
  { -1 },
};

const struct memory_description specifier_description[] = {
  { XD_BLOCK_PTR,  offsetof (Lisp_Specifier, methods), 1,
    { &specifier_methods_description } },
  { XD_LO_LINK,     offsetof (Lisp_Specifier, next_specifier) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, global_specs) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, device_specs) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, frame_specs) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, window_specs) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, buffer_specs) },
#ifdef NEW_GC
  { XD_LISP_OBJECT,  offsetof (Lisp_Specifier, caching) },
#else /* not NEW_GC */
  { XD_BLOCK_PTR,  offsetof (Lisp_Specifier, caching), 1,
    { &specifier_caching_description } },
#endif /* not NEW_GC */
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, magic_parent) },
  { XD_LISP_OBJECT, offsetof (Lisp_Specifier, fallback) },
  { XD_BLOCK_ARRAY, offsetof (Lisp_Specifier, data), 1,
    { specifier_extra_description_map } },
  { XD_END }
};

static const struct memory_description specifier_empty_extra_description_1[] =
  {
    { XD_END }
  };

const struct sized_memory_description specifier_empty_extra_description = {
  0, specifier_empty_extra_description_1
};

DEFINE_DUMPABLE_SIZABLE_LISP_OBJECT ("specifier", specifier,
				     mark_specifier, print_specifier,
				     IF_OLD_GC (finalize_specifier),
				     specifier_equal, specifier_hash,
				     specifier_description,
				     sizeof_specifier,
				     Lisp_Specifier);

/************************************************************************/
/*                       Creating specifiers                            */
/************************************************************************/

static struct specifier_methods *
decode_specifier_type (Lisp_Object type, Error_Behavior errb)
{
  int i;

  for (i = 0; i < Dynarr_length (the_specifier_type_entry_dynarr); i++)
    {
      if (EQ (type, Dynarr_at (the_specifier_type_entry_dynarr, i).symbol))
	return Dynarr_at (the_specifier_type_entry_dynarr, i).meths;
    }

  maybe_invalid_argument ("Invalid specifier type",
			  type, Qspecifier, errb);

  return 0;
}

static int
valid_specifier_type_p (Lisp_Object type)
{
  return decode_specifier_type (type, ERROR_ME_NOT) != 0;
}

DEFUN ("valid-specifier-type-p", Fvalid_specifier_type_p, 1, 1, 0, /*
Given a SPECIFIER-TYPE, return non-nil if it is valid.
Valid types are `generic', `integer', `boolean', `color', `font', `image',
`face-boolean', and `toolbar'.
*/
       (specifier_type))
{
  return valid_specifier_type_p (specifier_type) ? Qt : Qnil;
}

DEFUN ("specifier-type-list", Fspecifier_type_list, 0, 0, 0, /*
Return a list of valid specifier types.
*/
       ())
{
  return Fcopy_sequence (Vspecifier_type_list);
}

void
add_entry_to_specifier_type_list (Lisp_Object symbol,
				  struct specifier_methods *meths)
{
  struct specifier_type_entry entry;

  entry.symbol = symbol;
  entry.meths = meths;
  Dynarr_add (the_specifier_type_entry_dynarr, entry);
  Vspecifier_type_list = Fcons (symbol, Vspecifier_type_list);
}

static Lisp_Object
make_specifier_internal (struct specifier_methods *spec_meths,
			 Bytecount data_size, int call_create_meth)
{
  Lisp_Object specifier =
    ALLOC_SIZED_LISP_OBJECT (aligned_sizeof_specifier (data_size), specifier);
  Lisp_Specifier *sp = XSPECIFIER (specifier);

  sp->methods = spec_meths;
  sp->global_specs = Qnil;
  sp->device_specs = Qnil;
  sp->frame_specs = Qnil;
  sp->window_specs = make_weak_list (WEAK_LIST_KEY_ASSOC);
  sp->buffer_specs = Qnil;
  sp->fallback = Qnil;
  sp->magic_parent = Qnil;
  sp->caching = 0;
  sp->next_specifier = Vall_specifiers;

  Vall_specifiers = specifier;

  if (call_create_meth)
    {
      struct gcpro gcpro1;
      GCPRO1 (specifier);
      MAYBE_SPECMETH (XSPECIFIER (specifier), create, (specifier));
      UNGCPRO;
    }
  return specifier;
}

static Lisp_Object
make_specifier (struct specifier_methods *meths)
{
  return make_specifier_internal (meths, meths->extra_data_size, 1);
}

Lisp_Object
make_magic_specifier (Lisp_Object type)
{
  /* This function can GC */
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);
  Lisp_Object bodily, ghost;
  struct gcpro gcpro1;

  bodily = make_specifier (meths);
  GCPRO1 (bodily);
  ghost  = make_specifier_internal (meths, 0, 0);
  UNGCPRO;

  /* Connect guys together */
  XSPECIFIER (bodily)->magic_parent = Qt;
  XSPECIFIER (bodily)->fallback = ghost;
  XSPECIFIER (ghost)->magic_parent = bodily;

  return bodily;
}

DEFUN ("make-specifier", Fmake_specifier, 1, 1, 0, /*
Return a new specifier object of type TYPE.

A specifier is an object that can be used to keep track of a property
whose value can be per-buffer, per-window, per-frame, or per-device,
and can further be restricted to a particular console-type or
device-class.  Specifiers are used, for example, for the various
built-in properties of a face; this allows a face to have different
values in different frames, buffers, etc.

When speaking of the value of a specifier, it is important to
distinguish between the *setting* of a specifier, called an
\"instantiator\", and the *actual value*, called an \"instance\".  You
put various possible instantiators (i.e. settings) into a specifier
and associate them with particular locales (buffer, window, frame,
device, global), and then the instance (i.e. actual value) is
retrieved in a specific domain (window, frame, device) by looking
through the possible instantiators (i.e. settings).  This process is
called \"instantiation\".

To put settings into a specifier, use `set-specifier', or the
lower-level functions `add-spec-to-specifier' and
`add-spec-list-to-specifier'.  You can also temporarily bind a setting
to a specifier using `let-specifier'.  To retrieve settings, use
`specifier-specs', or its lower-level counterpart
`specifier-spec-list'.

To determine the actual value (i.e. the instance) in a particular domain, use
`specifier-instance'.  To determine the corresponding setting that yielded
the value (i.e. the instantiator), use `specifier-instantiator'.

For more information, see `set-specifier', `specifier-instance',
`specifier-specs', and `add-spec-to-specifier'; or, for a detailed
description of specifiers, including how exactly the instantiation
process works, see the chapter on specifiers in the XEmacs Lisp
Reference Manual.

TYPE specifies the particular type of specifier, and should be one of
the symbols `generic', `integer', `natnum', `boolean', `color', `font',
`image', `face-boolean', `display-table', `gutter', `gutter-size',
`gutter-visible' or `toolbar'.

For more information on particular types of specifiers, see the
functions `make-generic-specifier', `make-integer-specifier',
`make-natnum-specifier', `make-boolean-specifier',
`make-color-specifier', `make-font-specifier', `make-image-specifier',
`make-face-boolean-specifier', `make-gutter-size-specifier',
`make-gutter-visible-specifier', `default-toolbar', `default-gutter',
and `current-display-table'.
*/
       (type))
{
  /* This function can GC */
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);

  return make_specifier (meths);
}

DEFUN ("specifierp", Fspecifierp, 1, 1, 0, /*
Return t if OBJECT is a specifier.

A specifier is an object that can be used to keep track of a property
whose value can be per-buffer, per-window, per-frame, or per-device,
and can further be restricted to a particular console-type or device-class.
See `make-specifier'.
*/
       (object))
{
  return SPECIFIERP (object) ? Qt : Qnil;
}

DEFUN ("specifier-type", Fspecifier_type, 1, 1, 0, /*
Return the type of SPECIFIER.
*/
       (specifier))
{
  CHECK_SPECIFIER (specifier);
  return intern (XSPECIFIER (specifier)->methods->name);
}


/************************************************************************/
/*                       Locales and domains                            */
/************************************************************************/

DEFUN ("valid-specifier-locale-p", Fvalid_specifier_locale_p, 1, 1, 0, /*
Return t if LOCALE is a valid specifier locale.
Valid locales are devices, frames, windows, buffers, and `global'.
\(nil is not valid.)
*/
       (locale))
{
  /* This cannot GC. */
  return ((DEVICEP (locale) && DEVICE_LIVE_P (XDEVICE (locale))) ||
	  (FRAMEP  (locale) && FRAME_LIVE_P  (XFRAME  (locale))) ||
	  (BUFFERP (locale) && BUFFER_LIVE_P (XBUFFER (locale))) ||
	  /* dead windows are allowed because they may become live
	     windows again when a window configuration is restored */
	  WINDOWP (locale) ||
	  EQ (locale, Qglobal))
    ? Qt : Qnil;
}

DEFUN ("valid-specifier-domain-p", Fvalid_specifier_domain_p, 1, 1, 0, /*
Return t if DOMAIN is a valid specifier domain.
A domain is used to instantiate a specifier (i.e. determine the specifier's
value in that domain).  Valid domains are image instances, windows, frames,
and devices. \(nil is not valid.) image instances are pseudo-domains since
instantiation will actually occur in the window the image instance itself is
instantiated in.
*/
       (domain))
{
  /* This cannot GC. */
  return ((DEVICEP (domain) && DEVICE_LIVE_P (XDEVICE (domain))) ||
	  (FRAMEP  (domain) && FRAME_LIVE_P  (XFRAME  (domain))) ||
	  (WINDOWP (domain) && WINDOW_LIVE_P (XWINDOW (domain))) ||
	  /* #### get image instances out of domains! */
	  IMAGE_INSTANCEP (domain))
    ? Qt : Qnil;
}

DEFUN ("valid-specifier-locale-type-p", Fvalid_specifier_locale_type_p, 1,
       1, 0, /*
Given a specifier LOCALE-TYPE, return non-nil if it is valid.
Valid locale types are `global', `device', `frame', `window', and `buffer'.
\(Note, however, that in functions that accept either a locale or a locale
type, `global' is considered an individual locale.)
*/
       (locale_type))
{
  /* This cannot GC. */
  return (EQ (locale_type, Qglobal) ||
	  EQ (locale_type, Qdevice) ||
	  EQ (locale_type, Qframe)  ||
	  EQ (locale_type, Qwindow) ||
	  EQ (locale_type, Qbuffer)) ? Qt : Qnil;
}

static void
check_valid_locale_or_locale_type (Lisp_Object locale)
{
  /* This cannot GC. */
  if (EQ (locale, Qall) ||
      !NILP (Fvalid_specifier_locale_p (locale)) ||
      !NILP (Fvalid_specifier_locale_type_p (locale)))
    return;
  invalid_argument ("Invalid specifier locale or locale type", locale);
}

DEFUN ("specifier-locale-type-from-locale", Fspecifier_locale_type_from_locale,
       1, 1, 0, /*
Given a specifier LOCALE, return its type.
*/
       (locale))
{
  /* This cannot GC. */
  if (NILP (Fvalid_specifier_locale_p (locale)))
    invalid_argument ("Invalid specifier locale",
		      locale);
  if (DEVICEP (locale)) return Qdevice;
  if (FRAMEP  (locale)) return Qframe;
  if (WINDOWP (locale)) return Qwindow;
  if (BUFFERP (locale)) return Qbuffer;
  assert (EQ (locale, Qglobal));
  return Qglobal;
}

static Lisp_Object
decode_locale (Lisp_Object locale)
{
  /* This cannot GC. */
  if (NILP (locale))
    return Qglobal;
  else if (!NILP (Fvalid_specifier_locale_p (locale)))
    return locale;
  else
    invalid_argument ("Invalid specifier locale",
		      locale);

  return Qnil;
}

static enum spec_locale_type
decode_locale_type (Lisp_Object locale_type)
{
  /* This cannot GC. */
  if (EQ (locale_type, Qglobal)) return LOCALE_GLOBAL;
  if (EQ (locale_type, Qdevice)) return LOCALE_DEVICE;
  if (EQ (locale_type, Qframe))  return LOCALE_FRAME;
  if (EQ (locale_type, Qwindow)) return LOCALE_WINDOW;
  if (EQ (locale_type, Qbuffer)) return LOCALE_BUFFER;

  invalid_argument ("Invalid specifier locale type",
		    locale_type);
  RETURN_NOT_REACHED (LOCALE_GLOBAL);
}

Lisp_Object
decode_locale_list (Lisp_Object locale)
{
  /* This cannot GC. */
  /* The return value of this function must be GCPRO'd. */
  if (NILP (locale))
    {
      return list1 (Qall);
    }
  else if (CONSP (locale))
    {
      EXTERNAL_LIST_LOOP_2 (elt, locale)
	check_valid_locale_or_locale_type (elt);
      return locale;
    }
  else
    {
      check_valid_locale_or_locale_type (locale);
      return list1 (locale);
    }
}

static enum spec_locale_type
locale_type_from_locale (Lisp_Object locale)
{
  return decode_locale_type (Fspecifier_locale_type_from_locale (locale));
}

static void
check_valid_domain (Lisp_Object domain)
{
  if (NILP (Fvalid_specifier_domain_p (domain)))
    invalid_argument ("Invalid specifier domain",
		      domain);
}

Lisp_Object
decode_domain (Lisp_Object domain)
{
  if (NILP (domain))
    return Fselected_window (Qnil);
  check_valid_domain (domain);
  return domain;
}

/* Return the buffer of the domain if uniquely determinable; else, return
   Qnil. */
Lisp_Object
MAYBE_DOMAIN_BUFFER (Lisp_Object obj)
{
  Lisp_Object win = DOMAIN_WINDOW (obj);
  if (WINDOWP (win))
    return WINDOW_BUFFER (XWINDOW (win));
  else
    return Qnil;
}

/* Try hard to get a buffer from a domain.  If not uniquely determinable,
   return current buffer. @@#### Maybe we should use the buffer of the
   selected window. */
Lisp_Object
DOMAIN_BUFFER (Lisp_Object obj)
{
  Lisp_Object win = DOMAIN_WINDOW (obj);
  if (WINDOWP (win))
    return WINDOW_BUFFER (XWINDOW (win));
  else
    /* @@#### See comments above */
    return wrap_buffer (current_buffer);
}



/************************************************************************/
/*                                 Tags                                 */
/************************************************************************/

DEFUN ("valid-specifier-tag-p", Fvalid_specifier_tag_p, 1, 1, 0, /*
Return non-nil if TAG is a valid specifier tag.
See also `valid-specifier-tag-set-p'.
*/
       (tag))
{
  return (valid_console_type_p (tag) ||
	  valid_device_class_p (tag) ||
	  !NILP (assq_no_quit (tag, Vuser_defined_tags))) ? Qt : Qnil;
}

DEFUN ("valid-specifier-tag-set-p", Fvalid_specifier_tag_set_p, 1, 1, 0, /*
Return non-nil if TAG-SET is a valid specifier tag set.

A specifier tag set is an entity that is attached to an instantiator and can
be used to restrict the scope of that instantiator to a particular device
class, device type, or charset.  It can also be used to mark instantiators
added by a particular package so that they can be later removed as a group.

A specifier tag set consists of a list of zero of more specifier tags,
each of which is a symbol that is recognized by XEmacs as a tag.
\(The valid device types and device classes are always tags, as are
any tags defined by `define-specifier-tag'.) It is called a "tag set"
\(as opposed to a list) because the order of the tags or the number of
times a particular tag occurs does not matter.

Each tag has two predicates associated with it, which specify, respectively,
whether that tag applies to a particular device and whether it applies to a
particular character set.  The predefined tags which are device types and
classes match devices of that type or class.  User-defined tags can have any
device predicate, or none (meaning that all devices match).  When attempting
to instantiate a specifier, a particular instantiator is only considered if
the device of the domain being instantiated over matches all tags in the tag
set attached to that instantiator.

If a charset is to be considered--which is only the case for face
instantiators--this consideration may be done twice.  The first iteration
pays attention to the character set predicates; if no instantiator can be
found in that case, the search is repeated ignoring the character set
predicates.

Most of the time, a tag set is not specified, and the instantiator
gets a null tag set, which matches all devices.
*/
       (tag_set))
{
  Lisp_Object rest;

  for (rest = tag_set; !NILP (rest); rest = XCDR (rest))
    {
      if (!CONSP (rest))
	return Qnil;
      if (NILP (Fvalid_specifier_tag_p (XCAR (rest))))
	return Qnil;
      QUIT;
    }
  return Qt;
}

Lisp_Object
decode_specifier_tag_set (Lisp_Object tag_set)
{
  /* The return value of this function must be GCPRO'd. */
  if (!NILP (Fvalid_specifier_tag_p (tag_set)))
    return list1 (tag_set);
  if (NILP (Fvalid_specifier_tag_set_p (tag_set)))
    invalid_argument ("Invalid specifier tag-set",
		      tag_set);
  return tag_set;
}

static Lisp_Object
canonicalize_tag_set (Lisp_Object tag_set)
{
  int len = XFIXNUM (Flength (tag_set));
  Lisp_Object *tags, rest;
  int i, j;

  /* We assume in this function that the tag_set has already been
     validated, so there are no surprises. */

  if (len == 0 || len == 1)
    /* most common case */
    return tag_set;

  tags = alloca_array (Lisp_Object, len);

  i = 0;
  LIST_LOOP (rest, tag_set)
    tags[i++] = XCAR (rest);

  /* Sort the list of tags.  We use a bubble sort here (copied from
     extent_fragment_update()) -- reduces the function call overhead,
     and is the fastest sort for small numbers of items. */

  for (i = 1; i < len; i++)
    {
      j = i - 1;
      while (j >= 0 &&
	     qxestrcmp (XSTRING_DATA (XSYMBOL (tags[j])->name),
			XSTRING_DATA (XSYMBOL (tags[j+1])->name)) > 0)
	{
	  Lisp_Object tmp = tags[j];
	  tags[j] = tags[j+1];
	  tags[j+1] = tmp;
	  j--;
	}
    }

  /* Now eliminate duplicates. */

  for (i = 1, j = 1; i < len; i++)
    {
      /* j holds the destination, i the source. */
      if (!EQ (tags[i], tags[i-1]))
	tags[j++] = tags[i];
    }

  return Flist (j, tags);
}

DEFUN ("canonicalize-tag-set", Fcanonicalize_tag_set, 1, 1, 0, /*
Canonicalize the given tag set.
Two canonicalized tag sets can be compared with `equal' to see if they
represent the same tag set. (Specifically, canonicalizing involves
sorting by symbol name and removing duplicates.)
*/
       (tag_set))
{
  if (NILP (Fvalid_specifier_tag_set_p (tag_set)))
    invalid_argument ("Invalid tag set", tag_set);
  return canonicalize_tag_set (tag_set);
}

static int
device_matches_specifier_tag_set_p (Lisp_Object device, Lisp_Object tag_set)
{
  Lisp_Object devtype, devclass, rest;
  struct device *d = XDEVICE (device);

  devtype = DEVICE_TYPE (d);
  devclass = DEVICE_CLASS (d);

  LIST_LOOP (rest, tag_set)
    {
      Lisp_Object tag = XCAR (rest);
      Lisp_Object assoc;

      if (EQ (tag, devtype) || EQ (tag, devclass))
	continue;
      assoc = assq_no_quit (tag, DEVICE_USER_DEFINED_TAGS (d));
      /* other built-in tags (device types/classes) are not in
	 the user-defined-tags list. */
      if (NILP (assoc) || NILP (XCDR (assoc)))
	return 0;
    }

  return 1;
}

static int
charset_matches_specifier_tag_set_p (Lisp_Object charset, Lisp_Object tag_set,
				     enum font_specifier_matchspec_stages
				     stage)
{
  Lisp_Object rest;
  int res = 0;

  assert (stage < NUM_MATCHSPEC_STAGES);

  LIST_LOOP (rest, tag_set)
    {
      Lisp_Object tag = XCAR (rest);
      Lisp_Object assoc;
      Lisp_Object tag_list = Fgethash (charset, Vcharset_tag_lists, Qnil);

      /* In the event that, during the creation of a charset, no specifier
	 tags exist for which CHARSET-PREDICATE has been specified, then
	 that charset's entry in Vcharset_tag_lists will be nil, and this
	 charset shouldn't match. */

      if (NILP (tag_list))
	{
	  return 0;
	}

      /* Now, find out what the pre-calculated value is. */
      assoc = assq_no_quit (tag, tag_list);

      if (!(NILP (assoc)))
	{
	  assert (VECTORP (XCDR (assoc)));

	  /* In the event that a tag specifies a charset, then the specifier
	     must match for (this stage and this charset) for all
	     charset-specifying tags.  */
	  if (NILP (XVECTOR_DATA (XCDR (assoc))[stage]))
	    {
	      /* It doesn't match for this tag, even though the tag
		 specifies a charset. Return 0. */
	      return 0;
	    }

	  /* This tag specifies charset limitations, and this charset and
	     stage match those charset limitations.

	     In the event that a later tag specifies charset limitations
	     that don't match, the return 0 above prevents us giving a
	     positive match. */
	  res = 1;
	}
    }

  return res;
}


DEFUN ("device-matches-specifier-tag-set-p",
       Fdevice_matches_specifier_tag_set_p, 2, 2, 0, /*
Return non-nil if DEVICE matches specifier tag set TAG-SET.
This means that DEVICE matches each tag in the tag set. (Every
tag recognized by XEmacs has a predicate associated with it that
specifies which devices match it.)
*/
       (device, tag_set))
{
  CHECK_LIVE_DEVICE (device);

  if (NILP (Fvalid_specifier_tag_set_p (tag_set)))
    invalid_argument ("Invalid tag set", tag_set);

  return device_matches_specifier_tag_set_p (device, tag_set) ? Qt : Qnil;
}

/* Call CHARSET_PREDICATE on CHARSET, evaluating it at both stages (initial
   and final) and returning a size-two vector of the results. */

static Lisp_Object
call_charset_predicate (Lisp_Object charset_predicate, Lisp_Object charset)
{
  struct gcpro gcpro1;
  Lisp_Object charpres = make_vector (NUM_MATCHSPEC_STAGES, Qnil);
  int max_args = XFIXNUM (Ffunction_max_args (charset_predicate));
  GCPRO1 (charpres);
    

#define DEFINE_SPECIFIER_TAG_FROB(stage, enumstage)		\
  do {								\
    if (max_args > 1)						\
      {								\
	XVECTOR_DATA (charpres)[enumstage] =			\
	  call2_trapping_problems				\
	  ("Error during specifier tag charset predicate,"	\
	   " stage " #stage, charset_predicate,			\
	   charset, Q##stage, 0);				\
      }								\
    else							\
      {								\
	XVECTOR_DATA (charpres)[enumstage] =			\
	  call1_trapping_problems				\
	  ("Error during specifier tag charset predicate,"	\
	   " stage " #stage, charset_predicate,			\
	   charset, 0);						\
      }								\
								\
    if (UNBOUNDP (XVECTOR_DATA (charpres)[enumstage]))		\
      {								\
	XVECTOR_DATA (charpres)[enumstage] = Qnil;		\
      }								\
    else if (!NILP (XVECTOR_DATA (charpres)[enumstage]))	\
      {								\
	/* Don't want refs to random other objects.  */		\
	XVECTOR_DATA (charpres)[enumstage] = Qt;		\
      }								\
  } while (0)

  DEFINE_SPECIFIER_TAG_FROB (initial, STAGE_INITIAL);
  DEFINE_SPECIFIER_TAG_FROB (final, STAGE_FINAL);

#undef DEFINE_SPECIFIER_TAG_FROB

  UNGCPRO;

  return charpres;
}

Lisp_Object
define_specifier_tag (Lisp_Object tag, Lisp_Object device_predicate,
		      Lisp_Object charset_predicate)
{
  Lisp_Object assoc = assq_no_quit (tag, Vuser_defined_tags),
    concons, devcons;
  int recompute_devices = 0, recompute_charsets = 0;

  if (NILP (assoc))
    {
      recompute_devices = recompute_charsets = 1;
      Vuser_defined_tags = Fcons (list3 (tag, device_predicate,
					 charset_predicate),
				  Vuser_defined_tags);
      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  struct device *d = XDEVICE (XCAR (devcons));
	  /* Initially set the value to t in case of error
	     in device_predicate */
	  DEVICE_USER_DEFINED_TAGS (d) =
	    Fcons (Fcons (tag, Qt), DEVICE_USER_DEFINED_TAGS (d));
	}
    }
  else if (!NILP (device_predicate) && !NILP (XCADR (assoc)))
    {
      recompute_devices = 1;
      XCDR (assoc) = list2 (device_predicate, charset_predicate);
    }
  else if (!NILP (charset_predicate) || !NILP (XCADDR (assoc)))
    {
      /* If there exists a charset_predicate for the tag currently (even if
	 the new charset_predicate is nil), or if we're adding one, we need
	 to recompute.  This contrasts with the device predicates, where we
	 don't need to recompute if the old and new device predicates are
	 both nil.  */

      recompute_charsets = 1;
      XCDR (assoc) = list2 (device_predicate, charset_predicate);
    }

  /* Recompute the tag values for all devices and charsets, if necessary. In
     the special case where both the old and new device_predicates are nil,
     we know that we don't have to do it for the device. (It's probably
     common for people to call (define-specifier-tag) more than once on the
     same tag, and the most common case is where DEVICE_PREDICATE is not
     specified.) */

  if (recompute_devices)
    {
      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  Lisp_Object device = XCAR (devcons);
	  assoc = assq_no_quit (tag,
				DEVICE_USER_DEFINED_TAGS (XDEVICE (device)));
	  assert (CONSP (assoc));
	  if (NILP (device_predicate))
	    XCDR (assoc) = Qt;
	  else
	    XCDR (assoc) = !NILP (call1 (device_predicate, device)) ? Qt
	      : Qnil;
	}
    }

  if (recompute_charsets)
    {
      GC_EXTERNAL_LIST_LOOP_2 (charset_name, Fcharset_list ())
	{
	  Lisp_Object charset = Fget_charset (charset_name);
	  Lisp_Object tag_list = Fgethash (charset, Vcharset_tag_lists, Qnil);
	  Lisp_Object charpres;

          if (NILP (charset_predicate))
            {
	      Fputhash (charset, remassq_no_quit (tag, tag_list),
                        Vcharset_tag_lists);
              continue;
            }

	  charpres = call_charset_predicate (charset_predicate, charset);

	  assoc = assq_no_quit (tag, tag_list);
	  if (!NILP (assoc))
	    {
	      assert (CONSP (assoc));
	      XCDR (assoc) = charpres;
	    }
	  else
	    {
	      Fputhash (charset, Fcons (Fcons (tag, charpres), tag_list),
			Vcharset_tag_lists);
	    }
	}
      END_GC_EXTERNAL_LIST_LOOP (charset_name);
    }
  return Qt;
}

DEFUN ("define-specifier-tag", Fdefine_specifier_tag, 1, 3, 0, /*
Define a new specifier tag.

If DEVICE-PREDICATE is specified, it should be a function of one argument
\(a device) that specifies whether the tag matches that particular device.
If DEVICE-PREDICATE is omitted, the tag matches all devices.

If CHARSET-PREDICATE is supplied, it should be a function taking a single
Lisp character set argument.  A tag's charset predicate is primarily used to
determine what font to use for a given \(set of) charset\(s) when that tag
is used in a set-face-font call; a non-nil return value indicates that the
tag matches the charset.

The font matching process also has a concept of stages; the defined stages
are currently `initial' and `final', and there exist specifier tags with
those names that correspond to those stages.  On X11, 'initial is used when
the font matching process is looking for fonts that match the desired
registries of the charset--see the `charset-registries' function.  If that
match process fails, then the 'final tag becomes relevant; this means that a
more general lookup is desired, and that a font doesn't necessarily have to
match the desired XLFD for the face, just the charset repertoire for this
charset.  It also means that the charset registry and encoding used will be
`iso10646-1', and the characters will be converted to display using that
registry.

If a tag set matches no character set; the two-stage match process will
ignore the tag on its first pass, but if no match is found, it will respect
it on the second pass, where character set information is ignored.

You can redefine an existing user-defined specifier tag.  However, you
cannot redefine most of the built-in specifier tags \(the device types and
classes, `initial', and `final') or the symbols nil, t, `all', or `global'.
*/
       (tag, device_predicate, charset_predicate))
{
  CHECK_SYMBOL (tag);
  if (valid_device_class_p (tag) ||
      valid_console_type_p (tag) ||
      EQ (tag, Qinitial) || EQ (tag, Qfinal))
    invalid_change ("Cannot redefine built-in specifier tags", tag);
  /* Try to prevent common instantiators and locales from being
     redefined, to reduce ambiguity */
  if (NILP (tag) || EQ (tag, Qt) || EQ (tag, Qall) || EQ (tag, Qglobal))
    invalid_change ("Cannot define nil, t, `all', or `global'", tag);

  if (!NILP (charset_predicate))
    {
      Lisp_Object min_args = Ffunction_min_args (charset_predicate);
      Lisp_Object max_args = Ffunction_max_args (charset_predicate);
      if (!(FIXNUMP (min_args) && XFIXNUM (min_args) == 1 &&
	    FIXNUMP (max_args) && XFIXNUM (max_args) == 1))
	{
	  /* We only allow the stage argument to be specifed from C.  */
	  invalid_change ("Charset predicate must take one argument",
			  tag);
	}
    }

  return define_specifier_tag (tag, device_predicate, charset_predicate);
}

/* Called at device-creation time to initialize the user-defined
   tag values for the newly-created device. */

void
setup_device_initial_specifier_tags (struct device *d)
{
  Lisp_Object rest, rest2;
  Lisp_Object device = wrap_device (d);
  Lisp_Object device_predicate;
  int list_len;

  DEVICE_USER_DEFINED_TAGS (d) = Fcopy_alist (Vuser_defined_tags);

  /* Now set up the initial values */
  LIST_LOOP (rest, DEVICE_USER_DEFINED_TAGS (d))
    XCDR (XCAR (rest)) = Qt;

  for (rest = Vuser_defined_tags, rest2 = DEVICE_USER_DEFINED_TAGS (d);
       !NILP (rest); rest = XCDR (rest), rest2 = XCDR (rest2))
    {
      GET_LIST_LENGTH (XCAR(rest), list_len);

      assert (3 == list_len);

      device_predicate = XCADR (XCAR (rest));

      if (NILP (device_predicate))
	{
	  XCDR (XCAR (rest2)) = Qt;
	}
      else
	{
	  device_predicate = !NILP (call_critical_lisp_code
				    (d, device_predicate, device))
	    ? Qt : Qnil;
	  XCDR (XCAR (rest2)) = device_predicate;
	}
    }
}

void
setup_charset_initial_specifier_tags (Lisp_Object charset)
{
  Lisp_Object rest, charset_predicate, tag, new_value;
  Lisp_Object charset_tag_list = Qnil;

  LIST_LOOP (rest, Vuser_defined_tags)
    {
      tag = XCAR (XCAR (rest));
      charset_predicate = XCADDR (XCAR (rest));

      if (NILP (charset_predicate))
	{
	  continue;
	}

      new_value = call_charset_predicate (charset_predicate, charset);
      charset_tag_list = Fcons (Fcons (tag, new_value), charset_tag_list);
    }

  Fputhash (charset, charset_tag_list, Vcharset_tag_lists);
}

/* VM calls this, in vm-multiple-frames-possible-p, in the event that you're
   considering taking it out. */

DEFUN ("device-matching-specifier-tag-list",
       Fdevice_matching_specifier_tag_list,
       0, 1, 0, /*
Return a list of all specifier tags matching DEVICE.
DEVICE defaults to the selected device if omitted.
*/
       (device))
{
  struct device *d = decode_device (device);
  Lisp_Object rest, list = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (list);

  LIST_LOOP (rest, DEVICE_USER_DEFINED_TAGS (d))
    {
      if (!NILP (XCDR (XCAR (rest))))
	list = Fcons (XCAR (XCAR (rest)), list);
    }

  list = Fnreverse (list);
  list = Fcons (DEVICE_CLASS (d), list);
  list = Fcons (DEVICE_TYPE  (d), list);

  RETURN_UNGCPRO (list);
}

DEFUN ("specifier-tag-list", Fspecifier_tag_list, 0, 0, 0, /*
Return a list of all currently-defined specifier tags.
This includes the built-in ones (the device types and classes).
*/
       ())
{
  Lisp_Object list = Qnil, rest;
  struct gcpro gcpro1;

  GCPRO1 (list);

  LIST_LOOP (rest, Vuser_defined_tags)
    list = Fcons (XCAR (XCAR (rest)), list);

  list = Fnreverse (list);
  list = nconc2 (Fcopy_sequence (Vdevice_class_list), list);
  list = nconc2 (Fcopy_sequence (Vconsole_type_list), list);

  RETURN_UNGCPRO (list);
}

DEFUN ("specifier-tag-device-predicate", Fspecifier_tag_device_predicate,
       1, 1, 0, /*
Return the device predicate for the given specifier tag.
*/
       (tag))
{
  /* The return value of this function must be GCPRO'd. */
  CHECK_SYMBOL (tag);

  if (NILP (Fvalid_specifier_tag_p (tag)))
    invalid_argument ("Invalid specifier tag",
		      tag);

  /* Make up some predicates for the built-in types */

  if (valid_console_type_p (tag))
    return list3 (Qlambda, list1 (Qdevice),
		  list3 (Qeq, list2 (Qquote, tag),
			 list2 (Qconsole_type, Qdevice)));

  if (valid_device_class_p (tag))
    return list3 (Qlambda, list1 (Qdevice),
		  list3 (Qeq, list2 (Qquote, tag),
			 list2 (Qdevice_class, Qdevice)));

  return XCADR (assq_no_quit (tag, Vuser_defined_tags));
}

DEFUN ("specifier-tag-charset-predicate", Fspecifier_tag_charset_predicate,
       1, 1, 0, /*
Return the charset predicate for the given specifier tag.
*/
       (tag))
{
  /* The return value of this function must be GCPRO'd. */
  CHECK_SYMBOL (tag);

  if (NILP (Fvalid_specifier_tag_p (tag)))
    invalid_argument ("Invalid specifier tag",
		      tag);

  return XCADDR (assq_no_quit (tag, Vuser_defined_tags));
}

/* Return true if A "matches" B.  If EXACT_P is 0, A must be a subset of B.
   Otherwise, A must be `equal' to B.  The sets must be canonicalized. */
static int
tag_sets_match_p (Lisp_Object a, Lisp_Object b, int exact_p)
{
  if (!exact_p)
    {
      while (!NILP (a) && !NILP (b))
	{
	  if (EQ (XCAR (a), XCAR (b)))
	    a = XCDR (a);
	  b = XCDR (b);
	}

      return NILP (a);
    }
  else
    {
      while (!NILP (a) && !NILP (b))
	{
	  if (!EQ (XCAR (a), XCAR (b)))
	    return 0;
	  a = XCDR (a);
	  b = XCDR (b);
	}

      return NILP (a) && NILP (b);
    }
}


/************************************************************************/
/*                       Spec-lists and inst-lists                      */
/************************************************************************/

static Lisp_Object
call_validate_method (Lisp_Object boxed_method, Lisp_Object instantiator)
{
  ((void (*)(Lisp_Object)) get_opaque_ptr (boxed_method)) (instantiator);
  return Qt;
}

static Lisp_Object
check_valid_instantiator (Lisp_Object instantiator,
			  struct specifier_methods *meths,
			  Error_Behavior errb)
{
  if (meths->validate_method)
    {
      Lisp_Object retval;

      if (ERRB_EQ (errb, ERROR_ME))
	{
	  (meths->validate_method) (instantiator);
	  retval = Qt;
	}
      else
	{
	  Lisp_Object opaque = make_opaque_ptr ((void *)
						meths->validate_method);
	  struct gcpro gcpro1;

	  GCPRO1 (opaque);
	  retval = call_with_suspended_errors
	    ((lisp_fn_t) call_validate_method,
	     Qnil, Qspecifier, errb, 2, opaque, instantiator);

	  free_opaque_ptr (opaque);
	  UNGCPRO;
	}

      return retval;
    }
  return Qt;
}

DEFUN ("check-valid-instantiator", Fcheck_valid_instantiator, 2, 2, 0, /*
Signal an error if INSTANTIATOR is invalid for SPECIFIER-TYPE.
*/
       (instantiator, specifier_type))
{
  struct specifier_methods *meths = decode_specifier_type (specifier_type,
							   ERROR_ME);

  return check_valid_instantiator (instantiator, meths, ERROR_ME);
}

DEFUN ("valid-instantiator-p", Fvalid_instantiator_p, 2, 2, 0, /*
Return non-nil if INSTANTIATOR is valid for SPECIFIER-TYPE.
*/
       (instantiator, specifier_type))
{
  struct specifier_methods *meths = decode_specifier_type (specifier_type,
							   ERROR_ME);

  return check_valid_instantiator (instantiator, meths, ERROR_ME_NOT);
}

static Lisp_Object
check_valid_inst_list (Lisp_Object inst_list, struct specifier_methods *meths,
		       Error_Behavior errb)
{
  EXTERNAL_LIST_LOOP_2 (inst_pair, inst_list)
    {
      Lisp_Object tag_set;

      if (!CONSP (inst_pair))
	{
	  maybe_sferror (
			 "Invalid instantiator pair", inst_pair,
			 Qspecifier, errb);
	  return Qnil;
	}
      if (NILP (Fvalid_specifier_tag_set_p (tag_set = XCAR (inst_pair))))
	{
	  maybe_invalid_argument (
				  "Invalid specifier tag", tag_set,
				  Qspecifier, errb);
	  return Qnil;
	}

      if (NILP (check_valid_instantiator (XCDR (inst_pair), meths, errb)))
	return Qnil;
    }

  return Qt;
}

DEFUN ("check-valid-inst-list", Fcheck_valid_inst_list, 2, 2, 0, /*
Signal an error if INST-LIST is invalid for specifier type TYPE.
*/
       (inst_list, type))
{
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);

  return check_valid_inst_list (inst_list, meths, ERROR_ME);
}

DEFUN ("valid-inst-list-p", Fvalid_inst_list_p, 2, 2, 0, /*
Return non-nil if INST-LIST is valid for specifier type TYPE.
*/
       (inst_list, type))
{
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);

  return check_valid_inst_list (inst_list, meths, ERROR_ME_NOT);
}

static Lisp_Object
check_valid_spec_list (Lisp_Object spec_list, struct specifier_methods *meths,
		       Error_Behavior errb)
{
  EXTERNAL_LIST_LOOP_2 (spec, spec_list)
    {
      Lisp_Object locale;
      if (!CONSP (spec))
	{
	  maybe_sferror (
			 "Invalid specification list", spec_list,
			 Qspecifier, errb);
	  return Qnil;
	}
      if (NILP (Fvalid_specifier_locale_p (locale = XCAR (spec))))
	{
	  maybe_invalid_argument (
				  "Invalid specifier locale", locale,
				  Qspecifier, errb);
	  return Qnil;
	}

      if (NILP (check_valid_inst_list (XCDR (spec), meths, errb)))
	return Qnil;
    }

  return Qt;
}

DEFUN ("check-valid-spec-list", Fcheck_valid_spec_list, 2, 2, 0, /*
Signal an error if SPEC-LIST is invalid for specifier type TYPE.
*/
       (spec_list, type))
{
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);

  return check_valid_spec_list (spec_list, meths, ERROR_ME);
}

DEFUN ("valid-spec-list-p", Fvalid_spec_list_p, 2, 2, 0, /*
Return non-nil if SPEC-LIST is valid for specifier type TYPE.
*/
       (spec_list, type))
{
  struct specifier_methods *meths = decode_specifier_type (type, ERROR_ME);

  return check_valid_spec_list (spec_list, meths, ERROR_ME_NOT);
}

enum spec_add_meth
decode_how_to_add_specification (Lisp_Object how_to_add)
{
  if (NILP (how_to_add) || EQ (Qremove_tag_set_prepend, how_to_add))
    return SPEC_REMOVE_TAG_SET_PREPEND;
  if (EQ (Qremove_tag_set_append, how_to_add))
    return SPEC_REMOVE_TAG_SET_APPEND;
  if (EQ (Qappend, how_to_add))
    return SPEC_APPEND;
  if (EQ (Qprepend, how_to_add))
    return SPEC_PREPEND;
  if (EQ (Qremove_locale, how_to_add))
    return SPEC_REMOVE_LOCALE;
  if (EQ (Qremove_locale_type, how_to_add))
    return SPEC_REMOVE_LOCALE_TYPE;
  if (EQ (Qremove_all, how_to_add))
    return SPEC_REMOVE_ALL;

  invalid_constant ("Invalid `how-to-add' flag", how_to_add);

  RETURN_NOT_REACHED (SPEC_PREPEND);
}

/* Given a specifier object SPEC, return bodily specifier if SPEC is a
   ghost specifier, otherwise return the object itself
*/
static Lisp_Object
bodily_specifier (Lisp_Object spec)
{
  return (GHOST_SPECIFIER_P (XSPECIFIER (spec))
	  ? XSPECIFIER (spec)->magic_parent : spec);
}

/* Signal error if (specifier SPEC is read-only.
   Read only are ghost specifiers unless Vunlock_ghost_specifiers is
   non-nil.  All other specifiers are read-write.
*/
static void
check_modifiable_specifier (Lisp_Object spec)
{
  if (NILP (Vunlock_ghost_specifiers)
      && GHOST_SPECIFIER_P (XSPECIFIER (spec)))
    signal_error (Qsetting_constant,
		  "Attempt to modify read-only specifier",
		  spec);
}

int
unlock_ghost_specifiers_protected (void)
{
  return internal_bind_lisp_object (&Vunlock_ghost_specifiers, Qt);
}

/* This gets hit so much that the function call overhead had a
   measurable impact (according to Quantify).  #### We should figure
   out the frequency with which this is called with the various types
   and reorder the check accordingly. */
#define SPECIFIER_GET_SPEC_LIST(specifier, type)			\
  (type == LOCALE_GLOBAL ? &(XSPECIFIER (specifier)->global_specs)   :	\
   type == LOCALE_DEVICE ? &(XSPECIFIER (specifier)->device_specs)   :	\
   type == LOCALE_FRAME  ? &(XSPECIFIER (specifier)->frame_specs)    :	\
   type == LOCALE_WINDOW ? &(XWEAK_LIST_LIST				\
			     (XSPECIFIER (specifier)->window_specs)) :	\
   type == LOCALE_BUFFER ? &(XSPECIFIER (specifier)->buffer_specs)   :	\
   0)

static Lisp_Object *
specifier_get_inst_list (Lisp_Object specifier, Lisp_Object locale,
			 enum spec_locale_type type)
{
  Lisp_Object *spec_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object specification;

  if (type == LOCALE_GLOBAL)
    return spec_list;
  /* Calling assq_no_quit when it is just going to return nil anyhow
     is extremely expensive.  So sayeth Quantify. */
  if (!CONSP (*spec_list))
    return 0;
  specification = assq_no_quit (locale, *spec_list);
  if (NILP (specification))
    return 0;
  return &XCDR (specification);
}

/* For the given INST_LIST, return a new INST_LIST containing all elements
   where TAG-SET matches the element's tag set.  EXACT_P indicates whether
   the match must be exact (as opposed to a subset).  SHORT_P indicates
   that the short form (for `specifier-specs') should be returned if
   possible.  If COPY_TREE_P, `copy-tree' is used to ensure that no
   elements of the new list are shared with the initial list.
*/

static Lisp_Object
specifier_process_inst_list (Lisp_Object inst_list,
			     Lisp_Object tag_set, int exact_p,
			     int short_p, int copy_tree_p)
{
  Lisp_Object retval = Qnil;
  Lisp_Object rest;
  struct gcpro gcpro1;

  GCPRO1 (retval);
  LIST_LOOP (rest, inst_list)
    {
      Lisp_Object tagged_inst = XCAR (rest);
      Lisp_Object tagged_inst_tag = XCAR (tagged_inst);
      if (tag_sets_match_p (tag_set, tagged_inst_tag, exact_p))
	{
	  if (short_p && NILP (tagged_inst_tag))
	    retval = Fcons (copy_tree_p ?
			    Fcopy_tree (XCDR (tagged_inst), Qt) :
			    XCDR (tagged_inst),
			    retval);
	  else
	    retval = Fcons (copy_tree_p ? Fcopy_tree (tagged_inst, Qt) :
			    tagged_inst, retval);
	}
    }
  retval = Fnreverse (retval);
  UNGCPRO;
  /* If there is a single instantiator and the short form is
     requested, return just the instantiator (rather than a one-element
     list of it) unless it is nil (so that it can be distinguished from
     no instantiators at all). */
  if (short_p && CONSP (retval) && !NILP (XCAR (retval)) &&
      NILP (XCDR (retval)))
    return XCAR (retval);
  else
    return retval;
}

static Lisp_Object
specifier_get_external_inst_list (Lisp_Object specifier, Lisp_Object locale,
				  enum spec_locale_type type,
				  Lisp_Object tag_set, int exact_p,
				  int short_p, int copy_tree_p)
{
  Lisp_Object *inst_list = specifier_get_inst_list (specifier, locale,
						    type);
  if (!inst_list || NILP (*inst_list))
    {
      /* nil for *inst_list should only occur in `global' */
      assert (!inst_list || EQ (locale, Qglobal));
      return Qnil;
    }

  return specifier_process_inst_list (*inst_list, tag_set, exact_p,
				      short_p, copy_tree_p);
}

static Lisp_Object
specifier_get_external_spec_list (Lisp_Object specifier,
				  enum spec_locale_type type,
				  Lisp_Object tag_set, int exact_p)
{
  Lisp_Object *spec_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object retval = Qnil;
  Lisp_Object rest;
  struct gcpro gcpro1;

  assert (type != LOCALE_GLOBAL);
  /* We're about to let stuff go external; make sure there aren't
     any dead objects */
  *spec_list = cleanup_assoc_list (*spec_list);

  GCPRO1 (retval);
  LIST_LOOP (rest, *spec_list)
    {
      Lisp_Object spec = XCAR (rest);
      Lisp_Object inst_list =
	specifier_process_inst_list (XCDR (spec), tag_set, exact_p, 0, 1);
      if (!NILP (inst_list))
	retval = Fcons (Fcons (XCAR (spec), inst_list), retval);
    }
  RETURN_UNGCPRO (Fnreverse (retval));
}

static Lisp_Object *
specifier_new_spec (Lisp_Object specifier, Lisp_Object locale,
		    enum spec_locale_type type)
{
  Lisp_Object *spec_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object new_spec = Fcons (locale, Qnil);
  assert (type != LOCALE_GLOBAL);
  *spec_list = Fcons (new_spec, *spec_list);
  return &XCDR (new_spec);
}

/* For the given INST_LIST, return a new list comprised of elements
   where TAG_SET does not match the element's tag set.  This operation
   is destructive. */

static Lisp_Object
specifier_process_remove_inst_list (Lisp_Object inst_list,
				    Lisp_Object tag_set, int exact_p,
				    int *was_removed)
{
  Lisp_Object prev = Qnil, rest;

  *was_removed = 0;

  LIST_LOOP (rest, inst_list)
    {
      if (tag_sets_match_p (tag_set, XCAR (XCAR (rest)), exact_p))
	{
	  /* time to remove. */
	  *was_removed = 1;
	  if (NILP (prev))
	    inst_list = XCDR (rest);
	  else
	    XCDR (prev) = XCDR (rest);
	}
      else
	prev = rest;
    }

  return inst_list;
}

static void
specifier_remove_spec (Lisp_Object specifier, Lisp_Object locale,
		       enum spec_locale_type type,
		       Lisp_Object tag_set, int exact_p)
{
  Lisp_Object *spec_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object assoc;
  int was_removed;

  if (type == LOCALE_GLOBAL)
    *spec_list = specifier_process_remove_inst_list (*spec_list, tag_set,
						     exact_p, &was_removed);
  else
    {
      assoc = assq_no_quit (locale, *spec_list);
      if (NILP (assoc))
	/* this locale is not found. */
	return;
      XCDR (assoc) = specifier_process_remove_inst_list (XCDR (assoc),
							 tag_set, exact_p,
							 &was_removed);
      if (NILP (XCDR (assoc)))
	/* no inst-pairs left; remove this locale entirely. */
	*spec_list = remassq_no_quit (locale, *spec_list);
    }

  if (was_removed)
    MAYBE_SPECMETH (XSPECIFIER (specifier), after_change,
		    (bodily_specifier (specifier), locale));
}

static void
specifier_remove_locale_type (Lisp_Object specifier,
			      enum spec_locale_type type,
			      Lisp_Object tag_set, int exact_p)
{
  Lisp_Object *spec_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object prev = Qnil, rest;

  assert (type != LOCALE_GLOBAL);
  LIST_LOOP (rest, *spec_list)
    {
      int was_removed;
      int remove_spec = 0;
      Lisp_Object spec = XCAR (rest);

      /* There may be dead objects floating around */
      /* remember, dead windows can become alive again. */
      if (!WINDOWP (XCAR (spec)) && object_dead_p (XCAR (spec)))
	{
	  remove_spec = 1;
	  was_removed = 0;
	}
      else
	{
	  XCDR (spec) = specifier_process_remove_inst_list (XCDR (spec),
							    tag_set, exact_p,
							    &was_removed);
	  if (NILP (XCDR (spec)))
	    remove_spec = 1;
	}

      if (remove_spec)
	{
	  if (NILP (prev))
	    *spec_list = XCDR (rest);
	  else
	    XCDR (prev) = XCDR (rest);
	}
      else
	prev = rest;

      if (was_removed)
	MAYBE_SPECMETH (XSPECIFIER (specifier), after_change,
			(bodily_specifier (specifier), XCAR (spec)));
    }
}

/* NEW_LIST is going to be added to INST_LIST, with add method ADD_METH.
   Frob INST_LIST according to ADD_METH.  No need to call an after-change
   function; the calling function will do this.  Return either SPEC_PREPEND
   or SPEC_APPEND, indicating whether to prepend or append the NEW_LIST. */

static enum spec_add_meth
handle_multiple_add_insts (Lisp_Object *inst_list,
			   Lisp_Object new_list,
			   enum spec_add_meth add_meth)
{
  switch (add_meth)
    {
    case SPEC_REMOVE_TAG_SET_APPEND:
      add_meth = SPEC_APPEND;
      goto remove_tag_set;
    case SPEC_REMOVE_TAG_SET_PREPEND:
      add_meth = SPEC_PREPEND;
    remove_tag_set:
      {
	Lisp_Object rest;

	LIST_LOOP (rest, new_list)
	  {
	    Lisp_Object canontag = canonicalize_tag_set (XCAR (XCAR (rest)));
	    struct gcpro gcpro1;

	    GCPRO1 (canontag);
	    /* pull out all elements from the existing list with the
	       same tag as any tags in NEW_LIST. */
	    *inst_list = remassoc_no_quit (canontag, *inst_list);
	    UNGCPRO;
	  }
      }
      return add_meth;
    case SPEC_REMOVE_LOCALE:
      *inst_list = Qnil;
      return SPEC_PREPEND;
    case SPEC_APPEND:
      return add_meth;
    default:
      return SPEC_PREPEND;
    }
}

/* Given a LOCALE and INST_LIST that is going to be added to SPECIFIER,
   copy, canonicalize, and call the going_to_add methods as necessary
   to produce a new list that is the one that really will be added
   to the specifier. */

static Lisp_Object
build_up_processed_list (Lisp_Object specifier, Lisp_Object locale,
			 Lisp_Object inst_list)
{
  /* The return value of this function must be GCPRO'd. */
  Lisp_Object rest, list_to_build_up = Qnil;
  Lisp_Specifier *sp = XSPECIFIER (specifier);
  struct gcpro gcpro1;

  GCPRO1 (list_to_build_up);
  LIST_LOOP (rest, inst_list)
    {
      Lisp_Object tag_set = XCAR (XCAR (rest));
      Lisp_Object sub_inst_list = Qnil;
      Lisp_Object instantiator;
      struct gcpro ngcpro1, ngcpro2;

      if (HAS_SPECMETH_P (sp, copy_instantiator))
	instantiator = SPECMETH (sp, copy_instantiator,
				 (XCDR (XCAR (rest))));
      else
	instantiator = Fcopy_tree (XCDR (XCAR (rest)), Qt);

      NGCPRO2 (instantiator, sub_inst_list);
      /* call the will-add method; it may GC */
      sub_inst_list = HAS_SPECMETH_P (sp, going_to_add) ?
	SPECMETH (sp, going_to_add,
		  (bodily_specifier (specifier), locale,
		   tag_set, instantiator)) :
	Qt;
      if (EQ (sub_inst_list, Qt))
	/* no change here. */
	sub_inst_list = list1 (Fcons (canonicalize_tag_set (tag_set),
				      instantiator));
      else
	{
	  /* now canonicalize all the tag sets in the new objects */
	  Lisp_Object rest2;
	  LIST_LOOP (rest2, sub_inst_list)
	    XCAR (XCAR (rest2)) = canonicalize_tag_set (XCAR (XCAR (rest2)));
	}

      list_to_build_up = nconc2 (sub_inst_list, list_to_build_up);
      NUNGCPRO;
    }

  RETURN_UNGCPRO (Fnreverse (list_to_build_up));
}

/* Add a specification (locale and instantiator list) to a specifier.
   ADD_METH specifies what to do with existing specifications in the
   specifier, and is an enum that corresponds to the values in
   `add-spec-to-specifier'.  The calling routine is responsible for
   validating LOCALE and INST-LIST, but the tag-sets in INST-LIST
   do not need to be canonicalized. */

/* #### I really need to rethink the after-change
   functions to make them easier to use and more efficient. */

static void
specifier_add_spec (Lisp_Object specifier, Lisp_Object locale,
		    Lisp_Object inst_list, enum spec_add_meth add_meth)
{
  Lisp_Specifier *sp = XSPECIFIER (specifier);
  enum spec_locale_type type = locale_type_from_locale (locale);
  Lisp_Object *orig_inst_list, tem;
  Lisp_Object list_to_build_up = Qnil;
  struct gcpro gcpro1;

  if (NILP (inst_list))
    return;

  GCPRO1 (list_to_build_up);
  list_to_build_up = build_up_processed_list (specifier, locale, inst_list);
  /* Now handle REMOVE_LOCALE_TYPE and REMOVE_ALL.  These are the
     add-meth types that affect locales other than this one. */
  if (add_meth == SPEC_REMOVE_LOCALE_TYPE)
    specifier_remove_locale_type (specifier, type, Qnil, 0);
  else if (add_meth == SPEC_REMOVE_ALL)
    {
      specifier_remove_locale_type (specifier, LOCALE_BUFFER, Qnil, 0);
      specifier_remove_locale_type (specifier, LOCALE_WINDOW, Qnil, 0);
      specifier_remove_locale_type (specifier, LOCALE_FRAME,  Qnil, 0);
      specifier_remove_locale_type (specifier, LOCALE_DEVICE, Qnil, 0);
      specifier_remove_spec (specifier, Qglobal, LOCALE_GLOBAL, Qnil, 0);
    }

  orig_inst_list = specifier_get_inst_list (specifier, locale, type);
  if (!orig_inst_list)
    orig_inst_list = specifier_new_spec (specifier, locale, type);
  add_meth = handle_multiple_add_insts (orig_inst_list, list_to_build_up,
					add_meth);

  if (add_meth == SPEC_PREPEND)
    tem = nconc2 (list_to_build_up, *orig_inst_list);
  else if (add_meth == SPEC_APPEND)
    tem = nconc2 (*orig_inst_list, list_to_build_up);
  else
    {
      ABORT ();
      tem = Qnil;
    }

  *orig_inst_list = tem;

  UNGCPRO;

  /* call the after-change method */
  MAYBE_SPECMETH (sp, after_change,
		  (bodily_specifier (specifier), locale));
}

static void
specifier_copy_spec (Lisp_Object specifier, Lisp_Object dest,
		     Lisp_Object locale, enum spec_locale_type type,
		     Lisp_Object tag_set, int exact_p,
		     enum spec_add_meth add_meth)
{
  Lisp_Object inst_list =
    specifier_get_external_inst_list (specifier, locale, type, tag_set,
				      exact_p, 0, 0);
  specifier_add_spec (dest, locale, inst_list, add_meth);
}

static void
specifier_copy_locale_type (Lisp_Object specifier, Lisp_Object dest,
			    enum spec_locale_type type,
			    Lisp_Object tag_set, int exact_p,
			    enum spec_add_meth add_meth)
{
  Lisp_Object *src_list = SPECIFIER_GET_SPEC_LIST (specifier, type);
  Lisp_Object rest;

  /* This algorithm is O(n^2) in running time.
     It's certainly possible to implement an O(n log n) algorithm,
     but I doubt there's any need to. */

  LIST_LOOP (rest, *src_list)
    {
      Lisp_Object spec = XCAR (rest);
      /* There may be dead objects floating around */
      /* remember, dead windows can become alive again. */
      if (WINDOWP (XCAR (spec)) || !object_dead_p (XCAR (spec)))
	specifier_add_spec
	  (dest, XCAR (spec),
	   specifier_process_inst_list (XCDR (spec), tag_set, exact_p, 0, 0),
	   add_meth);
    }
}

/* map MAPFUN over the locales in SPECIFIER that are given in LOCALE.
   CLOSURE is passed unchanged to MAPFUN.  LOCALE can be one of

   -- nil (same as `all')
   -- a single locale, locale type, or `all'
   -- a list of locales, locale types, and/or `all'

   MAPFUN is called for each locale and locale type given; for `all',
   it is called for the locale `global' and for the four possible
   locale types.  In each invocation, either LOCALE will be a locale
   and LOCALE_TYPE will be the locale type of this locale,
   or LOCALE will be nil and LOCALE_TYPE will be a locale type.
   If MAPFUN ever returns non-zero, the mapping is halted and the
   value returned is returned from map_specifier().  Otherwise, the
   mapping proceeds to the end and map_specifier() returns 0.
*/

static int
map_specifier (Lisp_Object specifier, Lisp_Object locale,
	       int (*mapfun) (Lisp_Object specifier,
			      Lisp_Object locale,
			      enum spec_locale_type locale_type,
			      Lisp_Object tag_set,
			      int exact_p,
			      void *closure),
	       Lisp_Object tag_set, Lisp_Object exact_p,
	       void *closure)
{
  int retval = 0;
  Lisp_Object rest;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (tag_set, locale);
  locale = decode_locale_list (locale);
  tag_set = decode_specifier_tag_set (tag_set);
  tag_set = canonicalize_tag_set (tag_set);

  LIST_LOOP (rest, locale)
    {
      Lisp_Object theloc = XCAR (rest);
      if (!NILP (Fvalid_specifier_locale_p (theloc)))
	{
	  retval = (*mapfun) (specifier, theloc,
			      locale_type_from_locale (theloc),
			      tag_set, !NILP (exact_p), closure);
	  if (retval)
	    break;
	}
      else if (!NILP (Fvalid_specifier_locale_type_p (theloc)))
	{
	  retval = (*mapfun) (specifier, Qnil,
			      decode_locale_type (theloc), tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	}
      else
	{
	  assert (EQ (theloc, Qall));
	  retval = (*mapfun) (specifier, Qnil, LOCALE_BUFFER, tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	  retval = (*mapfun) (specifier, Qnil, LOCALE_WINDOW, tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	  retval = (*mapfun) (specifier, Qnil, LOCALE_FRAME, tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	  retval = (*mapfun) (specifier, Qnil, LOCALE_DEVICE, tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	  retval = (*mapfun) (specifier, Qglobal, LOCALE_GLOBAL, tag_set,
			      !NILP (exact_p), closure);
	  if (retval)
	    break;
	}
    }

  UNGCPRO;
  return retval;
}

DEFUN ("add-spec-to-specifier", Fadd_spec_to_specifier, 2, 5, 0, /*
Add a specification to SPECIFIER.
The specification maps from LOCALE (which should be a window, buffer,
frame, device, or `global', and defaults to `global') to INSTANTIATOR,
whose allowed values depend on the type of the specifier.  Optional
argument TAG-SET limits the instantiator to apply only to the specified
tag set, which should be a list of tags all of which must match the
device being instantiated over (tags are a device type, a device class,
or tags defined with `define-specifier-tag').  Specifying a single
symbol for TAG-SET is equivalent to specifying a one-element list
containing that symbol.  Optional argument HOW-TO-ADD specifies what to
do if there are already specifications in the specifier.
It should be one of

  `prepend'		Put at the beginning of the current list of
			instantiators for LOCALE.
  `append'		Add to the end of the current list of
			instantiators for LOCALE.
  `remove-tag-set-prepend' (this is the default)
			Remove any existing instantiators whose tag set is
			the same as TAG-SET; then put the new instantiator
			at the beginning of the current list. ("Same tag
			set" means that they contain the same elements.
			The order may be different.)
  `remove-tag-set-append'
			Remove any existing instantiators whose tag set is
			the same as TAG-SET; then put the new instantiator
			at the end of the current list.
  `remove-locale'	Remove all previous instantiators for this locale
			before adding the new spec.
  `remove-locale-type'	Remove all specifications for all locales of the
			same type as LOCALE (this includes LOCALE itself)
			before adding the new spec.
  `remove-all'		Remove all specifications from the specifier
			before adding the new spec.

You can retrieve the specifications for a particular locale or locale type
with the function `specifier-spec-list' or `specifier-specs'.
*/
       (specifier, instantiator, locale, tag_set, how_to_add))
{
  enum spec_add_meth add_meth;
  Lisp_Object inst_list;
  struct gcpro gcpro1;

  CHECK_SPECIFIER (specifier);
  check_modifiable_specifier (specifier);

  locale = decode_locale (locale);
  check_valid_instantiator (instantiator,
			    decode_specifier_type
			    (Fspecifier_type (specifier), ERROR_ME),
			    ERROR_ME);
  /* tag_set might be newly-created material, but it's part of inst_list
     so is properly GC-protected. */
  tag_set = decode_specifier_tag_set (tag_set);
  add_meth = decode_how_to_add_specification (how_to_add);

  inst_list = list1 (Fcons (tag_set, instantiator));
  GCPRO1 (inst_list);
  specifier_add_spec (specifier, locale, inst_list, add_meth);
  recompute_cached_specifier_everywhere (specifier);
  RETURN_UNGCPRO (Qnil);
}

DEFUN ("add-spec-list-to-specifier", Fadd_spec_list_to_specifier, 2, 3, 0, /*
Add SPEC-LIST (a list of specifications) to SPECIFIER.
The format of SPEC-LIST is

  ((LOCALE (TAG-SET . INSTANTIATOR) ...) ...)

where
  LOCALE := a window, a buffer, a frame, a device, or `global'
  TAG-SET := an unordered list of zero or more TAGS, each of which
	     is a symbol
  TAG := a device class (see `valid-device-class-p'), a device type
	 (see `valid-console-type-p'), or a tag defined with
	 `define-specifier-tag'
  INSTANTIATOR := format determined by the type of specifier

The pair (TAG-SET . INSTANTIATOR) is called an `inst-pair'.
A list of inst-pairs is called an `inst-list'.
The pair (LOCALE . INST-LIST) is called a `specification' or `spec'.
A spec-list, then, can be viewed as a list of specifications.

HOW-TO-ADD specifies how to combine the new specifications with
the existing ones, and has the same semantics as for
`add-spec-to-specifier'.

In many circumstances, the higher-level function `set-specifier' is
more convenient and should be used instead.
*/
       (specifier, spec_list, how_to_add))
{
  enum spec_add_meth add_meth;
  Lisp_Object rest;

  CHECK_SPECIFIER (specifier);
  check_modifiable_specifier (specifier);

  check_valid_spec_list (spec_list,
			 decode_specifier_type
			 (Fspecifier_type (specifier), ERROR_ME),
			 ERROR_ME);
  add_meth = decode_how_to_add_specification (how_to_add);

  LIST_LOOP (rest, spec_list)
    {
      /* Placating the GCC god. */
      Lisp_Object specification = XCAR (rest);
      Lisp_Object locale    = XCAR (specification);
      Lisp_Object inst_list = XCDR (specification);

      specifier_add_spec (specifier, locale, inst_list, add_meth);
    }
  recompute_cached_specifier_everywhere (specifier);
  return Qnil;
}

void
add_spec_to_ghost_specifier (Lisp_Object specifier, Lisp_Object instantiator,
			     Lisp_Object locale, Lisp_Object tag_set,
			     Lisp_Object how_to_add)
{
  int depth = unlock_ghost_specifiers_protected ();
  Fadd_spec_to_specifier (XSPECIFIER (specifier)->fallback,
			  instantiator, locale, tag_set, how_to_add);
  unbind_to (depth);
}

struct specifier_spec_list_closure
{
  Lisp_Object head, tail;
};

static int
specifier_spec_list_mapfun (Lisp_Object specifier,
			    Lisp_Object locale,
			    enum spec_locale_type locale_type,
			    Lisp_Object tag_set,
			    int exact_p,
			    void *closure)
{
  struct specifier_spec_list_closure *cl =
    (struct specifier_spec_list_closure *) closure;
  Lisp_Object partial;

  if (NILP (locale))
    partial = specifier_get_external_spec_list (specifier,
						locale_type,
						tag_set, exact_p);
  else
    {
      partial = specifier_get_external_inst_list (specifier, locale,
						  locale_type, tag_set,
						  exact_p, 0, 1);
      if (!NILP (partial))
	partial = list1 (Fcons (locale, partial));
    }
  if (NILP (partial))
    return 0;

  /* tack on the new list */
  if (NILP (cl->tail))
    cl->head = cl->tail = partial;
  else
    XCDR (cl->tail) = partial;
  /* find the new tail */
  while (CONSP (XCDR (cl->tail)))
    cl->tail = XCDR (cl->tail);
  return 0;
}

/* For the given SPECIFIER create and return a list of all specs
   contained within it, subject to LOCALE.  If LOCALE is a locale, only
   specs in that locale will be returned.  If LOCALE is a locale type,
   all specs in all locales of that type will be returned.  If LOCALE is
   nil, all specs will be returned.  This always copies lists and never
   returns the actual lists, because we do not want someone manipulating
   the actual objects.  This may cause a slight loss of potential
   functionality but if we were to allow it then a user could manage to
   violate our assertion that the specs contained in the actual
   specifier lists are all valid. */

DEFUN ("specifier-spec-list", Fspecifier_spec_list, 1, 4, 0, /*
Return the spec-list of specifications for SPECIFIER in LOCALE.

If LOCALE is a particular locale (a buffer, window, frame, device,
or `global'), a spec-list consisting of the specification for that
locale will be returned.

If LOCALE is a locale type (i.e. `buffer', `window', `frame', or `device'),
a spec-list of the specifications for all locales of that type will be
returned.

If LOCALE is nil or `all', a spec-list of all specifications in SPECIFIER
will be returned.

LOCALE can also be a list of locales, locale types, and/or `all'; the
result is as if `specifier-spec-list' were called on each element of the
list and the results concatenated together.

Only instantiators where TAG-SET (a list of zero or more tags) is a
subset of (or possibly equal to) the instantiator's tag set are returned.
\(The default value of nil is a subset of all tag sets, so in this case
no instantiators will be screened out.) If EXACT-P is non-nil, however,
TAG-SET must be equal to an instantiator's tag set for the instantiator
to be returned.
*/
       (specifier, locale, tag_set, exact_p))
{
  struct specifier_spec_list_closure cl;
  struct gcpro gcpro1, gcpro2;

  CHECK_SPECIFIER (specifier);
  cl.head = cl.tail = Qnil;
  GCPRO2 (cl.head, cl.tail);
  map_specifier (specifier, locale, specifier_spec_list_mapfun,
		 tag_set, exact_p, &cl);
  UNGCPRO;
  return cl.head;
}


DEFUN ("specifier-specs", Fspecifier_specs, 1, 4, 0, /*
Return the specification(s) for SPECIFIER in LOCALE.

If LOCALE is a single locale or is a list of one element containing a
single locale, then a "short form" of the instantiators for that locale
will be returned.  Otherwise, this function is identical to
`specifier-spec-list'.

The "short form" is designed for readability and not for ease of use
in Lisp programs, and is as follows:

1. If there is only one instantiator, then an inst-pair (i.e. cons of
   tag and instantiator) will be returned; otherwise a list of
   inst-pairs will be returned.
2. For each inst-pair returned, if the instantiator's tag is `any',
   the tag will be removed and the instantiator itself will be returned
   instead of the inst-pair.
3. If there is only one instantiator, its value is nil, and its tag is
   `any', a one-element list containing nil will be returned rather
   than just nil, to distinguish this case from there being no
   instantiators at all.
*/
       (specifier, locale, tag_set, exact_p))
{
  if (!NILP (Fvalid_specifier_locale_p (locale)) ||
      (CONSP (locale) && !NILP (Fvalid_specifier_locale_p (XCAR (locale))) &&
       NILP (XCDR (locale))))
    {
      struct gcpro gcpro1;

      CHECK_SPECIFIER (specifier);
      if (CONSP (locale))
	locale = XCAR (locale);
      GCPRO1 (tag_set);
      tag_set = decode_specifier_tag_set (tag_set);
      tag_set = canonicalize_tag_set (tag_set);
      RETURN_UNGCPRO
	(specifier_get_external_inst_list (specifier, locale,
					   locale_type_from_locale (locale),
					   tag_set, !NILP (exact_p), 1, 1));
    }
  else
    return Fspecifier_spec_list (specifier, locale, tag_set, exact_p);
}

static int
remove_specifier_mapfun (Lisp_Object specifier,
			 Lisp_Object locale,
			 enum spec_locale_type locale_type,
			 Lisp_Object tag_set,
			 int exact_p,
			 void *UNUSED (closure))
{
  if (NILP (locale))
    specifier_remove_locale_type (specifier, locale_type, tag_set, exact_p);
  else
    specifier_remove_spec (specifier, locale, locale_type, tag_set, exact_p);
  return 0;
}

DEFUN ("remove-specifier", Fremove_specifier, 1, 4, 0, /*
Remove specification(s) for SPECIFIER.

If LOCALE is a particular locale (a window, buffer, frame, device,
or `global'), the specification for that locale will be removed.

If instead, LOCALE is a locale type (i.e. `window', `buffer', `frame',
or `device'), the specifications for all locales of that type will be
removed.

If LOCALE is nil or `all', all specifications will be removed.

LOCALE can also be a list of locales, locale types, and/or `all'; this
is equivalent to calling `remove-specifier' for each of the elements
in the list.

Only instantiators where TAG-SET (a list of zero or more tags) is a
subset of (or possibly equal to) the instantiator's tag set are removed.
The default value of nil is a subset of all tag sets, so in this case
no instantiators will be screened out. If EXACT-P is non-nil, however,
TAG-SET must be equal to an instantiator's tag set for the instantiator
to be removed.
*/
       (specifier, locale, tag_set, exact_p))
{
  CHECK_SPECIFIER (specifier);
  check_modifiable_specifier (specifier);

  map_specifier (specifier, locale, remove_specifier_mapfun,
		 tag_set, exact_p, 0);
  recompute_cached_specifier_everywhere (specifier);
  return Qnil;
}

void
remove_ghost_specifier (Lisp_Object specifier, Lisp_Object locale,
			Lisp_Object tag_set, Lisp_Object exact_p)
{
  int depth = unlock_ghost_specifiers_protected ();
  Fremove_specifier (XSPECIFIER (specifier)->fallback,
		     locale, tag_set, exact_p);
  unbind_to (depth);
}

struct copy_specifier_closure
{
  Lisp_Object dest;
  enum spec_add_meth add_meth;
  int add_meth_is_nil;
};

static int
copy_specifier_mapfun (Lisp_Object specifier,
		       Lisp_Object locale,
		       enum spec_locale_type locale_type,
		       Lisp_Object tag_set,
		       int exact_p,
		       void *closure)
{
  struct copy_specifier_closure *cl =
    (struct copy_specifier_closure *) closure;

  if (NILP (locale))
    specifier_copy_locale_type (specifier, cl->dest, locale_type,
				tag_set, exact_p,
				cl->add_meth_is_nil ?
				SPEC_REMOVE_LOCALE_TYPE :
				cl->add_meth);
  else
    specifier_copy_spec (specifier, cl->dest, locale, locale_type,
			 tag_set, exact_p,
			 cl->add_meth_is_nil ? SPEC_REMOVE_LOCALE :
			 cl->add_meth);
  return 0;
}

DEFUN ("copy-specifier", Fcopy_specifier, 1, 6, 0, /*
Copy SPECIFIER to DEST, or create a new one if DEST is nil.

If DEST is nil or omitted, a new specifier will be created and the
specifications copied into it.  Otherwise, the specifications will be
copied into the existing specifier in DEST.

If LOCALE is nil or `all', all specifications will be copied.  If LOCALE
is a particular locale, the specification for that particular locale will
be copied.  If LOCALE is a locale type, the specifications for all locales
of that type will be copied.  LOCALE can also be a list of locales,
locale types, and/or `all'; this is equivalent to calling `copy-specifier'
for each of the elements of the list.  See `specifier-spec-list' for more
information about LOCALE.

Only instantiators where TAG-SET (a list of zero or more tags) is a
subset of (or possibly equal to) the instantiator's tag set are copied.
The default value of nil is a subset of all tag sets, so in this case
no instantiators will be screened out. If EXACT-P is non-nil, however,
TAG-SET must be equal to an instantiator's tag set for the instantiator
to be copied.

Optional argument HOW-TO-ADD specifies what to do with existing
specifications in DEST.  If nil, then whichever locales or locale types
are copied will first be completely erased in DEST.  Otherwise, it is
the same as in `add-spec-to-specifier'.
*/
       (specifier, dest, locale, tag_set, exact_p, how_to_add))
{
  struct gcpro gcpro1;
  struct copy_specifier_closure cl;

  CHECK_SPECIFIER (specifier);
  if (NILP (how_to_add))
    cl.add_meth_is_nil = 1;
  else
    cl.add_meth_is_nil = 0;
  cl.add_meth = decode_how_to_add_specification (how_to_add);
  if (NILP (dest))
    {
      /* #### What about copying the extra data? */
      dest = make_specifier (XSPECIFIER (specifier)->methods);
    }
  else
    {
      CHECK_SPECIFIER (dest);
      check_modifiable_specifier (dest);
      if (XSPECIFIER (dest)->methods != XSPECIFIER (specifier)->methods)
	invalid_argument ("Specifiers not of same type", Qunbound);
    }

  cl.dest = dest;
  GCPRO1 (dest);
  map_specifier (specifier, locale, copy_specifier_mapfun,
		 tag_set, exact_p, &cl);
  UNGCPRO;
  recompute_cached_specifier_everywhere (dest);
  return dest;
}


/************************************************************************/
/*                              Instantiation                              */
/************************************************************************/

static Lisp_Object
call_validate_matchspec_method (Lisp_Object boxed_method,
				Lisp_Object matchspec)
{
  ((void (*)(Lisp_Object)) get_opaque_ptr (boxed_method)) (matchspec);
  return Qt;
}

static Lisp_Object
check_valid_specifier_matchspec (Lisp_Object matchspec,
				 struct specifier_methods *meths,
				 Error_Behavior errb)
{
  if (meths->validate_matchspec_method)
    {
      Lisp_Object retval;

      if (ERRB_EQ (errb, ERROR_ME))
	{
	  (meths->validate_matchspec_method) (matchspec);
	  retval = Qt;
	}
      else
	{
	  Lisp_Object opaque =
	    make_opaque_ptr ((void *) meths->validate_matchspec_method);
	  struct gcpro gcpro1;

	  GCPRO1 (opaque);
	  retval = call_with_suspended_errors
	    ((lisp_fn_t) call_validate_matchspec_method,
	     Qnil, Qspecifier, errb, 2, opaque, matchspec);

	  free_opaque_ptr (opaque);
	  UNGCPRO;
	}

      return retval;
    }
  else
    {
      maybe_sferror
	("Matchspecs not allowed for this specifier type",
	 intern (meths->name), Qspecifier, errb);
      return Qnil;
    }
}

DEFUN ("check-valid-specifier-matchspec", Fcheck_valid_specifier_matchspec, 2,
       2, 0, /*
Signal an error if MATCHSPEC is invalid for SPECIFIER-TYPE.
See `specifier-matching-instance' for a description of matchspecs.
*/
       (matchspec, specifier_type))
{
  struct specifier_methods *meths = decode_specifier_type (specifier_type,
							   ERROR_ME);

  return check_valid_specifier_matchspec (matchspec, meths, ERROR_ME);
}

DEFUN ("valid-specifier-matchspec-p", Fvalid_specifier_matchspec_p, 2, 2, 0, /*
Return non-nil if MATCHSPEC is valid for SPECIFIER-TYPE.
See `specifier-matching-instance' for a description of matchspecs.
*/
       (matchspec, specifier_type))
{
  struct specifier_methods *meths = decode_specifier_type (specifier_type,
							   ERROR_ME);

  return check_valid_specifier_matchspec (matchspec, meths, ERROR_ME_NOT);
}

/* This function is purposely not callable from Lisp.  If a Lisp
   caller wants to set a fallback, they should just set the
   global value. */

void
set_specifier_fallback (Lisp_Object specifier, Lisp_Object fallback)
{
  Lisp_Specifier *sp = XSPECIFIER (specifier);
  assert (SPECIFIERP (fallback) ||
	  !NILP (Fvalid_inst_list_p (fallback, Fspecifier_type (specifier))));
  if (SPECIFIERP (fallback))
    assert (EQ (Fspecifier_type (specifier), Fspecifier_type (fallback)));
  if (BODILY_SPECIFIER_P (sp))
    GHOST_SPECIFIER (sp)->fallback = fallback;
  else
    sp->fallback = fallback;
  /* call the after-change method */
  MAYBE_SPECMETH (sp, after_change,
		  (bodily_specifier (specifier), Qfallback));
  recompute_cached_specifier_everywhere (specifier);
}

DEFUN ("specifier-fallback", Fspecifier_fallback, 1, 1, 0, /*
Return the fallback value for SPECIFIER.
Fallback values are provided by the C code for certain built-in
specifiers to make sure that instantiation won't fail even if all
specs are removed from the specifier, or to implement simple
inheritance behavior (e.g. this method is used to ensure that
faces other than `default' inherit their attributes from `default').
By design, you cannot change the fallback value, and specifiers
created with `make-specifier' will never have a fallback (although
a similar, Lisp-accessible capability may be provided in the future
to allow for inheritance).

The fallback value will be an inst-list that is instantiated like
any other inst-list, a specifier of the same type as SPECIFIER
\(results in inheritance), or nil for no fallback.

When you instantiate a specifier, you can explicitly request that the
fallback not be consulted. (The C code does this, for example, when
merging faces.) See `specifier-instance'.
*/
       (specifier))
{
  CHECK_SPECIFIER (specifier);
  return Fcopy_tree (XSPECIFIER (specifier)->fallback, Qt);
}

static Lisp_Object
specifier_instance_from_inst_list (Lisp_Object specifier,
				   Lisp_Object matchspec,
				   Lisp_Object domain,
				   Lisp_Object inst_list,
				   Error_Behavior errb, int no_quit,
				   Lisp_Object depth,
				   Lisp_Object *instantiator,
				   int no_fallback)
{
  /* This function can GC */
  Lisp_Specifier *sp;
  Lisp_Object device, charset = Qnil, rest;
  int count = specpdl_depth (), respected_charsets = 0;
  struct gcpro gcpro1, gcpro2;
  enum font_specifier_matchspec_stages stage = STAGE_INITIAL;

  GCPRO2 (specifier, inst_list);

  sp = XSPECIFIER (specifier);
  device = DOMAIN_DEVICE (domain);

  if (no_quit)
    /* The instantiate method is allowed to call eval.  Since it
       is quite common for this function to get called from somewhere in
       redisplay we need to make sure that quits are ignored.  Otherwise
       Fsignal will abort. */
    specbind (Qinhibit_quit, Qt);

#ifdef MULE
  /* #### FIXME Does this font-specific stuff need to be here and not in
     the font-specifier-specific code? --ben */
  if (CONSP (matchspec) && (CHARSETP (Ffind_charset (XCAR (matchspec)))))
    {
      charset = Ffind_charset (XCAR (matchspec));

#ifdef DEBUG_XEMACS
      /* This is mostly to have somewhere to set debug breakpoints. */
      if (!EQ (charset, Vcharset_ascii))
	{
	  (void) 0;
	}
#endif /* DEBUG_XEMACS */

      if (!NILP (XCDR (matchspec)))
	{

#define FROB(new_stage, enumstage)			\
          if (EQ (Q##new_stage, XCDR (matchspec)))	\
	    {						\
	      stage = enumstage;			\
	    }

	  FROB (initial, STAGE_INITIAL)
	  else FROB (final, STAGE_FINAL)
	  else assert (0);
#undef FROB

	}
    }
#endif /* MULE */

  LIST_LOOP (rest, inst_list)
    {
      Lisp_Object tagged_inst = XCAR (rest);
      Lisp_Object tag_set = XCAR (tagged_inst);
      Lisp_Object val, the_instantiator;

      if (!device_matches_specifier_tag_set_p (device, tag_set))
	{
	  continue;
	}

      val = XCDR (tagged_inst);
      the_instantiator = val;

      if (!NILP (charset) &&
	  !(charset_matches_specifier_tag_set_p (charset, tag_set, stage)))
	{
	  ++respected_charsets;
	  continue;
	}

      if (HAS_SPECMETH_P (sp, instantiate))
	val = call_with_suspended_errors
	  ((lisp_fn_t) RAW_SPECMETH (sp, instantiate),
	   Qunbound, Qspecifier, ERROR_ME_WARN, 5, specifier,
	   matchspec, domain, val, depth, no_fallback);

      if (!UNBOUNDP (val))
	{
	  unbind_to (count);
	  UNGCPRO;
	  if (instantiator)
	    *instantiator = the_instantiator;
	  return val;
	}
    }

  /* We've checked all the tag sets, and checking the charset part of the
     specifier never returned 0 (preventing the attempted instantiation), so
     there's no need to loop for the second time to avoid checking the
     charsets. */
  if (!respected_charsets)
    {
      unbind_to (count);
      UNGCPRO;
      return Qunbound;
    }

  /* Right, didn't instantiate a specifier last time, perhaps because we
     paid attention to the charset-specific aspects of the specifier.  Try
     again without checking the charset information.

     We can't emulate the approach for devices, defaulting to matching all
     character sets for a given specifier, because $random font instantiator
     cannot usefully show all character sets, and indeed having it try is a
     failure on our part.  */
  LIST_LOOP (rest, inst_list)
    {
      Lisp_Object tagged_inst = XCAR (rest);
      Lisp_Object tag_set = XCAR (tagged_inst);
      Lisp_Object val, the_instantiator;

      if (!device_matches_specifier_tag_set_p (device, tag_set))
	{
	  continue;
	}

      val = XCDR (tagged_inst);
      the_instantiator = val;

      if (HAS_SPECMETH_P (sp, instantiate))
	val = call_with_suspended_errors
	  ((lisp_fn_t) RAW_SPECMETH (sp, instantiate),
	   Qunbound, Qspecifier, errb, 5, specifier,
	   matchspec, domain, val, depth, no_fallback);

      if (!UNBOUNDP (val))
	{
	  unbind_to (count);
	  UNGCPRO;
	  if (instantiator)
	    *instantiator = the_instantiator;
	  return val;
	}
    }

  unbind_to (count);
  UNGCPRO;
  return Qunbound;
}

/* Given a SPECIFIER and a DOMAIN, return a specific instance for that
   specifier. Try to find one by checking the specifier types from most
   specific (window) to most general (global).  If we find an instance,
   return it.  Otherwise return Qunbound. */

#define CHECK_INSTANCE_ENTRY(key, matchspec, type) do {			\
    Lisp_Object *CIE_inst_list =					\
      specifier_get_inst_list (specifier, key, type);			\
    if (CIE_inst_list)							\
      {									\
	Lisp_Object CIE_val =						\
	  specifier_instance_from_inst_list (specifier, matchspec,	\
					     domain, *CIE_inst_list,	\
					     errb, no_quit, depth,	\
					     instantiator, no_fallback); \
	if (!UNBOUNDP (CIE_val))					\
	  return CIE_val;						\
      }									\
  } while (0)

/* We accept any window, frame or device domain and do our checking
   starting from as specific a locale type as we can determine from the
   domain we are passed and going on up through as many other locale types
   as we can determine.  In practice, when called from redisplay the
   arg will usually be a window and occasionally a frame.  If
   triggered by a user call, who knows what it will usually be. */

static Lisp_Object
specifier_instance_1 (Lisp_Object specifier, Lisp_Object matchspec,
		      Lisp_Object domain, Error_Behavior errb, int no_quit,
		      int no_fallback, Lisp_Object depth,
		      Lisp_Object *instantiator)
{
  Lisp_Object buffer = Qnil;
  Lisp_Object window = Qnil;
  Lisp_Object frame = Qnil;
  Lisp_Object device = Qnil;
  Lisp_Specifier *sp = XSPECIFIER (specifier);

  if (instantiator)
    *instantiator = Qunbound;

  /* Attempt to determine buffer, window, frame, and device from the
     domain. */
  /* #### get image instances out of domains! */
  if (IMAGE_INSTANCEP (domain))
    window = DOMAIN_WINDOW (domain);
  else if (WINDOWP (domain))
    window = domain;
  else if (FRAMEP (domain))
    frame = domain;
  else if (DEVICEP (domain))
    device = domain;
  else
    /* dmoore writes: [dammit, this should just signal an error or something
       shouldn't it?]

       No. Errors are handled in Lisp primitives implementation.
       Invalid domain is a design error here - kkm. */
    ABORT ();

  if (NILP (buffer) && !NILP (window))
    buffer = WINDOW_BUFFER (XWINDOW (window));
  if (NILP (frame) && !NILP (window))
    frame = XWINDOW (window)->frame;
  if (NILP (device))
    /* frame had better exist; if device is undeterminable, something
       really went wrong. */
    device = FRAME_DEVICE (XFRAME (frame));

  /* device had better be determined by now; abort if not. */
  (void) DEVICE_CLASS (XDEVICE (device));

  depth = make_fixnum (1 + XFIXNUM (depth));
  if (XFIXNUM (depth) > 20)
    {
      maybe_signal_error (Qstack_overflow,
			  "Apparent loop in specifier inheritance",
			  Qunbound, Qspecifier, errb);
      /* The specification is fucked; at least try the fallback
	 (which better not be fucked, because it's not changeable
	 from Lisp). */
      depth = Qzero;
      goto do_fallback;
    }

 retry:
  /* First see if we can generate one from the window specifiers. */
  if (!NILP (window))
    CHECK_INSTANCE_ENTRY (window, matchspec, LOCALE_WINDOW);

  /* Next see if we can generate one from the buffer specifiers. */
  if (!NILP (buffer))
    CHECK_INSTANCE_ENTRY (buffer, matchspec, LOCALE_BUFFER);

  /* Next see if we can generate one from the frame specifiers. */
  if (!NILP (frame))
    CHECK_INSTANCE_ENTRY (frame, matchspec, LOCALE_FRAME);

  /* If we still haven't succeeded try with the device specifiers. */
  CHECK_INSTANCE_ENTRY (device, matchspec, LOCALE_DEVICE);

  /* Last and least try the global specifiers. */
  CHECK_INSTANCE_ENTRY (Qglobal, matchspec, LOCALE_GLOBAL);

 do_fallback:
  /* We're out of specifiers and we still haven't generated an
     instance.  At least try the fallback ...  If this fails,
     then we just return Qunbound. */

  if (no_fallback || NILP (sp->fallback))
    /* I said, I don't want the fallbacks. */
    return Qunbound;

  if (SPECIFIERP (sp->fallback))
    {
      /* If you introduced loops in the default specifier chain,
	 then you're fucked, so you better not do this. */
      specifier = sp->fallback;
      sp = XSPECIFIER (specifier);
      goto retry;
    }

  assert (CONSP (sp->fallback));
  return specifier_instance_from_inst_list (specifier, matchspec, domain,
					    sp->fallback, errb, no_quit,
					    depth, instantiator,
					    no_fallback);
}
#undef CHECK_INSTANCE_ENTRY

Lisp_Object
specifier_instance (Lisp_Object specifier, Lisp_Object matchspec,
		    Lisp_Object domain, Error_Behavior errb, int no_quit,
		    int no_fallback, Lisp_Object depth)
{
  return specifier_instance_1 (specifier, matchspec, domain, errb,
			       no_quit, no_fallback, depth, NULL);
}

Lisp_Object
specifier_instance_no_quit (Lisp_Object specifier, Lisp_Object matchspec,
			    Lisp_Object domain, Error_Behavior errb,
			    int no_fallback, Lisp_Object depth)
{
  return specifier_instance_1 (specifier, matchspec, domain, errb,
			       1, no_fallback, depth, NULL);
}

static Lisp_Object
specifier_matching_foo (Lisp_Object specifier,
			Lisp_Object matchspec,
			Lisp_Object domain,
			Lisp_Object default_,
			Lisp_Object no_fallback,
			int want_instantiator)
{
  Lisp_Object instance, instantiator;

  CHECK_SPECIFIER (specifier);
  if (!UNBOUNDP (matchspec))
    check_valid_specifier_matchspec (matchspec,
				     XSPECIFIER (specifier)->methods,
				     ERROR_ME);
  domain = decode_domain (domain);

  instance = specifier_instance_1 (specifier, matchspec, domain, ERROR_ME,
				   0, !NILP (no_fallback), Qzero,
				   &instantiator);
  return UNBOUNDP (instance) ? default_ : want_instantiator ? instantiator :
    instance;
}

DEFUN ("specifier-instance", Fspecifier_instance, 1, 4, 0, /*
Instantiate SPECIFIER (return its value) in DOMAIN.
If no instance can be generated for this domain, return DEFAULT.

DOMAIN is nearly always a window (defaulting to the selected window if
omitted), but can be a window, frame, or device.  Other values that are legal
as a locale (e.g. a buffer) are not valid as a domain because they do not
provide enough information to identify a particular device (see
`valid-specifier-domain-p').  Window domains are used internally in nearly
all circumstances when computing specifier instances of display properties.
Frame domains are used in a few circumstances (such as when computing the
geometry of a frame based on properties such as the toolbar widths), and
device domains are rarely if ever used internally.

This function looks through the specifications in SPECIFIER that correspond
to DOMAIN, from most specific (specifications for DOMAIN itself) to most
general (global specifications), for matching instantiators, and attempts
to compute an instance value for each instantiator found.  The first
successfully computed value is returned.  The corresponding instantiator
can be returned using `specifier-instantiator'.

A specifier is a generalized object for controlling the value of a property --
typically, but not necessarily, a display-related property -- that can vary
over particular buffers, frames, device types, etc.

A fundamental distinction must be made between the specification of a
property's value, and the resulting value itself.  This distinction is
clearest in the case of an image -- the specification describes the source
of the image (for example, a file of JPEG data), and the resulting value
encapsulates a window-system object describing the image as displayed on a
particular device (for example, a particular X display).  The specification
might also be an instruction of the form "use the background pixmap of the
`modeline' face".  A similar mapping exists between color strings and
color-instance objects, and font strings and font-instance objects.  In
some cases, the specification and the resulting value are of the same type,
but the distinction is still logically made.

The specification of a value is called an instantiator, and the resulting
value the instance.

"Instantiating" a specifier in a particular domain means determining
the specifier's "value" in that domain.  This is accomplished by
searching through the specifications in the specifier that correspond
to all locales that can be derived from the given domain, from specific
to general.  In most cases, the domain is an Emacs window.  In that case
specifications are searched for as follows:

1. A specification whose locale is the window itself;
2. A specification whose locale is the window's buffer;
3. A specification whose locale is the window's frame;
4. A specification whose locale is the window's frame's device;
5. A specification whose locale is `global'.

If all of those fail, then the C-code-provided fallback value for
this specifier is consulted (see `specifier-fallback').  If it is
an inst-list, then this function attempts to instantiate that list
just as when a specification is located in the first five steps above.
If the fallback is a specifier, `specifier-instance' is called
recursively on this specifier and the return value used.  Note,
however, that if the optional argument NO-FALLBACK is non-nil,
the fallback value will not be consulted.

Note that there may be more than one specification matching a particular
locale; all such specifications are considered before looking for any
specifications for more general locales.  Any particular specification
that is found may be rejected because its tag set does not match the
device being instantiated over, or because the specification is not
valid for the device of the given domain (e.g. the font or color name
does not exist for this particular X server).

NOTE: When errors occur in the process of trying a particular instantiator,
and the instantiator is thus skipped, warnings will be issued at level
`debug'.  Normally, such warnings are ignored entirely, but you can change
this by setting `log-warning-minimum-level'.  This is useful if you're
trying to debug why particular instantiators are not being processed.

The returned value is dependent on the type of specifier.  For example,
for a font specifier (as returned by the `face-font' function), the returned
value will be a font-instance object.  For glyphs, the returned value
will be an image-instance object.

See also `specifier-matching-instance'.
*/
       (specifier, domain, default_, no_fallback))
{
  return specifier_matching_foo (specifier, Qunbound, domain, default_,
				 no_fallback, 0);
}

DEFUN ("specifier-instantiator", Fspecifier_instantiator, 1, 4, 0, /*
Return instantiator that would be used to instantiate SPECIFIER in DOMAIN.
If no instance can be generated for this domain, return DEFAULT.

DOMAIN should be a window, frame, or device.  Other values that are legal
as a locale (e.g. a buffer) are not valid as a domain because they do not
provide enough information to identify a particular device (see
`valid-specifier-domain-p').  DOMAIN defaults to the selected window
if omitted.

See `specifier-instance' for more information about the instantiation process.
*/
       (specifier, domain, default_, no_fallback))
{
  return specifier_matching_foo (specifier, Qunbound, domain, default_,
				 no_fallback, 1);
}

/* MATCHSPEC is backward-incompatible with code written to 21.4's API.
   So far such code has been seen only in x-symbol-mule.el, and that
   was addressed by a change `face-property-matching-instance'.
   See tracker issue752 for a more general patch against 21.5.29. */
DEFUN ("specifier-matching-instance", Fspecifier_matching_instance, 2, 5, 0, /*
Return an instance for SPECIFIER in DOMAIN that matches MATCHSPEC.
If no instance can be generated for this domain, return DEFAULT.

This function is identical to `specifier-instance' except that a
specification will only be considered if it matches MATCHSPEC.
The definition of "match", and allowed values for MATCHSPEC, are
dependent on the particular type of specifier.  Here are some examples:

-- For chartable (e.g. display table) specifiers, MATCHSPEC should be a
   character, and the specification (a chartable) must give a value for
   that character in order to be considered.  This allows you to specify,
   e.g., a buffer-local display table that only gives values for particular
   characters.  All other characters are handled as if the buffer-local
   display table is not there. (Chartable specifiers are not yet
   implemented.)

-- For font specifiers, MATCHSPEC should be a cons (CHARSET . STAGE).
   The defined stages are currently `initial' and `final'.  On X11, 'initial
   is used when the font matching process is looking for fonts that match
   the desired registries of the charset--see the `charset-registries'
   function.  If that match process fails, then the 'final stage comes into
   play; this means that a more general lookup is desired, and that a font
   doesn't necessarily have to match the desired XLFD for the face, just the
   charset repertoire for this charset.  It also means that the charset
   registry and encoding used will be `iso10646-1', and the characters will
   be converted to display using that registry.

   See `define-specifier-tag' for details on how to create a tag that
   specifies a given character set and stage combination.  You can supply
   such a tag to `set-face-font' in order to set a face's font for that
   character set and stage combination.
*/
       (specifier, matchspec, domain, default_, no_fallback))
{
  return specifier_matching_foo (specifier, matchspec, domain, default_,
				 no_fallback, 0);
}

DEFUN ("specifier-matching-instantiator", Fspecifier_matching_instantiator,
       2, 5, 0, /*
Return instantiator for instance of SPECIFIER in DOMAIN that matches MATCHSPEC.
If no instance can be generated for this domain, return DEFAULT.

This function is identical to `specifier-matching-instance' but returns
the instantiator used to generate the instance, rather than the actual
instance.
*/
       (specifier, matchspec, domain, default_, no_fallback))
{
  return specifier_matching_foo (specifier, matchspec, domain, default_,
				 no_fallback, 1);
}

static Lisp_Object
specifier_matching_foo_from_inst_list (Lisp_Object specifier,
				       Lisp_Object matchspec,
				       Lisp_Object domain,
				       Lisp_Object inst_list,
				       Lisp_Object default_,
				       int want_instantiator)
{
  Lisp_Object val = Qunbound;
  Lisp_Specifier *sp = XSPECIFIER (specifier);
  struct gcpro gcpro1;
  Lisp_Object built_up_list = Qnil;
  Lisp_Object instantiator;

  CHECK_SPECIFIER (specifier);
  if (!UNBOUNDP (matchspec))
    check_valid_specifier_matchspec (matchspec,
				     XSPECIFIER (specifier)->methods,
				     ERROR_ME);
  check_valid_domain (domain);
  check_valid_inst_list (inst_list, sp->methods, ERROR_ME);
  GCPRO1 (built_up_list);
  built_up_list = build_up_processed_list (specifier, domain, inst_list);
  if (!NILP (built_up_list))
    val = specifier_instance_from_inst_list (specifier, matchspec, domain,
					     built_up_list, ERROR_ME,
					     0, Qzero, &instantiator, 0);
  UNGCPRO;
  return UNBOUNDP (val) ? default_ : want_instantiator ? instantiator : val;

}

DEFUN ("specifier-instance-from-inst-list", Fspecifier_instance_from_inst_list,
       3, 4, 0, /*
Attempt to convert a particular inst-list into an instance.
This attempts to instantiate INST-LIST in the given DOMAIN,
as if INST-LIST existed in a specification in SPECIFIER.  If
the instantiation fails, DEFAULT is returned.  In most circumstances,
you should not use this function; use `specifier-instance' instead.
*/
       (specifier, domain, inst_list, default_))
{
  return specifier_matching_foo_from_inst_list (specifier, Qunbound,
						domain, inst_list, default_,
						0);
}

DEFUN ("specifier-instantiator-from-inst-list",
       Fspecifier_instantiator_from_inst_list, 3, 4, 0, /*
Attempt to convert an inst-list into an instance; return instantiator.
This is identical to `specifier-instance-from-inst-list' but returns
the instantiator used to generate the instance, rather than the instance
itself.
*/
       (specifier, domain, inst_list, default_))
{
  return specifier_matching_foo_from_inst_list (specifier, Qunbound,
						domain, inst_list, default_,
						1);
}

DEFUN ("specifier-matching-instance-from-inst-list",
       Fspecifier_matching_instance_from_inst_list,
       4, 5, 0, /*
Attempt to convert a particular inst-list into an instance.
This attempts to instantiate INST-LIST in the given DOMAIN
\(as if INST-LIST existed in a specification in SPECIFIER),
matching the specifications against MATCHSPEC.

This function is analogous to `specifier-instance-from-inst-list'
but allows for specification-matching as in `specifier-matching-instance'.
See that function for a description of exactly how the matching process
works.
*/
       (specifier, matchspec, domain, inst_list, default_))
{
  return specifier_matching_foo_from_inst_list (specifier, matchspec,
						domain, inst_list, default_,
						0);
}

DEFUN ("specifier-matching-instantiator-from-inst-list",
       Fspecifier_matching_instantiator_from_inst_list,
       4, 5, 0, /*
Attempt to convert an inst-list into an instance; return instantiator.
This is identical to `specifier-matching-instance-from-inst-list' but returns
the instantiator used to generate the instance, rather than the instance
itself.
*/
       (specifier, matchspec, domain, inst_list, default_))
{
  return specifier_matching_foo_from_inst_list (specifier, matchspec,
						domain, inst_list, default_,
						1);
}


/************************************************************************/
/*                 Caching in the struct window or frame                */
/************************************************************************/

/* Cause the current value of SPECIFIER in the domain of each frame and/or
   window to be cached in the struct frame at STRUCT_FRAME_OFFSET and the
   struct window at STRUCT_WINDOW_OFFSET.  When the value changes in a
   particular window, VALUE_CHANGED_IN_WINDOW is called.  When the value
   changes in a particular frame, VALUE_CHANGED_IN_FRAME is called.

   Either STRUCT_WINDOW_OFFSET or STRUCT_FRAME_OFFSET can be 0 to indicate
   no caching in that sort of object.  However, if they're not 0, you
   must supply a corresponding value-changed function. (This is the case
   so that you are forced to consider the ramifications of a value change.
   You nearly always need to do something, e.g. set a dirty flag.)

   If you create a built-in specifier, you should do the following:

   - Make sure the file you create the specifier in has a
   specifier_vars_of_foo() function.  If not, create it, declare it in
   symsinit.h, and make sure it's called in the appropriate place in
   emacs.c.
   - In specifier_vars_of_foo(), do a DEFVAR_SPECIFIER(), followed by
   initializing the specifier using Fmake_specifier(), followed by
   set_specifier_fallback(), followed (optionally) by
   set_specifier_caching().
   - If you used set_specifier_caching(), make sure to create the
   appropriate value-changed functions.  Also make sure to add the
   appropriate slots where the values are cached to frameslots.h and
   winslots.h.

   Do a grep for menubar_visible_p for an example.
*/

/* #### It would be nice if the specifier caching automatically knew
   about specifier fallbacks, so we didn't have to do it ourselves. */

void
set_specifier_caching (Lisp_Object specifier, int struct_window_offset,
		       void (*value_changed_in_window)
		       (Lisp_Object specifier, struct window *w,
			Lisp_Object oldval),
		       int struct_frame_offset,
		       void (*value_changed_in_frame)
		       (Lisp_Object specifier, struct frame *f,
			Lisp_Object oldval),
		       int always_recompute)
{
  Lisp_Specifier *sp = XSPECIFIER (specifier);
  assert (!GHOST_SPECIFIER_P (sp));

  if (!sp->caching)
#ifdef NEW_GC
    sp->caching = XSPECIFIER_CACHING (ALLOC_NORMAL_LISP_OBJECT (specifier_caching));
#else /* not NEW_GC */
  sp->caching = xnew_and_zero (struct specifier_caching);
#endif /* not NEW_GC */
  sp->caching->offset_into_struct_window = struct_window_offset;
  sp->caching->value_changed_in_window = value_changed_in_window;
  sp->caching->offset_into_struct_frame = struct_frame_offset;
  sp->caching->value_changed_in_frame = value_changed_in_frame;
  if (struct_window_offset)
    assert (value_changed_in_window);
  if (struct_frame_offset)
    assert (value_changed_in_frame);
  sp->caching->always_recompute = always_recompute;
  Vcached_specifiers = Fcons (specifier, Vcached_specifiers);
  if (BODILY_SPECIFIER_P (sp))
    GHOST_SPECIFIER (sp)->caching = sp->caching;
  recompute_cached_specifier_everywhere (specifier);
}

static void
recompute_one_cached_specifier_in_window (Lisp_Object specifier,
					  struct window *w)
{
  Lisp_Object window;
  Lisp_Object newval, *location, oldval;

  assert (!GHOST_SPECIFIER_P (XSPECIFIER (specifier)));

  window = wrap_window (w);

  newval = specifier_instance (specifier, Qunbound, window, ERROR_ME_WARN,
			       0, 0, Qzero);
  /* If newval ended up Qunbound, then the calling functions
     better be able to deal.  If not, set a default so this
     never happens or correct it in the value_changed_in_window
     method. */
  location = (Lisp_Object *)
    ((char *) w + XSPECIFIER (specifier)->caching->offset_into_struct_window);
  /* #### What's the point of this check, other than to optimize image
     instance instantiation? Unless you specify a caching instantiate
     method the instantiation that specifier_instance will do will
     always create a new copy. Thus EQ will always fail. Unfortunately
     calling equal is no good either as this doesn't take into account
     things attached to the specifier - for instance strings on
     extents. --andyp */
  if (!EQ (newval, *location) || XSPECIFIER (specifier)->caching->always_recompute)
    {
      oldval = *location;
      *location = newval;
      (XSPECIFIER (specifier)->caching->value_changed_in_window)
	(specifier, w, oldval);
    }
}

static void
recompute_one_cached_specifier_in_frame (Lisp_Object specifier,
					 struct frame *f)
{
  Lisp_Object frame;
  Lisp_Object newval, *location, oldval;

  assert (!GHOST_SPECIFIER_P (XSPECIFIER (specifier)));

  frame = wrap_frame (f);

  newval = specifier_instance (specifier, Qunbound, frame, ERROR_ME_WARN,
			       0, 0, Qzero);
  /* If newval ended up Qunbound, then the calling functions
     better be able to deal.  If not, set a default so this
     never happens or correct it in the value_changed_in_frame
     method. */
  location = (Lisp_Object *)
    ((char *) f + XSPECIFIER (specifier)->caching->offset_into_struct_frame);
  if (!EQ (newval, *location) || XSPECIFIER (specifier)->caching->always_recompute)
    {
      oldval = *location;
      *location = newval;
      (XSPECIFIER (specifier)->caching->value_changed_in_frame)
	(specifier, f, oldval);
    }
}

void
recompute_all_cached_specifiers_in_window (struct window *w)
{
  Lisp_Object rest;

  LIST_LOOP (rest, Vcached_specifiers)
    {
      Lisp_Object specifier = XCAR (rest);
      if (XSPECIFIER (specifier)->caching->offset_into_struct_window)
	recompute_one_cached_specifier_in_window (specifier, w);
    }
}

void
recompute_all_cached_specifiers_in_frame (struct frame *f)
{
  Lisp_Object rest;

  LIST_LOOP (rest, Vcached_specifiers)
    {
      Lisp_Object specifier = XCAR (rest);
      if (XSPECIFIER (specifier)->caching->offset_into_struct_frame)
	recompute_one_cached_specifier_in_frame (specifier, f);
    }
}

static int
recompute_cached_specifier_everywhere_mapfun (struct window *w,
					      void *closure)
{
  Lisp_Object specifier = Qnil;

  specifier = GET_LISP_FROM_VOID (closure);
  recompute_one_cached_specifier_in_window (specifier, w);
  return 0;
}

static void
recompute_cached_specifier_everywhere (Lisp_Object specifier)
{
  Lisp_Object frmcons, devcons, concons;

  specifier = bodily_specifier (specifier);

  if (!XSPECIFIER (specifier)->caching)
    return;

  if (XSPECIFIER (specifier)->caching->offset_into_struct_window)
    {
      FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	map_windows (XFRAME (XCAR (frmcons)),
		     recompute_cached_specifier_everywhere_mapfun,
		     STORE_LISP_IN_VOID (specifier));
    }

  if (XSPECIFIER (specifier)->caching->offset_into_struct_frame)
    {
      FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	recompute_one_cached_specifier_in_frame (specifier,
						 XFRAME (XCAR (frmcons)));
    }
}

DEFUN ("set-specifier-dirty-flag", Fset_specifier_dirty_flag, 1, 1, 0, /*
Force recomputation of any caches associated with SPECIFIER.
Note that this automatically happens whenever you change a specification
 in SPECIFIER; you do not have to call this function then.
One example of where this function is useful is when you have a
 toolbar button whose `active-p' field is an expression to be
 evaluated.  Calling `set-specifier-dirty-flag' on the
 toolbar specifier will force the `active-p' fields to be
 recomputed.
*/
       (specifier))
{
  CHECK_SPECIFIER (specifier);
  recompute_cached_specifier_everywhere (specifier);
  return Qnil;
}


/************************************************************************/
/*                        Generic specifier type                        */
/************************************************************************/

DEFINE_SPECIFIER_TYPE (generic);

#if 0

/* This is the string that used to be in `generic-specifier-p'.
   The idea is good, but it doesn't quite work in the form it's
   in. (One major problem is that validating an instantiator
   is supposed to require only that the specifier type is passed,
   while with this approach the actual specifier is needed.)

   What really needs to be done is to write a function
   `make-specifier-type' that creates new specifier types.

   #### [I'll look into this for 19.14.]  Well, sometime. (Currently
   May 2000, 21.2 is in development.  19.14 was released in June 1996.) */

"A generic specifier is a generalized kind of specifier with user-defined\n"
"semantics.  The instantiator can be any kind of Lisp object, and the\n"
"instance computed from it is likewise any kind of Lisp object.  The\n"
"SPECIFIER-DATA should be an alist of methods governing how the specifier\n"
"works.  All methods are optional, and reasonable default methods will be\n"
"provided.  Currently there are two defined methods: `instantiate' and\n"
"`validate'.\n"
"\n"
"`instantiate' specifies how to do the instantiation; if omitted, the\n"
"instantiator itself is simply returned as the instance.  The method\n"
"should be a function that accepts three parameters (a specifier, the\n"
"instantiator that matched the domain being instantiated over, and that\n"
"domain), and should return a one-element list containing the instance,\n"
"or nil if no instance exists.  Note that the domain passed to this function\n"
"is the domain being instantiated over, which may not be the same as the\n"
"locale contained in the specification corresponding to the instantiator\n"
"(for example, the domain being instantiated over could be a window, but\n"
"the locale corresponding to the passed instantiator could be the window's\n"
"buffer or frame).\n"
"\n"
"`validate' specifies whether a given instantiator is valid; if omitted,\n"
"all instantiators are considered valid.  It should be a function of\n"
"two arguments: an instantiator and a flag CAN-SIGNAL-ERROR.  If this\n"
"flag is false, the function must simply return t or nil indicating\n"
"whether the instantiator is valid.  If this flag is true, the function\n"
"is free to signal an error if it encounters an invalid instantiator\n"
"(this can be useful for issuing a specific error about exactly why the\n"
"instantiator is valid).  It can also return nil to indicate an invalid\n"
"instantiator; in this case, a general error will be signalled."

#endif /* 0 */

DEFUN ("generic-specifier-p", Fgeneric_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a generic specifier.

See `make-generic-specifier' for a description of possible generic
instantiators.
*/
       (object))
{
  return GENERIC_SPECIFIERP (object) ? Qt : Qnil;
}


/************************************************************************/
/*                        Integer specifier type                        */
/************************************************************************/

DEFINE_SPECIFIER_TYPE (integer);

static void
integer_validate (Lisp_Object instantiator)
{
  CHECK_FIXNUM (instantiator);
}

DEFUN ("integer-specifier-p", Finteger_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is an integer specifier.

See `make-integer-specifier' for a description of possible integer
instantiators.
*/
       (object))
{
  return INTEGER_SPECIFIERP (object) ? Qt : Qnil;
}

/************************************************************************/
/*                   Non-negative-integer specifier type                */
/************************************************************************/

DEFINE_SPECIFIER_TYPE (natnum);

static void
natnum_validate (Lisp_Object instantiator)
{
  CHECK_NATNUM (instantiator);
}

DEFUN ("natnum-specifier-p", Fnatnum_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a natnum (non-negative-integer) specifier.

See `make-natnum-specifier' for a description of possible natnum
instantiators.
*/
       (object))
{
  return NATNUM_SPECIFIERP (object) ? Qt : Qnil;
}

/************************************************************************/
/*                        Boolean specifier type                        */
/************************************************************************/

DEFINE_SPECIFIER_TYPE (boolean);

static void
boolean_validate (Lisp_Object instantiator)
{
  if (!EQ (instantiator, Qt) && !EQ (instantiator, Qnil))
    invalid_constant ("Must be t or nil", instantiator);
}

DEFUN ("boolean-specifier-p", Fboolean_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a boolean specifier.

See `make-boolean-specifier' for a description of possible boolean
instantiators.
*/
       (object))
{
  return BOOLEAN_SPECIFIERP (object) ? Qt : Qnil;
}

/************************************************************************/
/*                        Display table specifier type                  */
/************************************************************************/

DEFINE_SPECIFIER_TYPE (display_table);

#define VALID_SINGLE_DISPTABLE_INSTANTIATOR_P(instantiator)		\
  (VECTORP (instantiator)						\
   || (CHAR_TABLEP (instantiator)					\
       && (XCHAR_TABLE_TYPE (instantiator) == CHAR_TABLE_TYPE_CHAR	\
	   || XCHAR_TABLE_TYPE (instantiator) == CHAR_TABLE_TYPE_GENERIC)) \
   || RANGE_TABLEP (instantiator))

static void
display_table_validate (Lisp_Object instantiator)
{
  if (NILP (instantiator))
    /* OK */
    ;
  else if (CONSP (instantiator))
    {
      EXTERNAL_LIST_LOOP_2 (car, instantiator)
	{
	  if (!VALID_SINGLE_DISPTABLE_INSTANTIATOR_P (car))
	    goto lose;
	}
    }
  else
    {
      if (!VALID_SINGLE_DISPTABLE_INSTANTIATOR_P (instantiator))
	{
	lose:
	  dead_wrong_type_argument
	    (display_table_specifier_methods->predicate_symbol,
	     instantiator);
	}
    }
}

DEFUN ("display-table-specifier-p", Fdisplay_table_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a display-table specifier.

See `current-display-table' for a description of possible display-table
instantiators.
*/
       (object))
{
  return DISPLAYTABLE_SPECIFIERP (object) ? Qt : Qnil;
}



#ifdef MEMORY_USAGE_STATS

struct specifier_stats
{
  struct usage_stats u;
  /* Ancillary Lisp */
  Bytecount global, device, frame, window, buffer, fallback;
  Bytecount magic_parent;
};

static void
specifier_memory_usage (Lisp_Object UNUSED (specifier),
			struct generic_usage_stats * UNUSED (gustats))
{
#if 0
  struct specifier_stats *stats = (struct specifier_stats *) gustats;
  Lisp_Specifier *spec = XSPECIFIER (specifier);

  /* #### FIXME -- sometimes it appears that the specs, or at least global
     specs, can have circularities in the tree structure.  This makes
     everything much slower and in fact can result in a hang with 100% CPU.
     Need to investigate properly and figure out what's going on here,
     since the specs are copied when stored in and so supposedly, circular
     structures shouldn't exist. */
  stats->global = tree_memory_usage (spec->global_specs, 1);
  stats->device = tree_memory_usage (spec->device_specs, 1);
  stats->frame = tree_memory_usage (spec->frame_specs, 1);
  stats->window = tree_memory_usage (spec->window_specs, 1);
  stats->buffer = tree_memory_usage (spec->buffer_specs, 1);
  stats->fallback = tree_memory_usage (spec->fallback, 1);
  if (SPECIFIERP (spec->magic_parent))
    stats->magic_parent = lisp_object_memory_usage (spec->magic_parent);
#endif
}

#endif /* MEMORY_USAGE_STATS */

/************************************************************************/
/*                           Initialization                             */
/************************************************************************/

void
specifier_objects_create (void)
{
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_METHOD (specifier, memory_usage);
#endif
}

void
syms_of_specifier (void)
{
  INIT_LISP_OBJECT (specifier);
#ifdef NEW_GC
  INIT_LISP_OBJECT (specifier_caching);
#endif /* NEW_GC */

  DEFSYMBOL (Qspecifierp);

  DEFSYMBOL (Qconsole_type);
  DEFSYMBOL (Qdevice_class);

  /* specifier types defined in general.c. */

  DEFSUBR (Fvalid_specifier_type_p);
  DEFSUBR (Fspecifier_type_list);
  DEFSUBR (Fmake_specifier);
  DEFSUBR (Fspecifierp);
  DEFSUBR (Fspecifier_type);

  DEFSUBR (Fvalid_specifier_locale_p);
  DEFSUBR (Fvalid_specifier_domain_p);
  DEFSUBR (Fvalid_specifier_locale_type_p);
  DEFSUBR (Fspecifier_locale_type_from_locale);

  DEFSUBR (Fvalid_specifier_tag_p);
  DEFSUBR (Fvalid_specifier_tag_set_p);
  DEFSUBR (Fcanonicalize_tag_set);
  DEFSUBR (Fdevice_matches_specifier_tag_set_p);
  DEFSUBR (Fdefine_specifier_tag);
  DEFSUBR (Fdevice_matching_specifier_tag_list);

  DEFSUBR (Fspecifier_tag_list);
  DEFSUBR (Fspecifier_tag_device_predicate);
  DEFSUBR (Fspecifier_tag_charset_predicate);

  DEFSUBR (Fcheck_valid_instantiator);
  DEFSUBR (Fvalid_instantiator_p);
  DEFSUBR (Fcheck_valid_inst_list);
  DEFSUBR (Fvalid_inst_list_p);
  DEFSUBR (Fcheck_valid_spec_list);
  DEFSUBR (Fvalid_spec_list_p);
  DEFSUBR (Fadd_spec_to_specifier);
  DEFSUBR (Fadd_spec_list_to_specifier);
  DEFSUBR (Fspecifier_spec_list);
  DEFSUBR (Fspecifier_specs);
  DEFSUBR (Fremove_specifier);
  DEFSUBR (Fcopy_specifier);

  DEFSUBR (Fcheck_valid_specifier_matchspec);
  DEFSUBR (Fvalid_specifier_matchspec_p);
  DEFSUBR (Fspecifier_fallback);
  DEFSUBR (Fspecifier_instance);
  DEFSUBR (Fspecifier_instantiator);
  DEFSUBR (Fspecifier_matching_instance);
  DEFSUBR (Fspecifier_matching_instantiator);
  DEFSUBR (Fspecifier_instance_from_inst_list);
  DEFSUBR (Fspecifier_instantiator_from_inst_list);
  DEFSUBR (Fspecifier_matching_instance_from_inst_list);
  DEFSUBR (Fspecifier_matching_instantiator_from_inst_list);
  DEFSUBR (Fset_specifier_dirty_flag);

  DEFSUBR (Fgeneric_specifier_p);
  DEFSUBR (Finteger_specifier_p);
  DEFSUBR (Fnatnum_specifier_p);
  DEFSUBR (Fboolean_specifier_p);
  DEFSUBR (Fdisplay_table_specifier_p);

  /* Symbols pertaining to specifier creation.  Specifiers are created
     in the syms_of() functions. */

  /* locales are defined in general.c. */

  /* some how-to-add flags in general.c. */
  DEFSYMBOL (Qremove_tag_set_prepend);
  DEFSYMBOL (Qremove_tag_set_append);
  DEFSYMBOL (Qremove_locale);
  DEFSYMBOL (Qremove_locale_type);
}

void
specifier_type_create (void)
{
  the_specifier_type_entry_dynarr = Dynarr_new (specifier_type_entry);
  dump_add_root_block_ptr (&the_specifier_type_entry_dynarr, &sted_description);

  Vspecifier_type_list = Qnil;
  staticpro (&Vspecifier_type_list);

  INITIALIZE_SPECIFIER_TYPE (generic, "generic", "generic-specifier-p");

  INITIALIZE_SPECIFIER_TYPE (integer, "integer", "integer-specifier-p");

  SPECIFIER_HAS_METHOD (integer, validate);

  INITIALIZE_SPECIFIER_TYPE (natnum, "natnum", "natnum-specifier-p");

  SPECIFIER_HAS_METHOD (natnum, validate);

  INITIALIZE_SPECIFIER_TYPE (boolean, "boolean", "boolean-specifier-p");

  SPECIFIER_HAS_METHOD (boolean, validate);

  INITIALIZE_SPECIFIER_TYPE (display_table, "display-table",
			     "display-table-p");

  SPECIFIER_HAS_METHOD (display_table, validate);
}

void
reinit_specifier_type_create (void)
{
  REINITIALIZE_SPECIFIER_TYPE (generic);
  REINITIALIZE_SPECIFIER_TYPE (integer);
  REINITIALIZE_SPECIFIER_TYPE (natnum);
  REINITIALIZE_SPECIFIER_TYPE (boolean);
  REINITIALIZE_SPECIFIER_TYPE (display_table);
}

void
vars_of_specifier (void)
{
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_PROPERTY (specifier, memusage_stats_list,
		       listu (Qt, Qglobal, Qdevice, Qframe, Qwindow, Qbuffer,
			      Qfallback, intern ("magic-parent"),
			      Qunbound));
#endif /* MEMORY_USAGE_STATS */

  Vcached_specifiers = Qnil;
  staticpro (&Vcached_specifiers);

  /* Do NOT mark through this, or specifiers will never be GC'd.
     This is the same deal as for weak hash tables. */
  Vall_specifiers = Qnil;
  dump_add_weak_object_chain (&Vall_specifiers);

  Vuser_defined_tags = Qnil;
  staticpro (&Vuser_defined_tags);

  Vunlock_ghost_specifiers = Qnil;
  staticpro (&Vunlock_ghost_specifiers);

  Vcharset_tag_lists =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, Qeq);
  staticpro (&Vcharset_tag_lists);
}
