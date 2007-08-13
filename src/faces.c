/* "Face" primitives
   Copyright (C) 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Written by Chuck Thompson and Ben Wing,
   based loosely on old face code by Jamie Zawinski. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "elhash.h"
#include "extents.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"
#include "hash.h"
#include "objects.h"
#include "specifier.h"
#include "window.h"

#ifdef HAVE_X_WINDOWS
#include "console-x.h"
#include "objects-x.h"
#include "EmacsFrame.h"
#endif /* HAVE_X_WINDOWS */

/* Qfont, Qdoc_string, Qface defined in general.c */
Lisp_Object Qfacep;
Lisp_Object Qforeground, Qbackground, Qdisplay_table;
/* Qhighlight, Qreverse defined in general.c */
Lisp_Object Qbackground_pixmap, Qunderline, Qdim;
Lisp_Object Qblinking, Qstrikethru;

Lisp_Object Qinit_face_from_resources;
Lisp_Object Qinit_frame_faces;
Lisp_Object Qinit_device_faces;
Lisp_Object Qinit_global_faces;

/* These faces are used directly internally.  We use these variables
   to be able to reference them directly and save the overhead of
   calling Ffind_face. */
Lisp_Object Vdefault_face, Vmodeline_face;
Lisp_Object Vleft_margin_face, Vright_margin_face, Vtext_cursor_face;
Lisp_Object Vpointer_face;

/* Qdefault, Qhighlight defined in general.c */
Lisp_Object Qmodeline, Qleft_margin, Qright_margin, Qtext_cursor;

/* In the old implementation Vface_list was a list of the face names,
   not the faces themselves.  We now distinguish between permanent and
   temporary faces.  Permanent faces are kept in a regular hash table,
   temporary faces in a weak hash table. */
Lisp_Object Vpermanent_faces_cache;
Lisp_Object Vtemporary_faces_cache;

Lisp_Object Vbuilt_in_face_specifiers;


static Lisp_Object mark_face (Lisp_Object, void (*) (Lisp_Object));
static void print_face (Lisp_Object, Lisp_Object, int);
static int face_equal (Lisp_Object, Lisp_Object, int depth);
static unsigned long face_hash (Lisp_Object obj, int depth);
static Lisp_Object face_getprop (Lisp_Object obj, Lisp_Object prop);
static int face_putprop (Lisp_Object obj, Lisp_Object prop, Lisp_Object value);
static int face_remprop (Lisp_Object obj, Lisp_Object prop);
static Lisp_Object face_plist (Lisp_Object obj);
DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS ("face", face,
					  mark_face, print_face, 0, face_equal,
					  face_hash, face_getprop,
					  face_putprop, face_remprop,
					  face_plist, struct Lisp_Face);

static Lisp_Object
mark_face (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Face *face =  XFACE (obj);

  ((markobj) (face->name));
  ((markobj) (face->doc_string));

  ((markobj) (face->foreground));
  ((markobj) (face->background));
  ((markobj) (face->font));
  ((markobj) (face->display_table));
  ((markobj) (face->background_pixmap));
  ((markobj) (face->underline));
  ((markobj) (face->strikethru));
  ((markobj) (face->highlight));
  ((markobj) (face->dim));
  ((markobj) (face->blinking));
  ((markobj) (face->reverse));

  ((markobj) (face->charsets_warned_about));

  return face->plist;
}

static void
print_face (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Face *face = XFACE (obj);

  if (print_readably)
    {
      write_c_string ("#s(face name ", printcharfun);
      print_internal (face->name, printcharfun, 1);
      write_c_string (")", printcharfun);
    }
  else
    {
      write_c_string ("#<face ", printcharfun);
      print_internal (face->name, printcharfun, 1);
      if (!NILP (face->doc_string))
	{
	  write_c_string (" ", printcharfun);
	  print_internal (face->doc_string, printcharfun, 1);
	}
      write_c_string (">", printcharfun);
    }
}

/* Faces are equal if all of their display attributes are equal.  We
   don't compare names or doc-strings, because that would make equal
   be eq.

   This isn't concerned with "unspecified" attributes, that's what
   #'face-differs-from-default-p is for. */
static int
face_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  struct Lisp_Face *f1 = XFACE (o1);
  struct Lisp_Face *f2 = XFACE (o2);

  depth++;

  return
    (internal_equal (f1->foreground,	     f2->foreground,	    depth) &&
     internal_equal (f1->background,	     f2->background,	    depth) &&
     internal_equal (f1->font,		     f2->font,		    depth) &&
     internal_equal (f1->display_table,	     f2->display_table,	    depth) &&
     internal_equal (f1->background_pixmap,  f2->background_pixmap, depth) &&
     internal_equal (f1->underline,	     f2->underline,	    depth) &&
     internal_equal (f1->strikethru,	     f2->strikethru,	    depth) &&
     internal_equal (f1->highlight,	     f2->highlight,	    depth) &&
     internal_equal (f1->dim,		     f2->dim,		    depth) &&
     internal_equal (f1->blinking,	     f2->blinking,	    depth) &&
     internal_equal (f1->reverse,	     f2->reverse,	    depth) &&

     ! plists_differ (f1->plist, f2->plist, 0, 0, depth + 1));
}

static unsigned long
face_hash (Lisp_Object obj, int depth)
{
  struct Lisp_Face *f = XFACE (obj);

  depth++;

  /* No need to hash all of the elements; that would take too long.
     Just hash the most common ones. */
  return HASH3 (internal_hash (f->foreground, depth),
		internal_hash (f->background, depth),
		internal_hash (f->font,       depth));
}

static Lisp_Object
face_getprop (Lisp_Object obj, Lisp_Object prop)
{
  struct Lisp_Face *f = XFACE (obj);

  return
    ((EQ (prop, Qforeground))	     ? f->foreground	    :
     (EQ (prop, Qbackground))	     ? f->background	    :
     (EQ (prop, Qfont))		     ? f->font		    :
     (EQ (prop, Qdisplay_table))     ? f->display_table	    :
     (EQ (prop, Qbackground_pixmap)) ? f->background_pixmap :
     (EQ (prop, Qunderline))	     ? f->underline	    :
     (EQ (prop, Qstrikethru))	     ? f->strikethru	    :
     (EQ (prop, Qhighlight))	     ? f->highlight	    :
     (EQ (prop, Qdim))		     ? f->dim		    :
     (EQ (prop, Qblinking))	     ? f->blinking	    :
     (EQ (prop, Qreverse))	     ? f->reverse	    :
     (EQ (prop, Qdoc_string))	     ? f->doc_string	    :
     external_plist_get (&f->plist, prop, 0, ERROR_ME));
}

static int
face_putprop (Lisp_Object obj, Lisp_Object prop, Lisp_Object value)
{
  struct Lisp_Face *f = XFACE (obj);

  if (EQ (prop, Qforeground)        ||
      EQ (prop, Qbackground)        ||
      EQ (prop, Qfont)              ||
      EQ (prop, Qdisplay_table)     ||
      EQ (prop, Qbackground_pixmap) ||
      EQ (prop, Qunderline)         ||
      EQ (prop, Qstrikethru)        ||
      EQ (prop, Qhighlight)         ||
      EQ (prop, Qdim)               ||
      EQ (prop, Qblinking)          ||
      EQ (prop, Qreverse))
    return 0;

  if (EQ (prop, Qdoc_string))
    {
      if (!NILP (value))
	CHECK_STRING (value);
      f->doc_string = value;
      return 1;
    }

  external_plist_put (&f->plist, prop, value, 0, ERROR_ME);
  return 1;
}

static int
face_remprop (Lisp_Object obj, Lisp_Object prop)
{
  struct Lisp_Face *f = XFACE (obj);

  if (EQ (prop, Qforeground)        ||
      EQ (prop, Qbackground)        ||
      EQ (prop, Qfont)              ||
      EQ (prop, Qdisplay_table)     ||
      EQ (prop, Qbackground_pixmap) ||
      EQ (prop, Qunderline)         ||
      EQ (prop, Qstrikethru)        ||
      EQ (prop, Qhighlight)         ||
      EQ (prop, Qdim)               ||
      EQ (prop, Qblinking)          ||
      EQ (prop, Qreverse))
    return -1;

  if (EQ (prop, Qdoc_string))
    {
      f->doc_string = Qnil;
      return 1;
    }

  return external_remprop (&f->plist, prop, 0, ERROR_ME);
}

static Lisp_Object
face_plist (Lisp_Object obj)
{
  struct Lisp_Face *f = XFACE (obj);
  Lisp_Object result = Qnil;

  /* backwards order; we reverse it below */
  result = Fcons (f->foreground,        Fcons (Qforeground,        result));
  result = Fcons (f->background,        Fcons (Qbackground,        result));
  result = Fcons (f->font,              Fcons (Qfont,              result));
  result = Fcons (f->display_table,     Fcons (Qdisplay_table,     result));
  result = Fcons (f->background_pixmap, Fcons (Qbackground_pixmap, result));
  result = Fcons (f->underline,         Fcons (Qunderline,         result));
  result = Fcons (f->strikethru,        Fcons (Qstrikethru,        result));
  result = Fcons (f->highlight,         Fcons (Qhighlight,         result));
  result = Fcons (f->dim,               Fcons (Qdim,               result));
  result = Fcons (f->blinking,          Fcons (Qblinking,          result));
  result = Fcons (f->reverse,           Fcons (Qreverse,           result));

  return nconc2 (Fnreverse (result), f->plist);
}


/************************************************************************/
/*                             face read syntax                         */
/************************************************************************/

static int
face_name_validate (Lisp_Object keyword, Lisp_Object value,
		    Error_behavior errb)
{
  if (ERRB_EQ (errb, ERROR_ME))
    {
      CHECK_SYMBOL (value);
      return 1;
    }

  return SYMBOLP (value);
}

static int
face_validate (Lisp_Object data, Error_behavior errb)
{
  int name_seen = 0;
  Lisp_Object valw = Qnil;

  data = Fcdr (data); /* skip over Qface */
  while (!NILP (data))
    {
      Lisp_Object keyw = Fcar (data);

      data = Fcdr (data);
      valw = Fcar (data);
      data = Fcdr (data);
      if (EQ (keyw, Qname))
	{
	  if (name_seen)
	    {
	      maybe_signal_simple_error ("Duplicate face name given", valw,
					 Qface, errb);
	      return 0;
	    }
	  name_seen = 1;
	}
      else
	abort ();
    }

  if (!name_seen)
    {
      maybe_error (Qface, errb, "No face name given");
      return 0;
    }

  if (NILP (Ffind_face (valw)))
    {
      maybe_signal_simple_error ("No such face", valw, Qface, errb);
      return 0;
    }

  return 1;
}

static Lisp_Object
face_instantiate (Lisp_Object data)
{
  return Fget_face (Fcar (Fcdr (data)));
}


/****************************************************************************
 *                             utility functions                            *
 ****************************************************************************/

static void
reset_face (struct Lisp_Face *f)
{
  f->name = Qnil;
  f->doc_string = Qnil;
  f->dirty = 0;
  f->foreground = Qnil;
  f->background = Qnil;
  f->font = Qnil;
  f->display_table = Qnil;
  f->background_pixmap = Qnil;
  f->underline = Qnil;
  f->strikethru = Qnil;
  f->highlight = Qnil;
  f->dim = Qnil;
  f->blinking = Qnil;
  f->reverse = Qnil;
  f->plist = Qnil;
  f->charsets_warned_about = Qnil;
}

static struct Lisp_Face *
allocate_face (void)
{
  struct Lisp_Face *result =
    alloc_lcrecord_type (struct Lisp_Face, lrecord_face);

  reset_face (result);
  return result;
}


/* We store the faces in hash tables with the names as the key and the
   actual face object as the value.  Occasionally we need to use them
   in a list format.  These routines provide us with that. */
struct face_list_closure
{
  Lisp_Object *face_list;
};

static void
add_face_to_list_mapper (CONST void *hash_key, void *hash_contents,
			 void *face_list_closure)
{
  /* This function can GC */
  Lisp_Object key, contents;
  Lisp_Object *face_list;
  struct face_list_closure *fcl =
    (struct face_list_closure *) face_list_closure;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  face_list = fcl->face_list;

  *face_list = Fcons (XFACE (contents)->name, *face_list);
}

static Lisp_Object
faces_list_internal (Lisp_Object list)
{
  Lisp_Object face_list = Qnil;
  struct gcpro gcpro1;
  struct face_list_closure face_list_closure;

  GCPRO1 (face_list);
  face_list_closure.face_list = &face_list;
  elisp_maphash (add_face_to_list_mapper, list, &face_list_closure);
  UNGCPRO;

  return face_list;
}

static Lisp_Object
permanent_faces_list (void)
{
  return faces_list_internal (Vpermanent_faces_cache);
}

static Lisp_Object
temporary_faces_list (void)
{
  return faces_list_internal (Vtemporary_faces_cache);
}


static void
mark_face_as_clean_mapper (CONST void *hash_key, void *hash_contents,
			   void *flag_closure)
{
  /* This function can GC */
  Lisp_Object key, contents;
  int *flag = flag_closure;
  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);
  XFACE (contents)->dirty = *flag;
}

static void
mark_all_faces_internal (int flag)
{
  elisp_maphash (mark_face_as_clean_mapper, Vpermanent_faces_cache, &flag);
  elisp_maphash (mark_face_as_clean_mapper, Vtemporary_faces_cache, &flag);
}

void
mark_all_faces_as_clean (void)
{
  mark_all_faces_internal (0);
}

/* #### OBSOLETE ME, PLEASE.  Maybe.  Maybe this is just as good as
   any other solution. */
struct face_inheritance_closure
{
  Lisp_Object face;
  Lisp_Object property;
};

static void
update_inheritance_mapper_internal (Lisp_Object cur_face,
				    Lisp_Object inh_face,
				    Lisp_Object property)
{
  /* #### fix this function */
  Lisp_Object elt = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (elt);

  for (elt = FACE_PROPERTY_SPEC_LIST (cur_face, property, Qall);
       !NILP (elt);
       elt = XCDR (elt))
    {
      Lisp_Object values = XCDR (XCAR (elt));

      for (; !NILP (values); values = XCDR (values))
	{
	  Lisp_Object value = XCDR (XCAR (values));
	  if (VECTORP (value) && XVECTOR_LENGTH (value))
	    {
	      if (EQ (Ffind_face (XVECTOR_DATA (value)[0]), inh_face))
		Fset_specifier_dirty_flag
		  (FACE_PROPERTY_SPECIFIER (inh_face, property));
	    }
	}
    }

  UNGCPRO;
}

static void
update_face_inheritance_mapper (CONST void *hash_key, void *hash_contents,
				void *face_inheritance_closure)
{
  Lisp_Object key, contents;
  struct face_inheritance_closure *fcl =
    (struct face_inheritance_closure *) face_inheritance_closure;

  CVOID_TO_LISP (key, hash_key);
  VOID_TO_LISP (contents, hash_contents);

  if (EQ (fcl->property, Qfont))
    {
      update_inheritance_mapper_internal (contents, fcl->face, Qfont);
    }
  else if (EQ (fcl->property, Qforeground) ||
	   EQ (fcl->property, Qbackground))
    {
      update_inheritance_mapper_internal (contents, fcl->face, Qforeground);
      update_inheritance_mapper_internal (contents, fcl->face, Qbackground);
    }
  else if (EQ (fcl->property, Qunderline)  ||
	   EQ (fcl->property, Qstrikethru) ||
	   EQ (fcl->property, Qhighlight)  ||
	   EQ (fcl->property, Qdim)        ||
	   EQ (fcl->property, Qblinking)   ||
	   EQ (fcl->property, Qreverse))
    {
      update_inheritance_mapper_internal (contents, fcl->face, Qunderline);
      update_inheritance_mapper_internal (contents, fcl->face, Qstrikethru);
      update_inheritance_mapper_internal (contents, fcl->face, Qhighlight);
      update_inheritance_mapper_internal (contents, fcl->face, Qdim);
      update_inheritance_mapper_internal (contents, fcl->face, Qblinking);
      update_inheritance_mapper_internal (contents, fcl->face, Qreverse);
    }
}

static void
update_faces_inheritance (Lisp_Object face, Lisp_Object property)
{
  struct face_inheritance_closure face_inheritance_closure;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (face, property);
  face_inheritance_closure.face = face;
  face_inheritance_closure.property = property;

  elisp_maphash (update_face_inheritance_mapper, Vpermanent_faces_cache,
		 &face_inheritance_closure);
  elisp_maphash (update_face_inheritance_mapper, Vtemporary_faces_cache,
		 &face_inheritance_closure);

  UNGCPRO;
}

Lisp_Object
face_property_matching_instance (Lisp_Object face, Lisp_Object property,
				 Lisp_Object charset, Lisp_Object domain,
				 Error_behavior errb, int no_fallback,
				 Lisp_Object depth)
{
  Lisp_Object retval =
    specifier_instance_no_quit (Fget (face, property, Qnil), charset,
				domain, errb, no_fallback, depth);

  if (UNBOUNDP (retval) && !no_fallback)
    {
      if (EQ (property, Qfont))
	{
	  if (NILP (memq_no_quit (charset,
				  XFACE (face)->charsets_warned_about)))
	    {
#ifdef MULE
	      if (! UNBOUNDP (charset))
		warn_when_safe
		  (Qfont, Qwarning,
		   "Unable to instantiate font for face %s, charset %s",
		   string_data (symbol_name
				(XSYMBOL (XFACE (face)->name))),
		   string_data (symbol_name
				(XSYMBOL (XCHARSET_NAME (charset)))));
	      else
#endif
		warn_when_safe (Qfont, Qwarning,
				"Unable to instantiate font for face %s",
				string_data (symbol_name
					     (XSYMBOL (XFACE (face)->name))));
	      XFACE (face)->charsets_warned_about =
		Fcons (charset, XFACE (face)->charsets_warned_about);
	    }
	  retval = Vthe_null_font_instance;
	}
    }

  return retval;
}


DEFUN ("facep", Ffacep, 1, 1, 0, /*
Return non-nil if OBJECT is a face.
*/
       (object))
{
  return FACEP (object) ? Qt : Qnil;
}

DEFUN ("find-face", Ffind_face, 1, 1, 0, /*
Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned.
*/
       (face_or_name))
{
  Lisp_Object retval;

  if (FACEP (face_or_name))
    return face_or_name;
  CHECK_SYMBOL (face_or_name);

  /* Check if the name represents a permanent face. */
  retval = Fgethash (face_or_name, Vpermanent_faces_cache, Qnil);
  if (!NILP (retval))
    return retval;

  /* Check if the name represents a temporary face. */
  return Fgethash (face_or_name, Vtemporary_faces_cache, Qnil);
}

DEFUN ("get-face", Fget_face, 1, 1, 0, /*
Retrieve the face of the given name.
Same as `find-face' except an error is signalled if there is no such
face instead of returning nil.
*/
       (name))
{
  Lisp_Object face = Ffind_face (name);

  if (NILP (face))
    signal_simple_error ("No such face", name);
  return face;
}

DEFUN ("face-name", Fface_name, 1, 1, 0, /*
Return the name of the given face.
*/
       (face))
{
  return XFACE (Fget_face (face))->name;
}

DEFUN ("built-in-face-specifiers", Fbuilt_in_face_specifiers, 0, 0, 0, /*
Return a list of all built-in face specifier properties.
Don't modify this list!
*/
       ())
{
  return Vbuilt_in_face_specifiers;
}

/* These values are retrieved so often that we make a special
   function.
*/

void
default_face_font_info (Lisp_Object domain, int *ascent, int *descent,
			int *height, int *width, int *proportional_p)
{
  Lisp_Object font_instance;

  if (noninteractive)
    {
      if (ascent)
        *ascent = 1;
      if (descent)
        *descent = 0;
      if (height)
        *height = 1;
      if (width)
        *width = 1;
      if (proportional_p)
        *proportional_p = 0;
      return;
    }

  /* We use ASCII here.  This is probably reasonable because the
     people calling this function are using the resulting values to
     come up with overall sizes for windows and frames. */
  if (WINDOWP (domain))
    {
      struct face_cachel *cachel;
      struct window *w = XWINDOW (domain);

      /* #### It's possible for this function to get called when the
	 face cachels have not been initialized.  I don't know why. */
      if (!Dynarr_length (w->face_cachels))
        reset_face_cachels (w);
      cachel = WINDOW_FACE_CACHEL (w, DEFAULT_INDEX);
      font_instance = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
    }
  else
    {
      font_instance = FACE_FONT (Vdefault_face, domain, Vcharset_ascii);
    }

  if (height)
    *height = XFONT_INSTANCE (font_instance)->height;
  if (width)
    *width = XFONT_INSTANCE (font_instance)->width;
  if (ascent)
    *ascent = XFONT_INSTANCE (font_instance)->ascent;
  if (descent)
    *descent = XFONT_INSTANCE (font_instance)->descent;
  if (proportional_p)
    *proportional_p = XFONT_INSTANCE (font_instance)->proportional_p;
}

void
default_face_height_and_width (Lisp_Object domain,
			       int *height, int *width)
{
  default_face_font_info (domain, 0, 0, height, width, 0);
}

DEFUN ("face-list", Fface_list, 0, 1, 0, /*
Return a list of the names of all defined faces.
If TEMPORARY is nil, only the permanent faces are included.
If it is t, only the temporary faces are included.  If it is any
other non-nil value both permanent and temporary are included.
*/
       (temporary))
{
  Lisp_Object face_list = Qnil;

  /* Added the permanent faces, if requested. */
  if (NILP (temporary) || !EQ (Qt, temporary))
    face_list = permanent_faces_list ();

  if (!NILP (temporary))
    {
      struct gcpro gcpro1;
      GCPRO1 (face_list);
      face_list = nconc2 (face_list, temporary_faces_list ());
      UNGCPRO;
    }

  return face_list;
}

DEFUN ("make-face", Fmake_face, 1, 3, 0, /*
Defines and returns a new FACE described by DOC-STRING.
You can modify the font, color, etc of a face with the set-face- functions.
If the face already exists, it is unmodified.
If TEMPORARY is non-nil, this face will cease to exist if not in use.
*/
       (name, doc_string, temporary))
{
  /* This function can GC if initialized is non-zero */
  struct Lisp_Face *f;
  Lisp_Object face;

  CHECK_SYMBOL (name);
  if (!NILP (doc_string))
    CHECK_STRING (doc_string);

  face = Ffind_face (name);
  if (!NILP (face))
    return face;

  f = allocate_face ();
  XSETFACE (face, f);

  f->name = name;
  f->doc_string = doc_string;
  f->foreground = Fmake_specifier (Qcolor);
  set_color_attached_to (f->foreground, face, Qforeground);
  f->background = Fmake_specifier (Qcolor);
  set_color_attached_to (f->background, face, Qbackground);
  f->font = Fmake_specifier (Qfont);
  set_font_attached_to (f->font, face, Qfont);
  f->background_pixmap = Fmake_specifier (Qimage);
  set_image_attached_to (f->background_pixmap, face, Qbackground_pixmap);
  f->display_table = Fmake_specifier (Qdisplay_table);
  f->underline = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->underline, face, Qunderline);
  f->strikethru = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->strikethru, face, Qstrikethru);
  f->highlight = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->highlight, face, Qhighlight);
  f->dim = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->dim, face, Qdim);
  f->blinking = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->blinking, face, Qblinking);
  f->reverse = Fmake_specifier (Qface_boolean);
  set_face_boolean_attached_to (f->reverse, face, Qreverse);
  if (!NILP (Vdefault_face))
    {
      /* If the default face has already been created, set it as
	 the default fallback specifier for all the specifiers we
	 just created.  This implements the standard "all faces
	 inherit from default" behavior. */
      set_specifier_fallback (f->foreground,
			     Fget (Vdefault_face, Qforeground, Qunbound));
      set_specifier_fallback (f->background,
			     Fget (Vdefault_face, Qbackground, Qunbound));
      set_specifier_fallback (f->font,
			     Fget (Vdefault_face, Qfont, Qunbound));
      set_specifier_fallback (f->background_pixmap,
			     Fget (Vdefault_face, Qbackground_pixmap,
				   Qunbound));
      set_specifier_fallback (f->display_table,
			     Fget (Vdefault_face, Qdisplay_table, Qunbound));
      set_specifier_fallback (f->underline,
			     Fget (Vdefault_face, Qunderline, Qunbound));
      set_specifier_fallback (f->strikethru,
			     Fget (Vdefault_face, Qstrikethru, Qunbound));
      set_specifier_fallback (f->highlight,
			     Fget (Vdefault_face, Qhighlight, Qunbound));
      set_specifier_fallback (f->dim,
			     Fget (Vdefault_face, Qdim, Qunbound));
      set_specifier_fallback (f->blinking,
			     Fget (Vdefault_face, Qblinking, Qunbound));
      set_specifier_fallback (f->reverse,
			     Fget (Vdefault_face, Qreverse, Qunbound));
    }

  /* Add the face to the appropriate list. */
  if (NILP (temporary))
    Fputhash (name, face, Vpermanent_faces_cache);
  else
    Fputhash (name, face, Vtemporary_faces_cache);

  /* Note that it's OK if we dump faces.
     When we start up again when we're not noninteractive,
     `init-global-faces' is called and it resources all
     existing faces. */
  if (initialized && !noninteractive)
    {
      struct gcpro gcpro1, gcpro2;

      GCPRO2 (name, face);
      call1 (Qinit_face_from_resources, name);
      UNGCPRO;
    }

  return face;
}


/*****************************************************************************
 initialization code
 ****************************************************************************/

void
init_global_faces (struct device *d)
{
  /* When making the initial terminal device, there is no Lisp code
     loaded, so we can't do this. */
  if (initialized && !noninteractive)
    {
      call_critical_lisp_code (d, Qinit_global_faces, Qnil);
    }
}

void
init_device_faces (struct device *d)
{
  /* When making the initial terminal device, there is no Lisp code
     loaded, so we can't do this. */
  if (initialized)
    {
      Lisp_Object tdevice;
      XSETDEVICE (tdevice, d);
      call_critical_lisp_code (d, Qinit_device_faces, tdevice);
    }
}

void
init_frame_faces (struct frame *frm)
{
  /* When making the initial terminal device, there is no Lisp code
     loaded, so we can't do this. */
  if (initialized)
    {
      Lisp_Object tframe;
      XSETFRAME (tframe, frm);

      /* DO NOT change the selected frame here.  If the debugger goes off
         it will try and display on the frame being created, but it is not
         ready for that yet and a horrible death will occur.  Any random
         code depending on the selected-frame as an implicit arg should be
         tracked down and shot.  For the benefit of the one known,
         xpm-color-symbols, make-frame sets the variable
         Vframe_being_created to the frame it is making and sets it to nil
         when done.  Internal functions that this could trigger which are
         currently depending on selected-frame should use this instead.  It
         is not currently visible at the lisp level. */
      call_critical_lisp_code (XDEVICE (FRAME_DEVICE (frm)),
			       Qinit_frame_faces, tframe);
    }
}


/****************************************************************************
 *                        face cache element functions                      *
 ****************************************************************************/

/*

#### Here is a description of how the face cache elements ought
to be redone.  It is *NOT* how they work currently:

However, when I started to go about implementing this, I realized
that there are all sorts of subtle problems with cache coherency
that are coming up.  As it turns out, these problems don't
manifest themselves now due to the brute-force "kill 'em all"
approach to cache invalidation when faces change; but if this
is ever made smarter, these problems are going to come up, and
some of them are very non-obvious.

I'm thinking of redoing the cache code a bit to avoid these
coherency problems.  The bulk of the problems will arise because
the current display structures have simple indices into the
face cache, but the cache can be changed at various times,
which could make the current display structures incorrect.
I guess the dirty and updated flags are an attempt to fix
this, but this approach doesn't really work.

Here's an approach that should keep things clean and unconfused:

1) Imagine a "virtual face cache" that can grow arbitrarily
   big and for which the only thing allowed is to add new
   elements.  Existing elements cannot be removed or changed.
   This way, any pointers in the existing redisplay structure
   into the cache never get screwed up. (This is important
   because even if a cache element is out of date, if there's
   a pointer to it then its contents still accurately describe
   the way the text currently looks on the screen.)
2) Each element in the virtual cache either describes exactly
   one face, or describes the merger of a number of faces
   by some process.  In order to simplify things, for mergers
   we do not record which faces or ordering was used, but
   simply that this cache element is the result of merging.
   Unlike the current implementation, it's important that a
   single cache element not be used to both describe a
   single face and describe a merger, even if all the property
   values are the same.
3) Each cache element can be clean or dirty.  "Dirty" means
   that the face that the element points to has been changed;
   this gets set at the time the face is changed.  This
   way, when looking up a value in the cache, you can determine
   whether it's out of date or not.  For merged faces it
   does not matter -- we don't record the faces or priority
   used to create the merger, so it's impossible to look up
   one of these faces.  We have to recompute it each time.
   Luckily, this is fine -- doing the merge is much
   less expensive than recomputing the properties of a
   single face.
4) For each cache element, we keep a hash value. (In order
   to hash the boolean properties, we convert each of them
   into a different large prime number so that the hashing works
   well.) This allows us, when comparing runes, to properly
   determine whether the face for that rune has changed.
   This will be especially important for TTY's, where there
   aren't that many faces and minimizing redraw is very
   important.
5) We can't actually keep an infinite cache, but that doesn't
   really matter that much.  The only elements we care about
   are those that are used by either the current or desired
   display structs.  Therefore, we keep a per-window
   redisplay iteration number, and mark each element with
   that number as we use it.  Just after outputting the
   window and synching the redisplay structs, we go through
   the cache and invalidate all elements that are not clean
   elements referring to a particular face and that do not
   have an iteration number equal to the current one.  We
   keep them in a chain, and use them to allocate new
   elements when possible instead of increasing the Dynarr.

   */

/* mark for GC a dynarr of face cachels. */

void
mark_face_cachels (face_cachel_dynarr *elements,
		   void (*markobj) (Lisp_Object))
{
  int elt;

  if (!elements)
    return;

  for (elt = 0; elt < Dynarr_length (elements); elt++)
    {
      struct face_cachel *cachel = Dynarr_atp (elements, elt);

      {
	int i;

	for (i = 0; i < NUM_LEADING_BYTES; i++)
	  if (!NILP (cachel->font[i]) && !UNBOUNDP (cachel->font[i]))
	    ((markobj) (cachel->font[i]));
      }
      ((markobj) (cachel->face));
      ((markobj) (cachel->foreground));
      ((markobj) (cachel->background));
      ((markobj) (cachel->display_table));
      ((markobj) (cachel->background_pixmap));
    }
}

/* ensure that the given cachel contains an updated font value for
   the given charset.  Return the updated font value. */

Lisp_Object
ensure_face_cachel_contains_charset (struct face_cachel *cachel,
				     Lisp_Object domain, Lisp_Object charset)
{
  Lisp_Object new_val;
  Lisp_Object face = cachel->face;
  int bound = 1;
  int offs = XCHARSET_LEADING_BYTE (charset) - MIN_LEADING_BYTE;

  if (!UNBOUNDP (cachel->font[offs])
      && cachel->font_updated[offs])
    return cachel->font[offs];

  if (UNBOUNDP (face))
    {
      /* a merged face. */
      int i;
      struct window *w = XWINDOW (domain);

      new_val = Qunbound;
      cachel->font_specified[offs] = 0;
      for (i = 0; i < cachel->nfaces; i++)
	{
	  struct face_cachel *oth;

	  oth = Dynarr_atp (w->face_cachels,
			    FACE_CACHEL_FINDEX_UNSAFE (cachel, i));
	  /* Tout le monde aime la recursion */
	  ensure_face_cachel_contains_charset (oth, domain, charset);

	  if (oth->font_specified[offs])
	    {
	      new_val = oth->font[offs];
	      cachel->font_specified[offs] = 1;
	      break;
	    }
	}

      if (!cachel->font_specified[offs])
	/* need to do the default face. */
	{
	  struct face_cachel *oth =
	    Dynarr_atp (w->face_cachels, DEFAULT_INDEX);
	  ensure_face_cachel_contains_charset (oth, domain, charset);

	  new_val = oth->font[offs];
	}

      if (!UNBOUNDP (cachel->font[offs]) && !EQ (cachel->font[offs], new_val))
	cachel->dirty = 1;
      cachel->font_updated[offs] = 1;
      cachel->font[offs] = new_val;
      return new_val;
    }

  new_val = face_property_matching_instance (face, Qfont, charset, domain,
					     /* #### look into ERROR_ME_NOT */
					     ERROR_ME_NOT, 1, Qzero);
  if (UNBOUNDP (new_val))
    {
      bound = 0;
      new_val = face_property_matching_instance (face, Qfont,
						 charset, domain,
						 /* #### look into
						    ERROR_ME_NOT */
						 ERROR_ME_NOT, 0, Qzero);
    }
  if (!UNBOUNDP (cachel->font[offs]) && !EQ (new_val, cachel->font[offs]))
    cachel->dirty = 1;
  cachel->font_updated[offs] = 1;
  cachel->font[offs] = new_val;
  cachel->font_specified[offs] = (bound || EQ (face, Vdefault_face));
  return new_val;
}

/* Ensure that the given cachel contains updated fonts for all
   the charsets specified. */

void
ensure_face_cachel_complete (struct face_cachel *cachel,
			     Lisp_Object domain, unsigned char *charsets)
{
  int i;

  for (i = 0; i < NUM_LEADING_BYTES; i++)
    if (charsets[i])
      {
	Lisp_Object charset = CHARSET_BY_LEADING_BYTE (i + MIN_LEADING_BYTE);
	assert (CHARSETP (charset));
	ensure_face_cachel_contains_charset (cachel, domain, charset);
      }
}

void
face_cachel_charset_font_metric_info (struct face_cachel *cachel,
				      unsigned char *charsets,
				      struct font_metric_info *fm)
{
  int i;

  fm->width = 1;
  fm->height = fm->ascent = 1;
  fm->descent = 0;
  fm->proportional_p = 0;

  for (i = 0; i < NUM_LEADING_BYTES; i++)
    {
      if (charsets[i])
	{
	  Lisp_Object charset = CHARSET_BY_LEADING_BYTE (i + MIN_LEADING_BYTE);
	  Lisp_Object font_instance = FACE_CACHEL_FONT (cachel, charset);
	  struct Lisp_Font_Instance *fi = XFONT_INSTANCE (font_instance);

	  assert (CHARSETP (charset));
	  assert (FONT_INSTANCEP (font_instance));

	  if (fm->ascent  < (int) fi->ascent)  fm->ascent  = (int) fi->ascent;
	  if (fm->descent < (int) fi->descent) fm->descent = (int) fi->descent;
	  fm->height = fm->ascent + fm->descent;
	  if (fi->proportional_p)
	    fm->proportional_p = 1;
	  if (EQ (charset, Vcharset_ascii))
	    fm->width = fi->width;
	}
    }
}

/* Called when the updated flag has been cleared on a cachel. */

void
update_face_cachel_data (struct face_cachel *cachel,
			 Lisp_Object domain,
			 Lisp_Object face)
{
  if (XFACE (face)->dirty || UNBOUNDP (cachel->face))
    {
      int default_face = EQ (face, Vdefault_face);
      cachel->face = face;

      /* We normally only set the _specified flags if the value was
         actually bound.  The exception is for the default face where
         we always set it since it is the ultimate fallback. */

#define FROB(field)							     \
  do {									     \
    Lisp_Object new_val =						     \
      FACE_PROPERTY_INSTANCE (face, Q##field, domain, 1, Qzero);	     \
    int bound = 1;							     \
    if (UNBOUNDP (new_val))						     \
      {									     \
	bound = 0;							     \
	new_val = FACE_PROPERTY_INSTANCE (face, Q##field, domain, 0, Qzero); \
      }									     \
    if (!EQ (new_val, cachel->field))					     \
      {									     \
	cachel->field = new_val;					     \
	cachel->dirty = 1;						     \
      }									     \
    cachel->field##_specified = (bound || default_face);		     \
  } while (0)

      FROB (foreground);
      FROB (background);
      FROB (display_table);
      FROB (background_pixmap);

      /*
       * A face's background pixmap will override the face's
       * background color.  But the background pixmap of the
       * default face should not override the background color of
       * a face if the background color has been specified or
       * inherited.
       *
       * To accomplish this we remove the background pixmap of the
       * cachel and mark it as having been specified so that cachel
       * merging won't override it later.
       */
      if (! default_face
	  && cachel->background_specified
	  && ! cachel->background_pixmap_specified)
	{
	  cachel->background_pixmap = Qunbound;
	  cachel->background_pixmap_specified = 1;
	}

#undef FROB

      ensure_face_cachel_contains_charset (cachel, domain, Vcharset_ascii);

#define FROB(field)							     \
  do {									     \
    Lisp_Object new_val =						     \
      FACE_PROPERTY_INSTANCE (face, Q##field, domain, 1, Qzero);	     \
    int bound = 1;							     \
    int new_val_int;							     \
    if (UNBOUNDP (new_val))						     \
      {									     \
	bound = 0;							     \
	new_val = FACE_PROPERTY_INSTANCE (face, Q##field, domain, 0, Qzero); \
      }									     \
    new_val_int = EQ (new_val, Qt);					     \
    if (cachel->field != new_val_int)					     \
      {									     \
	cachel->field = new_val_int;					     \
	cachel->dirty = 1;						     \
      }									     \
    cachel->field##_specified = bound;					     \
  } while (0)

      FROB (underline);
      FROB (strikethru);
      FROB (highlight);
      FROB (dim);
      FROB (reverse);
      FROB (blinking);
#undef FROB
    }

  cachel->updated = 1;
}

/* Merge the cachel identified by FINDEX in window W into the given
   cachel. */

static void
merge_face_cachel_data (struct window *w, face_index findex,
			struct face_cachel *cachel)
{
#define FINDEX_FIELD(field)						\
  Dynarr_atp (w->face_cachels, findex)->field

#define FROB(field)							\
  do {									\
    if (!cachel->field##_specified && FINDEX_FIELD (field##_specified))	\
      {									\
	cachel->field = FINDEX_FIELD (field);				\
	cachel->field##_specified = 1;					\
	cachel->dirty = 1;						\
      }									\
  } while (0)

  FROB (foreground);
  FROB (background);
  FROB (display_table);
  FROB (background_pixmap);
  FROB (underline);
  FROB (strikethru);
  FROB (highlight);
  FROB (dim);
  FROB (reverse);
  FROB (blinking);
  /* And do ASCII, of course. */
  {
    int offs = LEADING_BYTE_ASCII - MIN_LEADING_BYTE;

    if (!cachel->font_specified[offs] && FINDEX_FIELD (font_specified[offs]))
      {
	cachel->font[offs] = FINDEX_FIELD (font[offs]);
	cachel->font_specified[offs] = 1;
	cachel->dirty = 1;
      }
  }

#undef FROB
#undef FINDEX_FIELD

  cachel->updated = 1;
}

/* Initialize a cachel. */

void
reset_face_cachel (struct face_cachel *cachel)
{
  memset (cachel, 0, sizeof (struct face_cachel));
  cachel->face = Qunbound;
  cachel->nfaces = 0;
  cachel->merged_faces = 0;
  cachel->foreground = Qunbound;
  cachel->background = Qunbound;
  {
    int i;

    for (i = 0; i < NUM_LEADING_BYTES; i++)
      cachel->font[i] = Qunbound;
  }
  cachel->display_table = Qunbound;
  cachel->background_pixmap = Qunbound;
}

/* Add a cachel for the given face to the given window's cache. */

static void
add_face_cachel (struct window *w, Lisp_Object face)
{
  struct face_cachel new_cachel;
  Lisp_Object window;

  reset_face_cachel (&new_cachel);
  XSETWINDOW (window, w);
  update_face_cachel_data (&new_cachel, window, face);
  Dynarr_add (w->face_cachels, new_cachel);
}

/* Retrieve the index to a cachel for window W that corresponds to
   the specified face.  If necessary, add a new element to the
   cache. */

face_index
get_builtin_face_cache_index (struct window *w, Lisp_Object face)
{
  int elt;

  if (noninteractive)
    return 0;

  for (elt = 0; elt < Dynarr_length (w->face_cachels); elt++)
    {
      struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, elt);

      if (EQ (cachel->face, face))
	{
	  Lisp_Object window = Qnil;
	  XSETWINDOW (window, w);
	  if (!cachel->updated)
	    update_face_cachel_data (cachel, window, face);
	  return elt;
	}
    }

  /* If we didn't find the face, add it and then return its index. */
  add_face_cachel (w, face);
  return elt;
}

void
reset_face_cachels (struct window *w)
{
  /* #### Not initialized in batch mode for the stream device. */
  if (w->face_cachels)
    {
      int i;

      for (i = 0; i < Dynarr_length (w->face_cachels); i++)
	{
	  struct face_cachel *cachel = Dynarr_atp (w->face_cachels, i);
	  if (cachel->merged_faces)
	    Dynarr_free (cachel->merged_faces);
	}
      Dynarr_reset (w->face_cachels);
      get_builtin_face_cache_index (w, Vdefault_face);
      get_builtin_face_cache_index (w, Vmodeline_face);
      XFRAME (w->frame)->window_face_cache_reset = 1;
    }
}

void
mark_face_cachels_as_clean (struct window *w)
{
  int elt;

  for (elt = 0; elt < Dynarr_length (w->face_cachels); elt++)
    Dynarr_atp (w->face_cachels, elt)->dirty = 0;
}

void
mark_face_cachels_as_not_updated (struct window *w)
{
  int elt;

  for (elt = 0; elt < Dynarr_length (w->face_cachels); elt++)
    {
      struct face_cachel *cachel = Dynarr_atp (w->face_cachels, elt);
      int i;

      cachel->updated = 0;
      for (i = 0; i < NUM_LEADING_BYTES; i++)
	cachel->font_updated[i] = 0;
    }
}

#ifdef MEMORY_USAGE_STATS

int
compute_face_cachel_usage (face_cachel_dynarr *face_cachels,
			   struct overhead_stats *ovstats)
{
  int total = 0;

  if (face_cachels)
    {
      int i;

      total += Dynarr_memory_usage (face_cachels, ovstats);
      for (i = 0; i < Dynarr_length (face_cachels); i++)
	{
	  int_dynarr *merged = Dynarr_at (face_cachels, i).merged_faces;
	  if (merged)
	    total += Dynarr_memory_usage (merged, ovstats);
	}
    }

  return total;
}

#endif /* MEMORY_USAGE_STATS */


/*****************************************************************************
 *                             merged face functions                         *
 *****************************************************************************/

/* Compare two merged face cachels to determine whether we have to add
   a new entry to the face cache.

   Note that we do not compare the attributes, but just the faces the
   cachels are based on.  If they are the same, then the cachels certainly
   ought to have the same attributes, except in the case where fonts
   for different charsets have been determined in the two -- and in that
   case this difference is fine. */

static int
compare_merged_face_cachels (struct face_cachel *cachel1,
			     struct face_cachel *cachel2)
{
  int i;

  if (!EQ (cachel1->face, cachel2->face)
      || cachel1->nfaces != cachel2->nfaces)
    return 0;

  for (i = 0; i < cachel1->nfaces; i++)
    if (FACE_CACHEL_FINDEX_UNSAFE (cachel1, i)
	!= FACE_CACHEL_FINDEX_UNSAFE (cachel2, i))
      return 0;

  return 1;
}

/* Retrieve the index to a cachel for window W that corresponds to
   the specified cachel.  If necessary, add a new element to the
   cache.  This is similar to get_builtin_face_cache_index() but
   is intended for merged cachels rather than for cachels representing
   just a face.

   Note that a merged cachel for just one face is not the same as
   the simple cachel for that face, because it is also merged with
   the default face. */

static face_index
get_merged_face_cache_index (struct window *w,
			     struct face_cachel *merged_cachel)
{
  int elt;
  int cache_size = Dynarr_length (w->face_cachels);

  for (elt = 0; elt < cache_size; elt++)
    {
      struct face_cachel *cachel =
	Dynarr_atp (w->face_cachels, elt);

      if (compare_merged_face_cachels (cachel, merged_cachel))
	return elt;
    }

  /* We didn't find it so add this instance to the cache. */
  merged_cachel->updated = 1;
  merged_cachel->dirty = 1;
  Dynarr_add (w->face_cachels, *merged_cachel);
  return cache_size;
}

face_index
get_extent_fragment_face_cache_index (struct window *w,
				      struct extent_fragment *ef)
{
  struct face_cachel cachel;
  int len = Dynarr_length (ef->extents);
  face_index findex = 0;
  Lisp_Object window = Qnil;
  XSETWINDOW (window, w);

  /* Optimize the default case. */
  if (len == 0)
    return DEFAULT_INDEX;
  else
    {
      int i;

      /* Merge the faces of the extents together in order. */

      reset_face_cachel (&cachel);

      for (i = len - 1; i >= 0; i--)
	{
	  EXTENT current = Dynarr_at (ef->extents, i);
	  int has_findex = 0;
	  Lisp_Object face = extent_face (current);

	  if (FACEP (face))
	    {
	      findex = get_builtin_face_cache_index (w, face);
	      has_findex = 1;
	      merge_face_cachel_data (w, findex, &cachel);
	    }
	  /* remember, we're called from within redisplay
	     so we can't error. */
	  else while (CONSP (face))
	    {
	      Lisp_Object one_face = XCAR (face);
	      if (FACEP (one_face))
		{
		  findex = get_builtin_face_cache_index (w, one_face);
		  merge_face_cachel_data (w, findex, &cachel);

		  /* code duplication here but there's no clean
		     way to avoid it. */
		  if (cachel.nfaces >= NUM_STATIC_CACHEL_FACES)
		    {
		      if (!cachel.merged_faces)
			cachel.merged_faces = Dynarr_new (int);
		      Dynarr_add (cachel.merged_faces, findex);
		    }
		  else
		    cachel.merged_faces_static[cachel.nfaces] = findex;
		  cachel.nfaces++;
		}
	      face = XCDR (face);
	    }

	  if (has_findex)
	    {
	      if (cachel.nfaces >= NUM_STATIC_CACHEL_FACES)
		{
		  if (!cachel.merged_faces)
		    cachel.merged_faces = Dynarr_new (int);
		  Dynarr_add (cachel.merged_faces, findex);
		}
	      else
		cachel.merged_faces_static[cachel.nfaces] = findex;
	      cachel.nfaces++;
	    }
	}

      /* Now finally merge in the default face. */
      findex = get_builtin_face_cache_index (w, Vdefault_face);
      merge_face_cachel_data (w, findex, &cachel);

      return get_merged_face_cache_index (w, &cachel);
    }
}


/*****************************************************************************
 interface functions
 ****************************************************************************/

/* #### This function should be converted into appropriate device methods. */
static void
update_EmacsFrame (Lisp_Object frame, Lisp_Object name)
{
  struct frame *frm = XFRAME (frame);

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frm))
     {
       Arg av[10];
       int ac = 0;

       if (EQ (name, Qforeground))
	 {
	   Lisp_Object color = FACE_FOREGROUND (Vdefault_face, frame);
	   XColor fgc;

	   if (!EQ (color, Vthe_null_color_instance))
	     {
	       fgc = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (color));
	       XtSetArg (av[ac], XtNforeground, (void *) fgc.pixel); ac++;
	     }
	 }
       else if (EQ (name, Qbackground))
	 {
	   Lisp_Object color = FACE_BACKGROUND (Vdefault_face, frame);
	   XColor bgc;

	   if (!EQ (color, Vthe_null_color_instance))
	     {
	       bgc = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (color));
	       XtSetArg (av[ac], XtNbackground, (void *) bgc.pixel); ac++;
	     }

	   /* Really crappy way to force the modeline shadows to be
              redrawn.  But effective. */
	   MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (frm);
	   MARK_FRAME_CHANGED (frm);
	 }
       else if (EQ (name, Qfont))
	 {
	   Lisp_Object font = FACE_FONT (Vdefault_face, frame, Vcharset_ascii);

	   if (!EQ (font, Vthe_null_font_instance))
	     XtSetArg (av[ac], XtNfont,
		       (void *) FONT_INSTANCE_X_FONT (XFONT_INSTANCE (font)));
	   ac++;
	 }
       else
	 abort ();

       XtSetValues (FRAME_X_TEXT_WIDGET (frm), av, ac);

#ifdef HAVE_TOOLBARS
       /* Setting the background clears the entire frame area
          including the toolbar so we force an immediate redraw of
          it. */
       if (EQ (name, Qbackground))
	 MAYBE_DEVMETH (XDEVICE (frm->device), redraw_frame_toolbars, (frm));
#endif /* HAVE_TOOLBARS */

       /* The intent of this code is to cause the frame size in
	  characters to remain the same when the font changes, at the
	  expense of changing the frame size in pixels.  It's not
	  totally clear that this is the right thing to do, but it's
	  not clearly wrong either.  */
       if (EQ (name, Qfont))
	 {
	   EmacsFrameRecomputeCellSize (FRAME_X_TEXT_WIDGET (frm));
	   Fset_frame_size (frame,
			    make_int (frm->width),
			    make_int (frm->height),
			    Qnil);
	 }
     }
#endif /* HAVE_X_WINDOWS */
}

static void
update_EmacsFrames (Lisp_Object locale, Lisp_Object name)
{
  if (FRAMEP (locale))
    {
      update_EmacsFrame (locale, name);
    }
  else if (DEVICEP (locale))
    {
      Lisp_Object frmcons;

      DEVICE_FRAME_LOOP (frmcons, XDEVICE (locale))
	update_EmacsFrame (XCAR (frmcons), name);
    }
  else if (EQ (locale, Qglobal) || EQ (locale, Qfallback))
    {
      Lisp_Object frmcons, devcons, concons;

      FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	update_EmacsFrame (XCAR (frmcons), name);
    }
}

void
update_frame_face_values (struct frame *f)
{
  Lisp_Object frm = Qnil;

  XSETFRAME (frm, f);
  update_EmacsFrame (frm, Qforeground);
  update_EmacsFrame (frm, Qbackground);
  update_EmacsFrame (frm, Qfont);
}

void
face_property_was_changed (Lisp_Object face, Lisp_Object property,
			   Lisp_Object locale)
{
  int default_face = EQ (face, Vdefault_face);

  /* If the locale could affect the frame value, then call
     update_EmacsFrames just in case. */
  if (default_face &&
      (EQ (property, Qforeground) ||
       EQ (property, Qbackground) ||
       EQ (property, Qfont)))
    update_EmacsFrames (locale, property);

  if (WINDOWP (locale))
    {
      MARK_FRAME_FACES_CHANGED (XFRAME (XWINDOW (locale)->frame));
    }
  else if (FRAMEP (locale))
    {
      MARK_FRAME_FACES_CHANGED (XFRAME (locale));
    }
  else if (DEVICEP (locale))
    {
      MARK_DEVICE_FRAMES_FACES_CHANGED (XDEVICE (locale));
    }
  else
    {
      Lisp_Object devcons, concons;

      DEVICE_LOOP_NO_BREAK (devcons, concons)
	MARK_DEVICE_FRAMES_FACES_CHANGED (XDEVICE (XCAR (devcons)));
    }

  /*
   * This call to update_faces_inheritance isn't needed and makes
   * creating and modifying faces _very_ slow.  The point of
   * update_face_inheritances is to find all faces that inherit
   * directly from this face property and set the specifier "dirty"
   * flag on the corresponding specifier.  This forces recaching of
   * cached specifier values in frame and window struct slots.  But
   * currently no face properties are cached in frame and window
   * struct slots, so calling this function does nothing useful!
   *
   * Further, since update_faces_inheritance maps over the whole
   * face table every time it is called, it gets terribly slow when
   * there are many faces.  Creating 500 faces on a 50Mhz 486 took
   * 433 seconds when update_faces_inheritance was called.  With the
   * call commented out, creating those same 500 faces took 0.72
   * seconds.
   */
  /* update_faces_inheritance (face, property);*/
  XFACE (face)->dirty = 1;
}

DEFUN ("copy-face", Fcopy_face, 2, 6, 0, /*
Defines and returns a new face which is a copy of an existing one,
or makes an already-existing face be exactly like another. LOCALE,
TAG-SET, EXACT-P, and HOW-TO-ADD are as in `copy-specifier'.
*/
       (old_face, new_name, locale, tag_set, exact_p, how_to_add))
{
  struct Lisp_Face *fold, *fnew;
  Lisp_Object new_face = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  old_face = Fget_face (old_face);

  /* We GCPRO old_face because it might be temporary, and GCing could
     occur in various places below. */
  GCPRO4 (tag_set, locale, old_face, new_face);
  /* check validity of how_to_add now. */
  decode_how_to_add_specification (how_to_add);
  /* and of tag_set. */
  tag_set = decode_specifier_tag_set (tag_set);
  /* and of locale. */
  locale = decode_locale_list (locale);

  new_face = Ffind_face (new_name);
  if (NILP (new_face))
    {
      Lisp_Object temp;

      CHECK_SYMBOL (new_name);

      /* Create the new face with the same status as the old face. */
      temp = (NILP (Fgethash (old_face, Vtemporary_faces_cache, Qnil))
	      ? Qnil
	      : Qt);

      new_face = Fmake_face (new_name, Qnil, temp);
    }

  fold = XFACE (old_face);
  fnew = XFACE (new_face);

#define COPY_PROPERTY(property) \
  Fcopy_specifier (fold->property, fnew->property, \
                   locale, tag_set, exact_p, how_to_add);

  COPY_PROPERTY (foreground);
  COPY_PROPERTY (background);
  COPY_PROPERTY (font);
  COPY_PROPERTY (display_table);
  COPY_PROPERTY (background_pixmap);
  COPY_PROPERTY (underline);
  COPY_PROPERTY (strikethru);
  COPY_PROPERTY (highlight);
  COPY_PROPERTY (dim);
  COPY_PROPERTY (blinking);
  COPY_PROPERTY (reverse);
#undef COPY_PROPERTY
  /* #### should it copy the individual specifiers, if they exist? */
  fnew->plist = Fcopy_sequence (fold->plist);

  UNGCPRO;

  return new_name;
}


void
syms_of_faces (void)
{
  /* Qdefault defined in general.c */
  defsymbol (&Qmodeline, "modeline");
  defsymbol (&Qleft_margin, "left-margin");
  defsymbol (&Qright_margin, "right-margin");
  defsymbol (&Qtext_cursor, "text-cursor");

  DEFSUBR (Ffacep);
  DEFSUBR (Ffind_face);
  DEFSUBR (Fget_face);
  DEFSUBR (Fface_name);
  DEFSUBR (Fbuilt_in_face_specifiers);
  DEFSUBR (Fface_list);
  DEFSUBR (Fmake_face);
  DEFSUBR (Fcopy_face);

  defsymbol (&Qfacep, "facep");
  defsymbol (&Qforeground, "foreground");
  defsymbol (&Qbackground, "background");
  /* Qfont defined in general.c */
  defsymbol (&Qdisplay_table, "display-table");
  defsymbol (&Qbackground_pixmap, "background-pixmap");
  defsymbol (&Qunderline, "underline");
  defsymbol (&Qstrikethru, "strikethru");
  /* Qhighlight, Qreverse defined in general.c */
  defsymbol (&Qdim, "dim");
  defsymbol (&Qblinking, "blinking");

  defsymbol (&Qinit_face_from_resources, "init-face-from-resources");
  defsymbol (&Qinit_global_faces, "init-global-faces");
  defsymbol (&Qinit_device_faces, "init-device-faces");
  defsymbol (&Qinit_frame_faces, "init-frame-faces");
}

void
structure_type_create_faces (void)
{
  struct structure_type *st;

  st = define_structure_type (Qface, face_validate, face_instantiate);

  define_structure_type_keyword (st, Qname, face_name_validate);
}

void
vars_of_faces (void)
{
  staticpro (&Vpermanent_faces_cache);
  Vpermanent_faces_cache = Qnil;
  staticpro (&Vtemporary_faces_cache);
  Vtemporary_faces_cache = Qnil;

  staticpro (&Vdefault_face);
  Vdefault_face = Qnil;
  staticpro (&Vmodeline_face);
  Vmodeline_face = Qnil;

  staticpro (&Vleft_margin_face);
  Vleft_margin_face = Qnil;
  staticpro (&Vright_margin_face);
  Vright_margin_face = Qnil;
  staticpro (&Vtext_cursor_face);
  Vtext_cursor_face = Qnil;
  staticpro (&Vpointer_face);
  Vpointer_face = Qnil;

  {
    Lisp_Object syms[20];
    int n = 0;

    syms[n++] = Qforeground;
    syms[n++] = Qbackground;
    syms[n++] = Qfont;
    syms[n++] = Qdisplay_table;
    syms[n++] = Qbackground_pixmap;
    syms[n++] = Qunderline;
    syms[n++] = Qstrikethru;
    syms[n++] = Qhighlight;
    syms[n++] = Qdim;
    syms[n++] = Qblinking;
    syms[n++] = Qreverse;

    Vbuilt_in_face_specifiers = pure_list (n, syms);
    staticpro (&Vbuilt_in_face_specifiers);
  }
}

void
complex_vars_of_faces (void)
{
  Vpermanent_faces_cache = make_lisp_hashtable (10, HASHTABLE_NONWEAK,
						HASHTABLE_EQ);
  Vtemporary_faces_cache = make_lisp_hashtable (0, HASHTABLE_WEAK,
						HASHTABLE_EQ);

  /* Create the default face now so we know what it is immediately. */

  Vdefault_face = Qnil; /* so that Fmake_face() doesn't set up a bogus
			   default value */
  Vdefault_face = Fmake_face (Qdefault, build_string ("default face"),
			      Qnil);

  /* Provide some last-resort fallbacks to avoid utter fuckage if
     someone provides invalid values for the global specifications. */

  {
    Lisp_Object fg_inst_list = Qnil, bg_inst_list = Qnil;

#ifdef HAVE_X_WINDOWS
    fg_inst_list = Fcons (Fcons (list1 (Qx), build_string ("black")),
			  fg_inst_list);
    bg_inst_list = Fcons (Fcons (list1 (Qx), build_string ("white")),
			  bg_inst_list);
#endif
#ifdef HAVE_TTY
    fg_inst_list = Fcons (Fcons (list1 (Qtty), Fvector (0, 0)),
			  fg_inst_list);
    bg_inst_list = Fcons (Fcons (list1 (Qtty), Fvector (0, 0)),
			  bg_inst_list);
#endif
#ifdef HAVE_MS_WINDOWS
    fg_inst_list = Fcons (Fcons (list1 (Qmswindows), build_string ("black")),
			  fg_inst_list);
    bg_inst_list = Fcons (Fcons (list1 (Qmswindows), build_string ("white")),
			  bg_inst_list);
#endif
    set_specifier_fallback (Fget (Vdefault_face, Qforeground, Qnil),
			    fg_inst_list);
    set_specifier_fallback (Fget (Vdefault_face, Qbackground, Qnil),
			    bg_inst_list);
  }

  /* #### We may want to have different fallback values if NeXTstep
     support is compiled in. */
  {
    Lisp_Object inst_list = Qnil;
#ifdef HAVE_X_WINDOWS
    /* The same gory list from x-faces.el.
       (#### Perhaps we should remove the stuff from x-faces.el
       and only depend on this stuff here?  That should work.)
     */
    CONST char *fonts[] =
    {
      "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*",
      "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*",
      "-*-courier-*-r-*-*-*-120-*-*-*-*-iso8859-*",
      "-*-*-medium-r-*-*-*-120-*-*-m-*-iso8859-*",
      "-*-*-medium-r-*-*-*-120-*-*-c-*-iso8859-*",
      "-*-*-*-r-*-*-*-120-*-*-m-*-iso8859-*",
      "-*-*-*-r-*-*-*-120-*-*-c-*-iso8859-*",
      "-*-*-*-r-*-*-*-120-*-*-*-*-iso8859-*",
      "-*-*-medium-r-*-*-*-120-*-*-m-*-*-*",
      "-*-*-medium-r-*-*-*-120-*-*-c-*-*-*",
      "-*-*-*-r-*-*-*-120-*-*-m-*-*-*",
      "-*-*-*-r-*-*-*-120-*-*-c-*-*-*",
      "-*-*-*-r-*-*-*-120-*-*-*-*-*-*",
      "-*-*-*-*-*-*-*-120-*-*-*-*-*-*",
      "*"
    };
    CONST char **fontptr;

    for (fontptr = fonts + countof(fonts) - 1; fontptr >= fonts; fontptr--)
      inst_list = Fcons (Fcons (list1 (Qx), build_string (*fontptr)),
			 inst_list);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_TTY
    inst_list = Fcons (Fcons (list1 (Qtty), build_string ("normal")),
		       inst_list);
#endif /* HAVE_TTY */
#ifdef HAVE_MS_WINDOWS
    inst_list = Fcons (Fcons (list1 (Qmswindows), build_string ("Courier New")),
		       inst_list);
#endif /* HAVE_MS_WINDOWS */
    set_specifier_fallback (Fget (Vdefault_face, Qfont, Qnil), inst_list);
  }

  set_specifier_fallback (Fget (Vdefault_face, Qunderline, Qnil),
			 list1 (Fcons (Qnil, Qnil)));
  set_specifier_fallback (Fget (Vdefault_face, Qstrikethru, Qnil),
			 list1 (Fcons (Qnil, Qnil)));
  set_specifier_fallback (Fget (Vdefault_face, Qhighlight, Qnil),
			 list1 (Fcons (Qnil, Qnil)));
  set_specifier_fallback (Fget (Vdefault_face, Qdim, Qnil),
			 list1 (Fcons (Qnil, Qnil)));
  set_specifier_fallback (Fget (Vdefault_face, Qblinking, Qnil),
			 list1 (Fcons (Qnil, Qnil)));
  set_specifier_fallback (Fget (Vdefault_face, Qreverse, Qnil),
			 list1 (Fcons (Qnil, Qnil)));

  /* Now create the other faces that redisplay needs to refer to
     directly.  We could create them in Lisp but it's simpler this
     way since we need to get them anyway. */
  Vmodeline_face = Fmake_face (Qmodeline, build_string ("modeline face"),
			       Qnil);
  Vleft_margin_face = Fmake_face (Qleft_margin,
				  build_string ("left margin face"),
				  Qnil);
  Vright_margin_face = Fmake_face (Qright_margin,
				   build_string ("right margin face"),
				   Qnil);
  Vtext_cursor_face = Fmake_face (Qtext_cursor,
				  build_string ("face for text cursor"),
				  Qnil);
  Vpointer_face =
    Fmake_face (Qpointer,
		build_string
		("face for foreground/background colors of mouse pointer"),
		Qnil);
}
