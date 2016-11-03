/* Generic Objects and Functions.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002, 2004, 2005, 2010 Ben Wing.
   Copyright (C) 2010 Didier Verna

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

#include "buffer.h"
#include "device-impl.h"
#include "elhash.h"
#include "faces.h"
#include "frame.h"
#include "glyphs.h"
#include "fontcolor-impl.h"
#include "specifier.h"
#include "window.h"

#ifdef HAVE_TTY
#include "console-tty.h"
#endif

/* Objects that are substituted when an instantiation fails.
   If we leave in the Qunbound value, we will probably get crashes. */
Lisp_Object Vthe_null_color_instance, Vthe_null_font_instance;

/* Author: Ben Wing; some earlier code from Chuck Thompson, Jamie
   Zawinski. */

DOESNT_RETURN
finalose (void *ptr)
{
  Lisp_Object obj = wrap_pointer_1 (ptr);

  invalid_operation
    ("Can't dump an emacs containing window system objects", obj);
}


/****************************************************************************
 *                       Color-Instance Object                              *
 ****************************************************************************/

Lisp_Object Qcolor_instancep;

static const struct memory_description color_instance_data_description_1 []= {
#ifdef HAVE_TTY
#ifdef NEW_GC
  { XD_LISP_OBJECT, tty_console },
#else /* not NEW_GC */
  { XD_BLOCK_PTR, tty_console, 1, { &tty_color_instance_data_description } },
#endif /* not NEW_GC */
#endif
  { XD_END }
};

static const struct sized_memory_description color_instance_data_description = {
  sizeof (void *), color_instance_data_description_1
};

static const struct memory_description color_instance_description[] = {
  { XD_INT, offsetof (Lisp_Color_Instance, color_instance_type) },
  { XD_LISP_OBJECT, offsetof (Lisp_Color_Instance, name)},
  { XD_LISP_OBJECT, offsetof (Lisp_Color_Instance, device)},
  { XD_UNION, offsetof (Lisp_Color_Instance, data),
    XD_INDIRECT (0, 0), { &color_instance_data_description } },
  {XD_END}
};

static Lisp_Object
mark_color_instance (Lisp_Object obj)
{
  Lisp_Color_Instance *c = XCOLOR_INSTANCE (obj);
  mark_object (c->name);
  if (!NILP (c->device)) /* Vthe_null_color_instance */
    MAYBE_DEVMETH (XDEVICE (c->device), mark_color_instance, (c));

  return c->device;
}

static void
print_color_instance (Lisp_Object obj, Lisp_Object printcharfun,
		      int escapeflag)
{
  Lisp_Color_Instance *c = XCOLOR_INSTANCE (obj);
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);
  write_fmt_string_lisp (printcharfun, "#<color-instance %s", 1, c->name);
  write_fmt_string_lisp (printcharfun, " on %s", 1, c->device);
  if (!NILP (c->device)) /* Vthe_null_color_instance */
    MAYBE_DEVMETH (XDEVICE (c->device), print_color_instance,
		   (c, printcharfun, escapeflag));
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static void
finalize_color_instance (Lisp_Object obj)
{
  Lisp_Color_Instance *c = XCOLOR_INSTANCE (obj);

  if (!NILP (c->device))
    MAYBE_DEVMETH (XDEVICE (c->device), finalize_color_instance, (c));
}

static int
color_instance_equal (Lisp_Object obj1, Lisp_Object obj2, int depth,
		      int UNUSED (foldcase))
{
  Lisp_Color_Instance *c1 = XCOLOR_INSTANCE (obj1);
  Lisp_Color_Instance *c2 = XCOLOR_INSTANCE (obj2);

  return (c1 == c2) ||
    (EQ (c1->device, c2->device) &&
     DEVICEP (c1->device) &&
     HAS_DEVMETH_P (XDEVICE (c1->device), color_instance_equal) &&
     DEVMETH (XDEVICE (c1->device), color_instance_equal, (c1, c2, depth)));
}

static Hashcode
color_instance_hash (Lisp_Object obj, int depth, Boolint UNUSED (equalp))
{
  Lisp_Color_Instance *c = XCOLOR_INSTANCE (obj);
  struct device *d = DEVICEP (c->device) ? XDEVICE (c->device) : 0;

  return HASH2 ((Hashcode) d,
		!d ? LISP_HASH (obj)
		: DEVMETH_OR_GIVEN (d, color_instance_hash, (c, depth),
				    LISP_HASH (obj)));
}

DEFINE_NODUMP_LISP_OBJECT ("color-instance", color_instance,
			   mark_color_instance, print_color_instance,
			   finalize_color_instance, color_instance_equal,
			   color_instance_hash,
			   color_instance_description,
			   Lisp_Color_Instance);

DEFUN ("make-color-instance", Fmake_color_instance, 1, 3, 0, /*
Return a new `color-instance' object named NAME (a string).

Optional argument DEVICE specifies the device this object applies to
and defaults to the selected device.

An error is signaled if the color is unknown or cannot be allocated;
however, if optional argument NOERROR is non-nil, nil is simply
returned in this case. (And if NOERROR is other than t, a warning may
be issued.)

The returned object is a normal, first-class lisp object.  The way you
`deallocate' the color is the way you deallocate any other lisp object:
you drop all pointers to it and allow it to be garbage collected.  When
these objects are GCed, the underlying window-system data (e.g. X object)
is deallocated as well.
*/
       (name, device, noerror))
{
  Lisp_Object obj;
  Lisp_Color_Instance *c;
  int retval;

  CHECK_STRING (name);
  device = wrap_device (decode_device (device));

  obj = ALLOC_NORMAL_LISP_OBJECT (color_instance);
  c = XCOLOR_INSTANCE (obj);
  c->name = name;
  c->device = device;
  c->data = 0;
  c->color_instance_type = get_console_variant (XDEVICE_TYPE (c->device));

  retval = MAYBE_INT_DEVMETH (XDEVICE (device), initialize_color_instance,
			      (c, name, device,
			       decode_error_behavior_flag (noerror)));
  if (!retval)
    return Qnil;

  return obj;
}

DEFUN ("color-instance-p", Fcolor_instance_p, 1, 1, 0, /*
Return non-nil if OBJECT is a color instance.
*/
       (object))
{
  return COLOR_INSTANCEP (object) ? Qt : Qnil;
}

DEFUN ("color-instance-name", Fcolor_instance_name, 1, 1, 0, /*
Return the name used to allocate COLOR-INSTANCE.
*/
       (color_instance))
{
  CHECK_COLOR_INSTANCE (color_instance);
  return XCOLOR_INSTANCE (color_instance)->name;
}

DEFUN ("color-instance-rgb-components", Fcolor_instance_rgb_components, 1, 1, 0, /*
Return a three element list containing the red, green, and blue
color components of COLOR-INSTANCE, or nil if unknown.
Component values range from 0 to 65535.
*/
       (color_instance))
{
  Lisp_Color_Instance *c;

  CHECK_COLOR_INSTANCE (color_instance);
  c = XCOLOR_INSTANCE (color_instance);

  if (NILP (c->device))
    return Qnil;

  return MAYBE_LISP_DEVMETH (XDEVICE (c->device),
			     color_instance_rgb_components,
			     (c));
}

DEFUN ("valid-color-name-p", Fvalid_color_name_p, 1, 2, 0, /*
Return true if COLOR names a valid color for the current device.

Valid color names for X are listed in the file /usr/lib/X11/rgb.txt, or
whatever the equivalent is on your system.

Valid color names for TTY are those which have an ISO 6429 (ANSI) sequence.
In addition to being a color this may be one of a number of attributes
such as `blink'.
*/
       (color, device))
{
  struct device *d = decode_device (device);

  CHECK_STRING (color);
  return MAYBE_INT_DEVMETH (d, valid_color_name_p, (d, color)) ? Qt : Qnil;
}

DEFUN ("color-list", Fcolor_list, 0, 1, 0, /*
Return a list of color names.
DEVICE specifies which device to return names for, and defaults to the
currently selected device.
*/
       (device))
{
  device = wrap_device (decode_device (device));

  return MAYBE_LISP_DEVMETH (XDEVICE (device), color_list, ());
}


/***************************************************************************
 *                       Font-Instance Object                              *
 ***************************************************************************/

Lisp_Object Qfont_instancep;

static Lisp_Object font_instance_truename_internal (Lisp_Object xfont,
						    Error_Behavior errb);

static const struct memory_description font_instance_data_description_1 []= {
#ifdef HAVE_TTY
#ifdef NEW_GC
  { XD_LISP_OBJECT, tty_console },
#else /* not NEW_GC */
  { XD_BLOCK_PTR, tty_console, 1, { &tty_font_instance_data_description } },
#endif /* not NEW_GC */
#endif
  { XD_END }
};

static const struct sized_memory_description font_instance_data_description = {
  sizeof (void *), font_instance_data_description_1
};

static const struct memory_description font_instance_description[] = {
  { XD_INT, offsetof (Lisp_Font_Instance, font_instance_type) },
  { XD_LISP_OBJECT, offsetof (Lisp_Font_Instance, name)},
  { XD_LISP_OBJECT, offsetof (Lisp_Font_Instance, truename)},
  { XD_LISP_OBJECT, offsetof (Lisp_Font_Instance, device)},
  { XD_LISP_OBJECT, offsetof (Lisp_Font_Instance, charset)},
  { XD_UNION, offsetof (Lisp_Font_Instance, data),
    XD_INDIRECT (0, 0), { &font_instance_data_description } },
  { XD_END }
};


static Lisp_Object
mark_font_instance (Lisp_Object obj)
{
  Lisp_Font_Instance *f = XFONT_INSTANCE (obj);

  mark_object (f->name);
  mark_object (f->truename);
  if (!NILP (f->device)) /* Vthe_null_font_instance */
    MAYBE_DEVMETH (XDEVICE (f->device), mark_font_instance, (f));

  return f->device;
}

static void
print_font_instance (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  Lisp_Font_Instance *f = XFONT_INSTANCE (obj);
  if (print_readably)
    printing_unreadable_lisp_object (obj, 0);
  write_fmt_string_lisp (printcharfun, "#<font-instance %S", 1, f->name);
  write_fmt_string_lisp (printcharfun, " on %s", 1, f->device);
  if (!NILP (f->device))
    {
      MAYBE_DEVMETH (XDEVICE (f->device), print_font_instance,
		     (f, printcharfun, escapeflag));

    }
  write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

static void
finalize_font_instance (Lisp_Object obj)
{
  Lisp_Font_Instance *f = XFONT_INSTANCE (obj);

  if (!NILP (f->device))
    {
      MAYBE_DEVMETH (XDEVICE (f->device), finalize_font_instance, (f));
    }
}

/* Fonts are equal if they resolve to the same name.
   Since we call `font-truename' to do this, and since font-truename is lazy,
   this means the `equal' could cause XListFonts to be run the first time.
 */
static int
font_instance_equal (Lisp_Object obj1, Lisp_Object obj2, int depth,
		     int UNUSED (foldcase))
{
  /* #### should this be moved into a device method? */
  return internal_equal (font_instance_truename_internal
			 (obj1, ERROR_ME_DEBUG_WARN),
			 font_instance_truename_internal
			 (obj2, ERROR_ME_DEBUG_WARN),
			 depth + 1);
}

static Hashcode
font_instance_hash (Lisp_Object obj, int depth, Boolint UNUSED (equalp))
{
  return internal_hash (font_instance_truename_internal
			(obj, ERROR_ME_DEBUG_WARN),
			depth + 1, 0);
}

DEFINE_NODUMP_LISP_OBJECT ("font-instance", font_instance,
			   mark_font_instance, print_font_instance,
			   finalize_font_instance, font_instance_equal,
			   font_instance_hash, font_instance_description,
			   Lisp_Font_Instance);


/* #### Why is this exposed to Lisp?  Used in:
x-frob-font-size, gtk-font-menu-load-font, x-font-menu-load-font-xft,
x-font-menu-load-font-core, mswindows-font-menu-load-font,
mswindows-frob-font-style-and-sizify, mswindows-frob-font-size. */
DEFUN ("make-font-instance", Fmake_font_instance, 1, 4, 0, /*
Return a new `font-instance' object named NAME.
DEVICE specifies the device this object applies to and defaults to the
selected device.  An error is signalled if the font is unknown or cannot
be allocated; however, if NOERROR is non-nil, nil is simply returned in
this case.  CHARSET is used internally.  #### make helper function?

The returned object is a normal, first-class lisp object.  The way you
`deallocate' the font is the way you deallocate any other lisp object:
you drop all pointers to it and allow it to be garbage collected.  When
these objects are GCed, the underlying GUI data is deallocated as well.
*/
       (name, device, noerror, charset))
{
  Lisp_Object obj;
  Lisp_Font_Instance *f;
  int retval = 0;
  Error_Behavior errb = decode_error_behavior_flag (noerror);

  if (ERRB_EQ (errb, ERROR_ME))
    CHECK_STRING (name);
  else if (!STRINGP (name))
    return Qnil;

  device = wrap_device (decode_device (device));

  obj = ALLOC_NORMAL_LISP_OBJECT (font_instance);
  f = XFONT_INSTANCE (obj);
  f->name = name;
  f->truename = Qnil;
  f->device = device;

  f->data = 0;
  f->font_instance_type = get_console_variant (XDEVICE_TYPE (f->device));

  /* Stick some default values here ... */
  f->ascent = f->height = 1;
  f->descent = 0;
  f->width = 1;
  f->charset = charset;
  f->proportional_p = 0;

  retval = MAYBE_INT_DEVMETH (XDEVICE (device), initialize_font_instance,
			      (f, name, device, errb));

  if (!retval)
    return Qnil;

  return obj;
}

DEFUN ("font-instance-p", Ffont_instance_p, 1, 1, 0, /*
Return non-nil if OBJECT is a font instance.
*/
       (object))
{
  return FONT_INSTANCEP (object) ? Qt : Qnil;
}

DEFUN ("font-instance-name", Ffont_instance_name, 1, 1, 0, /*
Return the name used to allocate FONT-INSTANCE.
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return XFONT_INSTANCE (font_instance)->name;
}

DEFUN ("font-instance-ascent", Ffont_instance_ascent, 1, 1, 0, /*
Return the ascent in pixels of FONT-INSTANCE.
The returned value is the maximum ascent for all characters in the font,
where a character's ascent is the number of pixels above (and including)
the baseline.
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return make_fixnum (XFONT_INSTANCE (font_instance)->ascent);
}

DEFUN ("font-instance-descent", Ffont_instance_descent, 1, 1, 0, /*
Return the descent in pixels of FONT-INSTANCE.
The returned value is the maximum descent for all characters in the font,
where a character's descent is the number of pixels below the baseline.
\(Many characters to do not have any descent.  Typical characters with a
descent are lowercase p and lowercase g.)
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return make_fixnum (XFONT_INSTANCE (font_instance)->descent);
}

DEFUN ("font-instance-width", Ffont_instance_width, 1, 1, 0, /*
Return the width in pixels of FONT-INSTANCE.
The returned value is the average width for all characters in the font.
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return make_fixnum (XFONT_INSTANCE (font_instance)->width);
}

DEFUN ("font-instance-proportional-p", Ffont_instance_proportional_p, 1, 1, 0, /*
Return whether FONT-INSTANCE is proportional.
This means that different characters in the font have different widths.
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return XFONT_INSTANCE (font_instance)->proportional_p ? Qt : Qnil;
}

static Lisp_Object
font_instance_truename_internal (Lisp_Object font_instance,
				 Error_Behavior errb)
{
  Lisp_Font_Instance *f = XFONT_INSTANCE (font_instance);

  if (NILP (f->device))
    {
      maybe_signal_error (Qgui_error,
			  "can't determine truename: "
			  "no device for font instance",
			  font_instance, Qfont, errb);
      return Qnil;
    }

  return DEVMETH_OR_GIVEN (XDEVICE (f->device),
			   font_instance_truename, (f, errb), f->name);
}

DEFUN ("font-instance-truename", Ffont_instance_truename, 1, 1, 0, /*
Return the canonical name of FONT-INSTANCE.
Font names are patterns which may match any number of fonts, of which
the first found is used.  This returns an unambiguous name for that font
\(but not necessarily its only unambiguous name).
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return font_instance_truename_internal (font_instance, ERROR_ME);
}

DEFUN ("font-instance-charset", Ffont_instance_charset, 1, 1, 0, /*
Return the Mule charset that FONT-INSTANCE was allocated to handle.
*/
       (font_instance))
{
  CHECK_FONT_INSTANCE (font_instance);
  return XFONT_INSTANCE (font_instance)->charset;
}

DEFUN ("font-instance-properties", Ffont_instance_properties, 1, 1, 0, /*
Return the properties (an alist or nil) of FONT-INSTANCE.
*/
       (font_instance))
{
  Lisp_Font_Instance *f;

  CHECK_FONT_INSTANCE (font_instance);
  f = XFONT_INSTANCE (font_instance);

  if (NILP (f->device))
    return Qnil;

  return MAYBE_LISP_DEVMETH (XDEVICE (f->device),
			     font_instance_properties, (f));
}

DEFUN ("font-list", Ffont_list, 1, 3, 0, /*
Return a list of font names matching the given pattern.
DEVICE specifies which device to search for names, and defaults to the
currently selected device.
*/
       (pattern, device, maxnumber))
{
  CHECK_STRING (pattern);
  device = wrap_device (decode_device (device));

  return MAYBE_LISP_DEVMETH (XDEVICE (device), font_list, (pattern, device,
							    maxnumber));
}


/****************************************************************************
 Color Object
 ***************************************************************************/

static const struct memory_description color_specifier_description[] = {
  { XD_LISP_OBJECT, offsetof (struct color_specifier, face) },
  { XD_LISP_OBJECT, offsetof (struct color_specifier, face_property) },
  { XD_END }
};

DEFINE_SPECIFIER_TYPE_WITH_DATA (color);
/* Qcolor defined in general.c */

static void
color_create (Lisp_Object obj)
{
  Lisp_Specifier *color = XCOLOR_SPECIFIER (obj);

  COLOR_SPECIFIER_FACE (color) = Qnil;
  COLOR_SPECIFIER_FACE_PROPERTY (color) = Qnil;
}

static void
color_mark (Lisp_Object obj)
{
  Lisp_Specifier *color = XCOLOR_SPECIFIER (obj);

  mark_object (COLOR_SPECIFIER_FACE (color));
  mark_object (COLOR_SPECIFIER_FACE_PROPERTY (color));
}

/* No equal or hash methods; ignore the face the color is based off
   of for `equal' */

static Lisp_Object
color_instantiate (Lisp_Object specifier, Lisp_Object UNUSED (matchspec),
		   Lisp_Object domain, Lisp_Object instantiator,
		   Lisp_Object depth, int no_fallback)
{
  /* When called, we're inside of call_with_suspended_errors(),
     so we can freely error. */
  Lisp_Object device = DOMAIN_DEVICE (domain);
  struct device *d = XDEVICE (device);

  if (COLOR_INSTANCEP (instantiator))
    {
      /* If we are on the same device then we're done.  Otherwise change
	 the instantiator to the name used to generate the pixel and let the
	 STRINGP case deal with it. */
      if (NILP (device) /* Vthe_null_color_instance */
	  || EQ (device, XCOLOR_INSTANCE (instantiator)->device))
	return instantiator;
      else
	instantiator = Fcolor_instance_name (instantiator);
    }

  if (STRINGP (instantiator))
    {
      /* First, look to see if we can retrieve a cached value. */
      Lisp_Object instance =
	Fgethash (instantiator, d->color_instance_cache, Qunbound);
      /* Otherwise, make a new one. */
      if (UNBOUNDP (instance))
	{
	  /* make sure we cache the failures, too. */
	  instance = Fmake_color_instance (instantiator, device, Qt);
	  Fputhash (instantiator, instance, d->color_instance_cache);
	}

      return NILP (instance) ? Qunbound : instance;
    }
  else if (VECTORP (instantiator))
    {
      switch (XVECTOR_LENGTH (instantiator))
	{
	case 0:
	  if (DEVICE_TTY_P (d))
	    return Vthe_null_color_instance;
	  else
	    gui_error ("Color instantiator [] only valid on TTY's",
				 device);

	case 1:
	  if (NILP (COLOR_SPECIFIER_FACE (XCOLOR_SPECIFIER (specifier))))
	    gui_error ("Color specifier not attached to a face",
				 instantiator);
	  return (FACE_PROPERTY_INSTANCE_1
		  (Fget_face (XVECTOR_DATA (instantiator)[0]),
		   COLOR_SPECIFIER_FACE_PROPERTY
		   (XCOLOR_SPECIFIER (specifier)),
		   domain, ERROR_ME, no_fallback, depth));

	case 2:
	  return (FACE_PROPERTY_INSTANCE_1
		  (Fget_face (XVECTOR_DATA (instantiator)[0]),
		   XVECTOR_DATA (instantiator)[1], domain, ERROR_ME,
		   no_fallback, depth));

	default:
	  ABORT ();
	}
    }
  else if (NILP (instantiator))
    {
      if (DEVICE_TTY_P (d))
	return Vthe_null_color_instance;
      else
	gui_error ("Color instantiator [] only valid on TTY's",
			     device);
    }
  else
    ABORT ();	/* The spec validation routines are screwed up. */

  return Qunbound;
}

static void
color_validate (Lisp_Object instantiator)
{
  if (COLOR_INSTANCEP (instantiator) || STRINGP (instantiator))
    return;
  if (VECTORP (instantiator))
    {
      if (XVECTOR_LENGTH (instantiator) > 2)
	sferror ("Inheritance vector must be of size 0 - 2",
			     instantiator);
      else if (XVECTOR_LENGTH (instantiator) > 0)
	{
	  Lisp_Object face = XVECTOR_DATA (instantiator)[0];

	  Fget_face (face);
	  if (XVECTOR_LENGTH (instantiator) == 2)
	    {
	      Lisp_Object field = XVECTOR_DATA (instantiator)[1];
	      if (!EQ (field, Qforeground)
		  && !EQ (field, Qforeback)
		  && !EQ (field, Qbackground))
		invalid_constant
("Inheritance field must be `foreground', `foreback' or `background'",
 field);
	    }
	}
    }
  else
    invalid_argument ("Invalid color instantiator", instantiator);
}

static void
color_after_change (Lisp_Object specifier, Lisp_Object locale)
{
  Lisp_Object face = COLOR_SPECIFIER_FACE (XCOLOR_SPECIFIER (specifier));
  Lisp_Object property =
    COLOR_SPECIFIER_FACE_PROPERTY (XCOLOR_SPECIFIER (specifier));
  if (!NILP (face))
    {
      face_property_was_changed (face, property, locale);
      if (BUFFERP (locale))
	XBUFFER (locale)->buffer_local_face_property = 1;
    }
}

void
set_color_attached_to (Lisp_Object obj, Lisp_Object face, Lisp_Object property)
{
  Lisp_Specifier *color = XCOLOR_SPECIFIER (obj);

  COLOR_SPECIFIER_FACE (color) = face;
  COLOR_SPECIFIER_FACE_PROPERTY (color) = property;
}

DEFUN ("color-specifier-p", Fcolor_specifier_p, 1, 1, 0, /*
Return t if OBJECT is a color specifier.

See `make-color-specifier' for a description of possible color instantiators.
*/
       (object))
{
  return COLOR_SPECIFIERP (object) ? Qt : Qnil;
}


/****************************************************************************
 Font Object
 ***************************************************************************/

static const struct memory_description font_specifier_description[] = {
  { XD_LISP_OBJECT, offsetof (struct font_specifier, face) },
  { XD_LISP_OBJECT, offsetof (struct font_specifier, face_property) },
  { XD_END }
};

DEFINE_SPECIFIER_TYPE_WITH_DATA (font);
/* Qfont defined in general.c */

static void
font_create (Lisp_Object obj)
{
  Lisp_Specifier *font = XFONT_SPECIFIER (obj);

  FONT_SPECIFIER_FACE (font) = Qnil;
  FONT_SPECIFIER_FACE_PROPERTY (font) = Qnil;
}

static void
font_mark (Lisp_Object obj)
{
  Lisp_Specifier *font = XFONT_SPECIFIER (obj);

  mark_object (FONT_SPECIFIER_FACE (font));
  mark_object (FONT_SPECIFIER_FACE_PROPERTY (font));
}

/* No equal or hash methods; ignore the face the font is based off
   of for `equal' */

#ifdef MULE

/* Given a truename font spec (i.e. the font spec should have its registry
   field filled in), does it support displaying characters from CHARSET? */

static int
font_spec_matches_charset (struct device *d, Lisp_Object charset,
			   const Ibyte *nonreloc, Lisp_Object reloc,
			   Bytecount offset, Bytecount length,
			   enum font_specifier_matchspec_stages stage)
{
  return DEVMETH_OR_GIVEN (d, font_spec_matches_charset,
			   (d, charset, nonreloc, reloc, offset, length,
			    stage),
			   1);
}

static void
font_validate_matchspec (Lisp_Object matchspec)
{
  CHECK_CONS (matchspec);
  Fget_charset (XCAR (matchspec));

  do
    {
      if (EQ(XCDR(matchspec), Qinitial))
	{
	  break;
	}
      if (EQ(XCDR(matchspec), Qfinal))
	{
	  break;
	}

      invalid_argument("Invalid font matchspec stage",
		       XCDR(matchspec));
    } while (0);
}

void
initialize_charset_font_caches (struct device *d)
{
  /* Note that the following tables are bi-level. */
  d->charset_font_cache_stage_1 =
    make_lisp_hash_table (20, HASH_TABLE_NON_WEAK, Qeq);
  d->charset_font_cache_stage_2 =
    make_lisp_hash_table (20, HASH_TABLE_NON_WEAK, Qeq);
}

void
invalidate_charset_font_caches (Lisp_Object charset)
{
  /* Invalidate font cache entries for charset on all devices. */
  Lisp_Object devcons, concons, hash_table;
  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      hash_table = Fgethash (charset, d->charset_font_cache_stage_1,
			     Qunbound);
      if (!UNBOUNDP (hash_table))
	Fclrhash (hash_table);
      hash_table = Fgethash (charset, d->charset_font_cache_stage_2,
			     Qunbound);
      if (!UNBOUNDP (hash_table))
	Fclrhash (hash_table);
    }
}

#endif /* MULE */

/* It's a little non-obvious what's going on here.  Specifically:

   MATCHSPEC is a somewhat bogus way in the specifier mechanism of passing
   in additional information needed to instantiate some object.  For fonts,
   it's a cons of (CHARSET . SECOND-STAGE-P).  SECOND-STAGE-P, if set,
   means "try harder to find an appropriate font" and is a very bogus way
   of dealing with the fact that it may not be possible to may a charset
   directly onto a font; it's used esp. under Windows.  @@#### We need to
   change this so that MATCHSPEC is just a character.

   When redisplay is building up its structure, and needs font info, it
   calls functions in faces.c such as ensure_face_cachel_complete() (map
   fonts needed for a string of text) or
   ensure_face_cachel_contains_charset() (map fonts needed for a charset
   derived from a single character).  The former function calls the latter;
   the latter calls face_property_matching_instance(); this constructs the
   MATCHSPEC and calls specifier_instance_no_quit() twice (first stage and
   second stage, updating MATCHSPEC appropriately).  That function, in
   turn, looks up the appropriate specifier method to do the instantiation,
   which, lo and behold, is this function here (because we set it in
   initialization using `SPECIFIER_HAS_METHOD (font, instantiate);').  We
   in turn call the device method `find_charset_font', which maps to
   mswindows_find_charset_font(), x_find_charset_font(), or similar, in
   fontcolor-msw.c or the like.

   --ben */

static Lisp_Object
font_instantiate (Lisp_Object UNUSED (specifier),
		  Lisp_Object USED_IF_MULE (matchspec),
		  Lisp_Object domain, Lisp_Object instantiator,
		  Lisp_Object depth, int no_fallback)
{
  /* When called, we're inside of call_with_suspended_errors(),
     so we can freely error. */
  Lisp_Object device = DOMAIN_DEVICE (domain);
  struct device *d = XDEVICE (device);
  Lisp_Object instance;
  Lisp_Object charset = Qnil;
#ifdef MULE
  enum font_specifier_matchspec_stages stage = STAGE_INITIAL;

  if (!UNBOUNDP (matchspec))
    {
      charset = Fget_charset (XCAR (matchspec));

#define FROB(new_stage, enumstage)			\
          if (EQ(Q##new_stage, XCDR(matchspec)))	\
	    {						\
	      stage = enumstage;			\
	    }

	  FROB (initial, STAGE_INITIAL)
	  else FROB (final, STAGE_FINAL)
	  else assert(0);

#undef FROB

    }
#endif

  if (FONT_INSTANCEP (instantiator))
    {
      if (NILP (device)
	  || EQ (device, XFONT_INSTANCE (instantiator)->device))
	{
#ifdef MULE
	  if (font_spec_matches_charset (d, charset, 0,
					 Ffont_instance_truename
					 (instantiator),
					 0, -1, stage))
#endif
	    return instantiator;
	}
      instantiator = Ffont_instance_name (instantiator);
    }

  if (STRINGP (instantiator))
    {
#ifdef MULE
      /* #### rename these caches. */
      Lisp_Object cache = stage == STAGE_FINAL ?
	d->charset_font_cache_stage_2 :
	d->charset_font_cache_stage_1;
#else
      Lisp_Object cache = d->font_instance_cache;
#endif

#ifdef MULE
      if (!NILP (charset))
	{
	  /* The instantiator is a font spec that could match many
	     different fonts.  We need to find one of those fonts
	     whose registry matches the registry of the charset in
	     MATCHSPEC.  This is potentially a very slow operation,
	     as it involves doing an XListFonts() or equivalent to
	     iterate over all possible fonts, and a regexp match
	     on each one.  So we cache the results. */
	  Lisp_Object matching_font = Qunbound;
	  Lisp_Object hash_table = Fgethash (charset, cache, Qunbound);
	  if (UNBOUNDP (hash_table))
	    {
	      /* need to make a sub hash table. */
	      hash_table = make_lisp_hash_table (20, HASH_TABLE_KEY_WEAK,
						 Qequal);
	      Fputhash (charset, hash_table, cache);
	    }
	  else
	    matching_font = Fgethash (instantiator, hash_table, Qunbound);

	  if (UNBOUNDP (matching_font))
	    {
	      /* make sure we cache the failures, too. */
	      matching_font =
		DEVMETH_OR_GIVEN (d, find_charset_font,
				  (device, instantiator, charset, stage),
				  instantiator);
	      Fputhash (instantiator, matching_font, hash_table);
	    }
	  if (NILP (matching_font))
	    return Qunbound;
	  instantiator = matching_font;
	}
#endif /* MULE */

      /* First, look to see if we can retrieve a cached value. */
      instance = Fgethash (instantiator, cache, Qunbound);
      /* Otherwise, make a new one. */
      if (UNBOUNDP (instance))
	{
	  /* make sure we cache the failures, too. */
	  instance = Fmake_font_instance (instantiator, device, Qt, charset);
	  Fputhash (instantiator, instance, cache);
	}

      return NILP (instance) ? Qunbound : instance;
    }
  else if (VECTORP (instantiator))
    {
      Lisp_Object match_inst = Qunbound;
      assert (XVECTOR_LENGTH (instantiator) == 1);

      match_inst = face_property_matching_instance
	(Fget_face (XVECTOR_DATA (instantiator)[0]), Qfont,
	 charset, domain, ERROR_ME, no_fallback, depth, STAGE_INITIAL);

      if (UNBOUNDP(match_inst))
	{
	  match_inst = face_property_matching_instance
	    (Fget_face (XVECTOR_DATA (instantiator)[0]), Qfont,
	     charset, domain, ERROR_ME, no_fallback, depth, STAGE_FINAL);
	}

      return match_inst;

    }
  else if (NILP (instantiator))
    return Qunbound;
  else
    ABORT ();	/* Eh? */

  return Qunbound;
}

static void
font_validate (Lisp_Object instantiator)
{
  if (FONT_INSTANCEP (instantiator) || STRINGP (instantiator))
    return;
  if (VECTORP (instantiator))
    {
      if (XVECTOR_LENGTH (instantiator) != 1)
	{
	  sferror
	    ("Vector length must be one for font inheritance", instantiator);
	}
      Fget_face (XVECTOR_DATA (instantiator)[0]);
    }
  else
    invalid_argument ("Must be string, vector, or font-instance",
			 instantiator);
}

static void
font_after_change (Lisp_Object specifier, Lisp_Object locale)
{
  Lisp_Object face = FONT_SPECIFIER_FACE (XFONT_SPECIFIER (specifier));
  Lisp_Object property =
    FONT_SPECIFIER_FACE_PROPERTY (XFONT_SPECIFIER (specifier));
  if (!NILP (face))
    {
      face_property_was_changed (face, property, locale);
      if (BUFFERP (locale))
	XBUFFER (locale)->buffer_local_face_property = 1;
    }
}

void
set_font_attached_to (Lisp_Object obj, Lisp_Object face, Lisp_Object property)
{
  Lisp_Specifier *font = XFONT_SPECIFIER (obj);

  FONT_SPECIFIER_FACE (font) = face;
  FONT_SPECIFIER_FACE_PROPERTY (font) = property;
}

DEFUN ("font-specifier-p", Ffont_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a font specifier.

See `make-font-specifier' for a description of possible font instantiators.
*/
       (object))
{
  return FONT_SPECIFIERP (object) ? Qt : Qnil;
}


/*****************************************************************************
 Face Boolean Object
 ****************************************************************************/

static const struct memory_description face_boolean_specifier_description[] = {
  { XD_LISP_OBJECT, offsetof (struct face_boolean_specifier, face) },
  { XD_LISP_OBJECT, offsetof (struct face_boolean_specifier, face_property) },
  { XD_END }
};

DEFINE_SPECIFIER_TYPE_WITH_DATA (face_boolean);
Lisp_Object Qface_boolean;

static void
face_boolean_create (Lisp_Object obj)
{
  Lisp_Specifier *face_boolean = XFACE_BOOLEAN_SPECIFIER (obj);

  FACE_BOOLEAN_SPECIFIER_FACE (face_boolean) = Qnil;
  FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY (face_boolean) = Qnil;
}

static void
face_boolean_mark (Lisp_Object obj)
{
  Lisp_Specifier *face_boolean = XFACE_BOOLEAN_SPECIFIER (obj);

  mark_object (FACE_BOOLEAN_SPECIFIER_FACE (face_boolean));
  mark_object (FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY (face_boolean));
}

/* No equal or hash methods; ignore the face the face-boolean is based off
   of for `equal' */

static Lisp_Object
face_boolean_instantiate (Lisp_Object specifier,
			  Lisp_Object UNUSED (matchspec),
			  Lisp_Object domain, Lisp_Object instantiator,
			  Lisp_Object depth, int no_fallback)
{
  /* When called, we're inside of call_with_suspended_errors(),
     so we can freely error. */
  if (NILP (instantiator) || EQ (instantiator, Qt))
    return instantiator;
  else if (VECTORP (instantiator))
    {
      Lisp_Object retval;
      Lisp_Object prop;
      Elemcount instantiator_len = XVECTOR_LENGTH (instantiator);

      assert (instantiator_len >= 1 && instantiator_len <= 3);
      if (instantiator_len > 1)
	prop = XVECTOR_DATA (instantiator)[1];
      else
	{
	  if (NILP (FACE_BOOLEAN_SPECIFIER_FACE
		    (XFACE_BOOLEAN_SPECIFIER (specifier))))
	    gui_error
	      ("Face-boolean specifier not attached to a face", instantiator);
	  prop = FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY
	    (XFACE_BOOLEAN_SPECIFIER (specifier));
	}

      retval = (FACE_PROPERTY_INSTANCE_1
		(Fget_face (XVECTOR_DATA (instantiator)[0]),
		 prop, domain, ERROR_ME, no_fallback, depth));

      if (instantiator_len == 3 && !NILP (XVECTOR_DATA (instantiator)[2]))
	retval = NILP (retval) ? Qt : Qnil;

      return retval;
    }
  else
    ABORT ();	/* Eh? */

  return Qunbound;
}

static void
face_boolean_validate (Lisp_Object instantiator)
{
  if (NILP (instantiator) || EQ (instantiator, Qt))
    return;
  else if (VECTORP (instantiator) &&
	   (XVECTOR_LENGTH (instantiator) >= 1 &&
	    XVECTOR_LENGTH (instantiator) <= 3))
    {
      Lisp_Object face = XVECTOR_DATA (instantiator)[0];

      Fget_face (face);

      if (XVECTOR_LENGTH (instantiator) > 1)
	{
	  Lisp_Object field = XVECTOR_DATA (instantiator)[1];
	  if (!EQ (field, Qunderline)
	      && !EQ (field, Qstrikethru)
	      && !EQ (field, Qhighlight)
	      && !EQ (field, Qdim)
	      && !EQ (field, Qblinking)
	      && !EQ (field, Qreverse)
	      && !EQ (field, Qshrink))
	    invalid_constant ("Invalid face-boolean inheritance field",
			      field);
	}
    }
  else if (VECTORP (instantiator))
    sferror ("Wrong length for face-boolean inheritance spec",
			 instantiator);
  else
    invalid_argument ("Face-boolean instantiator must be nil, t, or vector",
			 instantiator);
}

static void
face_boolean_after_change (Lisp_Object specifier, Lisp_Object locale)
{
  Lisp_Object face =
    FACE_BOOLEAN_SPECIFIER_FACE (XFACE_BOOLEAN_SPECIFIER (specifier));
  Lisp_Object property =
    FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY (XFACE_BOOLEAN_SPECIFIER (specifier));
  if (!NILP (face))
    {
      face_property_was_changed (face, property, locale);
      if (BUFFERP (locale))
	XBUFFER (locale)->buffer_local_face_property = 1;
    }
}

void
set_face_boolean_attached_to (Lisp_Object obj, Lisp_Object face,
			      Lisp_Object property)
{
  Lisp_Specifier *face_boolean = XFACE_BOOLEAN_SPECIFIER (obj);

  FACE_BOOLEAN_SPECIFIER_FACE (face_boolean) = face;
  FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY (face_boolean) = property;
}

DEFUN ("face-boolean-specifier-p", Fface_boolean_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a face-boolean specifier.

See `make-face-boolean-specifier' for a description of possible
face-boolean instantiators.
*/
       (object))
{
  return FACE_BOOLEAN_SPECIFIERP (object) ? Qt : Qnil;
}


/*****************************************************************************
 Face Background Placement Object
 ****************************************************************************/
Lisp_Object Qabsolute, Qrelative;

static const struct memory_description
face_background_placement_specifier_description[] = {
  { XD_LISP_OBJECT, offsetof (struct face_background_placement_specifier,
			      face) },
  { XD_END }
};

DEFINE_SPECIFIER_TYPE_WITH_DATA (face_background_placement);
Lisp_Object Qface_background_placement;

static void
face_background_placement_create (Lisp_Object obj)
{
  Lisp_Specifier *face_background_placement
    = XFACE_BACKGROUND_PLACEMENT_SPECIFIER (obj);

  FACE_BACKGROUND_PLACEMENT_SPECIFIER_FACE (face_background_placement) = Qnil;
}

static void
face_background_placement_mark (Lisp_Object obj)
{
  Lisp_Specifier *face_background_placement
    = XFACE_BACKGROUND_PLACEMENT_SPECIFIER (obj);

  mark_object
    (FACE_BACKGROUND_PLACEMENT_SPECIFIER_FACE (face_background_placement));
}

/* No equal or hash methods; ignore the face the background-placement is based
   off of for `equal' */

extern Lisp_Object Qbackground_placement;

static Lisp_Object
face_background_placement_instantiate (Lisp_Object UNUSED (specifier),
				       Lisp_Object UNUSED (matchspec),
				       Lisp_Object domain,
				       Lisp_Object instantiator,
				       Lisp_Object depth,
				       int no_fallback)
{
  /* When called, we're inside of call_with_suspended_errors(),
     so we can freely error. */
  if (EQ (instantiator, Qabsolute) || EQ (instantiator, Qrelative))
    return instantiator;
  else if (VECTORP (instantiator))
    {
      assert (XVECTOR_LENGTH (instantiator) == 1);

      return FACE_PROPERTY_INSTANCE_1
	(Fget_face (XVECTOR_DATA (instantiator)[0]),
	 Qbackground_placement, domain, ERROR_ME, no_fallback, depth);
    }
  else
    ABORT ();	/* Eh? */

  return Qunbound;
}

static void
face_background_placement_validate (Lisp_Object instantiator)
{
  if (EQ (instantiator, Qabsolute) || EQ (instantiator, Qrelative))
    return;
  else if (VECTORP (instantiator) &&
	   (XVECTOR_LENGTH (instantiator) == 1))
    {
      Lisp_Object face = XVECTOR_DATA (instantiator)[0];

      Fget_face (face); /* just to check that the face exists -- dvl */
    }
  else if (VECTORP (instantiator))
    sferror ("Wrong length for background-placement inheritance spec",
	     instantiator);
  else
    invalid_argument
      ("\
Background-placement instantiator must be absolute, relative or vector",
       instantiator);
}

static void
face_background_placement_after_change (Lisp_Object specifier,
					Lisp_Object locale)
{
  Lisp_Object face
    = FACE_BACKGROUND_PLACEMENT_SPECIFIER_FACE
    (XFACE_BACKGROUND_PLACEMENT_SPECIFIER (specifier));

  if (!NILP (face))
    {
      face_property_was_changed (face, Qbackground_placement, locale);
      if (BUFFERP (locale))
	XBUFFER (locale)->buffer_local_face_property = 1;
    }
}

void
set_face_background_placement_attached_to (Lisp_Object obj, Lisp_Object face)
{
  Lisp_Specifier *face_background_placement
    = XFACE_BACKGROUND_PLACEMENT_SPECIFIER (obj);

  FACE_BACKGROUND_PLACEMENT_SPECIFIER_FACE (face_background_placement) = face;
}

DEFUN ("face-background-placement-specifier-p", Fface_background_placement_specifier_p, 1, 1, 0, /*
Return non-nil if OBJECT is a face-background-placement specifier.

See `make-face-background-placement-specifier' for a description of possible
face-background-placement instantiators.
*/
       (object))
{
  return FACE_BACKGROUND_PLACEMENT_SPECIFIERP (object) ? Qt : Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_fontcolor (void)
{
  INIT_LISP_OBJECT (color_instance);
  INIT_LISP_OBJECT (font_instance);

  DEFSUBR (Fcolor_specifier_p);
  DEFSUBR (Ffont_specifier_p);
  DEFSUBR (Fface_boolean_specifier_p);
  DEFSUBR (Fface_background_placement_specifier_p);

  DEFSYMBOL_MULTIWORD_PREDICATE (Qcolor_instancep);
  DEFSUBR (Fmake_color_instance);
  DEFSUBR (Fcolor_instance_p);
  DEFSUBR (Fcolor_instance_name);
  DEFSUBR (Fcolor_instance_rgb_components);
  DEFSUBR (Fvalid_color_name_p);
  DEFSUBR (Fcolor_list);

  DEFSYMBOL_MULTIWORD_PREDICATE (Qfont_instancep);
  DEFSUBR (Fmake_font_instance);
  DEFSUBR (Ffont_instance_p);
  DEFSUBR (Ffont_instance_name);
  DEFSUBR (Ffont_instance_ascent);
  DEFSUBR (Ffont_instance_descent);
  DEFSUBR (Ffont_instance_width);
  DEFSUBR (Ffont_instance_charset);
  DEFSUBR (Ffont_instance_proportional_p);
  DEFSUBR (Ffont_instance_truename);
  DEFSUBR (Ffont_instance_properties);
  DEFSUBR (Ffont_list);

  /* Qcolor, Qfont defined in general.c */
  DEFSYMBOL (Qface_boolean);

  DEFSYMBOL (Qface_background_placement);
  DEFSYMBOL (Qabsolute);
  DEFSYMBOL (Qrelative);
}

void
specifier_type_create_fontcolor (void)
{
  INITIALIZE_SPECIFIER_TYPE_WITH_DATA (color, "color", "color-specifier-p");
  INITIALIZE_SPECIFIER_TYPE_WITH_DATA (font, "font", "font-specifier-p");
  INITIALIZE_SPECIFIER_TYPE_WITH_DATA (face_boolean, "face-boolean",
					 "face-boolean-specifier-p");
  INITIALIZE_SPECIFIER_TYPE_WITH_DATA (face_background_placement,
				       "face-background-placement",
				       "\
face-background-placement-specifier-p");

  SPECIFIER_HAS_METHOD (color, instantiate);
  SPECIFIER_HAS_METHOD (font, instantiate);
  SPECIFIER_HAS_METHOD (face_boolean, instantiate);
  SPECIFIER_HAS_METHOD (face_background_placement, instantiate);

  SPECIFIER_HAS_METHOD (color, validate);
  SPECIFIER_HAS_METHOD (font, validate);
  SPECIFIER_HAS_METHOD (face_boolean, validate);
  SPECIFIER_HAS_METHOD (face_background_placement, validate);

  SPECIFIER_HAS_METHOD (color, create);
  SPECIFIER_HAS_METHOD (font, create);
  SPECIFIER_HAS_METHOD (face_boolean, create);
  SPECIFIER_HAS_METHOD (face_background_placement, create);

  SPECIFIER_HAS_METHOD (color, mark);
  SPECIFIER_HAS_METHOD (font, mark);
  SPECIFIER_HAS_METHOD (face_boolean, mark);
  SPECIFIER_HAS_METHOD (face_background_placement, mark);

  SPECIFIER_HAS_METHOD (color, after_change);
  SPECIFIER_HAS_METHOD (font, after_change);
  SPECIFIER_HAS_METHOD (face_boolean, after_change);
  SPECIFIER_HAS_METHOD (face_background_placement, after_change);

#ifdef MULE
  SPECIFIER_HAS_METHOD (font, validate_matchspec);
#endif
}

void
reinit_specifier_type_create_fontcolor (void)
{
  REINITIALIZE_SPECIFIER_TYPE (color);
  REINITIALIZE_SPECIFIER_TYPE (font);
  REINITIALIZE_SPECIFIER_TYPE (face_boolean);
  REINITIALIZE_SPECIFIER_TYPE (face_background_placement);
}

void
reinit_vars_of_fontcolor (void)
{
  {
    Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (color_instance);
    Lisp_Color_Instance *c = XCOLOR_INSTANCE (obj);
    c->name = Qnil;
    c->device = Qnil;
    c->data = 0;

    Vthe_null_color_instance = obj;
    staticpro_nodump (&Vthe_null_color_instance);
  }

  {
    Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (font_instance);
    Lisp_Font_Instance *f = XFONT_INSTANCE (obj);
    f->name = Qnil;
    f->truename = Qnil;
    f->device = Qnil;
    f->data = 0;

    f->ascent = f->height = 0;
    f->descent = 0;
    f->width = 0;
    f->proportional_p = 0;

    Vthe_null_font_instance = obj;
    staticpro_nodump (&Vthe_null_font_instance);
  }
}

void
vars_of_fontcolor (void)
{
}
