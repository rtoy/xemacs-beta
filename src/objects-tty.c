/* TTY-specific Lisp objects.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2001, 2002 Ben Wing.

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

#include <config.h>
#include "lisp.h"

#include "console-tty-impl.h"
#include "insdel.h"
#include "objects-tty-impl.h"
#include "device.h"
#include "charset.h"

#ifdef NEW_GC
# define UNUSED_IF_NEW_GC(decl) UNUSED (decl)
#else
# define UNUSED_IF_NEW_GC(decl) decl
#endif

/* An alist mapping from color names to a cons of (FG-STRING, BG-STRING). */
Lisp_Object Vtty_color_alist;
#if 0 /* This stuff doesn't quite work yet */
Lisp_Object Vtty_dynamic_color_fg;
Lisp_Object Vtty_dynamic_color_bg;
#endif

static const struct memory_description tty_color_instance_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct tty_color_instance_data, symbol) },
  { XD_END }
};

#ifdef NEW_GC
DEFINE_LRECORD_IMPLEMENTATION ("tty-color-instance-data", 
			       tty_color_instance_data,
			       0, /*dumpable-flag*/
                               0, 0, 0, 0, 0, 
			       tty_color_instance_data_description_1,
			       struct tty_color_instance_data);
#else /* not NEW_GC */
const struct sized_memory_description tty_color_instance_data_description = {
  sizeof (struct tty_color_instance_data), tty_color_instance_data_description_1
};
#endif /* not NEW_GC */

static const struct memory_description tty_font_instance_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct tty_font_instance_data, charset) },
  { XD_END }
};

#ifdef NEW_GC
DEFINE_LRECORD_IMPLEMENTATION ("tty-font-instance-data", 
			       tty_font_instance_data,
			       0, /*dumpable-flag*/
                               0, 0, 0, 0, 0, 
			       tty_font_instance_data_description_1,
			       struct tty_font_instance_data);
#else /* not NEW_GC */
const struct sized_memory_description tty_font_instance_data_description = {
  sizeof (struct tty_font_instance_data), tty_font_instance_data_description_1
};
#endif /* not NEW_GC */

DEFUN ("register-tty-color", Fregister_tty_color, 3, 3, 0, /*
Register COLOR as a recognized TTY color.
COLOR should be a string.
Strings FG-STRING and BG-STRING should specify the escape sequences to
 set the foreground and background to the given color, respectively.
*/
       (color, fg_string, bg_string))
{
  CHECK_STRING (color);
  CHECK_STRING (fg_string);
  CHECK_STRING (bg_string);

  color = Fintern (color, Qnil);
  Vtty_color_alist = Fremassq (color, Vtty_color_alist);
  Vtty_color_alist = Fcons (Fcons (color, Fcons (fg_string, bg_string)),
			    Vtty_color_alist);

  return Qnil;
}

DEFUN ("unregister-tty-color", Funregister_tty_color, 1, 1, 0, /*
Unregister COLOR as a recognized TTY color.
*/
       (color))
{
  CHECK_STRING (color);

  color = Fintern (color, Qnil);
  Vtty_color_alist = Fremassq (color, Vtty_color_alist);
  return Qnil;
}

DEFUN ("find-tty-color", Ffind_tty_color, 1, 1, 0, /*
Look up COLOR in the list of registered TTY colors.
If it is found, return a list (FG-STRING BG-STRING) of the escape
sequences used to set the foreground and background to the color, respectively.
If it is not found, return nil.
*/
       (color))
{
  Lisp_Object result;

  CHECK_STRING (color);

  result = Fassq (Fintern (color, Qnil), Vtty_color_alist);
  if (!NILP (result))
    return list2 (Fcar (Fcdr (result)), Fcdr (Fcdr (result)));
  else
    return Qnil;
}

static Lisp_Object
tty_color_list (void)
{
  Lisp_Object result = Qnil;
  Lisp_Object rest;

  LIST_LOOP (rest, Vtty_color_alist)
    {
      result = Fcons (Fsymbol_name (XCAR (XCAR (rest))), result);
    }

  return Fnreverse (result);
}

#if 0

/* This approach is too simplistic.  The problem is that the
   dynamic color settings apply to *all* text in the default color,
   not just the text output after the escape sequence has been given. */

DEFUN ("set-tty-dynamic-color-specs", Fset_tty_dynamic_color_specs, 2, 2, 0, /*
Set the dynamic color specifications for TTY's.
FG and BG should be either nil or vaguely printf-like strings,
where each occurrence of %s is replaced with the color name and each
occurrence of %% is replaced with a single % character.
*/
       (fg, bg))
{
  if (!NILP (fg))
    CHECK_STRING (fg);
  if (!NILP (bg))
    CHECK_STRING (bg);

  Vtty_dynamic_color_fg = fg;
  Vtty_dynamic_color_bg = bg;

  return Qnil;
}

DEFUN ("tty-dynamic-color-specs", Ftty_dynamic_color_specs, 0, 0, 0, /*
Return the dynamic color specifications for TTY's as a list of (FG BG).
See `set-tty-dynamic-color-specs'.
*/
       ())
{
  return list2 (Vtty_dynamic_color_fg, Vtty_dynamic_color_bg);
}

#endif /* 0 */

static int
tty_initialize_color_instance (Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object UNUSED (device),
			       Error_Behavior UNUSED (errb))
{
  Lisp_Object result;

  name = Fintern (name, Qnil);
  result = assq_no_quit (name, Vtty_color_alist);

  if (NILP (result))
    {
#if 0
      if (!STRINGP (Vtty_dynamic_color_fg)
	  && !STRINGP (Vtty_dynamic_color_bg))
#endif
        return 0;
    }

  /* Don't allocate the data until we're sure that we will succeed. */
#ifdef NEW_GC
  c->data = alloc_lrecord_type (struct tty_color_instance_data,
				&lrecord_tty_color_instance_data);
#else /* not NEW_GC */
  c->data = xnew (struct tty_color_instance_data);
#endif /* not NEW_GC */
  COLOR_INSTANCE_TTY_SYMBOL (c) = name;

  return 1;
}

static void
tty_mark_color_instance (Lisp_Color_Instance *c)
{
  mark_object (COLOR_INSTANCE_TTY_SYMBOL (c));
}

static void
tty_print_color_instance (Lisp_Color_Instance *UNUSED (c),
			  Lisp_Object UNUSED (printcharfun),
			  int UNUSED (escapeflag))
{
}

static void
tty_finalize_color_instance (Lisp_Color_Instance *UNUSED_IF_NEW_GC (c))
{
#ifndef NEW_GC
  if (c->data)
    xfree (c->data, void *);
#endif /* not NEW_GC */
}

static int
tty_color_instance_equal (Lisp_Color_Instance *c1,
			  Lisp_Color_Instance *c2,
			  int UNUSED (depth))
{
  return (EQ (COLOR_INSTANCE_TTY_SYMBOL (c1),
	      COLOR_INSTANCE_TTY_SYMBOL (c2)));
}

static Hashcode
tty_color_instance_hash (Lisp_Color_Instance *c, int UNUSED (depth))
{
  return LISP_HASH (COLOR_INSTANCE_TTY_SYMBOL (c));
}

static int
tty_valid_color_name_p (struct device *UNUSED (d), Lisp_Object color)
{
  return (!NILP (assoc_no_quit (Fintern (color, Qnil), Vtty_color_alist)));
#if 0
	  || STRINGP (Vtty_dynamic_color_fg)
	  || STRINGP (Vtty_dynamic_color_bg)
#endif
}


static int
tty_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
			      Lisp_Object UNUSED (device),
			      Error_Behavior UNUSED (errb))
{
  Ibyte *str = XSTRING_DATA (name);
  Lisp_Object charset = Qnil;

  if (qxestrncmp_ascii (str, "normal", 6))
    return 0;
  str += 6;
  if (*str)
    {
#ifdef MULE
      if (*str != '/')
	return 0;
      str++;
      charset = Ffind_charset (intern_int (str));
      if (NILP (charset))
	return 0;
#else
      return 0;
#endif
    }

  /* Don't allocate the data until we're sure that we will succeed. */
#ifdef NEW_GC
  f->data = alloc_lrecord_type (struct tty_font_instance_data,
				&lrecord_tty_font_instance_data);
#else /* not NEW_GC */
  f->data = xnew (struct tty_font_instance_data);
#endif /* not NEW_GC */
  FONT_INSTANCE_TTY_CHARSET (f) = charset;
#ifdef MULE
  if (CHARSETP (charset))
    f->width = XCHARSET_COLUMNS (charset);
  else
#endif
    f->width = 1;

  f->proportional_p = 0;
  f->ascent = f->height = 1;
  f->descent = 0;

  return 1;
}

static void
tty_mark_font_instance (Lisp_Font_Instance *f)
{
  mark_object (FONT_INSTANCE_TTY_CHARSET (f));
}

static void
tty_print_font_instance (Lisp_Font_Instance *UNUSED (f),
			 Lisp_Object UNUSED (printcharfun),
			 int UNUSED (escapeflag))
{
}

static void
tty_finalize_font_instance (Lisp_Font_Instance *UNUSED_IF_NEW_GC (f))
{
#ifndef NEW_GC
  if (f->data)
    xfree (f->data, void *);
#endif /* not NEW_GC */
}

static Lisp_Object
tty_font_list (Lisp_Object UNUSED (pattern), Lisp_Object UNUSED (device),
		Lisp_Object UNUSED (maxnumber))
{
  return list1 (build_string ("normal"));
}

#ifdef MULE

static int
tty_font_spec_matches_charset (struct device *UNUSED (d), Lisp_Object charset,
			       const Ibyte *nonreloc, Lisp_Object reloc,
			       Bytecount offset, Bytecount length,
			       enum font_specifier_matchspec_stages stage)
{
  const Ibyte *the_nonreloc = nonreloc;

  if (stage)
    return 0;

  if (!the_nonreloc)
    the_nonreloc = XSTRING_DATA (reloc);
  fixup_internal_substring (nonreloc, reloc, offset, &length);
  the_nonreloc += offset;

  if (NILP (charset))
    return !memchr (the_nonreloc, '/', length);
  the_nonreloc = (const Ibyte *) memchr (the_nonreloc, '/', length);
  if (!the_nonreloc)
    return 0;
  the_nonreloc++;
  {
    Lisp_Object s = symbol_name (XSYMBOL (XCHARSET_NAME (charset)));
    return !qxestrcmp (the_nonreloc, XSTRING_DATA (s));
  }
}

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
tty_find_charset_font (Lisp_Object device, Lisp_Object font,
		       Lisp_Object charset, 
		       enum font_specifier_matchspec_stages stage)
{
  Ibyte *fontname = XSTRING_DATA (font);

  if (stage)
    return Qnil;

  if (strchr ((const char *) fontname, '/'))
    {
      if (tty_font_spec_matches_charset (XDEVICE (device), charset, 0,
					 font, 0, -1, initial))
	return font;
      return Qnil;
    }

  if (NILP (charset))
    return font;

  return concat3 (font, build_string ("/"),
		  Fsymbol_name (XCHARSET_NAME (charset)));
}

#endif /* MULE */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_tty (void)
{
#ifdef NEW_GC
  INIT_LRECORD_IMPLEMENTATION (tty_color_instance_data);
  INIT_LRECORD_IMPLEMENTATION (tty_font_instance_data);
#endif /* NEW_GC */

  DEFSUBR (Fregister_tty_color);
  DEFSUBR (Funregister_tty_color);
  DEFSUBR (Ffind_tty_color);
#if 0
  DEFSUBR (Fset_tty_dynamic_color_specs);
  DEFSUBR (Ftty_dynamic_color_specs);
#endif
}

void
console_type_create_objects_tty (void)
{
  /* object methods */
  CONSOLE_HAS_METHOD (tty, initialize_color_instance);
  CONSOLE_HAS_METHOD (tty, mark_color_instance);
  CONSOLE_HAS_METHOD (tty, print_color_instance);
  CONSOLE_HAS_METHOD (tty, finalize_color_instance);
  CONSOLE_HAS_METHOD (tty, color_instance_equal);
  CONSOLE_HAS_METHOD (tty, color_instance_hash);
  CONSOLE_HAS_METHOD (tty, valid_color_name_p);
  CONSOLE_HAS_METHOD (tty, color_list);

  CONSOLE_HAS_METHOD (tty, initialize_font_instance);
  CONSOLE_HAS_METHOD (tty, mark_font_instance);
  CONSOLE_HAS_METHOD (tty, print_font_instance);
  CONSOLE_HAS_METHOD (tty, finalize_font_instance);
  CONSOLE_HAS_METHOD (tty, font_list);
#ifdef MULE
  CONSOLE_HAS_METHOD (tty, font_spec_matches_charset);
  CONSOLE_HAS_METHOD (tty, find_charset_font);
#endif
}

void
vars_of_objects_tty (void)
{
  staticpro (&Vtty_color_alist);
  Vtty_color_alist = Qnil;

#if 0
  staticpro (&Vtty_dynamic_color_fg);
  Vtty_dynamic_color_fg = Qnil;

  staticpro (&Vtty_dynamic_color_bg);
  Vtty_dynamic_color_bg = Qnil;
#endif
}
