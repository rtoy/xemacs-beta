/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996, 2002 Ben Wing.
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

/* Authors: Jamie Zawinski, Chuck Thompson, Ben Wing */
/* Gtk version by William Perry */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "device-impl.h"
#include "insdel.h"

#include "console-gtk-impl.h"
#include "fontcolor-gtk-impl.h"

/* sigh */
#include "sysgdkx.h"

/* XListFonts doesn't allocate memory unconditionally based on this. (For
   XFree86 in 2005, at least. */
#define MAX_FONT_COUNT INT_MAX

#ifdef DEBUG_XEMACS
Fixnum debug_gtk_fonts;
#endif /* DEBUG_XEMACS */
static Lisp_Object Vgtk_fallback_font_name;
static Lisp_Object Vgtk_fallback_font_size;


/************************************************************************/
/*                          color instances                             */
/************************************************************************/

gboolean
gtk_parse_nearest_color (struct device * UNUSED (d), GDK_COLOR *color,
			 Lisp_Object name, Error_Behavior errb)
{
  const Extbyte *extname;
  gboolean result;

  xzero (*color);
  CHECK_STRING (name);
  extname = LISP_STRING_TO_EXTERNAL (name, Qutf_8);
#ifdef HAVE_GTK2
  result = gdk_color_parse (extname, color);
#else
  result = gdk_rgba_parse (color, extname);
#endif

  if (result == FALSE)
    {
      maybe_invalid_argument ("unrecognized color", name, Qcolor, errb);
      return 0;
    }
  return result;
}

static gboolean
gtk_initialize_color_instance (struct Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object device, Error_Behavior errb)
{
  GDK_COLOR color;
  gboolean result;

  result = gtk_parse_nearest_color (XDEVICE (device), &color,
				    name, errb);

  if (!result)
    return FALSE;

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  c->data = xnew (struct gtk_color_instance_data);
#ifdef HAVE_GTK2
  COLOR_INSTANCE_GTK_COLOR (c) = gdk_color_copy (&color);
#else
  COLOR_INSTANCE_GTK_COLOR (c) = gdk_rgba_copy (&color);
#endif
  return TRUE;
}

static void
gtk_print_color_instance (struct Lisp_Color_Instance *c,
			  Lisp_Object printcharfun,
			  int UNUSED (escapeflag))
{
#ifdef HAVE_GTK2
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  write_fmt_string (printcharfun, " %ld=(%X,%X,%X)",
		    color->pixel, color->red, color->green, color->blue);
#else
  GdkRGBA *color = COLOR_INSTANCE_GTK_COLOR (c);
  write_fmt_string (printcharfun, " (%0f,%0f,%0f,%0f)",
		    color->red, color->green, color->blue, color->alpha);
#endif
}

static void
gtk_finalize_color_instance (struct Lisp_Color_Instance *c)
{
  if (c->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (c->device)))
	{
#ifdef HAVE_GTK2
	  gdk_color_free (COLOR_INSTANCE_GTK_COLOR (c));
#else
	  gdk_rgba_free (COLOR_INSTANCE_GTK_COLOR (c));
#endif
	}
      xfree (c->data);
      c->data = 0;
    }
}

static int
gtk_color_instance_equal (struct Lisp_Color_Instance *c1,
			  struct Lisp_Color_Instance *c2,
			  int UNUSED (depth))
{
#ifdef HAVE_GTK2
    return (gdk_color_equal (COLOR_INSTANCE_GTK_COLOR (c1),
			     COLOR_INSTANCE_GTK_COLOR (c2)));
#else
    return (gdk_rgba_equal (COLOR_INSTANCE_GTK_COLOR (c1),
			    COLOR_INSTANCE_GTK_COLOR (c2)));
#endif
}

static Hashcode
gtk_color_instance_hash (struct Lisp_Color_Instance *c, int UNUSED (depth))
{
#ifdef HAVE_GTK2
    return (gdk_color_hash (COLOR_INSTANCE_GTK_COLOR (c)));
#else
    return gdk_rgba_hash (COLOR_INSTANCE_GTK_COLOR (c));
#endif
}

static Lisp_Object
gtk_color_instance_rgb_components (struct Lisp_Color_Instance *c)
{
#ifdef HAVE_GTK2
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  return (list3 (make_fixnum (color->red),
		 make_fixnum (color->green),
		 make_fixnum (color->blue)));
#else
  GdkRGBA *color = COLOR_INSTANCE_GTK_COLOR (c);
  return (list4 (make_float (color->red),
		 make_float (color->green),
		 make_float (color->blue),
		 make_float (color->alpha)));
#endif
}

static int
gtk_valid_color_name_p (struct device *UNUSED (d), Lisp_Object color)
{
  GDK_COLOR c;
  const char *extname;

  extname = LISP_STRING_TO_EXTERNAL (color, Qutf_8);

#ifdef HAVE_GTK2
  return gdk_color_parse (extname, &c);
#else
  return gdk_rgba_parse (&c, extname);
#endif
}

static Lisp_Object
gtk_color_list (void)
{
  /* #### BILL!!!
     Is this correct? */
  return call0 (intern ("x-color-list-internal"));
}


/************************************************************************/
/*                           font instances                             */
/************************************************************************/

static PangoFontDescription *
font_description_from_string (char *extname)
{
  PangoFontDescription *pfd;
  char *p;
  int count = 0;
  size_t len = strlen (extname);

  p = extname;
  /* Current lisp code makes an XLFD, which we can't load. */
  while (*p)
    {
      if (*p == '-')
        count++;
      ++p;
    }
  /* XLFD */
  if (count >= 2 || strchr (extname, '*'))
    {
      CHECK_STRING (Vgtk_fallback_font_name);
      extname = LISP_STRING_TO_EXTERNAL (Vgtk_fallback_font_name, Qutf_8);
    }

  /* FontConfig */
  if (strcspn (extname, "-:=") != len)
    {
      FcPattern *pattern = FcNameParse ((const FcChar8 *)extname);
      pfd = pango_fc_font_description_from_pattern (pattern, TRUE);
    }
  else
    {
      pfd = pango_font_description_from_string (extname);
    }

  return pfd;
}

static int
gtk_initialize_font_instance (struct Lisp_Font_Instance *f,
			      Lisp_Object UNUSED (name),
			      Lisp_Object device, Error_Behavior errb)
{
  struct device *d = XDEVICE (device);
  Extbyte *extname;
  PangoFontDescription *pfd;
  PangoFontMetrics *pfm;
  PangoFontMask mask;
  PangoFont *pf;
  char *nm;
  
  extname = LISP_STRING_TO_EXTERNAL (f->name, Qutf_8);

  pfd = font_description_from_string (extname);
  /* We can get 0 size fonts here, which will screw up the metrics.
     So we force a size. */
  mask = pango_font_description_get_set_fields (pfd);
  if ((mask & PANGO_FONT_MASK_SIZE) == 0)
    {
      float pt_size;
      CHECK_FIXNUM_OR_FLOAT (Vgtk_fallback_font_size);
      pt_size = extract_float (Vgtk_fallback_font_size);
      pango_font_description_set_size(pfd, pt_size * PANGO_SCALE);
    }
  
  pf = pango_font_map_load_font (DEVICE_GTK_FONT_MAP (d),
				 DEVICE_GTK_CONTEXT (d), pfd);
  pfm = pango_font_get_metrics (pf, pango_language_from_string ("en"));
  pfd = pango_font_describe (pf);
  nm = pango_font_description_to_string (pfd);

  if (!pf)
    {
      maybe_signal_error (Qgui_error, "couldn't load font", f->name,
			  Qfont, errb);
      return 0;
    }
#ifdef DEBUG_XEMACS
  if (debug_gtk_fonts)
    debug_out ("font requested \"%s\" loaded \"%s\"\n", extname, nm);
#endif
  g_free (nm);
  
  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  f->data = xnew (struct gtk_font_instance_data);
  FONT_INSTANCE_GTK_FONT (f) = pf;
  FONT_INSTANCE_GTK_FONT_DESC (f) = pfd;
  FONT_INSTANCE_GTK_FONT_METRICS (f) = pfm;

  f->ascent = pango_font_metrics_get_ascent (pfm) / PANGO_SCALE;
  f->descent = pango_font_metrics_get_descent (pfm) / PANGO_SCALE;
  f->height = f->ascent + f->descent;
  f->width = pango_font_metrics_get_approximate_char_width (pfm) / PANGO_SCALE;
  f->proportional_p = 0;
  return 1;
}

static void
gtk_print_font_instance (struct Lisp_Font_Instance *f,
			 Lisp_Object printcharfun,
			 int UNUSED (escapeflag))
{
  write_fmt_string (printcharfun, " 0x%lx",
		    (unsigned long) FONT_INSTANCE_GTK_FONT (f));
  /* write_fmt_string (printcharfun, " %s", f->name); */
}

static void
gtk_finalize_font_instance (struct Lisp_Font_Instance *f)
{
  if (f->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (f->device)))
	{
	  pango_font_description_free (FONT_INSTANCE_GTK_FONT_DESC (f));
	  pango_font_metrics_unref (FONT_INSTANCE_GTK_FONT_METRICS (f));
	  g_object_unref (FONT_INSTANCE_GTK_FONT (f));
	}
      xfree (f->data);
      f->data = 0;
    }
}

/* Forward declarations for X specific functions at the end of the file */
Lisp_Object __get_gtk_font_truename (PangoFont *pf, int expandp);

static Lisp_Object
gtk_font_instance_truename (struct Lisp_Font_Instance *f,
			    Error_Behavior UNUSED (errb))
{
  if (NILP (FONT_INSTANCE_TRUENAME (f)))
    {
      FONT_INSTANCE_TRUENAME (f) = __get_gtk_font_truename (FONT_INSTANCE_GTK_FONT (f), 1);

      if (NILP (FONT_INSTANCE_TRUENAME (f)))
	{
	  /* Ok, just this once, return the font name as the truename.
	     (This is only used by Fequal() right now.) */
	  return f->name;
	}
    }
  return (FONT_INSTANCE_TRUENAME (f));
}

static Lisp_Object
gtk_font_instance_properties (struct Lisp_Font_Instance *UNUSED (f))
{
  Lisp_Object result = Qnil;

  /* #### BILL!!! */
  /* There seems to be no way to get this information under Gtk */
  return result;
}

static Lisp_Object
gtk_font_list (Lisp_Object pattern, Lisp_Object device,
               Lisp_Object UNUSED (maxnumber))
{
  struct device *d = XDEVICE (device);
  Lisp_Object result = Qnil;
  PangoFontMap *font_map = DEVICE_GTK_FONT_MAP (d);
  PangoFontFamily **families = NULL;
  int n_families, i;
  int monospace_only = 0;

  /* What to do with the pattern?  Add a single case for now. */
  if (lisp_strcasecmp_i18n (pattern,
			    build_extstring ("monospace", Qutf_8)) == 0)
    monospace_only = 1;

  /* Should we restrict to monospace somehow?  That can be done with
     fontconfig fonts. */
  pango_font_map_list_families (font_map, &families, &n_families);
  
  for (i = 0; i < n_families; i++)
    {
      const char *name;

      if (monospace_only && !pango_font_family_is_monospace (families[i]))
        continue;
      name = pango_font_family_get_name (families[i]);
      result = Fcons (build_extstring (name, Qutf_8), result);
    }
  g_free (families);
  return result;
}

/* Include the charset support, shared, for the moment, with X11.  */
#define THIS_IS_GTK
#include "fontcolor-xlike-inc.c"

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
gtk_find_charset_font (Lisp_Object UNUSED (device), Lisp_Object font,
                       Lisp_Object UNUSED (charset),
                       enum font_specifier_matchspec_stages UNUSED (stage))
{
  /* Pango doesn't understand charset.  Maybe PangoCoverage? */
  return font;
}

DEFUN ("gtk-set-font-weight", Fgtk_set_font_weight, 2, 3, 0, /*
Return a specificer for FONT with WEIGHT numeric value on DEVICE.
*/
       (font, weight, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  PangoWeight w;
  Lisp_Object val;

  CHECK_STRING (font);
  /* Not sure if range should be 200-900 --jsparkes */
  check_integer_range (weight, Qzero, make_integer (1000));

  w = (PangoWeight) (XFIXNUM (weight));
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);
  pango_font_description_set_weight (pfd, w);
  new_name = pango_font_description_to_string (pfd);
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}

DEFUN ("gtk-make-font-bold", Fgtk_make_font_bold, 1, 2, 0, /*
Return a specifier for bold version of FONT on DEVICE.
*/
       (font, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  Lisp_Object val;
  
  CHECK_STRING (font);
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);

  pango_font_description_set_weight (pfd, PANGO_WEIGHT_BOLD);
  new_name = pango_font_description_to_string (pfd);
#ifdef DEBUG_XEMACS
  if (debug_gtk_fonts)
    debug_out ("%s -> %s\n", extname, new_name);
#endif
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}

DEFUN ("gtk-make-font-unbold", Fgtk_make_font_unbold, 1, 2, 0, /*
Return a specifier for normal, non-bold version of FONT on DEVICE.
*/
       (font, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  Lisp_Object val;
  
  CHECK_STRING (font);
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);

  pango_font_description_set_weight (pfd, PANGO_WEIGHT_MEDIUM);
  new_name = pango_font_description_to_string (pfd);
#ifdef DEBUG_XEMACS
  if (debug_gtk_fonts)
    debug_out ("%s -> %s\n", extname, new_name);
#endif
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}

DEFUN ("gtk-make-font-italic", Fgtk_make_font_italic, 1, 2, 0, /*
Return a specifier for italic version of FONT on DEVICE.
*/
       (font, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  Lisp_Object val;
  
  CHECK_STRING (font);
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);

  pango_font_description_set_style (pfd, PANGO_STYLE_ITALIC);
  new_name = pango_font_description_to_string (pfd);
#ifdef DEBUG_XEMACS
  if (debug_gtk_fonts)
    debug_out ("%s -> %s\n", extname, new_name);
#endif
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}

DEFUN ("gtk-make-font-unitalic", Fgtk_make_font_unitalic, 1, 2, 0, /*
Return a specifier for normal, non-italic version of FONT on DEVICE.
*/
       (font, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  Lisp_Object val;
  
  CHECK_STRING (font);
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);

  pango_font_description_set_style (pfd, PANGO_STYLE_NORMAL);
  new_name = pango_font_description_to_string (pfd);
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}

DEFUN ("gtk-make-font-bold-italic", Fgtk_make_font_bold_italic, 1, 2, 0, /*
Return a specifier for bold italic version of FONT on DEVICE.
*/
       (font, UNUSED (device)))
{
  PangoFontDescription *pfd;
  char *extname, *new_name;
  Lisp_Object val;
  
  CHECK_STRING (font);
  extname = LISP_STRING_TO_EXTERNAL (font, Qutf_8);
  pfd = font_description_from_string (extname);

  pango_font_description_set_weight (pfd, PANGO_WEIGHT_BOLD);
  pango_font_description_set_style (pfd, PANGO_STYLE_ITALIC);
  new_name = pango_font_description_to_string (pfd);
#ifdef DEBUG_XEMACS
  if (debug_gtk_fonts)
    debug_out ("%s -> %s\n", extname, new_name);
#endif
  val = build_cistring (new_name);
  g_free (new_name);
  pango_font_description_free (pfd);
  return val;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_fontcolor_gtk (void)
{
  DEFSUBR (Fgtk_set_font_weight);
  DEFSUBR (Fgtk_make_font_bold);
  DEFSUBR (Fgtk_make_font_unbold);
  DEFSUBR (Fgtk_make_font_italic);
  DEFSUBR (Fgtk_make_font_unitalic);
  DEFSUBR (Fgtk_make_font_bold_italic);
}

void
console_type_create_fontcolor_gtk (void)
{
  /* object methods */

  CONSOLE_HAS_METHOD (gtk, initialize_color_instance);
  CONSOLE_HAS_METHOD (gtk, print_color_instance);
  CONSOLE_HAS_METHOD (gtk, finalize_color_instance);
  CONSOLE_HAS_METHOD (gtk, color_instance_equal);
  CONSOLE_HAS_METHOD (gtk, color_instance_hash);
  CONSOLE_HAS_METHOD (gtk, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (gtk, valid_color_name_p);
  CONSOLE_HAS_METHOD (gtk, color_list);

  CONSOLE_HAS_METHOD (gtk, initialize_font_instance);
  CONSOLE_HAS_METHOD (gtk, print_font_instance);
  CONSOLE_HAS_METHOD (gtk, finalize_font_instance);
  CONSOLE_HAS_METHOD (gtk, font_instance_truename);
  CONSOLE_HAS_METHOD (gtk, font_instance_properties);
  CONSOLE_HAS_METHOD (gtk, font_list);
#ifdef MULE
  CONSOLE_HAS_METHOD (gtk, find_charset_font);
  CONSOLE_HAS_METHOD (gtk, font_spec_matches_charset);
#endif
}

void
vars_of_fontcolor_gtk (void)
{
#ifdef DEBUG_XEMACS
  DEFVAR_INT ("debug-gtk-fonts", &debug_gtk_fonts /*
If non-zero, display debug information about Gtk fonts.
*/ );
  debug_gtk_fonts = 0;
#endif
  DEFVAR_LISP ("gtk-fallback-font-name", &Vgtk_fallback_font_name/*
Name of font to be loaded instead of a failed font.
*/);
  Vgtk_fallback_font_name = build_cistring ("Monospace");
  DEFVAR_LISP ("gtk-fallback-font-size", &Vgtk_fallback_font_size/*
Point size to use for fonts if not otherwise specified.
*/);
  Vgtk_fallback_font_size = make_float (10.0);
}

#if 0
/* It's difficult to validate font names in Gtk because the toolkit
   does fallbacks for you. */
static int
valid_font_name_p (Display *dpy, char *name)
{
  /* Maybe this should be implemented by calling XLoadFont and trapping
     the error.  That would be a lot of work, and wasteful as hell, but
     might be more correct.
   */
  int nnames = 0;
  char **names = 0;
  if (! name)
    return 0;
  names = XListFonts (dpy, name, 1, &nnames);
  if (names)
    XFreeFontNames (names);
  return (nnames != 0);
}
#endif

Lisp_Object
__get_gtk_font_truename (PangoFont *pf, int UNUSED (expandp))
{
  Lisp_Object font_name = Qnil;
  PangoFontDescription *pfd = pango_font_describe (pf);

  /* This is insufficent. */
  char *name = pango_font_description_to_string (pfd);
  
  if (name != NULL)
    font_name = build_cistring (name);

  g_free (name);
  return font_name;
}

#ifdef HAVE_GTK2
void
gtk_add_css_style(GtkWidget *UNUSED (widget), char *UNUSED (css));
{
  /* No CSS support in Gtk 2.X */
}
#endif

#ifdef HAVE_GTK3
void
gtk_widget_add_css_style(GtkWidget *widget, gchar *css)
{
  GtkStyleContext *style = gtk_widget_get_style_context (widget);
  GtkCssProvider *css_prov = gtk_css_provider_new ();
  GError *error;

  gtk_css_provider_load_from_data (css_prov, css, -1, &error);

  if (error != NULL)
    gtk_style_context_add_provider (style,
				    GTK_STYLE_PROVIDER (css_prov),
				    GTK_STYLE_PROVIDER_PRIORITY_FALLBACK);
  g_object_unref (css_prov);
}
#endif
