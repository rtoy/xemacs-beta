/* Include file for common code, X and GTK colors and fonts.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2004, 2005 Ben Wing.
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

/* Authors: Jamie Zawinski, Chuck Thompson, Ben Wing */

/* Extracted from objects-x.c, objects-gtk.c 2-13-05.
   NOTE: There is an advantage to having the code coalesced this way
   even when there is a fair amount of difference between the two versions,
   provided that they are still parallel -- having them side by side ensures
   that logic changes in one are propagated to the other, preventing bit-rot
   --ben
*/

#ifndef THIS_IS_GTK
#define ZZCOLOR_TYPE XColor
#define ZZCOLOR_INSTANCE(name) COLOR_INSTANCE_X_##name
#define ZZ(z) x_##z
#define ZZEND(z) z##_x
#define ZZCONSOLE_HAS_METHOD(name) CONSOLE_HAS_METHOD (x, name)
#define UNUSED_IF_GTK(arg) arg
#else
#define ZZCOLOR_TYPE GdkColor
#define ZZCOLOR_INSTANCE(name) COLOR_INSTANCE_GTK_##name
#define ZZ(z) gtk_##z
#define ZZEND(z) z##_gtk
#define ZZCONSOLE_HAS_METHOD(name) CONSOLE_HAS_METHOD (gtk, name)
#define UNUSED_IF_GTK(arg) UNUSED (arg)
#endif


/************************************************************************/
/*                          color instances                             */
/************************************************************************/

static int
ZZ (parse_nearest_color) (struct device *d, ZZCOLOR_TYPE *color,
			  Lisp_Object name, Error_Behavior errb)
{
#ifndef THIS_IS_GTK
  Display *dpy   = DEVICE_X_DISPLAY  (d);
  Colormap cmap  = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL   (d);
#else /* THIS_IS_GTK */
  GdkColormap *cmap = DEVICE_GTK_COLORMAP (d);
  GdkVisual *visual = DEVICE_GTK_VISUAL (d);
#endif /* THIS_IS_GTK */
  int result;

  xzero (*color);

#ifndef THIS_IS_GTK
    result =
      XParseColor (dpy, cmap,
		   NEW_LISP_STRING_TO_EXTERNAL (name, Qx_color_name_encoding),
		   color);
#else /* THIS_IS_GTK */
  result = gdk_color_parse (LISP_STRING_TO_GTK_TEXT (name), color);
#endif /* THIS_IS_GTK */
  if (!result)
    {
      maybe_signal_error (Qgui_error, "Unrecognized color",
			  name, Qcolor, errb);
      return 0;
    }
#ifndef THIS_IS_GTK
  result = ZZ (allocate_nearest_color) (dpy, cmap, visual, color);
#else /* THIS_IS_GTK */
  result = ZZ (allocate_nearest_color) (cmap, visual, color);
#endif /* THIS_IS_GTK */
  if (!result)
    {
      maybe_signal_error (Qgui_error, "Couldn't allocate color",
			  name, Qcolor, errb);
      return 0;
    }

  return result;
}

static int
ZZ (initialize_color_instance) (Lisp_Color_Instance *c, Lisp_Object name,
				Lisp_Object device, Error_Behavior errb)
{
  ZZCOLOR_TYPE color;
  int result;

  result = ZZ (parse_nearest_color) (XDEVICE (device), &color, name, errb);

  if (!result)
    return 0;

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  c->data = xnew (struct ZZ (color_instance_data));
  if (result == 3)
    ZZCOLOR_INSTANCE (DEALLOC) (c) = 0;
  else
    ZZCOLOR_INSTANCE (DEALLOC) (c) = 1;
#ifndef THIS_IS_GTK
  ZZCOLOR_INSTANCE (COLOR) (c) = color;
#else /* THIS_IS_GTK */
  ZZCOLOR_INSTANCE (COLOR) (c) = gdk_color_copy (&color);
#endif /* THIS_IS_GTK */
  return 1;
}

static void
ZZ (print_color_instance) (Lisp_Color_Instance *c,
			   Lisp_Object printcharfun,
			   int UNUSED (escapeflag))
{
#ifndef THIS_IS_GTK
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  write_fmt_string (printcharfun, " %ld=(%X,%X,%X)",
		    color.pixel, color.red, color.green, color.blue);
#else /* THIS_IS_GTK */
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  write_fmt_string (printcharfun, " %ld=(%X,%X,%X)",
		    color->pixel, color->red, color->green, color->blue);
#endif /* THIS_IS_GTK */
}

static void
ZZ (finalize_color_instance) (Lisp_Color_Instance *c)
{
  if (c->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (c->device)))
	{
	  if (ZZCOLOR_INSTANCE (DEALLOC) (c))
	    {
#ifndef THIS_IS_GTK
	      XFreeColors (DEVICE_X_DISPLAY (XDEVICE (c->device)),
			   DEVICE_X_COLORMAP (XDEVICE (c->device)),
			   &COLOR_INSTANCE_X_COLOR (c).pixel, 1, 0);
#else /* THIS_IS_GTK */
	      gdk_colormap_free_colors (DEVICE_GTK_COLORMAP
					(XDEVICE (c->device)),
					COLOR_INSTANCE_GTK_COLOR (c), 1);
#endif /* THIS_IS_GTK */
	    }
#ifdef THIS_IS_GTK
	  gdk_color_free (COLOR_INSTANCE_GTK_COLOR (c));
#endif /* THIS_IS_GTK */
	}
      xfree (c->data, void *);
      c->data = 0;
    }
}

/* Color instances are equal if they resolve to the same color on the
   screen (have the same RGB values).  I imagine that
   "same RGB values" == "same cell in the colormap."  Arguably we should
   be comparing their names or pixel values instead. */

static int
ZZ (color_instance_equal) (Lisp_Color_Instance *c1,
			   Lisp_Color_Instance *c2,
			   int UNUSED (depth))
{
#ifndef THIS_IS_GTK
  XColor color1 = COLOR_INSTANCE_X_COLOR (c1);
  XColor color2 = COLOR_INSTANCE_X_COLOR (c2);
  return ((color1.red == color2.red) &&
	  (color1.green == color2.green) &&
	  (color1.blue == color2.blue));
#else /* THIS_IS_GTK */
  return (gdk_color_equal (COLOR_INSTANCE_GTK_COLOR (c1),
			   COLOR_INSTANCE_GTK_COLOR (c2)));
#endif /* THIS_IS_GTK */
}

static Hashcode
ZZ (color_instance_hash) (Lisp_Color_Instance *c, int UNUSED (depth))
{
#ifndef THIS_IS_GTK
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return HASH3 (color.red, color.green, color.blue);
#else /* THIS_IS_GTK */
  return (gdk_color_hash (COLOR_INSTANCE_GTK_COLOR (c), NULL));
#endif /* THIS_IS_GTK */
}

static Lisp_Object
ZZ (color_instance_rgb_components) (Lisp_Color_Instance *c)
{
#ifndef THIS_IS_GTK
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return (list3 (make_int (color.red),
		 make_int (color.green),
		 make_int (color.blue)));
#else /* THIS_IS_GTK */
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  return (list3 (make_int (color->red),
		 make_int (color->green),
		 make_int (color->blue)));
#endif /* THIS_IS_GTK */
}

static int
ZZ (valid_color_name_p) (struct device *UNUSED_IF_GTK (d), Lisp_Object color)
{
#ifndef THIS_IS_GTK
  XColor c;
  Display *dpy = DEVICE_X_DISPLAY (d);
  Colormap cmap = DEVICE_X_COLORMAP (d);
  const Extbyte *extname;

  LISP_STRING_TO_EXTERNAL (color, extname, Qx_color_name_encoding);

  return XParseColor (dpy, cmap, extname, &c);
#else /* THIS_IS_GTK */
  GdkColor c;
  const Extbyte *extname;

  LISP_STRING_TO_EXTERNAL (color, extname, Vgtk_text_encoding);

  if (gdk_color_parse (extname, &c) != TRUE)
      return 0;
  return 1;
#endif /* THIS_IS_GTK */
}

static Lisp_Object
ZZ (color_list) (void)
{
#ifdef THIS_IS_GTK
  /* #### BILL!!!
     Is this correct? */
#endif /* THIS_IS_GTK */
  return call0 (intern ("x-color-list-internal"));
}


/************************************************************************/
/*                           font instances                             */
/************************************************************************/

static int
ZZ (initialize_font_instance) (Lisp_Font_Instance *f,
			       Lisp_Object UNUSED (name),
			       Lisp_Object UNUSED_IF_GTK (device),
			       Error_Behavior errb)
{
  XFontStruct *xf;
  const Extbyte *extname;

#ifndef THIS_IS_GTK
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  LISP_STRING_TO_EXTERNAL (f->name, extname, Qx_font_name_encoding);
  xf = XLoadQueryFont (dpy, extname);

  if (!xf)
    {
      maybe_signal_error (Qgui_error, "Couldn't load font", f->name,
			  Qfont, errb);
      return 0;
    }

  if (!xf->max_bounds.width)
    {
      /* yes, this has been known to happen. */
      XFreeFont (dpy, xf);
      maybe_signal_error (Qgui_error, "X font is too small", f->name, Qfont,
			  errb);

      return 0;
    }

#else /* THIS_IS_GTK */
  GdkFont *gf;

  LISP_STRING_TO_EXTERNAL (f->name, extname, Vgtk_text_encoding);
  gf = gdk_font_load (extname);

  if (!gf)
    {
      maybe_signal_error (Qgui_error, "Couldn't load font", f->name,
			  Qfont, errb);
      return 0;
    }

  xf = (XFontStruct *) GDK_FONT_XFONT (gf);

#endif /* THIS_IS_GTK */

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */

#ifndef THIS_IS_GTK
  f->data = xnew (struct x_font_instance_data);
  FONT_INSTANCE_X_FONT (f) = xf;
  f->ascent = xf->ascent;
  f->descent = xf->descent;
  f->height = xf->ascent + xf->descent;
#else /* THIS_IS_GTK */
  f->data = xnew (struct gtk_font_instance_data);
  FONT_INSTANCE_GTK_FONT (f) = gf;
  f->ascent = gf->ascent;
  f->descent = gf->descent;
  f->height = gf->ascent + gf->descent;
#endif /* THIS_IS_GTK */

  /* Now let's figure out the width of the font */

  {
    /* following change suggested by Ted Phelps <phelps@dstc.edu.au> */
    int def_char = 'n'; /*xf->default_char;*/
    int byte1, byte2;

  once_more:
    byte1 = def_char >> 8;
    byte2 = def_char & 0xFF;

    if (xf->per_char)
      {
	/* Old versions of the R5 font server have garbage (>63k) as
	   def_char. 'n' might not be a valid character. */
	if (byte1 < (int) xf->min_byte1         ||
	    byte1 > (int) xf->max_byte1         ||
	    byte2 < (int) xf->min_char_or_byte2 ||
	    byte2 > (int) xf->max_char_or_byte2)
	  f->width = 0;
	else
	  f->width = xf->per_char[(byte1 - xf->min_byte1) *
				  (xf->max_char_or_byte2 -
				   xf->min_char_or_byte2 + 1) +
				  (byte2 - xf->min_char_or_byte2)].width;
      }
    else
      f->width = xf->max_bounds.width;

    /* Some fonts have a default char whose width is 0.  This is no good.
       If that's the case, first try 'n' as the default char, and if n has
       0 width too (unlikely) then just use the max width. */
    if (f->width == 0)
      {
	if (def_char == (int) xf->default_char)
	  f->width = xf->max_bounds.width;
	else
	  {
	    def_char = xf->default_char;
	    goto once_more;
	  }
      }
  }
  /* If all characters don't exist then there could potentially be
     0-width characters lurking out there.  Not setting this flag
     trips an optimization that would make them appear to have width
     to redisplay.  This is bad.  So we set it if not all characters
     have the same width or if not all characters are defined.
     */
  /* #### This sucks.  There is a measurable performance increase
     when using proportional width fonts if this flag is not set.
     Unfortunately so many of the fucking X fonts are not fully
     defined that we could almost just get rid of this damn flag and
     make it an assertion. */
  f->proportional_p = (xf->min_bounds.width != xf->max_bounds.width ||
		       (
#ifndef THIS_IS_GTK
			x_handle_non_fully_specified_fonts &&
#else /* THIS_IS_GTK */
			/* x_handle_non_fully_specified_fonts */ 0 &&
#endif /* THIS_IS_GTK */
			!xf->all_chars_exist));

#if 0 /* THIS_IS_GTK */
  f->width = gdk_char_width (gf, 'n');
  f->proportional_p = (gdk_char_width (gf, '|') != gdk_char_width (gf, 'W'));
#endif
  return 1;
}

static void
ZZ (print_font_instance) (Lisp_Font_Instance *f,
			  Lisp_Object printcharfun,
			  int UNUSED (escapeflag))
{
  write_fmt_string (printcharfun, " 0x%lx",
#ifndef THIS_IS_GTK
		    (unsigned long) FONT_INSTANCE_X_FONT (f)->fid);
#else /* THIS_IS_GTK */
		    (unsigned long) gdk_font_id (FONT_INSTANCE_GTK_FONT (f)));
#endif /* THIS_IS_GTK */
}

static void
ZZ (finalize_font_instance) (Lisp_Font_Instance *f)
{
  if (f->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (f->device)))
	{
#ifndef THIS_IS_GTK
	  XFreeFont (DEVICE_X_DISPLAY (XDEVICE (f->device)),
		     FONT_INSTANCE_X_FONT (f));
#else /* THIS_IS_GTK */
	  gdk_font_unref (FONT_INSTANCE_GTK_FONT (f));
#endif /* THIS_IS_GTK */
	}
      xfree (f->data, void *);
      f->data = 0;
    }
}

/* Unbounded, for sufficiently small values of infinity... */
#define MAX_FONT_COUNT 5000

#ifndef THIS_IS_GTK
static Lisp_Object x_font_truename (Display *dpy, Extbyte *name,
				    XFontStruct *font);
#else
Lisp_Object __get_gtk_font_truename (GdkFont *gdk_font, int expandp);
#endif

static Lisp_Object
ZZ (font_instance_truename) (Lisp_Font_Instance *f, Error_Behavior errb)
{
  if (NILP (FONT_INSTANCE_TRUENAME (f)))
    {
#ifndef THIS_IS_GTK
      FONT_INSTANCE_TRUENAME (f) =
	x_font_truename (DEVICE_X_DISPLAY (XDEVICE (f->device)),
			 NEW_LISP_STRING_TO_EXTERNAL
			 (f->name, Qx_font_name_encoding),
			 FONT_INSTANCE_X_FONT (f));
#else
      FONT_INSTANCE_TRUENAME (f) =
	__get_gtk_font_truename (FONT_INSTANCE_GTK_FONT (f), 1);
#endif /* THIS_IS_GTK */

      if (NILP (FONT_INSTANCE_TRUENAME (f)))
	{
	  Lisp_Object font_instance = wrap_font_instance (f);


	  maybe_signal_error (Qgui_error, "Couldn't determine font truename",
			      font_instance, Qfont, errb);
	  /* Ok, just this once, return the font name as the truename.
	     (This is only used by Fequal() right now.) */
	  return f->name;
	}
    }
  return FONT_INSTANCE_TRUENAME (f);
}

#ifdef MULE

static int
ZZ (font_spec_matches_charset) (struct device *UNUSED (d), Lisp_Object charset,
				const Ibyte *nonreloc, Lisp_Object reloc,
				Bytecount offset, Bytecount length,
				int stage)
{
  if (stage)
    return 0;

  if (UNBOUNDP (charset))
    return 1;
  /* Hack! Short font names don't have the registry in them,
     so we just assume the user knows what they're doing in the
     case of ASCII.  For other charsets, you gotta give the
     long form; sorry buster.
     */
  if (EQ (charset, Vcharset_ascii))
    {
      const Ibyte *the_nonreloc = nonreloc;
      int i;
      Bytecount the_length = length;

      if (!the_nonreloc)
	the_nonreloc = XSTRING_DATA (reloc);
      fixup_internal_substring (nonreloc, reloc, offset, &the_length);
      the_nonreloc += offset;
      if (!memchr (the_nonreloc, '*', the_length))
	{
	  for (i = 0;; i++)
	    {
	      const Ibyte *new_nonreloc = (const Ibyte *)
		memchr (the_nonreloc, '-', the_length);
	      if (!new_nonreloc)
		break;
	      new_nonreloc++;
	      the_length -= new_nonreloc - the_nonreloc;
	      the_nonreloc = new_nonreloc;
	    }

	  /* If it has less than 5 dashes, it's a short font.
	     Of course, long fonts always have 14 dashes or so, but short
	     fonts never have more than 1 or 2 dashes, so this is some
	     sort of reasonable heuristic. */
	  if (i < 5)
	    return 1;
	}
    }

  return (fast_string_match (XCHARSET_REGISTRY (charset),
			     nonreloc, reloc, offset, length, 1,
			     ERROR_ME, 0) >= 0);
}

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
ZZ (find_charset_font) (Lisp_Object device, Lisp_Object font,
			Lisp_Object charset, int stage)
{
#ifdef THIS_IS_GTK
  /* #### copied from x_find_charset_font */
  /* #### BILL!!! Try to make this go away eventually */
#endif /* THIS_IS_GTK */
  Extbyte **names;
  int count = 0;
  Lisp_Object result = Qnil;
  const Extbyte *patternext;
  int i;

  if (stage)
    return Qnil;

  LISP_STRING_TO_EXTERNAL (font, patternext, Qx_font_name_encoding);

  names = XListFonts (
#ifndef THIS_IS_GTK
		      DEVICE_X_DISPLAY (XDEVICE (device)),
#else
		      GDK_DISPLAY (),
#endif
		      patternext, MAX_FONT_COUNT, &count);
  /* #### This code seems awfully bogus -- mrb */
  for (i = 0; i < count; i ++)
    {
      const Ibyte *intname;
      Bytecount intlen;

      EXTERNAL_TO_SIZED_C_STRING (names[i], intname, intlen,
				  Qx_font_name_encoding);
      if (ZZ (font_spec_matches_charset) (XDEVICE (device), charset,
					  intname, Qnil, 0, -1, stage))
	{
	  result = make_string (intname, intlen);
	  break;
	}
    }

  if (names)
    XFreeFontNames (names);

  /* Check for a short font name. */
  if (NILP (result)
      && ZZ (font_spec_matches_charset) (XDEVICE (device), charset, 0,
					 font, 0, -1, stage))
    return font;

  return result;
}

#endif /* MULE */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
ZZEND (console_type_create_objects) (void)
{
  /* object methods */

  ZZCONSOLE_HAS_METHOD (initialize_color_instance);
  ZZCONSOLE_HAS_METHOD (print_color_instance);
  ZZCONSOLE_HAS_METHOD (finalize_color_instance);
  ZZCONSOLE_HAS_METHOD (color_instance_equal);
  ZZCONSOLE_HAS_METHOD (color_instance_hash);
  ZZCONSOLE_HAS_METHOD (color_instance_rgb_components);
  ZZCONSOLE_HAS_METHOD (valid_color_name_p);
  ZZCONSOLE_HAS_METHOD (color_list);

  ZZCONSOLE_HAS_METHOD (initialize_font_instance);
  ZZCONSOLE_HAS_METHOD (print_font_instance);
  ZZCONSOLE_HAS_METHOD (finalize_font_instance);
  ZZCONSOLE_HAS_METHOD (font_instance_truename);
  ZZCONSOLE_HAS_METHOD (font_instance_properties);
  ZZCONSOLE_HAS_METHOD (font_list);
#ifdef MULE
  ZZCONSOLE_HAS_METHOD (find_charset_font);
  ZZCONSOLE_HAS_METHOD (font_spec_matches_charset);
#endif
}
