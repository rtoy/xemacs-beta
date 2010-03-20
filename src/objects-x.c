/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2004 Ben Wing.
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

/* Authors: Ben Wing, in its current form; based on earlier
   font/color-handling code by Jamie Zawinski.  Some code by Chuck
   Thompson. */

/* This file Mule-ized by Ben Wing, 7-10-00. */

#include <config.h>
#include "lisp.h"

#include "charset.h"
#include "device-impl.h"
#include "insdel.h"

#include "console-x-impl.h"
#include "objects-x-impl.h"
#include "elhash.h"

#ifdef HAVE_XFT
#include "font-mgr.h"
#endif

int x_handle_non_fully_specified_fonts;

#ifdef DEBUG_XEMACS 
Fixnum debug_x_objects;
#endif /* DEBUG_XEMACS */


/************************************************************************/
/*                          color instances                             */
/************************************************************************/

static int
x_parse_nearest_color (struct device *d, XColor *color, Lisp_Object name,
		       Error_Behavior errb)
{
  Display *dpy   = DEVICE_X_DISPLAY  (d);
  Colormap cmap  = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL   (d);
  int result;

  xzero (*color);
  {
    const Extbyte *extname;

    extname = LISP_STRING_TO_EXTERNAL (name, Qx_color_name_encoding);
    result = XParseColor (dpy, cmap, extname, color);
  }
  if (!result)
    {
      maybe_signal_error (Qgui_error, "Unrecognized color",
			  name, Qcolor, errb);
      return 0;
    }
  result = x_allocate_nearest_color (dpy, cmap, visual, color);
  if (!result)
    {
      maybe_signal_error (Qgui_error, "Couldn't allocate color",
			  name, Qcolor, errb);
      return 0;
    }

  return result;
}

static int
x_initialize_color_instance (Lisp_Color_Instance *c, Lisp_Object name,
			     Lisp_Object device, Error_Behavior errb)
{
  XColor color;
#ifdef HAVE_XFT
  XftColor xftColor;
#endif
  int result;

  result = x_parse_nearest_color (XDEVICE (device), &color, name, errb);

  if (!result)
    return 0;

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  c->data = xnew (struct x_color_instance_data);
  if (result == 3)
    COLOR_INSTANCE_X_DEALLOC (c) = 0;
  else
    COLOR_INSTANCE_X_DEALLOC (c) = 1;
  COLOR_INSTANCE_X_COLOR (c) = color;

#ifdef HAVE_XFT
  xftColor.pixel = color.pixel;
  xftColor.color.red = color.red;
  xftColor.color.green = color.green;
  xftColor.color.blue = color.blue;
  xftColor.color.alpha = 0xffff;

  COLOR_INSTANCE_X_XFTCOLOR (c) = xftColor;
#endif

  return 1;
}

static void
x_print_color_instance (Lisp_Color_Instance *c,
			Lisp_Object printcharfun,
			int UNUSED (escapeflag))
{
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  write_fmt_string (printcharfun, " %ld=(%X,%X,%X)",
		    color.pixel, color.red, color.green, color.blue);
}

static void
x_finalize_color_instance (Lisp_Color_Instance *c)
{
  if (c->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (c->device)))
	{
	  if (COLOR_INSTANCE_X_DEALLOC (c))
	    {
	      XFreeColors (DEVICE_X_DISPLAY (XDEVICE (c->device)),
			   DEVICE_X_COLORMAP (XDEVICE (c->device)),
			   &COLOR_INSTANCE_X_COLOR (c).pixel, 1, 0);
	    }
	}
      xfree (c->data);
      c->data = 0;
    }
}

/* Color instances are equal if they resolve to the same color on the
   screen (have the same RGB values).  I imagine that
   "same RGB values" == "same cell in the colormap."  Arguably we should
   be comparing their names or pixel values instead. */

static int
x_color_instance_equal (Lisp_Color_Instance *c1,
			Lisp_Color_Instance *c2,
			int UNUSED (depth))
{
  XColor color1 = COLOR_INSTANCE_X_COLOR (c1);
  XColor color2 = COLOR_INSTANCE_X_COLOR (c2);
  return ((color1.red == color2.red) &&
	  (color1.green == color2.green) &&
	  (color1.blue == color2.blue));
}

static Hashcode
x_color_instance_hash (Lisp_Color_Instance *c, int UNUSED (depth))
{
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return HASH3 (color.red, color.green, color.blue);
}

static Lisp_Object
x_color_instance_rgb_components (Lisp_Color_Instance *c)
{
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return (list3 (make_int (color.red),
		 make_int (color.green),
		 make_int (color.blue)));
}

static int
x_valid_color_name_p (struct device *d, Lisp_Object color)
{
  XColor c;
  Display *dpy = DEVICE_X_DISPLAY (d);
  Colormap cmap = DEVICE_X_COLORMAP (d);
  const Extbyte *extname;

  extname = LISP_STRING_TO_EXTERNAL (color, Qx_color_name_encoding);

  return XParseColor (dpy, cmap, extname, &c);
}

static Lisp_Object
x_color_list (void)
{
  return call0 (intern ("x-color-list-internal"));
}


/************************************************************************/
/*                           font instances                             */
/************************************************************************/


static int
x_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object UNUSED (name),
			    Lisp_Object device, Error_Behavior errb)
{
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  Extbyte *extname;
  XFontStruct *fs = NULL;	/* _F_ont _S_truct */
#ifdef HAVE_XFT
  XftFont *rf = NULL;		/* _R_ender _F_ont (X Render extension) */
#else
#define rf (0)
#endif

#ifdef HAVE_XFT
  DEBUG_XFT1 (2, "attempting to initialize font spec %s\n",
	      XSTRING_DATA(f->name));
  /* #### serialize (optimize) these later... */
  /* #### This function really needs to go away.
     The problem is that the fontconfig/Xft functions work much too hard
     to ensure that something is returned; but that something need not be
     at all close to what we asked for. */
  extname = LISP_STRING_TO_EXTERNAL (f->name, Qfc_font_name_encoding);
  rf = xft_open_font_by_name (dpy, extname);
#endif
  extname = LISP_STRING_TO_EXTERNAL (f->name, Qx_font_name_encoding);
  /* With XFree86 4.0's fonts, XListFonts returns an entry for
     -isas-fangsong ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0 but
     an XLoadQueryFont on the corresponding XLFD returns NULL.

     XListFonts is not trustworthy (of course, this is news to exactly
     no-one used to reading XEmacs source.) */
  fs = XLoadQueryFont (dpy, extname);
      
  if (!fs && !rf)
    {
      /* #### should this refer to X and/or Xft? */
      maybe_signal_error (Qgui_error, "Couldn't load font", f->name,
			  Qfont, errb);
      return 0;
    }

  if (rf && fs)
    {
      XFreeFont (dpy, fs);
      fs = NULL;		/* we don' need no steenkin' X font */
    }

  if (fs && !fs->max_bounds.width)
    {
      /* yes, this has been known to happen. */
      XFreeFont (dpy, fs);
      fs = NULL;
      maybe_signal_error (Qgui_error, "X font is too small", f->name, Qfont,
			  errb);
      return 0;
    }

  /* Now that we're sure that we will succeed, we can allocate data without
     fear that the finalize method may get fucked. */
  f->data = xnew (struct x_font_instance_data);

#ifdef HAVE_XFT
  FONT_INSTANCE_X_XFTFONT (f) = rf;
  if (rf)
    /* Have an Xft font, initialize font info from it. */
    {
      DEBUG_XFT4 (2, "pre-initial ascent %d descent %d width %d height %d\n",
		  f->ascent, f->descent, f->width, f->height);

      /* #### This shit is just plain wrong unless we have a character cell
	 font.  It really hoses us on large repertoire Unicode fonts with
	 "double-width" characters. */
      f->ascent = rf->ascent;
      f->descent = rf->descent;
      {
	/* This is an approximation that AFAIK only gets used to compute
	   cell size for estimating window dimensions.  The test_string8
	   is an  ASCII string whose characters should approximate the
	   distribution of widths expected in real text.  */
	static const FcChar8 test_string8[] = "Mmneei";
	static const int len = sizeof (test_string8) - 1;
	XGlyphInfo glyphinfo;

	XftTextExtents8 (dpy, rf, test_string8, len, &glyphinfo);
	/* #### maybe should be glyphinfo.xOff - glyphinfo.x? */
	f->width = (2*glyphinfo.width + len)/(2*len);
      }
      f->height = rf->height;
      f->proportional_p = 1; 	/* we can't recognize monospaced fonts! */

      /* #### This message appears wa-a-ay too often!
	 We probably need to cache truenames or something?
	 Even if Xft does it for us, we cons too many font instances. */
      DEBUG_XFT4 (0,
	"initialized metrics ascent %d descent %d width %d height %d\n",
	f->ascent, f->descent, f->width, f->height);
    } 
  else
    {
      DEBUG_XFT1 (0, "couldn't initialize Xft font %s\n",
		  XSTRING_DATA(f->name));
    }
#endif

  FONT_INSTANCE_X_FONT (f) = fs;
  if (fs)
    /* Have to use a core font, initialize font info from it. */
    {
      f->ascent = fs->ascent;
      f->descent = fs->descent;
      f->height = fs->ascent + fs->descent;
      {
	/* following change suggested by Ted Phelps <phelps@dstc.edu.au> */
	int def_char = 'n'; /*fs->default_char;*/
	int byte1, byte2;

      once_more:
	byte1 = def_char >> 8;
	byte2 = def_char & 0xFF;

	if (fs->per_char)
	  {
	    /* Old versions of the R5 font server have garbage (>63k) as
	       def_char. 'n' might not be a valid character. */
	    if (byte1 < (int) fs->min_byte1         ||
		byte1 > (int) fs->max_byte1         ||
		byte2 < (int) fs->min_char_or_byte2 ||
		byte2 > (int) fs->max_char_or_byte2)
	      f->width = 0;
	    else
	      f->width = fs->per_char[(byte1 - fs->min_byte1) *
				      (fs->max_char_or_byte2 -
				       fs->min_char_or_byte2 + 1) +
				      (byte2 - fs->min_char_or_byte2)].width;
	  }
	else
	  f->width = fs->max_bounds.width;

	/* Some fonts have a default char whose width is 0.  This is no good.
	   If that's the case, first try 'n' as the default char, and if n has
	   0 width too (unlikely) then just use the max width. */
	if (f->width == 0)
	  {
	    if (def_char == (int) fs->default_char)
	      f->width = fs->max_bounds.width;
	    else
	      {
		def_char = fs->default_char;
		goto once_more;
	      }
	  }
      }

      /* If all characters don't exist then there could potentially be
	 0-width characters lurking out there.  Not setting this flag
	 trips an optimization that would make them appear to have width
	 to redisplay.  This is bad.  So we set it if not all characters
	 have the same width or if not all characters are defined. */
      /* #### This sucks.  There is a measurable performance increase
	 when using proportional width fonts if this flag is not set.
	 Unfortunately so many of the fucking X fonts are not fully
	 defined that we could almost just get rid of this damn flag and
	 make it an assertion. */
      f->proportional_p = (fs->min_bounds.width != fs->max_bounds.width ||
			   (x_handle_non_fully_specified_fonts &&
			    !fs->all_chars_exist));
    }

#ifdef HAVE_XFT
  if (debug_xft > 0)
    {
      int n = 3, d = 5;
      /* check for weirdness */
      if (n * f->height < d * f->width)
	stderr_out ("font %s: width:height is %d:%d, larger than %d:%d\n",
		    XSTRING_DATA(f->name), f->width, f->height, n, d);
      if (f->height <= 0 || f->width <= 0)
	stderr_out ("bogus dimensions of font %s: width = %d, height = %d\n",
		    XSTRING_DATA(f->name), f->width, f->height);
      stderr_out ("initialized font %s\n", XSTRING_DATA(f->name));
    }
#else
#undef rf
#endif

  return 1;
}

static void
x_print_font_instance (Lisp_Font_Instance *f,
		       Lisp_Object printcharfun,
		       int UNUSED (escapeflag))
{
  /* We should print information here about initial vs. final stages; we
     can't rely on the device charset stage cache for that,
     unfortunately. */
  if (FONT_INSTANCE_X_FONT (f))
      write_fmt_string (printcharfun, " font id: 0x%lx,",
			(unsigned long) FONT_INSTANCE_X_FONT (f)->fid);

#ifdef HAVE_XFT
  /* #### What should we do here?  For now, print the address. */
  if (FONT_INSTANCE_X_XFTFONT (f))
    write_fmt_string (printcharfun, " xft font: 0x%lx",
		      (unsigned long) FONT_INSTANCE_X_XFTFONT (f));
#endif
}

static void
x_finalize_font_instance (Lisp_Font_Instance *f)
{

#ifdef HAVE_XFT
  DEBUG_XFT1 (0, "finalizing %s\n", (STRINGP (f->name)
				   ? (char *) XSTRING_DATA (f->name)
				   : "(unnamed font)"));
#endif

  if (f->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (f->device)))
	{
	  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (f->device));

	  if (FONT_INSTANCE_X_FONT (f))
	    XFreeFont (dpy, FONT_INSTANCE_X_FONT (f));
#ifdef HAVE_XFT
	  if (FONT_INSTANCE_X_XFTFONT (f))
	    XftFontClose (dpy, FONT_INSTANCE_X_XFTFONT (f));
#endif
	}
      xfree (f->data);
      f->data = 0;
    }
}

/* Determining the truename of a font is hard.  (Big surprise.)

   This is not true for fontconfig.  Each font has a (nearly) canonical
   representation up to permutation of the order of properties.  It is
   possible to construct a name which exactly identifies the properties of
   the current font.  However, it is theoretically possible that there exists
   another font with a super set of those properties that would happen to get
   selected. -- sjt

   By "truename" we mean an XLFD-form name which contains no wildcards, yet
   which resolves to *exactly* the same font as the one which we already have
   the (probably wildcarded) name and `XFontStruct' of.

   One might think that the first font returned by XListFonts would be the one
   that XOpenFont would pick.  Apparently this is the case on some servers,
   but not on others.  It would seem not to be specified.

   The MIT R5 server sometimes appears to be picking the lexicographically
   smallest font which matches the name (thus picking "adobe" fonts before
   "bitstream" fonts even if the bitstream fonts are earlier in the path, and
   also picking 100dpi adobe fonts over 75dpi adobe fonts even though the
   75dpi are in the path earlier) but sometimes appears to be doing something
   else entirely (for example, removing the bitstream fonts from the path will
   cause the 75dpi adobe fonts to be used instead of the 100dpi, even though
   their relative positions in the path (and their names!) have not changed).

   The documentation for XSetFontPath() seems to indicate that the order of
   entries in the font path means something, but it's pretty noncommittal about
   it, and the spirit of the law is apparently not being obeyed...

   All the fonts I've seen have a property named `FONT' which contains the
   truename of the font.  However, there are two problems with using this: the
   first is that the X Protocol Document is quite explicit that all properties
   are optional, so we can't depend on it being there.  The second is that
   it's conceivable that this alleged truename isn't actually accessible as a
   font, due to some difference of opinion between the font designers and
   whoever installed the font on the system.

   So, our first attempt is to look for a FONT property, and then verify that
   the name there is a valid name by running XListFonts on it.  There's still
   the potential that this could be true but we could still be being lied to,
   but that seems pretty remote.

     Late breaking news: I've gotten reports that SunOS 4.1.3U1
     with OpenWound 3.0 has a font whose truename is really
     "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"
     but whose FONT property contains "Courier".

     So we disbelieve the FONT property unless it begins with a dash and
     is more than 30 characters long.  X Windows: The defacto substandard.
     X Windows: Complex nonsolutions to simple nonproblems.  X Windows:
     Live the nightmare.

   If the FONT property doesn't exist, then we try and construct an XLFD name
   out of the other font properties (FOUNDRY, FAMILY_NAME, WEIGHT_NAME, etc).
   This is necessary at least for some versions of OpenWound.  But who knows
   what the future will bring.

   If that doesn't work, then we use XListFonts and either take the first font
   (which I think is the most sensible thing) or we find the lexicographically
   least, depending on whether the preprocessor constant `XOPENFONT_SORTS' is
   defined.  This sucks because the two behaviors are a property of the server
   being used, not the architecture on which emacs has been compiled.  Also,
   as I described above, sorting isn't ALWAYS what the server does.  Really it
   does something seemingly random.  There is no reliable way to win if the
   FONT property isn't present.

   Another possibility which I haven't bothered to implement would be to map
   over all of the matching fonts and find the first one that has the same
   character metrics as the font we already have loaded.  Even if this didn't
   return exactly the same font, it would at least return one whose characters
   were the same sizes, which would probably be good enough.

   More late-breaking news: on RS/6000 AIX 3.2.4, the expression
        XLoadQueryFont (dpy, "-*-Fixed-Medium-R-*-*-*-130-75-75-*-*-ISO8859-1")
   actually returns the font
        -Misc-Fixed-Medium-R-Normal--13-120-75-75-C-80-ISO8859-1
   which is crazy, because that font doesn't even match that pattern!  It is
   also not included in the output produced by `xlsfonts' with that pattern.

   So this is yet another example of XListFonts() and XOpenFont() using
   completely different algorithms.  This, however, is a goofier example of
   this bug, because in this case, it's not just the search order that is
   different -- the sets don't even intersect.

   If anyone has any better ideas how to do this, or any insights on what it is
   that the various servers are actually doing, please let me know!  -- jwz. */

static int
valid_x_font_name_p (Display *dpy, Extbyte *name)
{
  /* Maybe this should be implemented by calling XLoadFont and trapping
     the error.  That would be a lot of work, and wasteful as hell, but
     might be more correct.
   */
  int nnames = 0;
  Extbyte **names = 0;
  if (! name)
    return 0;
  names = XListFonts (dpy, name, 1, &nnames);
  if (names)
    XFreeFontNames (names);
  return (nnames != 0);
}

static Extbyte *
truename_via_FONT_prop (Display *dpy, XFontStruct *font)
{
  unsigned long value = 0;
  Extbyte *result = 0;
  if (XGetFontProperty (font, XA_FONT, &value))
    result = XGetAtomName (dpy, value);
  /* result is now 0, or the string value of the FONT property. */
  if (result)
    {
      /* Verify that result is an XLFD name (roughly...) */
      if (result [0] != '-' || strlen (result) < 30)
	{
	  XFree (result);
	  result = 0;
	}
    }
  return result;	/* this must be freed by caller if non-0 */
}

static Extbyte *
truename_via_random_props (Display *dpy, XFontStruct *font)
{
  struct device *d = get_device_from_display (dpy);
  unsigned long value = 0;
  Extbyte *foundry, *family, *weight, *slant, *setwidth, *add_style;
  unsigned long pixel, point, res_x, res_y;
  Extbyte *spacing;
  unsigned long avg_width;
  Extbyte *registry, *encoding;
  Extbyte composed_name [2048];
  int ok = 0;
  Extbyte *result;

#define get_string(atom,var)				\
  if (XGetFontProperty (font, (atom), &value))		\
    var = XGetAtomName (dpy, value);			\
  else	{						\
    var = 0;						\
    goto FAIL; }
#define get_number(atom,var)				\
  if (!XGetFontProperty (font, (atom), &var) ||		\
      var > 999)					\
    goto FAIL;

  foundry = family = weight = slant = setwidth = 0;
  add_style = spacing = registry = encoding = 0;

  get_string (DEVICE_XATOM_FOUNDRY (d), foundry);
  get_string (DEVICE_XATOM_FAMILY_NAME (d), family);
  get_string (DEVICE_XATOM_WEIGHT_NAME (d), weight);
  get_string (DEVICE_XATOM_SLANT (d), slant);
  get_string (DEVICE_XATOM_SETWIDTH_NAME (d), setwidth);
  get_string (DEVICE_XATOM_ADD_STYLE_NAME (d), add_style);
  get_number (DEVICE_XATOM_PIXEL_SIZE (d), pixel);
  get_number (DEVICE_XATOM_POINT_SIZE (d), point);
  get_number (DEVICE_XATOM_RESOLUTION_X (d), res_x);
  get_number (DEVICE_XATOM_RESOLUTION_Y (d), res_y);
  get_string (DEVICE_XATOM_SPACING (d), spacing);
  get_number (DEVICE_XATOM_AVERAGE_WIDTH (d), avg_width);
  get_string (DEVICE_XATOM_CHARSET_REGISTRY (d), registry);
  get_string (DEVICE_XATOM_CHARSET_ENCODING (d), encoding);
#undef get_number
#undef get_string

  sprintf (composed_name,
	   "-%s-%s-%s-%s-%s-%s-%ld-%ld-%ld-%ld-%s-%ld-%s-%s",
	   foundry, family, weight, slant, setwidth, add_style, pixel,
	   point, res_x, res_y, spacing, avg_width, registry, encoding);
  ok = 1;

 FAIL:
  if (ok)
    {
      int L = strlen (composed_name) + 1;
      result = xnew_extbytes (L);
      strncpy (result, composed_name, L);
    }
  else
    result = 0;

  if (foundry) XFree (foundry);
  if (family) XFree (family);
  if (weight) XFree (weight);
  if (slant) XFree (slant);
  if (setwidth) XFree (setwidth);
  if (add_style) XFree (add_style);
  if (spacing) XFree (spacing);
  if (registry) XFree (registry);
  if (encoding) XFree (encoding);

  return result;
}

/* XListFonts doesn't allocate memory unconditionally based on this. (For
   XFree86 in 2005, at least. */
#define MAX_FONT_COUNT INT_MAX

static Extbyte *
truename_via_XListFonts (Display *dpy, Extbyte *font_name)
{
  Extbyte *result = 0;
  Extbyte **names;
  int count = 0;

#ifndef XOPENFONT_SORTS
  /* In a sensible world, the first font returned by XListFonts()
     would be the font that XOpenFont() would use.  */
  names = XListFonts (dpy, font_name, 1, &count);
  if (count) result = names [0];
#else
  /* But the world I live in is much more perverse. */
  names = XListFonts (dpy, font_name, MAX_FONT_COUNT, &count);
  /* Find the lexicographic minimum of names[].
     (#### Should we be comparing case-insensitively?) */
  while (count--)
    /* [[ !!#### Not Mule-friendly ]]
       Doesn't matter, XLFDs are HPC (old) or Latin1 (modern).  If they
       aren't, who knows what they are? -- sjt */
    if (result == 0 || (strcmp (result, names [count]) < 0))
      result = names [count];
#endif

  if (result)
    result = xstrdup (result);
  if (names)
    XFreeFontNames (names);

  return result;	/* this must be freed by caller if non-0 */
}

static Lisp_Object
x_font_truename (Display *dpy, Extbyte *name, XFontStruct *font)
{
  Extbyte *truename_FONT = 0;
  Extbyte *truename_random = 0;
  Extbyte *truename = 0;

  /* The search order is:
     - if FONT property exists, and is a valid name, return it.
     - if the other props exist, and add up to a valid name, return it.
     - if we find a matching name with XListFonts, return it.
     - if FONT property exists, return it regardless.
     - if other props exist, return the resultant name regardless.
     - else return 0.
   */

  truename = truename_FONT = truename_via_FONT_prop (dpy, font);
  if (truename && !valid_x_font_name_p (dpy, truename))
    truename = 0;
  if (!truename)
    truename = truename_random = truename_via_random_props (dpy, font);
  if (truename && !valid_x_font_name_p (dpy, truename))
    truename = 0;
  if (!truename && name)
    truename = truename_via_XListFonts (dpy, name);

  if (!truename)
    {
      /* Gag - we weren't able to find a seemingly-valid truename.
	 Well, maybe we're on one of those braindead systems where
	 XListFonts() and XLoadFont() are in violent disagreement.
	 If we were able to compute a truename, try using that even
	 if evidence suggests that it's not a valid name - because
	 maybe it is, really, and that's better than nothing.
	 X Windows: You'll envy the dead.
       */
      if (truename_FONT)
	truename = truename_FONT;
      else if (truename_random)
	truename = truename_random;
    }

  /* One or both of these are not being used - free them. */
  if (truename_FONT && truename_FONT != truename)
    XFree (truename_FONT);
  if (truename_random && truename_random != truename)
    XFree (truename_random);

  if (truename)
    {
      Lisp_Object result = build_extstring (truename, Qx_font_name_encoding);
      XFree (truename);
      return result;
    }
  else
    return Qnil;
}

static Lisp_Object
x_font_instance_truename (Lisp_Font_Instance *f, Error_Behavior errb)
{
  struct device *d = XDEVICE (f->device);
  Display *dpy = DEVICE_X_DISPLAY (d);
  Extbyte *nameext;

  /* #### restructure this so that we return a valid truename at the end,
     and otherwise only return when we return something desperate that
     doesn't get stored for future use. */

#ifdef HAVE_XFT
  /* First, try an Xft font. */
  if (NILP (FONT_INSTANCE_TRUENAME (f)) && FONT_INSTANCE_X_XFTFONT (f))
    {
      /* The font is already open, we just unparse. */
      FcChar8 *res = FcNameUnparse (FONT_INSTANCE_X_XFTFONT (f)->pattern);
      if (! FONT_INSTANCE_X_XFTFONT (f)->pattern)
	{
	  maybe_signal_error (Qgui_error,
			      "Xft font present but lacks pattern",
			      wrap_font_instance(f), Qfont, errb);
	}
      if (res)
	{
	  FONT_INSTANCE_TRUENAME (f) = 
	    build_extstring ((Extbyte *) res, Qfc_font_name_encoding); 
	  free (res);
	  return FONT_INSTANCE_TRUENAME (f);
	}
      else
	{
	  maybe_signal_error (Qgui_error,
			      "Couldn't unparse Xft font to truename",
			      wrap_font_instance(f), Qfont, errb);
	  /* used to return Qnil here */
	}
    }
#endif	/* HAVE_XFT */

  /* OK, fall back to core font. */
  if (NILP (FONT_INSTANCE_TRUENAME (f))
      && FONT_INSTANCE_X_FONT (f))
    {
      nameext = LISP_STRING_TO_EXTERNAL (f->name, Qx_font_name_encoding);
      FONT_INSTANCE_TRUENAME (f) =
	x_font_truename (dpy, nameext, FONT_INSTANCE_X_FONT (f));
    }

  if (NILP (FONT_INSTANCE_TRUENAME (f)))
    {
      /* Urk, no luck.  Whine about our bad luck and exit. */
      Lisp_Object font_instance = wrap_font_instance (f);
      
      
      maybe_signal_error (Qgui_error, "Couldn't determine font truename",
			  font_instance, Qfont, errb);
      /* Ok, just this once, return the font name as the truename.
	 (This is only used by Fequal() right now.) */
      return f->name;
    }

  /* Return what we found. */
  return FONT_INSTANCE_TRUENAME (f);
}

static Lisp_Object
x_font_instance_properties (Lisp_Font_Instance *f)
{
  struct device *d = XDEVICE (f->device);
  int i;
  Lisp_Object result = Qnil;
  Display *dpy = DEVICE_X_DISPLAY (d);
  XFontProp *props = NULL;

  /* #### really should hack Xft fonts, too
     Strategy: fontconfig must have an iterator for this purpose. */
  if (! FONT_INSTANCE_X_FONT (f)) return result;

  props = FONT_INSTANCE_X_FONT (f)->properties;
  for (i = FONT_INSTANCE_X_FONT (f)->n_properties - 1; i >= 0; i--)
    {
      Lisp_Object name, value;
      Atom atom = props [i].name;
      Ibyte *name_str = 0;
      Bytecount name_len;
      Extbyte *namestrext = XGetAtomName (dpy, atom);

      if (namestrext)
	TO_INTERNAL_FORMAT (C_STRING, namestrext,
			    ALLOCA, (name_str, name_len),
			    Qx_atom_name_encoding);

      name = (name_str ? intern_istring (name_str) : Qnil);
      if (name_str &&
	  (atom == XA_FONT ||
	   atom == DEVICE_XATOM_FOUNDRY (d) ||
	   atom == DEVICE_XATOM_FAMILY_NAME (d) ||
	   atom == DEVICE_XATOM_WEIGHT_NAME (d) ||
	   atom == DEVICE_XATOM_SLANT (d) ||
	   atom == DEVICE_XATOM_SETWIDTH_NAME (d) ||
	   atom == DEVICE_XATOM_ADD_STYLE_NAME (d) ||
	   atom == DEVICE_XATOM_SPACING (d) ||
	   atom == DEVICE_XATOM_CHARSET_REGISTRY (d) ||
	   atom == DEVICE_XATOM_CHARSET_ENCODING (d) ||
	   !qxestrcmp_ascii (name_str, "CHARSET_COLLECTIONS") ||
	   !qxestrcmp_ascii (name_str, "FONTNAME_REGISTRY") ||
	   !qxestrcmp_ascii (name_str, "CLASSIFICATION") ||
	   !qxestrcmp_ascii (name_str, "COPYRIGHT") ||
	   !qxestrcmp_ascii (name_str, "DEVICE_FONT_NAME") ||
	   !qxestrcmp_ascii (name_str, "FULL_NAME") ||
	   !qxestrcmp_ascii (name_str, "MONOSPACED") ||
	   !qxestrcmp_ascii (name_str, "QUALITY") ||
	   !qxestrcmp_ascii (name_str, "RELATIVE_SET") ||
	   !qxestrcmp_ascii (name_str, "RELATIVE_WEIGHT") ||
	   !qxestrcmp_ascii (name_str, "STYLE")))
	{
	  Extbyte *val_str = XGetAtomName (dpy, props [i].card32);

	  value = (val_str ? build_extstring (val_str, Qx_atom_name_encoding)
		   : Qnil);
	}
      else
	value = make_int (props [i].card32);
      if (namestrext) XFree (namestrext);
      result = Fcons (Fcons (name, value), result);
    }
  return result;
}

static Lisp_Object
x_font_list (Lisp_Object pattern, Lisp_Object device, Lisp_Object maxnumber)
{
  Extbyte **names;
  int count = 0;
  int max_number = MAX_FONT_COUNT;
  Lisp_Object result = Qnil;
  const Extbyte *patternext;

  patternext = LISP_STRING_TO_EXTERNAL (pattern, Qx_font_name_encoding);

  if (!NILP(maxnumber) && INTP(maxnumber))
    {
      max_number = XINT(maxnumber);
    }

  names = XListFonts (DEVICE_X_DISPLAY (XDEVICE (device)),
		      patternext, max_number, &count);
  while (count--)
    result = Fcons (build_extstring (names[count], Qx_font_name_encoding),
		    result);
  if (names)
    XFreeFontNames (names);
  return result;
}

/* Include the charset support, shared, for the moment, with GTK.  */
#define THIS_IS_X
#include "objects-xlike-inc.c"


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_x (void)
{
}

void
console_type_create_objects_x (void)
{
  /* object methods */

  CONSOLE_HAS_METHOD (x, initialize_color_instance);
  CONSOLE_HAS_METHOD (x, print_color_instance);
  CONSOLE_HAS_METHOD (x, finalize_color_instance);
  CONSOLE_HAS_METHOD (x, color_instance_equal);
  CONSOLE_HAS_METHOD (x, color_instance_hash);
  CONSOLE_HAS_METHOD (x, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (x, valid_color_name_p);
  CONSOLE_HAS_METHOD (x, color_list);

  CONSOLE_HAS_METHOD (x, initialize_font_instance);
  CONSOLE_HAS_METHOD (x, print_font_instance);
  CONSOLE_HAS_METHOD (x, finalize_font_instance);
  CONSOLE_HAS_METHOD (x, font_instance_truename);
  CONSOLE_HAS_METHOD (x, font_instance_properties);
  CONSOLE_HAS_METHOD (x, font_list);
#ifdef MULE
  CONSOLE_HAS_METHOD (x, find_charset_font);
  CONSOLE_HAS_METHOD (x, font_spec_matches_charset);
#endif
}

void
vars_of_objects_x (void)
{
#ifdef DEBUG_XEMACS
  DEFVAR_INT ("debug-x-objects", &debug_x_objects /*
If non-zero, display debug information about X objects
*/ );
  debug_x_objects = 0;
#endif

  DEFVAR_BOOL ("x-handle-non-fully-specified-fonts",
	       &x_handle_non_fully_specified_fonts /*
If this is true then fonts which do not have all characters specified
will be considered to be proportional width even if they are actually
fixed-width.  If this is not done then characters which are supposed to
have 0 width may appear to actually have some width.

Note:  While setting this to t guarantees correct output in all
circumstances, it also causes a noticeable performance hit when using
fixed-width fonts.  Since most people don't use characters which could
cause problems this is set to nil by default.
*/ );
  x_handle_non_fully_specified_fonts = 0;

#ifdef HAVE_XFT
  Fprovide (intern ("xft-fonts"));
#endif
}

void
Xatoms_of_objects_x (struct device *d)
{
  Display *D = DEVICE_X_DISPLAY (d);

  DEVICE_XATOM_FOUNDRY         (d) = XInternAtom (D, "FOUNDRY",         False);
  DEVICE_XATOM_FAMILY_NAME     (d) = XInternAtom (D, "FAMILY_NAME",     False);
  DEVICE_XATOM_WEIGHT_NAME     (d) = XInternAtom (D, "WEIGHT_NAME",     False);
  DEVICE_XATOM_SLANT           (d) = XInternAtom (D, "SLANT",           False);
  DEVICE_XATOM_SETWIDTH_NAME   (d) = XInternAtom (D, "SETWIDTH_NAME",   False);
  DEVICE_XATOM_ADD_STYLE_NAME  (d) = XInternAtom (D, "ADD_STYLE_NAME",  False);
  DEVICE_XATOM_PIXEL_SIZE      (d) = XInternAtom (D, "PIXEL_SIZE",      False);
  DEVICE_XATOM_POINT_SIZE      (d) = XInternAtom (D, "POINT_SIZE",      False);
  DEVICE_XATOM_RESOLUTION_X    (d) = XInternAtom (D, "RESOLUTION_X",    False);
  DEVICE_XATOM_RESOLUTION_Y    (d) = XInternAtom (D, "RESOLUTION_Y",    False);
  DEVICE_XATOM_SPACING         (d) = XInternAtom (D, "SPACING",         False);
  DEVICE_XATOM_AVERAGE_WIDTH   (d) = XInternAtom (D, "AVERAGE_WIDTH",   False);
  DEVICE_XATOM_CHARSET_REGISTRY(d) = XInternAtom (D, "CHARSET_REGISTRY",False);
  DEVICE_XATOM_CHARSET_ENCODING(d) = XInternAtom (D, "CHARSET_ENCODING",False);
}
