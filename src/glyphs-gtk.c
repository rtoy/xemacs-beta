/* GTK-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996, 2001, 2002, 2004, 2005, 2010 Ben Wing
   Copyright (C) 1995 Sun Microsystems

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

/* Original author: Jamie Zawinski for 19.8
   font-truename stuff added by Jamie Zawinski for 19.10
   subwindow support added by Chuck Thompson
   additional XPM support added by Chuck Thompson
   initial X-Face support added by Stig
   rewritten/restructured by Ben Wing for 19.12/19.13
   GIF/JPEG support added by Ben Wing for 19.14
   PNG support added by Bill Perry for 19.14
   Improved GIF/JPEG support added by Bill Perry for 19.14
   Cleanup/simplification of error handling by Ben Wing for 19.14
   Pointer/icon overhaul, more restructuring by Ben Wing for 19.14
   GIF support changed to external GIFlib 3.1 by Jareth Hein for 21.0
   Many changes for color work and optimizations by Jareth Hein for 21.0
   Switch of GIF/JPEG/PNG to new EImage intermediate code by Jareth Hein for 21.0
   TIFF code by Jareth Hein for 21.0
   GIF/JPEG/PNG/TIFF code moved to new glyph-eimage.c for 21.0
   Gtk version by William Perry for 21.1

   TODO:
   Support the GrayScale, StaticColor and StaticGray visual classes.
   Convert images.el to C and stick it in here?
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "faces.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "glyphs.h"
#include "gui.h"
#include "imgproc.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"
#include "window.h"
#include "elhash.h"
#include "events.h"

#include "console-gtk-impl.h"
#include "glyphs-gtk.h"
#include "fontcolor-gtk-impl.h"
#include "ui-gtk.h"

#include "sysfile.h"

#include <setjmp.h>

#if defined (HAVE_XPM)
#include <X11/xpm.h>
#endif

/* Widget callback hash table callback slot. */
#define WIDGET_GLYPH_SLOT 0

DECLARE_IMAGE_INSTANTIATOR_FORMAT (nothing);
DECLARE_IMAGE_INSTANTIATOR_FORMAT (string);
DECLARE_IMAGE_INSTANTIATOR_FORMAT (formatted_string);
DECLARE_IMAGE_INSTANTIATOR_FORMAT (inherit);
#ifdef HAVE_JPEG
DECLARE_IMAGE_INSTANTIATOR_FORMAT (jpeg);
#endif
#ifdef HAVE_TIFF
DECLARE_IMAGE_INSTANTIATOR_FORMAT (tiff);
#endif
#ifdef HAVE_PNG
DECLARE_IMAGE_INSTANTIATOR_FORMAT (png);
#endif
#ifdef HAVE_GIF
DECLARE_IMAGE_INSTANTIATOR_FORMAT (gif);
#endif

#ifdef HAVE_XFACE
DEFINE_DEVICE_IIFORMAT (gtk, xface);
#endif

#ifdef HAVE_XPM
DEFINE_DEVICE_IIFORMAT (gtk, xpm);
#endif

DEFINE_DEVICE_IIFORMAT (gtk, xbm);
DEFINE_DEVICE_IIFORMAT (gtk, subwindow);

DEFINE_IMAGE_INSTANTIATOR_FORMAT (cursor_font);
Lisp_Object Qcursor_font;

/* We do not have support for making cursors from font glyphs in Gtk. */
/* DEFINE_IMAGE_INSTANTIATOR_FORMAT (font); */

DEFINE_IMAGE_INSTANTIATOR_FORMAT (autodetect);

#ifdef HAVE_WIDGETS
DECLARE_IMAGE_INSTANTIATOR_FORMAT (layout);
DEFINE_DEVICE_IIFORMAT (gtk, widget);
DEFINE_DEVICE_IIFORMAT (gtk, native_layout);
DEFINE_DEVICE_IIFORMAT (gtk, button);
DEFINE_DEVICE_IIFORMAT (gtk, progress_gauge);
DEFINE_DEVICE_IIFORMAT (gtk, edit_field);
DEFINE_DEVICE_IIFORMAT (gtk, combo_box);
DEFINE_DEVICE_IIFORMAT (gtk, tab_control);
DEFINE_DEVICE_IIFORMAT (gtk, label);
#endif

static void update_widget_face (GtkWidget *w, Lisp_Image_Instance *ii,
				Lisp_Object domain);
static void cursor_font_instantiate (Lisp_Object image_instance,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     Lisp_Object domain);

static GdkCursorType cursor_name_to_index (const char *name);

#ifndef BitmapSuccess
#define BitmapSuccess           0
#define BitmapOpenFailed        1
#define BitmapFileInvalid       2
#define BitmapNoMemory          3
#endif

#include "bitmaps.h"

DEFINE_IMAGE_INSTANTIATOR_FORMAT (gtk_resource);
Lisp_Object Qgtk_resource;
#ifdef HAVE_WIDGETS
Lisp_Object Qgtk_widget_instantiate_internal, Qgtk_widget_property_internal;
Lisp_Object Qgtk_widget_redisplay_internal, Qgtk_widget_set_style;
#endif

#ifdef HAVE_GTK
Lisp_Object Vgtk_cursor_names;
#endif


/************************************************************************/
/*                      image instance methods                          */
/************************************************************************/

/* Convert from a series of RGB triples to a GdkPixbuf. */
static GdkPixbuf *
convert_EImage_to_GDKPixbuf (Lisp_Object device, int width, int height,
			     unsigned char *pic)
{
  GdkVisual *vis;
  GdkVisualType vtype;
  GdkPixbuf *out;
  int i, j;
  /* int depth, byte_cnt; */
  int rowstride, n_channels;
  int rd,gr,bl;
  guchar *data, *ip, *dp = NULL;

  vis = DEVICE_GTK_VISUAL (XDEVICE(device));
  vtype = gdk_visual_get_visual_type (vis);

  if (vtype == GDK_VISUAL_GRAYSCALE || vtype == GDK_VISUAL_STATIC_COLOR ||
      vtype == GDK_VISUAL_STATIC_GRAY)
    {
      /* #### Implement me!!! */
      return NULL;
    }

  out = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE /* has_alpha */,
                        8, width, height);

  if (!out)
    return NULL;

  rowstride = gdk_pixbuf_get_rowstride (out);
  n_channels = gdk_pixbuf_get_n_channels (out);

  data = gdk_pixbuf_get_pixels (out);

  if (!data)
    return NULL;
  {
      unsigned long rshift,gshift,bshift,rbits,gbits,bbits;
      guint32 junk;
#ifdef HAVE_GTK2
      junk = vis->red_mask;
#endif
#ifdef HAVE_GTK_3
      gdk_visual_get_red_pixel_details(vis, &junk, NULL, NULL);
#endif
      rshift = 0;
      while ((junk & 0x1) == 0)
	{
	  junk = junk >> 1;
	  rshift ++;
	}
      rbits = 0;
      while (junk != 0)
	{
	  junk = junk >> 1;
	  rbits++;
	}
#ifdef HAVE_GTK2
      junk = vis->green_mask;
#endif
#ifdef HAVE_GTK3
      gdk_visual_get_green_pixel_details(vis, &junk, NULL, NULL);
#endif
      gshift = 0;
      while ((junk & 0x1) == 0)
	{
	  junk = junk >> 1;
	  gshift ++;
	}
      gbits = 0;
      while (junk != 0)
	{
	  junk = junk >> 1;
	  gbits++;
	}
#ifdef HAVE_GTK2
      junk = vis->blue_mask;
#endif
#ifdef HAVE_GTK3
      gdk_visual_get_blue_pixel_details(vis, &junk, NULL, NULL);
#endif
      bshift = 0;
      while ((junk & 0x1) == 0)
	{
	  junk = junk >> 1;
	  bshift ++;
	}
      bbits = 0;
      while (junk != 0)
	{
	  junk = junk >> 1;
	  bbits++;
	}
      ip = pic;
      for (i = 0; i < height; i++)
	{
	  for (j = 0; j < width; j++)
	    {
	      if (rbits > 8)
		rd = *ip++ << (rbits - 8);
	      else
		rd = *ip++ >> (8 - rbits);
	      if (gbits > 8)
		gr = *ip++ << (gbits - 8);
	      else
		gr = *ip++ >> (8 - gbits);
	      if (bbits > 8)
		bl = *ip++ << (bbits - 8);
	      else
		bl = *ip++ >> (8 - bbits);

              dp = data + i * rowstride + j * n_channels;
              dp[0] = rd;
              dp[1] = gr;
              dp[2] = bl;
              dp[3] = 255;
#ifdef OLD
	      conv.val = (rd << rshift) | (gr << gshift) | (bl << bshift);
#if G_BYTE_ORDER == G_BIG_ENDIAN
	      if (outimg->byte_order == GDK_MSB_FIRST)
		for (q = 4-byte_cnt; q < 4; q++) *dp++ = conv.cp[q];
	      else
		for (q = 3; q >= 4-byte_cnt; q--) *dp++ = conv.cp[q];
#else
	      if (outimg->byte_order == GDK_MSB_FIRST)
		for (q = byte_cnt-1; q >= 0; q--) *dp++ = conv.cp[q];
	      else
		for (q = 0; q < byte_cnt; q++) *dp++ = conv.cp[q];
#endif
#endif
	    }
	}
    }
  return out;
}

static void
gtk_print_image_instance (struct Lisp_Image_Instance *p,
			  Lisp_Object printcharfun,
			  int UNUSED (escapeflag))
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      write_fmt_string (printcharfun, " (0x%lx",
			(unsigned long) IMAGE_INSTANCE_GTK_PIXMAP (p));
      if (IMAGE_INSTANCE_GTK_MASK (p))
	write_fmt_string (printcharfun, "/0x%lx",
			  (unsigned long) IMAGE_INSTANCE_GTK_MASK (p));
      write_ascstring (printcharfun, ")");
      break;
#ifdef HAVE_SUBWINDOWS
    case IMAGE_SUBWINDOW:
      /* #### implement me */
#endif
    default:
      break;
    }
}

static void
gtk_finalize_image_instance (struct Lisp_Image_Instance *p)
{
  if (!p->data)
    return;

  if (DEVICE_LIVE_P (XDEVICE (p->device)))
    {
      if (0)
	;
#ifdef HAVE_WIDGETS
      if (IMAGE_INSTANCE_TYPE (p) == IMAGE_WIDGET)
	{
	  if (IMAGE_INSTANCE_SUBWINDOW_ID (p))
	    {
	      gtk_widget_destroy ((GtkWidget*) IMAGE_INSTANCE_SUBWINDOW_ID (p));

	      /* We can release the callbacks again. */
	      /* #### FIXME! */
	      /* ungcpro_popup_callbacks (...); */

	      /* IMAGE_INSTANCE_GTK_WIDGET_ID (p) = 0; */
	      IMAGE_INSTANCE_GTK_CLIPWIDGET (p) = 0;
	    }
	}
#endif
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
	{
	  ABORT();
	}
      else
	{
	  if (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p))
	    disable_glyph_animated_timeout (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p));

	  if (IMAGE_INSTANCE_GTK_MASK (p) &&
	      IMAGE_INSTANCE_GTK_MASK (p) != IMAGE_INSTANCE_GTK_PIXMAP (p))
	    g_object_unref (IMAGE_INSTANCE_GTK_MASK (p));

	  IMAGE_INSTANCE_PIXMAP_MASK (p) = 0;

	  g_object_unref (IMAGE_INSTANCE_GTK_PIXMAP (p));
	  IMAGE_INSTANCE_GTK_PIXMAP (p) = 0;

	  if (IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p))
	    {
	      int i;
	      for (i = 0; i < IMAGE_INSTANCE_PIXMAP_MAXSLICE (p); i++)
		if (IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p,i))
		  {
		    g_object_unref (IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p,i));
		    IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p, i) = 0;
		  }
	      xfree (IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p));
	      IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p) = 0;
	    }

	  if (IMAGE_INSTANCE_GTK_CURSOR (p))
	    {
#ifdef HAVE_GTK2
	      gdk_cursor_unref (IMAGE_INSTANCE_GTK_CURSOR (p));
#endif
#ifdef HAVE_GTK3
	      g_object_unref (IMAGE_INSTANCE_GTK_CURSOR (p));
#endif
	      IMAGE_INSTANCE_GTK_CURSOR (p) = 0;
	    }
	}
    }

  xfree (p->data);
  p->data = 0;
}

static int
gtk_image_instance_equal (struct Lisp_Image_Instance *p1,
			  struct Lisp_Image_Instance * UNUSED (p2),
			  int UNUSED (depth))
{
  switch (IMAGE_INSTANCE_TYPE (p1))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
return 0;
#ifdef HAVE_SUBWINDOWS
    case IMAGE_SUBWINDOW:
      /* #### implement me */
#endif
      break;
    default:
      break;
    }

  return 1;
}

#ifdef NOT_USED
static Hashcode
gtk_image_instance_hash (struct Lisp_Image_Instance *p, int UNUSED (depth),
			 Boolint UNUSED (equalp))
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      return IMAGE_INSTANCE_GTK_NPIXELS (p);
#ifdef HAVE_SUBWINDOWS
    case IMAGE_SUBWINDOW:
      /* #### implement me */
      return 0;
#endif
    default:
      return 0;
    }
}
#endif

/* Set all the slots in an image instance structure to reasonable
   default values.  This is used somewhere within an instantiate
   method.  It is assumed that the device slot within the image
   instance is already set -- this is the case when instantiate
   methods are called. */

static void
gtk_initialize_pixmap_image_instance (struct Lisp_Image_Instance *ii,
				      int slices,
				      enum image_instance_type type)
{
  ii->data = xnew_and_zero (struct gtk_image_instance_data);
  IMAGE_INSTANCE_PIXMAP_MAXSLICE (ii) = slices;
  IMAGE_INSTANCE_GTK_PIXMAP_SLICES (ii) =
    xnew_array_and_zero (GdkPixbuf *, slices);
  IMAGE_INSTANCE_TYPE (ii) = type;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_MASK_FILENAME (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_FG (ii) = Qnil;
  IMAGE_INSTANCE_PIXMAP_BG (ii) = Qnil;
}


/************************************************************************/
/*                        pixmap file functions                         */
/************************************************************************/

/* Where bitmaps are; initialized from resource database */
Lisp_Object Vgtk_bitmap_file_path;

#ifndef BITMAPDIR
#define BITMAPDIR "/usr/include/X11/bitmaps"
#endif

/* Given a pixmap filename, look through all of the "standard" places
   where the file might be located.  Return a full pathname if found;
   otherwise, return Qnil. */

static Lisp_Object
gtk_locate_pixmap_file (Lisp_Object name)
{
  /* This function can GC if IN_REDISPLAY is false */

  /* Check non-absolute pathnames with a directory component relative to
     the search path; that's the way Xt does it. */
  /* #### Unix-specific */
  if (string_byte (name, 0) == '/' ||
      (string_byte (name, 0) == '.' &&
       (string_byte (name, 1) == '/' ||
	(string_byte (name, 1) == '.' &&
	 (string_byte (name, 2) == '/')))))
    {
      if (!NILP (Ffile_readable_p (name)))
	return name;
      else
	return Qnil;
    }

  if (NILP (Vgtk_bitmap_file_path))
    {
      Vgtk_bitmap_file_path = nconc2 (Vgtk_bitmap_file_path,
				      (split_external_path (BITMAPDIR)));
    }

  {
    Lisp_Object found;
    if (locate_file (Vgtk_bitmap_file_path, name, Qnil, &found, R_OK) < 0)
      {
	Lisp_Object temp = list1 (Vdata_directory);
	struct gcpro gcpro1;

	GCPRO1 (temp);
	locate_file (temp, name, Qnil, &found, R_OK);
	UNGCPRO;
      }

    return found;
  }
}


/************************************************************************/
/*                           cursor functions                           */
/************************************************************************/

static void
register_cursor_name (const char *real_name, int value)
{
  Ibyte *name = alloca_ibytes (strlen (real_name));
  int len = strlen (real_name);
  int i;

  /* real_name is an ASCII string. */
  /* strlen ("GDK_") == 4 */
  for (i = 4; i < len; i++)
    {
      if (real_name[i] == 0)
        break;
      if (real_name[i] == '_')
        name[i - 4] = '-';
      else
        name[i - 4] = tolower (real_name[i]);
    }
  name[i-4] = 0;
  
  Vgtk_cursor_names = Facons (intern_istring (name), make_fixnum (value),
                              Vgtk_cursor_names);
}

/*
 * Cursor names are stored as an alist, symbol->internal enum value.
 * The first one is default.
 */
static void 
register_cursor_names (void)
{
  if (NILP (Vgtk_cursor_names))
    {
      /* Generated from GtkCursorType declaration. */
#define FROB_CURSOR_NAME(name, value) \
      register_cursor_name (#name , value)

      FROB_CURSOR_NAME (GDK_X_CURSOR, 0);
      FROB_CURSOR_NAME (GDK_ARROW, 2);
      FROB_CURSOR_NAME (GDK_BASED_ARROW_DOWN, 4);
      FROB_CURSOR_NAME (GDK_BASED_ARROW_UP, 6);
      FROB_CURSOR_NAME (GDK_BOAT, 8);
      FROB_CURSOR_NAME (GDK_BOGOSITY, 10);
      FROB_CURSOR_NAME (GDK_BOTTOM_LEFT_CORNER, 12);
      FROB_CURSOR_NAME (GDK_BOTTOM_RIGHT_CORNER, 14);
      FROB_CURSOR_NAME (GDK_BOTTOM_SIDE, 16);
      FROB_CURSOR_NAME (GDK_BOTTOM_TEE, 18);
      FROB_CURSOR_NAME (GDK_BOX_SPIRAL, 20);
      FROB_CURSOR_NAME (GDK_CENTER_PTR, 22);
      FROB_CURSOR_NAME (GDK_CIRCLE, 24);
      FROB_CURSOR_NAME (GDK_CLOCK, 26);
      FROB_CURSOR_NAME (GDK_COFFEE_MUG, 28);
      FROB_CURSOR_NAME (GDK_CROSS, 30);
      FROB_CURSOR_NAME (GDK_CROSS_REVERSE, 32);
      FROB_CURSOR_NAME (GDK_CROSSHAIR, 34);
      FROB_CURSOR_NAME (GDK_DIAMOND_CROSS, 36);
      FROB_CURSOR_NAME (GDK_DOT, 38);
      FROB_CURSOR_NAME (GDK_DOTBOX, 40);
      FROB_CURSOR_NAME (GDK_DOUBLE_ARROW, 42);
      FROB_CURSOR_NAME (GDK_DRAFT_LARGE, 44);
      FROB_CURSOR_NAME (GDK_DRAFT_SMALL, 46);
      FROB_CURSOR_NAME (GDK_DRAPED_BOX, 48);
      FROB_CURSOR_NAME (GDK_EXCHANGE, 50);
      FROB_CURSOR_NAME (GDK_FLEUR, 52);
      FROB_CURSOR_NAME (GDK_GOBBLER, 54);
      FROB_CURSOR_NAME (GDK_GUMBY, 56);
      FROB_CURSOR_NAME (GDK_HAND1, 58);
      FROB_CURSOR_NAME (GDK_HAND2, 60);
      FROB_CURSOR_NAME (GDK_HEART, 62);
      FROB_CURSOR_NAME (GDK_ICON, 64);
      FROB_CURSOR_NAME (GDK_IRON_CROSS, 66);
      FROB_CURSOR_NAME (GDK_LEFT_PTR, 68);
      FROB_CURSOR_NAME (GDK_LEFT_SIDE, 70);
      FROB_CURSOR_NAME (GDK_LEFT_TEE, 72);
      FROB_CURSOR_NAME (GDK_LEFTBUTTON, 74);
      FROB_CURSOR_NAME (GDK_LL_ANGLE, 76);
      FROB_CURSOR_NAME (GDK_LR_ANGLE, 78);
      FROB_CURSOR_NAME (GDK_MAN, 80);
      FROB_CURSOR_NAME (GDK_MIDDLEBUTTON, 82);
      FROB_CURSOR_NAME (GDK_MOUSE, 84);
      FROB_CURSOR_NAME (GDK_PENCIL, 86);
      FROB_CURSOR_NAME (GDK_PIRATE, 88);
      FROB_CURSOR_NAME (GDK_PLUS, 90);
      FROB_CURSOR_NAME (GDK_QUESTION_ARROW, 92);
      FROB_CURSOR_NAME (GDK_RIGHT_PTR, 94);
      FROB_CURSOR_NAME (GDK_RIGHT_SIDE, 96);
      FROB_CURSOR_NAME (GDK_RIGHT_TEE, 98);
      FROB_CURSOR_NAME (GDK_RIGHTBUTTON, 100);
      FROB_CURSOR_NAME (GDK_RTL_LOGO, 102);
      FROB_CURSOR_NAME (GDK_SAILBOAT, 104);
      FROB_CURSOR_NAME (GDK_SB_DOWN_ARROW, 106);
      FROB_CURSOR_NAME (GDK_SB_H_DOUBLE_ARROW, 108);
      FROB_CURSOR_NAME (GDK_SB_LEFT_ARROW, 110);
      FROB_CURSOR_NAME (GDK_SB_RIGHT_ARROW, 112);
      FROB_CURSOR_NAME (GDK_SB_UP_ARROW, 114);
      FROB_CURSOR_NAME (GDK_SB_V_DOUBLE_ARROW, 116);
      FROB_CURSOR_NAME (GDK_SHUTTLE, 118);
      FROB_CURSOR_NAME (GDK_SIZING, 120);
      FROB_CURSOR_NAME (GDK_SPIDER, 122);
      FROB_CURSOR_NAME (GDK_SPRAYCAN, 124);
      FROB_CURSOR_NAME (GDK_STAR, 126);
      FROB_CURSOR_NAME (GDK_TARGET, 128);
      FROB_CURSOR_NAME (GDK_TCROSS, 130);
      FROB_CURSOR_NAME (GDK_TOP_LEFT_ARROW, 132);
      FROB_CURSOR_NAME (GDK_TOP_LEFT_CORNER, 134);
      FROB_CURSOR_NAME (GDK_TOP_RIGHT_CORNER, 136);
      FROB_CURSOR_NAME (GDK_TOP_SIDE, 138);
      FROB_CURSOR_NAME (GDK_TOP_TEE, 140);
      FROB_CURSOR_NAME (GDK_TREK, 142);
      FROB_CURSOR_NAME (GDK_UL_ANGLE, 144);
      FROB_CURSOR_NAME (GDK_UMBRELLA, 146);
      FROB_CURSOR_NAME (GDK_UR_ANGLE, 148);
      FROB_CURSOR_NAME (GDK_WATCH, 150);
      FROB_CURSOR_NAME (GDK_XTERM, 152);

#undef FROB_CURSOR_NAME

      Vgtk_cursor_names = Fnreverse (Vgtk_cursor_names);
    }
}

/* Check that this server supports cursors of size WIDTH * HEIGHT.  If
   not, signal an error.  INSTANTIATOR is only used in the error
   message. */
static void
check_pointer_sizes (GdkDisplay *display, guint width, guint height,
		     Lisp_Object instantiator)
{
  guint max_width, max_height;

  gdk_display_get_maximal_cursor_size(display, &max_width, &max_height);
  if (width > max_width || height > max_height)
    signal_ferror_with_frob (Qgui_error, instantiator,
			     "pointer too large (%dx%d): "
			     "server requires %dx%d or smaller",
			     width, height, max_width, max_height);
}

static void
generate_cursor_fg_bg (Lisp_Object device, Lisp_Object *foreground,
		       Lisp_Object *background, GdkColor *xfg, GdkColor *xbg)
{
  if (!NILP (*foreground) && !COLOR_INSTANCEP (*foreground))
    *foreground =
      Fmake_color_instance (*foreground, device,
			    encode_error_behavior_flag (ERROR_ME));
  if (COLOR_INSTANCEP (*foreground))
    *xfg = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (*foreground));
  else
    {
      xfg->pixel = 0;
      xfg->red = xfg->green = xfg->blue = 0;
    }

  if (!NILP (*background) && !COLOR_INSTANCEP (*background))
    *background =
      Fmake_color_instance (*background, device,
			    encode_error_behavior_flag (ERROR_ME));
  if (COLOR_INSTANCEP (*background))
    *xbg = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (*background));
  else
    {
      xbg->pixel = 0;
      xbg->red = xbg->green = xbg->blue = ~0;
    }
}

static void
maybe_recolor_cursor (Lisp_Object UNUSED (image_instance),
		      Lisp_Object UNUSED (foreground),
		      Lisp_Object UNUSED (background))
{
#if 0
    /* #### BILL!!! */
  Lisp_Object device = XIMAGE_INSTANCE_DEVICE (image_instance);
  GdkColor xfg, xbg;

  generate_cursor_fg_bg (device, &foreground, &background, &xfg, &xbg);
  if (!NILP (foreground) || !NILP (background))
    {
      XRecolorCursor (DEVICE_X_DISPLAY (XDEVICE (device)),
		      XIMAGE_INSTANCE_GTK_CURSOR (image_instance),
		      &xfg, &xbg);
      XIMAGE_INSTANCE_PIXMAP_FG (image_instance) = foreground;
      XIMAGE_INSTANCE_PIXMAP_BG (image_instance) = background;
    }
#else
  /* stderr_out ("Don't know how to recolor cursors in Gtk!\n"); */
#endif
}


/************************************************************************/
/*                        color pixmap functions                        */
/************************************************************************/

/* Create a pointer from a color pixmap. */

static void
image_instance_convert_to_pointer (Lisp_Image_Instance *ii,
				   Lisp_Object instantiator,
				   Lisp_Object pointer_fg,
				   Lisp_Object pointer_bg)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GtkWidget *widget = DEVICE_GTK_APP_SHELL (XDEVICE (device));
  GdkDisplay *display = gtk_widget_get_display(widget);
  GdkPixbuf *pixbuf = IMAGE_INSTANCE_GTK_PIXMAP (ii);
  /* GdkPixbuf *mask = (GdkPixbuf *) IMAGE_INSTANCE_GTK_MASK (ii); */
  GdkColor fg, bg;
  gint xhot = 0, yhot = 0;
  int w, h;

  if (FIXNUMP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)))
    xhot = XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii));
  if (FIXNUMP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)))
    yhot = XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii));
  w = IMAGE_INSTANCE_PIXMAP_WIDTH (ii);
  h = IMAGE_INSTANCE_PIXMAP_HEIGHT (ii);

  check_pointer_sizes (gtk_widget_get_display
                       (DEVICE_GTK_APP_SHELL (XDEVICE (device))),
                       w, h, instantiator);

  generate_cursor_fg_bg (device, &pointer_fg, &pointer_bg,
			 &fg, &bg);
  IMAGE_INSTANCE_PIXMAP_FG (ii) = pointer_fg;
  IMAGE_INSTANCE_PIXMAP_BG (ii) = pointer_bg;
  IMAGE_INSTANCE_GTK_CURSOR (ii) =
    gdk_cursor_new_from_pixbuf (display, pixbuf, xhot, yhot);
}

/* Initialize an image instance from an XImage.

   DEST_MASK specifies the mask of allowed image types.

   PIXELS and NPIXELS specify an array of pixels that are used in
   the image.  These need to be kept around for the duration of the
   image.  When the image instance is freed, XFreeColors() will
   automatically be called on all the pixels specified here; thus,
   you should have allocated the pixels yourself using XAllocColor()
   or the like.  The array passed in is used directly without
   being copied, so it should be heap data created with xmalloc().
   It will be freed using xfree() when the image instance is
   destroyed.

   If this fails, signal an error.  INSTANTIATOR is only used
   in the error message.

   #### This should be able to handle conversion into `pointer'.
   Use the same code as for `xpm'. */

static void
init_image_instance_from_gdk_pixbuf (struct Lisp_Image_Instance *ii,
				     GdkPixbuf *gdk_pixbuf,
				     int dest_mask,
				     int slices,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  /* GdkWindow *d; */
  enum image_instance_type type;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  /* d = gtk_widget_get_window (DEVICE_GTK_APP_SHELL (XDEVICE (device))); */

  if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    type = IMAGE_COLOR_PIXMAP;
  else if (dest_mask & IMAGE_POINTER_MASK)
    type = IMAGE_POINTER;
  else
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK
			      | IMAGE_POINTER_MASK);

  gtk_initialize_pixmap_image_instance (ii, slices, IMAGE_COLOR_PIXMAP);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);
  IMAGE_INSTANCE_GTK_PIXMAP (ii) = gdk_pixbuf;
  IMAGE_INSTANCE_PIXMAP_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = gdk_pixbuf_get_width (gdk_pixbuf);
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = gdk_pixbuf_get_height (gdk_pixbuf);
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = gdk_pixbuf_get_bits_per_sample (gdk_pixbuf);

  g_object_ref (gdk_pixbuf);

  if (type == IMAGE_POINTER)
    image_instance_convert_to_pointer (ii, instantiator, pointer_fg,
				       pointer_bg);
}

static void
image_instance_add_gdk_pixbuf (Lisp_Image_Instance *ii,
			       GdkPixbuf *gdk_pixbuf,
			       int slice,
			       Lisp_Object UNUSED (instantiator))
{
  g_object_ref (gdk_pixbuf);
  IMAGE_INSTANCE_GTK_PIXMAP_SLICE (ii, slice) = gdk_pixbuf;
}

static void
gtk_init_image_instance_from_eimage (struct Lisp_Image_Instance *ii,
				     int width, int height,
				     int slices,
				     unsigned char *eimage,
				     int dest_mask,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     Lisp_Object UNUSED (domain))
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  int slice;
  GdkPixbuf *gdk_pixbuf;

  for (slice = 0; slice < slices; slice++)
    {
      gdk_pixbuf = convert_EImage_to_GDKPixbuf (device, width, height, eimage);
      if (!gdk_pixbuf)
	{
	  signal_image_error("EImage to GdkPixbuf conversion failed",
			     instantiator);
	}

      if (slice == 0)
	/* Now create the pixmap and set up the image instance */
	init_image_instance_from_gdk_pixbuf (ii, gdk_pixbuf, dest_mask,
					     slices, instantiator,
					     pointer_fg, pointer_bg);
      else
	image_instance_add_gdk_pixbuf (ii, gdk_pixbuf, slice, instantiator);

      if (gdk_pixbuf)
	{
	  g_object_unref (gdk_pixbuf);
	}
      gdk_pixbuf = 0;
    }
}

/* with help from glyphs-msw.c */

/* the bitmap data comes in the following format: Widths are padded to
   a multiple of 8.  Scan lines are stored in increasing byte order
   from left to right, little-endian within a byte.  0 = white, 1 =
   black. */

static int
get_bit (const Extbyte *bits, int row, int col, int width)
{
  const Extbyte *base = bits + row * ((width + 7) / 8);
  guchar bite = base [col / 8];
  int bitpos = col % 8;

#if G_BYTE_ORDER == G_LITTLE_ENDIAN
  int bit = bite & (1 << bitpos);
#else
  /* untested */
  int bit = bite & (0x80 >> bitpos);
#endif
  return bit;
}

static GdkPixbuf *
pixbuf_from_xbm_inline (Lisp_Object UNUSED (device), int width, int height,
			/* Note that data is in ext-format! */
			const Extbyte *bits, const GdkColor fg,
			const GdkColor bg)
{
  GdkPixbuf *out = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width, height);
  int rowstride;
  int n_channels;
  int i, j;
  guchar *data = NULL;

  if (out == NULL)
    return NULL;

  rowstride = gdk_pixbuf_get_rowstride (out);
  n_channels = gdk_pixbuf_get_n_channels (out);
  data = gdk_pixbuf_get_pixels (out);

  for (i = 0; i < height; i++)
    {
      for (j = 0; j < width; j++)
	{
	  int bit = get_bit (bits, i, j, width);
	  guchar *dp = data + i * rowstride + j * n_channels;

	  if (bit != 0)
	    {
	      *dp++ = fg.red;
	      *dp++ = fg.green;
	      *dp++ = fg.blue;
	    }
	  else
	    {
	      *dp++ = bg.red;
	      *dp++ = bg.green;
	      *dp++ = bg.blue;
	    }
	  /* Alpha */
	  *dp++ = 0xff;
	}
    }

  return out;
}

/* Given inline data for a mono pixmap, initialize the given
   image instance accordingly. */

static void
init_image_instance_from_xbm_inline (struct Lisp_Image_Instance *ii,
				     int width, int height,
				     /* Note that data is in ext-format! */
				     const char *bits,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     GdkPixbuf * UNUSED (mask),
				     Lisp_Object UNUSED (mask_filename))
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object foreground = find_keyword_in_vector (instantiator, Q_foreground);
  Lisp_Object background = find_keyword_in_vector (instantiator, Q_background);
  GdkColor fg;
  GdkColor bg;
  enum image_instance_type type;
  GdkWindow *draw = gtk_widget_get_window (DEVICE_GTK_APP_SHELL (XDEVICE (device)));
#ifdef HAVE_GTK2
  GdkDisplay *display = gdk_drawable_get_display (draw);
#endif
#ifdef HAVE_GTK3
  GdkDisplay *display = gdk_window_get_display (draw);
#endif
  GdkColor black;
  GdkColor white;

  gdk_color_parse ("black", &black);
  gdk_color_parse ("white", &white);

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  if ((dest_mask & IMAGE_MONO_PIXMAP_MASK) &&
      (dest_mask & IMAGE_COLOR_PIXMAP_MASK))
    {
      if (!NILP (foreground) || !NILP (background))
	type = IMAGE_COLOR_PIXMAP;
      else
	type = IMAGE_MONO_PIXMAP;
    }
  else if (dest_mask & IMAGE_MONO_PIXMAP_MASK)
    type = IMAGE_MONO_PIXMAP;
  else if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    type = IMAGE_COLOR_PIXMAP;
  else if (dest_mask & IMAGE_POINTER_MASK)
    type = IMAGE_POINTER;
  else
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_MONO_PIXMAP_MASK | IMAGE_COLOR_PIXMAP_MASK
			      | IMAGE_POINTER_MASK);

  gtk_initialize_pixmap_image_instance (ii, 1, type);
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = height;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  switch (type)
    {
    case IMAGE_MONO_PIXMAP:
      {
	IMAGE_INSTANCE_GTK_PIXMAP (ii) =
	  pixbuf_from_xbm_inline (device, width, height, (Extbyte *) bits,
				  black, white);
      }
      break;

    case IMAGE_COLOR_PIXMAP:
      {
	gint d = DEVICE_GTK_DEPTH (XDEVICE(device));

	if (!NILP (foreground) && !COLOR_INSTANCEP (foreground))
	  foreground =
	    Fmake_color_instance (foreground, device,
				  encode_error_behavior_flag (ERROR_ME));

	if (COLOR_INSTANCEP (foreground))
	  fg = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (foreground));

	if (!NILP (background) && !COLOR_INSTANCEP (background))
	  background =
	    Fmake_color_instance (background, device,
				  encode_error_behavior_flag (ERROR_ME));

	if (COLOR_INSTANCEP (background))
	  bg = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (background));

	/* We used to duplicate the pixels using XAllocColor(), to protect
	   against their getting freed.  Just as easy to just store the
	   color instances here and GC-protect them, so this doesn't
	   happen. */
	IMAGE_INSTANCE_PIXMAP_FG (ii) = foreground;
	IMAGE_INSTANCE_PIXMAP_BG (ii) = background;
	IMAGE_INSTANCE_GTK_PIXMAP (ii) =
	  gdk_pixbuf_new_from_data ((guchar *)bits, GDK_COLORSPACE_RGB, 0,
				    1, width, height, 8, NULL, NULL);
	IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = d;
      }
      break;

    case IMAGE_POINTER:
    {
	GdkColor fg_color, bg_color;
	GdkPixbuf *source;

	check_pointer_sizes (display, width, height, instantiator);

	source = gdk_pixbuf_new_from_data ((guchar *) bits, GDK_COLORSPACE_RGB, 0,
					   1, width, height, 8, NULL, NULL);

	if (NILP (foreground))
	  foreground = pointer_fg;
	if (NILP (background))
	  background = pointer_bg;
	generate_cursor_fg_bg (device, &foreground, &background,
			       &fg_color, &bg_color);

	IMAGE_INSTANCE_PIXMAP_FG (ii) = foreground;
	IMAGE_INSTANCE_PIXMAP_BG (ii) = background;
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) =
	  find_keyword_in_vector (instantiator, Q_hotspot_x);
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) =
	  find_keyword_in_vector (instantiator, Q_hotspot_y);
	IMAGE_INSTANCE_GTK_CURSOR (ii) =
	  gdk_cursor_new_from_pixbuf (display, source,
				      !NILP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)) ?
				      XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)) : 0,
				      !NILP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) ?
				      XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) : 0);

      }
      break;

    default:
      ABORT ();
    }
}

static void
xbm_instantiate_1 (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, int width, int height,
		   /* Note that data is in ext-format! */
		   const char *bits)
{
  /* Lisp_Object device = IMAGE_INSTANCE_DEVICE (XIMAGE_INSTANCE (image_instance)); */
  Lisp_Object mask_data = find_keyword_in_vector (instantiator, Q_mask_data);
  Lisp_Object mask_file = find_keyword_in_vector (instantiator, Q_mask_file);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  GdkPixbuf *mask = 0;
  const Extbyte *gcc_may_you_rot_in_hell;
  GdkColor black;
  GdkColor white;

  gdk_color_parse ("black", &black);
  gdk_color_parse ("white", &white);

  if (!NILP (mask_data))
    {
      gcc_may_you_rot_in_hell =
	LISP_STRING_TO_EXTERNAL (XCAR (XCDR (XCDR (mask_data))),
				 Qfile_name);
      mask =
	pixbuf_from_xbm_inline (IMAGE_INSTANCE_DEVICE (ii),
				XFIXNUM (XCAR (mask_data)),
				XFIXNUM (XCAR (XCDR (mask_data))),
				gcc_may_you_rot_in_hell,
				black, white);
    }

  init_image_instance_from_xbm_inline (ii, width, height, bits,
				       instantiator, pointer_fg, pointer_bg,
				       dest_mask, mask, mask_file);
}

/* Instantiate method for XBM's. */

static void
gtk_xbm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		     int dest_mask, Lisp_Object UNUSED (domain))
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  const char *gcc_go_home;

  assert (!NILP (data));

  gcc_go_home = LISP_STRING_TO_EXTERNAL (XCAR (XCDR (XCDR (data))), Qbinary);

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, XFIXNUM (XCAR (data)),
		     XFIXNUM (XCAR (XCDR (data))), gcc_go_home);
}


#ifdef HAVE_XPM
/**********************************************************************
 *                             XPM                                    *
 **********************************************************************/

struct color_symbol
{
  Ibyte*	name;
  GdkColor	color;
};

static struct color_symbol*
extract_xpm_color_names (Lisp_Object device,
			 Lisp_Object domain,
			 Lisp_Object color_symbol_alist,
			 int* nsymbols)
{
  /* This function can GC */
  Lisp_Object rest;
  Lisp_Object results = Qnil;
  int i, j;
  struct color_symbol *colortbl;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (results, device);

  /* We built up results to be (("name" . #<color>) ...) so that if an
     error happens we don't lose any malloc()ed data, or more importantly,
     leave any pixels allocated in the server. */
  i = 0;
  LIST_LOOP (rest, color_symbol_alist)
    {
      Lisp_Object cons = XCAR (rest);
      Lisp_Object name = XCAR (cons);
      Lisp_Object value = XCDR (cons);
      if (NILP (value))
	continue;
      if (STRINGP (value))
	value =
	  Fmake_color_instance
	  (value, device, encode_error_behavior_flag (ERROR_ME_DEBUG_WARN));
      else
	{
	  assert (COLOR_SPECIFIERP (value));
	  value = Fspecifier_instance (value, domain, Qnil, Qnil);
	}

      if (NILP (value))
	continue;
      results = noseeum_cons (noseeum_cons (name, value), results);
      i++;
    }
  UNGCPRO;			/* no more evaluation */

  *nsymbols=i;
  if (i == 0) return 0;

  colortbl = xnew_array_and_zero (struct color_symbol, i);

  for (j=0; j<i; j++)
    {
      Lisp_Object cons = XCAR (results);
      colortbl[j].color =
	* COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (XCDR (cons)));

      colortbl[j].name = XSTRING_DATA (XCAR (cons));
      free_cons (cons);
      cons = results;
      results = XCDR (results);
      free_cons (cons);
    }
  return colortbl;
}

static void
gtk_xpm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		     int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  int depth;
  /* GdkVisual *visual; */
  GdkPixbuf *pixbuf;
  int nsymbols = 0, i = 0;
  struct color_symbol *color_symbols = NULL;
  Lisp_Object color_symbol_alist = find_keyword_in_vector (instantiator,
							   Q_color_symbols);
  enum image_instance_type type;
  /* int force_mono; */
  struct gcpro gcpro1, gcpro2, gcpro3;
  const Extbyte * volatile dstring;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    type = IMAGE_COLOR_PIXMAP;
  else if (dest_mask & IMAGE_MONO_PIXMAP_MASK)
    type = IMAGE_MONO_PIXMAP;
  else if (dest_mask & IMAGE_POINTER_MASK)
    type = IMAGE_POINTER;
  else
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_MONO_PIXMAP_MASK | IMAGE_COLOR_PIXMAP_MASK
			      | IMAGE_POINTER_MASK);
  /* force_mono = (type != IMAGE_COLOR_PIXMAP); */

  GCPRO3 (device, data, color_symbol_alist);

  depth = DEVICE_GTK_DEPTH (XDEVICE (device));
  /* visual = DEVICE_GTK_VISUAL (XDEVICE (device)); */

  gtk_initialize_pixmap_image_instance (ii, 1, type);

  assert (!NILP (data));

  /* Extract all the entries from xpm-color-symbols */
  color_symbols = extract_xpm_color_names (device, domain, color_symbol_alist,
					   &nsymbols);
  assert (!NILP (data));

  dstring = LISP_STRING_TO_EXTERNAL (data, Qbinary);

  /*
   * GTK only uses the 'c' color entry of an XPM and doesn't use the symbolic
   * color names at all.  This is unfortunate because the way to change the
   * colors from lisp is by adding the symbolic names, and the new colors, to
   * the variable xpm-color-symbols.
   *
   * To get around this decode the XPM, add a 'c' entry of the desired color
   * for each matching symbolic color, recode the XPM and pass it to GTK.  The
   * decode and recode stages aren't too bad because this also performs the
   * external to internal format translation, which avoids contortions like
   * writing the XPM back to disk in order to get it processed.
   */
  {
    XpmImage image;
    XpmInfo info;
    char** data;

    XpmCreateXpmImageFromBuffer ((char*) dstring, &image, &info);

    for (i = 0; i < nsymbols; i++)
      {
	unsigned j;

	for (j = 0; j < image.ncolors; j++)
	  {
	    if (image.colorTable[j].symbolic != NULL &&
		!qxestrcasecmp_ascii(color_symbols[i].name, image.colorTable[j].symbolic))
	      {
		image.colorTable[j].c_color = xnew_ascbytes (16);

		sprintf(image.colorTable[j].c_color, "#%.4x%.4x%.4x",
			color_symbols[i].color.red, color_symbols[i].color.green,
			color_symbols[i].color.blue);
	      }
	  }
      }

    XpmCreateDataFromXpmImage (&data, &image, &info);

    pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)data);
  }

  if (color_symbols)
    xfree (color_symbols);

  if (!pixbuf)
    signal_image_error ("Error reading pixbuf", data);

  depth = gdk_pixbuf_get_bits_per_sample (pixbuf);

  IMAGE_INSTANCE_GTK_PIXMAP (ii) = pixbuf;
  IMAGE_INSTANCE_PIXMAP_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = gdk_pixbuf_get_width (pixbuf);
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = gdk_pixbuf_get_height (pixbuf);
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  switch (type)
    {
    case IMAGE_MONO_PIXMAP:
      break;

    case IMAGE_COLOR_PIXMAP:
      IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = depth;
      break;

    case IMAGE_POINTER:
      /* #### Gtk does not give us access to the hotspots of a pixmap */

      IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) = make_fixnum(1);
      IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) = make_fixnum(1);


      image_instance_convert_to_pointer (ii, instantiator, pointer_fg,
					 pointer_bg);
      break;

    default:
      ABORT ();
    }
  UNGCPRO;
}
#endif /* HAVE_XPM */


#ifdef HAVE_XFACE

/**********************************************************************
 *                             X-Face                                 *
 **********************************************************************/
#if defined(EXTERN)
/* This is about to get redefined! */
#undef EXTERN
#endif
/* We have to define SYSV32 so that compface.h includes string.h
   instead of strings.h. */
#define SYSV32
BEGIN_C_DECLS
#include <compface.h>
END_C_DECLS

/* JMP_BUF cannot be used here because if it doesn't get defined
   to jmp_buf we end up with a conflicting type error with the
   definition in compface.h */
extern jmp_buf comp_env;
#undef SYSV32

static void
gtk_xface_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		       Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		       int dest_mask, Lisp_Object UNUSED (domain))
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  int i, stattis;
  char *p, *bits, *bp;
  const char * volatile emsg = 0;
  char * volatile dstring;

  assert (!NILP (data));

  dstring = LISP_STRING_TO_EXTERNAL (data, Qbinary);

  if ((p = strchr (dstring, ':')))
    {
      dstring = p + 1;
    }

  /* Must use setjmp not SETJMP because we used jmp_buf above not JMP_BUF */
  if (!(stattis = setjmp (comp_env)))
    {
      UnCompAll ((char *) dstring);
      UnGenFace ();
    }

  switch (stattis)
    {
    case -2:
      emsg = "uncompface: internal error";
      break;
    case -1:
      emsg = "uncompface: insufficient or invalid data";
      break;
    case 1:
      emsg = "uncompface: excess data ignored";
      break;
    }

  if (emsg)
    gui_error_2 (emsg, data, Qimage);

  bp = bits = (char *) ALLOCA (PIXELS / 8);

  /* the compface library exports char F[], which uses a single byte per
     pixel to represent a 48x48 bitmap.  Yuck. */
  for (i = 0, p = F; i < (PIXELS / 8); ++i)
    {
      int n, b;
      /* reverse the bit order of each byte... */
      for (b = n = 0; b < 8; ++b)
	{
	  n |= ((*p++) << b);
	}
      *bp++ = (char) n;
    }

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, 48, 48, bits);
}

#endif /* HAVE_XFACE */

/**********************************************************************
 *                             RESOURCES                              *
 **********************************************************************/

static void
gtk_resource_validate (Lisp_Object instantiator)
{
  if ((NILP (find_keyword_in_vector (instantiator, Q_file))
       &&
       NILP (find_keyword_in_vector (instantiator, Q_resource_id)))
      ||
      NILP (find_keyword_in_vector (instantiator, Q_resource_type)))
    sferror ("Must supply :file, :resource-id and :resource-type",
			 instantiator);
}

static Lisp_Object
gtk_resource_normalize (Lisp_Object inst, Lisp_Object console_type,
			Lisp_Object dest_mask)
{
  return shared_resource_normalize (inst, console_type, dest_mask,
				    Qgtk_resource);
}

static int
gtk_resource_possible_dest_types (void)
{
  return IMAGE_POINTER_MASK | IMAGE_COLOR_PIXMAP_MASK;
}

extern gint lisp_to_gtk_enum (Lisp_Object);

static guint resource_name_to_resource (Lisp_Object UNUSED (name),
					enum image_instance_type type)
{
  if (type == IMAGE_POINTER)
    {
      // return (lisp_to_gtk_enum (name));
      return 1;
    }
  else
    return (0);
}

static enum image_instance_type
resource_symbol_to_type (Lisp_Object data)
{
  if (EQ (data, Qcursor))
    return IMAGE_POINTER;
#ifdef JSPARKES
  else if (EQ (data, Qicon))
    return IMAGE_ICON;
  else if (EQ (data, Qbitmap))
    return IMAGE_BITMAP;
#endif
  else
    return IMAGE_UNKNOWN;
}

static void
gtk_resource_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			  Lisp_Object UNUSED (pointer_fg),
			  Lisp_Object UNUSED (pointer_bg),
			  int UNUSED (dest_mask), Lisp_Object UNUSED (domain))
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  GdkCursor *c = NULL;
  enum image_instance_type type;
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object resource_type = find_keyword_in_vector (instantiator, Q_resource_type);
  Lisp_Object resource_id = find_keyword_in_vector (instantiator, Q_resource_id);

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a GTK device", device);

  type = resource_symbol_to_type (resource_type);

#if 0
  if (dest_mask & IMAGE_POINTER_MASK && type == IMAGE_POINTER_MASK)
    iitype = IMAGE_POINTER;
  else if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    iitype = IMAGE_COLOR_PIXMAP;
  else
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK | IMAGE_POINTER_MASK);
#endif

  /* mess with the keyword info we were provided with */
  gtk_initialize_pixmap_image_instance (ii, 1, type);
  c = gdk_cursor_new ((GdkCursorType) resource_name_to_resource (resource_id, type));
  IMAGE_INSTANCE_GTK_CURSOR (ii) = c;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) = resource_id;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = 10;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = 10;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = 1;
}

static void
check_valid_resource_symbol (Lisp_Object data)
{
  CHECK_SYMBOL (data);
  if (!resource_symbol_to_type (data))
    invalid_constant ("invalid resource type", data);
}

static void
check_valid_resource_id (Lisp_Object data)
{
  if (!resource_name_to_resource (data, IMAGE_POINTER)
      &&
      !resource_name_to_resource (data, IMAGE_COLOR_PIXMAP)
#if 0
      &&
      !resource_name_to_resource (data, IMAGE_BITMAP)
#endif
      )
    invalid_constant ("invalid resource identifier", data);
}

#if 0
void
check_valid_string_or_int (Lisp_Object data)
{
  if (!FIXNUMP (data))
    CHECK_STRING (data);
  else
    CHECK_FIXNUM (data);
}
#endif


/**********************************************************************
 *			 Autodetect		                         *
 **********************************************************************/

static void
autodetect_validate (Lisp_Object instantiator)
{
  data_must_be_present (instantiator);
}

static Lisp_Object
autodetect_normalize (Lisp_Object instantiator,
		      Lisp_Object console_type,
		      Lisp_Object UNUSED (dest_mask))
{
  Lisp_Object file = find_keyword_in_vector (instantiator, Q_data);
  Lisp_Object filename = Qnil;
  Lisp_Object data = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object alist = Qnil;

  GCPRO3 (filename, data, alist);

  if (NILP (file)) /* no conversion necessary */
    RETURN_UNGCPRO (instantiator);

  alist = tagged_vector_to_alist (instantiator);

  filename = gtk_locate_pixmap_file (file);
  if (!NILP (filename))
    {
      int xhot, yhot;
      /* #### Apparently some versions of XpmReadFileToData, which is
	 called by pixmap_to_lisp_data, don't return an error value
	 if the given file is not a valid XPM file.  Instead, they
	 just seg fault.  It is definitely caused by passing a
	 bitmap.  To try and avoid this we check for bitmaps first.  */

      data = bitmap_to_lisp_data (filename, &xhot, &yhot, 1);

      if (!EQ (data, Qt))
	{
	  alist = remassq_no_quit (Q_data, alist);
	  alist = Fcons (Fcons (Q_file, filename),
			 Fcons (Fcons (Q_data, data), alist));
	  if (xhot != -1)
	    alist = Fcons (Fcons (Q_hotspot_x, make_fixnum (xhot)),
			   alist);
	  if (yhot != -1)
	    alist = Fcons (Fcons (Q_hotspot_y, make_fixnum (yhot)),
			   alist);

	  alist = xbm_mask_file_munging (alist, filename, Qt, console_type);

	  {
	    Lisp_Object result = alist_to_tagged_vector (Qxbm, alist);
	    free_alist (alist);
	    RETURN_UNGCPRO (result);
	  }
	}

#ifdef HAVE_XPM
      data = pixmap_to_lisp_data (filename, 1);

      if (!EQ (data, Qt))
	{
	  alist = remassq_no_quit (Q_data, alist);
	  alist = Fcons (Fcons (Q_file, filename),
			 Fcons (Fcons (Q_data, data), alist));
	  alist = Fcons (Fcons (Q_color_symbols,
				evaluate_xpm_color_symbols ()),
			 alist);
	  {
	    Lisp_Object result = alist_to_tagged_vector (Qxpm, alist);
	    free_alist (alist);
	    RETURN_UNGCPRO (result);
	  }
	}
#endif
    }

  /* If we couldn't convert it, just put it back as it is.
     We might try to further frob it later as a cursor-font
     specification. (We can't do that now because we don't know
     what dest-types it's going to be instantiated into.) */
  {
    Lisp_Object result = alist_to_tagged_vector (Qautodetect, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

static int
autodetect_possible_dest_types (void)
{
  return
    IMAGE_MONO_PIXMAP_MASK  |
    IMAGE_COLOR_PIXMAP_MASK |
    IMAGE_POINTER_MASK      |
    IMAGE_TEXT_MASK;
}

static void
autodetect_instantiate (Lisp_Object image_instance,
				  Lisp_Object instantiator,
				  Lisp_Object pointer_fg,
				  Lisp_Object pointer_bg,
				  int dest_mask, Lisp_Object domain)
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object alist = Qnil;
  Lisp_Object result = Qnil;
  int is_cursor_font = 0;

  GCPRO3 (data, alist, result);

  alist = tagged_vector_to_alist (instantiator);
  if (dest_mask & IMAGE_POINTER_MASK)
    {
      const char *name_ext;

      TO_EXTERNAL_FORMAT (LISP_STRING, data,
			  C_STRING_ALLOCA, name_ext,
			  Qfile_name);

      if (cursor_name_to_index (name_ext) != -1)
	{
	  result = alist_to_tagged_vector (Qcursor_font, alist);
	  is_cursor_font = 1;
	}
    }

  if (!is_cursor_font)
    result = alist_to_tagged_vector (Qstring, alist);
  free_alist (alist);

  if (is_cursor_font)
    cursor_font_instantiate (image_instance, result, pointer_fg,
			     pointer_bg, dest_mask, domain);
  else
    string_instantiate (image_instance, result, pointer_fg,
			pointer_bg, dest_mask, domain);

  UNGCPRO;
}


/**********************************************************************
 *                              Font                                  *
 **********************************************************************/

#if 0
/* The font glyph instantiation does not work, and is incompatible with Gtk 3 */
static void
font_validate (Lisp_Object instantiator)
{
  data_must_be_present (instantiator);
}

static int
font_possible_dest_types (void)
{
  return IMAGE_POINTER_MASK;
}

static void
font_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object UNUSED (domain))
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkColor fg, bg;
  GdkFont *source, *mask;
  char source_name[PATH_MAX_INTERNAL], mask_name[PATH_MAX_INTERNAL], dummy;
  int source_char, mask_char;
  int count;
  Lisp_Object foreground, background;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  if (!STRINGP (data) ||
      strncmp ("FONT ", (char *) XSTRING_DATA (data), 5))
    invalid_argument ("Invalid font-glyph instantiator",
			 instantiator);

  if (!(dest_mask & IMAGE_POINTER_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_POINTER_MASK);

  foreground = find_keyword_in_vector (instantiator, Q_foreground);
  if (NILP (foreground))
    foreground = pointer_fg;
  background = find_keyword_in_vector (instantiator, Q_background);
  if (NILP (background))
    background = pointer_bg;

  generate_cursor_fg_bg (device, &foreground, &background, &fg, &bg);

  count = sscanf ((char *) XSTRING_DATA (data),
		  "FONT %s %d %s %d %c",
		  source_name, &source_char,
		  mask_name, &mask_char, &dummy);
  /* Allow "%s %d %d" as well... */
  if (count == 3 && (1 == sscanf (mask_name, "%d %c", &mask_char, &dummy)))
    count = 4, mask_name[0] = 0;

  if (count != 2 && count != 4)
    syntax_error ("invalid cursor specification", data);
  source = gdk_font_load (source_name);
  if (! source)
    gui_error_2 ("couldn't load font",
			   build_cistring (source_name),
			   data);
  if (count == 2)
    mask = 0;
  else if (!mask_name[0])
    mask = source;
  else
    {
      mask = gdk_font_load (mask_name);
      if (!mask)
	/* continuable */
	Fsignal (Qgui_error, list3 (build_msg_string ("couldn't load font"),
				    build_cistring (mask_name), data));
    }
  if (!mask)
    mask_char = 0;

  /* #### call XQueryTextExtents() and check_pointer_sizes() here. */

  gtk_initialize_pixmap_image_instance (ii, 1, IMAGE_POINTER);

  IMAGE_INSTANCE_GTK_CURSOR (ii) = NULL;

#if 0
  /* #### BILL!!! There is no way to call this function from Gdk */
    XCreateGlyphCursor (dpy, source, mask, source_char, mask_char,
			&fg, &bg);
#endif
  XIMAGE_INSTANCE_PIXMAP_FG (image_instance) = foreground;
  XIMAGE_INSTANCE_PIXMAP_BG (image_instance) = background;

  gdk_font_unref (source);
  if (mask && mask != source) gdk_font_unref (mask);
}
#endif


/**********************************************************************
 *                           Cursor-Font                              *
 **********************************************************************/

static void
cursor_font_validate (Lisp_Object instantiator)
{
  data_must_be_present (instantiator);
}

static int
cursor_font_possible_dest_types (void)
{
  return IMAGE_POINTER_MASK;
}

/* This is basically the equivalent of XmuCursorNameToIndex */
static GdkCursorType
cursor_name_to_index (const char *name)
{
  Lisp_Object sym;
  Lisp_Object val;
  
  if (name == 0 || name[0] == 0)
    /* wtaerror? */
    return (GdkCursorType) -1;

  sym = intern (name);

  if (NILP (Vgtk_cursor_names))
    invalid_state ("Gtk cursor names not registered", build_ascstring (name));

  val = Fassoc (Vgtk_cursor_names, sym);

  if (!NILP (val))
    return (GdkCursorType) XFIXNUM (Fcdr (val));

  return (GdkCursorType) -1;
}

static void
cursor_font_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			 int dest_mask, Lisp_Object UNUSED (domain))
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkCursorType i;
  const char *name_ext;
  Lisp_Object foreground, background;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  if (!(dest_mask & IMAGE_POINTER_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_POINTER_MASK);

  name_ext = LISP_STRING_TO_EXTERNAL (data, Qfile_name);

  if ((i = cursor_name_to_index (name_ext)) == -1)
    invalid_argument ("Unrecognized cursor-font name", data);

  gtk_initialize_pixmap_image_instance (ii, 1, IMAGE_POINTER);
  IMAGE_INSTANCE_GTK_CURSOR (ii) = gdk_cursor_new (i);
  foreground = find_keyword_in_vector (instantiator, Q_foreground);
  if (NILP (foreground))
    foreground = pointer_fg;
  background = find_keyword_in_vector (instantiator, Q_background);
  if (NILP (background))
    background = pointer_bg;
  maybe_recolor_cursor (image_instance, foreground, background);
}

static int
gtk_colorize_image_instance (Lisp_Object image_instance,
			       Lisp_Object foreground, Lisp_Object background);


/************************************************************************/
/*                      subwindow and widget support                      */
/************************************************************************/

/* unmap the image if it is a widget. This is used by redisplay via
   redisplay_unmap_subwindows */
static void
gtk_unmap_subwindow (Lisp_Image_Instance *p)
{
  if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
    {
      /* We don't support subwindows, but we do support widgets... */
      ABORT ();
    }
  else				/* must be a widget */
    {
      /* Since we are being unmapped we want the enclosing frame to
	 get focus. The losing with simple scrolling but is the safest
	 thing to do. */
      if (IMAGE_INSTANCE_GTK_CLIPWIDGET (p))
	gtk_widget_unmap (IMAGE_INSTANCE_GTK_CLIPWIDGET (p));
    }
}

/* map the subwindow. This is used by redisplay via
   redisplay_output_subwindow */
static void
gtk_map_subwindow (Lisp_Image_Instance *p, int x, int y,
		   struct display_glyph_area* dga)
{
  assert (dga->width > 0 && dga->height > 0);

  if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
    {
      /* No subwindow support... */
      ABORT ();
    }
  else				/* must be a widget */
    {
      struct frame *f = XFRAME (IMAGE_INSTANCE_FRAME (p));
      GtkWidget *wid = IMAGE_INSTANCE_GTK_CLIPWIDGET (p);
      GtkAllocation a, wa;
      int moving;

      if (!wid) return;

      a.x = x + IMAGE_INSTANCE_GTK_WIDGET_XOFFSET (p);
      a.y = y + IMAGE_INSTANCE_GTK_WIDGET_YOFFSET (p);
      a.width = dga->width;
      a.height = dga->height;

      gtk_widget_get_allocation (wid, &wa);
      /* Is the widget changing position? */
      moving = (a.x != wa.x) ||
	(a.y != wa.y);

      if ((a.width != wa.width) || (a.height != wa.height) || moving)
	{
	  gtk_widget_size_allocate (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), &a);
	}

      if (moving)
	{
	  /* GtkFixed widget queues a resize when you add a widget.
	  ** But only if it is visible.
	  ** losers.
          ** Check if still true for Gtk 2.0 - jsparkes
	  */
          gtk_widget_hide (FRAME_GTK_TEXT_WIDGET (f));

	  if (IMAGE_INSTANCE_GTK_ALREADY_PUT(p))
	    {
	      gtk_fixed_move (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
			      wid,
			      a.x, a.y);
	    }
	  else
	    {
	      IMAGE_INSTANCE_GTK_ALREADY_PUT(p) = TRUE;
	      gtk_fixed_put (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
			     wid,
			     a.x, a.y);
	    }

          gtk_widget_show (FRAME_GTK_TEXT_WIDGET (f));
	}
      else
	{
	  if (IMAGE_INSTANCE_GTK_ALREADY_PUT(p))
	    {
	      /* Do nothing... */
	    }
	  else
	    {
	      /* Must make sure we have put the image at least once! */
	      IMAGE_INSTANCE_GTK_ALREADY_PUT(p) = TRUE;
	      gtk_fixed_put (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
			     wid,
			     a.x, a.y);
	    }
	}

      if (!IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (p))
	{
	  gtk_widget_map (wid);
	}

      gtk_widget_queue_draw (wid);
    }
}

/* when you click on a widget you may activate another widget this
   needs to be checked and all appropriate widgets updated */
static void
gtk_redisplay_subwindow (Lisp_Image_Instance *p)
{
  /* Update the subwindow size if necessary. */
  if (IMAGE_INSTANCE_SIZE_CHANGED (p))
    {
#if 0
      XResizeWindow (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
		     IMAGE_INSTANCE_X_SUBWINDOW_ID (p),
		     IMAGE_INSTANCE_WIDTH (p),
		     IMAGE_INSTANCE_HEIGHT (p));
#endif
    }
}

/* Update all attributes that have changed. */
static void
gtk_redisplay_widget (Lisp_Image_Instance *p)
{
  /* This function can GC if IN_REDISPLAY is false. */

  if (!IMAGE_INSTANCE_GTK_CLIPWIDGET (p))
    return;

#ifdef HAVE_WIDGETS
  /* First get the items if they have changed since this is a
     structural change. As such it will nuke all added values so we
     need to update most other things after the items have changed.*/
  gtk_widget_show_all (IMAGE_INSTANCE_GTK_CLIPWIDGET (p));
  if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p))
    {
      /* Need to update GtkArgs that might have changed... */
      /* #### FIXME!!! */
      /*Lisp_Object image_instance = wrap_image_instance (p); */
    }
  else
    {
      /* #### FIXME!!! */
      /* No items changed, so do nothing, right? */
    }

  /* Possibly update the colors and font */
  if (IMAGE_INSTANCE_WIDGET_FACE_CHANGED (p)
      ||
      /* #### This is not sufficient because it will not cope with widgets
	 that are not currently visible. Once redisplay has done the
	 visible ones it will clear this flag so that when new ones
	 become visible they will not be updated. */
      XFRAME (IMAGE_INSTANCE_FRAME (p))->faces_changed
      ||
      XFRAME (IMAGE_INSTANCE_FRAME (p))->frame_changed
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p))
    {
      /* #### Write this function BILL! */
      update_widget_face (NULL, p, IMAGE_INSTANCE_FRAME (p));
    }

  /* Possibly update the text. */
  if (IMAGE_INSTANCE_TEXT_CHANGED (p))
    {
      char* str;
      Lisp_Object val = IMAGE_INSTANCE_WIDGET_TEXT (p);
      str = LISP_STRING_TO_EXTERNAL (val, Qnative);
      (void)str;
      /* #### Need to special case each type of GtkWidget here! */
    }

  /* Possibly update the size. */
  if (IMAGE_INSTANCE_SIZE_CHANGED (p)
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p)
      ||
      IMAGE_INSTANCE_TEXT_CHANGED (p))
    {
      GtkRequisition r;
      GtkAllocation a;

      assert (IMAGE_INSTANCE_GTK_WIDGET_ID (p) &&
	      IMAGE_INSTANCE_GTK_CLIPWIDGET (p)) ;

      gtk_widget_get_allocation (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), &a);

      a.width = r.width = IMAGE_INSTANCE_WIDTH (p);
      a.height = r.height = IMAGE_INSTANCE_HEIGHT (p);

      /* Force the widget's preferred and actual size to what we say it shall
	 be. */
#ifdef HAVE_GTK2
      gtk_widget_size_request (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), &r);
#endif
#ifdef HAVE_GTK3
      gtk_widget_get_preferred_size (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), NULL, &r);
#endif
      gtk_widget_size_allocate (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), &a);
    }

  /* Adjust offsets within the frame. */
  if (XFRAME (IMAGE_INSTANCE_FRAME (p))->size_changed)
    {
      /* I don't think we need to do anything for Gtk here... */
    }

  /* now modify the widget */
#endif
}

/* instantiate and gtk type subwindow */
static void
gtk_subwindow_instantiate (Lisp_Object image_instance,
			   Lisp_Object UNUSED (instantiator),
			   Lisp_Object UNUSED (pointer_fg),
			   Lisp_Object UNUSED (pointer_bg),
			   int UNUSED (dest_mask), Lisp_Object UNUSED (domain))
{
  /* This function can GC */
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a GTK device", device);

  IMAGE_INSTANCE_TYPE (ii) = IMAGE_SUBWINDOW;

  ii->data = xnew_and_zero (struct gtk_subwindow_data);

  /* Create a window for clipping */
  IMAGE_INSTANCE_GTK_CLIPWINDOW (ii) = NULL;

  /* Now put the subwindow inside the clip window. */
  IMAGE_INSTANCE_SUBWINDOW_ID (ii) = (void *) NULL;
}

#ifdef HAVE_WIDGETS

/************************************************************************/
/*                            widgets                            */
/************************************************************************/
static void
update_widget_face (GtkWidget *UNUSED (w), Lisp_Image_Instance *UNUSED (ii),
		    Lisp_Object UNUSED (domain))
{
  if (0)
    {
#if 0
      GtkStyle *style = gtk_widget_get_style (w);
      Lisp_Object pixel = Qnil;
      GdkColor *fcolor, *bcolor;

      style = gtk_style_copy (style);

      /* Update the foreground. */
      pixel = FACE_FOREGROUND (IMAGE_INSTANCE_WIDGET_FACE (ii), domain);
      fcolor = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (pixel));

      /* Update the background. */
      pixel = FACE_BACKGROUND (IMAGE_INSTANCE_WIDGET_FACE (ii), domain);
      bcolor = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (pixel));

      /* Update the font */
      /* #### FIXME!!! Need to copy the widgets style, dick with it, and
      ** set the widgets style to the new style...
      */
      gtk_widget_set_style (w, style);
#endif

      /* #### Megahack - but its just getting too complicated to do this
	 in the right place. */
#if 0
      if (EQ (IMAGE_INSTANCE_WIDGET_TYPE (ii), Qtab_control))
	update_tab_widget_face (wv, ii, domain);
#endif
    }
}

#if 0
static void
update_tab_widget_face (GtkWidget *w, Lisp_Image_Instance *ii,
			Lisp_Object domain)
{
  if (wv->contents)
    {
      widget_value* val = wv->contents, *cur;

      /* Give each child label the correct foreground color. */
      Lisp_Object pixel = FACE_FOREGROUND
	(IMAGE_INSTANCE_WIDGET_FACE (ii),
	 domain);
      XColor fcolor = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (pixel));
      lw_add_widget_value_arg (val, XtNtabForeground, fcolor.pixel);
      wv->change = VISIBLE_CHANGE;
      val->change = VISIBLE_CHANGE;

      for (cur = val->next; cur; cur = cur->next)
	{
	  cur->change = VISIBLE_CHANGE;
	  if (cur->value)
	    {
	      lw_copy_widget_value_args (val, cur);
	    }
	}
    }
}
#endif

static Lisp_Object
gtk_widget_instantiate_1 (Lisp_Object image_instance, Lisp_Object instantiator,
			  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			  Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object widget = Qnil;
  /* char *nm = NULL; */
  GtkWidget *w = NULL;
  struct gcpro gcpro1;

  IMAGE_INSTANCE_TYPE (ii) = IMAGE_WIDGET;

  if (!NILP (IMAGE_INSTANCE_WIDGET_TEXT (ii)))
    {
      /* nm = LISP_STRING_TO_EXTERNAL (IMAGE_INSTANCE_WIDGET_TEXT (ii), Qnative); */
    }

  ii->data = xnew_and_zero (struct gtk_subwindow_data);

  /* Create a clipping widget */
  IMAGE_INSTANCE_GTK_CLIPWIDGET (ii) = NULL;
  IMAGE_INSTANCE_GTK_ALREADY_PUT(ii) = FALSE;

  /* Create the actual widget */
  GCPRO1 (widget);
  widget = call5 (Qgtk_widget_instantiate_internal,
		  image_instance, instantiator,
		  pointer_fg, pointer_bg,
		  domain);

  if (!NILP (widget))
    {
      CHECK_GTK_OBJECT (widget);
      w = GTK_WIDGET (XGTK_OBJECT (widget)->object);
    }
  else
    {
      stderr_out ("Lisp-level creation of widget failed... falling back\n");
      w = gtk_label_new ("Widget Creation Failed...");
    }

  UNGCPRO;

  IMAGE_INSTANCE_SUBWINDOW_ID (ii) = (void *) w;

  /* #### HACK!!!!  We should make this do the right thing if we
  ** really need a clip widget!
  */
  IMAGE_INSTANCE_GTK_CLIPWIDGET (ii) = w;

  /* The current theme may produce a widget of a different size that what we
     expect so force reconsideration of the widget's size. */
  IMAGE_INSTANCE_LAYOUT_CHANGED (ii) = 1;

  return (Qt);
}

static void
gtk_widget_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			int UNUSED (dest_mask), Lisp_Object domain)
{
  call_with_suspended_errors ((lisp_fn_t) gtk_widget_instantiate_1,
			      Qnil, Qimage,
			      ERROR_ME_WARN, 5,
			      image_instance, instantiator,
			      pointer_fg,
			      pointer_bg,
			      domain);
}

/* get properties of a control */
static Lisp_Object
gtk_widget_property (Lisp_Object UNUSED (image_instance), Lisp_Object prop)
{
  /* Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance); */

  /* get the text from a control */
  if (EQ (prop, Q_text))
    {
      return Qnil;
    }
  return Qunbound;
}

#define FAKE_GTK_WIDGET_INSTANTIATOR(x)					\
static void								\
gtk_##x##_instantiate (Lisp_Object image_instance,			\
   Lisp_Object instantiator,						\
   Lisp_Object pointer_fg,						\
   Lisp_Object pointer_bg,						\
   int dest_mask, Lisp_Object domain)					\
{									\
  gtk_widget_instantiate (image_instance, instantiator, pointer_fg,	\
			  pointer_bg, dest_mask, domain);		\
}

FAKE_GTK_WIDGET_INSTANTIATOR(native_layout);
FAKE_GTK_WIDGET_INSTANTIATOR(button);
FAKE_GTK_WIDGET_INSTANTIATOR(progress_gauge);
FAKE_GTK_WIDGET_INSTANTIATOR(edit_field);
FAKE_GTK_WIDGET_INSTANTIATOR(combo_box);
FAKE_GTK_WIDGET_INSTANTIATOR(label);
/* Note: tab_control has a custom instantiator (see below) */

/*
  Ask the widget to return it's preferred size.  This device method must
  defined for all widgets that also have format specific version of
  query_geometry defined in glyphs-widget.c.  This is because those format
  specific versions return sizes that are appropriate for the X widgets.  For
  GTK, the size of a widget can change at runtime due to the user changing
  their theme.

  This method can be called before the widget is instantiated.  This is
  because instantiate_image_instantiator() is tying to be helpful to other
  toolkits and supply sane geometry values to them.  This is not appropriate
  for GTK and can be ignored.

  This method can be used by all widgets.
*/
static void
gtk_widget_query_geometry (Lisp_Object image_instance,
			   int* width, int* height,
			   enum image_instance_geometry UNUSED (disp),
			   Lisp_Object UNUSED (domain))
{
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);

  if (p->data != NULL)
    {
      GtkWidget *w = IMAGE_INSTANCE_GTK_CLIPWIDGET (p);

#ifdef HAVE_GTK2
      GtkRequisition r;

      gtk_widget_size_request(w, &r);
      *height= r.height;
      *width = r.width;
#endif
#ifdef HAVE_GTK3
      *height = gtk_widget_get_allocated_height (w);
      *width  = gtk_widget_get_allocated_width (w);
#endif
    }
}


/* Button functions. */

/* Update a button's clicked state. */
static void
gtk_button_redisplay (Lisp_Object image_instance)
{
  /* This function can GC if IN_REDISPLAY is false. */
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  GtkWidget *w = IMAGE_INSTANCE_GTK_CLIPWIDGET (p);

  if (G_OBJECT_TYPE (w) == gtk_button_get_type ())
    {
    }
  else if (G_OBJECT_TYPE (w) == gtk_check_button_get_type ())
    {
    }
  else if (G_OBJECT_TYPE (w) == gtk_radio_button_get_type ())
    {
    }
  else
    {
      /* Unknown button type... */
      ABORT();
    }
}

/* get properties of a button */
static Lisp_Object
gtk_button_property (Lisp_Object image_instance, Lisp_Object prop)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);

  /* check the state of a button */
  if (EQ (prop, Q_selected))
    {
      if (gtk_widget_has_focus (GTK_WIDGET (IMAGE_INSTANCE_SUBWINDOW_ID (ii))))
	return Qt;
      else
	return Qnil;
    }
  return Qunbound;
}


/* Progress gauge functions. */

/* set the properties of a progress gauge */
static void
gtk_progress_gauge_redisplay (Lisp_Object image_instance)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);

  if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii))
    {
      gfloat f;
      Lisp_Object val;

      val = XGUI_ITEM (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii))->value;
      f = XFLOATFIXNUM (val);

      gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (IMAGE_INSTANCE_SUBWINDOW_ID (ii)),
                                     f / 100.0);
    }
}


/* Tab Control functions. */

/*
  Register a widget's callbacks with the frame's hashtable.  The hashtable is
  weak so deregistration is handled automatically.  Tab controls have per-tab
  callback list functions and the GTK callback architecture is not
  sufficiently flexible to deal with this.  Instead, the functions are
  registered here and the id is passed through the callback loop.
 */
static int
gtk_register_gui_item (Lisp_Object image_instance, Lisp_Object gui,
		       Lisp_Object domain)
{
  struct frame *f = XFRAME(DOMAIN_FRAME(domain));
  int id = gui_item_id_hash(FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f),
			    gui, WIDGET_GLYPH_SLOT);

  Fputhash(make_fixnum(id), image_instance,
	   FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE (f));
  Fputhash(make_fixnum(id), XGUI_ITEM (gui)->callback,
	   FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE (f));
  Fputhash(make_fixnum(id), XGUI_ITEM (gui)->callback_ex,
	   FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE (f));
  return id;
}

/*
  Append the given item as a tab to the notebook. Callbacks, etc are all
  setup.
 */
static void
gtk_add_tab_item(Lisp_Object image_instance,
		 GtkNotebook* nb, Lisp_Object item,
		 Lisp_Object domain, int UNUSED (i))
{
  Lisp_Object name;
  int hash_id = 0;
  char *c_name = NULL;
  GtkWidget* box;

  if (GUI_ITEMP (item))
    {
      Lisp_Gui_Item *pgui = XGUI_ITEM (item);

      if (!STRINGP (pgui->name))
	pgui->name = eval_within_redisplay (pgui->name);

      if (!STRINGP (pgui->name)) {
	warn_when_safe (Qredisplay, Qwarning,
			"Name does not evaluate to string");

	return;
      }

      hash_id = gtk_register_gui_item (image_instance, item, domain);
      name = pgui->name;
    }
  else
    {
      CHECK_STRING (item);
      name = item;
    }

  c_name = LISP_STRING_TO_EXTERNAL (name, Qutf_8);

  /* Dummy widget that the notbook wants to display when a tab is selected. */
#ifdef HAVE_GTK2
  box = gtk_vbox_new (FALSE, 3);
#endif
#ifdef HAVE_GTK3
  box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
#endif

  /*
    Store the per-tab callback data id in the tab.  The callback functions
    themselves could have been stored in the widget but this avoids having to
    worry about the garbage collector running between here and the callback
    function.
  */
  g_object_set_qdata(G_OBJECT(box), GTK_DATA_TAB_HASHCODE_IDENTIFIER,
                     GUINT_TO_POINTER (hash_id));

  gtk_notebook_append_page (nb, box, gtk_label_new (c_name));
}

/* Signal handler for the switch-page signal. */
static void gtk_tab_control_callback(GtkNotebook *notebook,
				     gpointer *page,
				     gint UNUSED (page_num),
				     gpointer UNUSED (user_data))
{
  /*
    This callback is called for every selection, not just user selection.
    We're only interested in user selection, which occurs outside of
    redisplay.
  */

  if (!in_display)
    {
      Lisp_Object image_instance, callback, callback_ex;
      Lisp_Object frame, event;
      int update_subwindows_p = 0;
      struct frame *f = gtk_widget_to_frame(GTK_WIDGET(notebook));
      int id;

      if (!f)
	return;
      frame = wrap_frame (f);

      id             = GPOINTER_TO_UINT (g_object_get_qdata(G_OBJECT(page),
                                                            GTK_DATA_TAB_HASHCODE_IDENTIFIER));
      image_instance = Fgethash(make_fixnum_verify(id),
				FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE(f), Qnil);
      callback       = Fgethash(make_fixnum(id),
				FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f), Qnil);
      callback_ex    = Fgethash(make_fixnum(id),
				FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE(f), Qnil);
      update_subwindows_p = 1;

      /* It is possible for a widget action to cause it to get out of
	 sync with its instantiator. Thus it is necessary to signal
	 this possibility. */
      if (IMAGE_INSTANCEP (image_instance))
	XIMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (image_instance) = 1;

      if (!NILP (callback_ex) && !UNBOUNDP (callback_ex))
	{
	  event = Fmake_event (Qnil, Qnil);

	  XSET_EVENT_TYPE (event, misc_user_event);
	  XSET_EVENT_CHANNEL (event, frame);
	  XSET_EVENT_MISC_USER_FUNCTION (event, Qeval);
	  XSET_EVENT_MISC_USER_OBJECT (event, list4 (Qfuncall, callback_ex, image_instance, event));
	}
      else if (NILP (callback) || UNBOUNDP (callback))
	event = Qnil;
      else
	{
	  Lisp_Object fn, arg;

	  event = Fmake_event (Qnil, Qnil);

	  get_gui_callback (callback, &fn, &arg);
	  XSET_EVENT_TYPE (event, misc_user_event);
	  XSET_EVENT_CHANNEL (event, frame);
	  XSET_EVENT_MISC_USER_FUNCTION (event, fn);
	  XSET_EVENT_MISC_USER_OBJECT (event, arg);
	}

      if (!NILP (event))
	enqueue_dispatch_event (event);

      /* The result of this evaluation could cause other instances to change so
	 enqueue an update callback to check this. */
      if (update_subwindows_p && !NILP (event))
	enqueue_magic_eval_event (update_widget_instances, frame);
    }
}

/* Create a tab_control widget.  The special handling of the individual tabs
   means that the normal instantiation code cannot be used. */
static void
gtk_tab_control_instantiate (Lisp_Object image_instance,
			     Lisp_Object instantiator,
			     Lisp_Object pointer_fg,
			     Lisp_Object pointer_bg,
			     int dest_mask, Lisp_Object domain)
{
  Lisp_Object rest;
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  int i = 0;
  int selected = 0;
  GtkNotebook *nb;

  /* The normal instantiation is still needed. */
  gtk_widget_instantiate (image_instance, instantiator, pointer_fg,
			  pointer_bg, dest_mask, domain);

  nb = GTK_NOTEBOOK (IMAGE_INSTANCE_GTK_CLIPWIDGET (ii));

  /* Add items to the tab, find the current selection */
  LIST_LOOP (rest, XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)))
    {
      gtk_add_tab_item (image_instance, nb, XCAR (rest), domain, i);

      if (gui_item_selected_p (XCAR (rest)))
	selected = i;

      i++;
    }

  gtk_notebook_set_current_page(nb, selected);

  /* Call per-tab lisp callback when a tab is pressed. */
  assert (g_signal_connect (G_OBJECT (nb), "switch-page",
                            G_CALLBACK (gtk_tab_control_callback), NULL));
}

/* Set the properties of a tab control */
static void
gtk_tab_control_redisplay (Lisp_Object image_instance)
{
  /* #### Convert this to GTK baby! */
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);

  if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii) ||
      IMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (ii))
    {
      /* If only the order has changed then simply select the first
	 one of the pending set. This stops horrendous rebuilding -
	 and hence flicker - of the tabs each time you click on
	 one. */
      if (tab_control_order_only_changed (image_instance))
	{
	  int i = 0;
	  Lisp_Object rest, selected =
	    gui_item_list_find_selected
	    (NILP (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii)) ?
	     XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)) :
	     XCDR (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii)));

	  LIST_LOOP (rest, XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)))
	    {
	      if (gui_item_equal_sans_selected (XCAR (rest), selected, 0))
		{
		  Lisp_Object old_selected =gui_item_list_find_selected
		    (XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)));

		  /* Pick up the new selected item. */
		  XGUI_ITEM (old_selected)->selected =
		    XGUI_ITEM (XCAR (rest))->selected;
		  XGUI_ITEM (XCAR (rest))->selected =
		    XGUI_ITEM (selected)->selected;
		  /* We're not actually changing the items anymore. */
		  IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii) = 0;
		  IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii) = Qnil;

		  gtk_notebook_set_current_page
                    (GTK_NOTEBOOK (IMAGE_INSTANCE_GTK_CLIPWIDGET (ii)),
                     i);
		  break;
		}

	      i++;
	    }
	}
      else
	{
	  /* More than just the order has changed... let's get busy! */
	  GtkNotebook *nb = GTK_NOTEBOOK (IMAGE_INSTANCE_GTK_CLIPWIDGET (ii));
	  GList *children = gtk_container_get_children (GTK_CONTAINER (nb));
	  Lisp_Object rest;

	  /* Why is there no API to remove everything from a notebook? */
	  while (children)
	    {
	      gtk_widget_destroy (GTK_WIDGET (children->data));
	      children = children->next;
	    }

	  LIST_LOOP (rest, XCDR (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii)))
	    {
	      gtk_add_tab_item(image_instance, nb, XCAR(rest),
			       IMAGE_INSTANCE_FRAME(ii), 0);
	    }

	  /* Show all the new widgets we just added... */
	  gtk_widget_show_all (GTK_WIDGET (nb));
	}
    }

  /* Possibly update the face. */
#if 0
  if (IMAGE_INSTANCE_WIDGET_FACE_CHANGED (ii)
      ||
      XFRAME (IMAGE_INSTANCE_FRAME (ii))->faces_changed
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii))
    {
      update_tab_widget_face (wv, ii,
			      IMAGE_INSTANCE_FRAME (ii));
    }
#endif
}
#endif /* HAVE_WIDGETS */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/
void
syms_of_glyphs_gtk (void)
{
#ifdef HAVE_WIDGETS
  DEFSYMBOL (Qgtk_widget_instantiate_internal);
  DEFSYMBOL (Qgtk_widget_property_internal);
  DEFSYMBOL (Qgtk_widget_redisplay_internal);
  DEFSYMBOL (Qgtk_widget_set_style);
#endif
}

void
console_type_create_glyphs_gtk (void)
{
  /* image methods */
  CONSOLE_HAS_METHOD (gtk, print_image_instance);
  CONSOLE_HAS_METHOD (gtk, finalize_image_instance);
  CONSOLE_HAS_METHOD (gtk, image_instance_equal);
#if 0
  CONSOLE_HAS_METHOD (gtk, image_instance_hash);
#endif
  CONSOLE_HAS_METHOD (gtk, colorize_image_instance);
  CONSOLE_HAS_METHOD (gtk, init_image_instance_from_eimage);
  CONSOLE_HAS_METHOD (gtk, locate_pixmap_file);
  CONSOLE_HAS_METHOD (gtk, unmap_subwindow);
  CONSOLE_HAS_METHOD (gtk, map_subwindow);
  CONSOLE_HAS_METHOD (gtk, redisplay_widget);
  CONSOLE_HAS_METHOD (gtk, redisplay_subwindow);
}

void
image_instantiator_format_create_glyphs_gtk (void)
{
  IIFORMAT_VALID_CONSOLE (gtk, nothing);
  IIFORMAT_VALID_CONSOLE (gtk, string);
#ifdef HAVE_WIDGETS
  IIFORMAT_VALID_CONSOLE (gtk, layout);
#endif
  IIFORMAT_VALID_CONSOLE (gtk, formatted_string);
  IIFORMAT_VALID_CONSOLE (gtk, inherit);
#ifdef HAVE_XPM
  INITIALIZE_DEVICE_IIFORMAT (gtk, xpm);
  IIFORMAT_HAS_DEVMETHOD (gtk, xpm, instantiate);
#endif
#ifdef HAVE_JPEG
  IIFORMAT_VALID_CONSOLE (gtk, jpeg);
#endif
#ifdef HAVE_TIFF
  IIFORMAT_VALID_CONSOLE (gtk, tiff);
#endif
#ifdef HAVE_PNG
  IIFORMAT_VALID_CONSOLE (gtk, png);
#endif
#ifdef HAVE_GIF
  IIFORMAT_VALID_CONSOLE (gtk, gif);
#endif

  INITIALIZE_DEVICE_IIFORMAT (gtk, subwindow);
  IIFORMAT_HAS_DEVMETHOD (gtk, subwindow, instantiate);

#ifdef HAVE_WIDGETS
  /* layout widget */
  INITIALIZE_DEVICE_IIFORMAT (gtk, native_layout);
  IIFORMAT_HAS_DEVMETHOD (gtk, native_layout, instantiate);

  /* button widget */
  INITIALIZE_DEVICE_IIFORMAT (gtk, button);
  IIFORMAT_HAS_DEVMETHOD (gtk, button, property);
  IIFORMAT_HAS_DEVMETHOD (gtk, button, instantiate);
  IIFORMAT_HAS_DEVMETHOD (gtk, button, redisplay);
  IIFORMAT_HAS_SHARED_DEVMETHOD (gtk, button, query_geometry, widget);
  /* general widget methods. */
  INITIALIZE_DEVICE_IIFORMAT (gtk, widget);
  IIFORMAT_HAS_DEVMETHOD (gtk, widget, property);
  IIFORMAT_HAS_DEVMETHOD (gtk, widget, query_geometry);

  /* progress gauge */
  INITIALIZE_DEVICE_IIFORMAT (gtk, progress_gauge);
  IIFORMAT_HAS_DEVMETHOD (gtk, progress_gauge, redisplay);
  IIFORMAT_HAS_DEVMETHOD (gtk, progress_gauge, instantiate);
  IIFORMAT_HAS_SHARED_DEVMETHOD (gtk, progress_gauge, query_geometry, widget);
  /* text field */
  INITIALIZE_DEVICE_IIFORMAT (gtk, edit_field);
  IIFORMAT_HAS_DEVMETHOD (gtk, edit_field, instantiate);
  INITIALIZE_DEVICE_IIFORMAT (gtk, combo_box);
  IIFORMAT_HAS_DEVMETHOD (gtk, combo_box, instantiate);
  IIFORMAT_HAS_SHARED_DEVMETHOD (gtk, combo_box, redisplay, tab_control);
  /* tab control widget */
  INITIALIZE_DEVICE_IIFORMAT (gtk, tab_control);
  IIFORMAT_HAS_DEVMETHOD (gtk, tab_control, instantiate);
  IIFORMAT_HAS_DEVMETHOD (gtk, tab_control, redisplay);
  IIFORMAT_HAS_SHARED_DEVMETHOD (gtk, tab_control, query_geometry, widget);
  /* label */
  INITIALIZE_DEVICE_IIFORMAT (gtk, label);
  IIFORMAT_HAS_DEVMETHOD (gtk, label, instantiate);
#endif

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (cursor_font, "cursor-font");
  IIFORMAT_VALID_CONSOLE (gtk, cursor_font);

  IIFORMAT_HAS_METHOD (cursor_font, validate);
  IIFORMAT_HAS_METHOD (cursor_font, possible_dest_types);
  IIFORMAT_HAS_METHOD (cursor_font, instantiate);

  IIFORMAT_VALID_KEYWORD (cursor_font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (cursor_font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (cursor_font, Q_background, check_valid_string);

#if 0
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (font, "font");
  IIFORMAT_VALID_CONSOLE (gtk, font);

  IIFORMAT_HAS_METHOD (font, validate);
  IIFORMAT_HAS_METHOD (font, possible_dest_types);
  IIFORMAT_HAS_METHOD (font, instantiate);

  IIFORMAT_VALID_KEYWORD (font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_background, check_valid_string);
#endif

#ifdef HAVE_XPM
  INITIALIZE_DEVICE_IIFORMAT (gtk, xpm);
  IIFORMAT_HAS_DEVMETHOD (gtk, xpm, instantiate);
#endif

#ifdef HAVE_XFACE
  INITIALIZE_DEVICE_IIFORMAT (gtk, xface);
  IIFORMAT_HAS_DEVMETHOD (gtk, xface, instantiate);
#endif

  INITIALIZE_DEVICE_IIFORMAT (gtk, xbm);
  IIFORMAT_HAS_DEVMETHOD (gtk, xbm, instantiate);
  IIFORMAT_VALID_CONSOLE (gtk, xbm);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (gtk_resource, "gtk-resource");
  IIFORMAT_VALID_CONSOLE (gtk, gtk_resource);

  IIFORMAT_HAS_METHOD (gtk_resource, validate);
  IIFORMAT_HAS_METHOD (gtk_resource, normalize);
  IIFORMAT_HAS_METHOD (gtk_resource, possible_dest_types);
  IIFORMAT_HAS_METHOD (gtk_resource, instantiate);

  IIFORMAT_VALID_KEYWORD (gtk_resource, Q_resource_type, check_valid_resource_symbol);
  IIFORMAT_VALID_KEYWORD (gtk_resource, Q_resource_id, check_valid_resource_id);
  IIFORMAT_VALID_KEYWORD (gtk_resource, Q_file, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (autodetect, "autodetect");
  IIFORMAT_VALID_CONSOLE (gtk, autodetect);

  IIFORMAT_HAS_METHOD (autodetect, validate);
  IIFORMAT_HAS_METHOD (autodetect, normalize);
  IIFORMAT_HAS_METHOD (autodetect, possible_dest_types);
  IIFORMAT_HAS_METHOD (autodetect, instantiate);

  IIFORMAT_VALID_KEYWORD (autodetect, Q_data, check_valid_string);
}

void
vars_of_glyphs_gtk (void)
{
#ifdef HAVE_XFACE
  Fprovide (Qxface);
#endif

  DEFVAR_LISP ("gtk-bitmap-file-path", &Vgtk_bitmap_file_path /*
A list of the directories in which X bitmap files may be found.
If nil, this is initialized from the "*bitmapFilePath" resource.
This is used by the `make-image-instance' function (however, note that if
the environment variable XBMLANGPATH is set, it is consulted first).
*/ );
  Vgtk_bitmap_file_path = Qnil;

#ifdef HAVE_GTK
  DEFVAR_LISP ("gtk-cursor-names", &Vgtk_cursor_names /*
An alist of Gtk cursor names and internal integer values.
*/);
  Vgtk_cursor_names = Qnil;
#endif
}

void
complex_vars_of_glyphs_gtk (void)
{
#define BUILD_GLYPH_INST(variable, name)			\
  Fadd_spec_to_specifier					\
    (GLYPH_IMAGE (XGLYPH (variable)),				\
     vector3 (Qxbm, Q_data,					\
	      list3 (make_fixnum (name##_width),			\
		     make_fixnum (name##_height),			\
		     make_extstring ((Extbyte*) name##_bits,	\
				      sizeof (name##_bits),	\
				      Qbinary))),		\
     Qglobal, Qgtk, Qnil)

  BUILD_GLYPH_INST (Vtruncation_glyph, truncator);
  BUILD_GLYPH_INST (Vcontinuation_glyph, continuer);
  BUILD_GLYPH_INST (Vxemacs_logo, xemacs);
  BUILD_GLYPH_INST (Vhscroll_glyph, hscroll);

#undef BUILD_GLYPH_INST
  register_cursor_names ();
}

static int
gtk_colorize_image_instance (Lisp_Object image_instance,
			     Lisp_Object foreground, Lisp_Object background)
{
  struct Lisp_Image_Instance *p;

  p = XIMAGE_INSTANCE (image_instance);

  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
      IMAGE_INSTANCE_TYPE (p) = IMAGE_COLOR_PIXMAP;
      /* Make sure there aren't two pointers to the same mask, causing
	 it to get freed twice. */
      IMAGE_INSTANCE_PIXMAP_MASK (p) = 0;
      break;

    default:
      return 0;
    }

  {
    GdkPixbuf *pb = IMAGE_INSTANCE_GTK_PIXMAP (p);
    gint rowstride = gdk_pixbuf_get_rowstride (pb);
    gint n_channels = gdk_pixbuf_get_n_channels (pb);
    guchar *data = gdk_pixbuf_get_pixels (pb);
    guchar *dp = data;
    GdkColor *color;
    gint width = gdk_pixbuf_get_width (pb);
    gint height = gdk_pixbuf_get_height (pb);
    int i,j;

    for (i = 0; i < height; i++)
      {
	for (j = 0; j < width; j++)
	  {
	    dp = data + i * rowstride + j * n_channels;
	    if (*dp == 0)
	      color = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (foreground));
	    else
	      color = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (background));
	    *dp++ = color->red;
	    *dp++ = color->green;
	    *dp++ = color->blue;
	    if (n_channels == 4)
	      *dp++ = 0xff;
	  }
      }
    IMAGE_INSTANCE_PIXMAP_FG (p) = foreground;
    IMAGE_INSTANCE_PIXMAP_BG (p) = background;
    return 1;
  }
}
