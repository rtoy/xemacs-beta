/* GTK-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996, 2001, 2002 Ben Wing
   Copyright (C) 1995 Sun Microsystems

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

#include "console-gtk-impl.h"
#include "glyphs-gtk.h"
#include "objects-gtk-impl.h"
#include "ui-gtk.h"

#include "sysfile.h"

#include <setjmp.h>

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
Lisp_Object Qxface;
#endif

#ifdef HAVE_XPM
DEFINE_DEVICE_IIFORMAT (gtk, xpm);
#endif

DEFINE_DEVICE_IIFORMAT (gtk, xbm);
DEFINE_DEVICE_IIFORMAT (gtk, subwindow);

DEFINE_IMAGE_INSTANTIATOR_FORMAT (cursor_font);
Lisp_Object Qcursor_font;

DEFINE_IMAGE_INSTANTIATOR_FORMAT (font);

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

static gint cursor_name_to_index (const char *name);

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


/************************************************************************/
/*                      image instance methods                          */
/************************************************************************/

/************************************************************************/
/* convert from a series of RGB triples to an XImage formated for the   */
/* proper display 							*/
/************************************************************************/
static GdkImage *
convert_EImage_to_GDKImage (Lisp_Object device, int width, int height,
			    unsigned char *pic, unsigned long **pixtbl,
			    int *npixels)
{
  GdkColormap *cmap;
  GdkVisual *vis;
  GdkImage *outimg;
  int depth, byte_cnt, i, j;
  int rd,gr,bl,q;
  unsigned char *data, *ip, *dp = NULL;
  quant_table *qtable = NULL;
  union {
    UINT_32_BIT val;
    char cp[4];
  } conv;

  cmap = DEVICE_GTK_COLORMAP (XDEVICE(device));
  vis = DEVICE_GTK_VISUAL (XDEVICE(device));
  depth = DEVICE_GTK_DEPTH(XDEVICE(device));

  if (vis->type == GDK_VISUAL_GRAYSCALE || vis->type == GDK_VISUAL_STATIC_COLOR ||
      vis->type == GDK_VISUAL_STATIC_GRAY)
    {
      /* #### Implement me!!! */
      return NULL;
    }

  if (vis->type == GDK_VISUAL_PSEUDO_COLOR)
    {
      /* Quantize the image and get a histogram while we're at it.
	 Do this first to save memory */
      qtable = build_EImage_quantable(pic, width, height, 256);
      if (qtable == NULL) return NULL;
    }

  /* The first parameter (GdkWindow *) is allowed to be NULL if we
  ** specify the depth */
  outimg = gdk_image_new (GDK_IMAGE_FASTEST, vis, width, height);

  if (!outimg) return NULL;

  byte_cnt = outimg->bpp;

  data = (unsigned char *) outimg->mem;

  if (!data)
    {
      gdk_image_destroy (outimg);
      return NULL;
    }
  
  if (vis->type == GDK_VISUAL_PSEUDO_COLOR)
    {
      unsigned long pixarray[256];
      int pixcount, n;
      /* use our quantize table to allocate the colors */
      pixcount = 32;
      *pixtbl = xnew_array (unsigned long, pixcount);
      *npixels = 0;

      /* ### should implement a sort by popularity to assure proper allocation */
      n = *npixels;
      for (i = 0; i < qtable->num_active_colors; i++)
	{
	  GdkColor color;
	  int res;
	
	  color.red = qtable->rm[i] ? qtable->rm[i] << 8 : 0;
	  color.green = qtable->gm[i] ? qtable->gm[i] << 8 : 0;
	  color.blue = qtable->bm[i] ? qtable->bm[i] << 8 : 0;
	  res = allocate_nearest_color (cmap, vis, &color);
	  if (res > 0 && res < 3)
	    {
	      DO_REALLOC(*pixtbl, pixcount, n+1, unsigned long);
	      (*pixtbl)[n] = color.pixel;
	      n++;
	    }
	  pixarray[i] = color.pixel;
	}
      *npixels = n;
      ip = pic;
      for (i = 0; i < height; i++)
	{
	  dp = data + (i * outimg->bpl);
	  for (j = 0; j < width; j++)
	    {
	      rd = *ip++;
	      gr = *ip++;
	      bl = *ip++;
	      conv.val = pixarray[QUANT_GET_COLOR(qtable,rd,gr,bl)];
#if WORDS_BIGENDIAN
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
	    }
	}
      xfree(qtable, quant_table *);
    } else {
      unsigned long rshift,gshift,bshift,rbits,gbits,bbits,junk;
      junk = vis->red_mask;
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
      junk = vis->green_mask;
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
      junk = vis->blue_mask;
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
	  dp = data + (i * outimg->bpl);
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

	      conv.val = (rd << rshift) | (gr << gshift) | (bl << bshift);
#if WORDS_BIGENDIAN
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
	    }
	}
    }  
  return outimg;
}

static void
gtk_print_image_instance (struct Lisp_Image_Instance *p,
			  Lisp_Object printcharfun,
			  int escapeflag)
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
      write_c_string (printcharfun, ")");
      break;
#if HAVE_SUBWINDOWS
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
	      gtk_widget_destroy (IMAGE_INSTANCE_SUBWINDOW_ID (p));

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
	  abort();
	}
      else
	{
	  int i;
	  if (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p))
	    disable_glyph_animated_timeout (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p));

	  if (IMAGE_INSTANCE_GTK_MASK (p) &&
	      IMAGE_INSTANCE_GTK_MASK (p) != IMAGE_INSTANCE_GTK_PIXMAP (p))
	    gdk_pixmap_unref (IMAGE_INSTANCE_GTK_MASK (p));
	  IMAGE_INSTANCE_PIXMAP_MASK (p) = 0;

	  if (IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p))
	    {
	      for (i = 0; i < IMAGE_INSTANCE_PIXMAP_MAXSLICE (p); i++)
		if (IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p,i))
		  {
		    gdk_pixmap_unref (IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p,i));
		    IMAGE_INSTANCE_GTK_PIXMAP_SLICE (p, i) = 0;
		  }
	      xfree (IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p), GdkPixmap **);
	      IMAGE_INSTANCE_GTK_PIXMAP_SLICES (p) = 0;
	    }

	  if (IMAGE_INSTANCE_GTK_CURSOR (p))
	    {
	      gdk_cursor_destroy (IMAGE_INSTANCE_GTK_CURSOR (p));
	      IMAGE_INSTANCE_GTK_CURSOR (p) = 0;
	    }
	}

#if 0
	    /* #### BILL!!! */
      if (IMAGE_INSTANCE_GTK_NPIXELS (p) != 0)
	{
	  XFreeColors (dpy,
		       IMAGE_INSTANCE_GTK_COLORMAP (p),
		       IMAGE_INSTANCE_GTK_PIXELS (p),
		       IMAGE_INSTANCE_GTK_NPIXELS (p), 0);
	  IMAGE_INSTANCE_GTK_NPIXELS (p) = 0;
	}
#endif
    }

  if (IMAGE_INSTANCE_TYPE (p) != IMAGE_WIDGET
      && IMAGE_INSTANCE_TYPE (p) != IMAGE_SUBWINDOW
      && IMAGE_INSTANCE_GTK_PIXELS (p))
    {
      xfree (IMAGE_INSTANCE_GTK_PIXELS (p), unsigned long *);
      IMAGE_INSTANCE_GTK_PIXELS (p) = 0;
    }

  xfree (p->data, void *);
  p->data = 0;
}

static int
gtk_image_instance_equal (struct Lisp_Image_Instance *p1,
			  struct Lisp_Image_Instance *p2, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p1))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      if (IMAGE_INSTANCE_GTK_COLORMAP (p1) != IMAGE_INSTANCE_GTK_COLORMAP (p2) ||
	  IMAGE_INSTANCE_GTK_NPIXELS (p1) != IMAGE_INSTANCE_GTK_NPIXELS (p2))
	return 0;
#if HAVE_SUBWINDOWS
    case IMAGE_SUBWINDOW:
      /* #### implement me */
#endif
      break;
    default:
      break;
    }

  return 1;
}

static unsigned long
gtk_image_instance_hash (struct Lisp_Image_Instance *p, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      return IMAGE_INSTANCE_GTK_NPIXELS (p);
#if HAVE_SUBWINDOWS
    case IMAGE_SUBWINDOW:
      /* #### implement me */
      return 0;
#endif
    default:
      return 0;
    }
}

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
    xnew_array_and_zero (GdkPixmap *, slices);
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

static Lisp_Object
locate_pixmap_file (Lisp_Object name)
{
  return gtk_locate_pixmap_file (name);
}


/************************************************************************/
/*                           cursor functions                           */
/************************************************************************/

/* Check that this server supports cursors of size WIDTH * HEIGHT.  If
   not, signal an error.  INSTANTIATOR is only used in the error
   message. */

static void
check_pointer_sizes (unsigned int width, unsigned int height,
		     Lisp_Object instantiator)
{
    /* #### BILL!!! There is no way to call XQueryBestCursor from Gdk! */
#if 0
  unsigned int best_width, best_height;
  if (! XQueryBestCursor (DisplayOfScreen (xs), RootWindowOfScreen (xs),
			  width, height, &best_width, &best_height))
    /* this means that an X error of some sort occurred (we trap
       these so they're not fatal). */
    gui_error ("XQueryBestCursor() failed?", instantiator);

  if (width > best_width || height > best_height)
    signal_ferror_with_frob (Qgui_error, instantiator,
			     "pointer too large (%dx%d): "
			     "server requires %dx%d or smaller",
			     width, height, best_width, best_height);
#endif
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
maybe_recolor_cursor (Lisp_Object image_instance, Lisp_Object foreground,
		      Lisp_Object background)
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
init_image_instance_from_gdk_image (struct Lisp_Image_Instance *ii,
				    GdkImage *gdk_image,
				    int dest_mask,
				    GdkColormap *cmap,
				    unsigned long *pixels,
				    int npixels,
				    int slices,
				    Lisp_Object instantiator)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkGC *gc;
  GdkWindow *d;
  GdkPixmap *pixmap;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  d = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (device)));

  if (!(dest_mask & IMAGE_COLOR_PIXMAP_MASK))
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK);

  pixmap = gdk_pixmap_new (d, gdk_image->width, gdk_image->height, gdk_image->depth);
  if (!pixmap)
    gui_error ("Unable to create pixmap", instantiator);

  gc = gdk_gc_new (pixmap);
  if (!gc)
    {
      gdk_pixmap_unref (pixmap);
      gui_error ("Unable to create GC", instantiator);
    }

  gdk_draw_image (GDK_DRAWABLE (pixmap), gc, gdk_image,
		  0, 0, 0, 0, gdk_image->width, gdk_image->height);

  gdk_gc_destroy (gc);

  gtk_initialize_pixmap_image_instance (ii, slices, IMAGE_COLOR_PIXMAP);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  IMAGE_INSTANCE_GTK_PIXMAP (ii) = pixmap;
  IMAGE_INSTANCE_GTK_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = gdk_image->width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = gdk_image->height;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = gdk_image->depth;
  IMAGE_INSTANCE_GTK_COLORMAP (ii) = cmap;
  IMAGE_INSTANCE_GTK_PIXELS (ii) = pixels;
  IMAGE_INSTANCE_GTK_NPIXELS (ii) = npixels;
}

#if 0
void init_image_instance_from_gdk_pixmap (struct Lisp_Image_Instance *ii,
					  struct device *device,
					  GdkPixmap *gdk_pixmap,
					  int dest_mask,
					  Lisp_Object instantiator)
{
  GdkWindow *d;
  gint width, height, depth;

  if (!DEVICE_GTK_P (device))
    abort ();

  IMAGE_INSTANCE_DEVICE (ii) = device;
  IMAGE_INSTANCE_TYPE (ii) = IMAGE_COLOR_PIXMAP;

  d = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (device));

  if (!(dest_mask & IMAGE_COLOR_PIXMAP_MASK))
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK);

  gtk_initialize_pixmap_image_instance (ii, IMAGE_COLOR_PIXMAP);

  gdk_window_get_geometry (gdk_pixmap, NULL, NULL, &width, &height, &depth);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) = Qnil;
  IMAGE_INSTANCE_GTK_PIXMAP (ii) = gdk_pixmap;
  IMAGE_INSTANCE_GTK_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = height;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = depth;
  IMAGE_INSTANCE_GTK_COLORMAP (ii) = gdk_window_get_colormap (gdk_pixmap);
  IMAGE_INSTANCE_GTK_PIXELS (ii) = 0;
  IMAGE_INSTANCE_GTK_NPIXELS (ii) = 0;
}
#endif

static void
image_instance_add_gdk_image (Lisp_Image_Instance *ii,
			      GdkImage *gdk_image,
			      int slice,
			      Lisp_Object instantiator)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkWindow *d;
  GdkPixmap *pixmap;
  GdkGC *gc;

  d = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (device)));

  pixmap = gdk_pixmap_new (d, gdk_image->width, gdk_image->height, gdk_image->depth);

  if (!pixmap)
    gui_error ("Unable to create pixmap", instantiator);

  gc = gdk_gc_new (pixmap);

  if (!gc)
    {
      gdk_pixmap_unref (pixmap);
      gui_error ("Unable to create GC", instantiator);
    }

  gdk_draw_image (GDK_DRAWABLE (pixmap), gc, gdk_image, 0, 0, 0, 0,
		  gdk_image->width, gdk_image->height);

  gdk_gc_destroy (gc);

  IMAGE_INSTANCE_GTK_PIXMAP_SLICE (ii, slice) = pixmap;
}

static void
gtk_init_image_instance_from_eimage (struct Lisp_Image_Instance *ii,
				     int width, int height,
				     int slices,
				     unsigned char *eimage, 
				     int dest_mask,
				     Lisp_Object instantiator,
				     Lisp_Object domain)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkColormap *cmap = DEVICE_GTK_COLORMAP (XDEVICE(device));
  unsigned long *pixtbl = NULL;
  int npixels = 0;
  int slice;
  GdkImage* gdk_image;


  for (slice = 0; slice < slices; slice++)
    {
      gdk_image = convert_EImage_to_GDKImage (device, width, height, eimage,
					      &pixtbl, &npixels);
      if (!gdk_image)
	{
	  if (pixtbl)
	    xfree (pixtbl, unsigned long *);
	  signal_image_error("EImage to GdkImage conversion failed", instantiator);
	}

      if (slice == 0)
	/* Now create the pixmap and set up the image instance */
	init_image_instance_from_gdk_image (ii, gdk_image, dest_mask,
					    cmap, pixtbl, npixels, slices,
					    instantiator);
      else
	image_instance_add_gdk_image (ii, gdk_image, slice, instantiator);

      if (gdk_image)
	{
	  gdk_image_destroy (gdk_image);
	}
      gdk_image = 0;
    }
}

/* Given inline data for a mono pixmap, create and return the
   corresponding X object. */

static GdkPixmap *
pixmap_from_xbm_inline (Lisp_Object device, int width, int height,
			/* Note that data is in ext-format! */
			const Extbyte *bits)
{
    return (gdk_bitmap_create_from_data (GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (device))),
					 (char *) bits, width, height));
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
				     GdkPixmap *mask,
				     Lisp_Object mask_filename)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object foreground = find_keyword_in_vector (instantiator, Q_foreground);
  Lisp_Object background = find_keyword_in_vector (instantiator, Q_background);
  GdkColor fg;
  GdkColor bg;
  enum image_instance_type type;
  GdkWindow *draw = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (device)));
  GdkColormap *cmap = DEVICE_GTK_COLORMAP (XDEVICE(device));
  GdkColor black;
  GdkColor white;

  gdk_color_black(cmap, &black);
  gdk_color_white(cmap, &white);

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
	  pixmap_from_xbm_inline (device, width, height, (Extbyte *) bits);
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
	    gdk_pixmap_create_from_data (draw, (char *) bits, width, height, d, &fg, &bg);
	IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = d;
      }
      break;

    case IMAGE_POINTER:
    {
	GdkColor fg_color, bg_color;
	GdkPixmap *source;

	check_pointer_sizes (width, height, instantiator);

	source = gdk_pixmap_create_from_data (draw, (char *) bits, width, height, 1, &black, &white);

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
	    gdk_cursor_new_from_pixmap (source, mask, &fg_color, &bg_color,
					!NILP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)) ?
					XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)) : 0,
					!NILP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) ?
					XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) : 0);
      }
      break;

    default:
      abort ();
    }
}

static void
xbm_instantiate_1 (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, int width, int height,
		   /* Note that data is in ext-format! */
		   const char *bits)
{
  Lisp_Object mask_data = find_keyword_in_vector (instantiator, Q_mask_data);
  Lisp_Object mask_file = find_keyword_in_vector (instantiator, Q_mask_file);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  GdkPixmap *mask = 0;
  const char *gcc_may_you_rot_in_hell;

  if (!NILP (mask_data))
    {
      TO_EXTERNAL_FORMAT (LISP_STRING, XCAR (XCDR (XCDR (mask_data))),
			  C_STRING_ALLOCA, gcc_may_you_rot_in_hell,
			  Qfile_name);
      mask =
	pixmap_from_xbm_inline (IMAGE_INSTANCE_DEVICE (ii),
				XINT (XCAR (mask_data)),
				XINT (XCAR (XCDR (mask_data))),
				(const unsigned char *)
				gcc_may_you_rot_in_hell);
    }

  init_image_instance_from_xbm_inline (ii, width, height, bits,
				       instantiator, pointer_fg, pointer_bg,
				       dest_mask, mask, mask_file);
}

/* Instantiate method for XBM's. */

static void
gtk_xbm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		     int dest_mask, Lisp_Object domain)
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  const char *gcc_go_home;

  assert (!NILP (data));

  TO_EXTERNAL_FORMAT (LISP_STRING, XCAR (XCDR (XCDR (data))),
		      C_STRING_ALLOCA, gcc_go_home,
		      Qbinary);

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, XINT (XCAR (data)),
		     XINT (XCAR (XCDR (data))), gcc_go_home);
}


#ifdef HAVE_XPM
/**********************************************************************
 *                             XPM                                    *
 **********************************************************************/
static Lisp_Object
write_lisp_string_to_temp_file (Lisp_Object string)
{
  Lisp_Object instream, outstream;
  Lstream *istr, *ostr;
  Char_Binary tempbuf[1024]; /* some random amount */
  int fubar = 0;
  FILE *tmpfil;
  static Extbyte_dynarr *conversion_out_dynarr;
  Bytecount bstart, bend;
  Lisp_Object tempfile;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object conv_out_stream;
  Lstream *costr;

  /* This function can GC */
  if (!conversion_out_dynarr)
    conversion_out_dynarr = Dynarr_new (Extbyte);
  else
    Dynarr_reset (conversion_out_dynarr);

  /* Create the temporary file ... */
  tempfile = Fmake_temp_name (build_string ("/tmp/emacs"));
  tmpfil = qxe_fopen (XSTRING_DATA (tempfile), "w");
  if (!tmpfil)
    {
      if (tmpfil)
	{
	  int old_errno = errno;
	  retry_fclose (tmpfil);
	  qxe_unlink (XSTRING_DATA (tempfile));
	  errno = old_errno;
	}
      report_file_error ("Creating temp file", tempfile);
    }

  CHECK_STRING (string);
  get_string_range_byte (string, Qnil, Qnil, &bstart, &bend,
			 GB_HISTORICAL_STRING_BEHAVIOR);
  instream = make_lisp_string_input_stream (string, bstart, bend);
  istr = XLSTREAM (instream);
  /* setup the out stream */
  outstream =
    make_dynarr_output_stream ((unsigned_char_dynarr *) conversion_out_dynarr);
  ostr = XLSTREAM (outstream);
  /* setup the conversion stream */
  conv_out_stream =
    make_coding_output_stream (ostr, Qbinary, CODING_ENCODE, 0);
  costr = XLSTREAM (conv_out_stream);
  GCPRO4 (tempfile, instream, outstream, conv_out_stream);

  /* Get the data while doing the conversion */
  while (1)
    {
      int size_in_bytes = Lstream_read (istr, tempbuf, sizeof (tempbuf));
      if (!size_in_bytes)
	break;
      /* It does seem the flushes are necessary... */
      Lstream_write (costr, tempbuf, size_in_bytes);
      Lstream_flush (costr);
      Lstream_flush (ostr);
      if (retry_fwrite ((unsigned char *)Dynarr_atp(conversion_out_dynarr, 0),
		  Dynarr_length(conversion_out_dynarr), 1, tmpfil) != 1)
	{
	  fubar = 1;
	  break;
	}
      /* reset the dynarr */
      Lstream_rewind(ostr);
    }
  
  if (retry_fclose (tmpfil) != 0)
    fubar = 1;
  Lstream_close (istr);
  Lstream_close (costr);
  Lstream_close (ostr);

  Lstream_delete (istr);
  Lstream_delete (ostr);
  Lstream_delete (costr);

  if (fubar)
    report_file_error ("Writing temp file", tempfile);

  UNGCPRO;
  return tempfile;
}

struct color_symbol
{
  char*		name;
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

      colortbl[j].name = (char *) XSTRING_DATA (XCAR (cons));
      free_cons (XCONS (cons));
      cons = results;
      results = XCDR (results);
      free_cons (XCONS (cons));
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
  GdkColormap *cmap;
  int depth;
  GdkVisual *visual;
  GdkPixmap *pixmap;
  GdkPixmap *mask = 0;
  GdkWindow *window = 0;
  int nsymbols = 0, i = 0;
  struct color_symbol *color_symbols = NULL;
  GdkColor *transparent_color = NULL;
  Lisp_Object color_symbol_alist = find_keyword_in_vector (instantiator,
							   Q_color_symbols);
  enum image_instance_type type;
  int force_mono;
  unsigned int w, h;
  Lisp_Object tempfile = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

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
  force_mono = (type != IMAGE_COLOR_PIXMAP);

  GCPRO4 (device, data, color_symbol_alist, tempfile);

  window = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (device)));
  cmap = DEVICE_GTK_COLORMAP (XDEVICE (device));
  depth = DEVICE_GTK_DEPTH (XDEVICE (device));
  visual = DEVICE_GTK_VISUAL (XDEVICE (device));

  gtk_initialize_pixmap_image_instance (ii, 1, type);

  assert (!NILP (data));

  /* Need to get the transparent color here */
  color_symbols = extract_xpm_color_names (device, domain, color_symbol_alist,
					   &nsymbols);
  for (i = 0; i < nsymbols; i++)
    {
      if (!qxestrcasecmp ("BgColor", color_symbols[i].name) ||
	  !qxestrcasecmp ("None", color_symbols[i].name))
	{
	  transparent_color = &color_symbols[i].color;
	}
    }

  tempfile = write_lisp_string_to_temp_file (data);
  {
    Extbyte *tempfileout;

    LISP_STRING_TO_EXTERNAL (tempfile, tempfileout, Qfile_name);
    pixmap = gdk_pixmap_create_from_xpm (window, &mask, transparent_color,
					 tempfileout);
  }
  qxe_unlink (XSTRING_DATA (tempfile));

  if (color_symbols)
    xfree (color_symbols, struct color_symbol *);

  if (!pixmap)
    signal_image_error ("Error reading pixmap", data);

  gdk_window_get_geometry (pixmap, NULL, NULL, &w, &h, &depth);

  IMAGE_INSTANCE_GTK_PIXMAP (ii) = pixmap;
  IMAGE_INSTANCE_GTK_MASK (ii) = mask;
  IMAGE_INSTANCE_GTK_COLORMAP (ii) = cmap;
  IMAGE_INSTANCE_GTK_PIXELS (ii) = 0;
  IMAGE_INSTANCE_GTK_NPIXELS (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = w;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = h;
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
      {
	GdkColor fg, bg;
	unsigned int xhot, yhot;

	/* #### Gtk does not give us access to the hotspots of a pixmap */
	xhot = yhot = 1;
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) = make_int (xhot);
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) = make_int (yhot);

	check_pointer_sizes (w, h, instantiator);

	/* If the loaded pixmap has colors allocated (meaning it came from an
	   XPM file), then use those as the default colors for the cursor we
	   create.  Otherwise, default to pointer_fg and pointer_bg.
	*/
	if (depth > 1)
	  {
	    warn_when_safe (Qunimplemented, Qnotice,
			    "GTK does not support XPM cursors...\n");
	    IMAGE_INSTANCE_GTK_CURSOR (ii) = gdk_cursor_new (GDK_COFFEE_MUG);
	  }
	else
	  {
	    generate_cursor_fg_bg (device, &pointer_fg, &pointer_bg,
				   &fg, &bg);
	    IMAGE_INSTANCE_PIXMAP_FG (ii) = pointer_fg;
	    IMAGE_INSTANCE_PIXMAP_BG (ii) = pointer_bg;
	    IMAGE_INSTANCE_GTK_CURSOR (ii) =
	      gdk_cursor_new_from_pixmap (pixmap, mask, &fg, &bg, xhot, yhot);
	  }
      }

      break;

    default:
      abort ();
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
		   int dest_mask, Lisp_Object domain)
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  int i, stattis;
  char *p, *bits, *bp;
  const char * volatile emsg = 0;
  const char * volatile dstring;

  assert (!NILP (data));

  LISP_STRING_TO_EXTERNAL (data, dstring, Qbinary);

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

extern guint symbol_to_enum (Lisp_Object, GtkType);

static guint resource_name_to_resource (Lisp_Object name, int type)
{
  if (type == IMAGE_POINTER)
    return (symbol_to_enum (name, GTK_TYPE_GDK_CURSOR_TYPE));
  else
    return (0);
}

static int
resource_symbol_to_type (Lisp_Object data)
{
  if (EQ (data, Qcursor))
    return IMAGE_POINTER;
#if 0
  else if (EQ (data, Qicon))
    return IMAGE_ICON;
  else if (EQ (data, Qbitmap))
    return IMAGE_BITMAP;
#endif
  else
    return 0;
}

static void
gtk_resource_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			  int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  GdkCursor *c = NULL;
  unsigned int type = 0;
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
  c = gdk_cursor_new (resource_name_to_resource (resource_id, type));
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
  if (!INTP (data))
    CHECK_STRING (data);
  else
    CHECK_INT (data);
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
		      Lisp_Object dest_mask)
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

  filename = locate_pixmap_file (file);
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
	    alist = Fcons (Fcons (Q_hotspot_x, make_int (xhot)),
			   alist);
	  if (yhot != -1)
	    alist = Fcons (Fcons (Q_hotspot_y, make_int (yhot)),
			   alist);

	  alist = xbm_mask_file_munging (alist, filename, Qnil, console_type);

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
		  int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  GdkColor fg, bg;
  GdkFont *source, *mask;
  char source_name[PATH_MAX], mask_name[PATH_MAX], dummy;
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
			   build_string (source_name),
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
				    build_string (mask_name), data));
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

static char *__downcase (const char *name)
{
    char *converted = strdup(name);
    char *work = converted;

    while (*work)
    {
	*work = tolower(*work);
	work++;
    }
    return(converted);
}

/* This is basically the equivalent of XmuCursorNameToIndex */
static gint
cursor_name_to_index (const char *name)
{
    int i;
    static char *the_gdk_cursors[GDK_NUM_GLYPHS];

    if (!the_gdk_cursors[GDK_BASED_ARROW_UP])
    {
	/* Need to initialize the array */
	/* Supposedly since this array is static it should be
           initialized to NULLs for us, but I'm very paranoid. */
	for (i = 0; i < GDK_NUM_GLYPHS; i++)
	{
	    the_gdk_cursors[i] = NULL;
	}

#define FROB_CURSOR(x) the_gdk_cursors[GDK_##x] = __downcase(#x)
	FROB_CURSOR(ARROW);			FROB_CURSOR(BASED_ARROW_DOWN);
	FROB_CURSOR(BASED_ARROW_UP);		FROB_CURSOR(BOAT);
	FROB_CURSOR(BOGOSITY);			FROB_CURSOR(BOTTOM_LEFT_CORNER);
	FROB_CURSOR(BOTTOM_RIGHT_CORNER);	FROB_CURSOR(BOTTOM_SIDE);
	FROB_CURSOR(BOTTOM_TEE);		FROB_CURSOR(BOX_SPIRAL);
	FROB_CURSOR(CENTER_PTR);		FROB_CURSOR(CIRCLE);
	FROB_CURSOR(CLOCK);			FROB_CURSOR(COFFEE_MUG);
	FROB_CURSOR(CROSS);			FROB_CURSOR(CROSS_REVERSE);
	FROB_CURSOR(CROSSHAIR);			FROB_CURSOR(DIAMOND_CROSS);
	FROB_CURSOR(DOT);			FROB_CURSOR(DOTBOX);
	FROB_CURSOR(DOUBLE_ARROW);		FROB_CURSOR(DRAFT_LARGE);
	FROB_CURSOR(DRAFT_SMALL);		FROB_CURSOR(DRAPED_BOX);
	FROB_CURSOR(EXCHANGE);			FROB_CURSOR(FLEUR);
	FROB_CURSOR(GOBBLER);			FROB_CURSOR(GUMBY);
	FROB_CURSOR(HAND1);			FROB_CURSOR(HAND2);
	FROB_CURSOR(HEART);			FROB_CURSOR(ICON);
	FROB_CURSOR(IRON_CROSS);		FROB_CURSOR(LEFT_PTR);
	FROB_CURSOR(LEFT_SIDE);			FROB_CURSOR(LEFT_TEE);
	FROB_CURSOR(LEFTBUTTON);		FROB_CURSOR(LL_ANGLE);
	FROB_CURSOR(LR_ANGLE);			FROB_CURSOR(MAN);
	FROB_CURSOR(MIDDLEBUTTON);		FROB_CURSOR(MOUSE);
	FROB_CURSOR(PENCIL);			FROB_CURSOR(PIRATE);
	FROB_CURSOR(PLUS);			FROB_CURSOR(QUESTION_ARROW);
	FROB_CURSOR(RIGHT_PTR);			FROB_CURSOR(RIGHT_SIDE);
	FROB_CURSOR(RIGHT_TEE);			FROB_CURSOR(RIGHTBUTTON);
	FROB_CURSOR(RTL_LOGO);			FROB_CURSOR(SAILBOAT);
	FROB_CURSOR(SB_DOWN_ARROW);		FROB_CURSOR(SB_H_DOUBLE_ARROW);
	FROB_CURSOR(SB_LEFT_ARROW);		FROB_CURSOR(SB_RIGHT_ARROW);
	FROB_CURSOR(SB_UP_ARROW);		FROB_CURSOR(SB_V_DOUBLE_ARROW);
	FROB_CURSOR(SHUTTLE);			FROB_CURSOR(SIZING);
	FROB_CURSOR(SPIDER);			FROB_CURSOR(SPRAYCAN);
	FROB_CURSOR(STAR);			FROB_CURSOR(TARGET);
	FROB_CURSOR(TCROSS);			FROB_CURSOR(TOP_LEFT_ARROW);
	FROB_CURSOR(TOP_LEFT_CORNER);		FROB_CURSOR(TOP_RIGHT_CORNER);
	FROB_CURSOR(TOP_SIDE);			FROB_CURSOR(TOP_TEE);
	FROB_CURSOR(TREK);			FROB_CURSOR(UL_ANGLE);
	FROB_CURSOR(UMBRELLA);			FROB_CURSOR(UR_ANGLE);
	FROB_CURSOR(WATCH);			FROB_CURSOR(XTERM);
	FROB_CURSOR(X_CURSOR);
#undef FROB_CURSOR
    }

    for (i = 0; i < GDK_NUM_GLYPHS; i++)
    {
	if (!the_gdk_cursors[i]) continue;
	if (!strcmp (the_gdk_cursors[i], name))
	{
	    return (i);
	}
    }
    return(-1);
}

static void
cursor_font_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			 int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  int i;
  const char *name_ext;
  Lisp_Object foreground, background;

  if (!DEVICE_GTK_P (XDEVICE (device)))
    gui_error ("Not a Gtk device", device);

  if (!(dest_mask & IMAGE_POINTER_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_POINTER_MASK);

  TO_EXTERNAL_FORMAT (LISP_STRING, data,
		      C_STRING_ALLOCA, name_ext,
		      Qfile_name);

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
      abort ();
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
      abort ();
    }
  else				/* must be a widget */
    {
      struct frame *f = XFRAME (IMAGE_INSTANCE_FRAME (p));
      GtkWidget *wid = IMAGE_INSTANCE_GTK_CLIPWIDGET (p);
      GtkAllocation a;

      if (!wid) return;

      a.x = x + IMAGE_INSTANCE_GTK_WIDGET_XOFFSET (p);
      a.y = y + IMAGE_INSTANCE_GTK_WIDGET_YOFFSET (p);
      a.width = dga->width;
      a.height = dga->height;

      if ((a.width  != wid->allocation.width)  ||
	  (a.height != wid->allocation.height))
	{
	  gtk_widget_size_allocate (IMAGE_INSTANCE_GTK_CLIPWIDGET (p), &a);
	}

      /* #### FIXME DAMMIT */
      if ((wid->allocation.x != -dga->xoffset) ||
	  (wid->allocation.y != -dga->yoffset))
	{
	  guint32 old_flags = GTK_WIDGET_FLAGS (FRAME_GTK_TEXT_WIDGET (f));

	  /* Fucking GtkFixed widget queues a resize when you add a widget.
	  ** But only if it is visible.
	  ** losers.
	  */
	  GTK_WIDGET_FLAGS(FRAME_GTK_TEXT_WIDGET (f)) &= ~GTK_VISIBLE;
	  if (IMAGE_INSTANCE_GTK_ALREADY_PUT(p))
	    {
	      gtk_fixed_move (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
			      wid,
			      -dga->xoffset, -dga->yoffset);
	    }
	  else
	    {
	      IMAGE_INSTANCE_GTK_ALREADY_PUT(p) = TRUE;
	      gtk_fixed_put (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
			     wid,
			     -dga->xoffset, -dga->yoffset);
	    }
	  GTK_WIDGET_FLAGS(FRAME_GTK_TEXT_WIDGET (f)) = old_flags;
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
			     -dga->xoffset, -dga->yoffset);
	    }
	}

      if (!IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (p))
	{
	  gtk_widget_map (wid);
	}

      gtk_widget_draw (wid, NULL);
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
      Lisp_Object image_instance = wrap_image_instance (p);


      /* Need to update GtkArgs that might have changed... */
      /* #### FIXME!!! */
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
      LISP_STRING_TO_EXTERNAL (val, str, Qnative);

      /* #### Need to special case each type of GtkWidget here! */
    }

  /* Possibly update the size. */
  if (IMAGE_INSTANCE_SIZE_CHANGED (p)
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p)
      ||
      IMAGE_INSTANCE_TEXT_CHANGED (p))
    {
      assert (IMAGE_INSTANCE_GTK_WIDGET_ID (p) &&
	      IMAGE_INSTANCE_GTK_CLIPWIDGET (p)) ;

      /* #### Resize the widget! */
      /* gtk_widget_size_allocate () */
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
gtk_subwindow_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			   int dest_mask, Lisp_Object domain)
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
update_widget_face (GtkWidget *w, Lisp_Image_Instance *ii,
		    Lisp_Object domain)
{
  if (0)
    {
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
  char *nm = NULL;
  GtkWidget *w = NULL;
  struct gcpro gcpro1;

  IMAGE_INSTANCE_TYPE (ii) = IMAGE_WIDGET;

  if (!NILP (IMAGE_INSTANCE_WIDGET_TEXT (ii)))
    {
      LISP_STRING_TO_EXTERNAL (IMAGE_INSTANCE_WIDGET_TEXT (ii), nm, Qnative);
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

  return (Qt);
}

static void
gtk_widget_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			int dest_mask, Lisp_Object domain)
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
gtk_widget_property (Lisp_Object image_instance, Lisp_Object prop)
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
FAKE_GTK_WIDGET_INSTANTIATOR(tab_control);
FAKE_GTK_WIDGET_INSTANTIATOR(label);

/* Update a button's clicked state. */
static void
gtk_button_redisplay (Lisp_Object image_instance)
{
  /* This function can GC if IN_REDISPLAY is false. */
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  GtkWidget *w = IMAGE_INSTANCE_GTK_CLIPWIDGET (p);

  if (GTK_WIDGET_TYPE (w) == gtk_button_get_type ())
    {
    }
  else if (GTK_WIDGET_TYPE (w) == gtk_check_button_get_type ())
    {
    }
  else if (GTK_WIDGET_TYPE (w) == gtk_radio_button_get_type ())
    {
    }
  else
    {
      /* Unknown button type... */
      abort();
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
      if (GTK_WIDGET_HAS_FOCUS (IMAGE_INSTANCE_SUBWINDOW_ID (ii)))
	return Qt;
      else
	return Qnil;
    }
  return Qunbound;
}

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
      f = XFLOATINT (val);

      gtk_progress_set_value (GTK_PROGRESS (IMAGE_INSTANCE_SUBWINDOW_ID (ii)),
			      f);
    }
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
	  Lisp_Object rest, selected =
	    gui_item_list_find_selected
	    (NILP (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii)) ?
	     XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)) :
	     XCDR (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii)));

	  LIST_LOOP (rest, XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)))
	    {
	      if (gui_item_equal_sans_selected (XCAR (rest), selected, 0, 1))
		{
		  Lisp_Object old_selected =gui_item_list_find_selected
		    (XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)));

		  /* Need to focus on the widget... */
		  stderr_out ("Hey, change the tab-focus you boob...\n");

		  /* Pick up the new selected item. */
		  XGUI_ITEM (old_selected)->selected =
		    XGUI_ITEM (XCAR (rest))->selected;
		  XGUI_ITEM (XCAR (rest))->selected =
		    XGUI_ITEM (selected)->selected;
		  /* We're not actually changing the items anymore. */
		  IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii) = 0;
		  IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (ii) = Qnil;
		  break;
		}
	    }
	}
      else
	{
	  /* More than just the order has changed... let's get busy! */
	  GtkNotebook *nb = GTK_NOTEBOOK (IMAGE_INSTANCE_GTK_CLIPWIDGET (ii));
	  guint num_pages = g_list_length (nb->children);
	  Lisp_Object rest;

	  if (num_pages >= 0)
	    {
	      int i;
	      for (i = num_pages; i >= 0; --i)
		{
		  gtk_notebook_remove_page (nb, i);
		}
	    }

	  LIST_LOOP (rest, XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)))
	    {
	      Lisp_Gui_Item *pgui = XGUI_ITEM (XCAR (rest));
	      char *c_name = NULL;

	      if (!STRINGP (pgui->name))
		pgui->name = eval_within_redisplay (pgui->name);

	      if (!STRINGP (pgui->name))
		warn_when_safe (Qredisplay, Qwarning,
				"Name does not evaluate to string");
	      else
		{
		  TO_EXTERNAL_FORMAT (LISP_STRING, pgui->name,
				      C_STRING_ALLOCA, c_name,
				      Qctext);

		  gtk_notebook_append_page (nb,
					    gtk_vbox_new (FALSE, 3),
					    gtk_label_new (c_name));
		}
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
  CONSOLE_HAS_METHOD (gtk, image_instance_hash);
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
  /* general widget methods. */
  INITIALIZE_DEVICE_IIFORMAT (gtk, widget);
  IIFORMAT_HAS_DEVMETHOD (gtk, widget, property);

  /* progress gauge */
  INITIALIZE_DEVICE_IIFORMAT (gtk, progress_gauge);
  IIFORMAT_HAS_DEVMETHOD (gtk, progress_gauge, redisplay);
  IIFORMAT_HAS_DEVMETHOD (gtk, progress_gauge, instantiate);
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

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (font, "font");
  IIFORMAT_VALID_CONSOLE (gtk, font);

  IIFORMAT_HAS_METHOD (font, validate);
  IIFORMAT_HAS_METHOD (font, possible_dest_types);
  IIFORMAT_HAS_METHOD (font, instantiate);

  IIFORMAT_VALID_KEYWORD (font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_background, check_valid_string);

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
}

void
complex_vars_of_glyphs_gtk (void)
{
#define BUILD_GLYPH_INST(variable, name)			\
  Fadd_spec_to_specifier					\
    (GLYPH_IMAGE (XGLYPH (variable)),				\
     vector3 (Qxbm, Q_data,					\
	      list3 (make_int (name##_width),			\
		     make_int (name##_height),			\
		     make_ext_string (name##_bits,		\
				      sizeof (name##_bits),	\
				      Qbinary))),		\
     Qglobal, Qgtk, Qnil)

  BUILD_GLYPH_INST (Vtruncation_glyph, truncator);
  BUILD_GLYPH_INST (Vcontinuation_glyph, continuer);
  BUILD_GLYPH_INST (Vxemacs_logo, xemacs);
  BUILD_GLYPH_INST (Vhscroll_glyph, hscroll);

#undef BUILD_GLYPH_INST
}

/* X specific crap */
#include <gdk/gdkx.h>
/* #### Should remove all this X specific stuff when GTK/GDK matures a
   bit more and provides an abstraction for it. */
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
      IMAGE_INSTANCE_GTK_MASK (p) = 0;
      break;

    default:
      return 0;
    }

  {
    GdkWindow *draw = GET_GTK_WIDGET_WINDOW (DEVICE_GTK_APP_SHELL (XDEVICE (IMAGE_INSTANCE_DEVICE (p))));
    GdkPixmap *new_pxmp = gdk_pixmap_new (draw,
					  IMAGE_INSTANCE_PIXMAP_WIDTH (p),
					  IMAGE_INSTANCE_PIXMAP_HEIGHT (p),
					  DEVICE_GTK_DEPTH (XDEVICE (IMAGE_INSTANCE_DEVICE (p))));
    GdkGCValues gcv;
    GdkGC *gc;

    gcv.foreground = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (foreground));
    gcv.background = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (background));
    gc = gdk_gc_new_with_values (new_pxmp, &gcv, GDK_GC_BACKGROUND | GDK_GC_FOREGROUND);

    XCopyPlane (GDK_WINDOW_XDISPLAY (draw),
		GDK_WINDOW_XWINDOW (IMAGE_INSTANCE_GTK_PIXMAP (p)),
		GDK_WINDOW_XWINDOW (new_pxmp),
		GDK_GC_XGC (gc), 0, 0,
		IMAGE_INSTANCE_PIXMAP_WIDTH (p),
		IMAGE_INSTANCE_PIXMAP_HEIGHT (p),
		0, 0, 1);

    gdk_gc_destroy (gc);
    IMAGE_INSTANCE_GTK_PIXMAP (p) = new_pxmp;
    IMAGE_INSTANCE_PIXMAP_DEPTH (p) = DEVICE_GTK_DEPTH (XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
    IMAGE_INSTANCE_PIXMAP_FG (p) = foreground;
    IMAGE_INSTANCE_PIXMAP_BG (p) = background;
    return 1;
  }
}

