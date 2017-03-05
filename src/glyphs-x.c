/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996, 2001, 2002, 2003, 2004, 2005, 2010 Ben Wing
   Copyright (C) 1995 Sun Microsystems
   Copyright (C) 1999, 2000, 2002 Andy Piper

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

/* 7-8-00 This file is more or less Mule-ized. */

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
   GIF/JPEG/PNG/TIFF code moved to new glyph-eimage.c by Andy Piper for 21.0
   Subwindow and Widget support by Andy Piper for 21.2

   TODO:
   Support the GrayScale, StaticColor and StaticGray visual classes.
   Convert images.el to C and stick it in here?
 */

/* Mule-ized last 6-22-00 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "faces.h"
#include "file-coding.h"
#include "frame-impl.h"
#include "gui.h"
#include "imgproc.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"
#include "process.h" /* egetenv() */
#include "window.h"

#include "console-x-impl.h"
#include "glyphs-x.h"
#include "fontcolor-x-impl.h"

#include "sysfile.h"
#include "sysproc.h" /* for qxe_getpid() */

#include <setjmp.h>

#ifdef LWLIB_WIDGETS_MOTIF
#include "xmotif.h"
#include <Xm/Scale.h>
#endif
#include <X11/IntrinsicP.h>
#include <X11/Xmu/CurUtil.h>

#define LISP_DEVICE_TO_X_SCREEN(dev) XDefaultScreenOfDisplay (DEVICE_X_DISPLAY (XDEVICE (dev)))

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
#ifdef HAVE_XPM
DEFINE_DEVICE_IIFORMAT (x, xpm);
#endif
DEFINE_DEVICE_IIFORMAT (x, xbm);
DEFINE_DEVICE_IIFORMAT (x, subwindow);
#ifdef HAVE_XFACE
DEFINE_DEVICE_IIFORMAT (x, xface);
#endif

DEFINE_IMAGE_INSTANTIATOR_FORMAT (cursor_font);
Lisp_Object Qcursor_font;

DEFINE_IMAGE_INSTANTIATOR_FORMAT (font);

DEFINE_IMAGE_INSTANTIATOR_FORMAT (autodetect);

#ifdef HAVE_X_WIDGETS
DECLARE_IMAGE_INSTANTIATOR_FORMAT (layout);
DEFINE_DEVICE_IIFORMAT (x, widget);
DEFINE_DEVICE_IIFORMAT (x, native_layout);
DEFINE_DEVICE_IIFORMAT (x, button);
DEFINE_DEVICE_IIFORMAT (x, progress_gauge);
DEFINE_DEVICE_IIFORMAT (x, edit_field);
#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
DEFINE_DEVICE_IIFORMAT (x, combo_box);
#endif
DEFINE_DEVICE_IIFORMAT (x, tab_control);
DEFINE_DEVICE_IIFORMAT (x, label);
#endif

static void cursor_font_instantiate (Lisp_Object image_instance,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     Lisp_Object domain);

#ifdef HAVE_X_WIDGETS
static void update_widget_face (widget_value* wv,
				Lisp_Image_Instance* ii, Lisp_Object domain);
static void update_tab_widget_face (widget_value* wv,
				    Lisp_Image_Instance* ii,
				    Lisp_Object domain);
#endif
void emacs_Xt_handle_widget_losing_focus (struct frame* f,
					  Widget losing_widget);
void emacs_Xt_enqueue_focus_event (Widget wants_it, Lisp_Object frame,
				   int in_p);

#include "bitmaps.h"


/************************************************************************/
/*                      image instance methods                          */
/************************************************************************/

/************************************************************************/
/* convert from a series of RGB triples to an XImage formated for the   */
/* proper display							*/
/************************************************************************/
static XImage *
convert_EImage_to_XImage (Lisp_Object device, int width, int height,
			  Binbyte *pic, unsigned long **pixtbl,
			  int *npixels)
{
  Display *dpy;
  Colormap cmap;
  Visual *vis;
  XImage *outimg;
  int depth, bitmap_pad, bits_per_pixel, byte_cnt, i, j;
  int rd,gr,bl,q;
  Binbyte *data, *ip, *dp;
  quant_table *qtable = 0;
  union {
    UINT_32_BIT val;
    char cp[4];
  } conv;

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  cmap = DEVICE_X_COLORMAP (XDEVICE(device));
  vis = DEVICE_X_VISUAL (XDEVICE(device));
  depth = DEVICE_X_DEPTH(XDEVICE(device));

  if (vis->X_CLASSFIELD == GrayScale || vis->X_CLASSFIELD == StaticColor ||
      vis->X_CLASSFIELD == StaticGray)
    {
      /* #### Implement me!!! */
      return NULL;
    }

  if (vis->X_CLASSFIELD == PseudoColor)
    {
      /* Quantize the image and get a histogram while we're at it.
	 Do this first to save memory */
      qtable = build_EImage_quantable(pic, width, height, 256);
      if (qtable == NULL) return NULL;
    }

  bitmap_pad = ((depth > 16) ? 32 :
		(depth >  8) ? 16 :
		8);

  outimg = XCreateImage (dpy, vis,
			 depth, ZPixmap, 0, 0, width, height,
			 bitmap_pad, 0);
  if (!outimg) return NULL;

  bits_per_pixel = outimg->bits_per_pixel;
  byte_cnt = bits_per_pixel >> 3;

  data = xnew_binbytes (outimg->bytes_per_line * height);
  if (!data)
    {
      XDestroyImage (outimg);
      return NULL;
    }
  outimg->data = (char *) data;

  if (vis->X_CLASSFIELD == PseudoColor)
    {
      unsigned long pixarray[256];
      int pixcount, n;
      /* use our quantize table to allocate the colors */
      pixcount = 32;
      *pixtbl = xnew_array (unsigned long, pixcount);
      *npixels = 0;

      /* #### should implement a sort by popularity to assure proper allocation */
      n = *npixels;
      for (i = 0; i < qtable->num_active_colors; i++)
	{
	  XColor color;
	  int res;

	  color.red = qtable->rm[i] ? qtable->rm[i] << 8 : 0;
	  color.green = qtable->gm[i] ? qtable->gm[i] << 8 : 0;
	  color.blue = qtable->bm[i] ? qtable->bm[i] << 8 : 0;
	  color.flags = DoRed | DoGreen | DoBlue;
	  res = x_allocate_nearest_color (dpy, cmap, vis, &color);
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
	  dp = data + (i * outimg->bytes_per_line);
	  for (j = 0; j < width; j++)
	    {
	      rd = *ip++;
	      gr = *ip++;
	      bl = *ip++;
	      conv.val = pixarray[QUANT_GET_COLOR(qtable,rd,gr,bl)];
#ifdef WORDS_BIGENDIAN
	      if (outimg->byte_order == MSBFirst)
		for (q = 4-byte_cnt; q < 4; q++) *dp++ = conv.cp[q];
	      else
		for (q = 3; q >= 4-byte_cnt; q--) *dp++ = conv.cp[q];
#else
	      if (outimg->byte_order == MSBFirst)
		for (q = byte_cnt-1; q >= 0; q--) *dp++ = conv.cp[q];
	      else
		for (q = 0; q < byte_cnt; q++) *dp++ = conv.cp[q];
#endif
	    }
	}
      xfree (qtable);
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
	  dp = data + (i * outimg->bytes_per_line);
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
#ifdef WORDS_BIGENDIAN
	      if (outimg->byte_order == MSBFirst)
		for (q = 4-byte_cnt; q < 4; q++) *dp++ = conv.cp[q];
	      else
		for (q = 3; q >= 4-byte_cnt; q--) *dp++ = conv.cp[q];
#else
	      if (outimg->byte_order == MSBFirst)
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
x_print_image_instance (Lisp_Image_Instance *p,
			Lisp_Object printcharfun,
			int UNUSED (escapeflag))
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      write_fmt_string (printcharfun, " (0x%lx",
			(unsigned long) IMAGE_INSTANCE_X_PIXMAP (p));
      if (IMAGE_INSTANCE_X_MASK (p))
	{
	  write_fmt_string (printcharfun, "/0x%lx",
			    (unsigned long) IMAGE_INSTANCE_X_MASK (p));
	}
      write_ascstring (printcharfun, ")");
      break;
    default:
      break;
    }
}

#ifdef DEBUG_WIDGETS
extern int debug_widget_instances;
#endif

static void
x_finalize_image_instance (Lisp_Image_Instance *p)
{
  if (!p->data)
    return;

  if (DEVICE_LIVE_P (XDEVICE (IMAGE_INSTANCE_DEVICE (p))))
    {
      Display *dpy = DEVICE_X_DISPLAY
	(XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
      if (0)
	;
#ifdef HAVE_X_WIDGETS
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_WIDGET)
	{
	  if (IMAGE_INSTANCE_SUBWINDOW_ID (p))
	    {
#ifdef DEBUG_WIDGETS
	      debug_widget_instances--;
	      stderr_out ("widget destroyed, %d left\n", debug_widget_instances);
#endif
	      lw_destroy_widget (IMAGE_INSTANCE_X_WIDGET_ID (p));
	      lw_destroy_widget (IMAGE_INSTANCE_X_CLIPWIDGET (p));

	      /* We can release the callbacks again. */
	      ungcpro_popup_callbacks (IMAGE_INSTANCE_X_WIDGET_LWID (p));

	      IMAGE_INSTANCE_X_WIDGET_ID (p) = 0;
	      IMAGE_INSTANCE_X_CLIPWIDGET (p) = 0;
	    }
	}
#endif
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
	{
	  if (IMAGE_INSTANCE_SUBWINDOW_ID (p))
	    XDestroyWindow (dpy, IMAGE_INSTANCE_X_SUBWINDOW_ID (p));
	  IMAGE_INSTANCE_SUBWINDOW_ID (p) = 0;
	}
      else
	{
	  int i;
	  if (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p))
	    disable_glyph_animated_timeout (IMAGE_INSTANCE_PIXMAP_TIMEOUT (p));

	  if (IMAGE_INSTANCE_X_MASK (p) &&
	      IMAGE_INSTANCE_X_MASK (p) != IMAGE_INSTANCE_X_PIXMAP (p))
	    XFreePixmap (dpy, IMAGE_INSTANCE_X_MASK (p));
	  IMAGE_INSTANCE_PIXMAP_MASK (p) = 0;

	  if (IMAGE_INSTANCE_X_PIXMAP_SLICES (p))
	    {
	      for (i = 0; i < IMAGE_INSTANCE_PIXMAP_MAXSLICE (p); i++)
		if (IMAGE_INSTANCE_X_PIXMAP_SLICE (p,i))
		  {
		    XFreePixmap (dpy, IMAGE_INSTANCE_X_PIXMAP_SLICE (p,i));
		    IMAGE_INSTANCE_X_PIXMAP_SLICE (p, i) = 0;
		  }
	      xfree (IMAGE_INSTANCE_X_PIXMAP_SLICES (p));
	      IMAGE_INSTANCE_X_PIXMAP_SLICES (p) = 0;
	    }

	  if (IMAGE_INSTANCE_X_CURSOR (p))
	    {
	      XFreeCursor (dpy, IMAGE_INSTANCE_X_CURSOR (p));
	      IMAGE_INSTANCE_X_CURSOR (p) = 0;
	    }

	  if (IMAGE_INSTANCE_X_NPIXELS (p) != 0)
	    {
	      XFreeColors (dpy,
			   IMAGE_INSTANCE_X_COLORMAP (p),
			   IMAGE_INSTANCE_X_PIXELS (p),
			   IMAGE_INSTANCE_X_NPIXELS (p), 0);
	      IMAGE_INSTANCE_X_NPIXELS (p) = 0;
	    }
	}
    }
  /* You can sometimes have pixels without a live device. I forget
     why, but that's why we free them here if we have a pixmap type
     image instance. It probably means that we might also get a memory
     leak with widgets. */
  if (IMAGE_INSTANCE_TYPE (p) != IMAGE_WIDGET
      && IMAGE_INSTANCE_TYPE (p) != IMAGE_SUBWINDOW
      && IMAGE_INSTANCE_X_PIXELS (p))
    {
      xfree (IMAGE_INSTANCE_X_PIXELS (p));
      IMAGE_INSTANCE_X_PIXELS (p) = 0;
    }

  xfree (p->data);
  p->data = 0;
}

static int
x_image_instance_equal (Lisp_Image_Instance *p1,
			Lisp_Image_Instance *p2, int UNUSED (depth))
{
  switch (IMAGE_INSTANCE_TYPE (p1))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      if (IMAGE_INSTANCE_X_COLORMAP (p1) != IMAGE_INSTANCE_X_COLORMAP (p2) ||
	  IMAGE_INSTANCE_X_NPIXELS (p1) != IMAGE_INSTANCE_X_NPIXELS (p2))
	return 0;
      break;
    default:
      break;
    }

  return 1;
}

static Hashcode
x_image_instance_hash (Lisp_Image_Instance *p, int UNUSED (depth))
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      return IMAGE_INSTANCE_X_NPIXELS (p);
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
x_initialize_pixmap_image_instance (Lisp_Image_Instance *ii,
				    int slices,
				    enum image_instance_type type)
{
  ii->data = xnew_and_zero (struct x_image_instance_data);
  IMAGE_INSTANCE_PIXMAP_MAXSLICE (ii) = slices;
  IMAGE_INSTANCE_X_PIXMAP_SLICES (ii) =
    xnew_array_and_zero (Pixmap, slices);
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
Lisp_Object Vx_bitmap_file_path;

#ifndef BITMAPDIR
#define BITMAPDIR "/usr/include/X11/bitmaps"
#endif

#define USE_XBMLANGPATH

/* Given a pixmap filename, look through all of the "standard" places
   where the file might be located.  Return a full pathname if found;
   otherwise, return Qnil. */

/* #### FIXME: when Qnil is returned, the caller can't make a difference
   #### between a non existing X device, an unreadable file, or an actual
   #### failure to locate the file, so the issued message is really not
   #### informative. -- dvl */
static Lisp_Object
x_locate_pixmap_file (Lisp_Object name)
{
  /* This function can GC if IN_REDISPLAY is false */
  Display *display;

  /* #### Unix-specific */
  if (string_byte (name, 0) == '~' ||
      string_byte (name, 0) == '/' ||
      (string_byte (name, 0) == '.' &&
       (string_byte (name, 1) == '/' ||
	(string_byte (name, 1) == '.' &&
	 (string_byte (name, 2) == '/')))))
    {
      if (!NILP (Ffile_readable_p (name)))
	return Fexpand_file_name (name, Qnil);
      else
	return Qnil;
    }

  /* Check non-absolute pathnames with a directory component relative to
     the search path; that's the way Xt does it. */
  {
    Lisp_Object defx = get_default_device (Qx);
    if (NILP (defx))
      /* This may occur during initialization. */
      return Qnil;
    else
      /* We only check the bitmapFilePath resource on the original X device. */
      display = DEVICE_X_DISPLAY (XDEVICE (defx));
  }

#ifdef USE_XBMLANGPATH
  {
    Ibyte *path = egetenv ("XBMLANGPATH");
    if (path)
      {
	Extbyte *pathext;
	SubstitutionRec subs[1];
	subs[0].match = 'B';
	LISP_PATHNAME_CONVERT_OUT (name, subs[0].substitution);
	pathext = ITEXT_TO_EXTERNAL (path, Qfile_name);
	/* #### Motif uses a big hairy default if $XBMLANGPATH isn't set.
	   We don't.  If you want it used, set it. */
	if (pathext &&
	    (pathext = XtResolvePathname (display, "bitmaps", 0, 0, pathext,
					  subs, XtNumber (subs), 0)))
	  {
	    name = build_extstring (pathext, Qfile_name);
	    XtFree (pathext);
	    return (name);
	  }
      }
  }
#endif

  if (NILP (Vx_bitmap_file_path))
    {
      char *type = 0;
      XrmValue value;
      if (XrmGetResource (XtDatabase (display),
			  "bitmapFilePath", "BitmapFilePath", &type, &value)
	  && !strcmp (type, "String"))
	{
	  Ibyte *path;

	  path = EXTERNAL_TO_ITEXT (value.addr, Qfile_name);
	  Vx_bitmap_file_path = split_env_path (0, path);
	}
      Vx_bitmap_file_path = nconc2 (Vx_bitmap_file_path,
				    (split_external_path (BITMAPDIR)));
    }

  {
    Lisp_Object found;
    if (locate_file (Vx_bitmap_file_path, name, Qnil, &found, R_OK) < 0)
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
  return x_locate_pixmap_file (name);
}


/************************************************************************/
/*                           cursor functions                           */
/************************************************************************/

/* Check that this server supports cursors of size WIDTH * HEIGHT.  If
   not, signal an error.  INSTANTIATOR is only used in the error
   message. */

static void
check_pointer_sizes (Screen *xs, int width, int height,
		     Lisp_Object instantiator)
{
  unsigned int best_width, best_height;
  if (! XQueryBestCursor (DisplayOfScreen (xs), RootWindowOfScreen (xs),
			  width, height, &best_width, &best_height))
    /* this means that an X error of some sort occurred (we trap
       these so they're not fatal). */
    gui_error ("XQueryBestCursor() failed?", instantiator);

  if (width > (int) best_width || height > (int) best_height)
    signal_ferror_with_frob (Qgui_error, instantiator,
			     "pointer too large (%dx%d): "
			     "server requires %dx%d or smaller",
			     width, height, best_width, best_height);
}


static void
generate_cursor_fg_bg (Lisp_Object device, Lisp_Object *foreground,
		       Lisp_Object *background, XColor *xfg, XColor *xbg)
{
  if (!NILP (*foreground) && !COLOR_INSTANCEP (*foreground))
    *foreground =
      Fmake_color_instance (*foreground, device,
			    encode_error_behavior_flag (ERROR_ME));
  if (COLOR_INSTANCEP (*foreground))
    *xfg = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (*foreground));
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
    *xbg = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (*background));
  else
    {
      xbg->pixel = 0;
      xbg->red = xbg->green = xbg->blue = USHRT_MAX;
    }
}

static void
maybe_recolor_cursor (Lisp_Object image_instance, Lisp_Object foreground,
		      Lisp_Object background)
{
  Lisp_Object device = XIMAGE_INSTANCE_DEVICE (image_instance);
  XColor xfg, xbg;

  generate_cursor_fg_bg (device, &foreground, &background, &xfg, &xbg);
  if (!NILP (foreground) || !NILP (background))
    {
      XRecolorCursor (DEVICE_X_DISPLAY (XDEVICE (device)),
		      XIMAGE_INSTANCE_X_CURSOR (image_instance),
		      &xfg, &xbg);
      XIMAGE_INSTANCE_PIXMAP_FG (image_instance) = foreground;
      XIMAGE_INSTANCE_PIXMAP_BG (image_instance) = background;
    }
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
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  Screen *xs = DefaultScreenOfDisplay (dpy);
  int npixels = IMAGE_INSTANCE_X_NPIXELS (ii);
  unsigned long *pixels = IMAGE_INSTANCE_X_PIXELS (ii);
  Pixmap pixmap = IMAGE_INSTANCE_X_PIXMAP (ii);
  Pixmap mask = (Pixmap) IMAGE_INSTANCE_PIXMAP_MASK (ii);
  Colormap cmap;
  XColor fg, bg;
  int i;
  int xhot = 0, yhot = 0;
  int w, h;

  if (FIXNUMP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)))
    xhot = XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii));
  if (FIXNUMP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)))
    yhot = XFIXNUM (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii));
  w = IMAGE_INSTANCE_PIXMAP_WIDTH (ii);
  h = IMAGE_INSTANCE_PIXMAP_HEIGHT (ii);

#if 1
  /* Although I haven't found it documented yet, it appears that pointers are
     always colored via the default window colormap... Sigh. */
  cmap = DefaultColormap (dpy, DefaultScreen (dpy));
  IMAGE_INSTANCE_X_COLORMAP (ii) = cmap;
#else
  cmap = IMAGE_INSTANCE_X_COLORMAP (ii);
#endif

  check_pointer_sizes (xs, w, h, instantiator);

  /* If the loaded pixmap has colors allocated (meaning it came from an
     XPM file), then use those as the default colors for the cursor we
     create.  Otherwise, default to pointer_fg and pointer_bg.
  */
  if (npixels >= 2)
    {
      /* With an XBM file, it's obvious which bit is foreground
	 and which is background, or rather, it's implicit: in
	 an XBM file, a 1 bit is foreground, and a 0 bit is
	 background.

	 XCreatePixmapCursor() assumes this property of the
	 pixmap it is called with as well; the `foreground'
	 color argument is used for the 1 bits.

	 With an XPM file, it's tricker, since the elements of
	 the pixmap don't represent FG and BG, but are actual
	 pixel values.  So we need to figure out which of those
	 pixels is the foreground color and which is the
	 background.  We do it by comparing RGB and assuming
	 that the darker color is the foreground.  This works
	 with the result of xbmtopbm|ppmtoxpm, at least.

	 It might be nice if there was some way to tag the
	 colors in the XPM file with whether they are the
	 foreground - perhaps with logical color names somehow?

	 Once we have decided which color is the foreground, we
	 need to ensure that that color corresponds to a `1' bit
	 in the Pixmap.  The XPM library wrote into the (1-bit)
	 pixmap with XPutPixel, which will ignore all but the
	 least significant bit.

	 This means that a 1 bit in the image corresponds to
	 `fg' only if `fg.pixel' is odd.

	 (This also means that the image will be all the same
	 color if both `fg' and `bg' are odd or even, but we can
	 safely assume that that won't happen if the XPM file is
	 sensible I think.)

	 The desired result is that the image use `1' to
	 represent the foreground color, and `0' to represent
	 the background color.  So, we may need to invert the
	 image to accomplish this; we invert if fg is
	 odd. (Remember that WhitePixel and BlackPixel are not
	 necessarily 1 and 0 respectively, though I think it
	 might be safe to assume that one of them is always 1
	 and the other is always 0.  We also pretty much need to
	 assume that one is even and the other is odd.)
      */

      fg.pixel = pixels[0];	/* pick a pixel at random. */
      bg.pixel = fg.pixel;
      for (i = 1; i < npixels; i++) /* Look for an "other" pixel value.*/
	{
	  bg.pixel = pixels[i];
	  if (fg.pixel != bg.pixel)
	    break;
	}

      /* If (fg.pixel == bg.pixel) then probably something has
	 gone wrong, but I don't think signalling an error would
	 be appropriate. */

      XQueryColor (dpy, cmap, &fg);
      XQueryColor (dpy, cmap, &bg);

      /* If the foreground is lighter than the background, swap them.
	 (This occurs semi-randomly, depending on the ordering of the
	 color list in the XPM file.)
      */
      {
	unsigned short fg_total = ((fg.red / 3) + (fg.green / 3)
				   + (fg.blue / 3));
	unsigned short bg_total = ((bg.red / 3) + (bg.green / 3)
				   + (bg.blue / 3));
	if (fg_total > bg_total)
	  {
	    XColor swap;
	    swap = fg;
	    fg = bg;
	    bg = swap;
	  }
      }

      /* If the fg pixel corresponds to a `0' in the bitmap, invert it.
	 (This occurs (only?) on servers with Black=0, White=1.)
      */
      if ((fg.pixel & 1) == 0)
	{
	  XGCValues gcv;
	  GC gc;
	  gcv.function = GXxor;
	  gcv.foreground = 1;
	  gc = XCreateGC (dpy, pixmap, (GCFunction | GCForeground),
			  &gcv);
	  XFillRectangle (dpy, pixmap, gc, 0, 0, w, h);
	  XFreeGC (dpy, gc);
	}
    }
  else
    {
      generate_cursor_fg_bg (device, &pointer_fg, &pointer_bg,
			     &fg, &bg);
      IMAGE_INSTANCE_PIXMAP_FG (ii) = pointer_fg;
      IMAGE_INSTANCE_PIXMAP_BG (ii) = pointer_bg;
    }

  IMAGE_INSTANCE_X_CURSOR (ii) =
    XCreatePixmapCursor
    (dpy, pixmap, mask, &fg, &bg, xhot, yhot);
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
*/

static void
init_image_instance_from_x_image (Lisp_Image_Instance *ii,
				  XImage *ximage,
				  int dest_mask,
				  Colormap cmap,
				  unsigned long *pixels,
				  int npixels,
				  int slices,
				  Lisp_Object instantiator,
				  Lisp_Object pointer_fg,
				  Lisp_Object pointer_bg)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  GC gc;
  Drawable d;
  Pixmap pixmap;
  enum image_instance_type type;

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  d = XtWindow (DEVICE_XT_APP_SHELL (XDEVICE (device)));

  if (dest_mask & IMAGE_COLOR_PIXMAP_MASK)
    type = IMAGE_COLOR_PIXMAP;
  else if (dest_mask & IMAGE_POINTER_MASK)
    type = IMAGE_POINTER;
  else
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK
			      | IMAGE_POINTER_MASK);

  pixmap = XCreatePixmap (dpy, d, ximage->width,
			  ximage->height, ximage->depth);
  if (!pixmap)
    gui_error ("Unable to create pixmap", instantiator);

  gc = XCreateGC (dpy, pixmap, 0, NULL);
  if (!gc)
    {
      XFreePixmap (dpy, pixmap);
      gui_error ("Unable to create GC", instantiator);
    }

  XPutImage (dpy, pixmap, gc, ximage, 0, 0, 0, 0,
	     ximage->width, ximage->height);

  XFreeGC (dpy, gc);

  x_initialize_pixmap_image_instance (ii, slices, IMAGE_COLOR_PIXMAP);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  /* Fixup a set of pixmaps. */
  IMAGE_INSTANCE_X_PIXMAP (ii) = pixmap;

  IMAGE_INSTANCE_PIXMAP_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = ximage->width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = ximage->height;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = ximage->depth;
  IMAGE_INSTANCE_X_COLORMAP (ii) = cmap;
  IMAGE_INSTANCE_X_PIXELS (ii) = pixels;
  IMAGE_INSTANCE_X_NPIXELS (ii) = npixels;

  if (type == IMAGE_POINTER)
    image_instance_convert_to_pointer (ii, instantiator, pointer_fg,
				       pointer_bg);
}

static void
image_instance_add_x_image (Lisp_Image_Instance *ii,
			    XImage *ximage,
			    int slice,
			    Lisp_Object instantiator)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  GC gc;
  Drawable d;
  Pixmap pixmap;

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  d = XtWindow(DEVICE_XT_APP_SHELL (XDEVICE (device)));

  pixmap = XCreatePixmap (dpy, d, ximage->width,
			  ximage->height, ximage->depth);
  if (!pixmap)
    gui_error ("Unable to create pixmap", instantiator);

  gc = XCreateGC (dpy, pixmap, 0, NULL);
  if (!gc)
    {
      XFreePixmap (dpy, pixmap);
      gui_error ("Unable to create GC", instantiator);
    }

  XPutImage (dpy, pixmap, gc, ximage, 0, 0, 0, 0,
	     ximage->width, ximage->height);

  XFreeGC (dpy, gc);

  IMAGE_INSTANCE_X_PIXMAP_SLICE (ii, slice) = pixmap;
}

static void
x_init_image_instance_from_eimage (Lisp_Image_Instance *ii,
				   int width, int height,
				   int slices,
				   Binbyte *eimage,
				   int dest_mask,
				   Lisp_Object instantiator,
				   Lisp_Object pointer_fg,
				   Lisp_Object pointer_bg,
				   Lisp_Object UNUSED (domain))
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Colormap cmap = DEVICE_X_COLORMAP (XDEVICE(device));
  unsigned long *pixtbl = NULL;
  int npixels = 0;
  int slice;
  XImage* ximage;

  for (slice = 0; slice < slices; slice++)
    {
      ximage = convert_EImage_to_XImage (device, width, height,
					 eimage + (width * height * 3 * slice),
					 &pixtbl, &npixels);
      if (!ximage)
	{
	  if (pixtbl)
	    xfree (pixtbl);
	  signal_image_error ("EImage to XImage conversion failed",
			      instantiator);
	}

      /* Now create the pixmap and set up the image instance */
      if (slice == 0)
	init_image_instance_from_x_image (ii, ximage, dest_mask,
					  cmap, pixtbl, npixels, slices,
					  instantiator, pointer_fg,
					  pointer_bg);
      else
	image_instance_add_x_image (ii, ximage, slice, instantiator);

      if (ximage)
	{
	  if (ximage->data)
	    {
	      xfree (ximage->data);
	      ximage->data = 0;
	    }
	  XDestroyImage (ximage);
	  ximage = 0;
	}
    }
}

/* Given inline data for a mono pixmap, create and return the
   corresponding X object. */

static Pixmap
pixmap_from_xbm_inline (Lisp_Object device, int width, int height,
			CBinbyte *bits)
{
  return XCreatePixmapFromBitmapData (DEVICE_X_DISPLAY (XDEVICE (device)),
				      XtWindow (DEVICE_XT_APP_SHELL
						(XDEVICE (device))),
				      bits, width, height,
				      1, 0, 1);
}

/* Given inline data for a mono pixmap, initialize the given
   image instance accordingly. */

static void
init_image_instance_from_xbm_inline (Lisp_Image_Instance *ii,
				     int width, int height,
				     CBinbyte *bits,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     Pixmap mask,
				     Lisp_Object UNUSED (mask_filename))
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object foreground = find_keyword_in_vector (instantiator, Q_foreground);
  Lisp_Object background = find_keyword_in_vector (instantiator, Q_background);
  Display *dpy;
  Screen *scr;
  Drawable draw;
  enum image_instance_type type;

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  draw = XtWindow(DEVICE_XT_APP_SHELL (XDEVICE (device)));
  scr = DefaultScreenOfDisplay (dpy);

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

  x_initialize_pixmap_image_instance (ii, 1, type);
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = height;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  switch (type)
    {
    case IMAGE_MONO_PIXMAP:
      {
	IMAGE_INSTANCE_X_PIXMAP (ii) =
	  pixmap_from_xbm_inline (device, width, height, bits);
      }
      break;

    case IMAGE_COLOR_PIXMAP:
      {
	Dimension d = DEVICE_X_DEPTH (XDEVICE(device));
	unsigned long fg = BlackPixelOfScreen (scr);
	unsigned long bg = WhitePixelOfScreen (scr);

	if (!NILP (foreground) && !COLOR_INSTANCEP (foreground))
	  foreground =
	    Fmake_color_instance (foreground, device,
				  encode_error_behavior_flag (ERROR_ME));

	if (COLOR_INSTANCEP (foreground))
	  fg = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (foreground)).pixel;

	if (!NILP (background) && !COLOR_INSTANCEP (background))
	  background =
	    Fmake_color_instance (background, device,
				  encode_error_behavior_flag (ERROR_ME));

	if (COLOR_INSTANCEP (background))
	  bg = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (background)).pixel;

	/* We used to duplicate the pixels using XAllocColor(), to protect
	   against their getting freed.  Just as easy to just store the
	   color instances here and GC-protect them, so this doesn't
	   happen. */
	IMAGE_INSTANCE_PIXMAP_FG (ii) = foreground;
	IMAGE_INSTANCE_PIXMAP_BG (ii) = background;
	IMAGE_INSTANCE_X_PIXMAP (ii) =
	  XCreatePixmapFromBitmapData (dpy, draw,
				       (CBinbyte *) bits, width, height,
				       fg, bg, d);
	IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = d;
      }
      break;

    case IMAGE_POINTER:
    {
	XColor fg_color, bg_color;
	Pixmap source;

	check_pointer_sizes (scr, width, height, instantiator);

	source =
	  XCreatePixmapFromBitmapData (dpy, draw,
				       (CBinbyte *) bits, width, height,
				       1, 0, 1);

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
	IMAGE_INSTANCE_X_CURSOR (ii) =
	  XCreatePixmapCursor
	    (dpy, source, mask, &fg_color, &bg_color,
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
		   CBinbyte *bits)
{
  Lisp_Object mask_data = find_keyword_in_vector (instantiator, Q_mask_data);
  Lisp_Object mask_file = find_keyword_in_vector (instantiator, Q_mask_file);
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Pixmap mask = 0;

  if (!NILP (mask_data))
    {
      CBinbyte *ext_data;

      ext_data = LISP_STRING_TO_EXTERNAL (XCAR (XCDR (XCDR (mask_data))), 
					  Qbinary);
      mask = pixmap_from_xbm_inline (IMAGE_INSTANCE_DEVICE (ii),
				     XFIXNUM (XCAR (mask_data)),
				     XFIXNUM (XCAR (XCDR (mask_data))),
				     ext_data);
    }

  init_image_instance_from_xbm_inline (ii, width, height, bits,
				       instantiator, pointer_fg, pointer_bg,
				       dest_mask, mask, mask_file);
}

/* Instantiate method for XBM's. */

static void
x_xbm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object UNUSED (domain))
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  CBinbyte *ext_data;

  assert (!NILP (data));

  ext_data = LISP_STRING_TO_EXTERNAL (XCAR (XCDR (XCDR (data))), Qbinary);

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, XFIXNUM (XCAR (data)),
		     XFIXNUM (XCAR (XCDR (data))), ext_data);
}


#ifdef HAVE_XPM

/**********************************************************************
 *                             XPM                                    *
 **********************************************************************/
 /* xpm 3.2g and better has XpmCreatePixmapFromBuffer()...
    There was no version number in xpm.h before 3.3, but this should do.
  */
#if (XpmVersion >= 3) || defined(XpmExactColors)
# define XPM_DOES_BUFFERS
#endif

#ifndef XPM_DOES_BUFFERS
Your version of XPM is too old.  You cannot compile with it.
Upgrade to version 3.2g or better or compile with --with-xpm=no.
#endif /* !XPM_DOES_BUFFERS */

static XpmColorSymbol *
extract_xpm_color_names (XpmAttributes *xpmattrs, Lisp_Object device,
			 Lisp_Object domain,
			 Lisp_Object color_symbol_alist)
{
  /* This function can GC */
  Display *dpy =  DEVICE_X_DISPLAY (XDEVICE(device));
  Colormap cmap = DEVICE_X_COLORMAP (XDEVICE(device));
  XColor color;
  Lisp_Object rest;
  Lisp_Object results = Qnil;
  int i;
  XpmColorSymbol *symbols;
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

  if (i == 0) return 0;

  symbols = xnew_array (XpmColorSymbol, i);
  xpmattrs->valuemask |= XpmColorSymbols;
  xpmattrs->colorsymbols = symbols;
  xpmattrs->numsymbols = i;

  while (--i >= 0)
    {
      Lisp_Object cons = XCAR (results);
      color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (XCDR (cons)));
      /* Duplicate the pixel value so that we still have a lock on it if
	 the pixel we were passed is later freed. */
      assert (XAllocColor (dpy, cmap, &color));  /* it must be allocable since we're just duplicating it */

      
      symbols[i].name = LISP_STRING_TO_EXTERNAL_MALLOC (XCAR (cons), Qctext);
      symbols[i].pixel = color.pixel;
      symbols[i].value = 0;
      free_cons (cons);
      cons = results;
      results = XCDR (results);
      free_cons (cons);
    }
  return symbols;
}

static void
xpm_free (XpmAttributes *xpmattrs)
{
  /* Could conceivably lose if XpmXXX returned an error without first
     initializing this structure, if we didn't know that initializing it
     to all zeros was ok (and also that it's ok to call XpmFreeAttributes()
     multiple times, since it zeros slots as it frees them...) */
  XpmFreeAttributes (xpmattrs);
}

static void
x_xpm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  Display *dpy;
  Screen *xs;
  Colormap cmap;
  int depth;
  Visual *visual;
  Pixmap pixmap;
  Pixmap mask = 0;
  XpmAttributes xpmattrs;
  int result;
  XpmColorSymbol *color_symbols;
  Lisp_Object color_symbol_alist = find_keyword_in_vector (instantiator,
							   Q_color_symbols);
  enum image_instance_type type;
  int force_mono;
  int w, h;

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  xs = DefaultScreenOfDisplay (dpy);

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

#if 1
  /* Although I haven't found it documented yet, it appears that pointers are
     always colored via the default window colormap... Sigh. */
  if (type == IMAGE_POINTER)
    {
      cmap = DefaultColormap (dpy, DefaultScreen (dpy));
      depth = DefaultDepthOfScreen (xs);
      visual = DefaultVisualOfScreen (xs);
    }
  else
    {
      cmap = DEVICE_X_COLORMAP (XDEVICE (device));
      depth = DEVICE_X_DEPTH (XDEVICE (device));
      visual = DEVICE_X_VISUAL (XDEVICE (device));
    }
#else
  cmap = DEVICE_X_COLORMAP (XDEVICE (device));
  depth = DEVICE_X_DEPTH (XDEVICE (device));
  visual = DEVICE_X_VISUAL (XDEVICE (device));
#endif

  x_initialize_pixmap_image_instance (ii, 1, type);

  assert (!NILP (data));

 retry:

  xzero (xpmattrs); /* want XpmInitAttributes() */
  xpmattrs.valuemask = XpmReturnPixels;
  if (force_mono)
    {
      /* Without this, we get a 1-bit version of the color image, which
	 isn't quite right.  With this, we get the mono image, which might
	 be very different looking. */
      xpmattrs.valuemask |= XpmColorKey;
      xpmattrs.color_key = XPM_MONO;
      xpmattrs.depth = 1;
      xpmattrs.valuemask |= XpmDepth;
    }
  else
    {
      xpmattrs.closeness = 65535;
      xpmattrs.valuemask |= XpmCloseness;
      xpmattrs.depth = depth;
      xpmattrs.valuemask |= XpmDepth;
      xpmattrs.visual = visual;
      xpmattrs.valuemask |= XpmVisual;
      xpmattrs.colormap = cmap;
      xpmattrs.valuemask |= XpmColormap;
    }

  color_symbols = extract_xpm_color_names (&xpmattrs, device, domain,
					   color_symbol_alist);

  {
    Extbyte *dataext;

    dataext = LISP_STRING_TO_EXTERNAL (data, Qctext);

    result =
      XpmCreatePixmapFromBuffer (dpy,
				 XtWindow
				 (DEVICE_XT_APP_SHELL (XDEVICE(device))),
				 dataext, &pixmap, &mask, &xpmattrs);
  }

  if (color_symbols)
    {
      int i;

      for (i = 0; i < (int) xpmattrs.numsymbols; i++)
	xfree (color_symbols[i].name);
      xfree (color_symbols);
      xpmattrs.colorsymbols = 0; /* in case XpmFreeAttr is too smart... */
      xpmattrs.numsymbols = 0;
    }

  switch (result)
    {
    case XpmSuccess:
      break;
    case XpmFileInvalid:
      {
	xpm_free (&xpmattrs);
	signal_image_error ("invalid XPM data", data);
      }
    case XpmColorFailed:
    case XpmColorError:
      {
	xpm_free (&xpmattrs);
	if (force_mono)
	  {
	    /* second time; blow out. */
	    gui_error ("XPM color allocation failed", data);
	  }
	else
	  {
	    /* second time; blow out. */
	    if (! (dest_mask & IMAGE_MONO_PIXMAP_MASK))
	      gui_error ("XPM color allocation failed", data);
	    force_mono = 1;
	    IMAGE_INSTANCE_TYPE (ii) = IMAGE_MONO_PIXMAP;
	    goto retry;
	  }
      }
    case XpmNoMemory:
      {
	xpm_free (&xpmattrs);
	out_of_memory ("Parsing pixmap data", data);
      }
    default:
      {
	xpm_free (&xpmattrs);
	signal_error_2 (Qgui_error,
			"Parsing pixmap data: unknown error code",
			make_fixnum (result), data);
      }
    }

  w = xpmattrs.width;
  h = xpmattrs.height;

  {
    int npixels = xpmattrs.npixels;
    Pixel *pixels;

    if (npixels != 0)
      {
	pixels = xnew_array (Pixel, npixels);
	memcpy (pixels, xpmattrs.pixels, npixels * sizeof (Pixel));
      }
    else
      pixels = NULL;

    IMAGE_INSTANCE_X_PIXMAP (ii) = pixmap;
    IMAGE_INSTANCE_PIXMAP_MASK (ii) = (void*)mask;
    IMAGE_INSTANCE_X_COLORMAP (ii) = cmap;
    IMAGE_INSTANCE_X_PIXELS (ii) = pixels;
    IMAGE_INSTANCE_X_NPIXELS (ii) = npixels;
    IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = w;
    IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = h;
    IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
      find_keyword_in_vector (instantiator, Q_file);
  }

  switch (type)
    {
    case IMAGE_MONO_PIXMAP:
      break;

    case IMAGE_COLOR_PIXMAP:
      IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = depth;
      break;

    case IMAGE_POINTER:
      if (xpmattrs.valuemask & XpmHotspot)
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii) = make_fixnum (xpmattrs.x_hotspot);
      if (xpmattrs.valuemask & XpmHotspot)
	IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii) = make_fixnum (xpmattrs.y_hotspot);

      image_instance_convert_to_pointer (ii, instantiator, pointer_fg,
					 pointer_bg);
      break;

    default:
      ABORT ();
    }

  xpm_free (&xpmattrs);	/* after we've read pixels and hotspot */
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
x_xface_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		     int dest_mask, Lisp_Object UNUSED (domain))
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  int i, stattis;
  Binbyte *p, *bits, *bp;
  const CIbyte * volatile emsg = 0;
  const Binbyte * volatile dstring;

  assert (!NILP (data));

  dstring = (const Binbyte *) LISP_STRING_TO_EXTERNAL (data, Qbinary);

  if ((p = (Binbyte *) strchr ((char *) dstring, ':')))
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
    signal_image_error_2 (emsg, data, Qimage);

  bp = bits = alloca_binbytes (PIXELS / 8);

  /* the compface library exports char F[], which uses a single byte per
     pixel to represent a 48x48 bitmap.  Yuck. */
  for (i = 0, p = (Binbyte *) F; i < (PIXELS / 8); ++i)
    {
      int n, b;
      /* reverse the bit order of each byte... */
      for (b = n = 0; b < 8; ++b)
	{
	  n |= ((*p++) << b);
	}
      *bp++ = (Binbyte) n;
    }

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, 48, 48, (CBinbyte *) bits);
}

#endif /* HAVE_XFACE */


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
      LISP_PATHNAME_CONVERT_OUT (data, name_ext);
      if (XmuCursorNameToIndex (name_ext) != -1)
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

/* XmuCvtStringToCursor is bogus in the following ways:

   - When it can't convert the given string to a real cursor, it will
     sometimes return a "success" value, after triggering a BadPixmap
     error.  It then gives you a cursor that will itself generate BadCursor
     errors.  So we install this error handler to catch/notice the X error
     and take that as meaning "couldn't convert."

   - When you tell it to find a cursor file that doesn't exist, it prints
     an error message on stderr.  You can't make it not do that.

   - Also, using Xmu means we can't properly hack Lisp_Image_Instance
     objects, or XPM files, or $XBMLANGPATH.
 */

/* Duplicate the behavior of XmuCvtStringToCursor() to bypass its bogusness. */

static int XLoadFont_got_error;

static int
XLoadFont_error_handler (Display *UNUSED (dpy), XErrorEvent *UNUSED (xerror))
{
  XLoadFont_got_error = 1;
  return 0;
}

static Font
safe_XLoadFont (Display *dpy, Ibyte *name)
{
  Font font;
  int (*old_handler) (Display *, XErrorEvent *);

  XLoadFont_got_error = 0;
  XSync (dpy, 0);
  old_handler = XSetErrorHandler (XLoadFont_error_handler);
  font = XLoadFont (dpy, ITEXT_TO_EXTERNAL (name, Qfile_name));
  XSync (dpy, 0);
  XSetErrorHandler (old_handler);
  if (XLoadFont_got_error) return 0;
  return font;
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
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  XColor fg, bg;
  Font source, mask;
  Ibyte source_name[PATH_MAX_INTERNAL], mask_name[PATH_MAX_INTERNAL], dummy;
  int source_char, mask_char;
  int count;
  Lisp_Object foreground, background;

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

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
  if (count == 3 && (1 == sscanf ((char *) mask_name, "%d %c", &mask_char,
				  &dummy)))
    count = 4, mask_name[0] = 0;

  if (count != 2 && count != 4)
    syntax_error ("invalid cursor specification", data);
  source = safe_XLoadFont (dpy, source_name);
  if (! source)
    signal_error_2 (Qgui_error,
		    "couldn't load font", build_istring (source_name), data);
  if (count == 2)
    mask = 0;
  else if (!mask_name[0])
    mask = source;
  else
    {
      mask = safe_XLoadFont (dpy, mask_name);
      if (!mask)
	signal_continuable_error_2 (Qgui_error,
				    "couldn't load font",
				    build_istring (mask_name), data);
    }
  if (!mask)
    mask_char = 0;

  /* #### call XQueryTextExtents() and check_pointer_sizes() here. */

  x_initialize_pixmap_image_instance (ii, 1, IMAGE_POINTER);
  IMAGE_INSTANCE_X_CURSOR (ii) =
    XCreateGlyphCursor (dpy, source, mask, source_char, mask_char,
			&fg, &bg);
  XIMAGE_INSTANCE_PIXMAP_FG (image_instance) = foreground;
  XIMAGE_INSTANCE_PIXMAP_BG (image_instance) = background;
  XUnloadFont (dpy, source);
  if (mask && mask != source) XUnloadFont (dpy, mask);
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

static void
cursor_font_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			 int dest_mask, Lisp_Object UNUSED (domain))
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  int i;
  const Extbyte *name_ext;
  Lisp_Object foreground, background;

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  if (!(dest_mask & IMAGE_POINTER_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_POINTER_MASK);

  LISP_PATHNAME_CONVERT_OUT (data, name_ext);
  if ((i = XmuCursorNameToIndex (name_ext)) == -1)
    invalid_argument ("Unrecognized cursor-font name", data);

  x_initialize_pixmap_image_instance (ii, 1, IMAGE_POINTER);
  IMAGE_INSTANCE_X_CURSOR (ii) = XCreateFontCursor (dpy, i);
  foreground = find_keyword_in_vector (instantiator, Q_foreground);
  if (NILP (foreground))
    foreground = pointer_fg;
  background = find_keyword_in_vector (instantiator, Q_background);
  if (NILP (background))
    background = pointer_bg;
  maybe_recolor_cursor (image_instance, foreground, background);
}

static int
x_colorize_image_instance (Lisp_Object image_instance,
			   Lisp_Object foreground, Lisp_Object background)
{
  Lisp_Image_Instance *p;

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
    Display *dpy = DEVICE_X_DISPLAY (XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
    Drawable draw = XtWindow(DEVICE_XT_APP_SHELL (XDEVICE (IMAGE_INSTANCE_DEVICE (p))));
    Dimension d = DEVICE_X_DEPTH (XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
    Pixmap new_ = XCreatePixmap (dpy, draw,
				IMAGE_INSTANCE_PIXMAP_WIDTH (p),
				IMAGE_INSTANCE_PIXMAP_HEIGHT (p), d);
    XColor color;
    XGCValues gcv;
    GC gc;
    color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (foreground));
    gcv.foreground = color.pixel;
    color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (background));
    gcv.background = color.pixel;
    gc = XCreateGC (dpy, new_, GCBackground|GCForeground, &gcv);
    XCopyPlane (dpy, IMAGE_INSTANCE_X_PIXMAP (p), new_, gc, 0, 0,
		IMAGE_INSTANCE_PIXMAP_WIDTH (p),
		IMAGE_INSTANCE_PIXMAP_HEIGHT (p),
		0, 0, 1);
    XFreeGC (dpy, gc);
    IMAGE_INSTANCE_X_PIXMAP (p) = new_;
    IMAGE_INSTANCE_PIXMAP_DEPTH (p) = d;
    IMAGE_INSTANCE_PIXMAP_FG (p) = foreground;
    IMAGE_INSTANCE_PIXMAP_BG (p) = background;
    return 1;
  }
}


/************************************************************************/
/*                      subwindow and widget support                      */
/************************************************************************/

/* unmap the image if it is a widget. This is used by redisplay via
   redisplay_unmap_subwindows */
static void
x_unmap_subwindow (Lisp_Image_Instance *p)
{
  if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
    {
      XUnmapWindow
	(IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
	 IMAGE_INSTANCE_X_CLIPWINDOW (p));
      XUnmapSubwindows
	(IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
	 IMAGE_INSTANCE_X_CLIPWINDOW (p));
    }
  else				/* must be a widget */
    {
      /* Since we are being unmapped we want the enclosing frame to
	 get focus. The losing with simple scrolling but is the safest
	 thing to do. */
      emacs_Xt_handle_widget_losing_focus
	( XFRAME (IMAGE_INSTANCE_FRAME (p)),
	  IMAGE_INSTANCE_X_WIDGET_ID (p));
      XtUnmapWidget (IMAGE_INSTANCE_X_CLIPWIDGET (p));
    }
}

/* map the subwindow. This is used by redisplay via
   redisplay_output_subwindow */
static void
x_map_subwindow (Lisp_Image_Instance *p, int x, int y,
		 struct display_glyph_area* dga)
{
  assert (dga->width > 0 && dga->height > 0);
  if (IMAGE_INSTANCE_TYPE (p) == IMAGE_SUBWINDOW)
    {
      Window subwindow = IMAGE_INSTANCE_X_SUBWINDOW_ID (p);
      XMoveResizeWindow (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
			 IMAGE_INSTANCE_X_CLIPWINDOW (p),
			 x, y, dga->width, dga->height);
      XMoveWindow (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
		   subwindow, -dga->xoffset, -dga->yoffset);
      if (!IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (p))
	{
	  XMapWindow (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
		      IMAGE_INSTANCE_X_CLIPWINDOW (p));
	  XMapSubwindows (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
			  IMAGE_INSTANCE_X_CLIPWINDOW (p));
	}
    }
  else				/* must be a widget */
    {
      XtConfigureWidget (IMAGE_INSTANCE_X_CLIPWIDGET (p),
			 x + IMAGE_INSTANCE_X_WIDGET_XOFFSET (p),
			 y + IMAGE_INSTANCE_X_WIDGET_YOFFSET (p),
			 dga->width, dga->height, 0);
      XtMoveWidget (IMAGE_INSTANCE_X_WIDGET_ID (p),
		    -dga->xoffset, -dga->yoffset);
      if (!IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (p))
	XtMapWidget (IMAGE_INSTANCE_X_CLIPWIDGET (p));
      /* See comments in glyphs-msw.c about keyboard focus. */
      if (IMAGE_INSTANCE_WANTS_INITIAL_FOCUS (p))
	{
	  /* #### FIXME to pop-up the find dialog we map the text-field
	     seven times! This doesn't show on a fast linux box but does
	     under X on windows. */
	  emacs_Xt_enqueue_focus_event (IMAGE_INSTANCE_X_WIDGET_ID (p),
					IMAGE_INSTANCE_FRAME (p), 1);
	}
    }
}

/* when you click on a widget you may activate another widget this
   needs to be checked and all appropriate widgets updated */
static void
x_redisplay_subwindow (Lisp_Image_Instance *p)
{
  /* Update the subwindow size if necessary. */
  if (IMAGE_INSTANCE_SIZE_CHANGED (p))
    {
      XResizeWindow (IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (p),
		     IMAGE_INSTANCE_X_SUBWINDOW_ID (p),
		     IMAGE_INSTANCE_WIDTH (p),
		     IMAGE_INSTANCE_HEIGHT (p));
    }
}

/* Update all attributes that have changed. Lwlib actually does most
   of this for us. */
static void
x_redisplay_widget (Lisp_Image_Instance *p)
{
  /* This function can GC if IN_REDISPLAY is false. */
#ifdef HAVE_X_WIDGETS
  widget_value* wv = 0;

  /* First get the items if they have changed since this is a
     structural change. As such it will nuke all added values so we
     need to update most other things after the items have changed.*/
  if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p))
    {
      Lisp_Object image_instance = wrap_image_instance (p);

      wv = gui_items_to_widget_values
	(image_instance, IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (p),
	 /* #### this is not right; we need to keep track of which widgets
	    want accelerators and which don't */ 0);
      wv->change = STRUCTURAL_CHANGE;
    }
  else
    {
      /* Assume the lotus position, breath deeply and chant to
	 yourself lwlibsux, lwlibsux ... lw_get_all_values returns a
	 reference to the real values rather than a copy thus any
	 changes we make to the values we get back will look like they
	 have already been applied. If we rebuild the widget tree then
	 we may lose properties. */
      wv = copy_widget_value_tree (lw_get_all_values
				   (IMAGE_INSTANCE_X_WIDGET_LWID (p)),
				   NO_CHANGE);
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
      update_widget_face (wv, p, IMAGE_INSTANCE_FRAME (p));
    }

  /* Possibly update the text. */
  if (IMAGE_INSTANCE_TEXT_CHANGED (p))
    {
      Lisp_Object val = IMAGE_INSTANCE_WIDGET_TEXT (p);
      if (wv->value) free (wv->value);
      wv->value = LISP_STRING_TO_EXTERNAL_MALLOC (val, Qlwlib_encoding);
    }

  /* Possibly update the size. */
  if (IMAGE_INSTANCE_SIZE_CHANGED (p)
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p)
      ||
      IMAGE_INSTANCE_TEXT_CHANGED (p))
    {
      assert (IMAGE_INSTANCE_X_WIDGET_ID (p) &&
	      IMAGE_INSTANCE_X_CLIPWIDGET (p)) ;

      if (IMAGE_INSTANCE_X_WIDGET_ID (p)->core.being_destroyed
	  || !XtIsManaged(IMAGE_INSTANCE_X_WIDGET_ID (p)))
	{
	  Lisp_Object sw = wrap_image_instance (p);

	  signal_error (Qinternal_error,
			     "XEmacs bug: subwindow is deleted", sw);
	}

      lw_add_widget_value_arg (wv, XtNwidth,
			       (Dimension)IMAGE_INSTANCE_WIDTH (p));
      lw_add_widget_value_arg (wv, XtNheight,
			       (Dimension)IMAGE_INSTANCE_HEIGHT (p));
    }

  /* Adjust offsets within the frame. */
  if (XFRAME (IMAGE_INSTANCE_FRAME (p))->size_changed)
    {
      Arg al[2];
      Xt_SET_ARG (al [0], XtNx, &IMAGE_INSTANCE_X_WIDGET_XOFFSET (p));
      Xt_SET_ARG (al [1], XtNy, &IMAGE_INSTANCE_X_WIDGET_YOFFSET (p));
      XtGetValues (FRAME_X_TEXT_WIDGET
		   (XFRAME (IMAGE_INSTANCE_FRAME (p))), al, 2);
    }

  /* now modify the widget */
  lw_modify_all_widgets (IMAGE_INSTANCE_X_WIDGET_LWID (p), wv, True);
  free_widget_value_tree (wv);
  gcpro_popup_callbacks (IMAGE_INSTANCE_X_WIDGET_LWID (p));
#endif
}

/* instantiate and x type subwindow */
static void
x_subwindow_instantiate (Lisp_Object image_instance,
			 Lisp_Object UNUSED (instantiator),
			 Lisp_Object UNUSED (pointer_fg),
			 Lisp_Object UNUSED (pointer_bg),
			 int UNUSED (dest_mask), Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object frame = DOMAIN_FRAME (domain);
  struct frame* f = XFRAME (frame);
  Display *dpy;
  Screen *xs;
  Window pw, win;
  XSetWindowAttributes xswa;
  Mask valueMask = 0;
  int w = IMAGE_INSTANCE_WIDTH (ii), h = IMAGE_INSTANCE_HEIGHT (ii);

  if (!DEVICE_X_P (XDEVICE (device)))
    gui_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  xs = DefaultScreenOfDisplay (dpy);

  IMAGE_INSTANCE_TYPE (ii) = IMAGE_SUBWINDOW;

  pw = XtWindow (FRAME_X_TEXT_WIDGET (f));

  ii->data = xnew_and_zero (struct x_subwindow_data);

  IMAGE_INSTANCE_X_SUBWINDOW_PARENT (ii) = pw;
  IMAGE_INSTANCE_X_SUBWINDOW_DISPLAY (ii) = DisplayOfScreen (xs);

  xswa.backing_store = Always;
  valueMask |= CWBackingStore;
  xswa.colormap = DefaultColormapOfScreen (xs);
  valueMask |= CWColormap;

  /* Create a window for clipping */
  IMAGE_INSTANCE_X_CLIPWINDOW (ii) =
    XCreateWindow (dpy, pw, 0, 0, w, h, 0, CopyFromParent,
		   InputOutput, CopyFromParent, valueMask,
		   &xswa);

  /* Now put the subwindow inside the clip window. */
  win = XCreateWindow (dpy, IMAGE_INSTANCE_X_CLIPWINDOW (ii),
		       0, 0, w, h, 0, CopyFromParent,
		       InputOutput, CopyFromParent, valueMask,
		       &xswa);

  IMAGE_INSTANCE_SUBWINDOW_ID (ii) = (void*)win;
}

/* Account for some of the limitations with widget images. */
static int
x_widget_border_width (void)
{
  return DEFAULT_WIDGET_BORDER_WIDTH * 2;
}


#if 0
/* #### Should this function exist? If there's any doubt I'm not implementing it --andyp */
DEFUN ("change-subwindow-property", Fchange_subwindow_property, 3, 3, 0, /*
For the given SUBWINDOW, set PROPERTY to DATA, which is a string.
Subwindows are not currently implemented.
*/
       (subwindow, property, data))
{
  Atom property_atom;
  Lisp_Subwindow *sw;
  Display *dpy;
  Extbyte *propext, *dataext;
  Bytecount datalen;

  CHECK_SUBWINDOW (subwindow);
  CHECK_STRING (property);
  CHECK_STRING (data);

  sw = XSUBWINDOW (subwindow);
  dpy = DisplayOfScreen (LISP_DEVICE_TO_X_SCREEN
			 (FRAME_DEVICE (XFRAME (sw->frame))));

  LISP_TO_EXTERNAL (property, propext, Qctext);
  LISP_STRING_TO_SIZED_EXTERNAL (data, dataext, datalen, Qctext); 
  property_atom = XInternAtom (dpy, propext, False);
  XChangeProperty (dpy, sw->subwindow, property_atom, XA_STRING, 8,
		   PropModeReplace, dataext, datalen);
  return property;
}
#endif


#ifdef HAVE_X_WIDGETS

/************************************************************************/
/*                                widgets                               */
/************************************************************************/

static Lisp_Object
very_bogusly_return_only_the_first_needed_font (Lisp_Object string,
						Lisp_Object face,
						Lisp_Object domain)
{
  int i;

  struct face_cachel frame_cachel;
  struct face_cachel *cachel;
  Lisp_Object frame = DOMAIN_FRAME (domain);
  Ichar_dynarr *buf = Dynarr_new (Ichar);

  convert_ibyte_string_into_ichar_dynarr
    (XSTRING_DATA (string), XSTRING_LENGTH (string), buf);

  reset_face_cachel (&frame_cachel);
  update_face_cachel_data (&frame_cachel, frame, face);
  cachel = &frame_cachel;

  ensure_face_cachel_complete (cachel, domain,
			       Dynarr_atp (buf, 0),
			       Dynarr_length (buf));

  Dynarr_free (buf);

  /* @@#### This is majorly bogus.  We are just returning the first font
     we find, which will be wrong when there are multiple fonts needed. */
  for (i = 0; i < Stynarr_length (cachel->font); i++)
    {
      Lisp_Object font = Stynarr_at (cachel->font, i).value;
      assert (!UNBOUNDP (font));
      return font;
    }

  return Qnil;			/* NOT REACHED */
}

static void
update_widget_face (widget_value* wv, Lisp_Image_Instance *ii,
		    Lisp_Object domain)
{
#ifdef LWLIB_WIDGETS_MOTIF
  XmFontList fontList;
#endif
  /* Update the foreground. */
  Lisp_Object pixel = FACE_FOREGROUND
    (IMAGE_INSTANCE_WIDGET_FACE (ii),
     domain);
  XColor fcolor = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (pixel)), bcolor;
  lw_add_widget_value_arg (wv, XtNforeground, fcolor.pixel);

  /* Update the background. */
  pixel = FACE_BACKGROUND (IMAGE_INSTANCE_WIDGET_FACE (ii),
			   domain);
  bcolor = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (pixel));
  lw_add_widget_value_arg (wv, XtNbackground, bcolor.pixel);

  {
    Lisp_Object face = IMAGE_INSTANCE_WIDGET_FACE (ii);
    /* @@#### Fix me.  This should extract all fonts.  These should somehow
       get incorporated into the fontList below in XmFontListCreate(),
       and there should be a way of passing multiple fonts into lwlib. */
    Lisp_Font_Instance *fi =
      XFONT_INSTANCE (very_bogusly_return_only_the_first_needed_font
		      (IMAGE_INSTANCE_WIDGET_TEXT (ii), face, domain));
    XFontStruct *fs = FONT_INSTANCE_X_FONT (fi);
#ifdef HAVE_XFT
    XftFont *rf = FONT_INSTANCE_X_XFTFONT (fi);

    if (rf)
      {
	/* #### What to do about Motif? */
	lw_add_widget_value_arg (wv, (String) XtNxftFont, (XtArgVal) rf);
      }
#endif

    if (fs)
      {
#ifdef LWLIB_WIDGETS_MOTIF
	fontList = XmFontListCreate (fs, XmSTRING_DEFAULT_CHARSET);
	lw_add_widget_value_arg (wv, XmNfontList, (XtArgVal) fontList);
#endif
	lw_add_widget_value_arg (wv, XtNfont, (XtArgVal) fs);
      }

#ifdef HAVE_XFT
    /* #### sanity check, should wrap in appropriate ERROR_CHECK macro */
    if (!rf && !fs)
      warn_when_safe_lispobj
	(intern ("xft"), Qdebug,
	 Fcons (build_msg_string ("missing font in update_widget_face"),
		Fface_name (face)));
#endif
  }
  wv->change = VISIBLE_CHANGE;
  /* #### Megahack - but its just getting too complicated to do this
     in the right place. */
  if (EQ (IMAGE_INSTANCE_WIDGET_TYPE (ii), Qtab_control))
    update_tab_widget_face (wv, ii, domain);
}

static void
update_tab_widget_face (widget_value* wv, Lisp_Image_Instance *ii,
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
      lw_add_widget_value_arg (val, (String) XtNtabForeground, fcolor.pixel);
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

static void
x_widget_instantiate (Lisp_Object image_instance,
		      Lisp_Object UNUSED (instantiator),
		      Lisp_Object UNUSED (pointer_fg),
		      Lisp_Object UNUSED (pointer_bg),
		      int UNUSED (dest_mask), Lisp_Object domain,
		      const char* type, widget_value* wv)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii), pixel;
  struct device* d = XDEVICE (device);
  Lisp_Object frame = DOMAIN_FRAME (domain);
  struct frame* f = XFRAME (frame);
  Widget wid;
  Arg al [32];
  int ac = 0;
  int id = new_lwlib_id ();
  widget_value* clip_wv;
  XColor fcolor, bcolor;

  if (!DEVICE_X_P (d))
    gui_error ("Not an X device", device);

  /* have to set the type this late in case there is no device
     instantiation for a widget. But we can go ahead and do it without
     checking because there is always a generic instantiator. */
  IMAGE_INSTANCE_TYPE (ii) = IMAGE_WIDGET;

  ii->data = xnew_and_zero (struct x_subwindow_data);

  /* Create a clip window to contain the subwidget. Incredibly the
     XEmacs manager seems to be the most appropriate widget for
     this. Nothing else is simple enough and yet does what is
     required. */
  clip_wv = xmalloc_widget_value ();

  lw_add_widget_value_arg (clip_wv, XtNresize, False);
  lw_add_widget_value_arg (clip_wv, XtNwidth,
			   (Dimension)IMAGE_INSTANCE_WIDTH (ii));
  lw_add_widget_value_arg (clip_wv, XtNheight,
			   (Dimension)IMAGE_INSTANCE_HEIGHT (ii));
  clip_wv->enabled = True;

  clip_wv->name = xstrdup ("clip-window");
  clip_wv->value = xstrdup ("clip-window");

  IMAGE_INSTANCE_X_CLIPWIDGET (ii)
    = lw_create_widget ("clip-window", "clip-window", new_lwlib_id (),
			clip_wv, FRAME_X_CONTAINER_WIDGET (f),
			False, 0, 0, 0);

  free_widget_value_tree (clip_wv);

  /* create a sensible name. */
  if (wv->name == 0 || strcmp(wv->name, "") == 0)
    wv->name = xstrdup (type);

  if (!NILP (IMAGE_INSTANCE_WIDGET_TEXT (ii)))
    {
      wv->value
        = LISP_STRING_TO_EXTERNAL_MALLOC (IMAGE_INSTANCE_WIDGET_TEXT (ii),
                                          Qlwlib_encoding);
    }

  /* copy any args we were given */
  ac = 0;
  lw_add_value_args_to_args (wv, al, &ac);

  /* Fixup the colors. We have to do this *before* the widget gets
     created so that Motif will fix up the shadow colors
     correctly. Once the widget is created Motif won't do this
     anymore...*/
  pixel = FACE_FOREGROUND
    (IMAGE_INSTANCE_WIDGET_FACE (ii),
     IMAGE_INSTANCE_FRAME (ii));
  fcolor = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (pixel));

  pixel = FACE_BACKGROUND
    (IMAGE_INSTANCE_WIDGET_FACE (ii),
     IMAGE_INSTANCE_FRAME (ii));
  bcolor = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (pixel));

  lw_add_widget_value_arg (wv, XtNbackground, bcolor.pixel);
  lw_add_widget_value_arg (wv, XtNforeground, fcolor.pixel);
  /* we cannot allow widgets to resize themselves */
  lw_add_widget_value_arg (wv, XtNresize, False);
  lw_add_widget_value_arg (wv, XtNwidth,
			   (Dimension)IMAGE_INSTANCE_WIDTH (ii));
  lw_add_widget_value_arg (wv, XtNheight,
			   (Dimension)IMAGE_INSTANCE_HEIGHT (ii));
  /* update the font. */
  update_widget_face (wv, ii, domain);

  wid = lw_create_widget (type, wv->name, id, wv,
			  IMAGE_INSTANCE_X_CLIPWIDGET (ii),
			  False, 0, popup_selection_callback, 0);

  IMAGE_INSTANCE_SUBWINDOW_ID (ii) = (void*)wid;
  IMAGE_INSTANCE_X_WIDGET_LWID (ii) = id;
  /* because the EmacsManager is the widgets parent we have to
     offset the redisplay of the widget by the amount the text
     widget is inside the manager. */
  ac = 0;
  Xt_SET_ARG (al [ac], XtNx, &IMAGE_INSTANCE_X_WIDGET_XOFFSET (ii)); ac++;
  Xt_SET_ARG (al [ac], XtNy, &IMAGE_INSTANCE_X_WIDGET_YOFFSET (ii)); ac++;
  XtGetValues (FRAME_X_TEXT_WIDGET (f), al, ac);

  XtSetMappedWhenManaged (wid, TRUE);

  free_widget_value_tree (wv);
  /* A kludgy but simple way to make sure the callback for a widget
     doesn't get deleted. */
  gcpro_popup_callbacks (id);
}

/* get properties of a control */
static Lisp_Object
x_widget_property (Lisp_Object image_instance, Lisp_Object prop)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  /* get the text from a control */
  if (EQ (prop, Q_text))
    {
      widget_value* wv = lw_get_all_values (IMAGE_INSTANCE_X_WIDGET_LWID (ii));
      return build_extstring (wv->value, Qlwlib_encoding);
    }
  return Qunbound;
}

/* Instantiate a layout control for putting other widgets in. */
static void
x_native_layout_instantiate (Lisp_Object image_instance,
			     Lisp_Object instantiator,
			     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			     int dest_mask, Lisp_Object domain)
{
  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "layout", 0);
}

/* Instantiate a button widget. Unfortunately instantiated widgets are
   particular to a frame since they need to have a parent. It's not
   like images where you just select the image into the context you
   want to display it in and BitBlt it. So images instances can have a
   many-to-one relationship with things you see, whereas widgets can
   only be one-to-one (i.e. per frame) */
static void
x_button_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		      Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		      int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object gui = IMAGE_INSTANCE_WIDGET_ITEM (ii);
  Lisp_Object glyph = find_keyword_in_vector (instantiator, Q_image);
  widget_value* wv = gui_items_to_widget_values (image_instance, gui, 1);

  if (!NILP (glyph))
    {
      if (!IMAGE_INSTANCEP (glyph))
	glyph = glyph_image_instance (glyph, domain, ERROR_ME, 1);
    }

  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "button", wv);

  /* add the image if one was given */
  if (!NILP (glyph) && IMAGE_INSTANCEP (glyph)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (glyph)))
    {
      Arg al [2];
      int ac =0;
#ifdef LWLIB_WIDGETS_MOTIF
      Xt_SET_ARG (al [ac], XmNlabelType, XmPIXMAP);	ac++;
      Xt_SET_ARG (al [ac], XmNlabelPixmap, XIMAGE_INSTANCE_X_PIXMAP (glyph));
      ac++;
#else
      Xt_SET_ARG (al [ac], XtNpixmap, XIMAGE_INSTANCE_X_PIXMAP (glyph));	ac++;
#endif
      XtSetValues (IMAGE_INSTANCE_X_WIDGET_ID (ii), al, ac);
    }
}

/* Update a button's clicked state.

   #### This is overkill, but it works. Right now this causes all
   button instances to flash for some reason buried deep in lwlib. In
   theory this should be the Right Thing to do since lwlib should only
   merge in changed values - and if nothing has changed then nothing
   should get done. This may be because of the args stuff,
   i.e. although the arg contents may be the same the args look
   different and so are re-applied to the widget. */
static void
x_button_redisplay (Lisp_Object image_instance)
{
  /* This function can GC if IN_REDISPLAY is false. */
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  widget_value* wv =
    gui_items_to_widget_values (image_instance,
				IMAGE_INSTANCE_WIDGET_ITEMS (p), 1);

  /* now modify the widget */
  lw_modify_all_widgets (IMAGE_INSTANCE_X_WIDGET_LWID (p), wv, True);
  free_widget_value_tree (wv);
  gcpro_popup_callbacks (IMAGE_INSTANCE_X_WIDGET_LWID (p));
}

/* get properties of a button */
static Lisp_Object
x_button_property (Lisp_Object image_instance, Lisp_Object prop)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  /* check the state of a button */
  if (EQ (prop, Q_selected))
    {
      widget_value* wv = lw_get_all_values (IMAGE_INSTANCE_X_WIDGET_LWID (ii));

      if (wv->selected)
	return Qt;
      else
	return Qnil;
    }
  return Qunbound;
}

/* instantiate a progress gauge */
static void
x_progress_gauge_instantiate (Lisp_Object image_instance,
			      Lisp_Object instantiator,
			      Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			      int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object gui = IMAGE_INSTANCE_WIDGET_ITEM (ii);
  widget_value* wv = gui_items_to_widget_values (image_instance, gui, 0);

  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "progress", wv);
}

/* set the properties of a progress gauge */
static void
x_progress_gauge_redisplay (Lisp_Object image_instance)
{
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);

  if (IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p))
    {
      Lisp_Object val;
      val = XGUI_ITEM (IMAGE_INSTANCE_WIDGET_PENDING_ITEMS (p))->value;
      Xt_SET_VALUE (IMAGE_INSTANCE_X_WIDGET_ID (p), XtNvalue, XFIXNUM (val));
    }
}

/* instantiate an edit control */
static void
x_edit_field_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			  int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object gui = IMAGE_INSTANCE_WIDGET_ITEM (ii);
  widget_value* wv = gui_items_to_widget_values (image_instance, gui, 0);

  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "text-field", wv);
}

#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
/* instantiate a combo control */
static void
x_combo_box_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			 int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  widget_value * wv = 0;
  /* This is not done generically because of sizing problems under
     mswindows. */
  widget_instantiate (image_instance, instantiator, pointer_fg,
		      pointer_bg, dest_mask, domain);

  wv = gui_items_to_widget_values (image_instance,
				   IMAGE_INSTANCE_WIDGET_ITEMS (ii), 0);

  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "combo-box", wv);
}
#endif

static void
x_tab_control_instantiate (Lisp_Object image_instance,
			   Lisp_Object instantiator,
			   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
			   int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  widget_value * wv =
    gui_items_to_widget_values (image_instance,
				IMAGE_INSTANCE_WIDGET_ITEMS (ii), 0);
  update_tab_widget_face (wv, ii,
			  IMAGE_INSTANCE_FRAME (ii));
  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "tab-control", wv);
}

/* Set the properties of a tab control */
static void
x_tab_control_redisplay (Lisp_Object image_instance)
{
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
	      if (gui_item_equal_sans_selected (XCAR (rest), selected, 0))
		{
		  /* There may be an encapsulated way of doing this,
		     but I couldn't find it. */
		  Lisp_Object old_selected =
		    gui_item_list_find_selected
		    (XCDR (IMAGE_INSTANCE_WIDGET_ITEMS (ii)));
		  char* name;
		  unsigned int num_children, i;
		  Widget* children;

		  name =
		    LISP_STRING_TO_EXTERNAL (XGUI_ITEM (XCAR (rest))->name,
					     Qlwlib_encoding);
		  /* The name may contain a `.' which confuses
		     XtNameToWidget, so we do it ourselves. */
		  children =
		    XtCompositeChildren (IMAGE_INSTANCE_X_WIDGET_ID (ii),
					 &num_children);
		  for (i = 0; i < num_children; i++)
		    {
		      if (!strcmp (XtName (children [i]), name))
			{
			  Xt_SET_VALUE (IMAGE_INSTANCE_X_WIDGET_ID (ii),
					XtNtopWidget, children [i]);
			  break;
			}
		    }
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
    }
  /* Possibly update the face. */
  if (IMAGE_INSTANCE_WIDGET_FACE_CHANGED (ii)
      ||
      XFRAME (IMAGE_INSTANCE_FRAME (ii))->faces_changed
      ||
      IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (ii))
    {
      /* See previous comments on the brokeness of lwlib.

	 #### There's actually not much point in doing this here
	 since, colors will have been set appropriately by
	 x_redisplay_widget. */
      widget_value* wv =copy_widget_value_tree
	(lw_get_all_values
	 (IMAGE_INSTANCE_X_WIDGET_LWID (ii)),
	 NO_CHANGE);

      update_tab_widget_face (wv, ii,
			      IMAGE_INSTANCE_FRAME (ii));

      lw_modify_all_widgets (IMAGE_INSTANCE_X_WIDGET_LWID (ii), wv, True);
      free_widget_value_tree (wv);
      gcpro_popup_callbacks (IMAGE_INSTANCE_X_WIDGET_LWID (ii));
    }
}

/* instantiate a static control possible for putting other things in */
static void
x_label_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		     Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		     int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object gui = IMAGE_INSTANCE_WIDGET_ITEM (ii);
  widget_value* wv = gui_items_to_widget_values (image_instance, gui, 0);

  x_widget_instantiate (image_instance, instantiator, pointer_fg,
			pointer_bg, dest_mask, domain, "button", wv);
}
#endif /* HAVE_X_WIDGETS */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_glyphs_x (void)
{
#if 0
  DEFSUBR (Fchange_subwindow_property);
#endif
}

void
console_type_create_glyphs_x (void)
{
  /* image methods */

  CONSOLE_HAS_METHOD (x, print_image_instance);
  CONSOLE_HAS_METHOD (x, finalize_image_instance);
  CONSOLE_HAS_METHOD (x, image_instance_equal);
  CONSOLE_HAS_METHOD (x, image_instance_hash);
  CONSOLE_HAS_METHOD (x, colorize_image_instance);
  CONSOLE_HAS_METHOD (x, init_image_instance_from_eimage);
  CONSOLE_HAS_METHOD (x, locate_pixmap_file);
  CONSOLE_HAS_METHOD (x, unmap_subwindow);
  CONSOLE_HAS_METHOD (x, map_subwindow);
  CONSOLE_HAS_METHOD (x, redisplay_widget);
  CONSOLE_HAS_METHOD (x, redisplay_subwindow);
  CONSOLE_HAS_METHOD (x, widget_border_width);
}

void
image_instantiator_format_create_glyphs_x (void)
{
  IIFORMAT_VALID_CONSOLE (x, nothing);
  IIFORMAT_VALID_CONSOLE (x, string);
#ifdef HAVE_X_WIDGETS
  IIFORMAT_VALID_CONSOLE (x, layout);
#endif
  IIFORMAT_VALID_CONSOLE (x, formatted_string);
  IIFORMAT_VALID_CONSOLE (x, inherit);
#ifdef HAVE_XPM
  INITIALIZE_DEVICE_IIFORMAT (x, xpm);
  IIFORMAT_HAS_DEVMETHOD (x, xpm, instantiate);
#endif
#ifdef HAVE_JPEG
  IIFORMAT_VALID_CONSOLE (x, jpeg);
#endif
#ifdef HAVE_TIFF
  IIFORMAT_VALID_CONSOLE (x, tiff);
#endif
#ifdef HAVE_PNG
  IIFORMAT_VALID_CONSOLE (x, png);
#endif
#ifdef HAVE_GIF
  IIFORMAT_VALID_CONSOLE (x, gif);
#endif
  INITIALIZE_DEVICE_IIFORMAT (x, xbm);
  IIFORMAT_HAS_DEVMETHOD (x, xbm, instantiate);

  INITIALIZE_DEVICE_IIFORMAT (x, subwindow);
  IIFORMAT_HAS_DEVMETHOD (x, subwindow, instantiate);
#ifdef HAVE_X_WIDGETS
  /* layout widget */
  INITIALIZE_DEVICE_IIFORMAT (x, native_layout);
  IIFORMAT_HAS_DEVMETHOD (x, native_layout, instantiate);
  /* button widget */
  INITIALIZE_DEVICE_IIFORMAT (x, button);
  IIFORMAT_HAS_DEVMETHOD (x, button, property);
  IIFORMAT_HAS_DEVMETHOD (x, button, instantiate);
  IIFORMAT_HAS_DEVMETHOD (x, button, redisplay);
  /* general widget methods. */
  INITIALIZE_DEVICE_IIFORMAT (x, widget);
  IIFORMAT_HAS_DEVMETHOD (x, widget, property);
  /* progress gauge */
  INITIALIZE_DEVICE_IIFORMAT (x, progress_gauge);
  IIFORMAT_HAS_DEVMETHOD (x, progress_gauge, redisplay);
  IIFORMAT_HAS_DEVMETHOD (x, progress_gauge, instantiate);
  /* text field */
  INITIALIZE_DEVICE_IIFORMAT (x, edit_field);
  IIFORMAT_HAS_DEVMETHOD (x, edit_field, instantiate);
#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
  /* combo box */
  INITIALIZE_DEVICE_IIFORMAT (x, combo_box);
  IIFORMAT_HAS_DEVMETHOD (x, combo_box, instantiate);
  IIFORMAT_HAS_SHARED_DEVMETHOD (x, combo_box, redisplay, tab_control);
#endif
  /* tab control widget */
  INITIALIZE_DEVICE_IIFORMAT (x, tab_control);
  IIFORMAT_HAS_DEVMETHOD (x, tab_control, instantiate);
  IIFORMAT_HAS_DEVMETHOD (x, tab_control, redisplay);
  /* label */
  INITIALIZE_DEVICE_IIFORMAT (x, label);
  IIFORMAT_HAS_DEVMETHOD (x, label, instantiate);
#endif
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (cursor_font, "cursor-font");
  IIFORMAT_VALID_CONSOLE (x, cursor_font);

  IIFORMAT_HAS_METHOD (cursor_font, validate);
  IIFORMAT_HAS_METHOD (cursor_font, possible_dest_types);
  IIFORMAT_HAS_METHOD (cursor_font, instantiate);

  IIFORMAT_VALID_KEYWORD (cursor_font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (cursor_font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (cursor_font, Q_background, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (font, "font");

  IIFORMAT_HAS_METHOD (font, validate);
  IIFORMAT_HAS_METHOD (font, possible_dest_types);
  IIFORMAT_HAS_METHOD (font, instantiate);
  IIFORMAT_VALID_CONSOLE (x, font);

  IIFORMAT_VALID_KEYWORD (font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_background, check_valid_string);

#ifdef HAVE_XFACE
  INITIALIZE_DEVICE_IIFORMAT (x, xface);
  IIFORMAT_HAS_DEVMETHOD (x, xface, instantiate);
#endif

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (autodetect,
					"autodetect");

  IIFORMAT_HAS_METHOD (autodetect, validate);
  IIFORMAT_HAS_METHOD (autodetect, normalize);
  IIFORMAT_HAS_METHOD (autodetect, possible_dest_types);
  /* #### autodetect is flawed IMO:
  1. It makes the assumption that you can detect whether the user
  wanted a cursor or a string based on the data, since the data is a
  string you have to prioritise cursors. Instead we will force users
  to pick the appropriate image type, this is what we do under
  MS-Windows anyway.
  2. It doesn't fit with the new domain model - you cannot tell which
  domain it needs to be instantiated in until you've actually
  instantiated it, which mucks up caching.
  3. It only copes with cursors and strings which seems bogus. */
  IIFORMAT_HAS_SHARED_METHOD (autodetect, governing_domain, subwindow);
  IIFORMAT_HAS_METHOD (autodetect, instantiate);
  IIFORMAT_VALID_CONSOLE (x, autodetect);

  IIFORMAT_VALID_KEYWORD (autodetect, Q_data, check_valid_string);
}

void
vars_of_glyphs_x (void)
{
  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path /*
A list of the directories in which X bitmap files may be found.
If nil, this is initialized from the "*bitmapFilePath" resource.
This is used by the `make-image-instance' function (however, note that if
the environment variable XBMLANGPATH is set, it is consulted first).
*/ );
  Vx_bitmap_file_path = Qnil;
}

void
complex_vars_of_glyphs_x (void)
{
#define BUILD_GLYPH_INST(variable, name)			\
  Fadd_spec_to_specifier					\
    (GLYPH_IMAGE (XGLYPH (variable)),				\
     vector3 (Qxbm, Q_data,					\
	      list3 (make_fixnum (name##_width),			\
		     make_fixnum (name##_height),			\
		     make_extstring ((Extbyte *) name##_bits,	\
				      sizeof (name##_bits),	\
				      Qbinary))),		\
     Qglobal, Qx, Qnil)

  BUILD_GLYPH_INST (Vtruncation_glyph, truncator);
  BUILD_GLYPH_INST (Vcontinuation_glyph, continuer);
  BUILD_GLYPH_INST (Vxemacs_logo, xemacs);
  BUILD_GLYPH_INST (Vhscroll_glyph, hscroll);

#undef BUILD_GLYPH_INST
}
