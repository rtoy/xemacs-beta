/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems
   Copyright (C) 1995, 1996 Ben Wing
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

   TODO:
   ImageMagick support
   Convert images.el to C and stick it in here?
 */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "glyphs-x.h"
#include "objects-x.h"
#include "xmu.h"

#include "buffer.h"
#include "frame.h"
#include "insdel.h"
#include "opaque.h"

#include "sysfile.h"

#ifdef HAVE_IMAGEMAGICK
#define _XOS_H_
#ifdef MAGICK_HEADERS_ARE_UNDER_X11
#include <X11/magick/magick.h>
#else
#include <magick/magick.h>
#endif
/*#include <image.h>*/
/*#include <assert.h>*/

#define OLDCOMPAT /* allow lisp code using the old names to still function */
#endif

#define LISP_DEVICE_TO_X_SCREEN(dev)					\
  XDefaultScreenOfDisplay (DEVICE_X_DISPLAY (XDEVICE (dev)))

DEFINE_IMAGE_INSTANTIATOR_FORMAT (xbm);
Lisp_Object Qxbm;

Lisp_Object Q_mask_file, Q_mask_data, Q_hotspot_x, Q_hotspot_y;
Lisp_Object Q_foreground, Q_background;

#ifdef HAVE_XPM
DEFINE_IMAGE_INSTANTIATOR_FORMAT (xpm);
Lisp_Object Qxpm;
Lisp_Object Q_color_symbols;
#endif

#ifdef HAVE_XFACE
DEFINE_IMAGE_INSTANTIATOR_FORMAT (xface);
Lisp_Object Qxface;
#endif

#ifdef HAVE_IMAGEMAGICK
DEFINE_IMAGE_INSTANTIATOR_FORMAT (imagick);
Lisp_Object Qimagick;

#ifdef OLDCOMPAT /* old compatibility */
DEFINE_IMAGE_INSTANTIATOR_FORMAT (tiff);
DEFINE_IMAGE_INSTANTIATOR_FORMAT (png);
DEFINE_IMAGE_INSTANTIATOR_FORMAT (gif);
DEFINE_IMAGE_INSTANTIATOR_FORMAT (jpeg);
Lisp_Object Qtiff;
Lisp_Object Qpng;
Lisp_Object Qgif;
Lisp_Object Qjpeg;
#endif
#endif

DEFINE_IMAGE_INSTANTIATOR_FORMAT (cursor_font);
Lisp_Object Qcursor_font;

DEFINE_IMAGE_INSTANTIATOR_FORMAT (font);

DEFINE_IMAGE_INSTANTIATOR_FORMAT (autodetect);

static void cursor_font_instantiate (Lisp_Object image_instance,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     Lisp_Object domain);

#include "bitmaps.h"


/************************************************************************/
/*                      image instance methods                          */
/************************************************************************/

static void
x_print_image_instance (struct Lisp_Image_Instance *p,
			Lisp_Object printcharfun,
			int escapeflag)
{
  char buf[100];

  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      sprintf (buf, " (0x%lx", (unsigned long) IMAGE_INSTANCE_X_PIXMAP (p));
      write_c_string (buf, printcharfun);
      if (IMAGE_INSTANCE_X_MASK (p))
	{
	  sprintf (buf, "/0x%lx", (unsigned long) IMAGE_INSTANCE_X_MASK (p));
	  write_c_string (buf, printcharfun);
	}
      write_c_string (")", printcharfun);
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
x_finalize_image_instance (struct Lisp_Image_Instance *p)
{
  if (!p->data)
    return;

  if (DEVICE_LIVE_P (XDEVICE (p->device)))
    {
      Display *dpy = DEVICE_X_DISPLAY (XDEVICE (p->device));

      if (IMAGE_INSTANCE_X_PIXMAP (p))
	XFreePixmap (dpy, IMAGE_INSTANCE_X_PIXMAP (p));
      if (IMAGE_INSTANCE_X_MASK (p) &&
	  IMAGE_INSTANCE_X_MASK (p) != IMAGE_INSTANCE_X_PIXMAP (p))
	XFreePixmap (dpy, IMAGE_INSTANCE_X_MASK (p));
      IMAGE_INSTANCE_X_PIXMAP (p) = 0;
      IMAGE_INSTANCE_X_MASK (p) = 0;

      if (IMAGE_INSTANCE_X_CURSOR (p))
	{
	  XFreeCursor (dpy, IMAGE_INSTANCE_X_CURSOR (p));
	  IMAGE_INSTANCE_X_CURSOR (p) = 0;
	}

      if (IMAGE_INSTANCE_X_NPIXELS (p) != 0)
	{
	  XFreeColors (dpy,
		       DEVICE_X_COLORMAP (XDEVICE(p->device)),
		       IMAGE_INSTANCE_X_PIXELS (p),
		       IMAGE_INSTANCE_X_NPIXELS (p), 0);
	  IMAGE_INSTANCE_X_NPIXELS (p) = 0;
	}
    }
  if (IMAGE_INSTANCE_X_PIXELS (p))
    {
      xfree (IMAGE_INSTANCE_X_PIXELS (p));
      IMAGE_INSTANCE_X_PIXELS (p) = 0;
    }

  xfree (p->data);
  p->data = 0;
}

static int
x_image_instance_equal (struct Lisp_Image_Instance *p1,
			struct Lisp_Image_Instance *p2, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p1))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      if (IMAGE_INSTANCE_X_NPIXELS (p1) != IMAGE_INSTANCE_X_NPIXELS (p2))
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
x_image_instance_hash (struct Lisp_Image_Instance *p, int depth)
{
  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
    case IMAGE_COLOR_PIXMAP:
    case IMAGE_POINTER:
      return IMAGE_INSTANCE_X_NPIXELS (p);
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
x_initialize_pixmap_image_instance (struct Lisp_Image_Instance *ii,
				    enum image_instance_type type)
{
  ii->data = xnew_and_zero (struct x_image_instance_data);
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

static Lisp_Object
locate_pixmap_file (Lisp_Object name)
{
  /* This function can GC if IN_REDISPLAY is false */
  Display *display;

  /* Check non-absolute pathnames with a directory component relative to
     the search path; that's the way Xt does it. */
  /* #### Unix-specific */
  if (XSTRING_BYTE (name, 0) == '/' ||
      (XSTRING_BYTE (name, 0) == '.' &&
       (XSTRING_BYTE (name, 1) == '/' ||
	(XSTRING_BYTE (name, 1) == '.' &&
	 (XSTRING_BYTE (name, 2) == '/')))))
    {
      if (!NILP (Ffile_readable_p (name)))
	return name;
      else
	return Qnil;
    }

  if (NILP (Vdefault_x_device))
    /* This may occur during intialization. */
    return Qnil;
  else
    /* We only check the bitmapFilePath resource on the original X device. */
    display = DEVICE_X_DISPLAY (XDEVICE (Vdefault_x_device));

#ifdef USE_XBMLANGPATH
  {
    char *path = egetenv ("XBMLANGPATH");
    SubstitutionRec subs[1];
    subs[0].match = 'B';
    subs[0].substitution = (char *) XSTRING_DATA (name);
    /* #### Motif uses a big hairy default if $XBMLANGPATH isn't set.
       We don't.  If you want it used, set it. */
    if (path &&
	(path = XtResolvePathname (display, "bitmaps", 0, 0, path,
				   subs, XtNumber (subs), 0)))
      {
	name = build_string (path);
	XtFree (path);
        return (name);
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
	Vx_bitmap_file_path = decode_env_path (0, (char *) value.addr);
      Vx_bitmap_file_path = nconc2 (Vx_bitmap_file_path,
				    (list1 (build_string (BITMAPDIR))));
    }

  {
    Lisp_Object found;
    if (locate_file (Vx_bitmap_file_path, name, "", &found, R_OK) < 0)
      {
	Lisp_Object temp = list1 (Vdata_directory);
	struct gcpro gcpro1;

	GCPRO1 (temp);
	locate_file (temp, name, "", &found, R_OK);
	UNGCPRO;
      }

    return found;
  }
}

/* If INSTANTIATOR refers to inline data, return Qnil.
   If INSTANTIATOR refers to data in a file, return the full filename
   if it exists; otherwise, return a cons of (filename).

   FILE_KEYWORD and DATA_KEYWORD are symbols specifying the
   keywords used to look up the file and inline data,
   respectively, in the instantiator.  Normally these would
   be Q_file and Q_data, but might be different for mask data. */

static Lisp_Object
potential_pixmap_file_instantiator (Lisp_Object instantiator,
				    Lisp_Object file_keyword,
				    Lisp_Object data_keyword)
{
  Lisp_Object file;
  Lisp_Object data;

  assert (VECTORP (instantiator));

  data = find_keyword_in_vector (instantiator, data_keyword);
  file = find_keyword_in_vector (instantiator, file_keyword);

  if (!NILP (file) && NILP (data))
    {
      Lisp_Object retval = locate_pixmap_file (file);
      if (!NILP (retval))
	return retval;
      else
	return Fcons (file, Qnil); /* should have been file */
    }

  return Qnil;
}


static Lisp_Object
simple_image_type_normalize (Lisp_Object inst, Lisp_Object console_type,
			     Lisp_Object image_type_tag)
{
  /* This function can call lisp */
  Lisp_Object file = Qnil;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object alist = Qnil;

  GCPRO2 (file, alist);

  /* Now, convert any file data into inline data.  At the end of this,
     `data' will contain the inline data (if any) or Qnil, and `file'
     will contain the name this data was derived from (if known) or
     Qnil.

     Note that if we cannot generate any regular inline data, we
     skip out. */

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data);

  if (CONSP (file)) /* failure locating filename */
    signal_double_file_error ("Opening pixmap file",
			      "no such file or directory",
			      Fcar (file));

  if (NILP (file)) /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  {
    Lisp_Object data = make_string_from_file (file);
    alist = remassq_no_quit (Q_file, alist);
    /* there can't be a :data at this point. */
    alist = Fcons (Fcons (Q_file, file),
		   Fcons (Fcons (Q_data, data), alist));
  }

  {
    Lisp_Object result = alist_to_tagged_vector (image_type_tag, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

static void
write_lisp_string_to_temp_file (Lisp_Object string, char *filename_out)
{
  Extbyte *bytes;
  Extcount len;
  FILE *stream;

  /* #### This is a definite problem under Mule due to the amount of
     stack data it might allocate.  Need to be able to convert and
     write out to a file. */
  GET_STRING_BINARY_DATA_ALLOCA (string, bytes, len);

  /* Write out to a temporary file ... */
  sprintf (filename_out, "/tmp/emacs%d.XXXXXX", (int) getpid ());
  mktemp (filename_out);
  stream = fopen (filename_out, "w");
  if (!stream)
    {
    temp_file_error:
      if (stream)
	{
	  int old_errno = errno;
	  fclose (stream);
	  unlink (filename_out);
	  errno = old_errno;
	}
      report_file_error ("Creating temp file",
			 list1 (build_string (filename_out)));
    }

  if (fwrite (bytes, len, 1, stream) != 1)
    goto temp_file_error;

  if (fclose (stream) != 0)
    {
      stream = 0;
      goto temp_file_error;
    }
}


/************************************************************************/
/*                           cursor functions                           */
/************************************************************************/

/* Check that this server supports cursors of size WIDTH * HEIGHT.  If
   not, signal an error.  INSTANTIATOR is only used in the error
   message. */

static void
check_pointer_sizes (Screen *xs, unsigned int width, unsigned int height,
		     Lisp_Object instantiator)
{
  unsigned int best_width, best_height;
  if (! XQueryBestCursor (DisplayOfScreen (xs), RootWindowOfScreen (xs),
			  width, height, &best_width, &best_height))
    /* this means that an X error of some sort occurred (we trap
       these so they're not fatal). */
    signal_simple_error ("XQueryBestCursor() failed?", instantiator);

  if (width > best_width || height > best_height)
    error_with_frob (instantiator,
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
      xbg->red = xbg->green = xbg->blue = ~0;
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
init_image_instance_from_x_image (struct Lisp_Image_Instance *ii,
				  XImage *ximage,
				  int dest_mask,
				  unsigned long *pixels,
				  int npixels,
				  Lisp_Object instantiator)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  GC gc;
  Drawable d;
  Pixmap pixmap;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  d = XtWindow(DEVICE_XT_APP_SHELL (XDEVICE (device)));

  if (!(dest_mask & IMAGE_COLOR_PIXMAP_MASK))
    incompatible_image_types (instantiator, dest_mask,
			      IMAGE_COLOR_PIXMAP_MASK);

  pixmap = XCreatePixmap (dpy, d, ximage->width,
			  ximage->height, ximage->depth);
  if (!pixmap)
    signal_simple_error ("Unable to create pixmap", instantiator);

  gc = XCreateGC (dpy, pixmap, 0, NULL);
  if (!gc)
    {
      XFreePixmap (dpy, pixmap);
      signal_simple_error ("Unable to create GC", instantiator);
    }

  XPutImage (dpy, pixmap, gc, ximage, 0, 0, 0, 0,
	     ximage->width, ximage->height);

  XFreeGC (dpy, gc);

  x_initialize_pixmap_image_instance (ii, IMAGE_COLOR_PIXMAP);

  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  IMAGE_INSTANCE_X_PIXMAP (ii) = pixmap;
  IMAGE_INSTANCE_X_MASK (ii) = 0;
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = ximage->width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = ximage->height;
  IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = ximage->depth;
  IMAGE_INSTANCE_X_PIXELS (ii) = pixels;
  IMAGE_INSTANCE_X_NPIXELS (ii) = npixels;
}


/**********************************************************************
 *                             XBM                                    *
 **********************************************************************/

/* Check if DATA represents a valid inline XBM spec (i.e. a list
   of (width height bits), with checking done on the dimensions).
   If not, signal an error. */

static void
check_valid_xbm_inline (Lisp_Object data)
{
  Lisp_Object width, height, bits;

  CHECK_CONS (data);
  if (!CONSP (XCDR (data)) || !CONSP (XCDR (XCDR (data))) ||
      !NILP (XCDR (XCDR (XCDR (data)))))
    signal_simple_error ("Must be list of 3 elements", data);

  width = XCAR (data);
  height = XCAR (XCDR (data));
  bits = XCAR (XCDR (XCDR (data)));

  if (!INTP (width) || !INTP (height) || !STRINGP (bits))
    signal_simple_error ("Must be (width height bits)",
			 vector3 (width, height, bits));

  if (XINT (width) <= 0)
    signal_simple_error ("Width must be > 0", width);

  if (XINT (height) <= 0)
    signal_simple_error ("Height must be > 0", height);

  if (((unsigned) (XINT (width) * XINT (height)) / 8)
      > string_char_length (XSTRING (bits)))
    signal_simple_error ("data is too short for W and H",
			 vector3 (width, height, bits));
}

/* Validate method for XBM's. */

static void
xbm_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

/* Given a filename that is supposed to contain XBM data, return
   the inline representation of it as (width height bits).  Return
   the hotspot through XHOT and YHOT, if those pointers are not 0.
   If there is no hotspot, XHOT and YHOT will contain -1.

   If the function fails:

   -- if OK_IF_DATA_INVALID is set and the data was invalid,
      return Qt.
   -- maybe return an error, or return Qnil.
 */


static Lisp_Object
bitmap_to_lisp_data (Lisp_Object name, int *xhot, int *yhot,
		     int ok_if_data_invalid)
{
  unsigned int w, h;
  Extbyte *data;
  int result;
  CONST char *filename_ext;

  GET_C_STRING_FILENAME_DATA_ALLOCA (name, filename_ext);
  result = XmuReadBitmapDataFromFile (filename_ext, &w, &h, &data, xhot, yhot);

  if (result == BitmapSuccess)
    {
      Lisp_Object retval;
      int len = (w + 7) / 8 * h;

      retval = list3 (make_int (w), make_int (h),
		      make_ext_string (data, len, FORMAT_BINARY));
      XFree ((char *) data);
      return retval;
    }

  switch (result)
    {
    case BitmapOpenFailed:
      {
	/* should never happen */
	signal_double_file_error ("Opening bitmap file",
				  "no such file or directory",
				  name);
      }
    case BitmapFileInvalid:
      {
	if (ok_if_data_invalid)
	  return Qt;
	signal_double_file_error ("Reading bitmap file",
				  "invalid data in file",
				  name);
      }
    case BitmapNoMemory:
      {
	signal_double_file_error ("Reading bitmap file",
				  "out of memory",
				  name);
      }
    default:
      {
	signal_double_file_error_2 ("Reading bitmap file",
				    "unknown error code",
				    make_int (result), name);
      }
    }

  return Qnil; /* not reached */
}

static Lisp_Object
xbm_mask_file_munging (Lisp_Object alist, Lisp_Object file,
		       Lisp_Object mask_file)
{
  /* This is unclean but it's fairly standard -- a number of the
     bitmaps in /usr/include/X11/bitmaps use it -- so we support
     it. */
  if (NILP (mask_file)
      /* don't override explicitly specified mask data. */
      && NILP (assq_no_quit (Q_mask_data, alist))
      && !NILP (file))
    {
      mask_file =
	locate_pixmap_file (concat2 (file, build_string ("Mask")));
      if (NILP (mask_file))
	mask_file =
	  locate_pixmap_file (concat2 (file, build_string ("msk")));
    }

  if (!NILP (mask_file))
    {
      Lisp_Object mask_data =
	bitmap_to_lisp_data (mask_file, 0, 0, 0);
      alist = remassq_no_quit (Q_mask_file, alist);
      /* there can't be a :mask-data at this point. */
      alist = Fcons (Fcons (Q_mask_file, mask_file),
		     Fcons (Fcons (Q_mask_data, mask_data), alist));
    }

  return alist;
}

/* Normalize method for XBM's. */

static Lisp_Object
xbm_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  Lisp_Object file = Qnil, mask_file = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object alist = Qnil;

  GCPRO3 (file, mask_file, alist);

  /* Now, convert any file data into inline data for both the regular
     data and the mask data.  At the end of this, `data' will contain
     the inline data (if any) or Qnil, and `file' will contain
     the name this data was derived from (if known) or Qnil.
     Likewise for `mask_file' and `mask_data'.

     Note that if we cannot generate any regular inline data, we
     skip out. */

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data);
  mask_file = potential_pixmap_file_instantiator (inst, Q_mask_file,
						  Q_mask_data);

  if (CONSP (file)) /* failure locating filename */
    signal_double_file_error ("Opening bitmap file",
			      "no such file or directory",
			      Fcar (file));

  if (NILP (file) && NILP (mask_file)) /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  if (!NILP (file))
    {
      int xhot, yhot;
      Lisp_Object data = bitmap_to_lisp_data (file, &xhot, &yhot, 0);
      alist = remassq_no_quit (Q_file, alist);
      /* there can't be a :data at this point. */
      alist = Fcons (Fcons (Q_file, file),
		     Fcons (Fcons (Q_data, data), alist));

      if (xhot != -1 && NILP (assq_no_quit (Q_hotspot_x, alist)))
	alist = Fcons (Fcons (Q_hotspot_x, make_int (xhot)),
		       alist);
      if (yhot != -1 && NILP (assq_no_quit (Q_hotspot_y, alist)))
	alist = Fcons (Fcons (Q_hotspot_y, make_int (yhot)),
		       alist);
    }

  alist = xbm_mask_file_munging (alist, file, mask_file);

  {
    Lisp_Object result = alist_to_tagged_vector (Qxbm, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

/* Given inline data for a mono pixmap, create and return the
   corresponding X object. */

static Pixmap
pixmap_from_xbm_inline (Lisp_Object device, int width, int height,
			/* Note that data is in ext-format! */
			CONST Extbyte *bits)
{
  return XCreatePixmapFromBitmapData (DEVICE_X_DISPLAY (XDEVICE(device)),
				      XtWindow (DEVICE_XT_APP_SHELL (XDEVICE (device))),
				      (char *) bits, width, height,
				      1, 0, 1);
}

/* Given inline data for a mono pixmap, initialize the given
   image instance accordingly. */

static void
init_image_instance_from_xbm_inline (struct Lisp_Image_Instance *ii,
				     int width, int height,
				     /* Note that data is in ext-format! */
				     CONST char *bits,
				     Lisp_Object instantiator,
				     Lisp_Object pointer_fg,
				     Lisp_Object pointer_bg,
				     int dest_mask,
				     Pixmap mask,
				     Lisp_Object mask_filename)
{
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Lisp_Object foreground = find_keyword_in_vector (instantiator, Q_foreground);
  Lisp_Object background = find_keyword_in_vector (instantiator, Q_background);
  Display *dpy;
  Screen *scr;
  Drawable draw;
  enum image_instance_type type;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

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

  x_initialize_pixmap_image_instance (ii, type);
  IMAGE_INSTANCE_PIXMAP_WIDTH (ii) = width;
  IMAGE_INSTANCE_PIXMAP_HEIGHT (ii) = height;
  IMAGE_INSTANCE_PIXMAP_FILENAME (ii) =
    find_keyword_in_vector (instantiator, Q_file);

  switch (type)
    {
    case IMAGE_MONO_PIXMAP:
      {
	IMAGE_INSTANCE_X_PIXMAP (ii) =
	  pixmap_from_xbm_inline (device, width, height, (Extbyte *) bits);
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
				       (char *) bits, width, height,
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
				       (char *) bits, width, height,
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
	     XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii)) : 0,
	     !NILP (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) ?
	     XINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii)) : 0);
      }
      break;

    default:
      abort ();
    }
}

static int
xbm_possible_dest_types (void)
{
  return
    IMAGE_MONO_PIXMAP_MASK  |
    IMAGE_COLOR_PIXMAP_MASK |
    IMAGE_POINTER_MASK;
}

static void
xbm_instantiate_1 (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, int width, int height,
		   /* Note that data is in ext-format! */
		   CONST char *bits)
{
  Lisp_Object mask_data = find_keyword_in_vector (instantiator, Q_mask_data);
  Lisp_Object mask_file = find_keyword_in_vector (instantiator, Q_mask_file);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Pixmap mask = 0;
  CONST char *gcc_may_you_rot_in_hell;

  if (!NILP (mask_data))
{
      GET_C_STRING_BINARY_DATA_ALLOCA (XCAR (XCDR (XCDR (mask_data))),
				       gcc_may_you_rot_in_hell);
      mask =
	pixmap_from_xbm_inline (IMAGE_INSTANCE_DEVICE (ii),
				XINT (XCAR (mask_data)),
				XINT (XCAR (XCDR (mask_data))),
				(CONST unsigned char *)
				gcc_may_you_rot_in_hell);
}

  init_image_instance_from_xbm_inline (ii, width, height, bits,
				       instantiator, pointer_fg, pointer_bg,
				       dest_mask, mask, mask_file);
}

/* Instantiate method for XBM's. */

static void
xbm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object domain)
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  CONST char *gcc_go_home;

  assert (!NILP (data));

  GET_C_STRING_BINARY_DATA_ALLOCA (XCAR (XCDR (XCDR (data))),
				   gcc_go_home);

  xbm_instantiate_1 (image_instance, instantiator, pointer_fg,
		     pointer_bg, dest_mask, XINT (XCAR (data)),
		     XINT (XCAR (XCDR (data))), gcc_go_home);
}


#ifdef HAVE_XPM

/**********************************************************************
 *                             XPM                                    *
 **********************************************************************/

static void
check_valid_xpm_color_symbols (Lisp_Object data)
{
  Lisp_Object rest;

  for (rest = data; !NILP (rest); rest = XCDR (rest))
    {
      if (!CONSP (rest) ||
	  !CONSP (XCAR (rest)) ||
	  !STRINGP (XCAR (XCAR (rest))) ||
	  (!STRINGP (XCDR (XCAR (rest))) &&
	   !COLOR_SPECIFIERP (XCDR (XCAR (rest)))))
	signal_simple_error ("Invalid color symbol alist", data);
    }
}

static void
xpm_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
pixmap_to_lisp_data (Lisp_Object name, int ok_if_data_invalid)
{
  char **data;
  int result;

  result = XpmReadFileToData ((char *) XSTRING_DATA (name), &data);

  if (result == XpmSuccess)
    {
      Lisp_Object retval = Qnil;
      struct buffer *old_buffer = current_buffer;
      Lisp_Object temp_buffer =
	Fget_buffer_create (build_string (" *pixmap conversion*"));
      int elt;
      int height, width, ncolors;
      struct gcpro gcpro1, gcpro2, gcpro3;
      int speccount = specpdl_depth ();

      GCPRO3 (name, retval, temp_buffer);

      specbind (Qinhibit_quit, Qt);
      set_buffer_internal (XBUFFER (temp_buffer));
      Ferase_buffer (Qnil);

      buffer_insert_c_string (current_buffer, "/* XPM */\r");
      buffer_insert_c_string (current_buffer, "static char *pixmap[] = {\r");

      sscanf (data[0], "%d %d %d", &height, &width, &ncolors);
      for (elt = 0; elt <= width + ncolors; elt++)
	{
	  buffer_insert_c_string (current_buffer, "\"");
	  buffer_insert_c_string (current_buffer, data[elt]);

	  if (elt < width + ncolors)
	    buffer_insert_c_string (current_buffer, "\",\r");
	  else
	    buffer_insert_c_string (current_buffer, "\"};\r");
	}

      retval = Fbuffer_substring (Qnil, Qnil, Qnil);
      XpmFree (data);

      set_buffer_internal (old_buffer);
      unbind_to (speccount, Qnil);

      RETURN_UNGCPRO (retval);
    }

  switch (result)
    {
    case XpmFileInvalid:
      {
	if (ok_if_data_invalid)
	  return Qt;
	signal_simple_error ("invalid XPM data in file", name);
      }
    case XpmNoMemory:
      {
	signal_double_file_error ("Reading pixmap file",
				  "out of memory", name);
      }
    case XpmOpenFailed:
      {
	/* should never happen? */
	signal_double_file_error ("Opening pixmap file",
				  "no such file or directory", name);
      }
    default:
      {
	signal_double_file_error_2 ("Parsing pixmap file",
				    "unknown error code",
				    make_int (result), name);
	break;
      }
    }

  return Qnil; /* not reached */
}

Lisp_Object Vxpm_color_symbols;

static Lisp_Object
evaluate_xpm_color_symbols (void)
{
  Lisp_Object rest, results = Qnil;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (rest, results);
  for (rest = Vxpm_color_symbols; !NILP (rest); rest = XCDR (rest))
    {
      Lisp_Object name, value, cons;

      CHECK_CONS (rest);
      cons = XCAR (rest);
      CHECK_CONS (cons);
      name = XCAR (cons);
      CHECK_STRING (name);
      value = XCDR (cons);
      CHECK_CONS (value);
      value = XCAR (value);
      value = Feval (value);
      if (NILP (value))
	continue;
      if (!STRINGP (value) && !COLOR_SPECIFIERP (value))
	signal_simple_error
	  ("Result from xpm-color-symbols eval must be nil, string, or color",
	   value);
      results = Fcons (Fcons (name, value), results);
    }
  UNGCPRO;			/* no more evaluation */
  return results;
}

static Lisp_Object
xpm_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  Lisp_Object file = Qnil;
  Lisp_Object color_symbols;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object alist = Qnil;

  GCPRO2 (file, alist);

  /* Now, convert any file data into inline data.  At the end of this,
     `data' will contain the inline data (if any) or Qnil, and
     `file' will contain the name this data was derived from (if
     known) or Qnil.

     Note that if we cannot generate any regular inline data, we
     skip out. */

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data);

  if (CONSP (file)) /* failure locating filename */
    signal_double_file_error ("Opening pixmap file",
			      "no such file or directory",
			      Fcar (file));

  color_symbols = find_keyword_in_vector_or_given (inst, Q_color_symbols,
						   Qunbound);

  if (NILP (file) && !UNBOUNDP (color_symbols))
    /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  if (!NILP (file))
    {
      Lisp_Object data = pixmap_to_lisp_data (file, 0);
      alist = remassq_no_quit (Q_file, alist);
      /* there can't be a :data at this point. */
      alist = Fcons (Fcons (Q_file, file),
		     Fcons (Fcons (Q_data, data), alist));
    }

  if (UNBOUNDP (color_symbols))
    {
      color_symbols = evaluate_xpm_color_symbols ();
      alist = Fcons (Fcons (Q_color_symbols, color_symbols),
		     alist);
    }

  {
    Lisp_Object result = alist_to_tagged_vector (Qxpm, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

static int
xpm_possible_dest_types (void)
{
  return
    IMAGE_MONO_PIXMAP_MASK  |
    IMAGE_COLOR_PIXMAP_MASK |
    IMAGE_POINTER_MASK;
}

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
	    (value, device, encode_error_behavior_flag (ERROR_ME_NOT));
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
      if (! XAllocColor (dpy, cmap, &color))
	abort ();  /* it must be allocable since we're just duplicating it */

      symbols [i].name = (char *) XSTRING_DATA (XCAR (cons));
      symbols [i].pixel = color.pixel;
      symbols [i].value = 0;
      free_cons (XCONS (cons));
      cons = results;
      results = XCDR (results);
      free_cons (XCONS (cons));
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
xpm_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  Screen *xs;
  Colormap cmap;
  int depth;
  Pixmap pixmap;
  Pixmap mask = 0;
  XpmAttributes xpmattrs;
  int result;
  XpmColorSymbol *color_symbols;
  Lisp_Object color_symbol_alist = find_keyword_in_vector (instantiator,
							   Q_color_symbols);
  enum image_instance_type type;
  int force_mono;
  unsigned int w, h;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  cmap = DEVICE_X_COLORMAP (XDEVICE(device));
  depth = DEVICE_X_DEPTH (XDEVICE(device));
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

  x_initialize_pixmap_image_instance (ii, type);

  assert (!NILP (data));

 retry:

  memset (&xpmattrs, 0, sizeof (xpmattrs)); /* want XpmInitAttributes() */
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
      xpmattrs.visual = DEVICE_X_VISUAL (XDEVICE(device));
      xpmattrs.valuemask |= XpmVisual;
      xpmattrs.colormap = cmap;
      xpmattrs.valuemask |= XpmColormap;
    }

  color_symbols = extract_xpm_color_names (&xpmattrs, device, domain,
					   color_symbol_alist);

  result = XpmCreatePixmapFromBuffer (dpy,
				      XtWindow(DEVICE_XT_APP_SHELL (XDEVICE(device))),
				      (char *) XSTRING_DATA (data),
				      &pixmap, &mask, &xpmattrs);

  if (color_symbols)
    {
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
	signal_simple_error ("invalid XPM data", data);
      }
    case XpmColorFailed:
    case XpmColorError:
      {
	xpm_free (&xpmattrs);
	if (force_mono)
	  {
	    /* second time; blow out. */
	    signal_double_file_error ("Reading pixmap data",
				      "color allocation failed",
				      data);
	  }
	else
	  {
	    if (! (dest_mask & IMAGE_MONO_PIXMAP_MASK))
	      {
		/* second time; blow out. */
		signal_double_file_error ("Reading pixmap data",
					  "color allocation failed",
					  data);
	      }
	    force_mono = 1;
	    IMAGE_INSTANCE_TYPE (ii) = IMAGE_MONO_PIXMAP;
	    goto retry;
	  }
      }
    case XpmNoMemory:
      {
	xpm_free (&xpmattrs);
	signal_double_file_error ("Parsing pixmap data",
				  "out of memory", data);
      }
    default:
      {
	xpm_free (&xpmattrs);
	signal_double_file_error_2 ("Parsing pixmap data",
				    "unknown error code",
				    make_int (result), data);
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
    IMAGE_INSTANCE_X_MASK (ii) = mask;
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
      {
	IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = depth;
      }
      break;

    case IMAGE_POINTER:
      {
	int npixels = xpmattrs.npixels;
	Pixel *pixels = xpmattrs.pixels;
	XColor fg, bg;
	int i;
	int xhot = 0, yhot = 0;

	if (xpmattrs.valuemask & XpmHotspot)
	  {
	    xhot = xpmattrs.x_hotspot;
	    XSETINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_X (ii), xpmattrs.x_hotspot);
	  }
	if (xpmattrs.valuemask & XpmHotspot)
	  {
	    yhot = xpmattrs.y_hotspot;
	    XSETINT (IMAGE_INSTANCE_PIXMAP_HOTSPOT_Y (ii), xpmattrs.y_hotspot);
	  }
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

      break;

    default:
      abort ();
    }

  xpm_free (&xpmattrs);	/* after we've read pixels and hotspot */
}

#endif /* HAVE_XPM */


#ifdef HAVE_IMAGEMAGICK
/**********************************************************************
 *                             ImageMagick                            *
 **********************************************************************/
static void
imagick_validate (Lisp_Object instantiator)
{
	file_or_data_must_be_present (instantiator);
}

static Lisp_Object
imagick_normalize (Lisp_Object inst, Lisp_Object console_type)
{
	return simple_image_type_normalize (inst, console_type, Qimagick);
}

static int
imagick_possible_dest_types (void)
{
	return IMAGE_COLOR_PIXMAP_MASK;
}

struct imagick_unwind_data
{
	Display *dpy;
	Colormap cmap;
	FILE *instream;
	Image *image;
	XImage *ximage;
	unsigned long *pixels;
	unsigned long npixels;
	char tempfile[50];
	int tempfile_needs_to_be_removed;
};

static Lisp_Object
imagick_instantiate_unwind (Lisp_Object unwind_obj)
{
	struct imagick_unwind_data *data =
		(struct imagick_unwind_data *) get_opaque_ptr (unwind_obj);
 
	free_opaque_ptr (unwind_obj);
	if (data->instream)
		fclose (data->instream);
	if (data->tempfile_needs_to_be_removed)
		unlink (data->tempfile);

	if (data->image) {
		DestroyImage(data->image);
	}

	if (data->ximage) {
		if (data->ximage->data) {
			xfree (data->ximage->data);
			data->ximage->data = NULL;
		}
		XDestroyImage (data->ximage);
	}

	if (data->npixels > 0) {
	  XFreeColors(data->dpy, data->cmap, data->pixels, data->npixels, 0L);
	  xfree (data->pixels);
	}
 
	return Qnil;
}

static void
imagick_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
					 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
					 int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  Screen *scr;
  Visual *visual;
  Colormap cmap;
  Dimension depth;
  struct imagick_unwind_data unwind;
  int speccount;
  ImageInfo image_info;

  /* ImageMagick variables */

  /* Basic error checking */
  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  scr = DefaultScreenOfDisplay (dpy);
  depth = DEVICE_X_DEPTH (XDEVICE (device));
  visual = DEVICE_X_VISUAL (XDEVICE (device));
  cmap = DEVICE_X_COLORMAP (XDEVICE(device));

  /* Set up the unwind */
  memset (&unwind, 0, sizeof (unwind));
  unwind.dpy = dpy;
  unwind.cmap = cmap;
  speccount = specpdl_depth();
  record_unwind_protect(imagick_instantiate_unwind,make_opaque_ptr(&unwind));

  /* Write out to a temp file - not sure if ImageMagick supports the
  ** notion of an abstract 'data source' right now.
  ** JH: It doesn't as of 3.9.3
  */
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

    assert (!NILP (data));

    write_lisp_string_to_temp_file (data, unwind.tempfile);
    unwind.tempfile_needs_to_be_removed = 1;

    if ((unwind.instream = fopen (unwind.tempfile, "rb")) == NULL)
      report_file_error ("Opening ImageMagick temp file",
			 list1 (build_string (unwind.tempfile)));
  }

  /* Initialize structures and read in the image */
  GetImageInfo(&image_info);
  strcpy(image_info.filename,unwind.tempfile);
  unwind.image = ReadImage(&image_info);

  if (unwind.image == (Image *) NULL) {
    signal_simple_error ("Unable to read image.",instantiator);
  }

#if 1
  /*
   * For now, force dithering everything, and deal with all images as if they
   * were PseudoClass images
   */
  if (unwind.image->class != PseudoClass) {
    QuantizeInfo quantize_info;
    GetQuantizeInfo(&quantize_info);
    quantize_info.number_colors=256;
    quantize_info.tree_depth=8;
    quantize_info.dither=True;
    quantize_info.colorspace=RGBColorspace;
    QuantizeImage(&quantize_info, unwind.image);
    SyncImage(unwind.image);
    /* #### It would probably be a good idea to sort the colormap by popularity,
     * so that in case we run out of entries in the map, it will likely be on
     * the less used colors
     */
  } else {
    CompressColormap(unwind.image);
    SyncImage(unwind.image);
  }
  
#endif

#if 0
  DescribeImage(unwind.image,stderr,1);
#endif

  unwind.ximage = XCreateImage(dpy, visual, depth,
			       (depth == 1) ? XYPixmap : ZPixmap,
			       0, 0,
			       unwind.image->columns,
			       unwind.image->rows,
			       XBitmapPad(dpy), 0);

  if (!unwind.ximage) {
    signal_simple_error("Unable to allocate XImage structure",
			instantiator);
  }

  unwind.ximage->data = (char *) xmalloc(unwind.ximage->bytes_per_line *
					 unwind.ximage->height);

  if (unwind.ximage->data == (char *)NULL) {
    signal_simple_error("Unable to allocate XImage data information",
			instantiator);
  }

  
  /*
  ** First pull out all of the colors used, and create a lookup for them
  */

  if (unwind.image->class == PseudoClass) {
    int i;

    unwind.npixels = unwind.image->colors;
    unwind.pixels = xmalloc(unwind.npixels * sizeof(unsigned long));
    for (i = 0; i < unwind.npixels; i++) {
      XColor color;
      /* ImageMagic uses 8bit values for colors, whilst X expects 16bits */
      color.red = unwind.image->colormap[i].red << 8;
      color.green = unwind.image->colormap[i].green << 8;
      color.blue = unwind.image->colormap[i].blue << 8;
      color.flags = DoRed | DoGreen | DoBlue;
      allocate_nearest_color (dpy, cmap, visual, &color);
      unwind.pixels[i] = color.pixel;
    }
  }
  
  /*
  ** Need to pull the data from the 'Image' structure in
  ** unwind.image and convert it to an 'XImage' in unwind.ximage
  */
  {
    int i,j,x,b;
    unsigned int bytes_per_pixel, scanline_pad;
    unsigned long pixval;
    unsigned char *q, pixar[3];
    RunlengthPacket *p;

    q = (unsigned char *) unwind.ximage->data;
    x  = 0;
    p = unwind.image->pixels;
    scanline_pad = unwind.ximage->bytes_per_line -
      ((unwind.ximage->width * unwind.ximage->bits_per_pixel) >> 3);

    /* Convert to multi-byte color-mapped X image. */
    bytes_per_pixel=unwind.ximage->bits_per_pixel >> 3;

    for (i=0; i < unwind.image->packets; i++) {
      if (unwind.image->class == PseudoClass) 
	pixval = unwind.pixels[p->index];
      else
	{
	  /* ### NOW what? */
	  pixval = 0;
	}

      for (b=0; b < bytes_per_pixel; b++) {
	if (unwind.ximage->bitmap_bit_order == LSBFirst) 
	  pixar[b] = (unsigned char)pixval;
	else
	  pixar[bytes_per_pixel - 1 - b] = (unsigned char)pixval;
	pixval>>=8;
      }

      for (j=0; j <= ((int) p->length); j++) {
	for (b=0; b < bytes_per_pixel; b++) 
	  *q++= pixar[b];
	x++;
	if (x == unwind.ximage->width) {
	  x=0;
	  q+=scanline_pad;
	}
      }
      p++;
    }
  }

  init_image_instance_from_x_image (ii, unwind.ximage, dest_mask,
				    unwind.pixels, unwind.npixels,
				    instantiator);

  /* And we are done!
  ** Now that we've succeeded, we don't want the pixels
  ** freed right now.  They're kept around in the image instance
  ** structure until it's destroyed.
  */
  unwind.npixels = 0;
  unbind_to (speccount, Qnil);
}

#endif /* HAVE_IMAGEMAGICK */


#ifdef HAVE_XFACE

/**********************************************************************
 *                             X-Face                                 *
 **********************************************************************/

static void
xface_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
xface_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  /* This funcation can call lisp */
  Lisp_Object file = Qnil, mask_file = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object alist = Qnil;

  GCPRO3 (file, mask_file, alist);

  /* Now, convert any file data into inline data for both the regular
     data and the mask data.  At the end of this, `data' will contain
     the inline data (if any) or Qnil, and `file' will contain
     the name this data was derived from (if known) or Qnil.
     Likewise for `mask_file' and `mask_data'.

     Note that if we cannot generate any regular inline data, we
     skip out. */

  file = potential_pixmap_file_instantiator (inst, Q_file, Q_data);
  mask_file = potential_pixmap_file_instantiator (inst, Q_mask_file,
						  Q_mask_data);

  if (CONSP (file)) /* failure locating filename */
    signal_double_file_error ("Opening bitmap file",
			      "no such file or directory",
			      Fcar (file));

  if (NILP (file) && NILP (mask_file)) /* no conversion necessary */
    RETURN_UNGCPRO (inst);

  alist = tagged_vector_to_alist (inst);

  {
    Lisp_Object data = make_string_from_file (file);
    alist = remassq_no_quit (Q_file, alist);
    /* there can't be a :data at this point. */
    alist = Fcons (Fcons (Q_file, file),
		   Fcons (Fcons (Q_data, data), alist));
  }

  alist = xbm_mask_file_munging (alist, file, mask_file);

  {
    Lisp_Object result = alist_to_tagged_vector (Qxface, alist);
    free_alist (alist);
    RETURN_UNGCPRO (result);
  }
}

static int
xface_possible_dest_types (void)
{
  return
    IMAGE_MONO_PIXMAP_MASK  |
    IMAGE_COLOR_PIXMAP_MASK |
    IMAGE_POINTER_MASK;
}

#if defined(EXTERN)
/* This is about to get redefined! */
#undef EXTERN
#endif
/* We have to define SYSV32 so that compface.h includes string.h
   instead of strings.h. */
#define SYSV32
#ifdef __cplusplus
extern "C" {
#endif
#include <compface.h>
#ifdef __cplusplus
}
#endif
/* JMP_BUF cannot be used here because if it doesn't get defined
   to jmp_buf we end up with a conflicting type error with the
   definition in compface.h */
extern jmp_buf comp_env;
#undef SYSV32

static void
xface_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		   Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		   int dest_mask, Lisp_Object domain)
{
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  int i, stattis;
  char *p, *bits, *bp;
  CONST char * volatile emsg = 0;
  CONST char * volatile dstring;

  assert (!NILP (data));

  GET_C_STRING_BINARY_DATA_ALLOCA (data, dstring);

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
    signal_simple_error_2 (emsg, data, Qimage);

  bp = bits = (char *) alloca (PIXELS / 8);

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
 *			 Autodetect		                         *
 **********************************************************************/

static void
autodetect_validate (Lisp_Object instantiator)
{
  data_must_be_present (instantiator);
}

static Lisp_Object
autodetect_normalize (Lisp_Object instantiator,
				Lisp_Object console_type)
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

	  alist = xbm_mask_file_munging (alist, filename, Qnil);

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
      CONST char *name_ext;
      GET_C_STRING_FILENAME_DATA_ALLOCA (data, name_ext);
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
XLoadFont_error_handler (Display *dpy, XErrorEvent *xerror)
{
  XLoadFont_got_error = 1;
  return 0;
}

static Font
safe_XLoadFont (Display *dpy, char *name)
{
  Font font;
  int (*old_handler) (Display *, XErrorEvent *);
  XLoadFont_got_error = 0;
  XSync (dpy, 0);
  old_handler = XSetErrorHandler (XLoadFont_error_handler);
  font = XLoadFont (dpy, name);
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
		  int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  XColor fg, bg;
  Font source, mask;
  char source_name[MAXPATHLEN], mask_name[MAXPATHLEN], dummy;
  int source_char, mask_char;
  int count;
  Lisp_Object foreground, background;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  if (!STRINGP (data) ||
      strncmp ("FONT ", (char *) XSTRING_DATA (data), 5))
    signal_simple_error ("Invalid font-glyph instantiator",
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
    signal_simple_error ("invalid cursor specification", data);
  source = safe_XLoadFont (dpy, source_name);
  if (! source)
    signal_simple_error_2 ("couldn't load font",
			   build_string (source_name),
			   data);
  if (count == 2)
    mask = 0;
  else if (!mask_name[0])
    mask = source;
  else
    {
      mask = safe_XLoadFont (dpy, mask_name);
      if (!mask)
	/* continuable */
	Fsignal (Qerror, list3 (build_string ("couldn't load font"),
				build_string (mask_name), data));
    }
  if (!mask)
    mask_char = 0;

  /* #### call XQueryTextExtents() and check_pointer_sizes() here. */

  x_initialize_pixmap_image_instance (ii, IMAGE_POINTER);
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
			 int dest_mask, Lisp_Object domain)
{
  /* This function can GC */
  Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  int i;
  CONST char *name_ext;
  Lisp_Object foreground, background;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  if (!(dest_mask & IMAGE_POINTER_MASK))
    incompatible_image_types (instantiator, dest_mask, IMAGE_POINTER_MASK);

  GET_C_STRING_FILENAME_DATA_ALLOCA (data, name_ext);
  if ((i = XmuCursorNameToIndex (name_ext)) == -1)
    signal_simple_error ("Unrecognized cursor-font name", data);

  x_initialize_pixmap_image_instance (ii, IMAGE_POINTER);
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
  struct Lisp_Image_Instance *p;

  p = XIMAGE_INSTANCE (image_instance);

  switch (IMAGE_INSTANCE_TYPE (p))
    {
    case IMAGE_MONO_PIXMAP:
      IMAGE_INSTANCE_TYPE (p) = IMAGE_COLOR_PIXMAP;
      /* Make sure there aren't two pointers to the same mask, causing
	 it to get freed twice. */
      IMAGE_INSTANCE_X_MASK (p) = 0;
      break;

    default:
      return 0;
    }

  {
    Display *dpy = DEVICE_X_DISPLAY (XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
    Drawable draw = XtWindow(DEVICE_XT_APP_SHELL (XDEVICE (IMAGE_INSTANCE_DEVICE (p))));
    Dimension d = DEVICE_X_DEPTH (XDEVICE (IMAGE_INSTANCE_DEVICE (p)));
    Pixmap new = XCreatePixmap (dpy, draw,
				IMAGE_INSTANCE_PIXMAP_WIDTH (p),
				IMAGE_INSTANCE_PIXMAP_HEIGHT (p), d);
    XColor color;
    XGCValues gcv;
    GC gc;
    color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (foreground));
    gcv.foreground = color.pixel;
    color = COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (background));
    gcv.background = color.pixel;
    gc = XCreateGC (dpy, new, GCBackground|GCForeground, &gcv);
    XCopyPlane (dpy, IMAGE_INSTANCE_X_PIXMAP (p), new, gc, 0, 0,
		IMAGE_INSTANCE_PIXMAP_WIDTH (p),
		IMAGE_INSTANCE_PIXMAP_HEIGHT (p),
		0, 0, 1);
    XFreeGC (dpy, gc);
    IMAGE_INSTANCE_X_PIXMAP (p) = new;
    IMAGE_INSTANCE_PIXMAP_DEPTH (p) = d;
    IMAGE_INSTANCE_PIXMAP_FG (p) = foreground;
    IMAGE_INSTANCE_PIXMAP_BG (p) = background;
    return 1;
  }
}


#if HAVE_SUBWINDOWS
/************************************************************************/
/*                               subwindows                             */
/************************************************************************/

Lisp_Object Qsubwindowp;
static Lisp_Object mark_subwindow (Lisp_Object, void (*) (Lisp_Object));
static void print_subwindow (Lisp_Object, Lisp_Object, int);
static void finalize_subwindow (void *, int);
static int subwindow_equal (Lisp_Object o1, Lisp_Object o2, int depth);
static unsigned long subwindow_hash (Lisp_Object obj, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("subwindow", subwindow,
			       mark_subwindow, print_subwindow,
			       finalize_subwindow, subwindow_equal,
			       subwindow_hash, struct Lisp_Subwindow);

static Lisp_Object
mark_subwindow (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Subwindow *sw = XSUBWINDOW (obj);
  return sw->frame;
}

static void
print_subwindow (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  struct Lisp_Subwindow *sw = XSUBWINDOW (obj);
  struct frame *frm = XFRAME (sw->frame);

  if (print_readably)
    error ("printing unreadable object #<subwindow 0x%x>",
	   sw->header.uid);

  write_c_string ("#<subwindow", printcharfun);
  sprintf (buf, " %dx%d", sw->width, sw->height);
  write_c_string (buf, printcharfun);

  /* This is stolen from frame.c.  Subwindows are strange in that they
     are specific to a particular frame so we want to print in their
     description what that frame is. */

  write_c_string (" on #<", printcharfun);
  if (!FRAME_LIVE_P (frm))
    write_c_string ("dead", printcharfun);
  else if (FRAME_TTY_P (frm))
    write_c_string ("tty", printcharfun);
  else if (FRAME_X_P (frm))
    write_c_string ("x", printcharfun);
  else
    write_c_string ("UNKNOWN", printcharfun);
  write_c_string ("-frame ", printcharfun);
  print_internal (frm->name, printcharfun, 1);
  sprintf (buf, " 0x%x>", frm->header.uid);
  write_c_string (buf, printcharfun);

  sprintf (buf, ") 0x%x>", sw->header.uid);
  write_c_string (buf, printcharfun);
}

static void
finalize_subwindow (void *header, int for_disksave)
{
  struct Lisp_Subwindow *sw = (struct Lisp_Subwindow *) header;
  if (for_disksave) finalose (sw);
  if (sw->subwindow)
    {
      XDestroyWindow (DisplayOfScreen (sw->xscreen), sw->subwindow);
      sw->subwindow = 0;
    }
}

/* subwindows are equal iff they have the same window XID */
static int
subwindow_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (XSUBWINDOW (o1)->subwindow == XSUBWINDOW (o2)->subwindow);
}

static unsigned long
subwindow_hash (Lisp_Object obj, int depth)
{
  return XSUBWINDOW (obj)->subwindow;
}

/* #### PROBLEM: The display routines assume that the glyph is only
 being displayed in one buffer.  If it is in two different buffers
 which are both being displayed simultaneously you will lose big time.
 This can be dealt with in the new redisplay. */

/* #### These are completely un-re-implemented in 19.14.  Get it done
   for 19.15. */

DEFUN ("make-subwindow", Fmake_subwindow, 0, 3, 0, /*
Creates a new `subwindow' object of size WIDTH x HEIGHT.
The default is a window of size 1x1, which is also the minimum allowed
window size.  Subwindows are per-frame.  A buffer being shown in two
different frames will only display a subwindow glyph in the frame in
which it was actually created.  If two windows on the same frame are
displaying the buffer then the most recently used window will actually
display the window.  If the frame is not specified, the selected frame
is used.

Subwindows are not currently implemented.
*/
       (width, height, frame))
{
  Display *dpy;
  Screen *xs;
  Window pw;
  struct frame *f;
  unsigned int iw, ih;
  XSetWindowAttributes xswa;
  Mask valueMask = 0;

  error ("subwindows are not functional in 20.2; they may be again someday");

  f = decode_x_frame (frame);

  xs = LISP_DEVICE_TO_X_SCREEN (FRAME_DEVICE (f));
  dpy = DisplayOfScreen (xs);
  pw = XtWindow (FRAME_X_TEXT_WIDGET (f));

  if (NILP (width))
    iw = 1;
  else
    {
      CHECK_INT (width);
      iw = XINT (width);
      if (iw < 1) iw = 1;
    }
  if (NILP (height))
    ih = 1;
  else
    {
      CHECK_INT (height);
      ih = XINT (height);
      if (ih < 1) ih = 1;
    }

  {
    struct Lisp_Subwindow *sw =
      alloc_lcrecord_type (struct Lisp_Subwindow, lrecord_subwindow);
    Lisp_Object val;
    sw->frame = frame;
    sw->xscreen = xs;
    sw->parent_window = pw;
    sw->height = ih;
    sw->width = iw;

    xswa.backing_store = Always;
    valueMask |= CWBackingStore;

    xswa.colormap = DefaultColormapOfScreen (xs);
    valueMask |= CWColormap;

    sw->subwindow = XCreateWindow (dpy, pw, 0, 0, iw, ih, 0, CopyFromParent,
				   InputOutput, CopyFromParent, valueMask,
				   &xswa);

    XSETSUBWINDOW (val, sw);
    return val;
  }
}

/* #### Should this function exist? */
DEFUN ("change-subwindow-property", Fchange_subwindow_property, 3, 3, 0, /*
For the given SUBWINDOW, set PROPERTY to DATA, which is a string.
Subwindows are not currently implemented.
*/
       (subwindow, property, data))
{
  Atom property_atom;
  struct Lisp_Subwindow *sw;
  Display *dpy;

  CHECK_SUBWINDOW (subwindow);
  CHECK_STRING (property);
  CHECK_STRING (data);

  sw = XSUBWINDOW (subwindow);
  dpy = DisplayOfScreen (LISP_DEVICE_TO_X_SCREEN
			 (FRAME_DEVICE (XFRAME (sw->frame))));

  property_atom = XInternAtom (dpy, (char *) XSTRING_DATA (property), False);
  XChangeProperty (dpy, sw->subwindow, property_atom, XA_STRING, 8,
		   PropModeReplace,
		   XSTRING_DATA   (data),
		   XSTRING_LENGTH (data));

  return (property);
}

DEFUN ("subwindowp", Fsubwindowp, 1, 1, 0, /*
Return non-nil if OBJECT is a subwindow.
Subwindows are not currently implemented.
*/
       (object))
{
  return (SUBWINDOWP (object) ? Qt : Qnil);
}

DEFUN ("subwindow-width", Fsubwindow_width, 1, 1, 0, /*
Width of SUBWINDOW.
Subwindows are not currently implemented.
*/
       (subwindow))
{
  CHECK_SUBWINDOW (subwindow);
  return (make_int (XSUBWINDOW (subwindow)->width));
}

DEFUN ("subwindow-height", Fsubwindow_height, 1, 1, 0, /*
Height of SUBWINDOW.
Subwindows are not currently implemented.
*/
       (subwindow))
{
  CHECK_SUBWINDOW (subwindow);
  return (make_int (XSUBWINDOW (subwindow)->height));
}

DEFUN ("subwindow-xid", Fsubwindow_xid, 1, 1, 0, /*
Return the xid of SUBWINDOW as a number.
Subwindows are not currently implemented.
*/
       (subwindow))
{
  CHECK_SUBWINDOW (subwindow);
  return (make_int (XSUBWINDOW (subwindow)->subwindow));
}

DEFUN ("resize-subwindow", Fresize_subwindow, 1, 3, 0, /*
Resize SUBWINDOW to WIDTH x HEIGHT.
If a value is nil that parameter is not changed.
Subwindows are not currently implemented.
*/
       (subwindow, width, height))
{
  int neww, newh;
  struct Lisp_Subwindow *sw;

  CHECK_SUBWINDOW (subwindow);
  sw = XSUBWINDOW (subwindow);

  if (NILP (width))
    neww = sw->width;
  else
    neww = XINT (width);

  if (NILP (height))
    newh = sw->height;
  else
    newh = XINT (height);

  XResizeWindow (DisplayOfScreen (sw->xscreen), sw->subwindow, neww, newh);

  sw->height = newh;
  sw->width = neww;

  return subwindow;
}

DEFUN ("force-subwindow-map", Fforce_subwindow_map, 1, 1, 0, /*
Generate a Map event for SUBWINDOW.
Subwindows are not currently implemented.
*/
       (subwindow))
{
  CHECK_SUBWINDOW (subwindow);

  XMapWindow (DisplayOfScreen (XSUBWINDOW (subwindow)->xscreen),
	      XSUBWINDOW (subwindow)->subwindow);

  return subwindow;
}
#endif

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_glyphs_x (void)
{
#if HAVE_SUBWINDOWS
  defsymbol (&Qsubwindowp, "subwindowp");

  DEFSUBR (Fmake_subwindow);
  DEFSUBR (Fchange_subwindow_property);
  DEFSUBR (Fsubwindowp);
  DEFSUBR (Fsubwindow_width);
  DEFSUBR (Fsubwindow_height);
  DEFSUBR (Fsubwindow_xid);
  DEFSUBR (Fresize_subwindow);
  DEFSUBR (Fforce_subwindow_map);
#endif

  defkeyword (&Q_mask_file, ":mask-file");
  defkeyword (&Q_mask_data, ":mask-data");
  defkeyword (&Q_hotspot_x, ":hotspot-x");
  defkeyword (&Q_hotspot_y, ":hotspot-y");
  defkeyword (&Q_foreground, ":foreground");
  defkeyword (&Q_background, ":background");

#ifdef HAVE_XPM
  defkeyword (&Q_color_symbols, ":color-symbols");
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
}

void
image_instantiator_format_create_glyphs_x (void)
{
  /* image-instantiator types */

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (xbm, "xbm");

  IIFORMAT_HAS_METHOD (xbm, validate);
  IIFORMAT_HAS_METHOD (xbm, normalize);
  IIFORMAT_HAS_METHOD (xbm, possible_dest_types);
  IIFORMAT_HAS_METHOD (xbm, instantiate);

  IIFORMAT_VALID_KEYWORD (xbm, Q_data, check_valid_xbm_inline);
  IIFORMAT_VALID_KEYWORD (xbm, Q_file, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xbm, Q_mask_data, check_valid_xbm_inline);
  IIFORMAT_VALID_KEYWORD (xbm, Q_mask_file, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xbm, Q_hotspot_x, check_valid_int);
  IIFORMAT_VALID_KEYWORD (xbm, Q_hotspot_y, check_valid_int);
  IIFORMAT_VALID_KEYWORD (xbm, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xbm, Q_background, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (cursor_font, "cursor-font");

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

  IIFORMAT_VALID_KEYWORD (font, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (font, Q_background, check_valid_string);

#ifdef HAVE_IMAGEMAGICK
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (imagick, "imagick");

  IIFORMAT_HAS_METHOD (imagick, validate);
  IIFORMAT_HAS_METHOD (imagick, normalize);
  IIFORMAT_HAS_METHOD (imagick, possible_dest_types);
  IIFORMAT_HAS_METHOD (imagick, instantiate);

  IIFORMAT_VALID_KEYWORD (imagick, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (imagick, Q_file, check_valid_string);

#ifdef OLDCOMPAT /* old graphics compatibility */
#define IIFORMAT_USES_METHOD(format, source, m) \
  (format##_image_instantiator_methods->m##_method = source##_##m)

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (tiff, "tiff");
  IIFORMAT_USES_METHOD (tiff, imagick, validate);
  IIFORMAT_USES_METHOD (tiff, imagick, normalize);
  IIFORMAT_USES_METHOD (tiff, imagick, possible_dest_types);
  IIFORMAT_USES_METHOD (tiff, imagick, instantiate);
  IIFORMAT_VALID_KEYWORD (tiff, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (tiff, Q_file, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (png, "png");
  IIFORMAT_USES_METHOD (png, imagick, validate);
  IIFORMAT_USES_METHOD (png, imagick, normalize);
  IIFORMAT_USES_METHOD (png, imagick, possible_dest_types);
  IIFORMAT_USES_METHOD (png, imagick, instantiate);
  IIFORMAT_VALID_KEYWORD (png, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (png, Q_file, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (gif, "gif");
  IIFORMAT_USES_METHOD (gif, imagick, validate);
  IIFORMAT_USES_METHOD (gif, imagick, normalize);
  IIFORMAT_USES_METHOD (gif, imagick, possible_dest_types);
  IIFORMAT_USES_METHOD (gif, imagick, instantiate);
  IIFORMAT_VALID_KEYWORD (gif, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (gif, Q_file, check_valid_string);

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (jpeg, "jpeg");
  IIFORMAT_USES_METHOD (jpeg, imagick, validate);
  IIFORMAT_USES_METHOD (jpeg, imagick, normalize);
  IIFORMAT_USES_METHOD (jpeg, imagick, possible_dest_types);
  IIFORMAT_USES_METHOD (jpeg, imagick, instantiate);
  IIFORMAT_VALID_KEYWORD (jpeg, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (jpeg, Q_file, check_valid_string);

#endif /* old compat */

#endif

#ifdef HAVE_XPM
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (xpm, "xpm");

  IIFORMAT_HAS_METHOD (xpm, validate);
  IIFORMAT_HAS_METHOD (xpm, normalize);
  IIFORMAT_HAS_METHOD (xpm, possible_dest_types);
  IIFORMAT_HAS_METHOD (xpm, instantiate);

  IIFORMAT_VALID_KEYWORD (xpm, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xpm, Q_file, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xpm, Q_color_symbols, check_valid_xpm_color_symbols);
#endif

#ifdef HAVE_XFACE
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (xface, "xface");

  IIFORMAT_HAS_METHOD (xface, validate);
  IIFORMAT_HAS_METHOD (xface, normalize);
  IIFORMAT_HAS_METHOD (xface, possible_dest_types);
  IIFORMAT_HAS_METHOD (xface, instantiate);

  IIFORMAT_VALID_KEYWORD (xface, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xface, Q_file, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xface, Q_hotspot_x, check_valid_int);
  IIFORMAT_VALID_KEYWORD (xface, Q_hotspot_y, check_valid_int);
  IIFORMAT_VALID_KEYWORD (xface, Q_foreground, check_valid_string);
  IIFORMAT_VALID_KEYWORD (xface, Q_background, check_valid_string);
#endif

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (autodetect,
					"autodetect");

  IIFORMAT_HAS_METHOD (autodetect, validate);
  IIFORMAT_HAS_METHOD (autodetect, normalize);
  IIFORMAT_HAS_METHOD (autodetect, possible_dest_types);
  IIFORMAT_HAS_METHOD (autodetect, instantiate);

  IIFORMAT_VALID_KEYWORD (autodetect, Q_data, check_valid_string);
}

void
vars_of_glyphs_x (void)
{
#ifdef HAVE_XPM
  Fprovide (Qxpm);

  DEFVAR_LISP ("xpm-color-symbols", &Vxpm_color_symbols /*
Definitions of logical color-names used when reading XPM files.
Elements of this list should be of the form (COLOR-NAME FORM-TO-EVALUATE).
The COLOR-NAME should be a string, which is the name of the color to define;
the FORM should evaluate to a `color' specifier object, or a string to be
passed to `make-color-instance'.  If a loaded XPM file references a symbolic
color called COLOR-NAME, it will display as the computed color instead.

The default value of this variable defines the logical color names
\"foreground\" and \"background\" to be the colors of the `default' face.
*/ );
  Vxpm_color_symbols = Qnil; /* initialized in x-faces.el */
#endif

#ifdef HAVE_IMAGEMAGICK
  Fprovide (Qimagick);

#ifdef OLDCOMPAT
  Fprovide (Qtiff);
  Fprovide (Qpng);
  Fprovide (Qgif);
  Fprovide (Qjpeg);
#endif
#endif

#ifdef HAVE_XFACE
  Fprovide (Qxface);
#endif

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
	      list3 (make_int (name##_width),			\
		     make_int (name##_height),			\
		     make_ext_string (name##_bits,		\
				      sizeof (name##_bits),	\
				      FORMAT_BINARY))),		\
     Qglobal, Qx, Qnil)

  BUILD_GLYPH_INST (Vtruncation_glyph, truncator);
  BUILD_GLYPH_INST (Vcontinuation_glyph, continuer);
  BUILD_GLYPH_INST (Vxemacs_logo, xemacs);
  BUILD_GLYPH_INST (Vhscroll_glyph, hscroll);

#undef BUILD_GLYPH_INST
}
