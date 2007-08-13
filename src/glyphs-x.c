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
   TIFF Support
   Loadable module support for images
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

#ifdef HAVE_PNG
#include <png.h>
#else
#include <setjmp.h>
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

#ifdef HAVE_JPEG
DEFINE_IMAGE_INSTANTIATOR_FORMAT (jpeg);
Lisp_Object Qjpeg;
#endif

#ifdef HAVE_PNG
DEFINE_IMAGE_INSTANTIATOR_FORMAT (png);
Lisp_Object Qpng;
#endif

#ifdef HAVE_TIFF
DEFINE_IMAGE_INSTANTIATOR_FORMAT (tiff);
Lisp_Object Qtiff;
#endif

#ifdef HAVE_GIF
DEFINE_IMAGE_INSTANTIATOR_FORMAT (gif);
Lisp_Object Qgif;
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
      Screen *scr = LISP_DEVICE_TO_X_SCREEN (IMAGE_INSTANCE_DEVICE (p));

      if (IMAGE_INSTANCE_X_PIXMAP (p))
	XFreePixmap (DisplayOfScreen (scr), IMAGE_INSTANCE_X_PIXMAP (p));
      if (IMAGE_INSTANCE_X_MASK (p) &&
	  IMAGE_INSTANCE_X_MASK (p) != IMAGE_INSTANCE_X_PIXMAP (p))
	XFreePixmap (DisplayOfScreen (scr), IMAGE_INSTANCE_X_MASK (p));
      IMAGE_INSTANCE_X_PIXMAP (p) = 0;
      IMAGE_INSTANCE_X_MASK (p) = 0;

      if (IMAGE_INSTANCE_X_CURSOR (p))
	{
	  XFreeCursor (DisplayOfScreen (scr), IMAGE_INSTANCE_X_CURSOR (p));
	  IMAGE_INSTANCE_X_CURSOR (p) = 0;
	}

      if (IMAGE_INSTANCE_X_NPIXELS (p) != 0)
	{
	  XFreeColors (DisplayOfScreen (scr),
		       DefaultColormapOfScreen (scr),
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
      break;
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
#endif
      return 0;
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
  ii->data = malloc_type_and_zero (struct x_image_instance_data);
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
  Screen *xs;
  GC gc;
  Drawable d;
  Pixmap pixmap;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  xs = DefaultScreenOfDisplay (dpy);
  d = RootWindowOfScreen (xs);

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
  Screen *screen = LISP_DEVICE_TO_X_SCREEN (device);
  return XCreatePixmapFromBitmapData (DisplayOfScreen (screen),
				      RootWindowOfScreen (screen),
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
  enum image_instance_type type;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
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
	Dimension d = DefaultDepthOfScreen (scr);
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
	  XCreatePixmapFromBitmapData (DisplayOfScreen (scr),
				       RootWindowOfScreen (scr),
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
	  XCreatePixmapFromBitmapData (DisplayOfScreen (scr),
				       RootWindowOfScreen (scr),
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


#ifdef HAVE_JPEG

/**********************************************************************
 *                             JPEG                                   *
 **********************************************************************/

#include "jpeglib.h"
#include "jerror.h"

/* The in-core jpeg code doesn't work, so I'm avoiding it for now.  -sb  */
/* Late-breaking update, we're going to give it a try, I think it's */
/* fixed now -sb */
/* #define USE_TEMP_FILES_FOR_JPEG_IMAGES 1 */

static void
jpeg_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
jpeg_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  return simple_image_type_normalize (inst, console_type, Qjpeg);
}

static int
jpeg_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

/* To survive the otherwise baffling complexity of making sure
   everything gets cleaned up in the presence of an error, we
   use an unwind_protect(). */

struct jpeg_unwind_data
{
  Display *dpy;
  /* Stream that we need to close */
  FILE *instream;
  /* Object that holds state info for JPEG decoding */
  struct jpeg_decompress_struct *cinfo_ptr;
  /* Pixels to keep around while the image is active */
  unsigned long *pixels;
  int npixels;
  /* Client-side image structure */
  XImage *ximage;
  /* Tempfile to remove */
  char tempfile[50];
  int tempfile_needs_to_be_removed;
};

static Lisp_Object
jpeg_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct jpeg_unwind_data *data =
    (struct jpeg_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->cinfo_ptr)
    jpeg_destroy_decompress (data->cinfo_ptr);

  if (data->instream)
    fclose (data->instream);

  if (data->tempfile_needs_to_be_removed)
    unlink (data->tempfile);

  if (data->npixels > 0)
    {
      Screen *scr = DefaultScreenOfDisplay (data->dpy);
      Colormap cmap = DefaultColormapOfScreen (scr);
      XFreeColors (data->dpy, cmap, data->pixels, data->npixels, 0L);
      xfree (data->pixels);
    }

  if (data->ximage)
    {
      if (data->ximage->data)
        {
	  xfree (data->ximage->data);
          data->ximage->data = 0;
        }
      XDestroyImage (data->ximage);
    }

  return Qnil;
}

/*
 * ERROR HANDLING:
 *
 * The JPEG library's standard error handler (jerror.c) is divided into
 * several "methods" which you can override individually.  This lets you
 * adjust the behavior without duplicating a lot of code, which you might
 * have to update with each future release.
 *
 * Our example here shows how to override the "error_exit" method so that
 * control is returned to the library's caller when a fatal error occurs,
 * rather than calling exit() as the standard error_exit method does.
 *
 * We use C's setjmp/longjmp facility to return control.  This means that the
 * routine which calls the JPEG library must first execute a setjmp() call to
 * establish the return point.  We want the replacement error_exit to do a
 * longjmp().  But we need to make the setjmp buffer accessible to the
 * error_exit routine.  To do this, we make a private extension of the
 * standard JPEG error handler object.  (If we were using C++, we'd say we
 * were making a subclass of the regular error handler.)
 *
 * Here's the extended error handler struct:
 */

struct my_jpeg_error_mgr
{
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
};

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
our_init_source (j_decompress_ptr cinfo) {
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(boolean)
#else
METHODDEF boolean
#endif
our_fill_input_buffer (j_decompress_ptr cinfo) {
  /* Insert a fake EOI marker */
  struct jpeg_source_mgr *src = (struct jpeg_source_mgr *) cinfo->src;
  static JOCTET buffer[2];

  buffer[0] = (JOCTET) 0xFF;
  buffer[1] = (JOCTET) JPEG_EOI;

  src->next_input_byte = buffer;
  src->bytes_in_buffer = 2;
  return TRUE;
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
our_skip_input_data (j_decompress_ptr cinfo, long num_bytes) {
  struct jpeg_source_mgr *src = NULL;

  src = (struct jpeg_source_mgr *) cinfo->src;

  if (!src) {
    return;
  } else if (num_bytes > src->bytes_in_buffer) {
    num_bytes = (long)src->bytes_in_buffer;
  }

  src->bytes_in_buffer -= num_bytes;
  src->next_input_byte += num_bytes;
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
our_term_source (j_decompress_ptr cinfo) {
}

typedef struct {
  struct jpeg_source_mgr pub;
} our_jpeg_source_mgr;

static void jpeg_memory_src (j_decompress_ptr cinfo, JOCTET *data,
			     unsigned int len)
{
  struct jpeg_source_mgr *src = NULL;
  
  if (cinfo->src == NULL) {	/* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(our_jpeg_source_mgr));
    src = (struct jpeg_source_mgr *) cinfo->src;
    src->next_input_byte = data;
  }
  src = (struct jpeg_source_mgr *) cinfo->src;
  src->init_source = our_init_source;
  src->fill_input_buffer = our_fill_input_buffer;
  src->skip_input_data = our_skip_input_data;
  src->resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->term_source = our_term_source;
  src->bytes_in_buffer = len;
  src->next_input_byte = data;
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
my_jpeg_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  struct my_jpeg_error_mgr *myerr = (struct my_jpeg_error_mgr *) cinfo->err;

  /* Return control to the setjmp point */
  longjmp (myerr->setjmp_buffer, 1);
}

/* The code in this routine is based on example.c from the JPEG library
   source code and from gif_instantiate() */
static void
jpeg_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  Screen *scr;
  /* It is OK for the unwind data to be local to this function,
     because the unwind-protect is always executed when this
     stack frame is still valid. */
  struct jpeg_unwind_data unwind;
  int speccount = specpdl_depth ();

  /* This struct contains the JPEG decompression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  /* We use our private extension JPEG error handler.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_jpeg_error_mgr jerr;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  scr = DefaultScreenOfDisplay (dpy);

  /* Step -1: First record our unwind-protect, which will clean up after
     any exit, normal or not */

  memset (&unwind, 0, sizeof (unwind));
  unwind.dpy = dpy;
  record_unwind_protect (jpeg_instantiate_unwind, make_opaque_ptr (&unwind));

#ifdef USE_TEMP_FILES_FOR_JPEG_IMAGES
  /* Step 0: Write out to a temp file.

     The JPEG routines require you to read from a file unless
     you provide your own special input handlers, which I don't
     feel like doing. */
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

    assert (!NILP (data));

    write_lisp_string_to_temp_file (data, unwind.tempfile);
    unwind.tempfile_needs_to_be_removed = 1;

    /* VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
     * requires it in order to read binary files.
     */

    if ((unwind.instream = fopen (unwind.tempfile, "r")) == NULL)
      report_file_error ("Opening JPEG temp file",
			 list1 (build_string (unwind.tempfile)));
  }
#endif
  
  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error (&jerr.pub);
  jerr.pub.error_exit = my_jpeg_error_exit;

  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp (jerr.setjmp_buffer))
    {
      /* If we get here, the JPEG code has signaled an error.
       * We need to clean up the JPEG object, close the input file, and return.
       */

      {
	Lisp_Object errstring;
	char buffer[JMSG_LENGTH_MAX];
	
	/* Create the message */
	(*cinfo.err->format_message) ((j_common_ptr) &cinfo, buffer);
	errstring = build_string (buffer);
	
	signal_simple_error_2 ("JPEG decoding error",
			       errstring, instantiator);
      }
    }

  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress (&cinfo);
  unwind.cinfo_ptr = &cinfo;

  /* Step 2: specify data source (eg, a file) */

#ifdef USE_TEMP_FILES_FOR_JPEG_IMAGES
  jpeg_stdio_src (&cinfo, unwind.instream);
#else
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
    Extbyte *bytes;
    Extcount len;
  
    /* #### This is a definite problem under Mule due to the amount of
       stack data it might allocate.  Need to be able to convert and
       write out to a file. */
    GET_STRING_BINARY_DATA_ALLOCA (data, bytes, len);
    jpeg_memory_src (&cinfo, bytes, len);
  }
#endif

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header (&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  /* Step 4: set parameters for decompression.   */

  /* We request that the JPEG file be automatically quantized into
     8-bit color in case it's not already (many JPEGs are stored in
     24-bit color).  "Two-pass quantize" means that the colormap
     is determined on-the-fly for this particular image rather than
     quantizing to a supplied colormap.  We can get away with this
     because we then use allocate_nearest_color().

     #### Note of course that this is not the most color-effective
     way of doing things -- we could quantize an image that has
     lots of very similar colors, and eat up the colormap with these
     (useless to other images) colors.  Unfortunately I don't think
     there's any "general" way of maximizing the overall image
     quality of lots of images, given that we don't know the
     colors of the images until we come across each one.  Best we
     could do would be various sorts of heuristics, which I don't
     feel like dealing with now.  A better scheme would be the
     way things are done under MS Windows, where the colormap is
     dynamically adjusted for various applications; but that kind
     of thing would have to be provided by X, which it isn't. */

  cinfo.quantize_colors = TRUE;
  cinfo.two_pass_quantize = TRUE;
  cinfo.colormap = NULL;

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress (&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* At this point we know the size of the image and the colormap. */

  /* Step 5.33: Allocate the colors */
  {
    int i;

    /* Just in case the image contains out-of-range pixels, we go
       ahead and allocate space for all of them. */
    unwind.pixels = (unsigned long *) xmalloc (256 * sizeof (unsigned long));
    unwind.npixels = cinfo.actual_number_of_colors;

    for (i = 0; i < 256; i++)
      unwind.pixels[i] = 0;   /* Use a reasonable color for out of range. */

    /* Allocate pixels for the various colors. */
    for (i = 0; i < unwind.npixels; i++)
      {
	XColor color;
	int ri, gi, bi;

	ri = 0;
	gi = cinfo.out_color_components > 1 ? 1 : 0;
	bi = cinfo.out_color_components > 2 ? 2 : 0;

	/* Ok... apparently, an entry of cinfo.colormap can be NULL if
	   there are no bits of that color in the image.  How incredibly
	   gross.  Wouldn't it be nice to have exceptions!? */
	color.red = cinfo.colormap[ri] ? cinfo.colormap[ri][i] << 8 : 0;
	color.green = cinfo.colormap[gi] ? cinfo.colormap[gi][i] << 8 : 0;
	color.blue = cinfo.colormap[bi] ? cinfo.colormap[bi][i] << 8 : 0;
	color.flags = DoRed | DoGreen | DoBlue;

	allocate_nearest_color (dpy, DefaultColormapOfScreen (scr), &color);
	unwind.pixels[i] = color.pixel;
      }
  }

  /* Step 5.66: Create the image */
  {
    int height = cinfo.output_height;
    int width = cinfo.output_width;
    int depth;
    int bitmap_pad;
    
    depth = DefaultDepthOfScreen (scr);
    
    /* first get bitmap_pad (from XPM) */
    if (depth > 16)
      bitmap_pad = 32;
    else if (depth > 8)
      bitmap_pad = 16;
    else
      bitmap_pad = 8;
    
    unwind.ximage = XCreateImage (dpy, DefaultVisualOfScreen (scr),
				  depth, ZPixmap, 0, 0, width, height,
				  bitmap_pad, 0);

    if (!unwind.ximage)
      signal_simple_error ("Unable to create X image struct", instantiator);

    /* now that bytes_per_line must have been set properly alloc data */
    unwind.ximage->data =
      (char *) xmalloc (unwind.ximage->bytes_per_line * height);
  }

  /* Step 6: Read in the data and put into image */
  {
    JSAMPARRAY row_buffer;	/* Output row buffer */
    int row_stride;		/* physical row width in output buffer */

    /* We may need to do some setup of our own at this point before reading
     * the data.  After jpeg_start_decompress() we have the correct scaled
     * output image dimensions available, as well as the output colormap
     * if we asked for color quantization.
     * In this example, we need to make an output work buffer of the right size.
     */ 
    /* JSAMPLEs per row in output buffer.
       Since we asked for quantized output, cinfo.output_components
       will always be 1. */
    row_stride = cinfo.output_width * cinfo.output_components;
    /* Make a one-row-high sample array that will go away when done
       with image */
    row_buffer = ((*cinfo.mem->alloc_sarray)
		  ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1));
    
    /* Here we use the library's state variable cinfo.output_scanline as the
     * loop counter, so that we don't have to keep track ourselves.
     */
    while (cinfo.output_scanline < cinfo.output_height)
      {
	int i;
	int scanline = cinfo.output_scanline;

	/* jpeg_read_scanlines expects an array of pointers to scanlines.
	 * Here the array is only one element long, but you could ask for
	 * more than one scanline at a time if that's more convenient.
	 */
	(void) jpeg_read_scanlines (&cinfo, row_buffer, 1);

	for (i = 0; i < cinfo.output_width; i++)
	  XPutPixel (unwind.ximage, i, scanline,
		     /* Let's make sure we avoid getting bit like
			what happened for GIF's.  It's probably the
			case that JSAMPLE's are unsigned chars as
			opposed to chars, but you never know.

			(They could even be shorts if the library
			was compiled with 12-bit samples -- ####
			We should deal with this possibility) */
		     unwind.pixels[(unsigned char) row_buffer[0][i]]);
      }
  }

  /* Step 6.5: Create the pixmap and set up the image instance */
  init_image_instance_from_x_image (ii, unwind.ximage, dest_mask,
				    unwind.pixels, unwind.npixels,
				    instantiator);

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress (&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* And we're done!

     Now that we've succeeded, we don't want the pixels
     freed right now.  They're kept around in the image instance
     structure until it's destroyed. */
  unwind.npixels = 0;

  /* This will clean up everything else. */
  unbind_to (speccount, Qnil);
}

#endif /* HAVE_JPEG */


#ifdef HAVE_GIF

/**********************************************************************
 *                               GIF                                  *
 **********************************************************************/

#include "gif_lib.h" /* This is in our own source tree */

static void
gif_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
gif_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  return simple_image_type_normalize (inst, console_type, Qgif);
}

static int
gif_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

/* To survive the otherwise baffling complexity of making sure
   everything gets cleaned up in the presence of an error, we
   use an unwind_protect(). */

struct gif_unwind_data
{
  Display *dpy;
  /* Object that holds the decoded data from a GIF file */
  GifFileType *giffile;
  /* Pixels to keep around while the image is active */
  unsigned long *pixels;
  int npixels;
  /* Client-side image structure */
  XImage *ximage;
  /* Tempfile to remove */
  char tempfile[50];
  int tempfile_needs_to_be_removed;
};

static Lisp_Object
gif_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct gif_unwind_data *data =
    (struct gif_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->giffile)
    DGifCloseFile (data->giffile);
  if (data->tempfile_needs_to_be_removed)
    unlink (data->tempfile);
  if (data->npixels > 0)
    {
      Screen *scr = DefaultScreenOfDisplay (data->dpy);
      Colormap cmap = DefaultColormapOfScreen (scr);
      XFreeColors (data->dpy, cmap, data->pixels, data->npixels, 0L);
      xfree (data->pixels);
    }
  if (data->ximage)
    {
      if (data->ximage->data)
        {
	  xfree (data->ximage->data);
          data->ximage->data = 0;
        }
      XDestroyImage (data->ximage);
    }

  return Qnil;
}

#if 0
/* We provide our own version of DGifSlurp() because the standardly
   provided one doesn't handle interlaced GIFs.  This is based on
   code in gif2x11.c. */

/* Return value is GIF_ERROR, GIF_OK, or -1.
   #### We are using "forbidden" knowledge that neither of these
   constants is -1. */

static int
our_own_dgif_slurp_from_gif2x11_c (GifFileType *GifFile)
{
  int i, j, Row, Col, Width, Height;
  int ExtCode, Count;
  GifRecordType RecordType;
  GifByteType *Extension;
  SavedImage *sp = NULL;
  static int InterlacedOffset[] = { 0, 4, 2, 1 };
  static int InterlacedJumps[] = { 8, 8, 4, 2 };

  GifPixelType *ScreenBuffer =
    (GifPixelType *) xmalloc (GifFile->SHeight * GifFile->SWidth *
			      sizeof (GifPixelType));
  GifFile->SavedImages = (SavedImage *) xmalloc (sizeof(SavedImage));

  for (i = 0; i < GifFile->SHeight * GifFile->SWidth; i++)
    ScreenBuffer[i] = GifFile->SBackGroundColor;

  /* Scan the content of the GIF file and load the image(s) in: */
  do
    {
      if (DGifGetRecordType (GifFile, &RecordType) == GIF_ERROR)
	return GIF_ERROR;

      switch (RecordType)
	{
	case IMAGE_DESC_RECORD_TYPE:
	  if (DGifGetImageDesc (GifFile) == GIF_ERROR)
	    return GIF_ERROR;

	  sp = &GifFile->SavedImages[GifFile->ImageCount-1];
	  Row = GifFile->Image.Top; /* Image Position relative to Screen. */
	  Col = GifFile->Image.Left;
	  Width = GifFile->Image.Width;
	  Height = GifFile->Image.Height;
	  if (GifFile->Image.Left + GifFile->Image.Width > GifFile->SWidth ||
	      GifFile->Image.Top + GifFile->Image.Height > GifFile->SHeight)
	    return -1;

	  sp->RasterBits = (GifPixelType*) xmalloc(Width * Height *
						   sizeof (GifPixelType));
	  
	  if (GifFile->Image.Interlace)
	    {
	      /* Need to perform 4 passes on the images: */
	      for (Count = i = 0; i < 4; i++)
		for (j = Row + InterlacedOffset[i]; j < Row + Height;
		     j += InterlacedJumps[i])
		  {
		    if (DGifGetLine (GifFile, &sp->RasterBits[j * Width + Col],
				     Width) == GIF_ERROR)
		      return GIF_ERROR;
		  }
	    }
	  else
	    {
	      for (i = 0; i < Height; i++)
		{
		  if (DGifGetLine (GifFile,
				   &sp->RasterBits[(Row++) * Width + Col],
				   Width) == GIF_ERROR)
		    return GIF_ERROR;
		}
	    }

	  /* Only get 1 image from animated gifs. */
	  /* #### if the rest of the file was bad, we still return
	     GIF_OK, since we don't even bother looking at it.  Should
	     probably check for ImageCount == 1 above too, hmm. */
	  goto done;
	  break;

	case EXTENSION_RECORD_TYPE:
	  /* Skip any extension blocks in file: */
	  if (DGifGetExtension (GifFile, &ExtCode, &Extension) == GIF_ERROR)
	    return GIF_ERROR;

	  while (Extension != NULL)
	    {
	      if (DGifGetExtensionNext (GifFile, &Extension) == GIF_ERROR)
		return GIF_ERROR;
	    }
	  break;

	case TERMINATE_RECORD_TYPE:
	  break;

	default:		    /* Should be traps by DGifGetRecordType. */
	  break;
	}
    }
  while (RecordType != TERMINATE_RECORD_TYPE);

 done:

  return GIF_OK;
}
#endif

static void
gif_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  Screen *scr;
  /* It is OK for the unwind data to be local to this function,
     because the unwind-protect is always executed when this
     stack frame is still valid. */
  struct gif_unwind_data unwind;
  int speccount = specpdl_depth ();

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  scr = DefaultScreenOfDisplay (dpy);

  memset (&unwind, 0, sizeof (unwind));
  unwind.dpy = dpy;
  record_unwind_protect (gif_instantiate_unwind, make_opaque_ptr (&unwind));

  /* 1. Now decode the data. */

  /* #### The GIF routines currently require that you read from a file,
     so write out to a temp file.  We should change this. */
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

    assert (!NILP (data));

    write_lisp_string_to_temp_file (data, unwind.tempfile);
    unwind.tempfile_needs_to_be_removed = 1;

    /* Then slurp the image into memory, decoding along the way.
       The result is the image in a simple one-byte-per-pixel
       format (#### the GIF routines only support 8-bit GIFs,
       it appears). */
    unwind.giffile = DGifOpenFileName (unwind.tempfile);
    if (unwind.giffile == NULL)
      {
      gif_decode_error:
	signal_simple_error ("Unable to decode GIF",
			     build_string (EmacsPrintGifError ()));
      }
#if 0
    if (our_own_dgif_slurp_from_gif2x11_c(unwind.giffile) != GIF_OK)
#else
    /* DGifSlurp() doesn't handle interlaced files. */
    /* Actually, it does, sort of.  It just sets the Interlace flag 
       and stores RasterBits in interlaced order.  We handle that below. */
    if (DGifSlurp (unwind.giffile) != GIF_OK)
#endif
      goto gif_decode_error;
  }

  /* 2. Now allocate the colors for the image. */
  {
    int i;
    ColorMapObject *cmap = unwind.giffile->SColorMap;
    /* Just in case the image contains out-of-range pixels, we go
       ahead and allocate space for all of them. */
    unwind.pixels = (unsigned long *) xmalloc (256 * sizeof (unsigned long));
    unwind.npixels = cmap->ColorCount;

    for (i = 0; i < 256; i++)
      unwind.pixels[i] = 0;   /* Use a reasonable color for out of range. */

    /* Allocate pixels for the various colors. */
    for (i = 0; i < cmap->ColorCount; i++)
      {
	XColor color;

	color.red = cmap->Colors[i].Red << 8;
	color.green = cmap->Colors[i].Green << 8;
	color.blue = cmap->Colors[i].Blue << 8;
	color.flags = DoRed | DoGreen | DoBlue;

	allocate_nearest_color (dpy, DefaultColormapOfScreen (scr), &color);
	unwind.pixels[i] = color.pixel;
      }
  }

  /* 3. Now create the image */
  {
    int height = unwind.giffile->SHeight;
    int width = unwind.giffile->SWidth;
    int depth;
    int bitmap_pad;
    int i, j, row, pass, interlace;
    /* interlaced gifs have rows in this order:
       0, 8, 16, ..., 4, 12, 20, ..., 2, 6, 10, ..., 1, 3, 5, ...  */
    static int InterlacedOffset[] = { 0, 4, 2, 1 };
    static int InterlacedJumps[] = { 8, 8, 4, 2 };

    
    depth = DefaultDepthOfScreen (scr);
    
    /* first get bitmap_pad (from XPM) */
    if (depth > 16)
      bitmap_pad = 32;
    else if (depth > 8)
      bitmap_pad = 16;
    else
      bitmap_pad = 8;
    
    unwind.ximage = XCreateImage (dpy, DefaultVisualOfScreen (scr),
				  depth, ZPixmap, 0, 0, width, height,
				  bitmap_pad, 0);

    if (!unwind.ximage)
      signal_simple_error ("Unable to create X image struct", instantiator);

    /* now that bytes_per_line must have been set properly alloc data */
    unwind.ximage->data =
      (char *) xmalloc (unwind.ximage->bytes_per_line * height);

    /* write the data --
       #### XPutPixel() is a client-side-only function but could
       still be slow.  Another possibility is to just convert to
       XPM format and use the Xpm routines, which optimize this
       stuff; but it's doubtful that this will be faster in the
       long run, what with all the XPM overhead.  If this proves
       to be a bottleneck here, maybe we should just copy the
       optimization routines from XPM (they're in turn mostly
       copied from the Xlib source code). */
    
    /* Note: We just use the first image in the file and ignore the rest. 
             We check here that that image covers the full "screen" size.
	     I don't know whether that's always the case.
             -dkindred@cs.cmu.edu  */
    if (unwind.giffile->SavedImages[0].ImageDesc.Height != height
	|| unwind.giffile->SavedImages[0].ImageDesc.Width != width
	|| unwind.giffile->SavedImages[0].ImageDesc.Left != 0
	|| unwind.giffile->SavedImages[0].ImageDesc.Top != 0)
      signal_simple_error ("First image in GIF file is not full size",
			   instantiator);

    interlace = unwind.giffile->SavedImages[0].ImageDesc.Interlace;
    pass = 0;
    row = interlace ? InterlacedOffset[pass] : 0;
    for (i = 0; i < height; i++)
      {
	if (interlace && row >= height)
	  row = InterlacedOffset[++pass];

	for (j = 0; j < width; j++)
	  XPutPixel (unwind.ximage, j, row,
		     unwind.pixels[(unsigned char)
				  /* incorrect signed declaration
				     of RasterBits[] */
				  (unwind.giffile->SavedImages[0].
				   RasterBits[i * width + j])]);

	row += interlace ? InterlacedJumps[pass] : 1;
      }
  }

  /* 4. Now create the pixmap and set up the image instance */
  init_image_instance_from_x_image (ii, unwind.ximage, dest_mask,
				    unwind.pixels, unwind.npixels,
				    instantiator);
  /* Now that we've succeeded, we don't want the pixels
     freed right now.  They're kept around in the image instance
     structure until it's destroyed. */
  unwind.npixels = 0;
  unbind_to (speccount, Qnil);
}

#endif /* HAVE_GIF */


#ifdef HAVE_PNG

/**********************************************************************
 *                             PNG                                    *
 **********************************************************************/
static void
png_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
png_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  return simple_image_type_normalize (inst, console_type, Qpng);
}

static int
png_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

#if !defined (USE_TEMP_FILES_FOR_IMAGES) && (PNG_LIBPNG_VER >= 87)
struct png_memory_storage
{
  Extbyte *bytes;		/* The data       */
  Extcount len;			/* How big is it? */
  int index;			/* Where are we?  */
};

static void png_read_from_memory(png_structp png_ptr, png_bytep data,
				 png_uint_32 length)
{
   struct png_memory_storage *tbr =
     (struct png_memory_storage *) png_get_io_ptr (png_ptr);

   if (length > (tbr->len - tbr->index))
     png_error (png_ptr, (png_const_charp) "Read Error");
   memcpy(data,tbr->bytes + tbr->index,length);
   tbr->index = tbr->index + length;
}
#endif /* !USE_TEMP_FILES_FOR_IMAGESS || PNG_LIBPNG_VER >= 87 */

struct png_unwind_data
{
  Display *dpy;
  FILE *instream;
  png_struct *png_ptr;
  png_info *info_ptr;
  unsigned long *pixels;
  int npixels;
  XImage *ximage;
  char tempfile[50];
  int tempfile_needs_to_be_removed;
};

static Lisp_Object
png_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct png_unwind_data *data =
    (struct png_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->png_ptr)
    png_read_destroy (data->png_ptr, data->info_ptr, (png_info *) NULL);
  if (data->instream)
    fclose (data->instream);
  if (data->tempfile_needs_to_be_removed)
    unlink (data->tempfile);
  if (data->npixels > 0)
    {
      Screen *scr = DefaultScreenOfDisplay (data->dpy);
      Colormap cmap = DefaultColormapOfScreen (scr);
      XFreeColors (data->dpy, cmap, data->pixels, data->npixels, 0L);
      xfree (data->pixels);
    }

  if (data->ximage)
    {
      if (data->ximage->data)
	{
	  xfree (data->ximage->data);
	  data->ximage->data = 0;
	}
      XDestroyImage (data->ximage);
    }

  return Qnil;
}

/* This doesn't appear to be used. */
#if 0
#define get_png_val(p) _get_png_val (&(p), info_ptr.bit_depth)
png_uint_16
_get_png_val (png_byte **pp, int bit_depth)
{
  png_uint_16 c = 0;

  if (bit_depth == 16) {
    c = (*((*pp)++)) << 8;
  }
  c |= (*((*pp)++));

  return c;
}
#endif

static void
png_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  struct Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  Lisp_Object device = IMAGE_INSTANCE_DEVICE (ii);
  Display *dpy;
  Screen *scr;
  struct png_unwind_data unwind;
  int speccount = specpdl_depth ();

  /* PNG variables */
  png_struct *png_ptr;
  png_info *info_ptr;

  if (!DEVICE_X_P (XDEVICE (device)))
    signal_simple_error ("Not an X device", device);

  dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  scr = DefaultScreenOfDisplay (dpy);

  png_ptr = (png_struct *) xmalloc (sizeof (png_struct));
  info_ptr = (png_info *) xmalloc (sizeof (png_info));

  memset (&unwind, 0, sizeof (unwind));
  unwind.png_ptr = png_ptr;
  unwind.info_ptr = info_ptr;
  unwind.dpy = dpy;

  record_unwind_protect (png_instantiate_unwind, make_opaque_ptr (&unwind));

  /* This code is a mixture of stuff from Ben's GIF/JPEG stuff from
     this file, example.c from the libpng 0.81 distribution, and the
     pngtopnm sources. -WMP-
     */
#if defined (USE_TEMP_FILES_FOR_IMAGES) || (PNG_LIBPNG_VER < 87)
  /* Write out to a temp file - we really should take the time to
     write appropriate memory bound IO stuff, but I am just trying
     to get the stupid thing working right now.
     */
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

    assert (!NILP (data));

    write_lisp_string_to_temp_file (data, unwind.tempfile);
    unwind.tempfile_needs_to_be_removed = 1;

    if ((unwind.instream = fopen (unwind.tempfile, "rb")) == NULL)
      report_file_error ("Opening PNG temp file",
			 list1 (build_string (unwind.tempfile)));
  }
#else
  /* Nothing */
#endif

  /* Set the jmp_buf reurn context for png_error ... if this returns !0, then
     we ran into a problem somewhere, and need to clean up after ourselves. */
  if (setjmp (png_ptr->jmpbuf))
    {
      /* Am I doing enough here?  I think so, since most things happen
         in png_unwind */
      png_read_destroy (png_ptr, info_ptr, (png_info *) NULL);
      signal_simple_error ("Error decoding PNG", instantiator);
    }

  /* Initialize all PNG structures */
  png_info_init (info_ptr);
  png_read_init (png_ptr);

  /* Initialize the IO layer and read in header information */
#if defined (USE_TEMP_FILES_FOR_IMAGES) || (PNG_LIBPNG_VER < 87)
  png_init_io (png_ptr, unwind.instream);
#else
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
    Extbyte *bytes;
    Extcount len;
    struct png_memory_storage tbr; /* Data to be read */
    
    assert (!NILP (data));
    
    /* #### This is a definite problem under Mule due to the amount of
       stack data it might allocate.  Need to be able to convert and
       write out to a file. */
    GET_STRING_BINARY_DATA_ALLOCA (data, bytes, len);
    tbr.bytes = bytes;
    tbr.len = len;
    tbr.index = 0;
    png_set_read_fn(png_ptr,(void *) &tbr, png_read_from_memory);
  }
#endif

  png_read_info (png_ptr, info_ptr);

  /* set up the transformations you want.  Note that these are
     all optional.  Only call them if you want them */
  /* tell libpng to strip 16 bit depth files down to 8 bits */
  if (info_ptr->bit_depth == 16)
    png_set_strip_16 (png_ptr);
  if (info_ptr->bit_depth < 8)
    png_set_packing (png_ptr);
  /* ##### Perhaps some way to specify the screen gamma should be in here? */

  {
    int height = info_ptr->height;
    int width = info_ptr->width;
    int depth = info_ptr->bit_depth;
    int linesize = max (info_ptr->bit_depth >> 3, 1) * width;
    int bitmap_pad;
    int y;
    XColor color;
    png_byte *png_pixels;
    png_byte **row_pointers;
    png_color static_color_cube[216];

    /* Wow, allocate all the memory.  Truly, exciting. */
    unwind.pixels = (unsigned long *) xmalloc (256 * sizeof (unsigned long));
    png_pixels = (png_byte *) xmalloc (linesize * height * sizeof (png_byte*));
    row_pointers = (png_byte **) xmalloc (height * sizeof (png_byte *));

    for (y = 0; y < 256; y++)
      unwind.pixels[y] = 0;
    for (y = 0; y < height; y++)
      row_pointers[y] = png_pixels + (linesize * y);

    /*  #### This is where we should handle transparency, but I am unsure of
	how exactly to get that information right now, in a safe manner. */
#if 0
    {
      png_color_16 current_background;

      /* Some appropriate magic should go here to get the current
	 buffers (device?)  background color and convert it to a
	 png_color_16 struct */
      if (info_ptr->valid & PNG_INFO_bKGD)
	png_set_background (png_ptr, &(info_ptr->background), PNG_GAMMA_FILE,
			    1, 1.0);
      else
	png_set_background (png_ptr, &current_background, PNG_GAMMA_SCREEN,
			    0, 1.0);
    }
#endif

    if ((info_ptr->color_type == PNG_COLOR_TYPE_RGB) ||
	(info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA))
      {
	if (!(info_ptr->valid & PNG_INFO_PLTE))
	  {
	    for (y = 0; y < 216; y++)
	      {
		static_color_cube[y].red = (y % 6) * 255.0 / 5;
		static_color_cube[y].green = ((y / 6) % 6) * 255.0 / 5;
		static_color_cube[y].blue = (y / 36) * 255.0 / 5;
	      }
	    png_set_dither (png_ptr, static_color_cube, 216, 216, NULL, 1);
	  }
	else
	  {
	    png_set_dither (png_ptr, info_ptr->palette, info_ptr->num_palette,
			    info_ptr->num_palette, info_ptr->hist, 1);
	  }
      }
    
    png_read_image (png_ptr, row_pointers);
    png_read_end (png_ptr, info_ptr);

    /* Ok, now we go and allocate all the colors */
    if (info_ptr->valid & PNG_INFO_PLTE)
      {
	unwind.npixels = info_ptr->num_palette;
	for (y = 0; y < unwind.npixels; y++)
	  {
	    color.red = info_ptr->palette[y].red << 8;
	    color.green = info_ptr->palette[y].green << 8;
	    color.blue = info_ptr->palette[y].blue << 8;
	    color.flags = DoRed | DoGreen | DoBlue;
	    allocate_nearest_color (dpy, DefaultColormapOfScreen (scr),
				    &color);
	    unwind.pixels[y] = color.pixel;
	  }
      }
    else
      {
	unwind.npixels = 216;
	for (y = 0; y < 216; y++)
	  {
	    color.red = static_color_cube[y].red << 8;
	    color.green = static_color_cube[y].green << 8;
	    color.blue = static_color_cube[y].blue << 8;
	    color.flags = DoRed|DoGreen|DoBlue;
	    allocate_nearest_color (dpy, DefaultColormapOfScreen (scr),
				    &color);
	    unwind.pixels[y] = color.pixel;
	  }
      }

#ifdef PNG_SHOW_COMMENTS
    /* ####
     * I turn this off by default now, because the !%^@#!% comments
     * show up every time the image is instantiated, which can get
     * really really annoying.  There should be some way to pass this
     * type of data down into the glyph code, where you can get to it
     * from lisp anyway. - WMP
     */
    {
      int i;

      for (i = 0 ; i < info_ptr->num_text ; i++)
	{
	  /* How paranoid do I have to be about no trailing NULLs, and
	     using (int)info_ptr->text[i].text_length, and strncpy and a temp
	     string somewhere? */

	  warn_when_safe (Qpng, Qinfo, "%s - %s",
			  info_ptr->text[i].key,
			  info_ptr->text[i].text);
	}
    }
#endif

    /* Now create the image */
    
    depth = DefaultDepthOfScreen (scr);
    
    /* first get bitmap_pad (from XPM) */
    if (depth > 16)
      bitmap_pad = 32;
    else if (depth > 8)
      bitmap_pad = 16;
    else
      bitmap_pad = 8;
    
    unwind.ximage = XCreateImage (dpy, DefaultVisualOfScreen (scr),
				  depth, ZPixmap, 0, 0, width, height,
				  bitmap_pad, 0);

    if (!unwind.ximage)
      signal_simple_error ("Unable to create X image struct",
			   instantiator);

    /* now that bytes_per_line must have been set properly alloc data */
    unwind.ximage->data = (char *) xmalloc (unwind.ximage->bytes_per_line *
					    height);

    {
      int i, j;
      for (i = 0; i < height; i++)
	for (j = 0; j < width; j++)
	  XPutPixel (unwind.ximage, j, i,
		     unwind.pixels[png_pixels[i * width + j]]);
    }
    
    xfree (row_pointers);
    xfree (png_pixels);
  }

  init_image_instance_from_x_image (ii, unwind.ximage, dest_mask,
				    unwind.pixels, unwind.npixels,
				    instantiator);

  /* This will clean up everything else. */
  unwind.npixels = 0;
  unbind_to (speccount, Qnil);
}

#endif /* HAVE_PNG */


#ifdef HAVE_TIFF

/**********************************************************************
 *                             TIFF                                   *
 **********************************************************************/
static void
tiff_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
tiff_normalize (Lisp_Object inst, Lisp_Object console_type)
{
  signal_simple_error ("No TIFF support yet", inst);
  return Qnil;
}

static int
tiff_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

static void
tiff_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object domain)
{
  abort ();
}

#endif /* HAVE_TIFF */


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
      Ferase_buffer (Fcurrent_buffer ());

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

      retval = Fbuffer_substring (Qnil, Qnil, Fcurrent_buffer ());
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
  Screen *xs = LISP_DEVICE_TO_X_SCREEN (device);
  Display *dpy = DisplayOfScreen (xs);
  Colormap cmap = DefaultColormapOfScreen (xs);
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

  symbols = (XpmColorSymbol *) xmalloc (i * sizeof (XpmColorSymbol));
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
    }
  
  color_symbols = extract_xpm_color_names (&xpmattrs, device, domain,
					   color_symbol_alist);

  result = XpmCreatePixmapFromBuffer (dpy,
				      RootWindowOfScreen (xs),
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
    Pixel *pixels = 0;

    if (npixels != 0)
      {
	pixels = xmalloc (npixels * sizeof (Pixel));
	memcpy (pixels, xpmattrs.pixels, npixels * sizeof (Pixel));
      }
    else
      pixels = 0;

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
	/* XpmReadFileToPixmap() doesn't return the depth (bogus!) so
	   we need to get it ourself.  (No, xpmattrs.depth is not it;
	   that's an input slot, not output.)  We could just assume
	   that it has the same depth as the root window, but some
	   devices allow more than one depth, so that isn't
	   necessarily correct (I guess?) */
	Window root;
	int x, y;
	unsigned int w2, h2, bw;

	unsigned int d;

	if (!XGetGeometry (dpy, pixmap, &root, &x, &y, &w2, &h2, &bw, &d))
	  abort ();
	if (w != w2 || h != h2)
	  abort ();

	IMAGE_INSTANCE_PIXMAP_DEPTH (ii) = d;
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

	    XQueryColor (dpy, DefaultColormapOfScreen (xs), &fg);
	    XQueryColor (dpy, DefaultColormapOfScreen (xs), &bg);

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
#include <compface.h>
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
 *                           Autodetect                               *
 **********************************************************************/

static void
autodetect_validate (Lisp_Object instantiator)
{
  data_must_be_present (instantiator);
}

static Lisp_Object
autodetect_normalize (Lisp_Object instantiator, Lisp_Object console_type)
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
autodetect_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
			Lisp_Object pointer_fg, Lisp_Object pointer_bg,
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
    Screen *scr = DefaultScreenOfDisplay (dpy);
    Dimension d = DefaultDepthOfScreen (scr);
    Pixmap new = XCreatePixmap (dpy, RootWindowOfScreen (scr),
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

  error ("subwindows are not functional in 19.14; they will be in 19.15");

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
    struct Lisp_Subwindow *sw = alloc_lcrecord (sizeof (struct Lisp_Subwindow),
						lrecord_subwindow);
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

#ifdef HAVE_JPEG
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (jpeg, "jpeg");

  IIFORMAT_HAS_METHOD (jpeg, validate);
  IIFORMAT_HAS_METHOD (jpeg, normalize);
  IIFORMAT_HAS_METHOD (jpeg, possible_dest_types);
  IIFORMAT_HAS_METHOD (jpeg, instantiate);

  IIFORMAT_VALID_KEYWORD (jpeg, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (jpeg, Q_file, check_valid_string);
#endif

#ifdef HAVE_GIF
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (gif, "gif");

  IIFORMAT_HAS_METHOD (gif, validate);
  IIFORMAT_HAS_METHOD (gif, normalize);
  IIFORMAT_HAS_METHOD (gif, possible_dest_types);
  IIFORMAT_HAS_METHOD (gif, instantiate);

  IIFORMAT_VALID_KEYWORD (gif, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (gif, Q_file, check_valid_string);
#endif

#ifdef HAVE_PNG
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (png, "png");

  IIFORMAT_HAS_METHOD (png, validate);
  IIFORMAT_HAS_METHOD (png, normalize);
  IIFORMAT_HAS_METHOD (png, possible_dest_types);
  IIFORMAT_HAS_METHOD (png, instantiate);

  IIFORMAT_VALID_KEYWORD (png, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (png, Q_file, check_valid_string);
#endif
  
#ifdef HAVE_TIFF
  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (tiff, "tiff");

  IIFORMAT_HAS_METHOD (tiff, validate);
  IIFORMAT_HAS_METHOD (tiff, normalize);
  IIFORMAT_HAS_METHOD (tiff, possible_dest_types);
  IIFORMAT_HAS_METHOD (tiff, instantiate);

  IIFORMAT_VALID_KEYWORD (tiff, Q_data, check_valid_string);
  IIFORMAT_VALID_KEYWORD (tiff, Q_file, check_valid_string);
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

  INITIALIZE_IMAGE_INSTANTIATOR_FORMAT (autodetect, "autodetect");

  IIFORMAT_HAS_METHOD (autodetect, validate);
  IIFORMAT_HAS_METHOD (autodetect, normalize);
  IIFORMAT_HAS_METHOD (autodetect, possible_dest_types);
  IIFORMAT_HAS_METHOD (autodetect, instantiate);

  IIFORMAT_VALID_KEYWORD (autodetect, Q_data, check_valid_string);
}

void
vars_of_glyphs_x (void)
{
#ifdef HAVE_JPEG
  Fprovide (Qjpeg);
#endif

#ifdef HAVE_GIF
  Fprovide (Qgif);
#endif

#ifdef HAVE_PNG
  Fprovide (Qpng);
#endif
  
#ifdef HAVE_TIFF
  Fprovide (Qtiff);
#endif

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

#ifdef HAVE_XFACE
  Fprovide (Qxface);
#endif 

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path /*
A list of the directories in which X bitmap files may be found.
If nil, this is initialized from the \"*bitmapFilePath\" resource.
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
