/* EImage-specific Lisp objects.
   Copyright (C) 1993, 1994, 1998 Free Software Foundation, Inc.
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

/* Originally part of glyphs.c.

   GIF/JPEG support added by Ben Wing for 19.14
   PNG support added by Bill Perry for 19.14
   Improved GIF/JPEG support added by Bill Perry for 19.14
   Cleanup/simplification of error handling by Ben Wing for 19.14
   GIF support changed to external Gifreader lib by Jareth Hein for 21.0
   Many changes for color work and optimizations by Jareth Hein for 21.0
   Switch of GIF/JPEG/PNG to new EImage intermediate code by Jareth Hein for 21.0
   TIFF code by Jareth Hein for 21.0
   GIF support changed to external giflib by Jerry James for 21.5
   TODO:
   Convert images.el to C and stick it in here?
   This file is really repetitious; can we refactor?
 */

#include <config.h>
#include "lisp.h"
#include "lstream.h"
#include "console.h"
#include "device-impl.h"
#include "faces.h"
#include "glyphs.h"
#include "fontcolor-impl.h"

#include "buffer.h"
#include "frame.h"
#include "opaque.h"
#include "window.h"

#include "sysfile.h"

#ifdef HAVE_PNG

BEGIN_C_DECLS

#define message message_ /* Yuck */
#if !defined(_LARGEFILE64_SOURCE) /* Yuck**2 -- see <zconf.h>. */
#define _LARGEFILE64_SOURCE 0
#endif
#if !defined(_FILE_OFFSET_BITS) /* Yuck**2 -- see <zlib.h>. */
#define _FILE_OFFSET_BITS 0
#endif
#include <png.h>
#undef message

END_C_DECLS

#else
#include <setjmp.h>
#endif
#include "file-coding.h"

#ifdef HAVE_TIFF
DEFINE_IMAGE_INSTANTIATOR_FORMAT (tiff);
Lisp_Object Qtiff;
#endif

#ifdef HAVE_JPEG
DEFINE_IMAGE_INSTANTIATOR_FORMAT (jpeg);
Lisp_Object Qjpeg;
#endif

#ifdef HAVE_GIF
DEFINE_IMAGE_INSTANTIATOR_FORMAT (gif);
Lisp_Object Qgif;
#endif

#ifdef HAVE_PNG
DEFINE_IMAGE_INSTANTIATOR_FORMAT (png);
Lisp_Object Qpng;
#endif


#ifdef HAVE_JPEG

/**********************************************************************
 *                             JPEG                                   *
 **********************************************************************/

BEGIN_C_DECLS

#ifdef WIN32_ANY
/* #### Yuck!  More horrifitude.  tiffio.h, below, and sysfile.h above,
   include <windows.h>, which defines INT32 and INT16, the former
   differently and incompatibly from jmorecfg.h, included by jpeglib.h.  We
   can disable the stuff in jmorecfg.h by defining XMD_H (clever, huh?);
   then we define these typedefs the way that <windows.h> wants them (which
   is more correct, anyway; jmorecfg.h defines INT32 as `long'). */
#define XMD_H
typedef signed int INT32;
typedef signed short INT16;

/* And another one... jmorecfg.h defines the 'boolean' type as int,
   which conflicts with the standard Windows 'boolean' definition as
   unsigned char. Ref: http://www.asmail.be/msg0054688232.html */
#ifndef __RPCNDR_H__		/* don't conflict if rpcndr.h already read */
typedef unsigned char boolean;
#endif
#define HAVE_BOOLEAN		/* prevent jmorecfg.h from redefining it */
#endif

/* Yet more breakage... jmorecfg.h unconditionally defines FAR either as
   "far" or as blank.  Windef.h unconditionally defines FAR as "far".
   We'll avoid the compile warning by redefing FAR the way windows defines it,
   after loading the JPEG headers. */
#undef FAR
#include <jpeglib.h>
#include <jerror.h>
#undef FAR
#define FAR far

END_C_DECLS

/*#define USE_TEMP_FILES_FOR_JPEG_IMAGES 1*/
static void
jpeg_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
jpeg_normalize (Lisp_Object inst, Lisp_Object console_type,
		Lisp_Object UNUSED (dest_mask))
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
  /* Stream that we need to close */
  FILE *instream;
  /* Object that holds state info for JPEG decoding */
  struct jpeg_decompress_struct *cinfo_ptr;
  /* EImage data */
  Binbyte *eimage;
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
    {
      retry_fclose (data->instream);
      data->instream = 0;
    }

  if (data->eimage)
    {
      xfree (data->eimage);
      data->eimage = 0;
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
our_init_source (j_decompress_ptr UNUSED (cinfo))
{
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(boolean)
#else
METHODDEF boolean
#endif
our_fill_input_buffer (j_decompress_ptr cinfo)
{
  /* Insert a fake EOI marker */
  struct jpeg_source_mgr *src = cinfo->src;
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
our_skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
  struct jpeg_source_mgr *src = NULL;

  src = (struct jpeg_source_mgr *) cinfo->src;

  if (!src)
    return;
  else if (num_bytes > (long) src->bytes_in_buffer)
    {
      ERREXIT (cinfo, JERR_INPUT_EOF);
      /*NOTREACHED*/
    }

  src->bytes_in_buffer -= num_bytes;
  src->next_input_byte += num_bytes;
}

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
our_term_source (j_decompress_ptr UNUSED (cinfo))
{
}

typedef struct
{
  struct jpeg_source_mgr pub;
} our_jpeg_source_mgr;

static void
jpeg_memory_src (j_decompress_ptr cinfo, JOCTET *data, Bytecount len)
{
  struct jpeg_source_mgr *src;

  if (cinfo->src == NULL)
    {	/* first time for this JPEG object? */
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

#if defined(JPEG_LIB_VERSION) && (JPEG_LIB_VERSION >= 61)
METHODDEF(void)
#else
METHODDEF void
#endif
my_jpeg_output_message (j_common_ptr cinfo)
{
  Extbyte buffer[JMSG_LENGTH_MAX];
  Ibyte *intbuf;

  /* Create the message */
  (*cinfo->err->format_message) (cinfo, buffer);
  intbuf = EXTERNAL_TO_ITEXT (buffer, Qjpeg_error_message_encoding);
  warn_when_safe (Qjpeg, Qinfo, "%s", intbuf);
}

/* The code in this routine is based on example.c from the JPEG library
   source code and from gif_instantiate() */
static void
jpeg_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
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

  /* Step -1: First record our unwind-protect, which will clean up after
     any exit, normal or not */

  xzero (unwind);
  record_unwind_protect (jpeg_instantiate_unwind, make_opaque_ptr (&unwind));

  /* Step 1: allocate and initialize JPEG decompression object */

  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error (&jerr.pub);
  jerr.pub.error_exit = my_jpeg_error_exit;
  jerr.pub.output_message = my_jpeg_output_message;

  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp (jerr.setjmp_buffer))
    {
      /* If we get here, the JPEG code has signaled an error.
       * We need to clean up the JPEG object, close the input file, and return.
       */

      {
	Lisp_Object errstring;
	Extbyte buffer[JMSG_LENGTH_MAX];

	/* Create the message */
	(*cinfo.err->format_message) ((j_common_ptr) &cinfo, buffer);
	errstring = build_extstring (buffer, Qjpeg_error_message_encoding);

	signal_image_error_2 ("JPEG decoding error",
			      errstring, instantiator);
      }
    }

  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress (&cinfo);
  unwind.cinfo_ptr = &cinfo;

  /* Step 2: specify data source (eg, a file) */

  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
    const Binbyte *bytes;
    Bytecount len;

    /* #### This is a definite problem under Mule due to the amount of
       stack data it might allocate.  Need to be able to convert and
       write out to a file. */
    LISP_STRING_TO_SIZED_EXTERNAL (data, bytes, len, Qbinary);
    jpeg_memory_src (&cinfo, (JOCTET *) bytes, len);
  }

  /* Step 3: read file parameters with jpeg_read_header() */

  jpeg_read_header (&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */

  {
    UINT_64_BIT pixels_sq;
    int jpeg_gray = 0;		/* if we're dealing with a grayscale */
    /* Step 4: set parameters for decompression.   */

    /* Now that we're using EImages, send all data as 24bit color.
       The backend routine will take care of any necessary reductions.
       We do have to handle the grayscale case ourselves, however. */
    if (cinfo.jpeg_color_space == JCS_GRAYSCALE)
      {
	cinfo.out_color_space = JCS_GRAYSCALE;
	jpeg_gray = 1;
      }
    else
      {
	/* we're relying on the jpeg driver to do any other conversions,
	   or signal an error if the conversion isn't supported. */
	cinfo.out_color_space = JCS_RGB;
      }

    /* Step 5: Start decompressor */
    jpeg_start_decompress (&cinfo);

    /* Step 6: Read in the data and put into EImage format (8bit RGB triples)*/
    pixels_sq =
      (UINT_64_BIT) cinfo.output_width * (UINT_64_BIT) cinfo.output_height;
    if (pixels_sq > ((size_t) -1) / 3)
      signal_image_error ("JPEG image too large to instantiate", instantiator);
    unwind.eimage =
      xnew_binbytes (cinfo.output_width * cinfo.output_height * 3);
    if (!unwind.eimage)
      signal_image_error("Unable to allocate enough memory for image", instantiator);

    {
      JSAMPARRAY row_buffer;	/* Output row buffer */
      JSAMPLE *jp;
      int row_stride;		/* physical row width in output buffer */
      Binbyte *op = unwind.eimage;

      /* We may need to do some setup of our own at this point before reading
       * the data.  After jpeg_start_decompress() we have the correct scaled
       * output image dimensions available
       * We need to make an output work buffer of the right size.
       */
      /* JSAMPLEs per row in output buffer. */
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

	  /* jpeg_read_scanlines expects an array of pointers to scanlines.
	   * Here the array is only one element long, but you could ask for
	   * more than one scanline at a time if that's more convenient.
	   */
	  (void) jpeg_read_scanlines (&cinfo, row_buffer, 1);
	  jp = row_buffer[0];
	  for (i = 0; i < (int) cinfo.output_width; i++)
	    {
	      int clr;
	      if (jpeg_gray)
		{
		  Binbyte val;
#if (BITS_IN_JSAMPLE == 8)
		  val = (Binbyte) *jp++;
#else /* other option is 12 */
		  val = (Binbyte) (*jp++ >> 4);
#endif
		  for (clr = 0; clr < 3; clr++) /* copy the same value into RGB */
		      *op++ = val;
		}
	      else
		{
		  for (clr = 0; clr < 3; clr++)
#if (BITS_IN_JSAMPLE == 8)
		    *op++ = (Binbyte)*jp++;
#else /* other option is 12 */
		    *op++ = (Binbyte)(*jp++ >> 4);
#endif
		}
	    }
	}
    }
  }

  /* Step 6.5: Create the pixmap and set up the image instance */
  /* now instantiate */
  MAYBE_DEVMETH (DOMAIN_XDEVICE (ii->domain),
		 init_image_instance_from_eimage,
		 (ii, cinfo.output_width, cinfo.output_height, 1,
		  unwind.eimage, dest_mask,
		  instantiator, pointer_fg, pointer_bg, domain));

  /* Step 7: Finish decompression */

  jpeg_finish_decompress (&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* And we're done! */
  /* This will clean up everything else. */
  unbind_to (speccount);
}

#endif /* HAVE_JPEG */

#ifdef HAVE_GIF
/**********************************************************************
 *                               GIF                                  *
 **********************************************************************/

#include <gif_lib.h>

static void
gif_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
gif_normalize (Lisp_Object inst, Lisp_Object console_type,
	       Lisp_Object UNUSED (dest_mask))
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
  Binbyte *eimage;
  /* Object that holds the decoded data from a GIF file */
  GifFileType *giffile;
};

static Lisp_Object
gif_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct gif_unwind_data *data =
    (struct gif_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->giffile)
    {
      DGifCloseFile (data->giffile);
      FreeSavedImages (data->giffile);
      data->giffile = 0;
    }
  if (data->eimage)
    {
      xfree (data->eimage);
      data->eimage = 0;
    }

  return Qnil;
}

typedef struct gif_memory_storage
{
  Binbyte *bytes;		/* The data       */
  Bytecount len;		/* How big is it? */
  Bytecount index;		/* Where are we?  */
} gif_memory_storage;

static int
gif_read_from_memory (GifFileType *gif, GifByteType *buf, int size)
{
  gif_memory_storage *mem = (gif_memory_storage *) gif->UserData;

  if (size > (mem->len - mem->index))
    return -1;
  memcpy (buf, mem->bytes + mem->index, size);
  mem->index = mem->index + size;
  return size;
}

static const char *
gif_decode_error_string (void)
{
  switch (GifLastError ())
    {
    case D_GIF_ERR_OPEN_FAILED:
      return "GIF error: unable to open";
    case D_GIF_ERR_READ_FAILED:
      return "GIF error: read failed";
    case D_GIF_ERR_NOT_GIF_FILE:
      return "GIF error: not a GIF file";
    case D_GIF_ERR_NO_SCRN_DSCR:
      return "GIF error: no Screen Descriptor detected";
    case D_GIF_ERR_NO_IMAG_DSCR:
      return "GIF error: no Image Descriptor detected";
    case D_GIF_ERR_NO_COLOR_MAP:
      return "GIF error: no global or local color map";
    case D_GIF_ERR_WRONG_RECORD:
      return "GIF error: wrong record type";
    case D_GIF_ERR_DATA_TOO_BIG:
      return "GIF error: image is larger than indicated by header";
    case D_GIF_ERR_NOT_ENOUGH_MEM:
      return "GIF error: out of memory";
    case D_GIF_ERR_CLOSE_FAILED:
      return "GIF error: failed to close file";
    case D_GIF_ERR_NOT_READABLE:
      return "GIF error: file is not readable";
    case D_GIF_ERR_IMAGE_DEFECT:
      return "GIF error: image is defective";
    case D_GIF_ERR_EOF_TOO_SOON:
      return "GIF error: image EOF detected before image complete";
    default:
      return "GIF error: unknown error";
    }
}

static void
gif_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  /* It is OK for the unwind data to be local to this function,
     because the unwind-protect is always executed when this
     stack frame is still valid. */
  struct gif_unwind_data unwind;
  int speccount = specpdl_depth ();
  gif_memory_storage mem_struct;
  Binbyte *bytes;
  Bytecount len;
  int height = 0;
  int width = 0;

  xzero (unwind);
  record_unwind_protect (gif_instantiate_unwind, make_opaque_ptr (&unwind));

  /* 1. Now decode the data. */

  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);

    assert (!NILP (data));

    LISP_STRING_TO_SIZED_EXTERNAL (data, bytes, len, Qbinary); 
    mem_struct.bytes = bytes;
    mem_struct.len = len;
    mem_struct.index = 0;
    unwind.giffile = DGifOpen (&mem_struct, gif_read_from_memory);
    if (unwind.giffile == NULL)
      signal_image_error (gif_decode_error_string (), instantiator);

    /* Then slurp the image into memory, decoding along the way.
       The result is the image in a simple one-byte-per-pixel
       format. */
    if (DGifSlurp (unwind.giffile) == GIF_ERROR)
      signal_image_error (gif_decode_error_string (), instantiator);

    /* dgif_lib.c doesn't do any validation on these. */
    check_integer_range (make_fixnum (1),
                         make_fixnum (unwind.giffile->SHeight),
                         make_fixnum (MOST_POSITIVE_FIXNUM));
    check_integer_range (make_fixnum (1),
                         make_fixnum (unwind.giffile->SWidth),
                         make_fixnum (MOST_POSITIVE_FIXNUM));
  }

  /* 3. Now create the EImage(s) */
  {
    ColorMapObject *cmo
      = (unwind.giffile->Image.ColorMap ? unwind.giffile->Image.ColorMap
         : unwind.giffile->SColorMap);
    int i, j, row, interlace, slice;
    UINT_64_BIT pixels_sq;
    Binbyte *eip;
    /* interlaced gifs have rows in this order:
       0, 8, 16, ..., 4, 12, 20, ..., 2, 6, 10, ..., 1, 3, 5, ...  */
    static const int InterlacedOffset[] = { 0, 4, 2, 1 };
    static const int InterlacedJumps[] = { 8, 8, 4, 2 };

    if (cmo == NULL)
      signal_image_error ("GIF image has no color map", instantiator);

    height = unwind.giffile->SHeight;
    width = unwind.giffile->SWidth;
    pixels_sq = (UINT_64_BIT) width * (UINT_64_BIT) height;
    if (pixels_sq > ((size_t) -1) / (3 * unwind.giffile->ImageCount))
      signal_image_error ("GIF image too large to instantiate", instantiator);
    unwind.eimage =
      xnew_binbytes (width * height * 3 * unwind.giffile->ImageCount);
    if (!unwind.eimage)
      signal_image_error("Unable to allocate enough memory for image",
                         instantiator);

    /* write the data in EImage format (8bit RGB triples) */

    for (slice = 0; slice < unwind.giffile->ImageCount; slice++)
      {
	unsigned int pass;
	/* We check here that the current image covers the full "screen"
           size. */
	if (unwind.giffile->SavedImages[slice].ImageDesc.Height != height
	    || unwind.giffile->SavedImages[slice].ImageDesc.Width != width
	    || unwind.giffile->SavedImages[slice].ImageDesc.Left != 0
	    || unwind.giffile->SavedImages[slice].ImageDesc.Top != 0)
	  signal_image_error ("Image in GIF file is not full size",
			      instantiator);

	interlace = unwind.giffile->SavedImages[slice].ImageDesc.Interlace;

        pass = 0;
	row = 0; /* interlace ? InterlacedOffset[pass] : 0; */
	eip = unwind.eimage + (width * height * 3 * slice);
	for (i = 0; i < height; i++)
	  {
	    if (interlace)
	      if (row >= height) {
		row = InterlacedOffset[(++pass) & 3];
                glyph_checking_assert (pass < countof (InterlacedOffset));
		while (row >= height)
                  {
                    /* We only run into trouble here if height is less than 1
                       (the last value in InterlacedOffset), and we've checked
                       for that above.

                       The & 3 is not necessary--see the assertions--but it
                       quiets compiler warnings about reading beyond the end
                       of InterlacedOffset. */
                    row = InterlacedOffset[(++pass) & 3];
                    glyph_checking_assert (pass < countof (InterlacedOffset));
                  }
	      }
	    eip = unwind.eimage + (width * height * 3 * slice)
              + (row * width * 3);
	    for (j = 0; j < width; j++)
	      {
		Binbyte pixel =
		  unwind.giffile->SavedImages[slice].RasterBits[(i * width)
                                                                + j];
		*eip++ = cmo->Colors[pixel].Red;
		*eip++ = cmo->Colors[pixel].Green;
		*eip++ = cmo->Colors[pixel].Blue;
	      }
	    row += interlace ? InterlacedJumps[pass & 3] : 1;
	  }
      }

    /* now instantiate */
    MAYBE_DEVMETH (DOMAIN_XDEVICE (ii->domain),
		   init_image_instance_from_eimage,
		   (ii, width, height, unwind.giffile->ImageCount,
		    unwind.eimage, dest_mask, instantiator, pointer_fg,
		    pointer_bg, domain));
  }

  /* We read the gif successfully. If we have more than one slice then
     animate the gif. */
  if (unwind.giffile->ImageCount > 1)
    {
    /* See if there is a timeout value. In theory there could be one
       for every image - but that makes the implementation way too
       complicated for now so we just take the first. */
      unsigned short timeout = 0;
      Lisp_Object tid;

      if (unwind.giffile->SavedImages[0].Function == GRAPHICS_EXT_FUNC_CODE
	  &&
	  unwind.giffile->SavedImages[0].ExtensionBlockCount)
	{
	  timeout = (unsigned short)
	    ((unwind.giffile->SavedImages[0].ExtensionBlocks[0].Bytes[2] << 8) +
	     unwind.giffile-> SavedImages[0].ExtensionBlocks[0].Bytes[1]) * 10;
	}

      /* Too short a timeout will crucify us performance-wise. */
      tid = add_glyph_animated_timeout (timeout > 10 ? timeout : 10, image_instance);

      if (!NILP (tid))
	IMAGE_INSTANCE_PIXMAP_TIMEOUT (ii) = XFIXNUM (tid);
    }

  unbind_to (speccount);
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
png_normalize (Lisp_Object inst, Lisp_Object console_type,
	       Lisp_Object UNUSED (dest_mask))
{
  return simple_image_type_normalize (inst, console_type, Qpng);
}

static int
png_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

struct png_memory_storage
{
  const Binbyte *bytes;	/* The data       */
  Bytecount len;		/* How big is it? */
  Bytecount index;		/* Where are we?  */
};

static void
png_read_from_memory (png_structp png_ptr, png_bytep data,
		      png_size_t length)
{
   struct png_memory_storage *tbr =
     (struct png_memory_storage *) png_get_io_ptr (png_ptr);

   if ((Bytecount) length > (tbr->len - tbr->index))
     png_error (png_ptr, (png_const_charp) "Read Error");
   memcpy (data, tbr->bytes + tbr->index,length);
   tbr->index = tbr->index + length;
}

struct png_error_struct
{
  const char *err_str;
  jmp_buf setjmp_buffer;	/* for return to caller */
};

/* jh 98/03/12 - #### AARRRGH! libpng includes jmp_buf inside its own
   structure, and there are cases where the size can be different from
   between inside the library, and inside the code!  To do an end run
   around this, use our own error functions, and don't rely on things
   passed in the png_ptr to them.  This is an ugly hack and must
   go away when the lisp engine is threaded! */
static struct png_error_struct png_err_stct;

static void
png_error_func (png_structp UNUSED (png_ptr), png_const_charp msg)
{
  png_err_stct.err_str = msg;
  longjmp (png_err_stct.setjmp_buffer, 1);
}

static void
png_warning_func (png_structp UNUSED (png_ptr), png_const_charp msg)
{
  DECLARE_EISTRING (eimsg);

  eicpy_ext(eimsg, msg, Qbinary);  
  warn_when_safe (Qpng, Qinfo, "%s", eidata(eimsg));
}

struct png_unwind_data
{
  FILE *instream;
  Binbyte *eimage;
  png_structp png_ptr;
  png_infop info_ptr;
};

static Lisp_Object
png_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct png_unwind_data *data =
    (struct png_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->png_ptr)
    {
      /* ensure we can't get here again */
      png_structp tmp = data->png_ptr;
      data->png_ptr = NULL;
      png_destroy_read_struct (&tmp, &(data->info_ptr), (png_infopp)NULL);
    }

  if (data->instream)
    {
      retry_fclose (data->instream);
      data->instream = 0;
    }

  if (data->eimage)
    {
      xfree (data->eimage);
      data->eimage = 0;
    }

  return Qnil;
}

static void
png_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		 Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		 int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  struct png_unwind_data unwind;
  int speccount = specpdl_depth ();
  int height, width;
  struct png_memory_storage tbr;  /* Data to be read */

  /* PNG variables */
  png_structp png_ptr;
  png_infop info_ptr;

  xzero (unwind);
  record_unwind_protect (png_instantiate_unwind, make_opaque_ptr (&unwind));

  if (setjmp (png_err_stct.setjmp_buffer))
    {
      /* Something blew up:
	 just display the error (cleanup happens in the unwind) */
      signal_image_error_2 ("Error decoding PNG",
			     build_extstring (png_err_stct.err_str,
					       Qerror_message_encoding),
			     instantiator);
    }

  /* Initialize all PNG structures */
  png_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING,
				    (void *) &png_err_stct,
				    png_error_func, png_warning_func);
  if (!png_ptr)
    signal_image_error ("Error obtaining memory for png_read", instantiator);
  unwind.png_ptr = png_ptr;

  info_ptr = png_create_info_struct (png_ptr);
  if (!info_ptr)
    {
      unwind.png_ptr = NULL;	/* avoid re-calling png_destroy_read_struct
				   when unwinding */
      png_destroy_read_struct (&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
      signal_image_error ("Error obtaining memory for png_read", instantiator);
    }
  unwind.info_ptr = info_ptr;

  /* This code is a mixture of stuff from Ben's GIF/JPEG stuff from
     this file, example.c from the libpng 0.81 distribution, and the
     pngtopnm sources. -WMP-
     */
  /* It has been further modified to handle the API changes for 0.96,
     and is no longer usable for previous versions. jh
  */

  /* Initialize the IO layer and read in header information */
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
    const Binbyte *bytes;
    Bytecount len;

    assert (!NILP (data));

    /* #### This is a definite problem under Mule due to the amount of
       stack data it might allocate.  Need to think about using Lstreams */
    LISP_STRING_TO_SIZED_EXTERNAL (data, bytes, len, Qbinary); 
    tbr.bytes = bytes;
    tbr.len = len;
    tbr.index = 0;
    png_set_read_fn (png_ptr,(void *) &tbr, png_read_from_memory);
  }

  png_read_info (png_ptr, info_ptr);

  {
    int y, padding;
    Binbyte **row_pointers;
    UINT_64_BIT pixels_sq;
    height = png_get_image_height (png_ptr, info_ptr);
    width = png_get_image_width (png_ptr, info_ptr);
    pixels_sq = (UINT_64_BIT) width * (UINT_64_BIT) height;
    if (pixels_sq > ((size_t) -1) / 3)
      signal_image_error ("PNG image too large to instantiate", instantiator);

    /* Wow, allocate all the memory.  Truly, exciting.
       Well, yes, there's excitement to be had.  It turns out that libpng
       strips in place, so the last row overruns the buffer if depth is 16
       or there's an alpha channel.  This is a crash on Linux.  So we need
       to add padding.
       The worst case is reducing 8 bytes (16-bit RGBA) to 3 (8-bit RGB). */

    padding = 5 * width;
    unwind.eimage = xnew_array_and_zero (Binbyte,
					 (size_t) (pixels_sq * 3 + padding));

    /* libpng expects that the image buffer passed in contains a
       picture to draw on top of if the png has any transparencies.
       This could be a good place to pass that in... */

    row_pointers  = xnew_array (png_byte *, height);
    for (y = 0; y < height; y++)
      row_pointers[y] = unwind.eimage + (width * 3 * y);

    {
      /* if the png specifies a background chunk, go ahead and
	 use it, else use what we can get from the default face. */
      png_color_16 my_background, *image_background;
      Lisp_Object bkgd = Qnil;

      my_background.red   = 0x7fff;
      my_background.green = 0x7fff;
      my_background.blue  = 0x7fff;
      bkgd = FACE_BACKGROUND (Vdefault_face, domain);
      if (!COLOR_INSTANCEP (bkgd))
	{
	  warn_when_safe (Qpng, Qinfo, "Couldn't get background color!");
	}
      else
	{
	  Lisp_Color_Instance *c = XCOLOR_INSTANCE (bkgd);
	  Lisp_Object rgb = MAYBE_LISP_DEVMETH (XDEVICE (c->device),
						color_instance_rgb_components,
						(c));
#ifdef HAVE_GTK3
#define GETCOLOR(col) my_background.col = (unsigned short) (XFLOATFIXNUM (XCAR (rgb)) * 65535.0)
	  GETCOLOR(red); rgb = XCDR (rgb);
	  GETCOLOR(green); rgb = XCDR (rgb);
	  GETCOLOR(blue);
	  /* What about alpha? - jsparkes */
#else
#define GETCOLOR(col) my_background.col = (unsigned short) XFIXNUM (XCAR (rgb))
	  GETCOLOR(red); rgb = XCDR (rgb);
	  GETCOLOR(green); rgb = XCDR (rgb);
	  GETCOLOR(blue);
#endif
#undef GETCOLOR
	}

      if (png_get_bKGD (png_ptr, info_ptr, &image_background))
	png_set_background (png_ptr, image_background,
			    PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
      else
	png_set_background (png_ptr, &my_background,
			    PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
    }

    /* Now that we're using EImage, ask for 8bit RGB triples for any type
       of image*/
    switch (png_get_color_type (png_ptr, info_ptr))
      {
      case PNG_COLOR_TYPE_PALETTE:
        /* convert palette images to RGB */
        png_set_palette_to_rgb (png_ptr);
        break;

      case PNG_COLOR_TYPE_GRAY:
      case PNG_COLOR_TYPE_GRAY_ALPHA:
        /* convert grayscale images to RGB */
        png_set_gray_to_rgb (png_ptr);
        break;

      default:
        /* pad images with depth < 8 bits */
        if (png_get_bit_depth (png_ptr, info_ptr) < 8)
          {
            png_set_packing (png_ptr);
          }
        break;
      }

    /* strip 16-bit depth files down to 8 bits */
    if (png_get_bit_depth (png_ptr, info_ptr) == 16)
      png_set_strip_16 (png_ptr);
    /* strip alpha channel
       #### shouldn't we handle this?
       first call png_read_update_info in case above transformations
       have generated an alpha channel */
    png_read_update_info(png_ptr, info_ptr);
    if (png_get_color_type (png_ptr, info_ptr) & PNG_COLOR_MASK_ALPHA)
      png_set_strip_alpha (png_ptr);

    png_read_image (png_ptr, row_pointers);
    png_read_end (png_ptr, info_ptr);

    /* #### There should be some way to pass this type of data down
     * into the glyph code, where you can get to it from lisp
     * anyway. - WMP */
    {
      int i, num_text = 0;
      png_textp text_ptr = NULL;
      DECLARE_EISTRING (key);
      DECLARE_EISTRING (text);

      if (png_get_text (png_ptr, info_ptr, &text_ptr, &num_text) > 0)
        {
          for (i = 0 ; i < num_text; i++)
            {
              eireset (key);
              eireset (text);

              eicpy_ext (key, text_ptr[i].key, Qbinary);
              eicpy_ext (text, text_ptr[i].text, Qbinary);

              warn_when_safe (Qpng, Qinfo, "%s - %s", eidata (key),
                              eidata (text));
            }
        }
    }

    xfree (row_pointers);
  }

  /* now instantiate */
  MAYBE_DEVMETH (DOMAIN_XDEVICE (ii->domain),
		 init_image_instance_from_eimage,
		 (ii, width, height, 1, unwind.eimage, dest_mask,
		  instantiator, pointer_fg, pointer_bg, domain));

  /* This will clean up everything else. */
  unbind_to (speccount);
}

#endif /* HAVE_PNG */


#ifdef HAVE_TIFF
#include "tiffio.h"

/**********************************************************************
 *                             TIFF                                   *
 **********************************************************************/
static void
tiff_validate (Lisp_Object instantiator)
{
  file_or_data_must_be_present (instantiator);
}

static Lisp_Object
tiff_normalize (Lisp_Object inst, Lisp_Object console_type,
		Lisp_Object UNUSED (dest_mask))
{
  return simple_image_type_normalize (inst, console_type, Qtiff);
}

static int
tiff_possible_dest_types (void)
{
  return IMAGE_COLOR_PIXMAP_MASK;
}

struct tiff_unwind_data
{
  Binbyte *eimage;
  /* Object that holds the decoded data from a TIFF file */
  TIFF *tiff;
};

static Lisp_Object
tiff_instantiate_unwind (Lisp_Object unwind_obj)
{
  struct tiff_unwind_data *data =
    (struct tiff_unwind_data *) get_opaque_ptr (unwind_obj);

  free_opaque_ptr (unwind_obj);
  if (data->tiff)
    {
      TIFFClose (data->tiff);
      data->tiff = 0;
    }
  if (data->eimage)
    {
      xfree (data->eimage);
      data->eimage = 0;
    }

  return Qnil;
}

typedef struct tiff_memory_storage
{
  Binbyte *bytes;		/* The data       */
  Bytecount len;		/* How big is it? */
  Bytecount index;		/* Where are we?  */
} tiff_memory_storage;

static size_t
tiff_memory_read (thandle_t data, tdata_t buf, tsize_t size)
{
  tiff_memory_storage *mem = (tiff_memory_storage *) data;

  if ((Bytecount) size > (mem->len - mem->index))
    return (size_t) -1;
  memcpy (buf, mem->bytes + mem->index, size);
  mem->index = mem->index + size;
  return size;
}

static size_t
tiff_memory_write (thandle_t UNUSED (data), tdata_t UNUSED (buf),
		   tsize_t UNUSED (size))
{
  ABORT();
  return 0;
}

static toff_t
tiff_memory_seek (thandle_t data, toff_t off, int whence)
{
  tiff_memory_storage *mem = (tiff_memory_storage *) data;
  int newidx;
  switch(whence)
    {
    case SEEK_SET:
      newidx = off;
      break;
    case SEEK_END:
      newidx = mem->len + off;
      break;
    case SEEK_CUR:
      newidx = mem->index + off;
      break;
    default:
      fprintf (stderr, "Eh? invalid seek mode in tiff_memory_seek\n");
      return (toff_t) -1;
    }

  if ((newidx > mem->len) || (newidx < 0))
    return (toff_t) -1;

  mem->index = newidx;
  return newidx;
}

static int
tiff_memory_close (thandle_t UNUSED (data))
{
  return 0;
}

static int
tiff_map_noop (thandle_t UNUSED (data), tdata_t* UNUSED (pbase),
	       toff_t* UNUSED (psize))
{
  return 0;
}

static void
tiff_unmap_noop (thandle_t UNUSED (data), tdata_t UNUSED (pbase),
		 toff_t UNUSED (psize))
{
  return;
}

static toff_t
tiff_memory_size (thandle_t data)
{
  tiff_memory_storage *mem = (tiff_memory_storage*)data;
  return mem->len;
}

struct tiff_error_struct
{
#ifdef HAVE_VSNPRINTF
  char err_str[256];
#else
  char err_str[1024];		/* return the error string */
#endif
  jmp_buf setjmp_buffer;	/* for return to caller */
};

/* jh 98/03/12 - ###This struct for passing data to the error functions
   is an ugly hack caused by the fact that libtiff (as of v3.4) doesn't
   have any place to store error func data.  This should be rectified
   before XEmacs gets threads! */
static struct tiff_error_struct tiff_err_data;

static void
tiff_error_func (const char *UNUSED (module), const char *fmt, ...)
{
  va_list vargs;

  va_start (vargs, fmt);
#ifdef HAVE_VSNPRINTF
  vsnprintf (tiff_err_data.err_str, 255, fmt, vargs);
#else
  /* pray this doesn't overflow... */
  vsprintf (tiff_err_data.err_str, fmt, vargs);
#endif
  va_end (vargs);
  /* return to setjmp point */
  longjmp (tiff_err_data.setjmp_buffer, 1);
}

static void
tiff_warning_func (const char *module, const char *fmt, ...)
{
  va_list vargs;
#ifdef HAVE_VSNPRINTF
  char warn_str[256];
#else
  char warn_str[1024];
#endif
  DECLARE_EISTRING (eimodule);
  DECLARE_EISTRING (eiwarnstr);

  va_start (vargs, fmt);
#ifdef HAVE_VSNPRINTF
  vsnprintf (warn_str, 255, fmt, vargs);
#else
  vsprintf (warn_str, fmt, vargs);
#endif
  va_end (vargs);

  eicpy_ext(eimodule, module, Qbinary);  
  eicpy_ext(eiwarnstr, warn_str, Qbinary);  

  warn_when_safe (Qtiff, Qinfo, "%s - %s",
                  eidata(eimodule), 
                  eidata(eiwarnstr));
}

static void
tiff_instantiate (Lisp_Object image_instance, Lisp_Object instantiator,
		  Lisp_Object pointer_fg, Lisp_Object pointer_bg,
		  int dest_mask, Lisp_Object domain)
{
  Lisp_Image_Instance *ii = XIMAGE_INSTANCE (image_instance);
  tiff_memory_storage mem_struct;
  /* It is OK for the unwind data to be local to this function,
     because the unwind-protect is always executed when this
     stack frame is still valid. */
  struct tiff_unwind_data unwind;
  int speccount = specpdl_depth ();
  uint32 width, height;

  xzero (unwind);
  record_unwind_protect (tiff_instantiate_unwind, make_opaque_ptr (&unwind));

  /* set up error facilities */
  if (setjmp (tiff_err_data.setjmp_buffer))
    {
      /* An error was signaled. No clean up is needed, as unwind handles that
	 for us.  Just pass the error along. */
      signal_image_error_2 ("TIFF decoding error",
			    build_extstring (tiff_err_data.err_str,
					      Qerror_message_encoding),
			    instantiator);
    }
  TIFFSetErrorHandler ((TIFFErrorHandler)tiff_error_func);
  TIFFSetWarningHandler ((TIFFErrorHandler)tiff_warning_func);
  {
    Lisp_Object data = find_keyword_in_vector (instantiator, Q_data);
    Binbyte *bytes;
    Bytecount len;

    uint32 *raster;
    Binbyte *ep;
    UINT_64_BIT pixels_sq;

    assert (!NILP (data));

    /* #### This is a definite problem under Mule due to the amount of
       stack data it might allocate.  Think about Lstreams... */
    LISP_STRING_TO_SIZED_EXTERNAL (data, bytes, len, Qbinary);
    mem_struct.bytes = bytes;
    mem_struct.len = len;
    mem_struct.index = 0;

    unwind.tiff = TIFFClientOpen ("memfile", "r", (thandle_t) &mem_struct,
				  (TIFFReadWriteProc)tiff_memory_read,
				  (TIFFReadWriteProc)tiff_memory_write,
				  tiff_memory_seek, tiff_memory_close, tiff_memory_size,
				  tiff_map_noop, tiff_unmap_noop);
    if (!unwind.tiff)
      signal_image_error ("Insufficient memory to instantiate TIFF image", instantiator);

    TIFFGetField (unwind.tiff, TIFFTAG_IMAGEWIDTH, &width);
    TIFFGetField (unwind.tiff, TIFFTAG_IMAGELENGTH, &height);
    pixels_sq = (UINT_64_BIT) width * (UINT_64_BIT) height;
    if (pixels_sq >= 1 << 29)
      signal_image_error ("TIFF image too large to instantiate", instantiator);
    unwind.eimage = xnew_binbytes ((size_t) pixels_sq * 3);

    /* #### This is little more than proof-of-concept/function testing.
       It needs to be reimplemented via scanline reads for both memory
       compactness. */
    raster = (uint32*) _TIFFmalloc ((tsize_t) (pixels_sq * sizeof (uint32)));
    if (raster != NULL)
      {
	int i, j;
	uint32 *rp;
	ep = unwind.eimage;
	rp = raster;
	if (TIFFReadRGBAImage (unwind.tiff, width, height, raster, 0))
	  {
	    for (i = height - 1;  i >= 0; i--)
	      {
		/* This is to get around weirdness in the libtiff library where properly
		   made TIFFs will come out upside down.  libtiff bug or jhod-brainlock? */
		rp = raster + (i * width);
		for (j = 0; j < (int) width; j++)
		  {
		    *ep++ = (Binbyte)TIFFGetR(*rp);
		    *ep++ = (Binbyte)TIFFGetG(*rp);
		    *ep++ = (Binbyte)TIFFGetB(*rp);
		    rp++;
		  }
	      }
	  }
	_TIFFfree (raster);
      } else
	signal_image_error ("Unable to allocate memory for TIFFReadRGBA", instantiator);

  }

  /* now instantiate */
  MAYBE_DEVMETH (DOMAIN_XDEVICE (ii->domain),
		 init_image_instance_from_eimage,
		 (ii, width, height, 1, unwind.eimage, dest_mask,
		  instantiator, pointer_fg, pointer_bg, domain));

  unbind_to (speccount);
}

#endif /* HAVE_TIFF */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_glyphs_eimage (void)
{
}

void
image_instantiator_format_create_glyphs_eimage (void)
{
  /* image-instantiator types */
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

}

void
vars_of_glyphs_eimage (void)
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

}
