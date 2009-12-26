/* Interface to graphics/etc libraries, with cdecl calling convention.
   Copyright (C) 2002 Ben Wing

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

#include "libinterface.h"

/* YUUUUUUUUUUUUUUCK!

   This file exists only because VC++ lacks pragmas that let you change the
   default calling convention in the middle of a file.  With optimization,
   we compile XEmacs using fastcall as the default convention; but
   libraries are not compiled this way, and thus need __cdecl in their
   declaration.  Without the pragma, we either

   (a) depend on libraries that declare their external functions with
   suitable preprocessor definitions that we can change;

   (b) force the user to recompile the library with fastcall (not
   reasonable);

   (c) hack the headers manually (not reasonable);

   (d) put all calls to the library in a separate file where the default
   calling convention is __cdecl (hence, this file).

   In fact, some libraries do make (a) feasible.  This is the case with PNG
   and ZLIB.  JPEG almost does so, since all externs are wrapped with
   EXTERN(); but they fail to wrap their definition of EXTERN with #ifndef
   EXTERN, so we can't override it. XPM is similar, wrapping their externs
   with FUNC() -- again, not overridable.  With PNG, we define PNG_EXPORT;
   with ZLIB, we define ZEXPORT and ZEXPORTVA. */

#ifdef HAVE_XPM

void
qxe_XpmFreeXpmImage (XpmImage *image)
{
  XpmFreeXpmImage (image);
}

void
qxe_XpmFreeXpmInfo (XpmInfo *info)
{
  XpmFreeXpmInfo (info);
}

int
qxe_XpmCreateXpmImageFromBuffer (char *buffer, XpmImage *image, XpmInfo *info)
{
  return XpmCreateXpmImageFromBuffer (buffer, image, info);
}

void
qxe_XpmFree (void *ptr)
{
  XpmFree (ptr);
}

int
qxe_XpmReadFileToData (char *filename, char ***data_return)
{
  return XpmReadFileToData (filename, data_return);
}

#endif /* HAVE_XPM */

#ifdef HAVE_JPEG

boolean
qxe_jpeg_finish_decompress (j_decompress_ptr cinfo)
{
  return jpeg_finish_decompress (cinfo);
}

boolean
qxe_jpeg_start_decompress (j_decompress_ptr cinfo)
{
  return jpeg_start_decompress (cinfo);
}

JDIMENSION
qxe_jpeg_read_scanlines (j_decompress_ptr cinfo, JSAMPARRAY scanlines,
			 JDIMENSION max_lines)
{
  return jpeg_read_scanlines (cinfo, scanlines, max_lines);
}

int
qxe_jpeg_read_header (j_decompress_ptr cinfo, boolean require_image)
{
  return jpeg_read_header (cinfo, require_image);
}

void
qxe_jpeg_CreateDecompress (j_decompress_ptr cinfo, int version,
			   size_t structsize)
{
  jpeg_CreateDecompress (cinfo, version, structsize);
}

struct jpeg_error_mgr *
qxe_jpeg_std_error (struct jpeg_error_mgr *err)
{
  return jpeg_std_error (err);
}

void
qxe_jpeg_destroy_decompress (j_decompress_ptr cinfo)
{
  jpeg_destroy_decompress (cinfo);
}

boolean
qxe_jpeg_resync_to_restart (j_decompress_ptr cinfo, int desired)
{
  return jpeg_resync_to_restart (cinfo, desired);
}

#endif /* HAVE_JPEG */

#ifdef HAVE_TIFF

tdata_t
qxe_TIFFmalloc (tsize_t x1)
{
  return _TIFFmalloc (x1);
}

void
qxe_TIFFfree (tdata_t x1)
{
  _TIFFfree (x1);
}

void
qxe_TIFFClose (TIFF *x1)
{
  TIFFClose (x1);
}

int
qxe_TIFFGetField (TIFF *x1, ttag_t x2, uint32 *x3)
{
  return TIFFGetField (x1, x2, x3);
}

TIFF *
qxe_TIFFClientOpen (const char *x1, const char *x2,
		    thandle_t x3,
		    TIFFReadWriteProc x4, TIFFReadWriteProc x5,
		    TIFFSeekProc x6, TIFFCloseProc x7,
		    TIFFSizeProc x8,
		    TIFFMapFileProc x9, TIFFUnmapFileProc x10)
{
  return TIFFClientOpen (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
}

TIFFErrorHandler
qxe_TIFFSetErrorHandler (TIFFErrorHandler x1)
{
  return TIFFSetErrorHandler (x1);
}

TIFFErrorHandler
qxe_TIFFSetWarningHandler (TIFFErrorHandler x1)
{
  return TIFFSetWarningHandler (x1);
}

int
qxe_TIFFReadRGBAImage (TIFF *x1, uint32 x2, uint32 x3, uint32 *x4, int x5)
{
  return TIFFReadRGBAImage (x1, x2, x3, x4, x5);
}

#endif /* HAVE_TIFF */
