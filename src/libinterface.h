/* Define prototypes for graphics library interface functions.
   Copyright (C) 2002 Ben Wing.

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

#ifndef INCLUDED_libinterface_h_
#define INCLUDED_libinterface_h_

#ifdef HAVE_GIF
#include "gifrlib.h"
#endif /* HAVE_GIF */

#ifdef HAVE_PNG
BEGIN_C_DECLS
#define message message_ /* Yuck */
  /* See comment in libinterface.c for the following */
#define PNG_EXPORT(type, symbol) type XCDECL symbol
#define ZEXPORT XCDECL
#define ZEXPORTVA XCDECL
#include <png.h>
#undef message
END_C_DECLS
#endif /* HAVE_PNG */

/* #### WARNING: Utterly random magic here to prevent namespace conflicts
   when no one bothers to be the least bit namespace-clean.  Potential
   problems: INT32, FAR.  DONT_NEED_JPEG avoids the problem with FAR in
   glyphs-msw.c.  For some reason, putting the XPM code after this fixes
   other problems; don't move it before. --ben */
#if defined (HAVE_JPEG) && !defined (DONT_NEED_JPEG)
BEGIN_C_DECLS
#ifdef _MSC_VER
# ifndef XMD_H
  /* Yuck!  This tricks jpeglib.h into not defining INT32, which is defined
     in VC98/INCLUDE/basetsd.h */
#  define UNDEF_XMD_H_ME_HARDER
#  define XMD_H
# endif
#endif /* _MSC_VER */
#include <jpeglib.h>
#ifdef UNDEF_XMD_H_ME_HARDER
# undef XMD_H
#endif
#include <jerror.h>
END_C_DECLS

boolean XCDECL qxe_jpeg_finish_decompress (j_decompress_ptr cinfo);
boolean XCDECL qxe_jpeg_start_decompress (j_decompress_ptr cinfo);
JDIMENSION XCDECL qxe_jpeg_read_scanlines (j_decompress_ptr cinfo,
					  JSAMPARRAY scanlines,
					  JDIMENSION max_lines);
int XCDECL qxe_jpeg_read_header (j_decompress_ptr cinfo, boolean require_image);
#define qxe_jpeg_create_decompress(cinfo)				      \
    qxe_jpeg_CreateDecompress((cinfo), JPEG_LIB_VERSION,		      \
			      (size_t) sizeof(struct jpeg_decompress_struct))
void XCDECL qxe_jpeg_CreateDecompress (j_decompress_ptr cinfo, int version,
				      size_t structsize);
struct jpeg_error_mgr * XCDECL qxe_jpeg_std_error (struct jpeg_error_mgr *err);
void XCDECL qxe_jpeg_destroy_decompress (j_decompress_ptr cinfo);
boolean XCDECL qxe_jpeg_resync_to_restart (j_decompress_ptr cinfo, int desired);

#endif /* defined (HAVE_JPEG) && !defined (DONT_NEED_JPEG) */

#ifdef HAVE_XPM

#ifndef HAVE_GTK /* #### ????????????????????? No comprendo ni un poco.
		    This was here before, in a different file. --ben */
#include <X11/xpm.h>
#endif

void XCDECL qxe_XpmFreeXpmImage (XpmImage *image);
void XCDECL qxe_XpmFreeXpmInfo (XpmInfo *info);
int XCDECL qxe_XpmCreateXpmImageFromBuffer (char *buffer, XpmImage *image,
					   XpmInfo *info);
void XCDECL qxe_XpmFree (void *ptr);
int XCDECL qxe_XpmReadFileToData (char *filename, char ***data_return);

#endif /* HAVE_XPM */

#ifdef HAVE_TIFF
#include "tiffio.h"

tdata_t XCDECL qxe_TIFFmalloc (tsize_t x1);
void XCDECL qxe_TIFFfree (tdata_t x1);
void XCDECL qxe_TIFFClose (TIFF *x1);
int XCDECL qxe_TIFFGetField (TIFF *x1, ttag_t x2, uint32 *x3);
TIFF * XCDECL qxe_TIFFClientOpen (const char *x1, const char *x2,
				 thandle_t x3, TIFFReadWriteProc x4,
				 TIFFReadWriteProc x5, TIFFSeekProc x6,
				 TIFFCloseProc x7, TIFFSizeProc x8,
				 TIFFMapFileProc x9, TIFFUnmapFileProc x10);
TIFFErrorHandler XCDECL qxe_TIFFSetErrorHandler (TIFFErrorHandler x1);
TIFFErrorHandler XCDECL qxe_TIFFSetWarningHandler (TIFFErrorHandler x1);
int XCDECL qxe_TIFFReadRGBAImage (TIFF *x1, uint32 x2, uint32 x3, uint32 *x4,
				 int x5);

#endif /* HAVE_TIFF */

#endif /* INCLUDED_libinterface_h_ */
