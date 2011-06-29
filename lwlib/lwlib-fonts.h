/* Font data structures for X and Xft.

Copyright (C) 2004 Free Software Foundation, Inc.

Author:		Stephen J. Turnbull <stephen@xemacs.org>
Created:	24 Jul 2004 by Stephen J. Turnbull

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

/* Synched up with: Not in GNU Emacs. */

#ifndef INCLUDED_lwlib_fonts_h_
#define INCLUDED_lwlib_fonts_h_

/* get headers */

#ifdef HAVE_XFT
#define _XFT_NO_COMPAT_
/* shut up GCC */
#define face_index face_index_arg
#define glyph_index glyph_index_arg
#include <X11/Xft/Xft.h>
#undef glyph_index
#undef face_index

#if XFT_MAJOR < 2
#error Xft versions < 2 not supported
#endif
#endif /* HAVE_XFT */

/* Xt name macros */

#ifdef HAVE_XFT
#ifndef XtNxftFont
#define XtNxftFont "xftFont"
#define XtCXftFont "XftFont"
#endif
#ifndef XtNfcFont
#define XtNfcFontName "fcFontName"
#define XtCFcFontName "FcFontName"
#endif
#endif /* HAVE_XFT */

/* code 'n' stuff */

#ifdef HAVE_XFT
#define FCSTRLEN 512

/* non-Lisp prototypes */
/* #### change this back to _open_name */
XftFont *xft_open_font_by_name (Display *dpy, char *name);
#endif /* HAVE_XFT */

#endif /* INCLUDED_lwlib_fonts_h_ */
