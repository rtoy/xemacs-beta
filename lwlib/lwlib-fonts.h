/* Font data structures for X and Xft.

Copyright (C) 2004 Free Software Foundation, Inc.

Author:		Stephen J. Turnbull <stephen@xemacs.org>
Created:	24 Jul 2004 by Stephen J. Turnbull

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

/* Synched up with: Not in GNU Emacs. */

#ifndef INCLUDED_lwlib_fonts_h_
#define INCLUDED_lwlib_fonts_h_

/* get headers */

#ifdef USE_XFT
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
#endif /* USE_XFT */

/* Xt name macros */

#ifdef USE_XFT
#ifndef XtNxftFont
#define XtNxftFont "xftFont"
#endif
#ifndef XtCXftFont
#define XtCXftFont "XftFont"
#endif
#endif /* USE_XFT */

/* code 'n' stuff */

#ifdef USE_XFT
#define FCSTRLEN 512

/* non-Lisp prototypes */
/* #### change this back to _open_name */
XftFont *xft_open_font_by_name (Display *dpy, char *name);
#endif /* USE_XFT */

#endif /* INCLUDED_lwlib_fonts_h_ */
