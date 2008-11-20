/* Color data structures for X and Xft.

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

#ifndef INCLUDED_lwlib_colors_h_
#define INCLUDED_lwlib_colors_h_

#include <X11/Xlib.h>

/* WIDGET is an Xt widget, VISUAL and DEPTH are return values */
void visual_info_from_widget (Widget widget, Visual **visual, int *depth);

/* basic version from xlwmenu.c */
int FIXME_allocate_nearest_color (Display *display, Colormap screen_colormap,
				  XColor *color_def);
/* haired-up version from ../src/objects-x.c */
int x_allocate_nearest_color (Display *display, Colormap screen_colormap,
			      Visual *visual, XColor *color_def);

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

XftColor xft_convert_color (Display *dpy, Colormap cmap, Visual *visual,
			    int c, int dim);
#endif /* USE_XFT */

#endif /* INCLUDED_lwlib_colors_h_ */
