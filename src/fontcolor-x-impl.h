/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002 Ben Wing.

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

/* Synched up with:  Not in FSF. */

/* This file Mule-ized (more like Mule-verified) by Ben Wing, 7-10-00. */

#ifndef INCLUDED_fontcolor_x_impl_h_
#define INCLUDED_fontcolor_x_impl_h_

#include "fontcolor-impl.h"
#include "fontcolor-x.h"
#ifdef HAVE_XFT
/* for resource name definitions, etc */
#include "../lwlib/lwlib-fonts.h"
#endif

#ifdef HAVE_X_WINDOWS

/*****************************************************************************
 Color-Instance
 ****************************************************************************/

struct x_color_instance_data
{
  XColor color;
  /* Yes, it looks crazy to have both the XColor and the XftColor, but
     pragmatically both are used. */
#ifdef HAVE_XFT
  XftColor xftColor;
#endif
  char dealloc_on_gc;
};

#define X_COLOR_INSTANCE_DATA(c) ((struct x_color_instance_data *) (c)->data)
#define COLOR_INSTANCE_X_COLOR(c) (X_COLOR_INSTANCE_DATA (c)->color)
#define XCOLOR_INSTANCE_X_COLOR(c) COLOR_INSTANCE_X_COLOR (XCOLOR_INSTANCE (c))
#ifdef HAVE_XFT
#define COLOR_INSTANCE_X_XFTCOLOR(c) (X_COLOR_INSTANCE_DATA (c)->xftColor)
#endif
#define COLOR_INSTANCE_X_DEALLOC(c) (X_COLOR_INSTANCE_DATA (c)->dealloc_on_gc)

/*****************************************************************************
 Font-Instance
 ****************************************************************************/

struct x_font_instance_data
{
  /* X-specific information */
  /* Yes, it looks crazy to have both the XFontStruct and the XftFont, but
     pragmatically both are used (lwlib delegates labels to the widget sets,
     which internally use XFontStructs). */
  XFontStruct * font;
#ifdef HAVE_XFT
  XftFont *xftFont;
#endif

};

#define X_FONT_INSTANCE_DATA(f) ((struct x_font_instance_data *) (f)->data)
#define FONT_INSTANCE_X_FONT(f) (X_FONT_INSTANCE_DATA (f)->font)
#define XFONT_INSTANCE_X_FONT(c) FONT_INSTANCE_X_FONT (XFONT_INSTANCE (c))
#ifdef HAVE_XFT
#define FONT_INSTANCE_X_XFTFONT(f) (X_FONT_INSTANCE_DATA (f)->xftFont)
#endif

#endif /* HAVE_X_WINDOWS */

#endif /* INCLUDED_fontcolor_x_impl_h_ */
