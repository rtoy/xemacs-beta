/* Gtk-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.

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
/* Gtk version by William Perry */

#ifndef _XEMACS_OBJECTS_GTK_IMPL_H_
#define _XEMACS_OBJECTS_GTK_IMPL_H_

#include "fontcolor-impl.h"
#include "fontcolor-gtk.h"

#ifdef HAVE_GTK

/*****************************************************************************
 Color-Instance
 ****************************************************************************/

struct gtk_color_instance_data
{
  GdkColor *color;
  char dealloc_on_gc;
};

#define GTK_COLOR_INSTANCE_DATA(c) ((struct gtk_color_instance_data *) (c)->data)
#define COLOR_INSTANCE_GTK_COLOR(c) (GTK_COLOR_INSTANCE_DATA (c)->color)
#define XCOLOR_INSTANCE_GTK_COLOR(c) COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (c))
#define COLOR_INSTANCE_GTK_DEALLOC(c) (GTK_COLOR_INSTANCE_DATA (c)->dealloc_on_gc)

/*****************************************************************************
 Font-Instance
 ****************************************************************************/

struct gtk_font_instance_data
{
  /* Gtk-specific information */
  GdkFont *font;
};

#define GTK_FONT_INSTANCE_DATA(f) ((struct gtk_font_instance_data *) (f)->data)
#define FONT_INSTANCE_GTK_FONT(f) (GTK_FONT_INSTANCE_DATA (f)->font)
#define XFONT_INSTANCE_GTK_FONT(c) FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (c))

#endif /* HAVE_GTK */
#endif /* _XEMACS_OBJECTS_GTK_IMPL_H_ */
