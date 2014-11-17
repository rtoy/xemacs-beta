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

#ifndef _XEMACS_OBJECTS_GTK_H_
#define _XEMACS_OBJECTS_GTK_H_

#include "fontcolor.h"

#ifdef HAVE_GTK

/*****************************************************************************
 Color-Instance
 ****************************************************************************/

int gtk_parse_nearest_color (struct device *d, GdkColor *color,
			     Lisp_Object name, Error_Behavior errb);

/*****************************************************************************
 Font-Instance
 ****************************************************************************/

#endif /* HAVE_GTK */
#endif /* _XEMACS_OBJECTS_GTK_H_ */
