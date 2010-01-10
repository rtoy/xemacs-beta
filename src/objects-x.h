/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002 Ben Wing.

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

/* Synched up with:  Not in FSF. */

/* This file Mule-ized (more like Mule-verified) by Ben Wing, 7-10-00. */

#ifndef INCLUDED_objects_x_h_
#define INCLUDED_objects_x_h_

#include "objects.h"
#include "../lwlib/lwlib-colors.h" /* for x_allocate_nearest_color */

#ifdef HAVE_X_WINDOWS

#ifdef USE_XFT
EXFUN (Ffc_font_real_pattern, 2);
#endif

/* Lisp_Object Fxlfd_font_name_p; */

#endif /* HAVE_X_WINDOWS */

#endif /* INCLUDED_objects_x_h_ */
