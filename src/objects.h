/* Generic object functions -- interface.
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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_objects_h_
#define INCLUDED_objects_h_

DECLARE_DOESNT_RETURN (finalose (void *ptr));

/****************************************************************************
 *                           Color Instance Object                          *
 ****************************************************************************/

DECLARE_LRECORD (color_instance, Lisp_Color_Instance);
#define XCOLOR_INSTANCE(x) XRECORD (x, color_instance, Lisp_Color_Instance)
#define wrap_color_instance(p) wrap_record (p, color_instance)
#define COLOR_INSTANCEP(x) RECORDP (x, color_instance)
#define CHECK_COLOR_INSTANCE(x) CHECK_RECORD (x, color_instance)
#define CONCHECK_COLOR_INSTANCE(x) CONCHECK_RECORD (x, color_instance)

EXFUN (Fmake_color_instance, 3);

extern Lisp_Object Vthe_null_color_instance;

void set_color_attached_to (Lisp_Object obj, Lisp_Object face,
			    Lisp_Object property);

/****************************************************************************
 *                            Font Instance Object                          *
 ****************************************************************************/

void initialize_charset_font_caches (struct device *d);
void invalidate_charset_font_caches (Lisp_Object charset);

DECLARE_LRECORD (font_instance, Lisp_Font_Instance);
#define XFONT_INSTANCE(x) XRECORD (x, font_instance, Lisp_Font_Instance)
#define wrap_font_instance(p) wrap_record (p, font_instance)
#define FONT_INSTANCEP(x) RECORDP (x, font_instance)
#define CHECK_FONT_INSTANCE(x) CHECK_RECORD (x, font_instance)
#define CONCHECK_FONT_INSTANCE(x) CONCHECK_RECORD (x, font_instance)

EXFUN (Fmake_font_instance, 3);
EXFUN (Ffont_instance_name, 1);
EXFUN (Ffont_instance_p, 1);
EXFUN (Ffont_instance_truename, 1);

extern Lisp_Object Vthe_null_font_instance;

void set_font_attached_to (Lisp_Object obj, Lisp_Object face,
			   Lisp_Object property);

/*****************************************************************************
 *                       Face Boolean Specifier Object                       *
 *****************************************************************************/

void set_face_boolean_attached_to (Lisp_Object obj, Lisp_Object face,
				   Lisp_Object property);

#endif /* INCLUDED_objects_h_ */
