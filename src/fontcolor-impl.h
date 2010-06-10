/* Generic object functions -- header implementation.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002 Ben Wing.
   Copyright (C) 2010 Didier Verna

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

#ifndef INCLUDED_fontcolor_impl_h_
#define INCLUDED_fontcolor_impl_h_

#include "specifier.h"
#include "fontcolor.h"

/*****************************************************************************
 *                        Color Specifier Object                             *
 *****************************************************************************/

struct color_specifier
{
  Lisp_Object face;		/* face this is attached to, or nil */
  Lisp_Object face_property;	/* property of that face */
};

#define COLOR_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, color)
#define COLOR_SPECIFIER_FACE(g) (COLOR_SPECIFIER_DATA (g)->face)
#define COLOR_SPECIFIER_FACE_PROPERTY(g) \
  (COLOR_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE (color);
#define XCOLOR_SPECIFIER(x) XSPECIFIER_TYPE (x, color)
#define COLOR_SPECIFIERP(x) SPECIFIER_TYPEP (x, color)
#define CHECK_COLOR_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, color)
#define CONCHECK_COLOR_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, color)

/*****************************************************************************
 *                         Font Specifier Object                             *
 *****************************************************************************/

struct font_specifier
{
  Lisp_Object face;		/* face this is attached to, or nil */
  Lisp_Object face_property;	/* property of that face */
};

#define FONT_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, font)
#define FONT_SPECIFIER_FACE(g) (FONT_SPECIFIER_DATA (g)->face)
#define FONT_SPECIFIER_FACE_PROPERTY(g) \
  (FONT_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE (font);
#define XFONT_SPECIFIER(x) XSPECIFIER_TYPE (x, font)
#define FONT_SPECIFIERP(x) SPECIFIER_TYPEP (x, font)
#define CHECK_FONT_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, font)
#define CONCHECK_FONT_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, font)

/*****************************************************************************
 *                       Face Boolean Specifier Object                       *
 *****************************************************************************/

struct face_boolean_specifier
{
  Lisp_Object face;		/* face this is attached to, or nil */
  Lisp_Object face_property;	/* property of that face */
};

#define FACE_BOOLEAN_SPECIFIER_DATA(g) SPECIFIER_TYPE_DATA (g, face_boolean)
#define FACE_BOOLEAN_SPECIFIER_FACE(g) (FACE_BOOLEAN_SPECIFIER_DATA (g)->face)
#define FACE_BOOLEAN_SPECIFIER_FACE_PROPERTY(g) \
  (FACE_BOOLEAN_SPECIFIER_DATA (g)->face_property)

DECLARE_SPECIFIER_TYPE (face_boolean);
extern Lisp_Object Qface_boolean;
#define XFACE_BOOLEAN_SPECIFIER(x) XSPECIFIER_TYPE (x, face_boolean)
#define FACE_BOOLEAN_SPECIFIERP(x) SPECIFIER_TYPEP (x, face_boolean)
#define CHECK_FACE_BOOLEAN_SPECIFIER(x) \
  CHECK_SPECIFIER_TYPE (x, face_boolean)
#define CONCHECK_FACE_BOOLEAN_SPECIFIER(x) \
  CONCHECK_SPECIFIER_TYPE (x, face_boolean)

/*****************************************************************************
 *                  Background Placement Specifier Object                    *
 *****************************************************************************/

struct face_background_placement_specifier
{
  Lisp_Object face;		/* face this is attached to, or nil */
};

#define FACE_BACKGROUND_PLACEMENT_SPECIFIER_DATA(g)	\
  SPECIFIER_TYPE_DATA (g, face_background_placement)
#define FACE_BACKGROUND_PLACEMENT_SPECIFIER_FACE(g)	\
  (FACE_BACKGROUND_PLACEMENT_SPECIFIER_DATA (g)->face)

DECLARE_SPECIFIER_TYPE (face_background_placement);
extern Lisp_Object Qface_background_placement, Qabsolute, Qrelative;
#define XFACE_BACKGROUND_PLACEMENT_SPECIFIER(x)		\
  XSPECIFIER_TYPE (x, face_background_placement)
#define FACE_BACKGROUND_PLACEMENT_SPECIFIERP(x)		\
  SPECIFIER_TYPEP (x, face_background_placement)
#define CHECK_FACE_BACKGROUND_PLACEMENT_SPECIFIER(x)	\
  CHECK_SPECIFIER_TYPE (x, face_background_placement)
#define CONCHECK_FACE_BACKGROUND_PLACEMENT_SPECIFIER(x)		\
  CONCHECK_SPECIFIER_TYPE (x, face_background_placement)

/****************************************************************************
 *                           Color Instance Object                          *
 ****************************************************************************/

struct Lisp_Color_Instance
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object name;
  Lisp_Object device;

  /* See comment in struct console about console variants. */
  enum console_variant color_instance_type;

  /* console-type-specific data */
  void *data;
};

#define COLOR_INSTANCE_NAME(c)   ((c)->name)
#define COLOR_INSTANCE_DEVICE(c) ((c)->device)

/****************************************************************************
 *                            Font Instance Object                          *
 ****************************************************************************/

struct Lisp_Font_Instance
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object name; /* the instantiator used to create the font instance */
  Lisp_Object truename; /* used by the device-specific methods; we need to
			   call them to get the truename (#### in reality,
			   they all probably just store the truename here
			   if they know it, and nil otherwise; we should
			   check this and enforce it as a general policy
			   X and GTK do this, except that when they don't
			   know they return NAME and don't update TRUENAME.
			   MS Windows initializes TRUENAME when the font is
			   initialized.  TTY doesn't do truename.) */
  Lisp_Object device;
  Lisp_Object charset;  /* Mule charset, or whatever */

  /* See comment in struct console about console variants. */
  enum console_variant font_instance_type;

  unsigned short ascent;	/* extracted from `font', or made up */
  unsigned short descent;
  unsigned short width;
  unsigned short height;
  int proportional_p;

  /* console-type-specific data */
  void *data;
};

#define FONT_INSTANCE_NAME(f)	 ((f)->name)
#define FONT_INSTANCE_TRUENAME(f) ((f)->truename)
#define FONT_INSTANCE_CHARSET(f) ((f)->charset)
#define FONT_INSTANCE_DEVICE(f)	 ((f)->device)
#define FONT_INSTANCE_ASCENT(f)	 ((f)->ascent)
#define FONT_INSTANCE_DESCENT(f) ((f)->descent)
#define FONT_INSTANCE_WIDTH(f)	 ((f)->width)
#define FONT_INSTANCE_HEIGHT(f)	 ((f)->height)
#define FONT_INSTANCE_PROPORTIONAL(f) ((f)->proportional_p)

#define XFONT_INSTANCE_NAME(f)	   FONT_INSTANCE_NAME (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_TRUENAME(f) FONT_INSTANCE_TRUENAME (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_CHARSET(f)  FONT_INSTANCE_CHARSET (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_DEVICE(f)   FONT_INSTANCE_DEVICE (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_ASCENT(f)   FONT_INSTANCE_ASCENT (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_DESCENT(f)  FONT_INSTANCE_DESCENT (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_WIDTH(f)	   FONT_INSTANCE_WIDTH (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_HEIGHT(f)   FONT_INSTANCE_HEIGHT (XFONT_INSTANCE (f))
#define XFONT_INSTANCE_PROPORTIONAL(f) FONT_INSTANCE_PROPORTIONAL (XFONT_INSTANCE (f))

#endif /* INCLUDED_fontcolor_impl_h_ */
