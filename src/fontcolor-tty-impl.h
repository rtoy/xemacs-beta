/* TTY-specific Lisp objects.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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

#ifndef INCLUDED_fontcolor_tty_impl_h_
#define INCLUDED_fontcolor_tty_impl_h_

#include "fontcolor-impl.h"
#include "fontcolor-tty.h"

struct tty_color_instance_data
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */
  Lisp_Object symbol; /* so we don't have to constantly call Fintern() */
};

#ifdef NEW_GC
DECLARE_LISP_OBJECT (tty_color_instance_data, struct tty_color_instance_data);
#define XTTY_COLOR_INSTANCE_DATA(x) \
  XRECORD (x, tty_color_instance_data, struct tty_color_instance_data)
#define wrap_tty_color_instance_data(p) \
  wrap_record (p, tty_color_instance_data)
#define TTY_COLOR_INSTANCE_DATAP(x) RECORDP (x, tty_color_instance_data)
#define CHECK_TTY_COLOR_INSTANCE_DATA(x) \
  CHECK_RECORD (x, tty_color_instance_data)
#define CONCHECK_TTY_COLOR_INSTANCE_DATA(x) \
  CONCHECK_RECORD (x, tty_color_instance_data)
#endif /* NEW_GC */

#define TTY_COLOR_INSTANCE_DATA(c) 				\
  ((struct tty_color_instance_data *) (c)->data)

#define COLOR_INSTANCE_TTY_SYMBOL(c) (TTY_COLOR_INSTANCE_DATA (c)->symbol)

struct tty_font_instance_data
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */
  Lisp_Object charset;
};

#ifdef NEW_GC
DECLARE_LISP_OBJECT (tty_font_instance_data, struct tty_font_instance_data);
#define XTTY_FONT_INSTANCE_DATA(x) \
  XRECORD (x, tty_font_instance_data, struct tty_font_instance_data)
#define wrap_tty_font_instance_data(p) \
  wrap_record (p, tty_font_instance_data)
#define TTY_FONT_INSTANCE_DATAP(x) RECORDP (x, tty_font_instance_data)
#define CHECK_TTY_FONT_INSTANCE_DATA(x) \
  CHECK_RECORD (x, tty_font_instance_data)
#define CONCHECK_TTY_FONT_INSTANCE_DATA(x) \
  CONCHECK_RECORD (x, tty_font_instance_data)
#endif /* NEW_GC */

#define TTY_FONT_INSTANCE_DATA(c) 				\
  ((struct tty_font_instance_data *) (c)->data)

#define FONT_INSTANCE_TTY_CHARSET(c) (TTY_FONT_INSTANCE_DATA (c)->charset)

#endif /* INCLUDED_fontcolor_tty_impl_h_ */
