/* GTK internationalization functions.
   Copyright (C) 2003 Ben Wing.

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

/* Authorship:

   Current primary author: Ben Wing <ben@xemacs.org>
   */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"

/* As best as I can determine from reading the source code, all GTK strings
   (gchar *) are UTF-8 in GTK V2, but locale-encoded in GTK V1.2.  #### As
   usual, the documentation on this is completely nonexistent.  There may
   well be different encodings for particular kinds of data, e.g. selection
   data, drag-n-drop data, etc. --ben */

Lisp_Object Vgtk_text_encoding;

void
vars_of_intl_gtk (void)
{
  staticpro (&Vgtk_text_encoding);
}

void
init_intl_gtk (void)
{
  if (gtk_major_version >= 2)
    Vgtk_text_encoding = Qutf_8;
  else
    Vgtk_text_encoding = Qnative;
}
