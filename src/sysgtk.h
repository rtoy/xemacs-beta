/* Basic include file for GTK/GDK includes.

   Copyright (C) 2010 Ben Wing.

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

/* Synched up with: Not in FSF. */

/* Authorship:

   Ben Wing, 1-28-10 extracted out of console-gtk.h and various other
   files.
*/

#ifndef INCLUDED_sysgtk_h_
#define INCLUDED_sysgtk_h_

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <glib.h>

#include <pango/pango.h>
#include <pango/pangoxft.h>

BEGIN_C_DECLS

extern PangoFontDescription *
pango_fc_font_description_from_pattern (FcPattern *, gboolean);

END_C_DECLS

#endif /* INCLUDED_sysgtk_h_ */
