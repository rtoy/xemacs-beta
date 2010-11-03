/* Basic include file for GTK/GDK includes.

   Copyright (C) 2010 Ben Wing.

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

   Ben Wing, 1-28-10 extracted out of console-gtk.h and various other
   files.
*/

#ifndef INCLUDED_sysgtk_h_
#define INCLUDED_sysgtk_h_

#include <gtk/gtk.h>
#include <gtk/gtkfixed.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <glib.h>

#ifdef USE_PANGO
#include <pango/pango.h>
#include <pango/pangoxft.h>
#endif

#endif /* INCLUDED_sysgtk_h_ */
