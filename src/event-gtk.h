/* Header file for miscellaneous event functions for GTK.
   Copyright (C) 2002 William Perry.

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

#ifndef __EVENT_GTK_H__
#define __EVENT_GTK_H__

int gtk_event_to_emacs_event (struct frame *frame,
			      GdkEvent *gdk_event,
			      struct Lisp_Event *emacs_event);

gint emacs_gtk_key_event_handler(GtkWidget *widget, GdkEventKey *event);
gint emacs_gtk_button_event_handler(GtkWidget *widget, GdkEventButton *event);
gint emacs_gtk_motion_event_handler (GtkWidget *widget, GdkEventMotion *event);

gboolean emacs_shell_event_handler (GtkWidget *wid /* unused */,
				    GdkEvent *event,
				    gpointer closure);
void reinit_vars_of_event_gtk (void);

#endif /* __EVENT-GTK_H__ */
