/* Define X specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2002, 2003 Ben Wing.

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

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson (original
      multi-device work by Chuck Thompson).
 */

#ifndef _XEMACS_CONSOLE_GTK_H_
#define _XEMACS_CONSOLE_GTK_H_

#ifdef HAVE_GTK

#include "console.h"
#include <gtk/gtk.h>

/* Variables associated with the X display frame this emacs is using. */

extern Lisp_Object Vx_gc_pointer_shape;
extern Lisp_Object Vx_scrollbar_pointer_shape;

/* Number of pixels below each line. */
extern int gtk_interline_space;

extern int gtk_selection_timeout;

struct frame *gtk_widget_to_frame (GtkWidget *);
struct frame *gtk_any_window_to_frame (struct device *d, GdkWindow *);
struct frame *gtk_window_to_frame (struct device *d, GdkWindow *);
struct frame *gtk_any_widget_or_parent_to_frame (struct device *d, GtkWidget *widget);
struct frame *decode_gtk_frame (Lisp_Object);
struct device *gtk_any_window_to_device (GdkWindow *);
struct device *decode_gtk_device (Lisp_Object);
void gtk_handle_property_notify (GdkEventProperty *event);

void signal_special_gtk_user_event (Lisp_Object channel, Lisp_Object function,
				    Lisp_Object object);
void gtk_output_string (struct window *w, struct display_line *dl,
		      Ichar_dynarr *buf, int xpos, int xoffset,
		      int start_pixpos, int width, face_index findex,
		      int cursor, int cursor_start, int cursor_width,
		      int cursor_height);
void gtk_output_shadows (struct frame *f, int x, int y, int width,
		       int height, int shadow_thickness);
GdkGC *gtk_get_gc (struct device *d, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
		   Lisp_Object bg_pmap, Lisp_Object lwidth);

int gtk_initialize_frame_menubar (struct frame *f);
void gtk_init_modifier_mapping (struct device *d);

void Initialize_Locale (void);

extern Lisp_Object Vgtk_initial_argv_list; /* #### ugh! */

const char *gtk_event_name (GdkEventType event_type);

void reinit_console_type_create_gtk (void);

void emacs_gtk_selection_handle (GtkWidget *,
				 GtkSelectionData *selection_data,
				 guint info,
				 guint time_stamp,
				 gpointer data);
void emacs_gtk_selection_clear_event_handle (GtkWidget *widget,
					     GdkEventSelection *event,
					     gpointer data);
void emacs_gtk_selection_received (GtkWidget *widget,
				   GtkSelectionData *selection_data,
				   gpointer user_data);

typedef unsigned int GUI_ID;
extern GUI_ID new_gui_id (void);

extern void gcpro_popup_callbacks (GUI_ID id, Lisp_Object data);
extern void ungcpro_popup_callbacks (GUI_ID id);
extern Lisp_Object get_gcpro_popup_callbacks (GUI_ID id);

#endif /* HAVE_GTK */
#endif /* _XEMACS_DEVICE_X_H_ */
