/* Define GTK specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2002 Ben Wing.

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

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson (original
      multi-device work by Chuck Thompson).
   Gtk version by William M. Perry

 */

#ifndef INCLUDED_console_gtk_impl_h_
#define INCLUDED_console_gtk_impl_h_

#ifdef HAVE_GTK

#include "console-impl.h"
#include "console-gtk.h"
#include <X11/Xlib.h>

#ifndef GDK_DRAWABLE
//#define GDK_DRAWABLE(x) (GdkDrawable *) (x)
#endif

#define GCHAR(x) ((gchar *)x)

DECLARE_CONSOLE_TYPE (gtk);

struct gtk_device
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */
  /* Gtk application info. */
  GtkWidget *gtk_app_shell;

  /* Selected visual, depth and colormap for this device */
  GdkVisual *visual;
  int depth;

  PangoContext *context;
  PangoFontMap *font_map;

  /* frame that holds the WM_COMMAND property; there should be exactly
     one of these per device. */
  Lisp_Object WM_COMMAND_frame;

  /* The following items are all used exclusively in event-gtk.c. */
  int MetaMask, HyperMask, SuperMask, AltMask, ModeMask;
  KeySym lock_interpretation;

  XModifierKeymap *x_modifier_keymap;

  KeySym *x_keysym_map;
  int x_keysym_map_min_code;
  int x_keysym_map_max_code;
  int x_keysym_map_keysyms_per_code;
  Lisp_Object x_keysym_map_hashtable;

  /* #### It's not clear that there is much distinction anymore
     between mouse_timestamp and global_mouse_timestamp, now that
     Emacs doesn't see most (all?) events not destined for it. */

  /* The timestamp of the last button or key event used by emacs itself.
     This is used for asserting selections and input focus. */
  guint32 mouse_timestamp;

  /* This is the timestamp the last button or key event whether it was
     dispatched to emacs or widgets. */
  guint32 global_mouse_timestamp;

  /* This is the last known timestamp received from the server.  It is
     maintained by x_event_to_emacs_event and used to patch bogus
     WM_TAKE_FOCUS messages sent by Mwm. */
  guint32 last_server_timestamp;

  GdkAtom atom_WM_PROTOCOLS;
  GdkAtom atom_WM_TAKE_FOCUS;
  GdkAtom atom_WM_STATE;

#if 0
	/* #### BILL!!! */
  /* stuff for sticky modifiers: */
  unsigned int need_to_add_mask, down_mask;
  KeyCode last_downkey;
  guint32 release_time;
#endif
};

#ifdef NEW_GC
typedef struct gtk_device Lisp_Gtk_Device;

DECLARE_LISP_OBJECT (gtk_device, Lisp_Gtk_Device);

#define XGTK_DEVICE(x) \
  XRECORD (x, gtk_device, Lisp_Gtk_Device)
#define wrap_gtk_device(p) wrap_record (p, gtk_device)
#define GTK_DEVICE_P(x) RECORDP (x, gtk_device)
#endif /* NEW_GC */

#define DEVICE_GTK_DATA(d) DEVICE_TYPE_DATA (d, gtk)

#define DEVICE_GTK_VISUAL(d)	(DEVICE_GTK_DATA (d)->visual)
#define DEVICE_GTK_DEPTH(d)	(DEVICE_GTK_DATA (d)->depth)
#define DEVICE_GTK_CONTEXT(d) 	(DEVICE_GTK_DATA (d)->context)
#define DEVICE_GTK_FONT_MAP(d)	(DEVICE_GTK_DATA (d)->font_map)
#define DEVICE_GTK_APP_SHELL(d) 	(DEVICE_GTK_DATA (d)->gtk_app_shell)
#define DEVICE_GTK_WM_COMMAND_FRAME(d) (DEVICE_GTK_DATA (d)->WM_COMMAND_frame)
#define DEVICE_GTK_MOUSE_TIMESTAMP(d)  (DEVICE_GTK_DATA (d)->mouse_timestamp)
#define DEVICE_GTK_GLOBAL_MOUSE_TIMESTAMP(d) (DEVICE_GTK_DATA (d)->global_mouse_timestamp)
#define DEVICE_GTK_LAST_SERVER_TIMESTAMP(d)  (DEVICE_GTK_DATA (d)->last_server_timestamp)

/* The maximum number of widgets that can be displayed above the text
   area at one time.  Currently no more than 3 will ever actually be
   displayed (menubar, psheet, debugger panel). */
#define MAX_CONCURRENT_TOP_WIDGETS 8

struct gtk_frame
{
#ifdef NEW_GC
  NORMAL_LISP_OBJECT_HEADER header;
#endif /* NEW_GC */

  /* The widget of this frame. */
  GtkWidget *widget;		/* This is really a GtkWindow */

  /* The layout manager */
  GtkWidget *container;		/* actually a GtkVBox. */

  /* The widget of the menubar */
  GtkWidget *menubar_widget;

  /* The widget of the edit portion of this frame; this is a GtkDrawingArea,
     and the window of this widget is what the redisplay code draws on. */
  GtkWidget *edit_widget;
  /* #### WARNING: this does not currently work. -- dvl
     Position of the edit widget above, for absolute background placement.
     
     int x, y;
  */

  /* Lists the widgets above the text area, in the proper order. */
  GtkWidget *top_widgets[MAX_CONCURRENT_TOP_WIDGETS];
  int num_top_widgets;

  /* Our container widget as a Lisp_Object */
  Lisp_Object lisp_visible_widgets[3];

  Lisp_Object menubar_data;

  /* The icon pixmaps; these are Lisp_Image_Instance objects, or Qnil. */
  Lisp_Object icon_pixmap;
  Lisp_Object icon_pixmap_mask;

  /* geometry string that ought to be freed. */
  char *geom_free_me_please;

  /* 1 if the frame is completely visible on the display, 0 otherwise.
     if 0 the frame may have been iconified or may be totally
     or partially hidden by another X window */
  unsigned int totally_visible_p :1;

  /* Are we a top-level frame?  This means that our shell is a
     TopLevelShell, and we should do certain things to interact with
     the window manager. */
  unsigned int top_level_frame_p :1;

  /* Are we iconfied right now? */
  unsigned int iconified_p :1;

  /* Data for widget callbacks.  It is impossible to pass all the necessary
     data through the GTK signal API so instead it is registered here and the
     hash key is passed instead. */
  Lisp_Object widget_instance_hash_table;
  Lisp_Object widget_callback_hash_table;
  Lisp_Object widget_callback_ex_hash_table;
};

#ifdef NEW_GC
typedef struct gtk_frame Lisp_Gtk_Frame;

DECLARE_LISP_OBJECT (gtk_frame, Lisp_Gtk_Frame);

#define XGTK_FRAME(x) \
  XRECORD (x, gtk_frame, Lisp_Gtk_Frame)
#define wrap_gtk_frame(p) wrap_record (p, gtk_frame)
#define GTK_FRAME_P(x) RECORDP (x, gtk_frame)
#endif /* NEW_GC */

#define FRAME_GTK_DATA(f) FRAME_TYPE_DATA (f, gtk)

/* #### WARNING: this does not currently work. -- dvl
   #define FRAME_GTK_X(f) (FRAME_GTK_DATA (f)->x)
   #define FRAME_GTK_Y(f) (FRAME_GTK_DATA (f)->y)
*/
#define FRAME_GTK_SHELL_WIDGET(f)	    (FRAME_GTK_DATA (f)->widget)
#define FRAME_GTK_CONTAINER_WIDGET(f) (FRAME_GTK_DATA (f)->container)
#define FRAME_GTK_MENUBAR_WIDGET(f)   (FRAME_GTK_DATA (f)->menubar_widget)
#define FRAME_GTK_TEXT_WIDGET(f)	    (FRAME_GTK_DATA (f)->edit_widget)
#define FRAME_GTK_TOP_WIDGETS(f)	    (FRAME_GTK_DATA (f)->top_widgets)
#define FRAME_GTK_NUM_TOP_WIDGETS(f)	  (FRAME_GTK_DATA (f)->num_top_widgets)
#define FRAME_GTK_ICONIFIED_P(f)	  (FRAME_GTK_DATA (f)->iconfigied_p)

#define FRAME_GTK_MENUBAR_DATA(f)   (FRAME_GTK_DATA (f)->menubar_data)

#define FRAME_GTK_LISP_WIDGETS(f)      (FRAME_GTK_DATA (f)->lisp_visible_widgets)
#define FRAME_GTK_ICON_PIXMAP(f)	    (FRAME_GTK_DATA (f)->icon_pixmap)
#define FRAME_GTK_ICON_PIXMAP_MASK(f) (FRAME_GTK_DATA (f)->icon_pixmap_mask)

#define FRAME_GTK_GEOM_FREE_ME_PLEASE(f) (FRAME_GTK_DATA (f)->geom_free_me_please)

#define FRAME_GTK_TOTALLY_VISIBLE_P(f) (FRAME_GTK_DATA (f)->totally_visible_p)
#define FRAME_GTK_TOP_LEVEL_FRAME_P(f) (FRAME_GTK_DATA (f)->top_level_frame_p)
#define FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_instance_hash_table)
#define FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_callback_hash_table)
#define FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_callback_ex_hash_table)

extern struct console_type *gtk_console_type;

/* Special data used to quickly identify the frame that contains a widget. */
#define GTK_DATA_FRAME_IDENTIFIER g_quark_from_string ("xemacs::frame")

/* The hashcode in the frame hash table of a tab_control tab's callback data. */
#define GTK_DATA_TAB_HASHCODE_IDENTIFIER g_quark_from_string ("xemacs::tab_hashcode")

#define GTK_DATA_GUI_IDENTIFIER g_quark_from_string ("xemacs::gui_id")

#endif /* HAVE_GTK */
#endif /* INCLUDED_console_gtk_impl_h_ */
