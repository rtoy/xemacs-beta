/* Functions for the GTK toolkit.
   Copyright (C) 1989, 1992-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2002, 2003, 2010 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* Substantially rewritten for XEmacs.  */
/* Revamped to use Gdk/Gtk by William Perry */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "events.h"
#include "extents.h"
#include "faces.h"
#include "gutter.h"
#include "frame-impl.h"
#include "window.h"

#ifdef HAVE_DRAGNDROP
#include "dragdrop.h"
#endif

#include "elhash.h"
#include "console-gtk-impl.h"
#include "glyphs-gtk.h"
#include "fontcolor-gtk-impl.h"
#include "scrollbar-gtk.h"
#include "ui-gtk.h"

#include "gtk-xemacs.h"

#define BORDER_WIDTH 2
#define INTERNAL_BORDER_WIDTH 2

#define TRANSIENT_DATA_IDENTIFIER "xemacs::transient_for"
#define UNMAPPED_DATA_IDENTIFIER "xemacs::initially_unmapped"

#define STUPID_X_SPECIFIC_GTK_STUFF

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
#include "sysgdkx.h"
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_gtk_frame_plist;

Lisp_Object Qdetachable_menubar;
Lisp_Object Qtext_widget;
Lisp_Object Qcontainer_widget;
Lisp_Object Qshell_widget;

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
EXFUN (Fgtk_window_id, 1);
#endif

#ifdef HAVE_DRAGNDROP
enum {
  TARGET_TYPE_STRING,
  TARGET_TYPE_URI_LIST,
};

static GtkTargetEntry dnd_target_table[] = {
  { "STRING",     0, TARGET_TYPE_STRING },
  { "text/plain", 0, TARGET_TYPE_STRING },
  { "text/uri-list", 0, TARGET_TYPE_URI_LIST },
  { "_NETSCAPE_URL", 0, TARGET_TYPE_STRING }
};

static guint dnd_n_targets = sizeof(dnd_target_table) / sizeof(dnd_target_table[0]);

#endif

static const struct memory_description gtk_frame_data_description_1 [] = {
  { XD_LISP_OBJECT_ARRAY, offsetof (struct gtk_frame, lisp_visible_widgets),
    3 },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame, menubar_data) },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame, icon_pixmap) },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame, icon_pixmap_mask) },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame, widget_instance_hash_table) },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame, widget_callback_hash_table) },
  { XD_LISP_OBJECT, offsetof (struct gtk_frame,
                              widget_callback_ex_hash_table) },
  { XD_END }
};

#ifdef NEW_GC
DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("gtk-frame", gtk_frame,
				      0, gtk_frame_data_description_1,
				      Lisp_Gtk_Frame);
#else /* not NEW_GC */
extern const struct sized_memory_description gtk_frame_data_description;

const struct sized_memory_description gtk_frame_data_description = {
  sizeof (struct gtk_frame), gtk_frame_data_description_1
};
#endif /* not NEW_GC */


/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* Return the Emacs frame-object which contains the given widget. */
struct frame *
gtk_widget_to_frame (GtkWidget *w)
{
  struct frame *f = NULL;

  for (; w; w = w->parent)
    {
      if ((f = (struct frame *) g_object_get_qdata (G_OBJECT(w),
                                                    GTK_DATA_FRAME_IDENTIFIER)))
	return (f);
    }

  return (selected_frame());
}


/* Return the Emacs frame-object corresponding to an X window */
struct frame *
gtk_window_to_frame (struct device *d, GdkWindow *wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  /* This function was previously written to accept only a window argument
     (and to loop over all devices looking for a matching window), but
     that is incorrect because window ID's are not unique across displays. */

  for (tail = DEVICE_FRAME_LIST (d); CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
	continue;
      f = XFRAME (frame);
      if (FRAME_GTK_P (f) && GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f)) == wdesc)
	return f;
    }
  return 0;
}

/* Like gtk_window_to_frame but also compares the window with the widget's
   windows */
struct frame *
gtk_any_window_to_frame (struct device *d, GdkWindow *w)
{
    do
    {
	Lisp_Object frmcons;

	DEVICE_FRAME_LOOP (frmcons, d)
	    {
		struct frame *fr = XFRAME (XCAR (frmcons));
		if ((w == GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (fr))) ||
		    (w == GET_GTK_WIDGET_WINDOW (FRAME_GTK_CONTAINER_WIDGET (fr))) ||
#ifdef HAVE_MENUBARS
		    (w == GET_GTK_WIDGET_WINDOW (FRAME_GTK_MENUBAR_WIDGET (fr))) ||
#endif
		    (w == GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (fr))))
		{
		    return (fr);
		}
	    }
	w = gdk_window_get_parent (w);
    } while (w);

    return (0);
}

struct frame *
gtk_any_widget_or_parent_to_frame (struct device *d, GtkWidget *widget)
{
    return (gtk_any_window_to_frame (d, GET_GTK_WIDGET_WINDOW (widget)));
}

struct device *
gtk_any_window_to_device (GdkWindow *w)
{
  struct device *d = NULL;
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      d = XDEVICE (XCAR (devcons));
      if (!DEVICE_GTK_P (d)) continue;
      if (gtk_any_window_to_frame (d, w))
        return (d);
    }
  return (NULL);
}

struct frame *
decode_gtk_frame (Lisp_Object frame)
{
  if (NILP (frame))
    frame = wrap_frame (selected_frame ());
  CHECK_LIVE_FRAME (frame);
  /* this will also catch dead frames, but putting in the above check
     results in a more useful error */
  CHECK_GTK_FRAME (frame);
  return XFRAME (frame);
}


/************************************************************************/
/*			window-manager interactions			*/
/************************************************************************/
static int
gtk_frame_iconified_p (struct frame *f)
{
    return (f->iconified);
}


/************************************************************************/
/*                          frame properties                            */
/************************************************************************/

static Lisp_Object
gtk_toolbar_plist (struct frame *f)
{
  Lisp_Object plist = Qnil;

  plist = cons3 (Qright,
                 build_gtk_object ((GObject *) (FRAME_GTK_TOOLBAR_WIDGET (f)
                                                [RIGHT_EDGE])),
                 plist);
  plist = cons3 (Qleft,
                 build_gtk_object ((GObject *) (FRAME_GTK_TOOLBAR_WIDGET (f)
                                                [LEFT_EDGE])),
                 plist);
  plist = cons3 (Qbottom,
                 build_gtk_object ((GObject *) (FRAME_GTK_TOOLBAR_WIDGET (f)
                                                [BOTTOM_EDGE])),
                 plist);
  plist = cons3 (Qtop,
                 build_gtk_object ((GObject *) (FRAME_GTK_TOOLBAR_WIDGET (f)
                                                [TOP_EDGE])),
                 plist);
  return plist;
}

static Lisp_Object
gtk_frame_property (struct frame *f, Lisp_Object property)
{
  GtkWidget *shell = FRAME_GTK_SHELL_WIDGET (f);

  if (EQ (Qleft, property) || EQ (Qtop, property))
    {
      gint x, y;
      if (!GET_GTK_WIDGET_WINDOW(shell))
	return Qzero;
      gdk_window_get_origin (GET_GTK_WIDGET_WINDOW (shell), &x, &y);
      if (EQ (Qleft, property)) return make_fixnum (x);
      if (EQ (Qtop,  property)) return make_fixnum (y);
    }
  if (EQ (Qshell_widget, property))
    {
      return (FRAME_GTK_LISP_WIDGETS (f)[0]);
    }
  if (EQ (Qcontainer_widget, property))
    {
      return (FRAME_GTK_LISP_WIDGETS (f)[1]);
    }
  if (EQ (Qtext_widget, property))
    {
      return (FRAME_GTK_LISP_WIDGETS (f)[2]);
    }
  if (EQ (Qmenubar, property))
    {
      return build_gtk_object (G_OBJECT (FRAME_GTK_MENUBAR_WIDGET (f)));
    }
  if (EQ (Qtoolbar, property))
    {
      return gtk_toolbar_plist (f);
    }
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
  if (EQ (Qwindow_id, property))
    return Fgtk_window_id (wrap_frame (f));
#endif

  return Qunbound;
}

static int
gtk_internal_frame_property_p (struct frame *UNUSED(f), Lisp_Object property)
{
  return EQ (property, Qleft)
    || EQ (property, Qtop)
    || EQ (Qshell_widget, property)
    || EQ (Qcontainer_widget, property)
    || EQ (Qtext_widget, property)
    || EQ (property, Qwindow_id)
    || EQ (property, Qtoolbar)
    || EQ (property, Qmenubar)
    || STRINGP (property);
}

static Lisp_Object
gtk_frame_properties (struct frame *f)
{
  Lisp_Object props = Qnil;
  GtkWidget *shell = FRAME_GTK_SHELL_WIDGET (f);
  gint x, y;

  props = cons3 (Qshell_widget, FRAME_GTK_LISP_WIDGETS (f)[0], props);
  props = cons3 (Qcontainer_widget, FRAME_GTK_LISP_WIDGETS (f)[1], props);
  props = cons3 (Qtext_widget, FRAME_GTK_LISP_WIDGETS (f)[2], props);
  props = cons3 (Qmenubar, build_gtk_object (G_OBJECT (FRAME_GTK_MENUBAR_WIDGET (f))), props);
  props = cons3 (Qtoolbar, gtk_toolbar_plist (f), props);

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
  props = cons3 (Qwindow_id, Fgtk_window_id (wrap_frame (f)), props);
#endif

  if (!GET_GTK_WIDGET_WINDOW (shell))
    x = y = 0;
  else
    gdk_window_get_origin (GET_GTK_WIDGET_WINDOW (shell), &x, &y);

  props = cons3 (Qtop,  make_fixnum (y), props);
  props = cons3 (Qleft, make_fixnum (x), props);

  return props;
}


/* Functions called only from `gtk_set_frame_properties' to set
   individual properties. */

static void
gtk_set_frame_text_value (struct frame *UNUSED (f), Ibyte *value,
			  void (*func) (gpointer, gchar *),
			  gpointer arg)
{
  /* Programmer fuckup or window is not realized yet. */
  if (!func || !arg) return;

  (*func) (arg, ITEXT_TO_EXTERNAL (value, Qutf_8));
}

static void
gtk_set_title_from_ibyte (struct frame *f, Ibyte *name)
{
  if (GTK_IS_WINDOW (FRAME_GTK_SHELL_WIDGET (f)))
    gtk_set_frame_text_value (f, name,
			      (void (*)(gpointer, gchar *))
			      gtk_window_set_title, FRAME_GTK_SHELL_WIDGET (f));
}

static void
gtk_set_icon_name_from_ibyte (struct frame *f, Ibyte *name)
{
  gtk_set_frame_text_value (f, name,
			    (void (*)(gpointer, gchar *))
			    gdk_window_set_icon_name, FRAME_GTK_SHELL_WIDGET (f)->window);
}

/* Set the initial frame size as specified.  This function is used
   when the frame's widgets have not yet been realized.
*/
static void
gtk_set_initial_frame_size (struct frame *f, int x, int y,
			    unsigned int w, unsigned int h)
{
  GtkWidget *shell = FRAME_GTK_SHELL_WIDGET (f);
  GdkGeometry geometry;

  if (GTK_IS_WINDOW (shell))
    {
      GdkWindowHints geometry_mask = GDK_HINT_RESIZE_INC;
      /* Deal with the cell size */
      default_face_width_and_height (wrap_frame (f), &geometry.width_inc, &geometry.height_inc);

      gtk_window_set_geometry_hints (GTK_WINDOW (shell),
				     FRAME_GTK_TEXT_WIDGET (f), &geometry, geometry_mask);
      gdk_window_set_hints (GET_GTK_WIDGET_WINDOW (shell), x, y, 0, 0, 0, 0, GDK_HINT_POS);
      gtk_window_set_resizable (GTK_WINDOW (shell), TRUE);
    }

  FRAME_HEIGHT (f) = h;
  FRAME_WIDTH (f) = w;

  change_frame_size (f, w, h, 0);
  {
    GtkRequisition req;
 
    gtk_widget_size_request (FRAME_GTK_SHELL_WIDGET (f), &req);
  }
}

/* Report that a frame property of frame S is being set or changed.
   If the property is not specially recognized, do nothing.
 */

static void
gtk_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  gint x, y;
  gint width = 0, height = 0;
  gboolean width_specified_p = FALSE;
  gboolean height_specified_p = FALSE;
  gboolean x_position_specified_p = FALSE;
  gboolean y_position_specified_p = FALSE;
  Lisp_Object tail;

  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      Lisp_Object prop = Fcar (tail);
      Lisp_Object val = Fcar (Fcdr (tail));

      if (SYMBOLP (prop))
	{
	  if (EQ (prop, Qfont))
	  {
	      /* If the value is not a string we silently ignore it. */
	      if (STRINGP (val))
	      {
		  Lisp_Object frm, font_spec;

		  frm = wrap_frame (f);
		  font_spec = Fget (Fget_face (Qdefault), Qfont, Qnil);

		  Fadd_spec_to_specifier (font_spec, val, frm, Qnil, Qnil);
		  update_frame_face_values (f);
	      }
	      continue;
	  }
	  else if (EQ (prop, Qwidth))
	  {
	      CHECK_FIXNUM (val);
	      width = XFIXNUM (val);
	      width_specified_p = TRUE;
	      continue;
	  }
	  else if (EQ (prop, Qheight))
	  {
	      CHECK_FIXNUM (val);
	      height = XFIXNUM (val);
	      height_specified_p = TRUE;
	      continue;
	  }
	  /* Further kludge the x/y. */
	  else if (EQ (prop, Qleft) || EQ (prop, Qx))
	  {
	      CHECK_FIXNUM (val);
	      x = (gint) XFIXNUM (val);
	      x_position_specified_p = TRUE;
	      continue;
	  }
	  else if (EQ (prop, Qtop) || EQ (prop, Qy))
	  {
	      CHECK_FIXNUM (val);
	      y = (gint) XFIXNUM (val);
	      y_position_specified_p = TRUE;
	      continue;
	  }
	}
    }

  /* Kludge kludge kludge.   We need to deal with the size and position
   specially. */
  {
    int size_specified_p = width_specified_p || height_specified_p;
    int position_specified_p = x_position_specified_p || y_position_specified_p;

    if (!width_specified_p)
      width = 80;
    if (!height_specified_p)
      height = 40;

    /* Kludge kludge kludge kludge. */
    if (position_specified_p &&
	(!x_position_specified_p || !y_position_specified_p))
      {
	gint dummy;
	GtkWidget *shell = FRAME_GTK_SHELL_WIDGET (f);
	gdk_window_get_origin (GET_GTK_WIDGET_WINDOW (shell),
                               (x_position_specified_p ? &dummy : &x),
                               (y_position_specified_p ? &dummy : &y));
      }

    if (!f->init_finished)
      {
	if (size_specified_p || position_specified_p)
	  gtk_set_initial_frame_size (f, x, y, width, height);
      }
    else
      {
	if (size_specified_p)
	  {
	    Lisp_Object frame = wrap_frame (f);

	    Fset_frame_size (frame, make_fixnum (width), make_fixnum (height), Qnil);
	  }
	if (position_specified_p)
	  {
	    Lisp_Object frame = wrap_frame (f);

	    Fset_frame_position (frame, make_fixnum (x), make_fixnum (y));
	  }
      }
  }
}


/************************************************************************/
/*				widget creation				*/
/************************************************************************/
/* Figure out what size the shell widget should initially be,
   and set it.  Should be called after the default font has been
   determined but before the widget has been realized. */

extern Lisp_Object Vgtk_initial_geometry;

static int
get_number (const char **geometry)
{
  int value = 0;
  int mult  = 1;
	
  if (**geometry == '-'){
    mult = -1;
    (*geometry)++;
  }
  while (**geometry && isdigit (**geometry)){
    value = value * 10 + (**geometry - '0');
    (*geometry)++;
  }
  return value * mult;
}

/*
 */

/**
 * gnome_parse_geometry
 * @geometry: geometry string to be parsed
 * @xpos: X position geometry component
 * @ypos: Y position geometry component
 * @width: pixel width geometry component
 * @height: pixel height geometry component
 *
 * Description:
 * Parses the geometry string passed in @geometry, and fills
 * @xpos, @ypos, @width, and @height with
 * the corresponding values upon completion of the parse.
 * If the parse fails, it should be assumed that @xpos, @ypos, @width,
 * and @height contain undefined values.
 *
 * Returns:
 * %TRUE if the geometry was successfully parsed, %FALSE otherwise.
 **/

static gboolean
gnome_parse_geometry (const gchar *geometry, gint *xpos, 
		      gint *ypos, gint *width, gint *height)
{
  int subtract;

  g_return_val_if_fail (xpos != NULL, FALSE);
  g_return_val_if_fail (ypos != NULL, FALSE);
  g_return_val_if_fail (width != NULL, FALSE);
  g_return_val_if_fail (height != NULL, FALSE);
	
  *xpos = *ypos = *width = *height = -1;

  if (!geometry)
    return FALSE;

  if (*geometry == '=')
    geometry++;
  if (!*geometry)
    return FALSE;
  if (isdigit (*geometry))
    *width = get_number (&geometry);
  if (!*geometry)
    return TRUE;
  if (*geometry == 'x' || *geometry == 'X'){
    geometry++;
    *height = get_number (&geometry);
  }
  if (!*geometry)
    return 1;
  if (*geometry == '+'){
    subtract = 0;
    geometry++;
  } else if (*geometry == '-'){
    subtract = gdk_screen_width ();
    geometry++;
  } else
    return FALSE;
  *xpos = get_number (&geometry);
  if (subtract)
    *xpos = subtract - *xpos;
  if (!*geometry)
    return TRUE;
  if (*geometry == '+'){
    subtract = 0;
    geometry++;
  } else if (*geometry == '-'){
    subtract = gdk_screen_height ();
    geometry++;
  } else
    return FALSE;
  *ypos = get_number (&geometry);
  if (subtract)
    *ypos = subtract - *ypos;
  return TRUE;
}

static void
gtk_initialize_frame_size (struct frame *f)
{
  gint x = 10, y = 10, w = 80, h = 40;

  if (STRINGP (Vgtk_initial_geometry))
    {
      if (!gnome_parse_geometry (LISP_STRING_TO_EXTERNAL (Vgtk_initial_geometry,
                                                          Qutf_8),
                                 &x,&y,&w,&h))
	{
	  x = y = 10;
	  w = 80;
	  h = 40;
	}
    }

  /* set the position of the frame's root window now.  When the
     frame was created, the position was initialized to (0,0). */
  {
    struct window *win = XWINDOW (f->root_window);

    WINDOW_LEFT (win) = FRAME_PANED_LEFT_EDGE (f);
    WINDOW_TOP (win) = FRAME_PANED_TOP_EDGE (f);

    if (!NILP (f->minibuffer_window))
      {
	win = XWINDOW (f->minibuffer_window);
	WINDOW_LEFT (win) = FRAME_PANED_LEFT_EDGE (f);
      }
  }

  gtk_set_initial_frame_size (f, x, y, w, h);
}

static gboolean
size_allocate_cb (GtkWidget *w, GtkAllocation *allocation,
		 gpointer user_data)
{
  struct frame *f = (struct frame *) user_data;

  if (!GTK_IS_FIXED (w))
    return TRUE;

  f->pixwidth = allocation->width;
  f->pixheight = allocation->height;

  debug_out ("frame size allocation  %d %d\n", allocation->width,
             allocation->height);

/*   if (FRAME_GTK_TEXT_WIDGET (f)->window) */
/*     { */
/*       Lisp_Object frame = wrap_frame (f); */

/*       Fredraw_frame (frame, Qt); */
/*     } */

  return (FALSE);
}

static gboolean
size_request_cb (GtkWidget *w, GtkRequisition *req,
		 gpointer user_data)
{
  struct frame *f = (struct frame *) user_data;

  if (!GTK_IS_FIXED (w))
    return TRUE;

  debug_out ("frame size request  %d %d\n", req->width, req->height);

  // FRAME_PIXWIDTH (f)  = allocation->width;
  // FRAME_PIXHEIGHT (f)  = allocation->height;
  req->width  = FRAME_PIXWIDTH (f);
  req->height = FRAME_PIXHEIGHT (f);

  debug_out ("frame size request  %d %d\n", req->width, req->height);

  /*   if (FRAME_GTK_TEXT_WIDGET (f)->window) */
  /*     { */
  /*       Lisp_Object frame = wrap_frame (f); */

  /*       Fredraw_frame (frame, Qt); */
  /*     } */

  return (FALSE);
}

static gboolean
delete_event_cb (GtkWidget *UNUSED (w), GdkEvent *UNUSED (ev),
		 gpointer user_data)
{
    struct frame *f = (struct frame *) user_data;
    Lisp_Object frame = wrap_frame (f);

    enqueue_misc_user_event (frame, Qeval, list3 (Qdelete_frame, frame, Qt));

    /* See if tickling the event queue helps us with our delays when
       clicking 'close' */
    signal_fake_event ();

    return (TRUE);
}

extern gboolean emacs_shell_event_handler (GtkWidget *wid, GdkEvent *event, gpointer closure);
extern Lisp_Object build_gtk_object (GObject *obj);

#ifndef GNOME_IS_APP
#define GNOME_IS_APP(x) 0
#define gnome_app_set_contents(x,y) 0
#endif

static void
cleanup_deleted_frame (gpointer data, GObject *UNUSED(old))
{
  struct frame *f = (struct frame *) data;
  Lisp_Object frame = wrap_frame (f);

  Fdelete_frame (frame, Qt);
}

#ifdef HAVE_DRAGNDROP
extern void
dragndrop_data_received (GtkWidget          *widget,
			 GdkDragContext     *context,
			 gint                x,
			 gint                y,
			 GtkSelectionData   *data,
			 guint               info,
			 guint               time);

extern gboolean
dragndrop_dropped (GtkWidget *widget,
		   GdkDragContext *drag_context,
		   gint x,
		   gint y,
		   guint time,
		   gpointer user_data);

Lisp_Object Vcurrent_drag_object;

#define DRAG_SELECTION_DATA_ERROR "Error converting drag data to external format"
static void
dragndrop_get_drag (GtkWidget *UNUSED (widget),
		    GdkDragContext *UNUSED (drag_context),
		    GtkSelectionData *data,
		    guint info,
		    guint UNUSED (time),
		    gpointer UNUSED (user_data))
{
  gtk_selection_data_set (data, GDK_SELECTION_TYPE_STRING, 8,
			  (guchar *) DRAG_SELECTION_DATA_ERROR,
			  sizeof (DRAG_SELECTION_DATA_ERROR) - 1);

  switch (info)
    {
    case TARGET_TYPE_STRING:
      {
	Lisp_Object string = Vcurrent_drag_object;
        guchar *extstring;

	if (!STRINGP (Vcurrent_drag_object))
	  {
	    string = Fprin1_to_string (string, Qnil);
	    /* Convert to a string */
	  }

        extstring = LISP_STRING_TO_EXTERNAL (string, Qutf_8);
	
	gtk_selection_data_set (data, GDK_SELECTION_TYPE_STRING,
				8, extstring, strlen (extstring));
      }
      break;
    case TARGET_TYPE_URI_LIST:
      break;
    default:
      break;
    }
  Vcurrent_drag_object = Qnil;
}

DEFUN ("gtk-start-drag-internal", Fgtk_start_drag_internal, 2, 3, 0, /*
Start a GTK drag from a buffer.
First arg is the event that started the drag,
second arg should be some string, and the third
is the type of the data (this should be a MIME type as a string (ie: text/plain)).
The type defaults to text/plain.
*/
       (event, data, dtyp))
{
  if (EVENTP(event))
    {
      struct frame *f = decode_gtk_frame (Fselected_frame (Qnil));
      GtkWidget *wid = FRAME_GTK_TEXT_WIDGET (f);
      struct Lisp_Event *lisp_event = XEVENT(event);
      GdkAtom dnd_typ;
      GtkTargetList *tl = gtk_target_list_new (dnd_target_table, dnd_n_targets);

      /* only drag if this is really a press */
      if (EVENT_TYPE(lisp_event) != button_press_event)
	return Qnil;

      /* get the desired type */
      if (!NILP (dtyp) && STRINGP (dtyp))
	dnd_typ = gdk_atom_intern (LISP_STRING_TO_EXTERNAL (dtyp, Qutf_8),
                                   FALSE);

      gtk_drag_begin (wid, tl, GDK_ACTION_COPY,
		      EVENT_BUTTON_BUTTON (lisp_event), NULL);

      Vcurrent_drag_object = data;

      gtk_target_list_unref (tl);
    }
  return Qnil;
}
#endif

/* Creates the widgets for a frame.
   lisp_window_id is a Lisp description of an X window or Xt
   widget to parse.

   This function does not map the windows.  (That is
   done by gtk_popup_frame().)
*/
static void
gtk_create_widgets (struct frame *f, Lisp_Object lisp_window_id, Lisp_Object parent)
{
  const char *name;
  GtkWidget *text, *container, *shell;
  gboolean embedded_p = !NILP (lisp_window_id);
#ifdef HAVE_MENUBARS
  int menubar_visible;
#endif

  if (STRINGP (f->name))
    name = LISP_STRING_TO_EXTERNAL (f->name, Qutf_8);
  else
    name = "emacs";

  FRAME_GTK_TOP_LEVEL_FRAME_P (f) = 1;

  if (embedded_p)
    {
      CHECK_GTK_OBJECT (lisp_window_id);

      if (!GTK_IS_CONTAINER (XGTK_OBJECT (lisp_window_id)->object))
	{
	  invalid_argument ("Window ID must be a GtkContainer subclass", lisp_window_id);
	}

      shell = gtk_vbox_new (FALSE, 0);

      g_object_weak_ref (G_OBJECT (shell), cleanup_deleted_frame, f);
      gtk_container_add (GTK_CONTAINER (XGTK_OBJECT (lisp_window_id)->object), shell);
    }
  else
    {
      shell = GTK_WIDGET (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    }

  if (!NILP (parent))
  {
      /* If this is a transient window, keep the parent info around */
      GtkWidget *parentwid = FRAME_GTK_SHELL_WIDGET (XFRAME (parent));
      g_object_set_data (G_OBJECT (shell), TRANSIENT_DATA_IDENTIFIER, parentwid);
      gtk_window_set_transient_for (GTK_WINDOW (shell), GTK_WINDOW (parentwid));
  }

  gtk_container_set_border_width (GTK_CONTAINER (shell), 0);

  /* Add a mapping from widget to frame to help widget callbacks quickly find
     their corresponding frame. */
  g_object_set_qdata (G_OBJECT (G_OBJECT (shell)), GTK_DATA_FRAME_IDENTIFIER, f);

  FRAME_GTK_SHELL_WIDGET (f) = shell;
  gtk_widget_set_name (shell, name);

  text = GTK_WIDGET (gtk_xemacs_new (f));

  if (!GNOME_IS_APP (shell))
    container = GTK_WIDGET (gtk_vbox_new (FALSE, INTERNAL_BORDER_WIDTH));
  else
    container = shell;

  FRAME_GTK_CONTAINER_WIDGET (f) = container;
  FRAME_GTK_TEXT_WIDGET (f) = text;

#ifdef HAVE_DRAGNDROP
  gtk_drag_dest_set (text, GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT,
		     dnd_target_table, dnd_n_targets,
		     GDK_ACTION_COPY | GDK_ACTION_LINK | GDK_ACTION_ASK);
  assert (g_signal_connect (G_OBJECT (text), "drag_drop",
                            G_CALLBACK (dragndrop_dropped), text));
  assert (g_signal_connect (G_OBJECT (text), "drag_data_received",
                            G_CALLBACK (dragndrop_data_received), text));
  assert (g_signal_connect (G_OBJECT (text), "drag_data_get",
                            G_CALLBACK (dragndrop_get_drag), NULL));
#endif

#ifdef HAVE_MENUBARS
  /* Create the initial menubar widget. */
  menubar_visible = gtk_initialize_frame_menubar (f);

  if (menubar_visible)
    {
      gtk_widget_show_all (FRAME_GTK_MENUBAR_WIDGET (f));
    }
#endif /* HAVE_MENUBARS */

  if (GNOME_IS_APP (shell))
    gnome_app_set_contents (GNOME_APP (shell), text);
  else
    /* Now comes the drawing area, which should fill the rest of the
    ** frame completely.
    */
    if (GTK_IS_BOX (container)) 
      gtk_box_pack_end (GTK_BOX (container), text, TRUE, TRUE, 0);

  /* Connect main event handler */
  assert (g_signal_connect (G_OBJECT (shell), "delete-event",
                            G_CALLBACK (delete_event_cb), f));

  {
    static const gchar *events_to_frob[] =
      {
	"focus-in-event",
	"focus-out-event",
	"enter-notify-event",
	"leave-notify-event",
	"map-event",
	"unmap-event",
	"property-notify-event",
	"selection-clear-event",
	"selection-request-event",
	"selection-notify-event",
	"client-event",
	/* "configure-event", */
	"visibility-notify-event",
	"scroll-event",
	NULL
      };
    int i;

    for (i = 0; events_to_frob[i]; i++)
      {
	assert (g_signal_connect (G_OBJECT (shell), events_to_frob[i],
                                  G_CALLBACK (emacs_shell_event_handler), f));
      }
  }

  assert (g_signal_connect (G_OBJECT (shell), "size-allocate",
                            G_CALLBACK (size_allocate_cb), f));
  assert (g_signal_connect (G_OBJECT (shell), "size-request",
                            G_CALLBACK (size_request_cb), f));

  /* This might be safe to call now... */
  /* gtk_signal_connect (GTK_OBJECT (shell), "event", G_CALLBACK (emacs_shell_event_handler), f); */

  /* Let's make sure we get all the events we can */
  gtk_widget_add_events (shell, GDK_ALL_EVENTS_MASK);

  if (shell != container)
    gtk_container_add (GTK_CONTAINER (shell), container);

  gtk_widget_set_name (shell, "XEmacs::shell");
  gtk_widget_set_name (container, "XEmacs::container");
  gtk_widget_set_name (text, "XEmacs::text");

  FRAME_GTK_LISP_WIDGETS(f)[0] = build_gtk_object (G_OBJECT (shell));
  FRAME_GTK_LISP_WIDGETS(f)[1] = build_gtk_object (G_OBJECT (container));
  FRAME_GTK_LISP_WIDGETS(f)[2] = build_gtk_object (G_OBJECT (text));

  gtk_widget_realize (shell);
}

/* create the windows for the specified frame and display them.
   Note that the widgets have already been created, and any
   necessary geometry calculations have already been done. */
static void
gtk_popup_frame (struct frame *f)
{
  /* */
  if (g_object_get_data (G_OBJECT (FRAME_GTK_SHELL_WIDGET (f)),
                         UNMAPPED_DATA_IDENTIFIER))
    {
      FRAME_GTK_TOTALLY_VISIBLE_P (f) = 0;
      FRAME_VISIBLE_P (f) = 0;
      gtk_widget_realize (FRAME_GTK_SHELL_WIDGET (f));
      gtk_widget_realize (FRAME_GTK_TEXT_WIDGET (f));
      gtk_widget_hide_all (FRAME_GTK_SHELL_WIDGET (f));
    }
  else
    {
      gtk_widget_show_all (FRAME_GTK_SHELL_WIDGET (f));
    }
}

static void
allocate_gtk_frame_struct (struct frame *f)
{
  int i;

  /* zero out all slots. */
#ifdef NEW_GC
  f->frame_data = XGTK_FRAME (ALLOC_NORMAL_LISP_OBJECT (gtk_frame));
#else /* not NEW_GC */
  f->frame_data = xnew_and_zero (struct gtk_frame);
#endif /* not NEW_GC */

  /* yeah, except the lisp ones */
  FRAME_GTK_ICON_PIXMAP (f) = Qnil;
  FRAME_GTK_ICON_PIXMAP_MASK (f) = Qnil;
  FRAME_GTK_MENUBAR_DATA (f) = Qnil;
  for (i = 0; i < 3; i++)
    FRAME_GTK_LISP_WIDGETS (f)[i] = Qnil;

  /*
    Hashtables of callback data for glyphs on the frame.  [[ Make them EQ
    because we only use ints as keys.  Otherwise we run into stickiness in
    redisplay because internal_equal() can QUIT.  See
    enter_redisplay_critical_section() ]] -- probably not true any more,
    now that we have internal_equal_trapping_problems(). --ben
*/
  FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, Qeq);
  FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, Qeq);
  FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, Qeq);
}


/************************************************************************/
/*				Lisp functions				*/
/************************************************************************/

static void
gtk_init_frame_1 (struct frame *f, Lisp_Object props,
		  int UNUSED (frame_name_is_defaulted))
{
  /* This function can GC */
  Lisp_Object initially_unmapped;
  Lisp_Object device = FRAME_DEVICE (f);
  Lisp_Object lisp_window_id = Fplist_get (props, Qwindow_id, Qnil);
  Lisp_Object popup = Fplist_get (props, Qpopup, Qnil);

  if (!NILP (popup))
    {
      if (EQ (popup, Qt))
	popup = Fselected_frame (device);
      CHECK_LIVE_FRAME (popup);
      if (!EQ (device, FRAME_DEVICE (XFRAME (popup))))
	invalid_argument_2 ("Parent must be on same device as frame",
			    device, popup);
    }

  initially_unmapped = Fplist_get (props, Qinitially_unmapped, Qnil);

  /*
   * Previously we set this only if NILP (DEVICE_SELECTED_FRAME (d))
   * to make sure that messages were displayed as soon as possible
   * if we're creating the first frame on a device.  But it is
   * better to just set this all the time, so that when a new frame
   * is created that covers the selected frame, echo area status
   * messages can still be seen.  f->visible is reset later if the
   * initially-unmapped property is found to be non-nil in the
   * frame properties.
   */
  f->visible = 1;

  allocate_gtk_frame_struct (f);
  gtk_create_widgets (f, lisp_window_id, popup);

  if (!NILP (initially_unmapped))
    {
      g_object_set_data (G_OBJECT (FRAME_GTK_SHELL_WIDGET (f)),
			   UNMAPPED_DATA_IDENTIFIER, GUINT_TO_POINTER (1));
    }
}

static void
gtk_init_frame_2 (struct frame *f, Lisp_Object UNUSED (props))
{
  /* Set up the values of the widget/frame.  A case could be made for putting
     this inside of the widget's initialize method. */

  update_frame_face_values (f);
  gtk_initialize_frame_size (f);
  /* Kyle:
   *   update_frame_title() can't be done here, because some of the
   *   modeline specs depend on the frame's device having a selected
   *   frame, and that may not have been set up yet.  The redisplay
   *   will update the frame title anyway, so nothing is lost.
   * JV:
   *   It turns out it gives problems with FVWMs name based mapping.
   *   We'll just need to be careful in the modeline specs.
   */
  update_frame_title (f); 
}

static void
gtk_init_frame_3 (struct frame *f)
{
  /* Pop up the frame. */
  gtk_popup_frame (f);
}

static void
gtk_mark_frame (struct frame *f)
{
  mark_object (FRAME_GTK_LISP_WIDGETS (f)[0]);
  mark_object (FRAME_GTK_LISP_WIDGETS (f)[1]);
  mark_object (FRAME_GTK_LISP_WIDGETS (f)[2]);
  mark_object (FRAME_GTK_MENUBAR_DATA (f));
  mark_object (FRAME_GTK_ICON_PIXMAP (f));
  mark_object (FRAME_GTK_ICON_PIXMAP_MASK (f));
  mark_object (FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE (f));
  mark_object (FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE (f));
  mark_object (FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE (f));
}

static void
gtk_set_frame_icon (struct frame *f)
{
  GdkPixbuf *gtk_pixbuf = NULL;
  GList *icons = NULL;

  if (IMAGE_INSTANCEP (f->icon)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (f->icon)))
    {
      gtk_pixbuf = XIMAGE_INSTANCE_GTK_PIXMAP (f->icon);
      icons = g_list_append(icons, gtk_pixbuf);
      gdk_window_set_icon_list
	(GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f)), icons);
      g_list_free (icons);
    }
}

static void
gtk_set_frame_pointer (struct frame *f)
{
  GtkWidget *w = FRAME_GTK_TEXT_WIDGET (f);
  GdkCursor *c = XIMAGE_INSTANCE_GTK_CURSOR (f->pointer);

  if (POINTER_IMAGE_INSTANCEP (f->pointer))
    {
      gdk_window_set_cursor (GET_GTK_WIDGET_WINDOW (w), c);
      gdk_flush ();
    }
  else
    {
      /* ABORT()? */
      stderr_out ("POINTER_IMAGE_INSTANCEP (f->pointer) failed!\n");
    }
}

static Lisp_Object
gtk_get_frame_parent (struct frame *f)
{
  GtkWidget *parentwid = (GtkWidget*)
    g_object_get_data (G_OBJECT (FRAME_GTK_SHELL_WIDGET (f)),
                       TRANSIENT_DATA_IDENTIFIER);

    /* find the frame whose wid is parentwid */
    if (parentwid)
    {
	Lisp_Object frmcons;
	DEVICE_FRAME_LOOP (frmcons, XDEVICE (FRAME_DEVICE (f)))
	    {
		Lisp_Object frame = XCAR (frmcons);
		if (FRAME_GTK_SHELL_WIDGET (XFRAME (frame)) == parentwid)
		    return frame;
	    }
    }
    return Qnil;
}

#ifdef STUPID_X_SPECIFIC_GTK_STUFF
DEFUN ("gtk-window-id", Fgtk_window_id, 0, 1, 0, /*
Get the ID of the Gtk window.
This gives us a chance to manipulate the Emacs window from within a
different program.  Since the ID is an unsigned long, we return it as
a string.
*/
       (frame))
{
  Ascbyte str[255];
  struct frame *f = decode_gtk_frame (frame);

  /* Arrrrggghhh... this defeats the whole purpose of using Gdk... do we really need this? */
  sprintf (str, "%lu", GDK_WINDOW_XWINDOW( GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f))));
  return build_ascstring (str);
}
#endif


/************************************************************************/
/*			manipulating the X window			*/
/************************************************************************/

static void
gtk_set_frame_position (struct frame *f, int xoff, int yoff)
{
  /* Deprecated, what is the replacement? */
    gtk_widget_set_uposition (FRAME_GTK_SHELL_WIDGET (f), xoff, yoff);
}

/* Call this to change the size of frame S's x-window. */

static void
gtk_set_frame_size (struct frame *f, int cols, int rows)
{
  GtkWidget *shell = FRAME_GTK_SHELL_WIDGET (f);
  GdkGeometry geometry;

  if (GTK_IS_WINDOW (shell))
    {
      GdkWindowHints geometry_mask = GDK_HINT_RESIZE_INC;

      /* Update the cell size */
      default_face_width_and_height (wrap_frame (f), &geometry.width_inc,
				     &geometry.height_inc);

      gtk_window_set_geometry_hints (GTK_WINDOW (shell),
				     FRAME_GTK_TEXT_WIDGET (f), &geometry, geometry_mask);
    }

  change_frame_size (f, cols, rows, 0);

  {
    GtkRequisition req;

    gtk_widget_size_request (FRAME_GTK_SHELL_WIDGET (f), &req);
    gtk_widget_set_usize (FRAME_GTK_SHELL_WIDGET (f), req.width, req.height);
  }
}

static void
gtk_set_mouse_position (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  GdkDisplay *display = gtk_widget_get_display (FRAME_GTK_TEXT_WIDGET (f));
  GdkScreen *screen = gtk_widget_get_screen (FRAME_GTK_TEXT_WIDGET (f));
  gdk_display_warp_pointer (display, screen, 
                            w->pixel_left + x, w->pixel_top + y);
}

static int
gtk_get_mouse_position (struct device *d, Lisp_Object *frame, int *x, int *y)
{
    /* Returns the pixel position within the editor text widget */
    gint win_x, win_y;
    GdkWindow *w = gdk_window_at_pointer (&win_x, &win_y);
    struct frame *f = NULL;

    if (!w) return (0);

    /* At this point, w is the innermost GdkWindow containing the
    ** pointer and win_x and win_y are the coordinates of that window.
    */
    f = gtk_any_window_to_frame (d, w);

    if (!f) return (0);

    *frame = wrap_frame (f);

    gdk_window_get_pointer (GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f)),
			    &win_x, &win_y, NULL);

    *x = win_x;
    *y = win_y;

    return (1);
}

static DECLARE_DOESNT_RETURN (gtk_cant_notify_wm_error (void));

static DOESNT_RETURN
gtk_cant_notify_wm_error ()
{
  signal_error (Qgui_error, "Can't notify window manager of iconification", Qunbound);
}

/* Raise frame F.  */
static void
gtk_raise_frame_1 (struct frame *f, int force)
{
  if (FRAME_VISIBLE_P (f) || force)
    {
      GdkWindow *emacs_window = GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f));

      gdk_window_raise (emacs_window);
    }
}

static void
gtk_raise_frame (struct frame *f)
{
  gtk_raise_frame_1 (f, 1);
}

/* Lower frame F.  */
static void
gtk_lower_frame (struct frame *f)
{
  if (FRAME_VISIBLE_P (f))
    {
	gdk_window_lower (GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f)));
    }
}

/* Change from withdrawn state to mapped state. */
static void
gtk_make_frame_visible (struct frame *f)
{
    gtk_widget_map (FRAME_GTK_SHELL_WIDGET (f));
    gtk_raise_frame_1 (f, 0);
}

/* Change from mapped state to withdrawn state. */
static void
gtk_make_frame_invisible (struct frame *f)
{
    gtk_widget_unmap(FRAME_GTK_SHELL_WIDGET (f));
}

static int
gtk_frame_visible_p (struct frame *f)
{
    GtkWidget *w = FRAME_GTK_SHELL_WIDGET (f);

    FRAME_VISIBLE_P (f) = gtk_widget_get_visible (w);

    return FRAME_VISIBLE_P (f);
}

static int
gtk_frame_totally_visible_p (struct frame *f)
{
  return FRAME_GTK_TOTALLY_VISIBLE_P (f);
}

/* Change window state from mapped to iconified. */
static void
gtk_iconify_frame (struct frame *f)
{
  GdkWindow *w = GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f));

  /* There is no equivalent to XIconifyWindow in Gtk/Gdk. */
  if (!XIconifyWindow (GDK_WINDOW_XDISPLAY (w),
		       GDK_WINDOW_XWINDOW (w),
		       DefaultScreen (GDK_WINDOW_XDISPLAY (w))))
    gtk_cant_notify_wm_error ();

  f->iconified = 1;
}

/* Sets the X focus to frame f. */
static void
gtk_focus_on_frame (struct frame *f)
{
  GtkWidget *shell_widget;

  assert (FRAME_GTK_P (f));

  shell_widget = FRAME_GTK_SHELL_WIDGET (f);
  if (!GET_GTK_WIDGET_WINDOW (shell_widget))
    return;

  gtk_widget_grab_focus (shell_widget);
}

/* Destroy the window of frame S.  */
static void
gtk_delete_frame (struct frame *f)
{
    GtkWidget *w = FRAME_GTK_SHELL_WIDGET (f);

    gtk_widget_destroy (w);

    if (FRAME_GTK_GEOM_FREE_ME_PLEASE (f))
	xfree (FRAME_GTK_GEOM_FREE_ME_PLEASE (f));
#ifndef NEW_GC
    xfree (f->frame_data);
#endif /* not NEW_GC */
    f->frame_data = 0;
}

static void
gtk_recompute_cell_sizes (struct frame *frm)
{
  if (GTK_IS_WINDOW (FRAME_GTK_SHELL_WIDGET (frm)))
    {
      GtkWindow *w = GTK_WINDOW (FRAME_GTK_SHELL_WIDGET (frm));
      GdkGeometry geometry;
      GdkWindowHints geometry_mask;
      gint width_inc = 10;
      gint height_inc = 10;

      default_face_width_and_height (wrap_frame (frm), &width_inc, &height_inc);
      geometry_mask = GDK_HINT_RESIZE_INC;
      geometry.width_inc = width_inc;
      geometry.height_inc = height_inc;

      gtk_window_set_geometry_hints (w, FRAME_GTK_TEXT_WIDGET (frm), &geometry, geometry_mask);
    }
}

static void
gtk_update_frame_external_traits (struct frame* frm, Lisp_Object name)
{
  Lisp_Object frame = Qnil;

  frame = wrap_frame (frm);

  if (EQ (name, Qforeground))
   {
     Lisp_Object color = FACE_FOREGROUND (Vdefault_face, frame);

     if (!EQ (color, Vthe_null_color_instance))
       {
	 /*
	   GdkColor *fgc = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (color));
	 */
	 /* #### BILL!!! The X code set the XtNforeground property of
	    the text widget here.  Why did they bother?  All that type
	    of thing is done down in the guts of the redisplay code,
	    not in the Emacs* widgets. */
       }
   }
  else if (EQ (name, Qbackground))
   {
     Lisp_Object color = FACE_BACKGROUND (Vdefault_face, frame);
     GdkColor *bgc;

     if (!EQ (color, Vthe_null_color_instance))
       {
	 bgc = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (color));
	 if (FRAME_GTK_SHELL_WIDGET (frm)->window)
	   {
	     gdk_window_set_background (FRAME_GTK_SHELL_WIDGET (frm)->window, bgc);
	   }
	 if (FRAME_GTK_TEXT_WIDGET (frm)->window)
	   {
	     gdk_window_set_background (FRAME_GTK_TEXT_WIDGET (frm)->window, bgc);
	   }
       }

     /* Really crappy way to force the modeline shadows to be
	redrawn.  But effective. */
     MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (frm);
     MARK_FRAME_CHANGED (frm);
   }
  else if (EQ (name, Qfont))
   {
     Lisp_Object font = FACE_FONT (Vdefault_face, frame, Vcharset_ascii);

     /* It may be that instantiating the font has deleted the frame (will
	happen if the user has specified a charset registry for ASCII that
	isn't available on the server, and our fallback of iso8859-1 isn't
	available; something vanishingly rare.) In that case, return from
	this function. */

     if (!FRAME_LIVE_P(frm))
       {
	 return;
       }

     if (!EQ (font, Vthe_null_font_instance))
     {
	 /* #### BILL!!! The X code set the XtNfont property of the
	    text widget here.  Why did they bother?  All that type of
	    thing is done down in the guts of the redisplay code, not
	    in the Emacs* widgets. */
     }
   }
  else
   ABORT ();

#ifdef HAVE_TOOLBARS
  /* Setting the background clears the entire frame area
    including the toolbar so we force an immediate redraw of
    it. */
  if (EQ (name, Qbackground))
    MAYBE_DEVMETH (XDEVICE (frm->device), redraw_frame_toolbars, (frm));
#endif /* HAVE_TOOLBARS */

  /* Set window manager resize increment hints according to
     the new character size */
  if (EQ (name, Qfont) && FRAME_GTK_TOP_LEVEL_FRAME_P (frm))
	  gtk_recompute_cell_sizes (frm);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_frame_gtk (void)
{
#ifdef NEW_GC
  INIT_LISP_OBJECT (gtk_frame);
#endif /* NEW_GC */

  DEFSYMBOL (Qtext_widget);
  DEFSYMBOL (Qcontainer_widget);
  DEFSYMBOL (Qshell_widget);
  DEFSYMBOL (Qdetachable_menubar);

#ifdef HAVE_DRAGNDROP
  staticpro (&Vcurrent_drag_object);
  Vcurrent_drag_object = Qnil;
  DEFSUBR (Fgtk_start_drag_internal);
#endif
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
  DEFSUBR (Fgtk_window_id);
#endif
}

void
console_type_create_frame_gtk (void)
{
  /* frame methods */
  CONSOLE_HAS_METHOD (gtk, init_frame_1);
  CONSOLE_HAS_METHOD (gtk, init_frame_2);
  CONSOLE_HAS_METHOD (gtk, init_frame_3);
  CONSOLE_HAS_METHOD (gtk, mark_frame);
  CONSOLE_HAS_METHOD (gtk, focus_on_frame);
  CONSOLE_HAS_METHOD (gtk, delete_frame);
  CONSOLE_HAS_METHOD (gtk, get_mouse_position);
#ifdef STUPID_X_SPECIFIC_GTK_STUFF
  CONSOLE_HAS_METHOD (gtk, set_mouse_position);
#endif
  CONSOLE_HAS_METHOD (gtk, raise_frame);
  CONSOLE_HAS_METHOD (gtk, lower_frame);
  CONSOLE_HAS_METHOD (gtk, make_frame_visible);
  CONSOLE_HAS_METHOD (gtk, make_frame_invisible);
  CONSOLE_HAS_METHOD (gtk, iconify_frame);
  CONSOLE_HAS_METHOD (gtk, set_frame_size);
  CONSOLE_HAS_METHOD (gtk, set_frame_position);
  CONSOLE_HAS_METHOD (gtk, frame_property);
  CONSOLE_HAS_METHOD (gtk, internal_frame_property_p);
  CONSOLE_HAS_METHOD (gtk, frame_properties);
  CONSOLE_HAS_METHOD (gtk, set_frame_properties);
  CONSOLE_HAS_METHOD (gtk, set_title_from_ibyte);
  CONSOLE_HAS_METHOD (gtk, set_icon_name_from_ibyte);
  CONSOLE_HAS_METHOD (gtk, frame_visible_p);
  CONSOLE_HAS_METHOD (gtk, frame_totally_visible_p);
  CONSOLE_HAS_METHOD (gtk, frame_iconified_p);
  CONSOLE_HAS_METHOD (gtk, set_frame_pointer);
  CONSOLE_HAS_METHOD (gtk, set_frame_icon);
  CONSOLE_HAS_METHOD (gtk, get_frame_parent);
  CONSOLE_HAS_METHOD (gtk, update_frame_external_traits);
}

void
vars_of_frame_gtk (void)
{
  DEFVAR_LISP ("default-gtk-frame-plist", &Vdefault_gtk_frame_plist /*
Plist of default frame-creation properties for Gtk frames.
These override what is specified in the resource database and in
`default-frame-plist', but are overridden by the arguments to the
particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

  initially-unmapped		If non-nil, the frame will not be visible
				when it is created.  In this case, you
				need to call `make-frame-visible' to make
				the frame appear.
  popup				If non-nil, it should be a frame, and this
				frame will be created as a "popup" frame
				whose parent is the given frame.  This
				will make the window manager treat the
				frame as a dialog box, which may entail
				doing different things (e.g. not asking
				for positioning, and not iconifying
				separate from its parent).
  inter-line-space		Not currently implemented.
  toolbar-shadow-thickness	Thickness of toolbar shadows.
  background-toolbar-color	Color of toolbar background.
  bottom-toolbar-shadow-color	Color of bottom shadows on toolbars.
				(*Not* specific to the bottom-toolbar.)
  top-toolbar-shadow-color	Color of top shadows on toolbars.
				(*Not* specific to the top-toolbar.)
  internal-border-width		Width of internal border around text area.
  border-width			Width of external border around text area.
  top				Y position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  left				X position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  border-color			Color of external border around text area.
  cursor-color			Color of text cursor.

See also `default-frame-plist', which specifies properties which apply
to all frames, not just Gtk frames.
*/ );
  Vdefault_gtk_frame_plist = Qnil;

  gtk_console_methods->device_specific_frame_props = &Vdefault_gtk_frame_plist;
}
