/*
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
the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02111-1301, USA.  */

GType GTK_TYPE_ARRAY = 0;
GType GTK_TYPE_STRING_ARRAY = 0;
GType GTK_TYPE_FLOAT_ARRAY = 0;
GType GTK_TYPE_INT_ARRAY = 0;
GType GTK_TYPE_LISTOF = 0;
GType GTK_TYPE_STRING_LIST = 0;
GType GTK_TYPE_OBJECT_LIST = 0;
GType GTK_TYPE_GDK_GC = 0;

#include "console-gtk.h"
#include "fontcolor-gtk-impl.h"

static GType
xemacs_type_register (const gchar *name, GType parent)
{
  GType type_id;
  GTypeInfo info;

  info.class_size = 0;  //?
  info.base_init = NULL;
  into.base_finalize = NULL;

  info.class_init = NULL;
  info.class_finalize = NULL;
  info.class_data = 0;

  info.instance_size = 0;
  info.n_preallocs = 0;
  info.object_size = 0;
  info.instanace_init = 0;
  info.value_table = 0;

  type_id = gtk_type_unique (parent, &info);

  return (type_id);
}

static void
xemacs_init_gtk_classes (void)
{
  if (!GTK_TYPE_ARRAY)
    {
      GTK_TYPE_ARRAY = xemacs_type_register ("GtkArrayOf", 0);
      GTK_TYPE_STRING_ARRAY = xemacs_type_register ("GtkArrayOfString", GTK_TYPE_ARRAY);
      GTK_TYPE_FLOAT_ARRAY = xemacs_type_register ("GtkArrayOfFloat", GTK_TYPE_ARRAY);
      GTK_TYPE_INT_ARRAY = xemacs_type_register ("GtkArrayOfInteger", GTK_TYPE_ARRAY);
      GTK_TYPE_LISTOF = xemacs_type_register ("GtkListOf", 0);
      GTK_TYPE_STRING_LIST = xemacs_type_register ("GtkListOfString", GTK_TYPE_LISTOF);
      GTK_TYPE_OBJECT_LIST = xemacs_type_register ("GtkListOfObject", GTK_TYPE_LISTOF);
      GTK_TYPE_GDK_GC = xemacs_type_register ("GdkGC", GTK_TYPE_BOXED);
  }
}

static void
xemacs_list_to_gtklist (Lisp_Object obj, GType *arg)
{
  CHECK_LIST (obj);

  if (arg->type == GTK_TYPE_STRING_LIST)
    {
      Lisp_Object temp = obj;
      GList *strings = NULL;

      while (!NILP (temp))
	{
	  CHECK_STRING (XCAR (temp));
	  temp = XCDR (temp);
	}

      temp = obj;

      while (!NILP (temp))
	{
	  strings = g_list_append (strings, XSTRING_DATA (XCAR (temp)));
	  temp = XCDR (temp);
	}

      GTK_VALUE_POINTER (*arg) = strings;
    }
  else if (arg->type == GTK_TYPE_OBJECT_LIST)
    {
      Lisp_Object temp = obj;
      GList *objects = NULL;

      while (!NILP (temp))
	{
	  CHECK_GTK_OBJECT (XCAR (temp));
	  temp = XCDR (temp);
	}

      temp = obj;

      while (!NILP (temp))
	{
	  objects = g_list_append (objects, XGTK_OBJECT (XCAR (temp))->object);
	  temp = XCDR (temp);
	}

      GTK_VALUE_POINTER (*arg) = objects;
    }
  else
    {
      ABORT ();
    }
}

static void
__make_gtk_object_mapper (gpointer data, gpointer user_data)
{
  Lisp_Object *rv = (Lisp_Object *) user_data;

  *rv = Fcons (build_gtk_object (GTK_OBJECT (data)), *rv);
}

static void
__make_string_mapper (gpointer data, gpointer user_data)
{
  Lisp_Object *rv = (Lisp_Object *) user_data;

  *rv = Fcons (build_cistring ((char *)data), *rv);
}

static Lisp_Object
xemacs_gtklist_to_list (GType *arg)
{
  Lisp_Object rval = Qnil;

  if (GTK_VALUE_POINTER (*arg))
    {
      if (arg->type == GTK_TYPE_STRING_LIST)
	{
	  g_list_foreach ((GList*) GTK_VALUE_POINTER (*arg), __make_string_mapper, &rval);
	}
      else if (arg->type == GTK_TYPE_OBJECT_LIST)
	{
	  g_list_foreach ((GList*) GTK_VALUE_POINTER (*arg), __make_gtk_object_mapper, &rval);
	}
      else
	{
	  ABORT ();
	}
    }
  return (rval);
}

static void
xemacs_list_to_array (Lisp_Object obj, GType *arg)
{
  CHECK_LIST (obj);

#define FROB(ret_type,check_fn,extract_fn) \
  do {								\
    Lisp_Object temp = obj;					\
    int length = 0;						\
    ret_type *array = NULL;					\
								\
    while (!NILP (temp))					\
      {								\
	check_fn (XCAR (temp));					\
	length++;						\
	temp = XCDR (temp);					\
      }								\
								\
    array = xnew_array_and_zero (ret_type, length + 2);		\
    temp = obj;							\
    length = 0;							\
								\
    while (!NILP (temp))					\
      {								\
	array[length++] = extract_fn (XCAR (temp));		\
	temp = XCDR (temp);					\
      }								\
								\
    GTK_VALUE_POINTER (*arg) = array;				\
  } while (0);
  
  if (arg->type == GTK_TYPE_STRING_ARRAY)
    {
      FROB (gchar *, CHECK_STRING, (gchar*) XSTRING_DATA);
    }
  else if (arg->type == GTK_TYPE_FLOAT_ARRAY)
    {
      FROB (gfloat, CHECK_FLOAT, extract_float);
    }
  else if (arg->type == GTK_TYPE_INT_ARRAY)
    {
      FROB (gint, CHECK_INT, XINT);
    }
  else
    {
      ABORT ();
    }
#undef FROB
}

static GdkGC *
face_to_gc (Lisp_Object face)
{
  Lisp_Object frame = Fselected_frame (Qnil);

  return (gtk_get_gc (XFRAME (frame),
		      Fspecifier_instance (Fget (face, Qfont, Qnil),
					   frame, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qforeground, Qnil),
					   frame, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qbackground, Qnil),
					   frame, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qbackground_pixmap,
						 Qnil),
					   frame, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qbackground_placement,
						 Qnil),
					   frame, Qnil, Qnil),
		      Qnil));
}

static GtkStyle *
face_to_style (Lisp_Object face)
{
  Lisp_Object device = Fselected_device (Qnil);
  GtkStyle *style = gtk_style_new ();
  int i;

  Lisp_Object font = Fspecifier_instance (Fget (face, Qfont, Qnil),
					  device, Qnil, Qnil);
  Lisp_Object fg = Fspecifier_instance (Fget (face, Qforeground, Qnil),
					device, Qnil, Qnil);
  Lisp_Object bg = Fspecifier_instance (Fget (face, Qbackground, Qnil),
					device, Qnil, Qnil);
  Lisp_Object pm = Fspecifier_instance (Fget (face, Qbackground_pixmap,
					      Qnil), device, Qnil, Qnil);

  for (i = 0; i < 5; i++)
    style->fg[i] = *COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (fg));
  for (i = 0; i < 5; i++)
    style->bg[i] = *COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (bg));

  if (IMAGE_INSTANCEP (pm))
    {
      for (i = 0; i < 5; i++)
	style->bg_pixmap[i] = XIMAGE_INSTANCE_GTK_PIXMAP (pm);
    }

  style->font = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (font));

  return (style);
}

static Lisp_Object
gdk_event_to_emacs_event (GdkEvent *ev)
{
  Lisp_Object event = Qnil;

  if (ev)
    {
      Lisp_Event *emacs_event;

      event = Fmake_event (Qnil, Qnil);
      emacs_event = XEVENT (event);

      if (!gtk_event_to_emacs_event (NULL, ev, emacs_event))
	{
	  /* We need to handle a few more cases than the normal event
	  ** loop does.  Mainly the double/triple click events.
	  */
	  if ((ev->type == GDK_2BUTTON_PRESS) || (ev->type == GDK_3BUTTON_PRESS))
	    {
	      set_event_type (emacs_event, misc_user_event);
	      SET_EVENT_MISC_USER_BUTTON (emacs_event, ev->button.button);
	      SET_EVENT_MISC_USER_MODIFIERS (emacs_event, 0);
	      SET_EVENT_MISC_USER_X (emacs_event, (int) ev->button.x);
	      SET_EVENT_MISC_USER_Y (emacs_event, (int) ev->button.y);
	      if (ev->type == GDK_2BUTTON_PRESS)
		SET_EVENT_MISC_USER_FUNCTION (emacs_event, intern ("double-click"));
	      else
		SET_EVENT_MISC_USER_FUNCTION (emacs_event, intern ("triple-click"));
	    }
	  else
	    {
	      Fdeallocate_event (event);
	      event = Qnil;
	    }
	}
    }
  return (event);
}
