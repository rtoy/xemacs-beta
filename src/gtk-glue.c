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

GType GTK_TYPE_STRING_GARRAY = 0;
GType GTK_TYPE_FLOAT_GARRAY = 0;
GType GTK_TYPE_INT_GARRAY = 0;
GType GTK_TYPE_SLIST = 0;
GType GTK_TYPE_STRING_SLIST = 0;
GType GTK_TYPE_OBJECT_SLIST = 0;

#include "console-gtk.h"
#include "fontcolor-gtk-impl.h"
#include "frame.h"

extern int lisp_to_gtk_type (Lisp_Object obj, GValue *arg);

static GType
xemacs_type_register (const gchar *name, GType parent)
{
  GType type_id;
  GTypeInfo info;

  info.class_size = 0;  //?
  info.base_init = NULL;
  info.base_finalize = NULL;

  info.class_init = NULL;
  info.class_finalize = NULL;
  info.class_data = 0;

  info.instance_size = 0;
  info.n_preallocs = 0;
  info.instance_init = 0;
  info.value_table = 0;

  type_id = g_type_register_static (parent, name, &info, (GTypeFlags)0);

  return (type_id);
}

static void
xemacs_init_gtk_classes (void)
{
#if 0
  /* These are all stored as GValue or GValueArray */
  /* Why aren't GLib types GOBjects? */
  if (!GTK_TYPE_GARRAY)
    {
      GTK_TYPE_GARRAY = xemacs_type_register ("GArray", 0);
      GTK_TYPE_LIST = xemacs_type_register ("GSList", 0);
      GTK_TYPE_STRING_LIST = xemacs_type_register ("GtkListOfString", GTK_TYPE_LISTOF);
      GTK_TYPE_OBJECT_LIST = xemacs_type_register ("GtkListOfObject", GTK_TYPE_LISTOF);
  }
#endif
}

static void
xemacs_list_to_gtklist (Lisp_Object obj, GValue *arg)
{
  CHECK_LIST (obj);
  int len;
  GValueArray *array;
  
  /*
   * The list is converted into an array.   I'll figure out
   * how to a make a real list later, perhaps.  --jsparkes
   */
  GET_LIST_LENGTH (obj, len);
  //array = g_array_new (0, TRUE, sizeof (GValue *));  // XXX leak?
  array = g_value_array_new (len);

  SAFE_LIST_LOOP_2 (elt, obj)
    {
      GValue tmp, *copy;
      lisp_to_gtk_type (elt, &tmp);
      g_value_copy (&tmp, copy);/* leak XXX */
      g_value_array_append (array, copy);
    }

  g_value_init (arg, G_TYPE_VALUE_ARRAY);
  g_value_set_boxed (arg, array);

#if 0
  assert (G_TYPE_IS_VALUE (arg));
#endif
 
  //if (*arg == GTK_TYPE_POINTER)
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

#if 0

      arg = strings;
#endif

    }
    //  else if (*arg == GTK_TYPE_OBJECT_LIST)
    {
      ABORT();
#if 0

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
#endif
    }
    //else
    {
      ABORT ();
    }
}

static void
__make_gtk_object_mapper (gpointer data, gpointer user_data)
{
  Lisp_Object *rv = (Lisp_Object *) user_data;
  assert (G_IS_OBJECT (data));
  *rv = Fcons (build_gtk_object (G_OBJECT (data)), *rv);
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
#if 0
  if (G_TYPE_IS_ABSTRACT (*arg))
    {
      if (*arg == GTK_TYPE_STRING_LIST)
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
#endif
  return (rval);
}

int lisp_to_gtk_type (Lisp_Object obj, GValue *arg);

static void
xemacs_list_to_array (Lisp_Object obj, GValue *arg)
{
  int len = 0;
  GValueArray *array;
  GValue val;

  CHECK_LIST (obj);
  assert (G_VALUE_TYPE (arg) == G_TYPE_BOXED);
  g_value_init (arg, G_TYPE_VALUE_ARRAY);

  GET_LIST_LENGTH (obj, len);
  /* Does g_value_array need to be pre-allocated? */
  array = g_value_array_new (len);

  LIST_LOOP_3 (elt, obj, tail)
    {
      g_value_reset (&val);
      lisp_to_gtk_type (elt, &val);
      g_value_array_append (array, &val);
    }

  g_value_set_boxed (arg, array);

#ifdef JSPARKES

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
  
  if (*arg == GTK_TYPE_STRING_ARRAY)
    {
      //FROB (gchar *, CHECK_STRING, (gchar*) XSTRING_DATA);
    }
  else if (*arg == GTK_TYPE_FLOAT_ARRAY)
    {
      //FROB (gfloat, CHECK_FLOAT, extract_float);
    }
  else if (*arg == GTK_TYPE_INT_ARRAY)
    {
      //FROB (gint, CHECK_INT, XINT);
    }
  else
    {
      ABORT ();
    }
#undef FROB
#endif
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

#ifdef JSPARKES
  style->font = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (font));
#endif

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
