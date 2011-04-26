/* Implements an elisp-programmable menubar -- Gtk interface.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
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

/* created 16-dec-91 by jwz */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"           /* zmacs_regions */
#include "device-impl.h"
#include "events.h"
#include "frame-impl.h"
#include "gui.h"
#include "opaque.h"
#include "window.h"
#include "window-impl.h"

#include "console-gtk-impl.h"
#include "ui-gtk.h"
#include "menubar.h"

#define MENUBAR_TYPE	0
#define SUBMENU_TYPE	1
#define POPUP_TYPE	2

#define XEMACS_MENU_DESCR_TAG g_quark_from_string ("xemacs::menu::description")
#define XEMACS_MENU_FILTER_TAG g_quark_from_string ("xemacs::menu::filter")
#define XEMACS_MENU_GUIID_TAG g_quark_from_string ("xemacs::menu::gui_id")
#define XEMACS_MENU_FIRSTTIME_TAG g_quark_from_string ("xemacs::menu::first_time")
#define XEMACS_MENU_FRAME_TAG g_quark_from_string ("xemacs::menu::frame")

static GtkWidget *menu_descriptor_to_widget_1 (Lisp_Object descr, GtkAccelGroup* accel_group);

#define FRAME_GTK_MENUBAR_DATA(f) (FRAME_GTK_DATA (f)->menubar_data)
#define XFRAME_GTK_MENUBAR_DATA_LASTBUFF(f) XCAR (FRAME_GTK_MENUBAR_DATA (f))
#define XFRAME_GTK_MENUBAR_DATA_UPTODATE(f) XCDR (FRAME_GTK_MENUBAR_DATA (f))


GtkWidget *
gtk_xemacs_menubar_new (struct frame *f)
{
  GtkWidget *menubar = gtk_menu_bar_new ();
  g_object_set_qdata (G_OBJECT (menubar), XEMACS_MENU_FRAME_TAG, f);
  return (GTK_WIDGET (menubar));
}


/* Converting from XEmacs to GTK representation */
static Lisp_Object
menu_name_to_accelerator (Ibyte *name)
{
  while (*name) {
    if (*name=='%') {
      ++name;
      if (!(*name))
	return Qnil;
      if (*name=='_' && *(name+1))
	{
	  int accelerator = (int) (*(name+1));
	  return make_char (tolower (accelerator));
	}
    }
    ++name;
  }
  return Qnil;
}

static void __activate_menu(GtkMenuItem *, gpointer);

/* This is called when a menu is about to be shown... this is what
   does the delayed creation of the menu items.  We populate the
   submenu and away we go. */
static void
__maybe_destroy (GtkWidget *child, GtkWidget *UNUSED (precious))
{
  if (GTK_IS_MENU_ITEM (child))
    {
      if (GTK_WIDGET_VISIBLE (child))
	{
	  /* If we delete the menu item that was 'active' when the
	     menu was cancelled, GTK gets upset because it tries to
	     remove the focus rectangle from a (now) dead widget.

	     This widget will eventually get killed because it will
	     not be visible the next time the window is shown.
	  */
	  gtk_widget_set_sensitive (child, FALSE);
	  gtk_widget_hide_all (child);
	}
      else
	{
	  gtk_widget_destroy (child);
	}
    }
}

/* If user_data != 0x00 then we are using a hook to build the menu. */
static void
__activate_menu(GtkMenuItem *item, gpointer user_data)
{
  Lisp_Object desc;
  gpointer force_clear = g_object_get_qdata (G_OBJECT (item), XEMACS_MENU_FIRSTTIME_TAG);

  g_object_set_qdata (G_OBJECT (item), XEMACS_MENU_FIRSTTIME_TAG, 0x00);

  /* Delete the old contents of the menu if we are the top level menubar */
  if (GTK_IS_MENU_BAR (GTK_WIDGET (item)->parent) || force_clear)
    {
      GtkWidget *selected = gtk_menu_get_active (GTK_MENU (item->submenu));

     gtk_container_foreach (GTK_CONTAINER (item->submenu),
                            (GtkCallback) __maybe_destroy, selected);
    }
  else if (gtk_container_children (GTK_CONTAINER (item->submenu)))
    {
      return;
    }

  desc = GET_LISP_FROM_VOID (g_object_get_qdata (G_OBJECT (item), XEMACS_MENU_DESCR_TAG));

  if (user_data)
    {
      GUI_ID id = (GUI_ID) GPOINTER_TO_INT (g_object_get_qdata (G_OBJECT (item), XEMACS_MENU_GUIID_TAG));
      Lisp_Object hook_fn;
      struct gcpro gcpro1, gcpro2;

      hook_fn = GET_LISP_FROM_VOID (g_object_get_qdata (G_OBJECT (item), XEMACS_MENU_FILTER_TAG));

      GCPRO2 (desc, hook_fn);

      desc = call1 (hook_fn, desc);

      UNGCPRO;

      ungcpro_popup_callbacks (id);
      gcpro_popup_callbacks (id, desc);
    }

  /* Build the child widgets */
  for (; !NILP (desc); desc = Fcdr (desc))
    {
      GtkWidget *next = NULL;
      Lisp_Object child = Fcar (desc);

      if (NILP (child))	/* the partition */
	{
	  /* Signal an error here?  The NILP handling is handled a
             layer higher where appropriate */
	}
      else
	{
	  next = menu_descriptor_to_widget_1 (child, NULL);
          // gtk_menu_ensure_uline_accel_group (GTK_MENU (item->submenu)));
	}

      if (next)
        {
          gtk_widget_show_all (next);
          gtk_menu_append (GTK_MENU (item->submenu), next);
        }
    }
}

/* This is called whenever an item with a GUI_ID associated with it is
   destroyed.  This allows us to remove the references in gui-gtk.c
   that made sure callbacks and such were GCPRO-ed
*/
static void
__remove_gcpro_by_id (gpointer user_data)
{
  ungcpro_popup_callbacks ((GUI_ID) GPOINTER_TO_INT (user_data));
}

static void
__kill_stupid_gtk_timer (GtkObject *obj, gpointer UNUSED (user_data))
{
  GtkMenuItem *mi = GTK_MENU_ITEM (obj);

  if (mi->timer)
    {
      gtk_timeout_remove (mi->timer);
      mi->timer = 0;
    }
}

/* Convert the XEmacs menu accelerator representation to Gtk mnemonic form. If
  no accelerator has been provided, put one at the start of the string (this
  mirrors the behaviour under X).  This algorithm is also found in
  dialog-gtk.el:gtk-popup-convert-underscores.
*/
static char *
convert_underscores(const Ibyte *name)
{
  char *rval;
  int i,j;
  int found_accel = FALSE;
  int underscores = 0;

  for (i = 0; name[i]; ++i)
    if (name[i] == '%' && name[i+1] == '_')
      {
	found_accel = TRUE;
      }
    else if (name[i] == '_')
      {
	underscores++;
      }

  /* Allocate space for the original string, plus zero byte plus extra space
     for all quoted underscores plus possible additional leading accelerator. */
  rval = (char*) xmalloc_and_zero (qxestrlen(name) + 1 + underscores
				   + (found_accel ? 0 : 1));

  if (!found_accel)
    rval[0] = '_';

  for (i = 0, j = (found_accel ? 0 : 1); name[i]; i++)
    {
      if (name[i]=='%')
	{
	  i++;
	  if (!(name[i]))
	    continue;
	  
	  if ((name[i] != '_') && (name[i] != '%'))
	    i--;

	  found_accel = TRUE;
	}
      else if (name[i] == '_')
	{
	  rval[j++] = '_';
	}

      rval[j++] = name[i];
    }

  return rval;
}

/* Remove the XEmacs menu accellerator representation from a string. */
static char *
remove_underscores(const Ibyte *name)
{
  char *rval = (char*) xmalloc_and_zero (qxestrlen(name) + 1);
  int i,j;

  for (i = 0, j = 0; name[i]; i++)
    {
      if (name[i]=='%') {
	i++;
	if (!(name[i]))
	  continue;

	if ((name[i] != '_') && (name[i] != '%'))
	  i--;
	else
	  continue;
      }
      rval[j++] = name[i];
    }
  return rval;
}

/* This converts an entire menu into a GtkMenuItem (with an attached
   submenu).  A menu is a list of (STRING [:keyword value]+ [DESCR]+)
   DESCR is either a list (meaning a submenu), a vector, or nil (if
   you include a :filter keyword) */
static GtkWidget *
menu_convert (Lisp_Object desc, GtkWidget *reuse,
	      GtkAccelGroup* menubar_accel_group)
{
  GtkWidget *menu_item = NULL;
  GtkWidget *submenu = NULL;
  Lisp_Object key, val;
  Lisp_Object include_p = Qnil, hook_fn = Qnil, config_tag = Qnil;
  Lisp_Object active_p = Qt;
  Lisp_Object accel;
  int included_spec = 0;
  int active_spec = 0;

  if (STRINGP (XCAR (desc)))
    {
      accel = menu_name_to_accelerator (XSTRING_DATA (XCAR (desc)));

      if (!reuse)
	{
	  char *temp_menu_name = convert_underscores (XSTRING_DATA (XCAR (desc)));
	  GtkWidget* accel_label = gtk_label_new(NULL);
	  guint accel_key;

	  gtk_misc_set_alignment (GTK_MISC (accel_label), 0.0, 0.5);
          gtk_label_set_use_underline (GTK_LABEL (accel_label), TRUE);
          accel_key = gtk_label_parse_uline (GTK_LABEL (accel_label), temp_menu_name);
	  menu_item = gtk_menu_item_new ();
	  gtk_container_add (GTK_CONTAINER (menu_item), accel_label);
	  gtk_widget_show (accel_label);

	  if (menubar_accel_group)
	    gtk_widget_add_accelerator (menu_item,
					"activate",
					menubar_accel_group,
					accel_key, GDK_MOD1_MASK,
					GTK_ACCEL_LOCKED);
	  free (temp_menu_name);
	}
      else
	{
	  menu_item = reuse;
	}

      submenu = gtk_menu_new ();
      gtk_widget_show (menu_item);
      gtk_widget_show (submenu);

      if (!reuse)
        assert (g_signal_connect (G_OBJECT (menu_item), "destroy",
                                 GTK_SIGNAL_FUNC (__kill_stupid_gtk_timer), NULL));

      /* Without this sometimes a submenu gets left on the screen -
      ** urk
      */
      if (GTK_MENU_ITEM (menu_item)->submenu)
	{
	  gtk_widget_destroy (GTK_MENU_ITEM (menu_item)->submenu);
	}

      gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_item), submenu);

      /* We put this bogus menu item in so that GTK does the right
      ** thing when the menu is near the screen border.
      **
      ** Aug 29, 2000
      */
      {
	GtkWidget *bogus_item = gtk_menu_item_new_with_label ("A suitably long label here...");

	g_object_set_qdata (G_OBJECT (menu_item), XEMACS_MENU_FIRSTTIME_TAG, (gpointer)0x01);
	gtk_widget_show_all (bogus_item);
	gtk_menu_append (GTK_MENU (submenu), bogus_item);
      }

      desc = Fcdr (desc);

      while (key = Fcar (desc), KEYWORDP (key))
	{
	  Lisp_Object cascade = desc;
	  desc = Fcdr (desc);
	  if (NILP (desc))
	    sferror ("keyword in menu lacks a value",
				 cascade);
	  val = Fcar (desc);
	  desc = Fcdr (desc);
	  if (EQ (key, Q_included))
	    include_p = val, included_spec = 1;
	  else if (EQ (key, Q_config))
	    config_tag = val;
	  else if (EQ (key, Q_filter))
	    hook_fn = val;
	  else if (EQ (key, Q_active))
	    active_p = val, active_spec = 1;
	  else if (EQ (key, Q_accelerator))
	    {
#if 0
	      if ( SYMBOLP (val)
		   || CHARP (val))
		wv->accel = STORE_LISP_IN_VOID (val);
	      else
		invalid_argument ("bad keyboard accelerator", val);
#endif
	    }
	  else if (EQ (key, Q_label))
	    {
	      /* implement in 21.2 */
	    }
	  else
	    invalid_argument ("unknown menu cascade keyword", cascade);
	}

      g_object_set_qdata (G_OBJECT (menu_item), XEMACS_MENU_DESCR_TAG, STORE_LISP_IN_VOID (desc));
      g_object_set_qdata (G_OBJECT (menu_item), XEMACS_MENU_FILTER_TAG, STORE_LISP_IN_VOID (hook_fn));

      if ((!NILP (config_tag)
	   && NILP (Fmemq (config_tag, Vmenubar_configuration)))
	  || (included_spec &&
              NILP (IGNORE_MULTIPLE_VALUES (Feval (include_p)))))
	{
	  return (NULL);
	}

      if (active_spec)
        active_p = IGNORE_MULTIPLE_VALUES (Feval (active_p));

      gtk_widget_set_sensitive (GTK_WIDGET (menu_item), ! NILP (active_p));
    }
  else
    {
      invalid_argument ("menu name (first element) must be a string",
			   desc);
    }

  /* If we are reusing a widget, we need to make sure we clean
  ** everything up.
  */
  if (reuse)
    {
      gpointer id = g_object_get_qdata (G_OBJECT (reuse), XEMACS_MENU_GUIID_TAG);

      if (id)
	{
	  /* If the menu item had a GUI_ID that means it was a filter menu */
	  __remove_gcpro_by_id (id);
	  gtk_signal_disconnect_by_func (GTK_OBJECT (reuse),
                                       GTK_SIGNAL_FUNC (__activate_menu),
                                       (gpointer) 0x01);
	}
      else
	{
	  gtk_signal_disconnect_by_func (GTK_OBJECT (reuse),
					 GTK_SIGNAL_FUNC (__activate_menu),
					 NULL);
	}

      GTK_MENU_ITEM (reuse)->right_justify = 0;
    }

  if (NILP (hook_fn))
    {
      /* Generic menu builder */
      assert (g_signal_connect (G_OBJECT (menu_item), "activate",
                                GTK_SIGNAL_FUNC (__activate_menu),
                                NULL));
    }
  else
    {
      GUI_ID id = new_gui_id ();

      g_object_set_qdata (G_OBJECT (menu_item), XEMACS_MENU_GUIID_TAG,
                          GINT_TO_POINTER (id));

      /* Make sure we gcpro the menu descriptions */
      gcpro_popup_callbacks (id, desc);
      gtk_object_weakref (GTK_OBJECT (menu_item), __remove_gcpro_by_id,
			  GINT_TO_POINTER (id));

      assert (g_signal_connect (G_OBJECT (menu_item), "activate",
                                GTK_SIGNAL_FUNC (__activate_menu),
                                GINT_TO_POINTER (0x01)));
    }

  return (menu_item);
}

/* Called whenever a button, radio, or toggle is selected in the menu */
static void
__generic_button_callback (GtkMenuItem *item, gpointer user_data)
{
  Lisp_Object callback, function, data, channel;

  channel = wrap_frame (gtk_widget_to_frame (GTK_WIDGET (item)));

  callback = GET_LISP_FROM_VOID (user_data);

  get_gui_callback (callback, &function, &data);

  signal_special_gtk_user_event (channel, function, data);
}

/* Convert a single menu item descriptor to a suitable GtkMenuItem */
/* This function cannot GC.
   It is only called from menu_item_descriptor_to_widget_value, which
   prohibits GC. */
static GtkWidget *
menu_descriptor_to_widget_1 (Lisp_Object descr, GtkAccelGroup* accel_group)
{
  if (STRINGP (descr))
    {
      /* It is a separator.  Unfortunately GTK does not allow us to
         specify what our separators look like, so we can't do all the
         fancy stuff that the X code does.
      */
      return (gtk_separator_menu_item_new ());
    }
  else if (LISTP (descr))
    {
      /* It is a submenu */
      return (menu_convert (descr, NULL, accel_group));
    }
  else if (VECTORP (descr))
    {
      /* An actual menu item description!  This gets yucky. */
      Lisp_Object name       = Qnil;
      Lisp_Object callback   = Qnil;
      Lisp_Object suffix     = Qnil;
      Lisp_Object active_p   = Qt;
      Lisp_Object include_p  = Qt;
      Lisp_Object selected_p = Qnil;
      Lisp_Object keys       = Qnil;
      Lisp_Object style      = Qnil;
      Lisp_Object config_tag = Qnil;
      Lisp_Object accel = Qnil;
      GtkWidget *main_label = NULL;
      int length = XVECTOR_LENGTH (descr);
      Lisp_Object *contents = XVECTOR_DATA (descr);
      int plist_p;
      int selected_spec = 0, included_spec = 0;
      GtkWidget *widget = NULL;
      guint accel_key = 0;

      if (length < 2)
	sferror ("button descriptors must be at least 2 long", descr);

      /* length 2:		[ "name" callback ]
	 length 3:		[ "name" callback active-p ]
	 length 4:		[ "name" callback active-p suffix ]
	 or			[ "name" callback keyword  value  ]
	 length 5+:		[ "name" callback [ keyword value ]+ ]
      */
      plist_p = (length >= 5 || (length > 2 && KEYWORDP (contents [2])));
      
      if (!plist_p && length > 2)
	/* the old way */
	{
	  name = contents [0];
	  callback = contents [1];
	  active_p = contents [2];
	  if (length == 4)
	    suffix = contents [3];
	}
      else
	{
	  /* the new way */
	  int i;
	  if (length & 1)
	    sferror (
				 "button descriptor has an odd number of keywords and values",
				 descr);

	  name = contents [0];
	  callback = contents [1];
	  for (i = 2; i < length;)
	    {
	      Lisp_Object key = contents [i++];
	      Lisp_Object val = contents [i++];
	      if (!KEYWORDP (key))
		invalid_argument_2 ("not a keyword", key, descr);

	      if      (EQ (key, Q_active))   active_p   = val;
	      else if (EQ (key, Q_suffix))   suffix     = val;
	      else if (EQ (key, Q_keys))     keys       = val;
	      else if (EQ (key, Q_key_sequence))  ; /* ignored for FSF compat */
	      else if (EQ (key, Q_label))  ; /* implement for 21.0 */
	      else if (EQ (key, Q_style))    style      = val;
	      else if (EQ (key, Q_selected)) selected_p = val, selected_spec = 1;
	      else if (EQ (key, Q_included)) include_p  = val, included_spec = 1;
	      else if (EQ (key, Q_config))	 config_tag = val;
	      else if (EQ (key, Q_accelerator))
		{
		  if ( SYMBOLP (val) || CHARP (val))
		    accel = val;
		  else
		    invalid_argument ("bad keyboard accelerator", val);
		}
	      else if (EQ (key, Q_filter))
		sferror(":filter keyword not permitted on leaf nodes", descr);
	      else
		invalid_argument_2 ("unknown menu item keyword", key, descr);
	    }
	}

#ifdef HAVE_MENUBARS
      if ((!NILP (config_tag) && NILP (Fmemq (config_tag, Vmenubar_configuration)))
	  || (included_spec && NILP (IGNORE_MULTIPLE_VALUES (Feval (include_p)))))

	{
	  /* the include specification says to ignore this item. */
	  return 0;
	}
#endif /* HAVE_MENUBARS */

      CHECK_STRING (name);

      if (NILP (accel))
	accel = menu_name_to_accelerator (XSTRING_DATA (name));

      if (!NILP (suffix))
        suffix = IGNORE_MULTIPLE_VALUES (Feval (suffix));


      if (!separator_string_p (XSTRING_DATA (name)))
	{
	  Ibyte *label_buffer = NULL;
	  char *temp_label = NULL;

	  if (STRINGP (suffix) && XSTRING_LENGTH (suffix))
	    {
	      /* !!#### */
	      label_buffer = alloca_ibytes (XSTRING_LENGTH (name) + 15 + XSTRING_LENGTH (suffix));
	      qxesprintf (label_buffer, "%s %s ", XSTRING_DATA (name),
			  XSTRING_DATA (suffix));
	    }
	  else
	    {
	      label_buffer = alloca_ibytes (XSTRING_LENGTH (name) + 15);
	      qxesprintf (label_buffer, "%s ", XSTRING_DATA (name));
	    }

	  temp_label = convert_underscores (label_buffer);
	  main_label = gtk_label_new (NULL);
          gtk_label_set_label (GTK_LABEL (main_label), temp_label);
	  /* accel_key = */
          gtk_label_set_use_underline (GTK_LABEL (main_label), TRUE);
	  free (temp_label);
	}

      /* Evaluate the selected and active items now */
      if (selected_spec)
	{
	  if (NILP (selected_p) || EQ (selected_p, Qt))
	    {
	      /* Do nothing */
	    }
	  else
	    {
              selected_p = IGNORE_MULTIPLE_VALUES (Feval (selected_p));
	    }
	}

      if (NILP (active_p) || EQ (active_p, Qt))
	{
	  /* Do Nothing */
	}
      else
	{
          active_p = IGNORE_MULTIPLE_VALUES (Feval (active_p));
	}

      if (0 || 
#ifdef HAVE_MENUBARS
	  menubar_show_keybindings
#endif
	  )
	{
	  /* Need to get keybindings */
	  if (!NILP (keys))
	    {
	      /* User-specified string to generate key bindings with */
	      CHECK_STRING (keys);

	      keys = Fsubstitute_command_keys (keys);
	    }
	  else if (SYMBOLP (callback))
	    {
	      DECLARE_EISTRING_MALLOC (buf);

	      /* #### Warning, dependency here on current_buffer and point */
	      where_is_to_char (callback, buf);

	      if (eilen (buf) > 0)
		keys = eimake_string (buf);
	      else
		{
		  
		  keys = Qnil;
		}

	      eifree (buf);
	    }
	}

      /* Now we get down to the dirty business of creating the widgets */
      if (NILP (style) || EQ (style, Qtext) || EQ (style, Qbutton))
	{
	  /* A normal menu item */
	  widget = gtk_menu_item_new ();
	}
      else if (EQ (style, Qtoggle) || EQ (style, Qradio))
	{
	  /* They are radio or toggle buttons.

	     XEmacs' menu descriptions are fairly lame in that they do
	     not have the idea of a 'group' of radio buttons.  They
	     are exactly like toggle buttons except that they get
	     drawn differently.

	     GTK rips us a new one again.  If you have a radio button
	     in a group by itself, it always draws it as highlighted.
	     So we dummy up and create a second radio button that does
	     not get added to the menu, but gets invisibly set/unset
	     when the other gets unset/set.  *sigh*

	  */
	  if (EQ (style, Qradio))
	    {
	      GtkWidget *dummy_sibling = NULL;
	      GSList *group = NULL;

	      dummy_sibling = gtk_radio_menu_item_new (group);
	      group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (dummy_sibling));
	      widget = gtk_radio_menu_item_new (group);

	      /* We need to notice when the 'real' one gets destroyed
                 so we can clean up the dummy as well. */
	      gtk_object_weakref (GTK_OBJECT (widget),
				  (GtkDestroyNotify) gtk_widget_destroy,
				  dummy_sibling);
	    }
	  else
	    {
	      widget = gtk_check_menu_item_new ();
	    }

	  /* What horrible defaults you have GTK dear!  The default
	    for a toggle menu item is to not show the toggle unless it
	    is turned on or actively highlighted.  How absolutely
	    hideous. */
	  gtk_check_menu_item_set_show_toggle (GTK_CHECK_MENU_ITEM (widget), TRUE);
	  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (widget),
					  NILP (selected_p) ? FALSE : TRUE);
	}
      else
	{
	  invalid_argument_2 ("unknown style", style, descr);
	}

      gtk_widget_set_sensitive (widget, ! NILP (active_p));

      assert (g_signal_connect (G_OBJECT (widget), "activate-item",
                                GTK_SIGNAL_FUNC (__generic_button_callback),
                                STORE_LISP_IN_VOID (callback)));
      
      assert (g_signal_connect (G_OBJECT (widget), "activate",
                                GTK_SIGNAL_FUNC (__generic_button_callback),
                                STORE_LISP_IN_VOID (callback)));

      /* Now that all the information about the menu item is known, set the
	 remaining properties.
      */
      
      if (main_label)
        {
	  if (NILP (keys))
	    {
	      gtk_container_add (GTK_CONTAINER (widget), main_label);
	    }
	  else
	    {
	      /* Replace the label widget with a hbox containing label and
		 key sequence. */
	      GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
	      GtkWidget *acc = gtk_label_new (LISP_STRING_TO_EXTERNAL (keys, Qctext));
	      gtk_misc_set_alignment (GTK_MISC (acc), 1.0, 0.5);
	      gtk_container_add (GTK_CONTAINER (hbox), main_label);
	      gtk_container_add (GTK_CONTAINER (hbox), GTK_WIDGET (acc));
	      gtk_container_add (GTK_CONTAINER (widget), hbox);
	    }
	  gtk_misc_set_alignment (GTK_MISC (main_label), 0.0, 0.5);
          
	  if (accel_group && accel_key > 0)
	    gtk_widget_add_accelerator (widget,
					(gchar *)"activate",
					accel_group,
					accel_key,
                                        (GdkModifierType)0,
					GTK_ACCEL_LOCKED);
        }

      return (widget);
    }
  else
    {
      return (NULL);
      /* ABORT (); ???? */
    }
}

static GtkWidget *
menu_descriptor_to_widget (Lisp_Object descr, GtkAccelGroup* accel_group)
{
  GtkWidget *rval = NULL;
  int count = begin_gc_forbidden ();

  /* Cannot GC from here on out... */
  rval = menu_descriptor_to_widget_1 (descr, accel_group);
  unbind_to (count);
  return (rval);
  
}

static gboolean
menu_can_reuse_widget (GtkWidget *child, const Ibyte *label)
{
  /* Everything up at the top level was done using
  ** gtk_xemacs_accel_label_new(), but we still double check to make
  ** sure we don't seriously foobar ourselves.
  */
  gpointer possible_child =
    g_list_nth_data (gtk_container_children (GTK_CONTAINER (child)), 0);
  gboolean ret_val = FALSE;

  if (possible_child && GTK_IS_LABEL (possible_child))
    {
      char *temp_label = remove_underscores (label);

      if (!strcmp (GTK_LABEL (possible_child)->label, temp_label))
	ret_val = TRUE;

      free (temp_label);
    }

  return ret_val;
}

/* Converts a menubar description into a GtkMenuBar... a menubar is a
   list of menus or buttons 
*/
static void
menu_create_menubar (struct frame *f, Lisp_Object descr)
{
  gboolean right_justify = FALSE;
  Lisp_Object value = descr;
  GtkWidget *menubar = FRAME_GTK_MENUBAR_WIDGET (f);
  GUI_ID id = (GUI_ID) GPOINTER_TO_INT (g_object_get_qdata (G_OBJECT (menubar), XEMACS_MENU_GUIID_TAG));
  guint menu_position = 0;
  GtkAccelGroup *menubar_accel_group;

  /* Remove any existing protection for old menu items */
  ungcpro_popup_callbacks (id);

  /* GCPRO the whole damn thing */
  gcpro_popup_callbacks (id, descr);

  menubar_accel_group = gtk_accel_group_new();

  {
    EXTERNAL_LIST_LOOP_2 (item_descr, value)
      {
	gpointer current_child = g_list_nth_data (GTK_MENU_SHELL (menubar)->children, menu_position);

	if (NILP (item_descr))
	  {
	    /* Need to start right-justifying menus */
	    right_justify = TRUE;
	    menu_position--;
	  }
	else if (VECTORP (item_descr))
	  {
	    /* It is a button description */
	    GtkWidget *item;

	    item = menu_descriptor_to_widget (item_descr, menubar_accel_group);
	    gtk_widget_set_name (item, "XEmacsMenuButton");

	    if (!item)
	      {
		item = gtk_menu_item_new_with_label ("ITEM CREATION ERROR");
	      }

	    gtk_widget_show_all (item);
	    if (current_child) gtk_widget_destroy (GTK_WIDGET (current_child));
	    gtk_menu_bar_insert (GTK_MENU_BAR (menubar), item, menu_position);
	  }
	else if (LISTP (item_descr))
	  {
	    /* Need to actually convert it into a menu and slap it in */
	    GtkWidget *widget;
	    gboolean reused_p = FALSE;

	    /* We may be able to reuse the widget, let's at least check. */
	    if (current_child && menu_can_reuse_widget (GTK_WIDGET (current_child),
							XSTRING_DATA (XCAR (item_descr))))
	      {
		widget = menu_convert (item_descr, GTK_WIDGET (current_child),
				       menubar_accel_group);
		reused_p = TRUE;
	      }
	    else
	      {
		widget = menu_convert (item_descr, NULL, menubar_accel_group);
		if (current_child) gtk_widget_destroy (GTK_WIDGET (current_child));
		gtk_menu_bar_insert (GTK_MENU_BAR (menubar), widget, menu_position);
	      }

	    if (widget)
	      {
		if (right_justify) gtk_menu_item_right_justify (GTK_MENU_ITEM (widget));
	      }
	    else
	      {
		widget = gtk_menu_item_new_with_label ("ERROR");
		/* ABORT() */
	      }
	    gtk_widget_show_all (widget);
	  }
	else if (STRINGP (item_descr))
	  {
	    /* Do I really want to be this careful?  Anything else in a
	       menubar description is illegal */
	  }
	menu_position++;
      }
  }

  /* Need to delete any menu items that were past the bounds of the new one */
  {
    GList *l = NULL;

    while ((l = g_list_nth (GTK_MENU_SHELL (menubar)->children, menu_position)))
      {
	gpointer data = l->data;
	l = g_list_remove_link (GTK_MENU_SHELL (menubar)->children, l);

	if (data)
	  {
	    gtk_widget_destroy (GTK_WIDGET (data));
	  }
      }
  }

  /* Attach the new accelerator group to the frame. */
  gtk_window_add_accel_group (GTK_WINDOW (FRAME_GTK_SHELL_WIDGET(f)),
			      menubar_accel_group);
}


/* Deal with getting/setting the menubar */

static gboolean
run_menubar_hook (GtkWidget *widget, GdkEventButton *UNUSED (event),
		  gpointer UNUSED (user_data))
{
  if (!GTK_MENU_SHELL(widget)->active)
    {
      run_hook (Qactivate_menubar_hook);
    }
  return(FALSE);
}

static void
create_menubar_widget (struct frame *f)
{
  GUI_ID id = new_gui_id ();
  GtkWidget *menubar = gtk_xemacs_menubar_new (f);

  gtk_box_pack_start (GTK_BOX (FRAME_GTK_CONTAINER_WIDGET (f)), menubar, FALSE, FALSE, 0);

  assert (g_signal_connect (G_OBJECT (menubar), "button-press-event",
                            GTK_SIGNAL_FUNC (run_menubar_hook), NULL));

  FRAME_GTK_MENUBAR_WIDGET (f) = menubar;
  g_object_set_qdata (G_OBJECT (menubar), XEMACS_MENU_GUIID_TAG, GINT_TO_POINTER (id));
  gtk_object_weakref (GTK_OBJECT (menubar), __remove_gcpro_by_id, GINT_TO_POINTER (id));
}

static int
set_frame_menubar (struct frame *f, int first_time_p)
{
  Lisp_Object menubar;
  int menubar_visible;
  /* As for the toolbar, the minibuffer does not have its own menubar. */
  struct window *w = XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f));

  if (! FRAME_GTK_P (f))
    return 0;

  /***** first compute the contents of the menubar *****/

  if (! first_time_p)
    {
      /* evaluate `current-menubar' in the buffer of the selected window
	 of the frame in question. */
      menubar = symbol_value_in_buffer (Qcurrent_menubar, w->buffer);
    }
  else
    {
      /* That's a little tricky the first time since the frame isn't
	 fully initialized yet. */
      menubar = Fsymbol_value (Qcurrent_menubar);
    }

  if (NILP (menubar))
    {
      menubar = Vblank_menubar;
      menubar_visible = 0;
    }
  else
    {
      menubar_visible = !NILP (w->menubar_visible_p);
    }

  if (!FRAME_GTK_MENUBAR_WIDGET (f))
    {
      create_menubar_widget (f);
    }

  /* Populate the menubar, but nothing is shown yet */
  {
    Lisp_Object old_buffer;
    int count = specpdl_depth ();

    old_buffer = Fcurrent_buffer ();
    record_unwind_protect (Fset_buffer, old_buffer);
    Fset_buffer (XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer);

    menu_create_menubar (f, menubar);

    Fset_buffer (old_buffer);
    unbind_to (count);
  }

  FRAME_GTK_MENUBAR_DATA (f) = Fcons (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->buffer, Qt);

  return (menubar_visible);
}

/* Called from gtk_create_widgets() to create the inital menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible. */
int
gtk_initialize_frame_menubar (struct frame *f)
{
  create_menubar_widget  (f);
  return set_frame_menubar (f, 1);
}


static void
gtk_update_frame_menubar_internal (struct frame *f)
{
  /* We assume the menubar contents has changed if the global flag is set,
     or if the current buffer has changed, or if the menubar has never
     been updated before.
   */
  int menubar_contents_changed =
    (f->menubar_changed
     || NILP (FRAME_GTK_MENUBAR_DATA (f))
     || (!EQ (XFRAME_GTK_MENUBAR_DATA_LASTBUFF (f),
	      XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->buffer)));

  gboolean menubar_was_visible = GTK_WIDGET_VISIBLE (FRAME_GTK_MENUBAR_WIDGET (f));
  gboolean menubar_will_be_visible = menubar_was_visible;
  gboolean menubar_visibility_changed;

  if (menubar_contents_changed)
    {
      menubar_will_be_visible = set_frame_menubar (f, 0);
    }

  menubar_visibility_changed = menubar_was_visible != menubar_will_be_visible;

  if (!menubar_visibility_changed)
    {
      return;
    }

  /* We hide and show the menubar's parent (which is actually the
     GtkHandleBox)... this is to simplify the code that destroys old
     menu items, etc.  There is no easy way to get the child out of a
     handle box, and I didn't want to add yet another stupid widget
     slot to struct gtk_frame. */
  if (menubar_will_be_visible)
    {
      gtk_widget_show_all (FRAME_GTK_MENUBAR_WIDGET (f)->parent);
    }
  else
    {
      gtk_widget_hide_all (FRAME_GTK_MENUBAR_WIDGET (f)->parent);
    }

  MARK_FRAME_SIZE_SLIPPED (f);
}

static void
gtk_update_frame_menubars (struct frame *f)
{
  GtkWidget *menubar = NULL;

  assert (FRAME_GTK_P (f));

  menubar = FRAME_GTK_MENUBAR_WIDGET (f);

  if ((GTK_MENU_SHELL (menubar)->active) ||
      (GTK_MENU_SHELL (menubar)->have_grab) ||
      (GTK_MENU_SHELL (menubar)->have_xgrab))
    {
      return;
    }
 
  gtk_update_frame_menubar_internal (f);
}

static void
gtk_free_frame_menubars (struct frame *f)
{
  GtkWidget *menubar_widget;

  assert (FRAME_GTK_P (f));

  menubar_widget = FRAME_GTK_MENUBAR_WIDGET (f);
  if (menubar_widget)
    {
      gtk_widget_destroy (menubar_widget);
    }
}

static void 
popdown_menu_cb (GtkMenuShell *UNUSED (menu), gpointer UNUSED (user_data))
{
  popup_up_p--;
}

static void
gtk_popup_menu (Lisp_Object menu_desc, Lisp_Object event)
{
  struct Lisp_Event *eev = NULL;
  GtkWidget *widget = NULL;
  GtkWidget *menu = NULL;
  gpointer id = NULL;

  /* Do basic error checking first... */
  if (SYMBOLP (menu_desc))
    menu_desc = Fsymbol_value (menu_desc);
  CHECK_CONS (menu_desc);
  CHECK_STRING (XCAR (menu_desc));

  /* Now lets get down to business... */
  widget = menu_descriptor_to_widget (menu_desc, NULL);
  menu = GTK_MENU_ITEM (widget)->submenu;
  gtk_widget_set_name (widget, "XEmacsPopupMenu");
  id = g_object_get_qdata (G_OBJECT (widget), XEMACS_MENU_GUIID_TAG);

  __activate_menu (GTK_MENU_ITEM (widget), id);

  if (!NILP (event))
    {
      CHECK_LIVE_EVENT (event);
      eev = XEVENT (event);

      if ((eev->event_type != button_press_event) &&
	  (eev->event_type != button_release_event))
	wrong_type_argument (Qmouse_event_p, event);
    }
  else if (!NILP (Vthis_command_keys))
    {
      /* If an event wasn't passed, use the last event of the event
         sequence currently being executed, if that event is a mouse
         event. */
      eev = XEVENT (Vthis_command_keys);
      if ((eev->event_type != button_press_event) &&
	  (eev->event_type != button_release_event))
	eev = NULL;
    }

  gtk_widget_show (menu);

  popup_up_p++;
  assert (g_signal_connect (G_OBJECT (menu), "deactivate",
                            GTK_SIGNAL_FUNC (popdown_menu_cb), NULL));
		      
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  eev ? EVENT_BUTTON_BUTTON (eev) : 0,
                  eev ? eev->timestamp : GDK_CURRENT_TIME);
}

DEFUN ("gtk-build-xemacs-menu", Fgtk_build_xemacs_menu, 1, 1, 0, /*
Returns a GTK menu item from MENU, a standard XEmacs menu description.
See the definition of `popup-menu' for more information on the format of MENU.
*/
       (menu))
{
  GtkWidget *w = menu_descriptor_to_widget (menu, NULL);

  return (w ? build_gtk_object (G_OBJECT (w)) : Qnil);
}


void
syms_of_menubar_gtk (void)
{
  DEFSUBR (Fgtk_build_xemacs_menu);
}

void
console_type_create_menubar_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, update_frame_menubars);
  CONSOLE_HAS_METHOD (gtk, free_frame_menubars);
  CONSOLE_HAS_METHOD (gtk, popup_menu);
}

void
reinit_vars_of_menubar_gtk (void)
{
}

void
vars_of_menubar_gtk (void)
{
  Fprovide (intern ("gtk-menubars"));
}

/*---------------------------------------------------------------------------*/

