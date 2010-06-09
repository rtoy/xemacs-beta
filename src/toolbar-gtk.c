/* toolbar implementation -- GTK interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.

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

#include <config.h>
#include "lisp.h"

#include "frame.h"
#include "frame-impl.h"
#include "glyphs.h"
#include "glyphs-gtk.h"
#include "window.h"
#include "window-impl.h"

#include "toolbar.h"
#include "toolbar-xlike.h"

#include "console-gtk-impl.h"

/* We should really create a 'xlike' console type and fill it with
** all the shared code.  We would then just use
** CONSOLE_INHERITS_METHOD(x,xlike,blah)
*/
/* #define gtk_output_frame_toolbars xlike_output_frame_toolbars */
/* #define gtk_output_toolbar_button xlike_output_toolbar_button */
#define gtk_redraw_exposed_toolbars xlike_redraw_exposed_toolbars
#define gtk_redraw_frame_toolbars xlike_redraw_frame_toolbars
/* #define gtk_clear_frame_toolbars xlike_clear_frame_toolbars */


static void
gtk_initialize_frame_toolbars (struct frame *UNUSED (f))
{
  stderr_out ("gtk_initialize_frame_toolbars\n");
}

/* This only calls one function but we go ahead and create this in
   case we ever do decide that we need to do more work. */
static void
gtk_free_frame_toolbars (struct frame *UNUSED (f))
{
  stderr_out ("gtk_free_frame_toolbars\n");
}

static void
gtk_toolbar_callback (GtkWidget *UNUSED (w), gpointer user_data)
{
  struct toolbar_button *tb = (struct toolbar_button *) user_data;

  call0 (tb->callback);
}

void
gtk_clear_toolbar (struct frame *f, enum edge_pos pos)
{
  FRAME_GTK_TOOLBAR_CHECKSUM (f, pos) = 0;
  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 0);
  if (FRAME_GTK_TOOLBAR_WIDGET(f)[pos])
    gtk_widget_destroy ((GtkWidget *)FRAME_GTK_TOOLBAR_WIDGET(f)[pos]);
}

void
gtk_clear_frame_toolbars (struct frame *f)
{
  enum edge_pos pos;

  EDGE_POS_LOOP (pos)
    {
      gtk_clear_toolbar (f, pos);
    }
}

static void
gtk_output_toolbar (struct frame *f, enum edge_pos pos)
{
  GtkToolbar *toolbar;
  Lisp_Object button, window, glyph, instance;
  unsigned int checksum = 0;
  struct window *w;
  int x, y, bar_width, bar_height, vert;

  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 0);
	
  /* Get the toolbar and delete the old widgets in it */
  button = FRAME_TOOLBAR_BUTTONS (f, pos);
	
  /* First loop over all of the buttons to determine how many there
     are. This loop will also make sure that all instances are
     instantiated so when we actually output them they will come up
     immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      checksum = HASH4 (checksum,
			internal_hash (get_toolbar_button_glyph(w, tb), 0, 0),
			internal_hash (tb->callback, 0, 0),
			0 /* width */);
      button = tb->next;
    }

  /* Only do updates if the toolbar has changed, or this is the first
     time we have drawn it in this position
  */
  if (FRAME_GTK_TOOLBAR_WIDGET (f)[pos] &&
      FRAME_GTK_TOOLBAR_CHECKSUM (f, pos) == checksum)
    {
      return;
    }

  /* Loop through buttons and add them to our toolbar.
     This code ignores the button dimensions as we let GTK handle that :)
     Attach the toolbar_button struct to the toolbar button so we know what
     function to use as a callback. */

  {
    /* gtk_clear_toolbar (f, pos); */
    FRAME_GTK_TOOLBAR_WIDGET (f)[pos] = toolbar
      = (GtkToolbar *)gtk_toolbar_new ();
    gtk_widget_show (GTK_WIDGET (toolbar));
    gtk_widget_set_name (GTK_WIDGET (toolbar), "toolbar");
    
    if (EDGE_HORIZONTAL_P (pos))
        gtk_orientable_set_orientation ((GtkOrientable *)toolbar,
                                        GTK_ORIENTATION_HORIZONTAL);
    else
        gtk_orientable_set_orientation ((GtkOrientable *)toolbar,
                                        GTK_ORIENTATION_VERTICAL);
    gtk_toolbar_set_show_arrow ((GtkToolbar *)toolbar, TRUE);
  }

  /* if (NILP (w->toolbar_buttons_captioned_p)) */
  gtk_toolbar_set_style (toolbar, GTK_TOOLBAR_ICONS);
  /*  else
      gtk_toolbar_set_style (toolbar, GTK_TOOLBAR_BOTH); */

  FRAME_GTK_TOOLBAR_CHECKSUM(f, pos) = checksum;
  button = FRAME_TOOLBAR_BUTTONS (f, pos);

  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);

      if (tb->blank)
	{
          GtkToolItem *item = gtk_separator_tool_item_new ();
          gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);
	}
      else
	{
          if (!NILP (w->toolbar_buttons_captioned_p))
            glyph = tb->cap_up_glyph;
          else
            glyph = tb->up_glyph;

	  /* #### It is currently possible for users to trash us by directly
	     changing the toolbar glyphs.  Avoid crashing in that case. */
	  if (GLYPHP (glyph))
	    instance = glyph_image_instance (glyph, window,
					     ERROR_ME_DEBUG_WARN, 1);
	  else
	    instance = Qnil;
	  
	  if (IMAGE_INSTANCEP(instance))
	    {
              GtkToolItem *item;
	      GtkWidget *pixmapwid;
	      GdkPixmap *pixmap;
              GdkBitmap *mask;
	      Ibyte *tooltip = NULL;

	      if (STRINGP (tb->help_string))
		tooltip = XSTRING_DATA (tb->help_string);

              pixmap = XIMAGE_INSTANCE_GTK_PIXMAP (instance);
              mask = XIMAGE_INSTANCE_GTK_MASK (instance);
	      pixmapwid = gtk_pixmap_new (pixmap, mask);

              item = gtk_tool_button_new (pixmapwid, "");

              /* item = gtk_tool_button_new_from_stock (GTK_STOCK_OPEN); */

	      gtk_toolbar_insert (GTK_TOOLBAR(toolbar), item, -1);
              gtk_tool_item_set_tooltip_text (item,
                                              LISP_STRING_TO_EXTERNAL (tb->help_string, Qctext));
              g_signal_connect (G_OBJECT (item), "clicked",
                                G_CALLBACK (gtk_toolbar_callback),
                                (gpointer) tb);
	    }
	}
      /* Who's idea was it to use a linked list for toolbar buttons? */
      button = tb->next;
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);

  /* Are these border widths? */
  x -= vert ? 3 : 2;
  y -= vert ? 2 : 3;

#ifdef HAVE_GNOME
  if (pos == TOP_EDGE && GNOME_IS_APP (FRAME_GTK_SHELL_WIDGET (f)))
    gnome_app_set_toolbar (GNOME_APP (FRAME_GTK_SHELL_WIDGET (f)),
                           GTK_TOOLBAR (FRAME_GTK_TOOLBAR_WIDGET (f)[pos]));
  else
#endif
    gtk_fixed_put (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)),
                   GTK_WIDGET (FRAME_GTK_TOOLBAR_WIDGET (f)[pos]), x, y);

  gtk_widget_show_all (GTK_WIDGET (FRAME_GTK_TOOLBAR_WIDGET (f)[pos]));
}

void
gtk_output_frame_toolbars (struct frame *f)
{
  enum edge_pos pos;

  EDGE_POS_LOOP (pos)
    {
      if (FRAME_REAL_TOOLBAR_VISIBLE (f, pos))
	gtk_output_toolbar (f, pos);
    }
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, output_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, clear_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, free_frame_toolbars);
  /* CONSOLE_HAS_METHOD (gtk, output_toolbar_button); */
  /* CONSOLE_HAS_METHOD (gtk, redraw_exposed_toolbars); */
  /* CONSOLE_HAS_METHOD (gtk, redraw_frame_toolbars); */
}

void
vars_of_toolbar_gtk (void)
{
  Fprovide (intern ("toolbar-gtk"));
}
