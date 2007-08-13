/* Implements an elisp-programmable menubar -- X interface.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

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

#include "console-x.h"
#include "EmacsManager.h"
#include "EmacsFrame.h"
#include "EmacsShell.h"
#include "gui-x.h"

#include "buffer.h"
#include "commands.h"           /* zmacs_regions */
#include "events.h"
#include "frame.h"
#include "opaque.h"
#include "window.h"

static int set_frame_menubar (struct frame *f,
			      int deep_p,
			      int first_time_p);

#define FRAME_MENUBAR_DATA(frame) ((frame)->menubar_data)
#define XFRAME_MENUBAR_DATA(frame) XPOPUP_DATA ((frame)->menubar_data)

#define MENUBAR_TYPE	0
#define SUBMENU_TYPE	1
#define POPUP_TYPE	2


/* Converting Lisp menu tree descriptions to lwlib's `widget_value' form.

   menu_item_descriptor_to_widget_value() converts a lisp description of a
   menubar into a tree of widget_value structures.  It allocates widget_values
   with malloc_widget_value() and allocates other storage only for the `key'
   slot.  All other slots are filled with pointers to Lisp_String data.  We
   allocate a widget_value description of the menu or menubar, and hand it to
   lwlib, which then makes a copy of it, which it manages internally.  We then
   immediately free our widget_value tree; it will not be referenced again.

   Incremental menu construction callbacks operate just a bit differently.
   They allocate widget_values and call replace_widget_value_tree() to tell
   lwlib to destructively modify the incremental stub (subtree) of its
   separate widget_value tree.

   This function is highly recursive (it follows the menu trees) and may call
   eval.  The reason we keep pointers to lisp string data instead of copying
   it and freeing it later is to avoid the speed penalty that would entail
   (since this needs to be fast, in the simple cases at least).  (The reason
   we malloc/free the keys slot is because there's not a lisp string around
   for us to use in that case.)

   Since we keep pointers to lisp strings, and we call eval, we could lose if
   GC relocates (or frees) those strings.  It's not easy to gc protect the
   strings because of the recursive nature of this function, and the fact that
   it returns a data structure that gets freed later.  So...  we do the
   sleaziest thing possible and inhibit GC for the duration.  This is probably
   not a big deal...

   We do not have to worry about the pointers to Lisp_String data after
   this function successfully finishes.  lwlib copies all such data with
   strdup().  */

static widget_value *
menu_item_descriptor_to_widget_value_1 (Lisp_Object desc, 
					int menu_type, int deep_p,
					int filter_p,
					int depth)
{
  /* This function cannot GC.
     It is only called from menu_item_descriptor_to_widget_value, which
     prohibits GC. */
  /* !!#### This function has not been Mule-ized */
  int menubar_root_p = (menu_type == MENUBAR_TYPE && depth == 0);
  widget_value *wv;
  Lisp_Object wv_closure;
  int count = specpdl_depth ();
  int partition_seen = 0;

  wv = xmalloc_widget_value ();

  wv_closure = make_opaque_ptr (wv);
  record_unwind_protect (widget_value_unwind, wv_closure);

  if (STRINGP (desc))
    {
      char *string_chars = (char *) XSTRING_DATA (desc);
      wv->type = (separator_string_p (string_chars) ? SEPARATOR_TYPE :
		  TEXT_TYPE);
#if 1
      /* #### - should internationalize with X resources instead.
         Not so! --ben */
      string_chars = GETTEXT (string_chars);
#endif
      if (wv->type == SEPARATOR_TYPE)
	{
	  wv->value = menu_separator_style (string_chars);
	} 
      else
	{
	  wv->name = string_chars;
	  wv->enabled = 1;
	}
    }
  else if (VECTORP (desc))
    {
      if (!button_item_to_widget_value (desc, wv, 1,
					(menu_type == MENUBAR_TYPE
					 && depth <= 1)))
	{
	  /* :included form was nil */
	  wv = NULL;
	  goto menu_item_done;
	}
    }
  else if (CONSP (desc))
    {
      Lisp_Object incremental_data = desc;
      widget_value *prev = 0;

      if (STRINGP (XCAR (desc)))
	{
	  Lisp_Object key, val;
	  Lisp_Object include_p, hook_fn = Qnil, config_tag = Qnil;
	  int included_spec = 0;
	  wv->type = CASCADE_TYPE;
	  wv->enabled = 1;
	  wv->name = (char *) XSTRING_DATA (LISP_GETTEXT (XCAR (desc)));
	  desc = Fcdr (desc);

	  while (key = Fcar (desc), KEYWORDP (key))
	    {
	      Lisp_Object cascade = desc;
	      desc = Fcdr (desc);
	      if (NILP (desc))
		signal_simple_error ("keyword in menu lacks a value",
				     cascade); 
	      val = Fcar (desc);
	      desc = Fcdr (desc);
	      if (EQ (key, Q_included))
		include_p = val, included_spec = 1;
	      else if (EQ (key, Q_config))
		config_tag = val;
	      else if (EQ (key, Q_filter))
		hook_fn = val;
	      else 
		signal_simple_error ("unknown menu cascade keyword", cascade);
	    }

	  if ((!NILP (config_tag)
	       && NILP (Fmemq (config_tag, Vmenubar_configuration)))
	      || (included_spec && NILP (Feval (include_p))))
	    {
	      wv = NULL;
	      goto menu_item_done;
	    }
	  if (!NILP (hook_fn))
	    {
#ifdef LWLIB_MENUBARS_LUCID
	      if (filter_p || depth == 0)
		{
#endif 
		  desc = call1_trapping_errors ("Error in menubar filter",
						hook_fn, desc);
		  if (UNBOUNDP (desc))
		    desc = Qnil;
#ifdef LWLIB_MENUBARS_LUCID
		}
	      else
		{
		  widget_value *incr_wv = xmalloc_widget_value ();
		  wv->contents = incr_wv;
		  incr_wv->type = INCREMENTAL_TYPE;
		  incr_wv->enabled = 1;
		  incr_wv->name = wv->name;
		  /* This is automatically GC protected through
		     the call to lw_map_widget_values(); no need
		     to worry. */
		  incr_wv->call_data = LISP_TO_VOID (incremental_data);
		  goto menu_item_done;
		}
#endif /* LWLIB_MENUBARS_LUCID */
	    }
	  if (menu_type == POPUP_TYPE && popup_menu_titles && depth == 0)
	    {
	      /* Simply prepend three more widget values to the contents of
		 the menu: a label, and two separators (to get a double
		 line). */
	      widget_value *title_wv = xmalloc_widget_value ();
	      widget_value *sep_wv = xmalloc_widget_value ();
	      title_wv->type = TEXT_TYPE;
	      title_wv->name = wv->name;
	      title_wv->enabled = 1;
	      title_wv->next = sep_wv;
	      sep_wv->type = SEPARATOR_TYPE;
	      sep_wv->value = menu_separator_style ("==");
	      sep_wv->next = 0;
		  
	      wv->contents = title_wv;
	      prev = sep_wv;
	    }
	}
      else if (menubar_root_p)
	{
	  wv->name = (char *) "menubar";
	  wv->type = CASCADE_TYPE; /* Well, nothing else seems to fit and
				      this is ignored anyway...  */
	}
      else
	{
	  signal_simple_error ("menu name (first element) must be a string",
                               desc);
	}

      wv->enabled = 1;
      if (deep_p || menubar_root_p)
	{
	  widget_value *next;
	  for (; !NILP (desc); desc = Fcdr (desc))
	    {
	      Lisp_Object child = Fcar (desc);
	      if (menubar_root_p && NILP (child))	/* the partition */
		{
		  if (partition_seen)
		    error (
		     "more than one partition (nil) in menubar description");
		  partition_seen = 1;
		  next = xmalloc_widget_value ();
		  next->type = PUSHRIGHT_TYPE;
		}
	      else
		{
		  next = menu_item_descriptor_to_widget_value_1
		    (child, menu_type, deep_p, filter_p, depth + 1);
		}
	      if (! next)
		continue;
	      else if (prev)
		prev->next = next;
	      else
		wv->contents = next;
	      prev = next;
	    }
	}
      if (deep_p && !wv->contents)
	wv = NULL;
    }
  else if (NILP (desc))
    error ("nil may not appear in menu descriptions");
  else
    signal_simple_error ("unrecognized menu descriptor", desc);

menu_item_done:

  if (wv)
    {
      /* Completed normally.  Clear out the object that widget_value_unwind()
	 will be called with to tell it not to free the wv (as we are
	 returning it.) */
      set_opaque_ptr (wv_closure, 0);
    }

  unbind_to (count, Qnil);
  return wv;
}

static widget_value *
menu_item_descriptor_to_widget_value (Lisp_Object desc, 
				      int menu_type, /* if this is a menubar,
						     popup or sub menu */
				      int deep_p,    /*  */
				      int filter_p)  /* if :filter forms
							should run now */ 
{
  widget_value *wv;
  int count = specpdl_depth ();
  record_unwind_protect (restore_gc_inhibit,
			 make_int (gc_currently_forbidden));
  gc_currently_forbidden = 1;
  /* Can't GC! */
  wv = menu_item_descriptor_to_widget_value_1 (desc, menu_type, deep_p,
					       filter_p, 0);
  unbind_to (count, Qnil);
  return wv;
}


#ifdef LWLIB_MENUBARS_LUCID
int in_menu_callback;

Lisp_Object
restore_in_menu_callback(Lisp_Object val)
{
    in_menu_callback = XINT(val);
    return Qnil;
}
#endif /* LWLIB_MENUBARS_LUCID */


/* The order in which callbacks are run is funny to say the least.
   It's sometimes tricky to avoid running a callback twice, and to
   avoid returning prematurely.  So, this function returns true
   if the menu's callbacks are no longer gc protected.  So long
   as we unprotect them before allowing other callbacks to run,
   everything should be ok.

   The pre_activate_callback() *IS* intentionally called multiple times.
   If client_data == NULL, then it's being called before the menu is posted.
   If client_data != NULL, then client_data is a (widget_value *) and
   client_data->data is a Lisp_Object pointing to a lisp submenu description
   that must be converted into widget_values.  *client_data is destructively
   modified. 

   #### Stig thinks that there may be a GC problem here due to the
   fact that pre_activate_callback() is called multiple times, but I
   think he's wrong.

   */

static void
pre_activate_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  /* This function can GC */
  struct gcpro gcpro1;
  struct device *d = get_device_from_display (XtDisplay (widget));
  struct frame *f = x_any_window_to_frame (d, XtWindow (widget));
  Lisp_Object rest = Qnil;
  Lisp_Object frame;
  int any_changes = 0;
  int count;

  if (!f)
    f = x_any_window_to_frame (d, XtWindow (XtParent (widget)));
  if (!f)
    return;

  /* make sure f is the selected frame */
  XSETFRAME (frame, f);
  Fselect_frame (frame);

  if (client_data)
    {
      /* this is an incremental menu construction callback */
      widget_value *hack_wv = (widget_value *) client_data;
      Lisp_Object submenu_desc;
      widget_value *wv;

      assert (hack_wv->type == INCREMENTAL_TYPE);
      VOID_TO_LISP (submenu_desc, hack_wv->call_data);

      /*
       * #### Fix the menu code so this isn't necessary.
       *
       * Protect against reentering the menu code otherwise we will
       * crash later when the code gets confused at the state
       * changes.
       */
      count = specpdl_depth ();
      record_unwind_protect (restore_in_menu_callback,
			     make_int (in_menu_callback));
      in_menu_callback = 1;
      wv = menu_item_descriptor_to_widget_value (submenu_desc, SUBMENU_TYPE,
						 1, 0);
      unbind_to (count, Qnil);

      if (!wv)
	{
	  wv = xmalloc_widget_value ();
	  wv->type = CASCADE_TYPE;
	  wv->next = NULL;
	  wv->contents = xmalloc_widget_value ();
	  wv->contents->type = TEXT_TYPE;
	  wv->contents->name = (char *) "No menu";
	  wv->contents->next = NULL;
	}
      assert (wv && wv->type == CASCADE_TYPE && wv->contents);
      replace_widget_value_tree (hack_wv, wv->contents);
      free_popup_widget_value_tree (wv);
    }
  else
    {
      if (!POPUP_DATAP (FRAME_MENUBAR_DATA (f)))
	return;
      /* #### - this menubar update mechanism is expensively anti-social and
	 the activate-menubar-hook is now mostly obsolete. */
      /* make the activate-menubar-hook be a list of functions, not a single
	 function, just to simplify things. */
      if (!NILP (Vactivate_menubar_hook) &&
	  (!CONSP (Vactivate_menubar_hook) ||
	   EQ (XCAR (Vactivate_menubar_hook), Qlambda)))
	Vactivate_menubar_hook = Fcons (Vactivate_menubar_hook, Qnil);
      
      GCPRO1 (rest);
      for (rest = Vactivate_menubar_hook; !NILP (rest); rest = Fcdr (rest))
	if (!EQ (call0 (XCAR (rest)), Qt))
	  any_changes = 1;
#if 0
      /* #### - It is necessary to *ALWAYS* call set_frame_menubar() now that
	 incremental menus are implemented.  If a subtree of a menu has been
	 updated incrementally (a destructive operation), then that subtree
	 must somehow be wiped.

	 It is difficult to undo the destructive operation in lwlib because
	 a pointer back to lisp data needs to be hidden away somewhere.  So
	 that an INCREMENTAL_TYPE widget_value can be recreated...  Hmmmmm. */
      if (any_changes ||
	  !XFRAME_MENUBAR_DATA (f)->menubar_contents_up_to_date)
#endif 
	set_frame_menubar (f, 1, 0);
      UNGCPRO;
    }
}

#ifdef ENERGIZE
extern int *get_psheets_for_buffer (Lisp_Object, int *);

static void
set_panel_button_sensitivity (struct frame *f, widget_value *data)
{
  struct window *window = XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f));
  int current_buffer_psheets_count = 0;
  int *current_buffer_psheets =
    get_psheets_for_buffer (window->buffer, &current_buffer_psheets_count);
  int panel_enabled = FRAME_X_DESIRED_PSHEETS (f) ||
    current_buffer_psheets_count;
  widget_value *val;
  for (val = data->contents; val; val = val->next)
    if (val->name && !strcmp (val->name, "sheet"))
      {
	val->enabled = panel_enabled;
	return;
      }
}
#endif /* ENERGIZE */

static widget_value *
compute_menubar_data (struct frame *f, Lisp_Object menubar, int deep_p)
{
  widget_value *data;

  if (NILP (menubar))
    data = 0;
  else
    {
      Lisp_Object old_buffer;
      int count = specpdl_depth ();

      old_buffer = Fcurrent_buffer ();
      record_unwind_protect (Fset_buffer, old_buffer);
      Fset_buffer ( XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer);
      data = menu_item_descriptor_to_widget_value (menubar, MENUBAR_TYPE,
						   deep_p, 0);
#ifdef ENERGIZE
      if (data)
	set_panel_button_sensitivity (f, data);
#endif
      Fset_buffer (old_buffer);
      unbind_to (count, Qnil);
    }
  return data;
}

static int
set_frame_menubar (struct frame *f, int deep_p, int first_time_p)
{
  widget_value *data;
  Lisp_Object menubar;
  int menubar_visible;
  long id;
  /* As for the toolbar, the minibuffer does not have its own menubar. */
  struct window *w = XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f));

  if (! FRAME_X_P (f))
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
    menubar_visible = !NILP (w->menubar_visible_p);

  data = compute_menubar_data (f, menubar, deep_p);
  if (!data || (!data->next && !data->contents))
    abort ();
  
  if (NILP (FRAME_MENUBAR_DATA (f)))
    {
      struct popup_data *mdata =
	alloc_lcrecord (sizeof (struct popup_data), lrecord_popup_data);

      mdata->id = new_lwlib_id ();
      mdata->last_menubar_buffer = Qnil;
      mdata->menubar_contents_up_to_date = 0;
      XSETPOPUP_DATA (FRAME_MENUBAR_DATA (f), mdata);
    }

  /***** now store into the menubar widget, creating it if necessary *****/

  id = XFRAME_MENUBAR_DATA (f)->id;
  if (!FRAME_X_MENUBAR_WIDGET (f))
    {
      Widget parent = FRAME_X_CONTAINER_WIDGET (f);

      assert (first_time_p);

      /* It's the first time we've mapped the menubar so compute its
	 contents completely once.  This makes sure that the menubar
	 components are created with the right type. */
      if (!deep_p)
	{
	  free_popup_widget_value_tree (data);
	  data = compute_menubar_data (f, menubar, 1);
	}


      FRAME_X_MENUBAR_WIDGET (f) =
	lw_create_widget ("menubar", "menubar", id, data, parent,
			  0, pre_activate_callback,
			  popup_selection_callback, 0);

    }
  else
    {
      lw_modify_all_widgets (id, data, deep_p ? True : False);
    }
  free_popup_widget_value_tree (data);
	  
  XFRAME_MENUBAR_DATA (f)->menubar_contents_up_to_date = deep_p;
  XFRAME_MENUBAR_DATA (f)->last_menubar_buffer =
    XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->buffer;
  return menubar_visible;
}


/* Called from x_create_widgets() to create the inital menubar of a frame
   before it is mapped, so that the window is mapped with the menubar already
   there instead of us tacking it on later and thrashing the window after it
   is visible. */
int
x_initialize_frame_menubar (struct frame *f)
{
  return set_frame_menubar (f, 1, 1);
}


static LWLIB_ID last_popup_menu_selection_callback_id;

static void
popup_menu_selection_callback (Widget widget, LWLIB_ID id,
			       XtPointer client_data)
{
  last_popup_menu_selection_callback_id = id;
  popup_selection_callback (widget, id, client_data);
  /* lw_destroy_all_widgets() will be called from popup_down_callback() */
}

static void
popup_menu_down_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  if (popup_handled_p (id))
    return;
  assert (popup_up_p != 0);
  ungcpro_popup_callbacks (id);
  popup_up_p--;
  /* if this isn't called immediately after the selection callback, then
     there wasn't a menu selection. */
  if (id != last_popup_menu_selection_callback_id)
    popup_selection_callback (widget, id, (XtPointer) -1);
  lw_destroy_all_widgets (id);
}


static void
make_dummy_xbutton_event (XEvent *dummy,
			  Widget daddy,	
			  struct Lisp_Event *eev)
     /* NULL for eev means query pointer */
{
  XButtonPressedEvent *btn = (XButtonPressedEvent *) dummy;

  btn->type = ButtonPress;
  btn->serial = 0;
  btn->send_event = 0;
  btn->display = XtDisplay (daddy);
  btn->window = XtWindow (daddy);
  if (eev)
    {
      Position shellx, shelly, framex, framey;
      Widget shell = XtParent (daddy);
      btn->time = eev->timestamp;
      btn->button = eev->event.button.button;
      btn->root = RootWindowOfScreen (XtScreen (daddy));
      btn->subwindow = (Window) NULL;
      btn->x = eev->event.button.x;
      btn->y = eev->event.button.y;
      XtVaGetValues (shell, XtNx, &shellx, XtNy, &shelly, NULL);
      XtVaGetValues (daddy, XtNx, &framex, XtNy, &framey, NULL);
      btn->x_root = shellx + framex + btn->x;
      btn->y_root = shelly + framey + btn->y;;
      btn->state = ButtonPressMask; /* all buttons pressed */
    }
  else
    {
      /* CurrentTime is just ZERO, so it's worthless for
	 determining relative click times. */
      struct device *d = get_device_from_display (XtDisplay (daddy));
      btn->time = DEVICE_X_MOUSE_TIMESTAMP (d); /* event-Xt maintains this */
      btn->button = 0;
      XQueryPointer (btn->display, btn->window, &btn->root,
		     &btn->subwindow, &btn->x_root, &btn->y_root,
		     &btn->x, &btn->y, &btn->state);
    }
}


#ifdef ENERGIZE
extern int desired_debuggerpanel_exposed_p;
extern int current_debuggerpanel_exposed_p;
extern int debuggerpanel_sheet;
extern void notify_energize_sheet_hidden (unsigned long);
#endif

static void
x_update_frame_menubar_internal (struct frame *f)
{
  /* We assume the menubar contents has changed if the global flag is set,
     or if the current buffer has changed, or if the menubar has never
     been updated before.
   */
  int menubar_contents_changed =
    (f->menubar_changed
     || NILP (FRAME_MENUBAR_DATA (f))
     || (!EQ (XFRAME_MENUBAR_DATA (f)->last_menubar_buffer,
	      XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->buffer)));

  Boolean menubar_was_visible = XtIsManaged (FRAME_X_MENUBAR_WIDGET (f));
  Boolean menubar_will_be_visible = menubar_was_visible;
  Boolean menubar_visibility_changed;
  Cardinal new_num_top_widgets = 1; /* for the menubar */
  Widget container = FRAME_X_CONTAINER_WIDGET (f);
  
#ifdef ENERGIZE
  int *old_sheets = FRAME_X_CURRENT_PSHEETS (f);
  int *new_sheets = FRAME_X_DESIRED_PSHEETS (f);
  int old_count = FRAME_X_CURRENT_PSHEET_COUNT (f);
  int new_count = FRAME_X_DESIRED_PSHEET_COUNT (f);
  Lisp_Object old_buf = FRAME_X_CURRENT_PSHEET_BUFFER (f);
  Lisp_Object new_buf = FRAME_X_DESIRED_PSHEET_BUFFER (f);
  int psheets_changed = (old_sheets != new_sheets
			 || old_count != new_count
			 || !EQ (old_buf, new_buf));
  int debuggerpanel_changed = (desired_debuggerpanel_exposed_p
			       != current_debuggerpanel_exposed_p);

  if (desired_debuggerpanel_exposed_p && FRAME_X_TOP_WIDGETS (f) [1] == 0)
    /* This happens when the frame was just created. */
    debuggerpanel_changed = 1;

  FRAME_X_CURRENT_PSHEETS (f) = FRAME_X_DESIRED_PSHEETS (f);
  FRAME_X_CURRENT_PSHEET_COUNT (f) = FRAME_X_DESIRED_PSHEET_COUNT (f);
  FRAME_X_CURRENT_PSHEET_BUFFER (f) = FRAME_X_DESIRED_PSHEET_BUFFER (f);
#endif /* ENERGIZE */

  if (menubar_contents_changed)
    menubar_will_be_visible = set_frame_menubar (f, 0, 0);

  menubar_visibility_changed = menubar_was_visible != menubar_will_be_visible;

  if (! (menubar_visibility_changed
#ifdef ENERGIZE
	 || psheets_changed || debuggerpanel_changed
#endif
	 ))
    return;


  /* Set menubar visibility */
  if (menubar_visibility_changed)
    (menubar_will_be_visible ? XtManageChild : XtUnmanageChild)
      (FRAME_X_MENUBAR_WIDGET (f));
		 

#ifdef ENERGIZE
  /* Set debugger panel visibility */
  if (debuggerpanel_changed)
    {
      Widget w;
      int sheet = debuggerpanel_sheet;

      w = lw_get_widget (sheet, container, 0);
      if (desired_debuggerpanel_exposed_p)
	{
	  if (! w)
	    w = lw_make_widget (sheet, container, 0);
	  FRAME_X_TOP_WIDGETS (f)[1] = w;
	  XtManageChild (w);
	}
      else
	{
	  notify_energize_sheet_hidden (sheet);
	  if (w)
	    XtUnmanageChild (w);
	}
    }

  /* Set psheet visibility.  For the moment we just unmanage all the old
   ones, and then manage all the new ones.  If the number of psheets
   ever becomes a large number (i.e. > 1), then we can worry about a
   more sophisticated way of doing this. */
  if (psheets_changed)
    {
      int i;
      Widget w;
      unsigned long sheet;

      for (i=0; i<old_count; i++)
	{
	  sheet = old_sheets[i];
	  w = lw_get_widget (sheet, container, 0);
	  notify_energize_sheet_hidden (sheet);
	  if (w)
	    XtUnmanageChild (w);
	}

      for (i=0; i<new_count; i++)
	{
	  sheet = new_sheets[i];
	  /* #### This unconditional call to lw_make_widget() is a bad
	     idea.  Doesn't it cause a memory leak if the widget
	     already exists?

	     #### How does Energize know that a sheet just got displayed?
	     #### Energize knows all.  */
	  w = lw_make_widget (sheet, container, 0);
	  FRAME_X_TOP_WIDGETS (f)[2+i] = w;
	  XtManageChild (w);
	}
    }

  new_num_top_widgets += 1+new_count;
#endif /* ENERGIZE */

  /* Note that new_num_top_widgets doesn't need to reflect the actual
     number of top widgets, but just the limit of FRAME_X_TOP_WIDGETS (f)[]. */
  FRAME_X_NUM_TOP_WIDGETS (f) = new_num_top_widgets;
  {
    /* We want to end up as close in size as possible to what we
       were before.  So, ask the EmacsManager what size it wants
       to be (suggesting the current size), and resize it to that
       size.  It in turn will call our query-geometry callback,
       which will round the size to something that exactly fits
       the text widget. */
    XtWidgetGeometry req, repl;

    req.request_mode = CWWidth | CWHeight;
    XtVaGetValues (container,
		   XtNwidth, &req.width,
		   XtNheight, &req.height,
		   0);
    XtQueryGeometry (container, &req, &repl);
    EmacsManagerChangeSize (container, repl.width,
			    repl.height);
    /* The window size might not have changed but the text size
       did; thus, the base size might be incorrect.  So update
       it. */
    EmacsShellUpdateSizeHints (FRAME_X_SHELL_WIDGET (f));
  }

#ifdef ENERGIZE
  /* Give back the focus to emacs if no psheets are displayed anymore */
  if (psheets_changed)
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      Fselect_frame (frame);
    }
#endif /* ENERGIZE */
}

static void
x_update_frame_menubars (struct frame *f)
{
  assert (FRAME_X_P (f));

  x_update_frame_menubar_internal (f);

  /* #### This isn't going to work right now that this function works on
     a per-frame, not per-device basis.  Guess what?  I don't care. */
#ifdef ENERGIZE
  current_debuggerpanel_exposed_p = desired_debuggerpanel_exposed_p;
#endif
}

static void
x_free_frame_menubars (struct frame *f)
{
  Widget menubar_widget;

  assert (FRAME_X_P (f));
  
  menubar_widget = FRAME_X_MENUBAR_WIDGET (f);
  if (menubar_widget)
    {
      LWLIB_ID id = XFRAME_MENUBAR_DATA (f)->id;
      lw_destroy_all_widgets (id);
      XFRAME_MENUBAR_DATA (f)->id = 0;
    }

#ifdef ENERGIZE
  {
    /* Also destroy this frame's psheets */
    Widget parent = FRAME_X_CONTAINER_WIDGET (f);
    int *sheets = FRAME_X_CURRENT_PSHEETS (f);
    int i = FRAME_X_CURRENT_PSHEET_COUNT (f);
    while (i--)
      {
	unsigned long sheet = sheets [i];
	Widget w = lw_get_widget (sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
    FRAME_X_CURRENT_PSHEET_COUNT (f) = 0;

    /* Is this necessary? */
    sheets = FRAME_X_DESIRED_PSHEETS (f);
    i = FRAME_X_DESIRED_PSHEET_COUNT (f);
    while (i--)
      {
	unsigned long sheet = sheets [i];
	Widget w = lw_get_widget (sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
    FRAME_X_DESIRED_PSHEET_COUNT (f) = 0;

    /* sigh... debugger panel is special... */
    if (debuggerpanel_sheet)
      {
	Widget w = lw_get_widget (debuggerpanel_sheet, parent, 0);
	if (w)
	  lw_destroy_widget (w);
      }
  }
#endif /* ENERGIZE */
}

static void
x_popup_menu (Lisp_Object menu_desc, Lisp_Object event)
{
  int menu_id;
  struct frame *f = selected_frame ();
  widget_value *data;
  Widget parent;
  Widget menu;
  struct Lisp_Event *eev = NULL;
  XEvent xev;
  Lisp_Object frame = Qnil;

  XSETFRAME (frame, f);
  CHECK_X_FRAME (frame);
  parent = FRAME_X_SHELL_WIDGET (f);

  if (!NILP (event))
    {
      CHECK_LIVE_EVENT (event);
      eev= XEVENT (event);
      if (eev->event_type != button_press_event
	  && eev->event_type != button_release_event)
	wrong_type_argument (Qmouse_event_p, event);
    }
  else if (!NILP (Vthis_command_keys))
    {
      /* if an event wasn't passed, use the last event of the event sequence
	 currently being executed, if that event is a mouse event */
      eev = XEVENT (Vthis_command_keys); /* last event first */
      if (eev->event_type != button_press_event
	  && eev->event_type != button_release_event)
	eev = NULL;
    }
  make_dummy_xbutton_event (&xev, parent, eev);

  if (SYMBOLP (menu_desc))
    menu_desc = Fsymbol_value (menu_desc);
  CHECK_CONS (menu_desc);
  CHECK_STRING (XCAR (menu_desc));
  data = menu_item_descriptor_to_widget_value (menu_desc, POPUP_TYPE, 1, 1);

  if (! data) error ("no menu");
  
  menu_id = new_lwlib_id ();
  menu = lw_create_widget ("popup", "popup" /* data->name */, menu_id, data,
			   parent, 1, 0,
			   popup_menu_selection_callback,
			   popup_menu_down_callback);
  free_popup_widget_value_tree (data);

  gcpro_popup_callbacks (menu_id);

  /* Setting zmacs-region-stays is necessary here because executing a command
     from a menu is really a two-command process: the first command (bound to
     the button-click) simply pops up the menu, and returns.  This causes a
     sequence of magic-events (destined for the popup-menu widget) to begin.
     Eventually, a menu item is selected, and a menu-event blip is pushed onto
     the end of the input stream, which is then executed by the event loop.
     
     So there are two command-events, with a bunch of magic-events between
     them.  We don't want the *first* command event to alter the state of the
     region, so that the region can be available as an argument for the second
     command.
   */
  if (zmacs_regions)
    zmacs_region_stays = 1;

  popup_up_p++;
  lw_popup_menu (menu, &xev);
  /* this speeds up display of pop-up menus */
  XFlush (XtDisplay (parent));
}


void
syms_of_menubar_x (void)
{
}

void
console_type_create_menubar_x (void)
{
  CONSOLE_HAS_METHOD (x, update_frame_menubars);
  CONSOLE_HAS_METHOD (x, free_frame_menubars);
  CONSOLE_HAS_METHOD (x, popup_menu);
}

void
vars_of_menubar_x (void)
{
  last_popup_menu_selection_callback_id = -1;

#if defined (LWLIB_MENUBARS_LUCID)
  Fprovide (intern ("lucid-menubars"));
#elif defined (LWLIB_MENUBARS_MOTIF)
  Fprovide (intern ("motif-menubars"));
#elif defined (LWLIB_MENUBARS_ATHENA)
  Fprovide (intern ("athena-menubars"));
#endif
}
