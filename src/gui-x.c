/* General GUI code -- X-specific. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* This file Mule-ized by Ben Wing, 7-8-00. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "events.h"
#include "frame.h"
#include "glyphs.h"
#include "gui.h"
#include "menubar.h"
#include "opaque.h"
#include "redisplay.h"

#include "console-x-impl.h"

#ifdef LWLIB_USES_MOTIF
#include "xmotif.h" /* for XmVersion */
#endif

/* we need a unique id for each popup menu, dialog box, and scrollbar */
static LWLIB_ID lwlib_id_tick;

LWLIB_ID
new_lwlib_id (void)
{
  lwlib_id_tick++;
  if (!lwlib_id_tick)
    lwlib_id_tick++;
  return lwlib_id_tick;
}

widget_value *
xmalloc_widget_value (void)
{
  widget_value *tmp = malloc_widget_value ();
  if (!tmp) memory_full ();
  return tmp;
}



/* This contains an alist of (id . protect-me) for GCPRO'ing the callbacks
   of the popup menus and dialog boxes. */
static Lisp_Object Vpopup_callbacks;

struct widget_value_mapper
{
  Lisp_Object protect_me;
};

static int
snarf_widget_value_mapper (widget_value *val, void *closure)
{
  struct widget_value_mapper *z = (struct widget_value_mapper *) closure;

  if (val->call_data)
    z->protect_me = Fcons (VOID_TO_LISP (val->call_data), z->protect_me);
  if (val->accel)
    z->protect_me = Fcons (VOID_TO_LISP (val->accel), z->protect_me);

  return 0;
}

/* Snarf the callbacks and other Lisp data that are hidden in the lwlib
   call-data and accel associated with id ID and return them for
   proper marking. */

static Lisp_Object
snarf_widget_values_for_gcpro (LWLIB_ID id)
{
  struct widget_value_mapper z;

  z.protect_me = Qnil;
  lw_map_widget_values (id, snarf_widget_value_mapper, &z);
  return z.protect_me;
}

/* Given an lwlib id ID associated with a widget tree, make sure that all
   Lisp callbacks in the tree are GC-protected.  This can be called
   multiple times on the same widget tree -- this should be done at
   creation time and each time the tree is modified. */

void
gcpro_popup_callbacks (LWLIB_ID id)
{
  Lisp_Object lid = make_int (id);
  Lisp_Object this = assq_no_quit (lid, Vpopup_callbacks);

  if (!NILP (this))
    {
      free_list (XCDR (this));
      XCDR (this) = snarf_widget_values_for_gcpro (id);
    }
  else
    Vpopup_callbacks = Fcons (Fcons (lid, snarf_widget_values_for_gcpro (id)),
			      Vpopup_callbacks);
}

/* Remove GC-protection from the just-destroyed widget tree associated
   with lwlib id ID. */

void
ungcpro_popup_callbacks (LWLIB_ID id)
{
  Lisp_Object lid = make_int (id);
  Lisp_Object this = assq_no_quit (lid, Vpopup_callbacks);

  assert (!NILP (this));
  free_list (XCDR (this));
  Vpopup_callbacks = delq_no_quit (this, Vpopup_callbacks);
}

int
popup_handled_p (LWLIB_ID id)
{
  return NILP (assq_no_quit (make_int (id), Vpopup_callbacks));
}

/* menu_item_descriptor_to_widget_value() et al. mallocs a
   widget_value, but then may signal lisp errors.  If an error does
   not occur, the opaque ptr we have here has had its pointer set to 0
   to tell us not to do anything.  Otherwise we free the widget value.
   (This has nothing to do with GC, it's just about not dropping
   pointers to malloc'd data when errors happen.) */

Lisp_Object
widget_value_unwind (Lisp_Object closure)
{
  widget_value *wv = (widget_value *) get_opaque_ptr (closure);
  free_opaque_ptr (closure);
  if (wv)
    free_widget_value_tree (wv);
  return Qnil;
}

#if 0
static void
print_widget_value (widget_value *wv, int depth)
{
  /* strings in wv are in external format; use printf not stdout_out
     because the latter takes internal-format strings */
  Extbyte d [200];
  int i;
  for (i = 0; i < depth; i++) d[i] = ' ';
  d[depth]=0;
  /* #### - print type field */
  printf ("%sname:    %s\n", d, (wv->name ? wv->name : "(null)"));
  if (wv->value) printf ("%svalue:   %s\n", d, wv->value);
  if (wv->key)   printf ("%skey:     %s\n", d, wv->key);
  printf ("%senabled: %d\n", d, wv->enabled);
  if (wv->contents)
    {
      printf ("\n%scontents: \n", d);
      print_widget_value (wv->contents, depth + 5);
    }
  if (wv->next)
    {
      printf ("\n");
      print_widget_value (wv->next, depth);
    }
}
#endif

/* This recursively calls free_widget_value() on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.

   It used to be that emacs only allocated new storage for the `key' slot.
   All other slots are pointers into the data of Lisp_Strings, and must be
   left alone.  */
void
free_popup_widget_value_tree (widget_value *wv)
{
  if (! wv) return;
  if (wv->key) xfree (wv->key, char *);
  if (wv->value) xfree (wv->value, char *);
  if (wv->name) xfree (wv->name, char *);

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF; /* -559038737 base 10*/

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_popup_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_popup_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  free_widget_value (wv);
}

/* The following is actually called from somewhere within XtDispatchEvent(),
   called from XtAppProcessEvent() in event-Xt.c */

void
popup_selection_callback (Widget widget, LWLIB_ID ignored_id,
			  XtPointer client_data)
{
  Lisp_Object data, image_instance, callback, callback_ex;
  Lisp_Object frame, event;
  int update_subwindows_p = 0;
  struct device *d = get_device_from_display (XtDisplay (widget));
  struct frame *f = x_any_widget_or_parent_to_frame (d, widget);

#ifdef HAVE_MENUBARS
  /* set in lwlib to the time stamp associated with the most recent menu
     operation */
  extern Time x_focus_timestamp_really_sucks_fix_me_better;
#endif

  if (!f)
    return;
  if (((EMACS_INT) client_data) == 0)
    return;
  data = VOID_TO_LISP (client_data);
  frame = wrap_frame (f);

#if 0
  /* #### What the hell?  I can't understand why this call is here,
     and doing it is really courting disaster in the new event
     model, since popup_selection_callback is called from
     within next_event_internal() and Faccept_process_output()
     itself calls next_event_internal().  --Ben */

  /* Flush the X and process input */
  Faccept_process_output (Qnil, Qnil, Qnil);
#endif

  if (((EMACS_INT) client_data) == -1)
    {
      event = Fmake_event (Qnil, Qnil);

      XSET_EVENT_TYPE (event, misc_user_event);
      XSET_EVENT_CHANNEL (event, frame);
      XSET_EVENT_MISC_USER_FUNCTION (event, Qrun_hooks);
      XSET_EVENT_MISC_USER_OBJECT (event, Qmenu_no_selection_hook);
    }
  else
    {
      image_instance = XCAR (data);
      callback = XCAR (XCDR (data));
      callback_ex = XCDR (XCDR (data));
      update_subwindows_p = 1;
      /* It is possible for a widget action to cause it to get out of
	 sync with its instantiator. Thus it is necessary to signal
	 this possibility. */
      if (IMAGE_INSTANCEP (image_instance))
	XIMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (image_instance) = 1;

      if (!NILP (callback_ex) && !UNBOUNDP (callback_ex))
	{
	  event = Fmake_event (Qnil, Qnil);

	  XSET_EVENT_TYPE (event, misc_user_event);
	  XSET_EVENT_CHANNEL (event, frame);
	  XSET_EVENT_MISC_USER_FUNCTION (event, Qeval);
	  XSET_EVENT_MISC_USER_OBJECT (event, list4 (Qfuncall, callback_ex, image_instance, event));
	}
      else if (NILP (callback) || UNBOUNDP (callback))
	event = Qnil;
      else
	{
	  Lisp_Object fn, arg;

	  event = Fmake_event (Qnil, Qnil);

	  get_gui_callback (callback, &fn, &arg);
	  XSET_EVENT_TYPE (event, misc_user_event);
	  XSET_EVENT_CHANNEL (event, frame);
	  XSET_EVENT_MISC_USER_FUNCTION (event, fn);
	  XSET_EVENT_MISC_USER_OBJECT (event, arg);
	}
    }

  /* This is the timestamp used for asserting focus so we need to get an
     up-to-date value event if no events have been dispatched to emacs
     */
#ifdef HAVE_MENUBARS
  DEVICE_X_MOUSE_TIMESTAMP (d) = x_focus_timestamp_really_sucks_fix_me_better;
#else
  DEVICE_X_MOUSE_TIMESTAMP (d) = DEVICE_X_GLOBAL_MOUSE_TIMESTAMP (d);
#endif
  if (!NILP (event))
    enqueue_dispatch_event (event);
  /* The result of this evaluation could cause other instances to change so
     enqueue an update callback to check this. */
  if (update_subwindows_p && !NILP (event))
    enqueue_magic_eval_event (update_widget_instances, frame);
}

#if 1
  /* Eval the activep slot of the menu item */
# define wv_set_evalable_slot(slot,form) do {				\
  Lisp_Object wses_form = (form);					\
  (slot) = (NILP (wses_form) ? 0 :					\
	    EQ (wses_form, Qt) ? 1 :					\
	    !NILP (in_display ? eval_within_redisplay (wses_form)	\
		   : Feval (wses_form)));				\
} while (0)
#else
  /* Treat the activep slot of the menu item as a boolean */
# define wv_set_evalable_slot(slot,form)	\
      ((void) (slot = (!NILP (form))))
#endif

Extbyte *
menu_separator_style_and_to_external (const Ibyte *s)
{
  const Ibyte *p;
  Ibyte first;

  if (!s || s[0] == '\0')
    return NULL;
  first = s[0];
  if (first != '-' && first != '=')
    return NULL;
  for (p = s; *p == first; p++)
    DO_NOTHING;

  /* #### - cannot currently specify a separator tag "--!tag" and a
     separator style "--:style" at the same time. */
  /* #### - Also, the motif menubar code doesn't deal with the
     double etched style yet, so it's not good to get into the habit of
     using "===" in menubars to get double-etched lines */
  if (*p == '!' || *p == '\0')
    return ((first == '-')
	    ? NULL			/* single etched is the default */
	    : xstrdup ("shadowDoubleEtchedIn"));
  else if (*p == ':')
    {
      Extbyte *retval;

      C_STRING_TO_EXTERNAL_MALLOC (p + 1, retval, Qlwlib_encoding);
      return retval;
    }

  return NULL;
}

Extbyte *
add_accel_and_to_external (Lisp_Object string)
{
  int i;
  int found_accel = 0;
  Extbyte *retval;
  Ibyte *name = XSTRING_DATA (string);

  for (i = 0; name[i]; ++i)
    if (name[i] == '%' && name[i+1] == '_')
      {
	found_accel = 1;
	break;
      }

  if (found_accel)
    LISP_STRING_TO_EXTERNAL_MALLOC (string, retval, Qlwlib_encoding);
  else
    {
      Bytecount namelen = XSTRING_LENGTH (string);
      Ibyte *chars = (Ibyte *) ALLOCA (namelen + 3);
      chars[0] = '%';
      chars[1] = '_';
      memcpy (chars + 2, name, namelen + 1);
      C_STRING_TO_EXTERNAL_MALLOC (chars, retval, Qlwlib_encoding);
    }

  return retval;
}

/* This does the dirty work.  GC is inhibited when this is called.
 */
int
button_item_to_widget_value (Lisp_Object gui_object_instance,
			     Lisp_Object gui_item, widget_value *wv,
			     int allow_text_field_p, int no_keys_p,
			     int menu_entry_p, int accel_p)
{
  /* This function cannot GC because GC is inhibited when it's called */
  Lisp_Gui_Item* pgui = 0;

  /* degenerate case */
  if (STRINGP (gui_item))
    {
      wv->type = TEXT_TYPE;
      if (accel_p)
	wv->name = add_accel_and_to_external (gui_item);
      else
	LISP_STRING_TO_EXTERNAL_MALLOC (gui_item, wv->name, Qlwlib_encoding);
      return 1;
    }
  else if (!GUI_ITEMP (gui_item))
    invalid_argument ("need a string or a gui_item here", gui_item);

  pgui = XGUI_ITEM (gui_item);

  if (!NILP (pgui->filter))
    sferror (":filter keyword not permitted on leaf nodes", gui_item);

#ifdef HAVE_MENUBARS
  if (menu_entry_p && !gui_item_included_p (gui_item, Vmenubar_configuration))
    {
      /* the include specification says to ignore this item. */
      return 0;
    }
#endif /* HAVE_MENUBARS */

  if (!STRINGP (pgui->name))
    pgui->name = Feval (pgui->name);

  CHECK_STRING (pgui->name);
  if (accel_p)
    {
      wv->name = add_accel_and_to_external (pgui->name);
      wv->accel = LISP_TO_VOID (gui_item_accelerator (gui_item));
    }
  else
    {
      LISP_STRING_TO_EXTERNAL_MALLOC (pgui->name, wv->name, Qlwlib_encoding);
      wv->accel = LISP_TO_VOID (Qnil);
    }

  if (!NILP (pgui->suffix))
    {
      Lisp_Object suffix2;

      /* Shortcut to avoid evaluating suffix each time */
      if (STRINGP (pgui->suffix))
	suffix2 = pgui->suffix;
      else
	{
	  suffix2 = Feval (pgui->suffix);
	  CHECK_STRING (suffix2);
	}

      LISP_STRING_TO_EXTERNAL_MALLOC (suffix2, wv->value, Qlwlib_encoding);
    }

  wv_set_evalable_slot (wv->enabled, pgui->active);
  wv_set_evalable_slot (wv->selected, pgui->selected);

  if (!NILP (pgui->callback) || !NILP (pgui->callback_ex))
    wv->call_data = LISP_TO_VOID (cons3 (gui_object_instance,
					 pgui->callback,
					 pgui->callback_ex));

  if (no_keys_p
#ifdef HAVE_MENUBARS
      || (menu_entry_p && !menubar_show_keybindings)
#endif
      )
    wv->key = 0;
  else if (!NILP (pgui->keys))	/* Use this string to generate key bindings */
    {
      CHECK_STRING (pgui->keys);
      pgui->keys = Fsubstitute_command_keys (pgui->keys);
      if (XSTRING_LENGTH (pgui->keys) > 0)
	LISP_STRING_TO_EXTERNAL_MALLOC (pgui->keys, wv->key, Qlwlib_encoding);
      else
	wv->key = 0;
    }
  else if (SYMBOLP (pgui->callback))	/* Show the binding of this command. */
    {
      DECLARE_EISTRING_MALLOC (buf);
      /* #### Warning, dependency here on current_buffer and point */
      where_is_to_char (pgui->callback, buf);
      if (eilen (buf) > 0)
	C_STRING_TO_EXTERNAL_MALLOC (eidata (buf), wv->key, Qlwlib_encoding);
      else
	wv->key = 0;
      eifree (buf);
    }

  CHECK_SYMBOL (pgui->style);
  if (NILP (pgui->style))
    {
      Ibyte *intname;
      Bytecount intlen;
      /* If the callback is nil, treat this item like unselectable text.
	 This way, dashes will show up as a separator. */
      if (!wv->enabled)
	wv->type = BUTTON_TYPE;
      TO_INTERNAL_FORMAT (C_STRING, wv->name,
			  ALLOCA, (intname, intlen),
			  Qlwlib_encoding);
      if (separator_string_p (intname))
	{
	  wv->type = SEPARATOR_TYPE;
	  wv->value = menu_separator_style_and_to_external (intname);
	}
      else
	{
#if 0
	  /* #### - this is generally desirable for menubars, but it breaks
	     a package that uses dialog boxes and next_command_event magic
	     to use the callback slot in dialog buttons for data instead of
	     a real callback.

	     Code is data, right?  The beauty of LISP abuse.   --Stig */
	  if (NILP (callback))
	    wv->type = TEXT_TYPE;
	  else
#endif
	    wv->type = BUTTON_TYPE;
	}
    }
  else if (EQ (pgui->style, Qbutton))
    wv->type = BUTTON_TYPE;
  else if (EQ (pgui->style, Qtoggle))
    wv->type = TOGGLE_TYPE;
  else if (EQ (pgui->style, Qradio))
    wv->type = RADIO_TYPE;
  else if (EQ (pgui->style, Qtext))
    {
      wv->type = TEXT_TYPE;
#if 0
      wv->value = wv->name;
      wv->name = "value";
#endif
    }
  else
    invalid_constant_2 ("Unknown style", pgui->style, gui_item);

  if (!allow_text_field_p && (wv->type == TEXT_TYPE))
    sferror ("Text field not allowed in this context", gui_item);

  if (!NILP (pgui->selected) && EQ (pgui->style, Qtext))
    sferror
      (":selected only makes sense with :style toggle, radio or button",
       gui_item);
  return 1;
}

/* parse tree's of gui items into widget_value hierarchies */
static void gui_item_children_to_widget_values (Lisp_Object
						gui_object_instance,
						Lisp_Object items,
						widget_value* parent,
						int accel_p);

static widget_value *
gui_items_to_widget_values_1 (Lisp_Object gui_object_instance,
			      Lisp_Object items, widget_value* parent,
			      widget_value* prev, int accel_p)
{
  widget_value* wv = 0;

  assert ((parent || prev) && !(parent && prev));
  /* now walk the tree creating widget_values as appropriate */
  if (!CONSP (items))
    {
      wv = xmalloc_widget_value ();
      if (parent)
	parent->contents = wv;
      else
	prev->next = wv;
      if (!button_item_to_widget_value (gui_object_instance,
					items, wv, 0, 1, 0, accel_p))
	{
	  free_widget_value_tree (wv);
	  if (parent)
	    parent->contents = 0;
	  else
	    prev->next = 0;
	}
      else
	wv->value = xstrdup (wv->name);	/* what a mess... */
    }
  else
    {
      /* first one is the parent */
      if (CONSP (XCAR (items)))
	sferror ("parent item must not be a list", XCAR (items));

      if (parent)
	wv = gui_items_to_widget_values_1 (gui_object_instance,
					   XCAR (items), parent, 0, accel_p);
      else
	wv = gui_items_to_widget_values_1 (gui_object_instance,
					   XCAR (items), 0, prev, accel_p);
      /* the rest are the children */
      gui_item_children_to_widget_values (gui_object_instance,
					  XCDR (items), wv, accel_p);
    }
  return wv;
}

static void
gui_item_children_to_widget_values (Lisp_Object gui_object_instance,
				    Lisp_Object items, widget_value* parent,
				    int accel_p)
{
  widget_value* wv = 0, *prev = 0;
  Lisp_Object rest;
  CHECK_CONS (items);

  /* first one is master */
  prev = gui_items_to_widget_values_1 (gui_object_instance, XCAR (items),
				       parent, 0, accel_p);
  /* the rest are the children */
  LIST_LOOP (rest, XCDR (items))
    {
      Lisp_Object tab = XCAR (rest);
      wv = gui_items_to_widget_values_1 (gui_object_instance, tab, 0, prev,
					 accel_p);
      prev = wv;
    }
}

widget_value *
gui_items_to_widget_values (Lisp_Object gui_object_instance, Lisp_Object items,
			    int accel_p)
{
  /* This function can GC */
  widget_value *control = 0, *tmp = 0;
  int count;
  Lisp_Object wv_closure;

  if (NILP (items))
    sferror ("must have some items", items);

  /* Inhibit GC during this conversion.  The reasons for this are
     the same as in menu_item_descriptor_to_widget_value(); see
     the large comment above that function. */
  count = begin_gc_forbidden ();

  /* Also make sure that we free the partially-created widget_value
     tree on Lisp error. */
  control = xmalloc_widget_value ();
  wv_closure = make_opaque_ptr (control);
  record_unwind_protect (widget_value_unwind, wv_closure);

  gui_items_to_widget_values_1 (gui_object_instance, items, control, 0,
				accel_p);

  /* mess about getting the data we really want */
  tmp = control;
  control = control->contents;
  tmp->next = 0;
  tmp->contents = 0;
  free_widget_value_tree (tmp);

  /* No more need to free the half-filled-in structures. */
  set_opaque_ptr (wv_closure, 0);
  unbind_to (count);

  return control;
}

void
syms_of_gui_x (void)
{
}

void
reinit_vars_of_gui_x (void)
{
  lwlib_id_tick = (1<<16);	/* start big, to not conflict with Energize */
#ifdef HAVE_POPUPS
  popup_up_p = 0;
#endif
}

void
vars_of_gui_x (void)
{
  reinit_vars_of_gui_x ();

  Vpopup_callbacks = Qnil;
  staticpro (&Vpopup_callbacks);
}
