/* Implements elisp-programmable dialog boxes -- X interface.
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


static void
maybe_run_dbox_text_callback (LWLIB_ID id)
{
  /* !!#### This function has not been Mule-ized */
  widget_value *wv;
  int got_some;
  wv = xmalloc_widget_value ();
  wv->name = "value";
  got_some = lw_get_some_values (id, wv);
  if (got_some)
    {
      Lisp_Object text_field_callback;
      char *text_field_value = wv->value;
      VOID_TO_LISP (text_field_callback, wv->call_data);
      if (text_field_value)
	{
	  void *tmp = LISP_TO_VOID (list2 (text_field_callback,
                                           build_string (text_field_value)));
	  popup_selection_callback (0, id, (XtPointer) tmp);
	  xfree (text_field_value);
	}
    }
  free_widget_value (wv);
}

static void
dbox_selection_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  /* This is called with client_data == -1 when WM_DELETE_WINDOW is sent
     instead of a button being selected. */
  struct device *d = get_device_from_display (XtDisplay (widget));
  struct frame *f = 0;
  Widget cur_widget = widget;

  /* The parent which is actually connected to our EmacsFrame may be a
     ways up the tree. */
  while (!f && cur_widget)
    {
      f = x_any_window_to_frame (d, XtWindow (cur_widget));
      cur_widget = XtParent (cur_widget);
    }

  if (popup_handled_p (id))
    return;
  assert (popup_up_p != 0);
  ungcpro_popup_callbacks (id);
  popup_up_p--;
  maybe_run_dbox_text_callback (id);
  popup_selection_callback (widget, id, client_data);
  lw_destroy_all_widgets (id);

  /* The Motif dialog box sets the keyboard focus to itself.  When it
     goes away we have to take care of getting the focus back
     ourselves. */
#ifdef EXTERNAL_WIDGET
  /* #### Not sure if this special case is necessary. */
  if (!FRAME_X_EXTERNAL_WINDOW_P (f) && f)
#else
  if (f)
#endif
    lw_set_keyboard_focus (FRAME_X_SHELL_WIDGET (f), FRAME_X_TEXT_WIDGET (f));
}

static CONST char * CONST button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

/* can't have static frame locals because of some broken compilers */
static char tmp_dbox_name [255];

static widget_value *
dbox_descriptor_to_widget_value (Lisp_Object desc)
{
  /* !!#### This function has not been Mule-ized */
  /* This function can GC */
  char *name;
  int lbuttons = 0, rbuttons = 0;
  int partition_seen = 0;
  int text_field_p = 0;
  int allow_text_p = 1;
  widget_value *prev = 0, *kids = 0;
  int n = 0;
  int count = specpdl_depth ();
  Lisp_Object wv_closure;

  CHECK_CONS (desc);
  CHECK_STRING (XCAR (desc));
  name = (char *) string_data (XSTRING (LISP_GETTEXT (XCAR (desc))));
  desc = XCDR (desc);
  if (!CONSP (desc))
    error ("dialog boxes must have some buttons");

  /* Inhibit GC during this conversion.  The reasons for this are
     the same as in menu_item_descriptor_to_widget_value(); see
     the large comment above that function. */

  record_unwind_protect (restore_gc_inhibit,
			 make_int (gc_currently_forbidden));
  gc_currently_forbidden = 1;

  kids = prev = xmalloc_widget_value ();

  /* Also make sure that we free the partially-created widget_value
     tree on Lisp error. */

  wv_closure = make_opaque_ptr (kids);
  record_unwind_protect (widget_value_unwind, wv_closure);
  prev->name = "message";
  prev->value = xstrdup (name);
  prev->enabled = 1;

  for (; !NILP (desc); desc = Fcdr (desc))
    {
      Lisp_Object button = XCAR (desc);
      widget_value *wv;

      if (NILP (button))
	{
	  if (partition_seen)
	    error ("more than one partition (nil) seen in dbox spec");
	  partition_seen = 1;
	  continue;
	}
      CHECK_VECTOR (button);
      wv = xmalloc_widget_value ();

      if (!button_item_to_widget_value (button, wv, allow_text_p, 1))
	{
	  free_widget_value (wv);
	  continue;
	}

      if (wv->type == TEXT_TYPE)
	{
	  text_field_p = 1;
	  allow_text_p = 0;	 /* only allow one */
	}
      else			/* it's a button */
	{
	  allow_text_p = 0;	 /* only allow text field at the front */
	  wv->value = xstrdup (wv->name);	/* what a mess... */
	  wv->name = (char *) button_names [n];

	  if (partition_seen)
	    rbuttons++;
	  else
	    lbuttons++;
	  n++;

	  if (lbuttons > 9 || rbuttons > 9)
	    error ("too many buttons (9)"); /* #### this leaks */
	}

      prev->next = wv;
      prev = wv;
    }

  if (n == 0)
    error ("dialog boxes must have some buttons");
  {
    char type = (text_field_p ? 'P' : 'Q');
    widget_value *dbox;
    sprintf (tmp_dbox_name, "%c%dBR%d", type, lbuttons + rbuttons, rbuttons);
    dbox = xmalloc_widget_value ();
    dbox->name = tmp_dbox_name;
    dbox->contents = kids;

    /* No more need to free the half-filled-in structures. */
    set_opaque_ptr (wv_closure, 0);
    unbind_to (count, Qnil);
    return dbox;
  }
}

DEFUN ("popup-dialog-box", Fpopup_dialog_box, Spopup_dialog_box, 1, 1, 0 /*
Pop up a dialog box.
A dialog box description is a list.

The first element of a dialog box must be a string, which is the title or
question.

The rest of the elements are descriptions of the dialog box's buttons.
Each of these is a vector, the syntax of which is essentially the same as
that of popup menu items.  They may have any of the following forms:

 [ \"name\" callback <active-p> ]
 [ \"name\" callback <active-p> \"suffix\" ]
 [ \"name\" callback :<keyword> <value>  :<keyword> <value> ... ]

The name is the string to display on the button; it is filtered through the
resource database, so it is possible for resources to override what string
is actually displayed.

If the `callback' of a button is a symbol, then it must name a command.
It will be invoked with `call-interactively'.  If it is a list, then it is
evaluated with `eval'.

One (and only one) of the buttons may be `nil'.  This marker means that all
following buttons should be flushright instead of flushleft.

Though the keyword/value syntax is supported for dialog boxes just as in 
popup menus, the only keyword which is both meaningful and fully implemented
for dialog box buttons is `:active'.
*/ )
     (dbox_desc)
     Lisp_Object dbox_desc;
{
  int dbox_id;
  struct frame *f = selected_frame ();
  widget_value *data;
  Widget parent, dbox;
  Lisp_Object frame = Qnil;

  XSETFRAME (frame, f);

  CHECK_X_FRAME (frame);
  if (SYMBOLP (dbox_desc))
    dbox_desc = Fsymbol_value (dbox_desc);
  CHECK_CONS (dbox_desc);
  CHECK_STRING (XCAR (dbox_desc));
  data = dbox_descriptor_to_widget_value (dbox_desc);

  parent = FRAME_X_SHELL_WIDGET (f);

  dbox_id = new_lwlib_id ();
  dbox = lw_create_widget (data->name, "dialog", dbox_id, data, parent, 1, 0,
			   dbox_selection_callback, 0);
  lw_modify_all_widgets (dbox_id, data, True);
  lw_modify_all_widgets (dbox_id, data->contents, True);
  free_popup_widget_value_tree (data);

  gcpro_popup_callbacks (dbox_id);

  /* Setting zmacs-region-stays is necessary here because executing a
     command from a dialog is really a two-command process: the first
     command (bound to the button-click) simply pops up the dialog,
     and returns.  This causes a sequence of magic-events (destined
     for the dialog widget) to begin.  Eventually, a dialog item is
     selected, and a misc-user-event blip is pushed onto the end of
     the input stream, which is then executed by the event loop.
     
     So there are two command-events, with a bunch of magic-events
     between them.  We don't want the *first* command event to alter
     the state of the region, so that the region can be available as
     an argument for the second command. */
  if (zmacs_regions)
    zmacs_region_stays = 1;

  popup_up_p++;
  lw_pop_up_all_widgets (dbox_id);
  return Qnil;
}

void
syms_of_dialog_x (void)
{
  defsubr (&Spopup_dialog_box);
}

void
vars_of_dialog_x (void)
{
#if defined (LWLIB_DIALOGS_LUCID)
  Fprovide (intern ("lucid-dialogs"));
#elif defined (LWLIB_DIALOGS_MOTIF)
  Fprovide (intern ("motif-dialogs"));
#elif defined (LWLIB_DIALOGS_ATHENA)
  Fprovide (intern ("athena-dialogs"));
#endif
}
