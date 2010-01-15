/* GTK selection processing for XEmacs
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 2001 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* Authorship:

   Written by Kevin Gallo for FSF Emacs.
   Rewritten for mswindows by Jonathan Harris, December 1997 for 21.0.
   Rewritten for GTK by William Perry, April 2000 for 21.1
 */


#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device-impl.h"
#include "events.h"
#include "frame.h"
#include "opaque.h"
#include "select.h"

#include "console-gtk-impl.h"

static Lisp_Object Vretrieved_selection;
static gboolean waiting_for_selection;
Lisp_Object Vgtk_sent_selection_hooks;

extern int lisp_to_time (Lisp_Object, time_t *);
extern Lisp_Object time_to_lisp (time_t);

static GdkAtom
symbol_to_gtk_atom (struct device *UNUSED (d), Lisp_Object sym,
		    int only_if_exists)
{
  if (NILP (sym))		return GDK_SELECTION_PRIMARY;
  if (EQ (sym, Qt))		return GDK_SELECTION_SECONDARY;
  if (EQ (sym, QPRIMARY))	return GDK_SELECTION_PRIMARY;
  if (EQ (sym, QSECONDARY))	return GDK_SELECTION_SECONDARY;

  {
    const Extbyte *nameext;
    LISP_STRING_TO_EXTERNAL (Fsymbol_name (sym), nameext, Qctext);
    return gdk_atom_intern (nameext, only_if_exists ? TRUE : FALSE);
  }
}

static Lisp_Object
atom_to_symbol (struct device *UNUSED (d), GdkAtom atom)
{
  if (atom == GDK_SELECTION_PRIMARY) return (QPRIMARY);
  if (atom == GDK_SELECTION_SECONDARY) return (QSECONDARY);

  {
    Ibyte *intstr;
    Extbyte *str = gdk_atom_name (atom);

    if (! str) return Qnil;

    EXTERNAL_TO_C_STRING (str, intstr, Qctext);
    g_free (str);
    return intern_int (intstr);
  }
}

#define PROCESSING_GTK_CODE
#include "select-common.h"
#undef PROCESSING_GTK_CODE


/* Set the selection data to GDK_NONE and NULL data, meaning we were
** unable to do what they wanted.
*/
static void
gtk_decline_selection_request (GtkSelectionData *data)
{
  gtk_selection_data_set (data, GDK_NONE, 0, NULL, 0);
}

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.
 */
struct _selection_closure
{
  GtkSelectionData *data;
  gboolean successful;
};

static Lisp_Object
gtk_selection_request_lisp_error (Lisp_Object closure)
{
  struct _selection_closure *cl = (struct _selection_closure *)
    get_opaque_ptr (closure);

  free_opaque_ptr (closure);
  if (cl->successful == TRUE)
    return Qnil;
  gtk_decline_selection_request (cl->data);
  return Qnil;
}

/* This provides the current selection to a requester.
**
** This is connected to the selection_get() signal of the application
** shell in device-gtk.c:gtk_init_device().
**
** This is radically different than the old selection code (21.1.x),
** but has been modeled after the X code, and appears to work.
**
** WMP Feb 12 2001
*/
void
emacs_gtk_selection_handle (GtkWidget *UNUSED (widget),
			    GtkSelectionData *selection_data,
			    guint UNUSED (info),
			    guint time_stamp,
			    gpointer UNUSED (data))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;
  Lisp_Object temp_obj;
  Lisp_Object selection_symbol;
  Lisp_Object target_symbol = Qnil;
  Lisp_Object converted_selection = Qnil;
  guint32 local_selection_time;
  Lisp_Object successful_p = Qnil;
  int count;
  struct device *d = decode_gtk_device (Qnil);
  struct _selection_closure *cl = NULL;

  GCPRO2 (converted_selection, target_symbol);

  selection_symbol = atom_to_symbol (d, selection_data->selection);
  target_symbol = atom_to_symbol (d, selection_data->target);

#if 0 /* #### MULTIPLE doesn't work yet */
  if (EQ (target_symbol, QMULTIPLE))
    target_symbol = fetch_multiple_target (selection_data);
#endif

  temp_obj = get_selection_raw_time(selection_symbol);

  if (NILP (temp_obj))
    {
      /* We don't appear to have the selection. */
      gtk_decline_selection_request (selection_data);

      goto DONE_LABEL;
    }

  local_selection_time = * (guint32 *) XOPAQUE_DATA (temp_obj);

  if (time_stamp != GDK_CURRENT_TIME &&
      local_selection_time > time_stamp)
    {
      /* Someone asked for the selection, and we have one, but not the one
	 they're looking for. */
      gtk_decline_selection_request (selection_data);
      goto DONE_LABEL;
    }

  converted_selection = select_convert_out (selection_symbol,
					    target_symbol, Qnil);

  /* #### Is this the right thing to do? I'm no X expert. -- ajh */
  if (NILP (converted_selection))
    {
      /* We don't appear to have a selection in that data type. */
      gtk_decline_selection_request (selection_data);
      goto DONE_LABEL;
    }

  count = specpdl_depth ();

  cl = xnew (struct _selection_closure);
  cl->data = selection_data;
  cl->successful = FALSE;

  record_unwind_protect (gtk_selection_request_lisp_error,
			 make_opaque_ptr (cl));

  {
    Rawbyte *data;
    Bytecount size;
    int format;
    GdkAtom type;
    lisp_data_to_selection_data (d, converted_selection,
				 &data, &type, &size, &format);

    gtk_selection_data_set (selection_data, type, format, data,
			    /* #### is this right? */
			    (unsigned int) size);
    successful_p = Qt;
    /* Tell x_selection_request_lisp_error() it's cool. */
    cl->successful = TRUE;
    xfree (data, Rawbyte *);
  }

  unbind_to (count);

 DONE_LABEL:

  if (cl)
    xfree (cl, struct _selection_closure *);

  UNGCPRO;

  /* Let random lisp code notice that the selection has been asked for. */
  {
    Lisp_Object val = Vgtk_sent_selection_hooks;
    if (!UNBOUNDP (val) && !NILP (val))
      {
	Lisp_Object rest;
	if (CONSP (val) && !EQ (XCAR (val), Qlambda))
	  for (rest = val; !NILP (rest); rest = Fcdr (rest))
	    call3 (Fcar (rest), selection_symbol, target_symbol, successful_p);
	else
	  call3 (val, selection_symbol, target_symbol, successful_p);
      }
  }
}


void
emacs_gtk_selection_clear_event_handle (GtkWidget *UNUSED (widget),
                                        GdkEventSelection *event,
                                        gpointer UNUSED (data))
{
  GdkAtom selection = event->selection;
  guint32 changed_owner_time = event->time;
  struct device *d = decode_gtk_device (Qnil);

  Lisp_Object selection_symbol, local_selection_time_lisp;
  guint32 local_selection_time;

  selection_symbol = atom_to_symbol (d, selection);

  local_selection_time_lisp = get_selection_raw_time (selection_symbol);

  /* We don't own the selection, so that's fine. */
  if (NILP (local_selection_time_lisp))
    return;

  local_selection_time = *(guint32 *) XOPAQUE_DATA (local_selection_time_lisp);

  /* This SelectionClear is for a selection that we no longer own, so we can
     disregard it.  (That is, we have reasserted the selection since this
     request was generated.)
   */
  if (changed_owner_time != GDK_CURRENT_TIME &&
      local_selection_time > changed_owner_time)
    return;

  handle_selection_clear (selection_symbol);
}



static GtkWidget *reading_selection_reply;
static GdkAtom reading_which_selection;
static int selection_reply_timed_out;

/* Gets the current selection owned by another application */
void
emacs_gtk_selection_received (GtkWidget *UNUSED (widget),
			      GtkSelectionData *selection_data,
			      gpointer UNUSED (user_data))
{
  waiting_for_selection = FALSE;
  Vretrieved_selection = Qnil;

  reading_selection_reply = NULL;

  signal_fake_event ();

  if (selection_data->length < 0)
    {
      return;
    }

  Vretrieved_selection =
    selection_data_to_lisp_data (NULL,
				 selection_data->data,
				 selection_data->length,
				 selection_data->type,
				 selection_data->format);
}

static int
selection_reply_done (void *UNUSED (ignore))
{
  return !reading_selection_reply;
}

/* Do protocol to read selection-data from the server.
   Converts this to lisp data and returns it.
 */
static Lisp_Object
gtk_get_foreign_selection (Lisp_Object selection_symbol,
			   Lisp_Object target_type)
{
  /* This function can GC */
  struct device *d = decode_gtk_device (Qnil);
  GtkWidget *requestor = DEVICE_GTK_APP_SHELL (d);
  guint32 requestor_time = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  GdkAtom selection_atom = symbol_to_gtk_atom (d, selection_symbol, 0);
  int speccount;
  GdkAtom type_atom = symbol_to_gtk_atom (d, (CONSP (target_type) ?
					      XCAR (target_type) : target_type), 0);

  gtk_selection_convert (requestor, selection_atom, type_atom,
			 requestor_time);

  signal_fake_event ();

  /* Block until the reply has been read. */
  reading_selection_reply = requestor;
  reading_which_selection = selection_atom;
  selection_reply_timed_out = 0;

  speccount = specpdl_depth ();

#if 0
  /* add a timeout handler */
  if (gtk_selection_timeout > 0)
    {
      Lisp_Object id = Fadd_timeout (make_int (x_selection_timeout),
				     Qx_selection_reply_timeout_internal,
				     Qnil, Qnil);
      record_unwind_protect (Fdisable_timeout, id);
    }
#endif

  /* This is ^Gable */
  wait_delaying_user_input (selection_reply_done, 0);

  if (selection_reply_timed_out)
    signal_error (Qselection_conversion_error, "timed out waiting for reply from selection owner", Qunbound);

  unbind_to (speccount);

  /* otherwise, the selection is waiting for us on the requested property. */
  return select_convert_in (selection_symbol,
			    target_type,
			    Vretrieved_selection);
}


#if 0
static void
gtk_get_window_property (struct device *d, GtkWidget *window, GdkAtom property,
			 Extbyte **data_ret, int *bytes_ret,
			 GdkAtom *actual_type_ret, int *actual_format_ret,
			 unsigned long *actual_size_ret, int delete_p)
{
  /* deleted */
}


static void
receive_incremental_selection (Display *display, Window window, Atom property,
			       /* this one is for error messages only */
			       Lisp_Object target_type,
			       unsigned int min_size_bytes,
			       Extbyte **data_ret, int *size_bytes_ret,
			       Atom *type_ret, int *format_ret,
			       unsigned long *size_ret)
{
  /* deleted */
}


static Lisp_Object
gtk_get_window_property_as_lisp_data (struct device *d,
				      GtkWidget *window,
				      GdkAtom property,
				      /* next two for error messages only */
				      Lisp_Object target_type,
				      GdkAtom selection_atom)
{
  /* deleted */
}
#endif



static Lisp_Object
gtk_own_selection (Lisp_Object selection_name,
		   Lisp_Object UNUSED (selection_value),
		   Lisp_Object UNUSED (how_to_add),
		   Lisp_Object UNUSED (selection_type), int UNUSED (owned_p))
{
  struct device *d = decode_gtk_device (Qnil);
  GtkWidget *selecting_window = GTK_WIDGET (DEVICE_GTK_APP_SHELL (d));
  /* Use the time of the last-read mouse or keyboard event.
     For selection purposes, we use this as a sleazy way of knowing what the
     current time is in server-time.  This assumes that the most recently read
     mouse or keyboard event has something to do with the assertion of the
     selection, which is probably true.
     */
  guint32 thyme = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  GdkAtom selection_atom;

  CHECK_SYMBOL (selection_name);
  selection_atom = symbol_to_gtk_atom (d, selection_name, 0);

  gtk_selection_owner_set (selecting_window,
			   selection_atom,
			   thyme);

  /* [[ We do NOT use time_to_lisp() here any more, like we used to.
     That assumed equivalence of time_t and Time, which is not
     necessarily the case (e.g. under OSF on the Alphas, where
     Time is a 64-bit quantity and time_t is a 32-bit quantity).

     Opaque pointers are the clean way to go here. ]] 

     See my comment on the same issue in select-x.c -- Aidan. */
  return make_opaque (&thyme, sizeof (thyme));
}

static void
gtk_disown_selection (Lisp_Object selection, Lisp_Object timeval)
{
  struct device *d = decode_gtk_device (Qnil);
  GdkAtom selection_atom;
  guint32 timestamp;

  CHECK_SYMBOL (selection);
  selection_atom = symbol_to_gtk_atom (d, selection, 0);

  if (NILP (timeval))
    timestamp = DEVICE_GTK_MOUSE_TIMESTAMP (d);
  else
    {
      time_t the_time;
      lisp_to_time (timeval, &the_time);
      timestamp = (guint32) the_time;
    }

  gtk_selection_owner_set (NULL, selection_atom, timestamp);
}

static Lisp_Object
gtk_selection_exists_p (Lisp_Object selection,
			Lisp_Object UNUSED (selection_type))
{
  struct device *d = decode_gtk_device (Qnil);
  
  return (gdk_selection_owner_get (symbol_to_gtk_atom (d, selection, 0)) ? Qt : Qnil);
}


 
/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_select_gtk (void)
{
}

void
console_type_create_select_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, own_selection);
  CONSOLE_HAS_METHOD (gtk, disown_selection);
  CONSOLE_HAS_METHOD (gtk, selection_exists_p);
  CONSOLE_HAS_METHOD (gtk, get_foreign_selection);
}

void
vars_of_select_gtk (void)
{
  staticpro (&Vretrieved_selection);
  Vretrieved_selection = Qnil;

  DEFVAR_LISP ("gtk-sent-selection-hooks", &Vgtk_sent_selection_hooks /*
A function or functions to be called after we have responded to some
other client's request for the value of a selection that we own.  The
function(s) will be called with four arguments:
  - the name of the selection (typically PRIMARY, SECONDARY, or CLIPBOARD);
  - the name of the selection-type which we were requested to convert the
    selection into before sending (for example, STRING or LENGTH);
  - and whether we successfully transmitted the selection.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
This hook doesn't let you change the behavior of emacs's selection replies,
it merely informs you that they have happened.
*/ );
  Vgtk_sent_selection_hooks = Qunbound;
}
