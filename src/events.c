/* Events: printing them, converting them to and from characters.
   Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "console.h"
#include "device.h"
#include "extents.h"
#include "events.h"
#include "frame-impl.h"
#include "glyphs.h"
#include "keymap.h" /* for key_desc_list_to_event() */
#include "lstream.h"
#include "redisplay.h"
#include "toolbar.h"
#include "window.h"

#include "console-tty-impl.h" /* for stuff in character_to_event */

#ifdef USE_KKCC
#include "console-x.h"
#endif /* USE_KKCC */

/* Where old events go when they are explicitly deallocated.
   The event chain here is cut loose before GC, so these will be freed
   eventually.
 */
static Lisp_Object Vevent_resource;

Lisp_Object Qeventp;
Lisp_Object Qevent_live_p;
Lisp_Object Qkey_press_event_p;
Lisp_Object Qbutton_event_p;
Lisp_Object Qmouse_event_p;
Lisp_Object Qprocess_event_p;

Lisp_Object Qkey_press, Qbutton_press, Qbutton_release, Qmisc_user;
Lisp_Object Qascii_character;


/************************************************************************/
/*                       definition of event object                     */
/************************************************************************/

/* #### Ad-hoc hack.  Should be part of define_lrecord_implementation */
void
clear_event_resource (void)
{
  Vevent_resource = Qnil;
}

#ifdef USE_KKCC
/* Make sure we lose quickly if we try to use this event */
static void
deinitialize_event (Lisp_Object ev)
{
  Lisp_Event *event = XEVENT (ev);

  set_event_type (event, dead_event);
  SET_EVENT_CHANNEL (event, Qnil);
  set_lheader_implementation (&event->lheader, &lrecord_event);
  XSET_EVENT_NEXT (ev, Qnil);
  XSET_EVENT_DATA (ev, Qnil);
}

/* Set everything to zero or nil so that it's predictable. */
void
zero_event (Lisp_Event *e)
{
  SET_EVENT_DATA (e, Qnil);
  set_event_type (e, empty_event);
  SET_EVENT_NEXT (e, Qnil);
  SET_EVENT_CHANNEL (e, Qnil);
  SET_EVENT_TIMESTAMP_ZERO (e);
}

static const struct lrecord_description event_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Event, next) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Event, channel) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Event, event_data) },
  { XD_END }
};

static Lisp_Object
mark_event (Lisp_Object obj)
{
  mark_object (XEVENT_DATA(obj));
  mark_object (XEVENT_CHANNEL(obj));
  return (XEVENT_NEXT(obj));
}


static const struct lrecord_description key_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Key_Data, keysym) },
  { XD_END }
};

static Lisp_Object
mark_key_data (Lisp_Object obj)
{
  return (XKEY_DATA_KEYSYM(obj));
}


static const struct lrecord_description button_data_description [] = {
  { XD_END }
};

static Lisp_Object
mark_button_data (Lisp_Object obj)
{
  return Qnil;
}


static const struct lrecord_description motion_data_description [] = {
  { XD_END }
};

static Lisp_Object
mark_motion_data (Lisp_Object obj)
{
  return Qnil;
}


static const struct lrecord_description process_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Process_Data, process) },
  { XD_END }
};

static Lisp_Object
mark_process_data (Lisp_Object obj)
{
  return (XPROCESS_DATA_PROCESS(obj));
}


static const struct lrecord_description timeout_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Timeout_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Timeout_Data, object) },
  { XD_END }
};

static Lisp_Object
mark_timeout_data (Lisp_Object obj)
{
  mark_object (XTIMEOUT_DATA_FUNCTION(obj));
  return (XTIMEOUT_DATA_OBJECT(obj));
}


static const struct lrecord_description eval_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Eval_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Eval_Data, object) },
  { XD_END }
};

static Lisp_Object
mark_eval_data (Lisp_Object obj)
{
  mark_object (XEVAL_DATA_FUNCTION(obj));
  return (XEVAL_DATA_OBJECT(obj));
}


static const struct lrecord_description misc_user_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Misc_User_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Misc_User_Data, object) },
  { XD_END }
};

static Lisp_Object
mark_misc_user_data (Lisp_Object obj)
{
  mark_object (XMISC_USER_DATA_FUNCTION(obj));
  return (XMISC_USER_DATA_OBJECT(obj));
}


static const struct lrecord_description magic_eval_data_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Magic_Eval_Data, object) },
  { XD_END }
};

static Lisp_Object
mark_magic_eval_data (Lisp_Object obj)
{
  return (XMAGIC_EVAL_DATA_OBJECT(obj));
}


static const struct lrecord_description magic_data_description [] = {
  { XD_END }
};

static Lisp_Object
mark_magic_data (Lisp_Object obj)
{
  return Qnil;
}



static void
print_event (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<event>");

  switch (XEVENT_TYPE (obj))
    {
    case key_press_event:
      write_c_string (printcharfun, "#<keypress-event ");
      break;
    case button_press_event:
      write_c_string (printcharfun, "#<buttondown-event ");
      break;
    case button_release_event:
      write_c_string (printcharfun, "#<buttonup-event ");
      break;
    case magic_eval_event:
      write_c_string (printcharfun, "#<magic-eval-event ");
      break;
    case magic_event:
      write_c_string (printcharfun, "#<magic-event ");         
      break;
    case pointer_motion_event:
      write_c_string (printcharfun, "#<motion-event ");
      break;
    case process_event:
	write_c_string (printcharfun, "#<process-event ");
	break;
    case timeout_event:
	write_c_string (printcharfun, "#<timeout-event ");
	break;
    case misc_user_event:
	write_c_string (printcharfun, "#<misc-user-event ");
	break;
    case eval_event:
	write_c_string (printcharfun, "#<eval-event ");
	break;
    case empty_event:
	write_c_string (printcharfun, "#<empty-event>");
        return;
    case dead_event:
	write_c_string (printcharfun, "#<DEALLOCATED-EVENT>");
        return;
    default:
	write_c_string (printcharfun, "#<UNKNOWN-EVENT-TYPE>");
        return;
      }
  
  print_internal (XEVENT_DATA (obj), printcharfun, 1);
  write_c_string (printcharfun, ">");
}


static void
print_key_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[128];
  if (print_readably)
    printing_unreadable_object ("#<key_data>");

  sprintf (buf, "#<key_data ");
  /*  format_event_data_object (buf + 11, obj, 0);
  sprintf (buf + strlen (buf), ">");
  write_c_string (printcharfun, buf);*/
}

static void
print_button_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[128];
  if (print_readably)
    printing_unreadable_object ("#<button_data>");

  sprintf (buf, "#<button_data ");
  /*  format_event_data_object (buf + 14, obj, 0);
  sprintf (buf + strlen (buf), ">");
  write_c_string (printcharfun, buf);*/
}


static void
print_motion_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[64];

  if (print_readably)
    printing_unreadable_object ("#<motion_data>");

  sprintf (buf, "#<motion-data %ld, %ld>",
	   (long) XMOTION_DATA_X (obj),
	   (long) XMOTION_DATA_Y (obj));
  write_c_string (printcharfun, buf);
}


static void
print_process_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<process_data>");

  write_c_string (printcharfun, "#<process-data ");
  print_internal (XPROCESS_DATA_PROCESS (obj), printcharfun, 1);
  write_c_string (printcharfun, ">");
}


static void
print_timeout_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<timeout_data>");

  write_c_string (printcharfun, "#<timeout-data ");
  print_internal (XTIMEOUT_DATA_OBJECT (obj), printcharfun, 1);
  write_c_string (printcharfun, ">");
}


static void
print_eval_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<eval_data>");

  write_c_string (printcharfun, "#<eval-data ");
  print_internal (XEVAL_DATA_FUNCTION (obj), printcharfun, 1);
  write_c_string (printcharfun, " ");
  print_internal (XEVAL_DATA_OBJECT (obj), printcharfun, 1);
  write_c_string (printcharfun, ">");
}


static void
print_misc_user_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<misc_user_data>");

  write_c_string (printcharfun, "#<misc-user-data ");
  print_internal (XMISC_USER_DATA_FUNCTION (obj), printcharfun, 1);
  write_c_string (printcharfun, " ");
  print_internal (XMISC_USER_DATA_OBJECT (obj), printcharfun, 1);
  write_c_string (printcharfun, ">");
}


static void
print_magic_eval_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  //  char buf[128];

  if (print_readably)
    printing_unreadable_object ("#<magic_eval_data>");

  /*  format_event_data_object (buf + 18, obj, 0);*/
}


static void
print_magic_data (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[128];

  if (print_readably)
    printing_unreadable_object ("#<magic_data>");

  sprintf (buf, "#<magic-data ");
  /*  format_event_data_object (buf + 13, obj, 0);
  sprintf (buf + strlen (buf), ">");
  write_c_string (print_readably, buf);*/
}


static int
event_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Event *e1 = XEVENT (obj1);
  Lisp_Event *e2 = XEVENT (obj2);

  if (EVENT_TYPE (e1) != EVENT_TYPE (e2)) return 0;
  if (!EQ (EVENT_CHANNEL (e1), EVENT_CHANNEL (e2))) return 0;
/*  if (EVENT_TIMESTAMP (e1) != EVENT_TIMESTAMP (e2)) return 0; */
  switch (EVENT_TYPE (e1))
    {
    default: abort ();

    case process_event:
    case timeout_event:
    case pointer_motion_event:
    case key_press_event:
    case button_press_event:
    case button_release_event:
    case misc_user_event:
    case eval_event:
    case magic_eval_event:
      return internal_equal (EVENT_DATA (e1), EVENT_DATA (e2), 0);

    case magic_event:
      {
	struct console *con = XCONSOLE (CDFW_CONSOLE (EVENT_CHANNEL (e1)));

#ifdef HAVE_X_WINDOWS
	if (CONSOLE_X_P (con))
	  return (XMAGIC_DATA_X_EVENT (EVENT_DATA (e1)).xany.serial ==
		  XMAGIC_DATA_X_EVENT (EVENT_DATA (e2)).xany.serial);
#endif
#ifdef HAVE_GTK
	if (CONSOLE_GTK_P (con))
	  return (XMAGIC_DATA_GTK_EVENT (EVENT_DATA (e1)) ==
		  XMAGIC_DATA_GTK_EVENT (EVENT_DATA (e2)));
#endif
#ifdef HAVE_MS_WINDOWS
	if (CONSOLE_MSWINDOWS_P (con))
	  return (XMAGIC_DATA_MSWINDOWS_EVENT (EVENT_DATA (e1)) ==
		  XMAGIC_DATA_MSWINDOWS_EVENT (EVENT_DATA (e2)));
#endif
	abort ();
	return 1; /* not reached */
      }

    case empty_event:      /* Empty and deallocated events are equal. */
    case dead_event:
      return 1;
    }
}

static int
key_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (EQ (XKEY_DATA_KEYSYM (obj1), XKEY_DATA_KEYSYM (obj2)) &&
          (XKEY_DATA_MODIFIERS (obj1) == XKEY_DATA_MODIFIERS (obj2)));
}

static int
button_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (XBUTTON_DATA_BUTTON (obj1) == XBUTTON_DATA_BUTTON (obj2) &&
          XBUTTON_DATA_MODIFIERS (obj1) == XBUTTON_DATA_MODIFIERS (obj2));
}

static int
motion_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (XMOTION_DATA_X (obj1) == XMOTION_DATA_X (obj2) &&
          XMOTION_DATA_Y (obj1) == XMOTION_DATA_Y (obj2));
}

static int
process_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return EQ (XPROCESS_DATA_PROCESS (obj1), XPROCESS_DATA_PROCESS (obj2));
}

static int
timeout_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (internal_equal (XTIMEOUT_DATA_FUNCTION (obj1),
                          XTIMEOUT_DATA_FUNCTION (obj2), 0) &&
          internal_equal (XTIMEOUT_DATA_OBJECT (obj1),
                          XTIMEOUT_DATA_OBJECT (obj2), 0));
}

static int
eval_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (internal_equal (XEVAL_DATA_FUNCTION (obj1),
                          XEVAL_DATA_FUNCTION (obj2), 0) &&
          internal_equal (XEVAL_DATA_OBJECT (obj1),
                          XEVAL_DATA_OBJECT (obj2), 0));
}

static int
misc_user_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (internal_equal (XMISC_USER_DATA_FUNCTION (obj1), 
                          XMISC_USER_DATA_FUNCTION (obj2), 0) &&
          internal_equal (XMISC_USER_DATA_OBJECT (obj1), 
                          XMISC_USER_DATA_OBJECT (obj2), 0) &&
	      /* is this really needed for equality
	         or is x and y also important? */
          XMISC_USER_DATA_BUTTON (obj1) == XMISC_USER_DATA_BUTTON (obj2) &&
          XMISC_USER_DATA_MODIFIERS (obj1) == 
          XMISC_USER_DATA_MODIFIERS (obj2));
}

static int
magic_eval_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  return (XMAGIC_EVAL_DATA_INTERNAL_FUNCTION (obj1) ==
          XMAGIC_EVAL_DATA_INTERNAL_FUNCTION (obj2) &&
          internal_equal (XMAGIC_EVAL_DATA_OBJECT (obj1),
                          XMAGIC_EVAL_DATA_OBJECT (obj2), 0));
}

static int
magic_data_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{assert (0); return 0;}

static unsigned long
event_hash (Lisp_Object obj, int depth)
{
  Lisp_Event *e = XEVENT (obj);
  unsigned long hash;

  hash = HASH2 (EVENT_TYPE (e), LISP_HASH (EVENT_CHANNEL (e)));
  switch (EVENT_TYPE (e))
    {
    case process_event:
    case timeout_event:
    case key_press_event:
    case button_press_event:
    case button_release_event:
    case pointer_motion_event:
    case misc_user_event:
    case eval_event:
    case magic_eval_event:
      return HASH2 (hash, internal_hash (EVENT_DATA (e), depth + 1));

    case magic_event:
      {
	struct console *con = XCONSOLE (CDFW_CONSOLE (EVENT_CHANNEL (e)));
#ifdef HAVE_X_WINDOWS
	if (CONSOLE_X_P (con))
	  return HASH2 (hash, XMAGIC_DATA_X_EVENT (EVENT_DATA (e)).xany.serial);
#endif
#ifdef HAVE_GTK
	if (CONSOLE_GTK_P (con))
	  return HASH2 (hash, XMAGIC_DATA_GTK_EVENT (EVENT_DATA (e)));
#endif
#ifdef HAVE_MS_WINDOWS
	if (CONSOLE_MSWINDOWS_P (con))
	  return HASH2 (hash, XMAGIC_DATA_MSWINDOWS_EVENT (EVENT_DATA (e)));
#endif
	abort ();
	return 0;
      }

    case empty_event:
    case dead_event:
      return hash;

    default:
      abort ();
    }

  return 0; /* unreached */
}

static unsigned long
key_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 (LISP_HASH (XKEY_DATA_KEYSYM (obj)),
                XKEY_DATA_MODIFIERS (obj));
}

static unsigned long
button_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 (XBUTTON_DATA_BUTTON (obj), XBUTTON_DATA_MODIFIERS (obj));
}

static unsigned long
motion_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 (XMOTION_DATA_X (obj), XMOTION_DATA_Y (obj));
}

static unsigned long
process_data_hash (Lisp_Object obj, int depth)
{
  return LISP_HASH (XPROCESS_DATA_PROCESS (obj));
}

static unsigned long
timeout_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 (internal_hash (XTIMEOUT_DATA_FUNCTION (obj), depth + 1),
                internal_hash (XTIMEOUT_DATA_OBJECT (obj), depth + 1));
}

static unsigned long
eval_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 (internal_hash (XEVAL_DATA_FUNCTION (obj), depth + 1),
                internal_hash (XEVAL_DATA_OBJECT (obj), depth + 1));
}

static unsigned long
misc_user_data_hash (Lisp_Object obj, int depth)
{
  return HASH4 (internal_hash (XMISC_USER_DATA_FUNCTION (obj), depth + 1),
                internal_hash (XMISC_USER_DATA_OBJECT (obj), depth + 1),
                XMISC_USER_DATA_BUTTON (obj), XMISC_USER_DATA_MODIFIERS (obj));
}

static unsigned long
magic_eval_data_hash (Lisp_Object obj, int depth)
{
  return HASH2 ((unsigned long) XMAGIC_EVAL_DATA_INTERNAL_FUNCTION (obj),
                internal_hash (XMAGIC_EVAL_DATA_OBJECT (obj), depth + 1));
}

static unsigned long
magic_data_hash (Lisp_Object obj, int depth)
{assert(0); return 1;}

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("key-data", key_data,
				     0, /*dumpable-flag*/
				     mark_key_data, 
				     print_key_data, 0, 
				     key_data_equal, key_data_hash,
				     key_data_description, 
				     Lisp_Key_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("button-data", button_data,
				     0, /*dumpable-flag*/
				     mark_button_data, print_button_data, 0, 
				     button_data_equal, button_data_hash,
				     button_data_description, 
				     Lisp_Button_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("motion-data", motion_data,
				     0, /*dumpable-flag*/
				     mark_motion_data, print_motion_data, 0, 
				     motion_data_equal, motion_data_hash,
				     motion_data_description,
				     Lisp_Motion_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("process-data", process_data,
				     0, /*dumpable-flag*/
				     mark_process_data, 
				     print_process_data, 0,
				     process_data_equal, process_data_hash,
				     process_data_description,
				     Lisp_Process_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("timeout-data", timeout_data,
				     0, /*dumpable-flag*/
				     mark_timeout_data,
				     print_timeout_data, 0,
				     timeout_data_equal, timeout_data_hash,
				     timeout_data_description,
				     Lisp_Timeout_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("eval-data", eval_data,
				     0, /*dumpable-flag*/
				     mark_eval_data, 
				     print_eval_data, 0, 
				     eval_data_equal, eval_data_hash,
				     eval_data_description,
				     Lisp_Eval_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("misc-user-data", misc_user_data,
				     0, /*dumpable-flag*/
				     mark_misc_user_data,
				     print_misc_user_data,
				     0, misc_user_data_equal, 
				     misc_user_data_hash,
				     misc_user_data_description, 
				     Lisp_Misc_User_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("magic-eval-data", magic_eval_data,
				     0, /*dumpable-flag*/
				     mark_magic_eval_data, 
				     print_magic_eval_data, 0, 
				     magic_eval_data_equal,
				     magic_eval_data_hash,
				     magic_eval_data_description, 
				     Lisp_Magic_Eval_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("magic-data", magic_data,
				     0, /*dumpable-flag*/
				     mark_magic_data, print_magic_data, 0,
				     magic_data_equal, magic_data_hash,
				     magic_data_description,
				     Lisp_Magic_Data);



#else /* not USE_KKCC */
/* Make sure we lose quickly if we try to use this event */
static void
deinitialize_event (Lisp_Object ev)
{
  int i;
  Lisp_Event *event = XEVENT (ev);

  for (i = 0; i < (int) (sizeof (Lisp_Event) / sizeof (int)); i++)
    ((int *) event) [i] = 0xdeadbeef;
  event->event_type = dead_event;
  event->channel = Qnil;
  set_lheader_implementation (&event->lheader, &lrecord_event);
  XSET_EVENT_NEXT (ev, Qnil);
}

/* Set everything to zero or nil so that it's predictable. */
void
zero_event (Lisp_Event *e)
{
  xzero (*e);
  set_lheader_implementation (&e->lheader, &lrecord_event);
  e->event_type = empty_event;
  e->next = Qnil;
  e->channel = Qnil;
}

static Lisp_Object
mark_event (Lisp_Object obj)
{
  Lisp_Event *event = XEVENT (obj);

  switch (event->event_type)
    {
    case key_press_event:
      mark_object (event->event.key.keysym);
      break;
    case process_event:
      mark_object (event->event.process.process);
      break;
    case timeout_event:
      mark_object (event->event.timeout.function);
      mark_object (event->event.timeout.object);
      break;
    case eval_event:
    case misc_user_event:
      mark_object (event->event.eval.function);
      mark_object (event->event.eval.object);
      break;
    case magic_eval_event:
      mark_object (event->event.magic_eval.object);
      break;
    case button_press_event:
    case button_release_event:
    case pointer_motion_event:
    case magic_event:
    case empty_event:
    case dead_event:
      break;
    default:
      abort ();
    }
  mark_object (event->channel);
  return event->next;
}

static void
print_event_1 (const char *str, Lisp_Object obj, Lisp_Object printcharfun)
{
  DECLARE_EISTRING_MALLOC (ei);
  write_c_string (printcharfun, str);
  format_event_object (ei, XEVENT (obj), 0);
  write_eistring (printcharfun, ei);
  eifree (ei);
}

static void
print_event (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  if (print_readably)
    printing_unreadable_object ("#<event>");

  switch (XEVENT (obj)->event_type)
    {
    case key_press_event:
      print_event_1 ("#<keypress-event ", obj, printcharfun);
      break;
    case button_press_event:
      print_event_1 ("#<buttondown-event ", obj, printcharfun);
      break;
    case button_release_event:
      print_event_1 ("#<buttonup-event ", obj, printcharfun);
      break;
    case magic_event:
    case magic_eval_event:
      print_event_1 ("#<magic-event ", obj, printcharfun);
      break;
    case pointer_motion_event:
      {
	Lisp_Object Vx, Vy;
	Vx = Fevent_x_pixel (obj);
	assert (INTP (Vx));
	Vy = Fevent_y_pixel (obj);
	assert (INTP (Vy));
	write_fmt_string (printcharfun, "#<motion-event %ld, %ld",
			  (long) XINT (Vx), (long) XINT (Vy));
	break;
      }
    case process_event:
	write_fmt_string_lisp (printcharfun, "#<process-event %S", 1, XEVENT (obj)->event.process.process);
	break;
    case timeout_event:
	write_fmt_string_lisp (printcharfun, "#<timeout-event %S", 1, XEVENT (obj)->event.timeout.object);
	break;
    case empty_event:
	write_c_string (printcharfun, "#<empty-event");
	break;
    case misc_user_event:
	write_fmt_string_lisp (printcharfun, "#<misc-user-event (%S", 1, XEVENT (obj)->event.misc.function);
	write_fmt_string_lisp (printcharfun, " %S)", 1, XEVENT (obj)->event.misc.object);
	break;
    case eval_event:
	write_fmt_string_lisp (printcharfun, "#<eval-event (%S", 1, XEVENT (obj)->event.eval.function);
	write_fmt_string_lisp (printcharfun, " %S)", 1, XEVENT (obj)->event.eval.object);
	break;
    case dead_event:
	write_c_string (printcharfun, "#<DEALLOCATED-EVENT");
	break;
    default:
	write_c_string (printcharfun, "#<UNKNOWN-EVENT-TYPE");
	break;
      }
  write_c_string (printcharfun, ">");
}

static int
event_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Lisp_Event *e1 = XEVENT (obj1);
  Lisp_Event *e2 = XEVENT (obj2);

  if (e1->event_type != e2->event_type) return 0;
  if (!EQ (e1->channel, e2->channel)) return 0;
/*  if (e1->timestamp != e2->timestamp) return 0; */
  switch (e1->event_type)
    {
    default: abort ();

    case process_event:
      return EQ (e1->event.process.process, e2->event.process.process);

    case timeout_event:
      return (internal_equal (e1->event.timeout.function,
			      e2->event.timeout.function, 0) &&
	      internal_equal (e1->event.timeout.object,
			      e2->event.timeout.object, 0));

    case key_press_event:
      return (EQ (e1->event.key.keysym, e2->event.key.keysym) &&
	      (e1->event.key.modifiers == e2->event.key.modifiers));

    case button_press_event:
    case button_release_event:
      return (e1->event.button.button    == e2->event.button.button &&
	      e1->event.button.modifiers == e2->event.button.modifiers);

    case pointer_motion_event:
      return (e1->event.motion.x == e2->event.motion.x &&
	      e1->event.motion.y == e2->event.motion.y);

    case misc_user_event:
      return (internal_equal (e1->event.eval.function,
			      e2->event.eval.function, 0) &&
	      internal_equal (e1->event.eval.object,
			      e2->event.eval.object, 0) &&
	      /* is this really needed for equality
	         or is x and y also important? */
	      e1->event.misc.button    == e2->event.misc.button &&
	      e1->event.misc.modifiers == e2->event.misc.modifiers);

    case eval_event:
      return (internal_equal (e1->event.eval.function,
			      e2->event.eval.function, 0) &&
	      internal_equal (e1->event.eval.object,
			      e2->event.eval.object, 0));

    case magic_eval_event:
      return (e1->event.magic_eval.internal_function ==
	      e2->event.magic_eval.internal_function &&
	      internal_equal (e1->event.magic_eval.object,
			      e2->event.magic_eval.object, 0));

    case magic_event:
      return event_stream_compare_magic_event (e1, e2);

    case empty_event:      /* Empty and deallocated events are equal. */
    case dead_event:
      return 1;
    }
}

static Hashcode
event_hash (Lisp_Object obj, int depth)
{
  Lisp_Event *e = XEVENT (obj);
  Hashcode hash;

  hash = HASH2 (e->event_type, LISP_HASH (e->channel));
  switch (e->event_type)
    {
    case process_event:
      return HASH2 (hash, LISP_HASH (e->event.process.process));

    case timeout_event:
      return HASH3 (hash, internal_hash (e->event.timeout.function, depth + 1),
		    internal_hash (e->event.timeout.object, depth + 1));

    case key_press_event:
      return HASH3 (hash, LISP_HASH (e->event.key.keysym),
		    e->event.key.modifiers);

    case button_press_event:
    case button_release_event:
      return HASH3 (hash, e->event.button.button, e->event.button.modifiers);

    case pointer_motion_event:
      return HASH3 (hash, e->event.motion.x, e->event.motion.y);

    case misc_user_event:
      return HASH5 (hash, internal_hash (e->event.misc.function, depth + 1),
		    internal_hash (e->event.misc.object, depth + 1),
		    e->event.misc.button, e->event.misc.modifiers);

    case eval_event:
      return HASH3 (hash, internal_hash (e->event.eval.function, depth + 1),
		    internal_hash (e->event.eval.object, depth + 1));

    case magic_eval_event:
      return HASH3 (hash,
		    (Hashcode) e->event.magic_eval.internal_function,
		    internal_hash (e->event.magic_eval.object, depth + 1));

    case magic_event:
      return HASH2 (hash, event_stream_hash_magic_event (e));

    case empty_event:
    case dead_event:
      return hash;

    default:
      abort ();
    }

  return 0; /* unreached */
}
#endif /* not USE_KKCC */


#ifdef USE_KKCC
DEFINE_BASIC_LRECORD_IMPLEMENTATION ("event", event,
				     0, /*dumpable-flag*/
				     mark_event, print_event, 0, event_equal,
				     event_hash, 0/*event_description*/, Lisp_Event);
#else /* not USE_KKCC */
DEFINE_BASIC_LRECORD_IMPLEMENTATION ("event", event,
				     mark_event, print_event, 0, event_equal,
				     event_hash, 0, Lisp_Event);
#endif /* not USE_KKCC */

DEFUN ("make-event", Fmake_event, 0, 2, 0, /*
Return a new event of type TYPE, with properties described by PLIST.

TYPE is a symbol, either `empty', `key-press', `button-press',
 `button-release', `misc-user' or `motion'.  If TYPE is nil, it
 defaults to `empty'.

PLIST is a property list, the properties being compatible to those
 returned by `event-properties'.  The following properties are
 allowed:

 channel	-- The event channel, a frame or a console.  For
		   button-press, button-release, misc-user and motion events,
		   this must be a frame.  For key-press events, it must be
                   a console.  If channel is unspecified, it will be set to
                   the selected frame or selected console, as appropriate.
 key		-- The event key, a symbol or character.  Allowed only for
		   keypress events.
 button		-- The event button, integer 1, 2 or 3.  Allowed for
		   button-press, button-release and misc-user events.
 modifiers	-- The event modifiers, a list of modifier symbols.  Allowed
		   for key-press, button-press, button-release, motion and
		   misc-user events.
 function       -- Function. Allowed for misc-user events only.
 object         -- An object, function's parameter. Allowed for misc-user
                   events only.
 x		-- The event X coordinate, an integer.  This is relative
		   to the left of CHANNEL's root window.  Allowed for
		   motion, button-press, button-release and misc-user events.
 y		-- The event Y coordinate, an integer.  This is relative
		   to the top of CHANNEL's root window.  Allowed for
		   motion, button-press, button-release and misc-user events.
 timestamp	-- The event timestamp, a non-negative integer.  Allowed for
		   all types of events.  If unspecified, it will be set to 0
		   by default.

For event type `empty', PLIST must be nil.
 `button-release', or `motion'.  If TYPE is left out, it defaults to
 `empty'.
PLIST is a list of properties, as returned by `event-properties'.  Not
 all properties are allowed for all kinds of events, and some are
 required.

WARNING: the event object returned may be a reused one; see the function
 `deallocate-event'.
*/
       (type, plist))
{
  Lisp_Object event = Qnil;
  Lisp_Event *e;
  EMACS_INT coord_x = 0, coord_y = 0;
  struct gcpro gcpro1;

  GCPRO1 (event);

  if (NILP (type))
    type = Qempty;

  if (!NILP (Vevent_resource))
    {
      event = Vevent_resource;
      Vevent_resource = XEVENT_NEXT (event);
    }
  else
    {
      event = allocate_event ();
    }
  e = XEVENT (event);
  zero_event (e);

  if (EQ (type, Qempty))
    {
      /* For empty event, we return immediately, without processing
         PLIST.  In fact, processing PLIST would be wrong, because the
         sanitizing process would fill in the properties
         (e.g. CHANNEL), which we don't want in empty events.  */
#ifdef USE_KKCC
      set_event_type (e, empty_event);
#else /* not USE_KKCC */
      e->event_type = empty_event;
#endif /* not USE_KKCC */
      if (!NILP (plist))
	invalid_operation ("Cannot set properties of empty event", plist);
      UNGCPRO;
      return event;
    }
  else if (EQ (type, Qkey_press))
    {
#ifdef USE_KKCC
      set_event_type (e, key_press_event);
      XSET_KEY_DATA_KEYSYM (EVENT_DATA (e), Qunbound);
#else /* not USE_KKCC */
      e->event_type = key_press_event;
      e->event.key.keysym = Qunbound;
#endif /* not USE_KKCC */
    }
  else if (EQ (type, Qbutton_press))
#ifdef USE_KKCC
    set_event_type (e, button_press_event);
#else /* not USE_KKCC */
    e->event_type = button_press_event;
#endif /* not USE_KKCC */
  else if (EQ (type, Qbutton_release))
#ifdef USE_KKCC
    set_event_type (e, button_release_event);
#else /* not USE_KKCC */
    e->event_type = button_release_event;
#endif /* not USE_KKCC */
  else if (EQ (type, Qmotion))
#ifdef USE_KKCC
    set_event_type (e, pointer_motion_event);
#else /* not USE_KKCC */
    e->event_type = pointer_motion_event;
#endif /* not USE_KKCC */
  else if (EQ (type, Qmisc_user))
    {
#ifdef USE_KKCC
      set_event_type (e, misc_user_event);
      XSET_MISC_USER_DATA_FUNCTION (EVENT_DATA (e), Qnil); 
      XSET_MISC_USER_DATA_OBJECT (EVENT_DATA (e), Qnil);
#else /* not USE_KKCC */
      e->event_type = misc_user_event;
      e->event.eval.function = e->event.eval.object = Qnil;
#endif /* not USE_KKCC */
    }
  else
    {
      /* Not allowed: Qprocess, Qtimeout, Qmagic, Qeval, Qmagic_eval.  */
      invalid_constant ("Invalid event type", type);
    }

  EVENT_CHANNEL (e) = Qnil;

  plist = Fcopy_sequence (plist);
  Fcanonicalize_plist (plist, Qnil);

#define WRONG_EVENT_TYPE_FOR_PROPERTY(event_type, prop) \
  invalid_argument_2 ("Invalid property for event type", prop, event_type)

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (keyword, value, plist)
      {
	if (EQ (keyword, Qchannel))
	  {
#ifdef USE_KKCC
	    if (EVENT_TYPE(e) == key_press_event)
#else /* not USE_KKCC */
	    if (e->event_type == key_press_event)
#endif /* not USE_KKCC */
	      {
		if (!CONSOLEP (value))
		  value = wrong_type_argument (Qconsolep, value);
	      }
	    else
	      {
		if (!FRAMEP (value))
		  value = wrong_type_argument (Qframep, value);
	      }
	    EVENT_CHANNEL (e) = value;
	  }
	else if (EQ (keyword, Qkey))
	  {
#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case key_press_event:
		if (!SYMBOLP (value) && !CHARP (value))
		  invalid_argument ("Invalid event key", value);
#ifdef USE_KKCC
		XSET_KEY_DATA_KEYSYM (EVENT_DATA(e), value);
#else /* not USE_KKCC */
		e->event.key.keysym = value;
#endif /* not USE_KKCC */
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qbutton))
	  {
	    CHECK_NATNUM (value);
	    check_int_range (XINT (value), 0, 7);

#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case button_press_event:
	      case button_release_event:
#ifdef USE_KKCC
		XSET_BUTTON_DATA_BUTTON (EVENT_DATA (e), XINT (value));
#else /* not USE_KKCC */
		e->event.button.button = XINT (value);
#endif /* not USE_KKCC */
		break;
	      case misc_user_event:
#ifdef USE_KKCC
		XSET_MISC_USER_DATA_BUTTON (EVENT_DATA (e), XINT (value));
#else /* not USE_KKCC */
		e->event.misc.button = XINT (value);
#endif /* not USE_KKCC */
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qmodifiers))
	  {
	    int modifiers = 0;

	    EXTERNAL_LIST_LOOP_2 (sym, value)
	      {
		if      (EQ (sym, Qcontrol)) modifiers |= XEMACS_MOD_CONTROL;
		else if (EQ (sym, Qmeta))    modifiers |= XEMACS_MOD_META;
		else if (EQ (sym, Qsuper))   modifiers |= XEMACS_MOD_SUPER;
		else if (EQ (sym, Qhyper))   modifiers |= XEMACS_MOD_HYPER;
		else if (EQ (sym, Qalt))     modifiers |= XEMACS_MOD_ALT;
		else if (EQ (sym, Qsymbol))  modifiers |= XEMACS_MOD_ALT;
		else if (EQ (sym, Qshift))   modifiers |= XEMACS_MOD_SHIFT;
		else if (EQ (sym, Qbutton1))   modifiers |= XEMACS_MOD_BUTTON1;
		else if (EQ (sym, Qbutton2))   modifiers |= XEMACS_MOD_BUTTON2;
		else if (EQ (sym, Qbutton3))   modifiers |= XEMACS_MOD_BUTTON3;
		else if (EQ (sym, Qbutton4))   modifiers |= XEMACS_MOD_BUTTON4;
		else if (EQ (sym, Qbutton5))   modifiers |= XEMACS_MOD_BUTTON5;
		else
		  invalid_constant ("Invalid key modifier", sym);
	      }

#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case key_press_event:
#ifdef USE_KKCC
                XSET_KEY_DATA_MODIFIERS (EVENT_DATA (e), modifiers);
#else /* not USE_KKCC */
		e->event.key.modifiers = modifiers;
#endif /* not USE_KKCC */
		break;
	      case button_press_event:
	      case button_release_event:
#ifdef USE_KKCC
                XSET_BUTTON_DATA_MODIFIERS (EVENT_DATA (e), modifiers);
#else /* not USE_KKCC */
		e->event.button.modifiers = modifiers;
#endif /* not USE_KKCC */
		break;
	      case pointer_motion_event:
#ifdef USE_KKCC
                XSET_MOTION_DATA_MODIFIERS (EVENT_DATA (e), modifiers);
#else /* not USE_KKCC */
		e->event.motion.modifiers = modifiers;
#endif /* not USE_KKCC */
		break;
	      case misc_user_event:
#ifdef USE_KKCC
                XSET_MISC_USER_DATA_MODIFIERS (EVENT_DATA (e), modifiers);
#else /* not USE_KKCC */
		e->event.misc.modifiers = modifiers;
#endif /* not USE_KKCC */
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qx))
	  {
#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case pointer_motion_event:
	      case button_press_event:
	      case button_release_event:
	      case misc_user_event:
		/* Allow negative values, so we can specify toolbar
		   positions.  */
		CHECK_INT (value);
		coord_x = XINT (value);
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qy))
	  {
#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case pointer_motion_event:
	      case button_press_event:
	      case button_release_event:
	      case misc_user_event:
		/* Allow negative values; see above. */
		CHECK_INT (value);
		coord_y = XINT (value);
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qtimestamp))
	  {
	    CHECK_NATNUM (value);
#ifdef USE_KKCC
	    SET_EVENT_TIMESTAMP (e, XINT (value));
#else /* not USE_KKCC */
	    e->timestamp = XINT (value);
#endif /* not USE_KKCC */
	  }
	else if (EQ (keyword, Qfunction))
	  {
#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case misc_user_event:
#ifdef USE_KKCC
                XSET_MISC_USER_DATA_FUNCTION (EVENT_DATA (e), value);
#else /* not USE_KKCC */
		e->event.eval.function = value;
#endif /* not USE_KKCC */
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qobject))
	  {
#ifdef USE_KKCC
	    switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
	    switch (e->event_type)
#endif /* not USE_KKCC */
	      {
	      case misc_user_event:
#ifdef USE_KKCC
                XSET_MISC_USER_DATA_OBJECT (EVENT_DATA (e), value);
#else /* not USE_KKCC */
		e->event.eval.object = value;
#endif /* not USE_KKCC */
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else
	  invalid_constant_2 ("Invalid property", keyword, value);
      }
  }

  /* Insert the channel, if missing. */
  if (NILP (EVENT_CHANNEL (e)))
    {
#ifdef USE_KKCC
      if (EVENT_TYPE (e) == key_press_event)
#else /* not USE_KKCC */
      if (e->event_type == key_press_event)
#endif /* not USE_KKCC */
	EVENT_CHANNEL (e) = Vselected_console;
      else
	EVENT_CHANNEL (e) = Fselected_frame (Qnil);
    }

  /* Fevent_properties, Fevent_x_pixel, etc. work with pixels relative
     to the frame, so we must adjust accordingly.  */
  if (FRAMEP (EVENT_CHANNEL (e)))
    {
      coord_x += FRAME_REAL_LEFT_TOOLBAR_WIDTH (XFRAME (EVENT_CHANNEL (e)));
      coord_y += FRAME_REAL_TOP_TOOLBAR_HEIGHT (XFRAME (EVENT_CHANNEL (e)));

      switch (e->event_type)
	{
	case pointer_motion_event:
#ifdef USE_KKCC
	  XSET_MOTION_DATA_X (EVENT_DATA (e), coord_x);
	  XSET_MOTION_DATA_Y (EVENT_DATA (e), coord_y);
#else /* not USE_KKCC */
	  e->event.motion.x = coord_x;
	  e->event.motion.y = coord_y;
#endif /* not USE_KKCC */
	  break;
	case button_press_event:
	case button_release_event:
#ifdef USE_KKCC
	  XSET_BUTTON_DATA_X (EVENT_DATA (e), coord_x);
	  XSET_BUTTON_DATA_Y (EVENT_DATA (e), coord_y);
#else /* not USE_KKCC */
	  e->event.button.x = coord_x;
	  e->event.button.y = coord_y;
#endif /* not USE_KKCC */
	  break;
	case misc_user_event:
#ifdef USE_KKCC
	  XSET_MISC_USER_DATA_X (EVENT_DATA (e), coord_x);
	  XSET_MISC_USER_DATA_Y (EVENT_DATA (e), coord_y);
#else /* not USE_KKCC */
	  e->event.misc.x = coord_x;
	  e->event.misc.y = coord_y;
#endif /* not USE_KKCC */
	  break;
	default:
	  abort();
	}
    }

  /* Finally, do some more validation.  */
#ifdef USE_KKCC
  switch (EVENT_TYPE(e))
#else /* not USE_KKCC */
  switch (e->event_type)
#endif /* not USE_KKCC */
    {
    case key_press_event:
#ifdef USE_KKCC
      if (UNBOUNDP (XKEY_DATA_KEYSYM (EVENT_DATA (e))))
#else /* not USE_KKCC */
      if (UNBOUNDP (e->event.key.keysym))
#endif /* not USE_KKCC */
	sferror ("A key must be specified to make a keypress event",
		      plist);
      break;
    case button_press_event:
#ifdef USE_KKCC
      if (!XBUTTON_DATA_BUTTON (EVENT_DATA (e)))
#else /* not USE_KKCC */
      if (!e->event.button.button)
#endif /* not USE_KKCC */
	sferror
	  ("A button must be specified to make a button-press event",
	   plist);
      break;
    case button_release_event:
#ifdef USE_KKCC
      if (!XBUTTON_DATA_BUTTON (EVENT_DATA (e)))
#else /* not USE_KKCC */
      if (!e->event.button.button)
#endif /* not USE_KKCC */
	sferror
	  ("A button must be specified to make a button-release event",
	   plist);
      break;
    case misc_user_event:
#ifdef USE_KKCC
      if (NILP (XMISC_USER_DATA_FUNCTION (EVENT_DATA (e))))
#else /* not USE_KKCC */
      if (NILP (e->event.misc.function))
#endif /* not USE_KKCC */
	sferror ("A function must be specified to make a misc-user event",
		      plist);
      break;
    default:
      break;
    }

  UNGCPRO;
  return event;
}


#ifdef USE_KKCC

Lisp_Object 
make_key_data (void)
{
  Lisp_Object data = allocate_key_data ();

  XSET_KEY_DATA_KEYSYM (data, Qnil);
  XSET_KEY_DATA_MODIFIERS (data, 0);  
  
  return data;
}

Lisp_Object
make_button_data (void)
{
  Lisp_Object data = allocate_button_data ();

  XSET_BUTTON_DATA_BUTTON (data, 0);
  XSET_BUTTON_DATA_MODIFIERS (data, 0);
  XSET_BUTTON_DATA_X (data, 0);
  XSET_BUTTON_DATA_Y (data, 0);
  
  return data;
}

Lisp_Object
make_motion_data (void)
{
  Lisp_Object data = allocate_motion_data ();

  XSET_MOTION_DATA_X (data, 0);
  XSET_MOTION_DATA_Y (data, 0);
  XSET_MOTION_DATA_MODIFIERS (data, 0);
  
  return data;
}

Lisp_Object
make_process_data (void)
{
  Lisp_Object data = allocate_process_data ();

  XSET_PROCESS_DATA_PROCESS (data, Qnil);
  
  return data;
}

Lisp_Object
make_timeout_data (void)
{
  Lisp_Object data = allocate_timeout_data ();

  XSET_TIMEOUT_DATA_INTERVAL_ID (data, 0);
  XSET_TIMEOUT_DATA_ID_NUMBER(data, 0);
  XSET_TIMEOUT_DATA_FUNCTION(data, Qnil);
  XSET_TIMEOUT_DATA_OBJECT (data, Qnil);
  
  return data;
}

Lisp_Object
make_magic_eval_data (void)
{
  Lisp_Object data = allocate_magic_eval_data ();

  XSET_MAGIC_EVAL_DATA_OBJECT (data, Qnil);
  XSET_MAGIC_EVAL_DATA_INTERNAL_FUNOBJ (data, 0);
  
  return data;
}

Lisp_Object
make_eval_data (void)
{
  Lisp_Object data = allocate_eval_data ();

  XSET_EVAL_DATA_FUNCTION (data, Qnil);
  XSET_EVAL_DATA_OBJECT (data, Qnil);  

  return data;
}

Lisp_Object
make_magic_data (void)
{
  return allocate_magic_data ();
}

Lisp_Object
make_misc_user_data (void)
{
  Lisp_Object data = allocate_misc_user_data ();

  XSET_MISC_USER_DATA_FUNCTION (data, Qnil);
  XSET_MISC_USER_DATA_OBJECT (data, Qnil);
  XSET_MISC_USER_DATA_BUTTON (data, 0);
  XSET_MISC_USER_DATA_MODIFIERS (data, 0);
  XSET_MISC_USER_DATA_X (data, 0);
  XSET_MISC_USER_DATA_Y (data, 0);

  return data;
}
#endif /* USE_KKCC */



DEFUN ("deallocate-event", Fdeallocate_event, 1, 1, 0, /*
Allow the given event structure to be reused.
You MUST NOT use this event object after calling this function with it.
You will lose.  It is not necessary to call this function, as event
objects are garbage-collected like all other objects; however, it may
be more efficient to explicitly deallocate events when you are sure
that it is safe to do so.
*/
       (event))
{
  CHECK_EVENT (event);

  if (XEVENT_TYPE (event) == dead_event)
    invalid_argument ("this event is already deallocated!", Qunbound);

  assert (XEVENT_TYPE (event) <= last_event_type);

#if 0
  {
    int i, len;

    if (EQ (event, Vlast_command_event) ||
	EQ (event, Vlast_input_event)   ||
	EQ (event, Vunread_command_event))
      abort ();

    len = XVECTOR_LENGTH (Vthis_command_keys);
    for (i = 0; i < len; i++)
      if (EQ (event, XVECTOR_DATA (Vthis_command_keys) [i]))
	abort ();
    if (!NILP (Vrecent_keys_ring))
      {
	int recent_ring_len = XVECTOR_LENGTH (Vrecent_keys_ring);
	for (i = 0; i < recent_ring_len; i++)
	  if (EQ (event, XVECTOR_DATA (Vrecent_keys_ring) [i]))
	    abort ();
      }
  }
#endif /* 0 */

  assert (!EQ (event, Vevent_resource));
  deinitialize_event (event);
#ifndef ALLOC_NO_POOLS
  XSET_EVENT_NEXT (event, Vevent_resource);
  Vevent_resource = event;
#endif
  return Qnil;
}

#ifdef USE_KKCC
void
copy_event_data (Lisp_Object dest, Lisp_Object src)
{
  switch (XRECORD_LHEADER (dest)->type) {
  case lrecord_type_key_data:
    XSET_KEY_DATA_KEYSYM (dest, XKEY_DATA_KEYSYM (src));
    XSET_KEY_DATA_MODIFIERS (dest, XKEY_DATA_MODIFIERS (src));
    break;
  case lrecord_type_button_data:
    XSET_BUTTON_DATA_BUTTON (dest, XBUTTON_DATA_BUTTON (src));
    XSET_BUTTON_DATA_MODIFIERS (dest, XBUTTON_DATA_MODIFIERS (src));
    XSET_BUTTON_DATA_X (dest, XBUTTON_DATA_X (src));
    XSET_BUTTON_DATA_Y (dest, XBUTTON_DATA_Y (src));
    break;
  case lrecord_type_motion_data:
    XSET_MOTION_DATA_X (dest, XMOTION_DATA_X (src));
    XSET_MOTION_DATA_Y (dest, XMOTION_DATA_Y (src));
    XSET_MOTION_DATA_MODIFIERS (dest, XMOTION_DATA_MODIFIERS (src));
    break;
  case lrecord_type_process_data:
    XSET_PROCESS_DATA_PROCESS (dest, XPROCESS_DATA_PROCESS (src));
    break;
  case lrecord_type_timeout_data:
    XSET_TIMEOUT_DATA_INTERVAL_ID (dest, XTIMEOUT_DATA_INTERVAL_ID (src));
    XSET_TIMEOUT_DATA_ID_NUMBER (dest, XTIMEOUT_DATA_ID_NUMBER (src));
    XSET_TIMEOUT_DATA_FUNCTION (dest, XTIMEOUT_DATA_FUNCTION (src));
    XSET_TIMEOUT_DATA_OBJECT (dest, XTIMEOUT_DATA_OBJECT (src));
    break;
  case lrecord_type_eval_data:
    XSET_EVAL_DATA_FUNCTION (dest, XEVAL_DATA_FUNCTION (src));
    XSET_EVAL_DATA_OBJECT (dest, XEVAL_DATA_OBJECT (src));
    break;    
  case lrecord_type_misc_user_data:
    XSET_MISC_USER_DATA_FUNCTION (dest, XMISC_USER_DATA_FUNCTION (src));
    XSET_MISC_USER_DATA_OBJECT (dest, XMISC_USER_DATA_OBJECT (src));
    XSET_MISC_USER_DATA_BUTTON (dest, XMISC_USER_DATA_BUTTON (src));
    XSET_MISC_USER_DATA_MODIFIERS (dest, XMISC_USER_DATA_MODIFIERS (src));
    XSET_MISC_USER_DATA_X (dest, XMISC_USER_DATA_X (src));
    XSET_MISC_USER_DATA_Y (dest, XMISC_USER_DATA_Y (src));
    break;
  case lrecord_type_magic_eval_data:
    XSET_MAGIC_EVAL_DATA_INTERNAL_FUNCTION (dest, XMAGIC_EVAL_DATA_INTERNAL_FUNCTION (src));
    XSET_MAGIC_EVAL_DATA_OBJECT (dest, XMAGIC_EVAL_DATA_OBJECT (src));
    break;
  case lrecord_type_magic_data:
    XSET_MAGIC_DATA_UNDERLYING (dest, XMAGIC_DATA_UNDERLYING (src));
    break;
  default:
    break;
  }
}
#endif /* USE_KKCC */

DEFUN ("copy-event", Fcopy_event, 1, 2, 0, /*
Make a copy of the event object EVENT1.
If a second event argument EVENT2 is given, EVENT1 is copied into
EVENT2 and EVENT2 is returned.  If EVENT2 is not supplied (or is nil)
then a new event will be made as with `make-event'.  See also the
function `deallocate-event'.
*/
       (event1, event2))
{
  CHECK_LIVE_EVENT (event1);
  if (NILP (event2))
    event2 = Fmake_event (Qnil, Qnil);
  else
    {
      CHECK_LIVE_EVENT (event2);
      if (EQ (event1, event2))
	return signal_continuable_error_2
	  (Qinvalid_argument,
	   "copy-event called with `eq' events", event1, event2);
    }

  assert (XEVENT_TYPE (event1) <= last_event_type);
  assert (XEVENT_TYPE (event2) <= last_event_type);

#ifdef USE_KKCC
  XSET_EVENT_TYPE (event2, XEVENT_TYPE (event1));
  XSET_EVENT_CHANNEL (event2, XEVENT_CHANNEL (event1));
  XSET_EVENT_TIMESTAMP (event2, XEVENT_TIMESTAMP (event1));
  copy_event_data (XEVENT_DATA (event2), XEVENT_DATA (event1));

  return event2;
#else /* not USE_KKCC */
  {
    Lisp_Event *ev2 = XEVENT (event2);
    Lisp_Event *ev1 = XEVENT (event1);

    ev2->event_type = ev1->event_type;
    ev2->channel    = ev1->channel;
    ev2->timestamp  = ev1->timestamp;
    ev2->event      = ev1->event;

    return event2;
  }
#endif /* not USE_KKCC */
}


/************************************************************************/
/*                          event chain functions                       */
/************************************************************************/

/* Given a chain of events (or possibly nil), deallocate them all. */

void
deallocate_event_chain (Lisp_Object event_chain)
{
  while (!NILP (event_chain))
    {
      Lisp_Object next = XEVENT_NEXT (event_chain);
      Fdeallocate_event (event_chain);
      event_chain = next;
    }
}

/* Return the last event in a chain.
   NOTE: You cannot pass nil as a value here!  The routine will
   abort if you do. */

Lisp_Object
event_chain_tail (Lisp_Object event_chain)
{
  while (1)
    {
      Lisp_Object next = XEVENT_NEXT (event_chain);
      if (NILP (next))
	return event_chain;
      event_chain = next;
    }
}

/* Enqueue a single event onto the end of a chain of events.
   HEAD points to the first event in the chain, TAIL to the last event.
   If the chain is empty, both values should be nil. */

void
enqueue_event (Lisp_Object event, Lisp_Object *head, Lisp_Object *tail)
{
  assert (NILP (XEVENT_NEXT (event)));
  assert (!EQ (*tail, event));

  if (!NILP (*tail))
    XSET_EVENT_NEXT (*tail, event);
  else
   *head = event;
  *tail = event;

  assert (!EQ (event, XEVENT_NEXT (event)));
}

/* Remove an event off the head of a chain of events and return it.
   HEAD points to the first event in the chain, TAIL to the last event. */

Lisp_Object
dequeue_event (Lisp_Object *head, Lisp_Object *tail)
{
  Lisp_Object event;

  event = *head;
  *head = XEVENT_NEXT (event);
  XSET_EVENT_NEXT (event, Qnil);
  if (NILP (*head))
    *tail = Qnil;
  return event;
}

/* Enqueue a chain of events (or possibly nil) onto the end of another
   chain of events.  HEAD points to the first event in the chain being
   queued onto, TAIL to the last event.  If the chain is empty, both values
   should be nil. */

void
enqueue_event_chain (Lisp_Object event_chain, Lisp_Object *head,
		     Lisp_Object *tail)
{
  if (NILP (event_chain))
    return;

  if (NILP (*head))
    {
      *head = event_chain;
      *tail = event_chain;
    }
  else
    {
      XSET_EVENT_NEXT (*tail, event_chain);
      *tail = event_chain_tail (event_chain);
    }
}

/* Return the number of events (possibly 0) on an event chain. */

int
event_chain_count (Lisp_Object event_chain)
{
  Lisp_Object event;
  int n = 0;

  EVENT_CHAIN_LOOP (event, event_chain)
    n++;

  return n;
}

/* Find the event before EVENT in an event chain.  This aborts
   if the event is not in the chain. */

Lisp_Object
event_chain_find_previous (Lisp_Object event_chain, Lisp_Object event)
{
  Lisp_Object previous = Qnil;

  while (!NILP (event_chain))
    {
      if (EQ (event_chain, event))
	return previous;
      previous = event_chain;
      event_chain = XEVENT_NEXT (event_chain);
    }

  abort ();
  return Qnil;
}

Lisp_Object
event_chain_nth (Lisp_Object event_chain, int n)
{
  Lisp_Object event;
  EVENT_CHAIN_LOOP (event, event_chain)
    {
      if (!n)
	return event;
      n--;
    }
  return Qnil;
}

/* Return a freshly allocated copy of all events in the given chain. */

Lisp_Object
copy_event_chain (Lisp_Object event_chain)
{
  Lisp_Object new_chain = Qnil;
  Lisp_Object new_chain_tail = Qnil;
  Lisp_Object event;

  EVENT_CHAIN_LOOP (event, event_chain)
    {
      Lisp_Object copy = Fcopy_event (event, Qnil);
      enqueue_event (copy, &new_chain, &new_chain_tail);
    }

  return new_chain;
}

/* Given a pointer (maybe nil) into an old chain (also maybe nil, if
   pointer is nil) and a new chain which is a copy of the old, return
   the corresponding new pointer. */
Lisp_Object
transfer_event_chain_pointer (Lisp_Object pointer, Lisp_Object old_chain,
			      Lisp_Object new_chain)
{
  if (NILP (pointer))
    return Qnil;
  assert (!NILP (old_chain));
#ifdef ERROR_CHECK_STRUCTURES
  /* make sure we're actually in the chain */
  event_chain_find_previous (old_chain, pointer);
  assert (event_chain_count (old_chain) == event_chain_count (new_chain));
#endif /* ERROR_CHECK_STRUCTURES */
  return event_chain_nth (new_chain,
			  event_chain_count (old_chain) -
			  event_chain_count (pointer));
}


/************************************************************************/
/*                         higher level functions                       */
/************************************************************************/

Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn, QKescape,
 QKspace, QKdelete;

int
command_event_p (Lisp_Object event)
{
  switch (XEVENT_TYPE (event))
    {
    case key_press_event:
    case button_press_event:
    case button_release_event:
    case misc_user_event:
      return 1;
    default:
      return 0;
    }
}


void
character_to_event (Ichar c, Lisp_Event *event, struct console *con,
		    int use_console_meta_flag, int do_backspace_mapping)
{
  Lisp_Object k = Qnil;
  int m = 0;
#ifdef USE_KKCC
  if (EVENT_TYPE (event) == dead_event)
#else /* not USE_KKCC */
  if (event->event_type == dead_event)
#endif /* not USE_KKCC */
    invalid_argument ("character-to-event called with a deallocated event!", Qunbound);

#ifndef MULE
  c &= 255;
#endif
  if (c > 127 && c <= 255)
    {
      int meta_flag = 1;
      if (use_console_meta_flag && CONSOLE_TTY_P (con))
	meta_flag = TTY_FLAGS (con).meta_key;
      switch (meta_flag)
	{
	case 0: /* ignore top bit; it's parity */
	  c -= 128;
	  break;
	case 1: /* top bit is meta */
	  c -= 128;
	  m = XEMACS_MOD_META;
	  break;
	default: /* this is a real character */
	  break;
	}
    }
  if (c < ' ') c += '@', m |= XEMACS_MOD_CONTROL;
  if (m & XEMACS_MOD_CONTROL)
    {
      switch (c)
	{
	case 'I': k = QKtab;	  m &= ~XEMACS_MOD_CONTROL; break;
	case 'J': k = QKlinefeed; m &= ~XEMACS_MOD_CONTROL; break;
	case 'M': k = QKreturn;	  m &= ~XEMACS_MOD_CONTROL; break;
	case '[': k = QKescape;	  m &= ~XEMACS_MOD_CONTROL; break;
	default:
#if defined(HAVE_TTY)
	  if (do_backspace_mapping &&
	      CHARP (con->tty_erase_char) &&
	      c - '@' == XCHAR (con->tty_erase_char))
	    {
	      k = QKbackspace;
	      m &= ~XEMACS_MOD_CONTROL;
	    }
#endif /* defined(HAVE_TTY) && !defined(CYGWIN) */
	  break;
	}
      if (c >= 'A' && c <= 'Z') c -= 'A'-'a';
    }
#if defined(HAVE_TTY)
  else if (do_backspace_mapping &&
	   CHARP (con->tty_erase_char) && c == XCHAR (con->tty_erase_char))
    k = QKbackspace;
#endif /* defined(HAVE_TTY) && !defined(CYGWIN) */
  else if (c == 127)
    k = QKdelete;
  else if (c == ' ')
    k = QKspace;

#ifdef USE_KKCC
  set_event_type (event, key_press_event);
  SET_EVENT_TIMESTAMP_ZERO (event); /* #### */
  SET_EVENT_CHANNEL (event, wrap_console (con));
  XSET_KEY_DATA_KEYSYM (EVENT_DATA (event), (!NILP (k) ? k : make_char (c)));
  XSET_KEY_DATA_MODIFIERS (EVENT_DATA (event), m);
#else /* not USE_KKCC */
  event->event_type	     = key_press_event;
  event->timestamp	     = 0; /* #### */
  event->channel	     = wrap_console (con);
  event->event.key.keysym    = (!NILP (k) ? k : make_char (c));
  event->event.key.modifiers = m;
#endif /* not USE_KKCC */
}

/* This variable controls what character name -> character code mapping
   we are using.  Window-system-specific code sets this to some symbol,
   and we use that symbol as the plist key to convert keysyms into 8-bit
   codes.  In this way one can have several character sets predefined and
   switch them by changing this.

   #### This is utterly bogus and should be removed.
 */
Lisp_Object Vcharacter_set_property;

Ichar
event_to_character (Lisp_Event *event,
		    int allow_extra_modifiers,
		    int allow_meta,
		    int allow_non_ascii)
{
  Ichar c = 0;
  Lisp_Object code;

#ifdef USE_KKCC
  if (EVENT_TYPE (event) != key_press_event)
#else /* not USE_KKCC */
  if (event->event_type != key_press_event)
#endif /* not USE_KKCC */
    {
#ifdef USE_KKCC
      assert (EVENT_TYPE (event) != dead_event);
#else /* not USE_KKCC */
      assert (event->event_type != dead_event);
#endif /* not USE_KKCC */
      return -1;
    }
  if (!allow_extra_modifiers &&
#ifdef USE_KKCC
      XKEY_DATA_MODIFIERS (EVENT_DATA (event)) & (XEMACS_MOD_SUPER|XEMACS_MOD_HYPER|XEMACS_MOD_ALT))
#else /* not USE_KKCC */
      event->event.key.modifiers & (XEMACS_MOD_SUPER|XEMACS_MOD_HYPER|XEMACS_MOD_ALT))
#endif /* not USE_KKCC */
    return -1;
#ifdef USE_KKCC
  if (CHAR_OR_CHAR_INTP (XKEY_DATA_KEYSYM (EVENT_DATA (event))))
    c = XCHAR_OR_CHAR_INT (XKEY_DATA_KEYSYM (EVENT_DATA (event)));
  else if (!SYMBOLP (XKEY_DATA_KEYSYM (EVENT_DATA (event))))
#else /* not USE_KKCC */
  if (CHAR_OR_CHAR_INTP (event->event.key.keysym))
    c = XCHAR_OR_CHAR_INT (event->event.key.keysym);
  else if (!SYMBOLP (event->event.key.keysym))
#endif /* not USE_KKCC */
    abort ();
  else if (allow_non_ascii && !NILP (Vcharacter_set_property)
	   /* Allow window-system-specific extensibility of
	      keysym->code mapping */
#ifdef USE_KKCC
	   && CHAR_OR_CHAR_INTP (code = Fget (XKEY_DATA_KEYSYM (EVENT_DATA (event)),
					      Vcharacter_set_property,
					      Qnil)))
#else /* not USE_KKCC */
	   && CHAR_OR_CHAR_INTP (code = Fget (event->event.key.keysym,
					      Vcharacter_set_property,
					      Qnil)))
#endif /* not USE_KKCC */
    c = XCHAR_OR_CHAR_INT (code);
#ifdef USE_KKCC
  else if (CHAR_OR_CHAR_INTP (code = Fget (XKEY_DATA_KEYSYM (EVENT_DATA (event)),
					   Qascii_character, Qnil)))
#else /* not USE_KKCC */
  else if (CHAR_OR_CHAR_INTP (code = Fget (event->event.key.keysym,
					   Qascii_character, Qnil)))
#endif /* not USE_KKCC */
    c = XCHAR_OR_CHAR_INT (code);
  else
    return -1;

#ifdef USE_KKCC
  if (XKEY_DATA_MODIFIERS (EVENT_DATA (event)) & XEMACS_MOD_CONTROL)
#else /* not USE_KKCC */
  if (event->event.key.modifiers & XEMACS_MOD_CONTROL)
#endif /* not USE_KKCC */
    {
      if (c >= 'a' && c <= 'z')
	c -= ('a' - 'A');
      else
	/* reject Control-Shift- keys */
	if (c >= 'A' && c <= 'Z' && !allow_extra_modifiers)
	  return -1;

      if (c >= '@' && c <= '_')
	c -= '@';
      else if (c == ' ')  /* C-space and C-@ are the same. */
	c = 0;
      else
	/* reject keys that can't take Control- modifiers */
	if (! allow_extra_modifiers) return -1;
    }

#ifdef USE_KKCC
  if (XKEY_DATA_MODIFIERS (EVENT_DATA (event)) & XEMACS_MOD_META)
#else /* not USE_KKCC */
  if (event->event.key.modifiers & XEMACS_MOD_META)
#endif /* not USE_KKCC */
    {
      if (! allow_meta) return -1;
      if (c & 0200) return -1;		/* don't allow M-oslash (overlap) */
#ifdef MULE
      if (c >= 256) return -1;
#endif
      c |= 0200;
    }
  return c;
}

DEFUN ("event-to-character", Fevent_to_character, 1, 4, 0, /*
Return the closest ASCII approximation to the given event object.
If the event isn't a keypress, this returns nil.
If the ALLOW-EXTRA-MODIFIERS argument is non-nil, then this is lenient in
 its translation; it will ignore modifier keys other than control and meta,
 and will ignore the shift modifier on those characters which have no
 shifted ASCII equivalent (Control-Shift-A for example, will be mapped to
 the same ASCII code as Control-A).
If the ALLOW-META argument is non-nil, then the Meta modifier will be
 represented by turning on the high bit of the byte returned; otherwise, nil
 will be returned for events containing the Meta modifier.
If the ALLOW-NON-ASCII argument is non-nil, then characters which are
 present in the prevailing character set (see the `character-set-property'
 variable) will be returned as their code in that character set, instead of
 the return value being restricted to ASCII.
Note that specifying both ALLOW-META and ALLOW-NON-ASCII is ambiguous, as
 both use the high bit; `M-x' and `oslash' will be indistinguishable.
*/
     (event, allow_extra_modifiers, allow_meta, allow_non_ascii))
{
  Ichar c;
  CHECK_LIVE_EVENT (event);
  c = event_to_character (XEVENT (event),
			  !NILP (allow_extra_modifiers),
			  !NILP (allow_meta),
			  !NILP (allow_non_ascii));
  return c < 0 ? Qnil : make_char (c);
}

DEFUN ("character-to-event", Fcharacter_to_event, 1, 4, 0, /*
Convert KEY-DESCRIPTION into an event structure, replete with bucky bits.

KEY-DESCRIPTION is the first argument, and the event to fill in is the
second.  This function contains knowledge about what various kinds of
arguments ``mean'' -- for example, the number 9 is converted to the
character ``Tab'', not the distinct character ``Control-I''.

KEY-DESCRIPTION can be an integer, a character, a symbol such as 'clear,
or a list such as '(control backspace).

If the optional second argument EVENT is an event, it is modified and
returned; otherwise, a new event object is created and returned.

Optional third arg CONSOLE is the console to store in the event, and
defaults to the selected console.

If KEY-DESCRIPTION is an integer or character, the high bit may be
interpreted as the meta key. (This is done for backward compatibility
in lots of places.)  If USE-CONSOLE-META-FLAG is nil, this will always
be the case.  If USE-CONSOLE-META-FLAG is non-nil, the `meta' flag for
CONSOLE affects whether the high bit is interpreted as a meta
key. (See `set-input-mode'.)  If you don't want this silly meta
interpretation done, you should pass in a list containing the
character.

Beware that character-to-event and event-to-character are not strictly
inverse functions, since events contain much more information than the
Lisp character object type can encode.
*/
       (keystroke, event, console, use_console_meta_flag))
{
  struct console *con = decode_console (console);
  if (NILP (event))
    event = Fmake_event (Qnil, Qnil);
  else
    CHECK_LIVE_EVENT (event);
  if (CONSP (keystroke) || SYMBOLP (keystroke))
    key_desc_list_to_event (keystroke, event, 1);
  else
    {
      CHECK_CHAR_COERCE_INT (keystroke);
      character_to_event (XCHAR (keystroke), XEVENT (event), con,
			  !NILP (use_console_meta_flag), 1);
    }
  return event;
}

void
nth_of_key_sequence_as_event (Lisp_Object seq, int n, Lisp_Object event)
{
  assert (STRINGP (seq) || VECTORP (seq));
  assert (n < XINT (Flength (seq)));

  if (STRINGP (seq))
    {
      Ichar ch = string_ichar (seq, n);
      Fcharacter_to_event (make_char (ch), event, Qnil, Qnil);
    }
  else
    {
      Lisp_Object keystroke = XVECTOR_DATA (seq)[n];
      if (EVENTP (keystroke))
	Fcopy_event (keystroke, event);
      else
	Fcharacter_to_event (keystroke, event, Qnil, Qnil);
    }
}

Lisp_Object
key_sequence_to_event_chain (Lisp_Object seq)
{
  int len = XINT (Flength (seq));
  int i;
  Lisp_Object head = Qnil, tail = Qnil;

  for (i = 0; i < len; i++)
    {
      Lisp_Object event = Fmake_event (Qnil, Qnil);
      nth_of_key_sequence_as_event (seq, i, event);
      enqueue_event (event, &head, &tail);
    }

  return head;
}


/* Concatenate a string description of EVENT onto the end of BUF.  If
   BRIEF, use short forms for keys, e.g. C- instead of control-. */

#ifdef USE_KKCC
void
format_event_object (Eistring *buf, Lisp_Object event, int brief)
#else /* not USE_KKCC */
void
format_event_object (Eistring *buf, Lisp_Event *event, int brief)
#endif /* not USE_KKCC */
{
  int mouse_p = 0;
  int mod = 0;
  Lisp_Object key;

#ifdef USE_KKCC
  switch (EVENT_TYPE (XEVENT(event)))
#else /* not USE_KKCC */
  switch (event->event_type)
#endif /* not USE_KKCC */
    {
    case key_press_event:
      {
#ifdef USE_KKCC
	mod = XKEY_DATA_MODIFIERS (XEVENT_DATA(event));
	key = XKEY_DATA_KEYSYM (XEVENT_DATA(event));
#else /* not USE_KKCC */
        mod = event->event.key.modifiers;
        key = event->event.key.keysym;
#endif /* not USE_KKCC */
        /* Hack. */
        if (! brief && CHARP (key) &&
            mod & (XEMACS_MOD_CONTROL | XEMACS_MOD_META | XEMACS_MOD_SUPER |
		   XEMACS_MOD_HYPER))
	{
	  int k = XCHAR (key);
	  if (k >= 'a' && k <= 'z')
	    key = make_char (k - ('a' - 'A'));
	  else if (k >= 'A' && k <= 'Z')
	    mod |= XEMACS_MOD_SHIFT;
	}
        break;
      }
    case button_release_event:
      mouse_p++;
      /* Fall through */
    case button_press_event:
      {
        mouse_p++;
#ifdef USE_KKCC
	mod = XBUTTON_DATA_MODIFIERS (XEVENT_DATA(event));
	key = make_char (XBUTTON_DATA_BUTTON (XEVENT_DATA(event)) + '0');
#else /* not USE_KKCC */
        mod = event->event.button.modifiers;
        key = make_char (event->event.button.button + '0');
#endif /* not USE_KKCC */
        break;
      }
    case magic_event:
      {
	Lisp_Object stream;
	struct gcpro gcpro1;
	GCPRO1 (stream);

	stream = make_resizing_buffer_output_stream ();
#ifdef USE_KKCC
	event_stream_format_magic_event (XEVENT(event), stream);
#else /* not USE_KKCC */
	event_stream_format_magic_event (event, stream);
#endif /* not USE_KKCC */
	Lstream_flush (XLSTREAM (stream));
	eicat_raw (buf, resizing_buffer_stream_ptr (XLSTREAM (stream)),
		   Lstream_byte_count (XLSTREAM (stream)));
	Lstream_delete (XLSTREAM (stream));
	UNGCPRO;
	return;
      }
    case magic_eval_event:	eicat_c (buf, "magic-eval"); return;
    case pointer_motion_event:	eicat_c (buf, "motion");     return;
    case misc_user_event:	eicat_c (buf, "misc-user");  return;
    case eval_event:		eicat_c (buf, "eval");	     return;
    case process_event:		eicat_c (buf, "process");    return;
    case timeout_event:		eicat_c (buf, "timeout");    return;
    case empty_event:		eicat_c (buf, "empty");	     return;
    case dead_event:		eicat_c (buf, "DEAD-EVENT"); return;
    default:
      abort ();
      return;
    }
#define modprint(x,y) \
  do { if (brief) eicat_c (buf, (y)); else eicat_c (buf, (x)); } while (0)
  if (mod & XEMACS_MOD_CONTROL) modprint ("control-", "C-");
  if (mod & XEMACS_MOD_META)    modprint ("meta-",    "M-");
  if (mod & XEMACS_MOD_SUPER)   modprint ("super-",   "S-");
  if (mod & XEMACS_MOD_HYPER)   modprint ("hyper-",   "H-");
  if (mod & XEMACS_MOD_ALT)	modprint ("alt-",     "A-");
  if (mod & XEMACS_MOD_SHIFT)   modprint ("shift-",   "Sh-");
  if (mouse_p)
    {
      eicat_c (buf, "button");
      --mouse_p;
    }

#undef modprint

  if (CHARP (key))
    eicat_ch (buf, XCHAR (key));
  else if (SYMBOLP (key))
    {
      const Char_ASCII *str = 0;
      if (brief)
	{
	  if      (EQ (key, QKlinefeed))  str = "LFD";
	  else if (EQ (key, QKtab))       str = "TAB";
	  else if (EQ (key, QKreturn))    str = "RET";
	  else if (EQ (key, QKescape))    str = "ESC";
	  else if (EQ (key, QKdelete))    str = "DEL";
	  else if (EQ (key, QKspace))     str = "SPC";
	  else if (EQ (key, QKbackspace)) str = "BS";
	}
      if (str)
	eicat_c (buf, str);
      else
	eicat_lstr (buf, XSYMBOL (key)->name);
    }
  else
    abort ();
  if (mouse_p)
    eicat_c (buf, "up");
}


DEFUN ("eventp", Feventp, 1, 1, 0, /*
True if OBJECT is an event object.
*/
       (object))
{
  return EVENTP (object) ? Qt : Qnil;
}

DEFUN ("event-live-p", Fevent_live_p, 1, 1, 0, /*
True if OBJECT is an event object that has not been deallocated.
*/
       (object))
{
#ifdef USE_KKCC
  return EVENTP (object) && XEVENT_TYPE (object) != dead_event ?
    Qt : Qnil;
#else /* not USE_KKCC */
  return EVENTP (object) && XEVENT (object)->event_type != dead_event ?
    Qt : Qnil;
#endif /* not USE_KKCC */
}

#if 0 /* debugging functions */

DEFUN ("event-next", Fevent_next, 1, 1, 0, /*
Return the event object's `next' event, or nil if it has none.
The `next-event' field is changed by calling `set-next-event'.
*/
	 (event))
{
  Lisp_Event *e;
  CHECK_LIVE_EVENT (event);

  return XEVENT_NEXT (event);
}

DEFUN ("set-event-next", Fset_event_next, 2, 2, 0, /*
Set the `next event' of EVENT to NEXT-EVENT.
NEXT-EVENT must be an event object or nil.
*/
	 (event, next_event))
{
  Lisp_Object ev;

  CHECK_LIVE_EVENT (event);
  if (NILP (next_event))
    {
      XSET_EVENT_NEXT (event, Qnil);
      return Qnil;
    }

  CHECK_LIVE_EVENT (next_event);

  EVENT_CHAIN_LOOP (ev, XEVENT_NEXT (event))
    {
      QUIT;
      if (EQ (ev, event))
	invalid_operation_2 ("Cyclic event-next", event, next_event);
    }
  XSET_EVENT_NEXT (event, next_event);
  return next_event;
}

#endif /* 0 */

DEFUN ("event-type", Fevent_type, 1, 1, 0, /*
Return the type of EVENT.
This will be a symbol; one of

key-press	A key was pressed.
button-press	A mouse button was pressed.
button-release	A mouse button was released.
misc-user	Some other user action happened; typically, this is
		a menu selection or scrollbar action.
motion		The mouse moved.
process		Input is available from a subprocess.
timeout		A timeout has expired.
eval		This causes a specified action to occur when dispatched.
magic		Some window-system-specific event has occurred.
empty		The event has been allocated but not assigned.

*/
       (event))
{
  CHECK_LIVE_EVENT (event);
#ifdef USE_KKCC
  switch (XEVENT_TYPE (event))
#else /* not USE_KKCC */
  switch (XEVENT (event)->event_type)
#endif /* not USE_KKCC */
    {
    case key_press_event:	return Qkey_press;
    case button_press_event:	return Qbutton_press;
    case button_release_event:	return Qbutton_release;
    case misc_user_event:	return Qmisc_user;
    case pointer_motion_event:	return Qmotion;
    case process_event:		return Qprocess;
    case timeout_event:		return Qtimeout;
    case eval_event:		return Qeval;
    case magic_event:
    case magic_eval_event:
      return Qmagic;

    case empty_event:
      return Qempty;

    default:
      abort ();
      return Qnil;
    }
}

DEFUN ("event-timestamp", Fevent_timestamp, 1, 1, 0, /*
Return the timestamp of the event object EVENT.
Timestamps are measured in milliseconds since the start of the window system.
They are NOT related to any current time measurement.
They should be compared with `event-timestamp<'.
See also `current-event-timestamp'.
*/
       (event))
{
  CHECK_LIVE_EVENT (event);
  /* This junk is so that timestamps don't get to be negative, but contain
     as many bits as this particular emacs will allow.
   */
#ifdef USE_KKCC
  return make_int (((1L << (VALBITS - 1)) - 1) &
		      XEVENT_TIMESTAMP (event));
#else /* not USE_KKCC */
  return make_int (((1L << (VALBITS - 1)) - 1) &
		      XEVENT (event)->timestamp);
#endif /* not USE_KKCC */
}

#define TIMESTAMP_HALFSPACE (1L << (VALBITS - 2))

DEFUN ("event-timestamp<", Fevent_timestamp_lessp, 2, 2, 0, /*
Return true if timestamp TIME1 is earlier than timestamp TIME2.
This correctly handles timestamp wrap.
See also `event-timestamp' and `current-event-timestamp'.
*/
       (time1, time2))
{
  EMACS_INT t1, t2;

  CHECK_NATNUM (time1);
  CHECK_NATNUM (time2);
  t1 = XINT (time1);
  t2 = XINT (time2);

  if (t1 < t2)
    return t2 - t1 < TIMESTAMP_HALFSPACE ? Qt : Qnil;
  else
    return t1 - t2 < TIMESTAMP_HALFSPACE ? Qnil : Qt;
}

#ifdef USE_KKCC
#define CHECK_EVENT_TYPE(e,t1,sym) do {		\
  CHECK_LIVE_EVENT (e);				\
  if (XEVENT_TYPE (e) != (t1))	        	\
    e = wrong_type_argument (sym,e);		\
} while (0)
#else /* not USE_KKCC */
#define CHECK_EVENT_TYPE(e,t1,sym) do {		\
  CHECK_LIVE_EVENT (e);				\
  if (XEVENT(e)->event_type != (t1))		\
    e = wrong_type_argument (sym,e);		\
} while (0)
#endif /* not USE_KKCC */

#ifdef USE_KKCC
#define CHECK_EVENT_TYPE2(e,t1,t2,sym) do {		\
  CHECK_LIVE_EVENT (e);					\
  {							\
    emacs_event_type CET_type = XEVENT_TYPE (e);	\
    if (CET_type != (t1) &&				\
	CET_type != (t2))				\
      e = wrong_type_argument (sym,e);			\
  }							\
} while (0)
#else /* not USE_KKCC */
#define CHECK_EVENT_TYPE2(e,t1,t2,sym) do {		\
  CHECK_LIVE_EVENT (e);					\
  {							\
    emacs_event_type CET_type = XEVENT (e)->event_type;	\
    if (CET_type != (t1) &&				\
	CET_type != (t2))				\
      e = wrong_type_argument (sym,e);			\
  }							\
} while (0)
#endif /* not USE_KKCC */

#ifdef USE_KKCC
#define CHECK_EVENT_TYPE3(e,t1,t2,t3,sym) do {		\
  CHECK_LIVE_EVENT (e);					\
  {							\
    emacs_event_type CET_type = XEVENT_TYPE (e);	\
    if (CET_type != (t1) &&				\
	CET_type != (t2) &&				\
	CET_type != (t3))				\
      e = wrong_type_argument (sym,e);			\
  }							\
} while (0)
#else /* not USE_KKCC */
#define CHECK_EVENT_TYPE3(e,t1,t2,t3,sym) do {		\
  CHECK_LIVE_EVENT (e);					\
  {							\
    emacs_event_type CET_type = XEVENT (e)->event_type;	\
    if (CET_type != (t1) &&				\
	CET_type != (t2) &&				\
	CET_type != (t3))				\
      e = wrong_type_argument (sym,e);			\
  }							\
} while (0)
#endif /* not USE_KKCC */

DEFUN ("event-key", Fevent_key, 1, 1, 0, /*
Return the Keysym of the key-press event EVENT.
This will be a character if the event is associated with one, else a symbol.
*/
       (event))
{
  CHECK_EVENT_TYPE (event, key_press_event, Qkey_press_event_p);
#ifdef USE_KKCC
  return XKEY_DATA_KEYSYM (XEVENT_DATA (event));
#else /* not USE_KKCC */
  return XEVENT (event)->event.key.keysym;
#endif /* not USE_KKCC */
}

DEFUN ("event-button", Fevent_button, 1, 1, 0, /*
Return the button-number of the button-press or button-release event EVENT.
*/
       (event))
{

  CHECK_EVENT_TYPE3 (event, button_press_event, button_release_event,
		     misc_user_event, Qbutton_event_p);
#ifdef HAVE_WINDOW_SYSTEM
#ifdef USE_KKCC
  if ( XEVENT_TYPE (event) == misc_user_event)
    return make_int (XMISC_USER_DATA_BUTTON (XEVENT_DATA (event)));
  else
    return make_int (XBUTTON_DATA_BUTTON (XEVENT_DATA (event)));
#else /* not USE_KKCC */
  if ( XEVENT (event)->event_type == misc_user_event)
    return make_int (XEVENT (event)->event.misc.button);
  else
    return make_int (XEVENT (event)->event.button.button);
#endif /* not USE_KKCC */
#else /* !HAVE_WINDOW_SYSTEM */
  return Qzero;
#endif /* !HAVE_WINDOW_SYSTEM */

}

DEFUN ("event-modifier-bits", Fevent_modifier_bits, 1, 1, 0, /*
Return a number representing the modifier keys and buttons which were down
when the given mouse or keyboard event was produced.
See also the function `event-modifiers'.
*/
       (event))
{
 again:
  CHECK_LIVE_EVENT (event);
#ifdef USE_KKCC
  switch (XEVENT_TYPE (event))
    {
    case key_press_event:
      return make_int (XKEY_DATA_MODIFIERS (XEVENT_DATA (event)));
    case button_press_event:
    case button_release_event:
      return make_int (XBUTTON_DATA_MODIFIERS (XEVENT_DATA (event)));
    case pointer_motion_event:
      return make_int (XMOTION_DATA_MODIFIERS (XEVENT_DATA (event)));
    case misc_user_event:
      return make_int (XMISC_USER_DATA_MODIFIERS (XEVENT_DATA (event)));
    default:
      event = wrong_type_argument (intern ("key-or-mouse-event-p"), event);
      goto again;
    }
#else /* not USE_KKCC */
  switch (XEVENT (event)->event_type)
    {
    case key_press_event:
      return make_int (XEVENT (event)->event.key.modifiers);
    case button_press_event:
    case button_release_event:
      return make_int (XEVENT (event)->event.button.modifiers);
    case pointer_motion_event:
      return make_int (XEVENT (event)->event.motion.modifiers);
    case misc_user_event:
      return make_int (XEVENT (event)->event.misc.modifiers);
    default:
      event = wrong_type_argument (intern ("key-or-mouse-event-p"), event);
      goto again;
    }
#endif /* not USE_KKCC */
}

DEFUN ("event-modifiers", Fevent_modifiers, 1, 1, 0, /*
Return a list of symbols, the names of the modifier keys and buttons
which were down when the given mouse or keyboard event was produced.
See also the function `event-modifier-bits'.

The possible symbols in the list are

`shift':     The Shift key.  Will not appear, in general, on key events
             where the keysym is an ASCII character, because using Shift
             on such a character converts it into another character rather
             than actually just adding a Shift modifier.

`control':   The Control key.

`meta':      The Meta key.  On PC's and PC-style keyboards, this is generally
             labelled \"Alt\"; Meta is a holdover from early Lisp Machines and
             such, propagated through the X Window System.  On Sun keyboards,
             this key is labelled with a diamond.

`alt':       The \"Alt\" key.  Alt is in quotes because this does not refer
             to what it obviously should refer to, namely the Alt key on PC
             keyboards.  Instead, it refers to the key labelled Alt on Sun
             keyboards, and to no key at all on PC keyboards.

`super':     The Super key.  Most keyboards don't have any such key, but
             under X Windows using `xmodmap' you can assign any key (such as
             an underused right-shift, right-control, or right-alt key) to
             this key modifier.  No support currently exists under MS Windows
             for generating these modifiers.

`hyper':     The Hyper key.  Works just like the Super key.

`button1':   The mouse buttons.  This means that the specified button was held
`button2':   down at the time the event occurred.  NOTE: For button-press
`button3':   events, the button that was just pressed down does NOT appear in
`button4':   the modifiers.
`button5':

Button modifiers are currently ignored when defining and looking up key and
mouse strokes in keymaps.  This could be changed, which would allow a user to
create button-chord actions, use a button as a key modifier and do other
clever things.
*/
       (event))
{
  int mod = XINT (Fevent_modifier_bits (event));
  Lisp_Object result = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (result);
  if (mod & XEMACS_MOD_SHIFT)   result = Fcons (Qshift, result);
  if (mod & XEMACS_MOD_ALT)	result = Fcons (Qalt, result);
  if (mod & XEMACS_MOD_HYPER)   result = Fcons (Qhyper, result);
  if (mod & XEMACS_MOD_SUPER)   result = Fcons (Qsuper, result);
  if (mod & XEMACS_MOD_META)    result = Fcons (Qmeta, result);
  if (mod & XEMACS_MOD_CONTROL) result = Fcons (Qcontrol, result);
  if (mod & XEMACS_MOD_BUTTON1) result = Fcons (Qbutton1, result);
  if (mod & XEMACS_MOD_BUTTON2) result = Fcons (Qbutton2, result);
  if (mod & XEMACS_MOD_BUTTON3) result = Fcons (Qbutton3, result);
  if (mod & XEMACS_MOD_BUTTON4) result = Fcons (Qbutton4, result);
  if (mod & XEMACS_MOD_BUTTON5) result = Fcons (Qbutton5, result);
  RETURN_UNGCPRO (Fnreverse (result));
}

static int
event_x_y_pixel_internal (Lisp_Object event, int *x, int *y, int relative)
{
  struct window *w;
  struct frame *f;

#ifdef USE_KKCC
  if (XEVENT_TYPE (event) == pointer_motion_event)
    {
      *x = XMOTION_DATA_X (XEVENT_DATA (event));
      *y = XMOTION_DATA_Y (XEVENT_DATA (event));
    }
  else if (XEVENT_TYPE (event) == button_press_event ||
	   XEVENT_TYPE (event) == button_release_event)
    {
      *x = XBUTTON_DATA_X (XEVENT_DATA (event));
      *y = XBUTTON_DATA_Y (XEVENT_DATA (event));
    }
  else if (XEVENT_TYPE (event) == misc_user_event)
    {
      *x = XMISC_USER_DATA_X (XEVENT_DATA (event));
      *y = XMISC_USER_DATA_Y (XEVENT_DATA (event));
    }
  else
    return 0;
#else /* not USE_KKCC */
  if (XEVENT (event)->event_type == pointer_motion_event)
    {
      *x = XEVENT (event)->event.motion.x;
      *y = XEVENT (event)->event.motion.y;
    }
  else if (XEVENT (event)->event_type == button_press_event ||
	   XEVENT (event)->event_type == button_release_event)
    {
      *x = XEVENT (event)->event.button.x;
      *y = XEVENT (event)->event.button.y;
    }
  else if (XEVENT (event)->event_type == misc_user_event)
    {
      *x = XEVENT (event)->event.misc.x;
      *y = XEVENT (event)->event.misc.y;
    }
  else
    return 0;
#endif /* not USE_KKCC */
  f = XFRAME (EVENT_CHANNEL (XEVENT (event)));

  if (relative)
    {
      w = find_window_by_pixel_pos (*x, *y, f->root_window);

      if (!w)
	return 1;	/* #### What should really happen here? */

      *x -= w->pixel_left;
      *y -= w->pixel_top;
    }
  else
    {
      *y -= FRAME_REAL_TOP_TOOLBAR_HEIGHT (f) -
	FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH (f);
      *x -= FRAME_REAL_LEFT_TOOLBAR_WIDTH (f) -
	FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH (f);
    }

  return 1;
}

DEFUN ("event-window-x-pixel", Fevent_window_x_pixel, 1, 1, 0, /*
Return the X position in pixels of mouse event EVENT.
The value returned is relative to the window the event occurred in.
This will signal an error if the event is not a mouse event.
See also `mouse-event-p' and `event-x-pixel'.
*/
       (event))
{
  int x, y;

  CHECK_LIVE_EVENT (event);

  if (!event_x_y_pixel_internal (event, &x, &y, 1))
    return wrong_type_argument (Qmouse_event_p, event);
  else
    return make_int (x);
}

DEFUN ("event-window-y-pixel", Fevent_window_y_pixel, 1, 1, 0, /*
Return the Y position in pixels of mouse event EVENT.
The value returned is relative to the window the event occurred in.
This will signal an error if the event is not a mouse event.
See also `mouse-event-p' and `event-y-pixel'.
*/
       (event))
{
  int x, y;

  CHECK_LIVE_EVENT (event);

  if (!event_x_y_pixel_internal (event, &x, &y, 1))
    return wrong_type_argument (Qmouse_event_p, event);
  else
    return make_int (y);
}

DEFUN ("event-x-pixel", Fevent_x_pixel, 1, 1, 0, /*
Return the X position in pixels of mouse event EVENT.
The value returned is relative to the frame the event occurred in.
This will signal an error if the event is not a mouse event.
See also `mouse-event-p' and `event-window-x-pixel'.
*/
       (event))
{
  int x, y;

  CHECK_LIVE_EVENT (event);

  if (!event_x_y_pixel_internal (event, &x, &y, 0))
    return wrong_type_argument (Qmouse_event_p, event);
  else
    return make_int (x);
}

DEFUN ("event-y-pixel", Fevent_y_pixel, 1, 1, 0, /*
Return the Y position in pixels of mouse event EVENT.
The value returned is relative to the frame the event occurred in.
This will signal an error if the event is not a mouse event.
See also `mouse-event-p' `event-window-y-pixel'.
*/
       (event))
{
  int x, y;

  CHECK_LIVE_EVENT (event);

  if (!event_x_y_pixel_internal (event, &x, &y, 0))
    return wrong_type_argument (Qmouse_event_p, event);
  else
    return make_int (y);
}

/* Given an event, return a value:

     OVER_TOOLBAR:	over one of the 4 frame toolbars
     OVER_MODELINE:	over a modeline
     OVER_BORDER:	over an internal border
     OVER_NOTHING:	over the text area, but not over text
     OVER_OUTSIDE:	outside of the frame border
     OVER_TEXT:		over text in the text area
     OVER_V_DIVIDER:	over windows vertical divider

   and return:

   The X char position in CHAR_X, if not a null pointer.
   The Y char position in CHAR_Y, if not a null pointer.
   (These last two values are relative to the window the event is over.)
   The window it's over in W, if not a null pointer.
   The buffer position it's over in BUFP, if not a null pointer.
   The closest buffer position in CLOSEST, if not a null pointer.

   OBJ_X, OBJ_Y, OBJ1, and OBJ2 are as in pixel_to_glyph_translation().
*/

static int
event_pixel_translation (Lisp_Object event, int *char_x, int *char_y,
			 int *obj_x, int *obj_y,
			 struct window **w, Charbpos *bufp, Charbpos *closest,
			 Charcount *modeline_closest,
			 Lisp_Object *obj1, Lisp_Object *obj2)
{
  int pix_x = 0;
  int pix_y = 0;
  int result;
  Lisp_Object frame;

  int ret_x, ret_y, ret_obj_x, ret_obj_y;
  struct window *ret_w;
  Charbpos ret_bufp, ret_closest;
  Charcount ret_modeline_closest;
  Lisp_Object ret_obj1, ret_obj2;

  CHECK_LIVE_EVENT (event);
#ifdef USE_KKCC
  frame = XEVENT_CHANNEL (event);
  switch (XEVENT_TYPE (event))
    {
    case pointer_motion_event :
      pix_x = XMOTION_DATA_X (XEVENT_DATA (event));
      pix_y = XMOTION_DATA_Y (XEVENT_DATA (event));
      break;
    case button_press_event :
    case button_release_event :
      pix_x = XBUTTON_DATA_X (XEVENT_DATA (event));
      pix_y = XBUTTON_DATA_Y (XEVENT_DATA (event));
      break;
    case misc_user_event :
      pix_x = XMISC_USER_DATA_X (XEVENT_DATA (event));
      pix_y = XMISC_USER_DATA_Y (XEVENT_DATA (event));
      break;
    default:
      dead_wrong_type_argument (Qmouse_event_p, event);
    }
#else /* not USE_KKCC */
  frame = XEVENT (event)->channel;
  switch (XEVENT (event)->event_type)
    {
    case pointer_motion_event :
      pix_x = XEVENT (event)->event.motion.x;
      pix_y = XEVENT (event)->event.motion.y;
      break;
    case button_press_event :
    case button_release_event :
      pix_x = XEVENT (event)->event.button.x;
      pix_y = XEVENT (event)->event.button.y;
      break;
    case misc_user_event :
      pix_x = XEVENT (event)->event.misc.x;
      pix_y = XEVENT (event)->event.misc.y;
      break;
    default:
      dead_wrong_type_argument (Qmouse_event_p, event);
    }
#endif /* not USE_KKCC */

  result = pixel_to_glyph_translation (XFRAME (frame), pix_x, pix_y,
				       &ret_x, &ret_y, &ret_obj_x, &ret_obj_y,
				       &ret_w, &ret_bufp, &ret_closest,
				       &ret_modeline_closest,
				       &ret_obj1, &ret_obj2);

  if (result == OVER_NOTHING || result == OVER_OUTSIDE)
    ret_bufp = 0;
  else if (ret_w && NILP (ret_w->buffer))
    /* Why does this happen?  (Does it still happen?)
       I guess the window has gotten reused as a non-leaf... */
    ret_w = 0;

  /* #### pixel_to_glyph_translation() sometimes returns garbage...
     The word has type Lisp_Type_Record (presumably meaning `extent') but the
     pointer points to random memory, often filled with 0, sometimes not.
   */
  /* #### Chuck, do we still need this crap? */
  if (!NILP (ret_obj1) && !(GLYPHP (ret_obj1)
#ifdef HAVE_TOOLBARS
			    || TOOLBAR_BUTTONP (ret_obj1)
#endif
     ))
    abort ();
  if (!NILP (ret_obj2) && !(EXTENTP (ret_obj2) || CONSP (ret_obj2)))
    abort ();

  if (char_x)
    *char_x = ret_x;
  if (char_y)
    *char_y = ret_y;
  if (obj_x)
    *obj_x = ret_obj_x;
  if (obj_y)
    *obj_y = ret_obj_y;
  if (w)
    *w = ret_w;
  if (bufp)
    *bufp = ret_bufp;
  if (closest)
    *closest = ret_closest;
  if (modeline_closest)
    *modeline_closest = ret_modeline_closest;
  if (obj1)
    *obj1 = ret_obj1;
  if (obj2)
    *obj2 = ret_obj2;

  return result;
}

DEFUN ("event-over-text-area-p", Fevent_over_text_area_p, 1, 1, 0, /*
Return t if the mouse event EVENT occurred over the text area of a window.
The modeline is not considered to be part of the text area.
*/
       (event))
{
  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return result == OVER_TEXT || result == OVER_NOTHING ? Qt : Qnil;
}

DEFUN ("event-over-modeline-p", Fevent_over_modeline_p, 1, 1, 0, /*
Return t if the mouse event EVENT occurred over the modeline of a window.
*/
       (event))
{
  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return result == OVER_MODELINE ? Qt : Qnil;
}

DEFUN ("event-over-border-p", Fevent_over_border_p, 1, 1, 0, /*
Return t if the mouse event EVENT occurred over an internal border.
*/
       (event))
{
  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return result == OVER_BORDER ? Qt : Qnil;
}

DEFUN ("event-over-toolbar-p", Fevent_over_toolbar_p, 1, 1, 0, /*
Return t if the mouse event EVENT occurred over a toolbar.
*/
       (event))
{
  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return result == OVER_TOOLBAR ? Qt : Qnil;
}

DEFUN ("event-over-vertical-divider-p", Fevent_over_vertical_divider_p, 1, 1, 0, /*
Return t if the mouse event EVENT occurred over a window divider.
*/
       (event))
{
  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return result == OVER_V_DIVIDER ? Qt : Qnil;
}

struct console *
event_console_or_selected (Lisp_Object event)
{
  Lisp_Object channel = EVENT_CHANNEL (XEVENT (event));
  Lisp_Object console = CDFW_CONSOLE (channel);

  if (NILP (console))
    console = Vselected_console;

  return XCONSOLE (console);
}

DEFUN ("event-channel", Fevent_channel, 1, 1, 0, /*
Return the channel that the event EVENT occurred on.
This will be a frame, device, console, or nil for some types
of events (e.g. eval events).
*/
       (event))
{
  CHECK_LIVE_EVENT (event);
  return EVENT_CHANNEL (XEVENT (event));
}

DEFUN ("event-window", Fevent_window, 1, 1, 0, /*
Return the window over which mouse event EVENT occurred.
This may be nil if the event occurred in the border or over a toolbar.
The modeline is considered to be within the window it describes.
*/
       (event))
{
  struct window *w;

  event_pixel_translation (event, 0, 0, 0, 0, &w, 0, 0, 0, 0, 0);

  if (!w)
    return Qnil;
  else
    {
      return wrap_window (w);
    }
}

DEFUN ("event-point", Fevent_point, 1, 1, 0, /*
Return the character position of the mouse event EVENT.
If the event did not occur over a window, or did not occur over text,
then this returns nil.  Otherwise, it returns a position in the buffer
visible in the event's window.
*/
       (event))
{
  Charbpos bufp;
  struct window *w;

  event_pixel_translation (event, 0, 0, 0, 0, &w, &bufp, 0, 0, 0, 0);

  return w && bufp ? make_int (bufp) : Qnil;
}

DEFUN ("event-closest-point", Fevent_closest_point, 1, 1, 0, /*
Return the character position closest to the mouse event EVENT.
If the event did not occur over a window or over text, return the
closest point to the location of the event.  If the Y pixel position
overlaps a window and the X pixel position is to the left of that
window, the closest point is the beginning of the line containing the
Y position.  If the Y pixel position overlaps a window and the X pixel
position is to the right of that window, the closest point is the end
of the line containing the Y position.  If the Y pixel position is
above a window, return 0.  If it is below the last character in a window,
return the value of (window-end).
*/
       (event))
{
  Charbpos bufp;

  event_pixel_translation (event, 0, 0, 0, 0, 0, 0, &bufp, 0, 0, 0);

  return bufp ? make_int (bufp) : Qnil;
}

DEFUN ("event-x", Fevent_x, 1, 1, 0, /*
Return the X position of the mouse event EVENT in characters.
This is relative to the window the event occurred over.
*/
       (event))
{
  int char_x;

  event_pixel_translation (event, &char_x, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return make_int (char_x);
}

DEFUN ("event-y", Fevent_y, 1, 1, 0, /*
Return the Y position of the mouse event EVENT in characters.
This is relative to the window the event occurred over.
*/
       (event))
{
  int char_y;

  event_pixel_translation (event, 0, &char_y, 0, 0, 0, 0, 0, 0, 0, 0);

  return make_int (char_y);
}

DEFUN ("event-modeline-position", Fevent_modeline_position, 1, 1, 0, /*
Return the character position in the modeline that EVENT occurred over.
EVENT should be a mouse event.  If EVENT did not occur over a modeline,
nil is returned.  You can determine the actual character that the
event occurred over by looking in `generated-modeline-string' at the
returned character position.  Note that `generated-modeline-string'
is buffer-local, and you must use EVENT's buffer when retrieving
`generated-modeline-string' in order to get accurate results.
*/
       (event))
{
  Charcount mbufp;
  int where;

  where = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, &mbufp, 0, 0);

  return (mbufp < 0 || where != OVER_MODELINE) ? Qnil : make_int (mbufp);
}

DEFUN ("event-glyph", Fevent_glyph, 1, 1, 0, /*
Return the glyph that the mouse event EVENT occurred over, or nil.
*/
       (event))
{
  Lisp_Object glyph;
  struct window *w;

  event_pixel_translation (event, 0, 0, 0, 0, &w, 0, 0, 0, &glyph, 0);

  return w && GLYPHP (glyph) ? glyph : Qnil;
}

DEFUN ("event-glyph-extent", Fevent_glyph_extent, 1, 1, 0, /*
Return the extent of the glyph that the mouse event EVENT occurred over.
If the event did not occur over a glyph, nil is returned.
*/
       (event))
{
  Lisp_Object extent;
  struct window *w;

  event_pixel_translation (event, 0, 0, 0, 0, &w, 0, 0, 0, 0, &extent);

  return w && EXTENTP (extent) ? extent : Qnil;
}

DEFUN ("event-glyph-x-pixel", Fevent_glyph_x_pixel, 1, 1, 0, /*
Return the X pixel position of EVENT relative to the glyph it occurred over.
EVENT should be a mouse event.  If the event did not occur over a glyph,
nil is returned.
*/
       (event))
{
  Lisp_Object extent;
  struct window *w;
  int obj_x;

  event_pixel_translation (event, 0, 0, &obj_x, 0, &w, 0, 0, 0, 0, &extent);

  return w && EXTENTP (extent) ? make_int (obj_x) : Qnil;
}

DEFUN ("event-glyph-y-pixel", Fevent_glyph_y_pixel, 1, 1, 0, /*
Return the Y pixel position of EVENT relative to the glyph it occurred over.
EVENT should be a mouse event.  If the event did not occur over a glyph,
nil is returned.
*/
       (event))
{
  Lisp_Object extent;
  struct window *w;
  int obj_y;

  event_pixel_translation (event, 0, 0, 0, &obj_y, &w, 0, 0, 0, 0, &extent);

  return w && EXTENTP (extent) ? make_int (obj_y) : Qnil;
}

DEFUN ("event-toolbar-button", Fevent_toolbar_button, 1, 1, 0, /*
Return the toolbar button that the mouse event EVENT occurred over.
If the event did not occur over a toolbar button, nil is returned.
*/
       (event))
{
#ifdef HAVE_TOOLBARS
  Lisp_Object button;

  int result = event_pixel_translation (event, 0, 0, 0, 0, 0, 0, 0, 0, &button, 0);

  return result == OVER_TOOLBAR && TOOLBAR_BUTTONP (button) ? button : Qnil;
#else
  return Qnil;
#endif
}

DEFUN ("event-process", Fevent_process, 1, 1, 0, /*
Return the process of the process-output event EVENT.
*/
       (event))
{
#ifdef USE_KKCC
  CHECK_EVENT_TYPE (event, process_event, Qprocess_event_p);
  return XPROCESS_DATA_PROCESS (XEVENT_DATA (event));
#else /* not USE_KKCC */
  CHECK_EVENT_TYPE (event, process_event, Qprocess_event_p);
  return XEVENT (event)->event.process.process;
#endif /* not USE_KKCC */
}

DEFUN ("event-function", Fevent_function, 1, 1, 0, /*
Return the callback function of EVENT.
EVENT should be a timeout, misc-user, or eval event.
*/
       (event))
{
 again:
  CHECK_LIVE_EVENT (event);
#ifdef USE_KKCC
  switch (XEVENT_TYPE (event))
    {
    case timeout_event:
      return XTIMEOUT_DATA_FUNCTION (XEVENT_DATA (event));
    case misc_user_event:
      return XMISC_USER_DATA_FUNCTION (XEVENT_DATA (event));
    case eval_event:
      return XEVAL_DATA_FUNCTION (XEVENT_DATA (event));
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
#else /* not USE_KKCC */
  switch (XEVENT (event)->event_type)
    {
    case timeout_event:
      return XEVENT (event)->event.timeout.function;
    case misc_user_event:
      return XEVENT (event)->event.misc.function;
    case eval_event:
      return XEVENT (event)->event.eval.function;
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
#endif /* not USE_KKCC */
}

DEFUN ("event-object", Fevent_object, 1, 1, 0, /*
Return the callback function argument of EVENT.
EVENT should be a timeout, misc-user, or eval event.
*/
       (event))
{
 again:
  CHECK_LIVE_EVENT (event);
#ifdef USE_KKCC
  switch (XEVENT_TYPE (event))
    {
    case timeout_event:
      return XTIMEOUT_DATA_OBJECT (XEVENT_DATA (event));
    case misc_user_event:
      return XMISC_USER_DATA_OBJECT (XEVENT_DATA (event));
    case eval_event:
      return XEVAL_DATA_OBJECT (XEVENT_DATA (event));
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
#else /* not USE_KKCC */
  switch (XEVENT (event)->event_type)
    {
    case timeout_event:
      return XEVENT (event)->event.timeout.object;
    case misc_user_event:
      return XEVENT (event)->event.misc.object;
    case eval_event:
      return XEVENT (event)->event.eval.object;
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
#endif /* not USE_KKCC */
}

DEFUN ("event-properties", Fevent_properties, 1, 1, 0, /*
Return a list of all of the properties of EVENT.
This is in the form of a property list (alternating keyword/value pairs).
*/
       (event))
{
  Lisp_Object props = Qnil;
  Lisp_Event *e;
  struct gcpro gcpro1;

  CHECK_LIVE_EVENT (event);
  e = XEVENT (event);
  GCPRO1 (props);

  props = cons3 (Qtimestamp, Fevent_timestamp (event), props);

#ifdef USE_KKCC
  switch (EVENT_TYPE (e))
#else /* not USE_KKCC */
  switch (e->event_type)
#endif /* not USE_KKCC */
    {
    default: abort ();

    case process_event:
#ifdef USE_KKCC
      props = cons3 (Qprocess, XPROCESS_DATA_PROCESS (EVENT_DATA (e)), props);
#else /* not USE_KKCC */
      props = cons3 (Qprocess, e->event.process.process, props);
#endif /* not USE_KKCC */
      break;

    case timeout_event:
      props = cons3 (Qobject,	Fevent_object	(event), props);
      props = cons3 (Qfunction, Fevent_function (event), props);
#ifdef USE_KKCC
      props = cons3 (Qid, make_int (XTIMEOUT_DATA_ID_NUMBER (EVENT_DATA (e))), props);
#else /* not USE_KKCC */
      props = cons3 (Qid, make_int (e->event.timeout.id_number), props);
#endif /* not USE_KKCC */
      break;

    case key_press_event:
      props = cons3 (Qmodifiers, Fevent_modifiers (event), props);
      props = cons3 (Qkey,	 Fevent_key	  (event), props);
      break;

    case button_press_event:
    case button_release_event:
      props = cons3 (Qy,	 Fevent_y_pixel	  (event), props);
      props = cons3 (Qx,	 Fevent_x_pixel	  (event), props);
      props = cons3 (Qmodifiers, Fevent_modifiers (event), props);
      props = cons3 (Qbutton,	 Fevent_button	  (event), props);
      break;

    case pointer_motion_event:
      props = cons3 (Qmodifiers, Fevent_modifiers (event), props);
      props = cons3 (Qy,         Fevent_y_pixel   (event), props);
      props = cons3 (Qx,         Fevent_x_pixel   (event), props);
      break;

    case misc_user_event:
      props = cons3 (Qobject,	 Fevent_object	(event), props);
      props = cons3 (Qfunction,  Fevent_function (event), props);
      props = cons3 (Qy,	 Fevent_y_pixel	  (event), props);
      props = cons3 (Qx,	 Fevent_x_pixel	  (event), props);
      props = cons3 (Qmodifiers, Fevent_modifiers (event), props);
      props = cons3 (Qbutton,	 Fevent_button	  (event), props);
      break;

    case eval_event:
      props = cons3 (Qobject,	Fevent_object	(event), props);
      props = cons3 (Qfunction, Fevent_function (event), props);
      break;

    case magic_eval_event:
    case magic_event:
      break;

    case empty_event:
      RETURN_UNGCPRO (Qnil);
      break;
    }

  props = cons3 (Qchannel, Fevent_channel (event), props);
  UNGCPRO;

  return props;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_events (void)
{
  INIT_LRECORD_IMPLEMENTATION (event);
#ifdef USE_KKCC
  INIT_LRECORD_IMPLEMENTATION (key_data);
  INIT_LRECORD_IMPLEMENTATION (button_data);
  INIT_LRECORD_IMPLEMENTATION (motion_data);
  INIT_LRECORD_IMPLEMENTATION (process_data);
  INIT_LRECORD_IMPLEMENTATION (timeout_data);
  INIT_LRECORD_IMPLEMENTATION (eval_data);
  INIT_LRECORD_IMPLEMENTATION (misc_user_data);
  INIT_LRECORD_IMPLEMENTATION (magic_eval_data);
  INIT_LRECORD_IMPLEMENTATION (magic_data);
#endif /* USE_KKCC */  

  DEFSUBR (Fcharacter_to_event);
  DEFSUBR (Fevent_to_character);

  DEFSUBR (Fmake_event);
  DEFSUBR (Fdeallocate_event);
  DEFSUBR (Fcopy_event);
  DEFSUBR (Feventp);
  DEFSUBR (Fevent_live_p);
  DEFSUBR (Fevent_type);
  DEFSUBR (Fevent_properties);

  DEFSUBR (Fevent_timestamp);
  DEFSUBR (Fevent_timestamp_lessp);
  DEFSUBR (Fevent_key);
  DEFSUBR (Fevent_button);
  DEFSUBR (Fevent_modifier_bits);
  DEFSUBR (Fevent_modifiers);
  DEFSUBR (Fevent_x_pixel);
  DEFSUBR (Fevent_y_pixel);
  DEFSUBR (Fevent_window_x_pixel);
  DEFSUBR (Fevent_window_y_pixel);
  DEFSUBR (Fevent_over_text_area_p);
  DEFSUBR (Fevent_over_modeline_p);
  DEFSUBR (Fevent_over_border_p);
  DEFSUBR (Fevent_over_toolbar_p);
  DEFSUBR (Fevent_over_vertical_divider_p);
  DEFSUBR (Fevent_channel);
  DEFSUBR (Fevent_window);
  DEFSUBR (Fevent_point);
  DEFSUBR (Fevent_closest_point);
  DEFSUBR (Fevent_x);
  DEFSUBR (Fevent_y);
  DEFSUBR (Fevent_modeline_position);
  DEFSUBR (Fevent_glyph);
  DEFSUBR (Fevent_glyph_extent);
  DEFSUBR (Fevent_glyph_x_pixel);
  DEFSUBR (Fevent_glyph_y_pixel);
  DEFSUBR (Fevent_toolbar_button);
  DEFSUBR (Fevent_process);
  DEFSUBR (Fevent_function);
  DEFSUBR (Fevent_object);

  DEFSYMBOL (Qeventp);
  DEFSYMBOL (Qevent_live_p);
  DEFSYMBOL (Qkey_press_event_p);
  DEFSYMBOL (Qbutton_event_p);
  DEFSYMBOL (Qmouse_event_p);
  DEFSYMBOL (Qprocess_event_p);
  DEFSYMBOL (Qkey_press);
  DEFSYMBOL (Qbutton_press);
  DEFSYMBOL (Qbutton_release);
  DEFSYMBOL (Qmisc_user);
  DEFSYMBOL (Qascii_character);

  defsymbol (&QKbackspace, "backspace");
  defsymbol (&QKtab, "tab");
  defsymbol (&QKlinefeed, "linefeed");
  defsymbol (&QKreturn, "return");
  defsymbol (&QKescape, "escape");
  defsymbol (&QKspace, "space");
  defsymbol (&QKdelete, "delete");
}


void
reinit_vars_of_events (void)
{
  Vevent_resource = Qnil;
}

void
vars_of_events (void)
{
  reinit_vars_of_events ();

  DEFVAR_LISP ("character-set-property", &Vcharacter_set_property /*
A symbol used to look up the 8-bit character of a keysym.
To convert a keysym symbol to an 8-bit code, as when that key is
bound to self-insert-command, we will look up the property that this
variable names on the property list of the keysym-symbol.  The window-
system-specific code will set up appropriate properties and set this
variable.
*/ );
  Vcharacter_set_property = Qnil;
}
