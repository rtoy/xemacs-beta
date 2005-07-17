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

#ifdef HAVE_TTY
#define USED_IF_TTY(decl) decl
#else
#define USED_IF_TTY(decl) UNUSED (decl)
#endif

#ifdef HAVE_TOOLBARS
#define USED_IF_TOOLBARS(decl) decl
#else
#define USED_IF_TOOLBARS(decl) UNUSED (decl)
#endif

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
Lisp_Object Qcharacter_of_keysym, Qascii_character;


/************************************************************************/
/*                       definition of event object                     */
/************************************************************************/

/* #### Ad-hoc hack.  Should be part of define_lrecord_implementation */
void
clear_event_resource (void)
{
  Vevent_resource = Qnil;
}

/* Make sure we lose quickly if we try to use this event */
static void
deinitialize_event (Lisp_Object ev)
{
  Lisp_Event *event = XEVENT (ev);

  int i;
  for (i = 0; i < (int) (sizeof (Lisp_Event) / sizeof (int)); i++)
    ((int *) event) [i] = 0xdeadbeef; /* -559038737 base 10 */
  set_lheader_implementation (&event->lheader, &lrecord_event);
  set_event_type (event, dead_event);
  SET_EVENT_CHANNEL (event, Qnil);
  XSET_EVENT_NEXT (ev, Qnil);
}

/* Set everything to zero or nil so that it's predictable. */
void
zero_event (Lisp_Event *e)
{
  xzero (*e);
  set_lheader_implementation (&e->lheader, &lrecord_event);
  set_event_type (e, empty_event);
  SET_EVENT_CHANNEL (e, Qnil);
  SET_EVENT_NEXT (e, Qnil);
}

static const struct memory_description key_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Key_Data, keysym) },
  { XD_END }
};

static const struct sized_memory_description key_data_description = {
  sizeof (Lisp_Key_Data), key_data_description_1
};

static const struct memory_description button_data_description_1 [] = {
  { XD_END }
};

static const struct sized_memory_description button_data_description = {
  sizeof (Lisp_Button_Data), button_data_description_1
};

static const struct memory_description motion_data_description_1 [] = {
  { XD_END }
};

static const struct sized_memory_description motion_data_description = {
  sizeof (Lisp_Motion_Data), motion_data_description_1
};

static const struct memory_description process_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Process_Data, process) },
  { XD_END }
};

static const struct sized_memory_description process_data_description = {
  sizeof (Lisp_Process_Data), process_data_description_1
};

static const struct memory_description timeout_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Timeout_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Timeout_Data, object) },
  { XD_END }
};

static const struct sized_memory_description timeout_data_description = {
  sizeof (Lisp_Timeout_Data), timeout_data_description_1
};

static const struct memory_description eval_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Eval_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Eval_Data, object) },
  { XD_END }
};

static const struct sized_memory_description eval_data_description = {
  sizeof (Lisp_Eval_Data), eval_data_description_1
};

static const struct memory_description misc_user_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Misc_User_Data, function) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Misc_User_Data, object) },
  { XD_END }
};

static const struct sized_memory_description misc_user_data_description = {
  sizeof (Lisp_Misc_User_Data), misc_user_data_description_1
};

static const struct memory_description magic_eval_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Magic_Eval_Data, object) },
  { XD_END }
};

static const struct sized_memory_description magic_eval_data_description = {
  sizeof (Lisp_Magic_Eval_Data), magic_eval_data_description_1
};

static const struct memory_description magic_data_description_1 [] = {
  { XD_END }
};

static const struct sized_memory_description magic_data_description = {
  sizeof (Lisp_Magic_Data), magic_data_description_1
};

static const struct memory_description event_data_description_1 [] = {
  { XD_BLOCK_ARRAY, key_press_event, 1, { &key_data_description } },
  { XD_BLOCK_ARRAY, button_press_event, 1, { &button_data_description } },
  { XD_BLOCK_ARRAY, button_release_event, 1, { &button_data_description } },
  { XD_BLOCK_ARRAY, pointer_motion_event, 1, { &motion_data_description } },
  { XD_BLOCK_ARRAY, process_event, 1, { &process_data_description } },
  { XD_BLOCK_ARRAY, timeout_event, 1, { &timeout_data_description } },
  { XD_BLOCK_ARRAY, magic_event, 1, { &magic_data_description } },
  { XD_BLOCK_ARRAY, magic_eval_event, 1, { &magic_eval_data_description } },
  { XD_BLOCK_ARRAY, eval_event, 1, { &eval_data_description } },
  { XD_BLOCK_ARRAY, misc_user_event, 1, { &misc_user_data_description } },
  { XD_END }
};

static const struct sized_memory_description event_data_description = {
  0, event_data_description_1
};

static const struct memory_description event_description [] = {
  { XD_INT, offsetof (struct Lisp_Event, event_type) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Event, next) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Event, channel) },
  { XD_UNION, offsetof (struct Lisp_Event, event), 
    XD_INDIRECT (0, 0), { &event_data_description } },
  { XD_END }
};

#ifdef EVENT_DATA_AS_OBJECTS

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("key-data", key_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     key_data_description, 
				     Lisp_Key_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("button-data", button_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     button_data_description, 
				     Lisp_Button_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("motion-data", motion_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     motion_data_description,
				     Lisp_Motion_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("process-data", process_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     process_data_description,
				     Lisp_Process_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("timeout-data", timeout_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     timeout_data_description,
				     Lisp_Timeout_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("eval-data", eval_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     eval_data_description,
				     Lisp_Eval_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("misc-user-data", misc_user_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     misc_user_data_description, 
				     Lisp_Misc_User_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("magic-eval-data", magic_eval_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     magic_eval_data_description, 
				     Lisp_Magic_Eval_Data);

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("magic-data", magic_data,
				     0, /*dumpable-flag*/
				     0, 0, 0, 0, 0,
				     magic_data_description,
				     Lisp_Magic_Data);

#endif /* EVENT_DATA_AS_OBJECTS */

static Lisp_Object
mark_event (Lisp_Object obj)
{
  Lisp_Event *event = XEVENT (obj);

  switch (event->event_type)
    {
    case key_press_event:
      mark_object (EVENT_KEY_KEYSYM (event));
      break;
    case process_event:
      mark_object (EVENT_PROCESS_PROCESS (event));
      break;
    case timeout_event:
      mark_object (EVENT_TIMEOUT_FUNCTION (event));
      mark_object (EVENT_TIMEOUT_OBJECT (event));
      break;
    case eval_event:
    case misc_user_event:
      mark_object (EVENT_EVAL_FUNCTION (event));
      mark_object (EVENT_EVAL_OBJECT (event));
      break;
    case magic_eval_event:
      mark_object (EVENT_MAGIC_EVAL_OBJECT (event));
      break;
    case button_press_event:
    case button_release_event:
    case pointer_motion_event:
    case magic_event:
    case empty_event:
    case dead_event:
      break;
    default:
      ABORT ();
    }
  mark_object (event->channel);
  return event->next;
}

static void
print_event_1 (const char *str, Lisp_Object obj, Lisp_Object printcharfun)
{
  DECLARE_EISTRING_MALLOC (ei);
  write_c_string (printcharfun, str);
  format_event_object (ei, obj, 0);
  write_eistring (printcharfun, ei);
  eifree (ei);
}

static void
print_event (Lisp_Object obj, Lisp_Object printcharfun,
	     int UNUSED (escapeflag))
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
	write_fmt_string_lisp (printcharfun, "#<process-event %S", 1,
			       XEVENT_PROCESS_PROCESS (obj));
	break;
    case timeout_event:
	write_fmt_string_lisp (printcharfun, "#<timeout-event %S", 1,
			       XEVENT_TIMEOUT_OBJECT (obj));
	break;
    case empty_event:
	write_c_string (printcharfun, "#<empty-event");
	break;
    case misc_user_event:
	write_fmt_string_lisp (printcharfun, "#<misc-user-event (%S", 1,
			       XEVENT_MISC_USER_FUNCTION (obj));
	write_fmt_string_lisp (printcharfun, " %S)", 1,
			       XEVENT_MISC_USER_OBJECT (obj));
	break;
    case eval_event:
	write_fmt_string_lisp (printcharfun, "#<eval-event (%S", 1,
			       XEVENT_EVAL_FUNCTION (obj));
	write_fmt_string_lisp (printcharfun, " %S)", 1,
			       XEVENT_EVAL_OBJECT (obj));
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
event_equal (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth))
{
  Lisp_Event *e1 = XEVENT (obj1);
  Lisp_Event *e2 = XEVENT (obj2);

  if (e1->event_type != e2->event_type) return 0;
  if (!EQ (e1->channel, e2->channel)) return 0;
/*  if (e1->timestamp != e2->timestamp) return 0; */
  switch (e1->event_type)
    {
    default: ABORT ();

    case process_event:
      return EQ (EVENT_PROCESS_PROCESS (e1), EVENT_PROCESS_PROCESS (e2));

    case timeout_event:
      return (internal_equal (EVENT_TIMEOUT_FUNCTION (e1),
			      EVENT_TIMEOUT_FUNCTION (e2), 0) &&
	      internal_equal (EVENT_TIMEOUT_OBJECT (e1),
			      EVENT_TIMEOUT_OBJECT (e2), 0));

    case key_press_event:
      return (EQ (EVENT_KEY_KEYSYM (e1), EVENT_KEY_KEYSYM (e2)) &&
	      (EVENT_KEY_MODIFIERS (e1) == EVENT_KEY_MODIFIERS (e2)));

    case button_press_event:
    case button_release_event:
      return (EVENT_BUTTON_BUTTON (e1)    == EVENT_BUTTON_BUTTON (e2) &&
	      EVENT_BUTTON_MODIFIERS (e1) == EVENT_BUTTON_MODIFIERS (e2));

    case pointer_motion_event:
      return (EVENT_MOTION_X (e1) == EVENT_MOTION_X (e2) &&
	      EVENT_MOTION_Y (e1) == EVENT_MOTION_Y (e2));

    case misc_user_event:
      return (internal_equal (EVENT_EVAL_FUNCTION (e1),
			      EVENT_EVAL_FUNCTION (e2), 0) &&
	      internal_equal (EVENT_EVAL_OBJECT (e1),
			      EVENT_EVAL_OBJECT (e2), 0) &&
	      /* #### is this really needed for equality
	         or is x and y also important? */
	      EVENT_MISC_USER_BUTTON (e1)    == EVENT_MISC_USER_BUTTON (e2) &&
	      EVENT_MISC_USER_MODIFIERS (e1) == EVENT_MISC_USER_MODIFIERS (e2));

    case eval_event:
      return (internal_equal (EVENT_EVAL_FUNCTION (e1),
			      EVENT_EVAL_FUNCTION (e2), 0) &&
	      internal_equal (EVENT_EVAL_OBJECT (e1),
			      EVENT_EVAL_OBJECT (e2), 0));

    case magic_eval_event:
      return (EVENT_MAGIC_EVAL_INTERNAL_FUNCTION (e1) ==
	      EVENT_MAGIC_EVAL_INTERNAL_FUNCTION (e2) &&
	      internal_equal (EVENT_MAGIC_EVAL_OBJECT (e1),
			      EVENT_MAGIC_EVAL_OBJECT (e2), 0));

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
      return HASH2 (hash, LISP_HASH (EVENT_PROCESS_PROCESS (e)));

    case timeout_event:
      return HASH3 (hash,
		    internal_hash (EVENT_TIMEOUT_FUNCTION (e), depth + 1),
		    internal_hash (EVENT_TIMEOUT_OBJECT (e), depth + 1));

    case key_press_event:
      return HASH3 (hash, LISP_HASH (EVENT_KEY_KEYSYM (e)),
		    EVENT_KEY_MODIFIERS (e));

    case button_press_event:
    case button_release_event:
      return HASH3 (hash, EVENT_BUTTON_BUTTON (e), EVENT_BUTTON_MODIFIERS (e));

    case pointer_motion_event:
      return HASH3 (hash, EVENT_MOTION_X (e), EVENT_MOTION_Y (e));

    case misc_user_event:
      return HASH5 (hash,
		    internal_hash (EVENT_MISC_USER_FUNCTION (e), depth + 1),
		    internal_hash (EVENT_MISC_USER_OBJECT (e), depth + 1),
		    EVENT_MISC_USER_BUTTON (e), EVENT_MISC_USER_MODIFIERS (e));

    case eval_event:
      return HASH3 (hash, internal_hash (EVENT_EVAL_FUNCTION (e), depth + 1),
		    internal_hash (EVENT_EVAL_OBJECT (e), depth + 1));

    case magic_eval_event:
      return HASH3 (hash,
		    (Hashcode) EVENT_MAGIC_EVAL_INTERNAL_FUNCTION (e),
		    internal_hash (EVENT_MAGIC_EVAL_OBJECT (e), depth + 1));

    case magic_event:
      return HASH2 (hash, event_stream_hash_magic_event (e));

    case empty_event:
    case dead_event:
      return hash;

    default:
      ABORT ();
    }

  return 0; /* unreached */
}

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("event", event,
				     0, /*dumpable-flag*/
				     mark_event, print_event, 0, event_equal,
				     event_hash, event_description,
				     Lisp_Event);

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
      set_event_type (e, empty_event);
      if (!NILP (plist))
	invalid_operation ("Cannot set properties of empty event", plist);
      UNGCPRO;
      return event;
    }
  else if (EQ (type, Qkey_press))
    {
      set_event_type (e, key_press_event);
      SET_EVENT_KEY_KEYSYM (e, Qunbound);
    }
  else if (EQ (type, Qbutton_press))
    set_event_type (e, button_press_event);
  else if (EQ (type, Qbutton_release))
    set_event_type (e, button_release_event);
  else if (EQ (type, Qmotion))
    set_event_type (e, pointer_motion_event);
  else if (EQ (type, Qmisc_user))
    {
      set_event_type (e, misc_user_event);
      SET_EVENT_MISC_USER_FUNCTION (e, Qnil); 
      SET_EVENT_MISC_USER_OBJECT (e, Qnil);
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
	    if (EVENT_TYPE (e) == key_press_event)
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
	    switch (EVENT_TYPE (e))
	      {
	      case key_press_event:
		if (!SYMBOLP (value) && !CHARP (value))
		  invalid_argument ("Invalid event key", value);
		SET_EVENT_KEY_KEYSYM (e, value);
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

	    switch (EVENT_TYPE (e))
	      {
	      case button_press_event:
	      case button_release_event:
		SET_EVENT_BUTTON_BUTTON (e, XINT (value));
		break;
	      case misc_user_event:
		SET_EVENT_MISC_USER_BUTTON (e, XINT (value));
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

	    switch (EVENT_TYPE (e))
	      {
	      case key_press_event:
                SET_EVENT_KEY_MODIFIERS (e, modifiers);
		break;
	      case button_press_event:
	      case button_release_event:
                SET_EVENT_BUTTON_MODIFIERS (e, modifiers);
		break;
	      case pointer_motion_event:
                SET_EVENT_MOTION_MODIFIERS (e, modifiers);
		break;
	      case misc_user_event:
                SET_EVENT_MISC_USER_MODIFIERS (e, modifiers);
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qx))
	  {
	    switch (EVENT_TYPE (e))
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
	    switch (EVENT_TYPE (e))
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
	    SET_EVENT_TIMESTAMP (e, XINT (value));
	  }
	else if (EQ (keyword, Qfunction))
	  {
	    switch (EVENT_TYPE (e))
	      {
	      case misc_user_event:
                SET_EVENT_MISC_USER_FUNCTION (e, value);
		break;
	      default:
		WRONG_EVENT_TYPE_FOR_PROPERTY (type, keyword);
		break;
	      }
	  }
	else if (EQ (keyword, Qobject))
	  {
	    switch (EVENT_TYPE (e))
	      {
	      case misc_user_event:
                SET_EVENT_MISC_USER_OBJECT (e, value);
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
      if (EVENT_TYPE (e) == key_press_event)
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
	  SET_EVENT_MOTION_X (e, coord_x);
	  SET_EVENT_MOTION_Y (e, coord_y);
	  break;
	case button_press_event:
	case button_release_event:
	  SET_EVENT_BUTTON_X (e, coord_x);
	  SET_EVENT_BUTTON_Y (e, coord_y);
	  break;
	case misc_user_event:
	  SET_EVENT_MISC_USER_X (e, coord_x);
	  SET_EVENT_MISC_USER_Y (e, coord_y);
	  break;
	default:
	  ABORT ();
	}
    }

  /* Finally, do some more validation.  */
  switch (EVENT_TYPE (e))
    {
    case key_press_event:
      if (UNBOUNDP (EVENT_KEY_KEYSYM (e)))
	sferror ("A key must be specified to make a keypress event",
		      plist);
      break;
    case button_press_event:
      if (!EVENT_BUTTON_BUTTON (e))
	sferror
	  ("A button must be specified to make a button-press event",
	   plist);
      break;
    case button_release_event:
      if (!EVENT_BUTTON_BUTTON (e))
	sferror
	  ("A button must be specified to make a button-release event",
	   plist);
      break;
    case misc_user_event:
      if (NILP (EVENT_MISC_USER_FUNCTION (e)))
	sferror ("A function must be specified to make a misc-user event",
		      plist);
      break;
    default:
      break;
    }

  UNGCPRO;
  return event;
}

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
      ABORT ();

    len = XVECTOR_LENGTH (Vthis_command_keys);
    for (i = 0; i < len; i++)
      if (EQ (event, XVECTOR_DATA (Vthis_command_keys) [i]))
	ABORT ();
    if (!NILP (Vrecent_keys_ring))
      {
	int recent_ring_len = XVECTOR_LENGTH (Vrecent_keys_ring);
	for (i = 0; i < recent_ring_len; i++)
	  if (EQ (event, XVECTOR_DATA (Vrecent_keys_ring) [i]))
	    ABORT ();
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

  XSET_EVENT_TYPE (event2, XEVENT_TYPE (event1));
  XSET_EVENT_CHANNEL (event2, XEVENT_CHANNEL (event1));
  XSET_EVENT_TIMESTAMP (event2, XEVENT_TIMESTAMP (event1));
  
#ifdef EVENT_DATA_AS_OBJECTS
  copy_lisp_object (XEVENT_DATA (event2), XEVENT_DATA (event1));
#else
  XEVENT (event2)->event = XEVENT (event1)->event;
#endif
  return event2;
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

/* Map a function over each event in the chain.  If the function returns
   non-zero, remove the event just processed.  Return the total number of
   items removed.

   NOTE:

   If you want a simple mapping over an event chain, with no intention to
   add or remove items, just use EVENT_CHAIN_LOOP().
*/

int
map_event_chain_remove (int (*fn) (Lisp_Object ev, void *user_data),
			Lisp_Object *head, Lisp_Object *tail,
			void *user_data, int flags)
{
  Lisp_Object event;
  Lisp_Object previous_event = Qnil;
  int count = 0;

  EVENT_CHAIN_LOOP (event, *head)
    {
      if (fn (event, user_data))
	{
	  if (NILP (previous_event))
	    dequeue_event (head, tail);
	  else
	    {
	      XSET_EVENT_NEXT (previous_event, XEVENT_NEXT (event));
	      if (EQ (*tail, event))
		*tail = previous_event;
	    }

	  if (flags & MECR_DEALLOCATE_EVENT)
	    Fdeallocate_event (event);
	  count++;
	}
      else
	previous_event = event;
    }
  return count;
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

  ABORT ();
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

/* USE_CONSOLE_META_FLAG is as in `character-to-event'.
   DO_BACKSPACE_MAPPING means that if CON is a TTY, and C is a the TTY's
   backspace character, the event will have keysym `backspace' instead of
   '(control h).  It is clearly correct to do this conversion is the
   character was just read from a TTY, clearly incorrect inside of
   define-key, which must be able to handle all consoles.  #### What about
   in other circumstances?  #### Should the user have access to this flag? 

   #### We need to go through and review all the flags in
   character_to_event() and event_to_character() and figure out exactly
   under what circumstances they should or should not be set, then go
   through and review all callers of character_to_event(),
   Fcharacter_to_event(), event_to_character(), and Fevent_to_character()
   and check that they are passing the correct flags in for their varied
   circumstances.

   #### Some of this garbage, and some of the flags, could go away if we
   implemented the suggestion, originally from event-Xt.c:

   [[ The way that keysym correspondence to characters should work:
   - a Lisp_Event should contain a keysym AND a character slot.
   - keybindings are tried with the keysym.  If no binding can be found,
     and there is a corresponding character, call self-insert-command. ]]

       That's an X-specific way of thinking. All the other platforms--even
       the TTY, make sure you've done (set-input-mode t nil 1) and set your
       console coding system appropriately when checking--just use
       characters as emacs keysyms, and, together with defaulting to
       self-insert-command if an unbound key with a character correspondence
       is typed, that works fine for them. (Yes, this ignores GTK.)

   [[  [... snipping other suggestions which I've implemented.]
       Nuke the Qascii_character property. ]]

       Well, we've renamed it anyway--it was badly named.
       Qcharacter_of_keysym, here we go. It's really only with X11 that how
       to map between adiaeresis and (int-to-char #xE4), or ellipsis and
       whatever, becomes an issue, and IMO the property approach to this is
       fine. Aidan Kehoe, 2005-05-15.

   [[ This would apparently solve a lot of different problems. ]]

      I'd be interested to know what's left. Removing the allow-meta
      argument from event-to-character would be a Good Thing, IMO, but
      beyond that, I'm not sure what else there is to do wrt. key
      mappings. Of course, feedback from users of the Russian C-x facility
      is still needed. */

void
character_to_event (Ichar c, Lisp_Event *event, struct console *con,
		    int use_console_meta_flag,
		    int USED_IF_TTY (do_backspace_mapping))
{
  Lisp_Object k = Qnil;
  int m = 0;
  if (EVENT_TYPE (event) == dead_event)
    invalid_argument ("character-to-event called with a deallocated event!", Qunbound);

#ifndef MULE
  c &= 255;
#endif
  if (c > 127 && c <= 255)
    {
      /* #### What if the user wanted a Latin-1 char?  Perhaps the answer
	 is what was suggested above.
      */
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
#if defined (HAVE_TTY)
	  if (do_backspace_mapping &&
	      CHARP (con->tty_erase_char) &&
	      c - '@' == XCHAR (con->tty_erase_char))
	    {
	      k = QKbackspace;
	      m &= ~XEMACS_MOD_CONTROL;
	    }
#endif /* defined (HAVE_TTY) */
	  break;
	}
      if (c >= 'A' && c <= 'Z') c -= 'A'-'a';
    }
#if defined (HAVE_TTY)
  else if (do_backspace_mapping &&
	   CHARP (con->tty_erase_char) && c == XCHAR (con->tty_erase_char))
    k = QKbackspace;
#endif /* defined (HAVE_TTY) */
  else if (c == 127)
    k = QKdelete;
  else if (c == ' ')
    k = QKspace;

  set_event_type (event, key_press_event);
  SET_EVENT_TIMESTAMP_ZERO (event); /* #### */
  SET_EVENT_CHANNEL (event, wrap_console (con));
  SET_EVENT_KEY_KEYSYM (event, (!NILP (k) ? k : make_char (c)));
  SET_EVENT_KEY_MODIFIERS (event, m);
}

Ichar
event_to_character (Lisp_Object event,
		    int allow_extra_modifiers,
		    int allow_meta)
{
  Ichar c = 0;
  Lisp_Object code;

  if (XEVENT_TYPE (event) != key_press_event)
    {
      assert (XEVENT_TYPE (event) != dead_event);
      return -1;
    }
  if (!allow_extra_modifiers &&
      XEVENT_KEY_MODIFIERS (event) & 
      (XEMACS_MOD_SUPER|XEMACS_MOD_HYPER|XEMACS_MOD_ALT))
    return -1;
  if (CHAR_OR_CHAR_INTP (XEVENT_KEY_KEYSYM (event)))
    c = XCHAR_OR_CHAR_INT (XEVENT_KEY_KEYSYM (event));
  else if (!SYMBOLP (XEVENT_KEY_KEYSYM (event)))
    ABORT ();
  else if (CHAR_OR_CHAR_INTP (code = Fget (XEVENT_KEY_KEYSYM (event),
					   Qcharacter_of_keysym, Qnil)))
    c = XCHAR_OR_CHAR_INT (code);
  else
    {
      Lisp_Object thekeysym = XEVENT_KEY_KEYSYM (event);

      if (CHAR_OR_CHAR_INTP (code = Fget (thekeysym, Qascii_character, Qnil)))
	{
	  c = XCHAR_OR_CHAR_INT (code);
	  warn_when_safe(Qkey_mapping, Qwarning, 
			 "Obsolete key binding technique.\n"

"Some code you're using bound %s to `self-insert-command' and messed around\n"
"with its `ascii-character' property.  Doing this is deprecated, and the code\n"
"should be updated to use the `set-character-of-keysym' interface.\n"
"If you're the one updating the code, first check if there's still a need\n"
"for it; we support many more X11 keysyms out of the box now than we did\n"
"in the past. ", XSTRING_DATA(XSYMBOL_NAME(thekeysym)));
	  /* Only show the warning once for each keysym. */
	  Fput(thekeysym, Qcharacter_of_keysym, code);
	}
      else
	{
	  return -1;
	}
    }
  if (XEVENT_KEY_MODIFIERS (event) & XEMACS_MOD_CONTROL)
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

  if (XEVENT_KEY_MODIFIERS (event) & XEMACS_MOD_META)
    {
      if (! allow_meta) return -1;
      if (c >= 128) return -1;		/* don't allow M-oslash (overlap) */
      c |= 0200;
    }
  return c;
}

DEFUN ("event-to-character", Fevent_to_character, 1, 4, 0, /*
Return the closest character approximation to the given event object.
If the event isn't a keypress, this returns nil.
If the ALLOW-EXTRA-MODIFIERS argument is non-nil, then this is lenient in
 its translation; it will ignore modifier keys other than control and meta,
 and will ignore the shift modifier on those characters which have no
 shifted ASCII equivalent (Control-Shift-A for example, will be mapped to
 the same ASCII code as Control-A).
If the ALLOW-META argument is non-nil, then the Meta modifier will be
 represented by turning on the high bit of the byte returned; otherwise, nil
 will be returned for events containing the Meta modifier.
Note that ALLOW-META may cause ambiguity between meta characters and
 Latin-1 characters.
ALLOW-NON-ASCII is unused, and retained for compatibility. 
*/
       (event, allow_extra_modifiers, allow_meta, UNUSED(allow_non_ascii)))
{
  Ichar c;
  CHECK_LIVE_EVENT (event);
  c = event_to_character (event,
			  !NILP (allow_extra_modifiers),
			  !NILP (allow_meta));
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
interpreted as the meta key. (This is done for backward compatibility in
lots of places -- specifically, because lots of Lisp code uses specs like
?\M-d and "\M-d" in key code, expecting this to work; yet these are in
reality converted directly to 8-bit characters by the Lisp reader.)  If
USE-CONSOLE-META-FLAG is nil or CONSOLE is not a TTY, this will always be
the case.  If USE-CONSOLE-META-FLAG is non-nil and CONSOLE is a TTY, the
`meta' flag for CONSOLE affects whether the high bit is interpreted as a
meta key. (See `set-input-mode'.)  Don't set this flag to non-nil unless
you know what you're doing (more specifically, only if the character came
directly from a TTY, not from the user).  If you don't want this silly meta
interpretation done, you should pass in a list containing the character.

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

void
format_event_object (Eistring *buf, Lisp_Object event, int brief)
{
  int mouse_p = 0;
  int mod = 0;
  Lisp_Object key;

  switch (XEVENT_TYPE (event))
    {
    case key_press_event:
      {
	mod = XEVENT_KEY_MODIFIERS (event);
	key = XEVENT_KEY_KEYSYM (event);
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
	mod = XEVENT_BUTTON_MODIFIERS (event);
	key = make_char (XEVENT_BUTTON_BUTTON (event) + '0');
        break;
      }
    case magic_event:
      {
	Lisp_Object stream;
	struct gcpro gcpro1;
	GCPRO1 (stream);

	stream = make_resizing_buffer_output_stream ();
	event_stream_format_magic_event (XEVENT (event), stream);
	Lstream_flush (XLSTREAM (stream));
	eicat_raw (buf, resizing_buffer_stream_ptr (XLSTREAM (stream)),
		   Lstream_byte_count (XLSTREAM (stream)));
	Lstream_delete (XLSTREAM (stream));
	UNGCPRO;
	return;
      }
    case magic_eval_event:	eicat_ascii (buf, "magic-eval"); return;
    case pointer_motion_event:	eicat_ascii (buf, "motion");     return;
    case misc_user_event:	eicat_ascii (buf, "misc-user");  return;
    case eval_event:		eicat_ascii (buf, "eval");	     return;
    case process_event:		eicat_ascii (buf, "process");    return;
    case timeout_event:		eicat_ascii (buf, "timeout");    return;
    case empty_event:		eicat_ascii (buf, "empty");	     return;
    case dead_event:		eicat_ascii (buf, "DEAD-EVENT"); return;
    default:
      ABORT ();
      return;
    }
#define modprint(x,y) \
  do { if (brief) eicat_ascii (buf, (y)); else eicat_ascii (buf, (x)); } while (0)
  if (mod & XEMACS_MOD_CONTROL) modprint ("control-", "C-");
  if (mod & XEMACS_MOD_META)    modprint ("meta-",    "M-");
  if (mod & XEMACS_MOD_SUPER)   modprint ("super-",   "S-");
  if (mod & XEMACS_MOD_HYPER)   modprint ("hyper-",   "H-");
  if (mod & XEMACS_MOD_ALT)	modprint ("alt-",     "A-");
  if (mod & XEMACS_MOD_SHIFT)   modprint ("shift-",   "Sh-");
  if (mouse_p)
    {
      eicat_ascii (buf, "button");
      --mouse_p;
    }

#undef modprint

  if (CHARP (key))
    eicat_ch (buf, XCHAR (key));
  else if (SYMBOLP (key))
    {
      const Ascbyte *str = 0;
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
	eicat_ascii (buf, str);
      else
	eicat_lstr (buf, XSYMBOL (key)->name);
    }
  else
    ABORT ();
  if (mouse_p)
    eicat_ascii (buf, "up");
}

void
upshift_event (Lisp_Object event)
{
  Lisp_Object keysym = XEVENT_KEY_KEYSYM (event);
  Ichar c = 0;

  if (CHAR_OR_CHAR_INTP (keysym)
      && ((c = XCHAR_OR_CHAR_INT (keysym)),
	  c >= 'a' && c <= 'z'))
    XSET_EVENT_KEY_KEYSYM (event, make_char (c + 'A' - 'a'));
  else
    if (!(XEVENT_KEY_MODIFIERS (event) & XEMACS_MOD_SHIFT))
      XSET_EVENT_KEY_MODIFIERS
	(event, XEVENT_KEY_MODIFIERS (event) |= XEMACS_MOD_SHIFT);
}

void
downshift_event (Lisp_Object event)
{
  Lisp_Object keysym = XEVENT_KEY_KEYSYM (event);
  Ichar c = 0;

  if (XEVENT_KEY_MODIFIERS (event) & XEMACS_MOD_SHIFT)
    XSET_EVENT_KEY_MODIFIERS
      (event, XEVENT_KEY_MODIFIERS (event) & ~XEMACS_MOD_SHIFT);
  else if (CHAR_OR_CHAR_INTP (keysym)
	   && ((c = XCHAR_OR_CHAR_INT (keysym)),
	       c >= 'A' && c <= 'Z'))
    XSET_EVENT_KEY_KEYSYM (event, make_char (c + 'a' - 'A'));
}

int
event_upshifted_p (Lisp_Object event)
{
  Lisp_Object keysym = XEVENT_KEY_KEYSYM (event);
  Ichar c = 0;

  if ((XEVENT_KEY_MODIFIERS (event) & XEMACS_MOD_SHIFT)
      || (CHAR_OR_CHAR_INTP (keysym)
	  && ((c = XCHAR_OR_CHAR_INT (keysym)),
	      c >= 'A' && c <= 'Z')))
    return 1;
  else
    return 0;
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
  return EVENTP (object) && XEVENT_TYPE (object) != dead_event ?
    Qt : Qnil;
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
  switch (XEVENT_TYPE (event))
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
      ABORT ();
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
  return make_int (EMACS_INT_MAX & XEVENT_TIMESTAMP (event));
}

#define TIMESTAMP_HALFSPACE (1L << (INT_VALBITS - 2))

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

#define CHECK_EVENT_TYPE(e,t1,sym) do {		\
  CHECK_LIVE_EVENT (e);				\
  if (XEVENT_TYPE (e) != (t1))	        	\
    e = wrong_type_argument (sym,e);		\
} while (0)

#define CHECK_EVENT_TYPE2(e,t1,t2,sym) do {		\
  CHECK_LIVE_EVENT (e);					\
  {							\
    emacs_event_type CET_type = XEVENT_TYPE (e);	\
    if (CET_type != (t1) &&				\
	CET_type != (t2))				\
      e = wrong_type_argument (sym,e);			\
  }							\
} while (0)

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

DEFUN ("event-key", Fevent_key, 1, 1, 0, /*
Return the Keysym of the key-press event EVENT.
This will be a character if the event is associated with one, else a symbol.
*/
       (event))
{
  CHECK_EVENT_TYPE (event, key_press_event, Qkey_press_event_p);
  return XEVENT_KEY_KEYSYM (event);
}

DEFUN ("event-button", Fevent_button, 1, 1, 0, /*
Return the button-number of the button-press or button-release event EVENT.
*/
       (event))
{
  CHECK_EVENT_TYPE3 (event, button_press_event, button_release_event,
		     misc_user_event, Qbutton_event_p);
#ifdef HAVE_WINDOW_SYSTEM
  if (XEVENT_TYPE (event) == misc_user_event)
    return make_int (XEVENT_MISC_USER_BUTTON (event));
  else
    return make_int (XEVENT_BUTTON_BUTTON (event));
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
  switch (XEVENT_TYPE (event))
    {
    case key_press_event:
      return make_int (XEVENT_KEY_MODIFIERS (event));
    case button_press_event:
    case button_release_event:
      return make_int (XEVENT_BUTTON_MODIFIERS (event));
    case pointer_motion_event:
      return make_int (XEVENT_MOTION_MODIFIERS (event));
    case misc_user_event:
      return make_int (XEVENT_MISC_USER_MODIFIERS (event));
    default:
      event = wrong_type_argument (intern ("key-or-mouse-event-p"), event);
      goto again;
    }
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

  if (XEVENT_TYPE (event) == pointer_motion_event)
    {
      *x = XEVENT_MOTION_X (event);
      *y = XEVENT_MOTION_Y (event);
    }
  else if (XEVENT_TYPE (event) == button_press_event ||
	   XEVENT_TYPE (event) == button_release_event)
    {
      *x = XEVENT_BUTTON_X (event);
      *y = XEVENT_BUTTON_Y (event);
    }
  else if (XEVENT_TYPE (event) == misc_user_event)
    {
      *x = XEVENT_MISC_USER_X (event);
      *y = XEVENT_MISC_USER_Y (event);
    }
  else
    return 0;
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
  frame = XEVENT_CHANNEL (event);
  switch (XEVENT_TYPE (event))
    {
    case pointer_motion_event :
      pix_x = XEVENT_MOTION_X (event);
      pix_y = XEVENT_MOTION_Y (event);
      break;
    case button_press_event :
    case button_release_event :
      pix_x = XEVENT_BUTTON_X (event);
      pix_y = XEVENT_BUTTON_Y (event);
      break;
    case misc_user_event :
      pix_x = XEVENT_MISC_USER_X (event);
      pix_y = XEVENT_MISC_USER_Y (event);
      break;
    default:
      dead_wrong_type_argument (Qmouse_event_p, event);
    }

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
    ABORT ();
  if (!NILP (ret_obj2) && !(EXTENTP (ret_obj2) || CONSP (ret_obj2)))
    ABORT ();

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
       (USED_IF_TOOLBARS (event)))
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
  CHECK_EVENT_TYPE (event, process_event, Qprocess_event_p);
  return XEVENT_PROCESS_PROCESS (event);
}

DEFUN ("event-function", Fevent_function, 1, 1, 0, /*
Return the callback function of EVENT.
EVENT should be a timeout, misc-user, or eval event.
*/
       (event))
{
 again:
  CHECK_LIVE_EVENT (event);
  switch (XEVENT_TYPE (event))
    {
    case timeout_event:
      return XEVENT_TIMEOUT_FUNCTION (event);
    case misc_user_event:
      return XEVENT_MISC_USER_FUNCTION (event);
    case eval_event:
      return XEVENT_EVAL_FUNCTION (event);
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
}

DEFUN ("event-object", Fevent_object, 1, 1, 0, /*
Return the callback function argument of EVENT.
EVENT should be a timeout, misc-user, or eval event.
*/
       (event))
{
 again:
  CHECK_LIVE_EVENT (event);
  switch (XEVENT_TYPE (event))
    {
    case timeout_event:
      return XEVENT_TIMEOUT_OBJECT (event);
    case misc_user_event:
      return XEVENT_MISC_USER_OBJECT (event);
    case eval_event:
      return XEVENT_EVAL_OBJECT (event);
    default:
      event = wrong_type_argument (intern ("timeout-or-eval-event-p"), event);
      goto again;
    }
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

  switch (EVENT_TYPE (e))
    {
    default: ABORT ();

    case process_event:
      props = cons3 (Qprocess, EVENT_PROCESS_PROCESS (e), props);
      break;

    case timeout_event:
      props = cons3 (Qobject,	Fevent_object	(event), props);
      props = cons3 (Qfunction, Fevent_function (event), props);
      props = cons3 (Qid, make_int (EVENT_TIMEOUT_ID_NUMBER (e)), props);
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
#ifdef EVENT_DATA_AS_OBJECTS
  INIT_LRECORD_IMPLEMENTATION (key_data);
  INIT_LRECORD_IMPLEMENTATION (button_data);
  INIT_LRECORD_IMPLEMENTATION (motion_data);
  INIT_LRECORD_IMPLEMENTATION (process_data);
  INIT_LRECORD_IMPLEMENTATION (timeout_data);
  INIT_LRECORD_IMPLEMENTATION (eval_data);
  INIT_LRECORD_IMPLEMENTATION (misc_user_data);
  INIT_LRECORD_IMPLEMENTATION (magic_eval_data);
  INIT_LRECORD_IMPLEMENTATION (magic_data);
#endif /* EVENT_DATA_AS_OBJECTS */  

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
  DEFSYMBOL (Qcharacter_of_keysym);
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
}
