/* Definitions for the new event model;
   created 16-jul-91 by Jamie Zawinski
   Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2002 Ben Wing.

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

#ifndef INCLUDED_events_h_
#define INCLUDED_events_h_

#include "systime.h"

/*

See also

  (Info-goto-node "(internals)Event Stream Callback Routines")
  (Info-goto-node "(internals)Stream Pairs")

*/

/* typedef unsigned int USID; in lisp.h */
#define USID_ERROR ((USID)-1)
#define USID_DONTHASH ((USID)0)


struct event_stream
{
  int  (*event_pending_p)	(int);
  void (*next_event_cb)		(Lisp_Event *);
  void (*handle_magic_event_cb)	(Lisp_Event *);
  void (*format_magic_event_cb)	(Lisp_Event *, Lisp_Object pstream);
  int (*compare_magic_event_cb) (Lisp_Event *, Lisp_Event *);
  Hashcode (*hash_magic_event_cb)(Lisp_Event *);
  int  (*add_timeout_cb)	(EMACS_TIME);
  void (*remove_timeout_cb)	(int);
  void (*select_console_cb)	(struct console *);
  void (*unselect_console_cb)	(struct console *);
  void (*select_process_cb)	(Lisp_Process *, int doin, int doerr);
  void (*unselect_process_cb)	(Lisp_Process *, int doin, int doerr);
  void (*drain_queue_cb)	(void);
  void (*force_event_pending_cb)(struct frame* f);
  void (*create_io_streams_cb)  (void* /* inhandle*/, void* /*outhandle*/ ,
				 void * /* errhandle*/,
				 Lisp_Object* /* instream */,
				 Lisp_Object* /* outstream */,
				 Lisp_Object* /* errstream */,
				 USID * /* in_usid */, USID * /* err_usid */,
				 int /* flags */);
  void (*delete_io_streams_cb)  (Lisp_Object /* instream */,
				 Lisp_Object /* outstream */,
				 Lisp_Object /* errstream */,
				 USID * /* in_usid */, USID * /* err_usid */);
  int (*current_event_timestamp_cb) (struct console *);
};

/* Flags for create_io_streams_cb() FLAGS parameter */
#define STREAM_PTY_FLUSHING		0x0001
#define STREAM_NETWORK_CONNECTION	0x0002

extern struct event_stream *event_stream;

#ifdef EVENT_DATA_AS_OBJECTS
#define EVENT_FOO_BAR_1(extractor, field)  ((extractor)->field)
#define EVENT_FOO_BAR(e, uptype, downtype, field) EVENT_FOO_BAR_1 (X##uptype##_DATA (EVENT_DATA (e)), field)
#define SET_EVENT_FOO_BAR_1(extractor, field, val)  \
do { (extractor)->field = (val); } while (0)
#define SET_EVENT_FOO_BAR(e, uptype, downtype, field, val) SET_EVENT_FOO_BAR_1 (X##uptype##_DATA (EVENT_DATA (e)), field, val)
#else
#define EVENT_FOO_BAR(e, uptype, downtype, field) ((e)->event.downtype.field)
#define SET_EVENT_FOO_BAR(e, uptype, downtype, field, val) \
do { (e)->event.downtype.field = (val); } while (0)
#endif

typedef enum emacs_event_type
{
  empty_event,
  key_press_event,
  button_press_event,
  button_release_event,
  pointer_motion_event,
  process_event,
  timeout_event,
  magic_event,
  magic_eval_event,
  eval_event,
  misc_user_event,
  dead_event
} emacs_event_type;

#define first_event_type empty_event
#define last_event_type dead_event

enum alternative_key_chars
{
  KEYCHAR_CURRENT_LANGENV,
  KEYCHAR_DEFAULT_USER,
  KEYCHAR_DEFAULT_SYSTEM,
  KEYCHAR_UNDERLYING_VIRTUAL_KEY_CURRENT_LANGENV,
  KEYCHAR_UNDERLYING_VIRTUAL_KEY_DEFAULT_USER,
  KEYCHAR_UNDERLYING_VIRTUAL_KEY_DEFAULT_SYSTEM,
  KEYCHAR_QWERTY,
  KEYCHAR_LAST
};

struct Lisp_Key_Data
{
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  /* What keysym this is; a character or a symbol. */
  Lisp_Object keysym;
  /* Modifiers held down when key was pressed: control, meta, etc.
     Also includes buttons.  For many keys, Shift is not a bit; that
     is implicit in the keyboard layout. */
  int modifiers;
  /* Alternate character interpretations for this key in different
     keyboard layouts.  This deals with the problem of pressing C-x in
     the Russian layout (the so-called "Russian C-x problem"), for
     example: `x' gets mapped to a Cyrillic character, so what do we
     do?  For that matter, what about `C-x b'?  What we do is look the
     key up in the default locales (current language environment, user
     default, system default), then check to see if the underlying
     virtual key is alphabetic in the same three defaults, then
     finally check US ASCII.  We ignore the underlying virtual key for
     the current layout to avoid the problem of a French speaker
     (AZERTY layout) who temporarily switches to Russian: The virtual
     keys underlying Russian are US-ASCII, so what the French speaker
     things of as C-a (the key just to the right of TAB) appears as
     C-q. 

         I've just implemented this in event-stream.c, and I really want to
         see feedback from actual Russians about it, and whether it needs to
         be much more complete than what I've done. E.g, the mapping back to
         US Qwerty is hardcoded on the X11 side of things, and only deals
         with the alphabetic characters. 

	 Also, I think it's downright confusing for people with Roman
	 letters on their keyboard to have random other letters than are
	 described as calling some command, call that command. So I want to
	 consider enabling it by language environment.

     [[ (#### We should probably ignore the current char and look
     *ONLY* in alt_keychars for all control keys.  What about the
     English speaker who temporarily switches to the French layout and
     finds C-q mapped to C-a?) ]] 

         No, we shouldn't. People who use the French layout expect that
         pressing control with the key to the right of tab passes C-a to
         emacs; English speakers (more exactly, Qwerty users) who
         temporarily switch to the French layout encounter that issue in
         every other app too, and they normally remap the keyboard in
         software as soon as they can, or learn to live with Azerty. That
         applies for all the Roman-alphabet keyboard layouts. Aidan Kehoe,
         2005-05-15

         I've taken out the dependency on MULE for this feature because it's
         also useful in a non-Mule XEmacs where the user has set their font
         to something ending in iso8859-5. How many of those users there
         are, is another question. */
  Ichar alt_keychars[KEYCHAR_LAST];
};

typedef struct Lisp_Key_Data Lisp_Key_Data;

#define KEY_DATA_KEYSYM(d) ((d)->keysym)
#define KEY_DATA_MODIFIERS(d) ((d)->modifiers)
#define SET_KEY_DATA_KEYSYM(d, k) ((d)->keysym = k)
#define SET_KEY_DATA_MODIFIERS(d, m) ((d)->modifiers = m)

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (key_data, Lisp_Key_Data);
#define XKEY_DATA(x) XRECORD (x, key_data, Lisp_Key_Data)
#define wrap_key_data(p) wrap_record (p, key_data)
#define KEY_DATAP(x) RECORDP (x, key_data)
#define CHECK_KEY_DATA(x) CHECK_RECORD (x, key_data)
#define CONCHECK_KEY_DATA(x) CONCHECK_RECORD (x, key_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_KEY_KEYSYM(e) EVENT_FOO_BAR (e, KEY, key, keysym)
#define XEVENT_KEY_KEYSYM(e) EVENT_KEY_KEYSYM (XEVENT (e))
#define SET_EVENT_KEY_KEYSYM(e, val) \
  SET_EVENT_FOO_BAR (e, KEY, key, keysym, val)
#define XSET_EVENT_KEY_KEYSYM(e, val) \
  SET_EVENT_KEY_KEYSYM (XEVENT (e), val)

#define EVENT_KEY_MODIFIERS(e) EVENT_FOO_BAR (e, KEY, key, modifiers)
#define XEVENT_KEY_MODIFIERS(e) EVENT_KEY_MODIFIERS (XEVENT (e))
#define SET_EVENT_KEY_MODIFIERS(e, val) \
  SET_EVENT_FOO_BAR (e, KEY, key, modifiers, val)
#define XSET_EVENT_KEY_MODIFIERS(e, val) \
  SET_EVENT_KEY_MODIFIERS (XEVENT (e), val)

#define EVENT_KEY_ALT_KEYCHARS(e, n) \
  EVENT_FOO_BAR (e, KEY, key, alt_keychars[n])
#define XEVENT_KEY_ALT_KEYCHARS(e, n) EVENT_KEY_ALT_KEYCHARS (XEVENT (e), n)
#define SET_EVENT_KEY_ALT_KEYCHARS(e, n, val) \
  SET_EVENT_FOO_BAR (e, KEY, key, alt_keychars[n], val)
#define XSET_EVENT_KEY_ALT_KEYCHARS(e, n, val) \
  SET_EVENT_KEY_ALT_KEYCHARS (XEVENT (e), n, val)

struct Lisp_Button_Data
{
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  /* What button went down or up. */
  int button;
  /* Bucky-bits on that button: shift, control, meta, etc.  Also
     includes other buttons (not the one pressed). */
  int modifiers;
  /*  Where it was at the button-state-change (in pixels). */
  int x, y;
};
typedef struct Lisp_Button_Data Lisp_Button_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (button_data, Lisp_Button_Data);
#define XBUTTON_DATA(x) XRECORD (x, button_data, Lisp_Button_Data)
#define wrap_button_data(p) wrap_record (p, button_data)
#define BUTTON_DATAP(x) RECORDP (x, button_data)
#define CHECK_BUTTON_DATA(x) CHECK_RECORD (x, button_data)
#define CONCHECK_BUTTON_DATA(x) CONCHECK_RECORD (x, button_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_BUTTON_BUTTON(e) EVENT_FOO_BAR (e, BUTTON, button, button)
#define XEVENT_BUTTON_BUTTON(e) EVENT_BUTTON_BUTTON (XEVENT (e))
#define SET_EVENT_BUTTON_BUTTON(e, val) \
  SET_EVENT_FOO_BAR (e, BUTTON, button, button, val)
#define XSET_EVENT_BUTTON_BUTTON(e, val) \
  SET_EVENT_BUTTON_BUTTON (XEVENT (e), val)

#define EVENT_BUTTON_MODIFIERS(e) EVENT_FOO_BAR (e, BUTTON, button, modifiers)
#define XEVENT_BUTTON_MODIFIERS(e) EVENT_BUTTON_MODIFIERS (XEVENT (e))
#define SET_EVENT_BUTTON_MODIFIERS(e, val) \
  SET_EVENT_FOO_BAR (e, BUTTON, button, modifiers, val)
#define XSET_EVENT_BUTTON_MODIFIERS(e, val) \
  SET_EVENT_BUTTON_MODIFIERS (XEVENT (e), val)

#define EVENT_BUTTON_X(e) EVENT_FOO_BAR (e, BUTTON, button, x)
#define XEVENT_BUTTON_X(e) EVENT_BUTTON_X (XEVENT (e))
#define SET_EVENT_BUTTON_X(e, val) \
  SET_EVENT_FOO_BAR (e, BUTTON, button, x, val)
#define XSET_EVENT_BUTTON_X(e, val) \
  SET_EVENT_BUTTON_X (XEVENT (e), val)

#define EVENT_BUTTON_Y(e) EVENT_FOO_BAR (e, BUTTON, button, y)
#define XEVENT_BUTTON_Y(e) EVENT_BUTTON_Y (XEVENT (e))
#define SET_EVENT_BUTTON_Y(e, val) \
  SET_EVENT_FOO_BAR (e, BUTTON, button, y, val)
#define XSET_EVENT_BUTTON_Y(e, val) \
  SET_EVENT_BUTTON_Y (XEVENT (e), val)

struct Lisp_Motion_Data
{
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  /* Where it was after it moved (in pixels). */
  int x, y;
  /* Bucky-bits down when the motion was detected. */
  int modifiers;
};
typedef struct Lisp_Motion_Data Lisp_Motion_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (motion_data, Lisp_Motion_Data);
#define XMOTION_DATA(x) XRECORD (x, motion_data, Lisp_Motion_Data)
#define wrap_motion_data(p) wrap_record (p, motion_data)
#define MOTION_DATAP(x) RECORDP (x, motion_data)
#define CHECK_MOTION_DATA(x) CHECK_RECORD (x, motion_data)
#define CONCHECK_MOTION_DATA(x) CONCHECK_RECORD (x, motion_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_MOTION_X(e) EVENT_FOO_BAR (e, MOTION, motion, x)
#define XEVENT_MOTION_X(e) EVENT_MOTION_X (XEVENT (e))
#define SET_EVENT_MOTION_X(e, val) \
  SET_EVENT_FOO_BAR (e, MOTION, motion, x, val)
#define XSET_EVENT_MOTION_X(e, val) \
  SET_EVENT_MOTION_X (XEVENT (e), val)

#define EVENT_MOTION_Y(e) EVENT_FOO_BAR (e, MOTION, motion, y)
#define XEVENT_MOTION_Y(e) EVENT_MOTION_Y (XEVENT (e))
#define SET_EVENT_MOTION_Y(e, val) \
  SET_EVENT_FOO_BAR (e, MOTION, motion, y, val)
#define XSET_EVENT_MOTION_Y(e, val) \
  SET_EVENT_MOTION_Y (XEVENT (e), val)

#define EVENT_MOTION_MODIFIERS(e) EVENT_FOO_BAR (e, MOTION, motion, modifiers)
#define XEVENT_MOTION_MODIFIERS(e) EVENT_MOTION_MODIFIERS (XEVENT (e))
#define SET_EVENT_MOTION_MODIFIERS(e, val) \
  SET_EVENT_FOO_BAR (e, MOTION, motion, modifiers, val)
#define XSET_EVENT_MOTION_MODIFIERS(e, val) \
  SET_EVENT_MOTION_MODIFIERS (XEVENT (e), val)

struct Lisp_Process_Data
{
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  /* the XEmacs "process" object in question */
  Lisp_Object process;
};
typedef struct Lisp_Process_Data Lisp_Process_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (process_data, Lisp_Process_Data);
#define XPROCESS_DATA(x) XRECORD (x, process_data, Lisp_Process_Data)
#define wrap_process_data(p) wrap_record (p, process_data)
#define PROCESS_DATAP(x) RECORDP (x, process_data)
#define CHECK_PROCESS_DATA(x) CHECK_RECORD (x, process_data)
#define CONCHECK_PROCESS_DATA(x) CONCHECK_RECORD (x, process_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_PROCESS_PROCESS(e) EVENT_FOO_BAR (e, PROCESS, process, process)
#define XEVENT_PROCESS_PROCESS(e) EVENT_PROCESS_PROCESS (XEVENT (e))
#define SET_EVENT_PROCESS_PROCESS(e, val) \
  SET_EVENT_FOO_BAR (e, PROCESS, process, process, val)
#define XSET_EVENT_PROCESS_PROCESS(e, val) \
  SET_EVENT_PROCESS_PROCESS (XEVENT (e), val)

struct Lisp_Timeout_Data
{
/*
    interval_id		The ID returned when the associated call to
			add_timeout_cb() was made
	------ the rest of the fields are filled in by XEmacs -----
    id_number		The XEmacs timeout ID for this timeout (more
			than one timeout event can have the same value
			here, since XEmacs timeouts, as opposed to
			add_timeout_cb() timeouts, can resignal
			themselves)
    function		An elisp function to call when this timeout is
			processed.
    object		The object passed to that function.
*/
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  int interval_id;
  int id_number;
  Lisp_Object function;
  Lisp_Object object;
};
typedef struct Lisp_Timeout_Data Lisp_Timeout_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (timeout_data, Lisp_Timeout_Data);
#define XTIMEOUT_DATA(x) XRECORD (x, timeout_data, Lisp_Timeout_Data)
#define wrap_timeout_data(p) wrap_record(p, timeout_data)
#define TIMEOUT_DATAP(x) RECORDP (x, timeout_data)
#define CHECK_TIMEOUT_DATA(x) CHECK_RECORD (x, timeout_data)
#define CONCHECK_TIMEOUT_DATA(x) CONCHECK_RECORD (x, timeout_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_TIMEOUT_INTERVAL_ID(e) EVENT_FOO_BAR (e, TIMEOUT, timeout, interval_id)
#define XEVENT_TIMEOUT_INTERVAL_ID(e) EVENT_TIMEOUT_INTERVAL_ID (XEVENT (e))
#define SET_EVENT_TIMEOUT_INTERVAL_ID(e, val) \
  SET_EVENT_FOO_BAR (e, TIMEOUT, timeout, interval_id, val)
#define XSET_EVENT_TIMEOUT_INTERVAL_ID(e, val) \
  SET_EVENT_TIMEOUT_INTERVAL_ID (XEVENT (e), val)

#define EVENT_TIMEOUT_ID_NUMBER(e) EVENT_FOO_BAR (e, TIMEOUT, timeout, id_number)
#define XEVENT_TIMEOUT_ID_NUMBER(e) EVENT_TIMEOUT_ID_NUMBER (XEVENT (e))
#define SET_EVENT_TIMEOUT_ID_NUMBER(e, val) \
  SET_EVENT_FOO_BAR (e, TIMEOUT, timeout, id_number, val)
#define XSET_EVENT_TIMEOUT_ID_NUMBER(e, val) \
  SET_EVENT_TIMEOUT_ID_NUMBER (XEVENT (e), val)

#define EVENT_TIMEOUT_FUNCTION(e) EVENT_FOO_BAR (e, TIMEOUT, timeout, function)
#define XEVENT_TIMEOUT_FUNCTION(e) EVENT_TIMEOUT_FUNCTION (XEVENT (e))
#define SET_EVENT_TIMEOUT_FUNCTION(e, val) \
  SET_EVENT_FOO_BAR (e, TIMEOUT, timeout, function, val)
#define XSET_EVENT_TIMEOUT_FUNCTION(e, val) \
  SET_EVENT_TIMEOUT_FUNCTION (XEVENT (e), val)

#define EVENT_TIMEOUT_OBJECT(e) EVENT_FOO_BAR (e, TIMEOUT, timeout, object)
#define XEVENT_TIMEOUT_OBJECT(e) EVENT_TIMEOUT_OBJECT (XEVENT (e))
#define SET_EVENT_TIMEOUT_OBJECT(e, val) \
  SET_EVENT_FOO_BAR (e, TIMEOUT, timeout, object, val)
#define XSET_EVENT_TIMEOUT_OBJECT(e, val) \
  SET_EVENT_TIMEOUT_OBJECT (XEVENT (e), val)

struct Lisp_Eval_Data
{
/* This kind of event is used internally; sometimes the window system
   interface would like to inform XEmacs of some user action (such as
   focusing on another frame) but needs that to happen synchronously
   with the other user input, like keypresses.  This is useful when
   events are reported through callbacks rather than in the standard
   event stream.

    function		An elisp function to call with this event object.
    object		Argument of function.
*/
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  Lisp_Object function;
  Lisp_Object object;
};
typedef struct Lisp_Eval_Data Lisp_Eval_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (eval_data, Lisp_Eval_Data);
#define XEVAL_DATA(x) XRECORD (x, eval_data, Lisp_Eval_Data)
#define wrap_eval_data(p) wrap_record(p, eval_data)
#define EVAL_DATAP(x) RECORDP (x, eval_data)
#define CHECK_EVAL_DATA(x) CHECK_RECORD (x, eval_data)
#define CONCHECK_EVAL_DATA(x) CONCHECK_RECORD (x, eval_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_EVAL_FUNCTION(e) EVENT_FOO_BAR (e, EVAL, eval, function)
#define XEVENT_EVAL_FUNCTION(e) EVENT_EVAL_FUNCTION (XEVENT (e))
#define SET_EVENT_EVAL_FUNCTION(e, val) \
  SET_EVENT_FOO_BAR (e, EVAL, eval, function, val)
#define XSET_EVENT_EVAL_FUNCTION(e, val) \
  SET_EVENT_EVAL_FUNCTION (XEVENT (e), val)

#define EVENT_EVAL_OBJECT(e) EVENT_FOO_BAR (e, EVAL, eval, object)
#define XEVENT_EVAL_OBJECT(e) EVENT_EVAL_OBJECT (XEVENT (e))
#define SET_EVENT_EVAL_OBJECT(e, val) \
  SET_EVENT_FOO_BAR (e, EVAL, eval, object, val)
#define XSET_EVENT_EVAL_OBJECT(e, val) \
  SET_EVENT_EVAL_OBJECT (XEVENT (e), val)

struct Lisp_Misc_User_Data
{
/* #### The misc-user type is serious junk.  It should be separated
   out into different events.  There's no reason to create
   sub-subtypes of events.

    function		An elisp function to call with this event object.
    object		Argument of function.
    button		What button went down or up.
    modifiers		Bucky-bits on that button: shift, control, meta, etc.
    x, y		Where it was at the button-state-change (in pixels).
			This is similar to an eval_event, except that it is
			generated by user actions: selections in the
			menubar, scrollbar actions, or drag and drop actions.
			It is a "command" event, like key and mouse presses
			(and unlike mouse motion, process output, and enter
			and leave window hooks).  In many ways, eval_events
			are not the same as keypresses or misc_user_events.
			The button, modifiers, x, and y parts are only used
			by the XEmacs Drag'n'Drop system. Don't depend on their
			values for other types of misc_user_events.
*/
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  Lisp_Object function;
  Lisp_Object object;
  int button;
  int modifiers;
  int x, y;
};
typedef struct Lisp_Misc_User_Data Lisp_Misc_User_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (misc_user_data, Lisp_Misc_User_Data);
#define XMISC_USER_DATA(x) XRECORD (x, misc_user_data, Lisp_Misc_User_Data)
#define wrap_misc_user_data(p) wrap_record(p, misc_user_data)
#define MISC_USER_DATAP(x) RECORDP (x, misc_user_data)
#define CHECK_MISC_USER_DATA(x) CHECK_RECORD (x, misc_user_data)
#define CONCHECK_MISC_USER_DATA(x) CONCHECK_RECORD (x, misc_user_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_MISC_USER_FUNCTION(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, function)
#define XEVENT_MISC_USER_FUNCTION(e) EVENT_MISC_USER_FUNCTION (XEVENT (e))
#define SET_EVENT_MISC_USER_FUNCTION(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, function, val)
#define XSET_EVENT_MISC_USER_FUNCTION(e, val) \
  SET_EVENT_MISC_USER_FUNCTION (XEVENT (e), val)

#define EVENT_MISC_USER_OBJECT(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, object)
#define XEVENT_MISC_USER_OBJECT(e) EVENT_MISC_USER_OBJECT (XEVENT (e))
#define SET_EVENT_MISC_USER_OBJECT(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, object, val)
#define XSET_EVENT_MISC_USER_OBJECT(e, val) \
  SET_EVENT_MISC_USER_OBJECT (XEVENT (e), val)

#define EVENT_MISC_USER_BUTTON(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, button)
#define XEVENT_MISC_USER_BUTTON(e) EVENT_MISC_USER_BUTTON (XEVENT (e))
#define SET_EVENT_MISC_USER_BUTTON(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, button, val)
#define XSET_EVENT_MISC_USER_BUTTON(e, val) \
  SET_EVENT_MISC_USER_BUTTON (XEVENT (e), val)

#define EVENT_MISC_USER_MODIFIERS(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, modifiers)
#define XEVENT_MISC_USER_MODIFIERS(e) EVENT_MISC_USER_MODIFIERS (XEVENT (e))
#define SET_EVENT_MISC_USER_MODIFIERS(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, modifiers, val)
#define XSET_EVENT_MISC_USER_MODIFIERS(e, val) \
  SET_EVENT_MISC_USER_MODIFIERS (XEVENT (e), val)

#define EVENT_MISC_USER_X(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, x)
#define XEVENT_MISC_USER_X(e) EVENT_MISC_USER_X (XEVENT (e))
#define SET_EVENT_MISC_USER_X(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, x, val)
#define XSET_EVENT_MISC_USER_X(e, val) \
  SET_EVENT_MISC_USER_X (XEVENT (e), val)

#define EVENT_MISC_USER_Y(e) EVENT_FOO_BAR (e, MISC_USER, misc_user, y)
#define XEVENT_MISC_USER_Y(e) EVENT_MISC_USER_Y (XEVENT (e))
#define SET_EVENT_MISC_USER_Y(e, val) \
  SET_EVENT_FOO_BAR (e, MISC_USER, misc_user, y, val)
#define XSET_EVENT_MISC_USER_Y(e, val) \
  SET_EVENT_MISC_USER_Y (XEVENT (e), val)

struct Lisp_Magic_Eval_Data
{
/* This is like an eval event but its contents are not
   Lisp-accessible.  This allows for "internal eval events" that call
   non-Lisp-accessible functions.  Externally, a magic_eval_event just
   appears as a magic_event; the Lisp programmer need not know
   anything more.

    internal_function	An unexported function to call with this event
			object.  This allows eval events to call internal
			functions.  For a normal eval event, this field
			will always be 0.
    object		Argument of function.

*/
#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */
  void (*internal_function) (Lisp_Object);
  Lisp_Object object;
};
typedef struct Lisp_Magic_Eval_Data Lisp_Magic_Eval_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (magic_eval_data, Lisp_Magic_Eval_Data);
#define XMAGIC_EVAL_DATA(x) XRECORD (x, magic_eval_data, Lisp_Magic_Eval_Data)
#define wrap_magic_eval_data(p) wrap_record(p, magic_eval_data)
#define MAGIC_EVAL_DATAP(x) RECORDP (x, magic_eval_data)
#define CHECK_MAGIC_EVAL_DATA(x) CHECK_RECORD (x, magic_eval_data)
#define CONCHECK_MAGIC_EVAL_DATA(x) CONCHECK_RECORD (x, magic_eval_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_MAGIC_EVAL_INTERNAL_FUNCTION(e) EVENT_FOO_BAR (e, MAGIC_EVAL, magic_eval, internal_function)
#define XEVENT_MAGIC_EVAL_INTERNAL_FUNCTION(e) EVENT_MAGIC_EVAL_INTERNAL_FUNCTION (XEVENT (e))
#define SET_EVENT_MAGIC_EVAL_INTERNAL_FUNCTION(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC_EVAL, magic_eval, internal_function, val)
#define XSET_EVENT_MAGIC_EVAL_INTERNAL_FUNCTION(e, val) \
  SET_EVENT_MAGIC_EVAL_INTERNAL_FUNCTION (XEVENT (e), val)

#define EVENT_MAGIC_EVAL_OBJECT(e) EVENT_FOO_BAR (e, MAGIC_EVAL, magic_eval, object)
#define XEVENT_MAGIC_EVAL_OBJECT(e) EVENT_MAGIC_EVAL_OBJECT (XEVENT (e))
#define SET_EVENT_MAGIC_EVAL_OBJECT(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC_EVAL, magic_eval, object, val)
#define XSET_EVENT_MAGIC_EVAL_OBJECT(e, val) \
  SET_EVENT_MAGIC_EVAL_OBJECT (XEVENT (e), val)

#if defined (HAVE_X_WINDOWS) && defined(emacs)
# include <X11/Xlib.h>
#endif

#ifdef HAVE_GTK
#include <gdk/gdk.h>
#endif


struct Lisp_Magic_Data
{
/* No user-serviceable parts within.  This is for things like
   KeymapNotify and ExposeRegion events and so on that XEmacs itself
   doesn't care about, but which it must do something with for proper
   interaction with the window system.

   Magic_events are handled somewhat asynchronously, just like
   subprocess filters.  However, occasionally a magic_event needs to
   be handled synchronously; in that case, the asynchronous handling
   of the magic_event will push an eval_event back onto the queue,
   which will be handled synchronously later.  This is one of the
   reasons why eval_events exist; I'm not entirely happy with this
   aspect of this event model.
*/

#ifdef EVENT_DATA_AS_OBJECTS
  struct lrecord_header lheader;
#endif /* EVENT_DATA_AS_OBJECTS */

  union {
#ifdef HAVE_GTK
    GdkEvent          gdk_event;
#endif
#ifdef HAVE_X_WINDOWS
    XEvent            x_event;
#endif
#ifdef HAVE_MS_WINDOWS
    int               mswindows_event;
#endif
  } underlying;
};

typedef struct Lisp_Magic_Data Lisp_Magic_Data;

#ifdef EVENT_DATA_AS_OBJECTS
DECLARE_LRECORD (magic_data, Lisp_Magic_Data);
#define XMAGIC_DATA(x) XRECORD (x, magic_data, Lisp_Magic_Data)
#define wrap_magic_data(p) wrap_record(p, magic_data)
#define MAGIC_DATAP(x) RECORDP (x, magic_data)
#define CHECK_MAGIC_DATA(x) CHECK_RECORD (x, magic_data)
#define CONCHECK_MAGIC_DATA(x) CONCHECK_RECORD (x, magic_data)
#endif /* EVENT_DATA_AS_OBJECTS */

#define EVENT_MAGIC_UNDERLYING(e) EVENT_FOO_BAR (e, MAGIC, magic, underlying)
#define XEVENT_MAGIC_UNDERLYING(e) EVENT_MAGIC_UNDERLYING (XEVENT (e))
#define SET_EVENT_MAGIC_UNDERLYING(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC, magic, underlying, val)
#define XSET_EVENT_MAGIC_UNDERLYING(e, val) \
  SET_EVENT_MAGIC_UNDERLYING (XEVENT (e), val)

#ifdef HAVE_GTK
#define EVENT_MAGIC_GDK_EVENT(e) EVENT_FOO_BAR (e, MAGIC, magic, underlying.gdk_event)
#define XEVENT_MAGIC_GDK_EVENT(e) EVENT_MAGIC_GDK_EVENT (XEVENT (e))
#define SET_EVENT_MAGIC_GDK_EVENT(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC, magic, underlying.gdk_event, val)
#define XSET_EVENT_MAGIC_GDK_EVENT(e, val) \
  SET_EVENT_MAGIC_GDK_EVENT (XEVENT (e), val)
#endif

#ifdef HAVE_X_WINDOWS
#define EVENT_MAGIC_X_EVENT(e) EVENT_FOO_BAR (e, MAGIC, magic, underlying.x_event)
#define XEVENT_MAGIC_X_EVENT(e) EVENT_MAGIC_X_EVENT (XEVENT (e))
#define SET_EVENT_MAGIC_X_EVENT(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC, magic, underlying.x_event, val)
#define XSET_EVENT_MAGIC_X_EVENT(e, val) \
  SET_EVENT_MAGIC_X_EVENT (XEVENT (e), val)
#endif

#ifdef HAVE_MS_WINDOWS
#define EVENT_MAGIC_MSWINDOWS_EVENT(e) EVENT_FOO_BAR (e, MAGIC, magic, underlying.mswindows_event)
#define XEVENT_MAGIC_MSWINDOWS_EVENT(e) EVENT_MAGIC_MSWINDOWS_EVENT (XEVENT (e))
#define SET_EVENT_MAGIC_MSWINDOWS_EVENT(e, val) \
  SET_EVENT_FOO_BAR (e, MAGIC, magic, underlying.mswindows_event, val)
#define XSET_EVENT_MAGIC_MSWINDOWS_EVENT(e, val) \
  SET_EVENT_MAGIC_MSWINDOWS_EVENT (XEVENT (e), val)
#endif

struct Lisp_Timeout
{
  struct LCRECORD_HEADER header;
  int id; /* Id we use to identify the timeout over its lifetime */
  int interval_id; /* Id for this particular interval; this may
                      be different each time the timeout is
                      signalled.*/
  Lisp_Object function, object; /* Function and object associated
                                   with timeout. */
  EMACS_TIME next_signal_time;  /* Absolute time when the timeout
                                   is next going to be signalled. */
  unsigned int resignal_msecs;  /* How far after the next timeout
                                   should the one after that
                                   occur? */
};
typedef struct Lisp_Timeout Lisp_Timeout;

DECLARE_LRECORD (timeout, Lisp_Timeout);
#define XTIMEOUT(x) XRECORD (x, timeout, Lisp_Timeout)
#define wrap_timeout(p) wrap_record (p, timeout)
#define TIMEOUTP(x) RECORDP (x, timeout)
#define CHECK_TIMEOUT(x) CHECK_RECORD (x, timeout)
#define CONCHECK_TIMEOUT(x) CONCHECK_RECORD (x, timeout)

struct Lisp_Event
{
  /* header->next (aka XEVENT_NEXT ()) is used as follows:
     - For dead events, this is the next dead one.
     - For events on the command_event_queue, the next one on the queue.
     - Likewise for events chained in the command builder.
     - Otherwise it's Qnil.
   */
  struct lrecord_header lheader;
  Lisp_Object           next;
  emacs_event_type      event_type;

  /* Where this event occurred on.  This will be a frame, device,
     console, or nil, depending on the event type.  It is important
     that an object of a more specific type than is actually generated
     is not substituted -- e.g. there should not be a frame inserted
     when a key-press event occurs, because events on dead channels
     are automatically ignored.

     Specifically:

     -- for button and mouse-motion events, channel will be a
     frame. (The translation to a window occurs later.)

     -- for keyboard events, channel will be a console.  Note that
     fake keyboard events (generated by `character-to-event' or
     something that calls this, such as macros) need to have the
     selected console stored into them when the event is created.
     This is so that the correct console-local variables (e.g. the
     command builder) will get affected.

     -- for timer, process, magic-eval, and eval events, channel will
     be nil.

     -- for misc-user events, channel will be a frame.

     -- for magic events, channel will be a frame (usually) or a
     device. */
  Lisp_Object           channel;

  /* When this event occurred -- if not known, this is made up. ####
     All timestamps should be measured as milliseconds since XEmacs
     started.  Currently they are raw server timestamps. (The X
     protocol doesn't provide any easy way of translating between
     server time and real process time; yuck.) */
  unsigned int          timestamp;

#ifdef EVENT_DATA_AS_OBJECTS
  Lisp_Object event_data;
#else /* not EVENT_DATA_AS_OBJECTS */
  union
    {
      Lisp_Key_Data           key;
      Lisp_Button_Data        button;
      Lisp_Motion_Data        motion;
      Lisp_Process_Data       process;
      Lisp_Timeout_Data       timeout;
      Lisp_Eval_Data          eval;   /* misc_user_event no longer uses this */
      Lisp_Misc_User_Data     misc_user;/* because it needs position information */
      Lisp_Magic_Data         magic;
      Lisp_Magic_Eval_Data    magic_eval;
    } event;
#endif /* not EVENT_DATA_AS_OBJECTS */
};

DECLARE_LRECORD (event, Lisp_Event);
#define XEVENT(x) XRECORD (x, event, Lisp_Event)
#define wrap_event(p) wrap_record (p, event)
#define EVENTP(x) RECORDP (x, event)
#define CHECK_EVENT(x) CHECK_RECORD (x, event)
#define CONCHECK_EVENT(x) CONCHECK_RECORD (x, event)

DECLARE_LRECORD (command_builder, struct command_builder);

#define EVENT_CHANNEL(a) ((a)->channel)
#define XEVENT_CHANNEL(ev) (XEVENT (ev)->channel)
#define EVENT_TYPE(a) ((a)->event_type)
#define XEVENT_TYPE(a) (XEVENT (a)->event_type)
#define EVENT_NEXT(a) ((a)->next)
#define XEVENT_NEXT(e) (XEVENT (e)->next)
#define EVENT_TIMESTAMP(ev) ((ev)->timestamp)
#define XEVENT_TIMESTAMP(ev) EVENT_TIMESTAMP (XEVENT (ev))

#ifdef EVENT_DATA_AS_OBJECTS
#define XEVENT_DATA(ev) (XEVENT (ev)->event_data)
#define EVENT_DATA(ev) ((ev)->event_data)
#define SET_EVENT_DATA(ev, d)                           \
do {                                                    \
  Lisp_Event *mac_event = (ev);                         \
  mac_event->event_data = (d);                          \
} while (0)
#define XSET_EVENT_DATA(ev, d) SET_EVENT_DATA (XEVENT (ev), d)
#endif /* EVENT_DATA_AS_OBJECTS */

#define SET_EVENT_TIMESTAMP_ZERO(ev) ((ev)->timestamp = 0)
#define SET_EVENT_TIMESTAMP(ev, t) ((ev)->timestamp = (t))
#define XSET_EVENT_TIMESTAMP(ev, t) SET_EVENT_TIMESTAMP (XEVENT (ev), t)

#define SET_EVENT_CHANNEL(ev, c)                        \
do {                                                    \
  Lisp_Event *mac_event = (ev);                         \
  mac_event->channel = (c);                             \
} while (0)
#define XSET_EVENT_CHANNEL(ev, c) SET_EVENT_CHANNEL (XEVENT (ev), c) 

DECLARE_INLINE_HEADER (
void
set_event_type (struct Lisp_Event *event, emacs_event_type t) 
)
{
#ifdef EVENT_DATA_AS_OBJECTS
  switch (EVENT_TYPE (event))
    {
    case key_press_event:
      free_key_data (event->event_data);
      break;
    case button_press_event:
    case button_release_event:
      free_button_data (event->event_data);
      break;
    case pointer_motion_event:
      free_motion_data (event->event_data);
      break;
    case process_event:
      free_process_data (event->event_data);
      break;
    case timeout_event:
      free_timeout_data (event->event_data);
      break;
    case magic_event:
      free_magic_data (event->event_data);
      break;
    case magic_eval_event:
      free_magic_eval_data (event->event_data);
      break;
    case eval_event:
      free_eval_data (event->event_data);
      break;
    case misc_user_event:
      free_misc_user_data (event->event_data);
      break;
    default:
      break;
    }
#endif /* EVENT_DATA_AS_OBJECTS */

  event->event_type = t;

#ifdef EVENT_DATA_AS_OBJECTS
  switch (t)
    {
    case key_press_event:
      event->event_data = make_key_data ();
      break;
    case button_press_event:
    case button_release_event:
      event->event_data = make_button_data ();
      break;
    case pointer_motion_event:
      event->event_data = make_motion_data ();
      break;
    case process_event:
      event->event_data = make_process_data ();
      break;
    case timeout_event:
      event->event_data = make_timeout_data ();
      break;
    case magic_event:
      event->event_data = make_magic_data ();
      break;
    case magic_eval_event:
      event->event_data = make_magic_eval_data ();
      break;
    case eval_event:
      event->event_data = make_eval_data ();
      break;
    case misc_user_event:
      event->event_data = make_misc_user_data ();
      break;
    default:
      event->event_data = Qnil;
      break;
    }
#endif /* EVENT_DATA_AS_OBJECTS */
}

#define SET_EVENT_NEXT(ev, n) do { (ev)->next = (n); } while (0)
#define XSET_EVENT_NEXT(ev, n) SET_EVENT_NEXT (XEVENT (ev), n)

#define XSET_EVENT_TYPE(ev, t) set_event_type (XEVENT (ev), t)
#define SET_EVENT_TYPE(ev, t) set_event_type (ev, t)

#define EVENT_CHAIN_LOOP(event, chain) \
  for (event = chain; !NILP (event); event = XEVENT_NEXT (event))

#define EVENT_LIVE_P(a) (EVENT_TYPE (a) != dead_event)

#define CHECK_LIVE_EVENT(x) do {                        \
  CHECK_EVENT (x);                                      \
  if (! EVENT_LIVE_P (XEVENT (x)))                      \
    dead_wrong_type_argument (Qevent_live_p, (x));      \
} while (0)
#define CONCHECK_LIVE_EVENT(x) do {                     \
  CONCHECK_EVENT (x);                                   \
  if (! EVENT_LIVE_P (XEVENT (x)))                      \
    x = wrong_type_argument (Qevent_live_p, (x));       \
} while (0)


EXFUN (Fcharacter_to_event, 4);
EXFUN (Fdeallocate_event, 1);
EXFUN (Fevent_glyph_extent, 1);
EXFUN (Fevent_modeline_position, 1);
EXFUN (Fevent_over_modeline_p, 1);
EXFUN (Fevent_over_toolbar_p, 1);
EXFUN (Fevent_over_vertical_divider_p, 1);
EXFUN (Fevent_point, 1);
EXFUN (Fevent_window, 1);
EXFUN (Fmake_event, 2);
EXFUN (Fnext_command_event, 2);

extern Lisp_Object QKbackspace, QKdelete, QKescape, QKlinefeed, QKreturn;
extern Lisp_Object QKspace, QKtab, Qmouse_event_p, Vcharacter_set_property;
extern Lisp_Object Qcancel_mode_internal;
extern Lisp_Object Vmodifier_keys_sticky_time;

/* The modifiers XEmacs knows about; these appear in key and button events. */

enum event_modifiers
  {
    XEMACS_MOD_CONTROL  = (1<<0),
    XEMACS_MOD_META     = (1<<1),
    XEMACS_MOD_SUPER    = (1<<2),
    XEMACS_MOD_HYPER    = (1<<3),
    XEMACS_MOD_ALT      = (1<<4),
    XEMACS_MOD_SHIFT    = (1<<5)  /* not used for dual-case characters */,
#define FROB(num)				\
    XEMACS_MOD_BUTTON##num  = (1<<(num+5)),
#include "keymap-buttons.h"
  };
   
/* Note: under X Windows, XEMACS_MOD_ALT is generated by the Alt key
   if there are both Alt and Meta keys.  If there are no Meta keys,
   then Alt generates XEMACS_MOD_META instead.
 */

/* Maybe this should be trickier */
#define KEYSYM(x) (intern (x))

typedef enum character_to_event_meta_behavior
{
  high_bit_is_meta,
  use_console_meta_flag,
  latin_1_maps_to_itself
} character_to_event_meta_behavior;

/* from events.c */
void format_event_object (Eistring *buf, Lisp_Object event, int brief);
/*void format_event_data_object (Eistring *buf, Lisp_Object data, int brief);*/
void character_to_event (Ichar, Lisp_Event *, struct console *,
                         character_to_event_meta_behavior meta_flag,
                         int do_backspace_mapping);

void zero_event (Lisp_Event *e);

#define MECR_DEALLOCATE_EVENT 1

void deallocate_event_chain (Lisp_Object event);
Lisp_Object event_chain_tail (Lisp_Object event);
void enqueue_event (Lisp_Object event, Lisp_Object *head, Lisp_Object *tail);
Lisp_Object dequeue_event (Lisp_Object *head, Lisp_Object *tail);
void enqueue_event_chain (Lisp_Object event_chain, Lisp_Object *head,
                          Lisp_Object *tail);
int map_event_chain_remove (int (*fn) (Lisp_Object ev, void *user_data),
			    Lisp_Object *head, Lisp_Object *tail,
			    void *user_data, int flags);
int event_chain_count (Lisp_Object event_chain);
Lisp_Object event_chain_find_previous (Lisp_Object event_chain,
                                       Lisp_Object event);
Lisp_Object event_chain_nth (Lisp_Object event_chain, int n);
Lisp_Object copy_event_chain (Lisp_Object event_chain);
Lisp_Object transfer_event_chain_pointer (Lisp_Object pointer,
					  Lisp_Object old_chain,
					  Lisp_Object new_chain);


void nth_of_key_sequence_as_event (Lisp_Object seq, int n, Lisp_Object event);
Lisp_Object key_sequence_to_event_chain (Lisp_Object seq);
/* True if this is a non-internal event
   (keyboard press, menu, scrollbar, mouse button) */
int command_event_p (Lisp_Object event);
void define_self_inserting_symbol (Lisp_Object, Lisp_Object);
Ichar event_to_character (Lisp_Object, int, int);
struct console *event_console_or_selected (Lisp_Object event);
void upshift_event (Lisp_Object event);
void downshift_event (Lisp_Object event);
int event_upshifted_p (Lisp_Object event);

/* from event-stream.c */
extern Lisp_Object dispatch_event_queue;
Lisp_Object allocate_command_builder (Lisp_Object console, int with_echo_buf);
void enqueue_dispatch_event (Lisp_Object event);
Lisp_Object dequeue_dispatch_event (void);
void enqueue_magic_eval_event (void (*fun) (Lisp_Object), Lisp_Object object);
void event_stream_handle_magic_event (Lisp_Event *event);
void event_stream_format_magic_event (Lisp_Event *event, Lisp_Object pstream);
int event_stream_compare_magic_event (Lisp_Event *e1, Lisp_Event *e2);
Hashcode event_stream_hash_magic_event (Lisp_Event *e);
void event_stream_select_console   (struct console *con);
void event_stream_unselect_console (struct console *con);
void event_stream_select_process   (Lisp_Process *proc, int doin, int doerr);
void event_stream_unselect_process (Lisp_Process *proc, int doin, int doerr);
void event_stream_create_io_streams (void* inhandle, void* outhandle,
				     void *errhandle, Lisp_Object* instream,
				     Lisp_Object* outstream,
				     Lisp_Object* errstream,
				     USID* in_usid,
				     USID* err_usid,
				     int flags);
void event_stream_delete_io_streams (Lisp_Object instream,
				     Lisp_Object outstream,
				     Lisp_Object errstream,
				     USID* in_usid,
				     USID* err_usid);
Lisp_Object event_stream_protect_modal_loop (const char *error_string,
					     Lisp_Object (*bfun) (void *barg),
					     void *barg, int flags);
void event_stream_drain_queue (void);
void event_stream_quit_p (void);
void run_pre_idle_hook (void);

struct low_level_timeout
{
  int id;
  EMACS_TIME time;
  struct low_level_timeout *next;
};

int add_low_level_timeout (struct low_level_timeout **timeout_list,
                           EMACS_TIME thyme);
void remove_low_level_timeout (struct low_level_timeout **timeout_list,
                               int id);
int get_low_level_timeout_interval (struct low_level_timeout *
                                    timeout_list, EMACS_TIME *interval);
int pop_low_level_timeout (struct low_level_timeout **timeout_list,
                           EMACS_TIME *time_out);
int event_stream_generate_wakeup (unsigned int milliseconds,
                                  unsigned int vanilliseconds,
                                  Lisp_Object function,
                                  Lisp_Object object,
                                  int async_p);
int event_stream_resignal_wakeup (int interval_id, int async_p,
                                  Lisp_Object *function, Lisp_Object *object);
void event_stream_disable_wakeup (int id, int async_p);

/* from signal.c */
int signal_add_async_interval_timeout (EMACS_TIME thyme);
void signal_remove_async_interval_timeout (int id);

/* from event-stream.c -- focus sanity */
extern int focus_follows_mouse;
void investigate_frame_change (void);

void emacs_handle_focus_change_preliminary (Lisp_Object frame_inp_and_dev);
void emacs_handle_focus_change_final (Lisp_Object frame_inp_and_dev);

Lisp_Object extract_this_command_keys_nth_mouse_event (int n);
Lisp_Object extract_vector_nth_mouse_event (Lisp_Object vector, int n);

void single_console_state (void);
void any_console_state (void);
int in_single_console_state (void);

extern int emacs_is_blocking;
extern int in_modal_loop;
extern volatile int sigint_happened;

#ifdef HAVE_UNIXOID_EVENT_LOOP
/* from event-unixoid.c */

/* Ceci n'est pas un pipe. */
extern int signal_event_pipe[];

void signal_fake_event (void);
void drain_signal_event_pipe (void);
void drain_tty_devices (void);

extern int fake_event_occurred;

int event_stream_unixoid_select_console   (struct console *con);
int event_stream_unixoid_unselect_console (struct console *con);
void event_stream_unixoid_select_process (Lisp_Process *proc, int doin,
					  int doerr, int *infd, int *errfd);
void event_stream_unixoid_unselect_process (Lisp_Process *proc, int doin,
					    int doerr, int *infd, int *errfd);
struct console *find_tty_or_stream_console_from_fd (int fd);
int read_event_from_tty_or_stream_desc (Lisp_Event *event,
					struct console *con);
void event_stream_unixoid_create_io_streams (void* inhandle, void* outhandle,
					     void *errhandle,
					     Lisp_Object* instream,
					     Lisp_Object* outstream,
					     Lisp_Object* errstream,
					     USID* in_usid,
					     USID* err_usid,
					     int flags);
void event_stream_unixoid_delete_io_streams (Lisp_Object instream,
					     Lisp_Object outstream,
					     Lisp_Object errstream,
					     USID* in_usid,
					     USID* err_usid);

#endif /* HAVE_UNIXOID_EVENT_LOOP */

/* The following is not inside of HAVE_UNIXOID_EVENT_LOOP because of the
   possibility of combiling XEmacs with no-MSW, no-X, no-TTY --
   process-unix.c is still compiled.  #### Should we still compile
   subprocesses with no event loops? */
/* Beware: this evil macro evaluates its arg many times */
#define FD_TO_USID(fd) ((fd)==0 ? (USID)999999 : ((fd)<0 ? USID_DONTHASH : (USID)(fd)))

/* Define this if you want the tty event stream to be used when the
   first console is tty, even if HAVE_X_WINDOWS is defined */
/* #define DEBUG_TTY_EVENT_STREAM */


/* #### a hack, until accelerator shit is cleaned up */

/* This structure is what we use to encapsulate the state of a command sequence
   being composed; key events are executed by adding themselves to the command
   builder; if the command builder is then complete (does not still represent
   a prefix key sequence) it executes the corresponding command.
 */
struct command_builder
{
  struct LCRECORD_HEADER header;
  Lisp_Object console; /* back pointer to the console this command
                          builder is for */
#if 0
  /* #### Not implemented: nil, or an event representing the first
     event read after the last command completed.  Threaded. */
  Lisp_Object prefix_events;
#endif /* 0 */
  /* nil, or an event chain representing the events in the current
     keymap-lookup sequence.  NOTE: All events in the chain MUST be
     freshly allocated, with no pointers to them elsewhere. */
  Lisp_Object current_events;
  /* Last elt of current_events */
  Lisp_Object most_current_event;
  /* Last elt before function map code took over.  What this means is:
     All prefixes up to (but not including) this event have non-nil
     bindings, but the prefix including this event has a nil binding.
     Any events in the chain after this one were read solely because
     we're part of a possible function key.  If we end up with
     something that's not part of a possible function key, we have to
     unread all of those events. */
  Lisp_Object last_non_munged_event;
  /* One value for function-key-map, one for key-translation-map:
     First event that can begin a possible function key sequence
     (to be translated according to function-key-map).  Normally
     this is the first event in the chain.  However, once we've
     translated a sequence through function-key-map, this will point
     to the first event after the translated sequence: we don't ever
     want to translate any events twice through function-key-map, or
     things could get really screwed up (e.g. if the user created a
     translation loop).  If this is nil, then the next-read event is
     the first that can begin a function key sequence. */
  Lisp_Object first_mungeable_event[2];
  Ibyte *echo_buf;

  Bytecount echo_buf_length;          /* size of echo_buf */
  Bytecount echo_buf_index;           /* index into echo_buf
                                       * -1 before doing echoing for new cmd */
  /* Self-insert-command is magic in that it doesn't always push an undo-
     boundary: up to 20 consecutive self-inserts can happen before an undo-
     boundary is pushed.  This variable is that counter.
     */
  int self_insert_countdown;
};

#endif /* INCLUDED_events_h_ */
