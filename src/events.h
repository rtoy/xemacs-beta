/* Definitions for the new event model;
   created 16-jul-91 by Jamie Zawinski
   Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

#ifndef _XEMACS_EVENTS_H_
#define _XEMACS_EVENTS_H_

#include "systime.h"

/* There is one object called an event_stream.  This object contains
   callback functions for doing the window-system-dependent operations
   that XEmacs requires.

   If XEmacs is compiled with support for X11 and the X Toolkit, then this
   event_stream structure will contain functions that can cope with input
   on XEmacs windows on multiple displays, as well as input from dumb tty
   frames.

   If it is desired to have XEmacs able to open frames on the displays of
   multiple heterogeneous machines, X11 and SunView, or X11 and NeXT, for
   example, then it will be necessary to construct an event_stream structure
   that can cope with the given types.  Currently, the only implemented
   event_streams are for dumb-ttys, and for X11 plus dumb-ttys.
   
   To implement this for one window system is relatively simple.  
   To implement this for multiple window systems is trickier and may
   not be possible in all situations, but it's been done for X and TTY.

   Note that these callbacks are *NOT* console methods; that's because
   the routines are not specific to a particular console type but must
   be able to simultaneously cope with all allowable console types.

  The slots of the event_stream structure:

 next_event_cb		A function which fills in an XEmacs_event structure
			with the next event available.  If there is no event
			available, then this should block.

			IMPORTANT: timer events and especially process
			events *must not* be returned if there are
			events of other types available; otherwise you
			can end up with an infinite loop in Fdiscard_input().

 event_pending_cb	A function which says whether there are events to be
			read.  If called with an argument of 0, then this
			should say whether calling the next_event_cb will
			block.  If called with an argument of 1, then this
			should say whether there are user-generated events
			pending (that is, keypresses or mouse-clicks).  This
			is used for redisplay optimization, among other 
			things.  On dumb ttys, these two results are the 
			same, but under a window system, they are not.

			If this function is not sure whether there are events
			to be read, it *must* return 0.  Otherwise various
			undesirable effects will occur, such as redisplay
			not occurring until the next event occurs.

 handle_magic_event_cb	XEmacs calls this with an event structure which
  			contains window-system dependent information that
			XEmacs doesn't need to know about, but which must
			happen in order.  If the next_event_cb never returns
			an event of type "magic", this will never be used.

 add_timeout_cb		Called with an EMACS_TIME, the absolute time at
			which a wakeup event should be generated; and a
			void *, which is an arbitrary value that will be
			returned in the timeout event.  The timeouts
			generated by this function should be one-shots:
			they fire once and then disappear.  This callback
			should return an int id-number which uniquely
			identifies this wakeup.  If an implementation
			doesn't have microseconds or millisecond
			granularity, it should round up to the closest
			value it can deal with.

 remove_timeout_cb	Called with an int, the id number of a wakeup to 
 			discard.  This id number must have been returned by
			the add_timeout_cb.  If the given wakeup has
			already expired, this should do nothing.

 select_process_cb	These callbacks tell the underlying implementation to
 unselect_process_cb	add or remove a file descriptor from the list of fds
  			which are polled for inferior-process input.  When
			input becomes available on the given process
			connection, an event of type "process" should be
			generated.

 select_console_cb	These callbacks tell the underlying implementation 
 unselect_console_cb	to add or remove a console from the list of consoles
                        which are polled for user-input.

 select_device_cb	These callbacks are used by Unixoid event loops
 unselect_device_cb	(those that use select() and file descriptors and
			have a separate input fd per device).

 quitp_cb		A handler function called from the `QUIT' macro which
			should check whether the quit character has been
			typed.  On systems with SIGIO, this will not be called
			unless the `sigio_happened' flag is true (it is set
			from the SIGIO handler).

 XEmacs has its own event structures, which are distinct from the event
 structures used by X or any other window system.  It is the job of the
 event_stream layer to translate to this format.

 NOTE: #### All timestamps should be measured as milliseconds since XEmacs
       started.  Currently many or most events have a 0 as their
       timestamp value, and for other timestamps, they are raw server
       timestamps. (The X protocol doesn't provide any easy way of
       translating between server time and real process time; yuck.)

 Every event type has the following structures:

 channel		Where this event occurred on.  This will be
			a frame, device, console, or nil, depending on the
			event type.  It is important that an object of
			a more specific type than is actually generated
			is not substituted -- e.g. there should not be
			a frame inserted when a key-press event occurs,
			because events on dead channels are automatically
			ignored.

			Specifically:

			-- for button and mouse-motion events, channel
			   will be a frame. (The translation to a window
			   occurs later.)
			-- for keyboard events, channel will be a console.
			   Note that fake keyboard events (generated
			   by `character-to-event' or something that
			   calls this, such as macros) need to have
			   the selected console stored into them when
			   the event is created.  This is so that the
			   correct console-local variables (e.g. the
			   command builder) will get affected.
			-- for timer, process, magic-eval, and eval events,
			   channel will be nil.
			-- for scrollbar misc-user events, channel
			   will be a window.
			-- for menubar misc-user events, channel
			   will be a frame.
			-- for magic events, channel will be a frame
			   (usually) or a device.
			   
 timestamp		When this event occurred -- if not known, this
			is made up.

 In addition, the following structures are specific to particular event
 types:

 key_press_event	
    key			What keysym this is; an integer or a symbol.
			If this is an integer, it will be in the printing
			ASCII range: >32 and <127.
    modifiers		Bucky-bits on that key: control, meta, etc.
			For many keys, Shift is not a bit; that is implicit
			in the keyboard layout.

 button_press_event
 button_release_event
    button		What button went down or up.
    modifiers		Bucky-bits on that button: shift, control, meta, etc.
    x, y		Where it was at the button-state-change (in pixels).

 pointer_motion_event
    x, y		Where it was after it moved (in pixels).
    modifiers		Bucky-bits down when the motion was detected.
			(Possibly not all window systems will provide this?)

 process_event
    process		the XEmacs "process" object in question

 timeout_event
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

 eval_event
    function		An elisp function to call with this event object.
    internal_function	An unexported function to call with this event
			object.  This allows eval events to call internal
			functions.  For a normal eval event, this field
			will always be 0.
    object		Anything.
			This kind of event is used internally; sometimes the
			window system interface would like to inform XEmacs of
			some user action (such as focusing on another frame)
			but needs that to happen synchronously with the other
			user input, like keypresses.  This is useful when
			events are reported through callbacks rather
			than in the standard event stream.

 misc_user_event
    function		An elisp function to call with this event object.
    internal_function	Ignored.
    object		Anything.
			This is similar to an eval_event, except that it is
			generated by user actions: selections in the
			menubar or scrollbar actions.  It is a "command"
			event, like key and mouse presses (and unlike mouse
			motion, process output, and enter and leave window
			hooks).  In many ways, eval_events are not the same
			as keypresses or misc_user_events.

 magic_event
			No user-serviceable parts within.  This is for things
			like KeymapNotify and ExposeRegion events and so on
			that XEmacs itself doesn't care about, but which it
			must do something with for proper interaction with
			the window system.

			Magic_events are handled somewhat asynchronously, just
			like subprocess filters.  However, occasionally a 
			magic_event needs to be handled synchronously; in that
			case, the asynchronous handling of the magic_event will
			push an eval_event back onto the queue, which will be 
			handled synchronously later.  This is one of the
			reasons why eval_events exist; I'm not entirely happy
			with this aspect of this event model.

 magic_eval_event
			This is like an eval event but its contents are
			not Lisp-accessible.  This allows for "internal
			eval events" that call non-Lisp-accessible functions.
			Externally, a magic_eval_event just appears as
			a magic_event; the Lisp programmer need not know
			anything more.
 */


struct Lisp_Event;
struct Lisp_Process;

struct event_stream
{
  int  (*event_pending_p)	(int);
  void (*next_event_cb)		(struct Lisp_Event *);
  void (*handle_magic_event_cb)	(struct Lisp_Event *);
  int  (*add_timeout_cb)	(EMACS_TIME);
  void (*remove_timeout_cb)	(int);
  void (*select_console_cb)	(struct console *);
  void (*unselect_console_cb)	(struct console *);
  void (*select_process_cb)	(struct Lisp_Process *);
  void (*unselect_process_cb)	(struct Lisp_Process *);
  void (*quit_p_cb)		(void);
};


extern struct event_stream *event_stream;

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


struct key_data
{
  Lisp_Object       keysym;
  unsigned char     modifiers;
};

struct button_data
{
  int               button;
  unsigned char     modifiers;
  int               x, y;
};

struct motion_data
{
  int               x, y;
  unsigned char     modifiers;
};

struct process_data
{
  Lisp_Object       process;
};

struct timeout_data
{
  int		    interval_id;
  int		    id_number;
  Lisp_Object	    function, object;
};

struct eval_data
{
  Lisp_Object       function;
  Lisp_Object	    object;
};

struct magic_eval_data
{
  void		    (*internal_function) (Lisp_Object);
  Lisp_Object	    object;
};

#if defined (HAVE_X_WINDOWS) && defined(emacs)
# include <X11/Xlib.h>
#endif

union magic_data
{
  char             underlying_tty_event;
#ifdef HAVE_X_WINDOWS
  XEvent           underlying_x_event;
#endif
};

struct Lisp_Event
{
  /* header->next (aka XEVENT_NEXT ()) is used as follows:
     - For dead events, this is the next dead one.
     - For events on the command_event_queue, the next one on the queue.
     - Likewise for events chained in the command builder.
     - Otherwise it's Qnil.
   */
  struct lrecord_header lheader;
  Lisp_Object		next;
  emacs_event_type	event_type;
  Lisp_Object		channel;
  unsigned int		timestamp;
  union
    {
      struct key_data		key;
      struct button_data	button;
      struct motion_data	motion;
      struct process_data	process;
      struct timeout_data	timeout;
      struct eval_data		eval;	/* misc_user_event uses this too */
      union magic_data		magic;
      struct magic_eval_data	magic_eval;
    } event;
};

DECLARE_LRECORD (event, struct Lisp_Event);
#define XEVENT(x) XRECORD (x, event, struct Lisp_Event)
#define XSETEVENT(x, p) XSETRECORD (x, p, event)
#define EVENTP(x) RECORDP (x, event)
#define GC_EVENTP(x) GC_RECORDP (x, event)
#define CHECK_EVENT(x) CHECK_RECORD (x, event)
#define CONCHECK_EVENT(x) CONCHECK_RECORD (x, event)

DECLARE_LRECORD (command_builder, struct command_builder);

#define EVENT_CHANNEL(a) ((a)->channel)
#define EVENT_TYPE(a) ((a)->event_type)
#define XEVENT_TYPE(a) (XEVENT (a)->event_type)
#define EVENT_NEXT(a) ((a)->next)
#define XEVENT_NEXT(e) (XEVENT (e)->next)
#define XSET_EVENT_NEXT(e, n) do { (XEVENT (e)->next = (n)); } while (0)

#define EVENT_CHAIN_LOOP(event, chain) \
  for (event = chain; !NILP (event); event = XEVENT_NEXT (event))

#define EVENT_LIVE_P(a) (EVENT_TYPE (a) != dead_event)

#define CHECK_LIVE_EVENT(x)						\
  do { CHECK_EVENT (x);							\
       if (! EVENTP (x)							\
	   || ! EVENT_LIVE_P (XEVENT (x)))				\
         dead_wrong_type_argument (Qevent_live_p, (x)); } while (0)
#define CONCHECK_LIVE_EVENT(x)						\
  do { CONCHECK_EVENT (x);						\
       if (! EVENTP (x)							\
	   || ! EVENT_LIVE_P (XEVENT (x)))				\
         x = wrong_type_argument (Qevent_live_p, (x)); } while (0)


extern Lisp_Object Qevent_live_p;

/* Note: under X Windows, MOD_ALT is generated by the Alt key if there are
   both Alt and Meta keys.  If there are no Meta keys, then Alt generates
   MOD_META instead.
 */

#ifdef emacs
/* Maybe this should be trickier */
#define KEYSYM(x) (intern (x))

Lisp_Object allocate_command_builder (Lisp_Object console);

void format_event_object (char *buf, struct Lisp_Event *e, int brief);
void character_to_event (Emchar c, struct Lisp_Event *event,
			 struct console *con,
			 int use_console_meta_flag);
void enqueue_magic_eval_event (void (*fun) (Lisp_Object), Lisp_Object object);
void zero_event (struct Lisp_Event *e);

void deallocate_event_chain (Lisp_Object event);
Lisp_Object event_chain_tail (Lisp_Object event);
void enqueue_event (Lisp_Object event, Lisp_Object *head, Lisp_Object *tail);
Lisp_Object dequeue_event (Lisp_Object *head, Lisp_Object *tail);
void enqueue_event_chain (Lisp_Object event_chain, Lisp_Object *head,
			  Lisp_Object *tail);
int event_chain_count (Lisp_Object event_chain);
void nth_of_key_sequence_as_event (Lisp_Object seq, int n, Lisp_Object event);
Lisp_Object key_sequence_to_event_chain (Lisp_Object seq);
Lisp_Object event_chain_find_previous (Lisp_Object event_chain,
				       Lisp_Object event);
Lisp_Object event_chain_nth (Lisp_Object event_chain, int n);
Lisp_Object copy_event_chain (Lisp_Object event_chain);

/* True if this is a non-internal event
   (keyboard press, menu, scrollbar, mouse button) */
int command_event_p (Lisp_Object event);

struct console *event_console_or_selected (Lisp_Object event);

int event_stream_event_pending_p (int user);
void event_stream_next_event (struct Lisp_Event *event);
void event_stream_handle_magic_event (struct Lisp_Event *event);
void event_stream_select_console   (struct console *c);
void event_stream_unselect_console (struct console *c);
void event_stream_select_process   (struct Lisp_Process *proc);
void event_stream_unselect_process (struct Lisp_Process *proc);
void event_stream_quit_p (void);

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
void event_stream_disable_wakeup (int id, int async_p);
void event_stream_deal_with_async_timeout (int interval_id);

/* from signal.c */
int event_stream_add_async_timeout (EMACS_TIME thyme);
void event_stream_remove_async_timeout (int id);

void emacs_handle_focus_change_preliminary (Lisp_Object frame_inp_and_dev);
void emacs_handle_focus_change_final (Lisp_Object frame_inp_and_dev);

Lisp_Object extract_this_command_keys_nth_mouse_event (int n);
Lisp_Object extract_vector_nth_mouse_event (Lisp_Object vector, int n);

void single_console_state (void);
void any_console_state (void);
int in_single_console_state (void);

#ifdef HAVE_UNIXOID_EVENT_LOOP
/* Ceci n'est pas un pipe. */
extern int signal_event_pipe[];

void signal_fake_event (void);
void drain_signal_event_pipe (void);

extern int fake_event_occurred;

int event_stream_unixoid_select_console   (struct console *con);
int event_stream_unixoid_unselect_console (struct console *con);
int event_stream_unixoid_select_process   (struct Lisp_Process *proc);
int event_stream_unixoid_unselect_process (struct Lisp_Process *proc);
int read_event_from_tty_or_stream_desc (struct Lisp_Event *event,
					struct console *c, int fd);
#endif /* HAVE_UNIXOID_EVENT_LOOP */

extern int emacs_is_blocking;

extern Lisp_Object Vcontrolling_terminal;

extern volatile int sigint_happened;

/* Define this if you want the tty event stream to be used when the
   first console is tty, even if HAVE_X_WINDOWS is defined */
/* #define DEBUG_TTY_EVENT_STREAM */

#endif /* emacs */

#endif /* _XEMACS_EVENTS_H_ */
