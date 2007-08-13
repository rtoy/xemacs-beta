/* mswindows-specific defines for event-handling.
   Copyright (C) 1997 Jonathan Harris.

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

/* Authorship:

   Jonathan Harris, November 1997 for 20.4.
 */

#ifndef _XEMACS_EVENT_MSW_H_
#define _XEMACS_EVENT_MSW_H_

#include <windows.h>

/*
 * XXX FIXME: The following X modifier defs in events-mod.h clash with win32
 * hotkey defs in winuser.h. For the moment lose the win32 versions.
 * Maybe we should rename all of MOD_* to something that doesn't clash.
 */
#ifdef MOD_CONTROL
#  undef MOD_CONTROL
#endif  
#ifdef MOD_ALT
#  undef MOD_ALT
#endif  
#ifdef MOD_SHIFT
#  undef MOD_SHIFT
#endif  
#include "events-mod.h"

/* The name of the main window class */
#define XEMACS_CLASS "XEmacs"

/* Granularity of timeouts in milliseconds & max number of active timeouts */
#define MSW_TIMEOUT_GRANULARITY 25
#define MSW_TIMEOUT_MAX	32

/* Random globals */
extern LRESULT WINAPI mswindows_wnd_proc (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
extern Lisp_Object mswindows_pump_outstanding_events (void);
extern int mswindows_quit_chars_count;

/* These are Lisp integer variables */
/* Jonsthan, these need not to be globals after merge -- kkm */
extern int mswindows_dynamic_frame_resize;
extern int mswindows_num_mouse_buttons;
extern int mswindows_button2_max_skew_x;
extern int mswindows_button2_max_skew_y;
extern int mswindows_button2_chord_time;

/*
 * Event generating stuff
 */

/* The number of things we can wait on */
#define MAX_WAITABLE (MAXIMUM_WAIT_OBJECTS - 1)

typedef enum mswindows_waitable_type
{
  mswindows_waitable_type_none,
  mswindows_waitable_type_dispatch,
  mswindows_waitable_type_timeout,
  mswindows_waitable_type_process,
  mswindows_waitable_type_socket
} mswindows_waitable_type;

typedef struct mswindows_timeout_data
{
  int milliseconds;
  int id;
} mswindows_timeout_data;

typedef struct mswindows_waitable_info_type
{
  mswindows_waitable_type type;
  union
    {
      mswindows_timeout_data	timeout;
    } data;
} mswindows_waitable_info_type;

mswindows_waitable_info_type *mswindows_add_waitable(mswindows_waitable_info_type *info);
void mswindows_remove_waitable(mswindows_waitable_info_type *info);

/*
 * Some random function declarations in msw-proc.c
 */
extern void mswindows_enqeue_dispatch_event (Lisp_Object event);
Lisp_Object mswindows_cancel_dispatch_event (struct Lisp_Event* event);

/*
 * Inside mswindows magic events
 */
#define EVENT_MSWINDOWS_MAGIC_EVENT(e)	\
	((e)->event.magic.underlying_mswindows_event)
#define EVENT_MSWINDOWS_MAGIC_TYPE(e)	\
	(EVENT_MSWINDOWS_MAGIC_EVENT(e).message)
#define EVENT_MSWINDOWS_MAGIC_DATA(e)	\
	(*((RECT *) (&(EVENT_MSWINDOWS_MAGIC_EVENT(e).data))))

/*
 * Messages and magic events IDs
 */
#define XM_BUMPQUEUE	(WM_USER + 101)
#define XM_MAPFRAME	(WM_USER + 102)
#define XM_UNMAPFRAME	(WM_USER + 103)

/*
 * Window LONGs indices
 */
#define XWL_FRAMEOBJ	0

/* This must be number of the above long multiplied by 4 */
#define MSWINDOWS_WINDOW_EXTRA_BYTES 4

/* Fake key modifiers which attached to a quit char event.
   Removed upon dequeueing an event */
#define FAKE_MOD_QUIT	0x80

#endif /* _XEMACS_EVENT_MSW_H_ */
