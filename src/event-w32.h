/* Win32 specific defines for event-handling.
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

/* Random globals shared between main and message-processing thread */
extern DWORD w32_main_thread_id;
extern DWORD w32_win_thread_id;
extern CRITICAL_SECTION w32_dispatch_crit;


/*
 * Communication between main and windows thread
 */
#define WM_XEMACS_BASE		(WM_APP + 0)
#define WM_XEMACS_ACK		(WM_XEMACS_BASE + 0x00)
#define WM_XEMACS_CREATEWINDOW	(WM_XEMACS_BASE + 0x01)
#define WM_XEMACS_SETTIMER	(WM_XEMACS_BASE + 0x02)
#define WM_XEMACS_KILLTIMER	(WM_XEMACS_BASE + 0x03)
#define WM_XEMACS_END		(WM_XEMACS_BASE + 0x10)

typedef struct w32_request_type
{
  void *thing1;
  void *thing2;
} w32_request_type;

LPARAM w32_make_request(UINT message, WPARAM wParam, w32_request_type *request);
void w32_handle_request(MSG *msg);


/*
 * Event generating stuff
 */

/* The number of things we can wait on */
#define MAX_WAITABLE 256

typedef enum w32_waitable_type
{
  w32_waitable_type_none,
  w32_waitable_type_dispatch,
  w32_waitable_type_timeout,
  w32_waitable_type_process,
  w32_waitable_type_socket
} w32_waitable_type;

typedef struct w32_timeout_data
{
  int milliseconds;
  int id;
} w32_timeout_data;

typedef struct w32_waitable_info_type
{
  w32_waitable_type type;
  union
    {
      w32_timeout_data	timeout;
    } data;
} w32_waitable_info_type;

w32_waitable_info_type *w32_add_waitable(w32_waitable_info_type *info);
void w32_remove_waitable(w32_waitable_info_type *info);

/*
 * Some random function declarations in w32-proc.c
 */
DWORD w32_win_thread();
extern void w32_enqeue_dispatch_event (Lisp_Object event);


/*
 * Inside w32 magic events
 */
#define EVENT_W32_MAGIC_EVENT(e)	((e)->event.magic.underlying_w32_event)
#define EVENT_W32_MAGIC_TYPE(e)		(EVENT_W32_MAGIC_EVENT(e).message)
#define EVENT_W32_MAGIC_DATA(e)	\
	(*((RECT *) (&(EVENT_W32_MAGIC_EVENT(e).data))))

