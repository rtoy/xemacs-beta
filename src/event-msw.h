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

/* Random globals shared between main and message-processing thread */
extern DWORD mswindows_main_thread_id;
extern DWORD mswindows_win_thread_id;
extern CRITICAL_SECTION mswindows_dispatch_crit;


/*
 * Communication between main and windows thread
 */
#define WM_XEMACS_BASE		(WM_APP + 0)
#define WM_XEMACS_ACK		(WM_XEMACS_BASE + 0x00)
#define WM_XEMACS_CREATEWINDOW	(WM_XEMACS_BASE + 0x01)
#define WM_XEMACS_SETTIMER	(WM_XEMACS_BASE + 0x02)
#define WM_XEMACS_KILLTIMER	(WM_XEMACS_BASE + 0x03)
#define WM_XEMACS_END		(WM_XEMACS_BASE + 0x10)

typedef struct mswindows_request_type
{
  void *thing1;
  void *thing2;
} mswindows_request_type;

LPARAM mswindows_make_request(UINT message, WPARAM wParam, mswindows_request_type *request);
void mswindows_handle_request(MSG *msg);


/*
 * Event generating stuff
 */

/* The number of things we can wait on */
#define MAX_WAITABLE 256

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
 * Some random function declarations in mswindows-proc.c
 */
DWORD mswindows_win_thread();
extern void mswindows_enqeue_dispatch_event (Lisp_Object event);


/*
 * Inside mswindows magic events
 */
#define EVENT_MSWINDOWS_MAGIC_EVENT(e)	\
	((e)->event.magic.underlying_mswindows_event)
#define EVENT_MSWINDOWS_MAGIC_TYPE(e)	\
	(EVENT_MSWINDOWS_MAGIC_EVENT(e).message)
#define EVENT_MSWINDOWS_MAGIC_DATA(e)	\
	(*((RECT *) (&(EVENT_MSWINDOWS_MAGIC_EVENT(e).data))))


#endif /* _XEMACS_EVENT_MSW_H_ */
