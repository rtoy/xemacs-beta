/* mswindows specific event-handling.
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

/*
 * Comment:
 *
 * Windows user-input type events are stored in a per-thread message queue
 * and retrieved using GetMessage(). It is not possible to wait on this
 * queue and on other events (eg process input) simultaneously. Also, the
 * main event-handling code in windows (the "windows procedure") is called
 * asynchronously when windows has certain other types of events ("nonqueued
 * messages") to deliver. The documentation doesn't appear to specify the
 * context in which the windows procedure is called, but I assume that the
 * thread that created the window is temporarily highjacked for this purpose.
 *
 * We spawn off a single thread to deal with both kinds of messages. The
 * thread turns the windows events into emacs_events and stuffs them in a
 * queue which XEmacs reads at its leisure. This file contains the code for
 * the thread. This scheme also helps to prevent weird synchronisation and
 * deadlock problems that might occur if the windows procedure was called
 * when XEmacs was already in the middle of processing an event. 
 *
 * Unfortunately, only the thread that created a window can retrieve messages
 * destined for that window ("GetMessage does not retrieve messages for
 * windows that belong to other threads..."). This means that our message-
 * processing thread also has to do all window creation. We handle this
 * bogosity by getting the main XEmacs thread to send special user-defined
 * messages to the message-processing thread to instruct it to create windows.
 */


#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "device.h"
#include "frame.h"
#include "events.h"
#include "event-msw.h"

#define MSWINDOWS_FRAME_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_TILEDWINDOW
#define MSWINDOWS_POPUP_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_CAPTION|WS_POPUP

static LRESULT WINAPI mswindows_wnd_proc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
static Lisp_Object mswindows_find_console (HWND hwnd);
static Lisp_Object mswindows_find_frame (HWND hwnd);
static Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key);

/*
 * Entry point for the "windows" message-processing thread
 */
DWORD mswindows_win_thread()
{
  WNDCLASS wc;
  MSG msg;
  mswindows_waitable_info_type info;

  /* Register the main window class */
  wc.style = CS_OWNDC;	/* One DC per window */
  wc.lpfnWndProc = (WNDPROC) mswindows_wnd_proc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;	/* ? */
  wc.hInstance = NULL;	/* ? */
  wc.hIcon = LoadIcon (NULL, XEMACS_CLASS);
  wc.hCursor = LoadCursor (NULL, IDC_ARROW);
  wc.hbrBackground = NULL; /* GetStockObject (WHITE_BRUSH); */
  wc.lpszMenuName = NULL;	/* XXX FIXME? Add a menu? */
  wc.lpszClassName = XEMACS_CLASS;
  RegisterClass(&wc);		/* XXX FIXME: Should use RegisterClassEx */

  info.type = mswindows_waitable_type_dispatch;
  mswindows_add_waitable(&info);

  /* Ensure our message queue is created XXX FIXME: Is this necessary? */
  PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

  /* Notify the main thread that we're ready */
  assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, 0));

  /* Main windows loop */
  while (1)
  {
    GetMessage (&msg, NULL, 0, 0);

    /*
     * Process things that don't have an associated window, so wouldn't
     * get sent to mswindows_wnd_proc
     */

    /* Request from main thread */
    if (msg.message>=WM_XEMACS_BASE && msg.message<=WM_XEMACS_END)
      mswindows_handle_request(&msg);

    /* Timeout */
    else if (msg.message ==  WM_TIMER)
    {
      Lisp_Object emacs_event;
      struct Lisp_Event *event;

      KillTimer(NULL, msg.wParam);
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = Qnil;
      event->timestamp = msg.time;
      event->event_type = timeout_event;
      event->event.timeout.interval_id = msg.wParam;
      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    else
      /* Pass on to mswindows_wnd_proc */
      DispatchMessage (&msg);
  }
}

/*
 * The windows procedure for the window class XEMACS_CLASS
 * Stuffs messages in the mswindows event queue
 */
static LRESULT WINAPI mswindows_wnd_proc(HWND hwnd, UINT message, WPARAM wParam,
				   LPARAM lParam)
{
  /* Note: Remember to initialise these before use */
  Lisp_Object emacs_event;
  struct Lisp_Event *event;

  static int mods = 0;
  MSG msg = { hwnd, message, wParam, lParam, 0, {0,0} };
  msg.time = GetMessageTime();

#if 0 /* XXX */
  stderr_out("Message %04x, wParam=%04x, lParam=%08lx\n", message, wParam, lParam);
#endif
  switch (message)
  {
  case WM_KEYDOWN:
  case WM_SYSKEYDOWN:
    switch(wParam)
    {
    case VK_SHIFT:
      mods |= MOD_SHIFT;
      break;
    case VK_CONTROL:
      mods |= MOD_CONTROL;
      break;
    case VK_MENU:
      mods |= MOD_META;
      break;
    default:
      /* Handle those keys that TranslateMessage won't generate a WM_CHAR for */
      {
        Lisp_Object keysym;
        if (!NILP (keysym = mswindows_key_to_emacs_keysym(wParam)))
	{
          EnterCriticalSection (&mswindows_dispatch_crit);
	  emacs_event = Fmake_event (Qnil, Qnil);
	  event = XEVENT(emacs_event);

          event->channel = mswindows_find_console(hwnd);
          event->timestamp = msg.time;
          event->event_type = key_press_event;
          event->event.key.keysym = keysym;
	  event->event.key.modifiers = mods;
	  mswindows_enqueue_dispatch_event (emacs_event);
          LeaveCriticalSection (&mswindows_dispatch_crit);
	  return (0);
	}
      }
    }
    TranslateMessage (&msg);  /* Maybe generates WM_[SYS]CHAR in message queue */
    goto defproc;

  case WM_KEYUP:
  case WM_SYSKEYUP:
    switch(wParam)
    {
    case VK_SHIFT:
      mods &= ~MOD_SHIFT;
      break;
    case VK_CONTROL:
      mods &= ~MOD_CONTROL;
      break;
    case VK_MENU:
      mods &= ~MOD_META;
      break;
    }
    TranslateMessage (&msg);
    goto defproc;

  case WM_CHAR:
  case WM_SYSCHAR:
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_console(hwnd);
      event->timestamp = msg.time;
      event->event_type = key_press_event;
      event->event.key.modifiers = mods;
      event->event.key.modifiers = lParam & 0x20000000 ? MOD_META : 0; /* redundant? */
      if (wParam<' ')	/* Control char not handled under WM_KEYDOWN */
      {
	event->event.key.keysym = make_char(wParam+'a'-1);
	event->event.key.modifiers |= MOD_CONTROL;   /* redundant? */
      }
      else
      {
	/* Assumes that emacs keysym == ASCII code */
	event->event.key.keysym = make_char(wParam);
      }
      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_LBUTTONDOWN:
  case WM_MBUTTONDOWN:
  case WM_RBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_MBUTTONUP:
  case WM_RBUTTONUP:
    {
      /* XXX FIXME: Do middle button emulation */
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type =
	(message==WM_LBUTTONDOWN || message==WM_MBUTTONDOWN ||
	 message==WM_RBUTTONDOWN) ?
	 button_press_event : button_release_event;
#if 0
	((wParam & MK_CONTROL) ? MOD_CONTROL : 0) |
	 ((wParam & MK_SHIFT) ? MOD_SHIFT : 0);
#endif
      event->event.button.button =
	(message==WM_LBUTTONDOWN || message==WM_LBUTTONUP) ? 1 :
	 ((message==WM_RBUTTONDOWN || message==WM_RBUTTONUP) ? 3 : 2);
      event->event.button.x = LOWORD(lParam);
      event->event.button.y = HIWORD(lParam);
      event->event.button.modifiers = mods;
      
      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_MOUSEMOVE:
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type = pointer_motion_event;
      event->event.motion.x = LOWORD(lParam);
      event->event.motion.y = HIWORD(lParam);
      event->event.motion.modifiers = mods;
      
      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_PAINT:
    if (GetUpdateRect(hwnd, NULL, FALSE))
    {
      PAINTSTRUCT paintStruct;

      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type = magic_event;
      BeginPaint (hwnd, &paintStruct);
      EVENT_MSWINDOWS_MAGIC_TYPE(event) = message;
      EVENT_MSWINDOWS_MAGIC_DATA(event) = paintStruct.rcPaint;
      EndPaint (hwnd, &paintStruct);

      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_SIZE:
    /* We only care about this message if our size has really changed */
    if (wParam==SIZE_RESTORED || wParam==SIZE_MAXIMIZED || wParam==SIZE_MINIMIZED)
    {
      RECT rect;
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type = magic_event;
      if (wParam==SIZE_MINIMIZED)
	rect.left = rect.top = rect.right = rect.bottom = -1;
      else
	GetClientRect(hwnd, &rect);
      EVENT_MSWINDOWS_MAGIC_TYPE(event) = message;
      EVENT_MSWINDOWS_MAGIC_DATA(event) = rect;

      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_SETFOCUS:
  case WM_KILLFOCUS:
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type = magic_event;
      EVENT_MSWINDOWS_MAGIC_TYPE(event) = message;

      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_QUIT:
    /* XXX FIXME: Should do something here! */
  defproc:
  default:
    return DefWindowProc (hwnd, message, wParam, lParam);
  }
  return (0);
}


/*
 * Make a request to the message-processing thread to do things that
 * can't be done in the main thread.
 */
LPARAM
mswindows_make_request(UINT message, WPARAM wParam, mswindows_request_type *request)
{
  MSG msg;
  assert(PostThreadMessage (mswindows_win_thread_id, message, wParam,
			    (LPARAM) request));
  GetMessage (&msg, NULL, WM_XEMACS_ACK, WM_XEMACS_ACK);
  return (msg.lParam);
}


/* 
 * Handle a request from the main thread to do things that have to be
 * done in the message-processing thread.
 */
static void
mswindows_handle_request (MSG *msg)
{
  mswindows_request_type *request = (mswindows_request_type *) msg->lParam;

  switch (msg->message)
  {
  case WM_XEMACS_CREATEWINDOW:
    {
    struct frame *f = request->thing1;
    Lisp_Object *props = request->thing2;
    Lisp_Object name, height, width, popup, top, left;
    int pixel_width, pixel_height;
    RECT rect;
    DWORD style;
    HWND hwnd;

    name = Fplist_get (*props, Qname, Qnil);
    height = Fplist_get (*props, Qheight, Qnil);
    width = Fplist_get (*props, Qwidth, Qnil);
    popup = Fplist_get (*props, Qpopup, Qnil);
    top = Fplist_get (*props, Qtop, Qnil);
    left = Fplist_get (*props, Qleft, Qnil);

    style = (NILP(popup)) ? MSWINDOWS_FRAME_STYLE : MSWINDOWS_POPUP_STYLE;

    /* The +1 is because there is no msw-glyph.c yet. */
    char_to_pixel_size (f, 80+1, 24+1, &pixel_width, &pixel_height);
    rect.left = rect.top = 0;
    rect.right = INTP(width) ? XINT(width) : pixel_width;
    rect.bottom = INTP(height) ? XINT(height) : pixel_height;
#ifdef HAVE_MENUBARS
    AdjustWindowRect(&rect, style, TRUE);
#else
    AdjustWindowRect(&rect, style, FALSE);
#endif

    hwnd = CreateWindow (XEMACS_CLASS,
	STRINGP(f->name) ? XSTRING_DATA(f->name) :
	  (STRINGP(name) ? XSTRING_DATA(name) : XEMACS_CLASS),
	style,
	INTP(left) ? XINT(left) : CW_USEDEFAULT,
	INTP(top) ? XINT(top) : CW_USEDEFAULT,
	rect.right-rect.left, rect.bottom-rect.top,
	NULL, NULL, NULL, NULL);
    assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, (LPARAM) hwnd));
    }
    return;

  case WM_XEMACS_SETTIMER:
    {
    UINT id;
    id=SetTimer (NULL, 0, (UINT) request->thing1, NULL);
    assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, id));
    }
    break;

  case WM_XEMACS_KILLTIMER:
    {
    KillTimer (NULL, (UINT) request->thing1);
    assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, 0));
    }
    break;

  default:
    assert(0);
  }
}


/*
 * Translate a mswindows virtual key to a keysym.
 * Only returns non-Qnil for keys that don't generate WM_CHAR messages
 * or whose ASCII codes (like space) xemacs doesn't like.
 * Virtual key values are defined in winresrc.h
 * XXX I'm not sure that KEYSYM("name") is the best thing to use here.
 */
Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key)
{
  switch (mswindows_key)
  {
  /* First the predefined ones */
  case VK_BACK:		return QKbackspace;
  case VK_TAB:		return QKtab;
  case '\n':		return QKlinefeed;  /* No VK_LINEFEED in winresrc.h */
  case VK_RETURN:	return QKreturn;
  case VK_ESCAPE:	return QKescape;
  case VK_SPACE:	return QKspace;
  case VK_DELETE:	return QKdelete;

  /* The rest */
  case VK_PRIOR:	return KEYSYM ("prior");
  case VK_NEXT:		return KEYSYM ("next");
  case VK_END:		return KEYSYM ("end");
  case VK_HOME:		return KEYSYM ("home");
  case VK_LEFT:		return KEYSYM ("left");
  case VK_UP:		return KEYSYM ("up");
  case VK_RIGHT:	return KEYSYM ("right");
  case VK_DOWN:		return KEYSYM ("down");
  case VK_INSERT:	return KEYSYM ("insert");
  case VK_HELP:		return KEYSYM ("help");
  case VK_F1:		return KEYSYM ("F1");
  case VK_F2:		return KEYSYM ("F2");
  case VK_F3:		return KEYSYM ("F3");
  case VK_F4:		return KEYSYM ("F4");
  case VK_F5:		return KEYSYM ("F5");
  case VK_F6:		return KEYSYM ("F6");
  case VK_F7:		return KEYSYM ("F7");
  case VK_F8:		return KEYSYM ("F8");
  case VK_F9:		return KEYSYM ("F9");
  case VK_F10:		return KEYSYM ("F10");
  case VK_F11:		return KEYSYM ("F11");
  case VK_F12:		return KEYSYM ("F12");
  case VK_F13:		return KEYSYM ("F13");
  case VK_F14:		return KEYSYM ("F14");
  case VK_F15:		return KEYSYM ("F15");
  case VK_F16:		return KEYSYM ("F16");
  case VK_F17:		return KEYSYM ("F17");
  case VK_F18:		return KEYSYM ("F18");
  case VK_F19:		return KEYSYM ("F19");
  case VK_F20:		return KEYSYM ("F20");
  case VK_F21:		return KEYSYM ("F21");
  case VK_F22:		return KEYSYM ("F22");
  case VK_F23:		return KEYSYM ("F23");
  case VK_F24:		return KEYSYM ("F24");
  }
  return Qnil;
}


/*
 * Find the console that matches the supplied mswindows window handle
 */
static Lisp_Object
mswindows_find_console (HWND hwnd)
{
  Lisp_Object concons;

  CONSOLE_LOOP (concons)
    {
      Lisp_Object console = XCAR (concons);
      /* We only support one console so this must be it */
      return console;
    }

  return Qnil;
}

/*
 * Find the frame that matches the supplied mswindows window handle
 */
static Lisp_Object
mswindows_find_frame (HWND hwnd)
{
  Lisp_Object frmcons, devcons, concons;

  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
    {
      struct frame *f;
      Lisp_Object frame = XCAR (frmcons);
      f = XFRAME (frame);
      if (FRAME_TYPE_P(f, mswindows))	    /* Might be a stream-type frame */
	if (FRAME_MSWINDOWS_HANDLE(f)==hwnd)
	  return frame;
    }
  assert(0);  /* XXX Can't happen! we only get messages for our windows */
  return Qnil;
}

/*
 * Random helper functions for debugging.
 * Intended for use in the MSVC "Watch" window which doesn't like
 * the aborts that the error_check_foo() functions can make.
 */
struct lrecord_header *DHEADER(Lisp_Object obj)
{
  return LRECORDP(obj) ? XRECORD_LHEADER(obj) : NULL;
    /* (lrecord_header*)(obj & 0xfffffff) */
}

struct Lisp_Event *DEVENT(Lisp_Object obj)
{
  return (EVENTP (obj)) ? XEVENT(obj) : NULL;
}

struct Lisp_Cons *DCONS(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCONS(obj) : NULL;
}

Lisp_Object DCAR(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCAR(obj) : 0;
}

Lisp_Object DCDR(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCDR(obj) : 0;
}

char *DSTRING(Lisp_Object obj)
{
  return (STRINGP (obj)) ? XSTRING_DATA(obj) : NULL;
}

struct Lisp_Vector *DVECTOR(Lisp_Object obj)
{
  return (VECTORP (obj)) ? XVECTOR(obj) : NULL;
}

struct Lisp_Symbol *DSYMBOL(Lisp_Object obj)
{
  return (SYMBOLP (obj)) ? XSYMBOL(obj) : NULL;
}

char *DSYMNAME(Lisp_Object obj)
{
  return (SYMBOLP (obj)) ? XSYMBOL(obj)->name->_data : NULL;
}
