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
 * X on UNIX may be bad, but the win32 API really really really sucks.
 *
 * Windows user-input type events are stored in a per-thread message queue
 * and retrieved using GetMessage(). It is not possible to wait on this
 * queue and on other events (eg process input) simultaneously. Also, the
 * main event-handling code in windows (the "windows procedure") is called
 * asynchronously when windows has certain other types of events ("nonqueued
 * messages") to deliver. The documentation doesn't appear to specify the
 * context in which the windows procedure is called, but I assume that the
 * thread that created the window is temporarily highjacked for this purpose
 * when it calls GetMessage (a bit like X callbacks?).
 *
 * We spawn off a single thread to deal with both queued and non-queued
 * events. The thread turns both kinds of events into emacs_events and stuffs
 * them in a queue which XEmacs reads at its leisure. This file contains the
 * code for that thread.
 *
 * Unfortunately, under win32 a seemingly-random selection of resources are
 * owned by the thread that created/asked for them and not by the process. In
 * particular, only the thread that created a window can retrieve messages
 * destined for that window ("GetMessage does not retrieve messages for
 * windows that belong to other threads..."). This means that our message-
 * processing thread also has to do all window creation, deletion and various
 * other random stuff. We handle this bogosity by getting the main XEmacs
 * thread to send special user-defined messages to the message-processing
 * thread to instruct it to create windows etc.
 *
 * More bogosity: Windows95 doesn't offer any one-shot timers, only a
 * periodic timer. Worse, if you don't want a periodic timer to be associated
 * with a particular mswindows window (we don't) your periodic timers don't
 * have unique ids associated with them. We get round this lameness by
 * setting off a single periodic timer and we use this to schedule timeouts
 * manually. Implementing basic stuff like one-shot timers at the application
 * level is not particularly efficient, but Windows95 leaves us no choice.
 */


#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "device.h"
#include "frame.h"
#include "events.h"
#include "event-msw.h"

#ifdef DEBUG_XEMACS
# include "opaque.h"	/* For the debug functions at the end of this file */
# undef DEBUG_MESSAGES
# undef DEBUG_TIMEOUTS
#endif

#define MSWINDOWS_FRAME_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_TILEDWINDOW
#define MSWINDOWS_POPUP_STYLE WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_CAPTION|WS_POPUP

static LRESULT WINAPI mswindows_wnd_proc (HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
static Lisp_Object mswindows_find_console (HWND hwnd);
static Lisp_Object mswindows_find_frame (HWND hwnd);
static Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key, int mods);
static int mswindows_modifier_state (void);
static int mswindows_enqueue_timeout (int milliseconds);
static void mswindows_dequeue_timeout (int interval_id);

/* Virtual keycode of the '@' key */
static int virtual_at_key;

/* Timeout queue */
struct mswindows_timeout
{
  int ticks;
  int interval_id;
  struct mswindows_timeout *next;
};
typedef struct mswindows_timeout mswindows_timeout;
static mswindows_timeout timeout_pool[MSW_TIMEOUT_MAX];
static mswindows_timeout *timeout_head = NULL;
static int timeout_mswindows_id;

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

  /* Hack! Windows doesn't report Ctrl-@ characters so we have to find out
   * which virtual key generates '@' at runtime */
  virtual_at_key = VkKeyScan ('@');
  if (virtual_at_key & 0x200)	/* 0x200 means the control key */
    /* If you need Ctrl just to generate @, you can't do Ctrl-@ */
    virtual_at_key = -1;
  else
    virtual_at_key &= 0xff;	/* The low byte contains the keycode */

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

    /* Timeout(s) */
    else if (msg.message ==  WM_TIMER)
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      if (timeout_head!=NULL)
	--(timeout_head->ticks);

      while (timeout_head!=NULL && timeout_head->ticks==0)
	{
	  Lisp_Object emacs_event;
	  struct Lisp_Event *event;
	  int id = timeout_head->interval_id;

#ifdef DEBUG_TIMEOUTS
	  stderr_out("--> %x\n", id);
#endif
	  mswindows_dequeue_timeout (id);
	  emacs_event = Fmake_event (Qnil, Qnil);
	  event = XEVENT(emacs_event);

	  event->channel = Qnil;
	  event->timestamp = msg.time;
	  event->event_type = timeout_event;
	  event->event.timeout.interval_id = id;
	  mswindows_enqueue_dispatch_event (emacs_event);
	}
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

  static sizing = 0;
  MSG msg = { hwnd, message, wParam, lParam, 0, {0,0} };
  msg.time = GetMessageTime();

#ifdef DEBUG_MESSAGES
  stderr_out("Message %04x, wParam=%04x, lParam=%08lx\n", message, wParam, lParam);
#endif
  switch (message)
  {
  case WM_KEYDOWN:
  case WM_SYSKEYDOWN:
    {
      /* Handle those keys that TranslateMessage won't generate a WM_CHAR for */
      Lisp_Object keysym;
      int mods = mswindows_modifier_state();

      if (!NILP (keysym = mswindows_key_to_emacs_keysym(wParam, mods)))
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
    TranslateMessage (&msg);  /* Maybe generates WM_[SYS]CHAR in message queue */
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

      /* XEmacs doesn't seem to like Shift on non-alpha keys */
      event->event.key.modifiers = isalpha(wParam) ? 
				   mswindows_modifier_state() :
				   mswindows_modifier_state() & ~MOD_SHIFT;

      if (wParam<' ')	/* Control char not already handled under WM_KEYDOWN */
      {
	/* Don't capitalise alpha control keys */
	event->event.key.keysym = isalpha(wParam+'a'-1) ?
				  make_char(wParam+'a'-1) :
				  make_char(wParam+'A'-1);
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
      short x, y;

      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event.button.button =
	(message==WM_LBUTTONDOWN || message==WM_LBUTTONUP) ? 1 :
	 ((message==WM_RBUTTONDOWN || message==WM_RBUTTONUP) ? 3 : 2);
      x = LOWORD (lParam);
      y = HIWORD (lParam);
      event->event.button.x = x;
      event->event.button.y = y;
      event->event.button.modifiers = mswindows_modifier_state();
      
      if (message==WM_LBUTTONDOWN || message==WM_MBUTTONDOWN ||
	  message==WM_RBUTTONDOWN)
	{
	  event->event_type = button_press_event;
	  SetCapture (hwnd);
	}
      else
	{
	  event->event_type = button_release_event;
	  ReleaseCapture ();
	}

      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_MOUSEMOVE:
    /* Optimization: don't report mouse movement while size is changind */
    if (!sizing)
    {
      short x, y;

      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT(emacs_event);

      event->channel = mswindows_find_frame(hwnd);
      event->timestamp = msg.time;
      event->event_type = pointer_motion_event;
      x = LOWORD (lParam);
      y = HIWORD (lParam);
      event->event.motion.x = x;
      event->event.motion.y = y;
      event->event.motion.modifiers = mswindows_modifier_state();
      
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

  /* Misc magic events which only require that the frame be identified */
  case WM_SETFOCUS:
  case WM_KILLFOCUS:
  case WM_CLOSE:
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      emacs_event = Fmake_event (Qnil, Qnil);
      event = XEVENT (emacs_event);

      event->channel = mswindows_find_frame (hwnd);
      event->timestamp = msg.time;
      event->event_type = magic_event;
      EVENT_MSWINDOWS_MAGIC_TYPE (event) = message;

      mswindows_enqueue_dispatch_event (emacs_event);
      LeaveCriticalSection (&mswindows_dispatch_crit);
    }
    break;

  case WM_WINDOWPOSCHANGING:
    {
      WINDOWPOS *wp = (LPWINDOWPOS) lParam;
      WINDOWPLACEMENT wpl = { sizeof(WINDOWPLACEMENT) };
      GetWindowPlacement(hwnd, &wpl);

      /* Only interested if size is changing and we're not being iconified */
      if ((wpl.showCmd != SW_SHOWMINIMIZED) && !(wp->flags & SWP_NOSIZE))
      {
	RECT ncsize = { 0, 0, 0, 0 };
	int pixwidth, pixheight;
	AdjustWindowRect (&ncsize, GetWindowLong (hwnd, GWL_STYLE), FALSE);

	round_size_to_char (XFRAME (mswindows_find_frame (hwnd)),
			    wp->cx - (ncsize.right - ncsize.left),
			    wp->cy - (ncsize.bottom - ncsize.top),
			    &pixwidth, &pixheight);

	/* Convert client sizes to window sizes */
	pixwidth += (ncsize.right - ncsize.left);
	pixheight += (ncsize.bottom - ncsize.top);

	if (wpl.showCmd != SW_SHOWMAXIMIZED)
	  {
	    /* Adjust so that the bottom or right doesn't move if it's
	     * the top or left that's being changed */
	    RECT rect;
	    GetWindowRect (hwnd, &rect);

	    if (rect.left != wp->x)
	      wp->x += wp->cx - pixwidth;
	    if (rect.top != wp->y)
	      wp->y += wp->cy - pixheight;
	  }

	wp->cx = pixwidth;
	wp->cy = pixheight;
      }
    }
    break;

  case WM_ENTERSIZEMOVE:
  case WM_EXITSIZEMOVE:
    sizing = (message == WM_ENTERSIZEMOVE);
    goto defproc;

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

    FRAME_WIDTH (f) = INTP(width) ? XINT(width) : 80;
    FRAME_HEIGHT (f) = INTP(height) ? XINT(height) : 30;
    char_to_pixel_size (f, FRAME_WIDTH(f), FRAME_HEIGHT (f),
			&FRAME_PIXWIDTH (f), &FRAME_PIXHEIGHT (f));

    rect.left = rect.top = 0;
    rect.right = FRAME_PIXWIDTH (f);
    rect.bottom = FRAME_PIXHEIGHT (f);
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

  case WM_XEMACS_DESTROYWINDOW:
    {
      struct frame *f = request->thing1;
      ReleaseDC(FRAME_MSWINDOWS_HANDLE(f), FRAME_MSWINDOWS_DC(f));
      DestroyWindow(FRAME_MSWINDOWS_HANDLE(f));
      assert (PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, 0));
    }
    break;

  case WM_XEMACS_SETTIMER:
    {
      int id;
      EnterCriticalSection (&mswindows_dispatch_crit);
      id = mswindows_enqueue_timeout((int) request->thing1);
      LeaveCriticalSection (&mswindows_dispatch_crit);
      assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, id));
    }
    break;

  case WM_XEMACS_KILLTIMER:
    {
      EnterCriticalSection (&mswindows_dispatch_crit);
      mswindows_dequeue_timeout((int) request->thing1);
      LeaveCriticalSection (&mswindows_dispatch_crit);
      assert(PostThreadMessage (mswindows_main_thread_id, WM_XEMACS_ACK, 0, 0));
    }
    break;

  default:
    assert(0);
  }
}


/* Returns the state of the modifier keys in the format expected by the
 * Lisp_Event key_data, button_data and motion_data modifiers member */
int mswindows_modifier_state (void)
{
  /* Set high bit of GetKeyState's return value indicates the key is down */
  return ((GetKeyState (VK_SHIFT)   & 0x8000) ? MOD_SHIFT  : 0) |
	 ((GetKeyState (VK_CONTROL) & 0x8000) ? MOD_CONTROL: 0) |
	 ((GetKeyState (VK_MENU)    & 0x8000) ? MOD_META   : 0);
}


/*
 * Translate a mswindows virtual key to a keysym.
 * Only returns non-Qnil for keys that don't generate WM_CHAR messages
 * or whose ASCII codes (like space) xemacs doesn't like.
 * Virtual key values are defined in winresrc.h
 * XXX I'm not sure that KEYSYM("name") is the best thing to use here.
 */
Lisp_Object mswindows_key_to_emacs_keysym(int mswindows_key, int mods)
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
  case VK_CLEAR:	return KEYSYM ("clear");  /* Should do ^L ? */
  case VK_PRIOR:	return KEYSYM ("prior");
  case VK_NEXT:		return KEYSYM ("next");
  case VK_END:		return KEYSYM ("end");
  case VK_HOME:		return KEYSYM ("home");
  case VK_LEFT:		return KEYSYM ("left");
  case VK_UP:		return KEYSYM ("up");
  case VK_RIGHT:	return KEYSYM ("right");
  case VK_DOWN:		return KEYSYM ("down");
  case VK_SELECT:	return KEYSYM ("select");
  case VK_PRINT:	return KEYSYM ("print");
  case VK_EXECUTE:	return KEYSYM ("execute");
  case VK_SNAPSHOT:	return KEYSYM ("print");
  case VK_INSERT:	return KEYSYM ("insert");
  case VK_HELP:		return KEYSYM ("help");
#if 0	/* XXX What are these supposed to do? */
  case VK_LWIN		return KEYSYM ("");
  case VK_RWIN		return KEYSYM ("");
#endif
  case VK_APPS:		return KEYSYM ("menu");
  case VK_F1:		return KEYSYM ("f1");
  case VK_F2:		return KEYSYM ("f2");
  case VK_F3:		return KEYSYM ("f3");
  case VK_F4:		return KEYSYM ("f4");
  case VK_F5:		return KEYSYM ("f5");
  case VK_F6:		return KEYSYM ("f6");
  case VK_F7:		return KEYSYM ("f7");
  case VK_F8:		return KEYSYM ("f8");
  case VK_F9:		return KEYSYM ("f9");
  case VK_F10:		return KEYSYM ("f10");
  case VK_F11:		return KEYSYM ("f11");
  case VK_F12:		return KEYSYM ("f12");
  case VK_F13:		return KEYSYM ("f13");
  case VK_F14:		return KEYSYM ("f14");
  case VK_F15:		return KEYSYM ("f15");
  case VK_F16:		return KEYSYM ("f16");
  case VK_F17:		return KEYSYM ("f17");
  case VK_F18:		return KEYSYM ("f18");
  case VK_F19:		return KEYSYM ("f19");
  case VK_F20:		return KEYSYM ("f20");
  case VK_F21:		return KEYSYM ("f21");
  case VK_F22:		return KEYSYM ("f22");
  case VK_F23:		return KEYSYM ("f23");
  case VK_F24:		return KEYSYM ("f24");
  default:
    /* Special handling for Ctrl-'@' because '@' lives shifted on varying
     * virtual keys and because Windows doesn't report Ctrl-@ as a WM_CHAR */
    if (((mods & (MOD_SHIFT|MOD_CONTROL)) == (MOD_SHIFT|MOD_CONTROL)) &&
	(mswindows_key == virtual_at_key))
      return make_char('@');
  }
  return Qnil;
}


/*
 * Add a timeout to the queue. Returns the id or 0 on failure
 */
static int mswindows_enqueue_timeout (int milliseconds)
{
  static int timeout_last_interval_id;
  int target_ticks = (milliseconds + MSW_TIMEOUT_GRANULARITY-1) /
		     MSW_TIMEOUT_GRANULARITY;
  mswindows_timeout *target;
  int i;

  /* Find a free timeout */
  for (i=0; i<MSW_TIMEOUT_MAX; i++)
    {
      target = timeout_pool + i;
      if (target->interval_id == 0)
	break;
    }

  /* No free timeout */
  if (i==MSW_TIMEOUT_MAX)
    return 0;

  if (++timeout_last_interval_id == 0)
    ++timeout_last_interval_id;

  if (timeout_head == NULL || timeout_head->ticks >= target_ticks)
    {
      /* First or only timeout in the queue (common case) */
      target->interval_id = timeout_last_interval_id;
      target->ticks = target_ticks;
      target->next = timeout_head;
      timeout_head = target;

      if (target->next == NULL)
	{
	  /* Queue was empty - restart the timer */
	  timeout_mswindows_id = SetTimer (NULL, 0, MSW_TIMEOUT_GRANULARITY,
					   NULL);
#ifdef DEBUG_TIMEOUTS
	  stderr_out("Start\n");
#endif
	}
      else
	target->next->ticks -= target->ticks;
    }
  else
    {
      /* Find the timeout before this new one */
      mswindows_timeout *prev = timeout_head;
      int tick_count = prev->ticks;	/* Number of ticks up to prev */

      while (prev->next != NULL)
	{
	  if (tick_count + prev->next->ticks >= target_ticks)
	    break;
	  prev = prev->next;
	  tick_count += prev->ticks;
	}

      /* Insert the new timeout in the queue */
      target->interval_id = timeout_last_interval_id;
      target->ticks = target_ticks - tick_count;
      target->next = prev->next;
      prev->next = target;
      if (target->next != NULL)
	target->next->ticks -= target->ticks;
    }
#ifdef DEBUG_TIMEOUTS
  stderr_out("Set %x %d %d\n", timeout_last_interval_id, target_ticks, milliseconds);
#endif
  return timeout_last_interval_id;
}


/*
 * Remove a timeout from the queue
 */
static void mswindows_dequeue_timeout (int interval_id)
{
  mswindows_timeout *target;
  mswindows_timeout *prev;

  target = timeout_head;
  prev = NULL;
  while (target != NULL)
    {
      if (target->interval_id == interval_id)
	{
#ifdef DEBUG_TIMEOUTS
	  stderr_out("Kil %x %d\n", interval_id, target->ticks);
#endif
	  target->interval_id = 0;	/* Mark free */

	  if (prev!=NULL)
	    {
	      prev->next = target->next;
              if (target->next != NULL)
		target->next->ticks += target->ticks;
	    }
	  else if ((timeout_head = target->next) == NULL)
	    {
	      /* Queue is now empty - stop the timer */
	      KillTimer (NULL, timeout_mswindows_id);
	      timeout_mswindows_id = 0;
#ifdef DEBUG_TIMEOUTS
	      stderr_out("Stop\n");
#endif
	    }
	  return;
	}
      else
	{
	  prev = target;
	  target = target->next;
	}
    }

  /* Ack! the timeout wasn't in the timeout queue which means that it's
   * probably gone off and is now sitting in the dispatch queue. XEmacs will
   * be very unhappy if it sees the timeout so we have to fish it out of the
   * dispatch queue. This only happens if XEmacs can't keep up with events */
#ifdef DEBUG_TIMEOUTS
    stderr_out("Kil %x - not found\n", interval_id);
#endif
  {
    Lisp_Object match_event, emacs_event;
    struct Lisp_Event *event;
    match_event = Fmake_event (Qnil, Qnil);
    event = XEVENT(match_event);

    event->channel = Qnil;
    event->event_type = timeout_event;
    event->event.timeout.interval_id = interval_id;
    emacs_event = mswindows_cancel_dispatch_event (match_event);
    if (!NILP (emacs_event))
      Fdeallocate_event(emacs_event);
    Fdeallocate_event(match_event);
  }
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


#ifdef DEBUG_XEMACS
/*
 * Random helper functions for debugging.
 * Intended for use in the MSVC "Watch" window which doesn't like
 * the aborts that the error_check_foo() functions can make.
 */
struct lrecord_header *DHEADER(Lisp_Object obj)
{
  return (LRECORDP (obj)) ? XRECORD_LHEADER (obj) : NULL;
}

int DOPAQUE_DATA (Lisp_Object obj)
{
  return (OPAQUEP (obj)) ? OPAQUE_DATA (XOPAQUE (obj)) : NULL;
}

struct Lisp_Event *DEVENT(Lisp_Object obj)
{
  return (EVENTP (obj)) ? XEVENT (obj) : NULL;
}

struct Lisp_Cons *DCONS(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCONS (obj) : NULL;
}

Lisp_Object DCAR(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCAR (obj) : 0;
}

Lisp_Object DCDR(Lisp_Object obj)
{
  return (CONSP (obj)) ? XCDR (obj) : 0;
}

Lisp_Object DCONSCDR(Lisp_Object obj)
{
  return ((CONSP (obj)) && (CONSP (XCDR (obj)))) ? XCONS (XCDR (obj)) : 0;
}

Lisp_Object DCARCDR(Lisp_Object obj)
{
  return ((CONSP (obj)) && (CONSP (XCDR (obj)))) ? XCAR (XCDR (obj)) : 0;
}

char *DSTRING(Lisp_Object obj)
{
  return (STRINGP (obj)) ? XSTRING_DATA (obj) : NULL;
}

struct Lisp_Vector *DVECTOR(Lisp_Object obj)
{
  return (VECTORP (obj)) ? XVECTOR (obj) : NULL;
}

struct Lisp_Symbol *DSYMBOL(Lisp_Object obj)
{
  return (SYMBOLP (obj)) ? XSYMBOL (obj) : NULL;
}

char *DSYMNAME(Lisp_Object obj)
{
  return (SYMBOLP (obj)) ? XSYMBOL (obj)->name->_data : NULL;
}

#endif
