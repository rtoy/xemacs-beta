/* The mswindows event_stream interface.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996, 2000, 2001, 2002 Ben Wing.
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

/* This file essentially Mule-ized (except perhaps some Unicode splitting).
   5-2000. */

/* Authorship:

   Ultimately based on FSF.
   Rewritten by Ben Wing.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
   Subprocess and modal loop support by Kirill M. Katsnelson.
 */

#define NEED_MSWINDOWS_SHLOBJ /* for IShellLink */

#include <config.h>
#include "lisp.h"

#if defined (CYGWIN) && !defined (HAVE_MSG_SELECT)
#error We do not support non-select() versions (i.e. very old) of Cygwin.
#endif

/* Acceptable are:

   WIN32_NATIVE and HAVE_WIN32_PROCESSES and nothing else

   CYGWIN and HAVE_MSG_SELECT and HAVE_UNIX_PROCESSES and nothing else
*/
#ifdef WIN32_NATIVE
# if !(defined (HAVE_WIN32_PROCESSES) && !defined (HAVE_UNIX_PROCESSES) && !defined (HAVE_MSG_SELECT) && !defined (CYGWIN))
#  error Something is wrong with your process definitions for Windows native.
# endif
#elif defined (CYGWIN)
# if !(defined (HAVE_UNIX_PROCESSES) && defined (HAVE_MSG_SELECT) && !defined (HAVE_WIN32_PROCESSES) && !defined (WIN32_NATIVE))
#  error Something is wrong with your process definitions for Cygwin.
# endif
#else
# error Something is wrong -- you are neither Windows native (possibly MinGW) nor Cygwin.
#endif

#include "buffer.h"
#include "device-impl.h"
#include "events.h"
#include "faces.h"
#include "frame-impl.h"
#include "glyphs.h"
#include "lstream.h"
#include "process.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "console-stream-impl.h"
#include "console-msw-impl.h"
#include "objects-msw-impl.h"

#ifdef HAVE_SCROLLBARS
# include "scrollbar-msw.h"
#endif

#ifdef HAVE_MENUBARS
# include "menubar.h"
# include "menubar-msw.h"
#endif

#ifdef HAVE_DRAGNDROP
# include "dragdrop.h"
#endif

#include "sysfile.h"
#include "sysproc.h"
#include "systime.h"
#include "syswait.h"

#ifdef HAVE_MENUBARS
#define ADJR_MENUFLAG TRUE
#else
#define ADJR_MENUFLAG FALSE
#endif

/* Timer ID used for button2 emulation */
#define BUTTON_2_TIMER_ID 1

static Lisp_Object mswindows_find_console (HWND hwnd);
static Lisp_Object mswindows_key_to_emacs_keysym (int mswindows_key, int mods,
						  int extendedp);
static int mswindows_modifier_state (BYTE *keymap, DWORD fwKeys,
				     int has_AltGr);
static void mswindows_set_chord_timer (HWND hwnd);
static int mswindows_button2_near_enough (POINTS p1, POINTS p2);
static int mswindows_current_layout_has_AltGr (void);
static int mswindows_handle_sticky_modifiers (WPARAM wParam, LPARAM lParam,
					      int downp, int keyp);

static struct event_stream *mswindows_event_stream;

#ifdef CYGWIN

extern SELECT_TYPE input_wait_mask, non_fake_input_wait_mask;
extern SELECT_TYPE process_only_mask, tty_only_mask;
SELECT_TYPE zero_mask;
extern int signal_event_pipe_initialized;
int windows_fd;

#else

/* The number of things we can wait on */
#define MAX_WAITABLE (MAXIMUM_WAIT_OBJECTS - 1)

/* List of mswindows waitable handles. */
static HANDLE mswindows_waitable_handles[MAX_WAITABLE];

/* Number of wait handles */
static int mswindows_waitable_count = 0;

#endif

/*
 * We use an additional queue, as well as the normal dispatch queue, for
 * efficiency, the normal one for user events, and another (_s_) for non-user
 * ones. We always return events out of the first one until it is empty and
 * only then proceed with the second one.
 */
static Lisp_Object mswindows_s_dispatch_event_queue;
static Lisp_Object mswindows_s_dispatch_event_queue_tail;

/* Brush for painting widgets */
static HBRUSH widget_brush = 0;
static LONG	last_widget_brushed = 0;

/* These are Lisp integers; see DEFVARS in this file for description. */
int mswindows_dynamic_frame_resize;
int mswindows_alt_by_itself_activates_menu;
Fixnum mswindows_num_mouse_buttons;
Fixnum mswindows_mouse_button_max_skew_x;
Fixnum mswindows_mouse_button_max_skew_y;
Fixnum mswindows_mouse_button_tolerance;

#ifdef DEBUG_XEMACS
Fixnum debug_mswindows_events;

static void debug_output_mswin_message (HWND hwnd, UINT message_,
					WPARAM wParam, LPARAM lParam);
#endif

/* This is the event signaled by the event pump.
   See mswindows_pump_outstanding_events for comments */
static int mswindows_error_caught_in_modal_loop;
static int mswindows_in_modal_loop;

/* Count of wound timers */
static int mswindows_pending_timers_count;

static DWORD mswindows_last_mouse_button_state;


#ifndef CYGWIN /* Skips past slurp, shove, or winsock streams */

/************************************************************************/
/*                Pipe instream - reads process output                  */
/************************************************************************/

#define PIPE_READ_DELAY 20

#define HANDLE_TO_USID(h) ((USID)(h))

#define NTPIPE_SLURP_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, ntpipe_slurp)

/* This structure is allocated by the main thread, and is deallocated
   in the thread upon exit.  There are situations when a thread
   remains blocked for a long time, much longer than the lstream
   exists. For example, "start notepad" command is issued from the
   shell, then the shell is closed by C-c C-d. Although the shell
   process exits, its output pipe will not get closed until the
   notepad process exits also, because it inherits the pipe from the
   shell. In this case, we abandon the thread, and let it live until
   all such processes exit. While struct ntpipe_slurp_stream is
   deallocated in this case, ntpipe_slurp_stream_shared_data are not. */

struct ntpipe_slurp_stream_shared_data
{
  HANDLE hev_thread;	/* Our thread blocks on this, signaled by caller */
			/* This is a manual-reset object. 		 */
  HANDLE hev_caller;	/* Caller blocks on this, and we signal it	 */
			/* This is a manual-reset object. 		 */
  HANDLE hev_unsleep;	/* Pipe read delay is canceled if this is set	 */
			/* This is a manual-reset object. 		 */
  HANDLE hpipe;		/* Pipe read end handle.			 */
  LONG   die_p;		/* Thread must exit ASAP if non-zero		 */
  BOOL   eof_p   : 1;	/* Set when thread saw EOF			 */
  BOOL   error_p : 1;   /* Read error other than EOF/broken pipe	 */
  BOOL	 inuse_p : 1;	/* this structure is in use			 */
  LONG   lock_count;    /* Client count of this struct, 0=safe to free   */
  BYTE   onebyte;	/* One byte buffer read by thread		 */
};

#define MAX_SLURP_STREAMS 32
struct ntpipe_slurp_stream_shared_data
shared_data_block[MAX_SLURP_STREAMS]={{0}};

struct ntpipe_slurp_stream
{
  LPARAM user_data;	/* Any user data stored in the stream object	 */
  struct ntpipe_slurp_stream_shared_data *thread_data;
};

DEFINE_LSTREAM_IMPLEMENTATION ("ntpipe-input", ntpipe_slurp);

/* This function is thread-safe, and is called from either thread
   context. It serializes freeing shared data structure */
static void
slurper_free_shared_data_maybe (struct ntpipe_slurp_stream_shared_data *s)
{
  if (InterlockedDecrement (&s->lock_count) == 0)
    {
      /* Destroy events */
      CloseHandle (s->hev_thread);
      CloseHandle (s->hev_caller);
      CloseHandle (s->hev_unsleep);
      CloseHandle (s->hpipe);
      s->inuse_p = 0;
    }
}

static struct ntpipe_slurp_stream_shared_data *
slurper_allocate_shared_data (void)
{
  int i=0;
  for (i = 0; i < MAX_SLURP_STREAMS; i++)
    {
      if (!shared_data_block[i].inuse_p)
	{
	  shared_data_block[i].inuse_p = 1;
	  return &shared_data_block[i];
	}
    }
  return (struct ntpipe_slurp_stream_shared_data *)0;
}

static DWORD WINAPI
slurp_thread (LPVOID vparam)
{
  struct ntpipe_slurp_stream_shared_data *s =
    (struct ntpipe_slurp_stream_shared_data *)vparam;

  for (;;)
    {
      /* Read one byte from the pipe */
      DWORD actually_read;
      if (!ReadFile (s->hpipe, &s->onebyte, 1, &actually_read, NULL))
	{
	  DWORD err = GetLastError ();
	  if (err == ERROR_BROKEN_PIPE || err == ERROR_NO_DATA)
	    s->eof_p = TRUE;
	  else
	    s->error_p = TRUE;
	}
      else if (actually_read == 0)
	s->eof_p = TRUE;

      /* We must terminate on an error or eof */
      if (s->eof_p || s->error_p)
	InterlockedIncrement (&s->die_p);

      /* Before we notify caller, we unsignal our event. */
      ResetEvent (s->hev_thread);

      /* Now we got something to notify caller, either a byte or an
	 error/eof indication. Before we do, allow internal pipe
	 buffer to accumulate little bit more data.
	 Reader function pulses this event before waiting for
	 a character, to avoid pipe delay, and to get the byte
	 immediately. */
      if (!s->die_p)
	WaitForSingleObject (s->hev_unsleep, PIPE_READ_DELAY);

      /* Either make event loop generate a process event, or
	 inblock reader */
      SetEvent (s->hev_caller);

      /* Cleanup and exit if we're shot off */
      if (s->die_p)
	break;

      /* Block until the client finishes with retrieving the rest of
	 pipe data */
      WaitForSingleObject (s->hev_thread, INFINITE);
    }

  slurper_free_shared_data_maybe (s);

  return 0;
}

static Lisp_Object
make_ntpipe_input_stream (HANDLE hpipe, LPARAM param)
{
  Lstream *lstr = Lstream_new (lstream_ntpipe_slurp, "r");
  struct ntpipe_slurp_stream *s = NTPIPE_SLURP_STREAM_DATA (lstr);
  DWORD thread_id_unused;
  HANDLE hthread;

  /* We deal only with pipes, for we're using PeekNamedPipe api */
  assert (GetFileType (hpipe) == FILE_TYPE_PIPE);

  s->thread_data = slurper_allocate_shared_data();

  /* Create reader thread. This could fail, so do not create events
     until thread is created */
  hthread = CreateThread (NULL, 0, slurp_thread, (LPVOID)s->thread_data,
			  CREATE_SUSPENDED, &thread_id_unused);
  if (hthread == NULL)
    {
      Lstream_delete (lstr);
      s->thread_data->inuse_p=0;
      return Qnil;
    }

  /* Shared data are initially owned by both main and slurper
     threads. */
  s->thread_data->lock_count = 2;
  s->thread_data->die_p = 0;
  s->thread_data->eof_p = FALSE;
  s->thread_data->error_p = FALSE;
  s->thread_data->hpipe = hpipe;
  s->user_data = param;

  /* hev_thread is a manual-reset event, initially signaled */
  s->thread_data->hev_thread = qxeCreateEvent (NULL, TRUE, TRUE, NULL);
  /* hev_caller is a manual-reset event, initially nonsignaled */
  s->thread_data->hev_caller = qxeCreateEvent (NULL, TRUE, FALSE, NULL);
  /* hev_unsleep is a manual-reset event, initially nonsignaled */
  s->thread_data->hev_unsleep = qxeCreateEvent (NULL, TRUE, FALSE, NULL);

  /* Now let it go */
  ResumeThread (hthread);
  CloseHandle (hthread);

  lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
  return wrap_lstream (lstr);
}

static LPARAM
get_ntpipe_input_stream_param (Lstream *stream)
{
  struct ntpipe_slurp_stream *s = NTPIPE_SLURP_STREAM_DATA(stream);
  return s->user_data;
}

static HANDLE
get_ntpipe_input_stream_waitable (Lstream *stream)
{
  struct ntpipe_slurp_stream *s = NTPIPE_SLURP_STREAM_DATA(stream);
  return s->thread_data->hev_caller;
}

static Bytecount
ntpipe_slurp_reader (Lstream *stream, unsigned char *data,
		     Bytecount size)
{
  /* This function must be called from the main thread only */
  struct ntpipe_slurp_stream_shared_data *s =
    NTPIPE_SLURP_STREAM_DATA(stream)->thread_data;

  if (!s->die_p)
    {
      DWORD wait_result;
      /* Disallow pipe read delay for the thread: we need a character
         ASAP */
      SetEvent (s->hev_unsleep);

      /* Check if we have a character ready. Give it a short delay,
	 for the thread to awake from pipe delay, just ion case */
      wait_result = WaitForSingleObject (s->hev_caller, 2);

      /* Revert to the normal sleep behavior. */
      ResetEvent (s->hev_unsleep);

      /* If there's no byte buffered yet, give up */
      if (wait_result == WAIT_TIMEOUT)
	{
	  errno = EAGAIN;
	  return -1;
	}
    }

  /* Reset caller unlock event now, as we've handled the pending
     process output event */
  ResetEvent (s->hev_caller);

  /* It is now safe to do anything with contents of S, except for
     changing s->die_p, which still should be interlocked */

  if (s->eof_p)
    return 0;
  if (s->error_p || s->die_p)
    return -1;

  /* Ok, there were no error neither eof - we've got a byte from the
     pipe */
  *(data++) = s->onebyte;
  --size;

  {
    DWORD bytes_read = 0;
    if (size > 0)
      {
	DWORD bytes_available;

	/* If the api call fails, return at least one byte already
	   read.  ReadFile in thread will return error */
	if (PeekNamedPipe (s->hpipe, NULL, 0, NULL, &bytes_available, NULL))
	  {

	    /* Fetch available bytes. The same consideration applies,
	       so do not check for errors. ReadFile in the thread will
	       fail if the next call fails. */
	    if (bytes_available)
	      ReadFile (s->hpipe, data, min (bytes_available, (DWORD) size),
			&bytes_read, NULL);
	  }

	/* Now we can unblock thread, so it attempts to read more */
	SetEvent (s->hev_thread);
	return bytes_read + 1;
      }
  }
  return 0;
}

static int
ntpipe_slurp_closer (Lstream *stream)
{
  /* This function must be called from the main thread only */
  struct ntpipe_slurp_stream_shared_data *s =
    NTPIPE_SLURP_STREAM_DATA(stream)->thread_data;

  /* Force thread to stop */
  InterlockedIncrement (&s->die_p);

  /* Set events which could possibly block slurper. Let it finish soon
     or later. */
  SetEvent (s->hev_unsleep);
  SetEvent (s->hev_thread);

  /* Unlock and maybe free shared data */
  slurper_free_shared_data_maybe (s);

  return 0;
}

static void
init_slurp_stream (void)
{
  LSTREAM_HAS_METHOD (ntpipe_slurp, reader);
  LSTREAM_HAS_METHOD (ntpipe_slurp, closer);
}


/************************************************************************/
/*                Pipe outstream - writes process input                 */
/************************************************************************/

#define NTPIPE_SHOVE_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, ntpipe_shove)

#define MAX_SHOVE_BUFFER_SIZE 512

struct ntpipe_shove_stream
{
  LPARAM user_data;	/* Any user data stored in the stream object	 */
  HANDLE hev_thread;	/* Our thread blocks on this, signaled by caller */
			/* This is an auto-reset object. 		 */
  HANDLE hpipe;		/* Pipe write end handle.			 */
  HANDLE hthread;	/* Reader thread handle.			 */
  char	 buffer[MAX_SHOVE_BUFFER_SIZE];	/* Buffer being written		 */
  DWORD  size;		/* Number of bytes to write			 */
  LONG   die_p;		/* Thread must exit ASAP if non-zero		 */
  LONG   idle_p;	/* Non-zero if thread is waiting for job	 */
  BOOL   error_p : 1;   /* Read error other than EOF/broken pipe	 */
  BOOL   blocking_p : 1;/* Last write attempt would cause blocking	 */
};

DEFINE_LSTREAM_IMPLEMENTATION ("ntpipe-output", ntpipe_shove);

static DWORD WINAPI
shove_thread (LPVOID vparam)
{
  struct ntpipe_shove_stream *s = (struct ntpipe_shove_stream *) vparam;

  for (;;)
    {
      DWORD bytes_written;

      /* Block on event and wait for a job */
      InterlockedIncrement (&s->idle_p);
      WaitForSingleObject (s->hev_thread, INFINITE);

      if (s->die_p)
	break;

      /* Write passed buffer if any */
      if (s->size > 0)
	{
         if (!WriteFile (s->hpipe, s->buffer, s->size, &bytes_written, NULL)
             || bytes_written != s->size)
           {
             s->error_p = TRUE;
             InterlockedIncrement (&s->die_p);
           }
         /* Set size to zero so we won't write it again if the closer sets
            die_p and kicks us */
         s->size = 0;
	}

      if (s->die_p)
	break;
    }

  return 0;
}

static Lisp_Object
make_ntpipe_output_stream (HANDLE hpipe, LPARAM param)
{
  Lstream *lstr = Lstream_new (lstream_ntpipe_shove, "w");
  struct ntpipe_shove_stream *s = NTPIPE_SHOVE_STREAM_DATA (lstr);
  DWORD thread_id_unused;

  s->die_p = 0;
  s->error_p = FALSE;
  s->hpipe = hpipe;
  s->user_data = param;

  /* Create reader thread. This could fail, so do not
     create the event until thread is created */
  s->hthread = CreateThread (NULL, 0, shove_thread, (LPVOID)s,
			     CREATE_SUSPENDED, &thread_id_unused);
  if (s->hthread == NULL)
    {
      Lstream_delete (lstr);
      return Qnil;
    }

  /* Set the priority of the thread higher so we don't end up waiting
     on it to send things. */
  if (!SetThreadPriority (s->hthread, THREAD_PRIORITY_HIGHEST))
    {
      CloseHandle (s->hthread);
      Lstream_delete (lstr);
      return Qnil;
    }

  /* hev_thread is an auto-reset event, initially nonsignaled */
  s->hev_thread = qxeCreateEvent (NULL, FALSE, FALSE, NULL);

  /* Now let it go */
  ResumeThread (s->hthread);

  lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
  return wrap_lstream (lstr);
}

static LPARAM
get_ntpipe_output_stream_param (Lstream *stream)
{
  struct ntpipe_shove_stream *s = NTPIPE_SHOVE_STREAM_DATA(stream);
  return s->user_data;
}

static Bytecount
ntpipe_shove_writer (Lstream *stream, const unsigned char *data,
		     Bytecount size)
{
  struct ntpipe_shove_stream *s = NTPIPE_SHOVE_STREAM_DATA(stream);

  if (s->error_p)
    return -1;

  s->blocking_p = !s->idle_p;
  if (s->blocking_p)
    return 0;

  if (size>MAX_SHOVE_BUFFER_SIZE)
    return 0;

  memcpy (s->buffer, data, size);
  s->size = size;

  /* Start output */
  InterlockedDecrement (&s->idle_p);
  SetEvent (s->hev_thread);
  /* Give it a chance to run -- this dramatically improves performance
     of things like crypt. */
  if (xSwitchToThread) /* not in Win9x */
    (void) xSwitchToThread ();
  return size;
}

static int
ntpipe_shove_was_blocked_p (Lstream *stream)
{
  struct ntpipe_shove_stream *s = NTPIPE_SHOVE_STREAM_DATA(stream);
  return s->blocking_p;
}

static int
ntpipe_shove_closer (Lstream *stream)
{
  struct ntpipe_shove_stream *s = NTPIPE_SHOVE_STREAM_DATA(stream);

  /* Force thread stop */
  InterlockedIncrement (&s->die_p);

  /* Close pipe handle, possibly breaking it */
  CloseHandle (s->hpipe);

  /* Thread will end upon unblocking.  If it's already unblocked this will
     do nothing, but the thread won't look at die_p until it's written any
     pending output. */
  SetEvent (s->hev_thread);

  /* Wait while thread terminates */
  WaitForSingleObject (s->hthread, INFINITE);

  /* Close the thread handle */
  CloseHandle (s->hthread);

  /* Destroy the event */
  CloseHandle (s->hev_thread);

  return 0;
}

static void
init_shove_stream (void)
{
  LSTREAM_HAS_METHOD (ntpipe_shove, writer);
  LSTREAM_HAS_METHOD (ntpipe_shove, was_blocked_p);
  LSTREAM_HAS_METHOD (ntpipe_shove, closer);
}

/************************************************************************/
/*                         Winsock I/O stream                           */
/************************************************************************/

#define WINSOCK_READ_BUFFER_SIZE 1024

struct winsock_stream
{
  LPARAM user_data;		/* Any user data stored in the stream object */
  SOCKET s;			/* Socket handle (which is a Win32 handle)   */
  OVERLAPPED ov;		/* Overlapped I/O structure		     */
  void *buffer;			/* Buffer.                                   */
  DWORD bufsize;		/* Number of bytes last read		     */
  DWORD charbpos;		/* Position in buffer for next fetch	     */
  unsigned int error_p :1;	/* I/O Error seen			     */
  unsigned int eof_p :1;	/* EOF Error seen			     */
  unsigned int pending_p :1;	/* There is a pending I/O operation	     */
  unsigned int blocking_p :1;	/* Last write attempt would block	     */
};

#define WINSOCK_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, winsock)

DEFINE_LSTREAM_IMPLEMENTATION ("winsock", winsock);

static void
winsock_initiate_read (struct winsock_stream *str)
{
  ResetEvent (str->ov.hEvent);
  str->charbpos = 0;

  if (!ReadFile ((HANDLE)str->s, str->buffer, WINSOCK_READ_BUFFER_SIZE,
		 &str->bufsize, &str->ov))
    {
      if (GetLastError () == ERROR_IO_PENDING)
	str->pending_p = 1;
      else if (GetLastError () == ERROR_HANDLE_EOF)
	str->eof_p = 1;
      else
	str->error_p = 1;
    }
  else if (str->bufsize == 0)
    str->eof_p = 1;
}

static Bytecount
winsock_reader (Lstream *stream, unsigned char *data, Bytecount size)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (stream);

  /* If the current operation is not yet complete, there's nothing to
     give back */
  if (str->pending_p)
    {
      if (WaitForSingleObject (str->ov.hEvent, 0) == WAIT_TIMEOUT)
	{
	  errno = EAGAIN;
	  return -1;
	}
      else
	{
	  if (!GetOverlappedResult ((HANDLE)str->s, &str->ov, &str->bufsize,
				    TRUE))
	    {
	      if (GetLastError() == ERROR_HANDLE_EOF)
		str->bufsize = 0;
	      else
		str->error_p = 1;
	    }
	  if (str->bufsize == 0)
	    str->eof_p = 1;
	  str->pending_p = 0;
	}
    }

  if (str->eof_p)
    return 0;
  if (str->error_p)
    return -1;

  /* Return as much of buffer as we have */
  size = min (size, (Bytecount) (str->bufsize - str->charbpos));
  memcpy (data, (void *) ((BYTE *) str->buffer + str->charbpos), size);
  str->charbpos += size;

  /* Read more if buffer is exhausted */
  if (str->bufsize == str->charbpos)
    winsock_initiate_read (str);

  return size;
}

static Bytecount
winsock_writer (Lstream *stream, const unsigned char *data,
		Bytecount size)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (stream);

  if (str->pending_p)
    {
      if (WaitForSingleObject (str->ov.hEvent, 0) == WAIT_TIMEOUT)
	{
	  str->blocking_p = 1;
	  return -1;
	}
      else
	{
	  DWORD dw_unused;
	  if (!GetOverlappedResult ((HANDLE)str->s, &str->ov, &dw_unused,
				    TRUE))
	    str->error_p = 1;
	  str->pending_p = 0;
	}
    }

  str->blocking_p = 0;

  if (str->error_p)
    return -1;

  if (size == 0)
    return 0;

  ResetEvent (str->ov.hEvent);

  /* According to WriteFile docs, we must hold onto the data we pass to it
     and not make any changes until it finishes -- which may not be until
     the next time we get here, since we use asynchronous I/O.  We have
     in fact seen data loss as a result of not doing this. */
  str->buffer = xrealloc (str->buffer, size);
  memcpy (str->buffer, data, size);

  /* According to MSDN WriteFile docs, the fourth parameter cannot be NULL
     on Win95 even when doing an overlapped operation, as we are, where
     the return value through that parameter is not meaningful. */
  if (WriteFile ((HANDLE)str->s, str->buffer, size, &str->bufsize,
		 &str->ov)
      || GetLastError() == ERROR_IO_PENDING)
    str->pending_p = 1;
  else
    str->error_p = 1;

  return str->error_p ? -1 : size;
}

static int
winsock_closer (Lstream *lstr)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (lstr);

  if (lstr->flags & LSTREAM_FL_READ)
    shutdown (str->s, 0);
  else
    shutdown (str->s, 1);

  closesocket (str->s);
  if (str->pending_p)
    WaitForSingleObject (str->ov.hEvent, INFINITE);

  if (str->buffer)
    {
      xfree (str->buffer);
      str->buffer = 0;
    }

  CloseHandle (str->ov.hEvent);
  return 0;
}

static int
winsock_was_blocked_p (Lstream *stream)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (stream);
  return str->blocking_p;
}

static Lisp_Object
make_winsock_stream_1 (SOCKET s, LPARAM param, const char *mode)
{
  Lstream *lstr = Lstream_new (lstream_winsock, mode);
  struct winsock_stream *str = WINSOCK_STREAM_DATA (lstr);

  xzero (*str);
  str->s = s;
  str->user_data = param;

  str->ov.hEvent = qxeCreateEvent (NULL, TRUE, FALSE, NULL);

  if (lstr->flags & LSTREAM_FL_READ)
    {
      str->buffer = xmalloc (WINSOCK_READ_BUFFER_SIZE);
      winsock_initiate_read (str);
    }

  lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
  return wrap_lstream (lstr);
}

static Lisp_Object
make_winsock_input_stream (SOCKET s, LPARAM param)
{
  return make_winsock_stream_1 (s, param, "r");
}

static Lisp_Object
make_winsock_output_stream (SOCKET s, LPARAM param)
{
  return make_winsock_stream_1 (s, param, "w");
}

static HANDLE
get_winsock_stream_waitable (Lstream *lstr)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (lstr);
  return str->ov.hEvent;
}

static LPARAM
get_winsock_stream_param (Lstream *lstr)
{
  struct winsock_stream *str = WINSOCK_STREAM_DATA (lstr);
  return str->user_data;
}

static void
init_winsock_stream (void)
{
  LSTREAM_HAS_METHOD (winsock, reader);
  LSTREAM_HAS_METHOD (winsock, writer);
  LSTREAM_HAS_METHOD (winsock, closer);
  LSTREAM_HAS_METHOD (winsock, was_blocked_p);
}
#endif /* ! CYGWIN */

/************************************************************************/
/*                     Dispatch queue management                        */
/************************************************************************/

static int
mswindows_user_event_p (Lisp_Event *sevt)
{
  return (sevt->event_type == key_press_event
	  || sevt->event_type == button_press_event
	  || sevt->event_type == button_release_event
	  || sevt->event_type == misc_user_event);
}

/*
 * Add an emacs event to the proper dispatch queue
 */
void
mswindows_enqueue_dispatch_event (Lisp_Object event)
{
  int user_p = mswindows_user_event_p (XEVENT (event));
  if (user_p)
    enqueue_dispatch_event (event);
  else
    enqueue_event (event, &mswindows_s_dispatch_event_queue,
		   &mswindows_s_dispatch_event_queue_tail);

  /* Avoid blocking on WaitMessage */
  qxePostMessage (NULL, XM_BUMPQUEUE, 0, 0);
}

/*
 * Add a misc-user event to the dispatch queue.
 */
void
mswindows_enqueue_misc_user_event (Lisp_Object channel, Lisp_Object function,
				   Lisp_Object object)
{
  Lisp_Object event = Fmake_event (Qnil, Qnil);

  XSET_EVENT_TYPE (event, misc_user_event);
  XSET_EVENT_CHANNEL (event, channel);
  XSET_EVENT_TIMESTAMP (event, GetTickCount());
  XSET_EVENT_MISC_USER_FUNCTION (event, function);
  XSET_EVENT_MISC_USER_OBJECT (event, object);

  mswindows_enqueue_dispatch_event (event);
}

void
mswindows_enqueue_magic_event (HWND hwnd, UINT msg)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);

  XSET_EVENT_CHANNEL (emacs_event, hwnd ? mswindows_find_frame (hwnd) : Qnil);
  XSET_EVENT_TIMESTAMP (emacs_event, GetMessageTime ());
  XSET_EVENT_TYPE (emacs_event, magic_event);
  XSET_EVENT_MAGIC_MSWINDOWS_EVENT (emacs_event, msg);

  mswindows_enqueue_dispatch_event (emacs_event);
}

static void
mswindows_enqueue_process_event (Lisp_Process *p)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
  Lisp_Object process = wrap_process (p);

  XSET_EVENT_TYPE (emacs_event, process_event);
  XSET_EVENT_TIMESTAMP (emacs_event, GetTickCount ());
  XSET_EVENT_PROCESS_PROCESS (emacs_event, process);

  mswindows_enqueue_dispatch_event (emacs_event);
}

static void
mswindows_enqueue_mouse_button_event (HWND hwnd, UINT msg, POINTS where,
				      int mods, DWORD when)
{
  int downp = (msg == WM_LBUTTONDOWN || msg == WM_MBUTTONDOWN ||
	       msg == WM_RBUTTONDOWN);

  /* We always use last message time, because mouse button
     events may get delayed, and XEmacs double click
     recognition will fail */

  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);

  mswindows_handle_sticky_modifiers (0, 0, downp, 0);

  if (downp)
    {
      XSET_EVENT_TYPE (emacs_event, button_press_event);
    }
  else
    {
      XSET_EVENT_TYPE (emacs_event, button_release_event);
    }

  XSET_EVENT_CHANNEL (emacs_event, mswindows_find_frame (hwnd));
  XSET_EVENT_TIMESTAMP (emacs_event, when);
  XSET_EVENT_BUTTON_BUTTON (emacs_event, 
	 (msg==WM_LBUTTONDOWN || msg==WM_LBUTTONUP) ? 1 :
	 ((msg==WM_RBUTTONDOWN || msg==WM_RBUTTONUP) ? 3 : 2));
  XSET_EVENT_BUTTON_X (emacs_event, where.x);
  XSET_EVENT_BUTTON_Y (emacs_event, where.y);
  XSET_EVENT_BUTTON_MODIFIERS (emacs_event,
			       mswindows_modifier_state (NULL, mods, 0));

  if (downp)
    {
      SetCapture (hwnd);
      /* we need this to make sure the main window regains the focus
         from control subwindows */
      if (GetFocus() != hwnd)
	{
	  SetFocus (hwnd);
	  mswindows_enqueue_magic_event (hwnd, WM_SETFOCUS);
	}
    }
  else
    {
      ReleaseCapture ();
    }

  mswindows_enqueue_dispatch_event (emacs_event);
}

static Lisp_Object
mswindows_enqueue_keypress_event (HWND hwnd, Lisp_Object keysym, int mods)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);

  XSET_EVENT_CHANNEL (emacs_event,  mswindows_find_console(hwnd));
  XSET_EVENT_TIMESTAMP (emacs_event, GetMessageTime());
  XSET_EVENT_TYPE (emacs_event, key_press_event);
  XSET_EVENT_KEY_KEYSYM (emacs_event, keysym);
  XSET_EVENT_KEY_MODIFIERS (emacs_event, mods);
  mswindows_enqueue_dispatch_event (emacs_event);
  return emacs_event;
}

/*
 * Remove and return the first emacs event on the dispatch queue.
 * Give a preference to user events over non-user ones.
 */
static Lisp_Object
mswindows_dequeue_dispatch_event (void)
{
  assert (!NILP (dispatch_event_queue) ||
	  !NILP (mswindows_s_dispatch_event_queue));

  if (!NILP (dispatch_event_queue))
    return dequeue_dispatch_event ();
  else
    return dequeue_event (&mswindows_s_dispatch_event_queue,
			  &mswindows_s_dispatch_event_queue_tail);
}

#ifndef CYGWIN
/************************************************************************/
/*                     Waitable handles manipulation                    */
/************************************************************************/
static int
find_waitable_handle (HANDLE h)
{
  int i;
  for (i = 0; i < mswindows_waitable_count; ++i)
    if (mswindows_waitable_handles[i] == h)
      return i;

  return -1;
}

static BOOL
add_waitable_handle (HANDLE h)
{
  assert (find_waitable_handle (h) < 0);
  if (mswindows_waitable_count == MAX_WAITABLE)
    return FALSE;

  mswindows_waitable_handles [mswindows_waitable_count++] = h;
  return TRUE;
}

static void
remove_waitable_handle (HANDLE h)
{
  int ix = find_waitable_handle (h);
  if (ix < 0)
    return;

  mswindows_waitable_handles [ix] =
    mswindows_waitable_handles [--mswindows_waitable_count];
}

#endif /* CYGWIN */

/*
 * Given a lisp process pointer remove the corresponding process handle
 * from mswindows_waitable_handles if it is in it.  Normally the handle is
 * removed when the process terminates, but if the lisp process structure
 * is deleted before the process terminates we must delete the process
 * handle since it will be invalid and will cause the wait to fail
 */
void
mswindows_unwait_process (Lisp_Process *p)
{
#ifndef CYGWIN
  remove_waitable_handle (get_nt_process_handle (p));
#endif /* CYGWIN */
}


/************************************************************************/
/*                             Event pump                               */
/************************************************************************/

int
mswindows_window_is_xemacs (HWND hwnd)
{
  /* GetClassName will truncate a longer class name. By adding one
     extra character, we are forcing textual comparison to fail
     if the name is longer than XEMACS_CLASS */
  Extbyte class_name_buf[sizeof (XEMACS_CLASS) + 2];

  /* Use GetClassNameA because XEMACS_CLASS is not in Unicode format. */
  if (!GetClassNameA (hwnd, class_name_buf, sizeof (class_name_buf) - 1))
    return 0;

  return !ascii_strcasecmp (class_name_buf, XEMACS_CLASS);
}

struct mswindows_protect_modal_loop
{
  Lisp_Object (*bfun) (Lisp_Object barg);
  Lisp_Object barg;
};

static Lisp_Object
mswindows_protect_modal_loop_1 (void *gack)
{
  struct mswindows_protect_modal_loop *gata =
    (struct mswindows_protect_modal_loop *) gack;

  return (gata->bfun) (gata->barg);
}

Lisp_Object
mswindows_protect_modal_loop (const char *error_string,
			      Lisp_Object (*bfun) (Lisp_Object barg),
			      Lisp_Object barg, int flags)
{
  Lisp_Object tmp;
  struct mswindows_protect_modal_loop bluh;

  bluh.bfun = bfun;
  bluh.barg = barg;

  ++mswindows_in_modal_loop;
  tmp = call_trapping_problems (Qevent, error_string,
				flags, 0,
				mswindows_protect_modal_loop_1, &bluh);
  if (UNBOUNDP (tmp))
    mswindows_error_caught_in_modal_loop = 1;
  --mswindows_in_modal_loop;

  return tmp;
}

void
mswindows_unmodalize_signal_maybe (void)
{
  mswindows_error_caught_in_modal_loop = 0;
}

/*
 * This is an unsafe part of event pump, guarded by
 * condition_case. See mswindows_pump_outstanding_events
 */
static Lisp_Object
mswindows_unsafe_pump_events (Lisp_Object u_n_u_s_e_d)
{
  /* This function can call lisp */
  Lisp_Object event = Fmake_event (Qnil, Qnil);
  struct gcpro gcpro1;
  int do_redisplay = 0;
  GCPRO1 (event);

  while (detect_input_pending ())
    {
      Fnext_event (event, Qnil);
      Fdispatch_event (event);
      do_redisplay = 1;
    }

  if (do_redisplay)
    redisplay ();

  Fdeallocate_event (event);
  UNGCPRO;

  /* Qt becomes return value of mswindows_pump_outstanding_events
     once we get here */
  return Qt;
}

/*
 * This function pumps emacs events, while available, by using
 * next_message/dispatch_message loop. Errors are trapped around
 * the loop so the function always returns.
 *
 * Windows message queue is not looked into during the call,
 * neither are waitable handles checked. The function pumps
 * thus only dispatch events already queued, as well as those
 * resulted in dispatching thereof. This is done by setting
 * module local variable mswindows_in_modal_loop to nonzero.
 *
 * Return value is Qt if no errors was trapped, or Qunbound if
 * there was an error.
 *
 * In case of error, a warning is issued and the module local variable
 * mswindows_error_caught_in_modal_loop is set to non-zero.  Thus,
 * Windows internal modal loops are protected against throws, which
 * are proven to corrupt internal Windows structures.
 *
 * In case of success, mswindows_error_caught_in_modal_loop is
 * assigned 0.
 *
 * If the value of mswindows_error_caught_in_modal_loop is not
 * zero already upon entry, the function just returns non-nil.
 * This situation means that a new event has been queued while
 * in cancel mode. The event will be dequeued on the next regular
 * call of next-event; the pump is off since error is caught.
 * The caller must *unconditionally* cancel modal loop if the
 * value returned by this function is nil. Otherwise, everything
 * will become frozen until the modal loop exits under normal
 * condition (scrollbar drag is released, menu closed etc.)  */
Lisp_Object
mswindows_pump_outstanding_events (void)
{
  /* This function can call lisp */

  Lisp_Object result = Qt;
  struct gcpro gcpro1;
  GCPRO1 (result);

  if (!mswindows_error_caught_in_modal_loop)
    result = mswindows_protect_modal_loop
      ("Error during event handling", mswindows_unsafe_pump_events, Qnil, 0);
  UNGCPRO;
  return result;
}

/*
 * KEYBOARD_ONLY_P is set to non-zero when we are called from
 * QUITP, and are interesting in keyboard messages only.
 */
static void
mswindows_drain_windows_queue (void)
{
  MSG msg;

  /* should call mswindows_need_event_in_modal_loop() if in modal loop */
  assert (!mswindows_in_modal_loop);

  while (qxePeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
    {
#ifdef HAVE_DIALOGS
      /* Don't translate messages destined for a dialog box, this
	 makes keyboard traversal work. I think?? */
      if (mswindows_is_dialog_msg (&msg))
	{
	  mswindows_unmodalize_signal_maybe ();
	  continue;
	}
#endif /* HAVE_DIALOGS */

      /* We have to translate messages that are not sent to an XEmacs
         frame. This is so that key presses work ok in things like
         edit fields. However, we *musn't* translate message for XEmacs
         frames as this is handled in the wnd proc.
         We also have to avoid generating paint magic events for windows
	 that aren't XEmacs frames */

      if (!mswindows_window_is_xemacs (msg.hwnd))
	TranslateMessage (&msg);
      else if (msg.message == WM_PAINT)
	{
	  struct mswindows_frame *msframe;
	  
	  /* hdc will be NULL unless this is a subwindow - in which case we
	     shouldn't have received a paint message for it here. */
	  assert (msg.wParam == 0);

	  /* Queue a magic event for handling when safe */
	  msframe =
	    FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (msg.hwnd)));
	  if (!msframe->paint_pending)
	    {
	      msframe->paint_pending = 1;
	      mswindows_enqueue_magic_event (msg.hwnd, WM_PAINT);
	    }
	  /* Don't dispatch. WM_PAINT is always the last message in the
	     queue so it's OK to just return. */
	  return;
	}
      qxeDispatchMessage (&msg);
      mswindows_unmodalize_signal_maybe ();
    }
}

static void
emacs_mswindows_drain_queue (void)
{
  mswindows_drain_windows_queue ();
#ifdef HAVE_TTY
  drain_tty_devices ();
#endif
}

static int
emacs_mswindows_quit_check_disallowed_p (void)
{
  /* Quit cannot happen in modal loop: all program
     input is dedicated to Windows. */
  return mswindows_in_modal_loop;
}

/*
 * This is a special flavor of the mswindows_need_event function,
 * used while in event pump. Actually, there is only kind of events
 * allowed while in event pump: a timer.  An attempt to fetch any
 * other event leads to a deadlock, as there's no source of user input
 * ('cause event pump mirrors windows modal loop, which is a sole
 * owner of thread message queue).
 *
 * To detect this, we use a counter of active timers, and allow
 * fetching WM_TIMER messages. Instead of trying to fetch a WM_TIMER
 * which will never come when there are no pending timers, which leads
 * to deadlock, we simply signal an error.
 *
 * It might be possible to combine this with mswindows_drain_windows_queue
 * which fetches events when not in a modal loop.  It's not clear
 * whether the result would be more complex than is justified.
 */
static void
mswindows_need_event_in_modal_loop (int badly_p)
{
  MSG msg;

  /* Check if already have one */
  if (!NILP (dispatch_event_queue)
      || !NILP (mswindows_s_dispatch_event_queue))
    return;

  /* No event is ok */
  if (!badly_p)
    return;

  /* We do not check the user queue, because timers go to _s_ */
  while (NILP (mswindows_s_dispatch_event_queue))
    {
      /* We'll deadlock if go waiting */
      if (mswindows_pending_timers_count == 0)
	invalid_operation
	  ("Deadlock due to an attempt to call next-event in a wrong context",
	   Qunbound);

      /* Fetch and dispatch any pending timers */
      if (qxeGetMessage (&msg, NULL, WM_TIMER, WM_TIMER) > 0)
	qxeDispatchMessage (&msg);
    }
}

/*
 * This drains the event queue and fills up two internal queues until
 * an event of a type specified by USER_P is retrieved.
 *
 *
 * Used by emacs_mswindows_event_pending_p and emacs_mswindows_next_event
 */
static void
mswindows_need_event (int badly_p)
{
  while (NILP (dispatch_event_queue)
	 && NILP (mswindows_s_dispatch_event_queue))
    {
#ifdef CYGWIN
      int i;
      int active;
      SELECT_TYPE temp_mask = input_wait_mask;
      EMACS_TIME sometime;
      EMACS_SELECT_TIME select_time_to_block, *pointer_to_this;

      if (badly_p)
 	pointer_to_this = 0;
      else
	{
	  EMACS_SET_SECS_USECS (sometime, 0, 0);
	  EMACS_TIME_TO_SELECT_TIME (sometime, select_time_to_block);
	  pointer_to_this = &select_time_to_block;
	  if (mswindows_in_modal_loop)
	    /* In modal loop with badly_p false, don't care about 
	       Windows events. */
	    FD_CLR (windows_fd, &temp_mask);
	}

      active = select (MAXDESC, &temp_mask, 0, 0, pointer_to_this);

      if (active == 0)
	{
	  assert (!badly_p);
	  return;		/* timeout */
	}
      else if (active > 0)
	{
	  if (FD_ISSET (windows_fd, &temp_mask))
	    {
	      if (mswindows_in_modal_loop)
		mswindows_need_event_in_modal_loop (badly_p);
	      else
		mswindows_drain_windows_queue ();
	    }
	  else
	    {
#ifdef HAVE_TTY
	      /* Look for a TTY event */
	      for (i = 0; i < MAXDESC; i++)
		{
		  /* To avoid race conditions (among other things, an infinite
		     loop when called from Fdiscard_input()), we must return
		     user events ahead of process events. */
		  if (FD_ISSET (i, &temp_mask) && FD_ISSET (i, &tty_only_mask))
		    {
		      struct console *c =
			find_tty_or_stream_console_from_fd (i);
		      Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
		      Lisp_Event *event = XEVENT (emacs_event);
		      
		      assert (c);
		      if (read_event_from_tty_or_stream_desc (event, c))
			{
			  mswindows_enqueue_dispatch_event (emacs_event);
			  return;
			}
		    }
		}
#endif
	      /* Look for a process event */
	      for (i = 0; i < MAXDESC; i++)
		{
		  if (FD_ISSET (i, &temp_mask))
		    {
		      if (FD_ISSET (i, &process_only_mask))
			{
			  Lisp_Process *p =
			    get_process_from_usid (FD_TO_USID (i));

			  mswindows_enqueue_process_event (p);
			}
		      else
			{
			  /* We might get here when a fake event came
			     through a signal. Return a dummy event, so
			     that a cycle of the command loop will
			     occur. */
			  drain_signal_event_pipe ();
			  mswindows_enqueue_magic_event (NULL, XM_BUMPQUEUE);
			}
		    }
		}
	    }
	}
      else if (active == -1)
	{
	  if (errno != EINTR)
	    {
	      /* something bad happened */
	      assert (0);
	    }
	}
      else
	{
	  assert (0);
	}
#else /* not CYGWIN */
      /* Now try getting a message or process event */
      DWORD active;
      DWORD what_events;
      if (mswindows_in_modal_loop)
	/* In a modal loop, only look for timer events, and only if
	   we really need one. */
	{
	  if (badly_p)
	    what_events = QS_TIMER;
	  else
	    what_events = 0;
	}
      else
	/* Look for any event */
	what_events = QS_ALLINPUT;

      /*
	 #### YUCK YUCK YUCK!!!!

	 When running under a debugger, every time I hit F12 (which for me
	 is mapped to right-brace) I hit a breakpoint inside of Windows!

	 NTDLL! DbgBreakPoint@0 address 0x77f9eea9
	 KERNEL32! BaseAttachComplete@4 + 41 bytes
	 KERNEL32! BaseAttachCompleteThunk@0 + 19 bytes
	 USER32! MsgWaitForMultipleObjectsEx@20 + 224 bytes
	 USER32! MsgWaitForMultipleObjects@20 + 30 bytes

	 Microsoft says:

	 (Knowledge Base Q130667, PRB: F12 Causes Hard-Coded Breakpoint
	 Exception When Debugging)

	 CAUSE

	 When the F12 key is pressed and the application in focus is being
	 debugged, Windows NT calls a function similar to DebugBreak(),
	 which executes a hard coded breakpoint instruction. The integrated
	 debugger then traps the exception generated by this instruction.

	 This behavior is intentional and occurs with other debuggers such
	 as WinDbg from the Windows 32-bit SDK.

	 RESOLUTION
	 
	 While there is no way to disable this functionality, it doesn't
	 affect the application that's being debugged other than to pause
	 debugging and change focus. You can continue debugging by pressing
	 the F5 key.

	 This can be annoying if you have an application that heavily uses
	 the F12 key, so you may want to temporarily assign another key to
	 handle the F12 key functionality in your program when debugging.

	 STATUS

	 This behavior is by design. 


	 However, elsewhere I found this:

	 UserDebuggerHotKey 
	 HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\AeDebug 

	 Data type Range Default value 
	 REG_DWORD  0x0 - 0xFF  0x0  

	 Description

	 Specifies the key that, when pressed, establishes a breakpoint in
	 code being debugged.

	 The debugger interrupts code processing at the breakpoint so the
	 programmer can examine a suspected problem.

	 The key specified in this value only sets a breakpoint. It does
	 not invoke the debugger (the debugger must be running before the
	 key is pressed) and it does not switch the debugger to single-step
	 mode.

	 The value of this entry is a keyboard scan code. The default
	 value, 0x0, represents the F12 key on a 101-key keyboard or the -
	 (hyphen, VK_SUBTRACT) key on an 82-key keyboard.
	 */

      __try
	{
	  /* This fixes a long outstanding bug, where XEmacs would occasionally
	   * not redraw its window (or process other events) until "something
	   * happened" - usually the mouse moving over a frame.
	   *
	   * The problem is that MsgWaitForMultipleObjects only checks to see
	   * if NEW messages have been placed into the thread queue. So we
	   * specifically check to see if the queue is empty (using PeekMessage
	   * with the PM_NOREMOVE flag) before we wait.
	   */
	  MSG msg;
	  if (what_events == QS_ALLINPUT && badly_p &&
	      qxePeekMessage (&msg, 0, 0, 0, PM_NOREMOVE))
	    active = WAIT_OBJECT_0 + mswindows_waitable_count;
	  else
	    active = MsgWaitForMultipleObjects (mswindows_waitable_count,
						mswindows_waitable_handles,
						FALSE, badly_p ? INFINITE : 0,
						what_events);
	}
      __except (GetExceptionCode () == EXCEPTION_BREAKPOINT ?
		EXCEPTION_CONTINUE_EXECUTION :
		EXCEPTION_CONTINUE_SEARCH)
	{
	}

      /* This will assert if handle being waited for becomes abandoned.
	 Not the case currently tho */
      assert ((!badly_p && active == WAIT_TIMEOUT) ||
	      (active >= WAIT_OBJECT_0 &&
	       active <= WAIT_OBJECT_0 + mswindows_waitable_count));

      if (active == WAIT_TIMEOUT)
	{
	  /* No luck trying - just return what we've already got */
	  return;
	}
      else if (active == WAIT_OBJECT_0 + mswindows_waitable_count)
	{
	  /* Got your message, thanks */
	  if (mswindows_in_modal_loop)
	    mswindows_need_event_in_modal_loop (badly_p);
	  else
	    mswindows_drain_windows_queue ();
	}
      else
	{
	  int ix = active - WAIT_OBJECT_0;

	  /* look for a stream console event; see
	     emacs_mswindows_select_console below. */
	  LIST_LOOP_3 (porca_troia, Vconsole_list, vcontail)
	    {
	      struct console *con = XCONSOLE (porca_troia);

	      if (CONSOLE_STREAM_P (con))
		{
		  Lisp_Object instr = CONSOLE_STREAM_DATA (con)->instream;
		  if (!NILP (instr) && !UNBOUNDP (instr) &&
		      get_ntpipe_input_stream_waitable (XLSTREAM (instr)) ==
		      mswindows_waitable_handles [ix])
		    {
		      Ichar ch = Lstream_get_ichar (XLSTREAM (instr));
		      if (ch < 0)
			{
			  /* deleting the console might not be safe right now
			     ... */
			  enqueue_magic_eval_event (io_error_delete_console,
						    porca_troia);
			  /* but we definitely need to unselect it to avoid
			     infinite loops reading EOF's */
			  Fconsole_disable_input (porca_troia);
			  mswindows_enqueue_magic_event (NULL, XM_BUMPQUEUE);
			}
		      else
			{
			  Lisp_Object event = Fmake_event (Qnil, Qnil);
			  /* Here we really do want to set the
			     use_console_meta_flag because the char is from the
			     TTY. */
			  character_to_event (ch, XEVENT (event), con, 1, 1);
			  XSET_EVENT_CHANNEL (event, porca_troia);
			  enqueue_dispatch_event (event);
			}
		      break;
		    }
		}
	    }

	  if (NILP (vcontail))
	    { /* no stream console event, look for process event */
	      /* First, try to find which process' output has signaled */
	      Lisp_Process *p =
		get_process_from_usid (HANDLE_TO_USID
				       (mswindows_waitable_handles[ix]));
	      if (p != NULL)
		/* Found a signaled process input handle */
		mswindows_enqueue_process_event (p);
	      else
		{
		  /* None. This means that the process handle itself has
		     signaled.  Remove the handle from the wait vector, and
		     make status_notify note the exited process.  First
		     find the process object if possible. */
		  LIST_LOOP_3 (vaffanculo, Vprocess_list, vproctail)
		    if (get_nt_process_handle (XPROCESS (vaffanculo)) ==
			mswindows_waitable_handles [ix])
		      break;
		  mswindows_waitable_handles [ix] =
		    mswindows_waitable_handles [--mswindows_waitable_count];
		  kick_status_notify ();
		  /* We need to return a process event here so that (1)
		     accept-process-output will return when called on this
		     process, and (2) status notifications will happen in
		     accept-process-output, sleep-for, and sit-for. */
		  if (!NILP (vproctail))
		    mswindows_enqueue_process_event (XPROCESS (vaffanculo));
		  else
		    {
		      /* abort (); */
		      /* #### FUCKME!  When can this happen?  I hit this
			 abort() when I tried enabling it. */
		      /* Have to return something: there may be no
			 accompanying process event */
		      mswindows_enqueue_magic_event (NULL, XM_BUMPQUEUE);
		    }
		}
	    }
	}
#endif /* not CYGWIN */
    } /* while */
}

/************************************************************************/
/*                           Event generators                           */
/************************************************************************/

/*
 * Callback procedure for synchronous timer messages
 */
static void CALLBACK
mswindows_wm_timer_callback (HWND hwnd, UINT umsg, UINT id_timer, DWORD dwtime)
{
  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);

  if (KillTimer (NULL, id_timer))
    --mswindows_pending_timers_count;

  XSET_EVENT_CHANNEL (emacs_event, Qnil);
  XSET_EVENT_TIMESTAMP (emacs_event, dwtime);
  XSET_EVENT_TYPE (emacs_event, timeout_event);
  XSET_EVENT_TIMEOUT_INTERVAL_ID (emacs_event, id_timer);
  XSET_EVENT_TIMEOUT_FUNCTION (emacs_event, Qnil);
  XSET_EVENT_TIMEOUT_OBJECT (emacs_event, Qnil);

  mswindows_enqueue_dispatch_event (emacs_event);
}

/*
 * Callback procedure for dde messages
 *
 * We execute a dde Open("file") by simulating a file drop, so dde support
 * depends on dnd support.
 */
#ifdef HAVE_DRAGNDROP
extern int mswindows_dde_enable;

EXFUN(Fread_from_string, 3);

/* The following variables are used to maintain consistency of result and
 * error reporting to the client.
 * The basic protocol is to Execute a lisp form, and then Request one or
 * more of the following items: Status (1 = OK, 0 = Error), Result, or Error.
 * When the lisp form is queued, the dde_eval_pending flag is set to 1,
 * to indicate that the items are not yet available. The dde_eval_pending
 * flag is set to 0 when the evaluation is complete. Requests for the result
 * items will block while the dde_eval_pending flag is 1, to avoid clients
 * getting inconsistent results.
 */
static int dde_eval_pending;
static Lisp_Object dde_eval_result;
static Lisp_Object dde_eval_error;

static Lisp_Object
dde_error (Lisp_Object err, Lisp_Object obj)
{
  dde_eval_error = err;
  return Qnil;
}

/* Read lisp forms from a string. Evaluate the forms as if they were
 * wrapped in a progn form. Return the result of the form.
 */
static Lisp_Object
dde_eval_string (Lisp_Object str)
{
  struct gcpro gcpro1, gcpro2;
  Lisp_Object args[3];
  Lisp_Object obj;

  /* Heavy handed GCPROing, on the principle of it's better to be safe than
   * sorry...
   */
  args[0] = Qnil;
  args[1] = Qnil;
  args[2] = Qnil;
  GCPRO2 (args[0], str);
  gcpro1.nvars = 3;

  /* Wrap the user supplied string in string "(progn ...)".
   * We can now just read-from-string a single form. If we
   * get an error, or finish before the end of the string,
   * we know the original string had syntax errors.
   */
  args[0] = build_string ("(progn ");
  args[1] = str;
  args[2] = build_string (")");
  str = Fconcat (3, args);

  obj = Fread_from_string (str, Qnil, Qnil);
  UNGCPRO;

  /* The following doesn't check that the length fits in an EMACS_INT.
   * This won't be a problem in reality...?
   *
   * If the read didn't get to the end of the string, we have a syntax
   * error in the string supplied by the user.
   */
  if (XINT (XCDR (obj)) != XSTRING_LENGTH (str))
    return Qnil;

  GCPRO1 (obj);
  obj = Feval (XCAR (obj));

  RETURN_UNGCPRO (obj);
}

/* Evaluate the supplied string as a sequence of Lisp forms, wrapped in
 * a progn. Catch any evaluation errors. Set the evaluation status and
 * result variables.
 */
static void
dde_eval (Lisp_Object str)
{
  dde_eval_error = Qnil;
  dde_eval_result = condition_case_1 (Qt, dde_eval_string, str,
				      dde_error, Qnil);
  dde_eval_pending = 0;

  /* Re-enable callbacks in case the client is waiting on a request */
  DdeEnableCallback (mswindows_dde_mlid, NULL, EC_ENABLEALL);

  /* Post advise notifications on the result item */
  DdePostAdvise (mswindows_dde_mlid, mswindows_dde_topic_eval,
		 mswindows_dde_item_result);
}

/* A list of DDE advise tokens. Each token is an uninterned symbol,
 * whose value is the DDE string handle for its name (stored as a float,
 * as a Lisp int cannot hold a full C int).
 * The token's 'dde-data property is used to store data for a dde-advise.
 */
Lisp_Object Vdde_advise_items;

/* The symbol 'HSZ */
Lisp_Object QHSZ;

DEFUN("dde-alloc-advise-item", Fdde_alloc_advise_item, 0, 1, 0, /*
Allocate an advise item, and return its token.
*/
      (name))
{
  Lisp_Object token;
  Extbyte *str;
  HSZ hsz;
  struct gcpro gcpro1, gcpro2;

  if (!NILP (name))
    CHECK_STRING (name);
  else
    {
      static int num = 0;
      char buf[20];
      sprintf (buf, "Tok%d", num);
      ++num;
      name = build_string (buf);
    }

  token = Qnil;
  GCPRO2 (name, token);
  token = Fmake_symbol (name);
  TO_EXTERNAL_FORMAT (LISP_STRING, name, C_STRING_ALLOCA, str,
		      Qmswindows_tstr);
  hsz = qxeDdeCreateStringHandle (mswindows_dde_mlid, str,
				  XEUNICODE_P ? CP_WINUNICODE : CP_WINANSI);

  Fput(token, QHSZ, make_float ((int)hsz));
  Vdde_advise_items = Fcons (token, Vdde_advise_items);

  RETURN_UNGCPRO (token);
}

DEFUN("dde-free-advise-item", Fdde_free_advise_item, 1, 1, 0, /*
Free the resources associated with advise item ITEM.

Frees all resources allocated to allow clients to set up advise loops
on ITEM. It is assumed that no active advise loops remain. However, no
problems should arise if they do - it's just that we won't ever send any
notifications again.

If the user does not free an advise item, resources will be leaked.
*/
      (item))
{
  HSZ hsz;
  Lisp_Object val;

  CHECK_SYMBOL (item);
  val = Fget (item, QHSZ, Qnil);
  if (!FLOATP (val))
    return Qnil;
  hsz = (HSZ)(int)XFLOAT_DATA (val);
  DdeFreeStringHandle (mswindows_dde_mlid, hsz);
  Vdde_advise_items = delq_no_quit (item, Vdde_advise_items);
  return Qnil;
}

DEFUN("dde-advise", Fdde_advise, 2, 2, 0, /*
Post a DDE advise for ITEM with associated data DATA.

Records the value DATA for sending back to clients waiting for
notifications on DDE item ITEM in the system topic, and posts
the advise transaction.

ITEM must be an advise token allocated using dde-alloc-advise-item.
*/
      (item, data))
{
  HSZ hsz;
  Lisp_Object val;

  CHECK_SYMBOL (item);
  val = Fget (item, QHSZ, Qnil);
  if (!FLOATP (val))
    return Qnil;
  hsz = (HSZ)(int)XFLOAT_DATA (val);

  Fset (item, data);
  DdePostAdvise (mswindows_dde_mlid, mswindows_dde_topic_eval, hsz);
  return Qnil;
}

HDDEDATA CALLBACK
mswindows_dde_callback (UINT uType, UINT uFmt, HCONV hconv,
			HSZ hszTopic, HSZ hszItem, HDDEDATA hdata,
			DWORD dwData1, DWORD dwData2)
{
  switch (uType)
    {
    case XTYP_CONNECT:
      if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system)
          || !DdeCmpStringHandles (hszTopic, mswindows_dde_topic_eval))
	return (HDDEDATA) TRUE;
      return (HDDEDATA) FALSE;

    case XTYP_WILDCONNECT:
      {
	/* We support two {service,topic} pairs */
	HSZPAIR pairs[3] =
	{
	  { mswindows_dde_service, mswindows_dde_topic_system },
	  { mswindows_dde_service, mswindows_dde_topic_eval },
	  { 0, 0 }
	};

	if ((!hszItem
	     || !DdeCmpStringHandles (hszItem, mswindows_dde_service)) &&
	    (!hszTopic
	     || !DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system)
	     || !DdeCmpStringHandles (hszTopic, mswindows_dde_topic_eval)))
	  return (DdeCreateDataHandle (mswindows_dde_mlid, (LPBYTE) pairs,
				       sizeof (pairs), 0L, 0, uFmt, 0));
      }
      return (HDDEDATA) NULL;

    case XTYP_ADVSTART:
      if (!mswindows_dde_enable)
	return (HDDEDATA) FALSE;

      /* We only support advise loops on the eval topic for text data */
      if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_eval)
	  && (uFmt == CF_TEXT || uFmt == CF_UNICODETEXT))
	{
	  /* Only allocated items or Result, are allowed */
	  if (!DdeCmpStringHandles (hszItem, mswindows_dde_item_result))
	    return (HDDEDATA) TRUE;

	  {
	    EXTERNAL_LIST_LOOP_2 (elt, Vdde_advise_items)
	      {
		Lisp_Object val;
		HSZ hsz;
		if (!SYMBOLP (elt))
		  continue;
		val = Fget (elt, QHSZ, Qnil);
		if (!FLOATP (val))
		  continue;
		hsz = (HSZ) (int) XFLOAT_DATA (val);
		if (!DdeCmpStringHandles (hszItem, hsz))
		  return (HDDEDATA) TRUE;
	      }
	  }
	}
      return (HDDEDATA) FALSE;

      /* Both advise requests and normal requests work the same */
    case XTYP_ADVREQ:
    case XTYP_REQUEST:
      if (!mswindows_dde_enable)
	return (HDDEDATA) NULL;

      if (DdeCmpStringHandles (hszTopic, mswindows_dde_topic_eval) != 0)
	return (HDDEDATA) NULL;

      /* If this is a normal request and we're in the middle of
       * an Execute, block until the Execute completes.
       */
      if (dde_eval_pending && uType == XTYP_REQUEST)
	return (HDDEDATA) CBR_BLOCK;

      /* We can only support requests for ANSI or Unicode text */
      if (uFmt != CF_TEXT && uFmt != CF_UNICODETEXT)
	return (HDDEDATA) NULL;

      {
	Lisp_Object args[2];
	struct gcpro gcpro1;
	Lisp_Object res;
	Extbyte *result;
	DWORD bytes;

	args[0] = Qnil;
	args[1] = Qnil;
	GCPRO1 (args[0]);
	gcpro1.nvars = 2;


	if (!DdeCmpStringHandles (hszItem, mswindows_dde_item_result))
	  {
	    if (NILP (dde_eval_error))
	      {
		args[0] = build_string ("OK: %s");
		args[1] = dde_eval_result;
	      }
	    else
	      {
		args[0] = build_string ("ERR: %s");
		args[1] = dde_eval_error;
	      }
	  }
	else
	  {
	    EXTERNAL_LIST_LOOP_2 (elt, Vdde_advise_items)
	      {
		Lisp_Object val;
		HSZ hsz;
		if (!SYMBOLP (elt))
		  continue;
		val = Fget (elt, QHSZ, Qnil);
		if (!FLOATP (val))
		  continue;
		hsz = (HSZ) (int) XFLOAT_DATA (val);
		if (!DdeCmpStringHandles (hszItem, hsz))
		  args[1] = Fsymbol_value (elt);
	      }
	    args[0] = build_string ("%s");
	  }

	res = Fformat (2, args);
	UNGCPRO;

	bytes = (uFmt == CF_TEXT ? 1 : 2) * (XSTRING_LENGTH (res) + 1);
	TO_EXTERNAL_FORMAT (LISP_STRING, res,
			    C_STRING_ALLOCA, result,
			    uFmt == CF_TEXT ? Qmswindows_multibyte
			    : Qmswindows_unicode);

	/* If we cannot create the data handle, this passes the null
	 * return back to the client, which signals an error as we wish.
	 */
	return DdeCreateDataHandle (mswindows_dde_mlid, (LPBYTE)result,
				    bytes, 0L, hszItem, uFmt, 0);
      }

    case XTYP_EXECUTE:
      if (!mswindows_dde_enable)
	return (HDDEDATA) DDE_FBUSY;

      if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_eval))
	{
	  DWORD len;
	  LPBYTE extcmd;
	  Lisp_Object tmp;

	  /* Grab a pointer to the raw data supplied */
	  extcmd = DdeAccessData (hdata, &len);

	  TO_INTERNAL_FORMAT (DATA, (extcmd, len),
			      LISP_STRING, tmp,
			      Qmswindows_tstr);

	  /* Release and free the data handle */
	  DdeUnaccessData (hdata);
	  DdeFreeDataHandle (hdata);

	  /* Set a flag to say that the evaluation isn't yet complete,
	   * enqueue the evaluation, send a dummy event to trigger the
	   * event loop (I've no idea why this is needed, but it works...)
	   * and return success to the client.
	   */
	  dde_eval_pending = 1;
	  enqueue_magic_eval_event (dde_eval, tmp);
	  mswindows_enqueue_magic_event (NULL, XM_BUMPQUEUE);
	  return (HDDEDATA) DDE_FACK;
	}
      else if (!DdeCmpStringHandles (hszTopic, mswindows_dde_topic_system))
	{
	  DWORD len = DdeGetData (hdata, NULL, 0, 0);
	  LPBYTE extcmd = (LPBYTE) ALLOCA (len + 1);
	  Ibyte *cmd;
	  Ibyte *end;
	  struct gcpro gcpro1, gcpro2;
	  Lisp_Object l_dndlist = Qnil;
	  Lisp_Object emacs_event = Fmake_event (Qnil, Qnil);
	  Lisp_Object frmcons, devcons, concons;
	  Lisp_Event *event = XEVENT (emacs_event);

	  DdeGetData (hdata, extcmd, len, 0);
	  DdeFreeDataHandle (hdata);

	  TO_INTERNAL_FORMAT (DATA, (extcmd, len),
			      C_STRING_ALLOCA, cmd,
			      Qmswindows_tstr);

	  /* Check syntax & that it's an [Open("foo")] command, which we
	   * treat like a file drop */
	  if (*cmd == '[')
	    cmd++;
	  if (qxestrncasecmp_c (cmd, MSWINDOWS_DDE_ITEM_OPEN,
				strlen (MSWINDOWS_DDE_ITEM_OPEN)))
	    return DDE_FNOTPROCESSED;
	  cmd += strlen (MSWINDOWS_DDE_ITEM_OPEN);
	  while (*cmd == ' ')
	    cmd++;
	  if (*cmd != '(' || *(cmd + 1) != '\"')
	    return DDE_FNOTPROCESSED;
	  end = (cmd += 2);
	  while (*end && *end != '\"')
	    end++;
	  if (!*end)
	    return DDE_FNOTPROCESSED;
	  *end = '\0';
	  if (*++end != ')')
	    return DDE_FNOTPROCESSED;
	  if (*++end == ']')
	    end++;
	  if (*end)
	    return DDE_FNOTPROCESSED;

	  {
	    /* The drag-n-drop code in dragdrop.el expects pseudo-URL's,
	       consisting of just file: followed by the filename.  This
	       should maybe work, but both Netscape and IE complain
	       whenever they're not given the full file spec, like

	       file:///C|/foo/bar/  or equivalently
	       file:///C:/foo/bar/  (less portably)

	       they don't allow relative paths at all!  this is way bogus. */
	    cmd = urlify_filename (cmd);
	    l_dndlist = build_intstring (cmd);
	    xfree (cmd);
	  }
	  GCPRO2 (emacs_event, l_dndlist);

	  /* Find a mswindows frame */
	  event->channel = Qnil;
	  FRAME_LOOP_NO_BREAK (frmcons, devcons, concons)
	    {
	      Lisp_Object frame = XCAR (frmcons);
	      if (FRAME_TYPE_P (XFRAME (frame), mswindows))
		event->channel = frame;
	    };
	  assert (!NILP (event->channel));

	  SET_EVENT_TIMESTAMP (event, GetTickCount());
	  SET_EVENT_TYPE (event, misc_user_event);
	  SET_EVENT_MISC_USER_BUTTON (event, 1);
	  SET_EVENT_MISC_USER_MODIFIERS (event, 0);
	  SET_EVENT_MISC_USER_X (event, -1);
	  SET_EVENT_MISC_USER_Y (event, -1);
	  SET_EVENT_MISC_USER_FUNCTION (event,
					Qdragdrop_drop_dispatch);
	  SET_EVENT_MISC_USER_OBJECT (event,
				      Fcons (Qdragdrop_URL,
					    Fcons (l_dndlist, Qnil)));
	  mswindows_enqueue_dispatch_event (emacs_event);
	  UNGCPRO;
	  return (HDDEDATA) DDE_FACK;
	}
      DdeFreeDataHandle (hdata);
      return (HDDEDATA) DDE_FNOTPROCESSED;

    default:
      return (HDDEDATA) NULL;
    }
}
#endif

/*
 * Helper to do repainting - repaints can happen both from the windows
 * procedure and from magic events
 */
static void
mswindows_handle_paint (struct frame *frame)
{
  HWND hwnd = FRAME_MSWINDOWS_HANDLE (frame);

  /* According to the docs we need to check GetUpdateRect() before
     actually doing a WM_PAINT */
  if (GetUpdateRect (hwnd, NULL, FALSE))
    {
      PAINTSTRUCT paintStruct;
      int x, y, width, height;

      BeginPaint (hwnd, &paintStruct);
      x = paintStruct.rcPaint.left;
      y = paintStruct.rcPaint.top;
      width = paintStruct.rcPaint.right - paintStruct.rcPaint.left;
      height = paintStruct.rcPaint.bottom - paintStruct.rcPaint.top;
      /* Normally we want to ignore expose events when child
	 windows are unmapped, however once we are in the guts of
	 WM_PAINT we need to make sure that we don't register
	 unmaps then because they will not actually occur. */
      /* #### commenting out the next line seems to fix some problems
	 but not all.  only andy currently understands this stuff and
	 he needs to review it more carefully. --ben */
      if (!check_for_ignored_expose (frame, x, y, width, height))
	{
	  hold_ignored_expose_registration = 1;
	  mswindows_redraw_exposed_area (frame, x, y, width, height);
	  hold_ignored_expose_registration = 0;
	}
      EndPaint (hwnd, &paintStruct);
    }
}

/*
 * Returns 1 if a key is a real modifier or special key, which
 * is better handled by DefWindowProc
 */
static int
key_needs_default_processing_p (UINT vkey)
{
  if (mswindows_alt_by_itself_activates_menu && vkey == VK_MENU
      /* if we let ALT activate the menu like this, then sticky ALT-modified
	 keystrokes become impossible. */
      && !modifier_keys_are_sticky)
    return 1;

  return 0;
}

/* key-handling code is always ugly.  It just ends up working out
   that way.

   #### Most of the sticky-modifier code below is copied from similar
   code in event-Xt.c.  They should somehow or other be merged.

   Here are some pointers:

   -- DOWN_MASK indicates which modifiers should be treated as "down"
      when the corresponding upstroke happens.  It gets reset for
      a particular modifier when that modifier goes up, and reset
      for all modifiers when a non-modifier key is pressed.  Example:

      I press Control-A-Shift and then release Control-A-Shift.
      I want the Shift key to be sticky but not the Control key.

   -- If a modifier key is sticky, I can unstick it by pressing
      the modifier key again. */

static WPARAM last_downkey;
static int need_to_add_mask, down_mask;

#define XEMSW_LCONTROL (1<<0)
#define XEMSW_RCONTROL (1<<1)
#define XEMSW_LSHIFT (1<<2)
#define XEMSW_RSHIFT (1<<3)
#define XEMSW_LMENU (1<<4)
#define XEMSW_RMENU (1<<5)

static int
mswindows_handle_sticky_modifiers (WPARAM wParam, LPARAM lParam,
				   int downp, int keyp)
{
  int mods = 0;

  if (!modifier_keys_are_sticky) /* Optimize for non-sticky modifiers */
    return 0;

  if (! (keyp &&
	 (wParam == VK_CONTROL || wParam == VK_LCONTROL ||
	  wParam == VK_RCONTROL ||
	  wParam == VK_MENU || wParam == VK_LMENU ||
	  wParam == VK_RMENU ||
	  wParam == VK_SHIFT || wParam == VK_LSHIFT ||
	  wParam == VK_RSHIFT)))
    { /* Not a modifier key */
      if (downp && keyp && !last_downkey)
	last_downkey = wParam;
      /* If I hold press-and-release the Control key and then press
	 and hold down the right arrow, I want it to auto-repeat
	 Control-Right.  On the other hand, if I do the same but
	 manually press the Right arrow a bunch of times, I want
	 to see one Control-Right and then a bunch of Rights.
	 This means that we need to distinguish between an
	 auto-repeated key and a key pressed and released a bunch
	 of times. */
      else if ((downp && !keyp) ||
	       (downp && keyp && last_downkey &&
		(wParam != last_downkey ||
		 /* the "previous key state" bit indicates autorepeat */
		 ! (lParam & (1 << 30)))))
	{
	  need_to_add_mask = 0;
	  last_downkey = 0;
	}
      if (downp)
	down_mask = 0;

      mods = need_to_add_mask;
    }
  else                          /* Modifier key pressed */
    {
      /* If a non-modifier key was pressed in the middle of a bunch
	 of modifiers, then it unsticks all the modifiers that were
	 previously pressed.  We cannot unstick the modifiers until
	 now because we want to check for auto-repeat of the
	 non-modifier key. */

      if (last_downkey)
	{
	  last_downkey = 0;
	  need_to_add_mask = 0;
	}

#define FROB(mask)				\
do {						\
  if (downp && keyp)				\
    {						\
      /* If modifier key is already sticky,	\
         then unstick it.  Note that we do	\
         not test down_mask to deal with the	\
	 unlikely but possible case that the	\
	 modifier key auto-repeats. */		\
      if (need_to_add_mask & mask)		\
	{					\
	  need_to_add_mask &= ~mask;		\
	  down_mask &= ~mask;			\
	}					\
      else					\
	down_mask |= mask;			\
    }						\
  else						\
    {						\
      if (down_mask & mask)			\
	{					\
	  down_mask &= ~mask;			\
	  need_to_add_mask |= mask;		\
	}					\
    }						\
} while (0)

      if ((wParam == VK_CONTROL && (lParam & 0x1000000))
	  || wParam == VK_RCONTROL)
	FROB (XEMSW_RCONTROL);
      if ((wParam == VK_CONTROL && !(lParam & 0x1000000))
	  || wParam == VK_LCONTROL)
	FROB (XEMSW_LCONTROL);

      if ((wParam == VK_SHIFT && (lParam & 0x1000000))
	  || wParam == VK_RSHIFT)
	FROB (XEMSW_RSHIFT);
      if ((wParam == VK_SHIFT && !(lParam & 0x1000000))
	  || wParam == VK_LSHIFT)
	FROB (XEMSW_LSHIFT);

      if ((wParam == VK_MENU && (lParam & 0x1000000))
	  || wParam == VK_RMENU)
	FROB (XEMSW_RMENU);
      if ((wParam == VK_MENU && !(lParam & 0x1000000))
	  || wParam == VK_LMENU)
	FROB (XEMSW_LMENU);
    }
#undef FROB

  if (mods && downp)
    {
      BYTE keymap[256];

      GetKeyboardState (keymap);

      if (mods & XEMSW_LCONTROL)
	{
	  keymap [VK_CONTROL] |= 0x80;
	  keymap [VK_LCONTROL] |= 0x80;
	}
      if (mods & XEMSW_RCONTROL)
	{
	  keymap [VK_CONTROL] |= 0x80;
	  keymap [VK_RCONTROL] |= 0x80;
	}

      if (mods & XEMSW_LSHIFT)
	{
	  keymap [VK_SHIFT] |= 0x80;
	  keymap [VK_LSHIFT] |= 0x80;
	}
      if (mods & XEMSW_RSHIFT)
	{
	  keymap [VK_SHIFT] |= 0x80;
	  keymap [VK_RSHIFT] |= 0x80;
	}

      if (mods & XEMSW_LMENU)
	{
	  keymap [VK_MENU] |= 0x80;
	  keymap [VK_LMENU] |= 0x80;
	}
      if (mods & XEMSW_RMENU)
	{
	  keymap [VK_MENU] |= 0x80;
	  keymap [VK_RMENU] |= 0x80;
	}

      SetKeyboardState (keymap);
      return 1;
    }

  return 0;
}

static void
clear_sticky_modifiers (void)
{
  need_to_add_mask = 0;
  last_downkey     = 0;
  down_mask        = 0;
}

#ifdef DEBUG_XEMACS

#if 0

static void
output_modifier_keyboard_state (void)
{
  BYTE keymap[256];

  GetKeyboardState (keymap);

  stderr_out ("GetKeyboardState VK_MENU %d %d VK_LMENU %d %d VK_RMENU %d %d\n",
	      keymap[VK_MENU] & 0x80 ? 1 : 0,
	      keymap[VK_MENU] & 0x1 ? 1 : 0,
	      keymap[VK_LMENU] & 0x80 ? 1 : 0,
	      keymap[VK_LMENU] & 0x1 ? 1 : 0,
	      keymap[VK_RMENU] & 0x80 ? 1 : 0,
	      keymap[VK_RMENU] & 0x1 ? 1 : 0);
  stderr_out ("GetKeyboardState VK_CONTROL %d %d VK_LCONTROL %d %d VK_RCONTROL %d %d\n",
	      keymap[VK_CONTROL] & 0x80 ? 1 : 0,
	      keymap[VK_CONTROL] & 0x1 ? 1 : 0,
	      keymap[VK_LCONTROL] & 0x80 ? 1 : 0,
	      keymap[VK_LCONTROL] & 0x1 ? 1 : 0,
	      keymap[VK_RCONTROL] & 0x80 ? 1 : 0,
	      keymap[VK_RCONTROL] & 0x1 ? 1 : 0);
  stderr_out ("GetKeyboardState VK_SHIFT %d %d VK_LSHIFT %d %d VK_RSHIFT %d %d\n",
	      keymap[VK_SHIFT] & 0x80 ? 1 : 0,
	      keymap[VK_SHIFT] & 0x1 ? 1 : 0,
	      keymap[VK_LSHIFT] & 0x80 ? 1 : 0,
	      keymap[VK_LSHIFT] & 0x1 ? 1 : 0,
	      keymap[VK_RSHIFT] & 0x80 ? 1 : 0,
	      keymap[VK_RSHIFT] & 0x1 ? 1 : 0);
}

#endif

/* try to debug the stuck-alt-key problem.

 #### this happens only inconsistently, and may only happen when using
 StickyKeys in the Win2000 accessibility section of the control panel,
 which is extremely broken for other reasons.  */

static void
output_alt_keyboard_state (void)
{
  BYTE keymap[256];
  SHORT keystate[3];
  // SHORT asyncstate[3];

  GetKeyboardState (keymap);
  keystate[0] = GetKeyState (VK_MENU);
  keystate[1] = GetKeyState (VK_LMENU);
  keystate[2] = GetKeyState (VK_RMENU);
  /* Doing this interferes with key processing. */
/*   asyncstate[0] = GetAsyncKeyState (VK_MENU); */
/*   asyncstate[1] = GetAsyncKeyState (VK_LMENU); */
/*   asyncstate[2] = GetAsyncKeyState (VK_RMENU); */

  stderr_out ("GetKeyboardState VK_MENU %d %d VK_LMENU %d %d VK_RMENU %d %d\n",
	      keymap[VK_MENU] & 0x80 ? 1 : 0,
	      keymap[VK_MENU] & 0x1 ? 1 : 0,
	      keymap[VK_LMENU] & 0x80 ? 1 : 0,
	      keymap[VK_LMENU] & 0x1 ? 1 : 0,
	      keymap[VK_RMENU] & 0x80 ? 1 : 0,
	      keymap[VK_RMENU] & 0x1 ? 1 : 0);
  stderr_out ("GetKeyState VK_MENU %d %d VK_LMENU %d %d VK_RMENU %d %d\n",
	      keystate[0] & 0x8000 ? 1 : 0,
	      keystate[0] & 0x1 ? 1 : 0,
	      keystate[1] & 0x8000 ? 1 : 0,
	      keystate[1] & 0x1 ? 1 : 0,
	      keystate[2] & 0x8000 ? 1 : 0,
	      keystate[2] & 0x1 ? 1 : 0);
/*   stderr_out ("GetAsyncKeyState VK_MENU %d %d VK_LMENU %d %d VK_RMENU %d %d\n", */
/* 	      asyncstate[0] & 0x8000 ? 1 : 0, */
/* 	      asyncstate[0] & 0x1 ? 1 : 0, */
/* 	      asyncstate[1] & 0x8000 ? 1 : 0, */
/* 	      asyncstate[1] & 0x1 ? 1 : 0, */
/* 	      asyncstate[2] & 0x8000 ? 1 : 0, */
/* 	      asyncstate[2] & 0x1 ? 1 : 0); */
}

#endif /* DEBUG_XEMACS */


/*
 * The windows procedure for the window class XEMACS_CLASS
 */
LRESULT WINAPI
mswindows_wnd_proc (HWND hwnd, UINT message_, WPARAM wParam, LPARAM lParam)
{
  /* Note: Remember to initialize emacs_event and event before use.  This
     code calls code that can GC. You must GCPRO before calling such
     code. */
  Lisp_Object emacs_event = Qnil;
  Lisp_Object fobj = Qnil;

  Lisp_Event *event;
  struct frame *frame;
  struct mswindows_frame *msframe;

  /* If you hit this, rewrite the offending API call to occur after GC,
     using register_post_gc_action(). */
  assert (!gc_in_progress);

#ifdef DEBUG_XEMACS
  if (debug_mswindows_events)
    debug_output_mswin_message (hwnd, message_, wParam, lParam);
#endif /* DEBUG_XEMACS */

  assert (!qxeGetWindowLong (hwnd, GWL_USERDATA));
  switch (message_)
    {
    case WM_DESTROYCLIPBOARD:
      mswindows_handle_destroyclipboard ();
      break;

    case WM_ERASEBKGND:
      /* Erase background only during non-dynamic sizing */
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      if (msframe->sizing && !mswindows_dynamic_frame_resize)
	goto defproc;
      return 1;

    case WM_CLOSE:
      fobj = mswindows_find_frame (hwnd);
      mswindows_enqueue_misc_user_event (fobj, Qeval, list3 (Qdelete_frame, fobj,
							     Qt));
      break;

    case WM_KEYUP:
    case WM_SYSKEYUP:

      /* See Win95 comment under WM_KEYDOWN */
      {
	BYTE keymap[256];
	int should_set_keymap = 0;

#ifdef DEBUG_XEMACS
	if (debug_mswindows_events > 2)
	  output_alt_keyboard_state ();
#endif /* DEBUG_XEMACS */

	mswindows_handle_sticky_modifiers (wParam, lParam, 0, 1);
	if (wParam == VK_CONTROL)
	  {
	    GetKeyboardState (keymap);
	    keymap [(lParam & 0x1000000) ? VK_RCONTROL : VK_LCONTROL] &= ~0x80;
	    should_set_keymap = 1;
	  }
	else if (wParam == VK_MENU)
	  {
	    GetKeyboardState (keymap);
	    keymap [(lParam & 0x1000000) ? VK_RMENU : VK_LMENU] &= ~0x80;
	    should_set_keymap = 1;
	  }

	if (should_set_keymap)
	  //	    && (message_ != WM_SYSKEYUP
	  //	|| NILP (Vmenu_accelerator_enabled)))
	  SetKeyboardState (keymap);

      }

      if (key_needs_default_processing_p (wParam))
	goto defproc;
      else
	break;

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:

      /* In some locales the right-hand Alt key is labelled AltGr. This key
       * should produce alternative characters when combined with another key.
       * eg on a German keyboard pressing AltGr+q should produce '@'.
       * AltGr generates exactly the same keystrokes as LCtrl+RAlt. But if
       * TranslateMessage() is called with *any* combination of Ctrl+Alt down,
       * it translates as if AltGr were down.
       * We get round this by removing all modifiers from the keymap before
       * calling TranslateMessage() unless AltGr is *really* down. */
      {
	BYTE keymap_trans[256];
	BYTE keymap_orig[256];
	BYTE keymap_sticky[256];
	/* WARNING: XEmacs code paths are far more subtle than you
	   think.  In particular, QUIT checking will query and remove
	   events, including keyboard events, from the queue. (QUIT is
	   definitely invoked from TO_INTERNAL_FORMAT().) If we do
	   this recursively anywhere in the following code, it will
	   mess certain things up -- in particular, the OS-provided
	   sticky modifier code available as part of the accessibility
	   package.

	   (Academic question: If QUIT checking is supposed to be
	   triggered only every 1/4 second, why is it getting
	   consistently triggered here?  I saw the problem
	   consistently.  Answer: It appears that, currently,
	   sometimes the code to pump messages is wrapped with
	   begin_dont_check_for_quit() and sometimes it isn't. (####
	   FIX THIS SHIT!) cmdloop.c, for example, has it, but not
	   everywhere.  The current games with avoiding QUIT mean that
	   the 1/4-second timer consistently fires while
	   dont_check_for_quit is set [which causes the quit check to
	   get deferred but the flag is still on], and so the next
	   time it's unset and we call QUIT is *right here*.

	   In my stderr-proc ws I majorly cleaned up the whole shit by
	   just wrapping all the entry points in dont_check_for_quit.
	   This fixed the remaining bugs with C-g getting interpreted
	   wrong.)

	   #### We should probably wrap this whole function in
	   begin_dont_check_for_quit(); but then we should set this
	   back to 0 when handling a menu callback, which gets invoked
	   from within this function, specifically from
	   DefWindowProc(). (We already do the latter in my new
	   stderr-proc ws, because in that ws next_event_internal()
	   calls begin_dont_check_for_quit(). */

	int count = begin_dont_check_for_quit ();
	int has_AltGr = mswindows_current_layout_has_AltGr ();
	int mods = 0, mods_with_shift = 0;
	int extendedp = lParam & 0x1000000;
	Lisp_Object keysym;
	int sticky_changed;

#ifdef DEBUG_XEMACS
	if (debug_mswindows_events > 2)
	  output_alt_keyboard_state ();
#endif /* DEBUG_XEMACS */

	GetKeyboardState (keymap_orig);
	frame = XFRAME (mswindows_find_frame (hwnd));
	if ((sticky_changed =
	     mswindows_handle_sticky_modifiers (wParam, lParam, 1, 1)))
	  {
	    GetKeyboardState (keymap_sticky);
	    if (keymap_sticky[VK_MENU] & 0x80)
	      {
		message_ = WM_SYSKEYDOWN;
		/* We have to set the "context bit" so that the
		   TranslateMessage() call below that generates the
		   SYSCHAR message does its thing; see the documentation
		   on WM_SYSKEYDOWN */
		lParam |= 1 << 29;
	      }
	  }
	else
	  memcpy (keymap_sticky, keymap_orig, 256);

	mods = mswindows_modifier_state (keymap_sticky, (DWORD) -1, has_AltGr);
	mods_with_shift = mods;

	/* Handle non-printables */
	if (!NILP (keysym = mswindows_key_to_emacs_keysym (wParam, mods,
							   extendedp)))
	  {
	    mswindows_enqueue_keypress_event (hwnd, keysym, mods);
	    if (sticky_changed)
	      SetKeyboardState (keymap_orig);
	  }
	else	/* Normal keys & modifiers */
	  {
	    POINT pnt = { LOWORD (GetMessagePos()), HIWORD (GetMessagePos()) };
	    MSG msg, tranmsg;
#ifdef HAVE_MENUBARS
	    int potential_accelerator = 0;
#endif
	    int got_accelerator = 0;
	    /* No need to gcpro because the event is already on a
               queue when we retrieve it. */
	    Lisp_Object lastev = Qnil;

	    msg.hwnd = hwnd;
	    msg.message = message_;
	    msg.wParam = wParam;
	    msg.lParam = lParam;
	    msg.time = GetMessageTime();
	    msg.pt = pnt;

	    /* GetKeyboardState() does not work as documented on Win95. We have
	     * to loosely track Left and Right modifiers on behalf of the OS,
	     * without screwing up Windows NT which tracks them properly. */
	    if (wParam == VK_CONTROL)
	      {
		keymap_orig[extendedp ? VK_RCONTROL : VK_LCONTROL] |= 0x80;
		keymap_sticky[extendedp ? VK_RCONTROL : VK_LCONTROL] |= 0x80;
	      }
	    else if (wParam == VK_MENU)
	      {
		keymap_orig[extendedp ? VK_RMENU : VK_LMENU] |= 0x80;
		keymap_sticky[extendedp ? VK_RMENU : VK_LMENU] |= 0x80;
	      }

#ifdef HAVE_MENUBARS
	    if (!NILP (Vmenu_accelerator_enabled) &&
		!(mods & XEMACS_MOD_SHIFT) && message_ == WM_SYSKEYDOWN)
	      potential_accelerator = 1;
#endif

	    /* Remove shift modifier from an ascii character */
	    mods &= ~XEMACS_MOD_SHIFT;

	    memcpy (keymap_trans, keymap_sticky, 256);

	    /* Clear control and alt modifiers unless AltGr is pressed */
	    keymap_trans[VK_RCONTROL] = 0;
	    keymap_trans[VK_LMENU] = 0;
	    if (!has_AltGr || !(keymap_trans[VK_LCONTROL] & 0x80)
		|| !(keymap_trans[VK_RMENU] & 0x80))
	      {
		keymap_trans[VK_LCONTROL] = 0;
		keymap_trans[VK_CONTROL] = 0;
		keymap_trans[VK_RMENU] = 0;
		keymap_trans[VK_MENU] = 0;
	      }
	    SetKeyboardState (keymap_trans);

	    /* Maybe generate some WM_[SYS]CHARs in the queue */
	    TranslateMessage (&msg);

	    while (qxePeekMessage (&tranmsg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE)
		   || qxePeekMessage (&tranmsg, hwnd, WM_SYSCHAR, WM_SYSCHAR,
				      PM_REMOVE))
	      {
		int mods_with_quit = mods;
		int length;
		Extbyte extchar[4];
		Ibyte *intchar;
		Ichar ch;

		if (XEUNICODE_P)
		  {
		    length = unicode_char_to_text (tranmsg.wParam, extchar);
		    TO_INTERNAL_FORMAT (DATA, (extchar, length),
					C_STRING_ALLOCA, (intchar),
					Qmswindows_unicode);
		    ch = itext_ichar (intchar);
		  }
		else
		  {
		    length = ansi_char_to_text (tranmsg.wParam, extchar);
		    intchar = (convert_multibyte_to_internal_malloc
			       (extchar, length,
				mswindows_locale_to_code_page
				/* See intl-win32.c for an explanation of
				   the following */
				((LCID) GetKeyboardLayout (0) & 0xFFFF),
				NULL));
		    ch = itext_ichar (intchar);
		    xfree (intchar);
		  }

#ifdef DEBUG_XEMACS
		if (debug_mswindows_events)
		  {
		    stderr_out ("-> ");
		    debug_output_mswin_message (tranmsg.hwnd, tranmsg.message,
						tranmsg.wParam,
						tranmsg.lParam);
		  }
#endif /* DEBUG_XEMACS */

#ifdef HAVE_MENUBARS
		if (potential_accelerator && !got_accelerator &&
		    mswindows_char_is_accelerator (frame, ch))
		  {
		    got_accelerator = 1;
		    break;
		  }
#endif /* HAVE_MENUBARS */

		lastev = mswindows_enqueue_keypress_event (hwnd,
							   make_char (ch),
							   mods_with_quit);
	      } /* while */

#ifdef MULE
	    /* Also figure out what the character would be in other
	       possible keyboard layouts, in this order:

	       -- current language environment
	       -- user default language environment
	       -- system default language environment
	       -- same three, but checking the underlying virtual key,
   	          and only paying attention if it's alphabetic
	       -- US ASCII

	       See events.h, struct key_data, for why we do this.
	       */

	    if (!NILP (lastev))
	      {
		int i;
		int scan = (lParam >> 16) && 0xFF;

		for (i = 0; i < KEYCHAR_LAST; i++)
		  {
		    int vk_only = 0;
		    LCID lcid;
		    int virtual_key;

		    switch (i)
		      {
		      case KEYCHAR_UNDERLYING_VIRTUAL_KEY_CURRENT_LANGENV:
			vk_only = 1;
		      case KEYCHAR_CURRENT_LANGENV:
			lcid = mswindows_current_locale ();
			break;

		      case KEYCHAR_UNDERLYING_VIRTUAL_KEY_DEFAULT_USER:
			vk_only = 1;
		      case KEYCHAR_DEFAULT_USER:
			lcid = GetUserDefaultLCID ();
			break;

		      case KEYCHAR_UNDERLYING_VIRTUAL_KEY_DEFAULT_SYSTEM:
			vk_only = 1;
		      case KEYCHAR_DEFAULT_SYSTEM:
			lcid = GetSystemDefaultLCID ();
			break;

		      case KEYCHAR_QWERTY:
			lcid = MAKELANGID (LANG_ENGLISH, SUBLANG_ENGLISH_US);
			break;

		      default: abort (); lcid = 0;
		      }

		    /* VERY CONFUSING!  See intl-win32.c. */
		    lcid = lcid & 0xFFFF;

		    virtual_key = qxeMapVirtualKeyEx (scan, 1, (HKL) lcid);
		    if (!vk_only)
		      {
			if (XEUNICODE_P)
			  {
			    Extbyte received_keys[32];
			    int tounret =
			      ToUnicodeEx
				(virtual_key, scan, keymap_trans,
				 (LPWSTR) received_keys,
				 sizeof (received_keys) / XETCHAR_SIZE,
				 0, /* #### what about this flag? "if
				       bit 0 is set, a menu is
				       active???" */
				 (HKL) lcid);
			    if (tounret > 0)
			      {
				Ibyte *intchar;

				TO_INTERNAL_FORMAT
				  (DATA,
				   (received_keys + (tounret - 1) * 2, 2),
				   C_STRING_ALLOCA, intchar,
				   Qmswindows_unicode);
				XSET_EVENT_KEY_ALT_KEYCHARS
				  (lastev, i, itext_ichar (intchar));
			      }
			  }
			else
			  {
			    WORD received_keys[32];
			    int tounret =
			      ToAsciiEx (virtual_key, scan, keymap_trans,
					 received_keys,
					 0, /* #### what about this
					       flag? "if bit 0 is set, a
					       menu is active???" */
					 (HKL) lcid);
			    if (tounret > 0)
			      {
				/* #### I cannot find proper
				   documentation on what format the
				   return value is in.  I'm assuming
				   it's like WM_IME_CHAR: DBCS chars
				   have the lead byte in bits 8-15 of
				   the short. */
				Ibyte *intchar;
				Extbyte mbstuff[2];
				Bytecount mblength = 0;
				WORD thechar = received_keys[tounret - 1];

				mbstuff[mblength++] =
				  (Extbyte) (thechar & 0xFF);
				if (thechar > 0xFF)
				  mbstuff[mblength++] =
				    (Extbyte) ((thechar >> 8) & 0xFF);

				intchar = convert_multibyte_to_internal_malloc
				  (mbstuff, mblength,
				   mswindows_locale_to_code_page (lcid),
				   NULL);

				XSET_EVENT_KEY_ALT_KEYCHARS
				  (lastev, i, itext_ichar (intchar));
				xfree (intchar);
			      }
			  }
		      }
		    else
		      {
			Ichar altch;

			if (virtual_key >= 'A' && virtual_key <= 'Z')
			  altch =
			    virtual_key + (mods_with_shift & XEMACS_MOD_SHIFT ?
					   'a' - 'A' : 0);
			else
			  altch = 0;

			XSET_EVENT_KEY_ALT_KEYCHARS (lastev, i, altch);
		      }
		  }
	      }
#endif /* MULE */

	    /* This generates WM_SYSCHAR messages, which are interpreted
	       by DefWindowProc as the menu selections. */
	    if (got_accelerator)
	      {
		SetKeyboardState (keymap_sticky);
		TranslateMessage (&msg);
		SetKeyboardState (keymap_orig);
		unbind_to (count);
		goto defproc;
	      }

	    SetKeyboardState (keymap_orig);
	  } /* else */

	if (key_needs_default_processing_p (wParam))
	  {
	    unbind_to (count);
	    goto defproc;
	  }
	else
	  {
	    unbind_to (count);
	    break;
	  }
      }

    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
      /* Real middle mouse button has nothing to do with emulated one:
	 if one wants to exercise fingers playing chords on the mouse,
	 he is allowed to do that! */
      mswindows_enqueue_mouse_button_event (hwnd, message_,
					    MAKEPOINTS (lParam),
					    wParam &~ MK_MBUTTON,
					    GetMessageTime());
      break;

    case WM_LBUTTONUP:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      msframe->last_click_time = GetMessageTime();

      KillTimer (hwnd, BUTTON_2_TIMER_ID);
      msframe->button2_need_lbutton = 0;
      if (msframe->ignore_next_lbutton_up)
	{
	  msframe->ignore_next_lbutton_up = 0;
	}
      else if (msframe->button2_is_down)
	{
	  msframe->button2_is_down = 0;
	  msframe->ignore_next_rbutton_up = 1;
	  mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONUP,
						MAKEPOINTS (lParam),
						wParam
						&~ (MK_LBUTTON | MK_MBUTTON
						    | MK_RBUTTON),
						GetMessageTime());
	}
      else
	{
	  if (msframe->button2_need_rbutton)
	    {
	      msframe->button2_need_rbutton = 0;
	      mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam &~ MK_LBUTTON,
						    GetMessageTime());
	    }
	  mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONUP,
						MAKEPOINTS (lParam),
						wParam &~ MK_LBUTTON,
						GetMessageTime());
	}
      break;

    case WM_RBUTTONUP:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      msframe->last_click_time = GetMessageTime();

      KillTimer (hwnd, BUTTON_2_TIMER_ID);
      msframe->button2_need_rbutton = 0;
      if (msframe->ignore_next_rbutton_up)
	{
	  msframe->ignore_next_rbutton_up = 0;
	}
      else if (msframe->button2_is_down)
	{
	  msframe->button2_is_down = 0;
	  msframe->ignore_next_lbutton_up = 1;
	  mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONUP,
						MAKEPOINTS (lParam),
						wParam
						&~ (MK_LBUTTON | MK_MBUTTON
						    | MK_RBUTTON),
						GetMessageTime());
	}
      else
	{
	  if (msframe->button2_need_lbutton)
	    {
	      msframe->button2_need_lbutton = 0;
	      mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam &~ MK_RBUTTON,
						    GetMessageTime());
	    }
	  mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONUP,
						MAKEPOINTS (lParam),
						wParam &~ MK_RBUTTON,
						GetMessageTime());
	}
      break;

    case WM_LBUTTONDOWN:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));

      if (msframe->button2_need_lbutton)
	{
	  KillTimer (hwnd, BUTTON_2_TIMER_ID);
	  msframe->button2_need_lbutton = 0;
	  msframe->button2_need_rbutton = 0;
	  if (mswindows_button2_near_enough (msframe->last_click_point,
					     MAKEPOINTS (lParam)))
	    {
	      mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam
						    &~ (MK_LBUTTON | MK_MBUTTON
							| MK_RBUTTON),
						    GetMessageTime());
	      msframe->button2_is_down = 1;
	    }
	  else
	    {
	      mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						    msframe->last_click_point,
						    msframe->last_click_mods
						    &~ MK_RBUTTON,
						    msframe->last_click_time);
	      mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam &~ MK_LBUTTON,
						    GetMessageTime());
	    }
	}
      else
	{
	  mswindows_set_chord_timer (hwnd);
	  msframe->button2_need_rbutton = 1;
	  msframe->last_click_point = MAKEPOINTS (lParam);
	  msframe->last_click_mods = wParam;
	}
      msframe->last_click_time = GetMessageTime();
      break;

    case WM_RBUTTONDOWN:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));

      if (msframe->button2_need_rbutton)
	{
	  KillTimer (hwnd, BUTTON_2_TIMER_ID);
	  msframe->button2_need_lbutton = 0;
	  msframe->button2_need_rbutton = 0;
	  if (mswindows_button2_near_enough (msframe->last_click_point,
					     MAKEPOINTS (lParam)))
	    {
	      mswindows_enqueue_mouse_button_event (hwnd, WM_MBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam
						    &~ (MK_LBUTTON | MK_MBUTTON
							| MK_RBUTTON),
						    GetMessageTime());
	      msframe->button2_is_down = 1;
	    }
	  else
	    {
	      mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						    msframe->last_click_point,
						    msframe->last_click_mods
						    &~ MK_LBUTTON,
						    msframe->last_click_time);
	      mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						    MAKEPOINTS (lParam),
						    wParam &~ MK_RBUTTON,
						    GetMessageTime());
	    }
	}
      else
	{
	  mswindows_set_chord_timer (hwnd);
	  msframe->button2_need_lbutton = 1;
	  msframe->last_click_point = MAKEPOINTS (lParam);
	  msframe->last_click_mods = wParam;
	}
      msframe->last_click_time = GetMessageTime();
      break;

    case WM_TIMER:
      if (wParam == BUTTON_2_TIMER_ID)
	{
	  msframe =
	    FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
	  KillTimer (hwnd, BUTTON_2_TIMER_ID);

	  if (msframe->button2_need_lbutton)
	    {
	      msframe->button2_need_lbutton = 0;
	      mswindows_enqueue_mouse_button_event (hwnd, WM_RBUTTONDOWN,
						    msframe->last_click_point,
						    msframe->last_click_mods
						    &~ MK_RBUTTON,
						    msframe->last_click_time);
	    }
	  else if (msframe->button2_need_rbutton)
	    {
	      msframe->button2_need_rbutton = 0;
	      mswindows_enqueue_mouse_button_event (hwnd, WM_LBUTTONDOWN,
						    msframe->last_click_point,
						    msframe->last_click_mods
						    &~ MK_LBUTTON,
						    msframe->last_click_time);
	    }
	}
      else
	assert ("Spurious timer fired" == 0);
      break;

    case WM_MOUSEMOVE:
      /* Optimization: don't report mouse movement while size is changing */
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      if (!msframe->sizing)
	{
	  /* When waiting for the second mouse button to finish
	     button2 emulation, and have moved too far, just pretend
	     as if timer has expired. This improves drag-select feedback */
	  if ((msframe->button2_need_lbutton || msframe->button2_need_rbutton)
	      && !mswindows_button2_near_enough (msframe->last_click_point,
						 MAKEPOINTS (lParam)))
	    {
	      KillTimer (hwnd, BUTTON_2_TIMER_ID);
	      qxeSendMessage (hwnd, WM_TIMER, BUTTON_2_TIMER_ID, 0);
	    }

	  emacs_event = Fmake_event (Qnil, Qnil);
	  event = XEVENT(emacs_event);

	  XSET_EVENT_CHANNEL (emacs_event,  mswindows_find_frame(hwnd));
	  XSET_EVENT_TIMESTAMP (emacs_event, GetMessageTime());
	  XSET_EVENT_TYPE (emacs_event, pointer_motion_event);
	  XSET_EVENT_MOTION_X (emacs_event,MAKEPOINTS (lParam).x);
	  XSET_EVENT_MOTION_Y (emacs_event,MAKEPOINTS (lParam).y);
	  XSET_EVENT_MOTION_MODIFIERS (emacs_event,
                                mswindows_modifier_state (NULL, wParam, 0));

	  mswindows_enqueue_dispatch_event (emacs_event);
	}
      break;

    case WM_CANCELMODE:
      ReleaseCapture ();
      /* Queue a `cancel-mode-internal' misc user event, so mouse
	 selection would be canceled if any */
      mswindows_enqueue_misc_user_event (mswindows_find_frame (hwnd),
					 Qcancel_mode_internal, Qnil);
      break;

    case WM_NOTIFY:
      {
	LPNMHDR nmhdr = (LPNMHDR) lParam;

	if (nmhdr->code == TTN_NEEDTEXT)
	  {
#ifdef HAVE_TOOLBARS
	    LPTOOLTIPTEXTW tttextw = (LPTOOLTIPTEXTW) lParam;
	    Lisp_Object btext;
	    Extbyte *btextext = 0;

	    /* find out which toolbar */
	    frame = XFRAME (mswindows_find_frame (hwnd));
	    btext = mswindows_get_toolbar_button_text (frame, nmhdr->idFrom);

	    tttextw->hinst = NULL;

	    if (!NILP (btext))
	      LISP_STRING_TO_TSTR (btext, btextext);

	    if (btextext)
	      {
		/* WARNING: We can't just write a '\0' into the 79th
                   "character" because tttextw->szText is in WCHAR's but we
                   may be copying an ANSI string into it.  Easiest to just
                   zero the whole thing. */
		xzero (*tttextw->szText);
		xetcsncpy ((Extbyte *) tttextw->szText, btextext, 79);
	      }
	    else
	      tttextw->lpszText = NULL;
#endif
	  }
	/* handle tree view callbacks */
	else if (nmhdr->code == TVN_SELCHANGED)
	  {
	    NM_TREEVIEW *ptree = (NM_TREEVIEW *) lParam;
	    frame = XFRAME (mswindows_find_frame (hwnd));
	    mswindows_handle_gui_wm_command (frame, 0, ptree->itemNew.lParam);
	  }
	/* handle tab control callbacks */
	else if (nmhdr->code == TCN_SELCHANGE)
	  {
	    TC_ITEM item;
	    int idx = qxeSendMessage (nmhdr->hwndFrom, TCM_GETCURSEL, 0, 0);
	    frame = XFRAME (mswindows_find_frame (hwnd));

	    item.mask = TCIF_PARAM;
	    qxeSendMessage (nmhdr->hwndFrom, TCM_GETITEM, (WPARAM) idx,
			    (LPARAM) &item);

	    mswindows_handle_gui_wm_command (frame, 0, item.lParam);
	  }
      }
      break;

    case WM_PAINT:
      /* hdc will be NULL unless this is a subwindow - in which case we
	 shouldn't have received a paint message for it here. */
      assert (wParam == 0);

      /* Can't queue a magic event because windows goes modal and sends paint
	 messages directly to the windows procedure when doing solid drags
	 and the message queue doesn't get processed. */
      mswindows_handle_paint (XFRAME (mswindows_find_frame (hwnd)));
      break;

    case WM_ACTIVATE:
      {
        /*
         * If we receive a WM_ACTIVATE message that indicates that our frame
         * is being activated, make sure that the frame is marked visible
         * if the window itself is visible. This seems to fix the problem
         * where XEmacs appears to lock-up after switching desktops with
         * some virtual window managers.
         */
        int state = (int)(short) LOWORD(wParam);
#ifdef DEBUG_XEMACS
        if (debug_mswindows_events)
          stderr_out("state = %d\n", state);
#endif /* DEBUG_XEMACS */
        if (state == WA_ACTIVE || state == WA_CLICKACTIVE)
          {
#ifdef DEBUG_XEMACS
            if (debug_mswindows_events)
              stderr_out("  activating\n");
#endif /* DEBUG_XEMACS */
            
            fobj = mswindows_find_frame (hwnd);
            frame = XFRAME (fobj);
            if (IsWindowVisible (hwnd))
              {
#ifdef DEBUG_XEMACS
                if (debug_mswindows_events)
                  stderr_out("  window is visible\n");
#endif /* DEBUG_XEMACS */
                if (!FRAME_VISIBLE_P (frame))
                  {
#ifdef DEBUG_XEMACS
                    if (debug_mswindows_events)
                      stderr_out("  frame is not visible\n");
#endif /* DEBUG_XEMACS */
                    /*
                     * It seems that we have to enqueue the XM_MAPFRAME event
                     * prior to setting the frame visible so that
                     * suspend-or-iconify-emacs works properly.
                     */
                    mswindows_enqueue_magic_event (hwnd, XM_MAPFRAME);
                    FRAME_VISIBLE_P (frame) = 1;
                    FRAME_ICONIFIED_P (frame) = 0;
                  }
#ifdef DEBUG_XEMACS
                else
                  {
                    if (debug_mswindows_events)
                      stderr_out("  frame is visible\n");
                  }
#endif /* DEBUG_XEMACS */
              }
#ifdef DEBUG_XEMACS
            else
              {     
                if (debug_mswindows_events)
                  stderr_out("  window is not visible\n");
              }
#endif /* DEBUG_XEMACS */
          }
	return qxeDefWindowProc (hwnd, message_, wParam, lParam);
      }
      break;
      
    case WM_WINDOWPOSCHANGED:
      /* This is sent before WM_SIZE; in fact, the processing of this
	 by DefWindowProc() sends WM_SIZE.  But WM_SIZE is not sent when
	 a window is hidden (make-frame-invisible), so we need to process
	 this and update the state flags. */
      {
	fobj = mswindows_find_frame (hwnd);
	frame = XFRAME (fobj);
	if (IsIconic (hwnd))
	  {
	    FRAME_VISIBLE_P (frame) = 0;
	    FRAME_ICONIFIED_P (frame) = 1;
	  }
	else if (IsWindowVisible (hwnd))
	  {
	    /* APA: It's too early here to set the frame visible.
	     * Let's do this later, in WM_SIZE processing, after the
	     * magic XM_MAPFRAME event has been sent (just like 21.1
	     * did). */
	    /* FRAME_VISIBLE_P (frame) = 1; */
	    FRAME_ICONIFIED_P (frame) = 0;
	  }
	else
	  {
	    FRAME_VISIBLE_P (frame) = 0;
	    FRAME_ICONIFIED_P (frame) = 0;
	  }	    

	goto defproc;
      }

    case WM_SHOWWINDOW:
      /*
         The WM_SHOWWINDOW message is sent to a window when the window
         is about to be hidden or shown.
         APA: This message is also sent when switching to a virtual
         desktop under the virtuawin virtual window manager.
      
      */
      {
	fobj = mswindows_find_frame (hwnd);
	frame = XFRAME (fobj);
        if (wParam == TRUE)
          {
            mswindows_enqueue_magic_event (hwnd, XM_MAPFRAME);
            FRAME_VISIBLE_P (frame) = 1;
          }
        else
          {
            mswindows_enqueue_magic_event (hwnd, XM_UNMAPFRAME);
            FRAME_VISIBLE_P (frame) = 0;
          }
      }
      break;

    case WM_SIZE:
      /* We only care about this message if our size has really changed */
      if (wParam == SIZE_RESTORED || wParam == SIZE_MAXIMIZED ||
	  wParam == SIZE_MINIMIZED)
	{
	  RECT rect;
	  int columns, rows;

	  fobj = mswindows_find_frame (hwnd);
	  frame = XFRAME (fobj);
	  msframe = FRAME_MSWINDOWS_DATA (frame);

	  /* We cannot handle frame map and unmap hooks right in
	     this routine, because these may throw. We queue
	     magic events to run these hooks instead - kkm */

	  if (wParam == SIZE_MINIMIZED)
	    {
	      /* Iconified */
	      mswindows_enqueue_magic_event (hwnd, XM_UNMAPFRAME);
	    }
	  else
	    {
	      GetClientRect(hwnd, &rect);
	      FRAME_PIXWIDTH(frame) = rect.right;
	      FRAME_PIXHEIGHT(frame) = rect.bottom;

	      pixel_to_real_char_size (frame, rect.right, rect.bottom,
				       &FRAME_MSWINDOWS_CHARWIDTH (frame),
				       &FRAME_MSWINDOWS_CHARHEIGHT (frame));

	      pixel_to_char_size (frame, rect.right, rect.bottom, &columns,
				  &rows);
	      change_frame_size (frame, rows, columns, 1);

	      /* If we are inside frame creation, we have to apply geometric
		 properties now. */
	      if (FRAME_MSWINDOWS_TARGET_RECT (frame))
		{
		  /* Yes, we have to size again */
		  mswindows_size_frame_internal (frame,
						 FRAME_MSWINDOWS_TARGET_RECT
						 (frame));
		  /* Reset so we do not get here again. The SetWindowPos
		   * call in mswindows_size_frame_internal can cause
		   * recursion here. */
		  if (FRAME_MSWINDOWS_TARGET_RECT (frame))
		    {
		      xfree (FRAME_MSWINDOWS_TARGET_RECT (frame));
		      FRAME_MSWINDOWS_TARGET_RECT (frame) = 0;
		    }
		}
	      else
		{
		  if (!msframe->sizing && !FRAME_VISIBLE_P (frame))
                    {
                      mswindows_enqueue_magic_event (hwnd, XM_MAPFRAME);
                      /* APA: Now that the magic XM_MAPFRAME event has
                       * been sent we can mark the frame as visible (just
                       * like 21.1 did). */
                      FRAME_VISIBLE_P (frame) = 1;
                    }

		  if (!msframe->sizing || mswindows_dynamic_frame_resize)
		    redisplay ();
		}
	    }
	}
      break;

    case WM_DISPLAYCHANGE:
      {
	struct device *d;
	DWORD message_tick = GetMessageTime ();

	fobj = mswindows_find_frame (hwnd);
	frame = XFRAME (fobj);
	d = XDEVICE (FRAME_DEVICE (frame));

	/* Do this only once per message. XEmacs can receive this message
	   through as many frames as it currently has open. Message time
	   will be the same for all these messages. Despite extreme
	   efficiency, the code below has about one in 4 billion
	   probability that the HDC is not recreated, provided that
	   XEmacs is running sufficiently longer than 52 days. */
	if (DEVICE_MSWINDOWS_UPDATE_TICK(d) != message_tick)
	  {
	    DEVICE_MSWINDOWS_UPDATE_TICK(d) = message_tick;
	    DeleteDC (DEVICE_MSWINDOWS_HCDC(d));
	    DEVICE_MSWINDOWS_HCDC(d) = CreateCompatibleDC (NULL);
	  }
      }
      break;

      /* Misc magic events which only require that the frame be identified */
    case WM_SETFOCUS:
    case WM_KILLFOCUS:
      mswindows_enqueue_magic_event (hwnd, message_);
      break;

    case WM_WINDOWPOSCHANGING:
      {
	WINDOWPOS *wp = (LPWINDOWPOS) lParam;
	WINDOWPLACEMENT wpl = { sizeof(WINDOWPLACEMENT) };
	GetWindowPlacement(hwnd, &wpl);

	/* Only interested if size is changing and we're not being iconified */
	if (wpl.showCmd != SW_SHOWMINIMIZED
	    && wpl.showCmd != SW_SHOWMAXIMIZED
	    && !(wp->flags & SWP_NOSIZE))
	  {
	    RECT ncsize = { 0, 0, 0, 0 };
	    int pixwidth, pixheight;
	    AdjustWindowRectEx (&ncsize, qxeGetWindowLong (hwnd, GWL_STYLE),
				GetMenu(hwnd) != NULL,
				qxeGetWindowLong (hwnd, GWL_EXSTYLE));

	    round_size_to_real_char (XFRAME (mswindows_find_frame (hwnd)),
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
	/* DefWindowProc sends useful WM_GETMINMAXINFO message, and adjusts
	   window position if the user tries to track window too small */
      }
      goto defproc;

    case WM_ENTERSIZEMOVE:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      msframe->sizing = 1;
      return 0;

    case WM_EXITSIZEMOVE:
      msframe = FRAME_MSWINDOWS_DATA (XFRAME (mswindows_find_frame (hwnd)));
      msframe->sizing = 0;
      /* Queue noop event */
      mswindows_enqueue_magic_event (NULL, XM_BUMPQUEUE);
      return 0;

#ifdef HAVE_SCROLLBARS
    case WM_VSCROLL:
    case WM_HSCROLL:
      {
	/* Direction of scroll is determined by scrollbar instance. */
	int code = (int) LOWORD(wParam);
	int pos = (short int) HIWORD(wParam);
	HWND hwndScrollBar = (HWND) lParam;
	struct gcpro gcpro1, gcpro2;

	mswindows_handle_scrollbar_event (hwndScrollBar, code,  pos);
	GCPRO2 (emacs_event, fobj);
	if (UNBOUNDP (mswindows_pump_outstanding_events ())) /* Can GC */
	  {
	    /* Error during event pumping - cancel scroll */
	    qxeSendMessage (hwndScrollBar, WM_CANCELMODE, 0, 0);
	  }
	UNGCPRO;
	break;
      }

    case WM_MOUSEWHEEL:
      {
	int keys = LOWORD (wParam); /* Modifier key flags */
	int delta = (short) HIWORD (wParam); /* Wheel rotation amount */

	if (mswindows_handle_mousewheel_event (mswindows_find_frame (hwnd),
					       keys, delta,
					       MAKEPOINTS (lParam)))
	  /* We are not in a modal loop so no pumping is necessary. */
	  break;
	else
	  goto defproc;
      }
#endif

#ifdef HAVE_MENUBARS
    case WM_INITMENU:
      if (UNBOUNDP (mswindows_handle_wm_initmenu
		    ((HMENU) wParam,
		     XFRAME (mswindows_find_frame (hwnd)))))
	qxeSendMessage (hwnd, WM_CANCELMODE, 0, 0);
      break;

    case WM_INITMENUPOPUP:
      if (!HIWORD(lParam))
	{
	  if (UNBOUNDP (mswindows_handle_wm_initmenupopup
			((HMENU) wParam,
			 XFRAME (mswindows_find_frame (hwnd)))))
	    qxeSendMessage (hwnd, WM_CANCELMODE, 0, 0);
	}
      break;

#endif /* HAVE_MENUBARS */

    case WM_COMMAND:
      {
	WORD id = LOWORD (wParam);
	WORD nid = HIWORD (wParam);
	HWND cid = (HWND)lParam;
	frame = XFRAME (mswindows_find_frame (hwnd));

#ifdef HAVE_TOOLBARS
	if (!NILP (mswindows_handle_toolbar_wm_command (frame, cid, id)))
	  break;
#endif
	/* widgets in a buffer only eval a callback for suitable events. */
	switch (nid)
	  {
	  case BN_CLICKED:
	  case EN_CHANGE:
	  case CBN_EDITCHANGE:
	  case CBN_SELCHANGE:
	    if (!NILP (mswindows_handle_gui_wm_command (frame, cid, id)))
	      return 0;
	  }
	/* menubars always must come last since the hashtables do not
	   always exist */
#ifdef HAVE_MENUBARS
	if (!NILP (mswindows_handle_wm_command (frame, id)))
	  break;
#endif

	goto defproc;
	/* Bite me - a spurious command.  This used to not be able to
	   happen but with the introduction of widgets it's now
	   possible. #### Andy, fix the god-damn widget code!  It has
	   more bugs than a termite's nest! */
      }
      break;

    case WM_CTLCOLORBTN:
    case WM_CTLCOLORLISTBOX:
    case WM_CTLCOLOREDIT:
    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLORSCROLLBAR:
      {
	/* if we get an opportunity to paint a widget then do so if
	   there is an appropriate face */
	HWND crtlwnd = (HWND) lParam;
	LONG ii = qxeGetWindowLong (crtlwnd, GWL_USERDATA);
	if (ii)
	  {
	    Lisp_Object image_instance;
	    image_instance = VOID_TO_LISP ((void *) ii);
	    if (IMAGE_INSTANCEP (image_instance)
		&&
		IMAGE_INSTANCE_TYPE_P (image_instance, IMAGE_WIDGET))
	      {
		/* set colors for the buttons */
		HDC hdc = (HDC) wParam;
		if (last_widget_brushed != ii)
		  {
		    if (widget_brush)
		      DeleteObject (widget_brush);
		    widget_brush = CreateSolidBrush
		      (COLOR_INSTANCE_MSWINDOWS_COLOR
		       (XCOLOR_INSTANCE
			(FACE_BACKGROUND
			 (XIMAGE_INSTANCE_WIDGET_FACE (image_instance),
			  XIMAGE_INSTANCE_FRAME (image_instance)))));
		  }
		last_widget_brushed = ii;
		SetTextColor
		  (hdc,
		   COLOR_INSTANCE_MSWINDOWS_COLOR
		   (XCOLOR_INSTANCE
		    (FACE_FOREGROUND
		     (XIMAGE_INSTANCE_WIDGET_FACE (image_instance),
		      XIMAGE_INSTANCE_FRAME (image_instance)))));
		SetBkMode (hdc, OPAQUE);
		SetBkColor
		  (hdc,
		   COLOR_INSTANCE_MSWINDOWS_COLOR
		   (XCOLOR_INSTANCE
		    (FACE_BACKGROUND
		     (XIMAGE_INSTANCE_WIDGET_FACE (image_instance),
		      XIMAGE_INSTANCE_FRAME (image_instance)))));
		return (LRESULT)widget_brush;
	      }
	  }
      }
      goto defproc;

#ifdef HAVE_DRAGNDROP
    case WM_DROPFILES:		/* implementation ripped-off from event-Xt.c */
      {
	UINT filecount, i;
	POINT point;

	Lisp_Object l_dndlist = Qnil, l_item = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;

	emacs_event = Fmake_event (Qnil, Qnil);
	event = XEVENT (emacs_event);

	GCPRO3 (emacs_event, l_dndlist, l_item);

	if (!DragQueryPoint ((HDROP) wParam, &point))
	  point.x = point.y = -1; /* outside client area */

	XSET_EVENT_TYPE (emacs_event, misc_user_event);
	XSET_EVENT_CHANNEL (emacs_event,  mswindows_find_frame(hwnd));
	XSET_EVENT_TIMESTAMP (emacs_event, GetMessageTime());
	XSET_EVENT_MISC_USER_BUTTON (emacs_event, 1);
	XSET_EVENT_MISC_USER_MODIFIERS (emacs_event,
              mswindows_modifier_state (NULL, (DWORD) -1, 0));
	XSET_EVENT_MISC_USER_X (emacs_event, point.x);
	XSET_EVENT_MISC_USER_Y (emacs_event, point.y);
	XSET_EVENT_MISC_USER_FUNCTION (emacs_event,
					Qdragdrop_drop_dispatch);

	filecount = qxeDragQueryFile ((HDROP) wParam, 0xffffffff, NULL, 0);
	for (i = 0; i < filecount; i++)
	  {
	    Ibyte *fname;
	    Extbyte *fname_ext;
	    Bytecount fnamelen;
	    Charcount len = qxeDragQueryFile ((HDROP) wParam, i, NULL, 0);
	    /* The URLs that we make here aren't correct according to section
	     * 3.10 of rfc1738 because they're missing the //<host>/ part and
	     * because they may contain reserved characters. But that's OK -
	     * they just need to be good enough to keep dragdrop.el happy. */
	    fname_ext = (Extbyte *) ALLOCA ((len + 1) * XETCHAR_SIZE);
	    qxeDragQueryFile ((HDROP) wParam, i, fname_ext, len + 1);

	    TO_INTERNAL_FORMAT (DATA, (fname_ext, len * XETCHAR_SIZE),
				ALLOCA, (fname, fnamelen),
				Qmswindows_tstr);

	    /* May be a shell link aka "shortcut" - replace fname if so */
#if !defined (NO_CYGWIN_COM_SUPPORT)
	    if (!qxestrcasecmp_c (fname + fnamelen - 4, ".LNK"))
	      {
		/* ####
		   
		   Note the following in the docs:
		   
		   Note: The IShellLink interface has an ANSI version
		   (IShellLinkA) and a Unicode version (IShellLinkW). The
		   version that will be used depends on whether you compile
		   for ANSI or Unicode. However, Microsoft® Windows 95 and
		   Microsoft® Windows 98 only support IShellLinkA.
		   
		   We haven't yet implemented COM support in the
		   Unicode-splitting library.  I don't quite understand how
		   COM works yet, but it looks like what's happening is
		   that the ShellLink class implements both the IShellLinkA
		   and IShellLinkW interfaces.  To make this work at
		   run-time, we have to do something like this:
		   
		   -- define a new interface qxeIShellLink that uses
		   Extbyte * instead of LPSTR or LPWSTR. (not totally
		   necessary since Extbyte * == LPSTR).
		   
		   -- define a new class qxeShellLink that implements
		   qxeIShellLink.  the methods on this class need to create
		   a shadow ShellLink object to do all the real work, and
		   call the corresponding function from either the
		   IShellLinkA or IShellLinkW interfaces on this object,
		   depending on whether XEUNICODE_P is defined.
		   
		   -- with appropriate preprocessor magic, of course, we
		   could make things appear transparent; but we've decided
		   not to do preprocessor magic for the moment.
		   */

		/* #### Not Unicode-split for the moment; we have to do it
                   ourselves. */
		if (XEUNICODE_P)
		  {
		    IShellLinkW *psl;

		    if (CoCreateInstance (
					  XECOMID (CLSID_ShellLink),
					  NULL,
					  CLSCTX_INPROC_SERVER,
					  XECOMID (IID_IShellLinkW),
					  (void **) &psl) == S_OK)
		      {
			IPersistFile *ppf;

			if (XECOMCALL2 (psl, QueryInterface,
					XECOMID (IID_IPersistFile),
					(void **) &ppf) ==
			    S_OK)
			  {
			    Extbyte *fname_unicode;
			    WIN32_FIND_DATAW wfd;
			    LPWSTR resolved =
			      alloca_array (WCHAR, PATH_MAX + 1);

			    TO_EXTERNAL_FORMAT (DATA, (fname, fnamelen),
						C_STRING_ALLOCA,
						fname_unicode,
						Qmswindows_unicode);

			    if (XECOMCALL2 (ppf, Load,
					    (LPWSTR) fname_unicode,
					    STGM_READ) == S_OK &&
				/* #### YUCK!  Docs read

   cchMaxPath 

        Maximum number of bytes to copy to the buffer pointed to by the
	pszFile parameter.

   But "cch" means "count of characters", not bytes.  I'll assume the doc
   writers messed up and the programmer was correct.  Also, this approach
   is safe even if it's actually the other way around. */
#ifdef CYGWIN_HEADERS /* Another Cygwin prototype error */
				XECOMCALL4 (psl, GetPath, (LPSTR) resolved,
					    PATH_MAX, &wfd, 0)
#else
				XECOMCALL4 (psl, GetPath, resolved,
					    PATH_MAX, &wfd, 0)
#endif
				== S_OK)
			      TO_INTERNAL_FORMAT (C_STRING, resolved,
						  ALLOCA, (fname, fnamelen),
						  Qmswindows_tstr);

			    XECOMCALL0 (ppf, Release);
			  }

			XECOMCALL0 (psl, Release);
		      }
		  }
		else
		  {
		    IShellLinkA *psl;

		    if (CoCreateInstance (
					  XECOMID (CLSID_ShellLink),
					  NULL,
					  CLSCTX_INPROC_SERVER,
					  XECOMID (IID_IShellLinkA),
					  (void **) &psl) == S_OK)
		      {
			IPersistFile *ppf;

			if (XECOMCALL2 (psl, QueryInterface,
					XECOMID (IID_IPersistFile),
					(void **) &ppf) ==
			    S_OK)
			  {
			    Extbyte *fname_unicode;
			    WIN32_FIND_DATAA wfd;
			    LPSTR resolved =
			      alloca_array (CHAR, PATH_MAX + 1);

			    /* Always Unicode.  Not obvious from the
                               IPersistFile documentation, but look under
                               "Shell Link" for example code. */
			    TO_EXTERNAL_FORMAT (DATA, (fname, fnamelen),
						C_STRING_ALLOCA,
						fname_unicode,
						Qmswindows_unicode);

			    if (XECOMCALL2 (ppf, Load,
					    (LPWSTR) fname_unicode,
					    STGM_READ) == S_OK
				&& XECOMCALL4 (psl, GetPath, resolved,
					       PATH_MAX, &wfd, 0) == S_OK)
			      TO_INTERNAL_FORMAT (C_STRING, resolved,
						  ALLOCA, (fname, fnamelen),
						  Qmswindows_tstr);

			    XECOMCALL0 (ppf, Release);
			  }

			XECOMCALL0 (psl, Release);
		      }
		  }
	      }
#endif /* !defined (NO_CYGWIN_COM_SUPPORT) */
	    {
	      fname = urlify_filename (fname);
	      l_item = build_intstring (fname);
	      xfree (fname);
	      l_dndlist = Fcons (l_item, l_dndlist);
	    }
	  }

	DragFinish ((HDROP) wParam);

	SET_EVENT_MISC_USER_OBJECT (event,
				    Fcons (Qdragdrop_URL, l_dndlist));
	mswindows_enqueue_dispatch_event (emacs_event);
	UNGCPRO;
      }
      break;
#endif /* HAVE_DRAGNDROP */

#ifdef MULE
    case WM_IME_CHAR:

    case WM_IME_STARTCOMPOSITION:
      mswindows_start_ime_composition (XFRAME (mswindows_find_frame (hwnd)));
      goto defproc;

    case WM_IME_COMPOSITION:
      if (lParam & GCS_RESULTSTR)
       {
         HIMC imc = ImmGetContext (hwnd);
	 Extbyte *result;
         Bytecount len;
	 Ibyte *resultint, *endptr;
	 Bytecount lenint;
	 int speccount;

         if (!imc)
           break;

	 /* See WM_KEYDOWN above. */
	 speccount = begin_dont_check_for_quit ();

	 /* Sizes always in bytes, even for unicode.
            ImmGetCompositionStringW is supported even on Windows 9x, and
            allows us to handle multiple languages. */
         len = ImmGetCompositionStringW (imc, GCS_RESULTSTR, NULL, 0);
	 result = alloca_array (Extbyte, len);
         ImmGetCompositionStringW (imc, GCS_RESULTSTR, (WCHAR *) result, len);
         ImmReleaseContext (hwnd, imc);

	 TO_INTERNAL_FORMAT (DATA, (result, len),
			     ALLOCA, (resultint, lenint),
			     Qmswindows_tstr);

	 endptr = resultint + lenint;

	 while (resultint < endptr)
	   {
	     Ichar ch = itext_ichar (resultint);
	      if (ch == ' ')
		mswindows_enqueue_keypress_event (hwnd, QKspace, 0);
	      else
		mswindows_enqueue_keypress_event (hwnd, make_char (ch), 0);
	     INC_IBYTEPTR (resultint);
	   }

	 unbind_to (speccount);
       }
      goto defproc;
#endif /* MULE */

    defproc:
    default:
      return qxeDefWindowProc (hwnd, message_, wParam, lParam);
    }
  return (0);
}


/************************************************************************/
/*      keyboard, mouse & other helpers for the windows procedure       */
/************************************************************************/
static void
mswindows_set_chord_timer (HWND hwnd)
{
  int interval;

  /* We get one third half system double click threshold */
  if (mswindows_mouse_button_tolerance <= 0)
    interval = GetDoubleClickTime () / 3;
  else
    interval = mswindows_mouse_button_tolerance;

  SetTimer (hwnd, BUTTON_2_TIMER_ID, interval, 0);
}

static int
mswindows_button2_near_enough (POINTS p1, POINTS p2)
{
  int dx, dy;
  if (mswindows_mouse_button_max_skew_x <= 0)
    dx = GetSystemMetrics (SM_CXDOUBLECLK) / 2;
  else
    dx = mswindows_mouse_button_max_skew_x;

  if (mswindows_mouse_button_max_skew_y <= 0)
    dy = GetSystemMetrics (SM_CYDOUBLECLK) / 2;
  else
    dy = mswindows_mouse_button_max_skew_y;

  return abs (p1.x - p2.x) < dx && abs (p1.y- p2.y)< dy;
}

static int
mswindows_current_layout_has_AltGr (void)
{
  /* This simple caching mechanism saves 10% of CPU
     time when a key typed at autorepeat rate of 30 cps! */
  static HKL last_hkl = 0;
  static int last_hkl_has_AltGr;
  HKL current_hkl = GetKeyboardLayout (0);

  if (current_hkl != last_hkl)
    {
      int c;
      last_hkl_has_AltGr = 0;
      /* In this loop, we query whether a character requires
	 AltGr to be down to generate it. If at least such one
	 found, this means that the layout does regard AltGr */
      for (c = ' '; c <= 255 && !last_hkl_has_AltGr; ++c)
	/* #### This is not really such a good check.  What about under
	   CJK locales?  It may not matter there, though.  We always
	   call VkKeyScanA so that we check the locale-specific characters
	   in non-Latin-1 locales, instead of just the Latin-1 characters. */
	if (HIBYTE (VkKeyScanA ((char) c)) == 6)
	  last_hkl_has_AltGr = 1;
      last_hkl = current_hkl;
    }
  return last_hkl_has_AltGr;
}


/* Returns the state of the modifier keys in the format expected by the
 * Lisp_Event key_data, button_data and motion_data modifiers member */
static int
mswindows_modifier_state (BYTE *keymap, DWORD fwKeys, int has_AltGr)
{
  int mods = 0;
  int keys_is_real = 0;
  BYTE keymap2[256];

  if (fwKeys == (DWORD) -1)
    fwKeys = mswindows_last_mouse_button_state;
  else
    {
      keys_is_real = 1;
      mswindows_last_mouse_button_state = fwKeys;
    }

  if (keymap == NULL)
    {
      keymap = keymap2;
      GetKeyboardState (keymap);
      has_AltGr = mswindows_current_layout_has_AltGr ();
    }

  /* #### should look at fwKeys for MK_CONTROL.  I don't understand how
     AltGr works. */
  if (has_AltGr && (keymap [VK_LCONTROL] & 0x80) && (keymap [VK_RMENU] & 0x80))
    {
      mods |= (keymap [VK_LMENU] & 0x80) ? XEMACS_MOD_META : 0;
      mods |= (keymap [VK_RCONTROL] & 0x80) ? XEMACS_MOD_CONTROL : 0;
    }
  else
    {
      mods |= (keymap [VK_MENU] & 0x80) ? XEMACS_MOD_META : 0;
      mods |= (keymap [VK_CONTROL] & 0x80) ? XEMACS_MOD_CONTROL : 0;
    }

  mods |= (keys_is_real ? (int) (fwKeys & MK_SHIFT) :
	   (keymap [VK_SHIFT] & 0x80)) ? XEMACS_MOD_SHIFT : 0;
  mods |= fwKeys & MK_LBUTTON ? XEMACS_MOD_BUTTON1 : 0;
  mods |= fwKeys & MK_MBUTTON ? XEMACS_MOD_BUTTON2 : 0;
  mods |= fwKeys & MK_RBUTTON ? XEMACS_MOD_BUTTON3 : 0;

  return mods;
}

/*
 * Translate a mswindows virtual key to a keysym.
 * Only returns non-Qnil for keys that don't generate WM_CHAR messages
 * or whose ASCII codes (like space) xemacs doesn't like.
 */
Lisp_Object mswindows_key_to_emacs_keysym (int mswindows_key, int mods,
					   int extendedp)
{
  if (extendedp)	/* Keys not present on a 82 key keyboard */
    {
      switch (mswindows_key)
        {
	case VK_CANCEL:		return KEYSYM ("pause");
	case VK_RETURN:		return KEYSYM ("kp-enter");
	case VK_PRIOR:		return KEYSYM ("prior");
	case VK_NEXT:		return KEYSYM ("next");
	case VK_END:		return KEYSYM ("end");
	case VK_HOME:		return KEYSYM ("home");
	case VK_LEFT:		return KEYSYM ("left");
	case VK_UP:		return KEYSYM ("up");
	case VK_RIGHT:		return KEYSYM ("right");
	case VK_DOWN:		return KEYSYM ("down");
	case VK_INSERT:		return KEYSYM ("insert");
	case VK_DELETE:		return QKdelete;
#if 0	/* FSF Emacs allows these to return configurable syms/mods */
	case VK_LWIN		return KEYSYM ("");
	case VK_RWIN		return KEYSYM ("");
#endif
	case VK_APPS:		return KEYSYM ("menu");
	}
    }
  else
    {
      switch (mswindows_key)
	{

#if 0
	VK_LBUTTON:
	VK_RBUTTON:
	VK_CANCEL:
	VK_MBUTTON:
	VK_XBUTTON1:
	VK_XBUTTON2:
#endif /* 0 */

	case VK_BACK:		return QKbackspace;
	case VK_TAB:		return QKtab;
	  /* #### Officially 0A (and 0B too) are "reserved". */
	case '\n':		return QKlinefeed;
	case VK_CLEAR:		return KEYSYM ("clear");
	case VK_RETURN:		return QKreturn;

#if 0
	VK_SHIFT: "shift"
	VK_CONTROL: "control"
	VK_MENU: "alt"
#endif /* 0 */

	case VK_PAUSE:		return KEYSYM ("pause");

#if 0
	VK_CAPITAL: "caps-lock"
	VK_KANA: IME Kana mode 
	VK_HANGUEL: IME Hanguel mode (maintained for compatibility; use VK_HANGUL) 
	VK_HANGUL: IME Hangul mode 
	VK_JUNJA: IME Junja mode 
	VK_FINAL: IME final mode 
	VK_HANJA: IME Hanja mode 
	VK_KANJI: IME Kanji mode 
#endif /* 0 */

	case VK_ESCAPE:		return QKescape;

#if 0
	VK_CONVERT: IME convert 
	VK_NONCONVERT: IME nonconvert 
	VK_ACCEPT: IME accept 
	VK_MODECHANGE: IME mode change request 
#endif /* 0 */

	case VK_SPACE:		return QKspace;
	case VK_PRIOR:		return KEYSYM ("kp-prior");
	case VK_NEXT:		return KEYSYM ("kp-next");
	case VK_END:		return KEYSYM ("kp-end");
	case VK_HOME:		return KEYSYM ("kp-home");
	case VK_LEFT:		return KEYSYM ("kp-left");
	case VK_UP:		return KEYSYM ("kp-up");
	case VK_RIGHT:		return KEYSYM ("kp-right");
	case VK_DOWN:		return KEYSYM ("kp-down");
	case VK_SELECT:		return KEYSYM ("select");
	case VK_PRINT:		return KEYSYM ("print");
	case VK_EXECUTE:	return KEYSYM ("execute");
	case VK_SNAPSHOT:	return KEYSYM ("print");
	case VK_INSERT:		return KEYSYM ("kp-insert");
	case VK_DELETE:		return KEYSYM ("kp-delete");
	case VK_HELP:		return KEYSYM ("help");
#if 0
	  '0' through '9': numeric keys
	  'A' through 'Z': alphabetic keys
	  VK_LWIN: "lwin"
	  VK_RWIN: "rwin"
	  VK_APPS: "apps"
	  VK_SLEEP: "sleep"
#endif /* 0 */
	case VK_NUMPAD0:	return KEYSYM ("kp-0");
	case VK_NUMPAD1:	return KEYSYM ("kp-1");
	case VK_NUMPAD2:	return KEYSYM ("kp-2");
	case VK_NUMPAD3:	return KEYSYM ("kp-3");
	case VK_NUMPAD4:	return KEYSYM ("kp-4");
	case VK_NUMPAD5:	return KEYSYM ("kp-5");
	case VK_NUMPAD6:	return KEYSYM ("kp-6");
	case VK_NUMPAD7:	return KEYSYM ("kp-7");
	case VK_NUMPAD8:	return KEYSYM ("kp-8");
	case VK_NUMPAD9:	return KEYSYM ("kp-9");
	case VK_MULTIPLY:	return KEYSYM ("kp-multiply");
	case VK_ADD:		return KEYSYM ("kp-add");
	case VK_SEPARATOR:	return KEYSYM ("kp-separator");
	case VK_SUBTRACT:	return KEYSYM ("kp-subtract");
	case VK_DECIMAL:	return KEYSYM ("kp-decimal");
	case VK_DIVIDE:		return KEYSYM ("kp-divide");
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

#if 0
	VK_NUMLOCK: 90 NUM LOCK key  
	VK_SCROLL: 91 SCROLL LOCK key  
	  92~96 OEM specific;
	VK_LSHIFT:
	VK_RSHIFT:
	VK_LCONTROL:
	VK_RCONTROL:
	VK_LMENU:
	VK_RMENU:

#ifdef VK_BROWSER_BACK /* Windows 2000 only */
	VK_BROWSER_BACK: Browser Back key 
	VK_BROWSER_FORWARD: Browser Forward key 
	VK_BROWSER_REFRESH: Browser Refresh key 
	VK_BROWSER_STOP: Browser Stop key 
	VK_BROWSER_SEARCH: Browser Search key 
	VK_BROWSER_FAVORITES: Browser Favorites key 
	VK_BROWSER_HOME: Browser Start and Home key 
	VK_VOLUME_MUTE: Volume Mute key 
	VK_VOLUME_DOWN: Volume Down key 
	VK_VOLUME_UP: Volume Up key 
	VK_MEDIA_NEXT_TRACK: Next Track key 
	VK_MEDIA_PREV_TRACK: Previous Track key 
	VK_MEDIA_STOP: Stop Media key 
	VK_MEDIA_PLAY_PAUSE: Play/Pause Media key 
	VK_LAUNCH_MAIL: Start Mail key 
	VK_LAUNCH_MEDIA_SELECT: Select Media key 
	VK_LAUNCH_APP1: Start Application 1 key 
	VK_LAUNCH_APP2: Start Application 2 key 
	  B8-B9 Reserved;
	VK_OEM_1: For the US standard keyboard, the ';:' key
	VK_OEM_PLUS: For any country/region, the '+' key 
	VK_OEM_COMMA: For any country/region, the ',' key 
	VK_OEM_MINUS: For any country/region, the '-' key 
	VK_OEM_PERIOD: For any country/region, the '.' key 
	VK_OEM_2: For the US standard keyboard, the '/?' key 
	VK_OEM_3: For the US standard keyboard, the '`~' key 
	  C1~D7 Reserved;
	  D8~DA Unassigned;
	VK_OEM_4: For the US standard keyboard, the '[{' key 
	VK_OEM_5: For the US standard keyboard, the '\|' key 
	VK_OEM_6: For the US standard keyboard, the ']}' key 
	VK_OEM_7: For the US standard keyboard, the 'single-quote/double-quote' key 
	VK_OEM_8:
	  E0 Reserved;
	  E1 OEM specific;
	VK_OEM_102: Either the angle bracket key or the backslash key on the RT 102-key keyboard 
	  E3~E4 OEM specific;
#endif /* VK_BROWSER_BACK */
	VK_PROCESSKEY: E5 Windows 95/98, Windows NT 4.0, Windows 2000: IME PROCESS key 
	  E6 OEM specific;
	VK_PACKET: Used to pass Unicode characters as if they were keystrokes. The VK_PACKET key is the low word of a 32-bit Virtual Key value used for non-keyboard input methods. For more information, see Remark in KEYBDINPUT, SendInput, WM_KEYDOWN, and WM_KEYUP 
	  E8 Unassigned;
	  E9~F5 OEM specific;
	VK_ATTN: Attn key 
	VK_CRSEL: CrSel key 
	VK_EXSEL: ExSel key 
	VK_EREOF: Erase EOF key 
	VK_PLAY: Play key 
	VK_ZOOM: Zoom key 
	VK_NONAME: Reserved for future use  
	VK_PA1: PA1 key
	VK_OEM_CLEAR: Clear key
#endif /* 0 */

	}
    }
  return Qnil;
}

/*
 * Find the console that matches the supplied mswindows window handle
 */
Lisp_Object
mswindows_find_console (HWND hwnd)
{
  /* We only support one console */
  return XCAR (Vconsole_list);
}

/*
 * Find the frame that matches the supplied mswindows window handle
 */
Lisp_Object
mswindows_find_frame (HWND hwnd)
{
  LONG l = qxeGetWindowLong (hwnd, XWL_FRAMEOBJ);
  Lisp_Object f;
  if (l == 0)
    {
      /* We are in progress of frame creation. Return the frame
	 being created, as it still not remembered in the window
	 extra storage. */
      assert (!NILP (Vmswindows_frame_being_created));
      return Vmswindows_frame_being_created;
    }
  f = VOID_TO_LISP ((void *) l);
  return f;
}


/************************************************************************/
/*                            methods                                   */
/************************************************************************/

static int
emacs_mswindows_add_timeout (EMACS_TIME thyme)
{
  int milliseconds;
  EMACS_TIME current_time;
  EMACS_GET_TIME (current_time);
  EMACS_SUB_TIME (thyme, thyme, current_time);
  milliseconds = EMACS_SECS (thyme) * 1000 +
    (EMACS_USECS (thyme) + 500) / 1000;
  if (milliseconds < 1)
    milliseconds = 1;
  ++mswindows_pending_timers_count;
  return SetTimer (NULL, 0, milliseconds,
		   (TIMERPROC) mswindows_wm_timer_callback);
}

static int
remove_timeout_mapper (Lisp_Object ev, void *data)
{
  if (XEVENT_TYPE (ev) == timeout_event)
    {
      if ((int) data == XEVENT_TIMEOUT_INTERVAL_ID (ev))
	return 1;
    }

  return 0;
}

static void
emacs_mswindows_remove_timeout (int id)
{
  if (KillTimer (NULL, id))
    --mswindows_pending_timers_count;

  /* If there is a dispatch event generated by this
     timeout in the queue, we have to remove it too. */
  map_event_chain_remove (remove_timeout_mapper,
			  &mswindows_s_dispatch_event_queue,
			  &mswindows_s_dispatch_event_queue_tail,
			  (void *) id, MECR_DEALLOCATE_EVENT);
}

/* If `user_p' is false, then return whether there are any win32, timeout,
 * or subprocess events pending (that is, whether
 * emacs_mswindows_next_event() would return immediately without blocking).
 *
 * if `user_p' is true, then return whether there are any *user generated*
 * events available (that is, whether there are keyboard or mouse-click
 * events ready to be read).  This also implies that
 * emacs_mswindows_next_event() would not block.
 */
static int
emacs_mswindows_event_pending_p (int user_p)
{
  mswindows_need_event (0);
  return (!NILP (dispatch_event_queue)
	  || (!user_p && !NILP (mswindows_s_dispatch_event_queue)));
}

/*
 * Return the next event
 */
static void
emacs_mswindows_next_event (Lisp_Event *emacs_event)
{
  Lisp_Object event, event2;

  mswindows_need_event (1);

  event = mswindows_dequeue_dispatch_event ();
  event2 = wrap_event (emacs_event);
  Fcopy_event (event, event2);
  Fdeallocate_event (event);
}

static void
emacs_mswindows_format_magic_event (Lisp_Event *emacs_event,
				    Lisp_Object pstream)
{
#define FROB(msg) case msg: write_c_string (pstream, "type=" #msg); break

  switch (EVENT_MAGIC_MSWINDOWS_EVENT (emacs_event))
    {
      FROB (XM_BUMPQUEUE);
      FROB (WM_PAINT);
      FROB (WM_SETFOCUS);
      FROB (WM_KILLFOCUS);
      FROB (XM_MAPFRAME);
      FROB (XM_UNMAPFRAME);

    default: abort ();
    }
#undef FROB
  
  if (!NILP (EVENT_CHANNEL (emacs_event)))
    {
      write_c_string (pstream, " ");
      print_internal (EVENT_CHANNEL (emacs_event), pstream, 1);
    }
}

static int
emacs_mswindows_compare_magic_event (Lisp_Event *e1, Lisp_Event *e2)
{
  return (EVENT_MAGIC_MSWINDOWS_EVENT (e1) ==
	  EVENT_MAGIC_MSWINDOWS_EVENT (e2));
}

static Hashcode
emacs_mswindows_hash_magic_event (Lisp_Event *e)
{
  return (EVENT_MAGIC_MSWINDOWS_EVENT (e));
}

/*
 * Handle a magic event off the dispatch queue.
 */
static void
emacs_mswindows_handle_magic_event (Lisp_Event *emacs_event)
{
  switch (EVENT_MAGIC_MSWINDOWS_EVENT (emacs_event))
    {
    case XM_BUMPQUEUE:
      break;

    case WM_PAINT:
      {
	struct frame *f = XFRAME (EVENT_CHANNEL (emacs_event));
	mswindows_handle_paint (f);
	(FRAME_MSWINDOWS_DATA (f))->paint_pending = 0;
      }
      break;

    case WM_SETFOCUS:
    case WM_KILLFOCUS:
      {
	Lisp_Object frame = EVENT_CHANNEL (emacs_event);
	struct frame *f = XFRAME (frame);
	int in_p = (EVENT_MAGIC_MSWINDOWS_EVENT (emacs_event)
		    == WM_SETFOCUS);
	Lisp_Object conser;
	struct gcpro gcpro1;

	/* On focus change, clear all memory of sticky modifiers
	   to avoid non-intuitive behavior. */
	clear_sticky_modifiers ();

	conser = Fcons (frame, Fcons (FRAME_DEVICE (f), in_p ? Qt : Qnil));
	GCPRO1 (conser);
	emacs_handle_focus_change_preliminary (conser);
	/* Under X the stuff up to here is done in the X event handler.
	   I Don't know why */
	emacs_handle_focus_change_final (conser);
	UNGCPRO;

      }
      break;

    case XM_MAPFRAME:
    case XM_UNMAPFRAME:
      {
	Lisp_Object frame = EVENT_CHANNEL (emacs_event);
	va_run_hook_with_args (EVENT_MAGIC_MSWINDOWS_EVENT (emacs_event)
			       == XM_MAPFRAME ?
			       Qmap_frame_hook : Qunmap_frame_hook,
			       1, frame);
      }
      break;

      /* #### What about Enter & Leave */
#if 0
      va_run_hook_with_args (in_p ? Qmouse_enter_frame_hook :
			     Qmouse_leave_frame_hook, 1, frame);
#endif

    default:
      assert(0);
    }
}

#ifndef CYGWIN

static HANDLE
get_process_input_waitable (Lisp_Process *process)
{
  Lisp_Object instr, outstr, errstr, p;
  p = wrap_process (process);
  get_process_streams (process, &instr, &outstr, &errstr);
  assert (!NILP (instr));
  return (network_connection_p (p)
	  ? get_winsock_stream_waitable (XLSTREAM (instr))
	  : get_ntpipe_input_stream_waitable (XLSTREAM (instr)));
}

static HANDLE
get_process_stderr_waitable (Lisp_Process *process)
{
  Lisp_Object instr, outstr, errstr;
  get_process_streams (process, &instr, &outstr, &errstr);
  if (NILP (errstr))
    return INVALID_HANDLE_VALUE;
  return get_ntpipe_input_stream_waitable (XLSTREAM (errstr));
}

#endif /* not CYGWIN */

static void
emacs_mswindows_select_process (Lisp_Process *process, int doin, int doerr)
{
#ifdef CYGWIN
  int infd, errfd;

  event_stream_unixoid_select_process (process, doin, doerr, &infd, &errfd);
#else
  HANDLE hev = INVALID_HANDLE_VALUE;
  HANDLE herr = INVALID_HANDLE_VALUE;

  if (doin)
    {
      hev = get_process_input_waitable (process);
      if (!add_waitable_handle (hev))
	{
	  hev = INVALID_HANDLE_VALUE;
	  goto err;
	}
    }

  if (doerr)
    {
      herr = get_process_stderr_waitable (process);
      if (herr != INVALID_HANDLE_VALUE && !add_waitable_handle (herr))
	{
	  herr = INVALID_HANDLE_VALUE;
	  goto err;
	}
    }

  {
    /* Also select on the process handle itself, so we can receive
       exit notifications.  Only do this once, not each time this
       function is called (which can happen many times, e.g. if
       (set-process-filter proc t) is called and then a process filter
       is set again).  It will be unselected in mswindows_need_event(). */
    Lisp_Object p = wrap_process (process);

    if (!network_connection_p (p))
      {
	HANDLE hprocess = get_nt_process_handle_only_first_time (process);
	if (hprocess != INVALID_HANDLE_VALUE
	    && !add_waitable_handle (hprocess))
	  goto err;
      }
  }

  return;

 err:
  if (hev != INVALID_HANDLE_VALUE)
    remove_waitable_handle (hev);
  if (herr != INVALID_HANDLE_VALUE)
    remove_waitable_handle (herr);
  invalid_operation ("Too many active processes", wrap_process (process));
#endif /* CYGWIN */
}

static void
emacs_mswindows_unselect_process (Lisp_Process *process, int doin, int doerr)
{
#ifdef CYGWIN
  int infd, errfd;

  event_stream_unixoid_unselect_process (process, doin, doerr, &infd, &errfd);
#else
  if (doin)
    {
      /* Process handle is removed in the event loop as soon
	 as it is signaled, so don't bother here about it */
      HANDLE hev = get_process_input_waitable (process);
      remove_waitable_handle (hev);
    }
  if (doerr)
    {
      /* Process handle is removed in the event loop as soon
	 as it is signaled, so don't bother here about it */
      HANDLE herr = get_process_stderr_waitable (process);
      if (herr != INVALID_HANDLE_VALUE)
        remove_waitable_handle (herr);
    }
#endif /* CYGWIN */
}

static void
emacs_mswindows_select_console (struct console *con)
{
#ifdef CYGWIN
  if (CONSOLE_MSWINDOWS_P (con))
    return; /* mswindows consoles are automatically selected */

  event_stream_unixoid_select_console (con);
#else
#if 0
  /* This is an attempt to get `xemacs -batch -l dunnet' to work.
     Doesn't currently work and fucks other things up. */
  if (CONSOLE_STREAM_P (con) &&
      !UNBOUNDP (CONSOLE_STREAM_DATA (con)->instream))
    {
      HANDLE h =
	(HANDLE) _get_osfhandle (fileno (CONSOLE_STREAM_DATA (con)->in));
      if (PeekNamedPipe (h, 0, 0, 0, 0, 0))
	{
	  Lisp_Object lstr = make_ntpipe_input_stream (h, 0);
	  HANDLE hwait = get_ntpipe_input_stream_waitable (XLSTREAM (lstr));

	  if (!add_waitable_handle (hwait))
	    invalid_operation ("Too many active processes",
			       wrap_console (con));
	  CONSOLE_STREAM_DATA (con)->instream = lstr;
	}
      else
	/* Unable to select on this stream */
	CONSOLE_STREAM_DATA (con)->instream = Qunbound;
    }
#endif /* 0 */
#endif
}

static void
emacs_mswindows_unselect_console (struct console *con)
{
#ifdef CYGWIN
  if (CONSOLE_MSWINDOWS_P (con))
    return; /* mswindows consoles are automatically selected */

  event_stream_unixoid_unselect_console (con);
#else
#if 0 /* see above */
  if (CONSOLE_STREAM_P (con) &&
      !UNBOUNDP (CONSOLE_STREAM_DATA (con)->instream))
    {
      Lisp_Object instr = CONSOLE_STREAM_DATA (con)->instream;
      HANDLE hwait;

      assert (!NILP (instr));
      hwait = get_ntpipe_input_stream_waitable (XLSTREAM (instr));

      remove_waitable_handle (hwait);
    }
#endif /* 0 */
#endif
}

static void
emacs_mswindows_create_io_streams (void *inhandle, void *outhandle,
				   void *errhandle, Lisp_Object *instream,
				   Lisp_Object *outstream,
				   Lisp_Object *errstream,
				   USID *in_usid,
				   USID *err_usid,
				   int flags)
{
#ifdef CYGWIN
  event_stream_unixoid_create_io_streams (inhandle, outhandle,
					  errhandle, instream,
					  outstream, errstream,
					  in_usid, err_usid, flags);
#else
  /* Handles for streams */
  HANDLE hin, hout, herr;
  /* fds. These just stored along with the streams, and are closed in
     delete stream pair method, because we need to handle fake unices
     here. */
  int fdi, fdo, fde;

  /* Decode inhandle, outhandle, errhandle. Their meaning depends on
     the process implementation being used. */
  hin = (HANDLE) inhandle;
  hout = (HANDLE) outhandle;
  if (errhandle == (void *) -1)
    herr = INVALID_HANDLE_VALUE;
  else
    herr = (HANDLE) errhandle;
  fdi = fdo = fde = -1;

  *instream = (hin == INVALID_HANDLE_VALUE
	       ? Qnil
	       : flags & STREAM_NETWORK_CONNECTION
	       ? make_winsock_input_stream ((SOCKET) hin, fdi)
	       : make_ntpipe_input_stream (hin, fdi));

  *errstream = (herr == INVALID_HANDLE_VALUE
		? Qnil
		: make_ntpipe_input_stream (herr, fde));

  *outstream = (hout == INVALID_HANDLE_VALUE
		? Qnil
		: flags & STREAM_NETWORK_CONNECTION
		? make_winsock_output_stream ((SOCKET)hout, fdo)
		: make_ntpipe_output_stream (hout, fdo));

  *in_usid =
    (NILP (*instream)
     ? USID_ERROR
     : flags & STREAM_NETWORK_CONNECTION
     ? HANDLE_TO_USID (get_winsock_stream_waitable (XLSTREAM (*instream)))
     : HANDLE_TO_USID (get_ntpipe_input_stream_waitable (XLSTREAM
							 (*instream))));

  *err_usid =
    (NILP (*errstream)
     ? USID_DONTHASH
     : HANDLE_TO_USID (get_ntpipe_input_stream_waitable (XLSTREAM
							 (*errstream))));
#endif /* CYGWIN */
}

static void
emacs_mswindows_delete_io_streams (Lisp_Object instream,
				   Lisp_Object outstream,
				   Lisp_Object errstream,
				   USID *in_usid,
				   USID *err_usid)
{
#ifdef CYGWIN
  event_stream_unixoid_delete_io_streams (instream, outstream, errstream,
					  in_usid, err_usid);
#else
  *in_usid =
    (NILP (instream)
     ? USID_DONTHASH
     : LSTREAM_TYPE_P (XLSTREAM (instream), winsock)
     ? HANDLE_TO_USID (get_winsock_stream_waitable (XLSTREAM (instream)))
     : HANDLE_TO_USID (get_ntpipe_input_stream_waitable (XLSTREAM
							 (instream))));

  *err_usid =
    (NILP (errstream)
     ? USID_DONTHASH
     : HANDLE_TO_USID (get_ntpipe_input_stream_waitable (XLSTREAM
							 (errstream))));
#endif /* CYGWIN */
}

static int
emacs_mswindows_current_event_timestamp (struct console *c)
{
  return GetTickCount ();
}

#ifndef HAVE_X_WINDOWS
/* This is called from GC when a process object is about to be freed.
   If we've still got pointers to it in this file, we're gonna lose hard.
*/
void debug_process_finalization (Lisp_Process *p);
void
debug_process_finalization (Lisp_Process *p)
{
#if 0 /* #### */
  Lisp_Object instr, outstr, errstr;

  get_process_streams (p, &instr, &outstr, &errstr);
  /* if it still has fds, then it hasn't been killed yet. */
  assert (NILP (instr));
  assert (NILP (outstr));
  assert (NILP (errstr));

  /* #### More checks here */
#endif
}
#endif

#ifdef DEBUG_XEMACS

struct mswin_message_debug
{
  int mess;
  char *string;
};

#define FROB(val) { val, #val, },

struct mswin_message_debug debug_mswin_messages[] =
{
FROB (WM_NULL)
FROB (WM_CREATE)
FROB (WM_DESTROY)
FROB (WM_MOVE)
FROB (WM_SIZE)

FROB (WM_ACTIVATE)

FROB (WM_SETFOCUS)
FROB (WM_KILLFOCUS)
FROB (WM_ENABLE)
FROB (WM_SETREDRAW)
FROB (WM_SETTEXT)
FROB (WM_GETTEXT)
FROB (WM_GETTEXTLENGTH)
FROB (WM_PAINT)
FROB (WM_CLOSE)
FROB (WM_QUERYENDSESSION)
FROB (WM_QUIT)
FROB (WM_QUERYOPEN)
FROB (WM_ERASEBKGND)
FROB (WM_SYSCOLORCHANGE)
FROB (WM_ENDSESSION)
FROB (WM_SHOWWINDOW)
FROB (WM_WININICHANGE)
#if(WINVER >= 0x0400)
FROB (WM_SETTINGCHANGE)
#endif /* WINVER >= 0x0400 */

FROB (WM_DEVMODECHANGE)
FROB (WM_ACTIVATEAPP)
FROB (WM_FONTCHANGE)
FROB (WM_TIMECHANGE)
FROB (WM_CANCELMODE)
FROB (WM_SETCURSOR)
FROB (WM_MOUSEACTIVATE)
FROB (WM_CHILDACTIVATE)
FROB (WM_QUEUESYNC)

FROB (WM_GETMINMAXINFO)

FROB (WM_PAINTICON)
FROB (WM_ICONERASEBKGND)
FROB (WM_NEXTDLGCTL)
FROB (WM_SPOOLERSTATUS)
FROB (WM_DRAWITEM)
FROB (WM_MEASUREITEM)
FROB (WM_DELETEITEM)
FROB (WM_VKEYTOITEM)
FROB (WM_CHARTOITEM)
FROB (WM_SETFONT)
FROB (WM_GETFONT)
FROB (WM_SETHOTKEY)
FROB (WM_GETHOTKEY)
FROB (WM_QUERYDRAGICON)
FROB (WM_COMPAREITEM)
#if(WINVER >= 0x0500)
FROB (WM_GETOBJECT)
#endif /* WINVER >= 0x0500 */
FROB (WM_COMPACTING)
FROB (WM_COMMNOTIFY)
FROB (WM_WINDOWPOSCHANGING)
FROB (WM_WINDOWPOSCHANGED)

FROB (WM_POWER)

FROB (WM_COPYDATA)
FROB (WM_CANCELJOURNAL)

#if(WINVER >= 0x0400)
FROB (WM_NOTIFY)
FROB (WM_INPUTLANGCHANGEREQUEST)
FROB (WM_INPUTLANGCHANGE)
FROB (WM_TCARD)
FROB (WM_HELP)
FROB (WM_USERCHANGED)
FROB (WM_NOTIFYFORMAT)

FROB (WM_CONTEXTMENU)
FROB (WM_STYLECHANGING)
FROB (WM_STYLECHANGED)
FROB (WM_DISPLAYCHANGE)
FROB (WM_GETICON)
FROB (WM_SETICON)
#endif /* WINVER >= 0x0400 */

FROB (WM_NCCREATE)
FROB (WM_NCDESTROY)
FROB (WM_NCCALCSIZE)
FROB (WM_NCHITTEST)
FROB (WM_NCPAINT)
FROB (WM_NCACTIVATE)
FROB (WM_GETDLGCODE)
#ifdef WM_SYNCPAINT /* not in VC 5 */
FROB (WM_SYNCPAINT)
#endif /* WM_SYNCPAINT */
FROB (WM_NCMOUSEMOVE)
FROB (WM_NCLBUTTONDOWN)
FROB (WM_NCLBUTTONUP)
FROB (WM_NCLBUTTONDBLCLK)
FROB (WM_NCRBUTTONDOWN)
FROB (WM_NCRBUTTONUP)
FROB (WM_NCRBUTTONDBLCLK)
FROB (WM_NCMBUTTONDOWN)
FROB (WM_NCMBUTTONUP)
FROB (WM_NCMBUTTONDBLCLK)

/* FROB (WM_KEYFIRST) */
FROB (WM_KEYDOWN)
FROB (WM_KEYUP)
FROB (WM_CHAR)
FROB (WM_DEADCHAR)
FROB (WM_SYSKEYDOWN)
FROB (WM_SYSKEYUP)
FROB (WM_SYSCHAR)
FROB (WM_SYSDEADCHAR)
FROB (WM_KEYLAST)

#if(WINVER >= 0x0400) && defined (WM_IME_STARTCOMPOSITION)
/* not in Cygwin? */ 
FROB (WM_IME_STARTCOMPOSITION)
FROB (WM_IME_ENDCOMPOSITION)
FROB (WM_IME_COMPOSITION)
FROB (WM_IME_KEYLAST)
#endif /* WINVER >= 0x0400 && defined (WM_IME_STARTCOMPOSITION) */

FROB (WM_INITDIALOG)
FROB (WM_COMMAND)
FROB (WM_SYSCOMMAND)
FROB (WM_TIMER)
FROB (WM_HSCROLL)
FROB (WM_VSCROLL)
FROB (WM_INITMENU)
FROB (WM_INITMENUPOPUP)
FROB (WM_MENUSELECT)
FROB (WM_MENUCHAR)
FROB (WM_ENTERIDLE)
#if(WINVER >= 0x0500)
FROB (WM_MENURBUTTONUP)
FROB (WM_MENUDRAG)
FROB (WM_MENUGETOBJECT)
FROB (WM_UNINITMENUPOPUP)
FROB (WM_MENUCOMMAND)
#endif /* WINVER >= 0x0500 */


FROB (WM_CTLCOLORMSGBOX)
FROB (WM_CTLCOLOREDIT)
FROB (WM_CTLCOLORLISTBOX)
FROB (WM_CTLCOLORBTN)
FROB (WM_CTLCOLORDLG)
FROB (WM_CTLCOLORSCROLLBAR)
FROB (WM_CTLCOLORSTATIC)


/* FROB (WM_MOUSEFIRST) */
FROB (WM_MOUSEMOVE)
FROB (WM_LBUTTONDOWN)
FROB (WM_LBUTTONUP)
FROB (WM_LBUTTONDBLCLK)
FROB (WM_RBUTTONDOWN)
FROB (WM_RBUTTONUP)
FROB (WM_RBUTTONDBLCLK)
FROB (WM_MBUTTONDOWN)
FROB (WM_MBUTTONUP)
FROB (WM_MBUTTONDBLCLK)

#if (_WIN32_WINNT >= 0x0400) || (_WIN32_WINDOWS > 0x0400)
FROB (WM_MOUSEWHEEL)
FROB (WM_MOUSELAST)
#else
FROB (WM_MOUSELAST)
#endif /* if (_WIN32_WINNT < 0x0400) */

FROB (WM_PARENTNOTIFY)
FROB (WM_ENTERMENULOOP)
FROB (WM_EXITMENULOOP)

#if(WINVER >= 0x0400)
FROB (WM_NEXTMENU)

FROB (WM_SIZING)
FROB (WM_CAPTURECHANGED)
FROB (WM_MOVING)
FROB (WM_POWERBROADCAST)

FROB (WM_DEVICECHANGE)

#endif /* WINVER >= 0x0400 */

FROB (WM_MDICREATE)
FROB (WM_MDIDESTROY)
FROB (WM_MDIACTIVATE)
FROB (WM_MDIRESTORE)
FROB (WM_MDINEXT)
FROB (WM_MDIMAXIMIZE)
FROB (WM_MDITILE)
FROB (WM_MDICASCADE)
FROB (WM_MDIICONARRANGE)
FROB (WM_MDIGETACTIVE)


FROB (WM_MDISETMENU)
FROB (WM_ENTERSIZEMOVE)
FROB (WM_EXITSIZEMOVE)
FROB (WM_DROPFILES)
FROB (WM_MDIREFRESHMENU)

#ifdef WM_IME_SETCONTEXT /* not in Cygwin? */

#if(WINVER >= 0x0400)
FROB (WM_IME_SETCONTEXT)
FROB (WM_IME_NOTIFY)
FROB (WM_IME_CONTROL)
FROB (WM_IME_COMPOSITIONFULL)
FROB (WM_IME_SELECT)
FROB (WM_IME_CHAR)
#endif /* WINVER >= 0x0400 */
#if(WINVER >= 0x0500)
FROB (WM_IME_REQUEST)
#endif /* WINVER >= 0x0500 */
#if(WINVER >= 0x0400)
FROB (WM_IME_KEYDOWN)
FROB (WM_IME_KEYUP)
#endif /* WINVER >= 0x0400 */

#endif /* WM_IME_SETCONTEXT */

#if(_WIN32_WINNT >= 0x0400)
FROB (WM_MOUSEHOVER)
FROB (WM_MOUSELEAVE)
#endif /* _WIN32_WINNT >= 0x0400 */

FROB (WM_CUT)
FROB (WM_COPY)
FROB (WM_PASTE)
FROB (WM_CLEAR)
FROB (WM_UNDO)
FROB (WM_RENDERFORMAT)
FROB (WM_RENDERALLFORMATS)
FROB (WM_DESTROYCLIPBOARD)
FROB (WM_DRAWCLIPBOARD)
FROB (WM_PAINTCLIPBOARD)
FROB (WM_VSCROLLCLIPBOARD)
FROB (WM_SIZECLIPBOARD)
FROB (WM_ASKCBFORMATNAME)
FROB (WM_CHANGECBCHAIN)
FROB (WM_HSCROLLCLIPBOARD)
FROB (WM_QUERYNEWPALETTE)
FROB (WM_PALETTEISCHANGING)
FROB (WM_PALETTECHANGED)
FROB (WM_HOTKEY)

#if(WINVER >= 0x0400)
FROB (WM_PRINT)
FROB (WM_PRINTCLIENT)

FROB (WM_HANDHELDFIRST)
FROB (WM_HANDHELDLAST)

FROB (WM_AFXFIRST)
FROB (WM_AFXLAST)
#endif /* WINVER >= 0x0400 */

FROB (WM_PENWINFIRST)
FROB (WM_PENWINLAST)
};

#undef FROB

static void
debug_output_mswin_message (HWND hwnd, UINT message_, WPARAM wParam,
			    LPARAM lParam)
{
  Lisp_Object frame = mswindows_find_frame (hwnd);
  int i;
  char *str = 0;
  /* struct mswin_message_debug *i_hate_cranking_out_code_like_this; */

  for (i = 0; i < countof (debug_mswin_messages); i++)
    {
      if (debug_mswin_messages[i].mess == (int) message_)
	{
	  str = debug_mswin_messages[i].string;
	  break;
	}
    }

  if (str)
    stderr_out ("%s", str);
  else
    stderr_out ("%x", message_);

  if (debug_mswindows_events > 1)
    {
      stderr_out (" wparam=%d lparam=%d hwnd=%x frame: ",
		  wParam, (int) lParam, (unsigned int) hwnd);
      debug_print (frame);
      if (message_ == WM_WINDOWPOSCHANGED ||
          message_ == WM_WINDOWPOSCHANGING)
        {
          WINDOWPOS *wp = (WINDOWPOS *) lParam;
          stderr_out("  WINDOWPOS: x=%d, y=%d, h=%d, w=%d\n",
                     wp->x, wp->y, wp->cx, wp->cy);
        }
      else if (message_ == WM_MOVE)
        {
          int x = (int)(short) LOWORD(lParam);   /* horizontal position */
          int y = (int)(short) HIWORD(lParam);   /* vertical position */
          stderr_out("  MOVE: x=%d, y=%d\n", x, y);
        }
      else if (message_ == WM_SIZE)
        {
          int w = (int)(short) LOWORD(lParam);   /* width */
          int h = (int)(short) HIWORD(lParam);   /* height */
          stderr_out("  SIZE: w=%d, h=%d\n", w, h);
        }
    }
  else
    stderr_out ("\n");
}

#endif /* DEBUG_XEMACS */

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
reinit_vars_of_event_mswindows (void)
{
  mswindows_in_modal_loop = 0;
  mswindows_pending_timers_count = 0;

  mswindows_event_stream = xnew_and_zero (struct event_stream);

  mswindows_event_stream->event_pending_p 	= emacs_mswindows_event_pending_p;
  mswindows_event_stream->next_event_cb		= emacs_mswindows_next_event;
  mswindows_event_stream->handle_magic_event_cb = emacs_mswindows_handle_magic_event;
  mswindows_event_stream->format_magic_event_cb = emacs_mswindows_format_magic_event;
  mswindows_event_stream->compare_magic_event_cb= emacs_mswindows_compare_magic_event;
  mswindows_event_stream->hash_magic_event_cb   = emacs_mswindows_hash_magic_event;
  mswindows_event_stream->add_timeout_cb 	= emacs_mswindows_add_timeout;
  mswindows_event_stream->remove_timeout_cb 	= emacs_mswindows_remove_timeout;
  mswindows_event_stream->drain_queue_cb	= emacs_mswindows_drain_queue;
  mswindows_event_stream->quit_check_disallowed_p_cb = emacs_mswindows_quit_check_disallowed_p;
  mswindows_event_stream->select_console_cb 	= emacs_mswindows_select_console;
  mswindows_event_stream->unselect_console_cb	= emacs_mswindows_unselect_console;
  mswindows_event_stream->select_process_cb 	= emacs_mswindows_select_process;
  mswindows_event_stream->unselect_process_cb	= emacs_mswindows_unselect_process;
  mswindows_event_stream->create_io_streams_cb = emacs_mswindows_create_io_streams;
  mswindows_event_stream->delete_io_streams_cb = emacs_mswindows_delete_io_streams;
  mswindows_event_stream->current_event_timestamp_cb =
    emacs_mswindows_current_event_timestamp;

  dde_eval_pending = 0;
}

void
vars_of_event_mswindows (void)
{
  reinit_vars_of_event_mswindows ();

  mswindows_s_dispatch_event_queue = Qnil;
  staticpro (&mswindows_s_dispatch_event_queue);
  mswindows_s_dispatch_event_queue_tail = Qnil;
  dump_add_root_lisp_object (&mswindows_s_dispatch_event_queue_tail);

  mswindows_error_caught_in_modal_loop = 0;

#ifdef HAVE_DRAGNDROP
  Fprovide (Qdde);

  DEFVAR_LISP ("dde-advise-items", &Vdde_advise_items /*
A list of allocated DDE advise items.
Each item is an uninterned symbol, created using dde-alloc-advise-item.

The symbol's value is the data which is returned to the DDE client when
a request for the item is made (or a dde-advise call is made).

The symbol also has a 'HSZ property, which holds the DDE string handle
for the item, as a float. This is for internal use only, and should not
be modified.
*/ );
  Vdde_advise_items = Qnil;
  
  dde_eval_result = Qnil;
  staticpro (&dde_eval_result);
  dde_eval_error = Qnil;
  staticpro (&dde_eval_error);
#endif

#ifdef DEBUG_XEMACS
  DEFVAR_INT ("debug-mswindows-events", &debug_mswindows_events /*
If non-zero, display debug information about Windows messages that XEmacs sees.
Information is displayed in a console window.  Currently defined values are:

1 == non-verbose output (just the message name)
2 == verbose output (all parameters)
3 == even more verbose output (extra debugging info)
*/ );
  debug_mswindows_events = 0;
#endif

  DEFVAR_BOOL ("mswindows-alt-by-itself-activates-menu",
	       &mswindows_alt_by_itself_activates_menu /*
*Controls whether pressing and releasing the Alt key activates the menubar.
This applies only if no intervening key was pressed.  See also
`menu-accelerator-enabled', which is probably the behavior you actually want.
Default is t.
*/ );

  DEFVAR_BOOL ("mswindows-dynamic-frame-resize",
	       &mswindows_dynamic_frame_resize /*
*Controls redrawing frame contents during mouse-drag or keyboard resize
operation. When non-nil, the frame is redrawn while being resized. When
nil, frame is not redrawn, and exposed areas are filled with default
MDI application background color. Note that this option only has effect
if "Show window contents while dragging" is on in system Display/Plus!
settings.
Default is t on fast machines, nil on slow.
*/ );

  DEFVAR_INT ("mswindows-mouse-button-tolerance",
	      &mswindows_mouse_button_tolerance /*
*Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left/right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.
If negative or zero, currently set system default is used instead.
*/ );

  DEFVAR_INT ("mswindows-num-mouse-buttons", &mswindows_num_mouse_buttons /*
Number of physical mouse buttons.
*/ );

  DEFVAR_INT ("mswindows-mouse-button-max-skew-x",
	      &mswindows_mouse_button_max_skew_x /*
*Maximum horizontal distance in pixels between points in which left and
right button clicks occurred for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable `mswindows-mouse-button-tolerance'.
If negative or zero, currently set system default is used instead.
*/ );

  DEFVAR_INT ("mswindows-mouse-button-max-skew-y",
	      &mswindows_mouse_button_max_skew_y /*
*Maximum vertical distance in pixels between points in which left and
right button clicks occurred for them to be translated into single
middle button event. Clicks must occur in time not longer than defined
by the variable `mswindows-mouse-button-tolerance'.
If negative or zero, currently set system default is used instead.
*/ );

  mswindows_mouse_button_max_skew_x = 0;
  mswindows_mouse_button_max_skew_y = 0;
  mswindows_mouse_button_tolerance = 0;
  mswindows_alt_by_itself_activates_menu = 1;
}

void
syms_of_event_mswindows (void)
{
#ifdef HAVE_DRAGNDROP
  DEFSYMBOL(QHSZ);
  DEFSUBR(Fdde_alloc_advise_item);
  DEFSUBR(Fdde_free_advise_item);
  DEFSUBR(Fdde_advise);
#endif
}

void
lstream_type_create_mswindows_selectable (void)
{
#ifndef CYGWIN
  init_slurp_stream ();
  init_shove_stream ();
  init_winsock_stream ();
#endif
}

void
init_event_mswindows_late (void)
{
#ifdef CYGWIN
  windows_fd = retry_open ("/dev/windows", O_RDONLY | O_NONBLOCK, 0);
  assert (windows_fd >= 0);
  FD_SET (windows_fd, &input_wait_mask);
  FD_ZERO (&zero_mask);
#endif

  event_stream = mswindows_event_stream;

  mswindows_dynamic_frame_resize = !GetSystemMetrics (SM_SLOWMACHINE);
  mswindows_num_mouse_buttons = GetSystemMetrics (SM_CMOUSEBUTTONS);
}
