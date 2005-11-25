/* Stream device functions.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002, 2003 Ben Wing.

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

/* Written by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "device-impl.h"
#include "events.h"
#include "frame-impl.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "console-stream-impl.h"
#include "console-tty.h"

#include "sysfile.h"

DEFINE_CONSOLE_TYPE (stream);

Lisp_Object Vterminal_console;
Lisp_Object Vterminal_device;
Lisp_Object Vterminal_frame;

Lisp_Object Vstdio_str;

static const struct memory_description stream_console_data_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct stream_console, instream) },
  { XD_END }
};

#ifdef NEW_GC
DEFINE_LRECORD_IMPLEMENTATION ("stream-console", stream_console,
			       1, /*dumpable-flag*/
                               0, 0, 0, 0, 0,
			       stream_console_data_description_1,
			       Lisp_Stream_Console);
#else /* not NEW_GC */
const struct sized_memory_description stream_console_data_description = {
  sizeof (struct stream_console), stream_console_data_description_1
};
#endif /* not NEW_GC */

static void
stream_init_console (struct console *con, Lisp_Object UNUSED (params))
{
  Lisp_Object tty = CONSOLE_CONNECTION (con);
  struct stream_console *stream_con;

#ifdef NEW_GC
  if (CONSOLE_STREAM_DATA (con) == NULL)
    CONSOLE_STREAM_DATA (con) = alloc_lrecord_type (struct stream_console,
						    &lrecord_stream_console);
#else /* not NEW_GC */
  if (CONSOLE_STREAM_DATA (con) == NULL)
    CONSOLE_STREAM_DATA (con) = xnew_and_zero (struct stream_console);
#endif /* not NEW_GC */

  stream_con = CONSOLE_STREAM_DATA (con);

  stream_con->instream  = Qnil;

  /* Open the specified console */
  if (NILP (tty) || internal_equal (tty, Vstdio_str, 0))
    {
      stream_con->in  = stdin;
      stream_con->out = stdout;
      stream_con->err = stderr;
    }
  else
    {
      CHECK_STRING (tty);
      stream_con->in = stream_con->out = stream_con->err =
	/* #### We don't currently do coding-system translation on
	   this descriptor. */
	qxe_fopen (XSTRING_DATA (tty), READ_PLUS_TEXT);
      if (!stream_con->in)
	signal_error (Qio_error, "Unable to open tty", tty);
    }
}

static void
stream_init_device (struct device *d, Lisp_Object UNUSED (params))
{
  struct console *con = XCONSOLE (DEVICE_CONSOLE (d));

  DEVICE_INFD  (d) = fileno (CONSOLE_STREAM_DATA (con)->in);
  DEVICE_OUTFD (d) = fileno (CONSOLE_STREAM_DATA (con)->out);
  init_baud_rate (d);
  init_one_device (d);
}

static int
stream_initially_selected_for_input (struct console *UNUSED (con))
{
  return noninteractive && initialized;
}

extern int stdout_needs_newline;

static void
stream_delete_console (struct console *con)
{
  struct stream_console *stream_con = CONSOLE_STREAM_DATA (con);
  if (stream_con)
    {
      if (/* stream_con->needs_newline */
	  stdout_needs_newline) /* #### clean this up */
	{
	  fputc ('\n', stream_con->out);
	  fflush (stream_con->out);
	}
      if (stream_con->in != stdin)
	retry_fclose (stream_con->in);

#ifdef NEW_GC
      mc_free (stream_con);
#else /* not NEW_GC */
      xfree (stream_con, struct stream_console *);
#endif /* not NEW_GC */
      CONSOLE_STREAM_DATA (con) = NULL;
    }
}

Lisp_Object
stream_semi_canonicalize_console_connection (Lisp_Object connection,
					     Error_Behavior UNUSED (errb))
{
  return NILP (connection) ? Vstdio_str : connection;
}

Lisp_Object
stream_canonicalize_console_connection (Lisp_Object connection,
					Error_Behavior errb)
{
  if (NILP (connection) || internal_equal (connection, Vstdio_str, 0))
    return Vstdio_str;

  if (!ERRB_EQ (errb, ERROR_ME))
    {
      if (!STRINGP (connection))
	return Qunbound;
    }
  else
    CHECK_STRING (connection);

  return Ffile_truename (connection, Qnil);
}

Lisp_Object
stream_semi_canonicalize_device_connection (Lisp_Object connection,
					    Error_Behavior errb)
{
  return stream_semi_canonicalize_console_connection (connection, errb);
}

Lisp_Object
stream_canonicalize_device_connection (Lisp_Object connection,
				       Error_Behavior errb)
{
  return stream_canonicalize_console_connection (connection, errb);
}


static void
stream_init_frame_1 (struct frame *f, Lisp_Object UNUSED (props),
		     int frame_name_is_defaulted)
{
#if 0
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  if (!NILP (DEVICE_FRAME_LIST (d)))
    invalid_operation ("Only one frame allowed on stream devices", Qunbound);
#endif
  if (frame_name_is_defaulted)
    f->name = build_string ("stream");
  f->height = 80;
  f->width = 24;
  f->visible = 0; /* so redisplay doesn't try to do anything */
}


static int
stream_text_width (struct frame *UNUSED (f),
		   struct face_cachel *UNUSED (cachel),
		   const Ichar *UNUSED (str), Charcount len)
{
  return len;
}

static int
stream_left_margin_width (struct window *UNUSED (w))
{
  return 0;
}

static int
stream_right_margin_width (struct window *UNUSED (w))
{
  return 0;
}

static int
stream_divider_height (void)
{
  return 1;
}

static int
stream_eol_cursor_width (void)
{
  return 1;
}

/* We used to try and check for redisplaying on stream devices (e.g. in
   redisplay_device(), and beg out if so.  However, we didn't always manage
   completely.  Now we do manage completely, and to verify this we abort if
   we try to display a stream device.  This might fix some crashes I've
   been getting in pdump -- the only difference between crash and non-crash
   is a few changes to the redisplay critical-section handling. */

static void
stream_window_output_begin (struct window *UNUSED (w))
{
  ABORT ();
}

static void
stream_window_output_end (struct window *UNUSED (w))
{
  ABORT ();
}

static void
stream_frame_output_begin (struct frame *UNUSED (f))
{
  ABORT ();
}

static void
stream_frame_output_end (struct frame *UNUSED (f))
{
  ABORT ();
}

static void
stream_output_display_block (struct window *UNUSED (w),
			     struct display_line *UNUSED (dl),
			     int UNUSED (block), int UNUSED (start),
			     int UNUSED (end), int UNUSED (start_pixpos),
			     int UNUSED (cursor_start),
			     int UNUSED (cursor_width),
			     int UNUSED (cursor_height))
{
  ABORT ();
}

static void
stream_clear_region (Lisp_Object UNUSED (window), struct device* UNUSED (d),
		     struct frame *UNUSED (f), face_index UNUSED (findex),
		     int UNUSED (x), int UNUSED (y), int UNUSED (width),
		     int UNUSED (height), Lisp_Object UNUSED (fcolor),
		     Lisp_Object UNUSED (bcolor),
		     Lisp_Object UNUSED (background_pixmap))
{
  ABORT ();
}

static int
stream_flash (struct device *UNUSED (d))
{
  return 0; /* sorry can't do it */
}

static void
stream_ring_bell (struct device *d, int UNUSED (volume), int UNUSED (pitch),
		  int UNUSED (duration))
{
  struct console *c = XCONSOLE (DEVICE_CONSOLE (d));
  /* Don't output ^G when not a TTY -- in particular, under MS Windows, ^G
     is interpreted as bell by the console, but not when running under
     VC++.  Probably this would be the same under Unix. */
  if (isatty (fileno (CONSOLE_STREAM_DATA (c)->out)))
    {
      fputc (07, CONSOLE_STREAM_DATA (c)->out);
      fflush (CONSOLE_STREAM_DATA (c)->out);
    }
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_stream (void)
{
  INITIALIZE_CONSOLE_TYPE (stream, "stream", "console-stream-p");

  /* console methods */
  CONSOLE_HAS_METHOD (stream, init_console);
  CONSOLE_HAS_METHOD (stream, initially_selected_for_input);
  CONSOLE_HAS_METHOD (stream, delete_console);
  CONSOLE_HAS_METHOD (stream, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (stream, canonicalize_device_connection);
  CONSOLE_HAS_METHOD (stream, semi_canonicalize_console_connection);
  CONSOLE_HAS_METHOD (stream, semi_canonicalize_device_connection);

  /* device methods */
  CONSOLE_HAS_METHOD (stream, init_device);

  /* frame methods */
  CONSOLE_HAS_METHOD (stream, init_frame_1);

  /* redisplay methods */
  CONSOLE_HAS_METHOD (stream, text_width);
  CONSOLE_HAS_METHOD (stream, left_margin_width);
  CONSOLE_HAS_METHOD (stream, right_margin_width);
  CONSOLE_HAS_METHOD (stream, divider_height);
  CONSOLE_HAS_METHOD (stream, eol_cursor_width);
  CONSOLE_HAS_METHOD (stream, window_output_begin);
  CONSOLE_HAS_METHOD (stream, window_output_end);
  CONSOLE_HAS_METHOD (stream, frame_output_begin);
  CONSOLE_HAS_METHOD (stream, frame_output_end);
  CONSOLE_HAS_METHOD (stream, output_display_block);
  CONSOLE_HAS_METHOD (stream, clear_region);
  CONSOLE_HAS_METHOD (stream, flash);
  CONSOLE_HAS_METHOD (stream, ring_bell);
}

void
reinit_console_type_create_stream (void)
{
  REINITIALIZE_CONSOLE_TYPE (stream);
}

void
vars_of_console_stream (void)
{
  DEFVAR_LISP ("terminal-console", &Vterminal_console /*
The initial console object, which represents XEmacs' stdout.
*/ );
  Vterminal_console = Qnil;

  DEFVAR_LISP ("terminal-device", &Vterminal_device /*
The initial device object, which represents XEmacs' stdout.
*/ );
  Vterminal_device = Qnil;

  DEFVAR_LISP ("terminal-frame", &Vterminal_frame /*
The initial frame object, which represents XEmacs' stdout.
*/ );
  Vterminal_frame = Qnil;

  /* Moved from console-tty.c */
  Vstdio_str = build_string ("stdio");
  staticpro (&Vstdio_str);
}

#ifndef PDUMP
void
init_console_stream (int UNUSED (reinit))
{
  /* This function can GC */
  if (!initialized)
    {
      Vterminal_device = Fmake_device (Qstream, Qnil, Qnil);
      Vterminal_console = Fdevice_console (Vterminal_device);
      Vterminal_frame = Fmake_frame (Qnil, Vterminal_device);
      minibuf_window = XFRAME (Vterminal_frame)->minibuffer_window;
    }
  else
    {
      /* Re-initialize the FILE fields of the console. */
      stream_init_console (XCONSOLE (Vterminal_console), Qnil);
      if (noninteractive)
        event_stream_select_console (XCONSOLE (Vterminal_console));
    }
}

#else

void
init_console_stream (int reinit)
{
  /* This function can GC */
  if (!reinit)
    {
      Vterminal_device = Fmake_device (Qstream, Qnil, Qnil);
      Vterminal_console = Fdevice_console (Vterminal_device);
      Vterminal_frame = Fmake_frame (Qnil, Vterminal_device);
      minibuf_window = XFRAME (Vterminal_frame)->minibuffer_window;
    }
  if (initialized)
    {
      stream_init_console (XCONSOLE (Vterminal_console), Qnil);
      if (noninteractive)
	event_stream_select_console (XCONSOLE (Vterminal_console));
    }
}
#endif
