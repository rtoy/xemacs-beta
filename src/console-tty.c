/* TTY console functions.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

/* Authors: Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "console-tty.h"
#include "console-stream.h"
#include "events.h" /* for Vcontrolling_terminal */
#include "faces.h"
#include "frame.h"
#include "lstream.h"
#include "redisplay.h"
#include "sysdep.h"
#ifdef MULE
#include "mule-coding.h"
#endif

DEFINE_CONSOLE_TYPE (tty);

Lisp_Object Qterminal_type;

Lisp_Object Vstdio_str;


static void
allocate_tty_console_struct (struct console *con)
{
  con->console_data =
    (struct tty_console *) xmalloc (sizeof (struct tty_console));

  /* zero out all slots. */
  memset (con->console_data, 0, sizeof (struct tty_console));
  /* except the lisp ones ... */
  CONSOLE_TTY_DATA (con)->terminal_type = Qnil;
  CONSOLE_TTY_DATA (con)->instream = Qnil;
  CONSOLE_TTY_DATA (con)->outstream = Qnil;
}

static void
tty_init_console (struct console *con, Lisp_Object props)
{
  Lisp_Object tty = CONSOLE_CONNECTION (con), terminal_type = Qnil;
  int infd, outfd;
  struct gcpro gcpro1;

  GCPRO1 (terminal_type);

  terminal_type = Fplist_get (props, Qterminal_type, Qnil);

  /* Determine the terminal type */

  if (!NILP (terminal_type))
    CHECK_STRING (terminal_type);
  else
    {
      char *temp_type = (char *) getenv ("TERM");

      if (!temp_type)
	{
	  error ("Cannot determine terminal type");
	}
      else
	terminal_type = build_string (temp_type);
    }

  /* Open the specified console */

  allocate_tty_console_struct (con);
  if (!NILP (Fequal (tty, Vstdio_str)))
    {
      infd = fileno (stdin);
      outfd = fileno (stdout);
      CONSOLE_TTY_DATA (con)->is_stdio = 1;
    }
  else
    {
      infd = outfd = open ((char *) XSTRING_DATA (tty), O_RDWR);
      if (infd < 0)
	error ("Unable to open tty %s", XSTRING_DATA (tty));
      CONSOLE_TTY_DATA (con)->is_stdio = 0;
    }
  
  CONSOLE_TTY_DATA (con)->infd  = infd;
  CONSOLE_TTY_DATA (con)->outfd = outfd;
  CONSOLE_TTY_DATA (con)->instream  = make_filedesc_input_stream  (infd, 0,
								   -1, 0);
  CONSOLE_TTY_DATA (con)->outstream = make_filedesc_output_stream (outfd, 0,
								   -1, 0);
#ifdef MULE
  CONSOLE_TTY_DATA (con)->instream =
    make_decoding_input_stream (XLSTREAM (CONSOLE_TTY_DATA (con)->instream),
				Fget_coding_system (Vkeyboard_coding_system));
  Lstream_set_character_mode (XLSTREAM (CONSOLE_TTY_DATA (con)->instream));
  CONSOLE_TTY_DATA (con)->outstream =
    make_encoding_output_stream (XLSTREAM (CONSOLE_TTY_DATA (con)->outstream),
				 Fget_coding_system (Vterminal_coding_system));
#endif /* MULE */
  CONSOLE_TTY_DATA (con)->terminal_type = terminal_type;
  if (NILP (CONSOLE_NAME (con)))
    CONSOLE_NAME (con) = Ffile_name_nondirectory (tty);
  {
    int tty_pg;
    int controlling_tty_pg;
    int cfd;

    /* OK, the only sure-fire way I can think of to determine
       whether a particular TTY is our controlling TTY is to check
       if it has the same foreground process group as our controlling
       TTY.  This is OK because a process group can never simultaneously
       be the foreground process group of two TTY's (in that case it
       would have two controlling TTY's, which is not allowed). */

    EMACS_GET_TTY_PROCESS_GROUP (infd, &tty_pg);
    cfd = open ("/dev/tty", O_RDWR, 0);
    EMACS_GET_TTY_PROCESS_GROUP (cfd, &controlling_tty_pg);
    close (cfd);
    if (tty_pg == controlling_tty_pg)
      {
	CONSOLE_TTY_DATA (con)->controlling_terminal = 1;
	XSETCONSOLE (Vcontrolling_terminal, con);
	munge_tty_process_group ();
      }
    else
      CONSOLE_TTY_DATA (con)->controlling_terminal = 0;
  }

  UNGCPRO;
}

static void
tty_mark_console (struct console *con, void (*markobj) (Lisp_Object))
{
  ((markobj) (CONSOLE_TTY_DATA (con)->terminal_type));
  ((markobj) (CONSOLE_TTY_DATA (con)->instream));
  ((markobj) (CONSOLE_TTY_DATA (con)->outstream));
}

static int
tty_initially_selected_for_input (struct console *con)
{
  return 1;
}

static void
free_tty_console_struct (struct console *con)
{
  struct tty_console *tcon = (struct tty_console *) con->console_data;
  if (tcon && tcon->term_entry_buffer) /* allocated in term_init () */
    xfree (tcon->term_entry_buffer);
  if (tcon)
    xfree (tcon);
}

static void
tty_delete_console (struct console *con)
{
  Lstream_close (XLSTREAM (CONSOLE_TTY_DATA (con)->instream));
  Lstream_close (XLSTREAM (CONSOLE_TTY_DATA (con)->outstream));
  if (!CONSOLE_TTY_DATA (con)->is_stdio)
    close (CONSOLE_TTY_DATA (con)->infd);
  if (CONSOLE_TTY_DATA (con)->controlling_terminal)
    {
      Vcontrolling_terminal = Qnil;
      unmunge_tty_process_group ();
    }
  free_tty_console_struct (con);
}


static struct console *
decode_tty_console (Lisp_Object console)
{
  XSETCONSOLE (console, decode_console (console));
  CHECK_TTY_CONSOLE (console);
  return XCONSOLE (console);
}

DEFUN ("console-tty-terminal-type", Fconsole_tty_terminal_type, 0, 1, 0, /*
Return the terminal type of TTY console CONSOLE.
*/
       (console))
{
  return CONSOLE_TTY_DATA (decode_tty_console (console))->terminal_type;
}

Lisp_Object
tty_semi_canonicalize_console_connection (Lisp_Object connection,
					  Error_behavior errb)
{
  if (NILP (connection))
    return Vstdio_str;

  return connection;
}

Lisp_Object
tty_canonicalize_console_connection (Lisp_Object connection,
				     Error_behavior errb)
{
  if (NILP (connection) || !NILP (Fequal (connection, Vstdio_str)))
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
tty_semi_canonicalize_device_connection (Lisp_Object connection,
					 Error_behavior errb)
{
  return tty_semi_canonicalize_console_connection (connection, errb);
}

Lisp_Object
tty_canonicalize_device_connection (Lisp_Object connection,
				    Error_behavior errb)
{
  return tty_canonicalize_console_connection (connection, errb);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_console_tty (void)
{
  DEFSUBR (Fconsole_tty_terminal_type);
  defsymbol (&Qterminal_type, "terminal-type");
}

void
console_type_create_tty (void)
{
  INITIALIZE_CONSOLE_TYPE (tty, "tty", "console-tty-p");

  /* console methods */
  CONSOLE_HAS_METHOD (tty, init_console);
  CONSOLE_HAS_METHOD (tty, mark_console);
  CONSOLE_HAS_METHOD (tty, initially_selected_for_input);
  CONSOLE_HAS_METHOD (tty, delete_console);
  CONSOLE_HAS_METHOD (tty, canonicalize_console_connection);
  CONSOLE_HAS_METHOD (tty, canonicalize_device_connection);
  CONSOLE_HAS_METHOD (tty, semi_canonicalize_console_connection);
  CONSOLE_HAS_METHOD (tty, semi_canonicalize_device_connection);
}

void
vars_of_console_tty (void)
{
  Fprovide (Qtty);

  Vstdio_str = build_string ("stdio");
  staticpro (&Vstdio_str);
}
