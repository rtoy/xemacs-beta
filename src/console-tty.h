/* Define TTY specific console, device, and frame object for XEmacs.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1996, 2002 Ben Wing.

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

/* Written by Chuck Thompson and Ben Wing. */

/* NOTE: Currently each TTY console can have only one device.
   Therefore, all stuff for both input and output is lumped into
   the console structure.  If it ever becomes meaningful to
   have more than one device on a TTY console, the output stuff
   will have to get separated out. */

#ifndef INCLUDED_console_tty_h_
#define INCLUDED_console_tty_h_

#include "console.h"
#include "systty.h"

extern FILE *termscript;

EXFUN (Fconsole_tty_controlling_process, 1);

/******************     Prototypes from cm.c     *******************/

/* #### Verify that all of these are still needed. */

void cm_cost_init (struct console *c);
void cmputc (int c);
void cmgoto (struct frame *f, int row, int col);
extern struct console *cmputc_console;
void send_string_to_tty_console (struct console *c, unsigned char *str,
				 int len);

/***************     Prototypes from console-tty.c     ****************/

extern const struct sized_memory_description tty_console_data_description;


/***************     Prototypes from objects-tty.c     ****************/

extern const struct sized_memory_description tty_color_instance_data_description;
extern const struct sized_memory_description tty_font_instance_data_description;


/***************     Prototypes from redisplay-tty.c     ****************/

enum term_init_status
{
  TTY_UNABLE_OPEN_DATABASE,
  TTY_TYPE_UNDEFINED,
  TTY_TYPE_INSUFFICIENT,
  TTY_SIZE_UNSPECIFIED,
  TTY_INIT_SUCCESS
};

int init_tty_for_redisplay (struct device *d, char *terminal_type);
/* #### These should probably be methods. */
void set_tty_modes (struct console *c);
void reset_tty_modes (struct console *c);

/* Used in sysdep.c to properly clear and position the cursor when exiting. */
void tty_redisplay_shutdown (struct console *c);

/* called from console-stream.c */
Lisp_Object tty_semi_canonicalize_console_connection (Lisp_Object connection,
						      Error_Behavior errb);
Lisp_Object tty_canonicalize_console_connection (Lisp_Object connection,
						 Error_Behavior errb);
Lisp_Object tty_semi_canonicalize_device_connection (Lisp_Object connection,
						     Error_Behavior errb);
Lisp_Object tty_canonicalize_device_connection (Lisp_Object connection,
						Error_Behavior errb);

#endif /* INCLUDED_console_tty_h_ */
