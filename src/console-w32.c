/* Console functions for win32.
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

/* Authorship:

   Ben Wing: January 1996, for 19.14.
   Rewritten for win32 by Jonathan Harris, November 1997 for 20.4.
 */

#include <config.h>
#include "lisp.h"

#include "console-w32.h"

DEFINE_CONSOLE_TYPE (w32);


static int
w32_initially_selected_for_input (struct console *con)
{
  return 1;
}




/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_console_w32 (void)
{
}

void
console_type_create_w32 (void)
{
  INITIALIZE_CONSOLE_TYPE (w32, "w32", "console-w32-p");

  /* console methods */
/*  CONSOLE_HAS_METHOD (w32, init_console); */
/*  CONSOLE_HAS_METHOD (w32, mark_console); */
  CONSOLE_HAS_METHOD (w32, initially_selected_for_input);
/*  CONSOLE_HAS_METHOD (w32, delete_console); */
/*  CONSOLE_HAS_METHOD (w32, canonicalize_console_connection); */
/*  CONSOLE_HAS_METHOD (w32, canonicalize_device_connection); */
/*  CONSOLE_HAS_METHOD (w32, semi_canonicalize_console_connection); */
/*  CONSOLE_HAS_METHOD (w32, semi_canonicalize_device_connection); */
}

void
vars_of_console_w32 (void)
{
  Fprovide (Qw32);
}
