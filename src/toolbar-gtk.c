/* toolbar implementation -- GTK interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.

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

#include <config.h>
#include "lisp.h"

#include "frame.h"

#include "toolbar-common.h"

#include "console-gtk-impl.h"

/* We should really create a 'common' console type and fill it with
** all the shared code.  We would then just use
** CONSOLE_INHERITS_METHOD(x,common,blah)
*/
#define gtk_output_frame_toolbars common_output_frame_toolbars
#define gtk_output_toolbar_button common_output_toolbar_button
#define gtk_redraw_exposed_toolbars common_redraw_exposed_toolbars
#define gtk_redraw_frame_toolbars common_redraw_frame_toolbars
#define gtk_clear_frame_toolbars common_clear_frame_toolbars


static void
gtk_initialize_frame_toolbars (struct frame *UNUSED (f))
{
}

/* This only calls one function but we go ahead and create this in
   case we ever do decide that we need to do more work. */
static void
gtk_free_frame_toolbars (struct frame *UNUSED (f))
{
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, output_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, clear_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, free_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, output_toolbar_button);
  CONSOLE_HAS_METHOD (gtk, redraw_exposed_toolbars);
  CONSOLE_HAS_METHOD (gtk, redraw_frame_toolbars);
}
