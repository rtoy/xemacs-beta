/* Generic GUI code. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

Lisp_Object Q_active, Q_suffix, Q_keys, Q_style, Q_selected;
Lisp_Object Q_filter, Q_config, Q_included;
Lisp_Object Q_accelerator;
Lisp_Object Qtoggle, Qradio;

void
syms_of_gui (void)
{
  defkeyword (&Q_active,   ":active");
  defkeyword (&Q_suffix,   ":suffix");
  defkeyword (&Q_keys,     ":keys");
  defkeyword (&Q_style,    ":style");
  defkeyword (&Q_selected, ":selected");
  defkeyword (&Q_filter,   ":filter");
  defkeyword (&Q_config,   ":config");
  defkeyword (&Q_included, ":included");
  defkeyword (&Q_accelerator, ":accelerator");

  defsymbol (&Qtoggle, "toggle");
  defsymbol (&Qradio, "radio");
}

void
vars_of_gui (void)
{
}
