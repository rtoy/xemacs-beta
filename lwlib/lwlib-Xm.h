/* The lwlib interface to Motif widgets.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

The Lucid Widget Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with the Lucid Widget Library.  If not, see
<http://www.gnu.org/licenses/>. */

#ifndef INCLUDED_lwlib_Xm_h_
#define INCLUDED_lwlib_Xm_h_

#include "lwlib-internal.h"

extern const widget_creation_entry xm_creation_table [];

Widget 
xm_create_dialog (widget_instance* instance);

Widget
xm_create_label (Widget parent, widget_value* val);

Boolean
lw_motif_widget_p (Widget widget);

void
xm_update_one_widget (widget_instance* instance, Widget widget,
		      widget_value* val, Boolean deep_p);

void
xm_update_one_value (widget_instance* instance, Widget widget,
		     widget_value* val);

void
xm_destroy_instance (widget_instance* instance);

void
xm_set_keyboard_focus (Widget parent, Widget w);

void
xm_popup_menu (Widget widget, XEvent *event);

void
xm_pop_instance (widget_instance* instance, Boolean up);

extern Widget first_child (Widget);	/* garbage */

#endif /* INCLUDED_lwlib_Xm_h_ */
