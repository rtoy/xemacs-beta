/* The lwlib interface to "xlwmenu" menus.
   Copyright (C) 1992, 1994 Lucid, Inc.

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

#ifndef LWLIB_XLW_H
#define LWLIB_XLW_H

#include "lwlib-internal.h"

extern const widget_creation_entry xlw_creation_table [];
extern widget_creation_function xlw_create_dialog;

Boolean
lw_lucid_widget_p (Widget widget);

void
xlw_update_one_widget (widget_instance* instance, Widget widget,
		       widget_value* val, Boolean deep_p);

void
xlw_update_one_value (widget_instance* instance, Widget widget,
		      widget_value* val);

void
xlw_destroy_instance (widget_instance* instance);

void
xlw_pop_instance (widget_instance* instance, Boolean up);

void
xlw_popup_menu (Widget widget, XEvent *event);

#endif /* LWLIB_XLW_H */
