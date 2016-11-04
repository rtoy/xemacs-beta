/* Defines some widget utility functions.
   Copyright (C) 1992 Lucid, Inc.

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

#ifndef INCLUDED_lwlib_utils_h_
#define INCLUDED_lwlib_utils_h_

void destroy_all_children (Widget widget);
void XtNoClearRefreshWidget (Widget);

typedef void (*XtApplyToWidgetsProc) (Widget, XtPointer);
typedef void* (*XtApplyUntilToWidgetsProc) (Widget, XtPointer);

void XtApplyToWidgets (Widget, XtApplyToWidgetsProc, XtPointer);
void *XtApplyUntilToWidgets (Widget, XtApplyUntilToWidgetsProc, XtPointer);

Widget *XtCompositeChildren (Widget, unsigned int *);

/* returns True is the widget is being destroyed, False otherwise */
Boolean
XtWidgetBeingDestroyedP (Widget widget);

void XtSafelyDestroyWidget (Widget);

#ifdef USE_DEBUG_MALLOC
#include <dmalloc.h>
#endif

#endif /* INCLUDED_lwlib_utils_h_ */
