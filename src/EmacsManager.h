/* Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

/* Written by Ben Wing. */

#ifndef _EmacsManager_h
#define _EmacsManager_h

#ifndef XtNresizeCallback
#define XtNresizeCallback "resizeCallback"
#endif

#ifndef XtNqueryGeometryCallback
#define XtNqueryGeometryCallback "queryGeometryCallback"
#endif

#ifndef XtNuserData
#define XtNuserData "userData"
#endif
#ifndef XtCUserData
#define XtCUserData "UserData"
#endif

/* scrollbar placement types; like in ScrolledW.h */

#define EM_TOP          1
#define EM_BOTTOM       0
#define EM_LEFT         2
#define EM_RIGHT        0
 
#define XtTOP_LEFT      (EM_TOP | EM_LEFT)
#define XtBOTTOM_LEFT   (EM_BOTTOM  | EM_LEFT)
#define XtTOP_RIGHT     (EM_TOP | EM_RIGHT)
#define XtBOTTOM_RIGHT  (EM_BOTTOM  | EM_RIGHT)
 
typedef struct _EmacsManagerClassRec *EmacsManagerWidgetClass;
typedef struct _EmacsManagerRec *EmacsManagerWidget;
extern WidgetClass emacsManagerWidgetClass;

/* External entry points */
typedef struct
{
  Dimension width, height;
} EmacsManagerResizeStruct;

typedef struct
{
  Dimension proposed_width, proposed_height;
  XtGeometryMask request_mode;
} EmacsManagerQueryGeometryStruct;

void EmacsManagerChangeSize (Widget w, Dimension width, Dimension height);

#endif /* _EmacsManager_h */
