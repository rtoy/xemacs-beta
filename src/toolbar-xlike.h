/* toolbar-xlike.h
**
** Description: 
**
** Created by: William M. Perry
** Copyright (c) 2001 Free Software Foundation
**
** This file is part of XEmacs.
**
** XEmacs is free software: you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation, either version 3 of the License, or (at your
** option) any later version.
** 
** XEmacs is distributed in the hope that it will be useful, but WITHOUT
** ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
** for more details.
** 
** You should have received a copy of the GNU General Public License
** along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef __TOOLBAR_XLIKE_H__
#define __TOOLBAR_XLIKE_H__

extern void xlike_output_frame_toolbars (struct frame *f);
extern void xlike_redraw_exposed_toolbars (struct frame *f,
					    int x, int y,
					    int width, int height);
extern void xlike_redraw_frame_toolbars (struct frame *f);
extern void xlike_output_toolbar_button (struct frame *f, Lisp_Object button);
extern void xlike_clear_frame_toolbars (struct frame *f);

#endif /* __TOOLBAR_XLIKE_H__ */
