/* toolbar-xlike.h
**
** Description: 
**
** Created by: William M. Perry
** Copyright (c) 2001 Free Software Foundation
**
*/

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
