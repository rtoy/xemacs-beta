/* toolbar-common.h
**
** Description: 
**
** Created by: William M. Perry
** Copyright (c) 2001 Free Software Foundation
**
*/

#ifndef __TOOLBAR_COMMON_H__
#define __TOOLBAR_COMMON_H__

extern void common_output_frame_toolbars (struct frame *f);
extern void common_redraw_exposed_toolbars (struct frame *f,
					    int x, int y,
					    int width, int height);
extern void common_redraw_frame_toolbars (struct frame *f);
extern void common_output_toolbar_button (struct frame *f, Lisp_Object button);

#endif /* __TOOLBAR_COMMON_H__ */
