/* Implements a lightweight menubar widget.
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

#ifndef INCLUDED_xlwmenuP_h_
#define INCLUDED_xlwmenuP_h_

#include "xlwmenu.h"
#include <X11/CoreP.h>

#ifdef HAVE_XFT_MENUBARS
#include <X11/Xft/Xft.h>
#endif


/* Elements in the stack arrays. */
typedef struct _window_state
{
  Window	window;
  Position	x;
  Position	y;
  Dimension	width;
  Dimension	height;
  Dimension	label_width;
  Dimension	toggle_width;
} window_state;


/* New fields for the XlwMenu widget instance record */
typedef struct _XlwMenu_part 
{
  /* slots set by the resources */

#if defined(NEED_MOTIF) && !defined(HAVE_XFT_MENUBARS)
  XmFontList	font_list;
  XmFontList	font_list_2;
  XmFontList	fallback_font_list;
#else
  XFontStruct *	font;
#ifdef HAVE_XFT_MENUBARS
  String fcFontName;
  String xftFontName;
  XftFont *renderFont;
#endif
# ifdef USE_XFONTSET
  XFontSet font_set;
# endif
#endif
  Dimension	font_ascent, font_descent;  /* extracted from font/fontlist */

  Pixel		foreground;
  Pixel		button_foreground;
  Pixel		highlight_foreground;
  Pixel		title_foreground;
  Dimension	margin;
  Dimension	horizontal_margin;
  Dimension	vertical_margin;
  Dimension	column_spacing;
  Dimension	shadow_thickness;
  Dimension	indicator_size;
  Pixel 	top_shadow_color;
  Pixel 	bottom_shadow_color;
  Pixel 	select_color;
#ifdef HAVE_XFT_MENUBARS
#endif
  Pixmap	top_shadow_pixmap;
  Pixmap	bottom_shadow_pixmap;
  Cursor	cursor_shape;
  XtCallbackList	open;
  XtCallbackList	select;
  widget_value*	contents;
  int		horizontal;
  Boolean	use_backing_store;
  Boolean	bounce_down;
  Boolean       lookup_labels;
  
  /* State of the XlwMenu */
  int			old_depth;
  widget_value**	old_stack;
  int			old_stack_length;

  /* New state after the user moved */
  int			new_depth;
  widget_value**	new_stack;
  int			new_stack_length;

  /* Window resources */
  window_state*		windows;
  int			windows_length;

  /* Internal part, set by the XlwMenu */
  GC			foreground_gc;
  GC			button_gc;
  GC			background_gc;
  GC			inactive_gc;
  GC			inactive_button_gc;
  GC			shadow_top_gc;
  GC			shadow_bottom_gc;
  GC			select_gc;
  GC			highlight_gc;
  GC			title_gc;
  Cursor		cursor;
  Boolean		popped_up;
  Pixmap		gray_pixmap;

  /* Stay-up stuff */
  Boolean               pointer_grabbed;
  Boolean		next_release_must_exit;
  Time			menu_post_time, menu_bounce_time;
  widget_value *	last_selected_val;
} XlwMenuPart;

/* Full instance record declaration */
typedef struct _XlwMenuRec 
{
  CorePart	core;
  XlwMenuPart	menu;
} XlwMenuRec;

/* New fields for the XlwMenu widget class record */
typedef struct 
{
  int	dummy;
} XlwMenuClassPart;

/* Full class record declaration. */
typedef struct _XlwMenuClassRec 
{
  CoreClassPart		core_class;
  XlwMenuClassPart	menu_class;
} XlwMenuClassRec;

/* Class pointer. */
extern XlwMenuClassRec xlwMenuClassRec;

#endif /* INCLUDED_xlwmenuP_h_ */
