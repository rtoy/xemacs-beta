/* Window definitions for XEmacs.
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002 Ben Wing.
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

/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_window_h_
#define INCLUDED_window_h_

#include "redisplay.h"
#ifdef HAVE_SCROLLBARS
#include "scrollbar.h"
#endif

struct window;

DECLARE_LRECORD (window, struct window);
#define XWINDOW(x) XRECORD (x, window, struct window)
#define wrap_window(p) wrap_record (p, window)
#define WINDOWP(x) RECORDP (x, window)
#define CHECK_WINDOW(x) CHECK_RECORD (x, window)
#define CONCHECK_WINDOW(x) CONCHECK_RECORD (x, window)

/* Basic properties available to non-privileged users; redefined in
   window-impl.h */

int window_live_p (struct window *w);
Lisp_Object window_frame (struct window *w);
Lisp_Object window_buffer (struct window *w);

#define WINDOW_LIVE_P(w) window_live_p (w)
#define WINDOW_FRAME(w) window_frame (w)
#define WINDOW_BUFFER(w) window_buffer (w)

#define WINDOW_XFRAME(w) XFRAME (WINDOW_FRAME (w))
#define WINDOW_DEVICE(w) XFRAME_DEVICE (WINDOW_FRAME (w))
#define WINDOW_XDEVICE(w) XDEVICE (WINDOW_DEVICE (w))
#define WINDOW_CONSOLE(w) XDEVICE_CONSOLE (WINDOW_DEVICE (w))
#define WINDOW_XCONSOLE(w) XCONSOLE (WINDOW_CONSOLE (w))
#define WINDOW_XBUFFER(w) XBUFFER (WINDOW_BUFFER (w))

#define XWINDOW_FRAME(w) WINDOW_FRAME (XWINDOW (w))
#define XWINDOW_XFRAME(w) XFRAME (XWINDOW_FRAME (w))
#define XWINDOW_DEVICE(w) XFRAME_DEVICE (XWINDOW_FRAME (w))
#define XWINDOW_XDEVICE(w) XDEVICE (XWINDOW_DEVICE (w))
#define XWINDOW_CONSOLE(w) XDEVICE_CONSOLE (XWINDOW_DEVICE (w))
#define XWINDOW_XCONSOLE(w) XCONSOLE (XWINDOW_CONSOLE (w))
#define XWINDOW_BUFFER(w) WINDOW_BUFFER (XWINDOW (w))
#define XWINDOW_XBUFFER(w) XBUFFER (XWINDOW_BUFFER (w))

#define CHECK_LIVE_WINDOW(x) do {			\
  CHECK_WINDOW (x);					\
  if (!WINDOW_LIVE_P (XWINDOW (x)))			\
    dead_wrong_type_argument (Qwindow_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_WINDOW(x) do {			\
  CONCHECK_WINDOW (x);					\
  if (!WINDOW_LIVE_P (XWINDOW (x)))			\
    x = wrong_type_argument (Qwindow_live_p, (x));	\
} while (0)

struct window_mirror;

DECLARE_LRECORD (window_mirror, struct window_mirror);
#define XWINDOW_MIRROR(x) XRECORD (x, window_mirror, struct window_mirror)
#define wrap_window_mirror(p) wrap_record (p, window_mirror)
#define WINDOW_MIRRORP(x) RECORDP (x, window_mirror)
#define CHECK_WINDOW_MIRROR(x) CHECK_RECORD (x, window_mirror)
#define CONCHECK_WINDOW_MIRROR(x) CONCHECK_RECORD (x, window_mirror)

DECLARE_LRECORD (window_configuration, struct window_config);

EXFUN (Fget_buffer_window, 3);
EXFUN (Fmove_to_window_line, 2);
EXFUN (Frecenter, 2);
EXFUN (Freplace_buffer_in_windows, 3);
EXFUN (Fselect_window, 2);
EXFUN (Fselected_window, 1);
EXFUN (Fset_window_buffer, 3);
EXFUN (Fset_window_hscroll, 2);
EXFUN (Fset_window_point, 2);
EXFUN (Fset_window_start, 3);
EXFUN (Fwindow_buffer, 1);
EXFUN (Fwindow_highest_p, 1);
EXFUN (Fwindow_point, 1);
EXFUN (Fwindow_start, 1);
EXFUN (Fcurrent_window_configuration, 1);

Lisp_Object save_window_excursion_unwind (Lisp_Object);
Lisp_Object display_buffer (Lisp_Object, Lisp_Object, Lisp_Object);

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; but you can test for minibufferness of
   the selected window or of any window that is displayed.  */
extern Lisp_Object minibuf_window;

/* Prompt to display in front of the minibuffer contents, or nil */
extern Lisp_Object Vminibuf_prompt;
/* Prompt to display in front of the minibuffer prompt, or nil */
extern Lisp_Object Vminibuf_preprompt;

Lisp_Object allocate_window (void);
int window_char_width (struct window *, int include_margins_p);
int window_char_height (struct window *, int include_gutters_p);
int window_displayed_height (struct window *);
int window_is_leftmost (struct window *w);
int window_is_rightmost (struct window *w);
int window_is_lowest (struct window *w);
int window_is_highest (struct window *w);
int window_truncation_on (struct window *w);
int window_needs_vertical_divider (struct window *);
int window_scrollbar_width (struct window *w);
int window_scrollbar_height (struct window *w);
int window_modeline_height (struct window *w);
int window_left_margin_width (struct window *w);
int window_right_margin_width (struct window *w);
int window_top_gutter_height (struct window *w);
int window_bottom_gutter_height (struct window *w);
int window_left_gutter_width (struct window *w, int modeline);
int window_right_gutter_width (struct window *w, int modeline);

void delete_all_subwindows (struct window *w);
void set_window_pixheight (Lisp_Object window, int pixheight,
			   int nodelete);
void set_window_pixwidth (Lisp_Object window, int pixwidth,
			  int nodelete);
void window_scroll (Lisp_Object window, Lisp_Object n, int direction,
		    Error_Behavior errb);
int buffer_window_count (struct buffer *b, struct frame *f);
int buffer_window_mru (struct window *w);
void check_frame_size (struct frame *frame, int *rows, int *cols);
int frame_pixsize_valid_p (struct frame *frame, int width, int height);
int frame_size_valid_p (struct frame *frame, int rows, int cols);
struct window *decode_window (Lisp_Object window);
struct window *find_window_by_pixel_pos (int pix_x, int pix_y, Lisp_Object win);

void free_window_mirror (struct window_mirror *mir);
Lisp_Object real_window (struct window_mirror *mir, int no_abort);
struct window_mirror *find_window_mirror (struct window *w);
display_line_dynarr *window_display_lines (struct window *w, int);
struct buffer *window_display_buffer (struct window *w);
void set_window_display_buffer (struct window *w, struct buffer *b);
void update_frame_window_mirror (struct frame *f);

int map_windows (struct frame *f,
		 int (*mapfun) (struct window *w, void *closure),
		 void *closure);
void some_window_value_changed (Lisp_Object specifier, struct window *w,
				Lisp_Object oldval);
int invalidate_vertical_divider_cache_in_window (struct window *w,
						 void *u_n_u_s_e_d);
int window_divider_width (struct window *w);

#endif /* INCLUDED_window_h_ */
