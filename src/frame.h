/* Define frame-object for XEmacs.
   Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 2002 Ben Wing.

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

#ifndef INCLUDED_frame_h_
#define INCLUDED_frame_h_

#include "redisplay.h"
#include "console.h" /* for error_check_frame_type */

struct frame;

EXFUN (Fselected_frame, 1);
EXFUN (Fdelete_frame, 2);
EXFUN (Fframe_iconified_p, 1);
EXFUN (Fframe_name, 1);
EXFUN (Fframe_property, 3);
EXFUN (Fmake_frame, 2);
EXFUN (Fmake_frame_visible, 1);
EXFUN (Fraise_frame, 1);
EXFUN (Fselect_frame, 1);
EXFUN (Fset_frame_pointer, 2);
EXFUN (Fset_frame_position, 3);
EXFUN (Fset_frame_properties, 2);
EXFUN (Fset_frame_size, 4);

extern Lisp_Object Qbackground_toolbar_color, Qbell_volume, Qborder_color;
extern Lisp_Object Qborder_width, Qbottom_toolbar_shadow_color;
extern Lisp_Object Qbottom_toolbar_shadow_pixmap, Qdelete_frame;
extern Lisp_Object Qdeselect_frame_hook, Qdrag_and_drop_functions, Qgc_pointer;
extern Lisp_Object Qiconic, Qinitially_unmapped, Qinter_line_space;
extern Lisp_Object Qinternal_border_width, Qinvisible, Qmap_frame_hook;
extern Lisp_Object Qminibuffer, Qmodeline_pointer, Qmouse_enter_frame_hook;
extern Lisp_Object Qmouse_leave_frame_hook, Qpointer_background;
extern Lisp_Object Qpointer_color, Qpopup, Qscrollbar_placement;
extern Lisp_Object Qselect_frame_hook, Qspace_pointer;
extern Lisp_Object Qsynchronize_minibuffers, Qtext_pointer;
extern Lisp_Object Qtoolbar_shadow_thickness, Qtop_toolbar_shadow_color;
extern Lisp_Object Qtop_toolbar_shadow_pixmap, Qunmap_frame_hook;
extern Lisp_Object Qunsplittable, Quse_backing_store, Qvisible, Qvisual_bell;
extern Lisp_Object Vframe_icon_title_format, Vframe_title_format;
extern Lisp_Object Vmouse_motion_handler;

DECLARE_LRECORD (frame, struct frame);
#define XFRAME(x) XRECORD (x, frame, struct frame)
#define wrap_frame(p) wrap_record (p, frame)
#define FRAMEP(x) RECORDP (x, frame)
#define CHECK_FRAME(x) CHECK_RECORD (x, frame)
#define CONCHECK_FRAME(x) CONCHECK_RECORD (x, frame)

/* Basic properties available to non-privileged users; redefined in
   frame-impl.h */

int frame_live_p (struct frame *f);
Lisp_Object frame_device (struct frame *f);

#define FRAME_LIVE_P(f) frame_live_p (f)
#define FRAME_DEVICE(f) frame_device (f)

#define FRAME_XDEVICE(f) XDEVICE (FRAME_DEVICE (f))
#define FRAME_CONSOLE(f) XDEVICE_CONSOLE (FRAME_DEVICE (f))
#define FRAME_XCONSOLE(f) XCONSOLE (FRAME_CONSOLE (f))

#define XFRAME_DEVICE(f) FRAME_DEVICE (XFRAME (f))
#define XFRAME_XDEVICE(f) XDEVICE (XFRAME_DEVICE (f))
#define XFRAME_CONSOLE(f) XDEVICE_CONSOLE (XFRAME_DEVICE (f))
#define XFRAME_XCONSOLE(f) XCONSOLE (XFRAME_CONSOLE (f))

#define CHECK_LIVE_FRAME(x) do {			\
  CHECK_FRAME (x);					\
  if (! FRAME_LIVE_P (XFRAME (x)))			\
    dead_wrong_type_argument (Qframe_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_FRAME(x) do {			\
  CONCHECK_FRAME (x);					\
  if (! FRAME_LIVE_P (XFRAME (x)))			\
    x = wrong_type_argument (Qframe_live_p, (x));	\
} while (0)

#define FW_FRAME(obj)					\
   (WINDOWP (obj) ? WINDOW_FRAME (XWINDOW (obj))	\
    : (FRAMEP  (obj) ? obj				\
       : Qnil))

/* Equivalent in FSF Emacs:

   FOR_EACH_FRAME (LIST_VAR, FRAME_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vframe_list.  The
   loop will set FRAME_VAR, a Lisp_Object, to each frame in
   Vframe_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object too; it is used to iterate through the
   Vframe_list.
   */

/* NO_BREAK means that "break" doesn't do what you think it does!
   Use goto instead.  "continue" is OK, though. */
#define FRAME_LOOP_NO_BREAK(frmcons, devcons, concons)		\
  DEVICE_LOOP_NO_BREAK (devcons, concons)			\
    DEVICE_FRAME_LOOP (frmcons, XDEVICE (XCAR (devcons)))

void update_frame_title (struct frame *f);
Lisp_Object next_frame (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object previous_frame (Lisp_Object, Lisp_Object, Lisp_Object);
void pixel_to_char_size (struct frame *f, int pixel_width, int pixel_height,
			 int *char_width, int *char_height);
void char_to_pixel_size (struct frame *f, int char_width, int char_height,
			 int *pixel_width, int *pixel_height);
void round_size_to_char (struct frame *f, int in_width, int in_height,
			 int *out_width, int *out_height);
void pixel_to_real_char_size (struct frame *f, int pixel_width, int pixel_height,
			 int *char_width, int *char_height);
void char_to_real_pixel_size (struct frame *f, int char_width, int char_height,
			 int *pixel_width, int *pixel_height);
void round_size_to_real_char (struct frame *f, int in_width, int in_height,
			      int *out_width, int *out_height);
void change_frame_size (struct frame *frame,
			int newlength, int newwidth,
			int delay);
void adjust_frame_size (struct frame *frame);
void frame_size_slipped (Lisp_Object specifier, struct frame *f,
			 Lisp_Object oldval);
int enter_redisplay_critical_section (void);
void exit_redisplay_critical_section (int);
void select_frame_1 (Lisp_Object frame);
void select_frame_2 (Lisp_Object frame);
struct frame *selected_frame (void);
struct frame *device_selected_frame (struct device *d);
struct frame *decode_frame (Lisp_Object frame);
struct frame *decode_frame_or_selected (Lisp_Object cdf);
Lisp_Object make_frame (struct frame *f);
int other_visible_frames (struct frame *f);
void delete_frame_internal (struct frame *f, int force,
			    int called_from_delete_device,
			    int from_io_error);
void io_error_delete_frame (Lisp_Object frame);
Lisp_Object find_some_frame (int (*predicate) (Lisp_Object, void *),
			     void *closure);
int device_matches_device_spec (Lisp_Object device, Lisp_Object device_spec);
Lisp_Object frame_first_window (struct frame *f);
int show_gc_cursor (struct frame *f, Lisp_Object cursor);
void set_frame_selected_window (struct frame *f, Lisp_Object window);
int is_surrogate_for_selected_frame (struct frame *f);
void update_frame_icon (struct frame *f);
void invalidate_vertical_divider_cache_in_frame (struct frame *f);

void init_frame (void);

#endif /* INCLUDED_frame_h_ */
