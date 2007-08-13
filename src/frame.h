/* Define frame-object for XEmacs.
   Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.

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

#ifndef _XEMACS_FRAME_H_
#define _XEMACS_FRAME_H_

#include "scrollbar.h"
#include "toolbar.h"
#include "device.h"

#define FRAME_TYPE_NAME(f) ((f)->framemeths->name)
#define FRAME_TYPE(f) ((f)->framemeths->symbol)

/******** Accessing / calling a frame method *********/

#define HAS_FRAMEMETH_P(f, m) HAS_CONTYPE_METH_P ((f)->framemeths, m)
#define FRAMEMETH(f, m, args) CONTYPE_METH ((f)->framemeths, m, args)
#define MAYBE_FRAMEMETH(f, m, args) \
  MAYBE_CONTYPE_METH ((f)->framemeths, m, args)
#define FRAMEMETH_OR_GIVEN(f, m, args, given) \
  CONTYPE_METH_OR_GIVEN((f)->framemeths, m, args, given)
#define MAYBE_INT_FRAMEMETH(f, m, args) \
  MAYBE_INT_CONTYPE_METH ((f)->framemeths, m, args)
#define MAYBE_LISP_FRAMEMETH(f, m, args) \
  MAYBE_LISP_CONTYPE_METH ((f)->framemeths, m, args)

struct frame
{
  struct lcrecord_header header;

  /* Methods for this frame's console.  This can also be retrieved
     through frame->device->console, but it's faster this way. */
  struct console_methods *framemeths;

  /* Size of this frame, in units of characters.  */
  int height;
  int width;

  /* Size of this frame, in units of pixels. */
  int pixheight;
  int pixwidth;

#ifdef HAVE_TTY
  /* The count of frame number.  This applies to TTY frames only. */
  int order_count;
#endif

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  It is normally only non-zero on
     X frames, but we put it here to avoid introducing window system
     dependencies. */
  int internal_border_width;

  /* New height and width for pending size change.  0 if no change pending.  */
  int new_height, new_width;

  /* This frame's root window mirror.  This structure exactly mirrors
     the frame's window structure but contains only pointers to the
     display structures. */
  struct window_mirror *root_mirror;

  int modiff;

#ifdef HAVE_SCROLLBARS
  /* frame-local scrollbar information.  See scrollbar.c. */
  int scrollbar_y_offset;
  int scrollbar_on_left;
  int scrollbar_on_top;

  /* cache of created scrollbars */
  struct scrollbar_instance *sb_vcache;
  struct scrollbar_instance *sb_hcache;
#endif

#ifdef HAVE_TOOLBARS
  /* We handle these specially rather than putting them in
     frameslots.h because we want them to be arrays. */

  /* Data representing each currently displayed toolbar.  See
     toolbar.c */
  Lisp_Object toolbar_data[4];

  /* Cached specifiers: */

  /* Size of the toolbars.  The frame-local toolbar space is
     subtracted before the windows are arranged.  Window and buffer
     local toolbars overlay their windows. */
  Lisp_Object toolbar_size[4];
  /* Visibility of the toolbars.  This acts as a valve for toolbar_size. */
  Lisp_Object toolbar_visible_p[4];
  /* Thickness of the border around the toolbar. */
  Lisp_Object toolbar_border_width[4];
#endif

  /* A structure of auxiliary data specific to the device type.
     struct x_frame is used for X window frames; defined in console-x.h */
  void *frame_data;

#define MARKED_SLOT(x) Lisp_Object x
#include "frameslots.h"
#undef MARKED_SLOT

    /* Nonzero if frame is currently displayed.
       Mutally exclusive with iconfied
       JV: This now a tristate flag:
Value : Emacs meaning                           :f-v-p : X meaning
0     : not displayed                           : nil  : unmapped
>0    : user can access it,needs repainting     : t    : mapped and visible
<0    : user can access it,needs no repainting  : hidden :mapped and invisible
     where f-v-p is the return value of frame-visible-p */
  int visible;

  /* one-bit flags: */

  /* Are we finished initializing? */
  unsigned int init_finished :1;

  /* Is frame marked for deletion?  This is used in XSetErrorHandler().  */
  unsigned int being_deleted :1;

  /* Nonzero if this frame has been destroyed. */
  unsigned int dead :1;

  /* Nonzero if last attempt at redisplay on this frame was preempted.  */
  unsigned int display_preempted :1;

  /* Nonzero if window is currently iconified.
     This and visible are mutually exclusive.  */
  unsigned int iconified :1;

  /* Nonzero if this frame should be cleared and then redrawn.
     Setting this will also effectively set frame_changed. */
  unsigned int clear :1;

  /* True if frame actually has a  minibuffer window on it.
     0 if using a minibuffer window that isn't on this frame.  */
  unsigned int has_minibuffer :1;

  /* True if frame's root window can't be split.  */
  unsigned int no_split :1;

  unsigned int top_toolbar_was_visible :1;
  unsigned int bottom_toolbar_was_visible :1;
  unsigned int left_toolbar_was_visible :1;
  unsigned int right_toolbar_was_visible :1;

  /* redisplay flags */
  unsigned int buffers_changed :1;
  unsigned int clip_changed :1;
  unsigned int extents_changed :1;
  unsigned int faces_changed :1;
  unsigned int frame_changed :1;
  unsigned int icon_changed :1;
  unsigned int menubar_changed :1;
  unsigned int modeline_changed :1;
  unsigned int point_changed :1;
  unsigned int size_changed :1;
  unsigned int toolbar_changed :1;
  unsigned int windows_changed :1;
  unsigned int windows_structure_changed :1;
  unsigned int window_face_cache_reset :1;	/* used by expose handler */
  unsigned int echo_area_garbaged :1;	/* used by Fredisplay_echo_area */

  unsigned int size_change_pending :1;
  unsigned int mirror_dirty :1;

  /* flag indicating if any window on this frame is displaying a subwindow */
  unsigned int subwindows_being_displayed :1;
};

/* If this is non-nil, it is the frame that make-frame is currently
   creating.  We can't set the current frame to this in case the
   debugger goes off because it would try and display to it.  However,
   there are some places which need to reference it which have no
   other way of getting it if it isn't the selected frame. */
extern Lisp_Object Vframe_being_created;

DECLARE_LRECORD (frame, struct frame);
#define XFRAME(x) XRECORD (x, frame, struct frame)
#define XSETFRAME(x, p) XSETRECORD (x, p, frame)
#define FRAMEP(x) RECORDP (x, frame)
#define GC_FRAMEP(x) GC_RECORDP (x, frame)
#define CHECK_FRAME(x) CHECK_RECORD (x, frame)
#define CONCHECK_FRAME(x) CONCHECK_RECORD (x, frame)

#define CHECK_LIVE_FRAME(x)						\
  do { CHECK_FRAME (x);							\
       if (! FRAMEP (x)							\
	   || ! FRAME_LIVE_P (XFRAME (x)))				\
         dead_wrong_type_argument (Qframe_live_p, (x)); } while (0)
#define CONCHECK_LIVE_FRAME(x)						\
  do { CONCHECK_FRAME (x);						\
       if (! FRAMEP (x)							\
	   || ! FRAME_LIVE_P (XFRAME (x)))				\
         x = wrong_type_argument (Qframe_live_p, (x)); } while (0)

#define FRAME_TYPE_P(f, type)	EQ (FRAME_TYPE (f), Q##type)

#ifdef ERROR_CHECK_TYPECHECK
MAC_DECLARE_EXTERN (struct frame *, MTframe_data)
# define FRAME_TYPE_DATA(f, type)			\
MAC_BEGIN						\
  MAC_DECLARE (struct frame *, MTframe_data, f)		\
  assert (FRAME_TYPE_P (MTframe_data, type))		\
  MAC_SEP						\
  (struct type##_frame *) MTframe_data->frame_data	\
MAC_END
#else
# define FRAME_TYPE_DATA(f, type)			\
  ((struct type##_frame *) (f)->frame_data)
#endif

#define CHECK_FRAME_TYPE(x, type)			\
  do {							\
    CHECK_FRAME (x);					\
    if (!FRAME_TYPE_P (XFRAME (x), type))		\
      dead_wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)
#define CONCHECK_FRAME_TYPE(x, type)			\
  do {							\
    CONCHECK_FRAME (x);					\
    if (!FRAME_TYPE_P (XFRAME (x), type))		\
      x = wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)

/* #### These should be in the frame-*.h files but there are
   too many places where the abstraction is broken.  Need to
   fix. */

#define FRAME_X_P(frm) CONSOLE_TYPESYM_X_P (FRAME_TYPE (frm))
#define CHECK_X_FRAME(z) CHECK_FRAME_TYPE (z, x)
#define CONCHECK_X_FRAME(z) CONCHECK_FRAME_TYPE (z, x)

#define FRAME_TTY_P(frm) CONSOLE_TYPESYM_TTY_P (FRAME_TYPE (frm))
#define CHECK_TTY_FRAME(z) CHECK_FRAME_TYPE (z, tty)
#define CONCHECK_TTY_FRAME(z) CONCHECK_FRAME_TYPE (z, tty)

#define FRAME_STREAM_P(frm) CONSOLE_TYPESYM_STREAM_P (FRAME_TYPE (frm))
#define CHECK_STREAM_FRAME(z) CHECK_FRAME_TYPE (z, stream)
#define CONCHECK_STREAM_FRAME(z) CONCHECK_FRAME_TYPE (z, stream)

#define FRAME_WIN_P(frm) CONSOLE_TYPESYM_WIN_P (FRAME_TYPE (frm))

extern Lisp_Object Qframe_live_p;
extern Lisp_Object Qframe_x_p, Qframe_tty_p;
extern Lisp_Object Vframe_title_format, Vframe_icon_title_format;

extern int frame_changed;

#define MARK_FRAME_FACES_CHANGED(f) do {				\
  (f)->faces_changed = 1;						\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_FACES_CHANGED (XDEVICE ((f)->device));			\
  else									\
    faces_changed = 1; } while (0)

#define MARK_FRAME_TOOLBARS_CHANGED(f) do {				\
  (f)->toolbar_changed = 1;						\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_TOOLBARS_CHANGED (XDEVICE ((f)->device));		\
  else									\
    toolbar_changed = 1; } while (0)

#define MARK_FRAME_SIZE_CHANGED(f) do {					\
  (f)->size_changed = 1;						\
  (f)->size_change_pending = 1;						\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_SIZE_CHANGED (XDEVICE ((f)->device));			\
  else									\
    size_changed = 1; } while (0)

#define MARK_FRAME_CHANGED(f) do {					\
  (f)->frame_changed = 1;						\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_FRAME_CHANGED (XDEVICE ((f)->device));			\
  else									\
    frame_changed = 1; } while (0)

#define MARK_FRAME_WINDOWS_CHANGED(f) do {				\
  (f)->windows_changed = 1;						\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_WINDOWS_CHANGED (XDEVICE ((f)->device));		\
  else									\
    windows_changed = 1; } while (0)

#define MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(f) do {			\
  (f)->windows_structure_changed = 1;					\
  (f)->modiff++;							\
  if (!NILP ((f)->device))						\
    MARK_DEVICE_WINDOWS_STRUCTURE_CHANGED (XDEVICE ((f)->device));	\
  else									\
    windows_structure_changed = 1; } while (0)


#define SET_FRAME_CLEAR(f) MARK_FRAME_CHANGED (f); (f)->clear = 1
#define FRAME_DEVICE(f) ((f)->device)
#define FRAME_CONSOLE(f) DEVICE_CONSOLE (XDEVICE (FRAME_DEVICE (f)))
#define FRAME_LIVE_P(f) (!(f)->dead)

#define FRAME_MINIBUF_ONLY_P(f) \
  EQ (FRAME_ROOT_WINDOW (f), FRAME_MINIBUF_WINDOW (f))
#define FRAME_HAS_MINIBUF_P(f) ((f)->has_minibuffer)
#define FRAME_HEIGHT(f) ((f)->height)
#define FRAME_WIDTH(f) ((f)->width)
#define FRAME_PIXHEIGHT(f) ((f)->pixheight)
#define FRAME_PIXWIDTH(f) ((f)->pixwidth)
#ifdef HAVE_SCROLLBARS
#define FRAME_SCROLLBAR_WIDTH(f) XINT ((f)->scrollbar_width)
#define FRAME_SCROLLBAR_HEIGHT(f) XINT ((f)->scrollbar_height)
#else
#define FRAME_SCROLLBAR_WIDTH(f) 0
#define FRAME_SCROLLBAR_HEIGHT(f) 0
#endif

#define FRAME_NEW_HEIGHT(f) ((f)->new_height)
#define FRAME_NEW_WIDTH(f) ((f)->new_width)
#define FRAME_CURSOR_X(f) ((f)->cursor_x)
#define FRAME_CURSOR_Y(f) ((f)->cursor_y)
#define FRAME_VISIBLE_P(f) ((f)->visible)
#define FRAME_REPAINT_P(f) ((f)->visible>0)
#define FRAME_NO_SPLIT_P(f) ((f)->no_split)
#define FRAME_ICONIFIED_P(f) ((f)->iconified)
#define FRAME_FOCUS_FRAME(f) ((f)->focus_frame)
#define FRAME_MINIBUF_WINDOW(f) ((f)->minibuffer_window)
#define FRAME_ROOT_WINDOW(f) ((f)->root_window)
/* Catch people attempting to set this. */
#define FRAME_SELECTED_WINDOW(f) NON_LVALUE ((f)->selected_window)
#define FRAME_LAST_NONMINIBUF_WINDOW(f) \
  NON_LVALUE ((f)->last_nonminibuf_window)
#define FRAME_SB_VCACHE(f) ((f)->sb_vcache)
#define FRAME_SB_HCACHE(f) ((f)->sb_hcache)

#if 0 /* FSFmacs */

#define FRAME_VISIBLE_P(f) ((f)->visible != 0)
#define FRAME_SET_VISIBLE(f,p) \
  ((f)->async_visible = (p), FRAME_SAMPLE_VISIBILITY (f))

/* Emacs's redisplay code could become confused if a frame's
   visibility changes at arbitrary times.  For example, if a frame is
   visible while the desired glyphs are being built, but becomes
   invisible before they are updated, then some rows of the
   desired_glyphs will be left marked as enabled after redisplay is
   complete, which should never happen.  The next time the frame
   becomes visible, redisplay will probably barf.

   Currently, there are no similar situations involving iconified, but
   the principle is the same.

   So instead of having asynchronous input handlers directly set and
   clear the frame's visibility and iconification flags, they just set
   the async_visible and async_iconified flags; the redisplay code
   calls the FRAME_SAMPLE_VISIBILITY macro before doing any redisplay,
   which sets visible and iconified from their asynchronous
   counterparts.

   Synchronous code must use the FRAME_SET_VISIBLE macro.

   Also, if a frame used to be invisible, but has just become visible,
   it must be marked as garbaged, since redisplay hasn't been keeping
   up its contents.  */
#define FRAME_SAMPLE_VISIBILITY(f) \
  (((f)->async_visible && ! (f)->visible) ? SET_FRAME_GARBAGED (f) : 0, \
   (f)->visible = (f)->async_visible, \
   (f)->iconified = (f)->async_iconified)

#endif /* FSFmacs */

#define FRAME_BORDER_WIDTH(f) ((f)->internal_border_width)
#define FRAME_BORDER_HEIGHT(f) ((f)->internal_border_width)

/* This returns the frame-local value; that tells you what you should
   use when computing the frame size.  It is *not* the actual toolbar
   size because that depends on the selected window.  Use the macros
   below for that.
*/

#ifdef HAVE_TOOLBARS
#define FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE(f, pos) \
  (!NILP((f)->toolbar_data[pos]) && !NILP ((f)->toolbar_visible_p[pos]))
#define FRAME_RAW_THEORETICAL_TOOLBAR_SIZE(f, pos) \
  (!NILP ((f)->toolbar_data[pos]) ? \
   (XINT ((f)->toolbar_size[pos])) : 0)
#define FRAME_RAW_THEORETICAL_TOOLBAR_BORDER_WIDTH(f, pos) \
  (!NILP ((f)->toolbar_data[pos]) ? \
   (XINT ((f)->toolbar_border_width[pos])) : 0)
#else
#define FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE(f, pos) 0
#define FRAME_RAW_THEORETICAL_TOOLBAR_SIZE(f, pos) 0
#define FRAME_RAW_THEORETICAL_TOOLBAR_BORDER_WIDTH(f, pos) 0
#endif

#define FRAME_THEORETICAL_TOOLBAR_SIZE(f, pos)		\
  (FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE (f, pos)	\
   ? FRAME_RAW_THEORETICAL_TOOLBAR_SIZE (f, pos)	\
   : 0)

#define FRAME_THEORETICAL_TOP_TOOLBAR_HEIGHT(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, TOP_TOOLBAR)
#define FRAME_THEORETICAL_BOTTOM_TOOLBAR_HEIGHT(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, BOTTOM_TOOLBAR)
#define FRAME_THEORETICAL_LEFT_TOOLBAR_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, LEFT_TOOLBAR)
#define FRAME_THEORETICAL_RIGHT_TOOLBAR_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, RIGHT_TOOLBAR)

#define FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH(f, pos)		\
  (FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE (f, pos)	\
   ? FRAME_RAW_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, pos)	\
   : 0)

#define FRAME_THEORETICAL_TOP_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, TOP_TOOLBAR)
#define FRAME_THEORETICAL_BOTTOM_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, BOTTOM_TOOLBAR)
#define FRAME_THEORETICAL_LEFT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, LEFT_TOOLBAR)
#define FRAME_THEORETICAL_RIGHT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, RIGHT_TOOLBAR)

/* This returns the window-local value rather than the frame-local value;
   that tells you about what's actually visible rather than what should
   be used when computing the frame size. */

#ifdef HAVE_TOOLBARS
#define FRAME_RAW_REAL_TOOLBAR_VISIBLE(f, pos) \
  (HAS_DEVMETH_P (XDEVICE (FRAME_DEVICE (f)), initialize_frame_toolbars) \
   && !NILP (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_visible_p[pos]))
#define FRAME_RAW_REAL_TOOLBAR_BORDER_WIDTH(f, pos) \
  (XINT (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_border_width[pos]))
#define FRAME_RAW_REAL_TOOLBAR_SIZE(f, pos) \
  (XINT (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_size[pos]))
#define FRAME_REAL_TOOLBAR(f, pos) \
  (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar[pos])
#else
#define FRAME_RAW_REAL_TOOLBAR_VISIBLE(f, pos) 0
#define FRAME_RAW_REAL_TOOLBAR_BORDER_WIDTH(f, pos) 0
#define FRAME_RAW_REAL_TOOLBAR_SIZE(f, pos) 0
#define FRAME_REAL_TOOLBAR(f, pos) Qnil
#endif

/* Note to Chuck
   Note to Chuck
   Note to Chuck:

   The former definitions of FRAME_REAL_FOO_TOOLBAR_VISIBLE
   looked at the toolbar data to see what was there.  The
   current ones look at the current values of the specifiers.
   This is a semantic change; the former definition returned
   what was *actually* there right at the moment, while the
   current one returns what *ought* to be there once redisplay
   has run to completion.  I think this new definition is more
   correct in almost all circumstances and is much less likely
   to lead to strange race conditions.  I'm not completely
   sure that there aren't some places in the redisplay code
   that use these macros and expect the former semantics, so
   if you encounter some odd toolbar behavior, you might want
   to look into this. --ben */

#define FRAME_REAL_TOOLBAR_VISIBLE(f, pos)	\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))	        \
  && FRAME_RAW_REAL_TOOLBAR_SIZE (f, pos) > 0)	\
   ? FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos)	\
   : 0)
#define FRAME_REAL_TOOLBAR_SIZE(f, pos)		\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))	        \
  && FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos))	\
   ? FRAME_RAW_REAL_TOOLBAR_SIZE (f, pos)	\
   : 0)
#define FRAME_REAL_TOOLBAR_BORDER_WIDTH(f, pos)		\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))		\
  && FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos))		\
   ? FRAME_RAW_REAL_TOOLBAR_BORDER_WIDTH (f, pos)	\
   : 0)

#define FRAME_REAL_TOP_TOOLBAR_HEIGHT(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, TOP_TOOLBAR)
#define FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, BOTTOM_TOOLBAR)
#define FRAME_REAL_LEFT_TOOLBAR_WIDTH(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, LEFT_TOOLBAR)
#define FRAME_REAL_RIGHT_TOOLBAR_WIDTH(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, RIGHT_TOOLBAR)

#define FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, TOP_TOOLBAR)
#define FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, BOTTOM_TOOLBAR)
#define FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, LEFT_TOOLBAR)
#define FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, RIGHT_TOOLBAR)

#define FRAME_REAL_TOP_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, TOP_TOOLBAR)
#define FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, BOTTOM_TOOLBAR)
#define FRAME_REAL_LEFT_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, LEFT_TOOLBAR)
#define FRAME_REAL_RIGHT_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, RIGHT_TOOLBAR)

#define FRAME_TOP_BORDER_START(f)					\
  (FRAME_REAL_TOP_TOOLBAR_HEIGHT (f) +					\
   2 * FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH (f))
#define FRAME_TOP_BORDER_END(f)						\
  (FRAME_TOP_BORDER_START (f) + FRAME_BORDER_HEIGHT (f))

#define FRAME_BOTTOM_BORDER_START(f)					\
  (FRAME_PIXHEIGHT (f) - FRAME_BORDER_HEIGHT (f) -			\
   FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT (f) -				\
   2 * FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH (f))
#define FRAME_BOTTOM_BORDER_END(f)					\
  (FRAME_PIXHEIGHT (f) - FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT (f) -		\
   2 * FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH (f))

#define FRAME_LEFT_BORDER_START(f)					\
  (FRAME_REAL_LEFT_TOOLBAR_WIDTH (f) +					\
   2 * FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH (f))
#define FRAME_LEFT_BORDER_END(f)					\
  (FRAME_LEFT_BORDER_START (f) + FRAME_BORDER_WIDTH (f))

#define FRAME_RIGHT_BORDER_START(f)					\
  (FRAME_PIXWIDTH (f) - FRAME_BORDER_WIDTH (f) -			\
   FRAME_REAL_RIGHT_TOOLBAR_WIDTH(f) -                                                           \
   2 * FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH (f))
#define FRAME_RIGHT_BORDER_END(f)					\
  (FRAME_PIXWIDTH (f) - FRAME_REAL_RIGHT_TOOLBAR_WIDTH (f) -		\
   2 * FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH(f))

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

extern Lisp_Object Vdefault_frame_name;

void update_frame_title (struct frame *f);
Lisp_Object next_frame (Lisp_Object f, Lisp_Object frametype,
			Lisp_Object console);
Lisp_Object prev_frame (Lisp_Object f, Lisp_Object frametype,
			Lisp_Object console);
void store_in_alist (Lisp_Object *alistptr,
		     CONST char *propname,
		     Lisp_Object val);
void pixel_to_char_size (struct frame *f, int pixel_width, int pixel_height,
			 int *char_width, int *char_height);
void char_to_pixel_size (struct frame *f, int char_width, int char_height,
			 int *pixel_width, int *pixel_height);
void round_size_to_char (struct frame *f, int in_width, int in_height,
			 int *out_width, int *out_height);
void change_frame_size (struct frame *frame,
			int newlength, int newwidth,
			int delay);
void hold_frame_size_changes (void);
void unhold_one_frame_size_changes (struct frame *f);
void unhold_frame_size_changes (void);
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
int device_matches_console_spec (Lisp_Object frame, Lisp_Object device,
				 Lisp_Object console);
Lisp_Object frame_first_window (struct frame *f);
int show_gc_cursor (struct frame *f, Lisp_Object cursor);
void set_frame_selected_window (struct frame *f, Lisp_Object window);
int is_surrogate_for_selected_frame (struct frame *f);
void update_frame_icon (struct frame *f);

#endif /* _XEMACS_FRAME_H_ */
