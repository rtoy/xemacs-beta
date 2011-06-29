/* Define frame-object for XEmacs.
   Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_frame_impl_h_
#define INCLUDED_frame_impl_h_

#include "console-impl.h" /* for error_check_frame_type */
#include "frame.h"

#define FRAME_TYPE_NAME(f) ((f)->framemeths->name)
#define FRAME_TYPE(f) ((f)->framemeths->symbol)

/******** Accessing / calling a frame method *********/

#define HAS_FRAMEMETH_P(f, m) HAS_CONTYPE_METH_P ((f)->framemeths, m)
#define FRAMEMETH(f, m, args) CONTYPE_METH ((f)->framemeths, m, args)
#define MAYBE_FRAMEMETH(f, m, args) \
  MAYBE_CONTYPE_METH ((f)->framemeths, m, args)
#define FRAMEMETH_OR_GIVEN(f, m, args, given) \
  CONTYPE_METH_OR_GIVEN((f)->framemeths, m, args, given)

struct frame
{
  NORMAL_LISP_OBJECT_HEADER header;

  /* Methods for this frame's console.  This can also be retrieved
     through frame->device->console, but it's faster this way. */
  struct console_methods *framemeths;

  /* Duplicates framemeths->symbol.  See comment in struct console. */
  enum console_variant frametype;

  /* Size of text only area of this frame, excluding scrollbars,
     toolbars and end of line glyphs. The size can be in characters
     or pixels, depending on units in which window system resizes
     its windows */
  int height, width;

  /* New height and width for pending size change, in the same units
     as above. 0 if no change pending.  */
  int new_height, new_width;

  /* Size of text-only are of the frame, in default font characters.
     This may be inaccurate due to rounding error */
  int char_height, char_width;

  /* Size of the whole frame, including scrollbars, toolbars and end
     of line glyphs, in pixels */
  int pixheight, pixwidth;

#ifdef HAVE_TTY
  /* The count of frame number.  This applies to TTY frames only. */
  int order_count;
#endif

  /* Current page number for a printer frame. */
  int page_number;

  /* Width of the internal border.  This is a line of background color
     just inside the window's border.  It is normally only non-zero on
     X frames, but we put it here to avoid introducing window system
     dependencies. */
  int internal_border_width;

  int modiff;

  struct expose_ignore *subwindow_exposures;
  struct expose_ignore *subwindow_exposures_tail;

#ifdef HAVE_SCROLLBARS
  /* frame-local scrollbar information.  See scrollbar.c. */
  int scrollbar_y_offset;

  /* cache of created scrollbars */
  struct scrollbar_instance *sb_vcache;
  struct scrollbar_instance *sb_hcache;
#endif

#ifdef HAVE_TOOLBARS
  /* Size of toolbars as seen by redisplay. This is used to determine
     whether to re-layout windows by a call to change_frame_size early
     in redisplay_frame. */
  int current_toolbar_size[NUM_EDGES];

#ifdef HAVE_GTK
  void *gtk_toolbars[NUM_EDGES];
  unsigned int gtk_toolbar_checksum[NUM_EDGES];
#endif
  
#endif

  /* Size of gutters as seen by redisplay. This is used to determine
     whether to re-layout windows by a call to change_frame_size early
     in redisplay_frame. */
  int current_gutter_bounds[NUM_EDGES];

  /* Toolbar visibility */
  int toolbar_was_visible[NUM_EDGES];

  /* gutter visibility */
  int gutter_was_visible[NUM_EDGES];

  /* Dynamic arrays of display lines for gutters */
  display_line_dynarr *current_display_lines[NUM_EDGES];
  display_line_dynarr *desired_display_lines[NUM_EDGES];

  /* A structure of auxiliary data specific to the device type.  For
     example, struct x_frame is for X window frames; defined in
     console-x-impl.h. */
  void *frame_data;

#define FRAME_SLOT_DECLARATION
#define MARKED_SLOT(x) Lisp_Object x;
#include "frameslots.h"

    /* Nonzero if frame is currently displayed.
       Mutually exclusive with iconified
       JV: This now a tristate flag:
Value : Emacs meaning                           :f-v-p : X meaning
0     : not displayed                           : nil  : unmapped
>0    : user can access it,needs repainting     : t    : mapped and visible
<0    : user can access it,needs no repainting  : hidden :mapped and invisible
     where f-v-p is the return value of frame-visible-p */
  int visible;

  /* one-bit flags: */

  /* Is focusing onto this frame disabled? (Modal dialog boxes) */
  unsigned int disabled :1;

  /* Are we finished initializing? */
  unsigned int init_finished :1;

  /* Is frame marked for deletion?  This is used in XSetErrorHandler().  */
  unsigned int being_deleted :1;

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

  /* redisplay flags */
  unsigned int buffers_changed :1;
  unsigned int clip_changed :1;
  unsigned int extents_changed :1;
  unsigned int faces_changed :1;
  unsigned int frame_changed :1;
  unsigned int frame_layout_changed :1;	/* The layout of frame
 					   elements has changed. */
  unsigned int subwindows_changed :1;
  unsigned int subwindows_state_changed :1;
  unsigned int glyphs_changed :1;
  unsigned int icon_changed :1;
  unsigned int menubar_changed :1;
  unsigned int modeline_changed :1;
  unsigned int point_changed :1;
  unsigned int size_changed :1;
  unsigned int toolbar_changed :1;
  unsigned int gutter_changed :1;
  unsigned int windows_changed :1;
  unsigned int windows_structure_changed :1;
  unsigned int window_face_cache_reset :1;	/* used by expose handler */
  unsigned int echo_area_garbaged :1;	/* used by Fredisplay_echo_area */
  unsigned int size_slipped :1;

  unsigned int size_change_pending :1;
  unsigned int mirror_dirty :1;

  /* flag indicating if any window on this frame is displaying a subwindow */
  unsigned int subwindows_being_displayed :1;
};

/* Redefine basic properties more efficiently */

#undef FRAME_LIVE_P
#define FRAME_LIVE_P(f) (!EQ (FRAME_TYPE (f), Qdead))
#undef FRAME_DEVICE
#define FRAME_DEVICE(f) ((f)->device)

#define FRAME_TYPE_P(f, type)	EQ (FRAME_TYPE (f), Q##type)

#ifdef ERROR_CHECK_TYPES
DECLARE_INLINE_HEADER (
struct frame *
error_check_frame_type (struct frame * f, Lisp_Object sym)
)
{
  assert (EQ (FRAME_TYPE (f), sym));
  return f;
}
# define FRAME_TYPE_DATA(f, type)			\
 ((struct type##_frame *) error_check_frame_type (f, Q##type)->frame_data)
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

#define FRAME_DISPLAY_P(frm)				\
  (DEVICE_DISPLAY_P (XDEVICE (FRAME_DEVICE (frm))))

#define CHECK_DISPLAY_FRAME(frm)			\
  do {							\
    CHECK_FRAME (frm);					\
    CHECK_LIVE_FRAME (frm);				\
    CHECK_DISPLAY_DEVICE (FRAME_DEVICE (XFRAME (frm)));	\
  } while (0)

#define CONCHECK_DISPLAY_FRAME(frm)			\
  do {							\
    CONCHECK_FRAME (frm);				\
    CONCHECK_LIVE_FRAME (frm);				\
    CONCHECK_DISPLAY_DEVICE (FRAME_DEVICE (XFRAME (frm))); \
  } while (0)

#define FRAME_PRINTER_P(frm)				\
  (DEVICE_PRINTER_P (XDEVICE (FRAME_DEVICE (frm))))

#define CHECK_PRINTER_FRAME(frm)			\
  do {							\
    CHECK_FRAME (frm);					\
    CHECK_LIVE_FRAME (frm);				\
    CHECK_PRINTER_DEVICE (FRAME_DEVICE (XFRAME (frm)));	\
  } while (0)

#define CONCHECK_PRINTER_FRAME(frm)			\
  do {							\
    CONCHECK_FRAME (frm);				\
    CONCHECK_LIVE_FRAME (frm);				\
    CONCHECK_PRINTER_DEVICE (FRAME_DEVICE (XFRAME (frm))); \
  } while (0)

/* #### These should be in the frame-*.h files but there are
   too many places where the abstraction is broken.  Need to
   fix. */

#define FRAME_X_P(frm) CONSOLE_TYPESYM_X_P (FRAME_TYPE (frm))
#define CHECK_X_FRAME(z) CHECK_FRAME_TYPE (z, x)
#define CONCHECK_X_FRAME(z) CONCHECK_FRAME_TYPE (z, x)

#define FRAME_GTK_P(frm) CONSOLE_TYPESYM_GTK_P (FRAME_TYPE (frm))
#define CHECK_GTK_FRAME(z) CHECK_FRAME_TYPE (z, gtk)
#define CONCHECK_GTK_FRAME(z) CONCHECK_FRAME_TYPE (z, gtk)

#define FRAME_TTY_P(frm) CONSOLE_TYPESYM_TTY_P (FRAME_TYPE (frm))
#define CHECK_TTY_FRAME(z) CHECK_FRAME_TYPE (z, tty)
#define CONCHECK_TTY_FRAME(z) CONCHECK_FRAME_TYPE (z, tty)

#define FRAME_STREAM_P(frm) CONSOLE_TYPESYM_STREAM_P (FRAME_TYPE (frm))
#define CHECK_STREAM_FRAME(z) CHECK_FRAME_TYPE (z, stream)
#define CONCHECK_STREAM_FRAME(z) CONCHECK_FRAME_TYPE (z, stream)

#define FRAME_WIN_P(frm) CONSOLE_TYPESYM_WIN_P (FRAME_TYPE (frm))

extern int frame_changed;

#define MARK_FRAME_FACES_CHANGED(f) do {		\
  struct frame *mffc_f = (f);				\
  mffc_f->faces_changed = 1;				\
  mffc_f->modiff++;					\
  if (!NILP (mffc_f->device))				\
    {							\
      struct device *mffc_d = XDEVICE (mffc_f->device);	\
      MARK_DEVICE_FACES_CHANGED (mffc_d);		\
    }							\
  else							\
    faces_changed = 1;					\
} while (0)

#define MARK_FRAME_GLYPHS_CHANGED(f) do {		\
  struct frame *mfgc_f = (f);				\
  mfgc_f->glyphs_changed = 1;				\
  mfgc_f->modiff++;					\
  if (!NILP (mfgc_f->device))				\
    {							\
      struct device *mfgc_d = XDEVICE (mfgc_f->device);	\
      MARK_DEVICE_GLYPHS_CHANGED (mfgc_d);		\
    }							\
  else							\
    glyphs_changed = 1;					\
} while (0)

#define MARK_FRAME_SUBWINDOWS_CHANGED(f) do {		\
  struct frame *mfgc_f = (f);				\
  mfgc_f->subwindows_changed = 1;			\
  mfgc_f->modiff++;					\
  if (!NILP (mfgc_f->device))				\
    {							\
      struct device *mfgc_d = XDEVICE (mfgc_f->device);	\
      MARK_DEVICE_SUBWINDOWS_CHANGED (mfgc_d);		\
    }							\
  else							\
    subwindows_changed = 1;				\
} while (0)

#define MARK_FRAME_SUBWINDOWS_STATE_CHANGED(f) do {	\
  struct frame *mfgc_f = (f);				\
  mfgc_f->subwindows_state_changed = 1;			\
  mfgc_f->modiff++;					\
  if (!NILP (mfgc_f->device))				\
    {							\
      struct device *mfgc_d = XDEVICE (mfgc_f->device);	\
      MARK_DEVICE_SUBWINDOWS_STATE_CHANGED (mfgc_d);	\
    }							\
  else							\
    subwindows_state_changed = 1;			\
} while (0)

#define MARK_FRAME_TOOLBARS_CHANGED(f) do {		\
  struct frame *mftc_f = (f);				\
  mftc_f->toolbar_changed = 1;				\
  mftc_f->modiff++;					\
  if (!NILP (mftc_f->device))				\
    {							\
      struct device *mftc_d = XDEVICE (mftc_f->device);	\
      MARK_DEVICE_TOOLBARS_CHANGED (mftc_d);		\
    }							\
  else							\
    toolbar_changed = 1;				\
} while (0)

#define MARK_FRAME_GUTTERS_CHANGED(f) do {		\
  struct frame *mftc_f = (f);				\
  mftc_f->gutter_changed = 1;				\
  mftc_f->modiff++;					\
  if (!NILP (mftc_f->device))				\
    {							\
      struct device *mftc_d = XDEVICE (mftc_f->device);	\
      MARK_DEVICE_GUTTERS_CHANGED (mftc_d);		\
    }							\
  else							\
    gutter_changed = 1;					\
} while (0)

#define MARK_FRAME_SIZE_CHANGED(f) do {			\
  struct frame *mfsc_f = (f);				\
  mfsc_f->size_changed = 1;				\
  mfsc_f->size_change_pending = 1;			\
  mfsc_f->modiff++;					\
  if (!NILP (mfsc_f->device))				\
    {							\
      struct device *mfsc_d = XDEVICE (mfsc_f->device);	\
      MARK_DEVICE_SIZE_CHANGED (mfsc_d);		\
    }							\
  else							\
    size_changed = 1;					\
} while (0)

#define MARK_FRAME_CHANGED(f) do {			\
  struct frame *mfc_f = (f);				\
  mfc_f->frame_changed = 1;				\
  mfc_f->modiff++;					\
  if (!NILP (mfc_f->device))				\
    {							\
      struct device *mfc_d = XDEVICE (mfc_f->device);	\
      MARK_DEVICE_FRAME_CHANGED (mfc_d);		\
    }							\
  else							\
    frame_changed = 1;					\
} while (0)

#define MARK_FRAME_LAYOUT_CHANGED(f) do {		\
  struct frame *mfc_f = (f);				\
  mfc_f->frame_layout_changed = 1;			\
  mfc_f->modiff++;					\
  if (!NILP (mfc_f->device))				\
    {							\
      struct device *mfc_d = XDEVICE (mfc_f->device);	\
      MARK_DEVICE_FRAME_LAYOUT_CHANGED (mfc_d);		\
    }							\
  else							\
    frame_layout_changed = 1;				\
} while (0)

#define MARK_FRAME_WINDOWS_CHANGED(f) do {		\
  struct frame *mfwc_f = (f);				\
  mfwc_f->windows_changed = 1;				\
  mfwc_f->modiff++;					\
  if (!NILP (mfwc_f->device))				\
    {							\
      struct device *mfwc_d = XDEVICE (mfwc_f->device);	\
      MARK_DEVICE_WINDOWS_CHANGED (mfwc_d);		\
    }							\
  else							\
    windows_changed = 1;				\
} while (0)

#define MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(f) do {	\
  struct frame *fwsc_f = (f);				\
  fwsc_f->windows_structure_changed = 1;		\
  fwsc_f->modiff++;					\
  if (!NILP (fwsc_f->device))				\
    {							\
      struct device *fwsc_d = XDEVICE (fwsc_f->device);	\
      MARK_DEVICE_WINDOWS_STRUCTURE_CHANGED (fwsc_d);	\
    }							\
  else							\
    windows_structure_changed = 1;			\
  invalidate_vertical_divider_cache_in_frame (fwsc_f);	\
} while (0)

#define MARK_FRAME_SIZE_SLIPPED(f) do {			\
  struct frame *fwsc_f = (f);				\
  fwsc_f->size_slipped = 1;				\
  fwsc_f->modiff++;					\
  if (!NILP (fwsc_f->device))				\
    {							\
      struct device *fwsc_d = XDEVICE (fwsc_f->device);	\
      MARK_DEVICE_FRAME_CHANGED (fwsc_d);		\
    }							\
  else							\
    frame_changed = 1;					\
} while (0)

#define CLEAR_FRAME_SIZE_SLIPPED(f) do {		\
  struct frame *fwsc_f = (f);				\
  fwsc_f->size_slipped = 0;				\
} while (0)

#define SET_FRAME_CLEAR(f) MARK_FRAME_CHANGED (f); (f)->clear = 1

#define FRAME_MINIBUF_ONLY_P(f) \
  EQ (FRAME_ROOT_WINDOW (f), FRAME_MINIBUF_WINDOW (f))
#define FRAME_HAS_MINIBUF_P(f)  ((f)->has_minibuffer)
#define FRAME_HEIGHT(f)         ((f)->height)
#define FRAME_WIDTH(f)          ((f)->width)
#define FRAME_CHARHEIGHT(f)     ((f)->char_height)
#define FRAME_CHARWIDTH(f)      ((f)->char_width)
#define FRAME_PIXHEIGHT(f)      ((f)->pixheight)
#define FRAME_PIXWIDTH(f)       ((f)->pixwidth)
#define FRAME_PAGENUMBER(f)     ((f)->page_number + 0)
#define FRAME_SET_PAGENUMBER(f,x) (f)->page_number = (x);
#ifdef HAVE_SCROLLBARS
#define FRAME_SCROLLBAR_WIDTH(f)		\
  (NILP ((f)->vertical_scrollbar_visible_p) ?	\
    0 : XINT ((f)->scrollbar_width))
#define FRAME_SCROLLBAR_HEIGHT(f)		\
  (NILP ((f)->horizontal_scrollbar_visible_p) ?	\
    0 : XINT ((f)->scrollbar_height))
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
#define FRAME_SELECTED_XWINDOW(f) XWINDOW (FRAME_SELECTED_WINDOW (f))
#define FRAME_LAST_NONMINIBUF_WINDOW(f) \
  NON_LVALUE ((f)->last_nonminibuf_window)
#define FRAME_SB_VCACHE(f) ((f)->sb_vcache)
#define FRAME_SB_HCACHE(f) ((f)->sb_hcache)
#define FRAME_SUBWINDOW_CACHE(f) ((f)->subwindow_instance_cache)

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

#define FRAME_INTERNAL_BORDER_WIDTH(f) ((f)->internal_border_width)
#define FRAME_INTERNAL_BORDER_HEIGHT(f) ((f)->internal_border_width)
#define FRAME_INTERNAL_BORDER_SIZE(f, pos) ((f)->internal_border_width)

/************************************************************************/
/*	                        toolbars      				*/
/************************************************************************/

/*---------------- Theoretical and real toolbar values ----------------*/


/* This returns the frame-local value; that tells you what you should
   use when computing the frame size.  It is *not* the actual toolbar
   size because that depends on the selected window.  Use the macros
   below for that.
*/

#ifdef HAVE_TOOLBARS
#define FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE(f, pos) \
  (!NILP((f)->toolbar_buttons[pos]) && !NILP ((f)->toolbar_visible_p[pos]))
#define FRAME_RAW_THEORETICAL_TOOLBAR_SIZE(f, pos) \
  (!NILP ((f)->toolbar_buttons[pos]) && INTP((f)->toolbar_size[pos]) ? \
   (XINT ((f)->toolbar_size[pos])) : 0)
#define FRAME_RAW_THEORETICAL_TOOLBAR_BORDER_WIDTH(f, pos) \
  (!NILP ((f)->toolbar_buttons[pos]) && INTP((f)->toolbar_border_width[pos]) ? \
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
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, TOP_EDGE)
#define FRAME_THEORETICAL_BOTTOM_TOOLBAR_HEIGHT(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, BOTTOM_EDGE)
#define FRAME_THEORETICAL_LEFT_TOOLBAR_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, LEFT_EDGE)
#define FRAME_THEORETICAL_RIGHT_TOOLBAR_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_SIZE (f, RIGHT_EDGE)

#define FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH(f, pos)		\
  (FRAME_RAW_THEORETICAL_TOOLBAR_VISIBLE (f, pos)		\
   ? FRAME_RAW_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, pos)	\
   : 0)

#define FRAME_THEORETICAL_TOP_TOOLBAR_BORDER_WIDTH(f)   \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, TOP_EDGE)
#define FRAME_THEORETICAL_BOTTOM_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, BOTTOM_EDGE)
#define FRAME_THEORETICAL_LEFT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, LEFT_EDGE)
#define FRAME_THEORETICAL_RIGHT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_THEORETICAL_TOOLBAR_BORDER_WIDTH (f, RIGHT_EDGE)

/* This returns the window-local value rather than the frame-local value;
   that tells you about what's actually visible rather than what should
   be used when computing the frame size. */

#ifdef HAVE_TOOLBARS
#define FRAME_RAW_REAL_TOOLBAR_VISIBLE(f, pos) \
  (HAS_FRAMEMETH_P (f, initialize_frame_toolbars) \
   && !NILP (XWINDOW (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_visible_p[pos]))
#define FRAME_RAW_REAL_TOOLBAR_BORDER_WIDTH(f, pos) \
     ((INTP (XWINDOW \
	     (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_border_width[pos])) ? \
      (XINT (XWINDOW \
	     (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_border_width[pos])) \
      : 0)
#define FRAME_RAW_REAL_TOOLBAR_SIZE(f, pos) \
     ((INTP (XWINDOW \
	     (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_size[pos])) ? \
      (XINT (XWINDOW \
	     (FRAME_LAST_NONMINIBUF_WINDOW (f))->toolbar_size[pos])) : 0)
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

#define FRAME_REAL_TOOLBAR_VISIBLE(f, pos)		\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))			\
  && FRAME_RAW_REAL_TOOLBAR_SIZE (f, pos) > 0)		\
   ? FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos)		\
   : 0)
#define FRAME_REAL_TOOLBAR_SIZE(f, pos)			\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))	        	\
  && FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos))		\
   ? FRAME_RAW_REAL_TOOLBAR_SIZE (f, pos)		\
   : 0)
#define FRAME_REAL_TOOLBAR_BORDER_WIDTH(f, pos)		\
  ((!NILP (FRAME_REAL_TOOLBAR (f, pos))			\
  && FRAME_RAW_REAL_TOOLBAR_VISIBLE (f, pos))		\
   ? FRAME_RAW_REAL_TOOLBAR_BORDER_WIDTH (f, pos)	\
   : 0)

#define FRAME_REAL_TOOLBAR_BOUNDS(f, pos)		\
  (FRAME_REAL_TOOLBAR_SIZE (f, pos) +			\
   2 * FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, pos))

#define FRAME_REAL_TOP_TOOLBAR_HEIGHT(f)        \
  FRAME_REAL_TOOLBAR_SIZE (f, TOP_EDGE)
#define FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, BOTTOM_EDGE)
#define FRAME_REAL_LEFT_TOOLBAR_WIDTH(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, LEFT_EDGE)
#define FRAME_REAL_RIGHT_TOOLBAR_WIDTH(f) \
  FRAME_REAL_TOOLBAR_SIZE (f, RIGHT_EDGE)

#define FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH(f)  \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, TOP_EDGE)
#define FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, BOTTOM_EDGE)
#define FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, LEFT_EDGE)
#define FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH(f) \
  FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, RIGHT_EDGE)

#define FRAME_REAL_TOP_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, TOP_EDGE)
#define FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, BOTTOM_EDGE)
#define FRAME_REAL_LEFT_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, LEFT_EDGE)
#define FRAME_REAL_RIGHT_TOOLBAR_VISIBLE(f) \
  FRAME_REAL_TOOLBAR_VISIBLE (f, RIGHT_EDGE)

#define FRAME_REAL_TOP_TOOLBAR_BOUNDS(f)        \
  FRAME_REAL_TOOLBAR_BOUNDS (f, TOP_EDGE)
#define FRAME_REAL_BOTTOM_TOOLBAR_BOUNDS(f) \
  FRAME_REAL_TOOLBAR_BOUNDS (f, BOTTOM_EDGE)
#define FRAME_REAL_LEFT_TOOLBAR_BOUNDS(f) \
  FRAME_REAL_TOOLBAR_BOUNDS (f, LEFT_EDGE)
#define FRAME_REAL_RIGHT_TOOLBAR_BOUNDS(f) \
  FRAME_REAL_TOOLBAR_BOUNDS (f, RIGHT_EDGE)

/************************************************************************/
/*         frame dimensions defined using toolbars and gutters          */
/************************************************************************/

/* Bounds of the area framed by the toolbars is the client area --
   (0, 0) - (FRAME_PIXWIDTH, FRAME_PIXHEIGHT). */

/* Bounds of the area framed by the internal border width -- inside of the
   toolbars, outside of everything else. */

#define FRAME_TOP_INTERNAL_BORDER_START(f)				\
  FRAME_REAL_TOP_TOOLBAR_BOUNDS (f)
#define FRAME_TOP_INTERNAL_BORDER_END(f)				\
  (FRAME_TOP_INTERNAL_BORDER_START (f) + FRAME_INTERNAL_BORDER_HEIGHT (f))

#define FRAME_BOTTOM_INTERNAL_BORDER_START(f)				\
  (FRAME_BOTTOM_INTERNAL_BORDER_END (f) - FRAME_INTERNAL_BORDER_HEIGHT (f))
#define FRAME_BOTTOM_INTERNAL_BORDER_END(f)				\
  (FRAME_PIXHEIGHT (f) - FRAME_REAL_BOTTOM_TOOLBAR_BOUNDS (f))

#define FRAME_LEFT_INTERNAL_BORDER_START(f)				\
  FRAME_REAL_LEFT_TOOLBAR_BOUNDS (f)
#define FRAME_LEFT_INTERNAL_BORDER_END(f)				\
  (FRAME_LEFT_INTERNAL_BORDER_START (f) + FRAME_INTERNAL_BORDER_WIDTH (f))

#define FRAME_RIGHT_INTERNAL_BORDER_START(f)				\
  (FRAME_RIGHT_INTERNAL_BORDER_END (f) - FRAME_INTERNAL_BORDER_WIDTH (f))
#define FRAME_RIGHT_INTERNAL_BORDER_END(f)				\
  (FRAME_PIXWIDTH (f) - FRAME_REAL_RIGHT_TOOLBAR_BOUNDS (f))

/* Bounds of the area framed by the gutter -- inside of the
   toolbars and internal border width. */

#define FRAME_TOP_GUTTER_START(f)					\
  FRAME_TOP_INTERNAL_BORDER_END (f)
#define FRAME_TOP_GUTTER_END(f)						\
  (FRAME_TOP_GUTTER_START (f) + FRAME_TOP_GUTTER_BOUNDS (f))

#ifdef BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER
#define FRAME_BOTTOM_GUTTER_START(f)					\
  (FRAME_BOTTOM_GUTTER_END (f) - FRAME_BOTTOM_GUTTER_BOUNDS (f))
#define FRAME_BOTTOM_GUTTER_END(f)					\
  FRAME_BOTTOM_INTERNAL_BORDER_START (f)
#endif /* BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER */

#define FRAME_LEFT_GUTTER_START(f)					\
  FRAME_LEFT_INTERNAL_BORDER_END (f)
#define FRAME_LEFT_GUTTER_END(f)					\
  (FRAME_LEFT_GUTTER_START (f) + FRAME_LEFT_GUTTER_BOUNDS (f))

#define FRAME_RIGHT_GUTTER_START(f)					\
  (FRAME_RIGHT_GUTTER_END (f) - FRAME_RIGHT_GUTTER_BOUNDS (f))
#define FRAME_RIGHT_GUTTER_END(f)					\
  FRAME_RIGHT_INTERNAL_BORDER_START (f)

/* These are the bounds of the paned area -- inside of the toolbars,
   gutters, and internal border width.  The paned area is the same as the
   area occupied by windows, including the minibuffer.  See long comment in
   frame.c. */

#define FRAME_PANED_TOP_EDGE(f) FRAME_TOP_GUTTER_END (f)
#ifdef BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER
#define FRAME_PANED_BOTTOM_EDGE(f) FRAME_BOTTOM_GUTTER_START (f)
#endif /* BOTTOM_GUTTER_IS_OUTSIDE_MINIBUFFER */
#define FRAME_PANED_LEFT_EDGE(f) FRAME_LEFT_GUTTER_END (f)
#define FRAME_PANED_RIGHT_EDGE(f) FRAME_RIGHT_GUTTER_START (f)

/* Thickness of non-paned area at edge of frame;
   
   FRAME_PANED_TOP_EDGE (f) == FRAME_NONPANED_SIZE (f, TOP_EDGE)
   FRAME_PANED_LEFT_EDGE (f) == FRAME_NONPANED_SIZE (f, LEFT_EDGE)
   FRAME_PANED_BOTTOM_EDGE (f) ==
     FRAME_PIXHEIGHT (f) - FRAME_NONPANED_SIZE (f, BOTTOM_EDGE)
   FRAME_PANED_RIGHT_EDGE (f) ==
     FRAME_PIXWIDTH (f) - FRAME_NONPANED_SIZE (f, RIGHT_EDGE)
   
*/
#define FRAME_NONPANED_SIZE(f, pos)					\
  (FRAME_REAL_TOOLBAR_BOUNDS (f, pos) + FRAME_INTERNAL_BORDER_SIZE (f, pos) + \
   FRAME_GUTTER_BOUNDS (f, pos))

#ifdef HAVE_TOOLBARS
#ifdef HAVE_GTK
#define FRAME_GTK_TOOLBAR_WIDGET(f) f->gtk_toolbars
#define FRAME_GTK_TOOLBAR_CHECKSUM(f, pos) f->gtk_toolbar_checksum[pos]
#endif
#endif



#endif /* INCLUDED_frame_impl_h_ */
