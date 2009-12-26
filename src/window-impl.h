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

#ifndef INCLUDED_window_impl_h_
#define INCLUDED_window_impl_h_

#include "window.h"

/* All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the frame.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged on the frame
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
of each child points to the child above it, or is nil for the
highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the frame area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, on screens which have an ordinary window and a
minibuffer, prev of the minibuf window is the root window and next of
the root window is the minibuf window.  On minibufferless screens or
minibuffer-only screens, the root window and the minibuffer window are
one and the same, so its prev and next members are nil.

A dead window has the `dead' flag set on it.  Note that unlike other
dead objects, dead windows can be made live again through restoring a
window configuration.  This means that the values in a dead window
need to be preserved, except for those that are reconstructed by from
the window configuration. */

struct window
{
  struct LCRECORD_HEADER header;

  /* The upper left corner coordinates of this window,
     as integers (pixels) relative to upper left corner of frame = 0, 0 */
  int pixel_left;
  int pixel_top;
  /* The size of the window (in pixels) */
  int pixel_height;
  int pixel_width;

  /* Number of columns display within the window is scrolled to the left. */
  int hscroll;
  /* Idem for the window's modeline */
  Charcount modeline_hscroll;
  /* Amount to clip off the top line for pixel-based scrolling. Point
     will remain constant but this will be incremented to
     incrementally shift lines up. */
  int top_yoffset;
  /* Amount to clip off the left of the lines for pixel-based
     scrolling. Hscroll will remain constant but this will be
     incremented to incrementally shift lines left.*/
  int left_xoffset;

  /* face cache elements correct for this window and its current buffer */
  face_cachel_dynarr *face_cachels;
  /* glyph cache elements correct for this window and its current buffer */
  glyph_cachel_dynarr *glyph_cachels;
  /* List of starting positions for display lines.  Only valid if
     buffer has not changed. */
  line_start_cache_dynarr *line_start_cache;
  int line_cache_validation_override;

  /* Length of longest line currently displayed.  Used to control the
     width of the horizontal scrollbars. */
  int max_line_len;

  /* Frame coords of point at that time */
  int last_point_x[3];
  int last_point_y[3];

  /* Number of characters in buffer past bottom of window,
     as of last redisplay that finished. */
  /* need one for each set of display structures */
  int window_end_pos[3];

  /* Set by the extent code when extents in the gutter are changed. */
  int gutter_extent_modiff[4];

  /* Set by redisplay to the last position seen.  This is used
     to implement the redisplay-end-trigger-functions. */
  Charbpos last_redisplay_pos;

#define WINDOW_SLOT_DECLARATION
#define WINDOW_SLOT(slot) Lisp_Object slot;
#include "winslots.h"

  /* one-bit flags: */

  /* marker used when restoring a window configuration */
  unsigned int config_mark :1;
  /* Non-zero means window was dead. */
  unsigned int dead :1;
  /* Non-zero means next redisplay must use the value of start
     set up for it in advance.  Set by scrolling commands.  */
  unsigned int force_start :1;
  /* Non-zero means must regenerate modeline of this window */
  unsigned int redo_modeline :1;
  /* Non-zero means current value of `start'
     was the beginning of a line when it was chosen.  */
  unsigned int start_at_line_beg :1;
  /* new redisplay flag */
  unsigned int windows_changed :1;
  unsigned int shadow_thickness_changed :1;
  /* Vertical divider flag and validity of it */
  unsigned int need_vertical_divider_p :1;
  unsigned int need_vertical_divider_valid_p :1;
};

#define CURRENT_DISP	0
#define DESIRED_DISP	1
#define CMOTION_DISP	2

struct window_mirror
{
  struct LCRECORD_HEADER header;

  /* Frame this mirror is on. */
  struct frame *frame;

  /* Following child (to right or down) at same level of tree */
  struct window_mirror *next;

  /* There is no prev field because we never traverse this structure
     backwards.  Same goes for the parent field. */

  /* First child of this window. */
  /* vchild is used if this is a vertical combination,
     hchild if this is a horizontal combination. */
  struct window_mirror *hchild, *vchild;

  /* Dynamic array of display lines */
  display_line_dynarr *current_display_lines;
  display_line_dynarr *desired_display_lines;

  /* Buffer current_display_lines represent. */
  struct buffer *buffer;

#ifdef HAVE_SCROLLBARS
  /* Scrollbars associated with window, if any. */
  struct scrollbar_instance *scrollbar_vertical_instance;
  struct scrollbar_instance *scrollbar_horizontal_instance;
#endif /* HAVE_SCROLLBARS */

  /* Flag indicating whether a subwindow is currently being displayed. */
  unsigned int subwindows_being_displayed :1;

  /* Keep track of the truncation status in this window so we can
     detect when it has changed.  #### Magic variables would be a huge
     win here. */
  unsigned int truncate_win :1;
};

/* Redefine basic properties more efficiently */

#undef WINDOW_LIVE_P
#define WINDOW_LIVE_P(x) (!(x)->dead)
#undef WINDOW_FRAME
#define WINDOW_FRAME(w) ((w)->frame)
#undef WINDOW_BUFFER
#define WINDOW_BUFFER(w) ((w)->buffer)

/* 1 if W is a minibuffer window.  */
#define MINI_WINDOW_P(W)  (!NILP ((W)->mini_p))

/* 1 if we are dealing with a parentless window (this includes the
   root window on a frame and the minibuffer window; both of these
   are siblings). */
#define TOP_LEVEL_WINDOW_P(w) NILP ((w)->parent)

/* Set all redisplay flags indicating a window has changed */
#define MARK_WINDOWS_CHANGED(w) do {			\
  (w)->windows_changed = 1;				\
  if (!NILP (w->frame))					\
    {							\
      struct frame *mwc_frame = XFRAME (w->frame);	\
      MARK_FRAME_WINDOWS_CHANGED (mwc_frame);		\
    }							\
  else							\
    windows_changed = 1;				\
} while (0)

/* #### This should be fixed not to call MARK_FRAME_CHANGED because
   faces are cached per window.  Also, other code which changes window's
   face should use this macro.
*/
#define MARK_WINDOW_FACES_CHANGED(w)	\
  MARK_FRAME_FACES_CHANGED (XFRAME ((w)->frame))

#define WINDOW_TTY_P(w) FRAME_TTY_P (XFRAME ((w)->frame))
#define WINDOW_X_P(w)   FRAME_X_P   (XFRAME ((w)->frame))
#define WINDOW_NS_P(w)  FRAME_NS_P  (XFRAME ((w)->frame))
#define WINDOW_WIN_P(w) FRAME_WIN_P (XFRAME ((w)->frame))

/* XEmacs window size and positioning macros. */
#define WINDOW_TOP(w) ((w)->pixel_top)
#define WINDOW_TEXT_TOP(w) (WINDOW_TOP (w) + window_top_gutter_height (w))
#define WINDOW_TEXT_TOP_CLIP(w) ((w)->top_yoffset)
#define WINDOW_BOTTOM(w) ((w)->pixel_top + (w)->pixel_height)
#define WINDOW_TEXT_BOTTOM(w) (WINDOW_BOTTOM (w) - window_bottom_gutter_height (w))
#define WINDOW_LEFT(w) ((w)->pixel_left)
#define WINDOW_TEXT_LEFT(w) (WINDOW_LEFT (w) + window_left_gutter_width (w, 0))
#define WINDOW_MODELINE_LEFT(w)	\
  (WINDOW_LEFT (w) + window_left_gutter_width (w, 1))
#define WINDOW_RIGHT(w) ((w)->pixel_left + (w)->pixel_width)
#define WINDOW_TEXT_RIGHT(w)	\
  (WINDOW_RIGHT (w) - window_right_gutter_width (w, 0))
#define WINDOW_MODELINE_RIGHT(w)	\
  (WINDOW_RIGHT (w) - window_right_gutter_width (w, 1))

#define WINDOW_HEIGHT(w) ((w)->pixel_height)
#define WINDOW_TEXT_HEIGHT(w) (WINDOW_TEXT_BOTTOM (w) - WINDOW_TEXT_TOP (w))
#define WINDOW_WIDTH(w) ((w)->pixel_width)
#define WINDOW_TEXT_WIDTH(w) (WINDOW_TEXT_RIGHT (w) - WINDOW_TEXT_LEFT (w))

#define WINDOW_HAS_MODELINE_P(w) (!NILP (w->has_modeline_p))

#define MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED(win)		\
 abs ((!WINDOW_HAS_MODELINE_P (win)				\
       ? ((XINT (win->modeline_shadow_thickness) > 1)		\
	  ? XINT (win->modeline_shadow_thickness) - 1		\
	  : ((XINT (win->modeline_shadow_thickness) < -1)	\
	     ? XINT (win->modeline_shadow_thickness) + 1	\
	     : XINT (win->modeline_shadow_thickness)))		\
       : XINT (win->modeline_shadow_thickness)))

#define MODELINE_SHADOW_THICKNESS(win)				\
 (MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED (win) > 10 		\
  ? 10								\
  : MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED (win))

#endif /* INCLUDED_window_impl_h_ */
