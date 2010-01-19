/* Definitions of marked slots in windows and window configs
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2001, 2002 Ben Wing.
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

/* Split out of window.h and window.c
   by Kirill Katsnelson <kkm@kis.ru>, May 1998 */

/* Separation into WINDOW_SLOT / WINDOW_SAVED_SLOT by Ben Wing, June 2001.

   NOTE: No semicolons after slot declarations in this file!  The
   definitions of WINDOW_SLOT (and possibly WINDOW_SAVED_SLOT) need
   to include a semicolon.  This is because these may be defined as
   nothing, and some compilers don't tolerate extra semicolons in
   structure definitions.

   WINDOW_SLOT declares a Lisp_Object that is not copied into the
     saved_window struct of a window configuration, or is handled in
     a special way in window configurations.
   WINDOW_SLOT_ARRAY is the same for an array of Lisp_Objects.
   WINDOW_SAVED_SLOT declares a Lisp_Object that is copied with no
     special handling into the saved_window struct of a window
     configuration.  You must also declare the comparison function,
     either EQ or EQUAL_WRAPPED (i.e. Feq() or Fequal()).
   WINDOW_SAVED_SLOT_ARRAY is the same for an array of Lisp_Objects.

   Callers should define WINDOW_SLOT (with a terminating semicolon if not
   blank), and WINDOW_SAVED_SLOT if different; otherwise the latter will be
   defined using WINDOW_SLOT.  Callers should also either (a) do nothing
   else (which defines WINDOW_SLOT_ARRAY using a for() loop, appropriate
   for normal code), define WINDOW_SLOT_DECLARATION (which defines
   WINDOW_SLOT_ARRAY using WINDOW_SLOT (slot[size]), appropriate for a
   struct definition), or define WINDOW_SLOT_ARRAY themselves.  In the
   first two cases, WINDOW_SAVED_SLOT_ARRAY will be defined in the same
   fashion, using WINDOW_SAVED_SLOT.  In the last case, if
   WINDOW_SAVED_SLOT is defined, the caller must provide an appropriate
   definition of WINDOW_SAVED_SLOT_ARRAY; otherwise, it will be defined
   using WINDOW_SLOT_ARRAY.

   Callers do not need to undefine these definitions; it is done
   automatically.
*/

#ifdef WINDOW_SLOT_ARRAY
# ifndef WINDOW_SAVED_SLOT_ARRAY
#  ifdef WINDOW_SAVED_SLOT
#   error must define WINDOW_SAVED_SLOT_ARRAY if WINDOW_SAVED_SLOT and WINDOW_SLOT_ARRAY are defined
#  else
#   define WINDOW_SAVED_SLOT_ARRAY(slot, size, compare) \
     WINDOW_SLOT_ARRAY (slot, size)
#  endif /* WINDOW_SAVED_SLOT */
# endif /* not WINDOW_SAVED_SLOT_ARRAY */
#elif defined (WINDOW_SLOT_DECLARATION) /* not WINDOW_SLOT_ARRAY */
# define WINDOW_SLOT_ARRAY(slot, size) WINDOW_SLOT (slot[size])
# define WINDOW_SAVED_SLOT_ARRAY(slot, size, compare) \
  WINDOW_SAVED_SLOT (slot[size], compare)
#else /* not WINDOW_SLOT_DECLARATION, not WINDOW_SLOT_ARRAY */
# define WINDOW_SLOT_ARRAY(slot, size) do {	\
  int wsaidx;					\
  for (wsaidx = 0; wsaidx < size; wsaidx++)	\
    {						\
      WINDOW_SLOT (slot[wsaidx]);		\
    }						\
} while (0);
# define WINDOW_SAVED_SLOT_ARRAY(slot, size, compare) do {	\
  int wsaidx;							\
  for (wsaidx = 0; wsaidx < size; wsaidx++)			\
    {								\
      WINDOW_SAVED_SLOT (slot[wsaidx], compare);		\
    }								\
} while (0);
#endif /* WINDOW_SLOT_DECLARATION */

#ifndef WINDOW_SAVED_SLOT
#define WINDOW_SAVED_SLOT(slot, compare) WINDOW_SLOT (slot)
#endif

#define EQUAL_WRAPPED(x,y) internal_equal ((x), (y), 0)


  /* The frame this window is on.  */
  WINDOW_SLOT (frame)
  /* t if this window is a minibuffer window.  */
  WINDOW_SLOT (mini_p)
  /* Following child (to right or down) at same level of tree */
  WINDOW_SLOT (next)
  /* Preceding child (to left or up) at same level of tree */
  WINDOW_SLOT (prev)
  /* First child of this window. */
  /* vchild is used if this is a vertical combination,
     hchild if this is a horizontal combination. */
  WINDOW_SLOT (hchild)
  WINDOW_SLOT (vchild)
  /* The window this one is a child of. */
  WINDOW_SLOT (parent)

  /* The buffer displayed in this window */
  /* Of the fields vchild, hchild and buffer, only one is non-nil.  */
  WINDOW_SLOT (buffer)
  /* A marker pointing to where in the text to start displaying */
  /* need one for each set of display structures */
  WINDOW_SLOT_ARRAY (start, 3)
  /* A marker pointing to where in the text point is in this window,
     used only when the window is not selected.
     This exists so that when multiple windows show one buffer
     each one can have its own value of point.  */
  /* need one for each set of display structures */
  WINDOW_SLOT_ARRAY (pointm, 3)
  /* A marker pointing to where in the text the scrollbar is pointing;
     #### moved to scrollbar.c? */
  WINDOW_SLOT (sb_point)
  /* A table that remembers (in marker form) the value of point in buffers
     previously displayed in this window.  Switching back to those buffers
     causes the remembered point value to become current, rather than the
     buffer's point.  This is so that you get sensible behavior if you have
     a buffer displayed in multiple windows and temporarily switch away and
     then back in one window.  We don't save or restore this table in a
     window configuration, since that would be counterproductive -- we
     always want to remember the most recent value of point in buffers we
     switched away from. */
  WINDOW_SLOT (saved_point_cache)
  /* A table that remembers (in marker form) the value of window start in
     buffers previously displayed in this window.  Save reason as for
     the previous table. */
  WINDOW_SLOT (saved_last_window_start_cache)

  /* Number saying how recently window was selected */
  WINDOW_SLOT (use_time)
  /* text.modified of displayed buffer as of last time display completed */
  WINDOW_SLOT_ARRAY (last_modified, 3)
  /* Value of point at that time */
  WINDOW_SLOT_ARRAY (last_point, 3)
  /* Value of start at that time */
  WINDOW_SLOT_ARRAY (last_start, 3)
  /* buf.face_change as of last time display completed */
  WINDOW_SLOT_ARRAY (last_facechange, 3)

  /* we cannot have a per-device cache of widgets / subwindows because
     each visible instance needs to be a separate instance. The lowest
     level of granularity we can get easily is the window that the
     subwindow is in. This will fail if we attach the same subwindow
     twice to a buffer. However, we are quite unlikely to do this,
     especially with buttons which will need individual callbacks. The
     proper solution is probably not worth the effort. */
  WINDOW_SLOT (subwindow_instance_cache)

  WINDOW_SLOT (line_cache_last_updated)

  /*** Non-specifier vars of window and window config ***/

  /* Non-nil means window is marked as dedicated.  */
  WINDOW_SAVED_SLOT (dedicated, EQ)

  /*** specifier values cached in the struct window ***/

  /* Display-table to use for displaying chars in this window. */
  WINDOW_SAVED_SLOT (display_table, EQUAL_WRAPPED)
  /* Thickness of modeline shadow, in pixels.  If negative, draw
     as recessed. */
  WINDOW_SAVED_SLOT (modeline_shadow_thickness, EQ)
  /* Non-nil means to display a modeline for the buffer. */
  WINDOW_SAVED_SLOT (has_modeline_p, EQ)
  /* Thickness of vertical divider shadow, in pixels.  If negative, draw as
     recessed. */
  WINDOW_SAVED_SLOT (vertical_divider_shadow_thickness, EQ)
  /* Divider surface width (not counting 3-d borders) */
  WINDOW_SAVED_SLOT (vertical_divider_line_width, EQ)
  /* Spacing between outer edge of divider border and window edge */
  WINDOW_SAVED_SLOT (vertical_divider_spacing, EQ)
  /* Whether vertical dividers are always displayed */
  WINDOW_SAVED_SLOT (vertical_divider_always_visible_p, EQ)

#ifdef HAVE_SCROLLBARS
  /* Width of vertical scrollbars. */
  WINDOW_SAVED_SLOT (scrollbar_width, EQ)
  /* Height of horizontal scrollbars. */
  WINDOW_SAVED_SLOT (scrollbar_height, EQ)
  /* Whether the scrollbars are visible */
  WINDOW_SAVED_SLOT (horizontal_scrollbar_visible_p, EQ)
  WINDOW_SAVED_SLOT (vertical_scrollbar_visible_p, EQ)
  /* Scrollbar positions */
  WINDOW_SAVED_SLOT (scrollbar_on_left_p, EQ)
  WINDOW_SAVED_SLOT (scrollbar_on_top_p, EQ)
  /* Pointer to use for vertical and horizontal scrollbars. */
  WINDOW_SAVED_SLOT (scrollbar_pointer, EQ)
#endif /* HAVE_SCROLLBARS */
#ifdef HAVE_TOOLBARS
  /* Toolbar specification for each of the four positions.
     This is not a size hog because the value here is not copied,
     and will be shared with the specs in the specifier. */
  WINDOW_SAVED_SLOT_ARRAY (toolbar, 4, EQUAL_WRAPPED)
  /* Toolbar size for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (toolbar_size, 4, EQUAL_WRAPPED)
  /* Toolbar border width for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (toolbar_border_width, 4, EQUAL_WRAPPED)
  /* Toolbar visibility status for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (toolbar_visible_p, 4, EQUAL_WRAPPED)
  /* Caption status of toolbar. */
  WINDOW_SAVED_SLOT (toolbar_buttons_captioned_p, EQ)
  /* The following five don't really need to be cached except
     that we need to know when they've changed. */
  WINDOW_SAVED_SLOT (default_toolbar, EQUAL_WRAPPED)
  WINDOW_SAVED_SLOT (default_toolbar_width, EQ)
  WINDOW_SAVED_SLOT (default_toolbar_height, EQ)
  WINDOW_SAVED_SLOT (default_toolbar_visible_p, EQ)
  WINDOW_SAVED_SLOT (default_toolbar_border_width, EQ)
  WINDOW_SAVED_SLOT (toolbar_shadow_thickness, EQ)
#endif /* HAVE_TOOLBARS */

  /* Gutter specification for each of the four positions.
     This is not a size hog because the value here is not copied,
     and will be shared with the specs in the specifier. */
  WINDOW_SAVED_SLOT_ARRAY (gutter, 4, EQUAL_WRAPPED)
  /* Real (pre-calculated) gutter specification for each of the four positions.
     This is not a specifier, it is calculated by the specifier change
     functions. */
  WINDOW_SAVED_SLOT_ARRAY (real_gutter, 4, EQUAL_WRAPPED)
  /* Gutter size for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (gutter_size, 4, EQUAL_WRAPPED)
  /* Real (pre-calculated) gutter size for each of the four positions.
     This is not a specifier, it is calculated by the specifier change
     functions. */
  WINDOW_SAVED_SLOT_ARRAY (real_gutter_size, 4, EQUAL_WRAPPED)
  /* Gutter border width for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (gutter_border_width, 4, EQUAL_WRAPPED)
  /* Gutter visibility status for each of the four positions. */
  WINDOW_SAVED_SLOT_ARRAY (gutter_visible_p, 4, EQUAL_WRAPPED)
  /* The following five don't really need to be cached except
     that we need to know when they've changed. */
  WINDOW_SAVED_SLOT (default_gutter, EQUAL_WRAPPED)
  WINDOW_SAVED_SLOT (default_gutter_width, EQ)
  WINDOW_SAVED_SLOT (default_gutter_height, EQ)
  WINDOW_SAVED_SLOT (default_gutter_visible_p, EQ)
  WINDOW_SAVED_SLOT (default_gutter_border_width, EQ)
/* margins */
  WINDOW_SAVED_SLOT (left_margin_width, EQ)
  WINDOW_SAVED_SLOT (right_margin_width, EQ)
  WINDOW_SAVED_SLOT (minimum_line_ascent, EQ)
  WINDOW_SAVED_SLOT (minimum_line_descent, EQ)
  WINDOW_SAVED_SLOT (use_left_overflow, EQ)
  WINDOW_SAVED_SLOT (use_right_overflow, EQ)
#ifdef HAVE_MENUBARS
  /* Visibility of menubar. */
  WINDOW_SAVED_SLOT (menubar_visible_p, EQ)
#endif /* HAVE_MENUBARS */
  WINDOW_SAVED_SLOT (text_cursor_visible_p, EQ)

  /* Hara-kiri */
#undef EQUAL_WRAPPED
#undef WINDOW_SLOT_DECLARATION
#undef WINDOW_SLOT
#undef WINDOW_SLOT_ARRAY
#undef WINDOW_SAVED_SLOT
#undef WINDOW_SAVED_SLOT_ARRAY
