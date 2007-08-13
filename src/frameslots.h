/* Definitions of marked slots in frames
   Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

/* Synched up with: FSF 19.30.  Split out of frame.h.  */

  /* device frame belongs to. */
  MARKED_SLOT (device);

  /* Name of this frame: a Lisp string.
     NOT the same as the frame's title, even though FSF bogusly
     confuses the two.  The frame's name is used for resourcing
     and lookup purposes and is something you can count on having
     a specific value, while the frame's title may vary depending
     on the user's choice of `frame-title-format'. */
  MARKED_SLOT (name);

  /* The frame which should receive keystrokes that occur in this
     frame, or nil if they should go to the frame itself.  This is
     usually nil, but if the frame is minibufferless, we can use this
     to redirect keystrokes to a surrogate minibuffer frame when
     needed.

     Note that a value of nil is different than having the field point
     to the frame itself.  Whenever the Fselect_frame function is used
     to shift from one frame to the other, any redirections to the
     original frame are shifted to the newly selected frame; if
     focus_frame is nil, Fselect_frame will leave it alone.  */
  MARKED_SLOT (focus_frame);

  /* This frame's root window.  Every frame has one.
     If the frame has only a minibuffer window, this is it.
     Otherwise, if the frame has a minibuffer window, this is its sibling.  */
  MARKED_SLOT (root_window);

  /* This frame's selected window.
     Each frame has its own window hierarchy
     and one of the windows in it is selected within the frame.
     The selected window of the selected frame is Emacs's selected window.  */
  MARKED_SLOT (selected_window);

  /* This frame's minibuffer window.
     Most frames have their own minibuffer windows,
     but only the selected frame's minibuffer window
     can actually appear to exist.  */
  MARKED_SLOT (minibuffer_window);

  /* The most recently selected nonminibuf window.
     This is used by things like the toolbar code, which doesn't
     want the toolbar to change when moving to the minibuffer.
     This will only be a minibuf window if we are a minibuf-only
     frame. */
  MARKED_SLOT (last_nonminibuf_window);

  /* frame property list */
  MARKED_SLOT (plist);

  /* A copy of the global Vbuffer_list, to maintain a per-frame buffer
     ordering.  The Vbuffer_list variable and the buffer_list slot of each
     frame contain exactly the same data, just in different orders.  */
  MARKED_SLOT (buffer_alist);

  /* Predicate for selecting buffers for other-buffer.  */
  MARKED_SLOT (buffer_predicate);

  /* The current mouse pointer for the frame.  This is set by calling
     `set-frame-pointer'. */
  MARKED_SLOT (pointer);

  /* The current icon for the frame. */
  MARKED_SLOT (icon);

#ifdef HAVE_MENUBARS
  /* Vector representing the menubar currently displayed.  See menubar-x.c. */
  MARKED_SLOT (menubar_data);
#endif

  /* specifier values cached in the struct frame: */

#ifdef HAVE_MENUBARS
  MARKED_SLOT (menubar_visible_p);
#endif

#ifdef HAVE_SCROLLBARS
  /* Width and height of the scrollbars. */
  MARKED_SLOT (scrollbar_width);
  MARKED_SLOT (scrollbar_height);
#endif

#ifdef HAVE_TOOLBARS
  /* The following three don't really need to be cached except
     that we need to know when they've changed. */
  MARKED_SLOT (default_toolbar_width);
  MARKED_SLOT (default_toolbar_height);
  MARKED_SLOT (default_toolbar_visible_p);
#endif

  /* Possible frame-local default for outside margin widths. */
  MARKED_SLOT (left_margin_width);
  MARKED_SLOT (right_margin_width);
