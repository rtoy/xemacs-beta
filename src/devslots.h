/* Definitions of marked slots in consoles
   Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

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

/* Synched up with: Not in FSF. */

/* We define the Lisp_Objects in the device structure in a separate file
   because there are numerous places we want to iterate over them, such
   as when defining them in the structure, initializing them, or marking
   them.

   To use, define MARKED_SLOT before including this file.  In the structure
   definition, you also need to define FRAME_SLOT_DECLARATION.  No need to
   undefine either value; that happens automatically.  */

  /* Name of this device, for resourcing and printing purposes.
     If not explicitly given, it's initialized in a device-specific
     manner. */
  MARKED_SLOT (name);

  /* What this device is connected to */
  MARKED_SLOT (connection);

  /* A canonical name for the connection that is used to determine
     whether `make-device' is being called on an existing device. */
  MARKED_SLOT (canon_connection);

  /* List of frames on this device. */
  MARKED_SLOT (frame_list);

  /* The console this device is on. */
  MARKED_SLOT (console);

  /* Frame which is "currently selected".  This is what `selected-frame'
     returns and is the default frame for many operations.  This may
     not be the same as frame_with_focus; `select-frame' changes the
     selected_frame but not the frame_with_focus.  However, eventually
     either the two values will be the same, or frame_with_focus will
     be nil: right before waiting for an event, the focus is changed
     to point to the selected_frame if XEmacs currently has the focus
     on this device.  Note that frame_with_focus may be nil (none of the
     frames on this device have the window-system focus), but
     selected_frame will never be nil if there are any frames on
     the device. */
  MARKED_SLOT (selected_frame);
  /* Frame that currently contains the window-manager focus, or none.
     Note that we've split frame_with_focus into two variables.
     frame_with_focus_real is the value we use most of the time,
     but frame_with_focus_for_hooks is used for running the select-frame-hook
     and deselect-frame-hook.  We do this because we split the focus handling
     into two parts: one part (deals with drawing the solid/box cursor)
     runs as soon as a focus event is received; the other (running the
     hooks) runs after any pending sit-for/sleep-for/accept-process-output
     calls are done. */
  MARKED_SLOT (frame_with_focus_real);
  MARKED_SLOT (frame_with_focus_for_hooks);
  /* If we have recently issued a request to change the focus as a
     result of select-frame having been called, the following variable
     records the frame we are trying to focus on.  The reason for this
     is that the window manager may not grant our request to change
     the focus (so we can't just change frame_with_focus), and we don't
     want to keep sending requests again and again to the window manager.
     This variable is reset whenever a focus-change event is seen. */
  MARKED_SLOT (frame_that_ought_to_have_focus);

  /* Color class of this device. */
  MARKED_SLOT (device_class);

  /* Alist of values for user-defined tags in this device. */
  MARKED_SLOT (user_defined_tags);

  /* Hash tables for device-specific objects (fonts, colors, etc).
     These are key-weak hash tables (or hash tables containing key-weak
     hash tables) so that they disappear when the key goes away. */

  /* This is a simple key-weak hash table hashing color names to
     instances. */
  MARKED_SLOT (color_instance_cache);

  /* This is a simple key-weak hash table hashing font names to
     instances. */
  MARKED_SLOT (font_instance_cache);

#ifdef MULE
  /* This is a bi-level cache, where the hash table in this slot here
     indexes charset objects to key-weak hash tables, which in turn
     index font names to more specific font names that match the
     given charset's registry.  This speeds up the horrendously
     slow XListFonts() operation that needs to be done in order
     to determine an appropriate font. */
  MARKED_SLOT (charset_font_cache);
#endif

  /* This is a bi-level cache, where the hash table in this slot here
     indexes image-instance-type masks (there are currently 6
     image-instance types and thus 64 possible masks) to key-weak hash
     tables like the one for colors. */
  MARKED_SLOT (image_instance_cache);

#undef MARKED_SLOT
