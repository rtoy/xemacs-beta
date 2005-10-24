/* Define device-object for XEmacs.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 2002 Ben Wing
   Copyright (C) 1995 Sun Microsystems

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

/* Written by Chuck Thompson and Ben Wing. */

#ifndef INCLUDED_device_impl_h_
#define INCLUDED_device_impl_h_

#include "console-impl.h"
#include "device.h"

/* This should really be in redisplay.h but by putting it here we
   won't have to ensure that redisplay.h is always included before
   this file. */
struct pixel_to_glyph_translation_cache
{
  unsigned int valid :1;
  struct frame *frame;
  int low_x_coord, high_x_coord, col, obj_x;
  int low_y_coord, high_y_coord, row, obj_y;
  struct window *w;
  Charbpos charpos;
  Charbpos closest;
  Charcount modeline_closest;
  Lisp_Object obj1, obj2;
  int retval;
};

/* Less public: */
#define DEVICE_TYPE_NAME(d) ((d)->devmeths->name)
#define DEVICE_IMPL_FLAG(d, f) CONMETH_IMPL_FLAG ((d)->devmeths, (f))
#define DEVICE_SPECIFIC_FRAME_PROPS(d) \
  ((d)->devmeths->device_specific_frame_props)

/* More public: */
#define DEVICE_TYPE(d) ((d)->devmeths->symbol)
#define XDEVICE_TYPE(d) DEVICE_TYPE (XDEVICE (d))

/******** Accessing / calling a device method *********/

#define HAS_DEVMETH_P(d, m) HAS_CONTYPE_METH_P ((d)->devmeths, m)
#define DEVMETH(d, m, args) CONTYPE_METH ((d)->devmeths, m, args)
#define MAYBE_DEVMETH(d, m, args) MAYBE_CONTYPE_METH ((d)->devmeths, m, args)
#define DEVMETH_OR_GIVEN(d, m, args, given) \
  CONTYPE_METH_OR_GIVEN((d)->devmeths, m, args, given)
#define MAYBE_INT_DEVMETH(d, m, args) \
  MAYBE_INT_CONTYPE_METH ((d)->devmeths, m, args)
#define MAYBE_LISP_DEVMETH(d, m, args) \
  MAYBE_LISP_CONTYPE_METH ((d)->devmeths, m, args)

struct device
{
  struct LCRECORD_HEADER header;

  /* Methods for this device's console.  This can also be retrieved
     through device->console, but it's faster this way. */
  struct console_methods *devmeths;

  /* Duplicates devmeths->symbol.  See comment in struct console. */
  enum console_variant devtype;

  /* A structure of auxiliary data specific to the device type.
     struct x_device is used for X window frames; defined in console-x.h
     struct tty_device is used to TTY's; defined in console-tty.h */
  void *device_data;

  /* redisplay flags */
  unsigned int buffers_changed :1;
  unsigned int clip_changed :1;
  unsigned int extents_changed :1;
  unsigned int faces_changed :1;
  unsigned int frame_changed :1;
  unsigned int frame_layout_changed :1;	/* The layout of frame
					   elements has changed. */
  unsigned int glyphs_changed :1;
  unsigned int subwindows_changed :1;
  unsigned int subwindows_state_changed :1;
  unsigned int icon_changed :1;
  unsigned int menubar_changed :1;
  unsigned int modeline_changed :1;
  unsigned int point_changed :1;
  unsigned int size_changed :1;
  unsigned int gutter_changed :1;
  unsigned int toolbar_changed :1;
  unsigned int windows_changed :1;
  unsigned int windows_structure_changed :1;

  unsigned int locked :1;

  /* Cache information about last pixel position translated to a
     glyph.  The law of locality applies very heavily here so caching
     the value leads to a significant win.  At the moment this is
     really X specific but once we have generic mouse support it won't
     be. */
  struct pixel_to_glyph_translation_cache pixel_to_glyph_cache;

  /* Output baud rate of device; used for redisplay decisions.  */
  int baud_rate;

  /* sound flags */
  unsigned int on_console_p :1;
  unsigned int connected_to_nas_p :1;

#define MARKED_SLOT(x) Lisp_Object x;
#include "devslots.h"

  /* File descriptors for input and output.  Much of the time
     (but not always) these will be the same.  For an X device,
     these both hold the file descriptor of the socket used
     to communicate with the X server.  For a TTY device, these
     may or may not be the same and point to the terminal that
     is used for I/O. */
  int infd, outfd;

  /* infd and outfd are moved outside HAVE_UNIXOID_EVENT_LOOP conditionals,
     because Win32, presumably the first port which does not use select()
     polling, DOES have handles for a console device. -- kkm */

#ifdef HAVE_UNIXOID_EVENT_LOOP
  /* holds some data necessary for SIGIO control.  Perhaps this should
     be inside of device_data; but it is used for both TTY's and X
     device.  Perhaps it should be conditionalized on SIGIO; but
     this requires including syssignal.h. */
  int old_fcntl_owner;
#endif
};

/* Redefine basic properties more efficiently */

#undef DEVICE_LIVE_P
#define DEVICE_LIVE_P(d) (!EQ (DEVICE_TYPE (d), Qdead))
#undef DEVICE_CONSOLE
#define DEVICE_CONSOLE(d) ((d)->console)
#undef DEVICE_FRAME_LIST
#define DEVICE_FRAME_LIST(d) ((d)->frame_list)

#define DEVICE_TYPE_P(d, type)	EQ (DEVICE_TYPE (d), Q##type)

#ifdef ERROR_CHECK_TYPES
DECLARE_INLINE_HEADER (
struct device *
error_check_device_type (struct device *d, Lisp_Object sym)
)
{
  assert (EQ (DEVICE_TYPE (d), sym));
  return d;
}
# define DEVICE_TYPE_DATA(d, type)			\
  ((struct type##_device *) error_check_device_type (d, Q##type)->device_data)
#else
# define DEVICE_TYPE_DATA(d, type)			\
  ((struct type##_device *) (d)->device_data)
#endif

#define CHECK_DEVICE_TYPE(x, type)			\
  do {							\
    CHECK_DEVICE (x);					\
    if (!(DEVICEP (x) && DEVICE_TYPE_P (XDEVICE (x),	\
					 type)))	\
      dead_wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)
#define CONCHECK_DEVICE_TYPE(x, type)			\
  do {							\
    CONCHECK_DEVICE (x);				\
    if (!(DEVICEP (x) && DEVICE_TYPE_P (XDEVICE (x),	\
					 type)))	\
      x = wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)

#define DEVICE_DISPLAY_P(dev)				\
  (DEVICE_LIVE_P (dev) &&				\
    !DEVICE_IMPL_FLAG (dev, XDEVIMPF_IS_A_PRINTER))

#define CHECK_DISPLAY_DEVICE(dev)			\
  do {							\
    CHECK_DEVICE (dev);					\
    if (!(DEVICEP (dev)					\
          && DEVICE_DISPLAY_P (XDEVICE (dev))))		\
      dead_wrong_type_argument (Qdisplay, dev);		\
  } while (0)

#define CONCHECK_DISPLAY_DEVICE(dev)			\
  do {							\
    CONCHECK_DEVICE (dev);				\
    if (!(DEVICEP (dev)					\
          && DEVICE_DISPLAY_P (XDEVICE (dev))))		\
      wrong_type_argument (Qdisplay, dev);		\
  } while (0)

#define DEVICE_PRINTER_P(dev)				\
  (DEVICE_LIVE_P (dev) && !DEVICE_DISPLAY_P (dev))

#define CHECK_PRINTER_DEVICE(dev)			\
  do {							\
    CHECK_DEVICE (dev);					\
    if (!(DEVICEP (dev)					\
          && DEVICE_PRINTER_P (XDEVICE (dev))))		\
      dead_wrong_type_argument (Qprinter, dev);		\
  } while (0)

#define CONCHECK_PRINTER_DEVICE(dev)			\
  do {							\
    CONCHECK_DEVICE (dev);				\
    if (!(DEVICEP (dev)					\
          && DEVICE_PRINTER_P (XDEVICE (dev))))		\
      wrong_type_argument (Qprinter, dev);		\
  } while (0)

/* #### These should be in the device-*.h files but there are
   too many places where the abstraction is broken.  Need to
   fix. */

#define DEVICE_X_P(dev) CONSOLE_TYPESYM_X_P (DEVICE_TYPE (dev))
#define CHECK_X_DEVICE(z) CHECK_DEVICE_TYPE (z, x)
#define CONCHECK_X_DEVICE(z) CONCHECK_DEVICE_TYPE (z, x)

#define DEVICE_GTK_P(dev) CONSOLE_TYPESYM_GTK_P (DEVICE_TYPE (dev))
#define CHECK_GTK_DEVICE(z) CHECK_DEVICE_TYPE (z, gtk)
#define CONCHECK_GTK_DEVICE(z) CONCHECK_DEVICE_TYPE (z, gtk)

#define DEVICE_MSWINDOWS_P(dev) CONSOLE_TYPESYM_MSWINDOWS_P (DEVICE_TYPE (dev))
#define CHECK_MSWINDOWS_DEVICE(z) CHECK_DEVICE_TYPE (z, mswindows)
#define CONCHECK_MSWINDOWS_DEVICE(z) CONCHECK_DEVICE_TYPE (z, mswindows)

#define DEVICE_TTY_P(dev) CONSOLE_TYPESYM_TTY_P (DEVICE_TYPE (dev))
#define CHECK_TTY_DEVICE(z) CHECK_DEVICE_TYPE (z, tty)
#define CONCHECK_TTY_DEVICE(z) CONCHECK_DEVICE_TYPE (z, tty)

#define DEVICE_STREAM_P(dev) CONSOLE_TYPESYM_STREAM_P (DEVICE_TYPE (dev))
#define CHECK_STREAM_DEVICE(z) CHECK_DEVICE_TYPE (z, stream)
#define CONCHECK_STREAM_DEVICE(z) CONCHECK_DEVICE_TYPE (z, stream)

#define DEVICE_WIN_P(dev) CONSOLE_TYPESYM_WIN_P (DEVICE_TYPE (dev))

#define DEVICE_REDISPLAY_INFO(d) ((d)->redisplay_info)

#define DEVICE_NAME(d) ((d)->name)
#define DEVICE_CLASS(d) ((d)->device_class)
/* Catch people attempting to set this. */
#define DEVICE_SELECTED_FRAME(d) NON_LVALUE ((d)->selected_frame)
#define DEVICE_FRAME_WITH_FOCUS_REAL(d) ((d)->frame_with_focus_real)
#define DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d) ((d)->frame_with_focus_for_hooks)
#define DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d)			\
  ((d)->frame_that_ought_to_have_focus)
#define DEVICE_USER_DEFINED_TAGS(d) ((d)->user_defined_tags)
#define DEVICE_CONNECTION(d) ((d)->connection)
#define DEVICE_CANON_CONNECTION(d) ((d)->canon_connection)
#define DEVICE_BAUD_RATE(d) ((d)->baud_rate)
#define DEVICE_INFD(d) ((d)->infd)
#define DEVICE_OUTFD(d) ((d)->outfd)
#define DEVICE_OLD_FCNTL_OWNER(d) ((d)->old_fcntl_owner)
#define DEVICE_ON_CONSOLE_P(d) ((d)->on_console_p)
#define DEVICE_CONNECTED_TO_NAS_P(d) ((d)->connected_to_nas_p)

#define LOCK_DEVICE(d) ((void) ((d)->locked = 1))
#define UNLOCK_DEVICE(d) ((void) ((d)->locked = 0))

#define INVALIDATE_DEVICE_PIXEL_TO_GLYPH_CACHE(d)			\
  ((void) ((d)->pixel_to_glyph_cache.valid = 0))

#define INVALIDATE_PIXEL_TO_GLYPH_CACHE do {					\
  Lisp_Object IPTGC_devcons, IPTGC_concons;					\
  DEVICE_LOOP_NO_BREAK (IPTGC_devcons, IPTGC_concons)				\
    INVALIDATE_DEVICE_PIXEL_TO_GLYPH_CACHE (XDEVICE (XCAR (IPTGC_devcons)));	\
} while (0)

#define MARK_DEVICE_FACES_CHANGED(d)			\
  ((void) (faces_changed = (d)->faces_changed = 1))

#define MARK_DEVICE_GLYPHS_CHANGED(d)			\
  ((void) (glyphs_changed = (d)->glyphs_changed = 1))

#define MARK_DEVICE_SUBWINDOWS_CHANGED(d)			\
  ((void) (subwindows_changed = (d)->subwindows_changed = 1))

#define MARK_DEVICE_SUBWINDOWS_STATE_CHANGED(d)		\
  ((void) (subwindows_state_changed = (d)->subwindows_state_changed = 1))

#define MARK_DEVICE_TOOLBARS_CHANGED(d)			\
  ((void) (toolbar_changed = (d)->toolbar_changed = 1))

#define MARK_DEVICE_GUTTERS_CHANGED(d)		\
  ((void) (gutter_changed = (d)->gutter_changed = 1))

#define MARK_DEVICE_SIZE_CHANGED(d)			\
  ((void) (size_changed = (d)->size_changed = 1))

#define MARK_DEVICE_FRAMES_FACES_CHANGED(d) do {	\
  struct device *mdffc_d = (d);				\
  Lisp_Object frmcons;					\
  DEVICE_FRAME_LOOP (frmcons, mdffc_d)			\
    XFRAME (XCAR (frmcons))->faces_changed = 1;		\
  MARK_DEVICE_FACES_CHANGED (mdffc_d);			\
} while (0)

#define MARK_DEVICE_FRAMES_GLYPHS_CHANGED(d) do {	\
  struct device *mdffc_d = (d);				\
  Lisp_Object frmcons;					\
  DEVICE_FRAME_LOOP (frmcons, mdffc_d)			\
    XFRAME (XCAR (frmcons))->glyphs_changed = 1;		\
  MARK_DEVICE_GLYPHS_CHANGED (mdffc_d);		\
} while (0)

#define MARK_DEVICE_FRAME_CHANGED(d)			\
  ((void) (frame_changed = (d)->frame_changed = 1))

#define MARK_DEVICE_FRAME_LAYOUT_CHANGED(d)			\
  ((void) (frame_layout_changed = (d)->frame_layout_changed = 1))

#define MARK_DEVICE_WINDOWS_CHANGED(d)			\
  ((void) (windows_changed = (d)->windows_changed = 1))

#define MARK_DEVICE_WINDOWS_STRUCTURE_CHANGED(d)	\
  ((void) (windows_structure_changed = (d)->windows_structure_changed = 1))

#endif /* INCLUDED_device_impl_h_ */
