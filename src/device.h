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

#ifndef INCLUDED_device_h_
#define INCLUDED_device_h_

#include "console.h"

struct device;

DECLARE_LRECORD (device, struct device);
#define XDEVICE(x) XRECORD (x, device, struct device)
#define wrap_device(p) wrap_record (p, device)
#define DEVICEP(x) RECORDP (x, device)
#define CHECK_DEVICE(x) CHECK_RECORD (x, device)
#define CONCHECK_DEVICE(x) CONCHECK_RECORD (x, device)

/* Basic properties available to non-privileged users; redefined in
   device-impl.h */

int device_live_p (struct device *d);
Lisp_Object device_console (struct device *d);
Lisp_Object device_frame_list (struct device *d);

#define DEVICE_LIVE_P(d) device_live_p (d)
#define DEVICE_CONSOLE(d) device_console (d)
#define DEVICE_FRAME_LIST(d) device_frame_list (d)

#define DEVICE_XCONSOLE(d) XCONSOLE (DEVICE_CONSOLE (d))

#define XDEVICE_CONSOLE(d) DEVICE_CONSOLE (XDEVICE (d))
#define XDEVICE_XCONSOLE(d) XCONSOLE (XDEVICE_XCONSOLE (d))

#define CHECK_LIVE_DEVICE(x) do {			\
  CHECK_DEVICE (x);					\
  if (! DEVICE_LIVE_P (XDEVICE (x)))			\
    dead_wrong_type_argument (Qdevice_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_DEVICE(x) do {			\
  CONCHECK_DEVICE (x);					\
  if (! DEVICE_LIVE_P (XDEVICE (x)))			\
    x = wrong_type_argument (Qdevice_live_p, (x));	\
} while (0)

/* #### unify this with DOMAIN_DEVICE once the latter has image instances
   expunged from it. */
/* This turns out to be used heavily so we make it a macro to make it
   inline.  Also, the majority of the time the object will turn out to
   be a window so we move it from being checked last to being checked
   first. */
#define DFW_DEVICE(obj)					\
   (WINDOWP (obj) ? WINDOW_DEVICE (XWINDOW (obj))	\
 : (FRAMEP  (obj) ? FRAME_DEVICE (XFRAME (obj))		\
 : (DEVICEP (obj) ? obj					\
 : Qnil)))

/* NO_BREAK means that "break" doesn't do what you think it does!
   Use goto instead.  "continue" is OK, though. */
#define DEVICE_LOOP_NO_BREAK(devcons, concons)			\
  CONSOLE_LOOP (concons)					\
    CONSOLE_DEVICE_LOOP (devcons, XCONSOLE (XCAR (concons)))
#define DEVICE_FRAME_LOOP(frmcons, d) \
  LIST_LOOP (frmcons, DEVICE_FRAME_LIST (d))
#define CONSOLE_FRAME_LOOP_NO_BREAK(frmcons, devcons, con) \
  CONSOLE_DEVICE_LOOP (devcons, con) 			   \
    DEVICE_FRAME_LOOP (frmcons, XDEVICE (XCAR (devcons)))

EXFUN (Fdevice_console, 1);
EXFUN (Fdevice_name, 1);
EXFUN (Ffind_device, 2);
EXFUN (Fmake_device, 3);
EXFUN (Fselected_device, 1);

extern Lisp_Object Qcreate_device_hook, Qdelete_device_hook, Qgrayscale;
extern Lisp_Object Qinit_post_tty_win, Qmono;
extern Lisp_Object Vdevice_class_list;

int valid_device_class_p (Lisp_Object class);

void select_device_1 (Lisp_Object);
struct device *decode_device (Lisp_Object);
void handle_asynch_device_change (void);
Lisp_Object call_critical_lisp_code (struct device *d, Lisp_Object function,
				     Lisp_Object object);
void delete_device_internal (struct device *d, int force,
			     int called_from_delete_console,
			     int from_io_error);
void io_error_delete_device (Lisp_Object device);
Lisp_Object find_nonminibuffer_frame_not_on_device (Lisp_Object device);
void set_device_selected_frame (struct device *d, Lisp_Object frame);
Lisp_Object domain_device_type (Lisp_Object domain);
int window_system_pixelated_geometry (Lisp_Object domain);

Lisp_Object get_default_device (Lisp_Object type);
void set_default_device (Lisp_Object type, Lisp_Object device);
void clear_default_devices (void);

#endif /* INCLUDED_device_h_ */
