/* Define console object for XEmacs.
   Copyright (C) 1996, 2002 Ben Wing

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

/* Written by Ben Wing. */

#ifndef INCLUDED_console_h_
#define INCLUDED_console_h_

/* Devices and consoles are similar entities.  The idea is that
   a console represents a physical keyboard/mouse/other-input-source
   while a device represents a display where frames appear on.
   In the X world, a console is a "Display" while a device is a
   "Screen".  Implementationally, it can sometimes get confusing:
   under X, multiple devices on a single console are different
   "Display" connections to what is in reality the same Display on
   the same server.  Because of this, input comes from the device
   and not from the console.  This is OK because events are basically
   always tagged to a particular X window (i.e. frame),
   which exists on only one screen; therefore the event won't be
   reported multiple times even if there are multiple devices on
   the same physical display.  This is an implementation detail
   specific to X consoles (e.g. under NeXTstep or Windows, this
   could be different, and input would come directly from the console).
*/


/* GCC does not like forward enum declaration. This needs to be
   defined here. What a disgust! */

enum console_variant
{
  dead_console,
  tty_console,
  gtk_console,
  x_console,
  mswindows_console,
  msprinter_console,
  stream_console
};

enum device_metrics
{
  DM_color_default, DM_color_select, DM_color_balloon, DM_color_3d_face,
  DM_color_3d_light, DM_color_3d_dark, DM_color_menu, DM_color_menu_highlight,
  DM_color_menu_button, DM_color_menu_disabled, DM_color_toolbar,
  DM_color_scrollbar, DM_color_desktop, DM_color_workspace, DM_font_default,
  DM_font_menubar, DM_font_dialog, DM_size_cursor, DM_size_scrollbar,
  DM_size_menu, DM_size_toolbar, DM_size_toolbar_button,
  DM_size_toolbar_border, DM_size_icon, DM_size_icon_small, DM_size_device,
  DM_size_workspace, DM_offset_workspace, DM_size_device_mm, DM_device_dpi,
  DM_num_bit_planes, DM_num_color_cells, DM_num_screens, DM_mouse_buttons,
  DM_swap_buttons, DM_show_sounds, DM_slow_device, DM_security,
  DM_backing_store, DM_save_under
};

struct console;

DECLARE_LRECORD (console, struct console);
#define XCONSOLE(x) XRECORD (x, console, struct console)
#define wrap_console(p) wrap_record (p, console)
#define CONSOLEP(x) RECORDP (x, console)
#define CHECK_CONSOLE(x) CHECK_RECORD (x, console)
#define CONCHECK_CONSOLE(x) CONCHECK_RECORD (x, console)

/* Basic properties available to non-privileged users; redefined in
   console-impl.h */

struct console_methods;

int console_live_p (struct console *c);
Lisp_Object console_device_list (struct console *c);

#define CONSOLE_LIVE_P(c) console_live_p (c)
#define CONSOLE_DEVICE_LIST(c) console_device_list (c)

#define CHECK_LIVE_CONSOLE(x) do {			\
  CHECK_CONSOLE (x);					\
  if (! CONSOLE_LIVE_P (XCONSOLE (x)))			\
    dead_wrong_type_argument (Qconsole_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_CONSOLE(x) do {			\
  CONCHECK_CONSOLE (x);					\
  if (! CONSOLE_LIVE_P (XCONSOLE (x)))			\
    x = wrong_type_argument (Qconsole_live_p, (x));	\
} while (0)

#define CDFW_CONSOLE(obj)				\
   ((WINDOWP  (obj) && WINDOW_LIVE_P (XWINDOW(obj))) ? WINDOW_CONSOLE (XWINDOW (obj))	\
 : ((FRAMEP   (obj) && FRAME_LIVE_P (XFRAME (obj)))  ?  FRAME_CONSOLE (XFRAME  (obj))	\
 : ((DEVICEP  (obj) && DEVICE_LIVE_P (XDEVICE (obj))) ? DEVICE_CONSOLE (XDEVICE (obj))	\
 : ((CONSOLEP (obj) && CONSOLE_LIVE_P (XCONSOLE (obj))) ? obj				\
 : Qnil))))

#define CONSOLE_LOOP(concons) LIST_LOOP (concons, Vconsole_list)
#define CONSOLE_DEVICE_LOOP(devcons, con) \
  LIST_LOOP (devcons, CONSOLE_DEVICE_LIST (con))

EXFUN (Fconsole_disable_input, 1);
EXFUN (Fdelete_console, 2);
EXFUN (Fselect_console, 1);
EXFUN (Fselected_console, 0);

extern Lisp_Object Qcreate_console_hook, Qdelete_console_hook;
extern Lisp_Object Vconsole_defaults, Vconsole_type_list, Vselected_console;

int valid_console_type_p (Lisp_Object type);

Lisp_Object create_console (Lisp_Object name, Lisp_Object type,
			    Lisp_Object connection, Lisp_Object props);
void select_console_1 (Lisp_Object);
struct console *decode_console (Lisp_Object);
void add_entry_to_console_type_list (Lisp_Object symbol,
				     struct console_methods *type);
struct console_methods *decode_console_type (Lisp_Object type,
					     Error_Behavior errb);

enum console_variant get_console_variant (Lisp_Object type);

void delete_console_internal (struct console *con, int force,
			      int from_kill_emacs, int from_io_error);
void io_error_delete_console (Lisp_Object console);
void set_console_last_nonminibuf_frame (struct console *con,
					Lisp_Object frame);
void stuff_buffered_input (Lisp_Object);

#endif /* INCLUDED_console_h_ */
