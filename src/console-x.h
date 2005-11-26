/* Define X specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1996, 2002, 2003 Ben Wing.

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


/* Authorship:

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson (original
      multi-device work by Chuck Thompson).
 */

#ifndef INCLUDED_console_x_h_
#define INCLUDED_console_x_h_

#ifdef HAVE_X_WINDOWS

#include "console.h"
/* must come before Xutil.h, which is included indirectly from xintrinsic.h,
   to avoid breakage in keysym-parsing macros from X11R6, at least X.org as
   of 2005-01-17, and probably XFree86 too from reports on c.e.x */
#include <X11/keysym.h>
#include "xintrinsic.h"
#include "../lwlib/lwlib.h"

#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

#ifdef USE_XFT
/* shut up GCC */
#define face_index face_index_arg
#define glyph_index glyph_index_arg
#include <X11/Xft/Xft.h>
#undef glyph_index
#undef face_index
/* #### this should be made gone */
#ifndef XFT_VERSION
#define XFT_VERSION 1
#endif
#endif

/* R5 defines the XPointer type, but R4 doesn't.
   R4 also doesn't define a version number, but R5 does. */
#if (XlibSpecificationRelease < 5)
# define XPointer char *
#endif

#define Xt_SET_VALUE(widget, resource, value) do {	\
  Arg al;						\
  XtSetArg (al, resource, value);			\
  XtSetValues (widget, &al, 1);				\
} while (0)

#define Xt_GET_VALUE(widget, resource, location) do {	\
  Arg al;						\
  XtSetArg (al, resource, location);			\
  XtGetValues (widget, &al, 1);				\
} while (0)

#ifdef __cplusplus
#define X_CLASSFIELD c_class
#else
#define X_CLASSFIELD class
#endif

/* Variables associated with the X display frame this emacs is using. */
extern XtAppContext Xt_app_con;

extern Lisp_Object Vx_gc_pointer_shape;
extern Lisp_Object Vx_scrollbar_pointer_shape;
extern Lisp_Object Qx_error;

/* Number of pixels below each line. */
extern int x_interline_space; /* #### implement me */

extern Fixnum x_selection_timeout;

struct frame *x_any_window_to_frame (struct device *d, Window);
struct frame *x_any_widget_or_parent_to_frame (struct device *d,
					       Widget widget);
struct frame *decode_x_frame (Lisp_Object);
struct frame *x_window_to_frame (struct device *d, Window);
struct device *get_device_from_display (Display *dpy);
struct device *decode_x_device (Lisp_Object);

void x_handle_selection_notify (XSelectionEvent *event);
void x_handle_selection_request (XSelectionRequestEvent *event);
void x_handle_selection_clear (XSelectionClearEvent *event);
void x_handle_property_notify (XPropertyEvent *event);

void Xatoms_of_select_x (struct device *d);
void Xatoms_of_objects_x (struct device *d);

void x_wm_set_shell_iconic_p (Widget shell, int iconic_p);
void x_wm_set_cell_size (Widget wmshell, int cw, int ch);
void x_wm_set_variable_size (Widget wmshell, int width, int height);

const char *x_event_name (int event_type);
int check_if_pending_expose_event (struct device *d);
int x_error_handler (Display *disp, XErrorEvent *event);
void expect_x_error (Display *dpy);
int x_error_occurred_p (Display *dpy);
int signal_if_x_error (Display *dpy, int resumable_p);
int x_IO_error_handler (Display *disp);

widget_value *xmalloc_widget_value (void);

LWLIB_ID new_lwlib_id (void);

void gcpro_popup_callbacks (LWLIB_ID id);
void ungcpro_popup_callbacks (LWLIB_ID id);
int popup_handled_p (LWLIB_ID id);
void free_popup_widget_value_tree (widget_value *wv);
void popup_selection_callback (Widget widget, LWLIB_ID ignored_id,
			       XtPointer client_data);
Extbyte *add_accel_and_to_external (Lisp_Object string);
int button_item_to_widget_value (Lisp_Object gui_object_instance,
				 Lisp_Object gui_item, widget_value *wv,
				 int allow_text_field_p, int no_keys_p,
				 int menu_entry_p, int accel_p);
widget_value * gui_items_to_widget_values (Lisp_Object gui_object_instance,
					   Lisp_Object items, int accel_p);
Extbyte *menu_separator_style_and_to_external (const Ibyte *s);
Lisp_Object widget_value_unwind (Lisp_Object closure);

void x_output_string (struct window *w, struct display_line *dl,
		      Ichar_dynarr *buf, int xpos, int xoffset,
		      int start_pixpos, int width, face_index findex,
		      int cursor, int cursor_start, int cursor_width,
		      int cursor_height);
void x_output_x_pixmap (struct frame *f, Lisp_Image_Instance *p,
			int x, int y, int xoffset, int yoffset,
			int width, int height,
			unsigned long fg, unsigned long bg,
			GC override_gc);
void x_output_shadows (struct frame *f, int x, int y, int width,
		       int height, GC top_shadow_gc,
		       GC bottom_shadow_gc, GC background_gc,
		       int shadow_thickness, int edges);
void x_generate_shadow_pixels (struct frame *f,
			       unsigned long *top_shadow,
			       unsigned long *bottom_shadow,
			       unsigned long background,
			       unsigned long core_background);

int x_initialize_frame_menubar (struct frame *f);
void x_init_modifier_mapping (struct device *d);

int x_frame_window_state (struct frame *f);

#define X_ERROR_OCCURRED(dpy, body)	\
     (expect_x_error (dpy), body, x_error_occurred_p (dpy))

#define HANDLING_X_ERROR(dpy, body)	\
     (expect_x_error (dpy), body, signal_if_x_error (dpy, 0))

void Initialize_Locale (void);

#ifdef HAVE_XIM

/* X Input Method `methods' */
void XIM_init_device     (struct device *d);
void XIM_init_frame	 (struct frame *f);
void XIM_SetSpotLocation (struct frame *f, int x, int y);
void XIM_SetGeometry	 (struct frame *f);
void XIM_focus_event	 (struct frame *f, int in_p);

#ifdef XIM_XLIB
/* XtTypeConverter */
Boolean EmacsXtCvtStringToXIMStyles (
  Display     *dpy,
  XrmValuePtr  args,
  Cardinal    *num_args,
  XrmValuePtr  from,
  XrmValuePtr  to_in_out,
  XtPointer   *converter_data);

/* XtDestructor */
void EmacsFreeXIMStyles (
  XtAppContext app,
  XrmValuePtr  to,
  XtPointer    converter_data,
  XrmValuePtr  args,
  Cardinal    *num_args);

#ifdef DEBUG_XEMACS
void describe_Window	 (Window win);
void describe_XFontSet	 (XFontSet font_set);
void describe_XIM	 (XIM im);
void describe_XIMStyle	 (XIMStyle   style);
void describe_XIMStyles	 (XIMStyles *styles);
void describe_XIC	 (XIC ic);
void describe_event_mask (unsigned long mask);
void describe_XRectangle (char *name, XRectangle *rect);
void describe_Status	 (Status status);
#endif /* DEBUG_XEMACS */
#endif /* XIM_XLIB */
#endif /* HAVE_XIM */

extern Lisp_Object Qxintl;

extern int in_resource_setting;
extern int in_specifier_change_function;

extern Lisp_Object Vx_initial_argv_list; /* #### ugh! */

#endif /* HAVE_X_WINDOWS */

#endif /* INCLUDED_console_x_h_ */
