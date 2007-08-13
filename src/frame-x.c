/* Functions for the X window system.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* Substantially rewritten for XEmacs.  */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* Numerous places access the fields of
				   a core widget directly.  We could
				   use XtVaGetValues(), but ... */
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include "xmu.h"
#include "EmacsManager.h"
#include "EmacsFrameP.h"
#include "EmacsShell.h"
#ifdef EXTERNAL_WIDGET
#include "ExternalShell.h"
#endif
#include "glyphs-x.h"
#include "objects-x.h"
#include "scrollbar-x.h"

#include "buffer.h"
#include "events.h"
#include "extents.h"
#include "faces.h"
#include "frame.h"
#include "window.h"

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_x_frame_plist;

Lisp_Object Qwindow_id;
Lisp_Object Qpopup;
Lisp_Object Qx_resource_name;


/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* Return the Emacs frame-object corresponding to an X window */
struct frame *
x_window_to_frame (struct device *d, Window wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  /* This function was previously written to accept only a window argument
     (and to loop over all devices looking for a matching window), but
     that is incorrect because window ID's are not unique across displays. */

  for (tail = DEVICE_FRAME_LIST (d); CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
	continue;
      f = XFRAME (frame);
      if (FRAME_X_P (f) && XtWindow (FRAME_X_TEXT_WIDGET (f)) == wdesc)
	return f;
    }
  return 0;
}

/* Like x_window_to_frame but also compares the window with the widget's
   windows */
struct frame *
x_any_window_to_frame (struct device *d, Window wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  assert (DEVICE_X_P (d));

  /* This function was previously written to accept only a window argument
     (and to loop over all devices looking for a matching window), but
     that is incorrect because window ID's are not unique across displays. */

  for (tail = DEVICE_FRAME_LIST (d); CONSP (tail); tail = XCDR (tail))
    {
      int i;

      frame = XCAR (tail);
      f = XFRAME (frame);
      /* This frame matches if the window is any of its widgets. */
      if (wdesc == XtWindow (FRAME_X_SHELL_WIDGET (f)) ||
	  wdesc == XtWindow (FRAME_X_CONTAINER_WIDGET (f)) ||
	  wdesc == XtWindow (FRAME_X_TEXT_WIDGET (f)))
	return f;

      /* Match if the window is one of the widgets at the top of the frame
	 (menubar, Energize psheets). */

      /* Note: Jamie once said

	 "Do *not* match if the window is this frame's psheet."

	 But this is wrong and will screw up some functions that expect
	 x_any_window_to_frame() to work as advertised.  I think the reason
	 for this statement is that, in the old (broken) event loop, where
	 not all events went through XtDispatchEvent(), psheet events
	 would incorrectly get sucked away by Emacs if this function matched
	 on psheet widgets. */

      for (i = 0; i < FRAME_X_NUM_TOP_WIDGETS (f); i++)
	{
	  Widget wid = FRAME_X_TOP_WIDGETS (f)[i];
	  if (wid && XtIsManaged (wid) && wdesc == XtWindow (wid))
	    return f;
	}

#ifdef HAVE_SCROLLBARS
      /* Match if the window is one of this frame's scrollbars. */
      if (x_window_is_scrollbar (f, wdesc))
	return f;
#endif
    }

  return 0;
}

struct frame *
x_any_widget_or_parent_to_frame (struct device *d, Widget widget)
{
  while (widget)
    {
      struct frame *f = x_any_window_to_frame (d, XtWindow (widget));
      if (f)
	return f;
      widget = XtParent (widget);
    }

  return 0;
}

struct frame *
decode_x_frame (Lisp_Object frame)
{
  if (NILP (frame))
    XSETFRAME (frame, selected_frame ());
  CHECK_LIVE_FRAME (frame);
  /* this will also catch dead frames, but putting in the above check
     results in a more useful error */
  CHECK_X_FRAME (frame);
  return XFRAME (frame);
}


/************************************************************************/
/*			window-manager interactions			*/
/************************************************************************/

#if 0
/* Not currently used. */

void
x_wm_mark_shell_size_user_specified (Widget wmshell)
{
  if (! XtIsWMShell (wmshell)) abort ();
  EmacsShellSetSizeUserSpecified (wmshell);
}

void
x_wm_mark_shell_position_user_specified (Widget wmshell)
{
  if (! XtIsWMShell (wmshell)) abort ();
  EmacsShellSetPositionUserSpecified (wmshell);
}

#endif

void
x_wm_set_shell_iconic_p (Widget shell, int iconic_p)
{
  if (! XtIsWMShell (shell)) abort ();

  /* Because of questionable logic in Shell.c, this sequence can't work:

       w = XtCreatePopupShell (...);
       XtVaSetValues (w, XtNiconic, True, 0);
       XtRealizeWidget (w);

     The iconic resource is only consulted at initialization time (when
     XtCreatePopupShell is called) instead of at realization time (just
     before the window gets created, which would be more sensible) or
     at management-time (just before the window gets mapped, which would
     be most sensible of all).

     The bug is that Shell's SetValues method doesn't do anything to
     w->wm.wm_hints.initial_state until after the widget has been realized.
     Calls to XtSetValues are ignored in the window between creation and
     realization.  This is true of MIT X11R5 patch level 25, at least.
     (Apparently some other versions of Xt don't have this bug?)
   */
  XtVaSetValues (shell, XtNiconic, iconic_p, 0);
  EmacsShellSmashIconicHint (shell, iconic_p);
}

void
x_wm_set_cell_size (Widget wmshell, int cw, int ch)
{
  if (!XtIsWMShell (wmshell))
    abort ();
  if (cw <= 0 || ch <= 0)
    abort ();

  XtVaSetValues (wmshell,
		 XtNwidthInc, cw, 
		 XtNheightInc, ch,
		 0);
}

void
x_wm_set_variable_size (Widget wmshell, int width, int height)
{
  if (!XtIsWMShell (wmshell))
    abort ();
#ifdef DEBUG_GEOMETRY_MANAGEMENT
  /* See comment in EmacsShell.c */
  printf ("x_wm_set_variable_size: %d %d\n", width, height);
  fflush (stdout);
#endif
  XtVaSetValues (wmshell,
		 XtNwidthCells, width,
		 XtNheightCells, height,
		 0);
}

/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS
   and WM_DELETE_WINDOW, then add them.  (They may already be present
   because of the toolkit (Motif adds them, for example, but Xt doesn't).
 */
static void
x_wm_hack_wm_protocols (Widget widget)
{
  Display *dpy = XtDisplay (widget);
  struct device *d = get_device_from_display (dpy);
  Window w = XtWindow (widget);
  int need_delete = 1;
  int need_focus = 1;

  if (!XtIsWMShell (widget))
    abort ();

  {
    Atom type, *atoms = 0;
    int format = 0;
    unsigned long nitems = 0;
    unsigned long bytes_after;

    if (Success == XGetWindowProperty (dpy, w, DEVICE_XATOM_WM_PROTOCOLS (d),
				       0, 100, False, XA_ATOM,
				       &type, &format, &nitems, &bytes_after,
				       (unsigned char **) &atoms)
	&& format == 32 && type == XA_ATOM)
      while (nitems > 0)
	{
	  nitems--;
	  if (atoms [nitems] == DEVICE_XATOM_WM_DELETE_WINDOW (d))
	    need_delete = 0;
	  else if (atoms [nitems] == DEVICE_XATOM_WM_TAKE_FOCUS (d))
	    need_focus = 0;
	}
    if (atoms) XFree ((char *) atoms);
  }
  {
    Atom props [10];
    int count = 0;
    if (need_delete) props[count++] = DEVICE_XATOM_WM_DELETE_WINDOW (d);
    if (need_focus)  props[count++] = DEVICE_XATOM_WM_TAKE_FOCUS (d);
    if (count)
      XChangeProperty (dpy, w, DEVICE_XATOM_WM_PROTOCOLS (d), XA_ATOM, 32,
		       PropModeAppend, (unsigned char *) props, count);
  }
}

static void
x_wm_store_class_hints (Widget shell, char *frame_name)
{
  Display *dpy = XtDisplay (shell);
  char *app_name, *app_class;
  XClassHint classhint;

  if (!XtIsWMShell (shell))
    abort ();

  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  classhint.res_name = frame_name;
  classhint.res_class = app_class;
  XSetClassHint (dpy, XtWindow (shell), &classhint);
}

static void
x_wm_maybe_store_wm_command (struct frame *f)
{
  Widget w = FRAME_X_SHELL_WIDGET (f);
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  if (!XtIsWMShell (w))
    abort ();

  if (NILP (DEVICE_X_WM_COMMAND_FRAME (d)))
    {
      int argc;
      char **argv;
      make_argc_argv (Vcommand_line_args, &argc, &argv);
      XSetCommand (XtDisplay (w), XtWindow (w), argv, argc);
      free_argc_argv (argv);
      XSETFRAME (DEVICE_X_WM_COMMAND_FRAME (d), f);
    }
}

/* If we're deleting the frame on which the WM_COMMAND property has been
   set, then move that property to another frame so that there is exactly
   one frame that has that property set.
 */
static void
x_wm_maybe_move_wm_command (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  /* There may not be a frame in DEVICE_X_WM_COMMAND_FRAME()
     if we C-c'ed at startup at the right time. */
  if (FRAMEP (DEVICE_X_WM_COMMAND_FRAME (d))
      && f == XFRAME (DEVICE_X_WM_COMMAND_FRAME (d)))
    {
      Lisp_Object rest = DEVICE_FRAME_LIST (d);
      DEVICE_X_WM_COMMAND_FRAME (d) = Qnil;
      /* find some random other X frame that is not this one, or give up */
      /* skip non-top-level (ExternalClient) frames */
      while (!NILP (rest) &&
	     (f == XFRAME (XCAR (rest)) ||
	      !FRAME_X_TOP_LEVEL_FRAME_P (XFRAME (XCAR (rest)))))
	rest = XCDR (rest);
      if (NILP (rest))
	return;
      f = XFRAME (XCAR (rest));
      x_wm_maybe_store_wm_command (f);
    }
}

static int
x_frame_iconified_p (struct frame *f)
{
  Atom actual_type;
  int actual_format;
  unsigned long nitems, bytesafter;
  unsigned long *datap = 0;
  Widget widget;
  int result = 0;
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  widget = FRAME_X_SHELL_WIDGET (f);
  if (Success == XGetWindowProperty (XtDisplay (widget), XtWindow (widget),
				     DEVICE_XATOM_WM_STATE (d), 0, 2, False,
				     DEVICE_XATOM_WM_STATE (d), &actual_type,
				     &actual_format, &nitems, &bytesafter,
				     (unsigned char **) &datap)
      && datap)
    {
      if (nitems <= 2	/* "suggested" by ICCCM version 1 */
	  && datap[0] == IconicState)
	result = 1;
      XFree ((char *) datap);
    }
  return result;
}


/************************************************************************/
/*                          frame properties                            */
/************************************************************************/

/* Connect the frame-property names (symbols) to the corresponding
   X Resource Manager names.  The name of a property, as a Lisp symbol,
   has an `x-resource-name' property which is a Lisp_String. */

static void
init_x_prop_symbols (void)
{
#define def(sym, rsrc) \
   pure_put (sym, Qx_resource_name, build_string (rsrc))
#define defi(sym,rsrc) \
   def (sym, rsrc); pure_put (sym, Qintegerp, Qt)

#if 0 /* this interferes with things. #### fix this right */
  def (Qminibuffer, XtNminibuffer);
  def (Qunsplittable, XtNunsplittable);
#endif
  defi(Qinternal_border_width, XtNinternalBorderWidth);
#ifdef HAVE_TOOLBARS
  def (Qtop_toolbar_shadow_color, XtNtopToolBarShadowColor);
  def (Qbottom_toolbar_shadow_color, XtNbottomToolBarShadowColor);
  def (Qbackground_toolbar_color, XtNbackgroundToolBarColor);
  def (Qtop_toolbar_shadow_pixmap, XtNtopToolBarShadowPixmap);
  def (Qbottom_toolbar_shadow_pixmap, XtNbottomToolBarShadowPixmap);
  defi(Qtoolbar_shadow_thickness, XtNtoolBarShadowThickness);
#endif
  def (Qscrollbar_placement, XtNscrollBarPlacement);
  defi(Qinter_line_space, XtNinterline);
  /* font, foreground */
  def (Qiconic, XtNiconic);
  def (Qbar_cursor, XtNbarCursor);
  def (Qvisual_bell, XtNvisualBell);
  defi(Qbell_volume, XtNbellVolume);
  def (Qpointer_background, XtNpointerBackground);
  def (Qpointer_color, XtNpointerColor);
  def (Qtext_pointer, XtNtextPointer);
  def (Qspace_pointer, XtNspacePointer);
  def (Qmodeline_pointer, XtNmodeLinePointer);
  def (Qgc_pointer, XtNgcPointer);
  /* geometry, initial_geometry */
  def (Qinitially_unmapped, XtNinitiallyUnmapped);
  /* preferred_width, preferred_height */
  def (Quse_backing_store, XtNuseBackingStore);

  /* inherited: */

  def (Qborder_color, XtNborderColor);
  defi(Qborder_width, XtNborderWidth);
  defi(Qwidth, XtNwidth);
  defi(Qheight, XtNheight);
  defi(Qleft, XtNx);
  defi(Qtop, XtNy);
  
#undef def
}

static Lisp_Object
color_to_string (Widget w, unsigned long pixel)
{
  char buf[255];

  XColor color;
  color.pixel = pixel;
  XQueryColor (XtDisplay (w), w->core.colormap, &color);
  sprintf (buf, "#%04x%04x%04x", color.red, color.green, color.blue);
  return build_string (buf);
}

static void
x_get_top_level_position (Display *d, Window w, Position *x, Position *y)
{
  Window root, parent = w, *children;
  unsigned int nchildren;
  XWindowAttributes xwa;

  do
    {
      w = parent;
      if (!XQueryTree (d, w, &root, &parent, &children, &nchildren))
	{
	  *x = 0;
	  *y = 0;
	  return;
	}
      XFree (children);
    }
  while (root != parent);
  XGetWindowAttributes (d, w, &xwa);
  *x = xwa.x;
  *y = xwa.y;
}

static void
x_smash_bastardly_shell_position (Widget shell)
{
  /* Naturally those bastards who wrote Xt couldn't be bothered
     to learn about race conditions and such.  We can't trust
     the X and Y values to have any semblance of correctness,
     so we smash the right values in place. */
 
 /* We might be called before we've actually realized the window (if
     we're checking for the minibuffer resource).  This will bomb in
     that case so we don't bother calling it. */
  if (XtWindow (shell))
    x_get_top_level_position (XtDisplay (shell), XtWindow (shell),
			      &shell->core.x, &shell->core.y);
}

static Lisp_Object
x_frame_property (struct frame *f, Lisp_Object property)
{
  Widget shell = FRAME_X_SHELL_WIDGET (f);
  EmacsFrame w = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  Widget gw = (Widget) w;

#define FROB(propprop, value) 	\
do {				\
  if (EQ (property, propprop))	\
    {				\
      return (value);		\
    }				\
} while (0)

  if (EQ (property, Qleft) || EQ (property, Qtop))
    x_smash_bastardly_shell_position (shell);
  FROB (Qleft, make_int (shell->core.x));
  FROB (Qtop, make_int (shell->core.y));
  FROB (Qborder_width, make_int (w->core.border_width));
  FROB (Qinternal_border_width,
	make_int (w->emacs_frame.internal_border_width));
  FROB (Qborder_color, color_to_string (gw, w->core.border_pixel));
#ifdef HAVE_TOOLBARS
  FROB (Qtop_toolbar_shadow_color,
	color_to_string (gw, w->emacs_frame.top_toolbar_shadow_pixel));
  FROB (Qbottom_toolbar_shadow_color,
	color_to_string (gw, w->emacs_frame.bottom_toolbar_shadow_pixel));
  FROB (Qbackground_toolbar_color,
	color_to_string (gw, w->emacs_frame.background_toolbar_pixel));
  FROB (Qtoolbar_shadow_thickness,
	make_int (w->emacs_frame.toolbar_shadow_thickness));
#endif
  FROB (Qinter_line_space, make_int (w->emacs_frame.interline));
  FROB (Qwindow_id, Fx_window_id (make_frame (f)));

#undef FROB

  return Qunbound;
}

static int
x_internal_frame_property_p (struct frame *f, Lisp_Object property)
{
  if (EQ (property, Qleft)
      || EQ (property, Qtop)
      || EQ (property, Qborder_width)
      || EQ (property, Qinternal_border_width)
      || EQ (property, Qborder_color)
#ifdef HAVE_TOOLBARS
      || EQ (property, Qtop_toolbar_shadow_color)
      || EQ (property, Qbottom_toolbar_shadow_color)
      || EQ (property, Qbackground_toolbar_color)
      || EQ (property, Qtoolbar_shadow_thickness)
#endif
      || EQ (property, Qinter_line_space)
      || EQ (property, Qwindow_id)
      || STRINGP (property))
    return 1;

  return 0;
}

static Lisp_Object
x_frame_properties (struct frame *f)
{
  Lisp_Object result = Qnil;
  Widget shell = FRAME_X_SHELL_WIDGET (f);
  EmacsFrame w = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  Widget gw = (Widget) w;

#define FROB(propprop, value)				\
do {							\
  Lisp_Object temtem = (value);				\
  if (!NILP (temtem))					\
    /* backwards order; we reverse it below */		\
    result = Fcons (temtem, Fcons (propprop, result));	\
} while (0)

  x_smash_bastardly_shell_position (shell);
  FROB (Qleft, make_int (shell->core.x));
  FROB (Qtop, make_int (shell->core.y));
  FROB (Qborder_width, make_int (w->core.border_width));
  FROB (Qinternal_border_width,
	make_int (w->emacs_frame.internal_border_width));
  FROB (Qborder_color, color_to_string (gw, w->core.border_pixel));
#ifdef HAVE_TOOLBARS
  FROB (Qtop_toolbar_shadow_color,
	color_to_string (gw, w->emacs_frame.top_toolbar_shadow_pixel));
  FROB (Qbottom_toolbar_shadow_color,
	color_to_string (gw, w->emacs_frame.bottom_toolbar_shadow_pixel));
  FROB (Qbackground_toolbar_color,
	color_to_string (gw, w->emacs_frame.background_toolbar_pixel));
  FROB (Qtoolbar_shadow_thickness,
	make_int (w->emacs_frame.toolbar_shadow_thickness));
#endif
  FROB (Qinter_line_space, make_int (w->emacs_frame.interline));
  FROB (Qwindow_id, Fx_window_id (make_frame (f)));

#undef FROB

  return result;
}


/* Functions called only from `x_set_frame_properties' to set
   individual properties. */

static void
x_set_frame_text_value (struct frame *f, Bufbyte *value,
			String Xt_resource_name,
			String Xt_resource_encoding_name)
{
  Atom encoding = XA_STRING;
  String new_XtValue = (String) value;
  String old_XtValue = NULL;
  Bufbyte *ptr;
  Arg av[2];

  /* ### Caching is device-independent - belongs in update_frame_title. */
  XtSetArg (av[0], Xt_resource_name, &old_XtValue);
  XtGetValues (FRAME_X_SHELL_WIDGET (f), av, 1);
  if (!old_XtValue || strcmp (new_XtValue, old_XtValue))
    {
      XtSetArg (av[0], Xt_resource_name, new_XtValue);
      XtSetArg (av[1], Xt_resource_encoding_name, encoding);
      XtSetValues (FRAME_X_SHELL_WIDGET (f), av, 2);
    }
}

static void 
x_set_title_from_bufbyte (struct frame *f, Bufbyte *name)
{
  x_set_frame_text_value (f, name, XtNtitle, XtNtitleEncoding);
}

static void
x_set_icon_name_from_bufbyte (struct frame *f, Bufbyte *name)
{
  x_set_frame_text_value (f, name, XtNiconName, XtNiconNameEncoding);
}

/* Set the initial frame size as specified.  This function is used
   when the frame's widgets have not yet been realized.  In this
   case, it is not sufficient just to set the width and height of
   the EmacsFrame widget, because they will be ignored when the
   widget is realized (instead, the shell's geometry resource is
   used). */

static void
x_set_initial_frame_size (struct frame *f, int flags, int x, int y,
			  unsigned int w, unsigned int h)
{
  char shell_geom [255];
  int xval, yval;
  char xsign, ysign;
  char uspos = !!(flags & (XValue | YValue));
  char ussize = !!(flags & (WidthValue | HeightValue));
  char *temp;

  /* assign the correct size to the EmacsFrame widget ... */
  EmacsFrameSetCharSize (FRAME_X_TEXT_WIDGET (f), w, h);

  /* and also set the WMShell's geometry */
  (flags & XNegative) ? (xval = -x, xsign = '-') : (xval = x, xsign = '+');
  (flags & YNegative) ? (yval = -y, ysign = '-') : (yval = y, ysign = '+');
  
  if (uspos && ussize)
    sprintf (shell_geom, "=%dx%d%c%d%c%d", w, h, xsign, xval, ysign, yval);
  else if (uspos)
    sprintf (shell_geom, "=%c%d%c%d", xsign, xval, ysign, yval);
  else if (ussize)
    sprintf (shell_geom, "=%dx%d", w, h);
  
  if (uspos || ussize)
    {
      temp = xmalloc (1 + strlen (shell_geom));
      strcpy (temp, shell_geom);
      FRAME_X_GEOM_FREE_ME_PLEASE (f) = temp;
    }
  else
    temp = NULL;
  XtVaSetValues (FRAME_X_SHELL_WIDGET (f), XtNgeometry, temp, 0);
}

/* Report to X that a frame property of frame S is being set or changed.
   If the property is not specially recognized, do nothing.
 */

static void
x_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  int x = 0, y = 0;
  Dimension width = 0, height = 0;
  Bool width_specified_p = False;
  Bool height_specified_p = False;
  Bool x_position_specified_p = False;
  Bool y_position_specified_p = False;
  Bool internal_border_width_specified = False;
  Lisp_Object tail;
  Widget w = FRAME_X_TEXT_WIDGET (f);
  
  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      Lisp_Object prop = Fcar (tail);
      Lisp_Object val = Fcar (Fcdr (tail));
      
      if (STRINGP (prop))
	{
	  CONST char *extprop;
	  
	  if (string_length (XSTRING (prop)) == 0)
	    continue;

	  GET_C_STRING_CTEXT_DATA_ALLOCA (prop, extprop);
	  if (STRINGP (val))
	    {
	      Extbyte *extval;
	      Extcount extvallen;

	      GET_STRING_CTEXT_DATA_ALLOCA (val, extval, extvallen);
	      XtVaSetValues (w, XtVaTypedArg, extprop,
			     XtRString, extval, extvallen + 1, 0);
	    }
	  else
	    XtVaSetValues (w, XtVaTypedArg,
			   extprop, XtRInt, XINT (val),
			   sizeof (int),
			   0);
	}
      else if (SYMBOLP (prop))
	{
	  Lisp_Object str = Fget (prop, Qx_resource_name, Qnil);
	  int int_p = !NILP (Fget (prop, Qintegerp, Qnil));

	  if (NILP (prop) || NILP (str))
	    {
	      /* Kludge to handle the font property. */
	      if (EQ (prop, Qfont))
		{
		  /* If the value is not a string we silently ignore it. */
		  if (STRINGP (val))
		    {
		      Lisp_Object frm, font_spec;

		      XSETFRAME (frm, f);
		      font_spec = Fget (Fget_face (Qdefault), Qfont, Qnil);

		      Fadd_spec_to_specifier (font_spec, val, frm, Qnil, Qnil);
		      update_frame_face_values (f);
		    }

		  continue;
		}
	      else
		continue;
	    }
	  CHECK_STRING (str);

	  /* Kludge the width/height so that we interpret them in characters
	     instead of pixels.  Yuck yuck yuck. */
	  if (!strcmp ((char *) string_data (XSTRING (str)), "width"))
	    {
	      CHECK_INT (val);
	      width = XINT (val);
	      width_specified_p = True;
	      continue;
	    }
	  if (!strcmp ((char *) string_data (XSTRING (str)), "height"))
	    {
	      CHECK_INT (val);
	      height = XINT (val);
	      height_specified_p = True;
	      continue;
	    }
	  /* Further kludge the x/y. */
	  if (!strcmp ((char *) string_data (XSTRING (str)), "x"))
	    {
	      CHECK_INT (val);
	      x = XINT (val);
	      x_position_specified_p = True;
	      continue;
	    }
	  if (!strcmp ((char *) string_data (XSTRING (str)), "y"))
	    {
	      CHECK_INT (val);
	      y = XINT (val);
	      y_position_specified_p = True;
	      continue;
	    }
	  /* Have you figured out by now that this entire function is
             one gigantic kludge? */
	  if (!strcmp ((char *) string_data (XSTRING (str)),
		       "internalBorderWidth"))
	    {
	      internal_border_width_specified = True;
	    }

	  if (int_p)
	    {
	      CHECK_INT (val);
	      XtVaSetValues (w, (char *) string_data (XSTRING (str)),
			     XINT (val), 0);
	    }
	  else if (EQ (val, Qt))
	    XtVaSetValues (w,
			   /* XtN... */
			   (char *) string_data (XSTRING (str)),
			   True,
			   0);
	  else if (EQ (val, Qnil))
	    XtVaSetValues (w,
			   /* XtN... */
			   (char *) string_data (XSTRING (str)),
			   False,
			   0);
	  else
	    {
	      CHECK_STRING (val);
	      XtVaSetValues (w, XtVaTypedArg,
			     /* XtN... */
			     (char *) string_data (XSTRING (str)),
			     XtRString,
			     string_data (XSTRING (val)),
			     string_length (XSTRING (val)) + 1,
			     0);
	    }

#ifdef HAVE_SCROLLBARS
	  if (!strcmp ((char *) string_data (XSTRING (str)), "scrollBarWidth")
	      || !strcmp ((char *) string_data (XSTRING (str)),
			  "scrollBarHeight"))
	    {
	      x_update_frame_scrollbars (f);
	    }
#endif
	}
    }

  /* Kludge kludge kludge.   We need to deal with the size and position
   specially. */
  {
    int size_specified_p = width_specified_p || height_specified_p;
    int position_specified_p = x_position_specified_p ||
      y_position_specified_p;

    if (!width_specified_p)
      width = FRAME_WIDTH (f);
    if (!height_specified_p)
      height = FRAME_HEIGHT (f);
    
    /* Kludge kludge kludge kludge. */
    if (!x_position_specified_p)
      x = (int) (FRAME_X_SHELL_WIDGET (f)->core.x);
    if (!y_position_specified_p)
      y = (int) (FRAME_X_SHELL_WIDGET (f)->core.y);
      
    if (!f->init_finished)
      {
	int flags = (size_specified_p ? WidthValue | HeightValue : 0) |
	  (position_specified_p ? XValue | YValue : 0) |
	  (x < 0 ? XNegative : 0) | (y < 0 ? YNegative : 0);
	if (size_specified_p
	    || position_specified_p
	    || internal_border_width_specified)
	  x_set_initial_frame_size (f, flags, x, y, width, height);
      }
    else
      {
	if (size_specified_p || internal_border_width_specified)
	  {
	    Lisp_Object frame;
	    XSETFRAME (frame, f);
	    Fset_frame_size (frame, make_int (width),
			      make_int (height), Qnil);
	  }
	if (position_specified_p)
	  {
	    Lisp_Object frame;
	    XSETFRAME (frame, f);
	    Fset_frame_position (frame, make_int (x), make_int (y));
	  }
      }
  }
}

static int frame_title_format_already_set;

static void
maybe_set_frame_title_format (Widget shell)
{

  /* Only do this if this is the first X frame we're creating.
     
     If the *title resource (or -title option) was specified, then
     set frame-title-format to its value.
     */

  if (!frame_title_format_already_set)
    {
      /* No doubt there's a less stupid way to do this. */
      char *results [2];
      XtResource resources [2];
      results [0] = results [1] = 0;
      resources [0].resource_name = XtNtitle;
      resources [0].resource_class = XtCTitle;
      resources [0].resource_type = XtRString;
      resources [0].resource_size = sizeof (String);
      resources [0].resource_offset = 0;
      resources [0].default_type = XtRString;
      resources [0].default_addr = 0;
      resources [1].resource_name = XtNiconName;
      resources [1].resource_class = XtCIconName;
      resources [1].resource_type = XtRString;
      resources [1].resource_size = sizeof (String);
      resources [1].resource_offset = sizeof (char *);
      resources [1].default_type = XtRString;
      resources [1].default_addr = 0;
      XtGetSubresources (XtParent (shell), (XtPointer) results,
			 shell->core.name,
			 shell->core.widget_class->core_class.class_name,
			 resources, XtNumber (resources), 0, 0);
      if (results[0])
	Vframe_title_format = build_string (results[0]);
      if (results[1])
	Vframe_icon_title_format = build_string (results[1]);
    }

  frame_title_format_already_set = 1;
}

#ifdef HAVE_CDE
#include <Dt/Dt.h>
#include <Dt/Dnd.h>

void
x_cde_transfer_callback (Widget widget, XtPointer clientData,
			 XtPointer callData)
{
  char *filePath, *buf, *data;
  int ii;
  Lisp_Object path = Qnil;
  Lisp_Object frame = Qnil;
  struct gcpro gcpro1, gcpro2;
    
  DtDndTransferCallbackStruct *transferInfo =
    (DtDndTransferCallbackStruct *) callData;

  if (transferInfo == NULL)
    return;
	
  GCPRO2 (path, frame);
    
  frame = make_frame ((struct frame *) clientData);
  if (transferInfo->dropData->protocol == DtDND_FILENAME_TRANSFER)
    {
      for (ii = 0; ii < transferInfo->dropData->numItems; ii++) 
	{
	  filePath = transferInfo->dropData->data.files[ii];
	  path = make_string ((Bufbyte *)filePath, strlen (filePath));
	  va_run_hook_with_args (Qdrag_and_drop_functions, 2, frame, path);
	}
    }
  else if (transferInfo->dropData->protocol == DtDND_BUFFER_TRANSFER)
    {
      for (ii = 0; ii < transferInfo->dropData->numItems; ii++)
 	{
 	  filePath = transferInfo->dropData->data.buffers[ii].name;
 	  path = (filePath != NULL) ?
            make_string ((Bufbyte *)filePath, strlen (filePath)) : Qnil;
 	  buf = transferInfo->dropData->data.buffers[ii].bp;
 	  data = make_string ((Bufbyte *)buf,
			      transferInfo->dropData->data.buffers[ii].size);
 	  va_run_hook_with_args(Qdrag_and_drop_functions, 3, frame, path, data);
 	}
    }

  UNGCPRO;
  return;
}
#endif

#ifdef HAVE_OFFIX_DND
#include <OffiX/DragAndDrop.h>

void 
x_offix_drop_event_handler (Widget widget, XtPointer data, XEvent *event,
			    Boolean *b)
{
  int i, len, Type;	
  unsigned char *Data;
  unsigned long Size;

  Lisp_Object path = Qnil;
  Lisp_Object frame = Qnil;

  struct gcpro gcpro1, gcpro2;

  Type = DndDataType (event); 
  if ((Type != DndFile) && (Type != DndFiles) && (Type != DndExe))
    return;
  DndGetData (&Data, &Size);
  
  GCPRO2 (path, frame);

  frame = make_frame ((struct frame *) data);

  if (Type == DndFiles)
    {
      while (*Data)
	{
	  len = strlen ((char*) Data);
	  path = make_string ((char*) Data, len);
	  va_run_hook_with_args (Qdrag_and_drop_functions, 2, frame, path);
	  Data += len+1;
	}
    }
  else
    {
      path = make_string ((char*) Data, strlen (Data));    
      va_run_hook_with_args (Qdrag_and_drop_functions, 2, frame, path);
    }

  UNGCPRO;
  return;
}
#endif /* HAVE_OFFIX_DND */


/************************************************************************/
/*				widget creation				*/
/************************************************************************/

/* The widget hierarchy is

	argv[0]			shell		container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame

   We accept geometry specs in this order:

	*FRAME-NAME.geometry
	*EmacsFrame.geometry
	Emacs.geometry

   Other possibilities for widget hierarchies might be

	argv[0]			frame		container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame
   or
	argv[0]			FRAME-NAME	container	FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame
   or
	argv[0]			FRAME-NAME	container	emacsTextPane
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame

#ifdef EXTERNAL_WIDGET
   The ExternalShell widget is simply a replacement for the Shell widget 
   which is able to deal with using an externally-supplied window instead
   of always creating its own.
#endif

*/

#ifdef EXTERNAL_WIDGET

static int
is_valid_window (Window w, struct device *d)
{
  XWindowAttributes xwa;
  Display *dpy = DEVICE_X_DISPLAY (d);

  expect_x_error (dpy);
  XGetWindowAttributes (dpy, w, &xwa);
  return !x_error_occurred_p (dpy);
}

#endif /* EXTERNAL_WIDGET */

/* This sends a synthetic mouse-motion event to the frame, if the mouse
   is over the frame.  This ensures that the cursor gets set properly
   before the user moves the mouse for the first time. */

static void
x_send_synthetic_mouse_event (struct frame *f)
{
  /* #### write this function. */
}

static int
first_x_frame_p (struct frame *f)
{
  Lisp_Object rest = DEVICE_FRAME_LIST (XDEVICE (f->device));
  while (!NILP (rest) &&
	 (f == XFRAME (XCAR (rest)) ||
	  !FRAME_X_P (XFRAME (XCAR (rest)))))
    rest = XCDR (rest);
  return (NILP (rest));
}

/* Figure out what size the EmacsFrame widget should initially be,
   and set it.  Should be called after the default font has been
   determined but before the widget has been realized. */

static void
x_initialize_frame_size (struct frame *f)
{
  /* Geometry of the AppShell */
  int app_flags = 0;
  int app_x = 0;
  int app_y = 0;
  unsigned int app_w = 0;
  unsigned int app_h = 0;
  
  /* Geometry of the EmacsFrame */
  int frame_flags = 0;
  int frame_x = 0;
  int frame_y = 0;
  unsigned int frame_w = 0;
  unsigned int frame_h = 0;
  
  /* Hairily merged geometry */
  int x = 0;
  int y = 0;
  unsigned int w = 80;
  unsigned int h = 40;
  int flags = 0;

  char *geom = 0, *ew_geom = 0;
  Boolean iconic_p = False, ew_iconic_p = False;

  Widget wmshell = FRAME_X_SHELL_WIDGET (f);
  /* #### This may not be an ApplicationShell any more, with the 'popup
     frame property. */
  Widget app_shell = XtParent (wmshell);
  Widget ew = FRAME_X_TEXT_WIDGET (f);

/* set the position of the frame's root window now.  When the
   frame was created, the position was initialized to (0,0). */
  {
    struct window *win = XWINDOW (f->root_window);

    WINDOW_LEFT (win) = FRAME_LEFT_BORDER_END (f);
    WINDOW_TOP (win) = FRAME_TOP_BORDER_END (f);

    if (!NILP (f->minibuffer_window))
      {
	win = XWINDOW (f->minibuffer_window);
	WINDOW_LEFT (win) = FRAME_LEFT_BORDER_END (f);
      }
  }

#ifdef EXTERNAL_WIDGET
  /* If we're an external widget, then the size of the frame is predetermined
     (by the client) and is not our decision to make. */
  if (FRAME_X_EXTERNAL_WINDOW_P (f))
    return;
#endif

#if 0
  /* #### this junk has not been tested; therefore it's
     probably wrong.  Doesn't really matter at this point because
     currently all frames are either top-level or external widgets. */

  /* If we're not our own top-level window, then we shouldn't go messing around
     with top-level shells or "Emacs.geometry" or any such stuff.  Therefore,
     we do as follows to determine the size of the frame:

     1) If a value for the frame's "geometry" resource was specified, then
        use it.  (This specifies a size in characters.)
     2) Else, if the "width" and "height" resources were specified, then
        leave them alone.  (This is a value in pixels.  Sorry, we can't break
	Xt conventions here.)
     3) Else, assume a size of 64x12.  (This is somewhat arbitrary, but
        it's unlikely that a size of 80x40 is desirable because we're probably
	inside of a dialog box.)

     Set the widget's x, y, height, and width as determined.  Don't set the
     top-level container widget, because we don't necessarily know what it
     is. (Assume it is smart and pays attention to our values.)
  */

  if (!FRAME_X_TOP_LEVEL_FRAME_P (f))
    {
      XtVaGetValues (ew, XtNgeometry, &ew_geom, 0);
      if (ew_geom)
	frame_flags = XParseGeometry (ew_geom, &frame_x, &frame_y,
				       &frame_w, &frame_h);
      if (! (frame_flags & (WidthValue | HeightValue)))
	{
	  XtVaGetValues (ew, XtNwidth, &frame_w,
			 XtNheight, &frame_h, 0);
	  if (!frame_w && !frame_h)
	    {
	      frame_w = 64;
	      frame_h = 12;
	      frame_flags |= WidthValue | HeightValue;
	    }
	}
      if (frame_flags & (WidthValue | HeightValue))
	EmacsFrameSetCharSize (ew, frame_w, frame_h);
      if (frame_flags & (XValue | YValue))
	{
	  XtVaGetValues (ew, XtNwidth, &frame_w,
			 XtNheight, &frame_h, 0);
	  if (frame_flags & XNegative)
	    frame_x += frame_w;
	  if (frame_flags & YNegative)
	    frame_y += frame_h;
	  XtVaSetValues (ew, XtNx, frame_x, XtNy, frame_y, 0);
	}
      return;
    }
#endif

  /* OK, we're a top-level shell. */

  if (!XtIsWMShell (wmshell))
    abort ();

  /* If the EmacsFrame doesn't have a geometry but the shell does,
     treat that as the geometry of the frame.  (Is this bogus?
     I'm not sure.) */

  XtVaGetValues (ew, XtNgeometry, &ew_geom, 0);
  if (!ew_geom)
    {
      XtVaGetValues (wmshell, XtNgeometry, &geom, 0);
      if (geom)
	{
	  ew_geom = geom;
	  XtVaSetValues (ew, XtNgeometry, ew_geom, 0);
	}
    }

  /* If the Shell is iconic, then the EmacsFrame is iconic.  (Is
     this bogus? I'm not sure.) */
  XtVaGetValues (ew, XtNiconic, &ew_iconic_p, 0);
  if (!ew_iconic_p)
    {
      XtVaGetValues (wmshell, XtNiconic, &iconic_p, 0);
      if (iconic_p)
	{
	  ew_iconic_p = iconic_p;
	  XtVaSetValues (ew, XtNiconic, iconic_p, 0);
	}
    }
  
  XtVaGetValues (app_shell, XtNgeometry, &geom, 0);
  if (geom)
    app_flags = XParseGeometry (geom, &app_x, &app_y, &app_w, &app_h);

  if (ew_geom)
    frame_flags = XParseGeometry (ew_geom, &frame_x, &frame_y,
				   &frame_w, &frame_h);
  
  if (first_x_frame_p (f))
    {
      /* If this is the first frame created:
         ====================================

         - Use the ApplicationShell's size/position, if specified.
           (This is "Emacs.geometry", or the "-geometry" command line arg.)
         - Else use the EmacsFrame's size/position.
           (This is "*FRAME-NAME.geometry")

	 - If the AppShell is iconic, the frame should be iconic.

	 AppShell comes first so that -geometry always applies to the first
	 frame created, even if there is an "every frame" entry in the
	 resource database.
       */
      if (app_flags & (XValue | YValue))
	{
	  x = app_x; y = app_y;
	  flags |= (app_flags & (XValue | YValue | XNegative | YNegative));
	}
      else if (frame_flags & (XValue | YValue))
	{
	  x = frame_x; y = frame_y;
	  flags |= (frame_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w; h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
      else if (frame_flags & (WidthValue | HeightValue))
	{
	  w = frame_w; h = frame_h;
	  flags |= (frame_flags & (WidthValue | HeightValue));
	}

      /* If the AppShell is iconic, then the EmacsFrame is iconic. */
      if (!ew_iconic_p)
	{
	  XtVaGetValues (app_shell, XtNiconic, &iconic_p, 0);
	  if (iconic_p)
	    {
	      ew_iconic_p = iconic_p;
	      XtVaSetValues (ew, XtNiconic, iconic_p, 0);
	    }
	}
    }
  else
    {
      /* If this is not the first frame created:
         ========================================

         - use the EmacsFrame's size/position if specified
         - Otherwise, use the ApplicationShell's size, but not position.

         So that means that one can specify the position of the first frame
         with "Emacs.geometry" or `-geometry'; but can only specify the
	 position of subsequent frames with "*FRAME-NAME.geometry".

	 AppShell comes second so that -geometry does not apply to subsequent
	 frames when there is an "every frame" entry in the resource db,
	 but does apply to the first frame.
       */
      if (frame_flags & (XValue | YValue))
	{
	  x = frame_x; y = frame_y;
	  flags |= (frame_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (frame_flags & (WidthValue | HeightValue))
	{
	  w = frame_w; h = frame_h;
	  flags |= (frame_flags & (WidthValue | HeightValue));
	}
      else if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w;
	  h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
    }

  x_set_initial_frame_size (f, flags, x, y, w, h);
}

static void
x_get_layout_sizes (struct frame *f, Dimension *topbreadth)
{
  int i;

  /* compute height of all top-area widgets */
  for (i=0, *topbreadth = 0; i<FRAME_X_NUM_TOP_WIDGETS (f); i++)
    {
      Widget wid = FRAME_X_TOP_WIDGETS (f)[i];
      if (wid && XtIsManaged (wid))
	*topbreadth += wid->core.height + 2*wid->core.border_width;
    }
}

static void
x_layout_widgets (Widget w, XtPointer client_data, XtPointer call_data)
{
  struct frame *f = (struct frame *) client_data;
  EmacsManagerResizeStruct *emst = (EmacsManagerResizeStruct *) call_data;
  Dimension width = emst->width;
  Dimension height = emst->height;
  Widget text = FRAME_X_TEXT_WIDGET (f);
  Dimension textbord = text->core.border_width;
  Dimension topbreadth;
  Position text_x = 0, text_y = 0;
  int i;

  x_get_layout_sizes (f, &topbreadth);

  /* first the menubar and psheets ... */
  for (i=0; i<FRAME_X_NUM_TOP_WIDGETS (f); i++)
    {
      Widget wid = FRAME_X_TOP_WIDGETS (f)[i];
      if (wid && XtIsManaged (wid))
	{
	  Dimension bord = wid->core.border_width;
	  XtConfigureWidget (wid, 0, text_y,
			     width - 2*bord, wid->core.height,
			     bord);
	  text_y += wid->core.height + 2*bord;
	}
    }

#ifdef HAVE_SCROLLBARS
  {
    /* The scrollbar positioning is completely handled by redisplay.  We
       just need to know which sides they are supposed to go on. */
    unsigned char scrollbar_placement;
    XtVaGetValues (text, XtNscrollBarPlacement, &scrollbar_placement, 0);
    f->scrollbar_on_left = (scrollbar_placement == XtTOP_LEFT ||
                            scrollbar_placement == XtBOTTOM_LEFT);
    f->scrollbar_on_top  = (scrollbar_placement == XtTOP_LEFT ||
                            scrollbar_placement == XtTOP_RIGHT);
    f->scrollbar_y_offset = topbreadth + textbord;
  }
#endif

  /* finally the text area */
  XtConfigureWidget (text, text_x, text_y,
		     width - 2*textbord,
		     height - text_y - 2*textbord,
		     textbord);
}

static void
x_do_query_geometry (Widget w, XtPointer client_data, XtPointer call_data)
{
  struct frame *f = (struct frame *) client_data;
  EmacsManagerQueryGeometryStruct *emst =
    (EmacsManagerQueryGeometryStruct *) call_data;
  Widget text = FRAME_X_TEXT_WIDGET (f);
  Dimension textbord = text->core.border_width;
  Dimension topbreadth;
  XtWidgetGeometry req, repl;
  int mask = emst->request_mode & (CWWidth | CWHeight);

  x_get_layout_sizes (f, &topbreadth);

  /* strip away menubar from suggested size, and ask the text widget
     what size it wants to be */
  req.request_mode = mask;
  if (mask & CWWidth)
    req.width = emst->proposed_width - 2*textbord;
  if (mask & CWHeight)
    req.height = emst->proposed_height - topbreadth - 2*textbord;
  XtQueryGeometry (text, &req, &repl);

  /* Now add the menubar back again */
  emst->proposed_width = repl.width + 2*textbord;
  emst->proposed_height = repl.height + topbreadth + 2*textbord;
}

/* Creates the widgets for a frame.
   lisp_window_id is a Lisp description of an X window or Xt
   widget to parse.

   This function does not create or map the windows.  (That is
   done by x_popup_frame().)
 */
static void
x_create_widgets (struct frame *f, Lisp_Object lisp_window_id,
		  Lisp_Object parent)
{
  struct device *d = XDEVICE (f->device);
#ifdef EXTERNAL_WIDGET
  Window window_id = 0;
#endif
  CONST char *name;
  Arg av [25];
  int ac = 0;
  Widget text, container, shell;
  Widget parentwid = 0;
#ifdef HAVE_MENUBARS
  int menubar_visible;
  Widget menubar;
#endif

  if (STRINGP (f->name))
    GET_C_STRING_CTEXT_DATA_ALLOCA (f->name, name);
  else
    name = "emacs";
       
  /* The widget hierarchy is

	argv[0]			shell		pane		FRAME-NAME
	ApplicationShell	EmacsShell	EmacsManager	EmacsFrame

	(the type of the shell is ExternalShell if this frame is running
	in another client's window)

	However the EmacsShell widget has WM_CLASS of FRAME-NAME/Emacs.
	Normally such shells have name/class shellname/appclass, which in this
	case would be "shell/Emacs" instead of "frame-name/Emacs".  We could
	also get around this by naming the shell "frame-name", but that would
	be confusing because the text area (the EmacsFrame widget inferior of
	the shell) is also called that.  So we just set the WM_CLASS property.
   */

#ifndef EXTERNAL_WIDGET
  if (!NILP (lisp_window_id))
    error ("support for external widgets was not enabled at compile-time");
#else
  if (!NILP (lisp_window_id))
    {
      char *string;

      CHECK_STRING (lisp_window_id);
      string = (char *) (string_data (XSTRING (lisp_window_id)));
      if (string[0] == '0' && (string[1] == 'x' || string[1] == 'X'))
	sscanf (string+2, "%lxu", &window_id);
#if 0
      else if (string[0] == 'w')
	{
	  sscanf (string+1, "%x", &parent_widget);
	  if (parent_widget)
	    window_id = XtWindow (parent_widget);
	}
#endif
      else
	sscanf (string, "%lu", &window_id);
      if (!is_valid_window (window_id, d))
	error ("Invalid window %lu", (unsigned long) window_id);
      FRAME_X_EXTERNAL_WINDOW_P (f) = 1;
    } else
#endif /* EXTERNAL_WIDGET */
      FRAME_X_TOP_LEVEL_FRAME_P (f) = 1;

  ac = 0;
  XtSetArg (av[ac], XtNallowShellResize, True); ac++;
#ifdef LWLIB_USES_MOTIF
  /* Motif sucks beans.  Without this in here, it will delete the window
     out from under us when it receives a WM_DESTROY_WINDOW message
     from the WM. */
  XtSetArg (av[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
#endif

#ifdef EXTERNAL_WIDGET
  if (window_id)
    {
      XtSetArg (av[ac], XtNwindow, window_id); ac++;
    }
  else
#endif
    {
      XtSetArg (av[ac], XtNinput, True); ac++;
      XtSetArg (av[ac], (String) XtNminWidthCells, 10); ac++;
      XtSetArg (av[ac], (String) XtNminHeightCells, 1); ac++;
    }

  if (!NILP (parent))
    {
      parentwid = FRAME_X_SHELL_WIDGET (XFRAME (parent));
      XtSetArg (av[ac], XtNtransientFor, parentwid); ac++;
    }

  shell = XtCreatePopupShell ("shell",
			      (
#ifdef EXTERNAL_WIDGET
			       window_id ? externalShellWidgetClass :
#endif
			       parentwid ? transientEmacsShellWidgetClass :
			       topLevelEmacsShellWidgetClass
			       ),
			      parentwid ? parentwid :
			      DEVICE_XT_APP_SHELL (d),
			      av, ac);
  FRAME_X_SHELL_WIDGET (f) = shell;
  maybe_set_frame_title_format (shell);

  /* Create the manager widget */
  container = XtVaCreateWidget ("container",
				emacsManagerWidgetClass,
				shell, 0);
  FRAME_X_CONTAINER_WIDGET (f) = container;
  XtAddCallback (container, XtNresizeCallback, x_layout_widgets,
		 (XtPointer) f);
  XtAddCallback (container, XtNqueryGeometryCallback, x_do_query_geometry,
		 (XtPointer) f);

  /* Create the text area */
  ac = 0;
  XtSetArg (av[ac], XtNborderWidth, 0); ac++;	/* should this be settable? */
  XtSetArg (av[ac], (String) XtNemacsFrame, f); ac++;
  text = XtCreateWidget (name,
			 emacsFrameClass,
			 container, av, ac);
  FRAME_X_TEXT_WIDGET (f) = text;

#ifdef HAVE_MENUBARS  
  /* Create the initial menubar widget. */
  menubar_visible = x_initialize_frame_menubar (f);
  FRAME_X_TOP_WIDGETS (f)[0] = menubar = FRAME_X_MENUBAR_WIDGET (f);
  FRAME_X_NUM_TOP_WIDGETS (f) = 1;
  
  if (menubar_visible)
    XtManageChild (menubar);
#endif
  XtManageChild (text);
  XtManageChild (container);
}

/* We used to call XtPopup() in x_popup_frame, but that doesn't give
   you control over whether the widget is initially mapped or not
   because XtPopup() makes an unconditional call to XMapRaised().
   Boy, those Xt designers were clever.

   When we first removed it we only kept the XtRealizeWidget call in
   XtPopup.  For everything except HP's that was enough.  For HP's,
   though, the failure to call the popup callbacks resulted in XEmacs
   not accepting any input.  Bizarre but true.  Stupid but true.

   So, in case there are any other gotches floating out there along
   the same lines I've duplicated the majority of XtPopup here.  It
   assumes no grabs and that the widget is not already popped up, both
   valid assumptions for the one place this is called from. */
static void
xemacs_XtPopup (Widget widget)
{
  ShellWidget shell_widget = (ShellWidget) widget;
  XtGrabKind call_data = XtGrabNone;

  XtCallCallbacks (widget, XtNpopupCallback, (XtPointer)&call_data);

  shell_widget->shell.popped_up = TRUE;
  shell_widget->shell.grab_kind = XtGrabNone;
  shell_widget->shell.spring_loaded = False;

  if (shell_widget->shell.create_popup_child_proc != NULL)
    (*(shell_widget->shell.create_popup_child_proc))(widget);

  /* The XtVaSetValues below are not in XtPopup menu.  We just want to
     make absolutely sure... */
  XtVaSetValues (widget, XtNmappedWhenManaged, False, NULL);
  XtRealizeWidget (widget);
  XtVaSetValues (widget, XtNmappedWhenManaged, True, NULL);
}

#ifdef HAVE_CDE
/* Does this have to be non-automatic? */
/* hack frame to respond to dnd messages */
static XtCallbackRec dnd_transfer_cb_rec[2];
#endif

/* create the windows for the specified frame and display them.
   Note that the widgets have already been created, and any
   necessary geometry calculations have already been done. */
static void
x_popup_frame (struct frame *f)
{
  Widget shell_widget = FRAME_X_SHELL_WIDGET (f);
  Widget frame_widget = FRAME_X_TEXT_WIDGET (f);
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  /* Before mapping the window, make sure that the WMShell's notion of
     whether it should be iconified is synchronized with the EmacsFrame's
     notion.
     */
  if (FRAME_X_TOP_LEVEL_FRAME_P (f))
    x_wm_set_shell_iconic_p (shell_widget,
			     ((EmacsFrame) frame_widget)
			     ->emacs_frame.iconic);

  xemacs_XtPopup (shell_widget);

  if (!((EmacsFrame) frame_widget)->emacs_frame.initially_unmapped)
    XtMapWidget (shell_widget);
  else
    {
      /* We may have set f->visible to 1 in x_init_frame(), so undo
	 that now. */
      FRAME_X_TOTALLY_VISIBLE_P (f) = 0;
      f->visible = 0;
    }

#ifdef EXTERNAL_WIDGET
  if (FRAME_X_EXTERNAL_WINDOW_P (f))
    ExternalShellReady (shell_widget, XtWindow (frame_widget), KeyPressMask);
  else
#endif
    if (FRAME_X_TOP_LEVEL_FRAME_P (f))
      {
	/* tell the window manager about us. */
	x_wm_store_class_hints (shell_widget, XtName (frame_widget));
	x_wm_maybe_store_wm_command (f);
	x_wm_hack_wm_protocols (shell_widget);
      }

#ifdef HACK_EDITRES
  /* Allow XEmacs to respond to EditRes requests.  See the O'Reilly Xt */
  /* Instrinsics Programming Manual, Motif Edition, Aug 1993, Sect 14.14, */
  /* pp. 483-493. */
  XtAddEventHandler (shell_widget,           /* the shell widget in question */
		     (EventMask) NoEventMask,/* OR with existing mask */
		     True,                   /* called on non-maskable events? */
		     _XEditResCheckMessages, /* the handler */
		     NULL);
#endif

#ifdef HAVE_CDE
  {
    dnd_transfer_cb_rec[0].callback = x_cde_transfer_callback;
    dnd_transfer_cb_rec[0].closure = (XtPointer) f;
    dnd_transfer_cb_rec[1].callback = NULL;
    dnd_transfer_cb_rec[1].closure = NULL;

    DtDndVaDropRegister (FRAME_X_TEXT_WIDGET (f),
 			 DtDND_FILENAME_TRANSFER | DtDND_BUFFER_TRANSFER,
 			 XmDROP_COPY, dnd_transfer_cb_rec,
 			 DtNtextIsBuffer, True,
			 DtNpreserveRegistration, False,
			 NULL);
  }
#endif

#ifdef HAVE_OFFIX_DND
  {
    DndInitialize (FRAME_X_SHELL_WIDGET (f));
    DndRegisterDropWidget (FRAME_X_TEXT_WIDGET (f),
			   x_offix_drop_event_handler, 
			   (XtPointer) f);

  }
#endif

  /* Do a stupid property change to force the server to generate a
     propertyNotify event so that the event_stream server timestamp will
     be initialized to something relevant to the time we created the window.
     */
  XChangeProperty (XtDisplay (frame_widget), XtWindow (frame_widget),
		   DEVICE_XATOM_WM_PROTOCOLS (d), XA_ATOM, 32, PropModeAppend,
		   (unsigned char*) NULL, 0);
  
  x_send_synthetic_mouse_event (f);
}

static void
allocate_x_frame_struct (struct frame *f)
{
  /* zero out all slots. */
  f->frame_data = malloc_type_and_zero (struct x_frame);

  /* yeah, except the lisp ones */
  FRAME_X_ICON_PIXMAP (f) = Qnil;
  FRAME_X_ICON_PIXMAP_MASK (f) = Qnil;
#ifdef ENERGIZE
  FRAME_X_CURRENT_PSHEET_BUFFER (f) = Qnil;
  FRAME_X_DESIRED_PSHEET_BUFFER (f) = Qnil;
#endif
}


/************************************************************************/
/*				Lisp functions				*/
/************************************************************************/

static void
x_init_frame_1 (struct frame *f, Lisp_Object props)
{
  /* This function can GC */
  Lisp_Object device = FRAME_DEVICE (f);
  struct device *d = XDEVICE (device);
  Lisp_Object lisp_window_id;
  Lisp_Object popup;

  lisp_window_id = Fplist_get (props, Qwindow_id, Qnil);
  popup = Fplist_get (props, Qpopup, Qnil);
  if (!NILP (popup))
    {
      if (EQ (popup, Qt))
	popup = Fselected_frame (device);
      CHECK_LIVE_FRAME (popup);
      if (!EQ (device, FRAME_DEVICE (XFRAME (popup))))
	signal_simple_error_2 ("Parent must be on same device as frame",
			       device, popup);
    }

  if (NILP (DEVICE_SELECTED_FRAME (d)))
    {
      /* This means that this is the first frame on the device.
	 So short-ciruit the delay in processing the initial MapNotify
	 event so that output on the first frame shows up right
	 away... */
      f->visible = 1;
    }

  allocate_x_frame_struct (f);
  x_create_widgets (f, lisp_window_id, popup);
}

static void
x_init_frame_2 (struct frame *f, Lisp_Object props)
{
  /* Set up the values of the widget/frame.  A case could be made for putting
     this inside of the widget's initialize method. */

  update_frame_face_values (f);
  x_initialize_frame_size (f);
  update_frame_title (f);
}

static void
x_init_frame_3 (struct frame *f)
{
  /* Pop up the frame. */

  x_popup_frame (f);
}

static void
x_mark_frame (struct frame *f, void (*markobj) (Lisp_Object))
{
  ((markobj) (FRAME_X_ICON_PIXMAP (f)));
  ((markobj) (FRAME_X_ICON_PIXMAP_MASK (f)));
#ifdef ENERGIZE
  ((markobj) (FRAME_X_CURRENT_PSHEET_BUFFER (f)));
  ((markobj) (FRAME_X_DESIRED_PSHEET_BUFFER (f)));
#endif
}

static void
x_set_frame_icon (struct frame *f)
{
  Pixmap x_pixmap, x_mask;

  if (IMAGE_INSTANCEP (f->icon)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (f->icon)))
    {
      x_pixmap = XIMAGE_INSTANCE_X_PIXMAP (f->icon);
      x_mask = XIMAGE_INSTANCE_X_MASK (f->icon);
    }
  else
    {
      x_pixmap = 0;
      x_mask = 0;
    }

  /* Store the X data into the widget. */
  {
    Arg av [10];
    int ac = 0;
    XtSetArg (av [ac], XtNiconPixmap, x_pixmap); ac++;
    XtSetArg (av [ac], XtNiconMask, x_mask); ac++;
    XtSetValues (FRAME_X_SHELL_WIDGET (f), av, ac);
  }
}

static void
x_set_frame_pointer (struct frame *f)
{
  XDefineCursor (XtDisplay (FRAME_X_TEXT_WIDGET (f)),
		 XtWindow (FRAME_X_TEXT_WIDGET (f)),
		 XIMAGE_INSTANCE_X_CURSOR (f->pointer));
  XSync (XtDisplay (FRAME_X_TEXT_WIDGET (f)), 0);
}

static Lisp_Object
x_get_frame_parent (struct frame *f)
{
  Widget parentwid = 0;
  Arg av[1];

  XtSetArg (av[0], XtNtransientFor, &parentwid);
  XtGetValues (FRAME_X_SHELL_WIDGET (f), av, 1);
  /* find the frame whose wid is parentwid */
  if (parentwid)
    {
      Lisp_Object frmcons;
      DEVICE_FRAME_LOOP (frmcons, XDEVICE (FRAME_DEVICE (f)))
	{
	  Lisp_Object frame = XCAR (frmcons);
	  if (FRAME_X_SHELL_WIDGET (XFRAME (frame)) == parentwid)
	    return frame;
	}
    }
  return Qnil;
}

DEFUN ("x-window-id", Fx_window_id, Sx_window_id, 0, 1, 0 /*
Get the ID of the X11 window.
This gives us a chance to manipulate the Emacs window from within a
different program.  Since the ID is an unsigned long, we return it as
a string.
*/ )
  (frame)
  Lisp_Object frame;
{
  char str[255];
  struct frame *f = decode_x_frame (frame);

  sprintf (str, "%lu", XtWindow (FRAME_X_TEXT_WIDGET (f)));
  return build_string (str);
}


/************************************************************************/
/*			manipulating the X window			*/
/************************************************************************/

static void
x_set_frame_position (struct frame *f, int xoff, int yoff)
{
  Widget w = FRAME_X_SHELL_WIDGET (f);
  Display *dpy = XtDisplay (w);
  Dimension frame_w = DisplayWidth (dpy, DefaultScreen (dpy));
  Dimension frame_h = DisplayHeight (dpy, DefaultScreen (dpy));
  Dimension shell_w, shell_h, shell_bord;
  int win_gravity;

  XtVaGetValues (w,
		 XtNwidth, &shell_w,
		 XtNheight, &shell_h,
		 XtNborderWidth, &shell_bord,
		 0);

  win_gravity =
    xoff >= 0 && yoff >= 0 ? NorthWestGravity :
    xoff >= 0 ? SouthWestGravity :
    yoff >= 0 ? NorthEastGravity :
    SouthEastGravity;
  if (xoff < 0)
    xoff += frame_w - shell_w - 2*shell_bord;
  if (yoff < 0)
    yoff += frame_h - shell_h - 2*shell_bord;

  /* Update the hints so that, if this window is currently iconified, it will
     come back at the right place.  We can't look at s->visible to determine
     whether it is iconified because it might not be up-to-date yet (the queue
     might not be processed). */
  XtVaSetValues (w,
		 XtNwinGravity, win_gravity,
		 XtNx, xoff,
		 XtNy, yoff,
		 0);
  /* Sometimes you will find that

     (set-frame-position (selected-frame) -50 -50)

     doesn't put the frame where you expect it to:
     i.e. it's closer to the lower-right corner than
     it should be, and it appears that the size of
     the WM decorations was not taken into account.
     This is *not* a problem with this function.
     Both mwm and twm have bugs in handling this
     situation. (mwm ignores the window gravity
     and always assumes NorthWest, except the first
     time you map the window; twm gets things almost
     right, but forgets to account for the border
     width of the top-level window.) This function
     does what it's supposed to according to the ICCCM,
     and I'm not about to hack around window-manager
     bugs. */

#if 0
  /* This is not necessary under either mwm or twm */
  x_wm_mark_shell_position_user_specified (w);
#endif
}

/* Call this to change the size of frame S's x-window. */

static void
x_set_frame_size (struct frame *f, int cols, int rows)
{
  EmacsFrameSetCharSize (FRAME_X_TEXT_WIDGET (f), cols, rows);
#if 0
    /* this is not correct.  x_set_frame_size() is called from
       Fset_frame_size(), which may or may not have been called
       by the user (e.g. update_EmacsFrame() calls it when the font
       changes).  For now, don't bother with getting this right. */
  x_wm_mark_shell_size_user_specified (FRAME_X_SHELL_WIDGET (f));
#endif
}

static void
x_set_mouse_position (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);

  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));
  XWarpPointer (display, None, XtWindow (FRAME_X_TEXT_WIDGET (f)),
                0, 0, 0, 0, w->pixel_left + x, w->pixel_top + y);
}

static int
x_get_mouse_position (struct device *d, Lisp_Object *frame, int *x, int *y)
{
  Display *display = DEVICE_X_DISPLAY (d);
  Window child_window;
  Window root_window;
  Window win;
  int root_x, root_y;
  int win_x, win_y;
  unsigned int keys_and_buttons;
  struct frame *f;

  if (XQueryPointer (display, RootWindow (display, DefaultScreen (display)),
		     &root_window, &child_window, &root_x, &root_y,
		     &win_x, &win_y, &keys_and_buttons) == False)
    return 0;

  if (child_window == None)
    return 0;	/* not over any window. */

  while (1)
    {
      win = child_window;
      if (XTranslateCoordinates (display, root_window, win, root_x, root_y,
				 &win_x, &win_y, &child_window) == False)
	/* Huh? */
	return 0;

      if (child_window == None)
	break;
    }

  /* At this point, win is the innermost window containing the pointer
     and win_x and win_y are the coordinates of that window. */
  f = x_any_window_to_frame (d, win);
  if (!f)
    return 0;
  XSETFRAME (*frame, f);

  if (XTranslateCoordinates (display, win,
			     XtWindow (FRAME_X_TEXT_WIDGET (f)),
			     win_x, win_y, x, y, &child_window) == False)
    /* Huh? */
    return 0;

  return 1;
}

static void
x_cant_notify_wm_error (void)
{
  error ("Can't notify window manager of iconification.");
}

/* Raise frame F.  */
static void
x_raise_frame_1 (struct frame *f, int force)
{
  Widget bottom_dialog;
  Window emacs_window;
  XWindowChanges xwc;
  unsigned int flags;
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));

  if (f->visible || force)
    {
      emacs_window = XtWindow (FRAME_X_SHELL_WIDGET (f));
      /* first raises all the dialog boxes, then put emacs just below the 
       * bottom most dialog box */
      bottom_dialog = lw_raise_all_pop_up_widgets ();
      if (bottom_dialog && XtWindow (bottom_dialog))
	{
	  xwc.sibling = XtWindow (bottom_dialog);
	  xwc.stack_mode = Below;
	  flags = CWSibling | CWStackMode;
	}
      else
	{
	  xwc.stack_mode = Above;
	  flags = CWStackMode;
	}

      if (!XReconfigureWMWindow (display, emacs_window,
				 DefaultScreen (display),
				 flags, &xwc))
	x_cant_notify_wm_error ();
    }
}

static void
x_raise_frame (struct frame *f)
{
  x_raise_frame_1 (f, 1);
}

/* Lower frame F.  */
static void
x_lower_frame (struct frame *f)
{
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));
  XWindowChanges xwc;
  unsigned int flags;
  
  if (f->visible)
    {
      xwc.stack_mode = Below;
      flags = CWStackMode;
      if (!XReconfigureWMWindow (display, XtWindow (FRAME_X_SHELL_WIDGET (f)),
				 DefaultScreen (display), flags, &xwc))
	x_cant_notify_wm_error ();
    }
}

/* Change from withdrawn state to mapped state. */
static void
x_make_frame_visible (struct frame *f)
{
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));

  if (!f->visible)
    XMapRaised (display, XtWindow (FRAME_X_SHELL_WIDGET (f)));
  else
    x_raise_frame_1 (f, 0);
}

/* Change from mapped state to withdrawn state. */
static void
x_make_frame_invisible (struct frame *f)
{
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));

  if (!f->visible)
    return;

  if (!XWithdrawWindow (display,
			XtWindow (FRAME_X_SHELL_WIDGET (f)),
			DefaultScreen (display)))
    x_cant_notify_wm_error ();
}

static int
x_frame_visible_p (struct frame *f)
{
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));
  XWindowAttributes xwa;
  int result;

  if (!XGetWindowAttributes (display,
			     XtWindow (FRAME_X_SHELL_WIDGET (f)),
			     &xwa))
    result = 0;
  else
    result = xwa.map_state == IsViewable;

  f->visible = result;
  return result;
}

static int
x_frame_totally_visible_p (struct frame *f)
{
  return FRAME_X_TOTALLY_VISIBLE_P (f);
}

/* Change window state from mapped to iconified. */
static void
x_iconify_frame (struct frame *f)
{
  Display *display = DEVICE_X_DISPLAY (XDEVICE (f->device));

  if (!XIconifyWindow (display,
		       XtWindow (FRAME_X_SHELL_WIDGET (f)),
		       DefaultScreen (display)))
    x_cant_notify_wm_error ();

  f->iconified = 1;
}

/* Sets the X focus to frame f. */
static void
x_focus_on_frame (struct frame *f)
{
  XWindowAttributes xwa;
  Widget shell_widget;

  assert (FRAME_X_P (f));

  shell_widget = FRAME_X_SHELL_WIDGET (f);
  if (!XtWindow (shell_widget))
    return;

#ifdef EXTERNAL_WIDGET
  if (FRAME_X_EXTERNAL_WINDOW_P (f))
    ExternalShellSetFocus (shell_widget);
#endif /* EXTERNAL_WIDGET */

  /* Do the ICCCM focus change if the window is still visible.
     The s->visible flag might not be up-to-date, because we might
     not have processed magic events recently.  So make a server
     round-trip to find out whether it's really mapped right now.
     We grab the server to do this, because that's the only way to
     eliminate the race condition.
   */
  XGrabServer (XtDisplay (shell_widget));
  if (XGetWindowAttributes (XtDisplay (shell_widget),
			    XtWindow (shell_widget),
			    &xwa))
    f->visible = xwa.map_state == IsViewable;
      
  if (f->visible)
    {
      Window focus;
      int revert_to;
      XGetInputFocus (XtDisplay (shell_widget), &focus, &revert_to);
      /* Don't explicitly set the focus on this window unless the focus
	 was on some other window (not PointerRoot).  Note that, even when
	 running a point-to-type window manager like *twm, there is always
	 a focus window; the window manager maintains that based on the
	 mouse position.  If you set the "NoTitleFocus" option in these
	 window managers, then the server itself maintains the focus via
	 PointerRoot, and changing that to focus on the window would make
	 the window grab the focus.  Very bad.
	 */
      if (focus != PointerRoot)
	{
	  XSetInputFocus (XtDisplay (shell_widget),
			  XtWindow (shell_widget),
			  RevertToParent,
			  DEVICE_X_MOUSE_TIMESTAMP
			  (XDEVICE (FRAME_DEVICE (f))));
	  XFlush (XtDisplay (shell_widget));
	}
    }
  XUngrabServer (XtDisplay (shell_widget));
  XFlush (XtDisplay (shell_widget)); /* hey, I'd like to DEBUG this... */
}

/* Destroy the X window of frame S.  */
static void
x_delete_frame (struct frame *f)
{
  Widget w = FRAME_X_SHELL_WIDGET (f);
  Lisp_Object popup, frame;

  if (FRAME_X_TOP_LEVEL_FRAME_P (f))
    x_wm_maybe_move_wm_command (f);

  /* Frames with the popup property are using other frames as their
     widget parent.  Deleting them are their parent has already been
     deleted can lead to crashes. */
  XSETFRAME (frame, f);
  popup = Fframe_property (frame, Qpopup, Qnil);
  if (!NILP (popup))
    {
      /* If popup isn't nil then it means the frame has that property
         and the value is supposed to be the parent frame.  The FRAMEP
         check is to safeguard against it not being a frame. */
      if (!FRAMEP (popup) || !FRAME_LIVE_P (XFRAME (popup)))
	popup = Qt;
      else
	popup = Qnil;
    }

#ifdef EXTERNAL_WIDGET
  {
    Display *dpy = XtDisplay (w);
    expect_x_error (dpy);
    /* for obscure reasons having (I think) to do with the internal
       window-to-widget hierarchy maintained by Xt, we have to call
       XtUnrealizeWidget() here.  Xt can really suck. */
    if (f->being_deleted)
      XtUnrealizeWidget (w);
    if (NILP (popup))
      XtDestroyWidget (w);
    x_error_occurred_p (dpy);
  }
#else
  if (NILP (popup))
    XtDestroyWidget (w);
#endif /* EXTERNAL_WIDGET */

  if (FRAME_X_GEOM_FREE_ME_PLEASE (f))
    xfree (FRAME_X_GEOM_FREE_ME_PLEASE (f));
  xfree (f->frame_data);
  f->frame_data = 0;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_frame_x (void)
{
  defsymbol (&Qwindow_id, "window-id");
  defsymbol (&Qpopup, "popup");
  defsymbol (&Qx_resource_name, "x-resource-name");

  defsubr (&Sx_window_id);
}

void
console_type_create_frame_x (void)
{
  /* frame methods */
  CONSOLE_HAS_METHOD (x, init_frame_1);
  CONSOLE_HAS_METHOD (x, init_frame_2);
  CONSOLE_HAS_METHOD (x, init_frame_3);
  CONSOLE_HAS_METHOD (x, mark_frame);
  CONSOLE_HAS_METHOD (x, focus_on_frame);
  CONSOLE_HAS_METHOD (x, delete_frame);
  CONSOLE_HAS_METHOD (x, get_mouse_position);
  CONSOLE_HAS_METHOD (x, set_mouse_position);
  CONSOLE_HAS_METHOD (x, raise_frame);
  CONSOLE_HAS_METHOD (x, lower_frame);
  CONSOLE_HAS_METHOD (x, make_frame_visible);
  CONSOLE_HAS_METHOD (x, make_frame_invisible);
  CONSOLE_HAS_METHOD (x, iconify_frame);
  CONSOLE_HAS_METHOD (x, set_frame_size);
  CONSOLE_HAS_METHOD (x, set_frame_position);
  CONSOLE_HAS_METHOD (x, frame_property);
  CONSOLE_HAS_METHOD (x, internal_frame_property_p);
  CONSOLE_HAS_METHOD (x, frame_properties);
  CONSOLE_HAS_METHOD (x, set_frame_properties);
  CONSOLE_HAS_METHOD (x, set_title_from_bufbyte);
  CONSOLE_HAS_METHOD (x, set_icon_name_from_bufbyte);
  CONSOLE_HAS_METHOD (x, frame_visible_p);
  CONSOLE_HAS_METHOD (x, frame_totally_visible_p);
  CONSOLE_HAS_METHOD (x, frame_iconified_p);
  CONSOLE_HAS_METHOD (x, set_frame_pointer);
  CONSOLE_HAS_METHOD (x, set_frame_icon);
  CONSOLE_HAS_METHOD (x, get_frame_parent);
}

void
vars_of_frame_x (void)
{
#ifdef EXTERNAL_WIDGET
  Fprovide (intern ("external-widget"));
#endif

  /* this call uses only safe functions from emacs.c */
  init_x_prop_symbols ();

  DEFVAR_LISP ("default-x-frame-plist", &Vdefault_x_frame_plist /*
Plist of default frame-creation properties for X frames.
These override what is specified in the resource database and in
`default-frame-plist', but are overridden by the arguments to the
particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

  window-id			The X window ID corresponding to the
				frame.  May be set only at startup, and
				only if external widget support was
				compiled in; doing so causes the frame
				to be created as an \"external widget\"
				in another program that uses an existing
				window in the program rather than creating
				a new one.
  initially-unmapped		If non-nil, the frame will not be visible
				when it is created.  In this case, you
				need to call `make-frame-visible' to make
				the frame appear.
  popup				If non-nil, it should be a frame, and this
				frame will be created as a \"popup\" frame
				whose parent is the given frame.  This
				will make the window manager treat the
				frame as a dialog box, which may entail
				doing different things (e.g. not asking
				for positioning, and not iconifying
				separate from its parent).
  inter-line-space		Not currently implemented.
  toolbar-shadow-thickness	Thickness of toolbar shadows.
  background-toolbar-color	Color of toolbar background.
  bottom-toolbar-shadow-color	Color of bottom shadows on toolbars.
				(*Not* specific to the bottom-toolbar.)
  top-toolbar-shadow-color	Color of top shadows on toolbars.
				(*Not* specifier to the top-toolbar.)
  internal-border-width		Width of internal border around text area.
  border-width			Width of external border around text area.
  top				Y position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  left				X position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  border-color			Color of external border around text area.
  cursor-color			Color of text cursor.

See also `default-frame-plist', which specifies properties which apply
to all frames, not just X frames.
*/ );
  Vdefault_x_frame_plist = Qnil;

  x_console_methods->device_specific_frame_props = &Vdefault_x_frame_plist;
}
