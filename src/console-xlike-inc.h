/* Definitions for use in *-xlike-inc.c files.
   Copyright (C) 2010 Ben Wing.

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

#ifndef INCLUDED_console_xlike_inc_h_
#define INCLUDED_console_xlike_inc_h_

/* X and GTK are quite similar, since GTK based its structure on Xlib.
   However, there are many small differences.  Creating two separate
   versions of the code, as Bill Perry did originally, is easier to code
   but hard to maintain and leads to bit rot.  On the other hand, trying to
   abstract out into device methods, as is done generally for different
   window systems, gets very hairy very quickly because the differences are
   on the level of individual library functions, constants and types, of
   which there are large number.  Abstracting them into device methods
   would lead to a large number of very small functions and very
   hard-to-read code.

   Instead, we handle the situation by having only one copy, placed in a
   file called *-xlike-inc.c (e.g. redisplay-xlike-inc.c) and
   conditionalizing using ifdefs.  Because we can compile with both X and
   GTK at once, we include this file inside of the appropriate
   device-specific file (e.g. redisplay-gtk.c or redisplay-x.c).  The `inc'
   in *-xlike-inc.c indicates that this is a file meant to be included in
   another file, despite the fact that it is a .c file.

   To signal which variety of "xlike" we are compiling for, either
   THIS_IS_X or THIS_IS_GTK needs to be defined, prior to including the
   *-xlike-inc.c file. */


/* About the representation of color below:

   X has two ways of representing a color: (a) as an unsigned long
   representing a color pixel value, i.e. the actual value stored in memory
   or a file at a particular pixel location to indicate that the pixel
   takes on a specific color; and (b) an XColor structure, which
   encapsulates both the RGB components of a color and the associated color
   pixel value.

   We call the former type XLIKE_PIXEL and the latter XLIKE_COLOR, along
   with routines to convert from one to the other.  Differences between the
   two in specific functions and structures should be abstracted using the
   XLIKE_FOO() routines, e.g. XLIKE_SET_GC_COLOR(). */

#if defined (THIS_IS_X) && defined (THIS_IS_GTK)
#error "Exactly one of THIS_IS_X and THIS_IS_GTK may be defined."
#endif

#if !defined (THIS_IS_X) && !defined (THIS_IS_GTK)
#error "Either THIS_IS_X or THIS_IS_GTK must be defined."
#endif

#ifdef THIS_IS_X
#  include "console-x-impl.h"
#  ifdef NEED_GCCACHE_H
#    include "xgccache.h"
#  endif
#  ifdef NEED_GLYPHS_H
#    include "glyphs-x.h"
#  endif
#  ifdef NEED_OBJECTS_IMPL_H
#    include "objects-x-impl.h"
#  endif
#else /* THIS_IS_GTK */
#  include "console-gtk-impl.h"
#  ifdef NEED_GCCACHE_H
#    include "gccache-gtk.h"
#  endif
#  ifdef NEED_GLYPHS_H
#    include "glyphs-gtk.h"
#  endif
#  ifdef NEED_OBJECTS_IMPL_H
#    include "objects-gtk-impl.h"
#  endif
#endif /* THIS_IS_GTK */

/***************************************************************************/
/*                           Common definitions                            */
/***************************************************************************/

#define XLIKE_PASTE_1(a,b) a##_##b
#define XLIKE_PASTE(a,b) XLIKE_PASTE_1(a,b)
#define XFUN(name) XLIKE_PASTE (XLIKE_NAME, name)

#define XLIKE_CONSOLE_HAS_METHOD_1(xlike, name) CONSOLE_HAS_METHOD (xlike, name)
#define XLIKE_CONSOLE_HAS_METHOD(name) \
  XLIKE_CONSOLE_HAS_METHOD_1 (XLIKE_NAME, name)

#define XCOLOR_INSTANCE_XLIKE_COLOR(x) \
  COLOR_INSTANCE_XLIKE_COLOR (XCOLOR_INSTANCE (x))

#ifdef THIS_IS_X

/***************************************************************************/
/*               Definitions implementing X flavor of XLIKE                */
/***************************************************************************/

#define XLIKE_NAME x
#define USED_IF_X(var) var
#define USED_IF_GTK(var) UNUSED (var)

/* types */
typedef Display * XLIKE_DISPLAY;
typedef Window XLIKE_WINDOW;
typedef GC XLIKE_GC;
typedef XRectangle XLIKE_RECTANGLE;
typedef XGCValues XLIKE_GCVALUES;
typedef XColor XLIKE_COLOR;
typedef unsigned long XLIKE_PIXEL;

/* constants */
#define XLIKE_NONE None
#define XLIKE_FALSE False

#define XLIKE_GC_BACKGROUND GCBackground
#define XLIKE_GC_CLIP_MASK GCClipMask
#define XLIKE_GC_CLIP_X_ORIGIN GCClipXOrigin
#define XLIKE_GC_CLIP_Y_ORIGIN GCClipYOrigin
#define XLIKE_GC_EXPOSURES GCGraphicsExposures
#define XLIKE_GC_FILL GCFillStyle
#define XLIKE_GC_FONT GCFont
#define XLIKE_GC_FOREGROUND GCForeground
#define XLIKE_GC_FUNCTION GCFunction
#define XLIKE_GC_LINE_WIDTH GCLineWidth
#define XLIKE_GC_STIPPLE GCStipple
#define XLIKE_GC_TILE GCTile

#define XLIKE_GX_COPY GXcopy
#define XLIKE_GX_XOR GXxor

#define XLIKE_FILL_STIPPLED FillStippled
#define XLIKE_FILL_OPAQUE_STIPPLED FillOpaqueStippled
#define XLIKE_FILL_TILED FillTiled
#define XLIKE_FILL_SOLID FillSolid

/* functions */
#define GET_XLIKE_DISPLAY(d) DEVICE_X_DISPLAY (d)
#define GET_XLIKE_X_DISPLAY(d) DEVICE_X_DISPLAY (d)
#define GET_XLIKE_WINDOW(w) XtWindow (FRAME_X_TEXT_WIDGET (f))
#define XLIKE_FILL_RECTANGLE(dpy, x_win, gc, x, y, width, height) \
  XFillRectangle (dpy, x_win, gc, x, y, width, height)
#define XLIKE_DRAW_RECTANGLE(dpy, x_win, gc, x, y, width, height) \
  XDrawRectangle (dpy, x_win, gc, x, y, width, height)
#define XLIKE_DRAW_LINE(dpy, x_win, gc, x1, y1, x2, y2) \
  XDrawLine (dpy, x_win, gc, x1, y1, x2, y2)
#define XLIKE_TEXT_WIDTH(fi, ptr, len) XTextWidth (fi, (char *) ptr, len)
#define XLIKE_TEXT_WIDTH_WIDE(fi, ptr, len) XTextWidth16 (fi, (XChar2b *) ptr, len)

#define XLIKE_DISPLAY_LINE_HEIGHT(dl) DISPLAY_LINE_HEIGHT (dl)
#define XLIKE_DISPLAY_LINE_YPOS(dl) DISPLAY_LINE_YPOS (dl)
#define XLIKE_DISPLAY_LINE_TOP_CLIP(dl) ((dl)->top_clip)
#define XLIKE_SET_CLIP_RECTANGLE(dpy, gc, xorig, yorig, prect)		\
  /* #### why not Unsorted? */						\
  XSetClipRectangles (dpy, gc, xorig, yorig, prect, 1, YXBanded)
#define XLIKE_CLEAR_CLIP_MASK(dpy, gc)		\
do						\
  {						\
    XSetClipMask (dpy, gc, None);		\
    XSetClipOrigin (dpy, gc, 0, 0);		\
  }						\
while (0)
#define XLIKE_FLUSH(dpy) XSync (dpy, False)
#define XLIKE_CLEAR_AREA(dpy, win, x, y, width, height) \
  XClearArea (dpy, win, x, y, width, height, False)
 
#define IMAGE_INSTANCE_XLIKE_MASK IMAGE_INSTANCE_X_MASK
#define XIMAGE_INSTANCE_XLIKE_PIXMAP XIMAGE_INSTANCE_X_PIXMAP
#define COLOR_INSTANCE_XLIKE_COLOR COLOR_INSTANCE_X_COLOR
#define FONT_INSTANCE_XLIKE_FONT FONT_INSTANCE_X_FONT
#define DEVICE_XLIKE_GC_CACHE DEVICE_X_GC_CACHE
#define DEVICE_XLIKE_GRAY_PIXMAP DEVICE_X_GRAY_PIXMAP
#define XLIKE_SET_GC_FILL(gc, style) ((gc).fill_style = (style))
#define XLIKE_COLOR_TO_PIXEL(c) ((c).pixel)
#define XLIKE_SET_GC_COLOR(lval, rval) ((lval) = (rval).pixel)
#define XLIKE_SET_GC_PIXEL(lval, rval) ((lval) = (rval))
#define XLIKE_FONT_NUM(val) ((val)->fid)

/************ End X flavor of XLIKE **********/


#else /* THIS_IS_GTK */

/***************************************************************************/
/*              Definitions implementing GTK flavor of XLIKE               */
/***************************************************************************/

#define XLIKE_NAME gtk
#define USED_IF_X(var) UNUSED (var)
#define USED_IF_GTK(var) var

/*types */
typedef void * XLIKE_DISPLAY;
typedef GdkWindow * XLIKE_WINDOW;
typedef GdkGC * XLIKE_GC;
typedef GdkRectangle XLIKE_RECTANGLE;
typedef GdkGCValues XLIKE_GCVALUES;
typedef GdkColor XLIKE_COLOR;
typedef gulong   XLIKE_PIXEL;

/* constants */
#define XLIKE_NONE 0
#define XLIKE_FALSE FALSE

#define XLIKE_GC_BACKGROUND GDK_GC_BACKGROUND
#define XLIKE_GC_CLIP_MASK GDK_GC_CLIP_MASK
#define XLIKE_GC_CLIP_X_ORIGIN GDK_GC_CLIP_X_ORIGIN
#define XLIKE_GC_CLIP_Y_ORIGIN GDK_GC_CLIP_Y_ORIGIN
#define XLIKE_GC_EXPOSURES GDK_GC_EXPOSURES
#define XLIKE_GC_FILL GDK_GC_FILL
#define XLIKE_GC_FONT GDK_GC_FONT
#define XLIKE_GC_FOREGROUND GDK_GC_FOREGROUND
#define XLIKE_GC_FUNCTION GDK_GC_FUNCTION
#define XLIKE_GC_LINE_WIDTH GDK_GC_LINE_WIDTH
#define XLIKE_GC_STIPPLE GDK_GC_STIPPLE
#define XLIKE_GC_TILE GDK_GC_TILE

#define XLIKE_GX_COPY GDK_COPY
#define XLIKE_GX_XOR GDK_XOR

#define XLIKE_FILL_STIPPLED GDK_STIPPLED
#define XLIKE_FILL_OPAQUE_STIPPLED GDK_OPAQUE_STIPPLED
#define XLIKE_FILL_TILED GDK_TILED
#define XLIKE_FILL_SOLID GDK_SOLID

/* functions */

/* Avoid unused-variable warning involving D */
#define GET_XLIKE_DISPLAY(d) (USED (d), NULL)
#define GET_XLIKE_X_DISPLAY(d) (USED (d), GDK_DISPLAY ())
#define GET_XLIKE_WINDOW(w) GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (w))
#define XLIKE_FILL_RECTANGLE(dpy, x_win, gc, x, y, width, height)	\
  (USED (dpy), gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE,	\
                                   x, y, width, height))
#define XLIKE_DRAW_RECTANGLE(dpy, x_win, gc, x, y, width, height)	\
  (USED (dpy), gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, FALSE,	\
                                   x, y, width, height))
#define XLIKE_DRAW_LINE(dpy, x_win, gc, x1, y1, x2, y2)			\
  (USED (dpy), gdk_draw_line (GDK_DRAWABLE (x_win), gc, x1, y1, x2, y2))
#define XLIKE_TEXT_WIDTH(fi, ptr, len) \
  gdk_text_width (fi, (char *) ptr, len)
#define XLIKE_TEXT_WIDTH_WIDE(fi, ptr, len) \
  gdk_text_width_wc (fi, (GdkWChar *) ptr, len)

/* FIXME: This is totally bogus.  It removes dl->top_clip from the
   equations.  If there is a bug involving this, fix it properly!
   Or just ensure that top_clip is 0. */
#define XLIKE_DISPLAY_LINE_HEIGHT(dl) \
  ((dl)->ascent + (dl)->descent - (dl)->clip)
#define XLIKE_DISPLAY_LINE_YPOS(dl) ((dl)->ypos - (dl)->ascent)
#define XLIKE_DISPLAY_LINE_TOP_CLIP(dl) (0)
#define XLIKE_SET_CLIP_RECTANGLE(dpy, gc, xorig, yorig, prect)	\
do								\
  {								\
    USED (dpy);							\
    gdk_gc_set_clip_rectangle (gc, prect);			\
    gdk_gc_set_clip_origin (gc, xorig, yorig);			\
  }								\
while (0)
#define XLIKE_CLEAR_CLIP_MASK(dpy, gc)		\
do						\
  {						\
    USED (dpy);					\
    gdk_gc_set_clip_rectangle (gc, NULL);	\
    gdk_gc_set_clip_origin (gc, 0, 0);		\
  }						\
while (0)
#define XLIKE_FLUSH(dpy) gdk_flush ()
#define XLIKE_CLEAR_AREA(dpy, win, x, y, width, height) \
  (USED (dpy), gdk_window_clear_area (win, x, y, width, height))

#define IMAGE_INSTANCE_XLIKE_MASK IMAGE_INSTANCE_GTK_MASK
#define XIMAGE_INSTANCE_XLIKE_PIXMAP XIMAGE_INSTANCE_GTK_PIXMAP
#define COLOR_INSTANCE_XLIKE_COLOR(ci) (*COLOR_INSTANCE_GTK_COLOR (ci))
#define FONT_INSTANCE_XLIKE_FONT FONT_INSTANCE_GTK_FONT
#define DEVICE_XLIKE_GRAY_PIXMAP DEVICE_GTK_GRAY_PIXMAP
#define DEVICE_XLIKE_GC_CACHE DEVICE_GTK_GC_CACHE
#define XLIKE_SET_GC_FILL(gc, style) ((gc).fill = (style))
#define XLIKE_COLOR_TO_PIXEL(c) ((c).pixel)
#define XLIKE_SET_GC_COLOR(lval, rval) ((lval) = (rval))
#define XLIKE_SET_GC_PIXEL(lval, rval) ((lval).pixel = (rval))
#define XLIKE_FONT_NUM(val) (val)

/************ End GTK flavor of XLIKE **********/

#endif /* (not) THIS_IS_X */


#endif /* INCLUDED_console_xlike_inc_h_ */
