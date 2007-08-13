/* toolbar implementation -- X interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
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

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "glyphs-x.h"
#include "objects-x.h"
#include "xgccache.h"
#include "EmacsFrame.h"
#include "EmacsFrameP.h"
#include "EmacsManager.h"

#include "faces.h"
#include "frame.h"
#include "toolbar.h"
#include "window.h"

static void
x_draw_blank_toolbar_button (struct frame *f, int x, int y, int width,
			     int height, int threed)
{
  struct device *d = XDEVICE (f->device);
  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC top_shadow_gc, bottom_shadow_gc, background_gc;

  background_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f);

  if (threed)
    {
      top_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC (f);
      bottom_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f);
    }
  else
    {
      top_shadow_gc = background_gc;
      bottom_shadow_gc = background_gc;
    }

  /* Draw the outline. */
  x_output_shadows (f, x, y, width, height, top_shadow_gc,
		    bottom_shadow_gc, background_gc, shadow_thickness);

  /* Blank the middle. */
  XFillRectangle (dpy, x_win, background_gc, x + shadow_thickness,
		  y + shadow_thickness, width - shadow_thickness * 2,
		  height - shadow_thickness * 2);
}

static void
x_output_toolbar_button (struct frame *f, Lisp_Object button)
{
  struct device *d = XDEVICE (f->device);
  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;

  Display *dpy = DEVICE_X_DISPLAY (d);
  Window x_win = XtWindow (FRAME_X_TEXT_WIDGET (f));
  GC top_shadow_gc, bottom_shadow_gc, background_gc;
  Lisp_Object instance, frame, window, glyph;
  struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
  struct Lisp_Image_Instance *p;
  struct window *w;

  XSETFRAME (frame, f);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  glyph = get_toolbar_button_glyph (w, tb);

  if (tb->enabled)
    {
      if (tb->down)
	{
	  top_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f);
	  bottom_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC (f);
	}
      else
	{
	  top_shadow_gc = FRAME_X_TOOLBAR_TOP_SHADOW_GC (f);
	  bottom_shadow_gc = FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f);
	}
    }
  else
    {
      top_shadow_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f);
      bottom_shadow_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f);
    }
  background_gc = FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f);

  /* Draw the outline. */
  x_output_shadows (f, tb->x, tb->y, tb->width, tb->height, top_shadow_gc,
		    bottom_shadow_gc, background_gc, shadow_thickness);

  /* Clear the pixmap area. */
  XFillRectangle (dpy, x_win, background_gc, tb->x + shadow_thickness,
		  tb->y + shadow_thickness, tb->width - shadow_thickness * 2,
		  tb->height - shadow_thickness * 2);

  background_gc = FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC (f);

  /* #### It is currently possible for users to trash us by directly
     changing the toolbar glyphs.  Avoid crashing in that case. */
  if (GLYPHP (glyph))
    instance = glyph_image_instance (glyph, window, ERROR_ME_NOT, 1);
  else
    instance = Qnil;

  if (IMAGE_INSTANCEP (instance))
    {
      int width = tb->width - shadow_thickness * 2;
      int height = tb->height - shadow_thickness * 2;
      int x_offset = shadow_thickness;
      int y_offset = shadow_thickness;

      p = XIMAGE_INSTANCE (instance);

      if (IMAGE_INSTANCE_PIXMAP_TYPE_P (p))
	{
	  if (width > (int) IMAGE_INSTANCE_PIXMAP_WIDTH (p))
	    {
	      x_offset += ((int) (width - IMAGE_INSTANCE_PIXMAP_WIDTH (p))
			   / 2);
	      width = IMAGE_INSTANCE_PIXMAP_WIDTH (p);
	    }
	  if (height > (int) IMAGE_INSTANCE_PIXMAP_HEIGHT (p))
	    {
	      y_offset += ((int) (height - IMAGE_INSTANCE_PIXMAP_HEIGHT (p))
			   / 2);
	      height = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);
	    }

	  x_output_x_pixmap (f, XIMAGE_INSTANCE (instance), tb->x + x_offset,
			     tb->y + y_offset, 0, 0, 0, 0, width, height,
			     0, 0, 0, background_gc);
	}
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_TEXT)
	{
	  /* #### We need to make the face used configurable. */
	  struct face_cachel *cachel =
	    WINDOW_FACE_CACHEL (w, DEFAULT_INDEX);
	  struct display_line dl;
	  Lisp_Object string = IMAGE_INSTANCE_TEXT_STRING (p);
	  unsigned char charsets[NUM_LEADING_BYTES];
	  emchar_dynarr *buf;
	  struct font_metric_info fm;

	  /* This could be true if we were called via the Expose event
             handler.  Mark the button as dirty and return
             immediately. */
	  if (f->window_face_cache_reset)
	    {
	      tb->dirty = 1;
	      MARK_TOOLBAR_CHANGED;
	      return;
	    }
	  buf = Dynarr_new (Emchar);
	  convert_bufbyte_string_into_emchar_dynarr
	    (string_data (XSTRING (string)),
	     string_length (XSTRING (string)),
	     buf);
	  find_charsets_in_emchar_string (charsets, Dynarr_atp (buf, 0),
					  Dynarr_length (buf));
	  ensure_face_cachel_complete (cachel, window, charsets);
	  face_cachel_charset_font_metric_info (cachel, charsets, &fm);
	  
	  dl.ascent = fm.ascent;
	  dl.descent = fm.descent;
	  dl.ypos = tb->y + y_offset + fm.ascent;

	  if (fm.ascent + fm.descent <= height)
	    {
	      dl.ypos += (height - fm.ascent - fm.descent) / 2;
	      dl.clip = 0;
	    }
	  else
	    {
	      dl.clip = fm.ascent + fm.descent - height;
	    }

	  x_output_string (w, &dl, buf, tb->x + x_offset, 0, 0, width,
			   DEFAULT_INDEX, 0, 0, 0, 0);
	  Dynarr_free (buf);
	}

      /* We silently ignore the image if it isn't a pixmap or text. */
    }

  tb->dirty = 0;
}

static int
x_get_button_size (struct frame *f, Lisp_Object window,
		   struct toolbar_button *tb, int vert, int pos)
{
  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  int shadow_thickness = ef->emacs_frame.toolbar_shadow_thickness;
  int size;

  if (tb->blank)
    {
      if (!NILP (tb->down_glyph))
	size = XINT (tb->down_glyph);
      else
	size = DEFAULT_TOOLBAR_BLANK_SIZE;
    }
  else
    {
      struct window *w = XWINDOW (window);
      Lisp_Object glyph = get_toolbar_button_glyph (w, tb);

      /* Unless, of course, the user has done something stupid like
         change the glyph out from under us.  Use a blank placeholder
         in that case. */
      if (NILP (glyph))
	return XINT (f->toolbar_size[pos]);

      if (vert)
	size = glyph_height (glyph, Vdefault_face, 0, window);
      else
	size = glyph_width (glyph, Vdefault_face, 0, window);
    }

  if (!size)
    {
      /* If the glyph doesn't have a size we'll insert a blank
         placeholder instead. */
      return XINT (f->toolbar_size[pos]);
    }

  size += shadow_thickness * 2;

  return (size);
}

#define X_OUTPUT_BUTTONS_LOOP(left)					\
  do {									\
    while (!NILP (button))						\
      {									\
	struct toolbar_button *tb = XTOOLBAR_BUTTON (button);		\
	int size, height, width;					\
									\
	if (left && tb->pushright)					\
	  break;							\
									\
        size = x_get_button_size (f, window, tb, vert, pos);		\
									\
	if (vert)							\
	  {								\
	    width = bar_width;						\
	    if (y + size > max_pixpos)					\
	      height = max_pixpos - y;					\
	    else							\
	      height = size;						\
	  }								\
	else								\
	  {								\
	    if (x + size > max_pixpos)					\
	      width = max_pixpos - x;					\
	    else							\
	      width = size;						\
	    height = bar_height;					\
	  }								\
									\
	if (tb->x != x							\
	    || tb->y != y						\
	    || tb->width != width					\
	    || tb->height != height					\
	    || tb->dirty)						\
	  {								\
	    if (width && height)					\
	      {								\
		tb->x = x;						\
		tb->y = y;						\
		tb->width = width;					\
		tb->height = height;					\
									\
                if (tb->blank || NILP (tb->up_glyph))			\
		  {							\
		    int threed = (EQ (Qt, tb->up_glyph) ? 1 : 0);	\
		    x_draw_blank_toolbar_button (f, x, y, width,	\
						 height, threed);	\
		  }							\
	        else							\
		  x_output_toolbar_button (f, button);			\
	      }								\
	  }								\
									\
	if (vert)							\
	  y += height;							\
	else								\
	  x += width;							\
									\
	if ((vert && y == max_pixpos) || (!vert && x == max_pixpos))	\
	  button = Qnil;						\
	else								\
	  button = tb->next;						\
      }									\
  } while (0)

#define SET_TOOLBAR_WAS_VISIBLE_FLAG(frame, pos, flag)			\
  do {									\
    switch (pos)							\
      {									\
      case TOP_TOOLBAR:							\
	(frame)->top_toolbar_was_visible = flag;			\
	break;								\
      case BOTTOM_TOOLBAR:						\
	(frame)->bottom_toolbar_was_visible = flag;			\
	break;								\
      case LEFT_TOOLBAR:						\
	(frame)->left_toolbar_was_visible = flag;			\
	break;								\
      case RIGHT_TOOLBAR:						\
	(frame)->right_toolbar_was_visible = flag;			\
	break;								\
      default:								\
	abort ();							\
      }									\
  } while (0)

static void
x_output_toolbar (struct frame *f, enum toolbar_pos pos)
{
  struct device *d = XDEVICE (f->device);
  int x, y, bar_width, bar_height, vert;
  int max_pixpos, right_size, right_start, blank_size;
  Lisp_Object button, window;

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 1);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);

  if (vert)
    max_pixpos = y + bar_height;
  else
    max_pixpos = x + bar_width;

  button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;
  right_size = 0;

  /* First loop over all of the buttons to determine how much room we
     need for left hand and right hand buttons.  This loop will also
     make sure that all instances are instantiated so when we actually
     output them they will come up immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      int size = x_get_button_size (f, window, tb, vert, pos);

      if (tb->pushright)
	right_size += size;

      button = tb->next;
    }

  button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;

  /* Loop over the left buttons, updating and outputting them. */
  X_OUTPUT_BUTTONS_LOOP (1);

  /* Now determine where the right buttons start. */
  right_start = max_pixpos - right_size;
  if (right_start < (vert ? y : x))
    right_start = (vert ? y : x);

  /* Output the blank which goes from the end of the left buttons to
     the start of the right. */
  blank_size = right_start - (vert ? y : x);
  if (blank_size)
    {
      int height, width;

      if (vert)
	{
	  width = bar_width;
	  height = blank_size;
	}
      else
	{
	  width = blank_size;
	  height = bar_height;
	}

      x_draw_blank_toolbar_button (f, x, y, width, height, 1);

      if (vert)
	y += height;
      else
	x += width;
    }

  /* Loop over the right buttons, updating and outputting them. */
  X_OUTPUT_BUTTONS_LOOP (0);

  if (!vert)
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      DEVMETH (d, clear_region, (frame,
				 DEFAULT_INDEX, FRAME_PIXWIDTH (f) - 1, y, 1,
				 bar_height));
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);

  XFlush (DEVICE_X_DISPLAY (d));
}

static void
x_clear_toolbar (struct frame *f, enum toolbar_pos pos, int thickness_change)
{
  Lisp_Object frame = Qnil;
  struct device *d = XDEVICE (f->device);
  int x, y, width, height, vert;

  get_toolbar_coords (f, pos, &x, &y, &width, &height, &vert, 1);
  XSETFRAME (frame, f);

  /* The thickness_change parameter is used by the toolbar resize routines
     to clear any excess toolbar if the size shrinks. */
  if (thickness_change < 0)
    {
      if (pos == LEFT_TOOLBAR || pos == RIGHT_TOOLBAR)
	{
	  x = x + width + thickness_change;
	  width = -thickness_change;
	}
      else
	{
	  y = y + height + thickness_change;
	  height = -thickness_change;
	}
    }
 
  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 0);

  DEVMETH (d, clear_region, (frame, DEFAULT_INDEX, x, y, width, height));
  XFlush (DEVICE_X_DISPLAY (d));
}

static void
x_output_frame_toolbars (struct frame *f)
{
  assert (FRAME_X_P (f));

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    x_output_toolbar (f, TOP_TOOLBAR);
  else if (f->top_toolbar_was_visible)
    x_clear_toolbar (f, TOP_TOOLBAR, 0);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    x_output_toolbar (f, BOTTOM_TOOLBAR);
  else if (f->bottom_toolbar_was_visible)
    x_clear_toolbar (f, BOTTOM_TOOLBAR, 0);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    x_output_toolbar (f, LEFT_TOOLBAR);
  else if (f->left_toolbar_was_visible)
    x_clear_toolbar (f, LEFT_TOOLBAR, 0);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    x_output_toolbar (f, RIGHT_TOOLBAR);
  else if (f->right_toolbar_was_visible)
    x_clear_toolbar (f, RIGHT_TOOLBAR, 0);
}

static void
x_redraw_exposed_toolbar (struct frame *f, enum toolbar_pos pos, int x, int y,
			  int width, int height)
{
  int bar_x, bar_y, bar_width, bar_height, vert;
  Lisp_Object button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;

  get_toolbar_coords (f, pos, &bar_x, &bar_y, &bar_width, &bar_height,
		      &vert, 1);

  if (((y + height) < bar_y) || (y > (bar_y + bar_height)))
    return;
  if (((x + width) < bar_x) || (x > (bar_x + bar_width)))
    return;

  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);

      if (vert)
	{
	  if (((tb->y + tb->height) > y) && (tb->y < (y + height)))
	    tb->dirty = 1;

	  /* If this is true we have gone past the exposed region. */
	  if (tb->y > (y + height))
	    break;
	}
      else
	{
	  if (((tb->x + tb->width) > x) && (tb->x < (x + width)))
	    tb->dirty = 1;

	  /* If this is true we have gone past the exposed region. */
	  if (tb->x > (x + width))
	    break;
	}

      button = tb->next;
    }

  /* Even if none of the buttons is in the area, the blank region at
     the very least must be because the first thing we did is verify
     that some portion of the toolbar is in the exposed region. */
  x_output_toolbar (f, pos);
}

static void
x_redraw_exposed_toolbars (struct frame *f, int x, int y, int width,
			   int height)
{
  assert (FRAME_X_P (f));

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    x_redraw_exposed_toolbar (f, TOP_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    x_redraw_exposed_toolbar (f, BOTTOM_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    x_redraw_exposed_toolbar (f, LEFT_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    x_redraw_exposed_toolbar (f, RIGHT_TOOLBAR, x, y, width, height);
}

static void
x_redraw_frame_toolbars (struct frame *f)
{
  /* There are certain startup paths that lead to update_EmacsFrame in
     faces.c being called before a new frame is fully initialized.  In
     particular before we have actually mapped it.  That routine can
     call this one.  So, we need to make sure that the frame is
     actually ready before we try and draw all over it. */

  if (XtIsRealized (FRAME_X_SHELL_WIDGET (f)))
    x_redraw_exposed_toolbars (f, 0, 0, FRAME_PIXWIDTH (f),
			       FRAME_PIXHEIGHT (f));
}


static void
x_toolbar_size_changed_in_frame_1 (struct frame *f, enum toolbar_pos pos,
				   Lisp_Object old_visibility)
{
  XtWidgetGeometry req, repl;
  int newval;
  int oldval = FRAME_X_OLD_TOOLBAR_SIZE (f, pos);

  if (NILP (f->toolbar_visible_p[pos]))
    newval = 0;
  else if (!f->init_finished && !INTP (f->toolbar_size[pos]))
    /* the size might not be set at all if we're in the process of
       creating the frame.  Otherwise it better be, and we'll crash
       out if not. */
    newval = 0;
  else
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      newval = XINT (Fspecifier_instance (Vtoolbar_size[pos], frame, Qzero,
					  Qnil));
    }

  if (oldval == newval)
    return;

  /* We want the text area to stay the same size.  So, we query the
     current size and then adjust it for the change in the toolbar
     size. */

  in_specifier_change_function++;
  if (!in_resource_setting)
    /* mirror the value in the frame resources, unless it was already
       done. */
    XtVaSetValues (FRAME_X_TEXT_WIDGET (f),
		   pos == TOP_TOOLBAR ? XtNtopToolBarHeight :
		   pos == BOTTOM_TOOLBAR ? XtNbottomToolBarHeight :
		   pos == LEFT_TOOLBAR ? XtNleftToolBarWidth :
		   XtNrightToolBarWidth,
		   newval, 0);
  if (XtIsRealized (FRAME_X_CONTAINER_WIDGET (f)))
    {
      int change = newval - oldval;
      Lisp_Object new_visibility = f->toolbar_visible_p[pos];

      req.request_mode = 0;
      /* the query-geometry method looks at the current value of
	 f->toolbar_size[pos], so temporarily set it back to the old
	 one.  If we were called because of a visibility change we
	 also have to temporarily restore its old status as well. */
      f->toolbar_size[pos] = make_int (oldval);
      if (!EQ (old_visibility, Qzero))
	f->toolbar_visible_p[pos] = old_visibility;
      XtQueryGeometry (FRAME_X_CONTAINER_WIDGET (f), &req, &repl);
      f->toolbar_size[pos] = make_int (newval);
      if (!EQ (old_visibility, Qzero))
	f->toolbar_visible_p[pos] = new_visibility;
      
      if (change < 0)
	x_clear_toolbar (f, pos, change);
      if (pos == LEFT_TOOLBAR || pos == RIGHT_TOOLBAR)
	repl.width += change;
      else
	repl.height += change;

      MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (f);
      EmacsManagerChangeSize (FRAME_X_CONTAINER_WIDGET (f), repl.width,
			      repl.height);
    }
  /* #### should this go within XtIsRealized()?  probably not ... */
  FRAME_X_OLD_TOOLBAR_SIZE (f, pos) = newval;
  in_specifier_change_function--;
}

static void
x_toolbar_size_changed_in_frame (struct frame *f, enum toolbar_pos pos,
				 Lisp_Object oldval)
{
  x_toolbar_size_changed_in_frame_1 (f, pos, Qzero);
}

static void
x_toolbar_visible_p_changed_in_frame (struct frame *f, enum toolbar_pos pos,
				      Lisp_Object oldval)
{
  x_toolbar_size_changed_in_frame_1 (f, pos, oldval);
}

static void
x_initialize_frame_toolbar_gcs (struct frame *f)
{
  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);
  XGCValues gcv;
  unsigned long flags = (GCForeground | GCBackground | GCGraphicsExposures);

  gcv.foreground = ef->emacs_frame.background_toolbar_pixel;
  gcv.background = ef->core.background_pixel;
  gcv.graphics_exposures = False;
  FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f) =
    XtGetGC ((Widget) ef, flags, &gcv);

  if (ef->emacs_frame.top_toolbar_shadow_pixel == -1)
    {
      ef->emacs_frame.top_toolbar_shadow_pixel =
	ef->emacs_frame.background_toolbar_pixel;
    }
  if (ef->emacs_frame.bottom_toolbar_shadow_pixel == -1)
    {
      ef->emacs_frame.bottom_toolbar_shadow_pixel =
	ef->emacs_frame.background_toolbar_pixel;
    }

  x_generate_shadow_pixels (f, &ef->emacs_frame.top_toolbar_shadow_pixel,
			    &ef->emacs_frame.bottom_toolbar_shadow_pixel,
			    ef->emacs_frame.background_toolbar_pixel,
			    ef->core.background_pixel);

  gcv.foreground = ef->emacs_frame.top_toolbar_shadow_pixel;
  gcv.background = ef->core.background_pixel;
  gcv.graphics_exposures = False;
  flags = GCForeground | GCBackground | GCGraphicsExposures;
  if (ef->emacs_frame.top_toolbar_shadow_pixmap)
    {
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple = ef->emacs_frame.top_toolbar_shadow_pixmap;
      flags |= GCStipple | GCFillStyle;
    }
  FRAME_X_TOOLBAR_TOP_SHADOW_GC (f) = XtGetGC ((Widget) ef, flags, &gcv);

  gcv.foreground = ef->emacs_frame.bottom_toolbar_shadow_pixel;
  gcv.background = ef->core.background_pixel;
  gcv.graphics_exposures = False;
  flags = GCForeground | GCBackground | GCGraphicsExposures;
  if (ef->emacs_frame.bottom_toolbar_shadow_pixmap)
    {
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple = ef->emacs_frame.bottom_toolbar_shadow_pixmap;
      flags |= GCStipple | GCFillStyle;
    }
  FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f) = XtGetGC ((Widget) ef, flags, &gcv);

#ifdef HAVE_XPM
  FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC (f) =
    FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f);
#else
  {
    struct device *d = XDEVICE (f->device);
    Display *dpy = DEVICE_X_DISPLAY (d);

    gcv.background = WhitePixelOfScreen (DefaultScreenOfDisplay (dpy));
    gcv.foreground = BlackPixelOfScreen (DefaultScreenOfDisplay (dpy));
    gcv.graphics_exposures = False;
    flags = GCForeground | GCBackground | GCGraphicsExposures;
    FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC (f) =
      XtGetGC ((Widget) ef, flags, &gcv);
  }
#endif
}

static void
x_release_frame_toolbar_gcs (struct frame *f)
{
  Widget ew = (Widget) FRAME_X_TEXT_WIDGET (f);
  XtReleaseGC (ew, FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f));
  /* If compiled with XPM support, this is a pointer to the same GC as
     FRAME_X_BLANK_BACKGROUND_GC so we need to make sure we don't
     release it twice. */
#ifndef HAVE_XPM
  XtReleaseGC (ew, FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC (f));
#endif
  XtReleaseGC (ew, FRAME_X_TOOLBAR_TOP_SHADOW_GC (f));
  XtReleaseGC (ew, FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f));

  /* Seg fault if we try and use these again. */
  FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC (f) = (GC) -1;
  FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC (f) = (GC) -1;
  FRAME_X_TOOLBAR_TOP_SHADOW_GC (f) = (GC) -1;
  FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC (f) = (GC) -1;
}

static void
x_initialize_frame_toolbars (struct frame *f)
{
  EmacsFrame ef = (EmacsFrame) FRAME_X_TEXT_WIDGET (f);

  if (ef->emacs_frame.toolbar_shadow_thickness < MINIMUM_SHADOW_THICKNESS)
    {
      XtVaSetValues (FRAME_X_TEXT_WIDGET (f), XtNtoolBarShadowThickness,
		     MINIMUM_SHADOW_THICKNESS, 0);
    }

  x_initialize_frame_toolbar_gcs (f);
}

/* This only calls one function but we go ahead and create this in
   case we ever do decide that we need to do more work. */
static void
x_free_frame_toolbars (struct frame *f)
{
  x_release_frame_toolbar_gcs (f);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_x (void)
{
  CONSOLE_HAS_METHOD (x, output_frame_toolbars);
  CONSOLE_HAS_METHOD (x, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (x, free_frame_toolbars);
  CONSOLE_HAS_METHOD (x, output_toolbar_button);
  CONSOLE_HAS_METHOD (x, redraw_exposed_toolbars);
  CONSOLE_HAS_METHOD (x, redraw_frame_toolbars);
  CONSOLE_HAS_METHOD (x, toolbar_size_changed_in_frame);
  CONSOLE_HAS_METHOD (x, toolbar_visible_p_changed_in_frame);
}
