/* toolbar implementation -- Generic redisplay interface.
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

#include "faces.h"
#include "frame.h"
#include "toolbar.h"
#include "window.h"

/* Only a very few things need to differ based on the toolkit used.
**
** Some of the routines used assert(FRAME_yyy_P(f)) checks, this is
** now abstracted into __INTERNAL_APPROPRIATENESS_CHECK().  When we
** add new window systems that use this code, we should either add a
** new case here, or just remove the checks completely.
**
** At least for X & GTK redraw_frame_toolbars() might end up getting
** called before we are completely initialized.  To avoid this, we use
** the __INTERNAL_MAPPED_P(f) macro, that should return 0 if we should
** not draw the toolbars yet.  When we add new window systems that use
** this code, we should add a new case here, if they need it.
**
** When clearing the toolbar, it is nice to flush the drawing queue.
** Use __INTERNAL_FLUSH to do this.  It is passed a device.
*/
#if defined(HAVE_GTK)
#include "console-gtk.h"
#define __INTERNAL_MAPPED_P(f) GTK_WIDGET_REALIZED (FRAME_GTK_TEXT_WIDGET (f))
#define __INTERNAL_FLUSH(d) gdk_flush()
#define __INTERNAL_APPROPRIATENESS_CHECK(f) assert(FRAME_GTK_P (f))
#elif defined(HAVE_X_WINDOWS)
#include "console-x.h"
#define __INTERNAL_MAPPED_P(f) XtIsRealized (FRAME_X_SHELL_WIDGET (f))
#define __INTERNAL_APPROPRIATENESS_CHECK(f) assert(FRAME_X_P (f))
#define __INTERNAL_FLUSH(d) XFlush (DEVICE_X_DISPLAY (d))
#else
#define __INTERNAL_MAPPED_P(f) abort()
#define __INTERNAL_APPROPRIATENESS_CHECK(f) abort()
#define __INTERNAL_FLUSH(f) abort()
#endif

#include "toolbar-common.h"

static void __prepare_button_area (struct frame *f,
				   struct toolbar_button *tb)
{
  int sx = tb->x;
  int sy = tb->y;
  int swidth = tb->width;
  int sheight = tb->height;
  int border_width = tb->border_width;
  int x_adj, width_adj, y_adj, height_adj;
  struct device *d = XDEVICE (f->device);
  Lisp_Object  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  struct window *w = XWINDOW (window);
  int shadow_thickness = 2;
  face_index toolbar_findex;

  if (tb->vertical)
    {
      x_adj = border_width;
      width_adj = - 2 * border_width;
      y_adj = height_adj = 0;
    }
  else
    {
      x_adj = width_adj = 0;
      y_adj = border_width;
      height_adj = - 2 * border_width;
    }

  toolbar_findex = get_builtin_face_cache_index (w, Vtoolbar_face);

  /* Blank toolbar buttons that should be 3d will have EQ(tb->up_glyph, Qt)
  ** Blank toolbar buttons that should be flat will have NILP (tb->up_glyph)
  **
  ** Real toolbar buttons will check tb->enabled && tb->down
  */
  if (EQ (Qt, tb->up_glyph))
    {
      shadow_thickness = 2;
    }
  else if (NILP (tb->up_glyph))
    {
      shadow_thickness = 0;
    }
  else
    {
      if (tb->enabled)
	{
	  if (tb->down)
	    shadow_thickness = -2;
	  else
	    shadow_thickness = 2;
	}
      else
	{
	  shadow_thickness = 0;
	}
    }
  
  /* Blank the entire area. */
  redisplay_clear_region (window, toolbar_findex,
			  sx + x_adj, sy + y_adj,
			  swidth + width_adj,
			  sheight + height_adj);

  /* Draw the outline. */
  if (shadow_thickness)
    {
      MAYBE_DEVMETH (d, bevel_area,
		     (w, toolbar_findex, sx + x_adj,
		      sy + y_adj, swidth + width_adj,
		      sheight + height_adj, abs(shadow_thickness),
		      EDGE_ALL, (shadow_thickness < 0) ? EDGE_BEVEL_IN : EDGE_BEVEL_OUT));
    }

  /* Handle the borders... */
  redisplay_clear_region (window, toolbar_findex,
			  sx, sy,
			  (tb->vertical ? border_width : swidth),
			  (tb->vertical ? sheight : border_width));
  redisplay_clear_region (window, toolbar_findex,
			  (tb->vertical ? sx + swidth : sx),
			  (tb->vertical ? sy : sy + sheight),
			  (tb->vertical ? border_width : swidth),
			  (tb->vertical ? sheight : border_width));
}

#define common_draw_blank_toolbar_button(f,tb) __prepare_button_area (f,tb)

void
common_output_toolbar_button (struct frame *f, Lisp_Object button)
{
  int shadow_thickness = 2;
  int x_adj, y_adj, width_adj, height_adj;
  struct device *d = XDEVICE (f->device);
  Lisp_Object instance, frame, window, glyph;
  struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
  struct Lisp_Image_Instance *p;
  struct window *w;
  int vertical = tb->vertical;
  int border_width = tb->border_width;
  face_index toolbar_findex;

  if (vertical)
    {
      x_adj = border_width;
      width_adj = - 2 * border_width;
      y_adj = 0;
      height_adj = 0;
    }
  else
    {
      x_adj = 0;
      width_adj = 0;
      y_adj = border_width;
      height_adj = - 2 * border_width;
    }

  XSETFRAME (frame, f);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  glyph = get_toolbar_button_glyph (w, tb);

  if (tb->enabled)
    {
      if (tb->down)
	{
	  shadow_thickness = -2;
	}
      else
	{
	  shadow_thickness = 2;
	}
    }
  else
    {
      shadow_thickness = 0;
    }

  toolbar_findex = get_builtin_face_cache_index (w, Vtoolbar_face);

  __prepare_button_area (f, tb);

  /* #### It is currently possible for users to trash us by directly
     changing the toolbar glyphs.  Avoid crashing in that case. */
  if (GLYPHP (glyph))
    instance = glyph_image_instance (glyph, window, ERROR_ME_NOT, 1);
  else
    instance = Qnil;

  if (IMAGE_INSTANCEP (instance))
    {
      int width = tb->width + width_adj - shadow_thickness * 2;
      int height = tb->height + height_adj - shadow_thickness * 2;
      int x_offset = x_adj + shadow_thickness;
      int y_offset = y_adj + shadow_thickness;

      p = XIMAGE_INSTANCE (instance);

      if (IMAGE_INSTANCE_PIXMAP_TYPE_P (p))
	{
	  struct display_box db;
	  struct display_glyph_area dga;

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

	  /* Draw exactly in the area specified... */
	  db.xpos = tb->x + x_offset;
	  db.ypos = tb->y + y_offset;
	  db.width = width;
	  db.height = height;

	  /* Display the whole glyph */
	  dga.xoffset = 0;
	  dga.yoffset = 0;
	  dga.width = width;
	  dga.height = height;
	  
	  redisplay_output_pixmap (w, instance,
				   &db, &dga, 
				   toolbar_findex, 0, 0, 0, 0);
	}
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_TEXT)
	{
	  /* #### We need to make the face used configurable. */
	  struct face_cachel *cachel =
	    WINDOW_FACE_CACHEL (w, DEFAULT_INDEX);
	  struct display_line dl;
	  Lisp_Object string = IMAGE_INSTANCE_TEXT_STRING (p);
	  unsigned char charsets[NUM_LEADING_BYTES];
	  Emchar_dynarr *buf;
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
	  convert_intbyte_string_into_emchar_dynarr
	    (XSTRING_DATA (string), XSTRING_LENGTH (string), buf);
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

	  MAYBE_DEVMETH (d, output_string,
			 (w, &dl, buf, tb->x + x_offset, 0, 0, width,
			  toolbar_findex, 0, 0, 0, 0));
	  Dynarr_free (buf);
	}

      /* We silently ignore the image if it isn't a pixmap or text. */
    }

  tb->dirty = 0;
}

static int
common_get_button_size (struct frame *f, Lisp_Object window,
			struct toolbar_button *tb, int vert, int pos)
{
  int shadow_thickness = 2;
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
	size = glyph_height (glyph, window);
      else
	size = glyph_width (glyph, window);
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

#define COMMON_OUTPUT_BUTTONS_LOOP(left)				\
  do {									\
    while (!NILP (button))						\
      {									\
	struct toolbar_button *tb = XTOOLBAR_BUTTON (button);		\
	int size, height, width;					\
									\
	if (left && tb->pushright)					\
	  break;							\
									\
        size = common_get_button_size (f, window, tb, vert, pos);	\
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
	        tb->border_width = border_width;			\
	        tb->vertical = vert;					\
									\
                if (tb->blank || NILP (tb->up_glyph))			\
		  {							\
		    common_draw_blank_toolbar_button (f, tb);		\
		  }							\
	        else							\
		  common_output_toolbar_button (f, button);		\
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
common_output_toolbar (struct frame *f, enum toolbar_pos pos)
{
  int x, y, bar_width, bar_height, vert;
  int max_pixpos, right_size, right_start, blank_size;
  int border_width = FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, pos);
  Lisp_Object button, window;
  face_index toolbar_findex;

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 1);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  toolbar_findex = get_builtin_face_cache_index (XWINDOW (window), Vtoolbar_face);

  /* Do the border */
  redisplay_clear_region (window, toolbar_findex,
			  x, y,
			  (vert ? bar_width : border_width),
			  (vert ? border_width : bar_height));
  redisplay_clear_region (window, toolbar_findex,
			  (vert ? x : x + bar_width - border_width),
			  (vert ? y + bar_height - border_width : y),
			  (vert ? bar_width : border_width),
			  (vert ? border_width : bar_height));

  if (vert)
    {
      max_pixpos = y + bar_height - border_width;
      y += border_width;
    }
  else
    {
      max_pixpos = x + bar_width - border_width;
      x += border_width;
    }

  button = FRAME_TOOLBAR_BUTTONS (f, pos);
  right_size = 0;

  /* First loop over all of the buttons to determine how much room we
     need for left hand and right hand buttons.  This loop will also
     make sure that all instances are instantiated so when we actually
     output them they will come up immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      int size = common_get_button_size (f, window, tb, vert, pos);

      if (tb->pushright)
	right_size += size;

      button = tb->next;
    }

  button = FRAME_TOOLBAR_BUTTONS (f, pos);

  /* Loop over the left buttons, updating and outputting them. */
  COMMON_OUTPUT_BUTTONS_LOOP (1);

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

      /*
       * Use a 3D pushright separator only if there isn't a toolbar
       * border.  A flat separator meshes with the border and looks
       * better.
       */
      if (1)
	{
	  struct toolbar_button tb;

	  tb.x = x;
	  tb.y = y;
	  tb.width = width;
	  tb.height = height;
	  tb.border_width = border_width;
	  tb.vertical = vert;
	  tb.enabled = 1;
	  tb.up_glyph = border_width ? Qt : Qnil;

	  __prepare_button_area (f, &tb);
	}

      if (vert)
	y += height;
      else
	x += width;
    }

  /* Loop over the right buttons, updating and outputting them. */
  COMMON_OUTPUT_BUTTONS_LOOP (0);

  if (!vert)
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      redisplay_clear_region (frame,
			      DEFAULT_INDEX, FRAME_PIXWIDTH (f) - 1, y, 1,
			      bar_height);
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);
  __INTERNAL_FLUSH (XDEVICE (f->device));
}

static void
common_clear_toolbar (struct frame *f, enum toolbar_pos pos, int thickness_change)
{
  Lisp_Object frame;
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

  redisplay_clear_region (frame, DEFAULT_INDEX, x, y, width, height);

  __INTERNAL_FLUSH (XDEVICE (f->device));
}

void
common_output_frame_toolbars (struct frame *f)
{
  __INTERNAL_APPROPRIATENESS_CHECK(f);

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    common_output_toolbar (f, TOP_TOOLBAR);
  else if (f->top_toolbar_was_visible)
    common_clear_toolbar (f, TOP_TOOLBAR, 0);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    common_output_toolbar (f, BOTTOM_TOOLBAR);
  else if (f->bottom_toolbar_was_visible)
    common_clear_toolbar (f, BOTTOM_TOOLBAR, 0);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    common_output_toolbar (f, LEFT_TOOLBAR);
  else if (f->left_toolbar_was_visible)
    common_clear_toolbar (f, LEFT_TOOLBAR, 0);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    common_output_toolbar (f, RIGHT_TOOLBAR);
  else if (f->right_toolbar_was_visible)
    common_clear_toolbar (f, RIGHT_TOOLBAR, 0);
}

static void
common_redraw_exposed_toolbar (struct frame *f, enum toolbar_pos pos, int x, int y,
			    int width, int height)
{
  int bar_x, bar_y, bar_width, bar_height, vert;
  Lisp_Object button = FRAME_TOOLBAR_BUTTONS (f, pos);

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
  common_output_toolbar (f, pos);
}

void
common_redraw_exposed_toolbars (struct frame *f, int x, int y, int width,
				int height)
{
  __INTERNAL_APPROPRIATENESS_CHECK(f);

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    common_redraw_exposed_toolbar (f, TOP_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    common_redraw_exposed_toolbar (f, BOTTOM_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    common_redraw_exposed_toolbar (f, LEFT_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    common_redraw_exposed_toolbar (f, RIGHT_TOOLBAR, x, y, width, height);
}

void
common_redraw_frame_toolbars (struct frame *f)
{
  /* There are certain startup paths that lead to update_EmacsFrame in
     faces.c being called before a new frame is fully initialized.  In
     particular before we have actually mapped it.  That routine can
     call this one.  So, we need to make sure that the frame is
     actually ready before we try and draw all over it. */

  if (__INTERNAL_MAPPED_P(f))
    common_redraw_exposed_toolbars (f, 0, 0, FRAME_PIXWIDTH (f),
				    FRAME_PIXHEIGHT (f));
}
