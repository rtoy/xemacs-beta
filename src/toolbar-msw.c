/* toolbar implementation -- mswindows interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002, 2010 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.
   Copyright (C) 1998 Andy Piper.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* This implementation by Andy Piper <andy@xemacs.org>, with bits
   borrowed from toolbar-x.c */

/* Synched up with: Not in FSF. */

/* This file essentially Mule-ized (except perhaps some Unicode splitting).
   5-2000. (??? Needs a once-over.) */

#define NEED_MSWINDOWS_COMMCTRL

#include <config.h>
#include "lisp.h"

#include "device.h"
#include "elhash.h"
#include "faces.h"
#include "frame-impl.h"
#include "gui.h"
#include "toolbar.h"
#include "window.h"

#include "console-msw-impl.h"
#include "glyphs-msw.h"
/* #include "fontcolor-msw.h" */

#define TOOLBAR_ITEM_ID_MIN 0x4000
#define TOOLBAR_ITEM_ID_MAX 0x7FFF
#define TOOLBAR_ITEM_ID_BITS(x) (((x) & 0x3FFF) | 0x4000)
#define TOOLBAR_ID_BIAS 16
#define TOOLBAR_HANDLE(f,p) \
GetDlgItem (FRAME_MSWINDOWS_HANDLE (f), TOOLBAR_ID_BIAS + p)

#define MSWINDOWS_BUTTON_SHADOW_THICKNESS 2
#define MSWINDOWS_BLANK_SIZE 5
#define MSWINDOWS_MINIMUM_TOOLBAR_SIZE 8

static void
mswindows_move_toolbar (struct frame *f, enum edge_pos pos);

static int
allocate_toolbar_item_id (struct frame *f, struct toolbar_button *button,
			  enum edge_pos UNUSED (pos))
{
  /* hmm what do we generate an id based on */
  int id = TOOLBAR_ITEM_ID_BITS (internal_hash (button->callback, 0, 0));
  while (!NILP (Fgethash (make_fixnum (id),
			  FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f), Qnil)))
    {
      id = TOOLBAR_ITEM_ID_BITS (id + 1);
    }
  return id;
}

static void
mswindows_clear_toolbar (struct frame *f, enum edge_pos pos,
			 int UNUSED (thickness_change))
{
  HIMAGELIST ilist = NULL;
  int i;
  HWND toolbarwnd = TOOLBAR_HANDLE (f, pos);
  if (toolbarwnd)
    {
      TBBUTTON info;
      
      /* Delete the buttons and remove the command from the hash table */
      i = qxeSendMessage (toolbarwnd, TB_BUTTONCOUNT, 0, 0);
      for (i--; i >= 0; i--)
	{
	  qxeSendMessage (toolbarwnd, TB_GETBUTTON, (WPARAM) i,
		       (LPARAM) &info);
	  Fremhash (make_fixnum (info.idCommand), 
		    FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f));
	  qxeSendMessage (toolbarwnd, TB_DELETEBUTTON, (WPARAM) i, 0);
	}
	  
      /* finally get rid of the image list assuming it clears up its
         bitmaps */
      qxeSendMessage (toolbarwnd, TB_GETIMAGELIST, 0, (LONG) &ilist);
      if (ilist)
	{
	  ImageList_Destroy (ilist);
	}
      qxeSendMessage (toolbarwnd, TB_SETIMAGELIST, 0, (LPARAM)NULL);

      ShowWindow (toolbarwnd, SW_HIDE);
    }

  FRAME_MSWINDOWS_TOOLBAR_CHECKSUM (f, pos) = 0;
  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 0);
}

static void
mswindows_output_toolbar (struct frame *f, enum edge_pos pos)
{
  int x, y, bar_width, bar_height, vert;
  int width=-1, height=-1, bmwidth=0, bmheight=0, maxbmwidth, maxbmheight;
  int style_3d=0;
  int border_width = FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, pos);
  Lisp_Object button, glyph, instance;
  Lisp_Object window = FRAME_LAST_NONMINIBUF_WINDOW (f);

  int nbuttons=0;
  int shadow_thickness = 2;	/* get this from somewhere else? */
  int window_frame_width = 3;
  int padding = (border_width + shadow_thickness) * 2;
  unsigned int checksum=0;
  struct window *w = XWINDOW (window);
  TBBUTTON *button_tbl, *tbbutton;
  HIMAGELIST ilist=NULL;
  HWND toolbarwnd=NULL;

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 0);

  /* ediff bogusly sets the height to 2 for some obscure X-specific
     reason. This ensures that we only try and output a toolbar for
     sensible sizes */
  if (bar_width < MSWINDOWS_MINIMUM_TOOLBAR_SIZE
      ||
      bar_height < MSWINDOWS_MINIMUM_TOOLBAR_SIZE)
    {
      return;
    }

  if (x==1)
    x=0;

  toolbarwnd = TOOLBAR_HANDLE (f,pos);
  
  /* set button sizes based on bar size */
  if (vert)
    {
      if (style_3d)
	{
	  width = height = bar_width
	    - (window_frame_width + shadow_thickness) * 2; 
	}
      else 
	width = height = bar_width;

      maxbmwidth = maxbmheight = width - padding;
    }
  else
    {
      if (style_3d)
	{
	  height = width = bar_height 
	    - (window_frame_width + shadow_thickness) * 2; 
	}
      else 
	width = height = bar_height;

      maxbmwidth = maxbmheight = width - padding;
    }

  button = FRAME_TOOLBAR_BUTTONS (f, pos);

  /* First loop over all of the buttons to determine how many there
     are. This loop will also make sure that all instances are
     instantiated so when we actually output them they will come up
     immediately. */
  while (!NILP (button))
    {

      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      checksum = HASH5 (checksum, 
			internal_hash (get_toolbar_button_glyph (w, tb), 0, 0),
			internal_hash (tb->callback, 0, 0),
			width,
			LISP_HASH (w->toolbar_buttons_captioned_p));
      button = tb->next;
      nbuttons++;
    }

  /* only rebuild if something has changed */
  if (!toolbarwnd || FRAME_MSWINDOWS_TOOLBAR_CHECKSUM (f,pos)!=checksum)
    {
      /* remove the old one */
      mswindows_clear_toolbar (f, pos, 0);

      FRAME_MSWINDOWS_TOOLBAR_CHECKSUM (f, pos)=checksum;

      /* build up the data required by win32 fns. */
      button_tbl = xnew_array_and_zero (TBBUTTON, nbuttons);
      button = FRAME_TOOLBAR_BUTTONS (f, pos);
      tbbutton = button_tbl;

      while (!NILP (button))
	{
	  struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
	  HBITMAP bitmap=NULL, mask=NULL;
	  bitmap=mask=NULL;

	  if (tb->blank)
	    tbbutton->fsStyle = TBSTYLE_SEP;
	  else 
	    {
	      tbbutton->idCommand = allocate_toolbar_item_id (f, tb, pos);
	      /* currently we output the toolbar again with disabled
		 buttons it might be good to use the ms disabled code
		 instead but that means another image list, so we'll stick
		 with the emacs model. */
	      tbbutton->fsState = tb->enabled ? TBSTATE_ENABLED :
		TBSTATE_INDETERMINATE;
	      tbbutton->fsStyle = TBSTYLE_BUTTON;
	      tbbutton->dwData=0; 
	      tbbutton->iString=0;
	      
	      /* mess with the button image */
	      glyph = get_toolbar_button_glyph (w, tb);
	      
	      if (GLYPHP (glyph))
		instance = glyph_image_instance (glyph, window, 
						 ERROR_ME_DEBUG_WARN, 1);
	      else
		instance = Qnil;
	      
	      if (IMAGE_INSTANCEP (instance))
		{
		  Lisp_Image_Instance *p = XIMAGE_INSTANCE (instance);
		  
		  if (IMAGE_INSTANCE_PIXMAP_TYPE_P (p))
		    {
		      /* we are going to honor the toolbar settings
			 and resize the bitmaps accordingly if they are
			 too big.  If they are too small we leave them
			 and pad the difference - unless a different size
			 crops up in the middle, at which point we *have*
			 to resize since the ImageList won't cope. */
		      
		      if ((bmwidth 
			   && 
			   IMAGE_INSTANCE_PIXMAP_WIDTH (p) != bmwidth)
			  ||
			  (bmheight 
			   && 
			   IMAGE_INSTANCE_PIXMAP_HEIGHT (p) != bmheight)
			  ||
			  IMAGE_INSTANCE_PIXMAP_WIDTH (p) > maxbmwidth
			  ||
			  IMAGE_INSTANCE_PIXMAP_HEIGHT (p) > maxbmheight)
			{
			  if (!bmheight)
			    bmheight = min (maxbmheight, 
					    IMAGE_INSTANCE_PIXMAP_HEIGHT (p));
			  if (!bmwidth)
			    bmwidth = min (maxbmwidth,
					   IMAGE_INSTANCE_PIXMAP_WIDTH (p));
		      
			  if (! (bitmap = mswindows_create_resized_bitmap 
				 (p, f, bmwidth, bmheight)))
			    {
			      xfree (button_tbl);
			      if (ilist) ImageList_Destroy (ilist);
			      gui_error ("Couldn't resize pixmap", instance);
			    }
			  /* we don't care if the mask fails */
			  mask = mswindows_create_resized_mask 
			    (p, f, bmwidth, bmheight);
			}
		      else 
			{
			  if (!bmwidth)
			    bmwidth = IMAGE_INSTANCE_PIXMAP_WIDTH (p);
			  if (!bmheight)
			    bmheight = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);
			}
	      
		      /* need to build an image list for the bitmaps */
		      if (!ilist && !(ilist = ImageList_Create 
				      ( bmwidth, bmheight,
					(IMAGE_INSTANCE_MSWINDOWS_MASK (p) 
					 ? ILC_MASK  : 0) | ILC_COLOR24, 
					nbuttons, nbuttons * 2 )))
			{
			  xfree (button_tbl);
			  gui_error ("Couldn't create image list", instance);
			}

		      /* make the mask actually do something */
		      ImageList_SetBkColor (ilist, CLR_NONE);
		      /* add a bitmap to the list */
		      if ((tbbutton->iBitmap =
			   ImageList_Add
			   (ilist,
			    bitmap ? bitmap 
			    : IMAGE_INSTANCE_MSWINDOWS_BITMAP (p),
			    mask ? mask 
			    : IMAGE_INSTANCE_MSWINDOWS_MASK (p))) < 0)
			{
			  xfree (button_tbl);
			  if (ilist)
			    ImageList_Destroy (ilist);
			  gui_error 
			    ("couldn't add image to image list", instance);
			}
		      /* we're done with these now */
		      DeleteObject (bitmap);
		      DeleteObject (mask);
		    }
		}

	      Fputhash (make_fixnum (tbbutton->idCommand), 
			button, FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f));
	    }

	  /* now fix up the button size */
	  tb->x = x;
	  tb->y = y;
	  tb->vertical = vert;
	  tb->border_width = border_width;
	  tb->width = width + MSWINDOWS_BUTTON_SHADOW_THICKNESS * 2;
	  tb->height = height + MSWINDOWS_BUTTON_SHADOW_THICKNESS * 2;

	  if (tb->blank)
	    {
	      if (vert)
		tb->height = MSWINDOWS_BLANK_SIZE;
	      else
		tb->width = MSWINDOWS_BLANK_SIZE;
	    }
	  
	  if (vert)							
	    y += tb->height;
	  else
	    x += tb->width;
	  /* move on to the next button */
	  tbbutton++;
	  button = tb->next;
	}

      button = FRAME_TOOLBAR_BUTTONS (f, pos);

      /* create the toolbar window? */
      if (!toolbarwnd 
	  &&
	  (toolbarwnd = 
	   qxeCreateWindowEx (WS_EX_WINDOWEDGE,
			      XETEXT (TOOLBARCLASSNAME),
			      NULL,
			      WS_CHILD 
			      | (style_3d ? WS_DLGFRAME : 0)
			      | TBSTYLE_TOOLTIPS 
			      | CCS_NORESIZE 
			      | CCS_NOPARENTALIGN | CCS_NODIVIDER
			      | CCS_ADJUSTABLE,
			      x, y, bar_width, bar_height,
			      FRAME_MSWINDOWS_HANDLE (f),
			      (HMENU)(TOOLBAR_ID_BIAS + pos),
			      NULL, 
			      NULL))==NULL)
	{
	  xfree (button_tbl);
	  ImageList_Destroy (ilist);
	  gui_error ("couldn't create toolbar", Qunbound);
	}

      /* finally populate with images */
      if (qxeSendMessage (toolbarwnd, TB_BUTTONSTRUCTSIZE,
			  (WPARAM)sizeof (TBBUTTON), (LPARAM)0) == -1) 
	{
	  mswindows_clear_toolbar (f, pos, 0);
	  gui_error ("couldn't set button structure size", Qunbound);
	}

      if (vert)
	height = min (bmheight + padding, height);
      else
	width = min (bmwidth + padding, width);
	
      /* pad the buttons */
      qxeSendMessage (toolbarwnd, TB_SETPADDING,
		      0, MAKELPARAM (width - bmwidth, height - bmheight));

      /* set the size of buttons */
      qxeSendMessage (toolbarwnd, TB_SETBUTTONSIZE, 0, 
		      (LPARAM) MAKELONG (width, height));

      /* set the size of bitmaps */
      qxeSendMessage (toolbarwnd, TB_SETBITMAPSIZE, 0, 
		      (LPARAM) MAKELONG (bmwidth, bmheight));

      /* tell it we've done it */
      qxeSendMessage (toolbarwnd, TB_AUTOSIZE, 0, 0);
		   
      /* finally populate with images */
      if (!qxeSendMessage (toolbarwnd, TB_ADDBUTTONS,
			   (WPARAM) nbuttons, (LPARAM) button_tbl))
	{
	  mswindows_clear_toolbar (f, pos, 0);
	  gui_error ("couldn't add button list to toolbar", Qunbound);
	}

      /* vertical toolbars need more rows */
      if (vert)
	{
	  RECT tmp;
	  qxeSendMessage (toolbarwnd, TB_SETROWS, 
			  MAKEWPARAM (nbuttons, FALSE), (LPARAM) &tmp);
	}

      else
	{
	  RECT tmp;
	  qxeSendMessage (toolbarwnd, TB_SETROWS, MAKEWPARAM (1, FALSE), 
			  (LPARAM)&tmp);
	}

      /* finally populate with images */
      if (qxeSendMessage (toolbarwnd, TB_SETIMAGELIST, 0,
			  (LPARAM)ilist) < 0
	  ||
	  qxeSendMessage (toolbarwnd, TB_SETDISABLEDIMAGELIST, 0,
			  (LPARAM)ilist) < 0)
	{
	  mswindows_clear_toolbar (f, pos, 0);
	  gui_error ("couldn't add image list to toolbar", Qunbound);
	}

      /* now display the window */
      ShowWindow (toolbarwnd, SW_SHOW);
      /* no idea why this is necessary but initial display will not
         happen otherwise. */
      mswindows_move_toolbar (f, pos);

      if (button_tbl)
	xfree (button_tbl);

      SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);
    }
}

static void
mswindows_move_toolbar (struct frame *f, enum edge_pos pos)
{
  int bar_x, bar_y, bar_width, bar_height, vert;
  HWND toolbarwnd = TOOLBAR_HANDLE (f,pos);

  if (toolbarwnd)
    {
      get_toolbar_coords (f, pos, &bar_x, &bar_y, &bar_width, &bar_height,
			  &vert, 1);

      /* #### This terrible mangling with coordinates perhaps
	 arises from different treatment of toolbar positions
	 by Windows and by XEmacs. */
      switch (pos)
	{
	case TOP_EDGE:
	  bar_x--; bar_y-=2;
	  bar_width+=3; bar_height+=3;
	  break;
	case LEFT_EDGE:
	  bar_x--; bar_y-=2;
	  bar_height++; bar_width++;
	  break;
	case BOTTOM_EDGE:
	  bar_y-=2; 
	  bar_width+=4; bar_height+=4;
	  break;
	case RIGHT_EDGE:
	  bar_y-=2; bar_x++;
	  bar_width++; bar_height++;
	  break;
	}
      SetWindowPos (toolbarwnd, NULL, bar_x, bar_y, 
		    bar_width, bar_height, SWP_NOZORDER);
    }
}

static void
mswindows_redraw_exposed_toolbars (struct frame *f,
				   int UNUSED (x), int UNUSED (y),
				   int UNUSED (width), int UNUSED (height))
{
  enum edge_pos pos;
  assert (FRAME_MSWINDOWS_P (f));

  EDGE_POS_LOOP (pos)
    {
      if (FRAME_REAL_TOOLBAR_VISIBLE (f, pos))
	mswindows_move_toolbar (f, pos);
    }
}

static void
mswindows_redraw_frame_toolbars (struct frame *f)
{
  mswindows_redraw_exposed_toolbars (f, 0, 0, FRAME_PIXWIDTH (f),
				     FRAME_PIXHEIGHT (f));
}

static void
mswindows_initialize_frame_toolbars (struct frame *UNUSED (f))
{
}

static void
mswindows_output_frame_toolbars (struct frame *f)
{
  enum edge_pos pos;
  assert (FRAME_MSWINDOWS_P (f));

  EDGE_POS_LOOP (pos)
    {
      if (FRAME_REAL_TOOLBAR_VISIBLE (f, pos))
	mswindows_output_toolbar (f, pos);
    }
}

static void
mswindows_clear_frame_toolbars (struct frame *f)
{
  enum edge_pos pos;
  assert (FRAME_MSWINDOWS_P (f));

  EDGE_POS_LOOP (pos)
    {
      if (f->toolbar_was_visible[pos]
	  && !FRAME_REAL_TOOLBAR_VISIBLE (f, pos))
	mswindows_clear_toolbar (f, pos, 0);
    }
}

static void
mswindows_free_frame_toolbars (struct frame *f)
{
  HWND twnd=NULL;
#define DELETE_TOOLBAR(pos)				\
  mswindows_clear_toolbar (f, pos, 0);			\
  if ((twnd=GetDlgItem (FRAME_MSWINDOWS_HANDLE (f),	\
		       TOOLBAR_ID_BIAS + pos)))		\
      DestroyWindow (twnd)

  DELETE_TOOLBAR (TOP_EDGE);
  DELETE_TOOLBAR (BOTTOM_EDGE);
  DELETE_TOOLBAR (LEFT_EDGE);
  DELETE_TOOLBAR (RIGHT_EDGE);
#undef DELETE_TOOLBAR
}

Lisp_Object 
mswindows_get_toolbar_button_text (struct frame *f, int command_id)
{
  Lisp_Object button = Fgethash (make_fixnum (command_id),
				 FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f), Qnil);
  
  if (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      return tb->help_string;
    }
  return Qnil;
}

/*
 * Return value is Qt if we have dispatched the command,
 * or Qnil if id has not been mapped to a callback.
 * Window procedure may try other targets to route the
 * command if we return nil
 */
Lisp_Object
mswindows_handle_toolbar_wm_command (struct frame *f, HWND UNUSED (ctrl),
				     WORD id)
{
  /* Try to map the command id through the proper hash table */
  Lisp_Object button, data, fn, arg, frame;

  button = Fgethash (make_fixnum (id), 
		     FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f), Qnil);

  if (NILP (button))
    return Qnil;

  data = XTOOLBAR_BUTTON (button)->callback;

  /* #### ? */
  if (UNBOUNDP (data))
    return Qnil;

  /* Ok, this is our one. Enqueue it. */
  get_gui_callback (data, &fn, &arg);
  frame = wrap_frame (f);
  mswindows_enqueue_misc_user_event (frame, fn, arg);

  return Qt;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, output_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, clear_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, free_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, redraw_exposed_toolbars);
  CONSOLE_HAS_METHOD (mswindows, redraw_frame_toolbars);
}

