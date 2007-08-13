/* toolbar implementation -- mswindows interface.
   Copyright (C) 1998 Free Software Foundation.

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

/* This implementation by Andy Piper <andyp@parallax.co.uk>, with bits
   borrowed from toolbar-x.c */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "faces.h"
#include "frame.h"
#include "toolbar.h"
#include "window.h"
#include "gui.h"
#include "console-msw.h"
#include "glyphs-msw.h"
#include "objects-msw.h"

/* Why did Kirill choose this range ? */
#define TOOLBAR_ITEM_ID_MIN 0x4000
#define TOOLBAR_ITEM_ID_MAX 0x7FFF
#define TOOLBAR_ITEM_ID_BITS(x) (((x) & 0x3FFF) | 0x4000)
#ifndef TB_SETIMAGELIST
#define TB_SETIMAGELIST (WM_USER + 48)
#define TB_GETIMAGELIST (WM_USER + 49)
#endif

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

static int
allocate_toolbar_item_id (struct frame* f, struct toolbar_button* button)
{
  /* hmm what do we generate an id based on */
  int id = TOOLBAR_ITEM_ID_BITS (internal_hash (button->callback, 0));
  while (!NILP (Fgethash (make_int (id),
			  FRAME_MSWINDOWS_TOOLBAR_HASHTABLE (f), Qnil)))
    {
      id = TOOLBAR_ITEM_ID_BITS (id + 1);
    }
  return id;
}

static void
mswindows_clear_toolbar (struct frame *f, enum toolbar_pos pos,
			 int thickness_change)
{
  HIMAGELIST ilist=NULL;
  if (FRAME_MSWINDOWS_TOOLBAR (f))
    {
      /* get the image list and delete it */
      SendMessage (FRAME_MSWINDOWS_TOOLBAR (f), 
		   TB_GETIMAGELIST, 0, 
		   (LONG) &ilist);
      
      /* destroy the toolbar window */
      DestroyWindow (FRAME_MSWINDOWS_TOOLBAR (f));
      FRAME_MSWINDOWS_TOOLBAR (f) = 0;
      
      /* finally get rid of the image list assuming it clears up its
         bitmaps */
      if (ilist)
	{
	  ImageList_Destroy(ilist);
	}
    }
}

static void
mswindows_output_toolbar (struct frame *f, enum toolbar_pos pos)
{
  int x, y, bar_width, bar_height, vert;
  int width=-1, height=-1, bmwidth=-1, bmheight=-1;
  int border_width = FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, pos);
  Lisp_Object button, window, glyph, instance;
  int nbuttons=0;
  struct window *w;
  TBBUTTON* button_tbl, *tbbutton;
  HIMAGELIST ilist=NULL;

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 0);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  /* set button sizes based on bar size */
  if (vert)
    {
      width = height = bar_width - border_width * 2;
      bmwidth = bmheight = width -2;
    }
  else
    {
      height = width = bar_height - border_width * 2;
      bmwidth = bmheight = width -2;
    }

  button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;

  /* First loop over all of the buttons to determine how many there
     are. This loop will also make sure that all instances are
     instantiated so when we actually output them they will come up
     immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      button = tb->next;
      nbuttons++;
    }

  /* build up the data required by win32 fns. */
  button_tbl = xnew_array_and_zero (TBBUTTON, nbuttons);
  button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;
  tbbutton = button_tbl;

  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);

      tbbutton->idCommand = allocate_toolbar_item_id (f, tb);
      tbbutton->fsState=tb->enabled ? TBSTATE_ENABLED : TBSTATE_INDETERMINATE;
      tbbutton->fsStyle=tb->blank ? TBSTYLE_SEP : TBSTYLE_BUTTON;
      tbbutton->dwData=0; 
      tbbutton->iString=0;
      
      /* mess with the button image */
      glyph = get_toolbar_button_glyph (w, tb);

      if (GLYPHP (glyph))
	instance = glyph_image_instance (glyph, window, ERROR_ME_NOT, 1);
      else
	instance = Qnil;

      if (IMAGE_INSTANCEP (instance))
	{
	  struct Lisp_Image_Instance* p = XIMAGE_INSTANCE (instance);

	  if (IMAGE_INSTANCE_PIXMAP_TYPE_P (p))
	    {
	      /* we are going to honour the toolbar settings and
		 resize the bitmaps accordingly */
	      
	      if ((IMAGE_INSTANCE_PIXMAP_WIDTH (p) != bmwidth
		   ||
		   IMAGE_INSTANCE_PIXMAP_HEIGHT (p) != bmheight)
		  &&
		  !mswindows_resize_dibitmap_instance (p, f, 
						       bmwidth, bmheight))
		{
		  xfree (button_tbl);
		  if (ilist) ImageList_Destroy (ilist);
		  signal_simple_error ("couldn't resize pixmap", 
				       instance);
		}
	      
	      /* need to build an image list for the bitmaps */
	      if (!ilist)
		{
		  if (!(ilist = ImageList_Create 
			( IMAGE_INSTANCE_PIXMAP_WIDTH (p),
			  IMAGE_INSTANCE_PIXMAP_HEIGHT (p),
			  ILC_COLOR24, 	
			  nbuttons,
			  nbuttons * 2 )))
		    {
		      xfree (button_tbl);
		      signal_simple_error ("couldn't create image list",
					   instance);
		    }
		}
  
	      /* add a bitmap to the list */
	      if ((tbbutton->iBitmap =
		   ImageList_Add (ilist, 
				  IMAGE_INSTANCE_MSWINDOWS_BITMAP (p),
				  IMAGE_INSTANCE_MSWINDOWS_MASK (p))) < 0)
		{
		  xfree (button_tbl);
		  if (ilist) ImageList_Destroy (ilist);
		  signal_simple_error ("image list creation failed", 
				       instance);
		}
	    }
	}

      Fputhash (make_int (tbbutton->idCommand), 
		tb->callback, FRAME_MSWINDOWS_TOOLBAR_HASHTABLE (f));
      
      tbbutton++;
      button = tb->next;
    }

  button = FRAME_TOOLBAR_DATA (f, pos)->toolbar_buttons;

  /* now create the toolbar ... */
  if ((FRAME_MSWINDOWS_TOOLBAR (f) =
       CreateToolbarEx (FRAME_MSWINDOWS_HANDLE (f),
			TBSTYLE_ALTDRAG | WS_CHILD,
			NULL,
			nbuttons, 	
			0,
			0, 	
			button_tbl,
			nbuttons, 	
			width, height,
			bmwidth, bmheight, 	
			sizeof(TBBUTTON) )) == NULL)
    {
      xfree (button_tbl);
      ImageList_Destroy (ilist);
      error ("couldn't create toolbar");
    }

  /* finally populate with images */
  if (SendMessage (FRAME_MSWINDOWS_TOOLBAR (f), TB_SETIMAGELIST, NULL,
		   (LPARAM)ilist) == -1) 
    {
      mswindows_clear_toolbar (f, pos, 0);
      error ("couldn't add image list to toolbar");
    }

  /* now move the window */
  SetWindowPos (FRAME_MSWINDOWS_TOOLBAR (f), HWND_TOP, x, y, 
		bar_width, bar_height,
		SWP_SHOWWINDOW);
#if 0
  ShowWindow (FRAME_MSWINDOWS_TOOLBAR (f), SW_SHOWNORMAL);
#endif

  if (button_tbl) xfree (button_tbl);

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);
}

static void
mswindows_initialize_frame_toolbars (struct frame *f)
{

}

static void
mswindows_output_frame_toolbars (struct frame *f)
{
  assert (FRAME_MSWINDOWS_P (f));

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    mswindows_output_toolbar (f, TOP_TOOLBAR);
  else if (f->top_toolbar_was_visible)
    mswindows_clear_toolbar (f, TOP_TOOLBAR, 0);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    mswindows_output_toolbar (f, BOTTOM_TOOLBAR);
  else if (f->bottom_toolbar_was_visible)
    mswindows_clear_toolbar (f, BOTTOM_TOOLBAR, 0);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    mswindows_output_toolbar (f, LEFT_TOOLBAR);
  else if (f->left_toolbar_was_visible)
    mswindows_clear_toolbar (f, LEFT_TOOLBAR, 0);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    mswindows_output_toolbar (f, RIGHT_TOOLBAR);
  else if (f->right_toolbar_was_visible)
    mswindows_clear_toolbar (f, RIGHT_TOOLBAR, 0);
}

static void
mswindows_free_frame_toolbars (struct frame *f)
{
  mswindows_clear_toolbar(f, 0, 0);
}

/*
 * Return value is Qt if we have dispatched the command,
 * or Qnil if id has not been mapped to a callback.
 * Window procedure may try other targets to route the
 * command if we return nil
 */
Lisp_Object
mswindows_handle_toolbar_wm_command (struct frame* f, WORD id)
{
  /* Try to map the command id through the proper hash table */
  Lisp_Object command, funcsym, frame;
  struct gcpro gcpro1;

  command = Fgethash (make_int (id), 
		      FRAME_MSWINDOWS_TOOLBAR_HASHTABLE (f), Qunbound);
  if (UNBOUNDP (command))
    {
      return Qnil;
    }

  /* Need to gcpro because the hashtable may get destroyed
     by menu_cleanup(), and will not gcpro the command
     any more */
  GCPRO1 (command);
  
  /* Ok, this is our one. Enqueue it. */
  if (SYMBOLP (command))
      funcsym = Qcall_interactively;
  else if (CONSP (command))
      funcsym = Qeval;
  else
    signal_simple_error ("Callback must be either evallable form or a symbol",
			 command);

  XSETFRAME (frame, f);
  enqueue_misc_user_event (frame, funcsym, command);

  /* Needs good bump also, for WM_COMMAND may have been dispatched from
     mswindows_need_event, which will block again despite new command
     event has arrived */
  mswindows_bump_queue ();
  
  UNGCPRO; /* command */
  return Qt;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, output_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (mswindows, free_frame_toolbars);
}

