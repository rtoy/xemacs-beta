/* Color data structures for X and Xft.

Copyright (C) 2004 Free Software Foundation, Inc.

Author:		Stephen J. Turnbull <stephen@xemacs.org>
Created:	24 Jul 2004 by Stephen J. Turnbull

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

/* Synched up with: Not in GNU Emacs. */

#include <config.h>
#include <limits.h> 		/* for ULONG_MAX */
#include <stdlib.h> 		/* for malloc() */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <X11/ShellP.h>		/* for ShellWidget */
#include "lwlib-colors.h"

static int debug_colors = 1;

#ifdef __cplusplus
#define X_CLASSFIELD c_class
#else
#define X_CLASSFIELD class
#endif

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

/* WIDGET is an Xt widget, VISUAL and DEPTH are return values */
void
visual_info_from_widget (Widget widget, Visual **visual, int *depth)
{
  /* grab the visual and depth from the nearest shell ancestor */
  Widget p = XtParent(widget);

  *visual = CopyFromParent;
  *depth = -1;
  while (*visual == CopyFromParent && p)
    {
      if (XtIsShell(p))
	{
	  *visual = ((ShellWidget)p)->shell.visual;
	  *depth = p->core.depth;
	}
      p = XtParent(p);
    }
  if (*visual == CopyFromParent || !*visual)
    {
      if (debug_colors > 1)
	fprintf (stderr, "\nvisual_info_from_widget:"
		 " failed, using DefaultVisualOfScreen");
      *visual = DefaultVisualOfScreen (XtScreen (widget));
      *depth = DefaultDepthOfScreen (XtScreen (widget));
    }
  else if (debug_colors > 1)
    fprintf (stderr, "\nvisual_info_from_widget: succeeded");
}

/* Do we need all this hair on modern hardware? */

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  Original was from FSFmacs,
   but rewritten by Jareth Hein <jareth@camelot-soft.com> 97/11/25
   Modified by Lee Kindness <lkindness@csl.co.uk> 31/08/99 to handle previous
   total failure which was due to a read/write colorcell being the nearest
   match - tries the next nearest...

   Return value is 1 for normal success, 2 for nearest color success,
   3 for Non-deallocable success. */
int
x_allocate_nearest_color (Display *display, Colormap colormap,
			  Visual *visual, XColor *color_def)
{
  int status;

  /* #### [[Apparently this is often called with data derived from a widget
     with no ShellWidget ancestor, or before the shell has a visual.
     Currently this recovery code is in xlwmenu.c and xlwscrollbar.c, but
     likely should come here.]]
     I suspect the problem is that the visual-tracing code was improperly
     written, missing a level of indirection.
     CopyFromParent == NULL in XFree86/Darwin.
  */
  if (visual == CopyFromParent || !visual)
    {
      Screen *screen = DefaultScreenOfDisplay (display);
      fprintf (stderr, "\nx_allocate_nearest_color: bad visual (%08lx)",
	       (unsigned long) visual);
      visual = DefaultVisualOfScreen (screen);
    }

  if (visual->X_CLASSFIELD == DirectColor || visual->X_CLASSFIELD == TrueColor)
    {
      if (XAllocColor (display, colormap, color_def) != 0)
	{
	  status = 1;
	}
      else
	{
	  /* We're dealing with a TrueColor/DirectColor visual, so play games
	     with the RGB values in the XColor struct. */
	  /* #### JH: I'm not sure how a call to XAllocColor can fail in a
	     TrueColor or DirectColor visual, so I will just reformat the
	     request to match the requirements of the visual, and re-issue
	     the request.  If this fails for anybody, I wanna know about it
	     so I can come up with a better plan */

	  unsigned long rshift,gshift,bshift,rbits,gbits,bbits,junk;
	  junk = visual->red_mask;
	  rshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    rshift ++;
	  }
	  rbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    rbits++;
	  }
	  junk = visual->green_mask;
	  gshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    gshift ++;
	  }
	  gbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    gbits++;
	  }
	  junk = visual->blue_mask;
	  bshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    bshift ++;
	  }
	  bbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    bbits++;
 	  }

	  color_def->red = color_def->red >> (16 - rbits);
	  color_def->green = color_def->green >> (16 - gbits);
	  color_def->blue = color_def->blue >> (16 - bbits);
	  if (XAllocColor (display, colormap, color_def) != 0)
	    status = 1;
	  else
  	    {
  	      int rd, gr, bl;
	      /* #### JH: I'm punting here, knowing that doing this will at
		 least draw the color correctly.  However, unless we convert
		 all of the functions that allocate colors (graphics
		 libraries, etc) to use this function doing this is very
		 likely to cause problems later... */

	      if (rbits > 8)
		rd = color_def->red << (rbits - 8);
	      else
		rd = color_def->red >> (8 - rbits);
	      if (gbits > 8)
		gr = color_def->green << (gbits - 8);
	      else
		gr = color_def->green >> (8 - gbits);
	      if (bbits > 8)
		bl = color_def->blue << (bbits - 8);
	      else
		bl = color_def->blue >> (8 - bbits);
	      color_def->pixel = (rd << rshift) | (gr << gshift) | (bl <<
								    bshift);
	      status = 3;
	    }
	}
    }
  else
    {
      XColor *cells = NULL;
      /* JH: I can't believe there's no way to go backwards from a
	 colormap ID and get its visual and number of entries, but X
	 apparently isn't built that way... */
      int no_cells = visual->map_entries;
      status = 0;

      if (XAllocColor (display, colormap, color_def) != 0)
	status = 1;
      else while( status != 2 )
	{
	  /* If we got to this point, the colormap is full, so we're
	     going to try and get the next closest color.  The algorithm used
	     is a least-squares matching, which is what X uses for closest
	     color matching with StaticColor visuals. */
	  int nearest;
	  long nearest_delta, trial_delta;
	  int x;

	  if( cells == NULL )
	    {
	      /* #### this could be annoyingly slow
		 tell me again why lwlib can't use alloca & friends? */
	      cells = (XColor *) malloc (sizeof(XColor)*no_cells);
	      for (x = 0; x < no_cells; x++)
		cells[x].pixel = x;

	      /* read the current colormap */
	      XQueryColors (display, colormap, cells, no_cells);
	    }

	  nearest = 0;
	  /* I'm assuming CSE so I'm not going to condense this. */
	  nearest_delta = ((((color_def->red >> 8) - (cells[0].red >> 8))
			    * ((color_def->red >> 8) - (cells[0].red >> 8)))
			   +
			   (((color_def->green >> 8) - (cells[0].green >> 8))
			    * ((color_def->green >> 8) - (cells[0].green >>
							  8)))
			   +
			   (((color_def->blue >> 8) - (cells[0].blue >> 8))
			    * ((color_def->blue >> 8) - (cells[0].blue >>
							 8))));
	  for (x = 1; x < no_cells; x++)
	    {
	      trial_delta = ((((color_def->red >> 8) - (cells[x].red >> 8))
			      * ((color_def->red >> 8) - (cells[x].red >> 8)))
			     +
			     (((color_def->green >> 8) - (cells[x].green >> 8))
			      * ((color_def->green >> 8) - (cells[x].green >>
							    8)))
			     +
			     (((color_def->blue >> 8) - (cells[x].blue >> 8))
			      * ((color_def->blue >> 8) - (cells[x].blue >>
							   8))));

	      /* less? Ignore cells marked as previously failing */
	      if( (trial_delta < nearest_delta) &&
		  (cells[x].pixel != ULONG_MAX) )
		{
		  nearest = x;
		  nearest_delta = trial_delta;
		}
	    }
	  color_def->red = cells[nearest].red;
	  color_def->green = cells[nearest].green;
	  color_def->blue = cells[nearest].blue;
	  if (XAllocColor (display, colormap, color_def) != 0)
	    status = 2;
	  else
	    /* LSK: Either the colour map has changed since
	     * we read it, or the colour is allocated
	     * read/write... Mark this cmap entry so it's
	     * ignored in the next iteration.
	     */
	    cells[nearest].pixel = ULONG_MAX;
	}
    }
  return status;
}

#if 0
/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  From GNU Emacs.
   #### Review this to see if there's anything our hairy version could use. */

int
FIXME_allocate_nearest_color (Display *display, Colormap screen_colormap,
		        XColor *color_def)
{
  int status = XAllocColor (display, screen_colormap, color_def);
  if (status)
    return status;

    {
      /* If we got to this point, the colormap is full, so we're
	 going to try to get the next closest color.
	 The algorithm used is a least-squares matching, which is
	 what X uses for closest color matching with StaticColor visuals.  */

      int nearest, x;
      unsigned long nearest_delta = ULONG_MAX;

      int no_cells = XDisplayCells (display, XDefaultScreen (display));
      /* Don't use alloca here because lwlib doesn't have the
         necessary configuration information that src does. */
      XColor *cells = (XColor *) malloc (sizeof (XColor) * no_cells);

      for (x = 0; x < no_cells; x++)
	cells[x].pixel = x;

      XQueryColors (display, screen_colormap, cells, no_cells);

      for (nearest = 0, x = 0; x < no_cells; x++)
	{
	  long dred   = (color_def->red   >> 8) - (cells[x].red   >> 8);
	  long dgreen = (color_def->green >> 8) - (cells[x].green >> 8);
	  long dblue  = (color_def->blue  >> 8) - (cells[x].blue  >> 8);
	  unsigned long delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (delta < nearest_delta)
	    {
	      nearest = x;
	      nearest_delta = delta;
	    }
	}
      color_def->red   = cells[nearest].red;
      color_def->green = cells[nearest].green;
      color_def->blue  = cells[nearest].blue;
      free (cells);
      return XAllocColor (display, screen_colormap, color_def);
    }
}
#endif


#ifdef USE_XFT

XftColor
xft_convert_color (Display *dpy, Colormap cmap, Visual *visual, int c, int dim)
{
  static XColor color;		/* #### why is this static ?? */
  XftColor result;
      
  color.pixel = c;
  XQueryColor(dpy, cmap, &color);

  if (dim)
    {
      color.red   = MINL (65535, color.red   * 1.5);
      color.green = MINL (65535, color.green * 1.5);
      color.blue  = MINL (65535, color.blue  * 1.5);
      x_allocate_nearest_color (dpy, cmap, visual, &color);
    }     

  result.pixel = color.pixel;
  result.color.red = color.red;
  result.color.green = color.green;
  result.color.blue = color.blue;
  result.color.alpha = 0xffff;
  
  return result;
}

#endif /* USE_XFT */

/* end of lwlib-colors.c */
