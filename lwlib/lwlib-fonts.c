/* Font handling code for X and Xft.

Copyright (C) 2003 Eric Knauel
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
#include <stdio.h>
#include <stdlib.h>
#include "lwlib-fonts.h"

#if 0
/* these are all from ../src; if we need them move the code */
#include "lisp.h"
#include "device.h"
#include "device-impl.h"
#include "console-x-impl.h"
#include "font-mgr.h"
#endif

/*
 * code for handling Xft
 */

#ifdef USE_XFT

/* helper function to correctly open Xft/core fonts by name
   #### Can't we use FcParseName here?
   #### Is this done so often that the logic needs to be hard-coded in C?

   Daniel Pittman sez: Older code tried to enforce that an XLFD font was
   not scaled, while this version just doesn't care.  I think that is a
   better behavior, since if someone really wants a scaled font we should
   oblige them.

   Stephen sez: This whole function was ill-conceived, and I'm not sure it
   succeeds at any of the things it attempts to do.  First, we should be
   using fontconfig directly.  I'm not sure what Xft (or fontconfig) will
   try to do if passed an XLFD.  As for scaled fonts, both options are
   equally bad.  The problem is that the X server will often scale bitmap
   fonts willy-nilly; it's worth trying to avoid that, but I can't say
   whether that's worth overriding somebody who knows what they're doing.
   In any case, I think we should find out what Xft (fontconfig?) is able
   and willing to do with XLFDs, and probably move the logic to LISP.
*/
XftFont *
xft_open_font_by_name (Display *dpy, char *name)
{
  XftFont *res = NULL;

  /* if (!NILP (Fxft_xlfd_font_name_p (make_string (name, strlen (name))))) */
  /* #### this is bogus but ... */
  int count = 0;
  char *pos = name;
  /* extra parens shut up gcc */
  while ((pos = index (pos, '-')))
    {
      count++;
      pos++;
    }

  /* #### hard-coding DefaultScreen is evil! */
  if (count == 14		/* fully-qualified XLFD */
      || (count < 14		/* heuristic for wildcarded XLFD */
	  && count >= 5
	  && index (name, '*')))
    res = XftFontOpenXlfd (dpy, DefaultScreen (dpy), name);
  else
    res = XftFontOpenName (dpy, DefaultScreen (dpy), name);

  /* Try for a generic monospace font
     #### Why?  Menus don't need to line up in columns! */
  if (!res) 
    res = XftFontOpenName (dpy, DefaultScreen (dpy), "monospace");
  /* Try for anything we can get */
  if (!res)
    res = XftFontOpenName (dpy, DefaultScreen (dpy), "");

  if (!res)
    {
      /* #### This is Just So Wrong ... ! */
      /* sorry folks ... */
      fprintf (stderr,
	       "Unable to find any usable XFT font, even the defaults!\n");
      abort ();
      return 0;
    }

  return res;
}

#endif /* USE_XFT */

/* End of lwlib-fonts.c */
