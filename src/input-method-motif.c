/* Various functions for X11R5+ input methods, using the Motif XmIm* functions.
   input-method-xlib.c provides a lower-level implementation.
   Copyright (C) 1996 Sun Microsystems.

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

/* Written by Martin Buchholz. */

#include <config.h>
#include <X11/Xlocale.h>        /* More portable than <locale.h> ? */
#include "lisp.h"
#include "frame-impl.h"
#include "console-x-impl.h"
#include "device.h"
#include "frame.h"
#include "EmacsFrame.h"
#include <Xm/Xm.h>

#ifndef XIM_MOTIF
#error  XIM_MOTIF is not defined??
#endif


/* Create X input method for device */
void
XIM_init_device (struct device *d)
{
  /* Nothing to do */
}

/* Callback for the deleting frame. */
static void
XIM_delete_frame (Widget w, XtPointer client_data, XtPointer call_data)
{
  XmImUnregister ((Widget) client_data);
}

void
XIM_init_frame (struct frame *f)
{
  Widget w = FRAME_X_TEXT_WIDGET (f);
  XPoint spot = {0,0};
  XmFontList fontlist;
  XmFontListEntry fontlistEntry;

  typedef struct
    {
      XFontSet  fontset;
      Pixel     fg;
      Pixel     bg;
    } xim_resources_t;

  xim_resources_t xim_resources;

  /* mrb: #### Fix so that background and foreground is set from
     default face, rather than foreground and background resources, or
     that the user can use set-frame-parameters to set xic attributes */

#define res(name, class_, representation, field, default_value)	\
  { name, class_, representation, sizeof(xim_resources.field),	\
    XtOffsetOf(xim_resources_t, field),				\
    XtRString, (XtPointer) (default_value) }

  static XtResource resources[] =
  {
    /*  name              class          represent'n field    default value */
    res(XtNfontSet,       XtCFontSet,    XtRFontSet, fontset, XtDefaultFontSet),
    res(XtNximForeground, XtCForeground, XtRPixel,   fg,      XtDefaultForeground),
    res(XtNximBackground, XtCBackground, XtRPixel,   bg,      XtDefaultBackground)
  };

  XtGetApplicationResources (w, &xim_resources,
			     resources, XtNumber (resources),
			     NULL, 0);

  if (! xim_resources.fontset)
    {
      stderr_out ("Can't get fontset resource for Input Method\n");
      return;
    }

  fontlistEntry = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
					XmFONT_IS_FONTSET,
					(XtPointer) xim_resources.fontset);
  fontlist = XmFontListAppendEntry (NULL, fontlistEntry);
  XmImRegister (w, 0);
  XmImVaSetValues (w,
		   XmNfontList,     fontlist,
		   XmNforeground,   xim_resources.fg,
		   XmNbackground,   xim_resources.bg,
		   XmNspotLocation, &spot,
		   /*   XmNlineSpace, 0, */
		   NULL);

  XmFontListEntryFree (&fontlistEntry);

  XtAddCallback (w, XmNdestroyCallback, XIM_delete_frame, (XtPointer) w);
}

void
XIM_SetGeometry (struct frame *f)
{
}

void
XIM_SetSpotLocation (struct frame *f, int x, int y)
{
  /* #### FIX: Must make sure spot fits within Preedit Area */
  XPoint *spot = &(FRAME_X_XIC_SPOT (f));
  if (spot->x == (short) x &&
      spot->y == (short) y)
    return;

  spot->x = (short) x;
  spot->y = (short) y;

  XmImVaSetValues (FRAME_X_TEXT_WIDGET (f), XmNspotLocation, spot, NULL);
}

void
XIM_focus_event (struct frame *f, int in_p)
{
  if (in_p)
    XmImVaSetFocusValues (FRAME_X_TEXT_WIDGET (f), NULL);
  else
    XmImUnsetFocus (FRAME_X_TEXT_WIDGET (f));
}

void
vars_of_input_method_motif (void)
{
  Fprovide (intern ("xim"));
}
