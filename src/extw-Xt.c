/* Common code between client and shell widgets -- Xt only.
   Copyright (C) 1993, 1994 Sun Microsystems, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing, September 1993. */

#ifdef emacs

#include <config.h>

#ifndef EXTERNAL_WIDGET
ERROR!  This ought not be getting compiled if EXTERNAL_WIDGET is undefined
#endif

EXTERN_C void fatal (const char *fmt, ...);
#else /* not emacs */
static void fatal (char *msg);
#endif

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <stdlib.h>
#include <stdio.h>
#include "compiler.h"
#include "extw-Xt.h"

#ifdef DEBUG_WIDGET

static int geom_masks[] = {
  CWX, CWY, CWWidth, CWHeight, CWBorderWidth, CWSibling, CWStackMode,
  XtCWQueryOnly };
static char *geom_mask_strings[] = {
  "CWX", "CWY", "CWWidth", "CWHeight", "CWBorderWidth",
  "CWSibling", "CWStackMode", "XtCWQueryOnly" };
static int stack_modes[] = {
  Below, TopIf, BottomIf, Opposite, XtSMDontChange };
static char *stack_mode_strings[] = {
  "Below", "TopIf", "BottomIf", "Opposite", "XtSMDontChange" };

static void
print_geometry_structure(XtWidgetGeometry *xwg)
{
  int num = sizeof(geom_masks)/sizeof(int);
  int i;

  printf ("  masks:");
  for (i=0; i<num; i++)
    if (xwg->request_mode & geom_masks[i])
      printf (" %s", geom_mask_strings[i]);
  printf ("\n");
  printf ("  x:%d y:%d\n", xwg->x, xwg->y);
  printf ("  width:%d height:%d border_width:%d\n", xwg->width,
	  xwg->height, xwg->border_width);
  printf ("  sibling: %x\n", xwg->sibling);
  printf ("  stack_mode: ");
  for (i=0, num=sizeof(stack_modes)/sizeof(int); i<num; i++)
    if (xwg->stack_mode == stack_modes[i]) {
      printf ("%s", stack_mode_strings[i]);
      break;
    }
  printf ("\n");
}

static void
print_geometry_result (XtGeometryResult res)
{
  printf ("result: %s\n",
	  res == XtGeometryYes ? "XtGeometryYes" :
	  res == XtGeometryNo ? "XtGeometryNo" :
	  res == XtGeometryAlmost ? "XtGeometryAlmost" :
	  res == XtGeometryDone ? "XtGeometryDone" :
	  "unknown");
}

#endif

#ifndef emacs

static void
fatal (char *msg)
{
  fprintf (stderr, "%s", msg);
  exit (1);
}

#endif

/* put a geometry specification in the specified property on the window
   of the specified widget, and send a notification message to tell the
   client-side widget about this. */

void
extw_send_geometry_value(Display *display, Window win, Atom property,
			 en_extw_notify type, XtWidgetGeometry *xwg,
			 long data0)
{
  if (xwg != NULL)
    XChangeProperty(display, win, property,
		    a_EXTW_WIDGET_GEOMETRY, 32, PropModeReplace,
		    (unsigned char *) xwg, sizeof(*xwg)/sizeof(int));
  extw_send_notify_3(display, win, type, data0, 0, 0);
}

/* get the geometry specification stored in the specified property of the
   specified widget's window. */

void
extw_get_geometry_value(Display *display, Window win, Atom property,
			XtWidgetGeometry *xwg)
{
  Atom dummy;
  int format;
  unsigned long nitems, bytes_after;
  unsigned char *prop;

  if (XGetWindowProperty(display, win, property, 0,
			 sizeof(*xwg)/4, False, a_EXTW_WIDGET_GEOMETRY,
			 &dummy, &format, &nitems, &bytes_after,
			 &prop) != Success)
    goto error;
  if (format != 8*sizeof(int) || bytes_after) {
    XFree((char *) prop);
    goto error;
  }
  *xwg = * (XtWidgetGeometry *) prop;
  return;

 error:
  fatal("Unable to retrieve property for widget-geometry");
#if 0
  XtAppErrorMsg(XtWidgetToApplicationContext((Widget)w),
		"invalidProperty","get_geometry_value",XtCXtToolkitError,
		"Unable to retrieve property for widget-geometry",
		(String *)NULL, (Cardinal *)NULL);
#endif
}

typedef struct
{
  Widget w;
  unsigned long request_num;
  en_extw_notify type;
} QueryStruct;

/* check if an event is of the sort we're looking for */

static Bool
isMine(XEvent *event, QueryStruct *q)
{
  Widget w = q->w;

  if ( (event->xany.display != XtDisplay(w)) || (event->xany.window != XtWindow(w)) )
    {
      return FALSE;
    }
  if (event->xany.serial >= q->request_num)
    {
      if (event->type == ClientMessage &&
	  event->xclient.message_type == a_EXTW_NOTIFY &&
	  event->xclient.data.l[0] == 1 - extw_which_side &&
	  event->xclient.data.l[1] == (int) q->type)
	{
	  return TRUE;
	}
    }
  return FALSE;
}

void responseTimeOut(XtPointer clientData, XtIntervalId  * UNUSED (id))
{
  Bool *expired=(Bool *)clientData;
  *expired=TRUE;
}

/* wait for a ClientMessage of the specified type from the other widget, or
   time-out.  isMine() determines whether an event matches.
   Took out the call to _XtWaitForSomething and replaced it with public
   Xt api's.
*/

Bool
extw_wait_for_response (Widget w, XEvent *event, unsigned long request_num,
			en_extw_notify type, unsigned long timeout)
{
  XtAppContext app = XtWidgetToApplicationContext(w);
  XtInputMask inputMask;
  QueryStruct q;
  Bool expired;
  XtIntervalId id;

  q.w = w;
  q.request_num = request_num;
  q.type = type;
  expired=FALSE;

  id=XtAppAddTimeOut(app, timeout, responseTimeOut,&expired);
  while (!expired)
    {
      inputMask=XtAppPending(app);
      if (inputMask & XtIMXEvent)
	{
	  XtAppNextEvent(app, event);
	  if (isMine(event,&q))
	    {
	      if (!expired) XtRemoveTimeOut(id);
	      return True;
	    }
	  else
	    {
	      /* Do Nothing and go back to waiting */
	    }
	}
      if (inputMask & XtIMTimer)
	{
	  /* Process the expired timer */
	  XtAppProcessEvent(app,XtIMTimer);
	}
    }
  /* Must have expired */
  return False;
}
