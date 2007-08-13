/* Implements a lightweight scrollbar widget.  
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Created by Douglas Keller <dkeller@vnet.ibm.com> */

/*
 * Athena-style scrollbar button bindings added on Sun Dec 24 22:03:57 1995
 * by Jonathan Stigelman <Stig@hackvan.com>...   Ho ho ho!
 *
 * To use them, put this resource in your .Xdefaults
 *
 * Emacs*XlwScrollBar.translations: #override \n\
 *   <Btn1Down>:     PageDownOrRight()	  \n\
 *   <Btn3Down>:     PageUpOrLeft()
 * 
 */

/*
 * Resources Supported:
 *     XmNforeground
 *     XmNbackground
 *     XmNtopShadowColor
 *     XmNtopShadowPixmap
 *     XmNbottomShadowColor
 *     XmNbottomShadowPixmap
 *     XmNtroughColor
 *     XmNshadowThickness
 *     XmNshowArrows
 *     XmNorientation
 *     XmNborderWidth
 *
 *     XmNminimum
 *     XmNmaximum
 *     XmNvalue
 *     XmNincrement
 *     XmNpageIncrement
 *
 *     XmNvalueChangedCallback
 *     XmNincrementCallback
 *     XmNdecrementCallback
 *     XmNpageIncrementCallback
 *     XmNpageDecrementCallback
 *     XmNtoTopCallback
 *     XmNtoBottomCallback
 *     XmNdragCallback
 *
 *     XmNknobStyle      - values can be: "plain" or "dimple"
 *     XmNarrowPosition  - values can be: "opposite" or "same"
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/bitmaps/gray>

#include "xlwscrollbarP.h"
#include "xlwscrollbar.h"

#define DBUG(x)

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		  ? ((unsigned long) (x)) : ((unsigned long) (y)))

#define VERT(w) ((w)->sb.orientation == XmVERTICAL)

#define SS_MIN 8

#define ARROW_UP    0
#define ARROW_DOWN  1
#define ARROW_LEFT  2
#define ARROW_RIGHT 3

#define ARM_NONE   0
#define ARM_KNOB   1
#define ARM_UP     2
#define ARM_DOWN   3
#define ARM_PAGEUP 4
#define ARM_PAGEDOWN 5

#define BUTTON_NONE         0
#define BUTTON_KNOB         1
#define BUTTON_UP_ARROW     2
#define BUTTON_DOWN_ARROW   3
#define BUTTON_TROUGH_ABOVE 4
#define BUTTON_TROUGH_BELOW 5

#define KNOB_PLAIN  0
#define KNOB_DIMPLE 1

/************************************************************************
**
** Resources
**
*/
#define offset(field) XtOffset(XlwScrollBarWidget, field)

static XtResource resources[] = {
    { (String) XmNforeground, (String) XmCForeground, XtRPixel, sizeof(Pixel),
      offset(sb.foreground), XtRImmediate, (XtPointer) XtDefaultForeground },

    { (String) XmNtopShadowColor, (String) XmCTopShadowColor, XtRPixel,
      sizeof(Pixel), offset(sb.topShadowColor), XtRImmediate, (XtPointer) ~0 },
    { (String) XmNbottomShadowColor, (String) XmCBottomShadowColor, XtRPixel,
      sizeof(Pixel), offset(sb.bottomShadowColor), XtRImmediate,
      (XtPointer)~0 },

    { (String) XmNtopShadowPixmap, (String) XmCTopShadowPixmap, XtRPixmap,
      sizeof (Pixmap), offset(sb.topShadowPixmap), XtRImmediate,
      (XtPointer)None},
    { (String) XmNbottomShadowPixmap, (String) XmCBottomShadowPixmap,
      XtRPixmap, sizeof (Pixmap), offset(sb.bottomShadowPixmap),
      XtRImmediate, (XtPointer)None},

    { (String)XmNtroughColor, (String)XmCTroughColor, XtRPixel, sizeof(Pixel),
      offset(sb.troughColor), XtRImmediate, (XtPointer)~0 },

    { (String)XmNshadowThickness, (String)XmCShadowThickness, XtRInt,
      sizeof(int), offset(sb.shadowThickness), XtRImmediate, (XtPointer)2 },

    { (String) XmNborderWidth, (String) XmCBorderWidth, XtRDimension,
      sizeof(Dimension), offset(core.border_width), XtRImmediate,
      (XtPointer)0 },

    { (String) XmNshowArrows, (String) XmCShowArrows, XtRBoolean,
      sizeof(Boolean), offset(sb.showArrows), XtRImmediate, (XtPointer)True },

    { (String) XmNinitialDelay, (String) XmCInitialDelay, XtRInt, sizeof(int),
      offset(sb.initialDelay), XtRImmediate, (XtPointer) 250 },
    { (String) XmNrepeatDelay, (String) XmCRepeatDelay, XtRInt, sizeof(int),
      offset(sb.repeatDelay), XtRImmediate, (XtPointer) 50 },

    { (String) XmNorientation, (String) XmCOrientation, XtROrientation,
      sizeof(unsigned char), offset(sb.orientation), XtRImmediate,
      (XtPointer) XmVERTICAL },

    { (String) XmNminimum, (String) XmCMinimum, XtRInt, sizeof(int),
      offset(sb.minimum), XtRImmediate, (XtPointer) 0},
    { (String) XmNmaximum, (String) XmCMaximum, XtRInt, sizeof(int),
      offset(sb.maximum), XtRImmediate, (XtPointer) 100},
    { (String) XmNvalue, (String) XmCValue, XtRInt, sizeof(int),
      offset(sb.value), XtRImmediate, (XtPointer) 0},
    { (String) XmNsliderSize, (String) XmCSliderSize, XtRInt, sizeof(int),
      offset(sb.sliderSize), XtRImmediate, (XtPointer) 10},
    { (String) XmNincrement, (String) XmCIncrement, XtRInt, sizeof(int),
      offset(sb.increment), XtRImmediate, (XtPointer) 1},
    { (String)XmNpageIncrement, (String)XmCPageIncrement, XtRInt, sizeof(int),
      offset(sb.pageIncrement), XtRImmediate, (XtPointer) 10},

    { (String) XmNvalueChangedCallback, (String) XmCValueChangedCallback,
      XtRCallback, sizeof(XtPointer), offset(sb.valueChangedCBL),
      XtRCallback, NULL},
    { (String) XmNincrementCallback, (String) XmCIncrementCallback,
      XtRCallback, sizeof(XtPointer), offset(sb.incrementCBL),
      XtRCallback, NULL},
    { (String) XmNdecrementCallback, (String) XmCDecrementCallback,
      XtRCallback, sizeof(XtPointer), offset(sb.decrementCBL),
      XtRCallback, NULL},
    { (String) XmNpageIncrementCallback, (String) XmCPageIncrementCallback,
      XtRCallback, sizeof(XtPointer), offset(sb.pageIncrementCBL),
      XtRCallback, NULL},
    { (String) XmNpageDecrementCallback, (String) XmCPageDecrementCallback,
      XtRCallback, sizeof(XtPointer), offset(sb.pageDecrementCBL),
      XtRCallback, NULL},
    { (String) XmNtoTopCallback, (String) XmCToTopCallback, XtRCallback,
      sizeof(XtPointer), offset(sb.toTopCBL), XtRCallback, NULL},
    { (String) XmNtoBottomCallback, (String) XmCToBottomCallback, XtRCallback,
      sizeof(XtPointer), offset(sb.toBottomCBL), XtRCallback, NULL},
    { (String) XmNdragCallback, (String) XmCDragCallback, XtRCallback,
      sizeof(XtPointer), offset(sb.dragCBL), XtRCallback, NULL},

    { (String) XmNknobStyle, (String) XmCKnobStyle, XtRString, sizeof(char *),
      offset(sb.knobStyle), XtRImmediate, NULL},

    { (String) XmNarrowPosition, (String) XmCArrowPosition, XtRString,
      sizeof(char *), offset(sb.arrowPosition), XtRImmediate, NULL},
};

/************************************************************************
**
** Prototypes
**
*/

/*
** Actions
*/
static void Select(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void PageUpOrLeft(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void PageDownOrRight(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void Drag(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void Release(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void Jump(Widget w, XEvent *event, String *parms, Cardinal *num_parms);
static void Abort(Widget w, XEvent *event, String *parms, Cardinal *num_parms);

/*
** Methods
*/
static void Initialize(Widget treq, Widget tnew, ArgList args, Cardinal *num_args);
static Boolean SetValues(Widget current, Widget request, Widget nw, ArgList args, Cardinal *num_args);
static void Destroy(Widget widget);
static void Redisplay(Widget widget, XEvent *event, Region region);
static void Resize(Widget widget);
static void Realize(Widget widget, XtValueMask *valuemask, XSetWindowAttributes *attr);

/*
** Private
*/


/************************************************************************
**
** Actions Table
**
*/
static XtActionsRec actions[] = {
    {(String) "Select",		Select},
    {(String) "PageDownOrRight",PageDownOrRight},
    {(String) "PageUpOrLeft",	PageUpOrLeft},
    {(String) "Drag",		Drag},
    {(String) "Release",	Release},
    {(String) "Jump",		Jump},
    {(String) "Abort",		Abort},
};

/************************************************************************
**
** Default Translation Table
**
*/
static char default_translations[] =
    "<Btn1Down>:    Select()\n"
    "<Btn1Motion>:  Drag()\n"
    "<Btn1Up>:      Release()\n"
    "<Btn2Down>:    Jump()\n"
    "<Btn2Motion>:  Drag()\n"
    "<Btn2Up>:      Release()\n"
    "<Key>Delete:   Abort()"
;

/************************************************************************
**
** Class record initalization
**
*/
XlwScrollBarClassRec xlwScrollBarClassRec = {
    /* core_class fields */
    {
    /* superclass          */ (WidgetClass) &coreClassRec,
    /* class_name          */ (String) "XlwScrollBar",
    /* widget_size         */ sizeof(XlwScrollBarRec),
    /* class_initialize    */ NULL,
    /* class_part_init     */ NULL,
    /* class_inited        */ False,
    /* initialize          */ Initialize,
    /* initialize_hook     */ NULL,
    /* realize             */ Realize,
    /* actions             */ actions,
    /* num_actions         */ XtNumber(actions),
    /* resources           */ resources,
    /* num_resources       */ XtNumber(resources),
    /* xrm_class           */ NULLQUARK,
    /* compress_motion     */ True,
    /* compress_exposure   */ XtExposeCompressMultiple,
    /* compress_enterleave */ True,
    /* visible_interest    */ False,
    /* destroy             */ Destroy,
    /* resize              */ Resize,
    /* expose              */ Redisplay,
    /* set_values          */ SetValues,
    /* set_values_hook     */ NULL,
    /* set_values_almost   */ XtInheritSetValuesAlmost,
    /* get_values_hook     */ NULL,
    /* accept_focus        */ NULL,
    /* version             */ XtVersionDontCheck,
    /* callback_private    */ NULL,
    /* tm_table            */ default_translations,
    /* query_geometry      */ NULL,
    },
    /* scrollbar_class fields */
    {
        /* dummy_field         */ 0,
    },
};

WidgetClass xlwScrollBarWidgetClass = (WidgetClass) &xlwScrollBarClassRec;

/************************************************************************
**
** Debug functions
**
*/

#ifdef SHOW_CLEAR
static void
myXClearArea(Display *dpy, Drawable d, int x, int y, int w, int h,
	     Boolean exp, XlwScrollBarWidget widget)
{
  XFillRectangle (dpy, d, widget->sb.topShadowGC, x, y, w, h);
  XSync (dpy, False);
  sleep (2);
  XClearArea (dpy, d, x, y, w, h, exp);
}

#define XClearArea(dpy,win,x,y,width,height,exp) myXClearArea(dpy,win,x,y,width,height,exp,w)
#endif

#ifdef CHECK_VALUES
static void
check(XlwScrollBarWidget w)
{
  int height;
  
  height= widget_h (w);
  if (w->sb.showArrows) height -= (2 * arrow_h (w));

  if ((w->sb.above + w->sb.ss + w->sb.below > height) ||
      (w->sb.value < w->sb.minimum) ||
      (w->sb.value > w->sb.maximum - w->sb.sliderSize)
      )
    {
      printf("above=%d ss=%d below=%d height=%d\n", 
	     w->sb.above, w->sb.ss, w->sb.below, height);
      printf("value=%d min=%d max=%d ss=%d max-ss=%d\n",
	     w->sb.value, w->sb.minimum, w->sb.maximum,
	     w->sb.sliderSize, w->sb.maximum - w->sb.sliderSize);
      abort();
    }
}

#  define CHECK(w) check(w)
#else
#  define CHECK(w)
#endif

/************************************************************************
**
** Static functions
**
*/

static void
call_callbacks (XlwScrollBarWidget w, int reason,
		int value, int pixel, XEvent *event)
{
  XlwScrollBarCallbackStruct cbs;
  Boolean called_anything;

  cbs.reason = reason;
  cbs.event  = event;
  cbs.value  = value;
  cbs.pixel  = pixel;

  called_anything = False;

  switch (reason)
    {
    case XmCR_VALUE_CHANGED:
      XtCallCallbackList ((Widget) w, w->sb.valueChangedCBL, &cbs);
      called_anything = True;
      break;
    case XmCR_INCREMENT:
      if (w->sb.incrementCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.incrementCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_DECREMENT:
      if (w->sb.decrementCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.decrementCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_PAGE_INCREMENT:
      if (w->sb.incrementCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.pageIncrementCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_PAGE_DECREMENT:
      if (w->sb.decrementCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.pageDecrementCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_TO_TOP:
      if (w->sb.toTopCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.toTopCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_TO_BOTTOM:
      if (w->sb.toBottomCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.toBottomCBL, &cbs);
	  called_anything = True;
	}
      break;
    case XmCR_DRAG:
      if (w->sb.dragCBL)
	{
	  XtCallCallbackList ((Widget) w, w->sb.dragCBL, &cbs);
	}
      called_anything = True; /* Special Case */
      break;
    }

  if (!called_anything)
    {
      cbs.reason = XmCR_VALUE_CHANGED;
      XtCallCallbackList ((Widget) w, w->sb.valueChangedCBL, &cbs);
    }
}

/*
** Widget sizes minus the shadow and highlight area
*/
static int
widget_x (XlwScrollBarWidget w)
{
  return w->sb.shadowThickness;
}

static int
widget_y (XlwScrollBarWidget w)
{
  return w->sb.shadowThickness;
}

static int
widget_w (XlwScrollBarWidget w)
{
  int width, x = w->sb.shadowThickness;

  width = VERT (w) ? w->core.width : w->core.height;

  if (width <= (2 * x))
    return 1;
  else
    return width - (2 * x);
}

static int
widget_h (XlwScrollBarWidget w)
{
  int height, y = w->sb.shadowThickness;

  height = VERT (w) ? w->core.height : w->core.width;

  if (height <= (2 * y))
    return 1;
  else
    return height - (2 * y);
}

static int
arrow_h (XlwScrollBarWidget w)
{
  int width, height;

  width = widget_w (w);
  height= widget_h (w);

  if (width > ((height / 2) - (SS_MIN / 2) - 1))
    return     (height / 2) - (SS_MIN / 2) - 1 ;
  else
    return width;
}

static int
event_x (XlwScrollBarWidget w, XEvent *event)
{
  return VERT (w) ? event->xbutton.x : event->xbutton.y;
}

static int
event_y (XlwScrollBarWidget w, XEvent *event)
{
  return VERT (w) ? event->xbutton.y : event->xbutton.x;
}

/*
** Safe addition and subtraction
*/
static int
safe_add (int a, int b)
{
  if (a > 0 && INT_MAX - a < b) return INT_MAX;
  else return a + b;
}

static int
safe_subtract (int a, int b)
{
  if (a < 0 && -(INT_MIN - a) < b) return INT_MIN;
  else return a - b;
}

static int
knob_style (XlwScrollBarWidget w)
{
  return w->sb.knobStyle && w->sb.knobStyle[0] == 'd' ?
    KNOB_DIMPLE :
    KNOB_PLAIN;
}

static Boolean
arrow_same_end (XlwScrollBarWidget w)
{
  return w->sb.arrowPosition && w->sb.arrowPosition[0] == 's' ? True : False;
}

/*
** GC and Pixel allocation
*/
static GC
get_gc (XlwScrollBarWidget w, Pixel fg, Pixel bg, Pixmap pm)
{
  XGCValues values;
  XtGCMask mask;

  if (pm == w->sb.grayPixmap)
    {
      /* If we're using the gray pixmap, guarantee white on black ...
       * otherwise, we could end up with something odd like grey on white
       * when we're on a color display that ran out of color cells
       */

      fg = WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay (w)));
      bg = BlackPixelOfScreen (DefaultScreenOfDisplay (XtDisplay (w)));
    }
 
  values.foreground = fg;
  values.background = bg;
  values.fill_style = FillOpaqueStippled;
  values.stipple    = pm;
  mask              = GCForeground | GCBackground |
     (pm == None ? 0 : GCStipple | GCFillStyle);
  return XtGetGC((Widget) w, mask, &values);
}

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  From FSF Emacs. */

static int
allocate_nearest_color (Display *display, Colormap screen_colormap,
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
      unsigned long nearest_delta, trial_delta;

      int no_cells = XDisplayCells (display, XDefaultScreen (display));
      /* Don't use alloca here because lwlib doesn't have the
         necessary configuration information that src does. */
      XColor *cells = (XColor *) malloc (sizeof (XColor) * no_cells);

      for (x = 0; x < no_cells; x++)
	cells[x].pixel = x;

      XQueryColors (display, screen_colormap, cells, no_cells);

      for (x = 0; x < no_cells; x++)
	{
	  long dred   = (color_def->red   >> 8) - (cells[x].red   >> 8);
	  long dgreen = (color_def->green >> 8) - (cells[x].green >> 8);
	  long dblue  = (color_def->blue  >> 8) - (cells[x].blue  >> 8);
	  trial_delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (x == 0 || trial_delta < nearest_delta)
	    {
	      nearest = x;
	      nearest_delta = trial_delta;
	    }
	}
      color_def->red   = cells[nearest].red;
      color_def->green = cells[nearest].green;
      color_def->blue  = cells[nearest].blue;
      free (cells);
      return XAllocColor (display, screen_colormap, color_def);
    }
}

static void
make_shadow_pixels (XlwScrollBarWidget w)
{
  Display *dpy = XtDisplay((Widget) w);
  Colormap cmap = w->core.colormap;
  XColor topc, botc;
  int top_frobbed, bottom_frobbed;
  Pixel bg, fg;

  top_frobbed = bottom_frobbed = 0;

  bg = w->core.background_pixel;
  fg = w->sb.foreground;

  if (w->sb.topShadowColor    == (Pixel)~0) w->sb.topShadowColor    = bg;
  if (w->sb.bottomShadowColor == (Pixel)~0) w->sb.bottomShadowColor = fg;

  if (w->sb.topShadowColor == bg || w->sb.topShadowColor == fg)
    {
      topc.pixel = bg;
      XQueryColor (dpy, cmap, &topc);
      /* don't overflow/wrap! */
      topc.red   = MINL(65535, topc.red   * 1.2);
      topc.green = MINL(65535, topc.green * 1.2);
      topc.blue  = MINL(65535, topc.blue  * 1.2);
      if (allocate_nearest_color (dpy, cmap, &topc))
	{
	  if (topc.pixel == bg)
	    {
	      XFreeColors (dpy, cmap, &topc.pixel, 1, 0);
	      topc.red   = MINL(65535, topc.red   + 0x8000);
	      topc.green = MINL(65535, topc.green + 0x8000);
	      topc.blue  = MINL(65535, topc.blue  + 0x8000);
	      if (allocate_nearest_color (dpy, cmap, &topc))
		{
		  w->sb.topShadowColor = topc.pixel;
		}
	    }
	  else
	    {
	      w->sb.topShadowColor = topc.pixel;
	    }

	  top_frobbed = 1;
	}
    }

  if (w->sb.bottomShadowColor == fg || w->sb.bottomShadowColor == bg)
    {
      botc.pixel = bg;
      XQueryColor (dpy, cmap, &botc);
      botc.red   *= 0.6;
      botc.green *= 0.6;
      botc.blue  *= 0.6;
      if (allocate_nearest_color (dpy, cmap, &botc))
	{
	  if (botc.pixel == bg)
	    {
	      XFreeColors (dpy, cmap, &botc.pixel, 1, 0);
	      botc.red   = MINL(65535, botc.red   + 0x4000);
	      botc.green = MINL(65535, botc.green + 0x4000);
	      botc.blue  = MINL(65535, botc.blue  + 0x4000);
	      if (allocate_nearest_color (dpy, cmap, &botc))
		{
		  w->sb.bottomShadowColor = botc.pixel;
		}
	    }
	  else
	    {
	      w->sb.bottomShadowColor = botc.pixel;
	    }
	  bottom_frobbed = 1;
	}
    }

  if (top_frobbed && bottom_frobbed)
    {
      int top_avg = ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
      int bot_avg = ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
      if (bot_avg > top_avg)
	{
	  Pixel tmp = w->sb.topShadowColor;
	  w->sb.topShadowColor = w->sb.bottomShadowColor;
	  w->sb.bottomShadowColor = tmp;
	}
      else if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == bg)
	    w->sb.topShadowColor = bg;
	  else
	    w->sb.bottomShadowColor = fg;
	}
    }

  if (w->sb.topShadowColor    == w->core.background_pixel || 
      w->sb.bottomShadowColor == w->core.background_pixel)
    {
      /* Assume we're in mono. This code should be okay even if we're
       * really in color but just short on color cells -- We want the 
       * following behavior, which has been empirically determined to
       * work well for all fg/bg combinations in mono: If the trough
       * and thumb are BOTH black, then use a white top shadow and a
       * grey bottom shadow, otherwise use a grey top shadow and a
       * black bottom shadow.
       */

      Pixel white = WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay (w)));
      Pixel black = BlackPixelOfScreen (DefaultScreenOfDisplay (XtDisplay (w)));

      /* Note: core.background_pixel is the color of the thumb ... */

      if (w->core.background_pixel == black &&
	  w->sb.troughColor == black)
	{
	  w->sb.topShadowColor = white;
	  w->sb.bottomShadowPixmap = w->sb.grayPixmap;
	} else {
	  w->sb.topShadowPixmap = w->sb.grayPixmap;
	  w->sb.bottomShadowColor = black;
	}
    }
}

static void
make_trough_pixel (XlwScrollBarWidget w)
{
  Display *dpy = XtDisplay((Widget) w);
  Colormap cmap = DefaultColormapOfScreen (XtScreen ((Widget) w));
  XColor troughC;

  if (w->sb.troughColor == (Pixel)~0) w->sb.troughColor = w->core.background_pixel;

  if (w->sb.troughColor == w->core.background_pixel)
    {
      troughC.pixel = w->core.background_pixel;
      XQueryColor (dpy, cmap, &troughC);
      troughC.red   *= 0.8;
      troughC.green *= 0.8;
      troughC.blue  *= 0.8;
      if (allocate_nearest_color (dpy, cmap, &troughC))
	w->sb.troughColor = troughC.pixel;
    }
}

/*
** Draw 3d border
*/
static void
draw_shadows (Display *dpy, Drawable d, GC shine_gc, GC shadow_gc,
	      int x, int y, int width, int height, int shadowT)
{
  XSegment shine[10], shadow[10];
  int i;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) return;

  for (i = 0; i < shadowT; i++)
    {
      /*  Top segments  */
      shine[i].x1 = x;
      shine[i].y2 = shine[i].y1 = y + i;
      shine[i].x2 = x + width - i - 1;
      /*  Left segments  */
      shine[i + shadowT].x2 = shine[i + shadowT].x1 = x + i;
      shine[i + shadowT].y1 = y + shadowT;
      shine[i + shadowT].y2 = y + height - i - 1;

      /*  Bottom segments  */
      shadow[i].x1 = x + i;
      shadow[i].y2 = shadow[i].y1 = y + height - i - 1;
      shadow[i].x2 = x + width - 1 ;
      /*  Right segments  */
      shadow[i + shadowT].x2 = shadow[i + shadowT].x1 = x + width - i - 1;
      shadow[i + shadowT].y1 = y + i + 1;
      shadow[i + shadowT].y2 = y + height - 1 ;
    }

  XDrawSegments (dpy, d, shine_gc,  shine,  shadowT * 2);
  XDrawSegments (dpy, d, shadow_gc, shadow, shadowT * 2);
}

/*
** Draw 3d arrows, left, up, down, and right
*/
static int
make_vert_seg (XSegment *seg, int x1, int y1, int x2, int y2, int shadowT)
{
  int i;

  for (i=0; i<shadowT; i++)
    {
      seg[i].x1 = x1;
      seg[i].y1 = y1 + i;
      seg[i].x2 = x2;
      seg[i].y2 = y2 + i;
    }
  return shadowT;
}

static int
make_hor_seg (XSegment *seg, int x1, int y1, int x2, int y2, int shadowT)
{
  int i;

  for (i=0; i<shadowT; i++)
    {
      seg[i].x1 = x1 + i;
      seg[i].y1 = y1;
      seg[i].x2 = x2 + i;
      seg[i].y2 = y2;
    }
  return shadowT;
}

static void
draw_arrow_up (Display *dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
	       int x, int y, int width, int height, int shadowT)
{
  XSegment shine[10], shadow[10];
  XPoint triangle[3];
  int mid;

  mid = width / 2;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) shadowT = 0;

  /*  /  */
  make_vert_seg (shine,
		 x,       y + height - shadowT - 1,
		 x + mid, y, shadowT);
  /*  _\  */
  make_vert_seg (shadow,
		 x,             y + height - shadowT - 1,
		 x + width - 1, y + height - shadowT - 1, shadowT);
  make_vert_seg (shadow + shadowT,
		 x + mid,       y,
		 x + width - 1, y + height - shadowT - 1, shadowT);

  triangle[0].x = x;
  triangle[0].y = y + height - 1;
  triangle[1].x = x + mid;
  triangle[1].y = y;
  triangle[2].x = x + width - 1;
  triangle[2].y = y + height - 1;

  XFillPolygon (dpy, win, bgGC, triangle, 3, Convex, ArcChord);

  XDrawSegments (dpy, win, shadowGC, shadow, shadowT * 2);
  XDrawSegments (dpy, win, shineGC,  shine,  shadowT);
}

static void
draw_arrow_left (Display *dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		 int x, int y, int width, int height, int shadowT)
{
  XSegment shine[10], shadow[10];
  XPoint triangle[3];
  int mid;

  mid = width / 2;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) shadowT = 0;

  /*  /  */
  make_hor_seg (shine,
		x,         y + mid,
		x + width - shadowT - 1, y, shadowT);
  /*  \|  */
  make_hor_seg (shadow,
		x,         y + mid,
		x + width - shadowT - 1, y + height - 1, shadowT);
  make_hor_seg (shadow + shadowT,
		x + width - shadowT - 1, y,
		x + width - shadowT - 1, y + height - 1, shadowT);

  triangle[0].x = x + width - 1;
  triangle[0].y = y + height - 1;
  triangle[1].x = x;
  triangle[1].y = y + mid;
  triangle[2].x = x + width - 1;
  triangle[2].y = y;

  XFillPolygon (dpy, win, bgGC, triangle, 3, Convex, ArcChord);

  XDrawSegments (dpy, win, shadowGC, shadow, shadowT * 2);
  XDrawSegments (dpy, win, shineGC,  shine,  shadowT);
}

static void
draw_arrow_down (Display *dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		 int x, int y, int width, int height, int shadowT)
{
  XSegment shine[10], shadow[10];
  XPoint triangle[3];
  int mid;

  mid = width / 2;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) shadowT = 0;
      
  /*  \-  */
  make_vert_seg (shine,
		 x,       y,
		 x + mid, y + height - shadowT - 1, shadowT);
  make_vert_seg (shine + shadowT,
		 x,             y,
		 x + width - 1, y, shadowT);
  /*  /  */
  make_vert_seg (shadow,
		 x + width - 1, y,
		 x + mid,       y + height - shadowT - 1, shadowT);

  triangle[0].x = x;
  triangle[0].y = y;
  triangle[1].x = x + mid;
  triangle[1].y = y + height - 1;
  triangle[2].x = x + width - 1;
  triangle[2].y = y;

  XFillPolygon (dpy, win, bgGC, triangle, 3, Convex, ArcChord);

  XDrawSegments (dpy, win, shadowGC, shadow, shadowT);
  XDrawSegments (dpy, win, shineGC,  shine,  shadowT * 2);
}

static void
draw_arrow_right (Display *dpy, Drawable win, GC bgGC, GC shineGC, GC shadowGC,
		  int x, int y, int width, int height, int shadowT)
{
  XSegment shine[10], shadow[10];
  XPoint triangle[3];
  int mid;

  mid = width / 2;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) shadowT = 0;
      
  /*  |\  */
  make_hor_seg (shine,
		x,       y,
		x + width - shadowT - 1, y + mid, shadowT);
  make_hor_seg (shine + shadowT,
		x, y,
		x, y + height -1, shadowT);
  /*  /  */
  make_hor_seg (shadow,
		x, y + height -1,
		x + width - shadowT - 1, y + mid, shadowT);

  triangle[0].x = x + 1;
  triangle[0].y = y + height - 1;
  triangle[1].x = x + width - 1;
  triangle[1].y = y + mid;
  triangle[2].x = x + 1;
  triangle[2].y = y;

  XFillPolygon (dpy, win, bgGC, triangle, 3, Convex, ArcChord);

  XDrawSegments (dpy, win, shadowGC, shadow, shadowT);
  XDrawSegments (dpy, win, shineGC,  shine,  shadowT * 2);
}

static void
draw_dimple (Display *dpy, Drawable win, GC shine, GC shadow,
	     int x, int y, int width, int height)
{
  XDrawArc (dpy, win, shine,  x, y, width, height, 46*64, 180*64);
  XDrawArc (dpy, win, shadow, x, y, width, height, 45*64, -179*64);
}

/*
** Scrollbar values -> pixels, pixels -> scrollbar values
*/

static void
seg_pixel_sizes (XlwScrollBarWidget w, int *above_return,
		 int *ss_return, int *below_return)
{
  float total, height, fuz;
  int value;
  int above, ss, below;

  height= widget_h (w);
  if (w->sb.showArrows) height -= (2 * arrow_h (w));

  value = w->sb.value - w->sb.minimum;

  total = w->sb.maximum - w->sb.minimum;
  fuz   = total / 2;

  ss    = ((height * w->sb.sliderSize + fuz) / total);
  above = ((height * value + fuz) / total);
  below = ((height) - (ss + above));

  /* Dont' let knob get smaller than SS_MIN */
  if (ss < SS_MIN)
    {
      /* add a percent amount for integer rounding */
      float tmp = ((((float) (SS_MIN - ss) * (float) value)) / total) + 0.5;

      above -= (int) tmp;
      ss     = SS_MIN;
      below = ((height) - (ss + above));

      if (above < 0)
	{
	  above = 0;
	  below = height - ss;
	}
      if (below < 0)
	{
	  above = height - ss;
	  below = 0;
	}
      if (ss > height)
	{
	  above = 0;
	  ss    = height;
	  below = 0;
	}
    }

  *above_return = above;
  *ss_return    = ss;
  *below_return = below;

  CHECK (w);
}

static void
verify_values (XlwScrollBarWidget w)
{
  int total = w->sb.maximum - w->sb.minimum;

  if (w->sb.sliderSize > total)
      w->sb.sliderSize = total;

  if (w->sb.pageIncrement > total)
      w->sb.pageIncrement = total;

  if (w->sb.increment > total)
      w->sb.increment = total;

  if (w->sb.value < w->sb.minimum)
      w->sb.value = w->sb.minimum;

  if (w->sb.value > w->sb.maximum - w->sb.sliderSize)
      w->sb.value = w->sb.maximum - w->sb.sliderSize;

}

static int
value_from_pixel (XlwScrollBarWidget w, int above)
{
  float total, height, fuz;
  int value, ss;

  height= widget_h (w);
  if (w->sb.showArrows) height -= (2 * arrow_h (w));

  total = w->sb.maximum - w->sb.minimum;
  fuz = height / 2;

  ss = ((height * w->sb.sliderSize + (total / 2)) / total);

  if (ss < SS_MIN)
    {
      /* add a percent amount for integer rounding */
      above += ((((SS_MIN - ss) * above) + fuz) / height);
    }

  {
    /* Prevent SIGFPE's that would occur if we don't truncate the value. */
    float floatval = w->sb.minimum + ((float)(above * total + fuz) / height);
    if (floatval >= (float) INT_MAX)
      value = INT_MAX;
    else if (floatval <= (float) INT_MIN)
      value = INT_MIN;
    else
      value = floatval;
  }

  return value;
}


static void
redraw_dimple (XlwScrollBarWidget w, Display *dpy, Window win,
	       int x, int y, int width, int height)
{
  GC shine, shadow;
  int shadowT, size;

  if (KNOB_DIMPLE == knob_style (w))
    {
      if (w->sb.armed == ARM_KNOB)
	{
	  shine  = w->sb.bottomShadowGC;
	  shadow = w->sb.topShadowGC;
	}
      else
	{
	  shine  = w->sb.topShadowGC;
	  shadow = w->sb.bottomShadowGC;
	}

      shadowT = w->sb.shadowThickness;

      x += shadowT;
      y += shadowT;
      width  -= 2*shadowT;
      height -= 2*shadowT;

      size = (width < height ? width : height) * 3 / 4;

      if (size%2 != (width < height ? width : height)%2) size--;

      DBUG (fprintf (stderr, "%d %d\n",
		     x + (width / 2) - (size / 2) - 2*shadowT,
		     width - size - shadowT));

      draw_dimple (dpy, win, shine, shadow,
		   x + (width  / 2) - (size / 2),
		   y + (height / 2) - (size / 2),
		   size, size);
    }
}

static void
draw_knob (XlwScrollBarWidget w, int above, int ss, int below)
{
  Display *dpy = XtDisplay ((Widget) w);
  Window   win = XtWindow  ((Widget) w);
  int x, y, width, height;
  int shadowT;

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);

  shadowT = w->sb.shadowThickness;

  if (shadowT > (width  / 2)) shadowT = (width  / 2);
  if (shadowT > (height / 2)) shadowT = (height / 2);
  if (shadowT <= 0) return;

  if (w->sb.showArrows && !arrow_same_end (w)) y += arrow_h (w);

  /* trough above knob */
  if (above > 0)
    {
      if (VERT (w))
	XClearArea (dpy, win, x, y, width, above, False);
      else
	XClearArea (dpy, win, y, x, above, width, False);
    }

  /* knob */
  if (VERT (w))
    {
      draw_shadows (dpy, win, w->sb.topShadowGC, w->sb.bottomShadowGC,
		    x, y + above, width, ss, shadowT);
      XFillRectangle (dpy, win,
		      w->sb.backgroundGC,
		      x+shadowT, y + above + shadowT, width-2*shadowT, ss-2*shadowT);
      redraw_dimple (w, dpy, win, x, y + above, width, ss);
    }
  else
    {
      draw_shadows (dpy, win, w->sb.topShadowGC, w->sb.bottomShadowGC,
		    y + above, x, ss, width, shadowT);
      XFillRectangle (dpy, win,
		      w->sb.backgroundGC,
		      y + above + shadowT, x+shadowT, ss-2*shadowT, width-2*shadowT);
      redraw_dimple (w, dpy, win, y + above, x, ss, width);
    }

  /* trough below knob */
  if (below > 0)
    {
      if (VERT (w))
	XClearArea (dpy, win, x, y + above + ss, width, below, False);
      else
	XClearArea (dpy, win, y + above + ss, x, below, width, False);
    }

  CHECK (w);
}

static void
redraw_up_arrow (XlwScrollBarWidget w, Boolean armed, Boolean clear_behind)
{
  Display *dpy = XtDisplay ((Widget) w);
  Window   win = XtWindow  ((Widget) w);
  GC bg, shine, shadow;
  int x, y, width, height, arrow_height, shadowT;

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);
  arrow_height = arrow_h (w);

  shadowT = w->sb.shadowThickness;
  bg      = w->sb.backgroundGC;

  if (armed)
    {
      shine   = w->sb.bottomShadowGC;
      shadow  = w->sb.topShadowGC;
    }
  else
    {
      shine   = w->sb.topShadowGC;
      shadow  = w->sb.bottomShadowGC;
    }

  if (VERT (w))
    {
      if (arrow_same_end (w))
	{
	  y += height - 2 * arrow_h (w) + 2;
	}
      if (clear_behind)
	XClearArea (dpy, win, x, y, width, arrow_height + 1, False);
      draw_arrow_up (dpy, win, bg, shine, shadow,
		     x + (width - arrow_height)/2, y,
		     arrow_height, arrow_height, shadowT);
    }
  else
    {
      if (arrow_same_end (w))
	{
	  y += height - 2 * arrow_h (w);
	}
      if (clear_behind)
	XClearArea (dpy, win, y, x, arrow_height + 1, height, False);
      draw_arrow_left (dpy, win, bg, shine, shadow,
		       y, x + (width - arrow_height)/2,
		       arrow_height, arrow_height, shadowT);
    }
}

static void
redraw_down_arrow (XlwScrollBarWidget w, Boolean armed, Boolean clear_behind)
{
  Display *dpy = XtDisplay ((Widget) w);
  Window   win = XtWindow  ((Widget) w);
  GC bg, shine, shadow;
  int x, y, width, height, arrow_height, shadowT;

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);
  arrow_height = arrow_h (w);

  shadowT = w->sb.shadowThickness;
  bg      = w->sb.backgroundGC;

  if (armed)
    {
      shine  = w->sb.bottomShadowGC;
      shadow = w->sb.topShadowGC;
    }
  else
    {
      shine  = w->sb.topShadowGC;
      shadow = w->sb.bottomShadowGC;
    }

  if (VERT (w))
    {
      if (clear_behind)
	XClearArea (dpy, win, x, y + height - arrow_height, width,
		    arrow_height + 1, False);
      draw_arrow_down (dpy, win, bg, shine, shadow,
		       x + (width - arrow_height)/2,
		       y + height - arrow_height + 1,
		       arrow_height, arrow_height, shadowT);
    }
  else
    {
      if (clear_behind)
	XClearArea (dpy, win, y + height - arrow_height, x,
		    arrow_height + 1, height, False);
      draw_arrow_right (dpy, win, bg, shine, shadow,
			y + height - arrow_height + 1,
			x + (width - arrow_height)/2,
			arrow_height, arrow_height, shadowT);
    }
}

static void
redraw_everything (XlwScrollBarWidget w, Region region, Boolean behind_arrows)
{
  Display *dpy = XtDisplay ((Widget) w);
  Window   win = XtWindow  ((Widget) w);
  int x, y, width, height, shadowT, tmp;

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);
  shadowT = w->sb.shadowThickness;

  if (w->sb.showArrows)
    {
      if (region == NULL || XRectInRegion (region, x, y, width, width))
	{
	  redraw_up_arrow (w, False, behind_arrows);
	}
      if (VERT (w))
	{
	  y = y + height - width + 1;
	}
      else
	{
	  tmp = y;
	  y = x;
	  x = tmp + height - width + 1;
	}
      if (region == NULL || XRectInRegion (region, x, y, width, width))
	{
	  redraw_down_arrow (w, False, behind_arrows);
	}
    }

  draw_shadows (dpy, win, w->sb.bottomShadowGC, w->sb.topShadowGC,
		0, 0, w->core.width, w->core.height, shadowT);

  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

}

/************************************************************************
**
** Method functions
**
*/

static void
Initialize (Widget treq, Widget tnew, ArgList args, Cardinal *num_args)
{
  XlwScrollBarWidget request = (XlwScrollBarWidget) treq;
  XlwScrollBarWidget w = (XlwScrollBarWidget) tnew;
  Display *dpy = XtDisplay ((Widget) w);
  Window win = RootWindowOfScreen (DefaultScreenOfDisplay (dpy));

  DBUG (fprintf (stderr, "Initialize\n"));

  if (request->core.width  == 0) w->core.width  += (VERT (w) ? 12 : 25);
  if (request->core.height == 0) w->core.height += (VERT (w) ? 25 : 12);

  verify_values (w);

  w->sb.lastY = 0;
  w->sb.above = 0;
  w->sb.ss    = 0;
  w->sb.below = 0;
  w->sb.armed = ARM_NONE;

  if (w->sb.shadowThickness > 5) w->sb.shadowThickness = 5;

  w->sb.grayPixmap =
    XCreatePixmapFromBitmapData (dpy, win, (char *) gray_bits, gray_width,
				 gray_height, 1, 0, 1);

  make_trough_pixel (w);

  make_shadow_pixels (w);

  w->sb.backgroundGC =
    get_gc (w, w->core.background_pixel, w->core.background_pixel, None);
  w->sb.topShadowGC =
    get_gc (w, w->sb.topShadowColor, w->core.background_pixel,
	    w->sb.topShadowPixmap);
  w->sb.bottomShadowGC =
    get_gc (w, w->sb.bottomShadowColor, w->core.background_pixel,
	    w->sb.bottomShadowPixmap);

  w->sb.fullRedrawNext = True;

  w->sb.timerActive = False;
}

static void
Destroy (Widget widget)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  Display *dpy = XtDisplay ((Widget) w);

  DBUG (fprintf (stderr, "Destroy\n"));

  XtReleaseGC (widget, w->sb.bottomShadowGC);
  XtReleaseGC (widget, w->sb.topShadowGC);
  XtReleaseGC (widget, w->sb.backgroundGC);

  XFreePixmap (dpy, w->sb.grayPixmap);

  if (w->sb.timerActive)
    XtRemoveTimeOut (w->sb.timerId);
}

static void
Realize (Widget widget, XtValueMask *valuemask, XSetWindowAttributes *attr)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  Display *dpy = XtDisplay ((Widget) w);
  Window win;
  XSetWindowAttributes win_attr;

  DBUG (fprintf (stderr, "Realize\n"));

  (*coreClassRec.core_class.realize)(widget, valuemask, attr);

  win = XtWindow ((Widget) w);

  seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);

  XSetWindowBackground (dpy, win, w->sb.troughColor);

  /* Change bit gravity so widget is not cleared on resize */
  win_attr.bit_gravity   = NorthWestGravity;
  XChangeWindowAttributes (dpy, win, CWBitGravity , &win_attr);

}

static void
Resize (Widget widget)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  Display *dpy = XtDisplay ((Widget) w);
  Window win   = XtWindow  ((Widget) w);

  if (XtIsRealized (widget))
    {
      DBUG (fprintf (stderr, "Resize = %08lx\n", w));

      seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);

      /*redraw_everything(w, NULL, True);*/

      w->sb.fullRedrawNext = True;
      /* Force expose event */
      XClearArea (dpy, win, widget_x(w), widget_y(w), 1, 1, True);
    }
}

static void
Redisplay (Widget widget, XEvent *event, Region region)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

  DBUG (fprintf (stderr, "Redisplay = %08lx\n", w));

  if (XtIsRealized (widget))
    {
      if (w->sb.fullRedrawNext)
	{
	  redraw_everything (w, NULL, True);
	}
      else
	{
	  redraw_everything (w, region, False);
	}
      w->sb.fullRedrawNext = False;
    }
}

static Boolean
SetValues (Widget current, Widget request, Widget neww,
	   ArgList args, Cardinal *num_args)
{
  XlwScrollBarWidget cur = (XlwScrollBarWidget) current;
  XlwScrollBarWidget w = (XlwScrollBarWidget) neww;
  Boolean do_redisplay = False;

  if (cur->sb.troughColor != w->sb.troughColor)
    {
      if (XtIsRealized ((Widget) w))
	{
	  XSetWindowBackground (XtDisplay((Widget) w), XtWindow ((Widget) w),
				w->sb.troughColor);
	  do_redisplay = True;
	}
    }

  if (cur->core.background_pixel != w->core.background_pixel)
    {
      XtReleaseGC ((Widget)cur, cur->sb.backgroundGC);
      w->sb.backgroundGC =
	get_gc (w, w->core.background_pixel, w->core.background_pixel, None);
      do_redisplay = True;
    }

  if (cur->sb.topShadowColor != w->sb.topShadowColor ||
      cur->sb.topShadowPixmap != w->sb.topShadowPixmap)
    {
      XtReleaseGC ((Widget)cur, cur->sb.topShadowGC);
      w->sb.topShadowGC =
	get_gc (w, w->sb.topShadowColor, w->core.background_pixel,
		w->sb.topShadowPixmap);
      do_redisplay = True;
    }

  if (cur->sb.bottomShadowColor != w->sb.bottomShadowColor ||
      cur->sb.bottomShadowPixmap != w->sb.bottomShadowPixmap)
    {
      XtReleaseGC ((Widget)cur, cur->sb.bottomShadowGC);
      w->sb.bottomShadowGC =
	get_gc (w, w->sb.bottomShadowColor, w->core.background_pixel,
		w->sb.bottomShadowPixmap);
      do_redisplay = True;
    }

  if (cur->sb.orientation != w->sb.orientation)
    {
      do_redisplay = True;
    }


  if (cur->sb.minimum       != w->sb.minimum       ||
      cur->sb.maximum       != w->sb.maximum       ||
      cur->sb.sliderSize    != w->sb.sliderSize    ||
      cur->sb.value         != w->sb.value         ||
      cur->sb.pageIncrement != w->sb.pageIncrement ||
      cur->sb.increment     != w->sb.increment)
    {
      verify_values (w);
      if (XtIsRealized ((Widget) w))
	{
	  seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);
	}
    }

  if (w->sb.shadowThickness > 5) w->sb.shadowThickness = 5;

  return do_redisplay;
}

void
XlwScrollBarGetValues (Widget widget, int *value, int *sliderSize,
		       int *increment, int *pageIncrement)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

  if (w && XtClass ((Widget) w) == xlwScrollBarWidgetClass)
    {
      if (value)         *value         = w->sb.value;
      if (sliderSize)    *sliderSize    = w->sb.sliderSize;
      if (increment)     *increment     = w->sb.increment;
      if (pageIncrement) *pageIncrement = w->sb.pageIncrement;
    }
}

void
XlwScrollBarSetValues (Widget widget, int value, int sliderSize,
		       int increment, int pageIncrement, Boolean notify)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  int last_value;

  if (w && XtClass ((Widget) w) == xlwScrollBarWidgetClass &&
      (w->sb.value         != value         ||
       w->sb.sliderSize    != sliderSize    ||
       w->sb.increment     != increment     ||
       w->sb.pageIncrement != pageIncrement))
    {
      w->sb.value         = value;
      w->sb.sliderSize    = sliderSize;
      w->sb.increment     = increment;
      w->sb.pageIncrement = pageIncrement;

      verify_values (w);

      if (XtIsRealized (widget))
	{
	  seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

	  last_value  = w->sb.value;
	  w->sb.value = value_from_pixel (w, w->sb.above);
	  verify_values (w);

	  if (w->sb.value != last_value && notify)
	    {
	      call_callbacks (w, XmCR_VALUE_CHANGED, w->sb.value, 0, NULL);
	    }
	}
    }
}

/************************************************************************
**
** Action functions
**
*/

static void
timer (XtPointer data, XtIntervalId *id)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) data;
  int reason, last_value;
  
  w->sb.timerActive = False;

  if (w->sb.armed != ARM_NONE)
    {
      last_value = w->sb.value;
      reason     = XmCR_NONE;

      switch (w->sb.armed)
	{
	case ARM_PAGEUP:
	  w->sb.value = safe_subtract (w->sb.value, w->sb.pageIncrement);
	  reason = XmCR_PAGE_DECREMENT;
	  break;
	case ARM_PAGEDOWN:
	  w->sb.value = safe_add (w->sb.value, w->sb.pageIncrement);
	  reason = XmCR_PAGE_INCREMENT;
	  break;
	case ARM_UP:
	  w->sb.value = safe_subtract (w->sb.value, w->sb.increment);
	  reason = XmCR_DECREMENT;
	  break;
	case ARM_DOWN:
	  w->sb.value = safe_add (w->sb.value, w->sb.increment);
	  reason = XmCR_INCREMENT;
	  break;
	}

      verify_values (w);

      if (last_value != w->sb.value)
	{
	  seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

	  call_callbacks (w, reason, w->sb.value, 0, NULL);

	  w->sb.timerId =
	    XtAppAddTimeOut (XtWidgetToApplicationContext ((Widget) w),
			     (unsigned long) w->sb.repeatDelay,
			     timer,  (XtPointer) w);
	  w->sb.timerActive = True;
	}
    }
}

static int
what_button (XlwScrollBarWidget w, int mouse_x, int mouse_y)
{
  int x, y, width, height, arrow_height_top, arrow_height_bottom;
  int where;

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);

#if 0
  arrow_height = w->sb.showArrows ? arrow_h (w) : 0;
#endif
  if (w->sb.showArrows)
    {
      if (arrow_same_end (w))
	{
	  arrow_height_top = 0;
	  arrow_height_bottom = 2 * arrow_h (w);
	}
      else
	{
	  arrow_height_top = arrow_height_bottom = arrow_h (w);
	}
    }
  else
    {
      arrow_height_top = arrow_height_bottom = 0;
    }

  where = BUTTON_NONE;

  if (mouse_x > x && mouse_x < (x + width))
    {
      if (mouse_y > (y + arrow_height_top) &&
	  mouse_y < (y + height - arrow_height_bottom))
	{
	  if (mouse_y < (y + w->sb.above + arrow_height_top))
	    {
	      where = BUTTON_TROUGH_ABOVE;
	    }
	  else if (mouse_y > (y + w->sb.above + w->sb.ss + arrow_height_top))
	    {
	      where = BUTTON_TROUGH_BELOW;
	    }
	  else
	    {
	      where = BUTTON_KNOB;
	    }
	}
      else if (arrow_same_end (w))
	{
	  if (mouse_y > (y + height - arrow_height_bottom + 1) &&
	      mouse_y < (y + height))
	    {
	      if (mouse_y < (y + height - arrow_height_bottom/2))
		{
		  where = BUTTON_UP_ARROW;
		}
	      else
		{
		  where = BUTTON_DOWN_ARROW;
		}
	    }
	}
      else
	{
	  if (mouse_y > y && mouse_y < (y + arrow_height_top))
	    {
	      where = BUTTON_UP_ARROW;
	    }
	  else if (mouse_y > (y + height - arrow_height_bottom + 1) &&
		   mouse_y < (y + height))
	    {
	      where = BUTTON_DOWN_ARROW;
	    }
	}
    }
#if 0
  if (mouse_x > x && mouse_x < (x + width))
    {
      if (mouse_y > (y + arrow_height) && mouse_y < (y + height - arrow_height))
	{
	  if (mouse_y < (y+w->sb.above+arrow_height))
	    {
	      where = BUTTON_TROUGH_ABOVE;
	    }
	  else if (mouse_y > (y + w->sb.above + w->sb.ss + arrow_height))
	    {
	      where = BUTTON_TROUGH_BELOW;
	    }
	  else
	    {
	      where = BUTTON_KNOB;
	    }
	}
      else if (mouse_y > y && mouse_y < (y + arrow_height))
	{
	  where = BUTTON_UP_ARROW;
	}
      else if (mouse_y > (y + height - arrow_height + 1) &&
	       mouse_y < (y + height))
	{
	  where = BUTTON_DOWN_ARROW;
	}
    }
#endif
  return where;
}

#define FORCED_SCROLL_NONE	0
#define FORCED_SCROLL_DOWNRIGHT	1
#define FORCED_SCROLL_UPLEFT	2

int forced_scroll_flag = FORCED_SCROLL_NONE;

static void
PageDownOrRight (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  forced_scroll_flag = FORCED_SCROLL_DOWNRIGHT;
  Select (widget, event, parms, num_parms);
  forced_scroll_flag = FORCED_SCROLL_NONE;
}

static void
PageUpOrLeft (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  forced_scroll_flag = FORCED_SCROLL_UPLEFT;
  Select (widget, event, parms, num_parms);
  forced_scroll_flag = FORCED_SCROLL_NONE;
}

static void
Select (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  int mouse_x, mouse_y;
  int reason, last_value;
  int sb_button;

  DBUG (fprintf (stderr, "Select:\n"));

  mouse_x = event_x (w, event);
  mouse_y = event_y (w, event);

  w->sb.savedValue = w->sb.value;

  last_value = w->sb.value;
  reason     = XmCR_NONE;

  XtGrabKeyboard ((Widget) w, False, GrabModeAsync, GrabModeAsync,
		  event->xbutton.time);

  sb_button = what_button (w, mouse_x, mouse_y);

  if (forced_scroll_flag != FORCED_SCROLL_NONE)
    {
      switch  (sb_button)
	{
	case BUTTON_TROUGH_ABOVE:
	case BUTTON_TROUGH_BELOW:
	case BUTTON_KNOB:
	  sb_button= BUTTON_NONE; /* cause next switch to fall through */
	  if (forced_scroll_flag == FORCED_SCROLL_UPLEFT)
	    {
	      w->sb.value = safe_subtract (w->sb.value, w->sb.pageIncrement);
	      w->sb.armed = ARM_PAGEUP;
	      reason      = XmCR_PAGE_DECREMENT;
	      break;
	    }
	  else if (forced_scroll_flag == FORCED_SCROLL_DOWNRIGHT)
	    {
	      w->sb.value = safe_add (w->sb.value, w->sb.pageIncrement);
	      w->sb.armed = ARM_PAGEDOWN;
	      reason      = XmCR_PAGE_INCREMENT;
	      break;
	    }
	  abort();
	}
    }

  switch (sb_button)
    {
    case BUTTON_TROUGH_ABOVE:
      w->sb.value = safe_subtract (w->sb.value, w->sb.pageIncrement);
      w->sb.armed = ARM_PAGEUP;
      reason      = XmCR_PAGE_DECREMENT;
      break;
    case BUTTON_TROUGH_BELOW:
      w->sb.value = safe_add (w->sb.value, w->sb.pageIncrement);
      w->sb.armed = ARM_PAGEDOWN;
      reason      = XmCR_PAGE_INCREMENT;
      break;
    case BUTTON_KNOB:
      w->sb.lastY = mouse_y;
      w->sb.armed = ARM_KNOB;
      draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);
      break;
    case BUTTON_UP_ARROW:
      if (event->xbutton.state & ControlMask)
	{
	  w->sb.value = INT_MIN;
	  w->sb.armed = ARM_UP;
	  reason      = XmCR_TO_TOP;
	}
      else
	{
	  w->sb.value = safe_subtract (w->sb.value, w->sb.increment);
	  w->sb.armed = ARM_UP;
	  reason      = XmCR_DECREMENT;
	}
      redraw_up_arrow (w, True, False);
      break;
    case BUTTON_DOWN_ARROW:
      if (event->xbutton.state & ControlMask)
	{
	  w->sb.value = INT_MAX;
	  w->sb.armed = ARM_DOWN;
	  reason      = XmCR_TO_BOTTOM;
	}
      else
	{
	  w->sb.value = safe_add (w->sb.value, w->sb.increment);
	  w->sb.armed = ARM_DOWN;
	  reason      = XmCR_INCREMENT;
	}
      redraw_down_arrow (w, True, False);
      break;
    }

  verify_values (w);

  if (last_value != w->sb.value)
    {
      seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);
      draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);
	  
      call_callbacks (w, reason, w->sb.value, mouse_y, event);

      if (w->sb.timerActive)
	XtRemoveTimeOut (w->sb.timerId);

      w->sb.timerId =
	XtAppAddTimeOut (XtWidgetToApplicationContext ((Widget) w),
			 (unsigned long) w->sb.initialDelay,
			 timer,  (XtPointer) w);
      w->sb.timerActive = True;
    }

  CHECK (w);
}

static void
Drag (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  int diff;
  int height, mouse_y;
  int last_value, last_above;

  DBUG (fprintf (stderr, "Drag:\n"));

  if (w->sb.armed == ARM_KNOB)
    {
      height  = widget_h (w);
      if (w->sb.showArrows) height -= (2 * arrow_h (w));

      mouse_y = event_y (w, event);

      diff = mouse_y - w->sb.lastY;

      last_above = w->sb.above;
      last_value = w->sb.value;

      if (diff < 0)
	{
	  /* up */
	  w->sb.above -= (-diff);
	  if (w->sb.above < 0)
	    {
	      mouse_y = (mouse_y - w->sb.above);
	      w->sb.above = 0;
	      diff = 0;
	      w->sb.below = height - w->sb.ss;
	    }
	  w->sb.below -= diff;
	  CHECK (w);
	}
      else if (diff > 0)
	{
	  /* down */
	  w->sb.above += diff;
	  if (w->sb.above + w->sb.ss > height)
	    {
	      mouse_y = height + (mouse_y - (w->sb.above + w->sb.ss));
	      w->sb.above = height - w->sb.ss;
	      diff = 0;
	      w->sb.below = 0;
	    }
	  w->sb.below -= diff;
	  CHECK (w);
	}

      if (last_above != w->sb.above)
	{
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

	  w->sb.lastY = mouse_y;

	  w->sb.value = value_from_pixel (w, w->sb.above);
	  verify_values (w);
	  CHECK (w);

	  if (w->sb.value != last_value)
	    call_callbacks (w, XmCR_DRAG, w->sb.value, event_y (w, event), event);
	}
    }
  CHECK (w);
}

static void
Release (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

  DBUG (fprintf (stderr, "EndDrag:\n"));

  switch (w->sb.armed)
    {
    case ARM_KNOB:
      call_callbacks (w, XmCR_VALUE_CHANGED, w->sb.value, event_y (w, event), event);
      w->sb.armed = ARM_NONE;
      draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);
      break;
    case ARM_UP:
      redraw_up_arrow (w, False, False);
      break;
    case ARM_DOWN:
      redraw_down_arrow (w, False, False);
      break;
    }

  XtUngrabKeyboard ((Widget) w, event->xbutton.time);

  w->sb.armed = ARM_NONE;
}

static void
Jump (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;
  int x, y, width, height, mouse_x, mouse_y;
  int arrow_height;
  int last_above, last_value;

  DBUG (fprintf (stderr, "Jump:\n"));

  x       = widget_x (w);
  y       = widget_y (w);
  width   = widget_w (w);
  height  = widget_h (w);
  
  mouse_x = event_x (w, event);
  mouse_y = event_y (w, event);

  arrow_height = w->sb.showArrows ? arrow_h (w) : 0;

  XtGrabKeyboard ((Widget) w, False, GrabModeAsync, GrabModeAsync,
		  event->xbutton.time);

  switch (what_button (w, mouse_x, mouse_y))
    {
    case BUTTON_TROUGH_ABOVE:
    case BUTTON_TROUGH_BELOW:
    case BUTTON_KNOB:
      w->sb.savedValue = w->sb.value;

      height -= (2*arrow_height);
      y      += arrow_height;

      last_above = w->sb.above;
      last_value = w->sb.value;

      w->sb.armed = ARM_KNOB;
      draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

      w->sb.above = mouse_y - (w->sb.ss / 2) - arrow_height;
      if (w->sb.above < 0)
	{
	  w->sb.above = 0;
	}
      else if (w->sb.above + w->sb.ss > height)
	{
	  w->sb.above = height - w->sb.ss;
	}
      w->sb.below = (height - (w->sb.ss + w->sb.above));

      if (last_above != w->sb.above)
	{
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

	  w->sb.value = value_from_pixel (w, w->sb.above);
	  verify_values (w);
	  CHECK (w);

	  w->sb.lastY = mouse_y;
	  w->sb.lastY = w->sb.above + arrow_height + (w->sb.ss / 2);

	  if (w->sb.value != last_value)
	    {
	      call_callbacks (w, XmCR_DRAG, w->sb.value, event_y (w, event), event);
	    }
	}
      break;
    }
  CHECK (w);
}

static void
Abort (Widget widget, XEvent *event, String *parms, Cardinal *num_parms)
{
  XlwScrollBarWidget w = (XlwScrollBarWidget) widget;

  DBUG (fprintf (stderr, "Abort:\n"));

  if (w->sb.armed != ARM_NONE)
    {
      if (w->sb.value != w->sb.savedValue)
	{
	  w->sb.value = w->sb.savedValue;

	  seg_pixel_sizes (w, &w->sb.above, &w->sb.ss, &w->sb.below);
	  draw_knob (w, w->sb.above, w->sb.ss, w->sb.below);

	  call_callbacks (w, XmCR_VALUE_CHANGED, w->sb.value,
			  event_y (w, event), event);
	}

      switch (w->sb.armed)
	{
	case ARM_UP:   redraw_up_arrow   (w, False, False); break;
	case ARM_DOWN: redraw_down_arrow (w, False, False); break;
	}

      w->sb.armed = ARM_NONE;

      XtUngrabKeyboard ((Widget) w, event->xbutton.time);
    }
}
