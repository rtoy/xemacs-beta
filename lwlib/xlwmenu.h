/* Implements a lightweight menubar widget.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

The Lucid Widget Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with the Lucid Widget Library.  If not, see
<http://www.gnu.org/licenses/>. */

#ifndef INCLUDED_xlwmenu_h_
#define INCLUDED_xlwmenu_h_

/***********************************************************************
 *
 * XlwMenu Widget
 *
 ***********************************************************************/

#include "lwlib.h"

/* Resource names used by the XlwMenu widget */
#define XtNbuttonForeground "buttonForeground"
#define XtCButtonForeground "ButtonForeground"
#define XtNhighlightForeground "highlightForeground"
#define XtCHighlightForeground "HighlightForeground"
#define XtNtitleForeground "titleForeground"
#define XtCTitleForeground "TitleForeground"
#define XtNmargin "margin"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"
#define XtNarrowSpacing "arrowSpacing"
#define XtNmenu "menu"
#define XtCMenu "Menu"
#define XtNopen "open"
#define XtNselect "select"
#define XtNmenuBorderWidth "menuBorderWidth"
#define XtNhorizontal "horizontal"
#define XtCHorizontal "Horizontal"
#ifndef XtNcursor
#define XtNcursor "cursor"
#endif
#ifndef XtCCursor
#define XtCCursor "Cursor"
#endif
#ifndef XtNuseBackingStore
#define XtNuseBackingStore "useBackingStore"
#endif
#ifndef XtCUseBackingStore
#define XtCUseBackingStore "UseBackingStore"
#endif
#define XtNbounceDown "bounceDown"
#define XtCBounceDown "BounceDown"
#define XtNresourceLabels "resourceLabels"
#define XtCResourceLabels "ResourceLabels"

/* Motif-compatible resource names */
#ifndef XmNshadowThickness
# define XmNshadowThickness	"shadowThickness"
# define XmCShadowThickness	"ShadowThickness"
# define XmNtopShadowColor	"topShadowColor"
# define XmCTopShadowColor	"TopShadowColor"
# define XmNbottomShadowColor	"bottomShadowColor"
# define XmCBottomShadowColor	"BottomShadowColor"
# define XmNtopShadowPixmap	"topShadowPixmap"
# define XmCTopShadowPixmap	"TopShadowPixmap"
# define XmNbottomShadowPixmap	"bottomShadowPixmap"
# define XmCBottomShadowPixmap	"BottomShadowPixmap"
# define XmUNSPECIFIED_PIXMAP   2
# define XmRHorizontalDimension	"HorizontalDimension"
# define XmNspacing		"spacing"
# define XmCSpacing		"Spacing"
# define XmNindicatorSize	"indicatorSize"
# define XmCIndicatorSize	"IndicatorSize"
# define XmNselectColor		"selectColor"
# define XmCSelectColor		"SelectColor"
# define XmNmarginHeight	"marginHeight"
# define XmCMarginHeight	"MarginHeight"
# define XmNmarginWidth		"marginWidth"
# define XmCMarginWidth		"MarginWidth"
# define XmRVerticalDimension	"VerticalDimension"
#endif

typedef struct _XlwMenuRec *XlwMenuWidget;
typedef struct _XlwMenuClassRec *XlwMenuWidgetClass;

extern WidgetClass xlwMenuWidgetClass;

void
xlw_pop_up_menu (XlwMenuWidget mw, XButtonPressedEvent* event);

/* menu accelerator */

void xlw_set_menu (Widget w, widget_value *val);
void xlw_push_menu (widget_value *val);
int xlw_pop_menu (void);
void xlw_set_item (widget_value *val);
void xlw_map_menu (Time t);
void xlw_display_menu (Time t);
void xlw_kill_menus (widget_value *val);
widget_value *xlw_get_entries (int allp);
int xlw_menu_level (void);

#endif /* INCLUDED_xlwmenu_h_ */
