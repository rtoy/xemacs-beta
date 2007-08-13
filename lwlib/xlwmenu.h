#ifndef _XlwMenu_h
#define _XlwMenu_h

/***********************************************************************
 *
 * XlwMenu Widget
 *
 ***********************************************************************/

#include "lwlib.h"

/* Resource names used by the XlwMenu widget */
#define XtNbuttonForeground (String) "buttonForeground"
#define XtCButtonForeground (String) "ButtonForeground"
#define XtNmargin (String) "margin"
#define XtNhorizontalSpacing (String) "horizontalSpacing"
#define XtNverticalSpacing (String) "verticalSpacing"
#define XtNarrowSpacing (String) "arrowSpacing"
#define XtNmenu (String) "menu"
#define XtCMenu (String) "Menu"
#define XtNopen (String) "open"
#define XtNselect (String) "select"
#define XtNmenuBorderWidth (String) "menuBorderWidth"
#define XtNhorizontal (String) "horizontal"
#define XtCHorizontal (String) "Horizontal"
#ifndef XtNcursor
#define XtNcursor (String) "cursor"
#endif
#ifndef XtCCursor
#define XtCCursor (String) "Cursor"
#endif
#ifndef XtNuseBackingStore
#define XtNuseBackingStore (String) "useBackingStore"
#endif
#ifndef XtCUseBackingStore
#define XtCUseBackingStore (String) "UseBackingStore"
#endif
#define XtNbounceDown (String) "bounceDown"
#define XtCBounceDown (String) "BounceDown"
#define XtNresourceLabels (String) "resourceLabels"
#define XtCResourceLabels (String) "ResourceLabels"

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

#endif /* _XlwMenu_h */
