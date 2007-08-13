/* Public header for the Emacs frame widget.
   Copyright (C) 1993-1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

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

#ifndef _EmacsFrame_h
#define _EmacsFrame_h

#ifndef XtNminibuffer
#define XtNminibuffer (String) "minibuffer"
#endif
#ifndef XtCMinibuffer
#define XtCMinibuffer (String) "Minibuffer"
#endif

#ifndef XtNunsplittable
#define XtNunsplittable (String) "unsplittable"
#endif
#ifndef XtCUnsplittable
#define XtCUnsplittable (String) "Unsplittable"
#endif

#ifndef XtNinternalBorderWidth
#define XtNinternalBorderWidth (String) "internalBorderWidth"
#endif
#ifndef XtCInternalBorderWidth
#define XtCInternalBorderWidth (String) "InternalBorderWidth"
#endif

#ifndef XtNscrollBarWidth
#define XtNscrollBarWidth (String) "scrollBarWidth"
#endif
#ifndef XtCScrollBarWidth
#define XtCScrollBarWidth (String) "ScrollBarWidth"
#endif

#ifndef XtNscrollBarHeight
#define XtNscrollBarHeight (String) "scrollBarHeight"
#endif
#ifndef XtCScrollBarHeight
#define XtCScrollBarHeight (String) "ScrollBarHeight"
#endif

#ifndef XtNtopToolBarHeight
#define XtNtopToolBarHeight (String) "topToolBarHeight"
#endif
#ifndef XtCTopToolBarHeight
#define XtCTopToolBarHeight (String) "TopToolBarHeight"
#endif

#ifndef XtNbottomToolBarHeight
#define XtNbottomToolBarHeight (String) "bottomToolBarHeight"
#endif
#ifndef XtCBottomToolBarHeight
#define XtCBottomToolBarHeight (String) "BottomToolBarHeight"
#endif

#ifndef XtNleftToolBarWidth
#define XtNleftToolBarWidth (String) "leftToolBarWidth"
#endif
#ifndef XtCLeftToolBarWidth
#define XtCLeftToolBarWidth (String) "LeftToolBarWidth"
#endif

#ifndef XtNrightToolBarWidth
#define XtNrightToolBarWidth (String) "rightToolBarWidth"
#endif
#ifndef XtCRightToolBarWidth
#define XtCRightToolBarWidth (String) "RightToolBarWidth"
#endif

#ifndef XtNtopToolBarShadowColor
#define XtNtopToolBarShadowColor (String) "topToolBarShadowColor"
#endif
#ifndef XtCTopToolBarShadowColor
#define XtCTopToolBarShadowColor (String) "TopToolBarShadowColor"
#endif

#ifndef XtNbottomToolBarShadowColor
#define XtNbottomToolBarShadowColor (String) "bottomToolBarShadowColor"
#endif
#ifndef XtCBottomToolBarShadowColor
#define XtCBottomToolBarShadowColor (String) "BottomToolBarShadowColor"
#endif

#ifndef XtNbackgroundToolBarColor
#define XtNbackgroundToolBarColor (String) "backgroundToolBarColor"
#endif
#ifndef XtCBackgroundToolBarColor
#define XtCBackgroundToolBarColor (String) "BackgroundToolBarColor"
#endif

#ifndef XtNtopToolBarShadowPixmap
#define XtNtopToolBarShadowPixmap (String) "topToolBarShadowPixmap"
#endif
#ifndef XtCTopToolBarShadowPixmap
#define XtCTopToolBarShadowPixmap (String) "TopToolBarShadowPixmap"
#endif

#ifndef XtNbottomToolBarShadowPixmap
#define XtNbottomToolBarShadowPixmap (String) "bottomToolBarShadowPixmap"
#endif
#ifndef XtCBottomToolBarShadowPixmap
#define XtCBottomToolBarShadowPixmap (String) "BottomToolBarShadowPixmap"
#endif

#ifndef XtNtoolBarShadowThickness
#define XtNtoolBarShadowThickness (String) "toolBarShadowThickness"
#endif
#ifndef XtCToolBarShadowThickness
#define XtCToolBarShadowThickness (String) "ToolBarShadowThickness"
#endif

#ifndef XtNscrollBarPlacement
#define XtNscrollBarPlacement (String) "scrollBarPlacement"
#endif
#ifndef XtCScrollBarPlacement
#define XtCScrollBarPlacement (String) "ScrollBarPlacement"
#endif
#ifndef XtRScrollBarPlacement
#define XtRScrollBarPlacement (String) "ScrollBarPlacement"
#endif

#ifndef XtNinterline
#define XtNinterline (String) "interline"
#endif
#ifndef XtCInterline
#define XtCInterline (String) "Interline"
#endif

#ifndef XtNfont
#define XtNfont (String) "font"
#endif
#ifndef XtCFont
#define XtCFont (String) "Font"
#endif

#ifndef XtNforeground
#define XtNforeground (String) "foreground"
#endif
#ifndef XtCForeground
#define XtCForeground (String) "Foreground"
#endif

#ifndef XtNiconic
#define XtNiconic (String) "iconic"
#endif
#ifndef XtCIconic
#define XtCIconic (String) "Iconic"
#endif

#ifndef XtNcursorColor
#define XtNcursorColor (String) "cursorColor"
#endif
#ifndef XtCCursorColor
#define XtCCursorColor (String) "CursorColor"
#endif

#ifndef XtNbarCursor
#define XtNbarCursor (String) "barCursor"
#endif
#ifndef XtCBarCursor
#define XtCBarCursor (String) "BarCursor"
#endif

#ifndef XtNvisualBell
#define XtNvisualBell (String) "visualBell"
#endif
#ifndef XtCVisualBell
#define XtCVisualBell (String) "VisualBell"
#endif

#ifndef XtNbellVolume
#define XtNbellVolume (String) "bellVolume"
#endif
#ifndef XtCBellVolume
#define XtCBellVolume (String) "BellVolume"
#endif

#ifndef XtNpointerBackground
#define XtNpointerBackground (String) "pointerBackground"
#endif

#ifndef XtNpointerColor
#define XtNpointerColor "pointerColor"
#endif

#ifndef XtNtextPointer
#define XtNtextPointer (String) "textPointer"
#endif

#ifndef XtNspacePointer
#define XtNspacePointer (String) "spacePointer"
#endif

#ifndef XtNmodeLinePointer
#define XtNmodeLinePointer (String) "modePointer"
#endif

#ifndef XtNgcPointer
#define XtNgcPointer (String) "gcPointer"
#endif

#ifndef XtNemacsFrame
#define XtNemacsFrame (String) "emacsFrame"
#endif
#ifndef XtCEmacsFrame
#define XtCEmacsFrame (String) "EmacsFrame"
#endif

#ifndef XtNgeometry
#define XtNgeometry (String) "geometry"
#endif
#ifndef XtCGeometry
#define XtCGeometry (String) "Geometry"
#endif

#ifndef XtNinitialGeometry
#define XtNinitialGeometry (String) "initialGeometry"
#endif
#ifndef XtCInitialGeometry
#define XtCInitialGeometry (String) "InitialGeometry"
#endif

#ifndef XtNmenubar
#define XtNmenubar (String) "menubar"
#endif
#ifndef XtCMenubar
#define XtCMenubar (String) "Menubar"
#endif

#ifndef XtNinitiallyUnmapped
#define XtNinitiallyUnmapped (String) "initiallyUnmapped"
#endif
#ifndef XtCInitiallyUnmapped
#define XtCInitiallyUnmapped (String) "InitiallyUnmapped"
#endif

#ifndef XtNpreferredWidth
#define XtNpreferredWidth (String) "preferredWidth"
#endif
#ifndef XtCPreferredWidth
#define XtCPreferredWidth (String) "PreferredWidth"
#endif

#ifndef XtNpreferredHeight
#define XtNpreferredHeight (String) "preferredHeight"
#endif
#ifndef XtCPreferredHeight
#define XtCPreferredHeight (String) "PreferredHeight"
#endif

#ifndef XtNuseBackingStore
#define XtNuseBackingStore (String) "useBackingStore"
#endif
#ifndef XtCUseBackingStore
#define XtCUseBackingStore (String) "UseBackingStore"
#endif

/* scrollbar placement types; like in ScrolledW.h */
#define EM_TOP		1
#define EM_BOTTOM	0
#define EM_LEFT		2
#define EM_RIGHT	0
 
#define XtTOP_LEFT	(EM_TOP    | EM_LEFT)
#define XtBOTTOM_LEFT	(EM_BOTTOM | EM_LEFT)
#define XtTOP_RIGHT	(EM_TOP    | EM_RIGHT)
#define XtBOTTOM_RIGHT	(EM_BOTTOM | EM_RIGHT)
 
/* structures */
typedef struct _EmacsFrameRec *EmacsFrame;
typedef struct _EmacsFrameClassRec *EmacsFrameClass;

extern WidgetClass emacsFrameClass;

extern struct _DisplayContext* display_context;

/* Special entrypoints */
void EmacsFrameRecomputeCellSize (Widget widget);
void EmacsFrameSetCharSize (Widget widget, int rows, int cols);

#endif /* _EmacsFrame_h */
