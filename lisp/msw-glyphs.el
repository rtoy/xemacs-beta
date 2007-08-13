;;; msw-glyphs.el --- Support for glyphs in ms windows

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.

;; Author: Kirill M. Katsnelson <kkm@kis.ru>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file contains temporary definitions for 'mswindows glyphs.
;; Since there currently is no image support, the glyps are defined
;; TTY-style. This file has to be removed or reworked completely
;; when we have images.

;; This file is dumped with XEmacs.

;;; Code:

(eval-and-compile
  (set-console-type-image-conversion-list
   'mswindows
   '(("^#define" [string :data "[xpm]"])
     ("\\`X-Face:" [string :data "[xface]"])
     ("\\`/\\* XPM \\*/" [string :data "[xpm]"])
     ("\\`GIF87" [string :data "[gif]"])
     ("\\`\377\330\340\000\020JFIF" [string :data "[jpeg]"])
     ("" [string :data nil] 2)
     ;; this last one is here for pointers and icons and such --
     ;; strings are not allowed so they will be ignored.
     ("" [nothing])))

  ;; finish initializing truncation glyph -- created internally
  ;; because it has a built-in bitmap
  (set-glyph-image truncation-glyph "$" 'global 'mswindows)

  ;; finish initializing continuation glyph -- created internally
  ;; because it has a built-in bitmap
  (set-glyph-image continuation-glyph "\\" 'global 'mswindows)

  ;; finish initializing hscroll glyph -- created internally
  ;; because it has a built-in bitmap
  (set-glyph-image hscroll-glyph "$" 'global 'mswindows)

  (set-glyph-image octal-escape-glyph "\\")
  (set-glyph-image control-arrow-glyph "^")
  (set-glyph-image invisible-text-glyph " ...")

  (set-glyph-image xemacs-logo
		   "XEmacs <Images support is due in 20.6!>"
		   'global 'mswindows)
)

;;; msw-glyphs.el ends here
