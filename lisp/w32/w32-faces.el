;;; w32-faces.el --- win32-specific face stuff.

;;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: Jamie Zawinski
;; Modified by:  Chuck Thompson
;; Modified by:  Ben Wing
;; Modified by:  Martin Buchholz
;; Rewritten for win32 by:  Jonathan Harris

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This file does the magic to parse w32 font names, and make sure that the
;; default and modeline attributes of new frames are specified enough.

(defun w32-init-global-faces ()
  )

;;; ensure that the default face has some reasonable fallbacks if nothing
;;; else is specified.
(defun w32-init-device-faces (device)
  (or (face-font 'default 'global)
      (set-face-font 'default "Courier New:Regular:10")
      'global)
  (or (face-foreground 'default 'global)
      (set-face-foreground 'default "black" 'global 'w32))
  (or (face-background 'default 'global)
      (set-face-background 'default "white" 'global 'w32))
  (or (face-background 'modeline 'global)
      (set-face-background 'modeline "grey" 'global 'w32))
  )


(defun w32-init-frame-faces (frame)
  )


;;; Fill in missing parts of a font spec. This is primarily intended as a
;;; helper function for the functions below.
;;; w32 fonts look like:
;;;	fontname[:[weight ][style][:pointsize[:effects[:charset]]]]
;;; A minimal w32 font spec looks like:
;;;	Courier New
;;; A maximal w32 font spec looks like:
;;;	Courier New:Bold Italic:10:underline strikeout:ansi
;;; Missing parts of the font spec should be filled in with these values:
;;;	Courier New:Normal:10::ansi
(defun w32-canicolize-font (font &optional device)
  "Given a win32 font specification, this converts it to canonical form."
  nil)

(defun w32-make-font-bold (font &optional device)
  "Given a win32 font specification, this attempts to make a bold font.
If it fails, it returns nil."
  nil)

(defun w32-make-font-unbold (font &optional device)
  "Given a win32 font specification, this attempts to make a non-bold font.
If it fails, it returns nil."
  nil)

(defun w32-make-font-italic (font &optional device)
  "Given a win32 font specification, this attempts to make an `italic' font.
If it fails, it returns nil."
  nil)

(defun w32-make-font-unitalic (font &optional device)
  "Given a win32 font specification, this attempts to make a non-italic font.
If it fails, it returns nil."
  nil)

(defun w32-make-font-bold-italic (font &optional device)
  "Given a win32 font specification, this attempts to make a `bold-italic'
font. If it fails, it returns nil."
  nil)

(defun w32-find-smaller-font (font &optional device)
  "Loads a new, version of the given font (or font name).
Returns the font if it succeeds, nil otherwise.
If scalable fonts are available, this returns a font which is 1 point smaller.
Otherwise, it returns the next smaller version of this font that is defined."
  nil)

(defun w32-find-larger-font (font &optional device)
  "Loads a new, slightly larger version of the given font (or font name).
Returns the font if it succeeds, nil otherwise.
If scalable fonts are available, this returns a font which is 1 point larger.
Otherwise, it returns the next larger version of this font that is defined."
  nil)
