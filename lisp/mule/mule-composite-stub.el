;;; mule-composite-stub.el --- Stubs of composition support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2002 Ben Wing.

;; Keywords: multibyte character, composition

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Emacs 21.1 (src/fontset.c, src/composite.c).

;;; Commentary:

;;; Code:

(defvar use-default-ascent (make-char-table 'generic)
     "UNIMPLEMENTED.
Char table of characters whose ascent values should be ignored.
If an entry for a character is non-nil, the ascent value of the glyph
is assumed to be what specified by _MULE_DEFAULT_ASCENT property of a font.

This affects how a composite character which contains
such a character is displayed on screen.")

(defvar ignore-relative-composition (make-char-table 'generic)
     "UNIMPLEMENTED.
Char table of characters which is not composed relatively.
If an entry for a character is non-nil, a composition sequence
which contains that character is displayed so that
the glyph of that character is put without considering
an ascent and descent value of a previous character.")

(defvar compose-chars-after-function 'compose-chars-after
    "UNIMPLEMENTED.
Function to adjust composition of buffer text.

The function is called with three arguments FROM, TO, and OBJECT.
FROM and TO specify the range of text of which composition should be
adjusted.  OBJECT, if non-nil, is a string that contains the text.

This function is called after a text with `composition' property is
inserted or deleted to keep `composition' property of buffer text
valid.

The default value is the function `compose-chars-after'.")

(defvar composition-function-table (make-char-table 'generic)
    "UNIMPLEMENTED.
Char table of patterns and functions to make a composition.

Each element is nil or an alist of PATTERNs vs FUNCs, where PATTERNs
are regular expressions and FUNCs are functions.  FUNC is responsible
for composing text matching the corresponding PATTERN.  FUNC is called
with three arguments FROM, TO, and PATTERN.  See the function
`compose-chars-after' for more detail.

This table is looked up by the first character of a composition when
the composition gets invalid after a change in a buffer.")

(defun compose-region-internal (start end &optional components mod-func)
  "UNIMPLEMENTED.
Internal use only.

Compose text in the region between START and END.
Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.   See `compose-region' for more detial."
  nil)

(defun compose-string-internal (string start end &optional components mod-func)
  "UNIMPLEMENTED.
Internal use only.

Compose text between indices START and END of STRING.
Optional 4th and 5th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.   See `compose-string' for more detial."
  nil)

(defun find-composition-internal (pos limit string detail-p)
  "UNIMPLEMENTED.
Internal use only.

Return information about composition at or nearest to position POS.
See `find-composition' for more detail."
  nil)
