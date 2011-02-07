;;; msw-select.el --- Lisp interface to mswindows selections.

;; Copyright (C) 1990, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs (when mswindows support is compiled in).
;; #### Only copes with copying/pasting text

;;; Code:

(defun mswindows-paste-clipboard ()
  "Insert the current contents of the mswindows clipboard at point,
replacing the active selection if there is one."
  (interactive "*")
  (setq last-command nil)
  (setq this-command 'yank) ; so that yank-pop works.
  (let ((clip (get-clipboard)) (s (mark-marker)) (e (point-marker)))
    (or clip (error "there is no text on the clipboard"))
    (if s
	(if mouse-track-rectangle-p
	    (delete-rectangle s e)
	  (delete-region s e)))
    (push-mark)
    (if mouse-track-rectangle-p
	(insert-rectangle clip)
      (insert clip))))




