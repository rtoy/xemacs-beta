;;; featurep.el --- Support functions for reader conditionals

;; Copyright 1997 Naggum Software

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: internal

;; This file is not (yet) part of GNU Emacs, but distributed under the
;; same conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The #+ and #- reader macros require support code to work properly until
;; `featurep' is enhanced in the C code.  This support code is written in
;; Lisp to make it easier to experiment with the code.

;; XEmacs: this code has been ported to C by Steve Baur.  The
;; implementations should be equivalent.


;;; Code:

(eval-when-compile (require 'cl))

(provide (if (string-match "XEmacs" emacs-version) 'xemacs 'emacs))

(defvar featurep-emacs-version nil
  "The version number of this Emacs, as a floating-point number.")

(defun featurep (fexp)
  "Return non-nil if feature expression FEXP is true."
  (typecase fexp
    (symbol (and (memq fexp features)       ;original definition
		 t))
    (number (>= (or featurep-emacs-version
                    (setq featurep-emacs-version
                      (+ emacs-major-version
                         (/ emacs-minor-version 100.0))))
                fexp))
    (list (case (pop fexp)
            (not (let ((negate (pop fexp)))
                   (if fexp
                     (signal 'invalid-read-syntax (list fexp))
                     (not (featurep negate)))))
            (and (while (and fexp (featurep (car fexp)))
                   (pop fexp))
                 (null fexp))
            (or (while (and fexp (not (featurep (car fexp))))
                  (pop fexp))
                fexp)
            (t (signal 'invalid-read-syntax (list fexp)))))
    (t (signal 'invalid-read-syntax (list fexp)))))

;;; featurep.el ends here

