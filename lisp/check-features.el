;;; check-features.el --- Do a sanity check on an XEmacs build

;; Copyright (C) 1998 by Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

;; Author: SL Baur <steve@xemacs.org>
;; Keywords: internal

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

;; This file is executed after a build to check that all lisp packages that
;; need to be installed are.

;;; Code:

(require 'packages)

(defvar build-error 0)

(when (featurep 'tooltalk)
  (condition-case nil
      (package-require 'tooltalk 1.0)
    (t (progn
	 ;; (setq build-error 1)
	 (lwarn 'tooltalk 'alert
	   "Warning:  This XEmacs is built with tooltalk support but
does not have a tooltalk package installed.  Without the
tooltalk lisp package, Tooltalk support is broken.")))))

(when (featurep 'sparcworks)
  (condition-case nil
      (package-require 'Sun 1.0)
    (t (progn
	 ;; (setq build-error 1)
	 (lwarn 'sparcworks 'alert
	   "Warning:  This XEmacs is built with sparcworks support but
does not have the Sun package installed.  Without the Sun
lisp package, Sparcworks support will be broken.")))))

(kill-emacs build-error)

;;; check-features.el ends here
