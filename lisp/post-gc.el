;;; post-gc.el --- post-gc actions

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Author: Mike Sperber <mike@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file defines actions to happen after each GC to perform
;; additional cleanup, call finalizers, etc.

(defun run-finalizers (alist)
  "Run the finalizers for all objects that have just become unreachable."
  (let ((info (assq 'finalize-list alist)))
    (if info
	(let ((finalize-list (cdr info)))
	  (while finalize-list
	    (funcall (cdr (car finalize-list)) (car (car finalize-list)))
	    (setq finalize-list (cdr finalize-list)))))))

(add-hook 'post-gc-hook 'run-finalizers)

(defvar simple-finalizer-ephemerons '()
  "List of ephemerons for objects that have a finalizer attached..")

(defun add-finalizer (object func)
  "Add FUNC as a finalizer for object OBJECT."
  (setq simple-finalizer-ephemerons
	(cons (make-ephemeron object object func)
	      simple-finalizer-ephemerons)))

(defun cleanup-simple-finalizers (alist)
  "Clean up `simple-finalizer-ephemerons'."
  (and simple-finalizer-ephemerons
       (setq simple-finalizer-ephemerons
	     (delete-if-not #'ephemeron-ref simple-finalizer-ephemerons))))

(add-hook 'post-gc-hook 'cleanup-simple-finalizers)
