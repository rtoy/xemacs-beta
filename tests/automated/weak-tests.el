;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Mike Sperber <mike@xemacs.org>
;; Maintainer: Mike Sperber <mike@xemacs.org>
;; Created: 2002
;; Keywords: tests, database

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test implementation of weak boxes and ephemerons
;;; See test-harness.el

(condition-case err
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

(garbage-collect)
(let ((w (make-weak-box (cons 2 3))))
  (Assert (equal (cons 2 3) (weak-box-ref w)))
  (garbage-collect)
  (Assert (not (weak-box-ref w))))

(garbage-collect)

(let* ((p (cons 3 4))
       (finalized-p nil)
       (eph1 (make-ephemeron (cons 1 2) p
			     '(lambda (value)
				(setq finalized-p t))))
       (eph2 (make-ephemeron p p)))
  (Assert (eq p (ephemeron-ref (make-ephemeron (cons 1 2) p))))
  (Assert (ephemeron-p (make-ephemeron (cons 1 2) p)))

  (garbage-collect)
  (garbage-collect) ; ensure the post-gc hook runs

  (Assert finalized-p)
  (Assert (not (ephemeron-ref eph1)))

  (garbage-collect)
  
  (Assert (eq p (ephemeron-ref eph2))))

