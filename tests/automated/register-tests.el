;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mats Lidell <matsl@xemacs.org>
;; Maintainer: 
;; Created: 2012
;; Keywords: tests

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

;; Test tag support.
;; See test-harness.el for instructions on how to run these tests.

(require 'test-harness)

(let ((s "Short"))
  (with-output-to-temp-buffer "*Output Register Tests*"
    (set-register 'a s)
    (Assert (equal (get-register 'a) s))
    (Assert (equal (describe-register-1 'a t) s))
    (Assert (equal (describe-register-1 'a) s))))

;; describe-register-1 without verbose on depends on window-width
;; which is 10!? in batch mode. Hence the short part of the string
;; returned

(let ((s "String longer than 5 chars"))
  (with-output-to-temp-buffer "*Output Register Tests*"
    (set-register 'a s)
    (Assert (equal (get-register 'a) s))
    (Assert (equal (describe-register-1 'a t) s))
    (Assert (equal (describe-register-1 'a) (subseq s 0 5)))))
