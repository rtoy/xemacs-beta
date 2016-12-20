;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mats Lidell <matsl@xemacs.org>
;; Maintainer: 
;; Created: 2011
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

(with-temp-buffer
  (insert ";;;")
  (Assert (equal (syntax-ppss (point-min)) '(0 nil nil nil nil nil 0 nil nil nil)))
  (Assert (eq (point-min) (point)))
  (Assert (equal (syntax-ppss 3) '(0 nil nil nil nil nil 0 nil nil nil)))
  (Assert (eq 3 (point)))
  )

