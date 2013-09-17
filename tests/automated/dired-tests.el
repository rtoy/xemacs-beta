;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Mats Lidell <matsl@xemacs.org>
;; Maintainer: 
;; Created: 2013
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

(let* ((test-file-name (make-temp-file "dired-test"))
       (attr (file-attributes test-file-name))) 
  (Assert (equal (nth 0 attr) nil))
  (Assert (equal (nth 1 attr) 1))
  (Assert (numberp (nth 2 attr)))
  (Assert (equal (nth 2 attr) (user-uid)))
  (Assert (numberp (nth 3 attr)))
  (Assert (equal (nth 4 attr) (nth 5 attr)))
  (Assert (equal (nth 4 attr) (nth 6 attr)))
  (Assert (equal (nth 7 attr) 0)))

;; Optional ID-FORMAT set -> uid and gid are strings
(let* ((test-file-name (make-temp-file "dired-test"))
       (attr (file-attributes test-file-name t))) 
  (Assert (equal (nth 0 attr) nil))
  (Assert (equal (nth 1 attr) 1))
  (Assert (stringp (nth 2 attr)))
  (Assert (equal (nth 2 attr) (user-login-name)))
  (Assert (stringp (nth 3 attr)))
  (Assert (equal (nth 4 attr) (nth 5 attr)))
  (Assert (equal (nth 4 attr) (nth 6 attr)))
  (Assert (equal (nth 7 attr) 0)))
