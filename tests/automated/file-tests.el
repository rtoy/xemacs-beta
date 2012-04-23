;;; file-tests.el --- test support for filesystem primitives

;; Copyright (C) 2010 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: Ben Wing <ben@xemacs.org>
;; Created: 2010 January 25
;; Keywords: files, filenames, file names

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

;; Test file-system support.  Opening files, file names, etc.
;; See test-harness.el for instructions on how to run these tests.

;; Test that `file-truename' is idempotent (same value when called multiple
;; times).  Under Cygwin 1.7 as of 1-24-10, not true!
(loop for file in (list (temp-directory)
			(file-name-as-directory (temp-directory))
			"/"
			(file-name-as-directory "/")
			(make-temp-name "foo")
			)
  do
  (Assert (equal (file-truename (file-truename file)) (file-truename file))))


