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

;; Require a newline on save
(let ((test-file-name (make-temp-file "files-tests"))
      (require-final-newline t))
  (find-file test-file-name)
  (erase-buffer)
  (insert "no newline")
  (Silence-Message (save-buffer 0))
  (Assert (equal (buffer-string) "no newline\n"))
  (kill-buffer nil)
  (delete-file test-file-name))

;; Don't require a newline on save
(let ((test-file-name (make-temp-file "files-tests"))
      (require-final-newline nil))
  (find-file test-file-name)
  (erase-buffer)
  (insert "no newline")
  (Silence-Message (save-buffer 0))
  (Assert (equal (buffer-string) "no newline"))
  (kill-buffer nil)
  (delete-file test-file-name))

;; Require a newline on visit (not on save)
;; Answer query when saving with both no and yes.
(let ((test-file-name (make-temp-file "files-tests"))
      (require-final-newline nil))
  (find-file test-file-name)
  (erase-buffer)
  (insert "no newline")
  (Silence-Message (save-buffer 0))
  (kill-buffer nil)
  (let ((require-final-newline 'visit))
    (find-file test-file-name)
    (Assert (equal (buffer-string) "no newline\n"))

    ;; Answer no
    (erase-buffer)
    (insert "no newline")
    (flet ((y-or-n-p (prompt) nil)) 
      (Silence-Message (save-buffer 0)))
    (Assert (equal (buffer-string) "no newline"))

    ;; Answer yes
    (erase-buffer)
    (insert "no newline")
    (flet ((y-or-n-p (prompt) t)) 
      (Silence-Message (save-buffer 0)))
    (Assert (equal (buffer-string) "no newline\n")))

  (kill-buffer nil)
  (delete-file test-file-name))

;; Require a newline on visit and save
(let ((test-file-name (make-temp-file "files-tests"))
      (require-final-newline nil))
  (find-file test-file-name)
  (erase-buffer)
  (insert "no newline")
  (Silence-Message (save-buffer 0))
  (kill-buffer nil)
  (let ((require-final-newline 'visit-save))
    (find-file test-file-name)
    (Assert (equal (buffer-string) "no newline\n"))
    (erase-buffer)
    (insert "no newline")
    (Silence-Message (save-buffer 0))
    (Assert (equal (buffer-string) "no newline\n")))
  (kill-buffer nil)
  (delete-file test-file-name))

;; mode-require-final-newline is respected by text-mode
(let ((test-file-name (make-temp-file "files-tests"))
      (require-final-newline nil)
      (mode-require-final-newline t))
  (Assert (equal require-final-newline nil))
  (find-file test-file-name)
  (erase-buffer)
  (text-mode)
  (Assert (equal require-final-newline t))
  (insert "no newline")
  (Silence-Message (save-buffer 0))
  (Assert (equal (buffer-string) "no newline\n"))
  (kill-buffer nil)
  (delete-file test-file-name))
