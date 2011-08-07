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

(require 'test-harness)

(when (equal system-type 'linux)
  (setenv "LANG" "C")
  
  ;; One line output
  (Assert (= 0 (shell-command "echo hello")))
  (Assert (equal "hello" (message-displayed-p t)))
  (with-current-buffer " *Echo Area*"
    (goto-char (point-min))
    (Assert (looking-at "hello")))

  ;; Two lines. No output in echo area. (GNU resizes minibuffer but we
  ;; haven't implemented that.)
  (message "")
  (Assert (= 0 (shell-command "echo -e \"foo\nbar\n\"")))
  (with-current-buffer " *Echo Area*"
    (Assert (= 0 (buffer-size))))
  (with-current-buffer "*Shell Command Output*"
    (goto-char (point-min))
    (Assert (looking-at "foo"))):

  (Assert (= 127 (shell-command "unknown_command")))
  (Assert (= 2 (shell-command "exit 2")))
  (Assert (equal "(Shell command failed with code 2 and no output)" (message-displayed-p t)))
  
  ;; Output to stderr With error buffer
  (Assert (= 0 (shell-command "echo -e \"foo\nbar\n\" 1>&2" "Output buffer" "Error buffer")))
  (Assert (equal "(Shell command succeeded with some error output)" (message-displayed-p t)))
  (with-current-buffer "Error buffer" 
    (goto-char (point-min))
    (Assert (looking-at "foo")))
  (with-current-buffer "Output buffer" 
    (Assert (= 0 (buffer-size))))

  ;; Output to stderr but no error buffer
  (Assert (= 0 (shell-command "echo -e \"foobar\nfoobar\n\" 1>&2" "Output buffer")))
  (with-current-buffer "Output buffer" 
    (goto-char (point-min))
    (Assert (looking-at "foobar")))
)
