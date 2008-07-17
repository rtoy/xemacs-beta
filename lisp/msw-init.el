;;; msw-init.el --- initialization code for mswindows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Rewritten for mswindows by: Jonathan Harris

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar make-device-early-mswindows-entry-point-called-p nil
  "Whether `make-device-early-mswindows-entry-point' has been called")

(defvar make-device-late-mswindows-entry-point-called-p nil
  "Whether `make-device-late-mswindows-entry-point' has been called")

(defun make-device-early-mswindows-entry-point ()
  "Lisp code called before an `mswindows' device is created." 
  (unless make-device-early-mswindows-entry-point-called-p
    ;; Old-style mswindows bindings. The new-style mswindows bindings
    ;; (namely Ctrl-X, Ctrl-C and Ctrl-V) are already spoken for by XEmacs.
    (global-set-key '(shift delete)   'kill-primary-selection)
    (global-set-key '(shift insert)   'yank-clipboard-selection)
    (global-set-key '(control insert) 'copy-primary-selection)

    (global-set-key '(meta f4)	      'save-buffers-kill-emacs)
    (setq make-device-early-mswindows-entry-point-called-p t)))

(defun make-device-late-mswindows-entry-point (device)
  "Lisp code called after an `mswindows' device is created."
  (unless make-device-late-mswindows-entry-point-called-p
    (setq make-device-late-mswindows-entry-point-called-p t)))

