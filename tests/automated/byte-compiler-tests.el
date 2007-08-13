;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Martin Buchholz <martin@xemacs.org>
;; Maintainer: Martin Buchholz <martin@xemacs.org>
;; Created: 1998
;; Keywords: tests

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

;;; Test byte-compiler functionality
;;; See test-harness.el

(condition-case err
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

(require 'bytecomp)

;; test constant symbol warnings
(defmacro check-byte-compiler-message (message-regexp &rest body)
  `(Check-Message ,message-regexp (byte-compile '(lambda () ,@body))))

(check-byte-compiler-message "Attempt to set non-symbol" (setq 1 1))
(check-byte-compiler-message "Attempt to set constant symbol" (setq t 1))
(check-byte-compiler-message "Attempt to set constant symbol" (setq nil 1))
(check-byte-compiler-message "^$" (defconst :foo 1))

(check-byte-compiler-message "Attempt to let-bind non-symbol" (let ((1 'x)) 1))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((t 'x)) (foo)))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((nil 'x)) (foo)))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((:foo 'x)) (foo)))


(check-byte-compiler-message "bound but not referenced" (let ((foo 'x)) 1))
(Assert (not (boundp 'free-variable)))
(Assert (boundp 'byte-compile-warnings))
(check-byte-compiler-message "assignment to free variable" (setq free-variable 1))
(check-byte-compiler-message "reference to free variable" (car free-variable))
(check-byte-compiler-message "called with 2 args, but requires 1" (car 'x 'y))

(check-byte-compiler-message "^$" (setq :foo 1))
(let ((fun '(lambda () (setq :foo 1))))
  (fset 'test-byte-compiler-fun fun))
(Check-Error setting-constant (test-byte-compiler-fun))
(byte-compile 'test-byte-compiler-fun)
(Check-Error setting-constant (test-byte-compiler-fun))

(eval-when-compile (defvar setq-test-foo nil) (defvar setq-test-bar nil))
(progn
  (check-byte-compiler-message "set called with 1 arg, but requires 2" (setq setq-test-foo))
  (check-byte-compiler-message "set called with 1 arg, but requires 2" (setq setq-test-foo 1 setq-test-bar))
  (check-byte-compiler-message "set-default called with 1 arg, but requires 2" (setq-default setq-test-foo))
  (check-byte-compiler-message "set-default called with 1 arg, but requires 2" (setq-default setq-test-foo 1 setq-test-bar))
  )

;;-----------------------------------------------------
;; let, let*
;;-----------------------------------------------------

;; Test interpreted and compiled lisp separately here
(check-byte-compiler-message "malformed let binding" (let  ((x 1 2)) 3))
(check-byte-compiler-message "malformed let binding" (let* ((x 1 2)) 3))

(Check-Error-Message
 error "`let' bindings can have only one value-form"
 (eval '(let ((x 1 2)) 3)))

(Check-Error-Message
 error "`let' bindings can have only one value-form"
 (eval '(let* ((x 1 2)) 3)))

