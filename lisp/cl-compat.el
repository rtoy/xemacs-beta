;;; cl-compat.el --- Common Lisp extensions for XEmacs Lisp (compatibility)

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Keywords: extensions

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

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; This package works with Emacs 18, Emacs 19, and XEmacs/Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains emulations of internal routines of the older
;; CL package which users may have called directly from their code.
;; Use (require 'cl-compat) to get these routines.

;; See cl.el for Change Log.


;;; Code:

;; Require at load-time, but not when compiling cl-compat.
(or (featurep 'cl) (require 'cl))


;;; Keyword routines not supported by new package.

(defmacro defkeyword (x &optional doc)
  (list* 'defconst x (list 'quote x) (and doc (list doc))))

(defun keyword-of (sym)
  (or (keywordp sym) (keywordp (intern (format ":%s" sym)))))

;;; Routines for parsing keyword arguments.

(defun build-klist (arglist keys &optional allow-others)
  (let ((res (multiple-value-call 'mapcar* 'cons (unzip-lists arglist))))
    (or allow-others
	(let ((bad (set-difference (mapcar 'car res) keys)))
	  (if bad (error "Bad keywords: %s not in %s" bad keys))))
    res))

(defun extract-from-klist (klist key &optional def)
  (let ((res (assq key klist))) (if res (cdr res) def)))

(defun keyword-argument-supplied-p (klist key)
  (assq key klist))

(defun elt-satisfies-test-p (item elt klist)
  (let ((test-not (cdr (assq ':test-not klist)))
	(test (cdr (assq ':test klist)))
	(key (cdr (assq ':key klist))))
    (if key (setq elt (funcall key elt)))
    (if test-not (not (funcall test-not item elt))
      (funcall (or test 'eql) item elt))))

;;; Rounding functions with old-style multiple value returns.

(defun cl-floor (a &optional b) (values-list (floor* a b)))
(defun cl-ceiling (a &optional b) (values-list (ceiling* a b)))
(defun cl-round (a &optional b) (values-list (round* a b)))
(defun cl-truncate (a &optional b) (values-list (truncate* a b)))

(defun safe-idiv (a b)
  (let* ((q (/ (abs a) (abs b)))
         (s (* (signum a) (signum b))))
    (values q (- a (* s q b)) s)))

;; Internal routines.

(defun pair-with-newsyms (oldforms)
  (let ((newsyms (mapcar (function (lambda (x) (gensym))) oldforms)))
    (values (mapcar* 'list newsyms oldforms) newsyms)))

(defun zip-lists (evens odds)
  (mapcan 'list evens odds))

(defun unzip-lists (list)
  (let ((e nil) (o nil))
    (while list
      (setq e (cons (car list) e) o (cons (cadr list) o) list (cddr list)))
    (values (nreverse e) (nreverse o))))

(defun reassemble-argslists (list)
  (let ((n (apply 'min (mapcar 'length list))) (res nil))
    (while (>= (setq n (1- n)) 0)
      (setq res (cons (mapcar (function (lambda (x) (elt x n))) list) res)))
    res))

(defun duplicate-symbols-p (list)
  (let ((res nil))
    (while list
      (if (memq (car list) (cdr list)) (setq res (cons (car list) res)))
      (setq list (cdr list)))
    res))


;;; Setf internals.

(defun setnth (n list x)
  (setcar (nthcdr n list) x))

(defun setnthcdr (n list x)
  (setcdr (nthcdr (1- n) list) x))

(defun setelt (seq n x)
  (if (consp seq) (setcar (nthcdr n seq) x) (aset seq n x)))


;;; Functions omitted: case-clausify, check-do-stepforms, check-do-endforms,
;;; extract-do-inits, extract-do[*]-steps, select-stepping-forms,
;;; elt-satisfies-if[-not]-p, with-keyword-args, mv-bind-clausify,
;;; all names with embedded `$'.


(provide 'cl-compat)

;;; arch-tag: 9996bb4f-aaf5-4592-b436-bf64759a3163
;;; cl-compat.el ends here
