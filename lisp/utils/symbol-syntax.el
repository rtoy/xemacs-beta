;;; symbol-syntax.el --- find chars with symbol syntax

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Created by: JBW, JBW@_CORTEZ
;; Created on: Wed Jun 20 15:15:34 1990
;; Last modified by: Ben Wing, wing@666.com
;; Last modified on: Mon Oct  2 02:32:05 GMT 1995
;; Filename: symbol-syntax.el
;; Keywords: matching

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

(defvar symbol-syntax-table-alist nil)
;;  '((c-mode-syntax-table)
;;    (emacs-lisp-mode-syntax-table)
;;    (lisp-mode-syntax-table)
;;    (text-mode-syntax-table)))

(defun update-symbol-syntax-table-alist ()
  (let ((alist symbol-syntax-table-alist)
	item)
    (while (consp alist)
      (cond ((null (car alist))
	     (error "Missing alist item"))
	    ((null (car (car alist)))
	     (error "Alist item with null car"))
	    ;; this functionality not used
	    ((symbolp (setq item (car (car alist))))
	     (or (null (cdr (car alist)))
		 (error "Alist item expected to have null cdr"))
	     (while (symbolp item)
	       (setq item (symbol-value item)))
	     (setcar (car alist) item)))
      (cond ((not (syntax-table-p (car (car alist))))
	     (error "Alist item car expected to be symbol table"))
	    ((null (cdr (car alist)))
	     (setcdr (car alist)
		     (make-symbol-syntax-table (car (car alist))))))
      (setq alist (cdr alist)))))

(defun get-symbol-syntax-table (norm-table)
  (let (result)
    (if (setq result (assq norm-table symbol-syntax-table-alist))
	nil
      (update-symbol-syntax-table-alist)
      (if (setq result (assq norm-table symbol-syntax-table-alist))
	  nil
	(setq symbol-syntax-table-alist
	      (cons (list norm-table)
		    symbol-syntax-table-alist))
	(update-symbol-syntax-table-alist)
	(or (setq result (assq norm-table symbol-syntax-table-alist))
	    (error "Syntax table missing from symbol-syntax-table-alist"))))
    (or (setq result (cdr result))
	(error "Alist item has null cdr"))
    (or (syntax-table-p result)
	(error "Non-syntax-table item in alist"))
    result))

(defun make-symbol-syntax-table (in-table)
  (let ((osyn (syntax-table))
	(out-table (copy-syntax-table in-table))
	(i 0)
	(syntax nil))
    (while (< i 256)
      (setq syntax (aref out-table i))
      (if (eq 3 (logand 255 syntax))
	  (aset out-table i (logior 2 (logand (lognot 255) syntax))))
      (setq i (1+ i)))
    out-table))

;; stuff for examining contents of syntax tables
;;(show-chars-with-syntax
;; '(c-mode-syntax-table
;;   emacs-lisp-mode-syntax-table
;;   lisp-mode-syntax-table
;;   text-mode-syntax-table)
;; ?_)

(defun show-chars-with-syntax (tables syntax)
  (let ((osyn (syntax-table))
	(schars nil))
    (unwind-protect
	(while (consp tables)
	  (let* ((chars nil)
		 (table-symbol (car tables))
		 (table table-symbol)
		 (i 0))
	    (or (symbolp table-symbol)
		(error "bad argument non-symbol"))
	    (while (symbolp table)
	      (setq table (symbol-value table)))
	    (set-syntax-table table)
	    (while (< i 256)
	      (if (eq syntax (char-syntax i))
		  (setq chars (cons (format "%c" i) chars)))
	      (setq i (1+ i)))
	    (setq schars (cons (list table-symbol
				     (mapconcat 'identity (nreverse chars) ""))
			       schars)))
	  (setq tables (cdr tables)))
      (set-syntax-table osyn))
    (nreverse schars)))

(provide 'symbol-syntax)
