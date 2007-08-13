;;; tl-atype.el --- atype functions

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: tl-atype.el,v 1.3 1996/12/29 00:15:09 steve Exp $
;; Keywords: atype

;; This file is part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'emu)
(require 'tl-str)
(require 'tl-list)


;;; @ field
;;;

(defalias 'fetch-field 'assoc)
(defalias 'fetch-field-value 'assoc-value)
(defalias 'put-field 'put-alist)
(defalias 'delete-field 'del-alist)

(defun put-fields (tp c)
  (catch 'tag
    (let ((r tp) f ret)
      (while r
	(setq f (car r))
	(if (not (if (setq ret (fetch-field (car f) c))
		     (equal (cdr ret)(cdr f))
		   (setq c (cons f c))
		   ))
	    (throw 'tag 'error))
	(setq r (cdr r))
	))
    c))


;;; @ field unifier
;;;

(defun field-unifier-for-default (a b)
  (let ((ret
	 (cond ((equal a b)    a)
	       ((null (cdr b)) a)
	       ((null (cdr a)) b)
	       )))
    (if ret
	(list nil ret nil)
      )))

(defun field-unify (a b)
  (let ((sym (symbol-concat "field-unifier-for-" (car a))))
    (if (not (fboundp sym))
	(setq sym (function field-unifier-for-default))
      )
    (funcall sym a b)
    ))


;;; @ type unifier
;;;

(defun assoc-unify (class instance)
  (catch 'tag
    (let ((cla (copy-alist class))
	  (ins (copy-alist instance))
	  (r class)
	  cell aret ret prev rest)
      (while r
	(setq cell (car r))
	(setq aret (fetch-field (car cell) ins))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-field (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-field (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (put-field (car cell)(cdr (nth 1 ret)) cla))
		  (setq ins (delete-field (car cell) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (setq r (copy-alist ins))
      (while r
	(setq cell (car r))
	(setq aret (fetch-field (car cell) cla))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-field (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-field (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (delete-field (car cell) cla))
		  (setq ins (put-field (car cell)(cdr (nth 1 ret)) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (list prev (append cla ins) rest)
      )))

(defun get-unified-alist (db al)
  (let ((r db) ret)
    (catch 'tag
      (while r
	(if (setq ret (nth 1 (assoc-unify (car r) al)))
	    (throw 'tag ret)
	  )
	(setq r (cdr r))
	))))

(defun delete-atype (atl al)
  (let* ((r atl) ret oal)
    (setq oal
	  (catch 'tag
	    (while r
	      (if (setq ret (nth 1 (assoc-unify (car r) al)))
		  (throw 'tag (car r))
		)
	      (setq r (cdr r))
	      )))
    (delete oal atl)
    ))

(defun remove-atype (sym al)
  (and (boundp sym)
       (set sym (delete-atype (eval sym) al))
       ))

(defun replace-atype (atl old-al new-al)
  (let* ((r atl) ret oal)
    (if (catch 'tag
	  (while r
	    (if (setq ret (nth 1 (assoc-unify (car r) old-al)))
		(throw 'tag (rplaca r new-al))
	      )
	    (setq r (cdr r))
	    ))
	atl)))

(defun set-atype (sym al &rest options)
  (if (null (boundp sym))
      (set sym al)
    (let* ((replacement (memq 'replacement options))
	   (ignore-fields (car (cdr (memq 'ignore options))))
	   (remove (or (car (cdr (memq 'remove options)))
		       (let ((ral (copy-alist al)))
			 (mapcar (function
				  (lambda (type)
				    (setq ral (del-alist type ral))
				    ))
				 ignore-fields)
			 ral)))
	   )
      (set sym
	   (or (if replacement
		   (replace-atype (eval sym) remove al)
		 )
	       (cons al
		     (delete-atype (eval sym) remove)
		     )
	       )))))


;;; @ end
;;;

(provide 'tl-atype)

;;; tl-atype.el ends here
