;;; tl-misc.el --- miscellaneous utility of tl.

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: tl-misc.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
;; Keywords: load-path, module, structure

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
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'emu)
(require 'tl-str)

(autoload 'add-path "file-detect")
(autoload 'get-latest-path "file-detect")
(autoload 'file-installed-p "file-detect")


;;; @ module and hook
;;;

(defun call-after-loaded (module func &optional hook-name)
  "If MODULE is provided, then FUNC is called.
Otherwise func is set to MODULE-load-hook.
If optional argument HOOK-NAME is specified,
it is used as hook to set. [tl-misc.el]"
  (if (featurep module)
      (funcall func)
    (progn
      (if (null hook-name)
	  (setq hook-name (symbol-concat module "-load-hook"))
	)
      (add-hook hook-name func)
      )))


;;; @ structure
;;;

(defmacro define-structure (name &rest slots)
  (let ((pred (symbol-concat name '-p)))
    (cons 'progn
	  (nconc
	   (list
	    (` (defun (, pred) (obj)
		 (and (vectorp obj)
		      (eq (elt obj 0) '(, name))
		      ))
	       )
	    (` (defun (, (symbol-concat name '/create)) (, slots)
		 (, (cons 'vector (cons (list 'quote name) slots)))
		 )
	       ))
	   (let ((i 1))
	     (mapcar (function
		      (lambda (slot)
			(prog1
			    (` (defun (, (symbol-concat name '/ slot)) (obj)
				 (if ((, pred) obj)
				     (elt obj (, i))
				   ))
			       )
			  (setq i (+ i 1))
			  )
			)) slots)
	     )
	   (list (list 'quote name))
	   ))))


;;; @ end
;;;

(provide 'tl-misc)

;;; tl-misc.el ends here
