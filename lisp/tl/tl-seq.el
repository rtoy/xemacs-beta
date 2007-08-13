;;; tl-seq.el --- sequence functions

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: tl-seq.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
;; Keywords: sequence

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

(require 'file-detect)

(cond ((file-installed-p "cl-seq.elc")
       (require 'cl)
       )
      (t
       (defun find-if (pred seq)
	 "Return the first element of sequence SEQ satisfying PRED.
\[tl-seq.el]"
	 (let ((i 0)(len (length seq)) element)
	   (catch 'tag
	     (while (< i len)
	       (if (funcall pred (setq element (elt seq i)))
		   (throw 'tag element)
		 )
	       (setq i (+ i 1))
	       ))
	   ))
       
       (defun find (item seq)
	 "Return the first element which is found in sequence SEQ as item.
\[tl-seq.el]"
	 (find-if (function
		   (lambda (elt)
		     (eq elt item)
		     ))
		  seq))
       ))

(defun foldr (func a seq)
  "Return (func (func (func (... (func a Sn) ...) S2) S1) S0)
when func's argument is 2 and seq is a sequence whose
elements = S0 S1 S2 ... Sn. [tl-seq.el]"
  (let ((i (length seq)))
    (while (> i 0)
      (setq i (1- i))
      (setq a (funcall func a (elt seq i)))
      )
    a))

(defun foldl (func a seq)
  "Return (... (func (func (func a S0) S1) S2) ...)
when func's argument is 2 and seq is a sequence whose
elements = S0 S1 S2 .... [tl-seq.el]"
  (let ((len (length seq))
	(i 0))
    (while (< i len)
      (setq a (funcall func a (elt seq i)))
      (setq i (1+ i))
      )
    a))

(defun pack-sequence (seq size)
  (let ((len (length seq)) (p 0) obj
	unit (i 0)
	dest)
    (while (< p len)
      (setq obj (elt seq p))
      (setq unit (cons obj unit))
      (setq i (1+ i))
      (if (= i size)
	  (progn
	    (setq dest (cons (reverse unit) dest))
	    (setq unit nil)
	    (setq i 0)
	    ))
      (setq p (1+ p))
      )
    (if unit
	(setq dest (cons (reverse unit) dest))
      )
    (reverse dest)
    ))


;;; @ end
;;;

(provide 'tl-seq)

;;; tl-seq.el ends here
