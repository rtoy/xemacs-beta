;;; tl-list.el --- utility functions about list

;; Copyright (C) 1987 .. 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;         Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Version:
;;	$Id: tl-list.el,v 1.2 1996/12/22 00:29:32 steve Exp $
;; Keywords: list

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
       (require 'cless)
       )
      (t
       ;; New cl is not exist (Don't use old cl.el)

(defun last (ls &optional n)
  "Returns the last element in list LS.
With optional argument N, returns Nth-to-last link (default 1).
\[tl-list.el; tomo's Common Lisp emulating function]"
  (nthcdr (- (length ls) (or n 1)) ls)
  )

;; imported from cl.el
(defun list* (arg &rest rest)
  "Return a new list with specified args as elements, cons'd to last arg.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defconst :test ':test)

(defun MEMBER (elt list &rest keywords)
  (let ((test
	 (or
	  (let ((ret (memq ':test keywords)))
	    (car (cdr ret))
	    )
	  'eq)))
    (cond ((eq test 'eq)
	   (memq elt list)
	   )
	  ((eq test 'equal)
	   (member elt list)
	   )
	  (t
	   (catch 'tag
	     (while list
	       (let* ((cell (car list))
		      (ret (funcall test elt cell))
		      )
		 (if ret
		     (throw 'tag list)
		   ))
	       (setq list (cdr list))
	       ))))))

(defun ASSOC (key alist &rest keywords)
  (let ((test
	 (or
	  (let ((ret (memq ':test keywords)))
	    (car (cdr ret))
	    )
	  'eq)))
    (cond ((eq test 'eq)
	   (assq key alist)
	   )
	  ((eq test 'equal)
	   (assoc key alist)
	   )
	  (t
	   (catch 'tag
	     (while alist
	       (let* ((cell (car alist))
		      (ret (funcall test key (car cell)))
		      )
		 (if ret
		     (throw 'tag cell)
		   ))
	       (setq alist (cdr alist))
	       ))))))
))

(autoload 'compress-sorted-numbers "range")
(autoload 'expand-range "range")
(autoload 'member-of-range "range")


;;; @ list
;;;

(defun nnth-prev (n ls)
  "Modify list LS to remove elements after N th. [tl-list.el]"
  (and (> n 0)
       (let ((cell (nthcdr (1- n) ls)))
	 (if (consp cell)
	     (setcdr cell nil)
	   )
	 ls)))

(defun nth-prev (n ls)
  "Return the first N elements. [tl-list.el]"
  (let (dest) 
    (while (and (> n 0) ls)
      (setq dest (cons (car ls) dest))
      (setq ls (cdr ls)
	    n (1- n))
      )
    (nreverse dest)
    ))

(defun nexcept-nth (n ls)
  "Modify list LS to remove N th element. [tl-list.el]"
  (cond ((< n 0) ls)
	((= n 0) (cdr ls))
	(t
	 (let ((cell (nthcdr (1- n) ls)))
	   (if (consp cell)
	       (setcdr cell (cdr (cdr cell)))
	     ))
	 ls)))

(defun except-nth (n ls)
  "Return elements of LS except N th. [tl-list.el]"
  (if (< n 0)
      ls
    (let (dest) 
      (while (and (> n 0) ls)
	(setq dest (cons (car ls) dest))
	(setq ls (cdr ls)
	      n (1- n))
      )
      (setq ls (cdr ls))
      (while dest
	(setq ls (cons (car dest) ls))
	(setq dest (cdr dest))
	)
      ls)))

(defun last-element (ls)
  "Return last element. [tl-list.el]"
  (car (last ls))
  )

(defun cons-element (elt ls)
  "Cons ELT to LS if ELT is not nil. [tl-list.el]"
  (if elt
      (cons elt ls)
    ls))

(defun cons-if (elt ls)
  "Cons ELT to LS if LS is not nil, otherwise return nil. [tl-list.el]"
  (if ls
      (cons elt ls)
    ))

(defun append-element (ls elt)
  "Append ELT to last of LS if ELT is not nil. [tl-list.el]"
  (if elt
      (append ls (list elt))
    ls))


;;; @ permutation and combination
;;;

(defun every-combination (prev &rest rest)
  "Every arguments are OR list,
and return list of all possible sequence. [tl-list.el]"
  (if (null prev)
      (setq prev '(nil))
    )
  (cond ((null rest)
	 (mapcar 'list prev)
	 )
	(t (let (dest
		 (pr prev)
		 (rest-mixed (apply 'every-combination rest))
		 )
	     (while pr
	       (let ((rr rest-mixed))
		 (while rr
		   (setq dest (cons (cons (car pr)(car rr)) dest))
		   (setq rr (cdr rr))
		   ))
	       (setq pr (cdr pr))
	       )
	     (nreverse dest)
	     ))
	))

(defun permute (&rest ls)
  "Return permutation of arguments as list. [tl-list.el]"
  (let ((len (length ls)))
    (if (<= len 1)
	(list ls)
      (let (prev
	    (rest ls)
	    c dest)
	(while rest
	  (setq c (car rest))
	  (setq rest (cdr rest))
	  (setq dest
		(nconc dest
		       (mapcar (function
				(lambda (s)
				  (cons c s)
				  ))
			       (apply (function permute)
				      (append prev rest))
			       )))
	  (setq prev (nconc prev (list c)))
	  )
	dest)
      )))


;;; @ index
;;;

(defun index (start end &optional inc)
  "Return list of numbers from START to END.
Element of the list increases by INC (default value is 1).
\[tl-list.el; ELIS compatible function]"
  (or inc
      (setq inc 1)
      )
  (let ((pred (if (>= inc 0)
		  (function <=)
		(function >=)
		))
	(i start)
	dest)
    (while (funcall pred i end)
      (setq dest (cons i dest))
      (setq i (+ i inc))
      )
    (nreverse dest)
    ))


;;; @ set
;;;

(defun map-union (func ls)
  "Apply FUNC to each element of LS.
And return union of each result returned by FUNC. [tl-list.el]"
  (let ((r ls) ret rc dest)
    (while r
      (setq ret (funcall func (car r)))
      (while ret
	(setq rc (car ret))
	(or (member rc dest)
	    (setq dest (cons rc dest))
	    )
	(setq ret (cdr ret))
	)
      (setq r (cdr r))
      )
    (nreverse dest)
    ))


;;; @ alist
;;;

(defun put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tl-list.el; tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
	(progn
	  (setcdr pair value)
	  alist)
      (cons (cons item value) alist)
      )))

(defun del-alist (item alist)
  "If there is a pair whose key is <ITEM>, delete it from <ALIST>.
\[tl-list.el; mol's ELIS emulating function]"
  (if (equal item (car (car alist)))
      (cdr alist)
    (let ((pr alist)
	  (r (cdr alist))
	  )
      (catch 'tag
	(while (not (null r))
	  (if (equal item (car (car r)))
	      (progn
		(rplacd pr (cdr r))
		(throw 'tag alist)))
	  (setq pr r)
	  (setq r (cdr r))
	  )
	alist))))

(defun assoc-value (item alist)
  "Return value of <ITEM> from <ALIST>. [tl-list.el]"
  (cdr (assoc item alist))
  )

(defun set-alist (symbol item value)
  "Modify a alist indicated by SYMBOL to set VALUE to ITEM. [tl-list.el]"
  (or (boundp symbol)
      (set symbol nil)
      )
  (set symbol (put-alist item value (symbol-value symbol)))
  )

(defun remove-alist (symbol item)
  "Remove ITEM from the alist indicated by SYMBOL. [tl-list.el]"
  (and (boundp symbol)
       (set symbol (del-alist item (symbol-value symbol)))
       ))

(defun modify-alist (modifier default)
  "Modify alist DEFAULT into alist MODIFIER. [tl-list.el]"
  (mapcar (function
	   (lambda (as)
	     (setq default (put-alist (car as)(cdr as) default))
	     ))
	  modifier)
  default)

(defun set-modified-alist (sym modifier)
  "Modify a value of a symbol SYM into alist MODIFIER.
The symbol SYM should be alist. If it is not bound,
its value regard as nil. [tl-list.el]"
  (if (not (boundp sym))
      (set sym nil)
    )
  (set sym (modify-alist modifier (eval sym)))
  )


;;; @ poly-apply
;;;

(defun poly-funcall (functions arg)
  (while functions
    (setq arg (funcall (car functions) arg)
	  functions (cdr functions))
    )
  arg)


;;; @ end
;;;

(provide 'tl-list)

(require 'tl-seq)
(require 'tl-atype)

;;; tl-list.el ends here
