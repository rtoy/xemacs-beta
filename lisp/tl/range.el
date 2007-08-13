;;; range.el --- range functions

;; Copyright (C) 1987 .. 1996 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;         Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: range.el,v 1.2 1996/12/22 00:29:31 steve Exp $
;; Keywords: range

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

;; These functions were imported from September Gnus 0.40.

(defun compress-sorted-numbers (numbers &optional always-list)
  "Convert list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges. [range.el]"
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (if (not (listp (cdr numbers)))
	  numbers
	(while numbers
	  (cond ((= last (car numbers)) nil) ;Omit duplicated number
		((= (1+ last) (car numbers)) ;Still in sequence
		 (setq last (car numbers)))
		(t			;End of one sequence
		 (setq result
		       (cons (if (= first last) first
			       (cons first last)) result))
		 (setq first (car numbers))
		 (setq last  (car numbers))))
	  (setq numbers (cdr numbers)))
	(if (and (not always-list) (null result))
	    (if (= first last) (list first) (cons first last))
	  (nreverse (cons (if (= first last) first (cons first last))
			  result)))))))

(defun expand-range (range)
  "Expand a range into a list of numbers. [range.el]"
  (cond ((numberp range)
	 range)
	((numberp (cdr range))
	 (index (car range)(cdr range))
	 )
	(t
	 (let (dest ret)
	   (mapcar (function
		    (lambda (sec)
		      (setq ret (expand-range sec))
		      (setq dest
			    (nconc dest
				   (if (and (listp ret)
					    (listp (cdr ret)))
				       ret
				     (list ret)
				     )))
		      ))
		   range)
	   dest))))

(defun member-of-range (number range)
  "Return t if NUMBER is a member of RANGE. [range.el]"
  (cond ((numberp range)
	 (= number range)
	 )
	((numberp (cdr range))
	 (and (<= (car range) number)
	      (<= number (cdr range))
	      )
	 )
	(t
	 (catch 'tag
	   (while range
	     (if (member-of-range number (car range))
		 (throw 'tag t)
	       )
	     (setq range (cdr range))
	     ))
	 )))


;;; @ end
;;;

(provide 'range)

;;; range.el ends here
