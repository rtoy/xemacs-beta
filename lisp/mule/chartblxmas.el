;;; chartblxmas.el --- display table of charset by pop-up menu

;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: chartblxmas.el,v 1.1 1997/11/29 18:44:03 steve Exp $
;; Keywords: character, XEmacs/mule

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

;;; Code:

(require 'alist)
(require 'char-table)

(defun classify-charsets-by-dimension-and-chars (charset-list)
  (let (dest)
    (while charset-list
      (let* ((charset (car charset-list))
	     (chars (charset-chars charset))
	     (dim (charset-dimension charset))
	     (dim-alist (cdr (assq dim dest)))
	     )
	(setq dest
	      (put-alist dim
			 (put-alist chars
				    (cons charset
					  (cdr (assq chars dim-alist)))
				    dim-alist)
			 dest))
	)
      (setq charset-list (cdr charset-list))
      )
    dest))


;;;###autoload
(defun view-charset-by-menu ()
  "Display character table of CHARSET by pop-up menu."
  (interactive)
  (popup-menu
   (cons
    "Character set:"
    (mapcar (function
	     (lambda (cat)
	       (cons (car cat)
		     (sort
		      (mapcar (function
			       (lambda (charset)
				 (vector (charset-doc-string charset)
					 `(view-charset ',charset)
					 t)
				 ))
			      (cdr cat))
		      (function
		       (lambda (a b)
			 (string< (aref a 0)(aref b 0))
			 ))))))
	    (sort
	     (let ((rest
		    (classify-charsets-by-dimension-and-chars (charset-list))
		    ))
	       (while rest
		 (let* ((r (car rest))
			(d (car r)))
		   (setq r (cdr r))
		   (while r
		     (let* ((p (car r))
			    (n (int-to-string (car p)))
			    (s n)
			    (i 1))
		       (while (< i d)
			 (setq s (concat s " x " n))
			 (setq i (1+ i)))
		       (set-alist 'dest (concat s " character set") (cdr p)))
		     (setq r (cdr r))
		     ))
		 (setq rest (cdr rest)))
	       dest)
	     (function (lambda (a b)
			 (string< (car a)(car b))
			 )))
	    ))))

;;; chartblxmas.el ends here
