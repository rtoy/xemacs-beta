;;; tl-atype.el --- atype functions

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: tl-atype.el,v 1.3 1997/06/06 00:57:42 steve Exp $
;; Keywords: atype

;; This file is part of XEmacs.

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

(require 'tl-list)
(require 'atype)


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


;;; @ end
;;;

(provide 'tl-atype)

;;; tl-atype.el ends here
