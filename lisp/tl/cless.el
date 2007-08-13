;;; cless.el --- Common lisp and Emacs Lisp source sharing

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: cless.el,v 1.3 1996/12/29 00:15:08 steve Exp $
;; Keywords: common lisp

;; This file is not part of GNU Emacs.

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

(require 'cl)

(defun call-after-loaded (module func &optional hook-name)
  "If MODULE is provided, then FUNC is called.
Otherwise func is set to MODULE-load-hook.
If optional argument HOOK-NAME is specified,
it is used as hook to set. [cless.el; imported from tl-misc.el]"
  (if (featurep module)
      (funcall func)
    (progn
      (if (null hook-name)
	  (setq hook-name
		(intern (concat (symbol-name module) "-load-hook")))
	)
      (add-hook hook-name func)
      )))

(defun define-cless-alias (alias func)
  (defalias alias func)
  (call-after-loaded
   'cl-macs
   (` (lambda ()
	(define-compiler-macro (, alias) (&rest args)
	  (cons (, (list 'quote func)) args)
	  ))
      ))
  )

(define-cless-alias 'FLOOR 'floor*)
(define-cless-alias 'CEILING 'ceiling*)
(define-cless-alias 'TRUNCATE 'truncate*) 
(define-cless-alias 'ROUND 'round*)
(define-cless-alias 'MOD 'mod*)

(define-cless-alias 'DELETE 'delete*)
(define-cless-alias 'SORT 'sort*)
(define-cless-alias 'MEMBER 'member*)
(define-cless-alias 'ASSOC  'assoc*)
(define-cless-alias 'RASSOC  'rassoc*)

(define-cless-alias 'MAPCAR 'mapcar*)

(define-cless-alias 'DEFUN 'defun*)



;;; @ end
;;;

(provide 'cless)

;;; cless.el ends here
