;;; cl-autoload.el --- Generate the autoload file cl-defs.el.

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>, Ben Wing <wing@666.com>
;; Version: 2.02
;; Keywords: extensions, lisp

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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: FSF 19.30 (cl.el).

;;; Commentary:

;;; Run this file to regenerate cl-defs.el.
;;; Make sure to first erase the old autoloads from cl-defs.el!

;;; This file was extracted almost directly from cl.el, and the code
;;; there was replaced with (load "cl-defs.el").  What used to happen
;;; is that when cl.el was loaded, it set up autoloads for all of the
;;; functions and macros in the other files. (See the commented-out
;;; code below.) However, the doc strings didn't get properly inserted,
;;; so the functions had no documentation, which is bad.  I changed it
;;; so that you run this only once (to generate cl-defs.el), and then
;;; cl.el loads cl-defs.el.  Note that this relies on a hacked
;;; autoload.el (included with XEmacs 19.14 / 20.0). --ben

;;; Autoload the other portions of the package.
(mapcar (function
	 (lambda (set)
;	   The old definition:
;	   (mapcar (function
;		    (lambda (func)
;		      (autoload func (car set) nil nil (nth 1 set))))
;		   (cddr set))))
	   (find-file "cl-defs.el")
	   (goto-char (point-max))
	   (generate-file-autoloads (car set) (cddr set))))
	'(("cl-extra.el" nil
	   coerce equalp maplist mapc mapl mapcan mapcon
	   cl-map-keymap cl-map-keymap-recursively cl-map-intervals
	   cl-map-overlays cl-set-frame-visible-p cl-float-limits
	   gcd lcm isqrt floor* ceiling* truncate* round*
	   mod* rem* signum random* make-random-state random-state-p
	   concatenate cl-mapcar-many map some every notany
	   notevery revappend nreconc list-length tailp get* getf
	   cl-set-getf cl-do-remf make-hash-table cl-hash-lookup
	   cl-puthash hash-table-p
	   hash-table-count cl-progv-before cl-prettyexpand
	   cl-macroexpand-all
	   ;; XEmacs: removed the following:
	   ;; expt copy-tree subseq remprop gethash remhash clrhash maphash
	   ;; cl-map-keymap appeared twice
	   )
	  ("cl-seq.el" nil
	   reduce fill replace remq remove remove* remove-if remove-if-not
	   delete* delete-if delete-if-not remove-duplicates
	   delete-duplicates substitute substitute-if substitute-if-not
	   nsubstitute nsubstitute-if nsubstitute-if-not find find-if
	   find-if-not position position-if position-if-not count count-if
	   count-if-not mismatch search sort* stable-sort merge member*
	   member-if member-if-not cl-adjoin assoc* assoc-if assoc-if-not
	   rassoc* rassoc-if rassoc-if-not union nunion intersection
	   nintersection set-difference nset-difference set-exclusive-or
	   nset-exclusive-or subsetp subst-if subst-if-not nsubst nsubst-if
	   nsubst-if-not sublis nsublis tree-equal
	   ;; XEmacs: removed the following:
	   ;; delete rassoc
	   )
	  ("cl-macs.el" nil
	   gensym gentemp typep cl-do-pop get-setf-method
	   cl-struct-setf-expander compiler-macroexpand cl-compile-time-init)
	  ("cl-macs.el" t
	   defun* defmacro* function* destructuring-bind eval-when
	   load-time-value case ecase typecase etypecase
	   block return return-from loop do do* dolist dotimes do-symbols
	   do-all-symbols psetq progv flet labels macrolet symbol-macrolet
	   lexical-let lexical-let* multiple-value-bind multiple-value-setq
	   locally the declare define-setf-method defsetf define-modify-macro
	   setf psetf remf shiftf rotatef letf letf* callf callf2 defstruct
	   check-type assert ignore-errors define-compiler-macro
	   ;; XEmacs: removed the following:
	   ;; eval-when-compile
	   )))
