;;; -*- Mode: Lisp -*-

;;; lispworks.lisp --

;;; This file is part of ILISP.
;;; Version: 5.7
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995 Marco Antoniotti and Rick Busdiecker
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@lehman.com' to be included in the
;;; ILISP mailing list. 'ilisp@lehman.com' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;; LispWorks ILISP initializations.
;;;
;;; Independently written by:
;;;
;;; Jason Trenouth: jason@harlequin.co.uk
;;; Qiegang Long: qlong@cs.umass.edu
;;;
;;; and later merged together by Jason


(in-package "ILISP")

(defun ilisp-callers (symbol package)
  "Print a list of all of the functions that call FUNCTION.
Return T if successful."
  (ilisp-errors
      (let ((function-name (ilisp-find-symbol symbol package))
	    (*print-level* nil)
	    (*print-length* nil)
	    (*package* (find-package 'lisp))
	    (callers ())
	    )
	(when (and function-name (fboundp function-name))
	  (setf callers (munge-who-calls (lw:who-calls function-name)))
	  (dolist (caller callers)
	    (print caller))
	  t))))
	  
;; gross hack to munge who-calls output for ILISP
(defun munge-who-calls (who-calls)
  (labels ((top-level-caller (form)
	     (if (atom form)
		 form
		 (top-level-caller (second form)))))
    (delete-if-not 'symbolp
		   (delete-duplicates (mapcar #'top-level-caller who-calls)))))


;; Jason 6 SEP 94 -- tabularized Qiegang's code
;;
;; There are some problems lurking here:
;;   - the mapping ought to be done by LispWorks
;;   - surely you really want just three source types:
;;     function, type, and variable
;;
(defconstant *source-type-translations*
  '(
    ("class"     defclass)
    ("function"  )
    ("macro"     )
    ("structure" defstruct)
    ("setf"      defsetf)
    ("type"      deftype)
    ("variable"  defvar defparameter defconstant)
    ))


(defun translate-source-type-to-dspec (symbol type)
  (let ((entry (find type *source-type-translations*
		     :key 'first :test 'equal)))
    (if entry
	(let ((wrappers (rest entry)))
	  (if wrappers
	      (loop for wrap in wrappers collecting `(,wrap ,symbol))
	      `(,symbol)))
	(error "unknown source type for ~S requested from ILISP: ~S"
	       symbol type))))


(defun ilisp-source-files (symbol package type)
  "Print each file for PACKAGE:SYMBOL's TYPE definition on a line and
return T if successful.  A function to limit the search with type?"
  (ilisp-errors
   (let* ((symbol (ilisp-find-symbol symbol package))
	  (all (equal type "any"))
	  (paths (when symbol (compiler::find-source-file symbol)))
	  (dspecs (or all (translate-source-type-to-dspec symbol type)))
	  (cands ())
	  )
     (if (and paths (not all))
	 (setq cands
	       (loop for path in paths
		     when (find (car path) dspecs :test 'equal)
		     collect path))
       (setq cands paths))
     (if cands
	 (progn
	   (dolist (file (remove-duplicates paths
					    :key #'cdr :test #'equal))
	     (print (namestring (cadr file))))
	   t)
	 nil))))

(unless (compiled-function-p #'ilisp-callers)
  (format t "\"ILISP: File is not compiled, use M-x ilisp-compile-inits\""))

;;; end of file -- lispworks.lisp --
