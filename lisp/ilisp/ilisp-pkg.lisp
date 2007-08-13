;;; -*- Mode: Lisp -*-

;;; ilisp-pkg.lisp --

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



;;; CLtL2 defpackage definition for ILISP.
;;;
;;; Common Lisp initializations
;;;
;;; Author: Marco Antoniotti, marcoxa@cs.nyu.edu

;;;----------------------------------------------------------------------------
;;; Prologue

#+(or allegro-v4.0 allegro-v4.1)
(eval-when (compile load eval)
  (setq excl:*cltl1-in-package-compatibility-p* t))


;;;----------------------------------------------------------------------------
;;; Definitions

;;; ILISP package --

#-gcl
(defpackage "ILISP" (:use "LISP" #+:CMU "CONDITIONS")
  ;; The following symbols should properly 'shadow' the inherited
  ;; ones.
  (:export "ILISP-ERRORS"
	   "ILISP-SAVE"
	   "ILISP-RESTORE"
	   "ILISP-SYMBOL-NAME"
	   "ILISP-FIND-SYMBOL"
	   "ILISP-FIND-PACKAGE"
	   "ILISP-EVAL"
	   "ILISP-COMPILE"
	   "ILISP-DESCRIBE"
	   "ILISP-INSPECT"
	   "ILISP-ARGLIST"
	   "ILISP-DOCUMENTATION"
	   "ILISP-MACROEXPAND"
	   "ILISP-MACROEXPAND-1"
	   "ILISP-TRACE"
	   "ILISP-UNTRACE"
	   "ILISP-COMPILE-FILE"
	   "ILISP-CASIFY"
	   "ILISP-MATCHING-SYMBOLS"
	   "ILISP-CALLERS"
	   "ILISP-SOURCE-FILES")
  )
;;; ILISP --

;;; end of file -- ilisp-pkg.lisp --
