;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-luc.el --

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


;;;
;;; ILISP Lucid Common Lisp dialect definition
;;;



;;;%%%Lucid
(defvar ilisp-lucid-init-file "lucid.lisp")


(defun lucid-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 0 (string-match "\\(->\\)+" old)))
 			(- (match-end 0) (match-beginning 0))
 			0))
	 (new-level (if (eq 0 (string-match "\\(->\\)+" new))
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect lucid "Lucid Common LISP"
  clisp
  (ilisp-load-init 'lucid ilisp-lucid-init-file)
  (setq comint-prompt-regexp "^\\(->\\)+ \\|^[^> ]*> "
	comint-fix-error ":a"
	ilisp-reset ":a :t"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp "ILISP:[^\"]*\\|>>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-binary-command 
	"(first (last lucid::*load-binary-pathname-types*))"))

(if (not lucid-program) (setq lucid-program "lisp"))
