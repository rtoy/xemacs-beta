;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-hlw.el --

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
;;; ILISP LispWorks Common Lisp dialect definition
;;;
;;; Independently written by:
;;;
;;; Jason Trenouth: jason@harlequin.co.uk
;;; Qiegang Long: qlong@cs.umass.edu
;;;
;;; and later merged together by Jason
;;;

(defvar ilisp-lispworks-init-file "lispworks.lisp")

;; may use Qiegang's instead? "[-A-Z]+ [0-9]+ : \\([0-9]+\\) >"

(defun lispworks-break-level (prompt)
  (let ((position nil))
    (if (and prompt (setq position (string-match ": [0-9]+" prompt)))
	(string-to-int (substring prompt (+ 2 position)))
      0)))

(defun lispworks-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (<= (lispworks-break-level new) (lispworks-break-level old)))

;; Qiegang's prompt matcher "^\\([-A-Z]+ [0-9]+ >\\)\\|\\([-A-Z]+ [0-9]+ : [0-9]+ >\\) "
;; Qiegang's error matcher "\\(ILISP:[^\"]*\\)\\|\\(Error: [^\n]*\\)\\|\\(Break.[^\n]*\\)"

(defdialect lispworks "LispWorks"
  clisp
  (ilisp-load-init 'lispworks ilisp-lispworks-init-file)
  (setq comint-fix-error ":a"
	ilisp-reset ":a" ;; LW doesn't have a multi-level abort yet
	comint-continue ":c"
	comint-interrupt-regexp  "Break.\n.*")
  (setq comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lispworks-check-prompt))))
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  ;; (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[^>]*> ")
  (setq comint-prompt-regexp "^[A-Z=][-a-z0-9A-Z:= ]*[$%#>]+ *") 
  (setq ilisp-error-regexp "ILISP [0-9]* : [0-9]* > ")
  (setq ilisp-binary-command "system::*binary-file-type*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-directory-command "(lw:current-pathname)")
  (setq ilisp-set-directory-command "(lw:change-directory \"%s\")")
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-init-binary-command 
	"system::*binary-file-type*"))
  
(if (not lispworks-program) (setq lispworks-program "lispworks"))

(provide 'ilisp-lw)
