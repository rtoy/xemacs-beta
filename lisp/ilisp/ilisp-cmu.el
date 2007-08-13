;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cmu.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.


;;;
;;; ILISP CMU Common Lisp dialect definition
;;;
;;;%%%CMULisp

(defvar cmulisp-source-directory-regexp 
  "\\/afs\\/cs\\.cmu\\.edu\\/project\\/clisp\\/src\\/[0-9]*\\/"
  "*Regexp to match cmulisp source code directory.")

(defvar cmulisp-local-source-directory
  nil
  "*Where the cmulisp sources really are.")

(defvar ilisp-cmulisp-init-file "cmulisp.lisp")

(defun cmulisp-source-directory-fixup-function ()
  (if cmulisp-local-source-directory
      (replace-match cmulisp-local-source-directory)))

(defun cmulisp-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "]+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "]+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect cmulisp "CMU Common LISP"
  clisp
  (ilisp-load-init 'cmu ilisp-cmulisp-init-file)
  (if cmulisp-local-source-directory
      (setq ilisp-source-directory-fixup-alist
	    (list 
	     (cons cmulisp-source-directory-regexp
		   cmulisp-local-source-directory)))
    (message "cmulisp-local-source-directory not set."))
  (setq comint-prompt-regexp "^\\([0-9]+\\]+\\|\\*\\) "
	ilisp-trace-command "(ILISP:cmulisp-trace \"%s\" \"%s\" \"%s\")"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'cmulisp-check-prompt)))

	ilisp-error-regexp "ILISP:[^\"]*\\|Error [^\n]*\n\n"
	;; The above regexp has been suggested by
	;; hunter@work.nlm.nih.gov (Larry Hunter)

	ilisp-arglist-command "(ILISP:arglist \"%s\" \"%s\")"

	ilisp-find-source-command "(ILISP:source-file \"%s\" \"%s\" \"%s\")"

	comint-fix-error ":pop"

	comint-continue ":go"

	ilisp-reset ":q"

	comint-interrupt-regexp "Interrupted at"

	ilisp-binary-extension "sparcf"
	ilisp-init-binary-extension "sparcf"
	ilisp-binary-command "\"sparcf\""
	))
