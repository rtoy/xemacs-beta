;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-kcl.el --

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
;;; ILISP Kyoto Common Lisp dialect definition
;;;

;;;%%%KCL--these dialects by Tom Emerson
;;; kcl-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

(defun kcl-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match ">+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match ">+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect kcl "Kyoto Common LISP" clisp
  (setq comint-prompt-regexp "^>+"
        ilisp-error-regexp "Error: "
        ilisp-binary-extension "o"
        comint-fix-error ":q"
        comint-continue ":r"
	comint-prompt-status
	(function
	 (lambda (old line)
	   (comint-prompt-status old line 'kcl-check-prompt)))))
(if (not kcl-program) (setq kcl-program "kcl"))

;;;%%%AKCL
(defdialect akcl "Austin Kyoto Common LISP" kcl)
(if (not akcl-program) (setq akcl-program "akcl"))


;;;%%%IBCL
(defdialect ibcl "Ibuki Common LISP" kcl
  (setq comint-prompt-regexp "^[-A-Z]*>+\\|^[-A-Z]* ->"
        comint-interrupt-regexp ">>Condition: Terminal Interrupt"
        comint-continue ":q"
        ilisp-reset ":q!"
        ilisp-error-regexp ">>Error:"))
(if (not ibcl-program) (setq ibcl-program "ibcl"))


;;; GCL -- I assume it is exactly as AKCL.
;;; Should check whether it is similar to IBUKI.
(defdialect gcl "GNU Common LISP" akcl)
(if (not gcl-program) (setq gcl-program "gcl"))


;;; ECL -- Beppe Attardi's developments over AKCL

(defdialect ecl "EcoLisp Common LISP" akcl)
(if (not ecl-program) (setq gcl-program "ecl"))

;;; end of file -- ilisp-kcl.el --
