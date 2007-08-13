;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-sch.el --

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



;;; Scheme

(defdialect scheme "Scheme" ilisp
  (setq ilisp-block-command "(begin \n%s)")
  (setq ilisp-load-command "(load \"%s\")")
  )

(if (not scheme-program) (setq scheme-program "scheme"))

;;;Cscheme
;;; This has a problem since interrupts cause things to crash
;(defdialect cscheme "C Scheme"
;  scheme
;  (setq comint-prompt-regexp
;   "^[0-9]+ \\([\\]=]=>\\|Error->\\|Bkpt->\\|Debug->\\|Where->\\) ")
;  (setq ilisp-program "cscheme")
;  (setq ilisp-binary-extension "bin")
;  )


;;; Oaklisp

(defdialect oaklisp "Oaklisp Scheme"
  scheme
  (setq comint-prompt-regexp ">+ ")
  (setq comint-fix-error "(ret 0)")
  (setq ilisp-last-command "*")
  (setq ilisp-describe-command "(describe %s)"))


;;; end of file -- ilisp-sch.el --
