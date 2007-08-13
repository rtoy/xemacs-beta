;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mak.el --

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
;;; This file is used by make to compile ilisp.
;;;
;;; Note: 11/23/94 Marco Antoniotti. Actually I believe that this
;;; should be removed or redone.

(message "ILISP Compilation: starting.")

;;(require 'byte-compile)

(if (not (file-exists-p "ilcompat.el"))
    (error "ILISP Compilation: compatibility file 'ilcompat.el' non existent.")
  (progn
    (setq load-path (cons "." load-path))

    (load "ilcompat.el")		; Need to load this beforehand
					; to use the +ilisp-emacs-version-id+
					; constant.

    (message ";;; Emacs Version %s" +ilisp-emacs-version-id+)

    (if (eq +ilisp-emacs-version-id+ 'fsf-18)
	(load "comint-v18")
      (load "comint"))


    ;; Try to generate bytecodes for emacs 19.
    ;; I am no expert on the Byte Compiler.  Anyone who is please send
    ;; me mail.
    ;; Marco Antoniotti <marcoxa@icsi.berkeley.edu>

    (if (eq +ilisp-emacs-version-id+ 'fsf-18)
	(setq byte-compile-emacs18-compatibility t)
      (setq byte-compile-generate-emacs19-bytecodes t
	    byte-compile-warnings '(redefine callargs unresolved)))

    ;; Compile compatibility files
    (progn
      (cond ((or (eq +ilisp-emacs-version-id+ 'lucid-19)
		 (eq +ilisp-emacs-version-id+ 'lucid-19-new)
		 (eq +ilisp-emacs-version-id+ 'xemacs))
	     (byte-compile "illuc19.el") ; Note that in current version
					; of ILISP illuc19 and
					; ilxemacs are linked
	     )
	    ((eq +ilisp-emacs-version-id+ 'fsf-19)
	     (byte-compile "ilfsf19.el"))
	    ((eq +ilisp-emacs-version-id+ 'fsf-18)
	     (byte-compile "ilfsf18.el"))
	    (t (error "ILISP Compilation: unrecogninized Emacs version %s"
		      +ilisp-emacs-version-id+)))
      (byte-compile "ilcompat.el"))

    ;; Other files in the distribution.

    (let ((files '(completer
		   comint-ipc
		   bridge
		   ilisp-def
		   ilisp-el
		   ilisp-sym
		   ilisp-inp
		   ilisp-ind

		   ilisp-prc
		   ilisp-val
		   ilisp-out
		   ilisp-mov
		   ilisp-key
		   ilisp-prn
		   ilisp-low
		   ilisp-doc
		   ilisp-ext
		   ilisp-mod
		   ilisp-dia
		   ilisp-cmt
		   ilisp-rng
		   ilisp-hnd
		   ilisp-utl
		   ilisp-cmp
		   ilisp-kil
		   ilisp-snd
		   ilisp-xfr
		   ilisp-hi
		   ilisp-aut

		   ;; Dialects.
		   ilisp-cl
		   ilisp-cmu
		   ilisp-acl
		   ilisp-kcl
		   ilisp-hlw
		   ilisp-luc
		   ilisp-xls
		   ilisp-sch
		   )))
      (while files
	(byte-compile-file (format "%s.el" (car files)) 0)
	(load (format "%s" (car files)))
	(setq files (cdr files))))

    (message "Done")))

;;; end of file -- ilisp-mak.el --

