;;; -*- Mode: Emacs-Lisp -*-

;;; ilcompat.el --

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


;;;============================================================================
;;; Global definitions/declarations

(defconst +ilisp-emacs-version-id+
  (cond ((string-match "XEmacs" emacs-version)
	 'xemacs)
	((string-match "Lucid" emacs-version)
	 (if (string-match "^19.[0-7][^0-9]" emacs-version)
	     'lucid-19
	   'lucid-19-new))
	((string-match "^19" emacs-version)
	 'fsf-19)
	(t 'fsf-18))
  "The version of Emacs ILISP is running in.
Declared as '(member fsf-19 fsf-19 lucid-19 lucid-19-new xemacs.
Set in ilcompat.el.")


;;;============================================================================
;;; Code

(cond ((or (eq +ilisp-emacs-version-id+ 'lucid-19)
	   (eq +ilisp-emacs-version-id+ 'lucid-19-new))
       (load "illuc19"))
      ((eq +ilisp-emacs-version-id+ 'xemacs) (load "ilxemacs"))
      ((eq +ilisp-emacs-version-id+ 'fsf-19) (load "ilfsf19"))
      ((eq +ilisp-emacs-version-id+ 'fsf-18) (load "ilfsf18"))
      )

;;;============================================================================
;;; Epilogue

(provide 'compat)

;;; end of file -- compat.el --
