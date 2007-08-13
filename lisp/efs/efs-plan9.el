;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-plan9.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  efs support for the Plan 9 FTP Server
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Sat Jan 22 21:26:06 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 18:41:05 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Works for the plan 9 server plan9.att.com. Plan 9 is an
;;; AT&T operating system that is similar to unix.

(provide 'efs-plan9)
(require 'efs)

(defconst efs-plan9-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

(efs-defun efs-fix-dir-path plan9 (dir-path)
  ;; Convert DIR-PATH from UN*X-ish to Plan 9. Does nothing actually.
  ;; Avoids appending the "." that we do in unix.
  dir-path)

(efs-defun efs-allow-child-lookup plan9 (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  ;; Relies on the fact that directories can't have extensions in plan9,
  ;; I think.
  (and (not (and (string-equal dir "/") (string-equal file ".")))
       (progn
	 ;; Makes sure that this is cached, before cd'ing
	 (efs-expand-tilde "~" 'plan9 host user)
	 (efs-raw-send-cd host user
			  (if (string-equal file ".")
			      (efs-internal-file-name-nondirectory
			       dir)
			    (concat dir file))
			  t))))

;;; end of efs-plan9.el
