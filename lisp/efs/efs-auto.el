;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-auto.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.8 $
;; RCS:          
;; Description:  Simple way of autoloading efs
;; Author:       Andy Norman, Dawn
;; Created:      Thu Sep 24 09:50:08 1992
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.
  
;;; Provides a way of autoloading efs. To use this, just put
;;; (require 'efs-auto) in your .emacs file.
;;;
;;; The Bad News:
;;; 
;;; 1. Calls to load and require will not trigger efs to autoload.
;;;    If you are want to put remote directories in your load path,
;;;    you should require efs.
;;; 2. Because efs does not overload expand-file-name until it is loaded,
;;;    "smart" expansion of file names on remote apollos running domain
;;;    will not work yet.  This means that accessing a file on a remote
;;;    apollo may not correctly cause efs to autoload. This will depend
;;;    the details of your command sequence.

(provide 'efs-auto)
(require 'efs-ovwrt)
(require 'efs-fnh)

(defconst efs-auto-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.8 $" 11 -2)))

;;; Interactive functions that should be accessible from here.

(autoload 'efs-report-bug "efs-report" "Submit a bug report for efs." t)
(autoload
 'efs-set-passwd "efs-netrc"
 "For a given HOST and USER, set or change the associated PASSWORD." t)
(autoload 'efs-nslookup-host "efs"
	  "Attempt to resolve a hostname using nslookup if possible." t)
(autoload 'efs-display-ftp-activity "efs"
  "Displays the number of active background ftp sessions in the modeline.
Uses the variable `efs-mode-line-format' to determine how this will be
displayed." t)
(autoload 'efs-ftp-path "efs-cu"
  "Parse PATH according to efs-path-regexp.
Returns a list (HOST USER PATH), or nil if PATH does not match the format.")

;;; end of efs-auto.el
