;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-fnh.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.3 $
;; RCS:
;; Description:  Look for the emacs version, and install into
;;               the file-name-handler-alist
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Although used by efs, these utilities could be of general use to other
;;; packages too. Keeping them separate from the main efs program
;;; makes it easier for other programs to require them.

(provide 'efs-fnh)

(defconst efs-fnh-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.3 $" 11 -2)))

;;;; ----------------------------------------------------------------
;;;; Loading emacs version files
;;;; ----------------------------------------------------------------

(defun efs-handle-emacs-version ()
  ;; Load appropriate files for the current emacs version
  (let ((ehev-match-data (match-data)))
    (unwind-protect
	(let ((lucidp (string-match "Lucid" emacs-version))
	      ver subver)
	  (or (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
	      (error "efs does not work with emacs version %s" emacs-version))
	  (setq ver (string-to-int (substring emacs-version
					      (match-beginning 1)
					      (match-end 1)))
		subver (string-to-int (substring emacs-version
						 (match-beginning 2)
						 (match-end 2))))
	  (cond
	   
	   ;; Lucid XEmacs (emacs-version looks like \"19.xx XEmacs Lucid\")
	   (lucidp
	    (cond
	     ((and (= ver 19) (>= subver 11) (< subver 15))
	      (require 'efs-l19\.11))
	     ((and (= ver 19) (>= subver 15))
	      (require 'efs-x19\.15))
	     ((= ver 20)
	      (require 'efs-x19\.15))
	     (t
	      (error
	       "efs does not work with emacs version %s" emacs-version))))

	   ;; Original GNU Emacs from FSF
	   (t
	    (cond
	     ((and (= ver 19) (<= subver 22))
	      (require 'efs-19))
	     ((and (= ver 19) (>= subver 23))
	      (require 'efs-19\.23))
	     
	     ;; GNU Emacs 18-
	     ((<= ver 18)
	      (require 'efs-18)) ; this file will (require 'emacs-19)

	     (t
	      (error
	       "efs does not work with emacs version %s" emacs-version))))))
      
      (store-match-data ehev-match-data))))

;;;; --------------------------------------------------------------
;;;; Stuff for file name handlers.
;;;; --------------------------------------------------------------

;;; Need to do this now, to make sure that the file-name-handler-alist is
;;; defined for Emacs 18.

(efs-handle-emacs-version)

;; Also defined in efs-cu.el
(defvar efs-path-root-regexp "^/[^/:]+:"
  "Regexp to match the `/user@host:' root of an efs full path.")

(defun efs-file-name-handler-alist-sans-fn (fn)
  ;; Returns a version of file-name-handler-alist without efs.
  (delq nil (mapcar
	     (function
	      (lambda (x)
		(and (not (eq (cdr x) fn)) x)))
	     file-name-handler-alist)))

(defun efs-root-handler-function (operation &rest args)
  "Function to handle completion in the root directory."
  (let ((handler (and (if (boundp 'allow-remote-paths)
			  allow-remote-paths
			t)
		      (get operation 'efs-root))))
    (if handler
	(apply handler args)
      (let ((inhibit-file-name-handlers
	     (cons 'efs-root-handler-function
		   (and (eq inhibit-file-name-operation operation)
			inhibit-file-name-handlers)))
	    (inhibit-file-name-operation operation))
	(apply operation args)))))

(put 'file-name-completion 'efs-root 'efs-root-file-name-completion)
(put 'file-name-all-completions 'efs-root 'efs-root-file-name-all-completions)
(autoload 'efs-root-file-name-all-completions "efs-netrc")
(autoload 'efs-root-file-name-completion "efs-netrc")

(autoload 'efs-file-handler-function "efs"
	  "Function to use efs to handle remote files.")

;; Install into the file-name-handler-alist.
;; If we are already there, remove the old entry, and re-install.
;; Remove the ange-ftp entry too.

(setq file-name-handler-alist
      (let (dired-entry alist)
	(setq alist
	      (nconc
	       (list
		(cons efs-path-root-regexp 'efs-file-handler-function)
		'("^/$" . efs-root-handler-function))
	       (delq nil
		     (mapcar
		      (function
		       (lambda (x)
			 (if (eq (cdr x) 'dired-handler-fn)
			     (progn
			       (setq dired-entry x)
			       nil)
			   (and (not
				 (memq (cdr x)
				       '(remote-path-file-handler-function
					 efs-file-handler-function
					 efs-root-handler-function
					 ange-ftp-hook-function
					 ange-ftp-completion-hook-function)))
				x))))
		      file-name-handler-alist))))
	;; Make sure that dired is in first.
	(if dired-entry (cons dired-entry alist) alist)))

;;; end of efs-fnh.el
