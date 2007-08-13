;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-report.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Function to report efs bugs in a usable way.
;; Author:       Andy Norman, Dawn
;; Created:      Tue May 18 08:34:45 1993
;; Modified:     Sun Nov 27 18:41:45 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'efs-report)
(require 'efs)
(autoload 'reporter-submit-bug-report "reporter")
(defvar reporter-version) ; For the byte-compiler

;;; Variables

(defconst efs-report-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

(defconst efs-report-salutations
  ["Dear bug team:"
   "Ciao bug team:"
   "Salut bug team:"
   "Gruss bug team:"
   "To whom it may concern:"
   "Fellow efs'ers:"
   "Greetings earthlings:"])

(defvar efs-bug-address "efs-bugs@cuckoo.hpl.hp.com")

(defconst efs-report-other-vars
  ;; List of variables needed for efs-report, that aren't generated below.
  '(efs-ftp-program-name
    efs-ftp-program-args
    efs-local-host-regexp
    efs-ftp-local-host-regexp
    efs-gateway-host
    efs-gateway-type
    reporter-version
    features))

(defconst efs-report-avoid-vars
  ;; List of variables we don't want to see.
  '(efs-netrc-filename
    efs-default-password
    efs-default-account
    efs-default-user))

;; Dynamically bound.  Used to pass data to hooks.
(defvar efs-report-default-host nil)
(defvar efs-report-default-user nil)
(defvar efs-report-blurb nil)  

;;; Functions

(defun efs-report-get-host-type-regexps ()
  "Return a list of host type regexp's which are non-nil."
  (let ((list efs-host-type-alist)
	ent result)
    (while (setq ent (car list))
      (if (symbol-value (cdr ent))
	  (setq result (cons (cdr ent) result)))
      (setq list (cdr list)))
    result))

(defun efs-report-get-versions ()
  ;; Return a list of efs versions variables.
  (mapcar
   'intern
   (sort
    (let (completion-ignore-case)
      (all-completions
       "efs-" obarray
       (function
	(lambda (sym)
	  (and (boundp sym)
	       (let ((name (symbol-name sym)))
		 (and (>= (length name) 8)
		      (string-equal (substring name -8) "-version"))))))))
    'string-lessp)))

(defun efs-report-get-user-vars ()
  ;; Return a list of efs user variables.
  (mapcar
   'intern
   (sort
    (let (completion-ignore-case)
      (all-completions "efs-" obarray 'user-variable-p))
    'string-lessp)))

(defun efs-report-pre-hook ()
  ;; efs-report-default-host, efs-report-default-user, and
  ;; efs-report-blurb are dynamically bound.
  (save-excursion
    (let ((end (progn (mail-position-on-field "subject") (point))))
      (beginning-of-line)
      (search-forward ":" end)
      (delete-region (point) end)
      (insert
       " EFS "
       (or (and (boundp 'efs-version) (string-match "/" efs-version)
		(concat (substring efs-version 0 (match-beginning 0))
			" "))
	   "")
       "bug: ")))
  (let ((host (read-string "Bug occurred for remote host: "
			   efs-report-default-host))
	(user (read-string "Logged in as: "
			   efs-report-default-user))
	buff-name)
    (if (string-match "^ *$" host) (setq host nil))
    (if (string-match "^ *$" user) (setq user nil))
    (if host
	(insert "\nefs believes that the host type of " host " is "
		(symbol-name (efs-host-type host))
		".\n"))
    (if efs-report-blurb
	(insert "\n" efs-report-blurb "\n"))
    (if (and host
	     user
	     (get-buffer (setq buff-name (efs-ftp-process-buffer host user)))
	     (save-window-excursion
	       (y-or-n-p
		(progn
		  (with-output-to-temp-buffer "*Help*"
		    (princ
		     (format
		      "The contents of %s
will likely very useful for identifying any bugs.

You will be given a chance to edit out any sensitive information.
Passwords are never written into this buffer." buff-name)))
		  (format "Insert contents of %s? "
			  buff-name)))))
	(let ((header-1 (concat "Contents of " buff-name ":"))
	      (header-2 "Please edit sensitive or irrelevant information."))
	  (insert "\n" header-1 "\n" header-2 "\n")
	  (insert-char ?= (max (length header-1) (length header-2)))
	  (insert "\n\n")
	  (insert-buffer-substring buff-name)
	  (insert "\n")))))

(defun efs-report-post-hook ()
  ;; Post hook run by report-submit-bug-report.
  (save-excursion
    (mail-position-on-field "subject")
    (let ((subj (read-string "Subject header: ")))
      (if (string-equal subj "")
	  (subst-char-in-region
	   (point)
	   (progn
	     (insert
	      (if (or (fboundp 'yow) (load "yow" t t)) (yow) ""))
	     (point))
	   ?\n ?\ )
	(insert subj)))))

(defun efs-report-bug (&optional default-host  default-user blurb no-confirm)
  "Submit a bug report for efs."
  (interactive)
  (let (;; reporter-confirm-p and reporter-package-abbrev appeared once
	;; as fluid vars in reporter.el.  They aren't used any longer,
	;; but we let-bind them anyway in case the user has an old version
	;; of reporter.
	(reporter-confirm-p nil)
	(reporter-prompt-for-summary-p nil)
	(reporter-package-abbrev "efs"))
    ;; Look out for old reporter versions.
    (or (boundp 'reporter-version)
	(setq reporter-version
	      "Your version of reporter is obsolete.  Please upgrade."))
    (if (or no-confirm
	    (y-or-n-p "Do you want to submit a bug report on efs? "))
	(let ((efs-report-default-host default-host)
	      (efs-report-default-user default-user)
	      (efs-report-blurb blurb)
	      (vars (nconc (efs-report-get-versions)
			   (efs-report-get-user-vars)
			   efs-report-other-vars
			   (efs-report-get-host-type-regexps)))
	      (avoids efs-report-avoid-vars)
	      path)
	  (cond
	   ((or efs-report-default-host efs-report-default-user))
	   (efs-process-host
	    (setq efs-report-default-host efs-process-host
		  efs-report-default-user efs-process-user))
	   ((setq path (or buffer-file-name
			   (and (eq major-mode 'dired-mode)
				dired-directory)))
	    (let ((parsed (efs-ftp-path path)))
	      (setq efs-report-default-host (car parsed)
		    efs-report-default-user (nth 1 parsed)))))
	  (while avoids
	    (setq vars (delq (car avoids) vars))
	    (setq avoids (cdr avoids)))
	  (reporter-submit-bug-report
	   efs-bug-address
	   "efs"
	   vars
	   (function efs-report-pre-hook)
	   (function efs-report-post-hook)
	   (aref efs-report-salutations
		 (% (+ (% (random) 1000) 1000)
		    (length efs-report-salutations))))))))

;;; end of efs-report.el
