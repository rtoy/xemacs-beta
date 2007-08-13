;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-gwp.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Support for efs to use an interactive gateway.
;; Author:       Andy Norman, Dawn
;; Created:      Thu Mar 18 13:03:14 1993
;; Modified:     Sun Nov 27 18:31:50 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-gwp)
(require 'efs)

;;;; ------------------------------------------------------------
;;;; Interactive gateway program support.
;;;; ------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Variables and Documentation

(defvar efs-gwp-setup-term-command
  (if (eq system-type 'hpux)
      "stty -onlcr -echo\n"
    "stty -echo nl\n")
  "Command to do terminal setup on the gateway machine.
They must stop the terminal echoing each command and strip out trailing 
^M characters. This string must end in \\n. If you need to send multiple
commands, include them all in this string, separated by \\n.
See the documentation in efs.el for some example commands.")

;; About efs-gwp-term-setup-command:
;; 
;; It is important to get efs-gwp-setup-term-command right.
;; Here are some examples.  Please tell us about which commands
;; to use on other platforms, so that we can include it in the
;; documentation.
;;
;;
;; HP-UX:
;; 
;;     "stty -onlcr -echo\n"
;;
;; SunOS:
;; 
;;     "stty -echo nl\n"
;;
;; VMS: (this should work)
;;
;;     "set terminal/noecho\n"
;;


(defvar efs-gwp-prompt-pattern "^[^#$%>;]*[#$%>;] *"
  "*Regexp used to detect that the gateway login sequence has completed.
It will be assumed that the shell is ready to receive input.  Make this
regexp as strict as possible; it shouldn't match *anything* at all except
the shell's initial prompt.  The above string will fail under most SUN-3's
since it matches the login banner.")

;; About efs-gwp-prompt-pattern:
;;
;; It is very important that this not match anything in the machine's
;; login banner.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Internal Variables

(defconst efs-gwp-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

(defvar efs-gwp-running t)
(defvar efs-gwp-status nil)
(defvar efs-gwp-string "")

;;; Entry point (defined as an autoload in efs.el)

(defun efs-gwp-start (host user name)
  "Login to the gateway machine and fire up an ftp process."
  (message "Connecting to gateway %s..." efs-gateway-host)
  (let ((proc (apply 'start-process name (efs-ftp-process-buffer host user)
		     (nth 1 efs-gateway-type)
		     (append (nth 2 efs-gateway-type)
			     (list efs-gateway-host))))
	(ftp (concat (nth 3 efs-gateway-type) " "
		     (mapconcat (function identity) (nth 4 efs-gateway-type)
				" ") "\n")))
    (process-kill-without-query proc)
    (set-process-sentinel proc (function efs-gwp-sentinel))
    (set-process-filter proc (function efs-gwp-filter))
    (set-marker (process-mark proc) (point))
    (setq efs-gwp-running t
	  efs-gwp-status nil
	  efs-gwp-string "")
    (while efs-gwp-running		;perform login sequence
      (accept-process-output proc))
    (if (not efs-gwp-status)
	(efs-error host user "unable to login to gateway"))
    (message "Connecting to gateway %s...done" efs-gateway-host)
    (setq efs-gwp-running t
	  efs-gwp-status nil
	  efs-gwp-string "")
    (process-send-string proc efs-gwp-setup-term-command)
    (while efs-gwp-running		;zap ^M's and double echoing.
      (accept-process-output proc))
    (if (not efs-gwp-status)
	(efs-error host user "unable to set terminal modes on gateway"))
    (setq efs-gwp-running t
	  efs-gwp-status nil
	  efs-gwp-string "")
    (message "Opening FTP connection to %s..." host)
    (process-send-string proc ftp)
    proc))

;;; Process filter/sentinel

(defun efs-gwp-sentinel (proc str)
  (setq efs-gwp-running nil))

(defun efs-gwp-filter (proc str)
  (efs-save-match-data
    ;; Don't be sensitive to login vn LOGIN.
    (let ((case-fold-search t))
      (efs-process-log-string proc str)
      (setq efs-gwp-string (concat efs-gwp-string str))
      (cond ((string-match "\\(login\\|username\\): *$" efs-gwp-string)
	     (process-send-string proc
				  (concat
				   (let ((efs-default-user t))
				     (efs-get-user efs-gateway-host))
				   "\n")))
	    ((string-match "password: *$" efs-gwp-string)
	     (process-send-string proc
				  (concat
				   (efs-get-passwd efs-gateway-host
						   (efs-get-user
						    efs-gateway-host))
				   "\n")))
	    ((string-match efs-gateway-fatal-msgs
			   efs-gwp-string)
	     (delete-process proc)
	     (setq efs-gwp-running nil))
	    ((string-match efs-gwp-prompt-pattern
			   efs-gwp-string)
	     (setq efs-gwp-running nil
		   efs-gwp-status t))))))

;;; end of efs-gwp.el
