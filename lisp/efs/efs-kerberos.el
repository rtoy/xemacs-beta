;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-efs-kerberos.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:
;; Description:  Support for Kerberos gateways.
;; Author:       Sandy Rutherford <sandy@gandalf.sissa.it>
;; Created:      Thu Nov 24 21:19:25 1994 by sandy on gandalf
;; Modified:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Support for the Kerberos gateway authentication system from MIT's
;;; Project Athena.

(provide 'efs-kerberos)
(require 'efs)

(defconst efs-kerberos-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;; Internal Variables

(defvar efs-kerberos-passwd-sent nil)
;; Set to t after the passwd has been sent.
(defvar efs-kerberos-output "")
;; Holds the output lines from the kinit process.
(defvar efs-kerberos-buffer-name "*efs kerberos*")
;; Buffer where kinit output is logged.
(defvar efs-kerberos-passwd-prompt-regexp "^Password: *$")
;; Regular expression to match prompt used by the kinit program.
(defvar efs-kerberos-failed-msgs "[^ ]+")
;; Regular expression to match output for an invalid kinit ticket password.
;; Is this too general?
(defvar efs-kerberos-passwd-failed nil)
;; Whether the kinit command worked.
(defvar efs-kerberos-passwd-retry nil)

;;; Code

(defun efs-kerberos-process-filter (proc str)
  ;; Process filter for the kinit process.
  (setq efs-kerberos-output (concat efs-kerberos-output str))
  (let ((buff (get-buffer (process-buffer proc))))
    (if buff
	(efs-save-buffer-excursion
	  (set-buffer buff)
	  (efs-save-match-data
	    (goto-char (point-max))
	    (while (string-match "\n" efs-kerberos-output)
	      (let ((line (substring efs-kerberos-output 0
				     (match-beginning 0))))
		(insert line "\n")
		(and efs-kerberos-passwd-sent
		     (string-match efs-kerberos-failed-msgs line)
		     (setq efs-kerberos-passwd-failed t)))
	      (setq efs-kerberos-output (substring efs-kerberos-output
						   (match-end 0))))
	    (and (null efs-kerberos-passwd-sent)
		 (string-match efs-kerberos-passwd-prompt-regexp
			       efs-kerberos-output)
		 (memq (process-status proc) '(run open))
		 (let ((passwd (or
				(efs-lookup-passwd efs-gateway-host "kerberos")
				(read-passwd
				 (if efs-kerberos-passwd-retry
				     "Password failed.  Try again: "
				   (format "Kerberos password for %s: "
					   efs-gateway-host))))))
		   (unwind-protect
		       (progn
			 (insert efs-kerberos-output)
			 (setq efs-kerberos-output "")
			 (process-send-string proc passwd)
			 (insert "Turtle Power!\n"))
		     (fillarray passwd 0)))))))))

(defun efs-kerberos-get-ticket ()
  ;; Gets a kerbos ticket.  The password is actually sent by the process
  ;; filter.
  (let ((mess (format "Getting kerberos ticket for %s..." efs-gateway-host)))
    (message mess)
    (setq efs-kerberos-passwd-failed nil
	  efs-kerberos-passwd-sent nil
	  efs-kerberos-output "")
    (condition-case nil (delete-process "*efs kerberos*") (eror nil))
    (let* ((program (or (nth 3 efs-gateway-type) "kinit"))
	   (args (nth 4 efs-gateway-type))
	   (proc (apply 'start-process
			"*efs kerberos*" efs-kerberos-buffer-name
			program args)))
      (set-process-filter proc (function efs-kerberos-process-filter))
      ;; Should check for a pty, but efs-pty-check will potentially eat
      ;; important output.  Need to wait until Emacs 19.29 to do this properly.
      (while (memq (process-status proc) '(run open))
	(accept-process-output proc))
      (if efs-kerberos-passwd-failed
	  (let ((efs-kerberos-passwd-failed t))
	    (efs-kerberos-get-ticket))))
    (message "%sdone" mess)))

(defun efs-kerberos-login (host user proc)
  ;; Open a connection using process PROC to HOST adn USER, using a
  ;; kerberos gateway.  Returns the process object of the connection.
  ;; This may not be PROC, if a ticket collection was necessary.
  (let ((to host)
	result port cmd)
    (if (string-match "#" host)
	(setq to (substring host 0 (match-beginning 0))
	      port (substring host (match-end 0))))
    (and efs-nslookup-on-connect
	 (string-match "[^0-9.]" to)
	 (setq to (efs-nslookup-host to)))
    (setq cmd (concat "open " to))
    (if port (setq cmd (concat cmd " " port)))
    (setq result (efs-raw-send-cmd proc cmd))
    (while (and (car result)
		(string-match "\\bcannot authenticate to server\\b"
			      (nth 1 result)))
      (let ((name (process-name proc)))
	(condition-case nil (delete-process proc) (error nil))
	(efs-kerberos-get-ticket)
	(setq proc (efs-start-process host user name)
	      result (efs-raw-send-cmd proc cmd))))
    (if (car result)
	(progn
	  (condition-case nil (delete-process proc) (error nil))
	  (efs-error host user (concat "OPEN request failed: "
				       (nth 1 result)))))
    proc))

;;; End of efs-kerberos.el
