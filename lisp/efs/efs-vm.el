;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-vm.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Allows the VM  mail reader to access folders using efs.
;;               If you are looking for support for VM/CMS, see efs-cms.el.
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Mon Nov  9 23:49:18 1992 by sandy on riemann
;; Modified:     Sun Nov 27 18:44:07 1994 by sandy on gandalf
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If vm-get-new-mail (usually bound to "g") is given a prefix, it
;; will prompt for a folder from which to collect mail. With
;; efs-vm, this folder can be in efs syntax. As is usual
;; with VM, this folder will not be deleted. If at the folder prompt,
;; you give "/user@host:", efs-vm will collect mail from the
;; spool file on the remote machine. The spool file will be deleted if
;; the mail is successfully collected. It is not necessary for
;; movemail, nor even emacs, to be installed on the remote machine.
;; The functionality of movemail is mimicked with FTP commands. Both
;; local and remote crashboxes are used, so that mail will not be lost
;; if the FTP connection is lost.
;;
;; To use efs-vm, put (require 'efs-vm) in your .vm file.
;;
;; Works for vm 5.56 through 5.72.  May not work with older versions.
;; If vm grows some file-name-handler-alist support, we should use it.
;; Actually it has.  I just haven't gotten around to this yet.

;;; Known Bugs:
;;
;;  1. efs-vm will not be able to collect mail from a spool file if
;;     you do not have write permission in the spool directory.
;;     I think that this precludes HP-UX.
;;     I hope to do something about this.
;;
;;  2. efs-vm is as clever as at can be about spool file locking.
;;     i.e. not very clever at all.  At least it uses a rename command
;;     to minimize the window for problems.  Use POP if you want to
;;     be careful.
;;

;;; Provisions, requirements, and autoloads

(provide 'efs-vm)
(require 'efs-cu)
(require 'efs-ovwrt)
(require 'vm)
;(require 'vm-folder) ; not provided
(if (or (not (fboundp 'vm-get-new-mail))
	(eq (car-safe (symbol-function 'vm-get-new-mail)) 'autoload))
    (load-library "vm-folder"))
(autoload 'efs-make-tmp-name "efs")
(autoload 'efs-del-tmp-name "efs")
(autoload 'efs-send-cmd "efs")
(autoload 'efs-re-read-dir "efs")
(autoload 'efs-copy-file-internal "efs")

;;; User variables

(defvar efs-vm-spool-files nil
  "Association list of \( USER@MACHINE . SPOOLFILES \) pairs that
specify the location of the default remote spool file for MACHINE. SPOOLFILES
is a list of remote spool files.")

(defvar efs-vm-crash-box "~/EFS.INBOX.CRASH"
  "Local file where efs keeps its local crash boxes.")

;;; Internal variables

(defconst efs-vm-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))


(defun efs-vm-get-new-mail (&optional arg)
  "Documented as original"
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (vm-error-if-folder-read-only)
  (cond
   ((null arg)
    (if (not (eq major-mode 'vm-mode))
	(vm-mode))
    (if (consp (car (vm-spool-files)))
	(message "Checking for new mail for %s..." buffer-file-name)
      (message "Checking for new mail..."))
    (let (new-messages totals-blurb)
      (if (and (vm-get-spooled-mail)
	       (setq new-messages (vm-assimilate-new-messages t)))
	  (progn
	    (if vm-arrived-message-hook
		(while new-messages
		  (vm-run-message-hook (car new-messages)
				       'vm-arrived-message-hook)
		  (setq new-messages (cdr new-messages))))
	    ;; say this NOW, before the non-previewers read
	    ;; a message, alter the new message count and
	    ;; confuse themselves.
	    (setq totals-blurb (vm-emit-totals-blurb))
	    (vm-display nil nil '(vm-get-new-mail) '(vm-get-new-mail))
	    (if (vm-thoughtfully-select-message)
		(vm-preview-current-message)
	      (vm-update-summary-and-mode-line))
	    (message totals-blurb))
	(if (consp (car (vm-spool-files)))
	    (message "No new mail for %s" buffer-file-name)
	  (message "No new mail."))
	(sit-for 4)
	(message ""))))
   (t
    (let* ((buffer-read-only nil)
	   (folder (read-file-name "Gather mail from folder: "
				   vm-folder-directory t))
	   (parsed (efs-ftp-path folder))
	   mcount new-messages totals-blurb)
      (if parsed
	  (if (string-equal (nth 2 parsed) "")
	      ;; a spool file
	      (if (not (and (efs-vm-get-remote-spooled-mail folder)
			    (setq new-messages
				  (vm-assimilate-new-messages t))))
		  (progn
		    (message
		     "No new mail, or mail couldn't be retrieved by ftp.")
		    ;; don't let this message stay up forever...
		    (sit-for 4)
		    (message ""))
		(if vm-arrived-message-hook
		    (while new-messages
		      (vm-run-message-hook (car new-messages)
					   'vm-arrived-message-hook)
		      (setq new-messages (cdr new-messages))))
		;; say this NOW, before the non-previewers read
		;; a message, alter the new message count and
		;; confuse themselves.
		(setq totals-blurb (vm-emit-totals-blurb))
		(vm-display nil nil '(vm-get-new-mail) '(vm-get-new-mail))
		(if (vm-thoughtfully-select-message)
		    (vm-preview-current-message)
		  (vm-update-summary-and-mode-line))
		(message totals-blurb))
	    
	    ;; a remote folder
	    (let ((tmp-file (car (efs-make-tmp-name nil (car parsed))))
		  (folder (expand-file-name folder)))
	      (unwind-protect
		  (progn
		    (efs-copy-file-internal
		     folder parsed tmp-file nil t nil
		     (format "Getting %s" folder)
		     ;; asynch worries me here
		     nil nil)
		    (if (and vm-check-folder-types
			     (not (vm-compatible-folder-p tmp-file)))
			(error
			 "Folder %s is not the same format as this folder."
			 folder))
		    (save-excursion
		      (vm-save-restriction
		       (widen)
		       (goto-char (point-max))
		       (insert-file-contents tmp-file)))
		    (setq mcount (length vm-message-list))
		    (if (setq new-messages (vm-assimilate-new-messages))
			(progn
			  (if vm-arrived-message-hook
			      (while new-messages
				(vm-run-message-hook (car new-messages)
						     'vm-arrived-message-hook)
				(setq new-messages (cdr new-messages))))
			  ;; say this NOW, before the non-previewers read
			  ;; a message, alter the new message count and
			  ;; confuse themselves.
			  (setq totals-blurb (vm-emit-totals-blurb))
			  (vm-display nil nil '(vm-get-new-mail)
				      '(vm-get-new-mail))
			  (if (vm-thoughtfully-select-message)
			      (vm-preview-current-message)
			    (vm-update-summary-and-mode-line))
			  (message totals-blurb)
			  ;; The gathered messages are actually still on disk
			  ;; unless the user deletes the folder himself.
			  ;; However, users may not understand what happened if
			  ;; the messages go away after a "quit, no save".
			  (setq vm-messages-not-on-disk
				(+ vm-messages-not-on-disk
				   (- (length vm-message-list)
				      mcount))))
		      (message "No messages gathered."))
		    (efs-del-tmp-name tmp-file)))))

	;; local
	
	(if (and vm-check-folder-types
		 (not (vm-compatible-folder-p folder)))
	    (error "Folder %s is not the same format as this folder."
		   folder))
	(save-excursion
	  (vm-save-restriction
	   (widen)
	   (goto-char (point-max))
	   (insert-file-contents folder)))
	(setq mcount (length vm-message-list))
	(if (setq new-messages (vm-assimilate-new-messages))
	    (progn
	      (if vm-arrived-message-hook
		  (while new-messages
		       (vm-run-message-hook (car new-messages)
					    'vm-arrived-message-hook)
		       (setq new-messages (cdr new-messages))))
	      ;; say this NOW, before the non-previewers read
	      ;; a message, alter the new message count and
	      ;; confuse themselves.
	      (setq totals-blurb (vm-emit-totals-blurb))
	      (vm-display nil nil '(vm-get-new-mail) '(vm-get-new-mail))
	      (if (vm-thoughtfully-select-message)
		  (vm-preview-current-message)
		(vm-update-summary-and-mode-line))
	      (message totals-blurb)
	      ;; The gathered messages are actually still on disk
	      ;; unless the user deletes the folder himself.
	      ;; However, users may not understand what happened if
	      ;; the messages go away after a "quit, no save".
	      (setq vm-messages-not-on-disk
		       (+ vm-messages-not-on-disk
			  (- (length vm-message-list)
			     mcount))))
	  (message "No messages gathered.")))))))

(defun efs-vm-gobble-remote-crash-box (remote-crash-box)
  (let ((remote-crash-box (expand-file-name remote-crash-box))
	(crash-box (expand-file-name efs-vm-crash-box))
	lsize)
    (if (file-exists-p vm-crash-box)
	(progn
	  ;; This should never happen, but let's make sure that we never
	  ;; clobber mail.
	  (message "Recovering messages from local crash box...")
	  (vm-gobble-crash-box efs-vm-crash-box)
	  (message "Recovering messages from local crash box... done")))
    (efs-copy-file-internal remote-crash-box (efs-ftp-path remote-crash-box)
			    crash-box nil nil nil
			    (format "Getting %s" remote-crash-box)
			    ;; asynch worries me here
			    nil nil)
    ;; only delete the remote crash box if we are sure that we have everything
    (if (and (setq lsize (nth 7 (file-attributes crash-box)))
	     (eq lsize (nth 7 (file-attributes remote-crash-box)))
	     (vm-compatible-folder-p crash-box))
	(progn
	  (vm-gobble-crash-box crash-box)
	  (delete-file remote-crash-box))
      ;; don't leave garbage in the local crash box
      (condition-case () (delete-file crash-box) (error nil))
      (error "Problem reading remote crash box %s" remote-crash-box))))

(defun efs-vm-get-remote-spooled-mail (remote-path)
  ;; remote-path is usually of the form /user@machine:
  ;; Usually vm sets inhibit-quit to t for this. This is probably
  ;; a bad idea if there is ftp activity.
  ;; I don't want to assume that the remote machine has movemail.
  ;; Try to mimic movemail with ftp commands as best as possible.
  ;; For this to work, we need to be able to create a subdirectory
  ;; in the spool directory.
  (if vm-block-new-mail
    (error "Can't get new mail until you save this folder."))
  (let* ((parsed (efs-ftp-path remote-path))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (spool-files
	  (or (cdr (assoc (concat user "@" host)
			  efs-vm-spool-files))
	      (list (concat "/usr/spool/mail/" user))))
	 got-mail)
    (while spool-files
      (let* ((s-file (car spool-files))
	     (spool-file (format efs-path-format-string user host s-file))
	     ;; rmdir and mkdir bomb if this path ends in a /.
	     (c-dir (concat s-file ".CRASHBOX"))
	     (rc-file (concat c-dir "/CRASHBOX"))
	     (crash-dir (concat spool-file ".CRASHBOX/"))
	     (remote-crash-file (concat crash-dir "CRASHBOX"))
	     (crash-box (expand-file-name efs-vm-crash-box)))
	(if (file-exists-p crash-box)
	    (progn
	      (message "Recovering messages from crash box...")
	      (vm-gobble-crash-box crash-box)
		 (message "Recovering messages from crash box... done")
		 (setq got-mail t)))
	(if (let ((efs-allow-child-lookup nil))
	      (file-exists-p remote-crash-file))
	    (progn
	      (message "Recovering messages from remote crash box...")
	    (efs-vm-gobble-remote-crash-box remote-crash-file)
	    (message "Recovering messages from remote crash box... done")
	    (setq got-mail t)))
	(if (file-exists-p crash-box)
	    (progn
	      (message "Recovering messages from crash box...")
	      (vm-gobble-crash-box crash-box)
	      (message "Recovering messages from crash box... done")
	      (setq got-mail t)))
	(unwind-protect
	    (if (car
		 (efs-send-cmd
		  host user (list 'mkdir c-dir)
		  (format "Making crash directory %s" crash-dir)))
		(progn
		  (efs-re-read-dir crash-dir)
		  (message "Unable to make crash directory %s" crash-dir)
		  (ding))
	      (or (car
		   (efs-send-cmd host user (list 'rename s-file rc-file)
				 (format "Checking spool file %s" spool-file)))
		  (progn
		    (message "Getting new mail from %s..." spool-file)
		    ;; The rename above wouldn't have updated the cash.
		    (efs-re-read-dir crash-dir)
		    (efs-vm-gobble-remote-crash-box remote-crash-file)
		    (message "Getting new mail from %s... done" spool-file)
		    (setq got-mail t))))
	  (condition-case nil
	      (efs-send-cmd
	       host user (list 'rmdir c-dir)
	       "Removing crash directory")
	    (error nil))))
      (setq spool-files (cdr spool-files)))
    got-mail))

;;; Overwrite existing functions

(efs-overwrite-fn "efs" 'vm-get-new-mail)

;;; end of efs-vm.el
