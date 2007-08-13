;;;; -*-emacs-lisp-*-
;;;; EMACS interface for GNATS.
;;;;  Copyright (C) 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
;;;;  Contributed by Brendan Kehoe (brendan@cygnus.com)
;;;;   based on an original version by Heinz G. Seidl (hgs@ide.com).
;;;;
;;;; This file is part of GNU GNATS.
;;;;
;;;; GNU GNATS is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; GNU GNATS is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU GNATS; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, 59 Temple Place - Suite 330,
;;;; Boston, MA 02111, USA.  */

;;;; This file provides `edit-pr', `view-pr' `query-pr', for changing and
;;;; searching problem reports that are part of the GNATS database.  See the
;;;; gnats(1) man page or the GNATS documentation for further information.

(provide 'gnats)
(require 'send-pr)			;Shared stuff defined there

;;;;---------------------------------------------------------------------------
;;;; Customization: put the following forms into your default.el file
;;;; (or into your .emacs) and the whole file into your EMACS library.
;;;;---------------------------------------------------------------------------

;(autoload 'edit-pr "gnats"
;	  	  "Command to edit a problem report." t)

;(autoload 'view-pr "gnats"
;	  	  "Command to view a problem report." t)

;(autoload 'gnats-mode "gnats"
;	  "Major mode for editing of problem reports." t)

;(autoload 'query-pr "gnats"
;	  	  "Command to query information about problem reports." t)

;(autoload 'summ-pr "gnats"
;  "Command to display a summary listing of problem reports." t)

;;;;---------------------------------------------------------------------------
;;;; To reply by mail within gnats-mode
;;;;---------------------------------------------------------------------------

(defvar gnats-mailer 'mail
  "*Specifiles either `mail' or `mhe' as mailer for GNATS")
  
;; Provides mail reply and mail other window command using usual mail
;; interface and mh-e interface.
;;
;; To use MAIL: set the variable
;; `gnats-mailer' to `mail'
;;
;; To use MH-E: set the variable 
;; `gnats-mailer' to  `mhe'

(autoload 'mail "sendmail")
(autoload 'mail-other-window "sendmail")
(autoload 'mail-fetch-field "mail-utils")
(autoload 'rmail-dont-reply-to "mail-utils")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-send "sendmail")
(autoload 'mail-send-and-exit "sendmail")

(autoload 'mh-send "mh-e")
(autoload 'mh-send-other-window "mh-e")
(autoload 'mh-find-path "mh-e")
(autoload 'mh-get-field "mh-e")
(autoload 'mh-insert-fields "mh-e")
(defvar mh-show-buffer nil)
(defvar mh-sent-from-folder nil)
(defvar mh-sent-from-msg nil)

;;; User options

(defvar gnats:keep-edited-buffers t
  "*If non-nil, then PR buffers are kept with distinct names after
editing.  Otherwise, only the the most recently edited PR is kept.")

(defvar gnats:keep-sent-messages 1
  "*Non-nil value N causes GNATS to keep the last N messages sent from GNATS.
A value of 0 or nil causes GNATS never to keep such buffers.  A value of t
causes GNATS to keep all such buffers.")

(defvar gnats:network-server nil
  "*If non-nil, names the GNATS network server for remote queries and editing.")

(defvar gnats:run-in-background t
  "*If non-nil, make GNATS programs run in the background allowing the emacs to continue to other things.")

(defvar gnats:bury-edited-prs t
  "*If non-nil, then PR buffers are buried after editing.  Otherwise, they are left at the top of the buffer list.")

;;; emacs 19 uses compile-internal, emacs 18 uses compile1
(if gnats::emacs-19p
    (autoload 'compile-internal "compile")
  (autoload 'compile1 "compile")
  (fset 'compile-internal 'compile1))

;;; Misc constants.

;;(defvar gnats:root "/usr/share/gnats/gnats-db"
;;  "*The top of the tree containing the GNATS database.")

(defvar gnats:libdir (or (gnats::get-config "LIBDIR") "/usr/lib")
  "*Where the `gnats' subdirectory lives for things like pr-edit.")

(defvar gnats:addr (or (gnats::get-config "GNATS_ADDR") "bugs")
  "*Where bug reports are sent.")

(defvar gnats::version
  (concat "Version " (or (gnats::get-config "VERSION") "3.101")))

(defvar gnats::diffopt (or (gnats::get-config "DIFFOPT") "-u")
  "How to get human-friendly output from diff(1).")

(defvar gnats::categories nil
  "List of GNATS categories; computed at runtime.")

(defvar gnats::responsibles nil
  "List of GNATS responsibles; Computed at runtime.")

(defvar gnats::submitters nil
  "List of GNATS submitters; Computed at runtime.")

;;;###autoload
(defvar gnats::mode-name nil
  "Name of the GNATS mode.")

(defconst gnats::err-buffer "*gnats-error*"
  "Name of the temporary buffer, where gnats error messages appear.")

;;(defconst gnats::indent 17 "Indent for formatting the value.")

(defvar gnats:::pr-locked nil
  "Buffer local flag representing whether the associated pr is locked.")

(defvar gnats:::pr-errors nil
  "Buffer local buffer holding any errors from attempting to file this pr.")

(defvar gnats:::buffer-pr nil
  "Buffer local name of this pr.")

(defvar gnats:::current-pr nil
  "Buffer local value of the current pr.")

(defvar gnats:::do-file-pr nil
  "Buffer local value; if T, file the current pr.")

(defvar gnats:::force nil
  "Buffer local value; if T, ignore errors unlocking the current pr.")

(defvar gnats:::pr-buffer nil
  "Buffer local value of the buffer containing the pr.")

(defvar gnats:::audit-trail nil
  "Buffer local audit trail for the current pr.")

(defvar gnats:::backupname nil
  "Buffer local name of the backup file for this pr.")

(defvar gnats:::start-of-PR-fields nil
  "Buffer position of the beginning of the PR fields.")

(defvar gnats:::newfile nil
  "File used to store the results of npr-edit.")

(defvar gnats:::query-pr "query-pr"
  "The program name used to query problem reports.")

(defvar gnats:::nquery-pr "nquery-pr"
  "The program name used to query problem reports over the network.")

(defvar gnats:::query-regexp "n?query-pr:"
  "The regular expression to use to recognize a message from the query program.")

;; For example:
;;  (setq gnats:::types '( ( "Games" ( "/gnats/games"  "/usr/gamesdb/H-sun4/lib ")
;;                        ( "Tools" ( "/usr/toolsdb" "/usr/local/lib" ))))
(defvar gnats:::types nil
  "Alist of each type of GNATS database and its root and libdir settings.")

(defconst gnats::fields
  (let (fields)
    (setq
     fields
     ;; Duplicate send-pr::fields, don't just include it.
     ;; is there a better way than this?
     (append (read (prin1-to-string send-pr::fields))
	     '(("Arrival-Date" nil nil text)
	       ("Customer-Id")
	       ("Number" nil nil number)
	       ("Responsible" gnats::set-responsibles nil enum
		gnats::update-audit-trail)
	       ("State"
		(("open") ("analyzed") ("feedback") ("suspended") ("closed"))
		(lambda (x) (or (cdr (assoc x gnats::state-following)) ""))
		enum gnats::update-audit-trail))))
    ;; (setf (second (assoc "Category" fields)) 'gnats::set-categories)
    (setcar (cdr (assoc "Category" fields)) 'gnats::set-categories)
    (setcdr (nthcdr 3 (assoc "Category" fields))
	    '(gnats::update-responsible))
    (setcar (cdr (assoc "Class" fields))
	    '(("sw-bug") ("doc-bug") ("change-request") ("support")
	      ("mistaken") ("duplicate")))
    (setcdr (assoc "Submitter-Id" fields) '(gnats::set-submitters t enum))
    (setcdr (assoc "Customer-Id" fields) (cdr (assoc "Submitter-Id" fields)))
    fields)
  "AList of one-line PR fields and their possible values.")

(defconst gnats::state-following 
  '(("open"      . "analyzed")
    ("analyzed"  . "feedback")
    ("feedback"  . "closed")
    ("suspended" . "analyzed"))
  "A list of states and possible following states (does not describe all
possibilities).")

(defvar gnats::query-pr-history nil
  "Past arguments passed to the query-pr program.")

(defvar gnats::tmpdir (or (getenv "TMPDIR") "/tmp/")
  "Directory to use for temporary files.")

;;;;---------------------------------------------------------------------------
;;;; hooks
;;;;---------------------------------------------------------------------------

;; we define it here in case it's not defined
(or (boundp 'text-mode-hook) (setq text-mode-hook nil))
(defvar gnats-mode-hook text-mode-hook "Called when gnats mode is switched on.")

;;;;---------------------------------------------------------------------------
;;;; Error conditions
;;;;---------------------------------------------------------------------------

(put 'gnats::error 'error-conditions '(error gnats::error))
(put 'gnats::error 'error-message "GNATS error")

;; pr-edit --check was unhappy
(put 'gnats::invalid-fields 'error-conditions
     '(error gnats::error gnats::invalid-fields))
(put 'gnats::invalid-fields 'error-message "invalid fields in PR")
(put 'gnats::invalid-date 'error-conditions
     '(error gnats::error gnats::invalid-date))
(put 'gnats::invalid-date 'error-message "invalid date value")

;; pr-addr couldn't find an appropriate address
(put 'gnats::invalid-name 'error-conditions
     '(error gnats::error gnats::invalid-name))
(put 'gnats::invalid-name 'error-message "could not find the requested address")

;; what pr?
(put 'gnats::no-such-pr 'error-conditions '(error gnats::error gnats::no-such-pr))
(put 'gnats::no-such-pr 'error-message "PR does not exist")

;;
(put 'gnats::no-such-category 'error-conditions
     '(error gnats::error gnats::no-such-category))
(put 'gnats::no-such-category 'error-message "No such category")

;; there is no lock on that pr
(put 'gnats::pr-not-locked 'error-conditions
     '(error gnats::error gnats::pr-not-locked))
(put 'gnats::pr-not-locked 'error-message "No one is locking the PR")

;; there is a lock on that pr
(put 'gnats::locked-pr 'error-conditions '(error gnats::error gnats::locked-pr))
(put 'gnats::locked-pr 'error-message "PR locked by")

;; GNATS is locked
(put 'gnats::locked 'error-conditions '(error gnats::error gnats::locked))
(put 'gnats::locked 'error-message "GNATS is locked by another process---try again later.")

;; We can't lock GNATS.
(put 'gnats::cannot-lock 'error-conditions '(error gnats::error gnats::locked))
(put 'gnats::cannot-lock 'error-message "cannot lock GNATS; try again later.")

;;;;---------------------------------------------------------------------------
;;;; GNATS mode
;;;;---------------------------------------------------------------------------

(defvar gnats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'gnats:next-field)
    (define-key map "\M-p" 'gnats:previous-field)
    (define-key map "\C-\M-b" 'gnats:backward-field)
    (define-key map "\C-\M-f" 'gnats:forward-field)
    (define-key map "\C-c\C-a" 'gnats:mail-reply)
    (define-key map "\C-c\C-c" 'gnats:submit-pr)
    (define-key map "\C-c\C-e" 'gnats:edit-pr)
    (define-key map "\C-c\C-f" 'gnats:change-field)
    (define-key map "\C-c\C-m" 'gnats:mail-other-window)
    (define-key map "\C-c\C-q" 'gnats:unlock-buffer-force)
    (define-key map "\C-c\C-r" 'gnats:responsible-change-from-to)
    (define-key map "\C-c\C-s" 'gnats:state-change-from-to)
    (define-key map "\C-c\C-t" 'gnats:category-change-from-to)
    (define-key map "\C-c\C-u" 'gnats:unlock-pr)
    (or gnats::emacs-19p
	(define-key map "\C-xk" 'gnats:kill-buffer))
    map)
  "Keymap for gnats mode.")

(defsubst gnats::get-header (field)
  "Fetch the contents of mail header FIELD."
  (funcall (nth 4 (assoc gnats-mailer gnats::mail-functions)) field))

(defun gnats:submit-pr ()
  "Save the current PR into the database and unlock it.

This function has no effect if the PR is not locked.

Three cases:
      State change
      Responsibility change
      Other change (only interesting if the PR was changed by somebody 
                    other than the Reponsible person)

State changes are sent to the originator
Responsibility changes are sent to the new responsible person
Other changes are sent to the responsible person."
  ;;
  (interactive)
  (cond ((not (eq major-mode 'gnats:gnats-mode))
	 (error "edit-pr: not in GNATS mode.")) 
	(gnats:::pr-locked
	 (gnats::check-pr-background t)
	 (if gnats:run-in-background (bury-buffer)))
	  ;; If not locked, do nothing
	(t
	 (message "edit-pr: PR not locked."))))

;;;###autoload
(setq gnats::mode-name 'gnats:gnats-mode)

(defun gnats::rename-buffer ()
  (let ((category (gnats::field-contents "Category"))
	(number   (gnats::field-contents "Number"))
	buf)
    (setq gnats:::buffer-pr (format "%s/%s" category number))
    (and (setq buf (get-buffer gnats:::buffer-pr))
	 (save-excursion
	   (set-buffer buf)
	   (set-buffer-modified-p nil)
	   (kill-buffer buf)))
    (rename-buffer gnats:::buffer-pr)))

;; FIXME allow re-lock of unlocked PR
;; FIXME too many assumptions -- make more independent of edit-pr
;;;###autoload
(fset 'gnats-mode gnats::mode-name)
;;;###autoload
(defun gnats:gnats-mode ()
  "Major mode for editing problem reports.
For information about the form see gnats(1) and pr_form(5).

When you are finished editing the buffer, type \\[gnats:submit-pr] to commit
your changes to the PR database.  To abort the edit, type
\\[gnats:unlock-buffer].

Special commands:
\\{gnats-mode-map}
Turning on gnats-mode calls the value of the variable gnats-mode-hook,
if it is not nil."
  (gnats::patch-exec-path)		;Why is this necessary? --jason
  (gnats::set-categories)
  (gnats::set-responsibles)
  (gnats::set-submitters)
  (put gnats::mode-name 'mode-class 'special)
  (kill-all-local-variables)
  (setq major-mode gnats::mode-name)
  (setq mode-name "gnats")
  (use-local-map gnats-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (make-local-variable 'gnats:::start-of-PR-fields)
  (make-local-variable 'gnats:::pr-errors)
  (make-local-variable 'gnats:::buffer-pr)
  (gnats::rename-buffer)

  ;; we do this in gnats:edit-pr for the network version
  (if (not gnats:network-server)
      (progn
	(setq gnats:::backupname (gnats::make-temp-name))
	(copy-file (format "%s/%s" gnats:root gnats:::buffer-pr)
		   gnats:::backupname)))
  
  ;; edit-pr locks it for us
  (make-local-variable 'gnats:::pr-locked)
  (setq gnats:::pr-locked t)

  (cond (gnats::emacs-19p
	 (make-local-variable 'kill-buffer-hook)
	 (add-hook 'kill-buffer-hook 'gnats::kill-buffer-hook)))

  ; If they do C-x C-c, unlock all of the PRs they've edited.
  (if (fboundp 'add-hook)
      (add-hook 'kill-emacs-hook 'gnats::unlock-all-buffers)
    (setq kill-emacs-hook 'gnats::unlock-all-buffers))

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat (default-value 'paragraph-separate)
				    "\\|" gnats::keyword "$"))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat (default-value 'paragraph-start)
				"\\|" gnats::keyword))
  (make-local-variable 'gnats:::audit-trail)
  (goto-char (point-min))
  (search-forward "\n>Number:")
  (beginning-of-line)
  (setq gnats:::start-of-PR-fields (point-marker))
  (run-hooks 'gnats-mode-hook))

;;;;---------------------------------------------------------------------------
;;;; Mail customization
;;;;---------------------------------------------------------------------------

(or (string-match mail-yank-ignored-headers "X-mode:")
    (setq mail-yank-ignored-headers 
	  (concat "^X-mode:" "\\|" mail-yank-ignored-headers)))

(defconst gnats::mail-functions
  '((mail gnats::mail-other-window-using-mail
	  gnats::mail-reply-using-mail
	  gnats::mail-PR-changed-mail-setup
	  gnats::get-header-using-mail-fetch-field)
    (mhe  gnats::mail-other-window-using-mhe
	  gnats::mail-reply-using-mhe
	  gnats::mail-PR-changed-mhe-setup
	  gnats::get-header-using-mhe))
  "An association list of mailers and the functions that use them.
The functions are supposed to implement, respectively:
gnats::mail-other-window
gnats::mail-reply
gnats::mail-PR-changed-setup
gnats::get-header")

;;;;---------------------------------------------------------------------------
;;;; Toplevel functions and vars, to reply with mail within gnats-mode
;;;;---------------------------------------------------------------------------

(defun gnats:mail-other-window ()
  "Compose mail in other window.
Customize the variable `gnats-mailer' to use another mailer."
  ;;
  (interactive)
  (funcall (nth 1 (assoc gnats-mailer gnats::mail-functions))))

(defun gnats:mail-reply (&optional just-sender)
  "Reply mail to PR Originator.
Customize the variable `gnats-mailer' to use another mailer.
If optional argument JUST-SENDER is non-nil, send response only to
original submitter of problem report."
  ;;
  (interactive "P")
  (funcall (nth 2 (assoc gnats-mailer gnats::mail-functions)) just-sender))

;;;; common (and suppport) functions 

(defun gnats::isme (addr)
  (setq addr (mail-strip-quoted-names addr))
  (or (string= addr (user-login-name))
      (string= addr (concat (user-login-name) "@" (system-name)))))
  
(defsubst gnats::mail-PR-changed-setup (to subject cc buffer)
  (funcall (nth 3 (assoc gnats-mailer gnats::mail-functions))
	   to subject cc buffer))

(defun gnats::mail-PR-changed-mail-setup (to subject cc buffer)
  (or (gnats::vmish-mail
       (format "notification of changes to PR %s" gnats:::buffer-pr)
       nil to subject nil cc buffer)
      (error "Submit aborted; PR is still locked.")))

(defun gnats::mail-PR-changed-mhe-setup (to subject cc buffer)
  (let ((config (current-window-configuration))
	(pop-up-windows t)
	draft)
    (mh-find-path)
    (let ((pop-up-windows t))
      (mh-send-sub to (or cc "") subject config))
    (switch-to-buffer (current-buffer))
    (setq mh-sent-from-folder buffer
	  mh-sent-from-msg 1
	  mh-show-buffer buffer)))

(defun gnats::mail-PR-changed (user responsible resp-change state-change notify)
  "- Send mail to the responsible person if the PR has been changed
  by someone else
- Send mail to the originator when the state is changed.
- Send mail to old and new responsible people when the responsibility is
  transferred.
  `resp-change' is the list (old-resp new-resp start end)
- Send mail to any other parties in NOTIFY."
  ;; This function is really ugly !
  ;;
  (let ((to nil)
	(cc nil)
	(subn nil) (subm nil)
	(subject (gnats::get-reply-subject))
	(buffer  (current-buffer))
	(pr-change (not (or resp-change state-change)))
	(pr-backupname gnats:::backupname)
	)
    ;; Here we find out where to send the mail to
    (let (to-resp to-new-resp to-submitter to-bugs resp-addr new-resp-addr)
      (if pr-change (setq to-resp t to-bugs t)
	(if resp-change (setq to-resp t to-new-resp t))
	(if state-change (setq to-submitter t to-resp t)))
      (cond (to-new-resp
	     (setq new-resp-addr (gnats::pr-addr (car resp-change)))
	     (if (gnats::isme new-resp-addr)
		 (setq to-new-resp nil))))
      (cond (to-resp
	     (setq resp-addr (gnats::pr-addr responsible))
	     (if (gnats::isme resp-addr)
		 (setq to-resp nil))))
      (cond (to-submitter
	     (setq cc to)
	     (setq to (list (gnats::get-reply-to)))))
      (if to-resp (gnats::push resp-addr to))
      (if to-new-resp (gnats::push new-resp-addr to))
      (setq subm (or (gnats::field-contents "Customer-Id")
		     (gnats::field-contents "Submitter-Id")))
      (if subm
	  (progn
	    (setq subn (nth 5 (assoc subm gnats::submitters)))
	    (if (not (string= subn ""))
		(gnats::push subn cc))))
      (if to-bugs (gnats::push gnats:addr cc))
      (if notify (gnats::push notify cc))
      (setq to (mapconcat 'identity to ", ")
	    cc (mapconcat 'identity cc ", "))
      (if (string= cc "") (setq cc nil)))
    (gnats::mail-PR-changed-setup to subject cc buffer)
    ;; now we assume that the current buffer is the mail buffer
    (goto-char (point-max))
    (if pr-change
	(progn
	  (insert 
	   (format "\n\t`%s' made changes to this PR.\n\n" (user-full-name)))
	  (if (and pr-backupname (file-readable-p pr-backupname))
	      (let ((file (gnats::make-temp-name))
		    (default-directory (gnats::find-safe-default-directory)))
		(save-excursion
		  (set-buffer buffer)
		  (write-region (point-min) (point-max) file))
		(call-process "diff" nil t t gnats::diffopt
			      pr-backupname file)
		(delete-file file))))
      (if resp-change
	  (progn
	    (insert (format "\n\t`%s' changed the responsibility to `%s'.\n" 
			    (user-full-name) responsible))
	    (insert-buffer-substring buffer 
				     (nth 2 resp-change) 
				     (nth 3 resp-change)))
	(if state-change
	    (progn
	      (insert (format "\n\t`%s' changed the state to `%s'.\n" 
			      (user-full-name) (nth 1 state-change)))
	      (insert-buffer-substring buffer 
				       (nth 2 state-change)
				       (nth 3 state-change))))))
    ))

(defsubst gnats::bm (num)
  (buffer-substring (match-beginning num) (match-end num)))

(defun gnats::real-pr-addr (name)
  (if (zerop (length name))
      nil
    (let ((buf (generate-new-buffer gnats::err-buffer)))
      (unwind-protect
	  (save-excursion
	    (let ((default-directory (gnats::find-safe-default-directory)))
	      (call-process (format "%s/gnats/pr-addr" gnats:libdir)
			    nil buf nil "-F" name))
	    (set-buffer buf)
	    (goto-char (point-min))
	    (cond ((looking-at "pr-addr: could not find the requested address")
		   nil)
		  ((looking-at "^\\([^:]*\\):\\([^:]*\\):\\([^:]*\\)\n")
		   (list (gnats::bm 1) (gnats::bm 2) (gnats::bm 3)))
		  (t (signal 'gnats::error
			     (list (buffer-substring (point-min)
						     (1- (point-max))))))))
	(kill-buffer buf)))))

(defun gnats::pr-addr (name)
  "Find the e-mail address corresponding to maintainer NAME."
  (let (entry addr)
    (or (setq entry (assoc name gnats::responsibles))
	(and (setq entry (gnats::real-pr-addr name))
	     (gnats::push entry gnats::responsibles))
	(signal 'gnats::invalid-name (list name)))
    (setq addr (if (zerop (length (nth 2 entry)))
		   name
		 (nth 2 entry)))
    (if (zerop (length (nth 1 entry)))
	addr
      (concat (nth 1 entry) " <" addr ">"))))

(defun gnats::get-header-using-mail-fetch-field (field)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (narrow-to-region (point-min) (point))
      (mail-fetch-field field))))
      
(defun gnats::get-header-using-mhe (field)
  (save-excursion
    (let ((ret (mh-get-field (concat field ":"))))
      (if (string= ret "")
	  nil
	ret))))

(defun gnats::get-reply-to ()
  (or (gnats::get-header "Reply-To")
      (gnats::get-header "From")))

(defun gnats::get-reply-subject ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((category (gnats::field-contents "Category"))
	    (number   (gnats::field-contents "Number"))
	    (synopsis (gnats::field-contents "Synopsis" 0))
	    (subject))
	(goto-char (point-min))
	(narrow-to-region (point-min)
			  (progn (search-forward "\n\n" nil 'move)
				 (point-marker)))
	(setq subject (mail-fetch-field "subject"))
	(if (and synopsis (not (equal synopsis "")))
	    (format "Re: %s/%s: %s" category number synopsis)
	  (format "Re: %s/%s: %s" category number subject))))))

(defun gnats::make-in-reply-to-field (from date msg-id)
  (concat
   (substring from 0 (string-match "  *at \\|  *@ \\| *(\\| *<" from))
   "'s message of " date
   (if (not (equal msg-id ""))
       (concat "\n             " msg-id)
     "")))

;;;; Send mail using sendmail mail mode.

(defun gnats::mail-reply-using-mail (just-sender)
  ;;
   "Mail a reply to the originator of the PR.
Normally include CC: to all other recipients of original message;
argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
   ;;
   (let (from cc subject date to reply-to msg-id)
     (save-excursion
       (save-restriction
	 (widen)
	 (narrow-to-region (point-min) (progn (goto-char (point-min))
					      (search-forward "\n\n")
					      (- (point) 1)))
	 (setq from       (mail-fetch-field "from" nil t)
	       subject    (gnats::get-reply-subject)
	       reply-to   (or (mail-fetch-field "reply-to" nil t)
			      from)
	       date       (mail-fetch-field "date" nil t)
	       cc         (cond (just-sender nil)
				(t (mail-fetch-field "cc" nil t)))
	       to         (or (mail-fetch-field "to" nil t)
			      (mail-fetch-field "apparently-to" nil t)
			      "")
	       msg-id     (mail-fetch-field "message-id")
	       )))
     (gnats::vmish-mail-other-window
      (format "reply to PR %s" gnats:::buffer-pr)
      nil (mail-strip-quoted-names reply-to) subject
      (gnats::make-in-reply-to-field from date msg-id)
      (if just-sender
	  nil
	(let* ((cc-list (rmail-dont-reply-to (mail-strip-quoted-names
					      (if (null cc) to 
						(concat to ", " cc))))))
	  (if (string= cc-list "") nil cc-list)))
      (current-buffer))))

(defun gnats::mail-other-window-using-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (gnats::vmish-mail-other-window 
   (format "mail regarding PR %s" gnats:::buffer-pr)
   nil nil (gnats::get-reply-subject) nil nil (current-buffer)))

;; This must be done in two toplevel forms because of a 19.19 byte-compiler
;; bug.
(defun gnats::generate-new-buffer-name (prefix)
  (let ((name prefix) (n 1))
    (while (get-buffer name)
      (setq name (format "%s<%d>" prefix n))
      (setq n (1+ n)))
    name))

(if (fboundp 'generate-new-buffer-name)
    (fset 'gnats::generate-new-buffer-name 'generate-new-buffer-name))

(defvar gnats::kept-mail-buffers nil
  "Sent mail buffers waiting to be killed.")

(defun gnats::vmish-rename-after-send ()
  (or (string-match "^sent " (buffer-name))
      (rename-buffer (gnats::generate-new-buffer-name
		      (format "sent %s" (buffer-name)))))

  ;; Mostly lifted from vm-reply.el 5.35
  (setq gnats::kept-mail-buffers
	(cons (current-buffer) gnats::kept-mail-buffers))
  (if (not (eq gnats:keep-sent-messages t))
      (let ((extras (nthcdr (or gnats:keep-sent-messages 0)
			    gnats::kept-mail-buffers)))
	(mapcar (function (lambda (b) (and (buffer-name b) (kill-buffer b))))
		extras)
	(and gnats::kept-mail-buffers extras
	     (setcdr (memq (car extras) gnats::kept-mail-buffers) nil)))))

(if gnats::emacs-19p
    (defun gnats::vmish-mail-bindings ())
  (defun gnats::vmish-mail-send ()
    (interactive)
    (gnats::vmish-rename-after-send)
    (mail-send))
  (defun gnats::vmish-mail-send-and-exit (arg)
    (interactive "P")
    (gnats::vmish-rename-after-send)
    (mail-send-and-exit arg))
  (defun gnats::vmish-mail-bindings ()
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key "\C-c\C-s" 'gnats::vmish-mail-send)
    (local-set-key "\C-c\C-c" 'gnats::vmish-mail-send-and-exit))
  (defun string-to-number (str) (string-to-int str)))

;; ignore 'free variable' warnings about buf.
(defsubst gnats::vmish-rename-mail-buffer (buf)
  (save-excursion
    (set-buffer buf)
    (setq buf (gnats::generate-new-buffer-name "*not mail*"))
    (rename-buffer buf)))

;; ignore 'free variable' warnings about buf.
(defsubst gnats::vmish-restore-mail-buffer (buf)
  (save-excursion
    (let ((mbuf (get-buffer "*mail*")))
      (cond (mbuf			;maybe left over from m-o-w failure
	     (set-buffer mbuf)
	     (set-buffer-modified-p nil)
	     (kill-buffer mbuf))))
    (cond (buf
	   (set-buffer buf)
	   (rename-buffer "*mail*")))))

(defun gnats::vmish-mail-other-window
  (&optional buffer-name noerase to subject in-reply-to cc replybuffer actions)
  (let ((buf (get-buffer "*mail*")))
    (if buf (gnats::vmish-rename-mail-buffer buf))
    (or buffer-name (setq buffer-name "GNATS mail"))
    (unwind-protect
	(prog1
	    (if gnats::emacs-19p
		(mail-other-window
		 noerase to subject in-reply-to cc replybuffer
		 (cons '(gnats::vmish-rename-after-send) actions))
	      (prog1
		  (mail-other-window noerase to subject in-reply-to
				     cc replybuffer)
		(gnats::vmish-mail-bindings)))
	  (rename-buffer (gnats::generate-new-buffer-name buffer-name)))
      (gnats::vmish-restore-mail-buffer buf))))

(defun gnats::vmish-mail
  (&optional buffer-name noerase to subject in-reply-to cc replybuffer actions)
  (let (buf (get-buffer "*mail*"))
    (if buf (gnats::vmish-rename-mail-buffer buf))
    (or buffer-name (setq buffer-name "GNATS mail"))
    (unwind-protect
	(prog1
	    (if gnats::emacs-19p
		(mail noerase to subject in-reply-to cc replybuffer
		      (cons '(gnats::vmish-rename-after-send) actions))
	      (prog1
		  (mail noerase to subject in-reply-to cc replybuffer)
		(gnats::vmish-mail-bindings)))
	  (rename-buffer (gnats::generate-new-buffer-name buffer-name)))
      (gnats::vmish-restore-mail-buffer buf))))

;;;; Send mail using mh-e.

(defun gnats::mail-other-window-using-mhe ()
  "Compose mail other window using mh-e.
While composing the message, use \\[mh-yank-cur-msg] to yank the
original message into it."
  (let ((subject (gnats::get-reply-subject)))
    (setq mh-show-buffer (current-buffer))
    (mh-find-path)
    (mh-send-other-window "" "" subject)
    (setq mh-sent-from-folder (current-buffer))
    (setq mh-sent-from-msg 1)))


(defun gnats::mail-reply-using-mhe (just-sender)
  "Compose reply mail using mh-e.
The command \\[mh-yank-cur-msg] yanks the original message into current buffer.
If optional argument JUST-SENDER is non-nil, send response only to
original submitter of problem report."
  ;; First of all, prepare mhe mail buffer.
  (let (from cc subject date to reply-to (buffer (current-buffer)) msg-id)
    (save-restriction
      (setq from     (mh-get-field "From:")
	    subject  (gnats::get-reply-subject)
	    reply-to (or (mh-get-field "Reply-To:") from)
	    to	     (or (mh-get-field "To:")
			 (mh-get-field "Apparently-To:")
			 "")
	    cc       (mh-get-field "Cc:")
	    date     (mh-get-field "Date:")
	    msg-id   (mh-get-field "Message-Id:")
	    )
      (setq mh-show-buffer buffer)
      (mh-find-path)
      (mh-send reply-to (or (and just-sender "")
			    (if (null cc) to
			      (concat to ", " cc)))
	       subject)
      (save-excursion
	(mh-insert-fields
	 "In-Reply-To:" (gnats::make-in-reply-to-field from date msg-id)))
      (setq mh-sent-from-folder buffer)
      (setq mh-sent-from-msg 1)
      )))


;;;;---------------------------------------------------------------------------
;;;; Functions to change specific fields
;;;;---------------------------------------------------------------------------

(defun gnats:state-change-from-to ()
  "Change the value of the `>State:' field and update the audit trail."
  (interactive)
  (gnats:change-field "State"))

(defun gnats:responsible-change-from-to ()
  "Change the value of the `>Responsible:' field and update the audit trail."
  (interactive)
  (gnats:change-field "Responsible"))

(defun gnats:category-change-from-to ()
  "Change the value of the `>Category:' field and the responsible party."
  (interactive)
  (gnats:change-field "Category"))

(defun gnats::update-audit-trail (field old new)
  (if (gnats::position-on-field "Audit-Trail")
      (let (start end)
	(gnats::forward-eofield)
	(setq start (point-marker))
	(if (eq old t) (setq old "????"))
	(if (string= field "Responsible")
	    (insert (format "\n\n%s-Changed-From-To: %s->%s" field
			    (gnats::nth-word old)
			    (gnats::nth-word new)))
	  (insert (format "\n\n%s-Changed-From-To: %s-%s" field
			  (gnats::nth-word old)
			  (gnats::nth-word new))))
	(insert (format "\n%s-Changed-By: %s" field (user-login-name)))
	(insert (format "\n%s-Changed-When: %s" field (current-time-string)))
	(insert (format "\n%s-Changed-Why:\n" field))
	(save-excursion
	  (gnats::before-keyword t)
	  (setq end (point-marker)))
	;; here we record the changes in a assoc list
	(setq gnats:::audit-trail (cons (list field 
					     (gnats::nth-word old) 
					     (gnats::nth-word new)
					     start end) 
				       gnats:::audit-trail)))
    (error "Field `>Audit-Trail:' missing.")))

(defun gnats::category-responsible (category)
  (let ((entry (assoc category gnats::categories)))
    (if entry
	(nth 2 entry)
      (signal 'gnats::no-such-category (list category)))))
	      
(defun gnats::update-responsible (ignore1 ignore2 new)
  "Modify the responsible field of the current PR to match the new category."
  (and (y-or-n-p "Update the >Responsible: field? ")
       (gnats:change-field "Responsible" (gnats::category-responsible new))))

;;;;---------------------------------------------------------------------------

(defsubst gnats::rw (buf retval)
  (or
   retval				; call-process is broken under 19.19.2
   (save-excursion (set-buffer buf) (buffer-size))))

(defun gnats::handle-results (pr exit-status)
  "Handle the results of running pr-edit or npr-edit, giving a signal
if needed."
  (cond
   ((looking-at "n?pr-edit: cannot create lock file")
    (signal 'gnats::cannot-lock nil))
   ((looking-at "n?pr-edit: lock file exists")
    (signal 'gnats::locked nil))
   ((or (looking-at "n?pr-edit: no such PR")
	(looking-at "n?pr-edit: couldn.t find PR.*"))
    (signal 'gnats::no-such-pr nil))
   ((looking-at "n?pr-edit: PR \\(.*\\) locked by \\(.*\\)")
    (let* ((msg (gnats::bm 2))
	   (pr-path
	    (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	   (pr-name (progn (if (string-match gnats:root pr-path)
			       (substring pr-path (1+ (match-end 0)))
			     pr-path)))
	   (buf (get-buffer pr-name))
	   win)
      (if buf
	  ;; If we're already editing the PR, just go to that
	  ;; buffer and be done with it.
	  (progn
	    (if (setq win (get-buffer-window buf))
		(select-window win)
	      (switch-to-buffer buf))
	    (message "Already editing PR %s." pr-name))
	;; kick it to the next level
	(signal 'gnats::locked-pr (list msg)))))
   ((looking-at "n?pr-edit: PR is not locked")
    (if (not gnats:::force) (signal 'gnats::pr-not-locked nil)
      t))
   ((looking-at "n?pr-edit: invalid fields")
    (signal 'gnats::invalid-fields nil))
   ((looking-at "n?pr-edit: cannot parse the date")
    (signal 'gnats::invalid-date nil))
   ((looking-at "n?pr-edit: lock file .* does not exist"))
   (t (signal 'gnats::error
	      (list (if (eq (point-min) (point-max))
			(format "unknown error (exit status %d)"
				exit-status)
		      (buffer-substring (point-min) (- (point-max) 1))))))))

(if gnats::emacs-19p
    (require 'env))
(defun gnats::start-background (pr proctype sentinel &optional outfile filep args)
  (let ((buf (get-buffer-create gnats::err-buffer))
	inbuf proc-name proc-action proc-send-buffer)
    (save-excursion
      (setq inbuf (current-buffer))
      (set-buffer buf)
      (erase-buffer)
      (make-variable-buffer-local 'gnats:::force)
      (setq gnats:::force nil)
      (cond ((eq proctype 'check)
	     (progn
	       (setq proc-name "check-pr"
		     proc-action "Checking"
		     proc-send-buffer t)
	       (setq args (append (list "--check") args))
	       (make-variable-buffer-local 'gnats:::pr-buffer)
	       (setq gnats:::pr-buffer inbuf)
	       (make-variable-buffer-local 'gnats:::do-file-pr)
	       (setq gnats:::do-file-pr filep)))
	    ((eq proctype 'file)
	     (setq proc-name "file-pr"
		   proc-action "Filing"
		   proc-send-buffer t))
	    ((eq proctype 'unlock)
	     (progn
	       (setq proc-name "unlock-pr"
		     proc-action "Unlocking")
	       (make-variable-buffer-local 'gnats:::current-pr)
	       (setq gnats:::current-pr pr)
	       (setq args (append (list "--unlock" pr) args))))
	    ((eq proctype 'unlock-force)
	     (progn
	       (setq proc-name "unlock-pr"
		     proc-action "Unlocking"
		     gnats:::force t)
	       (make-variable-buffer-local 'gnats:::current-pr)
	       (setq gnats:::current-pr pr)
	       (setq args (append (list "--unlock" pr) args))))
	    ((eq proctype 'edit)
	     (progn
	       (setq proc-name "edit-pr"
		     proc-action "Fetching")
	       (make-variable-buffer-local 'gnats:::current-pr)
	       (setq gnats:::current-pr pr)
	       (make-variable-buffer-local 'gnats:::newfile)
	       (setq gnats:::newfile outfile)))
	    (t
	     (error "Invalid PROCTYPE for background GNATS process.")))
      (let ((process-environment 
	     (if gnats::emacs-19p (copy-sequence process-environment)))
	    proc)
	(setq proc
	      (apply 'start-process
		     (concat " *" proc-name "-" (random t))
		     buf
		     (format (if gnats:network-server
				 "%s/gnats/npr-edit"
			       "%s/gnats/pr-edit")
			     gnats:libdir)
		     (if gnats:network-server (format  "--host=%s" gnats:network-server))
		     args
		     ))

	;; Only set up the sentinel if they want stuff done in the background.
	(if gnats:run-in-background
	    (progn
	      (set-process-sentinel proc sentinel)
	      (message "%s PR %s in background." proc-action pr))
	  (message "%s PR %s..." proc-action pr))
	(if proc-send-buffer
	    (progn
	      (set-buffer inbuf)
	      (goto-char (point-min))
	      (process-send-region proc (point-min) (point-max))
	      (if (and (/= (point-min) (point-max))
		       (/= (char-after (- (point-max) 1)) ?\n))
		  (process-send-string proc "\n"))
	      (process-send-eof proc)))
	;; if they don't want it in the background, just sit and twiddle...
	(if (not gnats:run-in-background)
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      (while (memq (process-status proc) '(run open))
		(accept-process-output proc))
	      (funcall sentinel proc nil)))))))

(defun gnats::handle-pr-edit (process event)
  (let ((buf (process-buffer process))
	result pr newfile nbuf)
    (if (null (buffer-name buf)) ;; deleted buffer
	(set-process-buffer process nil)
      (save-excursion
	(set-buffer buf)
	(setq pr gnats:::current-pr)
	(setq result (process-exit-status process))
	(and (/= 0 result)
	     (goto-char (point-min))
	     (gnats::handle-results gnats:::current-pr result))
	(setq nbuf (generate-new-buffer
		    (concat "*edit-pr " gnats:::current-pr "*")))
	(setq newfile gnats:::newfile)
	(set-buffer nbuf)
	(insert-file-contents newfile)
	(make-local-variable 'gnats:::backupname)
	(put 'gnats:::backupname 'permanent-local t)
	;; we do this in gnats:gnats-mode for non-network
	(if gnats:network-server (setq gnats:::backupname newfile))
	(set-buffer-modified-p nil)
	(setq buffer-undo-list nil) ;flush undo list
	(gnats:gnats-mode)
	(make-variable-buffer-local 'gnats:::current-pr)
	(setq gnats:::current-pr pr)
	(goto-char gnats:::start-of-PR-fields))
      (message "Fetching PR %s done." pr)
      (if gnats:run-in-background
	  (display-buffer nbuf 'not-this-window)
	(switch-to-buffer nbuf)))))

(defun gnats::pr-edit-background (pr outfile args)
  (gnats::start-background pr 'edit 'gnats::handle-pr-edit outfile nil args))

(defun gnats::handle-check-pr (process event)
  (let ((buf (process-buffer process))
	result pr)
    (if (null (buffer-name buf)) ;; deleted buffer
	(set-process-buffer process nil)
      (save-excursion
	(set-buffer buf)
	(setq result (process-exit-status process))
	(and (/= 0 result)
	     (goto-char (point-min))
	     (gnats::handle-results gnats:::current-pr result))
	(message "Checked PR %s." gnats:::current-pr)
	(if gnats:::do-file-pr
	    (progn
	      (set-buffer gnats:::pr-buffer)
	      (gnats::file-pr-background)))))))

(defun gnats::check-pr-background (&optional filep)
  (gnats::start-background gnats:::current-pr 'check
			  'gnats::handle-check-pr nil filep))

(defun gnats::finish-filing ()
  (let (responsible user resp-change state-change buf)
    (if gnats:network-server (setq gnats:::pr-locked nil))
    (setq buf (current-buffer))
    (set-buffer-modified-p nil)
    (setq responsible  (gnats::field-contents "Responsible")
	  user         (user-login-name)
	  resp-change (cdr (assoc "Responsible" gnats:::audit-trail))
	  state-change (cdr (assoc "State" gnats:::audit-trail)))
    (if (or state-change
	    resp-change
	    (not (equal user responsible)))
	(gnats::mail-PR-changed user responsible
			       resp-change state-change
			       (gnats::get-header "X-GNATS-Notify")))
    (gnats:unlock-buffer buf)))

(defun gnats::handle-file-pr (process event)
  (let ((buf (process-buffer process))
	result pr prbuf)
    (if (null (buffer-name buf)) ;; deleted buffer
	(set-process-buffer process nil)
      (save-excursion
	(set-buffer buf)
	(setq result (process-exit-status process))
	(and (/= 0 result)
	     (goto-char (point-min))
	     (gnats::handle-results gnats:::current-pr result))
	(message "Filed PR %s." gnats:::current-pr)
	(set-buffer gnats:::pr-buffer)
	(gnats::finish-filing)))))

(defun gnats::file-pr-background ()
  (gnats::start-background gnats:::current-pr 'file 'gnats::handle-file-pr))

(defun gnats::lock (pr &optional outfile)
  (let ((lockl (list "--lock"
		(format "%s@%s" (user-login-name) (system-name))
		"-p"
		(if (fboundp 'emacs-pid)
		    (concat "emacs pid " (int-to-string (emacs-pid)))
		  "emacs18")
		pr)))
    (if gnats:network-server
	(setq lockl (append lockl (list "-o" outfile "--get-lists"
					"--host" gnats:network-server))))
    (gnats::pr-edit-background pr outfile lockl)))

(fset 'unlock-pr 'gnats:unlock-pr)
(fset 'gnats-unlock 'gnats:unlock-pr)	;backward compatibility
(defun gnats::handle-unlock-pr (process event)
  (let ((buf (process-buffer process))
	result pr newfile nbuf)
    (if (null (buffer-name buf)) ;; deleted buffer
	(set-process-buffer process nil)
      (save-excursion
	(set-buffer buf)
	(setq pr gnats:::current-pr)
	(setq result (process-exit-status process))
	(and (/= 0 result)
	     (goto-char (point-min))
	     (gnats::handle-results gnats:::current-pr result))
	(message "PR %s unlocked." gnats:::current-pr)))))

(defun gnats:unlock-pr-force (pr)
  (gnats::start-background pr 'unlock-force 'gnats::handle-unlock-pr))

(defun gnats:unlock-pr (pr)
  (interactive "sPR number: ")
  (gnats::start-background pr 'unlock 'gnats::handle-unlock-pr))

(defsubst gnats::buffer-major-mode (buffer)
  (save-excursion (set-buffer buffer) major-mode))

(defun gnats::unlock-all-buffers ()
  (save-excursion
    (mapcar
     (function
      (lambda (buffer)
	(let ((gnats:run-in-background nil))
	  (if (and (eq (gnats::buffer-major-mode buffer) gnats::mode-name))
	      (progn (set-buffer buffer)
		     (gnats:unlock-buffer-force buffer))))))
     (buffer-list))))

(if gnats::emacs-19p
    ;; Emacs 19 has kill-buffer-hook, v18 doesn't.
    (defun gnats::kill-buffer-hook ()
      "Unlock a GNATS buffer that is being killed."
      (gnats:unlock-buffer nil))
  (defun gnats:kill-buffer (buf)
    "Safely kill a GNATS buffer."
    (interactive "bKill buffer: ")
    (if (equal buf (buffer-name))
	(gnats:unlock-buffer (get-buffer buf)))
    (kill-buffer buf)))

(defun gnats:unlock-buffer-force (&optional buf)
  "Force a buffer to be unlocked, even if it isn't."
  (interactive)
  (if (null buf)
      (setq buf (current-buffer))
    (set-buffer buf))
  (gnats:unlock-buffer buf t))

(defun gnats::delete-file (filename)
  (if (file-readable-p filename) (delete-file filename)))
  
(defun gnats:unlock-buffer (&optional buf force)
  "Safely take a GNATS buffer out of gnats-mode."
  (interactive)
  (save-excursion
    (if (null buf)
	(setq buf (current-buffer))
      (set-buffer buf))
    (cond ((or force
	       (not (buffer-modified-p buf))
	       (not gnats:::pr-locked)
	       (y-or-n-p "Buffer modified; still unlock? "))
	   (if gnats:::pr-locked
	       (gnats:unlock-pr-force gnats:::buffer-pr))
	   (if gnats:::pr-errors
	       (kill-buffer gnats:::pr-errors))
	   (if gnats:::backupname
	       (progn
		 (gnats::delete-file gnats:::backupname)
		 (if gnats:network-server
		     (progn
		       (gnats::delete-file (concat gnats:::backupname ".cat"))
		       (gnats::delete-file (concat gnats:::backupname ".res"))
		       (gnats::delete-file (concat gnats:::backupname ".sub"))))))
	   (save-excursion
	     (set-buffer buf)
	     (let ((pr gnats:::buffer-pr))
	       (kill-all-local-variables)
	       (text-mode)
	       (make-local-variable 'gnats:::buffer-pr)
	       (setq gnats:::buffer-pr pr)
	       (use-local-map (copy-keymap (current-local-map)))
	       (local-set-key
		"e" (function (lambda () (interactive)
				(gnats:edit-pr gnats:::buffer-pr))))
	       (set-visited-file-name nil)
	       (setq buffer-read-only t)
	       ;; When GNATS:KEEP-EDITED-BUFFERS is nil, we always put the
	       ;; most recent PR in the *edited-pr* buffer.
	       (or gnats:keep-edited-buffers
		   (let ((old-buf (get-buffer (concat "*edited-pr*"))))
		     (cond (old-buf
			    (set-buffer old-buf)
			    (set-buffer-modified-p nil)
			    (kill-buffer old-buf)))
		     (set-buffer buf)
		     (rename-buffer (concat "*edited-pr*"))))))
	   (and gnats:bury-edited-prs
		(if (get-buffer-window buf)
		    (let ((win (selected-window)))
		      (select-window (get-buffer-window buf))
		      (bury-buffer)
		      (select-window win))
		  (bury-buffer buf))))
	  (t (error "PR unlock aborted.")))))
	
(defun gnats::delete-backups (filename)
  (let ((l (file-name-all-completions
	    (concat (file-name-nondirectory filename) ".~")
	    (file-name-directory filename)))
	(dir (file-name-directory filename)))
    (while l
      (delete-file (concat dir (car l)))
      (setq l (cdr l)))))

(defun gnats::reset-variables ()
  (setq gnats::submitters nil
	gnats::responsibles nil
	gnats::categories nil))

(defun gnats::set-responsibles (&optional arg)
  (or (and (null arg) gnats::responsibles)
      (setq gnats::responsibles
	    (gnats::get-list-from-file
	     (if gnats:network-server
		 "res"
	       "responsible") "responsible")))
  'gnats::try-responsible-completion)

(defun gnats::try-responsible-completion (string predicate do-list)
  (let (entry)
    (and (not (assoc string gnats::responsibles))
	 (setq entry (gnats::real-pr-addr string))
	 (gnats::push entry gnats::responsibles)))
  (let* ((completion-ignore-case t))
    (if do-list
	(all-completions string gnats::responsibles predicate)
      (try-completion string gnats::responsibles predicate))))

(defun gnats::set-categories (&optional arg)
  (or (and (null arg) gnats::categories)
      (setq gnats::categories
	    (gnats::get-list-from-file
	     (if gnats:network-server
		 "cat"
	       "categories") "categories"))))

(defun gnats::set-submitters (&optional arg)
  (or (and (null arg) gnats::submitters)
      (setq gnats::submitters
	    (gnats::get-list-from-file
	     (if gnats:network-server
		 "sub"
	       "submitters") "submitters"))))

(defun gnats::get-list (buffer)
  (let (result)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^[^#:]+" nil t)
        (gnats::push (list (gnats::bm 0)) result)))
    (reverse result)))

(defun gnats::parse-line ()
  (let ((end (progn (end-of-line) (point)))
	(p (match-beginning 0))
	l)
    (goto-char p)
    (while (search-forward ":" end 'move)
      (gnats::push (buffer-substring p (match-beginning 0)) l)
      (skip-chars-forward " " end)
      (setq p (point)))
    (gnats::push (buffer-substring p end) l)
    (reverse l)))

(defun gnats::get-alist (buffer)
  (let (result)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^[^#]" nil t)
	(gnats::push (gnats::parse-line) result)))
    (reverse result)))

(defun gnats::get-list-from-file (filename desc)
  (let ((buf nil)
	(result nil))
    (message "Parsing the %s file..." desc)
    (save-excursion
      (let ((bn gnats:::backupname))
	(setq buf (get-buffer-create " *gnats-grok*"))
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-file-contents
	 (if gnats:network-server
	     (concat bn "." filename)
	   (format "%s/gnats-adm/%s" gnats:root filename)))
	(setq result (gnats::get-alist buf))
	(kill-buffer buf))
      (message "Parsing the %s file...done." desc)
      result)))

(defun gnats::get-pr-category (number)
  "Return the category for the problem report NUMBER."
  (let ((buf nil)
	(result nil))
    (save-excursion
      (setq buf (get-buffer-create " *gnats-index*"))
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file-contents (format "%s/gnats-adm/index" gnats:root))
      (goto-char (point-min))
      (setq result
	    (catch 'res
	      (while (search-forward (format "/%s:" number) nil t)
		(beginning-of-line)
		(if (looking-at (format "\\([^/]+\\)/%s:" number))
		    (throw 'res (gnats::bm 1))
		  (end-of-line)))
	      nil))
      (kill-buffer buf))
    (or result (signal 'gnats::no-such-pr (list number)))))

(defsubst gnats::has-slash (string)
  (memq t (mapcar (function (lambda (char) (= char ?/))) string)))

(or (boundp 'view-hook) (setq view-hook nil))

;;;###autoload
(fset 'view-pr 'gnats:view-pr)
;;;###autoload
(defun gnats:view-pr (&optional id)
  "Visit the problem report named by the string ID.  While viewing, press
'e' to edit the currently viewed PR."
  (interactive "sPR number: ")
  (let (pr category temp-name buffer)
    (if (string= id "")
	(message "view-pr: must specify a PR")
      (if (or (gnats::has-slash id) gnats:network-server)
	  (setq pr id)
	(and (setq category (gnats::get-pr-category id))
	     (setq pr (format "%s/%s" category id))))
      (let ((view-hook (default-value 'view-hook))
	    buf func)
	(if (and pr
		 (or gnats:network-server
		     (setq buf (get-buffer pr))
		     (file-exists-p (format "%s/%s" gnats:root pr))))
	    (if buf
		(save-excursion
		  (set-buffer buf)
		  (goto-char (point-min))
		  (view-buffer buf))
	      (setq func
		    (function
		     (lambda ()
		       (and gnats::emacs-19p (rename-buffer pr))
		       (setq mode-line-buffer-identification
			     (format "Viewing %s" pr))
		       (make-local-variable 'gnats:::buffer-pr)
		       (setq gnats:::buffer-pr pr)
		       (use-local-map (copy-keymap (current-local-map)))
		       (local-set-key
			"e"
			(function (lambda () (interactive)
				    (gnats:edit-pr gnats:::buffer-pr)))))))
	      (if (fboundp 'add-hook)
		  (add-hook 'view-hook func)
		(setq view-hook func))
	      (if gnats:network-server
		  (gnats:net-view-pr id buf)
		(view-file (format "%s/%s" gnats:root pr))))
	  (signal 'gnats::no-such-pr (list id)))))))

(defun gnats:net-view-pr (id buf)
  "Use the network query to view problem report ID."
  (require 'view)
  (let ((result nil)
	(curr (current-buffer)))
    (unwind-protect
	(if (not buf)
	    (progn
	      ;; XXX fix this to include the category
	      (setq buf (get-buffer-create (concat "*view-pr " id "*")))
	      (setq buffer-read-only nil)))
      (let ((command (append (list 'funcall)
			     (list ''call-process)
			       (list gnats:::nquery-pr nil buf nil)
			       (list id "--full" "--host"
				     gnats:network-server))))
	  (save-excursion
	    (set-buffer buf)
	    (erase-buffer))
	  (let ((default-directory (gnats::find-safe-default-directory)))
	    (setq result (gnats::rw buf (eval command))))
	  (save-excursion
	    (set-buffer buf)
	    (and (/= 0 result)
		 (goto-char (point-min))
		 (cond
		  ((or (looking-at (concat gnats:::query-regexp " no PRs matched"))
		       (looking-at (concat gnats:::query-regexp " couldn.t find PR.*")))
		   (signal 'gnats::no-such-pr nil))
		  (t (signal 'gnats::error
			     (list (buffer-substring (point-min)
						     (- (point-max) 1)))))
		  ))))
      (switch-to-buffer buf)
      (if (fboundp 'view-mode-enter)
	  (view-mode-enter curr 'kill-buffer)
	(view-mode curr 'kill-buffer))
      (set-buffer-modified-p nil)
      (make-local-variable 'gnats:::buffer-pr)
      (if (not (gnats::has-slash id)) (gnats::rename-buffer))
      (setq buffer-read-only t)
      (setq buffer-undo-list nil) ;flush undo list
      (goto-char (point-min)))
    (zerop result)))

(fset 'change-gnats 'gnats:change-type)
(fset 'gnats-change-type 'gnats:change-type)
(defun gnats:change-type (type)
  "Change the GNATS database type in use."
  (interactive
   (list
    (progn
      (if (not gnats:::types)
	  (error "Value of gnats:::types has to be non-nil."))
      (let* ((completion-ignore-case t))
	(completing-read "Use GNATS database type: " gnats:::types nil t)))))
  (let ((newlist (car (cdr (assoc type gnats:::types)))))
    (setq gnats:root (car newlist)
	  gnats:libdir (car (cdr newlist))
	  gnats:::query-pr (car (cdr (cdr newlist)))
	  gnats:::nquery-pr (car (cdr (cdr (cdr newlist))))
	  gnats:::query-regexp (car (cdr (cdr (cdr (cdr newlist)))))
	  )
    (gnats::reset-variables)))

(defun gnats::find-pr-buffer (pr)
"*Find the buffer currently editing PR, returning the buffer or nil."
  (if (gnats::has-slash pr)
      ;; return the buffer if it exists
      (get-buffer pr)
    (let (buflist buf
	  (name (concat "/" pr "$")))
      (setq buflist
	    (delq nil
		  (mapcar
		   (function (lambda (buf)
			       (if (string-match name (buffer-name buf))
				   buf)))
		   (buffer-list))))
      ;; If we found one---and only one---then sanity-check some things
      ;; about it before we try to use it.
      (if (eq (length buflist) 1)
	  (progn
	    (setq buf (car buflist))
	    (save-excursion
	      (set-buffer buf)
	      ;; We make sure that we have a value for the PR, it's in
	      ;; the right mode, and that the buffer's writable.  If so,
	      ;; we'll return the buffer, otherwise the result of the if
	      ;; gets kicked back up to return nil.
	      (if (and gnats:::buffer-pr
		       (eq major-mode 'gnats:gnats-mode)
		       (eq buffer-read-only nil))
		  buf)))))))

;;;###autoload
(fset 'edit-pr 'gnats:edit-pr)
;;;###autoload
(defun gnats:edit-pr (&optional id)
  "Edit the problem report named by the string ID."
  (interactive "sPR number: ")
  (if (string= id "")
      (message "edit-pr: must specify a PR to edit")
    (let (pr category newfile
	     (buf (gnats::find-pr-buffer id)))
      (if buf
	  (progn
	    (switch-to-buffer buf)
	    (message "Already editing PR %s." id))
	(progn
	  (if (or (gnats::has-slash id) gnats:network-server)
	      (setq pr id)
	    (and (setq category (gnats::get-pr-category id))
		 (setq pr (format "%s/%s" category id)))))
	(if (and pr (or gnats:network-server
			(file-exists-p (format "%s/%s" gnats:root pr))))
	    (progn
	      (setq newfile (if gnats:network-server
				(gnats::make-temp-name)
			      (format "%s/%s" gnats:root pr)))
	      (gnats::lock pr newfile))
	  (signal 'gnats::no-such-pr (list id)))))))

(defvar gnats:query-pr-default-options nil
  "*Default options to pass to query-pr.")
(defsubst gnats::query-pr-default-options ()
  (or gnats:query-pr-default-options
      (if (not gnats:network-server)
	  (concat " --directory=" gnats:root " --print-path ")
	"")))

;;;###autoload
(fset 'query-pr 'gnats:query-pr)
;;;###autoload
(defun gnats:query-pr (options)
  "Run query-pr, with user-specified args, and collect output in a buffer.
While query-pr runs asynchronously, you can use the \\[next-error] command
to find the text that the hits refer to."
  (interactive
   (list (apply
	  'read-from-minibuffer "Run query-pr (with args): "
	  (if gnats::emacs-19p
	      (list (cons (gnats::query-pr-default-options) 1)
		    nil nil 'gnats::query-pr-history)
	    (list (gnats::query-pr-default-options) nil nil)))))
  (require 'compile)
  (compile-internal (concat
		     (if gnats:network-server
			 (format (concat gnats:::nquery-pr " --host %s ")
				 gnats:network-server)
		       (concat gnats:::query-pr " "))
		     options)
		    "No more query-pr hits" (concat gnats:::query-pr " ")))

(defun gnats::tr (string from to)
  (let ((s (copy-sequence string))
	(len (length string)))
    (while (>= (setq len (1- len)) 0)
      (if (eq (aref s len) (string-to-char from))
	  (aset s len (string-to-char to))))
    s))

;; Redefine so that buffers with, say, g++/1234 embedded in them can be
;; autosaved.  This was mostly copied from the Emacs 19.19 version.
(defun gnats::make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
  ; Since the user may have his own make-auto-save-file-name, try not to
  ; use our custom one unless we have to.
  (if (or (eq major-mode gnats::mode-name)
 	  ; Heuristic for noticing a mail buffer based on a PR
 	  (string-match " PR .*/" (buffer-name)))
      (if buffer-file-name
	  (concat (file-name-directory buffer-file-name)
		  "#"
		  (file-name-nondirectory buffer-file-name)
		  "#")
	;; For non-file bfr, use bfr name and Emacs pid.
	(expand-file-name (format "#%s#%s#"
				  (gnats::tr (buffer-name) "/" ":")
				  (make-temp-name ""))))
    (gnats::real-make-auto-save-file-name)))

(if (not (fboundp 'gnats::real-make-auto-save-file-name))
    (progn (fset 'gnats::real-make-auto-save-file-name 
		 (symbol-function 'make-auto-save-file-name))
 	   (fset 'make-auto-save-file-name 'gnats::make-auto-save-file-name)))

(defun gnats::make-temp-name ()
  (make-temp-name
   (concat (expand-file-name (file-name-as-directory gnats::tmpdir)) "gnats")))
  
;; Below this is the GNATS summary mode I've written.  Not quite 100%
;; integrated yet.

;; Temporary variables which are made buffer-local, but which the byte
;; compiler complaints about if the defvars aren't here.
(defvar gnats:::PRs nil
  "List of problem reports to be summarized.  This variable is buffer local.")
(make-variable-buffer-local 'gnats:::PRs)
(defvar gnats::options nil
  "Options used for nquery-pr in the current GNATS summary buffer.
This variable is buffer local.")
(make-variable-buffer-local 'gnats::options)

;; Note: "release" stays out of this list.  The "release" field is
;; unrestricted; the customer could put any old junk in there, and
;; often does.
(defvar gnats:::limited-fields '(category confidential severity priority responsible state class customer-id)
  "PR fields for which the possible values are limited in range.")

(defvar gnats::summary-sort-function nil
  "Holds a function used to filter and sort PRs before displaying a report.
This filtering does not affect the stored PR information, so an invocation
of gnats:summary-redisplay after changing this variable will do the right thing.")

(defun gnats:::prompt-for-pr-number (default)
  (let ((val (read-input (if default
			     (format "PR number (default %d): " default)
			   "PR number: "))))
    (if (and default (string= val ""))
	default
      (setq val (string-to-number val))
      (if (and (integerp val)
	       (> val 0))
	  val
	(error "PR number must be a positive integer.")))))

(defun gnats:summary-edit (num)
  "Edit the PR referenced by the current text, or get a PR number from user.
If a numeric prefix is given, it is used as the PR number.
If a non-numeric prefix is given, or the text at (point) doesn't have the
gnats::pr-number property, the user is prompted for a PR number."
  (interactive (list
		(let ((x (get-text-property (point) 'gnats::pr-number)))
		  (cond ((numberp current-prefix-arg) current-prefix-arg)
			(current-prefix-arg (gnats:::prompt-for-pr-number x))
			(x x)
			(t (gnats:::prompt-for-pr-number nil))))))
  (message "Editing PR %d..." num)
  (gnats:edit-pr (number-to-string num)))

(defun gnats:summary-view (num)
  "View the PR referenced by the current text, or get a PR number from user.
If a numeric prefix is given, it is used as the PR number.
If a non-numeric prefix is given, or the text at (point) doesn't have the
gnats::pr-number property, the user is prompted for a PR number."
  (interactive (list
		(let ((x (get-text-property (point) 'gnats::pr-number)))
		  (cond ((numberp current-prefix-arg) current-prefix-arg)
			(current-prefix-arg (gnats:::prompt-for-pr-number x))
			(x x)
			(t (gnats:::prompt-for-pr-number nil))))))
  (message "Viewing PR %d..." num)
  (gnats:view-pr (number-to-string num)))

(defun gnats:summary-quit nil
  "Quit GNATS summary mode."
  (interactive)
  (kill-buffer nil))

(defun gnats:summary-revert nil
  "Fetch PR data from server and rebuild the summary."
  (interactive)
  (gnats:summ-pr gnats::options))

;; Fetch field value from a PR.
(defsubst gnats:::fieldval (pr field)
  (let ((x (assq field pr)))
    (if x (cdr x) nil)))

;; Taken from gnus-parse-simple-format in (ding)Gnus 0.88.
;; Extended to handle width-1 fields more efficiently.
;; Extended to permit "*" to flag truncation.
;; Modified to call kqpr* functions instead of gnus-*.
(defun gnats:::parse-summary-format (format spec-alist)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string. The list will consist of the symbol `format', a format
  ;; specification string, and a list of forms depending on the
  ;; SPEC-ALIST.
  (let ((max-width 0)
	spec flist fstring b newspec max-width elem beg trunc-noisy)
    (save-excursion
      (set-buffer (get-buffer-create " *qpr work*"))
      (erase-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "%-?[0-9]*\\([,*]-?[0-9]*\\)*\\(.\\)\\(.\\)?" nil t)
	(setq spec (string-to-char (buffer-substring (match-beginning 2)
						     (match-end 2))))
	;; First check if there are any specs that look anything like
	;; "%12,12A", ie. with a "max width specification". These have
	;; to be treated specially.
	(if (setq beg (match-beginning 1))
	    (setq max-width 
		  (string-to-int 
		   (buffer-substring (1+ (match-beginning 1)) (match-end 1)))
		  trunc-noisy (= ?* (char-after beg)))
	  (setq max-width 0)
	  (setq beg (match-beginning 2))
	  (setq trunc-noisy nil))
	;; Find the specification from `spec-alist'.
	(if (not (setq elem (cdr (assq spec spec-alist))))
	    (setq elem '("*" ?s)))
	;; Treat user defined format specifiers specially
	(and (eq (car elem) 'user-defined)
	     (setq elem
		   (list 
		    (list (intern (concat "gnats:user-format-function-"
					  (buffer-substring
					   (match-beginning 3)
					   (match-end 3))))
			  'pr)
		    ?s))
	     (delete-region (match-beginning 3) (match-end 3)))
	(if (not (zerop max-width))
	    (if (and (= max-width 1)
		     (memq (car (cdr elem)) '(?c ?s)))
		(let ((el (car elem)))
		  (cond ((= (car (cdr elem)) ?c)
			 (setq newspec ?c)
			 (setq flist (cons el flist)))
			((= (car (cdr elem)) ?s)
			 (setq newspec ?c)
			 (setq flist (cons (list 'string-to-char el) flist)))
			(t
			 (error "eep!"))))
	      (let ((el (car elem)))
		(cond ((= (car (cdr elem)) ?c) 
		       (setq el (list 'char-to-string el)))
		      ((= (car (cdr elem)) ?d)
		       (numberp el) (setq el (list 'int-to-string el))))
		(setq flist (cons (list 'gnats:::format-max-width
					el max-width trunc-noisy)
				  flist))
		(setq newspec ?s)))
	  (setq flist (cons (car elem) flist))
	  (setq newspec (car (cdr elem))))
	;; Remove the old specification (and possibly a ",12" string).
	(delete-region beg (match-end 2))
	;; Insert the new specification.
	(goto-char beg)
	(insert newspec))
      (setq fstring (buffer-substring 1 (point-max)))
      (kill-buffer nil))
    (cons 'format (cons fstring (nreverse flist)))))

;; Try to keep this list similar to the command-line options for nquery-pr,
;; just to avoid confusing people.  If there are differences, it won't break
;; anything.
(defvar gnats::summary-format-alist
  (list (list ?r '(symbol-name (gnats:::fieldval pr 'responsible)) ?s)
	(list ?c '(symbol-name (gnats:::fieldval pr 'category)) ?s)
	(list ?C '(symbol-name (gnats:::fieldval pr 'confidential)) ?s)
	(list ?e '(symbol-name (gnats:::fieldval pr 'severity)) ?s)
	(list ?O '(gnats:::fieldval pr 'originator) ?s)
	(list ?p '(symbol-name (gnats:::fieldval pr 'priority)) ?s)
	(list ?L '(symbol-name (gnats:::fieldval pr 'class)) ?s)
	(list ?S '(symbol-name (gnats:::fieldval pr 'customer-id)) ?s) ; == submitter
	(list ?s '(symbol-name (gnats:::fieldval pr 'state)) ?s)

	(list ?n '(gnats:::fieldval pr 'number) ?d)
	(list ?R '(gnats:::fieldval pr 'release) ?s)
	(list ?j '(gnats:::fieldval pr 'synopsis) ?s)
	(list ?y '(gnats:::fieldval pr 'synopsis) ?s)
	(list ?u 'user-defined ?s)
	))

(defun gnats:::format-max-width (str len noisy)
  (if (> (length str) (if gnats::emacs-19p (abs len) len))
      (if noisy
	  (if (< len 0)
	      (concat "*" (substring str (1+ len)))
	    (concat (substring str 0 (- len 1)) "*"))
	(if (< len 0)
	    (substring str len)
	  (substring str 0 len)))
    str))

;; Redisplay the summary in the current buffer.
(defvar gnats::format-string
  "%5n %-4,4c %,1e%,1p %-8,8r %,2s %-10*10S %-10*-10R %j\n"
  "Format string for PR summary text.

If you've used format strings in (ding)Gnus, this will be familiar.

Most text is copied straight through verbatim.  Use \"%\" to indicate a
non-fixed field.

It can be followed by a number, indicating minimum width, a separator character
(\",\" or \"*\"), and another number, indicating maximum width.  These fields
are optional, except that the separator must be present if the maximum width is
specified.  Whitespace padding will be on the left unless the first number is
negative.  Truncation of the field will be done on the right, unless the second
number is negative.  If the separator character is \"*\", a \"*\" will be used
to indicate that truncation has been done; otherwise, it will be done silently.

After the \"%\" and optional width parameters, a letter is expected.  Most of
the letters are chosen to match the command-line options of `nquery-pr'.

%r	\"Responsible\" field.
%c	\"Category\" field.
%C	\"Confidential\" field.
%e	\"Severity\" field.
%O	\"Originator\" field.
%p	\"Priority\" field.
%L	\"Class\" field.
%S	\"Customer-id\" (\"submitter\") field.
%s	\"State\" field.
%n	\"Number\" field.
%R	\"Release\" field.
%j, %y	\"Synopsis\" field.  (\"j\" as in \"subJect\")
%u	Special: The next character is examined, and the function
	gnats:user-format-function-<char> is invoked.  One argument, the list
	of (FIELD . VALUE) pairs, is passed.

Any newlines you wish to have used must be included in this string; no
additional ones will be provided.

If the value is not a string, it is assumed to be a function which can
be funcalled to return a format string, to be interpreted as above.")

(defun gnats:summary-redisplay nil
  "Redisplay summary of stored GNATS data.
This is useful if you change your filtering criteria or format string but
do not wish to update the GNATS data by contacting the server."
  (interactive)
  (let (prs
	(buffer-read-only nil)
	format-form fmt)
    ;; Do this early, so if we're in the wrong buffer we blow up without
    ;; trashing the user's data.
    (setq prs (if gnats::summary-sort-function
		  (funcall gnats::summary-sort-function
			   (apply 'list gnats:::PRs))
		gnats:::PRs))
    ;; No wrapping -- ick!
    (if gnats::emacs-19p
	(buffer-disable-undo)
      (buffer-flush-undo (current-buffer)))
    (erase-buffer)
    (setq fmt (if (stringp gnats::format-string)
		  gnats::format-string
		(funcall gnats::format-string)))
    (setq format-form (gnats:::parse-summary-format fmt
						   gnats::summary-format-alist))
    (mapcar (function
	     (lambda (pr)
	       (let ((start (point)))
		 (insert (eval format-form))
		 ;; Magic.
		 (put-text-property start (point) 'gnats::pr-number
				    (gnats:::fieldval pr 'number))
		 )))
	    prs)
    (goto-char (point-min))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    ))

(defvar gnats-summary-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (if gnats::emacs-19p (suppress-keymap map))
    ;; basic mode stuff
    (define-key map "g" 'gnats:summary-revert)
    (define-key map "q" 'gnats:summary-quit)
    (define-key map "r" 'gnats:summary-redisplay)
    ;; do stuff to PRs
    (define-key map "e" 'gnats:summary-edit)
    (define-key map "v" 'gnats:summary-view)
    ;; navigation
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for GNATS summary mode.")

(defun gnats-summary-mode nil
  "Major mode for problem report summary.

You can use \\[gnats:summary-view] to view the PR specified by the
current line, or \\[gnats:summary-edit] to edit it.  Typing
\\[gnats:summary-revert] will update the PR list.

Special commands:
\\{gnats-summary-mode-map}

Entering GNATS summary mode will invoke any hooks listed in the variable
gnats-summary-mode-hook.  It will also use text-mode-hook, since the summary
mode is built on top of text mode."
  (interactive)
  (text-mode)
;  (make-local-variable 'gnats:::PRs)
;  (make-local-variable 'gnats::options)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq major-mode 'gnats-summary-mode)
  (setq mode-name "GNATS Summary")
  (use-local-map gnats-summary-mode-map)
  (run-hooks 'gnats-summary-mode-hook)
  )

;;;###autoload
(fset 'summ-pr 'gnats:summ-pr)
;;;###autoload
(defun gnats:summ-pr (options)
  "Run query-pr, with user-specified args, and display a pretty summary.
Well, display a summary, at least."
  (interactive
   (list
    (if (not gnats::emacs-19p)
	(error "GNATS summary mode will only work with emacs 19.")
      (apply
       'read-from-minibuffer "Run query-pr (with args): "
       (if gnats::emacs-19p
	   (list (cons (gnats::query-pr-default-options) 1)
		 nil nil 'gnats::query-pr-history)
	 (list (gnats::query-pr-default-options) nil nil))))))
  (let ((buf (get-buffer-create "*gnats-summ-pr-temp*"))
	(prs nil)
	pr fieldname value p)
;    (save-excursion
      (set-buffer buf)
      (if gnats::emacs-19p
	  (buffer-disable-undo)
	(buffer-flush-undo buf))
      (erase-buffer)
      ;; calling nquery-pr directly would be better, but I'd need a "split"
      ;; function of some sort to break apart the options string.
      (message "Fetching GNATS data...")
      (call-process "sh" nil buf nil "-c"
		    (concat
		     (if gnats:network-server
			 (format (concat gnats:::nquery-pr " --host %s ")
					 gnats:network-server)
		       (concat gnats:::query-pr " "))
		     options))
      ;; um, okay, how to i check for errors?
      (goto-char (point-min))
      (setq pr nil)
      (while (looking-at "ld.so: warning:")
	(forward-line 1))
      (while (not (eobp))
	(while (looking-at ">\\([a-zA-Z-]+\\):")
	  (setq fieldname (intern
			   (downcase
			    (buffer-substring (match-beginning 1)
					      (match-end 1)))))
	  (goto-char (match-end 0))
	  (while (looking-at "[ \t]")
	    (forward-char 1))
	  (setq p (point))
	  (setq value (buffer-substring p (progn (end-of-line) (point))))
	  (cond ((eq fieldname 'number)
		 (setq value (string-to-number value)))
		((memq fieldname gnats:::limited-fields)
		 (setq value (intern value))))
	  (setq pr (cons (cons fieldname value) pr))
	  (forward-char 1))
	(if (looking-at "\n")
	    (progn
	      (setq prs (cons (nreverse pr) prs)
		    pr nil)
	      (forward-char 1)))
	;; could be the result of --print-path
	(if (looking-at "/.*:0:$")
	    (next-line 1))
	(if (looking-at gnats:::query-regexp)
	    ;; error message
	    (progn
	      (goto-char (match-end 0))
	      (while (looking-at "[ \t]")
		(forward-char 1))
	      (setq p (point))
	      (end-of-line)
	      (setq value (buffer-substring p (point)))
	      (error "Database query failed: %s" value)))
	)
      (if pr
	  (setq prs (cons (nreverse pr) prs)))
      (setq prs (nreverse prs))

      ;; okay, now display it
      (pop-to-buffer (get-buffer-create "*gnats:summ-pr*"))
      (gnats-summary-mode)
      (setq gnats:::PRs prs)
      (setq gnats::options options)
      (gnats:summary-redisplay)
      (message "Fetching GNATS data...done.")
;      )
    (kill-buffer buf)
    ))

;;;; end of gnats.el
