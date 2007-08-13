;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-netrc.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Parses ~/.netrc file, and does completion in /.
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Fri Jan 28 19:32:47 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 18:38:50 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;;; ------------------------------------------------------------
;;;; Provisions and requirements.
;;;; ------------------------------------------------------------

(provide 'efs-netrc)
(require 'efs-cu)
(require 'efs-ovwrt)
(require 'passwd)
(require 'efs-fnh)

;;;; ------------------------------------------------------------
;;;; Internal Variables
;;;; ------------------------------------------------------------

(defconst efs-netrc-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;; Make the byte compiler happy.
(defvar dired-directory)

;;;; ------------------------------------------------------------
;;;; Use configuration variables.
;;;; ------------------------------------------------------------

(defvar efs-netrc-filename "~/.netrc"
  "*File in .netrc format to search for passwords.
If you encrypt this file, name it something other than ~/.netrc. Otherwise,
ordinary FTP will bomb.

If you have any cryption package running off of find-file-hooks
(such as crypt.el or crypt++.el), efs will use it to decrypt this file.
Encrypting this file is a good idea!")

(defvar efs-disable-netrc-security-check nil
  "*If non-nil avoid checking permissions for `efs-netrc-filename'.")

;;;; ------------------------------------------------------------
;;;; Host / User / Account mapping support.
;;;; ------------------------------------------------------------

(defun efs-set-passwd (host user passwd)
  "For a given HOST and USER, set or change the associated PASSWORD."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Password: ")))
  (efs-set-host-user-property host user 'passwd
			      (and passwd (efs-code-string passwd))))

(defun efs-set-account (host user minidisk account)
  "Given HOST, USER, and MINIDISK, set or change the ACCOUNT password.
The minidisk is only relevant for CMS. If minidisk is irrelevant,
give the null string for it. In lisp programs, give the minidisk as nil."
  (interactive (efs-save-match-data
		 (let* ((path (or buffer-file-name
				  (and (eq major-mode 'dired-mode)
				       dired-directory)))
			(parsed (and path (efs-ftp-path path)))
			(default-host (car parsed))
			(default-user (nth 1 parsed))
			(default-minidisk
			  (and parsed
			       (eq (efs-host-type default-host) 'cms)
			       (string-match "^/[^/]+/" (nth 2 parsed))
			       (substring (nth 2 parsed) 1
					  (1- (match-end 0)))))
			(host (read-string "Host: " default-host))
			(user (read-string "User: " default-user))
			(minidisk
			 (read-string
			  "Minidisk (enter null string if inapplicable): "
			  default-minidisk))
			(account (read-passwd "Account password: ")))
		   (if (string-match "^ *$" minidisk)
		       (setq minidisk nil))
		   (list host user minidisk account))))
  (and account (setq account (efs-code-string account)))
  (if minidisk
      (efs-put-hash-entry (concat (downcase host) "/" user "/" minidisk)
			  account efs-minidisk-hashtable)
    (efs-set-host-user-property host user 'account account)))

;;;; ------------------------------------------------------------
;;;; Parsing the ~/.netrc.
;;;; ------------------------------------------------------------

(defconst efs-netrc-modtime nil)
;; Last modified time of the netrc file from file-attributes.

(defun efs-netrc-next-token ()
  ;; Gets the next token plus it's value.
  ;; Returns \(token value-1 value-2 ...\)
  (skip-chars-forward " \t\n")
  (while (char-equal  (following-char) ?#)
    (forward-line 1)
    (skip-chars-forward " \t\n"))
  (let ((tok (and (not (eobp))
		  (downcase (buffer-substring
			     (point)
			     (progn
			       (skip-chars-forward "^ \n\t")
			       (point)))))))
    (cond
     ((null tok) nil)
     ((string-equal tok "default")
      (list tok))
     ((member tok (list "machine" "login" "password" "account"))
      (list tok (efs-netrc-read-token-value)))
     ((string-equal tok "minidisk")
      (list tok (efs-netrc-read-token-value)
	    (efs-netrc-read-token-value)))
     ((string-equal tok "include")
      (let ((start (- (point) 7))
	    (path (expand-file-name (efs-netrc-read-token-value))))
	(delete-region start (point))
	(save-excursion (insert (efs-netrc-get-include path))))
      (efs-netrc-next-token))
     ;; Deal with tokens that we skip
     ((string-equal tok "macdef")
      (efs-save-match-data
	(search-forward "\n\n" nil 'move))
      (if (eobp)
	  nil
	(efs-netrc-next-token)))
     (t (error "efs netrc file error: Invalid token %s." tok)))))

(defun efs-netrc-read-token-value ()
  ;; Read the following word as a token value.
  (skip-chars-forward " \t\n")
  (while (char-equal (following-char) ?#)
    (forward-line 1)
    (skip-chars-forward " \t\n"))
  (if (eq (following-char) ?\")	;quoted token value
      (prog2
       (forward-char 1)
       (buffer-substring (point)
			 (progn (skip-chars-forward "^\"") (point)))
       (forward-char 1))
    (buffer-substring (point)
		      (progn (skip-chars-forward "^ \n\t") (point)))))

(defun efs-netrc-get-include (path)
  ;; Returns the text of an include file.
  (let ((buff (create-file-buffer path)))
    (unwind-protect
	(save-excursion
	  (set-buffer buff)
	  (setq buffer-file-name path
		default-directory (file-name-directory path))
	  (insert-file-contents path)
	  (normal-mode t)
	  (mapcar 'funcall find-file-hooks)
	  (setq buffer-file-name nil)
	  (buffer-string))
      (condition-case nil
	  ;; go through this rigamoroll, because who knows
	  ;; where an interrupt in find-file-hooks leaves us.
	  (save-excursion
	    (set-buffer buff)
	    (set-buffer-modified-p nil)
	    (passwd-kill-buffer buff))
	(error nil)))))

(defun efs-parse-netrc-group (&optional machine)
  ;; Extract the values for the tokens  "machine", "login", "password",
  ;; "account" and "minidisk"  in the current buffer.  If successful, 
  ;; record the information found.
  (let (data login)
    ;; Get a machine token.
    (if (or machine (setq data (efs-netrc-next-token)))
	(progn
	  (cond
	   (machine) ; noop
	   ((string-equal (car data) "machine")
	    (setq machine (nth 1 data)))
	   ((string-equal (car data) "default")
	    (setq machine 'default))
	   (error
	    "efs netrc file error: %s"
	    "Token group must start with machine or default."))
	  ;; Next look for a login token.
	  (setq data (efs-netrc-next-token))
	  (cond
	   ((null data)
	    ;; This just interns in the hashtable for completion to
	    ;; work.  The username gets set later by efs-get-user.
	    (if (stringp machine) (efs-set-user machine nil))
	    nil)
	   ((string-equal (car data) "machine")
	    (if (stringp machine) (efs-set-user machine nil))
	    (nth 1 data))
	   ((string-equal (car data) "default")
	    'default)
	   ((not (string-equal (car data) "login"))
	    (error "efs netrc file error: Expected login token for %s."
		   (if (eq machine 'default)
		       "default"
		     (format "machine %s" machine))))
	   (t
	    (setq login (nth 1 data))
	    (if (eq machine 'default)
		(setq efs-default-user login)
	      (efs-set-user machine login)
	      ;; Since an explicit login entry is given, intern an entry
	      ;; in the efs-host-user-hashtable for completion purposes.
	      (efs-set-host-user-property machine login nil nil))
	    (while (and (setq data (efs-netrc-next-token))
			(not (or (string-equal (car data) "machine")
				 (string-equal (car data) "default"))))
	      (cond
	       ((string-equal (car data) "password")
		(if (eq machine 'default)
		    (setq efs-default-password (nth 1 data))
		  (efs-set-passwd machine login (nth 1 data))))
	       ((string-equal (car data) "account")
		(if (eq machine 'default)
		    (setq efs-default-account (nth 1 data))
		  (efs-set-account machine login nil (nth 1 data))))
	       ((string-equal (car data) "minidisk")
		(if (eq machine 'default)
		    (error "efs netrc file error: %s."
			   "Minidisk token is not allowed for default entry.")
		  (apply 'efs-set-account machine login (cdr data))))
	       ((string-equal (car data) "login")
		(error "efs netrc file error: Second login token for %s."
		       (if (eq machine 'default)
			   "default"
			 (format "machine %s" machine))))))
	    (and data (if (string-equal (car data) "machine")
			  (nth 1 data)
			'default))))))))

(defun efs-parse-netrc ()
  "Parse the users ~/.netrc file, or file specified `by efs-netrc-filename'.
If the file exists and has the correct permissions then extract the
\`machine\', \`login\', \`password\', \`account\', and \`minidisk\'
information from within."
  (interactive)
  (and efs-netrc-filename
       (let* ((file (expand-file-name efs-netrc-filename))
	      ;; Set to nil to avoid an infinite recursion if the
	      ;; .netrc file is remote.
	      (efs-netrc-filename nil)
	      (file (efs-chase-symlinks file))
	      (attr (file-attributes file))
	      netrc-buffer next)
	 (if (or (interactive-p) ; If interactive, really do something.
		 (and attr	 ; file exists.
		      ;; file changed
		      (not (equal (nth 5 attr) efs-netrc-modtime))))
	     (efs-save-match-data
	       (or efs-disable-netrc-security-check
		   (and (eq (nth 2 attr) (user-uid)) ; Same uids.
			(string-match ".r..------" (nth 8 attr)))
		   (efs-netrc-scream-and-yell file attr))
	       (unwind-protect
		   (save-excursion
		     ;; we are cheating a bit here.  I'm trying to do the
		     ;; equivalent of find-file on the .netrc file, but
		     ;; then nuke it afterwards.
		     ;; with the bit of logic below we should be able to have
		     ;; encrypted .netrc files.
		     (set-buffer (setq netrc-buffer
				       (generate-new-buffer "*ftp-.netrc*")))
		     (insert-file-contents file)
		     (setq buffer-file-name file)
		     (setq default-directory (file-name-directory file))
		     (normal-mode t)
		     (mapcar 'funcall find-file-hooks)
		     (setq buffer-file-name nil)
		     (goto-char (point-min))
		     (while (and (not (eobp))
				 (setq next (efs-parse-netrc-group next)))))
		 (condition-case nil
		     ;; go through this rigamoroll, because we knows
		     ;; where an interrupt in find-file-hooks leaves us.
		     (save-excursion
		       (set-buffer netrc-buffer)
		       (set-buffer-modified-p nil)
		       (passwd-kill-buffer netrc-buffer))
		   (error nil)))
	       (setq efs-netrc-modtime (nth 5 attr)))))))

(defun efs-netrc-scream-and-yell (file attr)
  ;; Complain about badly protected netrc files.
  (let* ((bad-own (/= (nth 2 attr) (user-uid)))
	 (modes (nth 8 attr))
	 (bad-protect (not (string-match ".r..------" modes))))
    (if (or bad-own bad-protect)
	(save-window-excursion
	  (with-output-to-temp-buffer "*Help*"
	    (if bad-own
		(princ
		 (format
		  "Beware that your .netrc file %s is not owned by you.\n"
		  file)))
	    (if bad-protect
		(progn
		  (if bad-own
		      (princ "\nAlso,")
		    (princ "Beware that"))
		  (princ
		   " your .netrc file ")
		  (or bad-own (princ (concat file " ")))
		  (princ
		   (format
		    "has permissions\n %s.\n" modes))))
	    (princ
	     "\nIf this is intentional, then setting \
efs-disable-netrc-security-check
to t will inhibit this warning in the future.\n"))
	  (select-window (get-buffer-window "*Help*"))
	  (enlarge-window (- (count-lines (point-min) (point-max))
			     (window-height) -1))
	  (if (and bad-protect
		   (y-or-n-p (format "Set permissions on %s to 600? " file)))
	      (set-file-modes file 384))))))

;;;; ----------------------------------------------------------------
;;;; Completion in the root directory.
;;;; ----------------------------------------------------------------

(defun efs-generate-root-prefixes ()
  "Return a list of prefixes of the form \"user@host:\".
Used when completion is done in the root directory."
  (efs-parse-netrc)
  (efs-save-match-data
    (let (res)
      (efs-map-hashtable
       (function
	(lambda (key value)
	  (if (string-match "^[^/]+\\(/\\).+$" key)
	      ;; efs-passwd-hashtable may have entries of the type
	      ;; "machine/" to indicate a password assigned to the default
	      ;; user for "machine". Don't use these entries for completion.
	      (let ((host (substring key 0 (match-beginning 1)))
		    (user (substring key (match-end 1))))
		(setq res (cons (list (format
				       efs-path-user-at-host-format
				       user host))
				res))))))
       efs-host-user-hashtable)
      (efs-map-hashtable
       (function (lambda (host user)
		   (setq res (cons (list (format efs-path-host-format
						 host))
				   res))))
       efs-host-hashtable)
      (if (and (null res)
	       (string-match "^1[0-8]\\.\\|^[0-9]\\." emacs-version))
	  (list nil)
	res))))

(defun efs-root-file-name-all-completions (file dir)
  ;; Generates all completions in the root directory.
  (let ((file-name-handler-alist (efs-file-name-handler-alist-sans-fn
				  'efs-root-handler-function)))
    (nconc (all-completions file (efs-generate-root-prefixes))
	   (file-name-all-completions file dir))))


(defun efs-root-file-name-completion (file dir)
  ;; Calculates completions in the root directory to include remote hosts.
  (let ((file-name-handler-alist (efs-file-name-handler-alist-sans-fn
				  'efs-root-handler-function)))
    (try-completion
     file
     (nconc (efs-generate-root-prefixes)
	    (mapcar 'list (file-name-all-completions file "/"))))))


;;; end of efs-netrc.el
