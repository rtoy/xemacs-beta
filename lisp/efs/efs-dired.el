;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dired.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  Extends much of Dired to work under efs.
;; Authors:      Sebastian Kremer <sk@thp.uni-koeln.de>, 
;;               Andy Norman <ange@hplb.hpl.hp.com>,
;;               Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Throughout the ages.
;; Modified:     Sun Nov 27 12:19:46 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Provisions and requirements

(provide 'efs-dired)
(require 'efs)
(require 'dired)
(autoload 'dired-shell-call-process "dired-shell")

(defconst efs-dired-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;;; ----------------------------------------------------------------
;;;; User Configuration Variables
;;;; ----------------------------------------------------------------

(defvar efs-dired-verify-modtime-host-regexp nil
  "Regular expression determining on which hosts dired modtimes are checked.")

(defvar efs-dired-verify-anonymous-modtime nil
  "If non-nil, dired modtimes are checked for anonymous logins.")

(defvar efs-remote-shell-file-name
  (if (memq system-type '(hpux usg-unix-v)) ; hope that's right
      "remsh"
    "rsh")
  "Remote shell used by efs.")

(defvar efs-remote-shell-takes-user
  (null (null (memq system-type '(aix-v3 hpux silicon-graphics-unix
					 berkeley-unix))))
  ;; Complete? Doubt it.
  "Set to non-nil if your remote shell command takes \"-l USER\".")

;;; Internal Variables

(make-variable-buffer-local 'dired-ls-F-marks-symlinks)

;;;; -----------------------------------------------------------
;;;; Inserting Directories into Buffers
;;;; -----------------------------------------------------------

;; The main command for inserting a directory listing in a buffer.
;; In Emacs 19 this is in files.el, and not specifically connected to
;; dired. Since our version of it uses some dired functions, it is
;; included here, but there is an autoload for it in efs.el.

(defun efs-insert-directory (file switches &optional wildcard full-directory-p
				  nowait marker-char)
  ;; Inserts a remote directory. Can do this asynch.
  (let* ((parsed (efs-ftp-path file))
	 (mk (point-marker))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (path (nth 2 parsed))
	 (host-type (efs-host-type host))
	 (dumb (memq host-type efs-dumb-host-types))
	 (subdir (and (null (or full-directory-p wildcard))
		      (condition-case nil
			  (dired-current-directory)
			(error nil))))
	 (case-fold-search nil) ; for testing switches
	 (parse (and full-directory-p (not wildcard)
		     (or dumb (efs-parsable-switches-p switches))))
	 ;; In case dired-omit-silent isn't defined.
	 (dired-omit-silent (and (boundp 'dired-omit-silent)
				 dired-omit-silent)))
    
    ;; Insert the listing. If it's not a wild-card, and not a full-dir,
    ;; then we are updating a dired-line. Do this asynch.
    ;; This way of doing the listing makes sure that the dired
    ;; buffer is still around after the listing is obtained.
    
    (efs-ls
     file switches t (if parse 'parse t) nil
     ;; asynch, if we're inserting in a subdir. Do it nowait = 0, so
     ;; updating the file line gets a high priority??
     ;; Insert subdir listings NOWAIT = 0 also so 1-line
     ;; updates don't toggle the mode line.
     (if (and subdir nowait) 0 nowait)
     (efs-cont (listing) (host user file path wildcard
			       nowait marker-char
			       mk subdir parse switches dired-omit-silent)
       ;; We pass the value of dired-omit-silent from the caller to the cont.
       (let ((host-type (efs-host-type host))
	     (listing-type (efs-listing-type host user)))
	 (if (marker-buffer mk)
	     (efs-save-buffer-excursion
	       (set-buffer (marker-buffer mk))
	       ;; parsing a listing, sometimes updates info
	       (if (and parse (eq major-mode 'dired-mode))
		   (progn
		     (setq efs-dired-host-type host-type
			   efs-dired-listing-type listing-type
			   efs-dired-listing-type-string
			   (and efs-show-host-type-in-dired
				(concat " "
					(symbol-name
					 efs-dired-listing-type))))
		     (if (memq host-type '(bsd-unix next-unix))
			 (setq dired-ls-F-marks-symlinks nil)
		       (if (memq host-type '(sysV-unix apollo-unix))
			   (setq dired-ls-F-marks-symlinks t)))))
	       (if subdir
		   ;; a 1-line re-list
		   (save-excursion
		     (efs-update-file-info
		      host-type file efs-data-buffer-name)
		     (goto-char mk)
		     (let ((new-subdir (condition-case nil
					   (dired-current-directory)
					 (error nil)))
			   buffer-read-only)
		       (if (and new-subdir
				(string-equal subdir new-subdir))
			   (progn
			     ;; Is there an existing entry?
			     (if (dired-goto-file file)
				 (progn
				   (delete-region
				    (save-excursion
				      (skip-chars-backward "^\n\r")
				      (1- (point)))
				    (progn
				      (skip-chars-forward "^\n\r")
				      (point)))
				   (goto-char mk)))
			     (insert listing)
			     (save-restriction
			       (narrow-to-region mk (point))
			       (efs-dired-fixup-listing
				listing-type file path switches wildcard)
			       (efs-dired-ls-trim
				listing-type)
			       ;; save-excursion loses if fixup had to
			       ;; remove and re-add the region. Say for
			       ;; sorting.
			       (goto-char (point-max)))
			     (if (and nowait (eq major-mode 'dired-mode))
				 (dired-after-add-entry
				  (marker-position mk)
				  marker-char))))))
		 (goto-char mk)
		 (let (buffer-read-only)
		   (insert listing)
		   (save-restriction
		     (narrow-to-region mk (point))
		     (efs-dired-fixup-listing
		      listing-type file path switches wildcard)
		     (goto-char (point-max))))))))))
     ;; Return 0 if synch, nil if asynch
    (if nowait nil 0)))

;;; Functions for cleaning listings.

(efs-defun efs-dired-ls-trim nil ()
  ;; Trims dir listings, so that the listing of a single file is one line.
  nil)

(efs-defun efs-dired-fixup-listing nil (file path &optional switches wildcard)
  ;; FILE is in efs syntax.
  ;; PATH is just the remote path.
  ;; Some ftpd's put the whole directory name in front of each filename.
  ;; Seems to depend in a strange way on server-client interaction.
  ;; Walk down the listing generated and remove this stuff.
  ;; SWITCHES is a string.
  (if (memq efs-key efs-unix-host-types)
      (let ((continue t)
	    spot bol)
	(goto-char (point-min))
	(while (and (not (eobp)) continue)
	  (and (setq bol (point)
		     spot (dired-manual-move-to-filename nil bol))
	       (setq continue (= (following-char) ?/))
	       (dired-manual-move-to-end-of-filename t bol)
	       (progn
		 (skip-chars-backward "^/")
		 (delete-region spot (point))))
	  (forward-line 1))
	(efs-save-match-data
	  (if (and switches (string-match "R" switches)
		   (not (string-match "d" switches)))
	      (let ((subdir-regexp "^\\(/[^ \n\r]+\\):[\n\r]")
		    name)
		(goto-char (point-min))
		(while (re-search-forward subdir-regexp nil t)
		  (goto-char (match-beginning 0))
		  ;; There may be /./ type nonsense.
		  ;; expand-file-name will handle it.
		  (setq name (expand-file-name
			      (buffer-substring (point) (match-end 0))))
		  (delete-region (point) (match-end 0))
		  (insert (efs-replace-path-component file name)))))))))


;;;; ------------------------------------------------------------
;;;; Tree Dired support
;;;; ------------------------------------------------------------

;;; efs-dired keymap

(defvar efs-dired-map nil
  "Keymap for efs commands in dired buffers.")

(if efs-dired-map
    ()
  (setq efs-dired-map (make-sparse-keymap))
  (define-key efs-dired-map "c" 'efs-dired-close-ftp-process)
  (define-key efs-dired-map "k" 'efs-dired-kill-ftp-process)
  (define-key efs-dired-map "o" 'efs-dired-display-ftp-process-buffer)
  (define-key efs-dired-map "p" 'efs-dired-ping-connection))

(fset 'efs-dired-prefix efs-dired-map)

;;; Functions for dealing with the FTP process

(defun efs-dired-close-ftp-process ()
  "Close the FTP process for the current dired buffer.
Closing causes the connection to be dropped, but efs will retain its
cached data for the connection.  This will make it more efficient to
reopen the connection."
  (interactive)
  (or efs-dired-host-type
      (error "Dired buffer is not for a remote directory."))
  (efs-close-ftp-process (current-buffer))
  (let ((parsed (efs-ftp-path default-directory)))
    (message "Closed FTP connection for %s@%s." (nth 1 parsed) (car parsed))))

(defun efs-dired-kill-ftp-process ()
  "Kills the FTP process for the current dired buffer.
Killing causes the connection to be closed, the process buffer to be killed,
and most of efs's cached data to be wiped."
  (interactive)
  (or efs-dired-host-type
      (error "Dired buffer is not for a remote directory."))
  (efs-kill-ftp-process (current-buffer))
  (let ((parsed (efs-ftp-path default-directory)))
    (message "Killed FTP connection for %s@%s." (nth 1 parsed) (car parsed))))

(defun efs-dired-display-ftp-process-buffer ()
  "Displays in another window the FTP process buffer for a dired buffer."
  (interactive)
  (or efs-dired-host-type
      (error "Dired buffer is not for a remote directory."))
  (efs-display-ftp-process-buffer (current-buffer)))

(defun efs-dired-ping-connection ()
  "Pings FTP connection associated with current dired buffer."
  (interactive)
  (or efs-dired-host-type
      (error "Dired buffer is not for a remote directory."))
  (efs-ping-ftp-connection (current-buffer)))


;;; Reading in dired buffers.

(defun efs-dired-revert (&optional arg noconfirm)
  (let ((efs-ls-uncache t))
    (dired-revert arg noconfirm)))

(defun efs-dired-default-dir-function ()
  (let* ((cd (dired-current-directory))
	 (parsed (efs-ftp-path cd)))
    (if parsed
	(efs-save-match-data
	  (let ((tail directory-abbrev-alist))
	    (while tail
	      (if (string-match (car (car tail)) cd)
		  (setq cd (concat (cdr (car tail))
				   (substring cd (match-end 0)))
			parsed nil))
	      (setq tail (cdr tail)))
	    (apply 'efs-unexpand-parsed-filename
		   (or parsed (efs-ftp-path cd)))))
      cd)))

(defun efs-dired-before-readin ()
  ;; Put in the dired-before-readin-hook.
  (let ((parsed (efs-ftp-path default-directory)))
    (if parsed
	(let ((host (car parsed))
	      (user (nth 1 parsed)))
	  (setq efs-dired-listing-type (efs-listing-type host user)
		efs-dired-host-type (efs-host-type host)
		efs-dired-listing-type-string
		(and efs-show-host-type-in-dired
		     (concat " " (symbol-name efs-dired-listing-type))))
	  (set (make-local-variable 'revert-buffer-function)
	       (function efs-dired-revert))
	  (set (make-local-variable 'default-directory-function)
	       (function efs-dired-default-dir-function))
	  (set (make-local-variable 'dired-verify-modtimes)
	       (null (null (and
			    efs-dired-verify-modtime-host-regexp
			    (efs-save-match-data
			      (let ((case-fold-search t))
				(string-match
				 efs-dired-verify-modtime-host-regexp host))
				  (or efs-dired-verify-anonymous-modtime
				      (not (efs-anonymous-p user))))))))
	  ;; The hellsoft ftp server mixes up cases.
	  ;; However, we may not be able to catch this until
	  ;; after the first directory is listed. 
	  (if (and
	       (eq efs-dired-host-type 'hell)
	       (not (string-equal default-directory
				  (setq default-directory
					(downcase default-directory)))))
	      (or (string-equal (buffer-name) (downcase (buffer-name)))
		  (rename-buffer (generate-new-buffer-name
				  (directory-file-name default-directory)))))
	  ;; Setup the executable and directory regexps
	  (let ((eentry (assq efs-dired-listing-type
			      efs-dired-re-exe-alist))
		(dentry (assq efs-dired-listing-type
			      efs-dired-re-dir-alist)))
	    (if eentry
		(set (make-local-variable 'dired-re-exe) (cdr eentry)))
	    (if dentry
		(set (make-local-variable 'dired-re-dir) (cdr dentry))))
	  ;; No switches are sent to dumb hosts, so don't confuse dired.
	  ;; I hope that dired doesn't get excited if it doesn't see the l
	  ;; switch. If it does, then maybe fake things by setting this to
	  ;; "-Al".
	  (if (eq efs-dired-listing-type 'vms)
	      (setq dired-internal-switches
		    (delq ?F dired-internal-switches))
	    (if (memq efs-dired-host-type efs-dumb-host-types)
		(setq dired-internal-switches '(?l ?A)
		      ;; Don't lie on the mode line
		      dired-sort-mode "")))
	  ;; If the remote file system is version-based, don't set
	  ;; dired-kept-versions to 0. It will flag the most recent
	  ;; copy of the file for deletion -- this isn't really a backup.
	  (if (memq efs-dired-host-type efs-version-host-types)
	      (set (make-local-variable 'dired-kept-versions)
		   (max 1 dired-kept-versions)))))))

(efs-defun efs-dired-insert-headerline (&use efs-dired-listing-type) (dir)
  "Documented as original."
  (efs-real-dired-insert-headerline dir))

(defun efs-dired-uncache (file dir-p)
  ;; Remove FILE from cache.
  (if dir-p
      (efs-del-from-ls-cache file nil t)
    (efs-del-from-ls-cache file t nil)))

;;; Checking modtimes of directories.
;;
;;  This only runs if efs-dired-verify-anonymous-modtime and
;;  efs-verify-modtime-host-regexp turn it on.  Few (any?) FTP servers
;;  support getting MDTM for directories.  As usual, we cache whether
;;  this works, and don't keep senselessly trying it if it doesn't.

(defun efs-dired-file-modtime (file)
  ;; Returns the modtime.
  (let* ((parsed (efs-ftp-path file))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (rpath (nth 2 parsed)))
    (and (null (efs-get-host-property host 'dir-mdtm-failed))
	 (let ((result (efs-send-cmd host user (list 'quote 'mdtm rpath)
				     (and (eq efs-verbose t)
					  "Getting modtime")))
	       mp)
	   (if (and (null (car result))
		    (setq mp (efs-parse-mdtime (nth 1 result))))
	       (let ((ent (efs-get-file-entry file)))
		 (if ent
		     (setcdr ent (list (nth 1 ent) (nth 2 ent)
				       (nth 3 ent) (nth 4 ent) mp)))
		 parsed)
	     (efs-set-host-property host 'dir-mdtm-failed t)
	     nil)))))

(defun efs-dired-set-file-modtime (file alist)
  ;; This works asynch.
  (let* ((parsed (efs-ftp-path file))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (path (nth 2 parsed)))
    (if (efs-get-host-property host 'dir-mdtm-failed)
	(let ((elt (assoc file alist)))
	  (if elt (setcar (nthcdr 4 elt) nil)))
      (efs-send-cmd
       host user (list 'quote 'mdtm path) nil nil
       (efs-cont (result line cont-lines) (file alist host)
	 (let ((elt (assoc file alist))
	       modtime)
	   (if (and (null result) (setq modtime (efs-parse-mdtime line)))
	       (if elt (setcar (nthcdr 4 elt) modtime))
	     (if elt (setcar (nthcdr 4 elt) nil))
	     (efs-set-host-property host 'dir-mdtm-failed t))))
       0)    ; Always do this NOWAIT = 0
      nil))) ; return NIL

;;; Asynch insertion of subdirs.  Used when renaming subdirs.

(defun efs-dired-insert-subdir (dirname &optional noerror nowait)
  (let ((buff (current-buffer))
	(switches (delq ?R (copy-sequence dired-internal-switches))))
    (efs-ls
     dirname (dired-make-switches-string switches)
     t nil noerror nowait
     (efs-cont (listing) (dirname buff switches)
       (if (and listing (get-buffer buff))
	   (save-excursion
	     (set-buffer buff)
	     (save-excursion
	       (let ((elt (assoc dirname dired-subdir-alist))
		     mark-list)
		 (if elt
		     (setq mark-list (dired-insert-subdir-del elt))
		   (dired-insert-subdir-newpos dirname))
		 (dired-insert-subdir-doupdate
		  dirname
		  (efs-dired-insert-subdir-do-insert dirname listing)
		  switches elt mark-list)))))))))

(defun efs-dired-insert-subdir-do-insert (dirname listing)
  (let ((begin (point))
	indent-tabs-mode end)
    (insert listing)
    (setq end (point-marker))
    (indent-rigidly begin end 2)
    (goto-char begin)
    (dired-insert-headerline dirname)
    ;; If the listing has null lines `quote' them so that "\n\n" delimits
    ;; subdirs.  This is OK, because we aren't inserting -R listings.
    (save-excursion
      (while (search-forward "\n\n" end t)
	(forward-char -1)
	(insert " ")))
    ;; point is now like in dired-build-subdir-alist
    (prog1
	(list begin (marker-position end))
      (set-marker end nil))))

;;; Moving around in dired buffers.

(efs-defun efs-dired-manual-move-to-filename (&use efs-dired-listing-type)
  (&optional raise-error bol eol)
  "Documented as original."
  (efs-real-dired-manual-move-to-filename raise-error bol eol))

(efs-defun efs-dired-manual-move-to-end-of-filename
  (&use efs-dired-listing-type) (&optional no-error bol eol)
  "Documented as original."
  (efs-real-dired-manual-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-make-filename-string (&use efs-dired-listing-type)
  (filename &optional reverse)
  "Documented as original."
  ;; This translates file names from the way that they are displayed
  ;; in listings to the way that the user gives them in the minibuffer.
  ;; For example, in CMS this should take "FOO BAR" to "FOO.BAR".
  filename)

(defun efs-dired-find-file ()
  "Documented as original."
  (interactive)
  (find-file
   (if (memq efs-dired-host-type efs-version-host-types)
       (efs-internal-file-name-sans-versions
	efs-dired-host-type (dired-get-filename) t)
     (dired-get-filename))))

(defun efs-dired-find-file-other-window (&optional display)
  "Documented as original."
  (interactive "P")
  (if display
      (dired-display-file)
    (let ((file (dired-get-filename)))
      (if (memq efs-dired-host-type efs-version-host-types)
	  (setq file (efs-internal-file-name-sans-versions
		      efs-dired-host-type file t)))
      (find-file-other-window file))))

(defun efs-dired-display-file ()
  "Documented as original."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (memq efs-dired-host-type efs-version-host-types)
	(setq file (efs-internal-file-name-sans-versions
		    efs-dired-host-type file t)))
    (display-buffer (find-file-noselect file))))

(defun efs-dired-find-file-other-frame ()
  "Documented as original."
  (interactive)
  (find-file-other-frame
   (if (memq efs-dired-host-type efs-version-host-types)
       (efs-internal-file-name-sans-versions
	efs-dired-host-type (dired-get-filename) t)
     (dired-get-filename))))

;;; Creating and deleting new directories.

(defun efs-dired-recursive-delete-directory (fn)
  ;; Does recursive deletion of remote directories for dired.
  (or (file-exists-p fn)
      (signal 'file-error
	      (list "Removing old file name" "no such directory" fn)))
  (efs-dired-internal-recursive-delete-directory fn))

(defun efs-dired-internal-recursive-delete-directory (fn)
  (if (eq (car (file-attributes fn)) t)
      (let ((files (efs-directory-files fn)))
	(if files
	    (mapcar (function
		     (lambda (ent)
		       (or (string-equal "." ent)
			   (string-equal ".." ent)
			   (efs-dired-internal-recursive-delete-directory
			    (expand-file-name ent fn)))))
		    files))
	(efs-delete-directory fn))
    (condition-case err
	(efs-delete-file fn)
      (ftp-error (if (and (nth 2 err) (stringp (nth 2 err))
			  (efs-save-match-data
			    (string-match "^FTP Error: \"550 " (nth 2 err))))
		     (message "File %s already deleted." fn)
		   (signal (car err) (cdr err)))))))

;;; File backups and versions.

(efs-defun efs-dired-flag-backup-files
  (&use efs-dired-host-type) (&optional unflag-p)
  "Documented as original."
  (interactive "P")
  (efs-real-dired-flag-backup-files unflag-p))

(efs-defun efs-dired-collect-file-versions (&use efs-dired-host-type) ()
  ;; If it looks like a file has versions, return a list of the versions.
  ;; The return value is ((FILENAME . (VERSION1 VERSION2 ...)) ...)
  (efs-real-dired-collect-file-versions))

;;; Sorting dired buffers

(defun efs-dired-file-name-lessp (name1 name2)
  (if (and efs-dired-host-type
	   (memq efs-dired-host-type efs-case-insensitive-host-types))
      (string< (downcase name1) (downcase name2))
    (string< name1 name2)))

;;; Support for async file creators.

(defun efs-dired-copy-file (from to ok-flag &optional cont nowait)
  ;; Version of dired-copy-file for remote files.
  ;; Assumes that filenames are already expanded.
  (dired-handle-overwrite to)
  (efs-copy-file-internal from (efs-ftp-path from) to (efs-ftp-path to)
			  ok-flag dired-copy-preserve-time 0 cont nowait))

(defun efs-dired-rename-file (from to ok-flag &optional cont nowait
				   insert-subdir)
  ;; Version of dired-rename-file for remote files.
  (dired-handle-overwrite to)
  (efs-rename-file-internal
   from to ok-flag nil
   (efs-cont (result line cont-lines) (from to cont insert-subdir)
     (if result
	 (if cont
	     (efs-call-cont cont result line cont-lines)
	   (signal 'ftp-error
		   (list "Dired Renaming"
			 (format "FTP Error: \"%s\"" line)
			 from to)))
       (dired-remove-file from)
       ;; Silently rename the visited file of any buffer visiting this file.
       ;; We do not maintain inserted subdirs for remote 
       (efs-dired-rename-update-buffers from to insert-subdir)
       (if cont (efs-call-cont cont result line cont-lines))))
   nowait))

(defun efs-dired-rename-update-buffers (from to &optional insert-subdir)
  (if (get-file-buffer from)
      (save-excursion
	(set-buffer (get-file-buffer from))
	(let ((modflag (buffer-modified-p)))
	  (set-visited-file-name to)	; kills write-file-hooks
	  (set-buffer-modified-p modflag)))
    ;; It's a directory.  More work to do.
    (let ((blist (buffer-list))
	  (from-dir (file-name-as-directory from))
	  (to-dir (file-name-as-directory to)))
      (save-excursion
	(while blist
	  (set-buffer (car blist))
	  (setq blist (cdr blist))
	  (cond
	   (buffer-file-name
	    (if (dired-in-this-tree buffer-file-name from-dir)
		(let ((modflag (buffer-modified-p)))
		  (unwind-protect
		      (set-visited-file-name
		       (concat to-dir (substring buffer-file-name
						 (length from-dir))))
		    (set-buffer-modified-p modflag)))))
	   (dired-directory
	    (if (string-equal from-dir (expand-file-name default-directory))
		;; If top level directory was renamed, lots of things
		;; have to be updated.
		(progn
		  (dired-unadvertise from-dir)
		  (setq default-directory to-dir
			dired-directory
			;; Need to beware of wildcards.
			(expand-file-name 
			 (file-name-nondirectory dired-directory)
			 to-dir))
		  (let ((new-name (file-name-nondirectory
				   (directory-file-name dired-directory))))
		    ;; Try to rename buffer, but just leave old name if new
		    ;; name would already exist (don't try appending "<%d>")
		    ;; Why?  --sandy 19-8-94
		    (or (get-buffer new-name)
			(rename-buffer new-name)))
		  (dired-advertise))
	      (and insert-subdir
		   (assoc (file-name-directory (directory-file-name to))
			  dired-subdir-alist)
		   (if (efs-ftp-path to)
		       (efs-dired-insert-subdir to t 1)
		     (dired-insert-subdir to)))))))))))

(defun efs-dired-make-relative-symlink (from to ok-flag &optional cont nowait)
  ;; efs version of dired-make-relative-symlink
  ;; Called as a file-name-handler when dired-make-relative-symlink is
  ;; called interactively.
  ;; efs-dired-create-files calls it directly to supply CONT
  ;; and NOWAIT args.
  (setq from (directory-file-name from)
	to (directory-file-name to))
  (efs-make-symbolic-link-internal
   (dired-make-relative from (file-name-directory to) t)
   to ok-flag cont nowait))

(defun efs-dired-create-files (file-creator operation fn-list name-constructor
					    &optional marker-char query
					    implicit-to)
  "Documented as original."
  (if (catch 'found
	(let ((list fn-list)
	      val)
	  (while list
	    (if (setq val (efs-ftp-path (car list)))
		(throw 'found val)
	      (if (setq val (funcall name-constructor (car list)))
		  (throw 'found (efs-ftp-path val))
		(setq list (cdr list)))))))
      (progn
	(cond ((eq file-creator 'dired-copy-file)
	       (setq file-creator 'efs-dired-copy-file))
	      ((eq file-creator 'dired-rename-file)
	       (setq file-creator 'efs-dired-rename-file))
	      ((eq file-creator 'make-symbolic-link)
	       (setq file-creator 'efs-make-symbolic-link-internal))
	      ((eq file-creator 'add-name-to-file)
	       (setq file-creator 'efs-add-name-to-file-internal))
	      ((eq file-creator 'dired-make-relative-symlink)
	       (setq file-creator 'efs-dired-make-relative-symlink))
	      ((eq file-creator 'dired-compress-file)
	       (setq file-creator 'efs-dired-compress-file))
	      ((error "Unable to perform operation %s on remote hosts."
		      file-creator)))
	;; use the process-filter driven routine rather than the iterative one.
	(efs-dcf-1 file-creator operation fn-list name-constructor
		   (if (eq marker-char t)
		       (mapcar 'dired-file-marker fn-list)
		     marker-char)
		   query (buffer-name (current-buffer))
		   nil	;overwrite-query
		   nil	;dired-overwrite-backup-query
		   nil  ;dired-file-creator-query
		   nil	;failures
		   nil	;skipped
		   0		;success-count
		   (length fn-list) ;total
		   implicit-to
		   (and (eq file-creator 'efs-dired-rename-file)
			(delq nil
			      (mapcar
			       (function
				(lambda (x)
				  (and (assoc (file-name-as-directory x)
					      dired-subdir-alist)
				       x)))
			       fn-list)))))
    ;; normal case... use the interative routine... much cheaper.
    (efs-real-dired-create-files file-creator operation fn-list
				 name-constructor marker-char query
				 implicit-to)))

(defun efs-dcf-1 (file-creator operation fn-list name-constructor
			       markers query buffer-name overwrite-query 
			       overwrite-backup-query file-creator-query
			       failures skipped success-count total
			       implicit-to insertions)
  (if (null fn-list)
      (efs-dcf-3 failures operation total skipped
		 success-count buffer-name)
    (let* ((from (car fn-list))
	   ;; For dired-handle-overwrite and the file-creator-query,
	   ;; need to set these 2 fluid vars according to the cont data.
	   (dired-overwrite-backup-query overwrite-backup-query)
	   (dired-file-creator-query file-creator-query)
	   (to (funcall name-constructor from))
	   (marker-char (if (consp markers)
			    (prog1 (car markers)
			      (setq markers (cdr markers)))
			  markers))
	   (fn-list (cdr fn-list)))
      (if to
	  (if (equal to from)
	      (progn
		(dired-log buffer-name "Cannot %s to same file: %s\n"
			   (downcase operation) from)
		(efs-dcf-1 file-creator operation fn-list name-constructor
			   markers query buffer-name overwrite-query
			   dired-overwrite-backup-query
			   dired-file-creator-query failures
			   (cons (dired-make-relative from nil t) skipped)
			   success-count total implicit-to insertions))
	    (if (or (null query)
		    (funcall query from to))
		(let* ((overwrite (let (jka-compr-enabled)
				    ;; Don't let jka-compr fool us.
				    (file-exists-p to)))
		       (overwrite-confirmed ; for dired-handle-overwrite
			(and overwrite
			     (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
			       (dired-query 'overwrite-query
					    "Overwrite `%s'?" to)))))
		  (condition-case err
		      (let ((dired-unhandle-add-files
			     (cons to dired-unhandle-add-files)))
			(if implicit-to
			    (funcall file-creator from overwrite-confirmed
				     (list (function efs-dcf-2)
					   file-creator operation fn-list
					   name-constructor markers
					   query marker-char
					   buffer-name to from overwrite
					   overwrite-confirmed overwrite-query 
					   dired-overwrite-backup-query
					   dired-file-creator-query
					   failures skipped success-count
					   total implicit-to insertions)
				     t)
			  (apply file-creator from to overwrite-confirmed
				 (list (function efs-dcf-2)
				       file-creator operation fn-list
				       name-constructor markers
				       query marker-char
				       buffer-name to from overwrite
				       overwrite-confirmed overwrite-query 
				       dired-overwrite-backup-query
				       dired-file-creator-query
				       failures skipped success-count total
				       implicit-to insertions)
				 (if insertions
				     (list t insertions)
				   '(t)))))
		    (error      		; FILE-CREATOR aborted
		     (efs-dcf-2 'failed ;result
				(format "%s" err) ;line
				"" file-creator operation fn-list
				name-constructor markers query marker-char
				buffer-name to from overwrite
				overwrite-confirmed overwrite-query
				dired-overwrite-backup-query
				dired-file-creator-query failures skipped
				success-count total implicit-to insertions))))
	      (efs-dcf-1 file-creator operation fn-list name-constructor
			 markers query buffer-name overwrite-query
			 dired-overwrite-backup-query dired-file-creator-query
			 failures
			 (cons (dired-make-relative from nil t) skipped)
			 success-count total implicit-to insertions)))
	(efs-dcf-1 file-creator operation fn-list name-constructor
		   markers query buffer-name overwrite-query
		   dired-overwrite-backup-query dired-file-creator-query
		   failures (cons (dired-make-relative from nil t) skipped)
		   success-count total implicit-to insertions)))))

(defun efs-dcf-2 (result line cont-lines file-creator operation fn-list
			 name-constructor markers query marker-char
			 buffer-name to from overwrite overwrite-confirmed
			 overwrite-query overwrite-backup-query
			 file-creator-query failures skipped success-count
			 total implicit-to insertions)
  (if result
      (progn
	(setq failures (cons (dired-make-relative from nil t) failures))
	(dired-log buffer-name "%s `%s' to `%s' failed:\n%s\n"
		   operation from to line))
    (setq success-count (1+ success-count))
    (message "%s: %d of %d" operation success-count total)
    (let ((efs-ls-uncache t))
      (dired-add-file to marker-char)))
  ;; iterate again
  (efs-dcf-1 file-creator operation fn-list name-constructor
	     markers query buffer-name overwrite-query overwrite-backup-query
	     file-creator-query failures skipped success-count total
	     implicit-to insertions))

(defun efs-dcf-3 (failures operation total skipped success-count buffer-name)
  (cond
   (failures
    (dired-log-summary buffer-name (format "%s failed for %d of %d file%s"
					   operation (length failures) total
					   (dired-plural-s total)) failures))
   (skipped
    (dired-log-summary buffer-name (format "%s: %d of %d file%s skipped"
					   operation (length skipped) total
					   (dired-plural-s total)) skipped))
   (t
    (message "%s: %s file%s."
	     operation success-count
	     (dired-plural-s success-count)))))

;;; Running remote shell commands

;;; This support isn't very good. efs is really about a virtual file system,
;;; and not remote processes. What is really required is low-level
;;; support for start-process & call-process on remote hosts. This shouldn't
;;; be part of efs, although.

(defun efs-dired-shell-unhandle-file-name (filename)
  ;; Puts remote file names into a form where they can be passed to remsh.
  (nth 2 (efs-ftp-path filename)))

(defun efs-dired-shell-call-process (command dir &optional in-background)
  ;; Runs shell process on remote hosts.
  (let* ((parsed (efs-ftp-path dir))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (rdir (nth 2 parsed))
	 (file-name-handler-alist nil))
    (or (string-equal (efs-internal-directory-file-name dir)
		      (efs-expand-tilde "~" (efs-host-type host) host user))
	(string-match "^cd " command)
	(setq command (concat "cd " rdir "; " command)))
    (setq command
	  (format  "%s %s%s \"%s\""	; remsh -l USER does not work well
					; on a hp-ux machine I tried
		   efs-remote-shell-file-name host
		   (if efs-remote-shell-takes-user
		       (concat " -l " user)
		     "")
		   command))
    (message "Doing shell command on %s..." host)
    (dired-shell-call-process
     command (file-name-directory efs-tmp-name-template) in-background)))

;;; Dired commands for running local processes on remote files.
;;
;;  Lots of things in this section need to be re-thunk.

(defun efs-dired-call-process (program discard &rest arguments)
  "Documented as original."
  ;; PROGRAM is always one of those below in the cond in dired.el.
  ;; The ARGUMENTS are (nearly) always files.
  (if (efs-ftp-path default-directory)
      ;; Can't use efs-dired-host-type here because the current
      ;; buffer is *dired-check-process output*
      (condition-case oops
	  (cond
	   ((string-equal "efs-call-compress" program)
	    (apply 'efs-call-compress arguments))
	   ((string-equal "chmod" program)
	    (efs-call-chmod arguments))
	   (t (error "Unknown remote command: %s" program)))
	(ftp-error (dired-log (buffer-name (current-buffer))
			      (format "%s: %s, %s\n"
				      (nth 1 oops)
				      (nth 2 oops)
				      (nth 3 oops))))
	(error (dired-log (buffer-name (current-buffer))
			  (format "%s\n" (nth 1 oops)))))
    (apply 'call-process program nil (not discard) nil arguments)))

(defun efs-dired-make-compressed-filename (name &optional method)
  ;; Version of dired-make-compressed-filename for efs.
  ;; If NAME is in the syntax of a compressed file (according to
  ;; dired-compression-method-alist), return the data (a list) from this
  ;; alist on how to uncompress it. Otherwise, return a string, the
  ;; uncompressed form of this file name. This is computed using the optional
  ;; argument METHOD (a symbol). If METHOD is nil, the ambient value of
  ;; dired-compression-method is used.
  (let* ((host-type (efs-host-type (car (efs-ftp-path name))))
	 (ef-alist (if (memq host-type efs-single-extension-host-types)
		       (mapcar
			(function
			 (lambda (elt)
			   (list (car elt)
				 (mapconcat
				  (function
				   (lambda (char)
				     (if (= char ?.)
					 "-"
				       (char-to-string char))))
				  (nth 1 elt) "")
				 (nth 2 elt)
				 (nth 3 elt))))
			dired-compression-method-alist)
		     dired-compression-method-alist))
	 (alist ef-alist)
	 (len (length name))
	 ext ext-len result)
    (if (memq host-type efs-version-host-types)
	(setq name (efs-internal-file-name-sans-versions host-type name)))
    (if (memq host-type efs-case-insensitive-host-types)
	(let ((name (downcase name)))
	  (while alist
	    (if (and (> len
			(setq ext-len (length (setq ext (nth 1 (car alist))))))
		     (string-equal (downcase ext)
				   (substring name (- ext-len))))
		(setq result (car alist)
		      alist nil)
	      (setq alist (cdr alist)))))
      (while alist
	(if (and (> len
		    (setq ext-len (length (setq ext (nth 1 (car alist))))))
		 (string-equal ext (substring name (- ext-len))))
	    (setq result (car alist)
		  alist nil)
	  (setq alist (cdr alist)))))
    (or result
	(concat name
		(nth 1 (or (assq (or method dired-compression-method)
				 ef-alist)
			   (error "Unknown compression method: %s"
				  (or method dired-compression-method))))))))

(defun efs-dired-compress-file (file ok-flag &optional cont nowait)
  ;; Version of dired-compress-file for remote files.
  (let* ((compressed-fn (efs-dired-make-compressed-filename file))
	 (host (car (efs-ftp-path file)))
	 (host-type (efs-host-type host)))
    (cond ((file-symlink-p file)
	   (if cont
	       (efs-call-cont
		cont 'failed
		(format "Cannot compress %s, a symbolic link." file) "")
	     (signal 'file-error (list "Compress error:" file
				       "a symbolic link"))))
	  ((listp compressed-fn)
	   (let ((newname (substring (if (memq host-type
					       efs-version-host-types)
					 (efs-internal-file-name-sans-versions
					  host-type file)
				       file)
				      0 (- (length (nth 1 compressed-fn)))))
		 (program (nth 3 compressed-fn)))
	     (if (and (memq host-type efs-unix-host-types)
		      (null (efs-get-host-property host 'exec-failed))
		      (null (eq (efs-get-host-property
				 host
				 (intern
				  (concat
				   "exec-"
				   (efs-compress-progname (car program)))))
				'failed)))
		 (efs-call-remote-compress
		  program file newname t ok-flag
		  (efs-cont (result line cont-lines) (program file newname
							      cont nowait)
		    (if result
			(if (eq result 'unsupported)
			    (efs-call-compress program file newname
					       t t cont nowait)
			  (if cont
			      (efs-call-cont cont result line cont-lines)
			    (signal 'ftp-error
				    (list "Uncompressing file"
					  (format "FTP Error: \"%s\" " line)
					  file))))
		      (if cont (efs-call-cont cont result line cont-lines))))
		  nowait)
	       (efs-call-compress
		program file newname t ok-flag cont nowait)
	       newname)))
	  ((stringp compressed-fn)
	   (let ((program (nth 2 (assq dired-compression-method
				       dired-compression-method-alist))))
	     (if (and (memq host-type efs-unix-host-types)
		      (null (efs-get-host-property host 'exec-failed))
		      (null (eq (efs-get-host-property
				 host
				 (intern
				  (concat
				   "exec-"
				   (efs-compress-progname (car program)))))
				'failed)))
		 (efs-call-remote-compress
		  program file compressed-fn nil ok-flag
		  (efs-cont (result line cont-lines) (program file
							      compressed-fn
							      cont nowait)
		    (if result
			(if (eq result 'unsupported)
			    (efs-call-compress program file compressed-fn nil
					       t cont nowait)
			  (if cont
			      (efs-call-cont cont result line cont-lines)
			    (signal 'ftp-error
				    (list "Compressing file"
					  (format "FTP Error: \"%s\" " line)
					  file))))
		      (if cont (efs-call-cont cont result line cont-lines))))
		  nowait)
	       (efs-call-compress
		program file compressed-fn nil ok-flag cont nowait)))
	   compressed-fn)
	  (t (error "Strange error in efs-dired-compress-file.")))))

(defun efs-dired-print-file (command file)
  ;; Version of dired-print-file for remote files.
  (let ((command (dired-trans-command command (list file) "")))
    ;; Only replace the first occurence of the file name?
    (if (string-match (concat "[ ><|]\\(" (regexp-quote
					   (dired-shell-quote file))
			      "\\)\\($\\|[ |><&]\\)")
		      command)
	(setq command (concat (substring command 0 (match-beginning 1))
			      "%s"
			      (substring command (match-end 1))))
      (error "efs-print-command: strange error"))
  (efs-call-lpr file command)))

;;;;----------------------------------------------------------------
;;;; Support for `processes' run on remote files.
;;;; Usually (but not necessarily) these are only called from dired.
;;;;----------------------------------------------------------------

(defun efs-compress-progname (program)
  ;; Returns a canonicalized i.e. without the "un", version of a compress
  ;; program name.
  (efs-save-match-data
    (if (string-equal program "gunzip")
	"gzip"
      (if (string-match "^un" program)
	  (substring program (match-end 0))
	program))))

(defun efs-call-remote-compress (program filename newname &optional uncompress
					 ok-if-already-exists cont nowait)
  ;; Run a remote compress process using SITE EXEC.
  (if (or (not ok-if-already-exists)
	  (numberp ok-if-already-exists))
      (efs-barf-or-query-if-file-exists
       newname
       (if uncompress
	   "uncompress to it"
	 "compress to it")
       (numberp ok-if-already-exists)))
  (let* ((filename (expand-file-name filename))
	 (parsed (efs-ftp-path filename))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (rpath (nth 2 parsed)))
    (if (efs-get-host-property host 'exec-failed)
	(if cont
	    (efs-call-cont cont 'unsupported "SITE EXEC not supported" "")
	  (signal 'ftp-error (list "Unable to SITE EXEC" host)))
      (let* ((progname (efs-compress-progname (car program)))
	     (propsym (intern  (concat "exec-" progname)))
	     (prop (efs-get-host-property host propsym)))
	(cond
	 ((eq prop 'failed)
	  (if cont
	      (efs-call-cont cont 'unsupported
			     (concat progname " not in FTP exec path") "")
	    (signal 'ftp-error
		    (list (concat progname " not in FTP exec path") host))))
	 ((eq prop 'worked)
	  (efs-send-cmd
	   host user
	   (list 'quote 'site 'exec
		 (concat (mapconcat 'identity program " ") " " rpath))
	   (concat (if uncompress "Uncompressing " "Compressing ") filename)
	   nil
	   (efs-cont (result line cont-lines) (host user filename cont)
	     (if result
		 (progn
		   (efs-set-host-property host 'exec-failed t)
		   (efs-error host user (concat "FTP exec Error: " line)))
	       (efs-save-match-data
		 (if (string-match "\n200-\\([^\n]*\\)" cont-lines)
		     (let ((err (substring cont-lines (match-beginning 1)
					   (match-end 1))))
		       (if cont
			   (efs-call-cont cont  'failed err cont-lines)
			 (efs-error host user (concat "FTP Error: " err))))
		   ;; This function only gets called for unix hosts, so
		   ;; we'll use the default version of efs-delete-file-entry
		   ;; and save a host-type lookup.
		   (efs-delete-file-entry nil filename)
		   (dired-remove-file filename)
		   (if cont (efs-call-cont cont nil line cont-lines))))))
	   nowait))
	 (t ; (null prop)
	  (efs-send-cmd
	   host user
	   (list 'quote 'site 'exec (concat progname " " "-V"))
	   (format "Checking for %s executable" progname)
	   nil
	   (efs-cont (result line cont-lines) (propsym host program filename
						       newname uncompress
						       cont nowait)
	     (efs-save-match-data
	       (if (string-match "\n200-" cont-lines)
		   (efs-set-host-property host propsym 'worked)
		 (efs-set-host-property host propsym 'failed)))
	     (efs-call-remote-compress program filename newname uncompress
				       t ; already tested for overwrite
				       cont nowait))
	   nowait)))))))

(defun efs-call-compress (program filename newname &optional uncompress
				  ok-if-already-exists cont nowait)
  "Perform a compress command on a remote file.
PROGRAM is a list of the compression program and args. Works by taking a 
copy of the file, compressing it and copying the file back. Returns 0 on
success, 1 or 2 on failure. If UNCOMPRESS is non-nil, does this instead."
  (let* ((filename (expand-file-name filename))
	 (newname (expand-file-name newname))
	 (parsed (efs-ftp-path filename))
	 (tmp1 (car (efs-make-tmp-name nil (car parsed))))
	 (tmp2 (car (efs-make-tmp-name nil (car parsed))))
	 (program (mapconcat 'identity program " ")))
    (efs-copy-file-internal
     filename parsed tmp1 nil
     t nil 2
     (efs-cont (result line cont-lines) (filename newname tmp1 tmp2 program
				       uncompress ok-if-already-exists
				       cont nowait)
       (if result
	   (signal 'ftp-error
		   (list "Opening input file"
			 (format "FTP Error: \"%s\" " line) filename))
	 (let ((err-buff (let ((default-major-mode 'fundamental-mode))
			   (get-buffer-create
			    (generate-new-buffer-name
			     (format
			      " efs-call-compress %s" filename))))))
	   (save-excursion
	     (set-buffer err-buff)
	     (set (make-local-variable 'efs-call-compress-filename) filename)
	     (set (make-local-variable 'efs-call-compress-newname) newname)
	     (set (make-local-variable 'efs-call-compress-tmp1) tmp1)
	     (set (make-local-variable 'efs-call-compress-tmp2) tmp2)
	     (set (make-local-variable 'efs-call-compress-cont) cont)
	     (set (make-local-variable 'efs-call-compress-nowait) nowait)
	     (set (make-local-variable 'efs-call-compress-ok)
		  ok-if-already-exists)
	     (set (make-local-variable 'efs-call-compress-uncompress)
		  uncompress)
	     (set (make-local-variable 'efs-call-compress-abbr)
		  (efs-relativize-filename filename))
	     (if efs-verbose
		 (efs-message
		  (format "%s %s..."
			  (if uncompress "Uncompressing" "Compressing")
			  (symbol-value (make-local-variable
					 'efs-call-compress-abbr)))))
	     (set-process-sentinel
	      (start-process (format "efs-call-compress %s" filename)
			     err-buff shell-file-name
			     "-c" (format "%s %s < %s > %s"
					  program
					  ;; Hope -c makes the compress
					  ;; program write to std out.
					  "-c"
					  tmp1 tmp2))
	      (function
	       (lambda (proc str)
		 (let ((buff (get-buffer (process-buffer proc))))
		   (if buff
		       (save-excursion
			 (set-buffer buff)
			 (if (/= (buffer-size) 0)
			     (if cont
				 (efs-call-cont
				  (symbol-value
				   (make-local-variable
				    'efs-call-compress-cont))
				  'failed
				  (concat
				   "failed to compress "
				   (symbol-value (make-local-variable
						  'efs-call-compress-filename))
				   ", "
				   (buffer-substring
				    (point-min)
				    (progn (goto-char (point-min))
					   (end-of-line) (point))))))
			   (efs-del-tmp-name (symbol-value
					      (make-local-variable
					       'efs-call-compress-tmp1)))
			   (let ((tmp2 (symbol-value
					(make-local-variable
					 'efs-call-compress-tmp2)))
				 (newname (symbol-value
					   (make-local-variable
					    'efs-call-compress-newname)))
				 (filename (symbol-value
					    (make-local-variable
					     'efs-call-compress-filename)))
				 (cont (symbol-value
					(make-local-variable
					 'efs-call-compress-cont)))
				 (nowait (symbol-value
					  (make-local-variable
					   'efs-call-compress-nowait)))
				 (ok (symbol-value
				      (make-local-variable
				       'efs-call-compress-ok)))
				 (uncompress
				  (symbol-value
				   (make-local-variable
				    'efs-call-compress-uncompress))))
			     (if efs-verbose
				 (efs-message
				  (format "%s %s...done"
					  (if uncompress
					      "Uncompressing"
					    "Compressing")
					  (symbol-value
					   (make-local-variable
					    'efs-call-compress-abbr)))))
			     (kill-buffer (current-buffer))
			     (efs-copy-file-internal
			      tmp2 nil newname (efs-ftp-path newname)
			      ok nil 1
			      (efs-cont (result line cont-lines) (cont
								  tmp2
								  filename)
				(efs-del-tmp-name tmp2)
				(or result
				    (let (efs-verbose)
				      (efs-delete-file filename)
				      (dired-remove-file filename)))
				(if cont
				    (efs-call-cont cont result line
						   cont-lines)))
			      nowait (if uncompress nil 'image)))))
		     (error "Strange error: %s" proc))))))))))
     nowait (if uncompress 'image nil))))

(defun efs-update-mode-string (perms modes)
  ;; For PERMS of the form `u+w', and MODES a unix 9-character mode string,
  ;; computes the new mode string.
  ;; Doesn't call efs-save-match-data. The calling function should.
  (or (string-match "^[augo]+\\([+-]\\)[rwxst]+$" perms)
      (error "efs-update-mode-string: invalid perms %s" perms))
  (let* ((who (substring perms 0 (match-beginning 1)))
	 (add (= (aref perms (match-beginning 1)) ?+))
	 (what (substring perms (match-end 1)))
	 (newmodes (copy-sequence modes))
	 (read (string-match "r" what))
	 (write (string-match "w" what))
	 (execute (string-match "x" what))
	 (sticky (string-match "t" what))
	 (suid (string-match "s" what)))
    (if (string-match "a" who)
	(if add
	    (progn
	      (if read
		  (progn
		    (aset newmodes 0 ?r)
		    (aset newmodes 3 ?r)
		    (aset newmodes 6 ?r)))
	      (if write
		  (progn
		    (aset newmodes 1 ?w)
		    (aset newmodes 4 ?w)
		    (aset newmodes 7 ?w)))
	      (if execute
		  (let ((curr (aref newmodes 2)))
		    (if (= curr ?-)
			(aset newmodes 2 ?x)
		      (if (= curr ?S)
			  (aset newmodes 2 ?s)))
		    (setq curr (aref newmodes 5))
		    (if (= curr ?-)
			(aset newmodes 5 ?x)
		      (if (= curr ?S)
			  (aset newmodes 5 ?s)))
		    (setq curr (aref newmodes 8))
		    (if (= curr ?-)
			(aset newmodes 8 ?x)
		      (if (= curr ?T)
			  (aset newmodes 8 ?t)))))
	      (if suid
		  (let ((curr (aref newmodes 2)))
		    (if (= curr ?-)
			(aset newmodes 2 ?S)
		      (if (= curr ?x)
			  (aset newmodes 2 ?s)))
		    (setq curr (aref newmodes 5))
		    (if (= curr ?-)
			(aset newmodes 5 ?S)
		      (if (= curr ?x)
			  (aset newmodes 5 ?s)))))
	      (if sticky
		  (let ((curr (aref newmodes 8)))
		    (if (= curr ?-)
			(aset newmodes 8 ?T)
		      (if (= curr ?x)
			  (aset newmodes 8 ?t))))))
	  (if read
	      (progn
		(aset newmodes 0 ?-)
		(aset newmodes 3 ?-)
		(aset newmodes 6 ?-)))
	  (if write
	      (progn
		(aset newmodes 1 ?-)
		(aset newmodes 4 ?-)
		(aset newmodes 7 ?-)))
	  (if execute
	      (let ((curr (aref newmodes 2)))
		(if (= curr ?x)
		    (aset newmodes 2 ?-)
		  (if (= curr ?s)
		      (aset newmodes 2 ?S)))
		(setq curr (aref newmodes 5))
		(if (= curr ?x)
		    (aset newmodes 5 ?-)
		  (if (= curr ?s)
		      (aset newmodes 5 ?S)))
		    (setq curr (aref newmodes 8))
		    (if (= curr ?x)
			(aset newmodes 8 ?-)
		      (if (= curr ?t)
			  (aset newmodes 8 ?T)))))
	  (if suid
	      (let ((curr (aref newmodes 2)))
		(if (= curr ?s)
		    (aset newmodes 2 ?x)
		  (if (= curr ?S)
		      (aset newmodes 2 ?-)))
		(setq curr (aref newmodes 5))
		(if (= curr ?s)
		    (aset newmodes 5 ?x)
		  (if (= curr ?S)
		      (aset newmodes 5 ?-)))))
	  (if sticky
	      (let ((curr (aref newmodes 8)))
		(if (= curr ?t)
		    (aset newmodes 8 ?x)
		  (if (= curr ?T)
		      (aset newmodes 8 ?-))))))
      (if (string-match "u" who)
	  (if add
	      (progn
		(if read
		    (aset newmodes 0 ?r))
		(if write
		    (aset newmodes 1 ?w))
		(if execute
		    (let ((curr (aref newmodes 2)))
		      (if (= curr ?-)
			  (aset newmodes 2 ?x)
			(if (= curr ?S)
			    (aset newmodes 2 ?s)))))
		(if suid
		    (let ((curr (aref newmodes 2)))
		      (if (= curr ?-)
			  (aset newmodes 2 ?S)
			(if (= curr ?x)
			    (aset newmodes 2 ?s))))))
	    (if read
		(aset newmodes 0 ?-))
	    (if write
		(aset newmodes 1 ?-))
	    (if execute
		(let ((curr (aref newmodes 2)))
		  (if (= curr ?x)
		      (aset newmodes 2 ?-)
		    (if (= curr ?s)
			(aset newmodes 2 ?S)))))
	    (if suid
		(let ((curr (aref newmodes 2)))
		  (if (= curr ?s)
		      (aset newmodes 2 ?x)
		    (if (= curr ?S)
			(aset newmodes 2 ?-)))))))
      (if (string-match "g" who)
	  (if add
	      (progn
		(if read
		    (aset newmodes 3 ?r))
		(if write
		    (aset newmodes 4 ?w))
		(if execute
		    (let ((curr (aref newmodes 5)))
		      (if (= curr ?-)
			  (aset newmodes 5 ?x)
			(if (= curr ?S)
			    (aset newmodes 5 ?s)))))
		(if suid
		    (let ((curr (aref newmodes 5)))
		      (if (= curr ?-)
			  (aset newmodes 5 ?S)
			(if (= curr ?x)
			    (aset newmodes 5 ?s))))))
	    (if read
		(aset newmodes 3 ?-))
	    (if write
		(aset newmodes 4 ?-))
	    (if execute
		(let ((curr (aref newmodes 5)))
		  (if (= curr ?x)
		      (aset newmodes 5 ?-)
		    (if (= curr ?s)
			(aset newmodes 5 ?S)))))
	    (if suid
		(let ((curr (aref newmodes 5)))
		  (if (= curr ?s)
		      (aset newmodes 5 ?x)
		    (if (= curr ?S)
			(aset newmodes 5 ?-)))))))
      (if (string-match "o" who)
	  (if add
	      (progn
		(if read
		    (aset newmodes 6 ?r))
		(if write
		    (aset newmodes 7 ?w))
		(if execute
		    (let ((curr (aref newmodes 8)))
		      (if (= curr ?-)
			  (aset newmodes 8 ?x)
			(if (= curr ?T)
			    (aset newmodes 8 ?t)))))
		(if sticky
		    (let ((curr (aref newmodes 8)))
		      (if (= curr ?-)
			  (aset newmodes 8 ?T)
			(if (= curr ?x)
			    (aset newmodes 5 ?t))))))
	    (if read
		(aset newmodes 6 ?-))
	    (if write
		(aset newmodes 7 ?-))
	    (if execute
		(let ((curr (aref newmodes 8)))
		  (if (= curr ?x)
		      (aset newmodes 8 ?-)
		    (if (= curr ?t)
			(aset newmodes 8 ?T)))))
	    (if suid
		(let ((curr (aref newmodes 8)))
		  (if (= curr ?t)
		      (aset newmodes 8 ?x)
		    (if (= curr ?T)
			(aset newmodes 8 ?-))))))))
    newmodes))

(defun efs-compute-chmod-arg (perms file)
  ;; Computes the octal number, represented as a string, required to 
  ;; modify the permissions PERMS of FILE.
  (efs-save-match-data
    (cond
     ((string-match "^[0-7][0-7]?[0-7]?[0-7]?$" perms)
      perms)
     ((string-match "^[augo]+[-+][rwxst]+$" perms)
      (let ((curr-mode (nth 3 (efs-get-file-entry file))))
	(or (and curr-mode
		 (stringp curr-mode)
		 (= (length curr-mode) 10))
	    (progn
	      ;; Current buffer is process error buffer
	      (insert "Require an octal integer to modify modes for "
		      file ".\n")
	      (error "Require an octal integer to modify modes for %s." file)))
	(format "%o"
		(efs-parse-mode-string
		 (efs-update-mode-string perms
					      (substring curr-mode 1))))))
     (t
      (insert "Don't know how to set modes " perms " for " file ".\n")
      (error "Don't know how to set modes %s" perms)))))

(defun efs-call-chmod (args)
  ;; Sends an FTP CHMOD command.
  (if (< (length args) 2)
      (error "efs-call-chmod: missing mode and/or filename: %s" args))
  (let ((mode (car args))
	bombed)
    (mapcar
     (function
      (lambda (file)
	(setq file (expand-file-name file))
	(let ((parsed (efs-ftp-path file)))
	  (if parsed
	      (condition-case nil
		  (let* ((mode (efs-compute-chmod-arg mode file))
			 (host (nth 0 parsed))
			 (user (nth 1 parsed))
			 (path (efs-quote-string
				(efs-host-type host user) (nth 2 parsed)))
			 (abbr (efs-relativize-filename file))
			 (result (efs-send-cmd host user
						    (list 'quote 'site 'chmod
							  mode path)
						    (format "doing chmod %s"
							    abbr))))
		    (efs-del-from-ls-cache file t)
		    (if (car result)
			(efs-error host user (format "chmod: %s: \"%s\"" file 
						     (nth 1 result)))))
		(error (setq bombed t)))))))
     (cdr args))
    (if bombed 1 0)))                      ; return code

(defun efs-call-lpr (file command-format)
  "Print remote file FILE. SWITCHES are passed to the print program."
  ;; Works asynch.
  (let* ((file (expand-file-name file))
	 (parsed (efs-ftp-path file))
	 (abbr (efs-relativize-filename file))
	 (temp (car (efs-make-tmp-name nil (car parsed)))))
    (efs-copy-file-internal
     file parsed temp nil t nil 2
     (efs-cont (result line cont-lines) (command-format file abbr temp)
       (if result
	   (signal 'ftp-error (list "Opening input file"
				    (format "FTP Error: \"%s\" " line)
				    file))
	 (message "Spooling %s..." abbr)
	 (set-process-sentinel
	  (start-process (format "*print %s /// %s*" abbr temp)
			 (generate-new-buffer-name " *print temp*")
			 "sh" "-c" (format command-format temp))
	  (function
	   (lambda (proc status)
	     (let ((buff (process-buffer proc))
		   (name (process-name proc)))
	       (if (and buff (get-buffer buff))
		   (unwind-protect
		       (save-excursion
			 (set-buffer buff)
			 (if (> (buffer-size) 0)
			     (let ((log-buff (get-buffer-create
					      "*Shell Command Output*")))
			       (set-buffer log-buff)
			       (goto-char (point-max))
			       (or (bobp)
				   (insert "\n"))
			       (insert-buffer-substring buff)
			       (goto-char (point-max))
			       (display-buffer log-buff))))
		     (condition-case nil (kill-buffer buff) (error nil))
		     (efs-save-match-data
		       (if (string-match "^\\*print \\(.*\\) /// \\(.*\\)\\*$"
					 name)
			   (let ((abbr (substring name (match-beginning 1)
						  (match-end 1)))
				 (temp (substring name (match-beginning 2)
						  (match-end 2))))
			     (or (= (match-beginning 2) (match-end 2))
				 (efs-del-tmp-name temp))
			     (message "Spooling %s...done" abbr))))))))))))
     t)))

;;;; --------------------------------------------------------------
;;;; Attaching onto dired.
;;;; --------------------------------------------------------------

;;; Look out for MULE
(if (or (boundp 'MULE) (featurep 'mule)) (load "efs-dired-mule"))

;;; Magic file name hooks for dired.

(put 'dired-print-file 'efs 'efs-dired-print-file)
(put 'dired-make-compressed-filename 'efs 'efs-dired-make-compressed-filename)
(put 'dired-compress-file 'efs 'efs-dired-compress-file)
(put 'dired-recursive-delete-directory 'efs
     'efs-dired-recursive-delete-directory)
(put 'dired-uncache 'efs 'efs-dired-uncache)
(put 'dired-shell-call-process 'efs 'efs-dired-shell-call-process)
(put 'dired-shell-unhandle-file-name 'efs 'efs-dired-shell-unhandle-file-name)
(put 'dired-file-modtime 'efs 'efs-dired-file-modtime)
(put 'dired-set-file-modtime 'efs 'efs-dired-set-file-modtime)

;;; Overwriting functions

(efs-overwrite-fn "efs" 'dired-call-process)
(efs-overwrite-fn "efs" 'dired-insert-headerline)
(efs-overwrite-fn "efs" 'dired-manual-move-to-filename)
(efs-overwrite-fn "efs" 'dired-manual-move-to-end-of-filename)
(efs-overwrite-fn "efs" 'dired-make-filename-string)
(efs-overwrite-fn "efs" 'dired-flag-backup-files)
(efs-overwrite-fn "efs" 'dired-create-files)
(efs-overwrite-fn "efs" 'dired-find-file)
(efs-overwrite-fn "efs" 'dired-find-file-other-window)
(efs-overwrite-fn "efs" 'dired-find-file-other-frame)
(efs-overwrite-fn "efs" 'dired-collect-file-versions)
(efs-overwrite-fn "efs" 'dired-file-name-lessp)

;;; Hooks

(add-hook 'dired-before-readin-hook 'efs-dired-before-readin)

;;; Handle dired-grep.el too.

(if (featurep 'dired-grep)
    (efs-overwrite-fn "efs" 'dired-grep-delete-local-temp-file
		      'efs-diff/grep-del-temp-file)
  (add-hook 'dired-grep-load-hook
	    (function
	     (lambda ()
	       (efs-overwrite-fn "efs" 'dired-grep-delete-local-temp-file
				 'efs-diff/grep-del-temp-file)))))

;;; end of efs-dired.el
