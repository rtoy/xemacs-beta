;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-tops-20.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  TOPS-20 support for efs
;; Author:       Sandy Rutherford <sandy@math.ubc.ca, sandy@itp.ethz.ch>
;; Created:      Fri Oct 23 08:52:00 1992
;; Modified:     Sun Nov 27 18:43:45 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(require 'efs)
(provide 'efs-tops-20)

(defconst efs-tops-20-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; TOPS-20 support
;;;; ------------------------------------------------------------

(efs-defun efs-send-pwd tops-20 (host user &optional xpwd)
  ;; pwd doesn't work for tops-20. Need to get the cwd from a dir listing
  ;; this function returns the cwd in tops-20 syntax
  (let* ((temp (efs-make-tmp-name host nil))
	 (cmd (concat "dir * " (cdr temp)))
	 dir u-dir full-dir result)
    (unwind-protect
	(if (null (and (car (setq result (efs-raw-send-cmd
					  (efs-get-process host user)
					  cmd
					  "Getting TOPS-20 PWD")))
		       (progn
			 (condition-case ()
			     (delete-file (car temp)) (error nil))
			 (car (setq result
				    (efs-raw-send-cmd
				     (efs-get-process host user)
				     cmd
				     "Trying to get TOPS-20 PWD, again."))))))
	    (save-excursion
	      (set-buffer (get-buffer-create
			   efs-data-buffer-name))
	      (erase-buffer)
	      (if (or (file-readable-p (car temp))
		      (sleep-for efs-retry-time)
		      (file-readable-p (car temp)))
		    ;; Try again.
		  (insert-file-contents (car temp))
		(efs-error host user
			   (format
			    "list data file %s not readable" (car temp))))
	      ;; get the cwd
	      (goto-char (point-min))
	      (efs-save-match-data
		(if (looking-at "[^ /:]+:<[^<>/ ]+>")
		    (progn
		      (setq dir (buffer-substring (match-beginning 0)
						  (match-end 0))
			    u-dir (efs-internal-directory-file-name
				   (efs-fix-path 'tops-20 dir t))
			    full-dir (format efs-path-format-string
					     user host u-dir))
		      ;; cache the files too
		      (efs-set-files full-dir
					  (efs-parse-listing
					   'tops-20 host user u-dir full-dir))
		      (efs-add-to-ls-cache full-dir nil (buffer-string) t))))))
      (efs-del-tmp-name (car temp)))
    (cons dir (nth 1 result))))

(efs-defun efs-fix-path tops-20 (path &optional reverse)
  ;; Convert PATH from UNIX-ish to tops-20. If REVERSE given, then
  ;; do just that.
  (efs-save-match-data
    (if reverse
	(if (string-match "^\\([^:]+:\\)?<\\([^>.][^>]*\\)>.*$" path)
	    (let ((device (and (match-beginning 1)
			       (substring path (match-beginning 1)
					  (match-end 1))))
		  (dir (substring path (match-beginning 2)
				  (match-end 2)))
		  (file (substring path (1+ (match-end 2)))))
	      (while (string-match "\\." dir)
		(setq dir (concat (substring dir 0 (match-beginning 0))
				  "/"
				  (substring dir (match-end 0)))))
	      (if device
		  (setq dir (concat "/" device "/" dir)))
	      (concat dir file))
	  (error "path %s didn't match tops-20 syntax" path))
      (if (string-match "^\\(/[^:/]+:/\\)?\\([^./]+/\\)*\\([^/]*\\)$" path)
	  (let ((device (and (match-beginning 1)
			     (substring path 1 (1- (match-end 1)))))
		(dir (and (match-beginning 2)
			  (substring path (match-beginning 2)
				     (1- (match-end 2)))))
		(file (substring path (match-beginning 3)
				 (match-end 3))))
	    (if dir
		(progn
		  (while (string-match "/" dir)
		    (setq dir (concat (substring dir 0 (match-beginning 0))
				      "."
				      (substring dir (match-end 0)))))
		  (if device
		      (concat device "<" dir ">" file)
		    (concat "<" dir ">" file)))
	      (if device
		  (error "%s is invalid relative syntax for tops-20" path)
		file)))
	(error "path %s is invalid syntax for tops-20" path)))))

(efs-defun efs-fix-dir-path tops-20 (dir-path)
  ;; Convert a path from UNIX-ish to Tops-20 fir a dir listing.
  (cond ((string-equal "/" dir-path)
	 (error "Can't list tops-20 devices"))
	((string-match "/[^:/]+:/$" dir-path)
	 (error "Can't list all root directories on a tops-20 device"))
	((efs-fix-path 'tops-20 dir-path nil))))


;; In tops-20 listings, the filename starts immediatley after the date regexp.

(defconst efs-tops-20-date-regexp
  (concat
   " [1-3]?[0-9]-\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\)-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] "))


(efs-defun efs-parse-listing tops-20
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a TOPS-20 directory
  ;; listing, and return a hashtable as the result.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory in as a full remote path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches (not relevant here)
  (let ((tbl (efs-make-hashtable))
	file)
    (goto-char (point-min))
    (efs-save-match-data
      (if (looking-at " *[^/:]+:<\\([^/.<>]+\\.\\)+> *$")
	  ;; looking at the directory name
	  (forward-line 1))
      (while (re-search-forward efs-tops-20-date-regexp nil t)
	(setq file (buffer-substring (point)
				     (progn (end-of-line) (point))))
	(if (string-match "\\.DIRECTORY\\.[0-9]+$" file)
	    ;; deal with directories
	    (efs-put-hash-entry
	     (substring file 0 (match-beginning 0)) '(t) tbl)
	  (efs-put-hash-entry file '(nil) tbl)
	  ;; sans extension
	  (if (string-match "\\.[0-9]+$" file)
	      (efs-put-hash-entry
	       (substring file 0 (match-beginning 0)) '(nil) tbl)))
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl))
    tbl))

(efs-defun efs-really-file-p tops-20 (file ent)
  ;; Eliminates the version entries
  (or (car ent) ; file-directory-p
      (efs-save-match-data
	(string-match "\\.[0-9]+$" file))))

(efs-defun efs-delete-file-entry tops-20 (path &optional dir-p)
  (let ((ignore-case (memq 'tops-20 efs-case-insensitive-host-types)))
    (if dir-p
	(let ((path (file-name-as-directory path))
	      files)
	  (efs-del-hash-entry path efs-files-hashtable ignore-case)
	  (setq path (directory-file-name path)
		files (efs-get-hash-entry (file-name-directory path)
					       efs-files-hashtable
					       ignore-case))
	  (if files
	      (efs-del-hash-entry (efs-get-file-part path)
				       files ignore-case)))
      (efs-save-match-data
	(let ((file (efs-get-file-part path)))
	  (if (string-match "\\.[0-9]+$" file)
	      ;; Only delete explicit versions
	      (let ((files (efs-get-hash-entry
			    (file-name-directory path)
			    efs-files-hashtable ignore-case)))
		(if files
		    (let ((root (substring file 0
					   (match-beginning 0)))
			  (completion-ignore-case ignore-case)
			  (len (match-beginning 0)))
		      (efs-del-hash-entry file files ignore-case)
		      ;; Now we need to check if there are any
		      ;; versions left. If not, then delete the
		      ;; root entry.
		      (or (all-completions
			   root files
			   (function
			    (lambda (sym)
			      (string-match "\\.[0-9]+$"
					    (symbol-name sym) len))))
			  (efs-del-hash-entry root files
						   ignore-case)))))))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-add-file-entry tops-20
  (path dir-p size owner &optional modes nlinks mdtm)
  ;; The tops-20 version of this function needs to keep track
  ;; of tops-20's file versions.
  (let ((ignore-case (memq 'tops-20 efs-case-insensitive-host-types))
	(ent (let ((dir-p (null (null dir-p))))
	       (if mdtm
		   (list dir-p size owner nil nil mdtm)
		 (list dir-p size owner)))))
    (if dir-p
	(let* ((path (directory-file-name path))
	       (files (efs-get-hash-entry  (file-name-directory path)
						efs-files-hashtable
						ignore-case)))
	  (if files
	      (efs-put-hash-entry (efs-get-file-part path)
				       ent files ignore-case)))
      (let ((files (efs-get-hash-entry
		    (file-name-directory path)
		    efs-files-hashtable ignore-case)))
	(if files
	    (let ((file (efs-get-file-part path)))
	      (efs-save-match-data
		(if (string-match "\\.[0-9]+$" file)
		    (efs-put-hash-entry
		     (substring file 0 (match-beginning 0))
		     ent files ignore-case)
		  ;; Need to figure out what version of the file
		  ;; is being added.
		  (let* ((completion-ignore-case ignore-case)
			 (len (length file))
			 (versions (all-completions
				    file files
				    (function
				     (lambda (sym)
				       (string-match "\\.[0-9]+$"
						     (symbol-name sym) len)))))
			 (N (1+ len))
			 (max (apply
			       'max
			       (cons 0 (mapcar
					(function
					 (lambda (x)
					   (string-to-int (substring x N))))
					versions)))))
		    ;; No need to worry about case here.
		    (efs-put-hash-entry
		     (concat file "." (int-to-string (1+ max))) ent files))))
	      (efs-put-hash-entry file ent files ignore-case)))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-internal-file-name-as-directory tops-20 (name)
  (efs-save-match-data
    (if (string-match "\\.DIRECTORY\\(\\.[0-9>]\\)?$" name)
	(setq name (substring name 0 (match-beginning 0))))
    (let (file-name-handler-alist)
      (file-name-as-directory name))))

;;; Tree Dired

(defconst efs-dired-tops-20-re-dir
  "^[^\n]+\\.DIRECTORY\\(\\.[0-9]+\\)?$")

(or (assq 'tops-20 efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'tops-20  efs-dired-tops-20-re-dir)
		efs-dired-re-dir-alist)))


(efs-defun efs-dired-manual-move-to-filename tops-20
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the Tops-20 version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-tops-20-date-regexp eol t)
	(point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename tops-20
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the Tops-20 version.
  (let ((opoint (point)))
    (and selective-display
	 (null no-error)
	 (eq (char-after
	      (1- (or bol (save-excursion
				(skip-chars-backward "^\r\n")
				(point)))))
	     ?\r)
	 ;; File is hidden or omitted.
	 (cond
	  ((dired-subdir-hidden-p (dired-current-directory))
	   (error
	    (substitute-command-keys
	     "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
	  ((error
	    (substitute-command-keys
	     "File line is omitted. Type \\[dired-omit-toggle] to un-omit."
	     )))))
    ;; Is this the right character set?
    (skip-chars-forward "-_A-Z0-9$.;")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r))))
	(if no-error
	      nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-internal--file-name-sans-versions tops-20
  (name &optional keep-backup-version)
  (efs-save-match-data
    (if (string-match "\\.[0-9]+$" name)
	(substring name 0 (match-beginning 0))
      name)))

(efs-defun efs-dired-insert-headerline tops-20 (dir)
  ;; TOPS-20 inserts a headerline. I would prefer the headerline
  ;; to be in efs format. This version tries to
  ;; be careful, because we can't count on a headerline
  ;; over ftp, and we wouldn't want to delete anything
  ;; important.
  (save-excursion
    (if (looking-at "^  wildcard ")
	(forward-line 1))
    (if (looking-at "^[ \n\t]*[^:/<>]+:<[^<>/]+> *\n")
	(delete-region (point) (match-end 0)))
    (insert "  " (directory-file-name dir) ":\n\n")))

;;; end of efs-tops-20.el
