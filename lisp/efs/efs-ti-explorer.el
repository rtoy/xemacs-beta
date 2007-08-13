;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-ti-explorer.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Explorer support for efs
;; Author:       Jamie Zawinski <jwz@lucid.com>
;; Created:      Thu Dec 17 15:04:14 1992
;; Modified:     Sun Nov 27 18:42:47 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-ti-explorer)
(require 'efs)

(defconst efs-ti-explorer-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; Explorer support.
;;;; ------------------------------------------------------------

;;; efs support for TI Explorer Lisp Machines.
;;; Symbolics Lispms use a different syntax, but I think that the 
;;; MIT and LMI Lispms use the same syntax as Explorers.

(defconst efs-ti-explorer-filename-regexp
  (let* ((excluded-chars ":;<>.#\n\r\ta-z")
	 (token  (concat "[^" excluded-chars "]+"))
	 (token* (concat "[^" excluded-chars "]*")))
    (concat "\\(" token ": *" "\\)?"			; optional device
	    "\\([^ " excluded-chars "]" token* "\\)"
	    "\\(\\." token "\\)*; *"			; directory
	    "\\(" token* "." token* "\\|\\) *"		; name and extension
	    "# *-?\\([0-9]+\\|>\\)")))			; version

(efs-defun efs-quote-string ti-explorer (string &optional not-space)
  ;; ## This is an EVIL hack.  Backslash is not what Explorers use to
  ;; quote magic characters, and in addition, it is *incorrect* to quote
  ;; spaces between the directory and filename: they are not a part of
  ;; the filename, they are ignored.  Quoting them would make them be
  ;; significant.
  (if not-space
      string
    (concat "\"" string "\"")))

(efs-defun efs-send-pwd ti-explorer (host user &optional xpwd)
;; TI-EXPLORER output from pwd's needs to be specially parsed because
;; the fullpath syntax contains spaces.
  (let* ((result (efs-send-cmd host user '(pwd) "Getting EXPLORER PWD"))
	 (line (nth 1 result))
	 dir)
    (if (car result)
	(efs-save-match-data
	  (and (string-match "^257 " line)
	       (setq dir (substring line 4)))))
    (cons dir line)))

(efs-defun efs-fix-path ti-explorer (path &optional reverse)
  ;; Convert PATH from UNIX-ish to Explorer.  If REVERSE given then convert
  ;; from Explorer to UNIX-ish.
  (efs-save-match-data
    (if reverse
	(if (string-match
	     "^\\([^:]+:\\)? *\\([^:]+:\\)? *\\([^;]*\\); *\\(.*\\)$"
	     path)
	    (let (dir file)
	      ;; I don't understand how "devices" work, so I'm ignoring them.
	      ;; (if (match-beginning 2)
	      ;;     (setq device (substring path
	      ;;                             (match-beginning 2)
	      ;;                             (1- (match-end 2)))))
	      (if (match-beginning 3)
		  (setq dir
			(substring path (match-beginning 3) (match-end 3))))
	      (if (match-beginning 4)
		  (setq file
			(substring path (match-beginning 4) (match-end 4))))
	      (cond (dir
		     (setq dir (apply (function concat)
				      (mapcar (function
					       (lambda (char)
						 (if (= char ?.)
						     (vector ?/)
						   (vector char))))
					      dir)))
		     (if (string-match "^/" dir)
			 (setq dir (substring dir 1))
		       (setq dir (concat "/" dir)))))
	      (concat
	       ;; (and device ":") device (and device ":")
	       dir (and dir "/")
	       file))
	  (error "path %s didn't match explorer syntax" path))
      (let (dir file tmp)
	;; (if (string-match "^/[^:]+:" path)
	;;      (setq device (substring path 1
	;;		                (1- (match-end 0)))
	;;	      path (substring path (match-end 0))))
	(cond ((setq tmp (file-name-directory path))
	       (setq dir (apply (function concat)
				(mapcar (function
					 (lambda (char)
					   (if (= char ?/)
					       (vector ?.)
					     (vector char))))
					(substring tmp 0 -1))))
	       (if (string-match "^[.]" dir)
		   (setq dir (substring dir 1))
		 (error "explorer pathnames can't be relative")
		 (setq dir (concat "." dir)))))
	(setq file (file-name-nondirectory path))
	(concat
	 ;; (and device ":") device (and device ":")
	 dir
	 (and dir  ";")
	 file)))))

;; (efs-fix-path-for-explorer "/PUBLIC/ZMACS/ZYMURG.LISP#1")
;; (efs-fix-path-for-explorer "PUBLIC.ZMACS;ZYMURG.LISP#1" t)

(efs-defun efs-fix-dir-path ti-explorer (dir-path)
  ;; Convert path from UNIX-ish to Explorer ready for a DIRectory listing.
  (cond ((string-equal dir-path "/")
	 (efs-fix-path 'ti-explorer "/~/" nil))
	((string-match "^/[-A-Z0-9_$]+:/" dir-path)
	 (error "Don't grok Explorer \"devices\" yet."))
	((efs-fix-path 'ti-explorer dir-path nil))))

(defmacro efs-parse-ti-explorer-filename ()
  ;; Extract the next filename from an Explorer dired-like listing.
  (` (if (re-search-forward
	  efs-ti-explorer-filename-regexp
	  nil t)
	 (buffer-substring (match-beginning 0) (match-end 0)))))

(efs-defun efs-parse-listing ti-explorer
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be an Explorer directory
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
      (while (setq file (efs-parse-ti-explorer-filename))
	;; Explorer/Twenex listings might come out in absolute form.
	(if (string-match "^[^;]*; *" file)
	    (setq file (substring file (match-end 0))))
	(if (string-match "\\.\\(DIRECTORY\\|directory\\)#[0-9]+$" file)
	    ;; deal with directories
	    (efs-put-hash-entry
	     (substring file 0 (match-beginning 0)) '(t) tbl)
	  (efs-put-hash-entry file '(nil) tbl)
	  (if (string-match "#[0-9]+$" file) ; deal with extension
	      ;; sans extension
	      (efs-put-hash-entry
	       (substring file 0 (match-beginning 0)) '(nil) tbl)))
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl))
    tbl))

(efs-defun efs-really-file-p ti-explorer (file ent)
  ;; Eliminates the version entries
  (or (car ent) ; file-directory-p
      (efs-save-match-data
	(string-match "#[0-9]+$" file))))

(efs-defun efs-delete-file-entry ti-explorer (path &optional dir-p)
  (let ((ignore-case (memq 'ti-explorer efs-case-insensitive-host-types)))
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
	  (if (string-match "#[0-9]+$" file)
	      ;; Only delete entries with explicit version numbers.
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
			      (string-match "#[0-9]+$"
					    (symbol-name sym) len))))
			  (efs-del-hash-entry root files
						   ignore-case)))))))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-add-file-entry ti-explorer
  (path dir-p size owner &optional modes nlinks mdtm)
  ;; The ti-explorer version of this function needs to keep track
  ;; of file versions.
  (let ((ignore-case (memq 'ti-explorer efs-case-insensitive-host-types))
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
		(if (string-match "#[0-9]+$" file)
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
				       (string-match "#[0-9]+$"
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
		     (concat file "#" (int-to-string (1+ max))) ent files))))
	      (efs-put-hash-entry file ent files ignore-case)))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-internal-file-name-as-directory ti-explorer (name)
  (efs-save-match-data
    (if (string-match "\\.\\(DIRECTORY\\|directory\\)\\(#[0-9>]\\)?$" name)
	(setq name (substring name 0 (match-beginning 0))))
    (let (file-name-handler-alist)
      (file-name-as-directory name))))

(efs-defun efs-allow-child-lookup ti-explorer (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.

  ;; Subdirs in EXPLORER can't have an extension (other than .DIRECTORY,
  ;; which we have truncated).
  (not (string-match "\\." file)))

;;; Tree Dired

(defconst efs-dired-ti-explorer-re-dir
  "^. *[^;\n\r]+;[^;\n\r.]+\\.\\(DIRECTORY\\|directory\\) *#"
  "Regular expression to use to search for Explorer directories.")

(or (assq 'ti-explorer efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'ti-explorer  efs-dired-ti-explorer-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename ti-explorer
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the Explorer version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-ti-explorer-filename-regexp eol t)
	(progn
	  (goto-char (match-beginning 0))
	  ;; Explorer listings might come out in absolute form.
	  (if (looking-at "[^;]*; *")
	      (goto-char (match-end 0))
	    (point)))
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename ti-explorer
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the Explorer version.
  (let (case-fold-search)
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
    (if (looking-at efs-ti-explorer-filename-regexp)
	(goto-char (match-end 0))
      (if no-error
	  nil
	(error "No file on this line")))))

(efs-defun efs-dired-ls-trim ti-explorer ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward efs-ti-explorer-filename-regexp))
  (beginning-of-line)
  (delete-region (point-min) (point))
  (forward-line 1)
  (delete-region (point) (point-max)))

(efs-defun efs-internal-file-name-sans-versions ti-explorer
  (name &optional keep-backup-version)
  (efs-save-match-data
    (if (string-match "#\\([0-9]+\\|>\\)$" name)
	(substring name 0 (match-beginning 0))
      name)))

;;; ### still need to ape these from vms:
;;;  efs-dired-vms-clean-directory
;;;  efs-dired-vms-collect-file-versions
;;;  efs-dired-vms-trample-file-versions
;;;  efs-dired-vms-flag-backup-files
;;;  efs-dired-vms-backup-diff

;;; end of efs-ti-explorer.el
