;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-vms.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.13 $
;; RCS:          
;; Description:  VMS support for efs
;; Authors:      Andy Norman, Joe Wells, Sandy Rutherford <sandy@itp.ethz.ch>
;; Modified:     Sun Nov 27 18:44:59 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-vms)
(require 'efs)

(defconst efs-vms-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.13 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; VMS support.
;;;; ------------------------------------------------------------

;;; efs has full support for VMS hosts, including tree dired support.  It
;;; should be able to automatically recognize any VMS machine. However, if it
;;; fails to do this, you can use the command efs-add-vms-host.  As well,
;;; you can set the variable efs-vms-host-regexp in your .emacs file. We
;;; would be grateful if you would report any failures to automatically
;;; recognize a VMS host as a bug.
;;;
;;; Filename Syntax:
;;;
;;; For ease of *implementation*, the user enters the VMS filename syntax in a
;;; UNIX-y way.  For example:
;;;  PUB$:[ANONYMOUS.SDSCPUB.NEXT]README.TXT;1
;;; would be entered as:
;;;  /PUB$$:/ANONYMOUS/SDSCPUB/NEXT/README.TXT;1
;;; i.e. to log in as anonymous on ymir.claremont.edu and grab the file:
;;;  [.CSV.POLICY]RULES.MEM
;;; you would type:
;;;  C-x C-f /anonymous@ymir.claremont.edu:CSV/POLICY/RULES.MEM
;;;
;;; A legal VMS filename is of the form: FILE.TYPE;##
;;; where FILE can be up to 39 characters
;;;       TYPE can be up to 39 characters
;;;       ## is a version number (an integer between 1 and 32,767)
;;; Valid characters in FILE and TYPE are A-Z 0-9 _ - $
;;; $ cannot begin a filename, and - cannot be used as the first or last
;;; character.
;;;
;;; Tips:
;;; 1. To access the latest version of file under VMS, you use the filename
;;;    without the ";" and version number. You should always edit the latest
;;;    version of a file. If you want to edit an earlier version, copy it to a
;;;    new file first. This has nothing to do with efs, but is simply
;;;    good VMS operating practice. Therefore, to edit FILE.TXT;3 (say 3 is
;;;    latest version), do C-x C-f /ymir.claremont.edu:FILE.TXT. If you
;;;    inadvertently do C-x C-f /ymir.claremont.edu:FILE.TXT;3, you will find
;;;    that VMS will not allow you to save the file because it will refuse to
;;;    overwrite FILE.TXT;3, but instead will want to create FILE.TXT;4, and
;;;    attach the buffer to this file. To get out of this situation, M-x
;;;    write-file /ymir.claremont.edu:FILE.TXT will attach the buffer to
;;;    latest version of the file. For this reason, in tree dired "f"
;;;    (dired-find-file), always loads the file sans version, whereas "v",
;;;    (dired-view-file), always loads the explicit version number. The
;;;    reasoning being that it reasonable to view old versions of a file, but
;;;    not to edit them.
;;; 2. EMACS has a feature in which it does environment variable substitution
;;;    in filenames. Therefore, to enter a $ in a filename, you must quote it
;;;    by typing $$. There is a bug in EMACS, in that it neglects to quote the
;;;    $'s in the default directory when it writes it in the minibuffer.  You
;;;    must edit the minibuffer to quote the $'s manually. Hopefully, this bug
;;;    will be fixed in EMACS 19. If you use Sebastian Kremer's gmhist (V 4.26
;;;    or newer), you will not have this problem.


;; Because some VMS ftp servers convert filenames to lower case
;; we allow a-z in the filename regexp.

(defconst efs-vms-filename-regexp
  "\\([_A-Za-z0-9$][-_A-Za-z0-9$]*\\)?\\.\\([-_A-Za-z0-9$]*\\);[0-9]+")
;; Regular expression to match for a valid VMS file name in Dired buffer.

(defvar efs-vms-month-alist
  '(("JAN" . 1) ("FEB". 2) ("MAR" . 3) ("APR" . 4) ("MAY" . 5) ("JUN" . 6)
    ("JUL" . 7) ("AUG" . 8) ("SEP" . 9) ("OCT" . 10)
    ("NOV" . 11) ("DEC" . 12)))

(defvar efs-vms-date-regexp
  (concat
   "\\([0-3]?[0-9]\\)-"
   "\\(JAN\\|FEB\\|MAR\\|APR\\|MAY\\|JUN\\|"
   "JUL\\|AUG\\|SEP\\|OCT\\|NOV\\|DEC\\)-"
   "\\([0-9][0-9][0-9]?[0-9]?\\) \\(\\([0-5][0-9]\\):\\([0-5][0-9]\\)"
   "\\(:[0-5][0-9]\\)?\\)? "))


;;; The following two functions are entry points to this file.
;;; They are defined as efs-autoloads in efs.el

(efs-defun efs-fix-path vms (path &optional reverse)
  ;; Convert PATH from UNIX-ish to VMS.
  ;; If REVERSE given then convert from VMS to UNIX-ish.
  (efs-save-match-data
    (if reverse
	(if (string-match
	     "^\\([^:]+:\\)?\\(\\[[^]]+\\]\\)?\\([^][]*\\)$" path)
	    (let (drive dir file)
	      (if (match-beginning 1)
		  (setq drive (substring path
					 (match-beginning 1)
					 (match-end 1))))
	      (if (match-beginning 2)
		  (setq dir
			(substring path (match-beginning 2) (match-end 2))))
	      (if (match-beginning 3)
		  (setq file
			(substring path (match-beginning 3) (match-end 3))))
	      (and dir
		   (setq dir (apply (function concat)
				    (mapcar (function
					     (lambda (char)
					       (if (= char ?.)
						   (vector ?/)
						 (vector char))))
					    (substring dir 1 -1)))))
	      (concat (and drive
			   (concat "/" drive "/"))
		      dir (and dir "/")
		      file))
	  (error "path %s didn't match" path))
      (let (drive dir file)
	(if (string-match "^/[^:/]+:/" path)
	    (setq drive (substring path 1 (1- (match-end 0)))
		  path (substring path (1- (match-end 0)))))
	(setq dir (file-name-directory path)
	      file (efs-internal-file-name-nondirectory path))
	(if dir
	    (let ((len (1- (length dir)))
		  (n 0))
	      (if (<= len 0)
		  (setq dir nil)
		(while (<= n len)
		  (and (char-equal (aref dir n) ?/)
		       (cond
			((zerop n) (aset dir n ?\[))
			((= n len) (aset dir n ?\]))
			(t (aset dir n ?.))))
		  (setq n (1+ n))))))
	(concat drive dir file)))))

;; It is important that this function barf for directories for which we know
;; that we cannot possibly get a directory listing, such as "/" and "/DEV:/".
;; This is because it saves an unnecessary FTP error, or possibly the listing
;; might succeed, but give erroneous info. This last case is particularly
;; likely for OS's (like MTS) for which we need to use a wildcard in order
;; to list a directory.

(efs-defun efs-fix-dir-path vms (dir-path)
  ;; Convert path from UNIX-ish to VMS ready for a DIRectory listing.
  ;; Should there be entries for .. -> [-] and . -> [] below. Don't
  ;; think so, because expand-filename should have already short-circuited
  ;; them.
  (cond ((string-equal dir-path "/")
	 (error "Cannot get listing for fictitious \"/\" directory."))
	((string-match "^/[-A-Z0-9_$]+:/$" dir-path)
	 (error "Cannot get listing for device."))
	((efs-fix-path 'vms dir-path))))
  
;; These parsing functions are as general as possible because the syntax
;; of ftp listings from VMS hosts is a bit erratic. What saves us is that
;; the VMS filename syntax is so rigid. If they bomb on a listing in the
;; standard VMS Multinet format, then this is a bug. If they bomb on a listing
;; from vms.weird.net, then too bad.

(defmacro efs-parse-vms-filename ()
  "Extract the next filename from a VMS dired-like listing."
  (` (if (re-search-forward
	  efs-vms-filename-regexp
	  nil t)
	 (buffer-substring (match-beginning 0) (match-end 0)))))

(defun efs-parse-vms-listing ()
  ;; Parse the current buffer which is assumed to be a VMS DIR
  ;; listing (either a short (NLIST) or long listing).
  ;; Assumes that point is at the beginning of the buffer.
  (let ((tbl (efs-make-hashtable))
	file)
    (goto-char (point-min))
    (efs-save-match-data
      (while (setq file (efs-parse-vms-filename))
	(if (string-match "\\.\\(DIR\\|dir\\);[0-9]+" file)
	    ;; deal with directories
	    (efs-put-hash-entry
	     (substring file 0 (match-beginning 0)) '(t) tbl)
	  (efs-put-hash-entry file '(nil) tbl)
	  (if (string-match ";[0-9]+$" file) ; deal with extension
	      ;; sans extension
	      (efs-put-hash-entry
	       (substring file 0 (match-beginning 0)) '(nil) tbl)))
	(forward-line 1))
      ;; Would like to look for a "Total" line, or a "Directory" line to
      ;; make sure that the listing isn't complete garbage before putting
      ;; in "." and "..", but we can't even count on all VAX's giving us
      ;; either of these.
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl))
    tbl))

(efs-defun efs-parse-listing vms
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a VMS FTP dir
  ;; format, and return a hashtable as the result. SWITCHES are never used,
  ;; but they must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory in as a full remote path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches (not relevant here)
  (goto-char (point-min))
  (efs-save-match-data
    ;; check for a DIR/FULL monstrosity
    (if (search-forward "\nSize:" nil t)
	(progn
	  (efs-add-listing-type 'vms:full host user)
	  ;; This will cause the buffer to be refilled with an NLIST
	  (let ((efs-ls-uncache t))
	    (efs-ls path nil (format "Relisting %s"
				     (efs-relativize-filename path))
		    t))
	  (goto-char (point-min))
	  (efs-parse-vms-listing))
      (efs-parse-vms-listing))))


;;;; Sorting of listings

(efs-defun efs-t-converter vms (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-vms-filename-regexp nil t)
	  (let (list-start start end list)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (and (looking-at efs-vms-filename-regexp)
			(progn
			  (setq start (point))
			  (goto-char (match-end 0))
			  (forward-line (if (eolp) 2 1))
			  (setq end (point))
			  (goto-char (match-end 0))
			  (re-search-forward efs-vms-date-regexp nil t)))
	      (setq list
		    (cons
		     (cons
		      (nconc
		       (list (string-to-int (buffer-substring
					     (match-beginning 3)
					     (match-end 3))) ; year
			     (cdr (assoc
				   (buffer-substring (match-beginning 2)
						     (match-end 2))
				   efs-vms-month-alist)) ; month
			     (string-to-int (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))) ;day
		       (if (match-beginning 4)
			   (list
			    (string-to-int (buffer-substring
					    (match-beginning 5)
					    (match-end 5))) ; hour
			    (string-to-int (buffer-substring
					    (match-beginning 6)
					    (match-end 6))) ; minute
			    (if (match-beginning 7)
				(string-to-int (buffer-substring
						(1+ (match-beginning 7))
						(match-end 7))) ; seconds
			      0))
			 (list 0 0 0)))
		      (buffer-substring start end))
		     list))
	      (goto-char end))
	    (if list
		(progn
		  (setq list
			(mapcar 'cdr
				(sort list 'efs-vms-t-converter-sort-pred)))
		  (if reverse (setq list (nreverse list)))
		  (delete-region list-start (point))
		  (apply 'insert list)))
	    t)))))

(defun efs-vms-t-converter-sort-pred (elt1 elt2)
  (let* ((data1 (car elt1))
	 (data2 (car elt2))
	 (year1 (car data1))
	 (year2 (car data2))
	 (month1 (nth 1 data1))
	 (month2 (nth 1 data2))
	 (day1 (nth 2 data1))
	 (day2 (nth 2 data2))
	 (hour1 (nth 3 data1))
	 (hour2 (nth 3 data2))
	 (minute1 (nth 4 data1))
	 (minute2 (nth 4 data2)))
    (or (> year1 year2)
	(and (= year1 year2)
	     (or (> month1 month2)
		 (and (= month1 month2)
		      (or (> day1 day2)
			  (and (= day1 day2)
			       (or (> hour1 hour2)
				   (and (= hour1 hour2)
					(or (> minute1 minute2)
					    (and (= minute1 minute2)
						 (or (> (nth 5 data1)
							(nth 5 data2)))
						 ))))))))))))


(efs-defun efs-X-converter vms (&optional regexp reverse)
  ;; Sorts by extension
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-vms-filename-regexp nil t)
	  (let (list-start start list)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (looking-at efs-vms-filename-regexp)
	      (setq start (point))
	      (goto-char (match-end 0))
	      (forward-line (if (eolp) 2 1))
	      (setq list
		    (cons
		     (cons (buffer-substring (match-beginning 2)
					     (match-end 2))
			   (buffer-substring start (point)))
		     list)))
	    (setq list
		  (mapcar 'cdr
			  (sort list
				(if reverse
				    (function
				     (lambda (x y)
					(string< (car y) (car x))))
				  (function
				   (lambda (x y)
				     (string< (car x) (car y))))))))
	    (delete-region list-start (point))
	    (apply 'insert list)
	    t)))))

;; This version only deletes file entries which have
;; explicit version numbers, because that is all VMS allows.

(efs-defun efs-delete-file-entry vms (path &optional dir-p)
  (let ((ignore-case (memq 'vms efs-case-insensitive-host-types)))
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
	  (if (string-match ";[0-9]+$" file)
	      ;; In VMS you can't delete a file without an explicit	
	      ;; version number, or wild-card (e.g. FOO;*)
	      ;; For now, we give up on wildcards.
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
			      (string-match ";[0-9]+$"
					    (symbol-name sym) len))))
			  (efs-del-hash-entry root files
						   ignore-case)))))))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-add-file-entry vms (path dir-p size owner
						  &optional modes nlinks mdtm)
  ;; The vms version of this function needs to keep track
  ;; of vms's file versions.
  (let ((ignore-case (memq 'vms efs-case-insensitive-host-types))
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
		;; In VMS files must have an extension. If there isn't
		;; one, it will be added.
		(or (string-match "^[^;]*\\." file)
		    (if (string-match ";" file)
			(setq file (concat
				    (substring file 0 (match-beginning 0))
				    ".;"
				    (substring file (match-end 0))))
		      (setq file (concat file "."))))
		(if (string-match ";[0-9]+$" file)
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
				       (string-match ";[0-9]+$"
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
		     (concat file ";" (int-to-string (1+ max))) ent files))))
	      (efs-put-hash-entry file ent files ignore-case)))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-really-file-p vms (file ent)
  ;; Returns whether the hash entry FILE with entry ENT is a real file.
  (or (car ent) ; file-directory-p
      (efs-save-match-data
	(string-match ";" file))))

(efs-defun efs-internal-file-name-as-directory vms (name)
  (efs-save-match-data
    (if (string-match "\\.\\(DIR\\|dir\\)\\(;[0-9]+\\)?$" name)
	(setq name (substring name 0 (match-beginning 0))))
    (let (file-name-handler-alist)
      (file-name-as-directory name))))

(efs-defun efs-remote-directory-file-name vms (dir)
  ;; Returns the VMS filename in unix directory syntax for directory DIR.
  ;; This is something like /FM/SANDY/FOOBAR.DIR;1
  (efs-save-match-data
    (setq dir (directory-file-name dir))
    (concat dir
	    (if (string-match "[a-z]" (nth 2 (efs-ftp-path dir)))
		".dir;1"
	      ".DIR;1"))))

(efs-defun efs-allow-child-lookup vms (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.

  ;; Subdirs in VMS can't have an extension (other than .DIR, which we
  ;; have truncated).
  (not (or (string-match "\\." file)
	   (and (boundp 'dired-local-variables-file)
		(stringp dired-local-variables-file)
		(string-equal dired-local-variables-file file)))))

;;; Tree dired support:

;; For this code I have borrowed liberally from Sebastian Kremer's
;; dired-vms.el


;; These regexps must be anchored to beginning of line.
;; Beware that the ftpd may put the device in front of the filename.

(defconst efs-dired-vms-re-exe
  "^. [^ \t.]+\\.\\(EXE\\|exe\\)[; ]")

(or (assq 'vms efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'vms  efs-dired-vms-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-vms-re-dir
  "^. [^ \t.]+\\.\\(DIR\\|dir\\)[; ]")

(or (assq 'vms efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'vms  efs-dired-vms-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline vms (dir)
  ;; VMS inserts a headerline. I would prefer the headerline
  ;; to be in efs format. This version tries to
  ;; be careful, because we can't count on a headerline
  ;; over ftp, and we wouldn't want to delete anything
  ;; important.
  (save-excursion
    (if (looking-at "^  \\(list \\)?wildcard ")
	(forward-line 1))
    ;; This is really aggressive. Too aggressive?
    (let ((start (point)))
      (skip-chars-forward " \t\n")
      (if (looking-at efs-vms-filename-regexp)
	  (beginning-of-line)
	(forward-line 1)
	(skip-chars-forward " \t\n")
	(beginning-of-line))
      (delete-region start (point)))
    (insert " \n"))
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-fixup-listing vms (file path &optional switches wildcard)
  ;; Some vms machines list the entire path. Scrape this off.
  (setq path (efs-fix-path
	      'vms
	      ;; Need the file-name-directory, in case of widcards.
	      ;; Note that path is a `local' path rel. the remote host.
	      ;; Lose on wildcards in parent dirs. Fix if somebody complains.
	      (let (file-name-handler-alist)
		(file-name-directory path))))
  ;; Some machines put a Node name down too.
  (let ((regexp (concat "^\\([_A-Za-z0-9][-_A-Za-z0-9]*\\$\\)?"
			(regexp-quote path))))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (delete-region (match-beginning 0) (match-end 0))))
  ;; Now need to deal with continuation lines.
  (goto-char (point-min))
  (let (col start end)
    (while (re-search-forward
	    ";[0-9]+[ \t]*\\(\n[ \t]+\\)[^; \t\n]+[^\n;]*\n" nil t)
      (setq start (match-beginning 1)
	    end (match-end 1))
	;; guess at the column dimensions
      (or col
	  (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward
		 (concat efs-vms-filename-regexp
			 "[ \t]+[^ \t\n\r]") nil t)
		(setq col (- (goto-char (match-end 0))
			     (progn (beginning-of-line) (point))
			     1))
	      (setq col 0))))
      ;; join cont. lines.
      (delete-region start end)
      (goto-char start)
      (insert-char ?   (max (- col (current-column)) 2))))
  ;; Some vms dir listings put a triple null line before the total line.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (if (search-forward "\n\n\n" nil t)
      (delete-char -1)))

(efs-defun efs-dired-manual-move-to-filename vms
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the VMS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-vms-filename-regexp eol t)
	(goto-char (match-beginning 0))
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename vms
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the VMS version.
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
    (skip-chars-forward "-_A-Za-z0-9$.;")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\  ?\t ?\n ?\r))))
	(if no-error
	    nil
	    (error "No file on this line"))
      (point))))

(efs-defun efs-dired-ls-trim vms ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward  efs-vms-filename-regexp))
  (beginning-of-line)
  (delete-region (point-min) (point))
  (forward-line 1)
  (delete-region (point) (point-max)))

(efs-defun efs-internal-file-name-sans-versions vms
  (name &optional keep-backup-version)
  (efs-save-match-data
    (if (string-match ";[0-9]+$" name)
	(substring name 0 (match-beginning 0))
      name)))

(efs-defun efs-dired-collect-file-versions vms ()
  ;; If it looks like file FN has versions, return a list of the versions.
  ;; That is a list of strings which are file names.
  ;; The caller may want to flag some of these files for deletion.
  (let ((completion-ignore-case (memq 'vms efs-case-insensitive-host-types))
	result)
    (dired-map-dired-file-lines
     (function
      (lambda (fn)
	(if (string-match ";[0-9]+$" fn)
	    (let* ((base-fn (substring fn 0 (match-beginning 0)))
		   (base-version (file-name-nondirectory
				  (substring fn 0 (1+ (match-beginning 0)))))
		   (bv-length (length base-version))
		   (possibilities (and
				   (null (assoc base-fn result))
				   (file-name-all-completions
				    base-version
				    (file-name-directory fn)))))
	      (if possibilities
		  (setq result
			(cons (cons base-fn
				    ;; code this explicitly
				    ;; using backup-extract-version has a
				    ;; lot of function-call overhead.
				    (mapcar (function
					     (lambda (fn)
					       (string-to-int
						(substring fn bv-length))))
					    possibilities)) result))))))))
    result))

(efs-defun efs-dired-flag-backup-files vms (&optional unflag-p)
  (interactive "P")
  (let ((dired-kept-versions 1)
	(kept-old-versions 0)
	marker msg)
    (if unflag-p
	(setq marker ?\040 msg "Unflagging old versions")
      (setq marker dired-del-marker msg "Purging old versions"))
    (dired-clean-directory 1 marker msg)))

(efs-defun efs-internal-diff-latest-backup-file vms (fn)
  ;; For FILE;#, returns the filename FILE;N, where N
  ;; is the largest number less than #, for which this file exists.
  ;; Returns nil if none found.
  (efs-save-match-data
    (and (string-match ";[0-9]+$" fn)
	 (let ((base (substring fn 0 (1+ (match-beginning 0))))
	       (num (1- (string-to-int (substring fn
						  (1+ (match-beginning 0))))))
	       found file)
	   (while (and (setq found (> num 0))
		       (not (file-exists-p
			     (setq file
				   (concat base (int-to-string num))))))
	     (setq num (1- num)))
	   (and found file)))))

;;;;--------------------------------------------------------------
;;;; Support for VMS DIR/FULL listings. (listing type vms:full)
;;;;--------------------------------------------------------------

(efs-defun efs-parse-listing vms:full
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a VMS FTP dir
  ;; format, and return a hashtable as the result. SWITCHES are never used,
  ;; but they must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory in as a full remote path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches (not relevant here)
  (goto-char (point-min))
  (efs-save-match-data
    (efs-parse-vms-listing)))

;;; Tree Dired

(or (assq 'vms:full efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'vms:full efs-dired-vms-re-exe)
		efs-dired-re-exe-alist)))

(or (assq 'vms:full efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'vms:full efs-dired-vms-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline vms:full (dir)
  ;; Insert a blank line for aesthetics.
  (insert " \n")
  (forward-char -2)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename vms:full
  (&optional raise-error bol eol)
  (let ((efs-dired-listing-type 'vms))
    (efs-dired-manual-move-to-filename raise-error bol eol)))

(efs-defun efs-dired-manual-move-to-end-of-filename vms:full
  (&optional no-error bol eol)
  (let ((efs-dired-listing-type 'vms))
    (efs-dired-manual-move-to-end-of-filename no-error bol eol)))

;;; end of efs-vms.el
