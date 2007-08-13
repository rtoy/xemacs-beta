;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-mvs.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.4 $
;; RCS:          
;; Description:  MVS support for efs
;; Author:       Sandy Rutherford <sandy@math.ubc.ca, sandy@itp.ethz.ch>
;; Created:      Sat Nov 14 02:04:54 1992
;; Modified:     Sun Nov 27 18:37:54 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; --------------------------------------------------------
;;; MVS support
;;; --------------------------------------------------------

(provide 'efs-mvs)
(require 'efs)

(defconst efs-mvs-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.4 $" 11 -2)))

;; What's the MVS character set for valid partitioned data sets?
;; I'll guess [-A-Z0-9_$+]

;; The top level directory in MVS contains partitioned data sets.
;; We will view these as directories. The data sets within each
;; partitioned data set will be viewed as files.
;;
;; In MVS an entry for a "sub-dir" may have the same name as a plain
;; file.  This is impossible in unix, so we retain the "dots" at the
;; end of subdir names, to distinuguish.
;; i.e. FOO.BAR --> /FOO./BAR

(efs-defun efs-send-pwd mvs (host user &optional xpwd)
  ;; Broken quoting for PWD output on some MVS servers.
  (let* ((result (efs-send-cmd host user '(pwd) "Getting EXPLORER PWD"))
	 (line (nth 1 result))
	 dir)
    (and (car result)
	 (efs-save-match-data
	   (and (string-match " \"'?\\([0-9A-Z]+\\)'?\"" line)
		(setq dir (substring line (match-beginning 1)
				     (match-end 1))))))
    (cons dir line)))
 
(efs-defun efs-fix-path mvs (path &optional reverse)
  ;; Convert PATH from UNIX-ish to MVS.
  (efs-save-match-data
    (if reverse
	(let ((start 0)
	      (res "/"))
	  ;; MVS has only files, some of which are partitioned
	  ;; into smaller files (partitioned data sets). We will
	  ;; assume that path starts with a partitioned dataset.
	  (while (string-match "\\." path)
	    ;; grab the dot too, because in mvs prefixes and plain
	    ;; files can have the same name.
	    (setq res (concat res (substring path start (match-end 0)) "/")
		  start (match-end 0)))
	  (concat res (substring path start)))
      (let ((start 1)
	    res)
	(while (string-match "/" path start)
	  (setq res (concat res (substring path start (match-beginning 0)))
		start (match-end 0)))
	(concat res (substring path start))))))
		
(efs-defun efs-fix-dir-path mvs (dir-path)
  ;; Convert path from UNIX-ish to MVS for a DIR listing.
  (cond
   ((string-equal "/" dir-path)
   " ")
   (t (concat (efs-fix-path 'mvs dir-path) "*"))))

(efs-defun efs-allow-child-lookup mvs (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  ;; MVS file system is flat. Only partitioned data sets are "subdirs".
  (efs-save-match-data
    (string-match "\\.$" file)))

(efs-defun efs-parse-listing mvs (host user dir path &optional switches)
  ;; Guesses the type of mvs listings.
  (efs-save-match-data
    (goto-char (point-min))
    (cond
     ((looking-at "Volume ")
      (efs-add-listing-type 'mvs:tcp  host user)
      (efs-parse-listing 'mvs:tcp host user dir path switches))

     ((looking-at "[-A-Z0-9_$.+]+ ")
      (efs-add-listing-type 'mvs:nih host user)
      (efs-parse-listing 'mvs:nih host user dir path switches))
     
     (t
      ;; Since MVS works on a template system, return an empty hashtable.
      (let ((tbl (efs-make-hashtable)))
	(efs-put-hash-entry "." '(t) tbl)
	(efs-put-hash-entry ".." '(t) tbl)
	tbl)))))

(efs-defun efs-ls-dumb-check mvs (line host file path lsargs msg noparse
				       noerror nowait cont)
  ;; Because of the template structure of the MVS file system, empty
  ;; directories are the same as non-existent.  It's better for us to treat
  ;; them as empty.
  (and (string-match "^550 " line)
       (let ((parse (or (null noparse) (eq noparse 'parse)
			(efs-parsable-switches-p lsargs t))))
	 (efs-add-to-ls-cache file lsargs "\n" parse)
	 (if parse
	     (efs-set-files file (let ((tbl (efs-make-hashtable)))
				   (efs-put-hash-entry "." '(t) tbl)
				   (efs-put-hash-entry ".." '(t) tbl)
				   tbl)))
	 (if nowait
	     (progn
	       (if cont
		   (efs-call-cont cont "\n"))
	       t)
	   (if cont
	       (efs-call-cont cont "\n"))
	   "\n"))))

;;;; ----------------------------------------------------
;;;; Support for the NIH FTP server.
;;;; ----------------------------------------------------

(efs-defun efs-parse-listing mvs:nih
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be an MVS listing
  ;; Based on the listing format of the NIH server. Hope that this format
  ;; is widespread. If a directory doesn't exist, get a 426 ftp error.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory as a remote full path
  ;; PATH = directory in full efs-syntax
  (let ((tbl (efs-make-hashtable))
	(top-p (string-equal "/" dir))
	;; assume that everything top-level is a partitioned data set
	)
    (goto-char (point-min))
    (efs-save-match-data
      (while (re-search-forward "^[-A-Z0-9_$.+]+" nil t)
	(efs-put-hash-entry
	 (concat (buffer-substring (match-beginning 0) (match-end 0))
		 (and top-p "."))
	 (list top-p) tbl)
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (or top-p (efs-put-hash-entry ".." '(t) tbl)))
    tbl))

;;; Tree dired support

(defconst efs-dired-mvs-re-exe
  "^. [-A-Z0-9_$+]+\\.EXE "
  "Regular expression to use to search for MVS executables.")

(or (assq  'mvs:nih efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'mvs:nih efs-dired-mvs-re-exe)
		efs-dired-re-exe-alist)))

(efs-defun efs-dired-insert-headerline mvs:nih (dir)
  ;; MVS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename mvs:nih
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of the filename on this line.
  ;; This is the MVS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    ;; MVS listings are pretty loose. Tough to tell when we've got a file line.
    (if (and
	 (> (- eol bol) 2)
	 (progn
	   (forward-char 2)
	   (skip-chars-forward " \t")
	   (looking-at "[-A-Z0-9$_.+]+[ \n\r]")))
	(point)
      (goto-char bol)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename mvs:nih
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the MVS version.
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
    (skip-chars-forward "-A-Z0-9$_.+" eol)
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r ?\ ))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-get-filename mvs:nih
  (&optional localp no-error-if-not-filep)
  (let ((name (efs-real-dired-get-filename localp no-error-if-not-filep))
	(parsed (efs-ftp-path (dired-current-directory))))
    (if (and name (string-equal "/" (nth 2 parsed)))
	(concat name ".")
      name)))

(efs-defun efs-dired-fixup-listing mvs:nih
  (file path &optional switches wildcard)
  ;; MVS listings have trailing spaces to 80 columns.
  ;; Can lead to a mess after indentation.
  (goto-char (point-min))
  (while (re-search-forward " +$" nil t)
    (replace-match "")))

;;;; -------------------------------------------------------
;;;; Support for the TCPFTP MVS server
;;;; -------------------------------------------------------
;;;
;;;  For TCPFTP IBM MVS V2R2.1  Does it really work?

(efs-defun efs-parse-listing mvs:tcp
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be an MVS listing
  ;; Based on the listing format of the NIH server. Hope that this format
  ;; is widespread. If a directory doesn't exist, get a 426 ftp error.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory as a remote full path
  ;; PATH = directory in full efs-syntax
  (efs-save-match-data
    (goto-char (point-min))
    (and (looking-at "Volume ")
	 (let ((top-tbl (efs-make-hashtable))
	       (case-fold (memq 'mvs efs-case-insensitive-host-types))
	       tbl-list file dn fn tbl dir-p)
	   (forward-line 1)
	   (while (not (eobp))
	     (end-of-line)
	     (setq file (buffer-substring (point)
					  (progn (skip-chars-backward "^ ")
						 (point)))
		   dn path
		   dir-p (string-match "\\." file))
	     (efs-put-hash-entry file '(nil) top-tbl)
	     (if dir-p
		 (progn
		   (setq dir-p (1+ dir-p)
			 fn (substring file 0 dir-p))
		   (efs-put-hash-entry fn '(t) top-tbl)
		   (while dir-p
		     (setq dn (efs-internal-file-name-as-directory nil
			       (concat dn fn))
			   file (substring file dir-p)
			   tbl (cdr (assoc dn tbl-list)))
		     (or tbl (setq tbl (efs-make-hashtable)
				   tbl-list (cons (cons dn tbl) tbl-list)))
		     (efs-put-hash-entry file '(nil) tbl)
		     (setq dir-p (string-match "\\." file))
		     (if dir-p
			 (progn
			   (setq dir-p (1+ dir-p)
				 fn (substring file 0 dir-p))
			   (efs-put-hash-entry fn '(t) tbl))))))
	     (forward-line 1))
	   (while tbl-list
	     (efs-put-hash-entry (car (car tbl-list)) (cdr (car tbl-list))
				 efs-files-hashtable case-fold)
	     (setq tbl-list (cdr tbl-list)))
	   top-tbl))))
	       
;;; Tree Dired

(efs-defun efs-dired-manual-move-to-filename mvs:tcp
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of the filename on this line.
  ;; This is the MVS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (and (re-search-forward " [0-9][0-9]/[0-9][0-9]/[0-9][0-9] " eol t)
	     (progn
	       (goto-char eol)
	       (skip-chars-backward "-A-Z0-9$_.")
	       (char-equal (preceding-char) ?\ ))
	     (/= eol (point)))
	(point)
      (goto-char bol)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename mvs:tcp
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the MVS version.
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
    (skip-chars-forward "-A-Z0-9$_.+" eol)
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r ?\ ))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))
    
;;; end of efs-mvs.el
