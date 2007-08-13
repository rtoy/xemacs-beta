;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-ka9q.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  KA9Q support for efs
;; Author:       Sandy Rutherford <sandy@tsmi19.sissa.it>
;; Created:      Mon Dec 21 10:34:43 1992 by sandy on ibm550
;; Modified:     Sun Nov 27 18:32:56 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Thanks go to Joe Reinhardt for beta testing.

(provide 'efs-ka9q)
(require 'efs)

(defconst efs-ka9q-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;;-----------------------------------------------------------------
;;; KA9Q support for efs
;;;-----------------------------------------------------------------
;;;
;;; KA9Q is not really an OS, but an ftp server that runs on PC's.
;;; It runs under DOS and unix. Seems to have been adopted by LINUX.

;; KA9Q uses unix syntax for paths, so don't need to bother with pathname
;; converters. It always gives a listing, even if a file or dir doesn't
;; exist. Therefore, we shall assume that empty dir = nonexistent dir. sigh...

(defconst efs-ka9q-date-regexp
  " +[.,0-9]* [ 0-2][0-9]:[0-9][0-9] +[0-9]+/[0-9]+/[0-9]+")
  ;; (match-beginning 0) should be the last char of the filename.

(defun efs-ka9q-bogus-listing (dir path)
  ;; Check to see if a 1-line ka9q listing is bogus, and the directory
  ;; is really just a file.
  (and
   (not (string-equal "/" dir))
   (goto-char (point-min))
   (looking-at (regexp-quote
		(concat (efs-internal-file-name-nondirectory
			 (efs-internal-directory-file-name dir))
			" ")))
   (forward-line 1)
   (looking-at "1 file\\. ")
   (string-match "^No files\\. "
		 ;; ls switches don't matter
		 (efs-ls (concat path "*") "-al" t t))))
  
(efs-defun efs-parse-listing ka9q
  (host user dir path &optional switches)
  ;; Parse the current listing which is assumed to be a ka9q listing.
  ;; Format is based on version 890421.1a.linux.7 (whatever that means).
  ;; Note that ka9q uses two files per line.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a remote full path
  ;; PATH = directory in full efs-path syntax
  (let ((tbl (efs-make-hashtable))
	dir-p file)
    (efs-save-match-data
      (if (and
	   (progn
	     (goto-char (point-max))
	     (forward-line -1)
	     ;; Although "No files." may refer to an empty
	     ;; directory, it may also be a non-existent
	     ;; dir. Returning nil should force a listing
	     ;; of the parent, which will sort things out.
	     (looking-at "[0-9]+ files?\\. "))
	   ;; Check for a bogus listing.
	   (not (efs-ka9q-bogus-listing dir path)))
	  (progn
	    (goto-char (point-min))
	    (while (re-search-forward efs-ka9q-date-regexp nil t)
	      (goto-char (match-beginning 0))
	      (if (setq dir-p (eq (preceding-char) ?/))
		  (forward-char -1))
	      (setq file (buffer-substring (point)
					   (progn (skip-chars-backward "^ \n")
						  (point))))
	      (efs-put-hash-entry file (list dir-p) tbl)
	      (goto-char (match-end 0)))
	    (efs-put-hash-entry "." '(t) tbl)
	    (efs-put-hash-entry ".." '(t) tbl)
	    tbl)))))

;;; Tree Dired

(defconst efs-dired-ka9q-re-exe
  "^. [^ \n\r./]+\\.exe ")

(or (assq 'ka9q efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'ka9q  efs-dired-ka9q-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-ka9q-re-dir
  "^. [^ \n\r/]+/ ")

(or (assq 'ka9q efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'ka9q  efs-dired-ka9q-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-fixup-listing ka9q (file path &optional switches wildcard)
  ;; ka9q puts two files per line. Need to put in one file per line format
  ;; for dired.
  (let ((regexp (concat efs-ka9q-date-regexp "   ")))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (delete-char -3)
      (insert-char ?\n 1))
    ;; is there a blank line left?
    (if (looking-at "[ \t]*\n")
	(delete-region (match-beginning 0) (match-end 0)))))

(efs-defun efs-dired-ls-trim ka9q ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (forward-line 1)
    (if (looking-at "\\([0-9]+\\|No\\) files?\\. ")
	(delete-region (point) (point-max)))))

(efs-defun efs-dired-insert-headerline ka9q (dir)
  ;; Insert a headerline
  (insert-char ?\n 1)
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename ka9q
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  ;; This is the KA9Q version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (re-search-forward efs-ka9q-date-regexp eol t)
	(progn
	  (goto-char (match-beginning 0))
	  (skip-chars-backward "^ " bol)
	  (point))
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename ka9q
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the KA9Q version.
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
    (skip-chars-forward "^ \n\r/")
    (if (or (= opoint (point)) (not (memq (following-char) '(?/ ?\ ))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

;;; end of efs-ka9q.el
