;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-guardian.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.7 $
;; RCS:          
;; Description:  Guardian support for efs
;; Author:       Sandy Rutherford <sandy@math.ubc.ca>
;; Created:      Sat Jul 10 12:26:12 1993 by sandy on ibm550
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Acknowledgements:
;;; Adrian Philips and David Karr for answering questions
;;; and debugging. Thanks.

(defconst efs-guardian-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.7 $" 11 -2)))

(provide 'efs-guardian)
(require 'efs)

;;;; ------------------------------------------------------------
;;;; Support for Tandem's GUARDIAN operating system.
;;;; ------------------------------------------------------------

;;;  Supposed to work for (Version 2.7 TANDEM 01SEP92).

;;;  File name syntax:
;;;
;;;  File names are of the form volume.subvolume.file where
;;;  volume is $[alphanumeric characters]{1 to 7}
;;;  subvolume is <alpha character>[<alphanumeric character>]{0 to 7}
;;;  and file is the same as subvolume.

(defconst efs-guardian-date-regexp
  (concat
   " [ 1-3][0-9]-\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|"
   "Sep\\|Oct\\|Nov\\|Dec\\)-[0-9][0-9] "))

;;; entry points -- 2 of 'em.

(efs-defun efs-fix-path guardian (path &optional reverse)
  ;; Convert PATH from unix-ish to guardian.
  ;; If REVERSE is non-nil do just that.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if reverse
	  (if (string-match
	       (concat
		"^\\(\\\\[A-Z0-9]+\\.\\)?"
		"\\(\\$[A-Z0-9]+\\)\\.\\([A-Z0-9]+\\)\\(\\.[A-Z0-9]+\\)?$")
	       path)
	      (concat
	       "/"
	       (substring path (match-beginning 2) (match-end 2))
	       "/"
	       (substring path (match-beginning 3) (match-end 3))
	       "/"
	       (and (match-beginning 4)
		    (substring path (1+ (match-beginning 4)))))
	    (error "path %s is invalid for the GUARDIAN operating system"
		   path))
	(if (string-match
	     "^/\\(\\$[A-Z0-9]+\\)/\\([A-Z0-9]+\\)\\(/[A-Z0-9]*\\)?$" path)
	    (apply 'concat
		   (substring path 1 (match-end 1))
		   "."
		   (substring path (match-beginning 2) (match-end 2))
		   (and (match-beginning 3)
			(/= (- (match-end 3) (match-beginning 3)) 1)
			(list "."
			      (substring path (1+ (match-beginning 3))))))
	  (error "path %s is invalid for the guardian operating system"
		 path))))))
  
(efs-defun efs-fix-dir-path guardian (dir-path)
  ;; Convert DIR-PATH from unix-ish to guardian fir a DIR listing.
  (efs-save-match-data
    (let ((case-fold-search t))
      (cond
       ((string-equal "/" dir-path)
	(error "Can't grok guardian disk volumes."))
       ((string-match "^/\\$[A-Z0-9]+/?$" dir-path)
	(error "Can't grok guardian subvolumes."))
       ((string-match "^/\\(\\$[A-Z0-9]+\\)/\\([A-Z0-9]+\\)\\(/[A-Z0-9]*\\)?$"
		      dir-path)
	(apply 'concat
	       (substring dir-path 1 (match-end 1))
	       "."
	       (substring dir-path (match-beginning 2) (match-end 2))
	       (and (match-beginning 3)
		    (/= (- (match-end 3) (match-beginning 3)) 1)
		    (list "."
			  (substring dir-path (1+ (match-beginning 3)))))))
       (t
	(error "path %s is invalid for the guardian operating system"))))))

(efs-defun efs-parse-listing guardian
  (host user dir path &optional switches)
  ;; Parses a GUARDIAN DIRectory listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a remote full path
  ;; PATH = directory as an efs full path
  ;; SWITCHES are never used here, but they
  ;; must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  (efs-save-match-data
    (goto-char (point-min))
    (if (re-search-forward efs-guardian-date-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      file size)
	  (while
	      (progn
		(beginning-of-line)
		(setq file (buffer-substring (point)
					     (progn
					       (skip-chars-forward "A-Z0-9")
					       (point))))
		(skip-chars-forward " ")
		(skip-chars-forward "^ ")
		(skip-chars-forward " ")
		(setq size (string-to-int (buffer-substring
					   (point)
					   (progn
					     (skip-chars-forward "0-9")))))
		(efs-put-hash-entry file (list nil size) tbl)
		(forward-line 1)
		(re-search-forward efs-guardian-date-regexp nil t)))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

(efs-defun efs-allow-child-lookup guardian (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  (efs-save-match-data
    (let ((case-fold-search t))
      (string-match "^/\\$[A-Z0-9]+/$" dir))))

(efs-defun efs-internal-file-directory-p guardian (file)
  ;; Directories pop into existence simply by putting files in them.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-match "^/\\$[A-Z0-9]+\\(/[A-Z0-9]+\\)?/?$" file)
	  t
	(efs-internal-file-directory-p nil file)))))

(efs-defun efs-internal-file-exists-p guardian (file)
  ;; Directories pop into existence simply by putting files in them.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-match "^/\\$[A-Z0-9]+\\(/[A-Z0-9]+\\)?/?$" file)
	  t
	(efs-internal-file-exists-p nil file)))))

;;; Tree Dired support

(defconst efs-dired-guardian-re-exe nil)

(or (assq 'guardian efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'guardian  efs-dired-guardian-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-guardian-re-dir nil)

(or (assq 'guardian efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'guardian  efs-dired-guardian-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename guardian
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the guardian version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (if bol
      (goto-char bol)
    (skip-chars-backward "^\n\r")
    (setq bol (point)))
  (if (save-excursion (re-search-forward efs-guardian-date-regexp eol t))
      (progn
	(if (looking-at ". [^ ]")
	    (forward-char 2))
	(point))
    (and raise-error (error "No file on this line"))))

(efs-defun efs-dired-manual-move-to-end-of-filename guardian
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the guardian version.
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
	   "File line is omitted. Type \\[dired-omit-toggle] to un-omit.")))))
  (if (and
       (>= (following-char) ?A)
       (<= (following-char) ?Z)
       (progn
	 (skip-chars-forward "A-Z0-9")
	 (= (following-char) ?\ )))
      (point)
    (and (null no-error)
	 (error "No file on this line"))))

(efs-defun efs-dired-ls-trim guardian ()
  (goto-char (point-min))
  (let (case-fold-search)
    (if (re-search-forward efs-guardian-date-regexp nil t)
	(progn
	  (beginning-of-line)
	  (delete-region (point-min) (point))
	  (forward-line 1)
	  (delete-region (point) (point-max))))))

;;; end of efs-guardian.el
