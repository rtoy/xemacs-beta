;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-ms-unix.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  efs support for the Microsoft PC FTP server in unix mode.
;; Author:       Sandy Rutherford <sandy@tsmi19.sissa.it>
;; Created:      Thu Aug 19 08:31:15 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 18:37:00 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'efs-ms-unix)
(require 'efs)

(defconst efs-ms-unix-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

(defvar efs-ms-unix-month-and-time-regexp
  (concat
   " \\([0-9]+\\) +" ; file size
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) [ 0-3][0-9]"
   " +\\([ 012][0-9]:[0-6][0-9]\\|[12][90][0-9][0-9]\\) +"))

;;; entry points

(efs-defun efs-fix-path ms-unix (path &optional reverse)
  ;; Convert PATH from UNIX-ish to MS-UNIX.
  (if reverse
      (concat "/" path)
    (substring path 1)))

(efs-defun efs-fix-dir-path ms-unix (dirpath)
  ;; Convert a path from UNIX-ish to MS-UNIX for a dir listing
  (if (string-equal dirpath "/")
      (error "Cannot grok disk names.")
    (setq dirpath (substring dirpath 1))
    (efs-save-match-data
      (if (string-match "/$" dirpath)
	  (concat dirpath "*")
	dirpath))))

(defmacro efs-ms-unix-parse-file-line ()
  ;; Extract the filename, size, and permission string from the current
  ;; line of a dired-like listing. Assumes that the point is at
  ;; the beginning of the line, leaves it just before the size entry.
  ;; Returns a list (name size perm-string nlinks owner).
  ;; If there is no file on the line, returns nil.
  (` (let ((eol (save-excursion (end-of-line) (point)))
	   name size modes nlinks owner)
       (skip-chars-forward " 0-9" eol)
       (and
	(looking-at efs-modes-links-owner-regexp)
	(setq modes (buffer-substring (match-beginning 1)
				      (match-end 1))
	      nlinks (string-to-int (buffer-substring (match-beginning 2)
						      (match-end 2)))
	      owner (buffer-substring (match-beginning 3) (match-end 3)))
	(re-search-forward efs-ms-unix-month-and-time-regexp eol t)
	(setq name (buffer-substring (point) eol)
	      size (string-to-int (buffer-substring (match-beginning 1)
						    (match-end 1))))
	(list name size modes nlinks owner)))))

(efs-defun efs-parse-listing ms-unix (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be output from
  ;; the Microsoft FTP server in unix mode.
  ;; Return a hashtable as the result. SWITCHES are never used,
  ;; but they must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory in as a full remote path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches
  (goto-char (point-min))
  (efs-save-match-data
    (if (re-search-forward efs-ms-unix-month-and-time-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      size modes nlinks dir-p owner file)
	  (beginning-of-line)
	  (while (setq file (efs-ms-unix-parse-file-line))
	    (setq size (nth 1 file)
		  modes (nth 2 file)
		  nlinks (nth 3 file)
		  owner (nth 4 file)
		  file (car file)
		  dir-p (= (string-to-char modes) ?d))
	    (if (and dir-p
		     (string-match "/$" file))
		(setq file (substring file 0 -1)))
	    (efs-put-hash-entry file (list dir-p size owner modes nlinks) tbl)
	    (forward-line 1))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

;;; Tree Dired

;; ms-unix does not have a total line

(efs-defun efs-dired-insert-headerline ms-unix (dir)
  ;; MTS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename ms-unix
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  ;; This version is for ms-unix.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (re-search-forward efs-ms-unix-month-and-time-regexp eol t)
	(point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename ms-unix
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the ms-unix version.
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
	    "File line is omitted. Type \\[dired-omit-toggle] to un-omit.")))))
    (if (eolp)
	(progn
	  (goto-char opoint)
	  (if no-error
	      nil
	    (error "No file on this line")))
      (end-of-line)
      (if (char-equal (preceding-char) ?/)
	  (forward-char -1))
      (point))))

;;; end of efs-ms-unix.el
