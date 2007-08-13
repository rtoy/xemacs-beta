;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-vos.el
;; Description:  VOS support for efs
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Sat Apr  3 03:05:00 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 18:45:24 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; The original ange-ftp VOS support was written by Joe Wells <jbw@cs.bu.edu>

;;; Thank you to Jim Franklin <jimf%shared@uunet.uu.net> for providing
;;; information on the VOS operating system.

(provide 'efs-vos)
(require 'efs)

(defconst efs-vos-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;;;---------------------------------------------------------------
;;;; VOS support for efs
;;;;---------------------------------------------------------------

;;; A legal VOS pathname is of the form:
;;; %systemname#diskname>dirname>dirname>dir-or-filename
;;;
;;; Each of systemname, diskname, dirname, dir-or-filename can be
;;; at most 32 characters.
;;; Valid characters are all alpha, upper and lower case, all digits,
;;; plus: @[]\^`{|}~"$+,-./:_
;;; restrictions: name cannot begin with hyphen (-) or period (.)
;;;               name must not end with a period (.)
;;;               name must not contain two adjacent periods (.)
;;;
;;; Invalid characters are:
;;;               non-printing control characters
;;;               SPACE and DEL
;;;               !#%&'()*;<=>?
;;;               all other ascii chars
;;;
;;; The full pathname must be less than or equal to 256 characters.
;;; VOS pathnames are CASE-SENSITIVE.
;;; The may be a directory depth limitation of 10 (newer versions may have
;;; eliminated this).

;;; entry points

(efs-defun efs-fix-path vos (path &optional reverse)
  ;; Convert PATH from UNIX-ish to VOS.
  ;; If REVERSE given then convert from VOS to UNIX-ish.
  ;; Does crude checking for valid path syntax, but is by no means exhaustive.
  (efs-save-match-data
    (if reverse
	(if (string-match "^\\(\\(%[^#>%]+\\)?#[^>#%]+\\)?>[^>#%]" path)
	    (let ((marker (1- (match-end 0)))
		  (result "/")
		  system drive)
	      (if (match-beginning 1)
		  (if (match-beginning 2)
		      (setq system (substring path 1 (match-end 2))
			    drive (substring path (1+ (match-end 2))
					     (match-end 1)))
		    (setq drive (substring 1 (match-end 1)))))
	      (while (string-match ">" path marker)
		(setq result (concat result
				     (substring path marker
						(match-beginning 0))
				     "/")
		      marker (match-end 0)))
	      (if drive
		  (if system
		      (concat "/" system "/" drive result
			    (substring path marker))
		    (concat "/" drive result (substring path marker)))
		(concat result (substring path marker))))
	  (error "Invalid VOS pathname %s" path))
    (if (string-match "^/\\([^/]+\\)/\\([^/]+\\)/[^/]" path)
	(let ((marker (1- (match-end 0)))
	      (result (concat "%"
			      (substring path
					 (match-beginning 1)
					 (match-end 1))
			      "#"
			      (substring path
					 (match-beginning 2)
					 (match-end 2))
			      ">")))
	  ;; I'm guessing that VOS doesn't have a directory syntax.
	  (setq path (efs-internal-directory-file-name path))
	  (while (string-match "/" path marker)
	    (setq result
		  (concat result
			  (substring path marker
				     (match-beginning 0))
			  ">")
		  marker (match-end 0)))
	  (concat result (substring path marker)))
      (error "Cannot convert path %s to VOS." path)))))

(efs-defun efs-fix-dir-path vos (dir-path)
  ;; Convert path from UNIX-ish to VMS ready for a DIRectory listing.
  (cond ((string-equal dir-path "/")
	 (error "Cannot gork VOS system names"))
	((string-match "^/[^/]/$" dir-path)
	 (error "Cannot grok VOS devices"))
	((efs-fix-path 'vos dir-path))))

(defconst efs-vos-date-and-time-regexp
  (concat
   "\\(^\\| \\)" ; For links, this must match at the beginning of the line.
   "[678901][0-9]-[01][0-9]-[0-3][0-9] [012][0-9]:[0-6][0-9]:[0-6][0-9]  "))
;; Regexp to match a VOS file line. The end of the regexp must correspond
;; to the start of the filename.

(defmacro efs-vos-parse-filename ()
  ;; Return the VOS filename on the current line of a listing.
  ;; Assumes that the point is at the beginning of the line.
  ;; Return nil if no filename is found.
  (` (let ((eol (save-excursion (end-of-line) (point))))
       (and (re-search-forward efs-vos-date-and-time-regexp eol t)
	    (buffer-substring (point) eol)))))

(efs-defun efs-parse-listing vos
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be in MultiNet FTP dir
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
    (let (tbl file)
      ;; Look file files.
      (if (search-forward "\nFiles: " nil t)
	  (progn
	    (setq tbl (efs-make-hashtable))
	    (forward-line 1)
	    (skip-chars-forward "\n")
	    (while (setq file (efs-vos-parse-filename))
	      (efs-put-hash-entry file '(nil) tbl)
	      (forward-line 1))))
      ;; Look for directories.
      (if (search-forward "\nDirs: " nil t)
	  (progn
	    (or tbl (setq tbl (efs-make-hashtable)))
	    (forward-line 1)
	    (skip-chars-forward "\n")
	    (while (setq file (efs-vos-parse-filename))
	      (efs-put-hash-entry file '(t) tbl)
	      (forward-line 1))))
      ;; Look for links
      (if (search-forward "\nLinks: " nil t)
	  (let (link)
	    (or tbl (setq tbl (efs-make-hashtable)))
	    (forward-line 1)
	    (skip-chars-forward "\n")
	    (while (setq file (efs-vos-parse-filename))
	      (if (string-match " ->  \\([^ ]+\\)" file)
		  ;; VOS puts a trailing blank after the name of a symlink
		  ;; target. Go figure...
		  (setq link (substring file (match-beginning 1) (match-end 1))
			file (substring file 0 (match-beginning 0)))
		(setq link "")) ; weird?
	      (efs-put-hash-entry file (list link) tbl)
	      (forward-line 1))))
      ;; This returns nil if no headings for files, dirs, or links
      ;; are found. In this case, we're assuming that it isn't a valid
      ;; listing.
      (if tbl
	  (progn
	    (efs-put-hash-entry "." '(t) tbl)
	    (efs-put-hash-entry ".." '(t) tbl)))
      tbl)))
	
(efs-defun efs-allow-child-lookup vos (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  ;; Directoried don't have a size.
  (string-match ": not a file\\.$"
		(cdr (efs-send-size host user (concat dir file)))))

;;; Tree Dired Support

(defconst efs-dired-vos-re-exe
  "^.  +e ")

(or (assq 'vos efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'vos  efs-dired-vos-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-vos-re-dir
  "^.  +[nsm] +[0-9]+ +[678901][0-9]-")

(or (assq 'vos efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'vos  efs-dired-vos-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename vos
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line, where
  ;; line can be delimited by either \r or \n.
  ;; Returns (point) or nil if raise-error is nil and there is no
  ;; filename on this line. In the later case, leaves the point at the
  ;; beginning of the line.
  ;; This version is for VOS.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-vos-date-and-time-regexp eol t)
	(point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename vos
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the VOS version.
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
    (skip-chars-forward "-a-zA-Z0-9@[]\\^`{|}~\"$+,./:_")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r ?\ ))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-fixup-listing vos (file path &optional switches wildcard)
  ;; VOS listing contain some empty lines, which is inconvenient for dired.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point))
  (while (search-forward "\n\n" nil t)
    (forward-char -2)
    (delete-char 1)))

(efs-defun efs-dired-ls-trim vos ()
  ;; Trims VOS dir listings for single files, so that they are exactly one line
  ;; long.
  (goto-char (point-min))
  (let (case-fold-search)
    (re-search-forward efs-vos-date-and-time-regexp))
  (beginning-of-line)
  (delete-region (point-min) (point))
  (forward-line 1)
  (delete-region (point) (point-max)))

;;; end of efs-vos.el
