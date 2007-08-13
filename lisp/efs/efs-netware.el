;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-netware.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.5 $
;; RCS:          
;; Description:  efs support for the Novell Netware FTP server
;; Author:       Sandy Rutherford <sandy@math.ubc.ca>
;; Created:      Fri Oct 15 00:30:50 1993 by sandy on gauss.math.ubc.ca
;; Modified:     Tue Nov 22 00:11:46 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Works for (at least) Novell NetWare v3.11. This is a DOS FTP server,
;;; however, it returns a unix-ish path format.

(provide 'efs-netware)
(require 'efs)

(defconst efs-netware-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.5 $" 11 -2)))

;;; Basic efs support

(defconst efs-netware-date-regexp
  (concat
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|"
   "Dec\\) [ 0-3][0-9] \\([0-9][0-9] \\)?[0-2][0-9]:[0-6][0-9] +"))

(efs-defun efs-fix-path netware (path &optional reverse)
  ;; Convert PATH from UNIX-ish to netware.
  (efs-save-match-data
    (if reverse
	(cond ((string-match "^[^/][^:]*:" path)
	       (concat "/" path))
	      ((string-match "^/" path)
	       path)
	      ((error "%s not a valid netware path." path)))
      (if (string-match ":" path)
	  (substring path 1)
	path))))

(efs-defun efs-fix-dir-path netware (dir-path)
  ;; Convert DIR-PATH from UN*X-ish to Netware for a DIR listing.
  (efs-fix-dir-path nil (efs-fix-path 'netware dir-path)))

(defun efs-netware-bogus-listing-p (dir path)
  (save-excursion
    (and
     (not (eobp))
     (save-excursion (forward-line 1) (eobp))
     (not (string-equal dir "/"))
     (re-search-forward efs-netware-date-regexp nil t)
     (search-forward "/.\n"))))

(efs-defun efs-parse-listing netware (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a listing from
  ;; a Novell Netware FTP server (runs under DOS).
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
    (if (re-search-forward efs-netware-date-regexp nil t)
	(progn
	  (beginning-of-line)
	  (and (not (efs-netware-bogus-listing-p dir path))
	       (let ((tbl (efs-make-hashtable))
		     dir-p file size)
		 (while (let ((eol (save-excursion (end-of-line) (point))))
			  (setq dir-p (= (following-char) ?d))
			  (re-search-forward efs-netware-date-regexp eol t))
		   (setq file (buffer-substring (point)
						(progn (end-of-line) (point)))
			 size (progn
				(goto-char (match-beginning 0))
				(skip-chars-backward " ")
				(buffer-substring (point)
						  (progn
						    (skip-chars-backward "0-9")
						    (point)))))
		   (if (string-equal size "")
		       (setq size nil)
		     (setq size (string-to-int size)))
		   (efs-put-hash-entry file (list dir-p size) tbl)
		   (forward-line 1))
		 (efs-put-hash-entry "." '(t) tbl)
		 (efs-put-hash-entry ".." '(t) tbl)
		 tbl))))))

;;; Sorting dir listings.

(efs-fset 'efs-t-converter 'netware 'efs-unix-t-converter)

;;; Dired support

(defconst efs-dired-netware-re-exe "\\.\\(exe\\|EXE\\)$")
(or (assq 'netware efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'netware efs-dired-netware-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-netware-re-dir "^.[ \t]+d ")
(or (assq 'netware efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'netware  efs-dired-netware-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename netware
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  ;; This is the Netware version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    ;; move over marker
    (if (re-search-forward efs-netware-date-regexp eol t)
	(goto-char (match-end 0)) ; returns (point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename netware
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the Netware version.
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
    (skip-chars-forward "^A-Z\n\r")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r ?\ ))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-insert-headerline netware (dir)
  ;; Insert a blank line for aesthetics.
  (insert " \n")
  (forward-char -2)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-fixup-listing netware
  (file path &optional switches wildcard)
  ;; listings come out in random order
  (let (case-fold-search)
    (if (or (null switches)
	    ;; In case efs is handling the switches itself.
	    (not (string-match "t" switches)))
	(progn
	  (goto-char (point-max))
	  (if (re-search-backward efs-netware-date-regexp nil t)
	      (save-restriction
		(forward-line 1)
		(narrow-to-region (point-min) (point))
		(forward-line -1)
		;; Count how many fields
		(let ((fields 0))
		  (skip-chars-forward " \t")
		  (while (not (eolp))
		    (skip-chars-forward "^  \t\n")
		    (skip-chars-forward " \t")
		    (setq fields (1+ fields)))
		  (sort-fields fields (point-min) (point-max)))))))))

;;; end of efs-netware.el
