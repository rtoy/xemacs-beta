;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-nos-ve.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  efs support for NOS/VE
;; Authors:      Sandy Rutherford <sandy@ibm550.sissa,it>
;; Created:      Fri Aug 19 04:57:09 1994 by sandy on ibm550
;; Modified:     Sun Nov 27 18:39:43 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-nos-ve)
(require 'efs)

;;; Works for NOS/VE from CDC.  NOS/VE runs on Cybers.

;;; Thank you to Jost Krieger <Jost.Krieger@rz.ruhr-uni-bochum.de> for
;;; providing imformation and testing.

(defconst efs-nos-ve-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;;;---------------------------------------------------------------
;;;; NOS/VE support for efs
;;;;---------------------------------------------------------------

;;; A legal NOS/VE filename is of the form
;;;    <family>.<dirname>.<dirname>.<direname>.....<filename>
;;;    where <family> always starts with the char : and is followed by
;;;    alphanumeric characters.  Each <dirname> or <filename> can be up to 31
;;;    characters.  File names are case insensistive.
;;; eg. :FOO.DIR_1.DIR_2.BAR
;;;
;;; The character set consists of (single case) alphabet, the numerals,
;;; and the characters "@$_#". (Not the quotes ...) The characters
;;; "[\]{|}" will also occur in a misguided attempt at
;;; internationalization. A filename may not start with a numeral.


;;; entry points

(efs-defun efs-fix-path nos-ve (path &optional reverse)
  ;; Convert path from UNIX to NOS/VE.
  ;; If REVERSE is non-nil, goes in the opposite direction.
  (if reverse
      (let* ((res (concat "." path))
	     (len (length res))
	     (n 0))
	(while (< n len)
	  (and (= (aref res n) ?.) (aset res n ?/))
	  (setq n (1+ n)))
	res)
    (let* ((res (substring (efs-internal-directory-file-name path) 1))
	   (len (length res))
	   (n 0))
      (while (< n len)
	(and (= (aref res n) ?/) (aset res n ?.))
	(setq n (1+ n)))
      res)))

(efs-defun efs-fix-dir-path nos-ve (dir-path)
  ;; Converts DIR-PATH to NOS/VE format for a directory listing.
  (efs-fix-path 'nos-ve dir-path))

;;; parser

(defconst efs-nos-ve-file-line-regexp
  (concat
   " \\([>0-9,]+\\) bytes \\(in [0-9]+ \\(file\\|catalog\\)s?\\)?\\|"
   "\\( -- empty catalog\\)\\| -- device"))

(efs-defun efs-parse-listing nos-ve (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a NOS/VE listing.
  ;; Returns a hashtable.
  (goto-char (point-min))
  (efs-save-match-data
    (if (and (re-search-forward efs-nos-ve-file-line-regexp
				(save-excursion (end-of-line) (point)) t)
	     (or (match-beginning 2) (match-beginning 4)))
	(let ((tbl (efs-make-hashtable))
	      size dir-p file)
	  (forward-line 1)
	  (while (re-search-forward efs-nos-ve-file-line-regexp
				    (save-excursion (end-of-line) (point)) t)
	    (setq size (and (match-beginning 1)
			    (buffer-substring
			     (match-beginning 1) (match-end 1)))
		  dir-p (null (null (or (match-beginning 2)
					(match-beginning 4)))))
	    (if size
		(let ((start 0)
		      res)
		  (while (string-match "," size start)
		    (setq res (concat res (substring size start
						     (match-beginning 0)))
			  start (match-end 0)))
		  (setq size (string-to-int
			      (concat res (substring size start))))))
	    (beginning-of-line)
	    (forward-char 2)
	    (setq file (buffer-substring
			(point)
			(progn (skip-chars-forward "^ \t\n") (point))))
	    (efs-put-hash-entry file (list dir-p size)
				(or tbl (setq tbl (efs-make-hashtable))))
	    (forward-line 1))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

(efs-defun efs-allow-child-lookup nos-ve (host user dir file)
  ;; Returns non-nil if in directory DIR,  FILE could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted. Note that DIR is in directory syntax.
  ;; i.e. /foo/bar/, not /foo/bar.
  ;; Deal with dired. Anything else?
  (not (and (boundp 'dired-local-variables-file)
	    (stringp dired-local-variables-file)
	    (string-equal (downcase dired-local-variables-file)
			  (downcase file)))))

;;; Tree Dired

(defconst efs-dired-nos-ve-re-exe "^.[^ \t\n]")
;; Matches no lines.  Should it match something?

(or (assq 'nos-ve efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'nos-ve  efs-dired-nos-ve-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-nos-ve-re-dir " [0-9,]+ bytes in [0-9]+ file")

(or (assq 'nos-ve efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'nos-ve  efs-dired-nos-ve-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-fixup-listing nos-ve (file path &optional switches
						wildcard)
  ;; Need to turn the header line into something to masquerading as a file
  ;; line, and need to remove the indentation.  Both upset dired.
  (goto-char (point-min))
  (while (search-forward "\n  " nil t)
    (delete-char -2))
  (goto-char (point-min))
  (if (looking-at "\\([^ \n]+ +\\)[0-9,]+ bytes in [0-9]+ file")
      (progn
	(delete-region (match-beginning 1) (match-end 1))
	(insert "  Total of "))))

(defconst efs-dired-nos-ve-file-line-regexp
  (concat
   ".[ \t]+\\([][{}|\\\\a-z0-9@$_#]+\\) +"
   "\\([>0-9,]+ bytes\\|-- \\(empty\\|device\\)\\)"))

(efs-defun efs-dired-manual-move-to-filename nos-ve
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the NOS/VE version.
  (if bol
      (goto-char bol)
    (skip-chars-backward "^\n\r"))
  (if (looking-at efs-dired-nos-ve-file-line-regexp)
      (goto-char (match-beginning 1))
    (and raise-error (error "No file on this line"))))

(efs-defun efs-dired-manual-move-to-end-of-filename nos-ve
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the NOS/VE version.
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
    (skip-chars-forward "_a-z0-9$@#\\\\[]{}|") ; right char set?
    (if (or (= opoint (point)) (/= (following-char) ?\ ))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

;;; end of efs-nos-ve.el
