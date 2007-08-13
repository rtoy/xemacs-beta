;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dl.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.3 $
;; RCS:          
;; Description:  Unix descriptive listing support for efs
;; Author:       Sandy Rutherford <sandy@tsmi19.sissa.it>
;; Created:      Wed Jan 13 19:19:20 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 18:29:41 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-unix:dl)
(require 'efs)

(defconst efs-dl-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.3 $" 11 -2)))

;;;-----------------------------------------------------------------
;;; Unix descriptive listing (dl) support for efs
;;;-----------------------------------------------------------------

;; this is also defined in efs.el, because it used to recognize
;; a dl listing. We re-define it here just to keep the dl stuff self-contained.

(defconst efs-unix:dl-listing-regexp
  "^[^ \n\t]+\n? +\\([0-9]+\\|-\\|=\\) ")

;; entry point

(efs-defun  efs-parse-listing unix:dl
  (host user dir path &optional switches)
  ;; Parse the current buffer, which is assumed to be a unix descriptive
  ;; listing, and return a hashtable.
  ;; HOST = remote host name
  ;; USER = user name
  ;; DIR = directory in as a full remote path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches (not relevant here)
  (goto-char (point-min))
  ;; Is it really a listing?
  (efs-save-match-data
    (if (re-search-forward efs-unix:dl-listing-regexp nil t)
	(let ((tbl (efs-make-hashtable)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (efs-put-hash-entry
	     (buffer-substring (point)
			       (progn
				 (skip-chars-forward "^ /\n")
				 (point)))
	     (list (eq (following-char) ?/))
	     tbl)
	    (forward-line 1))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

;;; Support for tree dired.

(defconst efs-dired-dl-re-dir
  "^. [^ /]+/[ \n]"
  "Regular expression to use to search for dl directories.")

(or (assq 'unix:dl efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'unix:dl  efs-dired-dl-re-dir)
		efs-dired-re-dir-alist)))


(efs-defun efs-dired-manual-move-to-filename unix:dl
  (&optional raise-error bol eol)
  ;; In dired, move to the first character of the filename on this line.
  ;; This is the Unix dl version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (and
	 (> (- eol bol) 3)
	 (progn
	   (forward-char 2)
	   (skip-chars-forward " \t")
	   (looking-at "[^ \n\t]+\n? +\\([0-9]+\\|-\\|=\\) ")))
	(point)
      (goto-char bol)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename unix:dl
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the Unix dl version.
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
    (skip-chars-forward "^ /\r\n\t")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\  ?/))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-insert-headerline unix:dl (dir)
  ;; Unix dl has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-fixup-listing unix:dl (file path &optional
						 switches wildcard)
  ;; Deal with continuation lines.
  (efs-save-match-data
    (goto-char (point-min))
    (while (re-search-forward "\n +" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert " "))))

;;; end of efs-dl.el
