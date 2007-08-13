;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dos-distinct.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  Distinct's DOS FTP server support for efs
;; Author:       Sandy Rutherford <sandy@tsmi19.sissa.it>
;; Created:      Fri Jan 15 22:20:32 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 18:30:04 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Thanks to Rodd Zurcher <rbz@hook.corp.mot.com> for beta testing.

(provide 'efs-dos-distinct)
(require 'efs)

(defconst efs-dos-distinct-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

;;;; -----------------------------------------------------------------
;;;; Distinct's DOS FTP server support for efs
;;;; -----------------------------------------------------------------

;;; This is not included in efs-dos.el with the support for the
;;; other dos ftp servers, because the Distinct server uses unix syntax
;;; for path names.

;; This is defined in efs.el, but we put it here too.

(defconst efs-dos-distinct-date-and-time-regexp
  (concat
   " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) [ 0-3][0-9],[12][90][0-9][0-9]  "
   "[ 12][0-9]:[0-5][0-9]  "))

;;; entry point

(efs-defun efs-parse-listing dos-distinct
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a listing from
  ;; Distinct's DOS FTP server. Both empty dirs, and ls errors return
  ;; empty buffers.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (goto-char (point-min))
  (efs-save-match-data
    (if (re-search-forward efs-dos-distinct-date-and-time-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      dir-p)
	(beginning-of-line)
	(while (progn
		 (setq dir-p (eq (following-char) ?d)) ; we're bolp
		 (re-search-forward
		  efs-dos-distinct-date-and-time-regexp nil t))
	  (efs-put-hash-entry (buffer-substring (point)
						     (progn (end-of-line)
							    (point)))
				   (list dir-p) tbl)
	  (forward-line 1))
	(efs-put-hash-entry "." '(t) tbl)
	(efs-put-hash-entry ".." '(t) tbl)
	tbl))))

(efs-defun efs-allow-child-lookup dos-distinct (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  ;; Subdirs in DOS can't have an extension.
  (not (string-match "\\." file)))

;;; Tree Dired

(defconst efs-dired-dos-distinct-re-exe
  "^[^\n]+\\.exe$")

(or (assq 'dos-distinct efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos-distinct  efs-dired-dos-distinct-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-dos-distinct-re-dir
  "^. [ \t]*d")

(or (assq 'dos-distinct efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'dos-distinct  efs-dired-dos-distinct-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos-distinct (dir)
  ;; The Distinct DOS server has no total line, so we insert a
  ;; blank line for aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos-distinct
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  ;; This version is for Distinct's DOS FTP server.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-dos-distinct-date-and-time-regexp eol t)
	(point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename dos-distinct
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the version for Distinct's DOS FTP server.
  (let ((opoint (point)))
    (and selective-display
	 (null no-error)
	 (eq (char-after
	      (1- (or bol (save-excursion
			    (skip-chars-backward "^\r\n")
			    (point)))))
	     ?\r)
	 ;; it's hidden or omitted
	 (cond
	  ((dired-subdir-hidden-p (dired-current-directory))
	   (error
	    (substitute-command-keys
	     "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
	  ((error
	    (substitute-command-keys
	     "File line is omitted. Type \\[dired-omit-toggle] to un-omit."
	     )))))
    (skip-chars-forward "-_+=a-z0-9.$")
    (if (or (= opoint (point)) (not (memq (following-char) '(\n \r))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

;;; end of efs-dos-distinct.el
