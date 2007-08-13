;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-hell.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  Hellsoft FTP server support for efs
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Tue May 25 02:31:37 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 18:32:27 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-hell)
(require 'efs)

(defconst efs-hell-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

;;;; --------------------------------------------------------------
;;;; Hellsoft FTP server support for efs
;;;; --------------------------------------------------------------

;;; The hellsoft FTP server runs on DOS PC's and Macs. The hellsoft
;;; support here probably won't work for Macs. If enough people need it
;;; the Mac support _might_ be fixed.

;;; Works for "novell FTP Server for NW 3.11 (v1.8), (c) by HellSoft."

;; Hellsoft uses unix path syntax. However, we shouldn't append a "."
;; to directories, because if foobar is a plain file, then
;; dir foobar/ will not give a listing (which is correct), but
;; dir foobar/. will give a one-line listing (which is a little strange).

(efs-defun efs-fix-dir-path hell (dir-path)
  dir-path)

;; Hellsoft returns PWD output in upper case, whereas dir listings are
;; in lower case. To avoid confusion, downcase pwd output.

(efs-defun efs-send-pwd hell (host user &optional xpwd)
  ;; Returns ( DIR . LINE ), where DIR is either the current directory, or
  ;; nil if this couldn't be found. LINE is the line of output from the
  ;; FTP server. Since the hellsoft server returns pwd output in uppercase, we
  ;; downcase it.
  (let ((result (efs-send-pwd 'unix host user xpwd)))
    (if (car result)
	(setcar result (downcase (car result))))
    result))

(defconst efs-hell-date-and-time-regexp
  (concat
   " \\([0-9]+\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) [0-3][0-9] "
   "\\([012][0-9]:[0-5][0-9]\\| [12][019][0-9][0-9]\\) "))
;; The end of this regexp corresponds to the start of a filename.

(defmacro efs-hell-parse-file-line ()
  ;; Returns ( FILENAME DIR-P SIZE ) from the current line
  ;; of a hellsoft listing. Assumes that the point is at the beginning
  ;; of the line.
  (` (let ((eol (save-excursion (end-of-line) (point)))
	   (dir-p (= (following-char) ?d)))
       (if (re-search-forward efs-hell-date-and-time-regexp eol t)
	   (list (buffer-substring (point) (progn (end-of-line) (point)))
		 dir-p
		 (string-to-int (buffer-substring (match-beginning 1)
						  (match-end 1))))))))
       
(efs-defun efs-parse-listing hell
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a listing from
  ;; a Hellsoft FTP server.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (goto-char (point-min))
  (efs-save-match-data
    (if (re-search-forward efs-hell-date-and-time-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      file-info)
	  (beginning-of-line)
	  (while (setq file-info (efs-hell-parse-file-line))
	    (efs-put-hash-entry (car file-info) (cdr file-info) tbl)
	    (forward-line 1))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl)
      (if (not (string-match (efs-internal-file-name-nondirectory
			      (efs-internal-directory-file-name dir)) "\\."))
	  ;; It's an empty dir
	  (let ((tbl (efs-make-hashtable)))
	    (efs-put-hash-entry "." '(t) tbl)
	    (efs-put-hash-entry ".." '(t) tbl)
	    tbl)))))


(efs-defun efs-allow-child-lookup hell (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  ;; Subdirs in DOS can't have an extension.
  (not (string-match "\\." file)))

;;; Tree Dired

(defconst efs-dired-hell-re-exe
  "^[^\n]+\\.exe$")

(or (assq 'hell efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'hell  efs-dired-hell-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-hell-re-dir
  "^. [ \t]*d")

(or (assq 'hell efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'hell  efs-dired-hell-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename hell
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line, where
  ;; line can be delimited by either \r or \n.
  ;; Returns (point) or nil if raise-error is nil and there is no
  ;; filename on this line. In the later case, leaves the point at the
  ;; beginning of the line.
  ;; This version is for the Hellsoft FTP server.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-hell-date-and-time-regexp eol t)
	(point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename hell
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the Hellsoft FTP server version.
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
    (skip-chars-forward "-_+=a-zA-Z0-9.$~")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\n ?\r))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-insert-headerline hell (dir)
  ;; Insert a blank line for aesthetics
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

;;; end of efs-hell.el
