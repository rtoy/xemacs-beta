;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-mts.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  MTS support for efs
;; Author:       Sandy Rutherford <sandy@itp.ethz.ch>
;; Created:      Fri Oct 23 08:51:29 1992
;; Modified:     Sun Nov 27 18:37:18 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-mts)
(require 'efs)

(defconst efs-mts-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; MTS support
;;;; ------------------------------------------------------------

;;; efs has full support, including tree dired support, for hosts running
;;; the Michigan terminal system.  It should be able to automatically
;;; recognize any MTS machine. We would be grateful if you
;;; would report any failures to automatically recognize a MTS host as a bug.
;;;
;;; Filename syntax:
;;; 
;;; MTS filenames are entered in a UNIX-y way. For example, if your account
;;; was YYYY, the file FILE in the account XXXX: on mtsg.ubc.ca would be
;;; entered as
;;;   /YYYY@mtsg.ubc.ca:/XXXX:/FILE
;;; In other words, MTS accounts are treated as UNIX directories. Of course,
;;; to access a file in another account, you must have access permission for
;;; it.  If FILE were in your own account, then you could enter it in a
;;; relative path fashion as
;;;   /YYYY@mtsg.ubc.ca:FILE
;;; MTS filenames can be up to 12 characters. Like UNIX, the structure of the
;;; filename does not contain a TYPE (i.e. it can have as many "."'s as you
;;; like.) MTS filenames are always in upper case, and hence be sure to enter
;;; them as such! MTS is not case sensitive, but an EMACS running under UNIX
;;; is.


(defconst efs-mts-date-regexp
  (concat
   " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) [ 123]?[0-9] "))

;;; The following two functions are entry points to this file.
;;; They are put into the appropriate alists in efs.el

(efs-defun efs-fix-path mts (path &optional reverse)
  ;; Convert PATH from UNIX-ish to MTS.
  ;; If REVERSE given then convert from MTS to UNIX-ish.
  (efs-save-match-data
    (if reverse
	(if (string-match "^\\([^:]+:\\)?\\(.*\\)$" path)
	    (let (acct file)
	      (if (match-beginning 1)
		  (setq acct (substring path 0 (match-end 1))))
	      (if (match-beginning 2)
		  (setq file (substring path
					(match-beginning 2) (match-end 2))))
	      (concat (and acct (concat "/" acct "/"))
		      file))
	  (error "path %s didn't match" path))
      (if (string-match "^/\\([^:]+:\\)/\\(.*\\)$" path)
	  (concat (substring path 1 (match-end 1))
		  (substring path (match-beginning 2) (match-end 2)))
	;; Let's hope that mts will recognize it anyway.
	path))))

(efs-defun efs-fix-dir-path mts (dir-path)
;; Convert path from UNIX-ish to MTS ready for a DIRectory listing.
;; Remember that there are no directories in MTS.
  (if (string-equal dir-path "/")
      (error "Cannot get listing for fictitious \"/\" directory.")
    (let ((dir-path (efs-fix-path 'mts dir-path)))
      (cond
       ((string-equal dir-path "")
	"?")
       ((efs-save-match-data (string-match ":$" dir-path))
	(concat dir-path "?"))
       (dir-path))))) ; It's just a single file.


(efs-defun efs-parse-listing mts
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be in
  ;; mts ftp dir format.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a remote full path
  ;; PATH = directory as an efs full path
  ;; SWITCHES are never used here, but they
  ;; must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  (let ((tbl (efs-make-hashtable))
	perms)
    (goto-char (point-min))
    (efs-save-match-data
      (while (re-search-forward efs-mts-date-regexp nil t)
	(beginning-of-line)
	(if (looking-at "[rwed]+")
	    (setq perms (buffer-substring (match-beginning 0) (match-end 0)))
	  (setq perms nil))
	(end-of-line)
	(skip-chars-backward " ")
	(let ((end (point)))
	  (skip-chars-backward "-A-Z0-9_.!")
	  (efs-put-hash-entry (buffer-substring (point) end)
				   (list nil nil nil perms) tbl))
	(forward-line 1)))
      ;; Don't need to bother with ..
    (efs-put-hash-entry "." '(t) tbl)
    tbl))

(efs-defun efs-allow-child-lookup mts (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.

  ;; MTS file system is flat. Only "accounts" are subdirs.
  (string-equal "/" dir))

(efs-defun efs-internal-file-writable-p mts (user owner modes)
  (if (stringp modes)
      (efs-save-match-data
	(null (null (string-match "w" modes))))
    t)) ; guess

(efs-defun efs-internal-file-readable-p mts (user owner modes)
  (if (stringp modes)
      (efs-save-match-data
	(null (null (string-match "r" modes))))
    t)) ; guess

;;; Tree dired support:

;; There aren't too many systems left that use MTS. This dired support will
;; work for the implementation of ftp on mtsg.ubc.ca. I hope other mts systems
;; implement ftp in the same way. If not, it might be necessary to make the
;; following more flexible.

(defconst efs-dired-mts-re-exe nil)

(or (assq 'mts efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'mts  efs-dired-mts-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-mts-re-dir nil)

(or (assq 'mts efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'mts  efs-dired-mts-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename mts
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the MTS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (if bol
      (goto-char bol)
    (skip-chars-backward "^\n\r"))
  (if (re-search-forward efs-mts-date-regexp eol t)
      (progn
	(skip-chars-forward " ")      ; Eat blanks after date
	(skip-chars-forward "0-9:")   ; Eat time or year
	(skip-chars-forward " ")      ; one space before filename
	(point))
    (and raise-error (error "No file on this line"))))

(efs-defun efs-dired-manual-move-to-end-of-filename mts
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the MTS version.
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
    (skip-chars-forward "-A-Z0-9._!")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\r ?\n))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-fixup-listing mts (file path &optional switches wildcard)
  ;; If you're not listing your own account, MTS puts the
  ;; account name in front of each filename. Scrape them off.
  ;; PATH will have unix /'s on it.
  ;; file-name-directory is in case of wildcards
  (let ((len (length path)))
    (if (> len 2)
	(progn
	  (if (= (aref path (1- len)) ?/)
	      (setq path (substring path -2))
	    (setq path (substring path -1)))
	  (goto-char (point-min))
	  (while (search-forward path nil t)
	    (delete-region (match-beginning 0) (match-end 0)))))))

(efs-defun efs-dired-insert-headerline mts (dir)
  ;; MTS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

;;; end of efs-mts.el
