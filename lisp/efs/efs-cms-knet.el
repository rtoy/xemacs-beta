;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-cms-knet.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.1 $
;; RCS:          
;; Description:  CMS support for efs using KNET/VM server
;; Authors:	 Sandy Rutherford <sandy@ibm550.sissa.it>
;;               Joerg-Martin Schwarz <schwarz@hal1.physik.uni-dortmund.de>
;; Created:      Wed Mar 23 14:39:00 1994 by schwarz on hal1 from efs-cms.el
;; Modified:     Sun Nov 27 11:45:58 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-cms-knet)
(require 'efs)

(defconst efs-cms-knet-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.1 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; CMS support for KNET-VM server
;;;; ------------------------------------------------------------

;;; efs has full support, including tree dired support, for hosts running
;;; CMS.  It should be able to automatically recognize any CMS machine.
;;; We would be grateful if you would report any failures to automatically
;;; recognize a CMS host as a bug.
;;; 
;;; Filename syntax:
;;;
;;; KNET/VM Support (J. M. Schwarz, Mar 12, 1994):
;;; This code has been developed and tested with 
;;; "KNET/VM FTP server Release 3.2.0" by Spartacus.
;;;
;;; This server uses not only a different listing format than the one used in
;;; efs-cms.el, but also handles minidisks differently. 
;;; The cd command for changing minidisk is not supported, 
;;; instead a full filename syntax "FILENAME.FILETYPE.FM" is used, where
;;; FM is the filemode. To access a file "PROFILE EXEC A0", efs uses a
;;; syntax "/cms-hostname:/A:/PROFILE.EXEC"   (Note the ':')
;;; 
;;; In this directory notation, "/A0:" is actually a subset of the "/A:"
;;; directory.

(efs-defun efs-send-pwd cms-knet (host user &optional xpwd)
  ;; cms-knet has no concept of current directory.
  ;; Is it safe to always assume this is the user's home?
  (cons "A" ""))

(efs-defun efs-fix-path cms-knet (path &optional reverse)
  ;; Convert PATH from UNIX-ish to CMS. If REVERSE is given, convert
  ;; from CMS to UNIX. Actually, CMS doesn't have a full pathname syntax,
  ;; so we fudge things by sending cd's.
  (if reverse
      ;; Since we only convert output from a pwd in this direction,
      ;; this should never be applied, as PWD doesn't work for this server.
      (concat "/" path "/")
    (efs-save-match-data
      (if (string-match "^/[A-Z]/\\([-A-Z0-9$_+@:]+\\.[-A-Z0-9$_+@:]+\\)$"
			path)
	  (concat
	   (substring path (match-beginning 1) (match-end 1))
	   "."
	   ;; minidisk
	   (substring path 1 2))
	(error "Invalid CMS-KNET filename")))))

(efs-defun efs-fix-dir-path cms-knet (dir-path)
  ;; Convert path from UNIX-ish to CMS-KNET ready for a DIRectory listing.
  (cond
   ((string-equal "/" dir-path)
    "*.*.*")
   ((string-match
     "^/[A-Z]/\\([-A-Z0-9$._+@:]+\\.[-A-Z0-9$._+@:]+\\)?$"
     dir-path)
    (concat 
     (if (match-beginning 1)
	 (substring dir-path (match-beginning 1) (match-end 1))
       "*")
     "."
     (substring dir-path 1 2)))
   (t (error "Invalid CMS-KNET pathname"))))

(defconst efs-cms-knet-file-name-regexp
  (concat
   "^  *\\([-A-Z0-9$_+@:]+\\) +\\([-A-Z0-9$_+@:]+\\) +"
   "\\([A-Z]\\)[0-9] +[VF] +[0-9]+ "))

(efs-defun efs-parse-listing cms-knet
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a CMS directory listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory as a full efs-path
  (let ((tbl (efs-make-hashtable)))
    (goto-char (point-min))
    (efs-save-match-data
      (if (string-equal dir "/")
	  (let ((case-fold (memq 'cms-knet efs-case-insensitive-host-types))
		tbl-alist md md-tbl)
	    (while (re-search-forward efs-cms-knet-file-name-regexp nil t)
	      (setq md (buffer-substring (match-beginning 3) (match-end 3))
		    md-tbl (or (cdr (assoc md tbl-alist))
			       (let ((new-tbl (efs-make-hashtable)))
				 (setq tbl-alist
				       (cons (cons md new-tbl)
					     tbl-alist))
				 new-tbl)))
	      (efs-put-hash-entry md '(t) tbl)
	      (efs-put-hash-entry (concat
				   (buffer-substring (match-beginning 1)
						     (match-end 1))
				   "."
				   (buffer-substring (match-beginning 2)
						     (match-end 2)))
				  '(nil) md-tbl)
	      (forward-line 1))
	    (while tbl-alist
	      (setq md (car (car tbl-alist))
		    md-tbl (cdr (car tbl-alist)))
	      (efs-put-hash-entry "." '(t) md-tbl)
	      (efs-put-hash-entry ".." '(t) md-tbl)
	      (efs-put-hash-entry (concat path md "/") md-tbl
				  efs-files-hashtable case-fold)
	      (setq tbl-alist (cdr tbl-alist))))
	(while (re-search-forward efs-cms-knet-file-name-regexp nil t)
	  (efs-put-hash-entry
	   (concat (buffer-substring (match-beginning 1)
				     (match-end 1))
		   "."
		   (buffer-substring (match-beginning 2)
				     (match-end 2)))
	   '(nil) tbl)
	  (forward-line 1)))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl))
    tbl))

(efs-defun efs-allow-child-lookup cms-knet (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  
  ;; CMS file system is flat. Only minidisks are "subdirs".
  (string-equal "/" dir))

;;; Tree dired support:

(defconst efs-dired-cms-re-exe
  "^. +[-A-Z0-9$_+@:]+ +\\(EXEC\\|MODULE\\) "
  "Regular expression to use to search for CMS executables.")

(or (assq 'cms efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'cms-knet efs-dired-cms-re-exe)
		efs-dired-re-exe-alist)))

(efs-defun efs-dired-insert-headerline cms-knet (dir)
  ;; CMS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename cms-knet
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; This is the CMS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (re-search-forward efs-cms-knet-file-name-regexp eol t)
	(goto-char (match-beginning 1))
      (if raise-error
	  (error "No file on this line.")
	(goto-char bol)))))

(efs-defun efs-dired-manual-move-to-end-of-filename cms-knet
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the CMS version.
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
  (if (looking-at "[-A-Z0-9$_+@:]+ +[-A-Z0-9$_+@:]+ +[A-Z][0-9] ")
      (goto-char (- (match-end 0) 2)) ; return point
    (if no-error
	nil
      (error "No file on this line."))))

(efs-defun efs-dired-get-filename cms-knet
  (&optional localp no-error-if-not-filep)
  (let ((name (efs-real-dired-get-filename 'no-dir no-error-if-not-filep)))
    (and name
	 (if (string-match
	      "^\\([-A-Z0-9$_+@:]+\\) +\\([-A-Z0-9$_+@:]+\\) +\\([A-Z]\\)$"
	      name)
	     (let* ((dir (dired-current-directory))
		    (rdir (nth 2 (efs-ftp-path dir))))
	       (setq name (concat (substring name (match-beginning 1)
					     (match-end 1))
				  "."
				  (substring name (match-beginning 2)
					     (match-end 2))))
	       (if (string-equal rdir "/")
		   (setq name (concat (substring name (match-beginning 3)
						 (match-end 3)) "/" name)))
	       (if (eq localp 'no-dir)
		   name
		 (concat (if localp
			     (dired-current-directory localp)
			   dir)
			 name)))
	   (error "Strange CMS-KNET file name %s" name)))))

;;; end of efs-cms-knet.el
