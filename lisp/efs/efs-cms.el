;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-cms.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.9 $
;; RCS:          
;; Description:  CMS support for efs
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Fri Oct 23 08:52:00 1992
;; Modified:     Sun Nov 27 11:46:51 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-cms)
(require 'efs)

(defconst efs-cms-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.9 $" 11 -2)))

;;;; ------------------------------------------------------------
;;;; CMS support
;;;; ------------------------------------------------------------

;;; efs has full support, including tree dired support, for hosts running
;;; CMS.  It should be able to automatically recognize any CMS machine.
;;; We would be grateful if you would report any failures to automatically
;;; recognize a CMS host as a bug.
;;; 
;;; This should also work with CMS machines running SFS (Shared File System).
;;; 
;;; Filename syntax:
;;;
;;; CMS filenames are entered in a UNIX-y way. In otherwords, minidisks are
;;; treated as UNIX directories. For example to access the file READ.ME in
;;; minidisk *.311 on cuvmb.cc.columbia.edu, you would enter
;;;   /anonymous@cuvmb.cc.columbia.edu:/*.311/READ.ME
;;; If *.301 is the default minidisk for this account, you could access
;;; FOO.BAR on this minidisk as
;;;   /anonymous@cuvmb.cc.columbia.edu:FOO.BAR
;;; CMS filenames are of the form FILE.TYPE, where both FILE and TYPE can be
;;; up to 8 characters. Again, beware that CMS filenames are always upper
;;; case, and hence must be entered as such.
;;;
;;; Tips:
;;; 1. CMS machines, with the exception of anonymous accounts, nearly always
;;;    need an account password. To have efs send an account password,
;;;    you can either include it in your .netrc file, or use
;;;    efs-set-account.
;;; 2. efs-set-account can be used to set account passwords for specific
;;;    minidisks. This is usually used to optain write access to the minidisk.
;;;    As well you can put tokens of the form
;;;    minidisk <minidisk name> <password> in your .netrc file. There can be
;;;    as many minidisk tokens as you like, however they should follow all
;;;    other tokens for a given machine entry. Of course, ordinary ftp
;;;    will not understand these entries in your .netrc file.
;;;


;;; Since CMS doesn't have any full pathname syntax, we have to fudge
;;; things with cd's. We actually send too many cd's, but is dangerous
;;; to try to remember the current minidisk, because if the connection
;;; is closed and needs to be reopened, we will find ourselves back in
;;; the default minidisk. This is fairly likely since CMS ftp servers
;;; usually close the connection after 5 minutes of inactivity.

;;; Have I got the filename character set right?

;;; The following three functions are entry points to this file.
;;; They have been added to the appropriate alists in efs.el

(efs-defun efs-fix-path cms (path &optional reverse)
  ;; Convert PATH from UNIX-ish to CMS. If REVERSE is given, convert
  ;; from CMS to UNIX. Actually, CMS doesn't have a full pathname syntax,
  ;; so we fudge things by sending cd's.
  (efs-save-match-data
    (if reverse
	(if (string-match ":" path)
	    ;; It's SFS
	    (let* ((start (match-end 0))
		   (return (concat "/" (substring path 0 start))))
	      (while (string-match "\\." path start)
		(setq return (concat return "/"
				     (substring path start
						(match-beginning 0)))
		      start (match-end 0)))
	      (concat return "/" (substring path start)))
	  ;; Since we only convert output from a pwd in this direction,
	  ;; we'll assume that it's a minidisk, and make it into a
	  ;; directory file name. Note that the expand-dir-hashtable
	  ;; stores directories without the trailing /.
	  (if (char-equal (string-to-char path) ?/)
	      path
	    (concat "/" path)))
      (if (let ((case-fold-search t))
	    (string-match
	     (concat
	      "^/\\([-A-Z0-9$*._+:]+\\)/"
	      ;; In case there is a SFS
	      "\\(\\([-A-Z0-9$*._+]+\\)/\\([-A-Z0-9$*._+]+/\\)?\\)?"
	      "\\([-A-Z0-9$._+]+\\)$")
	     path))
	  (let ((minidisk (substring path 1 (match-end 1)))
		(sfs (and (match-beginning 2)
			  (substring path (match-beginning 3)
				     (match-end 3))))
		(file (substring path (match-beginning 5) (match-end 5)))
		account)
	    (and sfs (match-beginning 4)
		 (setq sfs (concat sfs "." (substring path (match-beginning 4)
						      (1- (match-end 4))))))
	    (unwind-protect
		(progn
		  (or sfs
		      (setq account
			    (efs-get-account host user minidisk)))
		  (efs-raw-send-cd host user (if sfs
						 (concat minidisk sfs ".")
					       minidisk))
		  (if account
		      (efs-cms-send-minidisk-acct
		       host user minidisk account)))
	      (if account (fillarray account 0)))
	    file)
	(error "Invalid CMS filename")))))

(efs-defun efs-fix-dir-path cms (dir-path)
  ;; Convert path from UNIX-ish to VMS ready for a DIRectory listing.
  (efs-save-match-data
    (cond
     ((string-equal "/" dir-path)
      (error "Cannot get listing for CMS \"/\" directory."))
     ((let ((case-fold-search t))
	(string-match
	 (concat "^/\\([-A-Z0-9$*._+:]+\\)/"
		 "\\(\\([-A-Z0-9$*._+]+\\)/\\([-A-Z0-9$*._+]+/\\)?\\)?"
		 "\\([-A-Z0-9$*_.+]+\\)?$") dir-path))
      (let ((minidisk (substring dir-path (match-beginning 1) (match-end 1)))
	    (sfs (and (match-beginning 2)
		      (concat
		       (substring dir-path (match-beginning 3)
				  (match-end 3)))))
	    (file (if (match-beginning 5)
		      (substring dir-path (match-beginning 5) (match-end 5))
		    "*"))
	    account)
	(and sfs (match-beginning 4)
	     (setq sfs (concat sfs "." (substring dir-path
						  (match-beginning 4)
						  (1- (match-end 4))))))
	(unwind-protect
	    (progn
	      (or sfs
		  (setq account (efs-get-account host user minidisk)))
	      (efs-raw-send-cd host user (if sfs
					     (concat minidisk sfs ".")
					     minidisk))
	      (if account
		  (efs-cms-send-minidisk-acct host user minidisk account)))
	  (if account (fillarray account 0)))
	file))
     (t (error "Invalid CMS pathname")))))

(defconst efs-cms-file-line-regexp
  (concat
   "\\([-A-Z0-9$_+]+\\) +"
   "\\(\\(\\([-A-Z0-9$_+]+\\) +[VF] +[0-9]+ \\)\\|\\(DIR +- \\)\\)"))

(efs-defun efs-parse-listing cms
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a CMS directory listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory as a full efs-path
  (let ((tbl (efs-make-hashtable))
	fn dir-p)
    (goto-char (point-min))
    (efs-save-match-data
      (while (re-search-forward efs-cms-file-line-regexp nil t)
	(if (match-beginning 3)
	    (setq fn (concat (buffer-substring
			      (match-beginning 1) (match-end 1))
			     "."
			     (buffer-substring
			      (match-beginning 4) (match-end 4)))
		  dir-p nil)
	  (setq fn (buffer-substring (match-beginning 1) (match-end 1))
		dir-p t))
	(efs-put-hash-entry fn (list dir-p) tbl)
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl))
    tbl))

(defun efs-cms-send-minidisk-acct (host user minidisk account
					&optional noretry)
  "For HOST and USER, send the account password ACCOUNT. If MINIDISK is given,
the account password is for that minidisk. If PROC is given, send to that
process, rathr than use HOST and USER to look up the process."
  (efs-save-match-data
    (let ((result (efs-raw-send-cmd
		   (efs-get-process host user)
		   (concat "quote acct " account))))
      (cond
       ((eq (car result) 'failed)
	(setq account nil)
	(unwind-protect
	    (progn
	      (setq
	       account
	       (read-passwd
		(format
		 "Invalid acct. password for %s on %s@%s. Try again: "
		 minidisk user host)))
	      (if (string-equal "" account)
		  (setq account nil)))
	  ;; This guarantees that an interrupt will clear the account
	  ;; password.
	  (efs-set-account host user minidisk account))
	(if account ; give the user another chance
	    (efs-cms-send-minidisk-acct host user minidisk account)))
       ((eq (car result) 'fatal)
	(if noretry
	    ;; give up
	    (efs-error host user
		       (concat "ACCOUNT password failed: " (nth 1 result)))
	  ;; try once more
	  (efs-cms-send-minidisk-acct host user minidisk account t))))
      ;; return result
      result)))

(efs-defun efs-write-recover cms
  (line cont-lines host user cmd msg pre-cont cont nowait noretry)
  ;; If a write fails because of insufficient privileges, give the user a
  ;; chance to send an account password.
  (let ((cmd0 (car cmd))
	(cmd1 (nth 1 cmd))
	(cmd2 (nth 2 cmd)))
    (efs-save-match-data
      (if (and (or (memq cmd0 '(append put rename))
		   (and (eq cmd0 'quote) (eq cmd1 'stor)))
	       (string-match "^/\\([-A-Z0-9$*._+]+\\)/[-A-Z0-9$*._+]+$" cmd2))
	  (let ((minidisk (substring cmd2 (match-beginning 1) (match-end 1)))
		account retry)
	    (unwind-protect
		(progn
		  (setq account
			(read-passwd
			 (format "Account password for minidisk %s on %s@%s: "
				 minidisk user host)))
		  (if (string-equal account "")
		      (setq account nil)))
	      (efs-set-account host user minidisk account))
	    (if account
		(progn
		  (efs-cms-send-minidisk-acct host user minidisk account)
		  (setq retry
			(efs-send-cmd host user cmd msg pre-cont cont
				      nowait noretry))
		  (and (null (or cont nowait)) retry))
	      (if cont
		  (progn
		    (efs-call-cont cont 'failed line cont-lines)
		    nil)
		(and (null nowait) (list 'failed line cont-lines)))))
	(if cont
	    (progn
	      (efs-call-cont cont 'failed line cont-lines)
	      nil)
	  (and (null nowait) (list 'failed line cont-lines)))))))

(efs-defun efs-allow-child-lookup cms (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  
  ;; CMS file system is flat. Only minidisks are "subdirs".
  (or (string-equal "/" dir)
      (efs-save-match-data
	(string-match "^/[^/:]+:/$" dir))))

;;; Sorting listings

(defconst efs-cms-date-and-time-regexp
  (concat
   " \\(1?[0-9]\\)/\\([0-3][0-9]\\)/\\([0-9][0-9]\\) +"
   "\\([12]?[0-9]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\) "))

(efs-defun efs-t-converter cms (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-cms-date-and-time-regexp nil t)
	  (let (list-start list bol nbol)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (progn
		     (setq bol (point))
		     (re-search-forward efs-cms-date-and-time-regexp
					(setq nbol (save-excursion
						     (forward-line 1) (point)))
					t))
	      (setq list
		    (cons
		     (cons
		      (list (string-to-int (buffer-substring
					    (match-beginning 3)
					    (match-end 3))) ; year
			    (string-to-int (buffer-substring
					    (match-beginning 1)
					    (match-end 1))) ; month
			    (string-to-int (buffer-substring
					    (match-beginning 2)
					    (match-end 2))) ; day
			    (string-to-int (buffer-substring
					    (match-beginning 4)
					    (match-end 4))) ; hour
			    (string-to-int (buffer-substring
					    (match-beginning 5)
					    (match-end 5))) ; minutes
			    (string-to-int (buffer-substring
					    (match-beginning 6)
					    (match-end 6)))) ; seconds
		      (buffer-substring bol nbol))
		     list))
	      (goto-char nbol))
	    (if list
		(progn
		  (setq list
			(mapcar 'cdr
				(sort list 'efs-cms-t-converter-sort-pred)))
		  (if reverse (setq list (nreverse list)))
		  (delete-region list-start (point))
		  (apply 'insert list)))
	    t)))))

(defun efs-cms-t-converter-sort-pred (elt1 elt2)
  (let* ((data1 (car elt1))
	 (data2 (car elt2))
	 (year1 (car data1))
	 (year2 (car data2))
	 (month1 (nth 1 data1))
	 (month2 (nth 1 data2))
	 (day1 (nth 2 data1))
	 (day2 (nth 2 data2))
	 (hour1 (nth 3 data1))
	 (hour2 (nth 3 data2))
	 (minute1 (nth 4 data1))
	 (minute2 (nth 4 data2))
	 (second1 (nth 5 data1))
	 (second2 (nth 5 data2)))
    (or (> year1 year2)
	(and (= year1 year2)
	     (or (> month1 month2)
		 (and (= month1 month2)
		      (or (> day1 day2)
			  (and (= day1 day2)
			       (or (> hour1 hour2)
				   (and (= hour1 hour2)
					(or (> minute1 minute2)
					    (and (= minute1 minute2)
						 (or (> (nth 5 data1)
							(nth 5 data2)))
						 ))))))))))))


;;; Tree dired support:

(defconst efs-dired-cms-re-exe "^. [-A-Z0-9$_+]+ +EXEC ")

(or (assq 'cms efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'cms  efs-dired-cms-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-cms-re-dir "^. [-A-Z0-9$_+]+ +DIR ")

(or (assq 'cms efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'cms  efs-dired-cms-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline cms (dir)
  ;; CMS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (efs-real-dired-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename cms
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; This is the CMS version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (re-search-forward efs-cms-file-line-regexp eol t)
	(goto-char (match-beginning 0))
      (goto-char bol)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename cms
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the CMS version.
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
    (skip-chars-forward "-A-Z0-9$_+")
    (or (looking-at " +DIR ")
	(progn
	  (skip-chars-forward " ")
	  (skip-chars-forward "-A-Z0-9$_+")))
    (if (or (= opoint (point)) (/= (following-char) ?\ ))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-make-filename-string cms (filename &optional reverse)
  (if reverse
      (if (string-match "\\." filename)
	  ;; Can't count on the number of blanks between the base and the
	  ;; extension, so ignore the extension.
	  (substring filename 0 (match-beginning 0))
	filename)
    (if (string-match "^\\([^ ]+\\) +\\([^ ]+\\)$" filename)
	(concat (substring filename 0 (match-end 1))
		"."
		(substring filename (match-beginning 2) (match-end 2)))
      filename)))

;;; end of efs-cms.el
