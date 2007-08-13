;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-mpe.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.8 $
;; RCS:          
;; Description:  MPE (HP3000) support for efs.
;; Author:       (Corny de Souza) cdesouza@hpbbn.bbn.hp.com
;; Created:      Fri Jan 15 12:58:29 1993
;; Modified:     Sun Nov 27 18:36:13 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Credits 
;;
;; Sandy Rutherford for his help and advice.

;;; Usage
;;
;; For a general description of remote file access see efs.el.
;;
;; MPE Specifics
;;
;; *) To make things easier (for me) MPE has been UNIXified so think UNIX 
;;    and you stand a good chance of understanding everything.
;;
;; *) Filename syntax is as follows
;;
;;    /session,user.account,group@system:/account/group/file;buildparms
;;    
;;    the "session," and ",group" in the logon sequence are optional.
;;
;;    e.g. /CDSUSER.OSCAR@SYSTEM41:/OSCAR/CDSSRC/TST0000S
;;    will get the file TST0000S.CDSSRC.OSCAR
;;
;;    The ";buildparms" is also optional. It should be used when creating
;;    files whos characteristics differ from the default system buildparms,
;;    described in the file FTPDOC.ARPA.SYS (at least it is on my system).
;;    Also see variable efs-mpe-default-buildparms.
;;
;;    e.g. REC=-256,,V,ASCII
;;
;; *) Password syntax is as follows
;;    
;;    userpass,accountpass,grouppass
;;
;;    Leading commas cannot be omitted, trailing commas can.
;;    e.g. USERPASS,ACCTPASS   (no group password)
;;         ,ACCTPASS           (only account password)
;;         USERPASS,,GRPPASS   (no account password)
;;
;; *) Do not use account name completion on large systems. See the variable
;;    efs-mpe-account-completion-confirm 
;;
;; *) Do not use group name completion on large accounts. See the variable
;;    efs-mpe-group-completion-confirm
;;
;; *) The buffers FILE and FILE;BUILDPARMS both point to the same physical
;;    disc file.
;;  
;; *) When using filename completion you will usually be given the option
;;    between FILE and FILE;BUILDPARMS. Just ignore the FILE;BUILDPARMS
;;    bit.
;;
;; *) WARNING ********* Two buffer for the same file ************ WARNING
;;    If you land up with two buffers FILE and FILE;BUILDPARMS for the same
;;    file kill the FILE;BUILDPARMS one. If however this is newwer than
;;    the FILE buffer (and you cannot live with a buffer called 
;;    FILE;BUILDPARMS) save it kill both buffers and get the FILE buffer again.
;;
;; *) When creating new files only create FILES. It is possible to create
;;    files as GROUPs and ACCOUNTs but don't!  
;;
;;; To Do
;;
;; A lot of things are likely to change with MPE 4.5 and POSIX so I do not want
;; to invest too much time in this now. I would rather wait until I can see
;; what comes with POSIX.
;;
;; Feel free to send bugs, suggestions for enhancements and enhancements
;; to me cdesouza@hpbbn.bbn.hp.com. If I have TIME I will try to deal with
;; them. Also I'm not a lisp programmer so keep it simple or put in plenty
;; of comments.
;;
;;
;; *) Improve on the dired GROUP and ACCOUNT listings.
;;
;; *) Add ".." to dired FILE and GROUP listings.
;;
;; *) Support POSIX (need POSIX machine first though).
;;
;; *) Test ACCOUNT name completion and listings properly. I have the problem
;;    that the only systems available to me are large ( i.e. start a listf
;;    @.@.@,2  today and come back tomorrow), which makes
;;    it pretty hard for me to test.
;;

;;; Code

(provide 'efs-mpe)
(require 'efs)

;;; User Variables

(defvar efs-mpe-account-completion-confirm t
  "*Set to non-nil will cause a prompt to be issued before attempting ACCOUNT
name completion. For ACCOUNT name completion a LISTF @.@.@,2 is required. 
This can take a very long time on large systems")

(defvar efs-mpe-group-completion-confirm t
  "*Set to non-nil will cause a prompt to be issued before attempting GROUP
name completion. For GROUP name completion a LISTF @.@.ACCOUNT,2 is required. 
This can take a very long time on large accounts")

(defvar efs-mpe-default-buildparms ""
  "*If set to non empty string used to override the system default buildparms.")

;;; Internal Variables

(defconst efs-mpe-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.8 $" 11 -2)))

;;; Support for build parameters

(defun efs-mpe-get-buildparms (path)
  ;; Gets the mpe buildparms for PATH. PATH should be in efs syntax.
  (let ((files (efs-get-files-hashtable-entry (file-name-directory
					       (directory-file-name path)))))
    (if files
	(let* ((file (efs-get-file-part path))
	       (completion-ignore-case
		(memq 'mpe efs-case-insensitive-host-types))
	       (bpversions (all-completions (concat file ";") files)))
	  (cond
	   ((null bpversions)
	    efs-mpe-default-buildparms)
	   ((= (length bpversions) 1)
	    (substring (car bpversions) (length file)))
	   (t
	    (error
	     "efs-mpe: %s seems to have more than one set of buildparams."
	     path))))
      ;; return the default
      efs-mpe-default-buildparms)))
	
(defun efs-mpe-fix-buildparms (buildparms host user path)
  "Try to assign buildparms for the file being PUT"
  (or
   ;; Buildparms specified with file use them.
   buildparms
   (efs-mpe-get-buildparms (format efs-path-format-string user host path))))

;;; entry points

(efs-defun efs-fix-path mpe (path &optional reverse)
  ;; Convert PATH from UNIX-ish to MPE. If REVERSE given then convert from
  ;; MPE to UNIX-ish. N.B. Path does not contain HOST or USER part so the
  ;; dynamic variables HOST and USER are used.
  ;; Also uses the dynamic variable CMD0.
  (efs-save-match-data
    (if reverse
	;; This is never used as we only convert PWD (see below) output in
	;; this direction. However I will leave this here should it be
	;; required in the future.
	(if (let ((case-fold-search t))
	      (string-match 
	       (concat "^\\([A-Z][A-Z0-9]*\\)"    ; file
		       "\\(.[A-Z][A-Z0-9]*\\)"    ; group 
		       "\\(.[A-Z][A-Z0-9]*\\)$")  ; account
	       path))
	    (let (file group account)
	      (setq file (substring path 0 (match-end 1))) 
	      (if (match-beginning 2) 
		  (setq group (substring 
			       path (1+ (match-beginning 2)) (match-end 2))))
	      (if (match-beginning 3)
		  (setq account (substring 
				 path (1+ (match-beginning 3))
				 (match-end 3))))
	      (concat (and account (concat "/" account "/"))
		      (and group (concat group "/"))
		      file))
	  ;; handle PWD output
	  (if (let ((case-fold-search t))
		(string-match
		 (concat
		  "\\([A-Z][A-Z0-9]*\\)?"                  ; sessionname
		  ",[A-Z][A-Z0-9]*\.\\([A-Z][A-Z0-9]*\\)," ; username.account
		  "\\([A-Z][A-Z0-9]*\\)$")                 ; group
		 path))
	      (concat "/" 
		      (substring path (match-beginning 2) (match-end 2))
		      "/"
		      (substring path (match-beginning 3) (match-end 3))
		      "/")
	    (error "Invalid MPE (MPE->UNIX) filename: %s" path)))
      (if (let ((case-fold-search t))
	    (string-match 
	     (concat 
	      "^\\(/[A-Z][A-Z0-9]*/\\)"     ; account
	      "\\([A-Z][A-Z0-9]*/\\)"       ; group
	      "\\([A-Z][A-Z0-9]*\\)"        ; file 
	      "\\(;.*\\)?$")                ; buildparms
	     path))
	  (let ((for-put (and (boundp 'cmd0) (eq cmd0 'put)))
		file group account buildparms) 
	    (setq account (substring 
			   path (1+ (match-beginning 1)) (1- (match-end 1))))
	    (setq group (substring 
			 path (match-beginning 2) (1- (match-end 2))))
	    (setq file (substring path (match-beginning 3) (match-end 3))) 
	    (if for-put
		(setq buildparms 
		      (efs-mpe-fix-buildparms
		       (and (match-beginning 4)
			    (substring path
				       (match-beginning 4) (match-end 4)))
		       host user path)))
	    (concat file
		    (and group (concat "." group ))
		    (and account (concat "." account ))
		    (and for-put buildparms)))
	(error "Invalid MPE (UNIX->MPE) filename: *%s*" path)))))

(efs-defun efs-fix-dir-path mpe (dir-path)
  ;; Convert path from UNIX-ish to MPE ready for a DIRectory listing. MPE does
  ;; not have directories as such. It does have GROUPS and ACCOUNTS, but the 
  ;; DIR command does not let you list just ACCOUNTs on the system or just 
  ;; GROUPs in the ACCOUNT - no you always get everything downwards
  ;; i.e. ACCOUNTs + GROUPs + FILEs or GROUPs + FILEs or just FILEs
  ;; depending on the level.
  (efs-save-match-data
    (message "Fixing listing %s ..." dir-path)
    (cond
     ;; Everything !?! might take a while.
     ((string-equal dir-path "/")
      (if efs-mpe-account-completion-confirm
          (if (y-or-n-p "Continue with ACCOUNT name completion? ")
              "@.@.@"
            (error "Quit ACCOUNT name completion"))
          "@.@.@"))
     ;; specification starts with account
     ((let ((case-fold-search t))
	(string-match 
	 (concat 
	  "^\\(/[A-Z][A-Z0-9]*/\\)"     ; account
	  "\\([A-Z][A-Z0-9]*/\\)?"      ; group
	  "\\([A-Z][A-Z0-9]*\\)?"       ; file 
	  "\\(;.*\\)?/?$")              ; buildparms
	 dir-path))
      (let (file group account)
        (setq account (substring dir-path 
                                 (1+ (match-beginning 1)) (1- (match-end 1))))
        (if (match-beginning 2)
            (setq group (substring dir-path
                                   (match-beginning 2) (1- (match-end 2))))
          (if efs-mpe-group-completion-confirm
              (if (y-or-n-p "Continue with GROUP name completion? ")
                  (setq group "@")
                (error "Quit GROUP name completion"))
            (setq group "@")))
        (if (match-beginning 3)
            ;;(setq file (substring dir-path
            ;;                      (match-beginning 3) (1- (match-end 3))))
            ;; set the filename to something silly so that the DIR will fail
            ;; and so force a DIR for the group instead. Either I've
            ;; misunderstood something or you have to do it like this.  
            (setq file "~!#&*")
          (setq file "@"))
        (concat file "." group "." account)))
     (t
      (error "Invalid MPE (LISTF) filename: %s" dir-path)))))

(defconst efs-mpe-acct-grp-line-regexp
  "ACCOUNT= +\\([A-Z][A-Z0-9]*\\) +GROUP= +\\([A-Z][A-Z0-9]*\\)")
(defconst efs-mpe-file-line-regexp
  (concat
   "\\*?  +\\([A-Z0-9]*\\) +\\([0-9]+\\)"
   "\\([BW]\\) +\\([FV]\\)\\([AB]\\)\\([MCO]?\\) +\\([0-9]+\\)"))

(efs-defun efs-parse-listing mpe
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be in
  ;; mpe ftp dir format.
  ;; HOST is the name of the remote host.
  ;; USER is the user name.
  ;; DIR is the directory as a full remote path
  ;; PATH is the directory in full efs-syntax
  ;; SWITCHES are the switches passed to ls (not relevant for mpe)
  (goto-char (point-min))
  (efs-save-match-data
    ;;Make sure this is a valid listing 
    (if (re-search-forward "ACCOUNT= +[A-Z]+ +GROUP=" nil t)
        (let (acct-tbl grp-tbl file-tbl
              account group file
              acct-cur grp-cur)
          (goto-char (point-min))
          ;; Look for something that could be a filename.
          (while (re-search-forward "^[A-Z][A-Z0-9]*" nil t)
            (goto-char (match-beginning 0))
            ;; Check to see if looking at an ACCOUNT= GROUP= line. Could
            ;; be a continuation (cont). line or a change in account or group
            (if (looking-at efs-mpe-acct-grp-line-regexp)
                (progn 
                  (setq account (buffer-substring (match-beginning 1)
                                                  (match-end 1)))
                  (setq group (buffer-substring (match-beginning 2)
                                                (match-end 2)))
                  ;;Check for change of account
                  (if (not (string-equal acct-cur account))
                      (progn
                        ;;Create table for account names and fill with 
                        ;; "." entry.
                        (if (not acct-tbl)
                            (progn
                              (setq acct-tbl (efs-make-hashtable))
                              (efs-put-hash-entry "." '(t) acct-tbl)))
                        (efs-put-hash-entry account '(t) acct-tbl)
                        ;;Store the current group table
                        (if grp-tbl
                            (progn 
                              (efs-set-files
			       (efs-replace-path-component
				path
				(concat  "/" acct-cur "/"))
                               grp-tbl )
                              (setq grp-tbl nil)))))
                  ;;Check for change in group. Change in account is automatic
                  ;;change in group.
                  (if (or (not (string-equal acct-cur account))
                          (not (string-equal grp-cur group)))
                      (progn 
                        ;;Create table for group names and fill with 
                        ;; "." and ".." entries.
                        (if (not grp-tbl)
                            (progn
                              (setq grp-tbl (efs-make-hashtable))
                              (efs-put-hash-entry "." '(t) grp-tbl)
                              (efs-put-hash-entry ".." '(t) grp-tbl))) 
                        (efs-put-hash-entry group '(t) grp-tbl)
                        ;;Store current file table
                        (if file-tbl
                            (progn 
                              (efs-set-files
			       (efs-replace-path-component
				path
				(concat "/" acct-cur "/" grp-cur "/"))
                               file-tbl)
                              (setq file-tbl nil)))))
                  ;;Set new grp-cur and acct-cur incase one or both chnaged.
                  (setq grp-cur group acct-cur account)
                  )
              ;;Looking at either a file name, or the line 
              ;;"FILENAME      CODE --....--LOGICAL.."
              ;;Save the possible filename.
              (setq file (buffer-substring (point) 
                                           (progn 
                                             (skip-chars-forward "A-Z0-9")
                                             (point))))
              ;;Make sure its a file name.
              ;;"\\*?" is for files in access.
              ;; File codes can be numeric as well! CdS
              (if (looking-at efs-mpe-file-line-regexp)
                  ;;Hack out the buildparms
                  (let* ((code (and 
                               (/= (match-beginning 1) (match-end 1))
                               (concat ";CODE=" 
                                       (buffer-substring 
                                        (match-beginning 1) (match-end 1)))))
                        (length (buffer-substring (match-beginning 2)
                                                  (match-end 2)))
                        (eof (buffer-substring (match-beginning 7)
                                                  (match-end 7)))
                        (bytes (* (string-to-int eof)
                                  (string-to-int length)))
                        (word-byte  (buffer-substring (match-beginning 3)
						      (match-end 3)))
                        (fix-var  (buffer-substring (match-beginning 4)
						    (match-end 4)))
                        (ascii-binary  (buffer-substring (match-beginning 5)
                                                  (match-end 5)))
                        (cir-msg (and (match-beginning 6)
                                      (buffer-substring (match-beginning 6)
                                                        (match-end 6))))
                        (rec ";REC="))
                    (if (string-equal word-byte "B")
                        (setq rec (concat rec "-"))
                      (setq bytes (* 2 bytes)))
                    (setq rec (concat rec length ",," fix-var ","))
                    (if (string-equal ascii-binary "A")
                        (setq rec (concat rec "ASCII"))
                      (setq rec (concat rec "BINARY")))
                    (cond ((string-equal cir-msg "M")
                           (setq cir-msg ";MSG"))
                          ((string-equal cir-msg "O")
                           (setq cir-msg ";CIR"))
                          (t
                           (setq cir-msg nil)))
                    (if (not file-tbl)
                        (progn 
                          (setq file-tbl (efs-make-hashtable))
                          (efs-put-hash-entry "." '(t) file-tbl)
                          (efs-put-hash-entry ".." '(t) file-tbl)))
                    (message "Adding... %s" file)
                    (efs-put-hash-entry file (list nil bytes) file-tbl)
                    (efs-put-hash-entry (concat file rec code cir-msg) 
                                             (list nil bytes) file-tbl)))
              ) ;if looking-at
            (forward-line 1)
            );while
          ;;Check at what level the listing was done and return the
          ;;corresponding table. System = acct-tbl, Account = grp-tbl,
          ;;Group = file-tbl.
          (if (let ((case-fold-search t))
		(string-match 
		 "\\(/\\)\\([A-Z0-9]+/\\)?\\([A-Z0-9]+/\\)?\\([A-Z0-9]+/\\)?" 
		 dir))
              ;;group level listing, just return table of files
              (if (or (match-beginning 3) (match-beginning 4))
                  file-tbl
                ;;account level listing, return table of groups but do not
                ;;forget to store current table of files. 
                (if (match-beginning 2)
                    (progn
                      (if file-tbl
                          (efs-set-files
			   (efs-replace-path-component
			    path
			    (concat "/" acct-cur "/" grp-cur "/"))
                           file-tbl))
                      grp-tbl)
                  ;;System level listing, return table of accounts but do not
                  ;;forget to store current table of groups and files
                  (if (match-beginning 1)
                      (progn
                        (if file-tbl
                          (efs-set-files
			   (efs-replace-path-component
			    path
			    (concat "/" acct-cur "/" grp-cur "/"))
			   file-tbl))
			(if grp-tbl
			    (efs-set-files
			     (efs-replace-path-component
			      path
			      (concat "/" acct-cur "/"))
			     grp-tbl))
                        acct-tbl)
                    (error "Parse listing 0 path %s" path))))
            (error "Parse listing 1 path %s" path))))))


(efs-defun efs-really-file-p mpe (file ent)
  ;; Doesn't treat the buildparm entry as a real file entry.
  (efs-save-match-data
    (not (string-match ";" file))))
  
(efs-defun efs-delete-file-entry mpe (path &optional dir-p)
  ;; Deletes FILE and FILE;BUILDPARMS from file hashtable.
  (let ((ignore-case (memq 'mpe efs-case-insensitive-host-types)))
    (if dir-p
	(let ((path (file-name-as-directory path))
	      files)
	  (efs-del-hash-entry path efs-files-hashtable ignore-case)
	  (setq path (directory-file-name path)
		files (efs-get-files-hashtable-entry
		       (file-name-directory path)))
	  (if files
	      (efs-del-hash-entry (efs-get-file-part path)
				  files ignore-case)))
      (let ((file (efs-get-file-part path))
	    (files (efs-get-files-hashtable-entry
		    (file-name-directory path))))
	(if files
	    (efs-save-match-data
	      (if (string-match ";" file)
		  (let ((root (substring file (match-beginning 0))))
		    ;; delete ROOT from hashtable
		    (efs-del-hash-entry root files ignore-case)
		    ;; delete ROOT;BUILDPARAMS from hashtable
		    (efs-del-hash-entry file files ignore-case))
		;; we've specified only a root.
		(let* ((root (concat file ";"))
		       (completion-ignore-case ignore-case)
		       (extensions (all-completions root files)))
		  ;; Get rid of FILE.
		  (efs-del-hash-entry file files ignore-case)
		  ;; Get rid of all BUILDPARAMS versions
		  (while extensions
		    ;; all-completions will return names with the right case.
		    ;; Don't need to ignore-case now.
		    (efs-del-hash-entry (car extensions) files)
		    (setq extensions (cdr extensions)))))))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-add-file-entry mpe (path dir-p size owner
					&optional modes nlinks mdtm)
  ;; Deletes FILE (if present) and FILE;BUILDPARMS (if present) from hashtable
  ;; then adds FILE and  FILE;BUILDPARMS (if specified) to hashtable.
  (let ((ignore-case (memq 'mpe efs-case-insensitive-host-types))
	(ent (let ((dir-p (null (null dir-p))))
	       (if mdtm
		   (list dir-p size owner nil nil mdtm)
		 (list dir-p size owner)))))

    (if dir-p
	(let* ((path (directory-file-name path))
	       (files (efs-get-files-hashtable-entry
		       (file-name-directory path))))
	  (if files
	      (efs-put-hash-entry (efs-get-file-part path) ent files
				  ignore-case)))
      
      (let ((files (efs-get-files-hashtable-entry
		    (file-name-directory path))))
	(efs-save-match-data
	  (if files
	      (let* ((file (efs-get-file-part path))
		     (root (substring file 0 (string-match ";" file))))
                (if (equal root file)
                    (setq file (concat file (efs-mpe-get-buildparms path))))
		;; In case there is another entry with different buildparams,
		;; wipe it.
		(efs-delete-file-entry 'mpe path nil)
		(efs-put-hash-entry root ent files ignore-case)
		(efs-put-hash-entry file ent files ignore-case))))))
    (efs-del-from-ls-cache path t ignore-case)))

(efs-defun efs-allow-child-lookup mpe (host user dir file)
  ;; Returns non-NIL if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted. Note that DIR is in directory syntax i.e. /foo/bar/, not
  ;; /foo/bar.

  ;; Subdirs in MPE are accounts or groups.
  (string-match "^/\\([^/]+/\\)?$" dir))
            
(efs-defun efs-file-type mpe (path)
  ;; Returns whether to treat an efs file as a text file or not.
  (let ((buildparams (efs-mpe-get-buildparms path)))
    (efs-save-match-data
      (let ((case-fold-search t))
	(cond
	 ((string-match "BINARY" buildparams)
	  '8-binary)
	 (t
	  'text))))))

;;; Tree dired support:

(efs-defun efs-dired-manual-move-to-filename mpe
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the MPE version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    ;; The "\\|ACCOUNT=\\|GROUP=" bit is to take care of the hacked account and
    ;; group dired listings.
    (if (looking-at
	 ". [A-Z][A-Z0-9]*\\*?  +\\([A-Z]* +[0-9]+\\|ACCOUNT=\\|GROUP=\\)")
	(progn
	  (forward-char 2)
	  (point))
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename mpe
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the MPE version.
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
    (skip-chars-forward "A-Z0-9")
    (if (or (= opoint (point)) (not (memq (following-char) '(?\  ?*))))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

(efs-defun efs-dired-ls-trim mpe ()
  ;; trim single file listings 1-line.
  ;; This uses an evil dynamical binding of file.
  (if (and (boundp 'file) (stringp file))
      (let ((f (file-name-nondirectory file)))
	(or (zerop (length f))
	    (progn
	      (goto-char (point-min))
	      (if (search-forward (concat "\n" (upcase file) " ") nil t)
		  (progn
		    (beginning-of-line)
		    (delete-region (point-min) (point))
		    (forward-line 1)
		    (delete-region (point) (point-max)))))))))

(efs-defun efs-dired-fixup-listing mpe (file path &optional switches wildcard)
  ;; File (group) listings stay pretty much as they are group (account) and
  ;; account (system) listings get realy hacked.
  (efs-save-match-data
    (goto-char (point-max))
    (string-match 
     "\\(/\\)\\([A-Z0-9]+/\\)?\\([A-Z0-9]+/\\)?\\([A-Z0-9]+/\\)?" 
     path)
    ;; group or file level listing.
    (if (or (match-beginning 3) (match-beginning 4))
        ;; Hack out the continuation lines.
        (while 
            (re-search-backward
             "\n\nACCOUNT=.+GROUP=.+(CONT\\.)\n\n.*\n.*\n" nil t)
          (replace-match "" nil nil))
        ;;account level listing, hack out everything apart from group names
      (if (match-beginning 2)
          (let ((group nil)
                (grp-cur nil))
            (while 
                (re-search-backward
                 "GROUP= +\\([A-Z][A-Z0-9]*\\)\\(.\\|\n\\)*"
                 nil t)
              (setq group 
                    (buffer-substring (match-beginning 1) (match-end 1)))
              ;;Continuation header or new group
              (if (string-equal grp-cur group)
                  (replace-match "" nil nil)
                (replace-match (format "\n\n%-10sGROUP=" group) nil nil))
              (forward-line -1)
                (setq grp-cur group)
                (narrow-to-region (point-min) (point)))
            (widen)
            (goto-char (point-max))
            (insert "\n\n"))
        ;;System level listing, hack out everything apart from account names
        (if (match-beginning 1)
            (let (account acct-cur)
              (while
                  (re-search-backward 
                     "^ACCOUNT= +\\([A-Z][A-Z0-9]*\\)\\(.\\|\n\\)*"
                     nil t)
                (setq account
                      (buffer-substring (match-beginning 1) (match-end 1)))
                ;;Continuation header or new account
                (if (string-equal acct-cur account)
                    (replace-match "" nil nil)
                    (replace-match (format "%-10sACCOUNT=" account) nil nil))
                (forward-line -1)
                (setq acct-cur account)
                (narrow-to-region (point-min) (point)))
              (widen)
              (goto-char (point-max))
              (insert "\n\n")))))))

;;; end of efs-mpe.el
