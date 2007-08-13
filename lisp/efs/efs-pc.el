;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-pc.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.2 $
;; RCS:          
;; Description:  PC support for efs
;; Author:       Sandy Rutherford <sandy@tsmi19.sissa.it>
;; Created:      Thu Mar 18 13:06:25 1993
;; Modified:     Sun Nov 27 18:40:46 1994 by sandy on gandalf
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;; Thanks to jrs@world.std.com (Rick Sladkey) for providing support for
;;; the Frontier Technologies Super-TCP server

;;; Many thanks to the following people for beta testing:
;;;      Mike Northam <mbn@hfglobe.intel.com>
;;;      bagman@austin.ibm.com (Doug Bagley)
;;;      Jens Petersen <J.Petersen@qmw.ac.uk>
;;;      Jeff Morgenthaler <jpmorgen@wisp4.physics.wisc.edu>

(provide 'efs-pc)
(require 'efs)

(defconst efs-pc-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;;-----------------------------------------------------------------
;;; PC support for efs
;;;-----------------------------------------------------------------

;;; Works for the DOS FTP servers:
;;; Novell LAN WorkPlace v4.01 (NetWare & EXOS)
;;; PC/TCP Version 2.05 pl2 FTP Server by FTP Software
;;; Microsoft FTP Server service (beta 2)
;;; NCSA DOS ftp server.
;;; Frontier Technologies super tcp server (runs under MS WINDOWS)
;;; Alun's Windows FTP daemon for Winsock, v1.8b
;;;
;;; Works for IBM OS/2 TCP/IP FTP Version 1.2

;;; Currently support for all of the above FTP servers are in this file.
;;; Should they live in separate files?

;;; host and listing type hierarchy in this file
;;;
;;; dos: dos:novell, dos:ftp, dos:ncsa, dos:microsoft, dos:stcp, dos:winsock
;;; os2:

;;; DOS and OS/2 have slightly different filename syntaxes.
;;;
;;; DOS only allows at most one extension (".") per filename.
;;; A directory name usually has the extension ".DIR" implicit, but
;;; it seems that other extensions can be used.
;;;
;;; OS/2 running the FAT file system uses the same 8.3 format for
;;; filenames as DOS, except that extensions are allowed in directory names.
;;; OS/2 running the HPFS (high performance file system allows an arbitrary
;;; number of extensions in a filename.
;;; Mostly these differences are unimportant here, except in the dos
;;; definition of efs-allow-child-lookup.

;;;; ----------------------------------------------------
;;;; Utility functions and macros
;;;; ----------------------------------------------------

(defun efs-fix-pc-path (path &optional reverse)
  ;; Convert PATH from UNIX-ish to DOS or OS/2.
  ;; If REVERSE do just that.
  (efs-save-match-data
    (if reverse
	(let ((n 0)
	      len res)
	  (if (string-match "^[a-zA-Z0-9]:" path)
	      ;; there's a disk
	    (setq res (concat "\\" path))
	    (setq res (copy-sequence path)))
	  (setq len (length res))
	  (while (< n len)
	    (and (= (aref res n) ?\\ ) (aset res n ?/))
	    (setq n (1+ n)))
	  res)
      (let ((n 0)
	    len res)
	(if (string-match "^/[a-zA-Z0-9]:" path)
	    (setq res (substring path 1))
	  (setq res (copy-sequence path)))
	(setq len (length res))
	(while (< n len)
	  (and (= (aref res n) ?/) (aset res n ?\\ ))
	  (setq n (1+ n)))
	res))))

(defmacro efs-dired-pc-move-to-end-of-filename (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the DOS and OS/2 version. It is common to all of the PC ftp
  ;; servers since it depends only on the file name character set.
  (`
   (let ((opoint (point)))
     (and selective-display
	  (null (, no-error))
	  (eq (char-after
	       (1- (or (, bol) (save-excursion
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
     (if (= opoint (point))
	 (if (, no-error)
	     nil
	   (error "No file on this line"))
       (point)))))

(defun efs-dired-pc-insert-headerline (dir)
  ;; Insert a blank line for aesthetics.
  (insert " \n")
  (forward-char -2)
  (efs-real-dired-insert-headerline dir))


;;;;-----------------------------------------------------------
;;;; General DOS support
;;;;-----------------------------------------------------------

;;; Regexps to be used for host and listing-type identification.

(defconst efs-dos:ftp-file-line-regexp
  (concat
   " *\\([0-9]+\\|<dir>\\) +\\([-_+=a-zA-Z0-9$~.]+\\)"
   " +\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) "
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|"
   "Oct\\|Nov\\|Dec\\) [0-3][0-9] "))

(defconst efs-dos:microsoft-file-line-regexp
  ;; matches all the way to the first char of the filename.
  (concat
   "[01][0-9]-[0-3][0-9]-[0-9][0-9] +[012][0-9]:[0-5][0-9][AP]M +"
   "\\(<DIR>\\|[0-9]+\\) +"))

(defconst efs-dos:ncsa-file-line-regexp
  "\\([-_+=a-zA-Z0-9$.~]+\\) +\\(<DIR>\\|[0-9]+\\)[ \n]")

(defconst efs-dos:stcp-file-line-regexp
  (concat
   "\\([-_+=a-zA-Z0-9$~.]+\\) +\\(<DIR>\\|[0-9]+\\) "
   "+[0-9][0-9]?-[0-3][0-9]-[12][90][0-9][0-9] +"
   "[0-9][0-9]?:[0-5][0-9]"))

(defconst efs-dos:winsock-date-and-size-regexp
  (concat
   " \\([0-9]+\\) "
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|"
   "Dec\\) [ 0-3][0-9] \\( [12][0-9][0-9][0-9]\\|[0-2][0-9]:[0-6][0-9]\\) +"))

(efs-defun efs-parse-listing dos
  (host user dir path &optional switches)
  ;; Examine the listing, which is assumed to be either a DOS or OS/2
  ;; listing, and determine the operating system type and FTP server.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  ;; No need to check for OS/2, as it gets ID'ed by a SYST in
  ;; efs-guess-host-type.
  (efs-save-match-data
    (cond

     ;; Check for the Microsoft server
     ((re-search-forward efs-dos:microsoft-file-line-regexp nil t)
      (efs-add-listing-type 'dos:microsoft host user)
      (efs-parse-listing 'dos:microsoft host user dir path switches))
     
     ;; Check for the Novell FTP server
     ((save-excursion
	(goto-char (point-max))
	(forward-line -1)
	(looking-at " [0-9]+ File(s)\n"))
      (efs-add-listing-type 'dos:novell host user)
      (efs-parse-listing 'dos:novell host user dir path switches))

     ;; Check for FTP software's server
     ((re-search-forward efs-dos:ftp-file-line-regexp nil t)
      (efs-add-listing-type 'dos:ftp host user)
      (efs-parse-listing 'dos:ftp host user dir path switches))

     ;; Check for winsock
     ((re-search-forward efs-dos:winsock-date-and-size-regexp nil t)
      (efs-add-listing-type 'dos:winsock host user)
      (efs-parse-listing 'dos:winsock host user dir path switches))
     
     ;; Check for the NCSA FTP server
     ((re-search-forward efs-dos:ncsa-file-line-regexp nil t)
      (efs-add-listing-type 'dos:ncsa host user)
      (efs-parse-listing 'dos:ncsa host user dir path switches))

     ;; Check for Frontier's Super-TCP server
     ((re-search-forward efs-dos:stcp-file-line-regexp nil t)
      (efs-add-listing-type 'dos:stcp host user)
      (efs-parse-listing 'dos:stcp host user dir path switches))
     
     ((string-match "^/\\([A-Za-z0-9]:/\\)?$" dir)
      ;; root always exists
      (let ((tbl (efs-make-hashtable)))
	(efs-put-hash-entry "." '(t) tbl)
	(efs-put-hash-entry ".." '(t) tbl)
	tbl))
     (t
      ;; an error message?
      nil))))

;; Some DOS servers (NCSA), return a 501 message for an empty disk.
(efs-defun efs-ls-dumb-check dos (line host file path lsargs msg noparse
				       noerror nowait cont)
  (and (string-match "^501 " line)
       (string-match "^/[A-Za-z0-9]:/?$" path)
       (let ((parse (or (null noparse) (eq noparse 'parse)
			(efs-parsable-switches-p lsargs t))))
	 (efs-add-to-ls-cache file lsargs "\n" parse)
	 (if parse
	     (efs-set-files file (let ((tbl (efs-make-hashtable)))
				   (efs-put-hash-entry "." '(t) tbl)
				   (efs-put-hash-entry ".." '(t) tbl)
				   tbl)))
	 (if nowait
	     (progn
	       (if cont
		   (efs-call-cont cont "\n"))
	       t)
	   (if cont
	       (efs-call-cont cont "\n"))
	   "\n"))))

(efs-defun efs-fix-path dos (path &optional reverse)
  (efs-fix-pc-path path reverse))

(efs-defun efs-fix-dir-path dos (dir-path)
  ;; Convert path from UNIX-ish to DOS for a DIRectory listing.
  (cond ((string-match "^/\\(.:\\)?$" dir-path)
	 (error "Can't list DOS or OS/2 disks"))
	;; Neither DOS nor OS/2 allows us to end the name of a directory
	;; with an "\".
	;; Adding *.* to the end also allows us to distinguish plain files from
	;; directories.  All DOS servers seem to understand this except
	;; Frontier Technologies' super-tcp server.
	((string-match "/$" dir-path)
	 (concat (efs-fix-pc-path dir-path) "*.*"))
	(t (efs-fix-pc-path dir-path))))

(efs-defun efs-get-pwd dos (host user &optional xpwd)
  ;; Parses PWD output for the current working directory. Hopefully this is
  ;; DOS proof.
  (let* ((result (efs-send-cmd host user (list 'quote
						    (if xpwd 'xpwd 'pwd))
				    "Getting PWD"))
	 (line (nth 1 result))
	 dir)
    (if (car result)
	(efs-save-match-data
	  (and (or (string-match "\"\\([^\"]*\\)\"" line)
		   ;; FTP software's output. They should know better...
		   (string-match "Current working directory is +\\([^ ]+\\)$"
				 line))
	       (setq dir (substring line
				    (match-beginning 1)
				    (match-end 1))))))
    (cons dir line)))

(efs-defun efs-allow-child-lookup dos (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.

  ;; Subdirs in DOS usually don't have an extension.
  (not (string-match "\\." file)))

;;;;-----------------------------------
;;;; Support for the Novell FTP server
;;;;-----------------------------------

(defconst efs-dos:novell-file-line-regexp
  ;; Matches from the first character of the filename to the end of the date.
  ;; Does not match parent directories which the server might decide
  ;; to put in front of the filename.
  (concat
   "\\([-_+=a-zA-Z0-9$.~]+\\) +\\(<DIR>\\|[0-9]+\\) +"
   "[ 0-9][0-9]-[0-9][0-9]-[0-9][0-9] "))

(efs-defun efs-parse-listing dos:novell
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a Novell DOS FTP listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (let ((tbl (efs-make-hashtable))
	file size dir-p)
    (efs-save-match-data
      ;; Can we check somehow if the listing is really for something
      ;; that doesn't exist?
      (goto-char (point-min))
      (while (re-search-forward efs-dos:novell-file-line-regexp
				nil t)
	(setq file (buffer-substring (match-beginning 1)
				     (match-end 1))
	      size (buffer-substring (match-beginning 2)
				     (match-end 2)))
	(if (string-equal size "<DIR>")
	    (setq size nil
		  dir-p t)
	  (setq size (string-to-int size)
		dir-p nil))
	(efs-put-hash-entry file (list dir-p size) tbl)
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl)
      tbl)))

;;; Tree Dired Support

(defconst efs-dired-dos:novell-re-exe
  "^. [ \t]+[-_+=a-zA-Z0-9$~]+\\.exe ")

(or (assq 'dos:novell efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:novell  efs-dired-dos:novell-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:novell-re-dir
  "^. [ \t]+[-_+=a-zA-Z0-9$~]+ +<DIR>")

(or (assq 'dos:novell efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'dos:novell  efs-dired-dos:novell-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:novell (dir)
  (efs-dired-pc-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos:novell
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    ;; move over marker
    (if (re-search-forward efs-dos:novell-file-line-regexp eol t)
	(goto-char (match-beginning 0)) ; returns (point)
      (and raise-error (error "No file on this line")))))
	
(efs-defun efs-dired-manual-move-to-end-of-filename dos:novell
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-fixup-listing dos:novell
  (file path &optional switches wildcard)
  ;; DOS may insert the entire directory name in front of the file name.
  ;; Scrape it off. The Novell server seems to do weird things when insert
  ;; the full-path, so be liberal with the hatchet.
  (goto-char (point-min))
  (while (re-search-forward efs-dos:novell-file-line-regexp nil t)
    (beginning-of-line)
    (delete-region (point) (match-beginning 0))
    (forward-line 1))
  ;; the novell server outputs lines in seemingly random order
  ;; this isn't as good as sorting switches, but at least it's not random.
  (sort-fields 1 (point-min) (progn (goto-char (point-max))
				    (forward-line -1)
				    (point))))

(efs-defun efs-dired-ls-trim dos:novell ()
  (goto-char (point-min))
  (let (case-fold-search)
    (forward-line 1)
    (if (looking-at " [0-9]+ File(s)\n")
	(delete-region (match-beginning 0) (match-end 0)))))


;;;;-----------------------------------------------
;;;; PC/TCP (by FTP software) support
;;;;-----------------------------------------------

(efs-defun efs-parse-listing dos:ftp
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be an FTP Software DOS
  ;; listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (let ((tbl (efs-make-hashtable))
	file size dir-p)
    (efs-save-match-data
      ;; Can we check somehow if an empty directory is really
      ;; a nonexistent directory?
      (goto-char (point-min))
      (goto-char (point-min))
      (while (looking-at efs-dos:ftp-file-line-regexp)
	(setq file (buffer-substring (match-beginning 2)
				     (match-end 2))
	      size (buffer-substring (match-beginning 1)
				     (match-end 1)))
	(if (string-equal size "<dir>")
	    (setq size nil
		  dir-p t)
	  (setq size (string-to-int size)
		dir-p nil))
	(efs-put-hash-entry file (list dir-p size) tbl)
	(forward-line 1))
      (efs-put-hash-entry "." '(t) tbl)
      (efs-put-hash-entry ".." '(t) tbl)
      tbl)))

;;; Tree Dired Support

(defconst efs-dired-dos:ftp-re-exe
  "^. [ \t]*[0-9]+ +[-_+=a-zA-Z0-9$~]+\\.exe ")

(or (assq 'dos:ftp efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:ftp  efs-dired-dos:ftp-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:ftp-re-dir
  "^. [ \t]*<dir> ")

(or (assq 'dos:ftp efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'dos:ftp  efs-dired-dos:ftp-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:ftp (dir)
  (efs-dired-pc-insert-headerline dir))

;;; Because dos:ftp listings have the file names right justified,
;;; I have reversed what -move-to-filename and -move-to-end-of-filename
;;; actually do. This shouldn't confuse dired, and should make browsing
;;; a dos:ftp listing more aesthetically pleasing.

(efs-defun efs-dired-manual-move-to-filename dos:ftp
  (&optional raise-error bol eol)
  ;; In dired, move to the *last* char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-dos:ftp-file-line-regexp eol t)
	(goto-char (match-end 2)) ; returns (point)
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename dos:ftp
  (&optional no-error bol eol)
  ;; Assumes point is at the *end* of filename. Really moves the
  ;; point to the beginning of the filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  ;; This is the DOS version. It is common to all of the DOS ftp servers
  ;; since it depends only on the file name character set.
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
    (skip-chars-backward "-_+=a-zA-Z0-9.$~" bol)
    (if (= opoint (point))
	(if no-error
	    nil
	  (error "No file on this line"))
      (point))))

;;;;-----------------------------------------------
;;;; NCSA FTP support
;;;;-----------------------------------------------

(efs-defun efs-parse-listing dos:ncsa
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a Novell DOS FTP listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (let (tbl file size dir-p next)
    (efs-save-match-data
      (goto-char (point-min))
      (while (re-search-forward
	      efs-dos:ncsa-file-line-regexp
	      (setq next (save-excursion (forward-line 1) (point))) t)
	(setq file (buffer-substring (match-beginning 1)
				     (match-end 1))
	      size (buffer-substring (match-beginning 2)
				     (match-end 2)))
	(if (string-equal size "<DIR>")
	    (setq size nil
		  dir-p t)
	  (setq size (string-to-int size)
		dir-p nil))
	(efs-put-hash-entry file (list dir-p size)
			    (or tbl (setq tbl (efs-make-hashtable))))
	(goto-char next))
      ;; DOS does not put . and .. in the root directory.
      (if (or tbl
	      ;; root always exists
	      (string-match "^/\\([A-Za-z0-9]:/\\)?$" dir))
	  (progn
	    (efs-put-hash-entry "." '(t) tbl)
	    (efs-put-hash-entry ".." '(t) tbl)))
      tbl)))

;;; Tree Dired Support

(defconst efs-dired-dos:ncsa-re-exe
  "^. [ \t]+[-_+=a-zA-Z0-9$~]+\\.exe ")

(or (assq 'dos:ncsa efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:ncsa  efs-dired-dos:ncsa-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:ncsa-re-dir
  "^. [ \t]+[-_+=a-zA-Z0-9$~]+ +<DIR>")

(or (assq 'dos:ncsa efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'dos:ncsa  efs-dired-dos:ncsa-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:ncsa (dir)
  (efs-dired-pc-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos:ncsa
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward "[-_+=a-zA-Z0-9$.~]+ +\\(<DIR>\\|[0-9]\\)" eol t)
	(goto-char (match-beginning 0)) ; returns (point)
      (and raise-error (error "No file on this line")))))
	
(efs-defun efs-dired-manual-move-to-end-of-filename dos:ncsa
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-fixup-listing dos:ncsa
  (file path &optional switches wildcard)
  ;; DOS may insert the entire directory name in front of the file name.
  ;; Scrape it off.
  (let (bonl)
    (goto-char (point-min))
    (while (re-search-forward
	    efs-dos:ncsa-file-line-regexp
	    (setq bonl (save-excursion (forward-line 1) (point))) t)
      (goto-char (match-beginning 0))
      (delete-region (point) (progn (beginning-of-line) (point)))
      (goto-char bonl)))
  ;; sort the buffer
  (sort-fields 1 (point-min) (point-max)))

(efs-defun efs-dired-ls-trim dos:ncsa ()
  (goto-char (point-min))
  (if (re-search-forward efs-dos:ncsa-file-line-regexp nil t)
      (delete-region (point-min) (match-beginning 0))))

;;;;-----------------------------------------------
;;;; Microsoft DOS FTP support
;;;;-----------------------------------------------

(defconst efs-dos:microsoft-valid-listing-regexp
  (concat efs-dos:microsoft-file-line-regexp "\\."))
  
(efs-defun efs-parse-listing dos:microsoft
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a Novell DOS FTP listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax

  ;; Use the existence of a "." file as confirmation that it's really
  ;; a directory listing.
  (goto-char (point-min))
  (efs-save-match-data
    (if (or (string-match "^/.:/$" dir)
	    (re-search-forward efs-dos:microsoft-valid-listing-regexp nil t))
	(let ((tbl (efs-make-hashtable))
	      size dir-p)
	  (goto-char (point-min))
	  (while (re-search-forward efs-dos:microsoft-file-line-regexp nil t)
	    (setq size (buffer-substring (match-beginning 1) (match-end 1)))
	    (if (string-equal size "<DIR>")
		(setq size nil
		      dir-p t)
	      (setq size (string-to-int size)
		    dir-p nil))
	    (efs-put-hash-entry (buffer-substring (point)
						  (progn (end-of-line)
							 (point)))
				(list dir-p size) tbl)
	    (forward-line 1))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

;;; Tree Dired Support

(defconst efs-dired-dos:microsoft-re-exe
  "^[^\n]+ +[-_+=a-zA-Z0-9$~]+\\.\\(EXE\\|exe\\)$")

(or (assq 'dos:microsoft efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:microsoft  efs-dired-dos:microsoft-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:microsoft-re-dir
  "^[^\n]+ <DIR> ")

(or (assq 'dos:microsoft efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'dos:microsoft  efs-dired-dos:microsoft-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:microsoft (dir)
  (efs-dired-pc-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos:microsoft
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r"))
    (if (re-search-forward efs-dos:microsoft-file-line-regexp eol t)
	(goto-char (match-end 0)) ; returns (point)
      (and raise-error (error "No file on this line")))))
	
(efs-defun efs-dired-manual-move-to-end-of-filename dos:microsoft
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

;;;;-----------------------------------------------
;;;; Frontier's Super-TCP FTP Server for Windows
;;;;-----------------------------------------------

(efs-defun efs-parse-listing dos:stcp
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a Super-TCP FTP listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  
  ;; Use the existence of a strict file line pattern as
  ;; confirmation that it's really a directory listing.
  (goto-char (point-min))
  (efs-save-match-data
    (let ((regexp (concat "^" efs-dos:stcp-file-line-regexp)))
      (if (let ((eol (save-excursion (end-of-line) (point))))
	    (re-search-forward regexp eol t))
	  (let ((tbl (efs-make-hashtable))
		size dir-p)
	    (goto-char (point-min))
	    (while (re-search-forward regexp nil t)
	      (setq size (buffer-substring (match-beginning 2) (match-end 2)))
	      (if (string-equal size "<DIR>")
		  (setq size nil
			dir-p t)
		(setq size (string-to-int size)
		      dir-p nil))
	      (efs-put-hash-entry (buffer-substring (match-beginning 1)
						    (match-end 1))
				  (list dir-p size) tbl)
	      (forward-line 1))
	    (efs-put-hash-entry "." '(t) tbl)
	    (efs-put-hash-entry ".." '(t) tbl)
	    tbl)))))
 
;;; Tree Dired Support

(defconst efs-dired-dos:stcp-re-exe
  "^[-_+=a-zA-Z0-9$~]+\\.\\(EXE\\|exe\\) ")

(or (assq 'dos:stcp efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:stcp  efs-dired-dos:stcp-re-exe)
 		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:stcp-re-dir
  "^[^\n ]+ +<DIR> ")

(or (assq 'dos:stcp efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
 	  (cons (cons 'dos:stcp  efs-dired-dos:stcp-re-dir)
 		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:stcp (dir)
  (efs-dired-pc-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos:stcp
  (&optional raise-error bol eol)
   ;; In dired, move to the first char of filename on this line.
   ;; Returns (point) or nil if raise-error is nil, and there is no
   ;; no filename on this line.
   (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
   (let (case-fold-search)
     (if bol
	 (goto-char bol)
       (skip-chars-backward "^\n\r")
       (setq bol (point)))
     (if (re-search-forward efs-dos:stcp-file-line-regexp eol t)
	 (goto-char (match-beginning 0)) ; returns (point)
       (if raise-error
	   (error "No file on this line")
	 (goto-char bol)))))

(efs-defun efs-dired-manual-move-to-end-of-filename dos:stcp
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-fixup-listing dos:stcp
  (file path &optional switches wildcard)
  ;; The Super-TCP server outputs lines in seemingly random order.
  ;; This isn't as good as sorting switches, but at least it's not random.
  (sort-fields 1 (point-min) (point-max)))

;;;;----------------------------------------------------------
;;;; Winsock DOS FTP server (Alun's FTP server)
;;;;----------------------------------------------------------

(efs-defun efs-parse-listing dos:winsock
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be a DOS Winsock listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  
  (goto-char (point-min))
  (efs-save-match-data
    (if (re-search-forward efs-dos:winsock-date-and-size-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      size dirp)
	  (while
	      (progn
		(setq size (string-to-int (buffer-substring (match-beginning 1)
							    (match-end 1)))
		      dirp (save-excursion
			     (beginning-of-line)
			     (skip-chars-forward " ")
			     (char-equal (following-char) ?d)))
		(efs-put-hash-entry
		 (buffer-substring (point) (progn (end-of-line) (point)))
		 (list dirp size) tbl)
		(re-search-forward efs-dos:winsock-date-and-size-regexp nil t)))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

(defconst efs-dired-dos:winsock-re-exe "\\.exe$")

(or (assq 'dos:winsock efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'dos:winsock  efs-dired-dos:winsock-re-exe)
 		efs-dired-re-exe-alist)))

(defconst efs-dired-dos:winsock-re-dir "^. +d")

(or (assq 'dos:winsock efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
 	  (cons (cons 'dos:winsock efs-dired-dos:winsock-re-dir)
 		efs-dired-re-dir-alist)))

(efs-defun efs-dired-insert-headerline dos:winsock (dir)
  (efs-dired-pc-insert-headerline dir))

(efs-defun efs-dired-manual-move-to-filename dos:winsock
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (re-search-forward efs-dos:winsock-date-and-size-regexp eol t)
	(point)
      (if raise-error
	  (error "No file on this line")
	(goto-char bol)))))

(efs-defun efs-dired-manual-move-to-end-of-filename dos:winsock
  (&optional no-error bol eol)
  ;; Assumes point is at the beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t)
  ;; On failure signals an error, or returns nil.
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-fixup-listing dos:winsock
  (file path &optional switches wildcard)
  ;; The Winsock server outputs lines in seemingly random order.
  ;; This isn't as good as sorting switches, but at least it's not random.
  (sort-fields 9 (point-min) (point-max)))

;;;;-----------------------------------------------------------
;;;;  OS/2 Support
;;;;-----------------------------------------------------------

;;; OS/2 has two types of file systems, FAT and HPFS. In the FAT file system
;;; filenames are restricted to the traditional DOS 8 + 3 syntax. In the
;;; HPFS file system, filenames can have arbitrarily many extensions (.'s).
;;; As well, file lines for "." and ".." are listed for HPFS.
;;; For the FAT FS, "." and ".." lines are only listed for sudirs, it seems.
;;; Go figure...

(defconst efs-os2-file-line-regexp
  (concat
   " +\\([0-9]+\\) +\\([^ ]+\\)? +[01][0-9]-[0-3][0-9]-[0-9][0-9] +"
   "[0-2][0-9]:[0-6][0-9] +"))

(efs-defun efs-fix-path os2 (path &optional reverse)
  (efs-fix-pc-path path reverse))

(efs-defun efs-fix-dir-path os2 (dir-path)
  ;; Convert path from UNIX-ish to DOS for a DIRectory listing.
  (cond ((string-match "^/\\(.:\\)?$" dir-path)
	 (error "Can't list DOS or OS/2 disks"))
	;; Neither DOS nor OS/2 allows us to end the name of a directory
	;; with an "\".
	;; Can't just hack it off, because if the dir is C:, we'll get the
	;; default dir.
	;; Don't apend the filename wildcard to distinguish
	;; plain files from directories, because OS/2 and DOS may
	;; not agree on what the wildcard is. Also, can't then tell
	;; the difference between plain files and empty directories.
	((string-match "/$" dir-path)
	 (concat (efs-fix-pc-path dir-path) "."))
	(t (efs-fix-pc-path dir-path))))

(defconst efs-os2-dot-line-regexp
  (concat efs-os2-file-line-regexp "\\.\n"))

(efs-defun efs-parse-listing os2
  (host user dir path &optional switches)
  ;; Parse the current buffer which is assumed to be an OS/2 listing.
  ;; To make sure that it is really a directory listing and not a bogus
  ;; listing of a single file, make sure that there is an entry for ".".
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a full remote path
  ;; PATH = directory in full efs-path syntax
  (efs-save-match-data
    (if (or
	 (string-match "^/.:/$" dir) ; FAT proofing
	 (progn
	   (goto-char (point-min))
	   (re-search-forward efs-os2-dot-line-regexp nil t)))
	(let ((tbl (efs-make-hashtable)))
	  (goto-char (point-min))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  (while (looking-at efs-os2-file-line-regexp)
	    (end-of-line)
	    (efs-put-hash-entry
	     (buffer-substring (match-end 0) (point))
	     (list (and
		    (match-beginning 2)
		    (string-equal "DIR"
				  (buffer-substring (match-beginning 2)
						    (match-end 2))))
		   (string-to-int (buffer-substring (match-beginning 1)
						    (match-end 1))))
	     tbl)
	    (forward-line 1))
	  tbl))))

;;; Tree Dired

(defconst efs-dired-os2-re-exe
  "^[^\n]+\\.EXEC?$")

(or (assq 'os2 efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'os2  efs-dired-os2-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-os2-re-dir
  "^ +[0-9]+ +DIR ")

(or (assq 'os2 efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'os2  efs-dired-os2-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename os2
  (&optional raise-error bol eol)
  ;; In dired, move to the first char of filename on this line.
  ;; Returns (point) or nil if raise-error is nil, and there is no
  ;; no filename on this line.
  ;; This version is for OS/2
  (or eol (setq eol (save-excursion (skip-chars-forward "^\r\n") (point))))
  (let (case-fold-search)
    (if bol
	(goto-char bol)
      (skip-chars-backward "^\n\r")
      (setq bol (point)))
    (if (and
	 (> (- eol bol) 24)
	 (progn
	   (forward-char 2)
	   (looking-at efs-os2-file-line-regexp)))
	(goto-char (match-end 0))
      (and raise-error (error "No file on this line")))))

(efs-defun efs-dired-manual-move-to-end-of-filename os2
  (&optional no-error bol eol)
  (efs-dired-pc-move-to-end-of-filename no-error bol eol))

(efs-defun efs-dired-insert-headerline os2 (dir)
  (efs-dired-pc-insert-headerline dir))

;; end of efs-pc.el
