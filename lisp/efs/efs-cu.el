;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-cu.el
;; Release:      $efs release: 1.15 $
;; Version:      $Revision: 1.3 $
;; RCS:          
;; Description:  Common utilities needed by efs files.
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Fri Jan 28 19:55:45 1994 by sandy on ibm550
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

;;;; Provisions and autoloads.

(provide 'efs-cu)
(require 'backquote)
(autoload 'efs-get-process "efs")
(autoload 'efs-parse-netrc "efs-netrc")

;;;; ------------------------------------------------------------
;;;; Use configuration variables.
;;;; ------------------------------------------------------------

(defvar efs-default-user nil
  "*User name to use when none is specied in a pathname.

If a string, than this string is used as the default user name.
If nil, then the name under which the user is logged in is used.
If t, then the user is prompted for a name.
If an association list of the form

   '((REGEXP1 . USERNAME1) (REGEXP2 . USERNAME2) ...)

then the host name is tested against each of the regular expressions
REGEXP in turn, and the default user name is the corresponding value
of USERNAME.  USERNAME may be either a string, nil, or t, and these
values are interpreted as above.  If there are no matches, then the
user's curent login name is used.")

(defvar efs-default-password nil
  "*Password to use when the user is the same as efs-default-user.")

(defvar efs-default-account nil
  "*Account password to use when the user is efs-default-user.")

;;;; -------------------------------------------------------------
;;;; Internal variables.
;;;; -------------------------------------------------------------

(defconst efs-cu-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.3 $" 11 -2)))

(defconst efs-case-insensitive-host-types
  '(vms cms mts ti-twenex ti-explorer dos mvs tops-20 mpe ka9q dos-distinct
    os2 hell guardian ms-unix netware cms-knet nos-ve)
  "List of host types for which case is insignificant in file names.")

;;; Remote path name syntax

;; All of the following variables must be set consistently.
;; As well the below two functions depend on the grouping constructs
;; in efs-path-regexp. So know what you're doing if you change them.

(defvar efs-path-regexp "^/\\([^@:/]*@\\)?\\([^@:/]*\\):.*"
  "Regexp of a fully expanded remote path.")

(defvar efs-path-format-string "/%s@%s:%s"
  "Format of a fully expanded remote path. Passed to format with
additional arguments user, host, and remote path.")

(defvar efs-path-format-without-user "/%s:%s"
  "Format of a remote path, but not specifying a user.")

(defvar efs-path-user-at-host-format
  (substring efs-path-format-string 1 7)
  "Format to return `user@host:' strings for completion in root directory.")

(defvar efs-path-host-format
  (substring efs-path-user-at-host-format 3)
  "Format to return `host:' strings for completion in root directory.")

(defvar efs-path-root-regexp "^/[^/:]+:"
  "Regexp to match the `/user@host:' root of an efs full path.")

(defvar efs-path-root-short-circuit-regexp "//[^/:]+:")
;; Regexp to match an efs user@host root, which short-circuits
;; the part of the path to the left of this pattern.

;;;; -----------------------------------------------------------
;;;; Variables for multiple host type support
;;;; -----------------------------------------------------------

(defvar efs-vms-host-regexp nil
  "Regexp to match the names of hosts running VMS.")
(defvar efs-cms-host-regexp nil
  "Regexp to match the names of hosts running CMS.")
(defvar efs-mts-host-regexp nil
  "Regexp to match the names of hosts running MTS.")
(defvar efs-ti-explorer-host-regexp nil
  "Regexp to match the names of hosts running TI-EXPLORER.
These are lisp machines.")
(defvar efs-ti-twenex-host-regexp nil
  "Regexp to match the names of hosts running TI-TWENEX.
These are lisp machines, and this should not be confused with DEC's TOPS-20.")
(defvar efs-sysV-unix-host-regexp nil
  "Regexp to match the names of sysV unix hosts.
These are defined to be unix hosts which mark symlinks
with a @ in an ls -lF listing.")
(defvar efs-bsd-unix-host-regexp nil
  "Regexp to match the names of bsd unix hosts.
These are defined to be unix hosts which do not mark symlinks
with a @ in an ls -lF listing.")
(defvar efs-next-unix-host-regexp nil
  "Regexp to match names of NeXT unix hosts.
These are defined to be unix hosts which put a @ after the
destination of a symlink when doing ls -lF listing.")
(defvar efs-unix-host-regexp nil
  "Regexp to match names of unix hosts.
I you know which type of unix, it is much better to set that regexp instead.")
(defvar efs-dumb-unix-host-regexp nil
  "Regexp to match names of unix hosts which do not take ls switches.
For these hosts we use the \"dir\" command.")
(defvar efs-super-dumb-unix-host-regexp nil
  "Regexp to match names of unix hosts with FTP servers that cannot do a PWD.
It is also assumed that these hosts do not accept ls switches, whether
or not this is actually true.")
(defvar efs-dos-host-regexp nil
  "Regexp to match names of hosts running DOS.")
;; In principal there is apollo unix support -- at least efs
;; should do the right thing. However, apollo ftp servers can be
;; very flakey, especially about accessing files by fullpaths.
;; Good luck.
(defvar efs-apollo-unix-host-regexp nil
  "Regexp to match names of apollo unix hosts running Apollo's Domain.
For these hosts we don't short-circuit //'s immediately following 
\"/user@host:\"")
(defvar efs-mvs-host-regexp nil
  "Regexp to match names of hosts running MVS.")
(defvar efs-tops-20-host-regexp nil
  "Regexp to match names of hosts runninf TOPS-20.")
(defvar efs-mpe-host-regexp nil
  "Regexp to match hosts running the MPE operating system.")
(defvar efs-ka9q-host-regexp nil
  "Regexp to match hosts using the ka9q ftp server. 
These may actually be running one of DOS, LINUX, or unix.")
(defvar efs-dos-distinct-host-regexp nil
  "Regexp to match DOS hosts using the Distinct FTP server.
These are not treated as DOS hosts with a special listing format, because
the Distinct FTP server uses unix-style path syntax.")
(defvar efs-os2-host-regexp nil
  "Regexp to match names of hosts running OS/2.")
(defvar efs-vos-host-regexp nil
  "Regexp to match hosts running the VOS operating system.")
(defvar efs-hell-host-regexp nil
  "Regexp to match hosts using the hellsoft ftp server.
These map be either DOS PC's or Macs.")
;; The way that we implement the hellsoft support, it probably won't
;; work with Macs. This could probably be fixed, if enough people scream.
(defvar efs-guardian-host-regexp nil
  "Regexp to match hosts running Tandem's guardian operating system.")
;; Note that ms-unix is really an FTP server running under DOS.
;; It's not a type of unix.
(defvar efs-ms-unix-host-regexp nil
  "Regexp to match hosts using the Microsoft FTP server in unix mode.")
(defvar efs-plan9-host-regexp nil
  "Regexp to match hosts running ATT's Plan 9 operating system.")
(defvar efs-cms-knet-host-regexp nil
  "Regexp to match hosts running the CMS KNET FTP server.")
(defvar efs-nos-ve-host-regexp nil
  "Regexp to match hosts running NOS/VE.")
(defvar efs-netware-host-regexp nil
  "Regexp to match hosts running Novell Netware.")
(defvar efs-dumb-apollo-unix-regexp nil
  "Regexp to match dumb hosts running Apollo's Domain.
These are hosts which do not accept switches to ls over FTP.")

;;; Further host types:
;;
;; unknown: This encompasses ka9q, dos-distinct, unix, sysV-unix, bsd-unix,
;;          next-unix, and dumb-unix.

(defconst efs-host-type-alist
  ;; When efs-add-host is called interactively, it will only allow
  ;; host types from this list.
  '((dumb-unix . efs-dumb-unix-host-regexp)
    (super-dumb-unix . efs-super-dumb-unix-host-regexp)
    (next-unix . efs-next-unix-host-regexp)
    (sysV-unix . efs-sysV-unix-host-regexp)
    (bsd-unix . efs-bsd-unix-host-regexp)
    (apollo-unix . efs-apollo-unix-host-regexp)
    (unix . efs-unix-host-regexp)
    (vms . efs-vms-host-regexp)
    (mts . efs-mts-host-regexp)
    (cms . efs-cms-host-regexp)
    (ti-explorer . efs-ti-explorer-host-regexp)
    (ti-twenex . efs-ti-twenex-host-regexp)
    (dos . efs-dos-host-regexp)
    (mvs . efs-mvs-host-regexp)
    (tops-20 . efs-tops-20-host-regexp)
    (mpe . efs-mpe-host-regexp)
    (ka9q . efs-ka9q-host-regexp)
    (dos-distinct . efs-dos-distinct-host-regexp)
    (os2 . efs-os2-host-regexp)
    (vos . efs-vos-host-regexp)
    (hell . efs-hell-host-regexp)
    (guardian . efs-guardian-host-regexp)
    (ms-unix . efs-ms-unix-host-regexp)
    (plan9 . efs-plan9-host-regexp)
    (cms-net . efs-cms-knet-host-regexp)
    (nos-ve . efs-nos-ve-host-regexp)
    (netware . efs-netware-host-regexp)
    (dumb-apollo-unix . efs-dumb-apollo-unix-regexp)))

;; host type cache
(defconst efs-host-cache nil)
(defconst efs-host-type-cache nil)

;; cache for efs-ftp-path.
(defconst efs-ftp-path-arg "")
(defconst efs-ftp-path-res nil)

;;;; -------------------------------------------------------------
;;;; General macros.
;;;; -------------------------------------------------------------

(defmacro efs-save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data.
Before executing BODY, case-fold-search is locally bound to nil."
  ;; Because Emacs is buggy about let-binding buffer-local variables,
  ;; we have to do this in a slightly convoluted way.
  (let ((match-data-temp (make-symbol "match-data"))
	(buff-temp (make-symbol "buff"))
	(cfs-temp (make-symbol "cfs")))
    (list
     'let (list (list match-data-temp '(match-data))
		(list buff-temp '(current-buffer))
		(list cfs-temp 'case-fold-search))
     (list 'unwind-protect
           (cons 'progn
		 (cons
		  '(setq case-fold-search nil)
		  body))
	   (list 'condition-case nil
		 (list 'save-excursion
		       (list 'set-buffer buff-temp)
		       (list 'setq 'case-fold-search cfs-temp))
		 '(error nil))
           (list 'store-match-data match-data-temp)))))

(put 'efs-save-match-data 'lisp-indent-hook 0)
(put 'efs-save-match-data 'edebug-form-spec '(&rest form))

(defmacro efs-define-fun (fun args &rest body)
  "Like defun, but only defines a function if it has no previous definition."
  ;; There are easier ways to do this. This approach is used so that the
  ;; byte compiler won't complain about possibly undefined functions.
  (`
   (progn
     (put (quote (, fun)) 'efs-define-fun
	  (and (fboundp (quote (, fun)))
	       (symbol-function (quote (, fun)))))
     (defun (, fun) (, args) (,@ body))
     (if (and (get (quote (, fun)) 'efs-define-fun)
	      (not (eq (car-safe (get (quote (, fun)) 'efs-define-fun))
		       (quote autoload))))
	 (fset (quote (, fun)) (get (quote (, fun)) 'efs-define-fun)))
     (put (quote (, fun)) 'efs-define-fun nil)
     (quote (, fun)))))

(put 'efs-define-fun 'lisp-indent-hook 'defun)

(defmacro efs-quote-dollars (string)
  ;; Quote `$' as `$$' in STRING to get it past `substitute-in-file-name.'
  (`
   (let ((string (, string))
	 (pos 0))
     (while (setq pos (string-match "\\$" string pos))
       (setq string (concat (substring string 0 pos)
			  "$";; precede by escape character (also a $)
			  (substring string pos))
	     ;; add 2 instead 1 since another $ was inserted
	     pos (+ 2 pos)))
     string)))

(defmacro efs-cont (implicit-args explicit-args &rest body)
  "Defines an efs continuation function.
The IMPLICIT-ARGS are bound when the continuation function is called.
The EXPLICIT-ARGS are bound when the continuation function is set."
  (let ((fun (list 'function
		   (cons 'lambda
			 (cons
			  (append implicit-args explicit-args)
			  body)))))
    (if explicit-args
	(cons 'list (cons fun explicit-args))
      fun)))

(put 'efs-cont 'lisp-indent-hook 2)

;;;; ------------------------------------------------------------
;;;; Utility functions
;;;; ------------------------------------------------------------

(efs-define-fun efs-repaint-minibuffer ()
  ;; Set minibuf_message = 0, so that the contents of the minibuffer will show.
  ;; This is the Emacs V19 version of this function. For Emacs 18, it will
  ;; be redefined in a grotty way to accomplish the same thing.
  (message nil))

(defun efs-get-user (host)
  "Given a HOST, return the default USER."
  (efs-parse-netrc)
  ;; We cannot check for users case-insensitively on those systems
  ;; which are treat usernames case-insens., because we need to log in
  ;; first, before we know what type of system.
  (let ((user (efs-get-host-property host 'user)))
    (if (stringp user)
	user
      (prog1
	  (setq user
		(cond ((stringp efs-default-user)
		       ;; We have a default name.  Use it.
		       efs-default-user)
		      ((consp efs-default-user)
		       ;; Walk the list looking for a host-specific value.
		       (efs-save-match-data
			 (let ((alist efs-default-user)
			       (case-fold-search t)
			       result)
			   (while alist
			     (if (string-match (car (car alist)) host)
				 (setq result (cdr (car alist))
				       alist nil)
			       (setq alist (cdr alist))))
			   (cond
			    ((stringp result)
			     result)
			    (result
			     (let ((enable-recursive-minibuffers t))
			       (read-string (format "User for %s: " host)
					    (user-login-name))))
			    (t
			     (user-login-name))))))
		      (efs-default-user
		       ;; Ask the user.
		       (let ((enable-recursive-minibuffers t))
			 (read-string (format "User for %s: " host)
				      (user-login-name))))
		      ;; Default to the user's login name.
		      (t
		       (user-login-name))))
	(efs-set-user host user)))))

(defun efs-ftp-path (path)
  "Parse PATH according to efs-path-regexp.
Returns a list (HOST USER PATH), or nil if PATH does not match the format."
  (or (string-equal path efs-ftp-path-arg)
      (setq efs-ftp-path-res
	    (efs-save-match-data
	      (and (string-match efs-path-regexp path)
		   (let ((host (substring path (match-beginning 2)
					  (match-end 2)))
			 (user (and (match-beginning 1)
				    (substring path (match-beginning 1)
					       (1- (match-end 1)))))
			 (rpath (substring path (1+ (match-end 2)))))
		     (list (if (string-equal host "")
			       (setq host (system-name))
			     host)
			   (or user (efs-get-user host))
			   rpath))))
	    ;; Set this last, in case efs-get-user calls this
	    ;; function, which would modify an earlier setting.
	    efs-ftp-path-arg path))
  efs-ftp-path-res)

(defun efs-chase-symlinks (file)
  ;; If FILE is a symlink, chase it until we get to a real file.
  ;; Unlike file truename, this function does not chase symlinks at
  ;; every level, only the bottom level. Therefore, it is not useful for
  ;; obtaining the truename of a file. It is useful for getting at file
  ;; attributes, with a lot less overhead than file truename.
  (let ((target (file-symlink-p file)))
    (if target
	(efs-chase-symlinks
	 (expand-file-name target (file-name-directory file)))
      file)))

;; If efs-host-type is called with the optional user
;; argument, it will attempt to guess the host type by connecting
;; as user, if necessary.

(defun efs-host-type (host &optional user)
  "Return a symbol which represents the type of the HOST given.
If the optional argument USER is given, attempts to guess the
host-type by logging in as USER."

  (and host
       (let ((host (downcase host))
	     type)
	 (cond
	  
	  ((and efs-host-cache
		(string-equal host efs-host-cache)
		efs-host-type-cache))
	  
	  ((setq type
		 (efs-get-host-property host 'host-type))
	   (setq efs-host-cache host
		 efs-host-type-cache type))
	  
	  ;; Trigger an ftp connection, in case we need to
	  ;; guess at the host type.
	  ((and user (efs-get-process host user)
		(if (string-equal host efs-host-cache)
		    ;; logging in may update the cache
		    efs-host-type-cache
		  (and (setq type (efs-get-host-property host 'host-type))
		       (setq efs-host-cache host
			     efs-host-type-cache type)))))
	  
	  ;; Try the regexps.
	  ((setq type
		 (let ((alist efs-host-type-alist)
		       regexp type-pair)
		   (catch 'match
		     (efs-save-match-data
		       (let ((case-fold-search t))
			 (while alist
			   (progn
			     (and (setq type-pair (car alist)
					regexp (eval (cdr type-pair)))
				  (string-match regexp host)
				  (throw 'match (car type-pair)))
			     (setq alist (cdr alist)))))
		       nil))))
	   (setq efs-host-cache host
		 efs-host-type-cache type))
	  ;; Return 'unknown, but _don't_ cache it.
	  (t 'unknown)))))

;;;; -------------------------------------------------------------
;;;; Functions and macros for hashtables.
;;;; -------------------------------------------------------------

(defun efs-make-hashtable (&optional size)
  "Make an obarray suitable for use as a hashtable.
SIZE, if supplied, should be a prime number."
  (make-vector (or size 31) 0))

(defun efs-map-hashtable (fun tbl &optional property)
  "Call FUNCTION on each key and value in HASHTABLE.
If PROPERTY is non-nil, it is the property to be used as the second
argument to FUNCTION. The default property is 'val"
  (let ((prop (or property 'val)))
    (mapatoms
     (function 
      (lambda (sym)
	(funcall fun (symbol-name sym) (get sym prop))))
     tbl)))

(defmacro efs-make-hash-key (key)
  "Convert KEY into a suitable key for a hashtable. This returns a string."
  (` (let ((key (, key))) ; eval exactly once, in case evalling key moves the
			  ; point.
       (if (stringp key) key (prin1-to-string key)))))

;;; Note, if you store entries in a hashtable case-sensitively, and then
;;; retrieve them with IGNORE-CASE=t, it is possible that there may be
;;; be more than one entry that could be retrieved. It is more or less random
;;; which one you'll get. The onus is on the programmer to be consistent.
;;; Suggestions to make this faster are gratefully accepted!

(defmacro efs-case-fold-intern-soft (name tbl)
  "Returns a symbol with case-insensitive name NAME in the obarray TBL.
Case is considered insignificant in NAME. Note, if there is more than 
one possible match, it is hard to predicate which one you'll get."
  (`
   (let* ((completion-ignore-case t)
	  (name (, name))
	  (tbl (, tbl))
	  (len (length (, name)))
	  (newname (try-completion name tbl
				   (function
				    (lambda (sym)
				      (= (length (symbol-name sym)) len))))))
     (and newname
	  (if (eq newname t)
	      (intern name tbl)
	    (intern newname tbl))))))

(defmacro efs-hash-entry-exists-p (key tbl &optional ignore-case)
  "Return whether there is an association for KEY in TABLE. 
If optional IGNORE-CASE is non-nil, then ignore-case in the test."
  (` (let ((key (efs-make-hash-key (, key))))
       (if (, ignore-case)
	   (efs-case-fold-intern-soft key (, tbl))
       (intern-soft key (, tbl))))))

(defmacro efs-get-hash-entry (key tbl &optional ignore-case)
  "Return the value associated with KEY in HASHTABLE.
If the optional argument IGNORE-CASE is given, then case in the key is 
considered irrelevant."
  (` (let* ((key (efs-make-hash-key (, key)))
	    (sym (if (, ignore-case)
		     (efs-case-fold-intern-soft key (, tbl))
		   (intern-soft key (, tbl)))))
       (and sym (get sym 'val)))))

(defmacro efs-put-hash-entry (key val tbl &optional ignore-case)
  "Record an association between KEY and VALUE in HASHTABLE.
If the optional IGNORE-CASE argument is given, then check for an entry
which is the same modulo case, and update it instead of adding a new entry."
  (` (let* ((key (efs-make-hash-key (, key)))
	    (sym (if (, ignore-case)
		     (or (efs-case-fold-intern-soft key (, tbl))
			 (intern key (, tbl)))
		   (intern key (, tbl)))))
       (put sym 'val (, val)))))

(defun efs-del-hash-entry (key tbl &optional ignore-case)
  "Copy all symbols except KEY in HASHTABLE and return modified hashtable.
If the optional argument CASE-FOLD is non-nil, then fold KEY to lower case."
  (let* ((len (length tbl))
	 (new-tbl (efs-make-hashtable len))
	 (i (1- len))
	 (key (efs-make-hash-key key)))
    (if ignore-case (setq key (downcase key)))
    (efs-map-hashtable
     (if ignore-case
	 (function
	  (lambda (k v)
	    (or (string-equal (downcase k) key)
		;; Don't need to specify ignore-case here, because
		;; we have already weeded out possible case-fold matches.
		(efs-put-hash-entry k v new-tbl))))
       (function
	(lambda (k v)
	  (or (string-equal k key)
	      (efs-put-hash-entry k v new-tbl)))))
     tbl)
    (while (>= i 0)
      (aset tbl i (aref new-tbl i))
      (setq i (1- i)))
    ;; Return the result.
    tbl))

(defun efs-hash-table-keys (tbl &optional nosort)
  "Return a sorted of all the keys in the hashtable TBL, as strings.
This list is sorted, unless the optional argument NOSORT is non-nil."
  (let ((result (all-completions "" tbl)))
    (if nosort
	result
      (sort result (function string-lessp)))))

;;; hashtable variables

(defconst efs-host-hashtable (efs-make-hashtable)
  "Hash table holding data on hosts.")

(defconst efs-host-user-hashtable (efs-make-hashtable)
  "Hash table for holding data on host user pairs.")

(defconst efs-minidisk-hashtable (efs-make-hashtable)
  "Mapping between a host, user, minidisk triplet and a account password.")

;;;; ------------------------------------------------------------
;;;; Host / User mapping
;;;; ------------------------------------------------------------

(defun efs-set-host-property (host property value)
  ;; For HOST, sets PROPERTY to VALUE.
  (put (intern (downcase host) efs-host-hashtable) property value))

(defun efs-get-host-property (host property)
  ;; For HOST, gets PROPERTY.
  (get (intern (downcase host) efs-host-hashtable) property))

(defun efs-set-host-user-property (host user property value)
  ;; For HOST and USER, sets PROPERTY to VALUE.
  (let* ((key (concat (downcase host) "/" user))
	 (sym (and (memq (efs-host-type host) efs-case-insensitive-host-types)
		   (efs-case-fold-intern-soft key efs-host-user-hashtable))))
    (or sym (setq sym (intern key efs-host-user-hashtable)))
    (put sym property value)))

(defun efs-get-host-user-property (host user property)
  ;; For HOST and USER, gets PROPERTY.
  (let* ((key (concat (downcase host) "/" user))
	 (sym (and (memq (efs-host-type host) efs-case-insensitive-host-types)
		   (efs-case-fold-intern-soft key efs-host-user-hashtable))))
    (or sym (setq sym (intern key efs-host-user-hashtable)))
    (get sym property)))

(defun efs-set-user (host user)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsUser: ")
  (efs-set-host-property host 'user user))

;;;; ------------------------------------------------------------
;;;; Encryption
;;;; ------------------------------------------------------------

(defconst efs-passwd-seed nil)
;; seed used to encrypt the password cache.

(defun efs-get-passwd-seed ()
  ;; Returns a random number to use for encrypting passwords.
  (or efs-passwd-seed
      (setq efs-passwd-seed (+ 1 (random 255)))))

(defun efs-code-string (string)
  ;; Encode a string, using `efs-passwd-seed'. This is nil-potent,
  ;; meaning applying it twice decodes.
  (if (and (fboundp 'int-char) (fboundp 'char-int))
      (mapconcat
       (function
	(lambda (c)
	  (char-to-string
	   (int-char (logxor (efs-get-passwd-seed) (char-int c))))))
       string "")
    (mapconcat
     (function
      (lambda (c)
	(char-to-string (logxor (efs-get-passwd-seed) c))))
     string "")))

;;; end of efs-cu.el
