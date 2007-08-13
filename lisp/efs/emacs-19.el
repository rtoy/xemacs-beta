;;;; Emacs 19 compatibility functions for use in Emacs 18.
;;;; Based on: $Id: emacs-19.el,v 1.1 1997/02/11 05:05:14 steve Exp $
;;;;
;;;; Rewritten by sandy@ibm550.sissa.it after gnu emacs 19 was
;;;; released to make it closer to V19.
;;;; Last modified: Sun Jun 12 00:06:06 1994 by sandy on ibm550

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;; These functions are used in dired.el, but are also of general
;; interest, so you may want to add this to your .emacs:
;; 
;; (autoload 'make-directory "emacs-19" "Make a directory." t)
;; (autoload 'delete-directory "emacs-19" "Remove a directory." t)
;; (autoload 'member "emacs-19" "Like memq, but uses `equal' instead of `eq'.")
;; (autoload 'compiled-function-p "emacs-19" "Emacs 18 doesn't have these.")

(provide 'emacs-19)

;;; Variables

(defvar insert-directory-program "ls"
  "Absolute or relative name of the `ls' program used by `insert-directory'.")

(defvar bv-length) ; make the byte compiler a happy camper

(defconst directory-abbrev-alist
  nil
  "*Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
FROM with TO when it appears in a directory name.  This replacement is
done when setting up the default directory of a newly visited file.
*Every* FROM string should start with `^'.

Use this feature when you have directories which you normally refer to
via absolute symbolic links.  Make TO the name of the link, and FROM
the name it is linked to.")

(defconst automount-dir-prefix "^/tmp_mnt/"
  "Regexp to match the automounter prefix in a directory name.")

(defvar abbreviated-home-dir nil
  "The the user's homedir abbreviated according to `directory-abbrev-list'.")

;;; Autoloads

(autoload 'diff "diff" "Diff two files." t)
(autoload 'diff-backup "diff" "Diff a file with its most recent backup.")

;;; Functions which are subroutines in Emacs 19.

;; Provide a non-working version of find-file-name-handler.
;; If you want it to work, require 'fn-handler.

(or (fboundp 'find-file-name-handler) (fset 'find-file-name-handler 'ignore))
(or (boundp 'file-name-handler-alist) (defvar file-name-handler-alist nil))

;; The name of buffer-flush-undo has changed in V19.
(fset 'buffer-disable-undo 'buffer-flush-undo)

(defun current-time ()
  "Returns the number of seconds since midnight.
A poor man's version of the the function `current-time' in emacs 19."
  (let ((string (current-time-string)))
    (list
     0
     (+ (* 3600 (string-to-int (substring string 11 13)))
	(* 60 (string-to-int (substring string 14 16)))
	(string-to-int (substring string 17 19)))
     0)))

;; call-process below may lose if filename starts with a `-', but I
;; fear not all mkdir or rmdir implementations understand `--'.

(defun delete-directory (fn)
  "Delete a directory.
This is a subr in Emacs 19."
  (interactive 
   (list (read-file-name "Delete directory: " nil nil 'confirm)))
  (setq fn (expand-file-name fn))
  (if (file-directory-p fn)
      (call-process "rmdir" nil nil nil fn)
    (error "Not a directory: %s" fn))
  (if (file-exists-p fn)
      (error "Could not remove directory %s" fn)))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs."
  (interactive "FMake directory: \nP")
  (if (not parents)
      (make-directory-internal dir)
    (let ((dir (directory-file-name (expand-file-name dir)))
	  create-list)
      (while (not (file-exists-p dir))
	(setq create-list (cons dir create-list)	    
	      dir (directory-file-name (file-name-directory dir))))
      (while create-list
	(make-directory-internal (car create-list))
	(setq create-list (cdr create-list))))))

(defun make-directory-internal (fn)
  ;; This is a subroutine in emacs 19.
  (let* ((fn (expand-file-name fn))
	 (handler (find-file-name-handler fn 'make-directory-internal)))
    (if handler
	(funcall handler 'make-directory-internal fn)
      (setq fn (directory-file-name fn))
      (if (file-exists-p fn)
	  (error "Cannot make directory %s: file already exists" fn)
	(call-process "mkdir" nil nil nil fn))
      (or (file-directory-p fn)
	  (error "Could not make directory %s" fn)))))

(defun kill-new (string)
  "Save STRING as if killed in a buffer."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring))

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      (if (eq system-type 'vax-vms)
	  (vms-read-directory file switches (current-buffer))
	(if wildcard
	    ;; Run ls in the directory of the file pattern we asked for.
	    (let ((default-directory
		    (if (file-name-absolute-p file)
			(file-name-directory file)
		      (file-name-directory (expand-file-name file))))
		  (pattern (file-name-nondirectory file))
		  (beg 0))
	      ;; Quote some characters that have special meanings in shells;
	      ;; but don't quote the wildcards--we want them to be special.
	      ;; We also currently don't quote the quoting characters
	      ;; in case people want to use them explicitly to quote
	      ;; wildcard characters.
	      (while (string-match "[ \t\n;<>&|()#$]" pattern beg)
		(setq pattern
		      (concat (substring pattern 0 (match-beginning 0))
			      "\\"
			      (substring pattern (match-beginning 0)))
		      beg (1+ (match-end 0))))
	      (call-process shell-file-name nil t nil
			    "-c" (concat insert-directory-program
					 " -d "
					 (if (stringp switches)
					     switches
					   (mapconcat 'identity switches " "))
					 " "
					 pattern)))
	  ;; SunOS 4.1.3, SVr4 and others need the "." to list the
	  ;; directory if FILE is a symbolic link.
	  (apply 'call-process
		 insert-directory-program nil t nil
		 (let (list)
		   (if (listp switches)
		       (setq list switches)
		     (if (not (equal switches ""))
			 (progn
			   ;; Split the switches at any spaces
			   ;; so we can pass separate options as separate args.
			   (while (string-match " " switches)
			     (setq list (cons (substring switches 0
							 (match-beginning 0))
					      list)
				   switches (substring switches
						       (match-end 0))))
			   (setq list (cons switches list)))))
		   (append list
			   (list
			    (if full-directory-p
				(concat (file-name-as-directory file) ".")
			      file))))))))))

(defun file-local-copy (file)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'file-local-copy)))
    ;; Does nothing, if no handler.
    (if handler
	(funcall handler 'file-local-copy file))))

(defun file-truename (filename)
  "Return the truename of FILENAME, which should be absolute.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level."
  (if (or (string= filename "~")
	  (and (string= (substring filename 0 1) "~")
	       (string-match "~[^/]*" filename)))
      (progn
	(setq filename (expand-file-name filename))
	(if (string= filename "")
	    (setq filename "/"))))
  (let ((handler (find-file-name-handler filename 'file-truename)))
    ;; For file name that has a special handler, call handler.
    ;; This is so that ange-ftp can save time by doing a no-op.
    (if handler
	(funcall handler 'file-truename filename)
      (let ((dir (file-name-directory filename))
	    target dirfile file-name-handler-alist)
	;; Get the truename of the directory.
	(setq dirfile (directory-file-name dir))
	;; If these are equal, we have the (or a) root directory.
	(or (string= dir dirfile)
	    (setq dir (file-name-as-directory (file-truename dirfile))))
	(if (equal ".." (file-name-nondirectory filename))
	    (directory-file-name (file-name-directory
				  (directory-file-name dir)))
	  (if (equal "." (file-name-nondirectory filename))
	      (directory-file-name dir)
	    ;; Put it back on the file name.
	    (setq filename (concat dir (file-name-nondirectory filename)))
	    ;; Is the file name the name of a link?
	    (setq target (file-symlink-p filename))
	    (if target
		;; Yes => chase that link, then start all over
		;; since the link may point to a directory name that uses links.
		;; We can't safely use expand-file-name here
		;; since target might look like foo/../bar where foo
		;; is itself a link.  Instead, we handle . and .. above.
		(if (file-name-absolute-p target)
		    (file-truename target)
		  (file-truename (concat dir target)))
	      ;; No, we are done!
	      filename)))))))

(defun generate-new-buffer-name (name)
  "Return a string which is the name of no existing buffer based on
NAME. If there is no live buffer named NAME, return NAME. Otherwise,
modify name by appending `<NUMBER>', incrementing NUMBER until an
unused name is found. Return that name."
  (if (get-buffer name)
      (let ((num 2)
	    attempt)
	(while (progn
		 (setq attempt (concat name "<" (int-to-string num) ">"))
		 (get-buffer attempt))
	  (setq num (1+ num)))
	attempt)
    name))

(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory.
Type \\[describe-variable] directory-abbrev-alist RET for more information."
  ;; Get rid of the prefixes added by the automounter.
  (if (and (string-match automount-dir-prefix filename)
	   (file-exists-p (file-name-directory
			   (substring filename (1- (match-end 0))))))
      (setq filename (substring filename (1- (match-end 0)))))
  (let ((tail directory-abbrev-alist))
    ;; If any elt of directory-abbrev-alist matches this name,
    ;; abbreviate accordingly.
    (while tail
      (if (string-match (car (car tail)) filename)
	  (setq filename
		(concat (cdr (car tail)) (substring filename (match-end 0)))))
      (setq tail (cdr tail)))
    ;; Compute and save the abbreviated homedir name.
    ;; We defer computing this until the first time it's needed, to
    ;; give time for directory-abbrev-alist to be set properly.
    (or abbreviated-home-dir
	(setq abbreviated-home-dir
	      (let ((abbreviated-home-dir "$foo"))
		(concat "^" (abbreviate-file-name (expand-file-name "~"))))))
    ;; If FILENAME starts with the abbreviated homedir,
    ;; make it start with `~' instead.
    (if (string-match abbreviated-home-dir filename)
	(setq filename
	      (concat "~"
		      ;; If abbreviated-home-dir ends with a slash,
		      ;; don't remove the corresponding slash from
		      ;; filename.  On MS-DOS and OS/2, you can have
		      ;; home directories like "g:/", in which it is
		      ;; important not to remove the slash.  And what
		      ;; about poor root on Unix systems?
		      (if (eq ?/ (aref abbreviated-home-dir
				       (1- (length abbreviated-home-dir))))
			  "/"
			"")
		      (substring filename (match-end 0)))))
    filename))

(defun file-newest-backup (filename)
  "Return most recent backup file for FILENAME or nil if no backups exist."
  (let* ((filename (expand-file-name filename))
	 (file (file-name-nondirectory filename))
	 (dir  (file-name-directory    filename))
	 (comp (file-name-all-completions file dir))
	 newest)
    (while comp
      (setq file (concat dir (car comp))
	    comp (cdr comp))
      (if (and (backup-file-name-p file)
	       (or (null newest) (file-newer-than-file-p file newest)))
	  (setq newest file)))
    newest))

;; This is used in various files.
;; The usage of bv-length is not very clean,
;; but I can't see a good alternative,
;; so as of now I am leaving it alone.
(defun backup-extract-version (fn)
  "Given the name of a numeric backup file, return the backup number.
Uses the free variable `bv-length', whose value should be
the index in the name where the version number begins."
  (if (and (string-match "[0-9]+~$" fn bv-length)
	   (= (match-beginning 0) bv-length))
      (string-to-int (substring fn bv-length -1))
      0))

;; The standard V18 version of this function doesn't support
;; the arg KEEP-BACKUP-VERSION
(defun file-name-sans-versions (name &optional keep-backup-version)
  "Return FILENAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it.
If the optional argument KEEP-BACKUP-VERSION is non-nil,
we do not remove backup version numbers, only true file version numbers."
  (let ((handler (find-file-name-handler name 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions name keep-backup-version)
      (substring name 0
		 (if (eq system-type 'vax-vms)
		     ;; VMS version number is (a) semicolon, optional
		     ;; sign, zero or more digits or (b) period, option
		     ;; sign, zero or more digits, provided this is the
		     ;; second period encountered outside of the
		     ;; device/directory part of the file name.
		     (or (string-match ";[-+]?[0-9]*\\'" name)
			 (if (string-match "\\.[^]>:]*\\(\\.[-+]?[0-9]*\\)\\'"
					   name)
			     (match-beginning 1))
			 (length name))
		   (if keep-backup-version
		       (length name)
		     (or (string-match "\\.~[0-9]+~\\'" name)
			 (string-match "~\\'" name)
			 (length name))))))))

(defun member (x y)
  "Like memq, but uses `equal' for comparison.
This is a subr in Emacs 19."
  (while (and y (not (equal x (car y))))
    (setq y (cdr y)))
  y)

(defun compiled-function-p (x)
  "Emacs 18 doesn't have these."
  nil)

;; punt -- this will at least allow handlers to work for this.
(defun set-visited-file-modtime (&optional time)
  (error "set-visited-file-modtime not defined in emacs 18."))

(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (or (if (consp function)
	  ;; Clever way to tell whether a given lambda-expression
	  ;; is equal to anything in the hook.
	  (let ((tail (assoc (cdr function) (symbol-value hook))))
	    (equal function tail))
	(memq function (symbol-value hook)))
      (set hook 
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))))))

;;; after-save.el (Now part of files.el in Gnu Emacs V19)

;;; Copyright (C) 1990 Roland McGrath
;;;

(or (fboundp 'real-save-buffer)
    (fset 'real-save-buffer (symbol-function 'save-buffer)))

(defvar after-save-hook nil
  "A function or list of functions to be run after saving the current buffer.")

(defun save-buffer (&optional args)
  "Save the current buffer, and then run `after-save-buffer-hook'.
The hooks are only run if the buffer was actually written.
For more documentation, do \\[describe-function] real-save-buffer RET."
  (interactive "p")
  (let ((modp (buffer-modified-p)))
    (real-save-buffer args)
    (if modp
	(run-hooks 'after-save-hook))))

;;; end of after-save

;;;;
;;;; Correcting for V18 bugs, and hacking around stupidities.
;;;;

;; The 18.57 version has a bug that causes C-x C-v RET (which usually
;; re-visits the current buffer) to fail on dired buffers.
;; Only the last statement was changed to avoid killing the current
;; buffer.
(defun find-alternate-file (filename)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want."
  (interactive "FFind alternate file: ")
  (and (buffer-modified-p)
       (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(oname (buffer-name)))
    (rename-buffer " **lose**")
    (setq buffer-file-name nil)
    (unwind-protect
	(progn
	  (unlock-buffer)
	  (find-file filename))
      (cond ((eq obuf (current-buffer))
	     (setq buffer-file-name ofile)
	     (lock-buffer)
	     (rename-buffer oname))))
    (or (eq (current-buffer) obuf)
	(kill-buffer obuf))))

;; At least in Emacs 18.55 this defvar has been forgotten to be copied
;; from lpr.el into loaddefs.el

(defvar lpr-command (if (eq system-type 'usg-unix-v)
			"lp" "lpr")
  "Shell command for printing a file")


;; buffer-disable-undo used to be called buffer-flush-undo in Emacs
;; 18.55:
(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

;;; end of emacs-19.el
