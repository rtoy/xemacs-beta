;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         fn-handler.el
;; Description:  enhanced file-name-handler-alist support for pre-19.23 Emacs
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Sat Mar 19 00:50:10 1994 by sandy on ibm550
;; Modified:     Tue Sep 13 20:59:19 1994 by sandy on ibm550
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; One of the problems with the file-name-handler-alist, is that when
;;; a handler gets called, and it has nothing to do for that function,
;;; the usual procedure is to remove the handler from the alist, and
;;; re-call the function.  This is necessary to avoid an infinite
;;; recursion.  However, if the function calling
;;; find-file-name-handler is not a primitive, there may be other lisp
;;; functions inside of it for which the handler does have some
;;; special actions specified.  They won't run, because the let-bound
;;; value of file-name-handler-alist doesn't contain the handler.
;;;
;;; This problem was solved in Emacs 19.23 with the variables
;;; inhibit-file-name-handlers and inhibit-file-name-operation
;;; This file provides this solution to older versions of emacs.


(provide 'fn-handler)
(require 'efs-ovwrt)

(or (boundp 'file-name-handler-alist)
    (defvar file-name-handler-alist nil
      "Association list of regexps for special file names and handlers."))

(defvar inhibit-file-name-handlers nil
  "List of handlers \(symbols\) to be avoided by `find-file-name-handler'.")

(defvar inhibit-file-name-operation nil
  "Defines to which operation `inhibit-file-name-handlers applies'
Must be a synbol.")

(defun find-file-name-handler (filename &optional operation) 
  "Return FILENAME1's handler function, if its syntax is handled specially.
Does not return handlers in `inhibit-file-name-handlers' list.
If there is no handler for FILENAME1, searches for one for FILENAME2.
Returns nil, if there is no handler for either file name.
A file name is handles specially if one of the regular expressions in
`file-name-handler-alist' matches it."
  (let ((match-data (match-data)))
    (unwind-protect
	(catch 'handler
	  (mapcar (function
		   (lambda (x)
		     (and
		      (not
		       (and
			(or (null operation)
			    (eq operation inhibit-file-name-operation))
			(memq (cdr x) inhibit-file-name-handlers)))
		      (string-match (car x) filename)
		      (throw 'handler (cdr x)))))
		  file-name-handler-alist)
	  nil)
      (store-match-data match-data))))

;;; Overloads to supply the file-name-handler-alist

(defun fn-handler-insert-file-contents (filename &optional visit &rest args)
  "Documented as original."
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'insert-file-contents)))
    (if handler
	(apply handler 'insert-file-contents filename visit args)
      (let (file-name-handler-alist)
	(apply 'fn-handler-real-insert-file-contents filename visit args)))))

(efs-overwrite-fn "fn-handler" 'insert-file-contents
		  'fn-handler-insert-file-contents)

(defun fn-handler-directory-files (directory &optional full match &rest nosort)
  "Documented as original."
  (let ((handler (find-file-name-handler directory 'directory-files)))
    (if handler
	(apply handler 'directory-files directory full match nosort)
      (let (file-name-handler-alist)
	(apply 'fn-handler-real-directory-files
	       directory full match nosort)))))

(efs-overwrite-fn "fn-handler" 'directory-files 'fn-handler-directory-files)

(defun fn-handler-list-directory (dirname &optional verbose)
  "Documented as original."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-file-name (if pfx "List directory (verbose): "
					 "List directory (brief): ")
				       nil default-directory nil)
		       pfx)))
  (let ((handler (find-file-name-handler dirname 'list-directory)))
    (if handler
	(funcall handler 'list-directory dirname verbose)
      (let (file-name-handler-alist)
	(fn-handler-real-list-directory dirname verbose)))))

(efs-overwrite-fn "fn-handler" 'list-directory 'fn-handler-list-directory)

(defun fn-handler-file-directory-p (filename)
  "Documented as original."
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'file-directory-p)))
    (if handler
	(funcall handler 'file-directory-p filename)
      (let (file-name-handler-alist)
	(fn-handler-real-file-directory-p filename)))))

(efs-overwrite-fn "fn-handler" ' file-directory-p 'fn-handler-file-directory-p)

(defun fn-handler-file-writable-p (filename)
  "Documented as original."
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'file-writable-p)))
    (if handler
	(funcall handler 'file-writable-p filename)
      (let (file-name-handler-alist)
	(fn-handler-real-file-writable-p filename)))))

(efs-overwrite-fn "fn-handler" 'file-writable-p 'fn-handler-file-writable-p)

(defun fn-handler-file-readable-p (filename)
  "Documented as original."
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'file-readable-p)))
    (if handler
	(funcall handler 'file-readable-p filename)
      (let (file-name-handler-alist)
	(fn-handler-real-file-readable-p filename)))))

(efs-overwrite-fn "fn-handler" 'file-readable-p 'fn-handler-file-readable-p)

(defun fn-handler-file-symlink-p (filename)
  "Documented as original."
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'file-symlink-p)))
    (if handler
	(funcall handler 'file-symlink-p filename)
      (let (file-name-handler-alist)
	(fn-handler-real-file-symlink-p filename)))))

(efs-overwrite-fn "fn-handler" 'file-symlink-p 'fn-handler-file-symlink-p)

(defun fn-handler-delete-file (file)
  "Documented as original"
  (interactive (list (read-file-name "Delete-file: " nil nil t)))
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'delete-file)))
    (if handler
	(funcall handler 'delete-file file)
      (let (file-name-handler-alist)
	(fn-handler-real-delete-file file)))))

(efs-overwrite-fn "fn-handler" 'delete-file 'fn-handler-delete-file)

(defun fn-handler-file-exists-p (filename)
  "Documented as original"
  (let* ((filename (expand-file-name filename))
	 (handler (find-file-name-handler filename 'file-exists-p)))
    (if handler
	(funcall handler 'file-exists-p filename)
      (let (file-name-handler-alist)
	(fn-handler-real-file-exists-p filename)))))

(efs-overwrite-fn "fn-handler" 'file-exists-p 'fn-handler-file-exists-p)

(defun fn-handler-write-region (start end filename &optional append visit)
  "Documented as original"
  ;; Use read-file-name, rather then interactive spec,
  ;; to make it easier to get decent initial contents in the minibuffer.
  (interactive
   (progn
     (or (mark) (error "The mark is not set now."))
     (list (min (point) (mark))
	   (max (point) (mark))
	   (read-file-name "Write region to file: "))))
  (let* ((filename (expand-file-name filename))
	 (handler (or (find-file-name-handler filename 'write-region)
		      (and (stringp visit)
			   (find-file-name-handler (expand-file-name visit)
						   'write-region)))))
    (if handler
	(funcall handler 'write-region start end filename append visit)
      (let (file-name-handler-alist)
	(fn-handler-real-write-region start end filename append visit)))))

(efs-overwrite-fn "fn-handler" 'write-region
		  'fn-handler-write-region)

(defun fn-handler-verify-visited-file-modtime (buffer)
  "Documented as original"
  (let* ((file (buffer-file-name buffer))
	 (handler (and file (find-file-name-handler
			     file
			     'verify-visited-file-modtime))))
    (if handler
	(funcall handler 'verify-visited-file-modtime buffer)
      (let (file-name-handler-alist)
	(fn-handler-real-verify-visited-file-modtime buffer)))))

(efs-overwrite-fn "fn-handler" 'verify-visited-file-modtime
		  'fn-handler-verify-visited-file-modtime)

(defun fn-handler-backup-buffer ()
  "Documented as original"
  (let ((handler (and buffer-file-name
		      (find-file-name-handler buffer-file-name
					      'backup-buffer))))
    (if handler
	(funcall handler 'backup-buffer)
      ;; Don't let-bind file-name-handler-alist to nil, as backup-buffer
      ;; is a lisp function and I want handlers to be available inside it.
      (fn-handler-real-backup-buffer))))

(efs-overwrite-fn "fn-handler" 'backup-buffer 'fn-handler-backup-buffer)

(defun fn-handler-copy-file (filename newname &optional ok-if-already-exists
				  keep-date)
  "Documented as original"
  ;; handler for filename takes precedence over the handler for newname.
  (interactive
   (let* ((from (read-file-name "Copy file: " nil nil t))
	  (to (read-file-name (format "Copy %s to: " (abbreviate-file-name
						      from)))))
     (list from to 0 current-prefix-arg)))
  (let* ((filename (expand-file-name filename))
	 (newname (expand-file-name newname))
	 (handler (or (find-file-name-handler filename 'copy-file)
		      (find-file-name-handler newname 'copy-file))))
    (if handler
	;; Using the NOWAIT arg is a bit risky for other users of the
	;; handler-alist
	(funcall handler 'copy-file filename newname ok-if-already-exists
		 keep-date)
      (let (file-name-handler-alist)
	(fn-handler-real-copy-file filename newname ok-if-already-exists
			    keep-date)))))
    
(efs-overwrite-fn "fn-handler" 'copy-file 'fn-handler-copy-file)

(defun fn-handler-file-newer-than-file-p (file1 file2)
  "Documented as original"
  ;; The handler for file2 takes precedence over the handler for file1.
  (let* ((file1 (expand-file-name file1))
	 (file2 (expand-file-name file2))
	 (handler (or (find-file-name-handler file2 'file-newer-than-file-p)
		      (find-file-name-handler file1 'file-newer-than-file-p))))
    (if handler
	(funcall handler 'file-newer-than-file-p file1 file2)
      (let (file-name-handler-alist)
	(fn-handler-real-file-newer-than-file-p file1 file2)))))

(efs-overwrite-fn "fn-handler" 'file-newer-than-file-p
		  'fn-handler-file-newer-than-file-p)

(defun fn-handler-file-attributes (file)
  "Documented as original"
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'file-attributes)))
    (if handler
	(funcall handler 'file-attributes file)
      (let (file-name-handler-alist)
	(fn-handler-real-file-attributes file)))))

(efs-overwrite-fn "fn-handler" 'file-attributes 'fn-handler-file-attributes)

(defun fn-handler-file-name-directory (file)
  "Documented as original"
  (let ((handler (find-file-name-handler file 'file-name-directory)))
    (if handler
	(funcall handler 'file-name-directory file)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-directory file)))))

(efs-overwrite-fn "fn-handler" 'file-name-directory
		  'fn-handler-file-name-directory)

(defun fn-handler-rename-file (filename newname &optional ok-if-already-exists)
  "Documented as original"
  (interactive
   (let* ((from (read-file-name "Rename file: " nil nil t))
	  (to (read-file-name (format "Rename %s to: " (abbreviate-file-name
							from)))))
     (list from to 0)))
  (let* ((filename (expand-file-name filename))
	 (newname (expand-file-name newname))
	 (handler (or (find-file-name-handler filename 'rename-file)
		      (find-file-name-handler newname 'rename-file))))
    (if handler
	(funcall handler 'rename-file filename newname ok-if-already-exists)
      (let (file-name-handler-alist)
	(fn-handler-real-rename-file filename newname ok-if-already-exists)))))

(efs-overwrite-fn "fn-handler" 'rename-file 'fn-handler-rename-file)

(defun fn-handler-insert-directory (file switches
				     &optional wildcard full-directory-p)
  "Documented as original"
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches wildcard
		 full-directory-p)
      (let (file-name-handler-alist)
	(fn-handler-real-insert-directory file switches wildcard
					  full-directory-p)))))

(efs-overwrite-fn "fn-handler" 'insert-directory 'fn-handler-insert-directory)

(defun fn-handler-set-visited-file-modtime (&optional time)
  "Sets the buffer's record of file modtime to the modtime of buffer-file-name.
With optional TIME, sets the modtime to TIME. This is an emacs 19 function.
In emacs 18, efs will make this work for remote files only."
  (if buffer-file-name
      (let ((handler (find-file-name-handler buffer-file-name
					     'set-visited-file-modtime)))
	(if handler
	    (funcall handler 'set-visited-file-modtime time)
	  (let (file-name-handler-alist)
	    (fn-handler-real-set-visited-file-modtime time))))))

(efs-overwrite-fn "fn-handler" 'set-visited-file-modtime
		  'fn-handler-set-visited-file-modtime)

(defun fn-handler-file-name-nondirectory (name)
  "Documented as original"
  (let ((handler (find-file-name-handler name 'file-name-nondirectory)))
    (if handler
	(funcall handler 'file-name-nondirectory name)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-nondirectory name)))))

(efs-overwrite-fn "fn-handler" 'file-name-nondirectory
		  'fn-handler-file-name-nondirectory)

(defun fn-handler-file-name-as-directory (name)
  "Documented as original"
  (let ((handler (find-file-name-handler name 'file-name-as-directory)))
    (if handler
	(funcall handler 'file-name-as-directory name)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-as-directory name)))))

(efs-overwrite-fn "fn-handler" 'file-name-as-directory
		  'fn-handler-file-name-as-directory)

(defun fn-handler-directory-file-name (directory)
  "Documented as original"
  (let ((handler (find-file-name-handler directory 'directory-file-name)))
    (if handler
	(funcall handler 'directory-file-name directory)
      (let (file-name-handler-alist)
	(fn-handler-real-directory-file-name directory)))))

(efs-overwrite-fn "fn-handler" 'directory-file-name
		  'fn-handler-directory-file-name)

(defun fn-handler-get-file-buffer (file)
  "Documented as original"
  (let ((handler (find-file-name-handler file 'get-file-buffer)))
    (if handler
	(funcall handler 'get-file-buffer file)
      (let (file-name-handler-alist)
	(fn-handler-real-get-file-buffer file)))))

(efs-overwrite-fn "fn-handler" 'get-file-buffer 'fn-handler-get-file-buffer)

(defun fn-handler-create-file-buffer (file)
  "Documented as original"
  (let ((handler (find-file-name-handler file 'create-file-buffer)))
    (if handler
	(funcall handler 'create-file-buffer file)
      (let (file-name-handler-alist)
	(fn-handler-real-create-file-buffer file)))))

(efs-overwrite-fn "fn-handler" 'create-file-buffer
		  'fn-handler-create-file-buffer)

(defun fn-handler-set-file-modes (file mode)
  "Documented as original"
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'set-file-modes)))
    (if handler
	(funcall handler 'set-file-modes file mode)
      (let (file-name-handler-alist)
	(fn-handler-real-set-file-modes file mode)))))

(efs-overwrite-fn "fn-handler" 'set-file-modes 'fn-handler-set-file-modes)

(defun fn-handler-file-modes (file)
  "Documented as original"
  (let* ((file (expand-file-name file))
	 (handler (find-file-name-handler file 'file-modes)))
    (if handler
	(funcall handler 'file-modes file)
      (let (file-name-handler-alist)
	(fn-handler-real-file-modes file)))))

(efs-overwrite-fn "fn-handler" 'file-modes 'fn-handler-file-modes)

(if (string-match emacs-version "Lucid")
    
    (progn
      (defun fn-handler-abbreviate-file-name (filename &optional hack-homedir)
	"Documented as original"
	(let ((handler (find-file-name-handler filename
					       'abbreviate-file-name)))
	  (if handler
	      (funcall handler 'abbreviate-file-name filename hack-homedir)
	    (let (file-name-handler-alist)
	      (fn-handler-real-abbreviate-file-name filename hack-homedir))))))
  
  (defun fn-handler-abbreviate-file-name (filename)
    "Documented as original"
    (let ((handler (find-file-name-handler filename 'abbreviate-file-name)))
      (if handler
	  (funcall handler 'abbreviate-file-name filename)
	(let (file-name-handler-alist)
	  (fn-handler-real-abbreviate-file-name filename))))))
  
(efs-overwrite-fn "fn-handler" 'abbreviate-file-name
		  'fn-handler-abbreviate-file-name)

(defun fn-handler-file-name-sans-versions (filename
					   &optional keep-backup-version)
  "Documented as original"
  (let ((handler (find-file-name-handler filename 'file-name-sans-versions)))
    (if handler
	(funcall handler 'file-name-sans-versions filename
		 keep-backup-version)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-sans-versions filename
						 keep-backup-version)))))

(efs-overwrite-fn "fn-handler" 'file-name-sans-versions
		  'fn-handler-file-name-sans-versions)

(if (fboundp 'make-directory-internal) ; not defined in lemacs 19.[67]
    (progn
      (defun fn-handler-make-directory-internal (dirname)
	"Documented as original"
	(let* ((dirname (expand-file-name dirname))
	       (handler (find-file-name-handler dirname
						'make-directory-internal)))
	  (if handler
	      (funcall handler 'make-directory-internal dirname)
	    (let (file-name-handler-alist)
	      (fn-handler-real-make-directory-internal dirname)))))
      
      (efs-overwrite-fn "fn-handler" 'make-directory-internal
			'fn-handler-make-directory-internal)))

(defun fn-handler-delete-directory (dirname)
  "Documented as original"
  (let* ((dirname (expand-file-name dirname))
	 (handler (find-file-name-handler dirname 'delete-directory)))
    (if handler
	(funcall handler 'delete-directory dirname)
      (let (file-name-handler-alist)
	(fn-handler-real-delete-directory dirname)))))

(efs-overwrite-fn "fn-handler" 'delete-directory 'fn-handler-delete-directory)
		  
(defun fn-handler-make-symbolic-link (target linkname
					 &optional ok-if-already-exists)
  "Documented as original"
  (interactive
   (let (target)
      (list
       (setq target (read-string "Make symbolic link to file: "))
       (read-file-name (format "Make symbolic link to file %s: " target))
       0)))
  (let* ((linkname (expand-file-name linkname))
	 (handler (or (find-file-name-handler linkname 'make-symbolic-link)
		      (find-file-name-handler target 'make-symbolic-link))))
    (if handler
	(funcall handler 'make-symbolic-link
		 target linkname ok-if-already-exists)
      (let (file-name-handler-alist)
	(fn-handler-real-make-symbolic-link target linkname
					    ok-if-already-exists)))))

(efs-overwrite-fn "fn-handler" 'make-symbolic-link
		  'fn-handler-make-symbolic-link)

(defun fn-handler-add-name-to-file (file newname &optional
					 ok-if-already-exists)
  "Documented as original"
  (interactive
   (let (file)
     (list
      (setq file (read-file-name "Add name to file: " nil nil t))
      (read-file-name (format "Name to add to %s: " file))
      0)))
  (let* ((file (expand-file-name file))
	 (newname (expand-file-name newname))
	 (handler (or (find-file-name-handler newname 'add-name-to-file)
		      (find-file-name-handler file 'add-name-to-file))))
    (if handler
	(funcall handler 'add-name-to-file file newname ok-if-already-exists)
      (let (file-name-handler-alist)
	(fn-handler-real-add-name-to-file file newname
					  ok-if-already-exists)))))

(efs-overwrite-fn "fn-handler" 'add-name-to-file 'fn-handler-add-name-to-file)

(defun fn-handler-recover-file (file)
  "Documented as original"
  (interactive "FRecover file: ")
  (let* ((file (expand-file-name file))
	 (handler (or (find-file-name-handler file 'recover-file)
		      (find-file-name-handler (let ((buffer-file-name file))
						(make-auto-save-file-name))
					      'recover-file))))
    (if handler
	(funcall handler 'recover-file file)
      (let (file-name-handler-alist)
	(fn-handler-real-recover-file file)))))

(efs-overwrite-fn "fn-handler" 'recover-file 'fn-handler-recover-file)

(defun fn-handler-file-name-completion (file dir)
  "Documented as original."
  (let* ((dir (expand-file-name dir))
	 (handler (find-file-name-handler dir 'file-name-completion)))
    (if handler
	(funcall handler 'file-name-completion file dir)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-completion file dir)))))

(efs-overwrite-fn "fn-handler" 'file-name-completion
		  'fn-handler-file-name-completion)

(defun fn-handler-file-name-all-completions (file dir)
  "Documented as original."
  (let* ((dir (expand-file-name dir))
	 (handler (find-file-name-handler dir 'file-name-all-completions)))
    (if handler
	(funcall handler 'file-name-all-completions file dir)
      (let (file-name-handler-alist)
	(fn-handler-real-file-name-all-completions file dir)))))

(efs-overwrite-fn "fn-handler" 'file-name-all-completions
		  'fn-handler-file-name-all-completions)

(if (fboundp 'file-truename)
    (progn
      (defun fn-handler-file-truename (filename)
	"Documented as original"
	(let* ((fn (expand-file-name filename))
	       (handler (find-file-name-handler filename 'file-truename)))
	  (if handler
	      (funcall handler 'file-truename filename)
	    (let (file-name-handler-alist)
	      (fn-handler-real-file-truename filename)))))
      (efs-overwrite-fn "fn-handler" 'file-truename
			'fn-handler-file-truename)))

(if (fboundp 'unhandled-file-name-directory)
    (progn
      (defun fn-handler-unhandled-file-name-directory (filename)
	"Documented as original"
	(let ((handler (find-file-name-handler
			filename 'unhandled-file-name-directory)))
	  (if handler
	      (funcall handler  'unhandled-file-name-directory filename)
	    (let (file-name-handler-alist)
	      (fn-handler-real-unhandled-file-name-directory filename)))))
      
      (efs-overwrite-fn "fn-handler" 'unhandled-file-name-directory
			'fn-handler-unhandled-file-name-directory)))


;; We don't need the file-name-handler-alist for these.  Inhibit it to
;; avoid an infinite recursion.  Hope that this doesn't step
;; on any other packages' toes.
(defun fn-handler-expand-file-name (filename &optional default)
  "Documented as original."
  (let (file-name-handler-alist)
    (fn-handler-real-expand-file-name filename default)))

(efs-overwrite-fn "fn-handler" 'expand-file-name 'fn-handler-expand-file-name)

(defun fn-handler-substitute-in-file-name (filename)
  "Documented as original."
  (let ((handler (find-file-name-handler filename 'substitute-in-file-name)))
    (if handler
	(funcall handler 'substitute-in-file-name filename)
      (let (file-name-handler-alist)
	(fn-handler-real-substitute-in-file-name filename)))))

(efs-overwrite-fn "fn-handler" 'substitute-in-file-name
		  'fn-handler-substitute-in-file-name)

(if (fboundp 'file-executable-p)
    (progn
      (defun fn-handler-file-executable-p (file)
	(let ((handler (find-file-name-handler file 'file-executable-p)))
	  (if handler
	      (funcall handler 'file-executable-p file)
	    (let (file-name-handler-alist)
	      (fn-handler-real-file-executable-p file)))))
      (efs-overwrite-fn "fn-handler" 'file-executable-p
			'fn-handler-file-executable-p)))

(if (fboundp 'file-accessible-directory-p)
    (progn
      (defun fn-handler-file-accessible-directory-p (file)
	(let ((handler (find-file-name-handler file
					       'file-accessible-directory-p)))
	  (if handler
	      (funcall handler 'file-accessible-directory-p file)
	    (let (file-name-handler-alist)
	      (fn-handler-real-file-accessible-directory-p file)))))
      (efs-overwrite-fn "fn-handler" 'file-accessible-directory-p
			'fn-handler-file-accessible-directory-p)))

(defun fn-handler-load (file &optional noerror nomessage nosuffix)
  (let ((handler (find-file-name-handler file 'load)))
    (if handler
	(funcall handler 'load file noerror nomessage nosuffix)
      (let (file-name-handler-alist)
	(fn-handler-real-load file noerror nomessage nosuffix)))))

(efs-overwrite-fn "fn-handler" 'load 'fn-handler-load)

;; We don't need file-name-handlers for do-auto-save.
;; If it does try to access them there is a risk of an infinite recursion.
(defun fn-handler-do-auto-save (&rest args)
  "Documented as original."
  (let (file-name-handler-alist)
    (apply 'fn-handler-real-do-auto-save args)))

(efs-overwrite-fn "fn-handler" 'do-auto-save 'fn-handler-do-auto-save)

(if (fboundp 'vc-registered)
    (progn
      (defun fn-handler-vc-registered (file)
	"Documented as original."
	(let ((handler (find-file-name-handler file 'vc-registered)))
	  (if handler
	      (funcall handler 'vc-registered file)
	    (let (file-name-handler-alist)
	      (fn-handler-real-vc-registered file)))))
      
      (efs-overwrite-fn "fn-handler" 'vc-registered
			'fn-handler-vc-registered)))

;;; end of fn-handler.el
