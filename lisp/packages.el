;;; packages.el --- Low level support for XEmacs packages

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@altair.xemacs.org>
;; Keywords: internal, lisp, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file provides low level facilities for XEmacs startup --
;; particularly regarding the package setup.  This code has to run in
;; what we call "bare temacs" -- i.e. XEmacs without the usual Lisp
;; environment.  Pay special attention:

;; - not to use the `lambda' macro.  Use #'(lambda ...) instead.
;;   (this goes for any package loaded before `subr.el'.)
;;
;; - not to use macros, because they are not yet available (and this
;;   file must be loadable uncompiled.)  This rules out CL-style
;;   macros like `when', for instance.
;;
;; - not to use `defcustom'.  If you must add user-customizable
;;   variables here, use `defvar', and add the variable to
;;   `cus-start.el'.

;; Because of all this, make sure that the stuff you put here really
;; belongs here.


;;; Code:

(defvar autoload-file-name "auto-autoloads.el"
  "Filename that autoloads are expected to be found in.")

(defvar packages-hardcoded-lisp
  '(
    ;; "startup"
    )
  "Lisp packages that are always dumped with XEmacs")

(defvar packages-useful-lisp
  '("bytecomp"
    "byte-optimize"
    "advice"
    "shadow"
    "cl-macs")
  "Lisp packages that need early byte compilation.")

(defvar packages-unbytecompiled-lisp
  '("paths.el"
    "version.el")
  "Lisp packages that should not be byte compiled.")


;; Copied from help.el, could possibly move it to here permanently.
;; Unlike the FSF version, our `locate-library' uses the `locate-file'
;; primitive, which should make it lightning-fast.

(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'."
  (interactive (list (read-string "Locate library: ")
                     nil nil
                     t))
  (let ((result
	 (locate-file
	  library
	  (or path load-path)
	  (cond ((or (rassq 'jka-compr-handler file-name-handler-alist)
		     (and (boundp 'find-file-hooks)
			  (member 'crypt-find-file-hook find-file-hooks)))
		 ;; Compression involved.
		 (if nosuffix
		     ":.gz:.Z"
		   ".elc:.elc.gz:elc.Z:.el:.el.gz:.el.Z::.gz:.Z"))
		(t
		 ;; No compression.
		 (if nosuffix
		     ""
		   ".elc:.el:")))
	  4)))
    (and interactive-call
	 (if result
	     (message "Library is file %s" result)
	   (message "No library %s in search path" library)))
    result))

(defun packages-add-suffix (str)
  (if (null (string-match "\\.el\\'" str))
      (concat str ".elc")
    str))

(defun list-autoloads-path ()
  "List autoloads from precomputed load-path."
  (let ((path load-path)
	autoloads)
    (while path
      (if (file-exists-p (concat (car path)
				 autoload-file-name))
	  (setq autoloads (cons (concat (car path)
					autoload-file-name)
				autoloads)))
      (setq path (cdr path)))
    autoloads))

(defun list-autoloads ()
  "List autoload files in (what will be) the normal lisp search path.
This function is used during build to find where the global symbol files so
they can be perused for their useful information."
  ;; Source directory may not be initialized yet.
  ;; (print (prin1-to-string load-path))
  (if (null source-directory)
      (setq source-directory (concat (car load-path) "/..")))
  (let ((files (directory-files source-directory t ".*"))
	file autolist)
    (while (setq file (car-safe files))
      (if (and (file-directory-p file)
	       (file-exists-p (concat file "/" autoload-file-name)))
	  (setq autolist (cons (concat file "/" autoload-file-name)
			       autolist)))
      (setq files (cdr files)))
    autolist))

;; The following function is called from temacs
(defun packages-find-packages-1 (package path-only user-package)
  "Search the supplied directory for associated directories.
The top level is assumed to look like:
info/           Contain texinfo files for lisp installed in this hierarchy
etc/            Contain data files for lisp installled in this hiearchy
lisp/           Contain directories which either have straight lisp code
                or are self-contained packages of their own.

This is an internal function.  Do not call it after startup."
  ;; Info files
  (if (and (null path-only) (file-directory-p (concat package "/info")))
      (let ((dir (concat package "/info/")))
	(if (not (member dir Info-default-directory-list))
	    (nconc Info-default-directory-list (list dir)))))
  ;; Data files
  (if (and (null path-only) (file-directory-p (concat package "/etc")))
      (setq data-directory-list
	    (cons (concat package "/etc/") data-directory-list)))
  ;; Lisp files
  (if (file-directory-p (concat package "/lisp"))
      (progn
;	(print (concat "DIR: "
;		       (if user-package "[USER]" "")
;		       package
;		       "/lisp/"))
	(setq load-path (cons (concat package "/lisp/") load-path))
	(if user-package
	    (condition-case nil
		(load (concat package "/lisp/"
			      (file-name-sans-extension autoload-file-name)))
	      (t nil)))
	(let ((dirs (directory-files (concat package "/lisp/")
				     t "^[^-.]" nil 'dirs-only))
	      dir)
	  (while dirs
	    (setq dir (car dirs))
;	    (print (concat "DIR: " dir "/"))
	    (setq load-path (cons (concat dir "/") load-path))
	    (if user-package
		(condition-case nil
		    (progn
;		      (print
;		       (concat dir "/"
;			       (file-name-sans-extension autoload-file-name)))
		      (load
		       (concat dir "/"
			       (file-name-sans-extension autoload-file-name))))
		  (t nil)))
	    (packages-find-packages-1 dir path-only user-package)
	    (setq dirs (cdr dirs)))))))

;; The following function is called from temacs
(defun packages-find-packages (pkg-path path-only &optional suppress-user)
  "Search the supplied path for additional info/etc/lisp directories.
Lisp directories if configured prior to build time will have equivalent
status as bundled packages.
If the argument `path-only' is non-nil, only the `load-path' will be set,
otherwise data directories and info directories will be added.
If the optional argument `suppress-user' is non-nil, package directories
rooted in a user login directory (like ~/.xemacs) will not be searched.
This is used at dump time to suppress the builder's local environment."
  (let ((path (reverse pkg-path))
	dir)
    (while path
      (setq dir (car path))
      ;; (prin1 (concat "Find: " (expand-file-name dir) "\n"))
      (if (null (and (or suppress-user inhibit-package-init)
		     (string-match "^~" dir)))
	  (progn
	    ;; (print dir)
	    (packages-find-packages-1 (expand-file-name dir)
				      path-only
				      (string-match "^~" dir))))
      (setq path (cdr path)))))

;; Data-directory is really a list now.  Provide something to search it for
;; directories.

(defun locate-data-directory (name &optional dir-list)
  "Locate a directory in a search path DIR-LIST (a list of directories).
If no DIR-LIST is supplied, it defaults to `data-directory-list'."
  (unless dir-list
    (setq dir-list data-directory-list))
  (let (found found-dir)
    (while (and (null found-dir) dir-list)
      (setq found (concat (car dir-list) name "/")
	    found-dir (file-directory-p found))
      (or found-dir
	  (setq found nil))
      (setq dir-list (cdr dir-list)))
    found))

;; If we are being loaded as part of being dumped, bootstrap the rest of the
;; load-path for loaddefs.
(if (fboundp 'load-gc)
    (packages-find-packages package-path t t))

(provide 'packages)

;;; packages.el ends here
