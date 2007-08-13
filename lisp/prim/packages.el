;;; packages.el --- Low level support for XEmacs packages

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@altair.xemacs.org>
;; Keywords: internal, lisp

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

;; This file provides low level facilities for XEmacs startup.  Special
;; requirements apply to some of these functions because they can be called
;; during build from temacs and much of the usual lisp environment may
;; be missing.

;;; Code:

(defvar autoload-file-name "auto-autoloads.el"
  "Filename that autoloads are expected to be found in.")

(defvar packages-hardcoded-lisp
  '("cl-defs"
    ;; "startup"
    )
  "Lisp packages that are always dumped with XEmacs")

(defvar packages-useful-lisp
  '("bytecomp"
    "byte-optimize"
    "advice")
  "Lisp packages that need early byte compilation.")

(defvar packages-unbytecompiled-lisp
  '("paths.el"
    "version.el")
  "Lisp packages that should not be byte compiled.")

;; Copied from subr.el
(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  ;; #### - I don't see why.  So long as backquote.el doesn't use anything
  ;; from subr.el, there's no problem with using backquotes here.  --Stig 
  ;;(list 'function (cons 'lambda cdr)))
  ;; -slb, This has to run in a naked temacs.  Enough is enough.
  ;; `(function (lambda ,@cdr)))
  (list 'function (cons 'lambda cdr)))

;; Copied from help.el, could possibly move it to here permanently.
;; This is taken directly from Emacs 19.34.94.

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
  (let (result)
    (catch 'answer
      (mapcar
       (lambda (dir)
         (mapcar
          (lambda (suf)
            (let ((try (expand-file-name (concat library suf) dir)))
              (and (file-readable-p try)
                   (null (file-directory-p try))
                   (progn
                     (setq result try)
                     (throw 'answer try)))))
          (if nosuffix
              '("")
            (let ((basic '(".elc" ".el" ""))
                  (compressed '(".Z" ".gz" "")))
              ;; If autocompression mode is on,
              ;; consider all combinations of library suffixes
              ;; and compression suffixes.
              (if (or (rassq 'jka-compr-handler file-name-handler-alist)
		      (and (boundp 'find-file-hooks)
			   (member 'crypt-find-file-hook find-file-hooks)))
		  (apply 'nconc
			 (mapcar (lambda (compelt)
				   (mapcar (lambda (baselt)
					     (concat baselt compelt))
					   basic))
				 compressed))
		basic)))))
       (or path load-path)))
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
				 "/" autoload-file-name))
	  (setq autoloads (cons (concat (car path)
					"/" autoload-file-name)
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
(defun packages-find-packages-1 (package path-only)
  "Search the supplied directory for associated directories.
The top level is assumed to look like:
info/           Contain texinfo files for lisp installed in this hierarchy
etc/            Contain data files for lisp installled in this hiearchy
lisp/           Contain directories which either have straight lisp code
                or are self-contained packages of their own."
  ;; Info files
  (if (and (null path-only) (file-directory-p (concat package "/info")))
      (setq Info-default-directory-list
	    (cons (concat package "/info/") Info-default-directory-list)))
  ;; Data files
  (if (and (null path-only) (file-directory-p (concat package "/etc")))
      (setq data-directory-list
	    (cons (concat package "/etc/") data-directory-list)))
  ;; Lisp files
  (if (file-directory-p (concat package "/lisp"))
      (progn
	(setq load-path (cons (concat package "/lisp/") load-path))
	(let ((dirs (directory-files (concat package "/lisp/")
				     t "^[^-.]" nil 'dirs-only))
	      dir)
	  (while dirs
	    (setq dir (car dirs))
	    (setq load-path (cons (concat dir "/") load-path))
	    (packages-find-packages-1 dir path-only)
	    (setq dirs (cdr dirs)))))))

;; The following function is called from temacs
(defun packages-find-packages (pkg-path path-only)
  "Search the supplied path for additional info/etc/lisp directories.
Lisp directories if configured prior to build time will have equivalent
status as bundled packages."
  (let ((path pkg-path)
	dir)
    (while path
      (setq dir (car path))
      ;; (prin1 (concat "Find: " (expand-file-name dir) "\n"))
      (packages-find-packages-1 (expand-file-name dir) path-only)
      (setq path (cdr path)))))

;; Data-directory is really a list now.  Provide something to search it for
;; directories.

(defun locate-data-directory (name &optional data-dir-list)
  "Locate a directory in a search path."
  (unless data-dir-list
    (setq data-dir-list data-directory-list))
  (let (dir found found-dir (dirs data-dir-list))
    (while (and (null found-dir) dirs)
      (setq dir (car dirs))
      (setq found (concat dir name "/"))
      (setq found-dir (file-directory-p found))
      (setq dirs (cdr dirs)))
    found))

;; If we are being loaded as part of being dumped, bootstrap the rest of the
;; load-path for loaddefs.
(if (fboundp 'load-gc)
    (packages-find-packages package-path t))

(provide 'packages)

;;; packages.el ends here
