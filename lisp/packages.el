;;; packages.el --- Low level support for XEmacs packages

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@altair.xemacs.org>
;; Maintainer: Steven L Baur <steve@altair.xemacs.org>
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

;; This file requires find-paths.el.

;;; Code:

;;; Package versioning

(defvar packages-package-list nil
  "database of loaded packages and version numbers")

(defvar early-packages nil
  "Packages early in the load path.")

(defvar early-package-load-path nil
  "Load path for packages early in the load path.")

(defvar late-packages nil
  "Packages late in the load path.")

(defvar late-package-load-path nil
  "Load path for packages late in the load path.")

(defvar last-packages nil
  "Packages last in the load path.")

(defvar last-package-load-path nil
  "Load path for packages last in the load path.")

(defun package-get-key-1 (info key)
  "Locate keyword `key' in list."
  (cond ((null info)
	 nil)
	((eq (car info) key)
	 (nth 1 info))
	(t (package-get-key-1 (cddr info) key))))

(defun package-get-key (name key)
  "Get info `key' from package `name'."
  (let ((info (assq name packages-package-list)))
    (when info
      (package-get-key-1 (cdr info) key))))

(defun package-provide (name &rest attributes)
  (let ((info (if (and attributes (floatp (car attributes)))
		  (list :version (car attributes))
		attributes)))
    (remassq name packages-package-list)
    (setq packages-package-list
	  (cons (cons name info) packages-package-list))))

(defun package-require (name version)
  (let ((pkg (assq name packages-package-list)))
    (cond ((null pkg)
	   (error "Package %s has not been loaded into this XEmacsen"
		  name))
	  ((< (package-get-key name :version) version)
	   (error "Need version %g of package %s, got version %g"
		  version name (cdr pkg)))
	  (t t))))

;;; Build time stuff

(defvar autoload-file-name "auto-autoloads.el"
  "Filename that autoloads are expected to be found in.")

(defvar packages-hardcoded-lisp
  '(
    #+infodock "id-vers"
    #+infodock "easymenu-id-xemacs"
    )
  "Lisp packages that are always dumped with XEmacs")

(defvar packages-useful-lisp
  '("bytecomp"
    "byte-optimize"
    "shadow"
    "cl-macs")
  "Lisp packages that need early byte compilation.")

(defvar packages-unbytecompiled-lisp
  '("paths.el"
    "dumped-lisp.el"
    "dumped-pkg-lisp.el"
    "version.el"
    "Installation.el")
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

(defun packages-list-autoloads-path ()
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

(defun packages-list-autoloads ()
  "List autoload files in (what will be) the normal lisp search path.
This function is used during build to find where the global symbol files so
they can be perused for their useful information."
  ;; Source directory may not be initialized yet.
  ;; (print (prin1-to-string load-path))
  (if (null source-directory)
      (setq source-directory (concat (car load-path) "./")))
  (let ((files (directory-files (file-name-as-directory source-directory)
				t ".*"))
	file autolist)
    ;; (print (prin1-to-string source-directory))
    ;; (print (prin1-to-string files))
    (while (setq file (car-safe files))
      (if (and (file-directory-p file)
	       (file-exists-p (concat file "/" autoload-file-name)))
	  (setq autolist (cons (concat file "/" autoload-file-name)
			       autolist)))
      (setq files (cdr files)))
    autolist))

;; The following function cannot be called from a bare temacs
(defun packages-new-autoloads ()
  "Return autoloads files that have been added or modified since XEmacs dump."
  (require 'loadhist)
  (let ((me (concat invocation-directory invocation-name))
	(path load-path)
	result dir)
    (while path
      (setq dir (file-truename (car path)))
      (let ((autoload-file (file-name-sans-extension (concat
						      dir
						      autoload-file-name))))
	;; Check for:
	;; 1.  An auto-autoload file that hasn't provided a feature (because
	;;     it has been installed since XEmacs was dumped).
	;; 2.  auto-autoload.el being newer than the executable
	;; 3.  auto-autoload.elc being newer than the executable (the .el
	;;     could be missing or compressed)
	(when (or (and (null (file-provides autoload-file))
		       (or (file-exists-p (concat autoload-file ".elc"))
			   (file-exists-p (concat autoload-file ".el"))))
		  (and (file-newer-than-file-p (concat autoload-file ".el") me)
		       (setq autoload-file (concat autoload-file ".el")))
		  (and (file-newer-than-file-p (concat autoload-file
						       ".elc")
					       me)
		       (setq autoload-file (concat autoload-file ".elc"))))
	  (push autoload-file result)))
      (setq path (cdr path)))
    result))

;; The following function cannot be called from a bare temacs
(defun packages-reload-autoloads ()
  "Reload new or updated auto-autoloads files.
This is an extremely dangerous function to call after the user-init-files
is run.  Don't call it or you'll be sorry."
  (let ((autoload-list (packages-new-autoloads)))
    (while autoload-list
      (let* ((autoload-file (car autoload-list))
	     (feature (car-safe (file-provides autoload-file))))
	(when feature
	  ;; (message "(unload-feature %S)" feature)
	  (unload-feature feature))
	(condition-case nil
	    (load autoload-file)
	  (t nil)))
      (setq autoload-list (cdr autoload-list)))))

;; The following function cannot be called from a bare temacs
(defun packages-reload-dumped-lisp ()
  "Reload new or updated dumped lisp files (with exceptions).
This is an extremely dangerous function to call at any time."
  ;; Nothing for the moment
  nil)

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

;; Data-directory is really a list now.  Provide something to search it for
;; files.

(defun locate-data-file (name &optional dir-list)
  "Locate a file in a search path DIR-LIST (a list of directories).
If no DIR-LIST is supplied, it defaults to `data-directory-list'.
This function is basically a wrapper over `locate-file'."
  (unless dir-list
    (setq dir-list data-directory-list))
  (locate-file name dir-list))

;; Path setup

(defun packages-find-package-path (roots)
  "Construct the package path underneath installation roots ROOTS."
  (let ((envvar-value (getenv "EMACSPACKAGEPATH")))
    (if envvar-value
	(decode-path-internal envvar-value)
      (let ((site-base-directory (paths-find-site-directory roots "packages"))
	    (version-base-directory (paths-find-version-directory roots "packages")))
	(if (or site-base-directory version-base-directory)
	    (let ((site-mule-directory
		   (and (featurep 'mule)
			(paths-find-site-directory roots
						   "mule-packages")))
		  (version-mule-directory
		   (and (featurep 'mule)
			(paths-find-version-directory roots
						      "mule-packages")))
		  ;; There needs to be a cleverer way of doing this
		  (site-infodock-directory
		   (and (featurep 'infodock)
			(paths-find-site-directory roots
						   "infodock-packages")))
		  (version-infodock-directory
		   (and (featurep 'infodock)
			(paths-find-version-directory roots
						      "infodock-packages"))))
	      (append '("~/.xemacs/")
		      '(nil)
		      (and version-infodock-directory
			   (null (string-equal version-infodock-directory
					       site-infodock-directory))
			   (list version-infodock-directory))
		      (and site-infodock-directory
			   (list site-infodock-directory))
		      (and version-mule-directory
			   (null (string-equal version-mule-directory
					      site-mule-directory))
			   (list version-mule-directory))
		      (and site-mule-directory
			   (list site-mule-directory))
		      (and version-base-directory
			   (null (string-equal version-base-directory
					      site-base-directory))
			   (list version-base-directory))
		      (and site-base-directory
			   (list site-base-directory))))
	  configure-package-path)))))

(defvar packages-special-bases '("etc" "info" "lisp" "lib-src" "bin")
  "Special subdirectories of packages.")

(defun packages-find-packages-in-directories (directories)
  "Find all packages underneath directories in DIRECTORIES."
  (paths-find-recursive-path directories
			     (append paths-version-control-bases
				     packages-special-bases)))

(defun packages-split-path (path)
  "Split PATH at NIL, return pair with two components.
The second component is shared with PATH."
  (let ((reverse-tail '())
	(rest path))
    (while (and rest (null (null (car rest))))
      (setq reverse-tail (cons (car rest) reverse-tail))
      (setq rest (cdr rest)))
    (if (null rest)
	(cons path nil)
      (cons (nreverse reverse-tail) (cdr rest)))))

(defun packages-find-packages (package-path &optional inhibit)
  "Search for all packages in PACKAGE-PATH.
PACKAGE-PATH may distinguish (by NIL-separation) between early,
late and last packages.
If INHIBIT is non-NIL, return empty paths.
This returns (LIST EARLY-PACKAGES LATE-PACKAGES LAST-PACKAGES)."
  (if inhibit
      (list '() '() '())
    ;; When in doubt, it's late
    (let* ((stuff (packages-split-path package-path))
	   (early (and (cdr stuff) (car stuff)))
	   (late+last (or (cdr stuff) (car stuff)))
	   (stuff (packages-split-path late+last))
	   (late (car stuff))
	   (last (cdr stuff)))
      (list (packages-find-packages-in-directories early)
	    (packages-find-packages-in-directories late)
	    (packages-find-packages-in-directories last)))))

(defun packages-find-package-library-path (packages suffixes)
  "Construct a path into a component of the packages hierarchy.
PACKAGES is a list of package directories.
SUFFIXES is a list of names of package subdirectories to look for."
  (let ((directories
	 (apply
	  #'append
	  (mapcar #'(lambda (package)
		      (mapcar #'(lambda (suffix)
				  (concat package suffix))
			      suffixes))
		  packages))))
    (paths-directories-which-exist directories)))

(defun packages-find-package-load-path (packages)
  "Construct the load-path component for packages.
PACKAGES is a list of package directories."
  (paths-find-recursive-load-path
   (packages-find-package-library-path packages '("lisp/"))))

(defun packages-find-package-exec-path (packages)
  (packages-find-package-library-path packages
				   (list (concat "bin/" system-configuration "/")
					 "lib-src/")))

(defun packages-find-package-info-path (packages)
  (packages-find-package-library-path packages '("info/")))

(defun packages-find-package-data-path (packages)
  (packages-find-package-library-path packages '("etc/")))

;; Loading package initialization files

(defun packages-load-package-lisps (package-load-path base)
  "Load all Lisp files of a certain name along a load path.
BASE is the base name of the files."
  (mapc #'(lambda (dir)
	    (let ((file-name (expand-file-name base dir)))
	      (if (file-exists-p file-name)
		  (condition-case error
		      (load file-name)
		    (error
		     (warn (format "Autoload error in: %s:\n\t%s"
				   file-name
				   (with-output-to-string
				     (display-error error nil)))))))))
	package-load-path))

(defun packages-load-package-auto-autoloads (package-load-path)
  "Load auto-autoload files along a load path."
  (packages-load-package-lisps package-load-path
			       (file-name-sans-extension autoload-file-name)))

(defun packages-handle-package-dumped-lisps (handle package-load-path)
  "Load dumped-lisp.el files along a load path.
Call HANDLE on each file off definitions of PACKAGE-LISP there."
  (mapc #'(lambda (dir)
	    (let ((file-name (expand-file-name "dumped-lisp.el" dir)))
	      (if (file-exists-p file-name)
		  (let (package-lisp
			;; 20.4 packages could set this
			preloaded-file-list)
		    (load file-name)
		    ;; dumped-lisp.el could have set this ...
		    (if package-lisp
			(mapc #'(lambda (base)
				  (funcall handle base))
			      package-lisp))))))
	  package-load-path))

(defun packages-load-package-dumped-lisps (package-load-path)
  "Load dumped-lisp.el files along a load path.
Also load files off PACKAGE-LISP definitions there"
  (packages-handle-package-dumped-lisps #'load package-load-path))

(defun packages-collect-package-dumped-lisps (package-load-path)
  "Load dumped-lisp.el files along a load path.
Return list of files off PACKAGE-LISP definitions there"
  (let ((*files* '()))
    (packages-handle-package-dumped-lisps
     #'(lambda (file)
	 (setq *files* (cons file *files*)))
     package-load-path)
    (reverse *files*)))

(provide 'packages)

;;; packages.el ends here
