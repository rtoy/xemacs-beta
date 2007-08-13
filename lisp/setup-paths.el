;;; setup-paths.el --- setup various XEmacs paths

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Author: Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file contains the machinery necessary to find the various
;; paths into the XEmacs hierarchy.

(defvar paths-version-control-bases '("RCS" "CVS" "SCCS")
  "File bases associated with version control.")

(defun paths-find-recursive-path (directories &optional exclude)
  "Return a list of the directory hierarchy underneath DIRECTORIES.
The returned list is sorted by pre-order and lexicographically."
  (let ((path '()))
    (while directories
      (let ((directory (file-name-as-directory
			(expand-file-name
			 (car directories)))))
	(if (file-directory-p directory)
	    (let ((raw-dirs (directory-files directory nil "^[^-.]" nil 'dirs-only))
		  (reverse-dirs '()))

	      (while raw-dirs
		(if (null (member (car raw-dirs) exclude))
		    (setq reverse-dirs
			  (cons (expand-file-name (car raw-dirs) directory)
				reverse-dirs)))
		(setq raw-dirs (cdr raw-dirs)))

	      (let ((sub-path
		     (paths-find-recursive-path (reverse reverse-dirs) exclude)))
		(setq path (nconc path
				  (list directory)
				  sub-path))))))
      (setq directories (cdr directories)))
    path))

(defun paths-find-recursive-load-path (directories)
  "Construct a recursive load path underneath DIRECTORIES."
  (paths-find-recursive-path directories paths-version-control-bases))

(defun paths-emacs-root-p (directory)
  "Check if DIRECTORY is a plausible installation root for XEmacs."
  (or
   ;; installed
   (and (boundp 'emacs-version)
	(file-directory-p
	 (concat directory "lib/xemacs-" (construct-emacs-version))))
   ;; in-place
   (and 
    (file-directory-p (concat directory "lib-src"))
    (file-directory-p (concat directory "lisp"))
    (file-directory-p (concat directory "src")))))

(defun paths-find-emacs-root
  (invocation-directory invocation-name)
  "Find the run-time root of XEmacs."
  (let ((maybe-root-1 (file-name-as-directory
		       (expand-file-name ".." invocation-directory)))
	(maybe-root-2 (file-name-as-directory
		       (expand-file-name "../.." invocation-directory))))
    (cond
     ((paths-emacs-root-p maybe-root-1)
      maybe-root-1)
     ((paths-emacs-root-p maybe-root-2)
      maybe-root-2)
     (t
      (let ((maybe-symlink (file-symlink-p (concat invocation-directory
						   invocation-name))))
	(if maybe-symlink
	    (let ((directory (file-name-directory maybe-symlink)))
	      (paths-find-emacs-root directory invocation-name))
	  nil))))))

(defun paths-construct-emacs-directory (root suffix base)
  "Construct a directory name within the XEmacs hierarchy."
  (file-name-as-directory
   (expand-file-name 
    (concat
     (file-name-as-directory root)
     suffix
     base))))

(defun paths-find-emacs-directory (roots suffix base &optional envvar default)
  "Find a directory in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
SUFFIX is the subdirectory from there.
BASE is the base to look for.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is a fall-back value."
  (let ((envvar-value (and envvar (getenv envvar))))
    (if (and envvar-value
	     (file-directory-p envvar-value))
	(file-name-as-directory envvar-value)
      (catch 'gotcha
	(while roots
	  (let* ((root (car roots))
		 (path (paths-construct-emacs-directory root suffix base)))
	    ;; installed
	    (if (file-directory-p path)
		(throw 'gotcha path)
	      (let ((path (paths-construct-emacs-directory root "" base)))
		;; in-place
		(if (file-directory-p path)
		    (throw 'gotcha path)))))
	  (setq roots (cdr roots)))
	(if (and default
		 (file-directory-p default))
	    (file-name-as-directory default)
	  nil)))))

(defun paths-find-site-directory (roots base &optional envvar default)
  "Find a site-specific directory in the XEmacs hierarchy."
  (paths-find-emacs-directory roots "lib/xemacs/" base envvar default))

(defun paths-find-version-directory (roots base &optional envvar default)
  "Find a version-specific directory in the XEmacs hierarchy."
  (paths-find-emacs-directory roots
			      (concat "lib/xemacs-" (construct-emacs-version) "/")
			      base
			      envvar default))

(defun paths-find-architecture-directory (roots base &optional envvar default)
  "Find an architecture-specific directory in the XEmacs hierarchy."
  (or
   ;; from more to less specific
   (paths-find-version-directory roots
				 (concat base system-configuration)
				 envvar default)
   (paths-find-version-directory roots
				 system-configuration
				 envvar default)
   (paths-find-version-directory roots
				 base
				 envvar default)))
  
(defvar paths-path-emacs-version nil
  "Emacs version as it appears in paths.")

(defun construct-emacs-version ()
  "Construct the raw version number of XEmacs in the form XX.XX."
  ;; emacs-version isn't available early, but we really don't care then
  (if (null (boundp 'emacs-version))
      ""
  (or paths-path-emacs-version		; cache
      (progn
	(string-match "\\`[^0-9]*\\([0-9]+\\.[0-9]+\\)" emacs-version)
	(let ((version (substring emacs-version
				  (match-beginning 1) (match-end 1))))
	  (if (string-match "(beta *\\([0-9]+\\))" emacs-version)
	      (setq version (concat version
				    "-b"
				    (substring emacs-version
					       (match-beginning 1) (match-end 1)))))
	  (setq paths-path-emacs-version version)
	  version)))))
  
(defun paths-find-emacs-path (roots suffix base &optional envvar default)
  "Find a path in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
SUFFIX is the subdirectory from there.
BASE is the base to look for.
ENVVAR is the name of the environment variable that might also
specify the path.
DEFAULT is a fall-back value."
  (let ((envvar-value (and envvar (getenv envvar))))
    (if (and (fboundp 'parse-colon-path) envvar-value)
	(parse-colon-path envvar-value)
      (let ((directory (paths-find-emacs-directory roots base suffix)))
	(if (and directory (file-directory-p directory))
	    (list directory)
	  (paths-directories-which-exist default))))))

(defun paths-directories-which-exist (directories)
  "Return the directories among DIRECTORIES."
  (let ((reverse-directories '()))
    (while directories
      (if (file-directory-p (car directories))
	  (setq reverse-directories 
		(cons (car directories)
		      reverse-directories)))
      (setq directories (cdr directories)))
    (reverse reverse-directories)))

(defun paths-find-site-path (roots base &optional envvar default)
  "Find a path underneath the site hierarchy."
  (paths-find-emacs-path roots "lib/xemacs/" base envvar default))

(defun paths-find-version-path (roots base &optional envvar default)
  "Find a path underneath the site hierarchy."
  (paths-find-emacs-path roots
			 (concat "lib/xemacs-" (construct-emacs-version) "/")
			 base
			 envvar default))
		   
; Packages are special ...

(defun paths-find-package-path (roots)
  "Construct the package path underneath installation roots ROOTS."
  (let ((envvar-value (getenv "EMACSPACKAGEPATH")))
    (if (and (fboundp 'parse-colon-path) envvar-value)
	(parse-colon-path envvar-value)
      (let ((base-directory (paths-find-site-directory roots "packages")))
	(if base-directory
	    (let ((mule-directory (and (featurep 'mule)
				       (paths-find-site-directory roots
								  "mule-packages"))))
	      (append '("~/.xemacs/")
		      '(nil)
		      (and mule-directory
			   (list mule-directory))
		      (list base-directory)))
	  configure-package-path)))))

(defvar paths-package-special-bases '("etc" "info" "lisp" "lib-src" "bin")
  "Special subdirectories of packages.")

(defun paths-find-packages-in-directories (directories)
  "Find all packages underneath directories in DIRECTORIES."
  (paths-find-recursive-path directories
			     (append paths-version-control-bases
				     paths-package-special-bases)))

(defun paths-split-path (path)
  "Split PATH at NIL, return pair with two components.
The second component is shared with PATH."
  (let ((reverse-early '()))
    (while (and path (null (null (car path))))
      (setq reverse-early (cons (car path) reverse-early))
      (setq path (cdr path)))
    (if (null path)
	(cons nil path)
      (cons (reverse reverse-early) (cdr path)))))

(defun paths-find-packages (package-path)
  "Search for all packages in PACKAGE-PATH.
PACKAGE-PATH may distinguish (by NIL-separation) between early
and late packages.
This returns (CONS EARLY-PACKAGES LATE-PACKAGES)."
  (let* ((stuff (paths-split-path package-path))
	 (early (car stuff))
	 (late (cdr stuff)))
    (cons (paths-find-packages-in-directories early)
	  (paths-find-packages-in-directories late))))

(defun paths-find-package-library-path (packages suffixes)
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

(defun paths-find-package-load-path (packages)
  "Construct the load-path component for packages.
PACKAGES is a list of package directories."
  (paths-find-recursive-load-path
   (paths-find-package-library-path packages '("lisp/"))))

(defun paths-find-package-exec-path (packages)
  (paths-find-package-library-path packages
				   (list (concat "bin/" system-configuration "/")
					 "lib-src/")))

(defun paths-find-package-info-path (packages)
  (paths-find-package-library-path packages '("info/")))

(defun paths-find-package-data-path (packages)
  (paths-find-package-library-path packages '("etc/")))

(defun paths-find-emacs-roots (invocation-directory
			       invocation-name)
  "Find all plausible installation roots for XEmacs."
  (let ((invocation-root
	 (paths-find-emacs-root invocation-directory invocation-name))
	(installation-root
	 (if (and configure-prefix-directory
		  (file-directory-p configure-prefix-directory))
	     configure-prefix-directory)))
    (append (and invocation-root
		 (list invocation-root))
	    (and installation-root
		 (list installation-root)))))

(defun paths-find-load-path (roots early-package-load-path late-package-load-path)
  "Construct the load path."
  (let ((envvar-value (getenv "EMACSLOADPATH")))
    (if (and (fboundp 'parse-colon-path) envvar-value)
	(parse-colon-path envvar-value)
      (let* ((site-lisp-directory
	      (and allow-site-lisp
		   (paths-find-site-directory roots "site-lisp"
					      nil
					      configure-site-directory)))
	     (site-lisp-load-path
	      (and site-lisp-directory
		   (paths-find-recursive-load-path (list site-lisp-directory))))
	     (lisp-directory
	      (paths-find-version-directory roots "lisp"
					    nil
					    configure-lisp-directory))
	     (lisp-load-path
	      (paths-find-recursive-load-path (list lisp-directory))))
	(nconc early-package-load-path
	       site-lisp-load-path
	       late-package-load-path
	       lisp-load-path)))))

(defun paths-find-info-path (roots early-packages late-packages)
  "Construct the info path."
  (append
   (paths-find-package-info-path early-packages)
   (paths-find-package-info-path late-packages)
   (let ((info-directory
	  (paths-find-version-directory roots "info"
					nil
					(append
					 (and configure-info-directory
					      (list configure-info-directory))
					 configure-info-path))))
     (and info-directory
	  (list info-directory)))
   (let ((info-path-envval (getenv "INFOPATH")))
     (if (and (fboundp 'parse-colon-path) info-path-envval)
	 (parse-colon-path info-path-envval)))))

(defun paths-find-doc-directory (roots)
  "Find the documentation directory."
  (paths-find-architecture-directory roots "lib-src"))

(defun paths-find-lock-directory (roots)
  "Find the lock directory."
  (paths-find-site-path roots "lock" "EMACSLOCKDIR" configure-lock-directory))

(defun paths-find-superlock-file (lock-directory)
  "Find the superlock file."
  (cond
   ((null lock-directory)
    nil)
   ((and configure-superlock-file
	 (file-directory-p (file-name-directory configure-superlock-file)))
    configure-superlock-file)
   (t
    (expand-file-name "!!!SuperLock!!!" lock-directory))))

(defun paths-find-exec-directory (roots)
  "Find the binary directory."
  (paths-find-architecture-directory roots "lib-src"))

(defun paths-find-exec-path (roots exec-directory early-packages late-packages)
  "Find the binary path."
  (append
   (let ((path-envval (getenv "PATH")))
     (and (fboundp 'parse-colon-path) path-envval
	  (parse-colon-path path-envval)))
   (paths-find-package-exec-path early-packages)
   (paths-find-package-exec-path late-packages)
   (let ((emacspath-envval (getenv "EMACSPATH")))
     (if (and (fboundp 'parse-colon-path) emacspath-envval)
	 (parse-colon-path path-envval)
       (paths-directories-which-exist configure-exec-path)))
   (and exec-directory
	(list exec-directory))))

(defun paths-find-data-directory (roots)
  "Find the data directory."
  (paths-find-version-directory roots "etc" "EMACSDATA" configure-data-directory))

(defun paths-find-data-directory-list (data-directory early-packages late-packages)
  "Find the data path."
  (append
   (paths-find-package-data-path early-packages)
   (paths-find-package-data-path late-packages)
   (list data-directory)))

(defun paths-setup-paths ()
  "Setup all the various paths.
Call this as often as you like!"
  ;; XEmacs -- Steven Baur says invocation directory is nil if you
  ;; try to use XEmacs as a login shell.
  (or invocation-directory (setq invocation-directory default-directory))
  (if (fboundp 'abbreviate-file-name)
      ;; No abbreviate-file-name in temacs
      (setq invocation-directory
	    ;; don't let /tmp_mnt/... get into the load-path or exec-path.
	    (abbreviate-file-name invocation-directory)))

  (let ((roots (paths-find-emacs-roots invocation-directory invocation-name)))

    (setq package-path (paths-find-package-path roots))

    (let ((stuff (paths-find-packages package-path)))
      (setq early-packages (car stuff))
      (setq late-packages (cdr stuff)))

    (setq early-package-load-path (paths-find-package-load-path early-packages))
    (setq late-package-load-path (paths-find-package-load-path late-packages))

    (setq load-path (paths-find-load-path roots
					  early-package-load-path
					  late-package-load-path))

    (setq info-path (paths-find-info-path roots early-packages late-packages))

    (if (boundp 'lock-directory)
	(progn
	  (setq lock-directory (paths-find-lock-directory roots))
	  (setq superlock-file (paths-find-superlock-file lock-directory))))

    (setq exec-directory (paths-find-exec-directory roots))

    (setq exec-path (paths-find-exec-path roots exec-directory
					  early-packages late-packages))

    (setq doc-directory (paths-find-doc-directory roots))

    (setq data-directory (paths-find-data-directory roots))

    (setq data-directory-list (paths-find-data-directory-list data-directory
							      early-packages
							      late-packages))))

(defun paths-setup-paths-warning ()
  (let ((lock (if (boundp 'lock-directory) lock-directory 't))
	warnings message guess)
    (if (and (stringp lock) (null (file-directory-p lock)))
	(setq lock nil))
    (cond
     ((null (and exec-directory data-directory doc-directory load-path lock))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lock)           (push "lock-directory" warnings))
	(if (null exec-directory) (push "exec-directory" warnings))
	(if (null data-directory) (push "data-directory" warnings))
	(if (null doc-directory)  (push "doc-directory"  warnings))
	(if (null load-path)      (push "load-path"      warnings))
	(cond ((cdr (cdr warnings))
	       (setq message (apply 'format "%s, %s, and %s" warnings)))
	      ((cdr warnings)
	       (setq message (apply 'format "%s and %s" warnings)))
	      (t (setq message (format "variable %s" (car warnings)))))
	(insert "couldn't find an obvious default for " message
		", and there were no defaults specified in paths.h when "
		"XEmacs was built.  Perhaps some directories don't exist, "
		"or the XEmacs executable, " (concat invocation-directory
						     invocation-name)
		" is in a strange place?")

	(if (fboundp 'fill-region)
	    ;; Might not be bound in the cold load environment...
	    (let ((fill-column 76))
	      (fill-region (point-min) (point-max))))
	(goto-char (point-min))
	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))

(defun paths-load-package-lisps (package-load-path base)
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

(defun paths-load-package-auto-autoloads (package-load-path)
  "Load auto-autoload files along a load path."
  (paths-load-package-lisps package-load-path
			    (file-name-sans-extension autoload-file-name)))

(defun paths-load-package-dumped-lisps (package-load-path)
  "Load dumped-lisp.el files along a load path."
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
				  (load (expand-file-name base dir)))
			      package-lisp))))))
	package-load-path))

;;; setup-paths.el ends here
