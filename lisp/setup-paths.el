;;; setup-paths.el --- setup various XEmacs paths

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 2003 Ben Wing.

;; Author: Mike Sperber <mike@xemacs.orgx>
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

;; This file contains functions and variables that describe and construct
;; the various paths into the XEmacs hierarchy from a global viewpoint.

;; This file doesn't actually set any global variable, and doesn't
;; contain any state---it just contains the functionality for
;; searching directories and constructing paths.

;; It requires find-paths.el and packages.el.

;;; Code:

;(setq debug-paths t)


(defvar paths-core-load-path-depth 0
  "Depth of load-path searches in core Lisp paths.")

(defvar paths-site-load-path-depth 1
  "Depth of load-path searches in site Lisp paths.")

(defvar paths-mule-load-path-depth 0
  "Depth of load-path searches in Mule Lisp paths.")

(defvar paths-module-load-path-depth 1
  "Depth of load-path searches in module paths.")

(defvar paths-default-info-directories
  (mapcar (function
	   (lambda (dirlist)
	     (paths-construct-path
	      dirlist (char-to-string directory-sep-char))))
	  '(("usr" "local" "info")
	    ("usr" "info")
	    ("usr" "local" "share" "info")
	    ("usr" "share" "info")))
  "Directories appended to the end of the info path by default.")


;;; Basic utility functions.

(defun paths-emacs-root-p (directory)
  "Check if DIRECTORY is a plausible installation root."
  (or
   ;; installed
   (paths-file-readable-directory-p (paths-construct-path (list directory
								"lib"
								(construct-emacs-version-name))))
   ;; in-place or windows-nt.  windows-nt equivalent of --srcdir is
   ;; BUILD_DIR in config.inc, and has no lisp/ or etc/ since symlinks
   ;; don't exist.  instead, xemacs.mak points configure-lisp-directory and
   ;; configure-data-directory at the right places.
   (and
    (or configure-exec-directory (paths-file-readable-directory-p (paths-construct-path (list directory "lib-src"))) (eq system-type 'windows-nt))
    (or configure-lisp-directory (paths-file-readable-directory-p (paths-construct-path (list directory "lisp"))))
    (or configure-data-directory (paths-file-readable-directory-p (paths-construct-path (list directory "etc")))))))

(defun paths-emacs-data-root-p (directory)
  "Check if DIRECTORY is a plausible data installation root.
A data installation root is one containing data files that may be shared
among multiple different versions of XEmacs, the packages in particular.
This serves as an additional filter to narrow down the list of plausible
installation roots."
  (or
   ;; installed
   (paths-file-readable-directory-p (paths-construct-path (list directory
								"share"
								emacs-program-name)))
   (paths-file-readable-directory-p (paths-construct-path (list directory
								"share"
								(construct-emacs-version-name))))
   ;; in-place or windows-nt
   (and
    (paths-file-readable-directory-p (paths-construct-path (list directory "lisp")))
    (paths-file-readable-directory-p (paths-construct-path (list directory "etc"))))

   ;; searching for a package directory
   (and
    (string-match "win32" system-configuration)
    (paths-file-readable-directory-p (paths-construct-path (list directory
								 "xemacs-packages"))))))

(defun paths-find-invocation-roots (invocation-directory invocation-name root-p)
  "Find the list of run-time roots of XEmacs.
INVOCATION-DIRECTORY is a directory containing the XEmacs executable.
INVOCATION-NAME is the name of the executable itself
ROOT-P is a function that tests whether a root is plausible."
  (let* ((executable-file-name (paths-chase-symlink
				(concat invocation-directory
					invocation-name)))
	 (executable-directory (file-name-directory executable-file-name))
	 (maybe-root-1 (file-name-as-directory
			(paths-construct-path '("..") executable-directory)))
	 (maybe-root-2 (file-name-as-directory
			(paths-construct-path '(".." "..") executable-directory))))

    (paths-filter root-p
		  (list maybe-root-1 maybe-root-2))))

(defun paths-find-emacs-roots (invocation-directory
			       invocation-name
			       root-p)
  "Find all plausible installation roots for XEmacs.
This is a list of plausible directories in which to search for the important
directories used by XEmacs at run-time, for example `exec-directory',
`data-directory' and `lisp-directory'.
ROOT-P is a function that tests whether a root is plausible."
  (let* ((invocation-roots
	  (paths-find-invocation-roots invocation-directory
				       invocation-name
				       root-p))
	 (potential-installation-roots
	  (paths-uniq-append
	   (and configure-exec-prefix-directory
		(list (file-name-as-directory
		       configure-exec-prefix-directory)))
	   (and configure-prefix-directory
		(list (file-name-as-directory
		       configure-prefix-directory)))))
	 (installation-roots
	  (paths-filter root-p potential-installation-roots)))
    (paths-uniq-append invocation-roots
		       installation-roots)))

(defun paths-find-site-lisp-directory (roots)
  "Find the site Lisp directory of the XEmacs hierarchy.
ROOTS is a list of installation roots."
  (paths-find-site-directory roots (list "site-lisp")
			     nil nil
			     configure-site-directory))

(defun paths-find-site-module-directory (roots)
  "Find the site modules directory of the XEmacs hierarchy.
ROOTS is a list of installation roots."
  (paths-find-site-directory roots (list "site-modules")
			     t nil
			     configure-site-module-directory))

(defun paths-find-lisp-directory (roots)
  "Find the main Lisp directory of the XEmacs hierarchy.
ROOTS is a list of installation roots."
  (paths-find-version-directory roots (list "lisp")
				nil nil
				configure-lisp-directory))

(defun paths-find-mule-lisp-directory (roots &optional lisp-directory)
  "Find the Mule Lisp directory of the XEmacs hierarchy.
ROOTS is a list of installation roots."
  ;; #### kludge
  (if lisp-directory
      (let ((guess
	     (file-name-as-directory
	      (paths-construct-path (list lisp-directory "mule")))))
	(if (paths-file-readable-directory-p guess)
	    guess
	  (paths-find-version-directory roots (list "mule-lisp")
					nil nil
					configure-mule-lisp-directory)))))

(defun paths-find-module-directory (roots)
  "Find the main modules directory of the XEmacs hierarchy.
ROOTS is a list of installation roots."
  (paths-find-architecture-directory roots (list "modules")
				     nil configure-module-directory))

(defun paths-construct-load-path
  (roots early-package-load-path late-package-load-path last-package-load-path
	 lisp-directory
	 &optional site-lisp-directory mule-lisp-directory)
  "Construct the complete load path.
ROOTS is the list of installation roots.
EARLY-PACKAGE-LOAD-PATH, LATE-PACKAGE-LOAD-PATH, and LAST-PACKAGE-LOAD-PATH
are the load paths for the package hierarchies.
SITE-LISP-DIRECTORY and MULE-LISP-DIRECTORY are optional directories to be
included in the load path---SITE-LISP-DIRECTORY for the obsolete site-specific
Lisp files, and MULE-LISP-DIRECTORY for the Mule Lisp files, which exist
only in Mule installations."
  (let* ((envvar-value (getenv "EMACSLOADPATH"))
	 (env-load-path
	  (and envvar-value
	       (paths-decode-directory-path envvar-value 'drop-empties)))
	 (site-lisp-load-path
	  (and site-lisp-directory
	       (paths-find-recursive-load-path (list site-lisp-directory)
					       paths-site-load-path-depth)))
	 (mule-lisp-load-path
	  (and mule-lisp-directory
	       (paths-find-recursive-load-path (list mule-lisp-directory)
					       paths-mule-load-path-depth)))
	 (lisp-load-path
	  (and lisp-directory
	       (paths-find-recursive-load-path (list lisp-directory)
					       paths-core-load-path-depth))))
    (append env-load-path
	    early-package-load-path
	    site-lisp-load-path
	    late-package-load-path
	    mule-lisp-load-path
	    lisp-load-path
	    last-package-load-path)))

(defun paths-construct-module-load-path
  (root module-directory &optional site-module-directory)
  "Construct the modules load path."
  (let* ((envvar-value (getenv "EMACSMODULEPATH"))
	 (env-module-path
	  (and envvar-value
	       (paths-decode-directory-path envvar-value 'drop-empties)))
	 (site-module-load-path
	  (and site-module-directory
	       (paths-find-recursive-load-path (list site-module-directory)
					       paths-site-load-path-depth)))
	 (module-load-path
	  (and module-directory
	       (paths-find-recursive-load-path (list module-directory)
					       paths-module-load-path-depth))))
    (append env-module-path
	    site-module-load-path
	    module-load-path)))

(defun paths-construct-info-path (roots
				  early-package-hierarchies
				  late-package-hierarchies
				  last-package-hierarchies)
  "Construct the info path.
ROOTS is the list of installation roots.
EARLY-PACKAGE-HIERARCHIES, LATE-PACKAGE-HIERARCHIES, and
LAST-PACKAGE-HIERARCHIES are lists of package hierarchy roots,
respectively."
  (let ((info-path-envval (getenv "INFOPATH")))
    (paths-uniq-append
     (append
      (let ((info-directory
	     (paths-find-version-directory roots (list "info")
					   nil nil
					   configure-info-directory)))
	(and info-directory
	     (list info-directory)))
      (packages-find-package-info-path early-package-hierarchies)
      (packages-find-package-info-path late-package-hierarchies)
      (packages-find-package-info-path last-package-hierarchies)
      (and info-path-envval
	   (paths-decode-directory-path info-path-envval 'drop-empties)))
     (and (null info-path-envval)
	  (paths-uniq-append
	   (paths-directories-which-exist configure-info-path)
	   (paths-directories-which-exist paths-default-info-directories))))))

(defun paths-find-doc-directory (roots)
  "Find the documentation directory.
ROOTS is the list of installation roots."
  (paths-find-architecture-directory roots (list "lib-src") nil configure-doc-directory))

(defun paths-find-exec-directory (roots)
  "Find the binary directory.
ROOTS is the list of installation roots."
  (paths-find-architecture-directory roots (list "lib-src")
				     nil configure-exec-directory))

(defun paths-construct-exec-path (roots exec-directory
				  early-package-hierarchies
				  late-package-hierarchies
				  last-package-hierarchies)
  "Find the binary path.
ROOTS is the list of installation roots.
EARLY-PACKAGE-HIERARCHIES, LATE-PACKAGE-HIERARCHIES, and
LAST-PACKAGE-HIERARCHIES are lists of package hierarchy roots,
respectively.
EXEC-DIRECTORY is the directory of architecture-dependent files that
come with XEmacs.
EARLY-PACKAGES, LATE-PACKAGES, and LAST-PACKAGES are lists of
package hierarchy roots, respectively."
  (append
   (let ((path-envval (getenv "PATH")))
     (if path-envval
	 (paths-decode-directory-path path-envval 'drop-empties)))
   (packages-find-package-exec-path early-package-hierarchies)
   (packages-find-package-exec-path late-package-hierarchies)
   (let ((emacspath-envval (getenv "EMACSPATH")))
     (and emacspath-envval
	  (split-path emacspath-envval)))
   (and exec-directory
	(list exec-directory))
   (packages-find-package-exec-path last-package-hierarchies)))

(defun paths-find-data-directory (roots)
  "Find the data directory.
ROOTS is the list of installation roots."
  (paths-find-version-directory roots (list "etc") nil "EMACSDATA" configure-data-directory))

(defun paths-construct-data-directory-list (data-directory
					    early-package-hierarchies
					    late-package-hierarchies
					    last-package-hierarchies)
  "Construct the data path.
DATA-DIRECTORY is the data directory of the XEmacs installation.
EARLY-PACKAGE-HIERARCHIES, LATE-PACKAGE-HIERARCHIES, and
LAST-PACKAGE-HIERARCHIES are lists of package hierarchy roots,
respectively."
  (append
   (packages-find-package-data-path early-package-hierarchies)
   (packages-find-package-data-path late-package-hierarchies)
   (list data-directory)
   (packages-find-package-data-path last-package-hierarchies)))

;;; setup-paths.el ends here
