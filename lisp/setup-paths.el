;;; setup-paths.el --- setup various XEmacs paths

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 2003 Ben Wing.

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

;; This file contains functions and variables that describe and construct
;; the various paths into the XEmacs hierarchy from a global viewpoint.
;; This file doesn't actually do anything.

;; It requires find-paths.el and packages.el.

;;; Code:

;(setq debug-paths t)


;;; Path-related variables.
;;; NOTE: Many of them (`lisp-directory', `data-directory', etc.) are
;;; built-in.

(defvar emacs-roots nil
  "List of plausible roots of the XEmacs hierarchy.
This is a list of plausible directories in which to search for the important
directories used by XEmacs at run-time, for example `exec-directory',
`data-directory' and `lisp-directory'.

Normally set at startup by calling `paths-find-emacs-roots'.")

(defvar emacs-data-roots nil
  "List of plausible data roots of the XEmacs hierarchy.")

(defvar user-init-directory-base ".xemacs"
  "Base of directory where user-installed init files may go.")

(defvar user-init-directory
  (file-name-as-directory
   (paths-construct-path (list "~" user-init-directory-base)))
  "Directory where user-installed init files may go.")

(defvar user-init-file-base "init.el"
  "Default name of the user init file if uncompiled.
This should be used for migration purposes only.")

(defvar user-init-file-base-list '("init.el")
  "List of allowed init files in the user's init directory.
The first one found takes precedence.  .elc files do not need to be listed.")

(defvar user-home-init-file-base-list
  (append '(".emacs.el" ".emacs")
	  (and (eq system-type 'windows-nt)
	       '("_emacs.el" "_emacs")))
  "List of allowed init files in the user's home directory.
The first one found takes precedence.  .elc files do not need to be listed.")

(defvar load-home-init-file nil
  "Non-nil if XEmacs should load the init file from the home directory.
Otherwise, XEmacs will offer migration to the init directory.")

(defvar load-user-init-file-p t
  "Non-nil if XEmacs should load the user's init file.")

(defvar paths-core-load-path-depth 0
  "Depth of load-path searches in core Lisp paths.")

(defvar paths-site-load-path-depth 1
  "Depth of load-path searches in site Lisp paths.")

(defvar paths-mule-load-path-depth 0
  "Depth of load-path searches in Mule Lisp paths.")

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
among multiple different versions of XEmacs, the packages in particular."
  (or
   ;; installed
   (paths-file-readable-directory-p (paths-construct-path (list directory
								"lib"
								emacs-program-name)))
   (paths-file-readable-directory-p (paths-construct-path (list directory
								"lib"
								(construct-emacs-version-name))))
   ;; in-place or windows-nt
   (and
    (paths-file-readable-directory-p (paths-construct-path (list directory "lisp")))
    (paths-file-readable-directory-p (paths-construct-path (list directory "etc"))))))

(defun paths-find-emacs-root (invocation-directory invocation-name)
  "Find the run-time root of XEmacs."
  (let* ((executable-file-name (paths-chase-symlink
				(concat invocation-directory
					invocation-name)))
	 (executable-directory (file-name-directory executable-file-name))
	 (maybe-root-1 (file-name-as-directory
			(paths-construct-path '("..") executable-directory)))
	 (maybe-root-2 (file-name-as-directory
			(paths-construct-path '(".." "..") executable-directory))))
    (or (and (paths-emacs-root-p maybe-root-1)
	     maybe-root-1)
	(and (paths-emacs-root-p maybe-root-2)
	     maybe-root-2))))

(defun paths-find-emacs-roots (root-p)
  "Find all plausible installation roots for XEmacs.
This is a list of plausible directories in which to search for the important
directories used by XEmacs at run-time, for example `exec-directory',
`data-directory' and `lisp-directory'.
ROOT-P is a function that tests whether a root is plausible."
  (let* ((potential-invocation-root
	  (paths-find-emacs-root invocation-directory invocation-name))
	 (invocation-roots
	  (and potential-invocation-root
	       (list potential-invocation-root)))
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
  "Find the site Lisp directory of the XEmacs hierarchy."
  (paths-find-site-directory roots "site-lisp"
			     nil
			     configure-site-directory))

(defun paths-find-site-module-directory (roots)
  "Find the site modules directory of the XEmacs hierarchy."
  (paths-find-site-directory roots "site-modules"
			     nil
			     configure-site-module-directory))

(defun paths-find-lisp-directory (roots)
  "Find the main Lisp directory of the XEmacs hierarchy."
  (paths-find-version-directory roots "lisp"
				nil
				configure-lisp-directory))

(defun paths-find-mule-lisp-directory (roots &optional lisp-directory)
  "Find the Mule Lisp directory of the XEmacs hierarchy."
  ;; #### kludge
  (if lisp-directory
      (let ((guess
	     (file-name-as-directory
	      (paths-construct-path (list lisp-directory "mule")))))
	(if (paths-file-readable-directory-p guess)
	    guess
	  (paths-find-version-directory roots "mule-lisp"
					nil
					configure-mule-lisp-directory)))))

(defun paths-find-module-directory (roots)
  "Find the main modules directory of the XEmacs hierarchy."
  (paths-find-architecture-directory roots "modules"
				     nil configure-module-directory))

(defun paths-construct-load-path
  (roots early-package-load-path late-package-load-path last-package-load-path
	 lisp-directory
	 &optional site-lisp-directory mule-lisp-directory)
  "Construct the load path."
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
					       paths-core-load-path-depth))))
     (append env-module-path
	    site-module-load-path
	    module-load-path)))

(defun paths-construct-info-path (roots early-packages late-packages last-packages)
  "Construct the info path."
  (let ((info-path-envval (getenv "INFOPATH")))
    (paths-uniq-append
     (append
      (let ((info-directory
	     (paths-find-version-directory roots "info"
					   nil
					   configure-info-directory)))
	(and info-directory
	     (list info-directory)))
      (packages-find-package-info-path early-packages)
      (packages-find-package-info-path late-packages)
      (packages-find-package-info-path last-packages)
      (and info-path-envval
	   (paths-decode-directory-path info-path-envval 'drop-empties)))
     (and (null info-path-envval)
	  (paths-uniq-append
	   (paths-directories-which-exist configure-info-path)
	   (paths-directories-which-exist paths-default-info-directories))))))

(defun paths-find-doc-directory (roots)
  "Find the documentation directory."
  (paths-find-architecture-directory roots "lib-src" nil configure-doc-directory))

(defun paths-find-exec-directory (roots)
  "Find the binary directory."
  (paths-find-architecture-directory roots "lib-src"
				     nil configure-exec-directory))

(defun paths-construct-exec-path (roots exec-directory
				  early-packages late-packages last-packages)
  "Find the binary path."
  (append
   (let ((path-envval (getenv "PATH")))
     (if path-envval
	 (paths-decode-directory-path path-envval 'drop-empties)))
   (packages-find-package-exec-path early-packages)
   (packages-find-package-exec-path late-packages)
   (let ((emacspath-envval (getenv "EMACSPATH")))
     (and emacspath-envval
	  (split-path emacspath-envval)))
   (and exec-directory
	(list exec-directory))
   (packages-find-package-exec-path last-packages)))

(defun paths-find-data-directory (roots)
  "Find the data directory."
  (paths-find-version-directory roots "etc" "EMACSDATA" configure-data-directory))

(defun paths-construct-data-directory-list (data-directory
					    early-packages late-packages last-packages)
  "Find the data path."
  (append
   (packages-find-package-data-path early-packages)
   (packages-find-package-data-path late-packages)
   (list data-directory)
   (packages-find-package-data-path last-packages)))


;;; High-level functions to set up the paths.

(defun startup-find-load-path (&optional inhibit-packages
			       set-global-package-paths)
  "Determine the value for `load-path'.
INHIBIT-PACKAGES says which types of packages, if any, to omit from the
returned value.  It can be `t' (omit all), one of the symbols `early',
`late', or `last', or a list of one or more of the symbols.

If SET-GLOBAL-PACKAGE-PATHS is non-nil, initialize the global package path
variables referring to the particular types of packages (`early-packages',
`early-package-load-path', `late-packages', `late-package-load-path',
`last-packages', `last-package-load-path')."
  (let (earlyp latep lastp earlyp-lp latep-lp lastp-lp)
    (apply #'(lambda (early late last)
	       (setq earlyp (and (not (memq 'early inhibit-packages)) early))
	       (setq latep (and (not (memq 'late inhibit-packages)) late))
	       (setq lastp (and (not (memq 'last inhibit-packages)) last)))
	   (packages-find-packages
	    emacs-data-roots
	    (packages-compute-package-locations user-init-directory)))

  (setq earlyp-lp (packages-find-package-load-path earlyp))
  (setq latep-lp (packages-find-package-load-path latep))
  (setq lastp-lp (packages-find-package-load-path lastp))

  (when set-global-package-paths
    (setq early-packages earlyp
	  late-packages latep
	  last-packages lastp
	  early-package-load-path earlyp-lp
	  late-package-load-path latep-lp
	  last-package-load-path lastp-lp))

  (paths-construct-load-path emacs-roots earlyp-lp latep-lp lastp-lp
			     lisp-directory site-directory
			     mule-lisp-directory)))

(defun startup-setup-paths (&optional inhibit-packages called-early)
  "Setup all the various paths.
INHIBIT-PACKAGES says which types of packages, if any, to omit from the
returned value.  It can be `t' (omit all), one of the symbols `early',
`late', or `last', or a list of one or more of the symbols.

This function is idempotent, so call this as often as you like!"

  (setq debug-paths (or debug-paths
			(and (getenv "EMACSDEBUGPATHS")
			     t)))

  (setq emacs-roots (paths-find-emacs-roots #'paths-emacs-data-root-p))

  (setq emacs-data-roots (paths-find-emacs-roots #'paths-emacs-data-root-p))

  (if (null emacs-roots)
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))

	(insert "Couldn't find an obvious default for the root of the\n"
		"XEmacs hierarchy.")

	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)))

  (if (eq inhibit-packages t)
      (setq inhibit-packages '(early late last)))
  (if (not (listp inhibit-packages))
      (setq inhibit-packages (list inhibit-packages)))

  (when debug-paths
    (princ (format
"startup-setup-paths arguments:
  inhibit-packages: %S
  inhibit-site-lisp: %S
  called-early: %S
" inhibit-packages inhibit-site-lisp called-early)
	   'external-debugging-output)
    (princ (format
"emacs-roots:
%S
emacs-data-roots:
%S
user-init-directory: %S
configure-package-path: %S
" emacs-roots emacs-data-roots user-init-directory configure-package-path)
	   'external-debugging-output)
    )

  (setq lisp-directory (paths-find-lisp-directory emacs-roots))

  (if debug-paths
      (princ (format "lisp-directory:\n%S\n" lisp-directory)
	     'external-debugging-output))

  (if (featurep 'mule)
      (progn
	(setq mule-lisp-directory
	      (paths-find-mule-lisp-directory emacs-roots
					      lisp-directory))
	(if debug-paths
	    (princ (format "mule-lisp-directory:\n%S\n"
			   mule-lisp-directory)
		   'external-debugging-output)))
    (setq mule-lisp-directory '()))

  (setq site-directory (and (null inhibit-site-lisp)
			    (paths-find-site-lisp-directory emacs-roots)))

  (if (and debug-paths (null inhibit-site-lisp))
      (princ (format "site-directory:\n%S\n" site-directory)
	     'external-debugging-output))

  (setq load-path (startup-find-load-path inhibit-packages t))

  (when debug-paths
    (princ (format "early-packages and early-package-load-path:\n%S\n%S\n"
		   early-packages early-package-load-path)
	   'external-debugging-output)
    (princ (format "late-packages and late-package-load-path:\n%S\n%S\n"
		   late-packages late-package-load-path)
	   'external-debugging-output)
    (princ (format "last-packages and last-package-load-path:\n%S\n%S\n"
		   last-packages last-package-load-path)
	   'external-debugging-output))

  (if debug-paths
      (princ (format "load-path:\n%S\n" load-path)
            'external-debugging-output))
  (setq module-directory (paths-find-module-directory emacs-roots))
  (if debug-paths
      (princ (format "module-directory:\n%S\n" module-directory)
	     'external-debugging-output))
  (setq site-module-directory (and (null inhibit-site-modules)
				   (paths-find-site-module-directory
				    emacs-roots)))
  (if (and debug-paths (null inhibit-site-modules))
      (princ (format "site-module-directory:\n%S\n"
		     site-module-directory)
	     'external-debugging-output))

  (setq module-load-path (paths-construct-module-load-path
			  emacs-roots
			  module-directory
			  site-module-directory))

  (unless called-early
    (setq Info-directory-list
	  (paths-construct-info-path
	   emacs-roots early-packages late-packages last-packages))

    (if debug-paths
	(princ (format "Info-directory-list:\n%S\n" Info-directory-list)
	       'external-debugging-output))

    (setq exec-directory (paths-find-exec-directory emacs-roots))

    (if debug-paths
	(princ (format "exec-directory:\n%s\n" exec-directory)
	       'external-debugging-output))

    (setq exec-path
	  (paths-construct-exec-path emacs-roots exec-directory
				     early-packages late-packages
				     last-packages))

    (if debug-paths
	(princ (format "exec-path:\n%S\n" exec-path)
	       'external-debugging-output))

    (setq doc-directory (paths-find-doc-directory emacs-roots))

    (if debug-paths
	(princ (format "doc-directory:\n%S\n" doc-directory)
	       'external-debugging-output))
    
    (setq data-directory (paths-find-data-directory emacs-roots))
    
    (if debug-paths
	(princ (format "data-directory:\n%S\n" data-directory)
	       'external-debugging-output))

    (setq data-directory-list (paths-construct-data-directory-list
			       data-directory early-packages
			       late-packages last-packages))
    (if debug-paths
	(princ (format "data-directory-list:\n%S\n" data-directory-list)
	       'external-debugging-output))))

(defun startup-find-load-path-for-packages (packages)
  "Return a suitable load-path for PACKAGES.
PACKAGES is a list of package names (strings).  This looks for package
directories in the load path whose last component is one of the members of
PACKAGES."
  (mapcan
   #'(lambda (package)
       (and (member (file-name-nondirectory (directory-file-name package))
		    packages)
	    (list package)))
   (startup-find-load-path)))

; (defun startup-set-basic-packages-load-path ()
;   "#### This is a hack.  When recompiling .el files, we use -no-packages
; to avoid problems with packages shadowing standard Lisp files
; (e.g. unicode.el), but we really still need the stuff in xemacs-base and
; xemacs-devel, which SHOULD NOT be in the packages."
;   (setq load-path (startup-find-load-path-for-packages
; 		   '("xemacs-base" "xemacs-devel"))))


;;; Now actually set the paths up, for bootstrapping purposes.  This is run
;;; at early dump time and in certain cases where we use a minimal temacs
;;; to do useful things, like rebuild DOC.

(startup-setup-paths (if inhibit-all-packages t '(early last)) t)

;;; setup-paths.el ends here
