;; dump-paths.el --- set up XEmacs paths for dumping

;; Copyright (C) 1985, 1986, 1992, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This sets up the various paths for continuing loading files for dumping.
;; This is the only file of the basic path/package files (find-paths.el,
;; package.el, setup-paths.el, dump-paths.el) that actually does stuff.

(defun startup-setup-paths (roots user-init-directory
				  &optional
				  inhibit-packages inhibit-site-lisp
				  debug-paths called-early)
  "Setup all the various paths.
ROOTS is a list of plausible roots of the XEmacs directory hierarchy.
If INHIBIT-PACKAGES is non-NIL, don't do packages.
If INHIBIT-SITE-LISP is non-NIL, don't do site-lisp.
If DEBUG-PATHS is non-NIL, print paths as they are detected.
It's idempotent, so call this as often as you like!"

  (if (eq inhibit-packages t)
      (setq inhibit-packages '(early late last)))
  (if (not (listp inhibit-packages))
      (setq inhibit-packages (list inhibit-packages)))

  (apply #'(lambda (early late last)
	     (setq early-packages (and (not (memq 'early inhibit-packages))
				       early))
	     (setq late-packages (and (not (memq 'late inhibit-packages))
				       late))
	     (setq last-packages (and (not (memq 'last inhibit-packages))
				       last))
	     )
	 (packages-find-packages
	  roots
	  (packages-compute-package-locations user-init-directory)))

  (setq early-package-load-path (packages-find-package-load-path
				 early-packages))
  (setq late-package-load-path (packages-find-package-load-path late-packages))
  (setq last-package-load-path (packages-find-package-load-path last-packages))

  (if debug-paths
      (progn
	(princ (format "arguments:\nroots: %S\nuser-init-directory: %S\n"
		       roots user-init-directory)
	       'external-debugging-output)
	(princ (format "inhibit-packages: %S\ninhibit-site-lisp: %S\n"
		       inhibit-packages inhibit-site-lisp)
	       'external-debugging-output)
	(princ (format "debug-paths: %S\ncalled-early: %S\n\n"
		       debug-paths called-early)
	       'external-debugging-output)
	(princ (format "configure-package-path:\n%S\n" configure-package-path)
	       'external-debugging-output)
	(princ (format "early-packages and early-package-load-path:\n%S\n%S\n"
		       early-packages early-package-load-path)
	       'external-debugging-output)
	(princ (format "late-packages and late-package-load-path:\n%S\n%S\n"
		       late-packages late-package-load-path)
	       'external-debugging-output)
	(princ (format "last-packages and last-package-load-path:\n%S\n%S\n"
		       last-packages last-package-load-path)
	       'external-debugging-output)))

  (setq lisp-directory (paths-find-lisp-directory roots))

  (if debug-paths
      (princ (format "lisp-directory:\n%S\n" lisp-directory)
	     'external-debugging-output))

  (if (featurep 'mule)
      (progn
	(setq mule-lisp-directory
	      (paths-find-mule-lisp-directory roots
					      lisp-directory))
	(if debug-paths
	    (princ (format "mule-lisp-directory:\n%S\n"
			   mule-lisp-directory)
		   'external-debugging-output)))
    (setq mule-lisp-directory '()))

  (setq site-directory (and (null inhibit-site-lisp)
			    (paths-find-site-lisp-directory roots)))

  (if (and debug-paths (null inhibit-site-lisp))
      (princ (format "site-directory:\n%S\n" site-directory)
	     'external-debugging-output))

  (setq load-path (paths-construct-load-path roots
					     early-package-load-path
					     late-package-load-path
					     last-package-load-path
					     lisp-directory
					     site-directory
					     mule-lisp-directory))

  (if called-early
      (progn
	(setq module-directory (paths-find-module-directory roots))
	(if debug-paths
	    (princ (format "module-directory:\n%S\n" module-directory)
		   'external-debugging-output))
	(setq site-module-directory (and (null inhibit-site-modules)
					 (paths-find-site-module-directory
					  roots)))
	(if (and debug-paths (null inhibit-site-modules))
	    (princ (format "site-module-directory:\n%S\n"
			   site-module-directory)
		   'external-debugging-output))

	(setq module-load-path (paths-construct-module-load-path
				roots
				module-directory
				site-module-directory)))
    (setq Info-directory-list
	  (paths-construct-info-path
	   roots early-packages late-packages last-packages))

    (if debug-paths
	(princ (format "Info-directory-list:\n%S\n" Info-directory-list)
	       'external-debugging-output))

    (setq exec-directory (paths-find-exec-directory roots))

    (if debug-paths
	(princ (format "exec-directory:\n%s\n" exec-directory)
	       'external-debugging-output))

    (setq exec-path
	  (paths-construct-exec-path roots exec-directory
				     early-packages late-packages
				     last-packages))

    (if debug-paths
	(princ (format "exec-path:\n%S\n" exec-path)
	       'external-debugging-output))

    (setq doc-directory (paths-find-doc-directory roots))

    (if debug-paths
	(princ (format "doc-directory:\n%S\n" doc-directory)
	       'external-debugging-output))
    
    (setq data-directory (paths-find-data-directory roots))
    
    (if debug-paths
	(princ (format "data-directory:\n%S\n" data-directory)
	       'external-debugging-output))

    (setq data-directory-list (paths-construct-data-directory-list
			       data-directory early-packages
			       late-packages last-packages))
    (if debug-paths
	(princ (format "data-directory-list:\n%S\n" data-directory-list)
	       'external-debugging-output))))

;;; Now actually do something.

(let ((debug-paths (or debug-paths
		      (and (getenv "EMACSDEBUGPATHS")
			   t)))
      (roots (paths-find-emacs-roots invocation-directory
				     invocation-name)))

  (if debug-paths
      (princ (format "XEmacs thinks the roots of its hierarchy are:\n%S\n"
		     roots)
	     'external-debugging-output))
  (startup-setup-paths roots
		       (paths-construct-path '("~" ".xemacs"))
		       (if inhibit-all-packages t
			 '(early last))
		       inhibit-site-lisp
		       debug-paths
		       t))

;;; dump-paths.el ends here
