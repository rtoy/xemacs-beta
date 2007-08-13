;;; find-paths.el --- setup various XEmacs paths

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

;; This file contains the library functionality to find paths into the
;; XEmacs hierarchy.

;;; Code:

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
   (file-directory-p
    (concat directory "lib/xemacs-" (construct-emacs-version)))
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
      "XX.XX"
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
    (if envvar-value
	(decode-path-internal envvar-value)
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

;;; find-paths.el ends here
