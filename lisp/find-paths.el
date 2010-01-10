;;; find-paths.el --- setup various XEmacs paths

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 2003 Ben Wing.

;; Author: Mike Sperber <mike@xemacs.org>
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

;; This file contains basic library functionality for manipulating paths
;; and path lists and finding paths in the XEmacs hierarchy.


;;; Code:

(defvar paths-version-control-filename-regexp
  "^\\(RCS\\|CVS\\|SCCS\\)$"
  "File bases associated with version control.")

(defvar paths-lisp-filename-regexp
  "^\\(.*\\.elc?\\)$"
  "File bases that name Emacs Lisp files.")

(defvar paths-no-lisp-directory-regexp
  (concat "\\(" paths-version-control-filename-regexp "\\)"
	  "\\|"
	  "\\(" paths-lisp-filename-regexp "\\)")
  "File bases that may not be directories containing Lisp code.")

(defun paths-find-recursive-path (directories &optional max-depth exclude-regexp)
  "Return a list of the directory hierarchy underneath DIRECTORIES.
The returned list is sorted by pre-order and lexicographically.
MAX-DEPTH limits the depth of the search to MAX-DEPTH level,
if it is a number.  If MAX-DEPTH is NIL, the search depth is unlimited.
EXCLUDE-REGEXP is a regexp that matches directory names to exclude
from the search."
  (let ((path '()))
    (while directories
      (let ((directory (file-name-as-directory
			(expand-file-name
			 (car directories)))))
	(if (paths-file-readable-directory-p directory)
	    (let ((raw-entries
		   (if (equal 0 max-depth)
		       '()
		     (directory-files directory nil "^[^.-]")))
		  (reverse-dirs '()))
	      (while raw-entries
		(if (not (and exclude-regexp
			      (string-match exclude-regexp (car raw-entries))))
		    (setq reverse-dirs
			  (cons (expand-file-name (car raw-entries) directory)
				reverse-dirs)))
		(setq raw-entries (cdr raw-entries)))

	      (let ((sub-path
		     (paths-find-recursive-path (reverse reverse-dirs)
						(if (numberp max-depth)
						    (- max-depth 1)
						  max-depth)
						exclude-regexp)))
		(setq path (nconc path
				  (list directory)
				  sub-path))))))
      (setq directories (cdr directories)))
    path))

(defun paths-file-readable-directory-p (filename)
  "Check if filename is a readable directory."
  (and (file-directory-p filename)
       (file-readable-p filename)))

(defun paths-find-recursive-load-path (directories &optional max-depth)
  "Construct a recursive load path underneath DIRECTORIES."
  (paths-find-recursive-path directories
			     max-depth paths-no-lisp-directory-regexp))

(defun paths-chase-symlink (file-name)
  "Chase a symlink until the bitter end."
      (let ((maybe-symlink (file-symlink-p file-name)))
	(if maybe-symlink
	    (let* ((directory (file-name-directory file-name))
		   (destination (expand-file-name maybe-symlink directory)))
	      (paths-chase-symlink destination))
	  file-name)))

(defun paths-construct-path (components &optional expand-directory)
  "Convert list of path components COMPONENTS into a path.
If EXPAND-DIRECTORY is non-NIL, use it as a directory to feed
to EXPAND-FILE-NAME."
  (let* ((reverse-components (reverse components))
	 (last-component (car reverse-components))
	 (first-components (reverse (cdr reverse-components)))
	 (path
	  (apply #'concat
		 (append (mapcar #'file-name-as-directory first-components)
			 (list last-component)))))
    (if expand-directory
	(expand-file-name path expand-directory)
      path)))

(defun paths-construct-emacs-directory (root suffix base)
  "Construct a directory name within the XEmacs hierarchy.
ROOT must be an installation root.
SUFFIX is the subdirectory from there.
BASE is the base to look for."
  (file-name-as-directory
   (expand-file-name
    (concat
     (file-name-as-directory root)
     suffix
     base))))


(defun paths-for-each-emacs-directory (func
				       roots suffix bases
				       &optional envvar default keep-suffix)
  "Iterate over directories in the XEmacs hierarchy.
FUNC is a function that called for each directory, with the directory
as the only argument.
ROOTS must be a list of installation roots.
SUFFIX is the subdirectory from there.
BASEA is a list of possible bases to look for.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If KEEP-SUFFIX is non-nil, the suffix must be respected in searching
the directory."
  (let ((preferred-value (or (and envvar (getenv envvar))
			     default)))
    (if (and preferred-value
	     (paths-file-readable-directory-p preferred-value))
	(file-name-as-directory preferred-value)
      (while roots
	(let ((root (car roots))
	      (bases bases))
	  (while bases
	    (let* ((base (car bases))
		   ;; installed
		   (path (paths-construct-emacs-directory root suffix base)))
	      (if (paths-file-readable-directory-p path)
		  (funcall func path)
		;; in-place
		(if (null keep-suffix)
		    (let ((path (paths-construct-emacs-directory root "" base)))
		      (if (paths-file-readable-directory-p path)
			  (funcall func path))))))
	    (setq bases (cdr bases))))
	(setq roots (cdr roots))))))

(defun paths-find-emacs-directories (roots
				     suffix bases
				     &optional envvar default keep-suffix)
  "Find a list of directories in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
SUFFIX is the subdirectory from there.
BASES is a list of bases to look for.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If KEEP-SUFFIX is non-nil, the suffix must be respected in searching
the directory."
  (let ((l '()))
    (paths-for-each-emacs-directory #'(lambda (dir)
					(setq l (cons dir l)))
				    roots
				    suffix bases
				    envvar default keep-suffix)
    (reverse l)))

(defun paths-find-emacs-directory (roots suffix bases
				   &optional envvar default keep-suffix)
  "Find a directory in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
SUFFIX is the subdirectory from there.
BASES is a list of possible bases to look for.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If KEEP-SUFFIX is non-nil, the suffix must be respected in searching
the directory."
  (catch 'gotcha
    (paths-for-each-emacs-directory #'(lambda (dir)
					(throw 'gotcha dir))
				    roots
				    suffix bases
				    envvar default keep-suffix)))

(defun paths-for-each-site-directory (func
				      roots bases
				      arch-dependent-p
				      &optional envvar default)
  "Iterate over the site-specific directories in the XEmacs hierarchy.
FUNC is a function that called for each directory, with the directory
as the only argument.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value."
  (paths-for-each-emacs-directory func
				  roots
				  (file-name-as-directory
				   (paths-construct-path (list
							  (if arch-dependent-p "lib" "share")
							  emacs-program-name)))
				  bases
				  envvar default))

(defun paths-find-site-directory (roots bases arch-dependent-p &optional envvar default)
  "Find a site-specific directory in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value."
  (catch 'gotcha
    (paths-for-each-site-directory #'(lambda (dir)
				       (throw 'gotcha dir))
				   roots bases arch-dependent-p
				   envvar default)))

(defun paths-find-site-directories (roots bases arch-dependent-p &optional envvar default)
  "Find a list of site-specific directories in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
BASES is a list of bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value."
  (let ((l '()))
    (paths-for-each-site-directory #'(lambda (dir)
					(setq l (cons dir l)))
				   roots bases arch-dependent-p
				   envvar default)
    (reverse l)))

(defun paths-for-each-version-directory (func roots bases arch-dependent-p
					 &optional envvar default enforce-version)
  "Iterate over version-specific directories in the XEmacs hierarchy.
FUNC is a function that called for each directory, with the directory
as the only argument.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If ENFORCE-VERSION is non-nil, the directory must contain the XEmacs version."
  (paths-for-each-emacs-directory func
				  roots
				  (file-name-as-directory
				   (paths-construct-path
				    (list (if arch-dependent-p "lib" "share")
					  (construct-emacs-version-name))))
				  bases
				  envvar default))

(defun paths-find-version-directory (roots bases arch-dependent-p
				     &optional envvar default enforce-version)
  "Find a version-specific directory in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If ENFORCE-VERSION is non-nil, the directory must contain the XEmacs version."
  (catch 'gotcha
    (paths-for-each-version-directory #'(lambda (dir)
					  (throw 'gotcha dir))
				      roots bases arch-dependent-p
				      envvar default)))

(defun paths-find-version-directories (roots bases arch-dependent-p
				       &optional envvar default enforce-version)
  "Find a list of version-specific directories in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ARCH-DEPENDENT-P says whether the file is architecture-specific.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value.
If ENFORCE-VERSION is non-nil, the directory must contain the XEmacs version."
  (let ((l '()))
    (paths-for-each-version-directory #'(lambda (dir)
					  (setq l (cons dir l)))
				      roots bases arch-dependent-p
				      envvar default)
    (reverse l)))

(defun paths-find-architecture-directory (roots bases &optional envvar default)
  "Find an architecture-specific directory in the XEmacs hierarchy.
ROOTS must be a list of installation roots.
BASES is a list of possible bases to look for.
ENVVAR is the name of the environment variable that might also
specify the directory.
DEFAULT is the preferred value."
  (paths-find-version-directory roots
				;; from more to less specific
				(append
				 (mapcar
				  #'(lambda (base)
				      (paths-construct-path
				       (list system-configuration base)))
				  bases)
				 bases
				 (list system-configuration))
				t
				envvar default))

(defun construct-emacs-version-name ()
  "Construct a string from the raw XEmacs version number."
  (concat emacs-program-name "-" emacs-program-version))

(defun paths-directories-which-exist (directories)
  "Return the directories among DIRECTORIES.
DIRECTORIES is a list of strings."
  (let ((reverse-directories '()))
    (while directories
      (if (paths-file-readable-directory-p (car directories))
	  (setq reverse-directories
		(cons (car directories)
		      reverse-directories)))
      (setq directories (cdr directories)))
    (reverse reverse-directories)))

(defun paths-uniq-append (list-1 list-2)
  "Append LIST-1 and LIST-2, omitting EQUAL duplicates."
  (let ((reverse-survivors '()))
    (while list-2
      (if (null (member (car list-2) list-1))
	  (setq reverse-survivors (cons (car list-2) reverse-survivors)))
      (setq list-2 (cdr list-2)))
    (append list-1
	    (reverse reverse-survivors))))

(defun paths-filter (predicate list)
  "Delete all matches of PREDICATE from LIST."
  (let ((reverse-result '()))
    (while list
      (if (funcall predicate (car list))
	  (setq reverse-result (cons (car list) reverse-result)))
      (setq list (cdr list)))
    (nreverse reverse-result)))

(defun paths-decode-directory-path (string &optional drop-empties)
  "Split STRING at path separators into a directory list.
Non-\"\" components are converted into directory form.
If DROP-EMPTIES is non-NIL, \"\" components are dropped from the output.
Otherwise, they are left alone."
  (let* ((components (split-path string))
	 (directories
	  (mapcar #'(lambda (component)
		      (if (string-equal "" component)
			  component
			(file-name-as-directory component)))
		  components)))
    (if drop-empties
	(paths-filter #'(lambda (component)
			  (null (string-equal "" component)))
		      directories)
      directories)))

;;; find-paths.el ends here
