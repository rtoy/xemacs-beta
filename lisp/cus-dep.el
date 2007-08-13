;;; cus-dep.el --- Find customization dependencies.
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>, then
;;         Richard Stallman <rms@gnu.ai.mit.edu>, then
;;         Hrvoje Niksic <hniksic@xemacs.org>       (rewritten for XEmacs)
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched with FSF.


;;; Commentary:

;; This file generates the custom-load files, loaded by cus-load.el.
;; The only entry point is `Custom-make-dependencies'.

;; It works by scanning all the `.el' files in a directory, and
;; evaluates any `defcustom', `defgroup', or `defface' expression that
;; it finds.  The symbol changed by this expression is stored to a
;; hash table as the hash key, file name being the value.

;; After all the files have been examined, custom-loads.el is
;; generated by mapping all the atoms, and seeing if any of them
;; contains a `custom-group' property.  This property is a list whose
;; each element's car is the "child" group symbol.  If that property
;; is in the hash-table, the file name will be looked up from the
;; hash-table, and added to cusload-file.  Because the hash-table is
;; cleared whenever we process a new directory, we cannot get confused
;; by custom-loads from another directory, or from a previous
;; installation.  This is also why it is perfectly safe to have old
;; custom-loads around, and have them loaded by `cus-load.el' (as
;; invoked by `cus-edit.el').

;; A trivial, but useful optimization is that if cusload-file exists,
;; and no .el files in the directory are newer than cusload-file, it
;; will not be generated.  This means that the directories where
;; nothing has changed will be skipped.

;; The `custom-add-loads' function, used by files generated by
;; `Custom-make-dependencies', updates the symbol's `custom-loads'
;; property (a list of strings) with a new list of strings,
;; eliminating the duplicates.  Additionally, it adds the symbol to
;; `custom-group-hash-table'.  It is defined in `cus-load.el'.

;; Example:

;; (custom-add-loads 'foo 'custom-loads '("bar" "baz"))
;; (get 'foo 'custom-loads)
;;   => ("bar" "baz")
;;
;; (custom-add-loads 'foo 'custom-loads '("hmph" "baz" "quz"))
;; (get 'foo 'custom-loads)
;;   => ("bar" "baz" "hmph" "qux")

;; Obviously, this allows correct incremental loading of custom-load
;; files.  This is not necessary under FSF (they simply use `put'),
;; since they have only one file with custom dependencies.  With the
;; advent of packages, we cannot afford the same luxury.


;;; Code:

(require 'cl)
(require 'widget)
(require 'cus-face)

;; Don't change this, unless you plan to change the code in
;; cus-start.el, too.
(defconst cusload-base-file "custom-load.el")

;; Be very careful when changing this function.  It looks easy to
;; understand, but is in fact very easy to break.  Be sure to read and
;; understand the commentary above!

;;;###autoload
(defun Custom-make-dependencies (&optional subdirs)
  "Extract custom dependencies from .el files in SUBDIRS.
SUBDIRS is a list of directories.  If it is nil, the command-line
arguments are used.  If it is a string, only that directory is
processed.  This function is especially useful in batch mode.

Batch usage: xemacs -batch -l cus-dep.el -f Custom-make-dependencies DIRS"
  (interactive "DDirectory: ")
  (and (stringp subdirs)
       (setq subdirs (list subdirs)))
  (or subdirs
      ;; Usurp the command-line-args
      (setq subdirs command-line-args-left
	    command-line-args-left nil))
  (setq subdirs (mapcar #'expand-file-name subdirs))
  (with-temp-buffer
    (let ((enable-local-eval nil)
	  (hash (make-hash-table :test 'eq)))
      (dolist (dir subdirs)
	(princ (format "Processing %s\n" dir))
	(let ((cusload-file (expand-file-name cusload-base-file dir))
	      (files (directory-files dir t "\\`[^=].*\\.el\\'")))
	  ;; A trivial optimization: if no file in the directory is
	  ;; newer than custom-load.el, no need to do anything!
	  (if (and (file-exists-p cusload-file)
		   (dolist (file files t)
		     (when (file-newer-than-file-p file cusload-file)
		       (return nil))))
	      (princ "(No changes need to be written)\n")
	    ;; Process directory
	    (dolist (file files)
	      (when (file-exists-p file)
		(erase-buffer)
		(insert-file-contents file)
		(goto-char (point-min))
		(let ((name (file-name-sans-extension
			     (file-name-nondirectory file))))
		  ;; Search for defcustom/defface/defgroup
		  ;; expressions, and evaluate them.
		  (while (re-search-forward
			  "^(defcustom\\|^(defface\\|^(defgroup"
			  nil t)
		    (beginning-of-line)
		    (let ((expr (read (current-buffer))))
		      ;; We need to ignore errors here, so that
		      ;; defcustoms with :set don't bug out.  Of
		      ;; course, their values will not be assigned in
		      ;; case of errors, but their `custom-group'
		      ;; properties will by that time be in place, and
		      ;; that's all we care about.
		      (ignore-errors
			(eval expr))
		      ;; Hash the file of the affected symbol.
		      (setf (gethash (nth 1 expr) hash) name))))))
	    (cond
	     ((zerop (hash-table-count hash))
	      (princ "(No customization dependencies")
	      (when (file-exists-p cusload-file)
		(princ (format ", deleting %s" cusload-file))
		(delete-file cusload-file))
	      (princ ")\n"))
	     (t
	      (princ (format "Generating %s...\n" cusload-base-file))
	      (with-temp-file cusload-file
		(insert ";;; " cusload-base-file
			" --- automatically extracted custom dependencies\n"
			"\n;;; Code:\n\n"
			"(autoload 'custom-add-loads \"cus-load\")\n\n")
		(mapatoms
		 (lambda (sym)
		   (let ((members (get sym 'custom-group))
			 item where found)
		     (when members
		       (while members
			 (setq item (car (car members))
			       members (cdr members)
			       where (gethash item hash))
			 (unless (or (null where)
				     (member where found))
			   (if found
			       (insert " ")
			     (insert "(custom-add-loads '"
				     (prin1-to-string sym) " '("))
			   (prin1 where (current-buffer))
			   (push where found)))
		       (when found
			 (insert "))\n"))))))
		(insert "\n;;; custom-load.el ends here\n"))
	      (clrhash hash)))))))))

(provide 'cus-dep)

;;; cus-dep.el ends here
