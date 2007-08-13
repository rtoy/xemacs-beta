;;; cus-dep.el --- Find customization dependencies.
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>, then
;;         Richar Stallman <rms@gnu.ai.mit.edu>, then
;;         Hrvoje Niksic <hniksic@srce.hr>
;; Maintainer: Hrvoje Niksic <hniksic@srce.hr>
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

;;; Code:

(require 'cl)
(require 'widget)
(require 'cus-edit)
(require 'cus-face)

(defconst cusload-base-file "custom-load.el")

;;;###autoload
(defun custom-make-dependencies (&optional subdirs)
  "Extract custom dependencies from .el files in SUBDIRS.
SUBDIRS is a list of directories.  If it is nil, the command-line
arguments are used.  If it is a string, only that directory is
processed.  This function is especially useful in batch mode.

Batch usage: xemacs -batch -l cus-dep.el -f custom-make-dependencies DIRS"
  (interactive "DDirectory: ")
  (and (stringp subdirs)
       (setq subdirs (list subdirs)))
  (or subdirs
      (setq subdirs command-line-args-left))
  (setq subdirs (mapcar #'expand-file-name subdirs))
  (with-temp-buffer
    (let ((enable-local-eval nil)
	  (hash (make-hash-table :test 'eq)))
      (dolist (dir subdirs)
	(message "Processing %s" dir)
	(let ((cusload-file (expand-file-name cusload-base-file dir)))
	  (dolist (file (directory-files dir t "\\`[^=].*\\.el\\'"))
	    (when (file-exists-p file)
	      (erase-buffer)
	      (insert-file-contents file)
	      (goto-char (point-min))
	      (let ((name (file-name-sans-extension
			   (file-name-nondirectory file))))
		(condition-case nil
		    (while (re-search-forward
			    "^(defcustom\\|^(defface\\|^(defgroup"
			    nil t)
		      (beginning-of-line)
		      (let ((expr (read (current-buffer))))
			(eval expr)
			(setf (gethash (nth 1 expr) hash) name)))
		  (error nil)))))
	  (message "Generating %s..." cusload-base-file)
	  (with-temp-file cusload-file
	    (insert ";;; " cusload-base-file
		    " --- automatically extracted custom dependencies\n"
		    ";;\n;;; Code:\n\n")
	    (mapatoms (lambda (sym)
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
;;;				  (insert "(custom-add-loads '" (symbol-name sym)
				  (insert "(custom-put '" (symbol-name sym)
					  " '("))
				(prin1 where (current-buffer))
				(push where found)))
			    (when found
			      (insert "))\n"))))))
	    (insert "\n;;; custom-load.el ends here\n"))
	  (clrhash hash))))))

(provide 'cus-dep)

;;; cus-dep.el ends here
