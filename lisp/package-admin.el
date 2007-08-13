;;; package-admin.el --- Installation and Maintenance of XEmacs packages

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@altair.xemacs.org>
;; Keywords: internal

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

;; First pass at lisp front end to package maintenance.

;;; Code:

(require 'config)

(defvar package-admin-xemacs (concat invocation-directory invocation-name)
  "Location of XEmacs binary to use.")

(defvar package-admin-temp-buffer "*Package Output*"
  "Temporary buffer where output of backend commands is saved.")

(defvar package-admin-install-function 'package-admin-default-install-function
  "The function to call to install a package.
Three args are passed: FILENAME PKG-DIR BUF
Install package FILENAME into directory PKG-DIR, with any messages output
to buffer BUF.")

(defvar package-admin-error-messages '(
				       "No space left on device"
				       "No such file or directory"
				       "Filename too long"
				       "Read-only file system"
				       "File too large"
				       "Too many open files"
				       "Not enough space"
				       "Permission denied"
				       "Input/output error"
				       "Out of memory"
				       "Unable to create directory"
				       "Directory checksum error"
				       "Cannot exclusively open file"
				       "corrupted file"
				       "incomplete .* tree"
				       "Bad table"
				       "corrupt input"
				       "invalid compressed data"
				       "too many leaves in Huffman tree"
				       "not a valid zip file"
				       "first entry not deflated or stored"
				       "encrypted file --"
				       "unexpected end of file"
				       )
  "Regular expressions of possible error messages.
After each package extraction, the `package-admin-temp-buffer' buffer is
scanned for these messages.  An error code is returned if one of these are
found.

This is awful, but it exists because error return codes aren't reliable
under MS Windows.")

;;;###autoload
(defun package-admin-add-single-file-package (file destdir &optional pkg-dir)
  "Install a single file Lisp package into XEmacs package hierarchy.
`file' should be the full path to the lisp file to install.
`destdir' should be a simple directory name.
The optional `pkg-dir' can be used to override the default package hierarchy
\(car \(last late-packages))."
  (interactive "fLisp File: \nsDestination: ")
  (when (null pkg-dir)
    (setq pkg-dir (car (last late-packages))))
  (let ((destination (concat pkg-dir "/lisp/" destdir))
	(buf (get-buffer-create package-admin-temp-buffer)))
    (call-process "add-little-package.sh"
		  nil
		  buf
		  t
		  ;; rest of command line follows
		  package-admin-xemacs file destination)))

(defun package-admin-install-function-mswindows (file pkg-dir buf)
  "Install function for mswindows"
  (let ( (default-directory pkg-dir) )
    (call-process "djtar" nil buf t "-x" file)
    ))

(defun package-admin-default-install-function (file pkg-dir buf)
  "Default function to install a package.
Install package FILENAME into directory PKG-DIR, with any messages output
to buffer BUF."
  (let (filename)
    (setq filename (expand-file-name file pkg-dir))
    (if (shell-command (concat "gunzip -c " filename " | tar xvf -") buf)
	0
      1)
    ))

;  (call-process "add-big-package.sh"
;		nil
;		buf
;		t
;		;; rest of command line follows
;		package-admin-xemacs file pkg-dir))

(defun package-admin-get-install-dir (pkg-dir)
  (when (null pkg-dir)
    (when (or (not (listp late-packages))
	      (not late-packages))
      (error "No package path"))
    (setq pkg-dir (car (last late-packages))))
  pkg-dir
  )

;;;###autoload
(defun package-admin-add-binary-package (file &optional pkg-dir)
  "Install a pre-bytecompiled XEmacs package into package hierarchy."
  (interactive "fPackage tarball: ")
  (setq pkg-dir (package-admin-get-install-dir pkg-dir))
  (let ((buf (get-buffer-create package-admin-temp-buffer))
	(status 1)
	start err-list
	)
    ;; Insure that the current directory doesn't change
    (save-excursion
      (set-buffer buf)
      (setq default-directory pkg-dir)
      (setq case-fold-search t)
      (buffer-disable-undo)
      (goto-char (setq start (point-max)))
      (if (= 0 (setq status (funcall package-admin-install-function
				     file pkg-dir buf)))
	  (catch 'done
	    (goto-char start)
	    (setq err-list package-admin-error-messages)
	    (while err-list
	      (if (re-search-forward (car err-list) nil t)
		  (progn
		    (setq status 1)
		    (throw 'done nil)
		    ))
	      (setq err-list (cdr err-list))
	      )
	    ))
      )
    status
    ))

(provide 'package-admin)

;;; package-admin.el ends here
