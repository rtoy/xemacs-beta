;;; package-info.el --- Generate information about an XEmacs package

;; Copyright (C) 1998 by Free Software Foundation, Inc.

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

;; This file is used for building package distributions.

;;; Change Log:

;;; Code:

(defvar package-info "package-info"
  "File used to write out Package info")

(defvar package-info-template "package-info.in"
  "Template file for package-get info.")

;; Loses with Mule
;(defun pi-md5sum (file)
;  (let (result)
;    (with-temp-buffer
;      (let ((buffer-file-coding-system-for-read 'binary))
;	(insert-file-contents-literally file))
;      ;; (write-file "/tmp/x.x")
;      (setq result (md5 (current-buffer))))
;    result))

(defun pi-md5sum (file)
  (with-temp-buffer
    (call-process "md5sum" file t)
    (goto-char (point-min))
    (looking-at "[a-z0-9]+")
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun pi-update-key (key value)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward key)
	(replace-match value t)))))

(defun batch-update-package-info ()
  "Generate a package-info file for use by package-get.el.
Parameters are:
version -- Package version number
filename -- Filename of tarball to generate info for."
  (unless noninteractive
    (error "`batch-update-package-info' is to be used only with -batch"))
  (let ((version (nth 0 command-line-args-left))
	(filename (nth 1 command-line-args-left)))
    (find-file package-info)
    (erase-buffer)
    (insert-file-contents-literally package-info-template)
    (goto-char (point-min))
    (pi-update-key "VERSION" (format "\"%s\"" version))
    (pi-update-key "MD5SUM" (format "\"%s\""
				    (pi-md5sum filename)))
    (pi-update-key "FILENAME" (format "\"%s\""
				      (file-name-nondirectory filename)))
    (pi-update-key "SIZE" (format "%d"
				  (nth 7 (file-attributes filename))))
    (save-buffers-kill-emacs 0)))

(provide 'package-info)

;;; package-info.el ends here
