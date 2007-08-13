;;; packages.el --- Low level support for XEmacs packages

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@altair.xemacs.org>
;; Keywords: internal, lisp

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

;; This file provides low level facilities for XEmacs startup.  Special
;; requirements apply to some of these functions because they can be called
;; during build from temacs and much of the usual lisp environment may
;; be missing.

;;; Code:

(defvar autoload-file-name "auto-autoloads.el"
  "Filename that autoloads are expected to be found in.")

(defvar packages-hardcoded-lisp
  '("cl-defs"
    ;; "startup"
    )
  "Lisp packages that are always dumped with XEmacs")

(defvar packages-useful-lisp
  '("bytecomp"
    "byte-optimize"
    "advice")
  "Lisp packages that need early byte compilation.")

(defvar packages-unbytecompiled-lisp
  '("paths.el"
    "version.el")
  "Lisp packages that should not be byte compiled.")

;; Copied from subr.el
(if (null (fboundp 'lambda))
    (defmacro lambda (&rest cdr)
      (list 'function (cons 'lambda cdr))))

;; Copied from help.el, could possibly move it to here permanently.
;; This is taken directly from Emacs 19.34.94.

(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'."
  (interactive (list (read-string "Locate library: ")
                     nil nil
                     t))
  (let (result)
    (catch 'answer
      (mapcar
       (lambda (dir)
         (mapcar
          (lambda (suf)
            (let ((try (expand-file-name (concat library suf) dir)))
              (and (file-readable-p try)
                   (null (file-directory-p try))
                   (progn
                     (setq result try)
                     (throw 'answer try)))))
          (if nosuffix
              '("")
            (let ((basic '(".elc" ".el" ""))
                  (compressed '(".Z" ".gz" "")))
              ;; If autocompression mode is on,
              ;; consider all combinations of library suffixes
              ;; and compression suffixes.
              (if (rassq 'jka-compr-handler file-name-handler-alist)
                  (apply 'nconc
                         (mapcar (lambda (compelt)
                                   (mapcar (lambda (baselt)
                                             (concat baselt compelt))
                                           basic))
                                 compressed))
                basic)))))
       (or path load-path)))
    (and interactive-call
         (if result
             (message "Library is file %s" result)
           (message "No library %s in search path" library)))
    result))

(defun packages-add-suffix (str)
  (if (null (string-match "\\.el\\'" str))
      (concat str ".elc")
    str))

(defun list-autoloads ()
  "List autoload files in (what will be) the normal lisp search path.
This function is used during build to find where the global symbol files so
they can be perused for their useful information."
  ;; Source directory may not be initialized yet.
  ;; (print (prin1-to-string load-path))
  (if (null source-directory)
      (setq source-directory (concat (car load-path) "/..")))
  (let ((files (directory-files source-directory t ".*"))
	file autolist)
    (while (setq file (car-safe files))
      (if (and (file-directory-p file)
	       (file-exists-p (concat file "/" autoload-file-name)))
	  (setq autolist (cons (concat file "/" autoload-file-name)
			       autolist)))
      (setq files (cdr files)))
    autolist))

(provide 'packages)

;;; packages.el ends here
