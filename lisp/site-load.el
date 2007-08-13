;;; site-load.el --- Template file for site-wide XEmacs customization
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Steven L. Baur <steve@altair.xemacs.org>
;; Keywords: internal

;; This file is part of XEmacs.

;;; Commentary:

;; This is a prototype site-load.el file.
;; The site-load.el mechanism is provided so XEmacs installers can easily
;; dump lisp packages with XEmacs that do not get dumped standardly.

;; The file `site-packages' if it exists should look something like:
;; (setq site-load-packages '(
;; "../lisp/modes/cc-mode"
;; "../lisp/utils/redo"
;; "../lisp/packages/scroll-in-place"
;; )
;; )

;; The first line and the last line must be exact.  Each of the packages
;; listed must be double quoted, have either an absolute path, or a relative
;; to the build src directory path *and* be bytecompiled prior to the attempt
;; to dump.

;; Because this is a trial implementation and the file is shared with
;; make-docfiles, syntax is strict and unforgiving.  So sue me.  It
;; is still better than the way it used to be.

;; Also note that site-packages belongs in the top level directory not the
;; lisp directory for use with --srcdir configurations.

;;; Code:
(defvar site-load-package-file "../site-packages"
  "File name containing the list of extra packages to dump with XEmacs.")
(defvar site-load-packages nil
  "A list of .elc files that should be dumped with XEmacs.
This variable should be set by `site-load-package-file'.")

;; Load site specific packages for dumping with the XEmacs binary.
(when (file-exists-p site-load-package-file) 
  (let ((file))
    (load site-load-package-file t t t)
    ;; The `load-gc' macro is provided as a clue that a package is being loaded
    ;; in preparation of being dumped into XEmacs.
    (defmacro load-gc (file)
      (list 'prog1 (list 'load file) '(garbage-collect)))
    (message "Loading site-wide packages for dumping...")
    (while site-load-packages
      (setq file (car site-load-packages))
      (load-gc file)
      (setq site-load-packages (cdr site-load-packages)))
    (message "Loading site-wide packages for dumping...done")
    (fmakunbound 'load-gc)))

;; This file is intended for end user additions.
;; Put other initialization here, like setting of language-environment, etc.
;; Perhaps this should really be in the site-init.el.

;;; site-load.el ends here
