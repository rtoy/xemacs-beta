;; version.el --- Record version number of Emacs.

;; Copyright (C) 1985, 1991-1994, 1997 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
;; Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

(defconst xemacs-betaname
  (and emacs-beta-version (format "(beta%d)" emacs-beta-version))
  "Non-nil when this is a test (beta) version of XEmacs.
Warning, this variable did not exist in XEmacs versions prior to 20.3")

(defconst emacs-version
  (format "%d.%d %s%s%s%s%s"
	  emacs-major-version
	  emacs-minor-version
	  (if emacs-patch-level
	      (format "(patch %d)" emacs-patch-level)
	    "")
	  (if xemacs-betaname
	      (concat " " xemacs-betaname)
	    "")
	  (if xemacs-codename
	      (concat " \"" xemacs-codename "\"")
	    "")
	  (if xemacs-extra-name
	      (concat " " xemacs-extra-name)
	    "")
	  " XEmacs Lucid")
  "Version numbers of this version of XEmacs.")

(if (featurep 'infodock)
    (require 'id-vers))

;; Moved to C code as of XEmacs 20.3
;(defconst emacs-major-version
;  (progn (or (string-match "^[0-9]+" emacs-version)
;	     (error "emacs-version unparsable"))
;         (string-to-int (match-string 0 emacs-version)))
;  "Major version number of this version of Emacs, as an integer.
;Warning, this variable did not exist in Emacs versions earlier than:
;  FSF Emacs:   19.23
;  XEmacs:      19.10")

;; Moved to C code as of XEmacs 20.3
;(defconst emacs-minor-version
;  (progn (or (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
;	     (error "emacs-version unparsable"))
;         (string-to-int (match-string 1 emacs-version)))
;  "Minor version number of this version of Emacs, as an integer.
;Warning, this variable did not exist in Emacs versions earlier than:
;  FSF Emacs:   19.23
;  XEmacs:      19.10")

(defconst emacs-build-time (current-time-string)
  "Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defun emacs-version  (&optional arg)
  "Return string describing the version of Emacs that is running.
When called interactively with a prefix argument, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "p")
  (save-match-data
    (let ((version-string
	   (format
	    "XEmacs %s %s(%s%s) of %s %s on %s"
	    (substring emacs-version 0 (string-match " XEmacs" emacs-version))
	    (if (not (featurep 'infodock))
		"[Lucid] "
	      "")
	    system-configuration
	    (cond ((or (and (fboundp 'featurep)
			    (featurep 'mule))
		       (memq 'mule features)) ", Mule")
		  (t ""))
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time
		       (string-match "[0-9]*$" emacs-build-time))
	    emacs-build-system)))
      (cond
       ((null arg) version-string)
       ((eq arg 1) (message "%s" version-string))
       (t          (insert version-string))))))

;; from emacs-vers.el
(defun emacs-version>= (major &optional minor patch)
  "Return true if the Emacs version is >= to the given MAJOR, MINOR,
   and PATCH numbers.
The MAJOR version number argument is required, but the other arguments
argument are optional. Only the Non-nil arguments are used in the test."
  (let ((emacs-patch (or emacs-patch-level emacs-beta-version -1)))
    (cond ((> emacs-major-version major))
	  ((< emacs-major-version major) nil)
	  ((null minor))
	  ((> emacs-minor-version minor))
	  ((< emacs-minor-version minor) nil)
	  ((null patch))
	  ((>= emacs-patch patch)))))

;;; We hope that this alias is easier for people to find.
(define-function 'version 'emacs-version)

(defvar Installation-file-coding-system
  (eval-when-compile `,(coding-system-name (find-coding-system 'native)))
  "The coding system used to create the `Installation' file.

The `Installation' file is created by configure, and the
`Installation-string' variable reflects its contents.

This is initialized to reflect the native coding system at the time
version.el was byte-compiled; ideally it would reflect the native coding
system of the environment when XEmacs was dumped, but the locale
initialization code isn't called at dump time, and the appropriate value
at byte-compile time should be close enough.  Note that this means that the
value of `Installation-string' during dump time thus reflects loading the
file using the `binary' coding system.  ")

(defvar Installation-string
  ;; Initialize Installation-string.  We do it before loading
  ;; anything so that dumped code can make use of its value.
  (save-current-buffer
    (set-buffer (get-buffer-create (generate-new-buffer-name
				    " *temp*")))
    ;; insert-file-contents-internal bogusly calls
    ;; format-decode without checking if it's defined.
    (fset 'format-decode #'(lambda (f l &optional v) l))
    (insert-file-contents-internal
     (expand-file-name "Installation" build-directory)
     ;; Relies on our working out the system coding system
     ;; correctly at startup.
     nil nil nil nil 
     ;; Installation-file-coding-system is actually respected in
     ;; mule/general-late.el, after all the dumped coding systems have been
     ;; loaded.
     'binary)
    (fmakunbound 'format-decode)
    (prog1 (buffer-substring)
      (kill-buffer (current-buffer))))
  "Description of XEmacs installation.

This reflects the values that the configure script worked out at build time,
including things like the C code features included at compile time and the
installation prefix.  Normally used when submitting a bug report;
occasionally used, in a way the XEmacs developers don't endorse, to work out
version information.  ")

;;; version.el ends here