;;; behavior.el --- consistent interface onto behaviors

;; Copyright (C) 2000, 2001 Ben Wing.

;; Author: Ben Wing
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

;;; Authorship:

;; Created July 2000 by Ben Wing.

;;; Commentary:

;; This file will be dumped with XEmacs.

;;; Code:

(defvar behavior-hash-table (make-hash-table))

(defvar behavior-history nil
  "History of entered behaviors.")

(defun define-behavior (name doc-string &rest cl-keys)
  "Define a behavior named NAME.
DOC-STRING must be specified, a description of what the behavior does
when it's enabled and how to further control it (typically through
custom variables).  Accepted keywords are

:title    A \"pretty\" version of the name, for use in menus.  If omitted
          a prettified name will be generated.
:require  A single symbol or a list of such symbols, which need to be
          present at enable time, or will be loaded using `require'.
:enable   A function of no variables, which turns the behavior on.
:disable  A function of no variables, which turns the behavior off.

Behaviors are assumed to be global, and to take effect immediately; if
the underlying package is per-buffer, it may have to scan all existing
buffers and frob them.  When a behavior is disabled, it should completely
go away *everywhere*, as if it were never invoked at all.

The :disable keywords can be missing, although this is considered bad
practice.  In such a case, attempting to disable the behavior will signal
an error unless you use the `force' option."
  (cl-parsing-keywords
      ((:title (capitalize-string-as-title (replace-in-string
					    (symbol-name name) "-" " ")))
       :require
       :enable
       :disable)
      ()
    (let ((entry (list :title cl-title :require cl-require
		       :enable cl-enable :disable cl-disable)))
      (puthash name entry behavior-hash-table))))

(defun read-behavior (prompt &optional must-match initial-contents history
			     default-value)
  "Return a behavior symbol from the minibuffer, prompting with string PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
 in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list. (It defaults to
`behavior-history'.)
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history command, and as the value to return if the user enters the
 empty string."
  (let ((result
	 (completing-read
	  prompt
	  (let ((table (let (lis)
			 (maphash #'(lambda (key val)
				      (push (cons key val) lis))
				  behavior-hash-table)
			 (nreverse lis))))
	    (mapc #'(lambda (aentry)
		      (setcar aentry (symbol-name
				      (car aentry))))
		  table)
	    table)
	  nil must-match initial-contents
	  (or history 'behavior-history)
	  default-value)))
    (if (and result (stringp result))
	(intern result)
      result)))

(defun behavior-enabled-p (name))

(defun enable-behavior (behavior &optional force)
  "Enable the specified behavior."
  (interactive (list (read-behavior "Enable Behavior: " t) current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (let ((require (getf plist :require))
	  (enable (getf plist :enable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (if enable (funcall enable)))))

(defun disable-behavior (behavior &optional force)
  "Disable the specified behavior."
  (interactive (list (read-behavior "Disable Behavior: " t)
		     current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (let ((require (getf plist :require))
	  (disable (getf plist :disable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (if disable (funcall disable)))))

(provide 'behavior)

;;; finder-inf.el ends here
