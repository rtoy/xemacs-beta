;;; behavior.el --- consistent interface onto behaviors

;; Copyright (C) 2000, 2001, 2002 Ben Wing.

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

;; Hash table mapping behavior names to property lists, with entries for
;; :short-doc, :require, :enable, and :disable.
(defconst behavior-hash-table (make-hash-table))

(defvar within-behavior-enabling-disabling nil)

(defgroup behaviors nil
  "Behaviors -- high-level functionality interface.")

;; List of enabled behaviors.
(defcustom enabled-behavior-list nil
  "List of currently enabled behaviors.
Normally, don't set it directly; use `enable-behavior' or `disable-behavior'."
  :initialize #'set-default
  :set #'(lambda (sym val)
	   (if within-behavior-enabling-disabling
	       (set sym val)
	     (let* ((old-val enabled-behavior-list)
		    (disable-list (set-difference old-val val))
		    (enable-list (set-difference val old-val)))
	       (dolist (b disable-list)
		 (disable-behavior b t))
	       (dolist (b enable-list)
		 (enable-behavior b t))
	       (assert (equal (sort (copy-sequence enabled-behavior-list) 'string-lessp)
			      (sort (copy-sequence val) 'string-lessp))))))
  :type '(repeat (symbol :tag "Behavior"))
  :group 'behaviors)


(defvar behavior-history nil
  "History of entered behaviors.")

(defun define-behavior (name doc-string &rest cl-keys)
  "Define a behavior named NAME.
DOC-STRING must be specified, a description of what the behavior does
when it's enabled and how to further control it (typically through
custom variables).  Accepted keywords are

:short-doc  A \"pretty\" version of the name, for use in menus.  If omitted
              a prettified name will be generated.
:require    A single symbol or a list of such symbols, which need to be
              present at enable time, or will be loaded using `require'.
:enable     A function of no variables, which turns the behavior on.
:disable    A function of no variables, which turns the behavior off.

Behaviors are assumed to be global, and to take effect immediately; if
the underlying package is per-buffer, it may have to scan all existing
buffers and frob them.  When a behavior is disabled, it should completely
go away *everywhere*, as if it were never invoked at all.

The :disable keywords can be missing, although this is considered bad
practice.  In such a case, attempting to disable the behavior will signal
an error unless you use the `force' option."
  (cl-parsing-keywords
      ((:short-doc (capitalize-string-as-title (replace-in-string
						(symbol-name name) "-" " ")))
       :require
       :enable
       :disable)
      t
    (let ((entry (list :short-doc cl-short-doc :require cl-require
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

(defun behavior-enabled-p (behavior)
  "Non-nil if BEHAVIOR (a symbol) if currently enabled."
  (memq behavior enabled-behavior-list))

(defun enable-behavior (behavior &optional force)
  "Enable the specified behavior."
  (interactive (list (read-behavior "Enable Behavior: " t) current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (or force (not (memq behavior enabled-behavior-list))
	(error 'invalid-change "Behavior already enabled" behavior))
    (let ((require (getf plist :require))
	  (enable (getf plist :enable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (message "Enabling behavior %s..." behavior)
      (if enable (funcall enable))
      (message "Enabling behavior %s...done" behavior)
      (let ((within-behavior-enabling-disabling t))
	(customize-set-variable 'enabled-behavior-list
				(cons behavior enabled-behavior-list))))))

(defun disable-behavior (behavior &optional force)
  "Disable the specified behavior."
  (interactive (list (read-behavior "Disable Behavior: " t)
		     current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (or force (memq behavior enabled-behavior-list)
	(error 'invalid-change "Behavior not enabled" behavior))
    (let ((require (getf plist :require))
	  (disable (getf plist :disable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (message "Disabling behavior %s..." behavior)
      (if disable (funcall disable))
      (message "Disabling behavior %s...done" behavior)
      (let ((within-behavior-enabling-disabling t))
	(customize-set-variable 'enabled-behavior-list
				(delq behavior enabled-behavior-list))))))

(provide 'behavior)

;;; finder-inf.el ends here
