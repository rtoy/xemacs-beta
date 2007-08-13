;;; Toolbar related functions and commands
;;; Copyright (C) 1995 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'vm-toolbar)

(defvar vm-toolbar-specifier nil)

(defvar vm-toolbar-next-button
  [vm-toolbar-next-icon
   vm-toolbar-next-command
   (vm-toolbar-any-messages-p)
   "Go to the next message.\n
The command `vm-toolbar-next-command' is run, which is normally
bound to `vm-next-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-next-command 'some-other-command)"])
(defvar vm-toolbar-next-icon nil)
(or (fboundp 'vm-toolbar-next-command)
    (fset 'vm-toolbar-next-command 'vm-next-message))

(defvar vm-toolbar-previous-button
  [vm-toolbar-previous-icon
   vm-toolbar-previous-command
   (vm-toolbar-any-messages-p)
   "Go to the previous message.\n
The command `vm-toolbar-previous-command' is run, which is normally
bound to `vm-previous-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-previous-command 'some-other-command)"])
(defvar vm-toolbar-previous-icon nil)
(or (fboundp 'vm-toolbar-previous-command)
    (fset 'vm-toolbar-previous-command 'vm-previous-message))

(defvar vm-toolbar-autofile-button
  [vm-toolbar-autofile-icon
   vm-toolbar-autofile-message
   (vm-toolbar-can-autofile-p)
  "Save the current message to a folder selected using vm-auto-folder-alist."])
(defvar vm-toolbar-autofile-icon nil)

(defvar vm-toolbar-file-button
  [vm-toolbar-file-icon vm-toolbar-file-command (vm-toolbar-any-messages-p)
   "Save the current message to a folder.\n
The command `vm-toolbar-file-command' is run, which is normally
bound to `vm-save-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-file-command 'some-other-command)"])
(defvar vm-toolbar-file-icon nil)
(or (fboundp 'vm-toolbar-file-command)
    (fset 'vm-toolbar-file-command 'vm-save-message))

(defvar vm-toolbar-print-button
  [vm-toolbar-print-icon
   vm-toolbar-print-command
   (vm-toolbar-any-messages-p)
   "Print the current message.\n
The command `vm-toolbar-print-command' is run, which is normally
bound to `vm-print-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-print-command 'some-other-command)"])
(defvar vm-toolbar-print-icon nil)
(or (fboundp 'vm-toolbar-print-command)
    (fset 'vm-toolbar-print-command 'vm-print-message))

(defvar vm-toolbar-visit-button
  [vm-toolbar-visit-icon vm-toolbar-visit-command t
   "Visit a different folder.\n
The command `vm-toolbar-visit-command' is run, which is normally
bound to `vm-visit-folder'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-visit-command 'some-other-command)"])
(defvar vm-toolbar-visit-icon nil)
(or (fboundp 'vm-toolbar-visit-command)
    (fset 'vm-toolbar-visit-command 'vm-visit-folder))

(defvar vm-toolbar-reply-button
  [vm-toolbar-reply-icon
   vm-toolbar-reply-command
   (vm-toolbar-any-messages-p)
   "Reply to the current message.\n
The command `vm-toolbar-reply-command' is run, which is normally
bound to `vm-followup-include-text'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-reply-command 'some-other-command)"])
(defvar vm-toolbar-reply-icon nil)
(or (fboundp 'vm-toolbar-reply-command)
    (fset 'vm-toolbar-reply-command 'vm-followup-include-text))

(defvar vm-toolbar-compose-button
  [vm-toolbar-compose-icon vm-toolbar-compose-command t
   "Compose a new message.\n
The command `vm-toolbar-compose-command' is run, which is normally
bound to `vm-mail'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-compose-command 'some-other-command)"])
(defvar vm-toolbar-compose-icon nil)
(or (fboundp 'vm-toolbar-compose-command)
    (fset 'vm-toolbar-compose-command 'vm-mail))

(defvar vm-toolbar-delete-icon nil)

(defvar vm-toolbar-undelete-icon nil)

(defvar vm-toolbar-delete/undelete-button
  [vm-toolbar-delete/undelete-icon
   vm-toolbar-delete/undelete-message
   (vm-toolbar-any-messages-p)
   "Delete the current message, or undelete it if it is already deleted."])
(defvar vm-toolbar-delete/undelete-icon nil)
(make-variable-buffer-local 'vm-toolbar-delete/undelete-icon)

(defvar vm-toolbar-help-icon nil)

(defvar vm-toolbar-recover-icon nil)

(defvar vm-toolbar-helper-icon nil)
(make-variable-buffer-local 'vm-toolbar-helper-icon)

(defvar vm-toolbar-help-button
  [vm-toolbar-helper-icon vm-toolbar-helper-command t
   "Don't Panic.\n
VM uses this button to offer help if you're in trouble.
Under normal circumstances, this button runs `vm-help'.\n
If the current folder looks out-of-date relative to its auto-save
file then this button will run `recover-file'."])

(defvar vm-toolbar-helper-command nil)
(make-variable-buffer-local 'vm-toolbar-helper-command)

(defun vm-toolbar-helper-command ()
  (interactive)
  (setq this-command vm-toolbar-helper-command)
  (call-interactively vm-toolbar-helper-command))

(defvar vm-toolbar-quit-button
  [vm-toolbar-quit-icon vm-toolbar-quit-command t
   "Quit visiting this folder.\n
The command `vm-toolbar-quit-command' is run, which is normally
bound to `vm-quit'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'vm-toolbar-quit-command 'some-other-command)"])
(defvar vm-toolbar-quit-icon nil)
(or (fboundp 'vm-toolbar-quit-command)
    (fset 'vm-toolbar-quit-command 'vm-quit))

(defun vm-toolbar-any-messages-p ()
  (save-excursion
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer)
    vm-message-list))

(defun vm-toolbar-delete/undelete-message (&optional prefix-arg)
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((current-prefix-arg prefix-arg))
    (if (vm-deleted-flag (car vm-message-pointer))
	(call-interactively 'vm-undelete-message)
      (call-interactively 'vm-delete-message))))

(defun vm-toolbar-can-autofile-p ()
  (interactive)
  (save-excursion
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer)
    (and vm-message-pointer
	 (vm-auto-select-folder vm-message-pointer vm-auto-folder-alist))))

(defun vm-toolbar-autofile-message ()
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((file (vm-auto-select-folder vm-message-pointer vm-auto-folder-alist)))
    (if file
	(progn
	  (vm-save-message file 1)
	  (message "Message saved to %s" file))
      (error "No match for message in vm-auto-folder-alist."))))

(defun vm-toolbar-can-recover-p ()
  (save-excursion
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer)
    (and vm-folder-read-only
	 buffer-file-name
	 buffer-auto-save-file-name
	 (null (buffer-modified-p))
	 (file-newer-than-file-p
	  buffer-auto-save-file-name
	  buffer-file-name))))

(defun vm-toolbar-update-toolbar ()
  (if (and vm-message-pointer (vm-deleted-flag (car vm-message-pointer)))
      (setq vm-toolbar-delete/undelete-icon vm-toolbar-undelete-icon)
    (setq vm-toolbar-delete/undelete-icon vm-toolbar-delete-icon))
  (cond ((vm-toolbar-can-recover-p)
	 (setq vm-toolbar-helper-command 'recover-file
	       vm-toolbar-helper-icon vm-toolbar-recover-icon))
	(t
	 (setq vm-toolbar-helper-command 'vm-help
	       vm-toolbar-helper-icon vm-toolbar-help-icon)))
  (if vm-summary-buffer
      (vm-copy-local-variables vm-summary-buffer
			       'vm-toolbar-delete/undelete-icon
			       'vm-toolbar-helper-command
			       'vm-toolbar-helper-icon))
  (and vm-toolbar-specifier
       (progn
	 (set-specifier vm-toolbar-specifier (cons (current-buffer) nil))
	 (set-specifier vm-toolbar-specifier (cons (current-buffer)
						   vm-toolbar)))))

(defun vm-toolbar-install-toolbar ()
  (vm-toolbar-initialize)
  (let ((height (+ 4 (glyph-height (car vm-toolbar-help-icon))))
	(width (+ 4 (glyph-width (car vm-toolbar-help-icon))))
	toolbar )
    ;; honor user setting of vm-toolbar if they are daring enough
    ;; to set it.
    (if vm-toolbar
	(setq toolbar vm-toolbar)
      (setq toolbar (vm-toolbar-make-toolbar-spec)
	    vm-toolbar toolbar))
    (cond ((eq vm-toolbar-orientation 'right)
	   (setq vm-toolbar-specifier right-toolbar)
	   (set-specifier right-toolbar (cons (current-buffer) toolbar))
	   (set-specifier right-toolbar-width
			  (cons (selected-frame) width)))
	  ((eq vm-toolbar-orientation 'left)
	   (setq vm-toolbar-specifier left-toolbar)
	   (set-specifier left-toolbar (cons (current-buffer) toolbar))
	   (set-specifier left-toolbar-width
			  (cons (selected-frame) width)))
	  ((eq vm-toolbar-orientation 'bottom)
	   (setq vm-toolbar-specifier bottom-toolbar)
	   (set-specifier bottom-toolbar (cons (current-buffer) toolbar))
	   (set-specifier bottom-toolbar-height
			  (cons (selected-frame) height)))
	  (t
	   (setq vm-toolbar-specifier top-toolbar)
	   (set-specifier top-toolbar (cons (current-buffer) toolbar))
	   (set-specifier top-toolbar-height
			  (cons (selected-frame) height))))))

(defun vm-toolbar-make-toolbar-spec ()
  (let ((button-alist '(
			(autofile . vm-toolbar-autofile-button)
			(compose . vm-toolbar-compose-button)
			(delete/undelete . vm-toolbar-delete/undelete-button)
			(file . vm-toolbar-file-button)
			(help . vm-toolbar-help-button)
			(next . vm-toolbar-next-button)
			(previous . vm-toolbar-previous-button)
			(print . vm-toolbar-print-button)
			(quit . vm-toolbar-quit-button)
			(reply . vm-toolbar-reply-button)
			(visit . vm-toolbar-visit-button)
			))
	(button-list vm-use-toolbar)
	cons
	(toolbar nil))
    (while button-list
      (if (null (car button-list))
	  (setq toolbar (cons nil toolbar))
	(setq cons (assq (car button-list) button-alist))
	(if cons
	    (setq toolbar (cons (symbol-value (cdr cons)) toolbar))))
      (setq button-list (cdr button-list)))
    (nreverse toolbar) ))

(defun vm-toolbar-initialize ()
  ;; drag these in now instead of waiting for them to be
  ;; autoloaded.  the "loading..." messages could come at a bad
  ;; moment and wipe an important echo area message, like "Auto
  ;; save file is newer..."
  (require 'vm-save)
  (require 'vm-summary)
  (cond
   ((null vm-toolbar-help-icon)
    (let ((tuples
	   (if (featurep 'xpm)
	       '(
 (vm-toolbar-next-icon "next-up.xpm" "next-dn.xpm" "next-dn.xpm")
 (vm-toolbar-previous-icon "previous-up.xpm" "previous-dn.xpm"
			   "previous-dn.xpm")
 (vm-toolbar-delete-icon "delete-up.xpm" "delete-dn.xpm" "delete-dn.xpm")
 (vm-toolbar-undelete-icon "undelete-up.xpm" "undelete-dn.xpm"
			   "undelete-dn.xpm")
 (vm-toolbar-autofile-icon "autofile-up.xpm" "autofile-dn.xpm"
			   "autofile-dn.xpm")
 (vm-toolbar-file-icon "file-up.xpm" "file-dn.xpm" "file-dn.xpm")
 (vm-toolbar-reply-icon "reply-up.xpm" "reply-dn.xpm" "reply-dn.xpm")
 (vm-toolbar-compose-icon "compose-up.xpm" "compose-dn.xpm" "compose-dn.xpm")
 (vm-toolbar-print-icon "print-up.xpm" "print-dn.xpm" "print-dn.xpm")
 (vm-toolbar-visit-icon "visit-up.xpm" "visit-dn.xpm" "visit-dn.xpm")
 (vm-toolbar-quit-icon "quit-up.xpm" "quit-dn.xpm" "quit-dn.xpm")
 (vm-toolbar-help-icon "help-up.xpm" "help-dn.xpm" "help-dn.xpm")
 (vm-toolbar-recover-icon "recover-up.xpm" "recover-dn.xpm" "recover-dn.xpm")
	   )
	       '(
 (vm-toolbar-next-icon "next-up.xbm" "next-dn.xbm" "next-xx.xbm")
 (vm-toolbar-previous-icon "previous-up.xbm" "previous-dn.xbm"
			   "previous-xx.xbm")
 (vm-toolbar-delete-icon "delete-up.xbm" "delete-dn.xbm" "delete-xx.xbm")
 (vm-toolbar-undelete-icon "undelete-up.xbm" "undelete-dn.xbm"
			   "undelete-xx.xbm")
 (vm-toolbar-autofile-icon "autofile-up.xbm" "autofile-dn.xbm"
			   "autofile-xx.xbm")
 (vm-toolbar-file-icon "file-up.xbm" "file-dn.xbm" "file-xx.xbm")
 (vm-toolbar-reply-icon "reply-up.xbm" "reply-dn.xbm" "reply-xx.xbm")
 (vm-toolbar-compose-icon "compose-up.xbm" "compose-dn.xbm" "compose-xx.xbm")
 (vm-toolbar-print-icon "print-up.xbm" "print-dn.xbm" "print-xx.xbm")
 (vm-toolbar-visit-icon "visit-up.xbm" "visit-dn.xbm" "visit-xx.xbm")
 (vm-toolbar-quit-icon "quit-up.xbm" "quit-dn.xbm" "quit-xx.xbm")
 (vm-toolbar-help-icon "help-up.xbm" "help-dn.xbm" "help-xx.xpm")
 (vm-toolbar-recover-icon "recover-up.xbm" "recover-dn.xbm" "recover-xx.xpm")
	   )))
	  tuple files var)
      (if (not (file-directory-p vm-toolbar-pixmap-directory))
	  (error "Bad toolbar pixmap directory: %s"
		 vm-toolbar-pixmap-directory)
	(while tuples
	  (setq tuple (car tuples)
		var (car tuple)
		files (cdr tuple))
	  (set var (mapcar
		    (function
		     (lambda (f)
		       (make-glyph
			(expand-file-name f vm-toolbar-pixmap-directory))))
		    files))
	  (setq tuples (cdr tuples)))))))
  (setq vm-toolbar-delete/undelete-icon vm-toolbar-delete-icon)
  (setq vm-toolbar-helper-command 'vm-help)
  (setq vm-toolbar-helper-icon vm-toolbar-help-icon))