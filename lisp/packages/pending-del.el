;; pending-del.el --- Making insertions replace any selected text.

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Matthieu Devin <devin@lucid.com>, 14 Jul 92.
;; Maintainer: Hrvoje Niksic <hniksic@srce.hr>
;; Version 2.2

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: 19.34  (distributed as delsel.el in FSF)

;;; Commentary:

;; Much of this code was revamped by Hrvoje Niksic, July 1997, with
;; version number set to 2.x.

;; Pending-del is now a minor mode, with all the normal toggle
;; functions.  It should be somewhat faster, too.


;;; Code:

(defcustom pending-delete-mode nil
  "Non-nil when Pending Delete mode is enabled.
In Pending Delete mode, typed text replaces the selected region."
  :type 'boolean
  :set (lambda (symbol value)
	 (pending-delete-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'pending-del
  :group 'keyboard)

(defcustom pending-delete-modeline-string " PenDel"
  "*String to display in the modeline when Pending Delete mode is active."
  :type 'string
  :group 'keyboard)

(add-minor-mode 'pending-delete-mode 'pending-delete-modeline-string)


(defun pending-delete-active-region (&optional killp)
  (when (and (region-active-p)
	     (eq (extent-object zmacs-region-extent) (current-buffer))
	     (not buffer-read-only))
    ;; Here we used to check whether the point lies between the
    ;; beginning and end of the extent.  I don't see how it is
    ;; necessary, as the C code makes sure that this is so; it only
    ;; slow things down.
    (if killp
	(kill-region (region-beginning) (region-end))
      (delete-region (region-beginning) (region-end)))
    (zmacs-deactivate-region)
    t))

(defun pending-delete-pre-hook ()
  (condition-case e
      (let ((type (and (symbolp this-command)
		       (get this-command 'pending-delete))))
	(cond ((eq type 'kill)
	       (pending-delete-active-region t))
	      ((eq type 'supersede)
	       (if (pending-delete-active-region ())
		   (setq this-command (lambda () (interactive)))))
	      (type
	       (pending-delete-active-region ()))))
    (error
     (warn "Error caught in `pending-delete-pre-hook': %s"
	   (error-message-string e)))))


(put 'self-insert-command 'pending-delete t)

(put 'yank 'pending-delete t)
(put 'x-yank-clipboard-selection 'pending-delete t)
(put 'toolbar-paste 'pending-delete t)

(put 'delete-backward-char 'pending-delete 'supersede)
(put 'backward-delete-char-untabify 'pending-delete 'supersede)
(put 'delete-char 'pending-delete 'supersede)
(put 'c-electric-delete 'pending-delete 'supersede)

;; Support the XEmacs 20.3 'delete functions

(put 'backward-or-forward-delete-char 'pending-delete 'supersede)
(put 'cperl-electric-backspace 'pending-delete 'supersede)
(put 'cperl-electric-delete 'pending-delete 'supersede)

;; Don't delete for these.  They're more problematic than helpful.
;;
;; (put 'newline-and-indent 'pending-delete t)
;; (put 'newline 'pending-delete t)
;; (put 'open-line 'pending-delete t)

(put 'insert-register 'pending-delete t)


;;;###autoload
(defun turn-on-pending-delete (&optional ignored)
  "Turn on pending delete minor mode unconditionally."
  (interactive)
  (pending-delete-mode 1))

;;;###autoload
(defun turn-off-pending-delete (&optional ignored)
  "Turn off pending delete minor mode unconditionally."
  (interactive)
  (pending-delete-mode 0))

;;;###autoload
(defun pending-delete-mode (&optional arg)
  "Toggle Pending Delete minor mode.
When the pending delete is on, typed text replaces the selection.
With a positive argument, turns it on.
With a non-positive argument, turns it off."
  (interactive "P")
  (setq pending-delete-mode
	(if (null arg) (not pending-delete-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if pending-delete-mode
      (add-hook 'pre-command-hook 'pending-delete-pre-hook)
    (remove-hook 'pre-command-hook 'pending-delete-pre-hook))
  (force-mode-line-update))


;; Backward compatibility:
;;;###autoload
(define-obsolete-function-alias 'pending-delete-on 'turn-on-pending-delete)
;;;###autoload
(define-obsolete-function-alias 'pending-delete-off 'turn-off-pending-delete)

;; FSF compatibility:
;;;###autoload
(define-compatible-function-alias 'delete-selection-mode 'pending-delete-mode)

;; Compatibility and convenience:
;;;###autoload
(defalias 'pending-delete 'pending-delete-mode)


;; The following code used to turn the mode on unconditionally.
;; However, this is a very bad idea -- since pending-del is
;; autoloaded, (turn-on-pending-delete) is as easy to add to `.emacs'
;; as (require 'pending-del) used to be.

;(pending-delete-on (eq pending-delete-verbose t))

(provide 'pending-del)

;;; pending-del.el ends here
