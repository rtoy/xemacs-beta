;; pending-del.el --- Making insertions replace any selected text.

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Matthieu Devin <devin@lucid.com>, 14 Jul 92.

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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF.

;;; Code:

(defvar pending-delete-verbose
  1
  "*nil disables on/off messages for pending-del mode
1 suppresses messages on loading
t enables all messages")

(defun delete-active-region (&optional killp)
  (if (and (not buffer-read-only)
	   (extentp zmacs-region-extent)
	   (eq (current-buffer) (extent-buffer zmacs-region-extent))
	   (extent-start-position zmacs-region-extent)
	   (<= (extent-start-position zmacs-region-extent) (point))
	   (<= (point) (extent-end-position zmacs-region-extent)))
      (progn
	(if killp
	    (kill-region (extent-start-position zmacs-region-extent)
			 (extent-end-position zmacs-region-extent))
	  (delete-region (extent-start-position zmacs-region-extent)
			 (extent-end-position zmacs-region-extent)))
	(zmacs-deactivate-region)
	t)))

(defun pending-delete-pre-hook ()
  (let ((type (and (symbolp this-command)
		   (get this-command 'pending-delete))))
    (cond ((eq type 'kill)
	   (delete-active-region t))
	  ((eq type 'supersede)
	   (if (delete-active-region ())
	       (setq this-command '(lambda () (interactive)))))
	  (type
	   (delete-active-region ())))))

(put 'self-insert-command 'pending-delete t)

(put 'yank 'pending-delete t)
(put 'x-yank-clipboard-selection 'pending-delete t)

(put 'delete-backward-char 'pending-delete 'supersede)
(put 'backward-delete-char-untabify 'pending-delete 'supersede)
(put 'delete-char 'pending-delete 'supersede)
(put 'c-electric-delete 'pending-delete 'supersede)

;; Don't delete for these.  They're more problematic than helpful.
;;
;; (put 'newline-and-indent 'pending-delete t)
;; (put 'newline 'pending-delete t)
;; (put 'open-line 'pending-delete t)

(put 'insert-register 'pending-delete t)

;;;###autoload
(defun pending-delete-on (verbose)
  "Turn on pending delete.
When it is ON, typed text replaces the selection if the selection is active.
When it is OFF, typed text is just inserted at point."
  (interactive "P")
  (add-hook 'pre-command-hook 'pending-delete-pre-hook)
  (and verbose
    (message "Pending delete is ON, use M-x pending-delete to turn it OFF")))

;;;###autoload
(defun pending-delete-off (verbose)
  "Turn off pending delete.
When it is ON, typed text replaces the selection if the selection is active.
When it is OFF, typed text is just inserted at point."
  (interactive "P")
  (remove-hook 'pre-command-hook 'pending-delete-pre-hook)
  (and verbose (message "pending delete is OFF")))

;;;###autoload
(defun pending-delete (&optional arg)
  "Toggle automatic deletion of the selected region.
With a positive argument, turns it on.
With a non-positive argument, turns it off.
When active, typed text replaces the selection."
  (interactive "P")
  (let* ((was-on (not (not (memq 'pending-delete-pre-hook pre-command-hook))))
	 (on-p (if (null arg)
		   (not was-on)
		(> (prefix-numeric-value arg) 0))))
    (cond ((eq on-p was-on)
	   nil)
	  (on-p
	   (pending-delete-on pending-delete-verbose))
	  (t
	   (pending-delete-off pending-delete-verbose)))))
  
;; Add pending-del mode.  Assume that if we load it then we obviously wanted
;; it on, even if it is already on.
(pending-delete-on (eq pending-delete-verbose t))

(provide 'pending-del)

;;; pending-del.el ends here
