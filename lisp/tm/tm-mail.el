;;; tm-mail.el --- mail-mode extension.

;; Copyright (C) 1995,1996 KOBAYASHI Shuhei

;; Author: KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;; modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         and Neal Becker <neal@neal.ctd.comsat.com>
;; Maintainer: KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;; Created: 1995/11/27
;; Version: $Id: tm-mail.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $
;; Keywords: mail, MIME, multimedia

;; This file is part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tm-edit)

(defun tm-mail/insert-message (&optional message)
  (interactive)
  (let* (mail-yank-hooks
	 (mail-citation-hook '(mime-editor/inserted-message-filter))
	 )
    (cond
     ((and (boundp 'vm-mail-buffer) vm-mail-buffer)
      ;; called from VM.
      (let ((mail-reply-buffer vm-mail-buffer))
        (if (null message)
            (call-interactively 'vm-yank-message)
          (vm-yank-message message)))
      )
     ((boundp 'rmail-send-actions-rmail-buffer)
      ;; called from RMAIL, emacs-19.29 or later.
      (mail-yank-original nil)
      )
     ((and (boundp 'gnus-article-buffer) (get-buffer gnus-article-buffer))
      ;; maybe called from Gnus.
      (tm-gnus/insert-article)
      )
     ((and (boundp 'mail-reply-buffer) mail-reply-buffer)
      ;; maybe called from RMAIL.
      (mail-yank-original nil)
      )
     (t
      (message "Sorry, I don't have message inserter for your MUA.")
      ))
    ))

(defvar tm-mail/use-xemacs-popup-menu running-xemacs)

(if (and running-xemacs tm-mail/use-xemacs-popup-menu)
    (cond 
     (running-xemacs-19_14-or-later
      (setq mail-menubar-menu
	    (append mail-menubar-menu
		    (list "---"
			  mime-editor/popup-menu-for-xemacs)))
      )
     (t
      (setq mail-mode-menu
	    (append mail-mode-menu
		    (list "---"
			  mime-editor/popup-menu-for-xemacs)))
      )))


;;; @ end
;;;

(provide 'tm-mail)

;;; tm-mail.el ends here
