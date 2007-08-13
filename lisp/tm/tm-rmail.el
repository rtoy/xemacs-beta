;;; tm-rmail.el --- MIME extension for RMAIL

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; modified by KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;; Created: 1994/8/30
;; Version: $Revision: 1.1.1.2 $
;; Keywords: mail, MIME, multimedia, multilingual, encoded-word

;; This file is not part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-list)
(require 'tl-misc)
(require 'rmail)

(autoload 'mime/viewer-mode "tm-view" "View MIME message." t)
(autoload 'mime/Content-Type "tm-view" "parse Content-Type field.")
(autoload 'mime/decode-message-header "tm-ew-d" "Decode MIME encoded-word." t)


;;; @ variables
;;;

(defconst tm-rmail/RCS-ID
  "$Id: tm-rmail.el,v 1.1.1.2 1996/12/21 20:50:48 steve Exp $")
(defconst tm-rmail/version (get-version-string tm-rmail/RCS-ID))

(defvar tm-rmail/decode-all nil)


;;; @ message filter
;;;

(setq rmail-message-filter
      (function
       (lambda ()
	 (let ((mf (buffer-modified-p))
	       (buffer-read-only nil))
	   (mime/decode-message-header)
	   (set-buffer-modified-p mf)
	   ))))


;;; @ MIME preview
;;;

(defun tm-rmail/show-all-header-p ()
  (save-restriction
    (narrow-to-region (point-min)
		      (and (re-search-forward "^$" nil t)
			   (match-beginning 0)))
    (goto-char (point-min))
    (re-search-forward rmail-ignored-headers nil t)
    ))

(defun tm-rmail/preview-message ()
  (interactive)
  (setq tm-rmail/decode-all t)
  (let ((ret (tm-rmail/get-Content-Type-and-Content-Transfer-Encoding)))
    (narrow-to-region (point-min)
		      (save-excursion
			(goto-char (point-max))
			(if (and (re-search-backward "^\n")
				 (eq (match-end 0)(point-max)))
			    (match-beginning 0)
			  (point-max)
			  )))
    (let ((abuf (current-buffer))
	  (buf-name (format "*Preview-%s [%d/%d]*"
			    (buffer-name)
			    rmail-current-message rmail-total-messages))
	  buf win)
      (if (and mime::article/preview-buffer
		 (setq buf (get-buffer mime::article/preview-buffer))
		 )
	  (progn
	    (save-excursion
	      (set-buffer buf)
	      (rename-buffer buf-name)
	      )
	    (if (setq win (get-buffer-window buf))
		(progn
		  (delete-window (get-buffer-window abuf))
		  (set-window-buffer win abuf)
		  (set-buffer abuf)
		  ))
	    ))
      (setq win (get-buffer-window abuf))
      (save-window-excursion
	(mime/viewer-mode nil (car ret)(cdr ret) nil buf-name)
	(or buf
	    (setq buf (current-buffer))
	    )
	)
      (set-window-buffer win buf)
      )))

(defun tm-rmail/preview-message-if-you-need ()
  (if tm-rmail/decode-all
      (tm-rmail/preview-message)
    ))

(add-hook 'rmail-show-message-hook 'tm-rmail/preview-message-if-you-need)

(load "rmailsum")

(cond ((fboundp 'rmail-summary-rmail-update)
       ;; for Emacs 19 or later
       (or (fboundp 'tm:rmail-summary-rmail-update)
	   (fset 'tm:rmail-summary-rmail-update
		 (symbol-function 'rmail-summary-rmail-update))
	   )
       
       (defun rmail-summary-rmail-update ()
	 (tm:rmail-summary-rmail-update)
	 (if tm-rmail/decode-all
	     (let ((win (get-buffer-window rmail-buffer)))
	       (if win
		   (delete-window win)
		 )))
	 )
       
       (defun tm-rmail/get-Content-Type-and-Content-Transfer-Encoding ()
	 (rmail-widen-to-current-msgbeg
	  (function
	   (lambda ()
	     (cons (mime/Content-Type)
		   (mime/Content-Transfer-Encoding "7bit")
		   )))))
       )
      (t
       ;; for Emacs 18
       (defun tm-rmail/get-Content-Type-and-Content-Transfer-Encoding ()
	 (save-restriction
	   (rmail-widen-to-current-msgbeg
	    (function
	     (lambda ()
	       (goto-char (point-min))
	       (narrow-to-region (or (and (re-search-forward "^.+:" nil t)
					  (match-beginning 0))
				     (point-min))
				 (point-max))
	       )))
	   (cons (mime/Content-Type)
		 (mime/Content-Transfer-Encoding "7bit")
		 )))
       ))

(define-key rmail-mode-map "v" (function tm-rmail/preview-message))

(defun tm-rmail/setup ()
  (local-set-key "v" (function
		      (lambda ()
			(interactive)
			(set-buffer rmail-buffer)
			(tm-rmail/preview-message)
			)))
  )

(add-hook 'rmail-summary-mode-hook 'tm-rmail/setup)


;;; @ over-to-* and quitting methods
;;;

(defun tm-rmail/quitting-method-to-summary ()
  (mime-viewer/kill-buffer)
  (rmail-summary)
  (delete-other-windows)
  )

(defun tm-rmail/quitting-method-to-article ()
  (setq tm-rmail/decode-all nil)
  (mime-viewer/kill-buffer)
  )

(defalias 'tm-rmail/quitting-method 'tm-rmail/quitting-method-to-article)


(defun tm-rmail/over-to-previous-method ()
  (let (tm-rmail/decode-all)
    (mime-viewer/quit)
    )
  (if (not (eq (rmail-next-undeleted-message -1) t))
      (tm-rmail/preview-message)
    )
  )

(defun tm-rmail/over-to-next-method ()
  (let (tm-rmail/decode-all)
    (mime-viewer/quit)
    )
  (if (not (eq (rmail-next-undeleted-message 1) t))
      (tm-rmail/preview-message)
    )
  )

(defun tm-rmail/show-summary-method ()
  (save-excursion
    (set-buffer mime::preview/article-buffer)
    (rmail-summary)
    ))

(call-after-loaded
 'tm-view
 (function
  (lambda ()
    (set-alist 'mime-viewer/quitting-method-alist
	       'rmail-mode
	       (function tm-rmail/quitting-method))
    
    (set-alist 'mime-viewer/over-to-previous-method-alist
	       'rmail-mode
	       (function tm-rmail/over-to-previous-method))
    
    (set-alist 'mime-viewer/over-to-next-method-alist
	       'rmail-mode
	       (function tm-rmail/over-to-next-method))

    (set-alist 'mime-viewer/show-summary-method
	       'rmail-mode
	       (function tm-rmail/show-summary-method))
    )))


;;; @ for tm-partial
;;;

(call-after-loaded
 'tm-partial
 (function
  (lambda ()
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/partial")
		 (method . mime-article/grab-message/partials)
		 (major-mode . rmail-mode)
		 (summary-buffer-exp
		  . (progn
		      (rmail-summary)
		      (pop-to-buffer rmail-buffer)
		      rmail-summary-buffer))
		 ))
    (set-alist 'tm-partial/preview-article-method-alist
	       'rmail-mode
	       (function
		(lambda ()
		  (rmail-summary-goto-msg (count-lines 1 (point)))
		  (pop-to-buffer rmail-buffer)
		  (tm-rmail/preview-message)
		  )))
    )))


;;; @ for tm-edit
;;;

(defun tm-rmail/forward ()
  "Forward current message in message/rfc822 content-type message
from rmail. The message will be appended if being composed."
  (interactive)
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "forwarded" t)
  (let ((initialized nil)
	(beginning nil)
	(msgnum rmail-current-message)
	(rmail-buffer (current-buffer))
	(subject (concat "["
			 (mail-strip-quoted-names
			  (mail-fetch-field "From"))
			 ": " (or (mail-fetch-field "Subject") "") "]")))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (setq initialized
	  (if (one-window-p t)
	      (mail nil nil subject)
	    (mail-other-window nil nil subject)))
    (save-excursion
      ;; following two variables are used in 19.29 or later.
      (make-local-variable 'rmail-send-actions-rmail-buffer)
      (make-local-variable 'rmail-send-actions-rmail-msg-number)
      (make-local-variable 'mail-reply-buffer)
      (setq rmail-send-actions-rmail-buffer rmail-buffer)
      (setq rmail-send-actions-rmail-msg-number msgnum)
      (setq mail-reply-buffer rmail-buffer)
      (goto-char (point-max))
      (forward-line 1)
      (setq beginning (point))
      (mime-editor/insert-tag "message" "rfc822")
;;       (insert-buffer rmail-buffer))
;;       (mime-editor/inserted-message-filter))
      (tm-mail/insert-message))
    (if (not initialized)
	(goto-char beginning))
    ))

(defun gnus-mail-forward-using-mail-mime ()
  "Forward current article in message/rfc822 content-type message from
GNUS. The message will be appended if being composed."
  (let ((initialized nil)
	(beginning nil)
	(forwarding-buffer (current-buffer))
	(subject
	 (concat "[" gnus-newsgroup-name "] "
		 ;;(mail-strip-quoted-names (gnus-fetch-field "From")) ": "
		 (or (gnus-fetch-field "Subject") ""))))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (setq initialized
	  (if (one-window-p t)
	      (mail nil nil subject)
	    (mail-other-window nil nil subject)))
    (save-excursion
      (goto-char (point-max))
      (setq beginning (point))
      (mime-editor/insert-tag "message" "rfc822")
      (insert-buffer forwarding-buffer)
      ;; You have a chance to arrange the message.
      (run-hooks 'gnus-mail-forward-hook)
      )
    (if (not initialized)
	(goto-char beginning))
    ))

(call-after-loaded
 'mime-setup
 (function
  (lambda ()
    (substitute-key-definition
     'rmail-forward 'tm-rmail/forward rmail-mode-map)
    
    ;; (setq gnus-mail-forward-method 'gnus-mail-forward-using-mail-mime)
    
    (call-after-loaded
     'tm-edit
     (function
      (lambda ()
	(require 'tm-mail)
	(set-alist 'mime-editor/message-inserter-alist
		   'mail-mode (function tm-mail/insert-message))
	(set-alist 'mime-editor/split-message-sender-alist
		   'mail-mode (function
			       (lambda ()
				 (interactive)
				 (funcall send-mail-function)
				 )))
	)))
    )))


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'tm-bbdb)
    )))


;;; @ end
;;;

(provide 'tm-rmail)

(run-hooks 'tm-rmail-load-hook)

;;; tm-rmail.el ends here.
