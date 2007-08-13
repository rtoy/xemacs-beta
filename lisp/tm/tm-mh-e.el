;;; tm-mh-e.el --- MIME extension for mh-e

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1993/11/21 (obsolete mh-e-mime.el)
;; Version: $Revision: 1.3 $
;; Keywords: mail, MH, MIME, multimedia, encoded-word, multilingual

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-str)
(require 'tl-misc)
(require 'mh-e)
(or (featurep 'mh-utils)
    (require 'tm-mh-e3)
    )
(require 'tm-view)

(or (fboundp 'mh-get-header-field)
    (defalias 'mh-get-header-field 'mh-get-field)
    )
(or (boundp 'mh-temp-buffer)
    (defconst mh-temp-buffer " *mh-temp*")
    )


;;; @ version
;;;

(defconst tm-mh-e/RCS-ID
  "$Id: tm-mh-e.el,v 1.3 1997/09/27 16:57:47 steve Exp $")

(defconst tm-mh-e/version (get-version-string tm-mh-e/RCS-ID))


;;; @ variable
;;;

(defvar tm-mh-e/automatic-mime-preview t
  "*If non-nil, show MIME processed message.")

(defvar tm-mh-e/decode-encoded-word t
  "*If non-nil, decode encoded-word when it is not MIME preview mode.")


;;; @ functions
;;;

(defun mh-display-msg (msg-num folder &optional show-buffer mode)
  (or mode
      (setq mode tm-mh-e/automatic-mime-preview)
      )
  ;; Display message NUMBER of FOLDER.
  ;; Sets the current buffer to the show buffer.
  (set-buffer folder)
  (or show-buffer
      (setq show-buffer mh-show-buffer))
  ;; Bind variables in folder buffer in case they are local
  (let ((formfile mhl-formfile)
	(clean-message-header mh-clean-message-header)
	(invisible-headers mh-invisible-headers)
	(visible-headers mh-visible-headers)
	(msg-filename (mh-msg-filename msg-num))
	)
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist" msg-num))
    (set-buffer show-buffer)
    (cond ((not (equal msg-filename buffer-file-name))
	   ;; Buffer does not yet contain message.
	   (mh-unvisit-file)
	   (setq buffer-read-only nil)
	   (erase-buffer)
	   ;; Changing contents, so this hook needs to be reinitialized.
	   ;; pgp.el uses this.
	   (if (boundp 'write-contents-hooks) ;Emacs 19
	       (setq write-contents-hooks nil))
	   (if mode
	       (let* ((aname (concat "article-" folder))
		      (abuf (get-buffer aname))
		      )
		 (if abuf
		     (progn
		       (set-buffer abuf)
		       (setq buffer-read-only nil)
		       (erase-buffer)
		       )
		   (setq abuf (get-buffer-create aname))
		   (set-buffer abuf)
		   )
		 (as-binary-input-file
		  (insert-file-contents msg-filename)
		  ;; (goto-char (point-min))
		  (while (re-search-forward "\r$" nil t)
		    (replace-match "")
		    )
		  )
		 (set-buffer-modified-p nil)
		 (setq buffer-read-only t)
		 (setq buffer-file-name msg-filename)
		 (mh-show-mode)
		 (mime/viewer-mode nil nil nil
				   aname (concat "show-" folder))
		 (goto-char (point-min))
		 (let ( (buffer-read-only nil) )
		   (cond (clean-message-header
			  (mh-clean-msg-header (point-min)
					       invisible-headers
					     visible-headers)
			  (goto-char (point-min)))
			 (t
			  (mh-start-of-uncleaned-message))))
		 (goto-char (point-min))
		 )
	     (progn
	       (if formfile
		   (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
					   (if (stringp formfile)
					       (list "-form" formfile))
					   msg-filename)
		 (insert-file-contents msg-filename))
	       ;; end
	       (goto-char (point-min))
	       (cond (clean-message-header
		      (mh-clean-msg-header (point-min)
					   invisible-headers
					   visible-headers)
		      (goto-char (point-min)))
		     (t
		      (mh-start-of-uncleaned-message)))
	       (if tm-mh-e/decode-encoded-word
		   (mime/decode-message-header)
		 )
	       (set-buffer-modified-p nil)
	       (setq buffer-read-only t)
	       (setq buffer-file-name msg-filename)
	       (mh-show-mode)
	       ))
	   (set-buffer-modified-p nil)
	   (or (eq buffer-undo-list t)	;don't save undo info for prev msgs
	       (setq buffer-undo-list nil))
	   (set-buffer-auto-saved)
	   ;; the parts of set-visited-file-name we want to do (no locking)
	   (setq buffer-file-name msg-filename)
	   (setq buffer-backed-up nil)
	   (auto-save-mode 1)
	   (set-mark nil)
	   (setq mode-line-buffer-identification
		 (list (format mh-show-buffer-mode-line-buffer-id
			       folder msg-num)))
	   (set-buffer folder)
	   (setq mh-showing-with-headers nil)))))

(defun tm-mh-e/view-message (&optional msg)
  "MIME decode and play this message."
  (interactive)
  (if (or (null tm-mh-e/automatic-mime-preview)
	  (null (get-buffer mh-show-buffer))
	  (save-excursion
	    (set-buffer mh-show-buffer)
	    (not (eq major-mode 'mime/viewer-mode))
	    ))
      (let ((tm-mh-e/automatic-mime-preview t))
	(mh-invalidate-show-buffer)
	(mh-show-msg msg)
	))
  (pop-to-buffer mh-show-buffer)
  )

(defun tm-mh-e/toggle-decoding-mode (arg)
  "Toggle MIME processing mode.
With arg, turn MIME processing on if arg is positive."
  (interactive "P")
  (setq tm-mh-e/automatic-mime-preview
	(if (null arg)
	    (not tm-mh-e/automatic-mime-preview)
	  arg))
  (save-excursion
    (set-buffer mh-show-buffer)
    (if (null tm-mh-e/automatic-mime-preview)
	(if (and mime::preview/article-buffer
		 (get-buffer mime::preview/article-buffer))
	    (kill-buffer mime::preview/article-buffer)
	  )))
  (mh-invalidate-show-buffer)
  (mh-show (mh-get-msg-num t))
  )

(defun tm-mh-e/show (&optional message)
  (interactive)
  (mh-invalidate-show-buffer)
  (mh-show message)
  )

(defun tm-mh-e/header-display ()
  (interactive)
  (mh-invalidate-show-buffer)
  (let ((mime-viewer/ignored-field-regexp "^:$")
	tm-mh-e/decode-encoded-word)
    (mh-header-display)
    ))

(defun tm-mh-e/raw-display ()
  (interactive)
  (mh-invalidate-show-buffer)
  (let (tm-mh-e/automatic-mime-preview
	tm-mh-e/decode-encoded-word)
    (mh-header-display)
    ))

(defun tm-mh-e/burst-multipart/digest ()
  "Burst apart the current message, which should be a multipart/digest.
The message is replaced by its table of contents and the letters from the
digest are inserted into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (mh-set-folder-modified-p t)		; lock folder while bursting
    (message "Bursting digest...")
    (mh-exec-cmd "mhn" "-store" mh-current-folder digest)
    (mh-scan-folder mh-current-folder (format "%d-last" mh-first-msg-num))
    (message "Bursting digest...done")
    ))


;;; @ for tm-view
;;;

(fset 'tm-mh-e/decode-charset-buffer
      (symbol-function 'mime-charset/decode-buffer))

(set-alist 'mime-viewer/code-converter-alist
	   'mh-show-mode
	   (function tm-mh-e/decode-charset-buffer))

(defun tm-mh-e/content-header-filter ()
  (goto-char (point-min))
  (mime-preview/cut-header)
  (tm-mh-e/decode-charset-buffer default-mime-charset)
  (mime/decode-message-header)
  )

(set-alist 'mime-viewer/content-header-filter-alist
	   'mh-show-mode
	   (function tm-mh-e/content-header-filter))

(defun tm-mh-e/quitting-method ()
  (let ((win (get-buffer-window
	      mime/output-buffer-name))
	(buf (current-buffer))
	)
    (if win
	(delete-window win)
      )
    (pop-to-buffer
     (let ((name (buffer-name buf)))
       (substring name 5)
       ))
    (if (not tm-mh-e/automatic-mime-preview)
	(mh-invalidate-show-buffer)
      )
    (mh-show (mh-get-msg-num t))
    ))

(set-alist 'mime-viewer/quitting-method-alist
	   'mh-show-mode
	   (function tm-mh-e/quitting-method))
(set-alist 'mime-viewer/show-summary-method
	   'mh-show-mode
	   (function tm-mh-e/quitting-method))

(defun tm-mh-e/following-method (buf)
  (save-excursion
    (set-buffer buf)
    (goto-char (point-max))
    (setq mh-show-buffer buf)
    (apply (function mh-send)
	   (std11-field-bodies '("From" "cc" "Subject") ""))
    (setq mh-sent-from-folder buf)
    (setq mh-sent-from-msg 1)
    (let ((last (point)))
      (mh-yank-cur-msg)
      (goto-char last)
      )))

(set-alist 'mime-viewer/following-method-alist
	   'mh-show-mode
	   (function tm-mh-e/following-method))


;;; @@ for tm-partial
;;;

(call-after-loaded
 'tm-partial
 (function
  (lambda ()
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/partial")
		 (method . mime-article/grab-message/partials)
		 (major-mode . mh-show-mode)
		 (summary-buffer-exp
		  . (and (or (string-match "^article-\\(.+\\)$" article-buffer)
			     (string-match "^show-\\(.+\\)$" article-buffer))
			 (substring article-buffer
				    (match-beginning 1) (match-end 1))
			 ))
		 ))
    (set-alist 'tm-partial/preview-article-method-alist
	       'mh-show-mode
	       (function
		(lambda ()
		  (let ((tm-mh-e/automatic-mime-preview t))
		    (tm-mh-e/show)
		    ))))
    )))


;;; @ set up
;;;

(define-key mh-folder-mode-map "v" (function tm-mh-e/view-message))
(define-key mh-folder-mode-map "\et" (function tm-mh-e/toggle-decoding-mode))
(define-key mh-folder-mode-map "." (function tm-mh-e/show))
(define-key mh-folder-mode-map "," (function tm-mh-e/header-display))
(define-key mh-folder-mode-map "\e," (function tm-mh-e/raw-display))
(define-key mh-folder-mode-map "\C-c\C-b"
  (function tm-mh-e/burst-multipart/digest))

(defun tm-mh-e/summary-before-quit ()
  (let ((buf (get-buffer mh-show-buffer)))
    (if buf
	(let ((the-buf (current-buffer)))
	  (switch-to-buffer buf)
	  (if (and mime::article/preview-buffer
		   (setq buf (get-buffer mime::article/preview-buffer))
		   )
	      (progn
		(switch-to-buffer the-buf)
		(kill-buffer buf)
		)
	    (switch-to-buffer the-buf)
	    )
	  ))))

(add-hook 'mh-before-quit-hook (function tm-mh-e/summary-before-quit))
	     

;;; @@ for tmh-comp.el
;;;

(autoload 'tm-mh-e/edit-again "tmh-comp"
  "Clean-up a draft or a message previously sent and make it resendable." t)
(autoload 'tm-mh-e/extract-rejected-mail "tmh-comp"
  "Extract a letter returned by the mail system and make it re-editable." t)
(autoload 'tm-mh-e/forward "tmh-comp"
  "Forward a message or message sequence by MIME style." t)

(call-after-loaded
 'mime-setup
 (function
  (lambda ()
    (substitute-key-definition
     'mh-edit-again 'tm-mh-e/edit-again mh-folder-mode-map)
    (substitute-key-definition
     'mh-extract-rejected-mail 'tm-mh-e/extract-rejected-mail
     mh-folder-mode-map)
    (substitute-key-definition
     'mh-forward 'tm-mh-e/forward mh-folder-mode-map)

    (call-after-loaded
     'mh-comp
     (function
      (lambda ()
	(require 'tmh-comp)
	))
     'mh-letter-mode-hook)
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

(provide 'tm-mh-e)

(run-hooks 'tm-mh-e-load-hook)

;;; tm-mh-e.el ends here
