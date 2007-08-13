;;;
;;; tm-gnus4.el --- tm-gnus module for GNUS 4, 5.0.* and 5.1.*.
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; modified by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         and KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;;; Created: 1993/11/20 (merged tm-gnus5.el)
;;; Version: $Revision: 1.1.1.1 $
;;; Keywords: news, MIME, multimedia, multilingual, encoded-word
;;;
;;; This file is part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'tl-str)
(require 'tl-misc)


;;; @ version
;;;

(defconst tm-gnus/RCS-ID
  "$Id: tm-gnus4.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $")

(defconst tm-gnus/version
  (concat (get-version-string tm-gnus/RCS-ID) " for 3.15 .. 5.1.*"))


;;; @ variable
;;;

(defvar tm-gnus/automatic-mime-preview t
  "*If non-nil, show MIME processed article.
This variable is set to `gnus-show-mime'.")

(defvar tm-gnus/original-article-buffer " *Original Article*")
(defvar gnus-original-article-buffer nil)


;;; @ for tm-view
;;;

(autoload 'mime/viewer-mode "tm-view" "View MIME message." t)

(defun tm-gnus/view-message (arg)
  "MIME decode and play this message."
  (interactive "P")
  (let ((gnus-break-pages nil)
	(gnus-show-mime nil))
    (gnus-summary-select-article t t)
    )
  (pop-to-buffer gnus-article-buffer t)
  (let ((str (buffer-string))
	(obuf (get-buffer tm-gnus/original-article-buffer))
	(pbuf (current-buffer))
	)
    (if obuf
	(progn
	  (set-buffer obuf)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  )
      (setq obuf (get-buffer-create tm-gnus/original-article-buffer))
      (set-buffer obuf)
      )
    (insert str)
    (gnus-article-mode)
    (set-buffer pbuf)
    (make-local-variable 'tm:mother-button-dispatcher)
    (setq tm:mother-button-dispatcher
	  (function gnus-article-push-button))
    (mime/viewer-mode
     nil nil nil tm-gnus/original-article-buffer gnus-article-buffer)
    (let (buffer-read-only)
      (run-hooks 'tm-gnus/article-prepare-hook)
      )
    ))

(defun tm-gnus/summary-scroll-down ()
  "Scroll down one line current article."
  (interactive)
  (gnus-summary-scroll-up -1)
  )

(defun mime-viewer/quitting-method-for-gnus4 ()
  (if (not gnus-show-mime)
      (mime-viewer/kill-buffer)
    )
  (delete-other-windows)
  (gnus-article-show-summary)
  (if (or (not gnus-show-mime)
	  (null gnus-have-all-headers))
      (gnus-summary-select-article nil t)
    ))

(call-after-loaded
 'tm-view
 (function
  (lambda ()
    (set-alist 'mime-viewer/quitting-method-alist
	       'gnus-article-mode
	       (function mime-viewer/quitting-method-for-gnus4))
    (set-alist 'mime-viewer/show-summary-method
	       'gnus-article-mode
	       (function mime-viewer/quitting-method-for-gnus4))
    )))


;;; @ for tm-edit
;;;

;; suggested by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;	1995/11/08 (c.f. [tm ML:1067])
(defun tm-gnus/insert-article (&optional message)
  (interactive)
  (let (;; for Emacs 19
	(mail-citation-hook '(mime-editor/inserted-message-filter))
	news-reply-header-hook
	mail-yank-hooks
	
	;; for Emacs 18
	(mail-yank-ignored-headers mime-editor/yank-ignored-field-regexp)
	(news-make-reply-yank-header (function
				      (lambda (message-id from) "")
				      ))
	(news-yank-original-quoting-indicator "")
	
	;; select raw article buffer
	(mail-reply-buffer
	 (save-excursion
	   (set-buffer gnus-article-buffer)
	   (if (eq major-mode 'mime/viewer-mode)
	       mime::preview/article-buffer
	     gnus-article-buffer)))
	)
    (news-reply-yank-original 0)
    ))

;;; modified by Steven L. Baur <steve@miranova.com>
;;;	1995/12/6 (c.f. [tm-en:209])
(defun mime-editor/attach-to-news-reply-menu ()
  "Arrange to attach MIME editor's popup menu to VM's"
  (if (boundp 'news-reply-menu)
      (progn
	(setq news-reply-menu (append news-reply-menu
				      '("---")
				      mime-editor/popup-menu-for-xemacs))
	(remove-hook 'news-setup-hook
		     'mime-editor/attach-to-news-reply-menu)
	)))

(call-after-loaded
 'tm-edit
 (function
  (lambda ()
    (set-alist 'mime-editor/message-inserter-alist
	       'news-reply-mode (function tm-gnus/insert-article))
    
    (autoload 'tm-mail/insert-message "tm-mail")
    (set-alist 'mime-editor/message-inserter-alist
	       'mail-mode (function tm-mail/insert-message))
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	(add-hook 'news-setup-hook 'mime-editor/attach-to-news-reply-menu)
      )
    
    (set-alist 'mime-editor/split-message-sender-alist
	       'news-reply-mode
	       (function gnus-inews-news))
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
		 (major-mode . gnus-article-mode)
		 (summary-buffer-exp . gnus-summary-buffer)
		 ))
    
    (set-alist 'tm-partial/preview-article-method-alist
	       'gnus-article-mode
	       (function
		(lambda ()
		  (tm-gnus/view-message (gnus-summary-article-number))
		  )))
    )))


;;; @ set up
;;;

(define-key gnus-summary-mode-map "v" (function tm-gnus/view-message))
(define-key gnus-summary-mode-map
  "\e\r" (function tm-gnus/summary-scroll-down))

(defun tm-gnus/article-reset-variable ()
  (setq gnus-original-article-buffer nil)
  (setq tm-gnus/automatic-mime-preview nil)
  (gnus-article-mode)
  (setq buffer-read-only nil)
  )

(add-hook 'gnus-article-prepare-hook 'tm-gnus/article-reset-variable)

(defun tm-gnus/decode-encoded-word-if-you-need ()
  (if (not gnus-have-all-headers)
      (progn
	(mime/decode-message-header)
	(run-hooks 'tm-gnus/article-prepare-hook)
	)))

(defun tm-gnus/preview-article-if-you-need ()
  (if (not gnus-have-all-headers)
      (let ((str (buffer-string))
	    (obuf (get-buffer tm-gnus/original-article-buffer))
	    (pbuf (current-buffer))
	    )
	(if obuf
	    (progn
	      (set-buffer obuf)
	      (setq buffer-read-only nil)
	      (erase-buffer)
	      )
	  (setq obuf (get-buffer-create tm-gnus/original-article-buffer))
	  (set-buffer obuf)
	  )
	(insert str)
	(gnus-article-mode)
	(set-buffer pbuf)
	(make-local-variable 'tm:mother-button-dispatcher)
	(setq tm:mother-button-dispatcher
	      (function gnus-article-push-button))
	(save-window-excursion
	  (mime/viewer-mode
	   nil nil nil tm-gnus/original-article-buffer gnus-article-buffer)
	  )
	(setq tm-gnus/automatic-mime-preview t)
	(setq gnus-original-article-buffer tm-gnus/original-article-buffer)
	(let (buffer-read-only)
	  (run-hooks 'tm-gnus/article-prepare-hook)
	  )
	(if (featurep 'tm-gd3)
	    (setq buffer-read-only nil)
	  )
	)))

(setq gnus-show-mime-method
      (if tm-gnus/automatic-mime-preview
	  (function tm-gnus/preview-article-if-you-need)
	(function tm-gnus/decode-encoded-word-if-you-need)
	))

(setq gnus-show-mime t)


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'tm-bbdb)
    )))

(autoload 'tm-bbdb/update-record "tm-bbdb")

(defun tm-gnus/bbdb-setup ()
  (if (memq 'bbdb/gnus-update-record gnus-article-prepare-hook)
      (progn
	(remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-update-record)
	;;(add-hook 'tm-gnus/article-prepare-hook 'bbdb/gnus-update-record)
	(add-hook 'gnus-article-display-hook 'tm-bbdb/update-record)
	)))

(add-hook 'gnus-startup-hook 'tm-gnus/bbdb-setup t)

(tm-gnus/bbdb-setup)


;;; @ end
;;;

(provide 'tm-gnus4)

;;; tm-gnus4.el ends here
