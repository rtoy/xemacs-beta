;;;
;;; tm-gnus5.el --- MIME extender for Gnus 5.2 or later
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         and KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;;; Created: 1995/09/24
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
(require 'tl-list)
(require 'tl-misc)
(require 'tm-view)
(require 'gnus)

(eval-when-compile (require 'cl))


;;; @ version
;;;

(defconst tm-gnus/RCS-ID
  "$Id: tm-gnus5.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $")

(defconst tm-gnus/version
  (concat (get-version-string tm-gnus/RCS-ID) " for Gnus 5.2 or later"))


;;; @ variables
;;;

(defvar tm-gnus/automatic-mime-preview t
  "*If non-nil, show MIME processed article.
This variable is set to `gnus-show-mime'.")

(setq gnus-show-mime tm-gnus/automatic-mime-preview)


;;; @ command functions
;;;

(defun tm-gnus/view-message (arg)
  "MIME decode and play this message."
  (interactive "P")
  (let ((gnus-break-pages nil))
    (gnus-summary-select-article t t)
    )
  (pop-to-buffer gnus-original-article-buffer t)
  (let (buffer-read-only)
    (if (text-property-any (point-min) (point-max) 'invisible t)
	(remove-text-properties (point-min) (point-max)
				gnus-hidden-properties)
      ))
  (mime/viewer-mode nil nil nil gnus-original-article-buffer
		    gnus-article-buffer)
  )

(defun tm-gnus/summary-scroll-down ()
  "Scroll down one line current article."
  (interactive)
  (gnus-summary-scroll-up -1)
  )

(defun tm-gnus/summary-toggle-header (&optional arg)
  (interactive "P")
  (if tm-gnus/automatic-mime-preview
      (let* ((hidden
	      (save-excursion
		(set-buffer gnus-article-buffer)
		(text-property-any 
		 (goto-char (point-min)) (search-forward "\n\n")
		 'invisible t)
		))
	     (mime-viewer/redisplay t)
	     )
	(gnus-summary-select-article hidden t)
	)
    (gnus-summary-toggle-header arg))
  )

(define-key gnus-summary-mode-map "v" (function tm-gnus/view-message))
(define-key gnus-summary-mode-map
  "\e\r" (function tm-gnus/summary-scroll-down))
(substitute-key-definition
 'gnus-summary-toggle-header
 'tm-gnus/summary-toggle-header gnus-summary-mode-map)


;;; @ for tm-view
;;;

(defun tm-gnus/content-header-filter ()
  (goto-char (point-min))
  (mime-preview/cut-header)
  (decode-mime-charset-region (point-min)(point-max) default-mime-charset)
  (mime/decode-message-header)
  )

(set-alist 'mime-viewer/content-header-filter-alist
	   'gnus-original-article-mode
	   (function tm-gnus/content-header-filter))

(set-alist 'mime-viewer/code-converter-alist
	   'gnus-original-article-mode
	   (function mime-charset/decode-buffer))

(defun mime-viewer/quitting-method-for-gnus5 ()
  (if (not gnus-show-mime)
      (mime-viewer/kill-buffer))
  (delete-other-windows)
  (gnus-article-show-summary)
  (if (or (not gnus-show-mime)
	  (null gnus-have-all-headers))
      (gnus-summary-select-article nil t)
    ))

(set-alist 'mime-viewer/quitting-method-alist
	   'gnus-original-article-mode
	   (function mime-viewer/quitting-method-for-gnus5))
(set-alist 'mime-viewer/show-summary-method
	   'gnus-original-article-mode
	   (function mime-viewer/quitting-method-for-gnus5))


;;; @ for tm-edit
;;;

;; suggested by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;	1995/11/08 (c.f. [tm ML:1067])
(defun tm-gnus/insert-article (&optional message)
  (interactive)
  (let ((message-cite-function 'mime-editor/inserted-message-filter)
        (message-reply-buffer gnus-original-article-buffer)
	)
    (message-yank-original nil)
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
	       'message-mode (function tm-gnus/insert-article))
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	(add-hook 'news-setup-hook 'mime-editor/attach-to-news-reply-menu)
      )

    (set-alist 'mime-editor/split-message-sender-alist
	       'message-mode
	       (lambda ()
		 (interactive)
		 (let (message-send-hook
		       message-sent-message-via)
		   (message-send)
		   )))
    )))


;;; @ for tm-partial
;;;

(defun tm-gnus/partial-preview-function ()
  (tm-gnus/view-message (gnus-summary-article-number))
  )

(call-after-loaded
 'tm-partial
 (lambda ()
   (set-atype 'mime/content-decoding-condition
	      '((type . "message/partial")
		(method . mime-article/grab-message/partials)
		(major-mode . gnus-original-article-mode)
		(summary-buffer-exp . gnus-summary-buffer)
		))
   (set-alist 'tm-partial/preview-article-method-alist
	      'gnus-original-article-mode
	      'tm-gnus/partial-preview-function)
   ))


;;; @ article filter
;;;

(defun tm-gnus/article-reset-variable ()
  (setq tm-gnus/automatic-mime-preview nil)
  )

(add-hook 'gnus-article-prepare-hook 'tm-gnus/article-reset-variable)

(defun tm-gnus/preview-article ()
  (make-local-variable 'tm:mother-button-dispatcher)
  (setq tm:mother-button-dispatcher
	(function gnus-article-push-button))
  (let ((mime-viewer/ignored-field-regexp "^:$")
	(default-mime-charset
	  (save-excursion
	    (set-buffer gnus-summary-buffer)
	    default-mime-charset))
	)
    (mime/viewer-mode nil nil nil gnus-original-article-buffer
		      gnus-article-buffer
		      gnus-article-mode-map)
    )
  (setq tm-gnus/automatic-mime-preview t)
  (run-hooks 'tm-gnus/article-prepare-hook)
  )

(setq gnus-show-mime-method (function tm-gnus/preview-article))

(defun tm-gnus/article-decode-encoded-word ()
  (decode-mime-charset-region (point-min)(point-max)
			      (save-excursion
				(set-buffer gnus-summary-buffer)
				default-mime-charset))
  (mime/decode-message-header)
  (run-hooks 'tm-gnus/article-prepare-hook)
  )

(setq gnus-decode-encoded-word-method
      (function tm-gnus/article-decode-encoded-word))


;;; @ for mule (Multilingual support)
;;;

(defvar gnus-newsgroup-default-charset-alist nil)

(defun gnus-set-newsgroup-default-charset (newsgroup charset)
  "Set CHARSET for the NEWSGROUP as default MIME charset."
  (set-alist 'gnus-newsgroup-default-charset-alist
	     (concat "^" (regexp-quote newsgroup) "\\($\\|\\.\\)")
	     charset))

(cond
 ((featurep 'mule)
  (cond ((boundp 'MULE) ; for MULE 1.* and 2.*.
	 (define-service-coding-system gnus-nntp-service nil *noconv*)
	 (if (and (boundp 'nntp-server-process)
		  (processp nntp-server-process)
		  )
	     (set-process-coding-system nntp-server-process *noconv* *noconv*)
	   )
	 )
	(running-xemacs-20 ; for XEmacs/mule.
	 (if (and (boundp 'nntp-server-process)
		  (processp nntp-server-process)
		  )
	     (set-process-input-coding-system nntp-server-process 'noconv)
	   )
	 ))
  (call-after-loaded
   'nnheader
   (lambda ()
     (defun nnheader-find-file-noselect (filename &optional nowarn rawfile)
       (let ((file-coding-system-for-read *noconv*))
	 (find-file-noselect filename nowarn rawfile)
	 ))
     (defun nnheader-insert-file-contents-literally
       (filename &optional visit beg end replace)
       (let ((file-coding-system-for-read *noconv*))
	 (insert-file-contents-literally filename visit beg end replace)
	 ))
     ))
  ;; Please use Gnus 5.2.10 or later if you use Mule.
  (call-after-loaded
   'nnmail
   (lambda ()
     (defun nnmail-find-file (file)
       "Insert FILE in server buffer safely. [tm-gnus5.el]"
       (set-buffer nntp-server-buffer)
       (erase-buffer)
       (let ((format-alist nil)
             (after-insert-file-functions   ; for jam-code-guess
              (if (memq 'jam-code-guess-after-insert-file-function
                        after-insert-file-functions)
                  '(jam-code-guess-after-insert-file-function)))
	     (file-coding-system-for-read *noconv*))
	 (condition-case ()
	     (progn (insert-file-contents file) t)
	   (file-error nil))))
     ))
  (defun tm-gnus/prepare-save-mail-function ()
    (setq file-coding-system *noconv*)
    )
  (add-hook 'nnmail-prepare-save-mail-hook
	    'tm-gnus/prepare-save-mail-function)
  
  (gnus-set-newsgroup-default-charset "alt.chinese"		'hz)
  (gnus-set-newsgroup-default-charset "alt.chinese.text.big5"	'big5)
  (gnus-set-newsgroup-default-charset "tw"			'big5)
  (gnus-set-newsgroup-default-charset "hk"			'big5)
  (gnus-set-newsgroup-default-charset "hkstar"			'big5)
  (gnus-set-newsgroup-default-charset "han"    			'euc-kr)
  (gnus-set-newsgroup-default-charset "relcom"			'koi8-r)
  ))


;;; @ summary filter
;;;

(defun tm-gnus/decode-summary-from-and-subjects ()
  (let ((rest gnus-newsgroup-default-charset-alist)
	cell)
    (catch 'tag
      (while (setq cell (car rest))
	(if (string-match (car cell) gnus-newsgroup-name)
	    (throw 'tag
		   (progn
		     (make-local-variable 'default-mime-charset)
		     (setq default-mime-charset (cdr cell))
		     )))
	(setq rest (cdr rest))
	)))
  (mapcar
   (lambda (header)
     (let ((from (or (mail-header-from header) ""))
	   (subj (or (mail-header-subject header) ""))
	   (method (car gnus-current-select-method))
	   )
       (if (eq method 'nntp)
	   (progn
	     (setq from
		   (decode-mime-charset-string from default-mime-charset))
	     (setq subj
		   (decode-mime-charset-string subj default-mime-charset))
	     ))
       (mail-header-set-from
	header (mime-eword/decode-string from))
       (mail-header-set-subject
	header (mime-eword/decode-string subj))
       ))
   gnus-newsgroup-headers))

(or (boundp 'nnheader-encoded-words-decoding)
    (add-hook 'gnus-select-group-hook
	      'tm-gnus/decode-summary-from-and-subjects)
    )


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (lambda ()
   (require 'tm-bbdb)
   ))

(autoload 'tm-bbdb/update-record "tm-bbdb")

(defun tm-gnus/bbdb-setup ()
  (if (memq 'bbdb/gnus-update-record gnus-article-prepare-hook)
      (progn
	(remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-update-record)
	(add-hook 'gnus-article-display-hook 'tm-bbdb/update-record)
	)))

(add-hook 'gnus-startup-hook 'tm-gnus/bbdb-setup t)

(tm-gnus/bbdb-setup)


;;; @ end
;;;

(provide 'tm-gnus5)

;;; tm-gnus5.el ends here
