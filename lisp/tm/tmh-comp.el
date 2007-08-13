;;; tm-mh-e.el --- tm-mh-e functions for composing messages

;; Copyright (C) 1993,1994,1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/2/29 (separated from tm-mh-e.el)
;; Version: $Id: tmh-comp.el,v 1.2 1996/12/28 21:03:16 steve Exp $
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

(require 'mh-comp)
(require 'tm-edit)


;;; @ variable
;;;

(defvar tm-mh-e/forwcomps "forwcomps"
  "Name of file to be used as a skeleton for forwarding messages.
Default is \"forwcomps\".  If not a complete path name, the file
is searched for first in the user's MH directory, then in the
system MH lib directory.")

(defvar tm-mh-e/message-yank-function 'mh-yank-cur-msg)


;;; @ for tm-edit
;;;

(defun tm-mh-e::make-message (folder number)
  (vector folder number)
  )

(defun tm-mh-e::message/folder (message)
  (elt message 0)
  )

(defun tm-mh-e::message/number (message)
  (elt message 1)
  )

(defun tm-mh-e::message/file-name (message)
  (expand-file-name
   (tm-mh-e::message/number message)
   (mh-expand-file-name (tm-mh-e::message/folder message))
   ))

;;; modified by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;	1995/11/14 (cf. [tm-ja:1096])
(defun tm-mh-e/prompt-for-message (prompt folder &optional default)
  (let* ((files
	  (directory-files (mh-expand-file-name folder) nil "^[0-9]+$")
	  )
	 (folder-buf (get-buffer folder))
	 (default
	   (if folder-buf
	       (save-excursion
		 (set-buffer folder-buf)
 		 (let* ((show-buffer (get-buffer mh-show-buffer))
 			(show-buffer-file-name
 			 (buffer-file-name show-buffer)))
 		   (if show-buffer-file-name
 		       (file-name-nondirectory show-buffer-file-name)))))))
    (if (or (null default)
	    (not (string-match "^[0-9]+$" default)))
	(setq default
	      (if (and (string= folder mh-sent-from-folder)
		       mh-sent-from-msg)
		  (int-to-string mh-sent-from-msg)
		(save-excursion
		  (let (cur-msg)
		    (if (and
			 (= 0 (mh-exec-cmd-quiet nil "pick" folder "cur"))
			 (set-buffer mh-temp-buffer)
			 (setq cur-msg (buffer-string))
			 (string-match "^[0-9]+$" cur-msg))
			(substring cur-msg 0 (match-end 0))
		      (car files)))))))
    (completing-read prompt
		     (let ((i 0))
		       (mapcar (function
				(lambda (file)
				  (setq i (+ i 1))
				  (list file i)
				  ))
			       files)
		       ) nil nil default)
    ))

;;; modified by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;	1995/11/14 (cf. [tm-ja:1096])
(defun tm-mh-e/query-message (&optional message)
  (let (folder number)
    (if message
	(progn
	  (setq folder (tm-mh-e::message/folder message))
	  (setq number (tm-mh-e::message/number message))
	  ))
    (or (stringp folder)
	(setq folder (mh-prompt-for-folder
		      "Message from"
		      (if (and (stringp mh-sent-from-folder)
			       (string-match "^\\+" mh-sent-from-folder))
			  mh-sent-from-folder "+inbox")
		      nil)))
    (setq number
	  (if (numberp number)
	      (number-to-string number)
	    (tm-mh-e/prompt-for-message "Message number: " folder)
	    ))
    (tm-mh-e::make-message folder number)
    ))

(defun tm-mh-e/insert-message (&optional message)
  ;; always ignores message
  (let ((article-buffer
	 (if (not (and (stringp mh-sent-from-folder)
		       (numberp mh-sent-from-msg)
		       ))
	     (cond ((and (boundp 'gnus-original-article-buffer)
			 (bufferp mh-sent-from-folder)
			 (get-buffer gnus-original-article-buffer)
			 )
		    gnus-original-article-buffer)
		   ((and (boundp 'gnus-article-buffer)
			 (get-buffer gnus-article-buffer)
			 (bufferp mh-sent-from-folder)
			 )
		    (save-excursion
		      (set-buffer gnus-article-buffer)
		      (if (eq major-mode 'mime/viewer-mode)
			  mime::preview/article-buffer
			(current-buffer)
			)))
		   ))))
    (if (null article-buffer)
	(tm-mh-e/insert-mail
	 (tm-mh-e::make-message mh-sent-from-folder mh-sent-from-msg)
	 )
      (insert-buffer article-buffer)
      (mime-editor/inserted-message-filter)
      )
    ))

(defun tm-mh-e/insert-mail (&optional message)
  (save-excursion
    (save-restriction
      (let ((message-file
	     (tm-mh-e::message/file-name (tm-mh-e/query-message message))))
	(narrow-to-region (point) (point))
	(insert-file-contents message-file)
	(push-mark (point-max))
	(mime-editor/inserted-message-filter)
    ))))

(set-alist 'mime-editor/message-inserter-alist
	   'mh-letter-mode (function tm-mh-e/insert-message))
(set-alist 'mime-editor/mail-inserter-alist
	   'mh-letter-mode (function tm-mh-e/insert-mail))
(set-alist 'mime-editor/mail-inserter-alist
	   'news-reply-mode (function tm-mh-e/insert-mail))
(set-alist
 'mime-editor/split-message-sender-alist
 'mh-letter-mode
 (function
  (lambda (&optional arg)
    (interactive "P")
    (write-region (point-min) (point-max)
		  mime-editor/draft-file-name nil 'no-message)
    (cond (arg
	   (pop-to-buffer "MH mail delivery")
	   (erase-buffer)
	   (mh-exec-cmd-output mh-send-prog t "-watch" "-nopush"
			       "-nodraftfolder"
			       mh-send-args
			       mime-editor/draft-file-name)
	   (goto-char (point-max))	; show the interesting part
	   (recenter -1)
	   (sit-for 1))
	  (t
	   (apply 'mh-exec-cmd-quiet t mh-send-prog 
		  (mh-list-to-string
		   (list "-nopush" "-nodraftfolder"
			 "-noverbose" "-nowatch"
			 mh-send-args mime-editor/draft-file-name)))))
    )))


;;; @ commands using tm-edit features
;;;

(defun tm-mh-e/edit-again (msg)
  "Clean-up a draft or a message previously sent and make it resendable.
Default is the current message.
The variable mh-new-draft-cleaned-headers specifies the headers to remove.
See also documentation for `\\[mh-send]' function."
  (interactive (list (mh-get-msg-num t)))
  (catch 'tag
    (let* ((from-folder mh-current-folder)
	   (config (current-window-configuration))
	   code-conversion
	   (draft
	    (cond ((and mh-draft-folder (equal from-folder mh-draft-folder))
		   (let ((name (format "draft-%d" msg)))
		     (if (get-buffer name)
			 (throw 'tag (pop-to-buffer name))
		       )
		     (let ((file-coding-system-for-read *noconv*)
			   (filename
			    (mh-msg-filename msg mh-draft-folder)
			    ))
		       (set-buffer (get-buffer-create name))
		       (insert-file-contents filename)
		       (setq buffer-file-name filename)
		       (setq code-conversion t)
		       )
		     (pop-to-buffer name)
		     (if (re-search-forward "^-+$" nil t)
			 (replace-match "")
			 )
		     name))
		  (t
		   (prog1
		       (let ((file-coding-system-for-read *noconv*))
			 (mh-read-draft "clean-up" (mh-msg-filename msg) nil)
			 )
		     (setq code-conversion t)
		     ))))
	   )
      (goto-char (point-min))
      (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil)
      (if code-conversion
	  (let ((cs (detect-coding-region (point-min)(point-max))))
	    (set-buffer-file-coding-system
	     (if (listp cs)
		 (car cs)
	       cs))
	    ))
      (save-buffer)
      (mime/edit-again code-conversion t t)
      (goto-char (point-min))
      (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
				config)
      )))

;;; by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;	1996/2/29 (cf. [tm-ja:1643])
(defun tm-mh-e/extract-rejected-mail (msg)
  "Extract a letter returned by the mail system and make it re-editable.
Default is the current message.  The variable mh-new-draft-cleaned-headers
gives the headers to clean out of the original message."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder)
	(config (current-window-configuration))
	(draft (mh-read-draft "extraction" (mh-msg-filename msg) nil)))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (cond 
     ((and
       (re-search-forward
	(concat "^\\($\\|[Cc]ontent-[Tt]ype:[ \t]+multipart/\\)") nil t)
       (not (bolp))
       (re-search-forward "boundary=\"\\([^\"]+\\)\"" nil t))
      (let ((case-fold-search t)
	    (boundary (buffer-substring (match-beginning 1) (match-end 1))))
	(cond
	 ((re-search-forward
	   (concat "^--" boundary "\n"
		   "content-type:[ \t]+"
		   "\\(message/rfc822\\|text/rfc822-headers\\)\n"
		   "\\(.+\n\\)*\n") nil t)
	  (delete-region (point-min) (point))
	  (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil)
	  (search-forward
	   (concat "\n--" boundary "--\n") nil t)
	  (delete-region (match-beginning 0) (point-max)))
	 (t
	  (message "Seems no message/rfc822 part.")))))
     ((re-search-forward mh-rejected-letter-start nil t)
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (point))
      (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil))
     (t
      (message "Does not appear to be a rejected letter.")))
    (goto-char (point-min))
    (if (re-search-forward "^-+$" nil t)
	(replace-match "")
      )
    (mime/edit-again nil t t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder msg
			      (mh-get-header-field "To:")
			      (mh-get-header-field "From:")
			      (mh-get-header-field "Cc:")
			      nil nil config)))

;;; by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;	1995/11/14 (cf. [tm-ja:1099])
(defun tm-mh-e/forward (to cc &optional msg-or-seq)
  "Forward a message or message sequence as MIME message/rfc822.
Defaults to displayed message. If optional prefix argument provided,
then prompt for the message sequence. See also documentation for
`\\[mh-send]' function."
  (interactive (list (mh-read-address "To: ")
		     (mh-read-address "Cc: ")
		     (if current-prefix-arg
			 (mh-read-seq-default "Forward" t)
		       (mh-get-msg-num t)
		       )))
  (or msg-or-seq
      (setq msg-or-seq (mh-get-msg-num t)))
  (let* ((folder mh-current-folder)
	 (config (current-window-configuration))
	 ;; uses "draft" for compatibility with forw.
	 ;; forw always leaves file in "draft" since it doesn't have -draft
	 (draft-name (expand-file-name "draft" mh-user-path))
	 (draft (cond ((or (not (file-exists-p draft-name))
			   (y-or-n-p "The file `draft' exists.  Discard it? "))
		       (mh-exec-cmd "comp"
				    "-noedit" "-nowhatnowproc"
				    "-form" tm-mh-e/forwcomps
				    "-nodraftfolder")
		       (prog1
			   (mh-read-draft "" draft-name t)
			 (mh-insert-fields "To:" to "Cc:" cc)
			 (set-buffer-modified-p nil)))
		      (t
		       (mh-read-draft "" draft-name nil)))))
    (let ((msubtype "digest")
	  orig-from orig-subject multipart-flag
	  (tag-regexp
	   (concat "^"
		   (regexp-quote (mime-make-tag "message" "rfc822"))))
	  )
      (goto-char (point-min))
      (save-excursion
	(save-restriction
	  (goto-char (point-max))
	  (if (not (bolp)) (insert "\n"))
	  (let ((beg (point)))
	    (narrow-to-region beg beg)
	    (mh-exec-cmd-output "pick" nil folder msg-or-seq)
	    (if (> (count-lines (point) (point-max)) 1)
		(setq multipart-flag t)
	      )
	    (while (re-search-forward "^\\([0-9]+\\)\n" nil t)
	      (let ((forw-msg
		     (buffer-substring (match-beginning 1) (match-end 1)))
		    (beg (match-beginning 0))
		    (end (match-end 0))
		    )
		(save-restriction
		  (narrow-to-region beg end)
		  ;; modified for Emacs 18
		  (delete-region beg end)
		  (insert-file-contents
		   (mh-expand-file-name forw-msg
					(mh-expand-file-name folder))
		   )
		  (save-excursion
		    (push-mark (point-max))
		    (mime-editor/inserted-message-filter))
		  (goto-char (point-max))
		  )
		(save-excursion
		  (goto-char beg)
		  (mime-editor/insert-tag "message" "rfc822")
		  )))
	    (delete-region (point) (point-max))
	    (if multipart-flag
		(mime-editor/enclose-region "digest" beg (point))
	      ))))
      (re-search-forward tag-regexp)
      (forward-line 1)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(setq orig-from (mime-eword/decode-string
			 (mh-get-header-field "From:")))
	(setq orig-subject (mime-eword/decode-string
			    (mh-get-header-field "Subject:")))
	)
      (let ((forw-subject
	     (mh-forwarded-letter-subject orig-from orig-subject)))
	(mh-insert-fields "Subject:" forw-subject)
	(goto-char (point-min))
	(re-search-forward tag-regexp)
	(forward-line -1)
	(delete-other-windows)
	(if (numberp msg-or-seq)
	    (mh-add-msgs-to-seq msg-or-seq 'forwarded t)
	  (mh-add-msgs-to-seq (mh-seq-to-msgs msg-or-seq) 'forwarded t))
	(mh-compose-and-send-mail draft "" folder msg-or-seq
				  to forw-subject cc
				  mh-note-forw "Forwarded:"
				  config)))))

(cond ((not (featurep 'mh-utils))
       (defun tm-mh-e::insert-letter (folder number verbatim)
	 (mh-insert-letter verbatim folder number)
	 )
       )
      ((and (boundp 'mh-e-version)
	    (string-lessp mh-e-version "5"))
       (defun tm-mh-e::insert-letter (folder number verbatim)
	 (mh-insert-letter number folder verbatim)
	 )
       )
      (t
       (defalias 'tm-mh-e::insert-letter 'mh-insert-letter)
       ))

(defun tm-mh-e/insert-letter (verbatim)
  "Interface to mh-insert-letter."
  (interactive "P")
  (let*
      ((folder (mh-prompt-for-folder
		"Message from"
		(if (and (stringp mh-sent-from-folder)
			 (string-match "^\\+" mh-sent-from-folder))
		    mh-sent-from-folder "+inbox")
		nil))
       (number (tm-mh-e/prompt-for-message "Message number: " folder)))
    (tm-mh-e::insert-letter folder number verbatim)))

(defun tm-mh-e/yank-cur-msg-with-no-filter ()
  "Insert the current message into the draft buffer.
This function makes new show-buffer from article-buffer to disable
variable `mime-viewer/plain-text-preview-hook'. If you don't want to
use text filters for replying message, please set it to
`tm-mh-e/message-yank-function'.
Prefix each non-blank line in the message with the string in
`mh-ins-buf-prefix'. The entire message will be inserted if
`mh-yank-from-start-of-msg' is non-nil. If this variable is nil, the
portion of the message following the point will be yanked.  If
`mh-delete-yanked-msg-window' is non-nil, any window displaying the
yanked message will be deleted."
  (interactive)
  (if (and mh-sent-from-folder mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(if mh-delete-yanked-msg-window
	    (delete-windows-on mh-show-buffer))
	(set-buffer mh-show-buffer)	; Find displayed message
	(let ((mh-ins-str
	       (let (mime-viewer/plain-text-preview-hook buf)
		 (prog1
		     (save-window-excursion
		       (set-buffer mime::preview/article-buffer)
		       (setq buf (mime/viewer-mode))
		       (buffer-string)
		       )
		   (kill-buffer buf)))))
	  (set-buffer to-buffer)
	  (save-restriction
	    (narrow-to-region to-point to-point)
	    (push-mark)
	    (insert mh-ins-str)
	    (mh-insert-prefix-string mh-ins-buf-prefix)
	    (insert "\n"))))
    (error "There is no current message")))

(defun tm-mh-e/yank-current-message ()
  "Insert the current message into the draft buffer.
It uses variable `tm-mh-e/message-yank-function'
to select message yanking function."
  (interactive)
  (let ((mh-sent-from-folder mh-sent-from-folder)
	(mh-sent-from-msg mh-sent-from-msg))
    (if (and (not (stringp mh-sent-from-folder))
	     (boundp 'gnus-article-buffer)
	     (get-buffer gnus-article-buffer)
	     (bufferp mh-sent-from-folder)
	     ) ; might be called from GNUS
	(if (boundp 'gnus-article-copy) ; might be sgnus
	    (save-excursion
	      (gnus-copy-article-buffer)
	      (setq mh-sent-from-folder gnus-article-copy)
	      (set-buffer mh-sent-from-folder)
	      (setq mh-show-buffer gnus-article-copy)
	      )
	  (save-excursion
	    (setq mh-sent-from-folder gnus-article-buffer)
	    (set-buffer gnus-article-buffer)
	    (setq mh-show-buffer (current-buffer))
	    )))
    (funcall tm-mh-e/message-yank-function)
    ))

(substitute-key-definition
 'mh-yank-cur-msg 'tm-mh-e/yank-current-message mh-letter-mode-map)
(substitute-key-definition
 'mh-insert-letter 'tm-mh-e/insert-letter mh-letter-mode-map)


;;; @ end
;;;

(provide 'tmh-comp)
(require 'tm-mh-e)

;;; tmh-comp.el ends here
