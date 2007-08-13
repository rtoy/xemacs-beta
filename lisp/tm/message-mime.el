;;; message-mime.el --- MIME extensions for message.el

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/6
;; Version:
;;	$Id: message-mime.el,v 1.3 1997/02/15 22:21:26 steve Exp $
;; Keywords: news, MIME, multimedia, multilingual, encoded-word

;; This file is not part of GNU Emacs yet.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This program.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'tm-edit)

(setq message-forward-start-separator
      (concat (mime-make-tag "message" "rfc822") "\n"))

(setq message-forward-end-separator "")

(or (string-match message-included-forward-headers "Mime-Version:")
    (setq message-included-forward-headers
	  (concat message-included-forward-headers "\\|^Mime-Version:"))
    )

(or (string-match message-included-forward-headers "Content-Type:")
    (setq message-included-forward-headers
	  (concat message-included-forward-headers "\\|^Content-Type:"))
    )

(or (string-match message-included-forward-headers
                  "Content-Transfer-Encoding:")
    (setq message-included-forward-headers
	  (concat message-included-forward-headers
                  "\\|^Content-Transfer-Encoding:"))
    )

;;; @ for tm-edit
;;;

;; suggested by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;	1995/11/08 (c.f. [tm ML:1067])
(defun message-mime-insert-article (&optional message)
  (interactive)
  (let ((message-cite-function 'mime-editor/inserted-message-filter)
        (message-reply-buffer gnus-original-article-buffer)
	)
    (message-yank-original nil)
    ))

(set-alist 'mime-editor/message-inserter-alist
	   'message-mode (function message-mime-insert-article))
(set-alist 'mime-editor/split-message-sender-alist
	   'message-mode
	   (lambda ()
	     (interactive)
	     (let (message-send-hook
		   message-sent-message-via)
	       (message-send)
	       )))


;;; @ end
;;;

(provide 'message-mime)

;;; message-mime.el ends here
