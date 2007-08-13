;;;
;;; tm-text.el --- a content filter module of tm-view to display
;;;                text/plain, text/richtext and text/enriched
;;;                in Emacs buffers
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id: tm-text.el,v 1.1.1.1 1996/12/18 22:43:37 steve Exp $
;;; Keywords: mail, news, MIME, multimedia, text
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

;;; @ code conversion
;;;

(defvar mime-viewer/code-converter-alist
  '((mime/show-message-mode      . mime-charset/decode-buffer)
    (mime/temporary-message-mode . mime-charset/decode-buffer)
    (t				 . mime-charset/maybe-decode-buffer)
    ))

(defun mime-charset/decode-buffer (charset &optional encoding)
  (decode-mime-charset-region (point-min)(point-max)
			      (or charset default-mime-charset))
  )

(defun mime-charset/maybe-decode-buffer (charset &optional encoding)
  (or (member encoding '(nil "7bit" "8bit" "binary"))
      (mime-charset/decode-buffer charset)
      ))

(defun mime-preview/decode-text-buffer (charset encoding)
  (mime-decode-region (point-min) (point-max) encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (or (save-excursion
		  (set-buffer mime::preview/article-buffer)
		  mime::article/code-converter)
		(cdr (or (assq mode mime-viewer/code-converter-alist)
			 (assq t mime-viewer/code-converter-alist)))
		))
	 )
    (and (functionp m)
	 (funcall m charset encoding)
	 )))


;;; @ content filters for tm-view
;;;

(defun mime-preview/filter-for-text/plain (ctype params encoding)
  (mime-preview/decode-text-buffer (cdr (assoc "charset" params)) encoding)
  (goto-char (point-max))
  (if (not (eq (char-after (1- (point))) ?\n))
      (insert "\n")
    )
  (if browse-url-browser-function
      (progn
	(goto-char (point-min))
	(while (re-search-forward tm:URL-regexp nil t)
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    (tm:add-button beg end
			   (function tm:browse-url)
			   (list (buffer-substring beg end))))
	  )))
  (run-hooks 'mime-viewer/plain-text-preview-hook)
  )

(defun mime-preview/filter-for-text/richtext (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-preview/decode-text-buffer charset encoding)
    (richtext-decode beg (point-max))
    ))

(defun mime-preview/filter-for-text/enriched (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-viewer/code-converter-alist))
	 (charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-preview/decode-text-buffer charset encoding)
    (enriched-decode beg (point-max))
    ))


;;; @ end
;;;

(provide 'tm-text)

;;; tm-text.el ends here
