;;; gnus-sum-mime.el --- MIME extension for summary mode of Gnus

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/6
;; Version:
;;	$Id: gnus-sum-mime.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'gnus-mime)
(require 'gnus-art-mime)


;;; @ summary filter
;;;

(defun gnus-set-summary-default-charset ()
  (let ((charset
	 (if (buffer-live-p gnus-summary-buffer)
	     (save-excursion
	       (set-buffer gnus-summary-buffer)
	       (let ((ret (assoc-if (function
				     (lambda (key)
				       (string-match key gnus-newsgroup-name)
				       ))
				    gnus-newsgroup-default-charset-alist)
			  ))
		 (if ret
		     (progn
		       (make-local-variable 'default-mime-charset)
		       (setq default-mime-charset (cdr ret))
		       ))
		 )
	       default-mime-charset)
	   default-mime-charset)))
    (goto-char (point-min))
    (while (< (point)(point-max))
      (decode-mime-charset-region (point)
				  (progn
				    (end-of-line)
				    (point))
				  charset)
      (end-of-line)
      (forward-char)
      )))


;;; @ command functions
;;;

(defun gnus-summary-preview-mime-message (arg)
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

(defun gnus-summary-scroll-down ()
  "Scroll down one line current article."
  (interactive)
  (gnus-summary-scroll-up -1)
  )

(define-key gnus-summary-mode-map "v"
  (function gnus-summary-preview-mime-message))
(define-key gnus-summary-mode-map "\e\r"
  (function gnus-summary-scroll-down))


;;; @ end
;;;

(provide 'gnus-sum-mime)

;;; gnus-sum-mime.el ends here
