;;; gnus-msg-mime.el --- MIME extension for mail and post interface of Gnus

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/8
;; Version:
;;	$Id: gnus-msg-mime.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
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

(require 'gnus-msg)

(defun gnus-copy-article-buffer-with-no-filter ()
  ;; make a copy of the article buffer with all text properties removed
  ;; this copy is in the buffer gnus-article-copy.
  ;; if ARTICLE-BUFFER is nil, gnus-article-buffer is used
  ;; this buffer should be passed to all mail/news reply/post routines.
  (setq gnus-article-copy (get-buffer-create " *gnus article copy*"))
  (buffer-disable-undo gnus-article-copy)
  (or (memq gnus-article-copy gnus-buffer-list)
      (setq gnus-buffer-list (cons gnus-article-copy gnus-buffer-list)))
  (let (mime-viewer/plain-text-preview-hook
	(mime-viewer/ignored-field-regexp "^:$"))
    (save-window-excursion
      (mime/viewer-mode nil nil nil
			gnus-original-article-buffer gnus-article-copy)
      )
    gnus-article-copy))


;;(fset 'gnus-copy-article-buffer 'gnus-copy-article-buffer-with-no-filter)


;;; @ end
;;;

(provide 'gnus-msg-mime)

;;; gnus-msg-mime.el ends here
