;;; gnus-mime-old.el --- MIME extensions for Gnus 5.[01] and 5.[23]

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/9/4
;; Version:
;;	$Id: gnus-mime-old.el,v 1.2 1996/12/28 21:03:11 steve Exp $
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

(provide 'gnus-sum)
(provide 'gnus-art)

(or (boundp 'gnus-original-article-buffer)
    (progn
      ;; for Gnus 5.0.* and 5.1
      (defvar gnus-original-article-buffer " *Original Article*")
      
      (defun gnus-article-setup-original-article-buffer ()
	(save-excursion
	  (set-buffer (get-buffer-create gnus-original-article-buffer))
	  (erase-buffer)
	  (insert-buffer gnus-article-buffer)
	  (setq major-mode 'gnus-original-article-mode)
	  ))
      
      (add-hook 'gnus-article-prepare-hook
		'gnus-article-setup-original-article-buffer)
      
      (setq gnus-strict-mime nil)
      ))

(if running-xemacs
    (progn
      ;; modified by Steven L. Baur <steve@miranova.com>
      ;;	1995/12/6 (c.f. [tm-en:209])
      (defun mime-editor/attach-to-news-reply-menu ()
	"Arrange to attach MIME editor's popup menu to VM's"
	(if (boundp 'news-reply-menu)
	    (progn
	      (setq news-reply-menu
		    (append news-reply-menu
			    '("---")
			    mime-editor/popup-menu-for-xemacs))
	      (remove-hook 'news-setup-hook
			   'mime-editor/attach-to-news-reply-menu)
	      )))
      (call-after-loaded
       'tm-edit
       (function
	(lambda ()
	  (add-hook 'news-setup-hook
		    'mime-editor/attach-to-news-reply-menu)
	  )))
      ))


;;; @ end
;;;

(provide 'gnus-mime-old)

;;; gnus-mime-old.el ends here
