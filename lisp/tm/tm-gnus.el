;;;
;;; tm-gnus.el --- MIME extension for GNUS
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1993 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; modified by KOBAYASHI Shuhei <shuhei@cmpt01.phys.tohoku.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1993/11/20 (obsolete mol's gnus-mime.el)
;;; Version:
;;;	$Id: tm-gnus.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
;;; Keywords: news, MIME, multimedia, encoded-word, multilingual
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

(require 'gnus)


;;; @ variables
;;;

(defvar tm-gnus/startup-hook nil)


;;; @ set up
;;;

(cond ((boundp 'gnus-original-article-buffer)
       ;; for Gnus 5.2 or later
       (require 'tm-gnus5)
       )
      ((or (string-match
	    "^\\((ding) Gnus\\|Gnus v5\\|September Gnus\\)" gnus-version)
	   (fboundp 'mail-header-from))
       ;; for Gnus 5.0 .. 5.1.*
       (require 'tm-gnus4)
       (cond ((not (boundp 'nnheader-encoded-words-decoding))
	      (require 'tm-ew-d)
	      (defun tm-gnus/decode-summary-from-and-subjects ()
		(mapcar (lambda (header)
			  (let ((from (mail-header-from header))
				(subj (mail-header-subject header))
				)
			    (mail-header-set-from
			     header
			     (if from
				 (mime-eword/decode-string from)
			       ""))
			    (mail-header-set-subject
			     header
			     (if subj
				 (mime-eword/decode-string subj)
			       ""))
			    ))
			gnus-newsgroup-headers))
	      (add-hook 'gnus-select-group-hook
			(function tm-gnus/decode-summary-from-and-subjects))
	      ))
       )
      ((fboundp 'gnus-article-prepare)
       ;; for GNUS 3.15 .. 4.*
       (require 'tm-gd3)
       (require 'tm-gnus4)
       (add-hook 'gnus-select-group-hook 'tm-gnus/decode-summary-subjects)
       (fset 'gnus-article-set-mode-line
	     (function tm-gnus/article-set-mode-line))
       
       (or (fboundp 'tm:gnus-article-delete-headers)
	   (fset 'tm:gnus-article-delete-headers
		 (symbol-function 'gnus-article-delete-headers))
	   )
       (defun gnus-article-delete-headers ()
	 (or tm-gnus/automatic-mime-preview
	     (tm:gnus-article-delete-headers)
	     ))
       
       (require 'gnuspost)
       (or (fboundp 'tm-gnus/original-news-reply)
	   (fset 'tm-gnus/original-news-reply
		 (symbol-function 'gnus-news-reply))
	   )
       (defun gnus-news-reply (&optional yank)
	 (if (eq major-mode 'mime/viewer-mode)
	     (let ((major-mode 'gnus-article-mode))
	       (tm-gnus/original-news-reply yank)
	       )
	   (tm-gnus/original-news-reply yank)
	   ))
       )
      ((string-match "^GNUS 3" gnus-version)
       ;; for GNUS 3.14.*
       (require 'tm-gnus3)
       (defvar gnus-article-buffer gnus-Article-buffer)
       ))


;;; @ end
;;;

(provide 'tm-gnus)

(run-hooks 'tm-gnus-load-hook)

;;; tm-gnus.el ends here
