;;;
;;; tm-gd3.el --- tm-gnus module for GNUS 3.* and 4.*
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1995/05/23 (obsolete tm-ognus.el)
;;; Version:
;;;	$Id: tm-gd3.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $
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

(require 'tm-ew-d)


;;; @ to decode subjects in mode-line
;;;
;; This function imported from gnus.el.
;;
;; New implementation in gnus 3.14.3
;;

(defun tm-gnus/article-set-mode-line ()
  "Set Article mode line string.
If you don't like it, define your own gnus-article-set-mode-line."
  (let ((maxlen 15)			;Maximum subject length
	(subject
	 (if gnus-current-headers
	     (mime-eword/decode-string
	      (nntp-header-subject gnus-current-headers))
	   "")
	 ))
    ;; The value must be a string to escape %-constructs because of subject.
    (setq mode-line-buffer-identification
	  (format "GNUS: %s%s %s%s%s"
		  gnus-newsgroup-name
		  (if gnus-current-article
		      (format "/%d" gnus-current-article) "")
		  (truncate-string subject
				   (min (string-width subject) maxlen))
		  (if (> (string-width subject) maxlen) "..." "")
		  (make-string (max 0 (- 17 (string-width subject))) ? )
		  )))
  (set-buffer-modified-p t))


;;; @ to decode subjects in Summary buffer
;;;

(defun tm-gnus/decode-summary-subjects ()
  (mapcar (function
	   (lambda (header)
	     (let ((subj (or (gnus-header-subject header) "")))
	       (nntp-set-header-subject
		header (mime-eword/decode-string subj))
	       )))
	  gnus-newsgroup-headers)
  )


;;; @ end
;;;

(provide 'tm-gd3)

;;; tm-gd3.el ends here
