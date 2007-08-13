;;; gnus-mime.el --- MIME extensions for Gnus

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/6
;; Version: $Revision: 1.1.1.1 $
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-misc)


;;; @ version
;;;

(defconst gnus-mime-RCS-ID
  "$Id: gnus-mime.el,v 1.1.1.1 1996/12/18 03:55:31 steve Exp $")

(defconst gnus-mime-version
  (get-version-string gnus-mime-RCS-ID))


;;; @ variables
;;;

(defvar gnus-show-mime t
  "*If non-nil, do mime processing of articles.
The articles will simply be fed to the function given by
`gnus-show-mime-method'.")

(defvar gnus-show-mime-method 'gnus-article-preview-mime-message
  "*Function to process a MIME message.
The function is called from the article buffer.")

(defvar gnus-decode-encoded-word-method 'gnus-article-decode-encoded-word
  "*Function to decode a MIME encoded-words.
The function is called from the article buffer.")

(defvar gnus-parse-headers-hook
  '(gnus-set-summary-default-charset gnus-decode-rfc1522)
  "*A hook called before parsing the headers.")


;;; @ load
;;;

(require 'gnus)
(autoload 'gnus-decode-rfc1522			"gnus-art-mime")
(autoload 'gnus-article-preview-mime-message	"gnus-art-mime")
(autoload 'gnus-article-decode-encoded-word	"gnus-art-mime")
(autoload 'gnus-set-summary-default-charset	"gnus-sum-mime")
;;(autoload 'gnus-get-newsgroup-headers		"gnus-sum-mime")
;;(autoload 'gnus-get-newsgroup-headers-xover	"gnus-sum-mime")
(require 'gnus-charset)


;;; @ for tm-partial
;;;

(defun gnus-mime-partial-preview-function ()
  (gnus-summary-preview-mime-message (gnus-summary-article-number))
  )

(call-after-loaded
 'tm-partial
 (function
  (lambda ()
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/partial")
		 (method . mime-article/grab-message/partials)
		 (major-mode . gnus-original-article-mode)
		 (summary-buffer-exp . gnus-summary-buffer)
		 ))
    (set-alist 'tm-partial/preview-article-method-alist
	       'gnus-original-article-mode
	       'gnus-mime-partial-preview-function)
    )))


;;; @ end
;;;

(provide 'gnus-mime)

(if gnus-is-red-gnus-or-later
    (progn
      (call-after-loaded 'gnus-art (lambda ()
				     (require 'gnus-art-mime)
				     ))
      (call-after-loaded 'gnus-sum (lambda ()
				     (require 'gnus-sum-mime)
				     ))
      )
  (require 'gnus-mime-old)
  )

(run-hooks 'gnus-mime-load-hook)

;;; gnus-mime.el ends here
