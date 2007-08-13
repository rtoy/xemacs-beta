;;; gnus-art-mime.el --- MIME extension for article mode of Gnus

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/6
;; Version:
;;	$Id: gnus-art-mime.el,v 1.3 1997/02/02 05:06:18 steve Exp $
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

(require 'emu)
(require 'gnus-mime)
(require 'gnus-art)
(require 'tm-view)

(autoload 'mime-eword/decode-region "tm-ew-d"
  "Decode MIME encoded-words in region." t)
(autoload 'mime/decode-message-header "tm-ew-d"
  "Decode MIME encoded-words in message header." t)


;;; @ encoded-word
;;;

;;; `gnus-decode-rfc1522' of Gnus works only Q-encoded iso-8859-1
;;; encoded-words.  In addition, it does not apply decoding rule of
;;; RFC 1522 and it does not do unfolding.  So gnus-mime defines own
;;; function using tm-ew-d.

(defun gnus-decode-encoded-word ()
  (goto-char (point-min))
  (if (re-search-forward "^[0-9]+\t" nil t)
      (progn
	(goto-char (point-min))
	;; for XOVER
	(while (re-search-forward "^[0-9]+\t\\([^\t]+\\)\t" nil t)
	  (mime-eword/decode-region (match-beginning 1) (match-end 1)
				    'unfolding 'must-unfold)
	  (if (re-search-forward "[^\t]+" nil t)
	      (mime-eword/decode-region (match-beginning 0)(match-end 0)
					'unfolding 'must-unfold)
	    )
	  ))
    (mime-eword/decode-region (point-min)(point-max) t)
    ))

(defalias 'gnus-decode-rfc1522 'gnus-decode-encoded-word)

;; In addition, latest RFC about encoded-word is RFC 2047. (^_^;


;;; @ article filter
;;;

(defun gnus-article-preview-mime-message ()
  (make-local-variable 'tm:mother-button-dispatcher)
  (setq tm:mother-button-dispatcher
	(function gnus-article-push-button))
  (let ((mime-viewer/ignored-field-regexp "^:$")
	(default-mime-charset
	  (save-excursion
	    (set-buffer gnus-summary-buffer)
	    default-mime-charset))
	)
    (save-window-excursion
      (mime/viewer-mode nil nil nil gnus-original-article-buffer
			gnus-article-buffer
			gnus-article-mode-map)
      ))
  (run-hooks 'tm-gnus/article-prepare-hook)
  )

(defun gnus-article-decode-encoded-word ()
  (decode-mime-charset-region (point-min)(point-max)
			      (save-excursion
				(set-buffer gnus-summary-buffer)
				default-mime-charset))
  (mime/decode-message-header)
  (run-hooks 'tm-gnus/article-prepare-hook)
  )


;;; @ for tm-view
;;;

(defun gnus-content-header-filter ()
  (goto-char (point-min))
  (mime-preview/cut-header)
  (decode-mime-charset-region (point-min)(point-max) default-mime-charset)
  (mime/decode-message-header)
  )

(defun mime-viewer/quitting-method-for-gnus ()
  (if (not gnus-show-mime)
      (mime-viewer/kill-buffer))
  (delete-other-windows)
  (gnus-article-show-summary)
  (if (or (not gnus-show-mime)
	  (null gnus-have-all-headers))
      (gnus-summary-select-article nil t)
    ))

(call-after-loaded
 'tm-view
 (lambda ()
   (set-alist 'mime-viewer/content-header-filter-alist
	      'gnus-original-article-mode
	      (function gnus-content-header-filter))
   
   (set-alist 'mime-viewer/code-converter-alist
	      'gnus-original-article-mode
	      (function mime-charset/decode-buffer))
   
   (set-alist 'mime-viewer/quitting-method-alist
	      'gnus-original-article-mode
	      (function mime-viewer/quitting-method-for-gnus))
   
   (set-alist 'mime-viewer/show-summary-method
	      'gnus-original-article-mode
	      (function mime-viewer/quitting-method-for-gnus))
   ))


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'tm-bbdb)
    )))

(autoload 'tm-bbdb/update-record "tm-bbdb")

(defun tm-gnus/bbdb-setup ()
  (if (and (boundp 'gnus-article-prepare-hook)
	   (memq 'bbdb/gnus-update-record gnus-article-prepare-hook)
	   )
      (progn
	(remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-update-record)
	(add-hook 'gnus-article-display-hook 'tm-bbdb/update-record)
	)))

(add-hook 'gnus-startup-hook 'tm-gnus/bbdb-setup t)

(tm-gnus/bbdb-setup)


;;; @ end
;;;

(provide 'gnus-art-mime)

;;; gnus-art-mime.el ends here
