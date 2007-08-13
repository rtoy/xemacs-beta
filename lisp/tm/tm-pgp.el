;;; tm-pgp.el --- tm-view internal methods for PGP.

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/12/7
;; Version: $Id: tm-pgp.el,v 1.3 1996/12/29 00:15:14 steve Exp $
;; Keywords: mail, news, MIME, multimedia, PGP, security

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

;;; Commentary:

;;    This module is based on 2 drafts about PGP MIME integration:

;;	- draft-elkins-pem-pgp-04.txt
;;		``MIME Security with Pretty Good Privacy (PGP)''
;;		by Michael Elkins <elkins@aero.org> (1996/6)
;;
;;	- draft-kazu-pgp-mime-00.txt
;;		``PGP MIME Integration''
;;		by Kazuhiko Yamamoto <kazu@is.aist-nara.ac.jp> (1995/10)
;;
;;    These drafts may be contrary to each other. You should decide
;;  which you support.

;;; Code:

(require 'mailcrypt)
(require 'tm-play)


;;; @ internal method for application/pgp
;;;
;;; It is based on draft-kazu-pgp-mime-00.txt

(defun mime-article/view-application/pgp (beg end cal)
  (let* ((cnum (mime-article/point-content-number beg))
	 (cur-buf (current-buffer))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (mother mime::article/preview-buffer)
	 (mode major-mode)
	 code-converter str)
    (setq str (buffer-substring beg end))
    (switch-to-buffer new-name)
    (erase-buffer)
    (insert str)
    (cond ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t)
	     )
	   (mc-verify)
	   (goto-char (point-min))
	   (delete-region
	    (point-min)
	    (and
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+\n\n")
	     (match-end 0))
	    )
	   (delete-region
	    (and (re-search-forward "^-+BEGIN PGP SIGNATURE-+")
		 (match-beginning 0))
	    (point-max)
	    )
	   (goto-char (point-min))
	   (while (re-search-forward "^- -" nil t)
	     (replace-match "-")
	     )
	   (setq code-converter
		 (or
		  (cdr (assq mode mime-viewer/code-converter-alist))
		  (function mime-viewer/default-code-convert-region)))
	   )
	  ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP MESSAGE-+$" nil t)
	     )
	   (as-binary-process (mc-decrypt))
	   (goto-char (point-min))
	   (delete-region (point-min)
			  (and
			   (search-forward "\n\n")
			   (match-end 0)))
	   (setq code-converter (function mime-charset/decode-buffer))
	   ))
    (setq major-mode 'mime/show-message-mode)
    (setq mime::article/code-converter code-converter)
    (mime/viewer-mode mother)
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/pgp")
	     (method . mime-article/view-application/pgp)
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "text/x-pgp")
	     (method . mime-article/view-application/pgp)
	     ))


;;; @ Internal method for application/pgp-signature
;;;
;;; It is based on draft-elkins-pem-pgp-02.txt

(defvar tm-pgp::default-language 'en
  "*Symbol of language for pgp.
It should be ISO 639 2 letter language code such as en, ja, ...")

(defvar tm-pgp::good-signature-regexp-alist
  '((en . "Good signature from user.*$"))
  "Alist of language vs regexp to detect ``Good signature''.")

(defvar tm-pgp::key-expected-regexp-alist
  '((en . "Key matching expected Key ID \\(\\S +\\) not found"))
  "Alist of language vs regexp to detect ``Key expected''.")

(defun mime::article/call-pgp-to-check-signature (output-buffer orig-file)
  (save-excursion
    (set-buffer output-buffer)
    (erase-buffer)
    )
  (let* ((lang (or tm-pgp::default-language 'en))
	 (status
	  (call-process-region (point-min)(point-max)
			       "pgp" nil output-buffer nil orig-file
			       (format "+language=%s" lang)
			       ))
	 (regexp (cdr (assq lang tm-pgp::good-signature-regexp-alist)))
	 )
    (if (= status 0)
	(save-excursion
	  (set-buffer output-buffer)
	  (goto-char (point-min))
	  (message
	   (cond ((not (stringp regexp))
		  "Please specify right regexp for specified language")
		 ((re-search-forward regexp nil t)
		  (buffer-substring (match-beginning 0) (match-end 0))
		  )
		 (t
		  "Bad signature"
		  )))
	  ))))

(defun mime-article/check-pgp-signature (beg end cal)
  (let* ((encoding (cdr (assq 'encoding cal)))
	 (cnum (mime-article/point-content-number beg))
	 (rcnum (reverse cnum))
	 (rmcnum (cdr rcnum))
	 (knum (car rcnum))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (oinfo (mime-article/rcnum-to-cinfo (cons onum rmcnum)
					     mime::article/content-info))
	 status str kbuf
	 (basename (expand-file-name "tm" mime/tmp-dir))
	 (orig-file (make-temp-name basename))
	 (sig-file (concat orig-file ".sig"))
	 )
    (save-excursion
      (setq str (buffer-substring
		 (mime::content-info/point-min oinfo)
		 (mime::content-info/point-max oinfo)
		 ))
      (set-buffer (get-buffer-create mime/temp-buffer-name))
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match "\r\n")
	)
      (let ((mc-flag nil)                   ; for Mule
	    (file-coding-system *noconv*)
	    kanji-flag                      ; for NEmacs
	    (emx-binary-mode t)             ; for OS/2
	    jka-compr-compression-info-list ; for jka-compr
	    jam-zcat-filename-list          ; for jam-zcat
	    require-final-newline)
	(write-file orig-file)
	)
      (kill-buffer (current-buffer))
      )
    (save-excursion
      (mime-article/show-output-buffer)
      )
    (save-excursion
      (setq str (buffer-substring
		 (save-excursion
		   (goto-char beg)
		   (and (search-forward "\n\n")
			(match-end 0)))
		 end))
      (set-buffer (setq kbuf (get-buffer-create mime/temp-buffer-name)))
      (insert str)
      (mime-decode-region (point-min)(point-max) encoding)
      (let ((mc-flag nil)                   ; for Mule
	    (file-coding-system *noconv*)
	    kanji-flag                      ; for NEmacs
	    (emx-binary-mode t)             ; for OS/2
	    jka-compr-compression-info-list ; for jka-compr
	    jam-zcat-filename-list          ; for jam-zcat
	    require-final-newline)
	(write-file sig-file)
	)
      ;;(get-buffer-create mime/output-buffer-name)
      (or (mime::article/call-pgp-to-check-signature
	   mime/output-buffer-name orig-file)
	  (let (pgp-id)
	    (save-excursion
	      (set-buffer mime/output-buffer-name)
	      (goto-char (point-min))
	      (let ((regexp (cdr (assq (or tm-pgp::default-language 'en)
				       tm-pgp::key-expected-regexp-alist))))
		(cond ((not (stringp regexp))
		       (message
			"Please specify right regexp for specified language")
		       )
		      ((re-search-forward regexp nil t)
		       (setq pgp-id
			     (concat "0x" (buffer-substring-no-properties
					   (match-beginning 1)
					   (match-end 1))))
		       ))))
	    (if (and pgp-id
		     (y-or-n-p
		      (format "Key %s not found; attempt to fetch? " pgp-id))
		     )
		(progn
		  (mc-pgp-fetch-key (cons nil pgp-id))
		  (mime::article/call-pgp-to-check-signature
		   mime/output-buffer-name orig-file)
		  ))
	    ))
      (let ((other-window-scroll-buffer mime/output-buffer-name))
	(scroll-other-window 8)
	)
      (kill-buffer kbuf)
      (delete-file orig-file)
      (delete-file sig-file)
      )))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/pgp-signature")
	     (method . mime-article/check-pgp-signature)
	     ))


;;; @ Internal method for application/pgp-encrypted
;;;
;;; It is based on draft-elkins-pem-pgp-02.txt

(defun mime-article/decrypt-pgp (beg end cal)
  (let* ((cnum (mime-article/point-content-number beg))
	 (rcnum (reverse cnum))
	 (rmcnum (cdr rcnum))
	 (knum (car rcnum))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (oinfo (mime-article/rcnum-to-cinfo (cons onum rmcnum)
					     mime::article/content-info))
	 (obeg (mime::content-info/point-min oinfo))
	 (oend (mime::content-info/point-max oinfo))
	 )
    (mime-article/view-application/pgp obeg oend cal)
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/pgp-encrypted")
	     (method . mime-article/decrypt-pgp)
	     ))


;;; @ Internal method for application/pgp-keys
;;;
;;; It is based on draft-elkins-pem-pgp-02.txt

(autoload 'mc-snarf-keys "mc-toplev")

(defun mime-article/add-pgp-keys (beg end cal)
  (let* ((cnum (mime-article/point-content-number beg))
	 (cur-buf (current-buffer))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (mother mime::article/preview-buffer)
	 (charset (cdr (assoc "charset" cal)))
	 (encoding (cdr (assq 'encoding cal)))
	 (mode major-mode)
	 str)
    (setq str (buffer-substring beg end))
    (switch-to-buffer new-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\n" nil t)
	(delete-region (point-min) (match-end 0))
      )
    (mime-decode-region (point-min)(point-max) encoding)
    (mc-snarf-keys)
    (kill-buffer (current-buffer))
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/pgp-keys")
	     (method . mime-article/add-pgp-keys)
	     ))

	 
;;; @ end
;;;

(provide 'tm-pgp)

(run-hooks 'tm-pgp-load-hook)

;;; tm-pgp.el ends here
