;;;
;;; mel-u.el: uuencode encoder/decoder for GNU Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1995/10/25
;;; Version:
;;;	$Id: mel-u.el,v 1.3 1996/12/29 00:14:57 steve Exp $
;;; Keywords: uuencode
;;;
;;; This file is part of MEL (MIME Encoding Library).
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

(require 'emu)


;;; @ variables
;;;

(defvar mime/tmp-dir (or (getenv "TM_TMP_DIR") "/tmp/"))

(defvar uuencode-external-encoder '("uuencode" "-")
  "*list of uuencode encoder program name and its arguments.")

(defvar uuencode-external-decoder
  (list "sh" "-c" (format "(cd %s; uudecode)" mime/tmp-dir))
  "*list of uuencode decoder program name and its arguments.")


;;; @ uuencode encoder/decoder for region
;;;

(defun uuencode-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car uuencode-external-encoder)
			      t t nil (cdr uuencode-external-encoder))
		       )
    ;; for OS/2
    ;;   regularize line break code
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match "")
      )
    ))

(defun uuencode-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((filename (save-excursion
		      (save-restriction
			(narrow-to-region beg end)
			(goto-char beg)
			(if (re-search-forward "^begin [0-9]+ " nil t)
			    (if (looking-at ".+$")
				(buffer-substring (match-beginning 0)
						  (match-end 0))
			      ))))))
      (if filename
	  (as-binary-process
	   (apply (function call-process-region)
		  beg end (car uuencode-external-decoder)
		  t nil nil (cdr uuencode-external-decoder))
	   (setq filename (expand-file-name filename mime/tmp-dir))
	   (let ((file-coding-system-for-read *noconv*) ; for Mule
		 kanji-fileio-code		        ; for NEmacs
		 (emx-binary-mode t)                    ; for OS/2
		 jka-compr-compression-info-list        ; for jka-compr
		 jam-zcat-filename-list                 ; for jam-zcat
		 require-final-newline)
	     (insert-file-contents filename)
	     )
	   (delete-file filename)
	   ))
      )))

(defalias 'uuencode-encode-region 'uuencode-external-encode-region)
(defalias 'uuencode-decode-region 'uuencode-external-decode-region)


;;; @ uuencode encoder/decoder for file
;;;

(defun uuencode-insert-encoded-file (filename)
  (interactive (list (read-file-name "Insert encoded file: ")))
  (call-process (car uuencode-external-encoder) filename t nil
		(file-name-nondirectory filename))
  )


;;; @ end
;;;

(provide 'mel-u)

;;; mel-u.el ends here
