;;;
;;; mel-g.el: Gzip64 encoder/decoder for GNU Emacs
;;;
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;; Copyright (C) 1996 Shuhei KOBAYASHI
;;;
;;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;;         modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;; Created: 1995/10/25
;;; Version:
;;;	$Id: mel-g.el,v 1.1.1.1 1996/12/18 22:43:39 steve Exp $
;;; Keywords: MIME, base64, gzip
;;;
;;; This file is not part of MEL (MIME Encoding Library) yet.
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

(defvar gzip64-external-encoder '("sh" "-c" "gzip -c | mmencode")
  "*list of gzip64 encoder program name and its arguments.")

(defvar gzip64-external-decoder '("sh" "-c" "mmencode -u | gzip -dc")
  "*list of gzip64 decoder program name and its arguments.")


;;; @ encoder/decoder for region
;;;

(defun gzip64-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car gzip64-external-encoder)
			      t t nil (cdr gzip64-external-encoder))
		       )
    ;; for OS/2
    ;;   regularize line break code
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match "")
      )
    ))

(defun gzip64-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car gzip64-external-decoder)
			      t t nil (cdr gzip64-external-decoder))
		       )
    ))

(defalias 'gzip64-encode-region 'gzip64-external-encode-region)
(defalias 'gzip64-decode-region 'gzip64-external-decode-region)


;;; @ encoder/decoder for file
;;;

(defun gzip64-insert-encoded-file (filename)
  (interactive (list (read-file-name "Insert encoded file: ")))
  (apply (function call-process) (car gzip64-external-encoder)
	 filename t nil
	 (cdr gzip64-external-encoder))
  )


;;; @ end
;;;

(provide 'mel-g)

;;; mel-g.el ends here.
