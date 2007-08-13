;;;
;;; tm-html.el: a tm-view internal decoder for HTML
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1995/9/14
;;;    based on tm-latex.el by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;; Version:
;;;	$Id: tm-html.el,v 1.2 1996/12/22 00:29:40 steve Exp $
;;; Keywords: mail, news, MIME, multimedia, HTML, WWW
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

(require 'tm-view)

(defun mime-article/decode-html (beg end cal)
  (let* ((cur-buf (current-buffer))
	 new-buf
	 (name (or (cdr (assoc "name" cal))
		   (cdr (assoc "x-name" cal))
		   (concat (make-temp-name "tm") ".html")))
	 (encoding (cdr (assq 'encoding cal)))
	 ;; modified by Shuhei KOBAYASHI <shuhei@cmpt01.phys.tohoku.ac.jp>
	 ;;	1995/11/17 (cf. [tm-ja:1117])
         (html-helper-build-new-buffer nil)
	 )
    (switch-to-buffer mime::article/preview-buffer)
    (funcall mime/find-file-function (expand-file-name name mime/tmp-dir))
    (if (or (<= (buffer-size) 0)
	    (y-or-n-p "Replace the existing buffer?"))
	(progn
	  (erase-buffer)
	  (setq new-buf (current-buffer))
	  (save-excursion
	    (set-buffer cur-buf)
	    (goto-char beg)
	    (re-search-forward "^$")
	    (append-to-buffer new-buf (+ (match-end 0) 1) end)
	    )))
    (mime-decode-region (point-min)(point-max) encoding)
    (run-hooks 'mime-article/decode-html-hook)
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "text/html")
	     (method . mime-article/decode-html)
	     (mode . "extract")
	     ))


;;; @ end
;;;

(provide 'tm-html)

;;; end of tm-html.el
