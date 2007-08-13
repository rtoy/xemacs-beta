;;; tm-ftp.el --- tm-view internal method for anonymous ftp

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MASUTANI Yasuhiro <masutani@me.es.osaka-u.ac.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1994/11/5
;; Version: $Id: tm-ftp.el,v 1.4 1997/02/04 02:36:06 steve Exp $
;; Keywords: anonymous ftp, MIME, multimedia, mail, news

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

;;; Code:

(require 'tm-view)
(require 'ange-ftp)

(defvar mime-article/dired-function
  (if mime/use-multi-frame
      (function dired-other-frame)
    (function mime-article/dired-function-for-one-frame)
    ))

(defun mime-article/dired-function-for-one-frame (dir)
  (let ((win (or (get-buffer-window mime::article/preview-buffer)
		 (get-largest-window))))
    (select-window win)
    (dired dir)
    ))

(defun mime-article/decode-message/external-ftp (beg end cal)
  (let* ((access-type (cdr (assoc "access-type" cal)))
	 (site (cdr (assoc "site" cal)))
	 (directory (cdr (assoc "directory" cal)))
	 (name (cdr (assoc "name" cal)))
	 (mode (cdr (assoc "mode" cal)))
	 (pathname (concat "/anonymous@" site ":" directory))
	 )
    (message (concat "Accessing " (expand-file-name name pathname) "..."))
    (funcall mime-article/dired-function pathname)
    (goto-char (point-min))
    (search-forward name)
    ))

(set-atype 'mime/content-decoding-condition
	   '((type . "message/external-body")
	     ("access-type" . "anon-ftp")
	     (method . mime-article/decode-message/external-ftp)
	     ))


;;; @ end
;;;

(provide 'tm-ftp)

;;; tm-ftp.el ends here
