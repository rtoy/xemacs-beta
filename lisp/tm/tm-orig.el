;;;
;;; tm-orig.el --- tm definitions depended on FSF Original Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id: tm-orig.el,v 1.1.1.1 1996/12/18 03:55:32 steve Exp $
;;; Keywords: mail, news, MIME, multimedia, multilingual, encoded-word
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

(require 'emu)


;;; @ variables
;;;

(defvar mime/default-coding-system nil)

(defvar mime/lc-charset-alist
  (list
   (cons (list lc-ascii)         "US-ASCII")
   (cons (list lc-ascii lc-ltn1) "ISO-8859-1")
   ))

(defvar mime/unknown-charset "ISO-8859-1")


;;; @ functions
;;;

(defun mime/convert-string-to-emacs (charset str)
  (if (or (string= "US-ASCII"   charset)
	  (string= "ISO-8859-1" charset))
      str))

(defun mime/convert-string-from-emacs (str charset)
  (if (or (string= charset "US-ASCII")
	  (string= charset "ISO-8859-1"))
      str))

(defun mime/code-convert-region-to-emacs (beg end charset &optional encoding)
  )


;;; @ end
;;;

(provide 'tm-orig)

(run-hooks 'tm-orig-load-hook)

;;; tm-orig.el ends here
