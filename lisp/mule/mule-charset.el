;;; mule-charset.el --- Charset functions for Mule. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1992 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.
;; Copyright (C) 2002 Ben Wing.

;; Author: Unknown
;; Keywords: i18n, mule, internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.  API at source level synched with FSF 20.3.9.

;;; Commentary:

;; These functions are not compatible at the bytecode level with Emacs/Mule,
;; and they never will be.  -sb [1999-05-26]

;;; Code:

;;;; Classifying text according to charsets

(defun charsets-in-region (start end &optional buffer)
  "Return a list of the charsets in the region between START and END.
BUFFER defaults to the current buffer if omitted."
  (let (list)
    (save-excursion
      (if buffer
	  (set-buffer buffer))
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* (prev-charset
		 (ch (char-after (point)))
		 (charset (char-charset ch)))
	    (if (not (eq prev-charset charset))
		(progn
		  (setq prev-charset charset)
		  (or (memq charset list)
		      (setq list (cons charset list))))))
	  (forward-char))))
    list))

(defun charsets-in-string (string)
  "Return a list of the charsets in STRING."
  (let ((i 0)
 	(len (length string))
 	prev-charset charset list)
    (while (< i len)
      (setq charset (char-charset (aref string i)))
      (if (not (eq prev-charset charset))
 	  (progn
 	    (setq prev-charset charset)
 	    (or (memq charset list)
 		(setq list (cons charset list)))))
      (setq i (1+ i)))
    list))

(defalias 'find-charset-string 'charsets-in-string)
(defalias 'find-charset-region 'charsets-in-region)

;;;; Charset accessors

(defun charset-iso-graphic-plane (charset)
  "Return the `graphic' property of CHARSET.
See `make-charset'."
  (charset-property charset 'graphic))

(defun charset-iso-final-char (charset)
  "Return the final byte of the ISO 2022 escape sequence designating CHARSET."
  (charset-property charset 'final))

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (charset-property charset 'chars))

(defun charset-width (charset)
  "Return the number of display columns per character of CHARSET.
This only applies to TTY mode (under X, the actual display width can
be automatically determined)."
  (charset-property charset 'columns))

;; #### FSFmacs returns 0
(defun charset-direction (charset)
  "Return the display direction (0 for `l2r' or 1 for `r2l') of CHARSET.
Only left-to-right is currently implemented."
  (if (eq (charset-property charset 'direction) 'l2r)
      0
    1))

;; Not in Emacs/Mule
(defun charset-registry (charset)
  "Return the registry of CHARSET.
This is a regular expression matching the registry field of fonts
that can display the characters in CHARSET."
  (charset-property charset 'registry))

(defun charset-ccl-program (charset)
  "Return the CCL program of CHARSET.
See `make-charset'."
  (charset-property charset 'ccl-program))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(define-obsolete-function-alias 'charset-columns 'charset-width) ;; 19990409
(define-obsolete-function-alias 'charset-final 'charset-iso-final-char) ;; 19990409
(define-obsolete-function-alias 'charset-graphic 'charset-iso-graphic-plane) ;; 19990409
(define-obsolete-function-alias 'charset-doc-string 'charset-description) ;; 19990409

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
(defsetf charset-ccl-program set-charset-ccl-program)

;;; FSF compatibility functions
(defun charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil."
  (when (null pos)
    (setq pos (point)))
  (check-argument-type 'integerp pos)
  (unless (or (< pos (point-min))
	      (> pos (point-max)))
    (char-charset (char-after pos))))

;; Yuck!
;; We're not going to support these.
;(defun charset-info (charset) [incredibly broken function with random vectors]
;(defun define-charset (...) [incredibly broken function with random vectors]

;;; Charset property

(defalias 'get-charset-property 'get)
(defalias 'put-charset-property 'put)
(defalias 'charset-plist 'object-plist)
(defalias 'set-charset-plist 'setplist)


;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.
(let ((l '(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312 chinese-big5-1 chinese-big5-2)))
  (while l
    (put-char-table (car l) t auto-fill-chars)
    (setq l (cdr l))))

;;; mule-charset.el ends here
