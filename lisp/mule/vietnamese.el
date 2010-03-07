;;; vietnamese.el --- Support for Vietnamese -*- coding: utf-8; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2002, 2010 Ben Wing.

;; Keywords: multilingual, Vietnamese

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; For Vietnamese, the character sets VISCII and VSCII are supported.

;;; Code:

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
;Moved to mule-charset.el.
;(make-internal-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case" ...
;(make-internal-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case" ...

(define-category ?v "Vietnamese character.")
(modify-category-entry 'vietnamese-viscii-lower ?v)
(modify-category-entry 'vietnamese-viscii-upper ?v)

(make-internal-charset
 'vietnamese-viscii "Vietnamese VISCII1.1"
 '(dimension
   1
   registries ["VISCII1.1"]
   chars 256
   short-name "Vietnamese (VISCII)"
   long-name "Vietnamese (VISCII)"
   tags (latin vietnamese)
   unicode-map ("unicode/libiconv/VISCII.TXT")
   ))

(make-coding-system 
 'viscii 'multibyte "VISCII 1.1 (Vietnamese)"
 '(charsets (vietnamese-viscii)
   mnemonic "VISCII"))

(set-language-info-alist
 "Vietnamese" '((charset vietnamese-viscii-lower vietnamese-viscii-upper
			 vietnamese)
		(coding-system viscii)
		(coding-priority viscii)
		(locale "vietnamese" "vi")
                ;; Not available in packages. 
		;; (input-method . "vietnamese-viqr")
		(features viet-util)
		(sample-text . "Vietnamese (Tiếng Việt)	Chào bạn")
		(documentation . "\
For Vietnamese, Emacs uses special charsets internally.
They can be decoded from and encoded to VISCC, VSCII, and VIQR.
Current setting put higher priority to the coding system VISCII than VSCII.
If you prefer VSCII, please do: (prefer-coding-system 'vietnamese-vscii)")
		))

;;; vietnamese.el ends here
