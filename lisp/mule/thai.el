;;; thai.el --- support for Thai -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 2010 Ben Wing.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Thai

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

;;; Synched up with: Emacs 21.1 (language/thai.el).

;;; Commentary:

;; For Thai, the character set TIS620 is supported.

;;; Code:

(make-internal-charset
 'thai-iso8859-11
 "Right-Hand Part of Latin/Thai Alphabet (ISO/IEC 8859-11)"
 '(dimension 1
   registries ["ISO8859-11"]
   chars 96
   ;final ?T @@#### What is the final byte for this?
   graphic 1
   unicode-map ("unicode/unicode-consortium/ISO8859/8859-11.TXT" #xA0)
   short-name "Thai (ISO8859-11)"
   long-name "RHP of Thai (ISO 8859-11)"
   tags (iso8859 thai)))

(make-coding-system
 'thai-tis620 'multibyte "Thai/TIS620"
 '(charsets (ascii thai-tis620)
   mnemonic "Thai"
   safe-charsets (ascii thai-tis620)
   post-read-conversion thai-post-read-conversion
   documentation "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)"))

(define-coding-system-alias 'th-tis620 'thai-tis620)
(define-coding-system-alias 'tis620 'thai-tis620)
(define-coding-system-alias 'tis-620 'thai-tis620)

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (charset thai-tis620 thai)
	  (coding-system thai-tis620)
	  (coding-priority thai-tis620)
	  (nonascii-translation . thai-tis620)
	  (input-method . "thai-kesmanee")
	  (unibyte-display . thai-tis620)
	  (features thai-util)
	  (sample-text 
	   . (thai-compose-string
	      (copy-sequence "Thai (,T@RIRd7B(B)		,TJ0GQ1J04U1$0CQ1:(B, ,TJ0GQ1J04U10$h1P(B")))
	  (documentation . t)))


;; Register a function to compose Thai characters.
(put-char-table 'thai-tis620
		'(("\\c0\\c4\\|\\c0\\(\\c2\\|\\c3\\)\\c4?" .
		   thai-composition-function))
		composition-function-table)

;; @@#### Where are thai-composition-function, thai-compose-string, and
;; thai-post-read-conversion?

(provide 'thai)

;;; thai.el ends here
