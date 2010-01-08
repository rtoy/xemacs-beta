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
;(make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case" ...
;(make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case" ...

(define-category ?v "Vietnamese character.")
(modify-category-entry 'vietnamese-viscii-lower ?v)
(modify-category-entry 'vietnamese-viscii-upper ?v)

(make-charset
 'vietnamese-viscii "Vietnamese VISCII1.1"
 '(dimension
   1
   registries ["VISCII1.1"]
   chars 256
   short-name "Vietnamese (VISCII)"
   long-name "Vietnamese (VISCII)"
   unicode-map
   ((#x02 #x1EB2) ;; CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (#x05 #x1EB4) ;; CAPITAL LETTER A WITH BREVE AND TILDE
    (#x06 #x1EAA) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (#x14 #x1EF6) ;; CAPITAL LETTER Y WITH HOOK ABOVE
    (#x19 #x1EF8) ;; CAPITAL LETTER Y WITH TILDE
    (#x1E #x1EF4) ;; CAPITAL LETTER Y WITH DOT BELOW
    (#x80 #x1EA0) ;; CAPITAL LETTER A WITH DOT BELOW
    (#x81 #x1EAE) ;; CAPITAL LETTER A WITH BREVE AND ACUTE
    (#x82 #x1EB0) ;; CAPITAL LETTER A WITH BREVE AND GRAVE
    (#x83 #x1EB6) ;; CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (#x84 #x1EA4) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#x85 #x1EA6) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#x86 #x1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#x87 #x1EAC) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#x88 #x1EBC) ;; CAPITAL LETTER E WITH TILDE
    (#x89 #x1EB8) ;; CAPITAL LETTER E WITH DOT BELOW
    (#x8A #x1EBE) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#x8B #x1EC0) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#x8C #x1EC2) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#x8D #x1EC4) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (#x8E #x1EC6) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#x8F #x1ED0) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#x90 #x1ED2) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#x91 #x1ED4) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#x92 #x1ED6) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (#x93 #x1ED8) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#x94 #x1EE2) ;; CAPITAL LETTER O WITH HORN AND DOT BELOW
    (#x95 #x1EDA) ;; CAPITAL LETTER O WITH HORN AND ACUTE
    (#x96 #x1EDC) ;; CAPITAL LETTER O WITH HORN AND GRAVE
    (#x97 #x1EDE) ;; CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (#x98 #x1ECA) ;; CAPITAL LETTER I WITH DOT BELOW
    (#x99 #x1ECE) ;; CAPITAL LETTER O WITH HOOK ABOVE
    (#x9A #x1ECC) ;; CAPITAL LETTER O WITH DOT BELOW
    (#x9B #x1EC8) ;; CAPITAL LETTER I WITH HOOK ABOVE
    (#x9C #x1EE6) ;; CAPITAL LETTER U WITH HOOK ABOVE
    (#x9D #x0168) ;; CAPITAL LETTER U WITH TILDE
    (#x9E #x1EE4) ;; CAPITAL LETTER U WITH DOT BELOW
    (#x9F #x1EF2) ;; CAPITAL LETTER Y WITH GRAVE
    (#xA0 #x00D5) ;; CAPITAL LETTER O WITH TILDE
    (#xA1 #x1EAF) ;; SMALL LETTER A WITH BREVE AND ACUTE
    (#xA2 #x1EB1) ;; SMALL LETTER A WITH BREVE AND GRAVE
    (#xA3 #x1EB7) ;; SMALL LETTER A WITH BREVE AND DOT BELOW
    (#xA4 #x1EA5) ;; SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#xA5 #x1EA7) ;; SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#xA6 #x1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#xA7 #x1EAD) ;; SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#xA8 #x1EBD) ;; SMALL LETTER E WITH TILDE
    (#xA9 #x1EB9) ;; SMALL LETTER E WITH DOT BELOW
    (#xAA #x1EBF) ;; SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#xAB #x1EC1) ;; SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#xAC #x1EC3) ;; SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#xAD #x1EC5) ;; SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (#xAE #x1EC7) ;; SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#xAF #x1ED1) ;; SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#xB0 #x1ED3) ;; SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#xB1 #x1ED5) ;; SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#xB2 #x1ED7) ;; SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (#xB3 #x1EE0) ;; CAPITAL LETTER O WITH HORN AND TILDE
    (#xB4 #x01A0) ;; CAPITAL LETTER O WITH HORN
    (#xB5 #x1ED9) ;; SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#xB6 #x1EDD) ;; SMALL LETTER O WITH HORN AND GRAVE
    (#xB7 #x1EDF) ;; SMALL LETTER O WITH HORN AND HOOK ABOVE
    (#xB8 #x1ECB) ;; SMALL LETTER I WITH DOT BELOW
    (#xB9 #x1EF0) ;; CAPITAL LETTER U WITH HORN AND DOT BELOW
    (#xBA #x1EE8) ;; CAPITAL LETTER U WITH HORN AND ACUTE
    (#xBB #x1EEA) ;; CAPITAL LETTER U WITH HORN AND GRAVE
    (#xBC #x1EEC) ;; CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (#xBD #x01A1) ;; SMALL LETTER O WITH HORN
    (#xBE #x1EDB) ;; SMALL LETTER O WITH HORN AND ACUTE
    (#xBF #x01AF) ;; CAPITAL LETTER U WITH HORN
    (#xC0 #x00C0) ;; CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; CAPITAL LETTER A WITH TILDE
    (#xC4 #x1EA2) ;; CAPITAL LETTER A WITH HOOK ABOVE
    (#xC5 #x0102) ;; CAPITAL LETTER A WITH BREVE
    (#xC6 #x1EB3) ;; SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (#xC7 #x1EB5) ;; SMALL LETTER A WITH BREVE AND TILDE
    (#xC8 #x00C8) ;; CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x1EBA) ;; CAPITAL LETTER E WITH HOOK ABOVE
    (#xCC #x00CC) ;; CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; CAPITAL LETTER I WITH ACUTE
    (#xCE #x0128) ;; CAPITAL LETTER I WITH TILDE
    (#xCF #x1EF3) ;; SMALL LETTER Y WITH GRAVE
    (#xD0 #x0110) ;; CAPITAL LETTER D WITH STROKE
    (#xD1 #x1EE9) ;; SMALL LETTER U WITH HORN AND ACUTE
    (#xD2 #x00D2) ;; CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x1EA1) ;; SMALL LETTER A WITH DOT BELOW
    (#xD6 #x1EF7) ;; SMALL LETTER Y WITH HOOK ABOVE
    (#xD7 #x1EEB) ;; SMALL LETTER U WITH HORN AND GRAVE
    (#xD8 #x1EED) ;; SMALL LETTER U WITH HORN AND HOOK ABOVE
    (#xD9 #x00D9) ;; CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; CAPITAL LETTER U WITH ACUTE
    (#xDB #x1EF9) ;; SMALL LETTER Y WITH TILDE
    (#xDC #x1EF5) ;; SMALL LETTER Y WITH DOT BELOW
    (#xDD #x00DD) ;; CAPITAL LETTER Y WITH ACUTE
    (#xDE #x1EE1) ;; SMALL LETTER O WITH HORN AND TILDE
    (#xDF #x01B0) ;; SMALL LETTER U WITH HORN
    (#xE0 #x00E0) ;; SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; SMALL LETTER A WITH TILDE
    (#xE4 #x1EA3) ;; SMALL LETTER A WITH HOOK ABOVE
    (#xE5 #x0103) ;; SMALL LETTER A WITH BREVE
    (#xE6 #x1EEF) ;; SMALL LETTER U WITH HORN AND TILDE
    (#xE7 #x1EAB) ;; SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (#xE8 #x00E8) ;; SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x1EBB) ;; SMALL LETTER E WITH HOOK ABOVE
    (#xEC #x00EC) ;; SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; SMALL LETTER I WITH ACUTE
    (#xEE #x0129) ;; SMALL LETTER I WITH TILDE
    (#xEF #x1EC9) ;; SMALL LETTER I WITH HOOK ABOVE
    (#xF0 #x0111) ;; SMALL LETTER D WITH STROKE
    (#xF1 #x1EF1) ;; SMALL LETTER U WITH HORN AND DOT BELOW
    (#xF2 #x00F2) ;; SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; SMALL LETTER O WITH TILDE
    (#xF6 #x1ECF) ;; SMALL LETTER O WITH HOOK ABOVE
    (#xF7 #x1ECD) ;; SMALL LETTER O WITH DOT BELOW
    (#xF8 #x1EE5) ;; SMALL LETTER U WITH DOT BELOW
    (#xF9 #x00F9) ;; SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; SMALL LETTER U WITH ACUTE
    (#xFB #x0169) ;; SMALL LETTER U WITH TILDE
    (#xFC #x1EE7) ;; SMALL LETTER U WITH HOOK ABOVE
    (#xFD #x00FD) ;; SMALL LETTER Y WITH ACUTE
    (#xFE #x1EE3) ;; SMALL LETTER O WITH HORN AND DOT BELOW
    (#xFF #x1EEE)) ;; CAPITAL LETTER U WITH HORN AND TILDE
   ))

(make-coding-system 
 'viscii 'mbcs "VISCII 1.1 (Vietnamese)"
 '(charsets (vietnamese-viscii ascii)
   mnemonic "VISCII"))

(set-language-info-alist
 "Vietnamese" '((charset vietnamese-viscii-lower vietnamese-viscii-upper)
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

(make-one-dimension-windows-charset 1258 'latin "Vietnamese")

;;; vietnamese.el ends here
