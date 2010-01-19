;;; vietnamese.el --- Support for Vietnamese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2002 Ben Wing.

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
(make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case"
	      '(dimension
		1
		registries ["VISCII1.1"]
		chars 96
		columns 1
		direction l2r
		final ?1
		graphic 1
		short-name "VISCII lower"
		long-name "VISCII lower-case"
		))

(make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case"
	      '(dimension
		1
		registries ["VISCII1.1"]
		chars 96
		columns 1
		direction l2r
		final ?2
		graphic 1
		short-name "VISCII upper"
		long-name "VISCII upper-case"
		))

(define-category ?v "Vietnamese character.")
(modify-category-entry 'vietnamese-viscii-lower ?v)
(modify-category-entry 'vietnamese-viscii-upper ?v)

(make-coding-system 
 'viscii 'fixed-width "VISCII 1.1 (Vietnamese)"
 '(unicode-map
   ((#x02 ?\u1EB2) ;; CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (#x05 ?\u1EB4) ;; CAPITAL LETTER A WITH BREVE AND TILDE
    (#x06 ?\u1EAA) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (#x14 ?\u1EF6) ;; CAPITAL LETTER Y WITH HOOK ABOVE
    (#x19 ?\u1EF8) ;; CAPITAL LETTER Y WITH TILDE
    (#x1E ?\u1EF4) ;; CAPITAL LETTER Y WITH DOT BELOW
    (#x80 ?\u1EA0) ;; CAPITAL LETTER A WITH DOT BELOW
    (#x81 ?\u1EAE) ;; CAPITAL LETTER A WITH BREVE AND ACUTE
    (#x82 ?\u1EB0) ;; CAPITAL LETTER A WITH BREVE AND GRAVE
    (#x83 ?\u1EB6) ;; CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (#x84 ?\u1EA4) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#x85 ?\u1EA6) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#x86 ?\u1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#x87 ?\u1EAC) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#x88 ?\u1EBC) ;; CAPITAL LETTER E WITH TILDE
    (#x89 ?\u1EB8) ;; CAPITAL LETTER E WITH DOT BELOW
    (#x8A ?\u1EBE) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#x8B ?\u1EC0) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#x8C ?\u1EC2) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#x8D ?\u1EC4) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (#x8E ?\u1EC6) ;; CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#x8F ?\u1ED0) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#x90 ?\u1ED2) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#x91 ?\u1ED4) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#x92 ?\u1ED6) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (#x93 ?\u1ED8) ;; CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#x94 ?\u1EE2) ;; CAPITAL LETTER O WITH HORN AND DOT BELOW
    (#x95 ?\u1EDA) ;; CAPITAL LETTER O WITH HORN AND ACUTE
    (#x96 ?\u1EDC) ;; CAPITAL LETTER O WITH HORN AND GRAVE
    (#x97 ?\u1EDE) ;; CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (#x98 ?\u1ECA) ;; CAPITAL LETTER I WITH DOT BELOW
    (#x99 ?\u1ECE) ;; CAPITAL LETTER O WITH HOOK ABOVE
    (#x9A ?\u1ECC) ;; CAPITAL LETTER O WITH DOT BELOW
    (#x9B ?\u1EC8) ;; CAPITAL LETTER I WITH HOOK ABOVE
    (#x9C ?\u1EE6) ;; CAPITAL LETTER U WITH HOOK ABOVE
    (#x9D ?\u0168) ;; CAPITAL LETTER U WITH TILDE
    (#x9E ?\u1EE4) ;; CAPITAL LETTER U WITH DOT BELOW
    (#x9F ?\u1EF2) ;; CAPITAL LETTER Y WITH GRAVE
    (#xA0 ?\u00D5) ;; CAPITAL LETTER O WITH TILDE
    (#xA1 ?\u1EAF) ;; SMALL LETTER A WITH BREVE AND ACUTE
    (#xA2 ?\u1EB1) ;; SMALL LETTER A WITH BREVE AND GRAVE
    (#xA3 ?\u1EB7) ;; SMALL LETTER A WITH BREVE AND DOT BELOW
    (#xA4 ?\u1EA5) ;; SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (#xA5 ?\u1EA7) ;; SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (#xA6 ?\u1EA8) ;; CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (#xA7 ?\u1EAD) ;; SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (#xA8 ?\u1EBD) ;; SMALL LETTER E WITH TILDE
    (#xA9 ?\u1EB9) ;; SMALL LETTER E WITH DOT BELOW
    (#xAA ?\u1EBF) ;; SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (#xAB ?\u1EC1) ;; SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (#xAC ?\u1EC3) ;; SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (#xAD ?\u1EC5) ;; SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (#xAE ?\u1EC7) ;; SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (#xAF ?\u1ED1) ;; SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (#xB0 ?\u1ED3) ;; SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (#xB1 ?\u1ED5) ;; SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (#xB2 ?\u1ED7) ;; SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (#xB3 ?\u1EE0) ;; CAPITAL LETTER O WITH HORN AND TILDE
    (#xB4 ?\u01A0) ;; CAPITAL LETTER O WITH HORN
    (#xB5 ?\u1ED9) ;; SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (#xB6 ?\u1EDD) ;; SMALL LETTER O WITH HORN AND GRAVE
    (#xB7 ?\u1EDF) ;; SMALL LETTER O WITH HORN AND HOOK ABOVE
    (#xB8 ?\u1ECB) ;; SMALL LETTER I WITH DOT BELOW
    (#xB9 ?\u1EF0) ;; CAPITAL LETTER U WITH HORN AND DOT BELOW
    (#xBA ?\u1EE8) ;; CAPITAL LETTER U WITH HORN AND ACUTE
    (#xBB ?\u1EEA) ;; CAPITAL LETTER U WITH HORN AND GRAVE
    (#xBC ?\u1EEC) ;; CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (#xBD ?\u01A1) ;; SMALL LETTER O WITH HORN
    (#xBE ?\u1EDB) ;; SMALL LETTER O WITH HORN AND ACUTE
    (#xBF ?\u01AF) ;; CAPITAL LETTER U WITH HORN
    (#xC0 ?\u00C0) ;; CAPITAL LETTER A WITH GRAVE
    (#xC1 ?\u00C1) ;; CAPITAL LETTER A WITH ACUTE
    (#xC2 ?\u00C2) ;; CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 ?\u00C3) ;; CAPITAL LETTER A WITH TILDE
    (#xC4 ?\u1EA2) ;; CAPITAL LETTER A WITH HOOK ABOVE
    (#xC5 ?\u0102) ;; CAPITAL LETTER A WITH BREVE
    (#xC6 ?\u1EB3) ;; SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (#xC7 ?\u1EB5) ;; SMALL LETTER A WITH BREVE AND TILDE
    (#xC8 ?\u00C8) ;; CAPITAL LETTER E WITH GRAVE
    (#xC9 ?\u00C9) ;; CAPITAL LETTER E WITH ACUTE
    (#xCA ?\u00CA) ;; CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB ?\u1EBA) ;; CAPITAL LETTER E WITH HOOK ABOVE
    (#xCC ?\u00CC) ;; CAPITAL LETTER I WITH GRAVE
    (#xCD ?\u00CD) ;; CAPITAL LETTER I WITH ACUTE
    (#xCE ?\u0128) ;; CAPITAL LETTER I WITH TILDE
    (#xCF ?\u1EF3) ;; SMALL LETTER Y WITH GRAVE
    (#xD0 ?\u0110) ;; CAPITAL LETTER D WITH STROKE
    (#xD1 ?\u1EE9) ;; SMALL LETTER U WITH HORN AND ACUTE
    (#xD2 ?\u00D2) ;; CAPITAL LETTER O WITH GRAVE
    (#xD3 ?\u00D3) ;; CAPITAL LETTER O WITH ACUTE
    (#xD4 ?\u00D4) ;; CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 ?\u1EA1) ;; SMALL LETTER A WITH DOT BELOW
    (#xD6 ?\u1EF7) ;; SMALL LETTER Y WITH HOOK ABOVE
    (#xD7 ?\u1EEB) ;; SMALL LETTER U WITH HORN AND GRAVE
    (#xD8 ?\u1EED) ;; SMALL LETTER U WITH HORN AND HOOK ABOVE
    (#xD9 ?\u00D9) ;; CAPITAL LETTER U WITH GRAVE
    (#xDA ?\u00DA) ;; CAPITAL LETTER U WITH ACUTE
    (#xDB ?\u1EF9) ;; SMALL LETTER Y WITH TILDE
    (#xDC ?\u1EF5) ;; SMALL LETTER Y WITH DOT BELOW
    (#xDD ?\u00DD) ;; CAPITAL LETTER Y WITH ACUTE
    (#xDE ?\u1EE1) ;; SMALL LETTER O WITH HORN AND TILDE
    (#xDF ?\u01B0) ;; SMALL LETTER U WITH HORN
    (#xE0 ?\u00E0) ;; SMALL LETTER A WITH GRAVE
    (#xE1 ?\u00E1) ;; SMALL LETTER A WITH ACUTE
    (#xE2 ?\u00E2) ;; SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 ?\u00E3) ;; SMALL LETTER A WITH TILDE
    (#xE4 ?\u1EA3) ;; SMALL LETTER A WITH HOOK ABOVE
    (#xE5 ?\u0103) ;; SMALL LETTER A WITH BREVE
    (#xE6 ?\u1EEF) ;; SMALL LETTER U WITH HORN AND TILDE
    (#xE7 ?\u1EAB) ;; SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (#xE8 ?\u00E8) ;; SMALL LETTER E WITH GRAVE
    (#xE9 ?\u00E9) ;; SMALL LETTER E WITH ACUTE
    (#xEA ?\u00EA) ;; SMALL LETTER E WITH CIRCUMFLEX
    (#xEB ?\u1EBB) ;; SMALL LETTER E WITH HOOK ABOVE
    (#xEC ?\u00EC) ;; SMALL LETTER I WITH GRAVE
    (#xED ?\u00ED) ;; SMALL LETTER I WITH ACUTE
    (#xEE ?\u0129) ;; SMALL LETTER I WITH TILDE
    (#xEF ?\u1EC9) ;; SMALL LETTER I WITH HOOK ABOVE
    (#xF0 ?\u0111) ;; SMALL LETTER D WITH STROKE
    (#xF1 ?\u1EF1) ;; SMALL LETTER U WITH HORN AND DOT BELOW
    (#xF2 ?\u00F2) ;; SMALL LETTER O WITH GRAVE
    (#xF3 ?\u00F3) ;; SMALL LETTER O WITH ACUTE
    (#xF4 ?\u00F4) ;; SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 ?\u00F5) ;; SMALL LETTER O WITH TILDE
    (#xF6 ?\u1ECF) ;; SMALL LETTER O WITH HOOK ABOVE
    (#xF7 ?\u1ECD) ;; SMALL LETTER O WITH DOT BELOW
    (#xF8 ?\u1EE5) ;; SMALL LETTER U WITH DOT BELOW
    (#xF9 ?\u00F9) ;; SMALL LETTER U WITH GRAVE
    (#xFA ?\u00FA) ;; SMALL LETTER U WITH ACUTE
    (#xFB ?\u0169) ;; SMALL LETTER U WITH TILDE
    (#xFC ?\u1EE7) ;; SMALL LETTER U WITH HOOK ABOVE
    (#xFD ?\u00FD) ;; SMALL LETTER Y WITH ACUTE
    (#xFE ?\u1EE3) ;; SMALL LETTER O WITH HORN AND DOT BELOW
    (#xFF ?\u1EEE)) ;; CAPITAL LETTER U WITH HORN AND TILDE
   mnemonic "VISCII"))

(set-language-info-alist
 "Vietnamese" '((charset vietnamese-viscii-lower vietnamese-viscii-upper)
		(coding-system viscii)
		(coding-priority viscii)
		(locale "vietnamese" "vi")
                ;; Not available in packages. 
		;; (input-method . "vietnamese-viqr")
		(features viet-util)
		(sample-text . "Vietnamese (Ti,1*(Bng Vi,1.(Bt)	Ch,1`(Bo b,1U(Bn")
		(documentation . "\
For Vietnamese, Emacs uses special charsets internally.
They can be decoded from and encoded to VISCC, VSCII, and VIQR.
Current setting put higher priority to the coding system VISCII than VSCII.
If you prefer VSCII, please do: (prefer-coding-system 'vietnamese-vscii)")
		))

;;; vietnamese.el ends here
