;;; hebrew.el --- Support for Hebrew -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Hebrew

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  ISO 8859-8 (Hebrew) support.

;;; Code:

; (make-charset 'hebrew-iso8859-8 
; 	      "Right-Hand Part of Latin/Hebrew Alphabet (ISO/IEC 8859-8): ISO-IR-138"
; 	      '(dimension
; 		1
; 		registry "ISO8859-8"
; 		chars 96
; 		columns 1
; 		direction r2l
; 		final ?H
; 		graphic 1
; 		short-name "RHP of ISO8859/8"
; 		long-name "RHP of Hebrew (ISO 8859-8): ISO-IR-138"
; 		))

;; Syntax of Hebrew characters
(loop for c from 96 to 122
      do (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w"))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 32) "w") ; no-break space


(make-8-bit-coding-system
 'iso-8859-8
 '((#x80 ?\u0080) ;; <control>
   (#x81 ?\u0081) ;; <control>
   (#x82 ?\u0082) ;; <control>
   (#x83 ?\u0083) ;; <control>
   (#x84 ?\u0084) ;; <control>
   (#x85 ?\u0085) ;; <control>
   (#x86 ?\u0086) ;; <control>
   (#x87 ?\u0087) ;; <control>
   (#x88 ?\u0088) ;; <control>
   (#x89 ?\u0089) ;; <control>
   (#x8A ?\u008A) ;; <control>
   (#x8B ?\u008B) ;; <control>
   (#x8C ?\u008C) ;; <control>
   (#x8D ?\u008D) ;; <control>
   (#x8E ?\u008E) ;; <control>
   (#x8F ?\u008F) ;; <control>
   (#x90 ?\u0090) ;; <control>
   (#x91 ?\u0091) ;; <control>
   (#x92 ?\u0092) ;; <control>
   (#x93 ?\u0093) ;; <control>
   (#x94 ?\u0094) ;; <control>
   (#x95 ?\u0095) ;; <control>
   (#x96 ?\u0096) ;; <control>
   (#x97 ?\u0097) ;; <control>
   (#x98 ?\u0098) ;; <control>
   (#x99 ?\u0099) ;; <control>
   (#x9A ?\u009A) ;; <control>
   (#x9B ?\u009B) ;; <control>
   (#x9C ?\u009C) ;; <control>
   (#x9D ?\u009D) ;; <control>
   (#x9E ?\u009E) ;; <control>
   (#x9F ?\u009F) ;; <control>
   (#xA0 ?\u00A0) ;; NO-BREAK SPACE
   (#xA2 ?\u00A2) ;; CENT SIGN
   (#xA3 ?\u00A3) ;; POUND SIGN
   (#xA4 ?\u00A4) ;; CURRENCY SIGN
   (#xA5 ?\u00A5) ;; YEN SIGN
   (#xA6 ?\u00A6) ;; BROKEN BAR
   (#xA7 ?\u00A7) ;; SECTION SIGN
   (#xA8 ?\u00A8) ;; DIAERESIS
   (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
   (#xAA ?\u00D7) ;; MULTIPLICATION SIGN
   (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xAC ?\u00AC) ;; NOT SIGN
   (#xAD ?\u00AD) ;; SOFT HYPHEN
   (#xAE ?\u00AE) ;; REGISTERED SIGN
   (#xAF ?\u00AF) ;; MACRON
   (#xB0 ?\u00B0) ;; DEGREE SIGN
   (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
   (#xB2 ?\u00B2) ;; SUPERSCRIPT TWO
   (#xB3 ?\u00B3) ;; SUPERSCRIPT THREE
   (#xB4 ?\u00B4) ;; ACUTE ACCENT
   (#xB5 ?\u00B5) ;; MICRO SIGN
   (#xB6 ?\u00B6) ;; PILCROW SIGN
   (#xB7 ?\u00B7) ;; MIDDLE DOT
   (#xB8 ?\u00B8) ;; CEDILLA
   (#xB9 ?\u00B9) ;; SUPERSCRIPT ONE
   (#xBA ?\u00F7) ;; DIVISION SIGN
   (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xBC ?\u00BC) ;; VULGAR FRACTION ONE QUARTER
   (#xBD ?\u00BD) ;; VULGAR FRACTION ONE HALF
   (#xBE ?\u00BE) ;; VULGAR FRACTION THREE QUARTERS
   (#xDF ?\u2017) ;; DOUBLE LOW LINE
   (#xE0 ?\u05D0) ;; HEBREW LETTER ALEF
   (#xE1 ?\u05D1) ;; HEBREW LETTER BET
   (#xE2 ?\u05D2) ;; HEBREW LETTER GIMEL
   (#xE3 ?\u05D3) ;; HEBREW LETTER DALET
   (#xE4 ?\u05D4) ;; HEBREW LETTER HE
   (#xE5 ?\u05D5) ;; HEBREW LETTER VAV
   (#xE6 ?\u05D6) ;; HEBREW LETTER ZAYIN
   (#xE7 ?\u05D7) ;; HEBREW LETTER HET
   (#xE8 ?\u05D8) ;; HEBREW LETTER TET
   (#xE9 ?\u05D9) ;; HEBREW LETTER YOD
   (#xEA ?\u05DA) ;; HEBREW LETTER FINAL KAF
   (#xEB ?\u05DB) ;; HEBREW LETTER KAF
   (#xEC ?\u05DC) ;; HEBREW LETTER LAMED
   (#xED ?\u05DD) ;; HEBREW LETTER FINAL MEM
   (#xEE ?\u05DE) ;; HEBREW LETTER MEM
   (#xEF ?\u05DF) ;; HEBREW LETTER FINAL NUN
   (#xF0 ?\u05E0) ;; HEBREW LETTER NUN
   (#xF1 ?\u05E1) ;; HEBREW LETTER SAMEKH
   (#xF2 ?\u05E2) ;; HEBREW LETTER AYIN
   (#xF3 ?\u05E3) ;; HEBREW LETTER FINAL PE
   (#xF4 ?\u05E4) ;; HEBREW LETTER PE
   (#xF5 ?\u05E5) ;; HEBREW LETTER FINAL TSADI
   (#xF6 ?\u05E6) ;; HEBREW LETTER TSADI
   (#xF7 ?\u05E7) ;; HEBREW LETTER QOF
   (#xF8 ?\u05E8) ;; HEBREW LETTER RESH
   (#xF9 ?\u05E9) ;; HEBREW LETTER SHIN
   (#xFA ?\u05EA) ;; HEBREW LETTER TAV
   (#xFD ?\u200E) ;; LEFT-TO-RIGHT MARK
   (#xFE ?\u200F)) ;; RIGHT-TO-LEFT MARK
 "ISO-8859-8 (Hebrew)"
 '(mnemonic "MIME/Hbrw"))

(make-coding-system
 'ctext-hebrew 'iso2022
 "ISO-8859-8-E (Hebrew, explicit directional coding)"
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii hebrew-iso8859-8)
   mnemonic "CText/Hbrw"
   ))

(set-language-info-alist
 "Hebrew" '((charset hebrew-iso8859-8)
	    (coding-system iso-8859-8)
	    (coding-priority iso-8859-8)
            ;; Not available in packages. 
	    ;; (input-method . "hebrew")
	    (sample-text . "Hebrew	[2],Hylem[0](B")
	    (documentation . "Right-to-left writing is not yet supported.")
	    ))

;;; hebrew.el ends here
