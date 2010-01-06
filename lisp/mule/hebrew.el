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

;; Syntax of Hebrew characters
(loop for c from 96 to 122
      do (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w"))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 32) "w") ; no-break space


(make-coding-system
 'iso-8859-8 'fixed-width "ISO-8859-8 (Hebrew)"
 '(unicode-map
   ((#x80 #x0080) ;; <control>
    (#x81 #x0081) ;; <control>
    (#x82 #x0082) ;; <control>
    (#x83 #x0083) ;; <control>
    (#x84 #x0084) ;; <control>
    (#x85 #x0085) ;; <control>
    (#x86 #x0086) ;; <control>
    (#x87 #x0087) ;; <control>
    (#x88 #x0088) ;; <control>
    (#x89 #x0089) ;; <control>
    (#x8A #x008A) ;; <control>
    (#x8B #x008B) ;; <control>
    (#x8C #x008C) ;; <control>
    (#x8D #x008D) ;; <control>
    (#x8E #x008E) ;; <control>
    (#x8F #x008F) ;; <control>
    (#x90 #x0090) ;; <control>
    (#x91 #x0091) ;; <control>
    (#x92 #x0092) ;; <control>
    (#x93 #x0093) ;; <control>
    (#x94 #x0094) ;; <control>
    (#x95 #x0095) ;; <control>
    (#x96 #x0096) ;; <control>
    (#x97 #x0097) ;; <control>
    (#x98 #x0098) ;; <control>
    (#x99 #x0099) ;; <control>
    (#x9A #x009A) ;; <control>
    (#x9B #x009B) ;; <control>
    (#x9C #x009C) ;; <control>
    (#x9D #x009D) ;; <control>
    (#x9E #x009E) ;; <control>
    (#x9F #x009F) ;; <control>
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA2 #x00A2) ;; CENT SIGN
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x00A5) ;; YEN SIGN
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x00D7) ;; MULTIPLICATION SIGN
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xAF #x00AF) ;; MACRON
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x00B3) ;; SUPERSCRIPT THREE
    (#xB4 #x00B4) ;; ACUTE ACCENT
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x00B8) ;; CEDILLA
    (#xB9 #x00B9) ;; SUPERSCRIPT ONE
    (#xBA #x00F7) ;; DIVISION SIGN
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x00BC) ;; VULGAR FRACTION ONE QUARTER
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE #x00BE) ;; VULGAR FRACTION THREE QUARTERS
    (#xDF #x2017) ;; DOUBLE LOW LINE
    (#xE0 #x05D0) ;; HEBREW LETTER ALEF
    (#xE1 #x05D1) ;; HEBREW LETTER BET
    (#xE2 #x05D2) ;; HEBREW LETTER GIMEL
    (#xE3 #x05D3) ;; HEBREW LETTER DALET
    (#xE4 #x05D4) ;; HEBREW LETTER HE
    (#xE5 #x05D5) ;; HEBREW LETTER VAV
    (#xE6 #x05D6) ;; HEBREW LETTER ZAYIN
    (#xE7 #x05D7) ;; HEBREW LETTER HET
    (#xE8 #x05D8) ;; HEBREW LETTER TET
    (#xE9 #x05D9) ;; HEBREW LETTER YOD
    (#xEA #x05DA) ;; HEBREW LETTER FINAL KAF
    (#xEB #x05DB) ;; HEBREW LETTER KAF
    (#xEC #x05DC) ;; HEBREW LETTER LAMED
    (#xED #x05DD) ;; HEBREW LETTER FINAL MEM
    (#xEE #x05DE) ;; HEBREW LETTER MEM
    (#xEF #x05DF) ;; HEBREW LETTER FINAL NUN
    (#xF0 #x05E0) ;; HEBREW LETTER NUN
    (#xF1 #x05E1) ;; HEBREW LETTER SAMEKH
    (#xF2 #x05E2) ;; HEBREW LETTER AYIN
    (#xF3 #x05E3) ;; HEBREW LETTER FINAL PE
    (#xF4 #x05E4) ;; HEBREW LETTER PE
    (#xF5 #x05E5) ;; HEBREW LETTER FINAL TSADI
    (#xF6 #x05E6) ;; HEBREW LETTER TSADI
    (#xF7 #x05E7) ;; HEBREW LETTER QOF
    (#xF8 #x05E8) ;; HEBREW LETTER RESH
    (#xF9 #x05E9) ;; HEBREW LETTER SHIN
    (#xFA #x05EA) ;; HEBREW LETTER TAV
    (#xFD #x200E) ;; LEFT-TO-RIGHT MARK
    (#xFE #x200F)) ;; RIGHT-TO-LEFT MARK
   mnemonic "MIME/Hbrw"))

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
