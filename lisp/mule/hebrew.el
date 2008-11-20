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
 '((#xAA ?\u00D7) ;; MULTIPLICATION SIGN
   (#xBA ?\u00F7) ;; DIVISION SIGN
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
